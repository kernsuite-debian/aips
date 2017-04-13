#!/usr/bin/perl
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-1996, 1998, 2001-2002, 2004, 2010
#;  Associated Universities, Inc. Washington DC, USA.
#;
#;  This program is free software; you can redistribute it and/or
#;  modify it under the terms of the GNU General Public License as
#;  published by the Free Software Foundation; either version 2 of
#;  the License, or (at your option) any later version.
#;
#;  This program is distributed in the hope that it will be useful,
#;  but WITHOUT ANY WARRANTY; without even the implied warranty of
#;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#;  GNU General Public License for more details.
#;
#;  You should have received a copy of the GNU General Public
#;  License along with this program; if not, write to the Free
#;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
#;  MA 02139, USA.
#;
#;  Correspondence concerning AIPS should be addressed as follows:
#;         Internet email: aipsmail@nrao.edu.
#;         Postal address: AIPS Project Office
#;                         National Radio Astronomy Observatory
#;                         520 Edgemont Road
#;                         Charlottesville, VA 22903-2475 USA
#-----------------------------------------------------------------------

unshift (@INC, "$ENV{'SYSUNIX'}/PERL");

require 'check_version.pl';
                                        # Check for supported versions
&check_perl_version;
                                        # For dec/ehex conversions.
require 'ehex.pl';
                                        # temporary area
$tmpdir = $ENV{'AIPSTMP'} || "/tmp";
                                        # If we catch a signal, ensure
				        # our temp file is removed.
				        # DADEVS.SH will then probably
				        # take up the action, unless the
				        # calling script checks the
				        # return status of this process
				        # and acts accordingly.

$SIG{'HUP'} = $SIG{'INT'} = $SIG{'QUIT'} = $SIG{'TERM'} = 'cleanup';

                                        # For saving %ENV stuff for
				        # calling script, whose PID must
				        # be supplied.

&abort ('No PID supplied') unless $PID = shift;
                                        # Ehex maximum...
$max_disks = 35;
                                        # Listed in order of desired
				        # priority.
@DADEVS = (
	   "$ENV{'HOME'}/.dadevs.$ENV{'SITE'}",
	   "private $ENV{'SITE'}-specific",
	   "$ENV{'HOME'}/.dadevs",
	   "private",
	   "$ENV{'DA00'}/DADEVS.LIST",
	   "$ENV{'HOST'}-specific",
	   "$ENV{'NET0'}/DADEVS.LIST.$ENV{'SITE'}",
	   "$ENV{'SITE'}-specific default",
	   "$ENV{'NET0'}/DADEVS.LIST",
	   "global default",
	   );
                                        # Shaddap!
$ENV{'DADEVS_QUIET'} eq 'YES' && ($quiet = 1);

                                        # Figure out which file to use.

for ( ; $LIST = shift @DADEVS, $DESC = shift @DADEVS; @DADEVS) {
    -f "$LIST" && last;
}

&abort ('could not find DADEVS list file') unless $LIST;

&qprint ("  (Using $DESC file $LIST for DADEVS.PL)");

open (ENTRY, "<$LIST") ||
    &abort ("could not open DADEVS list file \"$LIST\"");
                                        # Slurp in the definitions file.
@ENTRY = <ENTRY>;
close ENTRY;

open (PIDFILE, ">$tmpdir/DADEVS.$PID") ||
    &abort ("could not open DADEVS PID file \"$tmpdir/DADEVS.$PID\"");

                                        # Upcase and fix.
($daopt = $ENV{'DAOPT'}) =~ tr/a-z/A-Z/;
$daopt =~ s/[^A-Z0-9\-]/ /g;
                                        # Startin' from scratch.
$nvol = 0;

@dreq = (); @dhost = (); @dopt = ();

                                        # user special file postpend

$LLIST = "$ENV{'HOME'}/.dadevs.always";
if (-f $LLIST) {
    open (LENTRY, "<$LLIST") ||
        &abort ("could not open LUSTRE list file \"$LLIST\"");
    @LENTRY = <LENTRY>;
    close LENTRY;
    foreach (@LENTRY) {
	if (! /^[+-]\s\s/) {
                                        # Need to reset these in case
                                        # there are blank lines.
	    $dareqd = '';
	    $LENTRY = '';
	    next;
	}
                                        # Zap any comments in-line
	s/ *\#.*$//;
	($dareqd, $LENTRY) = split;
                                        # All disks?
	if ($dareqd eq '+') {
	    push(@dreq, $LENTRY);
	} elsif ($dareqd eq '-') {
	    push(@dopt, $LENTRY);
	}
    }
}
                                        # main lists
if ($daopt ne 'DEFAULT') {
thing:
    foreach (@ENTRY) {
	if (! /^[+-]\s\s/) {
                                        # Need to reset these in case
                                        # there are blank lines.
	    $dareqd = '';
	    $ENTRY = '';
	    next;
	}
                                        # Zap any comments in-line
	s/ *\#.*$//;
	($dareqd, $ENTRY) = split;
                                        # All disks?
	if ($dareqd eq '+') {
	    push(@dreq, $ENTRY);
	} elsif ($ENTRY =~ /\/$ENV{'HOST'}$/) {
	    push(@dhost, $ENTRY);
	} elsif ($ENTRY =~ /\/$ENV{'HOST'}\_/) {
	    push(@dhost, $ENTRY);
	} elsif ($daopt eq 'ALL') {
	    push(@dopt, $ENTRY);
	} else {
	    foreach $OPT (split (' ', $daopt)) {
		if ($ENTRY =~ /\/$OPT(\_|$\)/) {
		    push(@dopt, $ENTRY);
		    next thing;
		}
	    }
	}
    }
    $tot = 3 + $#dhost + $#dreq + $#dopt;
    if ($tot == 0) {
	printf "DADEVS.PL error - no disks selected at all\n";
	printf "This is a FATAL error and nothing in AIPS will work.\n";
    } else {
                                        # Order is important: host
                                        # specific, required, and
                                        # finally "da=" selected.
	@done = ("I hope nothing ever matches this");
disk:
	foreach $ENTRY (@dreq, @dhost, @dopt) {
	    if ($nvol < 35) {
		if ( -f "$ENTRY/SPACE" ) {
		    foreach $onedone (@done) {
			if ($onedone =~ /^$ENTRY$/) {
			    next disk;
			}
		    }
		    push(@done, $ENTRY);
		    $nhex = &dec_to_ehex (++$nvol);
		    print PIDFILE "DA0$nhex=$ENTRY; export DA0$nhex\n";
		    &qprint ("   Disk $nvol ($nhex) is $ENTRY");
		} else {
		    $str = "                  ";
		    $str .= "$ENTRY is currently unavailable.";
		    &qprint ($str);
		}
	    } else {
		$str = "                 ";
		$str .= "$ENTRY skipped ($max_disks disk limit)";
		&qprint ($str);
	    }
	}
    }
} else {
                                        # $daopt eq "DEFAULT".
    $nhex = 0;
    &qprint ('   (Use da=host to select other disks)');

    while ($nhex < 35) {
	$ehex = &dec_to_ehex (++$nhex);
	($dname = $ENV{"DA0$ehex"}) &&
	    (&qprint ("   Disk $nhex ($ehex) is $dname"),
	     $nvol = $nhex);
    }
}

&qprint;
                                        # For ZDCHIN...
print PIDFILE "NVOL=$nvol; export NVOL\n";
close PIDFILE;
exit 0;
                                        # Subroutines.
sub qprint
{
    print "@_\n" unless $quiet;
}

sub abort
{
    die "\nDADEVS error - @_.
This is a FATAL error and nothing in AIPS will work.\n";
}

sub cleanup
{
    close PIDFILE if PIDFILE;
    unlink "$tmpdir/DADEVS.$PID" if -f "$tmpdir/DADEVS.$PID";
    print "DADEVS.PL caught signal @_, exiting...\n";
    exit 1;
}
