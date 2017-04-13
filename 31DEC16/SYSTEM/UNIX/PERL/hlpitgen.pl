#!/usr/bin/perl
# -*-Perl-*-
#-----------------------------------------------------------------------
#;  Copyright (C) 1995-1996, 2001, 2004, 2011
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

opendir (HLP, $ENV{'HLPFIL'}) ||
    die "Cannot open directory \"$ENV{'HLPFIL'}\"\n";

$out = "$ENV{'HLPFIL'}/HLPIT.LIS";
if ( -f $out ) {
    printf("File %s already exists...", $out);
    $newout = "/tmp/HLPIT.LIS";
    if ( -f $newout ) {
	printf ("so does %s!  Removing latter...");
	unlink ($newout) || die "Can't!";
	printf ("done\n");
	$out = $newout;
    } else {
	printf ("using %s instead.\n", $newout);
	$out = $newout;
    }
}

foreach $file (readdir HLP) {
    next if $file =~ /^(ZZ|\.)/;
    next unless $file =~ /\.HLP$/;

    open (FIL, "<$ENV{'HLPFIL'}/$file") ||
	warn "Cannot open file \"$ENV{'HLPFIL'}/$file\" for reading.\n";

    $file =~ s/\.HLP$//;

    foreach (<FIL>) {
	next unless /^;\#\s*TASK\s+/i;
	$list{$file} = '-';
	last;
    }
    close FIL;
    $list{$file} = '' unless $list{$file};
}
if (! open (OUT, ">$out")) {
    print "Cannot open file \"$out\" for writing!\n";
    print "Enter YES to get results to stdout..."; $ans = <STDIN>;
    if ($ans =~ /^YES$/) {
	map { printf ("%s%s\n", $list{$_}, lc $_) } sort keys %list;
	warn "Make sure you check this in to RCS!";
    }
} else {
    map {
	printf (OUT "%s%s\n", $list{$_}, lc $_)
	} sort keys %list;
    warn "Make sure you check in the new $out to RCS!";
}
