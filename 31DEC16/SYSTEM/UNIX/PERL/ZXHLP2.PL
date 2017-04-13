#!/usr/bin/perl -T
# -*-Perl-*-
#-----------------------------------------------------------------------
#;  Copyright (C) 1998-1999, 2001, 2004-2015
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
# This script assumes perl 5.  Things that may need modified are the
# default definition of AIPS_ROOT and the value of aipshelp.
#-----------------------------------------------------------------------
#                                       Version dependent
$TST = "31DEC16";
#                                       Secure/Taint requirements.
$ENV{'PATH'} = "/bin:/usr/bin";
$ENV{'IFS'} = " ";
#                                       Display of time in output.
require 'ctime.pl';
#                                       Get base URL for ourselves
#                                       Change this if you
$AIPSHELP = "http://www.aips.nrao.edu/cgi-bin/ZXHLP2.PL";
#                                         DEBUG
#    $HELP = "/home/filehost/egreisen/AIPS";
$HELP = "/home/www.aips.nrao.edu/content/HELP";
#                                       function
$skip2 = "VLBA";
$skip3 = "VLBI";
$skip4 = "POPS";
$skip5 = "IF";
$skip6 = "I";
$skip7 = "J";
$skip8 = "X";
$skip9 = "Y";
sub aiprpl {
    local($aname) = $_[0];
    if (($aname ne $item) && ($aname ne $skip2) && ($aname ne $skip3) &&
        ($aname ne $skip4) && ($aname ne $skip5) && ($aname ne $skip6) &&
        ($aname ne $skip7) && ($aname ne $skip8) && ($aname ne $skip9)) {
#    if (!($item =~ /^$aname$/)) {
        foreach $topic (@hlpit) {
#                                                    Note: minmatch!!!!!
            if ($topic =~ /^$aname/) {
                $file = "$HELP/$topic" . ".HLP";
                if ( -f $file ) {
                    $aname = "<A HREF=\"ZXHLP2.PL?$topic\"\>$aname\<\/A\>";
                    last;
                }
            }
        }
    }
    $aname;
}
sub aipepl {
    local($aname) = $_[0];
    if (($aname ne $item) && ($aname ne $skip2) && ($aname ne $skip3) &&
        ($aname ne $skip4) && ($aname ne $skip5) && ($aname ne $skip6) &&
        ($aname ne $skip7) && ($aname ne $skip8) && ($aname ne $skip9)) {
#    if (!($item =~ /^$aname$/)) {
        foreach $topic (@hlpit) {
#                                                    Note: exact match
            if ($topic =~ /^$aname$/) {
                $file = "$HELP/$topic" . ".HLP";
                if ( -f $file ) {
                    $aname = "<A HREF=\"ZXHLP2.PL?$topic\"\>$aname\<\/A\>";
                    last;
                }
            }
        }
    }
    $aname;
}
#                                       Print a basic header
printf("Content-type: text/html\n\n");
printf("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n");
printf("<HTML><HEAD>\n");
#                                       Get down to business
if ( $#ARGV == -1 ) {
#                                       Called without argument, that
#                                       means we need to ask for param.
    printf("<TITLE>AIPS HELP File Gateway</TITLE>\n");
    printf("</HEAD><BODY BGCOLOR=\"#F0F0F0\">\n");
    printf("<H1 ALIGN=CENTER>");
    printf("<A HREF=\"http://www.aips.nrao.edu/\"><IMG BORDER=1");
    printf(" ALIGN=LEFT SRC=\"/aips.gif\"");
    printf(" ALT=\"AIPS\"></A>\n");
    printf("<A HREF=\"http://www.nrao.edu/\"><IMG BORDER=0 ");
    printf(" ALIGN=RIGHT");
    printf(" SRC=\"/nraologo_tiny.gif\"");
    printf(" ALT=\"NRAO\"></A>\n");
    printf(" AIPS HELP files for $TST\n</H1>\n<BR CLEAR=ALL>\n");
    printf("<ISINDEX PROMPT=\"Show the AIPS HELP file for...\">\n\n");
    printf("Enter the <EM>simple</EM> name of the help file you want.\n");
    printf("For example: to see the $TST version of the help on IMAGR,\n");
    printf("you would enter<p><pre>\n");
    printf("     IMAGR\n");
    printf("</pre><p>\n");
    printf("in the search field above.<P>\n");
} else {
#                                       Got a parameter (help item).
#                                       First, get HLPIT.LIS
    if ( ! open (HLPIT, "<$HELP/HLPIT.LIS")) {
	@rawhlpit = ();
    } else {
	@rawhlpit = <HLPIT>;
	close(HLPIT);
    }
#                                       If we got it, uppercase all the
#                                       items in it and zap the dashes.
    @hlpit = ();
    if ($#rawhlpit != -1) {
	for ($i = 0; $i <= $#rawhlpit; $i++) {
	    $h = $rawhlpit[$i];
	    chomp($h);
	    $h =~ s/^-//;
	    $h = uc($h);
	    next if ($h =~ /^HLP/);
	    $hlpit[$i] = $h;
	}
    }
#                                       Now worry about the help file
    $item = $ARGV[0];
    $item = uc(substr($item, 0, 10));
    $item =~ s/[^A-Z0-9]//g;
    $file = "$HELP/$item" . ".HLP";
    if ( ! -f $file ) {
        foreach $topic (@hlpit) {
            if ($topic =~ /^$item/) {
                $file = "$HELP/$topic" . ".HLP";
                if ( -f $file ) {
                    $item = $topic; last;
                }
            }
        }
    }
    if ( ! -f $file ) {
	printf("<TITLE>No such help topic</TITLE></HEAD><BODY>\n");
	printf("<H1 ALIGN=CENTER>");
	printf("<A HREF=\"http://www.aips.nrao.edu/\"><IMG");
	printf(" BORDER=1 ALIGN=LEFT SRC=\"/aips.gif\" ALT=\"AIPS\"></A>\n");
	printf("<A HREF=\"http://www.nrao.edu/\"><IMG BORDER=0 ");
	printf(" ALIGN=RIGHT SRC=\"");
	printf("/nraologo_tiny.gif\" ");
	printf(" ALT=\"NRAO\"></A></H1>\n<H2>Not Found</H2>\n");
	printf("<BR CLEAR=ALL><HR>\n\nNo help on $item found.\n");
	printf("<HR><P><A HREF=\"$AIPSHELP\">Back to search page</A>");
	printf("</BODY></HTML>");
	exit;
    }
    if ( ! open (H, "<$file") ) {
	printf("<TITLE>File Access Denied</TITLE></HEAD><BODY>\n");
	printf("<H2>Help on $item Unreadable</H2>\n\n");
	printf("Unable to open $TST/HELP/$item.HLP\n");
	printf("<P>Check the file exists and that the permissions\n");
	printf("allow anyone to read it.</P>\n");
	printf("</BODY></HTML>");
	exit;
    }
    printf("<TITLE>AIPS HELP file (version $TST) for $item</TITLE>\n");
    printf("</HEAD><BODY>\n");
    printf("<H1 ALIGN=CENTER>");
    printf("<A HREF=\"http://www.aips.nrao.edu/\"><IMG BORDER=1 ");
    printf(" ALIGN=LEFT SRC=\"/aips.gif\"");
    printf(" ALT=\"AIPS\"></A>\n");
    printf("<A HREF=\"http://www.nrao.edu/\"><IMG BORDER=0 ");
    printf(" ALIGN=RIGHT");
    printf(" SRC=\"/nraologo_tiny.gif\" ");
    printf(" ALT=\"NRAO\"></A>\n");
    printf(" AIPS HELP file for $item in $TST\n</H1>\n");
    printf("<BR CLEAR=ALL><HR>");

    $now = &ctime(time);
    printf("<H4 ALIGN=CENTER>As of $now</H4>\n");
    printf("<HR>\n");

    $inputs = 0;
    $dohelp = 0;
    $explain = 0;
    $helpexpl = 0;
    $iitem = $item;
    while (<H>) {
	next if /^\;/;		# Skip comments
	if (/^$iitem\s*LL*UU*/) {
				# and the POPS column definer
	    $inputs = 1;
	    next;
	}
	chop;
	s/%/ percent/g;
	if (/^$iitem/) {
	    if ((! $helpexpl)) {
		s/$item\s*://;
		s/^$item\s*/ /;
		printf("<STRONG>$item</STRONG>:");
		printf("$_\n<H3>INPUTS</H3><PRE>\n");
		$inputs = 1;
                $iitem = "Garbage item";
		next;
	    }
	} elsif (/^----/) {
	    printf("</PRE><HR><PRE>\n");
	    if ($inputs) {	#
		$inputs = 0; $dohelp = 1; $helpexpl = 1;
		printf("</PRE><H3>HELP SECTION</H3><PRE>\n");
	    } elsif ($dohelp) {
		$dohelp = 0; $explain = 1;
		printf("</PRE><H3>EXPLAIN SECTION</H3><PRE>\n");
	    }
	    next;
	}
        if (/^$/) {
            printf("\n");
	} elsif (! /^-*$/ ) {
	    if ((! $helpexpl)) {
                s/^([A-Z][A-Z0-9]{3,9})\b/&aiprpl($1)/e;
                s/^([A-Z][A-Z0-9]{0,2})\b/&aipepl($1)/e;
		printf("$_\n");
	    } elsif ($helpexpl) {
                s/\b([A-Z][A-Z0-9]{3,9})\b/&aiprpl($1)/ge;
                s/\b([A-Z][A-Z0-9]{0,2})\b/&aipepl($1)/ge;
		printf("$_\n");
	    } else {
		printf("$_\n");
	    }
	}
    }
}
printf "</PRE><HR><DIV ALIGN=RIGHT>";
printf '<A HREF="http://www.aips.nrao.edu/">';
printf "AIPS</A></DIV>";
printf "<ADDRESS><!--see above--></ADDRESS>\n\n</BODY></HTML>\n";
