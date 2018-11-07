#!/usr/bin/perl
# -*-Perl-*-
#-----------------------------------------------------------------------
#;  Copyright (C) 2004-2018
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
$TST = "31DEC18";
$HELP = "/home/AIPS/31DEC18/HELP";
#                                       Secure/Taint requirements.
$ENV{'PATH'} = "/bin:/usr/bin";
$ENV{'IFS'} = " ";
#                                       Display of time in output.
require 'ctime.pl';
#                                       Called without argument - error
if ( $#ARGV == -1 ) {
    printf("What chapter do you want?\n");
    exit;
}
#                                       function
$skip1 = "AIPS";
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
    if (($aname ne $skip1) && ($aname ne $skip2) && ($aname ne $skip3) &&
        ($aname ne $skip4) && ($aname ne $skip5) && ($aname ne $skip6) &&
        ($aname ne $skip7) && ($aname ne $skip8) && ($aname ne $skip9)) {
        foreach $topic (@hlpit) {
#                                                    Note: minmatch!!!!!
            if ($topic =~ /^$aname/) {
                $file = "$HELP/$topic" . ".HLP";
                if ( -f $file ) {
                    $aname = "\\zxhlp\{$aname\}";
                    last;
                }
            }
        }
    }
    $aname;
}
sub aipepl {
    local($aname) = $_[0];
    if (($aname ne $skip1) && ($aname ne $skip2) && ($aname ne $skip3) &&
        ($aname ne $skip4) && ($aname ne $skip5) && ($aname ne $skip6) &&
        ($aname ne $skip7) && ($aname ne $skip8) && ($aname ne $skip9)) {
        foreach $topic (@hlpit) {
#                                                    Note: exact match
            if ($topic =~ /^$aname$/) {
                $file = "$HELP/$topic" . ".HLP";
                if ( -f $file ) {
                    $aname = "\\zxhlp\{$aname\}";
                    last;
                }
            }
        }
    }
    $aname;
}
#                                       Got a parameter (help item).
#                                       First, get HLPIT.LIS
if ( ! open (HLPIT, "<$HELP/HLPIT.LIS")) {
    printf("HLPIT.LIS not found");
    exit;
}
@rawhlpit = <HLPIT>;
close(HLPIT);
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
#                                       Now worry about the CookBook file
$item = $ARGV[0];
$ifile = "LaCook$item" . ".tex";
$ofile = "html/LaHook$item" . ".tex";
#                                       open input
if ( ! open (I, "<$ifile") ) {
   printf("Unable to open input file\n");
   exit;
}
#                                       Open output
if ( ! open (O, ">$ofile") ) {
   printf("Unable to open output file\n");
   exit;
}
#                                       Read loop input
$verbatim = 0;
while (<I>) {
    chop;
    if (/^$/) {
        printf(O "\n");
    } elsif (/^\\bve$/) {
        $verbatim = 1;
        print (O "$_\n");
    } elsif (/^\\end\{verbatim\}/) {
        $verbatim = 0;
        print (O "$_\n");
    } elsif ($verbatim) {
        print (O "$_\n");
    } elsif (/^\\[Ss]ubs|^\\[Ss]ect|^\\chapts/) {
        print (O "$_\n");
    } elsif (/^\\appen|^\\APPEN|^\\Appen/) {
        print (O "$_\n");
    } elsif (/^\\caption|^\\newcommand/) {
        print (O "$_\n");
    } else {
        s/\b([A-Z][A-Z0-9]{3,9})\b/&aiprpl($1)/ge;
        s/\b([A-Z][A-Z0-9]{0,2})\b/&aipepl($1)/ge;
        print (O "$_\n");
    }
}

