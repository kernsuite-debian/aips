#!/usr/bin/perl
# NOTE: if you have to change the line above, change it also in
# $AIPS_VERSION/SYSTEM/UNIX/DADEVS.PL!
#-----------------------------------------------------------------------
#;  Copyright (C) 2000-2016
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
# The install wizard we've always wanted.  I hope.
#
# First attempt.  Does the following:
#
#    1.  Offer to pull over $myvers.tar.gz if not found (now works!)
#    2.  Unpack $myvers.tar.gz (check dates, do if newer, etc)
#    3.  Discovery: arch, old versions, binaries, data areas, hosts,
#        printers, paper type, readline library (GETRLS sanity check).
#    4.  Get user input through a series of screens/menus:
#          - Destination directory (AIPS_ROOT)
#          - File Ownerships (group write or not, which group)
#          - Architecture (confirmation only)
#          - Hosts
#          - Data Areas
#          - Printers and Paper format
#          - Tape Drives
#          - Services (dialog; USEUNIXSOCK true/false, default to true?)
#          - Advanced: Compiler settings, AP size, local readline.
#    5.  Commit: do stuff after reviewing settings briefly.
#
# Most of the menu presentation is modular; a GUI front-end would be
# possible in principle if someone really wanted to do that.
#
# Ported installing-from-CDROM hooks from old INSTEP1 PPM 2002.02.01
#-----------------------------------------------------------------------
# require 'getopts.pl';
# require 'ctime.pl';
                                        # flush output
$| = 1;
                                        # This should be the ONLY place
                                        # you find this string here!!
$myvers = "31DEC17";
                                        # change for frozen
$mytype = "TST";
                                        # CHANGE THIS AS NEEDED!!!  It's
                                        # the start date for the MNJ.
                                        # Should be date of creation of
                                        # TST directory on AOC master.
$begindate = "20161201";

$AIPS_NETSERVER = "ftp.aoc.nrao.edu";

                                        ########### Functions ##########
sub aipserror {
    foreach $i (@_) {
        printf STDERR "AipsWiz: Error: $i\n";
    }
}

sub say {
    foreach $i (@_) {
        printf "AipsWiz: $i\n";
    }
}
sub dsay {
    if ($opt_d) {
        foreach $i (@_) {
            printf STDERR "AipsWiz-Debug: $i\n";
        }
    }
}
sub askuser {
    my $nargs = $#_;
    my $prompt = "Your input please: ";
    dsay "Askuser: Number of args is $nargs";
    if ($nargs >= 0) {
        $prompt = $_[$nargs];
        dsay "Askuser: resetting prompt to $prompt";
        if ($nargs > 0) {
            dsay "Askuser: processing extra arguments";
            for ($i = 0; $i < $nargs; $i++) {
                printf "$_[$i]\n";
            }
        }
    }
    printf "AipsWiz: ==> $prompt: ";
    my $result = <STDIN>; chomp($result);
    return ($result);
}

sub menu {
                                        # isolate this code, get
                                        # clever later if desired.
    my ($title, $descr, $prompt) = @_;
    my $result;

    if ((!$descr) && (!$prompt)) {
                                        # nothing to do?
        exit(0);
    }
    system("clear") unless ($opt_d);
    printf "\n===================== AIPS $myvers Install Wizard";
    printf "=====================\n\n";
    if ($title) {
        printf "$title\n";
        printf "-" x length($title); printf "\n";
    }
    if ($descr) {
        printf "$descr";
    } else {
        printf "\n";
    }
    if (!$prompt) {
        $prompt = "AipsWiz: (B)ack to revious menu, (Q)uit: ";
    } else {
        printf "\n         [You may also type B for back, or Q to Quit]\n\n";
    }
    printf "AipsWiz: ===> $prompt";
    $result = <STDIN>; chomp($result);
    if (($result eq "Q") || ($result eq "q")) {
        bailout();
    }
    printf "\n";
    return $result;
}

sub defaultrc {
    dsay("Setting Default RC values");
#                                  Use current location as default
    $AIPS_ROOT = `pwd`; chomp($AIPS_ROOT);
#                                  Group: can also use $(...
    $GROUP = (getgrgid((getpwnam(getlogin))[3]))[0];
    $GROUPWRITE = "YES";
    $INSTALL_FROM_CD = "NO";
    if ($opt_c){
       $INSTALL_FROM_CD = "YES";
    }
    $INSTALL_FROM_NET = "NO";
    if ($opt_n){
       $INSTALL_FROM_NET = "YES";
    }
    $LAST_VERSION = "$myvers";
    $ARCH = "UNKNOWN";
    $IS64 = "NO";
    $SITE = "UNSET";
    $THISHOST = `uname -n`; chomp($THISHOST);
    $THISHOST =~ s/\..*$//; $THISHOST = uc($THISHOST);
    @HOSTS = ( $THISHOST );
#       %PRINTERS = ( "lp", "PS~NONE~Default Pre-Configuration Printer" );
    %PRINTERS = ( );
#       can't initialize data areas easily as AIPS_ROOT will be unknown!
    %DATA_AREAS = ( );
    %TAPE_DRIVES = ( );
    @TPHOSTS = ( "127.0.0.1" );
    $APSIZE = 5.0;
    $PRINTMED = "A";
    $FORT = "";
    $FORTOPT = "";
    $CCOM = "";
    $CCOMOPT = "";
    $LINK = "";
    $READLINE = "";
    $DOTWOLIB = "NO";
    $DOWNLOADED = "";
    $UNPACKED = "";
}
sub readrc {
    my $defrc = $ENV{'HOME'} . "/.AIPSRC";
    if (! -f $defrc) {
        dsay "No default file $defrc; call defaultrc";
        defaultrc();
    } else {
        if (!open (RCFILE, "<$defrc")) {
            aipserror("Cannot read $defrc: $!",
                      "Will use default values instead");
            defaultrc();
        } else {
            $lineno = 0;
            while (<RCFILE>) {
                chomp; $lineno++;
                s/\#.*$//;
                next if (/^\s*$/);
                if (/^\s*(\w+)\s*=\s*(.*)$/) {
                    $keyw = $1; $val = $2;
                    if ($val =~ /".*"/) {
                        $string = "\$" . $keyw . ' = ' . $val;
                    } else {
                        $string = "\$" . $keyw . ' = "' . $val . '"';
                    }
                    if ($keyw =~ /^HOSTS/) {
                        $val =~ s/"(.*)"/$1/;
                        @HOSTS = split(" ", $val);
                    } elsif ($keyw =~ /^PRINTERS/) {
                        $val =~ s/"(.*)"/$1/;
                        @temp = split(" ", $val);
                        foreach $i (@temp) {
                            ($obj, $ttt) = split(":", $i);
                            ($type, $opts, $descr) = split("~", $ttt);
                            $descr =~ s/_/ /g;
                            $PRINTERS{$obj} = "$type~$opts~$descr";
                        }
                    } elsif ($keyw =~ /^DATA_AREAS/) {
                        $val =~ s/"(.*)"/$1/;
                        dsay "Read data areas $val, splitting...";
                        @temp = split(" ", $val);
                        foreach $i (@temp) {
                            dsay "  - doing $i";
                            ($area, $symlink) = split(":", $i);
                            dsay "  - split into $area and $symlink";
                            $DATA_AREAS{$area} = $symlink;
                        }
                    } elsif ($keyw =~ /^TAPE_DRIVES/) {
                        dsay "Found TAPE_DRIVES header";
                        $val =~ s/"(.*)"/$1/;
                        @temp = split(" ", $val);
                        foreach $i (@temp) {
                            ($drive, $descr) = split(":", $i);
                            $descr =~ s/_/ /g;
                            dsay "got host;drive $drive, descr $descr";
                                        # Host is encoded in device name
                                        # but we don't decode that here!
                            $TAPE_DRIVES{$drive} = "$descr";
                        }
                    } elsif ($keyw =~ /^TPHOSTS/) {
                        $val =~ s/"(.*)"/$1/;
                        @TPHOSTS = split(" ", $val);
                    } else {
                        eval $string;
                        if ($GROUPWRITE =~ /YES/) {
                            umask(0022);
                        } else {
                            umask(0002);
                        }
                                        # may need more sanity checking!
                    }
                } else {
                    aipserror ".AIPSRC, ignoring line $lineno";
                }
            }
        }
    }
}
sub writerc {
    my $defrc = $ENV{'HOME'} . "/.AIPSRC";
    if (-f $defrc) {
        $old = $defrc . ".old";
        unlink($old);
        rename($defrc, $old);
    }
    if (!open (AIPSRC, ">$defrc")) {
        aipserror ("Cannot create $defrc", "reason: $!");
    } else {
#                                       $now = &ctime(time); chop($now);
        $now = usertime();
        printf AIPSRC "# AIPSRC created by install.pl on $now\n#\n";
        printf AIPSRC "LAST_VERSION = $LAST_VERSION\n";
        printf AIPSRC "AIPS_ROOT = $AIPS_ROOT\n";
        printf AIPSRC "GROUP = $GROUP\n";
        printf AIPSRC "GROUPWRITE = $GROUPWRITE\n";
        printf AIPSRC "INSTALL_FROM_CD = $INSTALL_FROM_CD\n";
        printf AIPSRC "INSTALL_FROM_NET = $INSTALL_FROM_NET\n";
        printf AIPSRC "ARCH = $ARCH\n";
        printf AIPSRC "IS64 = $IS64\n";
        printf AIPSRC "SITE = $SITE\n";
        printf AIPSRC "THISHOST = $THISHOST\n";
        $string = "";
        foreach $thing (@HOSTS) {
            $string .= "$thing ";
        }
        $string =~ s/\s$//;
        printf AIPSRC "HOSTS = \"$string\"\n";
        $string = "";
        foreach $thing (keys %PRINTERS) {
            $string .= "$thing:";
            ($ptype, $opts, $descr) = split("~", $PRINTERS{$thing});
            $descr =~ s/\s/_/g;
            $string .= "$ptype~$opts~$descr ";
        }
        $string =~ s/\s$//;
        printf AIPSRC "PRINTERS = \"$string\"\n";
        $string = "";
        foreach $thing (keys %DATA_AREAS) {
            $string .= "$thing:$DATA_AREAS{$thing} ";
            dsay "Adding $thing:$DATA_AREAS{$thing} to DATA_AREAS";
        }
        $string =~ s/\s$//;
        printf AIPSRC "DATA_AREAS = \"$string\"\n";
        $string = "";
        foreach $thing (keys %TAPE_DRIVES) {
            $descr = $TAPE_DRIVES{$thing};
            $descr =~ s/\s/_/g;
            $string .= "$thing:$descr ";
                                        # Host is encoded in device name
                                        # but we don't decode that here!
        }
        $string =~ s/\s$//;
        printf AIPSRC "TAPE_DRIVES = \"$string\"\n";
        $string = "";
        foreach $thing (@TPHOSTS) {
            $string .= "$thing ";
        }
        $string =~ s/\s$//;
        printf AIPSRC "TPHOSTS = \"$string\"\n";
#        printf AIPSRC "APSIZE = $APSIZE\n";
        printf AIPSRC "PRINTMED = $PRINTMED\n";
        printf AIPSRC "FORT = $FORT\n";
        printf AIPSRC "FORTOPT = $FORTOPT\n";
        printf AIPSRC "CCOM = $CCOM\n";
        printf AIPSRC "CCOMOPT = $CCOMOPT\n";
        printf AIPSRC "LINK = $LINK\n";
        printf AIPSRC "READLINE = $READLINE\n";
        printf AIPSRC "DOTWOLIB = $DOTWOLIB\n";
        printf AIPSRC "DOWNLOADED = $DOWNLOADED\n";
        printf AIPSRC "UNPACKED = $UNPACKED\n";
        printf AIPSRC "\n#---- End of AIPSRC file\n";
    }
}
sub bailout {
    writerc(); printf "\n";
    say "Install Wizard stopping.  Current settings saved in ~/.AIPSRC";
    say "Abandoning ship!  Goodbye.";
    printf "=" x 72; printf "\n";
    exit(42);
}
sub filemtime {
    my ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst);
    my $file = @_[0];
    if ( -r $file) {
        ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
            gmtime((stat($file))[9]);
        $year += 1900; $mon++;
        return sprintf ("%4.4d.%2.2d.%2.2d:%2.2d:%2.2d.%2.2d",
                        $year, $mon, $mday, $hour, $min, $sec);
    } else {
                                        # nada, undefined.
        return;
    }
}
sub mnjfilemtime {
    my ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst);
    my $file = @_[0];
    if ( -r $file) {
        ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
            gmtime((stat($file))[9]);
        $year += 1900; $mon++;
        return sprintf ("%4.4d%2.2d%2.2d", $year, $mon, $mday);
    } else {
                                        # nada, undefined.
        return;
    }
}
sub currtime {
    my ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst);
    ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
        gmtime(time);
    $year += 1900; $mon++;
    return sprintf ("%4.4d.%2.2d.%2.2d:%2.2d:%2.2d:%2.2d",
                    $year, $mon, $mday, $hour, $min, $sec);
}
sub usertime {
    my ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst);
    ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
        localtime(time);
    $year += 1900; $mon++;
    return sprintf ("%4.4d.%2.2d.%2.2d:%2.2d:%2.2d:%2.2d",
                    $year, $mon, $mday, $hour, $min, $sec);
}
sub longline {
    my ($string, $limit) = @_;
    my $dest = "";
    if (!$limit) {
        $limit = 60;
    }
    if (!$string) {
        return "";
    }
    $l = length($string);
    while ($l > 0) {
        $dest .= substr($string, 0, $limit);
        $dest .= "\\\n\t\t";
        $rest = substr($string, $limit);
        $string = $rest;
        $l = length($string);
    }
    $dest = substr($dest, 0, length($dest) - 4);
    return $dest;
}

                                        # -d for debug/verbose messages
                                        # -f tarball-location NOT USED
                                        # -n net-location
                                        # -c CDROM installation
# &Getopts('dnf:');

#       $usage = "\nUsage:\tinstall.pl [-f tarball | -d | -c | -n]\n\n";
$usage = "\nUsage:\tinstall.pl  [ -d | -c | -n | -dn | -nd ]\n\n";
$usage .= "\td\tTurn on debug messages;\n";
# $usage .= "\tf\tLocation of $myvers.tar.gz (default is here)\n";
$usage .= "\tc\tTurn on binary installation from CDrom\n";
$usage .= "\tn\tTurn on network (binary) installation\n\n";

$opt_d = 0;
$opt_n = 0;
$opt_c = 0;
while($ARGV[0]=~/^-/){
    $_ = $ARGV[0];
    if ( /^-nd$/ ) { $opt_n = 1; $opt_d = 1; shift @ARGV; next }
    if ( /^-dn$/ ) { $opt_n = 1; $opt_d = 1; shift @ARGV; next }
    if ( /^-n$/ ) { $opt_n = 1; shift @ARGV; next }
    if ( /^-d$/ ) { $opt_d = 1; shift @ARGV; next }
    if ( /^-c$/ ) { $opt_c = 1; shift @ARGV; next }
die<<HERE;
illegal option: $_
$usage
HERE
}
                                        # on Interrupt, write RC file
$SIG{INT} = \&bailout;

 STARTOVER:
                                        # test for root
    $grps = `whoami` ;
if ($grps =~ /^root$/) {
   $found = "yes";
}
else {
   $grps = `groups`;
   $found = "no";
   foreach $g (split(" ", $grps)) {
       if ($g =~ /^root$/) {
          $found = "yes";
          last;
       }
   }
}
if ($found =~ /yes/) {
   aipserror("You are logged in as root or are a member of the",
      "root group.  This leads to all sorts of trouble.\n",
      "Quitting now - log in as a normal user and run",
      "install.pl from that account.\n\n");
   bailout();
}

                                        # Read in the RC file
dsay("reading your ~/.AIPSRC file...");
readrc();
                                        # CCOPTS can have INC from old vers.
if ($LAST_VERSION) {
    if ($myvers !~ /$LAST_VERSION/) {
        $dum = askuser("WARNING!  The ~/.AIPSRC file is from $LAST_VERSION\n",
                       "while this install wizard is for $myvers.\n",
                       "You may find some settings that point to files from\n",
                       "the wrong version, especially the compiler options.\n",
                       "Please BE CAREFUL to check these!!!!\n\n",
                       "Press <ENTER> to continue:");
    }
}
                                        # sniff around for tarball
                                        # but not if we have a dang CD!
if (($ENV{'AIPSCDROM'} =~ /YES/) || ($opt_c)) {
    dsay("Ah.  I see you have a CDROM!");
    $INSTALL_FROM_CD = "YES";
    $INSTALL_FROM_NET = "NO";
                                        # we SHOULD be in the right area...
                                        # this could go badly wrong :-(
                                        # Should we ask if different?
    $AIPS_ROOT = `pwd`; chomp($AIPS_ROOT);
} else {
    if ($INSTALL_FROM_CD =~ /YES/) {
       say "Were installing from CD but not now -";
       say "use option -c or edit ~/.AIPSRC and try again";
       bailout();
   }
}
if ($opt_n) {
    dsay("Ah.  I see you don't want to mess with compilers!");
    $INSTALL_FROM_NET = "YES";
    $INSTALL_FROM_CD = "NO";
} else {
    if ($INSTALL_FROM_NET =~ /YES/) {
       say "You were installing from the net but not now -";
       say "use option -n or edit ~/.AIPSRC and try again";
       bailout();
   }
}
                                        # find tarball if needed
if ($INSTALL_FROM_CD !~ /YES/ && $INSTALL_FROM_NET !~ /YES/) {
    $tarball = "$myvers" . ".tar.gz";
    dsay("Checking for $tarball...");
    if ( ! -r "$tarball" ) {
       dsay ("Not found here");
       if ( -f "$AIPS_ROOT/$tarball" ) {
           $tarball = "$AIPS_ROOT/$tarball";
           dsay ("Found in $AIPS_ROOT");
       } elsif ( -f "$ENV{'HOME'}/$tarball" ) {
           $tarball = $ENV{'HOME'} . "/$tarball";
           dsay ("Found in $ENV('HOME')");
       } else {
          $yn = askuser("I cannot find a tarball.  Did you mean to do",
              "a binary installation? [Y/N, default N] ");
          if ($yn =~ /[Yy]/) {
             $INSTALL_FROM_NET = "YES";
             $INSTALL_FROM_CD = "NO";
          }
       }
    } else {
        $here = `pwd`; chomp($here);
        $tarball = $here . "/" . $tarball;
        dsay("Found right here: $tarball");
    }
}
                                        ########## Screen 0 ############
                                        # Welcome/Intro
if ($SITE !~ /^UNSET$/) {
    $mn = menu("Screen 0: Re-Introduction",
               "Welcome back to the AIPS Install Wizard (AipsWiz).\n" .
               "It would appear you've been here before.\n\n" .
               "If you REALLY want to start over, you may want to remove\n" .
               "the .AIPSRC file in this account's home directory.\n\n" .
               "Otherwise, you can skip forward to a specific screen by\n" .
               "entering its number at the prompt.  The default action\n" .
               "is to simply start at screen 1.  Screens are:\n\n" .
               "  1.  Optionally download $myvers tarball (skipped if CD or network (binary) download)\n" .
               "  2.  Confirm that tarball is correct (skipped if CD or network)\n" .
               "  3.  Choose AIPS_ROOT, optionally unpack tarball (skipped if CD or network)\n" .
               "  4.  Group ownership, group write enable/disable\n" .
               "  5.  Site name, Architecture\n" .
               "  6.  Additional hosts\n" .
               "  7.  Data area definition\n" .
               "  8.  Printer setup\n" .
               "  9.  Tape drives\n" .
               " 10.  Advanced settings\n" .
               " 11.  Final check before doing the install\n" ,
               "Press <ENTER> or <RETURN>, or enter a screen number: ");
    if (($mn =~ /[Bb]/) || ($mn =~ /[Qq]/)) {
        bailout();
    } elsif ($mn =~ /^[Qq]$/) {
        bailout();
    } elsif ($mn =~ /[0-9][0-9]*/) {
                                        # forward to specific screen
                                        # this is cool
        if (($mn > 0) && ($mn < 13)) {
            $str = "goto SCREEN_$mn";
            eval $str;
        }
    }
} else {
    $yn = menu("Screen 0: INTRODUCTION",
               "Welcome to the AIPS Install Wizard (AipsWiz)!\n" .
               "It will do the following:\n\n" .
               "  1.  Look for (and/or download) the 'tarball' for $myvers\n" .
               "      (skipped if installing from CD or network)\n" .
               "  2.  Unpack it wherever you specify (skipped if CD or network)\n" .
               "  3.  Ask you for various settings through a set of menus\n" .
               "  4.  Review your settings, and\n" .
               "  5.  Do the installation with no further input needed.\n\n" .
               "Things can go wrong (!) so this procedure is not perfect,\n" .
               "but it saves state in a file .AIPSRC in your home area.\n" .
               "So you can easily restart an interrupted install attempt,\n" .
               "without re-entering all your settings.  You can also go\n" .
               "back a screen by typing 'B' to most prompts, or you\n" .
               "can Quit at any menu by entering 'Q'.\n",
               "Press <ENTER> or <RETURN> to get started: ");
    if (($yn =~ /[Bb]/) || ($yn =~ /[Qq]/)) {
        bailout();
    }
}
                                        ########## Screen 1 ############
                                        # Download stage.
 SCREEN_1:

                                        # Skip most of tarball stuff
if ($INSTALL_FROM_CD =~ /YES/ || $INSTALL_FROM_NET =~ /YES/ ) {
    goto SCREEN_4;
} elsif ( ! -f $tarball ) {
    if ($DOWNLOADED) {
        say "You seem to already have the $myvers version of AIPS";
        say "(the recorded UT time of download is $DOWNLOADED)";
        $yn = menu("Screen 1: DOWNLOAD",
                   "I cannot find $tarball in an obvious place.\n" .
                   "(checked here, your login area, and AIPS_ROOT)\n" .
                   "If it is elsewhere, please Quit and CD to there.\n" .
                   "Or it can be automatically fetched via ftp from\n" .
                   "the master AIPS FTP server at NRAO.\n",
                   "Would you like me to do this? (Y/N) [Y]");
        if (($yn =~ /[Bb]/) || ($yn =~ /[Nn]/) || ($yn =~ /[Qq]/)) {
           bailout();
           }
        }
    goto SCREEN_1_RETRY;
} else {
                                        # Some tarball or other found...
                                        # get its date (mtime).
    $tarballtime = filemtime($tarball);
    $text =  "Found a 'tarball' $tarball\n";
    $text .= "         ..................Dated $tarballtime (UT)\n";
    dsay ($text);
    $current = currtime();
    if ($DOWNLOADED) {
        if ($tarballtime > $DOWNLOADTIME) {
            $text .= "         ......... downloaded on $DOWNLOADED (UT)\n";
        } else {
            $text .= " It's OLDER than the download time ($DOWNLOADTIME UT)?\n";
        }
        $text .= "         ....... current time is $current (UT)\n\n";
    } else {
                                        # Careful: someone may have
                                        # already downloaded by hand!
        $text .= "         and the current time is $current (UT)\n\n";
        $text .= "If there's too much time between these two dates\n";
        $text .= "you may prefer to get a newer version (the file\n";
        $text .= "is generated daily).  If it's less than 1 week,\n";
        $text .= "it's probably fine.\n\n";
        $DOWNLOADED = $current;
    }
    $yn = menu("Screen 1: DOWNLOAD",
               $text .
               "You can:\n\n" .
               "   (G) Get a newer copy from NRAO's ftp site,\n" .
               "       (the file is generated nightly)\n" .
               "   (S) Skip the download and use the existing tarball, or\n" .
               "   (Q) Quit now.\n\n" .
               "NOTE: if by some chance the automatic ftp fetch fails, you\n" .
               "      may want to fetch it yourself by hand, from\n\n" .
               "      ftp.aoc.nrao.edu:/aips/$myvers/$myvers.tar.gz\n\n",
               "What do you want to do? [default S] ");
                                        # Back and Quit do the same here
    if (($yn =~ /[Nn]/) || ($yn =~ /[Bb]/) || ($yn =~ /[Qq]/)) {
        bailout();
    } elsif ($yn =~ /[Gg]/) {
        goto SCREEN_1_RETRY;
    } else {
        goto SCREEN_2;
    }

  SCREEN_1_RETRY:
    $TIMESTAMP = currtime();
    dsay "set \$| to 1, timestamp $TIMESTAMP";
    unlink("ftp.cmds") if ( -f "ftp.cmds");
    open(FTPIN, ">ftp.cmds");
    select(FTPIN); $| = 1;
    printf(FTPIN "user anonymous aipsinstaller@\n");
    printf(FTPIN "cd pub/software/aips/$myvers\n"); #
    printf(FTPIN "bin\n");
    printf(FTPIN "get $tarball\n");
    close(FTPIN);
    select(STDOUT);
    dsay "Created ftp.cmds file...";

    say "About to retrieve the file.  This may take a while.\n";

    open(FTP, "ftp -n ftp.aoc.nrao.edu <ftp.cmds|");
    select(FTP); $| = 1;
    while (<FTP>) {
        chomp;
        dsay "ftp: $_";
    }
    close(FTP);
    select(STDOUT);
    if (! -f $tarball) {
        $yn = askuser("That ftp attempt did not seem to work.",
                      "Try it again? [Y/N, default N] ");
        if ($yn =~ /[Yy]/) {
            goto SCREEN_1_RETRY;
        } else {
            bailout();
        }
    }
    unlink("ftp.cmds");
    say "The $tarball file seems to be there now.   Proceeding...";
    $DOWNLOADED = $TIMESTAMP;
    writerc();
}
                                        ########## Screen 2
                                        # Confirmation
 SCREEN_2:
$yn =
    menu("Screen 2: Confirmation",
         "Proceeding with $myvers install/upgrade.  Please check:\n\n"
         . `ls -lL $tarball` . "\n"
         . "that this is the file you want (should be >117 Mbytes)\n",
         "Proceed [Y/N, default Y] ");
if ($yn =~ /[Bb]/) {
    goto SCREEN_1;
}
                                        ######### Screen 3
                                        # Unpack it
 SCREEN_3:
if ($UNPACKED) {
    if ($UNPACKED =~ /Unpacked in (.*) at (.*)/) {
        $uroot = $1; $utime = $2;
    } else {
        aipserror("Cannot parse UNPACKED string: $UNPACKED",
                  "This string tells me where and when you unpacked" .
                  " the $myvers", "version of AIPS.  Because it is" .
                  " malformed, I cannot proceed.");
        bailout();
    }
    if ( ! -d "$uroot/$myvers" ) {
        aipserror("AIPS $myvers unpacked in $uroot/",
                  "but I can no longer find it there!  Giving up.\n",
                  "This $uroot was retrieved from your .AIPSRC file,",
                  "specifically in the UNPACKED variable.  You may want to",
                  "edit that file if you moved your AIPS installation since",
                  "this script was first run (and the \$SYSUNIX shell script",
                  "AIPSROOT.DEFINE too!)");
        bailout();
    }
    $aroot = menu("Screen 3: Where to unpack",
                  "You have unpacked the $myvers version of AIPS in\n" .
                  "$uroot at $utime\nso I will assume that is the " .
                  "AIPS_ROOT area.  If not, you should interrupt\n" .
                  "(ctrl-c) now, or if all is correct, just...\n",
                  "press <ENTER> or <RETURN> to continue: ");
    if ($aroot =~ /^[Bb]/) {
        goto SCREEN_2;
    } elsif ($aroot =~ /^[Qq]/) {
        bailout();
    } else {
                                        # Should we let them specify
                                        # alternate AIPS_ROOT after
                                        # unpacking?  Tricky...
        goto SCREEN_4;
    }
} else {
    $aroot = menu("Screen 3: Where to unpack",
         "Please tell me where to unpack the $myvers version of AIPS\n"
         . "No blanks are allowed in the path name\n"
         . "(This is the so-called AIPS_ROOT area).\n\n"
         . "The default is $AIPS_ROOT\n", "AIPS Root area: ");
    if ($aroot) {
        $n = 0;
        foreach $h (split(" ", $aroot)) {
            $n = $n + 1;
        }
        if ($n > 1) {
            say "No blanks are allowed!";
            goto SCREEN_3;
        }
    }
}
if (!$aroot) {
    $aroot = $AIPS_ROOT;
}
if ($aroot =~ /^[Bb]$/) {
    goto SCREEN_2;
}
if ( ! -d $aroot ) {
    $yn =
        askuser("$aroot does not exist; create it? [Y/N, default Y] ");
    if ($yn =~ /[Bb]/) {
        goto SCREEN_3;
    } elsif ($yn =~ /[Qq]/) {
        bailout();
    } elsif ($yn !~ /[Nn]/) {
        if (!mkdir($aroot, 0775)) {
            $yn = askuser("Cannot create $aroot: $!\n",
                          "Try another? [Y/N, default Y] ");
            if (($yn =~ /[Qq]/) || ($yn =~ /[Nn]/)) {
                bailout();
            }
            goto SCREEN_3;
        }

    }
} else {
                                        # Y is one of the last
                                        # directories to be unpacked
    $AIPS_ROOT = $aroot;
    if ( -d "$AIPS_ROOT/$myvers/Y" ) {
        $yn = askuser("*** You appear to have already unpacked $myvers",
                      "*** (Found directory $AIPS_ROOT/$myvers/Y/;",
                      "***  This indicates the tarball has been unpacked)",
                      "*** Please confirm:\n",
                      "Skip unpacking of tarball? [Y/N, default Y]");
        if ($yn =~ /^[Bb]/) {
            goto SCREEN_2;
        } elsif ($yn =~ /^[Qq]/) {
            bailout();
        } elsif ($yn !~ /^[Nn]/) {
            goto SCREEN_4;
        }
    }
}

$AIPS_ROOT = $aroot;
writerc();
                                        # look for GNU tar.
$tar = "tar";
$tryit = `$tar --version 2>/dev/null`;
$gotit = "";
if ($tryit) {
    $gotit = "yes";
} else {
    $tar = "gtar";
    $tryit = `$tar --version 2>/dev/null`;
    if ($tryit) {
        $gotit = "yes";
    } else {
        $tar = "";
        @tryem = ("/usr/local/bin", "/usr/local/gnu/bin",
                  "/usr/gnu/bin", "/opt/gnu/bin",
                  "/opt/local/bin", "/opt/local/gnu/bin");
        foreach $dir (@tryem) {
            $tryit = $dir . "/tar";
            $tryit2 = $dir . "/gtar";
            if ( -f $tryit ) {
                $tar = $tryit;
            } elsif ( -f $tryit2 ) {
                $tar = $tryit2;
            } else {
                next;
            }
        }
        if ($tar) {
            $tryit = `$tar --version 2>/dev/null`;
            if ($tryit) {
                $gotit = "yes";
            }
        }
    }
}
if (!$gotit) {
                                        # no gnu tar, check for gunzip
                                        # Thanks to Alan Chipperfield
                                        # for this recipe.
    $gunzok = `if gunzip --version 2>/dev/null; then echo OK;fi`;
    if ($gunzok) {
        $tarcmd = "gunzip -c $tarball | tar xvf -";
    } elsif (!$UNPACKED) {
        aipserror("PROBLEM!  You do not appear to have GNU tar, nor gunzip",
                  "so there is no way to unpack $tarball!",
                  "You may want to run 'gunzip' on it with another computer",
                  "and then alter the .AIPSRC file here to indicate that",
                  "the file is indeed unpacked.  Or you can get and install",
                  "either gzip/gunzip (same) or GNU tar, available at",
                  "http://www.gnu.org/ and many other sites.");
        bailout();
    }
} else {
                                        # Note: tar xfU will unlink
                                        # existing files first which is
                                        # what we want, I think.  But
                                        # this is a GNU tar extension.
                                # Make this tar xvf - and use a pipe like
    $tarcmd = "$tar zxvfU $tarball";
}
if ( -d "$AIPS_ROOT/$myvers" ) {
                                        # Check dates here.
    $zmtime = filemtime($tarball);
    if ( -f "$AIPS_ROOT/$myvers/TIMESTAMP" ) {
        $tmtime = filemtime("$AIPS_ROOT/$myvers/TIMESTAMP");
    } else {
        $tmtime = filemtime("$AIPS_ROOT/$myvers");
    }
    if ($tmtime < $zmtime) {
        say ("Time stamp on $tarball",
             "is newer than $AIPS_ROOT/$myvers",
             "This means you MAY be upgrading; good!");
    } else {
        say ("Time stamp on $tarball",
             "is older than $AIPS_ROOT/$myvers",
             "This means you MAY have already unpacked it.");
    }

                                        # Make absolutely sure...
    $yn = askuser("WARNING!  $AIPS_ROOT/$myvers/ already exists!",
                  "Proceeding will OVERWRITE the contents of that directory\n",
                  "Are you SURE that you want to do this? [Y/N, default N] ");
    if ($yn =~ /[Qq]/) {
        bailout();
    } elsif ($yn !~ /[Yy]/) {
        goto SCREEN_3;
    }
    if (!$gotit) {
                                        # Need to recursively chmod +w
                                        # everything.  Make dang sure
                                        # the user agrees to this!
        $yn = askuser("Using non-GNU tar (which you seem to have)",
                      " to unpack $tarball",
                      "may cause some errors unless all files in the",
                      "$AIPS_ROOT/ directory hierarchy are made writable.",
                      "The command 'chmod -R +w' will make it so.",
                      "Make $AIPS_ROOT files all writable? [Y/N, default Y] ");
        if ($yn =~ /[Qq]/) {
            bailout();
        } elsif ($yn =~ /[Bb]/) {
            goto SCREEN_3;
        } elsif ($yn =~ /[Nn]/) {
            $dum = askuser("Very well, but this MAY NOT WORK.",
                           "If there are read-only files present, they",
                           "CANNOT BE REPLACED with new versions!",
                           "Press <ENTER> to acknowledge:");
        } else {
            say "Starting 'chmod -R +w $AIPS_ROOT'...";
            $result = system("chmod -R +w $AIPS_ROOT") / 256;
            if ($result == 0) {
                say "Done, good status return.";
            } else {
                $yn = askuser ("Got error code $result; it did not work!",
                               "Please check that you have permission to",
                               "modify these files and directories.  I",
                               "recommend that you quit and fix the problem.",
                               "Quit? [Y/N, Default Y]: ");
                if ($yn =~ /[Bb]/) {
                    goto SCREEN_3;
                } elsif ($yn !~ /[Nn]/) {
                    bailout();
                } else {
                    $dum = askuser("Very well, but THIS MAY NOT WORK.",
                                   "If there are any read-only files present,",
                                   "they will PREVENT the new versions from",
                                   "being unpacked.",
                                   "Press <ENTER> to proceed: ");
                }
            }
        }
    }
}
                                        # Do the tar... or defer?
if (chdir($AIPS_ROOT)) {
    if ( -d "$AIPS_ROOT/TEXT") {
       say ("Moving TEXT directory to TEXT.OLD",
            " delet it when the install has worked.");
       system("mv -f $AIPS_ROOT/TEXT $AIPS_ROOT/TEXT.OLD");
       }
    say ("Unpacking the 'tarball' $tarball",
         "in the $AIPS_ROOT directory",
         "with command: $tarcmd",
         "This may generate some errors if you are overwriting, but",
         "these ('Cannot unlink: directory not empty') are benign.",
         "(printing a dot every 100 files for your amusement...");
    open(PIPE, "$tarcmd|") ||
        aipserror ("cannot execute $tarcmd: $!") && bailout();
    $nlines = 0; $| = 1;
    while (<PIPE>) {
        $nlines++;
                                        # this may not work unless we
                                        # flush things... some feedback.
        if (((int($nlines/100))*100) == $nlines) {
            printf ".";
        }
    }
    close(PIPE); printf ("\n");
    if ($nlines == 0) {
        say ("OOPS.  Zero files?  Not very useful, I'd say.",
             "The unpacking apparently did not work.  Perhaps you have",
             "an older version of GNU tar, or your system's native tar",
             "did not like the options I used:\n\n\t$tarcmd\n\n",
             "Please try to unpack it yourself and then restart me.",
             "If 1000s of file names came out, tar worked but fooled me.",
             "Restart me and I should be able to go on.");
        bailout();
    }
    say "Unpacking apparently worked (total of $nlines files)";
    $UNPACKED = "Unpacked in $AIPS_ROOT at " . localtime();
    $ttt = $AIPS_ROOT . "/" . $myvers . "/TIMESTAMP";
    if ( ! -f $ttt ) {
        open(FOO, ">$ttt");
        close (FOO);
    }
    writerc();
    $dum = askuser("Press <ENTER> to continue:");
} else {
    say "Error: cannot change directory to $AIPS_ROOT.  Permissions?";
    bailout();
}
                                        ######## Screen 4.
                                        # File Ownerships (group write
                                        # or not, which group)
                                        # $GROUP, $GROUPWRITE
 SCREEN_4:
$grp = menu("Screen 4: Group Ownership",
            "If more than one account is going to be using AIPS, " .
            "you should select\na 'Unix group' for these accounts.  " .
            "This can,\nbut need not be, their primary group.\n\n",
            "What group should be used for AIPS? [$GROUP] ");
if (!$grp) {
    $grp = $GROUP;
} elsif ($grp =~ /^[Qq]$/) {
    bailout();
} elsif ($grp =~ /^[Bb]$/) {
    goto SCREEN_3;
}
$grps = `groups`;
$found = "no";
foreach $g (split(" ", $grps)) {
    if ($g =~ /^$grp$/) {
        $found = "yes";
        last;
    }
}
if ($found =~ /yes/) {
    $GROUP = $grp;
} else {
    aipserror("You do not seem to be a member of group $grp",
              "Possible groups you are in include:",
              $grps);
    sleep(5);
    goto SCREEN_4;
}
writerc();

$grpwrt = menu("Screen 4b: Group Write",
               "If more than one account will use AIPS, " .
               "and you are using\na group ($GROUP) to permit file" .
               "sharing, you should also allow this group\nwrite " .
               "access to certain AIPS files.\n\n",
               "Allow $GROUP to have write access? [$GROUPWRITE] ");
if (!$grpwrt) {
    $grpwrt = "$GROUPWRITE";
} elsif ($grpwrt =~ /^[Qq]$/) {
    bailout();
} elsif ($grpwrt =~ /^[Bb]/) {
    goto SCREEN_4;
}
if ($grpwrt =~ /[Yy]/) {
    $GROUPWRITE = "YES";
    umask(0022);
} else {
    $GROUPWRITE = "NO";
    umask(0002);
}
writerc();
                                        ######## Screen 5.
                                        # Architecture and site name
 SCREEN_5:
if ($SITE =~ /UNSET/) {
    $resp = menu("Screen 5: Site Name",
                 "Please enter a single word that will be used to " .
                 "describe your AIPS site.\nThis may be used as a " .
                 "directory name.  Do NOT use names which contain\n" .
                 "NRAO or OARN - these have special meaning.  Examples\n" .
                 "MYSITE, CFALAPTOP, JOHN, MARY\n" .
                 "Your site name: [required, no default, no blanks]\n");
} else {
    $resp = menu("Screen 5: Site Name",
                 "You have already indicated your site name to be $SITE\n",
                 "Press <RETURN> to confirm, or enter a new name: ");
    if (!$resp) {
        $resp = $SITE;
    }
}
if (!$resp) {
                                        # yeah, this is annoying :-)
    goto SCREEN_5;
} elsif ($resp =~ /^[Qq]$/) {
    bailout();
} elsif ($resp =~ /^[Bb]$/) {
    goto SCREEN_4;
}
$SITE = uc((split(" ", $resp))[0]);
                                        ######## Screen 5A
                                        # LAPTOP
 SCREEN_5A:
if ($THISHOST =~ /LOCALHOST/) {
    $resp = menu("Screen 5a: Site type",
                 "You have already indicated your site to be a single computer or laptop\n",
                 "Press <RETURN> to confirm, or enter N or n to deny [Y]:");
    if (!$resp) {
        $resp = "Y";
    }
} else {
    $resp = menu("Screen 5a: Site type",
                 "\n****** READ CAREFULLY - HAS CHANGED ******\n\n" .
                 "If your site consists of a single computer, especially\n " .
                 "if it may get different names at different times,\n " .
                 "it is best to force the name for AIPS to LOCALHOST.\n ",
                 "If you do not want this, enter N or n [Y]");
    if (!$resp) {
        $resp = "Y";
    }
}
if ($resp =~ /^[Yy]$/) {
    $THISHOST = "LOCALHOST";
    @HOSTS = ( $THISHOST );
} elsif ($resp =~ /^[Nn]$/) {
    if ($THISHOST =~ /LOCALHOST/) {
       $THISHOST = `uname -n`; chomp($THISHOST);
       $THISHOST =~ s/\..*$//; $THISHOST = uc($THISHOST);
       @HOSTS = ( $THISHOST );
   }
} elsif ($resp =~ /^[Qq]$/) {
    bailout();
} elsif ($resp =~ /^[Bb]$/) {
    goto SCREEN_5;
}

                                        # Discovery of $ARCH
if ($ARCH =~ /UNKNOWN/) {
    $arch = $^O;
    $mach = `uname -m 2>/dev/null`;
    $iarch = `uname -i 2>/dev/null`;
    $sarch = `uname -s 2>/dev/null`;
    $rarch = `uname -r 2>/dev/null`;
    if ($arch =~ /dec_osf/) {
        $ARCH = "ALPHA";
    } elsif ($sarch =~ /Darwin/) {
        if ($mach =~ /i[34567]86/) {
           $ARCH = "MACINT";
        } elsif ($mach =~ /x86_64/) {
            $ARCH = "MACINT";
        } else {
            $ARCH = "MACPPC";
        }
    } elsif ($arch =~ /linux/) {
                                        # could be intel, alpha, sol...
        if ($mach =~ /i[34567]86/) {
            $ARCH = "LINUX";
        } elsif ($mach =~ /x86_64/) {
            $ARCH = "LNX64";
            $IS64 = "YES";
        } elsif ($mach =~ /x86/) {
            $ARCH = "LINUX";
        } elsif ($mach =~ /[Aa]lpha/) {
            $ARCH = "AXLINUX";
        } elsif ($mach =~ /[Ss](parc|un)/) {
            $ARCH = "SOLINUX";
        } elsif ($mach =~ /[Pp](ower|pc)/) {
                                        # Not sure what we call this?
            $ARCH = "LINUXPPC";
        }
    } elsif ($arch =~ /solaris/) {
        $ARCH = "SOL";
        if ($iarch =~ /Ultra/) {
            $ARCH = "SUL";
        } elsif ($iarch =~ /i86/) {
            $ARCH = "SOL86";
        }
    } elsif ($arch =~ /irix/) {
        $ARCH = "SGI";
    } else {
        $arch = uc($sarch);
        if ($arch =~ /HP-UX/) {
            $ARCH = "HP";
            if ($mach =~ /\/7[89]*/) {
                $ARCH = "HP2";
            }
        } elsif ($arch =~ /LINUX/) {
            $ARCH = "LINUX";
            if ($mach =~ /x86_64/) {
               $ARCH = "LNX64";
               $IS64 = "YES";
            }
        } elsif ($arch =~ /AIX/) {
            $ARCH = "IBM";
        } elsif ($arch =~ /I[3-9]86/) {
            $ARCH = "LINUX";
        } elsif ($arch =~ /IRIX/) {
            $ARCH = "SGI";
        } elsif ($arch =~ /OSF1/) {
            $ARCH = "ALPHA";
        } elsif ($arch =~ /ULTRIX/) {
            $ARCH = "DEC";
        } elsif ($arch =~ /SunOS/) {
            if ($rarch =~ /^4\./) {
                $ARCH = "SUN4";
            } elsif ($iarch =~ /Ultra/) {
                $ARCH = "SUL";
            } else {
                $ARCH = "SOL";
            }
        }
    }
}

 SCREEN_5B:
$newarch = menu("Screen 5b: Architecture Confirmation",
                "Your architecture appears to be $ARCH\n\n" .
                "If this is correct, press RETURN or ENTER.\n" .
                "If not, enter the correct architecture (or ? for " .
                "a list)\n\n",
                "Enter the architecture for this system [$ARCH] ");
if (!$newarch) {
    $newarch = $ARCH;
} elsif ($newarch =~ /^[Bb]$/) {
    goto SCREEN_5;
} elsif ($newarch =~ /^[Qq]$/) {
    bailout();
} elsif ($newarch =~ /\?/) {
    say("The architectures AIPS currently knows about are:\n",
        " ALPHA    Alpha systems running Digital Unix (OSF/1)",
        " AXLINUX  Alpha systems running Linux",
        " DEC      DecStations running Ultrix (*not* Alpha/OSF1)",
        " HP       Hewlett Packard 9000/700 series (up to 735)",
        " HP2      Hewlett Packard 9000/780 or 790 series",
        " IBM      IBM RS/6000 systems running AIX",
        " LINUX    Intel PC (386 or better) running Linux",
        " LNX64    Intel PC (386 or better) running Linux in 64 bit",
        " MACPPC   MacIntosh Darwin running OS/X on PowerPC",
        " MACINT   MacIntosh Darwin running OS/X on Intel",
        " SGI      Silicon Graphics systems running IRIX",
        " SOL      Sun Sparcsystems with Solaris 2.x or later",
        " SOL86    Intel PC (386 or better) running Solaris",
        " SUL      ULTRA Sparcsystems with Solaris 2.x or later",
        " SUN4     Sparcsystems with SunOS 4.x (NOT Solaris!)\n");
    askuser("Press <ENTER> when ready to procede");
    goto SCREEN_5B;
}
if ($newarch =~ /ALPHA/) {
    $archstr="Alpha system running Digital Unix (OSF/1)";
} elsif ($newarch =~ /AXLINUX/) {
    $archstr="Alpha system running Linux";
} elsif ($newarch =~ /CVEX/) {
    $archstr="Convex running ConvexOS (OLD port, unavailable)";
} elsif ($newarch =~ /DEC/) {
    $archstr="DecStation running Ultrix (not Alpha/OSF1)";
} elsif ($newarch =~ /FREEBSD/) {
    $archstr="Intel PC running FreeBSD (UNTESTED for AIPS!)";
} elsif ($newarch =~ /HP2/) {
    $archstr="HP 9000/700 series (78x or 79x) running HP-UX";
} elsif ($newarch =~ /HP/) {
    $archstr="HP 9000/700 series (up to 735) running HP-UX";
} elsif ($newarch =~ /IBM/) {
    $archstr="IBM RS/6000 running AIX";
} elsif ($newarch =~ /LINUX/) {
    $archstr="Intel system running Linux";
} elsif ($newarch =~ /LNX64/) {
    $archstr="Intel system running Linux in 64bit";
} elsif ($newarch =~ /MACPPC/) {
    $archstr="MacIntosh Darwin running OS/X on PowerPC";
} elsif ($newarch =~ /MACINT/) {
    $archstr="MacIntosh Darwin running OS/X on Intel";
} elsif ($newarch =~ /SGI/)  {
    $archstr="Silicon Graphics running IRIX";
} elsif ($newarch =~ /SUN4/) {
    $archstr="Sun-4 (Sparc) SunOS 3.x or 4.x" ;
} elsif ($newarch =~ /SOL/) {
    $archstr="Sun-4 (Sparc) SunOS 5.x or higher";
} elsif ($newarch =~ /SOL86/) {
    $archstr="Intel PC running Solaris";
} elsif ($newarch =~ /SUL/) {
    $archstr="Sun-4 (Sparc ULTRA) SunOS 5.x or higher";
} else {
    say "This script cannot guess your architecture based on the";
    say "output of the 'uname' command:\n\n";
    system("uname -a");
    say "\nor the built-in perl osname variable ($^O).\n";
    sleep(5);
    goto SCREEN_5B;
}
$ARCH = $newarch;
writerc();
                                        ######## Screen 6.
                                        # Additional Hosts
 SCREEN_6:
$hoststring = "";
$n = 0;
foreach $h (@HOSTS) {
    $hoststring .= "$h ";
    $n++;
    if ($n == 5 * int($n/5)) {
        $hoststring .= "\n";
    }
}
$resp = menu("Screen 6: Additional hosts if any",
             "You may configure your installation so it runs on " .
             "many systems,\nsharing the AIPS ROOT area via NFS.  " .
             "The list of host(s) is now:\n\n$hoststring\n\n" .
             "Any new hosts will initially be set to arch=$ARCH, but " .
             "you can\nchange this by editing HOSTS.LIST in " .
             "$AIPS_ROOT\nwhen the install is finished.\n",
             "Enter new hosts (no commas!) or $THISHOST to reset:");
if ($resp =~ /$THISHOST/) {
    @HOSTS = ( $THISHOST );
    writerc();
    goto SCREEN_6;
} elsif ($resp =~ /^[Bb]$/) {
    goto SCREEN_5;
} elsif ($resp =~ /^[Qq]$/) {
    bailout();
} elsif ($resp) {
    foreach $h (split(" ", $resp)) {
        push @HOSTS, uc($h);
    }
}
writerc();
                                        ######## Screen 7.
                                        # Data Areas

 SCREEN_7:
undef $spoo;
foreach $d (keys %DATA_AREAS) {
    $symlink = $DATA_AREAS{$d};
    $spoo .= "  $d ";
    if ($symlink !~ /^NONE$/) {
        $spoo .= "(-> $symlink) ";
    }
    $spoo .= "\n";
}
if (!$spoo) {
    $spoo = "  (no data areas have been defined yet)\n";
}

$sug = $AIPS_ROOT . "/DATA/" . $THISHOST . "_1";
$resp = menu("Screen 7: User Data Areas",
             "At least one data area needs to be defined for AIPS " .
             "user data.\nDo it here unless there is already a DADEVS.LIST " .
             "file.  It is\nimportant that the host name in caps appear in " .
             "the data area name.\nCurrently these area(s) are defined:\n\n" .
             "$spoo\nYou can add to this list after installing " .
             "by editing the two files\nDADEVS.LIST and NETSP in the " .
             "$AIPS_ROOT/DA00/ directory.\n\nPlease enter (additional) " .
             "data area(s) now as a space-separated list,\n" .
             "NOTE: THIS MEANS NO BLANKS IN THE PATH TO THE DATA AREA\n" .
             "or the word RESET to re-set the list.\n\n" .
             "   (If you want to use symlinks, use the notation\n\n" .
             "       actual_directory_location:symbolic_link_location\n\n" .
             "    instead of a simple directory location.)\n\n" .
             "The suggested data area is $sug\n" .
             "which will be put in the list if you do not enter any others\n",
             "Data Areas: ");
if ($resp =~ /^RESET$/) {
    undef %DATA_AREAS;
    goto SCREEN_7;
} elsif ($resp =~ /^[Bb]$/) {
    goto SCREEN_6;
} elsif ($resp =~ /^[Qq]$/) {
    bailout();
} elsif ($resp) {
    foreach $d (split(" ", $resp)) {
        if ($d =~ /^([^:]+):(.*)$/) {
            $dir = $1; $syml = $2;
        } else {
            $dir = $d; $syml = "NONE";
        }
        $DATA_AREAS{$dir} = $syml;
    }
}
                                        # 0 okay if DADEVS.LIST already
$NET0="$AIPS_ROOT/DA00";
if ( ! -d $NET0 ) {
    mkdir($NET0, 0775);
}
system("chgrp $GROUP $NET0");
if ($GROUPWRITE =~ /YES/) {
    chmod(02775, "$NET0");
} else {
    chmod(0755, "$NET0");
}
                                        # get number of keys (scalar context)
$ndata = keys(%DATA_AREAS);
if (! -f "$NET0/DADEVS.LIST" ) {
    if ($ndata == 0) {
       say "You must have at least one data area so\n" .
           "Adding $sug to list.\n";
       $DATA_AREAS{$sug} = "NONE";
       sleep(7);
    }
}
writerc();
                                        ######## Screen 8.
                                        # Printers and Paper format
 SCREEN_8:
$spoo = "";
$n = 0;
$np = keys(%PRINTERS);
if ($np == 0) {
    $spoo = "  (no printers are defined yet)\n";
} else {
    $spoo .= sprintf("  %-12s  %8s  %-15s  %s\n", "Printer Name",
                           "Type", "AIPS Options", "Description");
    foreach $prntr (keys %PRINTERS) {
        ($ptype, $opts, $descr) = split("~", $PRINTERS{$prntr});
        $spoo .= sprintf("  %-12s  %8s  %-15s  %s\n", $prntr,
                               $ptype, $opts, $descr);
    }
}
$spoo .= "\n  Paper Format: $PRINTMED\n";
$resp = menu("Screen 8: Printers",
             "You may define several printers for use with AIPS.\n" .
             "These will appear on startup as numbered choices to the user\n" .
             "At least one printer must be defined.  Currently defined:\n\n" .
             "$spoo\n\n" .
             "You can modify printer definitions after the install by\n" .
             "editing the file $AIPS_ROOT/DA00/PRDEVS.LIST .\n\n" .
             "Please indicate whether you want to\n\n" .
             "  - add (N)ew printers\n" .
             "  - (D)iscover your system's printers (may not work),\n" .
             "  - (A)ccept the current settings as is, or\n" .
             "  - (R)eset to clear the list.\n\n" .
             "(Paper format - A or A4 - will come next)\n",
             "Your choice (default: Accept):");
if (!$resp) {
    $resp = "A";
}
if ($resp =~ /^[Aa]$/) {
    goto SCREEN_8A;
} elsif ($resp =~ /^[Rr]$/) {
    undef %PRINTERS;
    $PRINTMED = "A";
    writerc();
    goto SCREEN_8;
} elsif ($resp =~ /^[Bb]$/) {
    goto SCREEN_7;
} elsif ($resp =~ /^[Qq]$/) {
    bailout();
} elsif ($resp =~ /^[Dd]$/) {
    if ( -r "/etc/printcap" ) {
        open(PCAP, "</etc/printcap");
        while (<PCAP>) {
            next if (/^\#/);
            if (/^[^\s]/) {
                push(@pcaps, (split(":"))[0]);
            }
        }
    }
    dsay("Found $#pcaps entries in printcap");
    if ( $#pcaps == -1 ) {
        if ( -x "/usr/bin/lpstat") {
            open(PCAP, "/usr/bin/lpstat -a|");
            while (<PCAP>) {
                chomp;
                dsay "Found $_";
                next if (/not accepting/i);
                dsay " and it's not 'not accepting'";
                next unless (/accepting/);
                dsay " and it IS accepting, good";
                push(@pcaps, (split(" "))[0]);
            }
            dsay("Found $#pcaps printers in lpstat -a list");
            if ($#pcaps == -1) {
                aipserror("Found exactly ZERO printers from lpstat?",
                          "The '/usr/bin/lpstat -a' command did not",
                          "help me find any printers.  Sorry!");
                sleep 5;
                goto SCREEN_8;
            }
        } else {
            aipserror("Cannot read /etc/printcap or find lpstat!",
                      "(latter is not in /usr/bin/ at least)",
                      "Sorry, cannot discover printers.");
            sleep 5;
            goto SCREEN_8;
        }
    }
    $npr = 1 + $#pcaps;
    dsay("npr is now $npr");
    if ($npr == 0) {
        aipserror("Did not find ANY printers via printcap or lpstat",
                  "You should enter printer definition(s) by hand.\n");
        sleep 5;
        goto SCREEN_8;
    } elsif ($npr > 15) {
        @pcaps = ( );
        $npr = askuser("Found $npr printers, over the threshold of 15",
                       "(this is a somewhat arbitrary limit, to avoid this",
                       "install procedure being too long!)",
                       "You should enter a few select printers by hand.",
                       "How many printers?");
    }
} elsif ($resp !~ /^[Nn]$/) {
    aipserror "Invalid response (use Q to quit)";
    sleep 5;
    goto SCREEN_8;
} else {
    $npr = askuser("How many printers?");
}
say("Configuring $npr printers.  For each, you will be asked if you",
    "want to configure AIPS to use it, its type, options, and a",
    "description.  AIPS priner types are:\n",
    "  PS       Black-and-white PostScript printer",
    "  COLOR    (or COLOUR) Color/Colour PostScript printer",
    "  TEXT     Plain text printers (including line printers)",
    "  QMS      QUIC format (QMS [not QMS PS!], Talaris) printers",
    "  PREVIEW  Screen Previewers, e.g. gv, ghostview, pageview\n",
    "And the valid AIPS printer options are:\n",
    "  NONE     (default), nothing special.",
    "  DUP      Printer will print on both sides of the paper",
    "  DEF      This will be the default AIPS printer",
    "  BIG=nnn  Force use of this printer for jobs > nnn lines\n",
    "If you want to specify multiple options for a printer,",
    "separate them with commas (NO SPACES!)\n");
for ($i = 0; $i < $npr; $i++) {
# foreach $i (sort @pcaps) {
    if ($#pcaps != -1) {
        $pnam = $pcaps[$i];
    } else {
        $n = $i + 1;
        $pnam = askuser("--Name ('xxx' in 'lpr -Pxxx') for printer $n");
    }
    $type = askuser("  Printer    type for $pnam (N to skip) [PS]");

    if (!$type) {
        $type = "PS";
    }
    $type = uc($type);
    next if ($type =~ /^N$/);
    if ($type =~ /^Q$/) {
        bailout();
    } elsif ($type =~ /^B$/) {
        goto SCREEN_8;
    } elsif (($type =~ /^PS-C/) || ($type =~ /^C/)) {
        $type = "PS-CMYK";
    } elsif ($type =~ /^PS/) {
        $type = "PS";
    } elsif ($type =~ /^T/) {
        $type = "TEXT";
    } elsif ($type =~ /^QM/) {
        $type = "QMS";
    } elsif ($type =~ /^PR/) {
        $type = "PREVIEW";
    } else {
        aipserror("Don't understand type $type", "will use PS.");
        $type = "PS";
    }
    $opts = askuser("  Printer options for $pnam (N to skip) [NONE]");
    if (!$opts) {
        $opts = "NONE";
    }
    $opts = uc($opts);
    next if ($opts =~ /^N$/);
    if ($type =~ /^Q$/) {
        bailout();
    } elsif ($type =~ /^B$/) {
        goto SCREEN_8;
    }
    if ($opts =~ /,/) {
        @optarr = split(",", $opts);
    } else {
        @optarr = ( $opts ) ;
    }
    $optstr = "";
    foreach $opt (@optarr) {
        if ($opt =~ /NO/) {
            $optstr = "NONE";
            last;
        } elsif ($opt =~ /^DU/) {
            $optstr .= ",DUP";
        } elsif ($opt =~ /^DE/) {
            $optstr .= ",DEF";
        } elsif ($opt =~ /^BIG=(\d+)/) {
            $n = $1;
            if (($n < 0 ) || ($n > 1000000)) {
                aipserror "Number out of reasonable range, using 10000";
                $n = 10000;
            }
            $optstr .= ",BIG=$n";
        }
    }
    $optstr =~ s/^,//;
    $descr= askuser("  Description for $pnam (N to skip)");
    if (!$descr) {
        $descr = " (no description supplied by installer) ";
    }
    $descr =~ s/ /_/g;
    $PRINTERS{$pnam} = "$type~$optstr~$descr";
}
 SCREEN_8A:
$resp = askuser("Do your printer(s) use A or A4 paper? [$PRINTMED]");
if ($resp =~ /^[Aa]4/) {
    $PRINTMED = "A4";
} elsif ($resp =~ /^[Aa]$/) {
    $PRINTMED = "A";
}
writerc();
                                        ######## Screen 9.
                                        # Tape Drives
 SCREEN_9:
$spoo = "";
$n = 0;
$nt = keys(%TAPE_DRIVES);
if ($nt == 0) {
    $spoo = "    (no tape drives are defined yet)\n";
} else {
    foreach $t (keys %TAPE_DRIVES) {
                                        # Host is encoded in device name
        ($h, $dv) = split(';', $t);
        $d = $TAPE_DRIVES{$t};
        $d =~ s/_/ /g;
        $spoo .= "host $h, device $dv, description \"$d\"\n";
        $n++;
    }
}
$resp = menu("Screen 9: Tape Drives",
             "If your network has any tape drives you would like " .
             "to use for AIPS,\nyou can specify those now.  You need " .
             "to configure each tape drive\nmachine to run AIPS (or " .
             "a subset of it).  Current definitions:\n\n$spoo\n" .
             "You can add to this list after the install by editing " .
             "TPDEVS.LIST\nin the $AIPS_ROOT/DA00 area.\nPlease " .
             "indicate whether you want to\n" .
             "\n\tadd (N)ew tape drive(s)" .
             "\n\t(D)iscover tape drives on $THISHOST (may not work)" .
             "\n\t(A)ccept the current settings as is, or" .
             "\n\t(R)eset to clear the list.\n",
             "Your choice (default: Accept):");
if (!$resp) {
    $resp = "A";
}
@tdevs = ( );
if ($resp =~ /^[Aa]$/) {
    goto SCREEN_9B;
} elsif ($resp =~ /^[Rr]$/) {
    undef(%TAPE_DRIVES);
    writerc();
    goto SCREEN_9;
} elsif ($resp =~ /^[Bb]$/) {
    goto SCREEN_8;
} elsif ($resp =~ /^[Qq]$/) {
    bailout();
} elsif ($resp =~ /^[Dd]$/) {
                                        # discover tape drives if we can
    dsay ("A-discovering we will go...");
    if ($ARCH =~ /SUN[34]/) {
        foreach $j ("s", "m") {
            foreach $i (0, 1, 2, 3, 4, 5, 6, 7) {
                $dnm = sprintf "/dev/nr%st%s",$j, $i;
                $stat = `mt -f $dnm status`;
                if ($stat !~ /No such/) {
                    push(@tdevs, $dnm);
                }
            }
        }
    } elsif ($ARCH =~ /IBM/) {
        $stat=`lsdev -C -c tape | awk '{printf "/dev/%s ",$1}'`;
        foreach $i (split(" ", $stat)) {
            push(@tdevs, $i);
        }
    } elsif (($ARCH =~ /DEC/) ||
             ($ARCH =~ /ALPHA/)) {
        foreach $i (0, 1, 2, 3, 4, 5, 6, 7) {
            $dnm = sprintf "/dev/nrmt%sl";
            if ( -c $dnm ) {
                push(@tdevs, $dnm);
            }
        }
    } elsif ($ARCH =~ /HP/) {
                                        # includes HP2
        foreach $j ("l", "m") {
            foreach $i (0, 1, 2, 3, 4, 5, 6, 7) {
                $dnm = sprintf "/dev/rmt/%s%sn", $j, $m;
                if ( -c $dnm ) {
                                        # could use /sbin/lssf to get
                                        # more details...
                    push(@tdevs, $dnm);
                }
            }
        }
    } elsif ($ARCH =~ /S[OU]L/) {
        foreach $i (0, 1, 2, 3, 4, 5, 6, 7, 8, 9) {
            $dnm = sprintf "/dev/rmt/%sln", $i;
            if ( -c $dnm ) {
                push(@tdevs, $dnm);
            }
        }
    } elsif (($ARCH =~ /LINUX/) || ($ARCH =~ /LNX64/)) {
        dsay ("Linux: checking /dev/nstX, x=1 thru 9...");
        foreach $i (0, 1, 2, 3, 4, 5, 6, 7, 8, 9) {
            $dnm = sprintf "/dev/nst%s", $i;
            dsay("checking $dnm via mt -f status...");
            if ( -c $dnm ) {
                $stat = `mt -f $dnm status 2>&1`;
                chomp($stat);
                if ($stat) {
                    dsay "Got response $stat";
                    if ($stat !~ /Device not configured/i) {
                        say("Found SCSI tape drive $dnm");
                        push(@tdevs, $dnm);
                    }
                }
            }
        }
    } elsif ($ARCH =~ /SGI/) {
        $osrev = `/sbin/uname -r`;
        chomp($osrev);
        if ($osrev =~ /^5\./) {
                                        # /dev/rmt/0ln, also move ZMOUN2
                                        # and ZTAP2 (.CI5/6)
            $dnm = "";
        } elsif ($osrev =~ /^6\./) {
                                        # Weird.  Sorry...
            $dnm = "";
        }
    } else {
        aipserror("Sorry, this script does not know how to discover\n" .
                  "tape drives on architecture $ARCH.\n");
        sleep 5;
        goto SCREEN_9;
    }
    if ($#tdevs == -1) {
        aipserror("I was unable to find any tape devices (arch $ARCH)",
                  "You should enter tape definition(s) by hand using N",
                  "or skip ahead via A (for accept)");
        sleep 5;
        goto SCREEN_9;
    } else {
        $ntap = 1 + $#tdevs; $drv = "drives";
        $drv = "drive" if ($ntap == 1);
        say("Found $ntap possible tape $drv on this host\n");
    }
} else {
    $ntap = askuser("Please indicate the number of tape drives, if any,",
                    "that are directly connected to $THISHOST",
                    "that you wish to use with AIPS.",
                    "Do NOT include quarter-inch cartridge (QIC) devices.",
                    "Just Enter <RETURN> if you have no tape drives.",
                    "Number of tape drives on $THISHOST");
    if (!$ntap) {
        $ntap = 0;
        goto SCREEN_9B;
    }
}
for ($i = 0; $i < $ntap; $i++) {
    if ($tdevs[$i]) {
        $devn = $tdevs[$i];
    } else {
      SCREEN_9A:
        $devn = askuser(
                "Tape Device name [? for help, <RETURN> to stop]");
        if (!$devn) {
            goto SCREEN_9B;
        } elsif ($devn =~ /^\?$/) {
            if ($ARCH =~ /SUN[34]/) {
                say("SUN3/4: use device names /dev/nrst<x> for SCSI",
                    "devices, and /dev/nrmt<x> for others, where <x>",
                    "is 0,1,2,..,7.  Do NOT use any higher numbers, as",
                    "the AIPS Z routines ASSUME the lower numbers and",
                    "they modify the name to get higher densities.");
            } elsif ($ARCH =~/IBM/) {
                say("AIX: You MUST use names of the form /dev/rmt<x>",
                    "where <x> is 0,1,2,.. Do NOT add any more numbers",
                    "(e.g. /dev/rmt0.1) as the AIPS Z routines ASSUME",
                    "the shorter form and will append the correct",
                    " decimal entity for the relevant density.");
            } elsif ($ARCH =~ /CVEX/) {
                say("(Warning: Convex advice is VERY out of date!)\n",
                    "You MUST use device names of the form /dev/rmtxx",
                    "(literally!) as the AIPS tape Z routines will",
                    "construct the correct tape name given the density",
                    "setting and drive number.",
                    "There is NO discovery capability for Convex.");
            } elsif (($ARCH =~ /DEC/) || ($ARCH =~ /ALPHA/)) {
                say("DEC: names MUST be specified as /dev/nrmt<x>l",
                    "where <x> is the digit 0,1,2,...  Do NOT specify",
                    "different devices for different densities; the",
                    "AIPS Z routines ASSUME this format of name and",
                    "will modify the last character to get the",
                    "requested density.");
            } elsif ($ARCH =~ /HP/) {
                say("HP: names are of the form /dev/rmt/<#><d>n where",
                    "<#> is the device number 0, 1, etc., <d> is the",
                    "density, one of l, m, or h for low, medium, high;",
                    "n (literally) signifies norewind.  DO NOT USE THE",
                    "BERKELEY OPTION (a trailing 'b'); it doesn't work",
                    "for AIPS.");
            } elsif ($ARCH =~ /S[OU]L/) {
                say("Sun: names for SunOS 5.x are /dev/rmt/<#><d><n>",
                    "where <#> is the device number 0,1,.., <d> is the",
                    "density (l, m, h for low, medium, high); and the",
                    "n (literally) signifies norewind.  DO NOT USE THE",
                    "BERKELEY OPTION (a trailing 'b'); it doesn't work",
                    "for AIPS.");
            } elsif (($ARCH =~ /LINUX/) || ($ARCH =~ /LNX64/)) {
                say("Linux: depends on your distributions, but device",
                    "names in recent (2.x) kernels will likely be of",
                    "the form /dev/nst<#> where <#> is the device",
                    "number 0, 1, ...  Do NOT configure QIC drives;",
                    "they cannot backspace and AIPS needs to.");
            } elsif ($ARCH =~ /SGI/) {
                $osrev = `/sbin/uname -r`; chomp($osrev);
                if ($osrev =~ /^5/) {
                    say("Device names on Irix 5.x systems are like",
                        "/dev/rmt/<#>dn where <#> is the device number",
                        "d is density code (l, m, h; changed by AIPS",
                        "at MOUNT time), n is no-rewind-on-close",
                        "(required by AIPS).  DO NOT USE THE BERKELEY",
                        "STYLE DEVICES (trailing 'b')!");
                } elsif ($osrev =~ /^6/) {
                    say("Devices on Irix 6.x systems may be, e.g.",
                        "/dev/rmt/tps1d5nrv.8500, and mediad should be",
                        "configured to IGNORE devices used by AIPS.");
                } else {
                    say("Sorry, your version of Irix is either too old",
                        "or too new for this poor little script.",
                        "What can I say: Read the Source, Luke!");
                }
            } else {
                say("I don't have any helpful advice for arch $ARCH",
                    "Sorry!  Try reading manual pages on mt, st, mtio",
                    "and other related topics, or talk to your systems",
                    "administrator.");
            }
            say("\nAdvice for ALL tape drives.  Make sure:\n",
                " 1) The tape device can backspace over files;",
                " 2) It can use arbitrary block sizes (eg 2880, 28800)",
                " 3) Always use the NOREWIND form of device name! and",
                " 4) If you depart from the advice here, you SHOULD",
                "    read the ZMOUN2.C for your architecture!");
            goto SCREEN_9A;
        }
    }
    if ( ! -c $devn ) {
        aipserror("WARNING: $devn is not a character device",
                  "         This may be a problem (or not)",
                  "         (Consult your systems administrator).",
                  "         You can always edit \$NET0/TPDEVS.LIST",
                  "         after installing to change things.");
        sleep(5);
    }
    $descr = askuser(
             "Description for $THISHOST tape drive $devn (or N to skip)");
    if (!$descr) {
        $descr = "(no description provided by installer yet)";
    } elsif ($descr =~ /^[Nn]$/) {
        next;
    } elsif ($descr =~ /^[Bb]$/) {
        goto SCREEN_9;
    } elsif ($descr =~ /^[Qq]$/) {
        bailout();
    }
    $descr =~ s/ /_/g;
    $tdr = "$THISHOST;$devn";
    $TAPE_DRIVES{$tdr} = "$descr";
    writerc();
}
                                        ######## Screen 9B
                                        # TPHOSTS
 SCREEN_9B:
$spoo = "";
$n = 0;
if ($#TPHOSTS == -1) {
    @TPHOSTS = ( "127.0.0.1" );
}
$spoo .= "-----------------------------------------------------------------\n";
foreach $thing (@TPHOSTS) {
    $spoo .= "    $thing\n";
}
$spoo .= "-----------------------------------------------------------------\n";
$resp = menu("Screen 9B: Tape Hosts",
             "THIS IS IMPORTANT:  READ ME!\n\n" .
             "The TPMON daemons that are automatically started or " .
             "restarted\nwith each AIPS session are servers that " .
             "give remote systems\naccess to your local tape drives " .
             "(TPMON2,3,...) and FITS\ndisk area (TPMON1).  There is " .
             "a mechanism to restrict connections\nto a set of hosts " .
             "and/or IP addresses.  You need to indicate what\n" .
             "remote system(s) if any are allowed to use your tapes\n" .
             "and FITS disk area.  The default configuration is to " .
             "ONLY allow\nyour local host (127.0.0.1).  The list of " .
             "hosts and/or IP addresses\nis currently set to:\n\n" .
             "$spoo\n" .
             "You can modify this list after the install by editing " .
             "file TPHOSTS\nin the $AIPS_ROOT/DA00 area.  Each line " .
             "in the file can be\n\n\t- an IP address, e.g. " .
             "192.33.115.11\n\t- a fully qualified domain name, e.g. " .
             "orangutan.cv.nrao.edu,\n\t- a limited IP address " .
             "wildcard, e.g. 192.33.115.*, or\n\t- a limited domain " .
             "name wildcard, e.g. *.cv.nrao.edu.\n\nPlease " .
             "indicate whether you want to\n\n\tadd (N)ew entries\n\t" .
             "(A)ccept the current settings as is, or \n\t(R)eset to " .
             "the default.\n", "Your choice (default: Accept):");
if (!$resp) {
    $resp = "A";
}
if ($resp =~ /^[Aa]$/) {
    goto SCREEN_10;
} elsif ($resp =~ /^[Rr]$/) {
    @TPHOSTS = ( "127.0.0.1" ) ;
    writerc();
    goto SCREEN_10;
} elsif ($resp =~ /^[Bb]$/) {
    goto SCREEN_9A;
} elsif ($resp =~ /^[Qq]$/) {
    bailout();
} elsif ($resp !~ /^[Nn]$/) {
    aipserror("Not a valid choice; please try again");
    sleep 5;
    goto SCREEN_9B;
}
 SCREEN_9B_LOOP:
$next = askuser("Next entry for TPHOSTS or <RETURN to finish>");
if ($next) {
                                        # is this too restrictive?
    $next =~ s/[^0-9a-zA-Z\.\-\*_]//g;
    push @TPHOSTS, $next;
    writerc();
    goto SCREEN_9B_LOOP;
}
                                        # Inet TV services
                                        # check if they're defined
open(TMP, "ypcat services 2>/dev/null|");
while (<TMP>) {
    if (/^sssin/i) {
        $sssin = "defined in YP/NIS";
    } elsif (/^ssslock/i) {
        $ssslock = "defined in YP/NIS";
    }
}
close(TMP);
open(TMP, "</etc/services");
while (<TMP>) {
    if (/^sssin/i) {
        if ($sssin) {
           $sssin .= " and /etc/services";
        } else {
            $sssin = "defined in /etc/services";
        }
    } elsif (/^ssslock/i) {
        if ($ssslock) {
           $ssslock .= " and /etc/services";
        } else {
            $ssslock = "defined in /etc/services";
        }
    }
}
close(TMP);
if (!$sssin) {
    $sssin = "NOT defined anywhere!";
}
if (!$ssslock) {
    $ssslock = "NOT defined anywhere!";
}
$string = "ok";
if ($sssin =~ /NOT/) {
   $string = "Warning: Unless you always enter aips tv=local,\n";
   $string .= "you need to define the AIPS Services:\n";
   $string .= "sssin (5000), ssslock (5002), msgserv (5008), ";
   $string .= "tekserv (5009)\nIf you want remote tape/disk ";
   $string .= "access, add aipsmt0 (5010, disk),\naipsmt1 (5011, ";
   $string .= "tape drive 1), and so on.\n";
}
if ($ssslock =~ /NOT/) {
   if ($sssin !~ /NOT/) {
      $string = "Warning: You need to define the AIPS TV lock " .
         "service (ssslock, 5002)\n";
    }
}
$servstring=$string;
if ($string ne "ok") {
    say($string);
    say("Services should be defined either in /etc/services or",
        " your YP/NIS services map (all tcp services).",
        " This may require root privileges (and I wont do it!)");
    $dum = askuser("Press <ENTER> to continue:");
}
                                        ######## Screen 10.
                                        # Advanced: Compiler settings,
                                        # AP size, local readline.
 SCREEN_10:
                                        # not on binary
if ($INSTALL_FROM_CD =~ /YES/ || $INSTALL_FROM_NET =~ /YES/ ) {
    goto SCREEN_11;
}
                                        # Discover memory size?
                                        # Linux
#if ( -f "/proc/meminfo" ) {
#   open(FOO, "</proc/meminfo");
#   while (<FOO>) {
#        if (/^MemTotal:\s+([0-9]+)\s.*$/) {
#           $mem = $1;
#           last;
#        }
#   }
#   close(FOO);
#}
                                        # Solaris
#if (!$mem) {
#    if ( -f "/usr/sbin/prtdiag") {
#       open(FOO, "/usr/sbin/prtdiag 2>/dev/null |");
#       while (<FOO>) {
#          if (/memory size: *([0-9]+) *megabytes/i) {
#             $mem = $1 * 1024;
#             last;
#          }
#       }
#       close(FOO);
#   }
#}
#if (!$mem) {
                                        # OSF1/Tru64
#    if ( -f "/var/adm/messages") {
#        open(FOO, "fgrep 'physical memory' /var/adm/messages 2>/dev/null|");
#        while (<FOO>) {
#            if (/physical memory = ([0-9]+) ([a-z]+)/) {
#                $mem = $1;
#                $units = $2;
#                if ($units =~ /megabytes/i) {
#                    $mem = $mem * 1024;
#                } elsif ($units =~ /gigabytes/i) {
#                    $mem = $mem * 1024 * 1024;
#                }
#                last;
#            }
#        }
#        close(FOO);
#    }
#}
#if (!$mem) {
#                                        # HP-UX 10.x
#    if ( -f "/var/adm/syslog/syslog.log") {
#        open(FOO, "grep 'vmunix.*Physical' /var/adm/syslog/syslog.log 2>/dev/null|");
#        while (<FOO>) {
#            if (/Physical: ([0-9]+) Kbytes/) {
#                $mem = $1;
#                last;
#            }
#        }
#        close(FOO);
#    }
#}
                                        # recommended AP size
#if (!$mem) {
#    $mem = "unknown";
#    $rec = "5";
#} elsif ($mem > 400000) {
#    $rec = "80";
#} elsif ($mem > 135000) {
#    $rec = "20";
#}
                                        # Compiler settings
                                        # This is inefficient.
dsay "ARCH $ARCH, AIPS_ROOT $AIPS_ROOT, myvers $myvers, SITE $SITE";
$fort = `sh -c 'ARCH=$ARCH; AIPS_VERSION=$AIPS_ROOT/$myvers; . \$AIPS_VERSION/SYSTEM/UNIX/AREAS.SH; . \$AIPS_VERSION/\$ARCH/SYSTEM/FDEFAULT.SH; echo \$COMPILER'`;
chomp($fort);
$fortopt = `sh -c 'ARCH=$ARCH; AIPS_VERSION=$AIPS_ROOT/$myvers; . \$AIPS_VERSION/SYSTEM/UNIX/AREAS.SH; . \$AIPS_VERSION/\$ARCH/SYSTEM/FDEFAULT.SH; echo \$COMPSWIT'`;
chomp($fortopt);
$ccom = `sh -c 'ARCH=$ARCH; AIPS_VERSION=$AIPS_ROOT/$myvers; . \$AIPS_VERSION/SYSTEM/UNIX/AREAS.SH; . \$AIPS_VERSION/$ARCH/SYSTEM/CCOPTS.SH; echo \$COMPILER'`;
chomp($ccom);
$ccomopt = `sh -c 'ARCH=$ARCH; AIPS_VERSION=$AIPS_ROOT/$myvers; . \$AIPS_VERSION/SYSTEM/UNIX/AREAS.SH; . \$AIPS_VERSION/$ARCH/SYSTEM/CCOPTS.SH; echo \$COMP'`;
chomp($ccomopt);
$link = `sh -c 'ARCH=$ARCH; AIPS_VERSION=$AIPS_ROOT/$myvers; . \$AIPS_VERSION/SYSTEM/UNIX/AREAS.SH; . \$AIPS_VERSION/$ARCH/SYSTEM/LDOPTS.SH; echo \$COMPILER'`;
chomp($link);
if (!$FORT) {
   $FORT = $fort;
}
if (!$FORTOPT) {
   $FORTOPT = $fortopt;
}
if (!$CCOM) {
   $CCOM = $ccom;
}
if (!$CCOMOPT) {
   $CCOMOPT = $ccomopt;
}
if (!$LINK) {
   $LINK = $link;
}
writerc();
                                        # branch here to reset FORTSTR
                                        # etc from user input
 SCREEN_10A:
                                        # AMD-64 repair
if  (($FORTOPT =~ /-malign-double/) && ($IS64 =~ /YES/ )) {
    $FORTOPT = "-c -fno-automatic -fno-globals -Wno-globals -Wimplicit -Wall";
}
                                        # sensible wrapping of lines
$limit = 60;
if (length($FORTOPT) > $limit) {
    $FORTSTR = longline($FORTOPT);
} else {
    $FORTSTR = $FORTOPT;
}
if (length($CCOMOPT) > $limit) {
    $CCOMSTR = longline($CCOMOPT);
} else {
    $CCOMSTR = $CCOMOPT;
}
if (length($fortopt) > $limit) {
    $fortstr = longline($fortopt);
} else {
    $fortstr = $fortopt;
}
if (length($ccomopt) > $limit) {
    $ccomstr = longline($ccomopt);
} else {
    $ccomstr = $ccomopt;
}
                                        # check local readline
if (!$READLINE) {
    foreach $i ("$AIPS_ROOT/$myvers/$ARCH/LIBR/GNU",
                "/usr/lib", "/usr/local/lib", "/opt/local/lib",
                "/usr/local/gnu/lib", "/usr/local/lib/gnu",
                "/usr/gnu/lib", "/sw/lib") {
        if ( -f "$i/libreadline.a" ) {
            $READLINE = "$i/libreadline.a";
            last;
        }
        if ( -f "$i/libreadline.so" ) {
            $READLINE = "$i/libreadline.so";
            last;
        }
        if ( -f "$i/libreadline.dynlib" ) {
            $READLINE = "$i/libreadline.dynlib";
            last;
        }
    }
}
#
$resp = menu("Screen 10: Advanced Settings",
             "In general, the default settings for these advanced " .
             "options will be OK.\nPlease review the settings, and " .
             "then decide if you want to change them.\n\n" .
#             "\tAP settings are in PAPC.INC (cf \$SYSLOCAL/INCS.SH)\n" .
             "\tFortran settings are in \$SYSLOCAL/FDEFAULT.SH\n" .
             "\tC settings are in \$SYSLOCAL/CCOPTS.SH, and \n" .
             "\tLink settings are stored in \$SYSLOCAL/LDOPTS.SH\n\n" .
             "Current settings of advanced parameters:\n\n" .
#             "    AP Size (MB): $APSIZE " .
#             "(for your memory size $mem KB, use $rec MB)\n" .
             "FORTRAN Compiler: $FORT (default $fort)\n" .
             "     and options: $FORTSTR\n" .
             " default options: $fortstr\n" .
             "          Linker: $LINK (default $link)\n" .
             "      C Compiler: $CCOM (default $ccom)\n" .
             "     and options: $CCOMSTR\n" .
             " default options: $ccomstr\n" .
             "Readline Library: $READLINE\n" .
             "  Debug Libs too: $DOTWOLIB\n",
             "[M]odify or [A]ccept (default Accept):");
if ($resp =~ /^[Bb]/) {
    goto SCREEN_9B;
} elsif ($resp =~ /^[Qq]/) {
    bailout();
} elsif ($resp =~ /^[Mm]/) {
    undef($done);
    while (!$done) {
#        $thing = askuser("Change item [APSIZE, FORT, FORTOPT, LINK, " .
        $thing = askuser("Change item [FORT, FORTOPT, LINK, " .
                         "CCOM, CCOMOPT,\n\t\t\tDEBUGLIBS, READLINE, " .
                         "or (default) END]");
        if (!$thing) {
            $done = 1;
        } elsif ($thing =~ /^Q/i) {
            bailout();
        } elsif ($thing =~ /^E/i) {
            $done = 1;
#        } elsif ($thing =~ /^A/i) {
#            $ap = askuser("AP Size in MB (1-512):");
#            if (($ap >= 1) && ($ap <= 512)) {
#                $APSIZE = $ap;
#            } else {
#                aipserror("Invalid AP Size: $ap (not 1-512 MB)");
#            }
        } elsif ($thing =~ /^FORTO/i) {
            $fo = askuser("Fortran compiler options or <RETURN>:");
            if ($fo) {
                $FORTOPT = $fo;
            }
        } elsif ($thing =~ /^F/i) {
            $fo = askuser("Fortran compiler command or <RETURN>:");
            if ($fo) {
                if ( -x "$fo" ) {
                    $FORT = $fo;
                } else {
                    aipserror("Cannot find executable $fo");
                }
            }
        } elsif ($thing =~ /^CCOMO/i) {
            $fo = askuser("C compiler options or <RETURN>:");
            if ($fo) {
                $CCOMOPT = $fo;
            }
        } elsif ($thing =~ /^C/i) {
            $fo = askuser("C compiler command or <RETURN>:");
            if ($fo) {
                if ( -x "$fo" ) {
                    $CCOM = $fo;
                } else {
                    aipserror("Cannot find executable $fo");
                }
            }
        } elsif ($thing =~ /^D/i) {
            $fo = askuser ("Generate debug and opt. libs [Y/N]:");
            if ($fo) {
                if ($fo =~ /^Y/i) {
                    $DOTWOLIB = "YES";
                } else {
                    $DOTWOLIB = "NO";
                }
            }
        } elsif ($thing =~ /^R/i) {
            $fo = askuser("Readline library pathname or <RETURN>:");
            if ($fo) {
                if ( -f "$fo" ) {
                    $READLINE = $fo;
                } elsif ( -f "$fo/libreadline.a" ) {
                    $READLINE = $fo . "/libreadline.a";
                } elsif ( -f "$fo.a" ) {
                    $READLINE = $fo . ".a";
                } else {
                    aipserror("Cannot find library $fo");
                }
            }
        } elsif ($thing =~ /^L/i) {
            $fo = askuser("Linker (usually the Fortran compiler):");
            if ($fo) {
                if ( -x "$fo" ) {
                    $LINK = $fo;
                } else {
                    aipserror("Cannot find executable $fo");
                }
            }
        }
        writerc();
    }
    goto SCREEN_10A;
}
writerc();
 SCREEN_11:
                                        ######## Screen 11.
                                        # 5.  Commit: do stuff after
                                        # reviewing settings briefly.
$s25 = "                        ";
$hostlist = "";
foreach $h (@HOSTS) {
    $hostlist .= " $h";
}
$hostlist =~ s/^ //;
$datalist = "";
foreach $d (keys %DATA_AREAS) {
    $symlink = $DATA_AREAS{$d};
    $ddd = " $d";
    if ($symlink !~ /^NONE$/) {
        $ddd .= " (-> $symlink)";
    }
    $datalist .= "$s25$ddd\n";
}
$datalist =~ s/^\s*//;
$tapelist = "";
foreach $t (keys %TAPE_DRIVES) {
    ($h, $dv) = split(';', $t);
    $descr = $TAPE_DRIVES{$t};
    $descr =~ s/_/ /g;
    $tapelist .= "$s25 host $h, tape drive $dv: $descr\n";
}
$tapelist =~ s/^\s*//;
if ($tapelist eq "") {
    $tapelist .= "\n";
}
$printlist = "";
foreach $p (keys %PRINTERS) {
    ($pt, $opt, $dsc) = split("~", $PRINTERS{$p});
    $dsc =~ s/_/ /g;
    $printlist .= sprintf("%s %6s  %12s  %8s  %s\n", $s25, $p, $pt,
                          $opt, $dsc);
}
$printlist =~ s/^\s*//;
$tphlist = "";
foreach $t (@TPHOSTS) {
    $tphlist .="$s25 $t\n";
}
$tphlist =~ s/^\s*//;
if ($tphlist eq "") {
    $tphlist .= "\n";
}
if ($INSTALL_FROM_CD =~ /YES/ || $INSTALL_FROM_NET =~ /YES/ ) {
$resp = menu("Screen 11: FINAL REVIEW before installing!",
             "This is your last, best hope for checking the settings " .
             "before\ncommitting to the install.  Please check these " .
             "settings, and\nmake sure they are what you want:\n\n" .
                                        # lotsa stuff goes in here
                                        # Make sure to include screen#
                                        # for each param...
             "   AIPS_ROOT (screen 3): $AIPS_ROOT\n" .
             "       Group (screen 4): $GROUP\n" .
             " Group Write (screen 4): $GROUPWRITE\n" .
             "Architecture (screen 5): $ARCH\n" .
             "   Site name (screen 5): $SITE\n" .
             "  AIPS hosts (screen 6): $hostlist\n" .
             "  Data areas (screen 7): $datalist" .
             "    Printers (screen 8): $printlist" .
             "  Paper type (screen 8): $PRINTMED\n" .
             " Tape drives (screen 9): $tapelist" .
             "  Tape hosts (screen 9): $tphlist" .
             "\nYou can skip back to a previous menu by entering its " .
             "number,\nor use the phrase 'start over' to go back " .
             " to the beginning.", "[A]ccept (default) or menu number:");
} else {
$resp = menu("Screen 11: FINAL REVIEW before installing!",
             "This is your last, best hope for checking the settings " .
             "before\ncommitting to the install.  Please check these " .
             "settings, and\nmake sure they are what you want:\n\n" .
                                        # lotsa stuff goes in here
                                        # Make sure to include screen#
                                        # for each param...
             "   AIPS_ROOT (screen 3): $AIPS_ROOT\n" .
             "       Group (screen 4): $GROUP\n" .
             " Group Write (screen 4): $GROUPWRITE\n" .
             "Architecture (screen 5): $ARCH\n" .
             "   Site name (screen 5): $SITE\n" .
             "  AIPS hosts (screen 6): $hostlist\n" .
             "  Data areas (screen 7): $datalist" .
             "    Printers (screen 8): $printlist" .
             "  Paper type (screen 8): $PRINTMED\n" .
             " Tape drives (screen 9): $tapelist" .
             "  Tape hosts (screen 9): $tphlist" .
             "   Advanced (screen 10): (not listed here)\n" .
             "\nYou can skip back to a previous menu by entering its " .
             "number,\nor use the phrase 'start over' to go back " .
             " to the beginning.", "[A]ccept (default) or menu number:");
}
$resp =~ s/\'//g;
if ($resp =~ /^[Bb]$/) {
    goto SCREEN_10;
} elsif ($resp =~ /^[Qq]$/) {
    bailout();
} elsif ($resp =~ /start\s+over/i) {
    goto STARTOVER;
} elsif ($resp =~ /[0-9][0-9]*/) {
                                        # back to specific screen
                                        # this is cool
    if (($resp > 0) && ($resp < 11)) {
        $str = "goto SCREEN_$resp";
        eval $str;
    }
}
$resp = askuser("Confirm: start the install with these settings [Y]");
if ($resp =~ /^[BbNn]$/) {
    goto SCREEN_11;
} elsif ($resp =~ /^[Qq]$/) {
    bailout();
}
                                        ################################
                                        # We're committed at this point.
                                        # This is basically INSTEP1
                                        # without the interaction.
                                        ################################
########################################################################
# checklist of other things that need done: (get order right!)
#  1   Create HOSTS.LIST from HOSTS. [DONE]
#  2.  Copy files to AIPS_ROOT: AIPS{ASSN,PATH}.{C,}SH, HOSTS.{C,}SH,
#      LOGIN.{C,}SH, START_{AIPS,TPSERVERS,TVSERVERS}, TVALT, [DONE]
#      AIPS.BOOT?  Yes, needed by AIPSROOT.DEFINE.  Grr!
#  3.  symlink TVDEVS.SH -> $SYSUNIX [DONE] (kludge for 15*9[234]?) No.
#  4.  Symlink $AIPS_ROOT/PRDEVS.SH -> $SYSUNIX/PRDEVS.SH [DONE]
#  5.  Run AIPSROOT.DEFINE (it can be left in $SYSUNIX) [DONE]
#  6.  Adjust AP Size [DONE].
#  7.  mkdir $myvers/$ARCH if necessary, then LIBR, LOAD, MEMORY, SYSTEM
#      (if not already there), TEMPLATE; also $AIPS_ROOT/RUN. ERRORS? no
#      [DONE]
#  8.  chmod/chgrp these according to GROUPWRITE.  LOAD too.  [DONE]
#  9.  mkdir $SYSLOCAL, copy subset of files from SYSUNIX if not there:
#      CDVER.CSH CDVER.SH CCOPTS.SH LDOPTS.SH ASOPTS.SH INCS.SH LIBR.DAT
#      [DONE]
# 10.  Create PRDEVS.LIST.  Hook for translating old version?  No.[DONE]
# 11.  Hack ZLPCL2 in $SYSUNIX for A or A4 paper [DONE].
# 12.  Symlink AIPS and aips in $SYSLOCAL; [DONE]
#      also LIBR/GNU/libreadline.a if requested. [DONE]
# 13.  Do anything about OLD/NEW/TST?  Need another screen?  No.
# 14.  DADEVS.LIST and NETSP (set DATA_ROOT in AIPSASSN.{C,}SH; default)
#      [DONE]
# 15.  TPDEVS.LIST and TPHOSTS [DONE]
# 16.  mkdir DA00/$HOST (and one for each @HOSTS). [DONE]
# 17.  Set compiler in CCOPTS.SH, LDOPTS.SH (and options), FDEFAULT.SH
#        [DONE, but need to rationalize stuff in latter first....]
# 18.  Make NEWEST, AIPWD, F2PS, F2TEXT, REVENV, PRINTENV, PP.EXE [DONE]
# 19.  Make XAS, and READLINE if needed.
# 20.  XHELP/CGI?  No.
# 21.  mkdir $ARCH/INSTALL; symlink INSTEP2-4.
# 22.  Set up midnight job (need to ask?)
# 23.  Register info (fill out form?  Web?)
# 24.  Warn about /usr/ccs/bin for $arch =~ /S[OU]L/ (at the end)
# 25.  General instructions: INSTEP2/4, POPSGN, etc.
#
# Stuff not ported from INSTEP1 or that is problematic
#  a) plopping a blank user#1 message file MSD00100.001\; in $DA01;
#  b) doing something sensible if DADEVS.LIST.$SITE is found (rare)
#  c) offer to run SYSETUP ?
########################################################################
if ($GROUPWRITE =~ /YES/) {
    umask(0002);
} else {
    umask(0022);
}

                                        # ----- Create HOSTS.LIST -----
say " =====> Creating or updating HOSTS.LIST file";
$hlist = "$AIPS_ROOT/HOSTS.LIST";
@nfound = ( );
if ( -s $hlist ) {
    system("chgrp $GROUP $hlist");
    if ($GROUPWRITE =~ /YES/) {
        chmod(0775, $hlist);
    } else {
        chmod(0755, $hlist);
    }
    say "       -- Already exists, checking list of hosts...";
    $oldl = `sh -c 'ARCH=$ARCH . $AIPS_ROOT/HOSTS.SH && echo \$HOSTS'`;
    @oldl = split(" ", $oldl);
    @missing = ( );
    foreach $i (@HOSTS) {
        $found = 0;
        foreach $j (@oldl) {
            if ($j eq $i) {
                $found = 1; push(@nfound, $i); last;
            }
        }
        if ($found == 0) {
            push(@missing, $i);
        }
    }
    if ($#missing == -1) {
        say "       -- no additional hosts need added.";
    } else {
        $nn = $#missing  + 1;
        say "       -- adding $nn host(s)...";
        open (HLIST, ">>$hlist");
    }
} else {
    @missing = @HOSTS;
    $orig = "$AIPS_ROOT/$myvers/SYSTEM/UNIX/HOSTS.LIST";
    open(ORIG, "<$orig");
    open(HLIST, ">$hlist");
    while (<ORIG>) {
        print HLIST;
    }
    close(ORIG);
}
if ($#missing > -1) {
    foreach $h (@missing) {
        printf HLIST "+  %-10s %7s %8s %11s Put description here\n",
                     $h, $ARCH, $SITE, "NONE";
    }
    close(HLIST);
    say "        -- Done.";
}
                                        # ----- make sure HOSTS.LIST okay
if ($#nfound > -1) {
   say "        -- checking HOSTS.LIST for match to ARCH, SITE";
   open(SRC, "<$AIPS_ROOT/HOSTS.LIST");
   open(DST, ">$AIPS_ROOT/HOSTS.LIST.new");
   while (<SRC>) {
      $found = 0;
      foreach $i (@nfound) {
          if (/^\+\ \ $i/ ) {
             $found = 1;
             ($hl1, $hl2, $hl3, $hl4, $hl5, $hl6) = split(' ',$_,6);
             chop $hl6;
             printf DST "+  %-10s %-7s %-8s %-10s  %-s\n",
                 $i, $ARCH, $SITE, "NONE", $hl6;
          }
      }
      if ($found == 0) {
         print DST;
      }
   }
   close(SRC);
   close(DST);
#   unlink("$AIPS_ROOT/HOSTS.LIST");
   rename("$AIPS_ROOT/HOSTS.LIST", "$AIPS_ROOT/HOSTS.LIST.old");
   rename("$AIPS_ROOT/HOSTS.LIST.new", "$AIPS_ROOT/HOSTS.LIST");
   system("chgrp $GROUP $AIPS_ROOT/HOSTS.LIST");
    say "        -- Done.";
}

                                        # ----- AIPS_ROOT files -----
chdir($AIPS_ROOT);
if( ! -d $myvers ){
   say "$AIPS_ROOT/$myvers does not exist, will be created.";
   mkdir($myvers, 0755);
}
                                        # binary install - get system area
$rsync = "rsync -auvz --timeout=120";
if ($INSTALL_FROM_NET =~ /YES/) {
   system("cd $myvers/; $rsync $AIPS_NETSERVER\:\:$myvers/SYSTEM .");
}
#
say " =====> Configuring your AIPS_ROOT area...";
foreach $i ("AIPSASSN.SH", "AIPSPATH.SH", "AIPSASSN.CSH",
            "AIPSPATH.CSH", "HOSTS.SH", "HOSTS.CSH", "LOGIN.SH",
            "LOGIN.CSH", "START_AIPS", "START_TPSERVERS", "AIPS.BOOT",
            "START_TVSERVERS", "TVALT", "SETNAME", "SETUNAME",
            "START_QMNGR") {
    $j = "$myvers/SYSTEM/UNIX/$i";
    if ( ! -f $j ) {
        aipserror("Cannot find $j", "AIPS SYSUNIX area is incomplete",
                  "(You may want to unpack the tarball again)",
                  "Cannot proceed further!");
        bailout();
    }
    if ( -f $i ) {
                                      # copy no matter what time
        say "        Moving old $i to $i.OLD";
        if ( -f "$i.OLD" ) {
            unlink("$i.OLD");
        }
        rename("$i", "$i.OLD");
    }
    system("cp $myvers/SYSTEM/UNIX/$i .; chgrp $GROUP $i; chmod +x $i");
    if ( ! -f $i ) {
        aipserror("Failed to copy $i to $AIPS_ROOT",
                  "Check permissions on directory, and your uid/gid:",
                  "`ls -ld $AIPS_ROOT; id`",
                  "Cannot proceed further!");
        bailout();
    }
}
                                        # TVDEVS.SH, PRDEVS.SH symlinks
if ( -f "TVDEVS.SH" ) {
    unlink("TVDEVS.SH");
}
symlink("$myvers/SYSTEM/UNIX/TVDEVS.SH", "TVDEVS.SH");
if ( -f "PRDEVS.SH" ) {
    unlink("PRDEVS.SH");
}
symlink("$myvers/SYSTEM/UNIX/PRDEVS.SH", "PRDEVS.SH");
                                        # AIPSROOT.DEFINE
system("chgrp $GROUP $myvers/SYSTEM/UNIX/AIPSROOT.DEFINE");
if ($GROUPWRITE =~ /YES/) {
    chmod(0775, "$myvers/SYSTEM/UNIX/AIPSROOT.DEFINE");
} else {
    chmod(0755, "$myvers/SYSTEM/UNIX/AIPSROOT.DEFINE");
}
say " =====> Running $myvers/SYSTEM/UNIX/AIPSROOT.DEFINE $AIPS_ROOT";
$result = system("$myvers/SYSTEM/UNIX/AIPSROOT.DEFINE $AIPS_ROOT")
    / 256;
if ($result != 0) {
    aipserror("AIPSROOT.DEFINE is not happy.  Something went wrong.",
              "Its function is to set the value of AIPS_ROOT in several",
              "shell scripts in the $AIPS_ROOT/ directory.",
              "Maybe something was write-protected or not owned by you?",
              "Please investigate, fix it if possible and re-try.");
    bailout();
}
                                        # ----- LOCALHOST = LAPTOP -----
if ($THISHOST =~ /LOCALHOST/) {
    say " =====> changing LOGIN.SH to LAPTOP=YES";
    $logc = "$AIPS_ROOT/LOGIN.SH";
    $logn = "$AIPS_ROOT/LOGIN.SH.NEW";
    if ( -f "$logn") {
       unlink($logn);
    }
    open(OLD, "<$logc");
    open(NEW, ">$logn");
    while (<OLD>) {
       if (/\#Changed.*install.pl/) {
          next;
          }
       if (/^LAPTOP="NO"$/) {
          s/$1/LAPTOP="YES"/;
          printf NEW ("\#Changed [install.pl] to yes\n");
       }
       print NEW;
    }
    close(OLD);
    close(NEW);
    unlink($logc);
    rename($logn, $logc);
    say " =====> changing LOGIN.CSH to LAPTOP=YES";
    $logc = "$AIPS_ROOT/LOGIN.CSH";
    $logn = "$AIPS_ROOT/LOGIN.CSH.NEW";
    if ( -f "$logn") {
       unlink($logn);
    }
    open(OLD, "<$logc");
    open(NEW, ">$logn");
    while (<OLD>) {
       if (/\#Changed.*install.pl/) {
          next;
          }
       if (/^setenv LAPTOP "NO"$/) {
          s/$1/setenv LAPTOP "YES"/;
          printf NEW ("\#Changed [install.pl] to yes\n");
       }
       print NEW;
    }
    close(OLD);
    close(NEW);
    unlink($logc);
    rename($logn, $logc);
}
                                        # ----- group write always -----
                                        # ----- RUN area -----
if (! -d "RUN") {
    mkdir("RUN", 0775);
}
system("chgrp $GROUP RUN");
chmod(02775, "RUN");
                                        # ----- PRTFIL area -----
if (! -d "PRINT") {
    mkdir("PRINT", 0775);
}
system("chgrp $GROUP PRINT");
chmod(02775, "PRINT");
                                        # ----- FITS area -----
if (! -d "FITS") {
    mkdir("FITS", 0775);
}
system("chgrp $GROUP FITS");
chmod(02775, "FITS");
                                        # ----- OFM area -----
if (! -d "OFM") {
    mkdir("OFM", 0775);
}
system("chgrp $GROUP OFM");
chmod(02775, "OFM");
say "        -- Done.";
#if (!($INSTALL_FROM_CD =~ /YES/ || $INSTALL_FROM_NET =~ /YES/ )) {
#                                        # ----- Fix AP Size -----
#   say " =====> Adjusting AP size to $APSIZE MBytes";
#   $papc = "$AIPS_ROOT/$myvers/INC/NOTST/$ARCH/PAPC.INC";
#   $papcnew = "$AIPS_ROOT/$myvers/INC/NOTST/$ARCH/PAPC.INC.NEW";
#   if (! -f $papc) {
#       say "        No $papc found";
#       $papc = "$AIPS_ROOT/$myvers/INC/PAPC.INC";
#       say "        - trying $papc";
#       if (! -f $papc) {
#           aipserror("Cannot find any PAPC.INC file",
#                  "This likely means some files are missing.",
#                  "It would not be good to proceed further until",
#                  "you can determine why $myvers/INC/PAPC.INC",
#                  "(and maybe other files?) is/are missing");
#           bailout();
#       }
#   }
#   $aps = $APSIZE * 262144;
#   if ( -f "$papcnew") {
#       unlink($papcnew);
#   }
#   open(OLD, "<$papc");
#   open(NEW, ">$papcnew");
#   while (<OLD>) {
#       if (/Changed.*install.pl/) {
#           next;
#       }
#       if (/^\s+PARAMETER \(APSIZE=([0-9]+)\)/) {
#           s/$1/$aps/;
#           printf NEW ("C                                       %s\n",
#                    "Changed [install.pl]: $APSIZE MBytes");
#       }
#       print NEW;
#   }
#   close(OLD);
#   close(NEW);
#   $papc = "$AIPS_ROOT/$myvers/INC/NOTST/$ARCH/PAPC.INC";
#   if ( -f "$papc") {
#       unlink($papc);
#   }
#   rename($papcnew, $papc);
#   say "        -- Done.";
#}
                                        # ----- $ARCH/* dirs -----
say " =====> Setting up $ARCH-specific areas and SYSLOCAL...";
                                        # Create the $ARCH area in case it
                                        # hasn't been created yet.
if (! -d "$AIPS_ROOT/$myvers/$ARCH" ){
        mkdir("$AIPS_ROOT/$myvers/$ARCH", 0775);
}
foreach $d ("LIBR", "LOAD", "MEMORY", "PREP", "SYSTEM",
            "SYSTEM/$SITE", "TEMPLATE") {
    $dd = "$AIPS_ROOT/$myvers/$ARCH/$d";
    if ( ! -d $dd ) {
        say "Creating directory $dd";
        mkdir($dd, 0775);
        if (! -d $dd ) {
            say "Failed to create $dd";
        }
    }
    if (($INSTALL_FROM_CD =~ /YES/) &&(($d =~ /LIBR/) || ($d =~ /LOAD/))) {
        dsay "Skip chmod on possible symlink $d to CDROM";
    } else {
        system("chgrp $GROUP $dd");
        if ($GROUPWRITE =~ /YES/) {
            chmod(02775, $dd);
        } else {
            chmod(0755, $dd);
        }
    }
}
$SYSLOCAL = "$AIPS_ROOT/$myvers/$ARCH/SYSTEM/$SITE";
$SYSUNIX  = "$AIPS_ROOT/$myvers/SYSTEM/UNIX";
                                        # copy stuff to syslocal
if ($INSTALL_FROM_NET !~ /YES/) {
   opendir(SA, "$AIPS_ROOT/$myvers/$ARCH/SYSTEM");
   dsay "Opened $myvers/$ARCH/SYSTEM with opendir";
   while (defined($f = readdir(SA))) {
      $o = "$AIPS_ROOT/$myvers/$ARCH/SYSTEM/$f";
                                        # skip directories, etc.
      next if ( -d $o );
      next if ($f =~ /^AIPS$/i);
                                        # We should nuke ld.uu dangit
      next if ($f =~ /^LD.UU$/);
      $n = "$SYSLOCAL/$f";
      if ( -f $n ) {
         $ortime = filemtime($o);
         $nrtime = filemtime($n);
         if ($nrtime > $ortime) {
            say "        Skipping $f (already copied to SYSLOCAL)";
            next;
         } else {
            if ( -f "$n.OLD" ) {
                unlink("$n.OLD");
            }
            rename("$n", "$n.OLD");
         }
      }
      system("cp $o $n");
      if ( ! -f $n ) {
         aipserror("Failed to copy $f to SYSLOCAL; cannot proceed!",
                   "The SYSLOCAL area is $SYSLOCAL",
                   "and the command used was: cp $o $n",
                   "Perhaps the area is write protected or the file",
                   "($n) exists and cannot be overwritten?");
         bailout();
      }
   }
   closedir(SA);

                                        # SYSUNIX stuff, just in case
   foreach $f ("CDVER.CSH", "CDVER.SH", "CCOPTS.SH", "LDOPTS.SH",
               "INCS.SH", "LIBR.DAT") {
      $s = "$SYSLOCAL/$f";
      $u = "$SYSUNIX/$f";
      if ( ! -f $s ) {
         say "        Odd: $s not found!";
         say "        - will copy from $u";
         say "        - but it may need hand editing!";
         system("cp $u $s");
      }
   }
                                        # UPDUNIX stuff -> SYSLOCAL
  foreach $f ("MAKE.MNJ") {
      $s = "$SYSLOCAL/$f";
      $u = "$SYSUNIX/UPDATE/$f";
      if ( ! -f $s ) {
         say "        Will copy $s";
         say "        - from $u";
         system("cp $u $s");
      }
   }
   if ( -e "$SYSLOCAL/AIPS") {
      unlink "$SYSLOCAL/AIPS";
   }
   symlink("$AIPS_ROOT/START_AIPS", "$SYSLOCAL/AIPS");
   if ( -e "$SYSLOCAL/aips") {
      unlink "$SYSLOCAL/aips";
   }
   symlink("$AIPS_ROOT/START_AIPS", "$SYSLOCAL/aips");
   if ($READLINE) {
      $rld = "$AIPS_ROOT/$myvers/$ARCH/LIBR/GNU";
      if (! -d $rld ) {
         system("mkdir -p $rld");
         if (! -d $rld) {
            aipserror("Cannot create directory $rld; help!");
            bailout();
         }
      }
      $rla = "$AIPS_ROOT/$myvers/$ARCH/LIBR/GNU/libreadline.a";
      if ( ! -e $rla ) {
         symlink ($READLINE, $rla);
         dsay "Symlinked $rla to $READLINE";
      }
   }
   say "        -- Done.";
}
                                        # ----- PRDEVS.LIST -----
say " =====> Configuring AIPS for your printers...";
$NET0="$AIPS_ROOT/DA00";
if ( ! -d $NET0 ) {
    mkdir($NET0, 0775);
}
system("chgrp $GROUP $NET0");
if ($GROUPWRITE =~ /YES/) {
    chmod(02775, "$NET0");
} else {
    chmod(0755, "$NET0");
}
if ( -f "$NET0/PRDEVS.LIST" ) {
                                        # get existing definitions
    @prdone = ( );
    open(PR, "<$NET0/PRDEVS.LIST");
    while (<PR>) {
        chomp;
        s/\#.*$//;
        next if /^\s*$/;
        $pr = (split(" "))[0];
        $found = 0;
      PRLOOP:
        foreach $p (keys %PRINTERS) {
            if ($p =~ /^$pr$/) {
                $found = 1; last PRLOOP;
            }
        }
        if ($found) {
            push(@prdone, $pr);
        }
    }
    close(PR);
                                        # and APPEND new definitions!
    open(PR, ">>$NET0/PRDEVS.LIST");
} else {
    open(OLD, "<$SYSUNIX/PRDEVS.LIST");
    open(PR, ">$NET0/PRDEVS.LIST");
    while (<OLD>) {
                                        # only copy leading comments
        last unless (/^\#/);
        print PR;
    }
    close(OLD);
}
 PWLOOP:
foreach $p (keys %PRINTERS) {
    foreach $j (@prdone) {
        if ($j =~ /^$p$/) {
            say "        skipping printer $p (already defined)";
            next PWLOOP;
        }
    }
    ($pt, $opts, $dsc) = split("~", $PRINTERS{$p});
    $dsc =~ s/_/ /g;
    printf PR "%-12s %-8s %-16s %s\n", $p, $pt, $opts, $dsc;
}
                                        # ZLPCL2 for paper size
unlink("$SYSUNIX/ZLPCL2.TMP");
open(OLD, "<$SYSUNIX/ZLPCL2");
open(NEW, ">$SYSUNIX/ZLPCL2.TMP");
while (<OLD>) {
    if (/^psfilter/) {
        if ($PRINTMED =~ /A4/) {
            printf NEW "psfilter=\"\$SYSLOCAL/F2PS -A4 -\$size\"\n";
        } else {
            printf NEW "psfilter=\"\$SYSLOCAL/F2PS -\$size\"\n";
        }
    } else {
        print NEW;
    }
}
close(OLD);
close(NEW);
close(PR);
unlink(OLD);
rename("$SYSUNIX/ZLPCL2.TMP", "$SYSUNIX/ZLPCL2");
system("chgrp $GROUP $SYSUNIX/ZLPCL2");
if ($GROUPWRITE =~ /YES/) {
    chmod(0775, "$SYSUNIX/ZLPCL2");
} else {
    chmod(0755, "$SYSUNIX/ZLPCL2");
}
say "        Text printing set up for '$PRINTMED' size paper";
say "        -- done";
                                        # ----- Disk config files -----
say " =====> Configure AIPS data areas ...";

if ( -f "$NET0/DADEVS.LIST" ) {
    @dadone = ( );
    open(DA, "<$NET0/DADEVS.LIST");
    while(<DA>) {
        chomp;
        next unless (/^[\+-]/);
        $d = (split(" "))[1];
        push(@dadone, $d);
    }
    close(DA);
    open(DA, ">>$NET0/DADEVS.LIST");
} else {
    open(OLD, "<$SYSUNIX/DADEVS.LIST");
    open(DA, ">$NET0/DADEVS.LIST");
    while(<OLD>) {
        last unless (/^\#/);
        print DA;
    }
    close(OLD);
}
if ( -f "$NET0/NETSP" ) {
    $nsdone = ( );
    open(NS, "<$NET0/NETSP");
    while(<NS>) {
        chomp;
        next unless (/^[\+-]/);
        push(@nsdone, (split(" "))[0]);
    }
    close(NS);
    open(NS, ">>$NET0/NETSP");
} else {
    open(OLD, "<$SYSUNIX/NETSP");
    open(NS, ">$NET0/NETSP");
    while(<OLD>) {
        next if (/These are samples/);
        last unless (/^\#/);
        print NS;
    }
    close(OLD);
}
  DALOOP:
foreach $d (keys %DATA_AREAS) {
    foreach $j (@dadone) {
        if ($j =~ /^$d$/) {
            say "        skipping $d (already defined in DADEVS.LIST)";
            next DALOOP;
        }
    }
    printf DA "-  $d\n";
}
close(DA);
foreach $d (keys %DATA_AREAS) {
    foreach $j (@nsdone) {
        if ($j =~ /^$d$/) {
            say "        skipping $d (already defined in NETSP)";
            next DALOOP;
        }
    }
    printf NS "%-25s 365.0    0    0    0    0    0    0    0    0\n",
              $d;
}
close(NS);
                                        # Create data areas if needed
foreach $d (keys %DATA_AREAS) {
    $sym = $DATA_AREAS{$d};
    if ( ! -d $d ) {
#       mkdir($d, 0775);
        system("mkdir -p $d");
        if ( ! -d $d ) {
            aipserror("FAILED to create directory $d",
                      "You will need to create it yourself later, and",
                      "create an empty SPACE file therein, both with",
                      "the right protections.");
            $dum = askuser("Press <ENTER> to acknowledge:");
            next;
        }
    }
    if ($sym !~ /^NONE$/) {
        symlink($d, $symlink);
    }
    if ( ! -f "$d/SPACE" ) {
        open(TMP, ">$d/SPACE");
        close(TMP);
        if ( ! -f "$d/SPACE") {
            aipserror("FAILED to create space lock file $d/SPACE",
                      "You will need to create it yourself later with",
                      "the right protections.");
            $dum = askuser("Press <ENTER> to acknowledge:");
            next;
        }
    }
    system("chgrp $GROUP $d");
    system("chgrp $GROUP $d/SPACE");
    chmod(0775, "$d/SPACE");
    if ($GROUPWRITE =~ /YES/) {
        chmod(02775, $d);
    } else {
        chmod(0755, $d);
    }
}
say "        -- done";
                                        # ----- Tape config files -----
say " =====> Configure AIPS Tape drives...";

if ( -f "$NET0/TPDEVS.LIST" ) {
    @tadone = ( ) ;
    open(TP, "<$NET0/TPDEVS.LIST");
    while(<TP>) {
        chomp;
        s/\#.*$//;
        next if /^\s*$/;
        $tp = (split(" "))[0];
        $found = 0;
      TPLOOP:
        foreach $t (keys %TAPE_DRIVES) {
            if ($t =~ /^$tp$/) {
                $found = 1; last TPLOOP;
            }
        }
        if ($found) {
            push(@tadone, $tp);
        }
    }
    close(TP);
    open(TP, ">>$NET0/TPDEVS.LIST");
} else {
    open(OLD, "<$SYSUNIX/TPDEVS.LIST");
    open(TP, ">$NET0/TPDEVS.LIST");
    while (<OLD>) {
                                        # only copy comments
        last if (/NRAO-CV hosts/);
        print PR;
    }
    print TP "# Host     Device-name    Comment\n\n";
    close(OLD);
}
 TWLOOP:
foreach $t (keys %TAPE_DRIVES) {
    foreach $j (@tadone) {
        if ($j =~ /^$t$/) {
            say "        skipping tape drive $t (already defined)";
            next TWLOOP;
        }
    }
    ($h, $dv) = split(";", $t);
    $descr = $TAPE_DRIVES{$t};
    $descr =~ s/_/ /g;
    printf TP "%-10s %-14s %s\n", $h, $dv, $descr;
}
close(TP);
                                        # Also do the TPHOSTS file
if ( -f "$NET0/TPHOSTS" ) {
    @tpdone = ( ) ;
    open(TP, "<$NET0/TPHOSTS");
    while(<TP>) {
        chomp;
        s/\#.*$//;
        next if /^\s*$/;
        $tp = (split(" "))[0];
        $found = 0;
      THLOOP:
        foreach $t (@TPHOSTS) {
            if ($t =~ /^$tp$/) {
                $found = 1; last THLOOP;
            }
        }
        if ($found) {
            push(@tpdone, $tp);
        }
    }
    close(TP);
    open(TP, ">>$NET0/TPHOSTS");
} else {
    open(OLD, "<$SYSUNIX/TPHOSTS");
    open(TP, ">$NET0/TPHOSTS");
    while (<OLD>) {
        print TP;
        last if (/^localhost/);
    }
    close(OLD);
}
 THLOOP2:
foreach $t (@TPHOSTS) {
    printf TP "$t\n";
}
close(TP);
say "        -- done";
                                        # ----- create DA00/$HOST -----
say " =====> Create DA00/hostname/ directories...";
foreach $h (@HOSTS) {
    $da00 = "$NET0/$h";
    if ( ! -d $da00) {
        mkdir($da00, 0775);
    }
    system("chgrp $GROUP $da00");
    if ($GROUPWRITE =~ /YES/) {
        chmod(02775, $da00);
    } else {
        chmod(0755, $da00);
    }
}
if ($INSTALL_FROM_NET =~ /YES/) {
    say "        -- and populating area for $THISHOST from rsync server";

     if ($AIPS_NETSERVER =~ /ftp.aoc.nrao.edu/){
                                   # Get the goods (have SYSTEM already)
#       system("cd $AIPS_ROOT/$myvers/; $rsync $AIPS_NETSERVER\:\:$myvers/SYSTEM .");
        system("cd $AIPS_ROOT/$myvers/; $rsync $AIPS_NETSERVER\:\:$myvers/AIPS .");
        system("cd $AIPS_ROOT/$myvers/; $rsync $AIPS_NETSERVER\:\:$myvers/APL .");
        system("cd $AIPS_ROOT/$myvers/; $rsync $AIPS_NETSERVER\:\:$myvers/CVS .");
        system("cd $AIPS_ROOT/$myvers/; $rsync $AIPS_NETSERVER\:\:$myvers/DOC .");
        system("cd $AIPS_ROOT/$myvers/; $rsync $AIPS_NETSERVER\:\:$myvers/HELP .");
        system("cd $AIPS_ROOT/$myvers/; $rsync $AIPS_NETSERVER\:\:$myvers/HIST .");
        system("cd $AIPS_ROOT/$myvers/; $rsync $AIPS_NETSERVER\:\:$myvers/INC .");
        system("cd $AIPS_ROOT/$myvers/; $rsync $AIPS_NETSERVER\:\:$myvers/Q .");
        system("cd $AIPS_ROOT/$myvers/; $rsync $AIPS_NETSERVER\:\:$myvers/QY .");
        system("cd $AIPS_ROOT/$myvers/; $rsync $AIPS_NETSERVER\:\:$myvers/RUN .");
        system("cd $AIPS_ROOT/$myvers/; $rsync $AIPS_NETSERVER\:\:$myvers/Y .");
        system("cd $AIPS_ROOT/$myvers/; $rsync $AIPS_NETSERVER\:\:$myvers/$ARCH .");
                                                       #  move the TEXT area
        system("cd $AIPS_ROOT/; $rsync $AIPS_NETSERVER\:\:TEXT TEXT");
                                                       # ather arch SYSTEM areas
        $Alist = "ALPHA AXLINUX DEC HP HP2 IBM LINUX LINUXPPC LNX64 MACINT MACPPC SGI SOL SOL86 SUL SUN4";
        @Alist = split(" ", $Alist);
        foreach $h (@Alist) {
            $adir = "$AIPS_ROOT/$myvers/$h";
            if ( ! -d $adir) {
                mkdir($adir, 0775);
                system("chgrp $GROUP $adir");
                if ($GROUPWRITE =~ /YES/) {
                   chmod(02775, $adir);
                } else {
                   chmod(0755, $adir);
                }
                $sdir = "$adir/SYSTEM";
                mkdir($sdir, 0775);
                system("chgrp $GROUP $sdir");
                if ($GROUPWRITE =~ /YES/) {
                   chmod(02775, $sdir);
                } else {
                   chmod(0755, $sdir);
                }
                system("cd $AIPS_ROOT/$myvers/$h; $rsync $AIPS_NETSERVER\:\:$myvers/$h/SYSTEM .");
                system("cd $AIPS_ROOT/$myvers/$h; $rsync $AIPS_NETSERVER\:\:$myvers/$h/CVS .");
            }
        }

                                   # OK mark this one to be updated via rsync
        system("cat /dev/null > $SYSLOCAL/RSYNC.ME");
                       # Here is where me move stuff from MASTER to $SYSLOCAL
        system("cd $AIPS_ROOT/$myvers; cp $ARCH/SYSTEM/MASTER/* $SYSLOCAL");
        if (!($ARCH =~ /MACPPC/ || $ARCH =~ /MACINT/)) {
           system("cd $SYSLOCAL; rm AIPS; ln -s $AIPS_ROOT/START_AIPS AIPS");
           }
        system("cd $SYSLOCAL; rm aips; ln -s $AIPS_ROOT/START_AIPS aips");
        system("cd $SYSLOCAL; rm UPDCONFIG");
    } else {
        say "*******************************************************";
        say "AIPS_NETSERVER has unrecognized value - no action taken";
        say "*******************************************************";
    }
}
if ($INSTALL_FROM_CD =~ /YES/) {
    say "        -- and populating area for $THISHOST from CD files";
    if (( -d "$AIPS_ROOT/DA00/$ARCH" ) &&
        ( -d "$AIPS_ROOT/DA00/$THISHOST" )) {
        system("cp $AIPS_ROOT/DA00/$ARCH/* $AIPS_ROOT/DA00/$THISHOST/");
    } elsif ( -d "/mnt/cdrom/DA00/$ARCH" ) {
        system("cp /mnt/cdrom/DA00/$ARCH/* $AIPS_ROOT/DA00/$THISHOST/");
    } else {
        say "WARNING: either $ARCH or $THISHOST area missing???";
        say "     (from the $AIPS_ROOT/DA00/ directory)";
        say "     - cannot install AIPS system files for $THISHOST";
        say "     - that means it can't run AIPS!";
    }
}
say "        -- done.";
                                        # ----- Compiler Options -----
if (!($INSTALL_FROM_CD =~ /YES/ || $INSTALL_FROM_NET =~ /YES/ )) {
   say " =====> Deploying your compiler options in script files...";
   open(SRC, "<$SYSLOCAL/CCOPTS.SH");
   open(DST, ">$SYSLOCAL/CCOPTS.SH.new");
   while (<SRC>) {
      if (/^COMPILER=/) {
          printf DST "# Original: $_";
          printf DST "# Changed by install.pl on $now\n";
          printf DST "COMPILER=$CCOM\n";
      } elsif (/^COMP=/) {
          printf DST "# Original: $_";
          printf DST "# Changed by install.pl on $now\n";
          printf DST "COMP=\"$CCOMOPT\"\n";
      } else {
          print DST;
      }
   }
   close(SRC);
   close(DST);
   chmod(0664, "$SYSLOCAL/CCOPTS.SH");
   unlink("$SYSLOCAL/CCOPTS.SH");
   rename("$SYSLOCAL/CCOPTS.SH.new", "$SYSLOCAL/CCOPTS.SH");
   system("chgrp $GROUP $SYSLOCAL/CCOPTS.SH");
   chmod(0664, "$SYSLOCAL/CCOPTS.SH");
   say "        CCOPTS.SH done (C Compiler and options)";

   open(SRC, "<$SYSLOCAL/LDOPTS.SH");
   open(DST, ">$SYSLOCAL/LDOPTS.SH.new");
   while (<SRC>) {
      if (/^COMPILER=/) {
         printf DST "# Original: $_";
         printf DST "# Changed by install.pl on $now\n";
         printf DST "COMPILER=$LINK\n";
      } else {
         print DST;
      }
   }
   close(SRC);
   close(DST);
   chmod(0664, "$SYSLOCAL/LDOPTS.SH");
   unlink("$SYSLOCAL/LDOPTS.SH");
   rename("$SYSLOCAL/LDOPTS.SH.new", "$SYSLOCAL/LDOPTS.SH");
   system("chgrp $GROUP $SYSLOCAL/LDOPTS.SH");
   chmod(0664, "$SYSLOCAL/LDOPTS.SH");
   say "        LDOPTS.SH done (Linker)";
                                        # Now the hard one.  Sigh...
                                        # Need to hack FDEFAULT.SH!
                                        # This HAS to be predictable.
   open(SRC, "<$SYSLOCAL/FDEFAULT.SH");
   open(DST, ">$SYSLOCAL/FDEFAULT.SH.new");
   $foundarch="";
   $donearch="";;
   $compdone ="";
   $coptdone ="";
#                                       $now = &ctime(time); chop($now);
   $now = usertime();
   while (<SRC>) {
      if ($foundarch) {
         if (!$donearch) {
            if (/^ *;;/) {
               $donearch=1;
            }
            if (/^ *COMPILER=/) {
                if (!$compdone) {
                    $orig = $_;
                    s/COMPILER=.*$/COMPILER=$FORT/;
                    printf DST "# Original: $orig";
                    printf DST "# Changed by install.pl on $now\n";
                    $compdone = 1;
                }
            } elsif (/^ *COMPSWIT=/) {
                if (!$coptdone) {
                    $orig = $_;
                    s/COMPSWIT=.*$/COMPSWIT="$FORTOPT"/;
                    printf DST "# Original: $orig";
                    printf DST "# Changed by install.pl on $now\n";
                    $coptdone = 1;
                }
            }
         }
      } elsif (/^ *$ARCH\)/) {
         $foundarch=1;
      }
      print DST;
   }
   close(SRC);
   close(DST);
   chmod(0644, "$SYSLOCAL/FDEFAULT.SH");
   if ( ! -f "$SYSLOCAL/FDEFAULT.SH.dist" ) {
      rename("$SYSLOCAL/FDEFAULT.SH", "$SYSLOCAL/FDEFAULT.SH.dist");
   } else {
      unlink("$SYSLOCAL/FDEFAULT.SH");
   }
   rename("$SYSLOCAL/FDEFAULT.SH.new", "$SYSLOCAL/FDEFAULT.SH");
   system("chgrp $GROUP $SYSLOCAL/FDEFAULT.SH");
   chmod(0644, "$SYSLOCAL/FDEFAULT.SH");
   say "        FDEFAULT.SH done (Fortran compiler and options)";
   askuser("WARNING!  The structure of $SYSLOCAL/FDEFAULT.SH",
        "          is very complex, and while this wizard install",
        "          tries VERY hard to change the right section,",
        "          it may have failed.  Please, PLEASE inspect the",
        "          file and compare it to the original (saved as",
        "          FDEFAULT.SH.dist in $SYSLOCAL)",
        "          to make sure the settings are right.  Do this",
        "          BEFORE you proceed any further.",
        "          THIS IS REQUIRED FOR THE 2.95.3 COMPILER IN LINUX",
        "Press <ENTER> after you have verified FDEFAULT.SH:");
}
                                        # ----- Make NEWEST etc. -----
if ($INSTALL_FROM_CD =~ /YES/) {
                                        # Copy the stuff over from CD...
    say " =====> Copy Utility Programs to $SYSLOCAL...";
    foreach $i ("F2PS", "F2TEXT", "NEWEST", "PRINTENV", "AIPWD", "REVENV") {
        if ( -f "$SYSLOCAL/$i" ) {
            say " - $i already there, won't copy it again";
        } elsif ( -f "$AIPS_ROOT/BIN/$ARCH/$i" ) {
            system("cp $AIPS_ROOT/BIN/$ARCH/$i $SYSLOCAL/");
        } elsif ( -f "/mnt/cdrom/BIN/$ARCH/$i" ) {
            say " - $i not where it should be?  But found on CDROM.  OK...";
            system("cp /mnt/cdrom/BIN/$ARCH/$i $SYSLOCAL/");
        } else {
            say " - $i not found?!  Have to compile it from source.";
        }
    }
}

if (!($INSTALL_FROM_CD =~ /YES/ || $INSTALL_FROM_NET =~ /YES/ )) {
   say " =====> Compile Utility Programs (if needed) in $SYSLOCAL...";
   chdir($SYSLOCAL);
   foreach $i ("F2PS", "F2TEXT", "NEWEST", "PRINTENV", "AIPWD", "REVENV") {
      if ( -x $i ) {
         say "        - found existing $i program; assume it's ok.";
      } else {
         $target = "$SYSUNIX/$i" . ".C";
         $symlink = $i . ".c";
         if ( -e $symlink ) {
            unlink($symlink);
         }
         symlink($target, $symlink);
         if ( -e $i ) {
            unlink($i);
         }
         say "Compiling $i...";
         system("$CCOM -O -o $i $symlink");
         if ( ! -x $i ) {
            aipserror("Failed to compile $i in $SYSLOCAL",
                      "You need to find out why (later), but for now",
                      "let's proceed...");
            $dum = askuser("Press <ENTER> to acknowledge:");
         }
      }
   }
   if ( -x "PP.EXE" ) {
      say "        - found existing PP.EXE program; assume it's ok.";
   } else {
      if ( -e "PP.EXE") { unlink "PP.EXE"; }
      if ( -e "PP.f") { unlink "PP.f"; }
      $target = "$SYSUNIX/PP.FOR";
      symlink($target, "PP.f");
      if (-f "ZTRLOP.c") { unlink "ZTRLOP.c"; }
      if (-f "ZTRLOP.o") { unlink "ZTRLOP.o"; }
      $target = "$AIPS_ROOT/$myvers/SYSTEM/UNIX/ZTRLOP.C";
      symlink($target, "ZTRLOP.c");
      say "        - Compiling ZTRLOP.c...";
      system("$CCOM -c -I$AIPS_ROOT/$myvers/INC ZTRLOP.c");
      if ( -s ZTRLOP.o ) {
         aipserror ("Failed to compile the ZTRLOP.c routine in $SYSLOCAL",
                   "(command: $CCOM -c -I$AIPS_ROOT/$myvers/INC ZTRLOP.c)",
                   "You probably want to interrupt this install now to",
                   "find out why this happened.");
         $dum = askuser("Press control-c to interrupt, or <ENTER> to proceed:");
      } else {
         say "        - done.  Now try building the PP preprocessor...";
      }
      if ($FORT =~ /xlf$/) {
         say "IBM compiler, special xlf command for building PP...";
         system("$FORT -O -qextname -o PP.EXE PP.f ZTRLOP.o");
      } elsif ($ARCH =~ /^HP/) {
         say "HP system, special hack to make PP compile...";
         if ( -f "PP.tmp") {
            unlink("PP.tmp");
         }
         rename("PP.f", "PP.tmp");
         open(TEMPIN, "<PP.tmp");
         open(TMPOUT, ">PP.f");
         while (<TEMPIN>) {
            s/CALL EXIT.*$/STOP/g;
            print TMPOUT $_;
         }
         close(TEMPIN); close (TMPOUT);
         system("$FORT +ppu -o PP.EXE PP.f ZTRLOP.o");
      } else {
         say "$FORT -O -o PP.EXE PP.f ZTRLOP.o";
         system("$FORT -O -o PP.EXE PP.f ZTRLOP.o");
      }
      if ( ! -x "PP.EXE" ) {
         aipserror("Failed to build PP.EXE in $SYSLOCAL",
                  "This will prevent INSTEP2 from starting!!!!",
                  "You need to find out why it failed, but for now",
                  "you can proceed with the rest of the install...");
         $dum = askuser("Press <ENTER> to acknowledge:");
      } else {
         say "        - that seemed to work.";
      }
   }
                                        # ----- Make READLINE -----
   $target = "$AIPS_ROOT/$myvers/$ARCH/LIBR/GNU";
   if ( -e $READLINE && -e "$target/libreadline.a" ) {
      dsay("$READLINE exists and so does $target/libreadline.a");
                                        # skip it
   } elsif (($INSTALL_FROM_CD =~ /YES/) &&
         (-f "/mnt/cdrom/$myvers/$ARCH/LIBR/GNU/libreadline.a" )) {
      dsay("emergency copy from CDROM [someone goofed]");
      system("cp /mnt/cdrom/$myvers/$ARCH/LIBR/GNU/libreadline.a $AIPS_ROOT/$myvers/$ARCH/LIBR/GNU/");
   } else {
     say " =====> Configuring READLINE library...";
     if ( ! -d $target ) {
        mkdir ($target, 0775);
     }
     system("chgrp $GROUP $target");
     if ($GROUPWRITE =~ /YES/) {
        chmod(02775, $target);
     } else {
        chmod(0755, $target);
     }
     chdir($target);
     say "Unpacking the READLINE.SHR archive...";
     $target = "$SYSUNIX/READLINE.SHR";
     symlink($target, "READLINE.SHR");
     system("sh ./READLINE.SHR");
     if ( ! -f "configure") {
        aipserror("Unpacking READLINE.SHR failed!",
                  "There seems to be no 'configure' file in the",
                  "$AIPS_ROOT/$myvers/$ARCH/LIBR/GNU/ directory.",
                  "Proceeding further now will cause INSTEP3/INSTEP4",
                  "to fail when they try to link any AIPS tasks.",
                  "Check why the sh READLINE.SHR failed.");
        $dum = askuser("Press <ENTER> to acknowledge:");
     } else {
        say "Unpacking seemed to work.";
     }
     $target = "configure";
     chmod 0775, $target;
#    mkdir "support", 0775;
     chdir("support");
#    $target = "$SYSUNIX/RLSUPP.SHR";
#    symlink($target, "RLSUPP.SHR");
#    say "Unpacking the RLSUPP.SHR archive...";
#    system("sh ./RLSUPP.SHR");
#    if ( ! -f "config.guess") {
#       aipserror("Unpacking RLSUPP.SHR failed!",
#                 "There seems to be no 'config.guess' file in the",
#                 "$AIPS_ROOT/$myvers/$ARCH/LIBR/GNU/support area.",
#                 "Proceeding further now will cause INSTEP3/INSTEP4",
#                 "to fail when they try to link any AIPS tasks.",
#                 "Check why the sh RLSUPP.SHR failed.");
#       $dum = askuser("Press <ENTER> to acknowledge:");
#    } else {
#       say "Unpacking seemed to work.";
#    }
     @targets = ("config.guess", "config.sub");
     chmod 0775, @targets;
     chdir("..");
     say "Running Readline 'configure' in quiet mode, and then make...";
     system("./configure --quiet");
     system("make");
     if ( -f "libreadline.a") {
        say "The GNU readline library was apparently created.";
     } else {
        aipserror("Either the configure or make step failed in",
                  "$AIPS_ROOT/$myvers/$ARCH/LIBR/GNU/.",
                  "This means INSTEP3/INSTEP4 will not work.");
        $dum = askuser("Press <ENTER> to acknowledge:");
     }
   }
                                        # doesn't really belong here,
                                        # but doesn't have a home
                                        # anywhere else either!
   $dotwo = "$SYSLOCAL/DOTWOLIB";
   if ($DOTWOLIB =~ /YES/) {
      if ( ! -f $dotwo) {
         open(FOO, ">$dotwo");
         close(FOO);
      }
   } else {
      if ( -f $dotwo) {
         unlink($dotwo);
      }
   }
                                        # ----- Make XAS -----
   say " =====> Make the XAS TV Server from source if needed...";
   $target = "$AIPS_ROOT/$myvers/$ARCH/LOAD";
   if ( ! -d $target ) {
      mkdir ($target, 0775);
   }
   system("chgrp $GROUP $target");
   if ($GROUPWRITE =~ /YES/) {
      chmod(02775, $target);
   } else {
      chmod(0755, $target);
   }
   $target .= "/XAS";
                                        # bug in makefile, work around.
   system("touch $target 2>/dev/null");
   if ( -x $target ) {
      say "XAS seems to be already built; good.";
   } else {
      $target = "$AIPS_ROOT/$myvers/Y/SERVERS/XAS";
      if ( ! -d $target ) {
         mkdir ($target, 0775);
      }
      system("chgrp $GROUP $target");
      if ($GROUPWRITE =~ /YES/) {
         chmod(02775, $target);
      } else {
         chmod(0755, $target);
      }
      chdir($target);
      if ( -f "xas.h" ) {
         say "XAS.SHR appears to be already unpacked.";
      } else {
         say "About to unpack the XAS.SHR archive.";
         system("sh ../XAS.SHR");
         say "Done.";
         if ( ! -f "xas.h" ) {
            aipserror("Failed to unpack the XAS.SHR archive",
                      "This means no AIPS TV will be available.");
            $dum = askuser("Press <ENTER> to acknowledge:");
         }
      }
                                        # need to put compiler into the
                                        # Makefile if it's changed.
      open(OLD, "<Makefile");
      open(NEW, ">Makefile.new");
      while (<OLD>) {
         if (/^CC_$ARCH/) {
            printf NEW "CC_$ARCH = $CCOM\n";
         } elsif (/^LOCALOPTS_$ARCH/) {
            printf NEW "LOCALOPTS_$ARCH = $CCOMOPT\n";
         } elsif ((/^LIBDIRS_$ARCH/) && ($IS64 =~ /YES/)) {
            printf NEW "LIBDIRS_$ARCH = -L/usr/X11R6/lib64\n";
         } else {
            print NEW;
         }
      }
      close(OLD); close(NEW);
      if ( ! -f "Makefile.orig") {
         rename("Makefile", "Makefile.orig");
      } else {
         unlink("Makefile");
      }
      rename("Makefile.new", "Makefile");
      say "About to try to make XAS...\n";
      system("sh -c '. $AIPS_ROOT/LOGIN.SH; make clean; make'");
      $target = "$AIPS_ROOT/$myvers/$ARCH/LOAD/XAS";
      if ( -x $target ) {
         say "That seemed to work.";
      } else {
         aipserror("Failed to build the XAS program.",
                  "This means no AIPS TV will be available",
                  "until you correct the Makefile and do make",
                  "in area $YSERV/XAS.");
         $dum = askuser("Press <ENTER> to acknowledge:");
      }
   }
}
                                        # ----- Prep for INSTEP2/4 -----
if (!($INSTALL_FROM_CD =~ /YES/ || $INSTALL_FROM_NET =~ /YES/ )) {
    say " =====> Preparing for INSTEP2 and INSTEP4 (building AIPS)...";
    $target = "$AIPS_ROOT/$myvers/$ARCH/INSTALL/";
    if (! -d $target ) {
        mkdir ($target, 0775);
    }
    system("chgrp $GROUP $target");
    if ($GROUPWRITE =~ /YES/) {
        chmod(02775, $target);
    } else {
        chmod(0755, $target);
    }
    chdir($target);
    unlink("INSTEP2");
    $target = "$SYSUNIX/INSTALL/INSTEP2";
    system("cp $target INSTEP2");
    unlink("INSTEP4");
    $target = "$SYSUNIX/INSTALL/INSTEP4";
    system("cp $target INSTEP4");
    foreach $i ("ERRORS", "MEMORY", "PREP", "TEMPLATE", "UPDATE") {
        $target = "$AIPS_ROOT/$myvers/$ARCH/$i";
        if ( ! -d $target ) {
            mkdir ($target, 0775);
        }
        system("chgrp $GROUP $target");
        if ($GROUPWRITE =~ /YES/) {
            chmod(02775, $target);
        } else {
            chmod(0755, $target);
        }
    }
}
                                        # ----- Set up MNJ -----
                                        # Don't if this is a CDROM install.
                                        # Don't if this is a frozen install.
if (!($INSTALL_FROM_CD =~ /YES/ || $INSTALL_FROM_NET =~ /YES/) && ($mytype =~ /TST/)) {
    say " =====> Preparing for the \"Midnight Job\"...";
    open(SRC, "<$SYSLOCAL/MAKE.MNJ");
    open(DST, ">$SYSLOCAL/MAKE.MNJ.new");
    while (<SRC>) {
        if (/^COMPILER=/) {
            printf DST "# Original: $_";
            printf DST "# Changed by install.pl on $now\n";
            printf DST "COMPILER=$CCOM\n";
        } elsif (/^COMP=/) {
            printf DST "# Original: $_";
            printf DST "# Changed by install.pl on $now\n";
            printf DST "COMP=\"$CCOMOPT\"\n";
        } else {
            print DST;
        }
    }
    close(SRC);
    close(DST);
    chmod(0664, "$SYSLOCAL/MAKE.MNJ");
    unlink("$SYSLOCAL/MAKE.MNJ");
    rename("$SYSLOCAL/MAKE.MNJ.new", "$SYSLOCAL/MAKE.MNJ");
    system("chgrp $GROUP $SYSLOCAL/MAKE.MNJ");
    chmod(0775, "$SYSLOCAL/MAKE.MNJ");
    say "        MAKE.MNJ done (C Compiler and options)";

    $target = "$AIPS_ROOT/$myvers/$ARCH/UPDATE";
    chdir($target);
    if (( -x "UPDLSTDAT" ) && ( -x "UPDOBSLT" )) {
        say "Midnight Job area $target seems OK.";
    } else {
        if ( -r $tarball ) {
            $tt = mnjfilemtime($tarball);
        } elsif ( -f "$AIPS_ROOT/$myvers/TIMESTAMP" ) {
            $tt = mnjfilemtime("$AIPS_ROOT/$myvers/TIMESTAMP");
        } else {
            $tt = mnjfilemtime("$AIPS_ROOT/$myvers");
        }
        if ($tt =~ /01$/) {
            $y = substr($tt,0,4);
            $m = substr($tt,4,2);
            $d = substr($tt,6,2);
            if ($m =~ /01/) {
                $y = $y - 1;
                $m = "12";
                $d = "31";
            } else {
                $m = $m - 1;
                if ($m == 4 || $m == 6 || $m == 9 || $m == 11) {
                    $d = 30;
                } elsif ($m != 2) {
                    $d = 31;
                } else {
                                        # dang leap years
                    $n = int(4 * ($y / 4));
                    $nn = int(100 * ($y / 100));
                    $nnn = int(400 * ($y / 400));
                    if ($n == $y) {
                        $d = 29;
                        if ($nn == $y) {
                            $d = 28;
                            if ($nnn == $y) {
                                $d = 29;
                            }
                        }
                    }
                }
                                        # eventually need time too?
                $tt = sprintf("%4.4d%2.2d%2.2d", $y, $m, $d);
            }
        } else {
            $tt = $tt - 1;
        }
        say ("\nThe AIPS Midnight Job is a way to keep your AIPS",
                 "installation up to date.  It is secure, relatively",
                 "easy, and can be done on demand or in a cron job",
                 "(i.e. periodically, daily or weekly or whenever).",
                 "It does require that you have the cvs software",
                 "package installed on your system.\n",
                 "This script will set up the infrastructure for you",
                 "to be *able* to run the Midnight Job (MNJ), but you",
                 "have the power to make it run or not.\n");
        if ($tt =~ /^-1/) {
            $n = askuser("Should you choose to run the Midnight Job, the",
                 "'begin date' has to be set by you - I failed to guess",
                 "a good value.  Enter the start date in the format",
                 "YYYYMMDD now:");
        } else {
            $n = askuser("Should you choose to run the Midnight Job, the",
                 "'begin date' will be set to $tt (format is YYYYMMDD);",
                 "This is ONE day before the generation time",
                 "of $tarball (if I've subtracted right!)\n",
                 "If you want a different date, enter it now:");
        }
        $begindate = $tt;
        if ($n) {
            if ($n !~ /^\d\d\d\d\d\d\d\d$/) {
                aipserror("Bad input ($n is not 8 digits)",
                          "Will stick to default of $begindate.");
            } else {
                $begindate = $n;
            }
        }
        $target = "$SYSLOCAL/MAKE.MNJ " . $begindate;
        say("About to run '$target'...",
            "This will generate a fair bit of output.");
        say("Engaging MAKE.MNJ...\n");
        system("sh -c '. $AIPS_ROOT/LOGIN.SH; \$CDTST; $target'");
        if ( -f "LASTGOOD.DOC" ) {
            say("=========================================================",
                "MAKE.MNJ seemed to work (LASTGOOD.DOC found)",
                "*****  You will have to edit $SYSLOCAL/UPDCONFIG",
                "*****  to correct your e-mail address");
            $dum = askuser("Press <ENTER> to continue...");
        } else {
            say("Not good; no LASTGOOD.DOC found; did MAKE.MNJ fail?",
                "You should investigate this.");
            $dum = askuser("Press <ENTER> to acknowledge:");
        }
    }
}
                                         # MNJ for binary
if (($INSTALL_FROM_NET =~ /YES/) && ($mytype =~ /TST/)) {
    $target = "$AIPS_ROOT/$myvers/$ARCH/UPDATE";
    chdir($target);
    say ("\nThe AIPS Midnight Job is a way to keep your AIPS",
         "installation up to date.  It is secure, relatively",
         "easy, and can be done on demand or in a cron job",
         "(i.e. periodically, daily or weekly or whenever).",
         "It does require that you have the cvs software",
         "package installed on your system.\n",
         "This script will set up the infrastructure for you",
         "to be *able* to run the Midnight Job (MNJ), but you",
         "have the power to make it run or not.\n");
    $target = "MAKE.BMNJ ";
    system("chgrp $GROUP MAKE.BMNJ");
    chmod(0775, "MAKE.BMNJ");
    say("About to run '$target'...",
        "This will generate a fair bit of output.");
    system("sh -c '. $AIPS_ROOT/LOGIN.SH; \$CDTST; $target'");
    $target = "$SYSLOCAL/UPDCONFIG";
    if ( -f $target ) {
        say("=========================================================",
            "MAKE.BMNJ seemed to work ($target found)"),
            "*****  You will have to edit $target",
            "*****  to correct your e-mail address";
        $dum = askuser("Press <ENTER> to continue...");
    } else {
        say("Not good; no $target found; did MAKE.BMNJ fail?",
            "You should investigate this.");
        $dum = askuser("Press <ENTER> to acknowledge:");
    }
}
                                        # ----- Send Registration -----
                                        # It needs overhauled.
chdir($AIPS_ROOT);
#if ( ! -f "$AIPS_ROOT/REGISTER.INFO" ) {
#    say " =====> Firing up the registration script...";
#    say "        (interrupt via control-c if you wish to do this later)";
#    if ( ! -e "$AIPS_ROOT/REGISTER") {
#        symlink("$myvers/SYSTEM/UNIX/INSTALL/REGISTER",
#                "$AIPS_ROOT/REGISTER");
#    }
#    system("sh -c '. ./LOGIN.SH; ./REGISTER'");
#}
                                        # ----- Start INSTEP2/4 -----
                                        # but first check user's shell
$shell = $ENV{'SHELL'};
if (($shell =~ /\/bash/) ||
    ($shell =~ /\/ksh/) ||
    ($shell =~ /\/zsh/) ||
    ($shell =~ /\/sh/)) {
    $shell = "bourne";
} elsif ($shell =~ /csh/) {
    $shell = "c";
} else {
    say ("I don't know if your shell '$shell' is bourne or c like!",
         "The instructions that follow will assume a bourne shell",
         "which covers sh (bourne), ksh (korn), bash, and zsh.");
    $shell = "bourne";
}

if ( !($INSTALL_FROM_CD =~ /YES/ || $INSTALL_FROM_NET =~ /YES/) ) {
    say ("=========================================================",
         " =====> start INSTEP2 and INSTEP4...\n");
    $yn = askuser("This takes hours, is VERY verbose (lots of output).",
              "It compiles the AIPS libraries (INSTEP2),",
              "and then links the 400 or so AIPS tasks (INSTEP4).",
              "We can start it now, or you can run it yourself later",
              "(instructions will be given if you do it later).\n",
              "Shall I start INSTEP2 and then INSTEP4 here, now? [Y]");
    if ($yn !~ /^[Nn]/i) {
        chdir("$AIPS_ROOT/$myvers/$ARCH/INSTALL");
        $cmd = "sh -c '. $AIPS_ROOT/LOGIN.SH; \$CDTST; ./INSTEP2'";
        $result = system($cmd) / 256;
        if ($result == 0) {
            say "That seemed to work.  Onwards...";
        } else {
            aipserror("INSTEP2 failed, no point in proceeding.",
                  "Look in $AIPS_ROOT/$myvers/$ARCH/INSTALL to see",
                  "if you can figure out what went wrong.  There are",
                  "many log and list files there.  You may want to do" .
                  "\n\ngrep -v ^- *.LIS\n\n",
                  "to see what modules did not compile.");
            bailout();
        }
        $cmd = "sh -c '. $AIPS_ROOT/LOGIN.SH; \$CDTST; ./INSTEP4'";
        $result = system($cmd) / 256;
        if ($result == 0) {
            say "That seemed to work.  GREAT!!!!";
        } else {
            aipserror("INSTEP4 failed, no point in proceeding.",
                  "Look in $AIPS_ROOT/$myvers/$ARCH/INSTALL to see",
                  "if you can figure out what went wrong.  There are",
                  "many log and list files there.  You may want to do" .
                  "\n\ngrep -v ^- *.LIS\n\n",
                  "to see what modules did not compile or link.");
            bailout();
        }
    } else {
        say ("OK.  Here are the instructions for your shell:\n\n");
        if ($shell =~ /bourne/) {
            say("\t. $AIPS_ROOT/LOGIN.SH; \$CDTST\n" .
                "\tcd $AIPS_ROOT/$myvers/$ARCH/INSTALL\n" .
                "\t( ./INSTEP2 >/dev/null 2>>INSTEP2.ERR )\n",
                "(and 'INSTEP4 >/dev/null 2>>INSTEP4.ERR' thereafter)");
        } else {
            say("\tsource $AIPS_ROOT/LOGIN.CSH; \$CDTST\n" .
                "\tcd $AIPS_ROOT/$myvers/$ARCH/INSTALL\n" .
                "\t( ( INSTEP2 >/dev/null ) >& INSTEP2.ERRS & )\n",
                "(and likewise for INSTEP4 thereafter)");
        }
        say("This lets you log out and check progress later via\n\n" .
            "\ttail -f INSTEP2.LOG\n",
            "(or INSTEP4.LOG depending on which step you're doing)",
        "One disadvantage is that the INSTEPn's are *very* hard to",
        "interrupt or stop if bad things happen!  If you prefer, you",
        "can just type ./INSTEP2 and leave it run in that window",
        "(and later INSTEP4).\n",
         "When INSTEP4 is done, give the commands:\n\n" .
        "\tRUN FILAIP\n\tRUN POPSGN\n",
        "to generate the (vital!) memory files.  FILAIP may give some",
        "errors about files already existing; you can ignore them.\n",
        "Finally, enable a cron job to run the do_daily script that",
        "was created by MAKE.MNJ above.");
        say("This script has done all it can for you.  Good luck!");
        exit(0);
    }
}
                                        # ----- RUN FILAIP/POPSGN -----
if ($INSTALL_FROM_CD =~ /YES/) {
    say "=====> Optional regen of system files (FILAIP/POPSGN)...";
    say("Your CD distribution for $ARCH includes pre-made system files",
        "(the output of FILAIP and POPSGN).  Using them will get",
        "AIPS up and going fast, but you may want to change the",
        "settings.  FILAIP was run with the following parameters:\n",
        "  #interactive AIPS, #batch queues: 8 2"
        );
    $yn = askuser("Re-run FILAIP to change these numbers? [N]");
    if ($yn !~ /^[Yy]/i) {
        chdir($AIPS_ROOT);
        goto END_POPSGN;
    }
} else {
    say " =====> Generate system files via FILAIP/POPSGN...";
    say("This will generate some output, including a copyright (left)",
    "statement and some prompts, but should end with a 'Done!' if",
    "everything worked...\n");
}
chdir($AIPS_ROOT);
$ntap2 = 2 + $ntap;
if ($ntap2 == 2) {
    $ntap2 = 4;
}
if ($ARCH =~ /MACINT/) {
   $f = "$AIPS_ROOT/$myvers/MACINT/SYSTEM/fix_aips_elcap.sh";
   $o = "$AIPS_ROOT/$myvers/MACINT/SYSTEM/$SITE/fix_aips_elcap.sh";
   system("cp -f $f $o");
   system("chmod +x $o");
   $o = "$AIPS_ROOT/fix_aips_elcap.sh";
   system("cp -f $f $o");
   system("chmod +x $o");
   say "***************** READ CAREFULLY **************************\n";
   say "If you are running on version 10.11 of the Max OS X called\n" .
       "El Capitan, you need to do one more thing NOW to correct for\n" .
       "the loss of the DYLD_LIBRARY_PATH functionality.  In a 2nd\n" .
       "window, start bash, source $AIPS_ROOT/LOGIN.SH\n" .
       "Then .$AIPS_ROOT/fix_aips_elcap.sh \n" .
       "Do not run as root or under sudo and note the dot\n" .
       "you will be prompted for your password once.\n";
    $yn = askuser("Hit return in this window when ready.");
}

say("About to run FILAIP.  If this is a re-installation, you",
    "will be prompted for an AIPS Manager password.  The",
    "default password is AMANAGER\n");
$cmd = "| ( sh -c '. $AIPS_ROOT/LOGIN.SH; RUN FILAIP' ) ";
open(PIPE, $cmd);
printf PIPE "8 2\n";
close(PIPE);
if ( -f "DA00/$THISHOST/ACD000000;" ) {
    say("FILAIP seems to have run correctly.  Now running POPSGN.",
        "This will show a prompt; don't type anything, as I will",
        "enter the magic incantations.  If it works, you will see",
        "the message 'Popsgen complete' momentarily...\n");
} else {
    aipserror("FILAIP did not work, or there's a missing file!",
              "This may mean that FILAIP did not work (it should have",
              "created it and many other files in that area).",
              "You can try running it by hand after (a) dotting the",
              "LOGIN.SH file or sourcing the LOGIN.CSH file; and",
              "(b) Typing 'RUN FILAIP'.  You will then be asked for",
              "two parameters.  Use\n8 2\n",
              "as the standard values.",
              "You will also have to RUN POPSGN if that works;\n",
              "Feed it\n\n0 POPSDAT TST\n\n",
              "when it asks for Idebug, etc. and press RETURN when",
              "you see the '>' prompt.\n\n");
    bailout();
}
                                        # for CDs', don't do this!
if ($INSTALL_FROM_CD !~ /YES/) {
    $cmd = "| ( sh -c '. $AIPS_ROOT/LOGIN.SH; RUN POPSGN' ) ";
    open(PIPE, $cmd);
    printf PIPE "0 POPSDAT TST\n\n";
    close(PIPE);
}
END_POPSGN:
say("Copying DA00/$THISHOST/* files to $myvers/$ARCH/TEMPLATE/ area\n");
system("cp DA00/$THISHOST/* $AIPS_ROOT/$myvers/$ARCH/TEMPLATE");
                                        # ----- Instructions -----
say " =====> We're DONE!  Let's have a nice Banana Split! <======\n";
say "***************** READ CAREFULLY **************************\n";
if ($shell =~ /bourne/) {
    $instr = "Reference the LOGIN.SH file in your .profile file\n" .
        "\t      or perhaps your .bashrc file\n" .
        "\t      (dot it now too, via \". ./LOGIN.SH\")\n";
} else {
    $instr = "Reference the LOGIN.CSH file in your .login file\n" .
        "\t      or perhaps your .tcshrc or .chhrc file\n" .
        "\t      (source it now too, via \"source ./LOGIN.CSH\")\n";
}
say "***************** READ CAREFULLY **************************\n";
if ($servstring ne "ok") {
    say("Services should be defined either in /etc/services\n or" .
        "your YP/NIS services map (all tcp services)\n" .
        "This may require sudo or root privileges\n");
}
if ($ARCH =~ /MACINT/) {
   say "***************** READ CAREFULLY **************************\n";
   say "On Macs, you may need to create an /etc/sysctl.conf file\n" .
       "Put in it the following lines\n" .
       "     kern.sysv.shmmax=10485760\n" .
       "     kern.sysv.shmmin=1\n" .
       "     kern.sysv.shmmni=32\n" .
       "     kern.sysv.shmseg=8\n" .
       "     kern.sysv.shmall=4096\n" .
       "Then reboot your computer and check with the command\n" .
       "     sysctl kern.sysv";
}
say "***************** READ CAREFULLY **************************\n";
say("Here are the final setup instructions for running AIPS\n\n" .
    "\t1.  $instr\n" .
    "\t2.  Check that it works:\n\n\t\taips notv tpok\n\n" .
    "\t      (this will not start a TV or tape servers).\n" .
    "\t    Try 'print 2 + 2' for a very basic test.\n\n" .
    "\t3.  Make a cron entry for the do_daily.$THISHOST file\n" .
    "\t      that the MAKE.MNJ created, so you can run the\n" .
    "\t      AIPS 'midnight job'.  This is optional but\n" .
    "\t      strongly recommended.\n",
    "That's it.  You should now have the latest AIPS!  Enjoy.");
                                         # this is the end.  Really.
