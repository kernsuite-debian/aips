#!/usr/local/bin/perl
#-----------------------------------------------------------------------
#;  Copyright (C) 1999-2015
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
# helper for update, sort of a stripped-down INSTEP1.  Ugh.
# It needs work, and was written, not designed.  You've been warned...
#
#-----------------------------------------------------------------------
require 'getopts.pl';
                                        # -a aipsroot
                                        # -d for debug/verbose messages
                                        # -f tarball-location
&Getopts('a:df:');
                                        # flush output
$| = 1;
$myvers = "31DEC16;
                                        # CHANGE THIS AS NEEDED!!!  It's
                                        # the start date for the MNJ.
$begindate = "20151201";
$usage = "\nUsage:\tupdate.pl [-a aipsroot] [-f tarball]\n\n";
$usage .= "\t-a\tAllows you to point at AIPS_ROOT if not defined;\n";
$usage .= "\t\d\tTurn on debug messages;\n";
$usage .= "\t\f\tLocation of $myvers.tar.gz (default is here)\n\n";

                                        ########### Functions ##########
sub filemtime {
    my $sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst;
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
                                        ########### Now do it ##########
                                        # Move to AIPS_ROOT area
if (!$opt_a) {
    if (!$ENV{AIPS_ROOT}) {
        printf STDERR $usage;
        die "AIPS_ROOT not defined and '-a' not used!\n";
    }
    $opt_a = $ENV{AIPS_ROOT};
}
chdir($opt_a) || die "Cannot cd to $opt_a because $!\n";

                                        # a few convenient definitions
$sysunix = $opt_a . "/" . $myvers . "/SYSTEM/UNIX/";
                                        # no trailing "/" on $inc!!!
$inc = $opt_a . "/" . $myvers . "/INC";
$yserv = $opt_a . "/" . $myvers . "/Y/SERVERS/";

                                        # Architecture,
$arch = $ENV{ARCH};
if (!$arch) {
    die "ARCH is not defined!  Old AIPS environment needed.\n";
}
if (($arch =~ /LINUX/) || ($arch =~ /BSD/)) {
    $cc = "gcc";
    $f77 = "g77";
    printf "Debug: using gcc/g77, found Linux or *BSD\n" if ($opt_d);
} else {
    printf "Using ARCH=$arch; This should be the AIPS architecture\n";
    $cc = "cc";
    $f77 = "f77";
    printf "Debug: using cc/f77, not Linux or *BSD\n" if ($opt_d);
}

$tarball = $myvers . ".tar.gz";
if ($opt_f) {
    $tarball = $opt_f;
}
printf "--------- unpacking stuff --------------\n";
if ( -d $myvers ) {
    printf "$myvers directory exists; assume unpacking done.\n";
} else {
    if (! -f $tarball) {
        printf STDERR $usage;
        die "Cannot find $tarball to unpack!\n";
    }
    $path = $ENV{PATH};
    @paths = split(":", $path);
    $gunzip = "";
    foreach $i (@paths) {
        $testit = $i . "/gunzip";
        if ( -x $testit ) {
            $gunzip = $testit;
            printf "Debug: found $gunzip\n" if ($opt_d);
            last;
        }
    }
    if (!$gunzip) {
        die "Cannot find 'gunzip' in your path ($path)\n";
    }
    if ( -d "TEXT.OLD" ) {
        if ( -d "TEXT" ) {
            printf "TEXT.OLD exists, will try to delete it...\n";
            system ("/bin/rm -fr TEXT.OLD");
            if ( -d "TEXT.OLD" ) {
                printf "That didn't work!  TEXT.OLD is still there.\n";
                printf "Proceeding, but the tar command will cause\n";
                printf "many 'file exists' error messages.\n";
            }
        } else {
            printf "TEXT already moved to TEXT.OLD; good.\n";
        }
    }
    if ( -d "TEXT" ) {
        printf "Moving TEXT directory to TEXT.OLD ...";
        rename ("TEXT","TEXT.OLD") || die "FAILED: $!\n";
        printf " done.\n";
    }
    if (($arch =~ /LINUX/) || ($arch =~ /BSD/)) {
        $tarcmd = "tar zxvf $tarball";
    } else {
        $tarcmd = "$gunzip -c $tarball | tar xvf -";
    }
    open(PIPE, "$tarcmd|") || die "cannot execute $tarcmd: $!\n";
    $nlines = 0;
    printf "Unpacking tarbal with $tarcmd, dot every 100 files:\n";
    while (<PIPE>) {
        $nlines++;
                                        # this may not work unless we
                                        # flush things... some feedback.
        if (((int($nlines/100))*100) == $nlines) {
            printf ".";
        }
    }
    close(PIPE);
    printf "\nDone ($nlines files)\n\n";
}
printf "------------- Set up temporary definition files -----------\n";
foreach $i ("AIPSPATH.SH", "AIPSPATH.CSH") {
    $ilc = lc($i);
    if ( -f $ilc ) {
        printf "Zapping old $ilc...";
        unlink($ilc) || die "failed: $!\n";
    }
    open (NEW, ">$ilc") || die "cannot create new $ilc: $!\n";
    open (OLD, "<$i") || die "cannot read $i: $!\n";
    while (<OLD>) {
        chomp;
        if (/^TST=\$AIPS_ROOT\/(\d\d...\d\d)/) {
            $oldtst = $1;
            printf NEW "TST=\$AIPS_ROOT/%s\n", $myvers;
        } elsif (/^setenv TST \$AIPS_ROOT\/(\d\d...\d\d)/) {
            $oldtst = $1;
            printf NEW "setenv TST \$AIPS_ROOT/%s\n", $myvers;
        } else {
            printf NEW "%s\n", $_;
        }
    }
    close(OLD); close(NEW);
    printf "Created %s with %s as TST\n", $ilc, $myvers;
    $shtype = $ilc;
    $shtype =~ s/aipspath.//;
    $SHTYPE = uc($shtype);
    $logsh = "login." . $shtype;
    $src = ".";
    if ($shtype =~ /csh/i) {
        $src = "source";
    }
    if ( -f $logsh ) {
        printf "No need to create $logsh, already exists\n";
    } else {
        open (LOG, ">$logsh") || die "Cannot create $logsh: $!\n";
        printf LOG "%s %s/aipspath.%s\n", $src, $opt_a, $shtype;
        printf LOG "%s %s/AIPSASSN.%s\n", $src, $opt_a, $SHTYPE;
        printf LOG "%s \$SYSUNIX/AREAS.%s\n", $src, $SHTYPE;
        printf LOG "echo AIPS_VERSION=\$AIPS_VERSION\n";
        close(LOG);
        printf "Created $logsh for defining new environment\n";
    }
}
printf "-------------- check GETRLS.FOR ---------------\n";
$getrls = $myvers . "/APL/SUB/GETRLS.FOR";
if ( ! -f $getrls ) {
    die "Something is wrong!  Cannot find $getrls\n";
}
$release = `grep '^      DATA CURRLS' $getrls`; chomp($release);
$release =~ s/^.*(\d{2}\S{3}\d{2}).*$/$1/;
if ($release == $myvers) {
    printf "$getrls has correct version ($myvers)\n";
} else {
    printf "Need to put $myvers in $getrls (had $release)\n";
    open (OLD, "<$getrls") || die "Can't read $getrls: $!\n";
    $new = $getrls . ".new";
    open (NEW, ">$new") || die "Cannot create $new: $!\n";
    while (<OLD>) {
                                        # sledgehammer!
        s/$release/$myvers/;
        print NEW;
    }
    close(OLD); close(NEW);
    $bad = $getrls . ".bad";
    rename ($getrls, $bad);
    rename ($new, $getrls);
    printf "Fixed $getrls\n";
}
printf "-------------- check AP size ----------------\n";
$papc = $opt_a . "/" . $myvers . "/INC/NOTST/" . $arch . "/PAPC.INC";
if (! -f $papc) {
    printf "no $papc found...\n";
    $papc = $myvers . "/INC/PAPC.INC";
    if (! -f $papc) {
        die "Cannot find $papc!\n";
    }
}
$apsize = `grep '^      PARAMETER (AP' $papc`; chomp ($apsize);
$apsize =~ s/.*APSIZE=(\d\d+).*$/$1/;
                                        # convert to Megabytes
$apsize = $apsize / 262144;
printf "Based on $papc, current AP size is $apsize Mbytes.\n";
printf "If this is not acceptable, please edit that file now,\n";
if ($papc !~ /INC\/NOTST/) {
    printf "or put a copy in $myvers/INC/NOTST/$arch/\n";
    printf "(but check $syslocal/INCS.SH first!!!)\n";
}
printf "Press <RETURN> when you are ready to proceed...";
$dum = <STDIN>;
printf "-------------- programs in \$SYSLOCAL -------------\n";
$site = $ENV{SITE};
$syslocal = $myvers . "/" . $arch . "/SYSTEM/";
$oldlocal = $syslocal . $site . "/";
if (! -d $oldlocal) {
    mkdir($oldlocal,0775) || die "Cannot create to $oldlocal: $!\n";
}

                                        # copy stuff to syslocal
opendir(SA, "$myvers/$arch/SYSTEM");
while (defined($f = readdir(SA))) {
                                        # skip directories, etc.
    $o = "$syslocal/$f";
    next if ( -d $o );
    next if ($f =~ /^AIPS$/i);
    next if ($f =~ /^LD.UU$/);
    $n = "$oldlocal/$f";
    if ( -f $n ) {
        printf "output file $o pre-exists\n";
        $ortime = filemtime($o);
        $nrtime = filemtime($n);
        if ($nrtime > $ortime) {
            printf "        Skipping $f (already copied to SYSLOCAL)";
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
        printf "Failed to copy $f to SYSLOCAL; cannot proceed!";
        bailout();
    }
}
closedir(SA);
$syslocal = $oldlocal;

printf "Using $syslocal as \$SYSLOCAL area\n";
chdir($syslocal) || die "Cannot move to $syslocal: $!\n";
foreach $i ("F2PS", "F2TEXT", "NEWEST", "PRINTENV", "AIPWD", "REVENV") {
    if ( -x $i ) {
        printf "Found existing $i program; good.\n";
    } else {
        $target = $sysunix . $i . ".C";
        printf "target = $target\n";
        $symlink = $i . ".c";
        if (-f $symlink) {
            unlink($symlink);
        }
        printf "symlink = $symlink\n";
        symlink($target, $symlink);
        if (-f $i) {
            unlink($i);
        }
        printf "Compiling $i...";
        system("$cc -O -o $i $symlink");
        if ( -x $i ) {
            printf "...done\n";
        } else {
            die "Oops: failed to create executable $i in $syslocal\n";
        }
    }
}
if ( -x "PP.EXE" ) {
    printf "Found existing PP.EXE program; good.\n";
} else {
    printf "Now creating PP.EXE from PP.FOR...\n";
    if (-f "PP.EXE") { unlink "PP.EXE"; }
    if (-f "PP.f") { unlink "PP.f"; }
    $target = $sysunix . "PP.FOR";
    symlink($target, "PP.f");
    if (-f "ZTRLOP.c") { unlink "ZTRLOP.c"; }
    if (-f "ZTRLOP.o") { unlink "ZTRLOP.o"; }
    $target = $opt_a . "/" . $myvers . "/SYSTEM/UNIX/ZTRLOP.C";
    symlink($target, "ZTRLOP.c");
    printf "ZTRLOP compile: $cc -c -I$inc ZTRLOP.c\n";
    system("$cc -c -I$inc ZTRLOP.c");
    if ($arch =~ /IBM/) {
        system("xlf -O -qextname -o PP.EXE PP.f ZTRLOP.o");
    } elsif ($arch =~ /^HP/) {
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
        system("$f77 +ppu -o PP.EXE PP.f ZTRLOP.o");
    } else {
        system("$f77 -O -o PP.EXE PP.f ZTRLOP.o");
    }
    if ( -x "PP.EXE" ) {
        printf "Done.\n";
    } else {
        die "Oops: failed to create executable PP.EXE in $syslocal\n";
    }
}
$dotwo = "DOTWOLIB";
if ( -f $dotwo ) {
    printf "$dotwo found, DEBUG and NODEBUG libraries will be built.\n";
} else {
    printf "No $dotwo, so no DEBUG libraries will be built.\n";
}
printf "If you want to change this, do so now.\n";
printf "Press <RETURN> to continue..."; $dum = <STDIN>;
                                        # Symlinks AIPS and aips...
$target = $opt_a . "/START_AIPS";
if ( -e "AIPS") {
    if ( ! -e "AIPS.OLD" ) {
        rename("AIPS", "AIPS.OLD");
        printf "Moved AIPS to AIPS.OLD in $syslocal\n";
    } elsif ( -l "AIPS" ) {
        printf "AIPS symlink already exists, assume it's ok.\n";
    } else {
        printf "WARNING: non-symlink AIPS found in $syslocal\n";
        printf "         It should point to $opt_a/START_AIPS\n";
        printf "         You should check & remove this (I won't)!\n";
        printf "         Press <RETURN> when done...";
        $dum = <STDIN>;
    }
}
symlink($target, "AIPS");
if ( -e "aips") {
    if ( ! -e "aips.OLD" ) {
        rename("aips", "aips.OLD");
        printf "Moved aips to aips.OLD in $syslocal\n";
    } elsif ( -l "aips" ) {
        printf "aips (lowercase) symlink exists, assume it's ok.\n";
    } else {
        printf "WARNING: non-symlink aips (lc) found in $syslocal\n";
        printf "         It should point to $opt_a/START_AIPS\n";
        printf "         You should check & remove this (I won't)!\n";
        printf "         Press <RETURN> when done...";
        $dum = <STDIN>;
    }
}
symlink($target, "aips");
printf "-------------- Readline Library ---------------\n";
$target = $opt_a . "/" . $myvers . "/" . $arch . "/LIBR";
if ( ! -d $target ) {
    mkdir $target, 0775;
    printf "Created $target directory (is mode correct?)\n";
}
$target .= "/GNU";
if ( ! -d $target ) {
    mkdir $target, 0775;
    printf "Created $target directory (is mode correct?)\n";
}
chdir($target) || die "Cannot cd to $target: $!\n";
$target = "libreadline.a";
if ( -f "libreadline.a" ) {
    printf "There's already a libreadline.a in LIBR/GNU; good.\n";
} else {
    if ( -f "/usr/lib/libreadline.a" ) {
        printf "Take a chance, will use your /usr/lib/libreadline.a\n";
        symlink("/usr/lib/libreadline.a", $target);
    } else {
                                        # See if UNSHR already here
        if ( -x "UNSHR.$arch" ) {
            printf "Ah, you've done this before.  UNSHR.$arch found!\n";
        } else {
            $target = $yserv . "UNSHR.FOR";
            symlink($target, "unshr.f");
            $target = $sysunix . "READLINE.SHR";
            symlink($target, "READLINE.SHR");
            printf "Compiling a version of UNSHR...";
            system("$f77 -O -o UNSHR.$arch unshr.f");
            if ( ! -x "UNSHR.$arch") {
                die \
                "$f77 -O -o UNSHR.$arch unshr.f failed in \$LIBR/GNU\n";
            }
        }
        printf "Done.\nUnpacking READLINE.SHR (ignore the prompt)...\n";
        system("echo ./READLINE.SHR | ./UNSHR.$arch");
        if ( ! -f "configure") {
            die "failed?  No configure file found in \$LIBR/GNU\n";
        }
        $target = "configure";
        chmod 0775, $target;
        printf "Done.\nAnd RLSUPP.SHR (again, ignore prompt)...";
        mkdir "support", 0775;
        chdir("support");
        $target = $sysunix . "RLSUPP.SHR";
        symlink($target, "RLSUPP.SHR");
        system("echo ./RLSUPP.SHR | ../UNSHR.$arch");
        if ( ! -f "config.guess") {
            die \
               "failed?  No config.guess found in \$LIBR/GNU/support\n";
        }
        @targets = ("config.guess", "config.sub");
        chmod 0775, @targets;
        chdir("..");
        printf "Done.\nNow to configure and make it...\n";
        system("./configure --quiet");
        system("make");
        if ( -f "libreadline.a") {
            printf "That produced a libreadline.a, assuming it's ok\n";
        } else {
            die "No libreadline.a produced, cannot proceed!\n";
        }
    }
}
printf "--------------- Make XAS (display server) --------------\n";
$target = $opt_a . "/" . $myvers . "/" . $arch . "/LOAD";
if ( ! -d $target ) {
    mkdir $target, 0775 || die "Cannot create $target: $!\n";
}
                                        # bug in makefile, work around.
system("touch $target/XAS");
$target .= "/XAS";
if ( -x $target ) {
    printf "XAS already exists in LOAD area, good.\n";
} else {
    $target = $yserv . "XAS";
    if ( ! -d $target ) {
        mkdir $target, 0775 || die "Cannot create $target: $!\n";
    }
    chdir($target);
    if ( -f "xas.h" ) {
        printf "You've been here before, XAS.SHR already unpacked.\n";
    } else {
        $target = $opt_a . "/" . $myvers . "/" . $arch
            . "/LIBR/GNU/UNSHR" . "." . $arch;
        if ( -x $target ) {
            $unshr = $target;
            printf "Using $target to unpack XAS.SHR\n";
        } else {
            $target = $yserv . "UNSHR.FOR";
            symlink($target, "unshr.f");
            printf "Compiling a version of UNSHR...";
            system("$f77 -O -o UNSHR.$arch unshr.f");
            if ( ! -x "UNSHR.$arch") {
                die \
               "$f77 -O -o UNSHR.$arch unshr.f failed in $yserv/XAS/\n";
            }
            $unshr = "./UNSHR.$arch";
        }
        printf "Unpacking XAS.SHR... (ignore the prompt)\n";
        system("echo ../XAS.SHR | $unshr");
        if ( ! -f "xas.h" ) {
            die "Unpacking failed?  No xas.h found.\n";
        }
    }
    printf "Now making.  Needs GNU make on some systems.\n";
    system("sh -c '. $opt_a/login.sh; make clean; make'");
    $target = $opt_a . "/" . $myvers . "/" . $arch . "/LOAD/XAS";
    if ( -x $target ) {
        printf "That produced an XAS in the LOAD area; good.\n";
    } else {
        die "No LOAD/XAS found, make failed!\n";
    }
}
                                        # set up symlinks for INSTEP2/4
$insdir = $opt_a . "/" . $myvers . "/" . $arch . "/INSTALL/";
if (! -d $insdir ) {
    mkdir $insdir, 0775 || die "Cannot find or create $insdir\n";
}
chdir($insdir);
unlink("INSTEP2");
$target = $sysunix . "INSTALL/INSTEP2";
symlink($target, "INSTEP2");
unlink("INSTEP4");
$target = $sysunix . "INSTALL/INSTEP4";
symlink($target, "INSTEP4");
$target = $opt_a . "/" . $myvers . "/" . $arch . "/PREP";
if ( ! -d $target ) {
    mkdir ($target, 0775) || die "Cannot create PREP area: $!\n";
}
printf "------------- set up midnight job -------------\n";
$target = $opt_a . "/" . $myvers . "/" . $arch . "/UPDATE";
if ( ! -d $target ) {
    printf "Creating Midnight Job area $target...\n";
    mkdir $target, 0775;
}
chdir($target);
if (( -x "UPDLSTDAT" ) && ( -x "UPDOBSLT" )) {
    printf "Midnight Job area $target seems OK.\n";
    printf "(found evidence of MAKE.MNJ being run...)\n";
} else {
    printf "About to set up midnight job...\n";
    printf "The begin date will be set to $begindate; this should be\n";
    printf "ONE day before the generation time of $tarball\n";
    printf "If you want a different date, enter it now (YYYYMMDD): ";
    $n = <STDIN>; chomp($n);
    if ($n) {
        if ($n !~ /^\d\d\d\d\d\d\d\d$/) {
            printf "Nope, can't do that ($n)\n";
            die "The format is YYYYMMDD (all numeric)\n";
        } else {
            printf "OK, will use $n as begin date.\n";
            $begindate = $n;
        }
    }
    $target = $sysunix . "/UPDATE/MAKE.MNJ " . $begindate;
    system("sh -c '. $opt_a/login.sh; $target'");
    if ( -f "LASTGOOD.DOC" ) {
        printf "That seemed to work (LASTGOOD.DOC found)\n";
    } else {
        printf "Not good; no LASTGOOD.DOC found; did MAKE.MNJ fail?\n";
        printf "You should investigate this.  <RETURN> to continue...";
        $dum = <STDIN>;
    }
}
printf "------------------------------------------------------------\n";
printf "Now you are ready to start INSTEP2.  Here's a checklist:\n";
printf "   1.\tSource the $opt_a/login.sh or login.csh file\n";
printf "   2.\tcd to $opt_a/$myvers/$arch/INSTALL\n";
printf "   3.\tStart INSTEP2 interactively or in the background,\n";
printf "\te.g. for bash, korn, bourne and zsh shells:\n";
printf "\t\t( ./INSTEP2 >/dev/null 2>>INSTEP2.ERR & )\n";
printf "   4.\tWhen INSTEP2 is done, start INSTEP4.\n";
printf "   5.\tChange the TST,NEW,OLD definitions in AIPSPATH.SH,\n";
printf "\tand AIPSPATH.CSH in $opt_a, and then type \$CDTST.\n";
printf "\tMake SURE you have the right version; you should see:\n";
printf "\t'AIPS_VERSION=$opt_a/$myvers'.\n";
printf "   6.\tGive the commands:\n\t\tRUN FILAIP\n";
printf "\t\tRUN POPSGN\n";
printf "\tfrom the Unix command line to generate the (vital!) memory\n";
printf "\tfiles.  For FILAIP, good numbers are:\n\t\t35 -100\n";
printf "\t\t8 2\n\t\t4\n\t(password AMANAGER).  That's MINUS 100.\n";
printf "\tThe POPSGN input when it asks 'Enter Idebug....' is\n";
printf "\t\t0 POPSDAT TST\n\tand one extra carriage return.";
printf "\tFILAIP may give some errors about files already existing;\n";
printf "\tyou can safely ignore these messages.\n";
printf "   7.\tEnable a cron job to run the do_daily script that is\n";
printf "\tcreated by MAKE.MNJ\n";
printf "   8.\tEat some banana flavoured Ice Cream.\n";
printf "------------------------------------------------------------\n";
                                        # this is the end.  Really.
