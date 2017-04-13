#!/usr/local/bin/perl5 -T
#-----------------------------------------------------------------------
#;  Copyright (C) 1999-2001
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
# Purpose:  Pseudo-daemon to serve out AIPS text files to midnight jobs.
# --------  This script is intended to be run from secure shell commands
# issued from clients of the form:
#
#    ssh -C -q -k -a -x -i ~/.ssh/mnj mnj.cv.nrao.edu -l aipsmgr aipfile
#
# where "aipfile" is, e.g. /AIPS/15OCT99/HELP/AIPS.HLP or perhaps
# /aips1/master/TST/UPDATE/PUTBCK.UPD.  Options used in the above are:
#
#   -C  to force compression
#   -q  run quietly
#   -k  disables forwarding of kerberos tickets
#   -a  disables forwarding of the authentication agent connection
#   -x  disables X11 forwarding/tunneling
#   -i  specify the identity file to be used
#   -l  specify the account on the target host
#
# The entry in the server's ~/.ssh/authorized_keys file should be:
#
# command="/wherever/UPDSSHD.PL",no-port-forwarding, no-X11-forwarding,
#          no-agent-forwarding,no-pty 1024 37 ... NRAO-MNJ
#
# The actual entry will of course have the full key in place of ... and
# will be on a single line.
#
# How it works:  Any argument, such as "aipfile" in the ssh example
# -------------  above, is passed to this script via ssh in the
# SSH_ORIGINAL_COMMAND environment variable.  This is sanitized:
#
#      - substrings of two or more dots (../../..../../.....) are
#        compressed into a single dot each (././././.)
#      - Any characters NOT in the set A-Z, a-z, 0-9, -, _, ., and /
#        are deleted.
#
# Then it is compared to a set of allowed directory trees:
#
#   /AIPS/
#   /home/aips1/master/TST/
#   /home/aips1/master/NEW/
#
# and finally, if the file exists, it is printed on stdout.  It is the
# responsibility of the client to capture the output and save it to a
# local file.
#
# If there is a second argument and it matches "MODE", the numerical
# protection mode of the file will be returned instead.
#
# If there is no argument or the argument becomes null after sanitizing,
# the server returns with no output and success status (0).
#
# If there is a violation or the file is not found, an error message is
# printed to stderr, nothing is sent to stdout and the server exits with
# a failed status (1).  The exact same error message is used in all
# cases for security reasons.
#
# Logging added 1999.09.13
#-----------------------------------------------------------------------
require 'ctime.pl';
                                        # Get the original cmd/args:
$arg  = substr((split(' ', $ENV{'SSH_ORIGINAL_COMMAND'}))[0], 0, 80);
                                        # Sanitize it: no ../..:
$arg =~ s/\.{2,}/./g;
                                        # and only legit chars in names:
$arg =~ s/[^A-Za-z0-9\/\.\-\_]//g;
if (!$arg) {
                                        # kicking the tires.
    exit 0;
}
                                        # second argument: MODE
$arg2 = substr((split(' ', $ENV{'SSH_ORIGINAL_COMMAND'}))[1], 0, 4);
$arg2 =~ s/[^A-Za-z0-9]//g;
$arg2 = uc($arg2);
                                        # Allowed hierarchies:
@allowed = ("/AIPS/", "/home/aips1/master/TST/",
            "/home/aips1/master/NEW/", "/home/kochab_1/AIPS/");
                                        # Logging
$log = "/AIPS/aipsmgr/mnj.log";
                                        # Configuration - log level etc.
$config = "/AIPS/aipsmgr/mnj.config";
if ( ! -f $config ) {
                                        # default values go here
    $loglevel = 1;
    $enable = 1;
    $hostnames = 0;
    @baddies = ();
} else {
                                        # suck in the configuration
    require $config;
}
if ( ! -f $log ) {
    open(LOG, ">$log") || die "Failed to create log file $log: $!\n";
} else {
    open(LOG, ">>$log") || die "Failed to open log file $log: $!\n";
}
$now = &ctime(time); chomp($now);
				        # Validate against these:
$ok = 0;
foreach $tree (@allowed) {
    if ($arg =~ /^$tree/) {
	$ok++;
	last;
    }
}
$remip = substr((split(' ', $ENV{'SSH_CLIENT'}))[0], 0, 50);
if ($#baddies >= 0) {
    foreach $baddie (@baddies) {
        if ($remip =~ /^$baddie/) {
            printf STDERR ("File not found or allowed: %s\n", $arg);
            printf LOG ("$now: %s <- %s BADHOST\n", $remip, $arg);
            close(LOG);
                                        # make DOS attacks harder
            sleep(60);
            exit(1);
        }
    }
}
if ($hostnames == 1) {
                                        # Do we look up hostnames?
    ($remhost, $aliases, $type, $len, $addrs) =
        gethostbyaddr(pack ('C4', split(/\./, $remip)),2); # AF_INET
    if ($remhost) {
        $remip = $remhost;
    }
}
                                        # is it an allowed hierarchy?
if ($ok == 0) {
                                        # No it's not; Go away, heathen!
    printf STDERR ("File not found or allowed: %s\n", $arg);
    printf LOG "$now: %s <- %s DENIED\n", $remip, $arg;
    close (LOG);
                                        # make DOS attacks harder
    sleep(60);
    exit 1;
}
if ( -r $arg ) {
                                        # We can read the file
    if ($arg2 =~ /MODE/) {
                                        # get everything in case we
                                        # decide to extend this more.
	($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,
	 $ctime,$blksize,$blocks) = stat($arg);
                                        # Make sure to only get the
                                        # significant parts of the mask
	$mode = $mode & 01777;
	printf("%o\n", $mode);
    } else {
	open(FILE,"<$arg");
	while (<FILE>) {
	    print;
	}
	close(FILE);
                                        # Log the transfer?
        if ($loglevel > 0) {
                                        # yes, at some level.
            if (($loglevel > 1) || ($arg =~ /\/aips1\/master/)) {
                printf LOG "$now: %s <- %s COPIED\n", $remip, $arg;
            }
        }
        close(LOG);
    }
} else {
                                        # We can't read the file.
				        # Make this the SAME error msg
                                        # as the other errors.
    printf STDERR ("File not found or allowed: %s\n", $arg);
    printf LOG "$now: %s <- %s NOT-FOUND\n", $remip, $arg;
    close (LOG);
    exit 1;
}
                                        # if we get here, it worked.
exit 0;
