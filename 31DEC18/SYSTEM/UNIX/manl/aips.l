.\"---------------------------------------------------------------------
.\";  Copyright (C) 1995, 1998, 2000, 2007, 2010
.\";  Associated Universities, Inc. Washington DC, USA.
.\";
.\";  This program is free software; you can redistribute it and/or
.\";  modify it under the terms of the GNU General Public License as
.\";  published by the Free Software Foundation; either version 2 of
.\";  the License, or (at your option) any later version.
.\";
.\";  This program is distributed in the hope that it will be useful,
.\";  but WITHOUT ANY WARRANTY; without even the implied warranty of
.\";  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
.\";  GNU General Public License for more details.
.\";
.\";  You should have received a copy of the GNU General Public
.\";  License along with this program; if not, write to the Free
.\";  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
.\";  MA 02139, USA.
.\";
.\";  Correspondence concerning AIPS should be addressed as follows:
.\";         Internet email: aipsmail@nrao.edu.
.\";         Postal address: AIPS Project Office
.\";                         National Radio Astronomy Observatory
.\";                         520 Edgemont Road
.\";                         Charlottesville, VA 22903-2475 USA
.\"---------------------------------------------------------------------
.TH AIPS LOCAL "September 23, 2010"
.UC
.SH NAME
AIPS \- Astronomical Image Processing System
.SH SYNOPSIS
\fBaips\fR [\fBOLD\fR, \fBNEW\fR, or \fBTST\fR]
     [\fBTV=\fR[\fBdisp\fR][\fB:\fR][\fBhost\fR]]
  or [\fBTV=local\fR[\fB:n\fR]]
  or [\fBNOTV\fR]
     [\fBTVOK\fR]
     [\fBDA=host\fR[\fB,host\fR,...]]
  or [\fBDA=default\fR]
  or [\fBDA=all\fR]
     [\fBTP=tphost\fR[\fB,tphost\fR,...]]
  or [\fBTPOK\fR]
     [\fBpr=\fR#]
     [\fBREMOTE\fR or \fBREM\fR or \fBTEK\fR]
     [\fBDEBUG\fR[\fB=prog\fR][\fB:aips\fR]]
     [\fBLOCAL\fR] [\fBNORL\fR] [\fBNOEX\fR]

.SH DESCRIPTION
The \fBaips\fR command starts up the AIPS command interpreter and
associated AIPS server processes.

.SH OPTIONS
All command line options are case insensitive.

AIPS allows up to three versions to co-exist (disk space permitting) in
one installation.  They are identified either by date (e.g. 15OCT98) or
name (OLD, NEW, or TST).  On most installations, these will all be the
same.

.IP old 8
Start the OLD version of AIPS.  For NRAO this is a frozen version which
has been distributed worldwide.
.IP new 8
Start the NEW version of AIPS.  For NRAO this is the most recently
released version and is frozen right at the time of initial public
release.
.IP tst 8
Start the TST version of AIPS.  For NRAO this is the unreleased,
development version.  This is the default.

.IP \fRTV=[\fBtvdisp\fR][\fB:\fR][\fBtvhost\fR]\ or\ TV=\fBlocal\fR[\fB:n\fR] 8
TV display server to use instead of the default.  The AIPS startup
script \fBtries\fR to deduce which host the user is sitting in front of
(this may not work; it is often difficult or impossible to determine
this information).  This may not be the same as the machine on which
AIPS is to be run if, for example, the user has remotely logged in to
another machine within a terminal emulator window.

The "TV=local" option allows use of Unix based sockets for the TV and
other servers.  If you choose this option, you MUST run the XAS server
and any AIPS sessions that will use it on the same host, though the
DISPLAYs can be the same or different.  Also, no remote AIPS sessions
will be able to talk to this local TV.

If you instead use "TV=local:0", it will attempt to start a new instance
of the TV and ancillary servers.  This can be used to have multiple TVs
on the same host, and is useful in a compute server environment with X
terminals.  If you have multiple Unix-socket based TVs already started,
you can choose which one a new AIPS session will use by,
e.g. "TV=local:2" to choose the second one.

NOTE: The default TV behavior is to use INET or Internet based sockets,
as the scripts have been doing since 1992.  The "local" Unix socket
based functionality does not change this.

For the default use of internet sockets, the full syntax of the TV=
option is \fBTV=\fRtvdisp\fB:\fRtvhost, where \fBtvhost\fR is the name
of the machine on which the TV display server (usually XAS), Tektronix
graphics server (TEKSRV), message server (MSGSRV), and TV Lock server
(TVSERV) are to run, and \fBtvdisp\fR indicates the machine to which the
DISPLAY environment variable should point for XAS.  Do NOT specify
TV=hostname:0.0!  Both TVHOST and TVDISP can be different from the
machine that AIPS itself is running on.  See the section on X Window
System servers below for more information on how to control the servers.

The default behavior of this option if only \fIone\fR of tvdisp and
tvhost is specified is
.RS 8
.IP TV=tvhost 12
tvdisp defaults to tvhost.
.IP TV=tvdisp: 12
tvhost defaults to the host AIPS is running on.
.IP TV=:tvhost 12
tvdisp defaults to the host AIPS is running on.
.RE

.RS 8
For the remote TV options to work, you must be able to use the \fIrsh\fR
or \fIremsh\fR command; see the notes on it under the \fBtp=\fR heading
below.  Also see the notes on environment variable AIPSREMOTE.  By
default, if you do not specify any \fBtv=\fR option, you will only get a
TV if your current \fITERM\fR environment variable matches sun*,
*xterm*, *hpterm, dtterm, or iris*.  The \fBDISPLAY\fR environment
variable is used if set, otherwise the \fBwho am i\fR (on HP-UX, with
the -R option) is used to make a guess at "where" you really are.
.RE

.IP NOTV 8
Prevents automatic activation of the TV servers if no display is wanted.
This option also disables the Tektronix graphics server, the message
server and the TV lock server.  See the section on X Window System
servers below for information on how to control the Tektronix and
message servers.

.IP TVOK 8
Assume that the TV display servers are already running; the particulars
(display, host) are still worked out -- from the TV=... argument (see
above) if necessary -- but no servers will be started.

.IP DA=\fBhost\fR[\fB,host\fR,...]\ or\ DA=\fBdefault\fR\ or\ DA=\fBall\fR 8
Select user data areas (directories, or "disks" in AIPSpeak) that are
local to the (comma separated) list of machines.  Data areas from
"required" hosts and those on the local machine are always added,
regardless of the list of hosts.

All disks from each named host will be assigned.  Use the FREE command
within AIPS to see the disk assignments you end up with.  They are also
shown on startup.

AIPS has a limit of 35 disks in any one session.  The limit on the
number of disks that can be defined for any given site is 512.  Disk 1
is special in that it stores the AIPS message and save/get files.  The
system is designed so that one particular required disk will almost
always be assigned as disk 1.  For performance reasons, this may be
undesirable if the filesystem in question is mounted via NFS.  See the
description of personal \fI.dadevs\fR files below, as it can be used to
customize the list of possible user data areas.

Selecting DA=ALL will try to include every area defined in the startup
file, up to the session limit.  Bear in mind that most AIPS tasks only
have 10 slots for "BADDISK".  Selecting DA=DEFAULT will completely
bypass the configurable data areas and choose only those data areas
preconfigured by the AIPS manager; THIS IS NOT NORMALLY ENABLED, CHECK
WITH YOUR AIPS MANAGER BEFORE USING DA=DEFAULT.

There is a hierarchy of data area "lists" that AIPS will look for on
startup.  These are:
.RS 8
.IP $HOME/.dadevs 20
This would be in your private login area (what $HOME points to).  It
need not exist.  If it doesn't, AIPS looks for the next file:
.IP $DA00/DADEVS.LIST 20
This is a host-specific file possibly set up by the AIPS manager.  If it
doesn't exist, AIPS finally looks for:
.IP $NET0/DADEVS.LIST 20
which is the site-wide data area configuration file.
.RE

.RS 8
The normal state of affairs is to have just one place for disks to be
defined, namely \fB$NET0/DADEVS.LIST\fR.  Your AIPS manager can choose
to install host-specific list files, and you can choose (if you run AIPS
from your own private account) to override both of these two with your
own private version.  This allows for considerable flexibility but moves
the onus of maintenance of these files to the user.  In other words, if
you have your own .dadevs file, \fByou\fR have to keep track of your
site's disk configuration!

If your AIPS installation supports multiple sites, e.g. to support both
little-endian (Intel, Alpha) and big-endian (Sparc) systems, you can
have any of these files refer to one or the othe site by appending the
site name, e.g. $HOME/.dadevs.VCOARN for SITE=VCOARN.

The format for these files is all the same: a list of directory names
preceded by a "+" for required or a "-" for optional.  There should be
two (2) spaces between the "+" or "-" (in the leftmost column) and the
directory name.

In addition to all of the above, you may define a list of data areas
in an optional \fB$HOME/.dadevs.always\fR file.  This is used in
addition to whichever of the DADEVS files have been selected by the
rules above.  The data areas that you will be assigned start with any
required data areas in the \fB$HOME/.dadevs.always\fR file followed by
any required data areas in the selected DADEVS file regardless of name.
The 3rd group of data areas are those optional ones in the selected
DADEVS file containing a string matching the user's host name.  Then
come the optional data areas in the \fB$HOME/.dadevs.always\fR file
regardless of name.  Finally, the optional data areas in the selected
DADEVS file with strings matching the names of any hosts given in the
DA=\fBhost\fR[\fB,host\fR,...] command-line option.  The order of data
areas within each group, i.e. which is disk 1, etc., is determined by
the order in the files.

There is also a \fB$NET0/NETSP\fR file that is maintained by the AIPS
manager and controls \fBTIMDEST\fR and aips user-number access to the
disks.  You will get error messages if your private .dadevs file
includes AIPS data areas ("disks") that are not in the \fBNETSP\fR file.
Regardless of the number of sites in your installation, there is only
one NETSP file.
.RE

.IP TP=\fBhost\fR[\fB,host\fR,...] 8
Make sure tape daemons (TPMON) are running on the comma separated list
of machines.  While the AIPS account is usually set up so that it can
perform remote shell (rsh or remsh) commands, your personal account may
not.  Check with your system administrator or network guru for details.
Also check the Unix manual pages on \fIrsh\fR (\fIremsh\fR on HP-UX),
\fIrhosts\fR, and \fIhosts.equiv\fR.  The \fBtp=\fR option uses
\fIrsh\fR to issue commands to remote hosts.

.IP TPOK 8
Do NOT check or launch the TPMON tape daemons on the local host.  The
default is to check if they are running and to launch them if not found.

.IP pr=# 8
Select printer number (e.g., pr=2).  If this option is not specified,
the user will be presented with a menu of available printers and
prompted to enter a choice.  If there is only one printer configured,
no menu will be presented.  You may change the selected printer within
AIPS via the PRINTER adverb.

.IP REMOTE\ or\ REM\ or\ TEK 8
Any one of these indicates that the user is running from a terminal with
Tektronix display capability.  Graphics output will be sent directly to
this terminal.  NOTE: AIPS will \fBnot\fR switch from text to graphics
mode on terminals with a separate graphics "screen".

.IP DEBUG[\fB=prog\fR][\fB:aips\fR] 8
Start AIPS in debug mode.  With no arguments, the user will be prompted
for the name of the debugger (e.g. gdb, dbx, adb, csd, xde, dbxtool,
debugger, xxgdb) and also whether to run AIPS itself under the
debugger. If you answer no, only AIPS tasks will be run in debug mode.
If \fB=prog\fR is specified, this suppresses the prompt for the name of the
debugger program.  If \fB:aips\fR is specified, this suppresses the prompt
for whether to run AIPS itself in debug mode and assumes it will.  Use of
both these options is useful in speeding up the startup of the system when
debugging a program or AIPS itself.

.IP LOCAL 8
Start a local copy of AIPS.EXE residing in the current directory.
Usually used by programmers for debugging purposes.

.IP NORL 8
Disable GNU readline library and command-line editing.  This is
primarily useful for running backgrounded AIPS sessions, running AIPS
from "here-document" shell-scripts, and for debugging.

.IP NOEX 8
This defers AIPS execution and is not normally used directly by users.

.SH COMMAND-LINE EDITING (NEW)

AIPS now utilizes, under most UNIX and UNIX-like operating systems, the
GNU "readline" library for user input.

This library gives the user the ability to use the cursor-arrow keys, as
well as various "control" and "escape" key-sequences, to recall
previously-entered commands, edit the current command-line (without
having to back-space and re-type the entire line), search the command
history for previously-executed commands, define customized key-bindings
for executing commands and macros, etc.

The default key-bindings should be very familiar to users of Emacs
and/or the bash shell; many of them should also be recognizable to users
of the Korn and tcsh shells.  Hard-core vi users can put AIPS into
"vi-mode" and use vi-like key-bindings instead.

The user's command-line history is automatically saved between sessions
(unique to both the user and the "AIPS number" of the session) and then
recovered at the next AIPS startup.

Within AIPS, "HELP READLINE" will give a short summary of this
functionality.  Outside of AIPS, "man readline" will probably give you
more information about this functionality than you would ever want to
know...

This feature can be disabled with the \fBnorl\fR command-line option.

.SH RUNFILES

There are two areas where AIPS "run files" are scanned for: $RUNSYS
and $RUNFIL.  The former is for system files (AIPS user number 1) and
the latter is for individual's own run files (any user number).  You
can override the latter if you have the environment variable RUNFIL
already defined (e.g., in you .login or .profile).

.SH X WINDOW SYSTEM SERVERS

If you are running under the X Window System, AIPS will open up to three
windows: a TV window (normally XAS), a message window (MSGSRV) and a
graphics window (TEKSRV).  If you specify the notv option on the command
line, none of these will be started.

MSGSRV and TEKSRV are actually simple programs running inside a terminal
emulator.  You may use any terminal emulator that you would normally use
on the machine on which you are running AIPS for the MSGSRV window.
Examples include xterm (the sample vt100/Tektronix emulator that comes
with the MIT X Window System code); cmdtool and shelltool (the standard
terminal emulators for OpenWindows) and AIXterm (the standard terminal
emulator on RS/6000s).  You can choose which one to use by setting the
environment variable AIPS_MSG_EMULATOR to the name of the terminal
emulator you wish to use.  For example, if you want to use cmdtool you
would type

setenv AIPS_MSG_EMULATOR cmdtool

if you use the C or TC Shell, or

AIPS_MSG_EMULATOR=cmdtool; export AIPS_MSG_EMULATOR

if you use Korn, BASH, or Bourne shells before you start up AIPS.  You
could also add these commands to your .login file (C Shell) or .profile
(Korn/BASH/Bourne Shells) to make the assignment more permanent.  You
can also give AIPS_MSG_EMULATOR the special value of "none" which will
disable the message window without affecting the Tektronix window or the
TV.  If AIPS_MSG_EMULATOR is not set, the default is xterm.

You may choose the terminal emulator used for the Tektronix window using
the environment variable AIPS_TEK_EMULATOR in the same way that you use
AIPS_MSG_EMULATOR to choose the terminal emulator, but it \fBmust\fR
support Tektronix graphics codes.  On most machines the only values of
AIPS_TEK_EMULATOR that make any sense are xterm and none.  If
AIPS_TEK_EMULATOR is not set AIPS will behave as if it were set to
xterm.  (Note: dxterm, aixterm, and cmdtool are not "xterm"; they cannot
display tek graphics).

You can set preferences for positions and colors for all three
servers using the standard X Window System mechanisms.  Further
information is available through the AIPS HELP system (subjects
MSGSRV, TEKSRV, XAS and XVSS).

Note that AIPS expects that a terminal emulator can start a
program that is specified using a \fB-e\fR flag on the command
line.  This is true of all of the terminal emulators we know
about but if you find one that requires a different flag you
can specify the flag as AIPS_TEK_EXE_FLAG or AIPS_MSG_EXE_FLAG.

.SH ENVIRONMENT\ VARIABLES

In addition to the Message and Tek server customizations, you may choose
to set a variable AIPSREMOTE to indicate your choice of remote shell
command.  It is strongly recommended that the secure shell (ssh) be used
in place of the traditional Berkeley rsh or remsh command:

.RS
setenv AIPSREMOTE "ssh -n"
.RE

for csh or tcsh shells, or

.RS
export AIPSREMOTE="ssh -n"
.RE

for bash, korn, zsh and other bourne-like shells.

.SH NOTES
Further help is available after AIPS starts via the HELP command.
Within AIPS, "HELP TASKS" lists a one-line summary of each task.  Also,
there is an APROPOS verb for listing tasks, verbs, procedures and
pseudoverbs that have a specified keyword in their one-line descriptions
(e.g. APROPOS GAUSSIAN to find out about things that deal with Gaussian
functions).

.SH AUTHORS (Approximately chronological)
.IP NRAO: 8
Eric Greisen, Bill Cotton, Gary Fickling, David Brown, Ed Fomalont, Fred
Schwab, Don Wells, Kerry Hilldrup, Tim Cornwell, Pat Moore, Arnold Rots,
Alan Bridle, Phil Diamond, Pat Murphy, Brian Glendenning, Bill Junor,
Chris Flatters, Dean Schlemmer, Doug Wood, Gareth Hunt, Glen Langston,
Dave Adler, Leonid Kogan, Juan Uson, Gustaaf van Moorsel, Jeff
Uphoff...
.IP ATNF: 8
Mike Kesteven, Mark Calabretta, Neil Killeen, Henrietta May.
.IP Leiden: 8
Walter Jaffe.
.BR

And plenty more.

.SH FILES

The following is a superficial list of the more important setup files.
All these files are located relative to the $AIPS_ROOT directory.

.IP LOGIN.CSH 18
File to call from your .login if your login shell is the C shell.
.IP LOGIN.SH
File to call from your .profile if your login shell is sh/ksh/bash.
.IP HOSTS.LIST
List of hosts that can run AIPS.  Also defines which are TV servers and
"X terminals".
.IP START_AIPS
The script that actually starts AIPS.  Usually pointed to by a symbolic
link $SYSLOCAL/aips, but can be /usr/local/bin/AIPS and/or
/usr/local/bin/aips.
.IP DA00/PRDEVS.LIST
Defines printers for your site.
.IP DA00/TPDEVS.LIST
Defines magtape devices.
.IP DA00/DADEVS.LIST
List of aips "disks" or data areas; see description above.
.IP DA00/NETSP
TIMDEST and user reservation parameters for disks.
.IP TVDEVS.SH
Defines hardwired AIPS TV devices, if any.
.IP AIPSASSN.[C]SH
Contains reserved and (hardwired) message terminal definitions (at the
end), if any.
.IP AIPS.MSG
Login notice, to be edited by the local AIPS manager.