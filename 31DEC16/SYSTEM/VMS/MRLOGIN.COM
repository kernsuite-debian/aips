$! LOGIN.COM file for MAIL-REMINDER utility captive account
$!----------------------------------------------------------------------
$!;  Copyright (C) 1995
$!;  Associated Universities, Inc. Washington DC, USA.
$!;
$!;  This program is free software; you can redistribute it and/or
$!;  modify it under the terms of the GNU General Public License as
$!;  published by the Free Software Foundation; either version 2 of
$!;  the License, or (at your option) any later version.
$!;
$!;  This program is distributed in the hope that it will be useful,
$!;  but WITHOUT ANY WARRANTY; without even the implied warranty of
$!;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
$!;  GNU General Public License for more details.
$!;
$!;  You should have received a copy of the GNU General Public
$!;  License along with this program; if not, write to the Free
$!;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
$!;  MA 02139, USA.
$!;
$!;  Correspondence concerning AIPS should be addressed as follows:
$!;         Internet email: aipsmail@nrao.edu.
$!;         Postal address: AIPS Project Office
$!;                         National Radio Astronomy Observatory
$!;                         520 Edgemont Road
$!;                         Charlottesville, VA 22903-2475 USA
$!----------------------------------------------------------------------
$!
$! Customize by modifying the next three command lines:
$!
$ LOCAL_MAIL_DIRECTORY == "DISK$RES:[DBANANAS.MAIL]"  ! Keep mail files here
$ MAIL_PROTECTION      == "(GROUP:RW,WORLD:RW)"       ! Who can access them
$ SET_TERMINAL_COMMAND == "Set Terminal /Inquire /Line_Editing" ! DEC terminals
$!
$! End of user-customizable section.
$!
$ On ERROR     then goto ENDIT
$ On CONTROL_Y then goto ENDIF
$ If f$Mode() .eqs. "BATCH" then       goto BATCH
$ If f$Mode() .nes. "INTERACTIVE" then goto ENDIT  ! Don't allow network access
$ Set NoOn
$ 'SET_TERMINAL_COMMAND'
$!
$ On ERROR then goto ENDIT
$!
$! Type out the login notice, if any.
$!
$ If f$Search("MRNotice.txt") .nes. "" then Type MRNotice.txt
$!
$! See if a privileged user is remoted in.  You should replace the names
$! below with suitable usernames that you trust (like your own).  If you
$! don't have DECNET (try SET HOST 0; if it doesn't work, you don't),
$! you will have to remove these lines down to but not including the
$! line "$CAPTIVE:".
$!
$ Remote_user = f$trnlnm("SYS$REM_ID", "LNM$JOB")
$ If Remote_user .eqs. "" then goto CAPTIVE
$   If Remote_user .eqs. "FRODO"   then goto EXIT
$   If Remote_user .eqs. "BILBO"   then goto EXIT
$   If Remote_user .eqs. "GANDALF" then goto EXIT
$   If Remote_user .eqs. "ARAGON"  then goto EXIT
$!
$CAPTIVE:
$ Define/User_Mode Sys$Input Sys$Command
$ Mail
$ Set Protection='MAIL_PROTECTION' 'LOCAL_MAIL_DIRECTORY'*.mai;*,mail$*.*;*
$ Purge
$ENDIT:
$ @Sys$Login:logout
$BATCH:
$!
$! Come here for batch jobs, i.e. maintenance, daemons, etc.
$!
$ Set NoOn
$ Set Protection='MAIL_PROTECTION' 'LOCAL_MAIL_DIRECTORY'*.mai;*,mail$*.*;*
$ Exit
$EXIT:
$ Set Control=(T,Y)
$ Write Sys$Output "You are a privileged user; do what you will."
$ Lo*gout :== @Sys$Login:logout
$ Exit
$!
$! End of LOGIN.COM
