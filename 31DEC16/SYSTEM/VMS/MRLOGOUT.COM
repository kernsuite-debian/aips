$! LOGOUT.COM - logout procedure for MAIL-REMINDER
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
$ Where = f$Environment("Default")
$ Set Default 'LOCAL_MAIL_DIRECTORY'
$ Set protection='MAIL_PROTECTION' *.mai;*,mail$*.*;*
$ PURGE
$ Set Default 'Where'
$ if f$getjpi("", "PrcCnt") .ne. 0 then goto SUBS
$ LO*gout := LOGOUT        ! avoid calling this procedure repeatedly!
$!
$! LOGOUT                  ! Change to "$ LOGOUT" at end of installation
$!
$SUBS:
$ write sys$Output "*** You still have sub-processes active! ***"
$ EXIT
$!
$! End of LOGOUT.COM
