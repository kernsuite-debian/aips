$! IREADTAP.COM
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
$!--------------------------------------------------------------------------
$! Read AIPS from tape drive TAPE.
$!--------------------------------------------------------------------------
$!
$ NewDir = f$logical("SYS$DISK") + f$directory() - "]" + "." + VERDATE
$ Write sys$output "Reading AIPS from tape"
$    backup /norewind                                      -
            /select=([AIPS.'VERDATE'...]*.*;)             -
          TAPE:AIPS.BCK    'NewDir'...]/NEW_VER
$ Write sys$output "You may wish to delete:"
$ Write sys$output "    [AIPS.",VERDATE,".APL.DEV.UNIX...]*.*"
$ Write sys$output "    [AIPS.",VERDATE,".SYSTEM.UNIX...]*.*"
$ EXIT
