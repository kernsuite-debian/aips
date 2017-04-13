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
$! LOGIN - This is a login procedure for the AIPS account.  ASSNSTART.COM
$! should be kept in the same directory in case AIPS_STARTUP is not
$! defined.
$!
$! Define AIPS_STARTUP if necessary
$!
$ IF F$TRNLNM("AIPS_STARTUP",,,,,).NES."" THEN GOTO OK
$    AREA = F$ENVIRONMENT("PROCEDURE")
$    AREA = F$PARSE(AREA,,,"DEVICE",) + F$PARSE(AREA,,,"DIRECTORY",)
$    @'AREA'ASSNSTART "DEFINE"
$!
$ OK:
$ @AIPS_STARTUP:AIPSUSER  ! Define necessary logicals and commands.
$ @AIPS_STARTUP:RUNAIPS      ! Startup AIPS in some fashion.
