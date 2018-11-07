$! BOOTUP - Initialization and cleanup procedure for AIPS that can be
$! run at boot time.
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
$! Initialize logical name tables.
$!
$       Set NoOn
$       @AIPS_STARTUP:USELNM USER
$       @AIPS_STARTUP:USELNM PROG
$!
$! Set liberal protections
$!
$       SET PROTECTION=(S:RWED,O:RWED,G:RWE,W:RWE) /DEFAULT
$!
$! Complete startup asynchronously in AIPS account.
$!
$	SUBMIT /log_file=aips_version:[update]aipsinit.log -
           /USER=AIPS -
           /NOPRINTER -
           AIPS_STARTUP:AIPSINIT
$!
$EXIT
