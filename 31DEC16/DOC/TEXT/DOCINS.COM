$!DOCINS.COM
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
$!----------------------------------------------------------------------
$! Make a set of instructions for the installation procedure.
$!----------------------------------------------------------------------
$                         ! To spinwriter
$ SPIN MV2C1002.             ! Introduction
$ SPIN MV2C1008.             ! Worksheet
$ SPIN U15NOV82.            ! Update instructions for various releases.
$ SPIN U15JAN83.
$ SPIN U15MAR83.
$ SPIN U15MAY83.
$ SPIN U15JUL83.
$ SPIN U15SEP83.
$ SPIN U15NOV83.
$ SPIN U15JAN84.
$                         ! To VERSATEC
$ PRINT /HEADER MV2C1003.    ! AIPS environment on VAX.
$ PRINT /HEADER MV2C1004.    ! Installation instructions
$ PRINT /HEADER MV2C1005.    ! Shortcut installation procedure.
$ PRINT /HEADER MV2C1006.    ! Trouble shooting & Programming
$ PRINT /HEADER MV2C1007.    ! Listings of installation procedures.
$ EXIT
 
