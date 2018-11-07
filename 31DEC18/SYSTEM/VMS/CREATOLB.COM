$! CREATOLB - create empty object libraries ready for a complete rebuild
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
$! Input:
$!      P1      Object library name
$!              If blank create all libraries
$!
$! Single library
$!
$       IF P1.EQS."" THEN GOTO ALL
$       LIBRARY/CREATE LIBR:'P1'
$       EXIT
$!
$! All libraries
$!
$ALL:
$       LIBR = "AIPS_PROC:LIBR.DAT"
$       ON ERROR THEN GOTO END
$       ON CONTROL_Y THEN GOTO END
$       OPEN/READ LIBR_LIST 'LIBR'
$LOOP:
$       READ/END_OF_FILE=END LIBR_LIST TEMP
$       TEMP = F$EDIT(TEMP,"COMPRESS,UNCOMMENT,UPCASE")
$       IF TEMP.EQS."" THEN GOTO LOOP
$       TEMP = F$ELEMENT(0," ",TEMP)
$       LIBRARY/CREATE LIBR:'TEMP'
$       GOTO LOOP
$END:
$       CLOSE LIBR_LIST
