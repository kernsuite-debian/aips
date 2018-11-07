$! REMOVE.COM
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
$!-----------------------------------------------------------------------
$!   Remove all files that have been deleted since last update.
$!
$!   Current version deletes the text file and tries to delete the module
$!   from the link library when appropriate.
$!-----------------------------------------------------------------------
$ Write sys$output "*** REMOVE starting."
$ define/nolog sys$error remove.err
$ Write sys$output "*** Error output diverted to REMOVE.ERR"
$!
$ open/read remove remove.unq
$ on error then GOTO ERRORHANDLER
$ sonc := syntax_only,no_conceal
$!
$ READLOOP:
$   read/end_of_file=ENDREADLOOP remove line
$   line   = f$edit(line,"COMPRESS,TRIM")
$   subdir = f$element(4," ",line)
$   name   = f$element(5," ",line)
$   file   = subdir + ":" + name
$   type   = f$parse(file,,,"type")
$   if f$search(file) .nes. "" then goto FILE_FOUND
$     write updlogfile "Can't delete : ", file, " - not found."
$     GoTo READLOOP
$FILE_FOUND:
$!                                          remove from library
$   if ((type.nes.".FOR") .and. (type.nes.".MAR")) Then GOTO FILE_DEL
$     name = f$parse(file,,,"name",sonc)
$     Set Noon
$!
$       LIBR = "AIPS_PROC:LIBR.DAT"
$       OPEN/READ LIB_LIST 'LIBR'
$       OLB_LIST = ""
$       SRC_LIST = ""
$LIBR_LOOP:
$       READ/END_OF_FILE=END_LIBR_LOOP LIB_LIST TEMP
$       TEMP = F$EDIT(TEMP,"COMPRESS,UNCOMMENT,UPCASE")
$       IF TEMP.EQS."" THEN GOTO LIBR_LOOP
$       OLB = F$ELEMENT(0," ",TEMP)
$       SRC = F$ELEMENT(1," ",TEMP)
$       SHR = F$ELEMENT(2," ",TEMP)
$!
$! Note any object libraries to be updated
$!
$       IF F$LOCATE(AREA,SRC).EQ.F$LENGTH(SRC) THEN GOTO LIBR_LOOP
$       OLB_LIST = OLB_LIST + "!" + OLB
$       SRC_LIST = SRC_LIST + "!" + SRC
$!
$! Check that some libraries to be updated
$!
$END_LIBR_LOOP:
$       CLOSE LIB_LIST
$SKIP_LIBR:
$       IF OLB_LIST.NES."" THEN GOTO LIBR_OK
$       GOTO FILE_DEL
$LIBR_OK:
$!
$! Only replace routines that are at the lowest level in the directory
$!
$       COUNT = 0
$REPLACE_LOOP:
$       COUNT = COUNT + 1
$       OLB = F$ELEMENT(COUNT,"!",OLB_LIST)
$       IF OLB.EQS."!" THEN GOTO FILE_DEL
$       SRC = F$ELEMENT(COUNT,"!",SRC_LIST)
$       DEFINE/NOLOG SEARCH_PATH 'SRC'
$       TEMP = "SEARCH_PATH:" + name + ".*R"
$       TEMP2 = F$SEARCH(TEMP)
$       TEMP3 = ""
$       TEMP4 = ""
$       TEMP5 = ""
$       IF TEMP2.NES."" THEN TEMP3 = F$SEARCH(TEMP)
$       IF TEMP3.NES."" THEN TEMP4 = F$SEARCH(TEMP)
$       IF TEMP4.NES."" THEN TEMP5 = F$SEARCH(TEMP)
$       IF TEMP2.NES.FILE THEN GOTO REPLACE_LOOP
$!
$! Delete module in object libraries
$!
$       SET MESSAGE/NOFACILITY/NOSEVERITY/NOIDENTIFICATION
$       LIBRARY/delete=('name')/LOG LIBR:'OLB'
$       SET MESSAGE 'MESSAGE'
$       GOTO REPLACE_LOOP
$!
$FILE_DEL:
$     delete 'file';*
$     write updlogfile "Deleted      : ", file
$     GoTo READLOOP
$!
$RESET_ERROR:
$     Set On
$     on error then GOTO ERRORHANDLER
$     GoTo READLOOP
$!
$ERRORHANDLER:
$  write updlogfile "Could not delete : ", file
$  on error then GOTO ERRORHANDLER
$  GoTo READLOOP
$!
$ENDREADLOOP:
$ close remove
$!
$ copy aips_version:[update]lastgood.tmp aips_version:[update]lastremove.dat
$ write Sys$Output "Created new LastRemove.DAT "
$ write updlogfile "Created new LastRemove.DAT "
$!
$ write sys$output "*** REMOVE finished."
$!
$ EXIT
