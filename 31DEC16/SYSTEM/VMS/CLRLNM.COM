$! CLRLNM - clear access to all AIPS logical names
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
$! This procedure works in two modes:
$!   1. If system wide logical name tables exist, LNM$FILE_DEV is
$!      redefined so as not to reference them.
$!   2. If the AIPS logical names are defined in the JOB table, ALL
$!      supervisor mode logical names are deleted from this table.
$!
$!
$! Does the table exist
$!
$       IF F$TRNLNM("AIPS_USER", "LNM$DIRECTORIES",,,, "TABLE") THEN GOTO EXISTS
$!
$! Delete all supervisor mode logical names in the JOB table
$!
$       DEASSIGN/JOB/SUPERVISOR_MODE/ALL
$       EXIT
$!
$! AIPS logical name tables exist.
$! Find existing LNM$FILE_DEV
$!
$EXISTS:
$       MAX = F$TRNLNM("LNM$FILE_DEV","LNM$DIRECTORIES",,,,"MAX_INDEX")
$       COUNT = -1
$       FULNAM = ""
$LOOP_1:
$       COUNT = COUNT + 1
$       NAME = F$TRNLNM("LNM$FILE_DEV","LNM$DIRECTORIES",COUNT)
$       IF F$EXTRACT(0,5,NAME).EQS."AIPS_" THEN GOTO LOOP_1
$       FULNAM = FULNAM + "," + NAME
$       IF COUNT.LT.MAX THEN GOTO LOOP_1
$!
$! Redefine LNM$FILE_DEV
$!
$       FULNAM = FULNAM - ","
$       DEFINE/TABLE=LNM$PROCESS_DIRECTORY/NOLOG LNM$FILE_DEV 'FULNAM'
