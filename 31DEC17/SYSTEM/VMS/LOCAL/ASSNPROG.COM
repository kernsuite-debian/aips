$! ASSNPROG - set up assignments for AIPS programmers
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
$! The logical names may be defined in various tables depending upon
$! how this procedure is invoked. P1 is a suitable DEFINE command and
$! any DEFINE's must be derived from This.
$!
$! P1 = a suitable DEFINE command to be used
$!
$! The DEFINE command
$!
$       DEF := 'P1'/NOLOG
$!
$! The list of areas is in the file AREAS.DAT
$!
$       OPEN/READ AREAS AIPS_PROC:AREAS.DAT
$       ON ERROR THEN GOTO END
$       ON CONTROL_Y THEN GOTO END
$LOOP:
$       READ/END_OF_FILE=END AREAS LINE
$       IF F$EXTRACT(0,1,LINE).EQS."!" THEN GOTO LOOP
$!       IF F$EXTRACT(0,7,LINE).EQS."SYSVMS " THEN GOTO LOOP
$!       IF F$EXTRACT(0,9,LINE).EQS."SYSLOCAL " THEN GOTO LOOP
$       PART1 = F$EXTRACT(0, F$LOCATE(" ", LINE), LINE)
$       PART2 = LINE - PART1
$       PART2 := 'PART2'
$       DEF 'PART1' AIPS_VERSION:['PART2']
$       GOTO LOOP
$END:
$       CLOSE AREAS
$!
$! Search path for include files
$!
$       DEF INCS INCVMS,INCNOT,INC
$!
$! Search path for update files
$!
$       DEF UPD_PROC UPDVLA,UPDVMS
