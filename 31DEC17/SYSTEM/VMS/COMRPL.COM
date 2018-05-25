$! COMRPL - compile subroutine and update libraries
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
$! Inputs:
$!      P1      "area:routine" for a single routine
$!              or "area1:routine1,area2:routine2" for a list
$!              or "@list" for a file containing a list
$!              or "area:*" for all routines in specified area
$!              or can use * and % as VMS wild card characters
$!      P2 on   List of options - see OPTIONS.COM for details
$!
$! File definitions needed by this procedure:
$!
$       OPTIONS= "@AIPS_PROC:OPTIONS.COM"
$       COMPILE = "@AIPS_PROC:COMPILE.COM"
$       TEMP = F$GETJPI("","PID")               ! Unique name
$       TEMP_LIST = "C" + TEMP + ".TMP"
$       OBJECT = "C" + TEMP + ".OBJ"
$       PPFILE = "C" + TEMP + ".FOR"
$       PP = "@AIPS_PROC:PP.COM"
$       LIBR = "AIPS_PROC:LIBR.DAT"
$       BUILDSHR = "@AIPS_PROC:BUILDSHR.COM"
$!
$! Parse options
$!
$       OPTIONS 'P2' 'P3' 'P4' 'P5' 'P6' 'P7' 'P8'
$!
$! Initialize symbols
$!
$       ERRCOUNT = 0
$       CURRENT_AREA = ""
$       MODE = 0
$       MESSAGE = F$ENVIRONMENT("MESSAGE")
$       OLB_LIST = ""
$       SHR_LIST = ""
$       SET ON
$!
$! Error handlers
$!
$       ON WARNING THEN GOTO ERROR
$       ON CONTROL_Y THEN GOTO ABORT
$!
$! Loop through any list specified in P1
$!
$       P1_COUNT = -1
$P1_LOOP:
$ROUTINE_LOOP_0:
$ROUTINE_LOOP_1:
$       P1_COUNT = P1_COUNT + 1
$       ROUTINE = F$ELEMENT(P1_COUNT,",",P1)
$       IF ROUTINE.EQS."," THEN GOTO END_P1_LOOP
$       TEMP = F$LENGTH(ROUTINE)
$       IF F$LOCATE("@",ROUTINE).NES.TEMP THEN GOTO LIST
$       IF F$LOCATE("*",ROUTINE).NES.TEMP THEN GOTO WILD
$       IF F$LOCATE("%",ROUTINE).NES.TEMP THEN GOTO WILD
$!
$! Routine name directly in P1
$!
$       MODE = 1
$       GOTO START_WORK
$!
$! Routine name list in a file
$!
$LIST:
$       ROUTINE = ROUTINE - "@"
$       OPEN/READ ROUTINE_LIST 'ROUTINE'
$       MODE = 2
$       GOTO ROUTINE_LOOP_2
$!
$! Wildcard search of an area
$!
$WILD:
$       TEMP = f$locate(":",ROUTINE)
$       if TEMP.eq.f$length(ROUTINE) then TEMP = 0
$       AREA = f$extract(0,TEMP,ROUTINE)
$       IF AREA.EQS."" THEN GOTO AREA_ERROR
$       IF f$trnlnm(AREA).EQS."" THEN GOTO AREA_ERROR
$       CHOICE = ROUTINE
$       TYPE = f$parse(ROUTINE,,,"type","syntax_only")
$       if (TYPE.eqs.".") then CHOICE = ROUTINE + ".FOR," + ROUTINE + ".MAR"
$       DIRECTORY/OUTPUT='TEMP_LIST'/NOHEADER/NOTRAILING/COLUMN=1 -
           /VERSIONS=1/EXCLUDE=(*.DIR,*.OLD)  'CHOICE'
$       OPEN/READ ROUTINE_LIST 'TEMP_LIST'
$       MODE = 3
$!
$! List of routines in file
$!
$ROUTINE_LOOP_2:
$ROUTINE_LOOP_3:
$       READ/END_OF_FILE=END_ROUTINE_LOOP ROUTINE_LIST ROUTINE
$       ROUTINE = F$EDIT(ROUTINE,"COMPRESS,UNCOMMENT,UPCASE")
$       IF ROUTINE.EQS."" THEN GOTO ROUTINE_LOOP_2
$!
$! We now have a routine name - check out area
$!
$START_WORK:
$       IF MODE.EQ.3 THEN GOTO AREA_OK
$       TEMP = F$LOCATE(":",ROUTINE)
$       IF TEMP.EQ.F$LENGTH(ROUTINE) THEN TEMP = 0
$       AREA = F$EXTRACT(0,TEMP,ROUTINE)
$       IF AREA.EQS."" THEN AREA = CURRENT_AREA
$       IF F$TRNLNM(AREA).EQS."" THEN GOTO AREA_ERROR
$AREA_OK:
$!
$! Check that source file exists - either .FOR or .MAR
$!
$       NAME = F$PARSE(ROUTINE,,,"NAME","SYNTAX_ONLY")
$       TYPE = F$PARSE(ROUTINE,".FOR;",,"TYPE","SYNTAX_ONLY")
$       XROUTINE = AREA + ":" + NAME + TYPE
$       FILE = F$SEARCH(XROUTINE)
$       IF FILE.NES."" THEN ROUTINE = XROUTINE
$       IF FILE.NES."" THEN GOTO FILE_OK
$       TYPE = F$PARSE(ROUTINE,".MAR;",,"TYPE","SYNTAX_ONLY")
$       XROUTINE = AREA + ":" + NAME + TYPE
$       FILE = F$SEARCH(XROUTINE)
$       IF FILE.NES."" THEN ROUTINE = XROUTINE
$       IF FILE.NES."" THEN GOTO FILE_OK
$       WRITE SYS$OUTPUT "File not found ", ROUTINE
$       GOTO ERROR
$FILE_OK:
$!
$! If the area has not changed we can skip the scan of LIBR
$!
$       IF (AREA.EQS.CURRENT_AREA) .AND. (OLB_LIST.NES."") THEN GOTO LIBR_OK
$!
$! Scan each entry in LIBR
$!
$       WRITE SYS$OUTPUT "Switching to area ", AREA
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
$! Note any object libraries or shareable images to be updated
$!
$       IF F$LOCATE(AREA,SRC).EQ.F$LENGTH(SRC) THEN GOTO LIBR_LOOP
$       OLB_LIST = OLB_LIST + "!" + OLB
$       SRC_LIST = SRC_LIST + "!" + SRC
$       COUNT = -1
$SHR_LOOP:
$       COUNT = COUNT + 1
$       TEMP = F$ELEMENT(COUNT,",",SHR)
$       IF TEMP.EQS."," THEN GOTO LIBR_LOOP
$       IF F$LOCATE(TEMP,SHR_LIST).EQ.F$LENGTH(SHR_LIST) THEN -
                        SHR_LIST = SHR_LIST + "," + TEMP
$       GOTO SHR_LOOP
$!
$! Check that some libraries to be updated
$!
$END_LIBR_LOOP:
$       CLOSE LIB_LIST
$       CURRENT_AREA = AREA
$SKIP_LIBR:
$       IF OLB_LIST.NES."" THEN GOTO LIBR_OK
$       WRITE SYS$OUTPUT "Area ", AREA, -
                " is not listed in the file ", LIBR
$       GOTO ROUTINE_LOOP_'MODE'
$LIBR_OK:
$!
$! Compile or assemble routine
$!
$       SET NOON
$       PP 'ROUTINE' 'PPFILE'
$       COMPILE 'ROUTINE' 'PPFILE' 'OBJECT' 'TYPE'
$       TEMP = $SEVERITY
$       IF TEMP.EQ.4 THEN GOTO ABORT
$       IF .NOT.TEMP THEN GOTO ERROR
$       SET ON
$!
$! Only replace routines that are at the lowest level in the directory
$!
$       COUNT = 0
$       USED = "FALSE"
$REPLACE_LOOP:
$       COUNT = COUNT + 1
$       OLB = F$ELEMENT(COUNT,"!",OLB_LIST)
$       IF OLB.EQS."!" THEN GOTO END_REPLACE_LOOP
$       SRC = F$ELEMENT(COUNT,"!",SRC_LIST)
$       DEFINE/NOLOG SEARCH_PATH 'SRC'
$       TEMP = "SEARCH_PATH:" + NAME + ".*R"
$       TEMP2 = F$SEARCH(TEMP)
$       TEMP3 = ""
$       TEMP4 = ""
$       TEMP5 = ""
$       IF TEMP2.NES."" THEN TEMP3 = F$SEARCH(TEMP)
$       IF TEMP3.NES."" THEN TEMP4 = F$SEARCH(TEMP)
$       IF TEMP4.NES."" THEN TEMP5 = F$SEARCH(TEMP)
$       IF TEMP2.NES.FILE THEN GOTO REPLACE_LOOP
$!
$! Replace module in object libraries
$!
$       USED = "TRUE"
$       SET MESSAGE/NOFACILITY/NOSEVERITY/NOIDENTIFICATION
$       LIBRARY/REPLACE/LOG LIBR:'OLB' 'OBJECT'
$       SET MESSAGE 'MESSAGE'
$       GOTO REPLACE_LOOP
$!
$! Delete object module
$!
$END_REPLACE_LOOP:
$       IF F$SEARCH(PPFILE).NES."" THEN DELETE/NOLOG/NOCONFIRM 'PPFILE';*
$       IF F$SEARCH(OBJECT).NES."" THEN DELETE/NOLOG/NOCONFIRM 'OBJECT';*
$       IF .NOT.USED THEN WRITE SYS$OUTPUT "Routine ", ROUTINE, -
                " is not needed in any object libraries for this system"
$!
$! Get next routine
$!
$       GOTO ROUTINE_LOOP_'MODE'
$!
$! Finished with routine list
$!
$END_ROUTINE_LOOP:
$       CLOSE ROUTINE_LIST
$       IF MODE.EQS.3 THEN DELETE/NOLOG/NOCONFIRM 'TEMP_LIST';*
$       MODE = 0
$       GOTO P1_LOOP
$!
$! Build appropriate shareable images
$!
$END_P1_LOOP:
$       ON WARNING THEN GOTO EXIT
$       ON CONTROL_Y THEN GOTO EXIT
$       IF SHR_LIST.NES."" THEN GOTO SHR_OK
$       WRITE SYS$OUTPUT "No shareable images relinked"
$       GOTO EXIT
$SHR_OK:
$       SHR_LIST = SHR_LIST - ","
$       BUILDSHR 'SHR_LIST'
$!
$! Normal exit
$!
$EXIT:
$       IF F$TRNLNM("LOGFILE").NES."" THEN CLOSE LOGFILE
$       IF (ERRCOUNT.NE.0) THEN EXIT  %X1000002C
$       EXIT
$!
$! Error in area
$!
$AREA_ERROR:
$       WRITE SYS$OUTPUT "Invalid area specified in ",ROUTINE
$!
$! Error recovery
$!
$ERROR:
$       ERRCOUNT = ERRCOUNT + 1
$       SET NOON
$       WRITE SYS$OUTPUT "Recovering from error"
$       SET MESSAGE 'MESSAGE'
$       IF F$TRNLNM("LIB_LIST").NES."" THEN CLOSE LIB_LIST
$       IF F$SEARCH(OBJECT).NES."" THEN DELETE/NOLOG/NOCONFIRM 'OBJECT';*
$       IF F$SEARCH(PPFILE).NES."" THEN DELETE/NOLOG/NOCONFIRM 'PPFILE';*
$       SET ON
$       ON WARNING THEN GOTO ERROR
$       ON CONTROL_Y THEN GOTO ABORT
$       GOTO ROUTINE_LOOP_'MODE'
$!
$! Abort exit
$!
$ABORT:
$       SET NOON
$       WRITE SYS$OUTPUT "Cleaning up from abort"
$       SET MESSAGE 'MESSAGE'
$       IF F$TRNLNM("ROUTINE_LIST").NES."" THEN CLOSE ROUTINE_LIST
$       IF MODE.EQ.3 THEN IF F$SEARCH(TEMP_LIST).NES."" THEN -
                        DELETE/NOLOG/NOCONFIRM 'TEMP_LIST';*
$       IF F$TRNLNM("LIB_LIST").NES."" THEN CLOSE LIB_LIST
$       IF F$SEARCH(OBJECT).NES."" THEN DELETE/NOLOG/NOCONFIRM 'OBJECT';*
$       IF F$SEARCH(PPFILE).NES."" THEN DELETE/NOLOG/NOCONFIRM 'PPFILE';*
$       IF F$TRNLNM("LOGFILE").NES."" THEN CLOSE LOGFILE
$       EXIT %X1000002C
