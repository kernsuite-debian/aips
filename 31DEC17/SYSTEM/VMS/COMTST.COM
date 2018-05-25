$! COMTST - compile and link an aips task or program
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
$!      P2 on   List of options - see OPTIONS.COM for details
$!
$! File definitions needed by this procedure:
$!
$       OPTIONS = "@AIPS_PROC:OPTIONS.COM"
$       COMPILE = "@AIPS_PROC:COMPILE.COM"
$       TEMP = F$GETJPI("","PID")               ! Unique name
$       TEMP_LIST = "C" + TEMP + ".TMP"
$!       PPFILE = "C" + TEMP + ".FOR"
$       PP = "@AIPS_PROC:PP.COM"
$       OPT_AREA = "AIPS_PROC:"
$!
$! Parse options
$!
$       OPTIONS 'P2' 'P3' 'P4' 'P5' 'P6' 'P7' 'P8'
$!
$! Initialize symbols
$!
$       CURRENT_AREA = ""
$       MODE = 0
$       MESSAGE = F$ENVIRONMENT("MESSAGE")
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
$       IF F$LOCATE("*",ROUTINE).NES.TEMP THEN GOTO ALL
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
$! All routines in an area
$!
$ALL:
$       TEMP = F$LOCATE(":",ROUTINE)
$       IF TEMP.EQ.F$LENGTH(ROUTINE) THEN TEMP = 0
$       AREA = F$EXTRACT(0,TEMP,ROUTINE)
$       DIRECTORY/OUTPUT='TEMP_LIST'/NOHEADER/NOTRAILING/COLUMN=1 -
           /VERSIONS=1/EXCLUDE=(*.DIR,*.OLD)  'ROUTINE'
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
$       IF (AREA.EQS."") THEN AREA = "SYS$DISK"
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
$       WRITE SYS$OUTPUT "ERROR - File not found ", ROUTINE
$       GOTO ERROR
$FILE_OK:
$!
$! Check out options file
$!
$       OPTIONSFILE = ""
$       TEMP = F$SEARCH("''AREA':LOCAL.OPT")
$       IF (TEMP.NES."") THEN OPTIONSFILE := 'AREA':LOCAL.OPT/OPT
$       TEMP = F$SEARCH("''AREA':''NAME'.OPT")
$       IF (TEMP.NES."") THEN OPTIONSFILE := 'AREA':'NAME'.OPT/OPT
$       IF (OPTIONSFILE.NES."") THEN GOTO OPT_OK
$          WRITE SYS$OUTPUT "ERROR - No options file for this routine"
$          GOTO ERROR
$OPT_OK:
$!
$! Set current area as default for next file
$!
$       CURRENT_AREA = AREA
$!
$! See if compile is necessary.
$!
$       OBJECT := 'NAME'.OBJ
$       PPFILE := 'NAME'.F
$       OBJDATE0 = "01-JAN-1900 00:00:00.00"
$       TEMP = F$SEARCH(OBJECT)
$       IF (TEMP.NES."") THEN OBJDATE0 = F$FILE_ATTRIBUTES(OBJECT,"CDT")
$       SOURCEDATE = F$FILE_ATTRIBUTES(ROUTINE,"CDT")
$       SOURCEDATE = F$CVTIME(SOURCEDATE)
$       OBJDATE = F$CVTIME(OBJDATE0)
$       IF (SOURCEDATE.GES.OBJDATE) THEN GOTO COMPILE
$          WRITE SYS$OUTPUT "Using EXISTING OBJECT FILE dated ",OBJDATE0
$          GOTO LINK
$!
$! Compile or assemble routine
$!
$ COMPILE:
$       SET NOON
$       PP 'ROUTINE' 'PPFILE'
$       COMPILE 'ROUTINE' 'PPFILE' 'OBJECT' 'TYPE'
$       TEMP = $SEVERITY
$       IF TEMP.EQ.4 THEN GOTO ABORT
$       IF .NOT.TEMP THEN GOTO ERROR
$       SET ON
$!
$! Link module with appropriate object libraries
$!
$LINK:
$       WRITE SYS$OUTPUT "Linking ",NAME,".  Output in ",F$DIRECTORY()
$       ZZLINKEXE /EXEC='NAME'.EXE 'OBJECT', 'OPTIONSFILE'
$       SET MESSAGE/NOFACILITY/NOSEVERITY/NOIDENTIFICATION
$!                                 Make PURGE failures non fatal.
$       SET NOON
$       IF ZZPURGE THEN PURGE/LOG/NOCONFIRM 'NAME'.EXE
$       SET ON
$       ON WARNING THEN GOTO ERROR
$       ON CONTROL_Y THEN GOTO ABORT
$       SET MESSAGE 'MESSAGE'
$!
$! Purge object module
$!
$ENDLINKLOOP:
$       IF F$SEARCH(OBJECT).NES."" THEN PURGE/NOLOG/NOCONFIRM 'OBJECT'
$       IF F$SEARCH(PPFILE).NES."" THEN PURGE/NOLOG/NOCONFIRM 'PPFILE'
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
$END_P1_LOOP:
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
$       SET NOON
$       WRITE SYS$OUTPUT "Recovering from error"
$       SET MESSAGE 'MESSAGE'
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
$       EXIT %X1000002C
