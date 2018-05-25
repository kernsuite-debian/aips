$! Procedure to list AIPS disk usage
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
$! It will spawn a sub-process to do the work if no terminal output is required
$!
$	IF P1.EQS."GO" THEN GOTO GO
$	SPACE := @AIPS_PROC:SPACE GO
$	OUTFILNAM := SPACE.LIS
$	DATA_VERSION := "B"
$!
$! Command section - set up all default parameters
$!
$	DISK1 = 1
$	DISK2 = 3
$	USER1 = 1
$	USER2 = 4095
$	TIME  = 0
$	OUT  := F,P
$!
$! Ask for parameters
$!
$DISKS:
$	ON WARNING THEN GOTO DISKS
$  WRITE SYS$OUTPUT "***  Use comma, no blanks between pairs of numbers  ***"
$	INQUIRE ANS "Range of disks (default=''DISK1',''DISK2') "
$	IF ANS.EQS."" THEN GOTO USERS
$	LEN = F$LENGTH(ANS)
$	TEMP = F$LOCATE(",",ANS)
$	IF TEMP.EQ.LEN THEN GOTO SINGLE_DISK
$	TEMP1 = F$INTEGER(F$EXTRACT(0,TEMP,ANS))
$	TEMP2 = F$INTEGER(F$EXTRACT(TEMP+1,LEN-TEMP-1,ANS))
$	GOTO DISK_OK
$SINGLE_DISK:
$	TEMP1 = F$INTEGER(ANS)
$	TEMP2 = TEMP1
$DISK_OK:
$	DISK1 = TEMP1
$	DISK2 = TEMP2
$!
$USERS:
$	ON WARNING THEN GOTO USERS
$	INQUIRE ANS "Range of users (default=''USER1',''USER2') "
$	IF ANS.EQS."" THEN GOTO TIME
$	LEN = F$LENGTH(ANS)
$	TEMP = F$LOCATE(",",ANS)
$	IF TEMP.EQ.LEN THEN GOTO SINGLE_USER
$	TEMP1 = F$INTEGER(F$EXTRACT(0,TEMP,ANS))
$	TEMP2 = F$INTEGER(F$EXTRACT(TEMP+1,LEN-TEMP-1,ANS))
$	GOTO USER_OK
$SINGLE_USER:
$	TEMP1 = F$INTEGER(ANS)
$	TEMP2 = TEMP1
$USER_OK:
$	USER1 = TEMP1
$	USER2 = TEMP2
$!
$TIME:
$	ON WARNING THEN GOTO TIME
$	INQUIRE ANS -
	"Only list users inactive for how may days (default=''TIME') "
$	IF ANS.EQS."" THEN GOTO OUT
$	ANS = F$INTEGER(ANS)
$	TIME = ANS
$!
$OUT:
$	ON WARNING THEN GOTO OUT
$	INQUIRE ANS -
	"Output to terminal (T), file (F) or printer (P) (default=''OUT') "
$	IF ANS.EQS."" THEN GOTO CONT
$	OUT := 'ANS'
$!
$! Call this procedure again to do the work
$!
$CONT:
$	IF F$LOCATE("T",OUT).LT.F$LENGTH(OUT) THEN GOTO IMMEDIATE
$	SPAWN/NOWAIT/NOTIFY/PROCESS=SYSYPHUS -
	SPACE 'DISK1' 'DISK2' 'USER1' 'USER2' 'TIME' 'OUT'
$	EXIT
$IMMEDIATE:
$	SPACE 'DISK1' 'DISK2' 'USER1' 'USER2' 'TIME' 'OUT'
$	EXIT
$!
$! This is the section of the procedure that really does the work
$!
$GO:
$	DISK1 = F$INTEGER(P2)
$	DISK2 = F$INTEGER(P3)
$	USER1 = F$INTEGER(P4)
$	USER2 = F$INTEGER(P5)
$	TIME  = ""
$	IF P6.NE.0 THEN TIME = "/MODIFIED /BEFORE=-''P6'-"
$	OUT  := 'P7'
$	LEN   = F$LENGTH(OUT)
$	TERM  = 0
$	IF F$LOCATE("T",OUT).LT.LEN THEN TERM = 1
$	FILE  = 0
$	IF F$LOCATE("F",OUT).LT.LEN THEN FILE = 1
$	PRIN  = 0
$	IF F$LOCATE("P",OUT).LT.LEN THEN PRIN = 1
$	IF PRIN THEN FILE = 1
$	VERSION = F$EXTRACT(1,1,F$GETSYI("VERSION"))
$!
$! Output files
$!
$	ON WARNING THEN GOTO CLEAN_UP
$	ON CONTROL_Y THEN GOTO CLEAN_UP
$	OUT1 = "!"
$	IF TERM THEN OUT1 := WRITE SYS$OUTPUT
$	OUT2 = "!"
$	IF .NOT.FILE THEN GOTO BEGIN
$	OUT2 := WRITE OUTFIL
$	CREATE/FDL=SYS$INPUT: 'OUTFILNAM'
RECORD
	CARRIAGE_CONTROL	carriage_return
$	OUTFILE = F$SEARCH(OUTFILNAM)
$	OPEN/READ/WRITE OUTFIL 'OUTFILE'
$!
$! Loop through requested disks
$!
$BEGIN:
$	DISK = DISK1
$DISK_LOOP:
$	AREA = "DA0''DISK'"
$!
$! Find each catalogue file
$!
$	TEMP = "*"
$	IF USER1.EQ.USER2 THEN TEMP = F$EXTRACT(1,3,F$FAO("!XW",USER1))
$	ON WARNING THEN GOTO NEXT_DISK
$	DIRECTORY 'TIME' /DATE=MODIFIED /COLUMNS=1 /NOHEADING /NOTRAILING -
	/OUTPUT=CAT.TMP 'AREA':CA'DATA_VERSION'00000.'TEMP';1
$	ON WARNING THEN GOTO CLEAN_UP
$	OPEN/READ CAT CAT.TMP
$	OUT1 ""
$	OUT2 ""
$	LINE = "AIPS disk usage on "
$	TEMP = F$LOGICAL("SYS$NODE")
$	IF TEMP.NES."" THEN LINE = LINE + "node " + TEMP + " "
$	LINE = LINE + "disk #" + F$STRING(DISK) + " at " + F$TIME()
$	OUT1 LINE
$	OUT2 LINE
$	OUT1 ""
$	OUT2 ""
$	GRAND_TOTAL = 0
$!
$! Extract user number
$!
$THE_START:
$	READ/END_OF_FILE=THE_END CAT LINE
$	HEX = F$EXTRACT(F$LOCATE(";",LINE)-3, 3, LINE)
$	DEC = %X'HEX'
$!
$! Extract the catalogue file modification date
$!
$	IF VERSION.EQS."4" THEN GOTO V4
$	DATE := 'F$EXTRACT(F$LOCATE("-",LINE)-2, 20, LINE)
$	GOTO USAGE
$V4:
$	READ/END_OF_FILE=THE_MIDDLE CAT DATE
$	DATE := 'DATE'
$!
$! Find total usage for each user
$!
$USAGE:
$	IF DEC.LT.USER1 .OR. DEC.GT.USER2 THEN GOTO THE_START
$	TOTAL_USE = 0
$	DIRECTORY /SIZE /TOTAL /NOHEADING -
	/OUTPUT=USE.TMP 'AREA':*.'HEX';*
$	OPEN/READ USE USE.TMP
$	READ/END_OF_FILE=THE_MIDDLE USE LINE
$	READ/END_OF_FILE=THE_MIDDLE USE LINE
$	COMMA = F$LOCATE(",",LINE) + 1
$	LEN = F$LOCATE("B",LINE) - 7 - COMMA
$	TEMP = F$INTEGER(F$EXTRACT(COMMA,LEN,LINE))
$	TOTAL_USE = TOTAL_USE + TEMP
$	GRAND_TOTAL = GRAND_TOTAL + TEMP
$!
$! Results
$!
$THE_MIDDLE:
$	CLOSE USE
$	DELETE/NOLOG/NOCONFIRM USE.TMP;*
$	OUT1 "User : ",DEC," Last access : ",DATE," Total usage = ",TOTAL_USE
$	OUT2 "User : ",DEC," Last access : ",DATE," Total usage = ",TOTAL_USE
$	GOTO THE_START
$!
$! Grand totals
$!
$THE_END:
$	CLOSE CAT
$	DELETE/NOLOG/NOCONFIRM CAT.TMP;*
$	OUT1 ""
$	OUT2 ""
$	OUT1 "Grand total of AIPS files on disk #",DISK," = ",GRAND_TOTAL
$	OUT2 "Grand total of AIPS files on disk #",DISK," = ",GRAND_TOTAL
$	OUT1 ""
$	OUT2 ""
$!
$! Loop if more disks
$!
$NEXT_DISK:
$	DISK = DISK + 1
$	IF DISK.LE.DISK2 THEN GOTO DISK_LOOP
$!
$! Finish off
$!
$	IF .NOT.FILE THEN EXIT
$	CLOSE OUTFIL
$	TEMP = OUTFILNAM + ";-5"
$	IF F$SEARCH(TEMP).NES."" THEN PURGE/KEEP=3/NOLOG/NOCONFIRM 'OUTFILNAM'
$	IF PRIN THEN PRINT/NOIDENTIFY 'OUTFILNAM'
$	EXIT
$!
$! Clean up
$!
$CLEAN_UP:
$	SET NOON
$	CLOSE USE
$	CLOSE CAT
$	CLOSE OUTFIL
$	DELETE/NOLOG/NOCONFIRM USE.TMP;*,CAT.TMP;*
