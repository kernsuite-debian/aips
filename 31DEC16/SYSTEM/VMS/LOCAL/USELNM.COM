$! USELNM - Access a logical name table
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
$! P1 = logical name table to use.
$!
$! This procedure works in three modes:
$!   1. If the logical name table does not exist and the process has
$!      suitable privilege, a system wide logical name table is created
$!      and a reference to it included in LNM$FILE_DEV.
$!      This mode typically occurs at system boot time.
$!   2. If the logical name table does not exist and the process has no
$!      privilege the logical names are defined in the JOB table. This
$!      table should already be referenced in LNM$FILE_DEV.
$!      This mode typically occurs at sites where AIPS has been installed
$!      by a non-privileged user.
$!   3. If the logical name table already exists, LNM$FILE_DEV is simply
$!      modified to reference it.
$!      This mode typically occurs when AIPS has been installed and booted
$!      by a privileged user.
$!
$! The reason for this complexity is to reduce the overheads involved in
$! process spawning.
$!
$!
$! Check table name
$!
$       IF P1.EQS."USER" .OR. P1.EQS."PROG" THEN GOTO OK
$       WRITE SYS$OUTPUT "Invalid logical name table"
$       EXIT 42
$OK:
$!
$! Does the table exist
$!
$       TABLE = "AIPS_" + P1
$       IF F$TRNLNM(TABLE, "LNM$DIRECTORIES",,,, "TABLE") THEN GOTO EXISTS
$!
$! Does the process have privilege to create a system wide table
$!
$       IF F$PRIVILEGE("SYSPRV") THEN GOTO PRIV
$!
$! Use the JOB table
$!
$       DEF := DEFINE/JOB
$       RETURN := EXIT
$       GOTO 'TABLE'
$!
$! Create a system wide table
$!
$PRIV:
$       CREATE/NAME_TABLE/PROTECTION=(S:RWED,O:RWED,G:RWE,W:RWE) -
                /PARENT_TABLE=LNM$SYSTEM_DIRECTORY 'TABLE'
$       DEF := DEFINE/TABLE='TABLE'
$       RETURN := GOTO EXISTS
$       GOTO 'TABLE'
$!
$! User table
$!
$AIPS_USER:
$       @AIPS_STARTUP:ASSNBASIC 'DEF'
$       @AIPS_STARTUP:ASSNLOCAL 'DEF'
$       RETURN
$!
$! Programmer table
$!
$AIPS_PROG:
$       @AIPS_STARTUP:ASSNPROG 'DEF'
$       RETURN
$!
$! Finally reference the table in LNM$FILE_DEV
$! Find existing LNM$FILE_DEV
$!
$EXISTS:
$       MAX = F$TRNLNM("LNM$FILE_DEV","LNM$DIRECTORIES",,,,"MAX_INDEX")
$       COUNT = 0
$LOOP_1:
$       NAME = F$TRNLNM("LNM$FILE_DEV","LNM$DIRECTORIES",COUNT)
$       IF NAME.EQS.TABLE THEN EXIT
$       IF F$LOCATE("LNM$GROUP",NAME).NES.F$LENGTH(NAME) THEN -
                        POSITION = COUNT
$       NAME_'COUNT' = NAME
$       COUNT = COUNT + 1
$       IF COUNT.LE.MAX THEN GOTO LOOP_1
$!
$! Redefine LNM$FILE_DEV
$!
$       COUNT = 0
$       NAME = ""
$LOOP_2:
$       IF COUNT.EQ.POSITION THEN NAME = NAME + "," + TABLE
$       NAME = NAME + "," + NAME_'COUNT'
$       COUNT = COUNT + 1
$       IF COUNT.LE.MAX THEN GOTO LOOP_2
$       NAME = NAME - ","
$       DEFINE/TABLE=LNM$PROCESS_DIRECTORY/NOLOG LNM$FILE_DEV 'NAME'
