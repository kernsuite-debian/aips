LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NPARMS
      PARAMETER (NPARMS=22)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                      1         2          3        4         5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INEXT',
C            6         7          8          9     10      11
     *   'INVERS', 'DODELAY', 'TIMERANG', 'BIF', 'EIF', 'ANTENNAS',
C            12        13          14         15        16        17
     *   'FREQID', 'SUBARRAY', 'FLAGVER', 'SOLINT', 'DETIME', 'DOTWO',
C            18        19         20        21        22
     *   'EXPERT', 'CROWDED', 'DO3COL', 'ANTUSE', 'BADDISK'/
C                    1       2       3       4       5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR,
C          6       7       8       9       10      11
     *   OOAINT, OOAint, OOARE, OOAINT, OOAINT, OOAINT,
C          12      13      14      15     16     17
     *   OOAINT, OOAINT, OOAINT, OOARE, OOARE, OOALOG,
C          18      19      20      21      22
     *   OOALOG, OOALOG, OOARE,  OOAINT, OOAINT/
C                   1     2     3     4     5
      DATA AVDIM /12,1,  6,1,  1,1,  1,1,  2,1,
C         6     7     8     9     10    11
     *   1,1,  1,1,  8,1,  1,1,  1,1,  50,1,
C         12    13    14    15    16    17
     *   1,1,  1,1,  1,1,  1,1,  1,1,  1,1,
C         18    19     20   21    22
     *   1,1,  1,1,  1,1, 50,1, 10,1/
LOCAL END
      PROGRAM SNEDT
C-----------------------------------------------------------------------
C! SN/CL table editing with the TV
C# Task AP OOP UV EDITING TV-APPL CALIBRATION
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2006, 2010, 2012, 2015
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   SN/CL table data editing
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UVDATA*32, TBEDIT*32, INEXT*2
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'SNEDT'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL SNEDIN (PRGM, UVDATA, TBEDIT, INEXT, IRET)
C                                       CLEAN
      IF (IRET.EQ.0) CALL SNEDIT (UVDATA, TBEDIT, INEXT, IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE SNEDIN (PRGN, UVDATA, TBEDIT, INEXT, IERR)
C-----------------------------------------------------------------------
C   SNEDIN gets input parameters for SNEDT, creates the EDIT object.,
C   and copies the input table to the output.
C   Inputs:
C      PRGN     C*6    Program name
C   Output:
C      UVDATA   C*32   Name of input uv data. - as master
C      TBEDIT   C*32   Name of table object (output) to be edited
C      INEXT    C*2    Type of extension file
C      IERR     I      Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER PRGN*6, UVDATA*(*), TBEDIT*(*), INEXT*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1, NKEY2
C                                       NKEY1 = # adverbs FOR UVDATA
      PARAMETER (NKEY1=5)
C                                       NKEY2 = # adverbs for UVIN
      PARAMETER (NKEY2=19)
      INTEGER   DIM(7), TYPE, I, DUMMY, BIF, EIF, NIF, DDELAY
      LOGICAL   DODELY, MULTIB
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, CDUMMY*1, TBTEMP*32
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to UVDATA object
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'FLAGVER'/
C                                       Rename
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'IN_FGVER'/
C                                       Adverbs for TBEDIT object
      DATA INK2 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INEXT',
     *   'INVERS', 'TIMERANG', 'BIF', 'EIF', 'ANTENNAS', 'FREQID',
     *   'SOLINT', 'DETIME', 'SUBARRAY', 'DOTWO', 'EXPERT', 'ANTUSE',
     *   'CROWDED', 'DO3COL'/
C                                       Rename
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'TBLTYPE', 'VER',
     *   'TIMERANG', 'BIF', 'EIF', 'ANTENNAS', 'FRQSEL', 'EQU_TIME',
     *   'GAP_TIME', 'SUBARR', 'COMPARE', 'DOEXPERT', 'ANTS2USE',
     *   'CROWDED', 'DO3COLOR'/
C-----------------------------------------------------------------------
C                                       Startup interactive
      CALL AV2INT (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IERR)
      IF (IERR.NE.0) GO TO 999
      RQUICK = .FALSE.
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Extension type
      CALL OGET ('Input', 'INEXT', TYPE, DIM, DUMMY, INEXT, IERR)
      IF (IERR.NE.0) GO TO 999
      IF ((INEXT.NE.'CL') .AND. (INEXT.NE.'TY') .AND.
     *   (INEXT.NE.'SY')) INEXT = 'SN'
      CALL OPUT ('Input', 'INEXT', TYPE, DIM, DUMMY, INEXT, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'DODELAY', TYPE, DIM, DDELAY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (DDELAY.GT.1.5) THEN
         MULTIB = .TRUE.
         DODELY = .FALSE.
      ELSE
         MULTIB = .FALSE.
         DODELY = DDELAY.GT.0.0
         END IF
      IF ((INEXT.NE.'SN') .AND. (INEXT.NE.'CL')) THEN
         MULTIB = .FALSE.
         DODELY = .FALSE.
         END IF
C                                       Create UV master object
      UVDATA = 'UVDATA master input object'
      CALL CREATE (UVDATA, 'UVDATA', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, UVDATA, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open UVDATA to be sure it's OK
      CALL OOPEN (UVDATA, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OCLOSE (UVDATA, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create TBEDIT object
      TBEDIT = 'Output SN/CL/TY/SY table to be edited'
      CALL CREATE (TBEDIT, 'TABLE', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, TBEDIT, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      DIM(2) = 1
      I = 0
      CALL OPUT (TBEDIT, 'VER', OOAINT, DIM, I, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (TBEDIT, 'DODELAY', OOALOG, DIM, DODELY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (TBEDIT, 'MULTIBAND', OOALOG, DIM, MULTIB, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create TBTEMP object
      TBTEMP = 'Temp input file to be copied'
      CALL CREATE (TBTEMP, 'TABLE', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, TBTEMP, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open TBTEMP to be sure it's OK.
      CALL OOPEN (TBTEMP, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (TBTEMP, 'KEY.NO_IF', TYPE, DIM, NIF, CDUMMY, IERR)
      IF ((IERR.NE.0) .OR. (NIF.LE.0)) NIF = 1
      CALL OCLOSE (TBTEMP, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       IF numbers
C                                       Default BIF, EIF
      CALL OGET ('Input', 'BIF', TYPE, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'EIF', TYPE, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      BIF = MAX (1, BIF)
      BIF = MIN (BIF, NIF)
      IF (EIF.LT.BIF) EIF = NIF
      EIF = MIN (EIF, NIF)
C                                       Save in Inputs for history
      DIM(1) = 1
      CALL OPUT ('Input', 'BIF', OOAINT, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT ('Input', 'EIF', OOAINT, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (TBEDIT, 'BIF', OOAINT, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (TBEDIT, 'EIF', OOAINT, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       copy input table to output
      CALL TBLCOP (TBTEMP, TBEDIT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'SNEDIN: TROUBLE COPYING INPUT TO OUTPUT TABLE'
         CALL MSGWRT (8)
C                                       remove object (leave input
C                                       table untouched)
      ELSE
         CALL TABDES (TBTEMP, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE SNEDIT (UVDATA, TBEDIT, INEXT, IERR)
C-----------------------------------------------------------------------
C   Does the editing
C   Inputs:
C      UVDATA   C*32   Name of input uv data. - as master
C      TBEDIT   C*32   Name of output table data
c      INEXT    c*2    Input extension type
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER UVDATA*(*), TBEDIT*(*), INEXT*(*)
C
      CHARACTER UVEDIT*32, STATUS*4, TVDEVC*32, CDUMMY*1
      INTEGER   JERR, DUMMY, DIM(7)
      LOGICAL   DOFLAG
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA DOFLAG /.FALSE./
C-----------------------------------------------------------------------
C                                       open TV object
      TVDEVC = 'SNEDT task TV object'
      CALL TVDCRE (TVDEVC, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TVDOPN (TVDEVC, STATUS, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       open edit object
      UVEDIT = 'SNEDT task EDIT object'
      CALL EDICRE (UVEDIT, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL EDIOPN (UVEDIT, STATUS, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       Insert object info
      DIM(1) = LEN(UVDATA)
      DIM(2) = 1
      CALL EDIPUT (UVEDIT, 'UVMASTER', OOACAR, DIM, DUMMY, UVDATA, IERR)
      IF (IERR.NE.0) GO TO 980
      DIM(1) = LEN(TBEDIT)
      CALL EDIPUT (UVEDIT, 'TBEDIT', OOACAR, DIM, DUMMY, TBEDIT, IERR)
      IF (IERR.NE.0) GO TO 980
      DIM(1) = LEN(TVDEVC)
      CALL EDIPUT (UVEDIT, 'TVDEVICE', OOACAR, DIM, DUMMY, TVDEVC, IERR)
      IF (IERR.NE.0) GO TO 980
      DIM(1) = 1
      CALL EDIPUT (UVEDIT, 'DOUVFLAG', OOALOG, DIM, DOFLAG, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 980
C                                       do TY table
      IF (INEXT.EQ.'TY') THEN
         CALL EDITTY ('INIT', UVEDIT, IERR)
C                                       Clean up - no apply
         IF (IERR.NE.0) THEN
            IF (IERR.LT.0) THEN
               CALL EDITTY ('ABOR', UVEDIT, JERR)
               IERR = 0
            ELSE
               CALL EDITTY ('KILL', UVEDIT, JERR)
               END IF
            CALL TABCLO (TBEDIT, JERR)
            CALL TABZAP (TBEDIT, JERR)
C                                       apply FG and clean up
         ELSE
            CALL EDITTY ('APPL', UVEDIT, IERR)
            CALL TABCLO (TBEDIT, JERR)
            CALL TABDES (TBEDIT, JERR)
            END IF
C                                       do SY table
      ELSE IF (INEXT.EQ.'SY') THEN
         CALL EDITSY ('INIT', UVEDIT, IERR)
C                                       Clean up - no apply
         IF (IERR.NE.0) THEN
            IF (IERR.LT.0) THEN
               CALL EDITSY ('ABOR', UVEDIT, JERR)
               IERR = 0
            ELSE
               CALL EDITSY ('KILL', UVEDIT, JERR)
               END IF
            CALL TABCLO (TBEDIT, JERR)
            CALL TABZAP (TBEDIT, JERR)
C                                       apply FG and clean up
         ELSE
            CALL EDITSY ('APPL', UVEDIT, IERR)
            CALL TABCLO (TBEDIT, JERR)
            CALL TABDES (TBEDIT, JERR)
            END IF
C                                       do SN/CL
      ELSE
         CALL EDITSN ('INIT', UVEDIT, IERR)
C                                       Clean up - no apply
         IF (IERR.NE.0) THEN
            IF (IERR.LT.0) THEN
               CALL EDITSN ('ABOR', UVEDIT, JERR)
               IERR = 0
            ELSE
               CALL EDITSN ('KILL', UVEDIT, JERR)
               END IF
            CALL TABCLO (TBEDIT, JERR)
            CALL TABZAP (TBEDIT, JERR)
C                                       apply FG and clean up
         ELSE
            CALL EDITSN ('APPL', UVEDIT, IERR)
            CALL TABCLO (TBEDIT, JERR)
            CALL TABDES (TBEDIT, JERR)
            END IF
         END IF
C                                       Delete object
      CALL EDICLO (UVEDIT, JERR)
 975  CALL EDIDES (UVEDIT, JERR)
C                                       close and delete TV device
 980  CALL TVDCLO (TVDEVC, JERR)
 985  CALL TVDDES (TVDEVC, JERR)
C                                       error message
 990  IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR EDITING ' // TBEDIT
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
      END
