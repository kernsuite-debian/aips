LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NPARMS
      PARAMETER (NPARMS=13)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                      1         2          3        4         5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVERS',
C            6         7           8           9      10     11
     *   'FREQID', 'SUBARRAY', 'TIMERANG', 'BIF', 'EIF', 'ANTENNAS',
C            12        13
     *   'DOHIST', 'ANTUSE'/
C                    1       2       3       4       5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT,
C          6       7       8      9       10      11
     *   OOAINT, OOAINT, OOARE, OOAINT, OOAINT, OOAINT,
C          12      13
     *   OOAINT, OOAINT/
C                   1     2     3     4     5
      DATA AVDIM /12,1,  6,1,  1,1,  1,1,  1,1,
C         6     7     8     9     10     11
     *   1,1,  1,1,  8,1,  1,1,  1,1,  50,1,
C         12    13
     *   1,1, 50,1/
LOCAL END
      PROGRAM PCEDT
C-----------------------------------------------------------------------
C! Edit the PC table with a graphical editor
C# Task AP OOP UV EDITING TV-APPL CALIBRATION VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 2016
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
C   TY table data used for uv editing
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UVDATA*32, TBEDIT*32, TBTYPE*2
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'PCEDT'/
C-----------------------------------------------------------------------
      TBTYPE = 'PC'
C                                       Startup
      CALL EDITIN (PRGM, UVDATA, TBEDIT, TBTYPE, IRET)
C                                       CLEAN
      IF (IRET.EQ.0) CALL EDITIT (UVDATA, TBEDIT, TBTYPE, IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE EDITIN (PRGN, UVDATA, TBEDIT, TBTYPE, IERR)
C-----------------------------------------------------------------------
C   EDITIN gets input parameters for PCEDT, creates the EDIT object.
C   Inputs:
C      PRGN     C*6    Program name
C   Output:
C      UVDATA   C*32   Name of input uv data. - as master
C      TBEDIT   C*32   Name of table object to be used in editing
C      IERR     I      Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER PRGN*6, UVDATA*(*), TBEDIT*(*), TBTYPE*2
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1, NKEY2
C                                       NKEY1 = # adverbs FOR UVDATA
      PARAMETER (NKEY1=5)
C                                       NKEY2 = # adverbs for TBEDIT
      PARAMETER (NKEY2=12)
      INTEGER   DIM(7), TYPE, DUMMY, BIF, EIF, NIF, SUBA, PCVERI,
     *   PCV, I
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, CDUMMY*1, PCIN*32, TBINP*32
      LOGICAL   EXIST
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to UVDATA object
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'DOHIST'/
C                                       Rename
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'DOHIST'/
C                                       Adverbs for TBINP object
      DATA INK2 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVERS',
     *   'FREQID', 'SUBARRAY', 'TIMERANG', 'BIF', 'EIF', 'ANTENNAS',
     *   'ANTUSE'/
C                                       Rename
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'VER',
     *   'FRQSEL', 'SUBARR', 'TIMERANG', 'BIF', 'EIF', 'ANTENNAS',
     *   'ANTS2USE'/
C-----------------------------------------------------------------------
C                                       Startup interactive
      CALL AV2INT (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IERR)
      IF (IERR.NE.0) GO TO 999
      RQUICK = .FALSE.
C                                       SUBARRAY
      CALL OGET ('Input', 'SUBARRAY', TYPE, DIM, SUBA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      SUBA = MAX (1, SUBA)
      CALL OPUT ('Input', 'SUBARRAY', TYPE, DIM, SUBA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
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
C                                       Find highest PC table.
      PCIN = 'Temporary PC table for PCEDT'
      PCV = 1
      CALL UV2TAB (UVDATA, PCIN, 'PC', PCV, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TBLHIV (PCIN, PCV, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDES (PCIN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'INVERS', TYPE, DIM, PCVERI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF ((PCVERI.LE.0) .OR. (PCVERI.GT.PCV)) PCVERI = PCV
      CALL OPUT ('Input', 'INVERS', TYPE, DIM, PCVERI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create TBINP object
      TBINP = TBTYPE // ' temporary table to be used in copying'
      CALL CREATE (TBINP, 'TABLE', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, TBINP, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 2
      DIM(2) = 1
      CALL OPUT (TBINP, 'TBLTYPE', OOACAR, DIM, DUMMY, TBTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Use input for editing
C                                       Open TBINP to be sure it's OK.
      CALL OOPEN (TBINP, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (TBINP, 'KEY.NO_BAND', TYPE, DIM, NIF, CDUMMY, IERR)
      IF ((IERR.NE.0) .OR. (NIF.LE.0)) NIF = 1
      CALL OCLOSE (TBINP, IERR)
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
      CALL OPUT (TBINP, 'BIF', OOAINT, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (TBINP, 'EIF', OOAINT, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       copy to new PC table
      TBEDIT = 'PC table to be used in editing'
      CALL CREATE (TBEDIT, 'TABLE', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       copy the object parts
      CALL TBCOPY (TBINP, TBEDIT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       did not get adverbs
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, TBedit, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 2
      DIM(2) = 1
      CALL OPUT (TBINP, 'TBLTYPE', OOACAR, DIM, DUMMY, TBTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       new version
      DIM(1) = 1
      DIM(2) = 1
      I = PCV + 1
      CALL OPUT (TBEDIT, 'VER', OOAINT, DIM, I, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGTXT,1010) PCVERI, I
      CALL MSGWRT (3)
C                                       Make sure output doesn't exist
      CALL OBFEXS (TBEDIT, EXIST, IERR)
      IERR = 0
      IF (EXIST) THEN
         CALL TABRMV (TBEDIT, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       copy contents
      CALL TBLCOP (TBINP, TBEDIT, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Copying PC table version',I4,' to new version',I4)
      END
      SUBROUTINE EDITIT (UVDATA, TBEDIT, TBTYPE, IERR)
C-----------------------------------------------------------------------
C   Does the editing
C   Inputs:
C      UVDATA   C*32   Name of input uv data. - as master
C      TBEDIT   C*32   Name of PC table data
C      TBTYPE   C*2    Type of table
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER UVDATA*(*), TBEDIT*(*), TBTYPE*2
C
      CHARACTER UVEDIT*32, STATUS*4, TVDEVC*32
      INTEGER   JERR, DUMMY, DIM(7)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       open TV object
      TVDEVC = 'PCEDT task TV object'
      CALL TVDCRE (TVDEVC, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TVDOPN (TVDEVC, STATUS, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       open edit object
      UVEDIT = 'PCEDT task EDIT object'
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
C                                       do it
      CALL EDITPC ('INIT', UVEDIT, TBTYPE, IERR)
C                                       Clean up - no apply
C                                       These zap TBEDIT
      IF (IERR.NE.0) THEN
         IF (IERR.LT.0) THEN
            CALL EDITPC ('ABOR', UVEDIT, TBTYPE, JERR)
            IERR = 0
         ELSE
            CALL EDITPC ('KILL', UVEDIT, TBTYPE, JERR)
            END IF
C                                       apply FG and clean up
      ELSE
         CALL EDITPC ('APPL', UVEDIT, TBTYPE, IERR)
         CALL TABCLO (TBEDIT, JERR)
         CALL TABDES (TBEDIT, JERR)
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
