LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NPARMS
      PARAMETER (NPARMS=17)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                      1         2          3        4         5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INEXT',
C            6         7          8         9           10
     *   'INVERS', 'FREQID', 'SUBARRAY', 'FLAGVER', 'OUTFGVER',
C            11          12     13     14          15        16
     *   'TIMERANG', 'BIF', 'EIF', 'ANTENNAS', 'DOHIST', 'REASON',
C            17        18        19       20        21         22
     *   'ANTUSE'/
C                    1       2       3       4       5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR,
C          6       7       8       9       10
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOAINT,
C          11      12      13      14      15      16
     *   OOARE,  OOAINT, OOAINT, OOAINT, OOARE, OOACAR,
C          17      18      19      20      21      22
     *   OOAINT/
C                   1     2     3     4     5
      DATA AVDIM /12,1,  6,1,  1,1,  1,1,  2,1,
C         6     7     8     9     10
     *   1,1,  1,1,  1,1,  1,1,  1,1,
C         11    12    13    14    15    16
     *   8,1,  1,1,  1,1, 50,1,  1,1, 24,1,
C         17
     *  50,1/
LOCAL END
      PROGRAM BPEDT
C-----------------------------------------------------------------------
C! UV editing with the TV displaying bandpass-like tables
C# Task AP OOP UV EDITING TV-APPL CALIBRATION
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
      DATA PRGM /'BPEDT'/
C-----------------------------------------------------------------------
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
C   EDITIN gets input parameters for BPEDT, creates the EDIT object.
C   Inputs:
C      PRGN     C*6    Program name
C   Output:
C      UVDATA   C*32   Name of input uv data. - as master
C      TBEDIT   C*32   Name of table object to be used in editing
C      TBTYPE   C*2    Type of table in use
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
      PARAMETER (NKEY2=14)
      INTEGER   DIM(7), TYPE, DUMMY, BIF, EIF, NIF, SUBA, FGVERI,
     *   FGVERO, FGVERC, FGV
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, CDUMMY*1, FGNAME(3)*32, FGIN*32
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to UVDATA object
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'DOHIST'/
C                                       Rename
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'DOHIST'/
C                                       Adverbs for TBEDIT object
      DATA INK2 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVERS',
     *   'FREQID', 'SUBARRAY', 'TIMERANG', 'BIF', 'EIF', 'ANTENNAS',
     *   'INEXT', 'REASON', 'ANTUSE'/
C                                       Rename
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'VER',
     *   'FRQSEL', 'SUBARR', 'TIMERANG', 'BIF', 'EIF', 'ANTENNAS',
     *   'TBLTYPE', 'REASON', 'ANTS2USE'/
      DATA FGNAME /'IN_FGVER', 'OUT_FGVER', 'COP_FGVER'/
C-----------------------------------------------------------------------
C                                       Startup interactive
      CALL AV2INT (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IERR)
      IF (IERR.NE.0) GO TO 999
      RQUICK = .FALSE.
C                                       check table type
      CALL OGET ('Input', 'INEXT', TYPE, DIM, DUMMY, TBTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
      IF ((TBTYPE.NE.'BP') .AND. (TBTYPE.NE.'BD') .AND.
     *   (TBTYPE.NE.'CP') .AND. (TBTYPE.NE.'PD')) THEN
         TBTYPE = 'BP'
         CALL OPUT ('Input', 'INEXT', TYPE, DIM, DUMMY, TBTYPE, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
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
C                                       Find highest FG table.
      FGIN = 'Temporary FG table for EDITR'
      FGV = 1
      CALL UV2TAB (UVDATA, FGIN, 'FG', FGV, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TBLHIV (FGIN, FGV, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDES (FGIN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set FLAGVER
      CALL OGET ('Input', 'FLAGVER', TYPE, DIM, FGVERI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTFGVER', TYPE, DIM, FGVERO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      FGVERI = MIN (FGVERI, FGV)
      IF (FGVERI.EQ.0) FGVERI = FGV
      IF ((FGVERO.LE.0) .OR. (FGVERO.GT.FGV)) FGVERO = FGV + 1
      FGVERC = FGVERI
      IF (FGVERO.LE.FGV) FGVERC = - ABS (FGVERI)
      CALL OPUT (UVDATA, FGNAME(1), TYPE, DIM, FGVERI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVDATA, FGNAME(2), TYPE, DIM, FGVERO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVDATA, FGNAME(3), TYPE, DIM, FGVERC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create TBEDIT object
      TBEDIT = TBTYPE // ' table to be used in editing'
      CALL CREATE (TBEDIT, 'TABLE', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, TBEDIT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Use input for editing
C                                       Open TBEDIT to be sure it's OK.
      CALL OOPEN (TBEDIT, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (TBEDIT, 'KEY.NO_IF', TYPE, DIM, NIF, CDUMMY, IERR)
      IF ((IERR.NE.0) .OR. (NIF.LE.0)) NIF = 1
      CALL OCLOSE (TBEDIT, IERR)
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
C
 999  RETURN
      END
      SUBROUTINE EDITIT (UVDATA, TBEDIT, TBTYPE, IERR)
C-----------------------------------------------------------------------
C   Does the editing
C   Inputs:
C      UVDATA   C*32   Name of input uv data. - as master
C      TBEDIT   C*32   Name of TY table data
C      TBTYPE   C*2    Type of table
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER UVDATA*(*), TBEDIT*(*), TBTYPE*2
C
      CHARACTER UVEDIT*32, STATUS*4, TVDEVC*32, CDUMMY*1
      INTEGER   JERR, DUMMY, DIM(7)
      LOGICAL   DOFLAG
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA DOFLAG /.TRUE./
C-----------------------------------------------------------------------
C                                       open TV object
      TVDEVC = 'BPEDT task TV object'
      CALL TVDCRE (TVDEVC, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TVDOPN (TVDEVC, STATUS, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       open edit object
      UVEDIT = 'BPEDT task EDIT object'
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
C                                       do it
      CALL EDITBP ('INIT', UVEDIT, TBTYPE, IERR)
C                                       Clean up - no apply
      IF (IERR.NE.0) THEN
         IF (IERR.LT.0) THEN
            CALL EDITBP ('ABOR', UVEDIT, TBTYPE, JERR)
            IERR = 0
         ELSE
            CALL EDITBP ('KILL', UVEDIT, TBTYPE, JERR)
            END IF
C                                       apply FG and clean up
      ELSE
         CALL EDITBP ('APPL', UVEDIT, TBTYPE, IERR)
         END IF
      CALL TABCLO (TBEDIT, JERR)
      CALL TABDES (TBEDIT, JERR)
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
