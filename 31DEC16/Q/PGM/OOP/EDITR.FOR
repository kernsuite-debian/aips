LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NPARMS
      PARAMETER (NPARMS=44)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                      1         2          3        4         5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'SOURCES',
C            6       7          8           9          10         11
     *   'QUAL', 'CALCODE', 'TIMERANG', 'SELBAND', 'SELFREQ', 'FREQID',
C            12          13         14         15       16       17
     *   'SUBARRAY', 'DOCALIB', 'GAINUSE', 'DOPOL', 'PDVER', 'BLVER',
C            18        19          20        21       22        23
     *   'FLAGVER', 'OUTFGVER', 'DOBAND', 'BPVER', 'SMOOTH', 'STOKES',
C            24       25      26     27      28         29
     *   'BCHAN', 'ECHAN', 'BIF', 'EIF', 'UVRANGE', 'ANTENNAS',
C            30         31          32        33         34        35
     *   'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK', 'DOHIST', 'SOLINT',
C            36        37         38        39         40        41
     *   'DETIME', 'DOWEIGHT', 'DOTWO', 'EXPERT', 'CROWDED', 'DO3COL',
C            42        43        44
     *   'REASON', 'ANTUSE', 'BADDISK'/
C                    1       2       3       4       5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR,
C          6       7       8      9      10     11
     *   OOAINT, OOACAR, OOARE, OOARE, OOARE, OOAINT,
C          12      13      14      15      16      17
     *   OOAINT, OOARE,  OOAINT, OOAINT, OOAINT, OOAINT,
C          18      19      20      21      22     23
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOARE, OOACAR,
C          24      25      26      27     28      29
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOARE, OOAINT,
C          30      31      32      33      34     35
     *   OOACAR, OOACAR, OOAINT, OOAINT, OOARE, OOARE,
C          36     37     38      39      40      41
     *   OOARE, OOARE, OOALOG, OOALOG, OOALOG, OOARE,
C          42      43      44
     *   OOACAR, OOAINT, OOAINT/
C                   1     2     3     4     5
      DATA AVDIM /12,1,  6,1,  1,1,  1,1, 16,30,
C         6     7     8     9     10    11
     *   1,1,  4,1,  8,1,  1,1,  1,1,  1,1,
C         12    13    14    15    16    17
     *   1,1,  1,1,  1,1,  1,1,  1,1,  1,1,
C         18    19    20    21    22    23
     *   1,1,  1,1,  1,1,  1,1,  3,1,  4,1,
C         24    25    26    27    28    29
     *   1,1,  1,1,  1,1,  1,1,  2,1, 50,1,
C         30    31    32    33    34    35
     *   12,1, 6,1,  1,1,  1,1,  1,1,  1,1,
C         36    37    38    39    40    41
     *   1,1,  1,1,  1,1,  1,1,  1,1,  1,1,
C         42    43    44
     *   24,1, 50,1, 10,1/
LOCAL END
      PROGRAM EDITR
C-----------------------------------------------------------------------
C! UV data editing with the TV in baseline-based manner
C# Task AP OOP UV EDITING TV-APPL
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-2000, 2003, 2006-2007, 2010, 2015
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
C   UV data editing
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UVDATA*32, UVDAT2*32
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'EDITR'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL EDITIN (PRGM, UVDATA, UVDAT2, IRET)
C                                       CLEAN
      IF (IRET.EQ.0) CALL EDITIT (UVDATA, UVDAT2, IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE EDITIN (PRGN, UVDATA, UVDAT2, IERR)
C-----------------------------------------------------------------------
C   EDITIN gets input parameters for EDITR and sets UV data adverbs
C   Inputs:
C      PRGN     C*6    Program name
C   Output:
C      UVDATA   C*32   Name of input uv data. - as master
C      UVDAT2   C*32   Name of 2nd input uv file to display
C      IERR     I      Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER PRGN*6, UVDATA*(*), UVDAT2*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1, NKEY2
C                                       NKEY1 = # adverbs FOR UVDATA
      PARAMETER (NKEY1=36)
C                                       NKEY2 = # adverbs for UVDAT2
      PARAMETER (NKEY2=12)
C
      INTEGER   DIM(7), TYPE, BCHAN, ECHAN, I, DUMMY, BIF, EIF, NIF,
     *   NAXIS(7), FQINDX, IFINDX, NCHN, BCHAN2, ECHAN2, BIF2, EIF2,
     *   STINDX, NCHN2, NIF2, FGVERI, FGVERO, FGV, FGVERC
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, CDUMMY*1, TINAME*12, TICLAS*6, STOKES*4,
     *   FGNAME(4)*32, FGIN*32
      DOUBLE PRECISION CRVAL(7)
      LOGICAL   LTEMP
      REAL      XDOCAL
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to UVDATA object
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'BCHAN', 'ECHAN', 'BIF', 'EIF',
     *   'STOKES', 'GAINUSE',
     *   'TIMERANG', 'UVRANGE', 'ANTENNAS',
     *   'SOURCES', 'QUAL', 'SELBAND',
     *   'SELFREQ', 'FREQID', 'CALCODE',
     *   'SUBARRAY', 'DOPOL', 'BLVER',
     *   'DOBAND', 'BPVER', 'SMOOTH',
     *   'SOLINT', 'DETIME', 'DOHIST', 'DOWEIGHT', 'DOTWO',
     *   'EXPERT', 'ANTUSE', 'REASON', 'CROWDED', 'DO3COL',
     *   'PDVER'/
C                                       Rename
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK',
     *   'CALEDIT.BCHAN', 'CALEDIT.ECHAN', 'CALEDIT.BIF', 'CALEDIT.EIF',
     *   'CALEDIT.STOKES', 'CALEDIT.CLUSE',
     *   'CALEDIT.TIMRNG', 'CALEDIT.UVRNG', 'CALEDIT.ANTENNS',
     *   'CALEDIT.SOURCS', 'CALEDIT.SELQUA', 'CALEDIT.SELBAN',
     *   'CALEDIT.SELFRQ', 'CALEDIT.FRQSEL', 'CALEDIT.SELCOD',
     *   'CALEDIT.SUBARR', 'CALEDIT.DOPOL', 'CALEDIT.BLVER',
     *   'CALEDIT.DOBAND', 'CALEDIT.BPVER', 'CALEDIT.SMOOTH',
     *   'EQU_TIME', 'GAP_TIME', 'DOHIST', 'REWEIGHT', 'COMPARE',
     *   'DOEXPERT', 'ANTS2USE', 'REASON', 'CROWDED', 'DO3COLOR',
     *   'CALEDIT.PDVER'/
C                                       Adverbs for UVDAT2 object
      DATA INK2 /'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK', 'TIMERANG',
     *   'UVRANGE', 'BCHAN', 'ECHAN', 'BIF', 'EIF', 'STOKES',
     *   'ANTENNAS'/
C                                       Rename
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'CALEDIT.TIMRNG',
     *   'CALEDIT.UVRNG', 'CALEDIT.BCHAN', 'CALEDIT.ECHAN',
     *   'CALEDIT.BIF', 'CALEDIT.EIF', 'CALEDIT.STOKES',
     *   'CALEDIT.ANTENNS'/
      DATA FGNAME /'CALEDIT.FGVER', 'IN_FGVER', 'OUT_FGVER',
     *   'COP_FGVER'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INT (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IERR)
      IF (IERR.NE.0) GO TO 999
      RQUICK = .FALSE.
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create UV master object
      UVDATA = 'UVDATA master input object'
      CALL CREATE (UVDATA, 'UVDATA', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, UVDATA, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOCALIB
      CALL OGET ('Input', 'DOCALIB', TYPE, DIM, XDOCAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      LTEMP = XDOCAL.GT.0.0
      CALL OPUT (UVDATA, 'CALEDIT.DOCAL', OOALOG, DIM, LTEMP, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      LTEMP = (XDOCAL.GT.0.0) .AND. (XDOCAL.LE.99.0)
      CALL OPUT (UVDATA, 'CALEDIT.DOWTCL', OOALOG, DIM, LTEMP, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get IF channel selection
      CALL SECSLT (UVDATA, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       All Stokes', IF, channels
      CALL SECSAV (UVDATA, 1, 0, 1, 0, '    ', IERR)
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
      CALL OPUT (UVDATA, FGNAME(2), TYPE, DIM, FGVERI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVDATA, FGNAME(3), TYPE, DIM, FGVERO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVDATA, FGNAME(4), TYPE, DIM, FGVERC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find size of axes
      CALL UVDFND (UVDATA, 2, 'FREQ', FQINDX, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDFND (UVDATA, 2, 'STOKES', STINDX, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDFND (UVDATA, 2, 'IF  ', IFINDX, IERR)
      IERR = 0
      CALL OGET (UVDATA, 'UV_DESC.NAXIS', TYPE, DIM, NAXIS, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (UVDATA, 'UV_DESC.CRVAL', TYPE, DIM, CRVAL, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       channel defaults
      NCHN = NAXIS(FQINDX)
      BCHAN = MAX (BCHAN, 1)
      BCHAN = MIN (BCHAN, NCHN)
      IF (ECHAN.LT.BCHAN) ECHAN = NCHN
      ECHAN = MIN (ECHAN, NCHN)
C                                       IF defaults
      IF (IFINDX.GT.0) THEN
         NIF = NAXIS(IFINDX)
         BIF = MAX (1, BIF)
         BIF = MIN (BIF, NIF)
         IF (EIF.LT.BIF) EIF = NIF
         EIF = MIN (EIF, NIF)
      ELSE
         BIF = 1
         EIF = 1
         NIF = 1
         END IF
C                                       Stokes
      IF ((STOKES.NE.'I') .AND. (CRVAL(STINDX).LT.0.0D0) .AND.
     *   (NAXIS(STINDX).GT.1)) THEN
         STOKES = 'HALF'
      ELSE
         STOKES = 'I'
         END IF
C                                       Do 2nd image?
      CALL OGET ('Input', 'IN2NAME', TYPE, DIM, DUMMY, TINAME, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'IN2CLASS', TYPE, DIM, DUMMY, TICLAS, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'IN2DISK', TYPE, DIM, I, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create UVDAT2 object
      IF ((TINAME.EQ.' ') .OR. (TICLAS.EQ.' ') .OR. (I.LE.0)) THEN
         UVDAT2 = ' '
      ELSE
         UVDAT2 = 'Input UVdata to be edited'
         CALL CREATE (UVDAT2, 'UVDATA', IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
         CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, UVDAT2, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Get IF channel selection
         CALL SECSLT (UVDAT2, BIF2, EIF2, BCHAN2, ECHAN2, STOKES, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       All Stokes', IF, channels
         CALL SECSAV (UVDAT2, 1, 0, 1, 0, '    ', IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Open UVDAT2 to be sure it's OK.
         CALL OOPEN (UVDAT2, 'READ', IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OCLOSE (UVDAT2, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       check parameters
C                                       Find size of axes
         CALL UVDFND (UVDAT2, 2, 'FREQ', FQINDX, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL UVDFND (UVDAT2, 2, 'STOKES', STINDX, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL UVDFND (UVDAT2, 2, 'IF  ', IFINDX, IERR)
         IERR = 0
         CALL OGET (UVDAT2, 'UV_DESC.NAXIS', TYPE, DIM, NAXIS, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (UVDAT2, 'UV_DESC.CRVAL', TYPE, DIM, CRVAL, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
C                                       channel defaults
         NCHN2 = NAXIS(FQINDX)
         IF (NCHN2.GE.NCHN) THEN
            BCHAN2 = BCHAN
            ECHAN2 = ECHAN
         ELSE
            BCHAN2 = 1
            ECHAN2 = ECHAN - BCHAN + 1
            ECHAN2 = MIN (ECHAN2, NCHN2)
            END IF
C                                       IF defaults
         IF (IFINDX.GT.0) THEN
            NIF2 = NAXIS(IFINDX)
         ELSE
            NIF2 = 1
            END IF
         IF (NIF2.GE.NIF) THEN
            BIF2 = BIF
            EIF2 = EIF
         ELSE
            BIF2 = 1
            EIF2 = EIF - BIF + 1
            IF (EIF2.GT.NIF2) THEN
               MSGTXT = 'IF AXES DO NOT MATCH AS SELECTED'
               IERR = 8
               CALL MSGWRT (8)
               GO TO 999
               END IF
            END IF
C                                       Stokes
         IF ((STOKES.NE.'I') .AND. (CRVAL(STINDX).LT.0.0D0) .AND.
     *         (NAXIS(STINDX).GT.1)) THEN
            STOKES = 'HALF'
         ELSE
            STOKES = 'I'
            END IF
C                                       Reset Selection
         CALL SECSAV (UVDAT2, BIF2, EIF2, BCHAN2, ECHAN2, STOKES, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Reset Selection
      CALL SECSAV (UVDATA, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Save in Inputs for history
      DIM(1) = 4
      CALL OPUT ('Input', 'STOKES', OOACAR, DIM, DUMMY, STOKES, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      CALL OPUT ('Input', 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT ('Input', 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT ('Input', 'BIF', OOAINT, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT ('Input', 'EIF', OOAINT, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE EDITIT (UVDATA, UVDAT2, IERR)
C-----------------------------------------------------------------------
C   Does the editing
C   Inputs:
C      UVDATA   C*32   Name of input uv data. - as master
C      UVDAT2   C*32   Name of input uv work file also to edit
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER UVDATA*(*), UVDAT2*(*)
C
      CHARACTER UVEDIT*32, STATUS*4, TVDEVC*32, CDUMMY*1
      INTEGER   JERR, DUMMY, DIM(7)
      LOGICAL   DOFLAG
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA DOFLAG /.TRUE./
C-----------------------------------------------------------------------
C                                       open TV object
      TVDEVC = 'EDITR task TV object'
      CALL TVDCRE (TVDEVC, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TVDOPN (TVDEVC, STATUS, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       open edit object
      UVEDIT = 'EDITR task EDIT object'
      CALL EDICRE (UVEDIT, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL EDIOPN (UVEDIT, STATUS, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       Insert object info
      DIM(1) = LEN(UVDATA)
      DIM(2) = 1
      CALL EDIPUT (UVEDIT, 'UVMASTER', OOACAR, DIM, DUMMY, UVDATA, IERR)
      IF (IERR.NE.0) GO TO 980
      DIM(1) = LEN(UVDAT2)
      CALL EDIPUT (UVEDIT, 'UVSECOND', OOACAR, DIM, DUMMY, UVDAT2, IERR)
      IF (IERR.NE.0) GO TO 980
      DIM(1) = LEN(TVDEVC)
      CALL EDIPUT (UVEDIT, 'TVDEVICE', OOACAR, DIM, DUMMY, TVDEVC, IERR)
      IF (IERR.NE.0) GO TO 980
      DIM(1) = 1
      CALL EDIPUT (UVEDIT, 'DOUVFLAG', OOALOG, DIM, DOFLAG, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 980
C                                       do it
      CALL EDITUV ('INIT', UVEDIT, IERR)
C                                       Clean up - no apply
      IF (IERR.NE.0) THEN
         IF (IERR.LT.0) THEN
            CALL EDITUV ('ABOR', UVEDIT, JERR)
            IERR = 0
         ELSE
            CALL EDITUV ('KILL', UVEDIT, JERR)
            END IF
C                                       apply FG and clean up
      ELSE
         CALL EDITUV ('APPL', UVEDIT, IERR)
         END IF
      IF (UVDAT2.NE.' ') CALL OUVDES (UVDAT2, JERR)
      CALL OUVDES (UVDATA, IERR)
C                                       Delete object
      CALL EDICLO (UVEDIT, JERR)
 975  CALL EDIDES (UVEDIT, JERR)
C                                       close and delete TV device
 980  CALL TVDCLO (TVDEVC, JERR)
 985  CALL TVDDES (TVDEVC, JERR)
C                                       error message
 990  IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR EDITING ' // UVDATA
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
      END
