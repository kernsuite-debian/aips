LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NPARMS
      PARAMETER (NPARMS=45)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'SOURCES',
     *   'QUAL', 'CALCODE', 'TIMERANG', 'SELBAND', 'SELFREQ', 'FREQID',
     *   'BIF', 'EIF', 'BCHAN', 'ECHAN', 'SUBARRAY', 'DOCALIB',
     *   'GAINUSE', 'DOPOL', 'PDVER', 'BLVER', 'FLAGVER', 'OUTFGVER',
     *   'DOBAND', 'BPVER', 'SMOOTH', 'UVRANGE', 'SOLINT', 'SCANLENG',
     *   'REFANT', 'OPTYPE', 'DOCAT', 'VECTOR', 'DOROBUST', 'BPARM',
     *   'CPARM', 'DPARM', 'STOKES', 'DOSTOKES', 'DOIFS', 'DOALL',
     *   'DOCRT', 'OUTPRINT', 'PRTLEV', 'BADDISK'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR,
     *   OOAINT, OOACAR, OOARE, OOARE, OOARE, OOAINT,
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOAINT, OOARE,
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOAINT, OOAINT,
     *   OOAINT, OOAINT, OOARE, OOARE, OOARE, OOARE,
     *   OOAINT, OOACAR, OOALOG, OOALOG, OOALOG, OOARE,
     *   OOARE, OOARE, OOACAR, OOAINT, OOALOG, OOALOG,
     *   OOAINT, OOACAR, OOAINT, OOAINT/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 16,30,
     *   1,1, 4,1, 8,1, 1,1, 1,1, 1,1,
     *   1,1, 1,1, 1,1, 1,1, 1,1, 1,1,
     *   1,1, 1,1, 1,1, 1,1, 1,1, 1,1,
     *   1,1, 1,1, 3,1, 2,1, 1,1, 1,1,
     *   1,1, 4,1, 1,1, 1,1, 1,1, 10,1,
     *   10,1, 10,1, 4,1, 1,1, 1,1, 1,1,
     *   1,1, 48,1, 1,1, 10,1/
LOCAL END
LOCAL INCLUDE 'XXTAB.INC'
C                                       Column indices
C                                         (1) time1
C                                         (2) time2
C                                         (3) Source
C                                         (4) NSAMP amplitudes
C                                         (5) NSAMP sigmas
C                                         (6) NSAMP weights
C                                         (7) NSAMP closure fractions
C                                         (8) NCORS reference ants
C                                       NCORS = Cif * Cpol
C                                       NSAMP = Cant * Cif * Cpol
      INTEGER NCOL
      PARAMETER (NCOL = 8)
C
      INTEGER   COLIDX(NCOL), NSAMP, NCORS, XXTYPE
C
      COMMON /TABXX/ COLIDX, NSAMP, NCORS, XXTYPE
LOCAL END
      PROGRAM FLAGR
C-----------------------------------------------------------------------
C! Automatic flagging task
C# Task OOP calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2004-2007, 2009-2011, 2015
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
C   Data editing based on rms with various algorithms
C   This task takes the rms in the Real part and the rms in the
C   Imaginary part as two samples of the Amplitude rms.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UVIN*32, PRINT*32, INPUT*32, FGTAB*32
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'FLAGR '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL FLAGRI (PRGM, INPUT, UVIN, PRINT, FGTAB, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Check interference
      CALL FLAGRO (INPUT, UVIN, PRINT, FGTAB, IRET)
C                                       Close down files, etc.
 990  CALL OUT2AV (IRET, 0, ' ', ' ', BUFF1)
C
 999  STOP
      END
      SUBROUTINE FLAGRI (PRGN, INPUT, UVIN, PRINT, FGTAB, IERR)
C-----------------------------------------------------------------------
C   FLAGRI gets input parameters for FLAGR and creates the input uvdata
C   object.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      INPUT   C*?   Task inputs object
C      UVIN    C*?   Input multisource uv data object.
C      PRINT   C*?   Print object
C      FGTAB   C*?   Output flag table object
C      IERR    I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER PRGN*(*), INPUT*(*), UVIN*(*), PRINT*(*), FGTAB*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INTEGER   NKEY, NKEY2
C                                       NKEY=no. adverbs to copy to
C                                       UVIN
      PARAMETER (NKEY=33)
C                                       NKEY2=no. adverbs for PRINT
      PARAMETER (NKEY2=2)
      INTEGER   DIM(7), TYPE, FGVER, DUMMY, DISKI, CNOI, FGOUT, FGV,
     *   TCAT(256), UCAT(256), FGROW, DOCRT, SORT(2)
      REAL      BPARM(10), CPARM(10), DPARM(10), SOLINT, XDOCAL
      LOGICAL   LTEMP
      CHARACTER INK(NKEY)*8, OUTK(NKEY)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, STOKES*4, CDUMMY*1, FGIN*32
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for UVIN
      DATA INK /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'GAINUSE', 'DOPOL', 'BLVER', 'DOBAND', 'BPVER',
     *   'FLAGVER',  'TIMERANG', 'UVRANGE', 'SOURCES', 'QUAL',
     *   'SELBAND', 'SELFREQ', 'FREQID', 'CALCODE', 'SUBARRAY',
     *   'BIF', 'EIF', 'BCHAN', 'ECHAN', 'PRTLEV', 'DOSTOKES', 'DOIFS',
     *   'DOALL', 'DOCAT', 'SCANLENG', 'REFANT', 'SMOOTH', 'PDVER',
     *   'DOROBUST'/
      DATA OUTK /'NAME', 'CLASS', 'IMSEQ', 'DISK',
     *   'CALEDIT.CLUSE', 'CALEDIT.DOPOL', 'CALEDIT.BLVER',
     *   'CALEDIT.DOBAND', 'CALEDIT.BPVER', 'CALEDIT.FGVER',
     *   'CALEDIT.TIMRNG', 'CALEDIT.UVRNG', 'CALEDIT.SOURCS',
     *   'CALEDIT.SELQUA', 'CALEDIT.SELBAN', 'CALEDIT.SELFRQ',
     *   'CALEDIT.FRQSEL', 'CALEDIT.SELCOD', 'CALEDIT.SUBARR',
     *   'CALEDIT.BIF', 'CALEDIT.EIF', 'CALEDIT.BCHAN',
     *   'CALEDIT.ECHAN', 'PRTLEV', 'DOSTOKES','DOIFS','DOCHANS',
     *   'DOCAT', 'SCANLENG', 'REFANT', 'CALEDIT.SMOOTH',
     *   'CALEDIT.PDVER', 'DOROBUST'/
C                                       Adverbs for PRINT
      DATA INK2  /'DOCRT', 'OUTPRINT'/
      DATA OUTK2 /'DOCRT', 'LPFILE'/
C-----------------------------------------------------------------------
C                                       Startup
      INPUT = 'Input'
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, INPUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET (INPUT, 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INPUT, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create UVIN
      UVIN = 'Input UVdata'
      CALL CREATE (UVIN, 'UVDATA', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ (INPUT, NKEY, INK, OUTK, UVIN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOCALIB
      CALL OGET ('Input', 'DOCALIB', TYPE, DIM, XDOCAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      DIM(2) = 1
      LTEMP = XDOCAL.GT.0.0
      CALL OPUT (UVIN, 'CALEDIT.DOCAL', OOALOG, DIM, LTEMP, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      LTEMP = (XDOCAL.GT.0.0) .AND. (XDOCAL.LE.99.0)
      CALL OPUT (UVIN, 'CALEDIT.DOWTCL', OOALOG, DIM, LTEMP, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Select all Stokes'
      STOKES = ' '
      DIM(1) = LEN (STOKES)
      DIM(2) = 1
      CALL OPUT (UVIN, 'CALEDIT.STOKES', OOACAR, DIM, DUMMY, STOKES,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open UVIN to be sure it's OK.
      CALL OOPEN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBDSKC (UVIN, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OCLOSE (UVIN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Digest BPARM
      CALL OGET (INPUT, 'BPARM', TYPE, DIM, BPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       MAXBAD (0.51, 0.25)
      IF (ABS (BPARM(1)).LE.1.0E-20) BPARM(1) = 0.51
      IF (ABS (BPARM(2)).LE.1.0E-20) BPARM(2) = 0.25
C                                       gains
      IF (BPARM(4).EQ.0.0) BPARM(4) = 0.1
C                                       individual baselines
      IF (BPARM(5).LE.0.0) BPARM(5) = BPARM(3)
      IF (BPARM(6).EQ.0.0) BPARM(6) = BPARM(4)
      IF (BPARM(8).EQ.0.0) BPARM(8) = BPARM(7)
C                                       Save defaults in BPARM
      CALL OPUT (INPUT, 'BPARM', OOARE, DIM, BPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Save on UVIN
      DIM(1) = 2
      DIM(2) = 1
      CALL OPUT (UVIN, 'MAXBAD', OOARE, DIM, BPARM(1), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVIN, 'MINHIST', OOARE, DIM, BPARM(7), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 4
      CALL OPUT (UVIN, 'MAXRMS', OOARE, DIM, BPARM(3), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      LTEMP = (BPARM(9).LE.0.0)
      CALL OPUT (UVIN, 'KILLONES', OOALOG, DIM, LTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INPUT, 'VECTOR', TYPE, DIM, LTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      LTEMP = .NOT.LTEMP
      CALL OPUT (UVIN, 'AMPONLY', OOALOG, DIM, LTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       CPARM options
      CALL OGET (INPUT, 'CPARM', TYPE, DIM, CPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CPARM(1) = MAX (0.0, CPARM(1))
      CPARM(3) = MAX (0.0, CPARM(3))
      IF (CPARM(2).LE.CPARM(1)) CPARM(2) = 1.E6
      IF (CPARM(4).LE.CPARM(3)) CPARM(4) = 1.E6
      IF (CPARM(7).LE.0.0) CPARM(7) = 3.5
      IF (CPARM(8).LE.0.0) CPARM(8) = 8.0
      IF (CPARM(9).LE.0.0) CPARM(9) = 7.0
      IF (CPARM(10).LE.0.0) CPARM(10) = 16.0
      CALL OPUT (INPUT, 'CPARM', TYPE, DIM, CPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 4
      CALL OPUT (UVIN, 'AMPCLIP', OOARE, DIM, CPARM(1), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVIN, 'CLIPRMS', OOARE, DIM, CPARM(7), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 2
      CALL OPUT (UVIN, 'RMSLIMIT', OOARE, DIM, CPARM(5), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default FLAGVER
      CALL OGET (INPUT, 'FLAGVER', TYPE, DIM, FGVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find highest FG table.
      FGIN = 'Temporary FG table for FLAGRI'
      FGV = 1
      CALL UV2TAB (UVIN, FGIN, 'FG', FGV, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TBLHIV (FGIN, FGV, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDES (FGIN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       use highest for in
      IF (FGVER.EQ.0) FGVER = FGV
      IF (FGVER.GT.FGV) FGVER = FGV
      CALL OPUT (INPUT, 'FLAGVER', OOAINT, DIM, FGVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVIN, 'CALEDIT.FGVER', OOAINT, DIM, FGVER, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default SOLINT (60 sec)
      CALL OGET (INPUT, 'SOLINT', TYPE, DIM, SOLINT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (SOLINT.LE.0.0) SOLINT = 60.0
      CALL OPUT (INPUT, 'SOLINT', OOARE, DIM, SOLINT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVIN, 'TIMEAVG', OOARE, DIM, SOLINT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DPARM options
      CALL OGET (INPUT, 'DPARM', TYPE, DIM, DPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DPARM(1) = MAX (0.0, DPARM(1))
      DPARM(3) = MAX (0.0, DPARM(3))
      IF (DPARM(2).LE.DPARM(1)) DPARM(2) = 1.E10
      IF (DPARM(4).LE.DPARM(3)) DPARM(4) = 1.E10
      IF (DPARM(5).LE.0.01) DPARM(5) = 0.6
      IF (DPARM(6).LE.0.01) DPARM(6) = 1.0
      CALL OPUT (INPUT, 'DPARM', TYPE, DIM, DPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 4
      CALL OPUT (UVIN, 'WTCLIP', OOARE, DIM, DPARM(1), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 2
      CALL OPUT (UVIN, 'CLOSCLIP', OOARE, DIM, DPARM(5), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
C                                       Copy flag table to out
      FGTAB = 'Output FG table'
      CALL OGET (INPUT, 'OUTFGVER', TYPE, DIM, FGOUT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF ((FGOUT.LE.0) .OR. (FGOUT.GT.FGV)) FGOUT = FGV + 1
      CALL UV2TAB (UVIN, FGTAB, 'FG', FGOUT, IERR)
      IF (IERR.NE.0) GO TO 999
      IF ((FGVER.GT.0) .AND. (FGOUT.GT.FGV)) THEN
         CALL UV2TAB (UVIN, FGIN, 'FG', FGVER, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TBLCOP (FGIN, FGTAB, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'ERROR COPYING OLD FG TABLE TO NEW'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         CALL TABDES (FGIN, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       reset sort order
      CALL OFGINI (FGTAB, 'WRIT', FGROW, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (FGTAB, 'SORT', TYPE, DIM, SORT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      SORT(1) = 0
      SORT(2) = 0
      CALL OPUT (FGTAB, 'SORT', TYPE, DIM, SORT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABCLO (FGTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Update uv CATBLK
      CALL OBHGET (UVIN, UCAT, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBHGET (FGTAB, TCAT, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OUVTNF (TCAT, UCAT, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBHPUT (UVIN, UCAT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Printer object
      IF (DOCRT.NE.0) THEN
         PRINT = 'Printer object'
         CALL PRTCRE (PRINT, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
         CALL IN2OBJ (INPUT, NKEY2, INK2, OUTK2, PRINT, IERR)
         IF (IERR.NE.0) GO TO 999
         DIM(1) = LEN (UVIN)
         DIM(2) = 1
         CALL PRTPUT (PRINT, 'PRINTOBJ', OOACAR, DIM, DUMMY, UVIN,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         PRINT = ' '
         END IF
C
 999  RETURN
      END
      SUBROUTINE FSTOKE (INPUT, UVIN, CPOL, DOPOLS, XPOLS, IERR)
C-----------------------------------------------------------------------
C   Find and parse the STOKES and DOSTOKES parameters
C   Inputs:
C      INPUT    C*?      Task input object.
C      UVIN     C*?      Input multisource uv object
C   Outputs
C      CPOL     I        Number of polarizations in data set
C      DOPOLS   L(4)     Test this polarization
C      XPOLS    L(4,4)   XPOLS(i,j) if i is bad flag j
C      IERR     I        Error code
C-----------------------------------------------------------------------
      CHARACTER INPUT*(*), UVIN*(*)
      INTEGER   CPOL, IERR
      LOGICAL   DOPOLS(4), XPOLS(4,4)
C
      INTEGER   INDX, TYPE, DIM(7), DOSTOK, IDUMMY, UCAT(256)
      REAL      UCATR(256)
      DOUBLE PRECISION UCATD(128)
      CHARACTER DUMMY*1, STOKES*4
      EQUIVALENCE (UCAT, UCATD, UCATR)
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      CALL OBHGET (UVIN, UCAT, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDFND (UVIN, 2, 'STOKES', INDX, IERR)
      IF (IERR.NE.0) GO TO 999
      CPOL = UCAT(KINAX+INDX-1)
      CALL OGET (INPUT, 'STOKES', TYPE, DIM, IDUMMY, STOKES, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INPUT, 'DOSTOKES', TYPE, DIM, DOSTOK, DUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (DOSTOK.LT.1) DOSTOK = 3
      DOSTOK = MOD (DOSTOK-1,4) + 1
      DOPOLS(1) = .FALSE.
      DOPOLS(2) = .FALSE.
      DOPOLS(3) = .FALSE.
      DOPOLS(4) = .FALSE.
      IF (STOKES.EQ.' ') STOKES = 'HALF'
C                                       numeric pattern
      IF ((STOKES(:1).EQ.'1') .OR. (STOKES(:1).EQ.'0')) THEN
         DOPOLS(1) = STOKES(1:1).EQ.'1'
         DOPOLS(2) = STOKES(2:2).EQ.'1'
         DOPOLS(3) = STOKES(3:3).EQ.'1'
         DOPOLS(4) = STOKES(4:4).EQ.'1'
C                                       all
      ELSE IF (STOKES.EQ.'FULL') THEN
         DOPOLS(1) = .TRUE.
         DOPOLS(2) = .TRUE.
         DOPOLS(3) = .TRUE.
         DOPOLS(4) = .TRUE.
C                                       true stokes
      ELSE IF (UCATD(KDCRV+INDX-1).GT.0.0) THEN
         IF (UCATR(KRCIC+INDX-1).EQ.3.0) THEN
            IF ((STOKES.EQ.'I') .OR. (STOKES.EQ.'IV') .OR.
     *         (STOKES.EQ.'HALF')) DOPOLS(1) = .TRUE.
            IF ((STOKES.EQ.'IV') .OR. (STOKES.EQ.'V'))
     *         DOPOLS(2) = .TRUE.
         ELSE
            IF (STOKES.EQ.'HALF') DOPOLS(1) = .TRUE.
            IF (INDEX(STOKES,'I').NE.0) DOPOLS(1) = .TRUE.
            IF (INDEX(STOKES,'Q').NE.0) DOPOLS(2) = .TRUE.
            IF (INDEX(STOKES,'U').NE.0) DOPOLS(3) = .TRUE.
            IF (INDEX(STOKES,'V').NE.0) DOPOLS(4) = .TRUE.
            END IF
C                                       LL
      ELSE IF (UCATD(KDCRV+INDX-1).EQ.-2.0D0) THEN
         DOPOLS(1) = .TRUE.
C                                       RR/LL/RL/LR
      ELSE IF (UCATD(KDCRV+INDX-1).EQ.-1.0D0) THEN
         IF ((STOKES.EQ.'HALF') .OR. (STOKES.EQ.'RRLL')) THEN
            DOPOLS(1) = .TRUE.
            DOPOLS(2) = .TRUE.
         ELSE IF (STOKES.EQ.'RR') THEN
            DOPOLS(1) = .TRUE.
         ELSE IF (STOKES.EQ.'LL') THEN
            DOPOLS(2) = .TRUE.
         ELSE IF (STOKES.EQ.'RLLR') THEN
            DOPOLS(3) = .TRUE.
            DOPOLS(4) = .TRUE.
            END IF
C                                       RR/LL/RL/LR
      ELSE IF (UCATD(KDCRV+INDX-1).EQ.-5.0D0) THEN
         IF ((STOKES.EQ.'HALF') .OR. (STOKES.EQ.'XXYY')) THEN
            DOPOLS(1) = .TRUE.
            DOPOLS(2) = .TRUE.
         ELSE IF (STOKES.EQ.'XX') THEN
            DOPOLS(1) = .TRUE.
         ELSE IF (STOKES.EQ.'YY') THEN
            DOPOLS(2) = .TRUE.
         ELSE IF (STOKES.EQ.'XYYX') THEN
            DOPOLS(3) = .TRUE.
            DOPOLS(4) = .TRUE.
            END IF
         END IF
C                                       flag what when what?
      CALL LFILL (16, .TRUE., XPOLS)
      IF (DOSTOK.EQ.4) THEN
      ELSE IF (UCATD(KDCRV+INDX-1).GT.0.0) THEN
         IF (DOSTOK.EQ.2) THEN
            XPOLS(1,2) = .FALSE.
            XPOLS(1,3) = .FALSE.
            XPOLS(1,4) = .FALSE.
         ELSE IF (DOSTOK.EQ.1) THEN
            XPOLS(2,1) = .FALSE.
            XPOLS(3,1) = .FALSE.
            XPOLS(4,1) = .FALSE.
            END IF
      ELSE
         XPOLS(1,2) = .FALSE.
         XPOLS(2,1) = .FALSE.
         IF (DOSTOK.EQ.1) THEN
            XPOLS(3,1) = .FALSE.
            XPOLS(4,1) = .FALSE.
            XPOLS(3,2) = .FALSE.
            XPOLS(4,2) = .FALSE.
         ELSE IF (DOSTOK.EQ.2) THEN
            XPOLS(1,3) = .FALSE.
            XPOLS(1,4) = .FALSE.
            XPOLS(2,3) = .FALSE.
            XPOLS(2,4) = .FALSE.
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE FLAGRO (INPUT, UVIN, PRINT, FGTAB, IERR)
C-----------------------------------------------------------------------
C   Process data: allocates memory, calls various operations
C   Inputs:
C      INPUT   C*?   Task input object.
C      UVIN    C*?   Input multisource uv object
C      PRINT   C*?   Print object
C      FGTAB   C*?   Flag table output object
C   Output:
C      IERR    I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INPUT*(*), UVIN*(*), PRINT*(*), FGTAB*(*)
      INTEGER   IERR
C
      INTEGER   TDIM
      PARAMETER (TDIM = 100)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   TYPE, DIM(7), DUMMY, CCHAN, CIF, CPOL, NAXIS(7), INDEX,
     *   FGVER, FGOUT, CANT, IS1, IS2, ANVER, MXA, IDUMMY, NTIME,
     *   COUNT(MAXANT,MAXANT,MAXIF,8), NV,
     *   THEIUF(56*MAXANT*MAXANT*MAXIF)
      LOGICAL   EXISTS, LTEMP, DOPOLS(4), XPOLS(4,4)
      CHARACTER OPTYPE*4, HILINE*72, CDUMMY*1, TABLE*32, OPTYPS(7)*4
      DOUBLE PRECISION SUM(MAXANT,MAXANT,MAXIF,8),
     *   SUMSQ(MAXANT,MAXANT,MAXIF,8), SUMWT(MAXANT,MAXANT,MAXIF,8)
      REAL      RUM(MAXANT,MAXANT,MAXIF,16),
     *   RUMSQ(MAXANT,MAXANT,MAXIF,16), RUMWT(MAXANT,MAXANT,MAXIF,16)
      REAL      ANTA(MAXANT,MAXIF,4), ANTSQ(MAXANT,MAXIF,4), ACLIP(4),
     *   ANTW(MAXANT,MAXIF,4), SOLINT, FRA(MAXANT,MAXIF,4),
     *   THEBUF(56*MAXANT*MAXANT*MAXIF)
      EQUIVALENCE (THEBUF, THEIUF, SUM, RUM), (SUMSQ, RUMSQ),
     *   (SUMWT, RUMWT)
      COMMON /SUMBUF/ SUM, SUMSQ, SUMWT, COUNT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'XXTAB.INC'
      DATA OPTYPS /'RFI ', 'HIST', 'ANTE', 'GAIN', 'TIME', 'VDIF',
     *   'VRFI'/
C-----------------------------------------------------------------------
C                                       opcode
      CALL OGET (INPUT, 'OPTYPE', TYPE, DIM, DUMMY, OPTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
      IF ((OPTYPE.NE.OPTYPS(1)) .AND. (OPTYPE.NE.OPTYPS(2)) .AND.
     *   (OPTYPE.NE.OPTYPS(3)) .AND. (OPTYPE.NE.OPTYPS(4)) .AND.
     *   (OPTYPE.NE.OPTYPS(5)) .AND. (OPTYPE.NE.OPTYPS(6)) .AND.
     *   (OPTYPE.NE.OPTYPS(7)))
     *   OPTYPE = 'TIME'
C                                       data parameters
      CALL UVDGET (UVIN, 'NAXIS', TYPE, DIM, NAXIS, DUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL FSTOKE (INPUT, UVIN, CPOL, DOPOLS, XPOLS, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDFND (UVIN, 2, 'FREQ', INDEX, IERR)
      IF (IERR.NE.0) GO TO 999
      CCHAN = NAXIS(INDEX)
      CALL UVDFND (UVIN, 2, 'IF', INDEX, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.1)) GO TO 999
      CIF = 1
      IF (IERR.EQ.0) CIF = NAXIS(INDEX)
C                                       find maximum antenna number
      TABLE = 'Temporary AN table for FIXWT'
      CANT = 0
      CALL OUVGET (UVIN, 'CALEDIT.SUBARR', TYPE, DIM, ANVER, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      IF (ANVER.GT.0) THEN
         IS1 = ANVER
         IS2 = ANVER
      ELSE
         IS1 = 1
         IS2 = 99
         END IF
      DO 20 ANVER = 1,99
         CALL UV2TAB (UVIN, TABLE, 'AN', ANVER, IERR)
         EXISTS = .FALSE.
         IF (IERR.EQ.0) CALL TABEXI (TABLE, EXISTS, IERR)
         MXA = 0
         IF ((IERR.EQ.0) .AND. (EXISTS)) CALL ANTNO (TABLE, ANVER, MXA,
     *      IERR)
         CALL TABDES (TABLE, IDUMMY)
         IF ((IERR.EQ.0) .AND. (EXISTS)) THEN
            CANT = MAX (CANT, MXA)
         ELSE
            GO TO 30
            END IF
 20      CONTINUE
 30   IF (CANT.LE.0) CANT = MAXANT
C                                       start on history
      CALL OUVGET (UVIN, 'CALEDIT.FGVER', TYPE, DIM, FGVER, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABGET (FGTAB, 'VER', TYPE, DIM, FGOUT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OHTIME (FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1700) FGVER
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1701) FGOUT
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1702) OPTYPE
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'TIMEAVG', TYPE, DIM, SOLINT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (HILINE,1703) SOLINT
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'AMPCLIP', TYPE, DIM, ACLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF ((ACLIP(1).GT.0.0) .OR. (ACLIP(2).LT.1.E5)) THEN
         ACLIP(2) = MIN (9999.,ACLIP(2))
         WRITE (HILINE,1704) 'AMP', ACLIP(1), ACLIP(2)
         CALL OHWRIT (HILINE, FGTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      IF ((ACLIP(3).GT.0.0) .OR. (ACLIP(4).LT.1.E5)) THEN
         ACLIP(4) = MIN (9999.,ACLIP(4))
         WRITE (HILINE,1705) 'AMP', ACLIP(3), ACLIP(4)
         CALL OHWRIT (HILINE, FGTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      CALL OUVGET (UVIN, 'WTCLIP', TYPE, DIM, ACLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF ((ACLIP(1).GT.0.0) .OR. (ACLIP(2).LT.1.E9)) THEN
         ACLIP(2) = MIN (9999.,ACLIP(2))
         WRITE (HILINE,1714) 'WT ', ACLIP(1), ACLIP(2)
         CALL OHWRIT (HILINE, FGTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      IF ((ACLIP(3).GT.0.0) .OR. (ACLIP(4).LT.1.E9)) THEN
         ACLIP(4) = MIN (9999.,ACLIP(4))
         WRITE (HILINE,1715) 'WT ', ACLIP(3), ACLIP(4)
         CALL OHWRIT (HILINE, FGTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      CALL OUVGET (UVIN, 'CLOSCLIP', TYPE, DIM, ACLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF ((ACLIP(1).LT.1.0) .AND. (ACLIP(1).GT.0.0)) THEN
         WRITE (HILINE,1710) 'CLS', ACLIP(1)
         CALL OHWRIT (HILINE, FGTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      IF ((ACLIP(2).LT.1.0) .AND. (ACLIP(2).GT.0.0)) THEN
         WRITE (HILINE,1711) 'CLS', ACLIP(2)
         CALL OHWRIT (HILINE, FGTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      CALL OUVGET (UVIN, 'MAXBAD', TYPE, DIM, ACLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1706) ACLIP(1)
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1707) ACLIP(2)
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      IF ((OPTYPE.NE.'VDIF') .AND. (OPTYPE.NE.'VRFI')) THEN
         CALL OUVGET (UVIN, 'AMPONLY', TYPE, DIM, LTEMP, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         LTEMP = .NOT.LTEMP
         WRITE (HILINE,1708) LTEMP
         CALL OHWRIT (HILINE, FGTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVGET (UVIN, 'KILLONES', TYPE, DIM, LTEMP, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (HILINE,1709) LTEMP
         CALL OHWRIT (HILINE, FGTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C
      NSAMP = CANT * CIF * CPOL
      NCORS = CIF * CPOL
C                                       Interference detection.: RFI
C                                       Bill Cotton - NVSS et al.
      IF ((OPTYPE.EQ.'RFI ') .OR. (OPTYPE.EQ.'HIST')) THEN
         CALL HISRMS (UVIN, PRINT, FGTAB, CANT, CIF, CPOL, CCHAN,
     *      OPTYPE, DOPOLS, XPOLS, SUM, SUMSQ, SUMWT, COUNT, IERR)
C                                       antenna-based RMS
      ELSE IF (OPTYPE.EQ.'ANTE') THEN
         CALL ANTRMS (UVIN, PRINT, FGTAB, CANT, CIF, CPOL, CCHAN,
     *      DOPOLS, XPOLS, SUM, SUMSQ, SUMWT, COUNT, ANTSQ, ANTA, ANTW,
     *      FRA, IERR)
      ELSE IF ((OPTYPE.EQ.'GAIN') .OR. (OPTYPE.EQ.'TIME')) THEN
         XXTYPE = 1
         NTIME = (8 * 7 * MAXIF * MAXANT * MAXANT) / (4 * NSAMP)
         NV = NSAMP * NTIME
         CALL ANTIME (UVIN, PRINT, FGTAB, CANT, CIF, CPOL, CCHAN,
     *      NTIME, OPTYPE, DOPOLS, XPOLS, SUM, SUMSQ, SUMWT, COUNT,
     *      ANTSQ, ANTA, ANTW, FRA, THEBUF(1), THEBUF(NV+1),
     *      THEBUF(2*NV+1), THEBUF(3*NV+1), IERR)
      ELSE IF (OPTYPE.EQ.'VDIF') THEN
         XXTYPE = 2
         NTIME = (8 * 7 * MAXIF * MAXANT * MAXANT) / (4 * NSAMP + NCORS)
         NV = NSAMP * NTIME
         CALL AMVDIF (UVIN, PRINT, FGTAB, CANT, CIF, CPOL, CCHAN,
     *      NTIME, DOPOLS, XPOLS, RUM, RUMSQ, COUNT, RUMWT, ANTSQ, ANTA,
     *      ANTW, FRA, THEBUF, THEBUF(NV+1), THEBUF(2*NV+1),
     *      THEBUF(3*NV+1), THEIUF(4*NV+1), IERR)
      ELSE IF (OPTYPE.EQ.'VRFI') THEN
         NSAMP = CANT * NSAMP
         NTIME = (8 * 7 * MAXIF * MAXANT * MAXANT - NSAMP) / (2 * NSAMP)
         NV = NSAMP * NTIME
         CALL AMVRFI (UVIN, PRINT, FGTAB, CANT, CIF, CPOL, CCHAN,
     *      NTIME, DOPOLS, XPOLS, THEBUF(1), THEBUF(NV+1),
     *      THEBUF(2*NV+1), IERR)
         END IF
C                                       Error
 990  IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR FLAGGING RMS IN ' // UVIN
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1700 FORMAT ('FLAGVER =',I5,5X,'/ input flag version copied')
 1701 FORMAT ('FLAGOUT =',I5,5X,'/ output flag version written')
 1702 FORMAT ('OPTYPE  = ''',A4,'''',4X,'/ operation type')
 1703 FORMAT ('SOLINT  =',F8.3,5X,'/ averaging time in sec')
 1704 FORMAT ('QCLIP',A,'=',F8.5,F10.5,' / question data outside range')
 1714 FORMAT ('QCLIP',A,'=',2(1PE10.3),' / question data outside range')
 1705 FORMAT ('FCLIP',A,'=',F8.5,F10.5,' / flag data outside range')
 1715 FORMAT ('FCLIP',A,'=',2(1PE10.3),' / flag data outside range')
 1706 FORMAT ('MAXBAD(1) =',F6.3,4X,'/ max fract questionable corrs.')
 1707 FORMAT ('MAXBAD(2) =',F6.3,4X,'/ max fract questionable ',
     *   'baselines')
 1708 FORMAT ('VECTOR  =',2X,L1,5X,'/ type of amplitude averaging')
 1709 FORMAT ('KILLONES=',2X,L1,5X,'/ kill or flag singletons')
 1710 FORMAT ('QCLIP',A,'=',F8.5,' / question excess closure error')
 1711 FORMAT ('FCLIP',A,'=',F8.5,' / flag excess closure error')
      END
      SUBROUTINE HISRMS (UVIN, PRINT, FGTAB, CANT, CIF, CPOL, CCHAN,
     *   OPTYPE, DOPOLS, XPOLS, SUM, SUMSQ, SUMWT, COUNT, IERR)
C-----------------------------------------------------------------------
C   Look for interference based on a high real or imaginary rms in time
C   interval TIMEAVG.
C   Inputs:
C      UVIN      C*?   Name of input uvdata object.
C      PRINT     C*?   Print object
C      FGTAB     C*?   Output flag table object
C      CANT      I     Maximum antenna number
C      CPOL      I     Number of polarizations
C      CIF       I     Number of IFs
C      CCHAN     I     Number of spactral channels
C      OPTYPE    C*4   HIST -> use histogram else do not
C      DOPOLS    L(4)  Which polarizations to test
C      XPOLS     L(4,4)  Which pols to flag (i,j) if i bad flag j
C   Scratch variables
C      SUM, SUMSQ, SUMWT, COUNT
C   Inputs attached to UVIN:
C      MAXRMS    R(*)  Maximum RMS allowed, constant plus amplitude
C                      coefficient.
C      MAXBAD    R(2)  Maximum allowed fraction of bad baselines.
C      TIMEAVG   R     Time in seconds for clipping interval
C      MINHIS    R     Histogram clip level must exceed this
C      DOIFS     L     Flag all IFs if one bad
C      AMPONLY   L     Use amplitude not Real/Imag
C      KILLONES  L     Kill 1-sample integrations
C      PRTLEV    I     Print level for debugging
C   Output:
C      IERR      I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), PRINT*(*), FGTAB*(*), OPTYPE*(*)
      INTEGER   CANT, CIF, CPOL, CCHAN, IERR
      INTEGER   COUNT(CANT,CANT,CIF,CPOL,2)
      DOUBLE PRECISION SUM(CANT,CANT,CIF,CPOL,2),
     *   SUMSQ(CANT,CANT,CIF,CPOL,2), SUMWT(CANT,CANT,CIF,CPOL,2)
      LOGICAL   DOPOLS(4), XPOLS(4,4)
C
      REAL      MAXRMS(2,2), MAXBAD(2), TCLIP, MINHIS(2), ACLIP(4),
     *   WCLIP(4)
      LOGICAL   DOIFS, DOROBU, AONLY, KILL1, DOCHNS
      INTEGER   PRTLEV
C
      INTEGER   NHIS
      PARAMETER (NHIS = 800)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   TYPE, DIM(7), DUMMY, ANT1, ANT2, NWORD, ENDVIS, VISNO,
     *   BCORNO, CURSOU, THISOU, TBVER, DOCRT, QUAL, BEGT(4), ENDT(4)
      LOGICAL   BADCOR(4,MAXIF), DONE, DOSOU, QUIT, DOCAL, SCDONE,
     *   GOTDAT(2)
      CHARACTER SUTAB*32, SUNAME*16, LINE*132, CDUMMY*1, REASON*24,
     *   ATIME*8, ADATE*12, HILINE*72
      DOUBLE PRECISION AMP, RMS, RMS2
      REAL      RP(50), MXR21, MXA21, MXR22, MXA22, TEPS,BEGTI2, LSTIM2,
     *   CORCNT(4,MAXIF), CORBAD(4,MAXIF), CURTIM, LSTIME, WORST,
     *   CORSUM(4,MAXIF), ENDTIM, BEGTIM, TEMP, TIMER(2), SIGMA, HISINC,
     *   HICLIP(4,MAXIF), VIS(3,MAXCIF), WEIGHT, WP(2), SP(2), SSP(2)
      LOGICAL   ISBAD, BADRFI, PFLAGS(4), DFLAGS(4), AFLAGS(4)
      INTEGER   ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ,
     *   ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR, JLOCD,
     *   JLOCIF, INCS, INCF, INCIF, LP, LIF, IP, NPRINT, NFLAG1, NFLAG2,
     *   NFLAG3, FGROW, SOURID, SUBA, FREQID, ANTS(2), IFS(2), CHANS(2),
     *   ID(3), IT(3), LL, HISSIG(NHIS,4,MAXIF), THISUB, THIFRQ, NBAD,
     *   CURSUB, CURFRQ, HICELL, NZERO, NP(2)
      INCLUDE 'INCS:DMSG.INC'
      COMMON /VBUFS/ VIS
C-----------------------------------------------------------------------
C                                       Open printer
      TEPS = 0.02 / (3600.0 * 24.0)
      NPRINT = 0
      NFLAG1 = 0
      NFLAG2 = 0
      NFLAG3 = 0
      GOTDAT(1) = .FALSE.
      GOTDAT(2) = .FALSE.
      IF (PRINT.NE.' ') THEN
         CALL OOPEN (PRINT, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (PRINT, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         DOCRT = 0
         END IF
      CALL ZDATE (ID)
      ID(1) = - ABS (ID(1))
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      REASON = TSKNAM // ADATE(:9) // ' ' // ATIME
      IFS(1) = 0
      IFS(2) = 0
      THISOU = 0
      THISUB = 0
      THIFRQ = 0
      CURSUB = 0
      CURFRQ = 0
C                                       Retrieve pointers into the data:
C                                       pointers into the data:
      CALL UVDPNT (UVIN, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IERR)
      IF (IERR.NE.0) GO TO 999
      INCS = INCS / 3
      INCF = INCF / 3
      INCIF = INCIF / 3
C                                       Open input.
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get parameters
      CALL OUVGET (UVIN, 'DOROBUST', TYPE, DIM, DOROBU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'AMPCLIP', TYPE, DIM, ACLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'WTCLIP', TYPE, DIM, WCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'MINHIST', TYPE, DIM, MINHIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'MAXBAD', TYPE, DIM, MAXBAD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'MAXRMS', TYPE, DIM, MAXRMS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'DOCHANS', TYPE, DIM, DOCHNS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'DOIFS', TYPE, DIM, DOIFS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'AMPONLY', TYPE, DIM, AONLY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'KILLONES', TYPE, DIM, KILL1, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'PRTLEV', TYPE, DIM, PRTLEV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      MXR21 = MAXRMS(1,1) * MAXRMS(1,1)
      MXA21 = MAXRMS(2,1) * MAXRMS(2,1)
      MXR22 = MAXRMS(1,2) * MAXRMS(1,2)
      MXA22 = MAXRMS(2,2) * MAXRMS(2,2)
      MINHIS(1) = MINHIS(1) * MINHIS(1)
      MINHIS(2) = MINHIS(2) * MINHIS(2)
      HISINC = 0.01 * MAXRMS(1,1)
      CALL OUVGET (UVIN, 'TIMEAVG', TYPE, DIM, TCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      TCLIP = TCLIP / 86400.0
      IF (.NOT.DOCHNS) THEN
         CALL OUVGET (UVIN, 'CALEDIT.BCHAN', TYPE, DIM, CHANS(1),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVGET (UVIN, 'CALEDIT.ECHAN', TYPE, DIM, CHANS(2),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CHANS(1) = 0
         CHANS(2) = 0
         END IF
C                                       Processing info
      CALL OUVGET (UVIN, 'CALEDIT.DOCAL', TYPE, DIM, DOCAL, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DOCRT.NE.0) THEN
         IF (DOCRT.GT.-3) THEN
            IF (DOCAL) THEN
               LINE = 'Calibration applied'
            ELSE
               LINE = 'NO Calibration applied'
               END IF
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            WRITE (LINE,1000) 'questionable', MAXRMS(1,1), MAXRMS(2,1)
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((ACLIP(1).GT.0.0) .OR. (ACLIP(2).LT.1.E5)) THEN
               WRITE (LINE,1001) 'questionable', ACLIP(1), ACLIP(2)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((WCLIP(1).GT.0.0) .OR. (WCLIP(2).LT.1.E9)) THEN
               WRITE (LINE,1002) 'questionable', WCLIP(1), WCLIP(2)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            WRITE (LINE,1005) MAXBAD
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            WRITE (LINE,1000) 'bad', MAXRMS(1,2), MAXRMS(2,2)
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((ACLIP(3).GT.0.0) .OR. (ACLIP(4).LT.1.E5)) THEN
               WRITE (LINE,1001) 'bad', ACLIP(3), ACLIP(4)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((WCLIP(3).GT.0.0) .OR. (WCLIP(4).LT.1.E9)) THEN
               WRITE (LINE,1002) 'bad', WCLIP(3), WCLIP(4)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            END IF
C                                       Page labels
         IF (PRTLEV.GE.2) THEN
            LINE = 'Individual data warnings and flags'
         ELSE IF (PRTLEV.GE.1) THEN
            LINE = 'Data to be flagged individually'
         ELSE
            LINE = 'Data flagged independent of antenna'
            END IF
         DIM(1) = LEN (LINE)
         DIM(2) = 1
         CALL OPUT (PRINT, 'TITLE1', OOACAR, DIM, DUMMY, LINE, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL PRTWRI (PRINT, LINE, QUIT, IERR)
         IF (QUIT) DOCRT = 0
         IF (IERR.NE.0) GO TO 990
         IF (PRTLEV.GE.1) THEN
            LINE = '        Time range         A1 A2 IF P       Amp' //
     *         '       Weight      RMS'
         ELSE
            LINE = '        Time range         Source           Qual' //
     *         '    Maxrms  (cc)  Corr flags'
            END IF
         CALL OPUT (PRINT, 'TITLE2', OOACAR, DIM, DUMMY, LINE, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (DOCRT.NE.0) CALL PRTWRI (PRINT, LINE, QUIT, IERR)
         IF (QUIT) DOCRT = 0
         IF (IERR.NE.0) GO TO 990
         END IF
      ACLIP(1) = ACLIP(1) * ACLIP(1)
      ACLIP(2) = ACLIP(2) * ACLIP(2)
      ACLIP(3) = ACLIP(3) * ACLIP(3)
      ACLIP(4) = ACLIP(4) * ACLIP(4)
      DOSOU = (ILOCSU.GT.0)
      IERR = 0
C                                       Source table/name
      IF (DOSOU) THEN
         SUTAB = 'SoUrce table for HISRMS'
         TBVER = 1
         CALL UV2TAB (UVIN, SUTAB, 'SU', TBVER, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       For single source use 'OBJECT'
      ELSE
         SUNAME = ' '
         CALL UVDGET (UVIN, 'OBJECT', TYPE, DIM, DUMMY, SUNAME, IERR)
         IF (IERR.NE.0) GO TO 990
         QUAL = 0
         END IF
C                                       Clear accumulators:
      NZERO = CANT * CANT * CIF * CPOL
      CALL DFILL (NZERO, 0.0D0, SUM)
      CALL DFILL (NZERO, 0.0D0, SUMSQ)
      CALL DFILL (NZERO, 0.0D0, SUMWT)
      CALL FILL (NZERO, 0, COUNT)
C                                       Initialize visibility count
      DONE = .FALSE.
      VISNO = 0
C                                       Open FG table
      CALL OFGINI (FGTAB, 'WRIT', FGROW, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Swallow input collecting
C                                       statistics.
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VIS, IERR)
         IF (IERR.LT.0) THEN
            DONE = .TRUE.
            IERR = 0
            END IF
         IF (IERR.GT.0) GO TO 990
         VISNO = VISNO + 1
 110     CURTIM = RP(ILOCT)
         IF (DOSOU) THEN
            CURSOU = RP(ILOCSU) + 0.5
         ELSE
            CURSOU = 0
            END IF
         IF (ILOCFQ.GT.0) CURFRQ = RP(ILOCFQ) + 0.5
C                                       Get antenna numbers
         IF (ILOCB.GT.0) THEN
            ANT1 = (RP(ILOCB) / 256.0) + 0.001
            ANT2 = (RP(ILOCB) - ANT1 * 256) + 0.001
            CURSUB = (RP(ILOCB) - 256 * ANT1 - ANT2) * 100.0 + 1.1
         ELSE
            ANT1 = RP(ILOCA1) + 0.001
            ANT2 = RP(ILOCA2) + 0.001
            CURSUB = RP(ILOCSA) + 0.001
            END IF
C                                       Initial selection.
         IF (VISNO.EQ.1) THEN
            THIFRQ = CURFRQ
            THISUB = CURSUB
            THISOU = CURSOU
            BEGTIM = CURTIM
            ENDTIM = CURTIM + TCLIP
            LSTIME = CURTIM
            END IF
C                                       Finished with data or interval?
         SCDONE = DONE .OR. (CURSOU.NE.THISOU) .OR. (CURSUB.NE.THISUB)
     *      .OR. (CURTIM.GT.LSTIME+TCLIP)
         IF ((SCDONE) .OR. (CURTIM.GT.ENDTIM)) GO TO 150
C                                       Set last vis of interval
         ENDVIS = VISNO
C                                       Set last time
         LSTIME = CURTIM
C                                       Baseline index
         DO 130 LP = 1,CPOL
            IF (.NOT.DOPOLS(LP)) GO TO 130
            DO 125 LIF = 1,CIF
               IP = (LP-1) * INCS + (LIF-1) * INCIF + 1
               CALL AVERAG (CCHAN, VIS(1,IP), INCF, ANT1, ANT2, AONLY,
     *            DOROBU, NP, WP, SP, SSP)
               COUNT(ANT1,ANT2,LIF,LP,1) = COUNT(ANT1,ANT2,LIF,LP,1) +
     *            NP(1)
               SUMWT(ANT1,ANT2,LIF,LP,1) = SUMWT(ANT1,ANT2,LIF,LP,1) +
     *            WP(1)
               SUM(ANT1,ANT2,LIF,LP,1) = SUM(ANT1,ANT2,LIF,LP,1) +
     *            SP(1)
               SUMSQ(ANT1,ANT2,LIF,LP,1) = SUMSQ(ANT1,ANT2,LIF,LP,1) +
     *            SSP(1)
               COUNT(ANT2,ANT1,LIF,LP,1) = COUNT(ANT2,ANT1,LIF,LP,1) +
     *            NP(2)
               SUMWT(ANT2,ANT1,LIF,LP,1) = SUMWT(ANT2,ANT1,LIF,LP,1) +
     *            WP(2)
               SUM(ANT2,ANT1,LIF,LP,1) = SUM(ANT2,ANT1,LIF,LP,1) +
     *            SP(2)
               SUMSQ(ANT2,ANT1,LIF,LP,1) = SUMSQ(ANT2,ANT1,LIF,LP,1) +
     *            SSP(2)
 125           CONTINUE
 130        CONTINUE
         GOTDAT(1) = .TRUE.
C                                       Next vis until done
         IF (.NOT.DONE) GO TO 100
 150     IERR = 0
C                                       Is buffer 1 singleton?
      IF ((SCDONE) .AND. (GOTDAT(2))) THEN
         DO 154 LIF = 1,CIF
            DO 153 LP = 1,CPOL
               DO 152 ANT2 = 1,CANT
                  DO 151 ANT1 = 1,CANT
                     IF (COUNT(ANT1,ANT2,LIF,LP,1).GT.1) GO TO 160
 151                 CONTINUE
 152              CONTINUE
 153           CONTINUE
 154        CONTINUE
C                                       Yes - add into buf 2
         DO 159 LIF = 1,CIF
            DO 158 LP = 1,CPOL
               DO 157 ANT2 = 1,CANT
                  DO 156 ANT1 = 1,CANT
                     COUNT(ANT1,ANT2,LIF,LP,2) =
     *                  COUNT(ANT1,ANT2,LIF,LP,2) +
     *                  COUNT(ANT1,ANT2,LIF,LP,1)
                     SUM(ANT1,ANT2,LIF,LP,2) =
     *                  SUM(ANT1,ANT2,LIF,LP,2) +
     *                  SUM(ANT1,ANT2,LIF,LP,1)
                     SUMSQ(ANT1,ANT2,LIF,LP,2) =
     *                  SUMSQ(ANT1,ANT2,LIF,LP,2) +
     *                  SUMSQ(ANT1,ANT2,LIF,LP,1)
                     SUMWT(ANT1,ANT2,LIF,LP,2) =
     *                  SUMWT(ANT1,ANT2,LIF,LP,2) +
     *                  SUMWT(ANT1,ANT2,LIF,LP,1)
 156                 CONTINUE
 157              CONTINUE
 158           CONTINUE
 159        CONTINUE
         GOTDAT(1) = .FALSE.
         LSTIM2 = MAX (LSTIME, LSTIM2)
         BEGTI2 = MIN (BEGTIM, BEGTI2)
         END IF
C                                       Get statistics
C                                       Re and Im rms are taken as 2
C                                       samples of the amp rms
 160  IF (GOTDAT(2)) THEN
         NWORD = NHIS * 2 * MAXIF
         CALL FILL (NWORD, 0, HISSIG)
         DO 175 ANT1 = 1,CANT
            DO 170 ANT2 = 1,CANT
               DO 165 LIF = 1,CIF
                  DO 161 LP = 1,CPOL
                     IF (SUMWT(ANT1,ANT2,LIF,LP,2).GT.0.0) THEN
                        SUM(ANT1,ANT2,LIF,LP,2) =
     *                     (SUM(ANT1,ANT2,LIF,LP,2)
     *                     / SUMWT(ANT1,ANT2,LIF,LP,2)) ** 2
                        SUMSQ(ANT1,ANT2,LIF,LP,2) =
     *                     SUMSQ(ANT1,ANT2,LIF,LP,2)
     *                     / SUMWT(ANT1,ANT2,LIF,LP,2)
                        END IF
                     IF (COUNT(ANT1,ANT2,LIF,LP,2).GT.1) THEN
                        RMS = SUMSQ(ANT1,ANT2,LIF,LP,2) -
     *                     SUM(ANT1,ANT2,LIF,LP,2)
                        RMS = (RMS * COUNT(ANT1,ANT2,LIF,LP,2)) /
     *                     (COUNT(ANT1,ANT2,LIF,LP,2) - 1.0D0)
                        RMS = ABS (RMS)
                        SUMSQ(ANT1,ANT2,LIF,LP,2) = RMS
                        SUMWT(ANT1,ANT2,LIF,LP,2) =
     *                     SUMWT(ANT1,ANT2,LIF,LP,2) /
     *                     COUNT(ANT1,ANT2,LIF,LP,2)
C                                       histogram
                        SIGMA = SQRT (RMS)
                        HICELL = 1 + SIGMA / HISINC
                        HICELL = MAX (1, MIN (HICELL, NHIS))
                        HISSIG(HICELL,LP,LIF) = HISSIG(HICELL,LP,LIF) +
     *                     1
C                                       singleton
                     ELSE IF (COUNT(ANT1,ANT2,LIF,LP,2).EQ.1) THEN
                        SUMSQ(ANT1,ANT2,LIF,LP,2) = 0.0
                        HISSIG(1,LP,LIF) = HISSIG(1,LP,LIF) + 1
                        END IF
 161                 CONTINUE
 165              CONTINUE
 170           CONTINUE
 175        CONTINUE
C                                       Inhibit histogram flagging
         IF (OPTYPE.NE.'HIST') THEN
            TEMP = 1.E20
            NWORD = 4 * MAXIF
            CALL RFILL (NWORD, TEMP, HICLIP)
C                                       Set histogram clipping levels
         ELSE
            CALL FLCLHI (PRTLEV, CPOL, CIF, NHIS, HISINC, HISSIG,
     *         MINHIS(1), HICLIP)
            END IF
C                                       apply
         CALL TODHMS (BEGTI2, BEGT)
         CALL TODHMS (LSTIM2, ENDT)
         NWORD = 4 * MAXIF
         CALL RFILL (NWORD, 0.0, CORCNT)
         CALL RFILL (NWORD, 0.0, CORBAD)
         CALL RFILL (NWORD, 0.0, CORSUM)
         CALL LFILL (NWORD, .FALSE., BADCOR)
         DO 200 ANT1 = 1,CANT
            DO 195 ANT2 = ANT1,CANT
               DO 190 LIF = 1,CIF
                  DO 185 LP = 1,CPOL
                     IF (COUNT(ANT1,ANT2,LIF,LP,2).GT.0) THEN
C                                       Real part
                        RMS = SUMSQ(ANT1,ANT2,LIF,LP,2)
                        AMP = SUM(ANT1,ANT2,LIF,LP,2)
                        WEIGHT = SUMWT(ANT1,ANT2,LIF,LP,2)
C                                       Imaginary part
                        IF ((ANT1.NE.ANT2) .AND. (.NOT.AONLY)) THEN
                           AMP = AMP + SUM(ANT2,ANT1,LIF,LP,2)
                           RMS = (RMS + SUMSQ(ANT2,ANT1,LIF,LP,2)) / 2.0
                           END IF
C                                       maximum rms**2
                        RMS2 = MXR21 + (MXA21*AMP)
                        IF (RMS2.GT.HICLIP(LP,LIF)) RMS2 =
     *                     HICLIP(LP,LIF)
C                                       Is this one bad?
                        ISBAD = RMS.GT.RMS2
                        ISBAD = ISBAD .OR. AMP.LE.ACLIP(1)
                        ISBAD = ISBAD .OR. AMP.GT.ACLIP(2)
                        ISBAD = ISBAD .OR. WEIGHT.LE.WCLIP(1)
                        ISBAD = ISBAD .OR. WEIGHT.GT.WCLIP(2)
C                                       Correlator info
                        CORCNT(LP,LIF) = CORCNT(LP,LIF) + 1.0
                        IF (ISBAD) THEN
                           RMS = SQRT (RMS)
                           CORBAD(LP,LIF)= CORBAD(LP,LIF) + 1.0
                           CORSUM(LP,LIF)= CORSUM(LP,LIF) + RMS
                           IF ((PRTLEV.GE.2) .AND. (DOCRT.NE.0)) THEN
                              AMP = SQRT (AMP)
                              WRITE (LINE,1170) BEGT, ENDT, ANT1, ANT2,
     *                           LIF, LP, AMP, WEIGHT, RMS
                              CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                              IF (QUIT) DOCRT = 0
                              IF (IERR.NE.0) GO TO 990
                              END IF
                           END IF
                        END IF
 185                 CONTINUE
 190              CONTINUE
 195           CONTINUE
 200        CONTINUE
C                                       Check for bad correlators
         BADRFI = .FALSE.
         NBAD = 0
         WORST = 0.0
         DO 210 LIF = 1,CIF
            DO 205 LP = 1,CPOL
               IF (CORCNT(LP,LIF).GT.1.1) THEN
C                                       Kill correlator
                  IF ((CORBAD(LP,LIF)/CORCNT(LP,LIF)).GT.MAXBAD(2)) THEN
                     BADCOR(LP,LIF) = .TRUE.
                     BADRFI = .TRUE.
                     NBAD = NBAD + 1
                     RMS2 = CORSUM(LP,LIF) / CORBAD(LP,LIF)
                     IF (WORST.LT.RMS2) THEN
                        WORST = RMS2
                        BCORNO = (LIF-1) * CPOL + LP
                        END IF
                     END IF
                  END IF
 205           CONTINUE
 210        CONTINUE
C                                       If bad tell about it
         IF ((BADRFI) .AND. (DOCRT.NE.0)) THEN
C                                       Get source name, qualifier
            IF (DOSOU) THEN
               CALL SOUNFO (SUTAB, THISOU, 'SOURCE', TYPE, DIM, DUMMY,
     *            SUNAME, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL SOUNFO (SUTAB, THISOU, 'QUAL', TYPE, DIM, QUAL,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            WRITE (LINE,1210) BEGT, ENDT, SUNAME, QUAL, WORST, BCORNO
            IP = 66
            DO 215 LIF = 1,CIF
               DO 214 LP = 1,CPOL
                  IP = IP + 1
                  IF (IP.LE.132) THEN
                     IF (BADCOR(LP,LIF)) THEN
                        LINE(IP:IP) = 'T'
                     ELSE IF (CORCNT(LP,LIF).LE.0) THEN
                        LINE(IP:IP) = '-'
                     ELSE
                        LINE(IP:IP) = 'F'
                        END IF
                     END IF
 214              CONTINUE
 215           CONTINUE
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) GO TO 990
            END IF
         IF (BADRFI) NPRINT = NPRINT + 1
C                                       Flagging: everything
         TEMP = CIF * CPOL
         TEMP = NBAD / TEMP
         IF (TEMP.GT.MAXBAD(1)) THEN
            NFLAG1 = NFLAG1 + 1
            PFLAGS(1) = .TRUE.
            PFLAGS(2) = .TRUE.
            PFLAGS(3) = .TRUE.
            PFLAGS(4) = .TRUE.
            SOURID = THISOU
            SUBA = THISUB
            FREQID = THIFRQ
            IFS(1) = 0
            IFS(2) = 0
            TIMER(1) = BEGTI2 - TEPS
            TIMER(2) = LSTIM2 + TEPS
            ANTS(1) = 0
            ANTS(2) = 0
            CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID, SUBA, FREQID,
     *         ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IERR)
            IF (IERR.NE.0) GO TO 990
C                                       an IF at a time
         ELSE
            DFLAGS(1) = .FALSE.
            DFLAGS(2) = .FALSE.
            DFLAGS(3) = .FALSE.
            DFLAGS(4) = .FALSE.
            DO 250 LIF = 1,CIF
               PFLAGS(1) = BADCOR(1,LIF)
               PFLAGS(2) = BADCOR(2,LIF)
               PFLAGS(3) = BADCOR(3,LIF)
               PFLAGS(4) = BADCOR(4,LIF)
               AFLAGS(1) = .FALSE.
               AFLAGS(2) = .FALSE.
               AFLAGS(3) = .FALSE.
               AFLAGS(4) = .FALSE.
               IF ((PFLAGS(1)) .OR. (PFLAGS(2)) .OR. (PFLAGS(3)) .OR.
     *            (PFLAGS(4))) THEN
                  SOURID = THISOU
                  SUBA = THISUB
                  FREQID = THIFRQ
C                                       cross flags
                  DO 220 LP = 1,4
                     DO 219 LL = 1,4
                        IF (PFLAGS(LL)) AFLAGS(LP) = AFLAGS(LP) .OR.
     *                     XPOLS(LL,LP)
 219                    CONTINUE
 220                 CONTINUE
                  IF (.NOT.DOIFS) THEN
                     IFS(1) = LIF
                     IFS(2) = LIF
                     END IF
                  TIMER(1) = BEGTI2 - TEPS
                  TIMER(2) = LSTIM2 + TEPS
                  ANTS(1) = 0
                  ANTS(2) = 0
                  CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID, SUBA,
     *               FREQID, ANTS, TIMER, IFS, CHANS, AFLAGS, REASON,
     *               IERR)
                  IF (IERR.NE.0) GO TO 990
                  NFLAG2 = NFLAG2 + 1
                  IF (DOIFS) THEN
                     IF (AFLAGS(1)) DFLAGS(1) = .TRUE.
                     IF (AFLAGS(2)) DFLAGS(2) = .TRUE.
                     IF (AFLAGS(3)) DFLAGS(3) = .TRUE.
                     IF (AFLAGS(4)) DFLAGS(4) = .TRUE.
                     IF ((DFLAGS(1)) .AND. (DFLAGS(2)) .AND. (DFLAGS(3))
     *                  .AND. (DFLAGS(4))) GO TO 300
                     END IF
                  END IF
C                                       one baseline at a time
               IF ((.NOT.AFLAGS(1)) .OR. (.NOT.AFLAGS(2)) .OR.
     *            (.NOT.AFLAGS(3)) .OR. (.NOT.AFLAGS(4))) THEN
                  DO 245 ANT1 = 1,CANT
                     DO 240 ANT2 = ANT1,CANT
                        DO 230 LP = 1,CPOL
                           PFLAGS(LP) = .FALSE.
                           IF (COUNT(ANT1,ANT2,LIF,LP,2).GT.0) THEN
                              AMP = SUM(ANT1,ANT2,LIF,LP,2)
                              RMS = SUMSQ(ANT1,ANT2,LIF,LP,2)
                              WEIGHT = SUMWT(ANT1,ANT2,LIF,LP,2)
                              IF ((ANT1.NE.ANT2) .AND. (.NOT.AONLY))
     *                           THEN
                                 AMP = AMP + SUM(ANT2,ANT1,LIF,LP,2)
                                 RMS = (RMS + SUMSQ(ANT2,ANT1,LIF,LP,2))
     *                              / 2.0
                                 END IF
                              PFLAGS(LP) = PFLAGS(LP) .OR.
     *                           AMP.LE.ACLIP(3)
                              PFLAGS(LP) = PFLAGS(LP) .OR.
     *                           AMP.GT.ACLIP(4)
                              PFLAGS(LP) = PFLAGS(LP) .OR.
     *                           WEIGHT.LE.WCLIP(3)
                              PFLAGS(LP) = PFLAGS(LP) .OR.
     *                           WEIGHT.GT.WCLIP(4)
                              TEMP = MXR22 + MXA22*AMP
                              RMS2 = MIN (TEMP, MAX (HICLIP(LP,LIF),
     *                           MINHIS(2)))
                              PFLAGS(LP) = PFLAGS(LP) .OR. RMS.GT.RMS2
                              IF (COUNT(ANT1,ANT2,LIF,LP,2).EQ.1)
     *                           PFLAGS(LP) = KILL1
                              IF ((PRTLEV.GE.1) .AND. (DOCRT.NE.0) .AND.
     *                           (PFLAGS(LP))) THEN
                                 AMP = SQRT (AMP)
                                 RMS = SQRT (RMS)
                                 WRITE (LINE,1250) BEGT, ENDT, ANT1,
     *                              ANT2, LIF, LP, AMP, WEIGHT, RMS
                                 CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                                 IF (QUIT) DOCRT = 0
                                 IF (IERR.NE.0) GO TO 990
                                 END IF
                              END IF
 230                       CONTINUE
                        IF ((PFLAGS(1)) .OR. (PFLAGS(2)) .OR.
     *                     (PFLAGS(3)) .OR. (PFLAGS(4))) THEN
                           SOURID = THISOU
                           SUBA = THISUB
                           FREQID = THIFRQ
                           DO 235 LP = 1,4
                              AFLAGS(LP) = .FALSE.
                              DO 234 LL = 1,4
                                 IF (PFLAGS(LL)) AFLAGS(LP) = AFLAGS(LP)
     *                              .OR. XPOLS(LL,LP)
 234                             CONTINUE
 235                          CONTINUE
                           IF (.NOT.DOIFS) THEN
                              IFS(1) = LIF
                              IFS(2) = LIF
                              END IF
                           TIMER(1) = BEGTI2 - TEPS
                           TIMER(2) = LSTIM2 + TEPS
                           ANTS(1) = ANT1
                           ANTS(2) = ANT2
                           CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID,
     *                        SUBA, FREQID, ANTS, TIMER, IFS, CHANS,
     *                        AFLAGS, REASON, IERR)
                           IF (IERR.NE.0) GO TO 990
                           NFLAG3 = NFLAG3 + 1
                           END IF
 240                    CONTINUE
 245                 CONTINUE
                  END IF
 250           CONTINUE
            END IF
         GOTDAT(2) = .FALSE.
         END IF
C                                       move data to buf 2
      IF (GOTDAT(1)) THEN
         CALL DPCOPY (NZERO, SUM(1,1,1,1,1), SUM(1,1,1,1,2))
         CALL DPCOPY (NZERO, SUMSQ(1,1,1,1,1), SUMSQ(1,1,1,1,2))
         CALL DPCOPY (NZERO, SUMWT(1,1,1,1,1), SUMWT(1,1,1,1,2))
         CALL COPY (NZERO, COUNT(1,1,1,1,1), COUNT(1,1,1,1,2))
         GOTDAT(1) = .FALSE.
         GOTDAT(2) = .TRUE.
         BEGTI2 = BEGTIM
         LSTIM2 = LSTIME
         END IF
C                                       clear buffers if scan done
      IF ((SCDONE) .AND. (GOTDAT(2))) GO TO 160
C                                       Clear accumulators:
 300  CALL DFILL (NZERO, 0.0D0, SUM)
      CALL DFILL (NZERO, 0.0D0, SUMSQ)
      CALL DFILL (NZERO, 0.0D0, SUMWT)
      CALL FILL (NZERO, 0, COUNT)
C                                       Another clipping interval?
      IF (.NOT.DONE) THEN
         THISOU = CURSOU
         BEGTIM = CURTIM
         ENDTIM = CURTIM + TCLIP
         THIFRQ = CURFRQ
         THISUB = CURSUB
         LSTIME = CURTIM
         GO TO 110
         END IF
C                                       close FG table
      CALL TABCLO (FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       history
      WRITE (HILINE,1700) MAXRMS(1,1), MAXRMS(2,1)
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1701) MAXRMS(1,2), MAXRMS(2,2)
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (OPTYPE.EQ.'HIST') THEN
         WRITE (HILINE,1702) MINHIS(1)
         CALL OHWRIT (HILINE, FGTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (HILINE,1703) MINHIS(2)
         CALL OHWRIT (HILINE, FGTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      WRITE (HILINE,1711) NFLAG1
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1712) NFLAG2
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1713) NFLAG3
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close file
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete temporary table
      IF (DOSOU) THEN
         CALL DESTRY (SUTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       report
      NBAD = NFLAG1 + NFLAG2 + NFLAG3
      WRITE (MSGTXT,1800) NPRINT, NBAD
      CALL MSGWRT (5)
      IF (NBAD.GT.0) THEN
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1801) NFLAG1
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1802) NFLAG2
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1803) NFLAG3
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         IERR = 0
         END IF
C                                       Close printer
      IF (PRINT.NE.' ') THEN
         CALL OCLOSE (PRINT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'HISRMS: ERROR IN RFI DETECTION FOR ' // UVIN
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('For ''',A,''' data: rms clipping coef.=',F8.4,F7.4)
 1001 FORMAT ('For ''',A,''' data: amp clipping range.=',F8.4,F11.4)
 1002 FORMAT ('For ''',A,''' data: wt clipping range.=',2(1PE10.3))
 1005 FORMAT ('Max. fract. questionable corrs.=',F6.3,'  baselines=',
     *   F6.3)
 1170 FORMAT (I3,'/',2(I2.2,':'),I2.2,' -',I3,'/',2(I2.2,':'),I2.2,
     *   3I3,I2,' ??? ',3(1PE11.3))
 1210 FORMAT (I3,'/',2(I2.2,':'),I2.2,' -',I3,'/',2(I2.2,':'),I2.2,1X,
     *   A,':',I5.5,1X,F10.5,'(',I2.2,')')
 1250 FORMAT (I3,'/',2(I2.2,':'),I2.2,' -',I3,'/',2(I2.2,':'),I2.2,
     *   3I3,I2,' bad ',3(1PE11.3))
 1700 FORMAT ('MAXRMS(1)=',F8.4,',',F7.4,' / rms Jy,gain for',
     *   ' questionable')
 1701 FORMAT ('MAXRMS(2)=',F7.4,',',F6.4,' / rms Jy,gain for',
     *   ' bad baseline')
 1702 FORMAT ('MINHIS(1)=',F8.4,5X,'/ Lowest histogram clip for ???')
 1703 FORMAT ('MINHIS(2)=',F8.4,5X,'/ Lowest histogram clip for flag')
 1711 FORMAT ('FLAGD(1) =',I8,' / integrations fully flagged')
 1712 FORMAT ('FLAGD(2) =',I8,' / corr/integrations fully flagged')
 1713 FORMAT ('FLAGD(3) =',I8,' / baselines/integrations flagged')
 1800 FORMAT ('Found',I6,' bad times,',I6,' total flags')
 1801 FORMAT ('Wrote',I6,' antenna & IF independent flags')
 1802 FORMAT ('Wrote',I6,' antenna independent, IF dependent flags')
 1803 FORMAT ('Wrote',I6,' antenna & IF dependent flags')
      END
      SUBROUTINE AVERAG (NC, VIS, INCF, ANT1, ANT2, AONLY, DOROBU,
     *   NP, WP, SP, SSP)
C-----------------------------------------------------------------------
C   AVERAG does normal or robust average of a spectrum
C   Inputs:
C      NC       I        Number channels
C      VIS      R(3,*)   Visibilities
C      INCF     I        Increment between frequencies in vis
C      ANT1     I        Antenna 1 number
C      ANT2     I        Antenna 2 number
C      AONLY    L        Do amplitude only
C      DOROBU   L        Do robust average
C   Outputs:
C      NP       I(2)     Count of included samples
C      WP       R(2)     Sum of weights
C      SP       R(2)     weighted sum (amp or Re/Im)
C      SSP      R(2)     weighted sum square (of amp or of Re/Im)
C-----------------------------------------------------------------------
      INTEGER   NC, INCF, ANT1, ANT2, NP(2)
      REAL      VIS(3,*), WP(2), SP(2), SSP(2)
      LOGICAL   AONLY, DOROBU
C
      INTEGER   I, J, ITER, NITER, NR, NI
      REAL      W, FACT(7)
      DOUBLE PRECISION SUMR, SUMI, SUMRS, SUMIS, WR, WI, AVR, AVI, RMR,
     *   RMI, AMP
      DATA FACT /6.0, 5.0, 4.0, 3.5, 3.1, 2.7, 3.0/
C-----------------------------------------------------------------------
      NITER = 1
      IF (DOROBU) NITER = 7
C                                       iterate
      AVR = 0.0D0
      AVI = 0.0D0
      RMR = 1.0D10
      RMI = 1.0D10
      IF ((AONLY) .OR. (ANT1.EQ.ANT2)) THEN
         DO 20 ITER = 1,NITER
            NR = 0
            SUMR = 0.0D0
            SUMRS = 0.0D0
            WR = 0.0D0
            J = 1 - INCF
            DO 10 I = 1,NC
               J = J + INCF
               W = VIS(3,J)
               IF (W.GT.0) THEN
                  IF (AONLY) THEN
                     AMP = SQRT (VIS(1,J)*VIS(1,J) + VIS(2,J)*VIS(2,J))
                  ELSE
                     AMP = VIS(1,J)
                     END IF
                  IF (ABS(AMP-AVR).LT.RMR) THEN
                     NR = NR + 1
                     WR = WR + W
                     SUMR = SUMR + W * AMP
                     SUMRS = SUMRS + W * AMP * AMP
                     END IF
                  END IF
 10            CONTINUE
            IF (NR.GT.0) THEN
               AVR = SUMR / WR
               RMR = SUMRS / WR - AVR * AVR
               RMR = SQRT (MAX (0.0D0, RMR)) * FACT(ITER)
               END IF
            IF ((NR.LT.3) .OR. (RMR.LE.0.0D0)) GO TO 25
 20         CONTINUE
 25      NP(1) = NR
         NP(2) = 0
         WP(1) = WR
         WP(2) = 0.
         SP(1) = SUMR
         SP(2) = 0.
         SSP(1) = SUMRS
         SSP(2) = 0.
      ELSE
         DO 40 ITER = 1,NITER
            NR = 0
            NI = 0
            SUMR = 0.0D0
            SUMI = 0.0D0
            SUMRS = 0.0D0
            SUMIS = 0.0D0
            WR = 0.0D0
            WI = 0.0D0
            J = 1 - INCF
            DO 30 I = 1,NC
               J = J + INCF
               W = VIS(3,J)
               IF (W.GT.0) THEN
                  IF (ABS(VIS(1,J)-AVR).LT.RMR) THEN
                     NR = NR + 1
                     WR = WR + W
                     SUMR = SUMR + W * VIS(1,J)
                     SUMRS = SUMRS + W * VIS(1,J) * VIS(1,J)
                     END IF
                  IF (ABS(VIS(2,J)-AVI).LT.RMI) THEN
                     NI = NI + 1
                     WI = WI + W
                     SUMI = SUMI + W * VIS(2,J)
                     SUMIS = SUMIS + W * VIS(2,J) * VIS(2,J)
                     END IF
                  END IF
 30            CONTINUE
            IF ((NR.GT.0) .AND. (NI.GT.0)) THEN
               AVR = SUMR / WR
               RMR = SUMRS / WR - AVR * AVR
               RMR = SQRT (MAX (0.0D0, RMR)) * FACT(ITER)
               AVI = SUMI / WI
               RMI = SUMIS / WI - AVI * AVI
               RMI = SQRT (MAX (0.0D0, RMI)) * FACT(ITER)
               END IF
            IF ((NR.LT.3) .OR. (RMR.LE.0.0D0)) GO TO 45
            IF ((NI.LT.3) .OR. (RMI.LE.0.0D0)) GO TO 45
 40         CONTINUE
 45      NP(1) = NR
         NP(2) = NI
         WP(1) = WR
         WP(2) = WI
         SP(1) = SUMR
         SP(2) = SUMI
         SSP(1) = SUMRS
         SSP(2) = SUMIS
         END IF
C
 999  RETURN
      END
      SUBROUTINE FLCLHI (PRTLEV, CPOL, CIF, MXHCEL, HISINC, HISSIG,
     *   MINHIS, HICLIP)
C-----------------------------------------------------------------------
C   Determine minimum clipping levels based on histogram of baseline
C   RMSes.  The clipping level is the median RMS plus 3 times the width
C   of the RMS distribution or the point which removes 6 percent of the
C   baselines whichever is greater.
C   Inputs:
C      PRTLEV   I   Print messages if > 0
C      CPOL     I   Number of polarizations
C      CIF      I   Number of IFs
C      MXHCEL   I   Number of cells in each histogram
C      HISINC   R   Increment in histogram
C   Input/output
C      HISSIG   I(*,*) (i,j) = cell i, correlator j; on output the
C                   histogram is integrated.
C   Output
C      HICLIP   R(*)  Clipping level in amp**2 units per correlator.
C-----------------------------------------------------------------------
      INTEGER   PRTLEV, CPOL, CIF, MXHCEL, HISSIG(MXHCEL,4,*)
      REAL      HISINC, MINHIS, HICLIP(4,*)
C
      INTEGER   LP, LIF, COUNT, I, J, IHALF, I6, I16, I56
      REAL      RHALF, R6, R16, R56, SIG, CLEV
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (CIF*CPOL.EQ.0) GO TO 999
C                                       Loop over correlator
      DO 100 LIF = 1,CIF
         DO 90 LP = 1,CPOL
C                                       Integrate histogram
            COUNT = 0
            DO 50 I = 1,MXHCEL
               J = MXHCEL - I + 1
               COUNT = COUNT + HISSIG(J,LP,LIF)
               HISSIG(J,LP,LIF) = COUNT
 50            CONTINUE
C                                       Determine levels
            RHALF = 0.5 * COUNT
            R6 = 0.06 * COUNT
            R16 = COUNT * (1.0 / 6.0)
            R56 = COUNT * (5.0 / 6.0)
            IHALF = 1
            I6 = 1
            I16 = 1
            I56 = 2
            DO 60 I = 1,MXHCEL
               IF (HISSIG(I,LP,LIF).GT.RHALF) IHALF = I
               IF (HISSIG(I,LP,LIF).GT.R6) I6 = I
               IF (HISSIG(I,LP,LIF).GT.R16) I16 = I
               IF (HISSIG(I,LP,LIF).GT.R56) I56 = I
 60            CONTINUE
C                                       Compute sigma of distribution
            SIG = 0.5 * (I16 - I56) * HISINC
C                                       Set clip level
            CLEV = MAX ((I6*HISINC), (IHALF*HISINC+3.0*SIG))
C                                       Trap histogram missing
            IF (IHALF.GE.(MXHCEL-2)) CLEV = 1000.0
            IF (COUNT.LT.4) CLEV = 1000.0
C                                       Return square of level
            HICLIP(LP,LIF) = MAX (CLEV * CLEV, MINHIS)
C                                       If the histogram has run off the
C                                       high end return a large value
            IF ((I6.GE.MXHCEL) .OR. (IHALF.GE.MXHCEL))
     *         HICLIP(LP,LIF) = 100.0
C                                       Tell result
            IF (PRTLEV.GE.4) THEN
               WRITE (MSGTXT,1100) LP, LIF, CLEV*1000.0
               CALL MSGWRT (3)
               END IF
 90         CONTINUE
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Histogram clip level, corr.',I2,I3,' is ',F13.3,
     *   ' mJy RMS')
      END
      SUBROUTINE ANTRMS (UVIN, PRINT, FGTAB, CANT, CIF, CPOL, CCHAN,
     *   DOPOLS, XPOLS, SUM, SUMSQ, SUMWT, COUNT, ANTSQ, ANTA, ANTW,
     *   FRA, IERR)
C-----------------------------------------------------------------------
C   Look for interference based on a high real or imaginary rms in time
C   interval TIMEAVG.
C   Inputs:
C      UVIN      C*?   Name of input uvdata object.
C      PRINT     C*?   Print object
C      FGTAB     C*?   Output flag table object
C      CANT      I     Maximum antenna number
C      CPOL      I     Number of polarizations
C      CIF       I     Number of IFs
C      CCHAN     I     Number of spactral channels
C      DOPOLS    L(4)  Which polarizations to test
C      XPOLS     L(4,4)  Which pols to flag (i,j) if i bad flag j
C   Scratch variables
C      SUM, SUMSQ, SUMWT, COUNT, ANTSQ, ANTA, ANTW
C   Inputs attached to UVIN:
C      MAXRMS    R(*)  Maximum RMS allowed, constant plus amplitude
C                      coefficient.
C      MAXBAD    R(2)  Maximum allowed fraction of bad baselines.
C      TIMEAVG   R     Time in seconds for clipping interval
C      DOIFS     L     Flag all IFs if one bad
C      AMPONLY   L     Use amplitude not Real/Imag
C      KILLONES  L     Kill 1-sample integrations
C      PRTLEV    I     Print level for debugging
C   Output:
C      IERR      I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), PRINT*(*), FGTAB*(*)
      INTEGER   CANT, CIF, CPOL, CCHAN, IERR
      INTEGER   COUNT(CANT,CANT,CIF,CPOL,2)
      DOUBLE PRECISION SUM(CANT,CANT,CIF,CPOL,2),
     *   SUMSQ(CANT,CANT,CIF,CPOL,2), SUMWT(CANT,CANT,CIF,CPOL,2)
      REAL      ANTSQ(CANT,CIF,CPOL), ANTA(CANT,CIF,CPOL),
     *   ANTW(CANT,CIF,CPOL), FRA(CANT,CIF,CPOL)
      LOGICAL   DOPOLS(4), XPOLS(4,4)
C
      REAL      MAXRMS(2,2), MAXBAD(2), TCLIP, ACLIP(4), WCLIP(4),
     *   CLCLIP(2)
      LOGICAL   DOIFS, DOROBU, AONLY, KILL1, DOCHNS
      INTEGER   PRTLEV
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LMAX
      PARAMETER (LMAX = (MAXANT*MAXANT))
      INTEGER   TYPE, DIM(7), DUMMY, ANT1, ANT2, NWORD, KK,
     *   ENDVIS, VISNO, BCORNO,CURSOU, THISOU, TBVER,DOCRT, QUAL,
     *   BEGT(4), ENDT(4), NACROS, BADANT(4,MAXIF,MAXANT)
      LOGICAL   BADCOR(4,MAXIF),  DONE, DOSOU, QUIT, DOCAL, SCDONE,
     *   GOTDAT(2)
      CHARACTER SUTAB*32, SUNAME*16, LINE*132, CDUMMY*1, REASON*24,
     *   ATIME*8, ADATE*12, HILINE*72
      DOUBLE PRECISION AMP, RMS
      REAL      RP(50), MXR21, MXA21, MXR22, MXA22, TEPS, WORST,
     *   CORCNT(4,MAXIF), CORBAD(4,MAXIF), CURTIM, LSTIME, WORSTM,
     *   CORSUM(4,MAXIF), ENDTIM, BEGTIM, TEMP, TIMER(2), BEGTI2,
     *   VIS(3,MAXCIF), CORWOR(4,MAXIF), WEIGHT, FRAC(MAXANT), LSTIM2,
     *   LSUM(LMAX), LSUMSQ(LMAX), LSUMWT(LMAX), WP(2), SP(2), SSP(2)
      LOGICAL   ISBAD, BADRFI, PFLAGS(4), DFLAGS(4), AFLAGS(4)
      INTEGER   ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ,
     *   ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR, JLOCD,
     *   JLOCIF, INCS, INCF, INCIF, LP, LIF, IP, NPRINT, NFLAG1, NFLAG2,
     *   NFLAG3, FGROW, SOURID, SUBA, FREQID, ANTS(2), IFS(2), CHANS(2),
     *   ID(3), IT(3), THISUB, THIFRQ, NBAD, CURSUB, CURFRQ, NZERO, LL,
     *   NP(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /VBUFS/ VIS
C-----------------------------------------------------------------------
C                                       Open printer
      TEPS = 0.02 / (3600.0 * 24.0)
      GOTDAT(1) = .FALSE.
      GOTDAT(2) = .FALSE.
      NPRINT = 0
      NFLAG1 = 0
      NFLAG2 = 0
      NFLAG3 = 0
      IF (PRINT.NE.' ') THEN
         CALL OOPEN (PRINT, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (PRINT, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (PRINT, 'NACROS', TYPE, DIM, NACROS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         DOCRT = 0
         END IF
      CALL ZDATE (ID)
      ID(1) = - ABS (ID(1))
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      REASON = TSKNAM // ADATE(:9) // ' ' // ATIME
      IFS(1) = 0
      IFS(2) = 0
      THISOU = 0
      THISUB = 0
      THIFRQ = 0
      CURSUB = 0
      CURFRQ = 0
C                                       Retrieve pointers into the data:
C                                       pointers into the data:
      CALL UVDPNT (UVIN, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IERR)
      IF (IERR.NE.0) GO TO 999
      INCS = INCS / 3
      INCF = INCF / 3
      INCIF = INCIF / 3
C                                       Open input.
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get parameters
      CALL OUVGET (UVIN, 'DOROBUST', TYPE, DIM, DOROBU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'CLOSCLIP', TYPE, DIM, CLCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'WTCLIP', TYPE, DIM, WCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'AMPCLIP', TYPE, DIM, ACLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'MAXBAD', TYPE, DIM, MAXBAD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'MAXRMS', TYPE, DIM, MAXRMS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'DOCHANS', TYPE, DIM, DOCHNS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'DOIFS', TYPE, DIM, DOIFS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'AMPONLY', TYPE, DIM, AONLY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'KILLONES', TYPE, DIM, KILL1, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'PRTLEV', TYPE, DIM, PRTLEV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      MXR21 = MAXRMS(1,1) * MAXRMS(1,1)
      MXA21 = MAXRMS(2,1) * MAXRMS(2,1)
      MXR22 = MAXRMS(1,2) * MAXRMS(1,2)
      MXA22 = MAXRMS(2,2) * MAXRMS(2,2)
      CALL OUVGET (UVIN, 'TIMEAVG', TYPE, DIM, TCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      TCLIP = TCLIP / 86400.0
      IF (.NOT.DOCHNS) THEN
         CALL OUVGET (UVIN, 'CALEDIT.BCHAN', TYPE, DIM, CHANS(1),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVGET (UVIN, 'CALEDIT.ECHAN', TYPE, DIM, CHANS(2),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CHANS(1) = 0
         CHANS(2) = 0
         END IF
C                                       Processing info
      CALL OUVGET (UVIN, 'CALEDIT.DOCAL', TYPE, DIM, DOCAL, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DOCRT.NE.0) THEN
         IF (DOCRT.GT.-3) THEN
            IF (DOCAL) THEN
               LINE = 'Calibration applied'
            ELSE
               LINE = 'NO Calibration applied'
               END IF
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            WRITE (LINE,1000) 'questionable', MAXRMS(1,1), MAXRMS(2,1)
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((ACLIP(1).GT.0.0) .OR. (ACLIP(2).LT.1.E5)) THEN
               WRITE (LINE,1001) 'questionable', ACLIP(1), ACLIP(2)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((WCLIP(1).GT.0.0) .OR. (WCLIP(2).LT.1.E9)) THEN
               WRITE (LINE,1002) 'questionable', WCLIP(1), WCLIP(2)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((CLCLIP(1).GT.0.0) .AND. (CLCLIP(1).LT.1.0)) THEN
               WRITE (LINE,1003) 'questionable', CLCLIP(1)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            WRITE (LINE,1005) MAXBAD
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            WRITE (LINE,1000) 'bad', MAXRMS(1,2), MAXRMS(2,2)
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((ACLIP(3).GT.0.0) .OR. (ACLIP(4).LT.1.E5)) THEN
               WRITE (LINE,1001) 'bad', ACLIP(3), ACLIP(4)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((WCLIP(3).GT.0.0) .OR. (WCLIP(4).LT.1.E9)) THEN
               WRITE (LINE,1002) 'bad', WCLIP(3), WCLIP(4)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((CLCLIP(2).GT.0.0) .AND. (CLCLIP(2).LT.1.0)) THEN
               WRITE (LINE,1003) 'bad', CLCLIP(2)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            END IF
C                                       Page labels
         IF (PRTLEV.GE.2) THEN
            LINE = 'Individual data warnings and flags'
         ELSE IF (PRTLEV.GE.1) THEN
            LINE = 'Data to be flagged individually'
         ELSE
            LINE = 'Data flagged independent of antenna'
            END IF
         DIM(1) = LEN (LINE)
         DIM(2) = 1
         CALL OPUT (PRINT, 'TITLE1', OOACAR, DIM, DUMMY, LINE, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL PRTWRI (PRINT, LINE, QUIT, IERR)
         IF (QUIT) DOCRT = 0
         IF (IERR.NE.0) GO TO 990
         IF (PRTLEV.GE.1) THEN
            LINE = '        Time range        Ant IF P       Amp' //
     *         '        Weight     RMS'
         ELSE IF (NACROS.GT.80) THEN
            LINE = '        Time range         Source           ' //
     *         '    Maxrms     Maxcor (cc)  Corr flags'
         ELSE
            LINE = '        Time range         Source' //
     *         '    Maxrms     Maxcor (cc)  Corr flags'
            END IF
         CALL OPUT (PRINT, 'TITLE2', OOACAR, DIM, DUMMY, LINE, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (DOCRT.NE.0) CALL PRTWRI (PRINT, LINE, QUIT, IERR)
         IF (QUIT) DOCRT = 0
         IF (IERR.NE.0) GO TO 990
         END IF
      DOSOU = (ILOCSU.GT.0)
      ACLIP(1) = ACLIP(1) * ACLIP(1)
      ACLIP(2) = ACLIP(2) * ACLIP(2)
      ACLIP(3) = ACLIP(3) * ACLIP(3)
      ACLIP(4) = ACLIP(4) * ACLIP(4)
      IERR = 0
C                                       Source table/name
      IF (DOSOU) THEN
         SUTAB = 'SoUrce table for ANTRMS'
         TBVER = 1
         CALL UV2TAB (UVIN, SUTAB, 'SU', TBVER, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       For single source use 'OBJECT'
      ELSE
         SUNAME = ' '
         CALL UVDGET (UVIN, 'OBJECT', TYPE, DIM, DUMMY, SUNAME, IERR)
         IF (IERR.NE.0) GO TO 990
         QUAL = 0
         END IF
C                                       Clear accumulators:
C                                       (1,*) =  count
C                                       (2,*) =  sum r then max rms**2
C                                       (3,*) =  sum**2 r then rms**2
C                                       (4,*) =  sum imaginary
C                                       (5,*) =  sum**2 imaginary
C                                       (6,*) =  sum amplitude
      NZERO = CANT * CANT * CIF * CPOL
      CALL DFILL (NZERO, 0.0D0, SUM)
      CALL DFILL (NZERO, 0.0D0, SUMSQ)
      CALL DFILL (NZERO, 0.0D0, SUMWT)
      CALL FILL (NZERO, 0, COUNT)
C                                       Initialize visibility count
      DONE = .FALSE.
      VISNO = 0
C                                       Open FG table
      CALL OFGINI (FGTAB, 'WRIT', FGROW, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Swallow input collecting
C                                       statistics.
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VIS, IERR)
         IF (IERR.LT.0) THEN
            DONE = .TRUE.
            IERR = 0
            END IF
         IF (IERR.GT.0) GO TO 990
         VISNO = VISNO + 1
 110     CURTIM = RP(ILOCT)
         IF (DOSOU) THEN
            CURSOU = RP(ILOCSU) + 0.5
         ELSE
            CURSOU = 0
            END IF
         IF (ILOCFQ.GT.0) CURFRQ = RP(ILOCFQ) + 0.5
C                                       Get antenna numbers
         IF (ILOCB.GT.0) THEN
            ANT1 = (RP(ILOCB) / 256.0) + 0.001
            ANT2 = (RP(ILOCB) - ANT1 * 256) + 0.001
            CURSUB = (RP(ILOCB) - 256 * ANT1 - ANT2) * 100.0 + 1.1
         ELSE
            ANT1 = RP(ILOCA1) + 0.001
            ANT2 = RP(ILOCA2) + 0.001
            CURSUB = RP(ILOCSA) + 0.001
            END IF
C                                       Initial selection.
         IF (VISNO.EQ.1) THEN
            THIFRQ = CURFRQ
            THISUB = CURSUB
            THISOU = CURSOU
            BEGTIM = CURTIM
            ENDTIM = CURTIM + TCLIP
            LSTIME = CURTIM
            END IF
C                                       Finished with data or interval?
         SCDONE = DONE .OR. (CURSOU.NE.THISOU) .OR. (CURSUB.NE.THISUB)
     *      .OR. (CURTIM.GT.LSTIME+TCLIP)
         IF ((SCDONE) .OR. (CURTIM.GT.ENDTIM)) GO TO 150
C                                       Set last vis of interval
         ENDVIS = VISNO
C                                       Set last time
         LSTIME = CURTIM
C                                       Baseline index
         DO 130 LP = 1,CPOL
            IF (.NOT.DOPOLS(LP)) GO TO 130
            DO 125 LIF = 1,CIF
               IP = (LP-1) * INCS + (LIF-1) * INCIF + 1
               CALL AVERAG (CCHAN, VIS(1,IP), INCF, ANT1, ANT2, AONLY,
     *            DOROBU, NP, WP, SP, SSP)
               COUNT(ANT1,ANT2,LIF,LP,1) = COUNT(ANT1,ANT2,LIF,LP,1) +
     *            NP(1)
               SUMWT(ANT1,ANT2,LIF,LP,1) = SUMWT(ANT1,ANT2,LIF,LP,1) +
     *            WP(1)
               SUM(ANT1,ANT2,LIF,LP,1) = SUM(ANT1,ANT2,LIF,LP,1) +
     *            SP(1)
               SUMSQ(ANT1,ANT2,LIF,LP,1) = SUMSQ(ANT1,ANT2,LIF,LP,1) +
     *            SSP(1)
               COUNT(ANT2,ANT1,LIF,LP,1) = COUNT(ANT2,ANT1,LIF,LP,1) +
     *            NP(2)
               SUMWT(ANT2,ANT1,LIF,LP,1) = SUMWT(ANT2,ANT1,LIF,LP,1) +
     *            WP(2)
               SUM(ANT2,ANT1,LIF,LP,1) = SUM(ANT2,ANT1,LIF,LP,1) +
     *            SP(2)
               SUMSQ(ANT2,ANT1,LIF,LP,1) = SUMSQ(ANT2,ANT1,LIF,LP,1) +
     *            SSP(2)
 125           CONTINUE
 130        CONTINUE
         GOTDAT(1) = .TRUE.
C                                       Next vis until done
         IF (.NOT.DONE) GO TO 100
 150     IERR = 0
C                                       Is buffer 1 singleton?
      IF ((SCDONE) .AND. (GOTDAT(2))) THEN
         DO 154 LIF = 1,CIF
            DO 153 LP = 1,CPOL
               DO 152 ANT2 = 1,CANT
                  DO 151 ANT1 = 1,CANT
                     IF (COUNT(ANT1,ANT2,LIF,LP,1).GT.1) GO TO 160
 151                 CONTINUE
 152              CONTINUE
 153           CONTINUE
 154        CONTINUE
C                                       Yes - add into buf 2
         DO 159 LIF = 1,CIF
            DO 158 LP = 1,CPOL
               DO 157 ANT2 = 1,CANT
                  DO 156 ANT1 = 1,CANT
                     COUNT(ANT1,ANT2,LIF,LP,2) =
     *                  COUNT(ANT1,ANT2,LIF,LP,2) +
     *                  COUNT(ANT1,ANT2,LIF,LP,1)
                     SUM(ANT1,ANT2,LIF,LP,2) =
     *                  SUM(ANT1,ANT2,LIF,LP,2) +
     *                  SUM(ANT1,ANT2,LIF,LP,1)
                     SUMSQ(ANT1,ANT2,LIF,LP,2) =
     *                  SUMSQ(ANT1,ANT2,LIF,LP,2) +
     *                  SUMSQ(ANT1,ANT2,LIF,LP,1)
                     SUMWT(ANT1,ANT2,LIF,LP,2) =
     *                  SUMWT(ANT1,ANT2,LIF,LP,2) +
     *                  SUMWT(ANT1,ANT2,LIF,LP,1)
 156                 CONTINUE
 157              CONTINUE
 158           CONTINUE
 159        CONTINUE
         GOTDAT(1) = .FALSE.
         LSTIM2 = MAX (LSTIME, LSTIM2)
         BEGTI2 = MIN (BEGTIM, BEGTI2)
         END IF
C                                       Get statistics
 160  IF (GOTDAT(2)) THEN
C                                       Get statistics
          DO 175 LIF = 1,CIF
             DO 170 LP = 1,CPOL
                DO 161 ANT1 = 1,CANT
                   BADANT(LP,LIF,ANT1) = 0
 161               CONTINUE
                KK = 0
                DO 165 ANT2 = 1,CANT
                   DO 163 ANT1 = 1,CANT
                      KK = KK + 1
                      IF (SUMWT(ANT1,ANT2,LIF,LP,2).GT.0.0) THEN
                         SUM(ANT1,ANT2,LIF,LP,2) =
     *                      (SUM(ANT1,ANT2,LIF,LP,2)
     *                      / SUMWT(ANT1,ANT2,LIF,LP,2)) ** 2
                         SUMSQ(ANT1,ANT2,LIF,LP,2) =
     *                      SUMSQ(ANT1,ANT2,LIF,LP,2)
     *                      / SUMWT(ANT1,ANT2,LIF,LP,2)
                        END IF
                     IF (COUNT(ANT1,ANT2,LIF,LP,2).GT.1) THEN
                        RMS = SUMSQ(ANT1,ANT2,LIF,LP,2) -
     *                     SUM(ANT1,ANT2,LIF,LP,2)
                        RMS = (RMS * COUNT(ANT1,ANT2,LIF,LP,2)) /
     *                     (COUNT(ANT1,ANT2,LIF,LP,2) - 1.0)
                        RMS = ABS (RMS)
                        LSUM(KK) = SUM(ANT1,ANT2,LIF,LP,2)
                        LSUMSQ(KK) = RMS
                        LSUMWT(KK) = (SUMWT(ANT1,ANT2,LIF,LP,2) /
     *                     COUNT(ANT1,ANT2,LIF,LP,2)) ** 2
                     ELSE
                        LSUMSQ(KK) = FBLANK
                        LSUM(KK) = FBLANK
                        LSUMWT(LL) = FBLANK
                        END IF
                     BADANT(LP,LIF,ANT1) = MAX (BADANT(LP,LIF,ANT1),
     *                  COUNT(ANT1,ANT2,LIF,LP,2))
                     BADANT(LP,LIF,ANT2) = MAX (BADANT(LP,LIF,ANT2),
     *                  COUNT(ANT1,ANT2,LIF,LP,2))
 163                 CONTINUE
 165              CONTINUE
C                                       solve for antenna RMS
               CALL ASOLVE (PRTLEV, 'rms', BEGTI2, LSTIM2, CANT, 3.0,
     *            LSUMSQ, ANTSQ(1,LIF,LP), FRAC)
C                                       solve for antenna weight
               CALL ASOLVE (PRTLEV, 'rms', BEGTI2, LSTIM2, CANT, 3.0,
     *            LSUMWT, ANTW(1,LIF,LP), FRAC)
C                                       solve for antenna amplitude
               CALL ASOLVE (PRTLEV, 'amp', BEGTI2, LSTIM2, CANT, 3.0,
     *            LSUM, ANTA(1,LIF,LP), FRA(1,LIF,LP))
 170           CONTINUE
 175        CONTINUE
C                                       apply
         CALL TODHMS (BEGTI2, BEGT)
         CALL TODHMS (LSTIM2, ENDT)
         NWORD = 4 * MAXIF
         CALL RFILL (NWORD, 0.0, CORCNT)
         CALL RFILL (NWORD, 0.0, CORBAD)
         CALL RFILL (NWORD, 0.0, CORWOR)
         CALL RFILL (NWORD, 0.0, CORSUM)
         CALL LFILL (NWORD, .FALSE., BADCOR)
         DO 200 ANT1 = 1,CANT
            DO 190 LIF = 1,CIF
               DO 185 LP = 1,CPOL
                  IF (ANTSQ(ANT1,LIF,LP).NE.FBLANK) THEN
C                                       Real part
                     RMS = ANTSQ(ANT1,LIF,LP) ** 2
C                                       Convert sum to maximum rms**2
                     AMP = ANTA(ANT1,LIF,LP) ** 2
                     WEIGHT = ANTW(ANT1,LIF,LP)
C                                       Is this one bad?
                     ISBAD = ANTSQ(ANT1,LIF,LP).LE.0.0
                     ISBAD = ISBAD .OR. ANTA(ANT1,LIF,LP).LE.0.0
                     ISBAD = ISBAD .OR. RMS.GT.(MXR21 + MXA21*AMP)
                     ISBAD = ISBAD .OR. AMP.LE.ACLIP(1)
                     ISBAD = ISBAD .OR. AMP.GT.ACLIP(2)
                     ISBAD = ISBAD .OR. WEIGHT.LE.WCLIP(1)
                     ISBAD = ISBAD .OR. WEIGHT.GT.WCLIP(2)
                     ISBAD = ISBAD .OR. FRA(ANT1,LIF,LP).GT.CLCLIP(1)
C                                       Correlator info
                     CORCNT(LP,LIF) = CORCNT(LP,LIF) + 1.0
                     IF (ISBAD) THEN
                        RMS = SQRT (RMS)
                        CORBAD(LP,LIF)= CORBAD(LP,LIF) + 1.0
                        CORSUM(LP,LIF)= CORSUM(LP,LIF) + RMS
                        IF (RMS.GT.CORWOR(LP,LIF)) CORWOR(LP,LIF) = RMS
                        IF ((PRTLEV.GE.2) .AND. (DOCRT.NE.0)) THEN
                           AMP = SQRT (AMP)
                           WRITE (LINE,1170) BEGT, ENDT, ANT1, LIF, LP,
     *                        AMP, WEIGHT, RMS
                           CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                           IF (QUIT) DOCRT = 0
                           IF (IERR.NE.0) GO TO 990
                           END IF
                        END IF
                     END IF
 185              CONTINUE
 190           CONTINUE
 200        CONTINUE
C                                       Check for bad correlators
         BADRFI = .FALSE.
         NBAD = 0
         WORST = 0.0
         WORSTM = 0.0
         DO 210 LIF = 1,CIF
            DO 205 LP = 1,CPOL
               IF (CORCNT(LP,LIF).GT.1.1) THEN
C                                       Kill correlator
                  IF ((CORBAD(LP,LIF)/CORCNT(LP,LIF)).GT.MAXBAD(2)) THEN
                     BADCOR(LP,LIF) = .TRUE.
                     BADRFI = .TRUE.
                     NBAD = NBAD + 1
                     RMS = CORSUM(LP,LIF) / CORBAD(LP,LIF)
                     IF (WORST.LT.RMS) THEN
                        WORST = RMS
                        BCORNO = (LIF-1) * CPOL + LP
                        END IF
                     WORSTM = MAX (WORSTM, CORWOR(LP,LIF))
                     END IF
                  END IF
 205           CONTINUE
 210        CONTINUE
C                                       If bad tell about it
         IF ((BADRFI) .AND. (DOCRT.NE.0)) THEN
C                                       Get source name, qualifier
            IF (DOSOU) THEN
               CALL SOUNFO (SUTAB, THISOU, 'SOURCE', TYPE, DIM, DUMMY,
     *            SUNAME, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL SOUNFO (SUTAB, THISOU, 'QUAL', TYPE, DIM, QUAL,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF (NACROS.GE.80) THEN
               WRITE (LINE,1210) BEGT, ENDT, SUNAME, WORSTM, WORST,
     *            BCORNO
               IP = 72
            ELSE
               WRITE (LINE,1211) BEGT, ENDT, SUNAME(:8), WORSTM, WORST,
     *            BCORNO
               IP = 64
               END IF
            DO 215 LIF = 1,CIF
               DO 214 LP = 1,CPOL
                  IP = IP + 1
                  IF (IP.LE.132) THEN
                     IF (BADCOR(LP,LIF)) THEN
                        LINE(IP:IP) = 'T'
                     ELSE IF (CORCNT(LP,LIF).LE.0) THEN
                        LINE(IP:IP) = '-'
                     ELSE
                        LINE(IP:IP) = 'F'
                        END IF
                     END IF
 214              CONTINUE
 215           CONTINUE
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) GO TO 990
            END IF
         IF (BADRFI) NPRINT = NPRINT + 1
C                                       Flagging: everything
         TEMP = CIF * CPOL
         TEMP = NBAD / TEMP
         IF (TEMP.GT.MAXBAD(1)) THEN
            NFLAG1 = NFLAG1 + 1
            PFLAGS(1) = .TRUE.
            PFLAGS(2) = .TRUE.
            PFLAGS(3) = .TRUE.
            PFLAGS(4) = .TRUE.
            SOURID = THISOU
            SUBA = THISUB
            FREQID = THIFRQ
            IFS(1) = 0
            IFS(2) = 0
            TIMER(1) = BEGTI2 - TEPS
            TIMER(2) = LSTIM2 + TEPS
            ANTS(1) = 0
            ANTS(2) = 0
            CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID, SUBA, FREQID,
     *         ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IERR)
            IF (IERR.NE.0) GO TO 990
C                                       an IF at a time
         ELSE
            DFLAGS(1) = .FALSE.
            DFLAGS(2) = .FALSE.
            DFLAGS(3) = .FALSE.
            DFLAGS(4) = .FALSE.
            DO 290 LIF = 1,CIF
               PFLAGS(1) = BADCOR(1,LIF)
               PFLAGS(2) = BADCOR(2,LIF)
               PFLAGS(3) = BADCOR(3,LIF)
               PFLAGS(4) = BADCOR(4,LIF)
               AFLAGS(1) = .FALSE.
               AFLAGS(2) = .FALSE.
               AFLAGS(3) = .FALSE.
               AFLAGS(4) = .FALSE.
               IF ((PFLAGS(1)) .OR. (PFLAGS(2)) .OR. (PFLAGS(3)) .OR.
     *            (PFLAGS(4))) THEN
                  SOURID = THISOU
                  SUBA = THISUB
                  FREQID = THIFRQ
C                                       cross flags
                  DO 220 LP = 1,4
                     DO 219 LL = 1,4
                        IF (PFLAGS(LL)) AFLAGS(LP) = AFLAGS(LP) .OR.
     *                     XPOLS(LL,LP)
 219                    CONTINUE
 220                 CONTINUE
                  IF (.NOT.DOIFS) THEN
                     IFS(1) = LIF
                     IFS(2) = LIF
                     END IF
                  TIMER(1) = BEGTI2 - TEPS
                  TIMER(2) = LSTIM2 + TEPS
                  ANTS(1) = 0
                  ANTS(2) = 0
                  CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID, SUBA,
     *               FREQID, ANTS, TIMER, IFS, CHANS, AFLAGS, REASON,
     *               IERR)
                  IF (IERR.NE.0) GO TO 990
                  NFLAG2 = NFLAG2 + 1
                  IF (DOIFS) THEN
                     IF (AFLAGS(1)) DFLAGS(1) = .TRUE.
                     IF (AFLAGS(2)) DFLAGS(2) = .TRUE.
                     IF (AFLAGS(3)) DFLAGS(3) = .TRUE.
                     IF (AFLAGS(4)) DFLAGS(4) = .TRUE.
                     IF ((DFLAGS(1)) .AND. (DFLAGS(2)) .AND. (DFLAGS(3))
     *                  .AND. (DFLAGS(4))) GO TO 300
                     END IF
                  END IF
C                                       one baseline at a time
               IF ((.NOT.AFLAGS(1)) .OR. (.NOT.AFLAGS(2)) .OR.
     *            (.NOT.AFLAGS(3)) .OR. (.NOT.AFLAGS(4))) THEN
                  DO 270 ANT1 = 1,CANT
                     DO 250 LP = 1,CPOL
                        PFLAGS(LP) = .FALSE.
                        IF (ANTSQ(ANT1,LIF,LP).NE.FBLANK) THEN
                           AMP = ANTA(ANT1,LIF,LP) ** 2
                           WEIGHT = ANTW(ANT1,LIF,LP)
                           RMS = ANTSQ(ANT1,LIF,LP) ** 2
                           PFLAGS(LP) = PFLAGS(LP) .OR.
     *                        ANTA(ANT1,LIF,LP).LE.0.0
                           PFLAGS(LP) = PFLAGS(LP) .OR.
     *                        ANTSQ(ANT1,LIF,LP).LE.0.0
                           PFLAGS(LP) = PFLAGS(LP) .OR. AMP.LE.ACLIP(3)
                           PFLAGS(LP) = PFLAGS(LP) .OR. AMP.GT.ACLIP(4)
                           PFLAGS(LP) = PFLAGS(LP) .OR.
     *                        WEIGHT.LE.WCLIP(3)
                           PFLAGS(LP) = PFLAGS(LP) .OR.
     *                        WEIGHT.GT.WCLIP(4)
                           PFLAGS(LP) = PFLAGS(LP) .OR.
     *                        FRA(ANT1,LIF,LP).GT.CLCLIP(2)
                           PFLAGS(LP) = PFLAGS(LP) .OR.
     *                        RMS.GT.(MXR22 + MXA22*AMP)
                           IF ((PRTLEV.GE.1) .AND. (DOCRT.NE.0) .AND.
     *                        (PFLAGS(LP))) THEN
                              AMP = SQRT (AMP)
                              RMS = SQRT (RMS)
                              WRITE (LINE,1250) BEGT, ENDT, ANT1, LIF,
     *                           LP, AMP, WEIGHT, RMS, FRA(ANT1,LIF,LP)
                              CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                              IF (QUIT) DOCRT = 0
                              IF (IERR.NE.0) GO TO 990
                              END IF
                           END IF
                        IF ((BADANT(LP,LIF,ANT1).EQ.1) .AND. (KILL1))
     *                     PFLAGS(LP) = .TRUE.
 250                    CONTINUE
                     IF ((PFLAGS(1)) .OR. (PFLAGS(2)) .OR. (PFLAGS(3))
     *                  .OR. (PFLAGS(4))) THEN
                        SOURID = THISOU
                        SUBA = THISUB
                        FREQID = THIFRQ
                        DO 255 LP = 1,4
                           AFLAGS(LP) = .FALSE.
                           DO 254 LL = 1,4
                              IF (PFLAGS(LL)) AFLAGS(LP) = AFLAGS(LP)
     *                           .OR. XPOLS(LL,LP)
 254                          CONTINUE
 255                       CONTINUE
                        IF (.NOT.DOIFS) THEN
                           IFS(1) = LIF
                           IFS(2) = LIF
                           END IF
                        TIMER(1) = BEGTI2 - TEPS
                        TIMER(2) = LSTIM2 + TEPS
                        ANTS(1) = ANT1
                        ANTS(2) = 0
                        CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID, SUBA,
     *                     FREQID, ANTS, TIMER, IFS, CHANS, AFLAGS,
     *                     REASON, IERR)
                        IF (IERR.NE.0) GO TO 990
                        NFLAG3 = NFLAG3 + 1
                        END IF
 270                 CONTINUE
                  END IF
 290           CONTINUE
            END IF
         GOTDAT(2) = .FALSE.
         END IF
C                                       move data to buf 2
      IF (GOTDAT(1)) THEN
         CALL DPCOPY (NZERO, SUM(1,1,1,1,1), SUM(1,1,1,1,2))
         CALL DPCOPY (NZERO, SUMSQ(1,1,1,1,1), SUMSQ(1,1,1,1,2))
         CALL DPCOPY (NZERO, SUMWT(1,1,1,1,1), SUMWT(1,1,1,1,2))
         CALL COPY (NZERO, COUNT(1,1,1,1,1), COUNT(1,1,1,1,2))
         GOTDAT(1) = .FALSE.
         GOTDAT(2) = .TRUE.
         BEGTI2 = BEGTIM
         LSTIM2 = LSTIME
         END IF
C                                       clear buffers if scan done
      IF ((SCDONE) .AND. (GOTDAT(2))) GO TO 160
C                                       Clear accumulators:
 300  CALL DFILL (NZERO, 0.0D0, SUM)
      CALL DFILL (NZERO, 0.0D0, SUMSQ)
      CALL DFILL (NZERO, 0.0D0, SUMWT)
      CALL FILL (NZERO, 0, COUNT)
C                                       Another clipping interval?
      IF (.NOT.DONE) THEN
         THISOU = CURSOU
         BEGTIM = CURTIM
         ENDTIM = CURTIM + TCLIP
         THIFRQ = CURFRQ
         THISUB = CURSUB
         LSTIME = CURTIM
         GO TO 110
         END IF
C                                       close FG table
      CALL TABCLO (FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       history
      WRITE (HILINE,1700) MAXRMS(1,1), MAXRMS(2,1)
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1701) MAXRMS(1,2), MAXRMS(2,2)
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1711) NFLAG1
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1712) NFLAG2
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1713) NFLAG3
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close file
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete temporary table
      IF (DOSOU) THEN
         CALL DESTRY (SUTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       report
      NBAD = NFLAG1 + NFLAG2 + NFLAG3
      WRITE (MSGTXT,1800) NPRINT, NBAD
      CALL MSGWRT (5)
      IF (NBAD.GT.0) THEN
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1801) NFLAG1
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1802) NFLAG2
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1803) NFLAG3
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         IERR = 0
         END IF
C                                       Close printer
       IF (PRINT.NE.' ') THEN
         CALL OCLOSE (PRINT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'ANTRMS: ERROR IN RFI DETECTION FOR ' // UVIN
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('For ''',A,''' data: rms clipping coef.=',F8.4,F7.4)
 1001 FORMAT ('For ''',A,''' data: amp clipping range.=',F8.4,F11.4)
 1002 FORMAT ('For ''',A,''' data: wt clipping range.=',2(1PE10.3))
 1003 FORMAT ('For ''',A,''' data: closure error fraction',F7.4)
 1005 FORMAT ('Max. fract. questionable corrs.=',F6.3,'  antennas=',
     *   F6.3)
 1170 FORMAT (I3,'/',2(I2.2,':'),I2.2,' -',I3,'/',2(I2.2,':'),I2.2,
     *   2I3,I2,' ??? ',3(1PE11.3))
 1210 FORMAT (I3,'/',2(I2.2,':'),I2.2,' -',I3,'/',2(I2.2,':'),I2.2,1X,
     *   A,1X,2F11.5,'(',I2.2,')')
 1211 FORMAT (I3,'/',2(I2.2,':'),I2.2,' -',I3,'/',2(I2.2,':'),I2.2,
     *   1X,A8,1X,2F11.5,'(',I2.2,')')
 1250 FORMAT (I3,'/',2(I2.2,':'),I2.2,' -',I3,'/',2(I2.2,':'),I2.2,
     *   2I3,I2,' bad ',3(1PE11.3),0PF7.3)
 1700 FORMAT ('MAXRMS(1)=',F8.4,',',F7.4,' / rms Jy,gain for',
     *   ' questionable')
 1701 FORMAT ('MAXRMS(2)=',F8.4,',',F7.4,' / rms Jy,gain for',
     *   ' bad baseline')
 1711 FORMAT ('FLAGD(1) =',I8,' / integrations fully flagged')
 1712 FORMAT ('FLAGD(2) =',I8,' / corr/integrations fully flagged')
 1713 FORMAT ('FLAGD(3) =',I8,' / antennas/integrations flagged')
 1800 FORMAT ('Found',I6,' bad times,',I6,' total flags')
 1801 FORMAT ('Wrote',I6,' antenna & IF independent flags')
 1802 FORMAT ('Wrote',I6,' antenna independent, IF dependent flags')
 1803 FORMAT ('Wrote',I6,' antenna & IF dependent flags')
      END
      SUBROUTINE ANTIME (UVIN, PRINT, FGTAB, CANT, CIF, CPOL, CCHAN,
     *   CTIME, OPTYPE, DOPOLS, XPOLS, SUM, SUMSQ, SUMWT, COUNT, ANTSQ,
     *   ANTA, ANTW, FRA, AMPS, RMSS, WTS, FRAS, IERR)
C-----------------------------------------------------------------------
C   Look for interference based on a high real or imaginary rms in time
C   interval TIMEAVG.
C   Inputs:
C      UVIN      C*?   Name of input uvdata object.
C      PRINT     C*?   Print object
C      FGTAB     C*?   Output flag table object
C      CANT      I     Maximum antenna number
C      CPOL      I     Number of polarizations
C      CIF       I     Number of IFs
C      CCHAN     I     Number of spactral channels
C      CTIME     I     Limit on number of times
C      OPTYPE    C*4   'GAIN' convert all amps to gains and do all
C                      sources together
C      DOPOLS    L(4)  Which polarizations to test
C      XPOLS     L(4,4)  Which pols to flag (i,j) if i bad flag j
C   Scratch variables
C      SUM, SUMSQ, SUMWT, COUNT, AMPS, RMSS, FRAS
C      Note AMPS, RMSS, WTS, FRAS cannot be used when
C           SUM, SUMSQ, SUMWT, COUNT are being used
C   Inputs attached to UVIN:
C      MAXBAD    R(2)  Maximum allowed fraction of bad baselines.
C      TIMEAVG   R     Time in seconds for clipping interval
C      DOIFS     L     Flag all IFs if one bad
C      AMPONLY   L     Use amplitude not Real/Imag
C      KILLONES  L     Kill 1-sample integrations
C      PRTLEV    I     Print level for debugging
C   Output:
C      IERR      I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), PRINT*(*), FGTAB*(*), OPTYPE*4
      INTEGER   CANT, CIF, CPOL, CCHAN, CTIME, IERR
      INTEGER   COUNT(CANT,CANT,CIF,CPOL,2)
      DOUBLE PRECISION SUM(CANT,CANT,CIF,CPOL,2),
     *   SUMSQ(CANT,CANT,CIF,CPOL,2), SUMWT(CANT,CANT,CIF,CPOL,2)
      REAL      ANTSQ(CANT,CIF,CPOL), ANTA(CANT,CIF,CPOL),
     *   ANTW(CANT,CIF,CPOL), FRA(CANT,CIF,CPOL), AMPS(CANT,CIF,CPOL,*),
     *   RMSS(CANT,CIF,CPOL,*), WTS(CANT,CIF,CPOL,*),
     *   FRAS(CANT,CIF,CPOL,*)
      LOGICAL   DOPOLS(4), XPOLS(4,4)
      INCLUDE 'INCS:PUVD.INC'
C
      REAL      MAXBAD(2), TCLIP, ACLIP(4), RMSLIM(2), CLIPR(4),
     *   WCLIP(4), CLCLIP(2)
      LOGICAL   DOIFS, DOROBU, AONLY, KILL1, DOCHNS, DOCAT
      INTEGER   PRTLEV
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NSORC, NTIME, LMAX
      PARAMETER (NSORC = 5000, NTIME = 15001)
      PARAMETER (LMAX = MAXANT*MAXANT)
      INTEGER   TYPE, DIM(7), DUMMY, ANT1, ANT2, ENDVIS, VISNO, CURSOU,
     *   THISOU, TBVER,DOCRT, I, J, LS, XS, BEGT(4), ENDT(4), NACROS,
     *   ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ, ILOCA1,
     *   ILOCA2, ILOCSA, JLOCC, LL, JLOCS, JLOCF, JLOCR, JLOCD, JLOCIF,
     *   LA, INCS, INCF, INCIF, LP, LIF, IP, NPRINT, NFLAG1, NFLAG2,
     *   NFLAG3, FGROW, SOURID, SUBA, FREQID, ANTS(2), IFS(2), CHANS(2),
     *   THISUB, THIFRQ, NBAD, CURSUB, CURFRQ, NZERO, XXROW,  ID(3),
     *   IT(3), SCNT(NSORC), SNUMS(NTIME), MTIME, SINUM(NSORC), SCOUNT,
     *   BADANT(MAXANT), QUAL(NSORC), IDUMMY(2), II, NFLAG4, KK, NP(2)
      LOGICAL   BADCOR(4,MAXIF), DONE, DOSOU, QUIT, DOCAL, PFLAGS(4),
     *   DFLAGS(4), FIRST, AFLAGS(4), GOTDAT(2), SCDONE
      CHARACTER SUTAB*32, SUNAME(NSORC)*16, LINE*132, CDUMMY*1,
     *   REASON*24, ATIME*8, ADATE*12, HILINE*72, XXFILE*32, TITLE1*132,
     *   TITLE2*132
      DOUBLE PRECISION AMP, RMS
      REAL      RP(50), TEPS, DBG1, DBG2, DBG3, DBG4, AMPR, LSTIM2,
     *   CORCNT(4,MAXIF), CORBAD(4,MAXIF), CURTIM, LSTIME, ENDTIM, RMSR,
     *   BEGTIM, TEMP, TIMER(2), VIS(3,MAXCIF), CORWOR(4,MAXIF),
     *   SASUM(NSORC), SRSUM(NSORC), SAASUM(NSORC), SRRSUM(NSORC),
     *   TIMR(2,NTIME), PAMP(4,MAXIF,MAXANT), PRMS(4,MAXIF,MAXANT),
     *   PAS(4,MAXIF,MAXANT), PRS(4,MAXIF,MAXANT), RMSCUT, RMSCT2,
     *   AMN(NSORC), AMX(NSORC), BMN(NSORC), BMX(NSORC), FRAC(MAXANT),
     *   CORSUM(4,MAXIF), ALIST(4*MAXANT*MAXIF), RLIST(4*MAXANT*MAXIF),
     *   TDUMMY(NTIME,4), BEGTI2, LAMP, LRMS, LSUM(LMAX), LSUMSQ(LMAX),
     *   LSUMWT(LMAX), WP(2), SP(2), SSP(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      COMMON /TBUFS/ TIMR, SNUMS, TDUMMY
      COMMON /VBUFS/ VIS
C-----------------------------------------------------------------------
      XXFILE = ' '
      TEPS = 0.02 / (3600.0 * 24.0)
      CALL FILL (NSORC, 0, SCNT)
      CALL RFILL (NSORC, 0.0, SASUM)
      CALL RFILL (NSORC, 0.0, SRSUM)
      CALL RFILL (NSORC, 0.0, SAASUM)
      CALL RFILL (NSORC, 0.0, SRRSUM)
      SCOUNT = 0
      GOTDAT(1) = .FALSE.
      GOTDAT(2) = .FALSE.
C                                       Open printer
      NPRINT = 0
      NFLAG1 = 0
      NFLAG2 = 0
      NFLAG3 = 0
      NFLAG4 = 0
      IF (PRINT.NE.' ') THEN
         CALL OOPEN (PRINT, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (PRINT, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (PRINT, 'NACROS', TYPE, DIM, NACROS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         DOCRT = 0
         END IF
      CALL ZDATE (ID)
      ID(1) = - ABS (ID(1))
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      REASON = TSKNAM // ADATE(:9) // ' ' // ATIME
      IFS(1) = 0
      IFS(2) = 0
      THISOU = 0
      THISUB = 0
      THIFRQ = 0
      CURSUB = 0
      CURFRQ = 0
C                                       Create and open a scratch table:
      XXFILE = 'Temporary results table'
      CALL UV2TAB (UVIN, XXFILE, 'XX', 1, IERR)
      IF (IERR.NE.0) THEN
         XXFILE = ' '
         GO TO 990
         END IF
      CALL OXXINI (XXFILE, 'WRIT', XXROW, IERR)
      IF (IERR.NE.0) THEN
         XXFILE = ' '
         GO TO 990
         END IF
C                                       Retrieve pointers into the data:
      CALL UVDPNT (UVIN, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IERR)
      IF (IERR.NE.0) GO TO 999
      INCS = INCS / 3
      INCF = INCF / 3
      INCIF = INCIF / 3
C                                       Open input.
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get parameters
      CALL OUVGET (UVIN, 'DOROBUST', TYPE, DIM, DOROBU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'CLOSCLIP', TYPE, DIM, CLCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'WTCLIP', TYPE, DIM, WCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'AMPCLIP', TYPE, DIM, ACLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'RMSLIMIT', TYPE, DIM, RMSLIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'CLIPRMS', TYPE, DIM, CLIPR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      RMSLIM(1) = RMSLIM(1) * CLIPR(1)
      RMSLIM(2) = RMSLIM(2) * CLIPR(1)
      CALL OUVGET (UVIN, 'MAXBAD', TYPE, DIM, MAXBAD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'DOCAT', TYPE, DIM, DOCAT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'DOCHANS', TYPE, DIM, DOCHNS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'DOIFS', TYPE, DIM, DOIFS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'AMPONLY', TYPE, DIM, AONLY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'KILLONES', TYPE, DIM, KILL1, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'PRTLEV', TYPE, DIM, PRTLEV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'TIMEAVG', TYPE, DIM, TCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      TCLIP = TCLIP / 86400.0
      IF (.NOT.DOCHNS) THEN
         CALL OUVGET (UVIN, 'CALEDIT.BCHAN', TYPE, DIM, CHANS(1),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVGET (UVIN, 'CALEDIT.ECHAN', TYPE, DIM, CHANS(2),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CHANS(1) = 0
         CHANS(2) = 0
         END IF
C                                       Processing info
      CALL OUVGET (UVIN, 'CALEDIT.DOCAL', TYPE, DIM, DOCAL, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DOCRT.NE.0) THEN
         IF (DOCRT.GT.-3) THEN
            IF (DOCAL) THEN
               LINE = 'Calibration applied'
            ELSE
               LINE = 'NO Calibration applied'
               END IF
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((ACLIP(1).GT.0.0) .OR. (ACLIP(2).LT.1.E5)) THEN
               WRITE (LINE,1001) 'questionable', ACLIP(1), ACLIP(2)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((WCLIP(1).GT.0.0) .OR. (WCLIP(2).LT.1.E9)) THEN
               WRITE (LINE,1002) 'questionable', WCLIP(1), WCLIP(2)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((CLCLIP(1).GT.0.0) .AND. (CLCLIP(1).LT.1.0)) THEN
               WRITE (LINE,1003) 'questionable', CLCLIP(1)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            WRITE (LINE,1005) MAXBAD
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((ACLIP(3).GT.0.0) .OR. (ACLIP(4).LT.1.E5)) THEN
               WRITE (LINE,1001) 'bad', ACLIP(3), ACLIP(4)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((WCLIP(3).GT.0.0) .OR. (WCLIP(4).LT.1.E9)) THEN
               WRITE (LINE,1002) 'bad', WCLIP(3), WCLIP(4)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((CLCLIP(2).GT.0.0) .AND. (CLCLIP(2).LT.1.0)) THEN
               WRITE (LINE,1003) 'bad', CLCLIP(2)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            END IF
         END IF
      DOSOU = (ILOCSU.GT.0)
      IERR = 0
C                                       Source table/name
      IF (DOSOU) THEN
         SUTAB = 'SoUrce table for ANTIME'
         TBVER = 1
         CALL UV2TAB (UVIN, SUTAB, 'SU', TBVER, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       For single source use 'OBJECT'
      ELSE
         CALL UVDGET (UVIN, 'OBJECT', TYPE, DIM, DUMMY, SUNAME, IERR)
         IF (IERR.NE.0) GO TO 990
         QUAL(1) = 0
         END IF
C                                       Clear accumulators:
C                                       (1,*) =  count
C                                       (2,*) =  sum r then max rms**2
C                                       (3,*) =  sum**2 r then rms**2
C                                       (4,*) =  sum imaginary
C                                       (5,*) =  sum**2 imaginary
C                                       (6,*) =  sum amplitude
      NZERO = CANT * CANT * CIF * CPOL
      CALL DFILL (NZERO, 0.0D0, SUM)
      CALL DFILL (NZERO, 0.0D0, SUMSQ)
      CALL DFILL (NZERO, 0.0D0, SUMWT)
      CALL FILL (NZERO, 0, COUNT)
C                                       Initialize visibility count
      DONE = .FALSE.
      VISNO = 0
C                                       Open FG table
      CALL OFGINI (FGTAB, 'WRIT', FGROW, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Swallow input collecting
C                                       statistics.
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VIS, IERR)
         IF (IERR.LT.0) THEN
            DONE = .TRUE.
            IERR = 0
            END IF
         IF (IERR.GT.0) GO TO 990
         VISNO = VISNO + 1
 110     CURTIM = RP(ILOCT)
         IF (DOSOU) THEN
            CURSOU = RP(ILOCSU) + 0.5
         ELSE
            CURSOU = 0
            END IF
         IF (ILOCFQ.GT.0) CURFRQ = RP(ILOCFQ) + 0.5
C                                       Get antenna numbers
         IF (ILOCB.GT.0) THEN
            ANT1 = (RP(ILOCB) / 256.0) + 0.001
            ANT2 = (RP(ILOCB) - ANT1 * 256) + 0.001
            CURSUB = (RP(ILOCB) - 256 * ANT1 - ANT2) * 100.0 + 1.1
         ELSE
            ANT1 = RP(ILOCA1) + 0.001
            ANT2 = RP(ILOCA2) + 0.001
            CURSUB = RP(ILOCSA) + 0.001
            END IF
C                                       Initial selection.
         IF (VISNO.EQ.1) THEN
            THIFRQ = CURFRQ
            THISUB = CURSUB
            THISOU = CURSOU
            BEGTIM = CURTIM
            ENDTIM = CURTIM + TCLIP
            LSTIME = CURTIM
            END IF
C                                       Finished with data or interval?
         SCDONE = DONE .OR. (CURSOU.NE.THISOU) .OR. (CURSUB.NE.THISUB)
     *      .OR. (CURTIM.GT.LSTIME+TCLIP)
         IF ((SCDONE) .OR. (CURTIM.GT.ENDTIM)) GO TO 150
C                                       Set last vis of interval
         ENDVIS = VISNO
C                                       Set last time
         LSTIME = CURTIM
C                                       Baseline index
         DO 130 LP = 1,CPOL
            IF (.NOT.DOPOLS(LP)) GO TO 130
            DO 125 LIF = 1,CIF
               IP = (LP-1) * INCS + (LIF-1) * INCIF + 1
               CALL AVERAG (CCHAN, VIS(1,IP), INCF, ANT1, ANT2, AONLY,
     *            DOROBU, NP, WP, SP, SSP)
               COUNT(ANT1,ANT2,LIF,LP,1) = COUNT(ANT1,ANT2,LIF,LP,1) +
     *            NP(1)
               SUMWT(ANT1,ANT2,LIF,LP,1) = SUMWT(ANT1,ANT2,LIF,LP,1) +
     *            WP(1)
               SUM(ANT1,ANT2,LIF,LP,1) = SUM(ANT1,ANT2,LIF,LP,1) +
     *            SP(1)
               SUMSQ(ANT1,ANT2,LIF,LP,1) = SUMSQ(ANT1,ANT2,LIF,LP,1) +
     *            SSP(1)
               COUNT(ANT2,ANT1,LIF,LP,1) = COUNT(ANT2,ANT1,LIF,LP,1) +
     *            NP(2)
               SUMWT(ANT2,ANT1,LIF,LP,1) = SUMWT(ANT2,ANT1,LIF,LP,1) +
     *            WP(2)
               SUM(ANT2,ANT1,LIF,LP,1) = SUM(ANT2,ANT1,LIF,LP,1) +
     *            SP(2)
               SUMSQ(ANT2,ANT1,LIF,LP,1) = SUMSQ(ANT2,ANT1,LIF,LP,1) +
     *            SSP(2)
 125           CONTINUE
 130        CONTINUE
         GOTDAT(1) = .TRUE.
C                                       Next vis until done
         IF (.NOT.DONE) GO TO 100
 150     IERR = 0
C                                       Is buffer 1 singleton?
      IF ((SCDONE) .AND. (GOTDAT(2))) THEN
         DO 154 LIF = 1,CIF
            DO 153 LP = 1,CPOL
               DO 152 ANT2 = 1,CANT
                  DO 151 ANT1 = 1,CANT
                     IF (COUNT(ANT1,ANT2,LIF,LP,1).GT.1) GO TO 160
 151                 CONTINUE
 152              CONTINUE
 153           CONTINUE
 154        CONTINUE
C                                       Yes - add into buf 2
         DO 159 LIF = 1,CIF
            DO 158 LP = 1,CPOL
               DO 157 ANT2 = 1,CANT
                  DO 156 ANT1 = 1,CANT
                     COUNT(ANT1,ANT2,LIF,LP,2) =
     *                  COUNT(ANT1,ANT2,LIF,LP,2) +
     *                  COUNT(ANT1,ANT2,LIF,LP,1)
                     SUM(ANT1,ANT2,LIF,LP,2) =
     *                  SUM(ANT1,ANT2,LIF,LP,2) +
     *                  SUM(ANT1,ANT2,LIF,LP,1)
                     SUMSQ(ANT1,ANT2,LIF,LP,2) =
     *                  SUMSQ(ANT1,ANT2,LIF,LP,2) +
     *                  SUMSQ(ANT1,ANT2,LIF,LP,1)
                     SUMWT(ANT1,ANT2,LIF,LP,2) =
     *                  SUMWT(ANT1,ANT2,LIF,LP,2) +
     *                  SUMWT(ANT1,ANT2,LIF,LP,1)
 156                 CONTINUE
 157              CONTINUE
 158           CONTINUE
 159        CONTINUE
         GOTDAT(1) = .FALSE.
         LSTIM2 = MAX (LSTIME, LSTIM2)
         BEGTI2 = MIN (BEGTIM, BEGTI2)
         END IF
C                                       Get statistics
 160  IF (GOTDAT(2)) THEN
         DO 175 LIF = 1,CIF
            DO 170 LP = 1,CPOL
               CALL FILL (CANT, 0, BADANT)
               KK = 0
               DO 165 ANT2 = 1,CANT
                  DO 161 ANT1 = 1,CANT
                     KK = KK + 1
                     IF (SUMWT(ANT1,ANT2,LIF,LP,2).GT.0.0) THEN
                        SUM(ANT1,ANT2,LIF,LP,2) =
     *                     (SUM(ANT1,ANT2,LIF,LP,2)
     *                     / SUMWT(ANT1,ANT2,LIF,LP,2)) ** 2
                        SUMSQ(ANT1,ANT2,LIF,LP,2) =
     *                     SUMSQ(ANT1,ANT2,LIF,LP,2) /
     *                     SUMWT(ANT1,ANT2,LIF,LP,2)
                        END IF
                     IF (COUNT(ANT1,ANT2,LIF,LP,2).GT.1) THEN
                        RMS = SUMSQ(ANT1,ANT2,LIF,LP,2) -
     *                     SUM(ANT1,ANT2,LIF,LP,2)
                        RMS = (RMS * COUNT(ANT1,ANT2,LIF,LP,2)) /
     *                     (COUNT(ANT1,ANT2,LIF,LP,2) - 1.0)
                        RMS = ABS (RMS)
                        LSUM(KK) = SUM(ANT1,ANT2,LIF,LP,2)
                        LSUMSQ(KK) = RMS
                        LSUMWT(KK) = (SUMWT(ANT1,ANT2,LIF,LP,2) /
     *                     COUNT(ANT1,ANT2,LIF,LP,2)) ** 2
                     ELSE
                        LSUMWT(KK) = FBLANK
                        LSUMSQ(KK) = FBLANK
                        LSUM(KK) = FBLANK
                        END IF
                     BADANT(ANT1) = MAX (BADANT(ANT1),
     *                  COUNT(ANT1,ANT2,LIF,LP,2))
                     BADANT(ANT2) = MAX (BADANT(ANT2),
     *                  COUNT(ANT1,ANT2,LIF,LP,2))
 161                 CONTINUE
 165              CONTINUE
C                                       solve for antenna RMS
               CALL ASOLVE (PRTLEV, 'rms', BEGTI2, LSTIM2, CANT, 3.0,
     *            LSUMSQ, ANTSQ(1,LIF,LP), FRAC)
C                                       solve for antenna weight
               CALL ASOLVE (PRTLEV, 'rms', BEGTI2, LSTIM2, CANT, 3.0,
     *            LSUMWT, ANTW(1,LIF,LP), FRAC)
C                                       solve for antenna amplitude
               CALL ASOLVE (PRTLEV, 'amp', BEGTI2, LSTIM2, CANT, 3.0,
     *            LSUM, ANTA(1,LIF,LP), FRA(1,LIF,LP))
C                                       mark all 1-sample
               IF (KILL1) THEN
                  DO 167 ANT1 = 1,CANT
                     IF (BADANT(ANT1).EQ.1) THEN
                        ANTA(ANT1,LIF,LP) = -1.E6
                        ANTSQ(ANT1,LIF,LP) = -1.E6
                        ANTW(ANT1,LIF,LP) = -1.E6
                        END IF
 167                 CONTINUE
                  END IF
 170           CONTINUE
 175        CONTINUE
C                                       local source number
         J = 0
         DO 176 I = 1,SCOUNT
            IF (THISOU.EQ.SINUM(I)) J = I
 176        CONTINUE
         IF (J.EQ.0) THEN
            SCOUNT = SCOUNT + 1
            J = SCOUNT
            SINUM(J) = THISOU
            END IF
C                                       Stuff into array
         DO 190 LA = 1,CANT
            DO 185 LIF = 1,CIF
               DO 180 LP = 1,CPOL
                  IF ((ANTSQ(LA,LIF,LP).NE.FBLANK) .AND.
     *               (ANTSQ(LA,LIF,LP).GT.0.0) .AND.
     *               (ANTW(LA,LIF,LP).GT.0.0) .AND.
     *               (ANTA(LA,LIF,LP).NE.FBLANK) .AND.
     *               (ANTA(LA,LIF,LP).GT.0.0)) THEN
                     SCNT(J) = SCNT(J) + 1
                     SRSUM(J) = SRSUM(J) + ANTSQ(LA,LIF,LP)
                     SASUM(J) = SASUM(J) + ANTA(LA,LIF,LP)
                     SRRSUM(J) = SRRSUM(J) + ANTSQ(LA,LIF,LP) ** 2
                     SAASUM(J) = SAASUM(J) + ANTA(LA,LIF,LP) ** 2
                     END IF
 180              CONTINUE
 185           CONTINUE
 190        CONTINUE
C                                       write to scratch table
        CALL OTABXX (XXFILE, 'WRIT', XXROW, BEGTI2, LSTIM2, J,
     *      ANTA, ANTSQ, ANTW, FRA, IDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         GOTDAT(2) = .FALSE.
         END IF
C                                       move data to buf 2
      IF (GOTDAT(1)) THEN
         CALL DPCOPY (NZERO, SUM(1,1,1,1,1), SUM(1,1,1,1,2))
         CALL DPCOPY (NZERO, SUMSQ(1,1,1,1,1), SUMSQ(1,1,1,1,2))
         CALL DPCOPY (NZERO, SUMWT(1,1,1,1,1), SUMWT(1,1,1,1,2))
         CALL COPY (NZERO, COUNT(1,1,1,1,1), COUNT(1,1,1,1,2))
         GOTDAT(1) = .FALSE.
         GOTDAT(2) = .TRUE.
         BEGTI2 = BEGTIM
         LSTIM2 = LSTIME
         END IF
C                                       clear buffers if scan done
      IF ((SCDONE) .AND. (GOTDAT(2))) GO TO 160
C                                       Clear accumulators:
      CALL DFILL (NZERO, 0.0D0, SUM)
      CALL DFILL (NZERO, 0.0D0, SUMSQ)
      CALL DFILL (NZERO, 0.0D0, SUMWT)
      CALL FILL (NZERO, 0, COUNT)
C                                       Another clipping interval?
      IF (.NOT.DONE) THEN
         THISOU = CURSOU
         BEGTIM = CURTIM
         ENDTIM = CURTIM + TCLIP
         THIFRQ = CURFRQ
         THISUB = CURSUB
         LSTIME = CURTIM
         GO TO 110
         END IF
C-----------------------------------------------------------------------
C                                       Okay - close table and re-read
C                                       to fill memory
      CALL TABCLO (XXFILE, IERR)
      IF (IERR.NE.0) GO TO 990
      MTIME = XXROW - 1
      IF ((MTIME.GT.CTIME) .OR. (MTIME.GT.NTIME)) THEN
         LP = MIN (NTIME, CTIME)
         WRITE (MSGTXT,1200) MTIME, LP
         CALL MSGWRT (7)
         MTIME = LP
         END IF
      CALL OXXINI (XXFILE, 'READ', XXROW, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 215 IP = 1,MTIME
         CALL OTABXX (XXFILE, 'READ', XXROW, TIMR(1,IP), TIMR(2,IP),
     *      SNUMS(IP), AMPS(1,1,1,IP), RMSS(1,1,1,IP), WTS(1,1,1,IP),
     *      FRAS(1,1,1,IP), IDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
 215     CONTINUE
      CALL TABCLO (XXFILE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       average source sums
      DO 220 LS = 1,SCOUNT
         IF (SCNT(LS).GT.0) THEN
            SRSUM(LS) = SRSUM(LS) / SCNT(LS)
            SASUM(LS) = SASUM(LS) / SCNT(LS)
            SRRSUM(LS) = SRRSUM(LS) / SCNT(LS)
            SAASUM(LS) = SAASUM(LS) / SCNT(LS)
            END IF
 220     CONTINUE
C                                       get source info, scale clips
      DO 230 LS = 1,SCOUNT
C                                       Get source name, qualifier
         THISOU = SINUM(LS)
         IF (DOSOU) THEN
            CALL SOUNFO (SUTAB, THISOU, 'SOURCE', TYPE, DIM, DUMMY,
     *         SUNAME(LS), IERR)
            IF (IERR.NE.0) GO TO 990
            CALL SOUNFO (SUTAB, THISOU, 'QUAL', TYPE, DIM, QUAL(LS),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         IF ((OPTYPE.EQ.'GAIN') .AND. (SASUM(LS).GT.0.0)) THEN
            AMN(LS) = ACLIP(1) / SASUM(LS)
            AMX(LS) = ACLIP(2) / SASUM(LS)
            BMN(LS) = ACLIP(3) / SASUM(LS)
            BMX(LS) = ACLIP(4) / SASUM(LS)
C                                       tell user
            WRITE (MSGTXT,1220) SUNAME(LS), QUAL(LS), SASUM(LS)
            LINE = MSGTXT
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (DOCRT.LE.0.0) CALL MSGWRT (4)
         ELSE
            AMN(LS) = ACLIP(1)
            AMX(LS) = ACLIP(2)
            BMN(LS) = ACLIP(3)
            BMX(LS) = ACLIP(4)
            END IF
 230     CONTINUE
C                                       scale to gains
      IF (OPTYPE.EQ.'GAIN') THEN
         DO 270 IP = 1,MTIME
            LS = SNUMS(IP)
            DO 260 LA = 1,CANT
               DO 250 LIF = 1,CIF
                  DO 240 LP = 1,CPOL
                     IF ((AMPS(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                  (AMPS(LA,LIF,LP,IP).GT.0.0)) AMPS(LA,LIF,LP,IP)
     *                  = AMPS(LA,LIF,LP,IP) / SASUM(LS)
 240                 CONTINUE
 250              CONTINUE
 260           CONTINUE
 270        CONTINUE
         SCOUNT = 1
         END IF
C                                       outer loops: antenna, source
      DO 500 XS = 1,SCOUNT
         LS = XS
C                                       find correct amp, rms
         DO 350 LA = 1,CANT
            DO 340 LIF = 1,CIF
               DO 330 LP = 1,CPOL
                  I = 0
                  J = 0
                  II = 0
                  DO 320 IP = 1,MTIME
                     IF (OPTYPE.EQ.'GAIN') LS = SNUMS(IP)
                     IF (SNUMS(IP).EQ.LS) THEN
                        IF (AMPS(LA,LIF,LP,IP).NE.FBLANK) THEN
                           II = II + 1
                           IF ((AMPS(LA,LIF,LP,IP).GT.AMN(LS)) .AND.
     *                        (AMPS(LA,LIF,LP,IP).LE.AMX(LS)) .AND.
     *                        (WTS(LA,LIF,LP,IP).GT.WCLIP(1)) .AND.
     *                        (WTS(LA,LIF,LP,IP).LE.WCLIP(2)) .AND.
     *                        (FRAS(LA,LIF,LP,IP).LE.CLCLIP(1))) THEN
                              I = I + 1
                              ALIST(I) = AMPS(LA,LIF,LP,IP)
                              END IF
                           END IF
                        IF ((RMSS(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                     (RMSS(LA,LIF,LP,IP).GT.0.0)) THEN
                           J = J + 1
                           RLIST(J) = RMSS(LA,LIF,LP,IP)
                           END IF
                        END IF
 320                 CONTINUE
                  IF (I.GT.0) THEN
                     CALL ROBUST (ALIST, I, PAMP(LP,LIF,LA), AMPR)
                     PAS(LP,LIF,LA) = MAX (AMPR, RMSLIM(1))
                  ELSE
                     PAMP(LP,LIF,LA) = 0.0
                     IF (II.GT.0) PAMP(LP,LIF,LA) = -1.E6
                     PAS(LP,LIF,LA) = FBLANK
                     END IF
                  IF (J.GT.0) THEN
                     CALL ROBUST (RLIST, J, PRMS(LP,LIF,LA), AMPR)
                     PRS(LP,LIF,LA) = MAX (AMPR, RMSLIM(2))
                  ELSE
                     PRMS(LP,LIF,LA) = 0.0
                     PRS(LP,LIF,LA) = FBLANK
                     END IF
 330              CONTINUE
 340           CONTINUE
 350        CONTINUE
C                                       get global averages reliably
         I = 0
         J = 0
         DO 380 LA = 1,CANT
            DO 370 LIF = 1,CIF
               DO 360 LP = 1,CPOL
                  IF ((PAS(LP,LIF,LA).NE.FBLANK) .AND.
     *               (PAMP(LP,LIF,LA).GT.0.0)) THEN
                     I = I + 1
                     ALIST(I) = PAMP(LP,LIF,LA)
                     END IF
                  IF ((PRS(LP,LIF,LA).NE.FBLANK) .AND.
     *               (PRMS(LP,LIF,LA).GT.0.0)) THEN
                     J = J + 1
                     RLIST(J) = PRMS(LP,LIF,LA)
                     END IF
 360              CONTINUE
 370           CONTINUE
 380        CONTINUE
         IF (I.GT.0) THEN
            CALL ROBUST (ALIST, I, LAMP, AMPR)
         ELSE
            LAMP = 0.0
            AMPR = 0.0
            END IF
         IF (J.GT.0) THEN
            CALL ROBUST (RLIST, J, LRMS, RMSR)
         ELSE
            LRMS = 0.0
            RMSR = 0.0
            END IF
         AMP = LAMP
         RMS = LRMS
         IF (OPTYPE.EQ.'GAIN') THEN
            WRITE (MSGTXT,1390) AMP, AMPR, 'All sources', ' '
            CALL MSGWRT (4)
            WRITE (MSGTXT,1391) RMS, RMSR, 'All sources', ' '
         ELSE
            WRITE (MSGTXT,1390) AMP, AMPR, 'source=', SUNAME(LS)
            CALL MSGWRT (4)
            WRITE (MSGTXT,1391) RMS, RMSR, 'source=', SUNAME(LS)
            END IF
         CALL MSGWRT (4)
C                                       Flags for all times
         FIRST = (DOCRT.NE.0)
         IF (FIRST) THEN
            TITLE1 = 'Correlators flagged for all times'
            DIM(1) = LEN (TITLE1)
            DIM(2) = 1
            CALL OPUT (PRINT, 'TITLE1', OOACAR, DIM, DUMMY, TITLE1,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            TITLE2 = '       Time range       Ant IF Ps        Flag'
            CALL OPUT (PRINT, 'TITLE2', OOACAR, DIM, DUMMY, TITLE2,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TODHMS (TIMR(1,1), BEGT)
            CALL TODHMS (TIMR(2,MTIME), ENDT)
            END IF
         DO 395 LA = 1,CANT
            DO 390 LIF = 1,CIF
               CALL LFILL (4, .FALSE., PFLAGS)
               DO 385 LP = 1,CPOL
                  PFLAGS(LP) = (PAMP(LP,LIF,LA).LT.-1.E5)
                  IF (PFLAGS(LP)) PRS(LP,LIF,LA) = FBLANK
 385              CONTINUE
C                                       Full time range flagged
               IF ((PFLAGS(1)) .OR. (PFLAGS(2)) .OR. (PFLAGS(3)) .OR.
     *            (PFLAGS(4))) THEN
C                                       page titles
                  IF (FIRST) THEN
                     CALL PRTWRI (PRINT, TITLE1, QUIT, IERR)
                     IF (QUIT) DOCRT = 0
                     IF (IERR.NE.0) GO TO 990
                     IF (DOCRT.NE.0) CALL PRTWRI (PRINT, TITLE2, QUIT,
     *                  IERR)
                     IF (QUIT) DOCRT = 0
                     IF (IERR.NE.0) GO TO 990
                     FIRST = .FALSE.
                     END IF
                  SOURID = SINUM(LS)
                  SUBA = THISUB
                  FREQID = THIFRQ
C                                       cross flags
                  DO 387 LP = 1,4
                     AFLAGS(LP) = .FALSE.
                     DO 386 LL = 1,4
                        IF (PFLAGS(LL)) AFLAGS(LP) = AFLAGS(LP) .OR.
     *                     XPOLS(LL,LP)
 386                    CONTINUE
 387                 CONTINUE
                  IF (DOCRT.NE.0) THEN
                     WRITE (LINE,1385) BEGT, ENDT, LA, LIF, PFLAGS,
     *                  AFLAGS
                     CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                     IF (QUIT) DOCRT = 0
                     IF (IERR.NE.0) GO TO 990
                     END IF
                  IF (.NOT.DOIFS) THEN
                     IFS(1) = LIF
                     IFS(2) = LIF
                     END IF
                  TIMER(1) = TIMR(1,1) - TEPS
                  TIMER(2) = TIMR(2,MTIME) + TEPS
                  ANTS(1) = LA
                  ANTS(2) = 0
                  CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID, SUBA,
     *               FREQID, ANTS, TIMER, IFS, CHANS, AFLAGS, REASON,
     *               IERR)
                  IF (IERR.NE.0) GO TO 990
                  NFLAG4 = NFLAG4 + 1
                  END IF
 390           CONTINUE
 395        CONTINUE
C                                       go through times
C                                       Page labels
         FIRST = DOCRT.NE.0
         IF (FIRST) THEN
            IF (PRTLEV.GE.2) THEN
               TITLE1 = 'Individual data warnings and flags'
            ELSE IF (PRTLEV.GE.1) THEN
               TITLE1 = 'Data to be flagged individually'
            ELSE
               TITLE1 = 'Data flagged independent of antenna'
               END IF
            DIM(1) = LEN (TITLE1)
            DIM(2) = 1
            CALL OPUT (PRINT, 'TITLE1', OOACAR, DIM, DUMMY, TITLE1,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            IF (PRTLEV.GE.1) THEN
               TITLE2 = '       Time range       Ant IF P       Amp' //
     *            '       Weight     Adif/RMS    Lim      Closure'
            ELSE
               TITLE2 = '       Time range        Source           Qual'
     *            // '  Corr flags'
               END IF
            CALL OPUT (PRINT, 'TITLE2', OOACAR, DIM, DUMMY, TITLE2,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         RMSCUT = RMS + CLIPR(2) * RMSR
         RMSCT2 = RMS + CLIPR(4) * RMSR
         DO 490 IP = 1,MTIME
            IF (OPTYPE.EQ.'GAIN') LS = SNUMS(IP)
            IF (SNUMS(IP).EQ.LS) THEN
               CALL TODHMS (TIMR(1,IP), BEGT)
               CALL TODHMS (TIMR(2,IP), ENDT)
C                                       check for all ant questionable
               LP = 4 * CIF
               CALL RFILL (LP, 0.0, CORCNT)
               CALL RFILL (LP, 0.0, CORBAD)
               CALL RFILL (LP, 0.0, CORWOR)
               CALL RFILL (LP, 0.0, CORSUM)
               DO 420 LA = 1,CANT
                  DO 410 LIF = 1,CIF
                     DO 400 LP = 1,CPOL
                        IF ((AMPS(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                     (WTS(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                     (PAS(LP,LIF,LA).NE.FBLANK)) THEN
                           CORCNT(LP,LIF) = CORCNT(LP,LIF) + 1.0
                           IF ((AMPS(LA,LIF,LP,IP).LE.AMN(LS)) .OR.
     *                        (AMPS(LA,LIF,LP,IP).GT.AMX(LS)) .OR.
     *                        (WTS(LA,LIF,LP,IP).LE.WCLIP(1)) .OR.
     *                        (WTS(LA,LIF,LP,IP).GT.WCLIP(2)) .OR.
     *                        (FRAS(LA,LIF,LP,IP).GT.CLCLIP(1)) .OR.
     *                        (ABS(AMPS(LA,LIF,LP,IP)-PAMP(LP,LIF,LA))
     *                        .GT.CLIPR(1)*PAS(LP,LIF,LA))) THEN
                              CORBAD(LP,LIF) = CORBAD(LP,LIF) + 1.0
                              IF ((PRTLEV.GE.2) .AND. (DOCRT.NE.0)) THEN
C                                       page titles
                                 IF (FIRST) THEN
                                    CALL PRTWRI (PRINT, TITLE1, QUIT,
     *                                 IERR)
                                    IF (QUIT) DOCRT = 0
                                    IF (IERR.NE.0) GO TO 990
                                    IF (DOCRT.NE.0) CALL PRTWRI (PRINT,
     *                                 TITLE2, QUIT,IERR)
                                    IF (QUIT) DOCRT = 0
                                    IF (IERR.NE.0) GO TO 990
                                    FIRST = .FALSE.
                                    END IF
                                 DBG1 = AMPS(LA,LIF,LP,IP)
                                 DBG2 = WTS(LA,LIF,LP,IP)
                                 DBG3 = ABS (AMPS(LA,LIF,LP,IP) -
     *                              PAMP(LP,LIF,LA))
                                 DBG4 = CLIPR(1) * PAS(LP,LIF,LA)
                                 WRITE (LINE,1400) BEGT, ENDT, LA, LIF,
     *                              LP, DBG1, DBG2, DBG3, DBG4,
     *                              FRAS(LA,LIF,LP,IP)
                                 CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                                 IF (QUIT) DOCRT = 0
                                 IF (IERR.NE.0) GO TO 990
                                 END IF
                              END IF
                           END IF
                        IF ((RMSS(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                     (PRS(LP,LIF,LA).NE.FBLANK)) THEN
                           CORSUM(LP,LIF) = CORSUM(LP,LIF) + 1.0
                           IF ((RMSS(LA,LIF,LP,IP).GT.RMSCUT) .OR.
     *                        (RMSS(LA,LIF,LP,IP).LE.0.0)) THEN
                              CORWOR(LP,LIF) = CORWOR(LP,LIF) + 1.0
                              IF ((PRTLEV.GE.2) .AND. (DOCRT.NE.0)) THEN
                                 DBG1 = RMSS(LA,LIF,LP,IP)
                                 DBG2 = RMSCUT
                                 WRITE (LINE,1401) BEGT, ENDT, LA, LIF,
     *                              LP, DBG1, DBG2
                                 CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                                 IF (QUIT) DOCRT = 0
                                 IF (IERR.NE.0) GO TO 990
                                 END IF
                              END IF
                           END IF
 400                    CONTINUE
 410                 CONTINUE
 420              CONTINUE
               NBAD = 0
               I = 0
               DO 440 LIF = 1,CIF
                  DO 430 LP = 1,CPOL
                     BADCOR(LP,LIF) = .FALSE.
                     IF ((CORCNT(LP,LIF).GT.0.0) .AND.
     *                  (CORSUM(LP,LIF).GT.0.0)) THEN
                        I = I + 1
                        IF ((CORBAD(LP,LIF)/CORCNT(LP,LIF).GT.MAXBAD(2))
     *                     .OR. (CORWOR(LP,LIF)/CORSUM(LP,LIF).GT.
     *                     MAXBAD(2))) THEN
                           NBAD = NBAD + 1
                           BADCOR(LP,LIF) = .TRUE.
                           END IF
                     ELSE IF (CORCNT(LP,LIF).GT.0.0) THEN
                        I = I + 1
                        IF (CORBAD(LP,LIF)/CORCNT(LP,LIF).GT.
     *                     MAXBAD(2)) THEN
                           NBAD = NBAD + 1
                           BADCOR(LP,LIF) = .TRUE.
                           END IF
                     ELSE IF (CORSUM(LP,LIF).GT.0.0) THEN
                        I = I + 1
                        IF (CORWOR(LP,LIF)/CORSUM(LP,LIF).GT.
     *                     MAXBAD(2)) THEN
                           NBAD = NBAD + 1
                           BADCOR(LP,LIF) = .TRUE.
                           END IF
                        END IF
 430                 CONTINUE
 440              CONTINUE
               TEMP = I
               TEMP = NBAD / TEMP
C                                       If bad tell about it
               IF ((NBAD.GT.0) .AND. (DOCRT.NE.0)) THEN
C                                       page titles
                  IF (FIRST) THEN
                     CALL PRTWRI (PRINT, TITLE1, QUIT, IERR)
                     IF (QUIT) DOCRT = 0
                     IF (IERR.NE.0) GO TO 990
                     IF (DOCRT.NE.0) CALL PRTWRI (PRINT, TITLE2, QUIT,
     *                  IERR)
                     IF (QUIT) DOCRT = 0
                     IF (IERR.NE.0) GO TO 990
                     FIRST = .FALSE.
                     END IF
                  WRITE (LINE,1440) BEGT, ENDT, SUNAME(LS), QUAL(LS)
                  I = 50
                  DO 450 LIF = 1,CIF
                     DO 449 LP = 1,CPOL
                        I = I + 1
                        IF (I.LE.132) THEN
                           IF (BADCOR(LP,LIF)) THEN
                              LINE(I:I) = 'T'
                           ELSE IF (CORCNT(LP,LIF).LE.0) THEN
                              LINE(I:I) = '-'
                           ELSE
                              LINE(I:I) = 'F'
                              END IF
                           END IF
 449                    CONTINUE
 450                 CONTINUE
                  CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                  IF (QUIT) DOCRT = 0
                  IF (IERR.NE.0) GO TO 990
                  END IF
               IF (NBAD.GT.0) NPRINT = NPRINT + 1
C                                       Flag everything
               IF (TEMP.GT.MAXBAD(1)) THEN
                  NFLAG1 = NFLAG1 + 1
                  PFLAGS(1) = .TRUE.
                  PFLAGS(2) = .TRUE.
                  PFLAGS(3) = .TRUE.
                  PFLAGS(4) = .TRUE.
                  SOURID = SINUM(LS)
                  SUBA = THISUB
                  FREQID = THIFRQ
                  IFS(1) = 0
                  IFS(2) = 0
                  TIMER(1) = TIMR(1,IP) - TEPS
                  TIMER(2) = TIMR(2,IP) + TEPS
                  ANTS(1) = 0
                  ANTS(2) = 0
                  CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID, SUBA,
     *               FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON,
     *               IERR)
                  IF (IERR.NE.0) GO TO 990
C                                       an IF at a time
               ELSE
                  DFLAGS(1) = .FALSE.
                  DFLAGS(2) = .FALSE.
                  DFLAGS(3) = .FALSE.
                  DFLAGS(4) = .FALSE.
                  DO 480 LIF = 1,CIF
                     PFLAGS(1) = BADCOR(1,LIF)
                     PFLAGS(2) = BADCOR(2,LIF)
                     PFLAGS(3) = BADCOR(3,LIF)
                     PFLAGS(4) = BADCOR(4,LIF)
                     AFLAGS(1) = .FALSE.
                     AFLAGS(2) = .FALSE.
                     AFLAGS(3) = .FALSE.
                     AFLAGS(4) = .FALSE.
                     IF ((PFLAGS(1)) .OR. (PFLAGS(2)) .OR. (PFLAGS(3))
     *                  .OR. (PFLAGS(4))) THEN
                        SOURID = SINUM(LS)
                        SUBA = THISUB
                        FREQID = THIFRQ
C                                       cross flags
                        DO 455 LP = 1,4
                           DO 454 LL = 1,4
                              IF (PFLAGS(LL)) AFLAGS(LP) = AFLAGS(LP)
     *                           .OR. XPOLS(LL,LP)
 454                          CONTINUE
 455                       CONTINUE
                        IF (.NOT.DOIFS) THEN
                           IFS(1) = LIF
                           IFS(2) = LIF
                           END IF
                        TIMER(1) = TIMR(1,IP) - TEPS
                        TIMER(2) = TIMR(2,IP) + TEPS
                        ANTS(1) = 0
                        ANTS(2) = 0
                        CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID, SUBA,
     *                     FREQID, ANTS, TIMER, IFS, CHANS, AFLAGS,
     *                     REASON, IERR)
                        IF (IERR.NE.0) GO TO 990
                        NFLAG2 = NFLAG2 + 1
                        IF (DOIFS) THEN
                           IF (AFLAGS(1)) DFLAGS(1) = .TRUE.
                           IF (AFLAGS(2)) DFLAGS(2) = .TRUE.
                           IF (AFLAGS(3)) DFLAGS(3) = .TRUE.
                           IF (AFLAGS(4)) DFLAGS(4) = .TRUE.
                           IF ((DFLAGS(1)) .AND. (DFLAGS(2)) .AND.
     *                        (DFLAGS(3)) .AND. (DFLAGS(4))) GO TO 490
                           END IF
                        END IF
C                                       one antenna at a time
                     IF ((.NOT.AFLAGS(1)) .OR. (.NOT.AFLAGS(2)) .OR.
     *                  (.NOT.AFLAGS(3)) .OR. (.NOT.AFLAGS(4))) THEN
                        DO 470 LA = 1,CANT
                           DO 460 LP = 1,CPOL
                              PFLAGS(LP) = .FALSE.
                              IF ((AMPS(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                           (WTS(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                           (PAS(LP,LIF,LA).NE.FBLANK)) THEN
                                 IF ((AMPS(LA,LIF,LP,IP).LE.BMN(LS))
     *                              .OR.(AMPS(LA,LIF,LP,IP).GT.BMX(LS))
     *                              .OR. (WTS(LA,LIF,LP,IP).LE.WCLIP(3))
     *                              .OR. (WTS(LA,LIF,LP,IP).GT.WCLIP(4))
     *                              .OR. (FRAS(LA,LIF,LP,IP).GT.
     *                              CLCLIP(2)) .OR.
     *                              (ABS(AMPS(LA,LIF,LP,IP)-
     *                              PAMP(LP,LIF,LA)).GT.
     *                              CLIPR(3)*PAS(LP,LIF,LA))) THEN
                                    PFLAGS(LP) = .TRUE.
                                    IF ((PRTLEV.GE.1) .AND.
     *                                 (DOCRT.NE.0)) THEN
C                                       page titles
                                       IF (FIRST) THEN
                                          CALL PRTWRI (PRINT, TITLE1,
     *                                       QUIT, IERR)
                                          IF (QUIT) DOCRT = 0
                                          IF (IERR.NE.0) GO TO 990
                                          IF (DOCRT.NE.0) CALL PRTWRI
     *                                       (PRINT, TITLE2, QUIT,IERR)
                                          IF (QUIT) DOCRT = 0
                                          IF (IERR.NE.0) GO TO 990
                                          FIRST = .FALSE.
                                          END IF
                                       DBG1 = AMPS(LA,LIF,LP,IP)
                                       DBG2 = WTS(LA,LIF,LP,IP)
                                       DBG3 = ABS (AMPS(LA,LIF,LP,IP) -
     *                                    PAMP(LP,LIF,LA))
                                       DBG4 = CLIPR(3)*PAS(LP,LIF,LA)
                                       WRITE (LINE,1450) BEGT, ENDT, LA,
     *                                    LIF, LP, DBG1, DBG2, DBG3,
     *                                    DBG4, FRAS(LA,LIF,LP,IP)
                                       CALL PRTWRI (PRINT, LINE, QUIT,
     *                                    IERR)
                                       IF (QUIT) DOCRT = 0
                                       IF (IERR.NE.0) GO TO 990
                                       END IF
                                    END IF
                                 END IF
                              IF ((RMSS(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                           (PRS(LP,LIF,LA).NE.FBLANK)) THEN
                                 IF ((RMSS(LA,LIF,LP,IP).GT.RMSCT2) .OR.
     *                              (RMSS(LA,LIF,LP,IP).LE.0.0)) THEN
                                    PFLAGS(LP) = .TRUE.
                                    IF ((PRTLEV.GE.1) .AND.
     *                                 (DOCRT.NE.0)) THEN
                                       DBG1 = RMSS(LA,LIF,LP,IP)
                                       DBG2 = RMSCT2
                                       WRITE (LINE,1451) BEGT, ENDT, LA,
     *                                    LIF, LP, DBG1, DBG2
                                       CALL PRTWRI (PRINT, LINE, QUIT,
     *                                    IERR)
                                       IF (QUIT) DOCRT = 0
                                       IF (IERR.NE.0) GO TO 990
                                       END IF
                                    END IF
                                 END IF
 460                          CONTINUE
                           IF ((PFLAGS(1)) .OR. (PFLAGS(2)) .OR.
     *                        (PFLAGS(3)) .OR. (PFLAGS(4))) THEN
                              SOURID = SINUM(LS)
                              SUBA = THISUB
                              FREQID = THIFRQ
C                                       cross flags
                              DO 465 LP = 1,4
                                 AFLAGS(LP) = .FALSE.
                                 DO 464 LL = 1,4
                                    IF (PFLAGS(LL)) AFLAGS(LP) =
     *                                 AFLAGS(LP) .OR. XPOLS(LL,LP)
 464                                CONTINUE
 465                             CONTINUE
                              IF (.NOT.DOIFS) THEN
                                 IFS(1) = LIF
                                 IFS(2) = LIF
                                 END IF
                              TIMER(1) = TIMR(1,IP) - TEPS
                              TIMER(2) = TIMR(2,IP) + TEPS
                              ANTS(1) = LA
                              ANTS(2) = 0
                              CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID,
     *                           SUBA, FREQID, ANTS, TIMER, IFS, CHANS,
     *                           AFLAGS, REASON, IERR)
                              IF (IERR.NE.0) GO TO 990
                              NFLAG3 = NFLAG3 + 1
                              END IF
 470                       CONTINUE
                        END IF
 480                 CONTINUE
                  END IF
               END IF
 490        CONTINUE
 500     CONTINUE
C-----------------------------------------------------------------------
C                                       close FG table
      CALL TABCLO (FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       history
      RMSLIM(1) = RMSLIM(1) / CLIPR(1)
      RMSLIM(2) = RMSLIM(2) / CLIPR(1)
      WRITE (HILINE,1700) RMSLIM
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1701) CLIPR(1)
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1702) CLIPR(2)
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1703) CLIPR(3)
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1704) CLIPR(4)
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1711) NFLAG1
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1712) NFLAG2
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1713) NFLAG3
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1714) NFLAG4
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close file
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.DOCAT) THEN
         CALL TABZAP (XXFILE, IERR)
         XXFILE = ' '
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Delete temporary table
      IF (DOSOU) THEN
         CALL DESTRY (SUTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       report
      NBAD = NFLAG1 + NFLAG2 + NFLAG3 + NFLAG4
      WRITE (MSGTXT,1800) NPRINT, NBAD
      CALL MSGWRT (5)
      IF (NBAD.GT.0) THEN
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1801) NFLAG1
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1802) NFLAG2
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1803) NFLAG3
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1804) NFLAG4
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         IERR = 0
         END IF
C                                       Close printer
      IF (PRINT.NE.' ') THEN
         CALL OCLOSE (PRINT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'ANTIME: ERROR IN RFI DETECTION FOR ' // UVIN
      CALL MSGWRT (7)
      IF ((.NOT.DOCAT) .AND. (XXFILE.NE.' ')) CALL TABZAP (XXFILE, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('For ''',A,''' data: amp clipping range.=',F8.4,F9.4)
 1002 FORMAT ('For ''',A,''' data: wt clipping range.=',2(1PE10.3))
 1003 FORMAT ('For ''',A,''' data: closure error fraction',F7.4)
 1005 FORMAT ('Max. fract. questionable corrs.=',F6.3,'  antennas=',
     *   F6.3)
 1200 FORMAT ('READ',I6,' TIMES, WILL DO ONLY',I5)
 1220 FORMAT ('Scale ',A,':',I5.5,' by rel flux',F10.5)
 1385 FORMAT (I2,'/',2(I2.2,':'),I2.2,' -',I2,'/',2(I2.2,':'),I2.2,2I3,
     *   4(1X,L1),2X,4L1)
 1390 FORMAT ('Overall average amp=',1PE10.3,' +-',1PE10.3,1X,A,1X,A)
 1391 FORMAT ('Overall average rms=',1PE10.3,' +-',1PE10.3,1X,A,1X,A)
 1400 FORMAT (I2,'/',2(I2.2,':'),I2.2,' -',I2,'/',2(I2.2,':'),I2.2,
     *   2I3,I2,' ??? ',4(1PE11.3),0PF7.3)
 1401 FORMAT (I2,'/',2(I2.2,':'),I2.2,' -',I2,'/',2(I2.2,':'),I2.2,
     *   2I3,I2,' ??? ',22X,2(1PE11.3))
 1440 FORMAT (I2,'/',2(I2.2,':'),I2.2,' -',I2,'/',2(I2.2,':'),I2.2,1X,
     *   A,':',I5.5,1X,2F9.5,'(',I2.2,')')
 1450 FORMAT (I2,'/',2(I2.2,':'),I2.2,' -',I2,'/',2(I2.2,':'),I2.2,
     *   2I3,I2,' bad ',4(1PE11.3),0PF7.3)
 1451 FORMAT (I2,'/',2(I2.2,':'),I2.2,' -',I2,'/',2(I2.2,':'),I2.2,
     *   2I3,I2,' bad ',22X,2(1PE11.3))
 1700 FORMAT ('RMSLIM  =',F7.4,',',F7.4,' / min rms allowed in amp',
     *   ' and rms')
 1701 FORMAT ('CLIPR(1)  =',F6.1,4X,'/ question data > CL(1)*rms from',
     *   ' mean')
 1702 FORMAT ('CLIPR(3)  =',F6.1,4X,'/ flag amp data > CL(3)*rms from',
     *   ' mean')
 1703 FORMAT ('CLIPR(2)  =',F6.1,4X,'/ question rms > CL(2)*avgrms',
     *   ' above mean')
 1704 FORMAT ('CLIPR(4)  =',F6.1,4X,'/ flag rms > CL(1)*avgrms above',
     *   ' mean')
 1711 FORMAT ('FLAGD(1) =',I8,' / integrations fully flagged')
 1712 FORMAT ('FLAGD(2) =',I8,' / corr/integrations fully flagged')
 1713 FORMAT ('FLAGD(3) =',I8,' / antennas/integrations flagged')
 1714 FORMAT ('FLAGD(4) =',I8,' / correlators for all times flagged')
 1800 FORMAT ('Found',I6,' bad times,',I6,' total flags')
 1801 FORMAT ('Wrote',I6,' antenna & IF independent flags')
 1802 FORMAT ('Wrote',I6,' antenna independent, IF dependent flags')
 1803 FORMAT ('Wrote',I6,' antenna & IF dependent flags')
 1804 FORMAT ('Wrote',I6,' antenna & IF dependent, all-times flags')
      END
      SUBROUTINE AMVDIF (UVIN, PRINT, FGTAB, CANT, CIF, CPOL, CCHAN,
     *   CTIME, DOPOLS, XPOLS, SUM, SUMWT, COUNT, WORK, CREAL, CIMAG,
     *   ANTW, FRA, AMPS, DIFS, WTS, FRAS, REFAS, IERR)
C-----------------------------------------------------------------------
C   Look for bad data via amplitude of the vector difference of the
C   current sample from a running mean of the samples
C   Inputs:
C      UVIN      C*?   Name of input uvdata object.
C      PRINT     C*?   Print object
C      FGTAB     C*?   Output flag table object
C      CANT      I     Maximum antenna number
C      CPOL      I     Number of polarizations
C      CIF       I     Number of IFs
C      CCHAN     I     Number of spactral channels
C      CTIME     I     Limit on number of times
C      DOPOLS    L(4)  Which polarizations to test
C      XPOLS     L(4,4)  Which pols to flag (i,j) if i bad flag j
C   Scratch variables
C      SUM, SUMSQ, SUMWT, COUNT, AMPS, DIFS
C      Note AMPS, DIFS, WTS cannot be used when
C           SUM, WORK, SUMWT, COUNT are being used
C   Inputs attached to UVIN:
C      MAXRMS    R(*)  Maximum RMS allowed, constant plus amplitude
C                      coefficient.
C      MAXBAD    R(2)  Maximum allowed fraction of bad baselines.
C      TIMEAVG   R     Time in seconds for clipping interval
C      DOIFS     L     Flag all IFs if one bad
C      AMPONLY   L     Use amplitude not Real/Imag
C      KILLONES  L     Kill 1-sample integrations
C      PRTLEV    I     Print level for debugging
C   Output:
C      IERR      I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), PRINT*(*), FGTAB*(*)
      INTEGER   CANT, CIF, CPOL, CCHAN, CTIME, IERR
      INTEGER   COUNT(CANT,CANT,CIF,CPOL), REFAS(CIF,CPOL,*)
      REAL      SUM(CANT,CANT,CIF,CPOL), WORK(CANT,CANT,CIF,CPOL),
     *   SUMWT(CANT,CANT,CIF,CPOL), CIMAG(CANT,CIF,CPOL),
     *   CREAL(CANT,CIF,CPOL), ANTW(CANT,CIF,CPOL), FRA(CANT,CIF,CPOL),
     *   AMPS(CANT,CIF,CPOL,*), DIFS(CANT,CIF,CPOL,*),
     *   WTS(CANT,CIF,CPOL,*), FRAS(CANT,CIF,CPOL,*)
      LOGICAL   DOPOLS(4), XPOLS(4,4)
C
      REAL      MAXBAD(2), TCLIP, ACLIP(4), WCLIP(4), MAXVD(4), SCNTIM,
     *   CLCLIP(2)
      LOGICAL   DOIFS, DOCHNS, DOCAT, DOROBU
      INTEGER   PRTLEV, REFANT
C
      INTEGER   NSORC, NTIME
      PARAMETER (NSORC = 5000, NTIME = 15001)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   TYPE, DIM(7), DUMMY, ANT1, ANT2, ENDVIS, VISNO, CURSOU,
     *   THISOU, TBVER,DOCRT, I, J, LS, BEGT(4), ENDT(4), NACROS, ILOCU,
     *   ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ, ILOCA1, ILOCA2,
     *   ILOCSA, JLOCC, JLOCS, LL, JLOCF, JLOCR, JLOCD, JLOCIF, LA,
     *   INCS, INCF, INCIF, LP, LIF, IP, NPRINT, NFLAG1, NFLAG2, NFLAG3,
     *   FGROW, SOURID, SUBA, FREQID, ANTS(2), IFS(2), CHANS(2), THISUB,
     *   THIFRQ, NBAD, CURSUB, CURFRQ, NZERO, XXROW,  ID(3), IT(3),
     *   SNUMS(NTIME), MTIME, SINUM(NSORC), SCOUNT, QUAL, MINNO, NPP(2),
     *   NREF(MAXIF*4), MODE, NREFS(MAXANT), LIST(NTIME), NP, KP,
     *   KP1, KP2
      LOGICAL   BADCOR(4,MAXIF), DONE, DOSOU, QUIT, DOCAL, PFLAGS(4),
     *   DFLAGS(4), AFLAGS(4), SCDONE
      CHARACTER SUTAB*32, SUNAME*16, LINE*132, CDUMMY*1, REASON*24,
     *   ATIME*8, ADATE*12, HILINE*72, XXFILE*32
      REAL      RP(50), TEPS, CORCNT(4,MAXIF), CORBAD(4,MAXIF), CURTIM,
     *   LSTIME, ENDTIM, BEGTIM, TEMP, TIMER(2), MERE, MEIM, MEDIUN,
     *   SCNT2, VIS(3,MAXCIF), TIMR(2,NTIME), RE(NTIME), IM(NTIME),
     *   TI(NTIME), WP(2), SP(2), SSP(2), DBG1, DBG2, DBG3, DBG4
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      COMMON /TBUFS/ TIMR, RE, IM, TI, LIST, SNUMS
      COMMON /VBUFS/ VIS
C-----------------------------------------------------------------------
      XXFILE = ' '
      TEPS = 0.02 / (3600.0 * 24.0)
      SCOUNT = 0
      MINNO = 3
      MODE = 10
C                                       Open printer
      NPRINT = 0
      NFLAG1 = 0
      NFLAG2 = 0
      NFLAG3 = 0
      IF (PRINT.NE.' ') THEN
         CALL OOPEN (PRINT, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (PRINT, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (PRINT, 'NACROS', TYPE, DIM, NACROS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         DOCRT = 0
         END IF
      CALL ZDATE (ID)
      ID(1) = - ABS (ID(1))
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      REASON = TSKNAM // ADATE(:9) // ' ' // ATIME
      IFS(1) = 0
      IFS(2) = 0
      THISOU = 0
      THISUB = 0
      THIFRQ = 0
      CURSUB = 0
      CURFRQ = 0
C                                       Create and open a scratch table:
      XXFILE = 'Temporary results table'
      CALL UV2TAB (UVIN, XXFILE, 'XX', 1, IERR)
      IF (IERR.NE.0) THEN
         XXFILE = ' '
         GO TO 990
         END IF
      CALL OXXINI (XXFILE, 'WRIT', XXROW, IERR)
      IF (IERR.NE.0) THEN
         XXFILE = ' '
         GO TO 990
         END IF
C                                       Retrieve pointers into the data:
      CALL UVDPNT (UVIN, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IERR)
      IF (IERR.NE.0) GO TO 999
      INCS = INCS / 3
      INCF = INCF / 3
      INCIF = INCIF / 3
C                                       Open input.
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get parameters
      CALL OUVGET (UVIN, 'DOROBUST', TYPE, DIM, DOROBU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'REFANT', TYPE, DIM, REFANT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (REFANT.LE.0) REFANT = 1
      CALL OUVGET (UVIN, 'WTCLIP', TYPE, DIM, WCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'CLOSCLIP', TYPE, DIM, CLCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'AMPCLIP', TYPE, DIM, ACLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'MAXBAD', TYPE, DIM, MAXBAD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'MAXRMS', TYPE, DIM, MAXVD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'DOCAT', TYPE, DIM, DOCAT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'DOCHANS', TYPE, DIM, DOCHNS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'DOIFS', TYPE, DIM, DOIFS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'PRTLEV', TYPE, DIM, PRTLEV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'SCANLENG', TYPE, DIM, SCNTIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      SCNTIM = SCNTIM / 86400.0
      SCNT2 = SCNTIM / 2.0
      CALL OUVGET (UVIN, 'TIMEAVG', TYPE, DIM, TCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      TCLIP = TCLIP / 86400.0
      IF (.NOT.DOCHNS) THEN
         CALL OUVGET (UVIN, 'CALEDIT.BCHAN', TYPE, DIM, CHANS(1),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVGET (UVIN, 'CALEDIT.ECHAN', TYPE, DIM, CHANS(2),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CHANS(1) = 0
         CHANS(2) = 0
         END IF
C                                       Processing info
      CALL OUVGET (UVIN, 'CALEDIT.DOCAL', TYPE, DIM, DOCAL, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DOCRT.NE.0) THEN
         IF (DOCRT.GT.-3) THEN
            IF (DOCAL) THEN
               LINE = 'Calibration applied'
            ELSE
               LINE = 'NO Calibration applied'
               END IF
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            WRITE (LINE,1000) 'questionable', MAXVD(1), MAXVD(2)
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((ACLIP(1).GT.0.0) .OR. (ACLIP(2).LT.1.E5)) THEN
               WRITE (LINE,1001) 'questionable', ACLIP(1), ACLIP(2)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((WCLIP(1).GT.0.0) .OR. (WCLIP(2).LT.1.E9)) THEN
               WRITE (LINE,1002) 'questionable', WCLIP(1), WCLIP(2)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((CLCLIP(1).GT.0.0) .AND. (CLCLIP(1).LT.1.0)) THEN
               WRITE (LINE,1003) 'questionable', CLCLIP(1)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            WRITE (LINE,1005) MAXBAD
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            WRITE (LINE,1000) 'bad', MAXVD(3), MAXVD(4)
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((ACLIP(3).GT.0.0) .OR. (ACLIP(4).LT.1.E5)) THEN
               WRITE (LINE,1001) 'bad', ACLIP(3), ACLIP(4)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((WCLIP(3).GT.0.0) .OR. (WCLIP(4).LT.1.E9)) THEN
               WRITE (LINE,1002) 'bad', WCLIP(3), WCLIP(4)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((CLCLIP(2).GT.0.0) .AND. (CLCLIP(2).LT.1.0)) THEN
               WRITE (LINE,1003) 'bad', CLCLIP(2)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            END IF
C                                       Page labels
         IF (PRTLEV.GE.2) THEN
            LINE = 'Individual data warnings and flags'
         ELSE IF (PRTLEV.GE.1) THEN
            LINE = 'Data to be flagged individually'
         ELSE
            LINE = 'Data flagged independent of antenna'
            END IF
         DIM(1) = LEN (LINE)
         DIM(2) = 1
         CALL OPUT (PRINT, 'TITLE1', OOACAR, DIM, DUMMY, LINE, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL PRTWRI (PRINT, LINE, QUIT, IERR)
         IF (QUIT) DOCRT = 0
         IF (IERR.NE.0) GO TO 990
         IF (PRTLEV.GE.1) THEN
            LINE = '         Time range       Ant IF P       Amp' //
     *         '        Weight     Vdif     closure'
         ELSE
            LINE = '         Time range        Source           Qual' //
     *         '  Corr flags'
            END IF
         CALL OPUT (PRINT, 'TITLE2', OOACAR, DIM, DUMMY, LINE, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL PRTWRI (PRINT, LINE, QUIT, IERR)
         IF (QUIT) DOCRT = 0
         IF (IERR.NE.0) GO TO 990
         END IF
      DOSOU = (ILOCSU.GT.0)
      IERR = 0
C                                       Source table/name
      IF (DOSOU) THEN
         SUTAB = 'SoUrce table for ANTIME'
         TBVER = 1
         CALL UV2TAB (UVIN, SUTAB, 'SU', TBVER, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       For single source use 'OBJECT'
      ELSE
         CALL UVDGET (UVIN, 'OBJECT', TYPE, DIM, DUMMY, SUNAME, IERR)
         IF (IERR.NE.0) GO TO 990
         QUAL = 0
         END IF
C                                       Clear accumulators:
C                                       (1,*) =  count
C                                       (2,*) =  sum r then max rms**2
C                                       (3,*) =  sum**2 r then rms**2
C                                       (4,*) =  sum imaginary
C                                       (5,*) =  sum**2 imaginary
C                                       (6,*) =  sum amplitude
      NZERO = CANT * CANT * CIF * CPOL
      CALL RFILL (NZERO, 0.0, SUM)
      CALL RFILL (NZERO, 0.0, SUMWT)
      CALL FILL (NZERO, 0, COUNT)
C                                       Initialize visibility count
      DONE = .FALSE.
      VISNO = 0
C                                       Open FG table
      CALL OFGINI (FGTAB, 'WRIT', FGROW, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Swallow input collecting
C                                       statistics.
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VIS, IERR)
         IF (IERR.LT.0) THEN
            DONE = .TRUE.
            IERR = 0
            END IF
         IF (IERR.GT.0) GO TO 990
         VISNO = VISNO + 1
 110     CURTIM = RP(ILOCT)
         IF (DOSOU) THEN
            CURSOU = RP(ILOCSU) + 0.5
         ELSE
            CURSOU = 0
            END IF
         IF (ILOCFQ.GT.0) CURFRQ = RP(ILOCFQ) + 0.5
C                                       Get antenna numbers
         IF (ILOCB.GT.0) THEN
            ANT1 = (RP(ILOCB) / 256.0) + 0.001
            ANT2 = (RP(ILOCB) - ANT1 * 256) + 0.001
            CURSUB = (RP(ILOCB) - 256 * ANT1 - ANT2) * 100.0 + 1.1
         ELSE
            ANT1 = RP(ILOCA1) + 0.001
            ANT2 = RP(ILOCA2) + 0.001
            CURSUB = RP(ILOCSA) + 0.001
            END IF
C                                       Initial selection.
         IF (VISNO.EQ.1) THEN
            THIFRQ = CURFRQ
            THISUB = CURSUB
            THISOU = CURSOU
            BEGTIM = CURTIM
            ENDTIM = CURTIM + TCLIP
            LSTIME = CURTIM
            END IF
C                                       Finished with data or interval?
         SCDONE = DONE .OR. (CURSOU.NE.THISOU) .OR. (CURSUB.NE.THISUB)
     *      .OR. (CURTIM.GT.LSTIME+TCLIP)
         IF ((SCDONE) .OR. (CURTIM.GT.ENDTIM)) GO TO 150
C                                       Set last vis of interval
         ENDVIS = VISNO
C                                       Set last time
         LSTIME = CURTIM
C                                       Baseline index
         DO 130 LP = 1,CPOL
            IF (.NOT.DOPOLS(LP)) GO TO 130
            DO 125 LIF = 1,CIF
               IP = (LP-1) * INCS + (LIF-1) * INCIF + 1
               CALL AVERAG (CCHAN, VIS(1,IP), INCF, ANT1, ANT2, .FALSE.,
     *            DOROBU, NPP, WP, SP, SSP)
               COUNT(ANT1,ANT2,LIF,LP) = COUNT(ANT1,ANT2,LIF,LP) +
     *            NPP(1)
               SUMWT(ANT1,ANT2,LIF,LP) = SUMWT(ANT1,ANT2,LIF,LP) +
     *            WP(1)
               SUM(ANT1,ANT2,LIF,LP) = SUM(ANT1,ANT2,LIF,LP) + SP(1)
               COUNT(ANT2,ANT1,LIF,LP) = COUNT(ANT2,ANT1,LIF,LP) +
     *            NPP(2)
               SUMWT(ANT2,ANT1,LIF,LP) = SUMWT(ANT2,ANT1,LIF,LP) +
     *            WP(2)
               SUM(ANT2,ANT1,LIF,LP) = SUM(ANT2,ANT1,LIF,LP) + SP(2)
 125           CONTINUE
 130        CONTINUE
C                                       Next vis until done
         IF (.NOT.DONE) GO TO 100
 150     IERR = 0
C                                       Average and square
       DO 170 LIF = 1,CIF
          DO 165 LP = 1,CPOL
             DO 160 ANT2 = 1,CANT
               DO 155 ANT1 = 1,CANT
                  IF (SUMWT(ANT1,ANT2,LIF,LP).GT.0.0) THEN
                     SUM(ANT1,ANT2,LIF,LP) = SUM(ANT1,ANT2,LIF,LP)
     *                  / SUMWT(ANT1,ANT2,LIF,LP)
                     SUMWT(ANT1,ANT2,LIF,LP) = (SUMWT(ANT1,ANT2,LIF,LP)
     *                  / COUNT(ANT1,ANT2,LIF,LP)) ** 2
                  ELSE
                     SUMWT(ANT1,ANT2,LIF,LP) = FBLANK
                     SUM(ANT1,ANT2,LIF,LP) = FBLANK
                     END IF
 155              CONTINUE
 160           CONTINUE
C                                       solve for antenna weight**2
            CALL ASOLVE (PRTLEV, 'rms', BEGTIM, LSTIME, CANT, 3.0,
     *         SUMWT(1,1,LIF,LP), ANTW(1,LIF,LP), FRA(1,LIF,LP))
C                                       solve for antenna real/imag
            KP = (LP - 1) * CIF + LIF
            CALL VSOLVE (SUM(1,1,LIF,LP), SUMWT(1,1,LIF,LP), CANT,
     *         REFANT, MODE, MINNO, 2.5, CREAL(1,LIF,LP),
     *         CIMAG(1,LIF,LP), NREF(KP), FRA(1,LIF,LP), WORK, PRTLEV,
     *         IERR)
 165        CONTINUE
 170     CONTINUE
C                                       local source number
      J = 0
      DO 175 I = 1,SCOUNT
         IF (THISOU.EQ.SINUM(I)) J = I
 175     CONTINUE
      IF (J.EQ.0) THEN
         SCOUNT = SCOUNT + 1
         J = SCOUNT
         SINUM(J) = THISOU
         END IF
C                                       write to scratch table
      CALL OTABXX (XXFILE, 'WRIT', XXROW, BEGTIM, LSTIME, J,
     *   CREAL, CIMAG, ANTW, FRA, NREF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Clear accumulators:
      CALL RFILL (NZERO, 0.0, SUM)
      CALL RFILL (NZERO, 0.0, SUMWT)
      CALL FILL (NZERO, 0, COUNT)
C                                       Another clipping interval?
      IF (.NOT.DONE) THEN
         THISOU = CURSOU
         BEGTIM = CURTIM
         ENDTIM = CURTIM + TCLIP
         THIFRQ = CURFRQ
         THISUB = CURSUB
         LSTIME = CURTIM
         GO TO 110
         END IF
C-----------------------------------------------------------------------
C                                       Okay - close table and re-read
C                                       to fill memory
      CALL TABCLO (XXFILE, IERR)
      IF (IERR.NE.0) GO TO 990
      MTIME = XXROW - 1
      IF ((MTIME.GT.CTIME) .OR. (MTIME.GT.NTIME)) THEN
         LP = MIN (NTIME, CTIME)
         WRITE (MSGTXT,1200) MTIME, LP
         CALL MSGWRT (7)
         MTIME = LP
         END IF
      CALL OXXINI (XXFILE, 'READ', XXROW, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 200 IP = 1,MTIME
         CALL OTABXX (XXFILE, 'READ', XXROW, TIMR(1,IP), TIMR(2,IP),
     *      SNUMS(IP), AMPS(1,1,1,IP), DIFS(1,1,1,IP), WTS(1,1,1,IP),
     *      FRAS(1,1,1,IP), REFAS(1,1,IP), IERR)
         IF (IERR.NE.0) GO TO 990
 200     CONTINUE
      CALL TABCLO (XXFILE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       outer loops: antenna, source
      DO 500 LS = 1,SCOUNT
C                                       Get source name, qualifier
         THISOU = SINUM(LS)
         IF (DOSOU) THEN
            CALL SOUNFO (SUTAB, THISOU, 'SOURCE', TYPE, DIM, DUMMY,
     *         SUNAME, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL SOUNFO (SUTAB, THISOU, 'QUAL', TYPE, DIM, QUAL,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       Compute amp vect diff in DIFS
C                                       Compute amp in AMPS
         DO 280 LIF = 1,CIF
            DO 270 LP = 1,CPOL
C                                       See about re-referencing
               CALL FILL (CANT, 0, NREFS)
               DO 210 IP = 1,MTIME
                  IF (SNUMS(IP).EQ.LS) THEN
                     LA = REFAS(LIF,LP,IP)
                     IF ((LA.GT.0) .AND. (LA.LE.CANT)) NREFS(LA) =
     *                  NREFS(LA) + 1
                     END IF
 210              CONTINUE
               REFANT = 0
               I = 0
               J = 0
               DO 220 LA = 1,CANT
                  IF (NREFS(LA).GT.I) THEN
                     REFANT = LA
                     I = NREFS(LA)
                     END IF
                  IF (NREFS(LA).GT.0) J = J + 1
 220              CONTINUE
C                                       re-referencing required
               IF ((REFANT.GT.0) .AND. (J.GT.1)) THEN
                  DO 230 LA = 1,CANT
                     IF ((NREFS(LA).GT.0) .AND. (LA.NE.REFANT))
     *                  CALL REREF (CANT, CIF, CPOL, MTIME, LIF, LP, LS,
     *                  REFANT, LA, SNUMS, TIMR, AMPS, DIFS, REFAS)
 230                 CONTINUE
                  END IF
C                                       do vect difference
               DO 260 LA = 1,CANT
C                                       list points
                  NP = 0
                  DO 240 IP = 1,MTIME
                     IF (SNUMS(IP).EQ.LS) THEN
                        IF ((AMPS(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                     (DIFS(LA,LIF,LP,IP).NE.FBLANK)) THEN
                           NP = NP + 1
                           TI(NP) = (TIMR(1,IP) + TIMR(2,IP)) / 2.0
                           RE(NP) = AMPS(LA,LIF,LP,IP)
                           IM(NP) = DIFS(LA,LIF,LP,IP)
                           LIST(NP) = IP
                           END IF
                        END IF
 240                 CONTINUE
C                                       finally do it
                  DO 250 KP = 1,NP
C                                       find range of points
                     KP1 = KP
                     KP2 = KP
 245                 IF (KP1.GT.1) THEN
                        IF (TI(KP)-TI(KP1-1).LE.SCNT2) THEN
                           KP1 = KP1 - 1
                           GO TO 245
                           END IF
                        END IF
 246                 IF (KP2.LT.NP) THEN
                        IF (TI(KP2+1)-TI(KP1).LE.SCNTIM) THEN
                           KP2 = KP2 + 1
                           GO TO 246
                           END IF
                     ELSE
 247                    IF (KP1.GT.1) THEN
                           IF (TI(KP2)-TI(KP1-1).LE.SCNTIM) THEN
                              KP1 = KP1 - 1
                              GO TO 247
                              END IF
                           END IF
                        END IF
                     I = KP2 - KP1 + 1
C                                       vector diff and amp
                     MERE = MEDIUN (RE(KP1), IM(KP1), I)
                     MEIM = MEDIUN (IM(KP1), RE(KP1), I)
                     IP = LIST(KP)
                     DIFS(LA,LIF,LP,IP) = SQRT ((RE(KP)-MERE)**2 +
     *                  (IM(KP)-MEIM)**2)
                     AMPS(LA,LIF,LP,IP) = SQRT (RE(KP)**2 + IM(KP)**2)
 250                 CONTINUE
 260              CONTINUE
 270           CONTINUE
 280        CONTINUE
C                                       Look for ?? and bad data
         DO 490 IP = 1,MTIME
            IF (SNUMS(IP).EQ.LS) THEN
               CALL TODHMS (TIMR(1,IP), BEGT)
               CALL TODHMS (TIMR(2,IP), ENDT)
C                                       check for all ant questionable
               LP = 4 * CIF
               CALL RFILL (LP, 0.0, CORCNT)
               CALL RFILL (LP, 0.0, CORBAD)
               DO 420 LA = 1,CANT
                  DO 410 LIF = 1,CIF
                     DO 400 LP = 1,CPOL
                        IF ((AMPS(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                     (WTS(LA,LIF,LP,IP).NE.FBLANK)) THEN
                           CORCNT(LP,LIF) = CORCNT(LP,LIF) + 1.0
                           IF ((AMPS(LA,LIF,LP,IP).LE.ACLIP(1)) .OR.
     *                        (AMPS(LA,LIF,LP,IP).GT.ACLIP(2)) .OR.
     *                        (WTS(LA,LIF,LP,IP).LE.WCLIP(1)) .OR.
     *                        (WTS(LA,LIF,LP,IP).GT.WCLIP(2)) .OR.
     *                        (FRAS(LA,LIF,LP,IP).GT.CLCLIP(1)) .OR.
     *                        (DIFS(LA,LIF,LP,IP).GT.
     *                        MAXVD(1)+MAXVD(2)*AMPS(LA,LIF,LP,IP)))
     *                        THEN
                              CORBAD(LP,LIF) = CORBAD(LP,LIF) + 1.0
                              IF ((PRTLEV.GE.2) .AND. (DOCRT.NE.0)) THEN
                                 DBG1 = AMPS(LA,LIF,LP,IP)
                                 DBG2 = WTS(LA,LIF,LP,IP)
                                 DBG3 = DIFS(LA,LIF,LP,IP)
                                 DBG4 = FRAS(LA,LIF,LP,IP)
                                 WRITE (LINE,1400) BEGT, ENDT, LA, LIF,
     *                              LP, DBG1, DBG2, DBG3, DBG4
                                 CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                                 IF (QUIT) DOCRT = 0
                                 IF (IERR.NE.0) GO TO 990
                                 END IF
                              END IF
                           END IF
 400                    CONTINUE
 410                 CONTINUE
 420              CONTINUE
               NBAD = 0
               I = 0
               DO 440 LIF = 1,CIF
                  DO 430 LP = 1,CPOL
                     BADCOR(LP,LIF) = .FALSE.
                     IF (CORCNT(LP,LIF).GT.0.0) THEN
                        I = I + 1
                        IF (CORBAD(LP,LIF)/CORCNT(LP,LIF).GT.MAXBAD(2))
     *                     THEN
                           NBAD = NBAD + 1
                           BADCOR(LP,LIF) = .TRUE.
                           END IF
                        END IF
 430                 CONTINUE
 440              CONTINUE
               TEMP = I
               TEMP = NBAD / TEMP
C                                       If bad tell about it
               IF ((NBAD.GT.0) .AND. (DOCRT.NE.0)) THEN
                  WRITE (LINE,1440) BEGT, ENDT, SUNAME, QUAL
                  I = 50
                  DO 450 LIF = 1,CIF
                     DO 449 LP = 1,CPOL
                        I = I + 1
                        IF (I.LE.132) THEN
                           IF (BADCOR(LP,LIF)) THEN
                              LINE(I:I) = 'T'
                           ELSE IF (CORCNT(LP,LIF).LE.0) THEN
                              LINE(I:I) = '-'
                           ELSE
                              LINE(I:I) = 'F'
                              END IF
                           END IF
 449                    CONTINUE
 450                 CONTINUE
                  CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                  IF (QUIT) DOCRT = 0
                  IF (IERR.NE.0) GO TO 990
                  END IF
               IF (NBAD.GT.0) NPRINT = NPRINT + 1
C                                       Flag everything
               IF (TEMP.GT.MAXBAD(1)) THEN
                  NFLAG1 = NFLAG1 + 1
                  PFLAGS(1) = .TRUE.
                  PFLAGS(2) = .TRUE.
                  PFLAGS(3) = .TRUE.
                  PFLAGS(4) = .TRUE.
                  SOURID = SINUM(LS)
                  SUBA = THISUB
                  FREQID = THIFRQ
                  IFS(1) = 0
                  IFS(2) = 0
                  TIMER(1) = TIMR(1,IP) - TEPS
                  TIMER(2) = TIMR(2,IP) + TEPS
                  ANTS(1) = 0
                  ANTS(2) = 0
                  CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID, SUBA,
     *               FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON,
     *               IERR)
                  IF (IERR.NE.0) GO TO 990
C                                       an IF at a time
               ELSE
                  DFLAGS(1) = .FALSE.
                  DFLAGS(2) = .FALSE.
                  DFLAGS(3) = .FALSE.
                  DFLAGS(4) = .FALSE.
                  DO 480 LIF = 1,CIF
                     PFLAGS(1) = BADCOR(1,LIF)
                     PFLAGS(2) = BADCOR(2,LIF)
                     PFLAGS(3) = BADCOR(3,LIF)
                     PFLAGS(4) = BADCOR(4,LIF)
                     AFLAGS(1) = .FALSE.
                     AFLAGS(2) = .FALSE.
                     AFLAGS(3) = .FALSE.
                     AFLAGS(4) = .FALSE.
                     IF ((PFLAGS(1)) .OR. (PFLAGS(2)) .OR. (PFLAGS(3))
     *                  .OR. (PFLAGS(4))) THEN
                        SOURID = SINUM(LS)
                        SUBA = THISUB
                        FREQID = THIFRQ
C                                       cross flags
                        DO 455 LP = 1,4
                           DO 454 LL = 1,4
                              IF (PFLAGS(LL)) AFLAGS(LP) = AFLAGS(LP)
     *                           .OR. XPOLS(LL,LP)
 454                          CONTINUE
 455                       CONTINUE
                        IF (.NOT.DOIFS) THEN
                           IFS(1) = LIF
                           IFS(2) = LIF
                           END IF
                        TIMER(1) = TIMR(1,IP) - TEPS
                        TIMER(2) = TIMR(2,IP) + TEPS
                        ANTS(1) = 0
                        ANTS(2) = 0
                        CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID, SUBA,
     *                     FREQID, ANTS, TIMER, IFS, CHANS, AFLAGS,
     *                     REASON, IERR)
                        IF (IERR.NE.0) GO TO 990
                        NFLAG2 = NFLAG2 + 1
                        IF (DOIFS) THEN
                           IF (AFLAGS(1)) DFLAGS(1) = .TRUE.
                           IF (AFLAGS(2)) DFLAGS(2) = .TRUE.
                           IF (AFLAGS(3)) DFLAGS(3) = .TRUE.
                           IF (AFLAGS(4)) DFLAGS(4) = .TRUE.
                           IF ((DFLAGS(1)) .AND. (DFLAGS(2)) .AND.
     *                        (DFLAGS(3)) .AND. (DFLAGS(4))) GO TO 490
                           END IF
                        END IF
C                                       one antenna at a time
                     IF ((.NOT.AFLAGS(1)) .OR. (.NOT.AFLAGS(2)) .OR.
     *                  (.NOT.AFLAGS(3)) .OR. (.NOT.AFLAGS(4))) THEN
                        DO 470 LA = 1,CANT
                           DO 460 LP = 1,CPOL
                              PFLAGS(LP) = .FALSE.
                              IF ((AMPS(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                           (WTS(LA,LIF,LP,IP).NE.FBLANK)) THEN
                                 IF ((AMPS(LA,LIF,LP,IP).LE.ACLIP(3))
     *                              .OR.(AMPS(LA,LIF,LP,IP).GT.ACLIP(4))
     *                              .OR. (WTS(LA,LIF,LP,IP).LE.WCLIP(3))
     *                              .OR. (WTS(LA,LIF,LP,IP).GT.WCLIP(4))
     *                              .OR. (FRAS(LA,LIF,LP,IP).GT.
     *                              CLCLIP(2)) .OR.
     *                              (DIFS(LA,LIF,LP,IP).GT.MAXVD(3)
     *                              + MAXVD(4)*AMPS(LA,LIF,LP,IP))) THEN
                                    PFLAGS(LP) = .TRUE.
                                    IF ((PRTLEV.GE.1) .AND.
     *                                 (DOCRT.NE.0)) THEN
                                       DBG1 = AMPS(LA,LIF,LP,IP)
                                       DBG2 = WTS(LA,LIF,LP,IP)
                                       DBG3 = DIFS(LA,LIF,LP,IP)
                                       DBG4 = FRAS(LA,LIF,LP,IP)
                                       WRITE (LINE,1450) BEGT, ENDT, LA,
     *                                    LIF, LP, DBG1, DBG2, DBG3,
     *                                    DBG4
                                       CALL PRTWRI (PRINT, LINE, QUIT,
     *                                    IERR)
                                       IF (QUIT) DOCRT = 0
                                       IF (IERR.NE.0) GO TO 990
                                       END IF
                                    END IF
                                 END IF
 460                          CONTINUE
                           IF ((PFLAGS(1)) .OR. (PFLAGS(2)) .OR.
     *                        (PFLAGS(3)) .OR. (PFLAGS(4))) THEN
                              SOURID = SINUM(LS)
                              SUBA = THISUB
                              FREQID = THIFRQ
C                                       cross flags
                              DO 465 LP = 1,4
                                 AFLAGS(LP) = .FALSE.
                                 DO 464 LL = 1,4
                                    IF (PFLAGS(LL)) AFLAGS(LP) =
     *                                 AFLAGS(LP) .OR. XPOLS(LL,LP)
 464                                CONTINUE
 465                             CONTINUE
                              IF (.NOT.DOIFS) THEN
                                 IFS(1) = LIF
                                 IFS(2) = LIF
                                 END IF
                              TIMER(1) = TIMR(1,IP) - TEPS
                              TIMER(2) = TIMR(2,IP) + TEPS
                              ANTS(1) = LA
                              ANTS(2) = 0
                              CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID,
     *                           SUBA, FREQID, ANTS, TIMER, IFS, CHANS,
     *                           AFLAGS, REASON, IERR)
                              IF (IERR.NE.0) GO TO 990
                              NFLAG3 = NFLAG3 + 1
                              END IF
 470                       CONTINUE
                        END IF
 480                 CONTINUE
                  END IF
               END IF
 490        CONTINUE
 500     CONTINUE
C-----------------------------------------------------------------------
C                                       close FG table
      CALL TABCLO (FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       history
      WRITE (HILINE,1700) MAXVD(1), MAXVD(2)
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1701) MAXVD(3), MAXVD(4)
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1711) NFLAG1
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1712) NFLAG2
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1713) NFLAG3
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close file
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.DOCAT) THEN
         CALL TABZAP (XXFILE, IERR)
         XXFILE = ' '
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Delete temporary table
      IF (DOSOU) THEN
         CALL DESTRY (SUTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       report
      NBAD = NFLAG1 + NFLAG2 + NFLAG3
      WRITE (MSGTXT,1800) NPRINT, NBAD
      CALL MSGWRT (5)
      IF (NBAD.GT.0) THEN
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1801) NFLAG1
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1802) NFLAG2
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1803) NFLAG3
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         IERR = 0
         END IF
C                                       Close printer
      IF (PRINT.NE.' ') THEN
         CALL OCLOSE (PRINT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'AMVDIF: ERROR IN RFI DETECTION FOR ' // UVIN
      CALL MSGWRT (7)
      IF ((.NOT.DOCAT) .AND. (XXFILE.NE.' ')) CALL TABZAP (XXFILE, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('For ''',A,''' data: VDIF clipping coef.=',F8.4,F7.4)
 1001 FORMAT ('For ''',A,''' data: amp clipping range =',F8.4,F9.4)
 1002 FORMAT ('For ''',A,''' data: wt clipping range  =',2(1PE10.3))
 1003 FORMAT ('For ''',A,''' data: closure error fraction',F7.4)
 1005 FORMAT ('Max. fract. questionable corrs.=',F6.3,'  antennas=',
     *   F6.3)
 1200 FORMAT ('READ',I6,' TIMES, WILL DO ONLY',I5)
 1400 FORMAT (I3,'/',2(I2.2,':'),I2.2,' -',I3,'/',2(I2.2,':'),I2.2,
     *   2I3,I2,' ??? ',3(1PE11.3),0PF7.3)
 1440 FORMAT (I3,'/',2(I2.2,':'),I2.2,' -',I3,'/',2(I2.2,':'),I2.2,1X,
     *   A,':',I5.5,1X,2F9.5,'(',I2.2,')')
 1450 FORMAT (I3,'/',2(I2.2,':'),I2.2,' -',I3,'/',2(I2.2,':'),I2.2,
     *   2I3,I2,' bad ',3(1PE11.3),0PF7.3)
 1700 FORMAT ('MAXVDIF(1)=',F8.4,',',F7.4,' / Vdif Jy,gain for',
     *   ' questionable')
 1701 FORMAT ('MAXVDIF(2)=',F7.4,',',F6.4,' / Vdif Jy,gain for',
     *   ' bad antenna')
 1711 FORMAT ('FLAGD(1) =',I8,' / integrations fully flagged')
 1712 FORMAT ('FLAGD(2) =',I8,' / corr/integrations fully flagged')
 1713 FORMAT ('FLAGD(3) =',I8,' / antennas/integrations flagged')
 1800 FORMAT ('Found',I6,' bad times,',I6,' total flags')
 1801 FORMAT ('Wrote',I6,' antenna & IF independent flags')
 1802 FORMAT ('Wrote',I6,' antenna independent, IF dependent flags')
 1803 FORMAT ('Wrote',I6,' antenna & IF dependent flags')
      END
      SUBROUTINE AMVRFI (UVIN, PRINT, FGTAB, CANT, CIF, CPOL, CCHAN,
     *   CTIME, DOPOLS, XPOLS, SUM, SUMWT, COUNT, IERR)
C-----------------------------------------------------------------------
C   Look for bad data via amplitude of the vector difference of the
C   current sample from a running mean of the samples - baseline based
C   Inputs:
C      UVIN      C*?   Name of input uvdata object.
C      PRINT     C*?   Print object
C      FGTAB     C*?   Output flag table object
C      CANT      I     Maximum antenna number
C      CPOL      I     Number of polarizations
C      CIF       I     Number of IFs
C      CCHAN     I     Number of spactral channels
C      CTIME     I     Limit on number of times
C      DOPOLS    L(4)  Which polarizations to test
C      XPOLS     L(4,4)  Which pols to flag (i,j) if i bad flag j
C   Scratch variables
C      SUM, SUMWT, COUNT
C   Inputs attached to UVIN:
C      MAXRMS    R(*)  Maximum RMS allowed, constant plus amplitude
C                      coefficient.
C      MAXBAD    R(2)  Maximum allowed fraction of bad baselines.
C      TIMEAVG   R     Time in seconds for clipping interval
C      DOIFS     L     Flag all IFs if one bad
C      PRTLEV    I     Print level for debugging
C   Output:
C      IERR      I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), PRINT*(*), FGTAB*(*)
      INTEGER   CANT, CIF, CPOL, CCHAN, CTIME, IERR
      REAL      SUM(CANT,CANT,CIF,CPOL,*), SUMWT(CANT,CANT,CIF,CPOL,*),
     *   COUNT(CANT,CANT,CIF,CPOL)
      LOGICAL   DOPOLS(4), XPOLS(4,4)
C
      REAL      MAXBAD(2), TCLIP, ACLIP(4), WCLIP(4), MAXVD(4), SCNTIM
      LOGICAL   DOIFS, DOCHNS
      INTEGER   PRTLEV
C
      INTEGER   NTIME
      PARAMETER (NTIME = 15001)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   TYPE, DIM(7), DUMMY, ANT1, ANT2, ENDVIS, VISNO, CURSOU,
     *   THISOU, TBVER, DOCRT, I, BEGT(4), ENDT(4), NACROS, ILOCU,
     *   ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ, ILOCA1, ILOCA2,
     *  ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR, JLOCD, JLOCIF, INCS, INCF,
     *   INCIF, LP, LIF, IP, NPRINT, NFLAG1, NFLAG2, NFLAG3, FGROW,
     *   SOURID, SUBA, FREQID, ANTS(2), IFS(2), CHANS(2), THISUB,
     *   THIFRQ, NBAD, CURSUB, CURFRQ, NZERO, ID(3), IT(3), QUAL, NP,
     *   KP, KP1, KP2, IP1, IP2, IPMAX, II, LL, NPP(2)
      LOGICAL   BADCOR(4,MAXIF), DONE, DOSOU, QUIT, DOCAL, PFLAGS(4),
     *   DFLAGS(4), AFLAGS(4), SCDONE, DOROBU
      CHARACTER SUTAB*32, SUNAME*16, LINE*132, CDUMMY*1, REASON*24,
     *   ATIME*8, ADATE*12, HILINE*72
      REAL      RP(50), TEPS, CORCNT(2,MAXIF), CORBAD(2,MAXIF),
     *   CURTIM, LSTIME, ENDTIM, BEGTIM, TEMP, TIMER(2), T2, AMP, WT,
     *   VIS(3,MAXCIF), TIMR(2,NTIME), RE(NTIME), IM(NTIME), MERE,
     *   MEIM, MEDIUN, SCNT2, DIF, TDUMMY(NTIME,3), WP(2), SP(2),
     *   SSP(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      COMMON /TBUFS/ TIMR, RE, IM, TDUMMY
      COMMON /VBUFS/ VIS
C-----------------------------------------------------------------------
      TEPS = 0.02 / (3600.0 * 24.0)
C                                       Open printer
      NPRINT = 0
      NFLAG1 = 0
      NFLAG2 = 0
      NFLAG3 = 0
      IF (PRINT.NE.' ') THEN
         CALL OOPEN (PRINT, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (PRINT, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (PRINT, 'NACROS', TYPE, DIM, NACROS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         DOCRT = 0
         END IF
      CALL ZDATE (ID)
      ID(1) = - ABS (ID(1))
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      REASON = TSKNAM // ADATE(:9) // ' ' // ATIME
      IFS(1) = 0
      IFS(2) = 0
      THISOU = 0
      THISUB = 0
      THIFRQ = 0
      CURSUB = 0
      CURFRQ = 0
C                                       Retrieve pointers into the data:
      CALL UVDPNT (UVIN, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IERR)
      IF (IERR.NE.0) GO TO 999
      INCS = INCS / 3
      INCF = INCF / 3
      INCIF = INCIF / 3
C                                       Open input.
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get parameters
      CALL OUVGET (UVIN, 'DOROBUST', TYPE, DIM, DOROBU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'WTCLIP', TYPE, DIM, WCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'AMPCLIP', TYPE, DIM, ACLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'MAXBAD', TYPE, DIM, MAXBAD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'MAXRMS', TYPE, DIM, MAXVD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'DOCHANS', TYPE, DIM, DOCHNS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'DOIFS', TYPE, DIM, DOIFS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'PRTLEV', TYPE, DIM, PRTLEV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'SCANLENG', TYPE, DIM, SCNTIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      SCNTIM = SCNTIM / 86400.0
      SCNT2 = SCNTIM / 2.0
      CALL OUVGET (UVIN, 'TIMEAVG', TYPE, DIM, TCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      TCLIP = TCLIP / 86400.0
      IF (.NOT.DOCHNS) THEN
         CALL OUVGET (UVIN, 'CALEDIT.BCHAN', TYPE, DIM, CHANS(1),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVGET (UVIN, 'CALEDIT.ECHAN', TYPE, DIM, CHANS(2),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CHANS(1) = 0
         CHANS(2) = 0
         END IF
C                                       Processing info
      CALL OUVGET (UVIN, 'CALEDIT.DOCAL', TYPE, DIM, DOCAL, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DOCRT.NE.0) THEN
         IF (DOCRT.GT.-3) THEN
            IF (DOCAL) THEN
               LINE = 'Calibration applied'
            ELSE
               LINE = 'NO Calibration applied'
               END IF
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            WRITE (LINE,1000) 'questionable', MAXVD(1), MAXVD(2)
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((ACLIP(1).GT.0.0) .OR. (ACLIP(2).LT.1.E5)) THEN
               WRITE (LINE,1001) 'questionable', ACLIP(1), ACLIP(2)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((WCLIP(1).GT.0.0) .OR. (WCLIP(2).LT.1.E9)) THEN
               WRITE (LINE,1002) 'questionable', WCLIP(1), WCLIP(2)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            WRITE (LINE,1005) MAXBAD
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            WRITE (LINE,1000) 'bad', MAXVD(3), MAXVD(4)
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((ACLIP(3).GT.0.0) .OR. (ACLIP(4).LT.1.E5)) THEN
               WRITE (LINE,1001) 'bad', ACLIP(3), ACLIP(4)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            IF ((WCLIP(3).GT.0.0) .OR. (WCLIP(4).LT.1.E9)) THEN
               WRITE (LINE,1002) 'bad', WCLIP(3), WCLIP(4)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            END IF
C                                       Page labels
         IF (PRTLEV.GE.2) THEN
            LINE = 'Individual data warnings and flags'
         ELSE IF (PRTLEV.GE.1) THEN
            LINE = 'Data to be flagged individually'
         ELSE
            LINE = 'Data flagged independent of antenna'
            END IF
         DIM(1) = LEN (LINE)
         DIM(2) = 1
         CALL OPUT (PRINT, 'TITLE1', OOACAR, DIM, DUMMY, LINE, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL PRTWRI (PRINT, LINE, QUIT, IERR)
         IF (QUIT) DOCRT = 0
         IF (IERR.NE.0) GO TO 990
         IF (PRTLEV.GE.1) THEN
            LINE = '         Time range        A1 A2 IF P       Amp' //
     *         '        Weight     Vdif'
         ELSE
            LINE = '         Time range        Source           Qual' //
     *         '  Corr flags'
            END IF
         CALL OPUT (PRINT, 'TITLE2', OOACAR, DIM, DUMMY, LINE, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (DOCRT.NE.0) CALL PRTWRI (PRINT, LINE, QUIT, IERR)
         IF (QUIT) DOCRT = 0
         IF (IERR.NE.0) GO TO 990
         END IF
      DOSOU = (ILOCSU.GT.0)
      IERR = 0
C                                       Source table/name
      IF (DOSOU) THEN
         SUTAB = 'SoUrce table for ANTIME'
         TBVER = 1
         CALL UV2TAB (UVIN, SUTAB, 'SU', TBVER, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       For single source use 'OBJECT'
      ELSE
         CALL UVDGET (UVIN, 'OBJECT', TYPE, DIM, DUMMY, SUNAME, IERR)
         IF (IERR.NE.0) GO TO 990
         QUAL = 0
         END IF
C                                       Clear accumulators:
C                                       (1,*) =  count
C                                       (2,*) =  sum r then max rms**2
C                                       (3,*) =  sum**2 r then rms**2
C                                       (4,*) =  sum imaginary
C                                       (5,*) =  sum**2 imaginary
C                                       (6,*) =  sum amplitude
      IP1 = 1
      IP2 = 1
      IP = 1
      NZERO = CANT * CANT * CIF * CPOL
      CALL RFILL (NZERO, 0.0, SUM(1,1,1,1,IP))
      CALL RFILL (NZERO, 0.0, SUMWT(1,1,1,1,IP))
      CALL RFILL (NZERO, 0.0, COUNT)
C                                       Initialize visibility count
      DONE = .FALSE.
      VISNO = 0
C                                       Open FG table
      CALL OFGINI (FGTAB, 'WRIT', FGROW, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Swallow input collecting
C                                       statistics.
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VIS, IERR)
         IF (IERR.LT.0) THEN
            DONE = .TRUE.
            IERR = 0
            END IF
         IF (IERR.GT.0) GO TO 990
         VISNO = VISNO + 1
 110     CURTIM = RP(ILOCT)
         IF (DOSOU) THEN
            CURSOU = RP(ILOCSU) + 0.5
         ELSE
            CURSOU = 0
            END IF
         IF (ILOCFQ.GT.0) CURFRQ = RP(ILOCFQ) + 0.5
C                                       Get antenna numbers
         IF (ILOCB.GT.0) THEN
            ANT1 = (RP(ILOCB) / 256.0) + 0.001
            ANT2 = (RP(ILOCB) - ANT1 * 256) + 0.001
            CURSUB = (RP(ILOCB) - 256 * ANT1 - ANT2) * 100.0 + 1.1
         ELSE
            ANT1 = RP(ILOCA1) + 0.001
            ANT2 = RP(ILOCA2) + 0.001
            CURSUB = RP(ILOCSA) + 0.001
            END IF
C                                       Initial selection.
         IF (VISNO.EQ.1) THEN
            THIFRQ = CURFRQ
            THISUB = CURSUB
            THISOU = CURSOU
            BEGTIM = CURTIM
            ENDTIM = CURTIM + TCLIP
            LSTIME = CURTIM
            END IF
C                                       Finished with data or interval?
         SCDONE = DONE .OR. (CURSOU.NE.THISOU) .OR. (CURSUB.NE.THISUB)
     *      .OR. (CURTIM.GT.LSTIME+TCLIP)
         IF ((SCDONE) .OR. (CURTIM.GT.ENDTIM)) GO TO 150
C                                       Set last vis of interval
         ENDVIS = VISNO
C                                       Set last time
         LSTIME = CURTIM
C                                       Baseline index
         DO 130 LP = 1,CPOL
            IF (.NOT.DOPOLS(LP)) GO TO 130
            DO 125 LIF = 1,CIF
               II = (LP-1) * INCS + (LIF-1) * INCIF + 1
               CALL AVERAG (CCHAN, VIS(1,II), INCF, ANT1, ANT2, .FALSE.,
     *            DOROBU, NPP, WP, SP, SSP)
               COUNT(ANT1,ANT2,LIF,LP) = COUNT(ANT1,ANT2,LIF,LP) +
     *            NPP(1)
               SUMWT(ANT1,ANT2,LIF,LP,IP) = SUMWT(ANT1,ANT2,LIF,LP,IP) +
     *            WP(1)
               SUM(ANT1,ANT2,LIF,LP,IP) = SUM(ANT1,ANT2,LIF,LP,IP) +
     *            SP(1)
               COUNT(ANT2,ANT1,LIF,LP) = COUNT(ANT2,ANT1,LIF,LP) +
     *            NPP(2)
               SUMWT(ANT2,ANT1,LIF,LP,IP) = SUMWT(ANT2,ANT1,LIF,LP,IP) +
     *            WP(2)
               SUM(ANT2,ANT1,LIF,LP,IP) = SUM(ANT2,ANT1,LIF,LP,IP) +
     *            SP(2)
 125           CONTINUE
 130        CONTINUE
C                                       Next vis until done
         IF (.NOT.DONE) GO TO 100
C                                       save times at end of interval
 150   TIMR(1,IP) = BEGTIM
       TIMR(2,IP) = LSTIME
C                                       Average
       DO 170 LIF = 1,CIF
          DO 165 LP = 1,CPOL
             DO 160 ANT2 = 1,CANT
               DO 155 ANT1 = 1,CANT
                  IF (SUMWT(ANT1,ANT2,LIF,LP,IP).GT.0.0) THEN
                     SUM(ANT1,ANT2,LIF,LP,IP) = SUM(ANT1,ANT2,LIF,LP,IP)
     *                  / SUMWT(ANT1,ANT2,LIF,LP,IP)
                     SUMWT(ANT1,ANT2,LIF,LP,IP) =
     *                  SUMWT(ANT1,ANT2,LIF,LP,IP) /
     *                  COUNT(ANT1,ANT2,LIF,LP)
                     END IF
 155              CONTINUE
 160           CONTINUE
 165        CONTINUE
 170     CONTINUE
C                                       process what we have
      IF ((DONE) .OR. (CURSOU.NE.THISOU) .OR. (IP.GE.CTIME) .OR.
     *   (IP.GT.NTIME) .OR. (CURSUB.NE.THISUB)) THEN
         IPMAX = IP
C                                       do to end
         IF ((DONE) .OR. (CURSOU.NE.THISOU) .OR. (CURSUB.NE.THISUB))
     *      THEN
            IP2 = IPMAX
C                                       will need to find the max to do
         ELSE
            TEMP = (TIMR(1,IPMAX) + TIMR(2,IPMAX)) / 2.0
            DO 175 IP = IP1,IPMAX
               IF ((TEMP-(TIMR(1,IP)+TIMR(2,IP))/2.0).GT.SCNT2) IP2 = IP
 175           CONTINUE
            END IF
         IF (DOSOU) THEN
            CALL SOUNFO (SUTAB, THISOU, 'SOURCE', TYPE, DIM, DUMMY,
     *         SUNAME, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL SOUNFO (SUTAB, THISOU, 'QUAL', TYPE, DIM, QUAL,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       loop over times of interest
         DO 330 IP = IP1,IP2
            CALL TODHMS (TIMR(1,IP), BEGT)
            CALL TODHMS (TIMR(2,IP), ENDT)
            CALL RFILL (NZERO, FBLANK, COUNT)
C                                       time range to cover here
            TEMP = (TIMR(1,IP) + TIMR(2,IP)) / 2.0
            KP1 = IP
            KP2 = IP
 180        IF (KP1.GT.1) THEN
               T2 = (TIMR(1,KP1-1) + TIMR(2,KP1-1)) / 2.0
               IF (TEMP-T2.LE.SCNT2) THEN
                  KP1 = KP1 - 1
                  GO TO 180
                  END IF
               END IF
 185        TEMP = (TIMR(1,KP1) + TIMR(2,KP1)) / 2.0
            IF (KP2.LT.IPMAX) THEN
               T2 = (TIMR(1,KP2+1) + TIMR(2,KP2+1)) / 2.0
               IF (T2-TEMP.LE.SCNTIM) THEN
                  KP2 = KP2 + 1
                  GO TO 185
                  END IF
            ELSE
               TEMP = (TIMR(1,KP2) + TIMR(2,KP2)) / 2.0
 186           IF (KP1.GT.1) THEN
                  T2 = (TIMR(1,KP1-1) + TIMR(2,KP1-1)) / 2.0
                  IF (TEMP-T2.LE.SCNTIM) THEN
                     KP1 = KP1 - 1
                     GO TO 186
                     END IF
                  END IF
               END IF
C                                       find all correlators Vdif
            DO 230 ANT1 = 1,CANT-1
               DO 225 ANT2 = ANT1+1,CANT
                  DO 220 LIF = 1,CIF
                     DO 215 LP = 1,CPOL
                        IF (SUMWT(ANT1,ANT2,LIF,LP,IP).GT.0.0) THEN
                           NP = 0
                           DO 210 KP = KP1,KP2
                              IF (SUMWT(ANT1,ANT2,LIF,LP,KP).GT.0.0)
     *                           THEN
                                 NP = NP + 1
                                 RE(NP) = SUM(ANT1,ANT2,LIF,LP,KP)
                                 IM(NP) = SUM(ANT2,ANT1,LIF,LP,KP)
                                 END IF
 210                          CONTINUE
                           MERE = MEDIUN (RE, IM, NP)
                           MEIM = MEDIUN (IM, RE, NP)
                           COUNT(ANT1,ANT2,LIF,LP) = SQRT
     *                        ((SUM(ANT1,ANT2,LIF,LP,IP)-MERE)**2 +
     *                        (SUM(ANT2,ANT1,LIF,LP,IP)-MEIM)**2)
                           COUNT(ANT2,ANT1,LIF,LP) = SQRT
     *                        (SUM(ANT1,ANT2,LIF,LP,IP)**2 +
     *                        SUM(ANT2,ANT1,LIF,LP,IP)**2)
                           END IF
 215                    CONTINUE
 220                 CONTINUE
 225              CONTINUE
 230           CONTINUE
C                                       check for all ant questionable
            LP = 4 * CIF
            CALL RFILL (LP, 0.0, CORCNT)
            CALL RFILL (LP, 0.0, CORBAD)
            DO 260 ANT1 = 1,CANT-1
               DO 255 ANT2 = ANT1+1,CANT
                  DO 250 LIF = 1,CIF
                     DO 245 LP = 1,CPOL
                        WT = SUMWT(ANT1,ANT2,LIF,LP,IP)
                        IF (WT.GT.0.0) THEN
                           AMP = COUNT(ANT2,ANT1,LIF,LP)
                           DIF = COUNT(ANT1,ANT2,LIF,LP)
                           IF ((AMP.LE.ACLIP(1)) .OR. (AMP.GT.ACLIP(2))
     *                        .OR. (WT.LE.WCLIP(1)) .OR.(WT.GT.WCLIP(2))
     *                        .OR. (DIF.GT.MAXVD(1)+MAXVD(2)*AMP)) THEN
                              CORBAD(LP,LIF) = CORBAD(LP,LIF) + 1.0
                              IF ((PRTLEV.GE.2) .AND. (DOCRT.NE.0)) THEN
                                 WRITE (LINE,1230) BEGT, ENDT, ANT1,
     *                              ANT2, LIF, LP, AMP, WT, DIF
                                 CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                                 IF (QUIT) DOCRT = 0
                                 IF (IERR.NE.0) GO TO 990
                                 END IF
                              END IF
                           END IF
 245                    CONTINUE
 250                 CONTINUE
 255              CONTINUE
 260           CONTINUE
            NBAD = 0
            I = 0
            DO 270 LIF = 1,CIF
               DO 265 LP = 1,CPOL
                  BADCOR(LP,LIF) = .FALSE.
                  IF (CORCNT(LP,LIF).GT.0.0) THEN
                     I = I + 1
                     IF (CORBAD(LP,LIF)/CORCNT(LP,LIF).GT.MAXBAD(2))
     *                  THEN
                        NBAD = NBAD + 1
                        BADCOR(LP,LIF) = .TRUE.
                        END IF
                     END IF
 265              CONTINUE
 270           CONTINUE
            TEMP = I
            TEMP = NBAD / TEMP
C                                       If bad tell about it
            IF ((NBAD.GT.0) .AND. (DOCRT.NE.0)) THEN
               WRITE (LINE,1270) BEGT, ENDT, SUNAME, QUAL
               I = 50
               DO 275 LIF = 1,CIF
                  DO 274 LP = 1,CPOL
                     I = I + 1
                     IF (I.LE.132) THEN
                        IF (BADCOR(LP,LIF)) THEN
                           LINE(I:I) = 'T'
                        ELSE IF (CORCNT(LP,LIF).LE.0) THEN
                           LINE(I:I) = '-'
                        ELSE
                           LINE(I:I) = 'F'
                           END IF
                        END IF
 274                 CONTINUE
 275              CONTINUE
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            IF (NBAD.GT.0) NPRINT = NPRINT + 1
C                                       Flag everything
            IF (TEMP.GT.MAXBAD(1)) THEN
               NFLAG1 = NFLAG1 + 1
               PFLAGS(1) = .TRUE.
               PFLAGS(2) = .TRUE.
               PFLAGS(3) = .TRUE.
               PFLAGS(4) = .TRUE.
               SOURID = THISOU
               SUBA = THISUB
               FREQID = THIFRQ
               IFS(1) = 0
               IFS(2) = 0
               TIMER(1) = TIMR(1,IP) - TEPS
               TIMER(2) = TIMR(2,IP) + TEPS
               ANTS(1) = 0
               ANTS(2) = 0
               CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID, SUBA, FREQID,
     *            ANTS, TIMER, IFS, CHANS, PFLAGS, REASON,IERR)
               IF (IERR.NE.0) GO TO 990
C                                       an IF at a time
            ELSE
               DFLAGS(1) = .FALSE.
               DFLAGS(2) = .FALSE.
               DFLAGS(3) = .FALSE.
               DFLAGS(4) = .FALSE.
               DO 310 LIF = 1,CIF
                  PFLAGS(1) = BADCOR(1,LIF)
                  PFLAGS(2) = BADCOR(2,LIF)
                  PFLAGS(3) = BADCOR(3,LIF)
                  PFLAGS(4) = BADCOR(4,LIF)
                  AFLAGS(1) = .FALSE.
                  AFLAGS(2) = .FALSE.
                  AFLAGS(3) = .FALSE.
                  AFLAGS(4) = .FALSE.
                  IF ((PFLAGS(1)) .OR. (PFLAGS(2)) .OR. (PFLAGS(3)) .OR.
     *               (PFLAGS(4))) THEN
                     SOURID = THISOU
                     SUBA = THISUB
                     FREQID = THIFRQ
C                                       cross flags
                     DO 280 LP = 1,4
                        DO 279 LL = 1,4
                           IF (PFLAGS(LL)) AFLAGS(LP) = AFLAGS(LP) .OR.
     *                        XPOLS(LL,LP)
 279                       CONTINUE
 280                    CONTINUE
                     IF (.NOT.DOIFS) THEN
                        IFS(1) = LIF
                        IFS(2) = LIF
                        END IF
                     TIMER(1) = TIMR(1,IP) - TEPS
                     TIMER(2) = TIMR(2,IP) + TEPS
                     ANTS(1) = 0
                     ANTS(2) = 0
                     CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID, SUBA,
     *                  FREQID, ANTS, TIMER, IFS, CHANS, AFLAGS, REASON,
     *                  IERR)
                     IF (IERR.NE.0) GO TO 990
                     NFLAG2 = NFLAG2 + 1
                     IF (DOIFS) THEN
                        IF (AFLAGS(1)) DFLAGS(1) = .TRUE.
                        IF (AFLAGS(2)) DFLAGS(2) = .TRUE.
                        IF (AFLAGS(3)) DFLAGS(2) = .TRUE.
                        IF (AFLAGS(4)) DFLAGS(4) = .TRUE.
                        IF ((DFLAGS(1)) .AND. (DFLAGS(2)) .AND.
     *                     (DFLAGS(3)) .AND. (DFLAGS(4))) GO TO 330
                        END IF
                     END IF
C                                       one baseline at a time
                  IF ((.NOT.AFLAGS(1)) .OR. (.NOT.AFLAGS(2)) .OR.
     *               (.NOT.AFLAGS(3)) .OR. (.NOT.AFLAGS(4))) THEN
                     DO 300 ANT1 = 1,CANT-1
                        DO 295 ANT2 = ANT1+1,CANT
                           DO 290 LP = 1,CPOL
                              PFLAGS(LP) = .FALSE.
                              WT = SUMWT(ANT1,ANT2,LIF,LP,IP)
                              IF (WT.GT.0.0) THEN
                                 AMP = COUNT(ANT2,ANT1,LIF,LP)
                                 DIF = COUNT(ANT1,ANT2,LIF,LP)
                                 IF ((AMP.LE.ACLIP(1)) .OR.
     *                              (AMP.GT.ACLIP(2)) .OR.
     *                              (WT.LE.WCLIP(1)) .OR.
     *                              (WT.GT.WCLIP(2)) .OR.
     *                              (DIF.GT.MAXVD(3)+MAXVD(4)*AMP))
     *                              THEN
                                    PFLAGS(LP) = .TRUE.
                                    IF ((PRTLEV.GE.1) .AND.
     *                                 (DOCRT.NE.0)) THEN
                                       WRITE (LINE,1290) BEGT, ENDT,
     *                                    ANT1, ANT2, LIF,LP, AMP,
     *                                    WT, DIF
                                       CALL PRTWRI (PRINT, LINE, QUIT,
     *                                    IERR)
                                       IF (QUIT) DOCRT = 0
                                       IF (IERR.NE.0) GO TO 990
                                       END IF
                                    END IF
                                 END IF
 290                          CONTINUE
                           IF ((PFLAGS(1)) .OR. (PFLAGS(2)) .OR.
     *                        (PFLAGS(3)) .OR. (PFLAGS(4))) THEN
                              SOURID = THISOU
                              SUBA = THISUB
                              FREQID = THIFRQ
C                                       cross flags
                              DO 292 LP = 1,4
                                 AFLAGS(LP) = .FALSE.
                                 DO 291 LL = 1,4
                                    IF (PFLAGS(LL)) AFLAGS(LP) =
     *                                 AFLAGS(LP) .OR. XPOLS(LL,LP)
 291                                CONTINUE
 292                             CONTINUE
                              IF (.NOT.DOIFS) THEN
                                 IFS(1) = LIF
                                 IFS(2) = LIF
                                 END IF
                              TIMER(1) = TIMR(1,IP) - TEPS
                              TIMER(2) = TIMR(2,IP) + TEPS
                              ANTS(1) = ANT1
                              ANTS(2) = ANT2
                              CALL OTABFG (FGTAB, 'WRIT', FGROW, SOURID,
     *                           SUBA, FREQID, ANTS, TIMER,IFS, CHANS,
     *                           AFLAGS, REASON, IERR)
                              IF (IERR.NE.0) GO TO 990
                              NFLAG3 = NFLAG3 + 1
                              END IF
 295                       CONTINUE
 300                    CONTINUE
                     END IF
 310              CONTINUE
               END IF
 330        CONTINUE
C                                       shift buffers down
         IF (IP2.LT.IPMAX) THEN
            KP1 = IP2 + 1
            TEMP = (TIMR(1,KP1) + TIMR(2,KP1)) / 2.0
 335        IF (KP1.GT.1) THEN
               T2 = (TIMR(1,KP1-1) +TIMR(2,KP1-1)) / 2.0
               IF (TEMP-T2.LE.SCNT2) THEN
                  KP1 = KP1 - 1
                  GO TO 335
                  END IF
               END IF
            IP1 = IP2 - KP1 + 2
            NP = 0
            DO 340 IP = KP1,IPMAX
               NP = NP + 1
               CALL RCOPY (NZERO, SUM(1,1,1,1,IP), SUM(1,1,1,1,NP))
               CALL RCOPY (NZERO, SUMWT(1,1,1,1,IP), SUMWT(1,1,1,1,NP))
               CALL RCOPY (2, TIMR(1,IP), TIMR(1,NP))
 340           CONTINUE
            IP = NP
         ELSE
            IP = 0
            IP1 = 1
            END IF
         END IF
C                                       advance for next buffer
      IF (.NOT.DONE) THEN
         IP = IP + 1
         CALL RFILL (NZERO, 0.0, SUM(1,1,1,1,IP))
         CALL RFILL (NZERO, 0.0, SUMWT(1,1,1,1,IP))
         CALL FILL (NZERO, 0, COUNT)
         THISOU = CURSOU
         BEGTIM = CURTIM
         ENDTIM = CURTIM + TCLIP
         THIFRQ = CURFRQ
         THISUB = CURSUB
         LSTIME = CURTIM
         GO TO 110
         END IF
C-----------------------------------------------------------------------
C                                       close FG table
      CALL TABCLO (FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       history
      WRITE (HILINE,1700) MAXVD(1), MAXVD(2)
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1701) MAXVD(3), MAXVD(4)
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1711) NFLAG1
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1712) NFLAG2
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (HILINE,1713) NFLAG3
      CALL OHWRIT (HILINE, FGTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close file
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete temporary table
      IF (DOSOU) THEN
         CALL DESTRY (SUTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       report
      NBAD = NFLAG1 + NFLAG2 + NFLAG3
      WRITE (MSGTXT,1800) NPRINT, NBAD
      CALL MSGWRT (5)
      IF (NBAD.GT.0) THEN
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1801) NFLAG1
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1802) NFLAG2
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         WRITE (MSGTXT,1803) NFLAG3
         CALL MSGWRT (5)
         IF (DOCRT.NE.0) THEN
            CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) DOCRT = 0
            END IF
         IERR = 0
         END IF
C                                       Close printer
      IF (PRINT.NE.' ') THEN
         CALL OCLOSE (PRINT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'AMVRFI: ERROR IN RFI DETECTION FOR ' // UVIN
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('For ''',A,''' data: VDIF clipping coef.=',F8.4,F7.4)
 1001 FORMAT ('For ''',A,''' data: amp clipping range =',F8.4,F9.4)
 1002 FORMAT ('For ''',A,''' data: wt clipping range  =',2(1PE10.3))
 1005 FORMAT ('Max. fract. questionable corrs.=',F6.3,'  antennas=',
     *   F6.3)
 1230 FORMAT (I3,'/',2(I2.2,':'),I2.2,' -',I3,'/',2(I2.2,':'),I2.2,
     *   3I3,I2,' ??? ',3(1PE11.3))
 1270 FORMAT (I3,'/',2(I2.2,':'),I2.2,' -',I3,'/',2(I2.2,':'),I2.2,1X,
     *   A,':',I5.5,1X,2F9.5,'(',I2.2,')')
 1290 FORMAT (I3,'/',2(I2.2,':'),I2.2,' -',I3,'/',2(I2.2,':'),I2.2,
     *   3I3,I2,' bad ',3(1PE11.3))
 1700 FORMAT ('MAXVDIF(1)=',F8.4,',',F7.4,' / Vdif Jy,gain for',
     *   ' questionable')
 1701 FORMAT ('MAXVDIF(2)=',F7.4,',',F6.4,' / Vdif Jy,gain for',
     *   ' bad antenna')
 1711 FORMAT ('FLAGD(1) =',I8,' / integrations fully flagged')
 1712 FORMAT ('FLAGD(2) =',I8,' / corr/integrations fully flagged')
 1713 FORMAT ('FLAGD(3) =',I8,' / antennas/integrations flagged')
 1800 FORMAT ('Found',I6,' bad times,',I6,' total flags')
 1801 FORMAT ('Wrote',I6,' antenna & IF independent flags')
 1802 FORMAT ('Wrote',I6,' antenna independent, IF dependent flags')
 1803 FORMAT ('Wrote',I6,' antenna & IF dependent flags')
      END
      SUBROUTINE OXXINI (TABLE, OPCODE, XXROW, IERR)
C-----------------------------------------------------------------------
C   Initialize a weights scratch table (type XX) for reading or writing,
C   creating it if necessary.
C
C   Inputs:
C      TABLE  C*(*)     Name of table object:  NAME, CLASS, IMSEQ, DISK
C                       and VER should be set.
C      OPCODE C*4       Operation code
C                         'READ' - open for reading
C                         'WRIT' - open for writing (create if
C                                  necessary)
C
C   Outputs:
C      XXROW  I         Next row to read or write
C      IERR   I         Return code: 0 => successful
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   XXROW, IERR
C
      INCLUDE 'XXTAB.INC'
      CHARACTER LABEL*56, COLLAB(NCOL,2)*24, COLUNT(NCOL)*8
      INTEGER   COLTYP(NCOL), COLDIM(NCOL), LCOL
      INTEGER   TYPE, DIM(3), IDUMMY
      CHARACTER DUMMY
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LABEL / 'AMP/RMS/WT SCRATCH TABLE'/
      DATA COLLAB / 'TIME1', 'TIME2', 'SOURCE', 'AMPLITUDE', 'RMS',
     *   'WEIGHT', 'CLOSURE', ' ',
     *   'TIME1', 'TIME2', 'SOURCE', 'REAL', 'IMAG', 'WEIGHT',
     *   'CLOSURE', 'REF ANT' /
      DATA COLUNT / 'DAYS', 'DAYS', ' ', 2*'Jy', '1/Jy**2', 2*' ' /
      DATA COLTYP / OOARE, OOARE, OOAINT, OOARE, OOARE, OOARE, OOARE,
     *   OOAINT /
      DATA COLDIM / 1, 1, 1, 1, 1, 1, 1, 1 /
C-----------------------------------------------------------------------
      LCOL = NCOL - 2 + XXTYPE
C                                       Fill in table type:
      TYPE = OOACAR
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'TBLTYPE', TYPE, DIM, IDUMMY, 'XX', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Initialize table data if
C                                       writing:
      IF (OPCODE.EQ.'WRIT') THEN
         TYPE = OOACAR
         DIM(1) = 56
         DIM(2) = 1
         DIM(3) = 0
         CALL TABPUT (TABLE, 'LABEL', TYPE, DIM, IDUMMY, LABEL, IERR)
         IF (IERR.NE.0) GO TO 999
         TYPE = OOAINT
         DIM(1) = 1
         CALL TABPUT (TABLE, 'NCOL', TYPE, DIM, LCOL, DUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABPUT (TABLE, 'VER', TYPE, DIM, 0, DUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         TYPE = OOACAR
         DIM(1) = 24
         DIM(2) = LCOL
         IF (XXTYPE.NE.2) XXTYPE = 1
         CALL TABPUT (TABLE, 'COLABEL', TYPE, DIM, IDUMMY,
     *      COLLAB(1,XXTYPE), IERR)
         IF (IERR.NE.0) GO TO 999
         DIM(1) = 8
         CALL TABPUT (TABLE, 'COLUNIT', TYPE, DIM, IDUMMY, COLUNT, IERR)
         IF (IERR.NE.0) GO TO 999
         TYPE = OOAINT
         DIM(1) = LCOL
         DIM(2) = 1
         CALL TABPUT (TABLE, 'COLTYPE', TYPE, DIM, COLTYP, DUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         COLDIM(4) = NSAMP
         COLDIM(5) = NSAMP
         COLDIM(6) = NSAMP
         COLDIM(7) = NSAMP
         COLDIM(8) = NCORS
         CALL TABPUT (TABLE, 'COLDIM', TYPE, DIM, COLDIM, DUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Initialize table:
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'CANNOT OPEN WEIGHTS SCRATCH (XX) TABLE'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Get column indices
      CALL TABCOL (TABLE, LCOL, COLLAB(1,XXTYPE), COLIDX, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get next row:
      IF (OPCODE.EQ.'READ') THEN
         XXROW = 1
      ELSE
         TYPE = OOAINT
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL TABGET (TABLE, 'NROW', TYPE, DIM, XXROW, DUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         XXROW = XXROW + 1
         END IF
C
 999  RETURN
      END
      SUBROUTINE OTABXX (TABLE, OPCODE, XXROW, TB, TE, SU, AMPS, RMSS,
     *   WTS, CLOS, REFS, IERR)
C-----------------------------------------------------------------------
C   Read or write a record to a amp/rms scratch table.
C   Inputs:
C      TABLE   C*(*)   Name of table object
C      OPCODE  C*4     Operation code: 'READ' or 'WRIT'
C   Input/output:
C      XXROW   I       Row number to read or write (incremented on exit)
C      TB      R       Start time
C      TE      R       End time
C      SU      I       source number
C      AMPS    R(*)    Amplitude values (Nant, Nif, Npol)
C      RMSS    R(*)    RMS values (Nant, Nif, Npol)
C      WTS     R(*)    Weight values (Nant, Nif, Npol)
C      CLOS    R(*)    Close excess fraction
C      REFS    I(*)    Reference antennas (Nif, Npol)
C   Output:
C      IERR    I       Return code: 0 => successful
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   XXROW, SU, REFS(*), IERR
      REAL      TB, TE, AMPS(*), RMSS(*), WTS(*), CLOS(*)
C
      INTEGER   TYPE, DIM(3), DUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'XXTAB.INC'
C-----------------------------------------------------------------------
      IF (OPCODE.EQ.'WRIT') THEN
         TYPE = OOAINT
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL TABDPT (TABLE, XXROW, COLIDX(3), TYPE, DIM, SU, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         TYPE = OOARE
         CALL TABDPT (TABLE, XXROW, COLIDX(1), TYPE, DIM, TB, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDPT (TABLE, XXROW, COLIDX(2), TYPE, DIM, TE, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         DIM(1) = NSAMP
         CALL TABDPT (TABLE, XXROW, COLIDX(4), TYPE, DIM, AMPS, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDPT (TABLE, XXROW, COLIDX(5), TYPE, DIM, RMSS, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDPT (TABLE, XXROW, COLIDX(6), TYPE, DIM, WTS, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDPT (TABLE, XXROW, COLIDX(7), TYPE, DIM, CLOS, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         IF (XXTYPE.EQ.2) THEN
            DIM(1) = NCORS
            TYPE = OOAINT
            CALL TABDPT (TABLE, XXROW, COLIDX(8), TYPE, DIM, REFS,
     *         DUMMY, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
      ELSE
         CALL TABDGT (TABLE, XXROW, COLIDX(1), TYPE, DIM, TB, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (TABLE, XXROW, COLIDX(2), TYPE, DIM, TE, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (TABLE, XXROW, COLIDX(3), TYPE, DIM, SU, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (TABLE, XXROW, COLIDX(4), TYPE, DIM, AMPS, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (TABLE, XXROW, COLIDX(5), TYPE, DIM, RMSS, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (TABLE, XXROW, COLIDX(6), TYPE, DIM, WTS, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (TABLE, XXROW, COLIDX(7), TYPE, DIM, CLOS, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         IF (XXTYPE.EQ.2) THEN
            CALL TABDGT (TABLE, XXROW, COLIDX(8), TYPE, DIM, REFS,
     *         DUMMY, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
C
      XXROW = XXROW + 1
C
 999  RETURN
      END
      SUBROUTINE REREF (CANT, CIF, CPOL, MTIME, LIF, LP, LS, REFANT,
     *   ANT, SNUMS, TIMR, CREAL, CIMAG, REFAS)
C-----------------------------------------------------------------------
C   Re-references from ANT to REFANT
C   Inputs:
C      CANT      I     Maximum antenna number
C      CIF       I     Number of IFs
C      CPOL      I     Number of polarizations
C      MTIME     I     Number of times
C      LIF       I     Current IF
C      LP        I     Current polarization
C      LS        I     Current source
C      REFANT    I     Desired reference antenna
C      ANT       I     Reference antenna to be undone
C      SNUMS     I(*)  Source numbers vs time
C      TIMR      R(*)  Time range (start/stop, time)
C   In/out:
C      CREAL     R(*)  Real (ant,if,pol,time)
C      CIMAG     R(*)  Imag (ant,if,pol,time)
C      REFAS     I(*)  Refants (if,pol,time)
C-----------------------------------------------------------------------
      INTEGER   CANT, CIF, CPOL, MTIME, LIF, LP, LS, REFANT, ANT,
     *   SNUMS(*), REFAS(CIF,CPOL,*)
      REAL      TIMR(2,*), CREAL(CANT,CIF,CPOL,*),
     *   CIMAG(CANT,CIF,CPOL,*)
C
      INTEGER   IP, NP, LIST(1001), IP1, IP2, KP1, KP2, LA
      REAL      RE1, RE2, IM1, IM2, TI1, TI2, W1, W2, TI, RE, IM, AMP,
     *   TRE, TIM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       make a list of matching times
      NP = 0
      DO 10 IP = 1,MTIME
         IF (SNUMS(IP).EQ.LS) THEN
            IF ((REFAS(LIF,LP,IP).EQ.REFANT) .AND.
     *         (CREAL(ANT,LIF,LP,IP).NE.FBLANK) .AND.
     *         ((CREAL(ANT,LIF,LP,IP).NE.0.0) .OR.
     *         (CIMAG(ANT,LIF,LP,IP).NE.0.0))) THEN
               NP = NP + 1
               LIST(NP) = IP
            ELSE IF ((REFAS(LIF,LP,IP).EQ.ANT) .AND.
     *         (CREAL(REFANT,LIF,LP,IP).NE.FBLANK) .AND.
     *         ((CREAL(REFANT,LIF,LP,IP).NE.0.0) .OR.
     *         (CIMAG(REFANT,LIF,LP,IP).NE.0.0))) THEN
               NP = NP + 1
               LIST(NP) = IP
               END IF
            END IF
 10      CONTINUE
C                                       no connection
      IF (NP.LE.0) THEN
         WRITE (MSGTXT,1010) ANT, REFANT, LIF, LP
         CALL MSGWRT (6)
C                                       re-reference
      ELSE
C                                       initial interp values
         IP1 = 1
         IP2 = MIN (2, NP)
         KP1 = LIST(IP1)
         KP2 = LIST(IP2)
         IF (REFAS(LIF,LP,KP1).EQ.REFANT) THEN
            RE1 = CREAL(REFANT,LIF,LP,KP1)
            IM1 = CIMAG(REFANT,LIF,LP,KP1)
         ELSE
            RE1 = CREAL(ANT,LIF,LP,KP1)
            IM1 = -CIMAG(ANT,LIF,LP,KP1)
            END IF
         IF (REFAS(LIF,LP,KP2).EQ.REFANT) THEN
            RE2 = CREAL(REFANT,LIF,LP,KP2)
            IM2 = CIMAG(REFANT,LIF,LP,KP2)
         ELSE
            RE2 = CREAL(ANT,LIF,LP,KP2)
            IM2 = -CIMAG(ANT,LIF,LP,KP2)
            END IF
         TI1 = (TIMR(1,KP1) + TIMR(2,KP1)) / 2.0
         TI2 = (TIMR(1,KP2) + TIMR(2,KP2)) / 2.0
C                                       loop through data
         DO 50 IP = 1,MTIME
            IF (SNUMS(IP).EQ.LS) THEN
               IF (REFAS(LIF,LP,IP).EQ.ANT) THEN
                  TI = (TIMR(1,IP) + TIMR(2,IP)) / 2.0
 20               IF ((TI.GE.TI1) .AND. (TI.LE.TI2)) THEN
                     IF (TI2.GT.TI1) THEN
                        W1 = (TI2 - TI) / (TI2 - TI1)
                     ELSE
                        W1 = 1.0
                        END IF
                  ELSE IF (TI.LT.TI1) THEN
                     W1 = 1.0
                  ELSE IF (IP2.GE.NP) THEN
                     W1 = 0.0
                  ELSE
                     RE1 = RE2
                     IM1 = IM2
                     TI1 = TI2
                     KP1 = KP2
                     IP1 = IP1 + 1
                     IP2 = MIN (IP2+1, NP)
                     KP2 = LIST(IP2)
                     IF (REFAS(LIF,LP,KP2).EQ.REFANT) THEN
                        RE2 = CREAL(REFANT,LIF,LP,KP2)
                        IM2 = CIMAG(REFANT,LIF,LP,KP2)
                     ELSE
                        RE2 = CREAL(ANT,LIF,LP,KP2)
                        IM2 = -CIMAG(ANT,LIF,LP,KP2)
                        END IF
                     TI2 = (TIMR(1,KP2) + TIMR(2,KP2)) / 2.0
                     GO TO 20
                     END IF
C                                       interpolate
                  W2 = 1.0 - W1
                  RE = W1 * RE1 + W2 * RE2
                  IM = W1 * IM1 + W2 * RE2
                  AMP = SQRT (RE * RE + IM * IM)
                  RE = RE / AMP
                  IM = IM / AMP
                  DO 30 LA = 1,CANT
                     IF ((CREAL(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                  (CIMAG(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                  ((CREAL(LA,LIF,LP,IP).NE.0.0) .OR.
     *                  (CIMAG(LA,LIF,LP,IP).NE.0.0))) THEN
                        TRE = CREAL(LA,LIF,LP,IP)
                        TIM = CIMAG(LA,LIF,LP,IP)
                        CREAL(LA,LIF,LP,IP) = TRE * RE - TIM * IM
                        CIMAG(LA,LIF,LP,IP) = TRE * IM + TIM * RE
                        END IF
 30                  CONTINUE
                  REFAS(LIF,LP,IP) = REFANT
                  END IF
               END IF
 50         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CANNOT REFERENCE ANT',I3,' TO',I3,' AT IF/POL',I3,I2)
      END
      REAL FUNCTION MEDIUN (A, B, LEN)
C-----------------------------------------------------------------------
C   The median of the first LEN numbers in A.  LEN should be an odd
C   number.
C
C   Input:
C      LEN       I         Length of array
C      A         R(*)      Data array
C      B         R(*)      Second array
C                             ignore points that have A(I)=B(I)=0.0
C   Usage example for remembrance
C
C      MEDRE = MEDIUN (RE, FILWID)
C                                       rms
C      DO 60 I = 1, FILWID
C         ADIFF(I) = ABS(RE(I) - MEDRE)
C 60      CONTINUE
C      SIGMA = ROOT2 * 1.5 * MEDIUN(ADIFF, FILWID)
C
C-----------------------------------------------------------------------
      REAL      A(*), B(*)
      INTEGER   LEN
C
      INTEGER   I, L
      REAL      X(50001), MEDIAN
C-----------------------------------------------------------------------
C                                       select
      L = 0
      DO 10 I = 1,LEN
         IF ((A(I).NE.0.0) .OR. (B(I).NE.0.0)) THEN
            L = L + 1
            X(L) = A(I)
            END IF
 10      CONTINUE
C                                       sort
      IF (L.GT.0) THEN
         MEDIUN = MEDIAN (L, X)
      ELSE
         MEDIUN = 0.0
         END IF
C
 999  RETURN
      END
