LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NPARMS
      PARAMETER (NPARMS=31)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'SOURCES',
     *   'QUAL', 'CALCODE', 'TIMERANG', 'SELBAND', 'SELFREQ', 'FREQID',
     *   'BIF', 'EIF', 'BCHAN', 'ECHAN', 'SUBARRAY', 'DOCALIB',
     *   'GAINUSE', 'DOPOL', 'PDVER', 'BLVER', 'FLAGVER', 'DOBAND',
     *   'BPVER', 'SMOOTH', 'UVRANGE', 'SOLINT', 'BPARM', 'DOCRT',
     *   'OUTPRINT', 'BADDISK'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR,
     *   OOAINT, OOACAR, OOARE, OOARE, OOARE, OOAINT,
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOAINT, OOARE,
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOAINT, OOAINT,
     *   OOAINT, OOARE, OOARE, OOARE, OOARE, OOAINT,
     *   OOACAR, OOAINT/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 16,30,
     *   1,1, 4,1, 8,1, 1,1, 1,1, 1,1,
     *   1,1, 1,1, 1,1, 1,1, 1,1, 1,1,
     *   1,1, 1,1, 1,1, 1,1, 1,1, 1,1,
     *   1,1, 3,1, 2,1, 1,1, 10,1, 1,1,
     *   48,1, 10,1/
LOCAL END
      PROGRAM RFI
C-----------------------------------------------------------------------
C! Interference detector
C# Task OOP calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2003-2004, 2006-2007, 2010, 2015
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
C   Interference detector
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UVMULT*32, PRINT*32, INPUT*32
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'RFI '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL RFIIN (PRGM, INPUT, UVMULT, PRINT, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Check interference
      CALL RFIO (UVMULT, PRINT, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE RFIIN (PRGN, INPUT, UVMULT, PRINT, IERR)
C-----------------------------------------------------------------------
C   RFIIN gets input parameters for RFI and creates the input uvdata
C   object.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      INPUT   C*?  Task inputs object
C      UVMULT  C*?  Input multisource uv data object.
C      PRINT   C*?  Print object
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER PRGN*6, INPUT*32, UVMULT*32, PRINT*32
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INTEGER   NKEY, NKEY2
C                                       NKEY=no. adverbs to copy to
C                                       UVMULT
      PARAMETER (NKEY=25)
C                                       NKEY2=no. adverbs for PRINT
      PARAMETER (NKEY2=2)
      INTEGER   DIM(7), TYPE, FGVER, DUMMY
      REAL      BPARM(10), SOLINT, XDOCAL
      LOGICAL   LTEMP
      CHARACTER INK(NKEY)*8, OUTK(NKEY)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, FGTAB*32, STOKES*4, CDUMMY*1
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for UVMULT
      DATA INK /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'GAINUSE', 'DOPOL', 'BLVER', 'DOBAND', 'BPVER',
     *   'FLAGVER',  'TIMERANG', 'UVRANGE', 'SOURCES', 'QUAL',
     *   'SELBAND', 'SELFREQ', 'FREQID', 'CALCODE', 'SUBARRAY',
     *   'BIF', 'EIF', 'BCHAN', 'ECHAN', 'SMOOTH', 'PDVER'/
      DATA OUTK /'NAME', 'CLASS', 'IMSEQ', 'DISK',
     *   'CALEDIT.CLUSE', 'CALEDIT.DOPOL', 'CALEDIT.BLVER',
     *   'CALEDIT.FGVER', 'CALEDIT.DOBAND', 'CALEDIT.BPVER',
     *   'CALEDIT.TIMRNG', 'CALEDIT.UVRNG', 'CALEDIT.SOURCS',
     *   'CALEDIT.SELQUA', 'CALEDIT.SELBAN', 'CALEDIT.SELFRQ',
     *   'CALEDIT.FRQSEL', 'CALEDIT.SELCOD', 'CALEDIT.SUBARR',
     *   'CALEDIT.BIF', 'CALEDIT.EIF', 'CALEDIT.BCHAN', 'CALEDIT.ECHAN',
     *   'CALEDIT.SMOOTH', 'CALEDIT.PDVER'/
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
C                                       Create UVMULT
      UVMULT = 'Input UVdata'
      CALL CREATE (UVMULT, 'UVDATA', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ (INPUT, NKEY, INK, OUTK, UVMULT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOCALIB
      CALL OGET ('Input', 'DOCALIB', TYPE, DIM, XDOCAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      DIM(2) = 1
      LTEMP = XDOCAL.GT.0.0
      CALL OPUT (UVMULT, 'CALEDIT.DOCAL', OOALOG, DIM, LTEMP, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      LTEMP = (XDOCAL.GT.0.0) .AND. (XDOCAL.LE.99.0)
      CALL OPUT (UVMULT, 'CALEDIT.DOWTCL', OOALOG, DIM, LTEMP, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Select all Stokes'
      STOKES = '    '
      DIM(1) = LEN (STOKES)
      DIM(2) = 1
      CALL OPUT (UVMULT, 'CALEDIT.STOKES', OOACAR, DIM, DUMMY, STOKES,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open UVMULT to be sure it's OK.
      CALL OOPEN (UVMULT, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OCLOSE (UVMULT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Check defaults
C                                       Digest BPARM
      CALL OGET (INPUT, 'BPARM', TYPE, DIM, BPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       MAXBAD (0.25)
      IF (ABS (BPARM(1)).LE.1.0E-20) BPARM(1) = 0.25
C                                       Save defaults in BPARM
      CALL OPUT (INPUT, 'BPARM', OOARE, DIM, BPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Save on UVMULT
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVMULT, 'MAXBAD', OOARE, DIM, BPARM(1), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 2
      CALL OPUT (UVMULT, 'MAXRMS', OOARE, DIM, BPARM(2), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default FLAGVER
      CALL OGET (INPUT, 'FLAGVER', TYPE, DIM, FGVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find highest FG table.
      IF (FGVER.LE.0.0) THEN
         FGTAB = 'Temporary FG table for RFIIN'
         FGVER = 1
         CALL UV2TAB (UVMULT, FGTAB, 'FG', FGVER, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TBLHIV (FGTAB, FGVER, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDES (FGTAB, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      CALL OPUT (INPUT, 'FLAGVER', OOAINT, DIM, FGVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVMULT, 'CALEDIT.FGVER', OOAINT, DIM, FGVER, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default SOLINT (60 sec)
      CALL OGET (INPUT, 'SOLINT', TYPE, DIM, SOLINT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (SOLINT.LE.0.0) SOLINT = 60.0
      CALL OPUT (INPUT, 'SOLINT', OOARE, DIM, SOLINT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVMULT, 'TIMEAVG', OOARE, DIM, SOLINT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Printer object
      PRINT = 'Printer object'
      CALL PRTCRE (PRINT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ (INPUT, NKEY2, INK2, OUTK2, PRINT, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = LEN (UVMULT)
      DIM(2) = 1
      CALL PRTPUT (PRINT, 'PRINTOBJ', OOACAR, DIM, DUMMY, UVMULT, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE RFIO (UVMULT, PRINT, IERR)
C-----------------------------------------------------------------------
C   Process data.
C   Inputs:
C      UVMULT  C*?  Input multisource uv object
C      PRINT   C*?  Print object
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVMULT*(*), PRINT*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Interference detection.
      CALL RFIRMS (UVMULT, PRINT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR FINDING RFI IN ' //UVMULT
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE RFIRMS (UVIN, PRINT, IERR)
C-----------------------------------------------------------------------
C   Look for interference based on a high real or imaginary rms in time
C   interval TIMEAVG.
C      Currently this routine is configured to do up to 28 antennas and
C   8 correlations, i.e. standard VLA continuum data.
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C      PRINT   C*?  Print object
C   Inputs attached to UVIN:
C      MAXRMS  R(2) Maximum RMS allowed, constant plus amplitude
C                   coefficient.
C      MAXBAD  R    Maximum allowed fraction of bad baselines.
C      TIMEAVG R    Time in seconds for clipping interval
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), PRINT*(*)
      INTEGER   IERR
C
      REAL      MAXRMS(2), MAXBAD, TCLIP
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXXA, MAXXBL, MAXXCR
C                                       Number (max) of Antennas.
      PARAMETER (MAXXA = MAXANT)
      PARAMETER (MAXXBL = MXBASE)
      PARAMETER (MAXXCR = 8)
      INTEGER   COUNT, TYPE, DIM(7), I, J, NCORR, INDXT, INDXB, INDXSU,
     *   ANT1, ANT2, BLINDX, BLOFF(MAXXA), CNTBAS, NWORD, OKCNT, DUMMY,
     *   ENDVIS, VISNO, BCORNO, CURSOU, THISOU, TBVER, DOCRT, NACROS,
     *   QUAL, BEGT(4), ENDT(4), MSGSAV, INDXA1, INDXA2
      LOGICAL   BADCOR(MAXXCR),  DONE, DOSOU, QUIT, DOCAL
      CHARACTER SUTAB*32, SUNAME*16, LINE*132, CDUMMY*1, UVTYPE*2
      REAL      ACC(6,MAXXCR,MAXXBL), AMP2, RMS1, RMS2, RP(50),
     *   VS(3,MAXXCR), MXRMS2, MXAMP2, CORCNT(MAXXCR), CORBAD(MAXXCR),
     *   CURTIM, LSTIME, WORST, CORSUM(MAXXCR), ENDTIM, BEGTIM
      LOGICAL   FLGBAS, ISBAD, BADRFI
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open printer
      CALL OOPEN (PRINT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (PRINT, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (PRINT, 'NACROS', TYPE, DIM, NACROS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open input.
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get parameters
      CALL OUVGET (UVIN, 'MAXBAD', TYPE, DIM, MAXBAD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'MAXRMS', TYPE, DIM, MAXRMS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      MXRMS2 = MAXRMS(1) * MAXRMS(1)
      MXAMP2 = MAXRMS(2) * MAXRMS(2)
      CALL OUVGET (UVIN, 'TIMEAVG', TYPE, DIM, TCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      TCLIP = TCLIP / 86400.0
C                                       Processing info
      CALL OUVGET (UVIN, 'CALEDIT.DOCAL', TYPE, DIM, DOCAL, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DOCAL) THEN
         LINE = 'Calibration applied'
      ELSE
         LINE = 'NO Calibration applied'
         END IF
      CALL PRTWRI (PRINT, LINE, QUIT, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (LINE,1001) MAXBAD, MAXRMS
      CALL PRTWRI (PRINT, LINE, QUIT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Page labels
      LINE = 'Data affected by interference'
      DIM(1) = LEN (LINE)
      DIM(2) = 1
      CALL OPUT (PRINT, 'TITLE1', OOACAR, DIM, DUMMY, LINE, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL PRTWRI (PRINT, LINE, QUIT, IERR)
      IF (QUIT) GO TO 700
      IF (IERR.NE.0) GO TO 990
      LINE = 'Source           Qual         Time range        ' //
     *   ' Corr flags   Maxrms(c)'
      CALL OPUT (PRINT, 'TITLE2', OOACAR, DIM, DUMMY, LINE, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL PRTWRI (PRINT, LINE, QUIT, IERR)
      IF (QUIT) GO TO 700
      IF (IERR.NE.0) GO TO 990
C                                       Need baseline RP
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (UVTYPE(:1).EQ.'S') THEN
         CALL UVDFND (UVIN, 1, 'BEAM', INDXB, IERR)
      ELSE
         CALL UVDFND (UVIN, 1, 'BASELINE', INDXB, IERR)
         IF (IERR.NE.0) THEN
            INDXB = -1
            CALL UVDFND (UVIN, 1, 'ANTENNA1', INDXA1, IERR)
            IF (IERR.EQ.0) CALL UVDFND (UVIN, 1, 'ANTENNA2', INDXA2,
     *         IERR)
            END IF
         END IF
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER BEAM/BASELINE'
         GO TO 990
         END IF
C                                       Need TIME RP
      CALL UVDFND (UVIN, 1, 'TIME1', INDXT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER TIME'
         GO TO 990
         END IF
C                                       Need SOURCE RP
C                                       May not be there
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL UVDFND (UVIN, 1, 'SOURCE', INDXSU, IERR)
      MSGSUP = MSGSAV
      DOSOU = (IERR.EQ.0) .AND. (INDXSU.GT.0)
      IERR = 0
C                                       Source table/name
      IF (DOSOU) THEN
         SUTAB = 'SoUrce table for RFIRMS'
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
C                                       Number of correlators
      CALL UVDGET (UVIN, 'NCORR', TYPE, DIM, NCORR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       Make sure it fits
      IF (NCORR.GT.MAXXCR) THEN
         WRITE (MSGTXT,1000) NCORR, MAXXCR
         IERR = 2
         GO TO 985
         END IF
C                                       Clear accumulators:
C                                       (1,*) =  count
C                                       (2,*) =  sum r then max rms**2
C                                       (3,*) =  sum**2 r then rms**2
C                                       (4,*) =  sum imaginary
C                                       (5,*) =  sum**2 imaginary
C                                       (6,*) =  sum amplitude
      NWORD = 6 * MAXXCR * MAXXBL
      CALL RFILL (NWORD, 0.0, ACC)
      CALL RFILL (NCORR, 0.0, CORCNT)
      CALL RFILL (NCORR, 0.0, CORBAD)
      CALL RFILL (NCORR, 0.0, CORSUM)
      DO 10 I = 1,NCORR
         BADCOR(I) = .FALSE.
 10      CONTINUE
C                                       Fill baseline index array
      COUNT = 1
      DO 50 I = 1,MAXXA
         BLOFF(I) = COUNT
         COUNT = COUNT + MAXXA - I + 1
 50      CONTINUE
C                                       Initialize visibility count
      COUNT = 0
      OKCNT = 0
      CNTBAS = 0
      DONE = .FALSE.
      VISNO = 0
      WORST = 0.0
C                                       Swallow input collecting
C                                       statistics.
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VS, IERR)
         IF (IERR.LT.0) THEN
            DONE = .TRUE.
            IERR = 0
            END IF
         IF (IERR.GT.0) GO TO 990
         VISNO = VISNO + 1
 110     CURTIM = RP(INDXT)
         IF (DOSOU) THEN
            CURSOU = RP(INDXSU) + 0.5
         ELSE
            CURSOU = 0
            END IF
C                                       Initial selection.
         IF (VISNO.EQ.1) THEN
            THISOU = CURSOU
            BEGTIM = CURTIM
            ENDTIM = CURTIM + TCLIP
            END IF
C                                       Finished with data or interval?
         IF (DONE .OR. (CURTIM.GT.ENDTIM) .OR. (CURSOU.NE.THISOU))
     *      GO TO 150
C                                       Set last vis of interval
         ENDVIS = VISNO
C                                       Set last time
         LSTIME = CURTIM
C                                       Get antenna numbers
         IF (INDXB.GT.0) THEN
            ANT1 = (RP(INDXB) / 256.0) + 0.001
            ANT2 = (RP(INDXB) - ANT1 * 256) + 0.001
         ELSE
            ANT1 = RP(INDXA1) + 0.1
            ANT2 = RP(INDXA2) + 0.1
            END IF
C                                       Baseline index
         BLINDX = BLOFF(ANT1) + ANT2 - ANT1
         DO 120 I = 1,NCORR
            IF (VS(3,I).GT.0) THEN
               ACC(1,I,BLINDX) = ACC(1,I,BLINDX) + 1.0
               ACC(2,I,BLINDX) = ACC(2,I,BLINDX) + ABS (VS(1,I))
               ACC(3,I,BLINDX) = ACC(3,I,BLINDX) + VS(1,I) * VS(1,I)
               IF (ANT1.NE.ANT2) THEN
                  ACC(4,I,BLINDX) = ACC(4,I,BLINDX) + ABS (VS(2,I))
                  ACC(5,I,BLINDX) = ACC(5,I,BLINDX) + VS(2,I) * VS(2,I)
                  ACC(6,I,BLINDX) = ACC(6,I,BLINDX) + SQRT
     *               (VS(1,I)*VS(1,I) + VS(2,I) * VS(2,I))
               ELSE
                  ACC(6,I,BLINDX) = ACC(6,I,BLINDX) + ABS (VS(1,I))
                  END IF
               END IF
 120        CONTINUE
C                                       Next vis until done
         IF (.NOT.DONE) GO TO 100
 150     IERR = 0
C                                       Get statistics
      DO 200 I = 1,MAXXBL
         FLGBAS = .FALSE.
         DO 190 J = 1,NCORR
            IF (ACC(1,J,I).GT.1.1) THEN
C                                       Real part
               RMS1 = (ACC(3,J,I) - ((ACC(2,J,I)**2) / ACC(1,J,I))) /
     *            (ACC(1,J,I) - 1.0)
               ACC(3,J,I) = RMS1
C                                       Convert sum to maximum rms**2
               AMP2 = (ACC(6,J,I) / ACC(1,J,I)) ** 2
               ACC(2,J,I) = MXRMS2 + (MXAMP2*AMP2)
C                                       Is this one bad?
               ISBAD = RMS1.GT.ACC(2,J,I)
C                                       Imaginary part
               RMS2 = (ACC(5,J,I) - ((ACC(4,J,I)**2) / ACC(1,J,I))) /
     *            (ACC(1,J,I) - 1.0)
C                                       Is this one bad?
               ISBAD = ISBAD .OR. (RMS2.GT.ACC(2,J,I))
               FLGBAS = FLGBAS .OR. ISBAD
C                                       Correlator info
               CORCNT(J) = CORCNT(J) + 1.0
               IF (ISBAD) THEN
                  CORBAD(J)= CORBAD(J) + 1.0
                  CORSUM(J)= CORSUM(J) + RMS2 + RMS1
                  END IF
               END IF
 190        CONTINUE
         IF (FLGBAS) CNTBAS = CNTBAS + 1
 200     CONTINUE
C                                       Check for bad correlators
      BADRFI = .FALSE.
      DO 210 I = 1,NCORR
         IF (CORCNT(I).GT.1.1) THEN
C                                       Kill correlator
            IF ((CORBAD(I)/CORCNT(I)).GT.MAXBAD) THEN
               BADCOR(I) = .TRUE.
               BADRFI = .TRUE.
               END IF
            IF (BADCOR(I)) THEN
               RMS2 = CORSUM(I) / CORBAD(I)
               IF (WORST.LT.RMS2) THEN
                  WORST = RMS2
                  BCORNO = I
                  END IF
               END IF
            END IF
 210     CONTINUE
C                                       If bad tell about it
      IF (BADRFI) THEN
C                                       Get source name, qualifier
         IF (DOSOU) THEN
            CALL SOUNFO (SUTAB, THISOU, 'SOURCE', TYPE, DIM, DUMMY,
     *         SUNAME, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL SOUNFO (SUTAB, THISOU, 'QUAL', TYPE, DIM, QUAL,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         CALL TODHMS (BEGTIM, BEGT)
         CALL TODHMS (LSTIME, ENDT)
         IF (WORST.GT.1.0E-10) WORST = SQRT (WORST)
         WRITE (LINE,1210) SUNAME, QUAL, BEGT, ENDT,
     *      (BADCOR(I), I = 1,8), WORST, BCORNO
         IF (NCORR.LT.8) LINE(51+NCORR:58) = ' '
         CALL PRTWRI (PRINT, LINE, QUIT, IERR)
         IF (QUIT) GO TO 700
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Clear accumulators:
      NWORD = 6 * MAXXCR * MAXXBL
      CALL RFILL (NWORD, 0.0, ACC)
      CALL RFILL (NCORR, 0.0, CORCNT)
      CALL RFILL (NCORR, 0.0, CORBAD)
      CALL RFILL (NCORR, 0.0, CORSUM)
      DO 220 I = 1,NCORR
         BADCOR(I) = .FALSE.
 220     CONTINUE
C                                       Another clipping interval?
      IF (.NOT.DONE) THEN
         THISOU = CURSOU
         BEGTIM = CURTIM
         ENDTIM = CURTIM + TCLIP
         WORST = 0.0
         GO TO 110
         END IF
C                                       Close file
 700  CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close printer
      CALL OCLOSE (PRINT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete temporary table
      CALL DESTRY (SUTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 985  CALL MSGWRT (7)
 990  MSGTXT = 'RFIRMS: ERROR IN RFI DETECTION FOR ' // UVIN
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RFIRMS: ERROR, NO. CORRELATORS = ',I4, ' > ', I4)
 1001 FORMAT ('Max. fract. bad baselines= ',F5.3,', Clipping coef.=',
     *   2F8.5)
 1210 FORMAT (A,':',I5.5,I3,'/',2(I2.2,':'),I2.2,' -',I3,'/',
     *   2(I2.2,':'),I2.2,2X,8L1,F10.5,'(',I1,')')
      END
