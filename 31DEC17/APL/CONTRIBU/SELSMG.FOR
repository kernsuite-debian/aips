      SUBROUTINE SELSMG (DISK, CNO, INVER, OUTVER, IERR)
C-----------------------------------------------------------------------
C! Selects calibrator data, smooths solutions.
C# UV Calibration EXT-appl Obselete
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C-----------------------------------------------------------------------
C   Selects calibrator data, smooths solutions as requested and
C   produces a temporary gain table.  Writes solution to end of the
C   output table.  Solutions for non selected sources have smoothed
C   values.  All antennas are processed.
C********   UNANSWERED QUESTIONS
C   1) Should both group and phase delay values be updated?  This is
C      especially a problem for the phase  and phase rate corrections.
C   2) What if anything should be done with the sideband?
C   3) How to deal with changing reference ant.  Here assumes that this
C      has already been taken care of.
C*******************************
C   Inputs:
C      DISK     I   Disk number of input gain file
C      CNO      I   catalog slot number
C      INVER    I   Input gain file version number.
C      OUTVER   I   Output gain table version number.
C   Inputs from common /SELCAL/:
C      SMOTYP   I       Smoothing type
C                          1 = amplitude
C                          2 = phase
C                          3 = sine, cosine (amp and phase)
C                          4 = sine, cosine, delay, rate
C                          5 = delay, rate
C      NCALWD   I       Number of calibrators included or excluded.
C      DOCWNT   L       If .TRUE. then calibrators in CALWAN are
C                       included, if .FALSE. then excluded.
C      CALWAN   I(30)   The source numbers of calibrators included or
C                       excluded.
C      BIF      I       First IF number selected, 1 rel. to first
C                       IF in data base. 0 => all
C      EIF      I       Last IF selected. 0=>all
C      INTFN    C*4     Interpolation function for gains
C                          '2PT ' => 2 point linear.
C                          'BOX ' => boxcar INTPRM(1) hours wide.
C      INTPRM   R(3)    Parameters for interpolation function.
C                          1 = amp, 2 = sine, cosine, 3 = delay, rate
C      SUBARR   I       Desired subarray, one and only one can be
C                       specified. If 0 then 1 will be used.
C      CATUV    I(256)  Input catalog header record.
C   Output:
C      OUTVER   I       Output gain file version number.
C      IERR     I       Return error code ,0=>OK else error.
C   Usage notes:
C    1) This routine attempts to update the total delay and rate by the
C       change in residual delay and rate.
C    2) All IF's are dealt with independently.
C    3) Sine and cosine phase smoothing may not preserve amplitude.
C    4) Input records must be 2 key sorted (antenna, Time with Antenna
C       the most slowly varying.  A check is made for Antenna sort.
C    5) The entire time range in the input file is processed, but only
C       the specified subarray.
C    6) Uses UBUFF, CURCAL, CALTAB as work space.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, INVER, OUTVER, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER LUNI, LUNO, INBUFF(512), OUTBUF(512), NUMTYP, SUBA, IA,
     *   REC2(1), IANT, IF, ITLIM, IPWT, IPVAL1, IPVAL2, PNTDAT(8),
     *   ITYP, NOIF, SOLTYP, IP, TIMKOL, INTKOL, SOUKOL, ANTKOL, SUBKOL,
     *   FRQKOL, IFRKOL, GDLKOL, DOPKOL, ATMKOL, DATKOL,
     *   MBD1KL, CGD1KL, DCG1KL, DSP1KL, DDP1KL, RE1KOL, IM1KOL, RA1KOL,
     *   DE1KOL, WE1KOL, RF1KOL,
     *   MBD2KL, CGD2KL, DCG2KL, DSP2KL, DDP2KL, RE2KOL, IM2KOL, RA2KOL,
     *   DE2KOL, WE2KOL, RF2KOL,
     *   NEXT, I, REC3(1), NUMSLN(2000), NSOLN, LIMSLN,
     *   ISOLN, INRNO, OUTRNO, LIM1, LIM2, OUTOF, LUN2
      LOGICAL   T, F, FSTPAS, SAME
      REAL      SCALE, PI, TIMES(2000), VALUE(2000), SVALUE(2000),
     *   VALU2(2000), SVALU2(2000), OLDVAL, OLDVA2, FACTOR, SQRT
C     REAL      DELOFF
      DOUBLE PRECISION    REC8(1)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   ISBAND(MAXIF), IIVER
      REAL      FINC(MAXIF), RECORD (13+30+MAXIF), FACIF(MAXIF)
      DOUBLE PRECISION    FOFF(MAXIF)
      EQUIVALENCE (NUMSLN, CURCAL)
      EQUIVALENCE  (TIMES, UBUFF),  (VALU2, UBUFF(2001)),
     *   (SVALU2, UBUFF(4001))
      EQUIVALENCE (VALUE, CALTAB(1,1)),   (SVALUE, CALTAB(1,2))
      EQUIVALENCE (CLKOLS(1), TIMKOL), (CLKOLS(2), INTKOL),
     *   (CLKOLS(3),SOUKOL), (CLKOLS(4),ANTKOL), (CLKOLS(5),SUBKOL),
     *   (CLKOLS(6),FRQKOL), (CLKOLS(7),IFRKOL),
     *   (CLKOLS(8),GDLKOL), (CLKOLS(9),DOPKOL),
     *   (CLKOLS(10),ATMKOL),(CLKOLS(11),DATKOL),
     *   (CLKOLS(12),MBD1KL),
     *   (CLKOLS(13),CGD1KL),(CLKOLS(14),DCG1KL),
     *   (CLKOLS(15),DSP1KL),(CLKOLS(16),DDP1KL),
     *   (CLKOLS(17),RE1KOL),(CLKOLS(18),IM1KOL),
     *   (CLKOLS(19),RA1KOL),(CLKOLS(20),DE1KOL),
     *   (CLKOLS(21),WE1KOL),(CLKOLS(22),RF1KOL),
     *   (CLKOLS(23),MBD2KL),
     *   (CLKOLS(24),CGD2KL),(CLKOLS(25),DCG2KL),
     *   (CLKOLS(26),DSP2KL),(CLKOLS(27),DDP2KL),
     *   (CLKOLS(28),RE2KOL),(CLKOLS(29),IM2KOL),
     *   (CLKOLS(30),RA2KOL),(CLKOLS(31),DE2KOL),
     *   (CLKOLS(32),WE2KOL),(CLKOLS(33),RF2KOL)
      EQUIVALENCE (REC8, RECORD, REC3, REC2)
      DATA T, F /.TRUE.,.FALSE./
      DATA PI /3.1415926/
      DATA LIMSLN, LUNI, LUNO, LUN2 /2000,28,29,30/
C-----------------------------------------------------------------------
C                                       IF tables
      CALL RFILL (NUMIF, 0.0, FACIF)
      NOIF = 1
      FOFF(1) = 0.0D0
      ISBAND(1) = 1
      IIVER = 1
      CALL CHNDAT ('READ', INBUFF, DISK, CNO, IIVER, CATUV, LUNI,
     *   NOIF, FOFF, ISBAND, FINC, FRQSEL, IERR)
C                                       Problem if CHNDAT failed and
C                                       more than one IF is present.
      IF ((IERR.NE.0) .AND. (NUMIF.GT.1)) GO TO 999
      DO 10 IF = 1,NOIF
         FACIF(IF) = (180.0/3.1415926) / (UVFREQ + FOFF(IF))
 10      CONTINUE
C                                       Check smoothing type 0=>1
      IF ((SMOTYP.LE.0) .OR. (SMOTYP.GT.4)) SMOTYP = 1
C                                       Determine subarray number.
      SUBA = SUBARR
      IF (SUBARR.LE.0) SUBA = 1
C                                       See if input and output files
C                                       are the same.
      SAME = INVER.EQ.OUTVER
C                                       Open input file
C                                       Reformat?
      CALL CLREFM (DISK, CNO, INVER, CATUV, LUNI, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CALINI ('READ', INBUFF, DISK, CNO, INVER, CATUV, LUNI,
     *   INRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, GMMOD, IERR)
      IF (IERR.EQ.0) GO TO 60
         WRITE (MSGTXT,1050) IERR, 'READ'
         GO TO 990
C                                       If same close input.
 60   IF (SAME) CALL TABIO ('CLOS', 0, INRNO, RECORD, INBUFF, IERR)
C                                       Open output
C                                       Reformat?
      CALL CLREFM (DISK, CNO, OUTVER, CATUV, LUNO, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CALINI ('WRIT', OUTBUF, DISK, CNO, OUTVER, CATUV, LUNO,
     *   OUTRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, GMMOD, IERR)
      IF (IERR.EQ.0) GO TO 70
         WRITE (MSGTXT,1050) IERR, 'WRIT'
         GO TO 990
C                                       Get number of solutions.
 70   NSOLN = INBUFF(5)
C                                       Check sort order, 1st key must
C                                       be Antenna no (col=4) ascend.
      IF (INBUFF(43).EQ.4) GO TO 80
         WRITE (MSGTXT,1070) INBUFF(43)
         IERR = 1
         GO TO 990
C                                       Copy sort order to output
 80   OUTBUF(43) = INBUFF(43)
      OUTBUF(44) = INBUFF(44)
C                                       Set no. of data types
      NUMTYP = 1
      IF (SMOTYP.EQ.4) NUMTYP = 2
C                                       Output offset.
      OUTOF = OUTRNO - 1
      IF (SAME) OUTOF = 0
C                                       Loop thru antennas.
C                                       Do all antennas.
      DO 600 IANT = 1,NUMANT
C                                       Find first desired record.
         LIM1 = INRNO
         DO 150 ISOLN = LIM1,NSOLN
C                                       Read.
            I = ISOLN
            IF (SAME) THEN
               CALL TABIO ('READ', 0, I, RECORD, OUTBUF, IERR)
            ELSE
               CALL TABIO ('READ', 0, I, RECORD, INBUFF, IERR)
               END IF
            IF (IERR.GT.0) GO TO 999
C                                       See if selected: antenna
            IF (REC2(ANTKOL).LT.IANT) GO TO 150
C                                       Subarray
            IF ((REC2(SUBKOL).NE.SUBA) .AND. (SUBA.NE.0)) GO TO 150
C                                       FREQID
            IF ((REC2(FRQKOL).NE.FRQSEL) .AND. (FRQSEL.GT.0))
     *         GO TO 150
            INRNO = ISOLN
            GO TO 160
 150        CONTINUE
 160     LIM1 = INRNO
         LIM2 = NSOLN
C                                       Loop thru IFs.
         DO 550 IF = BIF,EIF
C                                       Set data pointers.
            PNTDAT(1) = RE1KOL + IF - 1
            PNTDAT(2) = IM1KOL + IF - 1
            PNTDAT(3) = DE1KOL + IF - 1
            PNTDAT(4) = RA1KOL + IF - 1
            PNTDAT(5) = RE2KOL + IF - 1
            PNTDAT(6) = IM2KOL + IF - 1
            PNTDAT(7) = DE1KOL + IF - 1
            PNTDAT(8) = RA2KOL + IF - 1
C                                       Loop thru values to be smoothed.
C                                       Phase, amplitude, delay, rate;
C                                       on each IF.
C                                       (real,imag) and (delay,rate)
C                                       done as pairs.
            FSTPAS = T
            ITLIM = NUMPOL * NUMTYP
            DO 400 ITYP = 1,ITLIM
C                                       Determine type of data smoothed
C                                       1=amp, 2=phase, 3=cos,sin,
C                                       4=delay,rate
               SOLTYP = SMOTYP
               IF (SMOTYP.GE.4) SOLTYP = 4
               IF ((SMOTYP.EQ.4) .AND. ((ITYP.EQ.1).OR.(ITYP.EQ.3)))
     *            SOLTYP = 3
C                                       Init pointers, counters
               NEXT = 1
               IPWT = WE1KOL
               IF (ITYP.GT.4) IPWT = WE2KOL
               IP = 1
               IF (SOLTYP.EQ.4) IP = 3
               IF (ITYP.GT.NUMTYP) IP = IP + 4
               IPVAL1 = PNTDAT(IP)
               IPVAL2 = PNTDAT(IP+1)
C                                       Smoothing time
               SCALE = INTPRM(1)
               IF (SOLTYP.GT.1) SCALE = INTPRM(2)
               IF (SOLTYP.GE.4) SCALE = INTPRM(3)
C                                       Loop thru data
               DO 200 INRNO = LIM1,LIM2
C                                       Read.
                  IF (SAME) THEN
                     CALL TABIO ('READ', 0, INRNO, RECORD, OUTBUF, IERR)
                  ELSE
                     CALL TABIO ('READ', 0, INRNO, RECORD, INBUFF, IERR)
                     END IF
                  IF (IERR.GT.0) GO TO 999
C                                       Check if right data
                  IF (REC2(ANTKOL).GT.IANT)  GO TO 210
C                                       Check subarray.
                  IF ((REC2(SUBKOL).NE.SUBA) .AND. (SUBA.NE.0))
     *               GO TO 200
C                                       Check freqid
                  IF ((REC2(FRQKOL).NE.FRQSEL) .AND. (FRQSEL.GT.0))
     *               GO TO 200
C                                       Initialize
                  NUMSLN(NEXT) = INRNO
                  TIMES(NEXT) = RECORD(TIMKOL)
                  VALUE(NEXT) = FBLANK
                  VALU2(NEXT) = FBLANK
C                                       Check if data good
                  IF ((IERR.LT.0) .OR. (RECORD(IPWT).LT.0)) GO TO 190
C                                       Check source.
                  IF (NCALWD.EQ.0) GO TO 180
                  DO 170 IA = 1,NCALWD
                     IF ((REC2(SOUKOL).EQ.CALWAN(IA)) .AND. DOCWNT)
     *                  GO TO 180
                     IF ((REC2(SOUKOL).EQ.CALWAN(IA)) .AND.
     *                  (.NOT.DOCWNT)) GO TO 190
 170                 CONTINUE
                  IF (DOCWNT) GO TO 190
C                                       Data good.
C                                       Amplitude
 180              IF(SOLTYP.GT.1) GO TO 185
                  IF ((RECORD(IPVAL1).EQ.FBLANK) .OR.
     *               (RECORD(IPVAL2).EQ.FBLANK)) GO TO 190
                     VALUE(NEXT) = SQRT (RECORD(IPVAL1) *
     *                  RECORD(IPVAL1) + RECORD(IPVAL2) *
     *                  RECORD(IPVAL2))
                     GO TO 190
C                                       Other
 185              VALUE(NEXT) =  RECORD(IPVAL1)
                  VALU2(NEXT) =  RECORD(IPVAL2)
 190              NEXT = NEXT + 1
C                                       Too many data points
                  IF (NEXT.LE.LIMSLN) GO TO 200
                     WRITE (MSGTXT,1190) NEXT, LIMSLN, IANT, IF
                     CALL MSGWRT (6)
                     GO TO 210
 200        CONTINUE
 210        NEXT = NEXT - 1
            IF (INRNO.LT.LIM2) LIM2 = INRNO
C                                       See if data obtained.
            IF (NEXT.LE.0) GO TO 550
C                                       Call smoother.
               CALL BOXBSM (SCALE, TIMES, VALUE, FBLANK, NEXT, SVALUE)
               IF (SOLTYP.GT.1)
     *            CALL BOXBSM (SCALE, TIMES, VALU2, FBLANK, NEXT,
     *               SVALU2)
C                                       Update cal table.
               DO 300 I = 1,NEXT
                  ISOLN = NUMSLN(I)
                  OUTRNO = OUTOF + I
C                                       Read.
                  IF (FSTPAS.AND.(.NOT.SAME))
     *               CALL TABIO ('READ', 0, ISOLN, RECORD, INBUFF,
     *                  IERR)
                  IF (FSTPAS.AND.SAME)
     *               CALL TABIO ('READ', 0, ISOLN, RECORD, OUTBUF,
     *                  IERR)
                  IF (.NOT.FSTPAS) CALL TABIO ('READ', 0, OUTRNO,
     *               RECORD, OUTBUF, IERR)
                  IF (IERR.GT.0) GO TO 999
C                                       Get old delay or rate.
                  OLDVAL = RECORD(IPVAL1)
                  OLDVA2 = RECORD(IPVAL2)
C                                       Get, update record.
                  FACTOR = 1.0
C                                       Branch on type.
                  GO TO (225, 230, 235, 235), SOLTYP
C                                       Amplitude
 225                 IF ((RECORD(IPVAL1).NE.FBLANK) .AND.
     *                  (RECORD(IPVAL2).NE.FBLANK))
     *                  FACTOR = SVALUE(I) / (SQRT (RECORD(IPVAL1) *
     *                     RECORD(IPVAL1) + RECORD(IPVAL2) *
     *                     RECORD(IPVAL2)))
                     RECORD(IPVAL1) = FACTOR * RECORD(IPVAL1)
                     RECORD(IPVAL2) = FACTOR * RECORD(IPVAL2)
                     GO TO 290
C                                       Phase only
 230                 FACTOR = 1.0 / SQRT (SVALUE(I) * SVALUE(I) +
     *                  SVALU2(I) * SVALU2(I))
C                                       All but amplitude
 235                 RECORD(IPVAL1) = SVALUE(I) * FACTOR
                     RECORD(IPVAL2) = SVALU2(I) * FACTOR
C                                       Update total values.
                     SOLTYP = 1
CCC                     GO TO (290, 260, 260, 265), SOLTYP
                     GO TO 290
C                                       Phase delay
C 260                 DELOFF = 0
C                     IF ((OLDVAL.NE.FBLANK) .AND. (OLDVA2.NE.FBLANK))
C     *                  DELOFF = (ATAN2 (SVALU2(I), SVALUE(I)+1.0E-20) -
C     *                     ATAN2 (OLDVA2, OLDVAL+1.0E-20))
CC                                       Phase difference < pi
C                     DELOFF = MOD (DELOFF, PI) * FACIF(IF)
C                     IF (ITYP.GT.NUMTYP) GO TO 270
CC                                       Polarization 1
C                     REC8(APD1KL) = REC8(APD1KL) + DELOFF
C                     GO TO 290
CC                                       Group delay
C 265                 IF (ITYP.GT.NUMTYP) GO TO 275
CC                                       Polarization 1
C                     REC8(AGD1KL) = REC8(AGD1KL) + SVALUE(I) - OLDVAL
C                     REC8(APD1KL) = REC8(APD1KL) + SVALUE(I) - OLDVAL
CC                                       Delay rate 1
C                     REC8(DAG1KL) = REC8(DAG1KL) + SVALU2(I) - OLDVA2
C                     REC8(DAP1KL) = REC8(DAP1KL) + SVALU2(I) - OLDVA2
C                     GO TO 290
CC                                       Phase delay 2
C 270                 REC8(APD2KL) = REC8(APD2KL) + DELOFF
C                     REC8(AGD2KL) = REC8(AGD2KL) + DELOFF
C                     GO TO 290
CC                                       Group delay 2
C 275                 REC8(AGD2KL) = REC8(AGD2KL) + SVALUE(I) - OLDVAL
C                     REC8(APD2KL) = REC8(APD2KL) + SVALUE(I) - OLDVAL
CC                                       Delay rate 2
C                     REC8(DAG2KL) = REC8(DAG2KL) + SVALU2(I) - OLDVA2
C                     REC8(DAP2KL) = REC8(DAP2KL) + SVALU2(I) - OLDVA2
C                     GO TO 290
C                                       Write back.
 290              CALL TABIO ('WRIT', 0, OUTRNO, RECORD, OUTBUF, IERR)
                  IF (IERR.NE.0) GO TO 999
 300              CONTINUE
               FSTPAS = F
 400           CONTINUE
            OUTOF = OUTOF + NEXT
 550        CONTINUE
 600     CONTINUE
C                                       If overwriting file, set new
C                                       no. entries
      IF (INVER.EQ.OUTVER) OUTBUF(5) = OUTOF
C                                       Close CAL files.
      IF (.NOT.SAME) CALL TABIO ('CLOS', 0, INRNO, RECORD, INBUFF,
     *   IERR)
      CALL TABIO ('CLOS', 0, INRNO, RECORD, OUTBUF, IERR)
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('SELSMG: ERROR',I5,' INIT. FOR ',A4,' CAL TABLE')
 1070 FORMAT ('SELSMG: CAL TABLE IN WRONG ORDER FOR SMOOTH',I4)
 1190 FORMAT ('SELSMG: TOO MANY CALS=',I5,',MAX=',I5,',ANT=',I3,
     *   ',IF=',I2)
      END
