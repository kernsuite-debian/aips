      SUBROUTINE REFRAT (ISUB, REFA, ANT, NUMIF, TIMKOL, SUBKOL, ANTKOL,
     *   SOUKOL, FRQKOL, RF1KOL, RA1KOL, WT1KOL, RF2KOL, RA2KOL, WT2KOL,
     *   BUFFER, MXTIME, WRKTIM, WORK2, WORK3, IRET)
C-----------------------------------------------------------------------
C! Adjusts the reference antenna in an SN table.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C   Routine to rereference rates in an open table. Rates are averaged
C   over IF and polarization.  If possible only table entries with valid
C   entries in both polarizations are used to rereference data.
C   All valid entries are included.
C      Note: this routine should be called before REFFAZ.  This routine
C   does not modify the reference antennas in the table
C   Inputs:
C      ISUB     I  Subarray number, 0=>1
C      REFA     I  Primary reference antenna
C      ANT      I  Secondary reference antenna
C      NUMIF    I  The number of IFs
C      TIMKOL   I  Time column pointer.
C      SUBKOL   I  Subarray column pointer
C      ANTKOL   I  Antenna column pointer
C      SOUKOL   I  Source ID column pointer
C      FRQKOL   I  FQ id column pointer
C      RF1KOL   I  Reference antenna 1 pointer
C      RA1KOL   I  Rate 1  column pointer
C      WT1KOL   I  Weight 1 column pointer.
C      RF2KOL   I  Reference antenna 2 pointer <1 => not present
C      RA2KOL   I  Rate 2  column pointer <1 => not present
C      WT2KOL   I  Weight 2 column pointer <1 => not present
C      MXTIME   I  Dimension of work arrays
C   Input/Output:
C      BUFFER   I(*) Table I/O buffer
C      WRKTIM   R(*) Work array
C      WORK2    R(*) Work array
C      WORK3    R(*) Work array
C   Output:
C      IRET     I  Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   ISUB, REFA, ANT, NUMIF, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *   FRQKOL, RF1KOL, RA1KOL, WT1KOL, RF2KOL, RA2KOL, WT2KOL,
     *   BUFFER(*), MXTIME, IRET
      REAL      WRKTIM(MXTIME), WORK2(MXTIME), WORK3(MXTIME)
C
      INTEGER    LOOPR, IRCODE, NUMTIM, IPNT1, IPNT2, REFA1, REFA2, I,
     *   NUMREC, SUB, SA
      LOGICAL   NEED2, WANT
      REAL    WT1, WT2, SUM, COUNT, RATE, RATE1, RATE2, SMOTIM
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION TIMOFF, TIME, TIME1, TIME2
      INTEGER   RECORD(1024)
      REAL      RECR(1024)
      DOUBLE PRECISION RECD(512)
      EQUIVALENCE (RECORD, RECR, RECD)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
C                                       Initially require both
C                                       polarizations if present
      NEED2 = RA2KOL.GT.0
      SUB = MAX (1, ISUB)
C                                       Loop thru table referring ANT
C                                       to REFA.
      NUMREC = BUFFER(5)
 10   NUMTIM = 0
      DO 100 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) GO TO 100
         IF (IRET.NE.0) GO TO 900
         IF ((RECORD(ANTKOL).NE.ANT) .AND. (RECORD(ANTKOL).NE.REFA))
     *      GO TO 100
C                                       Right subarray?
         SA = RECORD(SUBKOL)
         IF ((SA.NE.SUB) .AND. (SA.GT.0)) GO TO 100
C                                        Find and check reference
C                                        antennas.  Must all be the
C                                        same.
         REFA1 = -1
         REFA2 = -1
C                                        Also accumulate rate.
         SUM = 0.0
         COUNT = 0.0
         DO 20 I = 1,NUMIF
            IF ((RECR(WT1KOL+I-1).GT.0.0) .AND.
     *         (RECR(RA1KOL+I-1).NE.FBLANK) .AND.
     *         (RECORD(RF1KOL+I-1).GT.0)) THEN
               IF (REFA1.LE.0) REFA1 = RECORD(RF1KOL+I-1)
               IF (REFA1.NE.RECORD(RF1KOL+I-1)) GO TO 100
               SUM = SUM + RECR(RA1KOL+I-1)
               COUNT = COUNT + 1
               END IF
            IF ((RA2KOL.GT.0) .AND. (RECR(WT2KOL+I-1).GT.0.0) .AND.
     *         (RECR(RA2KOL+I-1).NE.FBLANK) .AND.
     *         (RECORD(RF2KOL+I-1).GT.0)) THEN
               IF (REFA2.LE.0) REFA2 = RECORD(RF2KOL+I-1)
               IF (REFA2.NE.RECORD(RF2KOL+I-1)) GO TO 100
               SUM = SUM + RECR(RA2KOL+I-1)
               COUNT = COUNT + 1
               END IF
 20            CONTINUE
C                                       Desired antenna combination?
         IF (NEED2 .AND. (REFA1.GT.0) .AND. (REFA2.GT.0) .AND.
     *      (REFA1.NE.REFA2)) GO TO 100
         IF (REFA1.LT.0) REFA1 = REFA2
         IF ((REFA1.NE.ANT) .AND. (REFA1.NE.REFA))
     *      GO TO 100
         IF (RECORD(ANTKOL).EQ.REFA1) GO TO 100
         IF (NUMTIM.GE.MXTIME) GO TO 100
            RATE = 0.0
            IF (COUNT.GT.0) RATE = SUM / COUNT
            NUMTIM = NUMTIM + 1
            IF (NUMTIM.EQ.1) TIMOFF = RECD(TIMKOL)
            WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
            IF (REFA1.NE.ANT) THEN
C                                       REFA is reference ant
               WORK3(NUMTIM) = RATE
            ELSE
C                                       ANT is reference ant
               WORK3(NUMTIM) = -RATE
               END IF
            IF (COUNT.EQ.0) WORK3(NUMTIM) = FBLANK
 100     CONTINUE
      IRET = 0
      IF (NEED2 .AND. (NUMTIM.LE.0)) THEN
C                                       Try again with only one poln.
         NEED2 = .FALSE.
         GO TO 10
         END IF
C                                       Find any?
      IF (NUMTIM.LE.0) GO TO 999
C                                       Smooth (2 sec to extrapolate)
      SMOTIM = 2.0 / 86400.0
      CALL BOXSMO (SMOTIM, WRKTIM, WORK3, NUMTIM, WORK2)
C                                       Set up for interpolation
      IPNT1 = 1
      IPNT2 = 2
      TIME1 = WRKTIM(1)
      TIME2 = WRKTIM(2)
      IF (NUMTIM.EQ.1) THEN
         IPNT2 = 1
         TIME2 = TIME1
         END IF
C                                       Loop thru table changing any
C                                       data with ref=ANT to ref=REFA
      DO 200 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) GO TO 200
         IF (IRET.NE.0) GO TO 900
C                                       Right subarray?
         SA = RECORD(SUBKOL)
         IF ((SA.NE.SUB) .AND. (SA.GT.0)) GO TO 200
         WANT = .FALSE.
         DO 130 I = 1,NUMIF
            WANT = WANT .OR. ((RECORD(RF1KOL).EQ.ANT)
     *         .OR. ((RF2KOL.GT.0) .AND. (RECORD(RF2KOL).NE.ANT)))
 130        CONTINUE
         IF (.NOT.WANT) GO TO 200
C                                       Interpolate
         TIME = RECD(TIMKOL) - TIMOFF
 140           IF ((TIME.GE.TIME1) .AND. (TIME.LE.TIME2)) THEN
C                                       Between entries
                  IF (TIME2.NE.TIME1) THEN
                     WT1 = 1.0 - ((TIME-TIME1) / (TIME2-TIME1))
                  ELSE
                     WT1 = 1.0
                     END IF
               ELSE IF (TIME.LT.TIME1) THEN
C                                       Before first time
                  WT1 = 1.0
               ELSE IF (IPNT2.GE.NUMTIM) THEN
C                                       After last time
                  WT1 = 0.0
               ELSE
C                                       Shift in interpolation arrays
                  IPNT1 = IPNT1 + 1
                  TIME1 = WRKTIM(IPNT1)
                  IPNT2 = IPNT2 + 1
                  TIME2 = WRKTIM(IPNT2)
                  GO TO 140
                  END IF
C                                       Interpolate
                  WT2 = 1.0 - WT1
                  RATE1 = WORK2(IPNT1)
                  RATE2 = WORK2(IPNT2)
                  RATE = WT1*RATE1 + WT2*RATE2
                  DO 120 I = 1,NUMIF
                     IF ((RECR(WT1KOL+I-1).GT.0.0) .AND.
     *                  (RECR(RA1KOL+I-1).NE.FBLANK) .AND.
     *                  (RECORD(RF1KOL+I-1).EQ.ANT)) THEN
                        RECR(RA1KOL+I-1) = RECR(RA1KOL+I-1) + RATE
                        END IF
                     IF ((RA2KOL.GT.0) .AND. (RECR(WT2KOL+I-1).GT.0.0)
     *                  .AND.  (RECR(RA2KOL+I-1).NE.FBLANK) .AND.
     *                  (RECORD(RF2KOL+I-1).EQ.ANT)) THEN
                        RECR(RA2KOL+I-1) = RECR(RA2KOL+I-1) + RATE
                        END IF
 120                 CONTINUE
C                                       Rewrite record
            CALL TABIO ('WRIT', IRCODE, LOOPR, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 900
 200     CONTINUE
      IRET = 0
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET, ANT, REFA
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('REFRAT: TABIO ERROR',I3,' REREFERENCING ANT ',I3,' TO ',
     *   I3)
      END
