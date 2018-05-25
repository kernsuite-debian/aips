      SUBROUTINE REFFAZ (ISUB, REFA, ANT, TIMKOL, SUBKOL, ANTKOL,
     *   RF1KOL, RE1KOL, IM1KOL, WT1KOL, RF2KOL, RE2KOL, IM2KOL, WT2KOL,
     *   BUFFER, MXTIME, WRKTIM, WORK1, WORK2, WORK3, WORK4, WORK5,
     *   IRET)
C-----------------------------------------------------------------------
C! Adjusts the reference antenna in an SN table.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2003, 2015
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
C   Routine to rereference phases in an a polarization coherent fashion;
C   i.e. both polarizations must be present (if possible) in data used
C   to determine the relative phase between the primary and secondary
C   reference antennas.  This routine does one IF at a time but both
C   polarizations (if present) are done.
C   All valid entries are included.
C      Note: reference antenna values in the table are modified.
C   Inputs:
C      ISUB     I  Subarray number, 0=>1
C      REFA     I  Primary reference antenna
C      ANT      I  Secondary reference antenna
C      TIMKOL   I  Time column pointer.
C      SUBKOL   I  Subarray column pointer
C      ANTKOL   I  Antenna column pointer
C      RF1KOL   I  Reference antenna 1 pointer
C      RE1KOL   I  Real 1  column pointer
C      IM1KOL   I  Imaginary 1  column pointer
C      WT1KOL   I  Weight 1 column pointer.
C      RF2KOL   I  Reference antenna 2 pointer <1 => not present
C      RE2KOL   I  Real 2  column pointer <1 => not present
C      IM2KOL   I  Imaginary 2  column pointer
C      WT2KOL   I  Weight 2 column pointer <1 => not present
C      MXTIME   I  Dimension of work arrays
C   Input/Output:
C      BUFFER   I(*) Table I/O buffer
C      WRKTIM   R(*) Work array
C      WORK1    R(*) Work array
C      WORK2    R(*) Work array
C      WORK3    R(*) Work array
C      WORK4    R(*) Work array
C      WORK5    R(*) Work array
C   Output:
C      IRET     I  Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   ISUB,REFA, ANT, TIMKOL, SUBKOL, ANTKOL, RF1KOL, RE1KOL,
     *   IM1KOL, WT1KOL, RF2KOL, RE2KOL, IM2KOL, WT2KOL, BUFFER(*),
     *   MXTIME, IRET
      REAL      WRKTIM(MXTIME), WORK1(MXTIME), WORK2(MXTIME),
     *   WORK3(MXTIME), WORK4(MXTIME), WORK5(MXTIME)
C
      INTEGER    LOOPR, IRCODE, NUMTIM, IPNT1, IPNT2, REFA1, REFA2,
     *   NUMREC, SUB, SA, SORC
      LOGICAL   NEED2, DOTWO
      REAL    WT1, WT2, RE1, RE2, IM1, IM2, AMP, TRE, TIM, SMOTIM
      DOUBLE PRECISION TIMOFF, TIME, TIME1, TIME2
      INTEGER   RECORD(1024)
      REAL      RECR(1024)
      DOUBLE PRECISION RECD(512)
      EQUIVALENCE (RECORD, RECR, RECD)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
      SORC = -1
C                                       Initially require both
C                                       polarizations if present
      NEED2 = RE2KOL.GT.0
      DOTWO = RE2KOL.GT.0
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
C                                       Find and check reference
C                                       antennas.  Must all be the
C                                       same.
         REFA1 = RECORD(RF1KOL)
         REFA2 = RECORD(RF2KOL)
C                                        Bad solution?
         IF (((RECR(WT1KOL).LE.0.0) .OR.
     *      (RECR(RE1KOL).EQ.FBLANK) .OR.
     *      (RECORD(RF1KOL).LE.0))) GO TO 100
         IF (NEED2 .AND. ((RE2KOL.GT.0) .AND. ((RECR(WT2KOL).LE.0.0)
     *      .OR. (RECR(RE2KOL).EQ.FBLANK) .OR.
     *      (RECORD(RF2KOL).LE.0)))) GO TO 100
C                                       Desired antenna combination?
         IF (NEED2 .AND. (REFA1.GT.0) .AND. (REFA2.GT.0) .AND.
     *      (REFA1.NE.REFA2)) GO TO 100
         IF (REFA1.LT.0) REFA1 = REFA2
         IF ((REFA1.NE.ANT) .AND. (REFA1.NE.REFA))
     *      GO TO 100
         IF (RECORD(ANTKOL).EQ.REFA1) GO TO 100
         IF (NUMTIM.GE.MXTIME) GO TO 100
            NUMTIM = NUMTIM + 1
            IF (NUMTIM.EQ.1) TIMOFF = RECD(TIMKOL)
            WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
            IF (REFA1.NE.ANT) THEN
C                                       REFA is reference ant
               WORK2(NUMTIM) = RECR(RE1KOL)
               WORK3(NUMTIM) = RECR(IM1KOL)
               IF (DOTWO) THEN
                  WORK4(NUMTIM) = RECR(RE2KOL)
                  WORK5(NUMTIM) = RECR(IM2KOL)
                  END IF
            ELSE
C                                       ANT is reference ant
               WORK2(NUMTIM) = RECR(RE1KOL)
               WORK3(NUMTIM) = -RECR(IM1KOL)
               IF (DOTWO) THEN
                  WORK4(NUMTIM) = RECR(RE2KOL)
                  WORK5(NUMTIM) = -RECR(IM2KOL)
                  END IF
               END IF
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
      CALL BOXBSM (SMOTIM, WRKTIM, WORK2, SORC, FBLANK, NUMTIM, WORK1)
      CALL BOXBSM (SMOTIM, WRKTIM, WORK3, SORC, FBLANK, NUMTIM, WORK2)
      IF (DOTWO) THEN
         CALL BOXBSM (SMOTIM, WRKTIM, WORK4, SORC, FBLANK, NUMTIM,
     *      WORK3)
         CALL BOXBSM (SMOTIM, WRKTIM, WORK5, SORC, FBLANK, NUMTIM,
     *      WORK4)
         END IF
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
         IF ((RECORD(RF1KOL).NE.ANT) .AND. ((RF2KOL.GT.0) .AND.
     *      (RECORD(RF2KOL).NE.ANT))) GO TO 200
C                                       Right subarray?
         SA = RECORD(SUBKOL)
C                                       Interpolate
         IF ((SA.EQ.SUB) .OR. (SA.LE.0)) THEN
            TIME = RECD(TIMKOL) - TIMOFF
 140        IF ((TIME.GE.TIME1) .AND. (TIME.LE.TIME2)) THEN
C                                       Between entries
               IF (TIME2.NE.TIME1) THEN
                  WT1 = 1.0 - ((TIME-TIME1) / (TIME2-TIME1))
               ELSE
                  WT1 = 1.0
                  END IF
C                                       Before first time
            ELSE IF (TIME.LT.TIME1) THEN
               WT1 = 1.0
C                                       After last time
            ELSE IF (IPNT2.GE.NUMTIM) THEN
               WT1 = 0.0
C                                       Shift in interpolation arrays
            ELSE
               IPNT1 = IPNT1 + 1
               TIME1 = WRKTIM(IPNT1)
               IPNT2 = IPNT2 + 1
               TIME2 = WRKTIM(IPNT2)
               GO TO 140
               END IF
C                                       Interpolate
            WT2 = 1.0 - WT1
            IF (RECORD(RF1KOL).EQ.ANT) THEN
C                                       Interpolate phase pol 1
               IF ((RECR(IM1KOL).NE.FBLANK) .AND.
     *            (RECR(RE1KOL).NE.FBLANK)) THEN
                  RE1 = WT1 * WORK1(IPNT1) + WT2 * WORK1(IPNT2)
                  IM1 = WT1 * WORK2(IPNT1) + WT2 * WORK2(IPNT2)
                  AMP = MAX (SQRT (RE1*RE1 + IM1*IM1), 1.0E-10)
                  RE1 = RE1 / AMP
                  IM1 = IM1 / AMP
C                                       Correct phase 1
                  TRE = RECR(RE1KOL)
                  TIM = RECR(IM1KOL)
                  RECR(RE1KOL) = TRE*RE1 - TIM*IM1
                  RECR(IM1KOL) = TRE*IM1 + TIM*RE1
                  END IF
C                                       Relabel reference antenna
               RECORD(RF1KOL) = REFA
C                                       Null ref ant
            ELSE IF (RECORD(RF1KOL).EQ.0) THEN
C                                       Relabel if blanked
               IF ((RECR(IM1KOL).EQ.FBLANK) .OR.
     *            (RECR(RE1KOL).EQ.FBLANK)) RECORD(RF1KOL) = REFA
               END IF
            IF ((RE2KOL.GT.0) .AND. (RECORD(RF2KOL).EQ.ANT)) THEN
C                                       Interpolate second poln.
               IF ((RECR(IM2KOL).NE.FBLANK) .AND.
     *            (RECR(RE2KOL).NE.FBLANK)) THEN
                  RE2 = WT1 * WORK3(IPNT1) + WT2 * WORK3(IPNT2)
                  IM2 = WT1 * WORK4(IPNT1) + WT2 * WORK4(IPNT2)
                  AMP = MAX (SQRT (RE2*RE2 + IM2*IM2), 1.0E-10)
                  RE2 = RE2 / AMP
                  IM2 = IM2 / AMP
C                                       Correct phase.
                  TRE = RECR(RE2KOL)
                  TIM = RECR(IM2KOL)
                  RECR(RE2KOL) = TRE*RE2 - TIM*IM2
                  RECR(IM2KOL) = TRE*IM2 + TIM*RE2
                  END IF
C                                       Relabel reference antenna
               RECORD(RF2KOL) = REFA
C                                       Null ref ant
            ELSE IF ((RE2KOL.GT.0) .AND. (RECORD(RF2KOL).EQ.0)) THEN
C                                       Relabel if blanked
               IF ((RECR(IM2KOL).EQ.FBLANK) .OR.
     *            (RECR(RE2KOL).EQ.FBLANK)) RECORD(RF2KOL) = REFA
               END IF
C                                       Rewrite record
            CALL TABIO ('WRIT', IRCODE, LOOPR, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 900
            END IF
 200     CONTINUE
      IRET = 0
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET, ANT, REFA
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('REFFAZ: TABIO ERROR',I3,' REREFERENCING ANT ',I3,' TO ',
     *   I3)
      END
