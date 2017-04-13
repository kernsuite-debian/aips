      SUBROUTINE ASOLVE (PRTLEV, CTYPE, BEGTIM, ENDTIM, NANT, FFLAST,
     *   SSQ, SASQ, FRACT)
C-----------------------------------------------------------------------
C! Finds antenna-based amp from baseline amp**2 data ROBUSTLY
C# Subroutine OOP calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2004-2006
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
C   Converts array of baseline sigma**2 or amp**2 to antenna-based
C   sigma or amp
C   Inputs:
C      PRTLEV   I      Print answers errors
C      CTYPE    C*3    If 'rms', diagonal elements averaged
C      BEGTIM   R      Begin time
C      ENDTIM   R      End time
C      NANT     I      Number antennas
C      FFLAST   R      Mark bad those data with closure error >
C                      FFLAST * rms closure error on last iteration
C   In/Out
C      SSQ      R(*)   Sigma**2 by baseline
C   Outputs
C      SASQ     R(*)   Antenna sigma, amp
C                         FBLANK => no data, <= 0 => failed
C      FRACT    R(*)   Fraction of this antenna > 3. sigma from closure
C-----------------------------------------------------------------------
      INTEGER   PRTLEV, NANT
      CHARACTER CTYPE*(*)
      REAL      BEGTIM, ENDTIM, FFLAST, SSQ(NANT,NANT), SASQ(NANT),
     *   FRACT(NANT)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, J, NRMS, IT, NODATA(MAXANT), BEGT(4),  ENDT(4), ITER
      REAL      ARMS, FACT, SUM, SUMS, RMS, FF(10), LSQ(MAXANT), FB(10),
     *   RMSMIN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA FB /15.0, 8.0, 6.0, 4.5, 3.5, 3.0, 2.8, 2.6, 2.4, 3.0/
C-----------------------------------------------------------------------
      CALL RFILL (NANT, FBLANK, SASQ)
      FACT = 1.0
      IF (CTYPE.EQ.'rms') FACT = 0.5
      CALL RCOPY (10, FB, FF)
      IF (FFLAST.GT.1.0) FF(10) = FFLAST
C                                       sum in Re and Im
C                                       or take one if other blanked
C                                       (usually scalar amp...)
      NRMS = 0
      ARMS = 0.0
      DO 20 I = 1,NANT
         NODATA(I) = 0
         DO 10 J = I+1,NANT
            IF (SSQ(I,J).NE.FBLANK) THEN
               IF (SSQ(J,I).NE.FBLANK) SSQ(I,J) = (SSQ(I,J) + SSQ(J,I))
     *            * FACT
            ELSE
               IF (SSQ(J,I).NE.FBLANK) SSQ(I,J) = SSQ(J,I)
               END IF
            IF (SSQ(I,J).LE.0.0) SSQ(I,J) = FBLANK
            IF (SSQ(I,J).NE.FBLANK) THEN
               NRMS = NRMS + 1
               ARMS = ARMS + SSQ(I,J)
               END IF
 10         CONTINUE
 20      CONTINUE
      IF (ARMS.LE.0.0) THEN
         IF (PRTLEV.GE.3) THEN
            CALL TODHMS (BEGTIM, BEGT)
            CALL TODHMS (ENDTIM, ENDT)
            WRITE (MSGTXT,1010) CTYPE, BEGT, ENDT
            CALL MSGWRT (6)
            END IF
C                                       iteration 0
      ELSE
         ARMS = ARMS / NRMS
         ARMS = SQRT (ARMS)
         RMSMIN = ARMS / 1000.0
         IF (PRTLEV.GE.4) THEN
            CALL TODHMS (BEGTIM, BEGT)
            CALL TODHMS (ENDTIM, ENDT)
            WRITE (MSGTXT,1020) CTYPE, BEGT, ENDT, ARMS
            CALL MSGWRT (2)
            END IF
         DO 40 I = 1,NANT
            ARMS = 0.0
            NRMS = 0
            DO 25 J = I+1,NANT
               IF (SSQ(I,J).NE.FBLANK) THEN
                  NRMS = NRMS + 1
                  ARMS = ARMS + SSQ(I,J)
                  END IF
 25            CONTINUE
            DO 30 J = 1,I
               IF (SSQ(J,I).NE.FBLANK) THEN
                  NRMS = NRMS + 1
                  ARMS = ARMS + SSQ(J,I)
                  END IF
 30            CONTINUE
            IF (ARMS.GT.0.0) THEN
               SASQ(I) = SQRT (ARMS / NRMS)
            ELSE
               NODATA(I) = 1000000
               END IF
            LSQ(I) = SASQ(I)
 40         CONTINUE
         RMS = 1.E14
C                                       robust iterations
         DO 90 ITER = 1,10
C                                       iterate new solution
            DO 60 IT = 1,5
               DO 55 I = 1,NANT
                  ARMS = 0.0
                  NRMS = 0
                  DO 45 J = I+1,NANT
                     IF (SSQ(I,J).NE.FBLANK) THEN
                        IF (ABS(SSQ(I,J)-SASQ(I)*SASQ(J))/RMS
     *                     .LT.FF(ITER)) THEN
                           NRMS = NRMS + 1
                           ARMS = ARMS + SSQ(I,J) / LSQ(J)
                           END IF
                        END IF
 45                  CONTINUE
                  DO 50 J = 1,I
                     IF (SSQ(J,I).NE.FBLANK) THEN
                        IF (ABS(SSQ(J,I)-SASQ(I)*SASQ(J))/RMS
     *                     .LT.FF(ITER)) THEN
                           NRMS = NRMS + 1
                           ARMS = ARMS + SSQ(J,I) / LSQ(J)
                           END IF
                        END IF
 50                  CONTINUE
                  IF (ARMS.GT.0.0) THEN
                     LSQ(I) = ARMS / NRMS
                     NODATA(I) = 0
                  ELSE
                     IF (NODATA(I).LE.0) NODATA(I) = 1
                     END IF
 55               CONTINUE
 60            CONTINUE
            DO 70 I = 1,NANT
               SASQ(I) = LSQ(I)
 70            CONTINUE
C                                       find rms of soln
            SUM = 0
            SUMS = 0
            NRMS = 0
            DO 85 I = 1,NANT
               IF (SASQ(I).NE.FBLANK) THEN
                  DO 75 J = I+1,NANT
                     IF ((SSQ(I,J).NE.FBLANK) .AND. (SASQ(J).NE.FBLANK))
     *                  THEN
                        IF (ABS(SSQ(I,J)-SASQ(I)*SASQ(J))/RMS
     *                     .LT.FF(ITER)) THEN
                           ARMS = SSQ(I,J) - SASQ(I)*SASQ(J)
                           NRMS = NRMS + 1
                           SUM = SUM + ARMS
                           SUMS = SUMS + ARMS * ARMS
                           END IF
                        END IF
 75                  CONTINUE
                  DO 80 J = 1,I
                     IF ((SSQ(J,I).NE.FBLANK) .AND. (SASQ(J).NE.FBLANK))
     *                  THEN
                        IF (ABS(SSQ(J,I)-SASQ(I)*SASQ(J))/RMS
     *                     .LT.FF(ITER)) THEN
                           ARMS = SSQ(J,I) - SASQ(I)*SASQ(J)
                           NRMS = NRMS + 1
                           SUM = SUM + ARMS
                           SUMS = SUMS + ARMS * ARMS
                           END IF
                        END IF
 80                  CONTINUE
                  END IF
 85            CONTINUE
            SUM = SUM / NRMS
            SUMS = SUMS / NRMS - SUM * SUM
            RMS = SQRT (MAX (0.0, SUMS))
            RMS = MAX (RMS, RMSMIN)
            IF (RMS.LE.0.0) RMS = 1.0
 90         CONTINUE
C                                       DONE!  Get stat re error
         DO 120 I = 1,NANT
            NRMS = 0
            SUM = 0.
            IF (SASQ(I).NE.FBLANK) THEN
               DO 105 J = I+1,NANT
                  IF ((SSQ(I,J).NE.FBLANK) .AND. (SASQ(J).NE.FBLANK))
     *               THEN
                     ARMS = ABS (SSQ(I,J) - SASQ(I)*SASQ(J))
                     IF (ARMS/RMS.GT.5.0) SUM = SUM + 1.0
                     NRMS = NRMS + 1
                     END IF
 105              CONTINUE
               DO 110 J = 1,I
                  IF ((SSQ(J,I).NE.FBLANK) .AND. (SASQ(J).NE.FBLANK))
     *               THEN
                     ARMS = ABS (SSQ(J,I) - SASQ(I)*SASQ(J))
                     IF (ARMS/RMS.GT.3.0) SUM = SUM + 1.0
                     NRMS = NRMS + 1
                     END IF
 110              CONTINUE
               END IF
            IF (NRMS.GT.0) THEN
               FRACT(I) = SUM / NRMS
            ELSE
               FRACT(I) = 1.0
               END IF
 120        CONTINUE
C                                       now flag if needed
         DO 130 I = 1,NANT
            IF (NODATA(I).GE.1000000) THEN
               SASQ(I) = FBLANK
            ELSE IF (NODATA(I).GT.0) THEN
               SASQ(I) = - ABS (SASQ(I))
               END IF
 130        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ASOLV: FAILS ',A,' AT',I3,'/',2(I2.2,':'),I2.2,' -',
     *   I3,'/',2(I2.2,':'),I2.2)
 1020 FORMAT ('ASOLV: average ',A,' at',I3,'/',2(I2.2,':'),I2.2,' -',
     *   I3,'/',2(I2.2,':'),I2.2,' =',F9.5)
      END
