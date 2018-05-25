      SUBROUTINE ECALC (INIT, VOBS, IS, JS, WT, NUMBL, REFANT, MODE,
     *   MINNO, G, NREF, PRTLV, IERR)
C-----------------------------------------------------------------------
C! Gain Soln: exact solutions for 1 or 2 baselines only
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2004, 2014
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
C   ECALC computes antenna gains given visibilities divided by the
C   model values for 1 or 2 baselines only
C   Inputs:
C      INIT     L        T -> start with (1,0) compute NREF if needed
C                        F -> start with GLAST, keep NREF as is
C      VOBS     R(2,*)   Averaged visibility (real, imaginary)
C      IS       I(*)     Array of first antenna numbers.
C      JS       I(*)     Array of second antenna numbers.
C      NUMBL    I        Number of observations.
C      REFANT   I        Desired reference antenna.
C      MODE     I        Solution mode: 0 = full gain soln.
C                           1 = phase only keep ampl. info.
C                           2 = phase only discard ampl. info.
C                           4-6 => 0-2 with robustness
C                        + 10 * weight func (1 sqrt, 2 4th root, 3 -> 1
C      MINNO    I        Min. number of antennas acceptable.
C      PRTLV    I        Print level,   0 = none
C                           1 = print (MSGWRT) soln.
C                           2 = print RMS at each iteration
C                           3 = print data plus soln.
C      FFLAST   R        Mark bad those data with closure error >
C                        FFLAST * rms closure error on last iteration
C   In/out:
C      WT       R(*)     Array of visibility weights - adjusted for
C                        amplitude normalizations and weight function
C   Outputs:
C      G        R(2,*)   Antenna gains to be applied (real, imaginary)
C      WORK     R(*)     Work array the size of VOBS
C      NREF     I        Reference antenna used.
C      IERR     I        Return error code 0 = OK
C                           1 = no valid data
C                           2 = didn't converge
C                           3 = too few antennas
C-----------------------------------------------------------------------
      LOGICAL   INIT
      INTEGER   IS(*), JS(*), NUMBL, REFANT, MODE, MINNO, NREF, PRTLV,
     *   IERR
      REAL      VOBS(2,*), WT(*), G(2,*)
C
      REAL      AMP, PH, XX1, XX2
      INTEGER   I, J, K, I1, I2, J1, J2, LMODE
      INCLUDE 'INCS:PUVD.INC'
      REAL      GLAST(2,MAXANT), SWT(MAXANT)
      INCLUDE 'INCS:DMSG.INC'
      COMMON /GLASTS/ GLAST
C-----------------------------------------------------------------------
      LMODE = MOD (MODE, 10)
      IF (LMODE.GT.4) LMODE = LMODE - 4
      IF (LMODE.EQ.2) LMODE = 1
      IERR = 0
C                                       find antenna list
      DO 10 I = 1,MAXANT
         SWT(I) = 0.0
 10      CONTINUE
      DO 20 K = 1,NUMBL
         I = IS(K)
         J = JS(K)
         SWT(I) = SWT(I) + WT(K)
         SWT(J) = SWT(J) + WT(K)
 20      CONTINUE
C                                       Initialize solutions
      DO 30 I = 1,MAXANT
         IF (INIT) THEN
            G(1,I) = 1.0
            G(2,I) = 0.0
            GLAST(1,I) = G(1,I)
            GLAST(2,I) = G(2,I)
         ELSE
            G(1,I) = GLAST(1,I)
            G(2,I) = GLAST(2,I)
            END IF
 30      CONTINUE
C                                       one single baseline
      IF (NUMBL.EQ.1) THEN
         IF (MINNO.GT.2) IERR = 3
         I1 = IS(1)
         J1 = JS(1)
         IF (NREF.EQ.0) NREF = REFANT
         IF ((NREF.NE.I1) .AND. (NREF.NE.J1)) NREF = I1
         XX1 = 1.0
         IF (LMODE.EQ.1) XX1 = SQRT (VOBS(1,1)*VOBS(1,1) +
     *      VOBS(2,1)*VOBS(2,1))
         IF (I1.EQ.NREF) THEN
            G(1,I1) = 1.0
            G(2,I1) = 0.0
            G(1,J1) = VOBS(1,1) / XX1
            G(2,J1) = -VOBS(2,1) / XX1
         ELSE
            G(1,J1) = 1.0
            G(2,J1) = 0.0
            G(1,I1) = VOBS(1,1) / XX1
            G(2,I1) = VOBS(2,1) / XX1
            END IF
C                                       exact solutions: 2 BASELINE
      ELSE IF (NUMBL.EQ.2) THEN
         IF (MINNO.GT.3) IERR = 3
         I1 = IS(1)
         J1 = JS(1)
         I2 = IS(2)
         J2 = JS(2)
         IF (NREF.EQ.0) NREF = REFANT
         IF ((NREF.NE.I1) .AND. (NREF.NE.I2) .AND. (NREF.NE.J1)
     *      .AND. (NREF.NE.J2)) NREF = I1
         XX1 = 1.0
         XX2 = 1.0
         IF (LMODE.EQ.1) XX1 = SQRT (VOBS(1,1)*VOBS(1,1) +
     *      VOBS(2,1)*VOBS(2,1))
         IF (LMODE.EQ.2) XX2 = SQRT (VOBS(1,2)*VOBS(1,2) +
     *      VOBS(2,2)*VOBS(2,2))
         IF (I1.EQ.NREF) THEN
            G(1,I1) = 1.0
            G(2,I1) = 0.0
            G(1,J1) = VOBS(1,1) / XX1
            G(2,J1) = -VOBS(2,1) / XX1
            IF (I2.EQ.NREF) THEN
               G(1,J2) = VOBS(1,2) / XX2
               G(2,J2) = -VOBS(2,2) / XX2
            ELSE IF (J2.EQ.NREF) THEN
               G(1,I2) = VOBS(1,2) / XX2
               G(2,I2) = VOBS(2,2) / XX2
            ELSE IF (I2.EQ.J1) THEN
               G(1,J2) = (VOBS(1,2)/XX2 * G(1,I2) +
     *            VOBS(2,2)/XX2 * G(2,I2)) /
     *            (G(1,I2)*G(1,I2) + G(2,I2)*G(2,I2))
               G(2,J2) = (VOBS(1,2)/XX2 * G(2,I2) -
     *            VOBS(2,2)/XX2 * G(1,I2)) /
     *            (G(1,I2)*G(1,I2) + G(2,I2)*G(2,I2))
            ELSE IF (J2.EQ.J1) THEN
               G(1,I2) = (VOBS(1,2)/XX2 * G(1,J2) -
     *            VOBS(2,2)/XX2 * G(2,J2)) /
     *            (G(1,J2)*G(1,J2) + G(2,J2)*G(2,J2))
               G(2,I2) = (VOBS(2,2)/XX2 * G(1,J2) +
     *            VOBS(1,2)/XX2 * G(2,J2)) /
     *            (G(1,J2)*G(1,J2) + G(2,J2)*G(2,J2))
            ELSE
               IERR = 2
               END IF
         ELSE IF (J1.EQ.NREF) THEN
            G(1,J1) = 1.0
            G(2,J1) = 0.0
            G(1,I1) = VOBS(1,1) / XX1
            G(2,I1) = VOBS(2,1) / XX1
            IF (I2.EQ.NREF) THEN
               G(1,J2) = VOBS(1,2) / XX2
               G(2,J2) = -VOBS(2,2) / XX2
            ELSE IF (J2.EQ.NREF) THEN
               G(1,I2) = VOBS(1,2) / XX2
               G(2,I2) = VOBS(2,2) / XX2
            ELSE IF (I2.EQ.I1) THEN
               G(1,J2) = (VOBS(1,2)/XX2 * G(1,I2) +
     *            VOBS(2,2)/XX2 * G(2,I2)) /
     *            (G(1,I2)*G(1,I2) + G(2,I2)*G(2,I2))
               G(2,J2) = (VOBS(1,2)/XX2 * G(2,I2) -
     *            VOBS(2,2)/XX2 * G(1,I2)) /
     *            (G(1,I2)*G(1,I2) + G(2,I2)*G(2,I2))
            ELSE IF (J2.EQ.I1) THEN
               G(1,I2) = (VOBS(1,2)/XX2 * G(1,J2) -
     *            VOBS(2,2)/XX2 * G(2,J2)) /
     *            (G(1,J2)*G(1,J2) + G(2,J2)*G(2,J2))
               G(2,I2) = (VOBS(2,2)/XX2 * G(1,J2) +
     *            VOBS(1,2)/XX2 * G(2,J2)) /
     *            (G(1,J2)*G(1,J2) + G(2,J2)*G(2,J2))
            ELSE
               IERR = 2
               END IF
         ELSE IF (I2.EQ.NREF) THEN
            G(1,I2) = 1.0
            G(2,I2) = 0.0
            G(1,J2) = VOBS(1,2) / XX2
            G(2,J2) = -VOBS(2,2) / XX2
            IF (I1.EQ.J2) THEN
               G(1,J1) = (VOBS(1,1)/XX1 * G(1,I1) +
     *            VOBS(2,1)/XX1 * G(2,I1)) /
     *            (G(1,I2)*G(1,I2) + G(2,I2)*G(2,I2))
               G(2,J1) = (VOBS(1,2)/XX2 * G(2,I1) -
     *            VOBS(2,1)/XX1 * G(1,I1)) /
     *            (G(1,I2)*G(1,I2) + G(2,I2)*G(2,I2))
            ELSE IF (J1.EQ.J2) THEN
               G(1,I1) = (VOBS(1,1)/XX1 * G(1,J1) -
     *            VOBS(2,1)/XX1 * G(2,J1)) /
     *            (G(1,J2)*G(1,J2) + G(2,J2)*G(2,J2))
               G(2,I1) = (VOBS(2,1)/XX1 * G(1,J1) +
     *            VOBS(1,1)/XX1 * G(2,J1)) /
     *            (G(1,J2)*G(1,J2) + G(2,J2)*G(2,J2))
            ELSE
               IERR = 2
               END IF
         ELSE IF (J2.EQ.NREF) THEN
            G(1,J2) = 1.0
            G(2,J2) = 0.0
            G(1,I2) = VOBS(1,2) / XX2
            G(2,I2) = VOBS(2,2) / XX2
            IF (I1.EQ.I2) THEN
               G(1,J1) = (VOBS(1,1)/XX1 * G(1,I1) +
     *            VOBS(2,1)/XX1 * G(2,I1)) /
     *            (G(1,I2)*G(1,I2) + G(2,I2)*G(2,I2))
               G(2,J1) = (VOBS(1,2)/XX2 * G(2,I1) -
     *            VOBS(2,1)/XX1 * G(1,I1)) /
     *            (G(1,I2)*G(1,I2) + G(2,I2)*G(2,I2))
            ELSE IF (J1.EQ.I2) THEN
               G(1,I1) = (VOBS(1,1)/XX1 * G(1,J1) -
     *            VOBS(2,1)/XX1 * G(2,J1)) /
     *            (G(1,J2)*G(1,J2) + G(2,J2)*G(2,J2))
               G(2,I1) = (VOBS(2,1)/XX1 * G(1,J1) +
     *            VOBS(1,1)/XX1 * G(2,J1)) /
     *            (G(1,J2)*G(1,J2) + G(2,J2)*G(2,J2))
            ELSE
               IERR = 2
               END IF
            END IF
         END IF
C                                       squirrel and print away
      IF (IERR.EQ.0) THEN
C                                       Print data if nec.
         IF (PRTLV.GE.3) THEN
            DO 100 K = 1,NUMBL
               AMP = SQRT (VOBS(1,K)*VOBS(1,K) + VOBS(2,K)*VOBS(2,K))
               PH = 57.296 * ATAN2 (VOBS(2,K), VOBS(1,K)+1.0E-20)
               WRITE (MSGTXT,1000) K, AMP, PH, IS(K), JS(K), WT(K)
               CALL MSGWRT (3)
 100           CONTINUE
            END IF
C                                       Print results.
         IF (PRTLV.GE.1) THEN
            DO 110 I = 1,MAXANT
               IF (SWT(I).GT.1.0E-20) THEN
                  AMP = SQRT (G(1,I)*G(1,I) + G(2,I)*G(2,I))
                  PH = 57.2958 * ATAN2 (G(2,I), G(1,I))
                  WRITE (MSGTXT,1420) I, AMP, PH
                  CALL MSGWRT (3)
                  END IF
 110           CONTINUE
            END IF
C                                       insure phase only
         IF (LMODE.EQ.1) THEN
            DO 120 I = 1,MAXANT
               IF (SWT(I).GT.1.0E-20) THEN
                  AMP = SQRT (G(1,I)*G(1,I) + G(2,I)*G(2,I))
                  IF (AMP.LE.0.0) AMP = 1.0
                  G(1,I) = G(1,I) / AMP
                  G(2,I) = G(2,I) / AMP
                  END IF
 120           CONTINUE
            END IF
C                                       save answers
         GLAST(1,I1) = G(1,I1)
         GLAST(2,I1) = G(2,I1)
         GLAST(1,J1) = G(1,J1)
         GLAST(2,J1) = G(2,J1)
         IF (NUMBL.EQ.2) THEN
            GLAST(1,I2) = G(1,I2)
            GLAST(2,I2) = G(2,I2)
            GLAST(1,J2) = G(1,J2)
            GLAST(2,J2) = G(2,J2)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I4,' Amp =',1PE12.3,' Phase =',0PF7.1,2I3,1PE12.3)
 1420 FORMAT ('Ant=',I5,' Amp=',F12.5,' Phase=',F12.2)
      END
