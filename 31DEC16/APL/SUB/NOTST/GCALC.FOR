      SUBROUTINE GCALC (INIT, VOBS, IS, JS, WT, NUMBL, REFANT, MODE,
     *   MINNO, G, NREF, WORK, PRTLV, FFLIM, FFLAST, FRAC, RMS, IERR)
C-----------------------------------------------------------------------
C! Gain Soln: Compute least squares gains.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004-2005
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
C   GCALC computes antenna gains given visibilities divided by the
C   model values.
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
C      FFLIM    R        Limit clip to max (std, FFLIM) * rms
C      FFLAST   R        Mark bad those data with closure error >
C                        FFLAST * rms closure error on last iteration
C   In/out:
C      WT       R(*)     Array of visibility weights - adjusted for
C                        amplitude normalizations and weight function
C   Outputs:
C      G        R(2,*)   Antenna gains to be applied (real, imaginary)
C                        returnsd 1,0 for missing antennas
C      WORK     R(*)     Work array the size of VOBS
C      NREF     I        Reference antenna used.
C      FRAC     R        Fraction of data excluded in robust solution
C      RMS      R        RMS closure error
C      IERR     I        Return error code 0 = OK
C                           1 = no valid data
C                           2 = didn't converge
C                           3 = too few antennas
C-----------------------------------------------------------------------
      LOGICAL   INIT
      INTEGER   IS(*), JS(*), NUMBL, REFANT, MODE, MINNO, NREF, PRTLV,
     *   IERR
      REAL      VOBS(2,*), WT(*), G(2,*), WORK(2,*), FFLIM, FFLAST,
     *   FRAC, RMS
C
      INTEGER   ITRMAX
      PARAMETER (ITRMAX = 10)
C
      REAL      GNG(2), Z(2), ZR, ZI, AMP, GD, PH, QQ, S, SUMWT, TOL, W,
     *   X, XX, XXX, YY, ARG, XFAIL, XFAIL1, XFAIL2, FF(ITRMAX),
     *   GG(ITRMAX)
      INTEGER   I, II, IT, J, JJ, K, NIT, NT, NTD, MAXANS, ITMAX,
     *   NFAIL, LIMIT, LOOP, LMODE, LWT, ITRMIN, ITER
      LOGICAL   CONVGD, T, F, DORMS
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANDX1(MAXANT,MAXANT), ANDX2(MAXANT,MAXANT),
     *   ACNT1(MAXANT), ACNT2(MAXANT)
      REAL      GN(2,MAXANT), GLAST(2,MAXANT), SWT(MAXANT), TEMP2,
     *   TEMP3, LGLAST(2,MAXANT)
      INCLUDE 'INCS:DMSG.INC'
      COMMON /GLASTS/ GLAST
      DATA T, F /.TRUE.,.FALSE./
      DATA GG /7.0, 5.0, 4.0, 3.5, 3.0, 2.8, 2.6, 2.4, 2.2, 2.5/
C-----------------------------------------------------------------------
      IERR = 1
      IF (NUMBL.LE.0) GO TO 999
      CALL RCOPY (ITRMAX, GG, FF)
      DO 5 I = 1,ITRMAX
         FF(I) = MAX (FF(I), FFLIM)
 5       CONTINUE
      IF (FFLAST.GE.1.0) FF(ITRMAX) = FFLAST
C                                       interpret mode
      LMODE = MOD (MODE, 10)
      ITRMIN = ITRMAX
      IF (LMODE.GT.3) THEN
         ITRMIN = 1
         LMODE = LMODE - 4
         END IF
      LWT = MODE / 10
C                                       inits
      MAXANS = MAXANT
      ITMAX = 60
      IF (NUMBL.EQ.1) ITMAX = 1
      TOL = 5.0E-6
      NT = 0
      NTD = 0
      SUMWT = 0.0
      DO 10 I = 1,MAXANS
         SWT(I) = 0.0
         ACNT1(I) = 0
         ACNT2(I) = 0
 10      CONTINUE
C                                       Find which antennae have data
C                                       and normalize to unit amplitude
C                                       if requested.
      DO 20 K = 1,NUMBL
         I = IS(K)
         J = JS(K)
         NT = MAX (NT, J)
         XXX = SQRT ((VOBS(1,K)*VOBS(1,K)) + (VOBS(2,K)*VOBS(2,K)))
         IF (XXX.LE.1.0E-20) WT(K) = 0.
         IF (WT(K).GT.1.0E-20) THEN
            IF (LMODE.EQ.2) THEN
               VOBS(1,K) = VOBS(1,K) / XXX
               VOBS(2,K) = VOBS(2,K) / XXX
               WT(K) = WT(K) * XXX * XXX
               END IF
            CALL REWAIT (LWT, WT(K))
            SWT(I) = SWT(I) + WT(K)
            SWT(J) = SWT(J) + WT(K)
            SUMWT = SUMWT + WT(K)
C                                       Save weighted data in WORK
            WORK(1,K) = WT(K) * VOBS(1,K)
            WORK(2,K) = WT(K) * VOBS(2,K)
C                                       Antenna index tables
            ACNT1(I) = ACNT1(I) + 1
            II = ACNT1(I)
            ANDX1(II,I) = K
            ACNT2(J) = ACNT2(J) + 1
            JJ = ACNT2(J)
            ANDX2(JJ,J) = K
         ELSE
            WT(K) = 0.0
            WORK(1,K) = 0.0
            WORK(2,K) = 0.0
            END IF
 20      CONTINUE
C                                       No data
      IF (SUMWT.LE.1.0E-20) GO TO 999
C                                       Print data if nec.
      IF (PRTLV.GE.3) THEN
         DO 30 K = 1,NUMBL
            IF (WT(K).GT.1.0E-20) THEN
               AMP = SQRT (VOBS(1,K)*VOBS(1,K) + VOBS(2,K)*VOBS(2,K))
               PH = 57.296 * ATAN2 (VOBS(2,K), VOBS(1,K)+1.0E-20)
               WRITE (MSGTXT,1000) K, AMP, PH, IS(K), JS(K), WT(K)
               CALL MSGWRT (3)
               END IF
 30         CONTINUE
         END IF
C                                       Initialize solutions
      DO 40 I = 1,MAXANS
         IF ((INIT) .OR. (SWT(I).LE.0.0)) THEN
            G(1,I) = 1.0
            G(2,I) = 0.0
            GLAST(1,I) = G(1,I)
            GLAST(2,I) = G(2,I)
         ELSE
            G(1,I) = GLAST(1,I)
            G(2,I) = GLAST(2,I)
            END IF
         LGLAST(1,I) = 1.0
         LGLAST(2,I) = 0.0
         IF (SWT(I).GT.1.0E-20) NTD = NTD + 1
 40      CONTINUE
      DORMS = (2.5*(NTD-1).LE.NUMBL)
C                                       Pick reference ant.
      IF ((INIT) .OR. (NREF.LE.0) .OR. (NREF.GT.MAXANS)) THEN
         NREF = REFANT
         IF ((NREF.LT.1) .OR. (NREF.GT.MAXANS) .OR. (SWT(NREF).LE.0.))
     *      THEN
            XX = 0.0
            DO 50 I = 1,NT
               IF (SWT(I).GT.XX) THEN
                  XX = SWT(I)
                  NREF = I
                  END IF
 50            CONTINUE
            END IF
         END IF
C                                       Robust iteration
 100  IF (.NOT.DORMS) ITRMIN = ITRMAX
      DO 400 ITER = ITRMIN,ITRMAX
C                                       Too few antennas
         IF (NTD.LT.MINNO) THEN
            IERR = 3
            GO TO 999
            END IF
C                                       Print statistics
         IF (PRTLV.GE.2) THEN
C                                       Sum chi squares
            S = 0.0
            XXX = 1.0
            DO 110 K = 1,NUMBL
               IF (WT(K).GT.0.0) THEN
                  II = IS(K)
                  JJ = JS(K)
                  Z(1) = G(1,JJ)
                  Z(2) = - G(2,JJ)
                  ZR = G(1,II) * Z(1) - G(2,II) * Z(2)
                  ZI = G(1,II) * Z(2) + G(2,II) * Z(1)
                  IF (LMODE.EQ.1) XXX = SQRT ((VOBS(1,K)*VOBS(1,K)) +
     *               (VOBS(2,K)*VOBS(2,K)))
                  Z(1) = VOBS(1,K)/XXX - ZR
                  Z(2) = VOBS(2,K)/XXX - ZI
                  QQ = Z(1) * Z(1) + Z(2) * Z(2)
                  S = S + WT(K) * QQ
                  END IF
 110           CONTINUE
            RMS = SQRT (S/SUMWT)
            IT = 0
            WRITE (MSGTXT,1110) IT, S, RMS
            CALL MSGWRT (3)
            END IF
C                                       Begin solution iteration
         DO 290 IT = 1,ITMAX
            IF (IT.GT.15) TOL = (IT - 10) * 1.0E-6
            NIT = IT
C                                       Following for amplitude and
C                                       phase solution.
            IF (LMODE.LE.0) THEN
               IF (IT.EQ.1) W = .5
               IF (IT.EQ.2) W = .75
               IF (IT.GT.2) W = .9
               IF (IT.GT.10) W = .5
               IF (NTD.LE.6) W = .25
               DO 150 I = 1,NT
                  IF (SWT(I).EQ.0.) GO TO 150
                  GNG(1) = 0.
                  GNG(2) = 0.
                  GD = 0.
C                                       I is first antenna
                  LIMIT = ACNT1(I)
                  DO 120 LOOP = 1,LIMIT
                     K = ANDX1(LOOP,I)
                     IF (WT(K).GT.0.0) THEN
                        JJ = JS(K)
                        Z(1) = G(1,JJ)
                        Z(2) = G(2,JJ)
                        QQ = Z(1) * Z(1) + Z(2) * Z(2)
                        GD = GD + WT(K) * QQ
                        GNG(1) = GNG(1) +
     *                     (G(1,JJ) * WORK(1,K) - G(2,JJ) * WORK(2,K))
                        GNG(2) = GNG(2) +
     *                     (G(1,JJ) * WORK(2,K) + G(2,JJ) * WORK(1,K))
                        END IF
 120                 CONTINUE
C                                       I is second antenna:
                  LIMIT = ACNT2(I)
                  DO 130 LOOP = 1,LIMIT
                     K = ANDX2(LOOP,I)
                     IF (WT(K).GT.0.0) THEN
                        II = IS(K)
                        Z(1) = G(1,II)
                        Z(2) = G(2,II)
                        QQ = Z(1) * Z(1) + Z(2) * Z(2)
                        GD = GD + WT(K) * QQ
                        GNG(1) = GNG(1) +
     *                     (G(1,II) * WORK(1,K) + G(2,II) * WORK(2,K))
                        GNG(2) = GNG(2) +
     *                     (G(2,II) * WORK(1,K) - G(1,II) * WORK(2,K))
                        END IF
 130                 CONTINUE
                  G(1,I) = G(1,I) + W * (GNG(1) / GD - G(1,I))
                  G(2,I) = G(2,I) + W * (GNG(2) / GD - G(2,I))
 150              CONTINUE
C                                       Following for phase only.
            ELSE
               DO 210 I = 1,NT
                  GN(1,I) = 0.0
                  GN(2,I) = 0.0
 210              CONTINUE
               W = .8
               IF (NTD.LE.6) W = .25
C                                       do sums
               DO 220 K = 1,NUMBL
C                                       Z = G(II) * CONJG (G(JJ)) *
C                                           CONJG (WORK(K))
                  IF (WT(K).GT.0.0) THEN
                     II = IS(K)
                     JJ = JS(K)
                     ZR = G(1,II) * G(1,JJ) + G(2,II) * G(2,JJ)
                     ZI = G(2,II) * G(1,JJ) - G(1,II) * G(2,JJ)
                     TEMP2 = ZR * WORK(1,K) + ZI * WORK(2,K)
                     TEMP3 = ZI * WORK(1,K) - ZR * WORK(2,K)
C                                       GN(II) = GN(II) + Z
                     GN(1,II) = GN(1,II) + TEMP2
                     GN(2,II) = GN(2,II) + TEMP3
C                                       GN(JJ) = GN(JJ) + CONJG (Z)
                     GN(1,JJ) = GN(1,JJ) + TEMP2
                     GN(2,JJ) = GN(2,JJ) - TEMP3
                     END IF
 220              CONTINUE
C                                       Update solutions
               DO 230 I = 1,NT
                  IF (SWT(I).GT.0.0) THEN
                     XX = ATAN2 (G(2,I), G(1,I))
                     YY = ATAN2 (GN(2,I), GN(1,I)+1.0E-30)
                     ARG = XX - YY
                     G(1,I) = COS (ARG)
                     G(2,I) = SIN (ARG)
                     END IF
 230              CONTINUE
               END IF
C                                       Convergence test
            CONVGD = T
            NFAIL = 0
            XFAIL = 0.0
            DO 250 I = 1,NT
               IF (SWT(I).GT.0.) THEN
                  IF (IT.LE.30) THEN
                     IF (CONVGD) THEN
                        X = TOL * (5.0E-7 + SQRT (G(1,I)*G(1,I) +
     *                     G(2,I)*G(2,I)))
                        IF (ABS (G(1,I)-LGLAST(1,I)).GT.X) CONVGD = F
                        IF (ABS (G(2,I)-LGLAST(2,I)).GT.X) CONVGD = F
                        END IF
C                                       Use different criteria after
C                                       30 iterations
                  ELSE
                     XFAIL1 = ABS (G(1,I) - LGLAST(1,I))
                     XFAIL2 = ABS (G(2,I) - LGLAST(2,I))
                     XFAIL1 = MAX (XFAIL1, XFAIL2)
                     X = TOL * (5.0E-7 + SQRT (G(1,I)*G(1,I) +
     *                  G(2,I)*G(2,I)))
                     IF (XFAIL1.GT.X) THEN
                        CONVGD = F
                        NFAIL = NFAIL + 1
                        IF ((XFAIL1/X).GE.XFAIL) XFAIL = XFAIL1 / X
                        END IF
                     END IF
                  END IF
               LGLAST(1,I) = G(1,I)
               LGLAST(2,I) = G(2,I)
 250           CONTINUE
C                                       Find statistics
            S = 0.0
            XXX = 1.0
            DO 260 K = 1,NUMBL
               IF (WT(K).GT.0.0) THEN
                  II = IS(K)
                  JJ = JS(K)
                  Z(1) = G(1,JJ)
                  Z(2) = - G(2,JJ)
                  ZR = G(1,II) * Z(1) - G(2,II) * Z(2)
                  ZI = G(1,II) * Z(2) + G(2,II) * Z(1)
                  IF (LMODE.EQ.1) XXX = SQRT ((VOBS(1,K)*VOBS(1,K)) +
     *               (VOBS(2,K)*VOBS(2,K)))
                  Z(1) = VOBS(1,K)/XXX - ZR
                  Z(2) = VOBS(2,K)/XXX - ZI
                  QQ = Z(1) * Z(1) + Z(2) * Z(2)
                  S = S + WT(K) * QQ
                  END IF
 260           CONTINUE
            RMS = SQRT (S/SUMWT)
            IF (PRTLV.GE.2) THEN
               WRITE (MSGTXT,1110) IT, S, RMS
               CALL MSGWRT (3)
               END IF
            IF (CONVGD) GO TO 300
            IF ((IT.GT.30) .AND. (NFAIL.LE.1) .AND. (XFAIL.LE.10.))
     *         GO TO 300
C                                       End of iteration loop
 290        CONTINUE
C                                       Didn't converge
         IERR = 2
         GO TO 999
C                                       converged: what to use this time
 300     SUMWT = 0.0
         CALL RFILL (MAXANS, 0.0, SWT)
         XXX = 1.0
         IF (.NOT.DORMS) RMS = 1.0E10
         DO 310 K = 1,NUMBL
            II = IS(K)
            JJ = JS(K)
            IF (WT(K).NE.0.0) THEN
               WT(K) = ABS (WT(K))
               Z(1) = G(1,JJ)
               Z(2) = - G(2,JJ)
               ZR = G(1,II) * Z(1) - G(2,II) * Z(2)
               ZI = G(1,II) * Z(2) + G(2,II) * Z(1)
               IF (LMODE.EQ.1) XXX = SQRT ((VOBS(1,K)*VOBS(1,K)) +
     *            (VOBS(2,K)*VOBS(2,K)))
               Z(1) = ABS (VOBS(1,K)/XXX - ZR) / RMS
               Z(2) = ABS (VOBS(2,K)/XXX - ZI) / RMS
               IF (((Z(1).GT.FF(ITER)) .OR. (Z(2).GT.FF(ITER))) .AND.
     *            (DORMS)) THEN
                  WT(K) = -WT(K)
               ELSE
                  SWT(II) = SWT(II) + WT(K)
                  SWT(JJ) = SWT(JJ) + WT(K)
                  SUMWT = SUMWT + WT(K)
                  END IF
               END IF
 310        CONTINUE
 400     CONTINUE
      IF (.NOT.DORMS) RMS = 0.0
      IERR = 0
C                                       fraction excluded
      XX = 0.0
      YY = 0.0
      DO 410 K = 1,NUMBL
         IF (WT(K).NE.0.0) THEN
            XX = XX + 1.0
            IF (WT(K).LT.0.0) YY = YY + 1.0
            END IF
 410     CONTINUE
      FRAC = 1.0
      IF (XX.GT.0.0) FRAC = YY / XX
C                                       Refer solutions to reference
C                                       antenna.
      XXX = 1.0 / SQRT (G(1,NREF)*G(1,NREF) + G(2,NREF)*G(2,NREF))
      DO 420 I = 1,NT
         IF ((I.NE.NREF) .AND. (SWT(I).GT.1.0E-20)) THEN
            ZR = G(1,I) * G(1,NREF) + G(2,I) * G(2,NREF)
            ZI = G(2,I) * G(1,NREF) - G(1,I) * G(2,NREF)
            G(1,I) = XXX * ZR
            G(2,I) = XXX * ZI
            GLAST(1,I) = G(1,I)
            GLAST(2,I) = G(2,I)
            END IF
 420     CONTINUE
      G(1,NREF) = SQRT (G(1,NREF)*G(1,NREF) + G(2,NREF)*G(2,NREF))
      G(2,NREF) = 0.0
      GLAST(1,NREF) = G(1,NREF)
      GLAST(2,NREF) = G(2,NREF)
C                                       Print results.
      IF (PRTLV.GE.1) THEN
         DO 430 I = 1,NT
            IF (SWT(I).GT.1.0E-20) THEN
               AMP = SQRT (G(1,I)*G(1,I) + G(2,I)*G(2,I))
               PH = 57.2958 * ATAN2 (G(2,I), G(1,I))
               WRITE (MSGTXT,1420) I, AMP, PH
               CALL MSGWRT (3)
               END IF
 430        CONTINUE
         END IF
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I4,' Amp =',1PE12.3,' Phase =',0PF7.1,2I3,1PE12.3)
 1110 FORMAT ('Iter=',I5,' s=',1PE15.5,' RMS=',1E15.5)
 1420 FORMAT ('Ant=',I5,' Amp=',F12.5,' Phase=',F12.2)
      END
