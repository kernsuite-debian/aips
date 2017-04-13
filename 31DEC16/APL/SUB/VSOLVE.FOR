      SUBROUTINE VSOLVE (VOBS, WT, CANT, REFANT, MODE, MINNO, FFLAST,
     *   CREAL, CIMAG, NREF, FRACT, WORK, PRTLV, IERR)
C-----------------------------------------------------------------------
C! Finds antenna-based amp/phase from baseline vis data ROBUSTLY
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
C   VSOLVE is a version of GCALC to computes antenna amplitudes and
C   phases.
C   Inputs:
C      VOBS     R(*,*)   Averaged visibility
C                          real in VOBS(i,j) with i < j
C                          imag in VOBS(j,i) with i < j
C      CANT     I        Number of antennas
C      REFANT   I        Desired reference antenna.
C      MODE     I        Solution mode: 0 = full gain soln.
C                           1 = phase only keep ampl. info.
C                           2 = phase only discard ampl. info.
C                        + 10 * weight func (1 sqrt, 2 4th root, 3 -> 1
C      MINNO    I        Min. number of antennas acceptable.
C      FFLAST   R        Mark bad those data with closure error >
C                        FFLAST * rms closure error on last iteration
C      PRTLV    I        Print level,   0 = none
C                           1 = print (MSGWRT) soln.
C                           2 = print data plus soln.
C   In/out:
C      WT       R(*)     Array of visibility weights
C                        In: squared weights
C                        Out: adjusted for amplitude normalizations
C                              and weight function
C   Outputs:
C      CREAL    R(*)     Antenna vis real part
C      CIMAG    R(*)     Antenna vis imag part
C                           Both 0.0 => failed but there was data
C                           Both FBLANK => no data
C      WORK     R(*)     Work array the size of VOBS
C      NREF     I        Reference antenna used.
C      IERR     I        Return error code 0 = OK
C                           1 = no valid data
C                           2 = didn't converge
C                           3 = too few antennas
C-----------------------------------------------------------------------
      INTEGER   CANT, REFANT, MODE, MINNO, NREF, PRTLV, IERR
      REAL      VOBS(CANT,*), WT(CANT,*), FFLAST, CREAL(*), CIMAG(*),
     *   FRACT(*), WORK(2,*)
C
      INTEGER   ITRMAX
      PARAMETER (ITRMAX = 10)
C
      REAL      GNG(2), Z(2), ZR, ZI, AMP, GD, PH, QQ, RMS, S, SUMWT,
     *   TOL, W, X, XX, XXX, YY, WX, ARG, XFAIL, XFAIL1, XFAIL2,
     *   FF(ITRMAX), FB(ITRMAX), RMSMIN
      INTEGER   I, II, IT, J, JJ, K, NF, NIT, NTD, IOFF, ITMAX, NFAIL,
     *   IFAIL, LIMIT, LOOP, LMODE, LWT, ITER
      LOGICAL   CONVGD, T, F
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANDX1(MAXANT,MAXANT), ANDX2(MAXANT,MAXANT),
     *   ACNT1(MAXANT), ACNT2(MAXANT), IS(MXBASE), JS(MXBASE)
      REAL      GN(2,MAXANT), GLAST(2,MAXANT), SWT(MAXANT), INW(MAXANT),
     *   TEMP(MAXANT,3), TEMP2(MXBASE), TEMP3(MXBASE), G(2,MAXANT)
      INCLUDE 'INCS:GAIN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (GN, GRAD), (GLAST, STEP), (SWT, GWORK)
      EQUIVALENCE (ANDX1, TEMP2, SHESS),
     *   (ANDX2, TEMP3, SHESS(1,MAXPRM/2))
      DATA T, F /.TRUE.,.FALSE./
      DATA FB /8.0, 6.0, 5.0, 4.0, 3.5, 3.0, 2.7, 2.45, 2.2, 2.5/
C-----------------------------------------------------------------------
      CALL RFILL (CANT, FBLANK, CREAL)
      CALL RFILL (CANT, FBLANK, CIMAG)
      CALL RCOPY (ITRMAX, FB, FF)
      IF (FFLAST.GT.1.0) FF(ITRMAX) = FFLAST
      LMODE = MOD (MODE, 10)
      LWT = MODE / 10
      ITER = 0
      ITMAX = 60
      TOL = 5.0E-6
      NTD = 0
      SUMWT = 0.0
      DO 10 I = 1,CANT
         SWT(I) = 0.0
         ACNT1(I) = 0
         ACNT2(I) = 0
 10      CONTINUE
C                                       Print data if nec.
      IF (PRTLV.GE.5) THEN
         DO 20 I = 1,CANT-1
            DO 15 J = I+1,CANT
               IF ((WT(I,J).GT.1.0E-20) .AND. (WT(I,J).NE.FBLANK)) THEN
                  AMP = SQRT (VOBS(I,J)*VOBS(I,J) + VOBS(J,I)*VOBS(J,I))
                  PH = 57.296 * ATAN2 (VOBS(J,I), VOBS(I,J)+1.0E-20)
                  WRITE (MSGTXT,1000) I, J, AMP, PH, SQRT (WT(I,J))
                  CALL MSGWRT (3)
                  END IF
 15            CONTINUE
 20         CONTINUE
         END IF
C                                       Find which antennae have data
C                                       and normalize to unit amplitude
C                                       if requested.
      K = 0
      DO 40 I = 1,CANT-1
         DO 35 J = I+1,CANT
            K = K + 1
            IS(K) = I
            JS(K) = J
            AMP = SQRT ((VOBS(I,J)*VOBS(I,J)) + (VOBS(J,I)*VOBS(J,I)))
            IF (AMP.LE.1.0E-20) WT(I,J) = 0.
            WORK(1,K) = 0.0
            WORK(2,K) = 0.0
            IF (WT(I,J).EQ.FBLANK) THEN
               WT(I,J) = 0.0
            ELSE IF (WT(I,J).LT.0.0) THEN
               WT(I,J) = 0.0
            ELSE IF (WT(I,J).GT.0.0) THEN
               WT(I,J) = SQRT (WT(I,J))
               IF (LMODE.EQ.2) THEN
                  VOBS(I,J) = VOBS(I,J) / AMP
                  VOBS(J,I) = VOBS(J,I) / AMP
                  WT(I,J) = WT(I,J) * AMP * AMP
                  END IF
               CALL REWAIT (LWT, WT(I,J))
               SWT(I) = SWT(I) + WT(I,J)
               SWT(J) = SWT(J) + WT(I,J)
               SUMWT = SUMWT + WT(I,J)
C                                       Save weighted data in WORK
               WORK(1,K) = WT(I,J) * VOBS(I,J)
               WORK(2,K) = WT(I,J) * VOBS(J,I)
C                                       Antenna index tables
               ACNT1(I) = ACNT1(I) + 1
               II = ACNT1(I)
               ANDX1(II,I) = K
               ACNT2(J) = ACNT2(J) + 1
               JJ = ACNT2(J)
               ANDX2(JJ,J) = K
               END IF
            WT(J,I) = WT(I,J)
 35         CONTINUE
 40      CONTINUE
C                                       Initialize solutions
      DO 50 I = 1,CANT
         G(1,I) = 1.0
         G(2,I) = 0.0
         GLAST(1,I) = 1.0
         GLAST(2,I) = 0.0
         IF (SWT(I).GT.1.0E-20) NTD = NTD + 1
         INW(I) = SWT(I)
 50      CONTINUE
C                                       No data
      IF (SUMWT.LE.1.0E-20) THEN
         IERR = 1
         GO TO 999
         END IF
C                                       Pick reference ant.
      NREF = REFANT
      IF ((NREF.LT.1) .OR. (NREF.GT.CANT) .OR. (SWT(NREF).LE.0.)) THEN
         XX = 0.0
         DO 90 I = 1,CANT
            IF (SWT(I).GT.XX) THEN
               XX = SWT(I)
               NREF = I
               END IF
 90         CONTINUE
         END IF
 100  DO 400 ITER = 1,ITRMAX
C                                       Too few antennas
         IF (NTD.LT.MINNO) THEN
            IERR = 3
            GO TO 999
            END IF
         NF = NTD - 1
C                                       Print statistics
         IF (PRTLV.GE.5) THEN
C                                       Sum chi squares
            S = 0.0
            DO 110 I = 1,CANT-1
               DO 105 J = I+1,CANT
                  IF (WT(I,J).GT.0.0) THEN
                     Z(1) = G(1,J)
                     Z(2) = - G(2,J)
                     ZR = G(1,I) * Z(1) - G(2,I) * Z(2)
                     ZI = G(1,I) * Z(2) + G(2,I) * Z(1)
                     Z(1) = VOBS(I,J) - ZR
                     Z(2) = VOBS(J,I) - ZI
                     QQ = Z(1) * Z(1) + Z(2) * Z(2)
                     S = S + WT(I,J) * QQ
                     END IF
 105              CONTINUE
 110           CONTINUE
            RMS = SQRT (S/SUMWT)
            IT = 0
            WRITE (MSGTXT,1001) IT, S, RMS
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
               DO 150 I = 1,CANT
                  IF (SWT(I).EQ.0.) GO TO 150
C                                       I is first antenna
                  LIMIT = ACNT1(I)
                  DO 120 LOOP = 1,LIMIT
                     K = ANDX1(LOOP,I)
                     JJ = JS(K)
                     IF (WT(I,JJ).GT.0.0) THEN
                        Z(1) = G(1,JJ)
                        Z(2) = G(2,JJ)
                        QQ = Z(1) * Z(1) + Z(2) * Z(2)
                        TEMP(LOOP,1) = WT(I,JJ) * QQ
                        TEMP(LOOP,2) =
     *                     (G(1,JJ) * WORK(1,K) - G(2,JJ) * WORK(2,K))
                        TEMP(LOOP,3) =
     *                     (G(1,JJ) * WORK(2,K) + G(2,JJ) * WORK(1,K))
                     ELSE
                        TEMP(LOOP,1) = 0.0
                        TEMP(LOOP,2) = 0.0
                        TEMP(LOOP,3) = 0.0
                        END IF
 120                 CONTINUE
C                                       I is second antenna:
                  LIMIT = ACNT2(I)
                  IOFF = ACNT1(I)
                  DO 130 LOOP = 1,LIMIT
                     K = ANDX2(LOOP,I)
                     II = IS(K)
                     IF (WT(II,I).GT.0.0) THEN
                        Z(1) = G(1,II)
                        Z(2) = G(2,II)
                        QQ = Z(1) * Z(1) + Z(2) * Z(2)
                        TEMP(IOFF+LOOP,1) = WT(II,I) * QQ
                        TEMP(IOFF+LOOP,2) =
     *                     (G(1,II) * WORK(1,K) + G(2,II) * WORK(2,K))
                        TEMP(IOFF+LOOP,3) =
     *                     (G(2,II) * WORK(1,K) - G(1,II) * WORK(2,K))
                     ELSE
                        TEMP(IOFF+LOOP,1) = 0.0
                        TEMP(IOFF+LOOP,2) = 0.0
                        TEMP(IOFF+LOOP,3) = 0.0
                        END IF
 130                 CONTINUE
                  GNG(1) = 0.
                  GNG(2) = 0.
                  GD = 0.
                  LIMIT = ACNT1(I) + ACNT2(I)
C                                       Do sums
                  DO 140 LOOP = 1,LIMIT
                     GD = GD + TEMP(LOOP,1)
                     GNG(1) = GNG(1) + TEMP(LOOP,2)
                     GNG(2) = GNG(2) + TEMP(LOOP,3)
 140                 CONTINUE
                  G(1,I) = G(1,I) + W * (GNG(1) / GD - G(1,I))
                  G(2,I) = G(2,I) + W * (GNG(2) / GD - G(2,I))
 150              CONTINUE
C                                       Following for phase only.
            ELSE
               DO 210 I = 1,CANT
                  GN(1,I) = 0.0
                  GN(2,I) = 0.0
 210              CONTINUE
               W = .8
               IF (NTD.LE.6) W = .25
               K = 0
               DO 220 I = 1,CANT-1
                  DO 215 J = I+1,CANT
                     K = K + 1
C                                       Z = G(I) * CONJG (G(J)) *
C                                           CONJG (WORK(K))
                     IF (WT(I,J).GT.0.0) THEN
                        ZR = G(1,I) * G(1,J) + G(2,I) * G(2,J)
                        ZI = G(2,I) * G(1,J) - G(1,I) * G(2,J)
                        TEMP2(K) = ZR * WORK(1,K) + ZI * WORK(2,K)
                        TEMP3(K) = ZI * WORK(1,K) - ZR * WORK(2,K)
                     ELSE
                        TEMP2(K) = 0.0
                        TEMP3(K) = 0.0
                        END IF
 215                 CONTINUE
 220              CONTINUE
C                                       Do sums
               K = 0
               DO 230 I = 1,CANT-1
                  DO 225 J = I+1,CANT
                     K = K + 1
C                                       GN(I) = GN(I) + Z
                     GN(1,I) = GN(1,I) + TEMP2(K)
                     GN(2,I) = GN(2,I) + TEMP3(K)
C                                       GN(J) = GN(J) + CONJG (Z)
                     GN(1,J) = GN(1,J) + TEMP2(K)
                     GN(2,J) = GN(2,J) - TEMP3(K)
 225                 CONTINUE
 230              CONTINUE
C                                       Update solutions
               DO 240 I = 1,CANT
                  WX = W
                  IF (SWT(I).LE.0.0) WX = 0.0
                  XX = ATAN2 (G(2,I), G(1,I))
                  YY = ATAN2 (GN(2,I), GN(1,I)+1.0E-30)
                  ARG = XX - WX * YY
                  G(1,I) = COS (ARG)
                  G(2,I) = SIN (ARG)
 240              CONTINUE
               END IF
C                                       Convergence test
            CONVGD = T
            NFAIL = 0
            IFAIL = 0
            XFAIL = 0.0
            DO 250 I = 1,CANT
               IF (SWT(I).GT.0) THEN
                  IF (IT.LE.30) THEN
                     IF (CONVGD) THEN
                        X = TOL * (5.0E-7 + SQRT (G(1,I)*G(1,I) +
     *                     G(2,I)*G(2,I)))
                        IF (ABS (G(1,I)-GLAST(1,I)).GT.X) CONVGD = F
                        IF (ABS (G(2,I)-GLAST(2,I)).GT.X) CONVGD = F
                        END IF
C                                       Use different criteria after
C                                       30 iterations
                  ELSE
                     XFAIL1 = ABS (G(1,I) - GLAST(1,I))
                     XFAIL2 = ABS (G(2,I) - GLAST(2,I))
                     XFAIL1 = MAX (XFAIL1, XFAIL2)
                     X = TOL * (5.0E-7 + SQRT (G(1,I)*G(1,I) +
     *                  G(2,I)*G(2,I)))
                     IF (XFAIL1.GT.X) THEN
                        CONVGD = F
                        NFAIL = NFAIL + 1
                        IF ((XFAIL1/X).GE.XFAIL) THEN
                           XFAIL = XFAIL1 / X
                           IFAIL = I
                           END IF
                        END IF
                     END IF
                  END IF
               GLAST(1,I) = G(1,I)
               GLAST(2,I) = G(2,I)
 250           CONTINUE
C                                       Find statistics
            S = 0.0
            DO 260 I = 1,CANT-1
               DO 255 J = I+1,CANT
                  Z(1) = G(1,J)
                  Z(2) = - G(2,J)
                  ZR = G(1,I) * Z(1) - G(2,I) * Z(2)
                  ZI = G(1,I) * Z(2) + G(2,I) * Z(1)
                  Z(1) = VOBS(I,J) - ZR
                  Z(2) = VOBS(J,I) - ZI
                  QQ = Z(1) * Z(1) + Z(2) * Z(2)
                  IF (WT(I,J).GT.0.0) S = S + WT(I,J) * QQ
 255              CONTINUE
 260           CONTINUE
C                                       Print statistics
            S = S / SUMWT
            RMS = SQRT (S)
            IF (PRTLV.GE.5) THEN
               WRITE (MSGTXT,1001) IT, S, RMS
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
         CALL RFILL (CANT, 0.0, SWT)
         IF (ITER.EQ.1) RMSMIN = RMS / 1000.0
         RMS = MAX (RMS, RMSMIN)
         DO 320 I = 1,CANT-1
            DO 310 J = I+1,CANT
               IF (WT(I,J).NE.0.0) THEN
                  WT(I,J) = ABS (WT(I,J))
                  Z(1) = G(1,J)
                  Z(2) = - G(2,J)
                  ZR = G(1,I) * Z(1) - G(2,I) * Z(2)
                  ZI = G(1,I) * Z(2) + G(2,I) * Z(1)
                  Z(1) = ABS (VOBS(I,J) - ZR) / RMS
                  Z(2) = ABS (VOBS(J,I) - ZI) / RMS
                  IF ((Z(1).GT.FF(ITER)) .OR. (Z(2).GT.FF(ITER))) THEN
                     WT(I,J) = -WT(I,J)
                  ELSE
                     SWT(I) = SWT(I) + WT(I,J)
                     SWT(J) = SWT(J) + WT(I,J)
                     SUMWT = SUMWT + WT(I,J)
                     END IF
                  END IF
 310           CONTINUE
 320        CONTINUE
 400     CONTINUE
      IERR = 0
C                                       Fraction failed
      CALL RFILL (CANT, 0.0, TEMP2)
      CALL RFILL (CANT, 0.0, TEMP3)
      DO 420 I = 1,CANT-1
         DO 410 J = I+1,CANT
            IF (WT(I,J).NE.0.0) THEN
               TEMP2(I) = TEMP2(I) + 1.0
               TEMP2(J) = TEMP2(J) + 1.0
               END IF
            IF (WT(I,J).LT.0.0) THEN
               TEMP3(I) = TEMP3(I) + 1.0
               TEMP3(J) = TEMP3(J) + 1.0
               END IF
 410        CONTINUE
 420     CONTINUE
C                                       Refer solutions to reference
C                                       antenna.
      XXX = 1.0 / SQRT (G(1,NREF)*G(1,NREF) + G(2,NREF)*G(2,NREF))
      DO 430 I = 1,CANT
         IF ((I.NE.NREF) .AND. (SWT(I).GT.1.0E-20)) THEN
C                                       G(I) = G(I) / G(NREF) *
C                                          CABS (G(NREF))
            ZR = G(1,I) * G(1,NREF) + G(2,I) * G(2,NREF)
            ZI = G(2,I) * G(1,NREF) - G(1,I) * G(2,NREF)
            G(1,I) = XXX * ZR
            G(2,I) = XXX * ZI
            END IF
         CREAL(I) = G(1,I)
         CIMAG(I) = G(2,I)
 430     CONTINUE
      CREAL(NREF) = SQRT (G(1,NREF)*G(1,NREF) + G(2,NREF)*G(2,NREF))
      CIMAG(NREF) = 0.0
C                                       Convert and print results.
      DO 440 I = 1,CANT
         IF (TEMP2(I).GT.0.0) THEN
            FRACT(I) = TEMP3(I) / TEMP2(I)
         ELSE
            FRACT(I) = 1.0
            END IF
         IF (SWT(I).GT.1.0E-20) THEN
            AMP = CREAL(I)**2 + CIMAG(I)**2
            PH = ATAN2 (CIMAG(I), CREAL(I))
            CREAL(I) = AMP * COS (PH)
            CIMAG(I) = AMP * SIN (PH)
            IF (PRTLV.GE.4) THEN
               PH = PH * 57.296
               WRITE (MSGTXT,1090) I, AMP, PH
               CALL MSGWRT (3)
               END IF
C                                       zero fill failed
         ELSE IF (INW(I).GT.1.E-20) THEN
            CREAL(I) = 0.0
            CIMAG(I) = 0.0
            FRACT(I) = 1.0
C                                       blank empties
         ELSE
            CREAL(I) = FBLANK
            CIMAG(I) = FBLANK
            FRACT(I) = 1.0
            END IF
 440     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I2,I3,' Amp =',1PE12.3,' Phase =',0PF7.1,1PE12.3)
 1001 FORMAT ('Iter=',I5,' s=',1PE15.5,' RMS=',1E15.5)
 1090 FORMAT ('Ant=',I5,' Amp=',F12.5,' Phase=',F12.2)
      END
