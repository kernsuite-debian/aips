      SUBROUTINE GCALC1 (INIT, VOBS, IS, JS, WT, NUMBL, REFANT, MODE,
     *   MINNO, G, NREF, PRTLV, FFLIM, FFLAST, FRAC, RMS, IERR)
C-----------------------------------------------------------------------
C! Gain Soln: Does L1 solution for gains - can do it robustly
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
C   GCALC1 does an L1 type gain solution similar to GCALC.
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
      REAL      VOBS(2,*), WT(*), G(2,*), FFLIM, FFLAST, FRAC, RMS
C
      INTEGER   ITRMAX
      PARAMETER (ITRMAX = 10)
C
      INTEGER   K, NT, NTD, NE, IT, ITMAX, I, II, NIT, J, JJ, MAXANS,
     *   LMODE, LWT, ITRMIN, ITER, IE
      REAL      EPSS(3), AMP, PH, XLAMB, PTERM, FF(ITRMAX), GG(ITRMAX)
      DOUBLE PRECISION GD, T, EPS, F, TOL, S, QQ, W, XX, YY, X,
     *   XRR, XRD, XRZ, XRTEMP, XRGNG, XIR, XID, XIZ, XITEMP, XIGNG,
     *   D1, D2, F1, F2, RMEAN, T1, T2, SUMWT, XXX
      LOGICAL   CONVGD, TR, FA, DORMS
      INCLUDE 'INCS:PUVD.INC'
      REAL      SWT(MAXANT), P1(MAXANT), P2(MAXANT), GLAST(2,MAXANT)
      DOUBLE PRECISION XRDG(MAXANT), XIDG(MAXANT), XRGLAS(MAXANT),
     *   XIGLAS(MAXANT)
      INCLUDE 'INCS:DMSG.INC'
      COMMON /GLASTS/ GLAST
      DATA TR, FA /.TRUE.,.FALSE./
      DATA EPSS /5.E-3, 5.E-4, 5.E-5/
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
      XLAMB = 0.0
      PTERM = 0.0
      MAXANS = MAXANT
      NE = 3
      ITMAX = 60
      TOL = 5.0E-5
      NT = 0
      NTD = 0
      SUMWT = 0.0
      DO 10 I = 1,MAXANS
         SWT(I) = 0.
 10      CONTINUE
C                                       Find which antennae have data
C                                       and normalize to unit amplitude
C                                       if requested.
      DO 20 K = 1,NUMBL
         I = IS(K)
         J = JS(K)
         XXX = SQRT ((VOBS(1,K)*VOBS(1,K)) + (VOBS(2,K)*VOBS(2,K)))
         IF (XXX.LE.1.0E-20) WT(K) = 0.0
         IF (WT(K).GT.0.0) THEN
            NT = MAX (NT, J)
C                                       Normalize by amplitude if only
C                                       solving for phase without ampl.
            IF (LMODE.EQ.2) THEN
               VOBS(1,K) = VOBS(1,K) / XXX
               VOBS(2,K) = VOBS(2,K) / XXX
               WT(K)  = WT(K) * XXX * XXX
               END IF
            CALL REWAIT (LWT, WT(K))
            SWT(I) = SWT(I) + WT(K)
            SWT(J) = SWT(J) + WT(K)
            SUMWT = SUMWT + WT(K)
         ELSE
            WT(K) = 0.0
            END IF
 20      CONTINUE
C                                       No data
      IF (SUMWT.LE.1.0E-20) GO TO 999
C                                       Print data if requested
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
            XRDG(I) = 1.0D0
            XIDG(I) = 0.0D0
         ELSE
            G(1,I) = GLAST(1,I)
            G(2,I) = GLAST(2,I)
            XRDG(I) = GLAST(1,I)
            XIDG(I) = GLAST(2,I)
            END IF
         XRGLAS(I) = 1.0D0
         XIGLAS(I) = 0.0D0
         IF (SWT(I).GT.0.) NTD = NTD + 1
 40      CONTINUE
      DORMS = (2.5*(NTD-1).LE.NUMBL)
C                                       Pick reference antenna
      IF ((INIT) .OR. (NREF.LE.0) .OR. (NREF.GT.MAXANS)) THEN
         NREF = REFANT
         IF ((NREF.LT.1) .OR. (NREF.GT.MAXANS) .OR. (SWT(NREF).LE.0.0))
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
C                                       L1 loop
         DO 270 IE = 1,NE
            EPS = EPSS(IE)
            IF (PRTLV.GE.2) THEN
               S = 0.0
               DO 110 K = 1,NUMBL
                  IF (WT(K).GT.0.) THEN
                     I = IS(K)
                     J = JS(K)
                     XRZ = VOBS(1,K) - (XRDG(I) * XRDG(J) +
     *                  XIDG(I) * XIDG(J))
                     XIZ = VOBS(2,K) - (XRDG(J) * XIDG(I) -
     *                  XRDG(I) * XIDG(J))
                     QQ = XRZ * XRZ + XIZ * XIZ
                     S = S + WT(K) * SQRT (QQ + EPS)
                     END IF
 110              CONTINUE
               RMEAN = S / SUMWT
               IT = 0
               WRITE (MSGTXT,1110) IT, S, RMEAN
               CALL MSGWRT (3)
               END IF
C                                       Solution loop
            DO 260 IT = 1,ITMAX
               IF (IT.GT.15) TOL = (IT-10) * 1.0E-5
               NIT = IT
C                                       Section to solve for full
C                                       complex gains:
               IF (LMODE.LE.0) THEN
                  IF (IT.EQ.1) W = 0.5
                  IF (IT.EQ.2) W = 1.5
                  IF (IT.EQ.3) W = 1.5
                  IF (NTD.LE.6) W = .25
                  DO 130 I = 1,NT
                     IF (SWT(I).EQ.0.) GO TO 130
                     XRGNG = 0.0D0
                     XIGNG = 0.0D0
                     GD = 0.
                     DO 120 K = 1,NUMBL
                        II = IS(K)
                        JJ = JS(K)
                        IF ((WT(K).GT.0.0) .AND. (SWT(II).GT.0.) .AND.
     *                     (SWT(JJ).GT.0.)) THEN
                           IF (I.EQ.II) THEN
                              XRR = VOBS(1,K) - (XRDG(II) * XRDG(JJ) +
     *                           XIDG(II) * XIDG(JJ))
                              XIR = VOBS(2,K) - (XRDG(JJ) * XIDG(II) -
     *                           XRDG(II) * XIDG(JJ))
                              T = WT(K) / SQRT (XRR * XRR + XIR * XIR +
     *                           EPS)
                              XRZ = XRDG(JJ)
                              XIZ = XIDG(JJ)
                              GD = GD + T * (XRZ * XRZ + XIZ * XIZ)
                              XRGNG = XRGNG + T * (XRDG(JJ) * VOBS(1,K)
     *                           - XIDG(JJ) * VOBS(2,K))
                              XIGNG = XIGNG + T * (XRDG(JJ) * VOBS(2,K)
     *                           + VOBS(1,K) * XIDG(JJ))
                           ELSE IF (I.EQ.JJ) THEN
                              XRR = VOBS(1,K) - (XRDG(II) * XRDG(JJ) +
     *                           XIDG(II) * XIDG(JJ))
                              XIR = VOBS(2,K) - (XRDG(JJ) * XIDG(II) -
     *                           XRDG(II) * XIDG(JJ))
                              T = WT(K) / SQRT (XRR * XRR + XIR * XIR +
     *                           EPS)
                              XRZ = XRDG(II)
                              XIZ = XIDG(II)
                              GD = GD + T * (XRZ * XRZ + XIZ * XIZ)
                              XRGNG = XRGNG + T * (XRDG(II) * VOBS(1,K)
     *                           + XIDG(II) * VOBS(2,K))
                              XIGNG = XIGNG + T * (VOBS(1,K) * XIDG(II)
     *                           - XRDG(II) * VOBS(2,K))
                              END IF
                           END IF
 120                    CONTINUE
                     XRDG(I) = XRDG(I) + W * (XRGNG / GD - XRDG(I))
                     XIDG(I) = XIDG(I) + W * (XIGNG / GD - XIDG(I))
 130                 CONTINUE
C                                       Section to solve for phase
C                                       corrections alone:
               ELSE
                  DO 210 I = 1,NT
                     P1(I) = 0.
                     P2(I) = 0.
 210                 CONTINUE
                  W = 0.5
                  IF (IT.LE.3) W = .25
                  IF (NTD.LE.6) W = .25
                  DO 220 K = 1,NUMBL
                     I = IS(K)
                     J = JS(K)
                     IF ((SWT(I).GT.0.) .AND. (SWT(J).GT.0.) .AND.
     *                  (WT(K).GT.0.)) THEN
                        XRD = XRDG(I) * XRDG(J) + XIDG(I) * XIDG(J)
                        XID = XRDG(J) * XIDG(I) - XRDG(I) * XIDG(J)
                        XRR = VOBS(1,K) - XRD
                        XIR = VOBS(2,K) - XID
                        XRTEMP = XRD
                        XITEMP = XID
                        XRD = XRTEMP * VOBS(1,K) + XITEMP * VOBS(2,K)
                        XID = VOBS(1,K) * XITEMP - XRTEMP * VOBS(2,K)
                        QQ = XRR * XRR + XIR * XIR
                        F = QQ + EPS
                        F1 = SQRT (F)
                        F2 = F * F1
                        D1 = 2.0 * XID
                        D2 = 2.0 * XRD
                        T1 = .5 * WT(K) / F1 * D1
                        T2 = .5 * WT(K) * (D2 / F1-.5 * (D1 ** 2) / F2)
                        P1(I) = P1(I) + T1
                        P1(J) = P1(J) - T1
                        P2(I) = P2(I) + T2
                        P2(J) = P2(J) + T2
                        END IF
 220                 CONTINUE
                  DO 230 I = 1,NT
                     IF (SWT(I).GT.0.) THEN
                        XX = ATAN2 (XIDG(I), XRDG(I))
                        YY = ATAN2 (P1(I), P2(I))
                        XRDG(I) = COS (XX - W * YY)
                        XIDG(I) = SIN (XX - W * YY)
                        END IF
 230                 CONTINUE
                  END IF
C
               CONVGD = TR
               DO 240 I = 1,NT
                  IF ((SWT(I).GT.0.) .AND. (CONVGD)) THEN
                     X = TOL * (5E-7 + SQRT (XRDG(I) * XRDG(I) +
     *                  XIDG(I) * XIDG(I)))
                     IF (ABS (XRDG(I)-XRGLAS(I)).GT.X) CONVGD = FA
                     IF (ABS (XIDG(I)-XIGLAS(I)).GT.X) CONVGD = FA
                     END IF
                  XRGLAS(I) = XRDG(I)
                  XIGLAS(I) = XIDG(I)
 240              CONTINUE
               IF (PRTLV.GE.2) THEN
                  S = 0.0
                  DO 250 K = 1,NUMBL
                     IF (WT(K).GT.0.) THEN
                        I = IS(K)
                        J = JS(K)
                        XRZ = VOBS(1,K) - (XRDG(I) * XRDG(J) +
     *                     XIDG(I) * XIDG(J))
                        XIZ = VOBS(2,K) - (XRDG(J) * XIDG(I) -
     *                     XRDG(I) * XIDG(J))
                        QQ = XRZ * XRZ + XIZ * XIZ
                        S = S + WT(K) * SQRT(QQ + EPS)
                        END IF
 250                 CONTINUE
                  RMEAN = S / SUMWT
                  WRITE (MSGTXT,1110) IT, S, RMEAN
                  CALL MSGWRT (3)
                  END IF
               IF (CONVGD) GO TO 270
 260           CONTINUE
 270        CONTINUE
C                                       finished this robust iteration
C                                       get rms
         S = 0.0
         XXX = 1.0
         DO 280 K = 1,NUMBL
            IF (WT(K).GT.0.) THEN
               I = IS(K)
               J = JS(K)
               IF (LMODE.EQ.1) XXX = SQRT ((VOBS(1,K)*VOBS(1,K)) +
     *            (VOBS(2,K)*VOBS(2,K)))
               XRZ = VOBS(1,K)/XXX - (XRDG(I) * XRDG(J) +
     *            XIDG(I) * XIDG(J))
               XIZ = VOBS(2,K)/XXX - (XRDG(J) * XIDG(I) -
     *            XRDG(I) * XIDG(J))
               QQ = XRZ * XRZ + XIZ * XIZ
               S = S + WT(K) * QQ
               END IF
 280        CONTINUE
         RMS = SQRT (S / SUMWT)
C                                       converged: what to use this time
 300     SUMWT = 0.0
         CALL RFILL (MAXANS, 0.0, SWT)
         XXX = 1.0
         IF (.NOT.DORMS) RMS = 1.0E10
         DO 310 K = 1,NUMBL
            I = IS(K)
            J = JS(K)
            IF (WT(K).NE.0.0) THEN
               WT(K) = ABS (WT(K))
               IF (LMODE.EQ.1) XXX = SQRT ((VOBS(1,K)*VOBS(1,K)) +
     *            (VOBS(2,K)*VOBS(2,K)))
               XRZ = ABS (VOBS(1,K)/XXX - (XRDG(I) * XRDG(J) +
     *            XIDG(I) * XIDG(J))) / RMS
               XIZ = ABS (VOBS(2,K)/XXX - (XRDG(J) * XIDG(I) -
     *            XRDG(I) * XIDG(J))) / RMS
               IF (((XRZ.GT.FF(ITER)) .OR. (XIZ.GT.FF(ITER))) .AND.
     *            (DORMS)) THEN
                  WT(K) = -WT(K)
               ELSE
                  SWT(I) = SWT(I) + WT(K)
                  SWT(J) = SWT(J) + WT(K)
                  SUMWT = SUMWT + WT(K)
                  END IF
               END IF
 310        CONTINUE
 400     CONTINUE
C                                       all done finally
      IERR = 0
      IF (.NOT.DORMS) RMS = 0.0
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
      G(1,NREF) = XRDG(NREF)
      G(2,NREF) = XIDG(NREF)
      XX = 1.0 / SQRT (G(1,NREF)*G(1,NREF) + G(2,NREF)*G(2,NREF))
      DO 420 I = 1,NT
         G(1,I) = XRDG(I)
         G(2,I) = XIDG(I)
         IF ((I.NE.NREF) .AND. (SWT(I).GT.0.)) THEN
            XRR = G(1,I) * G(1,NREF) + G(2,I) * G(2,NREF)
            XIR = G(2,I) * G(1,NREF) - G(1,I) * G(2,NREF)
            G(1,I) = XX * XRR
            G(2,I) = XX * XIR
            GLAST(1,I) = G(1,I)
            GLAST(2,I) = G(2,I)
            END IF
 420     CONTINUE
      G(1,NREF) = SQRT (G(1,NREF)*G(1,NREF) + G(2,NREF)*G(2,NREF))
      G(2,NREF) = 0.0
      GLAST(1,NREF) = G(1,NREF)
      GLAST(2,NREF) = G(2,NREF)
C                                       print results
      IF (PRTLV.GE.1) THEN
         DO 430 I = 1,NT
            IF (SWT(I).GT.0.) THEN
               AMP = SQRT (G(1,I)*G(1,I) + G(2,I)*G(2,I))
               PH = 57.2958 * ATAN2 (G(2,I), G(1,I))
               WRITE (MSGTXT,1090) I, AMP, PH
               CALL MSGWRT (3)
               END IF
 430        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (1X,I4,1PE12.3,1E12.3,2I3,E12.3)
 1110 FORMAT ('Iter=',I5,' s=',1PE15.5,' rmean=',1E15.5)
 1090 FORMAT ('Ant=',I5,' amp=',F12.5,' phase=',F12.2)
      END
