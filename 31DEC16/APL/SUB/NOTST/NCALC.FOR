      SUBROUTINE NCALC (INIT, VOBS, IS, JS, WT, NUMBL, REFANT, MODE,
     *   MINNO, GAERR, CONFAC, G, NREF, PRTLV, FFLIM, FFLAST, FRAC, RMS,
     *   IERR)
C-----------------------------------------------------------------------
C! Gain Soln: Does least sq. gain solution with amplitude constraints.
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
C   NCALC computes antenna gains.  Will add penalty term to constrain
C   the amplitude of the gains.  Penalty term is proportional to the
C   difference in the current gain RMS (in sigmas) from the expected
C   value.
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
C      GAERR    R(*)     Standard deviations of the assumed
C                        distributions of the amplitudes of the gains.
C                        Used to constrain the solutions.
C      CONFAC   R        Factor to multiply times the penalty term to
C                        increase or decrease its importance.
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
C      NREF     I        Reference antenna used.
C      RMS      R        RMS closure error
C      IERR     I        Return error code 0 = OK
C                           1 = no valid data
C                           2 = didn't converge
C                           3 = too few antennas
C-----------------------------------------------------------------------
      LOGICAL   INIT
      INTEGER   IS(*), JS(*), NUMBL, REFANT, MODE, MINNO, NREF, PRTLV,
     *   IERR, IIER
      REAL      VOBS(2,*), WT(*), GAERR(*), CONFAC, G(2,*), FFLIM,
     *   FFLAST, FRAC, RMS
C
      INTEGER   ITRMAX
      PARAMETER (ITRMAX = 10)
C
      INTEGER   LDH, N
      INTEGER   IARG1, IARG2, MAXANS, K, KK, J, JJ, I, II, L, LL, NT,
     *   NTD, MODESV, IT, ITMAX, IIP, JJP, IIA, JJA, ID2, LMODE, LWT,
     *   ITER, ITRMIN
      REAL      XRE, XIM, XAMP, XPHAS, XXX, PH, AMP, SUMWT, FF(ITRMAX),
     *   Z(2), ZR, ZI, QQ, XX, YY, GG(ITRMAX)
      DOUBLE PRECISION S, SL, PENSUM, GN, W, RCOND, DET(2),
     *   TOL, XLAM, C, AI, AJ, PI, PJ, SR, SI, CR, CI
      LOGICAL   DORMS
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:GAIN.INC'
      INTEGER   IA(MAXANT), IB(MAXANT), INERT(3), KPVT(MAXPRM)
      REAL      SWT(MAXANT), GLAST(2,MAXANT)
      INCLUDE 'INCS:DMSG.INC'
      COMMON /GLASTS/ GLAST
      DATA ITMAX/50/, TOL/1D-6/
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
      LDH = MAXPRM
      MAXANS = MAXANT
      DO 10 I = 1,MAXANS
         SWT(I) = 0.0
 10      CONTINUE
      NT = 0
      SUMWT = 0.0
C                                       Determine which antennas have
C                                       data. NT is the highest
C                                       numbered antenna with data
C                                       present. SWT(K) is the sum of
C                                       data weights involving antenna
C                                       K. SUMWT is the sum of all data
C                                        weights.
      DO 20 K = 1,NUMBL
         I = IS(K)
         J = JS(K)
         NT = MAX (NT,J)
C                                       Assume that any 0 observations
C                                       ought to have been flagged, and
C                                       insist on non negative weights:
         XXX = SQRT (VOBS(1,K)*VOBS(1,K) + VOBS(2,K)*VOBS(2,K))
         IF (XXX.EQ.0.0) WT(K) = 0.0
         IF (WT(K).GT.0.) THEN
C                                       In case MODE=2, so that
C                                       amplitude info. is to be
C                                       ignored, give the obs. unit
C                                       modulus:
            IF (LMODE.EQ.2) THEN
               VOBS(1,K) = VOBS(1,K) / XXX
               VOBS(2,K) = VOBS(2,K) / XXX
               WT(K) = WT(K) * XXX * XXX
               END IF
            CALL REWAIT (LWT, WT(K))
            SWT(I) = SWT(I) + WT(K)
            SWT(J) = SWT(J) + WT(K)
            SUMWT = SUMWT + WT(K)
C                                       If PRTLV .ge. 3  print data.
            IF ((PRTLV.GE.3) .AND. (WT(K).GT.1.0E-20)) THEN
               XRE = VOBS(1,K) + 1.0E-20
               XIM = VOBS(2,K)
               XAMP = SQRT (XRE * XRE + XIM * XIM)
               XPHAS = 57.296 * ATAN2 (XIM, XRE)
               WRITE (MSGTXT,1010) I, J, XAMP, XPHAS
               CALL MSGWRT (3)
               END IF
         ELSE
            WT(K) = 0.0
            END IF
 20      CONTINUE
C                                       Check whether any data were
C                                       present:
      IF (SUMWT.EQ.0.) GO TO 999
C                                       init solutions
      DO 25 I = 1,MAXANS
         IF ((INIT) .OR. (SWT(I).LE.0.0)) THEN
            G(1,I) = 1.0
            G(2,I) = 0.0
         ELSE
            G(1,I) = GLAST(1,I) * COS (GLAST(2,I))
            G(2,I) = GLAST(1,I) * SIN (GLAST(2,I))
            END IF
 25      CONTINUE
C                                       Check the reference antenna for
C                                       presence of data.
C                                       Choose another, if need be:
      IF ((INIT) .OR. (NREF.LE.0) .OR. (NREF.GT.MAXANS)) THEN
         NREF = REFANT
         IF ((NREF.LT.1) .OR. (NREF.GT.MAXANS) .OR. (SWT(NREF).LE.0.0))
     *      THEN
            XXX = 0.0
            DO 30 I = 1,NT
               IF (SWT(I).GT.XXX) THEN
                  XXX = SWT(I)
                  NREF = I
                  END IF
 30            CONTINUE
            END IF
         END IF
C                                       Check for plenty of antennae,
C                                       and count the number (NTD)
C                                       with data:
      NTD = 0
      DO 40 I = 1,NT
         IF (SWT(I).GT.0.0) NTD = NTD + 1
 40      CONTINUE
      IERR = 3
      IF (NTD.LT.MINNO) GO TO 999
      DORMS = (2.5*(NTD-1).LE.NUMBL)
C                                       Assign a new antenna numbering,
C                                       based only on those antennas
C                                       with data present.  Assign 1
C                                       to the reference antenna.
C                                       Store the assignments in IA,
C                                       and let IB be the inverse
C                                       permutation.
      IA(1) = NREF
      IB(NREF) = 1
      K = 1
      DO 50 I = 1,NT
         IF ((SWT(I).GT.0.0) .AND. (I.NE.NREF)) THEN
            K = K + 1
            IA(K) = I
            IB(I) = K
            END IF
 50      CONTINUE
C                                       Set initial guesses to zero for
C                                       the arguments of the gains,
C                                       stored in XPRM(1),...,
C                                       XPRM(NTD-1),and
C                                       to unity for their moduli,
C                                       stored in XPRM(NTD),...,
C                                       XPRM(2*NTD-1).
C                                       Also initialize the correction
C                                       vector, STEP, to 0.
C                                       N is the total number of real
C                                       unknowns.
      N = 2 * NTD - 1
      IF ((LMODE.EQ.1) .OR. (LMODE.EQ.2)) N = NTD - 1
      DO 60 I = 1,N
         IF (INIT) THEN
            IF (I.LT.NTD) THEN
               XPRM(I) = 0.0D0
            ELSE
               XPRM(I) = 1.0D0
               END IF
         ELSE
            IF (I.LT.NTD) THEN
               K = IA(I+1)
               XPRM(I) = GLAST(2,K)
            ELSE
               K = IA(I+1-NTD)
               XPRM(I) = GLAST(1,K)
               END IF
            K = IA(I)
            END IF
         STEP(I) = 0.0D0
 60      CONTINUE
      MODESV = LMODE
C                                       Robust iteration
      IF (.NOT.DORMS) ITRMIN = ITRMAX
      DO 400 ITER = ITRMIN,ITRMAX
C                                       When solving for amplitude and
C                                       phase solutions, first converge
C                                       upon `phase-only' (MODE=1) soln
         IF (((LMODE.EQ.0) .OR. (LMODE.EQ.3)) .AND. (ITER.LT.ITRMIN+5))
     *      THEN
            LMODE = 1
            N = NTD - 1
            END IF
C                                       Loop-back point to do amp/phase
 100     CONTINUE
C
C                                       Iterate:
         DO 250 IT = 1,ITMAX
C                                       Zero the gradient and
C                                       Hessian arrays:
            DO 110 I = 1,N
               GRAD(I) = 0.0
               DO 105 J = 1,N
                  HESS(I,J) = 0.0
 105              CONTINUE
 110           CONTINUE
C                                       On first iteration, calculate
C                                       the objective function, SL:
            IF (IT.EQ.1) CALL NEVAL (XPRM, 0.0D0, STEP, VOBS, IS, JS,
     *         IA, WT, NUMBL, NTD, LMODE, PRTLV, GAERR, CONFAC, G, SL,
     *         PENSUM)
C                                       Accumulate (half) the gradient
C                                       of S and (half) the Hessian:
            DO 130 K = 1,NUMBL
               IF (WT(K).LE.0.) GO TO 130
               I = IS(K)
               J = JS(K)
               II = IB(I)
               JJ = IB(J)
               IIP = II - 1
               JJP = JJ - 1
               PI = 0.0D0
               PJ = 0.0D0
               IF (I.NE.NREF) PI = XPRM(IIP)
               IF (J.NE.NREF) PJ = XPRM(JJP)
               SR = SIN (PJ - PI) * VOBS(1,K)
               SI = SIN (PJ - PI) * VOBS(2,K)
               CR = COS (PJ - PI) * VOBS(1,K)
               CI = COS (PJ - PI) * VOBS(2,K)
               IF ((LMODE.NE.1) .AND. (LMODE.NE.2)) THEN
                  IIA = NTD + II - 1
                  JJA = NTD + JJ - 1
                  AI = XPRM(IIA)
                  AJ = XPRM(JJA)
                  END IF
               IF (I.EQ.NREF) THEN
                  IF ((LMODE.EQ.1) .OR. (LMODE.EQ.2)) THEN
                     GRAD(JJP) = GRAD(JJP) + WT(K) * (SR + CI)
                     HESS(JJP,JJP) = HESS(JJP,JJP) + WT(K) * (CR - SI)
                  ELSE
                     GRAD(JJP) = GRAD(JJP) + WT(K) * AI * AJ * (SR + CI)
                     GRAD(IIA) = GRAD(IIA) + WT(K) * AJ * (AI*AJ-CR+SI)
                     GRAD(JJA) = GRAD(JJA) + WT(K) * AI * (AI*AJ-CR+SI)
                     HESS(JJP,JJP) = HESS(JJP,JJP) + WT(K) * AI * AJ *
     *                  (CR - SI)
                     HESS(IIA,IIA) = HESS(IIA,IIA) + WT(K) * AJ**2
                     HESS(JJA,JJA) = HESS(JJA,JJA) + WT(K) * AI**2
                     IARG1 = MIN (IIA, JJA)
                     IARG2 = MAX (IIA, JJA)
                     HESS(IARG1,IARG2) = HESS(IARG1,IARG2)
     *                  + WT(K) * (2.0D0 * AI * AJ - CR + SI)
                     IARG1 = MIN (IIA, JJP)
                     IARG2 = MAX (IIA, JJP)
                     HESS(IARG1,IARG2) = HESS(IARG1,IARG2)
     *                  + WT(K) * AJ * (SR + CI)
                     HESS(JJP,JJA) = HESS(JJP,JJA) + WT(K) * AI *
     *                  (SR + CI)
                     END IF
               ELSE IF (J.EQ.NREF) THEN
                  IF ((LMODE.EQ.1) .OR. (LMODE.EQ.2)) THEN
                     GRAD(IIP) = GRAD(IIP) - WT(K) * (SR + CI)
                     HESS(IIP,IIP) = HESS(IIP,IIP) + WT(K) * (CR - SI)
                  ELSE
                     GRAD(IIP) = GRAD(IIP) - WT(K) * AI * AJ * (SR + CI)
                     GRAD(IIA) = GRAD(IIA) + WT(K) * AJ * (AI * AJ -
     *                  CR + SI)
                     GRAD(JJA) = GRAD(JJA) + WT(K) * AI * (AI * AJ -
     *                  CR + SI)
                     HESS(IIP,IIP) = HESS(IIP,IIP) + WT(K) * AI * AJ *
     *                  (CR - SI)
                     HESS(IIA,IIA) = HESS(IIA,IIA) + WT(K) * AJ**2
                     HESS(JJA,JJA) = HESS(JJA,JJA) + WT(K) * AI**2
                     IARG1 = MIN (IIA, JJA)
                     IARG2 = MAX (IIA, JJA)
                     HESS(IARG1,IARG2) = HESS(IARG1,IARG2)
     *                  + WT(K) * (2.0D0 * AI * AJ - CR + SI)
                     HESS(IIP,IIA) = HESS(IIP,IIA) - WT(K) * AJ *
     *                  (SR + CI)
                     IARG1 = MIN (IIP, JJA)
                     IARG2 = MAX (IIP, JJA)
                     HESS(IARG1,IARG2) = HESS(IARG1,IARG2)
     *                  - WT(K) * AI * (SR + CI)
                     END IF
               ELSE
                  IF ((LMODE.EQ.1) .OR. (LMODE.EQ.2)) THEN
                     GRAD(IIP) = GRAD(IIP) - WT(K) * (SR + CI)
                     GRAD(JJP) = GRAD(JJP) + WT(K) * (SR + CI)
                     HESS(IIP,IIP) = HESS(IIP,IIP) + WT(K) * (CR - SI)
                     HESS(JJP,JJP) = HESS(JJP,JJP) + WT(K) * (CR - SI)
                     IARG1 = MIN (IIP, JJP)
                     IARG2 = MAX (IIP, JJP)
                     HESS(IARG1,IARG2) = HESS(IARG1,IARG2)
     *                  - WT(K) * (CR - SI)
                  ELSE
                     GRAD(IIP) = GRAD(IIP) - WT(K) * AI * AJ * (SR + CI)
                     GRAD(JJP) = GRAD(JJP) + WT(K) * AI * AJ * (SR + CI)
                     GRAD(IIA) = GRAD(IIA) + WT(K) * AJ * (AI * AJ -
     *                  CR + SI)
                     GRAD(JJA) = GRAD(JJA) + WT(K) * AI * (AI * AJ -
     *                  CR + SI)
                     HESS(IIP,IIP) = HESS(IIP,IIP) + WT(K) * AI * AJ *
     *                  (CR - SI)
                     HESS(JJP,JJP) = HESS(JJP,JJP) + WT(K) * AI * AJ *
     *                  (CR - SI)
                     IARG1 = MIN (IIP, JJP)
                     IARG2 = MAX (IIP, JJP)
                     HESS(IARG1,IARG2) = HESS(IARG1,IARG2)
     *                  - WT(K) * AI * AJ * (CR - SI)
                     HESS(IIA,IIA) = HESS(IIA,IIA) + WT(K) * AJ**2
                     HESS(JJA,JJA) = HESS(JJA,JJA) + WT(K) * AI**2
                     IARG1 = MIN (IIA, JJA)
                     IARG2 = MAX (IIA, JJA)
                     HESS(IARG1,IARG2) = HESS(IARG1,IARG2)
     *                  + WT(K) * (2.0D0 * AI * AJ - CR + SI)
                     HESS(IIP,IIA) = HESS(IIP,IIA) - WT(K) * AJ *
     *                  (SR + CI)
                     IARG1 = MIN (IIA, JJP)
                     IARG2 = MAX (IIA, JJP)
                     HESS(IARG1,IARG2) = HESS(IARG1,IARG2)
     *                  + WT(K) * AJ * (SR + CI)
                     IARG1 = MIN (IIP, JJA)
                     IARG2 = MAX (IIP, JJA)
                     HESS(IARG1, IARG2) = HESS(IARG1,IARG2)
     *                  - WT(K) * AI * (SR + CI)
                     HESS(JJP,JJA) = HESS(JJP,JJA) + WT(K) * AI *
     *                  (SR + CI)
                     END IF
                  END IF
 130           CONTINUE
C                                       Add in the amplitude penalty
C                                       terms:
            IF (LMODE.EQ.3) THEN
               XLAM = PENSUM - NTD
               C = 0.1 * CONFAC
               DO 150 I = 1,NTD
                  K = IA(I)
                  KK = NTD + I - 1
                  GRAD(KK) = GRAD(KK) + 2.0D0*C * XLAM *
     *               (XPRM(KK) - 1.0D0) / GAERR(K)**2
                  HESS(KK,KK) = HESS(KK,KK) + 2.0D0*C * XLAM /
     *               GAERR(K)**2
                  DO 140 J = I,NTD
                     L = IA(J)
                     LL = NTD + J - 1
                     HESS(KK,LL) = HESS(KK,LL) + 4.0D0 * C *
     *                  (XPRM(LL) - 1.0D0) * (XPRM(KK) - 1.0D0)
     *                  / (GAERR(L) * GAERR(K))**2
 140                 CONTINUE
 150              CONTINUE
               END IF
C                                       Fill in the lower triangular
C                                       part of the Hessian:
            DO 160 I = 2,N
               JJ = I - 1
               DO 155 J = 1,JJ
                  HESS(I,J) = HESS(J,I)
 155              CONTINUE
 160           CONTINUE
C                                       Save the Hessian, in case
C                                       that it's indefinite:
            DO 170 I = 1,N
               DO 165 J = 1,N
                  SHESS(I,J) = HESS(I,J)
 165              CONTINUE
 170           CONTINUE
C                                       Calculate and print the
C                                       Euclidean norm of the gradient,
C                                       in order to monitor the
C                                       progress toward a critical
C                                       point:
            CALL DNRM2 (N, GRAD, 1, GN)
            IF (PRTLV.GE.2) THEN
               WRITE (MSGTXT,1000) IT,GN
               CALL MSGWRT (3)
               END IF
C                                       Factor the Hessian, and obtain
C                                       an estimate of its cond. no.:
            CALL DSICO (HESS, LDH, N, KPVT, RCOND, GWORK)
            IF (PRTLV.GE.2) THEN
               WRITE (MSGTXT,1001) RCOND
               CALL MSGWRT (3)
               END IF
C                                       Compute determinant and inertia:
            IF ((1.0D0+RCOND).NE.1.0D0) THEN
               CALL DSIDI (HESS, LDH, N, KPVT, DET, INERT, GWORK, 110)
               ID2 = DET(2)
               IF (PRTLV.GE.2) THEN
                  WRITE (MSGTXT,1002) DET(1), ID2, INERT
                  CALL MSGWRT (3)
                  END IF
               END IF
C                                       If the Hessian is indefinite
C                                       or singular, use Greenstadt's
C                                       modification:
            IF ((INERT(1).NE.N) .OR. ((1.0D0+RCOND).EQ.1.0D0)) THEN
               CALL GM (N, SHESS, HESS, GWORK, LDH, GRAD, IIER)
               IF (IIER.NE.0) GO TO 999
C                                       Solve for the Newton correction
C                                       (to be placed in GRAD):
            ELSE
               CALL DSISL (HESS, LDH, N, KPVT, GRAD)
               END IF
C                                       Take a damped Newton step:
            DO 180 I = 1,N
               STEP(I) = GRAD(I)
               IF (I.GE.NTD.AND.XPRM(I) - STEP(I).LE.0.)
     *            STEP(I) = 0.5 * XPRM(I)
 180           CONTINUE
            W = 1.
            DO 190 KK = 1,8
               CALL NEVAL (XPRM, W, STEP, VOBS, IS, JS, IA, WT, NUMBL,
     *            NTD, LMODE, PRTLV, GAERR, CONFAC, G, S, PENSUM)
               IF (S.LT.SL) GO TO 200
               IF (KK.LE.4) W = 0.5 * W
               IF (KK.GT.4) W = 0.1 * W
 190           CONTINUE
            GO TO 260
 200        IF ((ABS (1.0D0-S/SL).LE.5.0D-6) .AND. (GN.LT.1.)) GO TO 260
            SL = S
            DO 210 I = 1,N
               XPRM(I) = XPRM(I) - W * STEP(I)
 210           CONTINUE
C                                       not yet converged?
            DO 220 I = 1,N
               IF (ABS (STEP(I)).GT.(ABS (XPRM(I)) + 1D-3) * TOL)
     *            GO TO 250
 220           CONTINUE
            GO TO 260
 250        CONTINUE
         IERR = 2
         IF (LMODE.EQ.MODESV) GO TO 999
C                                       converged switch to A&P
 260     IF (MODESV.NE.LMODE) THEN
            LMODE = MODESV
            N = 2 * NTD - 1
            GO TO 100
            END IF
C                                       squirrel away results
         PH = 0.0
         AMP = 1.0
         DO 270 I = 1,NTD
            K = IA(I)
            IF ((LMODE.EQ.0) .OR. (LMODE.EQ.3)) AMP = XPRM(NTD+I-1)
            IF (I.GT.1) PH = XPRM(I-1)
            G(1,K) = AMP * COS (PH)
            G(2,K) = AMP * SIN (PH)
            GLAST(1,K) = AMP
            GLAST(2,K) = PH
 270        CONTINUE
C                                       find statistics
         S = 0.0
         XXX = 1.0
         DO 280 K = 1,NUMBL
            IF (WT(K).GT.0.0) THEN
               II = IS(K)
               JJ = JS(K)
               Z(1) = G(1,JJ)
               Z(2) = - G(2,JJ)
               ZR = G(1,II) * Z(1) - G(2,II) * Z(2)
               ZI = G(1,II) * Z(2) + G(2,II) * Z(1)
               IF (LMODE.EQ.1) XXX = SQRT ((VOBS(1,K)*VOBS(1,K)) +
     *            (VOBS(2,K)*VOBS(2,K)))
               Z(1) = VOBS(1,K)/XXX - ZR
               Z(2) = VOBS(2,K)/XXX - ZI
               QQ = Z(1) * Z(1) + Z(2) * Z(2)
               S = S + WT(K) * QQ
               END IF
 280        CONTINUE
         RMS = SQRT (S/SUMWT)
C                                       drop some of the extremes
         SUMWT = 0.0
         XXX = 1.0
         CALL RFILL (MAXANS, 0.0, SWT)
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
C                                       print results
      IF (PRTLV.GE.1) THEN
         AMP = 1.0
         DO 540 I = 1,NT
            IF (SWT(I).GT.0.) THEN
               K = IB(I)
               IF (LMODE.EQ.0.OR.LMODE.EQ.3) AMP = XPRM(NTD+K-1)
               PH = 0.0
               IF (K.GT.1) PH = 57.2958 * ATAN2 (G(2,I), G(1,I))
               WRITE (MSGTXT,1100) I, AMP, PH
               CALL MSGWRT (3)
               END IF
 540        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Iteration #',I4,'   gradient norm=',1PE15.5)
 1001 FORMAT ('Reciprocal cond. no.=',1PE15.5)
 1002 FORMAT ('Det.=',F15.5,'E',I4,'   Inertia=',3I3)
 1010 FORMAT ('Baseline ',I3,'-',I3,' Amp=',F10.5,' Phase=',F6.1)
 1100 FORMAT ('Ant=',I4,' Amp=',1PE15.5,' Phase=',0PF8.3)
      END
