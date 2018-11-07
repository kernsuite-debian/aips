      SUBROUTINE DVDMIN (FX, XI, ERR, N, EPS, ITMAX, FOPT, GNOPT,
     *   NPR, IER)
C-----------------------------------------------------------------------
C! Davidon's optimally conditioned method for function minimization.
C# Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2011
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
C    This is a Fortran implementation of Davidon's optimally conditioned
C  variable metric (quasi-Newton) method for function minimization.  It
C  is based on the algorithm given in W. C. Davidon:  Optimally condi-
C  tioned optimization algorithms without line searches, Mathematical
C  Programming, vol. 9 (1975) pp. 1-30.  One should refer to that re-
C  ference for the algorithmic details.  Here, the steps of the
C  algorithm which are delineated by COMMENT lines correspond to the
C  numbered steps in Davidon's paper.  The user must supply a subroutine
C  FX to calculate the objective function and its gradient at a given
C  point.  The objective function F is assumed to be a real-valued
C  function of N real variables.  Here, 0 is assumed to be a lower
C  bound for F.  If F can assume negative values, Step 2 must be modi-
C  fied in one of two different ways, depending on whether a lower
C  bound is known (see Davidon for details).
C  Inputs:
C    FX      ENTRY     A user-supplied subroutine of the form
C                      FX (X, F, G, K) which is used to calculate the
C                      value of the objective function F at X and, op-
C                      tionally, the gradient G of F at X.  When K=1, FX
C                      need only compute F.  When K=2, both F and G are
C                      required.
C    XI(N)   D         An initial estimate for the location of a mini-
C                      mum.
C    ERR(N)  D         An initial estimate of the square roots of the
C                      diagonal elements of the inverse of the Hessian
C                      matrix of the objective function evaluated at XI.
C                      When no estimates are known, it should suffice to
C                      set ERR(I)=1.0D0, for all I.
C    N       I         The number of unknowns.
C    EPS     D         A small positive number used in tests to set a
C                      lower bound on the squared Euclidean norm of
C                      vectors considered significantly different from
C                      0.  EPS is used in the convergence test.  Usually
C                      setting EPS in the range 10**(-12) to 10**(-8) is
C                      reasonable.  Very close to a minimum, the algo-
C                      rithm generally exhibits a quadratic rate of con-
C                      vergence, so setting EPS a few orders of magni-
C                      tude too small usually is not too costly.
C    ITMAX   I         The maximum number of iterations.  On average, a
C                      few evaluations of F and slightly more than one
C                      evaluation of G are required at each iteration.
C    NPR     I         A print flag.  When NPR=0, there is no printout;
C                      for NPR=1, the value of F and the Euclidean norm
C                      of G, both evaluated at the location of the best
C                      minimum found so far, are printed at each itera-
C                      tion; for NPR=2, the latter information, together
C                      with the location of the best minimum, is print-
C                      ed at each iteration.
C  Outputs:
C    XI(N)   I         The user-supplied initial guess is replaced by
C                      the location of the best minimum found by the al-
C                      gorithm.
C    ERR(N)  D         The initial estimate supplied by the user is re-
C                      placed by an estimate of the square roots of the
C                      diagonal elements of the Hessian matrix evaluated
C                      at the best minimum found.  In least-squares ap-
C                      plications, assuming that F is the sum of squared
C                      residuals, estimates of the standard errors of
C                      the unknowns can be obtained by multiplying ERR
C                      by the r.m.s. residual.
C    FOPT     D        The value of F evaluated at the location of the
C                      best minimum that was found.
C    GNOPT    D        The Euclidean norm of the gradient of the objec-
C                      tive function, evaluated at the location of the
C                      best minimum that was found.
C    IER      I        An error flag.  When IER=0, convergence was
C                      achieved in ITMAX or fewer iterations; other-
C                      wise not.
C  Remarks:
C  1) This algorithm can be used for under-determined problems.
C  2) It maintains an approximation, in factored form J*transpose(J),
C     to the inverse Hessian of F.  At each iteration, a rank two update
C     is added to this approximation.  This approximation remains posi-
C     tive definite throughout the iteration.  In cases where an un-
C     known, say the Ith unknown, is ill-determined, ERR(I) will be
C     finite on exit from this routine. So, in least-squares applica-
C     tions, the error estimates for ill-determined parameters are like-
C     ly to be too small.
C  2.5) In the case of an under-determined problem (i.e., when the
C     Hessian matrix is singular) J*transpose(J) is a non-singular
C     matrix whose inverse is close to the Hessian matrix.
C  3) Furthermore, in cases where an excellent initial guess is supplied
C     by the user, DVDMIN is likely to converge before it has iterated
C     long enough to get a good approximation to the inverse Hessian.
C     (Understand that it is trying to estimate this second-order in-
C     formation only from the first-order information that is supplied
C     by FX.)  So, in least-squares applications, when convergence oc-
C     curs in just a couple of iterations, the derived error estimates
C     may be inaccurate.
C  4) Another Fortran implementation is given in the technical report
C     by W. C. Davidon and L. Nazareth:  DRVOCR - A Fortran implementa-
C     tion of Davidon's optimally conditioned method, Argonne National
C     Lab., Applied Math. Div. Technical Memo. No. 306, August 1977.
C  5) Comparisons of Davidon's algorithm with other quasi-Newton mini-
C     mization algorithms are given in  J. N. Lyness:  A bench mark
C     experiment for minimization algorithms, Math. of Computation,
C     vol. 33 (1979) pp. 249-264.  This algorithm compares quite favor-
C     ably with others, including the routine QNMDER of Gill et al.,
C     and the Harwell Library routine VA13AD.
C  6) Argonne Lab.'s MINPACK routines (non-proprietary) or NAG Library
C     routines (proprietary) could be used in place of DVDMIN.  They
C     would provide somewhat more flexibility.  They're a bit more con-
C     servative (and therefore more robust, but perhaps less efficient).
C-----------------------------------------------------------------------
      INTEGER MAXUNK
C                                       MAXUNK = max. no unknowns.
      PARAMETER (MAXUNK=64)
      INTEGER   N, ITMAX, NPR, IER, NF, NG, IT, I, J, L, I1, II
      DOUBLE PRECISION XI(*), ERR(*), EPS, FOPT, GNOPT, LAMBDA, F0,
     *   MSQ, MU, NSQ, NU, XJ(MAXUNK,MAXUNK), X0(MAXUNK), X(MAXUNK),
     *   K0(MAXUNK), K(MAXUNK), S(MAXUNK), G(MAXUNK), M(MAXUNK),
     *   P(MAXUNK), Q(MAXUNK), WUN(MAXUNK), AX(MAXUNK),
     *   XDDOT, XDMACH, TINYC, F, GN, F0P, XX, UTU, FP,
     *   UTS, A, B, C, ALF, GAMMA, DELTA, T1, T2, T3, T4, T5, T6,
     *   QTK0, B0
C                                       This for STUPID SUNS
C                                       Well, not so dumb anymore
C     INTEGER FX
      EXTERNAL FX
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Trap excessive parameters
      IF (N.GT.MAXUNK) THEN
         MSGTXT = 'DVDMIN: PROGRAMMER ERROR --- TOO MANY PARAMETERS'
         CALL MSGWRT (8)
         MSGTXT = 'DVDMIN: IF THE TASK CLAIMS TO END SUCCESSFULLY, THE'
         CALL MSGWRT (8)
         MSGTXT = 'DVDMIN: RESULTS ARE UBRELIABLE AND SHOULD BE'
         CALL MSGWRT (8)
         MSGTXT = 'DVDMIN: DISREGARDED'
         END IF
C                                       Initialization:
      CALL DMACH (2, XDMACH)
      TINYC = 1.0D-3*SQRT (XDMACH)
      NF = 1
      NG = 1
      IT = -1
      DO 20 I = 1,N
         X(I) = XI(I)
         X0(I) = XI(I)
         DO 10 J = 1,N
            XJ(I,J) = 0.0D0
 10         CONTINUE
         XJ(I,I) = ERR(I)
 20      CONTINUE
      CALL FX (X, F, G, 2)
      F0 = F
      DO 40 I = 1,N
         DO 30 J = 1,N
            AX(J) = XJ(J,I)
 30         CONTINUE
         CALL DDOT (N, AX, 1, G, 1, WUN(I))
         K0(I) = WUN(I)
 40      CONTINUE
C                                       Step 1:
 100  IT = IT+1
      CALL DNRM2 (N, G, 1, GN)
      IF (NPR.GE.1) THEN
         WRITE (MSGTXT,1010) IT, F0, GN
         CALL MSGWRT (3)
         END IF
      IF (NPR.GT.1) THEN
         WRITE (MSGTXT,1020)
         DO 105 I = 1,N,5
            I1 = I + 4
            I1 = MIN (I1, N)
            WRITE (MSGTXT,1021) (X0(II), II = I,I1)
            CALL MSGWRT (3)
 105        CONTINUE
         END IF
      IF (IT.LT.ITMAX) GO TO 110
         IER = 1
         GO TO 900
 110  DO 120 I = 1, N
         S(I) = -K0(I)
 120     CONTINUE
      CALL DDOT (N, K0, 1, S, 1, F0P)
      LAMBDA = 2.0D0
      IF (4.0D0*F0.GE.-F0P) GO TO 200
         XX = -4.0D0*F0/F0P
         DO 130 I = 1, N
            S(I) = XX*S(I)
 130        CONTINUE
         F0P = -4.0D0*F0
C                                       Step 2:
 200  DO 220 I = 1,N
         DO 210 J = 1,N
            AX(J) = XJ(I,J)
 210        CONTINUE
         CALL DDOT (N, AX, 1, S, 1, XDDOT)
         X(I) = X0(I) + XDDOT
 220     CONTINUE
      IF (-F0P.GE.EPS) GO TO 230
         IER = 0
         GO TO 900
 230  CALL FX (X, F, G, 1)
      NF = NF+1
      IF (F.LT.F0) GO TO  300
         DO 240 I = 1,N
            S(I) = 0.5D0*S(I)
 240        CONTINUE
         F0P = 0.5D0*F0P
         LAMBDA = 0.5D0
         GO TO 200
C                                       Step 3:
 300  CALL FX (X, F, G, 2)
      NF = NF+1
      NG = NG+1
      DO 320 I = 1,N
         DO 310 J = 1,N
            AX(J) = XJ(J,I)
 310        CONTINUE
         CALL DDOT (N, AX, 1, G, 1, K(I))
         M(I) = S(I)+K0(I)-K(I)
         K0(I) = K(I)
         X0(I) = X(I)
 320     CONTINUE
      CALL DDOT (N, K, 1, S, 1, FP)
      B0 = FP-F0P
      F0 = F
      F0P = FP
      IF (B0.GE.EPS) GO TO 400
         DO 330 I = 1,N
            S(I) = LAMBDA*S(I)
 330        CONTINUE
         F0P = LAMBDA*F0P
         GO TO 200
C                                       Step 4:
 400  CALL DNRM2 (N, M, 1, MSQ)
      MSQ = MSQ * MSQ
      IF (MSQ.LT.EPS) GO TO 100
         CALL DDOT (N, M, 1, S, 1, NU)
         MU = NU-MSQ
         CALL DDOT (N, M, 1, WUN, 1, XX)
         XX = XX / MSQ
         DO 410 I = 1,N
            WUN(I) = WUN(I)-XX*M(I)
 410        CONTINUE
         CALL DNRM2 (N, WUN, 1, UTU)
         UTU = UTU * UTU
C                                       Trap trouble
         UTU = MAX (UTU, EPS)
         CALL DDOT (N, M, 1, WUN, 1, XX)
         IF (XX.LT.TINYC .OR. ((1D3*XX)**2.LT.MSQ*UTU)) GO TO 450
            DO 420 I = 1,N
               WUN(I) = 0.0D0
 420           CONTINUE
            NSQ = 0.0D0
            GO TO 500
C                                       Step 4A:
 450     CALL DDOT (N, WUN, 1, S, 1, UTS)
         XX = UTS / UTU
         DO 460 I = 1,N
            WUN(I) = XX * WUN(I)
 460        CONTINUE
         NSQ = UTS * XX
C                                       Step 5:
 500  XX = NU / MSQ
      B = NSQ + MU*XX
      IF (B.GE.EPS) GO TO 600
         DO 510 I = 1,N
            WUN(I) = S(I) - XX*M(I)
 510        CONTINUE
         NSQ = B0 - MU*XX
         B = B0
C                                       Step 6:
 600  IF (MU*NU.LT.MSQ*NSQ) GO TO 650
         GAMMA = 0.0D0
         DELTA = SQRT (NU/MU)
         GO TO 700
C                                       Step 6A:
 650  A = B-MU
      C = B+NU
      GAMMA = SQRT ((1.0D0 - MU*NU/(MSQ*NSQ))/(A*B))
      DELTA = SQRT (C / A)
      IF (C.LT.A) GAMMA = -GAMMA
C                                       Step 7:
 700  XX = NSQ * GAMMA
      ALF = NU + MU*DELTA + MSQ*XX
      T1 = DELTA - XX
      T2 = GAMMA * NU
      T3 = (1.0D0+XX) / ALF
      T4 = -GAMMA * MU / ALF
      XX = MU * NU / ALF
      T5 = NSQ * (1.0D0 + GAMMA*XX)
      T6 = -(1.0D0 + DELTA) * XX
      DO 710 I = 1,N
         P(I) = T1*M(I) + T2*WUN(I)
         Q(I) = T3*M(I) + T4*WUN(I)
         WUN(I) = T5*M(I) + T6*WUN(I)
 710     CONTINUE
      CALL DDOT (N, Q, 1, K0, 1, QTK0)
      DO 730 I = 1,N
         K0(I) = K0(I) + QTK0*P(I)
         DO 720 L = 1,N
            AX(L) = XJ(I,L)
 720        CONTINUE
         CALL DDOT (N, AX, 1, Q, 1, XX)
         DO 730 J = 1,N
            XJ(I,J) = XJ(I,J) + XX*P(J)
 730        CONTINUE
      IF (NSQ.GT.0.0D0) GO TO 100
         DO 740 I = 1,N
            WUN(I) = K0(I)
 740        CONTINUE
         GO TO 100
C                                       Exit:
 900  DO 920 I = 1,N
         XI(I) = X0(I)
         DO 910 J = 1,N
            AX(J) = XJ(I,J)
 910        CONTINUE
         CALL DNRM2 (N, AX, 1, ERR(I))
 920     CONTINUE
      FOPT = F0
      GNOPT = GN
      IF (NPR.LE.0) GO TO 999
         IF (IER.EQ.0) THEN
            WRITE (MSGTXT,1030)
            CALL MSGWRT (3)
         ELSE IF (IER.EQ.1) THEN
            WRITE (MSGTXT,1040)
            CALL MSGWRT (3)
            END IF
         WRITE (MSGTXT,1050) NF,NG
         CALL MSGWRT (3)
         WRITE (MSGTXT,1060)
         CALL MSGWRT (3)
         DO 930 I = 1,N,5
            I1 = I + 4
            I1 = MIN (I1, N)
            WRITE (MSGTXT,1061) (XI(II), II = I,I1)
            CALL MSGWRT (3)
 930        CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Iteration #',I4,4X,'F=',1PD16.8,' Gradient=',1PD16.8)
 1020 FORMAT ('Parameters:')
 1021 FORMAT (1P5D12.4)
 1030 FORMAT ('***  Convergence achieved.')
 1040 FORMAT ('***  Maximum number of iterations reached.')
 1050 FORMAT (I4,' Function evaluations and ',I4,
     *   ' Gradient evaluations.')
 1060 FORMAT ('Solution parameters:')
 1061 FORMAT (1P5D12.4)
      END
