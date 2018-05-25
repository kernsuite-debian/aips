C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003
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
C  subroutine:  fftv2
C-------------------------------------------------------------------
C  a new version of Singleton's fft
C  Multivariate complex fourier transform, computed in place
C  using mixed-radix fast fourier transform algorithm.
C
      SUBROUTINE FFTV2 (A, B, NSEG, N, NSPN, ISN)
C
C  Arrays a and b originally hold the real and imaginary
C     components of the data, and return the real and
C     imaginary components of the resulting fourier coefficients.
C  Multivariate data is indexed according to the Fortran
C     array element successor function, without limit on the
C     number of implied multiple subscripts.  The subroutine is
C     called once for each variate.  The calls for a multivariate
C     transform may be in any order.
C
C  n is the dimension of the current variable.
C  nspn is the spacing of consecutive data values
C     while indexing the current variable.
C  nseg*n*nspn is the total number of complex data values.
C  The sign of isn determines the sign of the complex
C     exponential, and the magnitude of isn is normally one.
C     The magnitude of isn determines the indexing increment
C     for a and b.
C
C  If fftv2 is called twice, with opposite signs on isn, an identity
C     transformation is done...calls can be in either order.  The
C     results are scaled by 1/n when the sign of isn is positive.
C
C  A tri-variate transform with a(n1,n2,n3), b(n1,n2,n3)
C     is computed by
C        call fftv2(a,b,n2*n3,n1,1,-1)
C        call fftv2(a,b,n3,n2,n1,-1)
C        call fftv2(a,b,1,n3,n1*n2,-1)
C
C  A single-variate transform of n complex data values is computed by
C        call fftv2(a,b,1,n,1,-1)
C
C  The data may alternatively be stored in a single complex array a,
C     then the magnitude of isn changed to two to give the correct
C     indexing increment and a(2) used to pass the initial address
C     for the sequence of imaginary values, e.g.
C        call fftv2(a,a(2),nseg,n,nspn,-2)
C
C  Array nfac is working storage for factoring n.  The smallest
C     number exceeding the 15 locations provided is 12,754,584.
C
C***********************************************************************
C
C  MODIFICATIONS:
C                Name was changed to fftv2 to avoid clashing with
C                current use of fft for the old version.
C
C                Block data routine has been incorporated into the
C                main subroutine fftv2
C
C            1987 April 21 -- Don McLean
C                The four lines of code starting at label 110 modified
C                to calculate the maximum factor (used for stack
C                allocation correctly. To see the difference try a
C                transform of length 392, 588, or 6272.
C
C           1990 April 4 -- H.May
C               Added variable declarations to make fortran standard
C                enough to run on SUNs.
C                Also put machine dependent data in include FFTV2.INC
C***********************************************************************
C
      REAL A(*), B(*)
      DOUBLE PRECISION DSTAK(2500)
      INTEGER ISTAK(5000)
      INTEGER M, N, K, J, JJ, J2, J3, KT, NF, NSPAN, NSPN, NSEG,
     *     NTOT, ISN, IERR, I1MACH, NFAC(15), MAXP, MAXF
      REAL RSTAK(5000)
      COMMON /CSTAK/ DSTAK
C***********************************************************************
C
C   This code used to be a block data subroutine
C   ILLEGAL TO DATA VARIABLES IN COMMON
C
      INTEGER ISIZE(5)
      INTEGER LOUT, LNOW, LUSED, LMAX, LBOOK, ISTKGT
C
      EQUIVALENCE (DSTAK(1),ISTAK(1),RSTAK(1))
      EQUIVALENCE (ISTAK(1),LOUT)
      EQUIVALENCE (ISTAK(2),LNOW)
      EQUIVALENCE (ISTAK(3),LUSED)
      EQUIVALENCE (ISTAK(4),LMAX)
      EQUIVALENCE (ISTAK(5),LBOOK)
      EQUIVALENCE (ISTAK(6),ISIZE(1))
C
C      DATA INIDAT /1,1,1,2,2, 0,10,10,5000,10/
C
C**********************************************************************
C                                       ILLEGAL TO DATA VARIABLES IN
C                                       COMMON
      ISIZE(1) = 1
      ISIZE(2) = 1
      ISIZE(3) = 1
      ISIZE(4) = 2
      ISIZE(5) = 2
      LOUT = 0
      LNOW = 10
      LUSED = 10
      LMAX = 5000
      LBOOK = 10
C
C  Determine the factors of n
C
      M = 0
      NF = IABS(N)
      K = NF
      IF (NF.EQ.1) RETURN
      NSPAN = IABS(NF*NSPN)
      NTOT = IABS(NSPAN*NSEG)
      IF (ISN*NTOT.NE.0) GO TO 20
      IERR = I1MACH(4)
      WRITE (IERR,9999) NSEG, N, NSPN, ISN
9999  FORMAT (' error - zero in fftv2 parameters', 4I10)
      RETURN
C
10    M = M +1
      NFAC(M) = 4
      K = K/16
20    IF (K-(K/16)*16.EQ.0) GO TO 10
      J = 3
      JJ = 9
      GO TO 40
30    M = M + 1
      NFAC(M) = J
      K = K/JJ
40    IF (MOD(K,JJ).EQ.0) GO TO 30
      J = J + 2
      JJ = J**2
      IF (JJ.LE.K) GO TO 40
      IF (K.GT.4) GO TO 50
      KT = M
      NFAC(M+1) = K
      IF (K.NE.1) M = M +1
      GO TO 90
50    IF (K-(K/4)*4.NE.0) GO TO 60
      M = M + 1
      NFAC(M) = 2
      K = K/4
C  all square factors out now, but k .ge. 5 still
60    KT = M
      MAXP = MAX0(KT+KT+2,K-1)
      J = 2
70    IF (MOD(K,J).NE.0) GO TO 80
      M = M + 1
      NFAC(M) = J
      K = K/J
80    J = ((J+1)/2)*2 + 1
      IF (J.LE.K) GO TO 70
90    IF (M.LE.KT+1) MAXP = M + KT + 1
      IF (M+KT.GT.15) GO TO 120
      IF (KT.EQ.0) GO TO 110
      J = KT
100   M = M + 1
      NFAC(M) = NFAC(J)
      J = J -1
      IF (J.NE.0) GO TO 100
C
C     Find maximum factor
C     (Next 4 code lines modified, Don McLean 1987 April 21)
C
110   MAXF = 0
      DO 115 J=1,M
         MAXF = MAX0(NFAC(J),MAXF)
 115     CONTINUE
      J = ISTKGT(MAXF*4,3)
      JJ = J + MAXF
      J2 = JJ + MAXF
      J3 = J2 + MAXF
      K = ISTKGT(MAXP,2)
      CALL FFTMX(A, B, NTOT, NF, NSPAN, ISN, M, KT,
     *  RSTAK(J),RSTAK(JJ), RSTAK(J2), RSTAK(J3), ISTAK(K), NFAC)
      CALL ISTKRL(2)
      RETURN
C
120   IERR = I1MACH(4)
      WRITE (IERR,9998) N
9998  FORMAT (' error - fftv2 parameter n has more than 15 '
     *  , 'factors-',I20)
      RETURN
      END
C
C  subroutine:  fftmx
C  called by subroutine 'fftv2' to compute mixed-radix fourier
C  transform
C
      SUBROUTINE FFTMX(A, B, NTOT, N, NSPAN, ISN, M, KT, AT, CK,
     *  BT,SK, NP, NFAC)
C
      INTEGER NP(*), NFAC(*), J, NTOT, N, NSPAN, ISN, M, KT,
     *  INC, JJ, KK, K1, KLIM, NN, JC, MAXF, KSPAN, K2, K3, K4,
     *  NT, KS, KSPNN, LIM, I, MM, MIN0, K, JF

      REAL A(*), B(*), AT(*), CK(*), BT(*), SK(*), DR,
     *  C72, S72, S120, RAD, AA, BB, C1, C2, S1, S2, S3,
     *  AKP, BKP, AKM, BKM, AK, BK, AJP, BJP, AJM, BJM, AJ, BJ,
     *  CD, SD, C3
C
      INC = IABS(ISN)
      NT = INC*NTOT
      KS = INC*NSPAN
      RAD = ATAN(1.0)
      S72 = RAD/0.625
      C72 = COS(S72)
      S72 = SIN(S72)
      S120 = SQRT(0.75)
      IF (ISN.GT.0) GO TO 10
      S72 = -S72
      S120 = -S120
      RAD = -RAD
      GO TO 30
C
C  scale by 1/n for isn .gt. 0
C
10    AK = 1.0/FLOAT(N)
      DO 20 J=1,NT,INC
      A(J) = A(J)*AK
      B(J) = B(J)*AK
20    CONTINUE
C
30    KSPAN = KS
      NN = NT - INC
      JC = KS/N
C
C  sin, cos values are re-initialised each lim steps
C
      LIM = 32
      KLIM = LIM*JC
      I = 0
      JF = 0
      MAXF = M - KT
      MAXF = NFAC(MAXF)
      IF (KT.GT.0) MAXF = MAX0(NFAC(KT),MAXF)
C
C  compute fourier transform
C
40    DR = 8.0*FLOAT(JC)/FLOAT(KSPAN)
      CD = 2.0*SIN(0.5*DR*RAD)**2
      SD = SIN(DR*RAD)
      KK = 1
      I = I + 1
      IF (NFAC(I).NE.2) GO TO 110
C
C  transform for factor of 2 (including rotation factor)
C
      KSPAN = KSPAN/2
      K1 = KSPAN + 2
50    K2 = KK + KSPAN
      AK = A(K2)
      BK = B(K2)
      A(K2) = A(KK) - AK
      B(K2) = B(KK) - BK
      A(KK) = A(KK) + AK
      B(KK) = B(KK) + BK
      KK = K2 + KSPAN
      IF (KK.LE.NN) GO TO 50
      KK = KK - NN
      IF (KK.LE.JC) GO TO 50
      IF (KK.GT.KSPAN) GO TO 350
60    C1 = 1.0 - CD
      S1 = SD
      MM = MIN0(K1/2,KLIM)
      GO TO 80
70    AK = C1 - (CD*C1+SD*S1)
      S1 = (SD*C1-CD*S1) + S1
C
C  the following three statements compensate for truncation
C     error.  If rounded arithmetic is used, substitute
C     c1=ak
C
      C1 = 0.5/(AK**2+S1**2) + 0.5
      S1 = C1*S1
      C1 = C1*AK
80    K2 = KK + KSPAN
      AK = A(KK) - A(K2)
      BK = B(KK) - B(K2)
      A(KK) = A(KK) + A(K2)
      B(KK) = B(KK) + B(K2)
      A(K2) = C1*AK - S1*BK
      B(K2) = S1*AK + C1*BK
      KK = K2 + KSPAN
      IF (KK.LT.NT) GO TO 80
      K2 = KK - NT
      C1 = -C1
      KK = K1 - K2
      IF (KK.GT.K2) GO TO 80
      KK = KK + JC
      IF (KK.LE.MM) GO TO 70
      IF (KK.LT.K2) GO TO 90
      K1 = K1 + INC + INC
      KK = (K1-KSPAN)/2 + JC
      IF (KK.LE.JC+JC) GO TO 60
      GO TO 40
90    S1 = FLOAT((KK-1)/JC)*DR*RAD
      C1 = COS(S1)
      S1 = SIN(S1)
      MM = MIN0(K1/2,MM+KLIM)
      GO TO 80
C
C  transform for factor of 3 (optional code)
C
100   K1 = KK + KSPAN
      K2 = K1 + KSPAN
      AK = A(KK)
      BK = B(KK)
      AJ = A(K1) + A(K2)
      BJ = B(K1) + B(K2)
      A(KK) = AK + AJ
      B(KK) = BK + BJ
      AK = -0.5*AJ + AK
      BK = -0.5*BJ + BK
      AJ = (A(K1)-A(K2))*S120
      BJ = (B(K1)-B(K2))*S120
      A(K1) = AK - BJ
      B(K1) = BK + AJ
      A(K2) = AK + BJ
      B(K2) = BK - AJ
      KK = K2 + KSPAN
      IF (KK.LT.NN) GO TO 100
      KK = KK - NN
      IF (KK.LE.KSPAN) GO TO 100
      GO TO 290
C
C  transform for factor of 4

110   IF (NFAC(I).NE.4) GO TO 230
      KSPNN = KSPAN
      KSPAN = KSPAN/4
120   C1 = 1.0
      S1 = 0.
      MM = MIN0(KSPAN,KLIM)
      GO TO 150
130   C2 = C1 - (CD*C1+SD*S1)
      S1 = (SD*C1-CD*S1) + S1
C
C  the following three statements compensate for truncation
C     error.  if rounded arithmetic is used, substitute
C     c1=c2
C
      C1 = 0.5/(C2**2+S1**2) + 0.5
      S1 = C1*S1
      C1 = C1*C2
140   C2 = C1**2 - S1**2
      S2 = C1*S1*2.0
      C3 = C2*C1 - S2*S1
      S3 = C2*S1 + S2*C1
150   K1 = KK + KSPAN
      K2 = K1 + KSPAN
      K3 = K2 + KSPAN
      AKP = A(KK) + A(K2)
      AKM = A(KK) - A(K2)
      AJP = A(K1) + A(K3)
      AJM = A(K1) - A(K3)
      A(KK) = AKP + AJP
      AJP = AKP - AJP
      BKP = B(KK) + B(K2)
      BKM = B(KK) - B(K2)
      BJP = B(K1) + B(K3)
      BJM = B(K1) - B(K3)
      B(KK) = BKP + BJP
      BJP = BKP - BJP
      IF (ISN.LT.0) GO TO 180
      AKP = AKM - BJM
      AKM = AKM + BJM
      BKP = BKM + AJM
      BKM = BKM - AJM
      IF (S1.EQ.0.0) GO TO 190
160   A(K1) = AKP*C1 - BKP*S1
      B(K1) = AKP*S1 + BKP*C1
      A(K2) = AJP*C2 - BJP*S2
      B(K2) = AJP*S2 + BJP*C2
      A(K3) = AKM*C3 - BKM*S3
      B(K3) = AKM*S3 + BKM*C3
      KK = K3 + KSPAN
      IF (KK.LE.NT) GO TO 150
170   KK = KK - NT + JC
      IF (KK.LE.MM) GO TO 130
      IF (KK.LT.KSPAN) GO TO 200
      KK = KK - KSPAN + INC
      IF (KK.LE.JC) GO TO 120
      IF (KSPAN.EQ.JC) GO TO 350
      GO TO 40
180   AKP = AKM + BJM
      AKM = AKM - BJM
      BKP = BKM - AJM
      BKM = BKM + AJM
      IF (S1.NE.0.0) GO TO 160
190   A(K1) = AKP
      B(K1) = BKP
      A(K2) = AJP
      B(K2) = BJP
      A(K3) = AKM
      B(K3) = BKM
      KK = K3 + KSPAN
      IF (KK.LE.NT) GO TO 150
      GO TO 170
200   S1 = FLOAT((KK-1)/JC)*DR*RAD
      C1 = COS(S1)
      S1 = SIN(S1)
      MM = MIN0(KSPAN,MM+KLIM)
      GO TO 140
C
C  transform for factor of 5 (optional code)
C
210   C2 = C72**2 - S72**2
      S2 = 2.0*C72*S72
220   K1 = KK + KSPAN
      K2 = K1 + KSPAN
      K3 = K2 + KSPAN
      K4 = K3 + KSPAN
      AKP = A(K1) + A(K4)
      AKM = A(K1) - A(K4)
      BKP = B(K1) + B(K4)
      BKM = B(K1) - B(K4)
      AJP = A(K2) + A(K3)
      AJM = A(K2) - A(K3)
      BJP = B(K2) + B(K3)
      BJM = B(K2) - B(K3)
      AA = A(KK)
      BB = B(KK)
      A(KK) = AA + AKP + AJP
      B(KK) = BB + BKP + BJP
      AK = AKP*C72 + AJP*C2 + AA
      BK = BKP*C72 + BJP*C2 + BB
      AJ = AKM*S72 + AJM*S2
      BJ = BKM*S72 + BJM*S2
      A(K1) = AK - BJ
      A(K4) = AK + BJ
      B(K1) = BK + AJ
      B(K4) = BK - AJ
      AK = AKP*C2 + AJP*C72 + AA
      BK = BKP*C2 + BJP*C72 + BB
      AJ = AKM*S2 - AJM*S72
      BJ = BKM*S2 - BJM*S72
      A(K2) = AK - BJ
      A(K3) = AK + BJ
      B(K2) = BK + AJ
      B(K3) = BK - AJ
      KK = K4 + KSPAN
      IF (KK.LT.NN) GO TO 220
      KK = KK - NN
      IF (KK.LE.KSPAN) GO TO 220
      GO TO 290
C
C  transform for odd factors
C
230   K = NFAC(I)
      KSPNN = KSPAN
      KSPAN = KSPAN/K
      IF (K.EQ.3) GO TO 100
      IF (K.EQ.5) GO TO 210
      IF (K.EQ.JF) GO TO 250
      JF = K
      S1 = RAD/(FLOAT(K)/8.0)
      C1 = COS(S1)
      S1 = SIN(S1)
      CK(JF) = 1.0
      SK(JF) = 0.0
      J = 1
240   CK(J) = CK(K)*C1 + SK(K)*S1
      SK(J) = CK(K)*S1 - SK(K)*C1
      K = K - 1
      CK(K) = CK(J)
      SK(K) = -SK(J)
      J = J + 1
      IF (J.LT.K) GO TO 240
250   K1 = KK
      K2 = KK + KSPNN
      AA = A(KK)
      BB = B(KK)
      AK = AA
      BK = BB
      J = 1
      K1 = K1 + KSPAN
260   K2 = K2 -KSPAN
      J = J + 1
      AT(J) = A(K1) + A(K2)
      AK = AT(J) + AK
      BT(J) = B(K1) + B(K2)
      BK = BT(J) + BK
      J = J + 1
      AT(J) = A(K1) - A(K2)
      BT(J) = B(K1) - B(K2)
      K1 = K1 + KSPAN
      IF (K1.LT.K2) GO TO 260
      A(KK) = AK
      B(KK) = BK
      K1 = KK
      K2 = KK + KSPNN
      J = 1
270   K1 = K1 + KSPAN
      K2 = K2 - KSPAN
      JJ = J
      AK = AA
      BK = BB
      AJ = 0.0
      BJ = 0.0
      K = 1
280   K = K + 1
      AK = AT(K)*CK(JJ) + AK
      BK = BT(K)*CK(JJ) + BK
      K = K + 1
      AJ = AT(K)*SK(JJ) + AJ
      BJ = BT(K)*SK(JJ) + BJ
      JJ = JJ + J
      IF (JJ.GT.JF) JJ = JJ - JF
      IF (K.LT.JF) GO TO 280
      K = JF - J
      A(K1) = AK - BJ
      B(K1) = BK + AJ
      A(K2) = AK + BJ
      B(K2) = BK - AJ
      J = J + 1
      IF (J.LT.K) GO TO 270
      KK = KK + KSPNN
      IF (KK.LE.NN) GO TO 250
      KK = KK - NN
      IF (KK.LE.KSPAN) GO TO 250
C
C  multiply by rotation factor (except for factors of 2 and 4)

290   IF (I.EQ.M) GO TO 350
      KK = JC + 1
300   C2 = 1.0 - CD
      S1 = SD
      MM = MIN0(KSPAN,KLIM)
      GO TO 320
310   C2 = C1 - (CD*C1+SD*S1)
      S1 = S1 + (SD*C1-CD*S1)
C
C  the following three statements compensate for truncation
C  error.  If rounded arithmetic is used, they may be deleted.
C
      C1 = 0.5/(C2**2+S1**2) + 0.5
      S1 = C1*S1
      C2 = C1*C2
320   C1 = C2
      S2 = S1
      KK = KK + KSPAN
330   AK = A(KK)
      A(KK) = C2*AK - S2*B(KK)
      B(KK) = S2*AK + C2*B(KK)
      KK = KK + KSPNN
      IF (KK.LE.NT) GO TO 330
      AK = S1*S2
      S2 = S1*C2 + C1*S2
      C2 = C1*C2 - AK
      KK = KK - NT + KSPAN
      IF (KK.LE.KSPNN) GO TO 330
      KK = KK - KSPNN + JC
      IF (KK.LE.MM) GO TO 310
      IF (KK.LT.KSPAN) GO TO 340
      KK = KK - KSPAN + JC + INC
      IF (KK.LE.JC+JC) GO TO 300
      GO TO 40
340   S1 = FLOAT((KK-1)/JC)*DR*RAD
      C2 = COS(S1)
      S1 = SIN(S1)
      MM = MIN0(KSPAN,MM+KLIM)
      GO TO 320
C
C  permute the results to normal order...done in two stages
C  permutation for square factors of n
C
350   NP(1) = KS
      IF (KT.EQ.0) GO TO 440
      K = KT + KT + 1
      IF (M.LT.K) K = K - 1
      J = 1
      NP(K+1) = JC
360   NP(J+1) = NP(J)/NFAC(J)
      NP(K) = NP(K+1)*NFAC(J)
      J = J + 1
      K = K - 1
      IF (J.LT.K) GO TO 360
      K3 = NP(K+1)
      KSPAN = NP(2)
      KK = JC + 1
      K2 = KSPAN + 1
      J = 1
      IF (N.NE.NTOT) GO TO 400
C
C  permutation for single-variate transform (optional code)
C
370   AK = A(KK)
      A(KK) = A(K2)
      A(K2) = AK
      BK = B(KK)
      B(KK) = B(K2)
      B(K2) = BK
      KK = KK + INC
      K2 = KSPAN + K2
      IF (K2.LT.KS) GO TO 370
380   K2 = K2 - NP(J)
      J = J + 1
      K2 = NP(J+1) + K2
      IF (K2.GT.NP(J)) GO TO 380
      J = 1
390   IF (KK.LT.K2) GO TO 370
      KK = KK + INC
      K2 = KSPAN + K2
      IF (K2.LT.KS) GO TO 390
      IF (KK.LT.KS) GO TO 380
      JC = K3
      GO TO 440
C
C  permutation for multivariate transform
C
400   K = KK + JC
410   AK = A(KK)
      A(KK) = A(K2)
      A(K2) = AK
      BK = B(KK)
      B(KK) = B(K2)
      B(K2) = BK
      KK = KK + INC
      K2 = K2 + INC
      IF (KK.LT.K) GO TO 410
      KK = KK + KS - JC
      K2 = K2 + KS - JC
      IF (KK.LT.NT) GO TO 400
      K2 = K2 - NT + KSPAN
      KK = KK - NT + JC
      IF (K2.LT.KS) GO TO 400
420   K2 = K2 - NP(J)
      J = J + 1
      K2 = NP(J+1) + K2
      IF (K2.GT.NP(J)) GO TO 420
      J = 1
430   IF (KK.LT.K2) GO TO 400
      KK = KK + JC
      K2 = KSPAN + K2
      IF (K2.LT.KS) GO TO 430
      IF (KK.LT.KS) GO TO 420
      JC = K3
440   IF (2*KT+1.GE.M) RETURN
      KSPNN = NP(KT+1)
C
C  permutation for square-free factors of n
C
      J = M - KT
      NFAC(J+1) = 1
450   NFAC(J) = NFAC(J)*NFAC(J+1)
      J = J - 1
      IF (J.NE.KT) GO TO 450
      KT = KT + 1
      NN = NFAC(KT) - 1
      JJ = 0
      J = 0
      GO TO 480
460   JJ = JJ - K2
      K2 = KK
      K = K + 1
      KK = NFAC(K)
470   JJ = KK + JJ
      IF (JJ.GE.K2) GO TO 460
      NP(J) = JJ
480   K2 = NFAC(KT)
      K = KT + 1
      KK = NFAC(K)
      J = J + 1
      IF (J.LE.NN) GO TO 470
C
C  determine the permutation cycles of length greater than 1
C
      J = 0
      GO TO 500
490   K = KK
      KK = NP(K)
      NP(K) = -KK
      IF (KK.NE.J) GO TO 490
      K3 = KK
500   J = J + 1
      KK = NP(J)
      IF (KK.LT.0) GO TO 500
      IF (KK.NE.J) GO TO 490
      NP(J) = -J
      IF (J.NE.NN) GO TO 500
      MAXF = INC*MAXF
C
C  reorder a and b, following the permutation cycles
C
      GO TO 570
510   J = J - 1
      IF (NP(J).LT.0) GO TO 510
      JJ = JC
520   KSPAN = JJ
      IF (JJ.GT.MAXF) KSPAN = MAXF
      JJ = JJ - KSPAN
      K = NP(J)
      KK = JC*K + I + JJ
      K1 = KK + KSPAN
      K2 = 0
530   K2 = K2 + 1
      AT(K2) = A(K1)
      BT(K2) = B(K1)
      K1 = K1 - INC
      IF (K1.NE.KK) GO TO 530
540   K1 = KK + KSPAN
      K2 = K1 - JC*(K+NP(K))
      K = -NP(K)
550   A(K1) = A(K2)
      B(K1) = B(K2)
      K1 = K1 - INC
      K2 = K2 - INC
      IF (K1.NE.KK) GO TO 550
      KK = K2
      IF (K.NE.J) GO TO 540
      K1 = KK + KSPAN
      K2 = 0
560   K2 = K2 + 1
      A(K1) = AT(K2)
      B(K1) = BT(K2)
      K1 = K1 - INC
      IF (K1.NE.KK) GO TO 560
      IF (JJ.NE.0) GO TO 520
      IF (J.NE.1) GO TO 510
570   J = K3 + 1
      NT = NT - KSPNN
      I = NT - INC + 1
      IF (NT.GE.0) GO TO 510
      RETURN
      END
C
C  subroutine:  reals
C  used with 'fftv2' to compute fourier transform or inverse for
C  real data.
C
      SUBROUTINE REALS(A, B, N, ISN)
C
C  if isn=-1, this subroutine completes the fourier transform
C    of 2*n real data values, where the original data values are
C    stored alternatively in arrays a and b, and are first
C    transformed by a complex fourier transform of dimension n.
C    the cosine coefficients are in a(1),a(2),...a(n),a(n+1)
C    and the sine coefficients are in b(1),b(2),...b(n),b(n+1).
C    note that the arrays a and b must have dimension n+1.
C    a typical calling sequence is
C        call fftv2(a,b,n,n,n,-1)
C        call reals(a,b,n,-1)
C
C  if isn=1, the inverse transformation is done, the first
C    step in evaluating a real fourier series.
C    A typical calling sequence is
C        call reals(a,b,n,1)
C        call fftv2(a,b,n,n,n,1)
C    The time domain results alternate in arrays a and b,
C    i.e. a(1),b(1),a(2),b(2),...a(n),b(n).
C
C  The data may alternatively be stored in a single complex
C    array a, then the magnitude of isn changed to two to
C    give the correct indexing increment and a(2) used to
C    pass the initial address for the sequence of imaginary
C    values, e.g.
C        call fftv2(a,a(2),n,n,n,-2)
C        call reals(a,a(2),n,-2)
C    In this case, the cosine and sine coefficients alternate in a.
C
      REAL A(*), B(*), RAD, DR, CD, SD, SN, CN, AA, AB, BA, BB,
     *  RE, EM
      INTEGER ISN, N, INC, NF, IERR, NH, LIM, MM, ML, J, NK,
     *  I1MACH, K

      INC = IABS(ISN)
      NF = IABS(N)
      IF (NF*ISN.NE.0) GO TO 10
      IERR = I1MACH(4)
      WRITE (IERR,9999) N, ISN
9999  FORMAT (' error - zero in reals parameters', 2I10)
      RETURN
C
10    NK = NF*INC + 2
      NH = NK/2
      RAD = ATAN(1.0)
      DR = -4.0/FLOAT(NF)
      CD = 2.0*SIN(0.5*DR*RAD)**2
      SD = SIN(DR*RAD)
C
C  sin,cos values are re-initialised each lim steps
C
      LIM = 32
      MM = LIM
      ML = 0
      SN = 0.0
      IF (ISN.GT.0) THEN
40       CN = -1.0
         SD = -SD
      ELSE
         CN = 1.0
         A(NK-1) = A(1)
         B(NK-1) = B(1)
         END IF
20    DO 30 J=1,NH,INC
      K = NK -J
      AA = A(J) + A(K)
      AB = A(J) - A(K)
      BA = B(J) + B(K)
      BB = B(J) - B(K)
      RE = CN*BA + SN*AB
      EM = SN*BA - CN*AB
      B(K) = (EM-BB)*0.5
      B(J) = (EM+BB)*0.5
      A(K) = (AA-RE)*0.5
      A(J) = (AA+RE)*0.5
      ML = ML + 1
      IF (ML.EQ.MM) THEN
50       MM = MM + LIM
         SN = FLOAT(ML)*DR*RAD
         CN = COS(SN)
         IF (ISN.GT.0) CN = -CN
         SN = SIN(SN)
      ELSE
         AA = CN - (CD*CN+SD*SN)
         SN = (SD*CN-CD*SN) + SN
C
C  the following three statements compensate for truncation
C    error.  If rounded arithmetic is used, substitute
C    cn=aa
C
         CN = 0.5/(AA**2+SN**2) + 0.5
         SN = CN*SN
         CN = CN*AA
         END IF
30    CONTINUE
      RETURN
C
      END
C-----------------------------------------------------------------------
C  subroutine:  realt
C  used with 'fftv2' or any other complex fourier transform to compute
C    transform or inverse for real data
C    the data may be either single-variate or multi-variate
C
      SUBROUTINE REALT(A, B, NSEG, N, NSPN, ISN)
C
C  if isn=-1, this subroutine completes the fourier transform
C    of 2*n real data values, where the original data values are
C    stored alternately in arrays a and b, and are first
C    transformed by a complex fourier transform of dimension n.
C    the cosine coefficients are in a(1),a(2),...a(n),a(n+1)
C    and the sine coefficients are in b(1),b(2),...b(n),b(n+1).
C    note that the arrays a and b must have dimesnion n+1.
C    A typical calling sequence is
C        call fftv2(a,b,1,n,1,-1)
C        call realt(a,b,1,n,1,-1)
C
C  if isn=1, the inverse ransformation is done, the first
C    step in evaluating a real fourier series.
C    a typical calling sequence is
C        call realt(a,b,1,n,1,1)
C        call fftv2(a,b,1,n,1,1)
C    The time domain results alternate in arrays a and b,
C    i.e., a(1),b(1),a(2),b(2),...a(n),b(n).
C
C  the data may alternatively be stored in a single complex
C    array a, then the magnitude of isn changed to two to
C    give the correct indexing increment and a(2) used to
C    pass the initial address for the sequence of imaginary
C    values, e.g.
C        call fftv2(a,a(2),1,n,1,-2)
C        call realt(a,a(2),1,n,1,-2)
C    in this case, the cosine and sine coefficients alternate in a.
C
C  this subroutine is set up to do the above-described operation on
C    all sub-vectors within any dimension of a multi-dimensional
C    fourier transform.  The parameters nseg, n, nspn, and inc
C    should agree with those used in the associated call of 'fftv2'.
C    The folding frequency cosine coefficients are stored at the end
C    of array a (with zeros in the corresponding locations in array b),
C    in a sub-matrix of dimension one less than the main array.,  The
C    deleted dimension is that corresponding to the parameter n in
C    the call of realt.  Thus arrays a and b must have dimension
C    nseg*nspn*(n+1)
C-----------------------------------------------------------------------
      REAL A(*), B(*), RAD, DR, CD, SD, SN, CN, AA, AB, BA, BB,
     *  RE, EM
      INTEGER INC, ISN, N, NF, IERR, NH, LIM, MM, NT, NSPN, KS, NS,
     *   I1MACH, JC, K2, KD, NN, KK, KLIM, NSEG
C-----------------------------------------------------------------------
      INC = IABS(ISN)
      KS = IABS(NSPN)*INC
      NF = IABS(N)
      NS = KS*NF
      NT = IABS(NS*NSEG)
      IF (ISN*NT.NE.0) GO TO 10
      IERR = I1MACH(4)
      WRITE (IERR,9999) NSEG, N, NSPN, ISN
9999  FORMAT (' error - zero in realt parameters', 3I10, I9)
      RETURN
C
10    JC = KS
      K2 = IABS(KS*NSEG) - INC
      KD = NS
      NH = NS/2 + 1
      NN = NT - INC
      NT = NT + 1
      KK = 1
      RAD = ATAN(1.0)
      DR = -4.0/FLOAT(NF)
      CD = 2.0*SIN(0.5*DR*RAD)**2
      SD = SIN(DR*RAD)
C
C  sin,cos values are re-initialised each lim steps
C
      LIM = 32
      KLIM = LIM*KS
      MM = MIN0(NH,KLIM)
      SN = 0.0
      IF (ISN.GT.0) GO TO 70
C
20    AA = A(KK)
      BA = B(KK)
      B(KK) = 0.
      A(KK) = AA + BA
      A(NT) = AA - BA
      B(NT) = 0.
      NT = NT + JC
      KK = KK + NS
      IF (KK.LE.NN) GO TO 20
      NT = NT - K2
      KK = KK - NN
      IF (KK.LE.JC) GO TO 20
      CN = 1.0
30    IF (NF.EQ.1) RETURN
C
40    AA = CN - (CD*CN+SD*SN)
      SN = (SD*CN-CD*SN) + SN
C
C  the following three statements compensate for truncation
C    error.  If rounded arithmetic is used, substitute
C    cn=aa
C
      CN = 0.5/(AA**2+SN**2) + 0.5
      SN = CN*SN
      CN = CN*AA
50    JC = JC + KS
      KD = KD - KS - KS
60    K2 = KK + KD
      AA = A(KK) + A(K2)
      AB = A(KK) - A(K2)
      BA = B(KK) + B(K2)
      BB = B(KK) - B(K2)
      RE = CN*BA + SN*AB
      EM = SN*BA - CN*AB
      B(K2) = (EM-BB)*0.5
      B(KK) = (EM+BB)*0.5
      A(K2) = (AA-RE)*0.5
      A(KK) = (AA+RE)*0.5
      KK = KK + NS
      IF (KK.LE.NN) GO TO 60
      KK = KK - NN
      IF (KK.LE.JC) GO TO 60
      IF (KK.LE.MM) GO TO 40
      IF (KK.GT.NH) RETURN
      SN = FLOAT(JC/KS)*DR*RAD
      CN = COS(SN)
      IF (ISN.GT.0) CN = -CN
      SN = SIN(SN)
      MM = MIN0(NH,MM+KLIM)
      GO TO 50
C
70    AA = A(KK)
      BA = A(NT)
      A(KK) = (AA+BA)*0.5
      B(KK) = (AA-BA)*0.5
      NT = NT + JC
      KK = KK + NS
      IF (KK.LE.NN) GO TO 70
      NT = NT - K2
      KK = KK - NN
      IF (KK.LE.JC) GO TO 70
      CN = -1.0
      SD = -SD
      GO TO 30
      END
C
C  function: istkgt(nitems,itype)
C  allocates working storage for nitems of itype, as follows
C
C  1 - logical
C  2 - integer
C  3 - real
C  4 - double precision
C  5 - complex
C
      INTEGER FUNCTION ISTKGT(NITEMS, ITYPE)
C
      COMMON /CSTAK/ DSTAK(2500)
C
      DOUBLE PRECISION DSTAK
      INTEGER ISTAK(5000)
      INTEGER ISIZE(5)
      INTEGER LOUT, LNOW, LUSED, LMAX, LBOOK, NITEMS, IERR,
     *  I1MACH, ITYPE, I, J
C
      EQUIVALENCE (DSTAK(1),ISTAK(1))
      EQUIVALENCE (ISTAK(1),LOUT)
      EQUIVALENCE (ISTAK(2),LNOW)
      EQUIVALENCE (ISTAK(3),LUSED)
      EQUIVALENCE (ISTAK(4),LMAX)
      EQUIVALENCE (ISTAK(5),LBOOK)
      EQUIVALENCE (ISTAK(6),ISIZE(1))
C
      ISTKGT = (LNOW*ISIZE(2)-1)/ISIZE(ITYPE) + 2
      I = ((ISTKGT-1+NITEMS)*ISIZE(ITYPE)-1)/ISIZE(2) + 3
      IF (I.GT.LMAX) GO TO 10
      ISTAK(I-1) = ITYPE
      ISTAK(I) = LNOW
      LOUT = LOUT + 1
      LNOW = I
      LUSED = MAX0(LUSED,LNOW)
      RETURN
C
10    IERR = I1MACH(4)
      WRITE (IERR,9999) I
9999  FORMAT (' overflow of common array istak --- need ',I10)
      WRITE (IERR,9998) (ISTAK(J),J=1,10), ISTAK(LNOW-1), ISTAK(LNOW)
9998  FORMAT (12I6)
      STOP 'ISTKGT IN FFTV2 DIES'
      END
C
C  subroutine:  istkrl(k)
C  de-allocates the last k working storage areas
C
      SUBROUTINE ISTKRL(K)
C
      COMMON /CSTAK/ DSTAK(2500)
C
      DOUBLE PRECISION DSTAK
      INTEGER ISTAK(5000), LOUT, LNOW, LUSED, LMAX, LBOOK, IN,
     *  K, IERR, I1MACH, J
C
      EQUIVALENCE (DSTAK(1),ISTAK(1))
      EQUIVALENCE (ISTAK(1),LOUT)
      EQUIVALENCE (ISTAK(2),LNOW)
      EQUIVALENCE (ISTAK(3),LUSED)
      EQUIVALENCE (ISTAK(4),LMAX)
      EQUIVALENCE (ISTAK(5),LBOOK)
C
      IN = K
C
      IF (LBOOK.LE.LNOW .AND. LNOW.LE.LUSED .AND. LUSED.LE.LMAX) GO
     *  TO10
      IERR = I1MACH(4)
      WRITE (IERR,9999)
9999  FORMAT (' Warning...istak(2),istak(3),istak(4) or '
     *  , 'istak(5) hit')
      WRITE (IERR,9997) (ISTAK(J),J=1,10), ISTAK(LNOW-1), ISTAK(LNOW)
C
10    IF (IN.LE.0) RETURN
      IF (LBOOK.GT.ISTAK(LNOW) .OR. ISTAK(LNOW).GE.LNOW-1) GO TO 20
      LOUT = LOUT - 1
      LNOW = ISTAK(LNOW)
      IN = IN - 1
      GO TO 10
C
20    IERR = I1MACH(4)
      WRITE (IERR,9998)
9998  FORMAT (' Warning...pointer at istak(lnow) overwritten'/11X
     *,'de-allocation not completed')
      WRITE (IERR,9997) (ISTAK(J),J=1,10), ISTAK(LNOW-1), ISTAK(LNOW)
9997  FORMAT (12I6)
      RETURN
C
      END

      INTEGER FUNCTION I1MACH(I)
C  I/O unit numbers
C
      INTEGER IMACH(16),OUTPUT, I

      EQUIVALENCE (IMACH(4),OUTPUT)
C
      INCLUDE 'INCS:FFTV2.INC'

      IF(I .LT. 1 .OR. I .GT. 16)GO TO 10
      I1MACH=IMACH(I)
      RETURN
10    WRITE  (OUTPUT,9000)
9000  FORMAT ('1error    1 in i1mach : i out of bounds')
      STOP 'I1MACH IN FFTV2 DIES'
      END
