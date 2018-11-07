      SUBROUTINE QXFOUR (JC, N, ISIGN)
C-----------------------------------------------------------------------
C! Pseudo AP routine: FFT routine.
C# AP-FFT
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
C   Cooley-Tukey Fast Fourier Transform in USASI basic FORTRAN.
C   one-dimensional transform of complex data, arbitrary number of
C   points.  N points can be transformed in time proportional to
C   N*Log(N) (for N non-prime), whereas other methods take N**2 time.
C   furthermore, because fewer arithmetic operations are performed,
C   less error is built up.  The transform done is--
C     TRANSFORM(K) = SUM(DATA(J)*EXP(ISIGN*2*PI*I*(J-1)*(K-1)/N)),
C        summed from J = 1 to N for all K from 1 to N.
C     Inputs:
C       JC     I    Base address in /APCORE/ of vector to FFT (0-rel)
C       N      I    Length of complex vector to FFT in complex elements
C       ISIGN  I    +1 or -1 for reverse or forward transform.
C
C   Usage notes:
C   The transform is in-place, with output replacing the input.
C   N may be any positive number, but it should be non-prime for speed.
C   A ISIGN=-1 transform followed by a +1 one (or vice versa)
C   returns N times the original data.
C   Running time is proportional to N * (sum of the prime factors of
C   N).  For example, N = 1960, time is to * 1960 * (2+2+2+5+7+7).
C   naive methods directly implementing the summation run in time
C   proportional to N**2.  An upper bound for the RMS relative error
C   is 3 * 2**(-B) * sum(F**1.5), where B is the number of bits in
C   the floating point fraction and the sum is over the prime
C   factors of N.
C   Written by Norman Brenner, MIT, August 1968.  See--
C   IEEE Audio and Electroacoustics Transactions, June 1967 and
C   June 1969, special issues on the FFT.
C   CRI version.
C-----------------------------------------------------------------------
      INTEGER   IFACT(32)
      INTEGER   JC, N, ISIGN, I1, I2, I3, I2MAX, I3REV, ID, IDIV, IF,
     *   IFCUR,IP0, IP1, IP2, IP3, IQUOT, IWMAX, J0, J1, JWORK,
     *   NFACT, NPART
      REAL      FLOAT, ROOTI, ROOTR, SINTH, SUMI, SUMR, TEMPI, TEMPR,
     *   THETA, TWOPI, WI, WMINI, WMINR, WR, WSTPI, WSTPR, TEMP
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      TWOPI = 6.2831853072 * ISIGN
C                                       Factor N into its prime factors
C                                       NFACT in number.  For example,
C                                       for N = 1960, NFACT = 6 and
C                                       IFACT(IF) = 2, 2, 2, 5, 7 and 7.
      IF = 0
      NPART = N
      INCLUDE 'INCS:ZVND.INC'
      DO 50 ID = 1,N,2
         IDIV = ID
         IF (ID-1) 10,10,20
 10      IDIV = 2
 20      IQUOT = NPART / IDIV
         IF (NPART-IDIV*IQUOT) 40,30,40
 30      IF = IF + 1
         IFACT(IF) = IDIV
         NPART = IQUOT
         GO TO 20
 40      IF (IQUOT-IDIV) 60,60,50
 50      CONTINUE
 60   IF (NPART-1) 80,80,70
 70   IF = IF + 1
      IFACT(IF) = NPART
 80   NFACT = IF
C                                       Shuffle the data array by
C                                       reversing the digits of the
C                                       index. Replace DATA(I) by
C                                       DATA(IREV) for all I from 1
C                                       to N.  IREV-1 is the integer
C                                       whose digit representation in
C                                       the multi-radix notation of
C                                       factors IFACT(IF) is the
C                                       reverse of the representation
C                                       of I-1.  For example, if all
C                                       IFACT(IF)=2, then for
C                                       I-1 = 11001, IREV-1 =  10011.
C                                       A work array of length N is
C                                       needed.
      IP0 = 2
      IP3 = IP0 * N
      JWORK = 1
      I3REV = 1
      INCLUDE 'INCS:ZVND.INC'
      DO 110 I3 = 1,IP3,IP0
         WRKFFT(JWORK) = APCORE(JC+I3REV)
         WRKFFT(JWORK+1) = APCORE(JC+I3REV+1)
         IP2 = IP3
      INCLUDE 'INCS:ZVND.INC'
         DO 100 IF = 1,NFACT
            IP1 = IP2 / IFACT(IF)
            I3REV = I3REV + IP1
            IF (I3REV-IP2) 105,105,90
 90         I3REV = I3REV - IP2
            IP2 = IP1
 100        CONTINUE
 105     JWORK = JWORK + IP0
 110     CONTINUE
      JWORK = 1
      INCLUDE 'INCS:ZVND.INC'
      DO 120 I3 = 1,IP3,IP0
         APCORE(JC+I3) = WRKFFT(JWORK)
         APCORE(JC+I3+1) = WRKFFT(JWORK+1)
         JWORK = JWORK + IP0
 120     CONTINUE
C                                       Phase-shifted Fourier transform
C                                       of length IFACT(IF).
C                                       IPROD = IP1 / IP0
C                                       IREM = N / (IFACT(IF) * IPROD)
C                                       DATA(I1,J2,I3) = SUM (DATA(I1,
C                                       I2,I3) * W**(I2-1)), summed
C                                       over I2 = 1 to IFACT(IF) for
C                                       all I1 from 1 to IPROD, J2 from
C                                       1 to IFACT(IF) and I3 from 1
C                                       to IREM.
C                                       W = EXP(ISIGN*2*PI*I*(I1-1+
C                                           IPROD*(J2-1)) /
C                                           (IPROD*IFACT(IF))).
      IF = 0
      IP1 = IP0
 130  IF (IP1-IP3) 140,240,240
 140  IF = IF + 1
      IFCUR = IFACT(IF)
      IP2 = IP1 * IFCUR
      THETA = TWOPI / REAL (IFCUR)
      SINTH = SIN (THETA / 2.)
      ROOTR = -2. * SINTH * SINTH
      ROOTI = SIN (THETA)
      THETA = TWOPI / REAL (IP2 / IP0)
      SINTH = SIN (THETA / 2.)
      WSTPR = -2. * SINTH * SINTH
      WSTPI = SIN (THETA)
      WMINR = 1.
      WMINI = 0.
      INCLUDE 'INCS:ZVND.INC'
      DO 230 I1 = 1,IP1,IP0
         IF (IFCUR-2) 150,150,170
      INCLUDE 'INCS:ZVND.INC'
 150     DO 160 I3 = I1,IP3,IP2
            J0 = I3
            J1 = I3 + IP1
            TEMPR = WMINR * APCORE(JC+J1) - WMINI * APCORE(JC+J1+1)
            TEMPI = WMINR * APCORE(JC+J1+1) + WMINI * APCORE(JC+J1)
            APCORE(JC+J1) = APCORE(JC+J0) - TEMPR
            APCORE(JC+J1+1) = APCORE(JC+J0+1) - TEMPI
            APCORE(JC+J0) = APCORE(JC+J0) + TEMPR
            APCORE(JC+J0+1) = APCORE(JC+J0+1) + TEMPI
 160        CONTINUE
         GO TO 220
 170     IWMAX = IP0 * IFCUR
      INCLUDE 'INCS:ZVND.INC'
         DO 210 I3 = I1,IP3,IP2
            I2MAX = I3 + IP2 - IP1
            WR = WMINR
            WI = WMINI
      INCLUDE 'INCS:ZVND.INC'
            DO 200 JWORK = 1,IWMAX,IP0
               I2 = I2MAX
               SUMR = APCORE(JC+I2)
               SUMI = APCORE(JC+I2+1)
 180           I2 = I2 - IP1
               TEMPR = SUMR
               SUMR = WR * SUMR - WI * SUMI + APCORE(JC+I2)
               SUMI = WR * SUMI + WI * TEMPR + APCORE(JC+I2+1)
               IF (I2-I3) 190,190,180
 190           WRKFFT(JWORK) = SUMR
               WRKFFT(JWORK+1) = SUMI
               TEMP = WR
               WR = WR * ROOTR - WI * ROOTI + WR
               WI = TEMP * ROOTI + WI * ROOTR + WI
 200           CONTINUE
            JWORK = 1
      INCLUDE 'INCS:ZVND.INC'
            DO 210 I2 = I3,I2MAX,IP1
               APCORE(JC+I2) = WRKFFT(JWORK)
               APCORE(JC+I2+1) = WRKFFT(JWORK+1)
               JWORK = JWORK + IP0
 210           CONTINUE
 220     TEMPR = WMINR
         WMINR = WMINR * WSTPR - WMINI * WSTPI + WMINR
         WMINI = TEMPR * WSTPI + WMINI * WSTPR + WMINI
 230     CONTINUE
      IP1 = IP2
      GO TO 130
 240  CONTINUE
C
 999  RETURN
      END
