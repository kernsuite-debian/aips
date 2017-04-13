      SUBROUTINE FOURG ( DATA, N, ISIGN, WORK)
C-----------------------------------------------------------------------
C! Cooley-Tukey fast fourier transform.
C# Math
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
C     Cooley-Tukey fast fourier transform.
C     One-dimensional transform of complex data, arbitrary number of
C     points.  N points can be transformed in time proportional to
C     N*log(N) (for N non-prime), whereas other methods take N**2 time.
C     furthermore, because fewer arithmetic operations are performed,
C     less error is built up.
C     The transform done is--
C           Real*4 data(n),transform(n),work(n)
C           Complex data,transform,work
C       transform(k) = sum(data(j)*exp(isign*2*pi*i*(j-1)*(k-1)/n)),
C     summed from j = 1 to n for all k from 1 to N.  The transform
C     values are returned to data, replacing the input.  N may be any
C     positive number, but it should be non-prime for speed.
C     ISIGN = +1 or -1.
C     A -1 transform followed by a +1 one (or vice versa)
C     returns n times the original data.  work is a one-dimensional
C     complex array of length n used for working storage.
C     running time is proportional to n * (sum of the prime factors of
C     n).  for example, n = 1960, time is to * 1960 * (2+2+2+5+7+7).
C     naive methods directly implementing the summation run in time
C     proportional to n**2.  an upper bound for the rms relative error
C     is 3 * 2**(-b) * sum(f**1.5), where b is the number of bits in
C     the floating point fraction and the sum is over the prime
C     factors of n.
C
C  Inputs:
C     DATA(*)        R           Array of length N to be FFT'd
C     N              I           Length of data array - need not
C                                be a power of two.
C     ISIGN          I           Direction of transform
C     WORK(*)        R           Array of length N for temporary
C                                storage.
C
C  Outputs:
C     DATA(*)        R           Transformed array
C
C     Written by Norman Brenner, Mit, August 1968.
C     Modified for AIPS by P. Diamond, March 1988.
C-----------------------------------------------------------------------
      REAL    DATA(1), WORK(1), TWOPI, THETA, SINTH, ROOTR,
     *   ROOTI, WSTPR, WSTPI, WMINR, WMINI, TEMPR, TEMPI,
     *   WR, WI, SUMR, SUMI, TEMP
      INTEGER   IFACT(32), N, ISIGN, IF, NPART, ID, IDIV, IQUOT,
     *   NFACT, IP0, IP3, IWORK, I3REV, I3, IP2, IP1,
     *   IFCUR, I1, J0, J1, IWMAX, I2MAX, I2
C-----------------------------------------------------------------------
      TWOPI = 6.2831853072 * REAL (ISIGN)
C                                           factor n into its prime
C                                           factors, nfact in number.
C                                           for example, for n = 1960,
C                                           nfact = 6 and ifact(if) = 2,
C                                           2, 2, 5, 7 and 7.
      IF = 0
      NPART = N
      DO 50 ID = 1, N, 2
      IDIV = ID
      IF (ID-1) 10,10,20
 10   IDIV = 2
 20   IQUOT = NPART / IDIV
      IF (NPART - IDIV * IQUOT) 40,30,40
 30   IF = IF + 1
      IFACT(IF) = IDIV
      NPART = IQUOT
      GO TO 20
 40   IF (IQUOT-IDIV) 60,60,50
 50   CONTINUE
 60   IF (NPART-1) 80,80,70
 70   IF = IF + 1
      IFACT(IF) = NPART
 80   NFACT = IF
C                                       shuffle the data array by
C                                       reversing the digits of the
C                                       index. Replace data(i) by
C                                       data(irev) for all i from 1
C                                       to n.  irev-1 is the integer
C                                       whose digit representation
C                                       in the multi-radix notation
C                                       of factors ifact(if) is the
C                                       reverse of the representatn
C                                       of i-1. For example, if all
C                                       ifact(if) = 2, then for i-1
C                                       = 11001, irev-1 = 10011. A
C                                       work array of length n is
C                                       needed.
      IP0 = 2
      IP3 = IP0 * N
      IWORK = 1
      I3REV = 1
      DO 110 I3 = 1, IP3, IP0
      WORK(IWORK) = DATA(I3REV)
      WORK(IWORK+1) = DATA(I3REV+1)
      IP2 = IP3
      DO 100 IF = 1, NFACT
      IP1 = IP2 / IFACT(IF)
      I3REV = I3REV + IP1
      IF (I3REV-IP2) 110,110,90
 90   I3REV = I3REV - IP2
 100  IP2 = IP1
 110  IWORK = IWORK + IP0
      IWORK = 1
      DO 120 I3 = 1, IP3, IP0
      DATA(I3) = WORK(IWORK)
      DATA(I3+1) = WORK(IWORK+1)
 120  IWORK = IWORK + IP0
C                                       phase-shifted fourier transform
C                                       of length ifact(if).
C                                       iprod=ip1/ip0
C                                       irem=n/(ifact(if)*iprod)
C                                       real data(iprod,ifact(if),irem),
C                                       work(ifact(if))
C                                       complex data,work
C                                       data(i1,j2,i3) =
C                                         sum(data(i1,i2,i3)
C                                         * w**(i2-1)), summed over
C                                       i2 = 1 to ifact(if) for all i1
C                                       from 1 to iprod, j2 from 1 to
C                                       ifact(if) and i3 from 1 to irem.
C                                       w = exp(isign*2*pi*i*
C                                         (i1-1+iprod*(j2-1))/
C                                         (iprod*ifact(if))).
      IF = 0
      IP1 = IP0
 130  IF (IP1-IP3) 140,240,240
 140  IF = IF + 1
      IFCUR = IFACT(IF)
      IP2 = IP1 * IFCUR
      THETA = TWOPI / REAL (IFCUR)
      SINTH = SIN (THETA/2.)
      ROOTR = -2. * SINTH * SINTH
      ROOTI = SIN (THETA)
      THETA = TWOPI / REAL (IP2/IP0)
      SINTH = SIN (THETA / 2.)
      WSTPR = -2. * SINTH * SINTH
      WSTPI = SIN (THETA)
      WMINR = 1.
      WMINI = 0.
      DO 230 I1 = 1, IP1, IP0
      IF (IFCUR-2) 150,150,170
 150  DO 160 I3 = I1, IP3, IP2
      J0 = I3
      J1 = I3 + IP1
      TEMPR = WMINR * DATA(J1) - WMINI * DATA(J1+1)
      TEMPI = WMINR * DATA(J1+1) + WMINI * DATA(J1)
      DATA(J1) = DATA(J0) - TEMPR
      DATA(J1+1) = DATA(J0+1) - TEMPI
      DATA(J0) = DATA(J0) + TEMPR
 160  DATA(J0+1) = DATA(J0+1) + TEMPI
      GO TO 220
 170  IWMAX = IP0 * IFCUR
      DO 210 I3 = I1, IP3, IP2
      I2MAX = I3 + IP2 - IP1
      WR = WMINR
      WI = WMINI
      DO 200 IWORK = 1, IWMAX, IP0
      I2 = I2MAX
      SUMR = DATA(I2)
      SUMI = DATA(I2+1)
 180  I2 = I2 - IP1
      TEMPR = SUMR
      SUMR = WR * SUMR - WI * SUMI + DATA(I2)
      SUMI = WR * SUMI + WI * TEMPR + DATA(I2+1)
      IF (I2-I3) 190, 190, 180
 190  WORK(IWORK) = SUMR
      WORK(IWORK+1) = SUMI
      TEMP = WR
      WR = WR * ROOTR - WI * ROOTI + WR
 200  WI = TEMP * ROOTI + WI * ROOTR + WI
      IWORK = 1
      DO 210 I2 = I3, I2MAX, IP1
      DATA(I2) = WORK(IWORK)
      DATA(I2+1) = WORK(IWORK+1)
 210  IWORK = IWORK + IP0
 220  TEMPR = WMINR
      WMINR = WMINR * WSTPR - WMINI * WSTPI + WMINR
 230  WMINI = TEMPR * WSTPI + WMINI * WSTPR + WMINI
      IP1 = IP2
      GO TO 130
 240  RETURN
      END
