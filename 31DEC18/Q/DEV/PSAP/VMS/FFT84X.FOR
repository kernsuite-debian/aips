      SUBROUTINE FFT84X (IN, N, X)
C-----------------------------------------------------------------------
C! VMS Pseudo AP routine: FFT routine
C#AP-FFT
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
C   Version of FFT842 using Bob Saults' VAX assembler R8TXX.
C   This program replaces the complex vector  X by its finite discrete,
C   complex Fourier transform if IN = 0.  The inverse transform is
C   calculated for IN=1.  It performs as many base 8 iterations as
C   possible and then finishes with a base 4 iteration or a base 2
C   iteration if needed.
C   Inputs:
C      IN  I      Transform direction; 0=>forward, 1=>reverse.
C      N   I      Number of points in array (must be power of two).
C   Input/Output:
C      X   I(2,N)  Complex array to transform / array transformed
C   Code Modified from:
C   Subroutine FFT842, by Bergland and Dolan
C   Programs for  Digital Signal Processing, IEEE press, 1979.
C   Uses routines R2TX, R4TX, R8TXX.
C-----------------------------------------------------------------------
      REAL       X(2,*), R, FI
      INTEGER    I, J, K, M, N2POW, NTHPO, N, IN, N8POW,
     *   IPASS, NXTLT, LENGT, NXTLT2, INDX(33)
C-----------------------------------------------------------------------
      M = 0
      INDX(1) = 1
      K = 1
C                                       Bit reversal/power array
 10   IF (K.LT.N) THEN
         M = M + 1
         K = K + K
         INDX(M+1) = K
         GO TO 10
         END IF
 20   N2POW = M
      NTHPO = N
      IF (IN.EQ.1) GO TO 40
      DO 30 I = 1,N
         X(2,I) = -X(2,I)
 30      CONTINUE
 40   N8POW = N2POW / 3
      IF (N8POW.EQ.0) GO TO 60
C                                       Radix 8 passes, if any.
      DO 50 IPASS = 1,N8POW
         NXTLT = N2POW - 3*IPASS + 1
         NXTLT = INDX(NXTLT)
         LENGT = 8 * NXTLT
         CALL R8TXX (NXTLT, NTHPO, LENGT, X(1,1), X(1,NXTLT+1),
     *      X(1,2*NXTLT+1), X(1,3*NXTLT+1), X(1,4*NXTLT+1),
     *      X(1,5*NXTLT+1), X(1,6*NXTLT+1),  X(1,7*NXTLT+1))
 50      CONTINUE
C                                       Reset NTHPO for other routines
      NTHPO = 2 * NTHPO
C                                       Is there a factor of 4 left?
 60   IF (N2POW-3*N8POW-1) 90, 70, 80
C                                       Go through the base 2 iteration
 70   CALL R2TX (NTHPO, X(1,1), X(1,2), X(2,1), X(2,2))
      GO TO 90
C                                       Go through the base 4 iteration
 80   CALL R4TX (NTHPO, X(1,1), X(1,2), X(1,3), X(1,4), X(2,1), X(2,2),
     *   X(2,3), X(2,4))
C                                       Bit reversal
 90   I = 0
      DO 110 J = 1,N-2
        K = M - 1
 100    IF (I.GE.INDX(K+1)) THEN
           I = I - INDX(K+1)
           K = K - 1
           GO TO 100
           END IF
        I = I + INDX(K+1)
        IF (I.GT.J) THEN
           R = X(1,I+1)
           X(1,I+1) = X(1,J+1)
           X(1,J+1) = R
           FI = X(2,I+1)
           X(2,I+1) = X(2,J+1)
           X(2,J+1) = FI
           END IF
 110    CONTINUE
      IF (IN.EQ.1) GO TO 150
      DO 140 I = 1,N
         X(2,I) = -X(2,I)
 140     CONTINUE
 150  CONTINUE
 170  RETURN
      END
