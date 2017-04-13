      SUBROUTINE FFT842 (IN, N, X)
C-----------------------------------------------------------------------
C! VMS Pseudo AP routine:  Full Complex FFT routine.
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
C   This program replaces the complex vector  X by its finite discrete,
C   complex Fourier transform if IN = 0.  The inverse transform is
C   calculated for IN=1.  It performs as many base 8 iterations as
C   possible and then finishes with a base 4 iteration or a base 2
C   iteration if needed.
C   Inputs:
C      IN  I       Transform direction; 0=>forward, 1=>reverse.
C      N   I       Number of points in array (must be power of two).
C   Input/Output:
C      X   I(2,N)  Complex array to transform / array transformed
C   Code Modified from:
C   Subroutine FFT842, by Bergland and Dolan
C   Programs for  Digital Signal Processing, IEEE press, 1979.
C   Uses routines R2TX, R4TX, R8TX.
C-----------------------------------------------------------------------
      REAL       X(2,*), PI2, P7, R, FI
      INTEGER    L(15), L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11,
     *   L12, L13, L14, L15, I, J, M, NT, N2POW, NTHPO, N, IN, N8POW,
     *   IPASS, NXTLT, LENGT, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10,
     *   J11, J12, J13, J14, JI, IJ, NXTLT2
      COMMON /CON2/ PI2, P7
      EQUIVALENCE (L15, L(1)), (L14, L(2)), (L13, L(3)), (L12, L(4)),
     *   (L11, L(5)), (L10, L(6)), (L9, L(7)), (L8, L(8)), (L7, L(9)),
     *   (L6, L(10)), (L5, L(11)), (L4, L(12)), (L3, L(13)),
     *   (L2, L(14)), (L1, L(15))
C-----------------------------------------------------------------------
      PI2 = 8.0 * ATAN (1.0)
      P7 = 1.0 / SQRT (2.0)
      DO 10 I = 1,15
         M = I
         NT = 2 ** I
         IF (N.EQ.NT) GO TO 20
 10      CONTINUE
 20   N2POW = M
      NTHPO = N * 2
      IF (IN.EQ.1) GO TO 40
      DO 30 I = 1,N
         X(2,I) = -X(2,I)
 30      CONTINUE
 40   N8POW = N2POW / 3
      IF (N8POW.EQ.0) GO TO 60
C                                       Radix 8 passes, if any.
      DO 50 IPASS = 1,N8POW
         NXTLT = 2 ** (N2POW-3*IPASS)
         NXTLT2 = 2 * NXTLT
         LENGT = 8 * NXTLT2
         CALL R8TX (NXTLT2, NTHPO, LENGT, X(1,1), X(1,NXTLT+1),
     *      X(1,2*NXTLT+1), X(1,3*NXTLT+1), X(1,4*NXTLT+1),
     *      X(1,5*NXTLT+1), X(1,6*NXTLT+1),  X(1,7*NXTLT+1),
     *      X(2,1), X(2,NXTLT+1), X(2,2*NXTLT+1), X(2,3*NXTLT+1),
     *      X(2,4*NXTLT+1), X(2,5*NXTLT+1), X(2,6*NXTLT+1),
     *      X(2,7*NXTLT+1))
 50      CONTINUE
C                                       Is there a factor of 4 left?
 60   IF (N2POW-3*N8POW-1) 90, 70, 80
C                                       Go through the base 2 iteration
 70   CALL R2TX (NTHPO, X(1,1), X(1,2), X(2,1), X(2,2))
      GO TO 90
C                                       Go through the base 4 iteration
 80   CALL R4TX (NTHPO, X(1,1), X(1,2), X(1,3), X(1,4), X(2,1), X(2,2),
     *   X(2,3), X(2,4))
 90   DO 110 J = 1,15
         L(J) = 1
         IF ((J-N2POW) .LE. 0) L(J) = 2 ** (N2POW+1-J)
 110     CONTINUE
      IJ = 1
      DO 130 J1 = 1,L1
         DO 130 J2 = J1,L2,L1
            DO 130 J3 = J2,L3,L2
               DO 130 J4 = J3,L4,L3
                  DO 130 J5 = J4,L5,L4
                     DO 130 J6 = J5,L6,L5
                        DO 130 J7 = J6,L7,L6
                           DO 130 J8 = J7,L8,L7
                              DO 130 J9 = J8,L9,L8
                                 DO 130 J10 = J9,L10,L9
                                    DO 130 J11 = J10,L11,L10
      DO 130 J12 = J11,L12,L11
         DO 130 J13 = J12,L13,L12
            DO 130 J14 = J13,L14,L13
               DO 130 JI = J14,L15,L14
                  IF (IJ .GE. JI) GO TO 130
                     R = X(1,IJ)
                     X(1,IJ) = X(1,JI)
                     X(1,JI) = R
                     FI = X(2,IJ)
                     X(2,IJ) = X(2,JI)
                     X(2,JI) = FI
 130              IJ = IJ + 1
      IF (IN.EQ.1) GO TO 150
      DO 140 I = 1,N
         X(2,I) = -X(2,I)
 140     CONTINUE
 150  CONTINUE
 170  RETURN
      END
