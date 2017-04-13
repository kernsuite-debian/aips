      SUBROUTINE INVERT (MINP, MINV, N, IERR)
C-----------------------------------------------------------------------
C! Routine to invert a matrix by Gaussian elimination
C# Math
C-----------------------------------------------------------------------
C;  Copyright (C) 2001
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
C   Routine to invert a matrix by Gaussian elimination
C   Sizes maxed at 64 x 64
C   Inputs:
C      MINP      R(N,N)  The input matrix
C      N         I       Order of the matrix
C   Output:
C      MINV      R(N,N)  The inverse matrix
C      IERR I    Error code, 0=OK,
C                1=matrix singular.
C-----------------------------------------------------------------------
      INTEGER N, IERR
      INTEGER N2, I, J, K, IJ
      DOUBLE PRECISION MINP(*), MINV(*)
      DOUBLE PRECISION D(64,128), ALPHA, BETA
      DOUBLE PRECISION A(64,64)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       store the input matrix in the
C                                       two dimension array
      DO 20 I = 1,N
         DO 10 J = 1,N
            IJ = J + (I-1)*N
            A(I,J) = MINP(IJ)
   10       CONTINUE
   20    CONTINUE
C
      N2 = 2*N
      DO 40 I = 1,N
         DO 30 J = 1,N
            D(I,J) = A(I,J)
            D(I,N+J) = 0.
   30       CONTINUE
         D(I,N+I) = 1.
   40    CONTINUE
C                                       Do the reduction
      DO 80 I = 1,N
         ALPHA = D(I,I)
         IF(ALPHA .EQ. 0.) GO TO 300
         DO 50 J = 1,N2
            D(I,J) = D(I,J)/ALPHA
   50       CONTINUE
         DO 70 K = 1,N
            IF((K-I).EQ.0) GO TO 70
            BETA = D(K,I)
            DO 60 J = 1,N2
               D(K,J) = D(K,J) - BETA*D(I,J)
   60          CONTINUE
   70       CONTINUE
   80    CONTINUE
C                                       Copy result into output matrix
       DO 100 I = 1,N
          DO 90 J = 1,N
             IJ = J + (I-1)*N
             MINV(IJ) = D(I,J+N)
   90        CONTINUE
  100     CONTINUE
       GO TO 999
 300   IERR = 1
       WRITE (MSGTXT,1000)
       CALL MSGWRT (5)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('*** ERROR: SINGULAR MATRIX ***')
      END

