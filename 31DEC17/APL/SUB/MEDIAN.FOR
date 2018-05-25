      REAL FUNCTION MEDIAN (N, A)
C-----------------------------------------------------------------------
C! Finds the median using method due to Wirth
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 2010
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
C   Find the median of an array
C   Use Niklaus Wirth's algorithm to find Kth ordered element in an
C   array.  From page 84 of: "Algorithms + Data Structures = Programs"
C   Input:
C      N        I      Length of array
C   In/out:
C      A        R(*)   Data array (will be reordered)
C
C   Input/output:
C      MEDIAN   R      The (N+1)/2 element
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      A(*)
C
      REAL      NWIRTH
      INTEGER   K
C-----------------------------------------------------------------------
      K = (N + 1) / 2
      MEDIAN = NWIRTH (A, N, K)
C
 999  RETURN
      END
      REAL FUNCTION NWIRTH (A, N, K)
C-----------------------------------------------------------------------
C   Use Niklaus Wirth's algorithm to find Kth ordered element in an
C   array.  From page 84 of: "Algorithms + Data Structures = Programs"
C   Input:
C      N        I      Length of array
C      K        I
C   In/out:
C      A        R(*)   Data array (will be reordered)
C   Output:
C      NWIRTH   R      The Kth element in array A
C-----------------------------------------------------------------------
      INTEGER   N, K
      REAL      A(*)
C
      REAL      X, T
      INTEGER   I, J, L, M
C-----------------------------------------------------------------------
C                                       Standard FORTRAN goes 1:N
C                                       Wirth used 0:N-1
      L = 1
      M = N
 10   IF (L.LT.M) THEN
         X = A(K)
         I = L
         J = M
 20      IF (I.LE.J) THEN
 30         IF (A(I).LT.X) THEN
               I = I + 1
               GO TO 30
               END IF
 40         IF (X.LT.A(J)) THEN
               J = J - 1
               GO TO 40
               END IF
           IF (I.LE.J) THEN
               T = A(I)
               A(I) = A(J)
               A(J) = T
               I = I + 1
               J = J - 1
               END IF
            GO TO 20
            END IF
         IF (J.LT.K) L = I
         IF (K.LT.I) M = J
         GO TO 10
         END IF
C
      NWIRTH = A(K)
C
 999  RETURN
      END
