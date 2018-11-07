      DOUBLE PRECISION FUNCTION ENORM (N, X)
C-----------------------------------------------------------------------
C! computes the Euclidean norm of a N-vector
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
C   Given an N-vector X, this function calculates the
C   Euclidean norm of X.
C
C   The Euclidean norm is computed by accumulating the sum of
C   squares in three different sums. The sums of squares for the
C   small and large components are scaled so that no overflows
C   occur. Non-destructive underflows are permitted. Underflows
C   and overflows do not occur in the computation of the unscaled
C   sum of squares for the intermediate components.
C   The definitions of small, intermediate and large components
C   depend on two constants, Rdwarf and RGIANT. The main
C   restrictions on these constants are that RDWARF**2 not
C   underflow and RGIANT**2 not overflow. The constants
C   given here are suitable for every known computer.
C   Subprograms called
C       FORTRAN-supplied ... DABS,DSQRT
C   Argonne National Laboratory. Minpack Project. March 1980.
C   Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C   INPUTS:   N   I      length of array X (adj. array dim.)
C             X   D(N)   input array.
C   OUTPUT:   X   D(N)   Euclidian norm of input array.
C-----------------------------------------------------------------------
      INTEGER   N
      DOUBLE PRECISION    X(N)
C
      INTEGER   I
      DOUBLE PRECISION AGIANT, FLOATN, RDWARF, RGIANT, S1, S2,
     *   S3, XABS, X1MAX, X3MAX, ZERO, ONE
      DATA ONE, ZERO, RDWARF, RGIANT /1.0D0, 0.0D0, 3.834D-20, 1.304D19/
C-----------------------------------------------------------------------
      S1 = ZERO
      S2 = ZERO
      S3 = ZERO
      X1MAX = ZERO
      X3MAX = ZERO
      FLOATN = N
      AGIANT = RGIANT / FLOATN
      DO 90 I = 1,N
         XABS = ABS (X(I))
C                                       Sum for intermediate components
         IF ((XABS.GT.RDWARF) .AND. (XABS.LT.AGIANT)) THEN
            S2 = S2 + XABS**2
C                                       Sum for large components.
         ELSE IF (XABS.GT.RDWARF) THEN
            IF (XABS.GT.X1MAX) THEN
               S1 = ONE + S1 * (X1MAX/XABS)**2
               X1MAX = XABS
            ELSE
               S1 = S1 + (XABS/X1MAX)**2
               END IF
C                                       Sum for small components.
         ELSE
            IF (XABS.GT.X3MAX) THEN
               S3 = ONE + S3 * (X3MAX/XABS)**2
               X3MAX = XABS
            ELSE
               IF (XABS.NE.ZERO) S3 = S3 + (XABS/X3MAX)**2
               END IF
            END IF
 90      CONTINUE
C
C                                       Calculation of norm.
C
      IF (S1.NE.ZERO) THEN
         ENORM = X1MAX * SQRT (S1 + (S2/X1MAX)/X1MAX)
      ELSE IF (S2.NE.ZERO) THEN
         IF (S2.GE.X3MAX)
     *      ENORM = SQRT (S2 * (ONE + (X3MAX/S2) * (X3MAX*S3)))
         IF (S2.LT.X3MAX)
     *      ENORM = SQRT (X3MAX * ((S2/X3MAX) + (X3MAX*S3)))
      ELSE
         ENORM = X3MAX * SQRT (S3)
         END IF
C
 999  RETURN
      END
