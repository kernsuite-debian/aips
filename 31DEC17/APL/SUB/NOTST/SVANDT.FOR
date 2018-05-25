      SUBROUTINE SVANDT (N, X, Y)
C-----------------------------------------------------------------------
C! Solve a Vandermonde system of equations for polynomial interpolation
C# Util
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
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
C   Solve the Vandermonde system of equations Va = y where V is the
C   transposed Vandermonde matrix
C
C        +-                                    -+
C        |   1  X(1)   X(1)**2   ... X(1)**N    |
C        |   1  X(2)   X(2)**2   ... X(2)**N    |
C        |   ...          ...    ...   ...      |
C        |   1  X(N+1) X(N+1)**2 ... X(N+1)**N  |
C        +-                                    -+
C
C   This usually occurs when trying to interpolate tabulated values
C   of a function (y) with an N-th order polynomial in which case
C   the solution vector, a, contains the polynomial coeeficients.
C
C   This routine uses Algorithm 4.6.1 from Matrix Computations (second
C   edition) by Gene H. Glob and Charles F. van Loan, John Hopkins
C   University Press, 1989.  This requires 5 * N**2 / 2 FLOPS.
C
C   Inputs:
C      N        I        The order of the polynomial
C      X        R(N+1)   Sample points of the  function to be
C                        interpolated
C
C   Input/Output
C      Y        R(N+1)   The sampled function values on input,
C                        overwritten by the polynomial coefficients.
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      X(N+1), Y(N+1)
C
      INTEGER   I, K
C-----------------------------------------------------------------------
C                                       Calculate the Newton
C                                       representation of the
C                                       interpolating polynomial.
      DO 20 K = 0, N - 1
         DO 10 I = N + 1, K + 2, -1
            Y(I) = (Y(I) - Y(I - 1)) / (X(I) - X(I - K - 1))
   10    CONTINUE
   20 CONTINUE
C                                       Calculate polynomial
C                                       coefficients.
      DO 40 K = N, 1, -1
         DO 30 I = K, N
            Y(I) = Y(I) - Y(I + 1) * X(K)
   30    CONTINUE
   40 CONTINUE
C
      END
