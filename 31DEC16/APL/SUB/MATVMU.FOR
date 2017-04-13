      SUBROUTINE MATVMU (M, VI, VO, N)
C-----------------------------------------------------------------------
C! multiplies a matrix and a vector
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
C   Matrix-vector multiplication  VO = VI * M
C   Inputs:
C      M(N,N)   R(N,N)   Input matrix
C      VI(N)    R(N)     Input vector
C      N        I        Adjustable array dimension
C   OUTPUTS:
C      VO       R(N)     Output vector
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      M(N,N), VI(N), VO(N)
C
      INTEGER   I, J
      REAL      S
C-----------------------------------------------------------------------
      DO 20 I = 1,N
         S = 0.0
         DO 10 J = 1,N
            S = S + M(I,J) * VI(J)
 10         CONTINUE
         VO(I) = S
 20      CONTINUE
C
 999  RETURN
      END
