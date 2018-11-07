      SUBROUTINE PRJMUL (INDEX, IN, MAT, OUT)
C-----------------------------------------------------------------------
C! Multiplies 3x1 or 1x3 matrix times 3x3
C# Map Header coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
C;  Associated Universities, Inc. Washington DC, USA.
C;  Eric W. Greisen
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
C   Returns a 1x3 (rows, cols) matrix = 1x3 times 3x3 (INDEX = 1)
C   Returns a 3x1 (rows, cols) matrix = 3x3 times 3x1 (INDEX = 2)
C   Input:
C      INDEX    I        Type of multiply 1 is 1x3 (uvw's), type 2 is
C                        3x1 (xyz's)
C      IN       R(3)     Input matrix
C      MAT      R(3,3)   Matrix
C   Output
C      OUT      R(3)     Output matrix can be input
C-----------------------------------------------------------------------
      INTEGER   INDEX
      REAL      IN(3), MAT(3,3), OUT(3)
C
      INTEGER   I, J
      REAL      T(3)
C-----------------------------------------------------------------------
      DO 10 I = 1,3
         T(I) = IN(I)
 10      CONTINUE
C                                       uvw's
      IF (INDEX.EQ.1) THEN
         DO 20 I = 1,3
            OUT(I) = 0.0
            DO 15 J = 1,3
               OUT(I) = OUT(I) + T(J) * MAT(J,I)
 15            CONTINUE
 20         CONTINUE
C                                       xyz's
      ELSE
         DO 30 I = 1,3
            OUT(I) = 0.0
            DO 25 J = 1,3
               OUT(I) = OUT(I) + T(J) * MAT(I,J)
 25            CONTINUE
 30         CONTINUE
         END IF
C
 999  RETURN
      END
