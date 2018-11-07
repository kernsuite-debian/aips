      SUBROUTINE QPRJM (INDEX, IN, MAT, OUT, N, INC, OUTC)
C-----------------------------------------------------------------------
C! Multiplies 3x1 or 1x3 matrix times 3x3
C# ap-apply
C-----------------------------------------------------------------------
C;  Copyright (C) 2008, 2012
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
C   Returns a 1x3 (rows, cols) matrix = 1x3 times 3x3 (INDEX = 1)
C   Returns a 3x1 (rows, cols) matrix = 3x3 times 3x1 (INDEX = 2)
C   Input:
C      INDEX    I        Type of multiply 1 is 1x3 (uvw's),
C                           type 2 is 3x1 (xyz's)
C      IN       I        Base address of input matrix
C      MAT      R(3,3)   rotation matirx
C      OUT      I        Base address of output matrix (can be input
C      N        I        Number of input matrices
C      INC      I        Increment in AP between input matrices
C      OUTC     I        Increment in AP between output matrices
C-----------------------------------------------------------------------
      INTEGER   INDEX, IN, OUT, N, INC, OUTC
      REAL      MAT(3,3)
C
      LONGINT   JI, JO
      INTEGER   I, J, K
      DOUBLE PRECISION T(3)
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      JI = IN + PSAPOF
      JO = OUT + PSAPOF
C                                       uvw's
      IF (INDEX.EQ.1) THEN
         DO 50 K = 1,N
            T(1) = APCORE(JI)
            T(2) = APCORE(JI+1)
            T(3) = APCORE(JI+2)
            DO 20 I = 1,3
               APCORE(JO) = 0.0D0
               DO 10 J = 1,3
                  APCORE(JO) = APCORE(JO) + T(J) * MAT(J,I)
 10               CONTINUE
               JO = JO + 1
 20            CONTINUE
            JO = JO + OUTC - 3
            JI = JI + INC
 50         CONTINUE
C                                       xyz's
      ELSE
         DO 100 K = 1,N
            T(1) = APCORE(JI)
            T(2) = APCORE(JI+1)
            T(3) = APCORE(JI+2)
            DO 70 I = 1,3
               APCORE(JO) = 0.0D0
               DO 60 J = 1,3
                  APCORE(JO) = APCORE(JO) + T(J) * MAT(I,J)
 60               CONTINUE
               JO = JO + 1
 70            CONTINUE
            JO = JO + OUTC - 3
            JI = JI + INC
 100        CONTINUE
         END IF
C
 999  RETURN
      END
