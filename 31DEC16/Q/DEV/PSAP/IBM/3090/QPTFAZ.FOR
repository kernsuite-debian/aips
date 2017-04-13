@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QPTFAZ (X, INC, U, REAL, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: zCompute phase in model visibilities.
C# AP-appl  UV
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
C   Pseudo-AP version
C   Used to compute phases.   It does the following:
C         APCORE(REAL+m*INC) = APCORE(X+m*INC) * APCORE(U) +
C                            APCORE(X+1+m*INC) * APCORE(U+1) +
C                            APCORE(X+2+m*INC) * APCORE(U+2)
C         for m = 0 to N - 1
C   Inputs:
C      X     I  Base address of X,Y,Z array
C      INC   I  Increment of X and REAL
C      U     I  Base address of U,V,W array
C      REAL  I  Base address of phase array
C      N     I  Element count.
C-----------------------------------------------------------------------
      INTEGER   X, INC, U, REAL,  N, JX, JU, JREAL, LOOP
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                        Setup
      JX = X + 1
      JU = U + 1
      JREAL = REAL + 1
C                                       Loop
C@DIR IGNORE RECRDEPS(APCORE)
      DO 100 LOOP = 1,N
         APCORE(JREAL) = APCORE(JX) * APCORE(JU)
     *                 + APCORE(JX+1) * APCORE(JU+1)
     *                 + APCORE(JX+2) * APCORE(JU+2)
C                                       Update pointers.
         JREAL = JREAL + INC
         JX = JX + INC
 100     CONTINUE
C
 999  RETURN
      END
