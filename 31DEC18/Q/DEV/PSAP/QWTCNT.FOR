      SUBROUTINE QWTCNT (A, I, WT, J, C, N, NB)
C-----------------------------------------------------------------------
C! Sums positive values into an array
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2012
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
C   Pseudo-AP version
C   Sums positive values into an array.
C   Inputs:
C      A     I  Vector of array indices as integers (1-rel)
C      I     I  A address increment.
C      WT    I  Vector of values; values < 0 replaced by 0.
C      J     I  Increment of WT
C      C     I  Summing array
C               Array must be cleared before first call.
C      N     I  Element count for A, WT
C      NB    I  Number of cells in C
C-----------------------------------------------------------------------
      INTEGER   A, I, WT, J, C, N, NB
C
      LONGINT   JA, JC, JWT, LOOP, LEND, INDEX
      INTEGER   ELEM
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                       Addresses to 1-rel
      JA = A + PSAPOF
      JC = C + PSAPOF - 1
      JWT = WT + PSAPOF
C                                        SUM
      LEND = JA + (N-1) * I
      DO 100 LOOP = JA,LEND,I
         ELEM = APCORI(LOOP)
         INDEX = MAX (MIN (ELEM, NB), 1) + JC
         APCORE(INDEX) = APCORE(INDEX) + MAX (0.0D0, APCORE(JWT))
         JWT = JWT + J
 100     CONTINUE
C
 999  RETURN
      END
