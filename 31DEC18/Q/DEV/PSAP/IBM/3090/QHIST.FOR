      SUBROUTINE  QHIST (A, I, C, N, NB, AMAX, AMIN)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Make histogram of a vector.
C# AP-appl
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
C   QHIST computes the histogram of a vector.
C   Histogram element (NB-1)*(DATA-MIN)/(MAX-MIN) where DATA
C   is the data value is incremented.
C   Inputs:
C      A     I  Source vector base address.
C      I     I  A address increment.
C      C     I  Histogram base address
C                 Histogram must be cleared before first call.
C      N     I  Element count for A
C      NB    I  Number of bins in histogram
C      AMAX  I  Address of histogram maximum.
C      AMIN  I  Address of histogram minimum.
C-----------------------------------------------------------------------
      INTEGER   A, I, C, N, NB, AMAX, AMIN, JA, JC, JAMAX, JAMIN, INDEX,
     *   LOOP, LEND, ITEMP
      REAL      FACT, XMIN
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                       Addresses to 1-rel
      JA = A + 1
      JC = C + 1
      JAMAX = AMAX + 1
      JAMIN = AMIN + 1
C                                        Compute histogram.
      FACT = (NB-1) / (APCORE(JAMAX) - APCORE(JAMIN))
      XMIN = APCORE(JAMIN)
      LEND = JA + (N-1) * I
      DO 100 LOOP = JA,LEND,I
         INDEX = (APCORE(LOOP) - XMIN) * FACT + 0.5
         INDEX = INDEX + JC
         INDEX = MAX (JC, INDEX)
         ITEMP = JC + NB - 1
         INDEX = MIN (INDEX, ITEMP)
         APCORE(INDEX) = APCORE(INDEX) + 1.0
 100     CONTINUE
C
 999  RETURN
      END
