      SUBROUTINE RSORT (LEN, A)
C-----------------------------------------------------------------------
C! sort a real array into descending order
C# Sort Utility
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
C   RSORT sorts by Shell-Mezgar algorithm.  It is faster for ordered 
C   data.  This is sort of a binary sort
C   Input:
C      LEN   I         Length of array
C   Input/Output:
C      A     R(LEN)  Sorted data.
C Reference: Kunuth, Donald, Sorting and Searching,
C                            The art of computer Programming
C-----------------------------------------------------------------------
      INTEGER   LEN
      REAL      A(*)
C
      INTEGER   OUTER, INNER, M, II, K, I, L
C-----------------------------------------------------------------------
      M = LEN
C                                       Set outer loop lengths
      OUTER = INT ( ALOG(FLOAT(LEN))/LOG(2.)+1.0E-6)
C                                       Begin sort loop.
      DO 200 II = 1, OUTER
         M = M / 2
         K = LEN - M
C                                       Search for next highest value.
         DO 100 INNER = 1, K
            I = INNER
 50         CONTINUE
               L = I + M
C                                       Consider a swap
               IF (A(L).GT.A(I)) THEN
C                                       Its bigger, swap
                  CALL RSWAP ( A(I), A(L))
                  I = I - M
                  IF (I.GE.1)  GO TO 50
C                                       End if swapping
                  END IF
 100        CONTINUE
C                                       End Outer loop
 200     CONTINUE
C
      RETURN
      END
