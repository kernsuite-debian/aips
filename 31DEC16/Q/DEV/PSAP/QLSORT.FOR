      SUBROUTINE QLSORT (A, N, KEY1, KEY2, LEN)
C-----------------------------------------------------------------------
C! sort a data buffer minimizing number times records are switched
C# Sort Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006
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
C   QLSORT sorts by repeatedlly searching for the next largest key and
C   switching records with the next value.  It is best suited for sorts
C   involving long record lengths as it minimizes the number of times
C   records are switched.
C   Inputs:
C      A     R(LEN,N)  Data array to be sorted.
C      N     I         Number of records to be sorted.
C      KEY1  I         Word number in the record of slower varying key
C      KEY2  I         Word number in the record of faster varying key
C      LEN   I         Length of record in words.
C   Output:
C      A     R(LEN,N)  Sorted data.
C-----------------------------------------------------------------------
      INTEGER   LEN, N, KEY1, KEY2
      REAL      A(LEN,N)
C
      REAL      TEMP
      INTEGER   NEXT, HIGH, NEXT1, I, LN, LLEN
C-----------------------------------------------------------------------
      LN = N
      LLEN = LEN
      NEXT = 1
C                                       Begin sort loop.
 20   CONTINUE
         HIGH = NEXT
         NEXT1 = NEXT + 1
C                                       Search for next highest value.
         DO 50 I = NEXT1,LN
            IF (A(KEY1,HIGH)-A(KEY1,I)) 40,30,50
 30            IF (A(KEY2,HIGH).GE.A(KEY2,I)) GO TO 50
 40               HIGH = I
 50         CONTINUE
C                                       Switch if necessary.
         IF (HIGH.NE.NEXT) THEN
            DO 60 I = 1,LLEN
               TEMP = A(I,HIGH)
               A(I,HIGH) = A(I,NEXT)
               A(I,NEXT) = TEMP
 60            CONTINUE
            END IF
C                                       Increment NEXT
 70      NEXT = NEXT + 1
C                                       Check if finished, if not loop.
         IF (NEXT.LT.LN) GO TO 20
C
 999  RETURN
      END
