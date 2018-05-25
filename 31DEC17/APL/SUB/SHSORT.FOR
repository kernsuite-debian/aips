      SUBROUTINE SHSORT (A, N, KEY1, KEY2, LEN, IERR)
C-----------------------------------------------------------------------
C! Shell sort of an array or records on two keys
C# Sort UV-util
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
C   SHSORT does a shell sort of records on two keys.  The algorithm is
C   CALCACM number 201.  Data are ordered to descending values of the
C   keys.
C   Inputs:
C      A      R(LEN,N)   Data array to be sorted.
C      N      I          Number of records to be sorted.
C      KEY1   I          Word number in record of slower varying key
C      KEY2   I          Word number in record of faster varying key
C      LEN    I          Length of record in REAL words (must be <=50)
C   Output:
C      A      R(LEN,N)   Sorted data.
C      IERR   I          Return code: 0 ok
C                             1 = bad input parameters.
C   Programmer: W. D. Cotton, Feb. 1981.
C-----------------------------------------------------------------------
      INTEGER   LEN, N, KEY1, KEY2, IERR
      REAL      A(LEN,N)
C
      INTEGER   D, I, J, LLN, LLEN
      REAL      Y(50)
C-----------------------------------------------------------------------
C                                        Check LEN
      IERR = 1
      LLN = N
      LLEN = LEN
      IF (LEN.GT.50) GO TO 999
      D = 1.442695 * LOG(REAL(LLN)) + 0.01
      D = 2**D - 1
      IERR = 0
 10   IF (D.LE.0) GO TO 999
         I = 1
 20      CONTINUE
            J = I
            CALL RCOPY (LLEN, A(1,I+D), Y)
 30         CONTINUE
               IF (Y(KEY1)-A(KEY1,J)) 50,40,70
 40               IF (Y(KEY2).GT.A(KEY2,J)) GO TO 70
 50                  CALL RCOPY (LLEN, Y, A(1,J+D))
                     I = I + 1
                     IF (I+D.LE.LLN) GO TO 20
                        D = (D-1) / 2
                        GO TO 10
 70            CONTINUE
                  CALL RCOPY (LLEN, A(1,J), A(1,J+D))
                  J = J - D
                  IF (J.GT.0) GO TO 30
                  GO TO 50
C
 999  RETURN
      END
