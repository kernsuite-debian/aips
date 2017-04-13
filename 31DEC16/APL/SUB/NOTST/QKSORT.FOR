      SUBROUTINE QKSORT (A, N, KEY1, KEY2, LEN, IERR)
C-----------------------------------------------------------------------
C! Two key "quick" sort routine to sort arrays.
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
C   QKSORT is a modification of the VLA quick sort routine QWKSRT.
C   Data are ordered to descending values of the keys.
C   Inputs:
C      A(LEN,N)          I    Data array to be sorted.
C      N                 I    Number of records to be sorted.
C      KEY1,KEY2         I    Word number in the record of the two keys
C                             KEY2 varies fastest.
C      LEN               I    Length of record in words (<=50).
C   Output:
C      A(LEN,N)          I    Sorted data.
C      IERR              I    Return code
C                             0 = OK
C                             1 = bad input parameters.
C                             2 = Hit recursion limit.
C-----------------------------------------------------------------------
      INTEGER   XAXDEP
C                                       XAXDEP = recursion limit
      PARAMETER (XAXDEP = 50)
      INTEGER   N, LEN
      INTEGER   A(LEN,N), KEY1, KEY2, IERR, I(XAXDEP), J(XAXDEP),
     *   L(XAXDEP), R(XAXDEP), MAXDEP, IDEP, IMED, II, JJ, K, KT
      LOGICAL   SORTR(XAXDEP), T, F
      DATA MAXDEP /XAXDEP/,    T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Check parameters.
      IERR = 1
      IF ((N.LE.0) .OR. (LEN.LE.0)) GO TO 999
      IF ((KEY1.GT.LEN) .OR. (KEY2.GT.LEN)) GO TO 999
      IERR = 0
C                                       Setup for first recursive level.
      IDEP = 1
      L(1) = 1
      R(1) = N
C                                       Begin recursive portion.
 10   I(IDEP) = L(IDEP)
      J(IDEP) = R(IDEP)
C                                       Assume median in middle.
      IMED = (L(IDEP) + R(IDEP)) / 2
C                                       Following WHILE loop sorts the
C                                       partition into records with keys
C                                       above and below those of IMED.
 20   IF (I(IDEP).GE.J(IDEP)) GO TO 120
C                                       Key comparison.
C                                       Step left and right boundaries
C                                       until you hit a record on the
C                                       wrong side of IMED.
 30   II = I(IDEP)
      IF (A(KEY1,II)-A(KEY1,IMED)) 60,40,50
 40   IF (A(KEY2,II)-A(KEY2,IMED)) 60,60,50
 50   I(IDEP) = I(IDEP) + 1
      GO TO 30
C
 60   JJ = J(IDEP)
      IF (A(KEY1,JJ)-A(KEY1,IMED)) 80,70,90
 70   IF (A(KEY2,JJ)-A(KEY2,IMED)) 80,90,90
 80   J(IDEP) = J(IDEP) - 1
      GO TO 60
C                                       If I>J the loop is over.
C                                       Else a swap must be made.
 90   IF (I(IDEP).GT.J(IDEP)) GO TO 120
         II = I(IDEP)
         JJ = J(IDEP)
         DO 100 K = 1,LEN
            KT = A(K,II)
            A(K,II) = A(K,JJ)
            A(K,JJ) = KT
 100        CONTINUE
         I(IDEP) = I(IDEP) + 1
         J(IDEP) = J(IDEP) - 1
         GO TO 20
C                                       Here is the main recursion.
C                                       Unless J=L generate a new level
C                                       and recurse.
 120  IF (L(IDEP).GE.J(IDEP)) GO TO 140
      IF (IDEP.LT.MAXDEP) GO TO 130
         IERR = 2
         GO TO 999
 130  L(IDEP+1) = L(IDEP)
      R(IDEP+1) = J(IDEP)
      SORTR(IDEP) = F
      IDEP = IDEP + 1
      GO TO 10
C                                       Unless I=R generate a new level
C                                       and recurse.
 140  IF (I(IDEP).GE.R(IDEP)) GO TO 160
      IF (IDEP.LT.MAXDEP) GO TO 150
         IERR = 2
         GO TO 999
 150  L(IDEP+1) = I(IDEP)
      R(IDEP+1) = R(IDEP)
      SORTR(IDEP) = T
      IDEP = IDEP + 1
      GO TO 10
C                                       OK, Now pop the level and see
C                                       where we were on the new level.
 160  IDEP = IDEP - 1
      IF (IDEP.EQ.0) GO TO 999
      IF (.NOT.SORTR(IDEP)) GO TO 140
      GO TO 160
C
 999  RETURN
      END
