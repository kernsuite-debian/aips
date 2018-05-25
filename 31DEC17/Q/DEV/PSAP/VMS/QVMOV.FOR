      SUBROUTINE QVMOV (A, I, C, K, N)
C-----------------------------------------------------------------------
C! VMS Pseudo AP routine: Vector move.
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
C   VMS version.
C   Copy one vector to another:
C       C(mK) = A(mI)  for m=0 to N-1
C   Inputs:
C      A  I  Source vector base address
C      I  I  A address increment
C      C  I  Destination vector base address
C      K  I  C address increment
C      N  I  Element count
C-----------------------------------------------------------------------
      INTEGER   A, I, C, K, N, JA, JC, LOOP, NPASS, LENGTH, STAT,
     *   NLEFT, LEN
      INTEGER*2 ILEN
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Check increments = 1
      IF ((I.EQ.1) .AND. (K.EQ.1)) GO TO 200
C                                       Check increments = -1
      IF (((I.EQ.-1) .AND. (K.EQ.-1)) .AND. ((C-A).GT.10)) GO TO 300
C                                       Addresses 1-rel
      JA = A + 1
      JC = C + 1
C                                       Loop
      INCLUDE 'INCS:ZVND.INC'
      DO 100 LOOP = 1,N
         APCORE (JC) = APCORE (JA)
         JC = JC + K
         JA = JA + I
 100     CONTINUE
      GO TO 999
C                                       Increment = 1 use library
C                                       routine
 200  CONTINUE
C                                       Real Values, Init block transfer
      LENGTH = 8191
      NLEFT = N
C                                       Send what is left if less than max
      LEN = MIN (NLEFT, LENGTH)
C                                       Calc Num. block transfers + a few.
      NPASS = (REAL(N) / REAL(LENGTH)) + 2.0
C                                       Start at the beginning of the blocks
      JA = A + 1
      JC = C + 1
      DO 250 LOOP = 1,NPASS
         ILEN = LEN * 4
         CALL LIB$MOVC3 (ILEN, APCORE(JA), APCORE(JC))
         NLEFT = NLEFT - LEN
C                                       Exit if none left
         IF (NLEFT .LE. 0) GO TO 999
         JA = JA + LEN
         JC = JC + LEN
         LEN = MIN (NLEFT, LENGTH)
 250     CONTINUE
      GO TO 999
C                                       Increment = -1 use library
C                                       routine in sections
 300  CONTINUE
      LENGTH = (C - A)
      IF (LENGTH.GT.8191) LENGTH = 8191
      NLEFT = N
C                                       Send what is left if less than max
      LEN = MIN (NLEFT, LENGTH)
C                                       Calc Num. block transfers + a few.
      NPASS = (REAL(N) / REAL(LENGTH)) + 2.0
C                                       Start at the beginning of the blocks
      JA = A + 2 - LEN
      JC = C + 2 - LEN
      DO 350 LOOP = 1,NPASS
         ILEN = LEN * 4
         CALL LIB$MOVC3 (ILEN, APCORE(JA), APCORE(JC))
         NLEFT = NLEFT - LEN
C                                       Exit if none left
         IF (NLEFT .LE. 0) GO TO 999
         JA = JA - LEN
         JC = JC - LEN
         LEN = MIN (NLEFT, LENGTH)
 350     CONTINUE
C
 999  RETURN
      END
