      SUBROUTINE CUBINT (A, MAPXY, NX, NY, ANS, IERR)
C-----------------------------------------------------------------------
C! does 2-dimensional cubic interpolation of array values to position
C# Math Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2014
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
C   CUBINT does a two dimentional cubic interpolation to the position in
C   array A specified by MAPXY.  It checks for blanked pixels.
C   Inputs:
C      A      R(16,16)    Data array
C      MAPXY  R(2)        Position in array A (count from 1 wrt A)
C      NX     I           X-dimension of actual data in A
C      NY     I           Y-dimension of actual data in A
C   Output:
C      ANS    R           Interpolated pixel value.
C      IERR   I           0 -> ok, 1 can't interpolate (array size)
C                              2 -> can't interpolate (blanked)
C-----------------------------------------------------------------------
      REAL      A(16,16), MAPXY(2), ANS
      INTEGER   NX, NY, IERR
C
      REAL      DX, DY, AM1, AZ, AP1, AP2, X, FM1, FZ, FP1, FP2, EPS,
     *   CUBE
      INTEGER   IX, IY, LX, LY, I, J
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA EPS /1.E-3/
C-----------------------------------------------------------------------
C                                       Inline cubic interpolation.
      CUBE (X, FM1, FZ, FP1, FP2) = FZ
     *   + X *         (FP1 - (FP2 + 3.*FZ + 2.*FM1) / 6.)
     *   + X * X *     ((FM1 + FP1) / 2. - FZ)
     *   + X * X * X * (FP2 - 3.*FP1 + 3.*FZ - FM1) / 6.
C-----------------------------------------------------------------------
      IERR = 0
      ANS = 0.
C                                       Coords in array
      IX = MAPXY(1) + EPS
      IY = MAPXY(2) + EPS
      LX = IX - 1
      LY = IY - 1
      DX = MAPXY(1) - IX
      DY = MAPXY(2) - IY
C                                       On cell:
      IF ((ABS(DX).GE.EPS) .OR. (ABS(DY).GE.EPS)) GO TO 10
         ANS = A(IX,IY)
         IF (ANS.EQ.FBLANK) IERR = 2
         IF ((IX.LT.1) .OR. (IX.GT.NX) .OR. (IY.LT.1) .OR. (IY.GT.NY))
     *      IERR = 1
         GO TO 990
C                                       Check Y interpolation range
 10   IF (ABS(DY).LT.EPS) GO TO 30
         IF (LY.GE.1) GO TO 20
            LY = LY + 1
            DY = DY - 1.0
            GO TO 10
 20      IF (LY.LE.NY-3) GO TO 30
            LY = LY - 1
            DY = DY + 1.0
            IF (LY.GE.1) GO TO 20
               IERR = 1
               GO TO 990
C                                       Check X interpolation range
 30   IF (ABS(DX).LT.EPS) GO TO 50
         IF (LX.GE.1) GO TO 40
            LX = LX + 1
            DX = DX - 1.0
            GO TO 30
 40      IF (LX.LE.NX-3) GO TO 50
            LX = LX - 1
            DX = DX + 1.0
            IF (LX.GE.1) GO TO 40
               IERR = 1
               GO TO 990
C                                       On an X pixel
 50   IF (ABS(DX).GE.EPS) GO TO 60
         IF ((A(IX,LY).EQ.FBLANK) .OR. (A(IX,LY+1).EQ.FBLANK) .OR.
     *      (A(IX,LY+2).EQ.FBLANK) .OR. (A(IX,LY+3).EQ.FBLANK)) IERR = 2
         IF (IERR.EQ.0) ANS = CUBE (DY, A(IX,LY), A(IX,LY+1),
     *      A(IX,LY+2), A(IX,LY+3))
         GO TO 990
C                                       On a Y pixel
 60   IF (ABS(DX).GE.EPS) GO TO 70
         IF ((A(LX,IY).EQ.FBLANK) .OR. (A(LX+1,IY).EQ.FBLANK) .OR.
     *      (A(LX+2,IY).EQ.FBLANK) .OR. (A(LX+3,IY).EQ.FBLANK)) IERR = 2
         IF (IERR.EQ.0) ANS = CUBE (X, A(LX,IY), A(LX+1,IY),
     *      A(LX+2,IY), A(LX+3,IY))
         GO TO 990
C                                       Interpolate in X, then Y.
 70   CONTINUE
C                                       Check
         IERR = 2
         DO 80 I = 1,4
            DO 75 J = 1,4
               IF (A(LX+I-1,LY+J-1).EQ.FBLANK) GO TO 990
 75            CONTINUE
 80         CONTINUE
         IERR = 0
C                                       Interpolate in X.
         AM1 = CUBE (DX, A(LX,LY), A(LX+1,LY), A(LX+2,LY),
     *      A(LX+3,LY))
         AZ = CUBE (DX, A(LX,LY+1), A(LX+1,LY+1), A(LX+2,LY+1),
     *      A(LX+3,LY+1))
         AP1 = CUBE (DX, A(LX,LY+2), A(LX+1,LY+2), A(LX+2,LY+2),
     *      A(LX+3,LY+2))
         AP2 = CUBE (DX, A(LX,LY+3), A(LX+1,LY+3), A(LX+2,LY+3),
     *      A(LX+3,LY+3))
C                                       Interpolate in Y.
         ANS = CUBE (DY, AM1, AZ, AP1, AP2)
C                                       Possible error
 990  IF (IERR.EQ.0) GO TO 999
         IF (IERR.EQ.1) WRITE (MSGTXT,1990)
         IF (IERR.EQ.2) WRITE (MSGTXT,1991)
         CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('CUBINT: ARRAY IS TOO SMALL FOR INTERPOLATION')
 1991 FORMAT ('CUBINT: REQUIRED PIXEL VALUE IS BLANKED')
      END
