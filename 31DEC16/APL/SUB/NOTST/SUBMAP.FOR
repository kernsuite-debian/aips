      SUBROUTINE SUBMAP (IN1, IN2, OUT, INDEF, IERR)
C-----------------------------------------------------------------------
C! Tim Cornwell Routine: Subtract two images.
C# IO-appl Map Math
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
C  Subtracts IN2 from IN1 putting the answer into OUT
C   Programmer =  R. Braun                            November 1987
C-----------------------------------------------------------------------
      INTEGER   IN1, IN2, OUT
      REAL      INDEF
      INTEGER   AKOPEN, AKCESS, AKCLOS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C
      IERR = 0
      IF (AKOPEN (IN1, 1, 'READ', BUFFR1) .NE.0) GO TO 990
      IF (AKOPEN (IN2, 2, 'READ', BUFFR2) .NE.0) GO TO 990
      IF (AKOPEN (SCR, 3, 'WRIT', BUFFR3) .NE.0) GO TO 990
C                                       Loop through map
      DO 250 IY = 1,HNY
C
         IF (AKCESS (SCR, BUFFR3) .NE.0) GO TO 990
C                                        Do calculations
         IF ((IY.LT.YBEG) .OR. (IY.GT.YEND)) GO TO 250
            IF (AKCESS (IN1, BUFFR1) .NE.0) GO TO 990
            IF (AKCESS (IN2, BUFFR2) .NE.0) GO TO 990
            I1 = BIND(1)
            I2 = BIND(2)
            I3 = BIND(3) + XBEG - 1
            CALL RFILL (HNX, INDEF, BUFFR3(BIND(3)))
            DO 190 IX = XBEG, XEND
               IF (BUFFR1(I1).NE.INDEF) THEN
                  BUFFR3(I3) = 0.0
                  IF (BUFFR2(I2).NE.0.0)
     *              BUFFR3(I3) = BUFFR1(I1) - BUFFR2(I2)
               END IF
               I1 = I1 + 1
               I2 = I2 + 1
               I3 = I3 + 1
 190           CONTINUE
 250     CONTINUE
      IF (AKCLOS (IN1, BUFFR1) .NE.0) GO TO 990
      IF (AKCLOS (IN2, BUFFR2) .NE.0) GO TO 990
      IF (AKCLOS (SCR, BUFFR3) .NE.0) GO TO 990
C
      CALL FILSWP(OUT, SCR)
      GO TO 999
C
 990  WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUBMAP')
      END
