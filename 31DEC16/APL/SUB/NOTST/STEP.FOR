      SUBROUTINE STEP (IN1, IN2, FSMALL, STPLEN, IERR)
C-----------------------------------------------------------------------
C! Tim COrnwell routine: adds a fraction of one image to another.
C# IO-appl Math
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
C   STEP takes a step in the image i.e.  (IN1) = (IN1) + STPLEN*(IN2)
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      INTEGER   IN1, IN2
      REAL      FSMALL, STPLEN
      INTEGER   AKOPEN, AKCESS, AKCLOS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IMGMAX = -1.0E20
      IMGMIN =  1.0E20
      IF (AKOPEN (IN1, 1, 'READ', BUFFR1) .NE.0) GO TO 990
      IF (AKOPEN (IN2, 2, 'READ', BUFFR2) .NE.0) GO TO 990
      IF (AKOPEN (SCR, 3, 'WRIT', BUFFR3) .NE.0) GO TO 990
C                                       Loop through map
      DO 250 IY = 1,HNY
C                                       Write row to WRK file.
         IF (AKCESS (SCR, BUFFR3) .NE.0) GO TO 990
C                                        Do calculations
         CALL RFILL (HNX, 0.0, BUFFR3(BIND(3)))
         IF ((IY.LT.YBEG) .OR. (IY.GT.YEND)) GO TO 250
            IF (AKCESS (IN1, BUFFR1) .NE.0) GO TO 990
            IF (AKCESS (IN2, BUFFR2) .NE.0) GO TO 990
            I1 = BIND(1)
            I2 = BIND(2)
            I3 = BIND(3) + XBEG - 1
            DO 190  IX = XBEG, XEND
C                                       Take step
               BUFFR3(I3) = MAX (BUFFR1(I1) + STPLEN*BUFFR2(I2),
     *            FSMALL)
               I1 = I1 + 1
               I2 = I2 + 1
               I3 = I3 + 1
 190           CONTINUE
C                                       Find extrema
            I3 = BIND(3) + XBEG - 1
            DO 200  IX = XBEG, XEND
               IMGMAX = MAX (IMGMAX, BUFFR3(I3))
               IMGMIN = MIN (IMGMIN, BUFFR3(I3))
               I3 = I3 + 1
 200           CONTINUE
 250     CONTINUE
      IF (AKCLOS (IN1, BUFFR1) .NE.0) GO TO 990
      IF (AKCLOS (IN2, BUFFR2) .NE.0) GO TO 990
      IF (AKCLOS (SCR, BUFFR3) .NE.0) GO TO 990
      CALL FILSWP (SCR, IN1)
      GO TO 999
C
 990  WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
      IERR = 1
C
 999  CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('STEP')
      END
