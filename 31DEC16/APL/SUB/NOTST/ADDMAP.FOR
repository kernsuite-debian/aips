      SUBROUTINE ADDMAP (IN1, IN2, OUT, VAL, OUTMAX, OUTMIN, IERR)
C-----------------------------------------------------------------------
C! *TESS routine to add images
C# MAP-util
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
C  Adds IN1 to IN2 putting the answer into OUT
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      INTEGER   IN1, IN2, OUT
      REAL      OUTMAX, OUTMIN, VAL
      INTEGER   AKOPEN, AKCESS, AKCLOS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C
      IERR = 0
      IF (AKOPEN (IN1,1,'READ',BUFFR1).NE.0) GO TO 990
      IF (AKOPEN (IN2,2,'READ',BUFFR2).NE.0) GO TO 990
      IF (AKOPEN (SCR,3,'WRIT',BUFFR3).NE.0) GO TO 990
C                                       Loop through map
      DO 250 IY = 1,HNY
C
         IF (AKCESS(SCR,BUFFR3).NE.0) GO TO 990
C                                        Do calculations
         IF ((IY.LT.YBEG) .OR. (IY.GT.YEND)) GO TO 250
            IF (AKCESS(IN1,BUFFR1).NE.0) GO TO 990
            IF (AKCESS(IN2,BUFFR2).NE.0) GO TO 990
            I1 = BIND(1)
            I2 = BIND(2)
            I3 = BIND(3) + XBEG - 1
            CALL RFILL(HNX, VAL, BUFFR3(BIND(3)))
            DO 190 IX = XBEG, XEND
               IF (BUFFR1(I1).NE.FBLANK) THEN
                  IF (BUFFR2(I2).NE.FBLANK) THEN
                     BUFFR3(I3) = BUFFR1(I1) + BUFFR2(I2)
                     OUTMAX = MAX(OUTMAX, BUFFR3(I3))
                     OUTMIN = MIN(OUTMIN, BUFFR3(I3))
                  ELSE
                     BUFFR3(I3) = BUFFR1(I1)
                     OUTMAX = MAX(OUTMAX, BUFFR3(I3))
                     OUTMIN = MIN(OUTMIN, BUFFR3(I3))
                  END IF
               ELSE
                  IF (BUFFR2(I2).NE.FBLANK) THEN
                     BUFFR3(I3) = BUFFR2(I2)
                     OUTMAX = MAX(OUTMAX, BUFFR3(I3))
                     OUTMIN = MIN(OUTMIN, BUFFR3(I3))
                  ELSE
                     BUFFR3(I3) = FBLANK
                  END IF
               END IF
               I1 = I1 + 1
               I2 = I2 + 1
               I3 = I3 + 1
 190           CONTINUE
 250     CONTINUE
      IF (AKCLOS (IN1,BUFFR1).NE.0) GO TO 990
      IF (AKCLOS (IN2,BUFFR2).NE.0) GO TO 990
      IF (AKCLOS (SCR,BUFFR3).NE.0) GO TO 990
C
      CALL FILSWP (SCR, OUT)
      GO TO 999
C
 990  WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ADDMAP')
      END
