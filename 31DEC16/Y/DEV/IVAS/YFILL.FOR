      SUBROUTINE YFILL (CHAN, IX0, IY0, IXT, IYT, IVAL, IBLK, IERR)
C-----------------------------------------------------------------------
C! fill rectangle of TV memory with a constant value
C# TV-util
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
C   YFILL will write a constant in a given rectangle in a given graphics
C   or image plane.  It will use fast methods if full screen requested
C   with IVAL 0.
C   Inputs:
C      CHAN     I   Channel (1 to NGRAY+NGRAPH)
C      IX0      I   lower left X pixel (1 relative) of rectangle
C      IY0      I   lower left Y pixel of rectangle.
C      IXT      I   top right X pixel of rectangle.
C      IYT      I   top right Y pixel of rectangle.
C      IVAL     I   desired value: for graphics = 0 or 1
C                                  for grey scale = 0 - MAXINT
C   In/out:
C      IBLK     I(IXT-IX0+1)   work buffer.
C   Output:
C      IERR     I   error code of Z...XF: 0 ok, 2 input error
C   IVAS version: faster than Generic and will work better too
C-----------------------------------------------------------------------
      INTEGER   IX0, IY0, IXT, IYT, IVAL, IBLK(*), IERR
C
      INTEGER   CHAN, IY, INUM, LNUM, X0, Y0, IV, JERR, LV, LX, LY, NX,
     *   IC, IPL
      LOGICAL   VRTRTC
      INTEGER   FIVASMAIMAGE, FIVASGPHVALUE, FIVASGPHMOVE, FIVASGPHFILL
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      DATA VRTRTC /.FALSE./
C-----------------------------------------------------------------------
C                                       partial planes
      IF ((IX0.LE.1) .AND. (IY0.LE.1) .AND. (IXT.GE.MAXXTV(1)) .AND.
     *   (IYT.GE.MAXXTV(2)) .AND. (IVAL.EQ.0)) GO TO 100
         IERR = 2
         IF ((IXT.LT.IX0) .OR. (IYT.LT.IY0)) GO TO 999
         IF ((IX0.LT.1) .OR. (IY0.LT.1)) GO TO 999
         IF ((IXT.GT.MAXXTV(1)) .OR. (IYT.GT.MAXXTV(2))) GO TO 999
         INUM = IXT - IX0 + 1
         LNUM = IYT - IY0 + 1
         X0 = IX0
         Y0 = IY0
         CALL YVRTR (VRTRTC, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Grey scale
         IF (CHAN.GT.NGRAY) GO TO 50
            IV = MAX (IVAL, 0)
            CALL FILL (INUM, IV, IBLK)
            NX = INUM
            LX = X0 - 1
            LY = MAXXTV(2) - Y0
            IC = CHAN - 1
            IPL = 4095
            IF (NGRAY.GT.2) IPL = 255
C                                       Loop for all lines.
            DO 30 IY = 1,LNUM
               JERR = FIVASMAIMAGE (IBLK, NX, 1, 2, NX, LX, LY, IC,
     *            IPL, 0)
               IF (JERR.NE.0) GO TO 900
               LY = LY - 1
 30            CONTINUE
            GO TO 999
C                                       Graphics
 50      CONTINUE
            IV = IVAL
            IF (IV.LE.0) THEN
               LV = 15 - 2 ** (CHAN-NGRAY-1)
            ELSE
               LV = 2 ** (CHAN-NGRAY-1)
               END IF
            JERR = FIVASGPHVALUE (LV, LV)
            IF (JERR.NE.0) GO TO 900
            LV = 0
            LX = IX0 - 1
            LY = IY0 - 1
            JERR = FIVASGPHMOVE (LX, LY, LV)
            IF (JERR.NE.0) GO TO 900
            LX = IXT - 1
            LY = IYT - 1
            LV = 1
            IF (IV.LE.0) LV = 2
            JERR = FIVASGPHFILL (LX, LY, LV)
            IF (JERR.NE.0) GO TO 900
            GO TO 999
C                                       full planes
 100  CONTINUE
            CALL YZERO (CHAN, IERR)
            GO TO 999
C                                       Error
 900  CALL YDOERR ('YFILL', JERR, IERR)
C
 999  RETURN
      END
