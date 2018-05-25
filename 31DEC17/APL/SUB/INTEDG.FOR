      SUBROUTINE INTEDG (BLC, TRC, XL, YL, XU, YU, NO, XO, YO)
C-----------------------------------------------------------------------
C! returns intersections of a line with the edges of a box
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   INTEDG takes the corners of a box and the end points of a line and
C   returns the 0, 1 or 2 coordinate pairs at which the line crosses
C   the box.  NOTE: 1 point must be outside the box or NO = 0.
C   See also LINLIM.
C   Inputs:
C      BLC    R(2)        Bottom left corner of box
C      TRC    R(2)        Top right corner of box
C      XL     R           X coordinate of end point 1 of line
C      YL     R           Y coordinate of end point 1 of line
C      XU     R           X coordinate of end point 2 of line
C      YU     R           Y coordinate of end point 2 of line
C   Output:
C      NO     I           Number of intersections (0, 1, or 2)
C                           -1 => both points inside
C      XO     R(2)        X coordinates of intersections
C      YO     R(2)        Y coordinates of intersections
C-----------------------------------------------------------------------
      REAL      BLC(2), TRC(2), XL, YL, XU, YU, XO(2), YO(2)
      INTEGER   NO
C
      REAL      X, Y
C-----------------------------------------------------------------------
C                                       Check for all in
      NO = -1
      IF ((XL.GE.BLC(1)) .AND. (XL.LE.TRC(1)) .AND. (YL.GE.BLC(2)) .AND.
     *   (YL.LE.TRC(2)) .AND. (XU.GE.BLC(1)) .AND. (XU.LE.TRC(1)) .AND.
     *   (YU.GE.BLC(2)) .AND. (YU.LE.TRC(2))) GO TO 999
C                                       Check for all off
      NO = 0
      IF (((XL.LT.BLC(1)) .AND. (XU.LT.BLC(1))) .OR. ((XL.GT.TRC(1))
     *   .AND. (XU.GT.TRC(1))) .OR. ((YL.LT.BLC(2)) .AND.
     *   (YU.LT.BLC(2))) .OR. ((YL.GT.TRC(2)) .AND. (YU.GT.TRC(2))))
     *   GO TO 999
C                                       Left edge
      IF (((XL.LE.BLC(1)) .OR. (XU.LE.BLC(1))) .AND. (XL.NE.XU)) THEN
         Y = YL + (BLC(1)-XL) * (YU-YL) / (XU-XL)
         IF ((Y.GT.BLC(2)) .AND. (Y.LT.TRC(2))) THEN
            NO = NO + 1
            XO(NO) = BLC(1)
            YO(NO) = Y
            END IF
         END IF
C                                       Right edge
      IF (((XL.GE.TRC(1)) .OR. (XU.GE.TRC(1))) .AND. (XL.NE.XU)) THEN
         Y = YL + (TRC(1)-XL) * (YU-YL) / (XU-XL)
         IF ((Y.GT.BLC(2)) .AND. (Y.LT.TRC(2))) THEN
            NO = NO + 1
            XO(NO) = TRC(1)
            YO(NO) = Y
            END IF
         END IF
C                                       Bottom edge
      IF (((YL.LE.BLC(2)) .OR. (YU.LE.BLC(2))) .AND. (YL.NE.YU)) THEN
         X = XL + (BLC(2)-YL) * (XU-XL) / (YU-YL)
         IF ((X.GE.BLC(1)) .AND. (X.LE.TRC(1))) THEN
            NO = NO + 1
            YO(NO) = BLC(2)
            XO(NO) = X
            END IF
         END IF
C                                       Top edge
      IF (((YL.GE.TRC(2)) .OR. (YU.GE.TRC(2))) .AND. (YL.NE.YU)) THEN
         X = XL + (TRC(2)-YL) * (XU-XL) / (YU-YL)
         IF ((X.GE.BLC(1)) .AND. (X.LE.TRC(1))) THEN
            NO = NO + 1
            YO(NO) = TRC(2)
            XO(NO) = X
            END IF
         END IF
C
 999  RETURN
      END
