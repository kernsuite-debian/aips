      SUBROUTINE TVSLAC (YVAL, XVAL, IERR)
C-----------------------------------------------------------------------
C! activates and reads TV cursor, converts result to image coordinates
C# Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 2000, 2007
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
C   TVSLAC will read the TV cursor and convert the position to units
C   displayed on TV SLICE plot.  Requires the image cat block in
C   common and the location common to be set up for the image and
C   requires the TV to be open.
C   Outputs:
C      YVAL    R       y position in janskys or whatever.
C      XVAL    R       x position in arc sec or whatever..
C      IERR    I       error code:  0=ok,  1=cursor not on plot,
C                         2 = TV I/O error,  3=bad position
C   Common input:
C      /MAPHDR/ CATBLK image catalog header (from IC not CB file)
C-----------------------------------------------------------------------
      INTEGER   IERR
      REAL      YVAL, XVAL
C
      DOUBLE PRECISION DX, DY, DZ
      REAL      PX0, PX1, PY0, PY1, XPIX, YPIX, RPOS(2), ITX0, ITX1,
     *   ITY0, ITY1
      INTEGER   QUAD, BUTTON
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTKS.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Open, read, close Tektronix.
      CALL YCURSE ('READ', .TRUE., .TRUE., RPOS, QUAD, BUTTON, IERR)
      IF (IERR.NE.0) THEN
         IERR = 2
         GO TO 999
         END IF
C                                       Plot corners in Tek units.
      ITX0 = CATBLK(IICOR)
      ITY0 = CATBLK(IICOR+1)
      ITX1 = CATBLK(IICOR+2)
      ITY1 = CATBLK(IICOR+3)
C                                       Corners in funny scale units.
      PX0 = CATBLK(IIWIN)
      PY0 = CATBLK(IIWIN+1)
      PX1 = CATBLK(IIWIN+2)
      PY1 = CATBLK(IIWIN+3)
C                                       Cursor position not on plot.
      IF ((RPOS(1).LT.ITX0) .OR. (RPOS(1).GT.ITX1) .OR.
     *   (RPOS(2).LT.ITY0) .OR. (RPOS(2).GT.ITY1)) THEN
         WRITE (MSGTXT,1000) RPOS(1), RPOS(2)
         CALL MSGWRT (6)
         IERR = 1
C                                       Calc XVAL sec from center.
      ELSE
         XPIX = (PX1 - PX0) * (RPOS(1)-ITX0) / (ITX1-ITX0) + PX0
         YPIX = (PY1 - PY0) * (RPOS(2)-ITY0) / (ITY1-ITY0) + PY0
         CALL XYVAL (XPIX, YPIX, DX, DY, DZ, IERR)
         XVAL = DX
         YVAL = DY
         IF (IERR.NE.0) IERR = 3
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CURSOR POSITION (X,Y) =',2F7.1,' NOT ON PLOT')
      END
