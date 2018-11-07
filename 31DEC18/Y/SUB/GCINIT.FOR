      SUBROUTINE GCINIT (CHN, COL, IERR)
C-----------------------------------------------------------------------
C! inits the image catalog and image plane for TV-using plot
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2002
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
C   GCINIT initializes a TV channel: YCINIT, YZERO, ZSLECT
C   Inputs:
C      CHN     I   TV channel 1 - NGRAY+NGRAPH
C      COL     I   0 ALL, 1 R, 2 G, 3 B
C   Output:
C      IERR    I   Error code
C-----------------------------------------------------------------------
      INTEGER   CHN, COL, IERR
C
      INTEGER   LC
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DGPH.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IF (CHN.LT.0) CHN = -CHN
      IF (GPHCON(CHN).LE.0) THEN
         LC = COL
         IF (CHN.GT.NGRAY) LC = 0
         CALL YCINIT (CHN, GPHSCR)
         CALL YZERO (CHN, IERR)
         IF ((IERR.EQ.0) .AND. (LC.NE.0)) CALL YSLECT ('OFFF', CHN, 0,
     *      GPHSCR, IERR)
         IF (IERR.EQ.0) CALL YSLECT ('ONNN', CHN, LC, GPHSCR, IERR)
         GPHCON(CHN) = 1
         END IF
C
 999  RETURN
      END

