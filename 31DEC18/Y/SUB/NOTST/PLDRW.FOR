      SUBROUTINE PLDRW (XVAL, YVAL, IRET)
C-----------------------------------------------------------------------
C! draw line to new pixel position from current pixel position in buffer
C# Utility Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2007
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
C   Draw a line to the pixel posn. defined by data values (XVAL, YVAL)
C   Inputs:
C      XVAL    R     x-coordinate value
C      YVAL    R     y-coordinate value
C   Outputs:
C      IRET    I     Return code (0 => ok)
C---------------------------------------------------------------------
      REAL XVAL, YVAL
      INTEGER IRET
C
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DPLD.INC'
      DOUBLE PRECISION DXSCAL, DYSCAL
      REAL XPIX, YPIX
C---------------------------------------------------------------------
C                                       Scale X,Y values
      DXSCAL = XVAL * XPFACT
      DYSCAL = YVAL * YPFACT
C                                       Compute pixel position
      CALL XYPIX (DXSCAL, DYSCAL, XPIX, YPIX, IRET)
C                                       Check if out of range
      IF (IRET.EQ.0) THEN
         XPIX = MIN (XPIX, PLTRC(1))
         XPIX = MAX (XPIX, PLBLC(1))
         YPIX = MIN (YPIX, PLTRC(2))
         YPIX = MAX (YPIX, PLBLC(2))
C                                       Go to pixel position
         CALL GVEC (XPIX, YPIX, PLTBLK, IRET)
         END IF
C                                       Exit
 999  RETURN
      END
