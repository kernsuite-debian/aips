      SUBROUTINE PLPOS (X, Y, IERR)
C-----------------------------------------------------------------------
C! puts a 'position vector' command in a plot file: Gary plot package
C# plot-util
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
C   This routine will put a 'position vector' command in a plot file.
C   Inputs:
C      X       R    X value.
C      Y       R    Y value.
C      COMMON /PLTCOM/
C   Output:
C      IERR    I    Error code.  0 means OK.
C-----------------------------------------------------------------------
      REAL      X, Y
      INTEGER   IERR
C
      REAL      XX, YY
      INCLUDE 'INCS:DPLT.INC'
C-----------------------------------------------------------------------
      XX = XSCAL * X + XOFF
      YY = YSCAL * Y + YOFF
C                                       Position off plot: flag
      IF ((XX.LT.PBLC(1)) .OR. (XX.GT.PTRC(1)) .OR. (YY.LT.PBLC(2))
     *   .OR. (YY.GT.PTRC(2))) THEN
         IOFFPL = IOFFPL + 1
C                                       Put position in plot file
      ELSE
         CALL GPOS (XX, YY, PLTBLK, IERR)
         END IF
C                                       Save last position
      XLAST = XX
      YLAST = YY
C
 999  RETURN
      END
