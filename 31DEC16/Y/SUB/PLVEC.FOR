      SUBROUTINE PLVEC (X, Y, IERR)
C-----------------------------------------------------------------------
C! puts a 'draw vector' command in a plot file: Gary plot package
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
C   This routine will put a 'draw vector' command in a plot file.
C   Vectors that are too big are interpolated.
C   Inputs:
C      X       R    X value.
C      Y       R    Y value.
C   Common:
C      /PLTCOM/
C   Output:
C      IERR    I    Error code.  0 means OK.
C-----------------------------------------------------------------------
      REAL      X, Y
      INTEGER   IERR
C
      REAL      XX, YY, XT(2), YT(2)
      INTEGER   NO
      INCLUDE 'INCS:DPLT.INC'
C-----------------------------------------------------------------------
      XX = XSCAL * X + XOFF
      YY = YSCAL * Y + YOFF
C                                       Test edge intersections
      CALL INTEDG (PBLC, PTRC, XX, YY, XLAST, YLAST, NO, XT, YT)
C                                       Vector all in plot
      IF (NO.GE.0) GO TO 10
         CALL GVEC (XX, YY, PLTBLK, IERR)
         GO TO 100
C                                       Both points off plot
 10   IF (NO.EQ.1) GO TO 20
         IOFFPL = IOFFPL + 1
         IF (NO.EQ.0) GO TO 100
            CALL GPOS (XT(1), YT(1), PLTBLK, IERR)
            IF (IERR.EQ.0) CALL GVEC (XT(2), YT(2), PLTBLK, IERR)
            GO TO 100
C                                       This point off plot
 20   IF ((XX.GE.PBLC(1)) .AND. (XX.LE.PTRC(1)) .AND. (YY.GE.PBLC(2))
     *   .AND. (YY.LE.PTRC(2))) GO TO 30
         IOFFPL = IOFFPL + 1
         CALL GVEC (XT(1), YT(1), PLTBLK, IERR)
         GO TO 100
C                                       Last point off plot
 30   CONTINUE
         CALL GPOS (XT(1), YT(1), PLTBLK, IERR)
         IF (IERR.EQ.0) CALL GVEC (XX, YY, PLTBLK, IERR)
C                                       Save last desired position
 100  XLAST = XX
      YLAST = YY
C
 999  RETURN
      END
