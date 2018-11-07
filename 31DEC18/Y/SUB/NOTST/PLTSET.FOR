      SUBROUTINE PLTSET (XMIN, XMAX, YMIN, YMAX, XYSCL, XYOFF, YYOFF,
     *   IERR)
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
C   Sets up the common area /CPLOTU/ for PLOTDA.  This subroutine must
C   have been called before any calls are made to PLOTDA and it must
C   be segmented at a higher level than PLOTDA.
C   Inputs: XMIN,XMAX      R    min and max of X-axis in user
C                                 coordinates
C           YMIN,YMAX      R    same for Y-axis
C           XYSCL(2)       R    Scaling in X and Y
C           XYOFF(2)       R    X and Y offset in current plot.
C           YYOFF(2)       R    X and Y offset of current plot.
C   Programmer: L.B.Baath  Onsala Space Observatory  29 october 1982
C-----------------------------------------------------------------------
      INTEGER   IERR
      REAL      XMIN, XMAX, YMIN, YMAX, XYSCL(2), XYOFF(2), YYOFF(2),
     *    X(10)
      COMMON /CPLOTU/ X
C-----------------------------------------------------------------------
      X(1) = XYSCL(1)
      X(2) = XYSCL(2)
      X(3) = XYOFF(1)
      X(4) = XYOFF(2)
      X(5) = XMIN
      X(6) = XMAX
      X(7) = YMIN
      X(8) = YMAX
      X(9) = YYOFF(1)
      X(10) = YYOFF(2)
      IERR = 0
C
 999  RETURN
      END
