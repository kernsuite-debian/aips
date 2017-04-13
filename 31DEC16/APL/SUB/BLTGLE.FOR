      SUBROUTINE BLTGLE (XT, YT, X1, Y1, X2, Y2, ANG)
C-----------------------------------------------------------------------
C! returns angle from A through a test position to B
C# Map-util Utility
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
C   BLTGLE returns the angle in degrees from (X1, Y1) to (XT, YT) to
C   (X2, Y2).
C   Inputs:  XT    R      Test position (X)
C            YT    R      Test position (Y)
C            X1    I      Mark position 1 (X)
C            Y1    I      Mark position 1 (Y)
C            X2    I      Mark position 2 (X)
C            Y2    I      Mark position 2 (Y)
C   Output:  ANG   I      Angle between 1 -> T -> 2
C-----------------------------------------------------------------------
      INTEGER   X1, Y1, X2, Y2
      REAL      XT, YT, ANG
C
      REAL      XX1, XX2, YY1, YY2, CROSS, DOT, R2D
      DATA R2D /57.29578/
C-----------------------------------------------------------------------
C                                       vector coordinates
      XX1 = X1 - XT
      YY1 = Y1 - YT
      XX2 = X2 - XT
      YY2 = Y2 - YT
C                                       CROSS prop SIN
C                                       DOT prop COS
      CROSS = XX1 * YY2  -  XX2 * YY1
      DOT   = XX1 * XX2  +  YY1 * YY2
      ANG = 0.0
      IF ((CROSS.NE.0.) .OR. (DOT.NE.0.)) ANG = R2D *
     *   ATAN2 (CROSS, DOT)
C
 999  RETURN
      END
