      SUBROUTINE ATFPNT (X0, LII, BII, X, Y)
C-----------------------------------------------------------------------
C! Routine to calculate X-Y coords from galactic coords
C# Coordinates
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
C   Routine to calculate X-Y coords from galactic coords
C    Inputs:
C     XO        R       Semi-major axis
C     LII       R       Longitude
C     BII       R       Latitude
C    Outputs:
C     X         R       X position from centre of plot
C     Y         R       Y position from centre
C-----------------------------------------------------------------------
      REAL TETA, PHI, X, Y, X0
      REAL LII, BII
C-----------------------------------------------------------------------
C
      PHI = BII*0.01745329
      IF (LII .GT. 180.) LII = LII-360.
      TETA = LII*0.01745329
      X = -X0*COS(PHI)*SIN(TETA/2.)/(SQRT(1.+COS(PHI)*COS(TETA/2.)))
      Y = X0/2.*SIN(PHI)/(SQRT(1.+COS(PHI)*COS(TETA/2.)))
C
 999  RETURN
      END
