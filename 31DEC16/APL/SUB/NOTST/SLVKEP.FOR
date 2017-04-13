      SUBROUTINE SLVKEP (M, ECCEN, E, PREC)
C-----------------------------------------------------------------------
C! Solve Kepler's equation for the eccentric anomaly
C# Util Orbits
C-----------------------------------------------------------------------
C;  Copyright (C) 1998
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
C   Solve Kepler's equation for the eccentric anomaly
C   E = M + ECCEN * SIN(E) to precision PREC given the mean anomaly M.
C
C   Inputs:
C      M        Mean anomaly (radians)
C      ECCEN    Orbital eccentricity
C      PREC     Desired precision (radians)
C
C   Output:
C      E        Eccentric anomaly
C
C   References:
C     "Orbital Motion", A.E. Roy, Institute of Physics Press, 1988
C     (Section 4.5.5)
C-----------------------------------------------------------------------
      DOUBLE PRECISION M, ECCEN, E, PREC
C
      DOUBLE PRECISION TRIAL
C-----------------------------------------------------------------------
      E = M
      TRIAL = M + ECCEN * SIN (E)
   10 IF (ABS(TRIAL - E) .GT. PREC) THEN
         E = TRIAL
         TRIAL = M + ECCEN * SIN (E)
         GO TO 10
      END IF
C
      END
