      SUBROUTINE NATCEL (PHI, THETA, PHIP, ALPHAP, DELTAP, ALPHA, DELTA)
C-----------------------------------------------------------------------
C! Celeestial coordinate rotation from native to celestial
C# Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 2013
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
C   NATCEL does a coordinate rotation in the notation of WCS Paper II
C   from native coordinates to celestial coordinates
C   Inputs:
C      PHI      D   Native longitude
C      THETA    D   Native latitude
C      PHIP     D   Native longitude of North pole
C      ALPHAP   D   Celestial longitude of the native pole
C      DELTAP   D   Celestial latitude of the native pole
C   Outputs:
C      ALPHA    D   Celestial longitude
C      DELTA    D   Celestial latitude
C   ALL COORDINATES ARE IN RADIANS
C-----------------------------------------------------------------------
      DOUBLE PRECISION PHI, THETA, PHIP, ALPHAP, DELTAP, ALPHA, DELTA
C
C-----------------------------------------------------------------------
      ALPHA = ALPHAP + ATAN2 (-COS(THETA) * SIN(PHI-PHIP),
     *   SIN(THETA)*COS(DELTAP) - COS(THETA) * SIN(DELTAP) *
     *   COS(PHI-PHIP))
      DELTA = ASIN (SIN(THETA) * SIN(DELTAP) + COS(THETA) * COS(DELTAP)
     *   * COS(PHI-PHIP))
C
 999  RETURN
      END
