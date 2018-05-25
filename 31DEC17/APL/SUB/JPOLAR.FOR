      SUBROUTINE JPOLAR (DIR, POLARX, POLARY, POS)
C-----------------------------------------------------------------------
C! Correct rectangular position for polar motion.
C# Math Coordinates
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
C   Routine to correct apparent positions for the position of the
C   Earth's pole.
C   Method from Astronomical Almanac 1986 B59.
C    Inputs:
C     DIR      I  1 => Celestial to terrestrial
C                -1 => Terrestrial to celestial
C     POLARX   R  Polar "X" position (arcsec)
C     POLARY   R  Polar "Y" position (arcsec)
C    Input/Output:
C     POS(3)   D  Position in rectangular coordinates (angle cosines)
C-----------------------------------------------------------------------
      INTEGER DIR
      REAL    POLARX, POLARY
      DOUBLE PRECISION POS(3)
      DOUBLE PRECISION CC, CX, SX, CY, SY, T1, T2
C-----------------------------------------------------------------------
      CC = 4.848136811D-6
      CX = COS (CC * POLARX)
      SX = SIN (CC * POLARX)
      CY = COS (CC * POLARY)
      SY = SIN (CC * POLARY)
C                                       Direction?
      IF (DIR.GT.0) THEN
C                                       Celestial to terrestial
C                                       Y correction.
         T1 = CY * POS(2) - SY * POS(3)
         T2 = SY * POS(2) + CY * POS(3)
         POS(2) = T1
         POS(3) = T2
C                                       X correction.
         T1 = CX * POS(1) + SX * POS(3)
         T2 = -SX * POS(1) + CX * POS(3)
         POS(1) = T1
         POS(3) = T2
      ELSE
C                                       Terrestial to celestial
C                                       X correction.
         T1 = CX * POS(1) - SX * POS(3)
         T2 = SX * POS(1) + CX * POS(3)
         POS(1) = T1
         POS(3) = T2
C                                       Y correction.
         T1 = CY * POS(2) + SY * POS(3)
         T2 = -SY * POS(2) + CY * POS(3)
         POS(2) = T1
         POS(3) = T2
         END IF
C
 999  RETURN
      END
