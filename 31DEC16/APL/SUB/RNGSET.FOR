      SUBROUTINE RNGSET (IR, MMAX, MMIN, RANG)
C-----------------------------------------------------------------------
C! set plat intensity range from image header and user parameters
C# Plot-util
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
C   RNGSET calculates range parameters for displaying a map using the
C   IRANGE adverb supplied by POPS plus scaling information derived
C   from the map header.
C   Inputs:
C      IR     R(2)   Range values specified by user
C      MMAX   R      Map maximum value from header
C      MMIN   R      Map minimum value from header
C   Outputs:
C      RANG   R(2)   Output range values calculated using defaults
C-----------------------------------------------------------------------
      REAL      IR(2), MMAX, MMIN, RANG(2)
C
      REAL      LIMIT, RM
      DATA LIMIT /100.0/
C-----------------------------------------------------------------------
C                                       general default: map range
      IF (IR(2).LE.IR(1)) THEN
         RANG(2) = MMAX
         RANG(1) = MMIN
C                                       limit to reasonable range
      ELSE
         RANG(2) = IR(2)
         RANG(1) = IR(1)
         RM = LIMIT * ABS (MMAX - MMIN)
         IF ((RANG(2).LT.MMIN) .OR. (RANG(2)-MMAX.GE.RM)) RANG(2) = MMAX
         IF ((RANG(1).GT.MMAX) .OR. (MMIN-RANG(1).GE.RM)) RANG(1) = MMIN
         END IF
C
 999  RETURN
      END
