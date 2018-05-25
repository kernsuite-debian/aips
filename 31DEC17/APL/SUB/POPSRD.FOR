      SUBROUTINE POPSRD (OP, RV, DV)
C-----------------------------------------------------------------------
C! Converts POPS double real to to/from true double precision
C# POPS
C-----------------------------------------------------------------------
C;  Copyright (C) 2006
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
C   POPSRD converts the 2 real representation of doubles to or from
C   true double precision
C   Input:
C      OP   C*(*)   'R2D' or 'D2R'
C   In/out:
C      RV   R(2)    Reals
C      DV   D       Double precision
C   This routine should not be optimized in order to avoid premature
C   conversion to real
C-----------------------------------------------------------------------
      CHARACTER OP*(*)
      REAL      RV(2)
      DOUBLE PRECISION DV
C
      DOUBLE PRECISION XX
C-----------------------------------------------------------------------
      IF (OP.EQ.'R2D') THEN
         DV = RV(1)
         IF (RV(2).NE.RV(1)) DV = DV + RV(2)
      ELSE IF (OP.EQ.'D2R') THEN
         RV(1) = DV
         XX = RV(1)
         XX = DV - XX
         RV(2) = XX
         END IF
C
 999  RETURN
      END

