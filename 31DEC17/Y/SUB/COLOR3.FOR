      SUBROUTINE COLOR3 (VAL, CIRCLE, COL)
C-----------------------------------------------------------------------
C! determines a circular color to give to a value
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2003
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
C   COLOR3 uses an STC algorithm like COLORH but to return a single
C   color
C   Inputs:
C      VAL      I      Value 0.0 - 1.0
C      CIRCLE   L      T -> full circle of color in the 0-1 range
C                      F -> R -> G -> B only in the 0-1 range
C   Output:
C      COL      R(3)   RGB color (0.0-1.0)
C-----------------------------------------------------------------------
      REAL      VAL, COL(3)
      LOGICAL   CIRCLE
C
      REAL      HUE, V
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      V = VAL
      IF ((V.LT.-0.01) .OR. (V.GT.1.01)) V = MOD (V+1000.0, 1.0)
      V = MAX (0.0, MIN (1.0, V))
C                                       simple method
      HUE = DG2RAD * 180.0 * V
      IF (CIRCLE) HUE = HUE * 1.5
      IF (HUE.LE.PI/2.0D0) THEN
         COL(1) = COS (HUE)
         COL(2) = SIN (HUE)
         COL(3) = 0.0
      ELSE IF (HUE.LE.PI) THEN
         COL(1) = 0.0
         COL(2) = SIN (HUE)
         COL(3) = -COS (HUE)
      ELSE
         COL(1) = -SIN (HUE)
         COL(2) = 0.0
         COL(3) = -COS (HUE)
         END IF
C
 999  RETURN
      END
