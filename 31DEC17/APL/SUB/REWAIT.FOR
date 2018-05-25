      SUBROUTINE REWAIT (LWT, WT)
C-----------------------------------------------------------------------
C! alters data weight by specified function
C# Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2004
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
C   Alters data weights.
C   Inputs:
C      LWT   I   1 - sqrt
C                2 - wt**0.25
C                3 - 1.0
C   In/out:
C      WT    R   Data weight changed if > 0.
C-----------------------------------------------------------------------
      INTEGER   LWT
      REAL      WT
C
C-----------------------------------------------------------------------
      IF (WT.GT.0.0) THEN
         IF (LWT.EQ.1) THEN
            WT = SQRT (WT)
         ELSE IF (LWT.EQ.2) THEN
            WT = WT ** 0.25
         ELSE IF (LWT.EQ.3) THEN
            WT = 1.0
            END IF
         END IF
C
 999  RETURN
      END
