      SUBROUTINE REAVRT (IOP, RV, IEEE)
C-----------------------------------------------------------------------
C! converts between local REAL and local INT binary forms for transport
C# FITS TV-util
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
C   converts between real numbers and a local integer form suitable for
C   ZILI16 and transport using an intermediate IEEE representation
C   Inputs:
C      IOP    I       1 => R4 -> IEEE4,    2 => R8 -> IEEE8
C                     3 => IEEE4 -> R4,    4 => IEEE8 -> R8
C   In/out:
C      RV     "R"     Local value
C      IEEE   I(4)    IEEE value in integer buffer (3-4 for R8 only)
C   Note that RV is just passed through to other routines.  Thus it can
C   have either REAL or DOUBLE PRECISION declaration.
C-----------------------------------------------------------------------
      INTEGER   IOP, IEEE(4)
      REAL      RV
C
      REAL      X
      DOUBLE PRECISION DX
C-----------------------------------------------------------------------
      IF (IOP.EQ.1) THEN
         CALL ZRLR32 (1, 1, RV, X)
         CALL ZI16IL (2, 1, X, IEEE)
      ELSE IF (IOP.EQ.2) THEN
         CALL ZRLR64 (1, 1, RV, DX)
         CALL ZI16IL (4, 1, DX, IEEE)
      ELSE IF (IOP.EQ.3) THEN
         CALL ZILI16 (2, IEEE, 1, X)
         CALL ZR32RL (1, 1, X, RV)
      ELSE IF (IOP.EQ.4) THEN
         CALL ZILI16 (4, IEEE, 1, DX)
         CALL ZR64RL (1, 1, DX, RV)
         END IF
C
 999  RETURN
      END
