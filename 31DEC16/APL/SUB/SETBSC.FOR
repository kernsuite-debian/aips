      SUBROUTINE SETBSC (RMAX, RMIN, BSC, BZE)
C-----------------------------------------------------------------------
C! determines scaling/offset parameters to convert image to integer
C# Map-util FITS
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
C   SETBSC computes the data scaling parameters for conversion from
C   floating point to integer maps.  Its intent is to have a true data
C   0 value be an exact integer on output.
C   Inputs:
C      RMAX   R      Highest data value
C      RMIN   R      Lowest data value
C   Output:
C      BSC    D      Multiplier: real = BSC * integer + BZE
C      BZE    D      Offset: ditto
C   Common
C      /DCHCOM/ for NBITWD  (to force 16 or 32 bit, then - save local
C                           NBITWD, set 16/32, call SETBSC, restore
C                           local NBITWD)
C-----------------------------------------------------------------------
      REAL      RMAX, RMIN
      DOUBLE PRECISION BSC, BZE
C
      DOUBLE PRECISION DMAX, DMIN, DT, DTT
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Basic scaling
      DMAX = RMAX
      DMIN = RMIN
      DT = 2.0D0 ** MIN (32, NBITWD) - 8.0D0
      BSC = (DMAX - DMIN) / DT
      BZE = (DMAX + DMIN) / 2.0D0
C                                       constant image
      IF (BSC.NE.0.0D0) GO TO 10
         BSC = 1.0D0
         GO TO 999
C                                       reset for 0.0 integer
 10   DT = -BZE / BSC
      DT = DT + 0.5D0
      DTT = MOD (DT, 1.0D0)
      IF ((DT.LT.0.0D0) .AND. (DTT.GT.0.0D0)) DTT = 1.0D0 - DTT
      DT = DT - DTT
      BZE = -DT * BSC
C
 999  RETURN
      END
