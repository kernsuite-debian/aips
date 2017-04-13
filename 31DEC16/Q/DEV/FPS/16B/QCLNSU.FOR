      SUBROUTINE QCLNSU (COMP, AMAP, LMAP, ABEAM, BBEAM)
C-----------------------------------------------------------------------
C! FPS AP version: Barry Clark CLEAN inner loop
C# AP-util
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
C   FPS AP version
C   QCLNSU does a CLEAN on the map points in the pseudo AP
C   using the portion of the beam in the pseudo AP.
C    Inputs:
C      COMP  I  Base address of the component vector:
C                 0 => intensity
C                 1 => x in cells
C                 2 => Y in cells
C                 3 => CLEAN loop gain (fractional)
C      AMAP  I  Base address of the map stored as (loc,intensity)
C                 Loc = (2**14)*X+Y (cells)
C                 -1<X<NX  -1<Y<NY
C      LMAP  I  Number of map points
C      ABEAM I  Base address of the piece of the beam.  The Y
C                 dimension varies the fastest: -BY<Y<BY,
C                 X varies slowest -1<X<BX
C      BBEAM I  Base address of beam descriptor vector (BX,BY)
C-----------------------------------------------------------------------
      INTEGER COMP, AMAP, LMAP, ABEAM, BBEAM
      INTEGER*2 ICOMP, IAMAP, ILMAP, IABEAM, IBBEAM
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (COMP.LT.32768) ICOMP = COMP
      IF (COMP.GE.32768) ICOMP = COMP - 65536
      IF (AMAP.LT.32768) IAMAP = AMAP
      IF (AMAP.GE.32768) IAMAP = AMAP - 65536
      IF (LMAP.LT.32768) ILMAP = LMAP
      IF (LMAP.GE.32768) ILMAP = LMAP - 65536
      IF (ABEAM.LT.32768) IABEAM = ABEAM
      IF (ABEAM.GE.32768) IABEAM = ABEAM - 65536
      IF (BBEAM.LT.32768) IBBEAM = BBEAM
      IF (BBEAM.GE.32768) IBBEAM = BBEAM - 65536
C                                        Call FPS routine.
      CALL CLNSUB (ICOMP, IAMAP, ILMAP, IABEAM, IBBEAM)
C
 999  RETURN
      END
