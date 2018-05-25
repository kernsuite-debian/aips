      SUBROUTINE QMULCL (COMP, FLDECS, ABEAM, BBEAM, NCOMP)
C-----------------------------------------------------------------------
C! FPS AP version: Higher lever Clark CLEAN routine
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
C   Calls B. Clark's QCLNSU routine multiple times to
C   accumulate a number of CLEAN components in one AP call.
C     Inputs:
C      COMP  I  Base address of component vector 4 words per comp.:
C                  1 = component intensity.
C                  2 = x location (cells)
C                  3 = y location (cells)
C                  4 = field number
C                  NOTE: this vector is the output of this routine.
C      FLDESC I  Field descriptor vector base address.
C                  0 = Number of fields (REAL value)
C                   for each field:
C                  0 = Start address of residuals (REAL value) (AMAP)
C                     (location,intensity), intensity=2**14*x + y
C                  1 = Number of points in the map. (REAL value) (LMAP)
C                  2 = Current max. flux density (loc = FCOMP)
C                  3 = X position of current max.
C                  4 = Y position of current max.
C                  5 = CLEAN gain
C   ABEAM     I  Base address of the square (folded) beam patch.
C                  The Y direction is most rapid variable:-BY<Y<BY,
C                  X varies less rapidly, -1<X<BX
C   BBEAM     I  Beam description vector (BX,BY).
C                  In the current QCLNSU BX must equal BY.
C   NCOMP     I  Number of components to CLEAN.
C-----------------------------------------------------------------------
      INTEGER COMP, FLDECS, ABEAM, BBEAM, NCOMP
      INTEGER*2 ICOMP, IFLDEC, IABEAM, IBBEAM, INCOMP
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (COMP.LT.32768) ICOMP = COMP
      IF (COMP.GE.32768) ICOMP = COMP - 65536
      IF (FLDECS.LT.32768) IFLDEC = FLDECS
      IF (FLDECS.GE.32768) IFLDEC = FLDECS - 65536
      IF (ABEAM.LT.32768) IABEAM = ABEAM
      IF (ABEAM.GE.32768) IABEAM = ABEAM - 65536
      IF (BBEAM.LT.32768) IBBEAM = BBEAM
      IF (BBEAM.GE.32768) IBBEAM = BBEAM - 65536
      IF (NCOMP.LT.32768) INCOMP = NCOMP
      IF (NCOMP.GE.32768) INCOMP = NCOMP - 65536
C                                        Call FPS routine.
      CALL MULCLN (ICOMP, IFLDEC, IABEAM, IBBEAM, INCOMP)
C
 999  RETURN
      END
