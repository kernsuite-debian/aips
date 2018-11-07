      SUBROUTINE QGRDCC (CMS, CMG, CMP, GRD, NY, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Grid and FT Clean components.
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999, 2006, 2008
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
C   Pseudo-AP version.
C   GRDCC is a utility routine for GRDCRM to do gridded Clean component
C   modeling.  GRDCC does a direct Fourier transform on components in
C   vector CMS to vector CMP, FFTs in Y, then swaps zero to the center.
C   It then tapers as needed and finally adds to the previous GRID
C   value.  Output is in vector CMP.  Finally GRDCC prepares CMS and the
C   phase ramp values for the next call.
C   Inputs:
C      CMS   I   Base address of component vector, increment=5:
C                  0 = Y (cells, integer)
C                  1 = Flux * cos(ux)
C                  2 = Flux * sin(ux)
C                  3 = cos(x)
C                  4 = sin(x)
C      CMG   I   Base address of Gaussian taper function (if >= 0)
C      CMP   I   Base address of complex work row. (increment = 2)
C      GRD   I   Base address of input/output row. (increment=2)
C      NY    I   Length of row (number of complex values)
C      N     I   Number of components in vector CMS
C-----------------------------------------------------------------------
      INTEGER   CMS, CMG, CMP, GRD, NY, N
C
      INTEGER   NY2, CMS1, CMS3, CMPH, CMP1
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Setup pointers etc.
      NY2 = NY * 2
      CMS1 = CMS + 1
      CMS3 = CMS + 3
      CMPH = CMP + NY
      CMP1 = CMP + 1
C                                       Clear CMP (output vector)
      CALL QVCLR (CMP, 1, NY2)
C                                       DFT in X
      CALL QDIRAD (CMS, 5, CMP, N)
C                                       FFT in Y
      CALL QCFFT (CMP, NY, 1)
C                                       Swap zero to center.
      CALL QVSWAP (CMP, 1, CMPH, 1, NY)
C                                       Taper
      IF (CMG.GE.0) THEN
         CALL QVMUL (CMG, 1, CMP, 2, CMP, 2, NY)
         CALL QVMUL (CMG, 1, CMP1, 2, CMP1, 2, NY)
         END IF
C                                       Add previous grid.
      CALL QVADD (CMP, 1, GRD, 1, GRD, 1, NY2)
C                                       Rotate DFT for next call
      CALL QCVMUL (CMS1, 5, CMS3, 5, CMS1, 5, N, 1)
C
 999  RETURN
      END
