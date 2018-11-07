      SUBROUTINE QGRD4 (UV, VIS, WT, GRID, CONX, CONY, NO2, M, LROW,
     *   INC, NVIS)
C-----------------------------------------------------------------------
C! FPS AP version: Convolves UV data onto a grid
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
C   QGRD4 convolves visibility data onto a grid.
C   A single channel is gridded at a time.
C   It assumes that NO points lie within one half the
C   convolving function support size of the outside edge.
C    Inputs:
C     UV    I   Location of (u,v) values in cells.
C     VIS   I   Location of (complex) visibilities.
C     WT    I   Weight for data. Assumes any tapering
C                 has already been done.
C     GRID  I   base address of gridded data.
C                 Order assumed to be the following
C                 for each of the M rows:
C                  1) 2 * LROW visibilities
C     CONX  I   base address of X convolving fn.
C     CONY  I   base address of Y convolving fn.
C     NO2   I   INT( (# cells used on a row) / 2 )
C     M     I   number of rows kept in the AP.
C     LROW  I   length of a row ( max. X).
C     INC   I   increment for UV, VIS and WT
C     NVIS  I   number of visibilities to grid.
C      In the above, X refers to rows and y to columns
C      in the gridded data, NOT on the sky.  The total
C      numbers of rows and cells used on a row should
C      be odd.
C     All AP memory I/O values are assumed floating.
C     It is assumed that all values of v correspond to row M/2.
C-----------------------------------------------------------------------
      INTEGER UV, VIS, WT, GRID, CONX,  CONY, NO2, M, LROW,
     *   INC, NVIS
      INTEGER*2 IUV, IVIS, IWT, IGRID, ICONX, ICONY, INO2, IM, ILROW,
     *   IINC, INVIS
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (UV.LT.32768) IUV = UV
      IF (UV.GE.32768) IUV = UV - 65536
      IF (VIS.LT.32768) IVIS = VIS
      IF (VIS.GE.32768) IVIS = VIS - 65536
      IF (WT.LT.32768) IWT = WT
      IF (WT.GE.32768) IWT = WT - 65536
      IF (GRID.LT.32768) IGRID = GRID
      IF (GRID.GE.32768) IGRID = GRID - 65536
      IF (CONX.LT.32768) ICONX = CONX
      IF (CONX.GE.32768) ICONX = CONX - 65536
      IF (CONY.LT.32768) ICONY = CONY
      IF (CONY.GE.32768) ICONY = CONY - 65536
      IF (NO2.LT.32768) INO2 = NO2
      IF (NO2.GE.32768) INO2 = NO2 - 65536
      IF (M.LT.32768) IM = M
      IF (M.GE.32768) IM = M - 65536
      IF (LROW.LT.32768) ILROW = LROW
      IF (LROW.GE.32768) ILROW = LROW - 65536
      IF (INC.LT.32768) IINC = INC
      IF (INC.GE.32768) IINC = INC - 65536
      IF (NVIS.LT.32768) INVIS = NVIS
      IF (NVIS.GE.32768) INVIS = NVIS - 65536
C                                        Call FPS routine.
      CALL APGRD4 (IUV, IVIS, IWT, IGRID, ICONX, ICONY, INO2, IM, ILROW,
     *   IINC, INVIS)
C
 999  RETURN
      END
