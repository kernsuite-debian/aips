      SUBROUTINE QINTP (UV, VIS, PHAS, GRID, INTP, M, LROW, INC, NVIS)
C-----------------------------------------------------------------------
C! FPS AP version: Interpolates model visibilities from a grid
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
C   Interpolates model visibilities from a grid,
C   corrects the phase and subtracts from the observations.
C   Assumes that the observations are never within M/2
C   of the outside edge of the grid.
C    Inputs:
C     UV    I  Base address of (u,v) values in cells.
C     VIS   I  Base address of (complex) visibilities.
C     PHAS  I  Base address of phase correction, complex value
C                to be multiplied by model vis.
C     GRID  I  Base address of gridded model vis. Order assumed to
C                be the following  for each of the M rows:
C                               1) 2 * LROW visibilities
C     INTP  I  Base address of interpolation function.
C     M     I  Number of rows kept in the AP.  Also support size of
C                interp. fn.
C     LROW  I  Length of a row.
C     INC   I  Increment for UV,VIS and PHAS
C     NVIS  I  Number of visibility points to grid.
C   All AP memory values are assumed floating.
C   It is assumed that all values of v correspond to row M/2.
C-----------------------------------------------------------------------
      INTEGER UV, VIS, PHAS, GRID, INTP, M, LROW, INC,  NVIS
      INTEGER*2 IUV, IVIS, IPHAS, IGRID, IINTP, IM, ILROW, IINC, INVIS
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (UV.LT.32768) IUV = UV
      IF (UV.GE.32768) IUV = UV - 65536
      IF (VIS.LT.32768) IVIS = VIS
      IF (VIS.GE.32768) IVIS = VIS - 65536
      IF (PHAS.LT.32768) IPHAS = PHAS
      IF (PHAS.GE.32768) IPHAS = PHAS - 65536
      IF (GRID.LT.32768) IGRID = GRID
      IF (GRID.GE.32768) IGRID = GRID - 65536
      IF (INTP.LT.32768) IINTP = INTP
      IF (INTP.GE.32768) IINTP = INTP - 65536
      IF (M.LT.32768) IM = M
      IF (M.GE.32768) IM = M - 65536
      IF (LROW.LT.32768) ILROW = LROW
      IF (LROW.GE.32768) ILROW = LROW - 65536
      IINC = INC
      IF (NVIS.LT.32768) INVIS = NVIS
      IF (NVIS.GE.32768) INVIS = NVIS - 65536
C                                        Call FPS routine.
      CALL APINTP (IUV, IVIS, IPHAS, IGRID, IINTP, IM, ILROW, IINC,
     *   INVIS)
C
 999  RETURN
      END
