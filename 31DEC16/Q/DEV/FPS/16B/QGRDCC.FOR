      SUBROUTINE QGRDCC (CMS, CMP, GRD, NY, N)
C-----------------------------------------------------------------------
C! FPS AP version: Grids and Fourier transforms Clean components
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
C   GRDCC is a utility routine for MX subroutine MXSGRD.  GRDCC
C   does a direct Fourier transform on components in vector CMS to
C   vector CMP, FFTs in Y, then swaps zero to the center.
C   Then adds the previous GRID value.  Output is in vector
C   CMP.  Finally GRDCC prepares CMS and the phase ramp values for
C   the next call.
C    Inputs:
C     CMS I   Base address of component vector, increment=5:
C                 0 = Y (cells, integer)
C                 1 = Flux * cos(ux)
C                 2 = Flux * sin(ux)
C                 3 = cos(x)
C                 4 = sin(x)
C     CMP  I  Base address of complex output row. (increment = 2)
C     GRD  I  Base address of previous grid. (increment=2)
C     NY   I  Length of row (number of complex values)
C     N    I  Number of components in vector CMS
C-----------------------------------------------------------------------
      INTEGER CMS, CMP, GRD, NY, N
      INTEGER*2 ICMS, ICMP, IGRD, INY, IN
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (CMS.LT.32768) ICMS = CMS
      IF (CMS.GE.32768) ICMS = CMS - 65536
      IF (CMP.LT.32768) ICMP = CMP
      IF (CMP.GE.32768) ICMP = CMP - 65536
      IF (GRD.LT.32768) IGRD = GRD
      IF (GRD.GE.32768) IGRD = GRD - 65536
      IF (NY.LT.32768) INY = NY
      IF (NY.GE.32768) INY = NY - 65536
      IF (N.LT.32768) IN = N
      IF (N.GE.32768) IN = N - 65536
C                                        Call FPS routine.
      CALL GRDCC (ICMS, ICMP, IGRD, INY, IN)
C
 999  RETURN
      END
