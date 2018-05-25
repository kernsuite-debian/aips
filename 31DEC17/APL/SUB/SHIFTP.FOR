      SUBROUTINE SHIFTP (XSHIFT, YSHIFT, DXC, DYC, DZC)
C-----------------------------------------------------------------------
C! Calculate phase shift terms for the field center
C# EXT-appl Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999
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
C   SHIFTP calculates the X, Y and Z phase shift terms for the center of
C   the field.
C   Input:
C      XSHIFT   R        Field center shift (Arc Seconds)
C      YSHIFT   R        Field center shift (Arc Seconds)
C   Output:
C      DXC      R        U Phase term for Position offset in X
C      DYC      R        V Phase term for Position offset in Y
C      DZC      R        W Phase term for Position offset in X, Y
C   This only works when the XSHIFT,YSHIFT are already in the correct
C   (-SIN) projection and so should now be replaced by SHISIN/SHINCP.
C-----------------------------------------------------------------------
      REAL      XSHIFT, YSHIFT, DXC, DYC, DZC
C
      DOUBLE PRECISION DZCTMP
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Convert to radians
      DXC = XSHIFT * DG2RAD / 3600.0D0
      DYC = YSHIFT * DG2RAD / 3600.0D0
      DZCTMP = 1.0D0 - DXC**2 - DYC**2
      DZCTMP = SQRT (DZCTMP)
C                                       Prepare phase calc; mult by 2pi
      DXC = TWOPI * DXC
      DYC = TWOPI * DYC
      DZC = TWOPI *(DZCTMP - 1.0D0)
C
      RETURN
      END
