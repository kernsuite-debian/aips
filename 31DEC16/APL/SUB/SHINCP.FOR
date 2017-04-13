      SUBROUTINE SHINCP (RA, DEC, ROTATE, XRA, XDEC, DXC, DYC, DZC)
C-----------------------------------------------------------------------
C! Returns shifts of coordinates in phase-shift units (-NCP projection)
C# Map Header coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 1999
C;  Associated Universities, Inc. Washington DC, USA.
C;  Eric W. Greisen
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
C   Finds coordinate shift frm RA,DEC to XRA, XDEC and phase terms for
C   -NCP projection
C   Input:
C      RA       D   Initial RA in degrees: reference position
C      DEC      D   Initial Declination in degrees
C      ROTATE   R   Image rotation in degrees
C      XRA      D   RA of shifted point in degrees
C      XDEC     D   Declination of shifted point in degrees
C   Output
C      DXC      R        U Phase term for Position offset in X
C      DYC      R        V Phase term for Position offset in Y
C      DZC      R        W Phase term for Position offset in X, Y
C-----------------------------------------------------------------------
      DOUBLE PRECISION RA, DEC, XRA, XDEC
      REAL      ROTATE, DXC, DYC, DZC
C
      REAL      MAPRR, XSHIFT, YSHIFT
      DOUBLE PRECISION XXSHFT, YYSHFT
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      MAPRR = ROTATE * DG2RAD
C                                       L, M
      XXSHFT = COS (DG2RAD*XDEC) * SIN (DG2RAD*(XRA-RA))
      YYSHFT = SIN (DG2RAD * DEC)
      IF (YYSHFT.NE.0.0D0) THEN
         YYSHFT = (COS (DG2RAD*DEC) - COS (DG2RAD*XDEC) *
     *       COS (DG2RAD*(XRA-RA))) / YYSHFT
      ELSE
         YYSHFT = XDEC - DEC
         END IF
C                                       Undo rotation
      XSHIFT = (COS (MAPRR) * XXSHFT + SIN (MAPRR) * YYSHFT) / DG2RAD *
     *   3600.0
      YSHIFT = (-SIN (MAPRR) * XXSHFT + COS (MAPRR) * YYSHFT) / DG2RAD *
     *   3600.0
C                                       Convert to radians
      DXC = XSHIFT * DG2RAD / 3600.0D0
      DYC = YSHIFT * DG2RAD / 3600.0D0
C                                       Prepare phase calc; mult by 2pi
      DXC = TWOPI * DXC
      DYC = TWOPI * DYC
      DZC = 0.0
C
 999  RETURN
      END
