      SUBROUTINE SHFTXY (RA, DEC, ROTATE, XRA, XDEC, XSHIFT, YSHIFT)
C-----------------------------------------------------------------------
C! Returns shifts of coordinates
C# Map Header coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 1999
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
C   Finds coordinate shift from RA,DEC to XRA,XDEC
C   Input:
C      RA       D   Initial RA in degrees: reference point
C      DEC      D   Initial Declination in degrees: reference point
C      ROTATE   R   Image rotation in degrees
C      XRA      D   RA of shifted point in degrees
C      XDEC     D   Declination of shifted point in degrees
C   Output
C      XSHIFT   R   Shift in "X" in arc sec
C      YSHIFT   R   Shift in "Y" in arc sec
C   No longer implements -SIN projection
C-----------------------------------------------------------------------
      DOUBLE PRECISION RA, DEC, XRA, XDEC
      REAL      XSHIFT, YSHIFT, ROTATE
C
      REAL      MAPRR
      DOUBLE PRECISION XXSHFT, YYSHFT
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      MAPRR = ROTATE * DG2RAD
C                                       L, M: SIN projection
C     XXSHFT = COS (DG2RAD*XDEC) * SIN (DG2RAD*(XRA-RA))
C     YYSHFT = SIN (DG2RAD*XDEC) * COS (DG2RAD*DEC) -
C    *   COS (DG2RAD*XDEC) * SIN (DG2RAD*DEC) * COS (DG2RAD*(XRA-RA))
C                                       now do simple "linear" terms
      XXSHFT = COS (DG2RAD*DEC) * (XRA - RA)
      YYSHFT = XDEC - DEC
C                                       Undo rotation
      XSHIFT = (COS (MAPRR) * XXSHFT + SIN (MAPRR) * YYSHFT) * 3600.0
      YSHIFT = (-SIN (MAPRR) * XXSHFT + COS (MAPRR) * YYSHFT) * 3600.0
C
 999  RETURN
      END
