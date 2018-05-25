      SUBROUTINE XYSHFT (RA, DEC, XSHIFT, YSHIFT, ROTATE, XRA, XDEC)
C-----------------------------------------------------------------------
C! Returns shifted coordinates
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
C   Shifts coordinates from RA, DEC
C   Input:
C      RA       D   Initial RA in degrees: ref point
C      DEC      D   Initial Declination in degrees: ref point
C      XSHIFT   R   Shift in "X" in arc sec - simple shifts not -SIN
C      YSHIFT   R   Shift in "Y" in arc sec
C      ROTATE   R   Image rotation in degrees
C   Output
C      XRA      D   RA of shifted point in degrees
C      XDEC     D   Declination of shifted point in degrees
C-----------------------------------------------------------------------
      DOUBLE PRECISION RA, DEC, XRA, XDEC
      REAL      XSHIFT, YSHIFT, ROTATE
C
      REAL      MAPRR
      DOUBLE PRECISION XXINC, XXSHFT, YYSHFT
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      MAPRR = ROTATE * DG2RAD
C                                       Undo rotation
      XXSHFT = (COS (MAPRR) * XSHIFT - SIN (MAPRR) * YSHIFT) / 3600.0
      YYSHFT = (SIN (MAPRR) * XSHIFT + COS (MAPRR) * YSHIFT) / 3600.0
C                                       -SIN shifts
C     XXINC = ATAN (XXSHFT / (YYSHFT * SIN (DEC * DG2RAD) -
C    *   COS (DEC * DG2RAD) * SQRT ( 1.0 - XXSHFT * XXSHFT -
C    *   YYSHFT * YYSHFT))) / DG2RAD
C     XRA = RA - XXINC
C     XDEC = ASIN (YYSHFT * COS (DEC * DG2RAD) +
C    *   SQRT (1.0 - XXSHFT * XXSHFT - YYSHFT * YYSHFT) *
C    *   SIN (DEC * DG2RAD)) / DG2RAD
C                                       simple shifts
      XDEC = DEC + YYSHFT
      XXINC = COS (DG2RAD * DEC)
      IF (XXINC.NE.0) THEN
         XRA = RA + XXSHFT / XXINC
      ELSE
         XRA = RA
         END IF
C
 999  RETURN
      END
