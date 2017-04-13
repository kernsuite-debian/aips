      SUBROUTINE VBPREC (JD, ZETA, Z, THETA, EPS)
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
C   Subroutine to calculate astronomical constants that are changing
C   due to precession.  TC is tropical centuries (36524.21988 days)
C   ELAPSED SINCE THE BEGINNING OF THE BESSELIAN YEAR DENOTED 1950.0
C   (=JULIAN DAY NUMBER 2433282.423). EXPRESSIONS ARE TAKEN FROM TECH.
C   REP. 32-1306, CONST. AND REL. INF. FOR ASTR. CAL. 1968,
C   JPL PAGE 9.                      H.NES, NTH, 1977.11.01
C   Converted to a more usable form:
C   L.B.Baath  october 1982
C-----------------------------------------------------------------------
      DOUBLE PRECISION JD, ZETA, Z, THETA, EPS, TC
      DOUBLE PRECISION PI
      DATA   PI/3.14159265358979D0/
C-----------------------------------------------------------------------
      TC = (JD-2433282.423D0) / 36524.21988D0
      ZETA = 2304.952D0*TC + 0.3022D0*TC*TC + 0.0180D0*TC*TC*TC
      ZETA = ZETA / 3600.D0 / 180.D0 * PI
      Z    = 2304.952D0*TC + 1.0951D0*TC*TC + 0.0183D0*TC*TC*TC
      Z    = Z / 3600.D0 / 180.D0 * PI
      THETA = 2004.257D0*TC - 0.4268D0*TC*TC - 0.0418D0*TC*TC*TC
      THETA = THETA / 3600.D0 / 180.D0 * PI
      EPS = -46.850D0*TC - 0.0034D0*TC*TC + 0.0018D0*TC*TC*TC
      EPS = (84404.84+EPS) / 3600.D0 / 180.D0 * PI
C
 999  RETURN
      END
