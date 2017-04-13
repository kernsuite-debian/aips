      SUBROUTINE JULDAY (CLD, JD)
C-----------------------------------------------------------------------
C! converts a character-encoded calendar date to Julian day number
C# Utility Header FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2000
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
C   converts a character encoded Gregorian calendar date into a Julian
C   day number.
C   Inputs:
C       CLD   C*8   Gregorian calander date in form 'DD/MM/YY' or in new
C                   form 'YYYYMMDD'
C   Output:
C       JD    D     Julian date.
C   Algorithm from ACM Algorithm number 199
C   NOTE:  This routine is good from 1 Mar 1900 indefinitely.
C-----------------------------------------------------------------------
      CHARACTER CLD*8
      DOUBLE PRECISION JD
C
      INTEGER   IDATE(3)
      DOUBLE PRECISION K, D, M, YA, C
C-----------------------------------------------------------------------
C                                       Get date
      CALL DATEST (CLD, IDATE)
C                                       Convert to D
      YA = IDATE(1)
      M  = IDATE(2)
      D  = IDATE(3)
      IF (M.LE.2.0D0) THEN
         M = AINT (M + 9.0D0)
         YA = AINT (YA - 1.0D0)
      ELSE
         M = AINT (M - 3.0D0)
         END IF
      C = AINT (YA/100.0D0)
      YA = YA - 100.0D0 * C
      K = AINT (146097.D0 * C * 0.25D0) + AINT (1461.0D0 * YA * 0.25D0)
     *   + AINT ((153.0D0 * M + 2.0D0) * 0.2D0) + D
C                                       Following good for > 20th cent.
      JD = AINT (K) + 1721118.50D0
      GO TO 999
C                                       Error
 900  JD = 0.0D0
C
 999  RETURN
      END
