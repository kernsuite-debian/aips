      SUBROUTINE DAT2JD (IT, JD)
C-----------------------------------------------------------------------
C! converts date and time to a Julian date
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   DAT2JD converts current date/time to Real*8 Julian Date
C   Inputs:
C      IT   I(6)   yy,mm,dd  hh,mm,ss  (binary)
C                  yy should be full year: if < 40 => 2000+, else 1900+
C   Output:
C      JD   D      Julian date
C-----------------------------------------------------------------------
      INTEGER   IT(6)
      DOUBLE PRECISION  JD
C
      INTEGER   MY, MM, IC
      DOUBLE PRECISION  DCE, EPS, DYA, DMA, DZERO, D60, D24, D2, D4, D5
      DATA DCE /146097.0D0/,     DYA /1461.0D0/
      DATA DMA /153.0D0/,        EPS /0.01D0/
      DATA D2, D4, D5, D24, D60 /2.D0,4.D0,5.D0,24.D0,60.D0/
      DATA DZERO /1721119.0D0/
C-----------------------------------------------------------------------
      MY = IT(1)
      IF (MY.LT.40) MY = MY + 2000
      IF (MY.LT.200) MY = MY + 1900
      IF (IT(2).GT.2) THEN
         MM = IT(2) - 3
      ELSE
         MM = IT(2) + 9
         MY = MY - 1
         END IF
 10   IC = MY / 100
      MY = MY - IC * 100
      JD = AINT ((DCE*IC)/D4 + EPS)  +  AINT ((DYA*MY)/D4 + EPS)
     *   + AINT ((DMA*MM+D2)/D5 + EPS)  +  IT(3) + DZERO - 0.5
     *   + ((IT(6)/D60 + IT(5))/D60 + IT(4))/D24
C
 999  RETURN
      END
