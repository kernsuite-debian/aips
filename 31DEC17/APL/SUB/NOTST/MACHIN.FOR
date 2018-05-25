      SUBROUTINE MACHIN (U)
C-----------------------------------------------------------------------
C! Returns the smallest positive value that added to 1.0 is .gt. 1.0.
C# Math
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
C   U is the smallest positive number such that (1.0+U) .gt. 1.0 .
C   U is computed approximately as a power of 1/2.
C   This code is completely explained and documented in the text:
C   Computer solution of ordinary differential equations:  The initial
C   value problem  by L. F. Shampine and M. K. Gordon.
C-----------------------------------------------------------------------
      REAL   U, HALFU, TEMP1
C-----------------------------------------------------------------------
      HALFU = 0.5
   50 TEMP1 = 1.0 + HALFU
      IF(TEMP1.LE.1.0) GO TO 100
         HALFU = 0.5 * HALFU
         GO TO 50
  100 U = 2.0 * HALFU
C
 999  RETURN
      END
