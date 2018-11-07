      INTEGER FUNCTION NBYPX (INCODE)
C-----------------------------------------------------------------------
C! returns number of AIPS "bytes" per pixel based on code
C# Obsolete
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
C   NBYPX returns the number of "bytes" per pixel based on the code:
C      ICODE = 1  => integer
C              2  => real
C              3  => double precision
C              4  => complex real
C              5  => complex double precision
C   It also supports 16 and 32 as 1 and 2, resp.
C-----------------------------------------------------------------------
      INTEGER   INCODE
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      NBYPX = 0
      IF ((INCODE.EQ.1) .OR. (INCODE.EQ.16)) NBYPX = 2
      IF ((INCODE.EQ.2) .OR. (INCODE.EQ.32)) NBYPX = 2
      IF (INCODE.EQ.3) NBYPX = 2 * NWDPDP
      IF (INCODE.EQ.4) NBYPX = 4
      IF (INCODE.EQ.5) NBYPX = 4 * NWDPDP
C
 999  RETURN
      END
