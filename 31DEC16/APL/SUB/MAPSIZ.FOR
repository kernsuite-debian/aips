      SUBROUTINE MAPSIZ (NAX, NP, ISIZE)
C-----------------------------------------------------------------------
C! returns the file size needed to hold the specified image in AIPS
C# Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997
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
C   MAPSIZ computes the correct number of bytes to request from ZCREAT
C   for a file using map IO methods.
C   Inputs:
C      NAX    I        # axes
C      NP     I(NAX)   Number of points on each axis
C   Output:
C      ISIZE  I        File size in AIPS blocks (256 integers)
C-----------------------------------------------------------------------
      INTEGER   NAX, NP(*), ISIZE
C
      INTEGER   I
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Number sectors/plane
      ISIZE = 2 * NP(1)
      IF ((NAX.GE.2) .AND. (ISIZE.LE.NBPS/2)) GO TO 10
         ISIZE = 1 + (ISIZE-1) / NBPS
         IF (NAX.GE.2) ISIZE = ISIZE * NP(2)
         GO TO 20
 10   CONTINUE
         ISIZE = NBPS / ISIZE
         ISIZE = 1 + (NP(2)-1) / ISIZE
C                                       Number sectors -> bytes
 20   ISIZE = ISIZE * (NBPS / 512)
      IF (NAX.LE.2) GO TO 999
         DO 30 I = 3,NAX
            ISIZE = ISIZE * NP(I)
 30         CONTINUE
C
 999  RETURN
      END
