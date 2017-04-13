       SUBROUTINE IMGSIZ (CATB, CATR, MX, MY, XOFF, YOFF, IERR)
C-----------------------------------------------------------------------
C! returns image size to uss plus offsets in FFT
C# AP-fft utility
C-----------------------------------------------------------------------
C;  Copyright (C) 2008
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
C   Determines the power of 2 image size to contain a subimage with
C   the reference pixel centered on the field
C   Input:
C      CATB   I(256)    Image header as I
C      CATR   R(256)    Image header as float
C   Output:
C      MX     I         X image size
C      MY     I         Y image size
C      XOFF   I         X offset in pixels
C      YOFF   I         Y offset in pixels
C      IERR   I         0 okay, 1 reference pixel not an integer
C-----------------------------------------------------------------------
      INTEGER   CATB(256), MX, MY, XOFF, YOFF, IERR
      REAL      CATR(256)
C
      INTEGER   IROUND, IX, IY, IM
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       X axis
      IERR = 0
      IX = IROUND (CATR(KRCRP))
      IF (ABS(IX-CATR(KRCRP)).GT.0.02) IERR = 1
      IM = 2 * MAX (IX, CATB(KINAX)-IX) - 1
      CALL POWER2 (IM, MX)
      MX = 2 * MX
C                                       Y axis
      IY = IROUND (CATR(KRCRP+1))
      IF (ABS(IY-CATR(KRCRP+1)).GT.0.02) IERR = 1
      IM = 2 * MAX (IY-1, CATB(KINAX+1)-IY+1) - 1
      CALL POWER2 (IM, MY)
      MY = 2 * MY
      IF (CATB(KINAX).EQ.CATB(KINAX+1)) THEN
         MX = MAX (MX, MY)
         MY = MX
         END IF
      YOFF = MY / 2 + 1 - IY
      XOFF = MX / 2 - IX
C
 999  RETURN
      END
