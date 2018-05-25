      SUBROUTINE MP2IMA (MPIX, IMPIX)
C-----------------------------------------------------------------------
C! convert image pixel positions to TV pixel positions
C# Catalog TV-util
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
C   Subroutine to convert map pixel locations to TV image pixel
C   locations using the image catalog, passed through common /MAPHDR/
C   Inputs:
C      MPIX     R(2)     x and y map pixel locations
C   Output:
C      IMPIX    R(2)     x and y TV image pixel locations
C   Common:
C      /MAPHDR/ CATBLK(256)  Image catalog block
C-----------------------------------------------------------------------
      REAL      IMPIX(2), MPIX(2)
C
      REAL      TEMP
      INTEGER   CATBLK(256), I, J
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
C-----------------------------------------------------------------------
      DO 10 I = 1,2
         TEMP = 1.0
         J = I - 1
         IF (CATBLK(IICOR+2+J).NE.CATBLK(IICOR+J)) TEMP =
     *      REAL (CATBLK(IIWIN+J+2) - CATBLK(IIWIN+J)) /
     *      REAL (CATBLK(IICOR+2+J) - CATBLK(IICOR+J))
         IMPIX(I) = CATBLK(IICOR+J) + (MPIX(I)-CATBLK(IIWIN+J)) / TEMP
 10      CONTINUE
C
 999  RETURN
      END
