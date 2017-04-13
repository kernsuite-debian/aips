      SUBROUTINE G3SCAL (NPIX, INB, NOFM, ROFM, GOFM, BOFM, ROU, GOU,
     *   BOU)
C-----------------------------------------------------------------------
C! scale a buffer by 3 color lookup tables
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2002, 2008
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
C   Convert a grey-scale buffer into 3 colors with OFMs
C   Inputs:
C      NPIX   I        Pixels/line
C      INB    I(*)     Input buffer
C      NOFM   I        Number values in OFM
C      ROFM   R(*)     Red OFM memory (0-1)
C      GOFM   R(*)     Green OFM memory (0-1)
C      BOFM   R(*)     Blue OFM memory (0-1)
C   Outputs:
C      ROU    I(*)     output red buffer with scaled data
C      GOU    I(*)     output green buffer with scaled data
C      BOU    I(*)     output blue buffer with scaled data
C-----------------------------------------------------------------------
      INTEGER   NPIX, INB(*), NOFM, ROU(*), GOU(*), BOU(*)
      REAL      ROFM(*), GOFM(*), BOFM(*)
C
      INTEGER   I, J
      DOUBLE PRECISION RMAX, RMIN, SCL, T
      INCLUDE 'INCS:DGPH.INC'
C-----------------------------------------------------------------------
      RMIN = GPHTLO
      RMAX = GPHTHI
      SCL = (RMAX - RMIN) / (NOFM - 1.0D0)
      DO 10 I = 1,NPIX
         J = (INB(I) - RMIN) / SCL + 1.5D0
         T = ROFM(J) * RMAX + (1.0-ROFM(J)) * RMIN
         IF (T.GE.0.0D0) THEN
            ROU(I) = T + 0.5D0
         ELSE
            ROU(I) = T - 0.5D0
            END IF
         T = GOFM(J) * RMAX + (1.0-GOFM(J)) * RMIN + 0.5
         IF (T.GE.0.0D0) THEN
            GOU(I) = T + 0.5D0
         ELSE
            GOU(I) = T - 0.5D0
            END IF
         T = BOFM(J) * RMAX + (1.0-BOFM(J)) * RMIN + 0.5
         IF (T.GE.0.0D0) THEN
            BOU(I) = T + 0.5D0
         ELSE
            BOU(I) = T - 0.5D0
            END IF
 10      CONTINUE
C
 999  RETURN
      END
