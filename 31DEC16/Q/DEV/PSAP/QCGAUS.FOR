      SUBROUTINE QCGAUS (APBASE, NX, NY, BMAJ, BMIN, BPA)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Create a complex Gaussian centered at NX/2,NY/2+1
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2012
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
C   Pseudo-AP version
C   Create a complex Gaussian centered at NX/2,NY/2+1
C   Inputs:
C      APBASE  I  Base address of complex Gaussian image.
C      NX      I  X dimension of image
C      NY      I  Y dimension of image
C      BMAJ    R  Gaussian major axis in cells
C      BMIN    R  Gaussian minor axis in cells
C      BPA     R  Gaussian position angle in degrees
C-----------------------------------------------------------------------
      INTEGER   APBASE, NX, NY
      REAL      BMAJ, BMIN, BPA
C
      INCLUDE 'INCS:DAPC.INC'
      INCLUDE 'INCS:PSTD.INC'
      LONGINT   JB
      INTEGER   LOOPX, LOOPY
      DOUBLE PRECISION X, Y, XC, YC, XX, YY, CPHI, SPHI, YSSPHI, YSCPHI
C-----------------------------------------------------------------------
C                                       Addresses 1-rel
      JB = APBASE + PSAPOF
C                                       Set image center
      XC = NX / 2
      YC = NY / 2 + 1
C                                       Trig. combinations of model p.a.
      SPHI = SIN (BPA * DG2RAD)
      CPHI = COS (BPA * DG2RAD)
C                                       Loop
      INCLUDE 'INCS:ZVND.INC'
      DO 200 LOOPY = 1,NY
         Y = LOOPY - YC
         YSCPHI = Y * CPHI
         YSSPHI = Y * SPHI
      INCLUDE 'INCS:ZVND.INC'
         DO 100 LOOPX = 1,NX
            X = LOOPX - XC
            XX = (YSCPHI - X*SPHI) / BMAJ
            YY = (X*CPHI + YSSPHI) / BMIN
C                                       Gaussian; 2.772588722 = 4ln(2)
            APCORE(JB) = EXP (-2.772588722D0 * (XX*XX + YY*YY))
            APCORE(JB+1) = 0.0
            JB = JB + 2
 100        CONTINUE
 200     CONTINUE
C
 999  RETURN
      END
