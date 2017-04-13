      SUBROUTINE QTAPER (GPARMS, UU, DV, N, CMG)
C-----------------------------------------------------------------------
C! Pseudo AP routine: prepare Gaussian taper
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1999, 2002, 2006, 2012
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
C   Pseudo-AP version.
C   QTAPER is a utility routine for model subtraction of Gaussian
C   components.  It computes the Gaussian taper for a row at UU and V
C   pixels from -n/2*DV to n/2*dv.
C   Inputs:
C      GPARMS   R(3)   Bmaj, Bmin, Bpa in degrees
C      UU       R      U value in wavelengths
C      DV       R      V increment per pixel
C      N        I      Number of Y pixels
C      CMG      I      Base address of multiplier vector
C   Output: APCORE(CMG and up) taper multiplier
C-----------------------------------------------------------------------
      INTEGER   N, CMG
      REAL      GPARMS(3), UU, DV
C
      DOUBLE PRECISION VV, B1, B2, B3, CPA, SPA, XMAJ, XMIN, CONST, TEXP
      INTEGER   NY, I
      LONGINT   CM
      INCLUDE 'INCS:DAPC.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Setup pointers etc.
      NY = N + 1
      CM = CMG + PSAPOF
C                                       compute beam constants
      CONST = DG2RAD * PI * SQRT (0.5D0) / 1.17741022D0
      XMAJ = GPARMS(1) * CONST
      XMIN = GPARMS(2) * CONST
      CPA = COS (DG2RAD * GPARMS(3))
      SPA = SIN (DG2RAD * GPARMS(3))
      B1 = -(((CPA*XMAJ)**2) + ((SPA*XMIN)**2))
      B2 = -(((SPA*XMAJ)**2) + ((CPA*XMIN)**2))
      B3 = - 2.0D0 * SPA * CPA * (XMAJ*XMAJ - XMIN*XMIN)
C                                       loop to compute
      B2 = B2 * UU * UU
      B3 = B3 * UU
      VV = - N * DV / 2.0D0
      DO 10 I = 1,NY
         TEXP = B1 * VV * VV + B2 + B3 * VV
         IF (TEXP.GT.-14.0D0) THEN
            APCORE(CM) = EXP (TEXP)
         ELSE
            APCORE(CM) = 0.0D0
            END IF
         CM = CM + 1
         VV = VV + DV
 10      CONTINUE
C
 999  RETURN
      END
