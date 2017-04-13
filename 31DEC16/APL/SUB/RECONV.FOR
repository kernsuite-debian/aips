      SUBROUTINE RECONV (CMAJ, CMIN, CPA, RMAJ, RMIN, RPA, FMAJ, FMIN,
     *   FPA, IERR)
C-----------------------------------------------------------------------
C! convolves two gaussians
C# Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1998
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
C   RECONV convolves a gaussian "beam" with a gaussian component.
C   Inputs:
C      CMAJ     R    Point source major axis
C      CMIN     R    Point source minor axis
C      CPA      R    Point source position angle of major axis
C      RMAJ     R    Real major axis
C      RMIN     R    Real minor axis
C      RPA      R    Real position angle of major axis
C   Outputs:
C      FMAJ     R    Fitted major axis
C      FMIN     R    Fitted minor axis
C      FPA      R    Fitted position angle of major axis
C      IERR     I    Error return: 0 => ok
C                         1-> unable to deconvolve
C-----------------------------------------------------------------------
      INTEGER   IERR
      REAL      FMAJ, FMIN, FPA, CMAJ, CMIN, CPA, RMAJ, RMIN, RPA
C
      REAL      CMJ2, CMN2, FMJ2, FMN2, CONST, RHOC, SIGIC2, DET, RMJ2,
     *   RMN2, DEN
      DATA CONST /28.647888/
C-----------------------------------------------------------------------
C                                       Get useful constants
      CMJ2 = CMAJ * CMAJ
      CMN2 = CMIN * CMIN
      RMJ2 = RMAJ * RMAJ
      RMN2 = RMIN * RMIN
      SIGIC2 = (RPA - CPA) / CONST
      DEN = CMJ2 - CMN2 + COS(SIGIC2) * (RMJ2 - RMN2)
      DET = SIN(SIGIC2) * (RMJ2 - RMN2)
      IF ((DET.EQ.0.0) .AND. (DEN.EQ.0.0)) THEN
         FPA = 0.0
      ELSE
         FPA = ATAN2 (DET, DEN)
         END IF
      DET = RMJ2 + RMN2 + CMJ2 + CMN2
      RHOC = COS (FPA)
      IF (RHOC.NE.0.0) THEN
         FMJ2 = (DET + DEN / RHOC) / 2.0
         FMN2 = (DET - DEN / RHOC) / 2.0
      ELSE
         FMJ2 = DET / 2.0
         FMN2 = FMJ2
         END IF
C                                       fix units
      FPA = FPA * CONST  + CPA
      FMAJ = SQRT (ABS (FMJ2))
      FMIN = SQRT (ABS (FMN2))
      FPA = MOD (FPA+720.0, 180.0)
      IF (FMAJ.LT.FMIN) THEN
         DET = FMAJ
         FMAJ = FMIN
         FMIN = DET
         FPA = MOD (FPA + 450.0, 180.0)
         END IF
      IERR = 0
      IF ((FMJ2.LE.0.0) .OR. (FMN2.LE.0.0)) IERR = 1
C
 999  RETURN
      END
