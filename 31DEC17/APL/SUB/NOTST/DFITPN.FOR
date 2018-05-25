      SUBROUTINE DFITPN (ARG, FUNC, WT, NFIT, NEQUAT, FITPAR, VARRES,
     *   IRET)
C-----------------------------------------------------------------------
C! Fit a polynomial in double precision to a data array
C# math
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2010
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
C   Routine to fit a polynomial to the data by Least Square method
C   Fits
C      FUNC(i) = FITPAR(1) + FITPAR(2)*ARG(I) + FITPAR(3)*ARG(I)^2 +
C         FITPAR(4)*ARG(I)^3 + ...
C   Input:
C      ARG      D(*)   x: Array of data arguments
C      FUNC     D(*)   Y: Array of data function
C      NFIT     I      Number of parameters to fit, equals the
C                         polynomial degree + 1
C      NEQUAT   I      Number of points in arrays ARG, FUNC
C   Output:
C      FITPAR   D(*)   Array of fitted polynomial coefficients
C      VARRES   D      SQRT of residuals
C      IRET     I      Error; 0 => OK
C   Double precision version of POLINO uses DLESQR rather than LEASQR
C-----------------------------------------------------------------------
      INTEGER   NFIT, NEQUAT, IRET
      DOUBLE PRECISION ARG(*), FUNC(*), WT(*), VARRES, FITPAR(*)
c
      INTEGER   I, IFIT, KFIT, IKFIT
      DOUBLE PRECISION R(20), MATR(400), NOBS, SUM, SSQ, VX(20), SSQRES,
     *   VARY, FIT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (NFIT.GT.20) THEN
         IRET = 1
         WRITE (MSGTXT,1000) NFIT
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Force result vector R(NFIT),
C                                       matrix M(NFIT*NFIT) to zero
      DO 20 IFIT = 1,NFIT
         R(IFIT) = 0.0D0
         DO 10 KFIT = 1,NFIT
            IKFIT = IFIT + (KFIT - 1)*NFIT
            MATR(IKFIT) = 0.0D0
 10         CONTINUE
 20      CONTINUE
      SUM = 0.0D0
      SSQ = 0.0D0
      NOBS = 0.0D0
C                                       Prepare result vector R(NFIT)
C                                       and matrix MATR(NFIT*NFIT)
C                                       for routine LEASQR
      DO 80 I = 1,NEQUAT
         NOBS = NOBS + 1.0D0
         SUM = SUM + WT(I) * FUNC(I)
         SSQ = SSQ + WT(I) * FUNC(I)*FUNC(I)
C
         DO 60 IFIT = 1,NFIT
            R(IFIT) = R(IFIT) + WT(I) * FUNC(I) * (ARG(I) ** (IFIT - 1))
C                                       calculate upper/right
C                                       triangle of MATR
            DO 40 KFIT = IFIT, NFIT
               IKFIT = IFIT + (KFIT-1)*NFIT
               MATR(IKFIT) = MATR(IKFIT) + WT(I) *
     *            (ARG(I) ** (IFIT - 1)) * (ARG(I) ** (KFIT - 1))
 40            CONTINUE
 60         CONTINUE
 80      CONTINUE
C
      CALL DLESQR (NFIT, NOBS, SUM, SSQ, R, MATR, FITPAR, VX, SSQRES,
     *   VARRES, VARY, FIT, IRET)
      VARRES = SQRT (VARRES)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DFITPN: REQESTED POLYNOMIAL ORDER',I4,' EXCEEDS 20')
      END
