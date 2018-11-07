      SUBROUTINE POLINO(ARG, FUNC, NFIT, NEQUAT, FITPAR, VARRES, IRET)
C-----------------------------------------------------------------------
C! Fit a polinom to the given data
C# math
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
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
C   Routine to fit a polinomial to the data by Least Square method
C   Input:
C      ARG     R(*)  Array of data arguments
C      FUNC    R(*)  Array of data function
C      NFIT    I     Number of parameters to fit, equaled the polinom
C                    degree + 1
C      NEQUAT  I     Number of points at arrays ARG, FUNC
C   Output:
C      FITPAR  R(*)  Array of found fitting polinom's coefficients
C      VARRES  R     SQRT of residuals
C      IRET    I     Error; 0 => OK
C-----------------------------------------------------------------------
      INTEGER NFIT, NEQUAT, IRET
      REAL    ARG(*), FUNC(*), VARRES, FITPAR(*)
      INTEGER   I, IFIT, KFIT, IKFIT
      REAL      R(20), MATR(400), NOBS, SUM, SSQ,
     *   VX(20), SSQRES, VARY, FIT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (NFIT .GT. 20) THEN
         IRET = 1
         WRITE (MSGTXT,1000) NFIT
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Force result vector R(NFIT),
C                                       matrix M(NFIT*NFIT) to zero
      DO 20 IFIT = 1, NFIT
         R(IFIT) = 0.0
         DO 10 KFIT = 1, NFIT
            IKFIT = IFIT + (KFIT - 1)*NFIT
            MATR (IKFIT) = 0.0
 10         CONTINUE
 20      CONTINUE
      SUM = 0.0
      SSQ = 0.0
      NOBS = 0.0
C                                       Prepare result vector R(NFIT)
C                                       and matrix MATR(NFIT*NFIT)
C                                       for routine LEASQR
      DO 80 I = 1, NEQUAT
         NOBS = NOBS + 1
         SUM = SUM + FUNC(I)
         SSQ = SSQ + FUNC(I)*FUNC(I)
C
         DO 60 IFIT = 1, NFIT
            R(IFIT) = R(IFIT) + FUNC(I) * (ARG(I) ** (IFIT - 1))
C                                       calculate upper/right
C                                       triangle of MATR
            DO 40 KFIT = IFIT, NFIT
               IKFIT = IFIT + (KFIT-1)*NFIT
               MATR(IKFIT) = MATR(IKFIT) +
     *            (ARG(I) ** (IFIT - 1)) * (ARG(I) ** (KFIT - 1))
   40          CONTINUE
   60       CONTINUE
 80      CONTINUE
C
      CALL LEASQR (NFIT, NOBS, SUM, SSQ, R, MATR, FITPAR, VX, SSQRES,
     *   VARRES, VARY, FIT, IRET)
      VARRES = SQRT(VARRES)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('POLINO: The number of fitting parameters ', I4,
     *   ' exceeds 20')
      END
