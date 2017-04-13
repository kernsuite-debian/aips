      SUBROUTINE FITBM (IFIELD, X, Y, CELLSP, ROT, BMAJ, BMIN, BPA)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999, 2007
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
C   FITBM fits an eliptical Gaussian given the least squares Hessian
C   matrix and residual vector.  If the fit fails a default Gaussian
C   with sigma of one cell is returned.
C   Inputs:
C      IFIELD  I      Field number in message, 0 => no field number
C                     < 0 => no message
C      X       R(3,3) Hessian matrix, (i,j) = SUM P_i * P_j where P_i is
C                     the partial derivative of each data point w.r.t
C                     parameter i and the sum is over all beam pixels
C                     included in the fit.  Parameter 1 is the
C                     coefficient of X*X, 2 of Y*Y and 3 of X*Y.
C      Y       R(3)   Residual vector (i) is the SUM over all included
C                     beam pixels of - ln(pixel_value) * P_i.
C      CELLSP  R      Cell spacing in arcseconds of the first axis.
C      ROT     R      Rotation angle on the sky in degrees.
C   Outputs:
C      BMAJ    R    Major axis size (FWHP in sec).
C      BMIN    R    Minor axis size (FWHP in sec).
C      BPA     R    Position angle of major axis (degrees).
C-----------------------------------------------------------------------
      REAL      X(3,3), Y(3), CELLSP, ROT, BMAJ, BMIN, BPA
      INTEGER   IFIELD
C
      INTEGER   JERR
      LOGICAL LERR
      CHARACTER PREFIX*5
      REAL      XFACT, C1, C2, C3, TEMP, S, C, SS, CC, XBMAJ, XBMIN
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      XFACT = ABS (CELLSP)
C                                       Solve linear equations.
C                                       Invert matrix
      CALL SYMINV (3, X, JERR)
C                                       Solution failed, write message
      IF (JERR.NE.0) THEN
         MSGTXT = 'FITBM: SOLUTION FOR RESTORING BEAM FAILED '
         CALL MSGWRT (6)
C                                       default = 3 cell circular.
         C1 = 1.5
         C2 = 1.5
         C3 = 0.0
C                                       Get Gaussian parameters
      ELSE
         C1 = Y(1) * X(1,1) + Y(2) * X(2,1) + Y(3) * X(3,1)
         C2 = Y(1) * X(1,2) + Y(2) * X(2,2) + Y(3) * X(3,2)
         C3 = Y(1) * X(1,3) + Y(2) * X(2,3) + Y(3) * X(3,3)
         END IF
C                                       Convert to sigmas and PA.
C                                       Make sure arg of ATAN
C                                       is determinate.
 100  IF (ABS (C1-C2).LT.1.0E-10) THEN
         BPA = SIGN (45.0, C3)
      ELSE
         BPA = 28.6478 * ATAN (C3 / (C1-C2))
         END IF
C                                       Compute sigma**2
      S = SIN (BPA/57.29578)
      C = COS (BPA/57.29578)
      SS = S * S
      CC = C * C
      IF (ABS (SS-CC) .GT. 1.0E-4) THEN
         BMAJ = 0.5 * (CC*CC - SS*SS) / (CC*C1 - SS*C2)
         BMIN = 0.5 * (SS*SS - CC*CC) / (SS*C1 - CC*C2)
C                                       Special case near 45 deg.
      ELSE
         BMAJ = C3 / (4.0 * S * C) + C1
         BMIN = 0.5 / (C1/CC - BMAJ)
         BMAJ = 0.5 / BMAJ
         END IF
C                                       Check if soln. is real
C                                       Use default
      IF ((BMAJ.LE.0.0) .OR. (BMIN.LE.0.0)) THEN
         C1 = 0.5
         C2 = 0.5
         C3 = 0.0
         MSGTXT = 'FITBM: SOLUTION FOR RESTORING BEAM FAILED '
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       Convert to sigmas.
      BMAJ = SQRT (BMAJ) * XFACT
      BMIN = SQRT (BMIN) * XFACT
C                                       Up to here BMAJ is minor
C                                       axis: rest of pgm wants it as
C                                       major - fix this here
      IF (BMAJ.GT.BMIN) THEN
         BPA = BPA - 90.0
      ELSE
         TEMP = BMAJ
         BMAJ = BMIN
         BMIN = TEMP
         END IF
C                                       Convert to FWHM
      BMAJ = BMAJ * 2.3548
      BMIN = BMIN * 2.3548
C                                       Add map rotation.
      BPA = BPA - ROT
      IF (BPA.GT.90.0) BPA = BPA - 180.0
      IF (BPA.LT.-90.0) BPA = BPA + 180.0
C                                       Write results.
C                                       Scale
      XBMAJ = BMAJ
      CALL METSCA (XBMAJ, PREFIX, LERR)
      XBMIN = BMIN * XBMAJ / BMAJ
      IF (IFIELD.EQ.0) THEN
         WRITE (MSGTXT,1100) XBMAJ, XBMIN, PREFIX, BPA
         CALL MSGWRT (4)
      ELSE IF (IFIELD.GT.0) THEN
         WRITE (MSGTXT,1110) IFIELD, XBMAJ, XBMIN, PREFIX, BPA
         CALL MSGWRT (4)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Fit Gaussian FWHM = ',F8.3,' x ',F8.3,1X,A,'arcsec, PA=',
     *   F7.1)
 1110 FORMAT ('Field',I5,' fit FWHM= ',F8.3,' x ',F8.3,1X,A,
     *   'arcsec, PA=',F7.1)
      END
