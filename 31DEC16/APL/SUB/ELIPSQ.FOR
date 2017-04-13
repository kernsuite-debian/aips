      SUBROUTINE ELIPSQ (BMAJ0, BMIN0, BPA0, CX, CY, BMAJ1, BMIN1, BPA1)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002
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
C   ELIPSQ computes the parameters of an ellipse which has undergone
C   a scaling transformation, X0 = CX*X1, Y0 = CY*Y1.
C   Inputs:
C      BMAJ0   R   Major axis length before scaling.
C      BMIN0   R   Minor axis length before scaling.
C      BPA0    R   Position angle of the major axis before scaling,
C                  measured anticlockwise from the Y0 axis in degrees.
C      CX      R   Scaling coefficient in X.
C      CY      R   Scaling coefficient in Y.
C   Outputs:
C      BMAJ1   R   Major axis length after scaling.
C      BMIN1   R   Minor axis length after scaling.
C      BPA1    R   Position angle of the major axis after scaling,
C                  measured anticlockwise from the Y1 axis in degrees.
C   Algorithm:
C      In the coordinate system (X,Y) defined by
C             X  =  X0*COS(BPA0) + Y0*SIN(BPA0)
C             Y  = -X0*SIN(BPA0) + Y0*COS(BPA0)
C      the equation of the ellipse before scaling is
C             (X/BMIN0)**2 + (Y/BMAJ0)**2 = 1
C      and the scaling transformation is
C             X0 = CX*X1
C             Y0 = CY*Y1.
C      In the coordinate system (X',Y') defined by
C             X' =  X1*COS(BPA1) + Y1*SIN(BPA1)
C             Y' = -X1*SIN(BPA1) + Y1*COS(BPA1)
C      the equation of the ellipse after scaling is
C             (X'/BMIN1)**2 + (Y'/BMAJ1)**2 = 1.
C   Thus, the equation of the ellipse after scaling may be expressed
C      in terms of (BMAJ0, BMIN0, BPA0), and again in terms of (BMAJ1,
C      BMIN1, BPA1).  By equating coefficients in X1**2, X1*Y1, and
C      Y1**2, a solution for the latter triad may be found in terms of
C      the former.
C   Notes:
C      1) The axis lengths must be in consistent units.
C      2) If CX=CY, then the conversion is just a length conversion of
C      the majpr and minor axes.
C   Author: Mike Kesteven and Mark Calabretta, Australia Telescope.
C-----------------------------------------------------------------------
      REAL      BMAJ0, BMIN0, BPA0, CX, CY, BMAJ1, BMIN1, BPA1
C
      DOUBLE PRECISION AXX, AXY, AYY, COSPA0, SINPA0, TMP, ARG
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Initialize.
      COSPA0 = COS (BPA0 * DG2RAD)
      SINPA0 = SIN (BPA0 * DG2RAD)
      BMAJ1 = 0.0
      BMIN1 = 0.0
      BPA1   = 0.0
C                                       simple scaling
      IF ((CX.EQ.CY) .AND. (CX.NE.0.0)) THEN
         BPA1 = BPA0
         BMAJ1 = BMAJ0 / CX
         BMIN1 = BMIN0 / CX
C                                       Transform the ellipse to the
C                                       new coordinate system.
      ELSE IF ((BMAJ0.NE.0.0) .AND. (BMIN0.NE.0.0)) THEN
         AXX = (CX * SINPA0 / BMAJ0)**2 + (CX * COSPA0 / BMIN0)**2
         AYY = (CY * COSPA0 / BMAJ0)**2 + (CY * SINPA0 / BMIN0)**2
         AXY = 2.0D0 * CX * CY * COSPA0 * SINPA0 *
     *      (1.0D0 / (BMIN0**2) - 1.0D0 / (BMAJ0**2))

C                                       Compute new position angle.
         IF ((AXY.NE.0.0) .OR. (AXX.NE.AYY)) THEN
            BPA1 = ATAN2 (AXY, AXX-AYY) / 2.0D0 * RAD2DG
         ELSE
            BPA1 = 0.0
            END IF
C                                       Compute the new major and minor
C                                       axis lengths.
         TMP = SQRT ((AXX-AYY)**2 + AXY**2)
         ARG = (AXX + AYY - TMP)
         BMIN1 = SQRT (2.0 / (AXX + AYY + TMP))
         IF (ARG.GT.0.0D0) BMAJ1 = SQRT (2.0 / ARG)
         END IF
C
 999  RETURN
      END
