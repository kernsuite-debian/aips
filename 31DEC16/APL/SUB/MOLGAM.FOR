      SUBROUTINE MOLGAM (THETA, GAMMA, IERR)
C-----------------------------------------------------------------------
C! solves transcendental equation for Molweide's projection
C# Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 2013
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
C   Solves the equation
C      sin(theta) = 2 Gamma / pi + sin(2 Gamma) / pi
C   Inputs:
C      THETA   D   Angle radians
C   Outputs
C      GAMMA   D   solution
C      IERR    I   Error
C-----------------------------------------------------------------------
      DOUBLE PRECISION THETA, GAMMA
      INTEGER   IERR
C
      INTEGER   I, NC
      DOUBLE PRECISION VAL, XMIN, XMAX, X, DX, DIST, XP, V
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      VAL = PI * SIN (THETA)
      XMIN = -TWOPI * 2.0D0
      XMAX = -XMIN
      NC = 0
      IERR = 0
 10   IF (NC.LT.10) THEN
         DX = (XMAX - XMIN) / 999.0D0
         DIST = 1.D10
         X = XMIN
         DO 20 I = 1,1000
            V = X + SIN(X)
            IF (ABS(V-VAL).LT.DIST) THEN
               XP = X
               DIST = ABS (V-VAL)
               GAMMA = X / 2.0D0
               END IF
            X = X + DX
 20         CONTINUE
         IF ((DIST.LT.1.D5) .AND. (DIST.GT.1.D-5)) THEN
            XMIN = XP - 2.0D0 * DX
            XMAX = XP + 2.0D0 * DX
            GO TO 10
            END IF
         END IF
C
      IF (DIST.GT.1.E-3) IERR = 3
C
 999  RETURN
      END
