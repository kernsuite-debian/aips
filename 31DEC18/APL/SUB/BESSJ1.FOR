      FUNCTION BESSJ1 (X)
C-----------------------------------------------------------------------
C! Computes the J1 Bessel function for argumanet X
C# Math
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
C   Compute the J1 Bessel function for a given argument
C   Inputs:
C      X     R    The argument of the J1 Bessel function desired
C   Output:
C            R    Returns the resultant J1 Bessel function
C   Method
C      from "Numerical Recipes" by Press et al.
C-----------------------------------------------------------------------
      REAL      BESSJ1, X
C
      DOUBLE PRECISION P1, P2, P3, P4, P5, Q1, Q2, Q3, Q4, Q5, R1, R2,
     *   R3, R4, R5, R6, S1, S2, S3, S4, S5, S6, Y, AX, XX, Z
      SAVE P1, P2, P3, P4, P5, Q1, Q2, Q3, Q4, Q5, R1, R2, R3, R4, R5,
     *   R6, S1, S2, S3, S4, S5, S6
      DATA R1, R2, R3, R4, R5, R6 /72362614232.D0, -7895059235.D0,
     *   242396853.1D0, -2972611.439D0, 15704.48260D0, -30.16036606D0/
      DATA S1, S2, S3, S4, S5, S6 /144725228442.D0, 2300535178.D0,
     *   18583304.74D0, 99447.43394D0, 376.9991397D0, 1.D0/
      DATA P1, P2, P3, P4, P5 /1.D0, .183105D-2, -.3516396496D-4,
     *   .2457520174D-5, -.240337019D-6/
      DATA Q1, Q2, Q3, Q4, Q5 /.04687499995D0, -.2002690873D-3,
     *   .8449199096D-5, -.88228987D-6, .105787412D-6/
C-----------------------------------------------------------------------
      IF (ABS(X).LT.8.) THEN
         Y = X**2
         BESSJ1 = X*(R1+Y*(R2+Y*(R3+Y*(R4+Y*(R5+Y*R6))))) /
     *      (S1+Y*(S2+Y*(S3+Y*(S4+Y*(S5+Y*S6)))))
      ELSE
         AX = ABS (X)
         Z = 8. / AX
         Y = Z**2
         XX = AX - 2.356194491
         BESSJ1 = SIGN (1., X) * SQRT (.636619772/AX) *
     *      (COS(XX)*(P1+Y*(P2+Y*(P3+Y*(P4+Y*P5)))) -
     *       Z*SIN(XX)*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*Q5)))))
         END IF
C
 999  RETURN
      END
