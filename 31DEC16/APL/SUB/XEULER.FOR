      SUBROUTINE XEULER (P01, TH1, PH1, P02, TH2, PH2, PHI0, THETA, PHI)
C-----------------------------------------------------------------------
C! Combine the Euler angles for two consecutive rotations into one.
C# Math Coordinates
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
C-----------------------------------------------------------------------
C   XEULER combines the Euler angles for two consecutive rotations into
C   one.
C   Inputs:
C      P01     D     The three Euler angles for the first
C      TH1     D     rotation, in degrees.
C      PH1     D
C      P02     D     The three Euler angles for the second
C      TH2     D     rotation, in degrees.
C      PH2     D
C   Output:
C      PHI0    D     The three Euler angles for the combined
C      THETA   D     rotation, in degrees.
C      PHI     D
C   Called: EULROT
C   Algorithm:
C      The two rotations are applied successively to special points in
C      the original coordinate frame.  In any rotation the north pole
C      will be transformed to polar angle  90 - THETA,  and thus THETA,
C      and likewise PHI, for the combined rotation can be found easily.
C      Determination of PHI0 is a little trickier, and requires the
C      coordinates in the original system of the north pole of the new
C      frame.  However, it is straightforward to express the inverse of
C      the combined rotation in terms of the inverse of the two
C      constituent rotations.
C   Notes:
C      1) Note that rotations are not commutative.  Therefore, the
C         order of the two rotations is important.
C
C      2) If the Euler angles for a rotation are (PHI0, THETA, PHI)
C         then for the inverse rotation they are (PHI, -THETA, PHI0).
C
C      3) If R1, R2, and R are the rotations, and r1, r2, and r are
C         their inverses then
C           R(x) = R2(R1(x))   and    r(x) = r1(r2(x)).
C   Author: Mark Calabretta, Australia Telescope.
C      Origin; 1988/Oct/05, Code last modified; 1990/Jul/25
C-----------------------------------------------------------------------
      DOUBLE PRECISION  P01, TH1, PH1, P02, TH2, PH2, PHI0, THETA, PHI
C
      DOUBLE PRECISION  AZ0, AZ1, AZ2, D2R, EULER1(5), EULER2(5), PI,
     *   POL0, POL1, POL2, ROTN
      PARAMETER (PI = 3.141592653589793238462643D0)
      PARAMETER (D2R = PI/180D0)
C-----------------------------------------------------------------------
C                                       Apply the two rotations in
C                                       succession to the north pole
      EULER1(1) = P01
      EULER1(2) = TH1
      EULER1(3) = PH1
      EULER1(4) = COS (TH1*D2R)
      EULER1(5) = SIN (TH1*D2R)
      CALL EULROT (0D0, 90D0, EULER1, AZ1, POL1, ROTN)
C
      EULER2(1) = P02
      EULER2(2) = TH2
      EULER2(3) = PH2
      EULER2(4) = COS (TH2*D2R)
      EULER2(5) = SIN (TH2*D2R)
      CALL EULROT (AZ1, POL1, EULER2, AZ2, POL2, ROTN)
C                                       THETA is easily found
      THETA =  90D0 - POL2
C                                       Now determine PHI and PHI0
C                                       THETA = zero is special case
      IF (SIN(THETA*D2R).LT.1E-9) THEN
         CALL EULROT (0D0,  0D0, EULER1, AZ1, POL1, ROTN)
         CALL EULROT (AZ1, POL1, EULER2, AZ2, POL2, ROTN)
         PHI0 = 0D0
         PHI  = AZ2
C                                       Otherwise PHI is obtained in the
C                                       same way as THETA
      ELSE
         PHI = AZ2 - 90D0
         IF (PHI.LT.0D0) PHI = PHI + 360D0
C                                       The easiest way to find PHI0 is
C                                       to apply the inverse
C                                       transformation to the north pole
C                                       of the new coordinate frame
         EULER2(1) = PH2
         EULER2(2) = -TH2
         EULER2(3) = P02
         EULER2(4) = COS (TH2*D2R)
         EULER2(5) = -SIN (TH2*D2R)
         CALL EULROT (0D0, 90D0, EULER2, AZ1, POL1, ROTN)
C
         EULER1(1) = PH1
         EULER1(2) = -TH1
         EULER1(3) = P01
         EULER1(4) = COS (TH1*D2R)
         EULER1(5) = -SIN (TH1*D2R)
         CALL EULROT (AZ1, POL1, EULER1, AZ0, POL0, ROTN)
C
         PHI0 = AZ0 + 90D0
         IF (PHI0.GE.360D0) PHI0 = PHI0 - 360D0
         END IF
C
 999  RETURN
      END
