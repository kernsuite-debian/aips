      SUBROUTINE EULROT (AZ1, POL1, EULER, AZ2, POL2, ROTN)
C-----------------------------------------------------------------------
C! Apply Euler angle based rotation of spherical coordinates.
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
C   EULROT applies the Euler angle based rotation of spherical
C   coordinates.
C   Inputs
C      AZ1     D      Azimuthal and polar angles in the old
C      POL1    D      coordinate system, in degrees.
C      EULER   D(5)   Euler angles for the rotation.
C                     1) (PHI0) Longitude of the ascending node in the
C                        old system, in degrees.  The ascending node is
C                        the point of intersection of the equators of
C                        the two systems such that the equator of the
C                        new system crosses from south to north as
C                        viewed in the old system.
C                     2) (THETA) The angle between the poles of the two
C                        systems.  THETA is positive for a positive
C                        rotation about the ascending node.
C                     3) (PHI) Longitude of the ascending node in the
C                        new system, in degrees. 
C                     4) (CTHETA) = cos(THETA).
C                     5) (STHETA) = sin(THETA).
C   Outputs;
C      AZ2     D      Azimuthal and polar angles in the new coordinate
C      POL2    D      system, in degrees.  AZ2 lies between 0 and 360
C      ROTN    D      The position angle of the old pole at the
C                     specified point, in degrees.
C   Algorithm:
C      A coordinate rotation specified by the three Euler angles.
C   Notes:
C      1) Longitude at the poles in the new system is consistent with
C         that specified in the old system.  This may be important
C         when dealing with map projections in which the poles are
C         represented by finite line segments.  Such is the case for
C         cylindrical projections for example.
C   Author: Mark Calabretta, Australia Telescope.
C      Origin; 1988/Oct/05. Code last modified; 1990/Aug/15
C-----------------------------------------------------------------------
      DOUBLE PRECISION   AZ1, POL1, EULER(5), AZ2, POL2, ROTN
C
      DOUBLE PRECISION   AZ1P, CAZ1P, CPOL1, CTHETA, D2R, PHI0, PHI, PI,
     *   R2D, SAZ1P, SPOL1, STHETA, THETA, X, Y
      PARAMETER (PI = 3.141592653589793238462643D0)
      PARAMETER (D2R = PI/180D0)
      PARAMETER (R2D = 180D0/PI)
C-----------------------------------------------------------------------
C                                       The following assignments are
C                                       made for convenience
      PHI0   = EULER(1)
      THETA  = EULER(2)
      PHI    = EULER(3)
      CTHETA = EULER(4)
      STHETA = EULER(5)
C                                       Compute trigonometric functions
C                                       of the old coordinates
      AZ1P  = AZ1 - PHI0
      CAZ1P = COS (AZ1P*D2R)
      SAZ1P = SIN (AZ1P*D2R)
      CPOL1 = COS (POL1*D2R)
      SPOL1 = SIN (POL1*D2R)
C                                       Compute the azimuthal angle in
C                                       the new system
      X = CPOL1 * CAZ1P
      Y = CPOL1 * SAZ1P *CTHETA + SPOL1 * STHETA
      IF ((X.NE.0D0) .OR. (Y.NE.0D0)) THEN
         AZ2 = PHI + ATAN2 (Y, X) * R2D
C                                       Longitude at the poles in the
C                                       new system is consistent with
C                                       that specified in the old system
      ELSE
         AZ2 = PHI + AZ1P
         END IF
C                                       Compute the polar angle in the
C                                       new system
      POL2 = ASIN (SPOL1*CTHETA - CPOL1*STHETA*SAZ1P) * R2D
C                                       Compute the position angle of
C                                       the old pole at this point
      X = SAZ1P * STHETA * SPOL1 + CTHETA * CPOL1
      Y = CAZ1P * STHETA
      ROTN = ATAN2 (Y, X) * R2D
C                                       Normalize
      AZ2 = MOD (AZ2, 360D0)
      IF (AZ2.LT.0D0) AZ2 = AZ2 + 360D0
C
 999  RETURN
      END
