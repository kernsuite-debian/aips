      SUBROUTINE CRDTRN (AZ1, POL1, CRDPRM, AZ2, POL2, ROTN)
C-----------------------------------------------------------------------
C! Apply Euler angle based coordinate transformation.
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
C   CRDTRN applies the Euler angle based coordinate transformation.
C   Inputs:
C      AZ1      D       Azimuthal angle in the old system (degrees)
C      POL1     D       Polar angle in old system (degrees)
C      CRDPRM   D(11)   Parameters for the coordinate transformation.
C                       Euler angles for the transformation.
C                       1) (PHI0) Longitude of the ascending node in the
C                       old system, in degrees.  The ascending node is
C                       the point of intersection of the equators of the
C                       two systems such that the equator of the new
C                       system crosses from south to north as viewed in
C                       the old system.
C                       2) (THETA) The angle between the poles of the
C                       two systems.  THETA is positive for a positive
C                       rotation about the ascending node.
C                       3) (PHI) Longitude of the ascending node in the
C                       new system, in degrees.
C                       4) (CTHETA) = cos(THETA).
C                       5) (STHETA) = sin(THETA).
C                       The elliptic terms of aberration which were
C                       applied to pre-IAU1976 catalogue positions.
C                       6) C term
C                       7) D term
C                       8) C*TAN(eccentricity)
C                       9-11) similarly for the output system
C   Outputs:
C      AZ2      D       Azimuthal angle in the new system (degrees)
C      POL2     D       Polar angle in new system (degrees)
C      ROTN     D       The position angle of the old pole at the
C                       specified point, in degrees.
C   Called: EULROT
C   Algorithm:
C      A coordinate rotation specified by the three Euler angles.
C      The elliptic terms of aberration are applied as necessary,
C      Refer to the precursor comments to CRDSET.
C   Notes:
C   1) Longitude at the poles in the new system is consistent with that
C   specified in the old system.  This may be important when dealing
C   with map projections in which the poles are represented by finite
C   line segments.  Such is the case for cylindrical projections for
C   example.
C   2) Subroutine CRDSET may be called to set the transformation
C   parameters for any of a variety of transformations.
C   Author: Mark Calabretta, Australia Telescope.
C      Origin; 1988/Oct/05. Code last modified; 1990/Aug/15
C-----------------------------------------------------------------------
      DOUBLE PRECISION   AZ1, AZ2, POL1, POL2, CRDPRM(11), ROTN
C
      DOUBLE PRECISION   CAZ1, CAZ2, CPOL1, CPOL2, D2R, EAZ1, EAZ2,
     *   EPOL1, EPOL2, PI, SAZ1, SAZ2, SPOL1, SPOL2
C
      PARAMETER (PI = 3.141592653589793238462643D0)
      PARAMETER (D2R = PI/180D0)
C-----------------------------------------------------------------------
C                                       Remove the E-terms (non-zero
C                                       only for FK4)
      CAZ1  = COS (AZ1*D2R)
      SAZ1  = SIN (AZ1*D2R)
      CPOL1 = COS (POL1*D2R)
      SPOL1 = SIN (POL1*D2R)
      IF (CPOL1.NE.0D0) THEN
         EAZ1 = (CRDPRM(6) * CAZ1 + CRDPRM(7) * SAZ1) / CPOL1
      ELSE
         EAZ1 = 0D0
         END IF
      EPOL1 = (CRDPRM(7) * CAZ1 - CRDPRM(6) * SAZ1) * SPOL1 +
     *   CRDPRM(8) * CPOL1
C                                       Spherical coordinate rotation
      CALL EULROT (AZ1-EAZ1, POL1-EPOL1, CRDPRM, AZ2, POL2, ROTN)
C                                       Apply the E-terms (non-zero only
C                                       for FK4)
      CAZ2  = COS (AZ2*D2R)
      SAZ2  = SIN (AZ2*D2R)
      CPOL2 = COS (POL2*D2R)
      SPOL2 = SIN (POL2*D2R)
      IF (CPOL2.NE.0D0) THEN
         EAZ2 = (CRDPRM(9) * CAZ2 + CRDPRM(10) * SAZ2) / CPOL2
      ELSE
         EAZ2 = 0D0
         END IF
      EPOL2 = (CRDPRM(10) * CAZ2 - CRDPRM(9) * SAZ2) * SPOL2 +
     *   CRDPRM(11) * CPOL2
C
      AZ2  = AZ2  + EAZ2
      POL2 = POL2 + EPOL2
C                                       Normalize
      AZ2 = MOD (AZ2, 360D0)
      IF (AZ2.LT.0D0) AZ2 = AZ2 + 360D0
C
 999  RETURN
      END
