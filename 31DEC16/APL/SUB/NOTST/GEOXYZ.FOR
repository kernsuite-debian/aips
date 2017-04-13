      SUBROUTINE GEOXYZ( MODE, LONG, LAT, HT, X, Y, Z, IERR )
C-----------------------------------------------------------------------
C! Make conversions between geodetic and geocentric coordinates.
C# Math Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 2002
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
C   Routine to make conversions between geodetic and geocentric
C   coordinates.
C   Algorithms from the 1992 Astronomical Almanac Explanatory
C   Supplement, Chapter 4, are used.  The IERS 1989 ellipsoid
C   is used (Semi-major axis and inverse flattening).
C   The routine results are at the cm level accuracy.
C   This routine is based on a SCHED routine written by Craig Walker.
C
C   Inputs:
C     MODE     I  0 => Input data are longitude, latitude and height,
C                 either geocentric or geodetic (height < 1E6)
C                 Output data are geodetic long, lat, and height and
C                 geocentric X, Y, and Z.
C                 1 => Input data are geocentric X, Y, and Z.
C                 Output data are geodetic long, lat, and height
C                 above the geoid.
C   Input/Output:
C     LONG     D  Geodetic(geocentric) longitude; positive to west,
C                 in radians
C     LAT      D  Geodetic(geocentric) latitude, in radians
C     HT       D  Height above the geoid, in meters
C                 If (at input) HT.GT.1E6, LONG, LAT are geocentric
C     X,Y,Z    D  Geocentric equatorial coordinates at the right hand
C                 coordinate system
C   Output:
C     IERR     I  Return code.  0 => ok.  1 => bad mode.
C-----------------------------------------------------------------------
      INTEGER           MODE, IERR
      DOUBLE PRECISION  LONG, LAT, HT, X, Y, Z
      DOUBLE PRECISION  SMAXIS, FLATEN, C, S
      PARAMETER         (SMAXIS=6378137.D0)
      PARAMETER         (FLATEN=1.D0/298.257223563D0)
      DOUBLE PRECISION  SLAT, CLAT, SLONG, CLONG
      DOUBLE PRECISION  B, R, E, F, P, Q, D, NU, G, T, PI
      LOGICAL           MAKEGD
C ----------------------------------------------------------------------
      PI = DATAN( 1.0D0 ) * 4.D0
      MAKEGD = MODE .EQ. 1
      IERR = 1
      IF( MODE .EQ. 0 ) THEN
C
C        Input is long, lat, and ht of some sort.
C
         IF( HT .LT. 1.D6 ) THEN
C
C           Input coordinates are geodetic long, lat, ht.
C
            SLAT = DSIN( LAT )
            CLAT = DCOS( LAT )
            SLONG = DSIN( -LONG )
            CLONG = DCOS( -LONG )
            C = CLAT**2 + ( 1.D0 - FLATEN )**2 * SLAT**2
            C = 1.D0 / SQRT( C )
            S = ( 1.D0 - FLATEN )**2 * C
            X = ( SMAXIS * C + HT ) * CLAT * CLONG
            Y = ( SMAXIS * C + HT ) * CLAT * SLONG
            Z = ( SMAXIS * S + HT ) * SLAT
            IERR = 0
         ELSE
C
C           Input coordinates are geocentric long, lat, ht.
C           Derive X, Y, and Z.  Then convert angles to
C           geodetic in section for data starting with X, Y, Z.
C
            X = HT * COS( LAT ) * COS( -LONG )
            Y = HT * COS( LAT ) * SIN( -LONG )
            Z = HT * SIN( LAT )
            MAKEGD = .TRUE.
            END IF
         END IF
C
C     Now do any required geocentric to geodetic conversion.
C
      IF( MAKEGD ) THEN
         R = SQRT( X**2 + Y**2 )
C
C        Deal with singular cases.
C
         IF( Z .EQ. 0.D0 .AND. R .NE. 0.D0 ) THEN
C
C           On equitorial plane.
C
            LAT = 0.D0
            LONG = -DATAN2( Y, X )
            HT = SQRT( X**2 + Y**2 ) - SMAXIS
C
         ELSE IF( R .EQ. 0.D0 .AND. Z .NE. 0.D0 ) THEN
C
C           On Z axis.
C
            LAT = DSIGN( PI / 2.D0, Z )
            LONG = 0.D0
            HT = Z - SMAXIS * ( 1.D0 - FLATEN )
C
         ELSE IF( R .EQ. 0.D0 .AND. Z .EQ. 0.D0 ) THEN
C
C           Center of earth.
C
            LAT = 0.D0
            LONG = 0.D0
            HT = -SMAXIS
         ELSE
C
C           Reasonable range.
C
            B = DSIGN( SMAXIS * ( 1.D0 - FLATEN ), Z )
            E = ( B * Z - ( SMAXIS**2 - B**2 ) ) / ( SMAXIS * R )
            F = ( B * Z + ( SMAXIS**2 - B**2 ) ) / ( SMAXIS * R )
            P = ( 4.0D0 / 3.0D0 ) * ( E * F + 1.D0 )
            Q = 2.D0 * ( E**2 - F**2 )
            D = P**3 + Q**2
            IF( D .LT. 0.D0 ) THEN
               NU = 2.D0 * SQRT( -P ) * COS( (1.D0/3.D0) *
     1              ACOS( Q / ( P * SQRT( -P ) ) ) )
            ELSE
               NU = ( SQRT( D ) - Q )**(1.D0/3.D0) -
     1              ( SQRT( D ) + Q )**(1.D0/3.D0)
            END IF
C
C           Deal with cases near singularities as per Almanac.
C           The criteria are a wild guess.
C
            IF( Z .LT. SMAXIS / 1.D5 .OR. R .LT. SMAXIS / 1.D5 ) THEN
               NU = (-1.D0) * ( NU**3 + 2.D0 * Q ) / ( 3.D0 * P )
            END IF
            G = 0.5D0 * ( SQRT( E**2 + NU ) + E )
            T = SQRT( G**2 + ( F - NU * G ) / ( 2.D0 * G - E ) ) - G
            LAT = DATAN( SMAXIS * ( 1.D0 - T**2 ) / ( 2.D0 * B * T ) )
            HT = ( R - SMAXIS * T ) * COS( LAT ) +
     1           ( Z - B ) * SIN( LAT )
            LONG = -DATAN2( Y, X )
         END IF
         IERR = 0
      END IF
C
      RETURN
      END
