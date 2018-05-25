      SUBROUTINE CRDSET (CRD1, CRD2, CRDPRM, IERR)
C-----------------------------------------------------------------------
C! Define coefficients for transforming between coordinate systems.
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
C   CRDSET defines the coefficients for transforming between spherical
C   coordinate systems.
C   Inputs
C      CRD1     C*40    Coordinate system to transform from. Options
C                       currently supported are listed below.
C      CRD2     C*40    Coordinate system to transform to.
C   Outputs:
C      CRDPRM   D(11)   Parameters for the coordinate transformation.
C                       Euler angles for the transformation.
C                       1) (PHI0) Longitude of the ascending node in the
C                          old system, in degrees.  The ascending node
C                          is the point of intersection of the equators
C                          of the two systems such that the equator of
C                          the new system crosses from south to north as
C                          viewed in the old system.
C                       2) (THETA) The angle between the poles of the
C                          two systems.  THETA is positive for a
C                          positive rotation about the ascending node.
C                       3) (PHI) Longitude of the ascending node in the
C                          new system, in degrees.
C                       4) (CTHETA) = cos(THETA).
C                       5) (STHETA) = sin(THETA).
C                       The elliptic terms of aberration which were
C                       applied to pre-IAU1976 catalogue positions.
C                       6) C term
C                       7) D term
C                       8) C*TAN(eccentricity)
C                       9-11) similarly for the output system
C      IERR     I       Error code, 0:  success
C                          1:  unrecognized coordinate system.
C   Algorithm:
C      The Euler angles for the transformation from a variety of
C      coordinate systems to Equatorial B1950.0 are stored or computed.
C      To obtain the Euler angles to go from system X to system Y, those
C      from X to B1950.0 are combined with the inverse of those from Y
C      to B1950.0.
C   References:
C      Smith, C.A., et al, 1989.  Astron. J., 97, 265.
C      Yallop, B.D., et al, 1989.  Astron. J., 97, 274.
C   Notes:
C       1) A failing of the IAU1976 system was not to give simple and
C          unambiguous names to the systems of constants and
C          coordinates.  For example, the equatorial coordinate system
C          loosely described as "J2000.0" usually means "the position at
C          epoch J2000.0 on the mean equator and equinox of J2000.0 in
C          the IAU1976 system of coordinates".  However, the J prefix
C          only indicates the new convention for computing epochs, there
C          is no formal reason to disassociate it from the old Bessel-
C          Newcomb system.
C
C          Nevertheless, one is left to assume that any coordinate
C          system associated with a Julian epoch refers to the IAU1976
C          system, and that Besselian epochs indicate Bessel-Newcomb
C          precession.  An epoch prefix of 'b' is interpreted as
C          Bessel-Newcomb without E-terms.
C
C       2) References here to "equatorial B1950.0" or "ecliptic J2000.0"
C          etc. refer strictly to space-fixed coordinate systems.  These
C          are defined by the mean equator (or ecliptic) and equinox
C          specified by the Bessel-Newcomb precession formulae for
C          Besselian epochs, or the IAU1976 system for Julian epochs.
C
C          In transforming star catalogue positions it should be noted
C          that coordinates in the "FK4" system include the E-terms of
C          aberration (less than 1 arcsec), whereas the "FK5" system
C          excludes them.  The exact transformation between these two
C          systems is therefore not a simple rotation.
C
C       3) The angles for transforming from J2000.0 to B1950.0 were
C          computed from the matrix coefficients Mij in the SLALIB
C          routine FK524 according to the following
C             PHI0  = ATAN2(-M31,M32)
C             THETA = ATAN2(STH,M33)
C             PHI   = ATAN2(M13,-M23)
C          where STH is an average
C             STH = (M31/SIN(PHI0) - M13/SIN(PHI))/2D0
C
C       4) Formulae for the precession angles are conventionally given
C          for precession from epoch E1 to epoch E2, referenced to the
C          basic epoch E0 (J2000.0 or B1900.0).  This is avoided here,
C          since it results in precession coefficients which are not
C          independent of one another.  Instead, only precession from
C          the basic epoch E0 to arbitrary epoch E is used. Precession
C          between any two arbitrary epochs is then given by the inverse
C          of precession from E0 to E1 followed by precession from E0 to
C          E2.
C
C       5) If the Euler angles for a rotation are (PHI0, THETA, PHI)
C          then for the inverse rotation they are (PHI, -THETA, PHI0).
C   Author: Mark Calabretta, Australia Telescope.
C      Origin; 1988/Oct/06. Code last modified; 1990/Aug/15
C-----------------------------------------------------------------------
      CHARACTER CRD1*40, CRD2*40
      DOUBLE PRECISION  CRDPRM(11)
      INTEGER   IERR
C                                       # coordinate systems supported
      INTEGER   NCRD
      PARAMETER (NCRD = 7)
C
      INTEGER   CTYP(2), FOUND, I, J, K, NDX1, NDX2
      DOUBLE PRECISION  CRDEUL(3,NCRD), D2R, ECCENT, EPS, EPOCH,
     *   EUL(3,2), KABERR, OBLQTY, PERIGE, PHI, PHI0, PI, T, THETA, W1,
     *   W2, W3
      CHARACTER CRD*40, CRDNAM(NCRD)*40
C
      PARAMETER (PI = 3.141592653589793238462643D0)
      PARAMETER (D2R = PI/180D0)
C                                       Euler angles defining the
C                                       transformation from each
C                                       coordinate system to equatorial
C                                       b1950.0 (PHI0, THETA, PHI).
      DATA (CRDNAM(J), (CRDEUL(I,J), I=1,3), J=1,NCRD)
     *  /'EQUATORIAL b1950.0',
     *     0.0D0, 0.0D0, 0.0D0,
     *   'EQUATORIAL b1900.0',
     *     89.6799436805556D0, +0.278397013888889D0, 90.3201112500000D0,
     *   'EQUATORIAL J2000.0',
     *     90.3202082924616D0, -0.278405865666317D0, 89.6795393829316D0,
     *   'GALACTIC',
     *     33.0D0, -62.6D0, -77.75D0,
     *   'OLD GALACTIC (OHLSSON)',
     *     -0.05634D0, -62.27390D0,  -79.38616D0,
     *   'OLD GALACTIC (VAN TULDER)',
     *     -0.06146D0, -62.77299D0,  -78.38808D0,
     *   'SUPERGALACTIC (REVISED)',
     *     +116.86867D0, +74.35592D0, +193.18941D0/
C-----------------------------------------------------------------------
C                                       Anticipate and return
C                                       immediately on error
      IERR = 1
      DO 10 J = 1,11
         CRDPRM(J) = 0D0
 10      CONTINUE
      CRDPRM(4) = 1D0
C                                       Check for a unity transformation
      CALL MATCH (CRD1, CRD2, FOUND, NDX1, NDX2)
      IF ((NDX1.EQ.1) .AND. (NDX2.EQ.1)) THEN
         IERR = 0
         GO TO 999
         END IF
C                                       Identify coordinates
      DO 30 K = 1,2
         IF (K.EQ.1) CRD = CRD1
         IF (K.EQ.2) CRD = CRD2
C
         CTYP(K) = 0
         DO 20 J = 3,NCRD
            CALL MATCH (CRD, CRDNAM(J), FOUND, NDX1, NDX2)
            IF (FOUND.GT.0) THEN
               CTYP(K) = J
               EUL(1,K) = CRDEUL(1,J)
               EUL(2,K) = CRDEUL(2,J)
               EUL(3,K) = CRDEUL(3,J)
               GO TO 30
               END IF
 20         CONTINUE
C                                       Test for equatorial coordinates
         CALL MATCH (CRD(1:10), 'EQUATORIAL', FOUND, NDX1, NDX2)
         IF (FOUND.GT.0) THEN
            READ (CRD(13:),1020,ERR=999) EPOCH
            IF ((CRD(12:12).EQ.'B') .OR. (CRD(12:12).EQ.'b')) THEN
C                                       Elliptic terms of aberration
               IF (CRD(12:12).EQ.'B') THEN
                  T = 1.00002135903D0*(EPOCH - 1950D0)/100D0
                  KABERR = 20.49552/3600D0
                  W1 = (0.00004193D0 + 0.000000126D0*T)*T
                  ECCENT = 0.01673011D0 - W1
                  W1 = (6190.67D0 + (1.65D0 + 0.012D0*T)*T)*T
                  PERIGE = (282.080541944D0 + W1/3600D0)*D2R
                  W1 = (46.8495D0 + (0.00319 - 0.00181D0*T)*T)*T
                  OBLQTY = (23.445787778D0 - W1/3600D0)*D2R
                  J = 5 + 3*(K-1)
                  CRDPRM(J+1) = -KABERR*ECCENT*COS(PERIGE)*COS(OBLQTY)
                  CRDPRM(J+2) = -KABERR*ECCENT*SIN(PERIGE)
                  CRDPRM(J+3) = CRDPRM(J+1)*TAN(OBLQTY)
                  END IF
C                                       Bessel-Newcomb precession from
C                                       B1900.0 to epoch
               T = (EPOCH - 1900D0)/100D0
               W1 = (2304.25D0  + (0.302D0 + 0.018D0*T)*T)*T
               W2 = (2004.682D0 - (0.426D0 + 0.042D0*T)*T)*T
               W3 = (2304.25D0  + (1.093D0 + 0.018D0*T)*T)*T
               PHI0  = 90D0 - W1/3600D0
               THETA = W2/3600D0
               PHI   = 90D0 + W3/3600D0
C                                       Epoch to B1900.0, followed by
C                                       B1900.0 to B1950.0
               CALL XEULER (PHI, -THETA, PHI0, CRDEUL(1,2), CRDEUL(2,2),
     *            CRDEUL(3,2), EUL(1,K), EUL(2,K), EUL(3,K))
C                                       IAU1976 precession from J2000.0
C                                       to epoch
            ELSE IF (CRD(12:12).EQ.'J') THEN
               T = (EPOCH - 2000D0)/100D0
               W1 = (2306.2181D0 + (0.30188D0 + 0.017998D0*T)*T)*T
               W2 = (2004.3109D0 - (0.42665D0 + 0.041833D0*T)*T)*T
               W3 = (2306.2181D0 + (1.09468D0 + 0.018203D0*T)*T)*T
               PHI0  = 90D0 - W1/3600D0
               THETA = W2/3600D0
               PHI   = 90D0 + W3/3600D0
C                                       Epoch to J2000.0, followed by
C                                       J2000.0 to B1950.0
               CALL XEULER (PHI, -THETA, PHI0, CRDEUL(1,3), CRDEUL(2,3),
     *            CRDEUL(3,3), EUL(1,K), EUL(2,K), EUL(3,K))
C                                       Epoch prefix not recognized
            ELSE
               GO TO 999
               END IF
            GO TO 30
            END IF
C                                       Test for ecliptic coordinates
         CALL MATCH (CRD(1:8), 'ECLIPTIC', FOUND, NDX1, NDX2)
         IF (FOUND.GT.0) THEN
            READ (CRD(11:),1020,ERR=999) EPOCH
            T = EPOCH/100D0
            IF ((CRD(10:10).EQ.'B') .OR. (CRD(10:10).EQ.'b')) THEN
C                                       Bessel-Newcomb mean obliquity of
C                                       the ecliptic
               T = (EPOCH - 1900D0)/100D0
               EPS = (84428D0 - 47D0*T)/3600D0
C                                       Bessel-Newcomb precession from
C                                       B1900.0 to epoch
               W1 = (2304.25D0  + (0.302D0 + 0.018D0*T)*T)*T
               W2 = (2004.682D0 - (0.426D0 + 0.042D0*T)*T)*T
               W3 = (2304.25D0  + (1.093D0 + 0.018D0*T)*T)*T
               PHI0  = 90D0 - W1/3600D0
               THETA = W2/3600D0
               PHI   = 90D0 + W3/3600D0
C                                       Ecliptic to equatorial, followed
C                                       by epoch to B1900.0...
               CALL XEULER (0D0, -EPS, 0D0, PHI, -THETA, PHI0, EUL(1,K),
     *            EUL(2,K), EUL(3,K))
C                                       ...followed by B1900.0 to
C                                       B1950.0
               CALL XEULER (EUL(1,K), EUL(2,K), EUL(3,K), CRDEUL(1,2),
     *            CRDEUL(2,2), CRDEUL(3,2), EUL(1,K), EUL(2,K),
     *            EUL(3,K))
            ELSE IF (CRD(10:10).EQ.'J') THEN
C                                       IAU1976 mean obliquity of the
C                                       ecliptic.
               T = (EPOCH - 2000D0)/100D0
               W1 = (46.8150D0 + (0.00059D0 - 0.001813D0*T)*T)*T
               EPS = (84381.448D0 - W1)/3600D0
C                                       IAU1976 precession from J2000.0
C                                       to epoch
               W1 = (2306.2181D0 + (0.30188D0 + 0.017998D0*T)*T)*T
               W2 = (2004.3109D0 - (0.42665D0 + 0.041833D0*T)*T)*T
               W3 = (2306.2181D0 + (1.09468D0 + 0.018203D0*T)*T)*T
               PHI0  = 90D0 - W1/3600D0
               THETA = W2/3600D0
               PHI   = 90D0 + W3/3600D0
C                                       Ecliptic to equatorial, followed
C                                       by epoch to J2000.0...
               CALL XEULER (0D0, -EPS, 0D0, PHI, -THETA, PHI0, EUL(1,K),
     *            EUL(2,K), EUL(3,K))
C                                       ...followed by J2000.0 to
C                                       B1950.0
               CALL XEULER (EUL(1,K), EUL(2,K), EUL(3,K), CRDEUL(1,3),
     *            CRDEUL(2,3), CRDEUL(3,3), EUL(1,K), EUL(2,K),
     *            EUL(3,K))
C                                       Epoch prefix not recognized
            ELSE
               GO TO 999
               END IF
            GO TO 30
            END IF
C                                       Coordinate system not recognized
         GO TO 999
 30      CONTINUE
C                                       Determine the Euler angles
C                                       Transformation from equatorial
C                                       B1950.0
      IF (CTYP(1).EQ.1) THEN
         PHI0  =  EUL(3,2)
         THETA = -EUL(2,2)
         PHI   =  EUL(1,2)
C                                       Transformation to equatorial
C                                       B1950.0
      ELSE IF (CTYP(2).EQ.1) THEN
         PHI0  = EUL(1,1)
         THETA = EUL(2,1)
         PHI   = EUL(3,1)
      ELSE
         CALL XEULER (EUL(1,1), EUL(2,1), EUL(3,1), EUL(3,2), -EUL(2,2),
     *      EUL(1,2), PHI0, THETA, PHI)
         END IF
C
      CRDPRM(1) = PHI0
      CRDPRM(2) = THETA
      CRDPRM(3) = PHI
      CRDPRM(4) = COS(THETA*D2R)
      CRDPRM(5) = SIN(THETA*D2R)
C                                       Success
      IERR = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (F9.4)
      END
