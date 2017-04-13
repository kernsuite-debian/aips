      SUBROUTINE DOPLR (RA, DEC, NYR, NDAY, UT, X, Y, Z,
     *     VSUN, VT)
C-----------------------------------------------------------------------
C! Doppler velocity calculations
C# Spectral
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
C  Routine DOPLR calculates the velocity component of the observer
C  with respect to the local standard of rest as projected onto
C  a line specified by the right ascension and declination (epoch=
C  date) for a specified time.
C  The location of the observer is specified by the geocentric
C  coordinates X, Y and Z (in metres)
C
C  This version of dop takes into account components of the observer's
C  motion due to the rotation of the earth, the revolution of the
C  earth-moon barycenter about the sun, and the motion of the earth's
C  centre about the earth-moon barycenter.  The perturbations of the
C  earth's orbit due to planets are neglected.  The absolute precision
C  of this version of dop is about 0.004 km/sec, but since the dominant
C  error term is slowly varying the relative error will be considerably
C  less for times up to a week or so.
C
C  The routine omits the planetary perturbations on the earth's orbit.
C  They amount to about 0.003 km/sec and are thought to be the largest
C  contribution to the error in the velocity.
C  Inputs:
C    RA             D           Right Ascension of date (radians)
C    DEC            D           Declination of date (radians)
C    NYR            I           Year of observation
C    NDAY           I           Day number of observation
C    UT             D           UT of datum (days)
C    X,Y,Z          D           Geocentric station coords. (metres)
C
C  Outputs:
C    VSUN           D           The component of the sun's motion
C                               with respect to the lsr as projected
C                               onto the line of sight to the source.
C                               (m/s)
C    VT             D           The total velocity component ditto
C                               (m/s)
C-----------------------------------------------------------------------
      INTEGER    NYR, NDAY, JULDA
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION   ZRO, AAA, DD, DELA, DELDD, XO, YO, ZO,
     *         RA, DEC, UT, LONG, VSUN, VT, RA1, PIE,
     *         CC, CS, S, CAT, WLONG, DU, TU, UTDA, SMD, T,
     *         START, C1, GST, XLST, RHO, VRHO, DLAT, VOBS,
     *         AM, E, AI, VS, XLAM, ALAM, AA, AN, HOP,
     *         V, OMGA, OMGAR, AMON, GAMP, PIM, EM, OLAMM,
     *         AIM, AMM, VSM, ALAMM, ANM, AAM, HOPM, VMON,
     *         BETA, DC, ALONG, ALGM, BETAM, X, Y, Z
C-----------------------------------------------------------------------
C                                      Vel of Sun wrt lsr is
C                                      20 km/s towards RA 18 hrs
C                                      Dec +30 deg
      ZRO = 0.D0
      AAA = 18.D0 * PI / 12.D0
      DD  = 30.D0 * PI / 180.D0
C                                      precesses this direction
C                                      to date
      CALL RDMOVE( 1900, NYR, 1, NDAY, AAA, DD, DELA, DELDD, DC)
      AAA = AAA + DELA
      DD = DD + DELDD
C                                      this velocity is converted
C                                      to cartesian components
      XO = 20.D0 * COS(AAA) * COS(DD)
      YO = 20.D0 * SIN(AAA) * COS(DD)
      ZO = 20.D0 * SIN(DD)
C                                      ra1 = RA in days
      RA1 = (RA * (12.D0 / PI) ) / 24.D0
C                                      CC, CS, and S are
C                                      the direction cosines
C                                      corresponding to ra and dec
      CC = COS(DEC) * COS(RA)
      CS = COS(DEC) * SIN(RA)
      S = SIN(DEC)
C                                      VSUN is the projection onto
C                                      the line of sight to the
C                                      star of the sun's motion wrt
C                                      to the lsr (km/sec)
      VSUN = -XO * CC - YO * CS - ZO * S
C
C                                      determine geodetic lat & long.
      IF ((X.EQ.0.D0) .AND. (Y.EQ.0.D0) .AND. (Z.EQ.0.D0)) THEN
         CAT = 0.D0
         LONG = 0.D0
      ELSE
         CALL GEODLL (X, Y, Z, CAT, LONG)
         END IF
      WLONG = LONG / (2.D0 * PI)
C                                      Time calculations
C                                      The epoch is 1900 jan 0.5 UT
C                                       = julian day 2415020.0
C                                      DU is the time from epoch
C                                      to jan 0.0 of the current year
C                                      (days)
      DU = (JULDA(NYR) - 2415020) - 0.5D0
C                                      TU is DU converted to julian
C                                      centuries
      TU = DU / 36525.0D0
C                                      UTDA is the gmt from
C                                      jan 0.0 to the present (days)
      UTDA = NDAY + UT
C                                      SMD is the time from the epoch
C                                      to the present (days)
      SMD = DU + UTDA
C                                      T is SMD converted to julian
C                                      centuries
      T = SMD / 36525.D0
C                                      START is the greenwich mean
C                                      sidereal time on jan 0.0 (days)
      START = (6.D0 + 38.D0 / 60.D0 + (45.836D0 + 129.1794D0 +
     *   8640184.542D0 * (TU-0.7D0) + 0.0929D0 * TU **2)
     *   / 3600.D0) / 24.D0
C                                      C1 is the conversion factor from
C                                      solar time to sidereal time
      C1 = 0.997269566414D0
C                                      GST is the greenwich mean
C                                      sidereal time (days)
      GST = START + UTDA / C1
C                                      XLST is the local mean sidereal
C                                      time (from jan 0) (days)
      XLST = GST - WLONG
      XLST = XLST - INT(XLST)
C                                      Observer's motion wrt earth's
C                                      centre
C                                      RHO is the radius vector from
C                                      the earth's center to the
C                                      observer
      RHO = SQRT (X*X + Y*Y + Z*Z)
C                                      VRHO is corresponding circular
C                                      velocity (meters/sidereal day)
      VRHO = 2.D0 * PI * RHO
C                                      converted to kilometers/sec
      VRHO = VRHO / 24.0D3 / 3600.D0 / C1
C                                      reduction of geodetic latitude
C                                      to geocentric latitude (arcsecs)
      DLAT = -(11.D0 * 60.D0 + 32.7430D0) * SIN(2.D0 * CAT)
     *   + 1.1633D0 * SIN(4.D0 * CAT) -0.0026D0 * SIN(6.D0 * CAT)
C                                      convert CAT to geocentric lat
C                                      (radians)
      CAT = CAT + DLAT * PI / 3600.D0 / 180.D0
C                                      VOBS is the projection onto the
C                                      line of sight to the star of the
C                                      velocity of the observer wrt the
C                                      earth's center (km/sec)
      VOBS = VRHO * COS(CAT) * COS(DEC) *
     *   SIN(2.D0 * PI * (XLST - RA1))
C                                      Earth's orbit about sun
C                                      AM is the mean anomaly (of the
C                                      earth's orbit) (radians)
      AM = (358.47583D0 + 0.9856002670D0 * SMD-0.000150D0 * T**2 -
     *   0.000003D0 * T**3) * PI / 180.0D0
C                                      PIE is the mean longitude of
C                                      perihelion (radians)
      PIE = (101.22083D0 + 0.0000470684D0 * SMD + 0.000453D0 * T**2 +
     *   0.000003D0 * T**3) * PI / 180.0D0
C                                      E is the eccentricity of
C                                      the orbit (dimensionless)
      E = 0.01675104D0 - 0.00004180D0 * T - 0.000000126D0 * T**2
C                                      AI is the mean obliquity
C                                      of the ecliptic (radians)
      AI = (23.452294D0 - 0.0130125D0 * T - 0.00000164D0 * T**2 +
     *   0.000000503D0 * T**3) * PI / 180.0D0
C                                      VS is the true anomaly
C                                      (approximate formula) (radians)
      VS = AM + (2.D0 * E - 0.25D0 * E**3) * SIN(AM) + 1.25D0
     *   * E**2 * SIN(2.D0 * AM) + 13.D0 / 12.D0 * E**3
     *   * SIN(3.D0 * AM)
C                                      XLAM is the true longitude
C                                      of the earth as seen from
C                                      the sun (radians)
      XLAM = PIE + VS
C                                      ALAM is the true longitude
C                                      of the sun as seen from the
C                                      earth (radians)
      ALAM = XLAM + PI
C                                      BETA is the latitude of the
C                                      star (radians). ALONG is the
C                                      longitude of the star (radians)
      CALL CDTRN (ZRO, ZRO, (-PI/2.D0), (PI/2.D0 - AI),
     *   RA, DEC, ALONG, BETA)
C                                      AA is the semi-major axis of
C                                      the earth's orbit (km)
      AA = 149598500.0D0
C                                      AN is the mean angular rate
C                                      of the earth about the sun
C                                      (radians/day)
      AN = 2.D0 * PI / 365.2564D0
C                                      HOP is h/p from smart = the
C                                      component of the earth's
C                                      velocity perpendicular to the
C                                      radius vector (km/day)
      HOP = AN * AA / SQRT(1.D0 - E**2)
C                                      converted to km/sec
      HOP = HOP / 86400.D0
C                                      V is the projection onto the
C                                      line of sight to the star of
C                                      the velocity of the earth-moon
C                                      barycenter with respect to the
C                                      sun (km/sec)
      V = -HOP * COS(BETA) * (SIN(ALAM - ALONG) - E
     *   * SIN(PIE - ALONG))
C                                      Calculate moon's orbit around
C                                      the earth-moon barycentre.
C                                      OMGA is the longitude of the
C                                      mean ascending node of the lunar
C                                      pole (degrees)
      OMGA = 259.183275D0 - 0.0529539222D0 * SMD + 0.002078D0
     *   * T**2 + 0.000002D0 * T**3
C                                      OMGAR is omga in radians
      OMGAR = OMGA * PI / 180.D0
C                                      AMON is OMGA plus the mean
C                                      lunar longitude of the moon
C                                      (degrees - should be
C                                       13.1763965268)
      AMON = 270.434164D0 + 13.176396527D0 * SMD - 0.001133D0
     *   * T**2 + 0.0000019D0 * T**3
C                                      GAMP (gamma-prime) is OMGA
C                                      plus the lunar longitude of
C                                      lunar perigee
      GAMP = 334.329556D0 + 0.1114040803D0 * SMD - 0.010325D0
     *   * T**2 - 0.000012D0 * T**3
C                                      PIM is the mean lunar longitude
C                                      of lunar perigee (in radians)
      PIM = (GAMP - OMGA) * PI / 180.D0
C                                      EM is the eccentricity of the
C                                      lunar orbit
      EM = 0.054900489D0
C                                      OLAMM is the mean lunar long.
C                                      of the moon (in radians)
      OLAMM = (AMON - OMGA) * PI / 180.D0
C                                      AIM is the inclination of the
C                                      lunar orbit to the ecliptic
C                                      (radians)
      AIM = 5.1453964D0 * PI / 180.D0
C                                      AMM is the approximate mean
C                                      anomaly (radians)
      AMM = OLAMM - PIM
C                                      VSM is the true anomaly
C                                      (approx formula) (radians)
      VSM = AMM + (2.D0 * EM - 0.25D0 * EM**3) * SIN(AMM)
     *   + 1.25D0 * EM**2 * SIN(2.D0 *AMM) + 13.D0 /
     *   12.D0 * EM**3 * SIN(3.D0 * AMM)
C                                      ALAMM is the true lunar
C                                      longitude of the moon (radians)
      ALAMM = PIM + VSM
C                                      ANM is the mean angular rate of
C                                      the lunar rotation (radians/day)
      ANM = 2.D0 * PI / 27.321661D0
C                                      AAM is the semi-major axis of
C                                      the lunar obrit (kilometers)
      AAM = 60.2665D0 * 6378.388D0
C                                      BETAM is the lunar latitude of
C                                      the star (rads). ALGM is the
C                                      lunar longitude of the star.
      CALL CDTRN (OMGAR, ZRO, (OMGAR - PI / 2.0D0),
     *   (PI / 2.0D0 - AIM), ALONG, BETA, ALGM, BETAM)
C                                      HOPM is h/p from SMART = the
C                                      component of the lunar velocity
C                                      perpendicular to the radius
C                                      vector (km/day)
      HOPM = ANM * AAM / SQRT(1.D0 - EM**2)
C                                      converted to km/sec
      HOPM = HOPM / 86400.D0
C                                      VMON is the projection onto the
C                                      line of sight to the star of the
C                                      velocity of the earth's center
C                                      with respect to the earth-moon
C                                      barycenter (km/s) (the 81.30 is
C                                      the ratio of the earth's mass to
C                                      the moon's mass)
      VMON = -HOPM / 81.30D0 * COS(BETAM) * (SIN(ALAMM - ALGM)
     *   - EM * SIN(PIM - ALGM))
C
      VT = V + VSUN + VMON + VOBS
      VT = VT * 1000.D0
      VSUN = VSUN * 1000.D0
      RETURN
      END
      SUBROUTINE CDTRN (AO, BO, AP, BP, A1, B1, A2, B2)
C-----------------------------------------------------------------------
C  Routine to convert the longitude-like (A1) and latitude-like (B1)
C  coordinates of a point on a sphere into the corresponding
C  coordinates (A2,B2) in a different coordinate system that is
C  specified by the coordinates of its origin (AO, BO) and its north
C  pole (AP, BP) in the original coordinate system.
C  The range of A2 will be from -pi to pi.
C   Inputs:
C    AO            D
C    BO            D
C    AP            D
C    BP            D
C    A1            D
C    B1            D
C   Outputs:
C    A2            D
C    B2            D
C-----------------------------------------------------------------------
      DOUBLE PRECISION AO, BO, AP, BP, A1, B1, A2, B2,
     *   SBO, CBO, SBP, CBP, SB1, CB1, SB2, CB2,
     *   SAA, CAA, SBB, CBB, SA2, CA2, TA2O2
C-----------------------------------------------------------------------
      SBO = SIN(BO)
      CBO = COS(BO)
      SBP = SIN(BP)
      CBP = COS(BP)
      SB1 = SIN(B1)
      CB1 = COS(B1)
C
      SB2 = SBP * SB1 + CBP * CB1 * COS(AP - A1)
      B2 = ASIN(SB2)
      CB2 = COS(B2)
C
      SAA = SIN(AP - A1) * CB1 / CB2
      CAA = (SB1 - SB2 * SBP) / (CB2 * CBP)
C
      CBB = SBO / CBP
      SBB = SIN(AP - AO) * CBO
C
      SA2 = SAA * CBB - CAA * SBB
      CA2 = CAA * CBB + SAA * SBB
      IF (CA2) 10, 10, 20
   10 TA2O2 = (1.0D0 - CA2) / SA2
      GO TO 30
   20 TA2O2 = SA2 / (1.0D0 + CA2)
   30 A2 = 2.D0 * ATAN(TA2O2)
C
      RETURN
      END
      SUBROUTINE RDMOVE ( NYRI, NYRF, MO, NDA, RA, D,
     *   DELR, DELD, DC )
C-----------------------------------------------------------------------
C  Routine RDMOVE calculates the corrections in RA and Dec
C  to be added to mean coordinates for epoch NYRI.
C  If the day-number is known, use it for nda and set mo=1
C  Move also calculates the equation of the equinoxes (DC)
C  which may be added to the mean sidereal time to give the
C  apparent sidereal time.
C  DELR and DELD contain corrections for precession,
C  aberration, and some terms of nutation.
C  Inputs:
C    NYRI        I         Epoch of coords
C    NYRF        I         Year of observation
C    MO          I         Month of observation
C    NDA         I         Day of observation
C    RA          D         RA of epoch (rads)
C    D           D         Dec of epoch (rads)
C  Outputs:
C    DELR        D         RA correction (rads)
C    DELD        D         Dec correction (rads)
C    DC          D         Equation of the equinoxes (mins of time)
C-----------------------------------------------------------------------
      INTEGER   NYRI, NYRF, MO, NDA
      DOUBLE PRECISION    RA, D, DELR, DELD, DC, SND, CSD, TND,CSR, SNR,
     *          AL, TO, T, ZETAO, Z, THETA, AM, AN, ALAM, SNL,
     *          CSL, OMEGA, ARG, DLONG, DOBLQ
C-----------------------------------------------------------------------
      SND = SIN(D)
      CSD = COS(D)
      TND = SND / CSD
C
      CSR = COS(RA)
      SNR = SIN(RA)
C                                       AL is an approximate day number
C                                       (i.e. the number of days since
C                                       January of the year NYRF).
      AL = 30 * (MO - 1) + NDA
C                                       TO is the time from 1900 to
C                                       AYRI (centuries), t is the time
C                                       from AYRI to date (NYRF,MO,NDA)
C                                       (centuries) (365.2421988 is the
C                                       number of ephemeris days in a
C                                       tropical year)
      TO = (NYRI - 1900) / 100.D0
      T = ((NYRF - NYRI) + AL / 365.2421988D0) / 100.D0
C                                       ZETAO is a precessional angle
      ZETAO = (2304.250D0 + 1.396D0 * TO) * T + 0.302D0
     *   * T**2 + 0.018D0 * T**3
C                                       ditto for z
      Z = ZETAO + 0.791D0 * T**2
C                                       and theta
      THETA = (2004.682D0 - 0.853D0 * TO) * T - 0.426D0
     *   * T**2 - 0.042D0 * T**3
C                                       AM and AN are the M and N
C                                       precessional numbers
      AM = (ZETAO + Z) * 4.848136811D-6
      AN = THETA * 4.848136811D-6
C                                       ALAM is an approximate mean
C                                       longitude for sun (radians)
      ALAM = (0.985647D0 * AL + 278.5D0) * 0.0174532925D0
      SNL = SIN(ALAM)
      CSL = COS(ALAM)
C                                       DELR is the annual aberration
C                                       term in RA (radians)
      DELR = -9.92413605D-5 * (SNL * SNR + 0.91745051D0
     *   * CSL * CSR) / CSD
C                                       plus precession terms
     *   + AM + AN * SNR * TND
C                                       DELD is ditto above in dec
      DELD = -9.92413605D-5 * (SNL * CSR * SND - 0.91745051D0
     *   * CSL * SNR * SND + 0.39784993D0 * CSL * CSD) + AN * CSR
C                                       calculate the nutation (approx)
C                                       omega is the angle of the
C                                       first term of nutation
      OMEGA = 259.183275D0 - 1934.142D0 * ( TO + T )
C                                       arg is omega converted to rads
      ARG = OMEGA * 0.0174532925D0
C                                       dlong is the nutation in
C                                       longitude (delta-psi) (radians)
      DLONG = -8.3597D-5 * SIN(ARG)
C                                       doblq is the nutation in
C                                       obliquity (delta-epsilon) (rads)
      DOBLQ = 4.4678D-5 * COS(ARG)
C                                       add nutation in ra into delr
      DELR = DELR + DLONG * ( 0.91745051D0 + 0.39784993D0
     *   * SNR * TND ) - CSR * TND * DOBLQ
C                                        and dec.
      DELD = DELD + 0.39784993D0 * CSR * DLONG + SNR * DOBLQ
C                                        dc is the equation of the
C                                        equinoxes (minutes of time)
      DC = DLONG * 210.264169D0
      RETURN
      END
      INTEGER FUNCTION JULDA (NYR)
C-----------------------------------------------------------------------
C   This function computes the Julian day number at 12 hrs ut on
C   January 0 of the year NYR (Gregorian calendar). Julda is an
C   integer because of this definition.  For example,
C   JULDA = 2439856 for NYR = 1968.
C
C   Inputs:
C    NYR          I         Year
C   Outputs:
C    JULDA        I         Julian day (as above)
C-----------------------------------------------------------------------
      INTEGER    NYR, NYRM1, IC
C-----------------------------------------------------------------------
      NYRM1 = NYR-1
      IC = NYRM1/100
      JULDA = 1721425 + 365 * NYRM1 + NYRM1 / 4 - IC + IC / 4
      RETURN
      END
