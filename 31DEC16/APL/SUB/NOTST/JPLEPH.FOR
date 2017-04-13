      SUBROUTINE JPLEPH( FILE, NAME, TIME, LONG, LAT, HEIGHT, EPOCH,
     *   GEOTOP, RA, DEC, DRA, DDEC, DIST, DDIST, DORECT, IER )
C-----------------------------------------------------------------------
C! Computes position of the planet at the equatorial coordinate system
C# Math Coordinates UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2004-2005, 2015
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
C   Subroutine to compute position of the planet (NAME) at the equatorial
C   coordinate system originated at the center of the Earth.
C   Ephemerids should be given at the file: FILE
C   B J Butler, L. Kogan   NRAO/Socorro
C-----------------------------------------------------------------------
C
C     Coordinates in J2000 will be wanted for the VLBA.  For the VLA,
C     they will be wanted for date.
C
C     The first calls to this routine will be just to determine if
C     a specified source is in the ephemeris and to get nominal
C     coordinates if it is.  IER is used to tell the result.
C
C     Input variables:
C                                       ! Ephemeris file name (<80 chr).
         CHARACTER         FILE*(*)
C                                       ! Object name (up to 12 char).
         CHARACTER         NAME*(*)
C                                       ! Modified Julian day (UTC).
         DOUBLE PRECISION  TIME
C                                       ! East longitude (radians).
         DOUBLE PRECISION  LONG
C                                       ! Geodetic latitude (radians).
         DOUBLE PRECISION  LAT
C                                       ! Geodetic height (meters).
         DOUBLE PRECISION  HEIGHT
C                                       ! Coordinate epoch
C                                       !  ('J2000' or 'DATE').
         CHARACTER         EPOCH*(*)
C                                       ! Geocentric or Topocentric?
C                                       !  ('GEO' or 'TOPO')
         CHARACTER         GEOTOP*(*)
C                                       ! if T, return rectangular
C                                       ! coordinates instead of ra,dec
         LOGICAL           DORECT
C     Returned data:
C                                       ! RA (J2000) (radians) or
C                                       !  X coordinate (AU)
         DOUBLE PRECISION  RA
C                                       ! Dec (J2000) (radians) or
C                                       !  Y coordinate (AU)
         DOUBLE PRECISION  DEC
C                                       ! RA rate (seconds of time per
C                                       !  UT day) or X coordinate rate
C                                       !  AU/day)
         DOUBLE PRECISION  DRA
C                                       ! Dec rate of date (seconds of
C                                       !  arc per UT day) or Y
C                                       !  coordinate rate AU/day)
         DOUBLE PRECISION  DDEC
C                                       ! distance to planet (AU) or
C                                       !  Z coordinate (AU)
         DOUBLE PRECISION  DIST
C                                       ! distance rate (AU/day) or
C                                       !  Z coordinate rate (AU/day)
         DOUBLE PRECISION  DDIST

         INTEGER           IER
C                                       ! Error condition (0 => ok).
C                                       !    1 => NAME not supported
C                                       !    2 => TIME not supported
C ---------------------------------------------------------------------
      DOUBLE PRECISION TDTTIM, JTDT, SLADAT, LEAPS, VALS(400), SS(3)
      INTEGER NPLAN, NP, II, NVS
      PARAMETER (NPLAN = 10)
      LOGICAL TOPO, FIRST
      CHARACTER PLANET(NPLAN)*12, NAMS(400)*6, EPHFI2*80
      INCLUDE 'INCS:DMSG.INC'
C
      COMMON / FNAME / EPHFI2

      DATA PLANET / 'MERCURY     ', 'VENUS       ', 'MOON        ',
     *              'MARS        ', 'JUPITER     ', 'SATURN      ',
     *              'URANUS      ', 'NEPTUNE     ', 'PLUTO       ',
     *              'SUN         ' /
      DATA FIRST / .TRUE. /

      IER = 1
      DO 10 II =1, NPLAN
         IF (NAME .EQ. PLANET(II)) THEN
            IER = 0
            NP = II
            END IF
   10    CONTINUE
C
      IF (IER .EQ. 0) THEN
         TOPO = .NOT. (GEOTOP .EQ. 'GEO')
C                                       find the TDT (given UTC)
         LEAPS = SLADAT (TIME)
         TDTTIM = TIME + (LEAPS + 32.184D0) / 86400.0D0
         JTDT = TDTTIM + 2400000.5D0
         IF (FIRST) THEN
            FIRST = .FALSE.
            EPHFI2 = FILE
            CALL CONST (NAMS, VALS, SS, NVS)
            END IF
         IF (JTDT .LT. SS(1) .OR. JTDT .GT. SS(2)) THEN
C                                       ephemeris doesn't support that
C                                       date/time
            IER = 2
            WRITE (MSGTXT,1200)
            CALL MSGWRT (8)
         ELSE
            CALL JPLPOS (TDTTIM, NP, LONG, LAT, HEIGHT, RA, DRA, DEC,
     *                   DDEC, DIST, DDIST, EPOCH, TOPO, DORECT)
            END IF
      ELSE
C                                       Tell User
         WRITE (MSGTXT,1000) NAME
         CALL MSGWRT (8)
         END IF
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('JPLEPH: The object name ', A12,
     *   ' is not at the given planet list')
 1200 FORMAT ('JPLEPH: The time is not supported ',
     *   ' by the given ephemerid table')
C
      END
C
      SUBROUTINE JPLPOS (DATEP, NP, ELONG, PHI, HEIGHT, RA, RARAT,
     *                   DEC, DECRAT, DIST, DISRAT, EPOCH, TOPO, DORECT)
C
C  Exact geocentric or topocentric apparent RA, RA rate, Dec, Dec rate,
C  and distance of a planet, in either J2000.0 or position of date.
C  Based on section 3.3 of the Explanatory Supplement to the
C  Astronomical Almanac.  Specifically, this subroutine performs the
C  steps in equation 3.31-1 for positions of date, and 3.41-1 for
C  positions of J2000.  For topocentric positions, the correction
C  described in section 3.35 is performed.  The 4 different
C  combinations of J2000/date/geocentric/topocentric correspond to
C  the following designations and sections in the Explanatory Suppl:
C    date+geocentric = 'Apparent-Place'; section 3.31
C    date+topocentric = 'Topocentric-Place'; section 3.35
C    J2000+geocentric = 'Virtual Place'; section 3.41
C    J2000+topocentric = 'Local Place'; section 3.42
C  This routine calls the very accurate JPL ephemeris routines.  The
C  most current ephemeris at JPL is DE403 (as of 6/25/96), which is the
C  one used by this routine.  Errors are very small, on the order of a
C  few milliarcseconds (without rigorous testing, though...).
C
C  Inputs:
C   DATEP   = MJD of observation (JD - 2400000.5), in the TDT timescale
C   NP      = planet for which positions and rates are desired:
C                1 = Mercury
C                2 = Venus
C                3 = Moon
C                4 = Mars
C                5 = Jupiter
C                6 = Saturn
C                7 = Uranus
C                8 = Neptune
C                9 = Pluto
C                else = Sun
C   ELONG   = observer's East longitude (rad)
C   PHI     = observer's geodetic latitude (rad)
C   HEIGHT  = observer's height above geoid (meters)
C   EPOCH   = coordinate epoch (possibilities are 'J2000' or 'DATE')
C   TOPO    = indication of whether to do geocentric or topocentric
C             positions, rates, and distance:
C                TOPO = .FALSE. ==> geocentric
C                TOPO = .TRUE.  ==> topocentric
C   DORECT  = indication of whether to return RA + DEC + distance
C             and their rates, or true rectangular coordinates + rates
C                DORECT = .FALSE. ==> RA/DEC/distance
C                DORECT = .TRUE.  ==> rectangular coordinates
C
C  Outputs:
C   RA      = RA or X coordinate (see DORECT), for equinox and equator
C             of J2000.0 or of date (radians or AU)
C   DEC     = Dec or Y coordiante (see DORECT), for equinox and equator
C             of J2000.0 or of date (radians or AU)
C   RARAT   = RA or X coordinate rate (see DORECT), for the desired
C             date (seconds of time per UT day or AU per UT day)
C   DECRAT  = Dec or Y coordinate rate (see DORECT), for the desired
C             date (seconds of arc per UT day or AU per UT day)
C   DIST    = distance or Z coordinate (see DORECT), for the desired
C             date (AU)
C   DISRAT  = distance or Z coordinate rate (see DORECT), for the
C             desired date (AU/day)
C
C  Calls: SLAEPJ, SLAGMST, SLAEQEQX, SLAPRE, SLANUT, SLAMXV,
C         SLAC2S, SLADRM, PVOBS, PLEPH, FINDT, TRANSP
C
C    All SLAXXX routines are from the slalib library, PLEPH and related
C    routines are from the JPL software, PVOBS, FINDT and TRANSP are
C    my own routines.
C
C  B J Butler    NRAO/VLA   summer 1996, modified summer 1997

C IMPLICIT NONE

C passed parameters:
      DOUBLE PRECISION DATEP, ELONG, PHI, HEIGHT, RA, DEC, DIST,
     *                 RARAT, DECRAT, DISRAT
      INTEGER NP
      LOGICAL TOPO, DORECT
      CHARACTER EPOCH*(*)
C constants:
      DOUBLE PRECISION PI, CC, MU
      PARAMETER (PI = 3.14159265358979323846D0)
      PARAMETER (CC = 173.144633D0)
      PARAMETER (MU = 2.9591220788654D-4)
C time stuff:
      DOUBLE PRECISION JEDP, DATE, JED, TPRIME, MM, SS, TT, UT1, GMST,
     *                 GAST, DELTAT, FINDT
C position and velocity vectors:
      DOUBLE PRECISION EB(6), SB(6), EH(6), UB(6), UU(6), QQ(6),
     *                 LITTLU(6), LITTLQ(6), LITTLE(6), VEC1(3),
     *                 VEC2(3), PP(3), VV(3), U1(6), U2(6), U3(6),
     *                 U4(6), LITTLG(6), BIGG(6), BIGG2(6)
C other stuff:
      DOUBLE PRECISION LITTIM, LITTIP, DU, DE, DQ, G1, G2, QDOTE, UDOTQ,
     *                 EDOTU, BETA, F1, F2, ARGV,
     *                 RMATP(3,3), RMATN(3,3)
      LOGICAL ISSUN
C slalib functions:
      DOUBLE PRECISION SLAGMS, SLAEQX, SLAEPJ, SLADRM
      INTEGER CONVRT(0:9), OTHBOD, IP, II

      DATA CONVRT / 11, 1, 2, 10, 4, 5, 6, 7, 8, 9 /

C 3.311 Relevant Time Arguments
C step a
      JEDP = DATEP + 2400000.5D0
C step b
      TPRIME = (JEDP - 2451545.0D0) / 36525.0D0
C step c
      MM = (357.528D0 + 35999.050D0 * TPRIME) * 2.0D0 * PI / 360.0D0
C step d
      SS = 0.001658D0 * DSIN (MM + 0.1671D0 * DSIN (MM))
      JED = JEDP + SS / 86400.0D0
      DATE = JED - 2400000.5D0
      TT = (JED - 2451545.0D0) / 36525.0D0
      IF (TOPO) THEN
C step a of topocentric part
C   first, find deltaT, the difference between TDT and UT1
         DELTAT = FINDT (SLAEPJ (DATEP))
         UT1 = DATEP - DELTAT / 86400.0D0
C step d of topocentric part
         GMST = SLAGMS (UT1)
         GAST = GMST + SLAEQX (DATEP)
C        LST = GAST + ELONG
C steps b, e & f of topocentric part
         CALL PVOBS (PHI, ELONG, HEIGHT, GAST, LITTLG)
C step g of topocentric part
C   Nutation:
         CALL SLANUT (DATE, RMATN)
C   transpose...
         CALL TRANSP (RMATN)
C   apply:
         CALL SLAMXV (RMATN, LITTLG, BIGG)
         CALL SLAMXV (RMATN, LITTLG(4), BIGG(4))
C   Precession:
         CALL SLAPRE (2000.0D0, SLAEPJ (DATE), RMATP)
C   transpose...
         CALL TRANSP (RMATP)
C   apply:
         CALL SLAMXV (RMATP, BIGG, BIGG2)
         CALL SLAMXV (RMATP, BIGG(4), BIGG2(4))
C   transpose back, if needed for later 'of date' conversion
C   (steps q & r)...
         IF (EPOCH .NE. 'J2000') THEN
            CALL TRANSP (RMATN)
            CALL TRANSP (RMATP)
         END IF
C step h of topocentric part
C   barycentric position and velocity of the earth
         CALL PLEPH (JED, 3, 12, EB)
         DO 10 II = 1, 6
            EB(II) = EB(II) + BIGG2(II)
   10       CONTINUE
      ELSE
C step e
C 3.312 Ephemeris Data for the Earth and Sun
C step f
C   barycentric position and velocity of the earth
         CALL PLEPH (JED, 3, 12, EB)
      END IF
C   barycentric position and velocity of the sun
      CALL PLEPH (JED, 11, 12, SB)
C   form the heliocentric position of the earth
      DO 20 II = 1, 3
         EH(II) = EB(II) - SB(II)
   20    CONTINUE
C step g
C   first, classify the other body, and convert it to JPL's notation...
      IP = NP
      IF (IP .LT. 0 .OR. IP .GT. 9) IP = 0
      OTHBOD = CONVRT(IP)
      ISSUN = OTHBOD .EQ. 11
C   barycentric position and velocity of the desired body
C     CALL PLEPH (JED, OTHBOD, 12, UB)
C 3.314 Geometric Distance Between Earth and Planet
C step h
C   first, distance
      DIST = DSQRT ((UB(1) - EB(1)) ** 2.0D0 +
     *              (UB(2) - EB(2)) ** 2.0D0 +
     *              (UB(3) - EB(3)) ** 2.0D0)
C   then, light-time
      LITTIP = DIST / CC
C 3.315 Geocentric Position of Planet, Accounting for Light-Time
C precalculate this one:
      DE = DSQRT (EH(1) ** 2.0D0 + EH(2) ** 2.0D0 + EH(3) ** 2.0D0)
      LITTIM = 0.0D0

   40 CONTINUE
      IF (DABS (LITTIM - LITTIP) .GE. 1.0D-8) THEN
         LITTIM = LITTIP
C step i
C   barycentric position and velocity of the desired body
         CALL PLEPH (JED-LITTIP, OTHBOD, 12, UB)
C   barycentric position and velocity of the sun
         IF (.NOT. ISSUN) CALL PLEPH (JED-LITTIP, 11, 12, SB)
C step j
         DO 30 II = 1, 3
C   geocentric position of the planet
            UU(II) = UB(II) - EB(II)
C   heliocentric position of the planet
            IF (.NOT. ISSUN) QQ(II) = UB(II) - SB(II)
   30       CONTINUE
C step k
C   first, magnitudes of vectors
         DU = DSQRT (UU(1) ** 2.0D0 + UU(2) ** 2.0D0 + UU(3) ** 2.0D0)
         IF (.NOT. ISSUN) THEN
            DQ = DSQRT (QQ(1)**2.0D0 + QQ(2)**2.0D0 + QQ(3)**2.0D0)
C   the second term is due to the relativistic delay caused by the
C   Sun's gravitational field, so we only calculate it (and the
C   necessary other quantities) if we're not looking at the Sun itself.
            LITTIP = (DU + (2.0D0 * MU / CC ** 2.0D0) *
     *                DLOG ((DE + DU + DQ) / (DE - DU + DQ))) / CC
         ELSE
            LITTIP = DU / CC
            END IF
C
         GO TO 40
         END IF


C step l
C                                       barycentric position and
C                                       velocity of the desired body
      CALL PLEPH (JED-LITTIP, OTHBOD, 12, UB)
C                                       barycentric position and
C                                       velocity of the sun
      CALL PLEPH (JED-LITTIP, 11, 12, SB)
      DO 50 II = 1, 6
C                                       geocentric position of the
C                                       planet
         UU(II) = UB(II) - EB(II)
C                                       heliocentric position of the
C                                       planet
         QQ(II) = UB(II) - SB(II)
   50    CONTINUE
C
      IF (ISSUN) THEN
         DO 60 II = 1, 3
            U1(II) = UU(II)
   60       CONTINUE
      ELSE
C                                       3.316 Relativistic Deflection of
C                                       Light
C                                       step m
         DU = DSQRT (UU(1) ** 2.0D0 + UU(2) ** 2.0D0 + UU(3) ** 2.0D0)
         DQ = DSQRT (QQ(1) ** 2.0D0 + QQ(2) ** 2.0D0 + QQ(3) ** 2.0D0)
         DO 70 II = 1, 3
            LITTLU(II) = UU(II) / DU
            LITTLQ(II) = QQ(II) / DQ
            LITTLE(II) = EH(II) / DE
   70       CONTINUE
C
         G1 = 2 * MU / (CC * CC * DE)
         QDOTE = LITTLQ(1) * LITTLE(1) + LITTLQ(2) * LITTLE(2) +
     *           LITTLQ(3) * LITTLE(3)
         G2 = 1.0D0 + QDOTE
C                                       step n
         UDOTQ = LITTLU(1) * LITTLQ(1) + LITTLU(2) * LITTLQ(2) +
     *           LITTLU(3) * LITTLQ(3)
         EDOTU = LITTLE(1) * LITTLU(1) + LITTLE(2) * LITTLU(2) +
     *           LITTLE(3) * LITTLU(3)
         DO 80 II = 1, 3
            VEC1(II) = UDOTQ * LITTLE(II)
            VEC2(II) = EDOTU * LITTLQ(II)
   80       CONTINUE
C
         DO 90 II = 1, 3
            U1(II) = DU * (LITTLU(II) + (G1/G2) * (VEC1(II) - VEC2(II)))
   90       CONTINUE
      END IF
C                                       3.317 Aberration of Light
C                                       step o
      DU = DSQRT (U1(1) ** 2.0D0 + U1(2) ** 2.0D0 + U1(3) ** 2.0D0)
      DO 110 II = 1, 3
         PP(II) = U1(II) / DU
C                                       bjb - change to ignore
C                                       annual aberration for J2000
         IF (EPOCH .NE. 'J2000') THEN
            VV(II) = EB(II+3) / CC
         ELSE
            VV(II) = BIGG2(II+3) / CC
            END IF
  110    CONTINUE
C
      ARGV = VV(1) * VV(1) + VV(2) * VV(2) + VV(3) * VV(3)
      BETA = DSQRT (1.0D0 - ARGV)
      F1 = PP(1) * VV(1) + PP(2) * VV(2) + PP(3) * VV(3)
      F2 = 1.0D0 + F1 / (1 + BETA)
C                                       step p
      DO 120 II = 1, 3
         U2(II) = (BETA * U1(II) + F2 * DU * VV(II)) / (1.0D0 + F1)
  120    CONTINUE
C
C                                       XXX don't know for sure if this
C                                       is right, but it seems so...
C                                       XXX
      DO 130 II = 4, 6
         U2(II) = UU(II)
  130    CONTINUE
C                                       3.318 Precession, & 3.319
C                                       Nutation
      IF (EPOCH .EQ. 'DATE') THEN
         IF (.NOT. TOPO) THEN
C                                       step q
            CALL SLAPRE (2000.0D0, SLAEPJ (DATE), RMATP)
C                                       step s
            CALL SLANUT (DATE, RMATN)
         END IF
C                                       step r
         CALL SLAMXV (RMATP, U2, U3)
         CALL SLAMXV (RMATP, U2(4), U3(4))
C                                       step t
         CALL SLAMXV (RMATN, U3, U4)
         CALL SLAMXV (RMATN, U3(4), U4(4))
C                                       step u
         IF (DORECT) THEN
            RA = U4(1)
            DEC = U4(2)
            DIST = U4(3)
            RARAT = U4(4)
            DECRAT = U4(5)
            DISRAT = U4(6)
         ELSE
            CALL SLAC2S (U4, RA, DEC)
            RA = SLADRM (RA)
         END IF
      ELSE
         IF (DORECT) THEN
            RA = U2(1)
            DEC = U2(2)
            DIST = U2(3)
            RARAT = U2(4)
            DECRAT = U2(5)
            DISRAT = U2(6)
         ELSE
            CALL SLAC2S (U2, RA, DEC)
            RA = SLADRM (RA)
            DO 140 II = 1, 6
               U4(II) = U2(II)
  140          CONTINUE
         END IF
      END IF
      IF (.NOT. DORECT) THEN
         DIST = DSQRT (U4(1) ** 2.0D0 + U4(2) ** 2.0D0 + U4(3) ** 2.0D0)
C                                       rates...
         DISRAT = (1.0D0 / (2.0D0 * DIST)) *
     *            (2.0D0 * U4(1) * U4(4) + 2.0D0 * U4(2) * U4(5) +
     *             2.0D0 * U4(3) * U4(6))
         RARAT = (U4(1) * U4(5) - U4(2) * U4(4)) /
     *           (U4(1) ** 2.0D0 + U4(2) ** 2.0D0)
C                                       convert to seconds of time per
C                                       UT day:
         RARAT = 4.32D4 * RARAT / PI
         DECRAT = ((U4(6) * (U4(1) ** 2.0D0 + U4(2) ** 2.0D0)) -
     *             (U4(3) * (U4(1) * U4(4) + U4(2) * U4(5)))) /
     *            ((U4(1) * U4(1) + U4(2) * U4(2) + U4(3) * U4(3)) *
     *             DSQRT (U4(1) * U4(1) + U4(2) * U4(2)))
C                                       convert to seconds of arc per UT
C                                       day:
         DECRAT = 6.48D5 * DECRAT / PI
      END IF
      RETURN
      END
C
      SUBROUTINE TRANSP (MATX)
C
C in-place transpose
C
      DOUBLE PRECISION MATX(3,3), SWAPD

      SWAPD = MATX(2,1)
      MATX(2,1) = MATX(1,2)
      MATX(1,2) = SWAPD
      SWAPD = MATX(3,1)
      MATX(3,1) = MATX(1,3)
      MATX(1,3) = SWAPD
      SWAPD = MATX(3,2)
      MATX(3,2) = MATX(2,3)
      MATX(2,3) = SWAPD
      RETURN
      END
      SUBROUTINE PVOBS (PHI, ELONG, HEIGHT, GAST, LITTLG)
C
C calculate the position of the observer on the surface of the Earth,
C in an Earth-fixed, geocentric, right-handed coordinate system with
C the xy-plane the Earth's equator, the xz-plane the Greenwich
C meridian, and the z-axis pointed toward the north terrestrial pole.
C taken from section 3.353 of the Explanatory Supplement to the
C Astronomical Almanac.
C
C  Inputs:
C   PHI    = observer's geodetic latitude (radians)
C   ELONG  = observer's East longitude (radians)
C   HEIGHT = observer's height above geoid (meters)
C   GAST   = Greenwich Apparent Sidereal Time (radians)
C
C  Outputs:
C   LITTLG = the geocentric position and velocity vector of
C            the observer (AU, AU/day)
C
C  Calls: SLAMXM, SLAMXV
C
C  B.J.Butler    NRAO/VLA   summer 1996

C IMPLICIT NONE

      DOUBLE PRECISION PHI, ELONG, HEIGHT, GAST, LITTLG(6)
      DOUBLE PRECISION FLAT, EQRAD, OMEGA, AU
      PARAMETER (FLAT = 1.0D0 / 298.257D0)
      PARAMETER (EQRAD = 6.37814D6)
      PARAMETER (OMEGA = 7.2921151467D-5)
      PARAMETER (AU = 1.4959787066D11)
      DOUBLE PRECISION CPHI, SPHI, CVAL, SVAL, TVAL, XP, YP
      DOUBLE PRECISION LITTLR(3), R1(3,3), R2(3,3), R3(3,3),
     *                 IMTX1(3,3), IMTX2(3,3)
      INTEGER II

C                                       3.351 Location and Universal
C                                       Time of the Observation
C                                       step b
      CPHI = DCOS (PHI)
      SPHI = DSIN (PHI)
      TVAL = (1 - FLAT) * (1 - FLAT)
      CVAL = 1.0D0 / DSQRT (CPHI * CPHI + TVAL * SPHI * SPHI)
      SVAL = CVAL * TVAL
      LITTLR(1) = (EQRAD * CVAL + HEIGHT) * CPHI * DCOS (ELONG)
      LITTLR(2) = (EQRAD * CVAL + HEIGHT) * CPHI * DSIN (ELONG)
      LITTLR(3) = (EQRAD * SVAL + HEIGHT) * SPHI
C                                       3.353 Geocentric Position and
C                                       Velocity Vectors of the Observer
C                                       step e
C   here, if you have access to the coordinates of the Celestial
C   Ephemeris Pole (CEP), XP and YP, put them in.  i haven't put
C   in a routine to calculate them yet, although it appears that
C   it could be done (see the web site at:
C   file://maia.usno.navy.mil/standards/iersch5.t2)
C   the supplement states that neglecting these terms affects the
C   topocentric place of the moon by several milliarcsec, with a
C   smaller effect, inversely proportional to distance, for other
C   bodies.  neglecting these terms is in effect setting R1 and
C   R2 to unit matrices...
C   set up the XP matrix (R2):
      XP = 0.0D0
      R2(1,1) = DCOS (XP)
      R2(1,2) = 0.0D0
      R2(1,3) = -DSIN (XP)
      R2(2,1) = 0.0D0
      R2(2,2) = 1.0D0
      R2(2,3) = 0.0D0
      R2(3,1) = DSIN (XP)
      R2(3,2) = 0.0D0
      R2(3,3) = DCOS (XP)
C                                       set up the YP matrix (R1):
      YP = 0.0D0
      R1(1,1) = 1.0D0
      R1(1,2) = 0.0D0
      R1(1,3) = 0.0D0
      R1(2,1) = 0.0D0
      R1(2,2) = DCOS (YP)
      R1(2,3) = DSIN (YP)
      R1(3,1) = 0.0D0
      R1(3,2) = -DSIN (YP)
      R1(3,3) = DCOS (YP)
C                                       set up the GAST matrix (R3)
C                                       [remember, rotating by -GAST]:
      R3(1,1) = DCOS (GAST)
      R3(1,2) = -DSIN (GAST)
      R3(1,3) = 0.0D0
      R3(2,1) = DSIN (GAST)
      R3(2,2) = DCOS (GAST)
      R3(2,3) = 0.0D0
      R3(3,1) = 0.0D0
      R3(3,2) = 0.0D0
      R3(3,3) = 1.0D0
C                                       multiply R3 and R1, and store in
C                                       an intermediate matrix:
      CALL SLAMXM (R3, R1, IMTX1)
C                                       multiply that intermediate
C                                       matrix by R2, and store in
C                                       another
C                                       intermediate matrix:
      CALL SLAMXM (IMTX1, R2, IMTX2)
C                                       multiply this by the observers
C                                       position vector:
      CALL SLAMXV (IMTX2, LITTLR, LITTLG)
C                                       now, set up the rotation vector
C                                       (R3 again...)
      R3(1,1) = -DSIN (GAST)
      R3(1,2) = -DCOS (GAST)
      R3(1,3) = 0.0D0
      R3(2,1) = DCOS (GAST)
      R3(2,2) = -DSIN (GAST)
      R3(2,3) = 0.0D0
      R3(3,1) = 0.0D0
      R3(3,2) = 0.0D0
      R3(3,3) = 0.0D0
C                                       multiply R3 and R1, and store in
C                                       an intermediate matrix:
      CALL SLAMXM (R3, R1, IMTX1)
C                                       multiply that intermediate
C                                       matrix by R2, and store in
C                                       another intermediate matrix:
C
      CALL SLAMXM (IMTX1, R2, IMTX2)
C                                       multiply this by the observers
C                                       velocity vector:
      CALL SLAMXV (IMTX2, LITTLR, LITTLG(4))
C                                       multiply by earth rotation rate:
      DO 10 II = 4, 6
         LITTLG(II) = LITTLG(II) * OMEGA
  10     CONTINUE
C step f
      DO 20 II = 1, 3
         LITTLG(II) = LITTLG(II) / AU
   20    CONTINUE
      DO 30 II = 4, 6
         LITTLG(II) = LITTLG(II) * 86400.0D0 / AU
   30    CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION FINDT (YEAR)
C
C finds deltaT (TDT - UT1), given the year.  interpolates into
C values on page K-9 of the Astronomical Almanac, and predictions
C from the NEOS up to 2008.  currently only does a linear
C interpolation, which is probably OK for epochs near 2000.0.
C strictly, should probably use 2nd order interpolation, since the
C variations seem to go as time squared at some points.
C
C NOTE: as new accurate values for deltaT become available, they
C       should be used to replace the predictions from 2006-2008
C       (i last did this 2004aug13).
C

      DOUBLE PRECISION YEAR, TABDT(1820:2008), SLADT
      INTEGER IYEAR

      DATA TABDT /
     * 12.00,11.70,11.40,11.10,10.60,10.20, 9.60, 9.10, 8.60, 8.00,
C                                       1830
     *  7.50, 7.00, 6.60, 6.30, 6.00, 5.80, 5.70, 5.60, 5.60, 5.60,
C                                       1840
     *  5.70, 5.80, 5.90, 6.10, 6.20, 6.30, 6.50, 6.60, 6.80, 6.90,
C                                       1850
     *  7.10, 7.20, 7.30, 7.40, 7.50, 7.60, 7.70, 7.70, 7.80, 7.80,
C                                       1860
     *  7.88, 7.82, 7.54, 6.97, 6.40, 6.02, 5.41, 4.10, 2.92, 1.82,
C                                       1870
     *  1.61, 0.10,-1.02,-1.28,-2.69,-3.24,-3.64,-4.54,-4.71,-5.11,
C                                       1880
     * -5.40,-5.42,-5.20,-5.46,-5.46,-5.79,-5.63,-5.64,-5.80,-5.66,
C                                       1890
     * -5.87,-6.01,-6.19,-6.64,-6.44,-6.47,-6.09,-5.76,-4.66,-3.74,
C                                       1900
     * -2.72,-1.54,-0.02, 1.24, 2.64, 3.86, 5.37, 6.14, 7.75, 9.13,
C                                       1910
     * 10.46,11.53,13.36,14.65,16.01,17.20,18.24,19.06,20.25,20.95,
C                                       1920
     * 21.16,22.25,22.41,23.03,23.49,23.62,23.86,24.49,24.34,24.08,
C                                       1930
     * 24.02,24.00,23.87,23.95,23.86,23.93,23.73,23.92,23.96,24.02,
C                                       1940
     * 24.33,24.83,25.30,25.70,26.24,26.77,27.28,27.78,28.25,28.71,
C                                       1950
     * 29.15,29.57,29.97,30.36,30.72,31.07,31.35,31.68,32.18,32.68,
C                                       1960
     * 33.15,33.59,34.00,34.47,35.03,35.73,36.54,37.43,38.29,39.20,
C                                       1970
     * 40.18,41.17,42.23,43.37,44.49,45.48,46.46,47.52,48.53,49.59,
C                                       1980
     * 50.54,51.38,52.17,52.96,53.79,54.34,54.87,55.32,55.82,56.30,
C                                       1990
     * 56.86,57.57,58.31,59.12,59.98,60.79,61.63,62.30,62.97,63.47,
C                                       2000
     * 63.83,64.09,64.30,64.47,64.57,64.69,64.69,64.69,64.69 /

        IF (YEAR .GT. 1820.0 .AND. YEAR .LE. 2008) THEN
           IYEAR = INT (YEAR)
           FINDT = TABDT(IYEAR) +
     *             (YEAR - IYEAR) * (TABDT(IYEAR+1) - TABDT(IYEAR))
        ELSE
           FINDT = SLADT (YEAR)
        END IF
        RETURN
        END

C++++++++++++++++++++++++
C
      SUBROUTINE FSIZR2(NRECL,KSIZE,NRFILE)
C
C++++++++++++++++++++++++
C  THIS SUBROUTINE OPENS THE FILE, 'NAMFIL', WITH A PHONY RECORD LENGTH, READS
C  THE FIRST RECORD, AND USES THE INFO TO COMPUTE KSIZE, THE NUMBER OF SINGLE
C  PRECISION WORDS IN A RECORD.
C
C  THE SUBROUTINE ALSO SETS THE VALUES OF  NRECL, AND NRFILE.

C     IMPLICIT DOUBLE PRECISION(A-H,O-Z)

      SAVE

      CHARACTER*6 TTL(14,3),CNAM(400)
      CHARACTER*80 NAMFIL
      COMMON/FNAME/NAMFIL

      DOUBLE PRECISION SS(3), AU, EMRAT

      INTEGER IPT(3,13), NRECL, NRFILE, KSIZE, MRECL, NCON, J, I,
     *        NUMDE, KMX, KHI, ND

      NRECL=4
      NRFILE=12

C  *****************************************************************
C  *****************************************************************

C  **  OPEN THE DIRECT-ACCESS FILE AND GET THE POINTERS IN ORDER TO
C  **  DETERMINE THE SIZE OF THE EPHEMERIS RECORD

      MRECL=NRECL*1000

        OPEN(NRFILE,
     *       FILE=NAMFIL,
     *       ACCESS='DIRECT',
     *       FORM='UNFORMATTED',
     *       RECL=MRECL,
     *       STATUS='OLD')

      READ(NRFILE,REC=1)TTL,CNAM,SS,NCON,AU,EMRAT,
     * ((IPT(I,J),I=1,3),J=1,12),NUMDE,(IPT(I,13),I=1,3)

      CLOSE(NRFILE)

C  FIND THE NUMBER OF EPHEMERIS COEFFICIENTS FROM THE POINTERS

      KMX = 0
      KHI = 0
C
      DO 10 I = 1,13
         IF (IPT(1,I) .GT. KMX) THEN
            KMX = IPT(1,I)
            KHI = I
         ENDIF
   10    CONTINUE
C
      ND = 3
      IF (KHI .EQ. 12) ND=2

      KSIZE = 2*(IPT(1,KHI)+ND*IPT(2,KHI)*IPT(3,KHI)-1)

      RETURN

      END
C++++++++++++++++++++++++++
C
      SUBROUTINE PLEPH ( ET, NTARG, NCENT, RRD )
C
C++++++++++++++++++++++++++
C  NOTE : Over the years, different versions of PLEPH have had a fifth argument:
C  sometimes, an error return statement number; sometimes, a logical denoting
C  whether or not the requested date is covered by the ephemeris.  We apologize
C  for this inconsistency; in this present version, we use only the four necessary
C  arguments and do the testing outside of the subroutine.
C
C
C
C     THIS SUBROUTINE READS THE JPL PLANETARY EPHEMERIS
C     AND GIVES THE POSITION AND VELOCITY OF THE POINT 'NTARG'
C     WITH RESPECT TO 'NCENT'.
C
C     CALLING SEQUENCE PARAMETERS:
C
C       ET = D.P. JULIAN EPHEMERIS DATE AT WHICH INTERPOLATION
C            IS WANTED.
C
C       ** NOTE THE ENTRY DPLEPH FOR A DOUBLY-DIMENSIONED TIME **
C          THE REASON FOR THIS OPTION IS DISCUSSED IN THE
C          SUBROUTINE STATE
C
C     NTARG = INTEGER NUMBER OF 'TARGET' POINT.
C
C     NCENT = INTEGER NUMBER OF CENTER POINT.
C
C            THE NUMBERING CONVENTION FOR 'NTARG' AND 'NCENT' IS:
C
C                1 = MERCURY           8 = NEPTUNE
C                2 = VENUS             9 = PLUTO
C                3 = EARTH            10 = MOON
C                4 = MARS             11 = SUN
C                5 = JUPITER          12 = SOLAR-SYSTEM BARYCENTER
C                6 = SATURN           13 = EARTH-MOON BARYCENTER
C                7 = URANUS           14 = NUTATIONS (LONGITUDE AND OBLIQ)
C                            15 = LIBRATIONS, IF ON EPH FILE
C
C             (IF NUTATIONS ARE WANTED, SET NTARG = 14. FOR LIBRATIONS,
C              SET NTARG = 15. SET NCENT=0.)
C
C      RRD = OUTPUT 6-WORD D.P. ARRAY CONTAINING POSITION AND VELOCITY
C            OF POINT 'NTARG' RELATIVE TO 'NCENT'. THE UNITS ARE AU AND
C            AU/DAY. FOR LIBRATIONS THE UNITS ARE RADIANS AND RADIANS
C            PER DAY. IN THE CASE OF NUTATIONS THE FIRST FOUR WORDS OF
C            RRD WILL BE SET TO NUTATIONS AND RATES, HAVING UNITS OF
C            RADIANS AND RADIANS/DAY.
C
C            The option is available to have the units in km and km/sec.
C            For this, set km=.true. in the STCOMX common block.
C

C     IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DOUBLE PRECISION RRD(6),ET2Z(2),ET2(2),PV(6,13)
      DOUBLE PRECISION SS(3),CVAL(400),PVSUN(6)

      LOGICAL BSAVE,KM,BARY

      INTEGER LIST(12),IPT(39),DENUM

      INTEGER NCON, NTARG, NCENT, I, K
      DOUBLE PRECISION AU, EMRAT, ET

      COMMON/EPHHDR/CVAL,SS,AU,EMRAT,DENUM,NCON,IPT

      COMMON/STCOMX/KM,BARY,PVSUN

C     INITIALIZE ET2 FOR 'STATE' AND SET UP COMPONENT COUNT
C
      ET2(1)=ET
      ET2(2)=0.D0
      GO TO 11

C     ENTRY POINT 'DPLEPH' FOR DOUBLY-DIMENSIONED TIME ARGUMENT
C          (SEE THE DISCUSSION IN THE SUBROUTINE STATE)

      ENTRY DPLEPH(ET2Z,NTARG,NCENT,RRD)

      ET2(1)=ET2Z(1)
      ET2(2)=ET2Z(2)

  11  DO 15 I=1,6
         RRD(I)=0.D0
   15    CONTINUE

      IF (NTARG .EQ. NCENT) RETURN

      DO 20 I=1,12
         LIST(I)=0
   20    CONTINUE

C     CHECK FOR NUTATION CALL

      IF(NTARG.NE.14) GO TO 97
        IF(IPT(35).GT.0) THEN
          LIST(11)=2
          CALL STATE(ET2,LIST,PV,RRD)
          RETURN
        ELSE
          WRITE(6,297)
  297     FORMAT(' *****  NO NUTATIONS ON THE EPHEMERIS FILE  *****')
          STOP
        ENDIF

C     CHECK FOR LIBRATIONS

  97  IF(NTARG.NE.15) GO TO 98
        IF(IPT(38).GT.0) THEN
          LIST(12)=2
          CALL STATE(ET2,LIST,PV,RRD)
          DO 30 I=1,6
             RRD(I)=PV(I,11)
   30        CONTINUE
          RETURN
        ELSE
          WRITE(6,298)
  298     FORMAT(' *****  NO LIBRATIONS ON THE EPHEMERIS FILE  *****')
          STOP
        ENDIF

C       FORCE BARYCENTRIC OUTPUT BY 'STATE'

  98  BSAVE=BARY
      BARY=.TRUE.

C                                       SET UP PROPER ENTRIES IN 'LIST'
C                                       ARRAY FOR STATE CALL

      DO 40 I=1,2
         K=NTARG
         IF(I .EQ. 2) K=NCENT
         IF(K .LE. 10) LIST(K)=2
         IF(K .EQ. 10) LIST(3)=2
         IF(K .EQ. 3) LIST(10)=2
         IF(K .EQ. 13) LIST(3)=2
   40    CONTINUE


C       MAKE CALL TO STATE

      CALL STATE(ET2,LIST,PV,RRD)

      IF(NTARG .EQ. 11 .OR. NCENT .EQ. 11) THEN
         DO 50 I=1,6
            PV(I,11)=PVSUN(I)
   50       CONTINUE
         ENDIF

      IF(NTARG .EQ. 12 .OR. NCENT .EQ. 12) THEN
         DO 60 I=1,6
            PV(I,12)=0.D0
   60       CONTINUE
         ENDIF

      IF(NTARG .EQ. 13 .OR. NCENT .EQ. 13) THEN
         DO 70 I=1,6
            PV(I,13)=PV(I,3)
   70       CONTINUE
         ENDIF

      IF(NTARG*NCENT .EQ. 30 .AND. NTARG+NCENT .EQ. 13) THEN
         DO 80 I=1,6
            PV(I,3)=0.D0
   80       CONTINUE
         GO TO 99
         ENDIF

      IF(LIST(3) .EQ. 2) THEN
         DO 90 I=1,6
            PV(I,3)=PV(I,3)-PV(I,10)/(1.D0+EMRAT)
   90       CONTINUE
         ENDIF

      IF(LIST(10) .EQ. 2) THEN
         DO 110 I=1,6
            PV(I,10)=PV(I,3)+PV(I,10)
  110       CONTINUE
         ENDIF

  99  DO 120 I=1,6
         RRD(I)=PV(I,NTARG)-PV(I,NCENT)
  120    CONTINUE

      BARY=BSAVE

      RETURN
      END
C+++++++++++++++++++++++++++++++++
C
      SUBROUTINE JPLINT(BUF,T,NCF,NCM,NA,IFL,PV)
C
C+++++++++++++++++++++++++++++++++
C
C     THIS SUBROUTINE DIFFERENTIATES AND INTERPOLATES A
C     SET OF CHEBYSHEV COEFFICIENTS TO GIVE POSITION AND VELOCITY
C
C     CALLING SEQUENCE PARAMETERS:
C
C       INPUT:
C
C         BUF   1ST LOCATION OF ARRAY OF D.P. CHEBYSHEV COEFFICIENTS OF POSITION
C
C           T   T(1) IS DP FRACTIONAL TIME IN INTERVAL COVERED BY
C               COEFFICIENTS AT WHICH INTERPOLATION IS WANTED
C               (0 .LE. T(1) .LE. 1).  T(2) IS DP LENGTH OF WHOLE
C               INTERVAL IN INPUT TIME UNITS.
C
C         NCF   # OF COEFFICIENTS PER COMPONENT
C
C         NCM   # OF COMPONENTS PER SET OF COEFFICIENTS
C
C          NA   # OF SETS OF COEFFICIENTS IN FULL ARRAY
C               (I.E., # OF SUB-INTERVALS IN FULL INTERVAL)
C
C          IFL  INTEGER FLAG: =1 FOR POSITIONS ONLY
C                             =2 FOR POS AND VEL
C
C
C       OUTPUT:
C
C         PV   INTERPOLATED QUANTITIES REQUESTED.  DIMENSION
C               EXPECTED IS PV(NCM,IFL), DP.
C
C
C     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      SAVE
C
      INTEGER NCF, NCM, NA, IFL, NP, NV, L, I, J
      DOUBLE PRECISION BUF(NCF,NCM,*),T(2),PV(NCM,*),PC(18),VC(18)
      DOUBLE PRECISION TWOT, DNA, DT1, TC, VFAC, TEMP
C
      DATA NP/2/
      DATA NV/3/
      DATA TWOT/0.D0/
      DATA PC(1),PC(2)/1.D0,0.D0/
      DATA VC(2)/1.D0/
C
C       ENTRY POINT. GET CORRECT SUB-INTERVAL NUMBER FOR THIS SET
C       OF COEFFICIENTS AND THEN GET NORMALIZED CHEBYSHEV TIME
C       WITHIN THAT SUBINTERVAL.
C
      DNA=DBLE(NA)
      DT1=DINT(T(1))
      TEMP=DNA*T(1)
      L=IDINT(TEMP-DT1)+1

C         TC IS THE NORMALIZED CHEBYSHEV TIME (-1 .LE. TC .LE. 1)

      TC=2.D0*(DMOD(TEMP,1.D0)+DT1)-1.D0

C       CHECK TO SEE WHETHER CHEBYSHEV TIME HAS CHANGED,
C       AND COMPUTE NEW POLYNOMIAL VALUES IF IT HAS.
C       (THE ELEMENT PC(2) IS THE VALUE OF T1(TC) AND HENCE
C       CONTAINS THE VALUE OF TC ON THE PREVIOUS CALL.)

      IF(TC.NE.PC(2)) THEN
        NP=2
        NV=3
        PC(2)=TC
        TWOT=TC+TC
      ENDIF
C
C       BE SURE THAT AT LEAST 'NCF' POLYNOMIALS HAVE BEEN EVALUATED
C       AND ARE STORED IN THE ARRAY 'PC'.
C
      IF(NP.LT.NCF) THEN
        DO 1 I=NP+1,NCF
        PC(I)=TWOT*PC(I-1)-PC(I-2)
    1   CONTINUE
        NP=NCF
      ENDIF
C
C       INTERPOLATE TO GET POSITION FOR EACH COMPONENT
C
      DO 2 I=1,NCM
      PV(I,1)=0.D0
      DO 3 J=NCF,1,-1
      PV(I,1)=PV(I,1)+PC(J)*BUF(J,I,L)
    3 CONTINUE
    2 CONTINUE
      IF(IFL.LE.1) RETURN
C
C       IF VELOCITY INTERPOLATION IS WANTED, BE SURE ENOUGH
C       DERIVATIVE POLYNOMIALS HAVE BEEN GENERATED AND STORED.
C
      VFAC=(DNA+DNA)/T(2)
      VC(3)=TWOT+TWOT
      IF(NV.LT.NCF) THEN
        DO 4 I=NV+1,NCF
        VC(I)=TWOT*VC(I-1)+PC(I-1)+PC(I-1)-VC(I-2)
    4   CONTINUE
        NV=NCF
      ENDIF
C
C       INTERPOLATE TO GET VELOCITY FOR EACH COMPONENT
C
      DO 5 I=1,NCM
      PV(I,2)=0.D0
      DO 6 J=NCF,2,-1
      PV(I,2)=PV(I,2)+VC(J)*BUF(J,I,L)
    6 CONTINUE
      PV(I,2)=PV(I,2)*VFAC
    5 CONTINUE
C
      RETURN
C
      END

C+++++++++++++++++++++++++
C
      SUBROUTINE SPLIT(TT,FR)
C
C+++++++++++++++++++++++++
C
C     THIS SUBROUTINE BREAKS A D.P. NUMBER INTO A D.P. INTEGER
C     AND A D.P. FRACTIONAL PART.
C
C     CALLING SEQUENCE PARAMETERS:
C
C       TT = D.P. INPUT NUMBER
C
C       FR = D.P. 2-WORD OUTPUT ARRAY.
C            FR(1) CONTAINS INTEGER PART
C            FR(2) CONTAINS FRACTIONAL PART
C
C            FOR NEGATIVE INPUT NUMBERS, FR(1) CONTAINS THE NEXT
C            MORE NEGATIVE INTEGER; FR(2) CONTAINS A POSITIVE FRACTION.
C
C       CALLING SEQUENCE DECLARATIONS
C
      DOUBLE PRECISION TT, FR(2)

C       MAIN ENTRY -- GET INTEGER AND FRACTIONAL PARTS

      FR(1)=DINT(TT)
      FR(2)=TT-FR(1)

      IF(TT.GE.0.D0 .OR. FR(2).EQ.0.D0) RETURN

C       MAKE ADJUSTMENTS FOR NEGATIVE INPUT NUMBER

      FR(1)=FR(1)-1.D0
      FR(2)=FR(2)+1.D0

      RETURN

      END


C++++++++++++++++++++++++++++++++
C
      SUBROUTINE STATE(ET2,LIST,PV,PNUT)
C
C++++++++++++++++++++++++++++++++
C
C THIS SUBROUTINE READS AND INTERPOLATES THE JPL PLANETARY EPHEMERIS FILE
C
C     CALLING SEQUENCE PARAMETERS:
C
C     INPUT:
C
C         ET2   DP 2-WORD JULIAN EPHEMERIS EPOCH AT WHICH INTERPOLATION
C               IS WANTED.  ANY COMBINATION OF ET2(1)+ET2(2) WHICH FALLS
C               WITHIN THE TIME SPAN ON THE FILE IS A PERMISSIBLE EPOCH.
C
C                A. FOR EASE IN PROGRAMMING, THE USER MAY PUT THE
C                   ENTIRE EPOCH IN ET2(1) AND SET ET2(2)=0.
C
C                B. FOR MAXIMUM INTERPOLATION ACCURACY, SET ET2(1) =
C                   THE MOST RECENT MIDNIGHT AT OR BEFORE INTERPOLATION
C                   EPOCH AND SET ET2(2) = FRACTIONAL PART OF A DAY
C                   ELAPSED BETWEEN ET2(1) AND EPOCH.
C
C                C. AS AN ALTERNATIVE, IT MAY PROVE CONVENIENT TO SET
C                   ET2(1) = SOME FIXED EPOCH, SUCH AS START OF INTEGRATION,
C                   AND ET2(2) = ELAPSED INTERVAL BETWEEN THEN AND EPOCH.
C
C        LIST   12-WORD INTEGER ARRAY SPECIFYING WHAT INTERPOLATION
C               IS WANTED FOR EACH OF THE BODIES ON THE FILE.
C
C                         LIST(I)=0, NO INTERPOLATION FOR BODY I
C                                =1, POSITION ONLY
C                                =2, POSITION AND VELOCITY
C
C               THE DESIGNATION OF THE ASTRONOMICAL BODIES BY I IS:
C
C                         I = 1: MERCURY
C                           = 2: VENUS
C                           = 3: EARTH-MOON BARYCENTER
C                           = 4: MARS
C                           = 5: JUPITER
C                           = 6: SATURN
C                           = 7: URANUS
C                           = 8: NEPTUNE
C                           = 9: PLUTO
C                           =10: GEOCENTRIC MOON
C                           =11: NUTATIONS IN LONGITUDE AND OBLIQUITY
C                           =12: LUNAR LIBRATIONS (IF ON FILE)
C
C
C     OUTPUT:
C
C          PV   DP 6 X 11 ARRAY THAT WILL CONTAIN REQUESTED INTERPOLATED
C               QUANTITIES.  THE BODY SPECIFIED BY LIST(I) WILL HAVE ITS
C               STATE IN THE ARRAY STARTING AT PV(1,I).  (ON ANY GIVEN
C               CALL, ONLY THOSE WORDS IN 'PV' WHICH ARE AFFECTED BY THE
C               FIRST 10 'LIST' ENTRIES (AND BY LIST(12) IF LIBRATIONS ARE
C               ON THE FILE) ARE SET.  THE REST OF THE 'PV' ARRAY
C               IS UNTOUCHED.)  THE ORDER OF COMPONENTS STARTING IN
C               PV(1,I) IS: X,Y,Z,DX,DY,DZ.
C
C               ALL OUTPUT VECTORS ARE REFERENCED TO THE EARTH MEAN
C               EQUATOR AND EQUINOX OF J2000 IF THE DE NUMBER IS 200 OR
C               GREATER; OF B1950 IF THE DE NUMBER IS LESS THAN 200.
C
C               THE MOON STATE IS ALWAYS GEOCENTRIC; THE OTHER NINE STATES
C               ARE EITHER HELIOCENTRIC OR SOLAR-SYSTEM BARYCENTRIC,
C               DEPENDING ON THE SETTING OF COMMON FLAGS (SEE BELOW).
C
C               LUNAR LIBRATIONS, IF ON FILE, ARE PUT INTO PV(K,11) IF
C               LIST(12) IS 1 OR 2.
C
C         NUT   DP 4-WORD ARRAY THAT WILL CONTAIN NUTATIONS AND RATES,
C               DEPENDING ON THE SETTING OF LIST(11).  THE ORDER OF
C               QUANTITIES IN NUT IS:
C
C                        D PSI  (NUTATION IN LONGITUDE)
C                        D EPSILON (NUTATION IN OBLIQUITY)
C                        D PSI DOT
C                        D EPSILON DOT
C
C           *   STATEMENT # FOR ERROR RETURN, IN CASE OF EPOCH OUT OF
C               RANGE OR I/O ERRORS.
C
C
C     COMMON AREA STCOMX:
C
C          KM   LOGICAL FLAG DEFINING PHYSICAL UNITS OF THE OUTPUT
C               STATES. KM = .TRUE., KM AND KM/SEC
C                          = .FALSE., AU AND AU/DAY
C               DEFAULT VALUE = .FALSE.  (KM DETERMINES TIME UNIT
C               FOR NUTATIONS AND LIBRATIONS.  ANGLE UNIT IS ALWAYS RADIANS.)
C
C        BARY   LOGICAL FLAG DEFINING OUTPUT CENTER.
C               ONLY THE 9 PLANETS ARE AFFECTED.
C                        BARY = .TRUE. =\ CENTER IS SOLAR-SYSTEM BARYCENTER
C                             = .FALSE. =\ CENTER IS SUN
C               DEFAULT VALUE = .FALSE.
C
C       PVSUN   DP 6-WORD ARRAY CONTAINING THE BARYCENTRIC POSITION AND
C               VELOCITY OF THE SUN.
C
C
C     IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      SAVE

      DOUBLE PRECISION ET2(2),PV(6,12),PNUT(4),T(2),PJD(4),BUF(1500),
     *                 SS(3),CVAL(400),PVSUN(3,2)

      INTEGER LIST(12),IPT(3,13)

      DOUBLE PRECISION AU, EMRAT, S, AUFAC
      INTEGER NUMDE, NCON, NRECL, KSIZE, NRFILE, IRECSZ, J, I, NRL,
     *        NR, K, NCOEFS, IND

      LOGICAL FIRST

      CHARACTER*6 TTL(14,3),CNAM(400)
      CHARACTER*80 NAMFIL
      COMMON/FNAME/NAMFIL

      LOGICAL KM,BARY

      COMMON/EPHHDR/CVAL,SS,AU,EMRAT,NUMDE,NCON,IPT
      COMMON/CHRHDR/CNAM,TTL
      COMMON/STCOMX/KM,BARY,PVSUN

      DATA FIRST/.TRUE./
C
C       ENTRY POINT - 1ST TIME IN, GET POINTER DATA, ETC., FROM EPH FILE
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL FSIZR2(NRECL,KSIZE,NRFILE)

        IF(NRECL .EQ. 0) WRITE(*,*)'  ***** FSIZER IS NOT WORKING *****'

        IRECSZ=NRECL*KSIZE
        NCOEFS=KSIZE/2

        OPEN(NRFILE,
     *       FILE=NAMFIL,
     *       ACCESS='DIRECT',
     *       FORM='UNFORMATTED',
     *       RECL=IRECSZ,
     *       STATUS='OLD')

        READ(NRFILE,REC=1)TTL,CNAM,SS,NCON,AU,EMRAT,
     .   ((IPT(I,J),I=1,3),J=1,12),NUMDE,(IPT(I,13),I=1,3)

        READ(NRFILE,REC=2)CVAL

        NRL=0

      ENDIF


C       ********** MAIN ENTRY POINT **********


      IF(ET2(1) .EQ. 0.D0) RETURN

      S=ET2(1)-.5D0
      CALL SPLIT(S,PJD(1))
      CALL SPLIT(ET2(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)+.5D0
      PJD(2)=PJD(2)+PJD(4)
      CALL SPLIT(PJD(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)

C       ERROR RETURN FOR EPOCH OUT OF RANGE

      IF(PJD(1)+PJD(4).LT.SS(1) .OR. PJD(1)+PJD(4).GT.SS(2)) GO TO 98

C       CALCULATE RECORD # AND RELATIVE TIME IN INTERVAL

      NR=IDINT((PJD(1)-SS(1))/SS(3))+3
      IF(PJD(1).EQ.SS(2)) NR=NR-1
      T(1)=((PJD(1)-(DBLE(NR-3)*SS(3)+SS(1)))+PJD(4))/SS(3)

C       READ CORRECT RECORD IF NOT IN CORE

      IF(NR.NE.NRL) THEN
        NRL=NR
        READ(NRFILE,REC=NR,ERR=99)(BUF(K),K=1,NCOEFS)
      ENDIF

      IF(KM) THEN
      T(2)=SS(3)*86400.D0
      AUFAC=1.D0
      ELSE
      T(2)=SS(3)
      AUFAC=1.D0/AU
      ENDIF

C   INTERPOLATE SSBARY SUN

      CALL JPLINT(BUF(IPT(1,11)),T,IPT(2,11),3,IPT(3,11),2,PVSUN)

      DO 20 I = 1, 3
         DO 10 J = 1, 2
            PVSUN(I,J) = PVSUN(I,J) * AUFAC
   10       CONTINUE
   20    CONTINUE

C   CHECK AND INTERPOLATE WHICHEVER BODIES ARE REQUESTED

      DO 50 I=1,10
         IF(LIST(I).EQ.0) GO TO 50

         CALL JPLINT(BUF(IPT(1,I)),T,IPT(2,I),3,IPT(3,I),
     *      LIST(I),PV(1,I))

         DO 40 K = 1, 2
            DO 30 J = 1, 3
               IND = J + (K - 1) * 3
               IF (I .LE. 9 .AND. .NOT. BARY) THEN
                  PV(IND,I)=PV(IND,I)*AUFAC-PVSUN(J,K)
               ELSE
                  PV(IND,I)=PV(IND,I)*AUFAC
                  END IF
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE

C       DO NUTATIONS IF REQUESTED (AND IF ON FILE)

      IF(LIST(11).GT.0 .AND. IPT(2,12).GT.0)
     * CALL JPLINT(BUF(IPT(1,12)),T,IPT(2,12),2,IPT(3,12),
     * LIST(11),PNUT)

C       GET LIBRATIONS IF REQUESTED (AND IF ON FILE)

      IF(LIST(12).GT.0 .AND. IPT(2,13).GT.0)
     * CALL JPLINT(BUF(IPT(1,13)),T,IPT(2,13),3,IPT(3,13),
     * LIST(12),PV(1,11))

      RETURN

  98  WRITE(*,198)ET2(1)+ET2(2),SS(1),SS(2)
 198  FORMAT(' ***  Requested JED,',F12.2,
     * ' not within ephemeris limits,',2F12.2,'  ***')

      STOP

   99 WRITE(*,'(2F12.2,A80)')ET2,'ERROR RETURN IN STATE'

      STOP

      END
C+++++++++++++++++++++++++++++
C
      SUBROUTINE CONST(NAM,VAL,SSS,N)
C
C+++++++++++++++++++++++++++++
C
C     THIS ENTRY OBTAINS THE CONSTANTS FROM THE EPHEMERIS FILE
C
C     CALLING SEQEUNCE PARAMETERS (ALL OUTPUT):
C
C       NAM = CHARACTER*6 ARRAY OF CONSTANT NAMES
C
C       VAL = D.P. ARRAY OF VALUES OF CONSTANTS
C
C       SSS = D.P. JD START, JD STOP, STEP OF EPHEMERIS
C
C         N = INTEGER NUMBER OF ENTRIES IN 'NAM' AND 'VAL' ARRAYS
C
C     IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      SAVE

      CHARACTER*6 NAM(*),TTL(14,3),CNAM(400)

      DOUBLE PRECISION VAL(*),SSS(3),SS(3),CVAL(400), AU, EMRAT
      DOUBLE PRECISION DZERO(72)
      INTEGER IZERO(12)

      INTEGER IPT(3,13),DENUM, NCON, N, I

      COMMON/EPHHDR/CVAL,SS,AU,EMRAT,DENUM,NCON,IPT
      COMMON/CHRHDR/CNAM,TTL
      DATA DZERO, IZERO /72*0.0D0, 12*0/

C  CALL STATE TO INITIALIZE THE EPHEMERIS AND READ IN THE CONSTANTS

      CALL STATE(DZERO, IZERO, DZERO, DZERO)

      N=NCON

      DO 10 I=1,3
         SSS(I)=SS(I)
   10    CONTINUE

      DO 20 I=1,N
         NAM(I)=CNAM(I)
         VAL(I)=CVAL(I)
   20    CONTINUE

      RETURN

      END
C
      SUBROUTINE SLAC2S (V, A, B)
*+
*     - - - - - -
*      D C C 2 S
*     - - - - - -
*
*  Direction cosines to spherical coordinates (double precision)
*
*  Given:
*     V     d(3)   x,y,z vector
*
*  Returned:
*     A,B   d      spherical coordinates in radians
*
*  The spherical coordinates are longitude (+ve anticlockwise
*  looking from the +ve latitude pole) and latitude.  The
*  Cartesian coordinates are right handed, with the x axis
*  at zero longitude and latitude, and the z axis at the
*  +ve latitude pole.
*
*  If V is null, zero A and B are returned.
*  At either pole, zero A is returned.
*
*  P.T.Wallace   Starlink   July 1989
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

C IMPLICIT NONE

      DOUBLE PRECISION V(3),A,B

      DOUBLE PRECISION X,Y,Z,R


      X = V(1)
      Y = V(2)
      Z = V(3)
      R = SQRT(X*X+Y*Y)

      IF (R.EQ.0D0) THEN
         A = 0D0
      ELSE
         A = ATAN2(Y,X)
      END IF

      IF (Z.EQ.0D0) THEN
         B = 0D0
      ELSE
         B = ATAN2(Z,R)
      END IF

      END
      SUBROUTINE SLAEUL (ORDER, PHI, THETA, PSI, RMAT)
*+
*     - - - - - - -
*      D E U L E R
*     - - - - - - -
*
*  Form a rotation matrix from the Euler angles - three successive
*  rotations about specified Cartesian axes (double precision)
*
*  Given:
*    ORDER  c*(*)    specifies about which axes the rotations occur
*    PHI    dp       1st rotation (radians)
*    THETA  dp       2nd rotation (   "   )
*    PSI    dp       3rd rotation (   "   )
*
*  Returned:
*    RMAT   dp(3,3)  rotation matrix
*
*  A rotation is positive when the reference frame rotates
*  anticlockwise as seen looking towards the origin from the
*  positive region of the specified axis.
*
*  The characters of ORDER define which axes the three successive
*  rotations are about.  A typical value is 'ZXZ', indicating that
*  RMAT is to become the direction cosine matrix corresponding to
*  rotations of the reference frame through PHI radians about the
*  old Z-axis, followed by THETA radians about the resulting X-axis,
*  then PSI radians about the resulting Z-axis.
*
*  The axis names can be any of the following, in any order or
*  combination:  X, Y, Z, uppercase or lowercase, 1, 2, 3.  Normal
*  axis labelling/numbering conventions apply;  the xyz (=123)
*  triad is right-handed.  Thus, the 'ZXZ' example given above
*  could be written 'zxz' or '313' (or even 'ZxZ' or '3xZ').  ORDER
*  is terminated by length or by the first unrecognised character.
*
*  Fewer than three rotations are acceptable, in which case the later
*  angle arguments are ignored.  Zero rotations produces a unit RMAT.
*
*  P.T.Wallace   Starlink   November 1988
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

C IMPLICIT NONE

      CHARACTER*(*) ORDER
      DOUBLE PRECISION PHI,THETA,PSI,RMAT(3,3)

      INTEGER J,I,L,N,K
      DOUBLE PRECISION RESULT(3,3),ROTN(3,3),ANGLE,S,C,W,WM(3,3)
      CHARACTER AXIS



*  Initialise result matrix
      DO 20 J=1,3
         DO 10 I=1,3
            IF (I.NE.J) THEN
               RESULT(I,J) = 0D0
            ELSE
               RESULT(I,J) = 1D0
               END IF
   10       CONTINUE
   20    CONTINUE

*  Establish length of axis string
      L = LEN(ORDER)

*  Look at each character of axis string until finished
      DO 110 N=1,3
         IF (N.LE.L) THEN

*        Initialise rotation matrix for the current rotation
            DO 40 J=1,3
               DO 30 I=1,3
                  IF (I.NE.J) THEN
                     ROTN(I,J) = 0D0
                  ELSE
                     ROTN(I,J) = 1D0
                  END IF
   30             CONTINUE
   40          CONTINUE

*        Pick up the appropriate Euler angle and take sine & cosine
            IF (N.EQ.1) THEN
               ANGLE = PHI
            ELSE IF (N.EQ.2) THEN
               ANGLE = THETA
            ELSE
               ANGLE = PSI
            END IF
            S = SIN(ANGLE)
            C = COS(ANGLE)

*        Identify the axis
            AXIS = ORDER(N:N)
            IF (AXIS.EQ.'X'.OR.
     :          AXIS.EQ.'x'.OR.
     :          AXIS.EQ.'1') THEN

*           Matrix for x-rotation
               ROTN(2,2) = C
               ROTN(2,3) = S
               ROTN(3,2) = -S
               ROTN(3,3) = C

            ELSE IF (AXIS.EQ.'Y'.OR.
     :               AXIS.EQ.'y'.OR.
     :               AXIS.EQ.'2') THEN

*           Matrix for y-rotation
               ROTN(1,1) = C
               ROTN(1,3) = -S
               ROTN(3,1) = S
               ROTN(3,3) = C

            ELSE IF (AXIS.EQ.'Z'.OR.
     :               AXIS.EQ.'z'.OR.
     :               AXIS.EQ.'3') THEN

*           Matrix for z-rotation
               ROTN(1,1) = C
               ROTN(1,2) = S
               ROTN(2,1) = -S
               ROTN(2,2) = C

            ELSE

*           Unrecognised character - fake end of string
               L = 0

            END IF

*        Apply the current rotation (matrix ROTN x matrix RESULT)
            DO 70 I=1,3
               DO 60 J=1,3
                  W = 0D0
                  DO 50 K=1,3
                     W = W+ROTN(I,K)*RESULT(K,J)
   50                CONTINUE
                  WM(I,J) = W
   60             CONTINUE
   70          CONTINUE
            DO 90 J=1,3
               DO 80 I=1,3
                  RESULT(I,J) = WM(I,J)
   80             CONTINUE
   90          CONTINUE

         END IF

  110    CONTINUE

*  Copy the result
      DO 130 J=1,3
         DO 120 I=1,3
            RMAT(I,J) = RESULT(I,J)
  120       CONTINUE
  130    CONTINUE

      END
      SUBROUTINE SLAMXM (A, B, C)
*+
*     - - - - -
*      D M X M
*     - - - - -
*
*  Product of two 3x3 matrices:
*
*      matrix C  =  matrix A  x  matrix B
*
*  (double precision)
*
*  Given:
*      A      dp(3,3)        matrix
*      B      dp(3,3)        matrix
*
*  Returned:
*      C      dp(3,3)        matrix result
*
*  To comply with the ANSI Fortran 77 standard, A, B and C must
*  be different arrays.  However, the routine is coded so as to
*  work properly on the VAX and many other systems even if this
*  rule is violated.
*
*  P.T.Wallace   Starlink   5 April 1990
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

C IMPLICIT NONE

      DOUBLE PRECISION A(3,3),B(3,3),C(3,3)

      INTEGER I,J,K
      DOUBLE PRECISION W,WM(3,3)


*  Multiply into scratch matrix
      DO 30 I=1,3
         DO 20 J=1,3
            W=0D0
            DO 10 K=1,3
               W=W+A(I,K)*B(K,J)
   10          CONTINUE
            WM(I,J)=W
   20       CONTINUE
   30    CONTINUE

*  Return the result
      DO 50 J=1,3
         DO 40 I=1,3
            C(I,J)=WM(I,J)
   40       CONTINUE
   50    CONTINUE

      END
      SUBROUTINE SLAMXV (DM, VA, VB)
*+
*     - - - - -
*      D M X V
*     - - - - -
*
*  Performs the 3-D forward unitary transformation:
*
*     vector VB = matrix DM * vector VA
*
*  (double precision)
*
*  Given:
*     DM       dp(3,3)    matrix
*     VA       dp(3)      vector
*
*  Returned:
*     VB       dp(3)      result vector
*
*  P.T.Wallace   Starlink   March 1986
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

C IMPLICIT NONE

      DOUBLE PRECISION DM(3,3),VA(3),VB(3)

      INTEGER I,J
      DOUBLE PRECISION W,VW(3)


*  Matrix DM * vector VA -> vector VW
      DO 20 J=1,3
         W=0D0
         DO 10 I=1,3
            W=W+DM(J,I)*VA(I)
   10       CONTINUE
         VW(J)=W
   20    CONTINUE

*  Vector VW -> vector VB
      DO 30 J=1,3
         VB(J)=VW(J)
   30    CONTINUE

      END
      DOUBLE PRECISION FUNCTION SLADRM (ANGLE)
*+
*     - - - - - - -
*      D R A N R M
*     - - - - - - -
*
*  Normalise angle into range 0-2 pi  (double precision)
*
*  Given:
*     ANGLE     dp      the angle in radians
*
*  The result is ANGLE expressed in the range 0-2 pi (double
*  precision).
*
*  P.T.Wallace   Starlink   10 December 1993
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

C IMPLICIT NONE

      DOUBLE PRECISION ANGLE

      DOUBLE PRECISION D2PI
      PARAMETER (D2PI=6.283185307179586476925286766559D0)


      SLADRM=MOD(ANGLE,D2PI)
      IF (SLADRM.LT.0D0) SLADRM=SLADRM+D2PI

      END
      DOUBLE PRECISION FUNCTION SLADT (EPOCH)
*+
*     - - -
*      D T
*     - - -
*
*  Estimate the offset between dynamical time and Universal Time
*  for a given historical epoch.
*
*  Given:
*     EPOCH       d        (Julian) epoch (e.g. 1850D0)
*
*  The result is a rough estimate of ET-UT (after 1984, TT-UT) at
*  the given epoch, in seconds.
*
*  Notes:
*
*  1  Depending on the epoch, one of three parabolic approximations
*     is used:
*
*      before 979    Stephenson & Morrison's 390 BC to AD 948 model
*      979 to 1708   Stephenson & Morrison's 948 to 1600 model
*      after 1708    McCarthy & Babcock's post-1650 model
*
*     The breakpoints are chosen to ensure continuity:  they occur
*     at places where the adjacent models give the same answer as
*     each other.
*
*  2  The accuracy is modest, with errors of up to 20 sec during
*     the interval since 1650, rising to perhaps 30 min by 1000 BC.
*     Comparatively accurate values from AD 1600 are tabulated in
*     the Astronomical Almanac (see section K8 of the 1995 AA).
*
*  3  The use of double-precision for both argument and result is
*     purely for compatibility with other SLALIB time routines.
*
*  4  The models used are based on a lunar tidal acceleration value
*     of -26.00 arcsec per century.
*
*  Reference:  Explanatory Supplement to the Astronomical Almanac,
*              ed P.K.Seidelmann, University Science Books (1992),
*              section 2.553, p83.  This contains references to
*              the Stephenson & Morrison and McCarthy & Babcock
*              papers.
*
*  P.T.Wallace   Starlink   1 March 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-
C IMPLICIT NONE

      DOUBLE PRECISION EPOCH
      DOUBLE PRECISION T,W,S


*  Centuries since 1800
      T=(EPOCH-1800D0)/100D0

*  Select model
      IF (EPOCH.GE.1708.185161980887D0) THEN

*     Post-1708: use McCarthy & Babcock
         W=T-0.19D0
         S=5.156D0+13.3066D0*W*W
      ELSE IF (EPOCH.GE.979.0258204760233D0) THEN

*     979-1708: use Stephenson & Morrison's 948-1600 model
         S=25.5D0*T*T
      ELSE

*     Pre-979: use Stephenson & Morrison's 390 BC to AD 948 model
         S=1360.0D0+(320D0+44.3D0*T)*T
      END IF

*  Result
      SLADT=S

      END
      DOUBLE PRECISION FUNCTION SLAEPJ (DATE)
*+
*     - - - -
*      E P J
*     - - - -
*
*  Conversion of Modified Julian Date to Julian Epoch (double precision)
*
*  Given:
*     DATE     dp       Modified Julian Date (JD - 2400000.5)
*
*  The result is the Julian Epoch.
*
*  Reference:
*     Lieske,J.H., 1979. Astron.Astrophys.,73,282.
*
*  P.T.Wallace   Starlink   February 1984
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

C IMPLICIT NONE

      DOUBLE PRECISION DATE


      SLAEPJ = 2000D0 + (DATE-51544.5D0)/365.25D0

      END
      SUBROUTINE SLAGEO (P, H, R, Z)
*+
*     - - - - -
*      G E O C
*     - - - - -
*
*  Convert geodetic position to geocentric (double precision)
*
*  Given:
*     P     dp     latitude (geodetic, radians)
*     H     dp     height above reference spheroid (geodetic, metres)
*
*  Returned:
*     R     dp     distance from Earth axis (AU)
*     Z     dp     distance from plane of Earth equator (AU)
*
*  Notes:
*     1)  Geocentric latitude can be obtained by evaluating ATAN2(Z,R).
*     2)  IAU 1976 constants are used.
*
*  Reference:
*     Green,R.M., Spherical Astronomy, CUP 1985, p98.
*
*  P.T.Wallace   Starlink   4th October 1989
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

C     IMPLICIT NONE

      DOUBLE PRECISION P,H,R,Z

*  Earth equatorial radius (metres)
      DOUBLE PRECISION A0
      PARAMETER (A0=6378140D0)

*  Reference spheroid flattening factor and useful function
      DOUBLE PRECISION F,B
      PARAMETER (F=1D0/298.257D0,B=(1D0-F)**2)

*  Astronomical unit in metres
      DOUBLE PRECISION AU
      PARAMETER (AU=1.4959787066D11)

      DOUBLE PRECISION SP,CP,C,S



*  Geodetic to geocentric conversion
      SP=SIN(P)
      CP=COS(P)
      C=1D0/SQRT(CP*CP+B*SP*SP)
      S=B*C
      R=(A0*C+H)*CP/AU
      Z=(A0*S+H)*SP/AU

      END
      DOUBLE PRECISION FUNCTION SLAGMS (UT1)
*+
*     - - - - -
*      G M S T
*     - - - - -
*
*  Conversion from universal time to sidereal time (double precision)
*
*  Given:
*    UT1    dp     universal time (strictly UT1) expressed as
*                  modified Julian Date (JD-2400000.5)
*
*  The result is the Greenwich mean sidereal time (double
*  precision, radians).
*
*  The IAU 1982 expression (see page S15 of 1984 Astronomical
*  Almanac) is used, but rearranged to reduce rounding errors.
*  This expression is always described as giving the GMST at
*  0 hours UT.  In fact, it gives the difference between the
*  GMST and the UT, which happens to equal the GMST (modulo
*  24 hours) at 0 hours UT each day.  In this routine, the
*  entire UT is used directly as the argument for the
*  standard formula, and the fractional part of the UT is
*  added separately;  note that the factor 1.0027379... does
*  not appear.
*
*  See also the routine SL_GMSA, which delivers better numerical
*  precision by accepting the UT date and time as separate arguments.
*
*  Called:  SLADRM
*
*  P.T.Wallace   Starlink   14 September 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

C IMPLICIT NONE

      DOUBLE PRECISION UT1

      DOUBLE PRECISION SLADRM

      DOUBLE PRECISION D2PI,S2R
      PARAMETER (D2PI=6.283185307179586476925286766559D0,
     :           S2R=7.272205216643039903848711535369D-5)

      DOUBLE PRECISION TU



*  Julian centuries from fundamental epoch J2000 to this UT
      TU=(UT1-51544.5D0)/36525D0

*  GMST at this UT
      SLAGMS=SLADRM(MOD(UT1,1D0)*D2PI+
     :                    (24110.54841D0+
     :                    (8640184.812866D0+
     :                    (0.093104D0-6.2D-6*TU)*TU)*TU)*S2R)

      END
      DOUBLE PRECISION FUNCTION SLAEQX (DATE)
*+
*     - - - - - -
*      E Q E Q X
*     - - - - - -
*
*  Equation of the equinoxes  (IAU 1994, double precision)
*
*  Given:
*     DATE    dp      TDB (loosely ET) as Modified Julian Date
*                                          (JD-2400000.5)
*
*  The result is the equation of the equinoxes (double precision)
*  in radians:
*
*     Greenwich apparent ST = GMST + SLAEQX
*
*  References:  IAU Resolution C7, Recommendation 3 (1994)
*               Capitaine, N. & Gontier, A.-M., Astron. Astrophys.,
*               275, 645-650 (1993)
*
*  Called:  SLNUTC
*
*  Patrick Wallace   Starlink   21 November 1994
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

C IMPLICIT NONE

      DOUBLE PRECISION DATE

*  Turns to arc seconds and arc seconds to radians
      DOUBLE PRECISION T2AS,AS2R
      PARAMETER (T2AS=1296000D0,
     :           AS2R=0.4848136811095359949D-05)

      DOUBLE PRECISION T,OM,DPSI,DEPS,EPS0



*  Interval between basic epoch J2000.0 and current epoch (JC)
      T=(DATE-51544.5D0)/36525D0

*  Longitude of the mean ascending node of the lunar orbit on the
*   ecliptic, measured from the mean equinox of date
      OM=AS2R*(450160.280D0+(-5D0*T2AS-482890.539D0
     :         +(7.455D0+0.008D0*T)*T)*T)

*  Nutation
      CALL SLNUTC(DATE,DPSI,DEPS,EPS0)

*  Equation of the equinoxes
      SLAEQX=DPSI*COS(EPS0)+AS2R*(0.00264D0*SIN(OM)+
     :                               0.000063D0*SIN(OM+OM))

      END
      SUBROUTINE SLANUT (DATE, RMATN)
*+
*     - - - -
*      N U T
*     - - - -
*
*  Form the matrix of nutation for a given date - IAU 1980 theory
*  (double precision)
*
*  References:
*     Final report of the IAU Working Group on Nutation,
*      chairman P.K.Seidelmann, 1980.
*     Kaplan,G.H., 1981, USNO circular no. 163, pA3-6.
*
*  Given:
*     DATE   dp         TDB (loosely ET) as Modified Julian Date
*                                           (=JD-2400000.5)
*  Returned:
*     RMATN  dp(3,3)    nutation matrix
*
*  The matrix is in the sense   V(true)  =  RMATN * V(mean)
*
*  Called:   SLNUTC, SLAEUL
*
*  P.T.Wallace   Starlink   1 January 1993
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

C IMPLICIT NONE

      DOUBLE PRECISION DATE,RMATN(3,3)

      DOUBLE PRECISION DPSI,DEPS,EPS0



*  Nutation components and mean obliquity
      CALL SLNUTC(DATE,DPSI,DEPS,EPS0)

*  Rotation matrix
      CALL SLAEUL('XZX',EPS0,-DPSI,-(EPS0+DEPS),RMATN)

      END
      SUBROUTINE SLNUTC (DATE, DPSI, DEPS, EPS0)
*+
*     - - - - -
*      N U T C
*     - - - - -
*
*  Nutation:  longitude & obliquity components and mean
*  obliquity - IAU 1980 theory (double precision)
*
*  Given:
*
*     DATE        dp    TDB (loosely ET) as Modified Julian Date
*                                            (JD-2400000.5)
*  Returned:
*
*     DPSI,DEPS   dp    nutation in longitude,obliquity
*     EPS0        dp    mean obliquity
*
*  References:
*     Final report of the IAU Working Group on Nutation,
*      chairman P.K.Seidelmann, 1980.
*     Kaplan,G.H., 1981, USNO circular no. 163, pA3-6.
*
*  P.T.Wallace   Starlink   September 1987
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

C IMPLICIT NONE

      DOUBLE PRECISION DATE,DPSI,DEPS,EPS0

      DOUBLE PRECISION T2AS,AS2R,U2R
      DOUBLE PRECISION T,EL,EL2,EL3
      DOUBLE PRECISION ELP,ELP2
      DOUBLE PRECISION F,F2,F4
      DOUBLE PRECISION D,D2,D4
      DOUBLE PRECISION OM,OM2
      DOUBLE PRECISION DP,DE
      DOUBLE PRECISION A


*  Turns to arc seconds
      PARAMETER (T2AS=1296000D0)
*  Arc seconds to radians
      PARAMETER (AS2R=0.4848136811095359949D-05)
*  Units of 0.0001 arcsec to radians
      PARAMETER (U2R=AS2R/1D4)




*  Interval between basic epoch J2000.0 and current epoch (JC)
      T=(DATE-51544.5D0)/36525D0

*
*  FUNDAMENTAL ARGUMENTS in the FK5 reference system
*

*  Mean longitude of the moon minus mean longitude of the moon's perigee
      EL=AS2R*(485866.733D0+(1325D0*T2AS+715922.633D0
     :         +(31.310D0+0.064D0*T)*T)*T)

*  Mean longitude of the sun minus mean longitude of the sun's perigee
      ELP=AS2R*(1287099.804D0+(99D0*T2AS+1292581.224D0
     :         +(-0.577D0-0.012D0*T)*T)*T)

*  Mean longitude of the moon minus mean longitude of the moon's node
      F=AS2R*(335778.877D0+(1342D0*T2AS+295263.137D0
     :         +(-13.257D0+0.011D0*T)*T)*T)

*  Mean elongation of the moon from the sun
      D=AS2R*(1072261.307D0+(1236D0*T2AS+1105601.328D0
     :         +(-6.891D0+0.019D0*T)*T)*T)

*  Longitude of the mean ascending node of the lunar orbit on the
*   ecliptic, measured from the mean equinox of date
      OM=AS2R*(450160.280D0+(-5D0*T2AS-482890.539D0
     :         +(7.455D0+0.008D0*T)*T)*T)

*  Multiples of arguments
      EL2=EL+EL
      EL3=EL2+EL
      ELP2=ELP+ELP
      F2=F+F
      F4=F2+F2
      D2=D+D
      D4=D2+D2
      OM2=OM+OM


*
*  SERIES FOR THE NUTATION
*
      DP=0D0
      DE=0D0

*  106
      DP=DP+SIN(ELP+D)
*  105
      DP=DP-SIN(F2+D4+OM2)
*  104
      DP=DP+SIN(EL2+D2)
*  103
      DP=DP-SIN(EL-F2+D2)
*  102
      DP=DP-SIN(EL+ELP-D2+OM)
*  101
      DP=DP-SIN(-ELP+F2+OM)
*  100
      DP=DP-SIN(EL-F2-D2)
*  99
      DP=DP-SIN(ELP+D2)
*  98
      DP=DP-SIN(F2-D+OM2)
*  97
      DP=DP-SIN(-F2+OM)
*  96
      DP=DP+SIN(-EL-ELP+D2+OM)
*  95
      DP=DP+SIN(ELP+F2+OM)
*  94
      DP=DP-SIN(EL+F2-D2)
*  93
      DP=DP+SIN(EL3+F2-D2+OM2)
*  92
      DP=DP+SIN(F4-D2+OM2)
*  91
      DP=DP-SIN(EL+D2+OM)
*  90
      DP=DP-SIN(EL2+F2+D2+OM2)
*  89
      A=EL2+F2-D2+OM
      DP=DP+SIN(A)
      DE=DE-COS(A)
*  88
      DP=DP+SIN(EL-ELP-D2)
*  87
      DP=DP+SIN(-EL+F4+OM2)
*  86
      A=-EL2+F2+D4+OM2
      DP=DP-SIN(A)
      DE=DE+COS(A)
*  85
      A=EL+F2+D2+OM
      DP=DP-SIN(A)
      DE=DE+COS(A)
*  84
      A=EL+ELP+F2-D2+OM2
      DP=DP+SIN(A)
      DE=DE-COS(A)
*  83
      DP=DP-SIN(EL2-D4)
*  82
      A=-EL+F2+D4+OM2
      DP=DP-2D0*SIN(A)
      DE=DE+COS(A)
*  81
      A=-EL2+F2+D2+OM2
      DP=DP+SIN(A)
      DE=DE-COS(A)
*  80
      DP=DP-SIN(EL-D4)
*  79
      A=-EL+OM2
      DP=DP+SIN(A)
      DE=DE-COS(A)
*  78
      A=F2+D+OM2
      DP=DP+2D0*SIN(A)
      DE=DE-COS(A)
*  77
      DP=DP+2D0*SIN(EL3)
*  76
      A=EL+OM2
      DP=DP-2D0*SIN(A)
      DE=DE+COS(A)
*  75
      A=EL2+OM
      DP=DP+2D0*SIN(A)
      DE=DE-COS(A)
*  74
      A=-EL+F2-D2+OM
      DP=DP-2D0*SIN(A)
      DE=DE+COS(A)
*  73
      A=EL+ELP+F2+OM2
      DP=DP+2D0*SIN(A)
      DE=DE-COS(A)
*  72
      A=-ELP+F2+D2+OM2
      DP=DP-3D0*SIN(A)
      DE=DE+COS(A)
*  71
      A=EL3+F2+OM2
      DP=DP-3D0*SIN(A)
      DE=DE+COS(A)
*  70
      A=-EL2+OM
      DP=DP-2D0*SIN(A)
      DE=DE+COS(A)
*  69
      A=-EL-ELP+F2+D2+OM2
      DP=DP-3D0*SIN(A)
      DE=DE+COS(A)
*  68
      A=EL-ELP+F2+OM2
      DP=DP-3D0*SIN(A)
      DE=DE+COS(A)
*  67
      DP=DP+3D0*SIN(EL+F2)
*  66
      DP=DP-3D0*SIN(EL+ELP)
*  65
      DP=DP-4D0*SIN(D)
*  64
      DP=DP+4D0*SIN(EL-F2)
*  63
      DP=DP-4D0*SIN(ELP-D2)
*  62
      A=EL2+F2+OM
      DP=DP-5D0*SIN(A)
      DE=DE+3D0*COS(A)
*  61
      DP=DP+5D0*SIN(EL-ELP)
*  60
      A=-D2+OM
      DP=DP-5D0*SIN(A)
      DE=DE+3D0*COS(A)
*  59
      A=EL+F2-D2+OM
      DP=DP+6D0*SIN(A)
      DE=DE-3D0*COS(A)
*  58
      A=F2+D2+OM
      DP=DP-7D0*SIN(A)
      DE=DE+3D0*COS(A)
*  57
      A=D2+OM
      DP=DP-6D0*SIN(A)
      DE=DE+3D0*COS(A)
*  56
      A=EL2+F2-D2+OM2
      DP=DP+6D0*SIN(A)
      DE=DE-3D0*COS(A)
*  55
      DP=DP+6D0*SIN(EL+D2)
*  54
      A=EL+F2+D2+OM2
      DP=DP-8D0*SIN(A)
      DE=DE+3D0*COS(A)
*  53
      A=-ELP+F2+OM2
      DP=DP-7D0*SIN(A)
      DE=DE+3D0*COS(A)
*  52
      A=ELP+F2+OM2
      DP=DP+7D0*SIN(A)
      DE=DE-3D0*COS(A)
*  51
      DP=DP-7D0*SIN(EL+ELP-D2)
*  50
      A=-EL+F2+D2+OM
      DP=DP-10D0*SIN(A)
      DE=DE+5D0*COS(A)
*  49
      A=EL-D2+OM
      DP=DP-13D0*SIN(A)
      DE=DE+7D0*COS(A)
*  48
      A=-EL+D2+OM
      DP=DP+16D0*SIN(A)
      DE=DE-8D0*COS(A)
*  47
      A=-EL+F2+OM
      DP=DP+21D0*SIN(A)
      DE=DE-10D0*COS(A)
*  46
      DP=DP+26D0*SIN(F2)
      DE=DE-COS(F2)
*  45
      A=EL2+F2+OM2
      DP=DP-31D0*SIN(A)
      DE=DE+13D0*COS(A)
*  44
      A=EL+F2-D2+OM2
      DP=DP+29D0*SIN(A)
      DE=DE-12D0*COS(A)
*  43
      DP=DP+29D0*SIN(EL2)
      DE=DE-COS(EL2)
*  42
      A=F2+D2+OM2
      DP=DP-38D0*SIN(A)
      DE=DE+16D0*COS(A)
*  41
      A=EL+F2+OM
      DP=DP-51D0*SIN(A)
      DE=DE+27D0*COS(A)
*  40
      A=-EL+F2+D2+OM2
      DP=DP-59D0*SIN(A)
      DE=DE+26D0*COS(A)
*  39
      A=-EL+OM
      DP=DP+(-58D0-0.1D0*T)*SIN(A)
      DE=DE+32D0*COS(A)
*  38
      A=EL+OM
      DP=DP+(63D0+0.1D0*T)*SIN(A)
      DE=DE-33D0*COS(A)
*  37
      DP=DP+63D0*SIN(D2)
      DE=DE-2D0*COS(D2)
*  36
      A=-EL+F2+OM2
      DP=DP+123D0*SIN(A)
      DE=DE-53D0*COS(A)
*  35
      A=EL-D2
      DP=DP-158D0*SIN(A)
      DE=DE-COS(A)
*  34
      A=EL+F2+OM2
      DP=DP-301D0*SIN(A)
      DE=DE+(129D0-0.1D0*T)*COS(A)
*  33
      A=F2+OM
      DP=DP+(-386D0-0.4D0*T)*SIN(A)
      DE=DE+200D0*COS(A)
*  32
      DP=DP+(712D0+0.1D0*T)*SIN(EL)
      DE=DE-7D0*COS(EL)
*  31
      A=F2+OM2
      DP=DP+(-2274D0-0.2D0*T)*SIN(A)
      DE=DE+(977D0-0.5D0*T)*COS(A)
*  30
      DP=DP-SIN(ELP+F2-D2)
*  29
      DP=DP+SIN(-EL+D+OM)
*  28
      DP=DP+SIN(ELP+OM2)
*  27
      DP=DP-SIN(ELP-F2+D2)
*  26
      DP=DP+SIN(-F2+D2+OM)
*  25
      DP=DP+SIN(EL2+ELP-D2)
*  24
      DP=DP-4D0*SIN(EL-D)
*  23
      A=ELP+F2-D2+OM
      DP=DP+4D0*SIN(A)
      DE=DE-2D0*COS(A)
*  22
      A=EL2-D2+OM
      DP=DP+4D0*SIN(A)
      DE=DE-2D0*COS(A)
*  21
      A=-ELP+F2-D2+OM
      DP=DP-5D0*SIN(A)
      DE=DE+3D0*COS(A)
*  20
      A=-EL2+D2+OM
      DP=DP-6D0*SIN(A)
      DE=DE+3D0*COS(A)
*  19
      A=-ELP+OM
      DP=DP-12D0*SIN(A)
      DE=DE+6D0*COS(A)
*  18
      A=ELP2+F2-D2+OM2
      DP=DP+(-16D0+0.1D0*T)*SIN(A)
      DE=DE+7D0*COS(A)
*  17
      A=ELP+OM
      DP=DP-15D0*SIN(A)
      DE=DE+9D0*COS(A)
*  16
      DP=DP+(17D0-0.1D0*T)*SIN(ELP2)
*  15
      DP=DP-22D0*SIN(F2-D2)
*  14
      A=EL2-D2
      DP=DP+48D0*SIN(A)
      DE=DE+COS(A)
*  13
      A=F2-D2+OM
      DP=DP+(129D0+0.1D0*T)*SIN(A)
      DE=DE-70D0*COS(A)
*  12
      A=-ELP+F2-D2+OM2
      DP=DP+(217D0-0.5D0*T)*SIN(A)
      DE=DE+(-95D0+0.3D0*T)*COS(A)
*  11
      A=ELP+F2-D2+OM2
      DP=DP+(-517D0+1.2D0*T)*SIN(A)
      DE=DE+(224D0-0.6D0*T)*COS(A)
*  10
      DP=DP+(1426D0-3.4D0*T)*SIN(ELP)
      DE=DE+(54D0-0.1D0*T)*COS(ELP)
*  9
      A=F2-D2+OM2
      DP=DP+(-13187D0-1.6D0*T)*SIN(A)
      DE=DE+(5736D0-3.1D0*T)*COS(A)
*  8
      DP=DP+SIN(EL2-F2+OM)
*  7
      A=-ELP2+F2-D2+OM
      DP=DP-2D0*SIN(A)
      DE=DE+1D0*COS(A)
*  6
      DP=DP-3D0*SIN(EL-ELP-D)
*  5
      A=-EL2+F2+OM2
      DP=DP-3D0*SIN(A)
      DE=DE+1D0*COS(A)
*  4
      DP=DP+11D0*SIN(EL2-F2)
*  3
      A=-EL2+F2+OM
      DP=DP+46D0*SIN(A)
      DE=DE-24D0*COS(A)
*  2
      DP=DP+(2062D0+0.2D0*T)*SIN(OM2)
      DE=DE+(-895D0+0.5D0*T)*COS(OM2)
*  1
      DP=DP+(-171996D0-174.2D0*T)*SIN(OM)
      DE=DE+(92025D0+8.9D0*T)*COS(OM)

*  Convert results to radians
      DPSI=DP*U2R
      DEPS=DE*U2R

*  Mean obliquity
      EPS0=AS2R*(84381.448D0+
     :           (-46.8150D0+
     :           (-0.00059D0+
     :           0.001813D0*T)*T)*T)

      END
      SUBROUTINE SLAPRE (EP0, EP1, RMATP)
*+
*     - - - - - -
*      P R E C L
*     - - - - - -
*
*  Form the matrix of precession between two epochs, using the
*  model of Simon et al (1994), which is suitable for long
*  periods of time.
*
*  (double precision)
*
*  Given:
*     EP0    dp         beginning epoch
*     EP1    dp         ending epoch
*
*  Returned:
*     RMATP  dp(3,3)    precession matrix
*
*  Notes:
*
*     1)  The epochs are TDB Julian epochs.
*
*     2)  The matrix is in the sense   V(EP1)  =  RMATP * V(EP0)
*
*     3)  The absolute accuracy of the model is limited by the
*         uncertainty in the general precession, about 0.3 arcsec per
*         1000 years.  The remainder of the formulation provides a
*         precision of 1 mas over the interval from 1000AD to 3000AD,
*         0.1 arcsec from 1000BC to 5000AD and 1 arcsec from
*         4000BC to 8000AD.
*
*  Reference:
*     Simon, J.L. et al., 1994. Astron.Astrophys., 282, 663-683.
*
*  Called:  SLAEUL
*
*  P.T.Wallace   Starlink   23 August 1994
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

C IMPLICIT NONE

      DOUBLE PRECISION EP0,EP1,RMATP(3,3)

*  Arc seconds to radians
      DOUBLE PRECISION AS2R
      PARAMETER (AS2R=0.4848136811095359949D-05)

      DOUBLE PRECISION T0,T,TAS2R,W,ZETA,Z,THETA



*  Interval between basic epoch J2000.0 and beginning epoch (1000JY)
      T0 = (EP0-2000D0)/1000D0

*  Interval over which precession required (1000JY)
      T = (EP1-EP0)/1000D0

*  Euler angles
      TAS2R = T*AS2R
      W =      23060.9097D0+
     :          (139.7459D0+
     :           (-0.0038D0+
     :           (-0.5918D0+
     :           (-0.0037D0+
     :             0.0007D0*T0)*T0)*T0)*T0)*T0

      ZETA =   (W+(30.2226D0+
     :            (-0.2523D0+
     :            (-0.3840D0+
     :            (-0.0014D0+
     :              0.0007D0*T0)*T0)*T0)*T0+
     :            (18.0183D0+
     :            (-0.1326D0+
     :             (0.0006D0+
     :              0.0005D0*T0)*T0)*T0+
     :            (-0.0583D0+
     :            (-0.0001D0+
     :              0.0007D0*T0)*T0+
     :            (-0.0285D0+
     :            (-0.0002D0)*T)*T)*T)*T)*T)*TAS2R

      Z =     (W+(109.5270D0+
     :             (0.2446D0+
     :            (-1.3913D0+
     :            (-0.0134D0+
     :              0.0026D0*T0)*T0)*T0)*T0+
     :            (18.2667D0+
     :            (-1.1400D0+
     :            (-0.0173D0+
     :              0.0044D0*T0)*T0)*T0+
     :            (-0.2821D0+
     :            (-0.0093D0+
     :              0.0032D0*T0)*T0+
     :            (-0.0301D0+
     :              0.0006D0*T0
     :             -0.0001D0*T)*T)*T)*T)*T)*TAS2R

      THETA =  (20042.0207D0+
     :           (-85.3131D0+
     :            (-0.2111D0+
     :             (0.3642D0+
     :             (0.0008D0+
     :            (-0.0005D0)*T0)*T0)*T0)*T0)*T0+
     :           (-42.6566D0+
     :            (-0.2111D0+
     :             (0.5463D0+
     :             (0.0017D0+
     :            (-0.0012D0)*T0)*T0)*T0)*T0+
     :           (-41.8238D0+
     :             (0.0359D0+
     :             (0.0027D0+
     :            (-0.0001D0)*T0)*T0)*T0+
     :            (-0.0731D0+
     :             (0.0019D0+
     :              0.0009D0*T0)*T0+
     :            (-0.0127D0+
     :              0.0011D0*T0+0.0004D0*T)*T)*T)*T)*T)*TAS2R

*  Rotation matrix
      CALL SLAEUL('ZYZ',-ZETA,THETA,-Z,RMATP)

      END
      DOUBLE PRECISION FUNCTION SLADAT (UTC)
*+
*     - - - -
*      D A T
*     - - - -
*
*  Increment to be applied to Coordinated Universal Time UTC to give
*  International Atomic Time TAI (double precision)
*
*  Given:
*     UTC      d      UTC date as a modified JD (JD-2400000.5)
*
*  Result:  TAI-UTC in seconds
*
*  Notes:
*
*  1  The UTC is specified to be a date rather than a time to indicate
*     that care needs to be taken not to specify an instant which lies
*     within a leap second.  Though in most cases UTC can include the
*     fractional part, correct behaviour on the day of a leap second
*     can only be guaranteed up to the end of the second 23:59:59.
*
*  2  Pre 1972 January 1 a fixed value of 10 sec is returned.
*
*     :-----------------------------------------:
*     :                                         :
*     :                IMPORTANT                :
*     :                                         :
*     :  This routine must be updated on each   :
*     :     occasion that a leap second is      :
*     :                announced                :
*     :                                         :
*     :  Latest leap second:  1999 Jan 1        :
*     :                                         :
*     :-----------------------------------------:
*
*  P.T.Wallace   Starlink   14 November 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

C IMPLICIT NONE

      DOUBLE PRECISION UTC

      DOUBLE PRECISION DT



      DT = 10D0

*  1972 July 1
      IF (UTC.GE.41499D0) DT=11D0

*  1973 January 1
      IF (UTC.GE.41683D0) DT=12D0

*  1974 January 1
      IF (UTC.GE.42048D0) DT=13D0

*  1975 January 1
      IF (UTC.GE.42413D0) DT=14D0

*  1976 January 1
      IF (UTC.GE.42778D0) DT=15D0

*  1977 January 1
      IF (UTC.GE.43144D0) DT=16D0

*  1978 January 1
      IF (UTC.GE.43509D0) DT=17D0

*  1979 January 1
      IF (UTC.GE.43874D0) DT=18D0

*  1980 January 1
      IF (UTC.GE.44239D0) DT=19D0

*  1981 July 1
      IF (UTC.GE.44786D0) DT=20D0

*  1982 July 1
      IF (UTC.GE.45151D0) DT=21D0

*  1983 July 1
      IF (UTC.GE.45516D0) DT=22D0

*  1985 July 1
      IF (UTC.GE.46247D0) DT=23D0

*  1988 January 1
      IF (UTC.GE.47161D0) DT=24D0

*  1990 January 1
      IF (UTC.GE.47892D0) DT=25D0

*  1991 January 1
      IF (UTC.GE.48257D0) DT=26D0

*  1992 July 1
      IF (UTC.GE.48804D0) DT=27D0

*  1993 July 1
      IF (UTC.GE.49169D0) DT=28D0

*  1994 July 1
      IF (UTC.GE.49534D0) DT=29D0

*  1996 January 1
      IF (UTC.GE.50083D0) DT=30D0

*  1997 July 1
      IF (UTC.GE.50630-50630D0) DT=31D0

*  1999 Jan 1
      IF (UTC.GE.51179D0) DT=32D0

* - - - - - - - - - - - - - - - - - - - - - - - - - - *
*  (Add a new pair of lines to the above set on each  *
*      occasion that a leap second is announced)      *
* - - - - - - - - - - - - - - - - - - - - - - - - - - *

      SLADAT=DT

      END
