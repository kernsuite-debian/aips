      SUBROUTINE CD (JD, BESSC, BESSD)
C-----------------------------------------------------------------------
C! Computes Besselian day numbers C and D for aberration
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
C-----------------------------------------------------------------------
C   High-precision calculation of aberration.  Given the Julian date,
C   Besselian day numbers C and D are calculated (in arcseconds).  This
C   is an implementation of an algorithm by R. D'E. Atkinson appearing
C   in A.J., VOL. 77, AUGUST, 1972.
C   Angles are in radians
C    Input:
C     JD      D  Julian date
C    Output:
C     BESSC   D  Besselian day number C
C     BESSD   D  Besselian day number D
C-----------------------------------------------------------------------
      INTEGER   N
      DOUBLE PRECISION JD, BESSC, BESSD
      DOUBLE PRECISION AMPL(17), RADV(17), RATE(17), ANGL(17),
     *   RPL(17), D, L,
     *   G, E, G1, U, COSEPS, CNT, LPL, LPR, DLPL, DLPR, DPLT, C1, C2,
     *   PPL, PPR, LGPL, STL, TAU, PRDAL, DPPR, DPPL, DPR, VLON, VLT,
     *   V1, V2, V3, SINSTL, COSSTL, SINEPS, GJ, LJ, GS, LS, MVJ, MVS,
     *   S1, S2, MR, C3
      DATA AMPL /.21D0,.164D0,4.838D0,5.526D0,2.497D0,.666D0,
     *   1.559D0,2.043D0,7.208D0,2.731D0,1.61D0,.556D0,1.024D0,
     *   1.77D0,.5D0,.585D0,2.6D0/
      DATA RADV /376D0,281D0,2359D0,6842D0,869D0,1045D0,1497D0,
     *   2057D0,7067D0,4026D0,1459D0,803D0,194D0,151D0,478D0,105D0,
     *   244D0/
      DATA RATE /900.73772D0,988.934D0,225.18443D0,450.36886D0,
     *   90.37836D0,675.55329D0,315.56279D0,337.18146D0,329.64467D0,
     *   659.28934D0,299.29884D0,628.94351D0,-44.4277D0,-22.80904D0,
     *   314.37241D0,-45.61808D0,-30.34583D0/
      DATA ANGL /342.136D0,210.488D0,153.069D0,216.265D0,25.722D0,
     *   99.542D0,268.698D0,61.65D0,312.734D0,353.519D0,17.409D0,
     *   123.712D0,243.12D0,279.718D0,262.293D0,132.692D0,37.937D0/
      DATA MR, C3 /1.496D8, 7.96325D-6/
C-----------------------------------------------------------------------
C                                       D is days from 1900 January 0.5
      D = JD - 2415020.D0
C                                       Sun's mean longitude:
      L = 4.8816279D0 + .01720279126D0 * D + 2.4D-6
C                                       Sun's mean anomaly:
      G = 6.25658D0 + .0172019698D0 * D
C                                       Moon's mean elongation:
      E = 6.1215D0 + .2127687117D0 * D
C                                       Moon's mean anomaly:
      G1 = 5.168D0 + .2280271350D0 * D
C                                       Moon's mean argument of
C                                       latitude:
      U = .1964D0 + .2308957232D0 * D
C                                       Cosine of eccentricity:
      COSEPS = .9173918D0 + 2.4747D-9 * D
C                                       Sun's equation of center:
      CNT = (6910.057D0 - 4.720D-4 * D) * SIN(G)
     *   + 72.100D0 * SIN(G+G) + 1.054D0 * SIN(3.D0*G)
C                                       Lunar perturbation in
C                                       longitude ("):
      LPL = 6.454D0 * SIN(E) - 0.424D0 * SIN(E-G1)
C                                       Logarithm of the radius vector
C                                       (units of 8th dec.):
      LPR = 1336.D0 * COS(E) - 133.D0 * COS(E-G1)
C                                       Daily change of lunar
C                                       perturbation in radius vector
C                                       (change in 8th dec. of log/day)
      DLPR = -284.26D0 * SIN(E) - 1.91D0 * SIN(3D0*E)
     *   - 16.31D0 * SIN(E+G1) - 2.03D0 * SIN(E-G1)
     *   - 3.28D0 * SIN(3D0*E-G1) + 3.22D0 * SIN(E+G)
     *   - 7.04D0 * SIN(E-G)
C                                       Daily change ("/day) of lunar
C                                       perturbation in longitude:
      DLPL = 1.3732D0 * COS(E) + .0083D0 * COS(3D0*E)
     *    + .0780D0 * COS(E+G1)
     *    + .0065D0 * COS(E-G1) + .0160D0 * COS(3D0*E-G1)
     *   -.0147D0 * COS(E+G) + .0336D0 * COS(E-G)
C                                       Daily change in perturbation in
C                                       latitude ("/day):
      DPLT = .1330D0 * COS(U)
C                                       Perturbation arguments (radians)
      C1 = D / 365.25D0
      C2 = 4D0 * ATAN(1D0) / 180.D0
      DO 10 N = 1,17
        RPL(N) = (ANGL(N) + RATE(N) * C1) * C2
 10     CONTINUE
C                                       Total planetary perturbation in
C                                       longitude (")
      PPL = 0.D0
      DO 20 N = 3,17
         PPL = PPL + AMPL(N) * COS(RPL(N))
 20      CONTINUE
C                                       Planetary perturbation in LOG
C                                       (radius) (units of 9th decimal):
      PPR = 0.D0
      DO 30 N = 3,12
         PPR = PPR + RADV(N) * SIN(RPL(N))
 30      CONTINUE
C                                       Long-period perturbation in
C                                       longitude ("):
      LGPL = 6.4D0 * SIN(4.03503D0 + 9.6525D-6 * D)
     *    + 1.871D0 * SIN(0.99903D0 + 7.1806D-5 * D)
C                                       Sun's true longitude (mean
C                                       equinox of date):
      STL = L + (CNT + LPL + PPL + LGPL) / 206264.8062D0
      TAU = (D - .313D0) / 365.242194D0
      TAU = MOD (TAU, 1.D0)
      IF (TAU.LT..5D0) STL = STL - 2.437D-4 * TAU
      IF (TAU.GE..5D0) STL = STL + 2.437D-4 * (1.D0 - TAU)
C                                       (Km/day):
      PRDAL = 5.9256D6 * (LPR * 1.D-8 + PPR * 1.D-9)
C                                       Planetary rates of change (9th
C                                       dec. of log per day):
      DPPR = 0.D0
      C2 = C2 / 365.25D0
      DO 40 N = 1,12
         DPPR = DPPR + RADV(N) * RATE(N) * COS(RPL(N))
 40      CONTINUE
      DPPR = DPPR * C2
C                                       ("/day):
      DPPL = 0.E0
      DO 50 N = 1,12
         DPPL = DPPL-AMPL(N) * RATE(N) * SIN(RPL(N))
 50      CONTINUE
      DPPL = DPPL * C2
C                                       Velocity in radius vector
C                                       (km/day):
      DPR = 2.3026D0 * MR * (DLPR * 1D-8 + DPPR * 1D-9)
C                                       Velocity in longitude (km/day):
      VLON = PRDAL + MR * (DLPL+DPPL) / 206264.8062D0
C                                       Velocity in latitude (km/day):
      VLT = MR * DPLT / 206264.8062D0
C                                       Velocities of sun wrt earth
C                                       (aberration units):
      V1 = C3 * DPR
      V2 = 20.4958D0 + C3 * VLON
      V3 = C3 * VLT
C                                       Now get C and D from (3,4). (")
      SINSTL = SIN(STL)
      COSSTL = COS(STL)
      SINEPS = SQRT(1.D0 - COSEPS * COSEPS)
C                                       D  =  X DOT.
      BESSD = V1 * COSSTL - V2 * SINSTL
C                                       C  =  - Y DOT.
      BESSC = V3 * SINEPS - (V1 * SINSTL + V2 * COSSTL) * COSEPS
C                                       Now only have to add in the
C                                       planetary barycenter terms.
C                                       Jupiter mean anomaly & true
C                                       longitude:
      GJ = -27.481D0 + .0014501127D0 * D
      LJ = -27.2587D0 + .0014508827D0 * D
     *   + .09684D0 * SIN(GJ) + .00293D0 * SIN(GJ+GJ)
C                                       Saturn mean anomaly & true
C                                       longitude:
      GS = -9.508D0 + .0005837134D0 * D
      LS = -7.9185D0 + .0005846491D0 * D
     *    + .11136D0 * SIN(GS) + .00388D0 * SIN(GS+GS)
C                                       Add them in:
      MVJ = .008581D0
      MVS = .001896D0
      S1 = .0004D0
      S2 = .000204D0
      BESSD = BESSD - (MVJ * SIN(LJ) + MVS * SIN(LS) + S2)
      BESSC = BESSC - (MVJ * COS(LJ) + MVS * COS(LS) + S1) * COSEPS
C
 999  RETURN
      END
