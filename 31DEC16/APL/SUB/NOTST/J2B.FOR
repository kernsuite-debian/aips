      SUBROUTINE J2B (R2000, D2000, BEPOCH, R1950, D1950,
     *   DR1950, DD1950)
C-------------------------------------------------------------------
C! Convert J2000 to B1950 positions of the source
C# Math
C-----------------------------------------------------------------------
C;  Copyright (C) 2004
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
C   Routine to convert a J2000.0 FK5 source  position to B1950.0 FK4
C   assuming zero proper motion and parallax (double precision)
C
C   This routine converts the source  positions from the IAU 1976,
C   FK5, Fricke system to the Bessel-Newcomb, FK4 system.
C
C   Inputs:
C      R2000,D2000      D    J2000.0 FK5 RA, Dec (radians)
C      BEPOCH           D    Besselian epoch (e.g. 1979.3D0, 1950.0D0)
C
C   Outputs
C      R1950,D1950      D    B1950.0 FK4 RA,Dec (rad) at epoch BEPOCH
C      DR1950,DD1950    D    B1950.0 FK4 RA,Dec (rad) proper motions
C                            (rad/trop.yr)
C
C   Notes:
C
C   1)  The proper motion in RA is dRA/dt rather than cos(Dec)*dRA/dt.
C
C   2)  Conversion from Julian epoch 2000.0 to Besselian epoch 1950.0
C       only is provided for.  Conversions involving other epochs will
C       require use of the appropriate precession routines before and
C       after this routine is called.
C
C   3)  Unlike in the sla_FK524 routine, the FK5 proper motions, the
C       parallax and the radial velocity are presumed zero.
C
C   4)  It is the intention that FK5 should be a close approximation
C       to an inertial frame, so that distant objects have zero proper
C       motion;  such objects have (in general) non-zero proper motion
C       in FK4, and this routine returns those fictitious proper
C       motions.
C
C   5)  The position returned by this routine is in the B1950
C       reference frame but at Besselian epoch BEPOCH.  For
C       comparison with catalogues the BEPOCH argument will
C       frequently be 1950D0.
C
C   Called:  F52F4, PM
C
C   This subroutine is a modified verion of the SLALIB routine FK54Z
C   written by P.T.Wallace   Starlink   10 April 1990,
C   Rutherford Appleton Laboratory
C-------------------------------------------------------------------
      DOUBLE PRECISION R2000, D2000, BEPOCH, R1950, D1950, DR1950,
     *   DD1950
      DOUBLE PRECISION R,D,PX,RV
C
C                                       FK5 equinox J2000 (any epoch) to
C                                       FK4 equinox B1950 epoch B1950
      CALL F52F4(R2000, D2000, 0D0, 0D0, 0D0, 0D0, R, D, DR1950,
     *   DD1950, PX, RV)
C                                       Fictitious proper motion to
C                                       epoch BEPOCH
      CALL PM(R, D, DR1950, DD1950, 0D0, 0D0, 1950D0,BEPOCH,
     *            R1950,D1950)

      END
C
C
      SUBROUTINE F52F4 (R2000, D2000, DR2000, DD2000, P2000, V2000,
     *   R1950, D1950, DR1950, DD1950, P1950, V1950)
C-------------------------------------------------------------------
C
C   Routine to Convert J2000.0 FK5 source data to B1950.0 FK4
C   (double precision)
C
C   This routine converts source positions from the new, IAU 1976, FK5,
C   Fricke system, to the old, Bessel-Newcomb, FK4 system.  The precepts
C   of Smith et al (Ref 1) are followed, using the implementation
C   by Yallop et al (Ref 2) of a matrix method due to Standish.
C   Kinoshita's development of Andoyer's post-Newcomb precession is
C   used.  The numerical constants from Seidelmann et al (Ref 3) are
C   used canonically.
C
C   Inputs:  (all J2000.0,FK5)
C      R2000,D2000     D    J2000.0 RA,Dec (rad)
C      DR2000,DD2000   D    J2000.0 proper motions (rad/Jul.yr)
C      P2000           D    parallax (arcsec)
C      V2000           D    radial velocity
C                           (km/s, positive => moving away)
C   Outputs:  (all B1950.0,FK4)
C      R1950,D1950     D    B1950.0 RA,Dec (rad)
C      DR1950,DD1950   D    B1950.0 proper motions (rad/trop.yr)
C      P1950           D    parallax (arcsec)
C      V1950           D    radial velocity
C                           (km/s, positive => moving away)
C   Notes:
C
C   1)  The proper motions in RA are dRA/dt rather than
C       cos(Dec)C dRA/dt, and are per year rather than per century.
C
C   2)  Note that conversion from Julian epoch 2000.0 to Besselian
C       epoch 1950.0 only is provided for.  Conversions involving
C       other epochs will require use of the appropriate precession,
C       proper motion, and E-terms routines before and/or after
C       F52F4 is called.
C
C   3)  In the FK4 catalogue the proper motions of stars within
C       10 degrees of the poles do not embody the differential
C       E-term effect and should, strictly speaking, be handled
C       in a different manner from stars outside these regions.
C       However, given the general lack of homogeneity of the star
C       data available for routine astrometry, the difficulties of
C       handling positions that may have been determined from
C       astrometric fields spanning the polar and non-polar regions,
C       the likelihood that the differential E-terms effect was not
C       taken into account when allowing for proper motion in past
C       astrometry, and the undesirability of a discontinuity in
C       the algorithm, the decision has been made in this routine to
C       include the effect of differential E-terms on the proper
C       motions for all stars, whether polar or not.  At epoch 2000,
C       and measuring on the sky rather than in terms of dRA, the
C       errors resulting from this simplification are less than
C       1 milliarcsecond in position and 1 milliarcsecond per
C       century in proper motion.
C
C   References:
C
C      1  Smith, C.A. et al, 1989.  "The transformation of astrometric
C         catalog systems to the equinox J2000.0".  Astron.J. 97, 265.
C
C      2  Yallop, B.D. et al, 1989.  "Transformation of mean star places
C         from FK4 B1950.0 to FK5 J2000.0 using matrices in 6-space".
C         Astron.J. 97, 274.
C
C      3  Seidelmann, P.K. (ed), 1992.  "Explanatory Supplement to
C         the Astronomical Almanac", ISBN 0-935702-68-7.
C
C   This subroutine is a modified verion of the SLALIB routine FK524
C   written by P.T.Wallace   Starlink  19 December 1993,
C   Rutherford Appleton Laboratory
C-------------------------------------------------------------------
      DOUBLE PRECISION R2000, D2000, DR2000, DD2000, P2000, V2000,
     *                 R1950, D1950, DR1950, DD1950, P1950, V1950
C                                       Miscellaneous
      DOUBLE PRECISION R, D, UR, UD, PX, RV
      DOUBLE PRECISION SR, CR, SD, CD, X, Y, Z, W
      DOUBLE PRECISION V1(6), V2(6)
      DOUBLE PRECISION XD, YD, ZD
      DOUBLE PRECISION RXYZ, WD, RXYSQ, RXY
      INTEGER I, J
C                                       2Pi
      DOUBLE PRECISION D2PI
      PARAMETER (D2PI=6.283185307179586476925287D0)
C                                       Radians per year to arcsec per
C                                       century
      DOUBLE PRECISION PMF
      PARAMETER (PMF = 100D0*60D0*60D0*360D0/D2PI)
C                                       Small number to avoid arithmetic
C                                       problems
      DOUBLE PRECISION TINY
      PARAMETER (TINY=1D-30)
C                                       CANONICAL CONSTANTS  (see
C                                       references)
C                                       Km per sec to AU per tropical
C                                       century
C                                       86400 * 36524.2198782 /
C                                       149597870 = 21.095
      DOUBLE PRECISION VF
      PARAMETER (VF=21.095D0)

C   Constant vector and matrix (by columns)
      DOUBLE PRECISION A(6), EMI(6,6)
      DATA A/ -1.62557D-6,  -0.31919D-6, -0.13843D-6,
     *        +1.245D-3,    -1.580D-3,   -0.659D-3/
C
      DATA (EMI(I,1),I=1,6) / +0.9999256795D0,
     *                        -0.0111814828D0,
     *                        -0.0048590040D0,
     *                        -0.000551D0,
     *                        -0.238560D0,
     *                        +0.435730D0 /

      DATA (EMI(I,2),I=1,6) / +0.0111814828D0,
     *                        +0.9999374849D0,
     *                        -0.0000271557D0,
     *                        +0.238509D0,
     *                        -0.002667D0,
     *                        -0.008541D0 /

      DATA (EMI(I,3),I=1,6) / +0.0048590039D0,
     *                        -0.0000271771D0,
     *                        +0.9999881946D0,
     *                        -0.435614D0,
     *                        +0.012254D0,
     *                        +0.002117D0 /

      DATA (EMI(I,4),I=1,6) / -0.00000242389840D0,
     *                        +0.00000002710544D0,
     *                        +0.00000001177742D0,
     *                        +0.99990432D0,
     *                        -0.01118145D0,
     *                        -0.00485852D0 /

      DATA (EMI(I,5),I=1,6) / -0.00000002710544D0,
     *                        -0.00000242392702D0,
     *                        +0.00000000006585D0,
     *                        +0.01118145D0,
     *                        +0.99991613D0,
     *                        -0.00002716D0 /

      DATA (EMI(I,6),I=1,6) / -0.00000001177742D0,
     *                        +0.00000000006585D0,
     *                        -0.00000242404995D0,
     *                        +0.00485852D0,
     *                        -0.00002717D0,
     *                        +0.99996684D0 /
C-------------------------------------------------------------------


C                                       Pick up J2000 data (units
C                                       radians and arcsec/JC)
      R = R2000
      D = D2000
      UR = DR2000*PMF
      UD = DD2000*PMF
      PX = P2000
      RV = V2000

C                                       Spherical to Cartesian
      SR = SIN(R)
      CR = COS(R)
      SD = SIN(D)
      CD = COS(D)

      X = CR*CD
      Y = SR*CD
      Z =   SD

      W = VF*RV*PX

      V1(1) = X
      V1(2) = Y
      V1(3) = Z

      V1(4) = -UR*Y - CR*SD*UD + W*X
      V1(5) = UR*X - SR*SD*UD + W*Y
      V1(6) = CD*UD + W*Z

C                                       Convert position+velocity vector
C                                       to BN system
      DO 20 I = 1, 6
         W = 0D0
         DO 10 J =1, 6
            W = W + EMI(I,J)*V1(J)
   10       CONTINUE
         V2(I) = W
   20    CONTINUE

C                                       Position vector components and
C                                       magnitude
      X = V2(1)
      Y = V2(2)
      Z = V2(3)
      RXYZ = SQRT(X*X + Y*Y + Z*Z)

C                                       Apply E-terms to position
      W = X*A(1) + Y*A(2) + Z*A(3)
      X = X + A(1)*RXYZ - W*X
      Y = Y + A(2)*RXYZ - W*Y
      Z = Z + A(3)*RXYZ - W*Z

C                                       Recompute magnitude
      RXYZ = SQRT(X*X + Y*Y + Z*Z)

C                                       Apply E-terms to both position
C                                       and velocity
      X = V2(1)
      Y = V2(2)
      Z = V2(3)
      W = X*A(1) + Y*A(2) + Z*A(3)
      WD = X*A(4) + Y*A(5) + Z*A(6)
      X = X + A(1)*RXYZ - W*X
      Y = Y + A(2)*RXYZ - W*Y
      Z = Z + A(3)*RXYZ - W*Z
      XD = V2(4) + A(4)*RXYZ - WD*X
      YD = V2(5) + A(5)*RXYZ - WD*Y
      ZD = V2(6) + A(6)*RXYZ - WD*Z

C                                       Convert to spherical
      RXYSQ = X*X + Y*Y
      RXY = SQRT(RXYSQ)

      IF (X.EQ.0D0 .AND. Y.EQ.0D0) THEN
         R = 0D0
      ELSE
         R = ATAN2(Y,X)
         IF (R.LT.0.0D0) R = R + D2PI
         END IF
      D = ATAN2(Z,RXY)

      IF (RXY .GT. TINY) THEN
         UR = (X*YD - Y*XD)/RXYSQ
         UD = (ZD*RXYSQ - Z*(X*XD + Y*YD))/((RXYSQ + Z*Z)*RXY)
         END IF

C   Radial velocity and parallax
      IF (PX .GT. TINY) THEN
         RV = (X*XD + Y*YD + Z*ZD)/(PX*VF*RXYZ)
         PX = PX/RXYZ
         END IF

C                                       Return results
      R1950 = R
      D1950 = D
      DR1950 = UR/PMF
      DD1950 = UD/PMF
      P1950 = PX
      V1950 = RV
C
      END
C
C
      SUBROUTINE PM (R0, D0, PR, PD, PX, RV, EP0, EP1, R1, D1)
C
C  Apply corrections for proper motion to a star RA,Dec
C  (double precision)
C
C  References:
C     1984 Astronomical Almanac, pp B39-B41.
C     (also Lederle & Schwan, Astron. Astrophys. 134,
C      1-6, 1984)
C
C  Inputs:
C     R0,D0    D     RA,Dec at epoch EP0 (rad)
C     PR,PD    D     proper motions:  RA,Dec changes per year of epoch
C     PX       D     parallax (arcsec)
C     RV       D     radial velocity (km/sec, +ve if receding)
C     EP0      D     start epoch in years (e.g. Julian epoch)
C     EP1      D     end epoch in years (same system as EP0)
C
C  Outputs:
C     R1,D1    D     RA,Dec at epoch EP1 (rad)
C
C  Note:
C     The proper motions in RA are dRA/dt rather than
C     cos(Dec)*dRA/dt, and are in the same coordinate
C     system as R0,D0.
C
C   This subroutine is a modified verion of the SLALIB routine PM
C   written by P.T.Wallace   Starlink  12 April 1990,
C   Rutherford Appleton Laboratory
C-------------------------------------------------------------------
      DOUBLE PRECISION R0,D0,PR,PD,PX,RV,EP0,EP1,R1,D1

C  Km/s to AU/year multiplied by arc seconds to radians
      DOUBLE PRECISION VFR
      PARAMETER (VFR=0.21094502D0*0.4848136811095359949D-05)

      INTEGER I
      DOUBLE PRECISION W,EM(3),T,P(3)
      DOUBLE PRECISION X, Y, Z, R
      DOUBLE PRECISION D2PI
      PARAMETER (D2PI=6.283185307179586476925286766559D0)
C  Spherical to Cartesian
C      CALL sla_DCS2C(R0,D0,P)
      P(1) = COS(R0)*COS(D0)
      P(2) = SIN(R0)*COS(D0)
      P(3) = SIN(D0)
C  Space motion (radians per year)
      W = VFR*RV*PX
      EM(1) = -PR*P(2)-PD*COS(R0)*SIN(D0)+W*P(1)
      EM(2) =  PR*P(1)-PD*SIN(R0)*SIN(D0)+W*P(2)
      EM(3) =         PD*COS(D0)        +W*P(3)

C  Apply the motion
      T = EP1-EP0
      DO 10 I = 1,3
         P(I) = P(I)+T*EM(I)
   10    CONTINUE

C  Cartesian to spherical
C      CALL sla_DCC2S(P,R1,D1)
      X = P(1)
      Y = P(2)
      Z = P(3)
      R = SQRT(X*X+Y*Y)

      IF (R.EQ.0D0) THEN
         R1 = 0D0
      ELSE
         R1 = ATAN2(Y,X)
         END IF

      IF (Z.EQ.0D0) THEN
         D1 = 0D0
      ELSE
         D1 = ATAN2(Z,R)
         END IF
C      R1=sla_DRANRM(R1)
      R1 = MOD(R1,D2PI)
      IF (R1.LT.0D0) R1 = R1 + D2PI
C
      END










