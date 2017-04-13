      SUBROUTINE JPRENU (DIR, DATJUL, EPOCH, DONUT, ROTMAT)
C-----------------------------------------------------------------------
C! Compute rotation matrix for precession and nutation IAU 1980 series.
C# Math Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2001
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
C   Routine to compute the rotation matrix for rectangular source
C   coordinates for precession and nutation using the conventions for
C   the J2000 system.
C      The precession series is from the Supplement to the 1984
C    Astronomical Almanac except for the terms independent of T which
C    were taken from the 1986 Astronomical Almanac to give agreement
C    with the tabulated values.
C    Input:
C     DIR          I  1 = From Epoch to DATJUL
C                     -1 = from DATJUL to Epoch
C     DATJUL       D  Julian date
C     EPOCH        D  Reference epoch date (e.g. 2000.0)
C     DONUT        L  If true include nutation terms.
C    Output:
C     ROTMAT(3,3)  D  Rotation matrix
C-----------------------------------------------------------------------
      INTEGER   DIR
      DOUBLE PRECISION DATJUL, EPOCH, ROTMAT(3,3)
      LOGICAL   DONUT
      DOUBLE PRECISION DELPSI, DELEPS, PR(3,3), NR(3,3), ZETA,
     *   Z, THETA, T, TL, CON, JDREF, CC, CZETA, SZETA, CZ, SZ,
     *   CTHETA, STHETA, EPS, CEPS, SEPS
      INTEGER   IERR
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      CC = TWOPI / 1296000.0D0
      JDREF = 1721045.0D0 + EPOCH * 365.25D0
C                                       General precession
      T = (JDREF - 2451545.0D0) / 36525.0D0
      TL = (DATJUL - JDREF) / 36525
      CON = 2306.21796D0 + (1.39656D0 - 0.000139D0*T) * T
      ZETA = CC * (((0.0180D0 * TL) + (0.30204D0 - 0.000344*T)) * TL +
     *   CON) * TL
      Z = CC * (((0.01836D0 * TL) + 1.09476D0 + 0.000066*T) * TL +
     *   CON) * TL
      THETA = CC * (((-.04176D0 * TL) - (0.4266D0 + 0.000217*T)) * TL +
     *   2004.3108D0 - (0.85330D0 + 0.000217D0*T) * T) * TL
      CZETA = COS (ZETA)
      SZETA = SIN (ZETA)
      CZ = COS (Z)
      SZ = SIN (Z)
      CTHETA = COS (THETA)
      STHETA = SIN (THETA)
C                                       Precession rotation matrix
      PR(1,1) = CZETA*CTHETA*CZ - SZETA*SZ
      PR(1,2) = -SZETA*CTHETA*CZ - CZETA*SZ
      PR(1,3) = -STHETA*CZ
      PR(2,1) = CZETA*CTHETA*SZ + SZETA*CZ
      PR(2,2) = -SZETA*CTHETA*SZ + CZETA*CZ
      PR(2,3) = -STHETA*SZ
      PR(3,1) = CZETA*STHETA
      PR(3,2) = -SZETA*STHETA
      PR(3,3) = CTHETA
C                                       Nutation
      IF (DONUT) THEN
         CALL JNUT (DATJUL, DELPSI, DELEPS)
         EPS =  CC * 3600.0D0 * ((((5.04D-7 * T) - 1.6D-7) * T -
     *      0.0130042D0) * T + 23.439291D0)
         CEPS = COS (EPS)
         SEPS = SIN (EPS)
         NR(1,1) = 1.0D0
         NR(1,2) = -DELPSI * CEPS
         NR(1,3) = -DELPSI * SEPS
         NR(2,1) = DELPSI * CEPS
         NR(2,2) = 1.0D0
         NR(2,3) = -DELEPS
         NR(3,1) = DELPSI * SEPS
         NR(3,2) = DELEPS
         NR(3,3) = 1.0D0
C                                       Nutation * precession
         ROTMAT(1,1) =         PR(1,1) + NR(1,2)*PR(2,1) +
     *      NR(1,3)*PR(3,1)
         ROTMAT(1,2) = NR(2,1)*PR(1,1) +         PR(2,1) +
     *      NR(2,3)*PR(3,1)
         ROTMAT(1,3) = NR(3,1)*PR(1,1) + NR(3,2)*PR(2,1) +
     *      PR(3,1)
         ROTMAT(2,1) =         PR(1,2) + NR(1,2)*PR(2,2) +
     *      NR(1,3)*PR(3,2)
         ROTMAT(2,2) = NR(2,1)*PR(1,2) +         PR(2,2) +
     *      NR(2,3)*PR(3,2)
         ROTMAT(2,3) = NR(3,1)*PR(1,2) + NR(3,2)*PR(2,2) +
     *      PR(3,2)
         ROTMAT(3,1) =         PR(1,3) + NR(1,2)*PR(2,3) +
     *      NR(1,3)*PR(3,3)
         ROTMAT(3,2) = NR(2,1)*PR(1,3) +         PR(2,3) +
     *      NR(2,3)*PR(3,3)
         ROTMAT(3,3) = NR(3,1)*PR(1,3) + NR(3,2)*PR(2,3) +
     *      PR(3,3)
      ELSE
C                                       Precession only
         ROTMAT(1,1) = PR(1,1)
         ROTMAT(2,1) = PR(2,1)
         ROTMAT(3,1) = PR(3,1)
         ROTMAT(1,2) = PR(1,2)
         ROTMAT(2,2) = PR(2,2)
         ROTMAT(3,2) = PR(3,2)
         ROTMAT(1,3) = PR(1,3)
         ROTMAT(2,3) = PR(2,3)
         ROTMAT(3,3) = PR(3,3)
         END IF
C                                       inverse the matrix if DIR=-1
       IF (DIR.LT.0) CALL INVERT (ROTMAT, ROTMAT, 3, IERR)
C
 999  RETURN
      END
