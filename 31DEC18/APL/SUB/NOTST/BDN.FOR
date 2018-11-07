      SUBROUTINE BDN (DATJUL, EQUIN, DNS, DELPSI, SPSI, SEPS, NR)
C-----------------------------------------------------------------------
C! Computes Besselian day numbers of Julian date.
C# Coordinates
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
C  Given are the Julian date of observation and the mean equinox
C  of the position.  The program computes the Besselian day numbers
C  A,B,C,D, and E, total nutation in longitude, DELPSI, short period
C  nutation in longitude, SPSI, and short period nutation in
C  obliquity, SEPS.
C   Inputs:
C      DATJUL   D      Julian date of observation
C      EQUIN    D      mean equinox of the position
C      NR       L      T => NUT4, else NUT2 ???
C   Outputs:
C      DNS      D(*)   Besselian day numbers A,B,C,D, and E
C      DELPSI   D      total nutation in longitude
C      SPSI     D      short period nutation in longitude
C      SEPS     D      short period nutation in obliquity
C-----------------------------------------------------------------------
      DOUBLE PRECISION DATJUL, EQUIN, DNS(*), DELPSI, SPSI, SEPS
      LOGICAL   NR
C
      DOUBLE PRECISION EPS, CK3, T, DJULA, TAU, EPSLON, DELEPS, BESSC,
     *   BESSD
      DATA CK3 /.0027379093D0/
C-----------------------------------------------------------------------
      T = (EQUIN - 1967D0) / 100D0
      DJULA = (EQUIN - 1967D0) * 365.242195D0 + 2439491.541D0
      TAU = CK3 * (DATJUL- DJULA)
      EPSLON = EPS(DATJUL)
      IF (NR) THEN
         CALL NUT4 (DATJUL, DELPSI, DELEPS, SPSI, SEPS)
      ELSE
         CALL NUT2 (DATJUL, DELPSI, DELEPS, SPSI, SEPS)
         END IF
      DNS(1)= (9.7161995D-5 - 4.1209D-8 * T) * TAU + SIN(EPSLON) *
     *   DELPSI
      DNS(2) = -DELEPS
      DNS(5) = DELPSI * (2.226D-3 - 3.73D-4 * T)
      CALL CD (DATJUL, BESSC, BESSD)
      DNS(3) = BESSC / 206264.8062D0
      DNS(4) = BESSD / 206264.8062D0
C
 999  RETURN
      END
