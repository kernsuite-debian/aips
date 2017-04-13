      SUBROUTINE SIDERL (REFEP, IAT, IATUTC, GMST, GAST)
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
C  Subroutine to calculate Greenwich Sidereal Time for a given UT
C  Inputs:   REFEP          D    reference date
C            IAT            R    IAT in days from reference date
C            IATUTC         R    difference between IAT and UTC
C  Outputs:  GMST           D    Greenwich mean sidereal time (hours)
C            GAST           D    Greenwich apparent sidereal time (hours
C  Comment:  All real variables as double precision
C  Programmer: L.B.Baath  Onsala Space Observatory   29 october 1982
C-----------------------------------------------------------------------
      DOUBLE PRECISION PI, TWOPI, DTR, HTR, T, GMST, ZETA, REFEP,
     *   THETA,EPS, DELPSI, DELEPS, GAST, EQEQ, Z
      DOUBLE PRECISION JD
      REAL   IATUTC, IAT
C-----------------------------------------------------------------------
C                                     get julian date
      JD = REFEP
      JD = JD + IAT + IATUTC/24.D0/3600.D0 - 0.5D0
C                                     program constants
      PI = 3.14159265358979D0
      TWOPI = 2.D0 * PI
      DTR = PI/180.D0
      HTR = PI/12.D0
C                                     Tropical century from jan 0.5, 190
      T = (JD-2415020.D0) / 36525.D0
      GMST = 23925.836D0 + 8640184.542D0*T + 0.0929D0*T*T
      GMST = GMST / 3600.D0
      GMST = MOD (GMST, 24.0D0) + MOD (JD, 1.0D0) * 24.D0
      GMST = MOD (GMST, 24.0D0)
      CALL VBPREC (JD, ZETA, Z, THETA, EPS)
      CALL NUTATE (JD, DELPSI, DELEPS)
      EQEQ = DELPSI*COS(EPS)
      GAST = GMST + EQEQ/HTR
      IF (GMST.GT.24.D0) GMST = GMST - 24.D0
      IF (GMST.LT.0.D0) GMST = GMST + 24.D0
      IF (GAST.GT.24.D0) GAST = GAST - 24.D0
      IF (GAST.LT.0.D0) GAST = GAST + 24.D0
 999  RETURN
      END
