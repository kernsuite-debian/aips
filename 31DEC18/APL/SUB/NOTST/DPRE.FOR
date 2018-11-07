      SUBROUTINE DPRE (EQUIN0, EQUIN, PREMAT)
C-----------------------------------------------------------------------
C! Compute General precession matrix
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
C   Calculation of the matrix of general precession.
C   the matrix is computed which corresponds to the general
C   precession of equatorial coordinates from one mean equinox to
C   another.
C    Inputs:
C      EQUIN0       D  Initial mean equinox
C      EQUIN        D  Final mean equinox
C    Output:
C      PREMAT(3,3)  D  Precession matrix
C-----------------------------------------------------------------------
      DOUBLE PRECISION EQUIN0, EQUIN, PREMAT(3,3)
      DOUBLE PRECISION CSAR, T0, T, ZETA, ZETT, THET, SZETA,
     *   CZETA, SZETT, CZETT, STHET, CTHET, A, B, D, C
C-----------------------------------------------------------------------
      CSAR = ATAN (1.0D0) / 162000.0D0
      T0 = (EQUIN0-1900.0D0) / 100.0D0
      T = (EQUIN-EQUIN0) / 100.0D0
      ZETA = (((.0179D0*T+.302D0)*T
     *   +(2304.25D0+1.396D0*T0))*T)*CSAR
      ZETT = ZETA+T*(T*(.791D0+1D-3*T))*CSAR
      THET = (((-.042D0*T-.426D0)*T
     *   +(2004.682D0-.853D0*T0))*T)*CSAR
      SZETA = SIN (ZETA)
      CZETA = COS (ZETA)
      SZETT = SIN (ZETT)
      CZETT = COS (ZETT)
      STHET = SIN (THET)
      CTHET = COS (THET)
      A = SZETA*SZETT
      B = CZETA*SZETT
      C = SZETA*CZETT
      D = CZETA*CZETT
      PREMAT(1,1) = D*CTHET-A
      PREMAT(1,2) = -C*CTHET-B
      PREMAT(1,3) = -STHET*CZETT
      PREMAT(2,1) = B*CTHET+C
      PREMAT(2,2) = -A*CTHET+D
      PREMAT(2,3) = -STHET*SZETT
      PREMAT(3,1) = CZETA*STHET
      PREMAT(3,2) = -SZETA*STHET
      PREMAT(3,3) = CTHET
C
 999  RETURN
      END
