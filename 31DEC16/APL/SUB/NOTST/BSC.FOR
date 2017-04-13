      SUBROUTINE BSC (DATJUL, EQUIN, ALPHA, DELTA, SCRA, SCDEC)
C-----------------------------------------------------------------------
C! Computes Besselian star constants
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
C   Calculation of Besselian star constants.  A mean equinox and a mean
C   position are given.  The Besselian star constants a,b,c,d, a',b',c'
C   and d' are computed.
C-----------------------------------------------------------------------
      DOUBLE PRECISION DATJUL, EQUIN, ALPHA, DELTA, SCRA(*), SCDEC(*)
      DOUBLE PRECISION EPS, T, SINDEL, COSDEL, SECDEL, TANDEL,
     *   SINALF, COSALF,  M, N, TEPS
C-----------------------------------------------------------------------
      T = (EQUIN - 1900.D0) / 100.D0
      T = T * 365.25D0 / 365.24219897D0
      SINDEL = SIN(DELTA)
      COSDEL = COS(DELTA)
      SECDEL = 1.D0 / COSDEL
      TANDEL = SINDEL / COSDEL
      SINALF = SIN(ALPHA)
      COSALF = COS(ALPHA)
      M = 46.0851D0 + .0279D0*T
      N = 20.0468D0 - .0085D0*T
      TEPS = TAN(EPS(DATJUL))
      SCRA(1) = M / N + SINALF * TANDEL
      SCRA(2) = COSALF * TANDEL
      SCRA(3) = COSALF * SECDEL
      SCRA(4) = SINALF * SECDEL
      SCDEC(1) = COSALF
      SCDEC(2) = -SINALF
      SCDEC(3) = TEPS * COSDEL - SINALF * SINDEL
      SCDEC(4) = COSALF * SINDEL
C
 999  RETURN
      END
