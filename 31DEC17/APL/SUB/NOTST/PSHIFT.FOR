      SUBROUTINE PSHIFT (DELTAX, POLYN, DEGREE)
C-----------------------------------------------------------------------
C! Shifts the origin of a polynomial function by delta-x
C# EXT-appl VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-1998
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
C   Given the coefficients of a polynomial function of x in POLYN
C   such that the function has the value sum(POLYN(i) * x**i), shift
C   the origin of the polynomial to DELTAX.
C
C   Inputs:
C      DELTAX   D              Shift in origin
C      DEGREE   I              Degree of polynomial
C
C   Outputs:
C      POLYN    D(0:DEGREE)    Polynomial coefficients
C
C   DEGREE must lie in the range 0 to 16 inclusive; the upper bound
C   may be increased by changing the parameter MAXPLY and recompiling.
C
C   Algorithm from M. Shaw and J.F. Traub, JACM 21, 161-167 (1974)
C   reported in D.E. Knuth, "The Art of Computer Programming", Vol 2
C   (3rd edn.) p489.
C-----------------------------------------------------------------------
      INTEGER          DEGREE
      DOUBLE PRECISION DELTAX, POLYN(0:DEGREE)
C
C     Local variables:
C
C     MAXPLY    Maximum degree of polynomial
C     POWER     Precomputed powers of DELTAX
C     J         Loop counter
C     I         Loop counter
C
      INTEGER   MAXPLY
      PARAMETER (MAXPLY = 16)
      DOUBLE PRECISION POWER(0:MAXPLY)
      INTEGER   J, K
C-----------------------------------------------------------------------
C
C     Precompute powers of DELTAX (Step S1 in Knuth with x0 = DELTAX):
C
      POWER(0) = 1.0D0
      DO 10 J = 1, DEGREE
         POWER(J) = DELTAX * POWER(J - 1)
   10 CONTINUE
C
C     Set POLYN(j) to POLYN(j) * -DELTAX**j (Step S2 in Knuth):
C
      DO 20 J = 0, DEGREE
         POLYN(J) = POLYN(J) * POWER(J)
   20 CONTINUE
C
C     Compute polynomial of x0(x+1) (Step S3 in Knuth):
C
      DO 40 K = 0, DEGREE
         DO 30 J = DEGREE - 1, K, -1
            POLYN(J) = POLYN(J) + POLYN(J + 1)
   30    CONTINUE
   40 CONTINUE
C
C     Divide through by x0**j (Step S4 in Knuth):
C
      DO 50 J = 0, DEGREE
         POLYN(J) = POLYN(J) / POWER(J)
   50 CONTINUE
C
      END
