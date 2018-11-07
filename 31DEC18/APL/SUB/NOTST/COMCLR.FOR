      SUBROUTINE COMCLR (COMAR, AMPMAX, N, IARRAY)
C-----------------------------------------------------------------------
C! Scale and map complex array intp RBG space, amp=inten. phase=hue.
C# Map Plot-appl
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
C   Take a complex array, scale it, and map it into RGB space for
C   plotting as AMP = Intensity; PHASE -> Hue; SAT = 100%
C   Inputs: COMAR(N)      C          Input complex array
C           AMPMAX        R          Maximum amplitude in input array
C           N             I          # complex elements in COMAR
C           IARRAY(512,3) I          Output array , RGB colors
C-----------------------------------------------------------------------
      INTEGER   N, IARRAY(512,3)
      COMPLEX   COMAR(N)
      REAL      AMPMAX
      INTEGER   NM, I, ICOLOR
      REAL      COLX(3), COLY(3), COLZ(3), CABS, AMPINV, OFF, A, X, Y, Z
      INCLUDE 'INCS:DTVC.INC'
      DATA COLX / 2.580  , -1.090,  0.125 /,
     *     COLY /-1.150  ,  2.040, -0.295 /,
     *     COLZ /-0.422  ,  0.058,  1.170 /
C-----------------------------------------------------------------------
      NM = MIN (N, 512)
      AMPINV = 1. / (1.16 * AMPMAX)
      OFF = 0.16 / 1.16
      DO 20 I = 1,NM
         A = CABS(COMAR(I))
         Z = A * AMPINV + OFF
         Y = Z ** 3
         X = (Z + 0.2 * REAL(COMAR(I)) / A)
         Z = (Z - .5 * AIMAG(COMAR(I)) / A) ** 3
         DO 10 ICOLOR = 1,3
            Z = X * COLX(ICOLOR) + Y * COLY(ICOLOR) + Z * COLZ(ICOLOR)
            Z = MAX (0.0, MIN (1.0, Z))
            IARRAY(I, ICOLOR) = MAXINT * Z**2 + .01
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
