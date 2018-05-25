      SUBROUTINE LINTER (NP, IP, INB, OUTB)
C-----------------------------------------------------------------------
C! does linear interpolation of a 1-D INTEGER array
C# Utility
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
C   LINTER interpolates an INTEGER array.
C   Inputs:
C      NP    I      Number pixels in input array
C      IP    I      Interpolation factor (1 => copy)
C      INB   I(*)   In buffer
C   Output:
C      OUTB  I(*)   Out buffer (# pts = (NP-1)*IP +1 )
C-----------------------------------------------------------------------
      INTEGER   NP, IP, INB(*), OUTB(*)
C
      INTEGER   I, J, L
      REAL      X, Y
C-----------------------------------------------------------------------
      IF (NP.LE.0) GO TO 999
C                                       Null interpolation
      IF (IP.LT.2) THEN
         DO 10 I = 1,NP
            OUTB(I) = INB(I)
 10         CONTINUE
C                                       Real interpolation
      ELSE
         J = 1
         X = 1.0 / IP
         DO 30 I = 1,NP
            OUTB(J) = INB(I)
            IF (I.EQ.NP) GO TO 999
            J = J + 1
            Y = X * (INB(I+1) - INB(I))
            DO 20 L = 2,IP
               OUTB(J) = INB(I) + (L-1) * Y + 0.4999
               J = J + 1
 20            CONTINUE
 30         CONTINUE
         END IF
C
 999  RETURN
      END
