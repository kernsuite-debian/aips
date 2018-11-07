      SUBROUTINE BSCONV (NX, THROWP, THROWM, SCALE, IC, CONV)
C-----------------------------------------------------------------------
C! convolution function for centering a beam-switched image
C# Map-util singledish
C-----------------------------------------------------------------------
C;  Copyright (C) 1996, 1997
C;  Associated Universities, Inc. Washington DC, USA.
C;  Eric W, Greisen
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
C   Computes a convolution function to use for combining the two images
C   Inputs:
C      NX       I      Number X points on row
C      THROWP   R      Throw of "plus" image in cells
C      THROWM   R      Throw of "minus" image in cells
C      SCALE    R      Scale plus image wrt minus image
C      IC       I      Index in CONV for x = 0
C   Outputs:
C      CONV     R(*)   Convolution function
C   Restricted to throws that are integer multiples of AXV for now.
C-----------------------------------------------------------------------
      INTEGER   NX, IC
      REAL      THROWP, THROWM, SCALE, CONV(*)
C
      INTEGER   IP, J, IX, N
      REAL      V, VS, X, S, Y, XK, XF
      DOUBLE PRECISION DY
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      CALL RFILL (2*NX, 0.0, CONV)
C                                       get throws, scale
      VS = SCALE
      IF (VS.LE.0.0) VS = 1.0
      S = ABS (THROWM - THROWP)
      IF (THROWM.GT.THROWP) THEN
         XF = 0.5
      ELSE
         XF = -0.5
         END IF
      N = NX / S + 1
      IC = NX + 1
      IP = 2*NX+1
      DO 20 IX = 1,IP
         CONV(IX) = 0.0
         X = IX - IC
         DO 10 J = -N,N+1
            XK = 2*J - 1
            DY = (X - XK * S / 2.0) * PI
            IF (ABS(DY).LT.1.D-7) THEN
               Y = 1.0
            ELSE
               Y = SIN (DY) / DY
               END IF
            CONV(IX) = CONV(IX) + Y * XF * SIGN (1.0, XK)
 10         CONTINUE
 20      CONTINUE
C                                       scale
      IF (VS.NE.1.0) THEN
         DO 30 IX = 1,NX
            V = VS ** (IX/S)
            CONV(IC+IX) = CONV(IC+IX) * V
            CONV(IC-IX) = CONV(IC-IX) / V
 30         CONTINUE
         END IF
C
 999  RETURN
      END
