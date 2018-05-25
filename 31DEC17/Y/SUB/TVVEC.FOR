      SUBROUTINE TVVEC (XX, YY, IN, IERR)
C-----------------------------------------------------------------------
C! write bright or dark, scaled or unscaled vector to TV graphics device
C# Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 2000, 2005
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
C   This routine will plot lines on the TV.
C   Inputs:
C      XX    I   X coordinate value.
C      YY    I   Y coordinate value.
C      IN    I   control value:
C                1 = Scale XX and YY and preceed coordinates
C                    by 'write dark vector' control character
C                2 = Scale XX and YY, put in buffer.
C                3 = XX and YY are not scaled, 'write dark vector'
C                    control character is put into the buffer.
C                4 = no scale, write vector
C      FIND  I   FTAB position of TEKTRONIX device.
C   Output:
C      IERR  I   error code, 0=ok, 1=write error.
C   Common:
C      /TKSPCL/  in/out   TKBUFF, TKPOS, RXL, RYL
C   NOTE: dark vectors are just a positioning command
C-----------------------------------------------------------------------
      REAL      XX, YY
      INTEGER   IN, IERR
C
      INTEGER   JERR, IX(2), IY(2)
      REAL      BLC(2), TRC(2), AX(2), AY(2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVS.INC'
C-----------------------------------------------------------------------
C                                       Set last X and Y.
      IERR = 0
      BLC(1) = WINDTV(1)
      BLC(2) = WINDTV(2)
      TRC(1) = WINDTV(3)
      TRC(2) = WINDTV(4)
      AX(1) = RXL
      AY(1) = RYL
C                                       If IN = 1 or 2 do scaling.
      IF (IN.LE.2) THEN
         AX(2) = XX*SCALEX + RX0
         AY(2) = YY*SCALEY + RY0
      ELSE
         AX(2) = XX
         AY(2) = YY
         END IF
C                                       Save requested
 10   RXL = AX(2)
      RYL = AY(2)
C                                       Only bright vectors do anything
      IF ((IN.EQ.2) .OR. (IN.EQ.4)) THEN
C                                       Interpolate at edges
         CALL LINLIM (BLC, TRC, AX, AY, JERR)
         IF (JERR.EQ.0) THEN
            IX(1) = AX(1) + 0.5
            IX(2) = AX(2) + 0.5
            IY(1) = AY(1) + 0.5
            IY(2) = AY(2) + 0.5
            CALL IMVECT ('ONNN', IGR, 2, IX, IY, INBUF, IERR)
            END IF
         END IF
C
 999  RETURN
      END
