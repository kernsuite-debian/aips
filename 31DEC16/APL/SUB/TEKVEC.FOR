      SUBROUTINE TEKVEC (XX, YY, IN, IERR)
C-----------------------------------------------------------------------
C! write bright or dark, scaled or unscaled vector to TK graphics device
C# Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   This routine will put control characters, and X and Y coordinates
C   into the TEKTRONIX output buffer.
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
C-----------------------------------------------------------------------
      REAL      XX, YY
      INTEGER   IN, IERR
C
      INTEGER   X, Y, JERR
      LOGICAL   ISERR, LSTERR
      REAL      BLC(2), TRC(2), AX(2), AY(2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTKS.INC'
C-----------------------------------------------------------------------
C                                       Set last X and Y.
      BLC(1) = 1.0
      BLC(2) = 1.0
      TRC(1) = MAXXTK(1)
      TRC(2) = MAXXTK(2)
      AX(2) = XX
      AY(2) = YY
      AX(1) = RXL
      AY(1) = RYL
C                                       If IN = 1 or 2 do scaling.
      IF (IN.GT.2) GO TO 10
         AX(2) = XX*SCALEX + RX0
         AY(2) = YY*SCALEY + RY0
C                                       Save requested
 10   LSTERR = (RXL.LT.0.5) .OR. (RYL.LT.0.5) .OR.
     *   (RXL.GT.TRC(1)) .OR. (RYL.GT.TRC(2))
      RXL = AX(2)
      RYL = AY(2)
      ISERR = (RXL.LT.0.5) .OR. (RYL.LT.0.5) .OR.
     *   (RXL.GT.TRC(1)) .OR. (RYL.GT.TRC(2))
C                                       Interpolate at edges
      CALL LINLIM (BLC, TRC, AX, AY, JERR)
      IF (JERR.NE.0) GO TO 90
C                                       If IN = 1 or 3 'dark vector'.
      IF ((IN.EQ.1) .OR. (IN.EQ.3)) THEN
         X = AX(2) + 0.5
         Y = AY(2) + 0.5
         CALL TKDVEC (1, X, Y, IERR)
C                                       Bright vector
      ELSE
C                                       Check start point
         IF (LSTERR) THEN
            X = AX(1) + 0.5
            Y = AY(1) + 0.5
            CALL TKDVEC (1, X, Y, IERR)
            IF (IERR.NE.0) GO TO 90
            END IF
C                                       Draw end point
         X = AX(2) + 0.5
         Y = AY(2) + 0.5
         CALL TKDVEC (2, X, Y, IERR)
         END IF
C                                         Save new pos
 90   IF ((IERR.EQ.0) .AND. (ISERR)) IERR = 2
C
 999  RETURN
      END
