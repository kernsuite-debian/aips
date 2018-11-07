      SUBROUTINE TKCHAR (INCHAR, IANGL, DCX, DCY, TEXT, IERR)
C-----------------------------------------------------------------------
C! writes characters to a TK graphics device
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
C   TKCHAR will write characters to a TEKTRONIX 4012.
C   Inputs:
C      INCHAR  I       number of characters.
C      IANGL   I       0=horizontal, other = vertical.
C      DCX     R       X distance in characters from current position.
C      DCY     R       Y distance in characters from current position.
C      TEXT    C*(*)   packed characters.
C  Outputs:
C      IERR    I       error indicator. 0 = ok.
C-----------------------------------------------------------------------
      REAL      DCX, DCY
      INTEGER   INCHAR, IANGL, IERR
      CHARACTER TEXT*(*)
C
      REAL      X, Y
      INTEGER   IERRC, IERRL, LT, I, IT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTKS.INC'
      DATA IERRL /50/
C-----------------------------------------------------------------------
      IERR = 0
      IF (INCHAR.LE.0) GO TO 999
      IERRC = 0
      X = RXL + DCX * CSIZTK(1)
      Y = RYL + DCY * CSIZTK(2)
      CALL TEKVEC (X, Y, 3, IERR)
      IERRC = IERRC + IERR
      IF (IERRC.GT.IERRL) GO TO 980
C                                       Alpha mode.
      IT = 31
      CALL ZTKBUF (IT, 1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Loop for each character.
      DO 10 I = 1,INCHAR
         LT = ICHAR (TEXT(I:I))
         CALL ZTKBUF (LT, 3, IERR)
         IERRC = IERRC + IERR
         IF (IERRC.GT.IERRL) GO TO 980
C                                       Vertical, reposition.
         IF (IANGL.EQ.0) GO TO 10
            X = RXL
            Y = RYL - CSIZTK(2)
            CALL TEKVEC (X, Y, 3, IERR)
            IERRC = IERRC + IERR
            IF (IERRC.GT.IERRL) GO TO 980
            CALL ZTKBUF (IT, 1, IERR)
            IERRC = IERRC + IERR
            IF (IERRC.GT.IERRL) GO TO 980
 10      CONTINUE
      IERR = 0
      GO TO 999
C                                       Error limit exceeded.
 980  WRITE (MSGTXT,1980)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('ERROR LIMIT EXCEEDED.')
      END
