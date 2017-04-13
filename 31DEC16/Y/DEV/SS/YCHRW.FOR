      SUBROUTINE YCHRW (CHAN, X, Y, STRING, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! writes characters into image and graphics planes
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1996
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
C   writes characters into image planes of the TV.  The format is 5 x 7
C   or a multiple thereof with one or more blanks all around.  The net
C   is set to match CSIZTV if possible.  We recommend CSIZTV = 7, 9 for
C   TVs of size around 512 square and CSIZTV = 14, 22 or so for TVs of
C   size around 1024 square.  This version will work on all TVs which
C   allow horizontal writing to the right.  It is a Y routine to allow
C   for hardware character generators on the TV.
C   Inputs:
C      CHAN    I       channel select  (1 to NGRAY + NGRAPH)
C      X       I       X position lower left corner first char.
C      Y       I       Y position lower left corner first char.
C      STRING  C*(*)   character string - length passed from Fortran
C   Output:
C      SCRTCH  I(>)    scratch buffer (dim = 14*count+8 < MAXXTV(1))
C      IERR    I       error code of Z...XF:0 - ok
C                                           2 - input error
C   XAS version - let XAS do it
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
      INTEGER   CHAN, X, Y, SCRTCH(*), IERR
C
      INTEGER   COUNT, LX, LY, OPCODE, DAT(4), NWSD
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        check input
      IERR = 2
      COUNT = LEN (STRING)
      LX = X + COUNT*CSIZTV(1) - 1
      LY = Y + CSIZTV(2) - 1
      IF ((Y.LT.1) .OR. (LY.GT.MAXXTV(2))) GO TO 990
      IF ((X.LT.1) .OR. (LX.GT.MAXXTV(1))) GO TO 990
      IF (COUNT.LE.0) GO TO 990
C                                       do it
      OPCODE = 26
      DAT(1) = X
      DAT(2) = Y
      DAT(3) = CHAN
      DAT(4) = COUNT
      CALL ZCLC8 (COUNT, STRING, 1, SCRTCH)
      CALL ZSSSXF (OPCODE, DAT, COUNT, SCRTCH, NWSD, IERR)
      GO TO 999
C
 990  WRITE (MSGTXT,1990) X, LX, Y, LY, COUNT
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('YCHRW: USING CHAR SIZE',2I3,' NOT INPUT SIZE',2I3)
 1990 FORMAT ('YCHRW: BAD X,LX, Y,LY, COUNT =',5I7)
      END
