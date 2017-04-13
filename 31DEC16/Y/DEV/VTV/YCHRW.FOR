      SUBROUTINE YCHRW (CHAN, X, Y, STRING, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! writes characters into image and graphics planes
C# Y0 TV-util
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
C   YCHRW  writes characters into image planes of the TV.  The format is
C   5 by 7 with one blank all around: net 7 in X by 9 in Y. This version
C   will work on all TVs which allow horizontal writing to the right. It
C   is a Y routine to allow for hardware character generators on the TV.
C   Inputs:
C      CHAN    I       channel select  (1 to NGRAY + NGRAPH)
C      X       I       X position lower left corner first char.
C      Y       I       Y position lower left corner first char.
C      STRING  C*(*)   character string - length passed from Fortran
C   Output:
C      SCRTCH  I(>)    scratch buffer (dim = 14*count+8 < 1031)
C      IERR    I       error code of Z...XF:0 - ok
C                                           2 - input error
C   Virtual TV version is for TV by communication.
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
      INTEGER   CHAN, X, Y, SCRTCH(*), IERR
C
      REAL      XT(50)
      INTEGER   COUNT, IXT(50), I, LX, LY
      INCLUDE 'INCS:DVTV.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (XT, IXT)
C-----------------------------------------------------------------------
C                                        check input
      IERR = 2
      COUNT = LEN (STRING)
      LX = X + COUNT*CSIZTV(1) - 1
      LY = Y + CSIZTV(2) - 1
      IF ((Y.LT.1) .OR. (LY.GT.MAXXTV(2))) GO TO 900
      IF ((X.LT.1) .OR. (LX.GT.MAXXTV(1))) GO TO 900
      IF (COUNT.LE.0) GO TO 900
C                                       set up parms
      BUFFER(1) = CHAN
      BUFFER(2) = X
      BUFFER(3) = Y
      BUFFER(4) = COUNT
C                                       do string
      CALL ZCLC8 (COUNT, STRING, 1, XT)
      I = (COUNT - 1) / 2 + 1
      CALL ZI16IL (I, 1, IXT, BUFFER(5))
      BUFSW = 4 + I
      BUFSR = 0
      CALL MKYBUF ('WRIT', 'YCHRW ', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
      GO TO 999
C
 900  WRITE (MSGTXT,1900) X, LX, Y, LY, COUNT
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('YCHRW: BAD X,LX, Y,LY, COUNT =',5I7)
      END
