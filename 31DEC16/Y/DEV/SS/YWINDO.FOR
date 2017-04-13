      SUBROUTINE YWINDO (OPER, WIND, IERR)
C-----------------------------------------------------------------------
C! set the current window (viewport) into the TV memory
C# Y1 TV-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   YWINDO reads the current viewport into the TV memory.  It is hoped
C   that someday we will also offer writing to force the size.
C   Inputs:
C      OPER   C*4    'READ', 'WRIT'
C   In/out:
C      WIND   I(4)   BLC x,y, TRC x,y of window in TV pixels
C                    In: desired window ('WRIT' only)
C                    Out: actual window given
C   Output:
C      IERR    I     error code of Z...XF: 0 -> ok, 2 -> input error
C                    1 -> bad window returned, 11 -> comm. failure
C   SUN Screen Server TV version.
C-----------------------------------------------------------------------
      CHARACTER OPER*(*)
      INTEGER   WIND(4), IERR
C
      INTEGER   OPCODE, DAT(4), NWSD, TMPBUF(4), SCRTCH(256)
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF (OPER.EQ.'WRIT') THEN
         IF (WIND(1).GE.WIND(3)) GO TO 999
         IF (WIND(2).GE.WIND(4)) GO TO 999
         IF ((WIND(1).LT.1) .OR. (WIND(1).GT.MAXXTV(1))) GO TO 999
         IF ((WIND(2).LT.1) .OR. (WIND(2).GT.MAXXTV(2))) GO TO 999
         IF ((WIND(3).LT.1) .OR. (WIND(3).GT.MAXXTV(1))) GO TO 999
         IF ((WIND(4).LT.1) .OR. (WIND(4).GT.MAXXTV(2))) GO TO 999
         CALL COPY (4, WIND, DAT)
      ELSE IF (OPER.EQ.'READ') THEN
         CALL FILL (4, 0, DAT)
      ELSE
         GO TO 999
         END IF
C                                       Do it now
      OPCODE = 14
      CALL ZSSSXF (OPCODE, DAT, 0, TMPBUF, NWSD, IERR)
      IF ((NWSD.NE.8) .AND. (IERR.EQ.0)) IERR = 11
      IF (IERR.NE.0) GO TO 999
C                                       returned corners
      CALL ZI16IL (4, 1, TMPBUF, WIND)
      IF (WIND(1).GE.WIND(3)) IERR = 1
      IF (WIND(2).GE.WIND(4)) IERR = 1
      IF ((WIND(1).LT.1) .OR. (WIND(1).GT.MAXXTV(1))) IERR = 1
      IF ((WIND(2).LT.1) .OR. (WIND(2).GT.MAXXTV(2))) IERR = 1
      IF ((WIND(3).LT.1) .OR. (WIND(3).GT.MAXXTV(1))) IERR = 1
      IF ((WIND(4).LT.1) .OR. (WIND(4).GT.MAXXTV(2))) IERR = 1
      IF (IERR.NE.0) THEN
         MSGTXT = 'Window size error: interogating parms'
         CALL MSGWRT (6)
C                                       Now ask it for parameter common
         CALL YDTVC ('GET ', SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'Get parameters from XAS'
            CALL MSGWRT (7)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' on ',A)
      END
