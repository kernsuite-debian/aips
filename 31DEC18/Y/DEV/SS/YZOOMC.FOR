      SUBROUTINE YZOOMC (MAG, XZOOM, YZOOM, VRTRTC, IERR)
C-----------------------------------------------------------------------
C! set the TV zoom magnification and center
C# TV-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2008
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
C   YZOOM writes (ONLY!!!!) the zoom control registers of the TV
C   Inputs:
C      MAG     I     0-3 for magnification 1,2,4,8 times, resp.
C      XZOOM   I     X center of expansion (1-512, 1 => LHS)
C      YZOOM   I     Y center of expansion (1-512, 1 => bot)
C      VRTRTC  L     Do on vertical retrace only?
C   Output:
C      IERR    I     error code of Z...XF: 0 -> ok, 2 -> input error
C   YZOOMC updates the /TVDEV/ common TVZOOM parameter
C   SUN Screen Server Version
C-----------------------------------------------------------------------
      INTEGER   MAG, XZOOM, YZOOM, IERR
      LOGICAL   VRTRTC
C
      INTEGER   OPCODE, DAT(4), BUF, NWSD, MAGFAC, JX, JY, I, J
      REAL      TMAG
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA OPCODE /83/
C-----------------------------------------------------------------------
      IERR = 2
      IF ((MAG.LT.0) .OR. (MAG.GT.IABS(MXZOOM))) GO TO 999
      IF ((XZOOM.LT.-MAXXTV(1)) .OR. (XZOOM.GT.2*MAXXTV(1))) GO TO 999
      IF ((YZOOM.LT.-MAXXTV(2)) .OR. (YZOOM.GT.2*MAXXTV(2))) GO TO 999
C                                        Compute the upper left
      MAGFAC = 1 + MAG
      IF (MXZOOM.GT.0) MAGFAC = 2 ** MAG
      TMAG = MAGFAC
      JX = (XZOOM-1.0) * (TMAG-1.0) / TMAG
      JY = (MAXXTV(2)-YZOOM) * (TMAG-1.0) / TMAG
C                                        set data
      J = NGRAY + 1
      DO 10 I = 1,J
         DAT(1) = I
         DAT(2) = MAGFAC
         IF (I.LE.NGRAY) THEN
            DAT(3) = JX - TVSCRX(I)
            DAT(4) = JY + TVSCRY(I)
         ELSE
            DAT(3) = JX - TVSCGX
            DAT(4) = JY + TVSCGY
            END IF
         IF (DAT(3).LT.0) DAT(3) = DAT(3) + MAXXTV(1)
         IF (DAT(3).GE.MAXXTV(1)) DAT(3) = DAT(3) - MAXXTV(1)
         IF (DAT(4).LT.0) DAT(4) = DAT(4) + MAXXTV(2)
         IF (DAT(4).GE.MAXXTV(2)) DAT(4) = DAT(4) - MAXXTV(2)
         CALL ZSSSXF (OPCODE, DAT, 0, BUF, NWSD, IERR)
         IF (IERR.NE.0) GO TO 900
 10      CONTINUE
C                                       update common /TVCHAR/
      TVZOOM(1) = MAG
      TVZOOM(2) = XZOOM
      TVZOOM(3) = YZOOM
      GO TO 999
C
 900  WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('YZOOMC - ERROR',I5,' RETURNED BY ZSSSXF')
      END
