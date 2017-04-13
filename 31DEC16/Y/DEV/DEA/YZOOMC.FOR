      SUBROUTINE YZOOMC (MAG, XZOOM, YZOOM, VRTRTC, IERR)
C-----------------------------------------------------------------------
C! set the TV zoom magnification and center
C# Y1 TV-basic
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
C   YZOOM writes (ONLY!!!!) the zoom control registers of the TV
C   Inputs:
C      MAG     I     0-3 for magnification 1,2,4,8 times, resp.
C      XZOOM   I     X center of expansion (1-512, 1 => LHS)
C      YZOOM   I     Y center of expansion (1-512, 1 => bot)
C      VRTRTC  L     Do on vertical retrace only?
C   Output:
C      IERR    I     error code of Z...XF: 0 -> ok, 2 -> input error
C   YZOOMC updates the /TVDEV/ common TVZOOM parameter
C   Modified NOV-83 by Jaffe for DeAnza code
C-----------------------------------------------------------------------
      INTEGER   MAG, XZOOM, YZOOM, IERR
      LOGICAL   VRTRTC
C
      INCLUDE 'INCS:YDEA.INC'
      INTEGER   SCMASK, IMAG, JMAG, IX, IY, ZREG(2), SHIFT, ITEMP, I,
     *   KOFF, ISCX, ISCY, ITEMP2, ZAND
      LOGICAL   WAIT, UPDATE
      INCLUDE 'INCS:DTVC.INC'
      DATA WAIT /.TRUE./
C     DATA SHIFT, SCMASK /Z0020, Z03FF/
      DATA SHIFT, SCMASK /32,    1023/
C-----------------------------------------------------------------------
      IERR = 2
      IF ((MAG.LT.0) .OR. (MAG.GT.IABS(MXZOOM))) GO TO 999
      IF ((XZOOM.LT.1) .OR. (XZOOM.GT.MAXXTV(1))) GO TO 999
      IF ((YZOOM.LT.1) .OR. (YZOOM.GT.MAXXTV(2))) GO TO 999
C                                       set zoom factor
      IMAG = MAG + 1
      IF (MXZOOM.GT.0) IMAG = 2 ** MAG
      JMAG = SHIFT * (IMAG - 1)
C                                       Put zoom in upper byte
      DO 10 I = 1,2
         ZREG(I) = 0
         CALL ZPUTCH (JMAG, ZREG(I), 2)
 10      CONTINUE
C                                       calculate scroll equivalent
      IX = REAL(XZOOM-1) * REAL(IMAG-1) /REAL(IMAG)
      IY = (REAL(YZOOM-1) * REAL(IMAG-1) + REAL(MAXXTV(2) - 1))
     *   / REAL(IMAG)
C                                       Loop over channels
      IERR = 0
      ITEMP2 = NGRAY + 1
      DO 20 I = 1,ITEMP2
         UPDATE = .FALSE.
         KOFF = 4 * (I-1)
         ITEMP = IX - TVSCRX(I)
         ISCX = ZREG(1) + ZAND (ITEMP, SCMASK)
         ITEMP = IY - TVSCRY(I)
         ISCY = ZREG(2) + ZAND (ITEMP, SCMASK)
C                                       Do I need to change anything?
         CALL YTCOMP (YBUFF(KOFF + XSCZ), ISCX, UPDATE)
         CALL YTCOMP (YBUFF(KOFF + YSCZ), ISCY, UPDATE)
C                                       If so, do it.
         IF (UPDATE) THEN
            ITEMP = I - 1
            CALL ZDEAXF ('LMC ', YBUFF(KOFF+MEMREG), MRBYTE, ITEMP, 0,
     *         WAIT, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 20      CONTINUE
C                                       update common /TVCHAR/
      TVZOOM(1) = MAG
      TVZOOM(2) = XZOOM
      TVZOOM(3) = YZOOM
C
 999  RETURN
      END
