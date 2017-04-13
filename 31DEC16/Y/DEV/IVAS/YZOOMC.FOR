      SUBROUTINE YZOOMC (MAG, XZOOM, YZOOM, VRTRTC, IERR)
C-----------------------------------------------------------------------
C! set the TV zoom magnification and center
C# TV-basic
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
C   Version for the IVAS from IIS
C-----------------------------------------------------------------------
      INTEGER   MAG, XZOOM, YZOOM, IERR
      LOGICAL   VRTRTC
C
      INTEGER   JX, JY, MAGFAC, FIVASVPZOOMSCROLL, JERR, JXG, JYG
      REAL      TMAG
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((MAG.LT.0) .OR. (MAG.GT.IABS(MXZOOM))) GO TO 999
      IF ((XZOOM.LT.1) .OR. (XZOOM.GT.MAXXTV(1))) GO TO 999
      IF ((YZOOM.LT.1) .OR. (YZOOM.GT.MAXXTV(2))) GO TO 999
C                                       vertical retrace
      CALL YVRTR (VRTRTC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Scroll equivalent
      MAGFAC = 1 + MAG
      IF (MXZOOM.GT.0) MAGFAC = 2 ** MAG
      TMAG = MAGFAC
      JX = (XZOOM-1.0) * (TMAG-1.0) / TMAG
      JY = (MAXXTV(2)-YZOOM) * (TMAG-1.0) / TMAG
      JX = JX - TVSCRX(1)
      JY = JY + TVSCRY(1)
      IF (JX.LT.0) JX = JX + MAXXTV(1)
      IF (JX.GE.MAXXTV(1)) JX = JX - MAXXTV(1)
      IF (JY.LT.0) JY = JY + MAXXTV(2)
      IF (JY.GE.MAXXTV(2)) JY = JY - MAXXTV(2)
      JXG = JX
      JYG = JY
C                                       graphics off of image?
      IF ((NGRAY.EQ.2) .AND. ((SYSNAM(:5).EQ.'NRAO1') .OR.
     *   (SYSNAM(:5).EQ.'nrao1'))) THEN
         IF (MAGFAC.EQ.2) JXG = JXG + 1
         IF (MAGFAC.GT.2) JXG = JXG + 2
         IF (JXG.LT.0) JXG = JXG + MAXXTV(1)
         IF (JXG.GE.MAXXTV(1)) JXG = JXG - MAXXTV(1)
         END IF
C                                       Now do IO
      JERR = FIVASVPZOOMSCROLL (JX, JY, MAGFAC, MAGFAC, 1)
      IF (JERR.EQ.0) JERR = FIVASVPZOOMSCROLL (JXG, JYG, MAGFAC, MAGFAC,
     *   2)
C                                       update common /TVCHAR/
      IF (JERR.EQ.0) THEN
         TVZOOM(1) = MAG
         TVZOOM(2) = XZOOM
         TVZOOM(3) = YZOOM
C                                       Error
      ELSE
         CALL YDOERR ('YZOOMC', JERR, IERR)
         END IF
C
 999  RETURN
      END
