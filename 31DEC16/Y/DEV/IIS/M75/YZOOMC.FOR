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
C   Version for IIS Model 75
C-----------------------------------------------------------------------
      INTEGER   MAG, XZOOM, YZOOM, IERR
      LOGICAL   VRTRTC
C
      INTEGER   UNIT, HEADER(8), BUFFER(2), BUFSIZ, NB, MAGSFT, MAGPOW,
     *   ZMASK, TMASK, ICHAN
      LOGICAL   COMAND
      INCLUDE 'INCS:DTVC.INC'
      DATA UNIT, COMAND, BUFSIZ / 6, .FALSE., 2/
C-----------------------------------------------------------------------
      IERR = 2
      IF ((MAG.LT.0) .OR. (MAG.GT.MXZOOM)) GO TO 999
      IF ((XZOOM.LT.1) .OR. (XZOOM.GT.MAXXTV(1))) GO TO 999
      IF ((YZOOM.LT.1) .OR. (YZOOM.GT.MAXXTV(2))) GO TO 999
C                                       TMASK = 1201 hex
      TMASK = 4609
C                                       Set data.  Find the postion of
C                                       the upper left corner of the
C                                       zoom window
      MAGPOW = 2 ** MAG
C                                       Old IIS version
C      BUFFER(1) = MAX0 (0, ((XZOOM - 1) - (256 / MAGPOW)))
C      BUFFER(2) = MAX0 (0, ((MAXXTV(2) - YZOOM) - (256 / MAGPOW)))
C      BUFFER(1) = MIN0 (512 - (512 / MAGPOW), BUFFER(1))
C      BUFFER(2) = MIN0 (512 - (512 / MAGPOW), BUFFER(2))
C                                       Corrected we hope
      BUFFER(1) = (XZOOM-1.0) * (MAGPOW-1.0) / REAL(MAGPOW)
      BUFFER(2) = (MAXXTV(2) - YZOOM) * (MAGPOW-1.0) / REAL(MAGPOW)
      CALL YLOWON (TVLIMG(1), ICHAN)
      ICHAN = ICHAN + 1
      BUFFER(1) = BUFFER(1) - TVSCRX(ICHAN)
      BUFFER(2) = BUFFER(2) + TVSCRY(ICHAN)
      IF (BUFFER(1).LT.0) BUFFER(1) = BUFFER(1) + MAXXTV(1)
      IF (BUFFER(2).LT.0) BUFFER(2) = BUFFER(2) + MAXXTV(2)
      IF (BUFFER(1).GT.MAXXTV(1)) BUFFER(1) = BUFFER(1) - MAXXTV(1)
      IF (BUFFER(2).GT.MAXXTV(2)) BUFFER(2) = BUFFER(2) - MAXXTV(2)
C                                       Move two LSBs of zoom factor
C                                       MAG into data words bit 12 and
C                                       bit 13
      MAGSFT = MAG * 4096
      BUFFER(1) = MAGSFT + BUFFER(1)
      BUFFER(2) = MAGSFT + BUFFER(2)
C                                       Grey channels
C                                       Header + IO.  ZMASK = 7F hex
      ZMASK = 127
      CALL YMKHDR ('WRIT', UNIT, COMAND, BUFSIZ, 0, 0, ZMASK, TMASK,
     *   VRTRTC, HEADER)
      NB = 2 * BUFSIZ
      CALL ZM70XF ('WRIT', NB, HEADER, BUFFER, IERR)
C                                       ZMASK = 80 hex
C                                       Graphics planes
      ZMASK = 128
      BUFFER(1) = BUFFER(1) + TVSCRX(ICHAN) - TVSCRX(16) - MAGSFT
      BUFFER(2) = BUFFER(2) - TVSCRX(ICHAN) + TVSCRX(16) - MAGSFT
      IF (BUFFER(1).LT.0) BUFFER(1) = BUFFER(1) + MAXXTV(1)
      IF (BUFFER(2).LT.0) BUFFER(2) = BUFFER(2) + MAXXTV(2)
      IF (BUFFER(1).GT.MAXXTV(1)) BUFFER(1) = BUFFER(1) - MAXXTV(1)
      IF (BUFFER(2).GT.MAXXTV(2)) BUFFER(2) = BUFFER(2) - MAXXTV(2)
C                                       Increment BUFFER(1) and
C                                       BUFFER(2) by 800 hex
      BUFFER(1) = BUFFER(1) + 2048 + MAGSFT
      BUFFER(2) = BUFFER(2) + 2048 + MAGSFT
      CALL YMKHDR ('WRIT', UNIT, COMAND, BUFSIZ, 0, 0, ZMASK, TMASK,
     *   VRTRTC, HEADER)
      NB = 2 * BUFSIZ
      CALL ZM70XF ('WRIT', NB, HEADER, BUFFER, IERR)
C                                       Update common /TVCHAR/
      IF (IERR.EQ.0) THEN
         TVZOOM(1) = MAG
         TVZOOM(2) = XZOOM
         TVZOOM(3) = YZOOM
         END IF
C
 999  RETURN
      END
