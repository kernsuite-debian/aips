      SUBROUTINE YSCROL (CHANNL, SCROLX, SCROLY, VRTRTC, IERR)
C-----------------------------------------------------------------------
C! write the scroll registers (shift location of 1 or more TV channels)
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
C   YSCROL writes the scroll registers on the TV.
C   Inputs:
C      CHANNL   I   bit map channel select: bits 1-NGRAY gray channels,
C                   bit NGRAY+1 => all graphics
C      VRTRTC   L   T => do it on vertical retrace only
C   In/Out:
C      SCROLX   I   amount of X scroll (>0 to right)
C      SCROLY   I   amount of Y scroll (>0 upwards)
C   Output:
C      IERR     I   error from Z...XF : 0 => ok, 2 => input error
C   YSCROL updates the scroll variables in /TVDEV/ common
C   IVAS version
C-----------------------------------------------------------------------
      INTEGER   CHANNL, SCROLX, SCROLY, IERR
      LOGICAL   VRTRTC
C
      INTEGER   ISCX, ISCY, IC, ITEMP, ZAND, I, JERR, JX, JY, JD,
     *   MAGFAC, JXG, JYG
      INTEGER   FIVASVPZOOMSCROLL
      REAL      TMAG
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       vertical retrace
      CALL YVRTR (VRTRTC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        write: set buffer
      ISCX = MOD (16*MAXXTV(1)-SCROLX, MAXXTV(1))
      ISCY = MOD (16*MAXXTV(2)+SCROLY, MAXXTV(2))
      ISCX = (ISCX / SCXINC) * SCXINC
      ISCY = (ISCY / SCYINC) * SCYINC
      ITEMP = 2 ** NGRAY - 1
      IC = ZAND (CHANNL, ITEMP)
      JD = 0
      IF (IC.GT.0) JD = 1
      IF (IC.NE.CHANNL) JD = JD + 2
C                                       Scroll equivalent
      MAGFAC = 1 + TVZOOM(1)
      IF (MXZOOM.GT.0) MAGFAC = 2 ** TVZOOM(1)
      TMAG = MAGFAC
      JX = (TVZOOM(2)-1.0) * (TMAG-1.0) / TMAG
      JY = (MAXXTV(2)-TVZOOM(3)) * (TMAG-1.0) / TMAG
      JX = JX + ISCX
      JY = JY + ISCY
      IF (JX.LT.0) JX = JX + MAXXTV(1)
      IF (JX.GE.MAXXTV(1)) JX = JX - MAXXTV(1)
      IF (JY.LT.0) JY = JY + MAXXTV(2)
      IF (JY.GE.MAXXTV(2)) JY = JY - MAXXTV(2)
C                                       Graphics off of grey
      JXG = JX
      JYG = JY
      IF ((NGRAY.EQ.2) .AND. ((SYSNAM(:5).EQ.'NRAO1') .OR.
     *   (SYSNAM(:5).EQ.'nrao1'))) THEN
         IF (MAGFAC.EQ.2) JXG = JXG + 1
         IF (MAGFAC.GT.2) JXG = JXG + 2
         IF (JXG.LT.0) JXG = JXG + MAXXTV(1)
         IF (JXG.GE.MAXXTV(1)) JXG = JXG - MAXXTV(1)
         END IF
C                                       Now do IO
      JERR = 0
      IF (IC.GT.0) JERR = FIVASVPZOOMSCROLL (JX, JY, MAGFAC, MAGFAC, 1)
      IF (JERR.NE.0) GO TO 900
      IF (IC.NE.CHANNL) JERR = FIVASVPZOOMSCROLL (JXG, JYG, MAGFAC,
     *   MAGFAC, 2)
      IF (JERR.NE.0) GO TO 900
C                                        reset common scroll record
      IF ((JD.NE.1) .AND. (JD.NE.3)) GO TO 15
         DO 10 I = 1,NGRAY
            TVSCRX(I) = -ISCX
            TVSCRY(I) = ISCY
 10         CONTINUE
 15   IF (JD.LT.2) GO TO 999
         TVSCRX(NGRAY+1) = -ISCX
         TVSCRY(NGRAY+1) = ISCY
         GO TO 999
C                                       Error
 900  CALL YDOERR ('YSCROL', JERR, IERR)
C
 999  RETURN
      END
