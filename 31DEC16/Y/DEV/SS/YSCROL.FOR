      SUBROUTINE YSCROL (CHANNL, SCROLX, SCROLY, VRTRTC, IERR)
C-----------------------------------------------------------------------
C! write the scroll registers (shift location of 1 or more TV channels)
C# TV-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2008-2009, 2015
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
C   XAS version - note that XAS has only 1 usable scroll
C-----------------------------------------------------------------------
      INTEGER   CHANNL, SCROLX, SCROLY, IERR
      LOGICAL   VRTRTC
C
      INTEGER   ISCX, ISCY, IC, ITEMP, ZAND, JX, JY, JD, MAGFAC,
     *   OPCODE, BUFFER, BUFSIZ, NWSD, DAT(4)
      REAL      TMAG
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA OPCODE /83/
C-----------------------------------------------------------------------
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
C                                       Now do IO
      DAT(2) = MAGFAC
      DAT(3) = JX
      DAT(4) = JY
      BUFSIZ = 0
      IERR = 2
C                                        reset common scroll record
C     IF ((JD.EQ.1) .OR. (JD.EQ.3)) THEN
C        CALL ZGTBIT (16, CHANNL, BITS)
C        DO 10 I = 1,NGRAY
C           IF (BITS(I).GE.1) THEN
C              DAT(1) = I
C              CALL ZSSSXF (OPCODE, DAT, BUFSIZ, BUFFER, NWSD, IERR)
C              IF (IERR.NE.0) GO TO 900
C              TVSCRX(I) = -ISCX
C              TVSCRY(I) = ISCY
C              END IF
C10         CONTINUE
C        END IF
C                                       graphics too
C     IF (JD.GE.2) THEN
C        DAT(1) = NGRAY + 1
C        CALL ZSSSXF (OPCODE, DAT, BUFSIZ, BUFFER, NWSD, IERR)
C        IF (IERR.NE.0) GO TO 900
C        TVSCGX = -ISCX
C        TVSCGY = ISCY
C        END IF
      DAT(1) = 0
      CALL ZSSSXF (OPCODE, DAT, BUFSIZ, BUFFER, NWSD, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL FILL (NGRAY, -ISCX, TVSCRX)
      CALL FILL (NGRAY, ISCY, TVSCRY)
      TVSCGX = -ISCX
      TVSCGY = ISCY
      GO TO 999
C
900   WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
1900  FORMAT ('ERROR IN YSCROL FROM ZSSSXF',I5)
      END
