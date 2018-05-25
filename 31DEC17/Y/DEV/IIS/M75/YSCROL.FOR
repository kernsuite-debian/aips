      SUBROUTINE YSCROL (CHANNL, SCROLX, SCROLY, VRTRTC, IERR)
C-----------------------------------------------------------------------
C! write the scroll registers (shift location of 1 or more TV channels)
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
C   Version of IIS Model 75
C-----------------------------------------------------------------------
      INTEGER   CHANNL, SCROLX, SCROLY, IERR
      LOGICAL   VRTRTC
C
      INTEGER   UNIT, HEADER(8), BUFFER(2), BUFSIZ, NB, BITS(16), I,
     *   ITEMP, BIT16, IC, TMASK, ISCX, ISCY, MAGPOW, ZMASK, ZOR, ZAND
      LOGICAL   COMAND
      INCLUDE 'INCS:DTVC.INC'
      DATA UNIT, COMAND, BUFSIZ / 6, .FALSE., 2/
      DATA BIT16 /-32768/
C-----------------------------------------------------------------------
C                                       TMASK = 1201 hex
      TMASK = 4609
C                                       Set buffer
      ISCX = MOD (16*MAXXTV(1)-SCROLX, MAXXTV(1))
      ISCY = MOD (16*MAXXTV(2)+SCROLY, MAXXTV(2))
      ISCX = (ISCX / SCXINC) * SCXINC
      ISCY = (ISCY / SCYINC) * SCYINC
      MAGPOW = 2 ** TVZOOM(1)
      BUFFER(1) = (TVZOOM(2)-1.0) * (MAGPOW-1.0) / REAL(MAGPOW)
      BUFFER(2) = (MAXXTV(2) - TVZOOM(3)) * (MAGPOW-1.0) / REAL(MAGPOW)
      BUFFER(1) = BUFFER(1) + ISCX
      BUFFER(2) = BUFFER(2) + ISCY
      IF (BUFFER(1).LT.0) BUFFER(1) = BUFFER(1) + MAXXTV(1)
      IF (BUFFER(2).LT.0) BUFFER(2) = BUFFER(2) + MAXXTV(2)
      IF (BUFFER(1).GT.MAXXTV(1)) BUFFER(1) = BUFFER(1) - MAXXTV(1)
      IF (BUFFER(2).GT.MAXXTV(2)) BUFFER(2) = BUFFER(2) - MAXXTV(2)
      BUFFER(1) = BUFFER(1) + 4096 * TVZOOM(1)
      BUFFER(2) = BUFFER(2) + 4096 * TVZOOM(1)
      ITEMP = 2 ** NGRAY - 1
      IC = ZAND (CHANNL, ITEMP)
      ZMASK = IC
C                                       Header & IO
C                                       The LSB of T register is set to
C                                       update scroll on M75.
      CALL YMKHDR ('WRIT', UNIT, COMAND, BUFSIZ, 0, 0, ZMASK, TMASK,
     *   VRTRTC, HEADER)
      NB = 2 * BUFSIZ
      CALL ZM70XF ('WRIT', NB, HEADER, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Wrap around graphics channel
C                                       to display graphics.
      IF (IC.NE.CHANNL) IC = ZOR (IC, BIT16)
      CALL ZGTBIT (16, IC, BITS)
      IF (BITS(16).EQ.1) THEN
         ZMASK = 128
C                                       Increment BUFFER(1) and
C                                       BUFFER(2) by 800 hex
         BUFFER(1) = BUFFER(1) + 2048
         BUFFER(2) = BUFFER(2) + 2048
C                                       The LSB of T register is set to
C                                       update scroll on M75.
         CALL YMKHDR ('WRIT', UNIT, COMAND, BUFSIZ, 0, 0, ZMASK, TMASK,
     *      VRTRTC, HEADER)
         NB = 2 * BUFSIZ
         CALL ZM70XF ('WRIT', NB, HEADER, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Reset common scroll record
      DO 10 I = 1,NGRAY
         IF (BITS(I).EQ.1) THEN
            TVSCRX(I) = -ISCX
            TVSCRY(I) = ISCY
            END IF
 10      CONTINUE
      IF (BITS(16).EQ.1) THEN
         TVSCRX(NGRAY+1) = -ISCX
         TVSCRY(NGRAY+1) = ISCY
         END IF
C
 999  RETURN
      END
