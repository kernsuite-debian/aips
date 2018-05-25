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
C   IIS Model 70 version
C-----------------------------------------------------------------------
      INTEGER   CHANNL, SCROLX, SCROLY, IERR
      LOGICAL   VRTRTC
C
      INTEGER   UNIT, HEADER(8), BUFFER(2), BUFSIZ, NB, BITS(16), I,
     *   ITEMP, BIT16, IC, ZOR, ZAND
      LOGICAL   COMAND
      INCLUDE 'INCS:DTVC.INC'
      DATA UNIT, COMAND, BUFSIZ / 6, .FALSE., 2/
      DATA BIT16 /-32768/
C-----------------------------------------------------------------------
C                                        write: set buffer
      BUFFER(1) = MOD (16*MAXXTV(1)-SCROLX, MAXXTV(1))
      BUFFER(2) = MOD (16*MAXXTV(2)+SCROLY, MAXXTV(2))
      BUFFER(1) = (BUFFER(1) / SCXINC) * SCXINC
      BUFFER(2) = (BUFFER(2) / SCYINC) * SCYINC
      ITEMP = 2 ** NGRAY - 1
      IC = ZAND (CHANNL, ITEMP)
      IF (IC.NE.CHANNL) IC = ZOR (IC, BIT16)
C                                        header & IO
      CALL YMKHDR ('WRIT', UNIT, COMAND, BUFSIZ, 0, 0, IC, 0, VRTRTC,
     *   HEADER)
      NB = 2 * BUFSIZ
      CALL ZM70XF ('WRIT', NB, HEADER, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        reset common scroll record
      CALL ZGTBIT (16, IC, BITS)
      DO 10 I = 1,NGRAY
         IF (BITS(I).EQ.1) THEN
            TVSCRX(I) = -BUFFER(1)
            TVSCRY(I) = BUFFER(2)
            END IF
 10      CONTINUE
      IF (BITS(16).EQ.1) THEN
         TVSCRX(NGRAY+1) = -BUFFER(1)
         TVSCRY(NGRAY+1) = BUFFER(2)
         END IF
C
 999  RETURN
      END
