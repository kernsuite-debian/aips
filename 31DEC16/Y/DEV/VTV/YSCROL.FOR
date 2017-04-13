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
C   Virtual TV version.
C-----------------------------------------------------------------------
      INTEGER   CHANNL, SCROLX, SCROLY, IERR
      LOGICAL   VRTRTC
C
      INTEGER   BITS(16), I, J
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
C                                        write: set buffer
      BUFFER(1) = CHANNL
      BUFFER(2) = SCROLX
      BUFFER(3) = SCROLY
      BUFFER(4) = 0
      IF (VRTRTC) BUFFER(4) = 1
      BUFSW = 4
      BUFSR = 4
C                                        header & IO
      CALL MKYBUF ('READ', 'YSCROL', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C                                       return MODed scrolls
      IF (IERR.EQ.0) THEN
         CALL UNYBUF (BUFSR, HEADER, BUFFER)
         SCROLX = BUFFER(2)
         SCROLY = BUFFER(3)
C                                        reset common scroll record
         CALL ZGTBIT (16, CHANNL, BITS)
         DO 10 I = 1,NGRAY
            IF (BITS(I).EQ.1) THEN
               TVSCRX(I) = SCROLX
               TVSCRY(I) = SCROLY
               END IF
 10         CONTINUE
         IF (BITS(16).EQ.1) THEN
            TVSCRX(NGRAY+1) = -BUFFER(1)
            TVSCRY(NGRAY+1) = BUFFER(2)
            END IF
         END IF
C
 999  RETURN
      END
