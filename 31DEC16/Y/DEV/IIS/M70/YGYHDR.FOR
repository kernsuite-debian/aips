      SUBROUTINE YGYHDR (OP, NPIXEL, XINIT, YINIT, IANGLE, CHANNL,
     *   PLANES, PACKED, BYPIFM, BYTE, ADDWRT, ACCUM, VRTRTC, HEADER,
     *   IERR)
C-----------------------------------------------------------------------
C! builds basic TV IO header to write gray scale data
C# TV-util
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
C   YGYHDR builds an TV header to write image data.  The actual IO
C   must be done by calls to YIMGIO or Z...XF.
C   Inputs:
C      OP      C*4     'READ' from TV or 'WRIT' to TV
C      NPIXEL  I       number of pixel values to IO
C      XINIT   I       first pixel X coordinate (1-512, 1 -> LHS)
C      YINIT   I       first pixel Y coordinate (1-512, 1 -> bot)
C      IANGLE  I       (0 => data IO horizontal to right, 1 => up,
C                      2 => to left, 3 => down)
C      CHANNL  I       channel select bit mask
C      PLANES  I       bit plane select bit mask
C      PACKED  L       T => 2 values/word, F => 1 value/word
C      BYPIFM  L       F => IFM lookup applied to data (write)
C      BYTE    L       T => 8 values/byte (needs XINIT = 8*n+1)
C      ADDWRT  L       T => OR data with present memory contents
C      ACCUM   L       T => use 16-bit accumulator mode
C      VRTRTC  L       T => do it only during vertical retrace
C   Output:
C      HEADER  I(8)    header to be sent to TV
C      IERR    I       error code of 0 => ok
C                                    2 => input error
C   IIS Model 70 version
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   NPIXEL, XINIT, YINIT, IANGLE, CHANNL, PLANES,
     *   HEADER(8), IERR
      LOGICAL   PACKED, BYPIFM, BYTE, ADDWRT, ACCUM, VRTRTC
C
      INTEGER   UNIT, BITS(16), IC, IX, IY
      LOGICAL   COMAND
      INCLUDE 'INCS:DTVC.INC'
      DATA UNIT, COMAND / 1, .FALSE./
C-----------------------------------------------------------------------
C                                        I2S coordinates
      IX = XINIT - 1
      IY = MAXXTV(2) - YINIT
      IC = NPIXEL
      IF (BYTE) IC = (IC+7)/8
C                                        test errors
      IERR = 2
      IF ((IANGLE.NE.0) .AND. (IANGLE.NE.3)) GO TO 999
      IF (IC.LE.0) GO TO 999
      IF ((IX.LT.0) .OR. (IX.GT.MAXXTV(1)-1)) GO TO 999
      IF ((IY.LT.0) .OR. (IY.GT.MAXXTV(2)-1)) GO TO 999
      IF ((OP.NE.'READ') .AND. (OP.NE.'WRIT')) GO TO 999
C                                        ok - get basic header
      IERR = 0
      CALL YMKHDR (OP, UNIT, COMAND, IC, IX, IY, CHANNL, PLANES,
     *   VRTRTC, HEADER)
C                                        revise for vertical
C                                        note: IIS supports down
      IF (IANGLE.EQ.3) THEN
         HEADER(5) = IX + 16384
         HEADER(6) = IY + 16384
         END IF
C                                        add to Xfer ID word
      CALL ZGTBIT (16, HEADER(1), BITS)
      IF (ACCUM ) BITS(11) = 1
      IF (ADDWRT) BITS(12) = 1
      IF (BYTE  ) BITS(13) = 1
      IF (BYPIFM) BITS(14) = 1
      IF (PACKED) BITS(15) = 1
      CALL ZPTBIT (16, HEADER(1), BITS)
C                                        NO IO !!!!!
 999  RETURN
      END
