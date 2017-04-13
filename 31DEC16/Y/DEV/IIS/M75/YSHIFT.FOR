      SUBROUTINE YSHIFT (OP, SHIFTR, SHIFTG, SHIFTB, VRTRTC, IERR)
C-----------------------------------------------------------------------
C! read/write the shift (bias) registers of the TV (IIS M70, not used)
C# Y2 TV-basic
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
C   YSHIFT reads/writes the TV shift registers - which shift the
C   13-bit output of the adders before entry into the 10-bit OFM
C   Inputs:
C      OP      C*4   'READ' from TV or 'WRIT' to TV
C      VRTRTC  L     T => do on vertical retrace only
C   In/Out:
C      SHIFTR  I     # bits to shift (right) red channel
C      SHIFTG  I     # bits to shift green channel
C      SHIFTB  I     # bits to shift blue channel
C   Output:
C      IERR    I     error code of Z...XF : 0 - ok, 2 - input error
C   Version for IIS Model 75 - limited to 2 bits of shift
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   SHIFTR, SHIFTG, SHIFTB, IERR
      LOGICAL   VRTRTC
C
      INTEGER   UNIT, BUFFER, BUFSIZ, HEADER(8), BITS(6), NB
      LOGICAL   COMAND
      DATA UNIT, COMAND, BUFSIZ /3, .TRUE., 1/
C-----------------------------------------------------------------------
C                                       WRIT: check & set buffer
      IF (OP.EQ.'WRIT') THEN
         IERR = 2
C                                       Output from the adder array of
C                                       M70 contains 13 bits, but 12
C                                       bits on M75. Therefore the
C                                       shifting of data stream can be
C                                       up to 2 bits instead of 3 bits.
         IF ((SHIFTR.LT.0) .OR. (SHIFTR.GT.2)) GO TO 999
         IF ((SHIFTG.LT.0) .OR. (SHIFTG.GT.2)) GO TO 999
         IF ((SHIFTB.LT.0) .OR. (SHIFTB.GT.2)) GO TO 999
         CALL ZGTBIT (2, SHIFTB, BITS(1))
         CALL ZGTBIT (2, SHIFTG, BITS(3))
         CALL ZGTBIT (2, SHIFTR, BITS(5))
C                                       For some reason IIS programmer
C                                       removed next line
         BUFFER = 0
         CALL ZPTBIT (6, BUFFER, BITS)
         END IF
C                                       Header & IO
      CALL YMKHDR (OP, UNIT, COMAND, BUFSIZ, 0, 0, 0, 0, VRTRTC, HEADER)
      NB = 2 * BUFSIZ
      CALL ZM70XF (OP, NB, HEADER, BUFFER, IERR)
C                                       Return data
      IF ((OP.NE.'WRIT') .AND. (IERR.EQ.0)) THEN
         CALL ZGTBIT (6, BUFFER, BITS)
         CALL ZPTBIT (2, SHIFTB, BITS(1))
         CALL ZPTBIT (2, SHIFTG, BITS(3))
         CALL ZPTBIT (2, SHIFTR, BITS(5))
         END IF
C
 999  RETURN
      END
