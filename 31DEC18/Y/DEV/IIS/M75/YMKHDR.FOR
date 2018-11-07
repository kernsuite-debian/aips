      SUBROUTINE YMKHDR (OP, UNIT, COMAND, XFRCNT, XREG, YREG, ZREG,
     *   TREG, VRTRTC, HEADER)
C-----------------------------------------------------------------------
C! builds standard TV-IO header, used for IIS Models 70 and 75
C# Y3 TV-util
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
C   YMKHDR builds a standard TV header - no checksum, asserts advance X
C   on thing count and advance Y on X overflow
C   Inputs:
C      OP       C*4   'READ' from TV / 'WRIT' to TV
C      UNIT     I     TV subunit number
C      COMAND   L     T => command mode, F => data mode
C      XFRCNT   I     thing count: number of words to transfer not
C                     counting header / packed mode is number of bytes
C      XREG     I     low bits of address in subunit (0 - 511)
C      YREG     I     high bis of address in subunit (0 - 511)
C      ZREG     I     usually channel select mask
C      TREG     I     usually bit plane or subchannel select mask
C      VRTRTC   L     T => do IO only during vertical retrace
C   Output:
C      HEADER   I(8)  basic header
C   Version for IIS Model 75
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   UNIT, XFRCNT, XREG, YREG, ZREG, TREG, HEADER(8)
      LOGICAL   COMAND, VRTRTC
C
      INTEGER   BITS(16), BIT16
C-----------------------------------------------------------------------
C                                       Build z8000
      CALL FILL (16, 0, BITS)
      BITS(16) = 1
      CALL ZPTBIT (16, BIT16, BITS)
C                                       Build transfer ID
      IF (OP.EQ.'WRIT') BITS(16) = 0
      IF (VRTRTC) BITS(9) = 1
      CALL ZPTBIT (16, HEADER(1), BITS)
C                                       Negative count
      HEADER(2) = -XFRCNT
C                                       Subunit & command
      HEADER(3) = UNIT
      IF (COMAND) HEADER(3) = HEADER(3) + BIT16
C                                       X, Y (range not known)
      HEADER(5) = XREG
      HEADER(6) = YREG
C                                       Pass Z,T registers
      HEADER(7) = ZREG
      HEADER(8) = TREG
C
 999  RETURN
      END
