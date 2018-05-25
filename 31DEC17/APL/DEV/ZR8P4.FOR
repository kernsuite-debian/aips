      SUBROUTINE ZR8P4 (OP, INTG, DX)
C-----------------------------------------------------------------------
C! converts pseudo I*4 to double precision - for tape handling only
C# Binary Tape
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
C   ZR8P4 converts between pseudo I*4 and Double Precision.  The main
C   use of this is to handle I*4 integers in buffers with mainly I*2
C   integers for tape IO - i.e. EXPORT and IBM format headers.  The
C   assumption is that the full buffer has been treated with (on read
C   of tape) ZI16IL and needs to be repacked to 32 bits and then
C   changed to local integer and then to local D.P.  And the reverse
C   on tape writing.
C   Inputs:
C      OP     C*4    '4IB8' IBM I*4 to D
C                    '8IB4' D to IBM I*4
C   In/out:
C      INTG   I(2)   the I
C      DX     D      the D
C   IBM pseudo-I*4 has the form of a 2's complement, 32-bit integer
C   with the most significant 16 bits in the I word of lower index and
C   the least significant 16 bits in the I word of higher index.
C   Generic version - may work for all
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   INTG(2)
      DOUBLE PRECISION DX
C
      INTEGER   ITEMP, I4
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       '4IB8' : IBM to D.P.
      IF (OP.EQ.'4IB8') THEN
         CALL ZILI16 (2, INTG, 1, ITEMP)
         CALL ZI32IL (1, 1, ITEMP, I4)
         DX = I4
C                                       '8IB4' : D.P. to IBM
      ELSE IF (OP.EQ.'8IB4') THEN
         IF (DX.GT.0.0D0) THEN
            I4 = DX + 0.5D0
         ELSE
            I4 = DX - 0.5D0
            END IF
         CALL ZILI32 (1, I4, 1, ITEMP)
         CALL ZI16IL (2, 1, ITEMP, INTG)
C                                       Illegal OP
      ELSE
         WRITE (MSGTXT,1000) OP
         CALL MSGWRT (9)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZR8P4: OP = ''',A4,''' NOT SUPPORTED - CALL PROGRAMMERS',
     *   ' PLEASE')
      END
