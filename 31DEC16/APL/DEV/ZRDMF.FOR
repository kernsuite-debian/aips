      SUBROUTINE ZRDMF (NWORDS, INBUF, OUTBUF, FLAG)
C-----------------------------------------------------------------------
C! convert DEC Magtape Format (36 bits data in 40 bits) to 2 integers
C# Z Binary Tape
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
C   Convert data packed in DEC-Magtape format (DMF) to pairs of local
C   32-bit integers.
C
C   The DMF format is:
C
C      Track    1    2    3    4    5    6    7    8
C      Byte
C        1      F0   F1   F2   F3   F4   F5   F6   F7
C        2      F8   F9  F10  F11  F12  F13  F14  F15
C        3      F16  F17  R0   R1   R2   R3   R4   R5
C        4      R6   R7   R8   R9  R10  R11  R12  R13
C        5       0    0    0    0  R14  R15  R16  R17
C
C   where Rn refers to the right halfword, Fn to the left halfword.
C
C   Since the purpose of this routine is to read Modcomp tapes written
C   with this peculiar format, F16, F17, R16 and R17 (the high order
C   bits) are zero for VLA data, but are used for the word count.
C
C   The first word (5 bytes) of a tape block contains the word count of
C   the block.  The word count is a 16-bit, twos-complement integer
C   comprised of bits R2-R16.  All other words are treated as pairs of
C   16-bit, twos-complement integers comprising bits F0-F15 and R0-R15.
C
C   Input data is assumed to be packed into 1 1/4 integers and output
C   data will be returned in a pair of local integers per DEC-10 word.
C   The first integer of each pair corresponds to the left DEC-10
C   halfword (zero for the word count) and the second to the right
C   halfword.  The sign of each halfword is preserved on expansion to a
C   local integer.
C
C   Inputs:
C      NWORDS   I           Length of the input buffer in DEC-10
C                           words
C      INBUF    I(*)        Input buffer containing DMF format data
C      FLAG     I           If > 0, the first word word is the
C                           beginning of a tape block
C   Output:
C      OUTBUF   I(*)        Output buffer containing two local short
C                           integers per input DEC-10 word
C   Generic version (stubbed).
C-----------------------------------------------------------------------
      INTEGER   NWORDS, INBUF(*), OUTBUF(*), FLAG
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      WRITE (MSGTXT,1000)
      CALL MSGWRT (10)
      IF (NLUSER.GT.0) STOP 'ZRDMF missing'
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZRDMF REQUIRES LOCAL DEVELOPMENT')
      END
