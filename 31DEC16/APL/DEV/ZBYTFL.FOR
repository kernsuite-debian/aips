      SUBROUTINE ZBYTFL (NWORDS, INBUF, OUTBUF)
C-----------------------------------------------------------------------
C! interchange bytes in buffer if needed to go between local & standard
C# Z Binary Tape FITS
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
C   Interchange the low order and high order bytes for all words in the
C   input buffer and put the results in output buffer (which may be the
C   same as the input buffer).  For machines that are not byte flipped,
C   the output buffer is identical to the input buffer (see BYTFLP in
C   ZDCHIN).  Any required byte swapping is performed via a call to
C   ZBYTF2.  NOTE a byte is 8 bits here.
C   Inputs:
C      NWORDS   I           Number of 16-BIT words to byte swap
C      INBUF    I*2(*)      Input buffer
C   Output:
C      OUTBUF   I*2(*)      Output buffer containing the byte swapped
C                           words
C   Generic version - uses ZBYTF2 and ZADDR, works for 16, 32, 48, and
C   64-bit machines only
C-----------------------------------------------------------------------
      INTEGER   NWORDS
      INTEGER*2 INBUF(*), OUTBUF(*)
C
      INTEGER   IERR, I, J
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Only swap bytes/words if this is
C                                       a byte/word-swapped machine
C                                       (e.g., a VAX).
      IF (BYTFLP.GT.0) THEN
         CALL ZBYTF2 (NWORDS, BYTFLP, NBITWD, INBUF, OUTBUF)
C                                       Non-byte/word-swapped machine,
C                                       but is INBUF the same as OUTBUF?
      ELSE
         CALL ZADDR (INBUF, OUTBUF, IERR)
C                                       If not, copy buffers
         IF (IERR.NE.0) THEN
            J = NBITWD / 16
            IF (J*16.EQ.NBITWD) THEN
               DO 20 I = 1,NWORDS
                  OUTBUF(I) = INBUF(I)
 20               CONTINUE
            ELSE
               WRITE (MSGTXT,1000) NBITWD
               CALL MSGWRT (9)
               STOP 'ZBYTFL missing'
               END IF
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZBYTFL: REQUIRES LOCAL DEVELOPMENT FOR NBITWD =',I6)
      END
