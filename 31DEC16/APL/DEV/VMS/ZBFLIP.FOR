      SUBROUTINE ZBFLIP (NB, NVALS, INBUF, OUTBUF)
C-----------------------------------------------------------------------
C! interchange bytes in buffer if needed to go between local & standard
C# Z Binary Tape FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1998
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
C   Interchange the low order and high order bytes for all words in the
C   input buffer and put the results in output buffer (which may be the
C   same as the input buffer).  For machines that are not byte flipped,
C   the output buffer is identical to the input buffer (see BYTFLP in
C   ZDCHIN).  Any required byte swapping is performed via a call to
C   ZBYTF2.  NOTE a byte is 8 bits here.  Words are swapped as well
C   unlike ZBYTFL.
C   Inputs:
C      NB       I      Number bytes in a value
C      NVALS    I      Number of NB-byte values
C      INBUF    I(*)   Input buffer
C   Output:
C      OUTBUF   I(*)   Output buffer containing byte/word swapped values
C   Generic version - uses ZBYTF2 and ZADDR, works for 16, 32, 48, and
C   64-bit machines only
C   VMS version uses INTEGER*2
C-----------------------------------------------------------------------
      INTEGER   NB, NVALS, INBUF(*), OUTBUF(*)
C
      INTEGER*2 S2(4), JT
      INTEGER   S4(2), K, I
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (S4, S2)
C-----------------------------------------------------------------------
      IF (NBITWD.EQ.32) THEN
         K = NB * NVALS / 2
         CALL ZBYTFL (K, INBUF, OUTBUF)
         IF (BYTFLP.GE.2) THEN
            IF (NB.EQ.4) THEN
               DO 10 I = 1,NVALS
                  S4(1) = OUTBUF(I)
                  JT = S2(1)
                  S2(1) = S2(2)
                  S2(2) = JT
                  OUTBUF(I) = S4(1)
 10               CONTINUE
            ELSE IF (NB.EQ.8) THEN
               K = 2 * NVALS
               DO 20 I = 1,K,2
                  S4(1) = OUTBUF(I)
                  JT = S2(1)
                  S2(1) = S2(2)
                  S2(2) = JT
                  S4(2) = OUTBUF(I+1)
                  JT = S2(3)
                  S2(2) = S2(4)
                  S2(4) = JT
                  OUTBUF(I+1) = S4(1)
                  OUTBUF(I) = S4(2)
 20               CONTINUE
               END IF
            END IF
      ELSE
         MSGTXT = 'ZBFLIP REQUIRES LOCAL DEVELOPMENT'
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
      END
