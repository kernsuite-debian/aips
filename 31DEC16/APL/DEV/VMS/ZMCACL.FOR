      SUBROUTINE ZMCACL (NBYTES, INBUF, OUTBUF, LASTCH)
C-----------------------------------------------------------------------
C! convert Modcomp compressed ASCII to Hollerith characters (for FILLR)
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
C   Convert Modcomp compressed ASCII characters to local uncompressed
C   characters.  Successive calls will append data to the end of the
C   output buffer.  One compressed record is processed per call.
C
C   NOTE: this routine will not work inplace.
C
C   Modcomp compressed ASCII format for each logical record:
C      BYTE              Use
C       0     ASCII ETX (Hex 03)
C       1     checksum (optional)
C      2-3    byte count, negative => end of file (not on tape?)
C             (NOTE: may be bytes 1-2)
C      4-     Compressed ASCII characters.  A NULL (Hex 00) terminates.
C             A negative value (most significant bit on) indicates a
C             repetition of the previous character the number of times
C             indicated by the absolute value of the negative number.
C             Example: an ASCII 'C' followed by a byte with the HEX
C             value FF (2's complement -1) indicates two 'C's.
C
C   Inputs:
C      INBUF    I(*)   Input buffer of Modcomp compressed ASCII packed
C                      characters
C   In/out:
C      NBYTES   I      (In)  Maximum number of bytes to convert
C                      (Out) Number of bytes added to the output buffer
C      LASTCH   I      Position of the next character in the output
C                      buffer (zero before first call)
C   Output:
C      OUTBUF   H(*)   Output buffer containing packed characters (due
C                      to the expansion of the data, the size of the
C                      output buffer is not strictly predictable)
C   VMS version - is same as
C   Convex version (should work for any machine supporting the BYTE
C   data type and on which a BYTE variable can be treated as a signed
C   integer).
C-----------------------------------------------------------------------
      INTEGER   NBYTES, LASTCH
      BYTE      INBUF(*), OUTBUF(*)
C
      INTEGER   LOOP, RPOINT, BYTCNT, REPEAT, I, OBCNT, INPNT
      BYTE      ETX, NULL, LAST
      DATA ETX, NULL /3, 0/
C-----------------------------------------------------------------------
      IF (NBYTES.GT.0) THEN
C                                       Initialize pointers, etc.
         INPNT = 1
         IF (LASTCH.LE.0) LASTCH = 1
         RPOINT = LASTCH
         OBCNT = 0
C                                       First character should be ETX.
         IF (INBUF(INPNT).EQ.ETX) THEN
C                                       Skip check sum.
            INPNT = INPNT + 2
C                                       Get byte count.
            CALL ZI16IL (1, 1, INBUF(INPNT), BYTCNT)
            INPNT = INPNT + 2
C                                       Trap no data.
            IF (BYTCNT.LE.0) GO TO 100
C                                       Loop over record.
               DO 90 LOOP = 1,BYTCNT
C                                       Check for buffer exhausted or
C                                       next character NULL.
                  IF ((INPNT.GT.NBYTES) .OR. (INBUF(INPNT).EQ.NULL))
     *               GO TO 100
C                                       Expand repeated character.
                     IF (INBUF(INPNT).LE.0) THEN
                        REPEAT = INBUF(INPNT)
                        REPEAT = -REPEAT
                        LAST = OUTBUF(LASTCH-1)
                        DO 50 I = 1,REPEAT
                           OUTBUF(LASTCH) = LAST
                           LASTCH = LASTCH + 1
                           OBCNT = OBCNT + 1
 50                        CONTINUE
C                                       single character
                     ELSE
                        OUTBUF(LASTCH) = INBUF(INPNT)
                        LASTCH = LASTCH + 1
                        OBCNT = OBCNT + 1
                        END IF
                     INPNT = INPNT + 1
 90               CONTINUE
C                                       Convert to local characters.
C                                         Now redundant!
C 100        CALL ZC8CL (OBCNT, 1, OUTBUF(RPOINT), OUTBUF(RPOINT))
C
 100        NBYTES = OBCNT
C                                       Didn't find ETX as expected.
         ELSE
            NBYTES = 0
            END IF
         END IF
C
 999  RETURN
      END
