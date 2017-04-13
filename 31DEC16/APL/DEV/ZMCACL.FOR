      SUBROUTINE ZMCACL (NBYTES, INBUF, OUTBUF, LASTCH)
C-----------------------------------------------------------------------
C! convert Modcomp compressed ASCII to Hollerith characters (for FILLR)
C# Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003
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
C   Convert Modcomp compressed ASCII characters to local uncompressed
C   characters.  Successive calls will append data to the end of the
C   output buffer.  One compressed record is processed per call.
C
C   NOTE: this routine will not work in place.
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
C      INBUF    I(*)        Input buffer of Modcomp compressed ASCII
C                           packed characters
C   In/out:
C      NBYTES   I           (In)  Maximum number of bytes to convert
C                           (Out) Number of bytes added to the output
C                                 buffer
C      LASTCH   I           Position of the next character in the output
C                           buffer (zero before first call)
C   Output:
C      OUTBUF   I(*)        Output buffer containing packed characters
C                           (due to the expansion of the data, the size
C                           of the output buffer is not strictly
C                           predictable)
C   Generic version (stubbed).
C-----------------------------------------------------------------------
      HOLLERITH OUTBUF(*)
      INTEGER   NBYTES, INBUF(*), LASTCH
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (NLUSER.LT.0) GO TO 999
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
         STOP 'ZMCACL NEEDS LOCAL DEVELOPMENT'
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZMCACL: THIS ROUTINE REQUIRES LOCAL DEVELOPMENT')
      END
