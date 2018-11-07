      SUBROUTINE ZILI32 (NVAL, INB, NP, OUTB)
C-----------------------------------------------------------------------
C! convert local integer into FITS-standard 32-bit integers
C# Binary
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2003
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
C   Convert a buffer of local large integers to a buffer of standard
C   32-bit, 2's complement integers.  This must work even when the
C   address of the input and output buffers is the same.  The IBM order
C   applies to the output (i.e., the most significant part of the 32-bit
C   integer is in the lower index of the output buffer and the least
C   significant part is in the higher index).
C   Inputs:
C      NVAL   I         # integers to convert
C      INB    I(*)      Input buffer (start at index 1)
C      NP     I         Starting position in the output buffer
C                       (1-relative) in units of 32-bit integers
C   Output:
C      OUTB   I(NVAL)   Output buffer
C   Generic version  - does byte order flip in 16-bit words and a flip
C   of the order of the words.  This assumes local 32-bit integers
C-----------------------------------------------------------------------
      INTEGER   NVAL, NP, INB(*), OUTB(*)
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (NVAL.LE.0) GO TO 999
      IF (NBITWD.NE.32) THEN
         WRITE (MSGTXT,1000) NBITWD
         CALL MSGWRT (9)
         STOP 'ZILI32 NEEDS LOCAL DEVELOPMENT'
C                                       Flip bytes in each word or copy.
      ELSE
         CALL ZBFLIP (4, NVAL, INB(1), OUTB(NP))
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZILI32: REQUIRES LOCAL DEVELOPMENT FOR NBITWD =',I7)
      END
