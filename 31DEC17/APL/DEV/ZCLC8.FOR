      SUBROUTINE ZCLC8 (NCHAR, INBUF, NP, OUTBUF)
C-----------------------------------------------------------------------
C! convert local character string to packed ASCII buffer
C# Z Binary
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
C   Convert local characters in a buffer to standard 8-bit ASCII
C   character form
C   Inputs:
C      NCHAR    I       Number of characters to convert
C      INBUF    C*(*)   Characters in local form
C      NP       I       Starting position in output buffer in units of
C                       8-bit characters (1-relative)
C   Output:
C      OUTBUF   R(*)    Buffer containing characters in 8-bit ASCII form
C   Generic version - assumes local characters are ASCII.
C   Requires local development if NBITWD not n*8.
C-----------------------------------------------------------------------
      INTEGER   NCHAR, NP
      CHARACTER INBUF*(*)
      REAL      OUTBUF(*)
C
      INTEGER   J, NCHPFP, J1, J2, I, I1, I2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (NCHAR.LE.0) GO TO 999
      NCHPFP = NBITWD / 8
      J = MOD (NBITWD, 8)
      IF (J.NE.0) THEN
         WRITE (MSGTXT,9999) NBITWD
         CALL MSGWRT (9)
         STOP'ZCLC8 NEEDS LOCAL DEVELOPMENT'
      ELSE
         I1 = MAX (NP, 1) - 1
         DO 20 I = 1,NCHAR
            J = ICHAR (INBUF(I:I))
            I2 = I + I1
            J1 = (I2-1) / NCHPFP + 1
            J2 = MOD (I2-1, NCHPFP) + 1
            CALL ZPUTCH (J, OUTBUF(J1), J2)
 20         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 9999 FORMAT ('ZCLC8: REQUIRES LOCAL DEVELOPMENT FOR NBITWD=',I7)
      END
