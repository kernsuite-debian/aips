      SUBROUTINE ZILI16 (NINT, INB, NP, OUTB)
C-----------------------------------------------------------------------
C! convert local integers to 16-bit FITS integers in a buffer
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
C   Convert a buffer of local integers to a buffer of standard 16-bit,
C   2's complement integers.
C   Inputs:
C      NINT   I      Number of integers to convert
C      INB    I(*)   Input buffer (start at index 1)
C      NP     I      Starting index in the output buffer (1-relative)
C                    in units of 16-bit integers
C   Output:
C      OUTB   I(*)   Output buffer
C   Generic version - assumes that the local machine is 32-bit
C-----------------------------------------------------------------------
      INTEGER   NINT, INB(*), NP, OUTB(*)
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (NINT.LE.0) GO TO 999
      IF (NBITWD.NE.32) THEN
         WRITE (MSGTXT,1000) NBITWD
         CALL MSGWRT (9)
         STOP 'ZILI16 NEEDS LOCAL DEVELOPMENT'
C                                       Move data
      ELSE
         CALL ZIPACK ('LOTS', NINT, INB, NP, OUTB)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZILI16: REQUIRES LOCAL DEVELOPMENT FOR NBITWD =',I7)
      END
