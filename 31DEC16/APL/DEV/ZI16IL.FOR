      SUBROUTINE ZI16IL (NVAL, NP, INB, OUTB)
C-----------------------------------------------------------------------
C! convert FITS-standard 16-bit integers to local integers
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
C   Extract 16-bit, 2's complement integers from an input buffer and
C   put them into an output buffer in local integer form.  This must
C   work even when the address of the input and output buffers is the
C   same.
C   Inputs:
C      NVAL   I         Number of 16-bit integers to extract
C      NP     I         Starting position in the input buffer counting
C                       from 1 in units of 16-bit integers
C      INB    I*2(*)    Input buffer
C   Output:
C      OUTB   I(NVAL)   Output buffer
C   Generic version - assumes that the local machine is 32-bit
C-----------------------------------------------------------------------
      INTEGER   NVAL, NP, OUTB(*), INB(*)
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (NVAL.LE.0) GO TO 999
      IF (NBITWD.NE.32) THEN
         WRITE (MSGTXT,1000) NBITWD
         CALL MSGWRT (9)
         STOP 'ZI16IL NEEDS LOCAL DEVELOPMENT'
C                                       32-bit machines: move data
      ELSE
         CALL ZIPACK ('SOTL', NVAL, OUTB, NP, INB)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZI16IL: REQUIRES LOCAL DEVELOPMENT FOR NBITWD =',I7)
      END
