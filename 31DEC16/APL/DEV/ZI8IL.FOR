      SUBROUTINE ZI8IL (NVAL, NP, INB, OUTB)
C-----------------------------------------------------------------------
C! convert 8-bit unsigned integers in buffer to local integers
C# Binary
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
C   Convert 8-bit unsigned binary numbers to local integers.  This must
C   work even when the input and output buffers are the same.
C   Inputs:
C      NVAL   I         Number of 8-bit values to convert
C      NP     I         Starting position in the input buffer counting
C                       from 1 in units of 8-bit values
C      INB    I(*)      Input buffer
C   Output:
C      OUTB   I(NVAL)   Output buffer
C   Generic version - for NBITWD = n*8 uses ZGETCH, else is stubbed
C-----------------------------------------------------------------------
      INTEGER   NVAL, NP, INB(*), OUTB(*)
C
      INTEGER   I, J, K, I1, I2, L
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (NVAL.LE.0) GO TO 999
      L = NBITWD / 8
      IF (L*8.NE.NBITWD) THEN
         WRITE (MSGTXT,1000) NBITWD
         CALL MSGWRT (9)
         STOP 'ZI8IL NEEDS LOCAL DEVELOPMENT'
      ELSE
         J = NP + NVAL - 1
         K = NVAL
         DO 30 I = 1,NVAL
            I1 = (J - 1) / L + 1
            I2 = MOD (J-1, L) + 1
            CALL ZGETCH (OUTB(K), INB(I1), I2)
            J = J - 1
            K = K - 1
 30         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZI8IL NEEDS LOCAL DEVELOPMENT FOR NBITWD =',I7)
      END
