      SUBROUTINE TKCURS (IOBLK, IX, IY, IERR)
C-----------------------------------------------------------------------
C! turns on, reads, turns off the TK graphics cursor
C# Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   TKCURS will activate the cursor on the TEKTRONIX 4012 and wait for
C   a response from the 4012 keyboard. after the response the cursor
C   will disappear and TEKCUR will return the coordinate positions.
C   Inputs:
C      IOBLK   I(256)   I/O block for TEKTRONIX device.
C   Outputs:
C      IX      I        x cursor position.
C      IY      I        y cursor position.
C      IERR    I        0=ok, 1=TEK write error. 2=TEK read error.
C   WARNING: This routine assumes a normal interface to a TEK 401n.
C   Thus it may not work on all CPUs.
C-----------------------------------------------------------------------
      INTEGER   IOBLK(256), IX, IY, IERR
C
      INTEGER   IT, I1, I2, ZAND
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTKS.INC'
C-----------------------------------------------------------------------
C                                       Flush/zero buffer.
      CALL TEKFLS (IERR)
      IF (IERR.NE.0) THEN
         IERR = 1
         GO TO 999
         END IF
C                                       Activate cursor.
      IT = 27
      CALL ZTKBUF (IT, 1, IERR)
      IF (IERR.NE.0) GO TO 950
      IT = 26
      CALL ZTKBUF (IT, 1, IERR)
      IF (IERR.NE.0) GO TO 950
      CALL TEKFLS (IERR)
      IF (IERR.NE.0) THEN
         IERR = 1
         GO TO 999
         END IF
C                                       Read from TEK.
 10   CALL ZFIO ('READ', TKLUN, TKFIND, 5, IOBLK, IERR)
      IF (IERR.EQ.0) GO TO 20
         IERR = 2
         GO TO 999
C                                       Interpret the results.
C                                       X coordinate.
 20   CALL ZGETCH (IT, IOBLK(1), 2)
      IT = ZAND (IT, 31)
      CALL ZGETCH (IX, IOBLK(1), 3)
      IX = ZAND (IX, 31)
      IX = 32 * IT + IX + 1
C                                       Y coordinate.
      CALL ZGETCH (IT, IOBLK(1), 4)
      IT = ZAND (IT, 31)
      I1 = 2
      IF (NBITWD.GE.40) I1 = 1
      I2 = 5 - 4 * (I1-1)
      CALL ZGETCH (IY, IOBLK(I1), I2)
      IY = ZAND (IY, 31)
      IY = 32 * IT + IY + 1
      GO TO 999
C                                       ZTKBUF error.
 950  IERR = 3
C
 999  RETURN
      END
