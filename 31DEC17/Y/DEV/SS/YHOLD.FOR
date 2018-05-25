      SUBROUTINE YHOLD (OP, IERR)
C-----------------------------------------------------------------------
C! Turn command buffering on or off
C# Y1 TV-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   YHOLD informs the TV device or server when it may choose to become
C   asynchronous (delaying screen updates until convenient) and when it
C   must become syncronous again (doing each command as it's issued).
C   This function is placed in the hands of the programmer but you are
C   obliged to use it carefully.  Cursor reads and the like will not
C   call YHOLD ('OFFF', IERR) - the only automatic call is in YTVCLS.
C   Inputs:
C      OP     C*4   'ONNN' -> become asynchronous, 'OFFF' -> force
C                   resynchronization and remain synchronous
C   Output:
C      IERR   I     Error return - 0 is normal.
C   Screen server version: sends in parameter to SSS, XVSS, XAS
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      INTEGER   NWSD, OPCODE, DAT(4), DUMMY
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      OPCODE = 16
      CALL FILL (4, 0, DAT)
      IF (OP.EQ.'ONNN') THEN
         ISYNCH = 1
      ELSE
         ISYNCH = 0
         END IF
      DAT(1) = ISYNCH
      CALL ZSSSXF (OPCODE, DAT, 0, DUMMY, NWSD, IERR)
C
 999  RETURN
      END
