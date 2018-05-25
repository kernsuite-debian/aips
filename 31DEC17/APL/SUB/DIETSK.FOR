      SUBROUTINE DIETSK (IRET, RQUICK, IBUF)
C-----------------------------------------------------------------------
C! closes a task: restarting AIPS, settling the accounting, issuing msg
C# System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2005
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
C   DIETSK must be called at the end of each task as the last real
C   statement before the final RETURNs and STOP statement.  It issues a
C   closing message, terminates the accounting, and, if RQUICK is false,
C   restarts the initiating AIPS program.
C   Inputs:
C      IRET     I         0 => ok, else bad end
C      RQUICK   L         T => initiator already resumed
C   Output:
C      IBUF     I(256)    Scratch buffer
C-----------------------------------------------------------------------
      INTEGER   IRET, IBUF(256)
      LOGICAL   RQUICK
C
      INTEGER   IERR
      LONGINT   LTEMP
      CHARACTER OPT*4
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Message
      IF (IRET.NE.0) THEN
         MSGTXT = 'Purports to die of UNNATURAL causes'
      ELSE
         MSGTXT = 'Appears to have ended successfully'
         END IF
      CALL MSGWRT (3)
C                                       Clean out TC
      CALL GTTELL (-1, OPT, IBUF, IBUF, IERR)
C                                       clean out dynamic memory
      CALL ZMEMRY ('QUIT', TSKNAM, IERR, IERR, LTEMP, IERR)
C                                       Close accounting
      CALL ACOUNT (2)
C                                       Resume AIPS
      IF (.NOT.RQUICK) CALL RELPOP (IRET, IBUF, IERR)
C                                       System close downs ?
      CALL ZDIE
C
 999  RETURN
      END
