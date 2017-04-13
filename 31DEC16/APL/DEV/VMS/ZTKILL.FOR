      SUBROUTINE ZTKILL (NAME, PID, IERR)
C-----------------------------------------------------------------------
C! deletes (or kills) the specified process
C# Z System
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
C   ZTKILL will delete/kill the process specified by NAME.
C   Inputs:
C      NAME   C*6    actual task name
C      PID    I(4)   Process "ID" code:
C                       PID(1) user number  0 => any user
C                       PID(2-4) process #  0 => unknown
C   Output:
C      IERR   I      Error number: 0 => ok.
C                       1 => error.
C   VAX Version: PID(2) now used if not 0
C-----------------------------------------------------------------------
      CHARACTER NAME*6
      INTEGER   PID(4), IERR
C
      INTEGER   JERR, SYS$FORCEX, IL, ITRIM
      CHARACTER DNAME*6
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IL = ITRIM (NAME)
C                                       Force exit of subprocess.
      IF (PID(2).GT.0) THEN
         JERR = SYS$FORCEX (PID(2), DNAME(1:IL), )
      ELSE
         JERR = SYS$FORCEX ( , DNAME(1:IL), )
         END IF
C                                       Print system error msg.
      IF (JERR.NE.1) THEN
         CALL ZERROR ('ZTKILL', JERR, ' ', -999, .FALSE.)
         IERR = 1
         END IF
C
 999  RETURN
      END
