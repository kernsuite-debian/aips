      SUBROUTINE ZTACT2 (ILEN, NAME, PID, IERR)
C-----------------------------------------------------------------------
C! inquires if a task is currently active on the local computer
C# Z2 System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   ZTACTQ/ZTACT2 determines if a specified task is active.
C   Inputs:
C      ILEN   I      length used in NAME (for C or Macro...)
C      NAME   C*6    Actual task name - CHARACTER for VMS, HOLLERITH
C                    for all other systems
C   In/out:
C      PID    I(3)   Process "ID" code
C                    In:  PID(1-3) process #  0 => unknown
C                    Out: PID(1-3) the ID determined here from NAME
C   Output:
C      IERR   I      Error return code: 0 => ok, task active
C                       1 => task not active
C                       2 => invalid task name.
C                       3 => other
C   Generic version - stubbed for local development.
C-----------------------------------------------------------------------
      CHARACTER NAME*6
      INTEGER   ILEN, PID(3), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 3
      MSGTXT = 'ZTACT2: REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (8)
C
 999  RETURN
      END
