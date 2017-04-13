      SUBROUTINE ZTACTQ (NAME, PID, ACTIVE, IERR)
C-----------------------------------------------------------------------
C! inquires if a task is currently active on the local computer
C# Z System
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
C   ZTACTQ determines if a specified task is active.
C   Inputs:
C      NAME     C*6    Actual task name.
C   In/out:
C      PID      I(4)   Process "ID" code
C                      In:  PID(1) user number  0 => any
C                           PID(2-4) process #  0 => unknown
C                      Out: PID(1) not changed
C                           PID(2-4) the ID determined here from NAME
C   Output:
C      ACTIVE   L      Task active indicator (T => active)
C      IERR     I      Error return code: 0 => ok.
C                         2 => invalid task name.
C                         3 => other
C   Generic version - calls ZTACT2 w CHARACTER for VMS, HOLLERITH other
C-----------------------------------------------------------------------
      CHARACTER NAME*6
      INTEGER   PID(4), IERR
      LOGICAL   ACTIVE
C
      INTEGER   JERR, IL, ITRIM
      HOLLERITH HNAME(2)
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IL = ITRIM (NAME)
      IF (SYSTYP.EQ.'VMS') THEN
         CALL ZTACT2 (IL, NAME, PID(2), JERR)
      ELSE
         CALL CHR2H (IL, NAME, 1, HNAME)
         CALL ZTACT2 (IL, HNAME, PID(2), JERR)
         END IF
      ACTIVE = JERR.EQ.0
      IERR = 0
      IF (JERR.GT.1) IERR = JERR
C
 999  RETURN
      END
