      SUBROUTINE ZTABOR (NAME, PID, IERR)
C-----------------------------------------------------------------------
C! deletes (or kills) the specified process
C# System Z2
C-----------------------------------------------------------------------
C;  Copyright (C) 2004
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
C   ZTABOR will delete/kill the process specified by NAME.
C   Inputs:
C      NAME   C*6    actual task name
C      PID    I(4)   Process "ID" code:
C                       PID(1) user number  0 => any user
C                       PID(2-4) process #  0 => unknown
C   Output:
C      IERR   I      Error number: 0 => ok.
C                       1 => error.
C   Generic version - calls ZTKILL
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   PID(4), IERR
C
      HOLLERITH HNAME(2)
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (SYSTYP.EQ.'VMS') THEN
         CALL ZTKILL (NAME, PID, IERR)
      ELSE
         CALL CHR2H (6, NAME, 1, HNAME)
         CALL ZTKILL (HNAME, PID, IERR)
         END IF
C
 999  RETURN
      END
