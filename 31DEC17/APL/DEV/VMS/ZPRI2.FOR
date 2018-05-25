      SUBROUTINE ZPRI2 (OP, SYSERR, IERR)
C-----------------------------------------------------------------------
C! raise or lower the process priority
C# Z2 System
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
C   ZPRIO changes the current program's machine priority between that of
C   batch programs and that of interactive programs.
C   Inputs:
C      OP    C*4    'UPPP' to inter., 'DOWN' to batch
C   Outputs:
C      SYSERR   I     System-level error code
C      IERR     I     Error code: 0 => ok
C                        2 => illegal request
C                        3 => other failures
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   SYSERR, IERR
C
      INTEGER   JERR, SYS$SETPRI, OLDPRI, PRIO
C-----------------------------------------------------------------------
      IF (OP.EQ.'UPPP') THEN
         PRIO = 4
      ELSE
         PRIO = 3
         END IF
      JERR = SYS$SETPRI ( , , %VAL(PRIO), OLDPRI)
      IF (JERR.EQ.1) THEN
         IERR = 0
         SYSERR = 0
      ELSE
         IERR = 3
         SYSERR = JERR
         END IF
C
 999  RETURN
      END
