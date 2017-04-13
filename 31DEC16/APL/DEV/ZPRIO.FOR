      SUBROUTINE ZPRIO (OP, IERR)
C-----------------------------------------------------------------------
C! raise or lower the process priority
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
C   Change the execution priority of the current process between that of
C   batch and interactive processes.
C   Inputs:
C      OP     C*4   'UPPP' to interactive, 'DOWN' to batch
C      IERR   I     Error return code: 0 => no error
C                      1 => invalid opcode
C                      2 => illegal request
C                      3 => other
C   Generic version - uses ZPRI2 to do the real work.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      INTEGER   SYSERR
      LOGICAL   F
      HOLLERITH HOP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IERR = 1
      IF ((OP.NE.'UPPP') .AND. (OP.NE.'DOWN')) THEN
         WRITE (MSGTXT,1000) OP
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Change execution priority.
      IF (SYSTYP.EQ.'VMS') THEN
         CALL ZPRI2 (OP, SYSERR, IERR)
      ELSE
         CALL CHR2H (4, OP, 1, HOP)
         CALL ZPRI2 (HOP, SYSERR, IERR)
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, OP
         CALL MSGWRT (7)
         CALL ZERROR ('ZPRI2', SYSERR, ' ', -999, F)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZPRIO: INVALID OPERATION = ''',A4,'''')
 1010 FORMAT ('ZPRIO: ZPRI2 ERROR',I3,' FOR OP = ''',A4,'''')
      END
