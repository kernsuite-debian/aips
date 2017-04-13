      SUBROUTINE RELPOP (RETCOD, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! places a return code in the task data file, thereby resuming AIPS
C# System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000
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
C   RELPOP places the specified return code in the appropriate location
C   of the first record of the Task Data file.  This will allow the
C   calling program (AIPS, AIPSC, AIPSB, BATER) to resume normal
C   operations.
C   Inputs:
C      RETCOD  I       return code number
C   Outputs:
C      SCRTCH  I(256)  scratch buffer
C      IERR    I       error number: 0 -> ok
C                                  1,2 -> task not resumed
C                                    3 -> NPOPS out of range
C                                    4 -> parameter not passed
C-----------------------------------------------------------------------
      INTEGER   RETCOD, IERR, SCRTCH(256)
C
      CHARACTER PHNAME*48
      INTEGER   NAX, IE, IER, IFIND, LUNTD
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUNTD /13/
C-----------------------------------------------------------------------
      NAX = NINTRN
      IF (NBATQS.GT.0) NAX = NINTRN + 1 + NBATQS
      IERR = 3
      IF ((NPOPS.LT.1) .OR. (NPOPS.GT.NAX)) GO TO 900
C                                       write retcode on disk
      CALL ZPHFIL ('TD', 1, 0, 4, PHNAME, IER)
      IERR = 1
      CALL ZOPEN (LUNTD, IFIND, 1, PHNAME, F, T, T, IER)
      IF (IER.NE.0) GO TO 20
         IERR = 2
         CALL ZFIO ('READ', LUNTD, IFIND, 1, SCRTCH, IER)
         IF (IER.NE.0) GO TO 10
            SCRTCH(5*NPOPS-2) = RETCOD
            IF (RETCOD.LT.-100) SCRTCH(5*NPOPS-2) = -100
            CALL ZFIO ('WRIT', LUNTD, IFIND, 1, SCRTCH, IER)
 10      CALL ZCLOSE (LUNTD, IFIND, IE)
         IF (IER.EQ.0) IERR = 0
         IF (IER.EQ.0) GO TO 999
C                                       Error in pass back
 20   WRITE (MSGTXT,1020) NPOPS, IER
      CALL MSGWRT (8)
      IERR = 1
      GO TO 999
C                                       Errors: no resume
 900  WRITE (MSGTXT,1900) NPOPS
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('FAILS TO SEND RETURN CODE TO POPS',I3,' IO ERROR',I5)
 1900 FORMAT ('NPOPS',I3,': OUT OF RANGE')
      END
