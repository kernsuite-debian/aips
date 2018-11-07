      SUBROUTINE ZSHCM2 (LCOM, COMMND, LTASK, OUTDEV, IERR)
C-----------------------------------------------------------------------
C! execute a job-control level command string
C# Z3 System
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
C   Execute a "shell" (DCL or sh) command string below present task.
C   Inputs:
C      LCOM     I       Length of commnd
C      COMMND   C*(*)   The command line string
C      LTASK    I       Length of taskout
C      OUTDEV   C*(*)   File/device for standard output/error
C   Output:
C      IERR     I       Error return: 0 means okay
C                          1 input command too long
C                          2 some system error occurred
C   VMS version - true CHARACTER inputs.
C-----------------------------------------------------------------------
      INTEGER   LCOM, LTASK, IERR
      CHARACTER COMMND*(*), OUTDEV*(*)
C
      INTEGER   STATUS, LIB$SPAWN, PRCERR, I, ITRIM, JERR, ZAND
      CHARACTER PRCNAM*11, CTEMP*1, ZTEMP*8
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL ZEHEX (NPOPS, 1, CTEMP)
      I = ITRIM (TSKNAM)
      PRCNAM = TSKNAM(:I) // CTEMP // '_SUB'
      I = I + 5
      STATUS = LIB$SPAWN (COMMND, , OUTDEV, , PRCNAM(:I), , PRCERR, , ,
     *   , , )
      IERR = 0
      IF (STATUS.NE.1) THEN
         CALL ZHEX (STATUS, 8, ZTEMP)
         WRITE (MSGTXT,1010) ZTEMP
         CALL MSGWRT (8)
         IERR = 64
         END IF
      JERR = ZAND (PRCERR, 1)
      IF (JERR.NE.1) THEN
         CALL ZHEX (PRCERR, 8, ZTEMP)
         WRITE (MSGTXT,1020) ZTEMP
         CALL MSGWRT (8)
         IERR = IERR + 32
         CALL ZERROR ('ZSHCMD', PRCERR, ' ', -999, .FALSE.)
         END IF
C
 990  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('FAILURE: SPAWN STATUS=',A)
 1020 FORMAT ('SPAWNED PROCESS STATUS RETURNED=',A,' (NORMAL=1)')
      END
