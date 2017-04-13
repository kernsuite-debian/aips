      SUBROUTINE TASKWT (ANAME, IPOPS, PID, DOWAIT, TDL, SCRTCH,
     *   IRETCD, IERR)
C-----------------------------------------------------------------------
C! waits for tasks to begin, send resumption signal, and/or terminate
C# POPS-appl Tasks
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2007
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
C   TASKWT performs the AIPS standard things to be done after a ZACTV8
C   call:  (1) waits up to 200 * TDL for the job to begin (and zero
C   the first word in the TD area for the NPOPS number) (2) waits for a
C   resumption signal from the task (via the 3rd word in the TD area)
C   (3) waits for the task to quit (if DOWAIT true).
C   Inputs:
C      ANAME   C*6      Program name
C      IPOPS   I         POPS number used
C      PID     I(4)      Process ID used by ZTACTQ
C      DOWAIT  L         T => wait for completion
C      TDL     R         time delay in loops (seconds)
C   Outputs:
C      SCRTCH  I(256)    Scratch buffer
C      IRETCD  I         Task return code
C      IERR    I         Subroutine error code: 0 => OK
C                        1 => task starts + vanishes without resuming me
C                        2 => task fails to start in time
C                        3 => disk error
C-----------------------------------------------------------------------
      CHARACTER ANAME*6
      INTEGER   IPOPS, PID(4), SCRTCH(256), IRETCD, IERR
      LOGICAL   DOWAIT
      REAL      TDL
C
      CHARACTER PHNAME*48
      INTEGER   LUNTD, FINDTD, NLOOP, MLOOP, NFAIL, MSGSAV
      LOGICAL   T, F, ACTIVE
      REAL      ATDL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNTD /13/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      NFAIL = 0
      MSGSAV = MSGSUP
      CALL ZPHFIL ('TD', 1, 0, 4, PHNAME, IERR)
      ATDL = MAX (0.05, TDL)
C                                       Wait for resumption through
C                                       TD file or abort of task
      IRETCD = -999
      NLOOP = 0
      MLOOP = 15.0 / ATDL
      MLOOP = MAX (25, MLOOP)
C                                       wait for start
 10   CALL ZDELAY (ATDL, IERR)
         NLOOP = NLOOP + 1
         IF (NFAIL.LE.3) MSGSUP = 32000
         CALL ZOPEN (LUNTD, FINDTD, 1, PHNAME, F, T, T, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.0) THEN
            CALL ZFIO ('READ', LUNTD, FINDTD, 1, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 960
            CALL ZCLOSE (LUNTD, FINDTD, IERR)
            IF (SCRTCH(5*IPOPS-4).NE.0) THEN
               IF (NLOOP.GT.MLOOP) GO TO 900
               GO TO 10
               END IF
            IRETCD = SCRTCH(5*IPOPS-2)
            NLOOP = 0
            NFAIL = 0
         ELSE
            NFAIL = NFAIL + 1
            IF (NFAIL.LE.5) GO TO 10
            GO TO 980
            END IF
C                                       wait for return code
 20   CALL ZDELAY (ATDL, IERR)
C                                        read return code
         IF (IRETCD.EQ.-999) THEN
            IF (NFAIL.LE.3) MSGSUP = 32000
            CALL ZOPEN (LUNTD, FINDTD, 1, PHNAME, F, T, T, IERR)
            MSGSUP = MSGSAV
            IF (IERR.NE.0) THEN
               NFAIL = NFAIL + 1
               IF (NFAIL.GT.5) GO TO 980
            ELSE
               CALL ZFIO ('READ', LUNTD, FINDTD, 1, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 960
               CALL ZCLOSE (LUNTD, FINDTD, IERR)
               IRETCD = SCRTCH(5*IPOPS-2)
               NFAIL = 0
               END IF
            END IF
C                                       Task still running ?
 30      CALL ZTACTQ (ANAME, PID, ACTIVE, IERR)
         IF ((IERR.NE.0) .OR. (.NOT.ACTIVE)) GO TO 40
C                                       Did we wait long enough?
            IF ((DOWAIT) .OR. (IRETCD.EQ.-999)) GO TO 20
C                                       ABORT not resume via TD
 40   IF (IRETCD.NE.-999) GO TO 50
C                                       1 extra delay for timing
         ATDL = MIN (1.2, ATDL)
         NLOOP = NLOOP + 1
         IF (NLOOP.EQ.1) GO TO 20
C                                       error
         WRITE (MSGTXT,1040)
         CALL MSGWRT (8)
         IERR = 1
         GO TO 999
C                                       ok
 50   IERR = 0
      GO TO 999
C                                       not start
 900  ATDL = ATDL * NLOOP
      WRITE (MSGTXT,1900) ATDL
      CALL MSGWRT (8)
      IERR = 2
      GO TO 999
C                                       disk error
 960  CALL ZCLOSE (LUNTD, FINDTD, IERR)
 980  IERR = 3
      WRITE (MSGTXT,1980)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('TASK QUIT WITHOUT RESUMING ME')
 1900 FORMAT ('TASK HAS NOT BEGUN IN',F7.1,' SECONDS')
 1980 FORMAT ('DISK ERROR IN TD FILE WHILE WAITING ON TASK')
      END
