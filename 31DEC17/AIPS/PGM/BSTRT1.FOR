      PROGRAM BSTRT1
C-----------------------------------------------------------------------
C! BSRT1 starts all AIPS batch queues.
C# Batch Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   BSTRT1 starts all AIPS batch queues.  It should be run whenever
C   the system is booted
C-----------------------------------------------------------------------
      CHARACTER ANAME*6, AIPVER*48, PHNAME*48, PRGNAM*6
      INTEGER  BUFFER(256), IP, OUPOPS, IERR, IER, TDLUN, TDIND,
     *   IRETCD, ITEMP, IPTR, PID(4), IREC
      REAL      RBUFF(256), TDL
      LOGICAL   T, F, ACTIVE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RBUFF, BUFFER)
      DATA PRGNAM /'BSTRT '/
      DATA TDLUN /13/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       AIPS init
      TSKNAM = PRGNAM
      CALL ZDCHIN (.TRUE.)
      ISBTCH = 32000
      CALL ZMYVER
      CALL ACOUNT (1)
      IF (NBATQS.GT.0) GO TO 10
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
         GO TO 990
C                                        start QMNGR
 10   OUPOPS = NINTRN + 2
      CALL BLDTNM ('QMNGR', OUPOPS, ANAME, IERR)
      CALL FILL (4, 0, PID)
      CALL ZTACTQ (ANAME, PID, ACTIVE, IERR)
      IF ((IERR.EQ.0) .AND. (.NOT.ACTIVE)) GO TO 20
         WRITE (MSGTXT,1010) IERR
         IF (ACTIVE) WRITE (MSGTXT,1011) OUPOPS
         CALL MSGWRT (8)
         GO TO 100
C                                        open task data file: use
C                                        the batch guy's NPOPS area
 20   CALL ZPHFIL ('TD', 1, 0, 0, PHNAME, IERR)
      CALL ZOPEN (TDLUN, TDIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         GO TO 100
C                                        Parms data
 30   CALL FILL (256, 0, BUFFER)
C                                       user, dowait, tv, tk #'s
      BUFFER(1) = NLUSER
      BUFFER(2) = 0
      BUFFER(3) = 0
      BUFFER(4) = 0
      BUFFER(5) = ISBTCH
      BUFFER(6) = 0
      BUFFER(7) = 0
      BUFFER(8) = 0
      IPTR = 11
      RBUFF(IPTR-2) = -1.0
      CALL CHR2H (4, VERNAM, 1, RBUFF(IPTR-1))
C                                      Adverb: init's NPOPS
      RBUFF(IPTR) = NPOPS
      IREC = (OUPOPS-1)*3 + 2
      CALL ZFIO ('WRIT', TDLUN, TDIND, IREC, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 40
 35      WRITE (MSGTXT,1035) IERR
         CALL MSGWRT (8)
         GO TO 45
 40   CALL ZFIO ('READ', TDLUN, TDIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 35
      ITEMP = (OUPOPS-1)*5 + 1
      CALL CHR2H (6, 'QMNGR ', 1, BUFFER(ITEMP))
      IRETCD = -999
      BUFFER(ITEMP+2) = IRETCD
      BUFFER(ITEMP+3) = 0
      BUFFER(ITEMP+4) = 0
      CALL ZFIO ('WRIT', TDLUN, TDIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 35
 45   CALL ZCLOSE (TDLUN, TDIND, IER)
      IF (IERR.NE.0) GO TO 100
C                                        activate it finally
 50   WRITE (MSGTXT,1050) IP, OUPOPS
      CALL MSGWRT (2)
      AIPVER = VERNAM(1:3)
      CALL ZACTV8 ('QMNGR', OUPOPS, AIPVER, PID, IERR)
      IF (IERR.EQ.0) GO TO 60
         WRITE (MSGTXT,1051) IERR
         CALL MSGWRT (8)
         GO TO 100
C                                        suspend until QMNGR ok
 60   TDL = 3.0
      CALL TASKWT (ANAME, OUPOPS, PID, F, TDL, BUFFER, IRETCD, IERR)
C                                       ABORT not resume via TD
 75   IF ((IRETCD.NE.-999) .AND. (IERR.EQ.0)) GO TO 90
         WRITE (MSGTXT,1075)
         CALL MSGWRT (8)
         GO TO 100
C                                       Okay.
 90   CONTINUE
         WRITE (MSGTXT,1090)
         CALL MSGWRT (2)
 100  CONTINUE
C                                       Close down
 990  CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('There are no batch queues to start')
 1010 FORMAT ('ZTACTQ ERROR',I7)
 1011 FORMAT ('Qmngr number',I4,' already running')
 1020 FORMAT ('TASK DATA FILE OPEN ERROR =',I7)
 1035 FORMAT ('TASK DATA FILE I/O ERROR =',I7)
 1050 FORMAT ('Starting queue',I3,' as POPS number',I4)
 1051 FORMAT ('ZACTV8 ERROR =',I7)
 1075 FORMAT ('Qmngr vanished without normal restart')
 1090 FORMAT ('OK')
      END
