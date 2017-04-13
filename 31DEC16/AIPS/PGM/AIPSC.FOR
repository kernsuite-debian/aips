LOCAL INCLUDE 'AIPSC.INC'
      REAL      QUEUES, DETIME, RQUEUE, NAPTSK
      HOLLERITH AIPVER(12), XRHOST(6)
      INTEGER   OUPOPS, LINTRN, LBATQS
      COMMON /INPARM/ QUEUES, DETIME, AIPVER, XRHOST, RQUEUE,
     *   NAPTSK, OUPOPS, LINTRN, LBATQS
      CHARACTER RHOST*24, VERSON*48
      COMMON /INPARC/ RHOST, VERSON
LOCAL END
      PROGRAM AIPSC
C-----------------------------------------------------------------------
C! checks POPS language parts of submitted batch jobs and queues them
C# Batch POPS-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2005, 2007, 2009-2013, 2016
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
C   AIPSC is a special version of batch AIPS.  It is activated by
C   AIPS or BATER.  It reads input from the batch work files
C   attached to these tasks and writes them to batch files to be
C   executed by AIPSBn.  As it does so, AIPSC compiles the input
C   and executes the quick verbs to test for errors.  The job ends
C   up queued for batch execution only if no errors are detected.
C   AIPSC is the only task to activate the QMNGR.
C   Input adverbs from task data file:
C       NQUEUE      Queue number.
C       DETIME      Requested delay time (hours) before starting job.
C       VERSON      Desired version of batch processor AIPSB.
C   and of course NLUSER, IWAIT, and NPOPS.  Uses NPOPS = same as that
C   of interactive AIPS or BATER that started it.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   JOBNO, IERR, NQUEUE
      LOGICAL   RQUICK
      INCLUDE 'AIPSC.INC'
      INCLUDE 'INCS:DBAT.INC'
      INCLUDE 'INCS:DBWT.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DCON.INC'
      DATA PRGNAM /'AIPSC '/
C-----------------------------------------------------------------------
C                                        initialize
      CALL CHSTRT (JOBNO, NQUEUE, RQUICK, IERR)
      IF (WASERR) GO TO 30
      STORE1 = 0
C                                        usual AIPS no slow verbs
 10   CALL INIT
      IF (ERRNUM.EQ.0) GO TO 20
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         CALL OERROR
         ERRNUM = 100
         CALL OERROR
         WASERR = .TRUE.
         GO TO 30
C                                        process input until error
 20   CALL GTLINC
      IF (ERRNUM.LE.0) GO TO 25
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         CALL OERROR
         WASERR = .TRUE.
C                                        loop anyway
 25   IF (ERRNUM.EQ.-1) GO TO 10
      IF (ERRNUM.NE.-2) GO TO 20
      IF (.NOT.WASERR) IERR = 0
C                                        close down
 30   CALL CHSTOP (JOBNO, NQUEUE, RQUICK, IERR)
C
 999  STOP
      END
      SUBROUTINE CHSTRT (JOBNO, NQUEUE, RQUICK, IER)
C-----------------------------------------------------------------------
C   CHSTRT performs start up operations for the Checker version of
C   AIPS (AIPSC).  The routine does not clean up after itself on error,
C   but returns a pointer to let CHSTOP do it.
C   Outputs:
C      JOBNO   I    Job number assigned
C      NQUEUE  I    Queue number to use
C      RQUICK  L    Restart initiator quickly ?
C      IER     I    error code => clean up level needed
C-----------------------------------------------------------------------
      INTEGER   JOBNO, NQUEUE, IER
      LOGICAL   RQUICK
C
      CHARACTER PRGNAM*6, FILE*64, PHNAME*48
      INTEGER   INHUSE(3), BWTDDD(4), IERR, IER2, NPARM, NRECS, NLPR,
     *   ISIZE, LSIZE, J, JTRIM, IQUEUE
      LOGICAL   T, F
      REAL      EPS
      INCLUDE 'AIPSC.INC'
      INCLUDE 'INCS:DBAT.INC'
      INCLUDE 'INCS:DBWT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DRUN.INC'
      EQUIVALENCE (BWTDDD(1), BWTDAT(1))
      DATA EPS /0.01/
      DATA PRGNAM /'AIPSC '/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IER = 5
      JOBNO = 0
      WASERR = .TRUE.
      TSKNAM = PRGNAM
      CALL ZDCHIN (.TRUE.)
      NPARM = 21
      CALL GTPARM (PRGNAM, NPARM, RQUICK, QUEUES, BATDAT, IERR)
      IF (RQUICK) CALL RELPOP (IERR, BATDAT, IER2)
      IF (IERR.NE.0) GO TO 999
      CALL H2CHR (24, 1, XRHOST, RHOST)
      CALL H2CHR (48, 1, AIPVER, VERSON)
      IF (VERSON.EQ.' ') VERSON = VERNAM(1:3)
C                                        get POPS numbers
      DETIME = MAX (0.016666, MIN (480.0, DETIME))
      NAPTSK = 0.0
      IF (RHOST.EQ.' ') THEN
         LINTRN = NINTRN
         LBATQS = NBATQS
         RQUEUE = QUEUES
      ELSE
         IF (RQUEUE.LT.0.5) RQUEUE = QUEUES
         J = JTRIM (RHOST)
         CALL ZPHFIL ('SP', 1, 0, 0, PHNAME, IERR)
         FILE = 'NET0:' // RHOST(:J) // '/' // PHNAME(6:)
         BATLUN = 4
         CALL ZOPEN (BATLUN, BATIND, 1, FILE, F, T, T, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN REMOTE SP FILE'
            GO TO 990
            END IF
         CALL ZFIO ('READ', BATLUN, BATIND, 1, BATDAT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ REMOTE SP FILE'
            GO TO 990
            END IF
         LINTRN = BATDAT(10)
         LBATQS = BATDAT(5)
         CALL ZCLOSE (BATLUN, BATIND, IERR)
         END IF
      IQUEUE = QUEUES + 0.5
      NQUEUE = RQUEUE + 0.5
      OUPOPS = NQUEUE + LINTRN + 1
      IF ((NQUEUE.LT.1) .OR. (NQUEUE.GT.LBATQS)) THEN
         WRITE (MSGTXT,1005) NQUEUE, LBATQS
         GO TO 990
         END IF
      NUMRUN = 0
      CALL FILL (MAXRUN, 0, LUNRUN)
C                                        init IO numbers
      BATLUN = 4
      BATREC = 1
      BWTLUN = 26
      BWTREC = 1
C                                        get a job #
      CALL BATQ ('OPEN', DETIME, RHOST, NQUEUE, INHUSE, NLUSER, JOBNO,
     *   BATDAT, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.2) THEN
            WRITE (MSGTXT,1010) NQUEUE
         ELSE
            WRITE (MSGTXT,1011) IERR
            END IF
         GO TO 990
         END IF
C                                        open input file
      CALL ZPHFIL ('BA', 1, IQUEUE, NPOPS, BWTNAM, IERR)
      CALL ZOPEN (BATLUN, BATIND, 1, BWTNAM, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR
         IER = 4
         GO TO 990
         END IF
C                                        get size new file
      CALL ZFIO ('READ', BATLUN, BATIND, 1, BATDAT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         IER = 3
         GO TO 990
         END IF
      NRECS = BATDAT(2) - 1
      IF (NRECS.LE.1) THEN
         IER = 3
         NRECS = NRECS - 1
         WRITE (MSGTXT,1025) NRECS
         GO TO 990
         END IF
      NLPR = 252 / 22
      ISIZE = (NRECS-1) / NLPR + 2
C                                        create new file
      CALL ZPHFIL ('BA', 1, JOBNO, OUPOPS, BWTNAM, IERR)
      IF (RHOST.NE.' ') THEN
         J = JTRIM (RHOST)
         BWTNAM = 'NET0:' // RHOST(:J) // '/' // BWTNAM(6:)
         END IF
 32   CALL ZCREAT (1, BWTNAM, ISIZE, F, LSIZE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         IER = 3
C                                       destroy pre-existing
         IF (IERR.EQ.1) THEN
            WRITE (MSGTXT,1031)
            CALL MSGWRT (6)
            CALL ZDESTR (1, BWTNAM, IERR)
            IF (IERR.LE.1) GO TO 32
            WRITE (MSGTXT,1032) IERR
            END IF
         GO TO 990
         END IF
C                                        open it
      BWTNUM = LSIZE
      CALL ZOPEN (BWTLUN, BWTIND, 1, BWTNAM, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         IER = 2
         GO TO 990
         END IF
C                                        no error
      IER = 1
      WASERR = .FALSE.
C                                        global output pointers
      CALL FILL (256, 0, BWTDAT)
      BWTDDD(1) = BATDAT(1)
      BWTDDD(2) = BATDAT(2)
      BWTDDD(3) = BATDAT(2)-1
      BWTDDD(4) = BWTNUM
C                                        Force no TV, TK
      NTVDEV = 0
      NTKDEV = 0
      GO TO 999
C                                        error message
 990  CALL MSGWRT (9)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I4,' ON ',A)
 1005 FORMAT ('QUEUE',I5,' OUT OF RANGE 1 -',I3,' ALLOWED')
 1010 FORMAT ('QUEUE',I4,' FULL......SORRY')
 1011 FORMAT ('QUEUE OPEN ERROR',I7)
 1015 FORMAT ('INPUT FILE OPEN ERROR',I7)
 1020 FORMAT ('INPUT FILE READ ERROR',I7)
 1025 FORMAT ('INPUT FILE EMPTY: NRECS=',I7)
 1030 FORMAT ('BATCH FILE CREATE ERROR',I7)
 1031 FORMAT ('DESTROY UNKNOWN OLD FILE')
 1032 FORMAT ('DESTROY FAILED',I6,' CALL AIPS MANAGER')
 1035 FORMAT ('BATCH FILE OPEN ERROR',I7)
      END
      SUBROUTINE CHSTOP (JOBNO, NQUEUE, RQUICK, IER)
C-----------------------------------------------------------------------
C   CHSTOP does all the close down operations required by AIPSC.
C   It closes files, activates/deletes new batch job, activates
C   QMNGR if needed, and resumes the initiator.
C   Inputs:
C      JOBNO   I     job number
C      NQUEUE  I     queue number
C      RQUICK  L     if F then CHSTOP resumes initiator
C      IER     I     where to start closing up (>0 => error)
C-----------------------------------------------------------------------
      INTEGER   JOBNO, NQUEUE, IER
      LOGICAL   RQUICK
C
      CHARACTER QMNGR*8, ANAME*6, OP*4, PHNAME*48, COMAND*256
      INTEGER   JERR, IERR, INHUSE(3), TDLUN, TDIND, INUM, JER,
     *   BWTDDD(4), ID, ITPOPS, NLPR, ITEMP, PID(4), IRETCD, IRET, IREC,
     *   NBLOCK, I, J, JTRIM
      REAL      RBTDAT(128), TTIME, TCUTOF(2,2), TDL
      DOUBLE PRECISION    JD, JD0
      LOGICAL   ACTIVE, T, F
      INCLUDE 'AIPSC.INC'
      INCLUDE 'INCS:DBAT.INC'
      INCLUDE 'INCS:DBWT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      EQUIVALENCE  (BATDAT(1), RBTDAT(1)),    (BWTDAT(1), BWTDDD(1))
      DATA T, F /.TRUE.,.FALSE./
      DATA TDLUN /13/
      DATA JD0 /2445076.5D0/
      DATA QMNGR /'QMNGR'/
C-----------------------------------------------------------------------
      NBLOCK = 4
      IRET = 16
C                                       These times limit batch AP use:
      CALL RCOPY (4, TIMEBA, TCUTOF)
      NLPR = 252 / 22
C                                        BRANCH to first needed close
      JERR = IER + 1
      IF ((JERR.LT.1) .OR. (JERR.GT.6)) GO TO 10
      GO TO (10, 100, 120, 140, 160, 180), JERR
C                                        went thru AIPS ok !!!!
 10   IF (WASERR) GO TO 100
C                                        write last record
         IREC = (BWTREC-1) / NLPR + 1
         IF (IREC.GT.1) THEN
            IF (IREC.GT.BWTNUM) THEN
               INUM = 1
               CALL ZEXPND (BWTLUN, 1, BWTNAM, INUM, IERR)
               IF (IERR.NE.0) GO TO 95
               BWTNUM = BWTNUM + INUM
               END IF
            CALL ZFIO ('WRIT', BWTLUN, BWTIND, IREC, BWTDAT, IERR)
            IF (IERR.NE.0) GO TO 95
C                                        update first record pointers
            CALL ZFIO ('READ', BWTLUN, BWTIND, 1, BWTDAT, IERR)
            IF (IERR.NE.0) GO TO 95
            END IF
         BWTDDD(3) = BWTREC
         BWTDDD(2) = BWTREC+1
         BWTDDD(4) = BWTNUM
         CALL ZFIO ('WRIT', BWTLUN, BWTIND, 1, BWTDAT, IERR)
         IF (IERR.NE.0) GO TO 95
         TTIME = 0.0
C                                       AP tasks are a problem
C                                       save old code, but allow
C                                       all tasks for now
C        IF (NAPTSK.GT.0.99) THEN
         IF (NAPTSK.GT.2000.99) THEN
C                                        Not allowed in Q # 1
            IF ((LBATQS.GT.1) .AND. (NQUEUE.EQ.1)) THEN
               WRITE (MSGTXT,1030)
               CALL MSGWRT (8)
               WASERR = T
               GO TO 100
C                                        Delayed otherwise
            ELSE
               CALL CATIME (1, BATDAT, BWTDAT)
               CALL DAT2JD (BWTDAT, JD)
               JD = JD - JD0
               ID = JD
               JD = 24.0 * (JD - ID)
               ID = MOD (ID, 7)
               IF ((ID.LT.2) .AND. (JD.GT.TCUTOF(1,1))) TTIME =
     *            TCUTOF(2,1) - JD
               IF ((ID.GE.2) .AND. (JD.GT.TCUTOF(1,2))) TTIME =
     *            TCUTOF(2,2) - JD
               TTIME = MAX (0.0, TTIME)
               IF ((TTIME.GT.0.10) .AND. (TTIME.GT.DETIME)) THEN
                  WRITE (MSGTXT,1035) TTIME
                  CALL MSGWRT (3)
                  END IF
               END IF
            END IF
 40      DETIME = MAX (TTIME, DETIME)
C                                        close new file & Q job
         CALL ZCLOSE (BWTLUN, BWTIND, IERR)
         CALL BATQ ('RUNN', DETIME, RHOST, NQUEUE, INHUSE, NLUSER,
     *      JOBNO, BWTDAT, IERR)
         IF (IERR.EQ.0) GO TO 140
            WASERR = T
            WRITE (MSGTXT,1040) IERR
            CALL MSGWRT (8)
            GO TO 120
C-----------------------------------------------------------------------
C                                       was error:
 95   WASERR = T
      WRITE (MSGTXT,1010) IERR
      CALL MSGWRT (8)
C                                       close new file
 100  CALL ZCLOSE (BWTLUN, BWTIND, IERR)
      IF (IERR.EQ.0) GO TO 120
         WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (7)
C                                        was error: destroy new file
 120  CALL ZPHFIL ('BA', 1, JOBNO, OUPOPS, PHNAME, IERR)
      IF (RHOST.NE.' ') THEN
         J = JTRIM (RHOST)
         PHNAME = 'NET0:' // RHOST(:J) // '/' // PHNAME(6:)
         END IF
      CALL ZDESTR (1, PHNAME, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1120) IERR
         CALL MSGWRT (7)
         END IF
C                                        error or no: close input
 140  CALL ZCLOSE (BATLUN, BATIND, IERR)
C                                        error: de-Q the job
 160  IF (WASERR) THEN
         CALL BATQ ('CLOS', DETIME, RHOST, NQUEUE, INHUSE, NLUSER,
     *      JOBNO, BATDAT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1160) IERR
            CALL MSGWRT (7)
            END IF
         END IF
C-----------------------------------------------------------------------
C                                        start QMNGR
 180  IF (.NOT.WASERR) THEN
         ITPOPS = LINTRN + 2
         IF (RHOST.EQ.' ') THEN
            CALL BLDTNM (QMNGR, ITPOPS, ANAME, IERR)
            CALL FILL (4, 0, PID)
            IRET = 0
            CALL ZTACTQ (ANAME, PID, ACTIVE, IERR)
            IF (ACTIVE) GO TO 250
            IRET = 1
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1180) IERR
               CALL MSGWRT (8)
               GO TO 250
               END IF
            END IF
C                                        open task data file: use
C                                        the batch guy's NPOPS area
         CALL ZPHFIL ('TD', 1, 0, 4, PHNAME, IERR)
         IF (RHOST.NE.' ') THEN
            J = JTRIM (RHOST)
            PHNAME = 'NET0:' // RHOST(:J) // '/' // PHNAME(6:)
            END IF
         CALL ZOPEN (TDLUN, TDIND, 1, PHNAME, F, T, T, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1185) IERR
            CALL MSGWRT (8)
            GO TO 250
            END IF
         CALL ZFIO ('READ', TDLUN, TDIND, 1, BATDAT, IERR)
         IF (IERR.NE.0) GO TO 205
         NBLOCK = MAX (4, BATDAT(256))
C                                        Parms data
         CALL FILL (256, 0, BATDAT)
         BATDAT(1) = NLUSER
         BATDAT(2) = 0
         BATDAT(3) = 0
         BATDAT(4) = 0
         BATDAT(5) = 32000
         BATDAT(6) = 0
         BATDAT(7) = 0
         BATDAT(8) = 0
         RBTDAT(9) = 1.0
         CALL CHR2H (4, VERNAM, 1, RBTDAT(10))
         RBTDAT(11) = NPOPS
         IREC = (ITPOPS-1)*NBLOCK + 2
         CALL ZFIO ('WRIT', TDLUN, TDIND, IREC, BATDAT, IERR)
         IF (IERR.NE.0) GO TO 205
         CALL ZFIO ('READ', TDLUN, TDIND, 1, BATDAT, IERR)
         IF (IERR.NE.0) GO TO 205
         IREC = IREC - 1
         CALL CHR2H (8, QMNGR, 1, BATDAT(IREC))
         IRETCD = -999
         BATDAT(IREC+2) = IRETCD
         BATDAT(IREC+3) = 0
         BATDAT(IREC+4) = 0
         BATDAT(256) = NBLOCK
         CALL ZFIO ('WRIT', TDLUN, TDIND, 1, BATDAT, IERR)
 205     CALL ZCLOSE (TDLUN, TDIND, JER)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1195) IERR
            CALL MSGWRT (8)
            GO TO 250
            END IF
C                                        activate it finally
         IF (RHOST.EQ.' ') THEN
            CALL ZACTV8 (QMNGR, ITPOPS, VERSON, PID, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1210) IERR
               CALL MSGWRT (8)
               GO TO 250
               END IF
C                                        suspend until QMNGR ok
            TDL = 3.0
            CALL TASKWT (ANAME, ITPOPS, PID, F, TDL, BATDAT, IRETCD,
     *         IERR)
C                                       ABORT not resume via TD
C                                       Okay.
            IF ((IRETCD.NE.-999) .AND. (IERR.EQ.0)) THEN
               IRET = 0
               WRITE (MSGTXT,1215)
               CALL MSGWRT (2)
               END IF
C                                       remote host
         ELSE
            J = JTRIM (RHOST)
            PHNAME = RHOST
            CALL CHUTOL (J, PHNAME)
            COMAND = 'ssh ' // PHNAME(:J) //
     *         ' $AIPS_ROOT/START_QMNGR &'
            PHNAME = '/dev/tty'
            J = JTRIM (COMAND)
            JER = JTRIM (PHNAME)
            CALL ZSHCMD (J, COMAND, JER, PHNAME, IERR)
            IF (IERR.EQ.0) IRET = 0
            END IF
         END IF
C-----------------------------------------------------------------------
C                                        last messages & print
 250  ITEMP = 100*NQUEUE + JOBNO
      IF (IRET.GT.1) THEN
         WRITE (MSGTXT,1250) ITEMP
      ELSE
         WRITE (MSGTXT,1251) ITEMP
         END IF
      CALL MSGWRT (10)
C                                       Destroy TS file if any
      J = 400 + NPOPS
      MSGSUP = 32000
      DO 255 I = 1,36
         CALL ZPHFIL ('TG', 1, J, I-1, PHNAME, IERR)
         CALL ZDESTR (1, PHNAME, IERR)
 255     CONTINUE
      MSGSUP = 0
      OP = 'DELE'
      IF (IRET.GT.1) OP = 'PRIN'
      JERR = NLUSER
      ERRNUM = 0
C                                       delete or print messages
      CALL PRTMSG (OP, JERR, NPOPS, 0, 'AIPSC ', 0.0, -1.0, ' ', ITEMP,
     *   INUM, IERR)
      CALL MSGWRT (-1)
C                                        restart initiator
      ITEMP = 0
      IF (IRET.GT.1) ITEMP = 16
      CALL DIETSK (ITEMP, RQUICK, BATDAT)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('BATCH FILE WRITE ERROR',I7)
 1030 FORMAT ('ARRAY PROCESSOR TASKS NOT ALLOWED IN BATCH QUEUE 1')
 1035 FORMAT ('Job delayed by',F5.1,' hours because of AP tasks')
 1040 FORMAT ('QUEUE RUN ERROR',I7)
 1100 FORMAT ('BATCH FILE CLOSE ERROR',I7)
 1120 FORMAT ('BATCH FILE DESTROY ERROR',I7)
 1160 FORMAT ('JOB DE-QUE ERROR',I7)
 1180 FORMAT ('JOB ACTIVE QUESTION ERROR',I7)
 1185 FORMAT ('TASK DATA FILE OPEN ERROR',I7)
 1195 FORMAT ('TASK DATA FILE IO ERROR',I7)
 1210 FORMAT ('QMNGR ACTIVATE ERROR',I7)
 1215 FORMAT ('Resumed by QMNGR normally')
 1250 FORMAT ('Job',I4,' canceled')
 1251 FORMAT ('Job',I4,' submitted')
      END
      SUBROUTINE CU2 (BRANCH)
C-----------------------------------------------------------------------
C   CU2 handles tests of tasking activities for AIPSC.
C   Input: BRANCH = 1 activate a task (named TASK in /CORE/)  GO
C                   5 Save task adverbs for later TGET.       TPUT
C                  10 Save task adverbs for later VGET.       VPUT
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INTEGER   NSYSAD, NSYSTA, NSYSDA, NSYSCH, NSYSIM
      PARAMETER (NSYSAD=29, NSYSTA=2, NSYSDA=9, NSYSCH=4, NSYSIM=11)
      CHARACTER PRGNAM*6, ITASK*8, ANAME*6, PHNAME*48, CTEST*8, CDUM*1,
     *   LVERSN*48, AVERSN*48, JBUF*80, SYSADV(NSYSAD)*8, CVER(4)*4
      INTEGER   POTERR, FINDIN, IVER, IPRBUF(256), IUSER, JTASK, NREC,
     *   I, IER, IERR, LUNTS, FINDTS, ITIME(6), NTASK, COLLBN, COLUBN,
     *   COLCOM, IEOF, JTRIM, ILOC, IPTR, ITAG, ITYPE, LUNIN, NBLOCK,
     *   VERTYP, NWPL, NLPR, NMATCC, ITEMP, IDUM, COLSHO, LPTR, LPTRMX,
     *   TSBLK, IREC, LREC, ISIZE, LSIZE
      REAL   PARBUF(256), LMIN, LMAX, LIM(2), RDUM, SYSLIM(2), DOWAIT
      HOLLERITH HARBUF(256)
      LOGICAL   T, F, WASDEF, ISQUES, ISAST
      INCLUDE 'AIPSC.INC'
      INCLUDE 'INCS:DAPT.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:PUVD.INC'
      EQUIVALENCE (IPRBUF(1), PARBUF(1), HARBUF(1))
      INCLUDE 'INCS:VAPT.INC'
      DATA PRGNAM /'AU2 '/
      DATA LMIN, LMAX /-1.E16, 1.E16/
      DATA COLSHO, COLLBN, COLUBN, COLCOM /10, 11, 23, 35/
      DATA LUNIN, LUNTS /11, 14/
      DATA IEOF /2/
      DATA T, F /.TRUE.,.FALSE./
      DATA SYSADV /'INTAPE  ', 'OUTTAPE ', 'INDISK  ', 'IN2DISK ',
     *   'IN3DISK ', 'IN4DISK ', 'DDISK', 'MDISK', 'TDISK', 'OUTDISK ',
     *   'OUT2DISK', 'BCHAN', 'ECHAN', 'CHANSEL', 'CHANNEL',
     *   'BLC', 'TRC', 'TBLC', 'TTRC', 'PIXXY', 'IMSIZE', 'BOX',
     *   'CLBOX', 'FLDSIZE', 'PIX2XY', 'UVSIZE',
     *   'BATQUE', 'NUMTELL', 'PRNUMBER'/
      DATA CVER /'OLD:','NEW:','TST:','CVX:'/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.10)) GO TO 999
      NBLOCK = 4
      CALL ADVERB ('TASK', 'C', 1, 8, IDUM, RDUM, ITASK)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('DOWAIT', 'R', 1, 0, IDUM, DOWAIT, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       get member name from stack
      IF (BRANCH.EQ.2) GO TO 90
         POTERR = 7
         IF (SP.LT.4) GO TO 980
         POTERR = 8
         IF ((STACK(SP).NE.2) .OR. (STACK(SP-3).NE.14) .OR.
     *      (STACK(SP-2).NE.8)) GO TO 980
         WASDEF = CH(STACK(SP-1)).EQ.HBLANK
         IF (.NOT.WASDEF) CALL H2CHR (8, 1, C(STACK(SP-1)), ITASK)
         SP = SP - 4
C                                       open inputs file
         CALL ZPHFIL ('HE', 1, 0, 0, PHNAME, IERR)
         POTERR = 101
C                                       Allow min match
C                                       Return true value
         CALL ADVERB ('VERSION', 'C', 1, 48, IDUM, RDUM, LVERSN)
         IF (ERRNUM.NE.0) GO TO 980
         CALL VERMAT (1, PHNAME, ITASK, LVERSN, VERTYP, AVERSN, IERROR)
         IF (IERROR(1).NE.0) GO TO 980
         IF (ITASK(:4).EQ.'AIPS') THEN
            WRITE (MSGTXT,1000)
            CALL MSGWRT (8)
            POTERR = 101
            GO TO 980
            END IF
C                                       Branch to operation
 90   GO TO (100, 300, 400, 500, 100, 500, 500, 500, 500, 100), BRANCH
C-----------------------------------------------------------------------
C                                       GO
C                                       start a task, save adverbs
C                                       TPUT
C                                       Just save the adverbs
C-----------------------------------------------------------------------
 100  IF (WASDEF) THEN
         CALL ADVRBS ('TASK', 'C', 1, 8, IDUM, RDUM, ITASK)
         IF (ERRNUM.NE.0) GO TO 980
         END IF
      CALL ZTOPEN (LUNIN, FINDIN, 1, PHNAME, ITASK, AVERSN, T, IERROR)
      POTERR = 31
      IF (IERROR(1).NE.0) GO TO 980
C                                       Check for AP-using tasks
      DO 105 I = 1,NAPNAM
         IF (ITASK.EQ.APTASK(I)) NAPTSK = NAPTSK + 1.0
 105     CONTINUE
C                                       Open Task adverb save file
      ISIZE = 32
      IUSER = 400 + NPOPS
      IF (BRANCH.EQ.10) THEN
         CALL ADVERB ('VNUMBER', 'I', 1, 1, IVER, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         IF ((IVER.LT.1) .OR. (IVER.GT.36)) IVER = 1
      ELSE
         IVER = 0
         END IF
C                                       batch values
      CALL ZPHFIL ('TG', 1, IUSER, IVER, PHNAME, IERR)
      MSGSUP = 32000
      CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IER)
      MSGSUP = 0
      POTERR = 101
      IF (IER.EQ.0) ISIZE = LSIZE
      IF (IER.LT.2) GO TO 110
         WRITE (MSGTXT,1100) IER
         CALL MSGWRT (6)
         GO TO 970
 110  CALL ZOPEN (LUNTS, FINDTS, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Init file record 1
      IPRBUF(1) = ISIZE
      IPRBUF(2) = 0
      IPRBUF(3) = 0
      IPRBUF(4) = 0
      IPRBUF(5) = NBLOCK
      POTERR = 50
      NWPL = 5
      NLPR = 51
C                                       Read old record 1
      IF (IER.EQ.0) GO TO 115
         CALL ZFIO ('READ', LUNTS, FINDTS, 1, IPRBUF, IERR)
         IF (IERR.NE.0) GO TO 965
         NBLOCK = MAX (4, IPRBUF(5))
C                                       Search for task name
 115  LREC = 1
      NTASK = IPRBUF(2)
      IF (NTASK.GT.0) THEN
         DO 130 JTASK = 1,NTASK
            IREC = JTASK / NLPR + 1
C                                       Read next rec
            IF (IREC.EQ.LREC) GO TO 125
               CALL ZFIO ('READ', LUNTS, FINDTS, IREC, IPRBUF, IERR)
               IF (IERR.NE.0) GO TO 965
               LREC = IREC
 125        ITEMP = MOD (JTASK, NLPR) * NWPL+ 1
            CALL H2CHR (8, 1, IPRBUF(ITEMP), CTEST)
            IF (ITASK.EQ.CTEST) GO TO 140
 130        CONTINUE
         END IF
C                                       Add task to directory
      JTASK = NTASK + 1
C
 140  IREC = JTASK / NLPR + 1
      I = MOD (JTASK, NLPR) * NWPL  +  1
      IF ((IREC.NE.LREC) .AND. (I.GT.1)) CALL ZFIO ('READ', LUNTS,
     *   FINDTS, IREC, IPRBUF, IERR)
      IF (IERR.NE.0) GO TO 965
      CALL CHR2H (8, ITASK, 1, IPRBUF(I))
      CALL CATIME (1, IPRBUF(I+2), ITIME)
      IPRBUF(I+4) = VERTYP
C                                       Save & get rec 1 back
      IF (IREC.LE.1) GO TO 145
         CALL ZFIO ('WRIT', LUNTS, FINDTS, IREC, IPRBUF, IERR)
         IF (IERR.NE.0) GO TO 965
         CALL ZFIO ('READ', LUNTS, FINDTS, 1, IPRBUF, IERR)
         IF (IERR.NE.0) GO TO 965
C                                       Save task count, expand file
 145  IPRBUF(2) = MAX (NTASK, JTASK)
      TSBLK = NBLOCK * (JTASK-1) + 7
      CALL CATIME (1, IPRBUF(3), ITIME)
      IF (TSBLK+NBLOCK-1.LE.IPRBUF(1)) GO TO 150
         NREC = 16
         CALL ZEXPND (LUNTS, 1, PHNAME, NREC, IERR)
         IF (IERR.NE.0) GO TO 965
         IPRBUF(1) = IPRBUF(1) + NREC
C                                       Return Record 1
 150  CALL ZFIO ('WRIT', LUNTS, FINDTS, 1, IPRBUF, IERR)
      IF (IERR.NE.0) GO TO 965
      CALL FILL (256, 0, IPRBUF)
      IPTR = 11
      LPTR = IPTR
      LPTRMX = 256 * NBLOCK
      IPRBUF(1) = NLUSER
      IPRBUF(2) = NTVDEV
      IPRBUF(3) = NTKDEV
      IF ((ISBTCH.EQ.32000) .OR. (NPOPS.GT.LINTRN+1)) IPRBUF(2) = 0
      IF ((ISBTCH.EQ.32000) .OR. (NPOPS.GT.LINTRN+1)) IPRBUF(3) = 0
      IPRBUF(4) = MSGKIL
      IPRBUF(5) = ISBTCH
      IPRBUF(6) = 0
      IPRBUF(7) = 0
      IPRBUF(8) = 0
      PARBUF(9) = DOWAIT
      CALL CHR2H (4, VERNAM, 1, PARBUF(10))
      IF ((VERTYP.GE.1) .AND. (VERTYP.LE.4)) CALL CHR2H (4,
     *   CVER(VERTYP), 1, PARBUF(10))
      IF ((VERTYP.GE.5) .AND. (VERTYP.LE.8)) CALL CHR2H (4,
     *   CVER(VERTYP-3), 1, PARBUF(10))
C                                       skip first 2 records
      POTERR = 59
 160  CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      IF (JBUF(1:1).EQ.';') GO TO 160
      IF (JBUF(1:8).EQ.'--------') THEN
         POTERR = 54
         GO TO 960
         END IF
      CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      IF (JBUF(1:8).EQ.'--------') THEN
         POTERR = 54
         GO TO 960
         END IF
C                                       loop for adverbs
 210  CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
         IF (IERR.EQ.IEOF) GO TO 250
         POTERR = 59
         IF (IERR.NE.0) GO TO 960
         IF (JBUF(1:1).EQ.';') GO TO 210
         IF (JBUF(1:8).EQ.'--------') GO TO 250
         I = JTRIM (JBUF)
         ISAST = JBUF(COLSHO:COLSHO).EQ.'*'
         ISQUES = JBUF(COLSHO:COLSHO).EQ.'?'
         JBUF(COLSHO:COLSHO) = ' '
         IF ((BRANCH.NE.6) .AND. (ISQUES)) GO TO 210
         IF ((BRANCH.EQ.6) .AND. (.NOT.ISQUES) .AND. (.NOT.ISAST))
     *      GO TO 210
         KARBUF = JBUF
         KBPTR = NMATCC (1, ' ', KARBUF)
C                                       coment
         IF (KBPTR.GE.COLLBN) GO TO 210
C                                       adverb: is it system value?
         SYSLIM(1) = -1.0
         SYSLIM(2) = -2.0
         DO 212 I = 1,NSYSAD
            IF (KARBUF(1:8).EQ.SYSADV(I)) THEN
               IF (I.LE.NSYSTA) THEN
                  SYSLIM(2) = NTAPED
               ELSE IF (I.LE.NSYSTA+NSYSDA) THEN
                  SYSLIM(2) = NVOL
               ELSE IF (I.LE.NSYSTA+NSYSDA+NSYSCH) THEN
                  SYSLIM(2) = MAXCHA
               ELSE IF (I.LE.NSYSTA+NSYSDA+NSYSCH+NSYSIM) THEN
                  SYSLIM(2) = MAXIMG
               ELSE
                  SYSLIM(2) = LINTRN + 1 + LBATQS
                  IF (SYSADV(I).EQ.'BATQUE') SYSLIM(2) = LBATQS
                  END IF
               GO TO 213
               END IF
 212        CONTINUE
C                                       adverb
 213     CALL GETFLD
         IF (ERRNUM.NE.0) GO TO 960
         ITAG = TAG
         ILOC = LOCSYM
         ITYPE = SYTYPE
         LIM(1) = LMIN
         LIM(2) = LMAX
C                                       Type 7 is a string.
         IF (ITYPE.EQ.7) GO TO 225
         POTERR = 8
C                                       Type 1 is a scalar, 2=array.
         IF ((ITYPE.NE.1) .AND. (ITYPE.NE.2)) GO TO 960
C                                       find limits: reals, arrays
 215     KBPTR = NMATCC (KBPTR, ' ', KARBUF)
            IF (KBPTR.LT.COLCOM) THEN
               I = 2
               IF (KBPTR.LT.COLUBN) I = 1
               CALL GETFLD
               IF (ERRNUM.NE.0) GO TO 960
C                                       negative
               IF (TAG.EQ.6) THEN
                  CALL GETFLD
                  IF (ERRNUM.NE.0) GO TO 960
                  LIM(I) = -C(TAG)
               ELSE
                  LIM(I) = C(TAG)
                  END IF
               GO TO 215
               END IF
C                                       values to buffer
 225     ISIZE = 1
         IF (ITYPE.NE.1) ISIZE = K(K(ILOC+3))
         IF (SYSLIM(2).GE.SYSLIM(1)) CALL RCOPY (2, SYSLIM, LIM)
         DO 245 I = 1,ISIZE
            IF (ITYPE.EQ.7) THEN
               HARBUF(IPTR) = CH(ITAG)
            ELSE
               PARBUF(IPTR) = C(ITAG)
               CALL BOUNDS (C(ITAG), C(ITAG), LIM, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1235) JBUF(1:10)
                  CALL MSGWRT (6)
                  POTERR = 32
                  GO TO 960
                  END IF
               END IF
 240        ITAG = ITAG + 1
            IPTR = IPTR + 1
            LPTR = LPTR + 1
            IF (LPTR.GT.LPTRMX) THEN
               WRITE (MSGTXT,1240)
               CALL MSGWRT (8)
               POTERR = 32
               GO TO 960
               END IF
            IF (IPTR.LE.256) GO TO 245
              IPTR = 1
               POTERR = 50
               CALL ZFIO ('WRIT', LUNTS, FINDTS, TSBLK, IPRBUF, IERR)
               IF (IERR.NE.0) GO TO 960
               TSBLK = TSBLK + 1
               CALL FILL (256, 0, IPRBUF)
 245        CONTINUE
         GO TO 210
C                                       clear last block
 250  IF (IPTR.LE.1) GO TO 255
         POTERR = 50
         CALL ZFIO ('WRIT', LUNTS, FINDTS, TSBLK, IPRBUF, IERR)
         IF (IERR.NE.0) GO TO 960
C                                       close files
 255  CALL ZTCLOS (LUNIN, FINDIN, IERR)
      CALL ZCLOSE (LUNTS, FINDTS, IERR)
      IF (BRANCH.NE.1) GO TO 999
C                                       task has valid name?
      CALL BLDTNM (ITASK, NPOPS, ANAME, IERR)
      POTERR = 43
      IF (IERR.NE.0) GO TO 980
C                                       is TASK actually a verb?
C                                       This should never happen now.
      KARBUF = ITASK
      KBPTR = 1
      CALL GETFLD
      IERR = ERRNUM
      ERRNUM = 0
      IF (IERR.EQ.2) GO TO 999
      IF (IERR.EQ.0) GO TO 260
         POTERR = IERR
         GO TO 980
 260  IF ((SYTYPE.EQ.4) .OR. (SYTYPE.EQ.5)) GO TO 980
      GO TO 999
C-----------------------------------------------------------------------
C                                        SPY
C                                        list running tasks
C                                        Z routines do actual lists now
C-----------------------------------------------------------------------
 300  GO TO 999
C-----------------------------------------------------------------------
C                                        WAITTASK
C                                        wait for task to finish
C-----------------------------------------------------------------------
 400  GO TO 999
C-----------------------------------------------------------------------
C                                        ABORTASK
C                                        abort specified task
C-----------------------------------------------------------------------
 500  GO TO 999
C-----------------------------------------------------------------------
C                                       errors: close files
 960  CONTINUE
 965  CALL ZCLOSE (LUNTS, FINDTS, IERR)
 970  CALL ZTCLOS (LUNIN, FINDIN, IERR)
C                                       POPS error management
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.GT.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANNOT DO THIS WITH AIPS, AIPSC, OR AIPSB')
 1100 FORMAT ('UNABLE TO CREATE ADVERB SAVE FILE.  IER=',I7)
 1235 FORMAT ('ERROR IS IN VALUE OF ADVERB ',A)
 1240 FORMAT ('TOO MANY PARAMETERS FOR COMM FILE --- SEE THE',
     *   ' PROGRAMMER')
      END
      SUBROUTINE CU3A (BRANCH)
C-----------------------------------------------------------------------
C   CU3A is a version of AU3A for the Checker version of AIPS.
C   It is mostly a stub, but carries out the dialogue which requires a
C   response from the user.
C   AU3A contains the disk management verbs DISKUSE (list disk file
C   usage by user number), ALLDEST (verb to destroy all or most of a
C   users data files) and SCRDEST (destroy all scratch files created by
C   a specific task).  SAVDEST destroy user's SAVE/GET files.
C   INPUTS:  BRANCH I   1 = DISKUSE
C                       2 = ALLDEST
C                       3 = TIMDEST (by age) NOW BLOCKED
C                       4 = SAVDEST
C                       5 = SCRDEST
C   COMMON:  /CAPL/
C            USERID   R   (DISKUSE) User no. 0=logon, 32000=all.
C            INDISK   R   (DISKUSE) Disk, 0 = all.
C            SORT     R(2)   (DISKUSE) sort type:
C                            ' '=none, 'U'=user ID,
C                            'B'=no of blocks used.
C            INNAME   R(3)   (ALLDEST) image name.
C            INCLASS  R(2)   (ALLDEST) image class.
C            INSEQ    R      (ALLDEST) image seq no.
C            NLUSER   I       (ALLDEST, SAVDEST, SCRDEST) logon user #.
C            TASK     R(2)    (SCRDEST) task name.
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER IHDR*80, PRGNAM*6, CHNAME*12, CHCLAS*6, CHTYPE*2, LPT*1,
     *   CDUM*1
      INTEGER   IEVOL, IBVOL, IVOL, POTERR, ISEQ, I, J2, IDUM, J1
      LOGICAL   ALLDEF, CONF
      REAL      DTIME(36), TIMRUL(36), RDUM, DELTIM
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGNAM /'AU3A  '/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.5)) GO TO 999
C                                       block TIMDEST
      IF (BRANCH.EQ.3) THEN
         MSGTXT = 'TIMDEST IS NO LONGER ALLOWED'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      POTERR = 33
C                                       Determine defaults for disk.
      CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       Only one disk.
      IF ((IVOL.NE.0) .AND. (BRANCH.EQ.3)) THEN
         IBVOL = IVOL
         IEVOL = IVOL
      ELSE
         IBVOL = 1
         IEVOL = NVOL
         END IF
C                                       Check confirm on ALLDEST
 10   CALL ADVERB ('DOCONFRM', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CONF = (BRANCH.EQ.2) .AND. (RDUM.GT.0.0)
      IF (CONF) THEN
         IF ((IUNIT.NE.1) .AND. (IUNIT.NE.4)) THEN
            WRITE (MSGTXT,1010)
            CALL MSGWRT (8)
            POTERR = 53
            IF (IUNIT.EQ.3) POTERR = 60
            GO TO 980
            END IF
         END IF
C                                       Branch on opcode.
 20   GO TO (100, 200, 200, 400, 500), BRANCH
C-----------------------------------------------------------------------
C                                       FREESPAC
C                                       Report disk space, # files
C-----------------------------------------------------------------------
C                                       Stub this one completely
 100  GO TO 999
C-----------------------------------------------------------------------
C                                       ALLDEST
C                                       Destroy all images matching
C                                       adverbs
C-----------------------------------------------------------------------
 200  CALL ADVERB ('INSEQ', 'I', 1, 0, ISEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('DETIME', 'R', 1, 0, IDUM, DELTIM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL RCOPY (35, TIMEDA, TIMRUL)
      TIMRUL(36) = TIMESG
      DO 201 I = 1,36
         DTIME(I) = DELTIM
         IF (BRANCH.EQ.3) DTIME(I) = MAX (DELTIM, TIMRUL(I))
         IF (DTIME(I).LE.0.0) DTIME(I) = 0.0
 201     CONTINUE
      ALLDEF = .TRUE.
C                                       Check for all default values.
      IF (BRANCH.EQ.3) GO TO 205
         CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, CHNAME)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, CHCLAS)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('INTYPE', 'C', 1, 2, IDUM, RDUM, CHTYPE)
         IF (ERRNUM.NE.0) GO TO 980
         IF ((CHNAME.EQ.' ') .AND. (CHCLAS.EQ.' ') .AND.
     *      (CHTYPE.EQ.' ') .AND. (ISEQ.EQ.0)) GO TO 205
            ALLDEF = .FALSE.
            WRITE (MSGTXT,1200) CHTYPE, CHNAME, CHCLAS, ISEQ
            CALL MSGWRT (5)
 205  IF (DELTIM.GT.0.0) THEN
         J1 = 1
         J2 = MIN (5, NVOL)
         WRITE (MSGTXT,1205) (DTIME(I), I = 1,J2)
         CALL MSGWRT (5)
         ALLDEF = .FALSE.
 206     J1 = J2 + 1
         J2 = MIN (J1 + 9, NVOL)
         IF (J1.LE.NVOL) THEN
            WRITE (MSGTXT,1206) (DTIME(I), I = J1,J2)
            CALL MSGWRT (5)
            GO TO 206
            END IF
         END IF
 207  IF (BRANCH.EQ.2) WRITE (MSGTXT,1207) NLUSER, IBVOL, IEVOL
      IF (BRANCH.EQ.3) WRITE (MSGTXT,1208)
      CALL MSGWRT (5)
C                                       Chance to back out
      LPT = IPT
      IPT = '?'
      CALL PREAD (IHDR)
      IPT = LPT
      MSGTXT = JBUFF
      CALL MSGWRT (0)
      POTERR = 39
      CALL CHLTOU (3, JBUFF)
      IF (JBUFF(1:2).EQ.'NO') GO TO 999
      IF (JBUFF(1:3).NE.'YES') GO TO 980
C                                       Stub the rest
      IF ((BRANCH.EQ.2) .AND. (ALLDEF)) GO TO 400
         GO TO 999
C-----------------------------------------------------------------------
C                                       SAVDEST
C-----------------------------------------------------------------------
C                                       Ask permision.
 400  WRITE (MSGTXT,1400) NLUSER
      CALL MSGWRT (5)
      LPT = IPT
      IPT = '?'
      CALL PREAD (IHDR)
      IPT = LPT
      MSGTXT = JBUFF
      CALL MSGWRT (0)
      POTERR = 39
      CALL CHLTOU (3, JBUFF)
      IF (JBUFF(1:2).EQ.'NO') GO TO 999
      IF (JBUFF(1:3).NE.'YES') GO TO 980
C                                       Stub the SG destroy
C                                       Ask permision. for TS file
 450  WRITE (MSGTXT,1450) NLUSER
      CALL MSGWRT (5)
      LPT = IPT
      IPT = '?'
      CALL PREAD (IHDR)
      IPT = LPT
      MSGTXT = JBUFF
      CALL MSGWRT (0)
      POTERR = 39
      IF (JBUFF(1:2).EQ.'NO') GO TO 999
      IF (JBUFF(1:3).NE.'YES') GO TO 980
      GO TO 999
C-----------------------------------------------------------------------
C                                       SCRDEST
C-----------------------------------------------------------------------
 500  CONTINUE
      GO TO 999
C-----------------------------------------------------------------------
C                                       Catalog I/O error.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.GT.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CONFIRMATION ALLOWED ONLY IN INTERACTIVE MODE')
 1200 FORMAT ('Limited to ',A2,' files matching name parms ',A12,
     *   '.',A6,'.',I4)
 1205 FORMAT ('Limited to files last used >',5(F5.1,','))
 1206 FORMAT (10(F5.1,','))
 1207 FORMAT ('Destroy all files for user',I5,' disks',I2,'-',I1,
     *   '?   enter YES or NO')
 1208 FORMAT ('Destroy all files last used too many days ago?',
     *   '   Enter YES or NO')
 1400 FORMAT ('Destroy all SAVE files for user',I5,'?  enter YES or NO')
 1450 FORMAT ('Destroy TPUT/TGET files for user',I5,
     *   '?  enter YES or NO')
      END
      SUBROUTINE CU8 (BRANCH)
C-----------------------------------------------------------------------
C   CU8: checker version of AU8 performs some catalog operations:
C   BRANCH = 1  CLRNAME  clear name fields
C            2  GETNAME  set first name fields
C            3  GET2NAME set second name fields
C            4  GET3NAME set third name fields.
C            5  EXTDEST  destroy named extension file
C            6  CLR2NAME  clear name-2 fields
C            7  CLR3NAME  clear name-3 fields
C            8  EGETNAME  set first name fields w error adverb
C            9  GETONAME  set output fields
C           10  CLRONAME  clear output fields
C  CLRNAME blanks out INNAME, INCLASS, INSEQ, INTYPE, USERID.
C  GETNAME will fill in INNAME, INCLASS, INSEQ, INTYPE, USERID from a
C          catalog entry (given by # via immediate argument).
C  EXTDEST Inputs:   (through adverbs in common)
C     INNAME   C*12 the name of the cataloged file.  No defaults allowed
C     INCLASS  C*6  the class of the cataloged file.
C     INSEQ    R    the sequence number of the cataloged file.
C     INTYPE   C*2  the cataloged files's type.
C     INDISK   R    the disk volume number of the cataloged file.  If 0,
C                   all disk volumes are searched until a match is found
C     INEXT    C*2  the type of the extension file to destroy.
C                   No defaults allowed.
C     INVERS   R    the version number of the extension file
C                   to be destroyed.  No defaults allowed.
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER PRGNAM*6, XNNAM*12, XNCLS*6, XNTYP*2, CDUM*2
      REAL      RDUM
      INTEGER   ISLOT, POTERR, IDUM, XSEQ, IVOL, INUSER, IUSER, MAGIC
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DPOP.INC'
      DATA PRGNAM /'AU8 '/
      DATA MAGIC /32000/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.10)) GO TO 999
      POTERR = 0
C                                       init for blanking
      XNNAM = ' '
      XNCLS = ' '
      XNTYP = ' '
      XSEQ = 0
      IVOL = 0
      CALL ADVERB ('USERID', 'I', 1, 0, INUSER, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IUSER = 0
      IF (INUSER.EQ.MAGIC) IUSER = INUSER
C
      GO TO (100, 200, 200, 200, 400, 500, 600, 200, 200, 700), BRANCH
C-----------------------------------------------------------------------
C                                       CLRNAME
C                                       null values to INNAME, INCLASS.
C-----------------------------------------------------------------------
 100  CALL ADVRBS ('INNAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('INCLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('INTYPE', 'C', 1, 2, IDUM, RDUM, XNTYP)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('INSEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('USERID', 'I', 1, 0, IUSER, RDUM, CDUM)
      GO TO 980
C-----------------------------------------------------------------------
C                                       GETNAME
C                                       GET2NAME
C                                       GET3NAME
C                                       EGETNAME
C                                       GETONAME
C                                       set name to match cat# on stack
C-----------------------------------------------------------------------
C                                       Calculate catalog entry locatn.
 200  ISLOT = V(SP) + .01
      SP = SP - 1
      POTERR = 8
      IF ((SP.LT.0) .OR. (ISLOT.LE.0)) GO TO 980
      IF (BRANCH.EQ.8) THEN
         RDUM = -1.0
         CALL ADVRBS ('ERROR', 'R', 1, 0, IDUM, RDUM, CDUM)
         END IF
      POTERR = 0
      GO TO 980
C-----------------------------------------------------------------------
C                                       EXTDEST
C                                       destroy spec. extension file
C-----------------------------------------------------------------------
C                                       Can't assume here that name is
C                                       filled in, so no tests
 400  GO TO 999
C-----------------------------------------------------------------------
C                                       CLR2NAME
C                                       null values to IN2NAME, ...
C-----------------------------------------------------------------------
 500  CALL ADVRBS ('IN2NAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('IN2CLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('IN2TYPE', 'C', 1, 2, IDUM, RDUM, XNTYP)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('IN2SEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('IN2DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      GO TO 980
C-----------------------------------------------------------------------
C                                       CLR3NAME
C                                       null values to IN3NAME, ....
C-----------------------------------------------------------------------
 600  CALL ADVRBS ('IN3NAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('IN3CLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('IN3TYPE', 'C', 1, 2, IDUM, RDUM, XNTYP)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('IN3SEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('IN3DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      GO TO 980
C-----------------------------------------------------------------------
C                                       CLRONAME
C                                       null values to OUTNAME, ....
C-----------------------------------------------------------------------
 700  CALL ADVRBS ('OUTNAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('OUTCLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('OUTSEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('OUTDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      GO TO 980
C-----------------------------------------------------------------------
C                                       AIPS error management.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
      END
      SUBROUTINE GTLINC
C-----------------------------------------------------------------------
C   GTLINC is the main POPS routine.  It causes lines to be read by
C   PREAD, parsed and compiled or executed(pseudo verbs) by POLISH,
C   and finally executed by INTERC.  GTLINC returns only on error.
C   Version for AIPSC (Checker).
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DCON.INC'
      CHARACTER PRGNAM*6
      INTEGER   A(100), B(100), KKT(KKTSIZ), ILOCA, KTLP, KTLPSV, LLOCAT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DIO.INC'
      EQUIVALENCE (KKT(1), K(51))
      EQUIVALENCE (STACK(1), A(1)), (CSTACK (1), B(1))
      DATA PRGNAM /'GTLINC'/
C-----------------------------------------------------------------------
      ERRNUM = 0
C                                        reset temporary piece of K
 10   KKT(1) = 0
         KKT(2) = 0
         KKT(3) = 11
         KKT(4) = 0
         KKT(5) = KKTSIZ
         KKT(7) = 11
         KKT(10) = 4
         KTLP = 2
C                                       Read a line, skip comments.
 20      CALL PREAD (KARBUF)
         IF (ERRNUM.NE.0) GO TO 980
            MSGTXT = JBUFF
C                                       If reading from CRT write to
C                                       log file only.
            IF (IUNIT.EQ.1) THEN
               CALL MSGWRT (0)
C                                       Write to log file & crt if
C                                       reading from EDIT file
            ELSE IF (IUNIT.NE.5) THEN
               CALL MSGWRT (2)
               END IF
            IF (KARBUF(1:1).EQ.'*') GO TO 20
            KBPTR = 1
C                                          Compile a line.
            CALL POLISH
            IF (ERRNUM.NE.0) GO TO 980
C                                       MODE = 1 -> compilation
            IF ((AP.LE.0) .OR. (MODE.NE.0)) THEN
               IF (MODE.NE.1) THEN
                  MODE = 0
                  LPGM = 2
                  END IF
               GO TO 20
               END IF
C                                       Mode 0 -> Immed. execute.
 30      KTLPSV = KTLP
         ILOCA = AP + 2
         L = LLOCAT (ILOCA, KKT, KTLP)
         IF (ERRNUM.EQ.1) ERRNUM = 74
         IF (ERRNUM.NE.0) GO TO 980
         KKT(KTLPSV) = KKT(KTLPSV) + KT - 1
         KKT(7) = KKT(3)
         KKT(6) = KKT(10)
C                                       Address relative to K array.
         L = L + 2
         CALL COPY (AP, A(1), KKT(L))
         CALL INTERC (KKT(2))
         IF (ERRNUM.NE.0) GO TO 980
         GO TO 10
C                                        error return
 980  ERRLEV = ERRLEV+1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
      SUBROUTINE INTERC (KENTRY)
C-----------------------------------------------------------------------
C   INTERC causes the POPS code to be executed: placing operands on
C   the V and STACK stacks and calling VERBSC and QUICK for verbs.
C   This version for AIPSC (Checker) - doesn't really execute user
C   verbs.
C   Inputs:
C      KENTRY  I    pointer to first particle of executable code
C-----------------------------------------------------------------------
      INTEGER   KENTRY
C
      CHARACTER PRGNAM*6
      INTEGER   J, J1, KK, LFLAG, POTERR
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DIO.INC'
      DATA PRGNAM /'INTERC'/
C-----------------------------------------------------------------------
      IPT = '#'
      SP = 0
      CP = 0
      SP0 = 1
      LINK = K(KENTRY)
      L = KENTRY
C                                       Program chunk begins.
 10   L = L+1
C                                          Advance program counter.
 20      L = L+1
 25         J = K(L)
C                                       Debug.
               IF (IDEBUG.GT.0) THEN
                  KK = CSTACK(CP)
                  WRITE (MSGTXT,1025) L, J, SP, SP0, STACK(SP),
     *               V(SP), CP, CSTACK(CP), C(KK)
                  CALL MSGWRT (4)
                  END IF
C                                       Operand - push on stack.
 30            IF(J.LE.0) THEN
                  SP = SP + 1
                  POTERR = 7
                  IF ((SP.LT.1) .OR. (SP.GT.SLIM)) GO TO 980
                  IF ((CP.LT.0) .OR. (CP.GT.SLIM)) GO TO 980
                  J1 = -J
                  STACK(SP) = J1
                  V(SP) = C(J1)
C                                       Verb linkages.
               ELSE IF ((J.GE.200) .AND. (J.LE.999)) THEN
                  CALL VERBSC (J)
                  IF (ERRNUM.NE.0) GO TO 980
C                                       POPS operator linkages.
               ELSE
                  CALL KWICK (LFLAG, J)
                  IF (ERRNUM.NE.0) GO TO 980
                  IF ((LFLAG.GE.1) .AND. (LFLAG.LE.6)) GO TO (10, 20,
     *               25, 50, 60, 995), LFLAG
                     POTERR = 15
                     GO TO 980
C                                        store intermediate value
 50                  SP = SP-1
                     STACK(SP) = 0
 60                  V(SP) = XX
                  END IF
               GO TO 20
C                                       Error exit.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV .LE. 5) PNAME(ERRLEV) = PRGNAM
C                                       Normal exit.
 995  IPT = '>'
C
 999  RETURN
C-----------------------------------------------------------------------
 1025 FORMAT ('INTERC:', 2I6, 2I3, I5, G12.5, 2I5, G12.5)
      END
      SUBROUTINE VERBSC (J)
C-----------------------------------------------------------------------
C   VERBS establishes the correspondance between verb operators
C   and their subroutine calls.  VERSION for AIPSC (Checker) - calls
C   for special verb routines CU2, CU3A, and CU8.
C   Inputs:
C      J    I      verb number
C-----------------------------------------------------------------------
      INTEGER   J
C
      INTEGER   NUMSUB
      PARAMETER (NUMSUB = 36)
      CHARACTER PRGNAM*6
      INTEGER   JJJJ, IAB(NUMSUB), IAE(NUMSUB), I, IWSAVE(9)
      REAL      RWSAVE(30)
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DPOP.INC'
      DATA PRGNAM /'VERBSC'/
      DATA IAB /   200, 210, 220, 230, 250, 260, 270, 290, 300, 310,
     *   330, 340, 350, 360, 370, 380, 390, 400, 410, 415, 420, 425,
     *   440, 450, 460, 480, 499, 510, 520, 530, 540, 560, 561, 580,
     *   700, 900/
      DATA IAE /   209, 219, 229, 249, 259, 269, 289, 299, 309, 329,
     *   339, 349, 359, 369, 379, 389, 399, 409, 414, 419, 424, 439,
     *   449, 459, 479, 498, 509, 519, 529, 539, 559, 560, 579, 599,
     *   710, 909/
C                  AU1, A1A, AU2, A2A, AU3, A3A, A3B, AU4, A4A, AU5,
C    *   A5A, A5B, A5C, A5D, A5E, A5F, AU6, A6A, A6B, A6C, A6D, A6E,
C    *   AU7, A7A, A7B, AU8, A8A, AU9, A9A, A9B, A9C, AUA, AUB, AUC,
C    *   AUO, AUT
C-----------------------------------------------------------------------
C                                        save POPS environment
      CALL RCOPY (5, KPAK, RWSAVE(1))
      CALL RCOPY (25, X, RWSAVE(6))
      IWSAVE(1) = KBPTR
      IWSAVE(2) = LX
      IWSAVE(3) = TAG
      IWSAVE(4) = NKAR
      IWSAVE(5) = SYTYPE
      IWSAVE(6) = L
      IWSAVE(7) = LOCSYM
      IWSAVE(8) = K(KT+2)
      IWSAVE(9) = K(KT+9)
C                                       Search list for proper overlay.
      DO 5 I = 1,NUMSUB
         IF ((J.GE.IAB(I)) .AND. (J.LE.IAE(I))) THEN
            JJJJ = J - IAB(I) + 1
            GO TO (10, 11, 20, 21, 30, 31, 32, 40, 41, 50, 51, 52, 53,
     *         54, 55, 56, 60, 61, 62, 63, 64, 65, 70, 71, 72, 80, 81,
     *         90, 91, 92, 93, 100, 110, 120, 170, 190),  I
            END IF
 5       CONTINUE
      GO TO 999
C                                       Verb linkages.
 10   CONTINUE
         IF (JJJJ.GT.1) CALL AU1 (JJJJ)
         GO TO 980
C                                       Verb linkages.
 11   CONTINUE
         CALL AU1A (JJJJ)
         GO TO 980
 20   CONTINUE
         IF (JJJJ.EQ.6) GO TO 970
         CALL CU2 (JJJJ)
         GO TO 980
 21   CONTINUE
         CALL AU2A (JJJJ)
         GO TO 980
 30   CONTINUE
C        CALL AU3 (JJJJ)
         GO TO 980
 31   CONTINUE
         CALL CU3A (JJJJ)
         GO TO 980
 32   CONTINUE
C        CALL AU3B (JJJJ)
         GO TO 980
 40   CONTINUE
C        CALL AU4 (JJJJ)
         GO TO 970
 41   CONTINUE
C        CALL AU4A (JJJJ)
         GO TO 980
 50   CONTINUE
C        CALL AU5 (JJJJ)
         GO TO 970
 51   CONTINUE
C        CALL AU5A (JJJJ)
         GO TO 970
 52   CONTINUE
C        CALL AU5B (JJJJ)
         GO TO 970
 53   CONTINUE
C        CALL AU5C (JJJJ)
         GO TO 970
 54   CONTINUE
C        CALL AU5D (JJJJ)
         GO TO 970
 55   CONTINUE
C        CALL AU5E (JJJJ)
         GO TO 970
 56   CONTINUE
C        CALL AU5F (JJJJ)
         GO TO 970
 60   CONTINUE
C        CALL AU6 (JJJJ)
         GO TO 970
 61   CONTINUE
C        CALL AU6A (JJJJ)
         GO TO 970
 62   CONTINUE
C        CALL AU6B (JJJJ)
         GO TO 970
 63   CONTINUE
C        CALL AU6C (JJJJ)
         GO TO 970
 64   CONTINUE
         IF (JJJJ.NE.2) GO TO 970
C        CALL AU6D (JJJJ)
         GO TO 980
 65   CONTINUE
C        CALL AU6E (JJJJ)
         GO TO 970
 70   CONTINUE
C        CALL AU7 (JJJJ)
         GO TO 980
 71   CONTINUE
C        CALL AU7A (JJJJ)
         GO TO 980
 72      CONTINUE
C        CALL AU7B (JJJJ)
         GO TO 980
 80   CONTINUE
         CALL CU8 (JJJJ)
         GO TO 980
 81   CONTINUE
C        CALL AU8A (JJJJ)
         GO TO 980
 90   CONTINUE
C        CALL AU9 (JJJJ)
         GO TO 980
 91   CONTINUE
C        CALL AU9A (JJJJ)
         GO TO 970
 92   CONTINUE
C        CALL AU9B (JJJJ)
         GO TO 970
 93   CONTINUE
C        CALL AU9C (JJJJ)
         GO TO 970
 100  CONTINUE
C        CALL AUA (JJJJ)
         GO TO 970
 110  CONTINUE
C        CALL AUB (JJJJ)
         GO TO 970
 120  CONTINUE
C        CALL AUC (JJJJ)
         GO TO 970
 170  CONTINUE
C        CALL AUO (JJJJ)
         GO TO 970
 190  CONTINUE
C        CALL AUT (JJJJ)
         GO TO 970
C                                       Not in batch
 970  IF (ERRNUM.EQ.0) ERRNUM = 60
C                                       restore POPS environment
 980  KBPTR   = IWSAVE(1)
      LX      = IWSAVE(2)
      TAG     = IWSAVE(3)
      SYTYPE  = IWSAVE(5)
      L       = IWSAVE(6)
      LOCSYM  = IWSAVE(7)
      K(KT+2) = IWSAVE(8)
      K(KT+9) = IWSAVE(9)
      IF (IWSAVE(9).GT.4) K(KT+IWSAVE(9)-1) = 0
      CALL RCOPY (25, RWSAVE(6), X)
      IF (ERRNUM.EQ.0) THEN
         CALL RCOPY (5, RWSAVE(1), KPAK)
         NKAR = IWSAVE(4)
C                                       Error list
      ELSE
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
      END
