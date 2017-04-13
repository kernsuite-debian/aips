      SUBROUTINE AUB (BRANCH)
C-----------------------------------------------------------------------
C! verbs to prepare, edit, and review batch jobs and queues
C# POPS-appl Batch
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2010
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
C   AUB performs batch preparation operations
C   BRANCH = 1  BATCH     read input to build batch work file
C            2  BATEDIT   read input to insert in batch work file
C            3  UNQUE     deque job, move back to work file
C            4  BATCLEAR  initialize batch work file
C            5  BATLIST   list part of batch work file
C            6  QUEUE     list contents of job queue
C            7  JOBLIST   list part of submitted (inactive) job
C            8  BAMODIFY  Change text of one line of batch work file
C   Adverbs used: BATQUE    queue number (BRANCH = 1 - 6)
C                 JOBNUM    job number   (         3, 7)
C                 BATFLINE  first line # (         2, 5, 7)
C                 BATNLINE  # of lines   (         5, 7)
C-----------------------------------------------------------------------
      INTEGER    BRANCH
C
      CHARACTER PRGNAM*6, PHNAME*48, STATUS*8, ATIME*8, ADATE*12, LPT*1,
     *    TEMP*100, QUEUED*8, RUNING*8, SUBMIT*8, DONE*8, FAILED*8,
     *    BVERS(5)*4, HOLD*1, INSRT*1, DELETE*1, SPACE*1, ICHAR*1,
     *    CDUM*1, RHOST*24, FILE*64
      INTEGER    POTERR, IQ, IERR, J, JB, IW, IL, IPX, IM, LINE, IT0(6),
     *   IER2, IT(6), LINES, NLPR, I, NWPL, MSGLUN, INP, KOP, JQ, J1,
     *   LBYTES, MSGIND, LIUNIT, ITEMP, NCHCPY, IREC, LREC, JTRIM, J2,
     *   IDUM, LBATQS
      REAL      EPS, RLINE, RDUM
      DOUBLE PRECISION JD0, JD1
      LOGICAL   T, F, INSERT, INSIGN, BATOPN, BWTOPN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DBAT.INC'
      INTEGER   BWTBUF(256)
      INCLUDE 'INCS:DBWT.INC'
      DATA PRGNAM /'AUB '/
      DATA T, F /.TRUE.,.FALSE./
      DATA QUEUED, RUNING, SUBMIT, DONE, FAILED /'WAITING ',
     *   'RUNNING ','CHECKING','FINISHED','FAILED  '/
      DATA BVERS /'OLD ','NEW ','TST ','CVX ','??? '/
      DATA EPS /0.01/
      DATA MSGLUN, HOLD, INSRT, SPACE, DELETE /6, '?', '_', '@','$'/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.8)) GO TO 999
      BWTLUN = 26
      BATLUN = 4
      BWTOPN = .FALSE.
      BATOPN = .FALSE.
      NWPL = 2 + (NCHLIN + 3) / 4
      NLPR = 252 / NWPL
C                                        test input
      POTERR = 32
      CALL ADVERB ('BATQUE', 'I', 1, 0, IQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF ((BRANCH.EQ.3) .OR. (BRANCH.EQ.6) .OR. (BRANCH.EQ.7)) THEN
         CALL ADVERB ('REMHOST', 'C', 1, 24, IDUM, RDUM, RHOST)
         IF (ERRNUM.NE.0) GO TO 980
      ELSE
         RHOST = ' '
         END IF
      IF ((BRANCH.NE.6) .AND. (BRANCH.NE.7)) THEN
         IF ((IQ.LT.1) .OR. (IQ.GT.NBATQS)) THEN
            WRITE (MSGTXT,1000) IQ, NBATQS
            CALL MSGWRT (7)
            GO TO 980
            END IF
         END IF
C
      LBATQS = NBATQS
      IF (RHOST.NE.' ') LBATQS = 33
      CALL ADVERB ('JOBNUM', 'I', 1, 0, J, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      JQ = J / 100
      JB = MOD (J, 100)
      IF (BRANCH.EQ.6) JQ = IQ
C                                        open Queue file
      IF ((BRANCH.EQ.3) .OR. (BRANCH.EQ.6) .OR. (BRANCH.EQ.7)) THEN
         CALL ZPHFIL ('BQ', 1, 0, 0, PHNAME, IERR)
         J = JTRIM (RHOST)
C                                       local
         IF (J.EQ.0) THEN
            FILE = PHNAME
C                                       remote at this location
         ELSE
            FILE = 'NET0:' // RHOST(:J) // '/' // PHNAME(6:)
            END IF
         CALL ZEXIST (1, FILE, LBATQS, IERR)
         IF (IERR.NE.0) GO TO 960
         CALL ZOPEN (BATLUN, BATIND, 1, FILE, F, T, T, IERR)
         IF (IERR.NE.0) GO TO 960
         BATOPN = .TRUE.
         IF (BRANCH.NE.6) THEN
            CALL ZFIO ('READ', BATLUN, BATIND, JQ, BATDAT, IERR)
            IF (IERR.NE.0) GO TO 960
            END IF
         END IF
      IF ((BRANCH.EQ.3) .OR. (BRANCH.EQ.7)) THEN
         IF ((JQ.LT.1) .OR. (JQ.GT.LBATQS)) THEN
            JB = 100*LBATQS + 64
            WRITE (MSGTXT,1010) J, JB
            CALL MSGWRT (7)
            GO TO 980
            END IF
         IF ((JB.LT.1) .OR. (JB.GT.64)) THEN
            WRITE (MSGTXT,1015) J
            CALL MSGWRT (7)
            GO TO 980
            END IF
         END IF
C                                        open work file
      POTERR = 50
      IF ((BRANCH.NE.6) .AND. (BRANCH.NE.7)) THEN
         CALL ZPHFIL ('BA', 1, IQ, NPOPS, BWTNAM, IERR)
         CALL ZOPEN (BWTLUN, BWTIND, 1, BWTNAM, F, T, T, IERR)
         IF (IERR.NE.0) GO TO 980
         BWTOPN = .TRUE.
         CALL ZFIO ('READ', BWTLUN, BWTIND, 1, BWTBUF, IERR)
         IF (IERR.NE.0) GO TO 960
         BWTREC = 1
         BWTNUM = BWTBUF(4)
         END IF
C                                        branch to OP
      GO TO (100, 200, 300, 400, 500, 600, 700, 200), BRANCH
C-----------------------------------------------------------------------
C                                        BATCH
C                                        build work file
C-----------------------------------------------------------------------
C                                        DISP = NEW for new user, MOD
 100  IF (NLUSER.NE.BWTBUF(1)) THEN
         CALL FILL (256, 0, BWTBUF)
         BWTBUF(1) = NLUSER
         BWTBUF(2) = 2
         BWTBUF(3) = 1
         END IF
 110  IW = BWTBUF(2)
      IL = 0
      IPX = BWTBUF(3)
      IM = BWTBUF(2)
      CALL BBUILD (IW, IL, IPX, IM, BWTBUF)
      POTERR = 0
      GO TO 960
C-----------------------------------------------------------------------
C                                        BATEDIT
C                                        insert into work file
C-----------------------------------------------------------------------
C                                        must be user's work file
 200  IF (NLUSER.NE.BWTBUF(1)) THEN
         WRITE (MSGTXT,1200) NLUSER, BWTBUF(1)
         CALL MSGWRT (7)
         POTERR = 40
         GO TO 960
         END IF
C                                        get line #, opt. argument
      CALL ADVERB ('BATFLINE', 'R', 1, 0, IDUM, RLINE, CDUM)
      IF (ERRNUM.NE.0) GO TO 960
      IF ((SP.GE.1) .AND. (STACK(SP).NE.2)) THEN
         RLINE = V(SP)
         SP = SP - 1
         END IF
C                                        check line number
      LINE = RLINE + EPS
      INSERT = (ABS(RLINE-LINE) .GT. EPS) .AND. (BRANCH.EQ.2)
      IF ((RLINE.LE.EPS) .OR. (BRANCH.NE.2)) THEN
         IF ((BRANCH.NE.8) .OR. (LINE.LT.1) .OR.
     *      (LINE.GT.BWTBUF(2)-2)) THEN
            WRITE (MSGTXT,1210) RLINE
            CALL MSGWRT (7)
            POTERR = 32
            GO TO 960
            END IF
         END IF
      IF (LINE.GE.BWTBUF(2)-2) THEN
C                                        insert after current text
         IF ((INSERT) .OR. (LINE.NE.BWTBUF(2)-2)) THEN
            WRITE (MSGTXT,1215)
            CALL MSGWRT (3)
            GO TO 110
            END IF
         END IF
C                                        read file to find LINE
      IW = BWTBUF(2)
      IM = IW
      POTERR = 50
      DO 230 I = 1,LINE
         LREC = (BWTREC-1)/NLPR + 1
         J = 5 + NWPL * MOD (BWTREC-1, NLPR)
         BWTREC = BWTBUF(J)
         IF (BWTREC.LE.0) THEN
            IF ((I.NE.LINE) .OR. (LINE.NE.IM-2)) THEN
               WRITE (MSGTXT,1220) I
               CALL MSGWRT (9)
               POTERR = 40
               GO TO 960
               END IF
            END IF
         IREC = (BWTREC-1)/NLPR + 1
         IF (IREC.NE.LREC) THEN
            CALL ZFIO ('READ', BWTLUN, BWTIND, IREC, BWTBUF, IERR)
            IF (IERR.NE.0) GO TO 960
            END IF
 230     CONTINUE
C                                        set pointers
      J = 5 + NWPL * MOD (BWTREC-1, NLPR)
      IF (BRANCH.EQ.8) GO TO 800
      IL = BWTBUF(J)
      IF (INSERT) THEN
         IPX = BWTREC
      ELSE
         IPX = BWTBUF(J+1)
         IW = BWTREC
         END IF
      CALL BBUILD (IW, IL, IPX, IM, BWTBUF)
      POTERR = 0
      GO TO 960
C-----------------------------------------------------------------------
C                                        UNQUEUE
C                                        dequeue & recover text
C-----------------------------------------------------------------------
C                                        only inactive jobs this user
 300  J = 4 * JB - 3
      I = BATDAT(J)
      IF (I.NE.NLUSER) THEN
         IF (I.GT.0) THEN
            WRITE (MSGTXT,1300)
         ELSE IF (I.EQ.0) THEN
            WRITE (MSGTXT,1301)
         ELSE IF (BATDAT(J+1).GT.0) THEN
            WRITE (MSGTXT,1302)
         ELSE IF (BATDAT(J+1).EQ.0) THEN
            WRITE (MSGTXT,1303)
         ELSE
            WRITE (MSGTXT,1304)
            END IF
         CALL MSGWRT (7)
         POTERR = 40
         GO TO 960
         END IF
C                                        mark busy
      BATDAT(J) = -BATDAT(J)
      BATDAT(J+1) = ABS(BATDAT(J+1))
      CALL ZFIO ('WRIT', BATLUN, BATIND, JQ, BATDAT, IERR)
      CALL ZCLOSE (BATLUN, BATIND, IER2)
      IF ((IERR.NE.0) .OR. (IER2.NE.0)) THEN
         WRITE (MSGTXT,1305) IERR, IER2
         CALL MSGWRT (7)
         END IF
      BATOPN = .FALSE.
C                                        open file as input
      ITEMP = JQ + 1 + NINTRN
      CALL ZPHFIL ('BA', 1, JB, ITEMP, PHNAME, IERR)
      J = JTRIM (RHOST)
C                                       local
      IF (J.EQ.0) THEN
         FILE = PHNAME
C                                       remote at this location
      ELSE
         FILE = 'NET0:' // RHOST(:J) // '/' // PHNAME(6:)
         END IF
      CALL ZOPEN (BATLUN, BATIND, 1, FILE, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1310) IERR
         CALL MSGWRT (7)
         POTERR = 50
         GO TO 960
         END IF
      BATOPN = .TRUE.
C                                        set input unit
      LIUNIT = IUNIT
      IUNIT = 3
      BATREC = 1
      CALL ZFIO ('READ', BATLUN, BATIND, 1, BATDAT, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       just like BATCH here
C                                       Do BATCLEAR always now
      CALL FILL (256, 0, BWTBUF)
      BWTBUF(1) = NLUSER
      BWTBUF(2) = 2
      BWTBUF(3) = 1
 320  IW = BWTBUF(2)
      IL = 0
      IPX = BWTBUF(3)
      IM = BWTBUF(2)
      CALL BBUILD (IW, IL, IPX, IM, BWTBUF)
      POTERR = 0
      IUNIT = LIUNIT
C                                        close & destroy
      CALL ZCLOSE (BATLUN, BATIND, IERR)
      BATOPN = .FALSE.
      CALL ZDESTR (1, FILE, IER2)
      IF ((IERR.NE.0) .OR. (IER2.NE.0)) THEN
         WRITE (MSGTXT,1320) IERR, IER2
         CALL MSGWRT (7)
         POTERR = 50
         GO TO 960
         END IF
C                                        remove from queue
      CALL ZPHFIL ('BQ', 1, 0, 0, PHNAME, IERR)
      J = JTRIM (RHOST)
C                                       local
      IF (J.EQ.0) THEN
         FILE = PHNAME
C                                       remote at this location
      ELSE
         FILE = 'NET0:' // RHOST(:J) // '/' // PHNAME(6:)
         END IF
      CALL ZOPEN (BATLUN, BATIND, 1, FILE, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1750) IERR
         CALL MSGWRT (9)
         GO TO 960
         END IF
      BATOPN = .TRUE.
      CALL ZFIO ('READ', BATLUN, BATIND, JQ, BATDAT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1755) IERR
         CALL MSGWRT (9)
         GO TO 960
         END IF
      J = 4 * JB - 3
      CALL FILL (4, 0, BATDAT(J))
      CALL ZFIO ('WRIT', BATLUN, BATIND, JQ, BATDAT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1760) IERR
         CALL MSGWRT (9)
         END IF
      GO TO 960
C-----------------------------------------------------------------------
C                                        BATCLEAR
C                                        init work file
C-----------------------------------------------------------------------
 400  CALL FILL (256, 0, BWTBUF)
      BWTBUF(1) = NLUSER
      BWTBUF(2) = 2
      BWTBUF(3) = 1
      CALL ZFIO ('WRIT', BWTLUN, BWTIND, 1, BWTBUF, IERR)
      POTERR = 0
      IF (IERR.NE.0) POTERR = 50
      GO TO 960
C-----------------------------------------------------------------------
C                                        BATLIST
C                                        list work file
C-----------------------------------------------------------------------
 500  GO TO 720
C-----------------------------------------------------------------------
C                                        QUEUE
C                                        list jobs in queue
C-----------------------------------------------------------------------
 600  IF ((IQ.GE.1) .AND. (IQ.LE.LBATQS)) THEN
         J1 = IQ
         J2 = IQ
      ELSE
         J1 = 1
         J2 = LBATQS
         END IF
      CALL ZDATE (IT0(1))
      CALL ZTIME (IT0(4))
      CALL DAT2JD (IT0, JD0)
      WRITE (MSGTXT,1600)
      CALL MSGWRT (5)
C                                       loop over queues
      IW = 0
      DO 620 JQ = J1,J2
         IF (RHOST.NE.' ') MSGSUP = 32000
         CALL ZFIO ('READ', BATLUN, BATIND, JQ, BATDAT, IERR)
         MSGSUP = 0
         IF ((IERR.NE.0) .AND. (IW.GT.0)) GO TO 630
         IF (IERR.NE.0) GO TO 960
         IF (RHOST.NE.' ') IW = IW + 1
         ITEMP = 100*JQ
         DO 610 I = 1,64
            J = 4*I - 3
            IF (BATDAT(J).NE.0) THEN
               IW = ABS(BATDAT(J))
               IF (BATDAT(J).LE.0) THEN
                  STATUS = RUNING
                  IF (BATDAT(J+1).EQ.0) STATUS = SUBMIT
                  IF (BATDAT(J+1).LT.0) STATUS = DONE
               ELSE
                  STATUS = QUEUED
                  IF (BATDAT(J+1).LT.0) STATUS = FAILED
                  END IF
               BATDAT(J+1) = ABS(BATDAT(J+1))
               CALL CATIME (2, BATDAT(J+1), IT)
               CALL DAT2JD (IT, JD1)
               IF ((STATUS.NE.DONE) .OR. (JD1+7.GE.JD0)) THEN
                  IF ((STATUS.NE.FAILED) .OR. (JD1+15.GE.JD0)) THEN
                     IM = ITEMP + I
                     IF ((BATDAT(J+3).LE.0) .OR. (BATDAT(J+3).GT.4))
     *                  BATDAT(J+3) = 5
                     CALL TIMDAT (IT(4), IT(1), ATIME, ADATE)
                     WRITE (MSGTXT,1605) IM, ADATE, ATIME, STATUS, IW,
     *                  BVERS(BATDAT(J+3))
                     CALL MSGWRT (5)
                     END IF
                  END IF
               END IF
 610        CONTINUE
 620     CONTINUE
C                                        close Q file
 630  CALL ZCLOSE (BATLUN, BATIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1620) IERR
         CALL MSGWRT (8)
         END IF
      BATOPN = .FALSE.
      POTERR = 0
      GO TO 980
C-----------------------------------------------------------------------
C                                        JOBLIST
C                                        list submitted job
C-----------------------------------------------------------------------
C                                        list only inactive this user
 700  J = 4 * JB - 3
      I = BATDAT(J)
      IF (I.NE.NLUSER) THEN
         IF (I.GT.0) THEN
            WRITE (MSGTXT,1300)
         ELSE IF (I.EQ.0) THEN
            WRITE (MSGTXT,1301)
         ELSE IF (BATDAT(J+1).GT.0) THEN
            WRITE (MSGTXT,1302)
         ELSE IF (BATDAT(J+1).EQ.0) THEN
            WRITE (MSGTXT,1303)
         ELSE
            WRITE (MSGTXT,1304)
            END IF
         CALL MSGWRT (7)
         POTERR = 40
         GO TO 960
         END IF
C                                        mark busy
      BATDAT(J) = -BATDAT(J)
      INSIGN = BATDAT(J+1).LT.0
      BATDAT(J+1) = ABS(BATDAT(J+1))
      CALL ZFIO ('WRIT', BATLUN, BATIND, JQ, BATDAT, IERR)
      CALL ZCLOSE (BATLUN, BATIND, IER2)
      IF ((IERR.NE.0) .OR. (IER2.NE.0)) THEN
         WRITE (MSGTXT,1305) IERR, IER2
         CALL MSGWRT (7)
         END IF
      BATOPN = .FALSE.
C                                        open file
      ITEMP = JQ + 1 + NINTRN
      CALL ZPHFIL ('BA', 1, JB, ITEMP, PHNAME, IERR)
      J = JTRIM (RHOST)
C                                       local
      IF (J.EQ.0) THEN
         FILE = PHNAME
C                                       remote at this location
      ELSE
         FILE = 'NET0:' // RHOST(:J) // '/' // PHNAME(6:)
         END IF
      CALL ZOPEN (BWTLUN, BWTIND, 1, FILE, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 750
      BWTOPN = .TRUE.
      CALL ZFIO ('READ', BWTLUN, BWTIND, 1, BWTBUF, IERR)
      IF (IERR.NE.0) GO TO 750
C                                        list work or file
 720  BWTREC = 1
      CALL ADVERB ('BATFLINE', 'I', 1, 0, LINE, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 960
      CALL ADVERB ('BATNLINE', 'I', 1, 0, LINES, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 960
      IF (BWTBUF(2).LE.2) THEN
         WRITE (MSGTXT,1720)
         CALL MSGWRT (3)
         POTERR = 101
         GO TO 750
         END IF
      IF (LINE.GT.BWTBUF(2)-2) THEN
         LINES = BWTBUF(2)-2
         WRITE (MSGTXT,1723) LINE,LINES
         CALL MSGWRT (7)
         POTERR = 32
         GO TO 750
         END IF
C                                        finish setting line range
      LINE = MAX(LINE, 1)
      IF (LINES.LE.1) LINES = BWTBUF(2) - LINE - 1
      LINES = MIN(LINES, BWTBUF(2)-LINE-1)
      LINES = LINES + LINE - 1
      NCHCPY = MIN (74, NCHLIN)
      DO 740 I = 1,LINES
         LREC = (BWTREC-1)/NLPR + 1
         J = 5 + NWPL * MOD (BWTREC-1, NLPR)
         BWTREC = BWTBUF(J)
         IF ((BWTREC.LE.0) .AND. (I.NE.LINES)) THEN
            WRITE (MSGTXT,1220) I
            CALL MSGWRT (9)
            POTERR = 40
            GO TO 750
            END IF
         IREC = (BWTREC-1)/NLPR + 1
         IF (IREC.NE.LREC) THEN
            CALL ZFIO ('READ', BWTLUN, BWTIND, IREC, BWTBUF, IERR)
            IF (IERR.NE.0) GO TO 750
            END IF
         IF (I.GE.LINE) THEN
            J = 5 + NWPL * MOD (BWTREC-1, NLPR)
            WRITE (MSGTXT,1735) I
            MSGTXT(5:6) = '  '
            CALL H2CHR (NCHCPY, 1, BWTBUF(J+2), MSGTXT(7:))
            CALL MSGWRT (5)
            END IF
 740     CONTINUE
      POTERR = 0
C                                        clear busy setting
 750  IF (BRANCH.NE.7) THEN
         GO TO 960
      ELSE
         CALL ZPHFIL ('BQ', 1, 0, 0, PHNAME, IERR)
         J = JTRIM (RHOST)
C                                       local
         IF (J.EQ.0) THEN
            FILE = PHNAME
C                                       remote at this location
         ELSE
            FILE = 'NET0:' // RHOST(:J) // '/' // PHNAME(6:)
            END IF
         CALL ZOPEN (BATLUN, BATIND, 1, FILE, F, T, T, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1750) IERR
            CALL MSGWRT (9)
            GO TO 960
            END IF
         BATOPN = .TRUE.
         CALL ZFIO ('READ', BATLUN, BATIND, JQ, BATDAT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1755) IERR
            CALL MSGWRT (9)
            GO TO 960
            END IF
         J = 4 * JB - 3
         BATDAT(J) = ABS(BATDAT(J))
         IF (INSIGN) BATDAT(J+1) = -BATDAT(J+1)
         CALL ZFIO ('WRIT', BATLUN, BATIND, JQ, BATDAT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1760) IERR
            CALL MSGWRT (9)
            END IF
         GO TO 960
         END IF
C-----------------------------------------------------------------------
C                                        BAMODIFY (continued)
C                                        modify text of one line
C-----------------------------------------------------------------------
C                                       Echo old line
 800  TEMP = HOLD
      LBYTES = NCHLIN
      CALL H2CHR (LBYTES, 1, BWTBUF(J+2), TEMP(2:))
      TEMP(LBYTES+2:) = ' '
      INP = JTRIM (TEMP)
      I = MAX (INP, 1)
      CALL ZMSGOP (MSGLUN, MSGIND, 1, PHNAME, IERR)
      CALL ZTTYIO ('WRIT', MSGLUN, MSGIND, I, TEMP, IERR)
      CALL ZMSGCL (MSGLUN, MSGIND, IERR)
      POTERR = 0
C                                       Read new line
      LPT = IPT
      LIUNIT = IUNIT
      IUNIT = 1
      IPT = HOLD
      CALL PREAD (KARBUF)
      IUNIT = LIUNIT
      IPT = LPT
      IF (ERRNUM.NE.0) GO TO 960
      IF (NBYTES.LE.0) GO TO 960
C                                       Merge old and new lines
      INP = 1
      KOP = 1
      INSERT = .FALSE.
      DO 845 I = 1,1000
         ICHAR = KARBUF(I:I)
C                                       Insert mode
         IF (INSERT) THEN
            IF ((ICHAR.EQ.INSRT) .OR. (I.GT.NBYTES)) THEN
               INSERT = .FALSE.
            ELSE
               KARBUF(KOP:KOP) = ICHAR
               IF (ICHAR.EQ.SPACE) KARBUF(KOP:KOP) = ' '
               IF (ICHAR.NE.DELETE) KOP = KOP + 1
               END IF
C                                       Substitute mode
         ELSE
            IF ((I.GT.NBYTES) .AND. (INP.GT.LBYTES)) GO TO 850
C                                       Pure copy at end
            IF (I.GT.NBYTES) THEN
               CALL H2CHR (1, INP, BWTBUF(J+2), KARBUF(KOP:KOP))
               KOP = KOP + 1
               INP = INP + 1
C                                       Start insert
            ELSE IF (ICHAR.EQ.INSRT) THEN
               INSERT = .TRUE.
C                                       Delete a character
            ELSE IF (ICHAR.EQ.DELETE) THEN
               INP = INP + 1
C                                       Change to blank
            ELSE IF (ICHAR.EQ.SPACE) THEN
               KARBUF(KOP:KOP) = ' '
               KOP = KOP + 1
               INP = INP + 1
C                                       Keep old character
            ELSE IF (ICHAR.EQ.' ') THEN
               KARBUF(KOP:KOP) = ' '
               IF (INP.LE.LBYTES) CALL H2CHR (1, INP, BWTBUF(J+2),
     *            KARBUF(KOP:KOP))
               INP = INP + 1
               KOP = KOP + 1
C                                       New character
            ELSE
               KARBUF(KOP:KOP) = ICHAR
               IF ((ICHAR.EQ.SPACE) .OR. (ICHAR.EQ.INSRT) .OR.
     *            (ICHAR.EQ.DELETE)) KARBUF(KOP:KOP) = ' '
               KOP = KOP + 1
               INP = INP + 1
               END IF
            END IF
 845     CONTINUE
C                                       Echo new line
 850  TEMP = ' ' // KARBUF(1:NCHLIN)
      CALL CHR2H (NCHLIN, KARBUF, 1, BWTBUF(J+2))
      INP = JTRIM (TEMP)
      I = MAX (INP, 1)
      CALL ZMSGOP (MSGLUN, MSGIND, 1, PHNAME, IERR)
      CALL ZTTYIO ('WRIT', MSGLUN, MSGIND, I, TEMP, IERR)
      CALL ZMSGCL (MSGLUN, MSGIND, IERR)
C                                       Write out record
      CALL ZFIO ('WRIT', BWTLUN, BWTIND, IREC, BWTBUF, IERR)
      IF (IERR.NE.0) POTERR = 50
      GO TO 960
C-----------------------------------------------------------------------
C                                        Close down area
C                                        close BAT or Q
 960  IF (BATOPN) CALL ZCLOSE (BATLUN, BATIND, IERR)
C                                        work file
      IF (BWTOPN) CALL ZCLOSE (BWTLUN, BWTIND, IERR)
C                                        POPS error mess
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QUEUE #',I6,' OUTSIDE RANGE 1 -',I3)
 1010 FORMAT ('JOB #',I7,' OUTSIDE RANGE 101 -',I4)
 1015 FORMAT ('JOB #',I7,' INVALID')
 1200 FORMAT ('USER #',I5,' CANNOT EDIT BATCH OF',I5)
 1210 FORMAT ('BATFLINE',F7.1,' OUT OF RANGE')
 1215 FORMAT ('BATFLINE TOO LARGE: ADDING CODE AT END')
 1220 FORMAT ('LINK FAILS TO LINE',I6)
 1300 FORMAT ('YOU MAY ONLY UNQUEUE/JOBLIST YOUR OWN JOBS')
 1301 FORMAT ('JOB DOES NOT EXIST')
 1302 FORMAT ('JOB ALREADY RUNNING')
 1303 FORMAT ('JOB STILL SUBMITTING')
 1304 FORMAT ('JOB ALREADY DONE')
 1305 FORMAT ('DEQUE ERROR: WRITE',I7,' CLOSE',I7)
 1310 FORMAT ('OPEN BATCH FILE ERROR',I7)
 1320 FORMAT ('BATCH FILE ERROR CLOSE',I7,' DESTROY',I7)
 1600 FORMAT ('Job',8X,'Submit time',10X,'Status',11X,'Ver')
 1605 FORMAT (I3,3X,A12,1X,A8,4X,A8,I6,4X,A3)
 1620 FORMAT ('Q FILE CLOSE ERROR',I7)
 1720 FORMAT ('NO LINES IN FILE')
 1723 FORMAT ('BATFLINE',I7,' EXCEEDS MAX LINE',I6)
 1735 FORMAT (I4)
 1750 FORMAT ('JOB REQUEUE OPEN ERROR',I7)
 1755 FORMAT ('JOB REQUEUE READ ERROR',I7)
 1760 FORMAT ('JOB REQUEUE WRITE ERROR',I7)
      END
