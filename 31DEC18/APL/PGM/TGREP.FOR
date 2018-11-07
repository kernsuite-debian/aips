      PROGRAM TGREP
C-----------------------------------------------------------------------
C! Lists TGD files with desired task name
C# Utility Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 2018
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
C   A service routine to find and list contents of TG files
C-----------------------------------------------------------------------
      INTEGER   NMAX
      PARAMETER (NMAX=1000)
C
      CHARACTER PHNAME*48, TNAME*8, MSGBUF*80, PRGNAM*6, OLDTG*3,
     *   CTEST*8, ATIME*8, ADATE*12
      INTEGER   IBLK(256), TTYLUN, TTYIND, IERR, LUN, FIND, IREC, IVER,
     *   TTY(2), I, IUSER, MXUSER, LREC, NTASK, NWPL, NLPR, IOFF, JTRIM,
     *   BUFFER(256), INUSER, IRET, NPARM, NC
      LOGICAL   T, F, RQUICK, ISTASK
      HOLLERITH XXTASK(2)
      CHARACTER TNMS(NMAX)*8, OLDTGS(NMAX)*3, ANAME*8
      INTEGER   ITIMES(6,NMAX), USERS(NMAX), KK, KKLAST, K
      DOUBLE PRECISION JDS(NMAX), JD
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (TTY(1), TTYLUN),  (TTY(2), TTYIND)
      DATA PRGNAM /'TGREP '/
      DATA T, F /.TRUE.,.FALSE./
      DATA TTYLUN, LUN /5, 16/
C-----------------------------------------------------------------------
C                                       AIPS init
      NPOPS = 1
      MSGCNT = -1
      NLUSER = 1
      TSKNAM = PRGNAM
      CALL ZDCHIN (.TRUE.)
      NPARM = 2
      IRET = 0
      ISTASK = .FALSE.
C                                       Are we a task?
      CALL GTPARM (PRGNAM, NPARM, RQUICK, XXTASK, BUFFER, IERR)
      IF (IERR.EQ.0) THEN
         INUSER = NLUSER
         CALL H2CHR (8, 1, XXTASK, TNAME)
         ISTASK = .TRUE.
         IF (RQUICK) CALL RELPOP (IRET, BUFFER, IERR)
         IRET = 8
         IF (TNAME.EQ.' ') THEN
            MSGTXT = 'YOU MUST SPECIFY A TASK'
            CALL MSGWRT (8)
            GO TO 995
            END IF
      ELSE
         NPOPS = 1
         INUSER = 1
         RQUICK = .TRUE.
         END IF
C                                       Open terminal
      IF (.NOT.ISTASK) THEN
         CALL ZOPEN (TTYLUN, TTYIND, 1, PHNAME, F, F, T, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPENING TERMINAL'
            CALL MSGWRT (8)
            GO TO 995
            END IF
         END IF
      MSGTXT = 'I look for tasks in TGET files'
      CALL MSGWRT (2)
C                                       Loop to ask for vol, cat #s
 10   IF (.NOT.ISTASK) THEN
         MSGBUF = 'Enter desired task name - QUIT to stop'
         CALL INQSTR (TTY, MSGBUF, 8, TNAME, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING FROM TERMINAL'
            CALL MSGWRT (8)
            GO TO 995
            END IF
         IF (IERR.LT.0) GO TO 995
         END IF
      NC = JTRIM (TNAME)
      CALL CHLTOU (8, TNAME)
      IF (TNAME.EQ.'QUIT') GO TO 995
      MXUSER = 36 * 36 * 36 - 1
      IVER = 0
      KK = 0
      DO 100 IUSER = 1,MXUSER
         KKLAST = KK + 1
         NLUSER = IUSER
         CALL ZPHFIL ('TG', 1, 800, IVER, PHNAME, IERR)
         CALL ZEXIST (1, PHNAME, I, IERR)
         IF (IERR.EQ.1) THEN
            CALL ZPHFIL ('TG', 1, 400, IVER, PHNAME, IERR)
            CALL ZEXIST (1, PHNAME, I, IERR)
            OLDTG = 'old'
         ELSE
            OLDTG = 'new'
            END IF
         NLUSER = INUSER
         IF (IERR.NE.0) GO TO 100
C                                       open file
 20      NLUSER = INUSER
         CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN TG FILE'
            GO TO 90
            END IF
         CALL ZFIO ('READ', LUN, FIND, 1, IBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ TG FILE REC 1'
            GO TO 90
            END IF
         NTASK = IBLK(2)
         LREC = 1
         NWPL = 5
         NLPR = 256 / NWPL
         DO 30 I = 1,NTASK
            IREC = I / NLPR + 1
            IF (IREC.NE.LREC) THEN
               CALL ZFIO ('READ', LUN, FIND, IREC, IBLK, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ TG FILE'
                  GO TO 90
                  END IF
               END IF
            IOFF = MOD (I, NLPR) * NWPL
            CALL H2CHR (8, 1, IBLK(IOFF+1), CTEST)
            IF (CTEST(:NC).EQ.TNAME(:NC)) THEN
               IF (KK.GE.KKLAST) THEN
                  DO 25 K = KKLAST,KK
                     IF (CTEST.EQ.TNMS(K)) GO TO 30
 25                  CONTINUE
                  END IF
               KK = KK + 1
               TNMS(KK) = CTEST
               CALL CATIME (2, IBLK(IOFF+3), ITIMES(1,KK))
               USERS(KK) = IUSER
               OLDTGS(KK) = OLDTG
               CALL DAT2JD (ITIMES(1,KK), JDS(KK))
C               CALL TIMDAT (ITIME(4), ITIME(1), ATIME, ADATE)
C               WRITE (MSGTXT,1030) IUSER, CTEST, ADATE, ATIME, OLDTG
C               CALL MSGWRT (5)
C               J = J + 1
               END IF
 30         CONTINUE
         CALL ZCLOSE (LUN, FIND, IERR)
         IF (OLDTG.EQ.'new') THEN
            NLUSER = IUSER
            CALL ZPHFIL ('TG', 1, 400, IVER, PHNAME, IERR)
            CALL ZEXIST (1, PHNAME, I, IERR)
            OLDTG = 'old'
            IF (IERR.EQ.0) GO TO 20
            END IF
         GO TO 100
C                                       error
 90      CALL MSGWRT (8)
C                                       close
         CALL ZCLOSE (LUN, FIND, IERR)
 100     CONTINUE
C                                       now print
      KKLAST = KK
C                                       find a task to print
 110  ANAME = 'ZZZZZZZZ'
      KK = 0
      DO 115 K = 1,KKLAST
         IF (TNMS(K).NE.' ') THEN
            IF (TNMS(K).LT.ANAME) THEN
               KK = K
               ANAME = TNMS(K)
               END IF
            END IF
 115     CONTINUE
      IF (KK.LE.0) ANAME = ' '
C                                       now sort by time
      IF (ANAME.NE.' ') THEN
 120     JD = 1.E10
         KK = 0
         DO 125 K = 1,KKLAST
            IF ((TNMS(K).NE.' ') .AND. (JDS(K).GT.0.0D0)) THEN
               IF ((JDS(K).LT.JD) .AND. (TNMS(K).EQ.ANAME)) THEN
                  KK = K
                  JD = JDS(K)
                  END IF
               END IF
 125        CONTINUE
         IF (KK.GT.0) THEN
            CALL TIMDAT (ITIMES(4,KK), ITIMES(1,KK), ATIME, ADATE)
            WRITE (MSGTXT,1030) USERS(KK), TNMS(KK), ADATE, ATIME,
     *         OLDTGS(KK)
            CALL MSGWRT (5)
            JDS(KK) = -1.E10
            TNMS(KK) = ' '
            GO TO 120
            END IF
         GO TO 110
         END IF
C                                       loop for more interactive
      IF (.NOT.ISTASK) GO TO 10
      IRET = 0
C
 995  IF (ISTASK) THEN
         NLUSER = INUSER
         CALL DIETSK (IRET, RQUICK, BUFFER)
      ELSE
         CALL ZCLOSE (TTYLUN, TTYIND, IERR)
         NLUSER = 1
         CALL ACOUNT (2)
         END IF
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' ON ',A)
 1030 FORMAT ('Found user',I6,' task ',A,2X,A12,A8,' TG vers ',A)
      END
