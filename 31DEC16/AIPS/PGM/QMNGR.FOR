      PROGRAM QMNGR
C-----------------------------------------------------------------------
C! manages the AIPS batch queues, starting AIPSBs to run the jobs
C# Batch
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
C   QMNGR is the AIPS batch queue manager.  It's started by one of the
C   AIPSC (Checker) and runs as NPOPS = NINTRN+2.
C   Inputs: POPS number of routine which started it.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IBUF(512), IERR
      LOGICAL   RQUICK
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA PRGNAM /'QMNGR '/
C-----------------------------------------------------------------------
C                                       init the queues and job
 10   CALL QMNGIN (PRGNAM, IBUF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Do it until all empty
 20   CALL QMNGDO (IBUF, IERR)
C
 990  RQUICK = .TRUE.
      CALL DIETSK (IERR, RQUICK, IBUF)
C
 999  STOP
      END
      SUBROUTINE QMNGIN (PRGM, IBUF, IRET)
C-----------------------------------------------------------------------
C   QMNGIN starts up the Q manager program, resumes the initiator,
C   and switches to user number 1.
C   Inputs:
C      PRGM    C*6)      Program name
C   Output:
C      IBUF    I(512)    Scratch
C      IRET    I         0 => keep going, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IBUF(512), IRET
C
      INTEGER   IERR, INPOPS
      LOGICAL   RQUICK
      REAL      STARTR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        FIRST time: talk to initiator
C                                        initialize
      IRET = 1
      STARTR = 0.0
      TSKNAM = PRGM
      CALL ZDCHIN (.TRUE.)
      INPOPS = 1
      CALL GTPARM (PRGM, 1, RQUICK, STARTR, IBUF, IERR)
      IF ((IERR.GT.1) .AND. (IERR.NE.3)) THEN
         NPOPS = INPOPS
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (10)
         GO TO 999
         END IF
C                                        Force no TV, TK
      NTVDEV = 0
      NTKDEV = 0
C                                        set NPOPS of starter
      INPOPS = ABS(STARTR) + 0.1
C                                        restart initiator
      IF (IERR.EQ.0) THEN
         CALL RELPOP (0, IBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) INPOPS, IERR
            CALL MSGWRT (6)
            END IF
         END IF
C                                       go to user 1
 20   CALL ACOUNT (2)
      NLUSER = 1
      CALL ACOUNT (1)
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BATCH Q MANAGER CAN''T START',I7)
 1010 FORMAT ('RESUME INITIATOR',I4,' ERROR',I7)
      END
      SUBROUTINE QMNGDO (IBUF, IRET)
C-----------------------------------------------------------------------
C   QMNGDO begins by clearing any supposedly running jobs in any Q for
C   which there is not a current AIPSB.  Then it goes into a loop with
C   a delay looking for jobs which need to be run.  If there are none
C   it will exit.  If there is one that is ready, it will start the
C   appropriate AIPSBn.
C   Output:
C      IBUF   I(512)     scratch
C      IRET   I          0 => no errors, 1 => error occurred
C-----------------------------------------------------------------------
      INTEGER   IBUF(512), IRET
C
      CHARACTER AIPSB*6, ANAME*6, AIPVER*48, PHNAME*48
      INTEGER   NQ, OUPOPS, IERR, PID(4), ILUSER, INHUSE(3,12), JOBNO,
     *   TDLUN, TDIND, ISCRTC(256), ITEMP, IRETCD, IT, IREC
      LOGICAL   T, F, ACTIVE, ISJOB
      REAL      TDELAY, RSCRTC(256), TDL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (ISCRTC, RSCRTC)
      DATA T, F /.TRUE.,.FALSE./
      DATA AIPSB /'AIPSB '/
      DATA TDLUN /13/
C-----------------------------------------------------------------------
      IRET = 1
      CALL FILL (36, 0, INHUSE)
      IT = 0
C                                       MAIN LOOP
 100  ISJOB = .FALSE.
         TDELAY = 10.0
         IF (AIPSMK.GT.1.0) TDELAY = TDELAY / AIPSMK
         CALL ZDELAY (TDELAY, IERR)
         DO 120 NQ = 1,NBATQS
            OUPOPS = NINTRN + 1 + NQ
            CALL BLDTNM (AIPSB, OUPOPS, ANAME, IERR)
            CALL FILL (4, 0, PID)
            CALL ZTACTQ (ANAME, PID, ACTIVE, IERR)
            IF ((IERR.EQ.0) .AND. (ACTIVE)) THEN
               ISJOB = .TRUE.
            ELSE IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1100) NQ, IERR
               GO TO 990
C                                       Clear any failed batch jobs
            ELSE
               CALL BATQ ('FAIL', TDELAY, NQ, INHUSE, ILUSER, JOBNO,
     *            IBUF, IERR)
               IF ((IERR.NE.0) .AND. (IERR.NE.3)) THEN
                  WRITE (MSGTXT,1110) 'FAIL', NQ, IERR
                  GO TO 990
                  END IF
C                                       A job ready to run?
               CALL BATQ ('FIND', TDELAY, NQ, INHUSE(1,NQ), ILUSER,
     *            JOBNO, IBUF, IERR)
               IF ((IERR.GT.0) .AND. (IERR.NE.3)) THEN
                  WRITE (MSGTXT,1110) 'FIND', NQ, IERR
                  GO TO 990
                  END IF
               IF (IERR.EQ.0) GO TO 200
               IF (IERR.EQ.-1) ISJOB = .TRUE.
               END IF
 120        CONTINUE
         IF (.NOT.ISJOB) GO TO 900
            IF (IT.LT.10) IT = IT + 1
            IF (IT.LT.10) GO TO 100
               TDELAY = 140.0
               IF (AIPSMK.GT.1.0) TDELAY = TDELAY / SQRT (AIPSMK)
               CALL ZDELAY (TDELAY, IERR)
               GO TO 100
C                                       There is a job to run
 200  CONTINUE
         IT = 0
C                                       open task data file: use
C                                       the batch guy's NPOPS area
         ITEMP = JOBNO + 100 * NQ
         WRITE (MSGTXT,1200) ITEMP, ILUSER, OUPOPS
         CALL MSGWRT (4)
         CALL ZPHFIL ('TD', 1, 0, 4, PHNAME, IERR)
         CALL ZOPEN (TDLUN, TDIND, 1, PHNAME, F, T, T, IERR)
         IF (IERR.EQ.0) GO TO 210
            WRITE (MSGTXT,1201) IERR
            GO TO 990
C                                        Parms data
 210     CALL FILL (256, 0, ISCRTC)
         ISCRTC(1) = ILUSER
         ISCRTC(2) = 0
         ISCRTC(3) = 0
         ISCRTC(4) = 0
         ISCRTC(5) = 32000
         ISCRTC(6) = 0
         ISCRTC(7) = 0
         ISCRTC(8) = 0
         RSCRTC(9) = 1.
         CALL CHR2H (4, VERNAM, 1, RSCRTC(10))
         RSCRTC(11) = JOBNO
         IREC = (OUPOPS-1) * 4 + 2
         CALL ZFIO ('WRIT', TDLUN, TDIND, IREC, ISCRTC, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1215) IERR
            GO TO 990
            END IF
         CALL ZFIO ('READ', TDLUN, TDIND, 1, ISCRTC, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1215) IERR
            GO TO 990
            END IF
         ITEMP = (OUPOPS-1) * 5 + 1
         CALL CHR2H (8, AIPSB, 1, ISCRTC(ITEMP))
         IRETCD = -999
         ISCRTC(ITEMP+2) = IRETCD
         ISCRTC(ITEMP+3) = 0
         ISCRTC(ITEMP+4) = 0
         CALL ZFIO ('WRIT', TDLUN, TDIND, 1, ISCRTC, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1215) IERR
            GO TO 990
            END IF
         CALL ZCLOSE (TDLUN, TDIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1225) IERR
            GO TO 990
            END IF
C                                        activate it finally
 230     CALL FILL (4, 0, PID)
         AIPVER = VERNAM
         CALL ZACTV8 (AIPSB, OUPOPS, AIPVER, PID, IERR)
         IF (IERR.EQ.0) GO TO 235
            WRITE (MSGTXT,1230) NQ, IERR
            GO TO 990
C                                        suspend until AIPSB ok
C                                        read return code
 235     TDL = 3.0
         IF (AIPSMK.GT.0.0) TDL = 3.0 / MAX (0.5, AIPSMK)
         CALL TASKWT (ANAME, OUPOPS, PID, F, TDL, ISCRTC, IRETCD, IERR)
C                                       ABORT not resume via TD
 250     IF ((IRETCD.NE.-999) .AND. (IERR.EQ.0)) GO TO 255
            WRITE (MSGTXT,1250) NQ, IERR
            CALL MSGWRT (8)
            GO TO 100
C                                       Okay.
 255     CONTINUE
            WRITE (MSGTXT,1255) NQ, IRETCD
            CALL MSGWRT (2)
            GO TO 100
C                                        end BATCH: normal
 900  WRITE (MSGTXT,1900)
      CALL MSGWRT (2)
      IRET = 0
      GO TO 999
C                                       error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ZTACTQ ON AIPSB QUEUE',I5,' ERROR',I7)
 1110 FORMAT (A4,' QUEUE',I5,' ERROR',I7)
 1200 FORMAT ('Start job',I5,' for user',I6,' as POPS',I4)
 1201 FORMAT ('OPEN TASK DATA FILE ERROR',I7)
 1215 FORMAT ('READ/WRITE TASK DATA FILE ERROR',I7)
 1225 FORMAT ('CLOSE TASK DATA FILE ERROR',I7)
 1230 FORMAT ('ACTVIVATE QUEUE',I3,' AIPSB ZACTV8 ERROR',I7)
 1250 FORMAT ('AIPSB QUEUE',I4,' TASKWT ERROR CODE',I4)
 1255 FORMAT ('AIPSB queue',I4,' resumes me normally.  Code=',I7)
 1900 FORMAT ('All batch jobs have concluded')
      END
