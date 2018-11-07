      SUBROUTINE AUA (BRANCH)
C-----------------------------------------------------------------------
C! verb to submit batch jobs to AIPSC and the QMNGR queues
C# POPS-appl Batch
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2007, 2010
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
C   AUA handles activating batch checker task (AIPSCn)
C   Input: BRANCH = 1 activate AIPSCn
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER PRGNAM*6, PHNAME*48, LVERSN*48, AVERSN*48, CVER(4)*4,
     *   CTASK*8, ANAME*8, CDUM*1, RHOST*24
      INTEGER   POTERR, FINDTD, FINDTT, LUNTTY, IPRBUF(256), IERR,
     *   IRETCD, J, LUNTD, NBLOCK, I, ITEMP, PID(4), IPTR, JOFF, IDUM
      REAL      PARBUF(128), TDL, RDUM
      LOGICAL   T, F, ACTIVE, LDOWT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IPRBUF(1), PARBUF(1))
      DATA PRGNAM /'AUA '/
      DATA LUNTD, LUNTTY /13,5/
      DATA CTASK /'AIPSC '/
      DATA T, F /.TRUE.,.FALSE./
      DATA CVER /'OLD:','NEW:','TST:','CVX:'/
C-----------------------------------------------------------------------
      IF (BRANCH.NE.1) GO TO 999
      NBLOCK = 4
      GO TO (100), BRANCH
C-----------------------------------------------------------------------
C                                       SUBMIT
C                                       start AIPSC
C-----------------------------------------------------------------------
 100  CALL ADVERB ('BATQUE', 'I', 1, 0, J, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF ((J.LT.1) .OR. (J.GT.NBATQS)) THEN
         POTERR = 40
         WRITE (MSGTXT,1100) J, NBATQS
         CALL MSGWRT (7)
         GO TO 980
         END IF
C                                       open output file
 105  CALL ZPHFIL ('TD', 1, 0, 4, PHNAME, IERR)
      CALL ZOPEN (LUNTD, FINDTD, 1, PHNAME, F, T, T, IERR)
      POTERR = 101
      IF (IERR.NE.0) GO TO 980
      CALL ZFIO ('READ', LUNTD, FINDTD, 1, IPRBUF, IERR)
      POTERR = 50
      IF (IERR.NE.0) GO TO 960
      NBLOCK = MAX (4, IPRBUF(256))
C                                      initial values
      JOFF = (NPOPS-1)*NBLOCK + 2
      CALL ADVERB ('VERSION', 'C', 1, 48, IDUM, RDUM, LVERSN)
      IF (ERRNUM.NE.0) GO TO 960
      CALL ZPHFIL ('HE', 1, 0, 0, PHNAME, IERR)
      CALL VERMAT (1, PHNAME, CTASK, LVERSN, I, AVERSN, IERR)
      IF (IERR.NE.0) GO TO 960
      CALL FILL (256, 0, IPRBUF)
      IPTR = 11
      IPRBUF(1) = NLUSER
      IPRBUF(2) = 0
      IPRBUF(3) = 0
      IPRBUF(4) = MSGKIL
      IPRBUF(5) = ISBTCH
      IPRBUF(6) = DBGAIP
      IPRBUF(7) = 0
      IPRBUF(8) = 0
      CALL ADVERB ('DOWAIT', 'R', 1, 0, IDUM, PARBUF(IPTR-2), CDUM)
      IF (ERRNUM.NE.0) GO TO 960
      CALL CHR2H (4, VERNAM, 1, PARBUF(IPTR-1))
      IF ((I.GE.1) .AND. (I.LE.4)) CALL CHR2H (4, CVER(I), 1,
     *    PARBUF(IPTR-1))
      IF ((I.GE.5) .AND. (I.LE.8)) CALL CHR2H (4, CVER(I-3), 1,
     *    PARBUF(IPTR-1))
      CALL ADVERB ('BATQUE', 'R', 1, 0, IDUM, PARBUF(IPTR), CDUM)
      IF (ERRNUM.NE.0) GO TO 960
      CALL ADVERB ('DETIME', 'R', 1, 0, IDUM, PARBUF(IPTR+1), CDUM)
      IF (ERRNUM.NE.0) GO TO 960
      CALL CHR2H (48, AVERSN, 1, PARBUF(IPTR+2))
      CALL ADVERB ('REMHOST', 'C', 1, 24, IDUM, RDUM, RHOST)
      IF (ERRNUM.NE.0) GO TO 960
      CALL CHR2H (24, RHOST, 1, PARBUF(IPTR+14))
      CALL ADVERB ('REMQUE', 'R', 1, 0, IDUM, PARBUF(IPTR+20), CDUM)
      IF (ERRNUM.NE.0) GO TO 960
      CALL ZFIO ('WRIT', LUNTD, FINDTD, JOFF, IPRBUF, IERR)
      POTERR = 50
      IF (IERR.NE.0) GO TO 960
C                                        pointer block (#1)
      CALL ZFIO ('READ', LUNTD, FINDTD, 1, IPRBUF, IERR)
      POTERR = 50
      IF (IERR.NE.0) GO TO 960
      ITEMP = 5*(NPOPS-1)
      CALL CHR2H (8, CTASK, 1, IPRBUF(ITEMP+1))
      IPRBUF(3+ITEMP) = -999
      IPRBUF(4+ITEMP) = 0
      IPRBUF(5+ITEMP) = 0
      IPRBUF(256) = NBLOCK
      CALL ZFIO ('WRIT', LUNTD, FINDTD, 1, IPRBUF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       close files
      CALL ZCLOSE (LUNTD, FINDTD, IERR)
C                                       task already running
      CALL BLDTNM (CTASK, NPOPS, ANAME, IERR)
      POTERR = 43
      IF (IERR.NE.0) GO TO 980
      CALL FILL (4, 0, PID)
      PID(1) = NLUSER
      CALL ZTACTQ (ANAME, PID, ACTIVE, IERR)
      IF (IERR.NE.0) GO TO 980
      POTERR = 42
      IF (ACTIVE) GO TO 980
C                                        close message file
      CALL MSGWRT (-1)
C                                        close input terminal
      IF ((NPOPS.GT.NINTRN+1) .OR. (ISBTCH.EQ.32000)) GO TO 155
         CALL LSERCH ('SRCH', LUNTTY, FINDTT, F, IERR)
         POTERR = 58
         IF (IERR.NE.0) GO TO 980
         CALL ZCLOSE (LUNTTY, FINDTT, IERR)
         IF (IERR.NE.0) GO TO 980
C                                        activate the task
 155  CALL ZACTV8 (CTASK, NPOPS, AVERSN, PID, IERR)
      POTERR = 101
      IF (IERR.NE.0) GO TO 950
C                                       Wait for resumption through
C                                       TD file or abort of task
      TDL = 3.
      CALL ADVERB ('DOWAIT', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 950
      IF (RDUM.GT.0.0) TDL = 8.
      IF (NPOPS.GT.NINTRN+1) TDL = 20.
      IF (ISBTCH.GT.32000) TDL = 20.
      LDOWT = TDL.GT.4.0
      IF (AIPSMK.GT.0.3) TDL = TDL / AIPSMK
      CALL TASKWT (ANAME, NPOPS, PID, LDOWT, TDL, IPRBUF, IRETCD, IERR)
C                                        reopen terminal
 180  IF ((NPOPS.LE.NINTRN+1) .AND. (ISBTCH.NE.32000)) CALL ZOPEN
     *   (LUNTTY, FINDTT, 1, PHNAME, F, T, T, IERR)
C                                       resumption message
      WRITE (MSGTXT,1180)
      CALL MSGWRT (2)
      IF (IRETCD.EQ.0) GO TO 999
         POTERR = 101
C                                       abort batch
         IF (NPOPS.GT.NINTRN+1) POTERR = 100
         IF (ISBTCH.EQ.32000) POTERR = 100
         WRITE (MSGTXT,1185) IRETCD
         IF (IRETCD.NE.-999) CALL MSGWRT (9)
         GO TO 980
C                                       errors: close files
 950  IF ((NPOPS.LE.NINTRN+1) .AND. (ISBTCH.NE.32000)) CALL ZOPEN
     *   (LUNTTY, FINDTT, 1, PHNAME, F, T, T, IERR)
      GO TO 980
 960  CALL ZCLOSE (LUNTD, FINDTD, IERR)
C                                       POPS error management
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.EQ.0) GO TO 999
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('BATQUE',I6,' OUTSIDE RANGE 1 -',I2)
 1175 FORMAT ('TASK DISAPPEARED WITHOUT RESUMING ME: STOPPING')
 1180 FORMAT ('Resumes')
 1185 FORMAT ('RETURN CODE',I7,' RECEIVED: STOPPING')
      END
