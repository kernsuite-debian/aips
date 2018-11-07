      PROGRAM AIPSB
C-----------------------------------------------------------------------
C! Batch version of AIPS program, runs non-interactive verbs and tasks
C# Batch POPS-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998-2000, 2004-2005, 2011-2013, 2016, 2018
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
C   AIPSB is the batch version of AIPS.  It is activated by QMNGR and
C   runs as NPOPS = NINTRN+2,+3,...  Now runs only 1 job.
C   Inputs: Job number ( 1 - 64 )
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   JOBNO, INHUSE(3)
      LOGICAL   FIRST, FAILED
      INCLUDE 'INCS:DBAT.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBWT.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCON.INC'
      DATA PRGNAM /'AIPSB '/
C-----------------------------------------------------------------------
      FIRST = .TRUE.
C                                        flag says not AIPSC
      WASERR = .TRUE.
      FAILED = .FALSE.
      STORE1 = 0
C                                        FIRST = T do start up from RC
C                                        FIRST = F close current job
 10   CALL BATSET (FIRST, JOBNO, INHUSE, FAILED)
      IF (JOBNO.GT.0) THEN
C                                        do usual AIPS
 20      CALL INIT
         FAILED = ERRNUM.GT.0
         IF (ERRNUM.EQ.0) GO TO 30
            ERRLEV = ERRLEV + 1
            IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
            CALL OERROR
            ERRNUM = 100
            CALL OERROR
            GO TO 10
C                                        read input till error/end
 30      CALL GTLINB
         FAILED = ERRNUM.GT.0
         IF (ERRNUM.LE.0) GO TO 40
            ERRLEV = ERRLEV + 1
            IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
            CALL OERROR
C                                        batch quits on all errors
 40      IF (ERRNUM.EQ.-1) GO TO 20
         GO TO 10
         END IF
C
      CALL ZDIE
C
 999  STOP
      END
      SUBROUTINE BATSET (FIRST, JOBNO, INHUSE, FAILED)
C-----------------------------------------------------------------------
C   BATSET start, renew, and close job operations for AIPSB.
C   In/out:
C      FIRST   L     T => talk to initiator & begin
C                    F => close up JOBNO
C      JOBNO   I     old/new job number (returns 0 if none)
C      INHUSE  I(3)  inhibit user stack
C      FAILED  L     current job ends badly
C-----------------------------------------------------------------------
      INTEGER   JOBNO, INHUSE(3)
      LOGICAL   FIRST, FAILED
C
      CHARACTER PRGNAM*6, PHNAME*48
      LOGICAL   RQUICK, T, F, DORES
      INTEGER   NQUEUE, INPOPS, SCRTCH(256), IER, MAGIC, IJOB, IERR, I,
     *   J
      REAL      XJOBNO, EPS, DELTIM, DUMTIM, RTEMP
      INCLUDE 'INCS:DBAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DRUN.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA PRGNAM /'AIPSB '/
      DATA MAGIC /-32000/
      DATA EPS, DELTIM, DUMTIM /0.01, 300.0, 0.0/
C-----------------------------------------------------------------------
C                                        FIRST time: talk to initiator
      IF (.NOT.FIRST) GO TO 100
C                                        init initialize
         XJOBNO = 0.0
         TSKNAM = PRGNAM
         CALL ZDCHIN (.TRUE.)
         CALL HIINIT (3)
         FIRST = .FALSE.
         JOBNO = 0
         CALL FILL (3, 0, INHUSE)
         NQUEUE = -1
         INPOPS = 1
         CALL GTPARM (PRGNAM, 1, RQUICK, XJOBNO, BATDAT, IERR)
         IF ((IERR.GT.1) .AND. (IERR.NE.3)) THEN
            WRITE (MSGTXT,1000) IERR
            GO TO 980
            END IF
         DORES = IERR.EQ.0
C                                        Force no TV, TK
         NTVDEV = 0
         NTKDEV = 0
C                                        set NPOPS
         NQUEUE = NPOPS - NINTRN - 1
         IJOB = ABS(XJOBNO) + EPS
         IF ((NQUEUE.LT.1) .OR. (NQUEUE.GT.NBATQS)) THEN
            WRITE (MSGTXT,1010) NQUEUE, NBATQS
            GO TO 980
            END IF
         IF ((IJOB.LT.1) .OR. (IJOB.GT.64)) THEN
            WRITE (MSGTXT,1020) IJOB
            GO TO 980
            END IF
         BATLUN = 4
C                                        restart initiator
         IF (DORES) CALL RELPOP (0, SCRTCH, IERR)
         RTEMP = 10.0
         IF (AIPSMK.GT.0.0) RTEMP = RTEMP / AIPSMK
         CALL ZDELAY (RTEMP, IER)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR
            CALL MSGWRT (7)
            END IF
         CALL RANDIN (I)
         GO TO 200
C-----------------------------------------------------------------------
C                                        Other times: close the job
 100  CONTINUE
C                                        close input
         CALL ZCLOSE (BATLUN, BATIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR
            CALL MSGWRT (7)
            END IF
C                                       destroy input file
         IF (.NOT.FAILED) THEN
            CALL ZPHFIL ('BA', 1, JOBNO, NPOPS, PHNAME, IERR)
            CALL ZDESTR (1, PHNAME, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1110) PHNAME(1:24), IERR
               CALL MSGWRT (7)
               END IF
            END IF
C                                       Destroy Task save fil(s)
 130     IJOB = 400 + NPOPS
         MSGSUP = 32000
         DO 135 I = 1,36
            J = I - 1
            CALL ZPHFIL ('TG', 1, IJOB, J, PHNAME, IERR)
            CALL ZDESTR (1, PHNAME, IERR)
 135        CONTINUE
         MSGSUP = 0
C                                       print message file:
C                                       not done any more
         NQUEUE = NPOPS - NINTRN - 1
         IJOB = 100*NQUEUE + JOBNO
         WRITE (MSGTXT,1130) IJOB, NLUSER
         CALL MSGWRT (5)
         CALL MSGWRT (-1)
         CALL ACOUNT (2)
C                                        clear Q
         IF (FAILED) THEN
            CALL BATQ ('FAIL', DUMTIM, ' ', NQUEUE, INHUSE, NLUSER,
     *         JOBNO, BATDAT, IERR)
         ELSE
            CALL BATQ ('CLOS', DUMTIM, ' ', NQUEUE, INHUSE, NLUSER,
     *         JOBNO, BATDAT, IERR)
            END IF
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1131) JOBNO, IERR
            CALL MSGWRT (7)
            END IF
C                                       only 1 job
         JOBNO = 0
         GO TO 900
C-----------------------------------------------------------------------
C                                        open input file
 200  BATREC = 1
      CALL ZPHFIL ('BA', 1, IJOB, NPOPS, PHNAME, IERR)
      CALL ZOPEN (BATLUN, BATIND, 1, PHNAME, F, T, T, IERR)
      NUMRUN = 0
      CALL FILL (MAXRUN, 0, LUNRUN)
C                                        leave file as failed
      IF (IERR.EQ.0) GO TO 215
 210     JOBNO = IJOB
         IJOB = 100*NQUEUE + IJOB
         CALL BATQ ('FAIL', DUMTIM, ' ', NQUEUE, INHUSE, NLUSER, JOBNO,
     *      BATDAT, IERR)
         WRITE (MSGTXT,1210) IJOB, NLUSER, IERR
         GO TO 980
C                                        read record
 215  CALL ZFIO ('READ', BATLUN, BATIND, 1, BATDAT, IERR)
      IF (IERR.EQ.0) GO TO 220
         CALL ZCLOSE (BATLUN, BATIND, IER)
         GO TO 210
C                                        start up message
 220  JOBNO = IJOB
      IJOB = 100*NQUEUE + IJOB
C                                       Create private or public catlgs
      I = UCTSIZ
      IF (I.LE.0) I = 100
      CALL CATCR (0, I, SCRTCH, IER)
 230  WRITE (MSGTXT,1230) IJOB, NLUSER
      CALL MSGWRT (4)
      GO TO 999
C-----------------------------------------------------------------------
C                                        end BATCH: normal
 900  WRITE (MSGTXT,1900)
      CALL MSGWRT (2)
      CALL MSGWRT (-1)
      GO TO 990
C                                        bad trouble
 980  NPOPS = NINTRN + 2
      CALL MSGWRT (10)
      WRITE (MSGTXT,1980) NQUEUE, IERR
      CALL MSGWRT (10)
      CALL MSGWRT (-1)
C
 990  CALL ACOUNT (2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GTPARM ERROR',I5)
 1010 FORMAT ('NQUEUE =',I5,' OUTSIDE RANGE 1 TO',I3)
 1020 FORMAT ('JOB NUMBER =',I6,' OUTSIDE RANGE 1 TO 64')
 1030 FORMAT ('RESUME QMNGR ERROR',I7)
 1100 FORMAT ('CLOSE INPUT ERROR',I7)
 1110 FORMAT ('DESTROY ',A24,' ERROR',I7)
 1130 FORMAT ('Job',I4,' for',I5,' concludes')
 1131 FORMAT ('CLOSE Q ERROR:',2I7)
 1139 FORMAT ('PRTMSG ERRORS: IERR, ERRNUM',2I7)
 1210 FORMAT ('OPEN INPUT FILE FOR',I4,I5,' ERROR',I7,' FILE LEFT AS',
     *   ' FAILED')
 1230 FORMAT ('Job',I4,' for',I5,' begins')
 1900 FORMAT ('Batch job concludes')
 1980 FORMAT ('BATCH',I7,' CAN''T START',I7)
      END
      SUBROUTINE GTLINB
C-----------------------------------------------------------------------
C   GTLINB is the main POPS routine.  It causes lines to be read by
C   PREAD, parsed and compiled or executed(pseudo verbs) by POLISH,
C   and finally executed by INTERB.  GTLINB returns only on error.
C   Version for AIPSB (Batch AIPS).
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
      EQUIVALENCE (STACK(1), A(1)),        (CSTACK (1), B(1))
      DATA PRGNAM /'GTLINB'/
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
C                                       Compile a line.
            CALL POLISH
            IF (ERRNUM.NE.0) GO TO 980
C
C                                       MODE = 1 -> compilitation
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
         CALL INTERB (KKT(2))
         IF (ERRNUM.NE.0) GO TO 980
         GO TO 10
C                                        error return
 980  ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
      SUBROUTINE INTERB (KENTRY)
C-----------------------------------------------------------------------
C   INTERB causes the POPS code to be executed: placing operands on
C   the V and STACK stacks and calling VERBSB and QUICK for verbs.
C   This version for AIPSB (Batch AIPS).
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
      DATA PRGNAM /'INTERB'/
C-----------------------------------------------------------------------
      IPT = '#'
      SP = 0
      CP = 0
      SP0 = 1
      LINK = K(KENTRY)
      L = KENTRY
C                                       Program chunk begins.
 10   L = L + 1
C                                          Advance program counter.
 20      L = L + 1
 25         J = K(L)
C                                       Debug.
               IF (IDEBUG.GT.0) THEN
                  KK = CSTACK(CP)
                  WRITE (MSGTXT,1025) L, J, SP, SP0, STACK(SP),
     *               V(SP), CP, CSTACK(CP), C(KK)
                  CALL MSGWRT (4)
                  END IF
C                                       Operand - push on stack.
 30            IF (J.LE.0) THEN
                  SP = SP + 1
                  POTERR = 7
                  IF ((SP.LT.1) .OR. (SP.GT.SLIM)) GO TO 980
                  IF ((CP.LT.0) .OR. (CP.GT.SLIM)) GO TO 980
                  J1 = -J
                  STACK(SP) = J1
                  V(SP) = C(J1)
C                                       Verb linkages.
               ELSE IF ((J.GE.200) .AND. (J.LE.999)) THEN
                  CALL VERBSB (J)
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
 50                  SP = SP - 1
                     STACK(SP) = 0
 60                  V(SP) = XX
                  END IF
               GO TO 20
C                                       Error exit.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C                                       Normal exit.
 995  IPT = '>'
      IF ((ERRNUM.NE.0) .OR. ((SP.LE.0) .AND. (CP.LE.0))) GO TO 999
         WRITE (MSGTXT,1995)
         CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1025 FORMAT ('INTERB:',2I6,2I3,I5,G12.5,2I5,G12.5)
 1995 FORMAT ('INTERB: STACKS NOT EMPTY WHEN LINE DONE')
      END
      SUBROUTINE VERBSB (J)
C-----------------------------------------------------------------------
C   VERBSB establishes the correspondance between verb operators
C   and their subroutine calls.  VERSION for batch AIPSn (AIPSBn).
C   Inputs:
C      J    I      verb number
C-----------------------------------------------------------------------
      INTEGER   J
C
      INTEGER   NUMSUB
      PARAMETER (NUMSUB = 36)
      CHARACTER PRGNAM*6
      INTEGER   JJJJ,  IAB(NUMSUB), IAE(NUMSUB), I, IWSAVE(9)
      REAL      RWSAVE(30)
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DPOP.INC'
      DATA PRGNAM /'VERBSB'/
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
         CALL AU1 (JJJJ)
         GO TO 980
 11   CONTINUE
         CALL AU1A (JJJJ)
         GO TO 980
 20   CONTINUE
         IF (JJJJ.EQ.6) GO TO 970
         CALL AU2 (JJJJ)
         GO TO 980
 21   CONTINUE
         CALL AU2A (JJJJ)
         GO TO 980
 30   CONTINUE
         CALL AU3 (JJJJ)
         GO TO 980
 31   CONTINUE
         CALL AU3A (JJJJ)
         GO TO 980
 32   CONTINUE
         CALL AU3B (JJJJ)
         GO TO 980
 40   CONTINUE
C        CALL AU4 (JJJJ)
         GO TO 970
 41   CONTINUE
         CALL AU4A (JJJJ)
         GO TO 980
 50   CONTINUE
C        CALL AU5 (JJJJ)
         GO TO 970
 51   CONTINUE
C        CALL AU5A(JJJJ)
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
         CALL AU6D (JJJJ)
         GO TO 980
 65   CONTINUE
C        CALL AU6E (JJJJ)
         GO TO 970
 70   CONTINUE
         CALL AU7 (JJJJ)
         GO TO 980
 71   CONTINUE
         CALL AU7A (JJJJ)
         GO TO 980
 72      CONTINUE
         CALL AU7B (JJJJ)
         GO TO 980
 80   CONTINUE
         CALL AU8 (JJJJ)
         GO TO 980
 81   CONTINUE
         CALL AU8A (JJJJ)
         GO TO 980
 90   CONTINUE
         CALL AU9 (JJJJ)
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
         CALL AUO (JJJJ)
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
