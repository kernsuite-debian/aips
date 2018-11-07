      PROGRAM AIPS
C-----------------------------------------------------------------------
C! Main AIPS program - calls POPS language, verb subroutines, runs tasks
C# POPS-appl TV-appl Graphics Tape Header
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2013, 2015-2017
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
C   AIPS is the main routine for the POPS driver.  It handles
C   initialization, error message production, the opening read from the
C   user, and final file closings.  It is also used to specify adequate
C   (preferably excess) space for all commons.  In particular, it must
C   specify FTAB's real dimension and is hence machine dependent.
C   VERSION: INTERACTIVE (NPOPS=1-NINTRN)
C-----------------------------------------------------------------------
C                                       local
      CHARACTER PRGNAM*6
      INTEGER   IERR
      LOGICAL   FIRST
C                                       global
C                                       MODCOMP FTAB
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DBAT.INC'
      INCLUDE 'INCS:DBWT.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCON.INC'
C                                       global: used
      DATA PRGNAM /'AIPS  '/
C-----------------------------------------------------------------------
      FIRST = .TRUE.
      STORE1 = 0
C                                       Start up or restart
 10   CALL AIPBEG (PRGNAM, FIRST, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       process input till error
 20   CALL GTLINE
C                                       error
      CALL AIPERR (PRGNAM)
C                                       Restart requested
      IF (ERRNUM.EQ.-1) GO TO 10
      IF (ERRNUM.NE.-2) GO TO 20
C
 999  STOP
      END
      SUBROUTINE AIPBEG (PRGNAM, FIRST, IERR)
C-----------------------------------------------------------------------
C   AIPBEG performs start up operations for AIPS including, if FIRST,
C   calls to ZDCHIN et al and TVINIT.  In all cases, it gets the user
C   ID number and inits the POPS commmons.
C   Inputs:
C      PRGNAM  C*6   Task root name
C   In/Out:
C      FIRST   L     T => do TVINIT, ZDCHIN, etc
C   Output:
C      IERR    I     > 0 => not legal AIPS. QUIT!!!
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      LOGICAL   FIRST
      INTEGER   IERR
C
      CHARACTER PHNAME*48, CDATE*4
      LOGICAL   T, F
      INTEGER   LUNCRT, IFIND, IER, I, SCRBUF(256), J, JTRIM, IDATE(3)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBAT.INC'
      INCLUDE 'INCS:DBWT.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DDRC.INC'
      INCLUDE 'INCS:DISK.INC'
      INCLUDE 'INCS:DRUN.INC'
      SAVE LUNCRT, IFIND
      COMMON /AIPSCR/ SCRBUF
      DATA LUNCRT /5/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Overall init
      IF (FIRST) THEN
         TSKNAM = PRGNAM
         CALL ZDCHIN (.TRUE.)
         CALL ZWHOMI (IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZMYVER
         CALL HIINIT (3)
C                                       Open tty.
         CALL ZPHFIL ('TT', 1, 0, 0, PHNAME, IER)
         CALL ZOPEN (LUNCRT, IFIND, 1, PHNAME, F, T, T, IER)
         CALL RANDIN (IER)
C                                       init dynamic disks
         DO 10 I = 1,99
            IF (I.GT.NVOL) THEN
               DSKNAM(I) = ' '
               DSKHST(I) = ' '
            ELSE
               DSKHST(I) = HSTNAM
               J = JTRIM (HSTNAM)
               CALL CHLTOU (J, DSKHST(I))
               DSKNAM(I) = 'Leave this alone'
               END IF
 10         CONTINUE
         MINDSK = NVOL
         END IF
C                                       Interactive
      FIRST = F
      ISBTCH = 32000
      NUMRUN = 0
      CALL FILL (MAXRUN, 0, LUNRUN)
C                                       flag says not AIPSC
      WASERR = .TRUE.
C                                       read user #
      CALL RDUSER (BATDAT, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZCLOSE (LUNCRT, IFIND, IER)
C                                       Init accounting
      CALL ACOUNT (1)
C                                       prime TV parameters
      MSGSUP = 32000
      CALL TVOPEN (SCRBUF, IER)
      IF (IER.EQ.0) CALL TVCLOS (SCRBUF, IERR)
      MSGSUP = 0
C                                       Create private or public catlgs
C                                       Also create message files
      I = UCTSIZ
      IF (I.LE.0) I = 100
      CALL CATCR (0, I, BATDAT, IER)
      IF (IER.NE.0) THEN
         ERRNUM = 33
         IF (IER.GE.NVOL) THEN
            IERR = 10
            GO TO 900
            END IF
         END IF
C                                       GNU copyleft information
      MSGTXT = ' '
      MSGTXT(26:) = RLSNAM // 'AIPS:'
      CALL MSGWRT (2)
      CALL ZDATE (IDATE)
      IF (IDATE(1).LT.200) IDATE(1) = IDATE(1) + 1900
      WRITE (CDATE,1020) IDATE(1)
      MSGTXT = '     Copyright (C) 1995-' // CDATE //
     *   ' Associated Universities, Inc.'
      CALL MSGWRT (2)
      MSGTXT = '           AIPS comes with ABSOLUTELY NO WARRANTY;'
      CALL MSGWRT (2)
      MSGTXT = '                for details, type HELP GNUGPL'
      CALL MSGWRT (1)
      MSGTXT = 'This is free software, and you are welcome'
     *   // ' to redistribute it'
      CALL MSGWRT (1)
      MSGTXT = 'under certain conditions; type EXPLAIN GNUGPL' //
     *   ' for details.'
      CALL MSGWRT (1)
C                                       determine RW or read only
      CALL ZDRCHK
      IF (DSKLOK(1).GT.0) THEN
         MSGTXT = 'DISK ONE APPEARS TO BE READ-ONLY TO YOU: I QUIT'
         CALL MSGWRT (8)
         ERRNUM = 33
         IERR = 10
         GO TO 900
         END IF
C                                       POPS init
      CALL INIT
C                                       Initialize GNU readline
      CALL RLHIST ('OPEN')
C
      ISBTCH = 0
C                                       get LASTEXIT
      IF (ERRNUM.EQ.0) THEN
         CALL SGLAST (1)
         IF (ERRNUM.NE.0) THEN
            IF (ERRNUM.NE.45) CALL MSGWRT (8)
            ERRNUM = 0
            MSGTXT = 'Loading a brand new POPS vocabulary'
            CALL MSGWRT (8)
            CALL INIT
            END IF
         END IF
 900  IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         CALL OERROR
         END IF
      IF (IERR.NE.0) CALL ACOUNT (2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (I4)
      END
      SUBROUTINE AIPERR (PRGNAM)
C-----------------------------------------------------------------------
C   AIPERR handles errors and shut down operations for AIPS
C   Inputs:
C      PRGNAM   C*6   Task root name
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
C
      INTEGER   LUNMSG, LUNIN, LUNDE, I, IF, IERR, J
      LOGICAL   T, F
      INCLUDE 'INCS:DBAT.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUNMSG, LUNIN, LUNDE /12, 10, 11/
C-----------------------------------------------------------------------
C                                       Normal error
      IF (ERRNUM.LE.0) GO TO 10
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         CALL OERROR
C                                       -1 => RESTART
C                                       -2 => EXIT
 10   IF (ERRNUM.EQ.-1) THEN
         CALL OERROR
         ERRNUM = -1
         CALL ACOUNT (2)
         END IF
C                                       Exit requested or
C                                       abort forced
C                                       close files
      IF (ERRNUM.EQ.-2) THEN
         J = LUNMSG + 1
         DO 20 I = J,99
            CALL LSERCH ('SRCH', I, IF, F, IERR)
            IF (IERR.NE.0) CALL LSERCH ('SRCH', I, IF, T, IERR)
            IF (IERR.EQ.0) THEN
               IF ((I.EQ.LUNDE) .OR. (I.EQ.LUNIN)) THEN
                  CALL ZTCLOS (I, IF, IERR)
               ELSE
                  CALL ZCLOSE (I, IF, IERR)
                  END IF
               END IF
 20         CONTINUE
         J = LUNMSG - 1
         DO 30 I = 1,J
            CALL LSERCH ('SRCH', I, IF, F, IERR)
            IF (IERR.NE.0) CALL LSERCH ('SRCH', I, IF, T, IERR)
            IF (IERR.EQ.0) THEN
               IF ((I.EQ.LUNDE) .OR. (I.EQ.LUNIN)) THEN
                  CALL ZTCLOS (I, IF, IERR)
               ELSE
                  CALL ZCLOSE (I, IF, IERR)
                  END IF
               END IF
 30         CONTINUE
C                                       Close GNU readline
         CALL RLHIST ('CLOS')
C                                       message log file
         CALL MSGWRT (-1)
C                                       Accounting
         CALL ACOUNT (2)
         CALL ZSTAIP
         CALL ZDIE
         END IF
C
 999  RETURN
      END
      SUBROUTINE GTLINE
C-----------------------------------------------------------------------
C   GTLINE is the main POPS routine.  It causes lines to be read by
C   PREAD, parsed and compiled or executed(pseudo verbs) by POLISH,
C   and finally executed by INTRPR.  GTLINE returns only on error.
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
      DATA PRGNAM /'GTLINE'/
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
C                                       MODE = 1 -> compilation
            IF ((AP.LE.0) .OR. (MODE.NE.0)) THEN
               IF (MODE.NE.1) THEN
                  MODE = 0
                  LPGM = 2
                  END IF
               GO TO 20
               END IF
C                                       Mode 0 -> Immed. execute.
         KTLPSV = KTLP
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
         CALL INTRPR (KKT(2))
         IF (ERRNUM.NE.0) GO TO 980
         GO TO 10
C                                        error return
 980  ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
      SUBROUTINE INTRPR (KENTRY)
C-----------------------------------------------------------------------
C   INTRPR causes the POPS code to be executed: placing operands on
C   the V and STACK stacks and calling VERBS and QUICK for verbs.
C   Inputs:
C      KENTRY   I    pointer to first particle of executable code
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
      DATA PRGNAM /'INTRPR'/
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
               IF (J.LE.0) THEN
                  SP = SP + 1
                  POTERR = 7
                  IF ((SP.LT.1) .OR. (SP.GT.SLIM)) GO TO 980
                  IF ((CP.LT.0) .OR. (CP.GT.SLIM)) GO TO 980
                  J1 = -J
                  STACK(SP) = J1
                  V(SP) = C(J1)
C                                       Verb linkages.
               ELSE IF ((J.GE.200) .AND. (J.LE.999)) THEN
                  CALL VERBS (J)
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
 1025 FORMAT ('INTRPR:',2I6,2I3,I5,G12.5,2I5,G12.5)
 1995 FORMAT ('INTRPR: STACKS NOT EMPTY WHEN LINE DONE')
      END
      SUBROUTINE VERBS (J)
C-----------------------------------------------------------------------
C   VERBS establishes the correspondance between verb operators
C   and their subroutine calls.
C   Inputs:
C      J    I      verb number
C     MJK 84/Aug/21: Add AUTRP.
C-----------------------------------------------------------------------
      INTEGER   J
C
      INTEGER   NUMSUB
      PARAMETER (NUMSUB = 36)
      CHARACTER PRGNAM*6
      INTEGER   JJJJ, IAB(NUMSUB), IAE(NUMSUB), I, IWSAVE(9)
      REAL      RWSAVE(30)
      LOGICAL   BATCH
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DPOP.INC'
      DATA PRGNAM /'VERBS '/
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
      BATCH = (NPOPS.GT.NINTRN+1) .OR. (ISBTCH.EQ.32000)
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
         IF (.NOT.BATCH) CALL AU4 (JJJJ)
         GO TO 970
 41   CONTINUE
         CALL AU4A (JJJJ)
         GO TO 980
 50   CONTINUE
         IF (.NOT.BATCH) CALL AU5 (JJJJ)
         GO TO 970
 51   CONTINUE
         IF (.NOT.BATCH) CALL AU5A (JJJJ)
         GO TO 970
 52   CONTINUE
         IF (.NOT.BATCH) CALL AU5B (JJJJ)
         GO TO 970
 53   CONTINUE
         IF (.NOT.BATCH) CALL AU5C (JJJJ)
         GO TO 970
 54   CONTINUE
         IF (.NOT.BATCH) CALL AU5D (JJJJ)
         GO TO 970
 55   CONTINUE
         IF (.NOT.BATCH) CALL AU5E (JJJJ)
         GO TO 970
 56      CONTINUE
         IF (.NOT.BATCH) CALL AU5F (JJJJ)
         GO TO 970
 60   CONTINUE
         IF (.NOT.BATCH) CALL AU6 (JJJJ)
         GO TO 970
 61   CONTINUE
         IF (.NOT.BATCH) CALL AU6A (JJJJ)
         GO TO 970
 62   CONTINUE
         IF (.NOT.BATCH) CALL AU6B (JJJJ)
         GO TO 970
 63   CONTINUE
         IF (.NOT.BATCH) CALL AU6C (JJJJ)
         GO TO 970
 64   CONTINUE
         IF ((BATCH) .AND. (JJJJ.NE.2)) GO TO 970
         CALL AU6D (JJJJ)
         GO TO 980
 65   CONTINUE
         IF (.NOT.BATCH) CALL AU6E (JJJJ)
         GO TO 970
 70   CONTINUE
         CALL AU7 (JJJJ)
         GO TO 980
 71   CONTINUE
         CALL AU7A (JJJJ)
         GO TO 980
 72   CONTINUE
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
         IF (.NOT.BATCH) CALL AU9A (JJJJ)
         GO TO 970
 92   CONTINUE
         IF (.NOT.BATCH) CALL AU9B (JJJJ)
         GO TO 970
 93   CONTINUE
         IF (.NOT.BATCH) CALL AU9C (JJJJ)
         GO TO 970
 100  CONTINUE
         IF (.NOT.BATCH) CALL AUA (JJJJ)
         GO TO 970
 110  CONTINUE
         IF (.NOT.BATCH) CALL AUB (JJJJ)
         GO TO 970
 120  CONTINUE
         IF (.NOT.BATCH) CALL AUC (JJJJ)
         GO TO 970
 170  CONTINUE
         CALL AUO (JJJJ)
         GO TO 970
 190  CONTINUE
         IF (.NOT.BATCH) CALL AUT (JJJJ)
         GO TO 970
C                                       Not in batch
 970  IF ((ERRNUM.EQ.0) .AND. (BATCH)) ERRNUM = 60
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
