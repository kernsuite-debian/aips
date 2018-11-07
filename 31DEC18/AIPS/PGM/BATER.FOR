      PROGRAM BATER
C-----------------------------------------------------------------------
C! BATER is stand-alone task to prepare and submit text to batch AIPS
C# Service Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2003, 2010, 2013
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
C   BATER is a stand-alone AIPS-like program used to prepare and
C   submit text to batch AIPS.
C   It uses NPOPS = NINTRN  +  1.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IERR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DBAT.INC'
      INCLUDE 'INCS:DBWT.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DBCR.INC'
      DATA PRGNAM /'BATER '/
C-----------------------------------------------------------------------
      TSKNAM = PRGNAM
      CALL ZDCHIN (.TRUE.)
      CALL ZMYVER
      ISBTCH = 0
      NPOPS = NINTRN + 1
      WASERR = .TRUE.
C                                        init commons
 10   CALL BATINI (IERR)
      IF (IERR.NE.0) GO TO 999
C                                        process input until error
 20   CALL BATGTL
      IF (ERRNUM.LE.0) GO TO 30
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         CALL OERROR
         IUNIT = 1
C                                        loop all errors except EXIT
 30   IF (ERRNUM.NE.-2) GO TO 20
      WRITE (MSGTXT,1030)
      CALL MSGWRT (1)
      CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1030 FORMAT ('wishes you a good day')
      END
      SUBROUTINE BATGTL
C-----------------------------------------------------------------------
C   BATGTL is the main routine of BATER.  It reads input via PREAD,
C   parses it via BATPOL, and executes it via BATINT.  Returns only
C   on error.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DIO.INC'
      DATA PRGNAM /'BATGTL'/
C-----------------------------------------------------------------------
C                                        get line
 10   CALL PREAD (KARBUF)
      IF (ERRNUM.NE.0) GO TO 980
         AP = 0
         SP = 0
         MSGTXT(1:KARLIM) = JBUFF(1:KARLIM)
         CALL MSGWRT (0)
         IF (KARBUF(1:1).EQ.'*') GO TO 10
         KBPTR = 1
         CALL BATPOL
         IF (ERRNUM.NE.0) GO TO 980
            CALL BATINT
            IF (ERRNUM.EQ.0) GO TO 10
C                                        error
 980  ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
      SUBROUTINE BATINI (IRET)
C-----------------------------------------------------------------------
C   BATINI initializes commons for BATER
C-----------------------------------------------------------------------
      INTEGER   NSYMS
      PARAMETER (NSYMS=32)
      CHARACTER PHNAME*48, SYMBLS(NSYMS)*8
      INTEGER   LUNCRT, IF, IERR, LCOUNT(NSYMS), LTYPE(NSYMS), I, IRET
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DBCR.INC'
      INCLUDE 'INCS:DBAT.INC'
      INCLUDE 'INCS:DRUN.INC'
      DATA LUNCRT / 5/
      DATA T, F /.TRUE.,.FALSE./
      DATA SYMBLS / 'SUBMIT  ', 'BATCH   ', 'BATEDIT ', 'UNQUE   ',
     *              'BATCLEAR', 'BATLIST ', 'QUEUES  ', 'JOBLIST ',
     *              'BAMODIFY', 'INPUTS  ', 'CLRMSG  ', 'PRTMSG  ',
     *              'EXIT    ', 'HELP    ', '        ', '        ',
     *              '        ', '        ', '        ', 'DOCRT   ',
     *              'PRIORITY', 'PRNUMBER', 'PRTIME  ', 'PRTASK  ',
     *              'OUTPRINT', 'DETIME  ', 'BATQUE  ', 'JOBNUM  ',
     *              'BATFLINE', 'BATNLINE', 'REMHOST ', 'REMQUE  '/
      DATA LCOUNT / 6, 5, 7, 5, 8, 7, 6, 7, 8, 6,
     *              6, 6, 4, 4, 0, 0, 0, 0, 0, 5,
     *              8, 8, 6, 6, 8, 6, 6, 6, 8, 8, 7, 6/
      DATA LTYPE  / 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
     *              4, 4, 4, 4, 0, 0, 0, 0, 0, 1,
     *              1, 1, 1, 7, 7, 1, 1, 1, 1, 1, 7, 1/
C-----------------------------------------------------------------------
C                                        POPS controls pointers
      IUNIT = 1
      KARLIM = NCHLIN
      SLIM = 1000
      IDEBUG = 0
      MODE = 0
      IPT = '>'
      NUMRUN = 0
      CALL FILL (MAXRUN, 0, LUNRUN)
C                                        error com
      ERRNUM = 0
      ERRLEV = 0
C                                        message common
      MSGCNT = -1
      CALL ZOPEN (LUNCRT, IF, 1, PHNAME, F, T, T, IERR)
      CALL RDUSER (BATDAT, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL ACOUNT (1)
      CALL ZCLOSE (LUNCRT, IF, IERR)
      CALL CATCR (0, 0, BATDAT, IERR)
C                                        symbol table
      NKSYM = NSYMS
      DO 10 I = 1, NKSYM
         KSYM(I) = SYMBLS(I)
 10      CONTINUE
      CALL COPY (NKSYM, LCOUNT, KCOUNT)
      CALL COPY (NKSYM, LTYPE, KTYP)
      CALL RFILL (NKSYM, 0.0, KVALUE)
      KVALUE(20) = -1.0
C                                        strings: 100*addr + nchar
      KVALUE(24) =  105.
      KVALUE(25) =  248.
      KVALUE(31) =  324.
      NKSTR = NSYMS
      DO 20 I = 1,NKSTR
         KSTRNG(I) = ' '
 20      CONTINUE
      KSTRNG(1) = TSKNAM
C                                        stacks used
      AP = 0
      SP = 0
      CALL FILL (60, 0, CSTACK)
      CALL FILL (60, 0, STACK)
C                                       User instructions
      WRITE (MSGTXT,1010)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1011)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1012)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1013)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1014)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1015)
      CALL MSGWRT (1)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Hello.  I am a stand-alone program somewhat like AIPS.')
 1011 FORMAT ('However, my vocabulary is limited to verbs and adverbs')
 1012 FORMAT ('related to Batch jobs.  I can take more than one comma',
     *   'nd in a')
 1013 FORMAT ('line and do use min match.  Issue the command INPUTS')
 1014 FORMAT ('for a list of my full vocabulary and current adverb',
     *   ' values.')
 1015 FORMAT ('Have fun!')
      END
      SUBROUTINE BATINT
C-----------------------------------------------------------------------
C   BATINT interprets the stacks prepared by BATPOL.  It executes
C   =, EXIT, INPUTS, and HELP and calls PRTMSG, CUA, and CUB to do the
C   other verbs.
C-----------------------------------------------------------------------
      CHARACTER OP*4, IPRTSK*8, PHNAME*48, PRGNAM*6, PRTASK*6,
     *   LVERS*48, LPNAME*48, RHOST*24
      INTEGER   POTERR, J, NEXIT, NSUBM, NBATCH, NPRINT, NJOBL, I,
     *   NHELP, J1, J2, IUSER, NDONE, NLEFT, IERR, IROUND, NINPU,
     *   HLPLUN, FIND, NCLRM, IPOPS
      REAL      DOCRT, PRIOTY, DTIME, PRTIME, PRNUMB, LSIGN
      LOGICAL   EQUAL, F
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DBCR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (DOCRT,  KVALUE(20)),   (PRIOTY, KVALUE(21)),
     *            (PRNUMB, KVALUE(22)),   (PRTIME, KVALUE(23))
      EQUIVALENCE (PRTASK, KSTRNG(1)),    (LPNAME, KSTRNG(2)),
     *            (RHOST, KSTRNG(3))
      DATA NSUBM, NBATCH, NJOBL, NINPU, NHELP, NEXIT, NPRINT, NCLRM
     *    /1,2,9, 10,14, 13,12,11/
      DATA PRGNAM /'BATINT'/
      DATA F /.FALSE./
      DATA HLPLUN /11/
C-----------------------------------------------------------------------
C                                        jump pointer, identify op
 10   SP = SP + 1
      IF (SP.GT.AP) GO TO 999
         POTERR = 8
         IF (CSTACK(SP).EQ.1) GO TO 20
         IF (CSTACK(SP).EQ.7) GO TO 25
         IF (CSTACK(SP).EQ.4) GO TO 30
         GO TO 980
C-----------------------------------------------------------------------
C                                        adverbs: reals
 20   IF (SP+1.GT.AP) GO TO 980
C                                        next must be =
C                                        adverb/const after that
         I = CSTACK(SP+1)
         J = STACK(SP)
         LSIGN = 1.0
C                                        unary plus or minus
         IF (I.NE.3) GO TO 23
            SP = SP + 1
            LSIGN = V(SP)
            IF (SP+1.GT.AP) GO TO 980
            I = CSTACK(SP+1)
 23      IF ((I.NE.1) .AND. (I.NE.11)) GO TO 980
            IF (I.EQ.11) KVALUE(J) = LSIGN * V(SP+1)
            IF (I.EQ.1)  KVALUE(J) = LSIGN * KVALUE(STACK(SP+1))
            SP = SP + 1
            GO TO 10
C                                        char const after that
 25   IF (SP+1.GT.AP) GO TO 980
         I = CSTACK(SP+1)
         IF ((I.NE.7) .AND. (I.NE.14)) GO TO 980
            J = STACK(SP)
            J1 = MOD (KVALUE(J), 100.0) + 0.01
            J2 = (KVALUE(J) - J1) / 100.0 + 0.01
            KSTRNG(J2) = ' '
C                                       constant
            IF (I.EQ.7) GO TO 27
               I = MIN (STACK(SP+1), J1)
               CALL H2CHR (I, 1, V(SP+1), KSTRNG(J2))
               SP = SP + (STACK(SP+1) + 3) / 4
               GO TO 10
C                                       adverb
 27         CONTINUE
               I = KVALUE(STACK(SP+1)) + 0.01
               J = I / 100
               I = I - 100 * J
               I = MIN (I, J1)
               KSTRNG(J2)(1:I) = KSTRNG(J)(1:I)
               SP = SP + 1
               GO TO 10
C-----------------------------------------------------------------------
C                                        verbs
 30   IF (STACK(SP).GT.NKSYM) GO TO 980
      J = STACK(SP)
C                                        submit
 35   IF (J.NE.NSUBM) GO TO 40
         I = 1
         CALL CUA (I)
         IF (ERRNUM.NE.0) GO TO 980
         GO TO 10
C                                        work file ops
 40   IF ((J.LT.NBATCH) .OR. (J.GT.NJOBL)) GO TO 45
         I = J - NBATCH + 1
         CALL CUB (I)
         IF (ERRNUM.NE.0) GO TO 980
C                                        force pseudoverb on BATCH,EDIT
         IF (J.LE.NBATCH+1) GO TO 999
         GO TO 10
C                                        Inputs
 45   IF (J.NE.NINPU) GO TO 55
         WRITE (MSGTXT,1045)
         CALL MSGWRT (1)
         DO 50 I = 1,NKSYM
            IF ((KTYP(I).NE.1) .AND. (KTYP(I).NE.4) .AND.
     *         (KTYP(I).NE.7)) GO TO 50
            IF (KTYP(I).EQ.1) WRITE (MSGTXT,1046) KSYM(I), KVALUE(I)
            IF (KTYP(I).EQ.4) WRITE (MSGTXT,1047) KSYM(I)
            IF (KTYP(I).EQ.7) THEN
               WRITE (MSGTXT,1048) KSYM(I)
               J1 = MOD (KVALUE(I), 100.0) + 0.01
               J2 = (KVALUE(I) - J1) / 100.0 + 0.01
               J = 19
               MSGTXT(J:J+J1-1) = KSTRNG(J2)(1:J1)
               J = J + J1
               MSGTXT(J:J) = ''''
               END IF
            CALL MSGWRT (1)
 50         CONTINUE
         GO TO 10
C                                        help
 55   IF (J.NE.NHELP) GO TO 80
C                                        get desired help name
         I = 0
         SP = SP + 1
         IF (SP.LE.AP) I = STACK(SP)
         IF (I.LE.0) IPRTSK = TSKNAM
         IF (I.GT.0) IPRTSK = KSYM(I)
         CALL ZPHFIL ('HE', 1, 0, 0, PHNAME, IERR)
         LVERS = 'NEW'
         CALL ZTOPEN (HLPLUN, FIND, 1, PHNAME, IPRTSK, LVERS, F, IERR)
         POTERR = 31
         IF (IERR.NE.0) GO TO 980
C                                       skip inputs
 60      CALL ZTREAD (HLPLUN, FIND, MSGTXT, IERR)
            IF (IERR.EQ.2) GO TO 70
            IF (IERR.NE.0) GO TO 75
            EQUAL = MSGTXT(1:4) .EQ. '----'
            IF (.NOT.EQUAL) GO TO 60
 65      CALL ZTREAD (HLPLUN, FIND, MSGTXT, IERR)
            IF (IERR.EQ.2) GO TO 70
            IF (IERR.NE.0) GO TO 75
            IF (MSGTXT(1:1).EQ.';') GO TO 65
            EQUAL = MSGTXT(1:4) .EQ. '----'
            IF (EQUAL) GO TO 70
               CALL MSGWRT (1)
               GO TO 65
 70      CALL ZTCLOS (HLPLUN, FIND, IERR)
         GO TO 10
 75      CALL ZTCLOS (HLPLUN, FIND, IERR)
         POTERR = 50
         GO TO 980
C                                        message print and exit
 80   IF ((J.LT.NCLRM) .OR. (J.GT.NEXIT)) GO TO 10
         I = PRIOTY + 0.1
         IUSER = NLUSER
         OP = 'DELE'
         IF (J.EQ.NPRINT) OP = 'PRIN'
         DTIME = PRTIME
         IF (J.EQ.NEXIT) DTIME = 3.0
         IPOPS = IROUND (PRNUMB)
         IF (IPOPS.EQ.0) IPOPS = NPOPS
         IF (IPOPS.LT.0) IPOPS = 0
         IPRTSK = PRTASK
         IF (J.NE.NEXIT) CALL PRTMSG (OP, IUSER, IPOPS, I, IPRTSK,
     *      DTIME, DOCRT, LPNAME, NDONE, NLEFT, IERR)
         IF (J.EQ.NEXIT) CALL PRTMSG (OP, IUSER, 0, 0, '      ',
     *      DTIME, DOCRT, LPNAME, NDONE, NLEFT, IERR)
         IF ((ERRNUM.NE.0) .AND. (J.EQ.NEXIT)) ERRNUM = -2
         IF (ERRNUM.NE.0) GO TO 980
         IF (J.EQ.NPRINT) WRITE (MSGTXT,1080) NDONE
         IF (J.NE.NPRINT) WRITE (MSGTXT,1081) NDONE
         IF (NDONE.GT.0) CALL MSGWRT (1)
         IF (J.NE.NEXIT) GO TO 10
            POTERR = -2
            GO TO 980
C-----------------------------------------------------------------------
C                                        errors
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1045 FORMAT ('Name',7X,'Type',3X,'Value')
 1046 FORMAT (A8,2X,'Adverb',F7.1)
 1047 FORMAT (A8,3X,'Verb')
 1048 FORMAT (A8,2X,'Adverb ''')
 1080 FORMAT ('Printed',I7,' messages')
 1081 FORMAT ('Deleted',I7,' messages')
      END
      SUBROUTINE BATPOL
C-----------------------------------------------------------------------
C   BATPOL is a simplified parser/compiler used by BATER.  It
C   converts text in KARBUF to type, and tag stacks in CSTACK and
C   STACK, resp. with AP pointing at the last entry.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, M*1, KPAC*48
      INTEGER   POTERR, J, NIPL, ICL, IPL(20), J1, I, NHELP, NINPU
      LOGICAL   FLAG
      DOUBLE PRECISION    DBLX
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DBCR.INC'
      INCLUDE 'INCS:DPOP.INC'
      DATA PRGNAM, NHELP, NINPU /'BATPOL', 14,10/
C-----------------------------------------------------------------------
 5    NKAR = 0
C                                        skip leading blanks
 10   IF (KBPTR.GT.KARLIM) GO TO 70
         M = KARBUF(KBPTR:KBPTR)
         IF (M.NE.' ') GO TO 20
         KBPTR = KBPTR + 1
         GO TO 10
C                                        classify by first character
 20   IF (M.EQ.';') GO TO 70
      IF (M.EQ.'=') GO TO 60
      IF (M.EQ.'''') GO TO 80
      IF (M.EQ.'+') GO TO 90
      IF (M.EQ.'-') GO TO 90
      IF (M.EQ.'.') GO TO 50
      IF ((M.GE.'A') .AND. (M.LE.'Z')) GO TO 30
      IF ((M.GE.'a') .AND. (M.LE.'z')) GO TO 30
      IF ((M.GE.'0') .AND. (M.LE.'9')) GO TO 50
      POTERR = 8
      GO TO 980
C                                        symbol: find end
 30   J = KBPTR
 35   NKAR = NKAR + 1
         J = J + 1
         POTERR = 4
         IF (J.GT.KARLIM) GO TO 980
         M = KARBUF(J:J)
         IF ((M.GE.'A') .AND. (M.LE.'Z')) GO TO 35
         IF ((M.GE.'a') .AND. (M.LE.'z')) GO TO 35
         IF (M.EQ.'_') GO TO 35
         IF ((M.GE.'0') .AND. (M.LE.'9')) GO TO 35
      POTERR = 5
      IF (NKAR.GT.8) GO TO 980
      KPAC = KARBUF(KBPTR:KBPTR+NKAR-1)
      KBPTR = KBPTR + NKAR
      CALL CHLTOU (NKAR, KPAC)
C                                        locate in table
C                                        use min. match
      NIPL = 0
      DO 40 I = 1,NKSYM
         IF (NKAR.GT.KCOUNT(I)) GO TO 40
            DO 38 ICL = 1,NKAR
               FLAG = KSYM(I)(ICL:ICL) .EQ. KPAC(ICL:ICL)
               IF (.NOT.FLAG) GO TO 40
 38            CONTINUE
            IF (NKAR.EQ.KCOUNT(I)) GO TO 45
               NIPL = NIPL + 1
               IPL(NIPL) = I
 40      CONTINUE
      POTERR = 2
      IF (NIPL.EQ.0) GO TO 980
      IF (NIPL.GT.1) GO TO 900
      I = IPL(1)
C                                        put in stack
 45   AP = AP + 1
      CSTACK(AP) = KTYP(I)
      STACK(AP) = I
      V(AP) = KVALUE(I)
      GO TO 5
C                                        numeric field: get value
 50   AP = AP + 1
      CSTACK(AP) = 11
      STACK(AP) = NKSYM + 2
      CALL GETNUM (KARBUF, KARLIM, KBPTR, DBLX)
      IF (ERRNUM.NE.0) GO TO 980
      V(AP) = DBLX
      GO TO 5
C                                        = op: not on stack
 60   POTERR = 8
      IF ((CSTACK(AP).NE.1) .AND. (CSTACK(AP).NE.7)) GO TO 980
      KBPTR = KBPTR + 1
      GO TO 5
C                                        ; ignored
 70   KBPTR = KBPTR + 1
C                                        HELP HELP or no arg
      IF (STACK(AP).NE.NHELP) GO TO 75
         IF ((AP.GT.1) .AND. (STACK(AP-1).NE.NHELP)) AP = AP + 1
         IF (AP.EQ.1) AP = 2
         STACK(AP) = 0
         IF (KBPTR.GT.KARLIM) GO TO 999
         GO TO 5
C                                        HELP INPUTS
 75   IF ((STACK(AP).EQ.NINPU) .AND. (AP.GT.1) .AND.
     *   (STACK(AP-1).EQ.NHELP)) STACK(AP) = 0
      IF (KBPTR.GT.KARLIM) GO TO 999
      GO TO 5
C                                        Character string
 80   AP = AP + 1
      CSTACK(AP) = 14
      CALL GETSTR (KARBUF, KARLIM, 48, KBPTR, KPAC, NKAR)
      CALL CHR2H (NKAR, KPAC, 1, V(AP))
      STACK(AP) = NKAR
      AP = AP + (NKAR-1)/4
      GO TO 5
C                                        unary plus, minus
 90   AP = AP + 1
      CSTACK(AP) = 3
      V(AP) = 1.0
      IF (M.EQ.'-') V(AP) = -1.0
      KBPTR = KBPTR + 1
      GO TO 5
C                                        errors
C                                        multi-match
 900  DO 910 I = 1,NIPL
         J1 = 2 * IPL(I) - 1
         WRITE (MSGTXT,1900) KSYM(J1)
         CALL MSGWRT (1)
 910     CONTINUE
C                                        set branch back to OERROR
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('SYMBOL NOT UNIQUE COULD BE ',A8)
      END
      SUBROUTINE CUA (BRANCH)
C-----------------------------------------------------------------------
C   CUA handles activating batch checker task (AIPSCCn)
C   This version of AUA has small symbol table.
C   Input: BRANCH = 1 activate AIPSn
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER PRGNAM*6, CTASK*8, ANAME*8, VERSON*48, PHNAME*48,
     *   RHOST*24
      INTEGER   POTERR, FINDTD, FINDTT, LUNTTY, IPRBUF(256), IERR,
     *   IRETCD, J, LUNTD, NBLOCK, ITEMP, PID(4), IPTR, JOFF
      REAL      PARBUF(128), EPS, TDL, DOWAIT, BATQUE, JOBNUM, BTFLIN,
     *   BTNLIN, DETIME, REMQUE
      LOGICAL   T, F, ACTIVE, LDOWT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DBCR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IPRBUF(1), PARBUF(1))
      EQUIVALENCE (BATQUE, KVALUE(27)),        (JOBNUM, KVALUE(28)),
     *            (BTFLIN, KVALUE(29)),        (BTNLIN, KVALUE(30)),
     *            (DETIME, KVALUE(26)),        (REMQUE, KVALUE(32))
      EQUIVALENCE (RHOST, KSTRNG(3))
      DATA PRGNAM /'CUA '/
      DATA LUNTD, LUNTTY /13,5/
      DATA CTASK /'AIPSC'/
      DATA T, F /.TRUE.,.FALSE./
      DATA EPS /0.01/
      DATA DOWAIT /1.0/
C-----------------------------------------------------------------------
      IF (BRANCH.NE.1) GO TO 999
      NBLOCK = 4
      GO TO (100), BRANCH
C-----------------------------------------------------------------------
C                                       SUBMIT
C                                       start AIPSC
C-----------------------------------------------------------------------
 100  J = BATQUE + EPS
      IF ((J.LT.1) .OR. (J.GT.NBATQS)) THEN
         POTERR = 40
         WRITE (MSGTXT,1100) J, NBATQS
         CALL MSGWRT (7)
         GO TO 980
         END IF
C                                       open output file
      CALL ZPHFIL ('TD', 1, 0, 4, PHNAME, IERR)
      CALL ZOPEN (LUNTD, FINDTD, 1, PHNAME, F, T, T, IERR)
      POTERR = 101
      IF (IERR.NE.0) GO TO 980
      CALL ZFIO ('READ', LUNTD, FINDTD, 1, IPRBUF, IERR)
      POTERR = 50
      IF (IERR.NE.0) GO TO 960
      NBLOCK = MAX (4, IPRBUF(256))
C                                      initial values
      JOFF = (NPOPS-1)*NBLOCK + 2
      CALL FILL (256, 0, IPRBUF)
      IPTR = 11
      IPRBUF(1) = NLUSER
      IPRBUF(2) = 0
      IPRBUF(3) = 0
      IPRBUF(4) = MSGKIL
      IPRBUF(5) = ISBTCH
      IPRBUF(6) = 0
      IPRBUF(7) = 0
      IPRBUF(8) = 0
      PARBUF(IPTR-2) = DOWAIT
      VERSON = VERNAM(1:3)
      CALL CHR2H (4, VERSON, 1, PARBUF(IPTR-1))
      PARBUF(IPTR) = BATQUE
      PARBUF(IPTR+1) = DETIME
      CALL CHR2H (48, VERNAM, 1, PARBUF(IPTR+2))
      CALL CHR2H (24, RHOST, 1, PARBUF(IPTR+14))
      PARBUF(IPTR+20) = REMQUE
      CALL ZFIO ('WRIT', LUNTD, FINDTD, JOFF, IPRBUF, IERR)
      POTERR = 50
      IF (IERR.NE.0) GO TO 960
C                                        pointer block (#1)
      CALL ZFIO ('READ', LUNTD, FINDTD, 1, IPRBUF, IERR)
      POTERR = 50
      IF (IERR.NE.0) GO TO 960
      ITEMP = 5*(NPOPS-1)
      CALL CHR2H (8, CTASK, 1, PARBUF(ITEMP+1))
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
      CALL LSERCH ('SRCH', LUNTTY, FINDTT, F, IERR)
      POTERR = 58
      IF (IERR.NE.0) GO TO 980
      CALL ZCLOSE (LUNTTY, FINDTT, IERR)
      IF (IERR.NE.0) GO TO 980
C                                        activate the task
 155  CALL ZACTV8 (CTASK, NPOPS, VERSON, PID, IERR)
      POTERR = 101
      IF (IERR.NE.0) GO TO 950
C                                       Wait for resumption through
C                                       TD file or abort of task
      TDL = 3.
      IF (DOWAIT.GT.0.0) TDL = 8.
      LDOWT = TDL.GT.4.0
      IF (AIPSMK.GT.0.0) TDL = TDL / MAX (0.5, AIPSMK)
      CALL TASKWT (ANAME, NPOPS, PID, LDOWT, TDL, IPRBUF, IRETCD, IERR)
C                                        reopen terminal
 180  CALL ZOPEN (LUNTTY, FINDTT, 1, PHNAME, F, T, T, IERR)
C                                       resumption message
      WRITE (MSGTXT,1180)
      CALL MSGWRT (2)
      IF (IRETCD.EQ.0) GO TO 999
         POTERR = 101
C                                       abort batch
         WRITE (MSGTXT,1185) IRETCD
         IF (IRETCD.NE.-999) CALL MSGWRT (9)
         GO TO 980
C                                       errors: close files
 950  CALL ZOPEN (LUNTTY, FINDTT, 1, PHNAME, F, T, T, IERR)
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
      SUBROUTINE CUB (BRANCH)
C-----------------------------------------------------------------------
C   CUB performs batch preparation operations
C   This version of AUB has small symbol table.
C   WARNING: this now differs substantially from AUB in immediate arg
C   pickup for BATEDIT.
C   BRANCH = 1  BATCH     read input to build batch work file
C            2  BATEDIT   read input to insert in batch work file
C            3  UNQUE     deque job, move back to work file
C            4  BATCLEAR  initialize batch work file
C            5  BATLIST   list part of batch work file
C            6  QUEUE     list contents of job queue
C            7  JOBLIST   list part of submitted (inactive) job
C            8  BAMODIFY  Change text of one line of batch work file
C   Adverbs used: BATQUE    queue number (BRANCH = 1 - 6, 8)
C                 JOBNUM    job number   (         3, 7)
C                 BATFLINE  first line # (         2, 5, 7, 8)
C                 BATNLINE  # of lines   (         5, 7)
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER PRGNAM*6, ATIME*8, ADATE*12, PHNAME*48, STATUS*8, LPT*1,
     *   TEMP*100, QUEUED*8, RUNING*8, SUBMIT*8, DONE*8, FAILED*8,
     *   BVERS(5)*4, HOLD*1, INSRT*1, DELETE*1, SPACE*1, ICHAR*1,
     *   RHOST*24, FILE*64, LHOST*24
      INTEGER   POTERR, IQ, IERR, J, JB, IW, IL, IPX, IM, LINE, J1, J2,
     *   IT0(6), IER2, IT(6), LINES, NLPR, I, NWPL, MSGLUN, INP, KOP,
     *   LBYTES, MSGIND, LIUNIT, ITEMP, NCHCPY, ITRIM, JQ, IREC, LREC,
     *   LBATQS, JTRIM
      REAL      EPS, RLINE, BATQUE, JOBNUM, BTFLIN, BTNLIN
      DOUBLE PRECISION    JD0, JD1
      LOGICAL   T, F, INSERT, INSIGN, BATOPN, BWTOPN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DBCR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DBAT.INC'
      INTEGER   BWTBUF(256)
      HOLLERITH BWTBUH(256)
      EQUIVALENCE (BWTBUF, BWTBUH)
      INCLUDE 'INCS:DBWT.INC'
      EQUIVALENCE (BATQUE, KVALUE(27)),        (JOBNUM, KVALUE(28)),
     *            (BTFLIN, KVALUE(29)),        (BTNLIN, KVALUE(30))
      EQUIVALENCE (RHOST, KSTRNG(3))
      DATA PRGNAM /'CUB'/
      DATA T, F /.TRUE.,.FALSE./
      DATA QUEUED, RUNING, SUBMIT, DONE, FAILED /'WAITING ',
     *   'RUNNING ','CHECKING','FINISHED','FAILED  '/
      DATA BVERS /'OLD ','NEW ','TST ','CVX ', '??? '/
      DATA EPS /0.01/
      DATA MSGLUN, HOLD, INSRT, SPACE, DELETE /6, '?', '_', '@','$'/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.8)) GO TO 999
      BWTLUN = 26
      BATLUN = 4
      NWPL = 2 + (NCHLIN + 3) / 4
      NLPR = 252 / NWPL
      BWTOPN = .FALSE.
      BATOPN = .FALSE.
C                                        test input
      POTERR = 32
      IQ = BATQUE + EPS
      IF ((BRANCH.EQ.3) .OR. (BRANCH.EQ.6) .OR. (BRANCH.EQ.7)) THEN
         LHOST = RHOST
      ELSE
         LHOST = ' '
         END IF
      LBATQS = NBATQS
      IF (LHOST.NE.' ') LBATQS = 33
      IF ((BRANCH.NE.6) .AND. (BRANCH.NE.7)) THEN
         IF ((IQ.LT.1) .OR. (IQ.GT.NBATQS)) THEN
            WRITE (MSGTXT,1000) IQ, NBATQS
            CALL MSGWRT (7)
            GO TO 980
            END IF
         END IF
C
      J = JOBNUM + EPS
      JQ = J / 100
      JB = MOD (J, 100)
      IF (BRANCH.EQ.6) JQ = IQ
C                                        open Queue file
      IF ((BRANCH.EQ.3) .OR. (BRANCH.EQ.6) .OR. (BRANCH.EQ.7)) THEN
         CALL ZPHFIL ('BQ', 1, 0, 0, PHNAME, IERR)
         J = JTRIM (LHOST)
C                                       local
         IF (J.EQ.0) THEN
            FILE = PHNAME
C                                       remote at this location
         ELSE
            FILE = 'NET0:' // LHOST(:J) // '/' // PHNAME(6:)
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
C                                        get line #, opt. arg
      RLINE = BTFLIN
      IF (SP+1.LE.AP) THEN
         I = CSTACK(SP+1)
         IF ((I.EQ.1) .OR. (I.EQ.11)) THEN
            IF (I.EQ.11) RLINE = V(SP+1)
            IF (I.EQ.1) RLINE = KVALUE(STACK(SP+1))
            SP = SP + 1
            END IF
         END IF
C                                        check line number
      LINE = RLINE + EPS
      INSERT = (ABS(RLINE-LINE) .GT. EPS) .AND. (BRANCH.EQ.2)
      IF ((RLINE.LE.EPS) .OR. (BRANCH.NE.2)) THEN
         IF ((BRANCH.NE.8) .OR. (LINE.LT.1) .OR. (LINE.GT.BWTBUF(2)-2))
     *      THEN
            WRITE (MSGTXT,1210) RLINE
            CALL MSGWRT (7)
            POTERR = 32
            GO TO 960
            END IF
         END IF
C                                        insert after current text
      IF (LINE.GE.BWTBUF(2)-2) THEN
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
      J = ITRIM (LHOST)
      CALL ZPHFIL ('BA', 1, JB, ITEMP, PHNAME, IERR)
C                                       local
      IF (J.EQ.0) THEN
         FILE = PHNAME
C                                       remote at this location
      ELSE
         FILE = 'NET0:' // LHOST(:J) // '/' // PHNAME(6:)
         END IF
      CALL ZOPEN (BATLUN, BATIND, 1, PHNAME, F, T, T, IERR)
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
      CALL ZDESTR (1, PHNAME, IER2)
      IF ((IERR.NE.0) .OR. (IER2.NE.0)) THEN
         WRITE (MSGTXT,1320) IERR, IER2
         CALL MSGWRT (7)
         POTERR = 50
         GO TO 960
         END IF
C                                        remove from queue
      CALL ZPHFIL ('BQ', 1, 0, 0, PHNAME, IERR)
      J = JTRIM (LHOST)
C                                       local
      IF (J.EQ.0) THEN
         FILE = PHNAME
C                                       remote at this location
      ELSE
         FILE = 'NET0:' // LHOST(:J) // '/' // PHNAME(6:)
         END IF
      CALL ZOPEN (BATLUN, BATIND, 1, PHNAME, F, T, T, IERR)
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
      CALL FILL (5, 0, BATDAT(J))
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
 600  IF ((IQ.GE.1) .AND. (IQ.LE.NBATQS)) THEN
         J1 = IQ
         J2 = IQ
      ELSE
         J1 = 1
         J2 = NBATQS
         END IF
      CALL ZDATE (IT0(1))
      CALL ZTIME (IT0(4))
      CALL DAT2JD (IT0, JD0)
      WRITE (MSGTXT,1600)
      CALL MSGWRT (5)
C                                       loop over queues
      IW = 0
      DO 620 JQ = J1,J2
         IF (LHOST.NE.' ') MSGSUP = 32000
         CALL ZFIO ('READ', BATLUN, BATIND, JQ, BATDAT, IERR)
         MSGSUP = 0
         IF ((IERR.NE.0) .AND. (IW.GT.0)) GO TO 630
         IF (IERR.NE.0) GO TO 960
         IF (LHOST.NE.' ') IW = IW + 1
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
      J = JTRIM (LHOST)
C                                       local
      IF (J.EQ.0) THEN
         FILE = PHNAME
C                                       remote at this location
      ELSE
         FILE = 'NET0:' // LHOST(:J) // '/' // PHNAME(6:)
         END IF
      CALL ZOPEN (BWTLUN, BWTIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 750
      BWTOPN = .TRUE.
      CALL ZFIO ('READ', BWTLUN, BWTIND, 1, BWTBUF, IERR)
      IF (IERR.NE.0) GO TO 750
C                                        list work or file
 720  BWTREC = 1
      LINE = BTFLIN + EPS
      LINES = BTNLIN + EPS
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
            CALL H2CHR (NCHCPY, 1, BWTBUH(J+2), MSGTXT(7:))
            CALL MSGWRT (5)
            END IF
 740     CONTINUE
      POTERR = 0
C                                        clear busy setting
 750  IF (BRANCH.NE.7) THEN
         GO TO 960
      ELSE
         CALL ZPHFIL ('BQ', 1, 0, 0, PHNAME, IERR)
         J = JTRIM (LHOST)
C                                       local
         IF (J.EQ.0) THEN
            FILE = PHNAME
C                                       remote at this location
         ELSE
            FILE = 'NET0:' // LHOST(:J) // '/' // PHNAME(6:)
            END IF
         CALL ZOPEN (BATLUN, BATIND, 1, PHNAME, F, T, T, IERR)
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
      CALL H2CHR (LBYTES, 1, BWTBUH(J+2), TEMP(2:))
      TEMP(LBYTES+2:) = ' '
      INP = ITRIM (TEMP)
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
               CALL H2CHR (1, INP, BWTBUH(J+2), KARBUF(KOP:KOP))
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
               IF (INP.LE.LBYTES) CALL H2CHR (1, INP, BWTBUH(J+2),
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
      CALL CHR2H (NCHLIN, KARBUF, 1, BWTBUH(J+2))
      INP = ITRIM (TEMP)
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
