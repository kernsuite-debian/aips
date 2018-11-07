      PROGRAM GRIPR
C-----------------------------------------------------------------------
C! GRIPR is stand-alone task for perparing and submitting Gripes.
C# Gripes Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2002-2003, 2013
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
C   GRIPR is a stand-alone AIPS-like program used to prepare and
C   submit text to AIPS Gripe files.
C   It uses NPOPS = NINTRN  +  1.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IRET
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DBAT.INC'
      INCLUDE 'INCS:DBWT.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DBCR.INC'
      DATA PRGNAM /'GRIPR '/
C-----------------------------------------------------------------------
      TSKNAM = PRGNAM
      CALL ZDCHIN (.TRUE.)
      NPOPS = NINTRN + 1
      WASERR = .TRUE.
C                                        init commons
 10   CALL GRIINI (IRET)
      IF (IRET.NE.0) GO TO 999
C                                        process input until error
 20   CALL GRIGTL
      IF (ERRNUM.GT.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         CALL OERROR
         IUNIT = 1
         END IF
C                                        loop all errors except EXIT
 30   IF (ERRNUM.NE.-2) GO TO 20
      WRITE (MSGTXT,1030)
      CALL MSGWRT (1)
      CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1030 FORMAT ('Wishes you a good day')
      END
      SUBROUTINE GRIGTL
C-----------------------------------------------------------------------
C   GRIGTL is the main routine of GRIPR.  It reads input via PREAD,
C   parses it via GRIPOL, and executes it via GRIINT.  Returns only
C   on error.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DIO.INC'
      DATA PRGNAM /'GRIGTL'/
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
         CALL GRIPOL
         IF (ERRNUM.NE.0) GO TO 980
            CALL GRIINT
            IF (ERRNUM.EQ.0) GO TO 10
C                                        error
 980  ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
      SUBROUTINE GRIINI (IRET)
C-----------------------------------------------------------------------
C   GRIINI initializes commons for GRIPR
C-----------------------------------------------------------------------
      CHARACTER PHNAME*48, SYMBLS(30)*8
      INTEGER   LUNCRT, IF, IERR, LCOUNT(30), LTYPE(30), I, IRET
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DBCR.INC'
      INCLUDE 'INCS:DBAT.INC'
      INCLUDE 'INCS:DRUN.INC'
      DATA LUNCRT /5/
      DATA T, F /.TRUE.,.FALSE./
      DATA SYMBLS / 'GRIPE   ', 'GRINDEX ', 'GRLIST  ', 'INPUTS  ',
     *              'CLRMSG  ', 'PRTMSG  ', 'EXIT    ', 'HELP    ',
     *              'GRDROP  ', '        ', '        ', '        ',
     *              '        ', '        ', '        ', '        ',
     *              '        ', '        ', '        ', 'GREMAIL ',
     *              'GRNAME  ', 'GRADDRES', 'GRPHONE ', 'JOBNUM  ',
     *              'PRIORITY', 'DOCRT   ', 'PRNUMBER', 'PRTIME  ',
     *              'PRTASK  ', 'OUTPRINT'/
      DATA LCOUNT / 5, 7, 6, 6, 6, 6, 4, 4, 6, 0,
     *              0, 0, 0, 0, 0, 0, 0, 0, 0, 7,
     *              6, 8, 7, 6, 8, 5, 8, 6, 6, 8/
      DATA LTYPE  / 4, 4, 4, 4, 4, 4, 4, 4, 4, 0,
     *              0, 0, 0, 0, 0, 0, 0, 0, 0, 7,
     *              7, 7, 7, 1, 1, 1, 1, 1, 7, 7/
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
      NKSYM = 30
      DO 10 I = 1, NKSYM
         KSYM(I) = SYMBLS(I)
 10      CONTINUE
      CALL COPY (NKSYM, LCOUNT, KCOUNT)
      CALL COPY (NKSYM, LTYPE, KTYP)
      CALL RFILL (NKSYM, 0.0, KVALUE)
      KVALUE(26) = -1.0
C                                        strings: 100*addr + nchar
      KVALUE(20) = 648.
      KVALUE(21) = 120.
      KVALUE(22) = 248.
      KVALUE(23) = 316.
      KVALUE(29) = 405.
      KVALUE(30) = 548.
      NKSTR = 30
      DO 20 I = 1, NKSTR
         KSTRNG(I) = ' '
 20      CONTINUE
      KSTRNG(4) = TSKNAM
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
 1012 FORMAT ('related to Gripes.  I can take more than one command in')
 1013 FORMAT ('a line and do use min match.  Issue the command INPUTS')
 1014 FORMAT ('for a list of my full vocabulary and current adverb',
     *   ' values.')
 1015 FORMAT ('Have fun!')
      END
      SUBROUTINE GRIINT
C-----------------------------------------------------------------------
C   GRIINT interprets the stacks prepared by GRIPOL.  It executes
C   =, EXIT, INPUTS, and HELP and calls PRTMSG, CUA, and CUB to do the
C   other verbs.
C-----------------------------------------------------------------------
      CHARACTER OP*4, IPRTSK*8, PHNAME*48, PRGNAM*6, PRTASK*6,
     *   LVERS*48, LPNAME*48
      INTEGER   POTERR, J, NEXIT, NGRIPE, NPRINT, NGRLST, I, NHELP,
     *   J1, J2, IUSER, NDONE, NLEFT, IERR, NCLRM, NINPU, HLPLUN,
     *   FIND, IPOPS, IROUND, NGRDRP
      REAL      DOCRT, PRIOTY, DTIME, PRNUMB, PRTIME, LSIGN
      LOGICAL   EQUAL, F
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DBCR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (DOCRT,  KVALUE(26)),   (PRIOTY, KVALUE(25)),
     *            (PRNUMB, KVALUE(27)),   (PRTIME, KVALUE(28))
      EQUIVALENCE (PRTASK, KSTRNG(4)),    (LPNAME, KSTRNG(5))
      DATA NGRIPE, NGRLST, NINPU, NHELP, NEXIT, NPRINT, NCLRM, NGRDRP
     *   /1,3,4,8,7,6,5,9/
      DATA PRGNAM /'GRIINT'/
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
C                                        adverbs
 20   IF (SP+1.GT.AP) GO TO 980
C                                        next must be =
C                                        real adverb/const after that
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
C                                        work file ops
 40   IF (((J.LT.NGRIPE) .OR. (J.GT.NGRLST)) .AND. (J.NE.NGRDRP))
     *   GO TO 45
         I = J - NGRIPE + 1
         CALL CUC (I)
         IF (ERRNUM.NE.0) GO TO 980
C                                        inputs
 45   IF (J.NE.NINPU) GO TO 55
         WRITE (MSGTXT,1045)
         CALL MSGWRT (1)
         DO 50 I = 1,NKSYM
            IF (KTYP(I).EQ.1) THEN
               WRITE (MSGTXT,1046) KSYM(I), KVALUE(I)
            ELSE IF (KTYP(I).EQ.4) THEN
               WRITE (MSGTXT,1047) KSYM(I)
            ELSE IF (KTYP(I).EQ.7) THEN
               WRITE (MSGTXT,1048) KSYM(I)
               J1 = MOD (KVALUE(I), 100.0) + 0.01
               J2 = (KVALUE(I) - J1) / 100.0 + 0.01
               J = 19
               MSGTXT(J:J+J1-1) = KSTRNG(J2)(1:J1)
               J = J + J1
               MSGTXT(J:J) = ''''
            ELSE
               GO TO 50
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
         IF (J.EQ.NEXIT) DTIME = 3.
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
         IF (NDONE.GT.0) THEN
            IF (J.EQ.NPRINT) WRITE (MSGTXT,1080) NDONE
            IF (J.NE.NPRINT) WRITE (MSGTXT,1081) NDONE
            CALL MSGWRT (1)
            END IF
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
      SUBROUTINE GRIPOL
C-----------------------------------------------------------------------
C   GRIPOL is a simplified parser/compiler used by GRIPR.  It
C   converts text in KARBUF to type, and tag stacks in CSTACK and
C   STACK, resp. with AP pointing at the last entry.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, M*1, KPAC*48
      INTEGER   POTERR, J, NIPL, J0, ICL, IPL(20), I, NHELP, NINPU
      LOGICAL   FLAG
      DOUBLE PRECISION    DBLX
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DBCR.INC'
      INCLUDE 'INCS:DPOP.INC'
      DATA PRGNAM, NHELP, NINPU /'GRIPOL', 8,4/
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
      IF (M.EQ.'.') GO TO 50
      IF (M.EQ.'''') GO TO 80
      IF (M.EQ.'+') GO TO 90
      IF (M.EQ.'-') GO TO 90
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
         J0 = 2 * IPL(I) - 1
         WRITE (MSGTXT,1900) KSYM(J0)
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
      SUBROUTINE CUC (BRANCH)
C-----------------------------------------------------------------------
C   CUC performs operations related to the Gripe file.  Special version
C   for use by GRIPR, the stand-alone Gripe manipulator.
C   Branch = 1 : GRIPE    enter a new Gripe.
C            2 : GRINDEX  index the Gripes now in the file.
C            3 : GRLIST   list an individual gripe.
C            9 : GRDROP   drop the most recent gripe
C   The Gripe file is named GR100000. and starts with one record of
C   index information as:
C   Word   1   : Number of 512-byte records now in file.
C          2   : Number of Gripes now in file.
C          3   : Record number for next Gripe.
C          4   : Character position in that record for next Gripe.
C        5 - 7 : Reserved.
C          8   : Gripe #1 logon user number.
C          9   : Gripe #1 start record number.
C         10   : Gripe #1 start character position.
C         ...  : Gripes #2 - 83.
C   The individual entries are packed character strings having the
C   form: ' {date   time}  {system ID string}  {user ID #}
C   {...name...}  {...address...}  {...phone...}
C   {...gripe contents...}  { } { } { } '.
C-----------------------------------------------------------------------
      CHARACTER ATIME*8, ADATE*12, PHNAME*48, GRNAME*20, GRADDR*48,
     *   GRPHON*16, PRGNAM*6, SCNAME*256, EMAIL*48
      HOLLERITH CBUF(256), WBUF(256), HBUF(256)
      INTEGER   BRANCH
      INTEGER   LUN(2), IND(2), IERR, IBUF(256), IRNO, IP, IT(6), LP,
     *   NUMBER, POTERR, I, LLP, LPOS, KPOS, LLPOS, NCHPR, J0, J1, J2,
     *   OPOS, IPOS, NCH, ILP, LREC, KREC, LLREC, ITRIM, LENSTR
      REAL      JOBNUM
      LOGICAL   T, F
      INCLUDE 'INCS:DBCR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (HBUF, IBUF)
      EQUIVALENCE (JOBNUM, KVALUE(24))
      EQUIVALENCE (GRNAME, KSTRNG(1))
      EQUIVALENCE (GRADDR, KSTRNG(2))
      EQUIVALENCE (GRPHON, KSTRNG(3))
      EQUIVALENCE (EMAIL, KSTRNG(6))
      DATA T, F /.TRUE.,.FALSE./
      DATA PRGNAM /'CUC   '/
C-----------------------------------------------------------------------
      LUN(1) = 13
      LUN(2) = 0
      SCNAME = ' '
C                                       Check status: interactive only
      IF (((BRANCH.LT.1) .OR. (BRANCH.GT.3)) .AND. (BRANCH.NE.9))
     *   GO TO 999
      POTERR = 53
      IF ((IUNIT.NE.1) .AND. (BRANCH.EQ.1)) GO TO 980
C                                       Open disk and read
      POTERR = 50
      CALL ZPHFIL ('GR', 1, 0, 0, PHNAME, IERR)
      CALL ZOPEN (LUN, IND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL ZFIO ('READ', LUN, IND, 1, HBUF, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Branch to operation
      IF (BRANCH.EQ.9) GO TO 500
      GO TO (100, 200, 300), BRANCH
C-----------------------------------------------------------------------
C                                       GRIPE
C                                       Enter a new gripe.
C-----------------------------------------------------------------------
 100  IF (IBUF(2).GE.83) THEN
         WRITE (MSGTXT,1100)
         GO TO 960
         END IF
C                                       Create temporary mail file
      CALL ZFULLN (' ', 'FITS', 'gripemail', SCNAME, IERR)
      POTERR = 55
      IF (IERR.NE.0) GO TO 970
      LUN(2) = 3
      CALL ZTXOPN ('QWRT', LUN(2), IND(2), SCNAME, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         LUN(2) = 0
         SCNAME = ' '
         END IF
C                                       Set up new directory entry
      IP = 8 + 3 * IBUF(2)
      IBUF(IP) = NLUSER
      IBUF(IP+1) = IBUF(3)
      IBUF(IP+2) = IBUF(4)
      IPT = '!'
C                                       Read first Gripe record
      IRNO = IBUF(3)
      CALL ZFIO ('READ', LUN, IND, IRNO, CBUF, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Date and time
      CALL ZDATE (IT(1))
      CALL ZTIME (IT(4))
      CALL TIMDAT (IT(4), IT(1), ATIME, ADATE)
      JBUFF = ADATE // ATIME
      CALL CHGRIP (0, LUN, IND, IBUF, CBUF, JBUFF(1:20), IERR)
      IF (IERR.NE.0) GO TO 190
C                                       System ID
      JBUFF = HSTNAM // ' ' // SYSNAM
      CALL CHGRIP (0, LUN, IND, IBUF, CBUF, JBUFF(1:33), IERR)
      IF (IERR.NE.0) GO TO 190
C                                       User # ID, release
      WRITE (JBUFF,1105) NLUSER, RLSNAM, SYSTYP
      CALL CHGRIP (0, LUN, IND, IBUF, CBUF, JBUFF(1:20), IERR)
      IF (IERR.NE.0) GO TO 190
C                                       General instructions
 110  MSGTXT = 'Please limit each GRIPE to one bug report and/or ' //
     *   'suggestion.'
      CALL MSGWRT (1)
      MSGTXT = 'Please give details and use lower case where ' //
     *   'appropriate.'
      CALL MSGWRT (1)
C                                       Name
      IF (GRNAME(:4).NE.' ') THEN
         JBUFF = GRNAME
         MSGTXT = 'Hello again ' // JBUFF(1:20)
         CALL MSGWRT (1)
      ELSE
         MSGTXT ='What is your name?'
         CALL MSGWRT (1)
         CALL PREAD (KARBUF)
         IF (ERRNUM.NE.0) GO TO 970
         GRNAME = JBUFF
         END IF
      CALL CHGRIP (0, LUN, IND, IBUF, CBUF, JBUFF(1:20), IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Address
      IF (GRADDR(:4).NE.' ') THEN
         JBUFF = GRADDR
      ELSE
         MSGTXT = 'In one line, what is your address?'
         CALL MSGWRT (1)
         CALL PREAD (KARBUF)
         IF (ERRNUM.NE.0) GO TO 970
         GRADDR = JBUFF(1:48)
         END IF
      CALL CHGRIP (0, LUN, IND, IBUF, CBUF, JBUFF(1:48), IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Phone
      IF (GRPHON(:4).NE.' ') THEN
         JBUFF = GRPHON
      ELSE
         MSGTXT = 'What is your phone number?'
         CALL MSGWRT (1)
         CALL PREAD (KARBUF)
         IF (ERRNUM.NE.0) GO TO 970
         GRPHON = JBUFF(1:16)
         END IF
      CALL CHGRIP (0, LUN, IND, IBUF, CBUF, JBUFF(1:16), IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Get E-mail address
      JBUFF = EMAIL
      IF (JBUFF.EQ.' ') THEN
         MSGTXT = 'At what E-Mail address can you be reached?'
         CALL MSGWRT (1)
         CALL PREAD (KARBUF)
         IF (ERRNUM.NE.0) GO TO 970
         END IF
      EMAIL = JBUFF
C                                       Show system, version
 140  MSGTXT = 'Gripe from version ' // RLSNAM // ' System name: '//
     *   HSTNAM
      CALL MSGWRT (1)
C                                       Gripe itself
 150  MSGTXT = 'Enter your comment 1 line at a time.  Type _END ' //
     *   'or _end to stop.'
      CALL MSGWRT (1)
      MSGTXT = 'Type _FORGET or _forget to exit without recording the'
     *   // ' gripe'
      CALL MSGWRT (1)
      DO 160 I = 1,64
         CALL PREAD (KARBUF)
         IF (ERRNUM.NE.0) GO TO 970
         IP = 2
         IF (I.EQ.1) IP = 1
         CALL CHGRIP (IP, LUN, IND, IBUF, CBUF, JBUFF(1:NBYTES), IERR)
         IF (IERR.EQ.99) GO TO 170
         IF (IERR.NE.0) GO TO 190
 160     CONTINUE
      MSGTXT = 'Enough already!!!'
      CALL MSGWRT (1)
      JBUFF = '_END'
      CALL CHGRIP (IP, LUN, IND, IBUF, CBUF, JBUFF(1:4), IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.99)) GO TO 970
C                                       Get one line summary
 170  MSGTXT = 'Give a one line summary of your problem/suggestion:'
      CALL MSGWRT (1)
      CALL PREAD (KARBUF)
      IF (ERRNUM.NE.0) GO TO 970
C                                       Write one-liner, email final { }
      IP = 0
      LENSTR = ITRIM (JBUFF(:80))
      CALL CHGRIP (IP, LUN, IND, HBUF, CBUF, JBUFF(:LENSTR), IERR)
      IF (IERR.NE.0) GO TO 970
      LENSTR = ITRIM (EMAIL)
      CALL CHGRIP (IP, LUN, IND, HBUF, CBUF, EMAIL(1:LENSTR), IERR)
      IF (IERR.NE.0) GO TO 970
      CALL CHGRIP (IP, LUN, IND, HBUF, CBUF, '  ', IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Done : write data, dir.
      IRNO = IBUF(3)
      CALL ZFIO ('WRIT', LUN, IND, IRNO, CBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      IBUF(2) = IBUF(2) + 1
      CALL ZFIO ('WRIT', LUN, IND, 1, HBUF, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       send e-mail
      IF (LUN(2).GT.0) THEN
         CALL ZTXCLS (LUN(2), IND(2), IERR)
         CALL ZGMAIL (SCNAME, IERR)
         IF (IERR.NE.0) CALL ZTXZAP (3, SCNAME, IERR)
         SCNAME = ' '
         END IF
      GO TO 900
C                                       \forget occurred
 190  IF (IERR.GT.0) GO TO 970
         MSGTXT = 'Partial gripe has been ''FORGOTTEN'''
         CALL MSGWRT (3)
         IF (LUN(2).GT.0) CALL ZTXCLS (LUN(2), IND(2), IERR)
         IF (SCNAME.NE.' ') CALL ZTXZAP (3, SCNAME, IERR)
         GO TO 900
C-----------------------------------------------------------------------
C                                       GRINDEX
C                                       Index contents: #, date, names
C-----------------------------------------------------------------------
 200  WRITE (MSGTXT,1200)
      CALL MSGWRT (2)
      LP = 5
      NUMBER = IBUF(2)
      IBUF(3) = 0
      IF (NUMBER.LE.0) GO TO 900
      DO 220 I = 1,NUMBER
         LP = LP + 3
C                                       Read rec 1 of gripe
         IF (IBUF(LP+1).EQ.IBUF(3)) GO TO 210
            IBUF(3) = IBUF(LP+1)
            IRNO = IBUF(3)
            CALL ZFIO ('READ', LUN, IND, IRNO, CBUF, IERR)
            IF (IERR.NE.0) GO TO 970
 210     IBUF(4) = IBUF(LP+2)
C                                       Seq and user #s
         WRITE (MSGTXT,1210) I, IBUF(LP)
C                                       Time
         CALL CHGRIP (3, LUN, IND, IBUF, CBUF, JBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         MSGTXT(12:31) = JBUFF(1:20)
C                                       Skip System ID
         CALL CHGRIP (3, LUN, IND, IBUF, CBUF, JBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Skip user #ID, get version
         CALL CHGRIP (3, LUN, IND, IBUF, CBUF, JBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         J0 = ITRIM (JBUFF)
         MSGTXT(34:40) = JBUFF(J0-6:J0)
C                                       User name
         CALL CHGRIP (3, LUN, IND, IBUF, CBUF, JBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         MSGTXT(43:) = JBUFF
         CALL MSGWRT (2)
 220     CONTINUE
      GO TO 900
C-----------------------------------------------------------------------
C                                       GRLIST
C                                       List a gripe
C-----------------------------------------------------------------------
 300  LP = JOBNUM + 0.5
      IF ((LP.LT.1) .OR. (LP.GT.IBUF(2))) LP = IBUF(2)
      IF (LP.LE.0) GO TO 900
      I = 5 + 3 * LP
      IBUF(3) = IBUF(I+1)
      IBUF(4) = IBUF(I+2)
      IRNO = IBUF(3)
      CALL ZFIO ('READ', LUN, IND, IRNO, CBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      WRITE (MSGTXT,1300) LP, IBUF(I)
      CALL MSGWRT (3)
      DO 310 I = 1,72
         CALL CHGRIP  (3, LUN, IND, IBUF, CBUF, JBUFF, IERR)
         IF ((IERR.GT.0) .AND. (IERR.NE.100)) GO TO 970
         MSGTXT = JBUFF
         IF (I.NE.2) CALL MSGWRT (3)
         IF ((I.GT.6) .AND. (IERR.NE.100)) GO TO 900
 310     CONTINUE
      GO TO 900
C-----------------------------------------------------------------------
C                                       GRDROP
C                                       Delete a gripe
C-----------------------------------------------------------------------
 500  LP = JOBNUM + 0.5
      IF ((LP.LT.1) .OR. (LP.GT.IBUF(2))) LP = IBUF(2)
      IF (LP.LE.0) GO TO 900
      IP = 5 + 3 * LP
      IF (IBUF(IP).EQ.NLUSER) GO TO 510
         WRITE (MSGTXT,1500) LP
         POTERR = 101
         GO TO 960
C                                       gripe is last one
 510  IF (LP.LT.IBUF(2)) GO TO 520
         IBUF(3) = IBUF(IP+1)
         IBUF(4) = IBUF(IP+2)
         GO TO 530
C                                       Gripe is intermediate one
 520  CONTINUE
         LREC = IBUF(IP+1)
         KREC = IBUF(IP+4)
         LPOS = IBUF(IP+2)
         KPOS = IBUF(IP+5)
         LLREC = IBUF(3)
         LLPOS = IBUF(4)
         NCHPR = 1024
         CALL ZFIO ('READ', LUN, IND, LREC, WBUF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL ZFIO ('READ', LUN, IND, KREC, CBUF, IERR)
         IF (IERR.NE.0) GO TO 970
         OPOS = LPOS
         IPOS = KPOS
 530     J1 = NCHPR + 1 - OPOS
            J2 = NCHPR + 1 - IPOS
            IF (KREC.EQ.LLREC) J2 = LLPOS - IPOS
            J0 = MIN (J1, J2)
            CALL CHCOPY (J0, IPOS, CBUF, OPOS, WBUF)
            OPOS = OPOS + J0
            IPOS = IPOS + J0
C                                       output record full
            IF (OPOS.LE.NCHPR) GO TO 540
               CALL ZFIO ('WRIT', LUN, IND, LREC, WBUF, IERR)
               IF (IERR.NE.0) GO TO 970
               LREC = LREC + 1
               OPOS = 1
C                                       input record exhausted
 540        IF ((KREC.EQ.LLREC) .AND. (IPOS.GE.LLPOS)) GO TO 550
            IF (IPOS.LE.NCHPR) GO TO 530
               KREC = KREC + 1
               IF (KREC.GT.LLREC) GO TO 550
               CALL ZFIO ('READ', LUN, IND, KREC, CBUF, IERR)
               IF (IERR.NE.0) GO TO 970
               IPOS = 1
               GO TO 530
 550     IF (OPOS.LE.1) GO TO 560
            LLP = NCHPR + 1 - OPOS
            CALL CHFILL (LLP, HBLANK, OPOS, WBUF)
            CALL ZFIO ('WRIT', LUN, IND, LREC, WBUF, IERR)
            IF (IERR.NE.0) GO TO 970
C                                       fix directory
 560     LLP = IBUF(2) - 1
         LP = LP + 1
         IP = IP + 3
         IF (LP.GT.LLP) GO TO 570
            DO 565 ILP = LP,LLP
               IBUF(IP-3) = IBUF(IP)
               NCH = IBUF(IP+5) - IBUF(IP+2) + IBUF(IP-1) - 1
     *            + NCHPR * (IBUF(IP+4) - IBUF(IP+1))
               IBUF(IP+1) = IBUF(IP-2) + NCH / NCHPR
               IBUF(IP+2) = MOD (NCH, NCHPR) + 1
               IP = IP + 3
 565           CONTINUE
 570     NCH = IBUF(4) - IBUF(IP+2) + IBUF(IP-1) - 1
     *      + NCHPR * (IBUF(3) - IBUF(IP+1))
         IBUF(3) = IBUF(IP-2) + NCH / NCHPR
         IBUF(4) = MOD (NCH, NCHPR) + 1
         IBUF(IP-3) = IBUF(IP)
 580  IBUF(2) = IBUF(2) - 1
      CALL ZFIO ('WRIT', LUN, IND, 1, HBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      GO TO 900
C-----------------------------------------------------------------------
C                                       Close downs
 900  CALL ZCLOSE (LUN, IND, IERR)
      GO TO 990
C                                       Errors
 960  CALL MSGWRT (8)
 970  CALL ZCLOSE (LUN, IND, IERR)
      IF (SCNAME.NE.' ') CALL ZTXZAP (3, SCNAME, IERR)
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5)  PNAME(ERRLEV) = PRGNAM
C
 990  IPT = '>'
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('GRIPE FILE FULL - CALL AIPS MANAGER')
 1105 FORMAT (I5,2X,A7,2X,A4)
 1200 FORMAT ('Seq User',6X,'Date',7X,'Time',4X,'Version',2X,'Name')
 1210 FORMAT (I3,I5)
 1300 FORMAT ('Gripe number',I3,'  from user number',I5)
 1500 FORMAT ('GRIPE',I3,' DOES NOT BELONG TO YOU')
      END
