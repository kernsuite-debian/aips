LOCAL INCLUDE 'TABED.INC'
C                                       Local include for TABED
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO, IVER,
     *   OVER, NUMTAB, CATOLD(256), CATBLK(256), BC, EC
      LOGICAL   SAME, SAMOUT
      HOLLERITH XNAMEI(3), XCLAIN(2), XXEXT, XNAMOU(3), XCLAOU(2), XXOP,
     *   XXKEY(2), XXKEYS(4)
      CHARACTER NAMEIN*12, CLAIN*6, XEXT*4, NAMOUT*12, CLAOUT*6, XOP*4,
     *   XKEY*8, XKEYS*16, TABTYP*2
      REAL      XSEQIN, XDISKI, XIVER, XSEQO, XDISKO, XOVER, XBC, XEC,
     *   APARM(10), XKEYV(2), XTIME(8)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XXEXT, XIVER,
     *   XNAMOU, XCLAOU, XSEQO, XDISKO, XOVER, XBC, XEC, XXOP, APARM,
     *   XXKEY, XKEYV, XXKEYS, XTIME
      COMMON /EDTINF/ CATOLD, SAME, SAMOUT, SEQIN, SEQOUT, DISKIN,
     *   DISKO, NEWCNO, OLDCNO, IVER, OVER, NUMTAB, BC, EC
      COMMON /CHRCOM/ NAMEIN, CLAIN, XEXT, NAMOUT, CLAOUT, XOP, XKEY,
     *   XKEYS, TABTYP
      COMMON /MAPHDR/ CATBLK
LOCAL END
      PROGRAM TABED
C-----------------------------------------------------------------------
C! Edits AIPS tables files
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2005-2008, 2014, 2016
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
C   TABED edits AIPS extention tables files.
C   Adverbs:
C    INNAME      Input image name (name)
C    INCLASS     Input image name (class)
C    INSEQ       Input image name (seq. #)
C    INDISK      Input image disk unit #
C    INEXT       Input table extension type
C    INVERS      Input table file version no., -1=>all
C    OUTNAME     Output image name (name) NB: Def. output = input
C    OUTCLASS    Output image name (class)
C    OUTSEQ      Output image name (seq. #)
C    OUTDISK     Output image disk unit #.
C    OUTVERS     Output table file version., -1=>same as input
C    BCOUNT      Beginning row to copy
C    ECOUNT      Last row to copy: 0 => end
C    OPTYPE      'COPY','ADD ','MULT','DIV ',
C                'REPL','CLIP','DELE','INSR'
C                'KEY '
C    APARM       Column selection parameters:
C                1: Col. number.
C                2: Low subscript
C                3: High subscript
C                4: Keyword data type (1-6)
C                5: test col.
C                6: test value
C                7: test tolerance, 8: test subscript, 9: no. char.
C    KEYWORD     Name of table keyword.
C    KEYVALUE    Value given to numeric parm.
C    KEYSTRNG    Value given to a character-valued parameter.
C    TIMERANG    Time range to use.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, BUFFER(256), LOOP, LIM1, LIM2, NUMSEL, NFLAG
      LOGICAL   NOTIME, ISCHAR
      INCLUDE 'TABED.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA PRGM /'TABED '/
C-----------------------------------------------------------------------
C                                       Initialize
      CALL TEDIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Edit tables
      LIM1 = IVER
      LIM2 = IVER + NUMTAB - 1
      DO 100 LOOP = LIM1,LIM2
         IVER = LOOP
         IF (SAMOUT) OVER = LOOP
         CALL TEDITR (NOTIME, ISCHAR, NUMSEL, NFLAG, IRET)
         IF (IRET.NE.0) GO TO 995
 100     CONTINUE
C                                       Add history to output
      CALL TEDHIS (NOTIME, ISCHAR, NUMSEL, NFLAG)
C                                       Close down files, etc
 995  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE TEDIN (PRGM, IRET)
C-----------------------------------------------------------------------
C   Initialization subroutine for TABED
C   Inputs:  PRGM    C*6       Program name
C   Output:  IRET    I         Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
C
      CHARACTER TYPE*2, STAT*4
      HOLLERITH CATOH(256)
      INTEGER   BUFF1(256), NPARM, IROUND, IERR
      INCLUDE 'TABED.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATOLD, CATOH)
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      IRET = 0
      BC = 10000000
      EC = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 46
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
      IVER = IROUND (XIVER)
      OVER = IROUND (XOVER)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXEXT, XEXT)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XXOP, XOP)
      CALL H2CHR (8, 1, XXKEY, XKEY)
      CALL H2CHR (16, 1, XXKEYS, XKEYS)
      TABTYP = XEXT(1:2)
C                                       APARM defaults
      IF (APARM(8).LT.0.5) APARM(8) = 1
      IF (APARM(9).LT.0.5) APARM(9) = 8
C                                       Find input
      OLDCNO = 1
      TYPE = '  '
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, TYPE,
     *   NLUSER, STAT, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Default output = input
      IF (NAMOUT.EQ.' ') NAMOUT = NAMEIN
      IF (CLAOUT.EQ.' ') CLAOUT = CLAIN
      IF (SEQOUT.LE.0) SEQOUT = SEQIN
      IF (DISKO.LE.0) DISKO = DISKIN

      SAME = (NAMOUT.EQ.NAMEIN) .AND. (CLAOUT.EQ.CLAIN) .AND.
     *   (SEQOUT.EQ.SEQIN) .AND. (DISKO.EQ.DISKIN)
C                                       If OPTYPE='KEY ' SAME is true
      SAME = SAME .OR. (XOP.EQ.'KEY ')
      SAMOUT = (OVER.LT.0) .OR. (IVER.LT.0)
C                                       Output = input
      IF (SAME) THEN
         NEWCNO = OLDCNO
         IF (OVER.LE.0) OVER = IVER
C                                       Find output
      ELSE
         NEWCNO = 1
         TYPE = '  '
         CALL CATDIR ('SRCH', DISKO, NEWCNO, NAMOUT, CLAOUT, SEQOUT,
     *      TYPE, NLUSER, STAT, BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1030) IRET, NAMOUT, CLAOUT, SEQOUT, DISKO,
     *         NLUSER
            GO TO 990
            END IF
         END IF
      STAT = 'READ'
      IF (SAME) STAT = 'REST'
C                                       Read old CATBLK and mark 'READ'
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, STAT, BUFF1, IRET)
      IF ((IRET.GT.0) .AND. (IRET.LT.5)) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
      IF (.NOT.SAME) THEN
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKIN
         FCNO(NCFILE) = OLDCNO
         FRW(NCFILE) = 0
         END IF
C                                       Check number of tables
      CALL FNDEXT (TABTYP, CATOLD, NUMTAB)
      NUMTAB = MAX (1, NUMTAB)
C                                       See if all tables wanted
      IF (IVER.LT.0) THEN
         IVER = 1
      ELSE
         NUMTAB = 1
         END IF
C                                       Read new CATBLK and mark 'WRIT'
      CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'WRIT', BUFF1, IRET)
      IF ((IRET.GT.0) .AND. (IRET.LT.5)) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 1
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' GETTING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE TEDITR (NOTIME, ISCHAR, NUMSEL, NFLAG, IRET)
C-----------------------------------------------------------------------
C   Subroutine to edit Tables
C   Output:
C      NOTIME   L    If true, no "TIME" column was present.
C      ISCHAR   L    If true, the operation involved character input.
C      NUMSEL   I    Number of records selected.
C      NFLAG    I    Number records flagged
C      IRET     I    Return code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   NUMSEL, NFLAG, IRET
      LOGICAL   NOTIME, ISCHAR
C
      INTEGER   MAXKEY, MAXREC
C                                       MAXKEY = Max. no. keyword/value
C                                       pairs.
      PARAMETER (MAXKEY=1024)
C                                       MAXREC = Dimension of REC.
      INCLUDE 'INCS:PUVD.INC'
      PARAMETER (MAXREC=XBPRSZ)
      CHARACTER CHTIME*24, CHTRUE*8, OPCOD(9)*4, DTYPE(6)*8,
     *   COLPAC*1024, KEYDUM(MAXKEY)*8
      HOLLERITH RECH(MAXREC), VALH(2)
      INTEGER   BUFFER(512,2), LUN1, LUN2, NOPT, IOPT, I, IROUND, IBIN,
     *   IBOUT, NREC, NCOL, DATP(128,2), IRCODE, NKEY, KOLS(10), TIMKOL,
     *   REC(MAXREC), TYPCOD, CHKKOL, LOCS(MAXKEY), KEYTYP(MAXKEY),
     *   DATKOL, DATKLT, ARGI, NCHAR, LENGTH, VALUE(2*MAXKEY), I1, I2,
     *   IKEY, ICHPAT(8), CHKLEN, CHKPNT, NCOMP, NUMIN, NUMOUT, RNOIN,
     *   RNOOUT, LOOP, LIMIT1, LIMIT2, N
      LOGICAL   T, NEWTAB, EQUAL, ARGL, DOTR, DOTD, DOCHK, CHKI, CHKR,
     *   CHKD, CHKCH, VALL, LARRAY(100), FLAG
      REAL     VALR(2), RECR(MAXREC), ARGR, ARG2
      DOUBLE PRECISION T1, T2, TIME, RECD(MAXREC/2), ARGD, CHKVAL,
     *   CHKTOL, VALD, TEMPD, CHECK
      INCLUDE 'TABED.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (REC, RECH, RECR, RECD)
      EQUIVALENCE (VALUE, VALH, VALL, VALR, VALD)
      DATA CHTIME, CHTRUE /'TIME                    ','.TRUE.  '/
      DATA LUN1, LUN2 /27,28/
      DATA NOPT /9/
      DATA T /.TRUE./
      DATA OPCOD /'COPY','ADD ','MULT','DIV ','CLIP','REPL','DELE',
     *   'UFLG','KEY '/
      DATA DTYPE /'DOUBLE  ', 'SINGLE  ', 'CHAR    ',
     *   'LOGICAL ', 'INTEGER ', 'INTEGER '/
C-----------------------------------------------------------------------
      NOTIME = .TRUE.
      NUMSEL = 0
      NFLAG = 0
C                                       Find OPcode
      IOPT = 0
      DO 40 I = 1,NOPT
         IF (XOP.EQ.OPCOD(I)) IOPT = I
 40      CONTINUE
      IF (IOPT.LE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1040) XOP
         GO TO 990
         END IF
C                                       Same table?
      SAME = SAME .AND. ((OVER.EQ.0) .OR. (OVER.EQ.IVER))
      IF ((IOPT.EQ.7) .OR. (IOPT.EQ.8) .OR. (SAME)) APARM(10) = -1.0
C                                       On 'COPY' output must be
C                                       .NOT.SAME
      IF (SAME .AND. (IOPT.EQ.1)) THEN
         OVER = 0
         SAME = .FALSE.
         END IF
C                                       For KEYWORD open read first.
      IF (IOPT.EQ.NOPT) THEN
         DISKO = DISKIN
         NEWCNO = OLDCNO
         OVER = IVER
         CALL COPY (256, CATBLK, CATOLD)
         END IF
C                                       Open table(s)
      IBIN = 1
      IBOUT = 2
      IF (SAME) IBOUT = IBIN
      CALL TABINI ('READ', TABTYP, DISKIN, OLDCNO, IVER, CATOLD, LUN1,
     *   NKEY, NREC, NCOL, DATP, BUFFER(1,IBIN), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET, 'READ', TABTYP, IVER
         GO TO 990
         END IF
C                                       If SAME close and reopen WRITE
      IF (SAME) THEN
         OVER = IVER
         CALL TABIO ('CLOS', IRCODE, RNOIN, REC, BUFFER(1,IBIN), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1800) IRET, TABTYP, IVER
            GO TO 990
            END IF
         END IF
C                                       Open output
      IKEY = NKEY
      NREC = BUFFER(5,IBIN)
      CALL TABINI ('WRIT', TABTYP, DISKO, NEWCNO, OVER, CATBLK, LUN2,
     *   IKEY, NREC, NCOL, DATP, BUFFER(1,IBOUT), IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET, 'WRIT', TABTYP, IVER
         GO TO 990
         END IF
      NEWTAB = IRET.LT.0
C                                       Mark output as unsorted
      BUFFER(43,IBOUT) = 0
      BUFFER(44,IBOUT) = 0
C                                       See if A "TIME" axis exists
      IKEY = 1
      MSGSUP = 32000
      CALL FNDCOL (IKEY, CHTIME, 24, T, BUFFER(1,IBIN), KOLS, IRET)
      MSGSUP = 0
      NOTIME = IRET.NE.0
      IRET = 0
      I = KOLS(1)
      TIMKOL = DATP(I,1)
      DOTR = (.NOT.NOTIME) .AND. (MOD (DATP(I,2), 10).EQ.2)
      DOTD = (.NOT.NOTIME) .AND. (MOD (DATP(I,2), 10).EQ.1)
C                                       Set time range
      T1 = XTIME(1) + (XTIME(2) / 24.0) + (XTIME(3) / 1440.0) +
     *   (XTIME(4) / 86400.0)
      T2 = XTIME(5) + (XTIME(6) / 24.0) + (XTIME(7) / 1440.0) +
     *   (XTIME(8) / 86400.0)
      IF (ABS(T1).LE.1.0E-10) THEN
         T1 = -100000.0
         XTIME(1) = -100000.0
         END IF
      IF (ABS(T2).LE.1.0E-10) THEN
         T2 = 100000.0
         XTIME(5) = 100000.0
         END IF
      NOTIME = NOTIME .OR. ((T1.LT.-99999.0) .AND. (T2.GT.99999.0))
      TIME = (T1 + T2) * 0.5D0
C                                       New table, copy keywords. etc.
      IF (NEWTAB) CALL TABUP (BUFFER, NCOL, NKEY, REC, VALUE,
     *   LOCS, KEYTYP, KEYDUM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Non-KEYWORD operations
      IF (IOPT.NE.NOPT) THEN
C                                       Get number of input records
         NUMIN = BUFFER(5,IBIN)
C                                       Get number of records in output
         NUMOUT = BUFFER(5,IBOUT)
C                                       Setup for checking column value.
         DOCHK = APARM(5).GT.0.0
         CHKKOL = IROUND (APARM(5))
         CHKI = (DOCHK) .AND. (MOD (DATP(CHKKOL,2), 10).EQ.6)
         CHKI = (DOCHK) .AND. (MOD (DATP(CHKKOL,2), 10).EQ.4)
         CHKCH = (DOCHK) .AND. (MOD (DATP(CHKKOL,2), 10).EQ.3)
         CHKR = (DOCHK) .AND. (MOD (DATP(CHKKOL,2), 10).EQ.2)
         CHKD = (DOCHK) .AND. (MOD (DATP(CHKKOL,2), 10).EQ.1)
         DOCHK = DOCHK .AND. (CHKI.OR.CHKR.OR.CHKD.OR.CHKCH)
         CHKLEN = DATP(CHKKOL,2) / 10
         CHKKOL = DATP(CHKKOL,1)
         CHKVAL = APARM(6)
         CHKTOL = APARM(7)
         CHKPNT = APARM(8) + 0.5
         IF (CHKPNT.LT.1) CHKPNT = 1
         IF (CHKPNT.GT.CHKLEN) CHKPNT = CHKLEN
         APARM(8) = CHKPNT
C                                       Special for character test.
         IF (CHKCH) THEN
            NCOMP = APARM(9) + 0.5
            CALL PSFORM (NCOMP, XKEY, ICHPAT)
            END IF
C                                       Find data type code and column
C                                       pointer for column to work on.
         I = IROUND (APARM(1))
C                                       No col for "COPY","DELE","UFLG"
         IF (IOPT.EQ.1) I = 1
         IF (IOPT.GT.6) I = 1
         IF ((I.LT.1) .OR. (I.GT.NCOL)) THEN
            IRET = 1
            WRITE (MSGTXT,1120) I, NCOL
            GO TO 990
            END IF
         TYPCOD = MOD (DATP(I,2), 10)
         LENGTH = DATP(I,2) / 10
         N = LENGTH
         DATKOL = DATP(I,1)
C                                       No col. for "COPY"
         IF (IOPT.EQ.1) TYPCOD = 0
         ISCHAR = (TYPCOD.EQ.3) .OR. (TYPCOD.EQ.5) .OR. (TYPCOD.EQ.7)
C                                       Set array limits
         I1 = IROUND (APARM(2))
         I2 = IROUND (APARM(3))
         IF (I1.LE.0) I1 = 1
         IF (I2.LE.0) I2 = LENGTH
         IF (I2.GT.LENGTH) I2 = LENGTH
C                                       Check allowed operations for
C                                       data types:
         IF ((IOPT.GE.2) .AND. (IOPT.LE.5) .AND. (ISCHAR)) THEN
            IRET = 3
            WRITE (MSGTXT,1150) XOP, DTYPE(TYPCOD)
            GO TO 990
            END IF
C                                       need a value
         IF ((IOPT.GT.1) .AND. (IOPT.LT.7)) THEN
C                                       Get operand in correct form
C                                       Double precision
            CALL POPSRD ('R2D', XKEYV, ARGD)
C                                       Single precision
            ARGR = XKEYV(1)
            ARG2 = XKEYV(2)
C                                       Integer
            IF (TYPCOD.EQ.4) ARGI = IROUND (ARGR)
C                                       No. char.
            NCHAR = I2 - I1 + 1
            NCHAR = MIN (16, LENGTH, NCHAR)
C                                       Logical
            ARGL = CHTRUE .EQ. XKEYS(1:8)
            END IF
C                                       Loop thru table
         LIMIT1 = 1
         IF (XBC.GT.0.1) LIMIT1 = XBC + 0.5
         IF (LIMIT1.GT.NUMIN) LIMIT1 = NUMIN
         BC = MIN (LIMIT1, BC)
         LIMIT2 = NUMIN
         IF (XEC.GT.0.1) LIMIT2 = XEC + 0.5
         IF (LIMIT2.GT.NUMIN) LIMIT2 = NUMIN
         EC = MAX (LIMIT2, EC)
         IRCODE = 0
         RNOOUT = NUMOUT
         DO 200 LOOP = 1,NUMIN
            FLAG = .FALSE.
C                                       Read input record.
            RNOIN = LOOP
            CALL TABIO ('READ', IRCODE, RNOIN, REC, BUFFER(1,IBIN),
     *         IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1160) IRET, 'READ', RNOIN
               GO TO 990
               END IF
C                                       Ignore blanked except UFLG
            IF (IRET.LT.0) THEN
               FLAG = .TRUE.
               IF (IOPT.NE.8) GO TO 180
            ELSE
               IF (IOPT.EQ.8) GO TO 180
               END IF
            IF ((LOOP.LT.LIMIT1) .OR. (LOOP.GT.LIMIT2)) GO TO 180
C                                       Check time
            IF (DOTR) TIME = RECR(TIMKOL)
            IF (DOTD) TIME = RECD(TIMKOL)
            IF ((TIME.LT.T1) .OR. (TIME.GT.T2)) GO TO 180
C                                       Check test col:
            IF (CHKI) CHECK = REC(CHKKOL+CHKPNT-1)
            IF (CHKR) CHECK = RECR(CHKKOL+CHKPNT-1)
            IF (CHKD) CHECK = RECD(CHKKOL+CHKPNT-1)
C                                       Character test
            IF (CHKCH) THEN
               CALL H2CHR (CHKLEN, 1, RECH(CHKKOL), COLPAC)
               CALL CHWMAT (NCOMP, XKEY, ICHPAT, CHKPNT, COLPAC, EQUAL)
               CHECK = CHKVAL + 10.0 + 10.0 * CHKTOL
               IF (EQUAL) CHECK = CHKVAL
               END IF
            IF (DOCHK .AND. (ABS (CHECK-CHKVAL).GT.CHKTOL)) GO TO 180
C                                       Count records selected.
            NUMSEL = NUMSEL + 1
            DATKLT = DATKOL + I1 - 1
C                                       Characters (REPL only)
            IF (TYPCOD.EQ.3) THEN
               CALL CHR2H (NCHAR, XKEYS, I1, RECH(DATKOL))
C                                       Logical (REPL only)
            ELSE IF ((TYPCOD.EQ.5) .OR. (TYPCOD.EQ.7)) THEN
               CALL LG2BIT (N, LARRAY, REC(DATKOL), -1)
               DO 110 I = I1,I2
                  LARRAY(I) = ARGL
 110              CONTINUE
               CALL LG2BIT (N, LARRAY, REC(DATKOL), 1)
C                                       ADD
            ELSE IF (IOPT.EQ.2) THEN
               DO 120 I = I1,I2
                  IF (TYPCOD.EQ.1) RECD(DATKLT) = RECD(DATKLT) + ARGD
                  IF (TYPCOD.EQ.2) RECR(DATKLT) = RECR(DATKLT) + ARGR
                  IF (TYPCOD.EQ.4) REC(DATKLT) = REC(DATKLT) + ARGI
                  DATKLT = DATKLT + 1
 120              CONTINUE
C                                       MULT
            ELSE IF (IOPT.EQ.3) THEN
               DO 130 I = I1,I2
                  IF (TYPCOD.EQ.1) RECD(DATKLT) = RECD(DATKLT) * ARGD
                  IF (TYPCOD.EQ.2) RECR(DATKLT) = RECR(DATKLT) * ARGR
                  IF (TYPCOD.EQ.4) REC(DATKLT) = REC(DATKLT) * ARGI
                  DATKLT = DATKLT + 1
 130              CONTINUE
C                                       DIV
            ELSE IF (IOPT.EQ.4) THEN
               DO 140 I = I1,I2
                  IF (TYPCOD.EQ.1) RECD(DATKLT) = RECD(DATKLT) / ARGD
                  IF (TYPCOD.EQ.2) RECR(DATKLT) = RECR(DATKLT) / ARGR
                  IF (TYPCOD.EQ.4) REC(DATKLT) = REC(DATKLT) / ARGI
                  DATKLT = DATKLT + 1
 140              CONTINUE
C                                       CLIP
            ELSE IF (IOPT.EQ.5) THEN
               DO 150 I = I1,I2
                  IF (TYPCOD.EQ.1) TEMPD = RECD(DATKLT)
                  IF (TYPCOD.EQ.2) TEMPD = RECR(DATKLT)
                  IF (TYPCOD.EQ.4) TEMPD = REC(DATKLT)
                  IF (ARGR.LT.ARG2) THEN
                     IF ((TEMPD.LT.ARGR) .OR. (TEMPD.GT.ARG2))
     *                  FLAG = .TRUE.
                  ELSE
                     IF ((TEMPD.GE.ARG2) .AND. (TEMPD.LE.ARGR))
     *                  FLAG = .TRUE.
                     END IF
                  DATKLT = DATKLT + 1
 150              CONTINUE
C                                       REPL
            ELSE IF (IOPT.EQ.6) THEN
               DO 160 I = I1,I2
                  IF (TYPCOD.EQ.1) RECD(DATKLT) = ARGD
                  IF (TYPCOD.EQ.2) RECR(DATKLT) = ARGR
                  IF (TYPCOD.EQ.4) REC(DATKLT) = ARGI
                  DATKLT = DATKLT + 1
 160              CONTINUE
C                                       DELE, UFLG
            ELSE IF ((IOPT.EQ.7) .OR. (IOPT.EQ.8)) THEN
               FLAG = .NOT.FLAG
               END IF
            GO TO 190
C                                       do not copy all??
 180        IF (APARM(10).GT.0.0) GO TO 200
C                                       output
 190        IF (SAME) THEN
               RNOOUT = RNOIN
            ELSE
               RNOOUT = RNOOUT + 1
               END IF
            IF (FLAG) THEN
               NFLAG = NFLAG + 1
               CALL TABIO ('FLAG', IRCODE, RNOOUT, REC, BUFFER(1,IBOUT),
     *            IRET)
            ELSE
               CALL TABIO ('WRIT', IRCODE, RNOOUT, REC, BUFFER(1,IBOUT),
     *            IRET)
               END IF
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1160) IRET, 'WRIT', RNOOUT
               GO TO 990
               END IF
 200        CONTINUE
C                                       Tell number of records selected
         WRITE (MSGTXT,1200) NUMSEL
         CALL MSGWRT (4)
         WRITE (MSGTXT,1201) NFLAG
         IF (NFLAG.GT.0) CALL MSGWRT (4)
C                                       Keyword operations
      ELSE
         NKEY = 1
         KEYTYP(1) = IROUND (APARM(4))
C                                       6 (short integer) now 4
         IF (KEYTYP(1).EQ.6) KEYTYP(1) = 4
C                                       single precision deprecated too
         IF (KEYTYP(1).EQ.2) KEYTYP(1) = 1
         ISCHAR = (KEYTYP(1).EQ.3) .OR. (KEYTYP(1).EQ.5) .OR.
     *      (KEYTYP(1).EQ.7)
C                                       Enter data in VALUE via
C                                       EQUIVALENCE
         VALD = 0.0D0
         CALL POPSRD ('R2D', XKEYV, TEMPD)
         IF (KEYTYP(1).EQ.1) VALD = TEMPD
         IF (KEYTYP(1).EQ.3) CALL CHR2H (8, XKEYS(1:8), 1, VALH(1))
         IF (KEYTYP(1).EQ.4) VALUE(1) = IROUND (XKEYV(1))
         IF (KEYTYP(1).EQ.5) VALL = CHTRUE .EQ. XKEYS(1:8)
         LOCS(1) = 1
         NOTIME = .TRUE.
C                                       Write/update keyword.
         CALL TABKEY ('WRIT', XKEY, NKEY, BUFFER(1,IBOUT), LOCS, VALUE,
     *      KEYTYP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1500) IRET
            GO TO 990
            END IF
         END IF
C                                       Close tables
      CALL TABIO ('CLOS', IRCODE, RNOIN, REC, BUFFER(1,IBIN), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1800) IRET, TABTYP, IVER
         GO TO 990
         END IF
      IF (.NOT.SAME) THEN
         CALL TABIO ('CLOS', IRCODE, RNOOUT, REC, BUFFER(1,IBOUT), IRET)
         IF (IRET.NE.0) WRITE (MSGTXT,1800) IRET, TABTYP, OVER
         END IF
C                                       Error
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('UNKNOWN OPTYPE = ',A4)
 1100 FORMAT ('TABINI ERROR ',I3,' OPEN FOR ',A4,1X,A2,', TABLE ',
     *   ' VER=',I3)
 1120 FORMAT ('COLUMN ',I3,' OUT OF RANGE (1:',I4,')')
 1150 FORMAT ('OPTYPE ',A4,' INAPPROPRIATE FOR DATA TYPE ',A8)
 1160 FORMAT ('TABIO ERROR',I3,1X,A4,'ING RECORD ',I7)
 1200 FORMAT (I8, ' Records selected')
 1201 FORMAT (I8, ' Records of these were flagged')
 1500 FORMAT ('TABKEY ERROR',I3,' WRITING/UPDATING TABLE KEYWORD')
 1800 FORMAT ('TABIO ERROR ',I3,' CLOSING ',A2,' TABLE VER=',I3)
      END
      SUBROUTINE TEDHIS (NOTIME, ISCHAR, NUMSEL, NFLAG)
C-----------------------------------------------------------------------
C   Add reference in output history
C   Input:
C      NOTIME   L   If true then no time selection was done.
C      ISCHAR   L   If true operation used character input
C      NUMSEL   I   Number of records selected.
C      NFLAG    I   Number records flagged
C-----------------------------------------------------------------------
      LOGICAL   NOTIME, ISCHAR
      INTEGER   NUMSEL, NFLAG
C
      CHARACTER HILINE*72, CTIME(2)*12
      INTEGER   BUFFER(1024), LUN1, IRET, IAPARM(10), I, IROUND,
     *   TIME(3), DATE(3)
      REAL      TIME1, TIME2
      LOGICAL   T
      INCLUDE 'TABED.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA LUN1 /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN1, DISKO, NEWCNO, BUFFER, IRET)
      IF (IRET.GT.2) THEN
         WRITE (MSGTXT,1070) IRET
         CALL MSGWRT (6)
         GO TO 250
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1100) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN1, HILINE, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 250
C                                       Input file
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN1, BUFFER,
     *   IRET)
      IF (IRET.NE.0) GO TO 250
C                                       Type and version
      IF (NUMTAB.GT.1) THEN
         IVER = IROUND (XIVER)
         OVER = IROUND (XOVER)
         END IF
      WRITE (HILINE,2000) TSKNAM, TABTYP, IVER, OVER
      CALL HIADD (LUN1, HILINE, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 250
C                                       BCOUNT, ECOUNT
      IF (XOP.NE.'KEY ') THEN
         WRITE (HILINE,2001) TSKNAM, BC, EC
         CALL HIADD (LUN1, HILINE, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 250
         END IF
C                                       OPTYPE
      WRITE (HILINE,2002) TSKNAM, XOP
      CALL HIADD (LUN1, HILINE, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 250
C                                       APARM (integer)
      DO 150 I = 1,10
         IAPARM(I) = IROUND (APARM(I))
 150     CONTINUE
      WRITE (HILINE,2003) TSKNAM, (IAPARM(I), I=1,5)
      CALL HIADD (LUN1, HILINE, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 250
C                                       Test values
      IF (APARM(5).GT.0.001) THEN
         WRITE (HILINE,3004) TSKNAM, APARM(6), APARM(7)
         CALL HIADD (LUN1, HILINE, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 250
         WRITE (HILINE,3005) TSKNAM, IAPARM(8), IAPARM(9)
         CALL HIADD (LUN1, HILINE, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 250
         END IF
C                                       KEYWORD
      IF (('KEY '.EQ.XOP) .OR. (APARM(5).GT.0.1)) THEN
         WRITE (HILINE,2004) TSKNAM, XKEY
         CALL HIADD (LUN1, HILINE, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 250
         END IF
C                                       KEYSTRING
      IF (ISCHAR) THEN
         WRITE (HILINE,2005) TSKNAM, XKEYS
         CALL HIADD (LUN1, HILINE, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 250
C                                       KEYVALUE
      ELSE
         WRITE (HILINE,2006) TSKNAM, XKEYV
         CALL HIADD (LUN1, HILINE, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 250
         END IF
C                                       TIMERANG
      IF (.NOT.NOTIME) THEN
         TIME1 = XTIME(1) + (XTIME(2) / 24.0) + (XTIME(3) / 1440.0) +
     *      (XTIME(4) / 86400.0)
         TIME2 = XTIME(5) + (XTIME(6) / 24.0) + (XTIME(7) / 1440.0) +
     *      (XTIME(8) / 86400.0)
         CALL HITIME (TIME1, TIME2, LUN1, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 250
         END IF
C                                       Number of records selected
      IF (XOP.NE.'KEY ') THEN
         WRITE (HILINE,2008) TSKNAM, NUMSEL
         CALL HIADD (LUN1, HILINE, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 250
         IF (NFLAG.GT.0) THEN
            WRITE (HILINE,2009) TSKNAM, NFLAG
            CALL HIADD (LUN1, HILINE, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 250
            END IF
         END IF
C                                       Close HI file
 250  CALL HICLOS (LUN1, T, BUFFER, IRET)
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('ERROR',I3,' OPENING HISTORY FILE')
 1100 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2000 FORMAT (A6,' INEXT=''',A2,''' INVERS=',I4,' OUTVERS=', I4)
 2001 FORMAT (A6,' BCOUNT =',I8,' ECOUNT =',I8,' /Range of rows')
 2002 FORMAT (A6,' OPTYPE = ''',A4,''' /Operation code')
 2003 FORMAT (A6,' APARM=',4(I6,','),I6,' /Control info')
 3004 FORMAT (A6,' APARM(6)=',1PE13.5,', APARM(7)=',1E13.5,
     *   ' /Test value, tol.')
 3005 FORMAT (A6,' APARM(8)=',I6,', APARM(9)=',I6,
     *   ' /Test subscript, no. char.')
 2004 FORMAT (A6,' KEYWORD = ''',A8,'''')
 2005 FORMAT (A6,' KEYSTRING = ''',A16,''' /Char. operand')
 2006 FORMAT (A6,' KEYVALUE = ',1PE15.7,',',E15.7,' /Num. operand')
 2008 FORMAT (A6,' / ',I8,' Records selected')
 2009 FORMAT (A6,' / ',I8,' Records flagged')
      END
      SUBROUTINE TABUP (BUFFER, NCOL, NKEY, REC, VALUE, LOCS, KEYTYP,
     *     KEYDUM, IRET)
C-----------------------------------------------------------------------
C   Routine to update the header of a newly created table.
C   Inputs:
C    Buffer(512,2)   I    TABIO buffer of the old (*,1) and new (*,2)
C                         tables.
C    NCOL            I    Number of columns
C    NKEY            I    Number of keywords
C  Output:
C    REC(XBPRSZ)     I    Record array for I/O
C    VALUE(*)        I    Array for TABKEY.
C    LOCS(*)         I    Array for TABKEY.
C    KEYTYP(*)       I    Array for TABKEY.
C    KEYDUM(*)       C*8  Work array for keyword names.
C    IRET            I    Error code, 0=OK, else TABIO or TABKEY error.
C-----------------------------------------------------------------------
      INTEGER   BUFFER(512,2), NCOL, NKEY, REC(*), VALUE(*), LOCS(*),
     *   KEYTYP(*), IRET
      CHARACTER KEYDUM(NKEY)*8
C
      INTEGER   IKEY, IRCODE, NTT, IRNO
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Update keywords
C                                       Read
      IKEY = NKEY
      CALL TABKEY ('ALL ', KEYDUM, IKEY, BUFFER(1,1), LOCS, VALUE,
     *   KEYTYP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Write
      CALL TABKEY ('WRIT', KEYDUM, IKEY, BUFFER(1,2), LOCS, VALUE,
     *   KEYTYP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 990
         END IF
C                                       Copy  labels, units
      DO 100 IRNO = 1,NCOL
C                                       Col. labels.
         IRCODE = 3
C                                       Read
         CALL TABIO ('READ', IRCODE, IRNO, REC, BUFFER(1,1), IRET)
         IF (IRET.NE.0) GO TO 980
C                                       Write
         CALL TABIO ('WRIT', IRCODE, IRNO, REC, BUFFER(1,2), IRET)
         IF (IRET.NE.0) GO TO 980
C                                       Units
         IRCODE = 4
C                                       Read
         CALL TABIO ('READ', IRCODE, IRNO, REC, BUFFER(1,1), IRET)
         IF (IRET.NE.0) GO TO 980
C                                       Write
         CALL TABIO ('WRIT', IRCODE, IRNO, REC, BUFFER(1,2), IRET)
         IF (IRET.NE.0) GO TO 980
 100     CONTINUE
C                                       Fill in Table title
      NTT = 14
      CALL RCOPY (NTT, BUFFER(101,1), BUFFER(101,2))
      GO TO 999
C                                       TABIO error
 980  WRITE (MSGTXT,1980) IRET
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TABKEY ERROR',I3,' READ-FOR-COPY TABLE KEYWORDS')
 1050 FORMAT ('TABKEY ERROR',I3,' WRITE-FOR-COPY TABLE KEYWORDS')
 1980 FORMAT ('TABUP: TABIO ERROR',I3,' UPDATING NEW HEADER')
      END
