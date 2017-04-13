      PROGRAM TACOP
C-----------------------------------------------------------------------
C! Task to copy table extension files.
C# EXT-util Utility Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2002, 2005, 2008-2009, 2015
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
C  TACOP copies AIPS extension tables files.  The specified output table
C  should not exist before the execution of TACOP.
C  ADVERBS:
C    INNAME      Input image name (name)
C    INCLASS     Input image name (class)
C    INSEQ       Input image name (seq. #)
C    INDISK      Input image disk unit #
C    INEXT       Input table extension type
C    INVERS      Input table file version no.
C    OUTNAME     Output image name (name)
C    OUTCLASS    Output image name (class)
C    OUTSEQ      Output image name (seq. #)
C    OUTDISK     Output image disk unit #.
C    OUTVERS     Output table file version.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  NAMEIN*12, CLAIN*6, FTYPE*2, STAT*4, NAMOUT*12,
     *   CLAOUT*6, XEXT*2, XKEYS*16, TSTSTR*8, PRGM*6, TABTYP*2,
     *   KEYW*8, ATIME*8, ADATE*12, XKEYW*8
      HOLLERITH XNAMIN(3), XCLAIN(2), XNMOUT(3), XCLOUT(2), XNEXT,
     *   CATHLD(256), XHKEYW(2), XHKEYS(4)
      INTEGER  IRET, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD(256), BUFF1(512), BUFF2(512), IERR, IVER, OVER, LUN1,
     *   LUN2, NPARM, IROUND,  NTAB, ITAB, LOCS, KTYPE, VALI2, NKEY,
     *   NREC, NCOL, ICHPAT(8), NOLDT, CPYCNT, TIME(3), DATE(3),
     *   FLAGV
      LOGICAL   EQUAL, EXIST, TABLE, FITASC
      INTEGER   I4TEMP, VALI4
      REAL      XSEQIN, XDISKI, XIVER, XNC, XFLAG, XSEQO, XDISKO, XOVER,
     *   XKEYV(2), VALR4(2), VALUE, DDFLAG, SCRTCH(XBPRSZ)
      DOUBLE PRECISION    VALR8
      LOGICAL   T, SAME
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INPARM/ XNAMIN, XCLAIN, XSEQIN, XDISKI, XNEXT, XIVER,
     *   XNC, XFLAG, XNMOUT, XCLOUT, XSEQO, XDISKO, XOVER, XHKEYW,
     *   XKEYV, XHKEYS, DDFLAG
      EQUIVALENCE (VALI2, VALI4, VALR4, VALR8)
      EQUIVALENCE (CATOLD, CATHLD)
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA PRGM /'TACOP '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 28
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMIN, BUFF1, IERR)
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
      CALL SELINI
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMIN, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNMOUT, NAMOUT)
      CALL H2CHR (6, 1, XCLOUT, CLAOUT)
      CALL H2CHR (2, 1, XNEXT, XEXT)
      CALL H2CHR (16, 1, XHKEYS, XKEYS)
      CALL H2CHR (8, 1, XHKEYW, XKEYW)
C                                       Crunch input parameters.
      SEQIN  = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO  = IROUND (XDISKO)
      IVER   = IROUND (XIVER)
      OVER   = IROUND (XOVER)
      NTAB   = IROUND (XNC)
      FLAGV  = IROUND (XFLAG)
      XOVER  = OVER
      TABTYP = XEXT
C                                       Prevent write to disk # 0 !
      IF (DISKO.EQ.0) DISKO = DISKIN
C                                       Default output = input.
      IF (NAMOUT.EQ.' ') NAMOUT = NAMEIN
      IF (CLAOUT.EQ.' ') CLAOUT = CLAIN
C                                       Set up for character compare
      CALL PSFORM (8, XKEYS, ICHPAT)
C                                       Find input
      OLDCNO = 1
      FTYPE = ' '
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, FTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Find output
      NEWCNO = 1
      FTYPE = ' '
      CALL CATDIR ('SRCH', DISKO, NEWCNO, NAMOUT, CLAOUT, SEQOUT, FTYPE,
     *    NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMOUT, CLAOUT, SEQOUT, DISKO,
     *      NLUSER
         GO TO 990
         END IF
      SAME = (DISKIN.EQ.DISKO) .AND. (OLDCNO.EQ.NEWCNO)
      STAT = 'READ'
      IF (SAME) STAT = 'REST'
C                                       Read old CATBLK and mark 'READ'
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, STAT, BUFF1, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      IF (.NOT.SAME) THEN
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKIN
         FCNO(NCFILE) = OLDCNO
         FRW(NCFILE) = 0
         END IF
C                                       Read new CATBLK and mark 'WRIT'
      CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'WRIT', BUFF1, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 1
C                                       Check number of tables
      CALL FNDEXT (TABTYP, CATOLD, NOLDT)
C                                       use highest version when IVER = 0
      IF (IVER.LE.0) IVER = NOLDT
      XIVER = IVER
C                                       NCOUNT .le. 0 => do all with
C                                       version numbers greater than or
C                                       equal to IVER.
      IF (NTAB.LE.0) NTAB = NOLDT - IVER + 1
      NTAB = MIN (NTAB, NOLDT - IVER + 1)
      KEYW(1:8) = XKEYW(1:8)
      CPYCNT = 0
C                                       init flagging
      IF ((FLAGV.GT.0) .AND. (FTYPE.EQ.'UV')) THEN
         IF ((TABTYP.EQ.'SN') .OR. (TABTYP.EQ.'TY') .OR.
     *      (TABTYP.EQ.'SY')) THEN
            CALL COPY (256, CATBLK, BUFF1)
            CALL COPY (256, CATOLD, CATBLK)
            CALL UVPGET (IERR)
            CALL COPY (256, BUFF1, CATBLK)
            CALL INITFG (FLAGV, DISKIN, OLDCNO, CATOLD, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1045) IERR
               GO TO 990
               END IF
            END IF
         END IF
C                                       Loop over tables
      DO 90 ITAB = 1,NTAB
C                                       Is it a table
         CALL ISTAB (TABTYP, DISKIN, OLDCNO, IVER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (IERR.NE.0) THEN
            IRET = 5
            WRITE (MSGTXT,1080) IERR
            GO TO 990
            END IF
         IF (.NOT.EXIST) THEN
            WRITE (MSGTXT,1050) TABTYP, IVER
            CALL MSGWRT (6)
            GO TO 85
            END IF
C                                       See if desired:
         IF (KEYW.NE.' ') THEN
            IF (.NOT.TABLE) THEN
               MSGTXT = 'KEYWORD TEST MAY NOT BE APPLIED TO NON TABLES'
               IRET = 5
               GO TO 990
               END IF
C                                       Do test on table keyword
            NKEY = 0
            NREC = 0
            NCOL = 0
C                                       Open table
            CALL TABINI ('READ', TABTYP, DISKIN, OLDCNO, IVER, CATOLD,
     *         LUN1, NKEY, NREC, NCOL, BUFF2, BUFF1, IERR)
            IF (IERR.NE.0) THEN
               IRET = 5
               WRITE (MSGTXT,1081) IERR
               GO TO 990
               END IF
C                                       Get keyword
            NKEY = 1
            CALL TABKEY ('READ', KEYW, NKEY, BUFF1, LOCS, VALR8, KTYPE,
     *         IERR)
            IF ((IERR.GT.0) .AND. (IERR.LE.20)) THEN
               IRET = 5
               WRITE (MSGTXT,1082) IERR
               GO TO 990
               END IF
C                                       Don't do table with missing
C                                       keyword.
            IF (IERR.GT.20) GO TO 85
C                                       Close table
            CALL TABIO ('CLOS', 1, I4TEMP, BUFF2, BUFF1, IERR)
            IF (IERR.NE.0) THEN
               IRET = 5
               WRITE (MSGTXT,1083) IERR
               GO TO 990
               END IF
C                                       Decide if table wanted
            VALUE = 0.0
            IF (KTYPE.EQ.1) VALUE = VALR8
            IF (KTYPE.EQ.2) VALUE = VALR4(1)
            IF (KTYPE.EQ.4) VALUE = VALI4
            IF (KTYPE.EQ.6) VALUE = VALI2
            IF (KTYPE.EQ.3) THEN
C                                       Character
               CALL H2CHR (8, 1, VALR4, TSTSTR)
               CALL CHWMAT (8, XKEYS, ICHPAT, 1, TSTSTR, EQUAL)
               IF (.NOT.EQUAL) GO TO 85
            ELSE
C                                       Numeric keyword
C                                       Inclusive range
               IF (((VALUE.LT.XKEYV(1)) .OR. (VALUE.GT.XKEYV(2)))
     *            .AND. (XKEYV(1).LT.XKEYV(2))) GO TO 85
C                                       Exclusive range
               IF (((VALUE.GT.XKEYV(2)) .AND. (VALUE.LT.XKEYV(1)))
     *            .AND. (XKEYV(1).GT.XKEYV(2))) GO TO 85
               END IF
            END IF
C                                       Copy table
         IF ((FLAGV.GT.0) .AND. (TABTYP.EQ.'SN')) THEN
            CALL SNFCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IVER, OVER,
     *         CATOLD, CATBLK, LUN1, LUN2, DDFLAG, BUFF1, BUFF2, IRET)
         ELSE IF ((FLAGV.GT.0) .AND. (TABTYP.EQ.'TY')) THEN
            CALL TYFCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IVER, OVER,
     *         CATOLD, CATBLK, LUN1, LUN2, DDFLAG, BUFF1, BUFF2, IRET)
         ELSE IF ((FLAGV.GT.0) .AND. (TABTYP.EQ.'SY')) THEN
            CALL SYFCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IVER, OVER,
     *         CATOLD, CATBLK, LUN1, LUN2, DDFLAG, BUFF1, BUFF2, IRET)
         ELSE IF (TABLE) THEN
            CALL TBLCOP (TABTYP, IVER, OVER, LUN1, LUN2, DISKIN, DISKO,
     *         OLDCNO, NEWCNO, CATBLK, DDFLAG, BUFF1, BUFF2, SCRTCH,
     *         IRET)
         ELSE
            CALL FILCOP (TABTYP, IVER, OVER, LUN1, LUN2, DISKIN, DISKO,
     *         OLDCNO, NEWCNO, CATBLK, BUFF1, BUFF2, IRET)
            END IF
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.1) THEN
               MSGTXT = 'INPUT AND OUTPUT EXTSION FILES ARE THE SAME.'
               GO TO 990
            ELSE IF (IRET.EQ.6) THEN
               WRITE (MSGTXT,1085) TABTYP, OVER
               GO TO 990
            ELSE
               WRITE (MSGTXT,1086) IRET, TABTYP, IVER
               GO TO 990
               END IF
            END IF
         CPYCNT = CPYCNT + 1
         IF (OVER.GT.0) OVER = OVER + 1
 85      IVER = IVER + 1
 90      CONTINUE
C                                       Finished copying tables
      IRET = 0
C                                       Add history to output
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN1, DISKO, NEWCNO, BUFF1, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1090) IERR
         CALL MSGWRT (6)
         GO TO 150
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      WRITE (MSGTXT,1100) TSKNAM, RLSNAM, ADATE, ATIME
      CALL HIADD (LUN1, MSGTXT, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 150
C                                       Input file
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN1, BUFF1,
     *   IERR)
      IF (IERR.NE.0) GO TO 150
C                                       Type and version
      IVER = IROUND (XIVER)
      OVER = IROUND (XOVER)
      WRITE (MSGTXT,2000) TSKNAM, TABTYP, IVER, OVER
      CALL HIADD (LUN1, MSGTXT, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 150
C                                       NCOUNT
      WRITE (MSGTXT,2001) TSKNAM, NTAB
      CALL HIADD (LUN1, MSGTXT, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 150
C                                       Keyword
      IF (KEYW.NE.' ') THEN
         WRITE (MSGTXT,2002) TSKNAM, KEYW
         CALL HIADD (LUN1, MSGTXT, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 150
C                                       KEYSTRNG
         IF (KTYPE.EQ.3) THEN
            TSTSTR(1:8) = XKEYS(1:8)
            WRITE (MSGTXT,2003) TSKNAM, TSTSTR
            CALL HIADD (LUN1, MSGTXT, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 150
         ELSE
C                                       KEYVALUE
            WRITE (MSGTXT,2004) TSKNAM, XKEYV
            CALL HIADD (LUN1, MSGTXT, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 150
            END IF
         END IF
C                                       Number of tables copied
      WRITE (MSGTXT,2005) TSKNAM, CPYCNT
      IF (CPYCNT.LE.0) WRITE (MSGTXT,2006) TSKNAM
      CALL MSGWRT (6)
      CALL HIADD (LUN1, MSGTXT, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 150
C                                       Close HI file
 150  CALL HICLOS (LUN1, T, BUFF1, IERR)
      GO TO 995
C                                       Error
 990  CALL MSGWRT (8)
C                                       Close down files, etc
 995  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' GETING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1045 FORMAT ('ERROR',I4,' INITIALIZING FLAGGING')
 1050 FORMAT ('Extension file ',A2,' version',I5,' missing')
 1080 FORMAT ('ISTAB ERROR ',I3,' CHECKING FILE')
 1081 FORMAT ('TABINI ERROR ',I3,' EXAMINING KEYWORD')
 1082 FORMAT ('TABKEY ERROR ',I3,' EXAMINING KEYWORD')
 1083 FORMAT ('TABIO CLOSE ERROR ',I3,' EXAMINING KEYWORD')
 1085 FORMAT ('OUTPUT EXTENSION FILE ALREADY EXISTS: ',A2,I5)
 1086 FORMAT ('TBLCOP ERROR ',I4,' COPYING EXT FILE ',A2,I5)
 1090 FORMAT ('ERROR',I3,' OPENING HISTORY FILE')
 1100 FORMAT (A6,'Release =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2000 FORMAT (A6,' INEXT=''',A2,''' INVERS=',I4,' OUTVERS=', I4)
 2001 FORMAT (A6,' NCOUNT=',I5,' /Number of tables to copy')
 2002 FORMAT (A6,' KEYWORD=''',A8,''' / Keyword for test')
 2003 FORMAT (A6,' KEYSTRNG=''',A8,''' / String for test')
 2004 FORMAT (A6,' KEYVALUE=',1PE12.5,',',E12.5,' / Allowed range')
 2005 FORMAT (A6,' / Copied ',I6,' extension files')
 2006 FORMAT (A6,' / Warning: NO extension files copied')
      END
      SUBROUTINE TBLCOP (TYPE, INVER, OUTVER, LUNOLD, LUNNEW, VOLOLD,
     *   VOLNEW, CNOOLD, CNONEW, CATNEW, DDFLAG, BUFOLD, BUFNEW, SCRTCH,
     *   IRET)
C-----------------------------------------------------------------------
C   TABCOP copies Table extension file(s).  The output file must be a
C   new extension - old ones cannot be rewritten.  The output file
C   must be opened WRIT in the catalog and will have its CATBLK
C   updated on disk.
C     LOCAL VERSION: adds option to drop flagged records and so uses
C     TABINI etc rather than ZFIO.
C   Inputs:
C      TYPE     C*2     Extension file type (e.g. 'CC','AN')
C      INVER    I       Version number to copy, 0 => copy all.
C      OUTVER   I       Version number on output file, if more than one
C                       copied (INVER=0) this will be the number of the
C                       first file.  If OUTVER = 0, it will be taken as
C                       1 higher than the previous highest version.
C      LUNOLD   I       LUN for old file
C      LUNNEW   I       LUN for new file
C      VOLOLD   I       Disk number for old file.
C      VOLNEW   I       Disk number for new file.
C      CNOOLD   I       Catalog slot number for old file
C      CNONEW   I       Catalog slot number for new file
C      DDFLAG   R       > 0 => copy flagged rows
C   In/out:
C      CATNEW   I(256)  Catalog header for new file.
C   Output:
C      BUFOLD   I(512)  Work buffer
C      BUFNEW   I(512)  Work buffer
C      SCRTCH   I(*)    Buffer large enough for one data row
C      IRET     I       Return error code  0 => ok
C                          1 => files the same, no copy.
C                          2 => no input files exist
C                          3 => failed
C                          4 => no output files created.
C                          5 => failed to update CATNEW
C                          6 => output file exists
C   Inputs from MSG common:
C      MSGSUP   I       If 31990 < MSGSUP < 32000, the file copied
C                       message is suppressed.
C-----------------------------------------------------------------------
      INTEGER   INVER, OUTVER, LUNOLD, LUNNEW, VOLOLD, VOLNEW, CNOOLD,
     *   CNONEW, BUFOLD(512), BUFNEW(512), CATNEW(256), IRET
      CHARACTER TYPE*2
      REAL      DDFLAG, SCRTCH(*)
C
      INTEGER   COUNT, IVERI, IVERO, OVO, OVN, I, J, II, LIM, IER, JERR,
     *   IRNO, CATOLD(256), IDATP(128,2), ODATP(128,2), NREC, NCOL,
     *   NKEY, OFIND, IFIND, NFIRST, OREC
      LOGICAL   TABLE, EXIST, FITASC
      CHARACTER CHTEMP*2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Set initial version numbers.
      COUNT = 0
      IVERI = MAX (1, INVER)
      IVERO = OUTVER
C                                       Check if files the same
      IRET = 1
      IF ((VOLOLD.EQ.VOLNEW) .AND. (CNOOLD.EQ.CNONEW) .AND.
     *   (INVER.EQ.OUTVER)) GO TO 999
C                                       See if new file exists
      IRET = 6
      CALL ISTAB (TYPE, VOLNEW, CNONEW, OUTVER, LUNNEW, BUFOLD, TABLE,
     *   EXIST, FITASC, JERR)
      IF (EXIST .AND. (JERR.EQ.0)) GO TO 999
C                                       Get old CATBLK in BUFNEW.
      IRET = 3
      CALL CATIO ('READ', VOLOLD, CNOOLD, CATOLD, 'REST', BUFOLD, IER)
      IF ((IER.GT.0) .AND. (IER.LT.5)) THEN
         WRITE (MSGTXT,1010) IER
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Find # TYPE ext. files.
      OVO = 0
      OVN = 0
      J = 0
      CALL FXHDEX (CATOLD)
      CALL FXHDEX (CATNEW)
      DO 20 I = 1,KIEXTN
         II = KHEXT + I - 1
         CALL H2CHR (2, 1, CATOLD(II), CHTEMP)
         IF (TYPE.EQ.CHTEMP) OVO = I
         CALL H2CHR (2, 1, CATNEW(II), CHTEMP)
         IF (TYPE.EQ.CHTEMP) OVN = I
         IF ((CHTEMP.EQ.' ') .AND. (J.LE.0)) J = I
 20      CONTINUE
C                                       Old table files exist?
      IRET = 2
      IF (OVO.LE.0) GO TO 999
      LIM = CATOLD(KIVER+OVO-1)
      IF (LIM.LE.0) GO TO 999
      IF (INVER.GT.0) LIM = 1
C                                       No room to catalog new type
      IF ((J.LE.0) .AND. (OVN.LE.0)) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (6)
         IRET = 3
         GO TO 999
         END IF
C                                       Loop copying files.
      DO 100 J = 1,LIM
C                                       Open input file
         NREC = 20
         NCOL = 0
         NKEY = 0
         CALL TABINI ('READ', TYPE, VOLOLD, CNOOLD, IVERI, CATOLD,
     *      LUNOLD, NKEY, NREC, NCOL, IDATP, BUFOLD, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OLD', IVERI
            GO TO 990
            END IF
         NREC = BUFOLD(5)
         CALL COPY (256, IDATP, ODATP)
         CALL COPY (256, BUFOLD, BUFNEW)
C                                       create
         CALL TABINI ('WRIT', TYPE, VOLNEW, CNONEW, IVERO, CATNEW,
     *      LUNNEW, NKEY, NREC, NCOL, ODATP, BUFNEW, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN NEW', IVERO
            GO TO 990
            END IF
         IRET = 0
C                                       Save info from input header
C                                       Creation date, time
         CALL COPY (6, BUFOLD(11), BUFNEW(11))
C                                       Creation task
         CALL COPY (3, BUFOLD(29), BUFNEW(29))
C                                       Sort info
         BUFNEW(43) = BUFOLD(43)
         BUFNEW(44) = BUFOLD(44)
C                                       Number of keywords
         BUFNEW(53) = BUFOLD(53)
C                                       Selection strings
         CALL COPY (10, BUFOLD(61), BUFNEW(61))
C                                       Title
         CALL COPY (28, BUFOLD(101), BUFNEW(101))
C                                       Copy Keywords et al.
         OFIND = BUFNEW(82)
         IFIND = BUFOLD(82)
         NFIRST = BUFOLD(50) - 1
         DO 30 I = 2,NFIRST
            IRNO = I
            CALL ZFIO ('READ', LUNOLD, IFIND, IRNO, BUFOLD(257), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1030) IRET, 'READ', IRNO
               GO TO 990
               END IF
            CALL ZFIO ('WRIT', LUNNEW, OFIND, IRNO, BUFOLD(257), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1030) IRET, 'WRIT', IRNO
               GO TO 990
               END IF
 30         CONTINUE
         BUFOLD(77) = 5
         BUFOLD(78) = NFIRST
C                                       more set ups
         NREC = BUFOLD(5)
         OREC = 0
         DO 50 I = 1,NREC
            CALL TABIO ('READ', 0, I, SCRTCH, BUFOLD, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1030) IRET, 'READ', I
               GO TO 990
            ELSE IF (IRET.EQ.0) THEN
               OREC = OREC + 1
               CALL TABIO ('WRIT', 0, OREC, SCRTCH, BUFNEW, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1030) IRET, 'WRIT', OREC
                  GO TO 990
                  END IF
            ELSE IF ((IRET.LT.0) .AND. (DDFLAG.GT.0.0)) THEN
               OREC = OREC + 1
               CALL TABIO ('FLAG', 0, OREC, SCRTCH, BUFNEW, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1030) IRET, 'FLAG', OREC
                  GO TO 990
                  END IF
               END IF
 50         CONTINUE
         COUNT = COUNT + 1
         IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
            WRITE (MSGTXT,1050) TYPE, VOLOLD, CNOOLD, IVERI, VOLNEW,
     *         CNONEW, IVERO
            CALL MSGWRT (3)
            IF (OREC.NE.NREC) THEN
               WRITE (MSGTXT,1051) OREC, NREC
               CALL MSGWRT (3)
               END IF
            END IF
C                                       Close files
         CALL TABIO ('CLOS', 0, I, SCRTCH, BUFOLD, IRET)
         CALL TABIO ('CLOS', 0, OREC, SCRTCH, BUFNEW, IRET)
         IVERI = IVERI + 1
         IVERO = IVERO + 1
 100     CONTINUE
      IRET = 0
      CALL CATIO ('UPDT', VOLNEW, CNONEW, CATNEW, 'REST', BUFOLD, IER)
      IF (IER.NE.0) IRET = 5
      IF (COUNT.EQ.0) IRET = 4
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TBLCOP ERROR',I4,' DOING ',A,' VERSION',I4)
 1010 FORMAT ('TBLCOP: ERROR',I5,' READING OLD CATBLK')
 1020 FORMAT ('TBLCOP: NO ROOM IN NEW CATBLK FOR NEW EXTENSION TYPE')
 1030 FORMAT ('TBLCOP ERROR',I4,' ON ',A,' ROW',I5)
 1050 FORMAT ('Copied ',A2,' file from vol/cno/vers',I3,I5,I4,' to',
     *   I3,I5,I4)
 1051 FORMAT ('Copied only',I8,' unflagged records from',I8,' in input')
      END
      SUBROUTINE INITFG (FLAGV, DISKIN, CNOIN, CATIN, IRET)
C-----------------------------------------------------------------------
C   Set the DSEL common to do flagging
C   Output:
C      IRET   I   > 0 messed up
C-----------------------------------------------------------------------
      INTEGER   FLAGV, DISKIN, CNOIN, CATIN(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   IERR, KEY(2,2), KEYSUB(2,2), LUNTMP, K
      REAL      FKEY(2,2)
      DATA KEY /5,0, 1,0/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
      DOFLAG = .TRUE.
      IFLUN = LUNTMP (1)
      CALL COPY (256, CATIN, CATUV)
      KLOCSU = ILOCSU
      KLOCFQ = ILOCFQ
      KLOCIF = JLOCIF
      KLOCFY = JLOCF
      KCOR0 = ICOR0
C                                       Reformat table?
      CALL FGREFM (DISKIN, CNOIN, FLAGV, CATIN, IFLUN, IERR)
      CALL FLGINI ('READ', FGBUFF, DISKIN, CNOIN, FLAGV, CATIN,
     *   IFLUN, IFGRNO, FGKOLS, FGNUMV, IERR)
      IF (IERR.NE.0) DOFLAG = .FALSE.
C                                       Resort if necessary.
      IF ((DOFLAG) .AND. (FGBUFF(43).NE.KEY(1,1))) THEN
C                                       Sort to time order.
         CALL TABIO ('CLOS', 0, K, FGBUFF, FGBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABSRT (DISKIN, CNOIN, 'FG', FLAGV, FLAGV, KEY, KEYSUB,
     *      FKEY, FGBUFF, CATIN, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Re initialize.
         CALL FLGINI ('READ', FGBUFF, DISKIN, CNOIN, FLAGV, CATIN,
     *      IFLUN, IFGRNO, FGKOLS, FGNUMV, IERR)
         END IF
      IF (DOFLAG) THEN
         WRITE (MSGTXT,1000) FLAGV
      ELSE
         MSGTXT = 'No flag table is applied this time'
         END IF
      CALL MSGWRT (3)
      IRET = 0
      IF (.NOT.DOFLAG) IRET = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Applying flag table version',I4,' to the table data')
      END
      SUBROUTINE TYFCOP (DISKI, CNOI, DISKO, CNOO, IVER, OVER, CATIN,
     *   CATOUT, LUNI, LUNO, DDFLAG, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies a TY table with flagging
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      IVER     I        Version to read
C      OVER     I        Version to write
C      CATIN    I(256)   Input catalog header
C      CATOUT   I(256)   Output catalog header
C      LUNI     I        LUN to use
C      LUNO     I        LUN to use
C      DDFLAG   I        > 0 copy fully flagged
C   Input/Output:
C      CATOUT   I(256)   Output catalog header
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER DISKI, CNOI, DISKO, CNOO, IVER, OVER, CATIN(256),
     *   CATOUT(256), LUNI, LUNO, BUFFER(*), OBUFF(*), IRET
      REAL    DDFLAG
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ITYRNO, TYKOLS(MAXTYC), TYNUMV(MAXTYC), NUMPOL, NUMIF,
     *   OKOLS(MAXTYC), ONUMV(MAXTYC), NTYROW, I, SOURID, ANTNO,
     *   SUBA, FREQID, OTYRNO, NDEL, NTOT, JRET
      REAL      TIME, TIMEI, TSYS(2,MAXIF), TANT(2,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
C                                       open flag table
      TMFLST = -1.E20
      NUMFLG = 0
      IFGRNO = 1
      NDEL = 0
      NTOT = 0
C                                       Open TY file
      CALL TYINI ('READ', BUFFER, DISKI, CNOI, IVER, CATIN, LUNI,
     *   ITYRNO, TYKOLS, TYNUMV, NUMPOL, NUMIF, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NTYROW = BUFFER(5)
C                                       Open up new TY table
      CALL TYINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OTYRNO, OKOLS, ONUMV, NUMPOL, NUMIF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NTYROW
         CALL TABTY ('READ', BUFFER, ITYRNO, TYKOLS, TYNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, TSYS, TANT,
     *      IRET)
C                                       Error reading table
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
C                                       check subarray, time
         ELSE IF (IRET.EQ.0) THEN
            CALL TYFLG (NUMPOL, NUMIF, TIME, SOURID, ANTNO, SUBA,
     *         FREQID, TSYS, TANT, JRET)
            IF (JRET.GT.0) THEN
               IRET = JRET
               GO TO 999
               END IF
            IF (JRET.LT.0) NDEL = NDEL + 1
C                                       Write new one
            IF ((JRET.EQ.0) .OR. (DDFLAG.GT.0.0)) THEN
               NTOT = NTOT + 1
               CALL TABTY ('WRIT', OBUFF, OTYRNO, OKOLS, ONUMV, NUMPOL,
     *            NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *            TSYS, TANT, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1020) IRET
                  GO TO 990
                  END IF
               END IF
            END IF
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ITYRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OTYRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         WRITE (MSGTXT,1100) 'Copied TY', DISKI, CNOI, IVER, DISKO,
     *      CNOO, OVER
         CALL MSGWRT (3)
         WRITE (MSGTXT,1101) NDEL, NTYROW
         CALL REFRMT (MSGTXT, '_', I)
         CALL MSGWRT (3)
         IF ((DDFLAG.GT.0.0) .AND. (NDEL.GT.0)) THEN
            MSGTXT = '   Copied all records anyway'
            CALL MSGWRT (3)
            END IF
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('TYFSEL: ERROR ',I3,' RETURNED FROM TYINI')
 1020 FORMAT ('TYFSEL: ERROR ',I3,' RETURNED FROM TABTY')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
 1101 FORMAT ('__Fully deleted',I10,' of',I12,' TY records applying',
     *   ' flag table')
      END
      SUBROUTINE TYFLG (NPOL, NIF, TIME, SOURID, ANTNO, SUBA, FREQID,
     *   TSYS, TANT, IRET)
C-----------------------------------------------------------------------
C   Flags a TY table row based on the flags in FG table loaded to Common
C   Inputs:
C      NPOL     I      Number polarizations in TY data
C      NIF      I      Number of IFs in those data
C      TIME     R      Time of table row
C      SOURID   I      Source number of row
C      ANTNO    I      Antenna number of row
C      SUBA     I      Subarray of row
C      FREQID   I      Frequency ID if row
C   In/Out:
C      TSYS     R(*)   System temperature array - flagged -> FBLANK
C      TANT     R(*)   Antenna temperature array
C   Inputs from include DSEL.INC:
C      NUMFLG     I    Number of flagging entries.
C      TMFLST     R    Time of last visibility for which flagging
C                      was checked.
C      FLGSOU(*)  I    Source id numbers to flag, 0=all.
C      FLGANT(*)  I    Antenna numbers to flag, 0=all.
C      FLGBAS(*)  I    Baseline (A1*32768+A2) numbers to flag, 0=all.
C      FLGSUB(*)  I    Subarray numbers to flag, 0=all.
C      FLGFQD(*)  I    Freqid numbers to flag, <=0=all.
C                      Following should have defaults filled in.
C      FLGBIF(*)  I    First IF to flag.
C      FLGEIF(*)  I    Highest IF to flag.
C      FLGBCH(*)  I    First channel to flag.
C      FLGECH(*)  I    Highest channel to flag.
C      FLGPOL(4,*)L    Flags for the polarizations, should correspond
C                      to selected polarization types.
C   Output:
C      IRET     I      0 -> okay, -1 -> all flagged
C-----------------------------------------------------------------------
      INTEGER   NPOL, NIF, SOURID, ANTNO, SUBA, FREQID, IRET
      REAL      TIME, TSYS(2,*), TANT(2,*)
C
      INTEGER   IFLAG, FLGA, JPOLN, JIF, LIMF1, LIMF2, IPOLPT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (TMFLST.LT.TIME) CALL NXTFLG (TIME, .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NUMFLG.LE.0) GO TO 999
C                                       loop over current flags
      DO 50 IFLAG = 1,NUMFLG
C                                       Check time if needed
         IF ((TIME.LT.FLGTST(IFLAG)) .OR. (TIME.GT.FLGTND(IFLAG)))
     *      GO TO 50
C                                       Check source
         IF ((FLGSOU(IFLAG).NE.SOURID) .AND. (FLGSOU(IFLAG).NE.0) .AND.
     *      (SOURID.NE.0)) GO TO 50
C                                       Check antenna
         FLGA = FLGANT(IFLAG)
         IF ((FLGA.NE.0) .AND. (FLGA.NE.ANTNO)) GO TO 50
C                                       Check subarray
         IF ((FLGSUB(IFLAG).GT.0) .AND. (FLGSUB(IFLAG).NE.SUBA))
     *      GO TO 50
C                                       Check freqid.: may be changed
C                                       from input to 1 already
         IF ((FLGFQD(IFLAG).GT.0) .AND. (FLGFQD(IFLAG).NE.FREQID) .AND.
     *      (FREQID.GT.0)) GO TO 50
C                                       Some data to be flagged
C                                       Set limits
         LIMF1 = FLGBIF(IFLAG)
         LIMF2 = FLGEIF(IFLAG)
C                                       Loop over polarizations
         IPOLPT = ABS(KCOR0) - 1
         IF (KCOR0.LT.-4) IPOLPT = IPOLPT - 4
         DO 40 JPOLN = 1,NPOL
            IF (FLGPOL(JPOLN+IPOLPT,IFLAG)) THEN
C                                       Loop over IF
               DO 30 JIF = LIMF1,LIMF2
                  TSYS(JPOLN,JIF) = FBLANK
                  TANT(JPOLN,JIF) = FBLANK
 30               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Check if data all
      DO 70 JPOLN = 1,NPOL
         DO 60 JIF = 1,NIF
            IF ((TSYS(JPOLN,JIF).NE.FBLANK) .OR.
     *         (TANT(JPOLN,JIF).NE.FBLANK)) GO TO 999
 60         CONTINUE
 70      CONTINUE
      IRET = -1
C
 999  RETURN
      END
      SUBROUTINE SNFCOP (DISKI, CNOI, DISKO, CNOO, IVER, OVER, CATIN,
     *   CATOUT, LUNI, LUNO, DDFLAG, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies an subset of IFs in a SN table, can also modify the FQ ID
C   Applies flagging as well
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      VER      I        Version to check/modify
C      CATIN    I(256)   Input catalog header
C      CATOUT   I(256)   Output catalog header
C      LUNI     I        LUN to use
C      LUNO     I        LUN to use
C   Input/Output:
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, IVER, OVER, CATIN(256),
     *   CATOUT(256), LUNI, LUNO, BUFFER(*), OBUFF(*), IRET
      REAL      DDFLAG
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ISNRNO, SNKOLS(MAXSNC), SNNUMV(MAXSNC), NUMANT, NUMPOL,
     *   NUMIF, NUMNOD, OKOLS(MAXSNC), ONUMV(MAXSNC), NSNROW, I,
     *   SOURID, ANTNO, SUBA, FREQID, NODENO, REFA(2,MAXIF), OSNRNO,
     *   NDEL, NTOT, JRET
      LOGICAL   ISAPPL
      REAL      GMMOD, RANOD(25), DECNOD(25), TIMEI, IFR, MBDELY(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF), DISP(2), DDISP(2)
      DOUBLE PRECISION TIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
C                                       open flag table
      TMFLST = -1.E20
      NUMFLG = 0
      IFGRNO = 1
      NDEL = 0
      NTOT = 0
C                                       Open SN file
      CALL SNINI ('READ', BUFFER, DISKI, CNOI, IVER, CATIN, LUNI,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NSNROW = BUFFER(5)
C                                       Open up new SN table
      CALL SNINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OSNRNO, OKOLS, ONUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NSNROW
         CALL TABSN ('READ', BUFFER, ISNRNO, SNKOLS, SNNUMV, NUMPOL,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
C                                       check subarray and FQ
C                                       Error reading table
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
            CALL SNFLG (NUMPOL, NUMIF, TIME, SOURID, ANTNO, SUBA,
     *         FREQID, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, JRET)
            IF (JRET.GT.0) THEN
               IRET = JRET
               GO TO 999
               END IF
C                                       Select IFs
            IF (JRET.LT.0) NDEL = NDEL + 1
C                                       Write new one
            IF ((JRET.EQ.0) .OR. (DDFLAG.GT.0.0)) THEN
               NTOT = NTOT + 1
               CALL TABSN ('WRIT', OBUFF, OSNRNO, OKOLS, ONUMV, NUMPOL,
     *            TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *            MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *            WEIGHT, REFA, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1020) IRET
                  GO TO 990
                  END IF
               END IF
            END IF
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OSNRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         WRITE (MSGTXT,1100) 'Copied SN', DISKI, CNOI, IVER, DISKO,
     *      CNOO, OVER
         CALL MSGWRT (3)
         WRITE (MSGTXT,1101) NDEL, NSNROW
         CALL REFRMT (MSGTXT, '_', I)
         CALL MSGWRT (3)
         IF ((DDFLAG.GT.0.0) .AND. (NDEL.GT.0)) THEN
            MSGTXT = '   Copied all records anyway'
            CALL MSGWRT (3)
            END IF
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SNSEL: ERROR ',I3,' RETURNED FROM SNINI')
 1020 FORMAT ('SNSEL: ERROR ',I3,' RETURNED FROM TABSN')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
 1101 FORMAT ('__Fully deleted',I10,' of',I12,' SN records applying',
     *   ' flag table')
      END
      SUBROUTINE SNFLG (NPOL, NIF, TIME, SOURID, ANTNO, SUBA,
     *   FREQID, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
C-----------------------------------------------------------------------
C   Flags a SN table row based on the flags in FG table loaded to Common
C   Inputs:
C      NPOL     I        Number polarizations in TY data
C      NIF      I        Number of IFs in those data
C      TIME     D        Time of table row
C      SOURID   I        Source number of row
C      ANTNO    I        Antenna number of row
C      SUBA     I        Subarray of row
C      FREQID   I        Frequency ID if row
C   In/Out:
C      CREAL    R(2,*)   Real part of solution
C      CIMAG    R(2,*)   Imaginary part of solution
C      DELAY    R(2,*)   Delay
C      RATE     R(2,*)   Rate
C      WEIGHT   R(2,*)   Solution weight
C      REFA     I(2,*)   Reference antenna
C   Inputs from include DSEL.INC:
C      NUMFLG     I    Number of flagging entries.
C      TMFLST     R    Time of last visibility for which flagging
C                      was checked.
C      FLGSOU(*)  I    Source id numbers to flag, 0=all.
C      FLGANT(*)  I    Antenna numbers to flag, 0=all.
C      FLGBAS(*)  I    Baseline (A1*32768+A2) numbers to flag, 0=all.
C      FLGSUB(*)  I    Subarray numbers to flag, 0=all.
C      FLGFQD(*)  I    Freqid numbers to flag, <=0=all.
C                      Following should have defaults filled in.
C      FLGBIF(*)  I    First IF to flag.
C      FLGEIF(*)  I    Highest IF to flag.
C      FLGBCH(*)  I    First channel to flag.
C      FLGECH(*)  I    Highest channel to flag.
C      FLGPOL(4,*)L    Flags for the polarizations, should correspond
C                      to selected polarization types.
C   Output:
C      IRET     I      0 -> okay, -1 -> all flagged
C-----------------------------------------------------------------------
      DOUBLE PRECISION TIME
      INTEGER   NPOL, NIF, SOURID, ANTNO, SUBA, FREQID, REFA(2,*), IRET
      REAL      CREAL(2,*), CIMAG(2,*), DELAY(2,*), RATE(2,*),
     *   WEIGHT(2,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IFLAG, FLGA, JPOLN, JIF, LIMF1, LIMF2, IPOLPT
      REAL      RTIME
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      RTIME = TIME
      IF (TMFLST.LT.TIME) CALL NXTFLG (RTIME, .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NUMFLG.LE.0) GO TO 999
C                                       loop over current flags
      DO 50 IFLAG = 1,NUMFLG
C                                       Check time if needed
         IF ((TIME.LT.FLGTST(IFLAG)) .OR. (TIME.GT.FLGTND(IFLAG)))
     *      GO TO 50
C                                       Check source
         IF ((FLGSOU(IFLAG).NE.SOURID) .AND. (FLGSOU(IFLAG).NE.0) .AND.
     *      (SOURID.NE.0)) GO TO 50
C                                       Check antenna
         FLGA = FLGANT(IFLAG)
         IF ((FLGA.NE.0) .AND. (FLGA.NE.ANTNO)) GO TO 50
C                                       Check subarray
         IF ((FLGSUB(IFLAG).GT.0) .AND. (FLGSUB(IFLAG).NE.SUBA))
     *      GO TO 50
C                                       Check freqid.: may be changed
C                                       from input to 1 already
         IF ((FLGFQD(IFLAG).GT.0) .AND. (FLGFQD(IFLAG).NE.FREQID) .AND.
     *      (FREQID.GT.0)) GO TO 50
C                                       Some data to be flagged
C                                       Set limits
         LIMF1 = FLGBIF(IFLAG)
         LIMF2 = FLGEIF(IFLAG)
C                                       Loop over polarizations
         IPOLPT = ABS(KCOR0) - 1
         IF (KCOR0.LT.-4) IPOLPT = IPOLPT - 4
         DO 40 JPOLN = 1,NPOL
            IF (FLGPOL(JPOLN+IPOLPT,IFLAG)) THEN
C                                       Loop over IF
               DO 30 JIF = LIMF1,LIMF2
                  CREAL(JPOLN,JIF) = FBLANK
                  CIMAG(JPOLN,JIF) = FBLANK
                  DELAY(JPOLN,JIF) = FBLANK
                  RATE(JPOLN,JIF) = FBLANK
                  WEIGHT(JPOLN,JIF) = 0.0
                  REFA(JPOLN,JIF) = 0
 30               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Check if data all gone
      DO 70 JPOLN = 1,NPOL
         DO 60 JIF = 1,NIF
            IF ((CREAL(JPOLN,JIF).NE.FBLANK) .AND.
     *         (CIMAG(JPOLN,JIF).NE.FBLANK)) GO TO 999
 60         CONTINUE
 70      CONTINUE
      IRET = -1
C
 999  RETURN
      END
      SUBROUTINE SYFCOP (DISKI, CNOI, DISKO, CNOO, IVER, OVER, CATIN,
     *   CATOUT, LUNI, LUNO, DDFLAG, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies a subset of IFs in a SY table, can also modify the FQ ID
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      VER      I        Version to check/modify
C      CATIN    I(256)   Input catalog header
C      CATOUT   I(256)   Output catalog header
C      LUNI     I        LUN to use
C      LUNO     I        LUN to use
C   Input/Output:
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, IVER, OVER, CATIN(256),
     *   CATOUT(256), LUNI, LUNO, BUFFER(*), OBUFF(*), IRET
      REAL    DDFLAG
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXSYC
      PARAMETER (MAXSYC=12)
C
      INTEGER   ISYRNO, SYKOLS(MAXSYC), SYNUMV(MAXSYC), NUMANT, NUMPOL,
     *   NUMIF, OKOLS(MAXSYC), ONUMV(MAXSYC), NSYROW, I, SOURID, ANTNO,
     *   SUBA, FREQID, OSYRNO, NDEL, NTOT, JRET, NPART
      REAL      PDIFF(2,MAXIF), PSUM(2,MAXIF), PGAIN(2,MAXIF), TIMEI
      DOUBLE PRECISION TIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
C                                       open flag table
      TMFLST = -1.E20
      NUMFLG = 0
      IFGRNO = 1
      NDEL = 0
      NTOT = 0
      NPART = 0
C                                       Open SY file
      CALL SYINI ('READ', BUFFER, DISKI, CNOI, IVER, CATIN, LUNI,
     *   ISYRNO, SYKOLS, SYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NSYROW = BUFFER(5)
C                                       Open up new SY table
      CALL SYINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OSYRNO, OKOLS, ONUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NSYROW
         CALL TABSY ('READ', BUFFER, ISYRNO, SYKOLS, SYNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, PDIFF,
     *      PSUM, PGAIN, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
C                                       flag info
         ELSE IF (IRET.EQ.0) THEN
            CALL SYFLG (NUMPOL, NUMIF, TIME, SOURID, ANTNO, SUBA,
     *         FREQID, PDIFF, PSUM, PGAIN, NPART, JRET)
            IF (JRET.GT.0) THEN
               IRET = JRET
               GO TO 999
               END IF
            IF (JRET.LT.0) NDEL = NDEL + 1
C                                       Write new one
            IF ((JRET.EQ.0) .OR. (DDFLAG.GT.0.0)) THEN
               NTOT = NTOT + 1
               CALL TABSY ('WRIT', OBUFF, OSYRNO, OKOLS, ONUMV, NUMPOL,
     *            NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *            PDIFF, PSUM, PGAIN, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1040) IRET
                  GO TO 990
                  END IF
               END IF
            END IF
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ISYRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OSYRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         WRITE (MSGTXT,1100) 'Copied SY', DISKI, CNOI, IVER, DISKO,
     *      CNOO, OVER
         CALL MSGWRT (3)
         WRITE (MSGTXT,1101) NDEL, NSYROW
         CALL REFRMT (MSGTXT, '_', I)
         CALL MSGWRT (3)
         IF ((DDFLAG.GT.0.0) .AND. (NDEL.GT.0)) THEN
            MSGTXT = '   Copied all records anyway'
            CALL MSGWRT (3)
            END IF
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SYFSEL: ERROR ',I3,' INITING OLD TABLE')
 1020 FORMAT ('SYFSEL: ERROR ',I3,' INITING NEW TABLE')
 1030 FORMAT ('SYFSEL: ERROR ',I3,' READING OLD TABLE')
 1040 FORMAT ('SYFSEL: ERROR ',I3,' WRITING NEW TABLE')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
 1101 FORMAT ('__Fully deleted',I10,' of',I12,' SY records applying',
     *   ' flag table')
      END
      SUBROUTINE SYFLG (NPOL, NIF, TIME, SOURID, ANTNO, SUBA,
     *   FREQID, PDIFF, PSUM, PGAIN, NPART, IRET)
C-----------------------------------------------------------------------
C   Flags a TY table row based on the flags in FG table loaded to Common
C   Inputs:
C      NPOL     I      Number polarizations in TY data
C      NIF      I      Number of IFs in those data
C      TIME     R      Time of table row
C      SOURID   I      Source number of row
C      ANTNO    I      Antenna number of row
C      SUBA     I      Subarray of row
C      FREQID   I      Frequency ID if row
C   In/Out:
C      PDIFF    R(*)   Pon-Poff
C      PSUM     R(*)   Pon+Poff
C      PGAIN    R(*)   Post detection gains
C      NPART    I      count of partly flagged records
C   Inputs from include DSEL.INC:
C      NUMFLG     I    Number of flagging entries.
C      TMFLST     R    Time of last visibility for which flagging
C                      was checked.
C      FLGSOU(*)  I    Source id numbers to flag, 0=all.
C      FLGANT(*)  I    Antenna numbers to flag, 0=all.
C      FLGBAS(*)  I    Baseline (A1*32768+A2) numbers to flag, 0=all.
C      FLGSUB(*)  I    Subarray numbers to flag, 0=all.
C      FLGFQD(*)  I    Freqid numbers to flag, <=0=all.
C                      Following should have defaults filled in.
C      FLGBIF(*)  I    First IF to flag.
C      FLGEIF(*)  I    Highest IF to flag.
C      FLGBCH(*)  I    First channel to flag.
C      FLGECH(*)  I    Highest channel to flag.
C      FLGPOL(4,*)L    Flags for the polarizations, should correspond
C                      to selected polarization types.
C   Output:
C      IRET     I      0 -> okay, -1 -> all flagged
C-----------------------------------------------------------------------
      INTEGER   NPOL, NIF, SOURID, ANTNO, SUBA, FREQID, NPART, IRET
      REAL      PDIFF(2,*), PSUM(2,*), PGAIN(2,*)
      DOUBLE PRECISION TIME
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IFLAG, FLGA, JPOLN, JIF, LIMF1, LIMF2, IPOLPT
      REAL      RTIME
      LOGICAL   PART
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      RTIME = TIME
      IF (TMFLST.LT.TIME) CALL NXTFLG (RTIME, .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NUMFLG.LE.0) GO TO 999
      PART = .FALSE.
C                                       loop over current flags
      DO 50 IFLAG = 1,NUMFLG
C                                       Check time if needed
         IF ((TIME.LT.FLGTST(IFLAG)) .OR. (TIME.GT.FLGTND(IFLAG)))
     *      GO TO 50
C                                       Check source
         IF ((FLGSOU(IFLAG).NE.SOURID) .AND. (FLGSOU(IFLAG).NE.0) .AND.
     *      (SOURID.NE.0)) GO TO 50
C                                       Check antenna
         FLGA = FLGANT(IFLAG)
         IF ((FLGA.NE.0) .AND. (FLGA.NE.ANTNO)) GO TO 50
C                                       Check subarray
         IF ((FLGSUB(IFLAG).GT.0) .AND. (FLGSUB(IFLAG).NE.SUBA))
     *      GO TO 50
C                                       Check freqid.: may be changed
C                                       from input to 1 already
         IF ((FLGFQD(IFLAG).GT.0) .AND. (FLGFQD(IFLAG).NE.FREQID) .AND.
     *      (FREQID.GT.0)) GO TO 50
C                                       Some data to be flagged
C                                       Set limits
         LIMF1 = FLGBIF(IFLAG)
         LIMF2 = FLGEIF(IFLAG)
C                                       Loop over polarizations
         IPOLPT = ABS(KCOR0) - 1
         IF (KCOR0.LT.-4) IPOLPT = IPOLPT - 4
         DO 40 JPOLN = 1,NPOL
            IF (FLGPOL(JPOLN+IPOLPT,IFLAG)) THEN
               PART = .TRUE.
C                                       Loop over IF
               DO 30 JIF = LIMF1,LIMF2
                  PDIFF(JPOLN,JIF) = FBLANK
                  PSUM(JPOLN,JIF) = FBLANK
                  PGAIN(JPOLN,JIF) = FBLANK
 30               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Check if data all flagged
      IF (PART) NPART = NPART + 1
      DO 70 JPOLN = 1,NPOL
         DO 60 JIF = 1,NIF
            IF ((PDIFF(JPOLN,JIF).NE.FBLANK) .OR.
     *         (PSUM(JPOLN,JIF).NE.FBLANK)) GO TO 999
 60         CONTINUE
 70      CONTINUE
      IF (PART) NPART = NPART - 1
      IRET = -1
C
 999  RETURN
      END
