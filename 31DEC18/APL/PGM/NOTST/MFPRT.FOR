LOCAL INCLUDE 'MFPRT.INC'
      REAL      XSEQ, XDISK, XBCNT, XECNT, XINC, DOCRT, FLUX, CSIZE(2),
     *   DOHMS, FRMT, DOALL, CLEV, SYMBOL, UCOLS(40)
      HOLLERITH XINNAM(3), XINCLS(2), XLPNAM(12), XSORT, XOPTYP
      CHARACTER INNAM*12, INCLS*6, INEXT*2, LPNAME*48, SORT*2, OPTYPE*4
      REAL      ASCALE, SMN(128), SMX(128)
      INTEGER   BCOUNT, ECOUNT, ICOUNT, DATP(128,2), BUFFER(512),
     *   INSEQ, INDISK, INVERS, CNO, IUSER, NKEY, NCOL, OUTLUN, OUTIND,
     *   BROW, EROW, IROW, NLINE, ISCALE, IPAGE, NACROS, IDOALL
      LOGICAL   MULTIC
      COMMON /INPARM/ XINNAM, XINCLS, XSEQ, XDISK, XBCNT, XECNT, XINC,
     *   DOCRT, XLPNAM, FLUX, CSIZE, XSORT, XOPTYP, DOHMS, FRMT, DOALL,
     *   CLEV, SYMBOL, UCOLS
      COMMON /CHPARM/ INNAM, INCLS, INEXT, LPNAME, SORT, OPTYPE
      COMMON /MFPRTC/ BCOUNT, ECOUNT, ICOUNT, DATP, BUFFER, INSEQ,
     *   INDISK, INVERS, CNO, IUSER, NKEY, NCOL, OUTLUN, OUTIND, BROW,
     *   EROW, IROW, NLINE, ASCALE, ISCALE, IPAGE, SMN, SMX, MULTIC,
     *   NACROS, IDOALL
LOCAL END
      PROGRAM MFPRT
C-----------------------------------------------------------------------
C! Task to print contents of table extension files.
C# Calibration EXT-appl EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999, 2002, 2004-2005, 2007-2011, 2014-2016
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
C   MFPRT is an AIPS task to print the contents of generalized, tables
C   format extension files on the line printer or the CRT terminal.
C   AIPS adverbs:                     Use:
C     INNAME     INNAM(3)     Image name: standard defaults, wildcards
C     INCLASS    INCLS(2)     Image class: ditto
C     INSEQ      XSEQ (INSEQ) Image sequence number: ditto
C     INDISK     XDISK (INDISK)  Image disk number: 0 -> any
C     INTYPE     INTYP           Image type: '  ' => any
C     INEXT      INEXT           Extension type: '  ' => 'TA'
C     INVERS     XVER (INVERS)   Extension version number
C     BCOUNT     XBCNT (BCOUNT)  First row number
C     ECOUNT     XECNT (ECOUNT)  Last row number
C     XINC       XINC (ICOUNT)   Increment between printed rows
C     DOCRT      DOCRT           > 0. => CRT, else line printer
C     OUTPRINT   LPNAME          File to save printer output in
C-----------------------------------------------------------------------
      INTEGER   IRET, I, NCOUNT, TTY(2)
      CHARACTER SCRTCH*132, LLCH*4
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'MFPRT.INC'
C-----------------------------------------------------------------------
C                                       Get parms, open things
      CALL PRTBIN (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       do printing
      IF ((DOCRT.LE.0.0) .AND. (LPNAME.EQ.' ')) THEN
         NCOUNT = 0
         NACROS = 132
         DO 10 I = BCOUNT,ECOUNT,ICOUNT
C                                       source list
            IF ((OPTYPE.EQ.'LIST') .OR. (OPTYPE.EQ.'LIDE')) THEN
               CALL CHKLST (I, NCOUNT, IRET)
C                                       stars
            ELSE IF ((OPTYPE.EQ.'STAR') .OR. (OPTYPE.EQ.'STDE')) THEN
               CALL CHKSTR (I, NCOUNT, IRET)
C                                       user specified
            ELSE IF (OPTYPE.EQ.'USER') THEN
               CALL CHKUSR (I, NCOUNT, IRET)
C                                       line general
            ELSE IF (OPTYPE.EQ.'LINE') THEN
               CALL CHKMC1 (I, NCOUNT, IRET)
C                                       continuum general
            ELSE IF (OPTYPE.EQ.'CONT') THEN
               CALL CHK1C1 (I, NCOUNT, IRET)
C                                       line general
            ELSE IF (MULTIC) THEN
               CALL CHKMC1 (I, NCOUNT, IRET)
C                                       continuum general
            ELSE
               CALL CHK1C1 (I, NCOUNT, IRET)
               END IF
            IF (IRET.GT.0) GO TO 990
 10         CONTINUE
         IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
            IF (NCOUNT.GT.1000) IRET = -1
         ELSE IF (NCOUNT.GT.500) THEN
            TTY(1) = 5
            CALL ZOPEN (TTY(1), TTY(2), 1, SCRTCH, .FALSE., .FALSE.,
     *         .TRUE., IRET)
            MSGTXT = 'PROBLEM OPENING TERMINAL'
            IF (IRET.GT.0) GO TO 980
            WRITE (SCRTCH,1050) NCOUNT
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, IRET)
            MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
            IF (IRET.GT.0) GO TO 980
            SCRTCH = 'Do you really want to print this much??' //
     *         ' Enter Y or y if so'
            CALL INQSTR (TTY, SCRTCH, 1, LLCH, IRET)
            IF (IRET.GT.0) GO TO 980
            IF ((LLCH(:1).NE.'y') .AND. (LLCH(:1).NE.'Y')) THEN
               SCRTCH = 'Good choice - save trees'
               IRET = -1
            ELSE
               SCRTCH = 'OKAY, printing anyway'
               END IF
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, I)
            CALL ZCLOSE (TTY(1), TTY(2), I)
            END IF
         IF (IRET.NE.0) THEN
            IRET = MAX (0, IRET)
            GO TO 990
            END IF
         END IF
C                                       Open output device
      CALL LPOPEN (LPNAME, DOCRT, OUTLUN, OUTIND, NACROS, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 980
         END IF
C                                       do printing
      IPAGE = 0
      DO 100 I = BCOUNT,ECOUNT,ICOUNT
C                                       source list
         IF ((OPTYPE.EQ.'LIST') .OR. (OPTYPE.EQ.'LIDE')) THEN
            CALL MFPLST (I, IRET)
C                                       stars
         ELSE IF ((OPTYPE.EQ.'STAR') .OR. (OPTYPE.EQ.'STDE')) THEN
            CALL MFPSTR (I, IRET)
C                                       user specified
         ELSE IF (OPTYPE.EQ.'USER') THEN
            CALL MFPUSR (I, IRET)
C                                       line general
         ELSE IF (OPTYPE.EQ.'LINE') THEN
            CALL MFPMC1 (I, IRET)
C                                       continuum general
         ELSE IF (OPTYPE.EQ.'CONT') THEN
            CALL MFP1C1 (I, IRET)
C                                       line general
         ELSE IF (MULTIC) THEN
            CALL MFPMC1 (I, IRET)
C                                       continuum general
         ELSE
            CALL MFP1C1 (I, IRET)
            END IF
         IF (IRET.NE.0) THEN
            IRET = MAX (0, IRET)
            GO TO 110
            END IF
 100     CONTINUE
C
 110  CALL LPCLOS (OUTLUN, OUTIND, NLINE, I)
      GO TO 990
C
 980  CALL MSGWRT (8)
C                                       close down
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
C-----------------------------------------------------------------------
 1040 FORMAT ('ERROR',I5,' OPENING OUTPUT DEVICE')
 1050 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE PRTBIN (IRET)
C-----------------------------------------------------------------------
C   PRTBIN performs initialization for AIPS task MFPRT.  It gets the
C   adverbs, opens the catalog file for 'READ', opens the table
C   extension
C   file, and opens the output device.
C   Output: IRET    I      Error code: 0 => keep going, else quit.
C-----------------------------------------------------------------------
      CHARACTER INTYP*2, STAT*4, PRGN*6, TCOL*9
      INTEGER   IRET, NPARM, IROUND, NBUF, IERR, NREC, TABLUN, IK,
     *   KEY(2,2), I, NUMMFT, IFRQ, KEYSUB(2,2), DEPTH(5)
      REAL      FKEY(2,2), IKS
      LOGICAL   DONMSG
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'MFPRT.INC'
      DATA INTYP /'  '/
      DATA PRGN /'MFPRT '/
      DATA TCOL /'CFTXYJNP-'/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
C                                       AIPS init
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
      DONMSG = .FALSE.
C                                       get adverbs
      NPARM = 73
      IRET = 0
      CALL GTPARM (PRGN, NPARM, RQUICK, XINNAM, DATP, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       restart AIPS
 10   IF ((IRET.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      IF (DOCRT.GT.0.0) RQUICK = .FALSE.
      IF (RQUICK) RQUICK = LPNAME.NE.' '
      IF (RQUICK) CALL RELPOP (IRET, DATP, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XINNAM, INNAM)
      CALL H2CHR (6, 1, XINCLS, INCLS)
      CALL H2CHR (2, 1, XSORT, SORT)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      IKS = 1.0
      IK = INDEX (TCOL, SORT(1:1))
      IF (IK.EQ.LEN(TCOL)) THEN
         IKS = -1.0
         IK = INDEX (TCOL, SORT(2:2))
         END IF
      IF ((IK.LE.0) .OR. (IK.GE.LEN(TCOL))) THEN
         IK = 2
         IKS = -1.0
         SORT = '-F'
         END IF
      KEY(1,1) = IK
      KEY(2,1) = IK
      KEY(1,2) = 2
      KEY(2,2) = 2
      FKEY(1,1) = IKS
      FKEY(2,1) = IKS
      FKEY(1,2) = -1.
      FKEY(2,2) = -1.
      IK = IROUND (IK*IKS)
      INEXT = 'MF'
      IDOALL = DOALL + 0.1
      IDOALL = MAX (0, MIN (3, IDOALL))
C                                       find image file
      INSEQ = IROUND (XSEQ)
      INDISK = IROUND (XDISK)
      INVERS = 0
      ICOUNT = IROUND (XINC)
      BCOUNT = IROUND (XBCNT)
      ECOUNT = IROUND (XECNT)
      IF (ICOUNT.LE.0) ICOUNT = 1
      IF (BCOUNT.LE.0) BCOUNT = 1
      IUSER = NLUSER
      CNO = 1
      CALL CATDIR ('SRCH', INDISK, CNO, INNAM, INCLS, INSEQ, INTYP,
     *   IUSER, STAT, DATP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, INNAM, INCLS, INSEQ, INTYP,
     *      INDISK, IUSER
         GO TO 990
         END IF
C                                       Get catblk, mark file read
      CALL CATIO ('READ', INDISK, CNO, CATBLK, 'READ', DATP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      NCFILE = 1
      FVOL(1) = INDISK
      FCNO(1) = CNO
      FRW(1) = 0
      NBUF = 16384 * 2
C                                       coordinates
      CALL FILL (5, 1, DEPTH)
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
      IF (AXTYP(LOCNUM).NE.1) DOHMS = -1.0
C                                       multi-channel?
      CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATBLK(KHCTP), IFRQ, IERR)
      MULTIC = (IERR.EQ.0) .AND. (CATBLK(KINAX+IFRQ).GT.1)
C                                       Set scale
      ASCALE = MAX (ABS (CATR(KRCIC)), ABS (CATR(KRCIC+1)))
      IF (ASCALE.LE.0.05/3600.) THEN
         ISCALE = 1
         ASCALE = 3.6E6
      ELSE IF (ASCALE.LE.0.05) THEN
         ISCALE = 2
         ASCALE = 3.6E3
      ELSE
         ISCALE = 3
         ASCALE = 1.0
         END IF
      CSIZE(1) = CSIZE(1) / 3600.0 * ASCALE
      CSIZE(2) = CSIZE(2) / 3600.0 * ASCALE
C                                       Sort into order
      CALL FNDEXT ('MF', CATBLK, NUMMFT)
      ECOUNT = MIN (ECOUNT,NUMMFT)
      IF (ECOUNT.EQ.0) ECOUNT = NUMMFT
C
      DO 35 I = BCOUNT,ECOUNT,ICOUNT
         INVERS = I
C                                       Is it necessary?
         NKEY = 0
         NCOL = 0
         NREC = 0
         TABLUN = 27
         CALL TABINI ('READ', INEXT, INDISK, CNO, INVERS, CATBLK,
     *      TABLUN, NKEY, NREC, NCOL, DATP, BUFFER, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1030) IERR, INEXT, INVERS
            IF (IERR.NE.2) GO TO 990
            GO TO 35
            END IF
C                                       defend against old format
         IF (NCOL.LT.41) IDOALL = 0
         CALL TABIO ('CLOS', 3, 0, BUFFER, BUFFER, IERR)
         IF ((BUFFER(43).NE.IK) .OR. (BUFFER(44).NE.-2)) THEN
            IF (.NOT.DONMSG) THEN
               MSGTXT = 'Sorting files into ' // SORT // ' order'
               CALL MSGWRT (2)
               DONMSG = .TRUE.
               END IF
            CALL TABSRT (INDISK, CNO, INEXT, INVERS, INVERS, KEY,
     *         KEYSUB, FKEY, BUFFER, CATBLK, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1050) IERR, I
               GO TO 990
               END IF
            END IF
 35      CONTINUE
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' FINDING INPUT ADVERBS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   'DISK=',I2,' USER=',I5)
 1020 FORMAT ('ERROR',I5,' READING CATBLK FROM CATALOG FILE')
 1030 FORMAT ('ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1050 FORMAT ('ERROR',I5,' SORTING TABLE # ',I3)
      END
      SUBROUTINE MFPMC1 (INVERX, IRET)
C-----------------------------------------------------------------------
C   MFPMC1 reads, formats, and prints a table extension file
C   Input:
C      INVERX I   MF version number currently being printed
C   Output:
C      IRET   I   Error code: 0 => OK or user terminates,
C                    2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   INVERX, IRET
C
      INTEGER   MAXKEY
C                                       MAXKEY=max. no. keyword-values
      PARAMETER (MAXKEY=7)
      INCLUDE 'INCS:PMFC.INC'
      CHARACTER TITL1*132, TITL2*132, LINE*132, MSGBUF*72, SCRTCH*132,
     *   COLNUM*132, KEYWRD(MAXKEY)*8, CUNIT(3)*8, CHSIGN*1, CTEMP*8,
     *   BUNIT*8
      INTEGER   SCRBUF(NUMCOL), IRNO, RESLI(1), JCOL1, JCOL2, I, J, K,
     *   L, M, N, NCH(128), JJ, ICH(128), RESULT(NUMCOL), BITS(64),
     *   RTYPE, IERR, NUMKEY, IEL, LRNO, LOCS(MAXKEY), KEYTYP(MAXKEY),
     *   LENGTH, MAXLEN, HMS(2), JTRIM, MCOL, KTY(128), ALTAX,
     *   DEPTH(5), GCH(128),NREC, TABLUN, MCH(128), COLIDX(11), CLEN(3)
      LOGICAL   RESLO(1), OPTICL, RADIO, CHKSIZ, FIRST, OKLIN
      REAL      RES4(1), SEC, OFFSET, ALTRFP, NUX, VELINC, RCHAN, BMAJ,
     *   BMIN, SCALE(10), DX, DY, DLFACT, PBFACT, DELNU
      HOLLERITH RESH(1)
      DOUBLE PRECISION    RES8(1), REFVEL, VELPIX, RA, DEC, XX, YY, ZZ,
     *   AXDEN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'MFPRT.INC'
      EQUIVALENCE (RESULT, RES8, RES4, RESH, RESLI, RESLO)
      DATA CUNIT /'mas', 'asec', 'deg'/
      DATA CLEN /15, 12, 10/
C                                       Column in table corresponding
C                                       to column in output
      DATA COLIDX /COLPLN, COLPEK, COLDLX, COLDLY, COLMJX, COLMNX,
     *   COLPAN, COLD0J, COLD0N, COLD0P, COLPLN/
C-----------------------------------------------------------------------
      IRET = 0
      FIRST = .TRUE.
      OKLIN = FRMT.GT.-0.5
C                                       Open table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      TABLUN = 27
      CALL TABINI ('READ', INEXT, INDISK, CNO, INVERX, CATBLK, TABLUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1010) IERR, INEXT, INVERX
         IF (IERR.NE.2) GO TO 995
         GO TO 999
         END IF
      BROW = 1
      EROW = BUFFER(5)
      IF (EROW.LT.BROW) GO TO 200
      IROW = 1
C                                       init counters, line size
      JCOL2 = 0
      CHKSIZ = CSIZE(2).GT.0.0
      CALL PRTSCL (KTY, ICH, MCH, RESULT, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      CALL COPY (128, MCH, NCH)
      CALL RFILL (11, 1.0, SCALE)
      SCALE(3) = ASCALE
      SCALE(4) = ASCALE
      SCALE(5) = ASCALE
      SCALE(6) = ASCALE
      SCALE(8) = ASCALE
      SCALE(9) = ASCALE
      IF (MAX(ABS(SMX(COLPEK)), ABS(SMN(COLPEK))).LT.1.0)
     *   SCALE(2) = 1000.0
      IF (MAX(ABS(SMX(COLPEK)), ABS(SMN(COLPEK))).LT.0.001)
     *   SCALE(2) = 1.0E6
      IF (DOHMS.GT.0.0) THEN
         NCH(COLIDX(3)) = CLEN(ISCALE)
         NCH(COLIDX(4)) = CLEN(ISCALE)
         ICH(COLIDX(3)) = 1
         ICH(COLIDX(4)) = 1
      ELSE IF (AXTYP(LOCNUM).EQ.1) THEN
         NCH(COLIDX(3)) = 14
         NCH(COLIDX(4)) = 14
         ICH(COLIDX(3)) = 1
         ICH(COLIDX(4)) = 1
         END IF
      ALTAX = CATBLK(KIALT)
      MCOL = 11
      IF (ALTAX.LE.0) MCOL = 10
C                                       Loop: list all columns to fit
 10   JCOL1 = JCOL2 + 1
      MAXLEN = 1
C                                       how many columns this pass
      N = 0
      IF (OKLIN) N = 8
      DO 15 J = JCOL1,MCOL
         I = MOD (DATP(COLIDX(J),2), 10)
         LENGTH = DATP(COLIDX(J),2) / 10
         NCH(COLIDX(J)) = MIN (NCH(COLIDX(J)), NACROS-9)
         N = N + NCH(COLIDX(J)) + 2
         IF (N.LE.NACROS) JCOL2 = J
         IF (N.GT.NACROS) GO TO 20
         IF ((I.NE.3) .AND. (I.NE.7)) MAXLEN = MAX (MAXLEN, LENGTH)
 15      CONTINUE
C                                       Blanks between
 20   IF (JCOL2.LT.JCOL1) JCOL2 = JCOL1
      N = 0
      IF (OKLIN) N = 8
      DO 25 J = JCOL1,JCOL2
         N = N + NCH(COLIDX(J))
 25      CONTINUE
      N = (NACROS - N) / (JCOL2 - JCOL1 + 1)
      N = MIN (N, 6)
      GCH(JCOL1) = 1
      IF (OKLIN) GCH(JCOL1) = 9
      IF (JCOL1.LT.JCOL2) THEN
         I = JCOL1 + 1
         DO 30 J = I,JCOL2
            GCH(J) = GCH(J-1) + NCH(COLIDX(J-1)) + N
 30         CONTINUE
         END IF
C                                         First page
      NLINE = 990
C                                       Keyword/value pairs
      NUMKEY = 5
      KEYWRD(1) = 'DEPTH1'
      KEYWRD(2) = 'DEPTH2'
      KEYWRD(3) = 'DEPTH3'
      KEYWRD(4) = 'DEPTH4'
      KEYWRD(5) = 'DEPTH5'
      CALL TABKEY ('READ', KEYWRD, NUMKEY, BUFFER, LOCS, DEPTH,
     *   KEYTYP, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LE.20)) GO TO 90
C                                       Velocity calculation
      IF (IERR.NE.0) CALL FILL (5, 1, DEPTH)
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
      ALTAX = CATBLK(KIALT)
      IF (ALTAX.LE.0) THEN
         MSGTXT = 'VELOCITY INFO MISSING - OMITTED'
         CALL MSGWRT (6)
         VELINC = 0.0
         ALTAX = -1
         MCOL = 10
      ELSE
         MCOL = 11
         OPTICL = .TRUE.
         IF (ALTAX.GT.256) THEN
            RADIO = .TRUE.
            OPTICL = .FALSE.
            ALTAX = ALTAX - 256
            END IF
         CALL H2CHR (8, 1, CATH(KHCTP+2*KLOCF(LOCNUM)), CTEMP)
         IF (CTEMP(:4).EQ.'FREQ') THEN
            ALTRFP = CATR(KRARP)
            REFVEL = CATD(KDARV)
            DELNU = CATR(KRCIC+KLOCF(LOCNUM))
            NUX = CATD(KDCRV+KLOCF(LOCNUM)) + DELNU *
     *         (ALTRFP - CATR(KRCRP+KLOCF(LOCNUM)))
            IF (OPTICL) THEN
               VELINC = -(DELNU * (VELITE + REFVEL)) / NUX
               AXDEN = -VELINC / (VELITE + REFVEL)
            ELSE
               VELINC = -(DELNU * (VELITE - REFVEL)) / NUX
               END IF
C                                       axis ALTSW'ed
         ELSE
            AXDEN = AXDENU(LOCNUM)
            REFVEL = CATD(KDCRV+KLOCF(LOCNUM))
            VELINC = CATR(KRCIC+KLOCF(LOCNUM))
            ALTRFP = CATR(KRCRP+KLOCF(LOCNUM))
            END IF
         END IF
C                                       List column numbers
 90   TITL1 = ' '
      TITL2 = ' '
      COLNUM = ' '
      LINE = ' '
      IF (OKLIN) TITL1 = ' Row #'
C                                       Column label
      DO 95 J = JCOL1,JCOL2
         JJ = COLIDX(J)
         SCRTCH = ' '
         IF (J.EQ.1) SCRTCH = 'Channel '
         IF (J.EQ.11) SCRTCH = 'Velocity '
         IF (J.EQ.2) SCRTCH = 'Peak int'
         IF (J.EQ.3) THEN
            SCRTCH = 'X-offset'
            IF (AXTYP(LOCNUM).EQ.1) THEN
               IF (DOHMS.GT.0.0) THEN
                  SCRTCH = 'Right Ascension'
               ELSE
                  SCRTCH = 'RA offset'
                  END IF
               END IF
            END IF
         IF (J.EQ.4) THEN
            SCRTCH = 'Y-offset'
            IF (AXTYP(LOCNUM).EQ.1) THEN
               IF (DOHMS.GT.0.0) THEN
                  SCRTCH = 'Declination'
               ELSE
                  SCRTCH = 'DEC offset'
                  END IF
               END IF
            END IF
         IF (J.EQ.5) SCRTCH = 'Maj axis'
         IF (J.EQ.6) SCRTCH = 'Min axis'
         IF (J.EQ.7) SCRTCH = 'Pos angle'
         IF (J.EQ.8) SCRTCH = 'Decon maj'
         IF (J.EQ.9) SCRTCH = 'Decon min'
         IF (J.EQ.10) SCRTCH = 'Decon PA'
         M = JTRIM (SCRTCH(:NCH(JJ)))
         IF ((M.GT.0) .AND. (J.LE.MCOL)) TITL1(GCH(J)+NCH(JJ)-M:) =
     *      SCRTCH(:M)
 95      CONTINUE
      DO 100 J = JCOL1,JCOL2
         JJ = COLIDX(J)
         SCRTCH = ' '
         IF (J.EQ.11) THEN
            SCRTCH = 'km/s'
         ELSE IF (J.EQ.2) THEN
            CALL H2CHR (8, 1, CATH(KHBUN), SCRTCH)
            BUNIT = SCRTCH(:8)
            CALL CHLTOU (8, BUNIT)
            IF (BUNIT.EQ.'JY/BEAM') SCRTCH = 'Jy/beam'
            IF (SCALE(2).NE.1.0) THEN
               CTEMP = SCRTCH(:8)
               IF (SCALE(2).LT.1.E4) THEN
                  SCRTCH = 'm' // CTEMP(:7)
               ELSE
                  SCRTCH = 'u' // CTEMP(:7)
                  END IF
               END IF
         ELSE IF ((DOHMS.GT.0.0) .AND. ((J.EQ.3) .OR. (J.EQ.4))) THEN
            SCRTCH = ' '
         ELSE IF ((DOHMS.LE.-1.5) .AND. ((J.EQ.3) .OR. (J.EQ.4))) THEN
            SCRTCH = 'arc sec'
         ELSE IF ((J.EQ.3) .OR. (J.EQ.4)) THEN
            SCRTCH = 'degrees'
         ELSE IF ((J.GE.3) .AND. (J.LE.6)) THEN
            SCRTCH = CUNIT(ISCALE)
         ELSE IF ((J.GE.8) .AND. (J.LE.9)) THEN
            SCRTCH = CUNIT(ISCALE)
         ELSE IF ((J.EQ.7) .OR. (J.EQ.10)) THEN
            SCRTCH = 'degrees'
            END IF
         M = JTRIM (SCRTCH(:8))
         IF ((M.GT.0) .AND. (J.LE.MCOL)) TITL2(GCH(J)+NCH(JJ)-M:) =
     *      SCRTCH(:M)
 100     CONTINUE
C                                       Output the lines
      IF (FIRST) THEN
         FIRST = .FALSE.
         WRITE (LINE,1099) INVERX, INNAM, INCLS, INSEQ
         CALL REFRMT (LINE, '_', JJ)
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
      IF (DOCRT.LE.-2.5) THEN
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      TITL1, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      TITL1, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
      IF ((DOCRT.LE.0.0) .AND. (DOCRT.GT.-2.5)) THEN
         LINE = ' '
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Output loop
      LRNO = 0
      DLFACT = 1.0
      PBFACT = 1.0
      DO 150 IRNO = BROW,EROW,IROW
         IF ((IDOALL.EQ.1) .OR. (IDOALL.EQ.3)) THEN
            JJ = COBMFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            PBFACT = RES4(1)
            END IF
C                                       Check flux level
         IF (FLUX.NE.0.0) THEN
            JJ = COLFLX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            IF (PBFACT*RES4(1).LT.FLUX) GO TO 150
            END IF
C                                       Check component size
         IF (CHKSIZ) THEN
            JJ = COLMJX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            BMAJ = RES4(1)
            JJ = COLMNX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            BMIN = RES4(1)
            IF (MIN(BMAJ,BMIN)*ASCALE.LT.CSIZE(1)) GO TO 150
            IF (MAX(BMAJ,BMIN)*ASCALE.GT.CSIZE(2)) GO TO 150
            END IF
C                                       scaling
         IF (IDOALL.GE.2) THEN
            JJ = CODLFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            DLFACT = RES4(1)
            END IF
C                                       RA, Dec
         IF (AXTYP(LOCNUM).EQ.1) THEN
            JJ = COLIDX(3)
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            DX = RPLOC(1,LOCNUM) + RES4(1) / AXINC(1,LOCNUM)
            JJ = COLIDX(4)
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            DY = RPLOC(2,LOCNUM) + RES4(1) / AXINC(2,LOCNUM)
C                                       offsets to coords
            CALL XYVAL (DX, DY, XX, YY, ZZ, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'ERROR IN COORDINATE'
               CALL MSGWRT (7)
            ELSE IF (CORTYP(LOCNUM).EQ.1) THEN
               IF (DOHMS.GT.0.0) THEN
                  RA = XX
                  DEC = YY
               ELSE
                  RA = (XX - RPVAL(1,LOCNUM)) *
     *               COS (DG2RAD*RPVAL(2,LOCNUM))
                  DEC = YY - RPVAL(2,LOCNUM)
                  IF (DOHMS.LE.-1.5) THEN
                     RA = RA * 3.6D3
                     DEC = DEC * 3.6D3
                     END IF
                  END IF
            ELSE
               IF (DOHMS.GT.0.0) THEN
                  RA = YY
                  DEC = XX
               ELSE
                  RA = (YY - RPVAL(2,LOCNUM)) *
     *               COS (DG2RAD*RPVAL(1,LOCNUM))
                  DEC = XX - RPVAL(1,LOCNUM)
                  IF (DOHMS.LE.-1.5) THEN
                     RA = RA * 3.6D3
                     DEC = DEC * 3.6D3
                     END IF
                  END IF
               END IF
            END IF
C                                       Loop over element in arrays.
         DO 140 IEL = 1,MAXLEN
C                                       Clear output
            IF (OKLIN) THEN
               WRITE (LINE,1100) IRNO
            ELSE
               LINE = ' '
               END IF
            DO 130 J = JCOL1,JCOL2
               IF (J.GT.MCOL) GO TO 130
               JJ = COLIDX(J)
C                                       get data.
               CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *            SCRBUF, IERR)
               IF (IERR.LT.0) GO TO 150
               IF (IERR.NE.0) GO TO 960
               I = KTY(JJ)
               IF (J.EQ.1) THEN
                  RTYPE = 12
                  I = 2
                  END IF
C                                       See which have data.
               LENGTH = RTYPE/10
               IF (LENGTH.LT.IEL) GO TO 130
               IF ((IEL.GT.1) .AND. ((I.EQ.3).OR.(I.EQ.7)))
     *            GO TO 130
C                                       character
               IF (I.EQ.3) THEN
                  L = GCH(J) + MAX (0, (NCH(JJ)-ICH(JJ))/2)
                  CALL H2CHR (ICH(JJ), 1, RESH, SCRTCH)
                  M = JTRIM (SCRTCH(:ICH(JJ)))
                  IF (M.GT.0) LINE(L:L+M-1) = SCRTCH(:M)
C                                       bit
               ELSE IF (I.EQ.7) THEN
                  L = GCH(J) + MAX (0, (NCH(JJ)-ICH(JJ))/2)
                  N = (ICH(JJ) - 1) / NBITWD + 1
                  DO 125 I = 1,N
                     M = NBITWD
                     IF (I.EQ.N) M = ICH(JJ) - (N-1)*NBITWD
                     CALL ZGTBIT (M, RESULT(I), BITS)
                     WRITE (MSGBUF,1120) (BITS(K), K = 1,M)
                     LINE(L:L+M-1) = MSGBUF(1:M)
                     L = L + M
  125                CONTINUE
C                                       numeric
               ELSE
C                                       double precision
                  IF (I.EQ.1) THEN
                     IF (RES8(IEL).EQ.DBLANK) THEN
                        MSGBUF = '  INDE'
                     ELSE
                        WRITE (MSGBUF,1101) RES8(IEL)
                        END IF
                     END IF
C                                       single precision
                  IF (I.EQ.2) THEN
                     IF (RES4(IEL).EQ.FBLANK) THEN
                        MSGBUF = '  INDE'
C                                       ra/dec
                     ELSE IF ((J.GE.3) .AND. (J.LE.4)) THEN
                        IF (DOHMS.GT.0.0) THEN
C                                       ra
                           IF (J+CORTYP(LOCNUM).NE.5) THEN
                              CALL COORDD (1, RA, CHSIGN, HMS, SEC)
                              IF (ISCALE.EQ.1) WRITE (MSGBUF,1251) HMS,
     *                           SEC
                              IF (ISCALE.EQ.2) WRITE (MSGBUF,1252) HMS,
     *                           SEC
                              IF (ISCALE.EQ.3) WRITE (MSGBUF,1253) HMS,
     *                           SEC
                              IF (MSGBUF(7:7).EQ.' ') MSGBUF(7:7) = '0'
C                                       dec
                           ELSE
                              CALL COORDD (2, DEC, CHSIGN, HMS, SEC)
                              IF (ISCALE.EQ.1) WRITE (MSGBUF,1256)
     *                           CHSIGN, HMS, SEC
                              IF (ISCALE.EQ.2) WRITE (MSGBUF,1257)
     *                           CHSIGN, HMS, SEC
                              IF (ISCALE.EQ.3) WRITE (MSGBUF,1258)
     *                           CHSIGN, HMS, SEC
                              IF (MSGBUF(8:8).EQ.' ') MSGBUF(8:8) = '0'
                           END IF
                        ELSE
C                                       ra
                           IF (J+CORTYP(LOCNUM).NE.5) THEN
                              IF (DOHMS.GT.-1.5) THEN
                                 WRITE (MSGBUF,1254) RA
                              ELSE
                                 WRITE (MSGBUF,1255) RA
                                 END IF
                           ELSE
                              IF (DOHMS.GT.-1.5) THEN
                                 WRITE (MSGBUF,1254) DEC
                              ELSE
                                 WRITE (MSGBUF,1255) DEC
                                 END IF
                              END IF
                           END IF
C                                       plane -> velocity
                     ELSE IF (J.EQ.11) THEN
                        IF (ALTAX.LT.0) THEN
                           VELPIX = 0.0
                        ELSE
                           RCHAN = RES4(IEL)
                           IF (OPTICL) THEN
                              VELPIX = REFVEL + VELINC * (RCHAN-ALTRFP)
     *                           / (1.D0 + AXDEN * (RCHAN-ALTRFP))
                           ELSE
                              VELPIX = REFVEL + VELINC * (RCHAN-ALTRFP)
                              END IF
                           VELPIX = VELPIX / 1000.0
                           END IF
                        WRITE (MSGBUF,1102) VELPIX
                     ELSE
                        OFFSET = RES4(IEL) * SCALE(J)
                        IF (J.EQ.2) OFFSET = OFFSET * PBFACT * DLFACT
                        WRITE (MSGBUF,1102) OFFSET
                        END IF
                     END IF
                  IF (I.EQ.4) WRITE (MSGBUF,1103) RESLI(IEL)
                  IF (I.EQ.5) WRITE (MSGBUF,1104) RESLO(IEL)
                  IF (I.EQ.6) WRITE (MSGBUF,1103) RESULT(IEL)
                  LINE(GCH(J):GCH(J)+NCH(JJ)-1) = MSGBUF(ICH(JJ):)
                  END IF
 130           CONTINUE
C                                       do output finally
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 970
 140        CONTINUE
 150     CONTINUE
      IF (JCOL2.LT.10) GO TO 10
C                                       Close down
 200  CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      GO TO 999
C                                       errors
 960  IRET = 3
      WRITE (MSGTXT,1960) IERR
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      GO TO 995
 970  IRET = 0
      IF (IERR.LT.0) THEN
         CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
         IRET = -1
         GO TO 999
         END IF
      IRET = 2
      WRITE (MSGTXT,1970) IERR
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
C
 995  CALL MSGWRT (8)
      IF (IRET.EQ.0) IRET = 5
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1099 FORMAT ('*****__ MF file version = ',I5,' __ ',A12,'.',A6,'.',I5,
     *   ' __*****')
 1100 FORMAT (I6)
 1101 FORMAT (1PD13.6)
 1102 FORMAT (F10.3)
 1103 FORMAT (I12)
 1104 FORMAT (11X,L1)
 1120 FORMAT (64I1)
 1251 FORMAT (2(I2.2,':'),F9.6)
 1252 FORMAT (2(I2.2,':'),F6.3)
 1253 FORMAT (2(I2.2,':'),F4.1)
 1254 FORMAT (F14.8)
 1255 FORMAT (F14.5)
 1256 FORMAT (A1,2(I2.2,':'),F8.5)
 1257 FORMAT (A1,2(I2.2,':'),F5.2)
 1258 FORMAT (A1,2(I2.2,':'),F3.0)
 1960 FORMAT ('ERROR',I5,' READING TABLE DATA')
 1970 FORMAT ('ERROR',I5,' WRITING OUTPUT')
      END
      SUBROUTINE MFP1C1 (INVERX, IRET)
C-----------------------------------------------------------------------
C   MFP1C1 reads, formats, and prints a table extension file for
C   single-channel images
C   Input:
C      INVERX I   MF version number currently being printed
C   Output:
C      IRET   I   Error code: 0 => OK or user terminates,
C                    2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   INVERX, IRET
C
      INCLUDE 'INCS:PMFC.INC'
      CHARACTER TITL1*132, TITL2*132, LINE*132, MSGBUF*72, SCRTCH*132,
     *   COLNUM*132, CUNIT(3)*8, CHSIGN*1, CTEMP*8, KEYWRD(5)*8, BUNIT*8
      INTEGER   SCRBUF(NUMCOL), IRNO, RESLI(1), JCOL1, JCOL2, I, J, K,
     *   L, M, N, NCH(128), JJ, ICH(128), RESULT(NUMCOL), BITS(64),
     *   RTYPE, IERR, IEL, LRNO, LENGTH, MAXLEN, HMS(2), JTRIM,
     *   KTY(128), GCH(128), NREC, TABLUN, MCH(128), COLIDX(10),
     *   CLEN(3), DEPTH(5), NUMKEY, LOCS(5), KEYTYP(5)
      LOGICAL   RESLO(1), CHKSIZ, FIRST, OKLIN
      REAL      RES4(1), SEC, OFFSET, BMAJ, BMIN, SCALE(10), DX, DY,
     *   PBFACT, DLFACT
      HOLLERITH RESH(1)
      DOUBLE PRECISION    RES8(1), RA, DEC, XX, YY, ZZ
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'MFPRT.INC'
      EQUIVALENCE (RESULT, RES8, RES4, RESH, RESLI, RESLO)
      DATA CUNIT /'mas', 'asec', 'deg'/
      DATA CLEN /15, 12, 10/
C                                       Column in table corresponding
C                                       to column in output
      DATA COLIDX /COLPEK, COLFLX, COLDLX, COLDLY, COLMJX, COLMNX,
     *   COLPAN, COLD0J, COLD0N, COLD0P/
C-----------------------------------------------------------------------
      IRET = 0
      FIRST = .TRUE.
      OKLIN = FRMT.GT.-0.5
C                                       Open table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      TABLUN = 27
      CALL TABINI ('READ', INEXT, INDISK, CNO, INVERX, CATBLK, TABLUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1010) IERR, INEXT, INVERX
         IF (IERR.NE.2) GO TO 995
         GO TO 999
         END IF
      BROW = 1
      EROW = BUFFER(5)
      IF (EROW.LT.BROW) GO TO 200
      IROW = 1
C                                       stuff for coordinates
      NUMKEY = 5
      KEYWRD(1) = 'DEPTH1'
      KEYWRD(2) = 'DEPTH2'
      KEYWRD(3) = 'DEPTH3'
      KEYWRD(4) = 'DEPTH4'
      KEYWRD(5) = 'DEPTH5'
      CALL TABKEY ('READ', KEYWRD, NUMKEY, BUFFER, LOCS, DEPTH,
     *   KEYTYP, IERR)
C                                       Velocity calculation
      IF (IERR.NE.0) CALL FILL (5, 1, DEPTH)
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
C                                       init counters, line size
      JCOL2 = 0
      CHKSIZ = CSIZE(2).GT.0.0
      CALL PRTSCL (KTY, ICH, MCH, RESULT, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      CALL COPY (128, MCH, NCH)
      CALL RFILL (10, 1.0, SCALE)
      SCALE(3) = ASCALE
      SCALE(4) = ASCALE
      SCALE(5) = ASCALE
      SCALE(6) = ASCALE
      SCALE(8) = ASCALE
      SCALE(9) = ASCALE
      IF (MAX(ABS(SMX(COLPEK)), ABS(SMN(COLPEK))).LT.10.)
     *   SCALE(2) = 1000.0
      IF (MAX(ABS(SMX(COLPEK)), ABS(SMN(COLPEK))).LT.0.010)
     *   SCALE(2) = 1.0E6
      SCALE(1) = SCALE(2)
      IF (DOHMS.GT.0.0) THEN
         NCH(COLIDX(3)) = CLEN(ISCALE)
         NCH(COLIDX(4)) = CLEN(ISCALE)
         ICH(COLIDX(3)) = 1
         ICH(COLIDX(4)) = 1
      ELSE IF (AXTYP(LOCNUM).EQ.1) THEN
         NCH(COLIDX(3)) = 14
         NCH(COLIDX(4)) = 14
         ICH(COLIDX(3)) = 1
         ICH(COLIDX(4)) = 1
         END IF
C                                       Loop: list all columns to fit
 10   JCOL1 = JCOL2 + 1
      MAXLEN = 1
C                                       how many columns this pass
      N = 0
      IF (OKLIN) N = 8
      DO 15 J = JCOL1, 10
         I = MOD (DATP(COLIDX(J),2), 10)
         LENGTH = DATP(COLIDX(J),2) / 10
         NCH(COLIDX(J)) = MIN (NCH(COLIDX(J)), NACROS-9)
         N = N + NCH(COLIDX(J)) + 2
         IF (N.LE.NACROS) JCOL2 = J
         IF (N.GT.NACROS) GO TO 20
         IF ((I.NE.3) .AND. (I.NE.7)) MAXLEN = MAX (MAXLEN, LENGTH)
 15      CONTINUE
C                                       Blanks between
 20   IF (JCOL2.LT.JCOL1) JCOL2 = JCOL1
      N = 0
      IF (OKLIN) N = 8
      DO 25 J = JCOL1,JCOL2
         N = N + NCH(COLIDX(J))
 25      CONTINUE
      N = (NACROS - N) / (JCOL2 - JCOL1 + 1)
      N = MIN (N, 6)
      GCH(JCOL1) = 1
      IF (OKLIN) GCH(JCOL1) = 9
      IF (JCOL1.LT.JCOL2) THEN
         I = JCOL1 + 1
         DO 30 J = I,JCOL2
            GCH(J) = GCH(J-1) + NCH(COLIDX(J-1)) + N
 30         CONTINUE
         END IF
C                                       first time
      NLINE = 990
C                                       List column numbers
      TITL1 = ' '
      TITL2 = ' '
      COLNUM = ' '
      LINE = ' '
      IF (OKLIN) TITL1 = ' Row #'
C                                       Column label
      DO 95 J = JCOL1,JCOL2
         JJ = COLIDX(J)
         SCRTCH = ' '
         IF (J.EQ.1) SCRTCH = 'Peak int'
         IF (J.EQ.2) SCRTCH = 'Tot int'
         IF (J.EQ.3) THEN
            SCRTCH = 'X-offset'
            IF (AXTYP(LOCNUM).EQ.1) THEN
               IF (DOHMS.GT.0.0) THEN
                  SCRTCH = 'Right Ascension'
               ELSE
                  SCRTCH = 'RA offset'
                  END IF
               END IF
            END IF
         IF (J.EQ.4) THEN
            SCRTCH = 'Y-offset'
            IF (AXTYP(LOCNUM).EQ.1) THEN
               IF (DOHMS.GT.0.0) THEN
                  SCRTCH = 'Declination'
               ELSE
                  SCRTCH = 'DEC offset'
                  END IF
               END IF
            END IF
         IF (J.EQ.5) SCRTCH = 'Maj axis'
         IF (J.EQ.6) SCRTCH = 'Min axis'
         IF (J.EQ.7) SCRTCH = 'Pos angle'
         IF (J.EQ.8) SCRTCH = 'Decon maj'
         IF (J.EQ.9) SCRTCH = 'Decon min'
         IF (J.EQ.10) SCRTCH = 'Decon PA'
         M = JTRIM (SCRTCH(:NCH(JJ)))
         IF ((M.GT.0) .AND. (J.LE.10)) TITL1(GCH(J)+NCH(JJ)-M:) =
     *      SCRTCH(:M)
 95      CONTINUE
      DO 99 J = JCOL1,JCOL2
         JJ = COLIDX(J)
         SCRTCH = ' '
         IF (J.EQ.1) THEN
            CALL H2CHR (8, 1, CATH(KHBUN), SCRTCH)
            BUNIT = SCRTCH(:8)
            CALL CHLTOU (8, BUNIT)
            IF (BUNIT.EQ.'JY/BEAM') SCRTCH = 'Jy/beam'
            IF (SCALE(1).NE.1.0) THEN
               CTEMP = SCRTCH(:8)
               IF (SCALE(1).LT.1.E4) THEN
                  SCRTCH = 'm' // CTEMP(:7)
               ELSE
                  SCRTCH = 'u' // CTEMP(:7)
                  END IF
               END IF
         ELSE IF (J.EQ.2) THEN
            CALL H2CHR (8, 1, CATH(KHBUN), SCRTCH)
            BUNIT = SCRTCH(:8)
            CALL CHLTOU (8, BUNIT)
            IF (BUNIT.EQ.'JY/BEAM') SCRTCH = 'Jy'
            IF (SCALE(2).NE.1.0) THEN
               CTEMP = SCRTCH(:8)
               IF (SCALE(1).LT.1.E4) THEN
                  SCRTCH = 'm' // CTEMP(:7)
               ELSE
                  SCRTCH = 'u' // CTEMP(:7)
                  END IF
               END IF
         ELSE IF ((DOHMS.GT.0.0) .AND. ((J.EQ.3) .OR. (J.EQ.4))) THEN
            SCRTCH = ' '
         ELSE IF ((DOHMS.LE.-1.5) .AND. ((J.EQ.3) .OR. (J.EQ.4))) THEN
            SCRTCH = 'arc sec'
         ELSE IF ((J.EQ.3) .OR. (J.EQ.4)) THEN
            SCRTCH = 'degrees'
         ELSE IF ((J.GE.3) .AND. (J.LE.6)) THEN
            SCRTCH = CUNIT(ISCALE)
         ELSE IF ((J.GE.8) .AND. (J.LE.9)) THEN
            SCRTCH = CUNIT(ISCALE)
         ELSE IF ((J.EQ.7) .OR. (J.EQ.10)) THEN
            SCRTCH = 'degrees'
            END IF
         M = JTRIM (SCRTCH(:8))
         M = MAX (1, M)
         IF ((M.GT.0) .AND. (J.LE.10)) TITL2(GCH(J)+NCH(JJ)-M:) =
     *      SCRTCH(:M)
 99      CONTINUE
      IF (FIRST) THEN
         FIRST = .FALSE.
         WRITE (LINE,1099) INVERX, INNAM, INCLS, INSEQ
         CALL REFRMT (LINE, '_', JJ)
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
      IF (DOCRT.LE.-2.5) THEN
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      TITL1, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      TITL2, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
      IF ((DOCRT.LE.0.0) .AND. (DOCRT.GT.-2.5)) THEN
         LINE = ' '
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Output loop
      LRNO = 0
      PBFACT = 1.0
      DLFACT = 1.0
      DO 150 IRNO = BROW,EROW,IROW
C                                       scaling
         IF (IDOALL.GE.2) THEN
            JJ = CODLFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            DLFACT = RES4(1)
            END IF
         IF ((IDOALL.EQ.1) .OR. (IDOALL.EQ.3)) THEN
            JJ = COBMFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            PBFACT = RES4(1)
            END IF
C                                       Check flux level
         IF (FLUX.NE.0.0) THEN
            JJ = COLFLX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            IF (PBFACT*RES4(1).LT.FLUX) GO TO 150
            END IF
C                                       Check component size
         IF (CHKSIZ) THEN
            JJ = COLMJX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            BMAJ = RES4(1)
            JJ = COLMNX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            BMIN = RES4(1)
            IF (MIN(BMAJ,BMIN)*ASCALE.LT.CSIZE(1)) GO TO 150
            IF (MAX(BMAJ,BMIN)*ASCALE.GT.CSIZE(2)) GO TO 150
            END IF
C                                       RA, Dec
         IF (AXTYP(LOCNUM).EQ.1) THEN
            JJ = COLIDX(3)
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            DX = RPLOC(1,LOCNUM) + RES4(1) / AXINC(1,LOCNUM)
            JJ = COLIDX(4)
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            DY = RPLOC(2,LOCNUM) + RES4(1) / AXINC(2,LOCNUM)
C                                       offsets to coords
            CALL XYVAL (DX, DY, XX, YY, ZZ, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'ERROR IN COORDINATE'
               CALL MSGWRT (7)
            ELSE IF (CORTYP(LOCNUM).EQ.1) THEN
               IF (DOHMS.GT.0.0) THEN
                  RA = XX
                  DEC = YY
               ELSE
                  RA = (XX - RPVAL(1,LOCNUM)) *
     *               COS (DG2RAD*RPVAL(2,LOCNUM))
                  DEC = YY - RPVAL(2,LOCNUM)
                  IF (DOHMS.LE.-1.5) THEN
                     RA = RA * 3.6D3
                     DEC = DEC * 3.6D3
                     END IF
                  END IF
            ELSE
               IF (DOHMS.GT.0.0) THEN
                  RA = YY
                  DEC = XX
               ELSE
                  RA = (YY - RPVAL(2,LOCNUM)) *
     *               COS (DG2RAD*RPVAL(1,LOCNUM))
                  DEC = XX - RPVAL(1,LOCNUM)
                  IF (DOHMS.LE.-1.5) THEN
                     RA = RA * 3.6D3
                     DEC = DEC * 3.6D3
                     END IF
                  END IF
               END IF
            END IF
C                                       Loop over element in arrays.
         DO 140 IEL = 1,MAXLEN
C                                       Clear output
            IF (OKLIN) THEN
               WRITE (LINE,1100) IRNO
            ELSE
               LINE = ' '
               END IF
            DO 130 J = JCOL1,JCOL2
               IF (J.GT.10) GO TO 130
               JJ = COLIDX(J)
C                                       get data.
               CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *            SCRBUF, IERR)
               IF (IERR.LT.0) GO TO 150
               IF (IERR.NE.0) GO TO 960
               I = KTY(JJ)
C                                       See which have data.
               LENGTH = RTYPE/10
               IF (LENGTH.LT.IEL) GO TO 130
               IF ((IEL.GT.1) .AND. ((I.EQ.3).OR.(I.EQ.7)))
     *            GO TO 130
C                                       character
               IF (I.EQ.3) THEN
                  L = GCH(J) + MAX (0, (NCH(JJ)-ICH(JJ))/2)
                  CALL H2CHR (ICH(JJ), 1, RESH, SCRTCH)
                  M = JTRIM (SCRTCH(:ICH(JJ)))
                  IF (M.GT.0) LINE(L:L+M-1) = SCRTCH(:M)
C                                       bit
               ELSE IF (I.EQ.7) THEN
                  L = GCH(J) + MAX (0, (NCH(JJ)-ICH(JJ))/2)
                  N = (ICH(JJ) - 1) / NBITWD + 1
                  DO 125 I = 1,N
                     M = NBITWD
                     IF (I.EQ.N) M = ICH(JJ) - (N-1)*NBITWD
                     CALL ZGTBIT (M, RESULT(I), BITS)
                     WRITE (MSGBUF,1120) (BITS(K), K = 1,M)
                     LINE(L:L+M-1) = MSGBUF(1:M)
                     L = L + M
  125                CONTINUE
C                                       numeric
               ELSE
C                                       double precision
                  IF (I.EQ.1) THEN
                     IF (RES8(IEL).EQ.DBLANK) THEN
                        MSGBUF = '  INDE'
                     ELSE
                        WRITE (MSGBUF,1101) RES8(IEL)
                        END IF
                     END IF
C                                       single precision
                  IF (I.EQ.2) THEN
                     IF (RES4(IEL).EQ.FBLANK) THEN
                        MSGBUF = '  INDE'
C                                       ra/dec
                     ELSE IF ((J.GE.3) .AND. (J.LE.4)) THEN
                        IF (DOHMS.GT.0.0) THEN
C                                       ra
                           IF (J+CORTYP(LOCNUM).NE.5) THEN
                              CALL COORDD (1, RA, CHSIGN, HMS, SEC)
                              IF (ISCALE.EQ.1) WRITE (MSGBUF,1251) HMS,
     *                           SEC
                              IF (ISCALE.EQ.2) WRITE (MSGBUF,1252) HMS,
     *                           SEC
                              IF (ISCALE.EQ.3) WRITE (MSGBUF,1253) HMS,
     *                           SEC
                              IF (MSGBUF(7:7).EQ.' ') MSGBUF(7:7) = '0'
C                                       dec
                           ELSE
                              CALL COORDD (2, DEC, CHSIGN, HMS, SEC)
                              IF (ISCALE.EQ.1) WRITE (MSGBUF,1256)
     *                           CHSIGN, HMS, SEC
                              IF (ISCALE.EQ.2) WRITE (MSGBUF,1257)
     *                           CHSIGN, HMS, SEC
                              IF (ISCALE.EQ.3) WRITE (MSGBUF,1258)
     *                           CHSIGN, HMS, SEC
                              IF (MSGBUF(8:8).EQ.' ') MSGBUF(8:8) = '0'
                           END IF
                        ELSE
C                                       ra
                           IF (J+CORTYP(LOCNUM).NE.5) THEN
                              IF (DOHMS.GT.-1.5) THEN
                                 WRITE (MSGBUF,1254) RA
                              ELSE
                                 WRITE (MSGBUF,1255) RA
                                 END IF
                           ELSE
                              IF (DOHMS.GT.-1.5) THEN
                                 WRITE (MSGBUF,1254) DEC
                              ELSE
                                 WRITE (MSGBUF,1255) DEC
                                 END IF
                              END IF
                           END IF
                     ELSE
                        OFFSET = RES4(IEL) * SCALE(J)
                        IF (J.EQ.1) OFFSET = OFFSET * PBFACT * DLFACT
                        IF (J.EQ.2) OFFSET = OFFSET * PBFACT
                        WRITE (MSGBUF,1102) OFFSET
                        END IF
                     END IF
                  IF (I.EQ.4) WRITE (MSGBUF,1103) RESLI(IEL)
                  IF (I.EQ.5) WRITE (MSGBUF,1104) RESLO(IEL)
                  IF (I.EQ.6) WRITE (MSGBUF,1103) RESULT(IEL)
                  LINE(GCH(J):GCH(J)+NCH(JJ)-1) = MSGBUF(ICH(JJ):)
                  END IF
 130           CONTINUE
C                                       do output finally
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 970
 140        CONTINUE
 150     CONTINUE
      IF (JCOL2.LT.10) GO TO 10
C                                       Close down
 200  CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      GO TO 999
C                                       errors
 960  IRET = 3
      WRITE (MSGTXT,1960) IERR
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      GO TO 995
 970  IRET = 0
      IF (IERR.LT.0) THEN
         CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
         IRET = -1
         GO TO 999
         END IF
      IRET = 2
      WRITE (MSGTXT,1970) IERR
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
C
 995  CALL MSGWRT (8)
      IF (IRET.EQ.0) IRET = 5
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1099 FORMAT ('*****__ MF file version = ',I5,' __ ',A12,'.',A6,'.',I5,
     *   ' __*****')
 1100 FORMAT (I6)
 1101 FORMAT (1PD13.6)
 1102 FORMAT (F10.3)
 1103 FORMAT (I12)
 1104 FORMAT (11X,L1)
 1120 FORMAT (64I1)
 1251 FORMAT (2(I2.2,':'),F9.6)
 1252 FORMAT (2(I2.2,':'),F6.3)
 1253 FORMAT (2(I2.2,':'),F4.1)
 1254 FORMAT (F14.8)
 1255 FORMAT (F14.5)
 1256 FORMAT (A1,2(I2.2,':'),F8.5)
 1257 FORMAT (A1,2(I2.2,':'),F5.2)
 1258 FORMAT (A1,2(I2.2,':'),F3.0)
 1960 FORMAT ('ERROR',I5,' READING TABLE DATA')
 1970 FORMAT ('ERROR',I5,' WRITING OUTPUT')
      END
      SUBROUTINE MFPUSR (INVERX, IRET)
C-----------------------------------------------------------------------
C   MFPUSR reads, formats, and prints a table extension file for a list
C   of columns selected by the user
C   Input:
C      INVERX I   MF version number currently being printed
C   Output:
C      IRET   I   Error code: 0 => OK or user terminates,
C                    2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   INVERX, IRET
C
      INCLUDE 'INCS:PMFC.INC'
      CHARACTER TITL1*132, TITL2*132, LINE*132, MSGBUF*72, SCRTCH*132,
     *   COLNUM*132,  CUNIT(3)*8, CHSIGN*1, CTEMP*8, KEYWRD(5)*8,
     *   COLTIT(NUMCOL+1)*12, TYPCHR(4)*8, BUNIT*8
      INTEGER   SCRBUF(NUMCOL), IRNO, RESLI(1), JCOL1, JCOL2, I, J, K,
     *   L, M, N, NCH(128), JJ, ICH(128), RESULT(NUMCOL), BITS(64),
     *   RTYPE, IERR, IEL, LRNO, LENGTH, MAXLEN, HMS(2), JTRIM,
     *   KTY(128), GCH(128), NREC, TABLUN, MCH(128), COLIDX(NUMCOL+1),
     *   CLEN(3), DEPTH(5), NUMKEY, LOCS(5), KEYTYP(5), COLHDX(2),
     *   IROUND, MCOL, TYPCOL(NUMCOL+1), ISVEL, ALTAX
      LOGICAL   RESLO(1), CHKSIZ, FIRST, OKLIN, OPTICL
      REAL      RES4(1), SEC, OFFSET, BMAJ, BMIN, SCALE(NUMCOL+1), DX,
     *   DY, FSCALE, VELINC, ALTRFP, RCHAN, PBFACT, DLFACT, DELNU
      HOLLERITH RESH(1)
      DOUBLE PRECISION    RES8(1), RA, DEC, XX, YY, ZZ, REFVEL, VELPIX,
     *   NUX, AXDEN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'MFPRT.INC'
      EQUIVALENCE (RESULT, RES8, RES4, RESH, RESLI, RESLO)
      DATA CUNIT /'mas', 'asec', 'deg'/
      DATA CLEN /15, 12, 10/
      DATA COLHDX /COLDLX, COLDLY/
      DATA TYPCOL /1,2,2,4,4, 3,3,1,2,2, 2,2,2,3,3, 3,3,1,2,2,
     *   2,1,3,3,1, 3,3,1,3,3, 1,2,2,2,1, 1,1,1,1,1, 1,1/
      DATA COLTIT /'Plane', 'Peak int', 'Tot int', 'X-offset',
     *   'Y-offset', 'Maj axis', 'Min axis', 'Pos angle', 'Q flux',
     *   'U flux', 'V flux', 'Err peak int', 'Err flux', 'Err X-off',
     *   'Err Y-off', 'Err maj axis', 'Err min axis', 'Err PA',
     *   'Err Q flux', 'Err U flux', 'Err V flux', 'Model type',
     *   'Decon maj', 'Decon min', 'Decon PA', 'Decon-maj',
     *   'Decon-min', 'Decon-PA', 'Decon+maj', 'Decon+min',
     *   'Decon+PA', 'Resid rms', 'Resid peak', 'Resid flux',
     *   'X-pixel', 'Y-pixel', 'Maj pixs', 'Min pixs', 'PA pixs',
     *   'Pbeam factor', 'Dbeam factor', 'Velocity'/
      DATA TYPCHR /'   Point', 'Gaussian', 'Spheroid', 'Exponent'/
C-----------------------------------------------------------------------
      IRET = 0
      FIRST = .TRUE.
C                                       Open table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      TABLUN = 27
      CALL TABINI ('READ', INEXT, INDISK, CNO, INVERX, CATBLK, TABLUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1010) IERR, INEXT, INVERX
         IF (IERR.NE.2) GO TO 995
         GO TO 999
         END IF
      BROW = 1
      EROW = BUFFER(5)
      IF (EROW.LT.BROW) GO TO 200
      IROW = 1
      MCOL = 0
      OKLIN = FRMT.GT.-0.5
      ISVEL = 0
      DO 5 J = 1,NUMCOL+1
         N = IROUND (UCOLS(J))
         IF (N.LE.0) THEN
            GO TO 10
         ELSE
            MCOL = MCOL + 1
            COLIDX(MCOL) = N
            IF (N.EQ.NUMCOL+1) ISVEL = MCOL
            END IF
 5       CONTINUE
 10   IF (MCOL.LE.0) THEN
         DO 15 MCOL = 1,NUMCOL+1
            COLIDX(MCOL) = MCOL
 15         CONTINUE
         MCOL = NUMCOL+1
         ISVEL = MCOL
         END IF
C                                       stuff for coordinates
      NUMKEY = 5
      KEYWRD(1) = 'DEPTH1'
      KEYWRD(2) = 'DEPTH2'
      KEYWRD(3) = 'DEPTH3'
      KEYWRD(4) = 'DEPTH4'
      KEYWRD(5) = 'DEPTH5'
      CALL TABKEY ('READ', KEYWRD, NUMKEY, BUFFER, LOCS, DEPTH,
     *   KEYTYP, IERR)
C                                       Velocity calculation
      IF (IERR.NE.0) CALL FILL (5, 1, DEPTH)
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
      IF (ISVEL.GT.0) THEN
         ALTAX = CATBLK(KIALT)
         IF (ALTAX.LE.0) THEN
            MSGTXT = 'VELOCITY INFO MISSING - OMITTED'
            CALL MSGWRT (6)
            IF (ISVEL.EQ.MCOL) THEN
               MCOL = MCOL - 1
            ELSE
               COLIDX(ISVEL) = COLPLN
               END IF
            ISVEL = 0
            VELINC = 0.0
            ALTAX = -1
         ELSE
            OPTICL = ALTAX.LE.256
            CALL H2CHR (8, 1, CATH(KHCTP+2*KLOCF(LOCNUM)), CTEMP)
            IF (CTEMP(:4).EQ.'FREQ') THEN
               ALTRFP = CATR(KRARP)
               REFVEL = CATD(KDARV)
               DELNU = CATR(KRCIC+KLOCF(LOCNUM))
               NUX = CATD(KDCRV+KLOCF(LOCNUM)) + DELNU *
     *            (ALTRFP - CATR(KRCRP+KLOCF(LOCNUM)))
               IF (OPTICL) THEN
                  VELINC = -(DELNU * (VELITE + REFVEL)) / NUX
                  AXDEN = -VELINC / (VELITE + REFVEL)
               ELSE
                  VELINC = -(DELNU * (VELITE - REFVEL)) / NUX
                  END IF
C                                       axis ALTSW'ed
            ELSE
               AXDEN = AXDENU(LOCNUM)
               REFVEL = CATD(KDCRV+KLOCF(LOCNUM))
               VELINC = CATR(KRCIC+KLOCF(LOCNUM))
               ALTRFP = CATR(KRCRP+KLOCF(LOCNUM))
               END IF
            END IF
         END IF
C                                       init counters, line size
      JCOL2 = 0
      CHKSIZ = CSIZE(2).GT.0.0
      CALL PRTSCL (KTY, ICH, MCH, RESULT, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      CALL COPY (128, MCH, NCH)
      ICH(NUMCOL+1) = ICH(COLPLN)
      NCH(NUMCOL+1) = NCH(COLPLN)
      CALL RFILL (NUMCOL+1, 1.0, SCALE)
      FSCALE = 1.0
      IF (MAX(ABS(SMX(2)), ABS(SMN(2))).LT.10.) FSCALE = 1000.0
      IF (MAX(ABS(SMX(2)), ABS(SMN(2))).LT.0.010) FSCALE = 1.0E6
      DO 20 J = 1,MCOL
         N = TYPCOL(COLIDX(J))
         IF (N.EQ.2) THEN
            SCALE(J) = FSCALE
         ELSE IF (N.EQ.3) THEN
            SCALE(J) = ASCALE
         ELSE IF (N.EQ.4) THEN
            IF (DOHMS.GT.0.0) THEN
               NCH(COLIDX(J)) = CLEN(ISCALE)
               ICH(COLIDX(J)) = 1
            ELSE IF (AXTYP(LOCNUM).EQ.1) THEN
               NCH(COLIDX(J)) = 14
               ICH(COLIDX(J)) = 1
            ELSE
               SCALE(J) = ASCALE
               END IF
            END IF
 20      CONTINUE
C                                       Loop: list all columns to fit
 30   JCOL1 = JCOL2 + 1
      MAXLEN = 1
C                                       how many columns this pass
      N = 0
      IF (OKLIN) N = 8
      DO 65 J = JCOL1,MCOL
         I = MOD (DATP(COLIDX(J),2), 10)
         LENGTH = DATP(COLIDX(J),2) / 10
         NCH(COLIDX(J)) = MIN (NCH(COLIDX(J)), NACROS-9)
         N = N + NCH(COLIDX(J)) + 2
         IF (N.LE.NACROS) JCOL2 = J
         IF (N.GT.NACROS) GO TO 70
         IF ((I.NE.3) .AND. (I.NE.7)) MAXLEN = MAX (MAXLEN, LENGTH)
 65      CONTINUE
C                                       Blanks between
 70   IF (JCOL2.LT.JCOL1) JCOL2 = JCOL1
      N = 0
      IF (OKLIN) N = 8
      DO 75 J = JCOL1,JCOL2
         N = N + NCH(COLIDX(J))
 75      CONTINUE
      N = (NACROS - N) / (JCOL2 - JCOL1 + 1)
      N = MIN (N, 6)
      GCH(JCOL1) = 1
      IF (OKLIN) GCH(JCOL1) = 9
      IF (JCOL1.LT.JCOL2) THEN
         I = JCOL1 + 1
         DO 80 J = I,JCOL2
            GCH(J) = GCH(J-1) + NCH(COLIDX(J-1)) + N
 80         CONTINUE
         END IF
C                                       first time
      NLINE = 990
C                                       List column numbers
      TITL1 = ' '
      TITL2 = ' '
      COLNUM = ' '
      LINE = ' '
      IF (OKLIN) TITL1 = ' Row #'
C                                       Column label
      DO 90 J = JCOL1,JCOL2
         JJ = COLIDX(J)
         SCRTCH = COLTIT (JJ)
         IF ((JJ.EQ.COLDLX) .AND. (AXTYP(LOCNUM).EQ.1)) THEN
            IF (DOHMS.GT.0.0) THEN
               SCRTCH = 'Right Ascension'
            ELSE
               SCRTCH = 'RA offset'
               END IF
            END IF
         IF ((JJ.EQ.COLDLY) .AND. (AXTYP(LOCNUM).EQ.1)) THEN
            IF (DOHMS.GT.0.0) THEN
               SCRTCH = 'Declination'
            ELSE
               SCRTCH = 'DEC offset'
               END IF
            END IF
         M = JTRIM (SCRTCH)
         M = MIN (M, NCH(JJ))
         IF (SCRTCH(M:M).EQ.' ') M = M - 1
         IF ((M.GT.0) .AND. (J.LE.MCOL)) TITL1(GCH(J)+NCH(JJ)-M:) =
     *      SCRTCH(:M)
 90      CONTINUE
      DO 95 J = JCOL1,JCOL2
         JJ = COLIDX(J)
         SCRTCH = ' '
         IF (TYPCOL(JJ).EQ.2) THEN
            CALL H2CHR (8, 1, CATH(KHBUN), SCRTCH)
            BUNIT = SCRTCH(:8)
            CALL CHLTOU (8, BUNIT)
            IF (BUNIT.EQ.'JY/BEAM') THEN
               SCRTCH = 'Jy'
               IF ((JJ.EQ.COLPEK) .OR. (JJ.EQ.COEPEK))
     *            SCRTCH = 'Jy/beam'
               END IF
            IF (SCALE(1).NE.1.0) THEN
               CTEMP = SCRTCH(:8)
               IF (SCALE(1).LT.1.E4) THEN
                  SCRTCH = 'm' // CTEMP(:7)
               ELSE
                  SCRTCH = 'u' // CTEMP(:7)
                  END IF
               END IF
         ELSE IF (TYPCOL(JJ).EQ.4) THEN
            IF (DOHMS.GT.0.0) THEN
               SCRTCH = ' '
            ELSE IF (DOHMS.LE.-1.5) THEN
               SCRTCH = 'arc sec'
            ELSE
               SCRTCH = 'degrees'
               END IF
         ELSE IF (TYPCOL(JJ).EQ.3) THEN
            SCRTCH = CUNIT(ISCALE)
         ELSE
            SCRTCH = 'pixels'
            IF ((JJ.EQ.COLPAN) .OR. (JJ.EQ.COEPAN) .OR. (JJ.EQ.COLD0P)
     *         .OR. (JJ.EQ.COLDMP) .OR. (JJ.EQ.COLDPP) .OR.
     *         (JJ.EQ.COPPAN)) SCRTCH = 'degrees'
            IF ((JJ.EQ.COLTYP) .OR. (JJ.EQ.COBMFA) .OR. (JJ.EQ.CODLFA))
     *         SCRTCH = ' '
            IF (JJ.EQ.NUMCOL+1) SCRTCH = 'km/s'
            END IF
         M = JTRIM (SCRTCH(:8))
         M = MAX (1, M)
         IF ((M.GT.0) .AND. (J.LE.MCOL)) TITL2(GCH(J)+NCH(JJ)-M:) =
     *      SCRTCH(:M)
 95      CONTINUE
      IF (FIRST) THEN
         FIRST = .FALSE.
         WRITE (LINE,1095) INVERX, INNAM, INCLS, INSEQ
         CALL REFRMT (LINE, '_', JJ)
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
      IF (DOCRT.LE.-2.5) THEN
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      TITL1, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      TITL2, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
      IF ((DOCRT.LE.0.0) .AND. (DOCRT.GT.-2.5)) THEN
         LINE = ' '
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Output loop
      LRNO = 0
      PBFACT = 1.0
      DLFACT = 1.0
      DO 150 IRNO = BROW,EROW,IROW
C                                       scaling
         IF (IDOALL.GE.2) THEN
            JJ = CODLFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            DLFACT = RES4(1)
            END IF
         IF ((IDOALL.EQ.1) .OR. (IDOALL.EQ.3)) THEN
            JJ = COBMFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            PBFACT = RES4(1)
            END IF
C                                       Check flux level
         IF (FLUX.NE.0.0) THEN
            JJ = COLFLX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            IF (PBFACT*RES4(1).LT.FLUX) GO TO 150
            END IF
C                                       Check component size
         IF (CHKSIZ) THEN
            JJ = COLMJX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            BMAJ = RES4(1)
            JJ = COLMNX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            BMIN = RES4(1)
            IF (MIN(BMAJ,BMIN)*ASCALE.LT.CSIZE(1)) GO TO 150
            IF (MAX(BMAJ,BMIN)*ASCALE.GT.CSIZE(2)) GO TO 150
            END IF
C                                       RA, Dec
         IF (AXTYP(LOCNUM).EQ.1) THEN
            JJ = COLHDX(1)
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            DX = RPLOC(1,LOCNUM) + RES4(1) / AXINC(1,LOCNUM)
            JJ = COLHDX(2)
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            DY = RPLOC(2,LOCNUM) + RES4(1) / AXINC(2,LOCNUM)
C                                       offsets to coords
            CALL XYVAL (DX, DY, XX, YY, ZZ, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'ERROR IN COORDINATE'
               CALL MSGWRT (7)
            ELSE IF (CORTYP(LOCNUM).EQ.1) THEN
               IF (DOHMS.GT.0.0) THEN
                  RA = XX
                  DEC = YY
               ELSE
                  RA = (XX - RPVAL(1,LOCNUM)) *
     *               COS (DG2RAD*RPVAL(2,LOCNUM))
                  DEC = YY - RPVAL(2,LOCNUM)
                  IF (DOHMS.LE.-1.5) THEN
                     RA = RA * 3.6D3
                     DEC = DEC * 3.6D3
                     END IF
                  END IF
            ELSE
               IF (DOHMS.GT.0.0) THEN
                  RA = YY
                  DEC = XX
               ELSE
                  RA = (YY - RPVAL(2,LOCNUM)) *
     *               COS (DG2RAD*RPVAL(1,LOCNUM))
                  DEC = XX - RPVAL(1,LOCNUM)
                  IF (DOHMS.LE.-1.5) THEN
                     RA = RA * 3.6D3
                     DEC = DEC * 3.6D3
                     END IF
                  END IF
               END IF
            END IF
C                                       Loop over element in arrays.
         DO 140 IEL = 1,MAXLEN
C                                       Clear output
            IF (OKLIN) THEN
               WRITE (LINE,1100) IRNO
            ELSE
               LINE = ' '
               END IF
            DO 130 J = JCOL1,JCOL2
               IF (J.GT.MCOL) GO TO 130
               JJ = COLIDX(J)
               IF (JJ.EQ.NUMCOL+1) JJ = COLPLN
C                                       get data.
               CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *            SCRBUF, IERR)
               IF (IERR.LT.0) GO TO 150
               IF (IERR.NE.0) GO TO 960
               I = KTY(JJ)
               JJ = COLIDX(J)
C                                       See which have data.
               LENGTH = RTYPE/10
               IF (LENGTH.LT.IEL) GO TO 130
               IF ((IEL.GT.1) .AND. ((I.EQ.3).OR.(I.EQ.7)))
     *            GO TO 130
C                                       character
               IF (I.EQ.3) THEN
                  L = GCH(J) + MAX (0, (NCH(JJ)-ICH(JJ))/2)
                  CALL H2CHR (ICH(JJ), 1, RESH, SCRTCH)
                  M = JTRIM (SCRTCH(:ICH(JJ)))
                  IF (M.GT.0) LINE(L:L+M-1) = SCRTCH(:M)
C                                       bit
               ELSE IF (I.EQ.7) THEN
                  L = GCH(J) + MAX (0, (NCH(JJ)-ICH(JJ))/2)
                  N = (ICH(JJ) - 1) / NBITWD + 1
                  DO 125 I = 1,N
                     M = NBITWD
                     IF (I.EQ.N) M = ICH(JJ) - (N-1)*NBITWD
                     CALL ZGTBIT (M, RESULT(I), BITS)
                     WRITE (MSGBUF,1120) (BITS(K), K = 1,M)
                     LINE(L:L+M-1) = MSGBUF(1:M)
                     L = L + M
  125                CONTINUE
C                                       numeric
C                                       double precision
               ELSE IF (I.EQ.1) THEN
                  IF (RES8(IEL).EQ.DBLANK) THEN
                     MSGBUF = '  INDE'
                  ELSE
                     WRITE (MSGBUF,1101) RES8(IEL)
                     END IF
C                                       single precision
               ELSE IF (I.EQ.2) THEN
                  IF (RES4(IEL).EQ.FBLANK) THEN
                     MSGBUF = '  INDE'
C                                       ra/dec
                  ELSE IF ((JJ.GE.COLDLX) .AND. (JJ.LE.COLDLY)) THEN
C                                       ra
                     IF (JJ+CORTYP(LOCNUM).NE.2+COLDLX) THEN
                        IF (DOHMS.GT.0.0) THEN
                           CALL COORDD (1, RA, CHSIGN, HMS, SEC)
                           IF (ISCALE.EQ.1) WRITE (MSGBUF,1251) HMS, SEC
                           IF (ISCALE.EQ.2) WRITE (MSGBUF,1252) HMS, SEC
                           IF (ISCALE.EQ.3) WRITE (MSGBUF,1253) HMS, SEC
                           IF (MSGBUF(7:7).EQ.' ') MSGBUF(7:7) = '0'
                        ELSE IF (DOHMS.GT.-1.5) THEN
                           WRITE (MSGBUF,1254) RA
                        ELSE
                           WRITE (MSGBUF,1255) RA
                           END IF
C                                       dec
                     ELSE
                        IF (DOHMS.GT.0.0) THEN
                           CALL COORDD (2, DEC, CHSIGN, HMS, SEC)
                           IF (ISCALE.EQ.1) WRITE (MSGBUF,1256) CHSIGN,
     *                        HMS, SEC
                           IF (ISCALE.EQ.2) WRITE (MSGBUF,1257) CHSIGN,
     *                        HMS, SEC
                           IF (ISCALE.EQ.3) WRITE (MSGBUF,1258) CHSIGN,
     *                        HMS, SEC
                              IF (MSGBUF(8:8).EQ.' ') MSGBUF(8:8) = '0'
                        ELSE IF (DOHMS.GT.-1.5) THEN
                           WRITE (MSGBUF,1254) DEC
                        ELSE
                           WRITE (MSGBUF,1255) DEC
                           END IF
                        END IF
                  ELSE IF (JJ.EQ.COLTYP) THEN
                     K = RES4(IEL) + 1.5
                     MSGBUF = ' '
                     MSGBUF(NCH(JJ)+ICH(JJ)-8:) = TYPCHR(K)
                  ELSE IF (JJ.EQ.NUMCOL+1) THEN
                     IF (ALTAX.LT.0) THEN
                        VELPIX = 0.0
                     ELSE
                        RCHAN = RES4(IEL)
                        IF (OPTICL) THEN
                           VELPIX = REFVEL + VELINC * (RCHAN-ALTRFP)
     *                        / (1.D0 + AXDEN * (RCHAN-ALTRFP))
                        ELSE
                           VELPIX = REFVEL + VELINC * (RCHAN-ALTRFP)
                           END IF
                        VELPIX = VELPIX / 1000.0
                        END IF
                     WRITE (MSGBUF,1103) VELPIX
                  ELSE
                     OFFSET = RES4(IEL) * SCALE(J)
                     IF ((JJ.EQ.COLPEK) .OR. (JJ.EQ.COEPEK)) OFFSET =
     *                  OFFSET * PBFACT * DLFACT
                     IF ((JJ.EQ.COLFLX) .OR. (JJ.EQ.COEFLX)) OFFSET =
     *                  OFFSET * PBFACT
                     WRITE (MSGBUF,1103) OFFSET
                     END IF
               ELSE IF (I.EQ.4) THEN
                  WRITE (MSGBUF,1104) RESLI(IEL)
               ELSE IF (I.EQ.5) THEN
                  WRITE (MSGBUF,1105) RESLO(IEL)
               ELSE IF (I.EQ.6) THEN
                  WRITE (MSGBUF,1104) RESULT(IEL)
                  END IF
               LINE(GCH(J):GCH(J)+NCH(JJ)-1) = MSGBUF(ICH(JJ):)
 130           CONTINUE
C                                       do output finally
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 970
 140        CONTINUE
 150     CONTINUE
      IF (JCOL2.LT.MCOL) GO TO 30
C                                       Close down
 200  CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      GO TO 999
C                                       errors
 960  IRET = 3
      WRITE (MSGTXT,1960) IERR
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      GO TO 995
 970  IRET = 0
      IF (IERR.LT.0) THEN
         CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
         IRET = -1
         GO TO 999
         END IF
      IRET = 2
      WRITE (MSGTXT,1970) IERR
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
C
 995  CALL MSGWRT (8)
      IF (IRET.EQ.0) IRET = 5
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1095 FORMAT ('*****__ MF file version = ',I5,' __ ',A12,'.',A6,'.',I5,
     *   ' __*****')
 1100 FORMAT (I6)
 1101 FORMAT (1PD13.6)
 1103 FORMAT (F10.3)
 1104 FORMAT (I12)
 1105 FORMAT (11X,L1)
 1120 FORMAT (64I1)
 1251 FORMAT (2(I2.2,':'),F9.6)
 1252 FORMAT (2(I2.2,':'),F6.3)
 1253 FORMAT (2(I2.2,':'),F4.1)
 1254 FORMAT (F14.8)
 1255 FORMAT (F14.5)
 1256 FORMAT (A1,2(I2.2,':'),F8.5)
 1257 FORMAT (A1,2(I2.2,':'),F5.2)
 1258 FORMAT (A1,2(I2.2,':'),F3.0)
 1960 FORMAT ('ERROR',I5,' READING TABLE DATA')
 1970 FORMAT ('ERROR',I5,' WRITING OUTPUT')
      END
      SUBROUTINE PRTSCL (KTY, ICH, NCH, RESULT, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   determines the number of characters required for each column
C   Outputs:
C      KTY      I(128)   type of column: 1-7 = dp, sp, ch, i, l, i
C                           11,12 = dp and sp time in D/HMS
C      ICH      I(128)   start character position; number characters in
C                        actual data for character and bit columns
C      NCH      I(128)   number of characters: assume formats
C                        D13.6, E10.3, I12, L12
C      RESULT   I(*)     buffer
C      SCRTCH   I(*)     buffer
C      IERR     I        TABIO error code
C-----------------------------------------------------------------------
      INTEGER   KTY(128), ICH(128), NCH(128), RESULT(*), SCRTCH(*), IERR
C
      INTEGER   LCH(7), I, J, LENGTH, IMX(128), IMN(128), LMN(128), K,
     *   IL, JTRIM, IC, IRNO
      CHARACTER STRING*24, TSTR*128
      REAL      SRES(128)
      DOUBLE PRECISION DRES(128)
      INCLUDE 'MFPRT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LCH /12, 9, 0, 1, 1, 1, 0/
C-----------------------------------------------------------------------
C                                       set non-integer lengths
      K = 0
      IERR = 0
      DO 20 J = 1,NCOL
C                                       basics
         I = MOD (DATP(J,2), 10)
         KTY(J) = I
         LENGTH = DATP(J,2) / 10
         ICH(J) = 1
         NCH(J) = LCH(I)
C                                       check header, units strings
         CALL TABIO ('READ', 3, J, RESULT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL H2CHR (24, 1, RESULT, STRING)
         IL = JTRIM (STRING)
         IF (IL.GT.9) IL = MIN (8, IL)
         CALL TABIO ('READ', 4, J, RESULT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL H2CHR (8, 1, RESULT, STRING)
         IC = JTRIM (STRING)
         IC = MAX (IC, IL, 2)
         LMN(J) = LENGTH
         IMN(J) = 2000000000
         IMX(J) = -IMN(J)
         SMN(J) = 1.E20
         SMX(J) = -SMN(J)
         IF (I.EQ.3) THEN
            ICH(J) = 1
            NCH(J) = 1
         ELSE IF (I.EQ.7) THEN
            NCH(J) = LENGTH
            ICH(J) = LENGTH
         ELSE
            IF (I.EQ.5) ICH(J) = 12 - IC/2
            END IF
         NCH(J) = MAX (IC, NCH(J))
 20      CONTINUE
C                                       find integer max/min(s)
C                                       find length of chars too
      DO 140 IRNO = BROW,EROW,IROW
C                                       get row
         CALL TABIO ('READ', 0, IRNO, SCRTCH, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 999
         IF (IERR.LT.0) GO TO 140
         DO 135 J = 1,NCOL
            K = DATP(J,1)
            LENGTH = LMN(J)
C                                       integer
            IF ((KTY(J).EQ.4) .OR. (KTY(J).EQ.6)) THEN
C                                       get data.
               CALL COPY (LENGTH, SCRTCH(K), RESULT)
C                                       get max/min
               DO 110 I = 1,LENGTH
                  IMN(J) = MIN (IMN(J), RESULT(I))
                  IMX(J) = MAX (IMX(J), RESULT(I))
 110              CONTINUE
C                                       characters
            ELSE IF (KTY(J).EQ.3) THEN
               LENGTH = LMN(J)
               K = DATP(J,1)
               CALL H2CHR (LENGTH, 1, SCRTCH(K), TSTR(:LENGTH))
               K = JTRIM (TSTR(:LENGTH))
               ICH(J) = MAX (K, ICH(J))
C                                       single float
            ELSE IF (KTY(J).EQ.2) THEN
C                                       get data.
               LENGTH = MIN (128, LENGTH)
               CALL COPY (LENGTH, SCRTCH(K), SRES)
C                                       get max/min
               DO 115 I = 1,LENGTH
                  IF (SRES(I).NE.FBLANK) THEN
                     SMN(J) = MIN (SMN(J), SRES(I))
                     SMX(J) = MAX (SMX(J), SRES(I))
                     END IF
 115              CONTINUE
C                                       double float
            ELSE IF (KTY(J).EQ.1) THEN
C                                       get data.
               LENGTH = MIN (128, LENGTH) * NWDPDP
               K = (K-1) * NWDPDP + 1
               CALL COPY (LENGTH, SCRTCH(K), DRES)
C                                       get max/min
               LENGTH = LENGTH / NWDPDP
               DO 120 I = 1,LENGTH
                  IF (DRES(I).NE.DBLANK) THEN
                     SRES(I) = DRES(I)
                     SMN(J) = MIN (SMN(J), SRES(I))
                     SMX(J) = MAX (SMX(J), SRES(I))
                     END IF
 120              CONTINUE
C                                       time
            ELSE IF (KTY(J).EQ.12) THEN
C                                       get data.
               LENGTH = MIN (128, LENGTH)
               CALL COPY (LENGTH, SCRTCH(K), SRES)
C                                       get max/min
               DO 125 I = 1,LENGTH
                  RESULT(I) = SRES(I)
                  IMN(J) = MIN (IMN(J), RESULT(I))
                  IMX(J) = MAX (IMX(J), RESULT(I))
 125              CONTINUE
C                                       time
            ELSE IF (KTY(J).EQ.11) THEN
C                                       get data.
               LENGTH = MIN (128, LENGTH) * NWDPDP
               K = (K-1) * NWDPDP + 1
               CALL COPY (LENGTH, SCRTCH(K), DRES)
C                                       get max/min
               LENGTH = LENGTH / NWDPDP
               DO 130 I = 1,LENGTH
                  RESULT(I) = DRES(I)
                  IMN(J) = MIN (IMN(J), RESULT(I))
                  IMX(J) = MAX (IMX(J), RESULT(I))
 130           CONTINUE
               END IF
 135        CONTINUE
 140     CONTINUE
C                                       to characters
      DO 150 J = 1,NCOL
         IF ((KTY(J).EQ.4) .OR. (KTY(J).EQ.6)) THEN
            IMN(J) = -10 * IMN(J)
            K = MAX (IMX(J), IMN(J))
            LENGTH = 1
            IF (K.GT.9) LENGTH = 2
            IF (K.GT.99) LENGTH = 3
            IF (K.GT.999) LENGTH = 4
            IF (K.GT.9999) LENGTH = 5
            IF (K.GT.99999) LENGTH = 6
            IF (K.GT.999999) LENGTH = 7
            IF (K.GT.9999999) LENGTH = 8
            IF (K.GT.99999999) LENGTH = 9
            IF (K.GT.999999999) LENGTH = 10
            IF (LENGTH.GE.NCH(J)) THEN
               NCH(J) = LENGTH
               ICH(J) = 13 - LENGTH
            ELSE
               ICH(J) = 12 - (NCH(J) + LENGTH - 1) / 2
               END IF
         ELSE IF (KTY(J).EQ.3) THEN
            NCH(J) = MAX (NCH(J), ICH(J))
         ELSE IF (KTY(J).LT.3) THEN
            IF (SMN(J).LT.0.0) THEN
               NCH(J) = MAX (NCH(J), LCH(KTY(J))+1)
            ELSE
               ICH(J) = 2
               END IF
         ELSE IF (KTY(J).GT.10) THEN
            IF (IMX(J).GT.0) ICH(J) = 3
            IF (IMX(J).GT.9) ICH(J) = 2
            IF (IMX(J).GT.99) ICH(J) = 1
            NCH(J) = MAX (NCH(J), 15-ICH(J))
            END IF
 150     CONTINUE
      IERR = 0
C
 999  RETURN
      END
      SUBROUTINE MFPLST (INVERX, IRET)
C-----------------------------------------------------------------------
C   MFPLST reads, formats, and prints a MF table as source list
C   Input:
C      INVERX I   MF version number currently being printed
C   Output:
C      IRET   I   Error code: 0 => OK or user terminates,
C                    2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   INVERX, IRET
C
      INCLUDE 'INCS:PMFC.INC'
      CHARACTER TITL1*132, TITL2*132, LINE*132, SCRTCH*132
      INTEGER   SCRBUF(NUMCOL), IRNO, RESLI(1), JJ, NREC, IERR, RTYPE,
     *   RESULT(NUMCOL), DEPTH(5), TABLUN, ICH(128), KTY(128), MCH(128),
     *   IFLUX, LRNO
      LOGICAL   RESLO(1), CHKSIZ
      REAL      RES4(1), LFLUX, BMAJ, BMIN, DX, DY, FSCALE, PBFACT
      HOLLERITH RESH(1)
      DOUBLE PRECISION    RES8(1), RA, DEC, XX, YY, ZZ
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'MFPRT.INC'
      EQUIVALENCE (RESULT, RES8, RES4, RESH, RESLI, RESLO)
      DATA DEPTH /5*1/
C-----------------------------------------------------------------------
      IRET = 0
      CHKSIZ = CSIZE(2).GT.0.0
C                                       Open table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      TABLUN = 27
      CALL TABINI ('READ', INEXT, INDISK, CNO, INVERX, CATBLK, TABLUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1010) IERR, INEXT, INVERX
         IF (IERR.NE.2) GO TO 995
         GO TO 999
         END IF
      BROW = 1
      EROW = BUFFER(5)
      IROW = 1
C                                       init counters, line size
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
      NLINE = 999
C                                       check max min
      CALL PRTSCL (KTY, ICH, MCH, RESULT, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      FSCALE = 1.E3
      IF ((SMX(3).LT.1.0) .AND. (ABS(SMN(3)).LT.0.001)) THEN
         FSCALE = 1.E6
         MSGTXT = 'WARNING: FLUXES IN MICROJY, NOT USUAL MILLIJY'
         CALL MSGWRT (8)
         END IF
C                                       Loop: list all columns
      WRITE (TITL1,1000)
      WRITE (TITL2,1001)
      IF (FSCALE.EQ.1.E6) TITL2(32:32) = 'u'
      WRITE (LINE,1005) INNAM, INCLS, INSEQ
      CALL REFRMT (LINE, '_', JJ)
      CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *   LINE, NLINE, IPAGE, SCRTCH, IERR)
      LINE = ' '
C                                       Output loop
      LRNO = 0
      PBFACT = 1.0
      DO 150 IRNO = BROW,EROW,IROW
         IF ((IDOALL.EQ.1) .OR. (IDOALL.EQ.3)) THEN
            JJ = COBMFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            PBFACT = RES4(1)
            END IF
C                                       flux level
         JJ = COLFLX
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         LFLUX = RES4(1) * PBFACT
         IF (LFLUX.LT.FLUX) GO TO 150
         IFLUX = LFLUX * FSCALE + 0.5
         IFLUX = MIN (IFLUX, 9999999)
C                                       component size
         JJ = COLMJX
         IF (OPTYPE.EQ.'LIDE') JJ = COLD0J
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         BMAJ = RES4(1)
         JJ = COLMNX
         IF (OPTYPE.EQ.'LIDE') JJ = COLD0N
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         BMIN = RES4(1)
         IF (CHKSIZ) THEN
            IF (MIN(BMAJ,BMIN)*ASCALE.LT.CSIZE(1)) GO TO 150
            IF (MAX(BMAJ,BMIN)*ASCALE.GT.CSIZE(2)) GO TO 150
            END IF
         BMAJ = SQRT (BMAJ * BMIN) * 3600.0
C                                       offsets
         JJ = COLDLX
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         DX = RPLOC(1,LOCNUM) + RES4(1) / AXINC(1,LOCNUM)
         JJ = COLDLY
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         DY = RPLOC(2,LOCNUM) + RES4(1) / AXINC(2,LOCNUM)
C                                       offsets to coords
         CALL XYVAL (DX, DY, XX, YY, ZZ, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'ERROR IN COORDINATE'
            CALL MSGWRT (7)
         ELSE IF (CORTYP(LOCNUM).EQ.1) THEN
            RA = XX
            DEC = YY
         ELSE
            RA = YY
            DEC = XX
            END IF
         WRITE (LINE,1100) RA, DEC, IFLUX, BMAJ
C                                       do output finally
         IF (DOCRT.LE.0.0) THEN
            NLINE = 1
            IPAGE = 1
            END IF
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
 150     CONTINUE
C                                       Close down
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      GO TO 999
C                                       errors
 960  IRET = 3
      WRITE (MSGTXT,1960) IERR
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      GO TO 995
 970  IRET = 0
      IF (IERR.LT.0) THEN
         CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
         IRET = -1
         GO TO 999
         END IF
      IRET = 2
      WRITE (MSGTXT,1970) IERR
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
C
 995  CALL MSGWRT (8)
      IF (IRET.EQ.0) IRET = 5
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('; RA',12X,'DEC',11X,'Flux    Width')
 1001 FORMAT ('; deg',11X,'deg',11X,' mJy     asec')
 1005 FORMAT ('*****__ Data file ',A12,'.',A6,'.',I5,' __*****')
 1010 FORMAT ('ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1100 FORMAT (F12.8,2X,F12.8,I8,F11.4)
 1960 FORMAT ('ERROR',I5,' READING TABLE DATA')
 1970 FORMAT ('ERROR',I5,' WRITING OUTPUT')
      END
      SUBROUTINE MFPSTR (INVERX, IRET)
C-----------------------------------------------------------------------
C   MFPSTR reads, formats, and prints a MF table extension file in
C   STARS format.
C   Input:
C      INVERX   I   MF version number currently being printed
C   Output:
C      IRET     I   Error code: 0 => OK or user terminates,
C                      2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   INVERX, IRET
C
      INCLUDE 'INCS:PMFC.INC'
      CHARACTER TITL1*132, TITL2*132, LINE*132, SCRTCH*132, DSIGN*1,
     *   RSIGN*1, KEYWRD(5)*8
      INTEGER   SCRBUF(NUMCOL), IRNO, RESLI(1), JJ, IOFF, ISYM,
     *   RESULT(NUMCOL), RTYPE, IERR, LRNO, DHMS(2), RHMS(2), NREC,
     *   TABLUN, COLIDX(9), DEPTH(5), NUMKEY, LOCS(5), KEYTYP(5)
      LOGICAL   RESLO(1), CHKSIZ, FIRST
      REAL      RES4(1), RSEC, BMAJ, BMIN, SCALE(5), DX, DY, CFLUX,
     *   DSEC, BPA, PBFACT
      HOLLERITH RESH(1)
      DOUBLE PRECISION RES8(1), RA, DEC, XX, YY, ZZ
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'MFPRT.INC'
      EQUIVALENCE (RESULT, RES8, RES4, RESH, RESLI, RESLO)
C                                       Column in table corresponding
C                                       to column in output
      DATA COLIDX /COLFLX, COLDLX, COLDLY, COLMJX, COLMNX, COLPAN,
     *   COLD0J, COLD0N, COLD0P/
C-----------------------------------------------------------------------
      IRET = 0
      FIRST = .TRUE.
      ISYM = SYMBOL + 0.01
      ISYM = MAX (1, MIN (23, ISYM))
      IOFF = 0
      IF (OPTYPE.EQ.'STDE') IOFF = 3
C                                       Open table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      TABLUN = 27
      CALL TABINI ('READ', INEXT, INDISK, CNO, INVERX, CATBLK, TABLUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1010) IERR, INEXT, INVERX
         IF (IERR.NE.2) GO TO 995
         GO TO 999
         END IF
      BROW = 1
      EROW = BUFFER(5)
      IF (EROW.LT.BROW) GO TO 200
      IROW = 1
C                                       stuff for coordinates
      NUMKEY = 5
      KEYWRD(1) = 'DEPTH1'
      KEYWRD(2) = 'DEPTH2'
      KEYWRD(3) = 'DEPTH3'
      KEYWRD(4) = 'DEPTH4'
      KEYWRD(5) = 'DEPTH5'
      CALL TABKEY ('READ', KEYWRD, NUMKEY, BUFFER, LOCS, DEPTH,
     *   KEYTYP, IERR)
C                                       Velocity calculation
      IF (IERR.NE.0) CALL FILL (5, 1, DEPTH)
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
      CALL RFILL (5, 1.0, SCALE)
      IF (AXTYP(LOCNUM).NE.1) THEN
         DOHMS = -1.0
      ELSE
C         DOHMS = 1.0
         SCALE(3) = 3.6E3
         SCALE(4) = 3.6E3
         END IF
C                                       init counters, line size
      CHKSIZ = CSIZE(2).GT.0.0
C                                       first time
      NLINE = 0
      CRTMAX = 100000
      PRTMAX = 100000
C                                       List column numbers
      TITL1 = ' '
      TITL2 = ' '
      LINE = ' '
C                                       Output loop
      LRNO = 0
      PBFACT = 1.0
      DO 150 IRNO = BROW,EROW,IROW
         IF ((IDOALL.EQ.1) .OR. (IDOALL.EQ.3)) THEN
            JJ = COBMFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            PBFACT = RES4(1)
            END IF
C                                       Check flux level
         JJ = COLFLX
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         CFLUX = RES4(1)
         CFLUX = CFLUX * PBFACT
         IF (CFLUX.LT.FLUX) GO TO 150
         IF (SYMBOL.LT.0.0) THEN
            ISYM = CFLUX / (-SYMBOL) + 1
            ISYM = MAX (1, MIN (23, ISYM))
            END IF
C                                       Component
         JJ = COLIDX(4+IOFF)
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         BMAJ = RES4(1)
         JJ = COLIDX(5+IOFF)
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         BMIN = RES4(1)
         JJ = COLIDX(6+IOFF)
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         BPA = RES4(1)
C                                       Check component size
         IF (CHKSIZ) THEN
            IF (MIN(BMAJ,BMIN)*ASCALE.LT.CSIZE(1)) GO TO 150
            IF (MAX(BMAJ,BMIN)*ASCALE.GT.CSIZE(2)) GO TO 150
            END IF
C                                       scale
         IF (CLEV.GT.0.0) THEN
            BMAJ = BMAJ * CFLUX / CLEV
            BMIN = BMIN * CFLUX / CLEV
            END IF
C                                       RA, Dec
         JJ = COLIDX(2)
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         DX = RPLOC(1,LOCNUM) + RES4(1) / AXINC(1,LOCNUM)
         JJ = COLIDX(3)
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         DY = RPLOC(2,LOCNUM) + RES4(1) / AXINC(2,LOCNUM)
C                                       offsets to coords
         CALL XYVAL (DX, DY, XX, YY, ZZ, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'ERROR IN COORDINATE'
            CALL MSGWRT (7)
            END IF
C                                       sexagesimal
         BMAJ = BMAJ * SCALE(3)
         BMIN = BMIN * SCALE(4)
         IF (DOHMS.GT.0.0) THEN
            IF (CORTYP(LOCNUM).EQ.1) THEN
               RA = XX
               DEC = YY
            ELSE
               RA = YY
               DEC = XX
               END IF
            CALL COORDD (1, RA, RSIGN, RHMS, RSEC)
            CALL COORDD (2, DEC, DSIGN, DHMS, DSEC)
            IF (CORTYP(LOCNUM).EQ.1) THEN
               WRITE (LINE,1100) RHMS, RSEC, DSIGN, DHMS, DSEC, BMAJ,
     *            BMIN, BPA, ISYM
               IF (LINE(7:7).EQ.' ') LINE(7:7) = '0'
               IF (LINE(24:24).EQ.' ') LINE(24:24) = '0'
            ELSE
               WRITE (LINE,1101) DSIGN, DHMS, RHMS, RSEC, DSEC, BMAJ,
     *            BMIN, BPA, ISYM
               IF (LINE(8:8).EQ.' ') LINE(8:8) = '0'
               IF (LINE(23:23).EQ.' ') LINE(23:23) = '0'
               END IF
         ELSE IF (AXTYP(LOCNUM).EQ.1) THEN
            IF (DOHMS.GT.-1.5) THEN
               WRITE (LINE,1102) XX, YY, BMAJ, BMIN, BPA, ISYM
            ELSE IF (CORTYP(LOCNUM).EQ.1) THEN
               XX = (XX - RPVAL(1,LOCNUM)) *
     *            COS (DG2RAD*RPVAL(2,LOCNUM))
               YY = YY - RPVAL(2,LOCNUM)
               XX = XX * 3600.0D0
               YY = YY * 3600.0D0
               WRITE (LINE,1103) XX, YY, BMAJ, BMIN, BPA, ISYM
            ELSE
               YY = (YY - RPVAL(2,LOCNUM)) *
     *            COS (DG2RAD*RPVAL(1,LOCNUM))
               XX = XX - RPVAL(1,LOCNUM)
               XX = XX * 3600.0D0
               YY = YY * 3600.0D0
               WRITE (LINE,1103) XX, YY, BMAJ, BMIN, BPA, ISYM
               END IF
         ELSE
            WRITE (LINE,1104) XX, YY, BMAJ, BMIN, BPA, ISYM
            END IF
C                                       do output finally
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
 150     CONTINUE
C                                       Close down
 200  CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      GO TO 999
C                                       errors
 960  IRET = 3
      WRITE (MSGTXT,1960) IERR
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      GO TO 995
C
 970  IF (IERR.LT.0) THEN
         CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
         IRET = -1
         GO TO 999
         END IF
      IRET = 2
      WRITE (MSGTXT,1970) IERR
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
C
 995  CALL MSGWRT (8)
      IF (IRET.EQ.0) IRET = 5
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1100 FORMAT (I2.2,I3.2,F10.6,1X,A1,I2.2,I3.2,F9.5,2F12.5,F8.2,I3)
 1101 FORMAT (A1,I2.2,I3.2,F9.5,1X,I2.2,I3.2,F10.6,2F12.5,F8.2,I3)
 1102 FORMAT (2F14.8,2F12.5,F8.2,I3)
 1103 FORMAT (2F14.5,2F12.5,F8.2,I3)
 1104 FORMAT (4(1PE13.6),0PF8.2,I3)
 1960 FORMAT ('ERROR',I5,' READING TABLE DATA')
 1970 FORMAT ('ERROR',I5,' WRITING OUTPUT')
      END
      SUBROUTINE CHKLST (INVERX, NCOUNT, IRET)
C-----------------------------------------------------------------------
C   MFPLST reads, formats, and prints a MF table as source list
C   Input:
C      INVERX   I   MF version number currently being printed
C   Output:
C      NCOUNT   I   Count of lines of print
C      IRET     I   Error code: 0 => OK or user terminates,
C                      2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   INVERX, NCOUNT, IRET
C
      INCLUDE 'INCS:PMFC.INC'
      INTEGER   SCRBUF(NUMCOL), IRNO, RESLI(1), JJ, NREC, IERR, RTYPE,
     *   RESULT(NUMCOL), DEPTH(5), TABLUN, ICH(128), KTY(128), MCH(128),
     *   IFLUX, LRNO
      LOGICAL   RESLO(1), CHKSIZ
      REAL      RES4(1), LFLUX, BMAJ, BMIN, FSCALE, PBFACT
      HOLLERITH RESH(1)
      DOUBLE PRECISION RES8(1)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'MFPRT.INC'
      EQUIVALENCE (RESULT, RES8, RES4, RESH, RESLI, RESLO)
      DATA DEPTH /5*1/
C-----------------------------------------------------------------------
      IRET = 0
      CHKSIZ = CSIZE(2).GT.0.0
C                                       Open table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      TABLUN = 27
      CALL TABINI ('READ', INEXT, INDISK, CNO, INVERX, CATBLK, TABLUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1010) IERR, INEXT, INVERX
         IF (IERR.NE.2) GO TO 995
         GO TO 999
         END IF
      BROW = 1
      EROW = BUFFER(5)
      IROW = 1
C                                       init counters, line size
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
C                                       check max min
      CALL PRTSCL (KTY, ICH, MCH, RESULT, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      FSCALE = 1.E3
      IF ((SMX(3).LT.1.0) .AND. (ABS(SMN(3)).LT.0.001)) THEN
         FSCALE = 1.E6
         END IF
C                                       Loop: list all columns
      NCOUNT = NCOUNT + 1
C                                       Output loop
      LRNO = 0
      PBFACT = 1.0
      DO 150 IRNO = BROW,EROW,IROW
         IF ((IDOALL.EQ.1) .OR. (IDOALL.EQ.3)) THEN
            JJ = COBMFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            PBFACT = RES4(1)
            END IF
C                                       flux level
         JJ = COLFLX
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         LFLUX = RES4(1) * PBFACT
         IF (LFLUX.LT.FLUX) GO TO 150
         IFLUX = LFLUX * FSCALE + 0.5
         IFLUX = MIN (IFLUX, 9999999)
C                                       component size
         JJ = COLMJX
         IF (OPTYPE.EQ.'LIDE') JJ = COLD0J
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         BMAJ = RES4(1)
         JJ = COLMNX
         IF (OPTYPE.EQ.'LIDE') JJ = COLD0N
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         BMIN = RES4(1)
         IF (CHKSIZ) THEN
            IF (MIN(BMAJ,BMIN)*ASCALE.LT.CSIZE(1)) GO TO 150
            IF (MAX(BMAJ,BMIN)*ASCALE.GT.CSIZE(2)) GO TO 150
            END IF
         BMAJ = SQRT (BMAJ * BMIN) * 3600.0
C                                       do output finally
         NCOUNT = NCOUNT + 1
 150     CONTINUE
C                                       Close down
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      GO TO 999
C                                       errors
 960  IRET = 3
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      WRITE (MSGTXT,1960) IERR
C
 995  CALL MSGWRT (8)
      IF (IRET.EQ.0) IRET = 5
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CHKLST ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1960 FORMAT ('CHKLST ERROR',I5,' READING TABLE DATA')
      END
      SUBROUTINE CHKSTR (INVERX, NCOUNT, IRET)
C-----------------------------------------------------------------------
C   MFPSTR reads, formats, and prints a MF table extension file in
C   STARS format.
C   Input:
C      INVERX   I   MF version number currently being printed
C   Output:
C      NCOUNT   I   Count of lines of print
C      IRET     I   Error code: 0 => OK or user terminates,
C                      2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   INVERX, NCOUNT, IRET
C
      INCLUDE 'INCS:PMFC.INC'
      INTEGER   SCRBUF(NUMCOL), IRNO, RESLI(1), JJ, IOFF, RTYPE, IERR,
     *   RESULT(NUMCOL), LRNO, NREC, TABLUN, COLIDX(9),
     *   ISYM
      LOGICAL   RESLO(1), CHKSIZ
      REAL      RES4(1), BMAJ, BMIN, SCALE(5), CFLUX, BPA, PBFACT
      HOLLERITH RESH(1)
      DOUBLE PRECISION RES8(1)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'MFPRT.INC'
      EQUIVALENCE (RESULT, RES8, RES4, RESH, RESLI, RESLO)
C                                       Column in table corresponding
C                                       to column in output
      DATA COLIDX /COLFLX, COLDLX, COLDLY, COLMJX, COLMNX, COLPAN,
     *   COLD0J, COLD0N, COLD0P/
C-----------------------------------------------------------------------
      IRET = 0
      IOFF = 0
      IF (OPTYPE.EQ.'STDE') IOFF = 3
C                                       Open table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      TABLUN = 27
      CALL TABINI ('READ', INEXT, INDISK, CNO, INVERX, CATBLK, TABLUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1010) IERR, INEXT, INVERX
         IF (IERR.NE.2) GO TO 995
         GO TO 999
         END IF
      BROW = 1
      EROW = BUFFER(5)
      IF (EROW.LT.BROW) GO TO 200
      IROW = 1
      CALL RFILL (5, 1.0, SCALE)
C                                       init counters, line size
      CHKSIZ = CSIZE(2).GT.0.0
C                                       Output loop
      LRNO = 0
      PBFACT = 1.0
      DO 150 IRNO = BROW,EROW,IROW
         IF ((IDOALL.EQ.1) .OR. (IDOALL.EQ.3)) THEN
            JJ = COBMFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            PBFACT = RES4(1)
            END IF
C                                       Check flux level
         JJ = COLFLX
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         CFLUX = RES4(1)
         CFLUX = CFLUX * PBFACT
         IF (CFLUX.LT.FLUX) GO TO 150
         IF (SYMBOL.LT.0.0) THEN
            ISYM = CFLUX / (-SYMBOL) + 1
            ISYM = MAX (1, MIN (23, ISYM))
            END IF
C                                       Component
         JJ = COLIDX(4+IOFF)
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         BMAJ = RES4(1)
         JJ = COLIDX(5+IOFF)
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         BMIN = RES4(1)
         JJ = COLIDX(6+IOFF)
         CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *      SCRBUF, IERR)
         IF (IERR.LT.0) GO TO 150
         IF (IERR.NE.0) GO TO 960
         BPA = RES4(1)
C                                       Check component size
         IF (CHKSIZ) THEN
            IF (MIN(BMAJ,BMIN)*ASCALE.LT.CSIZE(1)) GO TO 150
            IF (MAX(BMAJ,BMIN)*ASCALE.GT.CSIZE(2)) GO TO 150
            END IF
C                                       do output finally
         NCOUNT = NCOUNT + 1
 150     CONTINUE
C                                       Close down
 200  CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      GO TO 999
C                                       errors
 960  IRET = 3
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      WRITE (MSGTXT,1960) IERR
C
 995  CALL MSGWRT (8)
      IF (IRET.EQ.0) IRET = 5
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CHKSTR ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1960 FORMAT ('CHKSTR ERROR',I5,' READING TABLE DATA')
      END
      SUBROUTINE CHKUSR (INVERX, NCOUNT, IRET)
C-----------------------------------------------------------------------
C   MFPUSR reads, formats, and prints a table extension file for a list
C   of columns selected by the user
C   Input:
C      INVERX   I   MF version number currently being printed
C   Output:
C      NCOUNT   I   Count of lines of print
C      IRET     I   Error code: 0 => OK or user terminates,
C                      2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   INVERX, NCOUNT, IRET
C
      INCLUDE 'INCS:PMFC.INC'
      INTEGER   SCRBUF(NUMCOL), IRNO, RESLI(1), JCOL1, JCOL2, I, J, N,
     *   NCH(128), JJ, ICH(128), RESULT(NUMCOL), RTYPE, IERR, IEL, LRNO,
     *   LENGTH, MAXLEN, KTY(128), GCH(128), NREC, TABLUN, MCH(128),
     *   COLIDX(NUMCOL+1), CLEN(3), IROUND, MCOL, TYPCOL(NUMCOL+1)
      LOGICAL   RESLO(1), CHKSIZ, FIRST, OKLIN
      REAL      RES4(1), BMAJ, BMIN, SCALE(NUMCOL+1), FSCALE, PBFACT,
     *   DLFACT
      HOLLERITH RESH(1)
      DOUBLE PRECISION    RES8(1)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'MFPRT.INC'
      EQUIVALENCE (RESULT, RES8, RES4, RESH, RESLI, RESLO)
      DATA CLEN /15, 12, 10/
      DATA TYPCOL /1,2,2,4,4, 3,3,1,2,2, 2,2,2,3,3, 3,3,1,2,2,
     *   2,1,3,3,1, 3,3,1,3,3, 1,2,2,2,1, 1,1,1,1,1, 1,1/
C-----------------------------------------------------------------------
      IRET = 0
      FIRST = .TRUE.
C                                       Open table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      TABLUN = 27
      CALL TABINI ('READ', INEXT, INDISK, CNO, INVERX, CATBLK, TABLUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1010) IERR, INEXT, INVERX
         IF (IERR.NE.2) GO TO 995
         GO TO 999
         END IF
      BROW = 1
      EROW = BUFFER(5)
      IF (EROW.LT.BROW) GO TO 200
      IROW = 1
      MCOL = 0
      OKLIN = FRMT.GT.-0.5
      DO 5 J = 1,NUMCOL+1
         N = IROUND (UCOLS(J))
         IF (N.LE.0) THEN
            GO TO 10
         ELSE
            MCOL = MCOL + 1
            COLIDX(MCOL) = N
            END IF
 5       CONTINUE
 10   IF (MCOL.LE.0) THEN
         DO 15 MCOL = 1,NUMCOL+1
            COLIDX(MCOL) = MCOL
 15         CONTINUE
         MCOL = NUMCOL+1
         END IF
C                                       init counters, line size
      JCOL2 = 0
      CHKSIZ = CSIZE(2).GT.0.0
      CALL PRTSCL (KTY, ICH, MCH, RESULT, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      CALL COPY (128, MCH, NCH)
      ICH(NUMCOL+1) = ICH(COLPLN)
      NCH(NUMCOL+1) = NCH(COLPLN)
      CALL RFILL (NUMCOL+1, 1.0, SCALE)
      FSCALE = 1.0
      IF (MAX(ABS(SMX(2)), ABS(SMN(2))).LT.10.) FSCALE = 1000.0
      IF (MAX(ABS(SMX(2)), ABS(SMN(2))).LT.0.010) FSCALE = 1.0E6
      DO 20 J = 1,MCOL
         N = TYPCOL(COLIDX(J))
         IF (N.EQ.2) THEN
            SCALE(J) = FSCALE
         ELSE IF (N.EQ.3) THEN
            SCALE(J) = ASCALE
         ELSE IF (N.EQ.4) THEN
            IF (DOHMS.GT.0.0) THEN
               NCH(COLIDX(J)) = CLEN(ISCALE)
               ICH(COLIDX(J)) = 1
            ELSE
               SCALE(J) = ASCALE
               END IF
            END IF
 20      CONTINUE
C                                       Loop: list all columns to fit
 30   JCOL1 = JCOL2 + 1
      MAXLEN = 1
C                                       how many columns this pass
      N = 0
      IF (OKLIN) N = 8
      DO 65 J = JCOL1,MCOL
         I = MOD (DATP(COLIDX(J),2), 10)
         LENGTH = DATP(COLIDX(J),2) / 10
         NCH(COLIDX(J)) = MIN (NCH(COLIDX(J)), NACROS-9)
         N = N + NCH(COLIDX(J)) + 2
         IF (N.LE.NACROS) JCOL2 = J
         IF (N.GT.NACROS) GO TO 70
         IF ((I.NE.3) .AND. (I.NE.7)) MAXLEN = MAX (MAXLEN, LENGTH)
 65      CONTINUE
C                                       Blanks between
 70   IF (JCOL2.LT.JCOL1) JCOL2 = JCOL1
      N = 0
      IF (OKLIN) N = 8
      DO 75 J = JCOL1,JCOL2
         N = N + NCH(COLIDX(J))
 75      CONTINUE
      N = (NACROS - N) / (JCOL2 - JCOL1 + 1)
      N = MIN (N, 6)
      GCH(JCOL1) = 1
      IF (OKLIN) GCH(JCOL1) = 9
      IF (JCOL1.LT.JCOL2) THEN
         I = JCOL1 + 1
         DO 80 J = I,JCOL2
            GCH(J) = GCH(J-1) + NCH(COLIDX(J-1)) + N
 80         CONTINUE
         END IF
C                                       List column numbers
      IF (FIRST) THEN
         FIRST = .FALSE.
         NCOUNT = NCOUNT + 1
         END IF
      IF (DOCRT.LE.-2.5) NCOUNT = NCOUNT + 2
      IF ((DOCRT.LE.0.0) .AND. (DOCRT.GT.-2.5)) NCOUNT = NCOUNT + 2
C                                       Output loop
      LRNO = 0
      PBFACT = 1.0
      DLFACT = 1.0
      DO 150 IRNO = BROW,EROW,IROW
C                                       scaling
         IF (IDOALL.GE.2) THEN
            JJ = CODLFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            DLFACT = RES4(1)
            END IF
         IF ((IDOALL.EQ.1) .OR. (IDOALL.EQ.3)) THEN
            JJ = COBMFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            PBFACT = RES4(1)
            END IF
C                                       Check flux level
         IF (FLUX.NE.0.0) THEN
            JJ = COLFLX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            IF (PBFACT*RES4(1).LT.FLUX) GO TO 150
            END IF
C                                       Check component size
         IF (CHKSIZ) THEN
            JJ = COLMJX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            BMAJ = RES4(1)
            JJ = COLMNX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            BMIN = RES4(1)
            IF (MIN(BMAJ,BMIN)*ASCALE.LT.CSIZE(1)) GO TO 150
            IF (MAX(BMAJ,BMIN)*ASCALE.GT.CSIZE(2)) GO TO 150
            END IF

C                                       Loop over element in arrays.
         DO 140 IEL = 1,MAXLEN
C                                       do output finally
            NCOUNT = NCOUNT + 1
 140        CONTINUE
 150     CONTINUE
      IF (JCOL2.LT.MCOL) GO TO 30
C                                       Close down
 200  CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      GO TO 999
C                                       errors
 960  IRET = 3
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      WRITE (MSGTXT,1960) IERR
C
 995  CALL MSGWRT (8)
      IF (IRET.EQ.0) IRET = 5
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CHKUSR ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1960 FORMAT ('CHKUSR ERROR',I5,' READING TABLE DATA')
      END
      SUBROUTINE CHKMC1 (INVERX, NCOUNT, IRET)
C-----------------------------------------------------------------------
C   MFPMC1 reads, formats, and prints a table extension file
C   Input:
C      INVERX   I   MF version number currently being printed
C   Output:
C      NCOUNT   I   Count of lines of print
C      IRET     I   Error code: 0 => OK or user terminates,
C                      2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   INVERX, NCOUNT, IRET
C                                       MAXKEY=max. no. keyword-values
      INTEGER   MAXKEY
      PARAMETER (MAXKEY=7)
C
      INCLUDE 'INCS:PMFC.INC'
      INTEGER   SCRBUF(NUMCOL), IRNO, RESLI(1), JCOL1, JCOL2, I, J, N,
     *   NCH(128), JJ, ICH(128), RESULT(NUMCOL), RTYPE, IERR, IEL, LRNO,
     *   LENGTH, MAXLEN, MCOL, KTY(128), ALTAX, GCH(128),NREC, TABLUN,
     *   MCH(128), COLIDX(11), CLEN(3)
      LOGICAL   RESLO(1), CHKSIZ, FIRST, OKLIN
      REAL      RES4(1), BMAJ, BMIN, DLFACT, PBFACT
      HOLLERITH RESH(1)
      DOUBLE PRECISION RES8(1)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'MFPRT.INC'
      EQUIVALENCE (RESULT, RES8, RES4, RESH, RESLI, RESLO)
      DATA CLEN /15, 12, 10/
C                                       Column in table corresponding
C                                       to column in output
      DATA COLIDX /COLPLN, COLPEK, COLDLX, COLDLY, COLMJX, COLMNX,
     *   COLPAN, COLD0J, COLD0N, COLD0P, COLPLN/
C-----------------------------------------------------------------------
      IRET = 0
      FIRST = .TRUE.
      OKLIN = FRMT.GT.-0.5
C                                       Open table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      TABLUN = 27
      CALL TABINI ('READ', INEXT, INDISK, CNO, INVERX, CATBLK, TABLUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1010) IERR, INEXT, INVERX
         IF (IERR.NE.2) GO TO 995
         GO TO 999
         END IF
      BROW = 1
      EROW = BUFFER(5)
      IF (EROW.LT.BROW) GO TO 200
      IROW = 1
C                                       init counters, line size
      JCOL2 = 0
      CHKSIZ = CSIZE(2).GT.0.0
      CALL PRTSCL (KTY, ICH, MCH, RESULT, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      CALL COPY (128, MCH, NCH)
      IF (DOHMS.GT.0.0) THEN
         NCH(COLIDX(3)) = CLEN(ISCALE)
         NCH(COLIDX(4)) = CLEN(ISCALE)
         ICH(COLIDX(3)) = 1
         ICH(COLIDX(4)) = 1
         END IF
      ALTAX = CATBLK(KIALT)
      MCOL = 11
      IF (ALTAX.LE.0) MCOL = 10
C                                       Loop: list all columns to fit
 10   JCOL1 = JCOL2 + 1
      MAXLEN = 1
C                                       how many columns this pass
      N = 0
      IF (OKLIN) N = 8
      DO 15 J = JCOL1,MCOL
         I = MOD (DATP(COLIDX(J),2), 10)
         LENGTH = DATP(COLIDX(J),2) / 10
         NCH(COLIDX(J)) = MIN (NCH(COLIDX(J)), NACROS-9)
         N = N + NCH(COLIDX(J)) + 2
         IF (N.LE.NACROS) JCOL2 = J
         IF (N.GT.NACROS) GO TO 20
         IF ((I.NE.3) .AND. (I.NE.7)) MAXLEN = MAX (MAXLEN, LENGTH)
 15      CONTINUE
C                                       Blanks between
 20   IF (JCOL2.LT.JCOL1) JCOL2 = JCOL1
      N = 0
      IF (OKLIN) N = 8
      DO 25 J = JCOL1,JCOL2
         N = N + NCH(COLIDX(J))
 25      CONTINUE
      N = (NACROS - N) / (JCOL2 - JCOL1 + 1)
      N = MIN (N, 6)
      GCH(JCOL1) = 1
      IF (OKLIN) GCH(JCOL1) = 9
      IF (JCOL1.LT.JCOL2) THEN
         I = JCOL1 + 1
         DO 30 J = I,JCOL2
            GCH(J) = GCH(J-1) + NCH(COLIDX(J-1)) + N
 30         CONTINUE
         END IF
C                                       Output the lines
      IF (FIRST) THEN
         FIRST = .FALSE.
         NCOUNT = NCOUNT + 1
         END IF
      IF (DOCRT.LE.-2.5) NCOUNT = NCOUNT + 2
      IF ((DOCRT.LE.0.0) .AND. (DOCRT.GT.-2.5)) NCOUNT = NCOUNT + 1
C                                       Output loop
      LRNO = 0
      DLFACT = 1.0
      PBFACT = 1.0
      DO 150 IRNO = BROW,EROW,IROW
         IF ((IDOALL.EQ.1) .OR. (IDOALL.EQ.3)) THEN
            JJ = COBMFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            PBFACT = RES4(1)
            END IF
C                                       Check flux level
         IF (FLUX.NE.0.0) THEN
            JJ = COLFLX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            IF (PBFACT*RES4(1).LT.FLUX) GO TO 150
            END IF
C                                       Check component size
         IF (CHKSIZ) THEN
            JJ = COLMJX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            BMAJ = RES4(1)
            JJ = COLMNX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            BMIN = RES4(1)
            IF (MIN(BMAJ,BMIN)*ASCALE.LT.CSIZE(1)) GO TO 150
            IF (MAX(BMAJ,BMIN)*ASCALE.GT.CSIZE(2)) GO TO 150
            END IF
C                                       scaling
         IF (IDOALL.GE.2) THEN
            JJ = CODLFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            DLFACT = RES4(1)
            END IF
C                                       Loop over element in arrays.
         DO 140 IEL = 1,MAXLEN
C                                       do output finally
            NCOUNT = NCOUNT + 1
 140        CONTINUE
 150     CONTINUE
      IF (JCOL2.LT.10) GO TO 10
C                                       Close down
 200  CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      GO TO 999
C                                       errors
 960  IRET = 3
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      WRITE (MSGTXT,1960) IERR
C
 995  CALL MSGWRT (8)
      IF (IRET.EQ.0) IRET = 5
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CHKMC1 ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1960 FORMAT ('CHKMC1 ERROR',I5,' READING TABLE DATA')
      END
      SUBROUTINE CHK1C1 (INVERX, NCOUNT, IRET)
C-----------------------------------------------------------------------
C   MFP1C1 reads, formats, and prints a table extension file for
C   single-channel images
C   Input:
C      INVERX   I   MF version number currently being printed
C   Output:
C      NCOUNT   I   Count of lines of print
C      IRET     I   Error code: 0 => OK or user terminates,
C                      2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   INVERX, NCOUNT, IRET
C
      INCLUDE 'INCS:PMFC.INC'
      INTEGER   SCRBUF(NUMCOL), IRNO, RESLI(1), JCOL1, JCOL2, I, J, N,
     *   NCH(128), JJ, ICH(128), RESULT(NUMCOL), RTYPE, IERR, IEL, LRNO,
     *   LENGTH, MAXLEN, KTY(128), GCH(128), NREC, TABLUN, MCH(128),
     *   COLIDX(10), CLEN(3)
      LOGICAL   RESLO(1), CHKSIZ, FIRST, OKLIN
      REAL      RES4(1), BMAJ, BMIN, PBFACT, DLFACT
      HOLLERITH RESH(1)
      DOUBLE PRECISION    RES8(1)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'MFPRT.INC'
      EQUIVALENCE (RESULT, RES8, RES4, RESH, RESLI, RESLO)
      DATA CLEN /15, 12, 10/
C                                       Column in table corresponding
C                                       to column in output
      DATA COLIDX /COLPEK, COLFLX, COLDLX, COLDLY, COLMJX, COLMNX,
     *   COLPAN, COLD0J, COLD0N, COLD0P/
C-----------------------------------------------------------------------
      IRET = 0
      FIRST = .TRUE.
      OKLIN = FRMT.GT.-0.5
C                                       Open table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      TABLUN = 27
      CALL TABINI ('READ', INEXT, INDISK, CNO, INVERX, CATBLK, TABLUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1010) IERR, INEXT, INVERX
         IF (IERR.NE.2) GO TO 995
         GO TO 999
         END IF
      BROW = 1
      EROW = BUFFER(5)
      IF (EROW.LT.BROW) GO TO 200
      IROW = 1
C                                       init counters, line size
      JCOL2 = 0
      CHKSIZ = CSIZE(2).GT.0.0
      CALL PRTSCL (KTY, ICH, MCH, RESULT, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      CALL COPY (128, MCH, NCH)
      IF (DOHMS.GT.0.0) THEN
         NCH(COLIDX(3)) = CLEN(ISCALE)
         NCH(COLIDX(4)) = CLEN(ISCALE)
         ICH(COLIDX(3)) = 1
         ICH(COLIDX(4)) = 1
         END IF
C                                       Loop: list all columns to fit
 10   JCOL1 = JCOL2 + 1
      MAXLEN = 1
C                                       how many columns this pass
      N = 0
      IF (OKLIN) N = 8
      DO 15 J = JCOL1, 10
         I = MOD (DATP(COLIDX(J),2), 10)
         LENGTH = DATP(COLIDX(J),2) / 10
         NCH(COLIDX(J)) = MIN (NCH(COLIDX(J)), NACROS-9)
         N = N + NCH(COLIDX(J)) + 2
         IF (N.LE.NACROS) JCOL2 = J
         IF (N.GT.NACROS) GO TO 20
         IF ((I.NE.3) .AND. (I.NE.7)) MAXLEN = MAX (MAXLEN, LENGTH)
 15      CONTINUE
C                                       Blanks between
 20   IF (JCOL2.LT.JCOL1) JCOL2 = JCOL1
      N = 0
      IF (OKLIN) N = 8
      DO 25 J = JCOL1,JCOL2
         N = N + NCH(COLIDX(J))
 25      CONTINUE
      N = (NACROS - N) / (JCOL2 - JCOL1 + 1)
      N = MIN (N, 6)
      GCH(JCOL1) = 1
      IF (OKLIN) GCH(JCOL1) = 9
      IF (JCOL1.LT.JCOL2) THEN
         I = JCOL1 + 1
         DO 30 J = I,JCOL2
            GCH(J) = GCH(J-1) + NCH(COLIDX(J-1)) + N
 30         CONTINUE
         END IF
      IF (FIRST) THEN
         FIRST = .FALSE.
         NCOUNT = NCOUNT + 1
         END IF
      IF (DOCRT.LE.-2.5) NCOUNT = NCOUNT + 2
      IF ((DOCRT.LE.0.0) .AND. (DOCRT.GT.-2.5)) NCOUNT = NCOUNT + 1
C                                       Output loop
      LRNO = 0
      PBFACT = 1.0
      DLFACT = 1.0
      DO 150 IRNO = BROW,EROW,IROW
C                                       scaling
         IF (IDOALL.GE.2) THEN
            JJ = CODLFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            DLFACT = RES4(1)
            END IF
         IF ((IDOALL.EQ.1) .OR. (IDOALL.EQ.3)) THEN
            JJ = COBMFA
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            PBFACT = RES4(1)
            END IF
C                                       Check flux level
         IF (FLUX.NE.0.0) THEN
            JJ = COLFLX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            IF (PBFACT*RES4(1).LT.FLUX) GO TO 150
            END IF
C                                       Check component size
         IF (CHKSIZ) THEN
            JJ = COLMJX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            BMAJ = RES4(1)
            JJ = COLMNX
            CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *         SCRBUF, IERR)
            IF (IERR.LT.0) GO TO 150
            IF (IERR.NE.0) GO TO 960
            BMIN = RES4(1)
            IF (MIN(BMAJ,BMIN)*ASCALE.LT.CSIZE(1)) GO TO 150
            IF (MAX(BMAJ,BMIN)*ASCALE.GT.CSIZE(2)) GO TO 150
            END IF
C                                       Loop over element in arrays.
         DO 140 IEL = 1,MAXLEN
            DO 130 J = JCOL1,JCOL2
               IF (J.GT.10) GO TO 130
               JJ = COLIDX(J)
C                                       get data.
               CALL GETCOL (IRNO, JJ, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *            SCRBUF, IERR)
               IF (IERR.LT.0) GO TO 150
               IF (IERR.NE.0) GO TO 960
               I = KTY(JJ)
C                                       See which have data.
               LENGTH = RTYPE/10
               IF (LENGTH.LT.IEL) GO TO 130
               IF ((IEL.GT.1) .AND. ((I.EQ.3).OR.(I.EQ.7)))
     *            GO TO 130
 130           CONTINUE
C                                       do output finally
            NCOUNT = NCOUNT + 1
 140        CONTINUE
 150     CONTINUE
      IF (JCOL2.LT.10) GO TO 10
C                                       Close down
 200  CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      GO TO 999
C                                       errors
 960  IRET = 3
      CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      WRITE (MSGTXT,1960) IERR
C
 995  CALL MSGWRT (8)
      IF (IRET.EQ.0) IRET = 5
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CHK1C1 ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1960 FORMAT ('CHK1C1 ERROR',I5,' READING TABLE DATA')
      END
