LOCAL INCLUDE 'PRTAB.INC'
      REAL      XSEQ, XDISK, XVER, XBCNT, XECNT, XINC, XNDIG, DOCRT,
     *   XDOHMS, X1, XB, XE, XLIST(40), DOFLAG, XARR(30), DOINV
      HOLLERITH XINNAM(3), XINCLS(2), XINEXT, XLPNAM(12), XSTR(4)
      CHARACTER INNAM*12, INCLS*6, INEXT*2, LPNAME*48, CTVAL*16
      INTEGER   BCOUNT, ECOUNT, ICOUNT, DATP(128,2), BUFFER(512),
     *   INSEQ, INDISK, INVERS, CNO, IUSER, NKEY, NCOL, OUTLUN, OUTIND,
     *   PN1, PN2, PN3, COLIST(128), MCOLST, LFMT, NACROS, NTVAL,
     *   ITVAL(3,7), NIRNO
      LOGICAL   DOHMS
      REAL      RTVAL(7), ETVAL(7)
      COMMON /INPARM/ XINNAM, XINCLS, XSEQ, XDISK, XINEXT, XVER, XBCNT,
     *   XECNT, XINC, XNDIG, DOCRT, XLPNAM, XDOHMS, X1, XB, XE, XLIST,
     *   DOFLAG, XARR, XSTR, DOINV
      COMMON /CHPARM/ INNAM, INCLS, INEXT, LPNAME, CTVAL
      COMMON /PRTABC/ RTVAL, ETVAL, NTVAL, ITVAL, BCOUNT, ECOUNT,
     *   ICOUNT, DATP, BUFFER, INSEQ, INDISK, INVERS, CNO, IUSER, NKEY,
     *   NCOL, OUTLUN, OUTIND, DOHMS, PN1, PN2, PN3, COLIST, MCOLST,
     *   LFMT, NACROS, NIRNO
LOCAL END
      PROGRAM PRTAB
C-----------------------------------------------------------------------
C! Task to print contents of table extension files.
C# Calibration EXT-appl EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2002, 2004-2005, 2007, 2009, 2011-2014
C;  Copyright (C) 2016
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
C   PRTAB is an AIPS task to print the contents of generalized, tables
C   format extension files on the line printer or the CRT terminal.
C   AIPS adverbs:                     Use:
C     USERID     USER         User number: 0 -> login, 32000 -> any
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
C     DOHMS      DOHMS           > 0. print times in hh:mm:ss.s form
C-----------------------------------------------------------------------
      INTEGER   IRET
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'PRTAB.INC'
C-----------------------------------------------------------------------
C                                       Get parms, open things
      CALL PRTBIN (IRET)
C                                       count line printer printing
      IF (IRET.EQ.0) CALL PRTBOK (IRET)
C                                       do printing
      IF (IRET.EQ.0) THEN
C                                       Open output device
         IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
         CALL LPOPEN (LPNAME, DOCRT, OUTLUN, OUTIND, NACROS, BUFFER,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET
            CALL MSGWRT (8)
         ELSE
            CALL PRTBDO (IRET)
            END IF
         END IF
C                                       close down
      IRET = MAX (0, IRET)
      CALL DIE (IRET, BUFFER)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' OPENING OUTPUT DEVICE')
      END
      SUBROUTINE PRTBIN (IRET)
C-----------------------------------------------------------------------
C   PRTBIN performs initialization for AIPS task PRTAB.  It gets the
C   adverbs, opens the catalog file for 'READ', opens the table
C   extension
C   file, and opens the output device.
C   Output: IRET    I      Error code: 0 => keep going, else quit.
C-----------------------------------------------------------------------
      CHARACTER INTYP*2, STAT*4, PRGN*6
      INTEGER   IRET, I4T, NPARM, IROUND, NREC, TABLUN, IERR, I, J, K,
     *   JTRIM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'PRTAB.INC'
      DATA TABLUN, INTYP /27, '  '/
      DATA PRGN /'PRTAB '/
C-----------------------------------------------------------------------
C                                       AIPS init
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       get adverbs
      NPARM = 106
      IRET = 0
      CALL GTPARM (PRGN, NPARM, RQUICK, XINNAM, DATP, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) THEN
            GO TO 999
         ELSE
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
         END IF
C                                       restart AIPS
      IF ((IRET.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
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
      CALL H2CHR (2, 1, XINEXT, INEXT)
      LFMT = 1
      IF (XNDIG.GT.3.0) LFMT = 2
      IF (XNDIG.LE.0.0) LFMT = 3
      IF (XNDIG.LT.-3.0) LFMT = 4
C                                       find image file
      INSEQ = IROUND (XSEQ)
      INDISK = IROUND (XDISK)
      INVERS = IROUND (XVER)
      ICOUNT = IROUND (XINC)
      BCOUNT = IROUND (XBCNT)
      ECOUNT = IROUND (XECNT)
      IF (ICOUNT.LE.0) ICOUNT = 1
      IF (BCOUNT.LE.0) BCOUNT = 1
      IF (INEXT.EQ.' ') INEXT = 'TA'
      IUSER = NLUSER
      DOHMS = XDOHMS.GT.0.0
      PN1 = IROUND (X1)
      PN2 = IROUND (XB)
      PN3 = IROUND (XE)
      IF ((PN1.LE.0) .AND. (((PN2.LE.0) .AND. (PN3.LE.0)) .OR.
     *   (PN3.LT.PN2))) THEN
         PN1 = 100000
         PN2 = 1
         PN3 = PN1
         END IF
      IF ((PN2.GT.1) .AND. (PN3.GE.PN2)) PN1 = MAX (1, PN1)
      CNO = 1
      CALL CATDIR ('SRCH', INDISK, CNO, INNAM, INCLS, INSEQ, INTYP,
     *   IUSER, STAT, DATP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, INNAM, INCLS, INSEQ, INTYP,
     *      INDISK, IUSER
         GO TO 990
         END IF
C                                       Get catblk, mark file read
      CALL CATIO ('READ', INDISK, CNO, CATBLK, 'REST', DATP, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Open table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      CALL TABINI ('READ', INEXT, INDISK, CNO, INVERS, CATBLK, TABLUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, INEXT, INVERS
         GO TO 990
         END IF
      I4T = BUFFER(5)
      IF (BCOUNT.GT.I4T) BCOUNT = 1
      IF ((ECOUNT.LT.BCOUNT) .OR. (ECOUNT.GT.I4T)) ECOUNT = I4T
      IF (ECOUNT.LE.9999999) THEN
         NIRNO = 8
      ELSE IF (ECOUNT.LE.99999999) THEN
         NIRNO = 9
      ELSE IF (ECOUNT.LE.999999999) THEN
         NIRNO = 10
      ELSE
         NIRNO = 11
         END IF
C                                       set column list
      MCOLST = 0
      DO 10 I = 1,40
         I4T = IROUND (XLIST(I))
         IF ((I4T.GT.0) .AND. (I4T.LE.NCOL)) THEN
            MCOLST = MCOLST + 1
            COLIST(MCOLST) = I4T
            END IF
 10      CONTINUE
      IF (MCOLST.LE.0) THEN
         DO 15 I = 1,NCOL
            COLIST(I) = I
 15         CONTINUE
         MCOLST = NCOL
         END IF
C                                       testing on a column value
      IRET = 0
      J = 1
      NTVAL = 0
      DO 20 K = 1,7
         ITVAL(1,K) = IROUND (XARR(J))
         IF ((ITVAL(1,K).GT.0) .AND. (ITVAL(1,K).LE.NCOL)) THEN
            NTVAL = K
            ITVAL(2,K) = IROUND (XARR(J+1))
            I = MOD (DATP(ITVAL(1,K),2), 10)
            I4T = DATP(ITVAL(1,K),2) / 10
            ITVAL(2,K) = MAX (1, MIN (I4T, ITVAL(2,K)))
C                                       allow integer to have
C                                       non-integer comparison value
C
            IF ((I.EQ.1) .OR. (I.EQ.2) .OR. (I.EQ.4)) THEN
               RTVAL(K) = XARR(J+2)
               IF (XARR(J+3).LE.0.0) XARR(J+3) = 1.0E-2
               ETVAL(K) = XARR(J+3)
            ELSE
               CALL H2CHR (16, 1, XSTR, CTVAL)
               ETVAL(K) = MIN (I4T, JTRIM (CTVAL))
               END IF
            ITVAL(3,K) = I
            J = J + 4
         ELSE
            GO TO 999
            END IF
 20      CONTINUE
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
      END
      SUBROUTINE PRTBOK (IRET)
C-----------------------------------------------------------------------
C   PRTBDO reads a table extension file, counts line to line printer
C   Output:
C      IRET   I   Error code: 0 => OK or user terminates,
C                    2 => error writing, 3 => error reading
C                    -1 => user decides against
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXKEY
C                                       MAXKEY=max. no. keyword-values
      PARAMETER (MAXKEY=1024)
      CHARACTER SCRTCH*132, COLNUM*132, KEYWRD(MAXKEY)*8, ATIME*8,
     *   ADATE*12, BTIME*8, BDATE*12, SORTOR(5)*8, XCOLN*8, CTEMP*12,
     *   SORTS(2)*12, FMT(128)*8
      INTEGER   IRET, SCRBUF(XBPRSZ), IRNO, RESLI(XBPRSZ), JCOL1, JCOL2,
     *   I, J, K, M, N, NLINE, NCH(128), FCH(128), ICH(128), RTYPE, II,
     *   RESULT(XBPRSZ), IERR, JJ, NUMKEY, TTY(2), LOCS(MAXKEY),
     *   KEYTYP(MAXKEY), VALUES(4*MAXKEY), LENGTH, MAXLEN, NCOPY, IEL,
     *   JTRIM, KTY(128), LRNO, KK, LL, IFMT(128), NCOUNT
      LOGICAL   RESLO(XBPRSZ), FIRST, DOIT
      REAL      RES4(XBPRSZ), BUFF4(512)
      HOLLERITH RESH(XBPRSZ)
      DOUBLE PRECISION    RES8(XBPRSZ/2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'PRTAB.INC'
      EQUIVALENCE (BUFFER, BUFF4)
      EQUIVALENCE (RESULT, RES8, RES4, RESH, RESLI, RESLO)
      DATA SORTOR /'ASCEND  ','ASCEAB  ','DESCND  ',
     *   'DESCAB  ', '        '/
      DATA XCOLN /'COL. NO.'/
C-----------------------------------------------------------------------
C                                       init counters, line size
      IRET = 0
C                                       do we need to do this
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
C                                       yes
      JCOL2 = 0
      FIRST = .TRUE.
      NCOUNT = 0
      NACROS = 132
C                                       get column dimensions scales
      CALL PRTSCL (KTY, IFMT, ICH, NCH, FMT, RESULT, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       Loop: list all columns to fit
 10   JCOL1 = JCOL2 + 1
         MAXLEN = 1
C                                       how many columns this pass
         N = NIRNO + 1
         DO 15 KK = JCOL1,MCOLST
            J = COLIST(KK)
            I = MOD (DATP(J,2), 10)
            LENGTH = DATP(J,2) / 10
            NCH(KK) = MIN (NCH(KK), NACROS-10)
            N = N + NCH(KK) + 2
            IF (N.LE.NACROS) JCOL2 = KK
            IF (N.GT.NACROS) GO TO 20
            IF ((I.NE.3) .AND. (I.NE.7)) MAXLEN = MAX (MAXLEN, LENGTH)
 15         CONTINUE
C                                       Blanks between
 20      IF (JCOL2.LT.JCOL1) JCOL2 = JCOL1
         N = 9
         DO 25 KK = JCOL1,JCOL2
            N = N + NCH(KK)
 25         CONTINUE
         N = (NACROS - N) / (JCOL2 - JCOL1 + 1)
         N = MIN (N, 6)
         FCH(JCOL1) = 9 + N
         IF (JCOL1.LT.JCOL2) THEN
            I = JCOL1 + 1
            DO 30 KK = I,JCOL2
               FCH(KK) = FCH(KK-1) + NCH(KK-1) + N
 30            CONTINUE
            END IF
C                                       Primary page: heading info
         IRNO = BUFFER(5)
         SORTS(1) = ' '
         SORTS(2) = ' '
         II = 5
         JJ = 5
         IF (BUFFER(43).GT.0) II = 1
         IF (BUFFER(44).GT.0) JJ = 1
         IF (BUFFER(43).LT.0) II = 3
         IF (BUFFER(44).LT.0) JJ = 3
         IF (BUFFER(43).GT.256) II = 2
         IF (BUFFER(44).GT.256) JJ = 2
         IF (BUFFER(43).LT.-256) II = 4
         IF (BUFFER(44).LT.-256) JJ = 4
         I = ABS(BUFFER(43))
         IF (I.GT.256) I = I - 256
         J = ABS(BUFFER(44))
         IF (J.GT.256) J = J - 256
         IF (I.GT.0) WRITE (CTEMP,1035) I, SORTOR(II)(1:6)
         IF (I.GT.0) SORTS(1)(1:12) = CTEMP(1:12)
         IF (J.GT.0) WRITE (CTEMP,1035) J, SORTOR(JJ)(1:6)
         IF (J.GT.0) SORTS(2)(1:12) = CTEMP(1:12)
         CALL TIMDAT (BUFFER(14), BUFFER(11), ATIME, ADATE)
         CALL TIMDAT (BUFFER(36), BUFFER(33), BTIME, BDATE)
         I = 100 + 1
         N = I + 13
C                                         First page
         NLINE = 900
         IF (.NOT.FIRST) GO TO 90
         NLINE = 999
         IF (DOCRT.LE.-2.5) THEN
            NCOUNT = NCOUNT + 1
         ELSE
            NCOUNT = NCOUNT + 1
            NCOUNT = NCOUNT + 1
            END IF
         NCOUNT = NCOUNT + 1
         IF ((PN1.GT.0) .AND. (PN2.GT.PN1) .AND. (PN3.GE.PN2)) THEN
            NCOUNT = NCOUNT + 1
            END IF
C                                       selection strings
         IF ((BUFFER(61).GT.0) .AND. (DOCRT.GT.-2.5)) THEN
            NCOUNT = NCOUNT + BUFFER(61)
            END IF
C                                       Keyword/value pairs
         IF ((BUFFER(53).GT.0) .AND. (JCOL1.LE.1) .AND. (DOCRT.GT.-2.5))
     *      THEN
            NUMKEY = MAXKEY
            NUMKEY = MIN (NUMKEY, BUFFER(53))
            CALL TABKEY ('ALL ', KEYWRD, NUMKEY, BUFFER, LOCS, VALUES,
     *         KEYTYP, IERR)
            IF (IERR.NE.0) GO TO 90
            NCOUNT = NCOUNT + 1
            NCOPY = 2
            NCOPY = MAX (NCOPY, 1)
            NCOUNT = NCOUNT + NUMKEY
            END IF
C                                       Tell if it can be FITS ASCII
         IF (DOCRT.GT.-2.5) THEN
            NCOUNT = NCOUNT + 1
            END IF
C                                       List column numbers
 90      COLNUM = ' '
         COLNUM(1:8) = XCOLN(1:8)
C                                       Column number
         DO 92 KK = JCOL1,JCOL2
            J = COLIST(KK)
            WRITE (CTEMP,1090) J
            N = 1
            IF (J.GT.9) N = 2
            IF (J.GT.99) N = 3
            K = (NCH(KK)+1 - N) / 2
            COLNUM(FCH(KK)+K:FCH(KK)+K+N-1) = CTEMP(6-N:5)
 92         CONTINUE
         IF ((FIRST) .AND. (DOCRT.GT.-2.5)) THEN
            NCOUNT = NCOUNT + 1
            END IF
         NCOUNT = NCOUNT + 1
C                                       Output the lines
         NCOUNT = NCOUNT + 1
         NCOUNT = NCOUNT + 1
         IF ((DOCRT.GT.-2.5) .AND. (DOCRT.LE.0.0)) THEN
            NCOUNT = NCOUNT + 1
            END IF
C                                       Output loop
         LRNO = 0
         DO 150 IRNO = BCOUNT,ECOUNT,ICOUNT
C                                       do we want this row
            DOIT = .TRUE.
            DO 105 LL = 1,NTVAL
               J = ITVAL(1,LL)
               CALL GETCOL (IRNO, J, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *            SCRBUF, IERR)
               IF ((IERR.LT.0) .AND. (DOFLAG.LE.0.0)) GO TO 150
               IF (IERR.GT.0) GO TO 960
               I = ITVAL(3,LL)
               IF (I.EQ.1) THEN
                  IF (ABS(RES8(ITVAL(2,LL))-RTVAL(LL)).GT.ETVAL(LL))
     *               DOIT = .FALSE.
               ELSE IF (I.EQ.2) THEN
                  IF (ABS(RES4(ITVAL(2,LL))-RTVAL(LL)).GT.ETVAL(LL))
     *               DOIT = .FALSE.
               ELSE IF (I.EQ.4) THEN
                  IF (ABS(RESLI(ITVAL(2,LL))-RTVAL(LL)).GT.ETVAL(LL))
     *               DOIT = .FALSE.
               ELSE
                  M = ETVAL(LL) + 0.1
                  CALL H2CHR (M, 1, RESH, SCRTCH)
                  I = JTRIM (SCRTCH(:M))
                  IF (SCRTCH(:M).NE.CTVAL(:M)) DOIT = .FALSE.
                  END IF
 105           CONTINUE
            IF ((DOINV.GT.0.0) .AND. (NTVAL.GT.0)) DOIT = .NOT.DOIT
            IF (.NOT.DOIT) GO TO 150
C                                       Loop over element in arrays.
            DO 145 IEL = 1,MAXLEN
               IF ((IEL.LE.PN1) .OR. ((IEL.GE.PN2) .AND. (IEL.LE.PN3)))
     *            THEN
               ELSE IF (((IEL.EQ.PN1+1) .AND. (IEL.LT.PN2)) .OR.
     *            (IEL.EQ.PN3+1)) THEN
C                                       suppress output
               ELSE
                  GO TO 145
                  END IF
C                                       do output finally
               NCOUNT = NCOUNT + 1
 145           CONTINUE
 150        CONTINUE
C                                       loop for more columns
         FIRST = .FALSE.
         IF (JCOL2.LT.MCOLST) GO TO 10
C                                       ask if needed
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
         IF (NCOUNT.GT.1000) IRET = -1
      ELSE IF (NCOUNT.GT.500) THEN
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRTCH, .FALSE., .FALSE.,
     *      .TRUE., IRET)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IRET.GT.0) GO TO 995
         WRITE (SCRTCH,1150) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, IRET)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IRET.GT.0) GO TO 995
         SCRTCH = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRTCH, 1, CTEMP, IRET)
         IF (IRET.GT.0) GO TO 995
         IF ((CTEMP(:1).NE.'y') .AND. (CTEMP(:1).NE.'Y')) THEN
            IRET = -1
            SCRTCH = 'Good choice - save trees'
         ELSE
            SCRTCH = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      GO TO 999
C                                       errors
 960  IRET = 3
      WRITE (MSGTXT,1960) IERR
C
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1035 FORMAT (I3,1X,'(',A6,')')
 1090 FORMAT (I5)
 1150 FORMAT ('Requested print job is',I10,' lines long!')
 1960 FORMAT ('ERROR',I5,' READING TABLE DATA')
      END
      SUBROUTINE PRTBDO (IRET)
C-----------------------------------------------------------------------
C   PRTBDO reads, formats, and prints a table extension file
C   Output:
C      IRET   I   Error code: 0 => OK or user terminates,
C                    2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXKEY
C                                       MAXKEY=max. no. keyword-values
      PARAMETER (MAXKEY=1024)
      CHARACTER TITL1*132, TITL2*132, LINE*132, MSGBUF*72, SCRTCH*132,
     *   COLNUM*132, ALL*4, CTEMP1*132, KEYWRD(MAXKEY)*8, ATIME*8,
     *   ADATE*12, BTIME*8, BDATE*12, SORTOR(5)*8, XROW*4, XNUMB1*8,
     *   XCOLN*8, CHSIGN*1, CTEMP*12, SORTS(2)*12, DOTS*20, XNUMB2*8,
     *   FMT(128)*8
      INTEGER   IRET, SCRBUF(XBPRSZ), IRNO, RESLI(XBPRSZ), JCOL1, JCOL2,
     *   IPAGE, I, J, K, L, M, N, NLINE, NCH(128), FCH(128), ICH(128),
     *   RESULT(XBPRSZ), BITS(64), RTYPE, IERR, II, JJ, NUMKEY, IPOINT,
     *   LOCS(MAXKEY), KEYTYP(MAXKEY), VALUES(4*MAXKEY), LENGTH, MAXLEN,
     *   NCOPY, KT, IEL, HMS(3), JTRIM, KTY(128), LRNO, KK, LL,
     *   IFMT(128)
      LOGICAL   RESLO(XBPRSZ), FIRST, DOIT
      REAL      RES4(XBPRSZ), BUFF4(512), SEC, TDAY
      HOLLERITH RESH(XBPRSZ)
      DOUBLE PRECISION    RES8(XBPRSZ/2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'PRTAB.INC'
      EQUIVALENCE (BUFFER, BUFF4)
      EQUIVALENCE (RESULT, RES8, RES4, RESH, RESLI, RESLO)
      DATA SORTOR /'ASCEND  ','ASCEAB  ','DESCND  ',
     *   'DESCAB  ', '        '/
      DATA ALL /'ALL '/
      DATA XROW, XNUMB1, XNUMB2, XCOLN /' ROW','F NUMBER','  NUMBER',
     *   'COL. NO.'/
      DATA DOTS /'++++++++++++++++++++'/
C-----------------------------------------------------------------------
C                                       init counters, line size
      IRET = 0
      JCOL2 = 0
      FIRST = .TRUE.
C                                       get column dimensions scales
      CALL PRTSCL (KTY, IFMT, ICH, NCH, FMT, RESULT, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       Loop: list all columns to fit
 10   JCOL1 = JCOL2 + 1
         MAXLEN = 1
C                                       how many columns this pass
         N = NIRNO + 1
         DO 15 KK = JCOL1,MCOLST
            J = COLIST(KK)
            I = MOD (DATP(J,2), 10)
            LENGTH = DATP(J,2) / 10
            NCH(KK) = MIN (NCH(KK), NACROS-10)
            N = N + NCH(KK) + 2
            IF (N.LE.NACROS) JCOL2 = KK
            IF (N.GT.NACROS) GO TO 20
            IF ((I.NE.3) .AND. (I.NE.7)) MAXLEN = MAX (MAXLEN, LENGTH)
 15         CONTINUE
C                                       Blanks between
 20      IF (JCOL2.LT.JCOL1) JCOL2 = JCOL1
         N = 9
         DO 25 KK = JCOL1,JCOL2
            N = N + NCH(KK)
 25         CONTINUE
         N = (NACROS - N) / (JCOL2 - JCOL1 + 1)
         N = MIN (N, 6)
         FCH(JCOL1) = 9 + N
         IF (JCOL1.LT.JCOL2) THEN
            I = JCOL1 + 1
            DO 30 KK = I,JCOL2
               FCH(KK) = FCH(KK-1) + NCH(KK-1) + N
 30            CONTINUE
            END IF
C                                       Primary page: heading info
         IRNO = BUFFER(5)
         SORTS(1) = ' '
         SORTS(2) = ' '
         II = 5
         JJ = 5
         IF (BUFFER(43).GT.0) II = 1
         IF (BUFFER(44).GT.0) JJ = 1
         IF (BUFFER(43).LT.0) II = 3
         IF (BUFFER(44).LT.0) JJ = 3
         IF (BUFFER(43).GT.256) II = 2
         IF (BUFFER(44).GT.256) JJ = 2
         IF (BUFFER(43).LT.-256) II = 4
         IF (BUFFER(44).LT.-256) JJ = 4
         I = ABS(BUFFER(43))
         IF (I.GT.256) I = I - 256
         J = ABS(BUFFER(44))
         IF (J.GT.256) J = J - 256
         IF (I.GT.0) WRITE (CTEMP,1035) I, SORTOR(II)(1:6)
         IF (I.GT.0) SORTS(1)(1:12) = CTEMP(1:12)
         IF (J.GT.0) WRITE (CTEMP,1035) J, SORTOR(JJ)(1:6)
         IF (J.GT.0) SORTS(2)(1:12) = CTEMP(1:12)
         CALL TIMDAT (BUFFER(14), BUFFER(11), ATIME, ADATE)
         CALL TIMDAT (BUFFER(36), BUFFER(33), BTIME, BDATE)
         I = 100 + 1
         N = I + 13
C                                         First page
         NLINE = 900
         IF (.NOT.FIRST) GO TO 90
         NLINE = 999
         WRITE (TITL1,1040) INNAM, INCLS, INSEQ, INDISK, INEXT,
     *      INVERS
         CALL H2CHR (112, 1, BUFF4(I), CTEMP1)
         M = JTRIM (CTEMP1)
         WRITE (TITL2,1041) CTEMP1
         IF (DOCRT.LE.-2.5) THEN
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *         TITL1, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 970
         ELSE
            CALL H2CHR (5, 1, BUFFER(29), CTEMP1)
            M = JTRIM (CTEMP1)
            WRITE (LINE,1042) CTEMP1, ADATE, ATIME
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL H2CHR (5, 1, BUFFER(39), CTEMP1)
            M = JTRIM (CTEMP1)
            WRITE (LINE,1043) CTEMP1, BDATE, BTIME
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
         WRITE (LINE,1044) NCOL, IRNO, SORTS
         CALL REFRMT (LINE, '_', I)
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         IF ((PN1.GT.0) .AND. (PN2.GT.PN1) .AND. (PN3.GE.PN2)) THEN
            WRITE (LINE,1045) PN1, PN2, PN3
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
C                                       selection strings
         IF ((BUFFER(61).GT.0) .AND. (DOCRT.GT.-2.5)) THEN
            N = BUFFER(61)
            DO 55 I = 1,N
               IRNO = I
               CALL TABIO ('READ', 2, IRNO, RESULT, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 960
               M = BUFFER(63+I) - BUFFER(62+I)
               IF (I.EQ.N) M = BUFFER(62) - BUFFER(62+I)
               M = MIN (M, 28)
               IF (16+4*M.GT.NACROS) M = (NACROS - 16)
               CALL H2CHR (M, 1, RES4(1), CTEMP1)
               L = JTRIM (CTEMP1)
               WRITE (LINE,1050) CTEMP1
               CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, NLINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 970
 55            CONTINUE
            END IF
C                                       Keyword/value pairs
         IF ((BUFFER(53).GT.0) .AND. (JCOL1.LE.1) .AND. (DOCRT.GT.-2.5))
     *      THEN
            NUMKEY = MAXKEY
            NUMKEY = MIN (NUMKEY, BUFFER(53))
            CALL TABKEY (ALL, KEYWRD, NUMKEY, BUFFER, LOCS, VALUES,
     *         KEYTYP, IERR)
            IF (IERR.NE.0) GO TO 90
            WRITE (LINE,1060) BUFFER(53)
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 970
            NCOPY = 2
            NCOPY = MAX (NCOPY, 1)
C                                       Loop through keywords.
            DO 70 IEL = 1,NUMKEY
               IPOINT = LOCS(IEL)
               KT = KEYTYP(IEL)
C                                       Copy value to RESULT for
C                                       EQUIVALENCE
               CALL COPY (NCOPY, VALUES(IPOINT), RESULT)
               IF (KT.EQ.1) THEN
                  WRITE (LINE,1061) KEYWRD(IEL), RES8(1)
               ELSE IF (KT.EQ.2) THEN
                  WRITE (LINE,1062) KEYWRD(IEL), RES4(1)
               ELSE IF (KT.EQ.3) THEN
                  CALL H2CHR (8, 1, RES4(1), CTEMP)
                  M = JTRIM (CTEMP)
                  WRITE (LINE,1063) KEYWRD(IEL), CTEMP
               ELSE IF (KT.EQ.4) THEN
                  WRITE (LINE,1064) KEYWRD(IEL), RESLI(1)
               ELSE IF (KT.EQ.5) THEN
                  WRITE (LINE,1065) KEYWRD(IEL), RESLO(1)
               ELSE IF (KT.EQ.6) THEN
                  WRITE (LINE,1066) KEYWRD(IEL), RESULT(1)
                  END IF
               CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, NLINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 970
 70            CONTINUE
            END IF
C                                       Tell if it can be FITS ASCII
         IF (DOCRT.GT.-2.5) THEN
            IF (BUFFER(60).NE.1) WRITE (LINE,1080)
            IF (BUFFER(60).EQ.1) WRITE (LINE,1081)
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
C                                       List column numbers
 90      TITL1 = ' '
         TITL2 = ' '
         COLNUM = ' '
         LINE = ' '
         COLNUM(1:8) = XCOLN(1:8)
C                                       Column number
         DO 92 KK = JCOL1,JCOL2
            J = COLIST(KK)
            WRITE (CTEMP,1090) J
            N = 1
            IF (J.GT.9) N = 2
            IF (J.GT.99) N = 3
            K = (NCH(KK)+1 - N) / 2
            COLNUM(FCH(KK)+K:FCH(KK)+K+N-1) = CTEMP(6-N:5)
 92         CONTINUE
         IF ((FIRST) .AND. (DOCRT.GT.-2.5)) THEN
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      COLNUM, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Prepare page titles
         TITL1(5:8) = XROW(1:4)
         TITL2(1:8) = XNUMB2(1:8)
         IF (DOFLAG.GT.0.0) TITL2(1:8) = XNUMB1(1:8)
         DO 95 KK = JCOL1,JCOL2
            J = COLIST(KK)
C                                       Column label
            IRNO = J
            CALL TABIO ('READ', 3, IRNO, RESULT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 960
            I = MIN (24, NCH(KK))
            CALL H2CHR (I, 1, RESH(1), SCRTCH(:I))
            M = JTRIM (SCRTCH(:I))
            IF (M.GT.0) TITL1(FCH(KK):) = SCRTCH(:M)
 95         CONTINUE
         DO 99 KK = JCOL1,JCOL2
            J = COLIST(KK)
            IRNO = J
            CALL TABIO ('READ', 4, IRNO, RESULT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 960
            CALL H2CHR (8, 1, RESH, SCRTCH)
            IF (KTY(KK).GT.10) SCRTCH = 'D/HMS'
            M = JTRIM (SCRTCH(:8))
            IF (M.GT.0) TITL2(FCH(KK):) = SCRTCH(:M)
 99         CONTINUE
C                                       Output the lines
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      TITL1, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *      TITL2, NLINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 970
         IF ((DOCRT.GT.-2.5) .AND. (DOCRT.LE.0.0)) THEN
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
C                                       Output loop
         LRNO = 0
         DO 150 IRNO = BCOUNT,ECOUNT,ICOUNT
C                                       do we want this row
            DOIT = .TRUE.
            DO 105 LL = 1,NTVAL
               J = ITVAL(1,LL)
               CALL GETCOL (IRNO, J, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *            SCRBUF, IERR)
               IF ((IERR.LT.0) .AND. (DOFLAG.LE.0.0)) GO TO 150
               IF (IERR.GT.0) GO TO 960
               I = ITVAL(3,LL)
               IF (I.EQ.1) THEN
                  IF (ABS(RES8(ITVAL(2,LL))-RTVAL(LL)).GT.ETVAL(LL))
     *               DOIT = .FALSE.
               ELSE IF (I.EQ.2) THEN
                  IF (ABS(RES4(ITVAL(2,LL))-RTVAL(LL)).GT.ETVAL(LL))
     *               DOIT = .FALSE.
               ELSE IF (I.EQ.4) THEN
                  IF (ABS(RESLI(ITVAL(2,LL))-RTVAL(LL)).GT.ETVAL(LL))
     *               DOIT = .FALSE.
               ELSE
                  M = ETVAL(LL) + 0.1
                  CALL H2CHR (M, 1, RESH, SCRTCH)
                  I = JTRIM (SCRTCH(:M))
                  IF (SCRTCH(:M).NE.CTVAL(:M)) DOIT = .FALSE.
                  END IF
 105           CONTINUE
            IF ((DOINV.GT.0.0) .AND. (NTVAL.GT.0)) DOIT = .NOT.DOIT
            IF (.NOT.DOIT) GO TO 150
C                                       Loop over element in arrays.
            DO 145 IEL = 1,MAXLEN
C                                       Clear output
               LINE = ' '
               IF ((IEL.LE.PN1) .OR. ((IEL.GE.PN2) .AND. (IEL.LE.PN3)))
     *            THEN
                  DO 130 KK = JCOL1,JCOL2
                     J = COLIST(KK)
C                                       get data.
                     CALL GETCOL (IRNO, J, DATP, LRNO, BUFFER, RTYPE,
     *                  RESULT, SCRBUF, IERR)
                     IF ((IERR.LT.0) .AND. (DOFLAG.LE.0.0)) GO TO 150
                     IF (IERR.GT.0) GO TO 960
                     I = KTY(KK)
C                                       See which have data.
                     LENGTH = RTYPE/10
                     IF (LENGTH.LT.IEL) GO TO 130
                     IF ((IEL.GT.1) .AND. ((I.EQ.3).OR.(I.EQ.7)))
     *                  GO TO 130
C                                       character
                     IF (I.EQ.3) THEN
                        L = FCH(KK) + MAX (0, (NCH(KK)-ICH(KK))/2)
                        CALL H2CHR (ICH(KK), 1, RESH, SCRTCH)
                        M = JTRIM (SCRTCH(:ICH(KK)))
                        IF (M.GT.0) LINE(L:L+M-1) = SCRTCH(:M)
C                                       bit
                     ELSE IF (I.EQ.7) THEN
                        L = FCH(KK) + MAX (0, (NCH(KK)-ICH(KK))/2)
                        N = (ICH(KK) - 1) / NBITWD + 1
                        DO 125 I = 1,N
                           M = NBITWD
                           IF (I.EQ.N) M = ICH(KK) - (N-1)*NBITWD
                           CALL ZGTBIT (M, RESULT(I), BITS)
                           WRITE (MSGBUF,1120) (BITS(K), K = 1,M)
                           LINE(L:L+M-1) = MSGBUF(1:M)
                           L = L + M
 125                       CONTINUE
C                                       numeric
                     ELSE
C                                       double precision
                        IF (I.EQ.1) THEN
                           IF (RES8(IEL).EQ.DBLANK) THEN
                              IF (NCH(KK).GE.4) THEN
                                 MSGBUF = ' '
                                 MSGBUF(NCH(KK)-3:) = 'INDE'
                              ELSE
                                 MSGBUF = 'INDE'
                                 END IF
                           ELSE IF (IFMT(KK).EQ.1) THEN
                              WRITE (MSGBUF,1101) RES8(IEL)
                           ELSE IF (IFMT(KK).EQ.2) THEN
                              WRITE (MSGBUF,1111) RES8(IEL)
                           ELSE
                              WRITE (MSGBUF,FMT(KK)) RES8(IEL)
                              END IF
                           END IF
C                                       single precision
                        IF (I.EQ.2) THEN
                           IF (RES4(IEL).EQ.FBLANK) THEN
                              IF (NCH(KK).GE.4) THEN
                                 MSGBUF = ' '
                                 MSGBUF(NCH(KK)-3:) = 'INDE'
                              ELSE
                                 MSGBUF = 'INDE'
                                 END IF
                           ELSE IF (IFMT(KK).EQ.1) THEN
                              WRITE (MSGBUF,1102) RES4(IEL)
                           ELSE IF (IFMT(KK).EQ.2) THEN
                              WRITE (MSGBUF,1112) RES4(IEL)
                           ELSE
                              WRITE (MSGBUF,FMT(KK)) RES4(IEL)
                              END IF
                           END IF
                        IF (I.EQ.4) WRITE (MSGBUF,1103) RESLI(IEL)
                        IF (I.EQ.5) WRITE (MSGBUF,1104) RESLO(IEL)
                        IF (I.EQ.6) WRITE (MSGBUF,1103) RESULT(IEL)
C                                       time in h:m:s.s
                        IF (I.GT.10) THEN
                           IF (I.EQ.12) THEN
                              TDAY = RES4(IEL)
                           ELSE
                              TDAY = RES8(IEL)
                              END IF
                           CALL TFDHMS (TDAY, 1, CHSIGN, HMS, SEC)
                           WRITE (MSGBUF,1107) HMS, SEC
                           IF (MSGBUF(11:11).EQ.' ') MSGBUF(11:11) = '0'
                           IF (MSGBUF(12:12).EQ.' ') MSGBUF(12:12) = '0'
                           IF (CHSIGN.EQ.'-') THEN
                              IF (MSGBUF(2:2).EQ.' ') THEN
                                 MSGBUF(2:2) = CHSIGN
                              ELSE
                                 MSGBUF(ICH(KK):ICH(KK)) = CHSIGN
                                 END IF
                              END IF
                           END IF
                        LINE(FCH(KK):FCH(KK)+NCH(KK)-1) =
     *                     MSGBUF(ICH(KK):)
                        END IF
 130                 CONTINUE
               ELSE IF (((IEL.EQ.PN1+1) .AND. (IEL.LT.PN2)) .OR.
     *            (IEL.EQ.PN3+1)) THEN
                  DO 140 KK = JCOL1,JCOL2
                     J = COLIST(KK)
C                                       get data.
                     CALL GETCOL (IRNO, J, DATP, LRNO, BUFFER, RTYPE,
     *                  RESULT, SCRBUF, IERR)
                     IF ((IERR.LT.0) .AND. (DOFLAG.LE.0.0)) GO TO 150
                     IF (IERR.GT.0) GO TO 960
                     I = KTY(KK)
C                                       See which have data.
                     LENGTH = RTYPE/10
                     IF (LENGTH.LT.IEL) GO TO 140
                     IF ((IEL.GT.1) .AND. ((I.EQ.3).OR.(I.EQ.7)))
     *                  GO TO 140
C                                       character
                     IF (I.EQ.3) THEN
                        L = FCH(KK) + MAX (0, (NCH(KK)-ICH(KK))/2)
                        SCRTCH = DOTS(:ICH(KK))
                        M = JTRIM (SCRTCH(:ICH(KK)))
                        IF (M.GT.0) LINE(L:L+M-1) = DOTS(:M)
C                                       bit
                     ELSE IF (I.EQ.7) THEN
                        L = FCH(KK) + MAX (0, (NCH(KK)-ICH(KK))/2)
                        N = (ICH(KK) - 1) / NBITWD + 1
                        DO 135 I = 1,N
                           M = NBITWD
                           IF (I.EQ.N) M = ICH(KK) - (N-1)*NBITWD
                           MSGBUF = DOTS(:MIN(20,M))
                           LINE(L:L+M-1) = MSGBUF(1:M)
                           L = L + M
 135                    CONTINUE
C                                       numeric
                     ELSE
                        MSGBUF = ' '
C                                       double precision
                        IF (I.EQ.1) THEN
                           IF (RES8(IEL).EQ.DBLANK) THEN
                              MSGBUF(4:7) = DOTS(:4)
                           ELSE
                              MSGBUF = DOTS(:13)
                              END IF
C                                       single precision
                        ELSE IF (I.EQ.2) THEN
                           IF (RES4(IEL).EQ.FBLANK) THEN
                              MSGBUF(4:7) = DOTS(:4)
                           ELSE
                              MSGBUF = DOTS(:10)
                              END IF
                           END IF
                        IF (I.EQ.4) MSGBUF(9:12) = DOTS(:4)
                        IF (I.EQ.5) MSGBUF(12:12) = DOTS(:1)
                        IF (I.EQ.6) MSGBUF(9:12) = DOTS(:4)
C                                       time in h:m:s.s
                        IF (I.GT.10) THEN
                           MSGBUF(3:14) = DOTS(:12)
                           END IF
                        LINE(FCH(KK):FCH(KK)+NCH(KK)-1) =
     *                     MSGBUF(ICH(KK):)
                        END IF
 140                 CONTINUE
C                                       suppress output
               ELSE
                  GO TO 145
                  END IF
C                                       row number
               WRITE (MSGBUF,1100) IRNO
               LINE(1:NIRNO) = MSGBUF(13-NIRNO:12)
               IF (IERR.LT.0) LINE(1:1) = '*'
C                                       do output finally
               CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, NLINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 970
 145           CONTINUE
 150        CONTINUE
C                                       loop for more columns
         FIRST = .FALSE.
         IF (JCOL2.LT.MCOLST) GO TO 10
C                                       Close down
 200  CALL TABIO ('CLOS', 3, IRNO, RESULT, BUFFER, IERR)
      CALL LPCLOS (OUTLUN, OUTIND, NLINE, IERR)
      GO TO 999
C                                       errors
 960  IRET = 3
      WRITE (MSGTXT,1960) IERR
      GO TO 995
 970  IRET = 0
      IF (IERR.LT.0) GO TO 200
         IRET = 2
         WRITE (MSGTXT,1970) IERR
C
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1035 FORMAT (I3,1X,'(',A6,')')
 1040 FORMAT (A12,'.',A6,'.',I4,'  Disk=',I2,4X,A2,' Table version',
     *   I4)
 1041 FORMAT ('Title: ',A56)
 1042 FORMAT ('Created by      ',A5,' on ',A12,A8)
 1043 FORMAT ('Last written by ',A5,' on ',A12,A8)
 1044 FORMAT ('Ncol',I4,'__Nrow',I12,'____','Sort cols:',2('_',A12))
 1045 FORMAT ('Printing array indices 1 -',I6,'  and',I6,' -',I6)
 1050 FORMAT (3X,'Selected on: ',A112)
 1060 FORMAT (3X,' Table has ',I5,' keyword-value pairs:')
 1061 FORMAT (3X,A8,' = ',1PD20.13)
 1062 FORMAT (3X,A8,' = ',1PE14.7)
 1063 FORMAT (3X,A8,' =  ',A8)
 1064 FORMAT (3X,A8,' = ',I12)
 1065 FORMAT (3X,A8,' = ',L2)
 1066 FORMAT (3X,A8,' = ',I6)
 1080 FORMAT (3X,'Table can be written as a FITS ASCII table')
 1081 FORMAT (3X,'Table format incompatable with FITS ASCII tables')
 1090 FORMAT (I5)
 1100 FORMAT (I12)
 1101 FORMAT (1PD13.6)
 1102 FORMAT (1PE10.3)
 1111 FORMAT (1PD17.10)
 1112 FORMAT (1PE13.6)
 1103 FORMAT (I12)
 1104 FORMAT (11X,L1)
 1107 FORMAT (I3,'/',I2.2,':',I2.2,':',F4.1)
 1120 FORMAT (64I1)
 1960 FORMAT ('ERROR',I5,' READING TABLE DATA')
 1970 FORMAT ('ERROR',I5,' WRITING OUTPUT')
      END
      SUBROUTINE PRTSCL (KTY, IFMT, ICH, NCH, FMT, RESULT, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   determines the number of characters required for each column
C   In/out (common):
C      DOHMS    L        Do conversion Days -> HH MM SS.S ?
C   Outputs:
C      KTY      I(128)   type of column: 1-7 = dp, sp, ch, i, l, i
C                           11,12 = dp and sp time in D/HMS
C      IFMT     I(128)   format type code - 1,2 exp   3,4 Fm.n
C      ICH      I(128)   start character position; number characters in
C                        actual data for character and bit columns
C      NCH      I(128)   number of characters: assume formats
C                        Level 1: 1PD13.6, 1PE10.3, I12, L12
C                        Level 2: 1PD17.10, 1PE13.6, I12, L12
C      FMT      C(*)*8   F type format to use
C      RESULT   I(*)     buffer
C      SCRTCH   I(*)     buffer
C      IERR     I        TABIO error code
C-----------------------------------------------------------------------
      INTEGER   KTY(128), IFMT(128), ICH(128), NCH(128), RESULT(*),
     *   SCRTCH(*), IERR
      CHARACTER FMT(*)*(*)
C
      INTEGER   LCH(7,4), I, J, LENGTH, IMX(128), IMN(128), LMN(128), K,
     *   IL, JTRIM, IC, IRNO, TCNT, KK, M, LL, LEC
      CHARACTER HDR*24, TSTR*128, UNITS*24
      REAL      SMN(128), SMX(128), SRES(128), SMA(128), TEMP
      DOUBLE PRECISION DRES(128)
      LOGICAL   DOIT
      INCLUDE 'PRTAB.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LCH /13, 10, 0, 1, 1, 1, 0,
     *          17, 13, 0, 1, 1, 1, 0,
     *          13, 10, 0, 1, 1, 1, 0,
     *          17, 13, 0, 1, 1, 1, 0/
C-----------------------------------------------------------------------
C                                       set non-integer lengths
      K = 0
      IERR = 0
      TCNT = 0
      DO 20 KK = 1,MCOLST
         J = COLIST(KK)
C                                       basics
         I = MOD (DATP(J,2), 10)
         KTY(KK) = I
         LENGTH = DATP(J,2) / 10
         ICH(KK) = 1
         NCH(KK) = LCH(I,LFMT)
         IFMT(KK) = LFMT
C                                       check header, units strings
         CALL TABIO ('READ', 3, J, RESULT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL H2CHR (24, 1, RESULT, HDR)
         IL = JTRIM (HDR)
         IF (IL.GT.9) IL = MIN (8, IL)
         CALL TABIO ('READ', 4, J, RESULT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL H2CHR (8, 1, RESULT, UNITS)
         IC = JTRIM (UNITS)
         IC = MAX (IC, IL, 2)
C                                       is it a time?
         IF ((I.LT.3) .AND. (DOHMS) .AND. (UNITS(:8).EQ.'DAYS') .AND.
     *      (HDR(:4).EQ.'TIME')) THEN
            KTY(KK) = 10 + I
            NCH(KK) = 10
            ICH(KK) = 5
            TCNT = TCNT + 1
            IC = MAX (IC, 5)
            END IF
         LMN(KK) = LENGTH
         IMN(KK) = 2000000000
         IMX(KK) = -IMN(KK)
         SMN(KK) = 1.E20
         SMX(KK) = -SMN(KK)
         SMA(KK) = SMN(KK)
         IF (I.EQ.3) THEN
            ICH(KK) = 1
            NCH(KK) = 1
         ELSE IF (I.EQ.7) THEN
            NCH(KK) = LENGTH
            ICH(KK) = LENGTH
         ELSE
            IF (I.EQ.5) ICH(KK) = 12 - IC/2
            END IF
         NCH(KK) = MAX (IC, NCH(KK))
 20      CONTINUE
      DOHMS = TCNT.GT.0
C                                       find integer max/min(s)
C                                       find length of chars too
      LEC = BCOUNT + 50000*ICOUNT
      LEC = MIN (LEC, ECOUNT)
      DO 140 IRNO = BCOUNT,LEC,ICOUNT
C                                       get row
         CALL TABIO ('READ', 0, IRNO, SCRTCH, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 999
         IF ((IERR.LT.0) .AND. (DOFLAG.LE.0.0)) GO TO 140
         DOIT = .TRUE.
         DO 105 LL = 1,NTVAL
            J = ITVAL(1,LL)
            I = ITVAL(3,LL)
            K = DATP(J,1)
            LENGTH = DATP(J,2) / 10
            IF (I.EQ.1) THEN
               LENGTH = MIN (128, LENGTH) * NWDPDP
               K = (K-1) * NWDPDP + 1
               CALL COPY (LENGTH, SCRTCH(K), DRES)
               IF (ABS(DRES(ITVAL(2,LL))-RTVAL(LL)).GT.ETVAL(LL)) DOIT =
     *            .FALSE.
            ELSE IF (I.EQ.2) THEN
               LENGTH = MIN (128, LENGTH)
               CALL COPY (LENGTH, SCRTCH(K), SRES)
               IF (ABS(SRES(ITVAL(2,LL))-RTVAL(LL)).GT.ETVAL(LL)) DOIT =
     *            .FALSE.
            ELSE IF (I.EQ.4) THEN
               CALL COPY (LENGTH, SCRTCH(K), RESULT)
               IF (ABS(RESULT(ITVAL(2,LL))-RTVAL(LL)).GT.ETVAL(LL))
     *            DOIT = .FALSE.
            ELSE
               M = XARR(3) + 0.1
               CALL H2CHR (M, 1, SCRTCH(K), TSTR(:M))
               I = JTRIM (TSTR(:M))
               IF (TSTR(:M).NE.CTVAL(:M)) DOIT = .FALSE.
               END IF
 105        CONTINUE
         IF ((DOINV.GT.0.0) .AND. (NTVAL.GT.0)) DOIT = .NOT.DOIT
         IF (.NOT.DOIT) GO TO 140
         DO 135 KK = 1,MCOLST
            J = COLIST(KK)
            K = DATP(J,1)
            LENGTH = LMN(KK)
C                                       integer
            IF ((KTY(KK).EQ.4) .OR. (KTY(KK).EQ.6)) THEN
C                                       get data.
               CALL COPY (LENGTH, SCRTCH(K), RESULT)
C                                       get max/min
               DO 110 I = 1,LENGTH
                  IMN(KK) = MIN (IMN(KK), RESULT(I))
                  IMX(KK) = MAX (IMX(KK), RESULT(I))
 110              CONTINUE
C                                       characters
            ELSE IF (KTY(KK).EQ.3) THEN
               LENGTH = LMN(KK)
               LENGTH = MIN (LENGTH, 128)
               K = DATP(J,1)
               CALL H2CHR (LENGTH, 1, SCRTCH(K), TSTR(:LENGTH))
               K = JTRIM (TSTR(:LENGTH))
               ICH(KK) = MAX (K, ICH(KK))
C                                       single float
            ELSE IF (KTY(KK).EQ.2) THEN
C                                       get data.
               LENGTH = MIN (128, LENGTH)
               CALL COPY (LENGTH, SCRTCH(K), SRES)
C                                       get max/min
               DO 115 I = 1,LENGTH
                  IF (SRES(I).NE.FBLANK) THEN
                     SMN(KK) = MIN (SMN(KK), SRES(I))
                     IF (SRES(I).NE.0.0) SMA(KK) = MIN (SMA(KK),
     *                  ABS(SRES(I)))
                     SMX(KK) = MAX (SMX(KK), SRES(I))
                     END IF
 115              CONTINUE
C                                       double float
            ELSE IF (KTY(KK).EQ.1) THEN
C                                       get data.
               LENGTH = MIN (128, LENGTH) * NWDPDP
               K = (K-1) * NWDPDP + 1
               CALL COPY (LENGTH, SCRTCH(K), DRES)
C                                       get max/min
               LENGTH = LENGTH / NWDPDP
               DO 120 I = 1,LENGTH
                  IF (DRES(I).NE.DBLANK) THEN
                     SRES(I) = DRES(I)
                     SMN(KK) = MIN (SMN(KK), SRES(I))
                     SMX(KK) = MAX (SMX(KK), SRES(I))
                     IF (SRES(I).NE.0.0) SMA(KK) = MIN (SMA(KK),
     *                  ABS(SRES(I)))
                     END IF
 120              CONTINUE
C                                       time
            ELSE IF (KTY(KK).EQ.12) THEN
C                                       get data.
               LENGTH = MIN (128, LENGTH)
               CALL COPY (LENGTH, SCRTCH(K), SRES)
C                                       get max/min
               DO 125 I = 1,LENGTH
                  RESULT(I) = SRES(I)
                  SMN(KK) = MIN (SMN(KK), SRES(I))
                  IMN(KK) = MIN (IMN(KK), RESULT(I))
                  IMX(KK) = MAX (IMX(KK), RESULT(I))
 125              CONTINUE
C                                       time
            ELSE IF (KTY(KK).EQ.11) THEN
C                                       get data.
               LENGTH = MIN (128, LENGTH) * NWDPDP
               K = (K-1) * NWDPDP + 1
               CALL COPY (LENGTH, SCRTCH(K), DRES)
C                                       get max/min
               LENGTH = LENGTH / NWDPDP
               DO 130 I = 1,LENGTH
                  RESULT(I) = DRES(I)
                  IMN(KK) = MIN (IMN(KK), RESULT(I))
                  IMX(KK) = MAX (IMX(KK), RESULT(I))
                  TEMP = DRES(I)
                  SMN(KK) = MIN (SMN(KK), TEMP)
 130           CONTINUE
               END IF
 135        CONTINUE
 140     CONTINUE
C                                       to characters
      DO 150 KK = 1,MCOLST
         IF ((KTY(KK).EQ.4) .OR. (KTY(KK).EQ.6)) THEN
            IMN(KK) = -10 * IMN(KK)
            K = MAX (IMX(KK), IMN(KK))
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
            IF (LENGTH.GE.NCH(KK)) THEN
               NCH(KK) = LENGTH
               ICH(KK) = 13 - LENGTH
            ELSE
               ICH(KK) = 12 - (NCH(KK) + LENGTH - 1) / 2
               END IF
         ELSE IF (KTY(KK).EQ.3) THEN
            NCH(KK) = MAX (NCH(KK), ICH(KK))
         ELSE IF (KTY(KK).LT.3) THEN
C                                       no max min found
            IF (SMN(KK).GT.SMX(KK)) THEN
               IF (LFMT.LE.2) THEN
                  SMN(KK) = -1.0
                  SMX(KK) = 1.0
                  END IF
               END IF
C                                       check format
            IF (LFMT.GT.2) THEN
               TEMP = MAX (ABS(SMN(KK)), ABS(SMX(KK)))
               IF ((TEMP.GT.0.0) .AND. (TEMP.LT.10.0**(2-2*LFMT)))
     *            IFMT(KK) = LFMT - 2
               END IF
            IF (IFMT(KK).LE.2) THEN
               IF (SMN(KK).LT.0.0) THEN
                  NCH(KK) = MAX (NCH(KK), LCH(KTY(KK),LFMT))
               ELSE
                  ICH(KK) = 2
                  END IF
            ELSE IF (SMN(KK).GT.SMX(KK)) THEN
               IF (LFMT.EQ.3) THEN
                  FMT(KK) = '(F9.4)'
                  NCH(KK) = 9
               ELSE
                  FMT(KK) = '(F12.6)'
                  NCH(KK) = 12
                  END IF
            ELSE
               IF (SMN(KK).LT.0.0) SMX(KK) = MAX (-10.*SMN(KK),
     *            ABS (SMX(KK)))
               IF (SMX(KK).EQ.0.0) THEN
                  K = 2
               ELSE
                  K = LOG10 (SMX(KK))
                  END IF
               K = MAX (1, K+1)
               IF (SMN(KK).LT.0) K = K + 1
               IF ((SMA(KK).NE.0.0) .AND. (SMA(KK).NE.1.E20)) THEN
                  I = LOG10 (SMA(KK))
               ELSE
                  I = -1
                  END IF
               IF (KTY(KK).EQ.2) THEN
                  LL = 0
               ELSE
                  LL = 3
                  END IF
               IF (LFMT.EQ.3) THEN
                  I = MAX (2+LL, -I+2)
               ELSE
                  I = MAX (5+LL, -I+2)
                  END IF
               IF (I+K+1.GT.NCH(KK)) I = NCH(KK) - K - 1
               I = MAX (0, I)
               NCH(KK) = I + K + 1
               WRITE (FMT(KK),1000) NCH(KK), I
               CALL DEFRMT (FMT(KK), ' ', I)
               END IF
         ELSE IF (KTY(KK).GT.10) THEN
            IF (IMX(KK).GT.0) ICH(KK) = 3
            IF (IMX(KK).GT.9) ICH(KK) = 2
            IF (IMX(KK).GT.99) ICH(KK) = 1
            IF (SMN(KK).LT.0.0) ICH(KK) = MIN (2, ICH(KK))
            IF (IMN(KK).LT.-9) ICH(KK) = 1
            NCH(KK) = MAX (NCH(KK), 15-ICH(KK))
            END IF
 150     CONTINUE
      IERR = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('(F',I2,'.',I2,')')
      END
