LOCAL INCLUDE 'TBOUT.INC'
C                                       Local include for TBOUT
      HOLLERITH XINNAM(3), XINCLS(2), XINEXT, XEXTFI(12)
      CHARACTER INNAM*12, INCLS*6, INEXT*2, EXTFIL*48
      REAL      XSEQ, XDISK, XVER, DOCRT, XBCOUN, XECOUN
      INTEGER   DATP(128,2), BUFFER(512), INSEQ, INDISK, INVERS, CNO,
     *   IUSER, NCOL, IBCOL(1000), BCOUNT, ECOUNT
      COMMON /INPARM/ XINNAM, XINCLS, XSEQ, XDISK, XINEXT, XVER, XEXTFI,
     *   DOCRT, XBCOUN, XECOUN
      COMMON /TBLCHR/ INNAM, INCLS, INEXT, EXTFIL
      COMMON /TBOUTC/ DATP, BUFFER, INSEQ, INDISK, INVERS, CNO, IUSER,
     *   NCOL, IBCOL, BCOUNT, ECOUNT
LOCAL END
C                                       Common for card I/O
LOCAL INCLUDE 'CARDIO.INC'
      INTEGER   EXTLUN, EXTIND
      COMMON /CARDIO/ EXTLUN, EXTIND
LOCAL END
      PROGRAM TBOUT
C-----------------------------------------------------------------------
C! Translates between AIPS tables and external files
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2004, 2007, 2009, 2012, 2015
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
C   TBOUT converts between external text files and AIPS tables.
C   AIPS adverbs:                     Use:
C     INNAME     INNAM(3)     Image name: standard defaults, wildcards
C     INCLASS    INCLS(2)     Image class: ditto
C     INSEQ      XSEQ (INSEQ) Image sequence number: ditto
C     INDISK     XDISK (INDISK)  Image disk number: 0 -> any
C     INEXT      INEXT           Extension type: '  ' => 'TA'
C     INVERS     XVER (INVERS)   Extension version number
C     OUTFILE    EXTFIL          External text file.
C     DOCRT      DOCRT           Width of output
C     BCOUNT     BCOUNT          First Row to be stored
C     ECOUNT     ECOUNT          Last Row to be stored
C-----------------------------------------------------------------------
      INTEGER   IERR, NACROS, NCH(128), FCH(128), RESULT(16), IPASS,
     *   NOPASS, NREC, NKEY, TABLUN, JCOL1(128), JCOL2(128),
     *   MAXDIM(128)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TBOUT.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA TABLUN, NREC /27, 0/
C-----------------------------------------------------------------------
C                                       Get parms, open things
      CALL TBLIIN (IERR)
      IF (IERR.NE.0) THEN
         WRITE(MSGTXT,1900)
         GO TO 990
         END IF
C                                       Open table file
      CALL TABINI ('READ', INEXT, INDISK, CNO, INVERS, CATBLK, TABLUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, INEXT, INVERS
         GO TO 990
         END IF
C                                        init counters, line size
      NREC = BUFFER(5)
      NACROS = DOCRT + 0.1
C                                        start and stop row limits
      BCOUNT = MIN(BCOUNT,NREC)
      ECOUNT = MIN(ECOUNT,NREC)
      IF (ECOUNT.LT.1) ECOUNT = NREC
C                                       Calc text output positions.
      CALL CALPOS (NACROS, NOPASS, JCOL1, JCOL2, MAXDIM, FCH, NCH)
C                                       Open external text file
      CALL OPCARD ('WRIT', EXTFIL, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Write header information.
      CALL EXWTHD (NACROS, NOPASS, MAXDIM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Loop: all columns for each pass
      DO 100 IPASS = 1, NOPASS
C                                       Label columns for this pass
         CALL LABPAS (JCOL1(IPASS), JCOL2(IPASS), FCH, NCH, RESULT,
     *      IERR)
         IF (IERR.NE.0) GO TO 950
C                                       Output loop
         CALL PUTPAS (MAXDIM(IPASS), JCOL1(IPASS), JCOL2(IPASS), FCH,
     *      NCH, IERR)
         IF (IERR.NE.0) GO TO 950
 100     CONTINUE
C                                       Close down
      CALL TABIO ('CLOS', 3, NREC, RESULT, BUFFER, IERR)
      CALL CLCARD (IERR)
      GO TO 995
C                                       write error
 950  IF (IERR.EQ.3) THEN
         WRITE (MSGTXT,1960) IERR
      ELSE
         WRITE (MSGTXT,1970) IERR
         END IF
C
 990  CALL MSGWRT (8)
C                                       close down
 995  CALL DIE (IERR, BUFFER)
 999  STOP
C-----------------------------------------------------------------------
 1030 FORMAT ('ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1900 FORMAT ('ERROR',I5,' DURING TBLIIN')
 1960 FORMAT ('ERROR',I5,' READING TABLE DATA')
 1970 FORMAT ('ERROR',I5,' WRITING OUTPUT')
      END
      SUBROUTINE TBLIIN (IERR)
C-----------------------------------------------------------------------
C   TBLIIN performs initialization for AIPS task TBOUT.  It gets the
C   adverbs, opens the catalog file and opens the external file.
C   Output: IERR    I      Error code: 0 => keep going, else quit.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      CHARACTER INTYP*2, STAT*4, PRGN*6
      INTEGER   NPARM, IROUND, IRET
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'TBOUT.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGN /'TBOUT '/
C-----------------------------------------------------------------------
C                                       AIPS init
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       get adverbs
      NPARM = 24
      IERR = 0
      CALL GTPARM (PRGN, NPARM, RQUICK, XINNAM, DATP, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       restart AIPS
      IRET = IERR
      IF (RQUICK) CALL RELPOP (IRET, DATP, IERR)
      IF (IERR.NE.0) GO TO 999
      IERR = 5
C                                       Convert characters
      CALL H2CHR (12, 1, XINNAM, INNAM)
      CALL H2CHR (6, 1, XINCLS, INCLS)
      CALL H2CHR (2, 1, XINEXT, INEXT)
      CALL H2CHR (48, 1, XEXTFI, EXTFIL)
C                                       find file
      INSEQ  = IROUND (XSEQ)
      INDISK = IROUND (XDISK)
      INVERS = IROUND (XVER)
      BCOUNT = IROUND (XBCOUN)
      BCOUNT = MAX(1,BCOUNT)
      ECOUNT = IROUND (XECOUN)
      IF (INEXT.EQ.'  ') INEXT = 'TA'
      IUSER = NLUSER
      IF (DOCRT.LE.40) DOCRT = 80
      IF (DOCRT.GT.1000) DOCRT = 1000
      CNO = 1
      INTYP = '  '
      CALL CATDIR ('SRCH', INDISK, CNO, INNAM, INCLS, INSEQ, INTYP,
     *   IUSER, STAT, DATP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, INNAM, INCLS, INSEQ, INTYP,
     *      INDISK, IUSER
         GO TO 990
         END IF
C                                       Get catblk, mark file
      CALL CATIO ('READ', INDISK, CNO, CATBLK, 'READ', DATP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      NCFILE = 1
      FVOL(1) = INDISK
      FCNO(1) = CNO
C                                       Mark table as read only
      FRW(1) = 0
C     FRW(1) = 1                        Mark as table write
      GO TO 999
C
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' FINDING INPUT ADVERBS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   'DISK=',I2,' USER=',I5)
 1020 FORMAT ('ERROR',I5,' READING CATBLK FROM CATALOG FILE')
      END
      SUBROUTINE CALPOS (NACROS, NOPASS, JCOL1, JCOL2, MAXDIM, FCH, NCH)
C-----------------------------------------------------------------------
C   CALPOS CALculates the POSition of variables output to a text file
C   Input:  NACROS    I     Number of columns of text file
C   Common: NCOL      I     Number of columns (variables) in table
C   Common: DATP(*,2) I     Array containing variable specification
C   Output: NOPASS    I     Number of passes to output all variables
C           JCOL1(*)  I     Start column output in pass
C           JCOL2(*)  I     End column output in pass
C           MAXDIM(*) I     Maximum dimension of table variables each
C                           pass
C           FCH(*)    I     Array of starting character positions for
C                           columns
C           NCH(*)    I     Array of string lengths for columns
C   Common: IBCOL(*)  I     Array describing var. location in output
C-----------------------------------------------------------------------
      INTEGER   NACROS, NOPASS, JCOL1(*), JCOL2(*), MAXDIM(*),
     *   FCH(*), NCH(*)
C                                       STRTCL=Start col of data print
C                                       Define character and bit types
      INTEGER   STRTCL, CHARTY, BITTYP
      PARAMETER (STRTCL=9,CHARTY=3,BITTYP=7)
      INTEGER   VTYPE, VARLEN, J, BCOL
      INCLUDE 'TBOUT.INC'
C-----------------------------------------------------------------------
C                                        Force new pass on first
C                                        variable
      BCOL = NACROS + 2
      NOPASS = 0
C                                        For all columns in table
      DO 100 J = 1,NCOL
C                                        Length of datum
         NCH(J) = VARLEN(J)
C                                        Another pass if more than
C                                        NACROS?
         IF ((BCOL+NCH(J)).GE.NACROS) THEN
C                                        Begin at start column
            BCOL = STRTCL
            NOPASS = NOPASS + 1
C                                        Init Max array dimension each
C                                        pass
            MAXDIM(NOPASS) = 1
C                                        Start Column for each pass
            JCOL1(NOPASS) = J
            END IF
C                                        Set first columns.
         FCH(J) = BCOL
C                                        Save var location for header
C                                        write
         IBCOL(J) = BCOL + (NOPASS-1) * 1000
C                                        Update next start col
         BCOL = BCOL + NCH(J)
C                                        Assume current col is last in
C                                        pass
         JCOL2(NOPASS) = J
C                                        Variable type is in 1's digit
         VTYPE = MOD( DATP(J,2), 10)
C                                        If not bit or char
         IF ((VTYPE.NE.CHARTY) .AND. (VTYPE.NE.BITTYP)) THEN
C                                        get table array dimension
            MAXDIM(NOPASS) = MAX (MAXDIM(NOPASS), (DATP(J,2)/10))
            END IF
 100     CONTINUE
      RETURN
      END
      SUBROUTINE LABPAS (JBEGIN, JEND, FCH, NCH, RESULT, IERR)
C-----------------------------------------------------------------------
C   LABPAS LABels one PASs of a write of an AIPS table to an
C          external text table.
C   Input:  JBEGIN I     Start column output in pass
C           JEND   I     End column output in pass
C           FCH(*) I     Array of starting character positions for
C                        columns
C           NCH(*) I     Array of string lengths for columns
C           RESULT(*) I  Array for receiving TABIO results.
C   Common: DATP, BUFFER
C   Output: IERR   I     Error code: 0 => OK,
C                        2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   JBEGIN, JEND, FCH(*), NCH(*), RESULT(*), IERR
C
      CHARACTER LINE*1024, CTEMP*8
      INTEGER   I, J, IP, NCP, JJ, JTRIM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TBOUT.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       No. characters per line.
      NCP = FCH(JEND) + NCH(JEND)
C                                       Column numbers
      LINE = 'COL. NO.  '
      DO 100 J = JBEGIN,JEND
         WRITE (LINE(FCH(J)+2:FCH(J)+6),1090) J
 100     CONTINUE
      CALL WRCARD ( LINE, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Prepare page titles
      LINE = '     ROW  '
      DO 110 J = JBEGIN,JEND
C                                       Column label
         CALL TABIO ('READ', 3, J, RESULT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 960
         I = 8
         IP = FCH(J) + 3
         CALL H2CHR (I, 1, RESULT, CTEMP)
         JJ = JTRIM (CTEMP)
         LINE(IP:IP+7) = CTEMP
 110     CONTINUE
      CALL WRCARD ( LINE, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Units
      LINE = '  NUMBER  '
      DO 120 J = JBEGIN,JEND
         CALL TABIO ('READ', 4, J, RESULT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 960
         IP = FCH(J) + 3
         CALL H2CHR (8, 1, RESULT, CTEMP)
         JJ = JTRIM (CTEMP)
         LINE(IP:IP+7) = CTEMP
 120     CONTINUE
      CALL WRCARD( LINE, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Mark Begin Pass
      LINE = '***BEGIN*PASS***'
      CALL WRCARD( LINE, IERR)
      IF (IERR.NE.0) GO TO 970
      GO TO 999
C                                       read error
 960  IERR = 3
      GO TO 995
C                                       write error
 970  IERR = 2
C
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1090 FORMAT (I5)
      END
      SUBROUTINE PUTPAS (MAXDIM, JBEGIN, JEND, FCH, NCH, IERR)
C-----------------------------------------------------------------------
C  PUTPAS writes one pass of an AIPS table to an external text table.
C   Input:  MAXDIM I     Maximum dimension of table arrays
C           JBEGIN I     Start column output in pass
C           JEND   I     End column output in pass
C           FCH(*) I     Array of starting character positions for
C                        columns
C           NCH(*) I     Array of string lengths for columns
C   Common: DATP, BUFFER, BCOUNT, ECOUNT
C   Output: IERR   I     Error code: 0 => OK,
C                        2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER MAXDIM, JBEGIN, JEND, FCH(*), NCH(*), IERR
C
      CHARACTER LINE*1024, CTEMP*80, STRING*1024
      INTEGER   CHARTY, BITTYP, JTRIM, JJ
C                                       Define character and bit types
      INCLUDE 'INCS:PUVD.INC'
      PARAMETER (CHARTY=3,BITTYP=7)
      INTEGER   IRNO, RESLI(XBPRSZ), RESULT(XBPRSZ), I, J, K, L, M, N,
     *   ROWB(XBPRSZ), BITS(64), RTYPE, IROW, IEL, NCP, JTYPE, JDIMEN,
     *   LRNO
      LOGICAL   RESLO(XBPRSZ), ROWFLG
      REAL      RES4(XBPRSZ)
      DOUBLE PRECISION RES8(XBPRSZ/2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TBOUT.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (RESULT, RES8, RES4, RESLI, RESLO)
C-----------------------------------------------------------------------
      IERR = 0
C                                       Max num chars = last col + len
      NCP = FCH(JEND) + NCH(JEND)
      IROW = 0
C                                       Output loop
      LRNO = 0
      DO 60 IRNO = BCOUNT,ECOUNT
C                                       Keep track of actual row written
         IROW = IROW + 1
C                                       Clear output
         LINE = ' '
C                                       Loop over element in arrays.
         DO 60 IEL = 1,MAXDIM
            DO 50 J = JBEGIN,JEND
C                                       get data.
               CALL GETCOL (IRNO, J, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *            ROWB, IERR)
C                                       if row is flagged
               ROWFLG = (IERR.LT.0)
               IF (ROWFLG) IERR = 0
               IF (IERR.NE.0) GO TO 960
C                                       Get variable type
               JTYPE = MOD (RTYPE, 10)
C                                       See which have data, get
C                                       dimension
               JDIMEN = RTYPE/10
C                                       for arrays with 0 elements or
C                                       arrays with smaller than IEL
C                                       elements
               IF ((JDIMEN.LT.IEL) .OR. ((IEL .GT. 1) .AND.
     *             ((JTYPE.EQ.CHARTY).OR.(JTYPE.EQ.BITTYP))))  THEN
                  L = FCH(J)
C                                       init with blanks
                  LINE(L:L+NCH(J)-1) = ' '
                  LINE(L+NCH(J)-2:L+NCH(J)-2) = ''''
                  LINE(L+NCH(J)-1:L+NCH(J)-1) = ''''
               ELSE
C                                       array has >= IEL elements
C                                       if bit or char type
                  IF ((JTYPE.EQ.CHARTY).OR.(JTYPE.EQ.BITTYP)) THEN
C                                       work to right justify
                     L = FCH(J)+NCH(J)-JDIMEN-2
C                                       First quote
                     LINE(L:L) = ''''
C                                       jump for character or bit data
                     IF (JTYPE.EQ.CHARTY) THEN
                        CALL H2CHR (JDIMEN, 1, RESULT, STRING)
                        JJ = JTRIM (STRING(:JDIMEN))
                        LINE(L+1:L+JDIMEN) = STRING(:JDIMEN)
                     ELSE
C                                       else bit type
C                                       calc number of words with NBITWD
C                                       bits
                        N = ((JDIMEN - 1) / NBITWD) + 1
                        DO 40 I = 1,N
                           M = NBITWD
C                                       last word has only a few bits
                           IF (I.EQ.N) M = JDIMEN - ((N-1)*NBITWD)
C                                       if fewer bits than fill a word
                           M = MIN (M, JDIMEN)
C                                       unpack word into an array
                           CALL ZGTBIT (M, RESULT(I),
     *                        BITS(((I-1)*NBITWD)+1))
  40                       CONTINUE
                        WRITE (LINE(L+1:L+JDIMEN),1230)
     *                             (BITS(K), K = 1,JDIMEN)
C                                       end if char type else bit
                        END IF
C                                       Final quote
                     LINE(L+JDIMEN+1:L+JDIMEN+1) = ''''
                  ELSE
C                                       else numeric, is var dimension
C                                       ok
                     IF (JDIMEN.GE.IEL) THEN
C                                       check floats for indefinite values
                        IF (JTYPE.EQ.1) THEN
                           IF (RES8(IEL).EQ.DBLANK) THEN
                              WRITE (CTEMP,1201)
                           ELSE
                              WRITE (CTEMP,1101) RES8(IEL)
                              END IF
                        ELSE IF (JTYPE.EQ.2) THEN
                           IF (RES4(IEL).EQ.FBLANK) THEN
                              WRITE (CTEMP,1201)
                           ELSE
                              WRITE (CTEMP,1102) RES4(IEL)
                              END IF
                        ELSE IF (JTYPE.EQ.4) THEN
                           WRITE (CTEMP,1103) RESLI(IEL)
                        ELSE IF (JTYPE.EQ.5) THEN
                           WRITE (CTEMP,1104) RESLO(IEL)
                        ELSE IF (JTYPE.EQ.6) THEN
                           WRITE (CTEMP,1105) RESULT(IEL)
                           END IF
C
                        LINE(FCH(J):FCH(J)+NCH(J)-1) = CTEMP(1:NCH(J))
C                                       end if var dimension < IEL
                        END IF
C                                       end if char or bit
                     END IF
C                                       if has elements
                  END IF
C                                       end for all columns loop
 50            CONTINUE
C                                       row number, minus if flaged
            IF (ROWFLG) THEN
               WRITE (LINE(1:8),1100) -IROW
            ELSE
               WRITE (LINE(1:8),1100) IROW
               END IF
C                                       do output finally
            CALL WRCARD ( LINE(1:NCP), IERR)
            IF (IERR.NE.0) THEN
               WRITE(MSGTXT, 1900) LINE(1:40)
               CALL MSGWRT(8)
               GO TO 970
               END IF
  60        CONTINUE
C                                       Mark End Pass
      LINE = '***END*PASS***'
      CALL WRCARD( LINE, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       loop for more columns
      GO TO 999
 960  IERR = 3
      GO TO 999
 970  IERR = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (I8)
 1101 FORMAT (1PD24.15)
 1102 FORMAT (1PE15.6)
 1103 FORMAT (I11,4X)
 1104 FORMAT (4X,L1,4X)
 1105 FORMAT (I6,4X)
 1201 FORMAT (' ''INDE''      ')
 1230 FORMAT (64I1)
 1900 FORMAT ('WRITE ERROR; LINE(1:40)=',A40)
      END
      INTEGER FUNCTION VARLEN (VARNUM)
C-----------------------------------------------------------------------
C Calculate the length in characters of the different data types
C Input  VARNUM  I  Index to Variable
C Output LENGTH  I  Integer length of variable in number of characters
C
C-----------------------------------------------------------------------
      INTEGER VARNUM
C
      INTEGER LENGTH, VTYPE, LCH(6), CHARTY, BITTYP
C                                       Define character and bit types
      PARAMETER (CHARTY=3,BITTYP=7)
      INCLUDE 'TBOUT.INC'
C                                       Length of types in characters
C                                       for:
C                                       R*8, R*4, C*8, I*4, L*2, I*2
      DATA LCH /24, 15, 8, 11, 8, 8/
C-----------------------------------------------------------------------
      VTYPE = MOD(DATP(VARNUM,2), 10)
C                                       If char or bit type
      IF ((VTYPE.EQ.CHARTY) .OR. (VTYPE.EQ.BITTYP)) THEN
C                                       length is hidden in tens digits
         LENGTH = (DATP(VARNUM,2) / 10) + 3
      ELSE
C                                       length is defined by formats
         LENGTH = LCH(VTYPE)
         END IF
C                                       never less than 9 chars
      VARLEN = MAX (LENGTH, 9)
      RETURN
      END
      SUBROUTINE EXWTHD (NACROS, NOPASS, MAXDIM, IERR)
C-----------------------------------------------------------------------
C   EXWTHD writes header information for a specified AIPS table
C   to an external text file.
C   Input:
C     NACROS     I    Number of characters per line of output text
C     NOPASS     I    Number of passes to output all columns of table
C     MAXDIM    I(*)  Maximum dimension of Array in each pass
C   Inputs from common:
C     BUFFER(*)  I    Table I/O buffer (table opened and closed
C                     outside of this routine).
C     TYPE       I    Table type (2 char)
C     VER        I    Table version number
C     IBCOL      I    First col no. + (no. pass - 1) * 1000
C   Output:
C     IER        I    Error number: 0 => none, else count of errors #
C-----------------------------------------------------------------------
      INTEGER   NACROS, NOPASS, MAXDIM(*), IERR
C
      CHARACTER KWNAME*8, PAD*16, FORM(7)*8, TYPTAB(2)*8, LINE*80,
     *   FRMOUT*8, ITOCHR*2, ATIME*8, ADATE*12
      DOUBLE PRECISION DVALUE
      REAL   RVALUE
      INTEGER   IPAIR, I, II, IVALUE, SRTORD, KTYPE, NNKEY, ILEN,
     *   TBMAXD, TABTYP,IPASS, ID(3), IT(3)
      INCLUDE 'TBOUT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      DATA FORM /'D24.15', 'E15.6', 'A', 'I11', 'A1', 'I6', ' '/
      DATA TYPTAB /'TABLE   ', 'BINTABLE'/, PAD /' '/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find max dimension of the table
      TBMAXD = 0
      DO 50 IPASS = 1, NOPASS
         TBMAXD = MAX (TBMAXD, MAXDIM(IPASS))
 50      CONTINUE
C                                       Assume 3d table, unless max dim
C                                       1
      TABTYP = 2
      IF (TBMAXD.EQ.1) TABTYP = 1
C                                       Everything you need to know
C                                       about the columns.
      CALL GETHUT (NCOL, DATP, BUFFER, TTYPE, TUNIT, TFCODE, TWIDTH,
     *   IERR)
C                                       Tell user table and type
      WRITE (MSGTXT,1005) INEXT
      CALL MSGWRT (5)
      WRITE (LINE,1050) TYPTAB(TABTYP)
      CALL WRCARD (LINE, IERR)
      WRITE (LINE,1051)
      CALL WRCARD (LINE, IERR)
      WRITE (LINE,1052)
      CALL WRCARD (LINE, IERR)
      WRITE (LINE,1053) NACROS
      CALL WRCARD (LINE, IERR)
      WRITE (LINE,1054) ECOUNT-BCOUNT+1
      CALL WRCARD (LINE, IERR)
      WRITE (LINE,1055)
      CALL WRCARD (LINE, IERR)
      WRITE (LINE,1056)
      CALL WRCARD (LINE, IERR)
C                                       Number of passes
      WRITE (LINE,2056) NOPASS
      CALL WRCARD (LINE, IERR)
      WRITE (LINE,1057) NCOL
      CALL WRCARD (LINE, IERR)
      WRITE (LINE,1058) INEXT
      CALL WRCARD (LINE, IERR)
      WRITE (LINE,1059) INVERS
      CALL WRCARD (LINE, IERR)
      DO 80 I = 1,NCOL
C                                       Turn I into a 2 character string
         IF (I.LT.10) THEN
            WRITE (ITOCHR,1060) I
         ELSE
            WRITE (ITOCHR,1061) I
            END IF
         WRITE (LINE,1062) ITOCHR, IBCOL(I)
         CALL WRCARD (LINE, IERR)
         IF (IERR.NE.0) GO TO 900
         II = TFCODE(I)
C                                       Get char or bit format
C                                       into the right form.
         IF ((II.EQ.3) .OR. (II .EQ. 7)) THEN
            WRITE (FRMOUT,1064) TWIDTH(I)
            CALL CHTRIM (FRMOUT, 8, FRMOUT, ILEN)
C                                       Char format
            IF (II.EQ.3) FORM(3) = 'A' // FRMOUT(1:ILEN)
C                                       Bit format
            IF (II.EQ.7) FORM(7) = 'X' // FRMOUT(1:ILEN)
            END IF
C                                       Get correct format for file type
         FRMOUT = FORM(II)
         WRITE (LINE,1068) ITOCHR, FRMOUT, I
         CALL WRCARD (LINE, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Get dimension of field
         ILEN = DATP(I,2) / 10
         WRITE (LINE,1070) ITOCHR, ILEN, I
         CALL WRCARD (LINE, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Pad 1 to 11 blanks
         ILEN = MAX(1,MIN(11,19-LEN(TTYPE(I))))
         WRITE (LINE,1072) ITOCHR, TTYPE(I), PAD(1:ILEN), I
         CALL WRCARD (LINE, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1074) ITOCHR, TUNIT(I), I
         CALL WRCARD (LINE, IERR)
         IF (IERR.NE.0) GO TO 900
 80      CONTINUE
C                                       Do all KeyWord value pairs.
      NNKEY = BUFFER(53)
      IF (NNKEY.GE.1) THEN
         DO 240 IPAIR = 1,NNKEY
            CALL GTPAIR (IPAIR, BUFFER, KWNAME, DVALUE, KTYPE)
            IF (KTYPE.EQ.0) GO TO 240
C                                       Double Precision.
            IF (KTYPE.EQ.1) THEN
               WRITE (LINE,1120) KWNAME, DVALUE
C                                       Real.
            ELSE IF (KTYPE.EQ.2) THEN
               RVALUE = DVALUE
               WRITE (LINE,1140) KWNAME, RVALUE
C                                       Character.
            ELSE IF (KTYPE.EQ.3) THEN
               WRITE (LINE,1160) KWNAME, DVALUE
C                                       Long Integer.
            ELSE IF (KTYPE.EQ.4) THEN
               IF (DVALUE.LT.0.0D0) THEN
                  IVALUE = DVALUE - 0.01D0
               ELSE
                  IVALUE = DVALUE + 0.01D0
                  END IF
               WRITE (LINE,1180) KWNAME, IVALUE
C                                       Logical.
            ELSE IF (KTYPE.EQ.5) THEN
               IF (DVALUE.GT.0.0D0) THEN
                  WRITE (LINE,1200) KWNAME
               ELSE
                  WRITE (LINE,1202) KWNAME
                  END IF
C                                       Integer.
            ELSE IF (KTYPE.EQ.6) THEN
               IF (DVALUE.LT.0.0D0) THEN
                  IVALUE = DVALUE - 0.01D0
               ELSE
                  IVALUE = DVALUE + 0.01D0
                  END IF
               WRITE (LINE,1220) KWNAME, IVALUE
               END IF
            CALL WRCARD ( LINE, IERR)
 240        CONTINUE
         END IF
C                                       Sort order.
      SRTORD = BUFFER(43)
      IF (SRTORD.NE.0) THEN
         WRITE (LINE,1230) SRTORD
         CALL WRCARD (LINE, IERR)
         END IF
C                                       HISTORY Card with File name
      WRITE (LINE,1300) INEXT, INVERS, INNAM, INCLS, INSEQ
      CALL WRCARD (LINE, IERR)
C
      WRITE (LINE,1400) EXTFIL(1:30)
      CALL WRCARD (LINE, IERR)
C                                       Date and time
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (LINE,1500) ADATE, ATIME
      CALL WRCARD (LINE, IERR)
C                                       END card.
      CALL WRCARD ('END ',IERR)
      GO TO 999
C                                       Error on I/O
 900  WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('Writing ASCII table of type ',A2)
 1050 FORMAT ('XTENSION= ''',A8,'''',11X,'/ extension type')
 1051 FORMAT ('BITPIX  =',20X,'8',1X,'/ printable ASCII codes')
 1052 FORMAT ('NAXIS   =',20X,'2',1X,'/ Table is a matrix')
 1053 FORMAT ('NAXIS1  =',15X,I6,1X,'/ Max. no. of characters/pass')
 1054 FORMAT ('NAXIS2  =',13X,I8,1X,'/ Number of entries in table')
 1055 FORMAT ('PCOUNT  =',20X,'0',1X,'/ Random parameter count')
 1056 FORMAT ('GCOUNT  =',20X,'1',1X,'/ Group count')
 2056 FORMAT ('NOPASS  =',18X,I3,1X,'/ Number of passes thru table')
 1057 FORMAT ('TFIELDS =',19X,I2,1X,'/ Number of fields in each row')
 1058 FORMAT ('EXTNAME = ''AIPS ',A2,1X,'''',11X,'/ AIPS table file')
 1059 FORMAT ('EXTVER  = ',15X,I5,1X,'/ Version Number of table')
 1060 FORMAT (I1,' ')
 1061 FORMAT (I2)
 1062 FORMAT ('TBCOL',A2,' = ',15X,I5,1X,'/ Starting char. pos. of',
     *   ' field',I3)
 1064 FORMAT (I6)
 1068 FORMAT ('TFORM',A2,' = ''',A8,'''',11X,'/ Fortran format',
     *   ' of field',I3)
 1070 FORMAT ('TFDIM',A2,' = ',15X,I5,1X,'/ Dimension of field',I3)
 1072 FORMAT ('TTYPE',A2,' = ''',A24,'''',A11,'/ type (heading)',
     *   ' of field',I3)
 1074 FORMAT ('TUNIT',A2,' = ''',A8,'''',11X,'/ physical units',
     *   ' of field',I3)
 1120 FORMAT (A8,'= ',1PD25.17)
 1140 FORMAT (A8,'= ',1PE15.7)
 1160 FORMAT (A8,'= ''',A8,'''')
 1180 FORMAT (A8,'= ',I12)
 1200 FORMAT (A8,'=',20X,'T')
 1202 FORMAT (A8,'=',20X,'F')
 1220 FORMAT (A8,'= ',I6)
 1230 FORMAT ('ISORTORD =',I20)
 1300 FORMAT ('HISTORY TBOUT  /', A2, ' table version', I3,
     *   ' of INNAME=''',A12,'.',A6,'.',I4,'''')
 1400 FORMAT ('HISTORY TBOUT  / copied to the text file ', A)
 1500 FORMAT ('HISTORY TBOUT  / ',A,A)
 1900 FORMAT ('EXWTHD: WRITE ERROR',I7)
      END
      SUBROUTINE WRCARD (LINE, IERR)
C-----------------------------------------------------------------------
C WRCARD WRites a fits CARD.
C Input:  LINE C*(*) Output fits card
C Output: IERR I     Error code, 0 => ok
C-----------------------------------------------------------------------
      CHARACTER LINE*(*)
      INTEGER IERR
C
      INTEGER   JTRIM
      INCLUDE 'CARDIO.INC'
C-----------------------------------------------------------------------
      CALL ZTXIO ('WRIT', EXTLUN, EXTIND, LINE(1:JTRIM(LINE)), IERR)
      RETURN
      END
      SUBROUTINE OPCARD (OPCODE, EXTFIL, IERR)
C-----------------------------------------------------------------------
C OPens file for reading or writing FITS Text Cards.
C Input:  OPCODE C*4   Opcode, 'READ' or 'WRIT'
C         EXTFIL C*(*) External File Name
C Output: IERR I       Error code, 0 => ok
C-----------------------------------------------------------------------
      CHARACTER OPCODE, EXTFIL*(*)
      INTEGER IERR
C
      LOGICAL F
      INCLUDE 'CARDIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA F/.FALSE./
C-----------------------------------------------------------------------
      EXTLUN = 10
      CALL ZTXOPN (OPCODE, EXTLUN, EXTIND, EXTFIL, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT(8)
         END IF
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' OPENING EXTERNAL FILE')
      END
      SUBROUTINE CLCARD (IERR)
C-----------------------------------------------------------------------
C CLoses a file for reading or writing FITS Text Cards.
C Output: IERR I     Error code, 0 => ok
C-----------------------------------------------------------------------
      INTEGER IERR
C
      INCLUDE 'CARDIO.INC'
C-----------------------------------------------------------------------
      CALL ZTXCLS (EXTLUN, EXTIND, IERR)
      RETURN
      END
