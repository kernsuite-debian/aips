LOCAL INCLUDE 'TBIN.INC'
C                                       Local include for TBIN
      HOLLERITH XNAME(3), XCLASS(2), XEXTFI(12)
      CHARACTER NAME*12, CLASS*6, OEXT*2, EXTFIL*48
      REAL      XSEQ, XDISK, XBCOUN, XECOUN
      INTEGER   DATP(128,2), BUFFER(512), OSEQ, ODISK, OVERS, CNO,
     *   IUSER, NCOL, IBCOL(1000), BCOUNT, ECOUNT
      COMMON /INPARM/ XNAME, XCLASS, XSEQ, XDISK, XEXTFI, XBCOUN, XECOUN
      COMMON /TBLCHR/ NAME, CLASS, OEXT, EXTFIL
      COMMON /TBINC/ DATP, BUFFER, OSEQ, ODISK, OVERS, CNO, IUSER,
     *   NCOL, IBCOL, BCOUNT, ECOUNT
LOCAL END
C                                       Common for card I/O
LOCAL INCLUDE 'CARDIO.INC'
      INTEGER   EXTLUN, EXTIND
      COMMON /CARDIO/ EXTLUN, EXTIND
LOCAL END
      PROGRAM TBIN
C-----------------------------------------------------------------------
C! Translates between AIPS tables and external files
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2002, 2007, 2009, 2011-2012, 2015
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
C   TBIN converts between external text files and AIPS tables.
C   AIPS adverbs:                     Use:
C     NAME       NAME(3)         Image name: standard defaults, wildcards
C     OUTCLASS   CLASS(2)        Image class: ditto
C     OSEQ       XSEQ (OSEQ)     Image sequence number: ditto
C     ODISK      XDISK (ODISK)   Image disk number: 0 -> any
C     INFILE     EXTFIL          External text file.
C     BCOUNT     BCOUNT          First Row to be stored
C     ECOUNT     ECOUNT          Last Row to be stored
C-----------------------------------------------------------------------
      INTEGER   IERR, NACROS, IPASS, NOPASS, NREC, JCOL1(128),
     *   JCOL2(128), MAXDIM(128), OUTROW
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TBIN.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Get parms, open things
      CALL TBLIIN (IERR)
      IF (IERR.NE.0) THEN
         WRITE(MSGTXT,1900)
         GO TO 990
         END IF
C                                       Open external text file
      CALL OPCARD ('READ', EXTFIL, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Read header information.
      CALL EXRDHD (NREC, NACROS, NOPASS, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1900)
         GO TO 990
         END IF
C                                        start and stop row limits
      BCOUNT = MIN (BCOUNT,100000000)
      ECOUNT = MIN (ECOUNT,100000000)
      IF (ECOUNT.LT.1) ECOUNT = 100000000
C                                       end must be after beginning
      ECOUNT = MAX (BCOUNT, ECOUNT)
C                                       Calc text output positions.
      CALL GETPOS (JCOL1, JCOL2, MAXDIM)
C                                       Loop: all columns for each pass
      DO 100 IPASS = 1,NOPASS
C                                       Read Column Label for this pass
         CALL GETLAB (IERR)
         IF (IERR.NE.0) GO TO 950
C                                       Input/Output loop
         CALL GETPAS (NACROS, MAXDIM(IPASS), JCOL1(IPASS), JCOL2(IPASS),
     *      OUTROW, IERR)
         IF (IERR.NE.0) GO TO 950
 100     CONTINUE
C                                       Warn user if input number of
C                                       records does not match output
      IF (NREC.NE.OUTROW) THEN
         WRITE (MSGTXT,1100,ERR=995) OUTROW
         CALL MSGWRT (5)
         END IF
C                                       Update number of records
      NREC = OUTROW
C                                       Close down
      CALL TABIO ('CLOS', 3, NREC, BUFFER, BUFFER, IERR)
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
 1100 FORMAT ('Warning: ',I8,' Records Output ')
 1900 FORMAT ('ERROR',I5,' DURING TBLIIN')
 1960 FORMAT ('ERROR',I5,' WRITING TABLE DATA')
 1970 FORMAT ('ERROR',I5,' READING TEXT INPUT')
      END
      SUBROUTINE TBLIIN (IERR)
C-----------------------------------------------------------------------
C   TBLIIN performs initialization for AIPS task TBIN.  It gets the
C   adverbs, opens the catalog file and opens the external file.
C   Output: IERR    I      Error code: 0 => keep going, else quit.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      CHARACTER  INTYP*2, STAT*4, PRGN*6
      INTEGER  NPARM, IROUND, IRET
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'TBIN.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGN /'TBIN  '/
C-----------------------------------------------------------------------
C                                       AIPS init
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       get adverbs
      NPARM = 21
      IERR = 0
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAME, DATP, IERR)
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
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      CALL H2CHR (48, 1, XEXTFI, EXTFIL)
C                                       find file
      OSEQ   = IROUND (XSEQ)
      ODISK  = IROUND (XDISK)
      BCOUNT = IROUND (XBCOUN)
      BCOUNT = MAX (1, BCOUNT)
      ECOUNT = IROUND (XECOUN)
      IUSER  = NLUSER
      CNO = 1
      INTYP = '  '
      CALL CATDIR ('SRCH', ODISK, CNO, NAME, CLASS, OSEQ, INTYP,
     *   IUSER, STAT, DATP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAME, CLASS, OSEQ, INTYP,
     *      ODISK, IUSER
         GO TO 990
         END IF
C                                       Get catblk, mark file
      CALL CATIO ('READ', ODISK, CNO, CATBLK, 'WRIT', DATP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      NCFILE = 1
      FVOL(1) = ODISK
      FCNO(1) = CNO
C                                       Mark table as write
      FRW(1) = 1
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
      END
      SUBROUTINE GETPOS (JCOL1, JCOL2, MAXDIM)
C-----------------------------------------------------------------------
C   GETs the POSition of variables read from a text file
C   Input:
C   Common: NCOL      I     Number of columns (variables) in table
C   Common: IBCOL(*)  I     Array describing var. location in output
C   Output: JCOL1(*)  I     Start column output in pass
C           JCOL2(*)  I     End column output in pass
C           MAXDIM(*) I     Maximum dimension of table variables each pass
C   Common: DATP(*,2) I     Array containing variable specification
C-----------------------------------------------------------------------
      INTEGER   JCOL1(*), JCOL2(*), MAXDIM(*)
C                                       STRTCL=Start col of data print
C                                       Define character and bit types
      INTEGER   STRTCL, CHARTY, BITTYP
      PARAMETER (STRTCL=9,CHARTY=3,BITTYP=7)
      INTEGER   VTYPE, J, IPASS, NOPASS
      INCLUDE 'TBIN.INC'
C-----------------------------------------------------------------------
      IPASS = 0
C                                        For all columns in table
      DO 100 J = 1,NCOL
C                                        Pass containing variable
         NOPASS  = 1 + (IBCOL(J)/1000)
C                                        Another pass if > NACROS?
         IF (IPASS.NE.NOPASS) THEN
C                                        Begin at start column
            IPASS = NOPASS
C                                        Init Max array dim each pass
            MAXDIM(NOPASS) = 1
C                                        Start Column for each pass
            JCOL1(NOPASS) = J
            END IF
C                                        current col is last in pass
         JCOL2(NOPASS) = J
C                                        variable type is in data buffer
         VTYPE = MOD (DATP(J,2), 10)
C                                        If not bit or char
         IF ((VTYPE.NE.CHARTY) .AND. (VTYPE.NE.BITTYP)) THEN
C                                        get table array dimension
            MAXDIM(NOPASS) = MAX (MAXDIM(NOPASS), DATP(J,2)/10)
            END IF
 100     CONTINUE
      RETURN
      END
      SUBROUTINE GETLAB (IERR)
C-----------------------------------------------------------------------
C   GETLAB Reads the label for one pass of a read of an AIPS table from
C          an external text table.
C   Output: IERR   I     Error code: 0 => OK,
C                        3 => error reading
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER ITRY, OUTLEN
      LOGICAL EOF
      CHARACTER LINE*1024
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       try to find BEGIN PASS marker
      DO 100 ITRY = 1,40
         CALL RDCARD (LINE, EOF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       remove leading and trailing blanks
         CALL CHTRIM (LINE, 80, LINE, OUTLEN)
C                                       If not first column, print warning
         IF (LINE(1:3).EQ.'***'.AND.
     *      (LINE(4:4).EQ.'B'.OR.LINE(4:4).EQ.'b')) GO TO 999
 100     CONTINUE
C                                       read error
 970  IERR = 3
      WRITE(MSGTXT,1970,ERR=990)
      CALL MSGWRT (8)
      GO TO 999
 990  IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1970 FORMAT ('LABEL READ ERROR')
      END
      SUBROUTINE GETPAS (NACROS, MAXDIM, JBEGIN, JEND, OUTROW, IERR)
C-----------------------------------------------------------------------
C   GETPAS reads one pass of an AIPS table from an external text table.
C   Input:  NACROS I     Number of characters to search on each card
C           MAXDIM I     Maximum dimension of table arrays
C           JBEGIN I     Start column output in pass
C           JEND   I     End column output in pass
C   Common: DATP, BUFFER
C   Output: OUTROW I     Number of Rows written to Table File
C           IERR   I     Error code: 0 => OK,
C                        2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   NACROS, MAXDIM, JBEGIN, JEND, OUTROW, IERR
C
      CHARACTER LINE*1024, STRING*1024
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   CHARTY, BITTYP, MAXIND
C                                       Define character and bit types
      PARAMETER (CHARTY=3,BITTYP=7,MAXIND=64000)
      INTEGER   IRNO, IROW, INROW, RESLI(XBPRSZ), NPNT, RESULT(XBPRSZ),
     *   I, J, K, M, N, ROWB(XBPRSZ), BITS(64), RTYPE, LROW, JTYPE,
     *   JDIMEN, NCHAR, ILOGIC, IDELTA, ROWORD(MAXIND), OUTLEN, IEL
      LOGICAL   RESLO(XBPRSZ), EOF, ROWFLG
      REAL      RES4(XBPRSZ)
      DOUBLE PRECISION RES8(XBPRSZ/2), X
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TBIN.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (RESULT, RES8, RES4, RESLI, RESLO)
C-----------------------------------------------------------------------
      IERR = 0
      ERRNUM = 0
C                                       Mark each pass
      WRITE (MSGTXT,1000,ERR=990) JBEGIN, JEND
      CALL MSGWRT (5)
C                                       count rows input and output
      IROW = 0
      OUTROW = 0
      LROW = 0
C                                       init difference between read and
C                                       expected row number
      IDELTA = 0
C                                       Input loop
      DO 70 IRNO = 1,100000000
C                                       Loop over element in arrays.
         DO 60 IEL = 1,MAXDIM
C                                       read in a line
            CALL RDCARD (LINE, EOF, IERR)
            IF (IERR.NE.0) GO TO 970
C                                       remove leading/trailing blanks
            CALL CHTRIM (LINE, NACROS, LINE, OUTLEN)
C                                       if special data marker
            IF (LINE(1:3).EQ.'***') THEN
C                                       if end of data
               IF ((LINE(4:4).EQ.'E') .OR. (LINE(4:4).EQ.'e')) THEN
C                                       if array is not complete
                  IF (IEL.NE.1) THEN
                     WRITE (MSGTXT,1901,ERR=980) IRNO, IEL
                     CALL MSGWRT (8)
                     GO TO 970
                     END IF
C                                       force last write to disk
                  CALL PUTCOL (0, J, DATP, LROW, BUFFER, RESULT, ROWB,
     *               IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1900) IERR, 'LAST PUTCOL'
                     CALL MSGWRT (8)
                     GO TO 960
                     END IF
                  GO TO 999
C                                       else bad marker
               ELSE
                  WRITE(MSGTXT,1902,ERR=980) IRNO, IEL
                  CALL MSGWRT (8)
                  GO TO 970
                  END IF
               END IF
C                                       start at beginning of card
            NPNT = 1
C                                       get a row number
            CALL GETNUM (LINE, NACROS, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 975
C                                       row flagged if number is < 0
            IF (X.GE.0) THEN
               ROWFLG = .FALSE.
               INROW = X + 0.01
            ELSE
               ROWFLG = .TRUE.
               INROW = -X + 0.01
               END IF
C                                       count rows actually read
            IF (IEL.EQ.1) IROW = IROW + 1
C                                       check differently on first pass
            IF ((JBEGIN.EQ.1) .AND. (IEL.EQ.1)) THEN
C                                       if row not expected
               IF (INROW.NE.IRNO+IDELTA) THEN
                  WRITE(MSGTXT,1910,ERR=990) INROW, IRNO+IDELTA
                  CALL MSGWRT (8)
C                                       keep track of shift because of
C                                       missing rows
                  IDELTA = INROW-IRNO
                  END IF
               IF (IROW.LE.MAXIND) ROWORD(IROW) = INROW
            ELSE
C                                       if row not beyound array limit
               IF (IROW.LT.MAXIND) THEN
C                                       is row as expected?
                  IF (ROWORD(IROW).NE.INROW) THEN
                     WRITE(MSGTXT,1911,ERR=990) INROW, ROWORD(IROW)
                     CALL MSGWRT (8)
                     GO TO 970
                     END IF
                  END IF
C                                       end if first pass
               END IF
C                                       if row should be ignored
            IF ((INROW.LT.BCOUNT) .OR. (INROW.GT.ECOUNT)) GO TO 60
C                                       determine output row
            IF (IEL.EQ.1) OUTROW = OUTROW + 1
C                                       read column values
            DO 50 J = JBEGIN,JEND
               STRING = '...'
               X = -1.
C                                       Get variable type
               JTYPE = MOD (DATP(J,2), 10)
C                                       See which have data, get
C                                       dimension
               JDIMEN = DATP(J,2)/10
C                                       for arrays with 0 elements or
C                                       arrays with < IEL elements
               IF ((JDIMEN.LT.IEL) .OR. ((IEL .GT. 1) .AND.
     *            ((JTYPE.EQ.CHARTY).OR.(JTYPE.EQ.BITTYP))))  THEN
C                                       read past double quotes
                  CALL GETSTR (LINE, NACROS, NACROS, NPNT, STRING,
     *               NCHAR)
               ELSE
C                                       if bit or char type
                  IF ((JTYPE.EQ.CHARTY).OR.(JTYPE.EQ.BITTYP)) THEN
C                                       get string
                     CALL GETSTR (LINE, NACROS, NACROS, NPNT,
     *                     STRING, NCHAR)
C                                       string length OK?
                     IF (NCHAR.LT.1.OR.JDIMEN.LT.1) THEN
                        WRITE(MSGTXT,1920,ERR=990) NCHAR, JDIMEN
                        CALL MSGWRT (8)
                        END IF
C                                       limit process to chars found
                     NCHAR = MAX(1,MIN(JDIMEN,NCHAR))
                     IF (JTYPE.EQ.CHARTY) THEN
                        CALL CHR2H (NCHAR, STRING, 1, RESULT)
C                                       else bit type
                     ELSE
                        READ(STRING,1230,ERR=960) (BITS(K),K=1,NCHAR)
C                                       # words with NBITWD bits
                        N = ((NCHAR - 1) / NBITWD) + 1
                        DO 40 I = 1,N
                           M = NBITWD
C                                       last word has only a few bits
                           IF (I.EQ.N) M = NCHAR - ((N-1)*NBITWD)
C                                       if fewer bits than fill a word
                           M = MIN (M, NCHAR)
C                                       pack word from array of BITS
                           CALL ZPTBIT(M, RESULT(I),
     *                        BITS(((I-1)*NBITWD)+1))
 40                        CONTINUE
C                                       end if char or bit
                        END IF
                  ELSE
C                                       if Multidimensional element
                     IF (IEL.GT.1) THEN
C                                       get earlier members of array
                        CALL GETCOL (OUTROW, J, DATP, LROW, BUFFER,
     *                     RTYPE, RESULT, ROWB, IERR)
                        IF (IERR.NE.0) THEN
                           WRITE (MSGTXT,1900) IERR, 'IEL=1 GETCOL'
                           CALL MSGWRT (8)
                           GO TO 960
                           END IF
                        END IF
C                                       if a logical
                     IF (JTYPE.EQ.5) THEN
                        CALL GETLG (LINE, NACROS, NPNT, ILOGIC)
                        RESLO(IEL) = ILOGIC.EQ.1
C                                       else get a number
                     ELSE
                        CALL GETNUM (LINE, NACROS, NPNT, X)
                        IF ((X.EQ.DBLANK) .AND. (ERRNUM.NE.0)) GO TO 975
                        IF (JTYPE.EQ.1) THEN
                           RES8(IEL) = X
                        ELSE IF (JTYPE.EQ.2) THEN
                           RES4(IEL) = FBLANK
                           IF (X.NE.DBLANK) RES4(IEL) = X
                        ELSE IF (JTYPE.EQ.4) THEN
                           IF (X.EQ.DBLANK) GO TO 975
                           RESLI(IEL) = X + 0.01
                        ELSE IF (JTYPE.EQ.6) THEN
                           IF (X.EQ.DBLANK) GO TO 975
                           RESULT(IEL) = X + 0.01
                           END IF
                        END IF
                     END IF
C                                       store data.
                  CALL PUTCOL (OUTROW, J, DATP, LROW, BUFFER, RESULT,
     *               ROWB, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1900) IERR, 'PUTCOL'
                     CALL MSGWRT (8)
                     GO TO 960
                     END IF
C                                       flag row at last use
                  IF ((ROWFLG) .AND. (JEND.EQ.NCOL) .AND. (J.EQ.JEND)
     *               .AND. (IEL.EQ.MAXDIM)) THEN
                     CALL TABIO ('FLAG', 0, OUTROW, ROWB, BUFFER, IERR)
                     IF (IERR.GT.0) THEN
                        WRITE(MSGTXT,1930,ERR=990) IERR, IROW
                        CALL MSGWRT (8)
                        GO TO 960
                        END IF
                     IERR = 0
                     LROW = 0
                     END IF
C                                       if double quote expected
                  END IF
C                                       end for all columns loop
  50           CONTINUE
C                                       for all elements in a dimension
  60        CONTINUE
C                                       for all rows loop
  70     CONTINUE
      GO TO 999
C                                       Table Write Error
 960  IERR = 3
      WRITE(MSGTXT, 1962,ERR=990) DATP(J,1), DATP(J,2)
      CALL MSGWRT (8)
      GO TO 999
C                                       Text Read Error
 970  IERR = 4
      WRITE(MSGTXT, 1970,ERR=990) LINE(1:40)
      CALL MSGWRT (8)
      GO TO 999
C                                       Parsing error
 975  MSGTXT = 'GETPAS: ERROR PARSING NUMBERS'
 980  CALL MSGWRT (8)
C                                       Jump at Write Error
 990  IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETPAS: Reading Columns ',I5,' to ',I5)
 1230 FORMAT (64I1)
 1900 FORMAT ('ERROR',I5,' ON ',A)
 1901 FORMAT ('ARRAY ERROR: ROW ',I7,'(',I4,'), INCOMPLETE ')
 1902 FORMAT ('EXTRA ***BEGIN*PASS*** AT ',I5,'(',I4,')')
 1910 FORMAT ('ROW Order Warning: FOUND ',I7,', EXPECTING ',I7)
 1911 FORMAT ('ROW ORDER ERROR  : FOUND ',I7,', EXPECTING ',I7)
 1920 FORMAT ('GETPAS: CHAR READ ERROR, NCHAR=',I5,', DIMESION=',I5)
 1930 FORMAT ('GETPAS: ERROR= ',I5,' FLAGING TABLE ENTRY ',I5)
 1962 FORMAT ('GETPAS: DATP(J,1)= ',I7,', DATP(J,2)= ',I7)
 1970 FORMAT ('LINE(1:40) =''',A40,'''')
      END
      SUBROUTINE EXRDHD (NREC, NACROS, NOPASS, IERR)
C-----------------------------------------------------------------------
C   EXRDHD reads header information for a specified AIPS table
C   from an external text file.
C   Inputs from common:
C     BUFFER(*)  I    Table I/O buffer (table opened and closed
C                     outside of this routine).
C     TYPE       I    Table type (2 char)
C     VER        I    Table version number
C     IBCOL      I    First col no. + (no. pass - 1) * 1000
C   Output:
C     IERR       I    Error number: 0 => none, else count of errors #
C-----------------------------------------------------------------------
      INTEGER   NREC, NACROS, NOPASS, IERR
C                                       Define array sizes of Keywords
      INTEGER   MAXKEY, MAXSYM, MAXFOR
      PARAMETER (MAXKEY=100, MAXSYM=9, MAXFOR=7)
C
      CHARACTER CARD*80, CARDS(60)*80
      INTEGER   HLUN, TABTYP, KEYTYP(MAXKEY), NUMKEY, KEYV(2*MAXKEY),
     *   KEYLOC(MAXKEY), I, PASKEY, IKEY, FDIMEN, SRTORD, HBUFF(256),
     *   TABLUN, VTYPE, HERR
      CHARACTER KEYWRD(MAXKEY)*8, KEYCHR(MAXKEY)*8
      DOUBLE PRECISION KEYVAL(MAXKEY)
      LOGICAL   EOF, T, ENDKEY
      INCLUDE 'TBIN.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      DATA HLUN, TABLUN / 27, 29/
      DATA T/.TRUE./, NUMKEY/0/, TABTYP/1/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Set standard table defaults
      CALL SETDEF
C                                       Get First 3 cards
      DO 30 I = 1,3
         CALL RDCARD (CARD, EOF, IERR)
         IF ((IERR.NE.0) .OR. (EOF)) GO TO 900
         CARDS(I) = CARD
 30      CONTINUE
C                                       Read required table parms
      CALL TABREQ (CARDS, EXTTYP, BITPIX, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get cards describing AXIES
      DO 50 I = 1,NAXIS+2
         CALL RDCARD (CARD, EOF, IERR)
         IF (IERR.NE.0.OR.EOF) GO TO 900
         CARDS(I) = CARD
 50      CONTINUE
C                                       decode cards
      CALL TABAXI (CARDS, NAXIS, NAXISI, PCOUNT, GCOUNT, IERR)
      IF (IERR.NE.0) GO TO 999
      NACROS = NAXISI(1)
      NREC = NAXISI(2)
C                                       Open history
      CALL HIINIT (3)
      CALL HIOPEN (HLUN, ODISK, CNO, HBUFF, HERR)
      IF (HERR.EQ.5) THEN
         MSGTXT = 'WARNING: CREATING NEW HI FILE'
         CALL MSGWRT (6)
         CALL HICREA (HLUN, ODISK, CNO, CATBLK, HBUFF, HERR)
         END IF
C                                       Assume 3d table unless simple
C                                       TABLE
      IF (EXTTYP(1:5).EQ.'TABLE') TABTYP = 0
C                                       Read other standard keys
      DO 80 I = 1, 100000
         CALL RDCARD (CARD, EOF, IERR)
         IF (IERR.NE.0.OR.EOF) GO TO 900
         CALL TABHDK (CARD, HLUN, HBUFF, MAXKEY, NUMKEY, KEYWRD, KEYVAL,
     *      KEYCHR, KEYTYP, ENDKEY, IERR)
         IF (ENDKEY) GO TO 90
         IF (IERR.NE.0) GO TO 900
 80      CONTINUE
 90      CONTINUE
C                                       put the name of the output UV
C                                       file to the HI table
      IF (HERR.EQ.0) CALL HENCOO (TSKNAM, NAME, CLASS, OSEQ, ODISK,
     *   HLUN, HBUFF, HERR)
C                                       Get number of cols from common
      NCOL = ITNCOL
      OEXT = ITYPE
C                                       Tell user table and type
      WRITE (MSGTXT,1005,ERR=990) OEXT
      CALL MSGWRT (5)
C                                       Normal table files. Special
C                                       processing if its an AIPS style
C                                       table.
       SRTORD = 0
       IF (ITYPE.NE.'UK') CALL ATCONV (NUMKEY, KEYWRD, KEYVAL, KEYTYP,
     *    KEYCHR, SRTORD)
C                                       Create and initialize the table
C                                       header with data in common.
      OVERS = 0
      CALL MAKTAB (SRTORD, ODISK, CNO, OVERS, CATBLK, TABLUN, DATP,
     *   BUFFER, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Look for certain symbols
      PASKEY = 0
      DO 100 IKEY = 1, NUMKEY
         IF (KEYWRD(IKEY).EQ.'NOPASS  ') THEN
            NOPASS = KEYVAL(IKEY) + 0.01
            PASKEY = IKEY
            END IF
 100     CONTINUE
C                                       Remove some keys
      IF (PASKEY.NE.0) THEN
         DO 200 IKEY = PASKEY, NUMKEY
            KEYWRD(IKEY) = KEYWRD(IKEY+1)
            KEYVAL(IKEY) = KEYVAL(IKEY+1)
            KEYCHR(IKEY) = KEYCHR(IKEY+1)
            KEYTYP(IKEY) = KEYTYP(IKEY+1)
 200        CONTINUE
         NUMKEY = NUMKEY - 1
         END IF
C                                       Write out keyword pairs.
      CALL TABPAK (NUMKEY, KEYWRD, KEYVAL, KEYCHR, KEYTYP, KEYV, KEYLOC,
     *   BUFFER, IERR)
C                                       Tell input parameters
      WRITE (MSGTXT,1010,ERR=990) NREC, NCOL
      CALL MSGWRT (5)
      WRITE (MSGTXT,1012,ERR=990) OEXT, OVERS, NREC
      CALL MSGWRT (5)
      WRITE (MSGTXT,1015,ERR=990) TSKNAM, OEXT, OVERS, NREC
      IF (HERR.EQ.0) CALL HIADD (HLUN, MSGTXT, HBUFF, HERR)
      WRITE (MSGTXT,1020,ERR=990) NOPASS, NACROS
      CALL MSGWRT (5)
C                                       Close history
      IF (HERR.EQ.0) CALL HICLOS (HLUN, T, HBUFF, HERR)
      FDIMEN = 1
C                                       Transfer begin columns
      DO 300 I = 1, NCOL
         IBCOL(I) = TBCOL(I)
         VTYPE = MOD (TFCODE(I), 10)
         IF (VTYPE.NE.3.AND.VTYPE.NE.7)
     *      FDIMEN = MAX(FDIMEN, (TFCODE(I)/10))
 300     CONTINUE
      WRITE(MSGTXT,1030,ERR=990) FDIMEN, EXTTYP
      CALL MSGWRT (5)
      GO TO 999
C                                       Error on I/O
 900  WRITE (MSGTXT,1900,ERR=990) IERR
      CALL MSGWRT (8)
      GO TO 999
 990  IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('Reading ASCII table of type ',A)
 1010 FORMAT ('Table Contains ',I7,' Records, with  ',I5,' Columns ')
 1012 FORMAT ('Created ',A2,' table version',I4,' with ',I7,' records')
 1015 FORMAT (A,'/ Created ',A2,' table version',I4,' with ',I7,
     *   ' records')
 1020 FORMAT ('Table Read in  ',I7,' Passes, Text is',I5,' chars wide')
 1030 FORMAT ('Table max dimension is ',I5,' for type ',A)
 1900 FORMAT ('EXWTHD: WRITE ERROR',I7)
      END
      SUBROUTINE RDCARD (LINE, EOF, IERR)
C-----------------------------------------------------------------------
C RDCARD ReaDs a fits CARD.  This routine must be supplied inorder
C to use the standard IDWCRD routine to read in standard table parms
C Output: LINE C*(*) Output fits card
C         EOF  L     T when end of file
C         IERR I     Error code, 0 => ok
C-----------------------------------------------------------------------
      CHARACTER LINE*(*)
      LOGICAL   EOF
      INTEGER   IERR
C
      INTEGER   JT, JTRIM
      INCLUDE 'CARDIO.INC'
C-----------------------------------------------------------------------
      EOF = .FALSE.
      CALL ZTXIO ('READ', EXTLUN, EXTIND, LINE, IERR)
      JT = JTRIM (LINE)
C
 999  RETURN
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
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
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
C
 999  RETURN
      END




