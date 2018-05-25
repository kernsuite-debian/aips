      SUBROUTINE TBDATA (OPER, IVOL, ISLOT, IERR)
C-----------------------------------------------------------------------
C! verbs to put/get table values
C# POPS-appl Ext-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2006-2007
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
C   This routine reads and writes values from/to an AIPS table.
C   Inputs:
C      OPER    C*4  'READ' or 'WRIT' operation desired.
C      IVOL    I    Disk number
C      ISLOT   I    Catalog slot number
C   Common: (though adverbs in common)
C      INEXT     H*2  Table type (e.g. AN, SU)
C      INVER     R    the version number of the table.
C      KEYWORD  H*8   FITS keyword corresponding to a header value
C      KEYVALUE R(2)  Numeric value out/in
C      KEYSTRNG H*16  String value out/in
C      PIXXY    R(7)  Location in table:
C                     (1) = Row number
C                     (2) = Column number
C                     (3) = subscript or start character
C   Output:
C      IERR     I     Error code, 0=> OK else failed
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   IVOL, ISLOT, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER PRGNAM*8, STVALU*16, TABTYP*2, NEWSTR*16, OLDSTR*16,
     *   HILINE*72, HILIN2*72, CDUM*1
      INTEGER   POTERR, IROUND, VER, IBUFF(512), TLUN, IKEY, NREC, NCOL,
     *   DATP(128,2), DATVI(XBPRSZ), ITYPE, NROW, ILEN, I, JERR, IROW,
     *   ICOL, ISUB, NCOPY, I1, I2, BITS(64), IHLUN, IERH, OLDVI, ITEMP,
     *   IDUM, LROW
      HOLLERITH DATVH(XBPRSZ), OLDVH(4)
      REAL      DATVR(XBPRSZ), OLDVR, SCRTCH(XBPRSZ), RDUM(2), DOHI
      DOUBLE PRECISION D, TEMPD, DATVD(XBPRSZ/2), OLDVD
      LOGICAL   DATVL(XBPRSZ), OLDVL, ISBLNK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (DATVI, DATVR, DATVH, DATVD, DATVL)
      EQUIVALENCE (OLDVI, OLDVR, OLDVH, OLDVD, OLDVL)
      COMMON /AIPSCR/ DATVR, SCRTCH
      DATA TLUN, IHLUN /28,27/
      DATA PRGNAM /'TBDATA'/
C-----------------------------------------------------------------------
      POTERR = 0
C                                       Read CATBLK
      CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'REST', IBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         IF (IERR.EQ.5) MSGTXT = 'FILE IS BUSY'
         POTERR = 33
         GO TO 970
         END IF
C                                       Open table
      IKEY = 1
      NREC = 10
      NCOL = 0
      CALL ADVERB ('INEXT', 'C', 1, 2, IDUM, RDUM, TABTYP)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INVERS', 'I', 1, 0, VER, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       Find highest if no version given
      IF (VER.LE.0) CALL FNDEXT (TABTYP, CATBLK, VER)
C                                       Open table
      CALL TABINI (OPER, TABTYP, IVOL, ISLOT, VER, CATBLK, TLUN,
     *   IKEY, NREC, NCOL, DATP, IBUFF, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1100) IERR, OPER, TABTYP, VER
         POTERR = 61
         GO TO 970
         END IF
      NROW = IBUFF(5)
C                                       Set row, col, etc.
      CALL ADVERB ('PIXXY', 'I', 7, 0, BITS, RDUM, CDUM)
      IROW = MAX (BITS(1), 1)
      ICOL = MAX (BITS(2), 1)
      ISUB = MAX (BITS(3), 1)
C                                       Check values: row number
      IF (IROW.GT.NROW) THEN
         WRITE (MSGTXT,1120) IROW, NROW
         CALL MSGWRT (7)
         IERR = 2
         POTERR = 32
         GO TO 800
         END IF
C                                       Column number
      IF (ICOL.GT.NCOL) THEN
         WRITE (MSGTXT,1130) ICOL, NCOL
         CALL MSGWRT (7)
         IERR = 2
         POTERR = 32
         GO TO 800
         END IF
C                                       Subscript
      ILEN = DATP(ICOL,2) / 10
      IF (ISUB.GT.ILEN) THEN
         WRITE (MSGTXT,1140) ISUB, ILEN
         CALL MSGWRT (7)
         IERR = 2
         POTERR = 32
         GO TO 800
         END IF
C                                       Check array size
      IF (ILEN.GT.MAXCIF) THEN
         WRITE (MSGTXT,1150) ILEN, MAXCIF
         CALL MSGWRT (7)
         IERR = 2
         POTERR = 32
         GO TO 800
         END IF
      ITYPE = MOD (DATP(ICOL,2), 10)
      LROW = 0
C                                       TABPUT
      IF (OPER.EQ.'WRIT') THEN
C                                       Get current entry
         CALL GETCOL (IROW, ICOL, DATP, LROW, IBUFF, ITYPE, DATVI,
     *      SCRTCH, IERR)
         ITYPE = MOD (ITYPE, 10)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1500) IERR, OPER
            CALL MSGWRT (7)
            POTERR = 61
            GO TO 800
            END IF
C                                       Blank?
         CALL ADVERB ('KEYSTRNG', 'C', 1, 16, IDUM, RDUM, STVALU)
         IF (ERRNUM.NE.0) GO TO 800
         ISBLNK = STVALU(:7) .EQ. 'BLANKED'
C                                       Enter data in VALUE via
C                                       EQUIVALENCE
         CALL ADVERB ('KEYVALUE', 'R', 2, 0, IDUM, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 800
         CALL POPSRD ('R2D', RDUM, TEMPD)
C                                       Double precision
         IF (ITYPE.EQ.1) THEN
            OLDVD = DATVD(ISUB)
            DATVD(ISUB) = TEMPD
            IF (ISBLNK) DATVD(ISUB) = DBLANK
C                                       Single precision
         ELSE IF (ITYPE.EQ.2) THEN
            OLDVR = DATVR(ISUB)
            DATVR(ISUB) = RDUM(1)
            IF (ISBLNK) DATVR(ISUB) = FBLANK
C                                       Character strings
         ELSE IF (ITYPE.EQ.3) THEN
            NCOPY = ILEN - ISUB + 1
            NCOPY = MIN (NCOPY, 16)
            CALL CHCOPY (NCOPY, ISUB, DATVH, 1, OLDVH)
            CALL CHR2H (NCOPY, STVALU, ISUB, DATVH)
C                                       Integers
         ELSE IF (ITYPE.EQ.4) THEN
            OLDVI = DATVI(ISUB)
            DATVI(ISUB) = IROUND (RDUM(1))
C                                       Logicals
         ELSE IF (ITYPE.EQ.5) THEN
            OLDVL = DATVL(ISUB)
            DATVL(ISUB) = STVALU(:1).EQ.'T'
C                                       Bit arrays
         ELSE IF (ITYPE.EQ.7) THEN
            I1 = ((ISUB-1) / NBITWD) + 1
            I2 = (ISUB-1)
            I2 = MOD (I2, NBITWD) + 1
            CALL ZGTBIT (NBITWD, DATVI(I1), BITS)
            OLDVL = BITS(I2).EQ.1
            BITS(I2) = 0
            IF (STVALU(:1).EQ.'T') BITS(I2) = 1
            CALL ZPTBIT (NBITWD, DATVI(I1), BITS)
            END IF
C                                       Write value
         CALL PUTCOL (IROW, ICOL, DATP, LROW, IBUFF, DATVI, SCRTCH,
     *      IERR)
C                                       Force it to disk
         IF (IERR.EQ.0) CALL PUTCOL (0, ICOL, DATP, LROW, IBUFF, DATVI,
     *      SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1500) IERR, OPER
            CALL MSGWRT (7)
            POTERR = 61
            GO TO 800
            END IF
C                                       TABGET
      ELSE IF (OPER.EQ.'READ') THEN
C                                       Read value
         CALL GETCOL (IROW, ICOL, DATP, LROW, IBUFF, ITYPE, DATVI,
     *      SCRTCH, IERR)
         ITYPE = MOD (ITYPE, 10)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1500) IERR, OPER
            CALL MSGWRT (7)
            POTERR = 61
            GO TO 800
            END IF
C                                       Initialize results
         RDUM(1) = 0.0
         RDUM(2) = 0.0
         STVALU = ' '
C                                       Put value back into adverbs
C                                       Double precision
         IF (ITYPE.EQ.1) THEN
            CALL RCOPY (NWDPDP, DATVD(ISUB), D)
            ISBLNK = D.EQ.DBLANK
            IF (ISBLNK) THEN
               STVALU = 'BLANKED'
            ELSE
               CALL POPSRD ('D2R', RDUM, D)
               END IF
C                                       Real
         ELSE IF (ITYPE.EQ.2) THEN
            ISBLNK = DATVR(ISUB).EQ.FBLANK
            IF (ISBLNK) THEN
               STVALU = 'BLANKED'
            ELSE
               RDUM(1) = DATVR(ISUB)
               END IF
C                                       Character
         ELSE IF (ITYPE.EQ.3) THEN
            NCOPY = ILEN - ISUB + 1
            NCOPY = MIN (NCOPY, 16)
            CALL H2CHR (NCOPY, ISUB, DATVH, STVALU)
C                                       Integer
         ELSE IF (ITYPE.EQ.4) THEN
            RDUM(1) = DATVI(ISUB)
C                                       Logical
         ELSE IF (ITYPE.EQ.5) THEN
            IF (DATVL(ISUB)) THEN
               STVALU = 'T'
            ELSE
               STVALU = 'F'
               END IF
C                                       Bit arrays
         ELSE IF (ITYPE.EQ.7) THEN
            I1 = ((ISUB-1) / NBITWD) + 1
            I2 = (ISUB-1)
            I2 = MOD (I2, NBITWD) + 1
            CALL ZGTBIT (NBITWD, DATVI(I1), BITS)
            IF (BITS(I2).EQ.1) THEN
               STVALU = 'T'
            ELSE
               STVALU = 'F'
               END IF
            END IF
         CALL ADVRBS ('KEYVALUE', 'R', 2, 0, IDUM, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 800
         CALL ADVRBS ('KEYSTRNG', 'C', 1, 16, IDUM, RDUM, STVALU)
         IF (ERRNUM.NE.0) GO TO 800
         END IF
C                                       Close table
 800  CALL TABIO ('CLOS', 1, I, SCRTCH, IBUFF, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1800) JERR, TABTYP, VER
         IF (POTERR.EQ.0) POTERR = 61
         IF (IERR.EQ.0) IERR = JERR
         GO TO 970
         END IF
C                                       Any history
      CALL ADVERB ('DOHIST', 'R', 1, 0, IDUM, DOHI, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF ((OPER.EQ.'WRIT') .AND. (DOHI.GT.-1.5)) THEN
C                                       Open history file
         CALL HIOPEN (IHLUN, IVOL, ISLOT, IBUFF, IERH)
         IF (IERH.NE.0) GO TO 980
C                                       Which table?
         WRITE (HILINE,2800) TSKNAM, 'TABPUT', TABTYP, VER
         CALL HIADD (IHLUN, HILINE, IBUFF, IERH)
C                                       Add entries by type
C                                       Double precision
         IF (ITYPE.EQ.1) THEN
            WRITE (HILINE,2801) TSKNAM, 'TABPUT',IROW, ICOL, ISUB, TEMPD
            WRITE (HILIN2,3801) TSKNAM, 'TABPUT', OLDVD
C                                       Either blanked?
            IF (ISBLNK) WRITE (HILINE,4801) TSKNAM, 'TABPUT', IROW,
     *         ICOL, ISUB
            IF (OLDVD.EQ.DBLANK) WRITE (HILIN2,5801) TSKNAM, 'TABPUT'
C                                       Real
         ELSE IF (ITYPE.EQ.2) THEN
            WRITE (HILINE,2802) TSKNAM, 'TABPUT',IROW, ICOL, ISUB,
     *         RDUM(1)
            WRITE (HILIN2,3802) TSKNAM, 'TABPUT', OLDVR
C                                       Either blanked?
            IF (ISBLNK) WRITE (HILINE,4802) TSKNAM, 'TABPUT', IROW,
     *         ICOL, ISUB
            IF (OLDVR.EQ.FBLANK) WRITE (HILIN2,5802) TSKNAM, 'TABPUT'
C                                       Character
         ELSE IF (ITYPE.EQ.3) THEN
            NEWSTR = STVALU
            CALL H2CHR (16, 1, OLDVH, OLDSTR)
            WRITE (HILINE,2803) TSKNAM, 'TABPUT',IROW, ICOL, ISUB,
     *         NEWSTR
            WRITE (HILIN2,3803) TSKNAM, 'TABPUT', OLDSTR
C                                       Integer
         ELSE IF (ITYPE.EQ.4) THEN
            ITEMP = IROUND(RDUM(1))
            WRITE (HILINE,2804) TSKNAM, 'TABPUT',IROW, ICOL, ISUB, ITEMP
            WRITE (HILIN2,3804) TSKNAM, 'TABPUT', OLDVI
C                                       Logical
         ELSE IF (ITYPE.EQ.5) THEN
            NEWSTR = STVALU
            CALL H2CHR (1, 1, OLDVH, OLDSTR)
            WRITE (HILINE,2805) TSKNAM, 'TABPUT',IROW, ICOL, ISUB,
     *         NEWSTR(1:1)
            WRITE (HILIN2,3805) TSKNAM, 'TABPUT', OLDSTR(1:1)
C                                       Logical
         ELSE IF (ITYPE.EQ.7) THEN
            NEWSTR = STVALU
            IF (OLDVL) THEN
               OLDSTR = 'T'
            ELSE
               OLDSTR = 'F'
               END IF
            WRITE (HILINE,2807) TSKNAM, 'TABPUT',IROW, ICOL, ISUB,
     *         NEWSTR(1:1)
            WRITE (HILIN2,3807) TSKNAM, 'TABPUT', OLDSTR(1:1)
            END IF
C                                       Add history entries
         IF (IERH.EQ.0) CALL HIADD (IHLUN, HILINE, IBUFF, IERH)
         IF (IERH.EQ.0) CALL HIADD (IHLUN, HILIN2, IBUFF, IERH)
         CALL HICLOS (IHLUN, .TRUE., IBUFF, IERH)
         END IF
      GO TO 980
C-----------------------------------------------------------------------
C                                       Print error.
 970  CALL MSGWRT (8)
C                                       AIPS error management.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.EQ.0) GO TO 999
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CATALOG READ ERROR',I5)
 1100 FORMAT ('TABINI ERROR ',I3,' OPEN FOR ',A4,1X,A2,', TABLE ',
     *   ' VER=',I3)
 1120 FORMAT ('ROW ',I6,' OUT OF RANGE (1:',I6,')')
 1130 FORMAT ('COLUMN ',I6,' OUT OF RANGE (1:',I6,')')
 1140 FORMAT ('SUBSCRIPT ',I6,' OUT OF RANGE (1:',I6,')')
 1150 FORMAT ('TABLE ENTRY SIZE ',I5,' TOO BIG FOR ARRAY ',I5)
 1500 FORMAT ('TABKEY ERROR',I3,1X,A,'ING TABLE VALUE')
 1800 FORMAT ('TABIO ERROR ',I3,' CLOSING ',A2,' TABLE VER=',I3)
 2800 FORMAT (A,':',A,': INEXT=''',A,''', INVERS=',I3,' /Table edited')
 2801 FORMAT (A,':',A,': PIXXY=',I8,2I5,',KEYVALUE=', 1PD20.12)
 3801 FORMAT (A,':',A,': / Old value =',1PD20.12)
 4801 FORMAT (A,':',A,': PIXXY=',I8,2I5,', KEYSTRNG=''BLANKED''')
 5801 FORMAT (A,':',A,': / Old value =''INDE''')
 2802 FORMAT (A,':',A,': PIXXY=',I8,2I5,',KEYVALUE(1)=',1PE15.5)
 3802 FORMAT (A,':',A,': / Old value =',1PE12.5)
 4802 FORMAT (A,':',A,': PIXXY=',I8,2I5,', KEYSTRNG=''BLANKED''')
 5802 FORMAT (A,':',A,': / Old value =''INDE''')
 2803 FORMAT (A,':',A,': PIXXY=',I8,2I5,',KEYSTRNG=''',A,'''')
 3803 FORMAT (A,':',A,': / Old value =''',A,'''')
 2804 FORMAT (A,':',A,': PIXXY=',I8,2I5,', KEYVALUE(1)=',I10)
 3804 FORMAT (A,':',A,': / Old value = ',I10)
 2805 FORMAT (A,':',A,': PIXXY=',I8,2I5,', KEYSTRNG=''',A,'''')
 3805 FORMAT (A,':',A,': / Old value =''',A,'''')
 2807 FORMAT (A,':',A,': PIXXY=',I8,2I5,', KEYSTRNG=''',A,'''')
 3807 FORMAT (A,':',A,': / Old value =''',A,'''')
      END
