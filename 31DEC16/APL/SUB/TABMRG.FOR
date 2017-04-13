      SUBROUTINE TABMRG (DISK, CNO, TYPE, INVER, OUTVER, EKOL, SKOL,
     *   TKOL, ITABL, OTABL, CATBLK, OUTNUM, IERR)
C-----------------------------------------------------------------------
C! merges rows of an an input table file
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999
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
C   TABMRG merges an AIPS general table-format file.  It does not sort
C   the file - so sorts must be done first if merging is to make any
C   sense.  TABMRG compares row N with row N+1 and merges the two,
C   summing columns SKOL and using row N for the others, if each of
C   columns EKOL are within TKOL of each other.
C   Inputs:
C      DISK    I         Disk number to use
C      CNO     I         Primary file catalog number
C      TYPE    C*2       Extension file type (e.g. CC)
C      INVER   I         Input extension file number (0 -> high)
C      EKOL    I(*)      Columns which must be "equal" by logical
C                        column number.  A zero MUST terminate the
C                        list unless it is 25 long.  Must have >0.
C      SKOL    I(*)      Columns which will be summed on a merge by
C                        column number.  A zero MUST terminate the
C                        list unless it is 25 long.  May be 0.
C      TKOL    R(*)      Tolerance for equality by logical column
C                        number (parallel to EKOL list)
C   In/out:
C      OUTVER  I         Output extension file number (0 -> high+1)
C      CATBLK  I(256)    Primary file header
C   Output:
C      ITABL   I(512)    Scratch for input table IO
C      OTABL   I(512)    Scratch for output table IO
C      OUTNUM  I         Number rows in output file.
C      IERR    I         Error code: 0 -> ok
C                           1 -> input file open error
C                           2 -> input parameter error
C                           3 -> output file create/open error
C                           4 -> ZFIO IO error
C                           5 -> TABIO IO error
C-----------------------------------------------------------------------
      CHARACTER TYPE*2
      INTEGER   DISK, CNO, INVER, OUTVER, EKOL(*), SKOL(*), ITABL(*),
     *   OTABL(*), CATBLK(256), OUTNUM, IERR
      REAL      TKOL(*)
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER PHNAME*48, CHTEST*2
      INTEGER   NREC, NCOL, NKEY, ILUN, OLUN, IFIND, OFIND, NE, NS, I,
     *   IDATP(128,2), ODATP(128,2), J, EPCOL(25), ESLEN(25), ETYPE(25),
     *   SPCOL(25), STYPE(25), IP, JP, LIM, NFIRST, NWORD, KKEY, ICOL,
     *   IREC(XBPRSZ), OREC(XBPRSZ), NCH, IER, SPLEN(25), NCOPY, ICOUNT,
     *   IRNO, OCOUNT, CHKTYP(128), CHKCOL(128), CHKLEN(128), JJP
      REAL      IREC4(XBPRSZ), OREC4(XBPRSZ)
      LOGICAL   SAME, FIRST, IRECL(XBPRSZ), ORECL(XBPRSZ), CHKIT(128)
      DOUBLE PRECISION    IREC8(XBPRSZ/2), OREC8(XBPRSZ/2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (IREC, IRECL, IREC4, IREC8)
      EQUIVALENCE (OREC, ORECL, OREC4, OREC8)
C-----------------------------------------------------------------------
C                                       Open input file
      NREC = 20
      NCOL = 0
      NKEY = 0
      ILUN = 29
      IFIND = 0
      OFIND = 0
      CALL TABINI ('READ', TYPE, DISK, CNO, INVER, CATBLK, ILUN, NKEY,
     *   NREC, NCOL, IDATP, ITABL, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1000) IER
         IERR = 1
         GO TO 990
         END IF
C                                       Init. columns to check for
C                                       blanking
      DO 10 ICOL = 1,NCOL
         CHKCOL(ICOL) = IDATP(ICOL,1)
         CHKLEN(ICOL) = IDATP(ICOL,2) / 10
         CHKTYP(ICOL) = MOD (IDATP(ICOL,2), 10)
C                                       Only check double and real
         CHKIT(ICOL) = (CHKTYP(ICOL).EQ.1) .OR. (CHKTYP(ICOL).EQ.2)
 10      CONTINUE
C                                       Check inputs
      NE = 0
      IFIND = ITABL(82)
      DO 20 I = 1,25
         IF ((EKOL(I).LE.0) .OR. (EKOL(I).GT.NCOL)) GO TO 25
            NE = I
            IP = EKOL(I)
            EPCOL(I) = IDATP(IP,1)
            ESLEN(I) = IDATP(IP,2) / 10
            ETYPE(I) = IDATP(IP,2) - 10 * ESLEN(I)
            IF (TKOL(I).LT.0.0) TKOL(I) = 0.0
            CHKIT(IP) = .FALSE.
 20      CONTINUE
 25   NS = 0
      IERR = 2
      DO 30 I = 1,25
         IP = SKOL(I)
         IF ((IP.LE.0) .OR. (IP.GT.NCOL)) GO TO 35
            NS = I
            SPCOL(I) = IDATP(IP,1)
            SPLEN(I) = IDATP(IP,2) / 10
            STYPE(I) = IDATP(IP,2) - 10 * SPLEN(I)
            CHKIT(IP) = .FALSE.
            IP = STYPE(I)
            IF ((IP.EQ.1) .OR. (IP.EQ.2) .OR. (IP.EQ.4)) GO TO 30
               WRITE (MSGTXT,1025)
               GO TO 990
 30      CONTINUE
 35   IF (NE.GT.0) GO TO 40
         WRITE (MSGTXT,1035)
         GO TO 990
C                                       Find in catalog header
C                                       TABINI called FXHDEX for us
 40   DO 45 I = 1,KIEXTN
         CALL H2CHR (2, 1, CATBLK(KHEXT+I-1), CHTEST)
         IF (TYPE.EQ.CHTEST) GO TO 50
 45      CONTINUE
      GO TO 999
C                                       check OUTVER
 50   JP = KIVER + I - 1
      LIM = CATBLK(JP)
      I = LIM + 1
      IF ((OUTVER.LE.0) .OR. (OUTVER.GT.I)) OUTVER = I
      SAME = OUTVER.EQ.INVER
C                                       Number input rows
      ICOUNT = ITABL(5)
C                                       Set up for create
      CALL COPY (256, ITABL, OTABL)
      CALL COPY (256, IDATP, ODATP)
      OLUN = ILUN + 1
      IF (.NOT.SAME) GO TO 55
         OLUN = ILUN
         CALL ZCLOSE (ILUN, IFIND, IER)
         IFIND = 0
C                                       Create
 55   CALL TABINI ('WRIT', TYPE, DISK, CNO, OUTVER, CATBLK, OLUN, NKEY,
     *   NREC, NCOL, ODATP, OTABL, IER)
      IF (IER.LE.0) GO TO 60
         WRITE (MSGTXT,1055) IER
         IERR = 3
         GO TO 990
C                                       Save info from input header
C                                       Creation date, time
 60   NCOPY = 6
      CALL COPY (NCOPY, ITABL(11), OTABL(11))
C                                       Creation task
      NCOPY = 3
      CALL COPY (NCOPY, ITABL(29), OTABL(29))
C                                       Sort info
      OTABL(43) = ITABL(43)
      OTABL(44) = ITABL(44)
C                                       Number of keywords
      OTABL(53) = ITABL(53)
C                                       Selection strings
      NCOPY = 10
      CALL COPY (NCOPY, ITABL(61), OTABL(61))
C                                       Title
      NCOPY = 28
      CALL COPY (NCOPY, ITABL(101), OTABL(101))
C                                       Copy Keywords et al.
      OFIND = OTABL(82)
      IF (SAME) IFIND = OFIND
      NFIRST = OTABL(50)
      IERR = 4
      IF (SAME) GO TO 75
         DO 70 I = 2,NFIRST
            IRNO = I
            CALL ZFIO ('READ', ILUN, IFIND, IRNO, ITABL(257), IER)
            IF (IER.EQ.0) GO TO 65
               WRITE (MSGTXT,1060) IER, 'READ', IRNO
               GO TO 900
 65         CALL ZFIO ('WRIT', OLUN, OFIND, IRNO, ITABL(257), IER)
            IF (IER.EQ.0) GO TO 70
               WRITE (MSGTXT,1060) IER, 'WRIT', IRNO
               GO TO 900
 70         CONTINUE
         ITABL(77) = 5
         ITABL(78) = NFIRST
C                                       more set ups
 75   NWORD = ITABL(72)
      FIRST = .TRUE.
      OCOUNT = 0
      IERR = 5
C                                      copy rows:
 100  DO 200 IRNO = 1,ICOUNT
         CALL TABIO ('READ', 0, IRNO, IREC, ITABL, IER)
         IF (IER.LT.0) GO TO 200
         IF (IER.EQ.0) GO TO 110
            WRITE (MSGTXT,1100) IER, 'READ', IRNO
            GO TO 900
 110     IF (FIRST) GO TO 195
C                                       is everything equal
         DO 150 ICOL = 1,NE
            IP = EPCOL(ICOL)
            KKEY = ETYPE(ICOL)
            NCH = ESLEN(ICOL)
            GO TO (115, 120, 125, 130, 135, 140, 145), KKEY
C                                       D
 115        CONTINUE
               DO 116 I = 1,NCH
                  IF (ABS(IREC8(IP+I-1)-OREC8(IP+I-1)).GT.TKOL(ICOL))
     *               GO TO 190
 116              CONTINUE
               GO TO 150
C                                       R
 120        CONTINUE
               DO 121 I = 1,NCH
                  IF (ABS(IREC4(IP+I-1)-OREC4(IP+I-1)).GT.TKOL(ICOL))
     *               GO TO 190
 121              CONTINUE
               GO TO 150
C                                       Hollerith: exact match
 125        CONTINUE
               DO 126 I = 1,NCH,4
                  J = (I-1) / 4 + IP
                  IF (IREC4(J).NE.OREC4(J)) GO TO 190
 126              CONTINUE
               GO TO 150
C                                       I
 130        CONTINUE
               DO 131 I = 1,NCH
                  IF (ABS(IREC(IP+I-1)-OREC(IP+I-1)).GT.TKOL(ICOL))
     *               GO TO 190
 131              CONTINUE
               GO TO 150
C                                       Logical
 135        CONTINUE
               DO 136 I = 1,NCH
                  IF ((IRECL(IP+I-1)) .AND. (ORECL(IP+I-1))) GO TO 190
                  IF ((.NOT.IRECL(IP+I-1)) .AND. (.NOT.ORECL(IP+I-1)))
     *               GO TO 190
 136              CONTINUE
               GO TO 150
C                                       I
 140        CONTINUE
               WRITE (MSGTXT,1140) ICOL
               IERR = 2
               GO TO 900
C                                       BIT: exact match
 145        CONTINUE
               NCH = (ESLEN(ICOL) - 1) / NBITWD + 1
               DO 146 I = 1,NCH
                  J = IP + I - 1
                  IF (IREC(J).NE.OREC(J)) GO TO 190
 146              CONTINUE
 150         CONTINUE
C                                       All equal: do summing taking
C                                       care of blanked values
         IF (NS.GT.0) THEN
            DO 160 ICOL = 1,NS
               IP = SPCOL(ICOL)
               KKEY = STYPE(ICOL)
               NCH = SPLEN(ICOL)
 155           IF (NCH.LE.0) GO TO 160
               IF (KKEY.EQ.1) THEN
                  IF (OREC8(IP).EQ.DBLANK) THEN
                     OREC8(IP) = IREC8(IP)
                  ELSE IF (IREC8(IP).NE.DBLANK) THEN
                     OREC8(IP) = OREC8(IP) + IREC8(IP)
                     END IF
               ELSE IF (KKEY.EQ.2) THEN
                  IF (OREC4(IP).EQ.FBLANK) THEN
                     OREC4(IP) = IREC4(IP)
                  ELSE IF (IREC4(IP).NE.FBLANK) THEN
                     OREC4(IP) = OREC4(IP) + IREC4(IP)
                     END IF
               ELSE IF (KKEY.EQ.4) THEN
                  OREC(IP) = OREC(IP) + IREC(IP)
                  END IF
               IP = IP + 1
               NCH = NCH - 1
               GO TO 155
 160           CONTINUE
            END IF
C                                       Take unblanked values
         DO 180 ICOL = 1,NCOL
            IP = CHKCOL(ICOL)
            KKEY = CHKTYP(ICOL)
            NCH = CHKLEN(ICOL)
            IF (.NOT.CHKIT(ICOL)) GO TO 180
C                                       Loop over elements
 175           IF (NCH.LE.0) GO TO 180
               IF (KKEY.EQ.1) THEN
                  IF (OREC8(IP).EQ.DBLANK) OREC8(IP) = IREC8(IP)
               ELSE IF (KKEY.EQ.2) THEN
                  IF (OREC4(IP).EQ.FBLANK) OREC4(IP) = IREC4(IP)
                  END IF
               IP = IP + 1
               NCH = NCH - 1
               GO TO 175
 180        CONTINUE
         GO TO 200
C                                       Not equal: write old
 190     OCOUNT = OCOUNT + 1
         CALL TABIO ('WRIT', 0, OCOUNT, OREC, OTABL, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1100) IER, 'WRIT', OCOUNT
            GO TO 900
            END IF
C                                       move new row to out buffer
 195     CALL COPY (NWORD, IREC, OREC)
         FIRST = .FALSE.
 200     CONTINUE
C                                       write last record
      IF (.NOT.FIRST) THEN
         OCOUNT = OCOUNT + 1
         CALL TABIO ('WRIT', 0, OCOUNT, OREC, OTABL, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1100) IER, 'WRIT', OCOUNT
            GO TO 900
            END IF
         END IF
C                                       close output
      OTABL(5) = OCOUNT
C                                       transfer sort order ?
      IF (.NOT.SAME) THEN
         IP = ABS (ITABL(43))
         IF (IP.GT.256) IP = IP - 256
         JJP = ABS (ITABL(44))
         IF (JJP.GT.256) JJP = JJP - 256
         DO 210 I = 1,NS
            IF ((SKOL(I).EQ.IP) .OR. (SKOL(I).EQ.JJP)) GO TO 220
 210        CONTINUE
         OTABL(43) = ITABL(43)
         OTABL(44) = ITABL(44)
         END IF
 220  CALL TABIO ('CLOS', 0, OCOUNT, OREC, OTABL, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1100) IER, 'CLOS', OCOUNT
         GO TO 900
         END IF
C                                       close input
      IF (.NOT.SAME) CALL TABIO ('CLOS', 0, IRNO, IREC, ITABL, IER)
      OUTNUM = OCOUNT
      IERR = 0
      GO TO 999
C                                       Error close down
 900  CALL MSGWRT (8)
      CALL ZCLOSE (OLUN, OFIND, IER)
      IF (.NOT.SAME) CALL ZCLOSE (ILUN, IFIND, IER)
C                                       destroy new files only
      IF (OUTVER.LE.LIM) GO TO 999
         CALL ZPHFIL (TYPE, DISK, CNO, OUTVER, PHNAME, IER)
         CALL ZDESTR (DISK, PHNAME, IER)
         IF (IER.NE.0) GO TO 999
            CATBLK(JP) = LIM
            CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', ITABL, IER)
            GO TO 999
C
 990  CALL MSGWRT (8)
      IF (OFIND.GT.0) CALL ZCLOSE (OLUN, OFIND, IER)
      IF ((IFIND.GT.0) .AND. (IFIND.NE.OFIND)) CALL ZCLOSE (ILUN,
     *   IFIND, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TABMRG: ERROR',I7,' OPENING INPUT TABLE')
 1025 FORMAT ('TABMRG: CANNOT SUM LOGICALS, CHARACTERS, OR BITS')
 1035 FORMAT ('TABMRG: NO VALID TEST COLUMNS SPECIFIED')
 1055 FORMAT ('TABMRG: ERROR',I7,' OPENING OUTPUT TABLE')
 1060 FORMAT ('TABMRG: ZFIO ERROR',I7,1X,A4,'ING RECORD',I10)
 1100 FORMAT ('TABMRG: TABIO ERROR',I7,1X,A4,'ING ROW',I10)
 1140 FORMAT ('TABMRG: COLUMN TYPE 6 IN COLUMN',I4,' NO LONGER LEGAL')
      END
