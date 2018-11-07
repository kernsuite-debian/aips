      SUBROUTINE BTBSRT (KEY, FKEY, COLTYP, IPCOL, BUFFER, TABUFF, DISK,
     *   CNO, TYPE, INVER, OUTVER, CATBLK, IERR)
C-----------------------------------------------------------------------
C! Read a table, list keys, sort list, write output table
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2008, 2015
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
C   BTBSRT is suitable for tables with very long rows.  It reads the
C   table making an in-core list of key values with row number.  It
C   then sorts the list.  Finally it writes the output table in order
C   reading the input table "randomly".
C   The columns and factors are specified in KEY and FKEY, the first
C   (Slowest varying key) is:
C      KEY_VALUE1 = COL_VALUE(KEY(1,1)) * FKEY(1,1) +
C                   COL_VALUE(KEY(2,1)) * FKEY(2,1)
C   The  faster changing key value is:
C      KEY_VALUE2 = COL_VALUE(KEY(1,2)) * FKEY(1,2) +
C                   COL_VALUE(KEY(2,2)) * FKEY(2,2)
C   In the case of bit or character strings only one column is used to
C   generate the key values.  If KEY(m,n) < 0, routine uses
C   ABS (COL_VALUE(-KEY(m,n))).
C   Inputs:
C      KEY(2,2)     I    Sort keys: may be linear combination of two
C                        numeric value columns.  KEY contains the column
C                        numbers and FKEY contains the factors.  If the
C                        column is a string (bit or char.) then
C                        FKEY(1,n)=first char/bit and FKEY(2,n)=number
C                        of char/bit and KEY(2,n) is ignored.
C                        KEY(2,n)=0 => ignore, <0 => use abs. value.
C                        Column no. is the logical number.
C     FKEY(2,2)    R    Key  coefficients, 0=>1, see above.
C     COLTYP(2,2)  I    Column data type of specified columns:
C                       1=D,   2=R,   3=char., 4=I,   5=L,
C                       6=Illegal,   7=Bit array.
C     IPCOL(2,2)   I    Pointers in appropriate array for data type.
C   In/out:
C     TABUFF(*)    I    Buffer large enough to handle I/O to table.
C                       in: table pointers, revised by I/O here
C   Output:
C     BUFFER(*)    R    I/O work buffer
C     ISCR         I    FILES common number of 1st scratch file of 2
C     NBUFF        I    Number  of buffers to use in AMERGE
C     NSORT        I    Size of presorted blocks.
C     IN           I    Pointer in BUFFER for input in AMERGE
C     OUT          I    Pointer in BUFFER for output in AMERGE
C     LENBU        I    Number of values in buffer for sorting.
C     IERR         I    Return error code.: 0 = OK
C                       1 = buffers too small
C                       2 = can't make scratch files.
C                       3 = can't open input file.
C                       4 = sort failure.
C                       8 = I/O error
C-----------------------------------------------------------------------
      INTEGER   KEY(2,2), COLTYP(2,2), IPCOL(2,2), TABUFF(*),
     *   DISK, CNO, INVER, OUTVER, CATBLK(256), IERR
      REAL      FKEY(2,2), BUFFER(*)
      CHARACTER TYPE*2
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER PHNAME*48, CHR*32, CHTMP*2, OUTNAM*48
      INTEGER   NICOPY, ILRECO, ICHA, INDEX, NVSUM, ICOL, NOCOL, IP,
     *   IICHR, NCHR, JKEY,IKEY, IPTR2, KEY1, KEY2, KKEY, LKEY, LSIZE,
     *   ILUN, OLUN, IIND, OIND, OVO, II, LIM, NFIRST, IRNO, ORNO, NRPR,
     *   RECNO, LER
      LOGICAL   T, F
C                                       Maximum size of table record.
      INTEGER   MXREC
      PARAMETER (MXREC = XBPRSZ+2)
      INTEGER   RECORD(MXREC), ITBRNO, I, NUMREC
      REAL      RECR(MXREC), FFKEY
      HOLLERITH RECH(MXREC)
      DOUBLE PRECISION    RECD(MXREC/2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (RECORD, RECH, RECR, RECD)
      DATA T, F /.TRUE.,.FALSE./
      DATA ILUN, OLUN /16, 17/
C-----------------------------------------------------------------------
C                                       Set keys.
      KEY1 = 1
      KEY2 = 2
C                                       Get number of records.
      NUMREC = TABUFF(5)
      NICOPY = TABUFF(8)
C                                       Prepare pointers and counters.
      ITBRNO = 1
      NVSUM = 0
      DO 100 I = 1,NUMREC
         IPTR2 = (I - 1) * 3
         ITBRNO = I
         CALL TABIO ('READ', 0, ITBRNO, RECORD, TABUFF, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) 'READ INPUT', IERR
            IERR = 8
            GO TO 998
            END IF
C                                       Sum no. records
         NVSUM = NVSUM + 1
C                                       Loop thru input, creat keys and
C                                       adding record no. to file.
C                                       Since AMERGE orders by
C                                       descending values of the keys
C                                       negate keys.
         BUFFER(IPTR2+3) = ITBRNO
         DO 50 IKEY = 1,2
            BUFFER(IPTR2+IKEY) = 0.0
            IF (KEY(1,IKEY).EQ.0) GO TO 50
            NOCOL = 2
            IF (KEY(2,IKEY).EQ.0) NOCOL = 1
            DO 30 ICOL = 1,NOCOL
C                                       Select key.
               IP = IPCOL(ICOL,IKEY)
               KKEY = COLTYP(ICOL,IKEY)
               LKEY = KEY(ICOL,IKEY)
               FFKEY = FKEY(ICOL,IKEY)
C                                       Double precision (1)
               IF (KKEY.EQ.1) THEN
                  IF (LKEY.GT.0) THEN
                     BUFFER(IPTR2+IKEY) = BUFFER(IPTR2+IKEY) -
     *                  RECD(IP) * FFKEY
                  ELSE
                     BUFFER(IPTR2+IKEY) = BUFFER(IPTR2+IKEY) -
     *                  ABS (RECD(IP)) * FFKEY
                     END IF
C                                       Single precision (2)
               ELSE IF (KKEY.EQ.2) THEN
                  IF (LKEY.GT.0) THEN
                     BUFFER(IPTR2+IKEY) = BUFFER(IPTR2+IKEY) -
     *                  RECR(IP) * FFKEY
                  ELSE
                     BUFFER(IPTR2+IKEY) = BUFFER(IPTR2+IKEY) -
     *                  ABS (RECR(IP)) * FFKEY
                     END IF
C                                       Char. string. (3)
               ELSE IF (KKEY.EQ.3) THEN
                  NCHR = FKEY(2,IKEY) + 0.5
                  INDEX = FKEY(1,IKEY) + 0.5
                  ICHA = ICHAR ('A')
                  CALL H2CHR (NCHR, INDEX, RECH(IP), CHR)
                  DO 20 IICHR = 1,NCHR
C                                       Use char-"A" * 64**(NCHR-IICHR)
                     BUFFER(IPTR2+IKEY) = BUFFER(IPTR2+IKEY) -
     *                  (ICHAR (CHR(IICHR:IICHR)) - ICHA) *
     *                  64 ** (NCHR-IICHR)
 20                  CONTINUE
C                                       Integer (4)
               ELSE IF (KKEY.EQ.4) THEN
                  IF (LKEY.GT.0) THEN
                     BUFFER(IPTR2+IKEY) = BUFFER(IPTR2+IKEY) -
     *                  RECORD(IP) * FFKEY
                  ELSE
                     BUFFER(IPTR2+IKEY) = BUFFER(IPTR2+IKEY) -
     *                  ABS (RECORD(IP)) * FFKEY
                     END IF
C                                       Logical (use I)   (5)
               ELSE IF (KKEY.EQ.5) THEN
                  BUFFER(IPTR2+IKEY) = BUFFER(IPTR2+IKEY) -
     *               RECORD(IP) * FKEY(ICOL,JKEY)
C                                       Short integer (Illegal) (6)
               ELSE IF (KKEY.EQ.6) THEN
                  IERR = 5
                  MSGTXT = 'SHORT INTEGERS NOW ILLEGAL'
                  GO TO 998
C                                       Bit array (7)
C                                       This doesn't make  much sense
C                                       use I   array.
               ELSE IF (KKEY.EQ.7) THEN
                  BUFFER(IPTR2+IKEY) = BUFFER(IPTR2+IKEY) -
     *               RECORD(IP) * FKEY(ICOL,JKEY)
                  END IF
 30            CONTINUE
 50         CONTINUE
 100     CONTINUE
C                                       Buffer loaded ready to sort.
      ILRECO = 3
      CALL ICSORT (BUFFER, ILRECO, NVSUM, KEY1, KEY2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'ICSORT FAILS', IERR
         IERR = 4
         GO TO 998
         END IF
C                                       close table IO
      CALL TABIO ('CLOS', 0, ITBRNO, RECORD, TABUFF, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) 'CLOSE INPUT', IERR
         IERR = 8
         GO TO 998
         END IF
C                                       open input file
      CALL ZPHFIL (TYPE, DISK, CNO, INVER, PHNAME, IERR)
      CALL ZEXIST (DISK, PHNAME, LSIZE, IERR)
      CALL ZOPEN (ILUN, IIND, DISK, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'REOPEN INPUT', IERR
         GO TO 998
         END IF
      CALL ZFIO ('READ', ILUN, IIND, 1, TABUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'REREAD INPUT 1', IERR
         GO TO 998
         END IF
C                                       output file
      OVO = 0
      CALL FXHDEX (CATBLK)
      DO 110 I = 1,KIEXTN
         II = KHEXT + I - 1
         CALL H2CHR (2, 1, CATBLK(II), CHTMP)
         IF (TYPE.EQ.CHTMP) OVO = I
 110     CONTINUE
C                                       Set output version number
      LIM = CATBLK(KIVER+OVO-1) + 1
      IF ((OVO.LE.0) .OR. (LIM.LE.0)) THEN
         IERR = 2
         MSGTXT = 'ERROR IN CATALOG HEADER - NO EXTENSION OF TYPE '
     *      // TYPE
         GO TO 998
         END IF
      IF (OUTVER.LE.0) OUTVER = LIM
      IF (OUTVER.GT.LIM) OUTVER = LIM
C                                       Initialize control record.
C                                       Set sort order
      TABUFF(43) = KEY(1,1)
      IF (TABUFF(43).LT.0) TABUFF(43) = 256 - KEY(1,1)
      TABUFF(44) = KEY(1,2)
      IF (TABUFF(44).LT.0) TABUFF(44) = 256 - KEY(1,2)
      IF (FKEY(1,1).LT.0.0) TABUFF(43) = -TABUFF(43)
      IF (FKEY(1,2).LT.0.0) TABUFF(44) = -TABUFF(44)
C                                       Other control info.
      TABUFF(32) = DISK
      CALL ZDATE (TABUFF(33))
      CALL ZTIME (TABUFF(36))
      CALL ZPHFIL (TYPE, DISK, CNO, OUTVER, OUTNAM, IERR)
      CALL CHR2H (48, OUTNAM, 1, TABUFF(17))
      CALL CHR2H (6, TSKNAM, 1, TABUFF(39))
C                                       use LIM for now
      CALL ZPHFIL ('SC', DISK, CNO, 2, PHNAME, IERR)
C                                       Does it already exist
      CALL ZEXIST (DISK, PHNAME, I, IERR)
C                                       destroy mystery one
      IF (IERR.NE.1) THEN
         CALL ZDESTR (DISK, PHNAME, IERR)
         WRITE (MSGTXT,1000) 'DESTROY PRE-EXISTING SC', IERR
         IF (IERR.GT.2) GO TO 998
         END IF
C                                       Create
      CALL ZCREAT (DISK, PHNAME, LSIZE, F, I, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) 'CREATE SCRATCH', IERR
         IF (IERR.EQ.3) MSGTXT = 'OTBSRT: INSUFFICIENT SPACE ON DISK'
         IF (IERR.EQ.5) WRITE (MSGTXT,1021)
         GO TO 998
         END IF
C                                       Open output (SC) file
      CALL ZOPEN (OLUN, OIND, DISK, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'OPEN OUTPUT', IERR
         GO TO 990
         END IF
C                                       write first record
      CALL ZFIO ('WRIT', OLUN, OIND, 1, TABUFF(1), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'WRITE OUTPUT 1', IERR
         GO TO 990
         END IF
C                                       copy init table sections
      NFIRST = TABUFF(50) - 1
      DO 120 IRNO = 2,NFIRST
         CALL ZFIO ('READ', ILUN, IIND, IRNO, TABUFF(257), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) 'READ INPUT', IRNO, IERR
            GO TO 990
            END IF
         CALL ZFIO ('WRIT', OLUN, OIND, IRNO, TABUFF(257), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) 'WRITE OUTPUT', IRNO, IERR
            GO TO 990
            END IF
 120     CONTINUE
C                                       copy data
      ORNO = NFIRST
      NRPR = -TABUFF(9)
      DO 150 RECNO = 1,NUMREC
         IPTR2 = RECNO * 3
         I = BUFFER(IPTR2) + 0.1
         IRNO = NFIRST + (I - 1) * NRPR
         DO 140 I = 1,NRPR
            IRNO = IRNO + 1
            CALL ZFIO ('READ', ILUN, IIND, IRNO, TABUFF(257), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) 'READ INPUT', IRNO, IERR
               GO TO 990
               END IF
            ORNO = ORNO + 1
            CALL ZFIO ('WRIT', OLUN, OIND, ORNO, TABUFF(257), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) 'WRITE OUTPUT', ORNO, IERR
               GO TO 990
               END IF
 140        CONTINUE
 150     CONTINUE
C                                       close
      CALL ZCLOSE (ILUN, IIND, IERR)
      CALL ZCLOSE (OLUN, OIND, IERR)
C                                       does outname exist
      CALL ZEXIST (DISK, OUTNAM, LSIZE, IERR)
      IF (IERR.EQ.0) CALL ZDESTR (DISK, OUTNAM, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1150) IERR
         CALL MSGWRT (8)
         CALL ZDESTR (DISK, PHNAME, LER)
      ELSE
         CALL ZRENAM (DISK, PHNAME, OUTNAM, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1155) IERR
            CALL MSGWRT (8)
            CALL ZDESTR (DISK, PHNAME, LER)
            END IF
         END IF
C                                       No error only:
C                                       Update CATBLK
      IF (IERR.EQ.0) THEN
         I = KIVER + OVO - 1
         CATBLK(I) = MAX (OUTVER, CATBLK(I))
         IF (CATBLK(I).GE.LIM) CALL CATIO ('UPDT', DISK, CNO, CATBLK,
     *      'REST', TABUFF, IERR)
         END IF
      GO TO 999
C                                       error after zcreat
 990  CALL MSGWRT (8)
      CALL ZDESTR (DISK, PHNAME, LER)
      GO TO 999
C                                       Error
 998  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BTBSRT: ',A,' ERROR',I3)
 1010 FORMAT ('BTBSRT: ',A,' REC',I6,' ERROR',I3)
 1021 FORMAT ('BTBSRT: DISK PROHIBITED FOR CREATING OUTPUT TABLE FILE')
 1150 FORMAT ('BTBSRT: ERROR',I5,' REMOVING OLD COPY OF EXTENSION FILE')
 1155 FORMAT ('BTBSRT: ERROR',I5,' ERROR RENAMING SCR TABLE TO OUTPUT')
      END
