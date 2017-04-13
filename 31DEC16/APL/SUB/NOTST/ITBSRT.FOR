      SUBROUTINE ITBSRT (KEY, FKEY, COLTYP, IPCOL, BUFFER, BUFSZ,
     *   TABUFF, ISCR, NBUFF, NSORT, IN, OUT, LENBU, IERR)
C-----------------------------------------------------------------------
C! Read a table and write a scratch file to be sorted.
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2003, 2007, 2011
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
C   ITBSRT reads an input Table, creates scratch files, and presorts
C   blocks of records into a scratch file.  Only the keys and record
C   numbers are kept in the file.
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
C     BUFSZ        I    Size of buffer in bytes.
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
      INTEGER   KEY(2,2), COLTYP(2,2), IPCOL(2,2), BUFSZ, TABUFF(*),
     *   ISCR, NBUFF, NSORT, IN, OUT, LENBU, IERR
      REAL      FKEY(2,2), BUFFER(*)
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER PHNAME*48, CHR*32
      INTEGER   MAXBUF, ILIM, NICOPY, BIND2, ILRECO, INVSUM, ICHA,
     *   INDEX, NVSUM, IBSTRT, ICOL, NOCOL, IP, IICHR, NCHR, JKEY,
     *   IBUFSZ, IKEY, IND2, IPTR2, LENIO, LRECO, LUN2, NIOUT, KEY1,
     *   KEY2, KKEY, KOUT, LKEY, LOOP
      LOGICAL   T, F, EOI
C                                       Maximum size of table record.
      INTEGER   MXREC
      PARAMETER (MXREC = XBPRSZ+2)
      INTEGER   RECORD(MXREC), BO, VO, ITBRNO, I, LIMIT, NUMREC, ISIZE
      REAL      RECR(MXREC), FFKEY
      HOLLERITH RECH(MXREC)
      DOUBLE PRECISION    RECD(MXREC/2)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RECORD, RECH, RECR, RECD)
      DATA T, F /.TRUE.,.FALSE./
      DATA BO, VO, MAXBUF /1, 0, 4/
C-----------------------------------------------------------------------
C                                       Set keys.
      KEY1 = 1
      KEY2 = 2
C                                       Get number of records.
      NUMREC = TABUFF(5)
C                                       Set length of buffers.
      LRECO = 2 + TABUFF(8)
C                                       Check size of table record.
      IF (LRECO.GT.MXREC) THEN
         IERR = 1
         WRITE (MSGTXT,1020) LRECO
         GO TO 998
         END IF
C
      NICOPY = TABUFF(8)
      NBUFF = MAXBUF
      LENBU = (BUFSZ - (2 * NBPS * (NBUFF+1))) /
     *   ((NBUFF+1) * LRECO * 2)
C                                       ICSORT now big buffers >> 1024
      ILIM = 204800 / NBUFF
      LENBU = MIN (ILIM, LENBU)
C                                       Make sure buffer big enough.
      IF ((LENBU.LE.0) .AND. (NBUFF.GE.4)) THEN
C                                       Try fewer buffers if necessary;
C                                       need at least two for merge sort.
         NBUFF = NBUFF / 2
         LENBU = (BUFSZ - (2 * NBPS * (NBUFF+1))) /
     *      ((NBUFF+1) * LRECO * 2)
         ILIM = 1024 / NBUFF
         LENBU = MIN (ILIM, LENBU)
         END IF
      IF (LENBU.LE.0) THEN
         IERR = 1
         WRITE (MSGTXT,1000) BUFSZ, LRECO
         GO TO 998
         END IF
C                                       Set number pre sorted.
      NSORT = NBUFF * LENBU
      IN = 1
      OUT = IN + LRECO * LENBU + NBPS
C                                       Make sure scratch files are
C                                       two buffers larger than nec.
      CALL UVSIZE (LRECO, NUMREC, ISIZE)
      CALL SCREAT (ISIZE, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 50
         WRITE (MSGTXT,1030) IERR
         IERR = 2
         GO TO 998
 50   ISCR = NSCR
      CALL SCREAT (ISIZE, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 70
         WRITE (MSGTXT,1030) IERR
         IERR = 2
         GO TO 998
C                                       Open output file.
 70   LUN2 = 17
      CALL ZPHFIL ('SC', SCRVOL(ISCR), SCRCNO(ISCR), 1, PHNAME, IERR)
      CALL ZOPEN (LUN2, IND2, SCRVOL(ISCR), PHNAME, T, T, T, IERR)
      IF (IERR.EQ.0) GO TO 90
         WRITE (MSGTXT,1070) IERR
         IERR = 3
         GO TO 998
C                                       Initialize output, single
C                                       buffer.
 90   IBUFSZ = LRECO * LENBU * 2  +  2 * NBPS
      NIOUT = LENBU
      CALL UVINIT ('WRIT', LUN2, IND2, NUMREC, VO, LRECO, NIOUT, IBUFSZ,
     *   BUFFER(OUT), BO, BIND2, IERR)
      IF (IERR.EQ.0) GO TO 100
         WRITE (MSGTXT,1080) IERR, 'WRIT'
         IERR = 8
         GO TO 999
C                                       Prepare pointers and counters.
 100  EOI = F
      ITBRNO = 1
C                                       Begin loop.
 200  CONTINUE
C                                       Read NBUFF buffers, add keys and
C                                       place in output buffer area.
C                                       Fill output buffer with
C                                       NBUFF input buffers.
         NVSUM = 0
         IPTR2 = OUT + BIND2 - 1
         LIMIT = NBUFF * LENBU
         IF (LIMIT.GT.NUMREC) LIMIT = NUMREC
         DO 400 I = 1,LIMIT
            IF (EOI) GO TO 410
            CALL TABIO ('READ', 0, ITBRNO, RECORD, TABUFF, IERR)
C                                       Get flag
            RECORD(NICOPY) = 1
            IF (IERR.LT.0) RECORD(NICOPY) = 0
            IF (IERR.LE.0) GO TO 210
               IF (IERR.EQ.4) GO TO 410
                  WRITE (MSGTXT,1200) 'READ',IERR
                  IERR = 8
                  GO TO 998
C                                       Sum no. records
 210        ITBRNO = ITBRNO + 1
            NVSUM = NVSUM + 1
C                                       Check if all data read.
            EOI = EOI .OR. (ITBRNO.GT.NUMREC)
C                                       Loop thru input, creat keys and
C                                       adding record no. to file.
C                                       Since AMERGE orders by
C                                       descending values of the keys
C                                       negate keys.
            IKEY = 0
            DO 350 JKEY = 1,2
               BUFFER(IPTR2+IKEY) = 0.0
               IF (KEY(1,JKEY).EQ.0) GO TO 350
               NOCOL = 2
               IF (KEY(2,JKEY).EQ.0) NOCOL = 1
               DO 330 ICOL = 1,NOCOL
C                                       Select key.
                  IP = IPCOL(ICOL,JKEY)
                  KKEY = COLTYP(ICOL,JKEY)
                  LKEY = KEY(ICOL,JKEY)
                  FFKEY = FKEY(ICOL,JKEY)
                  GO TO (220, 230, 240, 250, 260, 270, 280), KKEY
C                                       Double precision (1)
 220                 IF (LKEY.GT.0) BUFFER(IPTR2+IKEY) =
     *                  BUFFER(IPTR2+IKEY) - RECD(IP) * FFKEY
                     IF (LKEY.LT.0) BUFFER(IPTR2+IKEY) =
     *                  BUFFER(IPTR2+IKEY) - ABS (RECD(IP)) * FFKEY
                     GO TO 330
C                                       Single precision (2)
 230                 IF (LKEY.GT.0) BUFFER(IPTR2+IKEY) =
     *                  BUFFER(IPTR2+IKEY) - RECR(IP) * FFKEY
                     IF (LKEY.LT.0) BUFFER(IPTR2+IKEY) =
     *                  BUFFER(IPTR2+IKEY) - ABS (RECR(IP)) * FFKEY
                     GO TO 330
C                                       Char. string. (3)
 240                 NCHR = FKEY(2,JKEY) + 0.5
                     INDEX = FKEY(1,JKEY) + 0.5
                     ICHA = ICHAR ('A')
                     CALL H2CHR (NCHR, INDEX, RECH(IP), CHR)
                     DO 245 IICHR = 1,NCHR
C                                       Use char-"A" * 64**(NCHR-IICHR)
                        BUFFER(IPTR2+IKEY) = BUFFER(IPTR2+IKEY) -
     *                     (ICHAR (CHR(IICHR:IICHR)) - ICHA) *
     *                     64 ** (NCHR-IICHR)
 245                    CONTINUE
                     GO TO 330
C                                       Integer (4)
 250                 IF (LKEY.GT.0) BUFFER(IPTR2+IKEY) =
     *                  BUFFER(IPTR2+IKEY) - RECORD(IP) * FFKEY
                     IF ((LKEY.LT.0) .AND. (RECORD(IP).GT.0))
     *                  BUFFER(IPTR2+IKEY) = BUFFER(IPTR2+IKEY) -
     *                  RECORD(IP) * FFKEY
                     IF ((LKEY.LT.0) .AND. (RECORD(IP).LT.0))
     *                  BUFFER(IPTR2+IKEY) = BUFFER(IPTR2+IKEY) +
     *                  RECORD(IP) * FFKEY
                     GO TO 330
C                                       Logical (use I)   (5)
 260                 BUFFER(IPTR2+IKEY) = BUFFER(IPTR2+IKEY) -
     *                  RECORD(IP) * FKEY(ICOL,JKEY)
                     GO TO 330
C                                       Short integer (Illegal) (6)
 270                 IERR = 5
                     MSGTXT = 'SHORT INTEGERS NOW ILLEGAL'
                     GO TO 998
C                                       Bit array (7)
C                                       This doesn't make  much sense
C                                       use I   array.
 280                 BUFFER(IPTR2+IKEY) = BUFFER(IPTR2+IKEY) -
     *                  RECORD(IP) * FKEY(ICOL,JKEY)
                     GO TO 330
 330                 CONTINUE
C                                       Get set for second key.
                  IKEY = 1
 350              CONTINUE
C                                       Load record number.
               DO 360 LOOP = 1,NICOPY
                  BUFFER(IPTR2+LOOP+1) = RECR(LOOP)
 360              CONTINUE
C                                       Update pointer.
               IPTR2 = IPTR2 + LRECO
 400        CONTINUE
 410     IF (NVSUM.LE.0) GO TO 520
C                                       Output buffer now loaded ready
C                                       to sort.
         KOUT = OUT + BIND2 - 1
         ILRECO = LRECO
         INVSUM = NVSUM
         CALL ICSORT (BUFFER(KOUT), ILRECO, INVSUM, KEY1, KEY2, IERR)
         IF (IERR.EQ.0) GO TO 430
            WRITE (MSGTXT,1400) IERR
            IERR = 4
            GO TO 998
C                                       Dump output buffer to disk
C                                       Trick UVDISK by moving buffer
C                                       pointer adjusted to take care
C                                       of unwritten data.
 430     IBSTRT = BIND2 - 1
         DO 500 I = 1,NBUFF
            IF (NVSUM.LE.0) GO TO 510
            NIOUT = MIN (LENBU, NVSUM)
            NVSUM = NVSUM - NIOUT
            LENIO = (I-1) * LENBU * LRECO  +  IBSTRT
            KOUT = OUT + LENIO - MOD (LENIO, NBPS/2)
            CALL UVDISK ('FLSH', LUN2, IND2, BUFFER(KOUT), NIOUT, BIND2,
     *         IERR)
            IF (IERR.EQ.0) GO TO 440
               WRITE (MSGTXT,1200) 'FLSH' ,IERR
               IERR = 8
               GO TO 998
 440        IF (NIOUT.LE.0) GO TO 510
 500        CONTINUE
C                                        Copy any unwritten data
C                                        to start of buffer.
 510     IF (BIND2.GT.1) THEN
            DO 515 LOOP = 1,BIND2
               BUFFER(OUT+LOOP-1) = BUFFER(KOUT+LOOP-1)
 515           CONTINUE
            END IF
C                                       If not finished loop back
         IF (.NOT.EOI) GO TO 200
C                                       Empty buffer.
 520  NIOUT = 0
      IF (BIND2.LE.1) GO TO 550
         CALL UVDISK ('FLSH', LUN2, IND2, BUFFER(KOUT), NIOUT, BIND2,
     *      IERR)
         IF (IERR.EQ.0) GO TO 550
            WRITE (MSGTXT,1200) 'FLSH', IERR
            CALL MSGWRT (8)
            IERR = 8
C                                       Switch IN and OUT
 550  I = IN
      IN = OUT
      OUT = I
C                                       Close files.
      CALL ZCLOSE (LUN2, IND2, IERR)
      IERR = 0
      GO TO 999
C                                       Error
 998  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ITBSRT: BUFFER SIZE',I8,' TOO SMALL FOR LREC=',I6)
 1020 FORMAT ('ITBSRT: RECORD SIZE TOO SMALL FOR LREC=',I8)
 1030 FORMAT ('ITBSRT: ERROR',I3,' CREATING SCRATCH FILE')
 1070 FORMAT ('ITBSRT: OPEN ERROR',I3,' ON SCRATCH FILE')
 1080 FORMAT ('ITBSRT: INIT ERROR',I3,' ON ',A4)
 1200 FORMAT ('ITBSRT:',A4,' ERROR',I3)
 1400 FORMAT ('ITBSRT: ERROR',I3,' DURING INCORE SORT')
      END
