      SUBROUTINE TABSRT (DISK, CNO, TYPE, INVER, OUTVER, KEY, KEYSUB,
     *   FKEY, TABUFF, CATIN, IERR)
C-----------------------------------------------------------------------
C! Sorts the entries in an AIPS table.
C# EXT-util IO-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2005, 2007-2008, 2011-2012, 2015, 2018
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
C   Subroutine to sort an AIPS table extension file.  First key changes
C   the most slowly.  A linear combination of two columns or a substring
C   of a bit or character string may be used.  The columns and factors
C   are specified in KEY and FKEY, the first (slower varying key) is:
C      KEY_VALUE1 = COL_VALUE(KEY(1,1)) * FKEY(1,1) +
C                   COL_VALUE(KEY(2,1)) * FKEY(2,1)
C   The faster changing key value is:
C      KEY_VALUE2 = COL_VALUE(KEY(1,2)) * FKEY(1,2) +
C                   COL_VALUE(KEY(2,2)) * FKEY(2,2)
C   In the case of bit or character strings only one column is used to
C   generate the key values.
C   A KEY(m,n) < 0 => use ABS (COL_VALUE(-KEY(m,n))).
C   Inputs:
C      DISK     I        Disk number of the file.
C      CNO      I        Catalog slot number.
C      TYPE     I        Two character type code (e.g. 'CC')
C      INVER    I        Input version number
C      OUTVER   I        Output version number
C      KEY      I(2,2)   Sort keys: may be linear combination of two
C                        numeric value columns.  KEY contains the column
C                        numbers and FKEY contains the factors.  If the
C                        column is a string (bit or char.) then
C                        FKEY(1,n)=first char/bit and FKEY(2,n)=number
C                        of char/bit and KEY(2,n) is ignored.
C                        KEY(2,n)=0 => ignore, <0 => use abs. value.
C                        Column no. is the logical number.
C      KEYSUB   I(2,2)   Subscript in array columns for sort key value
C                        Normally (1,1,1,1)
C      FKEY     R(2,2)   Key  coefficients, 0=>1, see above.
C      CATIN    I(256)   Catalog header record.
C   Output:
C      TABUFF   I(512)   Buffer to handle I/O to table.
C      IERR     I        Error code, 0 => OK, else error.
C                        10 => Couldn't find or open file.
C   Useage Notes:
C        Normally the keys are sorted into ascending order, to sort into
C   descending order negate the values of FKEYn.
C        TWO standard scratch files will be created and entered into the
C   /CFILES/ common.  These scratch files will be deleted on normal
C   termination.  Includes DFIL.INC should be included in the main
C   routine and a call made to DIE rather than DIETSK should be made at
C   the end of the program execution.  The values in BADD (adverb
C   BADDISK) in the /CFILES/ common should be initialized.
C        IF a disk based sort is required, then a 4-way merge sort will
C   be used; the FTAB declaration in the main program and the call to
C   ZDCHIN should be large enough to handle 8 map-like files and 2
C   Non-map files at the same time.  (Additional files may be required
C   if they are left open by the calling program).
C        Since keys are converted into floating point numbers some
C   accuracy may be lost sorting on character or bit strings.
C        For a 1 key sort use KEY2(1) = 0.
C   NB: Will modify the contents of common /MAPHDR/.
C   CODE ADDED: 11/26/08 will invoke a brute force sort for long data
C   rows.  This sorts a list of key values plus row-in number in a
C   brute-force sort and then reads in/writes out directly.
C   Uses large dynamic memory (09/21/11) instead of input.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, INVER, OUTVER, KEY(2,2), KEYSUB(2,2),
     *   TABUFF(*), CATIN(256), IERR
      REAL      FKEY(2,2)
      CHARACTER TYPE*2
C
      INTEGER   LUN, NKEY, NREC, NCOL, DATP(128,2), IP1, IP2, ISCR,
     *   IPCOL(2,2), STRLEN(2,2), COLTYP(2,2), J, NBUFF, NSORT, IN, OUT,
     *   KEY1, KEY2, LRECM, LUNM(10), LENBU, SCRPNT, SCFRW(10), MSCR,
     *   IBUFF, NUMREC, BUFSZ, NWORDS
      LONGINT   PBUF
      REAL      BUFFER(256)
      CHARACTER PNAME*6
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNM /16,17,18,19,20,21,22,23,24,25/
C-----------------------------------------------------------------------
C                                       Save pointer in scratch file
C                                       common
      SCRPNT = NSCR + 1
      MSCR = NSCR
      PNAME = 'SORT' // TYPE
C                                       Open table file and create
C                                       scratch file.
      NREC = 20
      NCOL = 0
      NKEY = 0
      LUN = 89
      CALL TABINI ('READ', TYPE, DISK, CNO, INVER, CATIN, LUN, NKEY,
     *   NREC, NCOL, DATP, TABUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         IERR = 10
         GO TO 990
         END IF
C                                       Get number of records.
      NUMREC = TABUFF(5)
C                                       Check for empty table
      IF (NUMREC.LE.0) THEN
         CALL TABIO ('CLOS', 0, 1, TABUFF, TABUFF, IERR)
         IF (INVER.NE.OUTVER) CALL TABCOP (TYPE, INVER, OUTVER, LUNM(1),
     *      LUNM(2), DISK, DISK, CNO, CNO, CATIN, TABUFF, TABUFF(257),
     *      IERR)
         GO TO 999
         END IF
C                                       get big work space
      NWORDS = TABUFF(8) + 5
      NWORDS = MAX (NUMREC*NWORDS, 102400)
      NWORDS = (NWORDS - 1) / 1024 + 1
      NWORDS = MIN (NWORDS, KAPWRD)
      CALL ZMEMRY ('GET ', PNAME, NWORDS, BUFFER, PBUF, IERR)
C                                       try 2nd time
      IF (IERR.NE.0) THEN
         NWORDS = (NWORDS-1) / 8 + 1
         CALL ZMEMRY ('GET ', PNAME, NWORDS, BUFFER, PBUF, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'TABSRT: UNABLE TO GET NEEDED DYNAMIC MEMORY'
            GO TO 990
            END IF
         END IF
      BUFSZ = 2048 * NWORDS
C                                       Get column types
      DO 120 J = 1,2
         IP1 = ABS (KEY(1,J))
         IPCOL(1,J) = 1
         IF (IP1.GT.0) IPCOL(1,J) = DATP(IP1,1)
         IP2 = ABS (KEY(2,J))
         IPCOL(2,J) = 1
         IF (IP2.GT.0) IPCOL(2,J) = DATP(IP2,1)
C                                       Check factors.
         IF (FKEY(1,J).EQ.0.0) FKEY(1,J) = 1.0
         IF (FKEY(2,J).EQ.0.0) FKEY(2,J) = 1.0
C                                       Column out of range.
         IF (((IP1.GE.1) .AND. (IP1.LE.NCOL)) .AND.
     *      ((IP2.GE.0) .AND. (IP2.LE.NCOL))) GO TO 110
         IF ((J.EQ.2) .AND. (IP1.EQ.0)) GO TO 110
            IERR = 1
            WRITE (MSGTXT,1100) J, IP1, IP2
            GO TO 990
C                                       Get type (and length) of col.
 110     STRLEN(1,J) = DATP(IP1,2) / 10
         COLTYP(1,J) = DATP(IP1,2) - STRLEN(1,J) * 10
         IF (IP2.GT.0) STRLEN(2,J) = DATP(IP2,2) / 10
         IF (IP2.GT.0) COLTYP(2,J) = DATP(IP2,2) - STRLEN(2,J) * 10
C                                       Only 1 col. for strings.
         IF ((COLTYP(1,J).EQ.3) .OR. (COLTYP(2,J).EQ.3) .OR.
     *      (COLTYP(1,J).EQ.7) .OR. (COLTYP(2,J).EQ.7)) THEN
            KEY(2,J) = 0
            IP2 = 0
            END IF
         IF ((KEYSUB(1,J).GE.1) .AND. (KEYSUB(1,J).LE.STRLEN(1,J)) .AND.
     *      (COLTYP(1,J).NE.3) .AND. (COLTYP(1,J).NE.7)) IPCOL(1,J) =
     *      IPCOL(1,J) + KEYSUB(1,J) - 1
         IF ((KEYSUB(2,J).GE.1) .AND. (KEYSUB(2,J).LE.STRLEN(2,J)) .AND.
     *      (COLTYP(2,J).NE.3) .AND. (COLTYP(2,J).NE.7) .AND.
     *      (IP2.NE.0)) IPCOL(2,J) = IPCOL(2,J) + KEYSUB(2,J) - 1
 120     CONTINUE
C                                       are rows long?
      J = (BUFSZ - 10 * NBPS) / (20 * TABUFF(8))
C                                       brute sort
      IF ((TABUFF(9).LT.0) .AND. (J.LE.100) .AND.
     *   (6*TABUFF(5).LT.BUFSZ)) THEN
         CALL BTBSRT (KEY, FKEY, COLTYP, IPCOL, BUFFER(1+PBUF), TABUFF,
     *      DISK, CNO, TYPE, INVER, OUTVER, CATIN, IERR)
         CALL ZMEMRY ('FREE', PNAME, NWORDS, BUFFER, PBUF, J)
      ELSE
C                                       Read and partially sort data
C                                       to scratch file.
         CALL ITBSRT (KEY, FKEY, COLTYP, IPCOL, BUFFER(1+PBUF), BUFSZ,
     *      TABUFF, ISCR, NBUFF, NSORT, IN, OUT, LENBU, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Merge data.
         IF (NUMREC.GT.NSORT) THEN
            LRECM = 2 + TABUFF(8)
            KEY1 = 1
            KEY2 = 2
            IBUFF = NBUFF
            CALL AMERGE (KEY1, KEY2, NUMREC, LRECM, NSORT, IBUFF, LUNM,
     *         SCRVOL(ISCR), SCRCNO(ISCR), LENBU, BUFFER(IN+PBUF),
     *         BUFFER(OUT+PBUF), IERR)
            IF (IERR.NE.0) THEN
               CALL ZMEMRY ('FREE', PNAME, NWORDS, BUFFER, PBUF, J)
               GO TO 999
               END IF
            END IF
C                                       Write data back to table file.
         BUFSZ = MIN (BUFSZ, 512000000)
         CALL OTBSRT (DISK, CNO, TYPE, INVER, OUTVER, BUFFER(1+PBUF),
     *      BUFSZ, TABUFF, CATIN, KEY, FKEY, ISCR, IERR)
         CALL ZMEMRY ('FREE', PNAME, NWORDS, BUFFER, PBUF, J)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1200) IERR
            GO TO 990
            END IF
         END IF
C                                       Destroy scratch files
      IF (NSCR.GT.MSCR) THEN
         MSCR = NSCR - MSCR
         CALL FILL (MSCR, 2, SCFRW)
C                                       Save CATBLK - MAPCLR may
C                                       modify.
         CALL COPY (256, CATBLK, TABUFF)
         CALL MAPCLR (MSCR, SCRVOL(SCRPNT), SCRCNO(SCRPNT), SCFRW,
     *      BUFFER)
         IF (MSCR.LE.0) NSCR = SCRPNT - 1
C                                       Restore CATBLK
         CALL COPY (256, TABUFF, CATBLK)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TABSRT: ERROR ',I3,' OPENING INPUT TABLE')
 1100 FORMAT ('TABSRT: COLUMN OUT OF RANGE KEY ',I2,' = ',2I4)
 1200 FORMAT ('TABSRT: OTBSRT ERROR ',I3)
      END
