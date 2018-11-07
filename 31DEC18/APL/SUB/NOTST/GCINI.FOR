      SUBROUTINE GCINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IGCRNO, GCKOLS, GCNUMV, NPOLGC, NUMIF, NTABGC, IERR)
C-----------------------------------------------------------------------
C! creates and intializes a Gain Curve (GC) table
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2006-2007, 2010
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
C   Creates and initializes Gain curve tables.
C   Inputs:
C      OPCODE         C*4  Operation code:
C                          'WRIT' = create/init for write or read
C                          'READ' = open for read only
C      BUFFER(512)    I    I/O buffer and related storage, also defines
C                          file if open.
C      DISK           I    Disk to use.
C      CNO            I    Catalog slot number
C      CATBLK(256)    I    Catalog header block.
C      LUN            I    Logical unit number to use
C   Input/Output:
C      VER            I    GC table version
C   Input (create) / output (pre-existing)
C      NPOLGC         I    # polzns in the table
C      NUMIF          I    The number of bands (IF's) in the data.
C      NTABGC         I    # tabulated values per row
C   Output:
C      IGCRNO         I    Next row number, start of the file if READ,
C                          the last+1 if WRITE
C      GCKOLS(MAXGCC) I    The column pointer array in order:
C                             ANTENNA NO, SUBARRAY, FREQ ID,
C                             CURVE TYPE 1, NO ENTRIES 1, X TYPE 1,
C                             Y TYPE 1, X VALUE 1, Y VALUES 1,
C                             GAIN VALUES 1, SENSITIVITY 1
C                          the following are only present if NPOLGC = 2
C                             CURVE TYPE 2, NO ENTRIES 2, X TYPE 2,
C                             Y TYPE 2, X VALUE 2, Y VALUES 2,
C                             GAIN VALUES 2, SENSITIVITY 2,
C      GCNUMV(MAXGCC) I    Element count in each column.
C      IERR           I    Return error code, 0=>OK, else TABINI or
C                          TABIO error.
C----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PGCV.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, IGCRNO,
     *   GCKOLS(MAXGCC), GCNUMV(MAXGCC), NPOLGC, NTABGC, NUMIF, IERR
C
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C                                       Local variables
      LOGICAL   T, DOREAD, NEWFIL
      INTEGER   NUMKEY
      PARAMETER (NUMKEY = 4)
      CHARACTER LTGC(MAXGCC)*24, LTTGC*56, LUNTGC(MAXGCC)*8,
     *   LKEYGC(NUMKEY)*8
      INTEGER  DTYP(MAXGCC), DATP(128,2), NREC, NCOL, NKEY, I, ITEMP(6),
     *   JERR, NDATA, IPOINT, NTTGC, ITRIM, NC
      INTEGER  KEYVAL(2 * NUMKEY), KLOCS(NUMKEY), KEYTYP(NUMKEY)
      HOLLERITH HOLTMP(6)
      EQUIVALENCE (HOLTMP, ITEMP)
C                                       Data statements
      DATA LTGC /'ANTENNA_NO              ',
     *   'SUBARRAY                ', 'FREQ ID                 ',
     *   'TYPE_1                  ', 'NTERM_1                 ',
     *   'X_TYP_1                 ', 'Y_TYP_1                 ',
     *   'X_VAL_1                 ', 'Y_VAL_1                 ',
     *   'GAIN_1                  ', 'SENS_1                  ',
     *   'TYPE_2                  ', 'NTERM_2                 ',
     *   'X_TYP_2                 ', 'Y_TYP_2                 ',
     *   'X_VAL_2                 ', 'Y_VAL_2                 ',
     *   'GAIN_2                  ', 'SENS_2                  '/
      DATA LTTGC
     *   /'GAIN CURVE TABLE '/
      DATA LUNTGC /7*'        ', 2*'DEGREES ', '        ',
     *   'K/JY    ', 4*'        ', 2*'DEGREES ', '        ',
     *   'K/JY    '/
      DATA LKEYGC /'NO_BAND ', 'NO_POL', 'NO_TABS', 'TABREV' /
      DATA NTTGC /56/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Set up needed variables
      NREC = 30
      NCOL = 3 + 8 * MIN (2, NPOLGC)
      IF (DOREAD) NCOL = 0
      NKEY = NUMKEY
      NDATA = MAXGCC
      CALL FILL (NDATA, 0, GCKOLS)
      CALL FILL (NDATA, 0, GCNUMV)
      IF (.NOT.DOREAD) THEN
C                                       Check no. of entries requested
         IF (NTABGC.GT.MXTBGC) THEN
            IERR = 1
            WRITE (MSGTXT,1000) NTABGC
            CALL MSGWRT (7)
            GO TO 990
            END IF
C                                       Fill in types, lengths
C                                       See Going AIPS Vol 2 p13-3.
         DTYP(GCIANT) = TABINT + 10
         DTYP(GCISUB) = TABINT + 10
         DTYP(GCIFQD) = TABINT + 10
         DTYP(GCITP1) = TABINT + 10 * NUMIF
         DTYP(GCINT1) = TABINT + 10 * NUMIF
         DTYP(GCIXT1) = TABINT + 10 * NUMIF
         DTYP(GCIYT1) = TABINT + 10 * NUMIF
         DTYP(GCRXV1) = TABFLT + 10 * NUMIF
         DTYP(GCRYV1) = TABFLT + 10 * NUMIF * NTABGC
         DTYP(GCRGA1) = TABFLT + 10 * NUMIF * NTABGC
         DTYP(GCRSE1) = TABFLT + 10 * NUMIF
         DTYP(GCITP2) = TABINT + 10 * NUMIF
         DTYP(GCINT2) = TABINT + 10 * NUMIF
         DTYP(GCIXT2) = TABINT + 10 * NUMIF
         DTYP(GCIYT2) = TABINT + 10 * NUMIF
         DTYP(GCRXV2) = TABFLT + 10 * NUMIF
         DTYP(GCRYV2) = TABFLT + 10 * NUMIF * NTABGC
         DTYP(GCRGA2) = TABFLT + 10 * NUMIF * NTABGC
         DTYP(GCRSE2) = TABFLT + 10 * NUMIF
         CALL COPY (NCOL, DTYP, DATP(1,2))
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'GC', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'GCINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      IGCRNO = BUFFER(5) + 1
      IF (DOREAD) IGCRNO = 1
      NKEY = NUMKEY
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, LTGC(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'GCINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, LUNTGC(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'GCINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTTGC, LTTGC, 1, BUFFER(101))
C                                       Write keywords if table created
         KEYTYP(1) = TABINT
         KLOCS(1)  = 1
         KEYVAL(1) = NUMIF
         KEYTYP(2) = TABINT
         KLOCS(2)  = 2
         KEYVAL(2) = MIN (NPOLGC, 2)
         KEYTYP(3) = TABINT
         KLOCS(3)  = 3
         KEYVAL(3) = NTABGC
         KEYTYP(4) = TABINT
         KLOCS(4)  = 4
         KEYVAL(4) = IGCREV
         NKEY = NUMKEY
         CALL TABKEY ('WRIT', LKEYGC, NKEY, BUFFER, KLOCS, KEYVAL,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1).AND.(IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'GCINI ', IERR)
            GO TO 990
            END IF
      ELSE
C                                       Read keywords
         NKEY = NUMKEY
         CALL TABKEY ('READ', LKEYGC, NKEY, BUFFER, KLOCS, KEYVAL,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1).AND.(IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'GCINI ', IERR)
            GO TO 990
            END IF
         IF (KLOCS(1) .GT. 0) THEN
            NUMIF = KEYVAL(KLOCS(1))
            END IF
         IF (KLOCS(2) .GT. 0) THEN
            NPOLGC = KEYVAL(KLOCS(2))
            NPOLGC = MIN (2, NPOLGC)
            END IF
         IF (KLOCS(3) .GT. 0) THEN
            NTABGC = KEYVAL(KLOCS(3))
            END IF
         END IF
C                                       Get array indices
C                                       Prevent problems with FNDCOL -
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, 0, BUFFER, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'GCINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
C                                       Re-open
      CALL TABINI (OPCODE, 'GC', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'GCINI', IERR)
         GO TO 990
         END IF
C
      CALL FNDCOL (NDATA, LTGC, 24, T, BUFFER, GCKOLS, JERR)
C                                       Get array indices and no. values
      NCOL = 3 + 8 * MIN (2, NPOLGC)
      DO 150 I = 1,NDATA
         IPOINT = GCKOLS(I)
         IF (IPOINT.GT.0) THEN
            GCKOLS(I) = DATP(IPOINT,1)
            GCNUMV(I) = DATP(IPOINT,2) / 10
            IF (GCNUMV(I).LE.0) THEN
               NC = ITRIM (LTGC(I))
               WRITE (MSGTXT,1100) LTGC(I)(:NC)
               IF (I.LE.NCOL) CALL MSGWRT (6)
               END IF
         ELSE
            GCKOLS(I) = -1
            GCNUMV(I) = 0
            NC = ITRIM (LTGC(I))
            WRITE (MSGTXT,1101) LTGC(I)(:NC)
            IF (I.LE.NCOL) CALL MSGWRT (6)
            END IF
 150     CONTINUE
C
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GCINI: NTABGC=',I6,' EXCEEDS MAXIMUM ALLOWED')
 1100 FORMAT ('GCINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('GCINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('GCINI: ERROR INITIALIZING GAIN CURVE TABLE FOR ',A4)
      END
