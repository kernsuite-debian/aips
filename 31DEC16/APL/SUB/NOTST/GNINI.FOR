      SUBROUTINE GNINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   GNROW, GNCOLS, GNNUMV, NUMPOL, NUMIF, NTABS, TABREV, IRET)
C-----------------------------------------------------------------------
C! Intialize a FITS interchange gain curve table for reading
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2006-2007
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
C   Verify that a temporary gain curve table is a valid FITS
C   interferometry data interchange table and, if so, open it for
C   reading.
C   Inputs:
C      OPCODE      C*4        Operation code:
C                               'READ' - open for read only
C      DISK        I          Disk to use.
C      CNO         I          Catalog slot number
C      CATBLK      I(256)     Catalog header block
C      LUN         I          Logical unit number to use
C   Input/output:
C      BUFFER      I(512)     Table I/O control block and buffer
C      VER         I          GN file version (if VER = 0 when GNINI
C                               is called the the GN table with the
C                               highest version number will be
C                               initialized provided that one exists and
C                               VER will be set to its version number)
C   Output:
C      GNROW       I          Next GN table row to read (1)
C      GNCOLS      I(MAXGNC)  Column pointers in the order
C                                1  ANTENNA_NO
C                                2  ARRAY
C                                3  FREQID
C                                4  TYPE_1
C                                5  NTERM_1
C                                6  X_TYP_1
C                                7  Y_TYP_1
C                                8  X_VAL_1
C                                9  Y_VAL_1
C                               10  GAIN_1
C                               11  SENS_1
C                               12  TYPE_2
C                               13  NTERM_2
C                               14  X_TYP_2
C                               15  Y_TYP_2
C                               16  X_VAL_1
C                               17  Y_VAL_1
C                               18  GAIN_2
C                               19  SENS_2
C      GNNUMV      I(MAXGNC)  Column dimensions in the same order as
C                               GNCOLS
C      NUMIF       I          Number of IFs covered by the table
C      TABREV      I          Table revision number
C      IRET        I          Return code:
C                               0 -> table open
C                               1 -> table format is not valid
C                               2 -> table format is valid but exceeds
C                                    an AIPS limit
C                               3 -> an I/O error was detected
C                             999 -> subroutine call was invalid
C   Notes:
C      GC tables should be converted to AIPS GC tables when read from
C      FITS interferometry data interchange files.
C
C      NUMIF is set to the value of the NO_BAND keyword and need not
C      correspond to the number of IFs in the root data file (its value
C      may be greater than or equal to this quantity).
C
C      The table will be left open if IRET = 1 or 2 on exit but will
C      not be safe for reading using TABGN.  The state of the table
C      will be indeterminate if IRET = 3.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:PGNV.INC'
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, GNROW,
     *   GNCOLS(MAXGNC), GNNUMV(MAXGNC), NUMPOL, NUMIF, NTABS,
     *   TABREV, IRET
C
C     Local variables:
C
C     NKEY     Number of keywords to read from table
C     NCOL     Number of columns in table
C     NREC     Number of records by which to extend table (unused)
C     DATP     Table pointers and type codes
C     NGNKEY   Number of keywords to read from table (parameter)
C     GNKEYW   Keywords to be read
C     KEYLOC   Keyword pointers
C     KEYTYP   Keyword type codes
C     KEYVAL   Keyword value buffer
C     CURREV   Current GN revision number (parameter)
C     CTITLE   Column titles (indexed by column and table revision)
C     CTYPE    Column types(indexed by column and table revision)
C     CDIM     Column dimensions (indexed by column and table revision)
C     XTYPE    Expected column type codes for current table (indexed
C                by column)
C     XDIM     Expected column dimensions for current table (indexed by
C                column)
C     COLS     Column pointers
C     COL      Column number
C     KEYW     Keyword number
C     TYPE     Type of current column
C     MSGSAV   Saved message suppression level
C
      INTEGER   NKEY, NCOL, NREC, DATP(128,2)
C
      INTEGER   NGNKEY
      PARAMETER (NGNKEY = 4)
      CHARACTER GNKEYW(NGNKEY)*8
      INTEGER   KEYLOC(NGNKEY), KEYTYP(NGNKEY), KEYVAL(2 * NGNKEY)
C
      INTEGER   CURREV
      PARAMETER (CURREV = 1)
      CHARACTER CTITLE(MAXGNC, CURREV)*24
      INTEGER   CTYPE(MAXGNC, CURREV), CDIM(MAXGNC, CURREV)
      INTEGER   XTYPE(MAXGNC), XDIM(MAXGNC), COLS(MAXGNC)
C
      INTEGER   COL, KEYW, TYPE
      INTEGER   MSGSAV
C
      INTEGER   ITRIM, NC, JERR, IPOINT
      EXTERNAL  ITRIM
C
      INCLUDE 'INCS:DMSG.INC'
C                                       Keywords
      DATA GNKEYW /'TABREV  ', 'NO_POL  ', 'NO_BAND ', 'NO_TABS '/
C                                       Columns for TABREV 1
      DATA (CTITLE(COL, 1), COL = 1, MAXGNC)
     *   /'ANTENNA_NO              ', 'ARRAY                   ',
     *    'FREQID                  ', 'TYPE_1                  ',
     *    'NTERM_1                 ', 'X_TYP_1                 ',
     *    'Y_TYP_1                 ', 'X_VAL_1                 ',
     *    'Y_VAL_1                 ', 'GAIN_1                  ',
     *    'SENS_1                  ', 'TYPE_2                  ',
     *    'NTERM_2                 ', 'X_TYP_2                 ',
     *    'Y_TYP_2                 ', 'X_VAL_2                 ',
     *    'Y_VAL_2                 ', 'GAIN_2                  ',
     *    'SENS_2                  '/
      DATA (CTYPE(COL, 1), COL = 1, MAXGNC)
     *   / TABINT, TABINT, TABINT, TABINT, TABINT,
     *     TABINT, TABINT, TABFLT, TABFLT, TABFLT,
     *     TABFLT, TABINT, TABINT, TABINT, TABINT,
     *     TABFLT, TABFLT, TABFLT, TABFLT /
      DATA (CDIM(COL, 1), COL = 1, MAXGNC)
     *   / 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
     *     0, 0, 0, 0, 0, 0, 0, 0, 0 /
C-----------------------------------------------------------------------
C                                       open table - only READ allowed
      IF (OPCODE.EQ.'READ') THEN
         NKEY = NGNKEY
         NCOL = MAXGNC
         NREC = 0
         CALL TABINI (OPCODE, 'GN', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *      NREC, NCOL, DATP, BUFFER, IRET)
         IF (IRET.EQ.0) THEN
            GNROW = 1
C           Read the TABREV and NO_BAND keywords (the others will have
C           been used to associate this table with a file but are not
C           necessary to interpret the table) while supressing messages
C           from TABKEY:
            MSGSAV = MSGSUP
            MSGSUP = 32000
            NKEY = NGNKEY
            CALL TABKEY ('READ', GNKEYW, NKEY, BUFFER, KEYLOC, KEYVAL,
     *         KEYTYP, IRET)
            MSGSUP = MSGSAV
C                                       Check the revision number:
            IF (IRET.EQ.0) THEN
               IF (KEYTYP(1).EQ.TABINT) THEN
                  TABREV = KEYVAL(KEYLOC(1))
                  IF ((TABREV.LT.1) .OR. (TABREV.GT.CURREV)) THEN
                     WRITE (MSGTXT,9000) VER
                     CALL MSGWRT (6)
                     WRITE (MSGTXT,9001) TABREV
                     CALL MSGWRT (6)
                     IRET = 1
                     END IF
               ELSE
                  WRITE (MSGTXT,9000) VER
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,9002) GNKEYW(1), 'INTEGER'
                  CALL MSGWRT (6)
                  IRET = 1
                  END IF
C                                       Check the NO_POL keyword
               IF (KEYTYP(2).EQ.TABINT) THEN
                  NUMPOL = KEYVAL(KEYLOC(2))
                  IF ((NUMPOL.LT.1) .OR. (NUMPOL.GT.2)) THEN
                     WRITE (MSGTXT,9000) VER
                     CALL MSGWRT (6)
                     WRITE (MSGTXT,9002) GNKEYW(2), 'VALID'
                     CALL MSGWRT (6)
                     IRET = 1
                     END IF
               ELSE
                  WRITE (MSGTXT,9000) VER
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,9002) GNKEYW(2), 'INTEGER'
                  CALL MSGWRT (6)
                  IRET = 1
                  END IF
C                                       Check the NO_BAND keyword
               IF (KEYTYP(3).EQ.TABINT) THEN
                  NUMIF = KEYVAL(KEYLOC(3))
                  IF (NUMIF.LT.0) THEN
                     WRITE (MSGTXT,9000) VER
                     CALL MSGWRT (6)
                     WRITE (MSGTXT,9002) GNKEYW(3), 'POSITIVE'
                     CALL MSGWRT (6)
                     IRET = 1
                  ELSE IF (NUMIF.GT.MAXIF) THEN
                     WRITE (MSGTXT,9003) VER
                     CALL MSGWRT (6)
                     WRITE (MSGTXT,9004) GNKEYW(3), NUMIF, MAXIF
                     CALL MSGWRT (6)
                     IRET = 2
                     END IF
               ELSE
                  WRITE (MSGTXT,9000) VER
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,9002) GNKEYW(3), 'INTEGER'
                  CALL MSGWRT (6)
                  IRET = 1
                  END IF
C                                       Check the NO_TABS keyword
               IF (KEYTYP(4).EQ.TABINT) THEN
                  NTABS = KEYVAL(KEYLOC(4))
                  IF (NTABS.LT.0) THEN
                     WRITE (MSGTXT,9000) VER
                     CALL MSGWRT (6)
                     WRITE (MSGTXT,9002) GNKEYW(4), 'POSITIVE'
                     CALL MSGWRT (6)
                     IRET = 1
                  ELSE IF (NTABS.GT.MXTBGN) THEN
                     WRITE (MSGTXT,9003) VER
                     CALL MSGWRT (6)
                     WRITE (MSGTXT,9004) GNKEYW(4), NTABS, MXTBGN
                     CALL MSGWRT (6)
                     IRET = 2
                     END IF
               ELSE
                  WRITE (MSGTXT,9000) VER
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,9002) GNKEYW(4), 'INTEGER'
                  CALL MSGWRT (6)
                  IRET = 1
                  END IF
C                                       List missing keywords:
            ELSE IF (IRET.GT.20) THEN
               WRITE (MSGTXT,9000) VER
               CALL MSGWRT (6)
               DO 10 KEYW = 1,NGNKEY
                  IF (KEYLOC(KEYW) .LE. 0) THEN
                     WRITE (MSGTXT,9005) GNKEYW(KEYW)
                     CALL MSGWRT (6)
                     END IF
 10               CONTINUE
               IRET = 1
            ELSE
               CALL TABERR ('READ', 'TABKEY', 'GNINI ', IRET)
               IRET = 3
               END IF
C                                       Decode and verify the column
C                                       structure of the table
            IF (IRET.EQ.0) THEN
C                                       1 <= TABREV <= CURREV
C                                       0 <= NUMIF <= MAXIF
C                                       1 <= NUMPOL <= 2
C                                       0 <= NTABS <= MXTBGN
               CALL COPY (MAXGNC, CTYPE(1, TABREV), XTYPE)
               CALL COPY (MAXGNC, CDIM(1, TABREV), XDIM)
               NCOL = 3 + 8 * NUMPOL
               DO 20 COL = 1,NCOL
                  IF (((COL.GE.4) .AND. (COL.LE.8)) .OR. (COL.EQ.19)
     *                .OR. ((COL.GE.11) .AND. (COL.LE.16))) THEN
                     XDIM(COL) = NUMIF
                  ELSE IF ((COL.EQ.9) .OR. (COL.EQ.10) .OR. (COL.EQ.17)
     *               .OR. (COL.EQ.18)) THEN
                     XDIM(COL) = NTABS * NUMIF
                     END IF
 20               CONTINUE
               CALL FNDCOL (NCOL, CTITLE(1,TABREV), 24, .TRUE., BUFFER,
     *            COLS, JERR)
               IF ((JERR.EQ.0) .OR. (JERR.GT.10)) THEN
                  DO 40 COL = 1,NCOL
                     IPOINT = COLS(COL)
                     NC = ITRIM (CTITLE(COL,TABREV))
                     IF (IPOINT.GT.0) THEN
                        GNCOLS(COL) = DATP(IPOINT,1)
                        GNNUMV(COL) = DATP(IPOINT,2) / 10
                        IF (GNNUMV(COL).LE.0) THEN
                           WRITE (MSGTXT,1100) CTITLE(COL,TABREV)(:NC)
                           CALL MSGWRT (6)
                           END IF
                        TYPE = MOD (DATP(IPOINT,2), 10)
                        IF (TYPE.NE.XTYPE(COL)) THEN
                           WRITE (MSGTXT,9000) VER
                           CALL MSGWRT (6)
                           WRITE (MSGTXT,9030) CTITLE(COL, TABREV)(:NC)
                           CALL MSGWRT (6)
                           GNCOLS(COL) = -1
                           GNNUMV(COL) = 0
                           END IF
C                                       Check dimensions:
                        IF (GNNUMV(COL).NE.XDIM(COL)) THEN
                           WRITE (MSGTXT,9000) VER
                           CALL MSGWRT (6)
                           WRITE (MSGTXT,9031) CTITLE(COL, TABREV)(:NC),
     *                        GNNUMV(COL), XDIM(COL)
                           CALL MSGWRT (6)
                           GNCOLS(COL) = -1
                           GNNUMV(COL) = 0
                           END IF
                     ELSE
                        WRITE (MSGTXT,1101) CTITLE(COL,TABREV)(:NC)
                        CALL MSGWRT (6)
                        GNCOLS(COL) = -1
                        GNNUMV(COL) = 0
                        END IF
 40                  CONTINUE
               ELSE
                  CALL TABERR ('FIND', 'FNDCOL', 'GNINI ', IRET)
                  IRET = 3
                  END IF
               END IF
         ELSE
            CALL TABERR (OPCODE, 'TABINI', 'GNINI ', IRET)
            IRET = 3
            END IF
      ELSE
         WRITE (MSGTXT,9040) OPCODE
         IRET = 999
         END IF
c
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('GNINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('GNINI: ''',A,''' COLUMN NOT FOUND')
 9000 FORMAT ('GNINI: GAIN CURVE (GN) TABLE ', I3, ' IS NOT VALID')
 9001 FORMAT ('GNINI: UNRECOGNIZED REVISION NUMBER ', I3)
 9002 FORMAT ('GNINI: ', A, ' KEYWORD DOES NOT HAVE ', A, ' VALUE')
 9003 FORMAT ('GNINI: GAIN CURVE (GN) TABLE ', I3,
     *        'CANNOT BE USED BY AIPS')
 9004 FORMAT ('GNINI: ', A, ' VALUE ', I4, ' EXCEEDS LIMIT OF ', I4)
 9005 FORMAT ('GNINI: KEYWORD ', A, ' IS MISSING')
 9030 FORMAT ('GNINI: COLUMN ''', A, ''' HAS WRONG TYPE')
 9031 FORMAT ('GNINI: COLUMN ''', A, ''' HAS DIMENSION ', I4, ' NOT ',
     *        I4)
 9040 FORMAT ('GNINI: UNRECOGNIZED OPCODE ''', A, '''')
      END
