      SUBROUTINE PHINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   PHROW, PHCOLS, PHNUMV, NUMPOL, NUMIF, NUMTON, TABREV, IRET)
C-----------------------------------------------------------------------
C! Intialize a FITS interchange phase-cal table for reading
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2006, 2015
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
C   Verify that a temporary phase-cal (PH) table is a valid FITS
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
C      VER         I          PH file version (if VER = 0 when PHINI
C                               is called the the PH table with the
C                               highest version number will be
C                               initialized provided that one exists and
C                               VER will be set to its version number)
C   Output:
C      PHROW       I          Next PH table row to read (1)
C      PHCOLS      I(MAXPHC)  Column pointers in the order
C                                1  TIME
C                                2  TIME_INTERVAL
C                                3  SOURCE_ID
C                                4  ANTENNA_NO
C                                5  ARRAY
C                                6  FREQID
C                                7  CABLE_CAL
C                                8  STATE_1
C                                9  PC_FREQ_1
C                               10  PC_REAL_1
C                               11  PC_IMAG_1
C                               12  PC_RATE_1
C                               13  STATE_2
C                               14  PC_FREQ_2
C                               15  PC_REAL_2
C                               16  PC_IMAG_2
C                               17  PC_RATE_2
C      PHNUMV      I(MAXPHC)  Column dimensions in the same order as
C                               PHCOLS
C      NUMPOL      I          Number of polarizations in table
C      NUMIF       I          Number of IFs covered by the table
C      NUMTON      I          Number of phase-cal tones in table
C      TABREV      I          Table revision number
C      IRET        I          Return code:
C                               0 -> table open
C                               1 -> table format is not valid
C                               2 -> table format is valid but exceeds
C                                    an AIPS limit
C                               3 -> an I/O error was detected
C                             999 -> subroutine call was invalid
C   Notes:
C      PH tables should be converted to AIPS PC tables when read from
C      FITS interferometry data interchange files.
C
C      NUMIF is set to the value of the NO_BAND keyword and need not
C      correspond to the number of IFs in the root data file (its value
C      may be greater than or equal to this quantity).
C
C      The table will be left open if IRET = 1 or 2 on exit but will
C      not be safe for reading using TABPH.  The state of the table
C      will be indeterminate if IRET = 3.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:PPHV.INC'
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, PHROW,
     *   PHCOLS(MAXPHC), PHNUMV(MAXPHC), NUMPOL, NUMIF, NUMTON,
     *   TABREV, IRET
C
C     Local variables:
C
C     NKEY     Number of keywords to read from table
C     NCOL     Number of columns in table
C     NREC     Number of records by which to extend table (unused)
C     DATP     Table pointers and type codes
C     NPHKEY   Number of keywords to read from table (parameter)
C     PHKEYW   Keywords to be read
C     KEYLOC   Keyword pointers
C     KEYTYP   Keyword type codes
C     KEYVAL   Keyword value buffer
C     CURREV   Current PH revision number (parameter)
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
      INTEGER   NKEY, NCOL, NREC, DATP(128,2), JERR, NC, IPOINT
C
      INTEGER   NPHKEY
      PARAMETER (NPHKEY = 4)
      CHARACTER PHKEYW(NPHKEY)*8
      INTEGER   KEYLOC(NPHKEY), KEYTYP(NPHKEY), KEYVAL(2 * NPHKEY)
C
      INTEGER   CURREV
      PARAMETER (CURREV = 2)
      CHARACTER CTITLE(MAXPHC, CURREV)*24
      INTEGER   CTYPE(MAXPHC, CURREV), CDIM(MAXPHC, CURREV)
      INTEGER   XTYPE(MAXPHC), XDIM(MAXPHC), COLS(MAXPHC)
C
      INTEGER   COL, KEYW, TYPE
      INTEGER   MSGSAV
C
      INTEGER   ITRIM
      EXTERNAL  ITRIM
C
      INCLUDE 'INCS:DMSG.INC'
C                                       Keywords
      DATA PHKEYW /'TABREV', 'NO_POL', 'NO_BAND', 'NO_TONES'/
C                                       Columns for TABREV 1
      DATA (CTITLE(COL, 1), COL = 1, MAXPHC)
     *   /'TIME                    ', 'TIME_INTERVAL           ',
     *    'SOURCE_ID               ', 'ANTENNA_NO              ',
     *    'ARRAY                   ', 'FREQID                  ',
     *    'CABLE_CAL               ', 'STATE_1                 ',
     *    'PC_FREQ 1               ', 'PC_REAL 1               ',
     *    'PC_IMAG 1               ', 'PC_RATE 1               ',
     *    'STATE_2                 ', 'PC_FREQ 2               ',
     *    'PC_REAL 2               ', 'PC_IMAG 2               ',
     *    'PC_RATE 2               ' /
      DATA (CTYPE(COL, 1), COL = 1, MAXPHC)
     *   / TABDBL, TABFLT, TABINT, TABINT, TABINT,
     *     TABINT, TABDBL, TABFLT, TABDBL, TABFLT,
     *     TABFLT, TABFLT, TABFLT, TABDBL, TABFLT,
     *     TABFLT, TABFLT /
      DATA (CDIM(COL, 1), COL = 1, MAXPHC)
     *   / 1, 1, 1, 1, 1, 1, 1, 0, 0, 0,
     *     0, 0, 0, 0, 0, 0, 0 /
C                                       Columns for TABREV 2
      DATA (CTITLE(COL, 2), COL = 1, MAXPHC)
     *   /'TIME                    ', 'TIME_INTERVAL           ',
     *    'SOURCE_ID               ', 'ANTENNA_NO              ',
     *    'ARRAY                   ', 'FREQID                  ',
     *    'CABLE_CAL               ', 'STATE_1                 ',
     *    'PC_FREQ_1               ', 'PC_REAL_1               ',
     *    'PC_IMAG_1               ', 'PC_RATE_1               ',
     *    'STATE_2                 ', 'PC_FREQ_2               ',
     *    'PC_REAL_2               ', 'PC_IMAG_2               ',
     *    'PC_RATE_2               ' /
      DATA (CTYPE(COL, 2), COL = 1, MAXPHC)
     *   / TABDBL, TABFLT, TABINT, TABINT, TABINT,
     *     TABINT, TABDBL, TABFLT, TABDBL, TABFLT,
     *     TABFLT, TABFLT, TABFLT, TABDBL, TABFLT,
     *     TABFLT, TABFLT /
      DATA (CDIM(COL, 2), COL = 1, MAXPHC)
     *   / 1, 1, 1, 1, 1, 1, 1, 0, 0, 0,
     *     0, 0, 0, 0, 0, 0, 0 /
C-----------------------------------------------------------------------
C                                       open table only read allowed
      IF (OPCODE .EQ. 'READ') THEN
         NKEY = NPHKEY
         NCOL = MAXPHC
         NREC = 0
         CALL TABINI (OPCODE, 'PH', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *      NREC, NCOL, DATP, BUFFER, IRET)
         IF (IRET.EQ.0) THEN
            PHROW = 1
C           Read the mandatory keywords while supressing messages
C           from TABKEY:
            MSGSAV = MSGSUP
            MSGSUP = 32000
            NKEY = NPHKEY
            CALL TABKEY ('READ', PHKEYW, NKEY, BUFFER, KEYLOC, KEYVAL,
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
                  WRITE (MSGTXT,9002) PHKEYW(1), 'INTEGER'
                  CALL MSGWRT (6)
                  IRET = 1
                  END IF
C                                       Check the NO_POL keyword
               IF (KEYTYP(2).EQ.TABINT) THEN
                  NUMPOL = KEYVAL(KEYLOC(2))
                  IF ((NUMPOL.LT.1) .OR. (NUMPOL.GT.2)) THEN
                     WRITE (MSGTXT,9000) VER
                     CALL MSGWRT (6)
                     WRITE (MSGTXT,9002) PHKEYW(2), 'VALID'
                     CALL MSGWRT (6)
                     IRET = 1
                     END IF
               ELSE
                  WRITE (MSGTXT,9000) VER
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,9002) PHKEYW(2), 'INTEGER'
                  CALL MSGWRT (6)
                  IRET = 1
                  END IF
C                                       Check the NO_BAND keyword
               IF (KEYTYP(3).EQ.TABINT) THEN
                  NUMIF = KEYVAL(KEYLOC(3))
                  IF (NUMIF.LT.0) THEN
                     WRITE (MSGTXT,9000) VER
                     CALL MSGWRT (6)
                     WRITE (MSGTXT,9002) PHKEYW(3), 'POSITIVE'
                     CALL MSGWRT (6)
                     IRET = 1
                  ELSE IF (NUMIF.GT.MAXIF) THEN
                     WRITE (MSGTXT,9003) VER
                     CALL MSGWRT (6)
                     WRITE (MSGTXT,9004) PHKEYW(3), NUMIF, MAXIF
                     CALL MSGWRT (6)
                     IRET = 2
                     END IF
               ELSE
                  WRITE (MSGTXT,9000) VER
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,9002) PHKEYW(3), 'INTEGER'
                  CALL MSGWRT (6)
                  IRET = 1
                  END IF
C                                       Check the NO_TONES keyword
               IF (KEYTYP(4).EQ.TABINT) THEN
                  NUMTON = KEYVAL(KEYLOC(4))
                  IF (NUMTON.LT.0) THEN
                     WRITE (MSGTXT,9000) VER
                     CALL MSGWRT (6)
                     WRITE (MSGTXT,9002) PHKEYW(4), 'POSITIVE'
                     CALL MSGWRT (6)
                     IRET = 1
                  ELSE IF (NUMTON.GT.MAXTON) THEN
                     WRITE (MSGTXT,9003) VER
                     CALL MSGWRT (6)
                     WRITE (MSGTXT,9004) PHKEYW(4), NUMTON, MAXTON
                     CALL MSGWRT (6)
                     IRET = 2
                     END IF
               ELSE
                  WRITE (MSGTXT,9000) VER
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,9002) PHKEYW(4), 'INTEGER'
                  CALL MSGWRT (6)
                  IRET = 1
                  END IF
C                                       List missing keywords:
            ELSE IF (IRET.GT.20) THEN
               WRITE (MSGTXT,9000) VER
               CALL MSGWRT (6)
               DO 10 KEYW = 1,NPHKEY
                  IF (KEYLOC(KEYW).LE.0) THEN
                     WRITE (MSGTXT,9005) PHKEYW(KEYW)
                     CALL MSGWRT (6)
                     END IF
 10               CONTINUE
               IRET = 1
            ELSE
               CALL TABERR ('READ', 'TABKEY', 'PHINI ', IRET)
               IRET = 3
               END IF
C                                       Decode and verify the column
C                                       structure of the table
            IF (IRET.EQ.0) THEN
C                                       1 <= TABREV <= CURREV
C                                       0 <= NUMIF <= MAXIF
C                                       1 <= NUMPOL <= 2
C                                       0 <= NUMTON <= MAXTON
               CALL COPY (MAXPHC, CTYPE(1, TABREV), XTYPE)
               CALL COPY (MAXPHC, CDIM(1, TABREV), XDIM)
               NCOL = 7 + 5 * NUMPOL
               DO 20 COL = 8,NCOL
                  IF ((COL.EQ.8) .OR. (COL.EQ.13)) THEN
                     XDIM(COL) = 4 * NUMIF
                  ELSE
                     XDIM(COL) = NUMTON * NUMIF
                     END IF
   20             CONTINUE
               CALL FNDCOL (NCOL, CTITLE(1, TABREV), 24, .TRUE., BUFFER,
     *            COLS, JERR)
               IF ((JERR.EQ.0) .OR. (JERR.GT.10)) THEN
                  DO 40 COL = 1,NCOL
                     IPOINT = COLS(COL)
                     NC = ITRIM (CTITLE(COL,TABREV))
                     IF (IPOINT.GT.0) THEN
                        PHCOLS(COL) = DATP(IPOINT,1)
                        PHNUMV(COL) = DATP(IPOINT,2) / 10
                        IF (PHNUMV(COL).LE.0) THEN
                           WRITE (MSGTXT,1100) CTITLE(COL,TABREV)(:NC)
                           CALL MSGWRT (6)
                           END IF
C                                       Check type:
                        TYPE = MOD (DATP(IPOINT,2), 10)
                        IF (TYPE.NE.XTYPE(COL)) THEN
                           WRITE (MSGTXT,9000) VER
                           CALL MSGWRT (6)
                           WRITE (MSGTXT,9030) CTITLE(COL,TABREV)(:NC)
                           CALL MSGWRT (6)
                           PHCOLS(COL) = -1
                           PHNUMV(COL) = 0
                           END IF
C                                       Check dimensions:
                        IF (PHNUMV(COL).NE.XDIM(COL)) THEN
                           WRITE (MSGTXT,9000) VER
                           CALL MSGWRT (6)
                           WRITE (MSGTXT,9031) CTITLE(COL,TABREV)(:NC),
     *                        PHNUMV(COL), XDIM(COL)
                           CALL MSGWRT (6)
                           PHCOLS(COL) = -1
                           PHNUMV(COL) = 0
                           END IF
                     ELSE
                        WRITE (MSGTXT,1101) CTITLE(COL,TABREV)(:NC)
                        CALL MSGWRT (6)
                        PHCOLS(COL) = -1
                        PHNUMV(COL) = 0
                        END IF
   40                CONTINUE
               ELSE
                  CALL TABERR ('FIND', 'FNDCOL', 'PHINI ', IRET)
                  IRET = 3
                  END IF
               END IF
         ELSE
            CALL TABERR (OPCODE, 'TABINI', 'PHINI ', IRET)
            IRET = 3
            END IF
      ELSE
         WRITE (MSGTXT,9040) OPCODE
         IRET = 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('PHINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('PHINI: ''',A,''' COLUMN NOT FOUND')
 9000 FORMAT ('PHINI: PHASE-CAL (PH) TABLE ', I3, ' IS NOT VALID')
 9001 FORMAT ('PHINI: UNRECOGNIZED REVISION NUMBER ', I3)
 9002 FORMAT ('PHINI: ', A, ' KEYWORD DOES NOT HAVE ', A, ' VALUE')
 9003 FORMAT ('PHINI: PHASE-CAL (PH) TABLE ', I3,
     *        'CAN NOT BE USED BY AIPS')
 9004 FORMAT ('PHINI: ', A, ' VALUE ', I4, ' EXCEEDS LIMIT OF ', I4)
 9005 FORMAT ('PHINI: KEYWORD ', A, ' IS MISSING')
 9030 FORMAT ('PHINI: COLUMN ''', A, ''' HAS WRONG TYPE')
 9031 FORMAT ('PHINI: COLUMN ''', A, ''' HAS DIMENSION ', I4, ' NOT ',
     *        I4)
 9040 FORMAT ('PHINI: UNRECOGNIZED OPCODE ''', A, '''')
      END
