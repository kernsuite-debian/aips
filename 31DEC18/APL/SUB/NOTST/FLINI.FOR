      SUBROUTINE FLINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   FLROW, FLCOLS, FLNUMV, NUMIF, TABREV, IRET)
C-----------------------------------------------------------------------
C! Intialize a FITS interchange flag table for reading
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2006
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
C   Verify that a temporary flag table is a valid FITS interferometry
C   data interchange file and, if so, open it for reading.
C   Inputs:
C      OPCODE      C*4        Operation code:
C                               'READ' - open for read only
C      DISK        I          Disk to use.
C      CNO         I          Catalog slot number
C      CATBLK      I(256)     Catalog header block
C      LUN         I          Logical unit number to use
C   Input/output:
C      BUFFER      I(512)     Table I/O control block and buffer
C      VER         I          FL file version (if VER = 0 when FLINI
C                               is called the the FL table with the
C                               highest version number will be
C                               initialized provided that one exists and
C                               VER will be set to its version number)
C   Output:
C      FLROW       I          Next FL table row to read (1)
C      FLCOLS      I(MAXFLC)  Column pointers in the order
C                                1  SOURCE_ID
C                                2  ARRAY
C                                3  ANTS
C                                4  FREQID
C                                5  TIMERANG
C                                6  BANDS
C                                7  CHANS
C                                8  PFLAGS
C                                9  REASON
C                               10  SEVERITY
C      FLNUMV      I(MAXFLC)  Column dimensions in the same order as
C                               FLCOLS
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
C      FL tables should be converted to AIPS FG tables when read from
C      FITS interferometry data interchange files.
C
C      NUMIF is set to the value of the NO_BAND keyword and need not
C      correspond to the number of IFs in the root data file (its value
C      may be greater than or equal to this quantity).
C
C      The table will be left open if IRET = 1 or 2 on exit but will
C      not be safe for reading using TABFL.  The state of the table
C      will be indeterminate if IRET = 3.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:PFLV.INC'
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, FLROW,
     *   FLCOLS(MAXFLC), FLNUMV(MAXFLC), NUMIF, TABREV, IRET
C
C     Local variables:
C
C     NKEY     Number of keywords to read from table
C     NCOL     Number of columns in table
C     NREC     Number of records by which to extend table (unused)
C     DATP     Table pointers and type codes
C     NFLKEY   Number of keywords to read from table (parameter)
C     FLKEYW   Keywords to be read
C     KEYLOC   Keyword pointers
C     KEYTYP   Keyword type codes
C     KEYVAL   Keyword value buffer
C     CURREV   Current FL revision number (parameter)
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
      INTEGER   NFLKEY
      PARAMETER (NFLKEY = 2)
      CHARACTER FLKEYW(NFLKEY)*8
      INTEGER   KEYLOC(NFLKEY), KEYTYP(NFLKEY), KEYVAL(2 * NFLKEY)
C
      INTEGER   CURREV
      PARAMETER (CURREV = 2)
      CHARACTER CTITLE(MAXFLC,CURREV)*24
      INTEGER   CTYPE(MAXFLC,CURREV), CDIM(MAXFLC,CURREV)
      INTEGER   XTYPE(MAXFLC), XDIM(MAXFLC), COLS(MAXFLC)
C
      INTEGER   COL, KEYW, TYPE
      INTEGER   MSGSAV
C
      INTEGER   ITRIM
      EXTERNAL  ITRIM
C
      INCLUDE 'INCS:DMSG.INC'
C                                       Keywords
      DATA FLKEYW /'TABREV  ', 'NO_BAND '/
C                                       Columns for TABREV 1
      DATA (CTITLE(COL, 1), COL = 1, MAXFLC)
     *   /'SOURCE_ID               ', 'ARRAY                   ',
     *    'ANTS                    ', 'FREQID                  ',
     *    'TIMERANG                ', 'BANDS                   ',
     *    'CHANS                   ', 'PFLAGS                  ',
     *    'REASON                  ', 'SEVERITY                '/
      DATA (CTYPE(COL, 1), COL = 1, MAXFLC)
     *   / TABINT, TABINT, TABINT, TABINT, TABFLT,
     *     TABINT, TABINT, TABINT, TABHOL, TABINT /
      DATA (CDIM(COL, 1), COL = 1, MAXFLC)
     *   / 1, 1, 2, 1, 2, 2, 2, 4, 24, 1/
C                                       Columns for TABREV 2
      DATA (CTITLE(COL, 2), COL = 1, MAXFLC)
     *   /'SOURCE_ID               ', 'ARRAY                   ',
     *    'ANTS                    ', 'FREQID                  ',
     *    'TIMERANG                ', 'BANDS                   ',
     *    'CHANS                   ', 'PFLAGS                  ',
     *    'REASON                  ', 'SEVERITY                '/
      DATA (CTYPE(COL, 2), COL = 1, MAXFLC)
     *   / TABINT, TABINT, TABINT, TABINT, TABFLT,
     *     TABINT, TABINT, TABINT, TABHOL, TABINT /
      DATA (CDIM(COL, 2), COL = 1, MAXFLC)
     *   / 1, 1, 2, 1, 2, 0, 2, 4, 40, 1/
C-----------------------------------------------------------------------
C                                       open table only read allowed
      IF (OPCODE.EQ.'READ') THEN
         NKEY = NFLKEY
         NCOL = MAXFLC
         NREC = 0
         CALL TABINI (OPCODE, 'FL', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *      NREC, NCOL, DATP, BUFFER, IRET)
         IF (IRET.EQ.0) THEN
            FLROW = 1
C           Read the TABREV and NO_BAND keywords (the others will have
C           been used to associate this table with a file but are not
C           necessary to interpret the table) while supressing messages
C           from TABKEY:
            MSGSAV = MSGSUP
            MSGSUP = 32000
            NKEY = NFLKEY
            CALL TABKEY ('READ', FLKEYW, NKEY, BUFFER, KEYLOC, KEYVAL,
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
                  WRITE (MSGTXT,9002) FLKEYW(1), 'INTEGER'
                  CALL MSGWRT (6)
                  IRET = 1
                  END IF
C                                       Check the NO_BANDS keyword
               IF (KEYTYP(2).EQ.TABINT) THEN
                  NUMIF = KEYVAL(KEYLOC(2))
                  IF (NUMIF.LT.0) THEN
                     WRITE (MSGTXT,9000) VER
                     CALL MSGWRT (6)
                     WRITE (MSGTXT,9002) FLKEYW(2), 'POSITIVE'
                     CALL MSGWRT (6)
                     IRET = 1
                  ELSE IF (NUMIF.GT.MAXIF) THEN
                     WRITE (MSGTXT,9003) VER
                     CALL MSGWRT (6)
                     WRITE (MSGTXT,9004) FLKEYW(2), NUMIF, MAXIF
                     CALL MSGWRT (6)
                     IRET = 2
                     END IF
               ELSE
                  WRITE (MSGTXT, 9000) VER
                  CALL MSGWRT (6)
                  WRITE (MSGTXT, 9002) FLKEYW(2), 'INTEGER'
                  CALL MSGWRT (6)
                  IRET = 1
                  END IF
C                                       List missing keywords:
            ELSE IF (IRET.GT.20) THEN
               WRITE (MSGTXT,9000) VER
               CALL MSGWRT (6)
               DO 10 KEYW = 1,NFLKEY
                  IF (KEYLOC(KEYW).LE.0) THEN
                     WRITE (MSGTXT,9005) FLKEYW(KEYW)
                     CALL MSGWRT (6)
                     END IF
 10               CONTINUE
               IRET = 1
            ELSE
               CALL TABERR ('READ', 'TABKEY', 'FLINI ', IRET)
               IRET = 3
               END IF
C                                       Decode and verify the column
C                                       structure of the table
            IF (IRET.EQ.0) THEN
C                                       1 <= TABREV <= CURREV
C                                       0 <= NUMIF
               CALL COPY (MAXFLC, CTYPE(1,TABREV), XTYPE)
               CALL COPY (MAXFLC, CDIM(1,TABREV), XDIM)
               IF (TABREV.EQ.2) XDIM(6) = NUMIF
               NCOL = MAXFLC
               CALL FNDCOL (NCOL, CTITLE(1, TABREV), 24, .TRUE., BUFFER,
     *            COLS, JERR)
               IF ((JERR.EQ.0) .OR. (JERR.GT.10)) THEN
                  DO 30 COL = 1,MAXFLC
                     IPOINT = COLS(COL)
                     NC = ITRIM (CTITLE(COL,TABREV))
                     IF (IPOINT.GT.0) THEN
                        FLCOLS(COL) = DATP(IPOINT,1)
                        FLNUMV(COL) = DATP(IPOINT,2) / 10
                        IF (FLNUMV(COL).LE.0) THEN
                           WRITE (MSGTXT,1100) CTITLE(COL,TABREV)(:NC)
                           CALL MSGWRT (6)
                           END IF
C                                       Check type:
                        TYPE = MOD (DATP(IPOINT,2), 10)
                        IF (TYPE.NE.XTYPE(COL)) THEN
                           WRITE (MSGTXT,9000) VER
                           CALL MSGWRT (6)
                           WRITE (MSGTXT,9020) CTITLE(COL,TABREV)(:NC)
                           CALL MSGWRT (6)
                           FLCOLS(COL) = -1
                           FLNUMV(COL) = 0
                           END IF
C                                       Check dimensions (character cols
C                                       can be longer than specs)
                        IF (TYPE.EQ.TABHOL) THEN
                           IF (FLNUMV(COL).LT.XDIM(COL)) THEN
                              WRITE (MSGTXT,9000) VER
                              CALL MSGWRT (6)
                              WRITE (MSGTXT,9021)
     *                           CTITLE(COL,TABREV)(:NC),
     *                           FLNUMV(COL), XDIM(COL)
                              CALL MSGWRT (6)
                              FLCOLS(COL) = -1
                              FLNUMV(COL) = 0
                              END IF
                        ELSE IF (FLNUMV(COL).NE.XDIM(COL)) THEN
                           WRITE (MSGTXT, 9000) VER
                           CALL MSGWRT (6)
                           WRITE (MSGTXT,9021) CTITLE(COL,TABREV)(:NC),
     *                        FLNUMV(COL), XDIM(COL)
                           CALL MSGWRT (6)
                           FLCOLS(COL) = -1
                           FLNUMV(COL) = 0
                           END IF
                     ELSE
                        WRITE (MSGTXT,1101) CTITLE(COL,TABREV)(:NC)
                        CALL MSGWRT (6)
                        FLCOLS(COL) = -1
                        FLNUMV(COL) = 0
                        END IF
 30                  CONTINUE
               ELSE
                  CALL TABERR ('N/A ', 'FNDCOL', 'FLINI ', JERR)
                  IRET = 3
               END IF
            END IF
         ELSE
            CALL TABERR (OPCODE, 'TABINI', 'FLINI', IRET)
            IRET = 3
            END IF
      ELSE
         WRITE (MSGTXT,9030) OPCODE
         IRET = 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('FLINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('FLINI: ''',A,''' COLUMN NOT FOUND')
 9000 FORMAT ('FLINI: FLAG (FL) TABLE ',I3,' IS NOT VALID')
 9001 FORMAT ('FLINI: UNRECOGNIZED REVISION NUMBER ',I3)
 9002 FORMAT ('FLINI: ',A,' KEYWORD DOES NOT HAVE ',A,' VALUE')
 9003 FORMAT ('FLINI: FLAG (FL) TABLE ',I3,'CANNOT BE USED BY AIPS')
 9004 FORMAT ('FLINI: ',A,' VALUE ',I4,' EXCEEDS LIMIT OF ',I4)
 9005 FORMAT ('FLINI: KEYWORD ',A,' IS MISSING')
 9020 FORMAT ('FLINI: COLUMN ''',A,''' HAS WRONG TYPE')
 9021 FORMAT ('FLINI: COLUMN ''',A,''' HAS DIMENSION ',I4,' NOT ',
     *        I4)
 9030 FORMAT ('FLINI: UNRECOGNIZED OPCODE ''',A,'''')
      END
