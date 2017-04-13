      SUBROUTINE PCINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   PCROW, PCKOLS, PCNUMV, NUMPOL, NUMIF, NUMTON, IERR)
C-----------------------------------------------------------------------
C! creates and intialize AIPS phase-cal tables
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2006-2007, 2016
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
C   Open an AIPS phase-cal (PC) table for reading or writing.
C
C   Inputs:
C      OPCODE     C*4       Operation code:
C                              'WRIT' - open table for reading and
C                                       writing; create table if it
C                                       does not already exist
C                              'READ' - open table for reading only
C      DISK       I         AIPS disk number of root file
C      CNO        I         Catalogue number of root file
C      LUN        I         AIPS LUN to use for table
C
C   Input/Output:
C      BUFFER     I(512)    I/O control block and I/O buffer for PC
C                              table
C      VER        I         Version number of PC table.  If 0 on input
C                              then create a new table if OPCODE is
C                              'WRIT' or open the highest numbered PC
C                              table if OPCODE is 'WRIT'; in either case
C                              VER should be set to the actual version
C                              number of the table that is opened.
C      CATBLK     I(256)    Catalogue header of the root data file
C      NUMPOL     I         Number of polarizations present in table
C                              (1 or 2)
C      NUMIF      I         Number of IFs in table (should lie in the
C                              range 0 to MAXIF)
C      NUMTON     I         Number of tones in table (should lie in the
C                              range 0 to MAXTON)
C
C   Outputs:
C      PCROW      I         Next row to read (1 if OPCODE = 'READ',
C                              one greater than the number of rows
C                              in the table if OPCODE =' WRIT')
C      PCKOLS     I(MAXPCC) Column pointers in the order
C                              1   TIME
C                              2   TIME_INTERVAL
C                              3   SOURCE_ID
C                              4   ANTENNA_NO
C                              5   ARRAY
C                              6   FREQID
C                              7   CABLE_CAL
C                              8   STATE 1
C                              9   PC_FREQ 1
C                             10   PC_REAL 1
C                             11   PC_IMAG 1
C                             12   PC_RATE 1
C                             13   STATE 2
C                             14   PC_FREQ 2
C                             15   PC_REAL 2
C                             16   PC_IMAG 2
C                             17   PC_RATE 2
C      PCNUMV     I(MAXPCC) Column dimensions in the same order as
C                              PCKOLS
C      IERR       I         Status report:
C                              0   table opened
C                              1   unknown format revision
C                              2   I/O error detected
C                            999   Subroutine called incorrectly
C
C   Notes:
C      The table may be open or closed if IERR is not zero on exit.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:PPCV.INC'
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, PCROW,
     *          PCKOLS(MAXPCC), PCNUMV(MAXPCC), NUMPOL, NUMIF, NUMTON,
     *          IERR
C
C     Local variables
C
C     NKEY      Number of keywords
C     NREC      Minimum number of records by which to expand table
C     NCOL      Number of columns in table
C     DATP      Array of data pointers and type codes
C     NPCKEY    Number of mandatory PC table keywords
C     PCKEYW    List of mandatory PC table keywords
C     KEYLOC    Pointers to keyword values
C     KEYVAL    Buffer for keyword values
C     KEYTYP    Keyword type codes
C     TTITLE    Table title
C     CTITLE    List of column titles
C     CUNITS    List of column units
C     CTYPE     List of column types
C     CDIM      List of column dimensions
C     COLS      Column index
C     COL       Column number
C     HOLTMP    Hollerith buffer
C
      INTEGER   NPCKEY
      PARAMETER (NPCKEY = 4)
      CHARACTER PCKEYW(NPCKEY)*8
      INTEGER   NKEY, NREC, NCOL, DATP(128,2), KEYLOC(NPCKEY), COL,
     *   KEYVAL(NPCKEY), KEYTYP(NPCKEY), CTYPE(MAXPCC), CDIM(MAXPCC),
     *   IPOINT, NC, ITRIM, I
      CHARACTER TTITLE*56
      PARAMETER (TTITLE = 'AIPS PC TABLE ')
      CHARACTER CTITLE(MAXPCC)*24, CUNITS(MAXPCC)*8
      HOLLERITH HOLTMP(6)
      INCLUDE 'INCS:DMSG.INC'
      DATA PCKEYW / 'TABREV  ', 'NO_POL  ', 'NO_BAND ', 'NO_TONES' /
      DATA CTITLE /
     *   'TIME                    ', 'TIME_INTERVAL           ',
     *   'SOURCE_ID               ', 'ANTENNA_NO              ',
     *   'ARRAY                   ', 'FREQID                  ',
     *   'CABLE_CAL               ', 'STATE 1                 ',
     *   'PC_FREQ 1               ', 'PC_REAL 1               ',
     *   'PC_IMAG 1               ', 'PC_RATE 1               ',
     *   'STATE 2                 ', 'PC_FREQ 2               ',
     *   'PC_REAL 2               ', 'PC_IMAG 2               ',
     *   'PC_RATE 2               ' /
      DATA CUNITS / 'DAYS    ', 'DAYS    ', '        ', '        ',
     *              '        ', '        ', 'SECONDS ', 'PERCENT ',
     *              'HZ      ', '        ', '        ', 'SEC/SEC ',
     *              'PERCENT ', 'HZ      ', '        ', '        ',
     *              'SEC/SEC ' /
      DATA CTYPE / TABDBL, TABFLT, TABINT, TABINT, TABINT,
     *             TABINT, TABDBL, TABFLT, TABDBL, TABFLT,
     *             TABFLT, TABFLT, TABFLT, TABDBL, TABFLT,
     *             TABFLT, TABFLT /
      DATA CDIM / 1, 1, 1, 1, 1, 1, 1, 0, 0, 0,
     *            0, 0, 0, 0, 0, 0, 0 /
C-----------------------------------------------------------------------
      NKEY = NPCKEY
      NREC = 30
      IF (OPCODE.EQ.'WRIT') THEN
         CALL CHECK ('PCINI ', 1, ((NUMPOL.EQ.1) .OR. (NUMPOL.EQ.2)),
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL CHECK ('PCINI ', 2, ((NUMIF.GE.0) .AND. (NUMIF.LE.MAXIF)),
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL CHECK ('PCINI ', 3,
     *      ((NUMTON.GE.0) .AND. (NUMTON.LE.MAXTON)), IERR)
         IF (IERR.NE.0) GO TO 999
         NCOL = 7 + NUMPOL * 5
         DO 10 COL = 8,NCOL
            IF ((COL.EQ.8) .OR. (COL.EQ.13)) THEN
               CDIM(COL) = 4 * NUMIF
            ELSE
               CDIM(COL) = NUMTON * NUMIF
               END IF
 10         CONTINUE
         DO 20 COL = 1,NCOL
            DATP(COL,2) = CTYPE(COL) + 10 * CDIM(COL)
 20         CONTINUE
         END IF
C
      CALL TABINI (OPCODE, 'PC', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
C                                       A new table has been created.
      IF (IERR.LT.0) THEN
         PCROW = 1
C                                       Fill in the table title:
         CALL CHR2H (LEN(TTITLE), TTITLE, 1, BUFFER(101))
C                                       Set table revision number:
         KEYTYP(1) = TABINT
         KEYLOC(1) = 1
         KEYVAL(KEYLOC(1)) = PCREV
C                                       Set number of polarizations:
         KEYTYP(2) = TABINT
         KEYLOC(2) = 2
         KEYVAL(KEYLOC(2)) = NUMPOL
C                                       Set number of IFs:
         KEYTYP(3) = TABINT
         KEYLOC(3) = 3
         KEYVAL(KEYLOC(3)) = NUMIF
C                                       Set number of TONES:
         KEYTYP(4) = TABINT
         KEYLOC(4) = 4
         KEYVAL(KEYLOC(4)) = NUMTON
         NKEY = NPCKEY
         CALL TABKEY ('WRIT', PCKEYW, NKEY, BUFFER, KEYLOC, KEYVAL,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'PCINI', IERR)
            GO TO 990
            END IF
C                                       Write column headings:
         DO 30 COL = 1,NCOL
            CALL CHR2H (24, CTITLE(COL), 1, HOLTMP)
            CALL TABIO ('WRIT', TIOTTL, COL, HOLTMP, BUFFER, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('WRIT', 'TABIO ', 'PCINI ', IERR)
               GO TO 990
               END IF
            CALL CHR2H (8, CUNITS(COL), 1, HOLTMP)
            CALL TABIO ('WRIT', TIOUNT, COL, HOLTMP, BUFFER, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('WRIT', 'TABIO ', 'PCINI ', IERR)
               GO TO 990
               END IF
 30         CONTINUE
C                                       pre-existing file
      ELSE
C                                       read keywords:
         NKEY = NPCKEY
         CALL TABKEY ('READ', PCKEYW, NKEY, BUFFER, KEYLOC, KEYVAL,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'PCINI', IERR)
            GO TO 990
            END IF
C                                       Keywords are assumed to be
C                                       valid.
         NUMPOL = KEYVAL(KEYLOC(2))
         NUMIF  = KEYVAL(KEYLOC(3))
         NUMTON = KEYVAL(KEYLOC(4))
         IF (KEYVAL(KEYLOC(1)).ne.PCREV) THEN
            WRITE (MSGTXT,1030) KEYVAL(KEYLOC(1)), PCREV
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Flush modified table description
C                                       to disk by closing and reopening
      CALL TABIO ('CLOS', 0, COL, HOLTMP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'PCINI', IERR)
         GO TO 990
         END IF
      CALL TABINI (OPCODE, 'PC', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'PCINI ', IERR)
         GO TO 990
         END IF
C                                       Set row pointer:
      IF (OPCODE.EQ.'READ') THEN
         PCROW = 1
      ELSE
         PCROW = BUFFER(5) + 1
         END IF
C                                       column pointers
      NCOL = 7 + NUMPOL * 5
      CALL FNDCOL (NCOL, CTITLE, 24, .TRUE., BUFFER, PCKOLS, IERR)
      DO 150 I = 1,NCOL
         IPOINT = PCKOLS(I)
         IF (IPOINT.GT.0) THEN
            PCKOLS(I) = DATP(IPOINT,1)
            PCNUMV(I) = DATP(IPOINT,2) / 10
            IF (PCNUMV(I).LE.0) THEN
               NC = ITRIM (CTITLE(I))
               WRITE (MSGTXT,1100) CTITLE(I)(:NC)
               IF ((I.LE.8) .OR. (I.EQ.13) .OR. (NUMTON.GT.0))
     *            CALL MSGWRT (6)
               END IF
         ELSE
            PCKOLS(I) = -1
            PCNUMV(I) = 0
            NC = ITRIM (CTITLE(I))
            WRITE (MSGTXT,1101) CTITLE(I)(:NC)
            CALL MSGWRT (6)
            END IF
 150     CONTINUE
      IERR = 0
      GO TO 999
C                                      Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('WARNING: PC TABLE HAS REVISION NO. ',I3,' EXPECTED ',I3)
 1100 FORMAT ('PCINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('PCINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('PCINI: ERROR INITIALIZING PHASE-CAL TABLE FOR ',A4)
      END
