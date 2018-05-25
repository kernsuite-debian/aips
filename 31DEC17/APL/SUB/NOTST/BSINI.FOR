      SUBROUTINE BSINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   BSRNO, BSCOLS, BSNUMV, MODE, NUMIF, IERR)
C-----------------------------------------------------------------------
C! Open a baseline solution (BS) file and initialize I/O structures.
C# Ext-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2006
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
C   Open a baseline solution (BS) table and initialize its I/O
C   structures.  If OPCODE is 'READ' and VER is less than or equal to
C   zero then the BS table with the highest version number will be
C   opened.  If OPCODE is 'WRIT' and VER is less than or equal to zero
C   then a new table will be created.
C
C   If there are NROW rows in the table BSRNO will be 1 if OPCODE is
C   'READ' or NROW+1 if OPCODE is 'WRIT'.
C   Inputs:
C      OPCODE   C*4               Operation code:
C                                    'READ' - open in read-only mode;
C                                    'WRIT' - open for reading and
C                                             writing.
C      DISK     I                 AIPS disk number of primary file.
C      CNO      I                 AIPS catalogue number of primary
C                                  file.
C      LUN      I                 AIPS logical unit number for BS table.
C
C   Input/output:
C      BUFFER   I(512)   TABINI/TABIO workspace. Do not modify.
C      VER      I        On input: requested BS table version number.
C                        On output: actual  BS table version number.
C      CATBLK   I(256)   Header block of primary file.
C      BSCOLS   I(*)     Column addresses.  Do not modify.
C      BSNUMV   I(*)     Column dimensions.  Do not modify.
C      MODE     C*4      Solution mode used to derive the values in the
C                        table
C                        'INDE' - independent delays for each IF
C                        'VLBA' - one delay for all IFs
C                        'MK3 ' - multiband and single-band delays
C                        'RATE' - rate only
C      NUMIF    I         Number of IFs for which narrow-band delays are
C                         present.
C   Outputs:
C      BSRNO    I         Row number of next row to read or write.
C      IERR     I         Status code: 0 - no errors
C                            1 - could not initialize table
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, MODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, BSRNO,
     *   BSCOLS(*), BSNUMV(*), NUMIF, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
C                                        NUMKEY = number of keywords
      INTEGER   NUMKEY
      PARAMETER (NUMKEY = 3)
C                                        KEYWRD = keyword list
C                                        KEYTYP = keyword types
C                                        KEYLOC = keyword indices
C                                        IKVAL = integer keyword values
C                                        LKVAL = logical keyword values
      CHARACTER KEYWRD(NUMKEY)*8
      INTEGER   KEYTYP(NUMKEY), KEYLOC(NUMKEY), IKVAL(NUMKEY+1)
C                                       Need an extra work in IKVAL
C                                       since MODE requires 2 words.
C
C                                        BSVER = BS table version
      INTEGER   BSVER
      PARAMETER (BSVER = 4)
C                                        CTITLE = Column titles.
      CHARACTER CTITLE(MAXBSC)*24
C                                        CUNITS = Column units
      CHARACTER CUNITS(MAXBSC)*8
C                                        CTYPE = Column type code
      INTEGER   CTYPE(MAXBSC)
C                                        CDIMEN = Column dimension
      INTEGER   CDIMEN(MAXBSC)
C                                        TTITLE = table title
      CHARACTER TTITLE*56
      PARAMETER (TTITLE = 'AIPS BASELINE FRINGE SOLUTION TABLE')
C                                       Mode padded to 8 characters
      CHARACTER PMODE*8
C
      INTEGER   I, NKEY, NCOL, DATP(128,2), NC, ITRIM, IPOINT, NREC
      HOLLERITH HOLTMP(6)
C
      DATA KEYWRD /'VERSION ', 'NO_IF   ', 'MODE    '/
      DATA CTITLE
     *   /'TIME                    ', 'TIME INTERVAL           ',
     *    'BASELINE                ', 'SUBARRAY                ',
     *    'STOKES                  ', 'SOURCE                  ',
     *    'VECTOR AMPLITUDE        ', 'SCALAR AMPLITUDE        ',
     *    'RESIDUAL MB DELAY       ', 'MB DELAY ERROR          ',
     *    'MB DELAY AMBIGUITY      ',
     *    'RESIDUAL SB DELAY       ', 'SB DELAY ERROR          ',
     *    'SB DELAY AMBIGUITY      ',
     *    'RESIDUAL FRINGE RATE    ', 'FRINGE RATE ERROR       ',
     *    'FRINGE RATE AMBIGUITY   ',
     *    'RESIDUAL ACCELERATION   ', 'ACCELERATION ERROR      ',
     *    'RESIDUAL PHASE          ', 'PHASE ERROR             '/
      DATA CUNITS /'DAYS    ', 'DAYS    ', '        ', '        ',
     *             '        ', '        ', 'JANSKYS ', 'JANSKYS ',
     *             'SECONDS ', 'SECONDS ', 'SECONDS ', 'SECONDS ',
     *             'SECONDS ', 'SECONDS ', 'HZ      ', 'HZ      ',
     *             'HZ      ', 'HZ/SEC  ', 'HZ/SEC  ', 'DEGREES ',
     *             'DEGREES '/
      DATA CTYPE /TABDBL, TABFLT, TABINT, TABINT, TABINT, TABINT,
     *            TABFLT, TABFLT, TABFLT, TABFLT, TABFLT, TABFLT,
     *            TABFLT, TABFLT, TABFLT, TABFLT, TABFLT, TABFLT,
     *            TABFLT, TABFLT, TABFLT/
      DATA CDIMEN /1, 1, 2, 1, 1, 1, 0, 0, 1, 1,
     *             1, 0, 0, 1, 0, 0, 1, 0, 0, 0,
     *             0/
C-----------------------------------------------------------------------
C                                       Set the number of IFs to unity
C                                       for 'VLBA', 'MK3 ' and 'RATE'
C                                       modes to minimize wasted space
C                                       in the table.
      IF ((OPCODE.EQ.'WRIT') .AND. ((MODE.EQ.'VLBA') .OR.
     *   (MODE.EQ.'RATE') .OR.(MODE.EQ.'MK3 '))) NUMIF = 1
C                                       Initialize BS table:
      NKEY = NUMKEY
      NCOL = MAXBSC
      PMODE = MODE
C                                       Dimension fields by the number
C                                       of IFs where appropriate:
      IF (OPCODE.NE.'READ') THEN
         DO 10 I = 1,MAXBSC
            IF (CDIMEN(I).GT.0) THEN
               DATP(I, 2) = 10 * CDIMEN(I) + CTYPE(I)
            ELSE
               DATP(I, 2) = 10 * NUMIF + CTYPE(I)
               END IF
 10         CONTINUE
         END IF
      NREC = 128
      CALL TABINI (OPCODE, 'BS', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'BSINI', IERR)
         GO TO 990
C                                       new file
      ELSE IF (IERR.EQ.-1) THEN
         DO 20 I = 1,MAXBSC
C                                        Write column label to table:
            CALL CHR2H (24, CTITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', TIOTTL, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'BSINI', IERR)
               GO TO 990
               END IF
C                                        Write column units to table:
            CALL CHR2H (8, CUNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', TIOUNT, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'BSINI', IERR)
               GO TO 990
               END IF
 20         CONTINUE
C                                        Write table title:
         CALL CHR2H (56, TTITLE, 1, BUFFER(101))
C                                        Write keywords to table:
         KEYLOC(1) = 1
         KEYTYP(1) = TABINT
         IKVAL(1)  = BSVER
         KEYLOC(2) = 2
         KEYTYP(2) = TABINT
         IKVAL(2)  = NUMIF
         KEYLOC(3) = 3
         KEYTYP(3) = TABHOL
         CALL CHR2H (8, PMODE, 1, IKVAL(3))
         NKEY = NUMKEY
         CALL TABKEY ('WRIT', KEYWRD, NKEY, BUFFER, KEYLOC, IKVAL,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'BSINI', IERR)
            GO TO 990
            END IF
C                                        Read keyword/value pairs:
      ELSE
         NKEY = NUMKEY
         CALL TABKEY ('READ', KEYWRD, NKEY, BUFFER, KEYLOC, IKVAL,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'BSINI', IERR)
            GO TO 990
            END IF
C                                        Check BS table version no.:
         IF (KEYLOC(1).NE.-1) THEN
C                                        Must be from a later version
C                                        of AIPS.
            IF (IKVAL(KEYLOC(1)).GT.BSVER) THEN
               IERR = 1
               WRITE (MSGTXT,1020) IKVAL(KEYLOC(1))
               CALL MSGWRT (7)
               GO TO 990
               END IF
C                                       Can not cope with old versions
C                                       of the table format:
            IF (IKVAL(KEYLOC(1)).LT.BSVER) THEN
               IERR = 1
               WRITE (MSGTXT, 1021) 1
               CALL MSGWRT (7)
               GO TO 990
               END IF
         ELSE
            IERR = 1
            MSGTXT = 'BSINI: CORRUPT BS TABLE - NO REVISION NUMBER'
            CALL MSGWRT (7)
            GO TO 990
            END IF
C                                        Read number of IFs:
         IF (KEYLOC(2).NE.-1) THEN
            NUMIF = IKVAL(KEYLOC(2))
         ELSE
            IERR = 1
            MSGTXT = 'BSINI: CORRUPT BS TABLE - NO NO_IF KEYWORD'
            CALL MSGWRT (7)
            GO TO 990
            END IF
C                                        Read MODE keyword:
         IF (KEYLOC(3).NE.-1) THEN
            CALL H2CHR (8, 1, IKVAL(3), PMODE)
            MODE = PMODE(1:4)
         ELSE
            IERR = 1
            MSGTXT = 'BSINI: CORRUPT BS TABLE - NO MODE KEYWORD'
            CALL MSGWRT (7)
            GO TO 990
            END IF
C                                        Check keyword consistency:
         IF (((MODE.EQ.'VLBA') .OR. (MODE.EQ.'RATE') .OR.
     *      (MODE.EQ.'MK3 ')) .AND. (NUMIF.NE.1)) THEN
            IERR = 1
            WRITE (MSGTXT,1025) NUMIF, MODE
            CALL MSGWRT (7)
            GO TO 990
            END IF
         END IF
C                                        Set BSRNO:
      IF (OPCODE.EQ.'READ') THEN
         BSRNO = 1
      ELSE
         BSRNO = BUFFER(5) + 1
         END IF
C                                        Get array indices:
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, IKVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'BSINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'BS', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'BSINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXBSC, CTITLE, 24, .TRUE., BUFFER, BSCOLS, I)
C                                      Get array indices and no. values
      DO 150 I = 1,MAXBSC
         IPOINT = BSCOLS(I)
         IF (IPOINT.GT.0) THEN
            BSCOLS(I) = DATP(IPOINT,1)
            BSNUMV(I) = DATP(IPOINT,2) / 10
            IF (BSNUMV(I).LE.0) THEN
               NC = ITRIM (CTITLE(I))
               WRITE (MSGTXT,1100) CTITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            BSCOLS(I) = -1
            BSNUMV(I) = 0
            NC = ITRIM (CTITLE(I))
            WRITE (MSGTXT,1101) CTITLE(I)(:NC)
            CALL MSGWRT (6)
            END IF
 150     CONTINUE
      IERR = 0
      GO TO 999
C
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
  999 RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('BSINI: CAN NOT INITIALIZE BS TABLE')
 1020 FORMAT ('BSINI: CAN NOT HANDLE BS TABLE REVISION ', I2)
 1021 FORMAT ('BSINI: OBSOLETE BS TABLE REVISION - RERUN BLING')
 1025 FORMAT ('BSINI: ', I2, ' IFS INCONSISTENT WITH MODE ''', A4, '''')
 1100 FORMAT ('BSINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('BSINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('BSINI: ERROR INITIALIZING BASELINE SOLUTION TABLE FOR ',
     *   A4)
      END
