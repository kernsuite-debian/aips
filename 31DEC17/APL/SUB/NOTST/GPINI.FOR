      SUBROUTINE GPINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   GPROW, GPKOLS, GPNUMV, RCVR, RLONG, RLAT, RHT, IERR)
C-----------------------------------------------------------------------
C! Open a GPS (GP) file and initialize I/O structures
C# Ext-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1996, 1998-1999, 2006
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
C   Open a GPS (GP) table and initialize its I/O structures.
C
C   If OPCODE is 'READ' and VER is less than or equal to
C   zero then the GP table with the highest version number will be
C   opened.  If OPCODE is 'WRIT' and VER is less than or equal to zero
C   then a new table will be created.
C
C   If there are NROW rows in the table GPROW will be 1 if OPCODE is
C   'READ' or NROW+1 if OPCODE is 'WRIT'.
C   Inputs:
C      OPCODE   C*4               Operation code:
C                                    'READ' - open in read-only mode;
C                                    'WRIT' - open for reading and
C                                             writing.
C      DISK     I                 AIPS disk number of primary file.
C      CNO      I                 AIPS catalogue number of primary
C                                  file.
C      LUN      I                 AIPS logical unit number for GP table.
C   Input/output:
C      BUFFER   I(512)   TABINI/TABIO workspace. Do not modify.
C      VER      I        On input: requested GP table version number.
C                        On output: actual  GP table version number.
C      CATBLK   I(256)   Header block of primary file.
C      GPKOLS   I(*)     Column addresses.  Do not modify.
C      GPNUMV   I(*)     Column dimensions.  Do not modify.
C   Input (create) / output (pre-existing)
C      RCVR     C*8      Receiver name
C      RLONG    R        East longitude of receiver (degrees)
C      RLAT     R        Latitude of receiver (degrees)
C      RHT      R        Height of receiever above MSL (metres)
C
C   Outputs:
C      GPROW    I         Row number of next row to read or write.
C      IERR     I         Status code: 0 - no errors
C                            1 - could not initialize table
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, RCVR*8
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, GPROW,
     *   GPKOLS(*), GPNUMV(*), IERR
      REAL      RLONG, RLAT, RHT
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C                                        NUMKEY = number of keywords
      INTEGER   NUMKEY
      PARAMETER (NUMKEY = 5)
C                                        KEYWRD = keyword list
C                                        KEYTYP = keyword types
C                                        KEYLOC = keyword indices
C                                        IKVAL = integer keyword values
C                                        RKVAL = real keyword values
      CHARACTER KEYWRD(NUMKEY)*8
      INTEGER   KEYTYP(NUMKEY), KEYLOC(NUMKEY), IKVAL(2*NUMKEY)
      REAL      RKVAL(2*NUMKEY)
      DOUBLE PRECISION DKVAL
C                                       GPVER = GP table format version
      INTEGER   GPVER
      PARAMETER (GPVER = 1)
C                                       MAXGPC = number of columns in
C                                                a GP table
      INTEGER   MAXGPC
      PARAMETER (MAXGPC = 6)
C                                        CTITLE = Column titles.
      CHARACTER CTITLE(MAXGPC)*24
C                                        CUNITS = Column units
      CHARACTER CUNITS(MAXGPC)*8
C                                        CTYPE = Column type code
      INTEGER   CTYPE(MAXGPC)
C                                        CDIMEN = Column dimension
      INTEGER   CDIMEN(MAXGPC)
C                                        TTITLE = table title
      CHARACTER TTITLE*56
      PARAMETER (TTITLE = 'AIPS GPS DELAY TABLE')
C                                        Status codes:
C                                          OK = no errors detected
C                                          FAIL = error detected
C                                          CREATD = GP table created
      INTEGER   OK, FAIL, CREATD
      PARAMETER (OK = 0)
      PARAMETER (FAIL = 1)
      PARAMETER (CREATD = -1)
C
      INTEGER   I, NKEY, NCOL, DATP(128,2), IPOINT, JERR, NC, ITRIM,
     *   NREC
      HOLLERITH HOLTMP(6)
C
      EQUIVALENCE (IKVAL, RKVAL)
C
      DATA KEYWRD /'VERSION ', 'RECVR   ', 'RLONG   ', 'RLAT    ',
     *             'RHEIGHT '/
      DATA CTITLE
     *   /'TIME                    ', 'PRN                     ',
     *    'AZIMUTH                 ', 'ELEVATION               ',
     *    'TEC FROM DELAY          ', 'TEC FROM PHASE          '/
      DATA CUNITS /'DAYS    ', '        ', 'DEGREES ', 'DEGREES ',
     *             'M**-2   ', 'M**-2   '/
      DATA CTYPE /TABDBL, TABINT, TABFLT, TABFLT, TABFLT, TABFLT/
      DATA CDIMEN /1, 1, 1, 1, 1, 1/
C-----------------------------------------------------------------------
C                                       Initialize GP table:
      NKEY = NUMKEY
      NCOL = MAXGPC
C                                       Combine field dimensions and
C                                       types for DATP:
      IF (OPCODE.NE.'READ') THEN
         DO 10 I = 1,MAXGPC
            DATP(I, 2) = 10 * CDIMEN(I) + CTYPE(I)
 10         CONTINUE
         END IF
C                                       open table
      NREC = 128
      CALL TABINI (OPCODE, 'GP', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'GPINI', IERR)
         GO TO 990
         END IF
C                                       Set row number:
      IF (OPCODE.EQ.'READ') THEN
         GPROW = 1
      ELSE
         GPROW = BUFFER(5) + 1
         END IF
C                                       new file
      IF (IERR.LT.0) THEN
C                                       Write column label to table:
         DO 20 I = 1,MAXGPC
            CALL CHR2H (24, CTITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', TIOTTL, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'GPINI', IERR)
               GO TO 990
               END IF
C                                        Write column units to table:
            CALL CHR2H (8, CUNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', TIOUNT, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'GPINI', IERR)
               GO TO 990
               END IF
 20         CONTINUE
C                                        Write table title:
         CALL CHR2H (56, TTITLE, 1, BUFFER(101))
C                                        Write keywords to table:
         KEYLOC(1) = 1
         KEYTYP(1) = TABINT
         IKVAL(1)  = GPVER
         KEYLOC(2) = 2
         KEYTYP(2) = TABHOL
         CALL CHR2H (8, RCVR, 1, IKVAL(2))
         KEYLOC(3) = 4
         KEYTYP(3) = TABDBL
         DKVAL = RLONG
         CALL RCOPY (NWDPDP, DKVAL, RKVAL(4))
         I = 4 + NWDPDP
         KEYLOC(4) = I
         KEYTYP(4) = TABDBL
         DKVAL     = RLAT
         CALL RCOPY (NWDPDP, DKVAL, RKVAL(I))
         I = I + NWDPDP
         KEYLOC(5) = I
         KEYTYP(5) = TABDBL
         DKVAL     = RHT
         CALL RCOPY (NWDPDP, DKVAL, RKVAL(I))
         I = I + NWDPDP
         NKEY = NUMKEY
         CALL TABKEY ('WRIT', KEYWRD, NKEY, BUFFER, KEYLOC, IKVAL,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'GPINI', IERR)
            GO TO 990
            END IF
C                                        Read keyword/value pairs:
      ELSE
         NKEY = NUMKEY
         CALL TABKEY ('READ', KEYWRD, NKEY, BUFFER, KEYLOC, IKVAL,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'GPINI', IERR)
            GO TO 990
            END IF
C                                        Check GP table version no.:
         IF (KEYLOC(1).NE.-1) THEN
            IF (IKVAL(KEYLOC(1)).GT.GPVER) THEN
C                                        Must be from a later version
C                                        of AIPS.
               WRITE (MSGTXT,1020) IKVAL(KEYLOC(1))
               GO TO 980
C                                       Can not cope with old versions
C                                       of the table format:
            ELSE IF (IKVAL(KEYLOC(1)).LT.GPVER) THEN
               WRITE (MSGTXT,1021)
               GO TO 980
               END IF
         ELSE
            MSGTXT = 'GPINI: CORRUPT GP TABLE - NO REVISION NUMBER'
            GO TO 980
            END IF
C                                        Read receiver name:
         IF (KEYLOC(2).NE.-1) THEN
            CALL H2CHR (8, 1, IKVAL(KEYLOC(2)), RCVR)
         ELSE
            MSGTXT = 'GPINI: CORRUPT GP TABLE - NO RCVR KEYWORD'
            GO TO 980
            END IF
C                                        Read longitude:
         IF (KEYLOC(3).NE.-1) THEN
            IF (KEYTYP(3).EQ.TABDBL) THEN
               CALL RCOPY (NWDPDP, RKVAL(KEYLOC(3)), DKVAL)
               RLONG = DKVAL
            ELSE
               RLONG = RKVAL(KEYLOC(3))
               END IF
         ELSE
            MSGTXT = 'GPINI: CORRUPT GP TABLE - NO RLONG KEYWORD'
            GO TO 980
            END IF
C                                        Read latitude:
         IF (KEYLOC(4).NE.-1) THEN
            IF (KEYTYP(4).EQ.TABDBL) THEN
               CALL RCOPY (NWDPDP, RKVAL(KEYLOC(4)), DKVAL)
               RLAT = DKVAL
            ELSE
               RLAT = RKVAL(KEYLOC(4))
               END IF
         ELSE
            MSGTXT = 'GPINI: CORRUPT GP TABLE - NO RLAT KEYWORD'
            GO TO 980
            END IF
C                                        Read height
         IF (KEYLOC(5).NE.-1) THEN
            IF (KEYTYP(5).EQ.TABDBL) THEN
               CALL RCOPY (NWDPDP, RKVAL(KEYLOC(5)), DKVAL)
               RHT = DKVAL
            ELSE
               RHT = RKVAL(KEYLOC(5))
               END IF
         ELSE
            MSGTXT = 'GPINI: CORRUPT GP TABLE - NO RHT KEYWORD'
            GO TO 980
            END IF
         END IF
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, IKVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'GPINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'GP', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'GPINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXGPC, CTITLE, 24, .TRUE., BUFFER, GPKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,MAXGPC
         IPOINT = GPKOLS(I)
         IF (IPOINT.GT.0) THEN
            GPKOLS(I) = DATP(IPOINT,1)
            GPNUMV(I) = DATP(IPOINT,2) / 10
            IF (GPNUMV(I).LE.0) THEN
               NC = ITRIM (CTITLE(I))
               WRITE (MSGTXT,1100) CTITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            GPKOLS(I) = -1
            GPNUMV(I) = 0
            NC = ITRIM (CTITLE(I))
            WRITE (MSGTXT,1101) CTITLE(I)(:NC)
            CALL MSGWRT (6)
            END IF
 150     CONTINUE
C                                        Get array indices:
      DO 30 I = 1, NCOL
         GPKOLS(I) = DATP(I, 1)
         GPNUMV(I) = DATP(I, 2) / 10
 30      CONTINUE
      IERR = 0
      GO TO 999
C
 980  CALL MSGWRT (7)
      IERR = 1
C
C                                      Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('GPINI: CANNOT HANDLE GP TABLE REVISION ', I2)
 1021 FORMAT ('GPINI: OBSOLETE GP TABLE REVISION - RERUN LDGPS')
 1100 FORMAT ('GPINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('GPINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('GPINI: ERROR INITIALIZING GPS TABLE FOR ',A4)
      END
