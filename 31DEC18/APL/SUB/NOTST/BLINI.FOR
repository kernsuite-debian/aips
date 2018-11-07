      SUBROUTINE BLINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IBLRNO, BLKOLS, BLNUMV, NUMANT, NUMPOL, NUMIF, IERR)
C-----------------------------------------------------------------------
C! Create/open/init I/O to BL table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2006, 2009
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
C   Creates and initializes baseline correction (BL) extension tables.
C    Inputs:
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open.
C     DISK         I   Disk to use.
C     CNO          I   Catalog slot number
C     VER          I   BL file version
C     CATBLK(256)  I   Catalog header block.
C     LUN          I   Logical unit number to use
C    Input (create) / output (pre-existing)
C     NUMANT       I    Number of antennas
C     NUMPOL       I    Number of polarizations.
C     NUMIF        I    Number of IFs
C    Output:
C     IBLRNO       I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     BLKOLS(MAXBLC) I   The column pointer array in order, TIME, SOURID,
C                      SUBARRAY, ANTENNA1, ANTENNA2, FREQ. ID,
C                      REALM1, IMAGM1, REALA1, IMAGA1,
C                      Following used if 2 polarizations per IF
C                      REALM2, IMAGM2, REALA2, IMAGA2.
C     BLNUMV(MAXBLC) I   Element count in each column.
C     IERR         I   Return error code, 0=>OK, else TABINI or TABIO
C                      error.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, IBLRNO,
     *   BLKOLS(*), BLNUMV(*), NUMANT, NUMPOL, NUMIF, IERR
c
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH HOLTMP(6)
      CHARACTER KEYW(3)*8,  TTITLE*56, TITLE(MAXBLC)*24, UNITS(MAXBLC)*8
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(MAXBLC), NDATA,
     *   KLOCS(3), KEYVAL(3), ITRIM, KEYTYP(3), IPOINT, J, I, NC, JERR
      LOGICAL   DOREAD, NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NTT /56/
      DATA TTITLE /'AIPS UV DATA FILE BASELINE TABLE              '/
      DATA NDATA /MAXBLC/
      DATA DTYP /12,5*14,8*2/
      DATA TITLE /'TIME                    ',
     *   'SOURCE ID               ', 'SUBARRAY                ',
     *   'ANTENNA1                ', 'ANTENNA2                ',
     *   'FREQ ID                 ',
     *   'REAL M1                 ', 'IMAG M1                 ',
     *   'REAL A1                 ', 'IMAG A1                 ',
     *   'REAL M2                 ', 'IMAG M2                 ',
     *   'REAL A2                 ', 'IMAG A2                 '/
      DATA KEYW /'NO_ANT  ', 'NO_POL  ', 'NO_IF   '/
      DATA UNITS /'DAYS    ', 13*'        '/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      NREC = 500
      NKEY = 3
C                                       Fill in types
      IF (.NOT.DOREAD) THEN
         NCOL = 6 + NUMPOL * 4
         CALL COPY (NDATA, DTYP, DATP(1,2))
         DO 10 J = 7,NDATA
            DATP(J,2) = DTYP(J) + 10 * NUMIF
 10         CONTINUE
      ELSE
         NCOL = 0
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'BL', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'BLINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
      NKEY = 3
C                                       Get number of scans
      IBLRNO = BUFFER(5) + 1
      IF (DOREAD) IBLRNO = 1
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'BLINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'BLINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
C                                       No. antennas.
         KLOCS(1) = 1
         KEYTYP(1) = 4
         KEYVAL(1) = NUMANT
C                                       No. Polarizations
         KLOCS(2) = 2
         KEYTYP(2) = 4
         KEYVAL(2) = NUMPOL
C                                       No. IFs
         KLOCS(3) = 3
         KEYTYP(3) = 4
         KEYVAL(3) = NUMIF
C                                       Only write if just created.
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'BLINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'BLINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       No. antennas.
         IPOINT = KLOCS(1)
         IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. polarizations
         IPOINT = KLOCS(2)
         IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IFs
         IPOINT = KLOCS(3)
         IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
         END IF
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'BLINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'BL', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'BLINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (NDATA, TITLE, 24, .TRUE., BUFFER, BLKOLS, JERR)
C                                       Get array indices
      NCOL = 6 + NUMPOL * 4
      DO 150 I = 1,NCOL
         IPOINT = BLKOLS(I)
         IF (IPOINT.GT.0) THEN
            BLKOLS(I) = DATP(IPOINT,1)
            BLNUMV(I) = DATP(IPOINT,2) / 10
            IF (BLNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            BLKOLS(I) = -1
            BLNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('BLINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('BLINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('BLINI: ERROR INITIALIZING BASELINE TABLE FOR ',A4)
      END
