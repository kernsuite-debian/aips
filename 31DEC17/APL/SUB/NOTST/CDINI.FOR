      SUBROUTINE CDINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ICDRNO, CDKOLS, CDNUMV, NUMANT, NUMPOL, NUMIF, RDATE, IERR)
C-----------------------------------------------------------------------
C! Creates/opens/initializes CalDevice (CD) table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2010, 2013, 2017
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
C   Creates and initializes CalDevice tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                           'WRIT' = create/init for write or read
C                           'READ' = open for read only
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VER      I        CD file version
C      CATBLK   I(256)   Catalog header block.
C      LUN      I        Logical unit number to use
C   Input/output
C      NUMANT   I        Maximum antenna number
C      NUMPOL   I        Number of IFs per pair
C      NUMIF    I        Number of IF pairs
C      RDATE    C*8      'YYYYMMDD' approx date on information
C   Output:
C      ICDRNO   I        Next record number, start of the file
C                        if 'READ', the last+1 if WRITE
C      CDKOLS   I(5)     The column pointer array: antenna, subarray,
C                          freqid, Rcal1, Tcal2
C      CDNUMV   I(5)     Element count in each column.
C      IERR     I        Return error code, 0=>OK, else TABINI or TABIO
C                        error.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, RDATE*8
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, ICDRNO,
     *   CDKOLS(*), CDNUMV(*), NUMANT, NUMPOL, NUMIF, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXCDC
      PARAMETER (MAXCDC=7)
C
      HOLLERITH HOLTMP(6)
      CHARACTER TTITLE*56, TITLE(MAXCDC)*24, UNITS(MAXCDC)*8, KEYW(4)*8
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(MAXCDC), NDATA,
     *   KLOCS(4), KEYVAL(5), IPOINT, JERR, J, KEYTYP(4), I, MSGSAV, NC,
     *   ITRIM
      LOGICAL   T, DOREAD, NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T /.TRUE./
      DATA NTT /56/
      DATA TTITLE /'AIPS EVLA CalDevice TABLE '/
C                                       Values >10 are scalar
      DATA DTYP /3*14, 4*2/
C                                       NOTE: Change PCLTAB.INC when
C                                       changing the table columns.
      DATA TITLE /'ANTENNA NO.', 'SUBARRAY', 'FREQ ID', 'TCAL1',
     *   'SOLCAL1', 'TCAL2', 'SOLCAL2'/
      DATA KEYW /'NO_ANT  ', 'NO_POL  ', 'NO_IF   ', 'RDATE'/
      DATA UNITS /3*' ', 4*'KELVINS'/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 1000
      NCOL = 3 + 2 * NUMPOL
      IF (DOREAD) NCOL = 0
      NKEY = 4
      NDATA = MAXCDC
      CALL FILL (NDATA, 0, CDKOLS)
      CALL FILL (NDATA, 0, CDNUMV)
C                                       Fill in types
      IF (.NOT.DOREAD) THEN
         CALL COPY (NDATA, DTYP, DATP(1,2))
         DO 10 J = 1,NDATA
            IF (DTYP(J).LT.10) DATP(J,2) = DTYP(J) + 10 * NUMIF
 10         CONTINUE
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'CD', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CDINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      ICDRNO = BUFFER(5) + 1
      IF (DOREAD) ICDRNO = 1
      NKEY = 4
      MSGSAV = MSGSUP
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CDINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CDINI', IERR)
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
C                                       No. polarizations
         KLOCS(2) = 2
         KEYTYP(2) = 4
         KEYVAL(2) = NUMPOL
C                                       No. IFs.
         KLOCS(3) = 3
         KEYTYP(3) = 4
         KEYVAL(3) = NUMIF
C                                       date
         KLOCS(4) = 4
         KEYTYP(4) = 3
         CALL CHR2H (8, RDATE, 1, KEYVAL(4))
C                                       Only write if just created.
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'CDINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         MSGSUP = 32000
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         MSGSUP = MSGSAV
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'CDINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       No. antennas.
         IPOINT = KLOCS(1)
         IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. IFs per pair.
         IPOINT = KLOCS(2)
         IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF pairs.
         IPOINT = KLOCS(3)
         IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       date
         IPOINT = KLOCS(4)
         IF (IPOINT.GT.0) CALL H2CHR (8, 1, KEYVAL(IPOINT), RDATE)
         END IF
C                                       Get array indices
C                                       Cover your ass from FNDCOL -
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'CDINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'CD', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CDINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXCDC, TITLE, 24, T, BUFFER, CDKOLS, JERR)
C                                      Get array indices and no. values
      NCOL = 3 + 2*NUMPOL
      DO 150 I = 1,NCOL
         IPOINT = CDKOLS(I)
         IF (IPOINT.GT.0) THEN
            CDKOLS(I) = DATP(IPOINT,1)
            CDNUMV(I) = DATP(IPOINT,2) / 10
            IF ((CDNUMV(I).LE.0) .AND. (TITLE(I)(:6).NE.'SOLCAL')) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            CDKOLS(I) = -1
            CDNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            IF (TITLE(I)(:6).NE.'SOLCAL') CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('CDINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('CDINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('CDINI: ERROR INITIALIZING CAL-DEVICE TABLE FOR ',A4)
      END
