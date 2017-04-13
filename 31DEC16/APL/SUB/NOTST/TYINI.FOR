      SUBROUTINE TYINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ITYRNO, TYKOLS, TYNUMV, NUMPOL, NUMIF, IERR)
C-----------------------------------------------------------------------
C! Create/open/initialize Tsys (TY) table
C# EXT-appl UV Calibration
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
C   Creates and initializes T sys (TY) extension tables.
C    Inputs:
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open.
C     DISK         I   Disk to use.
C     CNO          I   Catalog slot number
C     VER          I   TY file version
C     CATBLK(256)  I   Catalog header block.
C     LUN          I   Logical unit number to use
C     NUMPOL       I   Number of polarizations (1 or 2)
C     NUMIF        I   Number of IFs for which data is stored in file
C    Output:
C     ITYRNO       I   Next record number, start of the file if 'READ',
C                      the last+1 if WRITE
C     TYKOLS(*)    I   The column pointer array in order defined in
C                      PTYTAB.INC.
C     TYNUMV(*)    I   Element count in each column
C     IERR         I   Return error code, 0=>OK, else TABINI or TABIO
C                      error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PTYTAB.INC'
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, ITYRNO,
     *   TYKOLS(MAXTYC), TYNUMV(MAXTYC), NUMPOL, NUMIF, IERR
C
      CHARACTER TTITLE*56, TITLE(TYRTA2)*24, UNITS(TYRTA2)*8,
     *   KEYWRD(3)*8
      HOLLERITH HOLTMP(6)
      INTEGER    NKEY, NREC, DATP(128,2), NCOL, KLOCS(3), KEYVAL(3),
     *   KEYTYP(3), NTT, NDATA, REVNO, IPOINT, MSGSAV, DTYP(TYRTA2), J,
     *   I, JERR, NC, ITRIM
      LOGICAL    DOREAD, NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NTT /56/
      DATA TTITLE /'AIPS UV DATA FILE TSYS TABLE                  '/
      DATA KEYWRD /'NO_POL', 'NO_IF','REVISION'/
C                                        Values larger than 10 are
C                                        scalars
      DATA DTYP /12,12,14,14,14,14,4*2/
C                                       NOTE: change PTYTAB.INC if the
C                                       columns are changed.
      DATA TITLE /'TIME                    ',
     *   'TIME INTERVAL          ',
     *   'SOURCE ID               ', 'ANTENNA NO.             ',
     *   'SUBARRAY                ', 'FREQ ID                 ',
     *   'TSYS 1                  ', 'TANT 1                  ',
     *   'TSYS 2                  ', 'TANT 2                  '/
      DATA UNITS /'DAYS    ', 'DAYS    ', 4*'        ',4*'KELVINS '/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 1000
      NCOL = 6 + 2 * NUMPOL
      IF (DOREAD) NCOL = 0
      NKEY = 3
      NDATA = MAXTYC
      CALL FILL (NDATA, 0, TYKOLS)
      CALL FILL (NDATA, 0, TYNUMV)
C                                       Fill in types
      IF (.NOT.DOREAD) THEN
         CALL COPY (NDATA, DTYP, DATP(1,2))
         DO 10 J = 1,NDATA
            IF (DTYP(J).LT.10) DATP(J,2) = DTYP(J) + 10 * NUMIF
 10         CONTINUE
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'TY', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'TYINI', IERR)
         GO TO 990
         END IF
C                                       Reset NKEY
      NKEY = 3
      NEWFIL = IERR .LT. 0
C                                       See if file exists.
      IF (NEWFIL) THEN
C                                       File created, initialize
         DO 40 I = 1,NCOL
C                                       Col. labels.
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'TYINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'TYINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       No. polarizations
         KLOCS(1) = 1
         KEYTYP(1) = 4
         KEYVAL(1) = NUMPOL
C                                       No. IFs
         KLOCS(2) = 2
         KEYTYP(2) = 4
         KEYVAL(2) = NUMIF
C                                       Revision number from PTYTAB.INC
         KLOCS(3) = 3
         KEYTYP(3) = 4
         KEYVAL(3) = TYREV
C                                       write keywords
         CALL TABKEY ('WRIT', KEYWRD, NKEY, BUFFER, KLOCS, KEYVAL,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'TYINI', IERR)
            GO TO 990
            END IF
C                                       pre-existing read keywords
      ELSE
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL TABKEY ('READ', KEYWRD, NKEY, BUFFER, KLOCS, KEYVAL,
     *      KEYTYP, IERR)
         MSGSUP = MSGSAV
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'TYINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       No. polarizations
         IPOINT = KLOCS(1)
         NUMPOL = 1
         IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF s
         IPOINT = KLOCS(2)
         NUMIF = 1
         IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       Revision number
         IPOINT = KLOCS(3)
         REVNO = -1
         IF (IPOINT.GT.0) REVNO = KEYVAL(IPOINT)
         END IF
C                                       Set ITYRNO
      ITYRNO = BUFFER(5) + 1
      IF (DOREAD) ITYRNO = 1
C                                       Get array indices
C                                       Cover your ass from FNDCOL -
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, ITYRNO, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'TYINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'TY', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'TYINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (NDATA, TITLE, 24, .TRUE., BUFFER, TYKOLS, JERR)
C                                       INTERPRET
      NCOL = 6 + 2 * NUMPOL
      DO 150 I = 1,NDATA
         IPOINT = TYKOLS(I)
         IF (IPOINT.GT.0) THEN
            TYKOLS(I) = DATP(IPOINT,1)
            TYNUMV(I) = DATP(IPOINT,2) / 10
            IF (TYNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               IF (I.LE.NCOL) CALL MSGWRT (6)
               END IF
         ELSE
            TYKOLS(I) = -1
            TYNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            IF (I.LE.NCOL) CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('TYINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('TYINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('TYINI: ERROR INITIALIZING TSYS TABLE FOR ',A4)
      END
