      SUBROUTINE POINI (OPCODE, POBUFF, DISK, CNO, VER, CATBLK, LUNPO,
     *   IPORNO, POKOLS, PONUMV, OBSDAT, TABVER, IERR)
C----------------------------------------------------------------------
C! Creates and initializes a planetary position (PO) table
C# EXT-appl
C----------------------------------------------------------------------
C;  Copyright (C) 2001-2002, 2006
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
C----------------------------------------------------------------------
C   Creates and initializes planetary position (PO) tables
C   Inputs:
C      OPCODE     C*4       Operation code:
C                           'WRIT' = create/init for write or read
C                           'READ' = open for read only
C      POBUFF     I(512)    I/O buffer and related storage, also
C                           defines file is open.
C      DISK       I         Disk volume
C      CNO        I         Catalog slot number
C      VER        I         PO table version
C      CATBLK     I(256)    Catalog header block
C      LUNPO      I         Logical unit number for table I/O
C   Output:
C      IPORNO     I         Next row number; start of file if READ,
C                           the last record plus one if WRIT
C      POKOLS(MAXPOC) I     The column pointer array in order:
C                           TIME, SOURCE, RA, DEC, DISTANCE
C      PONUMV(MAXPOC) I     Element count in each column. On input only
C                           used if the file is created.
C      OBSDAT     C*8       Observing date.
C      TABVER     I         Table revision number
C   Output:
C      IERR       I         Return code (0=>ok, else error)
C----------------------------------------------------------------------
      INCLUDE 'INCS:DPOV.INC'
      CHARACTER OPCODE*4, OBSDAT*8
      INTEGER   POBUFF(512), DISK, CNO, VER, CATBLK(256), LUNPO, IPORNO,
     *   POKOLS(MAXPOC), PONUMV(MAXPOC), TABVER, IERR
C
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL   T, DOREAD, NEWFIL
      CHARACTER TTITLE*56, TITLE(MAXPOC)*24, UNITS(MAXPOC)*8,
     *   KEYWRD(NKEYPO)*8
      INTEGER   I, JERR, ITEMP(6), NTTPO, NKEY, NREC, NCOL, IPOINT,
     *   NC, ITRIM, NDATA, DATP(128,2), KLOCS(NKEYPO), KEYTYP(NKEYPO),
     *   KEYI(NKYWPO), DTYP(MAXPOC)
      HOLLERITH KEYH(NKYWPO), HOLTMP(6)
      EQUIVALENCE (KEYI, KEYH)
      EQUIVALENCE (HOLTMP, ITEMP)
      DATA TTITLE / 'PLANETARY POSITION TABLE' /
      DATA TITLE / 'TIME', 'SOURCE ID', 'SOURCE RA', 'SOURCE DEC',
     *            'SOURCE DISTANCE' /
      DATA UNITS / 'DAYS', ' ', 'DEGREES', 'DEGREES', 'AU' /
      DATA KEYWRD / 'TABREV', 'RDATE' /
      DATA NTTPO / 56 /
      DATA T / .TRUE. /
C----------------------------------------------------------------------
C                                       Initialization
      IERR = 0
C                                       Check OPCODE
      DOREAD = (OPCODE.EQ.'READ')
C                                       Set up needed variables
      NREC = 30
      NCOL = MAXPOC
      IF (DOREAD) NCOL = 0
      NKEY = NKEYPO
      NDATA = MAXPOC
      CALL FILL (NDATA, 0, POKOLS)
      CALL FILL (NDATA, 0, PONUMV)
C                                       Fill in types, lengths:
C                                       See Going AIPS, Vol 2, p13-3.
      IF (.NOT.DOREAD) THEN
         DTYP(PODTIM) = TABDBL + 10
         DTYP(POISID) = TABINT + 10
         DTYP(PODRA) = TABDBL + 10
         DTYP(PODDEC) = TABDBL + 10
         DTYP(PODDST) = TABDBL + 10
         CALL COPY (NCOL, DTYP, DATP(1,2))
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'PO', DISK, CNO, VER, CATBLK, LUNPO, NKEY,
     *   NREC, NCOL, DATP, POBUFF, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'POINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      IPORNO = POBUFF(5) + 1
      IF (DOREAD) IPORNO = 1
      NKEY = NKEYPO
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Column labels
         DO 40 I = 1, NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, ITEMP, POBUFF, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'POINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, ITEMP, POBUFF, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'POINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in table title
         CALL CHR2H (NTTPO, TTITLE, 1, POBUFF(101))
C                                       Write keywords if table created
C                                       Table revision
         KLOCS(1) = 1
         KEYTYP(1) = TABINT
         KEYI(1) = IPOREV
C                                       Observing date
         KLOCS(2) = 2
         KEYTYP(2) = TABHOL
         CALL CHR2H (8, OBSDAT, 1, KEYH(KLOCS(2)))
C                                       Write to the PO table
         CALL TABKEY ('WRIT', KEYWRD, NKEYPO, POBUFF, KLOCS, KEYI,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'POINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         CALL TABKEY ('READ', KEYWRD, NKEYPO, POBUFF, KLOCS, KEYI,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'POINI', IERR)
            GO TO 990
            END IF
C                                       Table version
         TABVER = KEYI(KLOCS(1))
C                                       Observing date
         CALL H2CHR (8, 1, KEYH(KLOCS(2)), OBSDAT)
         END IF
C                                       Get array indices:
C                                       Prevent problems with FNDCOL -
C                                       close to flush the buffers
C                                       and then reopen.
      CALL TABIO ('CLOS', 0, 0, POBUFF, POBUFF, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'POINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
C                                       Re-open
      CALL TABINI (OPCODE, 'PO', DISK, CNO, VER, CATBLK, LUNPO, NKEY,
     *   NREC, NCOL, DATP, POBUFF, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'POINI', IERR)
         GO TO 990
         END IF
C
      CALL FNDCOL (NDATA, TITLE, 24, T, POBUFF, POKOLS, JERR)
C                                       Get array indices and no. val.
      DO 150 I = 1,NDATA
         IPOINT = POKOLS(I)
         IF (IPOINT.GT.0) THEN
            POKOLS(I) = DATP(IPOINT,1)
            PONUMV(I) = DATP(IPOINT,2) / 10
            IF (PONUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            POKOLS(I) = -1
            PONUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C                                       Exit
 999  RETURN
C----------------------------------------------------------------------
 1100 FORMAT ('POINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('POINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('POINI: ERROR INITIALIZING PLANETARY POSITION TABLE FOR ',
     *   A4)
      END
