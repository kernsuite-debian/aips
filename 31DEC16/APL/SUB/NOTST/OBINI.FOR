      SUBROUTINE OBINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IOBRNO, OBKOLS, OBNUMV, IERR)
C----------------------------------------------------------------------
C! Creates and initializes a spacecraft orbit (OB) table
C# EXT-appl
C----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2006-2007
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
C   Creates and initializes spacecraft orbit (OB) tables
C   Inputs:
C      OPCODE     C*4       Operation code:
C                           'WRIT' = create/init for write or read
C                           'READ' = open for read only
C      BUFFER     I(512)    I/O buffer and related storage, also
C                           defines file is open.
C      DISK       I         Disk volume
C      CNO        I         Catalog slot number
C      CATBLK     I(256)    Catalog header block
C      LUN        I         Logical unit number for table I/O
C   Input/output:
C      VER        I         OB table version
C   Output:
C      IOBRNO     I         Next row number; start of file if READ,
C                           the last record plus one if WRIT
C      OBKOLS     I(MAXOBC) The column pointer array in order:
C                           ANTENNA NO, SUBARRAY, TIME, ORBXYZ,
C                           VELXYZ, SUN ANGLE, ECLIPSE, ORIENTATION
C      OBNUMV     I(MAXOBC) Element count in each column
C      IERR       I         Return code (0=>ok, else error)
C----------------------------------------------------------------------
      INCLUDE 'INCS:POBV.INC'
      CHARACTER OPCODE*4
      INTEGER BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, IOBRNO,
     *   OBKOLS(MAXOBC), OBNUMV(MAXOBC), IERR
C
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL T, DOREAD, NEWFIL
      CHARACTER LTOB(MAXOBC)*24, LTTOB*56, LUNTOB(MAXOBC)*8,
     *   LKEYOB(NKEYOB)*8
      INTEGER NREC, NCOL, NKEY, NDATA, DTYP(MAXOBC), DATP(128,2), I,
     *   ITEMP(6), NTTOB, IPOINT, IPT, KLOCS(NKEYOB), KEYVAL(NKEYOB),
     *   KEYTYP(NKEYOB), JERR, NC, ITRIM
      HOLLERITH HOLTMP(6)
      EQUIVALENCE (HOLTMP, ITEMP)
      DATA LTOB /'ANTENNA_NO              ',
     *   'SUBARRAY                ', 'TIME                    ',
     *   'ORBXYZ                  ', 'VELXYZ                  ',
     *   'SUN_ANGLE               ', 'ECLIPSE                 ',
     *   'ORIENTATION             '/
      DATA LTTOB /'SPACECRAFT ORBIT TABLE '/
      DATA LUNTOB /2*'        ', 'DAYS    ', 'METERS  ', 'M/SEC   ',
     *   'DEGREES ', 'DAYS    ', 'DEGREES '/
      DATA LKEYOB /'TABREV  '/
      DATA NTTOB /56/
      DATA T /.TRUE./
C----------------------------------------------------------------------
C                                       Initialization
      IERR = 0
C                                       Check OPCODE
      DOREAD = (OPCODE.EQ.'READ')
C                                       Set up needed variables
      NREC = 30
      NCOL = MAXOBC
      IF (DOREAD) NCOL = 0
      NKEY = NKEYOB
      NDATA = MAXOBC
      CALL FILL (NDATA, 0, OBKOLS)
      CALL FILL (NDATA, 0, OBNUMV)
C                                       Fill in types, lengths:
C                                       See Going AIPS, Vol 2, p13-3.
      IF (.NOT.DOREAD) THEN
         DTYP(OBIANT) = TABINT + 10
         DTYP(OBISUB) = TABINT + 10
         DTYP(OBDTIM) = TABDBL + 10
         DTYP(OBDPOS) = TABDBL + 10 * 3
         DTYP(OBDVEL) = TABDBL + 10 * 3
         DTYP(OBRANG) = TABFLT + 10 * 3
         DTYP(OBRECL) = TABFLT + 10 * 4
         DTYP(OBRORI) = TABFLT + 10
         CALL COPY (NCOL, DTYP, DATP(1,2))
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'OB', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'OBINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      IOBRNO = BUFFER(5) + 1
      IF (DOREAD) IOBRNO = 1
      NKEY = NKEYOB
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Column labels
         DO 40 I = 1, NCOL
            CALL CHR2H (24, LTOB(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'OBINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, LUNTOB(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'OBINI', IERR)
               GO TO 990
               END IF
40          CONTINUE
C                                       Fill in table title
         CALL CHR2H (NTTOB, LTTOB, 1, BUFFER(101))
C                                       Write keywords if table created
         IPOINT = 1
         IPT = 1
C                                       Table revision
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = IOBREV
C                                       Write to the OB table
         CALL TABKEY ('WRIT', LKEYOB, NKEYOB, BUFFER, KLOCS, KEYVAL,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'OBINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         CALL TABKEY ('READ', LKEYOB, NKEYOB, BUFFER, KLOCS, KEYVAL,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'OBINI', IERR)
            GO TO 990
            END IF
         END IF
C                                       Get array indices:
C                                       Prevent problems with FNDCOL -
C                                       close to flush the buffers
C                                       and then reopen.
      CALL TABIO ('CLOS', 0, 0, BUFFER, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'OBINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
C                                       Re-open
      CALL TABINI (OPCODE, 'OB', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'OBINI', IERR)
         GO TO 990
         END IF
C
      CALL FNDCOL (NDATA, LTOB, 24, T, BUFFER, OBKOLS, JERR)
C                                       Get array indices and no. val.
      DO 150 I = 1,NDATA
         IPOINT = OBKOLS(I)
         IF (IPOINT.GT.0) THEN
            OBKOLS(I) = DATP(IPOINT,1)
            OBNUMV(I) = DATP(IPOINT,2) / 10
            IF (OBNUMV(I).LE.0) THEN
               NC = ITRIM (LTOB(I))
               WRITE (MSGTXT,1100) LTOB(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            OBKOLS(I) = -1
            OBNUMV(I) = 0
            NC = ITRIM (LTOB(I))
            WRITE (MSGTXT,1101) LTOB(I)(:NC)
            CALL MSGWRT (6)
            END IF
150      CONTINUE
C
      GO TO 999
C                                       Error
990   WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
 1100 FORMAT ('OBINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('OBINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('OBINI: ERROR INITIALIZING SPACECRAFT ORBIT TABLE FOR ',
     *   A4)
      END
