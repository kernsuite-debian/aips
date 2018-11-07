      SUBROUTINE OFINI (OPCODE, OFBUFF, DISK, CNO, VER, CATBLK, LUNOF,
     *   IOFRNO, OFKOLS, OFNUMV, NIF, NPOL, ANNAME, OBSCOD, OBSDAT,
     *   TABVER, IERR)
C-----------------------------------------------------------------------
C! Create/open/initialize Online Flags (OF) table
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2003, 2006, 2008
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
C   Creates and initializes Online Flags (OF) extension tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'WRIT' = create/init for write or read
C                        'READ' = open for read only
C      OFBUFF   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VER      I        OF file version
C      CATBLK   I(256)   Catalog header block.
C      LUNOF    I        Logical unit number to use
C   Input (create) / output (pre-existing) (file keywords):
C      NIF      I        Number of IFs
C      NPOL     I        Number polarizations (1 or 2)
C      ANNAME   C*8      Array name
C      OBSCOD   C*8      Observing code.
C      OBSDAT   C*8      Observing date.
C      TABVER   I        Table revision number.
C      OFNUMV   I(MAXOFC)   Element count in each column. On input only
C                        used if the file is created.
C   Output:
C      IOFRNO   I        Next record number, start of the file if
C                        'READ', the last+1 if WRITE
C      OFKOLS   I(MAXOFC)   The column pointer array in order defined in
C                        DOFV.INC.
C      IERR     I        Return error code, 0=>OK, else TABINI or TABIO
C                        error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DOFV.INC'
      CHARACTER OPCODE*4, ANNAME*8, OBSCOD*8, OBSDAT*8
      INTEGER   OFBUFF(512), DISK, CNO, VER, CATBLK(256), LUNOF, IOFRNO,
     *   OFKOLS(MAXOFC), OFNUMV(MAXOFC), TABVER, NIF, NPOL, IERR
C
      CHARACTER TTITLE*56, TITLE(MAXOFC)*24, UNITS(MAXOFC)*8,
     *   KEYWRD(NKEYOF)*8
      INTEGER    NKEY, NREC, DATP(128,2), NCOL, KLOCS(NKEYOF), I, JERR,
     *   KEYTYP(NKEYOF), NTTOF, NDATA, IPOINT, DTYP(MAXOFC), ITEMP(6),
     *   KEYI(NKYWOF), NC, ITRIM
      REAL      KEYR(NKYWOF)
      DOUBLE PRECISION KEYD(NKYWO2)
      HOLLERITH KEYH(NKYWOF), HOLTMP(6)
      LOGICAL    DOREAD, NEWFIL, T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KEYI, KEYR, KEYD, KEYH)
      EQUIVALENCE (HOLTMP, ITEMP)
      DATA NTTOF / 56 /
      DATA TTITLE / 'VLA ON-LINE FLAGS TABLE' /
      DATA KEYWRD /'NO_IF', 'NO_POL', 'ANNAME', 'OBSCODE', 'RDATE',
     *   'REVISION' /
C                                       NOTE: change DOFV.INC if the
C                                       columns are changed.
      DATA TITLE /'TIME', 'SOURCE ID', 'ANTENNA NO.', 'SUBARRAY',
     *   'FREQ ID', 'ANT FLAG', 'STATUS 1', 'STATUS 2'/
      DATA UNITS /'DAYS', 7*' ' /
      DATA T / .TRUE. /
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Set up needed variables
      NREC = 1000
      NCOL = MAXOFC - 2 + NPOL
      IF (DOREAD) NCOL = 0
      NKEY = NKEYOF
      NDATA = MAXOFC
      CALL FILL (NDATA, 0, OFKOLS)
      CALL FILL (NDATA, 0, OFNUMV)
C                                       Fill in types, lengths
C                                       See Going AIPS Vol 2 p13-3.
      IF (.NOT.DOREAD) THEN
         DTYP(OFRTIM) = TABFLT + 10
         DTYP(OFISID) = TABINT + 10
         DTYP(OFIANT) = TABINT + 10
         DTYP(OFISUB) = TABINT + 10
         DTYP(OFIFQI) = TABINT + 10
         DTYP(OFIANF) = TABINT + 10
         DTYP(OFIST1) = TABINT + 10 * NIF
         DTYP(OFIST2) = TABINT + 10 * NIF
         CALL COPY (NCOL, DTYP, DATP(1,2))
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'OF', DISK, CNO, VER, CATBLK, LUNOF, NKEY,
     *   NREC, NCOL, DATP, OFBUFF, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'OFINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      IOFRNO = OFBUFF(5) + 1
      IF (DOREAD) IOFRNO = 1
      NKEY = NKEYOF
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, ITEMP, OFBUFF, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'OFINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, ITEMP, OFBUFF, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'OFINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTTOF, TTITLE, 1, OFBUFF(101))
C                                       Set keyword values
C                                       Number of IFs
         KLOCS(1) = 1
         KEYTYP(1) = TABINT
         KEYI(KLOCS(1)) = NIF
C                                       Number of Polarizations
         KLOCS(2) = 2
         KEYTYP(2) = TABINT
         KEYI(KLOCS(2)) = NPOL
C                                       arrat name
         KLOCS(3) = 3
         KEYTYP(3) = TABHOL
         CALL CHR2H (8, ANNAME, 1, KEYH(KLOCS(3)))
C                                       Observing code
         KLOCS(4) = 5
         KEYTYP(4) = TABHOL
         CALL CHR2H (8, OBSCOD, 1, KEYH(KLOCS(4)))
C                                       Observing date
         KLOCS(5) = 7
         KEYTYP(5) = TABHOL
         CALL CHR2H (8, OBSDAT, 1, KEYH(KLOCS(5)))
C                                       Table revision number
         KLOCS(6) = 9
         KEYTYP(6) = TABINT
         KEYI(KLOCS(6)) = OFREV
C                                       Write to the OF table
         CALL TABKEY ('WRIT', KEYWRD, NKEYOF, OFBUFF, KLOCS, KEYI,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'OFINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         CALL TABKEY ('READ', KEYWRD, NKEYOF, OFBUFF, KLOCS, KEYI,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'OFINI', IERR)
            GO TO 990
            END IF
C                                       Number of IFs
         NIF = KEYI(KLOCS(1))
C                                       Number polarizations
         NPOL = KEYI(KLOCS(2))
C                                       Observing code
         CALL H2CHR (8, 1, KEYH(KLOCS(3)), ANNAME)
C                                       Observing code
         CALL H2CHR (8, 1, KEYH(KLOCS(4)), OBSCOD)
C                                       Observing date
         CALL H2CHR (8, 1, KEYH(KLOCS(5)), OBSDAT)
C                                       Table revision number
         TABVER = KEYI(KLOCS(6))
         END IF
C                                       Get array indices
C                                       Cover your ass from FNDCOL -
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYI, OFBUFF, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'OFINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'OF', DISK, CNO, VER, CATBLK, LUNOF, NKEY,
     *   NREC, NCOL, DATP, OFBUFF, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'OFINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXOFC, TITLE, 24, T, OFBUFF, OFKOLS, JERR)
C                                       Get array indices and no. values
      NCOL = MAXOFC - 2 + NPOL
      IF (NPOL.LT.2) OFNUMV(OFIST2) = 0
      DO 150 I = 1,NCOL
         IPOINT = OFKOLS(I)
         IF (IPOINT.GT.0) THEN
            OFKOLS(I) = DATP(IPOINT,1)
            OFNUMV(I) = DATP(IPOINT,2) / 10
            IF (OFNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            OFKOLS(I) = -1
            OFNUMV(I) = 0
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
 1100 FORMAT ('OFINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('OFINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('OFINI: ERROR INITIALIZING ON-LINE FLAGS TABLE FOR ', A4)
      END
