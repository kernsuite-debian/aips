      SUBROUTINE FQINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IFQRNO, FQKOLS, FQNUMV, NUMIF, IERR)
C-----------------------------------------------------------------------
C! Create/open/initialize frequency (FQ) table
C# EXT-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2005-2006, 2012, 2014
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
C   Creates and initializes frequency (FQ) extension tables.
C   Inputs:
C      OPCODE   C*4        Operation code:
C                          'WRIT' = create/init for write or read
C                          'READ' = open for read only
C      BUFFER   I(512)     I/O buffer and related storage, also defines
C                          file if open.
C      DIS      I          Disk to use.
C      CNO      I          Catalog slot number
C      VER      I          FQ file version
C      CATBLK   I(256)     Catalog header block.
C      LUN      I          Logical unit number to use
C   Input (create) / output (pre-existing)
C      NUMIF    I          Number of IFs
C   Output:
C      IFQRNO   I          Next row number, start of the file if 'READ',
C                          the last+1 if WRITE
C      FQKOLS   I(5)       The column pointer array in order:
C                          FQID, IFFREQ, IFCHW, IFTBW, IFSIDE
C      FQNUMV   I(5)       Element count in each column.
C      IERR     I          Return error code, 0=>OK, else TABINI or
C                          TABIO error.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), DISK, CNO, VER, CATBLK(256), LUN, IFQRNO,
     *   FQKOLS(*), FQNUMV(*), NUMIF, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH HOLTMP(6)
      CHARACTER TTITLE*56, TITLE(MAXFQC)*24, UNITS(MAXFQC)*8, KEYW(1)*8,
     *   XTITLE*24
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(MAXFQC), NDATA,
     *   KLOCS(1), KEYVAL(1), KEYTYP(1), IPOINT, J, I, ITRIM, NC
      LOGICAL   NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NTT /56/
      DATA TTITLE /'AIPS UV DATA FILE FREQUENCY TABLE '/
      DATA DTYP /14,1,2,2,4,3/
      DATA TITLE, XTITLE /'FRQSEL', 'IF FREQ', 'CH WIDTH',
     *   'TOTAL BANDWIDTH', 'SIDEBAND', 'RXCODE', 'BANDCODE'/
      DATA KEYW /'NO_IF'/
      DATA UNITS /' ',3*'HZ',2*' '/
C-----------------------------------------------------------------------
C                                       Open file
      NREC = 500
      NCOL = MAXFQC
      NDATA = MAXFQC
      NKEY = 1
C                                       Fill in types
      IF (OPCODE.NE.'READ') THEN
         DATP(1,2) = DTYP(1)
         DO 10 J = 2,NDATA
            IF (J.EQ.6) THEN
               DATP(J,2) = DTYP(J) + 10*NUMIF*8
            ELSE
               DATP(J,2) = DTYP(J) + 10*NUMIF
               END IF
 10         CONTINUE
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'FQ', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'FQINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of scans
      IFQRNO = BUFFER(5) + 1
      IF (OPCODE.EQ.'READ') IFQRNO = 1
      NKEY = 1
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NDATA
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'FQINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'FQINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
C                                       No. IF's
         KLOCS(1) = 1
         KEYTYP(1) = 4
         KEYVAL(1) = NUMIF
C                                       Only write if just created.
         CALL TABKEY ('WRIT', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'FQINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'FQINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       No. IF's
         IPOINT = KLOCS(1)
         IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
         END IF
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, I, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'FQINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'FQ', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'FQINI', IERR)
         GO TO 990
         END IF
C                                       Get array indices
      CALL FNDCOL (MAXFQC, TITLE, 24, .TRUE., BUFFER, FQKOLS, I)
      IF (FQKOLS(6).LE.0) CALL FNDCOL (1, XTITLE, 24, .TRUE., BUFFER,
     *   FQKOLS(6), I)
C                                       Get array indices
      DO 150 I = 1,NDATA
         IPOINT = FQKOLS(I)
         IF (IPOINT.GT.0) THEN
            FQKOLS(I) = DATP(I,1)
            FQNUMV(I) = DATP(I,2) / 10
            IF (FQNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
C                                       missing BANDCODE is silent
         ELSE
            FQKOLS(I) = -1
            FQNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            IF (I.NE.6) CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('FQINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('FQINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('FQINI: ERROR INITIALIZING FREQUENCY TABLE FOR ',A4)
      END
