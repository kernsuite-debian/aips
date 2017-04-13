      SUBROUTINE CPINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ICPRNO, CPKOLS, CPNUMV, NUMIF, NUMFRQ, FREQID, IERR)
C-----------------------------------------------------------------------
C! Create/open/initialize source polarization spectrum (CP) table
C# EXT-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2010, 2012
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
C   Creates and initializes source polarization spectrum (CP) extension
C   tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'WRIT' = create/init for write or read
C                        'READ' = open for read only
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      LUN      I        Logical unit number to use
C   Input/output
C      VER      I        CP file version
C      CATBLK   I(256)   Catalog header block.
C   Input (create) / output (pre-existing)
C      NUMIF    I        Number of IFs
C      NUMFRQ   I        Number of frequency channels
C      FREQID   I        Frequency ID code number
C   Output:
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      ICPRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      CPKOLS   I(MAXCPC)   The column pointer array in order:
C                        SOURCE, SOURCE ID, I, Q, U, V
C      CPNUMV   I(MAXCPC)   Element count in each column.
C      IERR     I        Return error code, 0=>OK, else TABINI or TABIO
C                        error.
C   MAXCPC = 6 only known locally
C-----------------------------------------------------------------------
C     INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXCPC
      PARAMETER (MAXCPC = 6)
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), DISK, CNO, VER, CATBLK(256), LUN, ICPRNO,
     *   CPKOLS(MAXCPC), CPNUMV(MAXCPC), NUMIF, NUMFRQ, FREQID, IERR
C
      HOLLERITH HOLTMP(6)
      CHARACTER TTITLE*56, TITLE(MAXCPC)*24, UNITS(MAXCPC)*8, KEYW(3)*8
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(MAXCPC), NDATA,
     *   KLOCS(3), KEYVAL(3), KEYTYP(3), IPOINT, MSGSAV, I, NC, JERR,
     *   ITRIM
      LOGICAL   DOREAD, NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NTT /56/
      DATA TTITLE /'AIPS SOURCE POLARIZATION SPECTRUM TABLE'/
      DATA DTYP /163, 14, 4*2/
      DATA TITLE /'SOURCE', 'SOURCE ID', 'I', 'Q', 'U', 'V'/
      DATA UNITS /2*' ', 4*'Jy'/
      DATA KEYW /'NO_IF', 'NO_CHAN', 'FREQID'/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 500
      NKEY = 2
      NDATA = MAXCPC
      CALL FILL (NDATA, 0, CPKOLS)
      CALL FILL (NDATA, 0, CPNUMV)
C                                       Fill in types
      IF (.NOT.DOREAD) THEN
         NCOL = MAXCPC
         CALL COPY (NDATA, DTYP, DATP(1,2))
         DATP(3,2) = DTYP(3) + 10*NUMFRQ*NUMIF
         DATP(4,2) = DTYP(4) + 10*NUMFRQ*NUMIF
         DATP(5,2) = DTYP(5) + 10*NUMFRQ*NUMIF
         DATP(6,2) = DTYP(6) + 10*NUMFRQ*NUMIF
      ELSE
         NCOL = 0
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'CP', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CPINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
      MSGSAV = MSGSUP
C                                       Get number of scans
      ICPRNO = BUFFER(5) + 1
      IF (DOREAD) ICPRNO = 1
      NKEY = 3
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 10 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CPINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CPINI', IERR)
               GO TO 990
               END IF
 10         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
C                                       No. IFs
         KLOCS(1) = 1
         KEYTYP(1) = 4
         KEYVAL(1) = NUMIF
C                                       No. CHAN
         KLOCS(2) = 2
         KEYTYP(2) = 4
         KEYVAL(2) = NUMFRQ
C                                       Frequency ID
         KLOCS(3) = 3
         KEYTYP(3) = 4
         KEYVAL(3) = FREQID
C                                       Only write if just created.
         CALL TABKEY ('WRIT', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'CPINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         MSGSUP = 32000
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         MSGSUP = MSGSAV
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'CPINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       No. IFs
         IPOINT = KLOCS(1)
         IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       No. CHAN
         IPOINT = KLOCS(2)
         IF (IPOINT.GT.0) NUMFRQ = KEYVAL(IPOINT)
C                                       Frequency ID
         IPOINT = KLOCS(3)
         IF (IPOINT.GT.0) FREQID = KEYVAL(IPOINT)
         END IF
      IERR = 0
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'CPINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'CP', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CPINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (NDATA, TITLE, 24, .TRUE., BUFFER, CPKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,NDATA
         IPOINT = CPKOLS(I)
         IF (IPOINT.GT.0) THEN
            CPKOLS(I) = DATP(IPOINT,1)
            CPNUMV(I) = DATP(IPOINT,2) / 10
            IF (CPNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            CPKOLS(I) = -1
            CPNUMV(I) = 0
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
 1100 FORMAT ('CPINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('CPINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('CPINI: ERROR INITIALIZING POLARIZATION TABLE FOR ',A)
      END
