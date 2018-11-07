      SUBROUTINE SYINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ISYRNO, SYKOLS, SYNUMV, NUMANT, NUMPOL, NUMIF, IERR)
C-----------------------------------------------------------------------
C! Creates/opens/initializes SysPower (SY) table
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
C   Creates and initializes calibration extension tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                           'WRIT' = create/init for write or read
C                           'READ' = open for read only
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VER      I        SY file version
C      CATBLK   I(256)   Catalog header block.
C      LUN      I        Logical unit number to use
C   Input/output
C      NUMANT   I        Number of antennas
C      NUMPOL   I        Number of IFs per pair
C      NUMIF    I        Number of IF pairs
C   Output:
C      ISYRNO   I        Next record number, start of the file
C                        if 'READ', the last+1 if WRITE
C      SYKOLS   I(12)    The column pointer array: time, interval,
C                        source, antenna, subarray, freqid, Pdif1,
C                        Psum1, Gb1, Pdif2, Psum2, Gb2
C      SYNUMV   I(12)    Element count in each column.
C      IERR     I        Return error code, 0=>OK, else TABINI or TABIO
C                        error.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, ISYRNO,
     *   SYKOLS(*), SYNUMV(*), NUMANT, NUMPOL, NUMIF, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXSYC
      PARAMETER (MAXSYC=13)
C
      HOLLERITH HOLTMP(6)
      CHARACTER TTITLE*56, TITLE(MAXSYC)*24, UNITS(MAXSYC)*8, KEYW(3)*8
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(MAXSYC), NDATA,
     *   KLOCS(3), KEYVAL(3), IPOINT, JERR, J, KEYTYP(3), I, MSGSAV, NC,
     *   ITRIM
      LOGICAL   T, DOREAD, NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T /.TRUE./
      DATA NTT /56/
      DATA TTITLE /'AIPS EVLA SysPower TABLE '/
C                                       Values >10 are scalar
      DATA DTYP /11, 12, 5*14, 6*2/
C                                       NOTE: Change PCLTAB.INC when
C                                       changing the table columns.
      DATA TITLE /'TIME           ', 'TIME INTERVAL           ',
     *   'CAL TYPE                ', 'SOURCE ID               ',
     *   'ANTENNA NO.             ', 'SUBARRAY                ',
     *   'FREQ ID                 ', 'POWER DIF1              ',
     *   'POWER SUM1              ', 'POST GAIN1              ',
     *   'POWER DIF2              ', 'POWER SUM2              ',
     *   'POST GAIN2              '/
      DATA KEYW /'NO_ANT  ', 'NO_POL  ', 'NO_IF   '/
      DATA UNITS /2*'DAYS', 5*' ', 2*'COUNTS', ' ', 2*'COUNTS', ' '/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 1000
      NCOL = 7 + NUMPOL * 3
      IF (DOREAD) NCOL = 0
      NKEY = 3
      NDATA = MAXSYC
      CALL FILL (NDATA, 0, SYKOLS)
      CALL FILL (NDATA, 0, SYNUMV)
C                                       Fill in types
      IF (.NOT.DOREAD) THEN
         CALL COPY (NDATA, DTYP, DATP(1,2))
         DO 10 J = 1,NDATA
            IF (DTYP(J).LT.10) DATP(J,2) = DTYP(J) + 10 * NUMIF
 10         CONTINUE
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'SY', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'SYINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      ISYRNO = BUFFER(5) + 1
      IF (DOREAD) ISYRNO = 1
      NKEY = 3
      MSGSAV = MSGSUP
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'SYINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'SYINI', IERR)
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
C                                       Only write if just created.
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'SYINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         MSGSUP = 32000
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         MSGSUP = MSGSAV
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'SYINI', IERR)
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
         END IF
C                                       Get array indices
C                                       Cover your ass from FNDCOL -
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'SYINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'SY', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'SYINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXSYC, TITLE, 24, T, BUFFER, SYKOLS, JERR)
C                                      Get array indices and no. values
      NCOL = 7 + NUMPOL * 3
      DO 150 I = 1,NCOL
         IPOINT = SYKOLS(I)
         IF (IPOINT.GT.0) THEN
            SYKOLS(I) = DATP(IPOINT,1)
            SYNUMV(I) = DATP(IPOINT,2) / 10
            IF (SYNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               IF (I.NE.3) CALL MSGWRT (6)
               END IF
         ELSE
            SYKOLS(I) = -1
            SYNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            IF (I.NE.3) CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('SYINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('SYINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('SYINI: ERROR INITIALIZING SYSPOWER TABLE FOR ',A4)
      END
