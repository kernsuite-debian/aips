      SUBROUTINE TABFLG (OPCODE, BUFFER, IFGRNO, FGKOLS, FGNUMV,
     *   SOURID, SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON,
     *   IERR)
C-----------------------------------------------------------------------
C! Does I/O to Flag (FG) table opened by FLGINI
C# EXT-util UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   Does I/O to FLAG (FG) extention tables. Usually used after setup by
C   FLGINI.
C    Inputs:
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table (must have been
C                      opened with 'WRIT'.
C                      'FLAG' = like 'WRIT' but entry deselected.
C                      'CLOS' = close file, flush on write
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open. Should have been returned by FLGINI or
C                      TABINI.
C     IFGRNO       I   Next FLAG entry number to read or write.
C     FGKOLS(MAXFGC)   I   The column pointer array in order, SOURCE,
C                      SUBARRAY, FREQID, ANTS, TIMERANG, IFS, CHANS,
C                      PFLAGS, REASON
C     FGNUMV(MAXFGC)    I   Element count in each column.
C    Input/output: (written to or read from FLAG file)
C     SOURID       I   Source ID as defined in the SOURCE table.
C     SUBA         I   Subarray number.
C     FREQID       I   Freq. ID number
C     ANTS(2)      I   Antenna numbers, 0=>all
C     TIMER(2)     R   Start and end time of data to be flagged (Days)
C     IFS(2)       I   First and last IF numbers to flag. 0=>all
C     CHANS(2)     I   First and last channel numbers to flag. 0=>all
C     PFLAGS(4)    L   Polarization flags, same order as in data.
C                      .TRUE. => polarization flagged.
C     REASON       C*24 Reason for flagging
C    Output:
C     IFGRNO       I   Next scan number.
C     IERR         I   Error code, 0=>OK else TABIO error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4, REASON*24
      HOLLERITH RECH(1)
      INTEGER   BUFFER(512), FGKOLS(MAXFGC), FGNUMV(MAXFGC), SOURID,
     *   SUBA, FREQID, ANTS(2), IFS(2), CHANS(2), IERR, TYPE,
     *   RECI(1), SOUKOL, SUBKOL, FRQKOL, ANTKOL, TIMKOL, IFKOL,
     *   CHKOL, POLKOL, REAKOL, IFGRNO
      LOGICAL   PFLAGS(4), DOREAD, DOCLOS
      REAL      TIMER(2), RECORD(31)
      EQUIVALENCE (RECORD, RECI, RECH)
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       Set pointers
C                                       Determine if old/new table
      TYPE = 1
      IF (BUFFER(10).EQ.9) TYPE = 2
C                                       If type=2, set up different
C                                       column id's
      IF (TYPE.EQ.2) THEN
         SOUKOL = FGKOLS(1)
         SUBKOL = FGKOLS(2)
         FRQKOL = FGKOLS(3)
         ANTKOL = FGKOLS(4)
         TIMKOL = FGKOLS(5)
         IFKOL = FGKOLS(6)
         CHKOL = FGKOLS(7)
         POLKOL = FGKOLS(8)
         REAKOL = FGKOLS(9)
      ELSE
         SOUKOL = FGKOLS(1)
         SUBKOL = FGKOLS(2)
         ANTKOL = FGKOLS(3)
         TIMKOL = FGKOLS(4)
         IFKOL = FGKOLS(5)
         CHKOL = FGKOLS(6)
         POLKOL = FGKOLS(7)
         REAKOL = FGKOLS(8)
         END IF
C                                       If write fill RECORD
      IF (DOREAD) GO TO 50
         RECI(SOUKOL) = SOURID
         RECI(SUBKOL) = SUBA
         RECI(FRQKOL) = FREQID
         RECI(ANTKOL) = ANTS(1)
         RECI(ANTKOL+1) = ANTS(2)
         RECORD(TIMKOL) = TIMER(1)
         RECORD(TIMKOL+1) = TIMER(2)
         RECI(IFKOL) = IFS(1)
         RECI(IFKOL+1) = IFS(2)
         RECI(CHKOL) = CHANS(1)
         RECI(CHKOL+1) = CHANS(2)
         CALL LG2BIT (4, PFLAGS, RECI(POLKOL), 1)
         CALL CHR2H (24, REASON, 1, RECH(REAKOL))
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, IFGRNO, RECORD, BUFFER, IERR)
      IFGRNO = IFGRNO + 1
C                                       If READ pick data from RECORD.
      IF (.NOT.DOREAD) GO TO 999
         SOURID = RECI(SOUKOL)
         SUBA = RECI(SUBKOL)
         IF (TYPE.EQ.1) THEN
            FREQID = -1
         ELSE
            FREQID = RECI(FRQKOL)
            END IF
         ANTS(1) = RECI(ANTKOL)
         ANTS(2) = RECI(ANTKOL+1)
         TIMER(1) = RECORD(TIMKOL)
         TIMER(2) = RECORD(TIMKOL+1)
         IFS(1) = RECI(IFKOL)
         IFS(2) = RECI(IFKOL+1)
         CHANS(1) = RECI(CHKOL)
         CHANS(2) = RECI(CHKOL+1)
         CALL LG2BIT (4, PFLAGS, RECI(POLKOL), -1)
         CALL H2CHR (24, 1, RECH(REAKOL), REASON)
         GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, IFGRNO, RECORD, BUFFER, IERR)
      GO TO 999
C
 999  RETURN
      END
