      SUBROUTINE TABNDX (OPCODE, BUFFER, INXRNO, NXKOLS, NXNUMV,
     *   TIME, DTIME, IDSOUR, SUBARR, VSTART, VEND, FREQID, IERR)
C-----------------------------------------------------------------------
C! Does I/O to Index (NX) table opened by NDXINI
C# EXT-util UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2013
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
C   Does I/O to INDEX extension tables. Usually used after setup by
C   NDXINI.
C    Inputs:
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open. Should have been returned by NDXINI or
C                      TABINI.
C     INXRNO       I   Next scan number to read or write.
C     NXKOLS(MAXNXC)  I   The column pointer array in order, TIME,
C                         TIME INTERVAL, SOURCE ID, SUBARRAY, START VIS,
C                         END VIS, FREQID.
C     NXNUMV(MAXNXC)  I   Element count in each column, set by NDXINI.
C    Input/output: (written to or read from INDEX file)
C     TIME         R   Center time of the scan (Days)
C     DTIME        R   Duration of scan (Days)
C     IDSOUR       I   Source ID as defined in then SOURCE table.
C     SUBARR       I   Subarray number.
C     VSTART       I   First visibility number in file.
C     VEND         I   Last visibility number in file.
C     FREQID       I   Freqid of scan
C    Output:
C     INXRNO       I   Next scan number.
C     IERR         I   Error code, 0=>OK else TABIO error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), NXKOLS(MAXNXC), NXNUMV(MAXNXC), IDSOUR,
     *   SUBARR, FREQID, IERR, TIMKOL, INTKOL, SOUKOL, SUBKOL, FRSKOL,
     *   LSTKOL, FRQKOL, RECI(12)
      LOGICAL   DOREAD, DOCLOS
      INTEGER   INXRNO, VSTART, VEND
      REAL      TIME, DTIME, RECORD(12)
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECORD, RECI)
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       Set pointers
      TIMKOL = NXKOLS(1)
      INTKOL = NXKOLS(2)
      SOUKOL = NXKOLS(3)
      SUBKOL = NXKOLS(4)
      FRSKOL = NXKOLS(5)
      LSTKOL = NXKOLS(6)
      FRQKOL = NXKOLS(7)
C                                       If write fill RECORD
      IF (DOREAD) GO TO 50
         RECORD(TIMKOL) = TIME
         RECORD(INTKOL) = DTIME
         RECI(SOUKOL) = IDSOUR
         RECI(SUBKOL) = SUBARR
         RECI(FRSKOL) = VSTART
         RECI(LSTKOL) = VEND
         RECI(FRQKOL) = FREQID
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, INXRNO, RECORD, BUFFER, IERR)
      INXRNO = INXRNO + 1
      IF (IERR.GT.0) GO TO 990
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
         TIME = RECORD(TIMKOL)
         DTIME = RECORD(INTKOL)
         IDSOUR = RECI(SOUKOL)
         SUBARR = RECI(SUBKOL)
         VSTART = RECI(FRSKOL)
         VEND = RECI(LSTKOL)
         IF (BUFFER(10).EQ.7) THEN
            FREQID = RECI(FRQKOL)
         ELSE
            FREQID = 1
            END IF
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, INXRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABNDX: TABIO ERROR',I3)
      END
