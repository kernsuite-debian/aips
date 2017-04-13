      SUBROUTINE TABFO (OPCODE, BUFFER, IFORNO, FOKOLS, FONUMV, TIME,
     *   TIMEI, SOURID, ANTNO, SUBA, FREQID, DOPOFF, IERR)
C-----------------------------------------------------------------------
C! Does I/O to frequency offset (FO) table opened by FOINI
C# EXT-util UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2009
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
C   Does I/O to frequency offset extension tables. Used after setup
C   by FOINI.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                          'READ' = read entry from table.
C                          'WRIT' = write entry in table.
C                          'CLOS' = close file, flush on write
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                           file if open. Should have been returned by
C                           TABINI or CALINI.
C      IFORNO   I        Next scan number to read or write.
C      FOKOLS   I(7)     The column pointer array in order defined by
C                          the parameters in PCLTAB.INC.
C      FONUMV   I(7)     Element count in each column.
C   Input/output: (written to or read from CAL file)
C      TIME     D        Center time of FO record (Days)
C      TIMEI    R        Time interval covered by record (days)
C      SOURID   I        Source ID as defined in the SoUrce table.
C      ANTNO    I        Antenna number as defined in Antenna table
C      SUBA     I        Subarray number.
C      FREQID   I        Freqid # as defined in FQ table.
C      DOPOFF   R(*)     Doppler offset for each IF (Hz)
C    Output:
C      IFORNO   I        Next CAL number.
C      IERR     I        Error code, 0=>OK else TABIO error.
C                           Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXFOC, XFORSZ
      PARAMETER (MAXFOC = 7)
      PARAMETER (XFORSZ = 7 + MAXIF)
C
      CHARACTER OPCODE*4
      DOUBLE PRECISION TIME
      REAL      TIMEI, DOPOFF(MAXIF)
      INTEGER   BUFFER(512), FOKOLS(MAXFOC), FONUMV(MAXFOC), SOURID,
     *   ANTNO, SUBA, FREQID, IFORNO, IERR
C
      INTEGER   RECI(XFORSZ)
      LOGICAL   DOREAD, DOCLOS
      REAL      RECORD(XFORSZ)
      DOUBLE PRECISION RECD(XFORSZ/2)
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECD, RECORD, RECI)
      DATA RECORD /XFORSZ*0.0D0/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
         RECD(FOKOLS(1)) = TIME
         RECORD(FOKOLS(2)) = TIMEI
         RECI(FOKOLS(3)) = SOURID
         RECI(FOKOLS(4)) = ANTNO
         RECI(FOKOLS(5)) = SUBA
         IF (FOKOLS(6).GT.0) RECI(FOKOLS(6)) = FREQID
         CALL RCOPY (FONUMV(7), DOPOFF, RECORD(FOKOLS(7)))
         END IF
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, IFORNO, RECORD, BUFFER, IERR)
      IFORNO = IFORNO + 1
      IF (IERR.GT.0) GO TO 990
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
         TIME = RECD(FOKOLS(1))
         TIMEI = RECORD(FOKOLS(2))
         SOURID = RECI(FOKOLS(3))
         ANTNO = RECI(FOKOLS(4))
         SUBA = RECI(FOKOLS(5))
         FREQID = -1
         IF (FOKOLS(6).GT.0) FREQID = RECI(FOKOLS(6))
         CALL RCOPY (FONUMV(7), RECORD(FOKOLS(7)), DOPOFF)
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, IFORNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABFO: TABIO ERROR',I3)
      END
