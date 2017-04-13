      SUBROUTINE FRTAB (OPCODE, BUFFER, IERR)
C-----------------------------------------------------------------------
C! I/O to frequency tables (following initialization by FRINI)
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2015
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
C   Does I/O to frequency tables. Usually used after setup by
C   FRINI.
C   Inputs:
C      OPCODE   C*4        Operation code:
C                          'READ' = read entry from table.
C                          'WRIT' = write entry in table.
C                          'CLOS' = close file, flush on write
C      BUFFER   I(512)     I/O buffer and related storage, also defines
C                          file if open. Should have been returned by
C                          FRINI.
C      IFRRNO   I          Next scan number to read or write
C   Inputs from common
C      FRKOLS   I(MAXFRC)  The column pointer array in order:
C                          FREQID, BANDFREQ, CH WIDTH, TOTAL BANDWIDTH,
C                          SIDEBAND, BBC CHANNEL
C      FRNUMV   I(MAXFRC)  Element count in each column.
C   Input/output: (written to or read from antenna file)
C      IFQID    I          Freqid number in uv data
C      BANDFR   D(*)       Freq. offset from ref. freq, one
C                          per band (IF). (Hz)
C      CHWID    R(*)       Bandwidth of a spectral channel (Hz)
C                          one per band (IF).
C      TBANDW   R(*)       Total bandwidth of a band (Hz)
C      ISIDEB   I(*)       Sideband of a band, -1 => lower
C                          1 => upper
C      IBBC     I(*)       BBC channel corresponding to the IF
C   Output:
C      IFRRNO   I          Next row number.
C      IERR     I          Error code, 0=>OK else TABIO error.
C                          Note: -1=> read but record deselected.
C   Usage NOTE: use the include 'DFRV.INC' for the declarations in
C   FRINI and FRTAB.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), IERR
C
      LOGICAL   DOREAD, DOCLOS
      INTEGER   NCOPY, I
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFRV.INC'
      INTEGER   RECI(MAXREC)
      REAL      RECORD(MAXREC)
      HOLLERITH RECH(MAXREC)
      DOUBLE PRECISION RECD(MAXRCD)
      EQUIVALENCE (RECD, RECORD, RECI, RECH)
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
C                                       Freq id
         RECI(FRKOLS(KFRE)) = IFQID
C                                       Band freq offset
         DO 100 I = 1, FRNUMV(KBFQ)
            RECD(FRKOLS(KBFQ)+I-1) = BANDFR(I)
 100        CONTINUE
C                                       Ch. width
         NCOPY = FRNUMV(KCHW)
         CALL RCOPY (NCOPY, CHWID, RECORD(FRKOLS(KCHW)))
C                                       Total bandwidth
         NCOPY = FRNUMV(KTBW)
         CALL RCOPY (NCOPY, TBANDW, RECORD(FRKOLS(KTBW)))
C                                       Sideband
         NCOPY = FRNUMV(KSID)
         CALL COPY (NCOPY, ISIDEB, RECI(FRKOLS(KSID)))
C                                       BBC channel
         NCOPY = FRNUMV(KBBC)
         CALL COPY (NCOPY, IBBC, RECI(FRKOLS(KBBC)))
         END IF
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, IFRRNO, RECORD, BUFFER, IERR)
      IFRRNO = IFRRNO + 1
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABIO', 'FRTAB', IERR)
         GO TO 990
         END IF
      IF (IERR.LT.0) GO TO 50
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
C                                       Freq id
         IFQID = RECI(FRKOLS(KFRE))
C                                       Band freq offset
         DO 200 I = 1, FRNUMV(KBFQ)
            BANDFR(I) = RECD(FRKOLS(KBFQ)+I-1)
 200        CONTINUE
C                                       Ch. width
         NCOPY = FRNUMV(KCHW)
         CALL RCOPY (NCOPY, RECORD(FRKOLS(KCHW)), CHWID)
C                                       Total bandwidth
         NCOPY = FRNUMV(KTBW)
         CALL RCOPY (NCOPY, RECORD(FRKOLS(KTBW)), TBANDW)
C                                       Sideband
         NCOPY = FRNUMV(KSID)
         CALL COPY (NCOPY, RECI(FRKOLS(KSID)), ISIDEB)
C                                       BBC channel
         NCOPY = FRNUMV(KBBC)
         CALL COPY (NCOPY, RECI(FRKOLS(KBBC)), IBBC)
         END IF
         GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, IFRRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'FRTAB', IERR)
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('FRTAB: TABIO ERROR',I5)
      END
