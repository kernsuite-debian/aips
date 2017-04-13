      SUBROUTINE TABTY (OPCODE, BUFFER, ITYRNO, TYKOLS, TYNUMV, NUMPOL,
     *   NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, TSYS, TANT,
     *   IERR)
C-----------------------------------------------------------------------
C! Does I/O to Tsys (TY) table opened by TYINI
C# EXT-util UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2013-2014
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
C   Does I/O to TSYS (TY) extention tables. Usually used after
C   setup by TYINI.
C    Inputs:
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open. Should have been returned by TABINI or
C                      TABINI.
C     ITYRNO       I   Next record number to read or write.
C     TYKOLS(*)    I   The column pointer array in order defined in
C                      PTYTAB.INC
C     TYNUMV(*)    I   Element count in each column.
C     NUMPOL       I   Number of polarizations (1 or 2)
C     NUMIF        I   Number of IFs in TY file
C    Input/output: (written to or read from Tsys file)
C     TIME         R   Time of record (Days)
C     TIMEI        R   Time interval covered by record (days)
C     SOURID       I   Source ID as defined in the SOURCE table.
C     ANTNO        I   Antenna number.
C     SUBA         I   Subarray number.
C     FREQID       I   Freqid #
C     TSYS(2,*)    R   System temperature in Kelvin (one per poln/IF)
C     TANT(2,*)    R   Antenna temperature in Kelvin (one per poln/IF)
C                         these set blank on read if value ~ 999
C    Output:
C     ITYRNO       I   Next record number.
C     IERR         I   Error code, 0=>OK else TABIO error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PTYTAB.INC'
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), ITYRNO, TYKOLS(MAXTYC), TYNUMV(MAXTYC),
     *   NUMPOL, NUMIF, SOURID, ANTNO, SUBA, FREQID, IERR
      REAL      TIME, TIMEI, TSYS(2,NUMIF), TANT(2,NUMIF)
C
      INTEGER   RECSIZ
C                                       RECSIZ = size of RECI
      PARAMETER (RECSIZ = 6 + 4*MAXIF)
      INTEGER   RECI(RECSIZ), IP, IP2, I
      LOGICAL   DOREAD, DOCLOS
      REAL      RECORD(RECSIZ)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECORD, RECI)
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
         RECORD(TYKOLS(TYRTIM)) = TIME
         RECORD(TYKOLS(TYRTMI)) = TIMEI
         RECI(TYKOLS(TYISID)) = SOURID
         RECI(TYKOLS(TYIANT)) = ANTNO
         RECI(TYKOLS(TYISUB)) = SUBA
         RECI(TYKOLS(TYIFQI)) = FREQID
         IP = TYKOLS(TYRTS1)
         IP2 = TYKOLS(TYRTA1)
         DO 20 I = 1,NUMIF
            RECORD(IP) = TSYS(1,I)
            RECORD(IP2) = TANT(1,I)
            IP = IP + 1
            IP2 = IP2 + 1
 20         CONTINUE
C                                       Second poln?
         IF (NUMPOL.GT.1) THEN
            IP = TYKOLS(TYRTS2)
            IP2 = TYKOLS(TYRTA2)
            DO 30 I = 1,NUMIF
               RECORD(IP) = TSYS(2,I)
               RECORD(IP2) = TANT(2,I)
               IP = IP + 1
               IP2 = IP2 + 1
 30            CONTINUE
            END IF
         END IF
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, ITYRNO, RECORD, BUFFER, IERR)
      ITYRNO = ITYRNO + 1
      IF (IERR.GT.0) GO TO 990
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
         TIME = RECORD(TYKOLS(TYRTIM))
         TIMEI = RECORD(TYKOLS(TYRTMI))
         SOURID = RECI(TYKOLS(TYISID))
         ANTNO = RECI(TYKOLS(TYIANT))
         SUBA = RECI(TYKOLS(TYISUB))
         FREQID = RECI(TYKOLS(TYIFQI))
         IP = TYKOLS(TYRTS1)
         IP2 = TYKOLS(TYRTA1)
         DO 70 I = 1,NUMIF
            TSYS(1,I) = FBLANK
            TANT(1,I) = FBLANK
            IF (TYNUMV(TYRTS1).GT.0) THEN
               IF (ABS(RECORD(IP)-999.).GT.0.5) TSYS(1,I) = RECORD(IP)
               END IF
            IF (TYNUMV(TYRTA1).GT.0) THEN
               IF (ABS(RECORD(IP2)-999.).GT.0.5) TANT(1,I) = RECORD(IP2)
               END IF
            IP = IP + 1
            IP2 = IP2 + 1
 70         CONTINUE
C                                       Second poln?
         IF (NUMPOL.GT.1) THEN
            IP = TYKOLS(TYRTS2)
            IP2 = TYKOLS(TYRTA2)
            DO 80 I = 1,NUMIF
               TSYS(2,I) = FBLANK
               TANT(2,I) = FBLANK
               IF (TYNUMV(TYRTS2).GT.0) THEN
                  IF (ABS(RECORD(IP)-999.).GT.0.5)
     *               TSYS(2,I) = RECORD(IP)
                  END IF
               IF (TYNUMV(TYRTA2).GT.0) THEN
                  IF (ABS(RECORD(IP2)-999.).GT.0.5)
     *               TANT(2,I) = RECORD(IP2)
                  END IF
               IP = IP + 1
               IP2 = IP2 + 1
 80            CONTINUE
            END IF
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, ITYRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABTY: TABIO ERROR',I3)
      END
