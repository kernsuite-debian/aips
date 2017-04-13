      SUBROUTINE TABGA (OPCODE, BUFFER, IGARNO, GAKOLS, GANUMV,
     *   NUMPOL, TIME, TIMEI, SOURID, ANTNO, SUBA, NODENO,
     *   CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C-----------------------------------------------------------------------
C! Does I/O to GAIN (GA) table opened by GAINI
C# EXT-util UV Calibration Obselete
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
C   Does I/O to GAIN extention tables. Usually used after setup by
C   GAINI.
C    Inputs:
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open. Should have been returned by TABINI or
C                      TABINI.
C     IGARNO       I   Next scan number to read or write.
C     GAKOLS(18)   I   The column pointer array in order, TIME,
C                      TIME INT., SOURCE ID., ANTENNA NO., SUBARRAY,
C                      NODE NO.,
C                      REAL1, IMAG1, DELAY1, RATE1, WEIGHT1, REFANT 1,
C                      Following used if 2 polarizations per IF
C                      REAL2, IMAG2, DELAY2, RATE2, WEIGHT2, REFANT 2,
C     GANUMV(18)   I   Element count in each column.
C     NUMPOL       I   Number of polarizations per IF.
C    Input/output: (written to or read from GAIN file)
C     TIME         D   Center time of GAIN record (Days)
C     TIMEI        R   Time interval covered by record (days)
C     SOURID       I   Source ID as defined in the SOURCE table.
C     ANTNO        I   Antenna number.
C     SUBA         I   Subarray number.
C     NODENO       I   Interpolation node number
C     CREAL(2,*)   R   Real part of the complex gain, 1 /Poln/IF
C     CIMAG(2,*)   R   Imag part of the complex gain, 1 /Poln/IF
C     DELAY(2,*)   R   Residual group delay (sec), 1 /Poln/IF
C     RATE(2,*)    R   Residual fringe rate (Hz), 1 /Poln/IF
C     WEIGHT(2,*)  R   Weight of solution, 1 /Poln/IF
C     REFA(2,*)    R   Ref. ant. of solution, 1 /Poln/IF
C    Output:
C     IGARNO       I   Next GAIN number.
C     IERR         I   Error code, 0=>OK else TABIO error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   MAXIF
      PARAMETER (MAXIF=15)
      INTEGER   BUFFER(512), GAKOLS(18), GANUMV(18), NUMPOL, IERR,
     *   SOURID, ANTNO, SUBA, NODENO, REFA(2,MAXIF),
     *   RECI(1), KOLS(18), NUMIF,
     *   TIMKOL, INTKOL, SOUKOL, ANTKOL, SUBKOL, NODKOL,
     *   RE1KOL, IM1KOL, DL1KOL, RA1KOL, WT1KOL, RF1KOL,
     *   RE2KOL, IM2KOL, DL2KOL, RA2KOL, WT2KOL, RF2KOL
      LOGICAL   DOREAD, DOCLOS
      INTEGER   IGARNO,  LOOP
      REAL      RECORD(7+12*MAXIF),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF), TIMEI
      DOUBLE PRECISION    TIME, RECD(1)
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (KOLS(1), TIMKOL), (KOLS(2), INTKOL),
     *   (KOLS(3), SOUKOL), (KOLS(4),ANTKOL), (KOLS(5), SUBKOL),
     *   (KOLS(6), NODKOL),
     *   (KOLS(7), RE1KOL), (KOLS(8),IM1KOL),
     *   (KOLS(9), DL1KOL), (KOLS(10), RA1KOL),
     *   (KOLS(11), WT1KOL), (KOLS(12), RF1KOL),
     *   (KOLS(13), RE2KOL), (KOLS(14),IM2KOL),
     *   (KOLS(15), DL2KOL), (KOLS(16), RA2KOL),
     *   (KOLS(17), WT2KOL), (KOLS(18), RF2KOL)
      EQUIVALENCE (RECD, RECORD, RECI)
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       Set pointers
      CALL COPY (18, GAKOLS, KOLS)
C                                       If write fill RECORD
      IF (DOREAD) GO TO 50
         RECORD(TIMKOL) = TIME
         RECORD(INTKOL) = TIMEI
         RECI(SOUKOL) = SOURID
         RECI(ANTKOL) = ANTNO
         RECI(SUBKOL) = SUBA
         RECI(NODKOL) = NODENO
C                                       First polarization
         NUMIF = GANUMV(7)
         DO 10 LOOP = 1,NUMIF
            RECORD(RE1KOL) = CREAL(1,LOOP)
            RECORD(IM1KOL) = CIMAG(1,LOOP)
            RECORD(DL1KOL) = DELAY(1,LOOP)
            RECORD(RA1KOL) = RATE(1,LOOP)
            RECORD(WT1KOL) = WEIGHT(1,LOOP)
            RECI(RF1KOL) = REFA(1,LOOP)
            IF (LOOP.GE.NUMIF) GO TO 10
               RE1KOL = RE1KOL + 1
               IM1KOL = IM1KOL + 1
               RA1KOL = RA1KOL + 1
               DL1KOL = DL1KOL + 1
               WT1KOL = WT1KOL + 1
               RF1KOL = RF1KOL + 1
 10         CONTINUE
         IF (NUMPOL.LT.2) GO TO 50
C                                       Two polarizations
            DO 20 LOOP = 1,NUMIF
               RECORD(RE2KOL) = CREAL(2,LOOP)
               RECORD(IM2KOL) = CIMAG(2,LOOP)
               RECORD(DL2KOL) = DELAY(2,LOOP)
               RECORD(RA2KOL) = RATE(2,LOOP)
               RECORD(WT2KOL) = WEIGHT(2,LOOP)
               RECI(RF2KOL) = REFA(2,LOOP)
               IF (LOOP.GE.NUMIF) GO TO 20
                  RE2KOL = RE2KOL + 1
                  IM2KOL = IM2KOL + 1
                  RA2KOL = RA2KOL + 1
                  DL2KOL = DL2KOL + 1
                  WT2KOL = WT2KOL + 1
                  RF2KOL = RF2KOL + 1
 20         CONTINUE
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, IGARNO, RECORD, BUFFER, IERR)
      IGARNO = IGARNO + 1
      IF (IERR.GT.0) GO TO 990
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
         TIME = RECORD(TIMKOL)
         TIMEI = RECORD(INTKOL)
         SOURID = RECI(SOUKOL)
         ANTNO = RECI(ANTKOL)
         SUBA = RECI(SUBKOL)
         NODENO = RECI(NODKOL)
C                                       First polarization
         DO 60 LOOP = 1,NUMIF
            CREAL(1,LOOP) = RECORD(RE1KOL)
            CIMAG(1,LOOP) = RECORD(IM1KOL)
            DELAY(1,LOOP) = RECORD(DL1KOL)
            RATE(1,LOOP) = RECORD(RA1KOL)
            WEIGHT(1,LOOP) = RECORD(WT1KOL)
            REFA(1,LOOP) = RECI(RF1KOL)
            IF (LOOP.LT.NUMIF) THEN
               RE1KOL = RE1KOL + 1
               IM1KOL = IM1KOL + 1
               RA1KOL = RA1KOL + 1
               DL1KOL = DL1KOL + 1
               WT1KOL = WT1KOL + 1
               RF1KOL = RF1KOL + 1
               END IF
 60         CONTINUE
C                                       Two polarizations
         IF (NUMPOL.GE.2) THEN
            DO 80 LOOP = 1,NUMIF
               CREAL(2,LOOP) = RECORD(RE2KOL)
               CIMAG(2,LOOP) = RECORD(IM2KOL)
               DELAY(2,LOOP) = RECORD(DL2KOL)
               RATE(2,LOOP) = RECORD(RA2KOL)
               WEIGHT(2,LOOP) = RECORD(WT2KOL)
               REFA(2,LOOP) = RECI(RF2KOL)
               IF (LOOP.LT.NUMIF) THEN
                  RE2KOL = RE2KOL + 1
                  IM2KOL = IM2KOL + 1
                  RA2KOL = RA2KOL + 1
                  DL2KOL = DL2KOL + 1
                  WT2KOL = WT2KOL + 1
                  RF2KOL = RF2KOL + 1
                  END IF
 80            CONTINUE
            END IF
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, IGARNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABGA: TABIO ERROR',I3)
      END
