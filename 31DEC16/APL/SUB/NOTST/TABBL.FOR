      SUBROUTINE TABBL (OPCODE, BUFFER, IBLRNO, BLKOLS, BLNUMV,
     *   NUMPOL, TIME, SOURID, SUBA, ANT1, ANT2, FREQID,
     *   FACMUL, FACADD, IERR)
C-----------------------------------------------------------------------
C! Do IO to Baseline (BL) table after setup by BLINI.
C# EXT-util UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2013, 2015
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
C   Does I/O to baseline (BL) extention tables. Usually used after
C   setup by BLINI.
C    Inputs:
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     BUFFER(1024) I   I/O buffer and related storage, also defines file
C                      if open. Should have been returned by BLINI or
C                      TABINI.
C     IBLRNO       I   Next entry number to read or write.
C     BLKOLS(MAXBLC)   I   The column pointer array in order, TIME,
C                      SOURID, SUBARRAY, ANTENNA1, ANTENNA2, FREQID,
C                      REALM1, IMAGM1, REALA1, IMAGA1,
C                      Following used if 2 polarizations per IF
C                      REALM2, IMAGM2, REALA2, IMAGA2.
C     BLNUMV(MAXBLC)   I   Element count in each column.
C     NUMPOL       I   Number of polarizations per IF.
C    Input/output: (written to or read from baseline file)
C     TIME         R   Center time of record (Days)
C     SOURID       I   Source ID number.
C     SUBA         I   Subarray number.
C     ANT1         I   First antenna number.
C     ANT2         I   Second antenna number.
C     FREQID       I   Freqid #
C     FACMUL(2,2,m)R   Multiplicative correction,  m IFs
C                      second dimension is polarization,
C                      (1,*,*) = real, (2,*,*) = imag.
C     FACADD(2,2,m)R   Additive correction, m IFs
C    Output:
C     IBLRNO       I   Next solution number.
C     IERR         I   Error code, 0=>OK else TABIO error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(1024), BLKOLS(MAXBLC), BLNUMV(MAXBLC), NUMPOL,
     *   IERR, SOURID, ANT1, ANT2, SUBA, FREQID, RECI(1), KOLS(MAXBLC),
     *   NUMIF, TIMKOL, SOUKOL, SUBKOL, ANT1KL, ANT2KL, FRQKOL,
     *   REM1KL, IMM1KL, REA1KL, IMA1KL,
     *   REM2KL, IMM2KL, REA2KL, IMA2KL
      LOGICAL   DOREAD, DOCLOS
      INTEGER   IBLRNO, LOOP, NDATA, TYPE
      REAL      RECORD(6+8*MAXIF),
     *   TIME, FACMUL(2,2,MAXIF), FACADD(2,2,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (KOLS(1), TIMKOL), (KOLS(2), SOUKOL),
     *   (KOLS(3), SUBKOL), (KOLS(4),ANT1KL), (KOLS(5), ANT2KL),
     *   (KOLS(6), REM1KL), (KOLS(7),IMM1KL),
     *   (KOLS(8), REA1KL), (KOLS(9), IMA1KL),
     *   (KOLS(10), REM2KL), (KOLS(11), IMM2KL),
     *   (KOLS(12), REA2KL), (KOLS(MAXBLC),IMA2KL)
      EQUIVALENCE (RECORD, RECI)
C-----------------------------------------------------------------------
      NUMIF = BLNUMV(7)
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       Set pointers
      NDATA = MAXBLC
      CALL COPY (NDATA, BLKOLS, KOLS)
C                                       Determine if old/new table
      TYPE = 1
      IF ((BUFFER(10).EQ.14) .OR. (BUFFER(10).EQ.10)) TYPE = 2
C                                       If type=2, set up different
C                                       column id's
      IF (TYPE.EQ.2) THEN
         TIMKOL = BLKOLS(1)
         SOUKOL = BLKOLS(2)
         SUBKOL = BLKOLS(3)
         ANT1KL = BLKOLS(4)
         ANT2KL = BLKOLS(5)
         FRQKOL = BLKOLS(6)
         REM1KL = BLKOLS(7)
         IMM1KL = BLKOLS(8)
         REA1KL = BLKOLS(9)
         IMA1KL = BLKOLS(10)
         REM2KL = BLKOLS(11)
         IMM2KL = BLKOLS(12)
         REA2KL = BLKOLS(13)
         IMA2KL = BLKOLS(14)
         END IF
C                                       If write fill RECORD
      IF (DOREAD) GO TO 50
         RECORD(TIMKOL) = TIME
         RECI(SOUKOL) = SOURID
         RECI(SUBKOL) = SUBA
         RECI(ANT1KL) = ANT1
         RECI(ANT2KL) = ANT2
         IF (FRQKOL.GT.0) RECI(FRQKOL) = FREQID
C                                       First polarization
         DO 10 LOOP = 1,NUMIF
            RECORD(REM1KL) = FACMUL(1,1,LOOP)
            RECORD(IMM1KL) = FACMUL(2,1,LOOP)
            RECORD(REA1KL) = FACADD(1,1,LOOP)
            RECORD(IMA1KL) = FACADD(2,1,LOOP)
            IF (LOOP.GE.NUMIF) GO TO 10
               REM1KL = REM1KL + 1
               IMM1KL = IMM1KL + 1
               REA1KL = REA1KL + 1
               IMA1KL = IMA1KL + 1
 10         CONTINUE
         IF (NUMPOL.LT.2) GO TO 50
C                                       Two polarizations
            DO 20 LOOP = 1,NUMIF
               RECORD(REM2KL) = FACMUL(1,2,LOOP)
               RECORD(IMM2KL) = FACMUL(2,2,LOOP)
               RECORD(REA2KL) = FACADD(1,2,LOOP)
               RECORD(IMA2KL) = FACADD(2,2,LOOP)
               IF (LOOP.GE.NUMIF) GO TO 20
                  REM2KL = REM2KL + 1
                  IMM2KL = IMM2KL + 1
                  REA2KL = REA2KL + 1
                  IMA2KL = IMA2KL + 1
 20         CONTINUE
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, IBLRNO, RECORD, BUFFER, IERR)
      IBLRNO = IBLRNO + 1
      IF (IERR.GT.0) GO TO 990
C                                       If READ pick data from RECORD.
      IF (.NOT.DOREAD) GO TO 999
         TIME = RECORD(TIMKOL)
         SOURID = RECI(SOUKOL)
         SUBA = RECI(SUBKOL)
         ANT1 = RECI(ANT1KL)
         ANT2 = RECI(ANT2KL)
         IF ((TYPE.EQ.1) .OR. (FRQKOL.LE.0)) THEN
            FREQID = 1
         ELSE IF (TYPE.EQ.2) THEN
            FREQID = RECI(FRQKOL)
            END IF
C                                       First polarization
         DO 60 LOOP = 1,NUMIF
            FACMUL(1,1,LOOP) = RECORD(REM1KL)
            FACMUL(2,1,LOOP) = RECORD(IMM1KL)
            FACADD(1,1,LOOP) = RECORD(REA1KL)
            FACADD(2,1,LOOP) = RECORD(IMA1KL)
            IF (LOOP.GE.NUMIF) GO TO 60
               REM1KL = REM1KL + 1
               IMM1KL = IMM1KL + 1
               REA1KL = REA1KL + 1
               IMA1KL = IMA1KL + 1
 60         CONTINUE
         IF (NUMPOL.GE.2) THEN
C                                       Two polarizations
            DO 80 LOOP = 1,NUMIF
               FACMUL(1,2,LOOP) = RECORD(REM2KL)
               FACMUL(2,2,LOOP) = RECORD(IMM2KL)
               FACADD(1,2,LOOP) = RECORD(REA2KL)
               FACADD(2,2,LOOP) = RECORD(IMA2KL)
               IF (LOOP.LT.NUMIF) THEN
                  REM2KL = REM2KL + 1
                  IMM2KL = IMM2KL + 1
                  REA2KL = REA2KL + 1
                  IMA2KL = IMA2KL + 1
                  END IF
 80         CONTINUE
            END IF
         GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, IBLRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABBL: TABIO ERROR',I3)
      END
