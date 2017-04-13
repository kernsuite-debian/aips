      SUBROUTINE TABBP (OPCODE, BUFFER, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *   NUMFRQ, NUMPOL, TIME, INTERV, SOURID, SUBA, ANT, BANDW, CHSHFT,
     *   FREQID, REFANT, WEIGHT, BNDPAS, IERR)
C-----------------------------------------------------------------------
C! Does I/O to bandpass (BP) table opened by BPINI
C# EXT-util UV Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2013, 2015
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
C   Does I/O to bandpass (BP) extention tables. Usually used after
C   setup by BPINI.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'READ' = read entry from table.
C                        'WRIT' = write entry in table.
C                        'CLOS' = close file, flush on write
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open. Should have been returned by
C                        BPINI or TABINI.
C      IBPRNO   I        Next entry number to read or write.
C      BPKOLS   I(MAXBPC) The column pointer array in order,
C                        TIME, INTERVAL, SOURID,
C                        SUBARRAY, ANTENNA,
C                        BANDW (of individual channel), CHN_SHIFT,
C                        FREQID,
C                        REFANT1, WT1, REAL1, IMAG1, for all channels
C                        Following used if 2 polarizations per IF
C                        REFANT2, WT2, REAL2, IMAG2.
C      BPNUMV   I(MAXBPC) Element count in each column.
C      NUMIF    I        Number of IF's
C      NUMFRQ   I        Number of chns
C      NUMPOL   I        Number of polarizations per IF.
C   Input/output: (written to or read from baseline file)
C      TIME    D        Center time of record (Days)
C      INTERV  R        Time interval of record (Days)
C      SOURID  I        Source ID number.
C      SUBA    I        Subarray number.
C      ANT     I        Antenna number.
C      BANDW   R        Bandwidth of an individual channel (Hz)
C      CHSHFT  D(m)     Frequency shift for each IF.
C      FREQID  I        Freq. id number
C      REFANT  I(2)     Reference Antenna; one for each poln
C      WEIGHT  R(m,P  ) Weights for complex bandpass
C                       m IFS; p polns
C      BNDPAS  C(n,m,p) Complex bandpass: m IFS; n channels; p polns
C   Output:
C      IBPRNO    I      Next solution number.
C      IERR      I      Error code, 0=>OK else TABIO error.
C                       Note: -1=> read but all polzn #1 flagged
C                             -2=> read but all polzn #2 flagged
C                             -3=> both flagged
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), IBPRNO, BPKOLS(MAXBPC), BPNUMV(MAXBPC),
     *   NUMIF, NUMFRQ, NUMPOL, SOURID, SUBA,  ANT, FREQID, REFANT(2),
     *   IERR
      DOUBLE PRECISION TIME, CHSHFT(MAXIF)
      REAL      INTERV, WEIGHT(*), BNDPAS(*), BANDW
C
      INTEGER   RECI(XBPRSZ), KOLS(MAXBPC), TIMKOL, SOUKOL, SUBKOL,
     *   ANTKL, INTKOL, BWKOL, CHSKOL, FRQKOL, REF1KL, REF2KL, RE1KL,
     *   IM1KL, RE2KL, IM2KL, LOOP, IFLP, NDATA, BDCNT1, BDCNT2, INDX,
     *   NNDX, WT1KL, WT2KL
      REAL      RECORD(XBPRSZ), WT
      DOUBLE PRECISION RECD(XBPRSZ/2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KOLS(1), TIMKOL), (KOLS(2), INTKOL),
     *   (KOLS(3), SOUKOL), (KOLS(4), SUBKOL), (KOLS(5), ANTKL),
     *   (KOLS(6), BWKOL),  (KOLS(7), CHSKOL), (KOLS(8), FRQKOL),
     *   (KOLS(9), REF1KL), (KOLS(10), WT1KL),  (KOLS(11), RE1KL),
     *   (KOLS(12), IM1KL), (KOLS(13), REF2KL), (KOLS(14), WT2KL),
     *   (KOLS(15),RE2KL), (KOLS(16),IM2KL)
      SAVE RECORD
      EQUIVALENCE (RECD, RECORD, RECI)
C-----------------------------------------------------------------------
      BDCNT1 = 0
      BDCNT2 = 0
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, IBPRNO, RECORD, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 980
         GO TO 999
         END IF
C                                       Check sizes
      NNDX = NUMPOL * NUMFRQ * NUMIF
      IF (NNDX.GT.MAXCIF) THEN
         IERR = 1
         MSGTXT = 'TABBP: RECORDS TOO BIG FOR BUFFERS'
         GO TO 985
         END IF
      IF (NNDX.LE.0) THEN
         IERR = 1
         MSGTXT = 'TABBP: ZERO SIZE DATA REQUESTED'
         GO TO 985
         END IF
      IF (NUMPOL.GT.2) THEN
         IERR = 1
         MSGTXT = 'TABBP: > 2 POLARIZATIONS IN BANDPASS TABLE!'
         GO TO 985
         END IF
C                                       Set pointers
      NDATA = MAXBPC
      CALL COPY (NDATA, BPKOLS, KOLS)
      NNDX = NUMFRQ * NUMIF
C                                       If write fill RECORD
      IF (OPCODE.NE.'READ') THEN
         RECD(TIMKOL) = TIME
         RECORD(INTKOL) = INTERV
         RECI(SOUKOL) = SOURID
         RECI(SUBKOL) = SUBA
         RECI(ANTKL) = ANT
         RECORD(BWKOL) = BANDW
         DO 10 IFLP = 1, NUMIF
            RECD(CHSKOL) = CHSHFT(IFLP)
            CHSKOL = CHSKOL + 1
            IF (WT1KL.GT.0) THEN
               RECORD(WT1KL) = WEIGHT(IFLP)
               WT1KL = WT1KL + 1
               END IF
            IF (NUMPOL.GT.1) THEN
               IF (WT2KL.GT.0) THEN
                  RECORD(WT2KL) = WEIGHT(IFLP+NUMIF)
                  WT2KL = WT2KL + 1
                  END IF
               END IF
 10         CONTINUE
         IF (FRQKOL.GT.0) RECI(FRQKOL) = FREQID
         RECI(REF1KL) = REFANT(1)
C                                       First polarization
         INDX = 0
         DO 20 LOOP = 1,NNDX
            INDX = INDX + 1
            RECORD(RE1KL) = BNDPAS(INDX)
            INDX = INDX + 1
            RECORD(IM1KL) = BNDPAS(INDX)
            RE1KL = RE1KL + 1
            IM1KL = IM1KL + 1
 20         CONTINUE
C                                       Second polarization
         IF (NUMPOL.GT.1) THEN
            RECI(REF2KL) = REFANT(2)
            DO 30 LOOP = 1,NNDX
               INDX = INDX + 1
               RECORD(RE2KL) = BNDPAS(INDX)
               INDX = INDX + 1
               RECORD(IM2KL) = BNDPAS(INDX)
               RE2KL = RE2KL + 1
               IM2KL = IM2KL + 1
 30            CONTINUE
            END IF
         END IF
C                                       Process record.
      CALL TABIO (OPCODE, 0, IBPRNO, RECORD, BUFFER, IERR)
      IBPRNO = IBPRNO + 1
      IF (IERR.GT.0) GO TO 980
C                                       If READ pick data from RECORD.
      IF (OPCODE.EQ.'READ') THEN
         TIME   = RECD(TIMKOL)
         INTERV = RECORD(INTKOL)
         SOURID = RECI(SOUKOL)
         SUBA   = RECI(SUBKOL)
         ANT    = RECI(ANTKL)
         BANDW  = RECORD(BWKOL)
         WT = INTERV * 24. * 60. * 6.
         DO 70 IFLP = 1,NUMIF
            CHSHFT(IFLP) = RECD(CHSKOL)
            CHSKOL = CHSKOL + 1
            IF (WT1KL.GT.0) THEN
               WEIGHT(IFLP) = RECORD(WT1KL)
               WT1KL = WT1KL + 1
            ELSE
               WEIGHT(IFLP) = WT
               END IF
            IF (NUMPOL.GT.1) THEN
               IF (WT2KL.GT.0) THEN
                  WEIGHT(IFLP+NUMIF) = RECORD(WT2KL)
                  WT2KL = WT2KL + 1
               ELSE
                  WEIGHT(IFLP+NUMIF) = WT
                  END IF
               END IF
 70         CONTINUE
         IF (FRQKOL.LE.0) THEN
            FREQID = 1
         ELSE
            FREQID = RECI(FRQKOL)
            END IF
         REFANT(1) = RECI(REF1KL)
C                                       First polarization
         INDX = 0
         DO 80 LOOP = 1,NNDX
            INDX = INDX + 1
            BNDPAS(INDX) = RECORD(RE1KL)
            INDX = INDX + 1
            BNDPAS(INDX) = RECORD(IM1KL)
            IF ((RECORD(RE1KL).EQ.FBLANK) .OR.
     *         (RECORD(IM1KL).EQ.FBLANK)) BDCNT1 = BDCNT1 + 1
            RE1KL = RE1KL + 1
            IM1KL = IM1KL + 1
 80         CONTINUE
C                                       Second polarization
         IF (NUMPOL.GT.1) THEN
            REFANT(2) = RECI(REF2KL)
            DO 90 LOOP = 1,NNDX
               INDX = INDX + 1
               BNDPAS(INDX) = RECORD(RE2KL)
               INDX = INDX + 1
               BNDPAS(INDX) = RECORD(IM2KL)
               IF ((RECORD(RE2KL).EQ.FBLANK) .OR.
     *             (RECORD(IM2KL).EQ.FBLANK)) BDCNT2 = BDCNT2 + 1
               RE2KL = RE2KL + 1
               IM2KL = IM2KL + 1
 90            CONTINUE
            END IF
         END IF
      IF (BDCNT1.EQ.(NUMIF*NUMFRQ)) IERR = -1
      IF (BDCNT2.EQ.(NUMIF*NUMFRQ)) IERR = -2
      IF ((BDCNT1.EQ.(NUMIF*NUMFRQ)) .AND.
     *   (BDCNT2.EQ.(NUMIF*NUMFRQ))) IERR = -3
      GO TO 999
C                                       Error
 980  WRITE (MSGTXT,1980) IERR
C
 985  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('TABBP: TABIO ERROR',I3)
      END
