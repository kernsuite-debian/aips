      SUBROUTINE TABBD (OPCODE, BUFFER, IBDRNO, BDKOLS, BDNUMV, NUMIF,
     *   NUMFRQ, NUMPOL, TIME, SOURID, SUBA, ANT1, ANT2, FREQID,
     *   BNDPAS, IERR)
C-----------------------------------------------------------------------
C! Does I/O to BLCAL bandpass (BD) table opened by BDINI
C# EXT-util UV Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 2009, 2013, 2015
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
C   Does I/O to bandpass (BD) extention tables. Usually used after
C   setup by BDINI.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'READ' = read entry from table.
C                        'WRIT' = write entry in table.
C                        'CLOS' = close file, flush on write
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open. Should have been returned by
C                        BDINI or TABINI.
C      IBDRNO   I        Next entry number to read or write.
C      BDKOLS   I(MAXBDC) The column pointer array in order,
C                        TIME, SOURID, SUBARRAY, ANTENNA1, ANTENNA2,
C                        FREQID, REAL1, IMAG1,
C                        Following used if 2 polarizations per IF
C                        REAL2, IMAG2.
C      BDNUMV   I(MAXBDC) Element count in each column.
C      NUMIF    I        Number of IF's
C      NUMFRQ   I        Number of chns
C      NUMPOL   I        Number of polarizations per IF.
C   Input/output: (written to or read from baseline file)
C      TIME    R        Center time of record (Days)
C      SOURID  I        Source ID number.
C      SUBA    I        Subarray number.
C      ANT     I        Antenna number.
C      FREQID  I        Freq. id number
C      BNDPAS  C(n,m,p) Complex bandpass: m IFS; n channels; p polns
C   Output:
C      IBDRNO    I      Next solution number.
C      IERR      I      Error code, 0=>OK else TABIO error.
C                       Note: -1=> read but all polzn #1 flagged
C                             -2=> read but all polzn #2 flagged
C                             -3=> both flagged
C-----------------------------------------------------------------------
      INTEGER   MAXBDC
      PARAMETER (MAXBDC = 10)
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), IBDRNO, BDKOLS(MAXBDC), BDNUMV(MAXBDC),
     *   NUMIF, NUMFRQ, NUMPOL, SOURID, SUBA, ANT1, ANT2, FREQID, IERR
      REAL      TIME, BNDPAS(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   XBDRSZ
      PARAMETER (XBDRSZ = 6 + 2*MAXCIF)
C
      INTEGER   RECI(XBDRSZ), KOLS(MAXBDC), TIMKOL, SOUKOL, SUBKOL,
     *   ANT1KL, ANT2KL, FRQKOL, RE1KL, IM1KL, RE2KL, IM2KL, LOOP,
     *   NDATA, BDCNT1, BDCNT2, INDX, NNDX
      REAL      RECORD(XBDRSZ)
      DOUBLE PRECISION RECD(XBDRSZ/2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE RECORD
      EQUIVALENCE (KOLS(1), TIMKOL), (KOLS(2), SOUKOL),
     *   (KOLS(3), SUBKOL), (KOLS(4), ANT1KL), (KOLS(5), ANT2KL),
     *   (KOLS(6), FRQKOL), (KOLS(7), RE1KL), (KOLS(8), IM1KL),
     *   (KOLS(9),RE2KL), (KOLS(10),IM2KL)
      EQUIVALENCE (RECD, RECORD, RECI)
C-----------------------------------------------------------------------
      BDCNT1 = 0
      BDCNT2 = 0
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, IBDRNO, RECORD, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 980
         GO TO 999
         END IF
C                                       Check sizes
      NNDX = NUMPOL * NUMFRQ * NUMIF
      IF (NNDX.GT.MAXCIF) THEN
         IERR = 1
         MSGTXT = 'TABBD: RECORDS TOO BIG FOR BUFFERS'
         GO TO 985
         END IF
      IF (NNDX.LE.0) THEN
         IERR = 1
         MSGTXT = 'TABBD: ZERO SIZE DATA REQUESTED'
         GO TO 985
         END IF
      IF (NUMPOL.GT.2) THEN
         IERR = 1
         MSGTXT = 'TABBD: > 2 POLARIZATIONS IN BANDPASS TABLE!'
         GO TO 985
         END IF
C                                       Set pointers
      NDATA = MAXBDC
      CALL COPY (NDATA, BDKOLS, KOLS)
      NNDX = NUMFRQ * NUMIF
C                                       If write fill RECORD
      IF (OPCODE.NE.'READ') THEN
         RECORD(TIMKOL) = TIME
         RECI(SOUKOL) = SOURID
         RECI(SUBKOL) = SUBA
         RECI(ANT1KL) = ANT1
         RECI(ANT2KL) = ANT2
         IF (FRQKOL.GT.0) RECI(FRQKOL) = FREQID
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
      CALL TABIO (OPCODE, 0, IBDRNO, RECORD, BUFFER, IERR)
      IBDRNO = IBDRNO + 1
      IF (IERR.GT.0) GO TO 980
C                                       If READ pick data from RECORD.
      IF (OPCODE.EQ.'READ') THEN
         TIME   = RECORD(TIMKOL)
         SOURID = RECI(SOUKOL)
         SUBA   = RECI(SUBKOL)
         ANT1    = RECI(ANT1KL)
         ANT2    = RECI(ANT2KL)
         IF (FRQKOL.LE.0) THEN
            FREQID = 1
         ELSE
            FREQID = RECI(FRQKOL)
            END IF
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
 1980 FORMAT ('TABBD: TABIO ERROR',I3)
      END
