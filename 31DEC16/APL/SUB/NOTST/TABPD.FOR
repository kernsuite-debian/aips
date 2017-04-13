      SUBROUTINE TABPD (OPCODE, BUFFER, IPDRNO, PDKOLS, PDNUMV, NUMIF,
     *   NUMFRQ, NUMPOL, ANT, SUBA, FREQID, REFANT, PHDIFF, DTERMS,
     *   IERR)
C-----------------------------------------------------------------------
C! Does I/O to antenna polarization spectrum (PD) table opened by PDINI
C# EXT-util UV Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 2010, 2013, 2015
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
C   Does I/O to antenna polarization spectrum (PD) extention tables.
C   Usually used after setup by PDINI.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'READ' = read entry from table.
C                        'WRIT' = write entry in table.
C                        'CLOS' = close file, flush on write
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open. Should have been returned by
C                        PDINI or TABINI.
C      IPDRNO   I        Next entry number to read or write.
C      PDKOLS   I(MAXPDC) The column pointer array in order,
C                        ANTENNA, SUBARRAY, FREQID, REFANT,
C                        P_DIFF, REAL1, IMAG1,
C                        Following used if 2 polarizations per IF
C                        REAL2, IMAG2.
C      PDNUMV   I(MAXPDC) Element count in each column.
C      NUMIF    I        Number of IF's
C      NUMFRQ   I        Number of chns
C      NUMPOL   I        Number of polarizations per IF.
C   Input/output: (written to or read from baseline file)
C      ANT     I        Antenna number.
C      SUBA    I        Subarray number.
C      FREQID  I        Freq. id number
C      REFANT  I(2)     Reference Antenna; one for each poln
C      P_DIFF  R(n,m)   R-L phase difference n channels, m IFS
C      DTERMS  C(n,m,p) Complex bandpass: n channels; m IFS; p polns
C   Output:
C      IPDRNO    I      Next solution number.
C      IERR      I      Error code, 0=>OK else TABIO error.
C                       Note: -1=> read but all polzn #1 flagged
C                             -2=> read but all polzn #2 flagged
C                             -3=> both flagged
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXPDC, XPDRSZ
      PARAMETER (MAXPDC = 9)
      PARAMETER (XPDRSZ = 2 * MAXCIF + 4 + MAXIF)
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), IPDRNO, PDKOLS(MAXPDC), PDNUMV(MAXPDC),
     *   NUMIF, NUMFRQ, NUMPOL, SUBA, ANT, FREQID, REFANT(2), IERR
      REAL      PHDIFF(*), DTERMS(*)
C
      INTEGER   RECI(XPDRSZ), KOLS(MAXPDC), SUBKOL, ANTKL, FRQKOL,
     *   REF1KL, RE1KL, IM1KL, RE2KL, IM2KL, PDFKL, LOOP, NDATA,
     *   BDCNT1, BDCNT2, INDX, NNDX
      REAL      RECR(XPDRSZ)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KOLS(1), ANTKL), (KOLS(2), SUBKOL),
     *   (KOLS(3), FRQKOL), (KOLS(4), REF1KL), (KOLS(5), PDFKL),
     *   (KOLS(6), RE1KL), (KOLS(7), IM1KL), (KOLS(8), RE2KL),
     *   (KOLS(9), IM2KL)
      SAVE RECR
      EQUIVALENCE (RECR, RECI)
C-----------------------------------------------------------------------
      BDCNT1 = 0
      BDCNT2 = 0
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, IPDRNO, RECR, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 980
         GO TO 999
         END IF
C                                       Check sizes
      NNDX = NUMPOL * NUMFRQ * NUMIF
      IF (NNDX.GT.MAXCIF) THEN
         IERR = 1
         MSGTXT = 'TABPD: RECRS TOO BIG FOR BUFFERS'
         GO TO 985
         END IF
      IF (NNDX.LE.0) THEN
         IERR = 1
         MSGTXT = 'TABPD: ZERO SIZE DATA REQUESTED'
         GO TO 985
         END IF
      IF (NUMPOL.GT.2) THEN
         IERR = 1
         MSGTXT = 'TABPD: > 2 POLARIZATIONS IN POL SPECTRUM TABLE!'
         GO TO 985
         END IF
C                                       Set pointers
      NDATA = MAXPDC
      CALL COPY (NDATA, PDKOLS, KOLS)
      NNDX = NUMFRQ * NUMIF
C                                       If write fill RECR
      IF (OPCODE.NE.'READ') THEN
         RECI(ANTKL) = ANT
         RECI(SUBKOL) = SUBA
         IF (FRQKOL.GT.0) RECI(FRQKOL) = FREQID
         RECI(REF1KL) = REFANT(1)
C                                       First polarization
         INDX = 0
         DO 20 LOOP = 1,NNDX
            INDX = INDX + 1
            RECR(RE1KL) = DTERMS(INDX)
            INDX = INDX + 1
            RECR(IM1KL) = DTERMS(INDX)
            RE1KL = RE1KL + 1
            IM1KL = IM1KL + 1
            IF (PDNUMV(5).GT.0) THEN
               RECR(PDFKL) = PHDIFF(LOOP)
               PDFKL = PDFKL + 1
               END IF
 20         CONTINUE
C                                       Second polarization
         IF (NUMPOL.GT.1) THEN
            DO 30 LOOP = 1,NNDX
               INDX = INDX + 1
               RECR(RE2KL) = DTERMS(INDX)
               INDX = INDX + 1
               RECR(IM2KL) = DTERMS(INDX)
               RE2KL = RE2KL + 1
               IM2KL = IM2KL + 1
 30            CONTINUE
            END IF
         END IF
C                                       Process record.
      CALL TABIO (OPCODE, 0, IPDRNO, RECR, BUFFER, IERR)
      IPDRNO = IPDRNO + 1
      IF (IERR.GT.0) GO TO 980
C                                       If READ pick data from RECR.
      IF (OPCODE.EQ.'READ') THEN
         ANT    = RECI(ANTKL)
         SUBA   = RECI(SUBKOL)
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
            DTERMS(INDX) = RECR(RE1KL)
            INDX = INDX + 1
            DTERMS(INDX) = RECR(IM1KL)
            IF ((RECR(RE1KL).EQ.FBLANK) .OR.
     *         (RECR(IM1KL).EQ.FBLANK)) BDCNT1 = BDCNT1 + 1
            RE1KL = RE1KL + 1
            IM1KL = IM1KL + 1
            IF (PDNUMV(5).GT.0) THEN
               PHDIFF(LOOP) = RECR(PDFKL)
               PDFKL = PDFKL + 1
               END IF
 80         CONTINUE
C                                       Second polarization
         IF (NUMPOL.GT.1) THEN
            DO 90 LOOP = 1,NNDX
               INDX = INDX + 1
               DTERMS(INDX) = RECR(RE2KL)
               INDX = INDX + 1
               DTERMS(INDX) = RECR(IM2KL)
               IF ((RECR(RE2KL).EQ.FBLANK) .OR.
     *             (RECR(IM2KL).EQ.FBLANK)) BDCNT2 = BDCNT2 + 1
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
 1980 FORMAT ('TABPD: TABIO ERROR',I3)
      END
