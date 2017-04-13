      SUBROUTINE TABSN (OPCODE, BUFFER, ISNRNO, SNKOLS, SNNUMV, NUMPOL,
     *   TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO, MBDELY,
     *   DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C-----------------------------------------------------------------------
C! Does I/O to Solution (SN) table opened by SNINI
C# EXT-util UV Calibration
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
C   Does I/O to solution (SN) extention tables. Usually used after
C   setup by SNINI.
C   Inputs:
C      OPCODE   C*4         Operation code:
C                           'READ' = read entry from table.
C                           'WRIT' = write entry in table.
C                           'CLOS' = close file, flush on write
C      BUFFER   I(512)      I/O buffer and related storage, also
C                           defines file if open. Should have been
C                           returned by TABINI or TABINI.
C      ISNRNO   I           Next scan number to read or write.
C      SNKOLS   I(MAXSNC)   The column pointer array in order defined
C                           by the parameters in PSNTAB.INC.
C      SNNUMV   I(MAXSNC)   Element count in each column.
C      NUMPOL   I           Number of polarizations per IF.
C   Input/output: (written to or read from solution file)
C      TIME     D           Center time of solution record (Days)
C      TIMEI    R           Time interval covered by record (days)
C      SOURID   I            Source ID as defined in the SOURCE table.
C      ANTNO    I           Antenna number.
C      SUBA     I           Subarray number.
C      FREQID   I           Freqid #
C      IFR      R           Ionospheric Faraday Rotation (rad/m**2)
C      NODENO   I           Interpolation node number
C      MBDELY   R(2)        Multi band delays (sec) 1/Poln
C      DISP     R(2)        Dispersive delay (sec/m^2 scales as lambda^2)
C                           1/poln
C      DDISP    R(2)        Time derivative of DISP (sec/sec/m^2)
C      CREAL    R(2,*)      Real part of the complex gain, 1 /Poln/IF
C      CIMAG    R(2,*)      Imag part of the complex gain, 1 /Poln/IF
C      DELAY    R(2,*)      Residual group delay (sec), 1 /Poln/IF
C      RATE     R(2,*)      Residual fringe rate (Hz), 1 /Poln/IF
C      WEIGHT   R(2,*)      Weight of solution, 1 /Poln/IF
C      REFA     R(2,*)      Ref. ant. of solution, 1 /Poln/IF
C   Output:
C      ISNRNO   I           Next solution number.
C      IERR     I           Error code, 0=>OK else TABIO error.
C                           Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), ISNRNO, SNKOLS(MAXSNC), SNNUMV(MAXSNC),
     *   NUMPOL, SOURID, ANTNO, SUBA, FREQID, NODENO, REFA(2,MAXIF),
     *   IERR
      DOUBLE PRECISION TIME
      REAL      TIMEI, IFR, MBDELY(2), DISP(2), DDISP(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF)
C
      INCLUDE 'INCS:PSNTAB.INC'
      INTEGER   RECI(XCLRSZ), NUMIF,  LOOP,
     *   TIMKOL, INTKOL, SOUKOL, ANTKOL, SUBKOL, FRQKOL, IFRKOL, NODKOL,
     *   MB1KOL, DS1KOL, DD1KOL, RE1KOL, IM1KOL, DL1KOL, RA1KOL, WT1KOL,
     *   RF1KOL, MB2KOL, DS2KOL, DD2KOL, RE2KOL, IM2KOL, DL2KOL, RA2KOL,
     *   WT2KOL, RF2KOL, NDATA
      LOGICAL   DOREAD, DOCLOS
      REAL      RECORD(XCLRSZ)
      DOUBLE PRECISION RECD(XCLRSZ/2)
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECD, RECORD, RECI)
      DATA RECORD /XCLRSZ*0.0D0/
C-----------------------------------------------------------------------
      NUMIF = SNNUMV(SNRRE1)
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
      NDATA = MAXSNC
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       Set pointers
      TIMKOL = SNKOLS(SNDTIM)
      INTKOL = SNKOLS(SNRTMI)
      SOUKOL = SNKOLS(SNISID)
      ANTKOL = SNKOLS(SNIANT)
      SUBKOL = SNKOLS(SNISUB)
      FRQKOL = SNKOLS(SNIFQI)
      IFRKOL = SNKOLS(SNRIFR)
      NODKOL = SNKOLS(SNINOD)
      MB1KOL = SNKOLS(SNRMD1)
      DS1KOL = SNKOLS(SNRDS1)
      DD1KOL = SNKOLS(SNRDD1)
      RE1KOL = SNKOLS(SNRRE1)
      IM1KOL = SNKOLS(SNRIM1)
      RA1KOL = SNKOLS(SNRRA1)
      DL1KOL = SNKOLS(SNRDE1)
      WT1KOL = SNKOLS(SNRWE1)
      RF1KOL = SNKOLS(SNIRF1)
      MB2KOL = SNKOLS(SNRMD2)
      DS2KOL = SNKOLS(SNRDS2)
      DD2KOL = SNKOLS(SNRDD2)
      RE2KOL = SNKOLS(SNRRE2)
      IM2KOL = SNKOLS(SNRIM2)
      RA2KOL = SNKOLS(SNRRA2)
      DL2KOL = SNKOLS(SNRDE2)
      WT2KOL = SNKOLS(SNRWE2)
      RF2KOL = SNKOLS(SNIRF2)
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
         RECD(TIMKOL) = TIME
         RECORD(INTKOL) = TIMEI
         RECI(SOUKOL) = SOURID
         RECI(ANTKOL) = ANTNO
         RECI(SUBKOL) = SUBA
         IF (FRQKOL.GT.0) RECI(FRQKOL) = FREQID
         IF (IFRKOL.GT.0) RECORD(IFRKOL) = IFR
         RECI(NODKOL) = NODENO
         IF (MB1KOL.GT.0) RECORD(MB1KOL) = MBDELY(1)
         IF (DS1KOL.GT.0) RECORD(DS1KOL) = DISP(1)
         IF (DD1KOL.GT.0) RECORD(DD1KOL) = DDISP(1)
C                                       First polarization
         DO 10 LOOP = 1,NUMIF
            RECORD(RE1KOL+LOOP-1) = CREAL(1,LOOP)
            RECORD(IM1KOL+LOOP-1) = CIMAG(1,LOOP)
            RECORD(DL1KOL+LOOP-1) = DELAY(1,LOOP)
            RECORD(RA1KOL+LOOP-1) = RATE(1,LOOP)
            RECORD(WT1KOL+LOOP-1) = WEIGHT(1,LOOP)
            RECI(RF1KOL+LOOP-1) = REFA(1,LOOP)
 10         CONTINUE
C                                       Two polarizations
         IF (NUMPOL.GE.2) THEN
            IF (MB2KOL.GT.0) RECORD(MB2KOL) = MBDELY(2)
            IF (DS2KOL.GT.0) RECORD(DS2KOL) = DISP(2)
            IF (DD2KOL.GT.0) RECORD(DD2KOL) = DDISP(2)
            DO 20 LOOP = 1,NUMIF
               RECORD(RE2KOL+LOOP-1) = CREAL(2,LOOP)
               RECORD(IM2KOL+LOOP-1) = CIMAG(2,LOOP)
               RECORD(DL2KOL+LOOP-1) = DELAY(2,LOOP)
               RECORD(RA2KOL+LOOP-1) = RATE(2,LOOP)
               RECORD(WT2KOL+LOOP-1) = WEIGHT(2,LOOP)
               RECI(RF2KOL+LOOP-1) = REFA(2,LOOP)
 20            CONTINUE
            END IF
         END IF
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, ISNRNO, RECORD, BUFFER, IERR)
      ISNRNO = ISNRNO + 1
      IF (IERR.GT.0) GO TO 990
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
         TIME = RECD(TIMKOL)
         TIMEI = RECORD(INTKOL)
         SOURID = RECI(SOUKOL)
         ANTNO = RECI(ANTKOL)
         SUBA = RECI(SUBKOL)
         NODENO = RECI(NODKOL)
         FREQID = 1
         IF (FRQKOL.GT.0) FREQID = RECI(FRQKOL)
         IFR = 0.0
         IF (IFRKOL.GT.0) IFR = RECORD(IFRKOL)
         MBDELY(1) = 0.0
         IF (MB1KOL.GT.0) MBDELY(1) = RECORD(MB1KOL)
         DISP(1) = 0.0
         IF (DS1KOL.GT.0) DISP(1) = RECORD(DS1KOL)
         DDISP(1) = 0.0
         IF (DD1KOL.GT.0) DDISP(1) = RECORD(DD1KOL)
C                                       First polarization
         DO 60 LOOP = 1,NUMIF
            CREAL(1,LOOP) = RECORD(RE1KOL+LOOP-1)
            CIMAG(1,LOOP) = RECORD(IM1KOL+LOOP-1)
            DELAY(1,LOOP) = RECORD(DL1KOL+LOOP-1)
            RATE(1,LOOP) = RECORD(RA1KOL+LOOP-1)
            WEIGHT(1,LOOP) = RECORD(WT1KOL+LOOP-1)
            REFA(1,LOOP) = RECI(RF1KOL+LOOP-1)
 60         CONTINUE
C                                       Two polarizations
         IF (NUMPOL.GE.2) THEN
            MBDELY(2) = 0.0
            IF (MB2KOL.GT.0) MBDELY(2) = RECORD(MB2KOL)
            DISP(2) = 0.0
            IF (DS2KOL.GT.0) DISP(2) = RECORD(DS2KOL)
            DDISP(2) = 0.0
            IF (DD2KOL.GT.0) DDISP(2) = RECORD(DD2KOL)
            DO 80 LOOP = 1,NUMIF
               CREAL(2,LOOP) = RECORD(RE2KOL+LOOP-1)
               CIMAG(2,LOOP) = RECORD(IM2KOL+LOOP-1)
               DELAY(2,LOOP) = RECORD(DL2KOL+LOOP-1)
               RATE(2,LOOP) = RECORD(RA2KOL+LOOP-1)
               WEIGHT(2,LOOP) = RECORD(WT2KOL+LOOP-1)
               REFA(2,LOOP) = RECI(RF2KOL+LOOP-1)
 80            CONTINUE
            END IF
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, ISNRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABSN: TABIO ERROR',I3)
      END
