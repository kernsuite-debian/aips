      SUBROUTINE TABCAL (OPCODE, BUFFER, ICLRNO, CLKOLS, CLNUMV, NUMPOL,
     *   NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, GEODLY,
     *   DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP, DDISP,
     *   CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C-----------------------------------------------------------------------
C! Does I/O to Calibration (CL) table opened by CALINI
C# EXT-util UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2009
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
C   Does I/O to CALIBRATION extention tables. Usually used after setup
C   by CALINI.
C    Inputs:
C     OPCODE   C*4         Operation code:
C                          'READ' = read entry from table.
C                          'WRIT' = write entry in table.
C                          'CLOS' = close file, flush on write
C     BUFFER   I(512)      I/O buffer and related storage, also defines
C                          file if open. Should have been returned by
C                          TABINI or CALINI.
C     ICLRNO   I           Next scan number to read or write.
C     CLKOLS   I(MAXCLC)   The column pointer array in order defined by
C                          the parameters in PCLTAB.INC.
C     CLNUMV   I(MAXCLC)   Element count in each column.
C     NUMPOL   I           Number of polarizations per IF.
C     NUMIF    I           Number of IFs.
C   Input/output: (written to or read from CAL file)
C     TIME     D           Center time of CAL record (Days)
C     TIMEI    R           Time interval covered by record (days)
C     SOURID   I           Source ID as defined in the SoUrce table.
C     ANTNO    I           Antenna number as defined in Antenna table
C     SUBA     I           Subarray number.
C     FREQID   I           Freqid # as defined in FQ table.
C     IFR      R           Ionospheric Faraday Rotation (rad/m**2)
C     GEODLY   D(*)        Geometric delay polynomial series at TIME
C                          (sec)
C     DOPOFF   R(*)        Doppler offset for each IF (Hz)
C     ATMOS    R(2)        Atmospheric delay (sec) 1/Poln
C     DATMOS   R(2)        Time derivative of ATMOS (sec/sec)
C     MBDELY   R(2)        Multi band delays (sec) 1/Poln
C     CLOCK    R(2,*)      "Clock" epoch error (sec) 1/poln
C     DCLOCK   R(2,*)      Time derivative of CLOCK (sec/sec)
C     DISP     R(2,*)      Dispersive delay (sec/m^2 scales as lambda^2)
C                          1/poln
C     DDISP    R(2,*)      Time derivative of DISP (sec/sec/m^2)
C     CREAL    R(2,*)      Real part of the complex gain, 1/poln/IF
C     CIMAG    R(2,*)      Imag part of the complex gain, 1/poln/IF
C     DELAY    R(2,*)      Residual group delay (sec), 1/poln/IF
C     RATE     R(2,*)      Residual fringe rate (Hz), 1/poln/IF
C     WEIGHT   R(2,*)      Weight of solution, 1/poln/IF
C     REFA     I(2,*)      Reference antenna use for cal. solution.
C    Output:
C     ICLRNO   I           Next CAL number.
C     IERR     I           Error code, 0=>OK else TABIO error.
C                          Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4
      DOUBLE PRECISION TIME, GEODLY(*)
      REAL      TIMEI, IFR, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2),
     *   CLOCK(2), DCLOCK(2), DISP(2), DDISP(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF)
      INTEGER   BUFFER(512), CLKOLS(MAXCLC), CLNUMV(MAXCLC), NUMPOL,
     *   NUMIF, SOURID, ANTNO, SUBA, FREQID, REFA(2,MAXIF), IERR
C
      INCLUDE 'INCS:PCLTAB.INC'
      INTEGER   RECI(XCLRSZ), LOOP, NTERMS, DOPKOL, REAKOL, IMAKOL,
     *   DELKOL, RATKOL, WEIKOL, REFKOL, ICLRNO
      LOGICAL   DOREAD, DOCLOS
      REAL      RECORD(XCLRSZ)
      DOUBLE PRECISION RECD(XCLRSZ/2)
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECD, RECORD, RECI)
      DATA RECORD /XCLRSZ*0.0D0/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
         RECD(CLKOLS(CLDTIM)) = TIME
         RECORD(CLKOLS(CLRTMI)) = TIMEI
         RECI(CLKOLS(CLISID)) = SOURID
         RECI(CLKOLS(CLIANT)) = ANTNO
         RECI(CLKOLS(CLISUB)) = SUBA
         IF (CLKOLS(CLIFQI).GT.0) RECI(CLKOLS(CLIFQI)) = FREQID
         IF (CLKOLS(CLRIFR).GT.0) RECORD(CLKOLS(CLRIFR)) = IFR
         NTERMS = CLNUMV(CLDDEL)
         DO 5 LOOP = 1,NTERMS
            RECD(CLKOLS(CLDDEL)+LOOP-1) = GEODLY(LOOP)
 5          CONTINUE
         IF (CLKOLS(CLRATM).GT.0) RECORD(CLKOLS(CLRATM)) = ATMOS
         IF (CLKOLS(CLRDAT).GT.0) RECORD(CLKOLS(CLRDAT)) = DATMOS
         IF (CLKOLS(CLRMD1).GT.0) RECORD(CLKOLS(CLRMD1)) = MBDELY(1)
         IF (CLKOLS(CLRCK1).GT.0) RECORD(CLKOLS(CLRCK1)) = CLOCK(1)
         IF (CLKOLS(CLRDC1).GT.0) RECORD(CLKOLS(CLRDC1)) = DCLOCK(1)
         IF (CLKOLS(CLRDS1).GT.0) RECORD(CLKOLS(CLRDS1)) = DISP(1)
         IF (CLKOLS(CLRDD1).GT.0) RECORD(CLKOLS(CLRDD1)) = DDISP(1)
C                                       Setup for looping over IF
         DOPKOL = CLKOLS(CLRDOP)
         REAKOL = CLKOLS(CLRRE1)
         IMAKOL = CLKOLS(CLRIM1)
         RATKOL = CLKOLS(CLRRA1)
         DELKOL = CLKOLS(CLRDE1)
         WEIKOL = CLKOLS(CLRWE1)
         REFKOL = CLKOLS(CLIRF1)
C                                       First polarization
         DO 10 LOOP = 1,NUMIF
            RECORD(DOPKOL+LOOP-1) = DOPOFF(LOOP)
            RECORD(REAKOL+LOOP-1) = CREAL(1,LOOP)
            RECORD(IMAKOL+LOOP-1) = CIMAG(1,LOOP)
            RECORD(DELKOL+LOOP-1) = DELAY(1,LOOP)
            RECORD(RATKOL+LOOP-1) = RATE(1,LOOP)
            RECORD(WEIKOL+LOOP-1) = WEIGHT(1,LOOP)
            RECI(REFKOL+LOOP-1) = REFA(1,LOOP)
 10         CONTINUE
         IF (NUMPOL.GE.2) THEN
C                                       Two polarizations
            IF (CLKOLS(CLRMD2).GT.0) RECORD(CLKOLS(CLRMD2)) = MBDELY(2)
            IF (CLKOLS(CLRCK2).GT.0) RECORD(CLKOLS(CLRCK2)) = CLOCK(2)
            IF (CLKOLS(CLRDC2).GT.0) RECORD(CLKOLS(CLRDC2)) = DCLOCK(2)
            IF (CLKOLS(CLRDS2).GT.0) RECORD(CLKOLS(CLRDS2)) = DISP(2)
            IF (CLKOLS(CLRDD2).GT.0) RECORD(CLKOLS(CLRDD2)) = DDISP(2)
C                                       Setup for looping over IF
            REAKOL = CLKOLS(CLRRE2)
            IMAKOL = CLKOLS(CLRIM2)
            RATKOL = CLKOLS(CLRRA2)
            DELKOL = CLKOLS(CLRDE2)
            WEIKOL = CLKOLS(CLRWE2)
            REFKOL = CLKOLS(CLIRF2)
            DO 20 LOOP = 1,NUMIF
               RECORD(REAKOL+LOOP-1) = CREAL(2,LOOP)
               RECORD(IMAKOL+LOOP-1) = CIMAG(2,LOOP)
               RECORD(DELKOL+LOOP-1) = DELAY(2,LOOP)
               RECORD(RATKOL+LOOP-1) = RATE(2,LOOP)
               RECORD(WEIKOL+LOOP-1) = WEIGHT(2,LOOP)
               RECI(REFKOL+LOOP-1) = REFA(2,LOOP)
 20            CONTINUE
            END IF
         END IF
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, ICLRNO, RECORD, BUFFER, IERR)
      ICLRNO = ICLRNO + 1
      IF (IERR.GT.0) GO TO 990
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
         TIME = RECD(CLKOLS(CLDTIM))
         TIMEI = RECORD(CLKOLS(CLRTMI))
         SOURID = RECI(CLKOLS(CLISID))
         ANTNO = RECI(CLKOLS(CLIANT))
         SUBA = RECI(CLKOLS(CLISUB))
         NTERMS = CLNUMV(CLDDEL)
         DO 55 LOOP = 1,NTERMS
            GEODLY(LOOP) = RECD(CLKOLS(CLDDEL)+LOOP-1)
 55         CONTINUE
         FREQID = 1
         IF (CLKOLS(CLIFQI).GT.0) FREQID = RECI(CLKOLS(CLIFQI))
         IFR = 0.0
         IF (CLKOLS(CLRIFR).GT.0) IFR = RECORD(CLKOLS(CLRIFR))
         ATMOS = 0.0
         IF (CLKOLS(CLRATM).GT.0) ATMOS = RECORD(CLKOLS(CLRATM))
         DATMOS = 0.0
         IF (CLKOLS(CLRDAT).GT.0) DATMOS = RECORD(CLKOLS(CLRDAT))
         MBDELY(1) = 0.0
         IF (CLKOLS(CLRMD1).GT.0) MBDELY(1) = RECORD(CLKOLS(CLRMD1))
         CLOCK(1) = 0.0
         IF (CLKOLS(CLRCK1).GT.0) CLOCK(1) = RECORD(CLKOLS(CLRCK1))
         DCLOCK(1) = 0.0
         IF (CLKOLS(CLRDC1).GT.0) DCLOCK(1) = RECORD(CLKOLS(CLRDC1))
         DISP(1) = 0.0
         IF (CLKOLS(CLRDS1).GT.0) DISP(1) = RECORD(CLKOLS(CLRDS1))
         DDISP(1) = 0.0
         IF (CLKOLS(CLRDD1).GT.0) DDISP(1) = RECORD(CLKOLS(CLRDD1))
C                                       Setup for looping over IF
         DOPKOL = CLKOLS(CLRDOP)
         REAKOL = CLKOLS(CLRRE1)
         IMAKOL = CLKOLS(CLRIM1)
         RATKOL = CLKOLS(CLRRA1)
         DELKOL = CLKOLS(CLRDE1)
         WEIKOL = CLKOLS(CLRWE1)
         REFKOL = CLKOLS(CLIRF1)
C                                       First polarization
         DO 60 LOOP = 1,NUMIF
            DOPOFF(LOOP) = RECORD(DOPKOL+LOOP-1)
            CREAL(1,LOOP) = RECORD(REAKOL+LOOP-1)
            CIMAG(1,LOOP) = RECORD(IMAKOL+LOOP-1)
            DELAY(1,LOOP) = RECORD(DELKOL+LOOP-1)
            RATE(1,LOOP) = RECORD(RATKOL+LOOP-1)
            WEIGHT(1,LOOP) = RECORD(WEIKOL+LOOP-1)
            REFA(1,LOOP) = RECI(REFKOL+LOOP-1)
 60         CONTINUE
         IF (NUMPOL.GE.2) THEN
C                                       Two polarizations
            MBDELY(2) = 0.0
            IF (CLKOLS(CLRMD2).GT.0) MBDELY(2) = RECORD(CLKOLS(CLRMD2))
            CLOCK(2) = 0.0
            IF (CLKOLS(CLRCK2).GT.0) CLOCK(2) = RECORD(CLKOLS(CLRCK2))
            DCLOCK(2) = 0.0
            IF (CLKOLS(CLRDC2).GT.0) DCLOCK(2) = RECORD(CLKOLS(CLRDC2))
            DISP(2) = 0.0
            IF (CLKOLS(CLRDS2).GT.0) DISP(2) = RECORD(CLKOLS(CLRDS2))
            DDISP(2) = 0.0
            IF (CLKOLS(CLRDD2).GT.0) DDISP(2) = RECORD(CLKOLS(CLRDD2))
C                                       Setup for looping over IF
            REAKOL = CLKOLS(CLRRE2)
            IMAKOL = CLKOLS(CLRIM2)
            RATKOL = CLKOLS(CLRRA2)
            DELKOL = CLKOLS(CLRDE2)
            WEIKOL = CLKOLS(CLRWE2)
            REFKOL = CLKOLS(CLIRF2)
            DO 70 LOOP = 1,NUMIF
               CREAL(2,LOOP) = RECORD(REAKOL+LOOP-1)
               CIMAG(2,LOOP) = RECORD(IMAKOL+LOOP-1)
               DELAY(2,LOOP) = RECORD(DELKOL+LOOP-1)
               RATE(2,LOOP) = RECORD(RATKOL+LOOP-1)
               WEIGHT(2,LOOP) = RECORD(WEIKOL+LOOP-1)
               REFA(2,LOOP) = RECI(REFKOL+LOOP-1)
 70            CONTINUE
            END IF
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, ICLRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABCAL: TABIO ERROR',I3)
      END
