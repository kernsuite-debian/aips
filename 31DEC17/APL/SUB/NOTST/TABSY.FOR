      SUBROUTINE TABSY (OPCODE, BUFFER, ISYRNO, SYKOLS, SYNUMV, NUMPOL,
     *   NUMIF, TIME, TIMEI, CALTYP, SOURID, ANTNO, SUBA, FREQID, PDIFF,
     *   PSUM, PGAIN, IERR)
C-----------------------------------------------------------------------
C! Does I/O to EVLA SysPower (SY) table opened by SYINI
C# EXT-util UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2010, 2017
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
C   Inputs:
C      OPCODE   C*4      Operation code:
C                          'READ' = read entry from table.
C                          'WRIT' = write entry in table.
C                          'CLOS' = close file, flush on write
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open. Should have been returned by
C                        TABINI or CALINI.
C      ISYRNO   I        Next record number to read or write.
C      SYKOLS   I(12)    The column pointer array in order defined by
C                        the parameters in PCLTAB.INC.
C      SYNUMV   I(12)    Element count in each column.
C      NUMPOL   I        Number of polarizations per IF.
C      NUMIF    I        Number of IFs.
C   Input/output: (written to or read from SY file)
C      TIME     D        Center time of CAL record (Days)
C      TIMEI    R        Time interval covered by record (days)
C      CALTYP   I        0 => normal cal, 1 => Solar cal, 2 => ??
C      SOURID   I        Source ID as defined in the SoUrce table.
C      ANTNO    I        Antenna number as defined in Antenna table
C      SUBA     I        Subarray number.
C      FREQID   I        Freqid # as defined in FQ table.
C      PDIFF    R(2,*)   G*(Pon-Poff) (pol,if)
C      PSUM     R(2,*)   G*(Pon+Poff) (pol,if)
C      PGAIN    R(2,*)   G (pol,if) of requantization
C   Output:
C      ISYRNO   I        Next record number.
C      IERR     I        Error code, 0=>OK else TABIO error.
C                           Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXSYC, XSYRSZ
      PARAMETER (MAXSYC = 13)
      PARAMETER (XSYRSZ = 7 + 6*MAXIF)
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), ISYRNO, SYKOLS(MAXSYC), SYNUMV(MAXSYC),
     *   NUMPOL, NUMIF, caltyp, SOURID, ANTNO, SUBA, FREQID, IERR
      DOUBLE PRECISION TIME
      REAL      TIMEI, PDIFF(2,*), PSUM(2,*), PGAIN(2,*)
C
      INTEGER   RECI(XSYRSZ), DIFKOL, SUMKOL, GKOL, LOOP
      LOGICAL   DOREAD, DOCLOS
      REAL      RECORD(XSYRSZ)
      DOUBLE PRECISION RECD(XSYRSZ/2)
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECD, RECORD, RECI)
      DATA RECI /XSYRSZ*0/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       If write fill RECORD
      IF (.NOT.DOREAD) THEN
         RECD(SYKOLS(1)) = TIME
         RECORD(SYKOLS(2)) = TIMEI
         IF (SYKOLS(3).GT.0) RECI(SYKOLS(3)) = CALTYP
         RECI(SYKOLS(4)) = SOURID
         RECI(SYKOLS(5)) = ANTNO
         RECI(SYKOLS(6)) = SUBA
         IF (SYKOLS(7).GT.0) RECI(SYKOLS(7)) = FREQID
C                                       Setup for looping over IF
         DIFKOL = SYKOLS(8) - 1
         SUMKOL = SYKOLS(9) - 1
         GKOL   = SYKOLS(10) - 1
C                                       First polarization
         DO 10 LOOP = 1,NUMIF
            RECORD(DIFKOL+LOOP) = PDIFF(1,LOOP)
            RECORD(SUMKOL+LOOP) = PSUM(1,LOOP)
            RECORD(GKOL+LOOP)   = PGAIN(1,LOOP)
 10         CONTINUE
C                                       Two polarizations
         IF (NUMPOL.GE.2) THEN
            DIFKOL = SYKOLS(11) - 1
            SUMKOL = SYKOLS(12) - 1
            GKOL   = SYKOLS(13) - 1
C                                       First polarization
            DO 20 LOOP = 1,NUMIF
               RECORD(DIFKOL+LOOP) = PDIFF(2,LOOP)
               RECORD(SUMKOL+LOOP) = PSUM(2,LOOP)
               RECORD(GKOL+LOOP)   = PGAIN(2,LOOP)
 20            CONTINUE
            END IF
         END IF
C                                       Process record.
      CALL TABIO (OPCODE, 0, ISYRNO, RECORD, BUFFER, IERR)
      ISYRNO = ISYRNO + 1
      IF (IERR.GT.0) GO TO 990
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
         TIME = RECD(SYKOLS(1))
         TIMEI = RECORD(SYKOLS(2))
         CALTYP = 0
         IF (SYKOLS(3).GT.0) CALTYP = RECI(SYKOLS(3))
         SOURID = RECI(SYKOLS(4))
         ANTNO = RECI(SYKOLS(5))
         SUBA = RECI(SYKOLS(6))
         FREQID = 1
         IF (SYKOLS(7).GT.0) FREQID = RECI(SYKOLS(7))
C                                       Setup for looping over IF
         DIFKOL = SYKOLS(8) - 1
         SUMKOL = SYKOLS(9) - 1
         GKOL   = SYKOLS(10) - 1
C                                       First polarization
         DO 60 LOOP = 1,NUMIF
            PDIFF(1,LOOP) = RECORD(DIFKOL+LOOP)
            PSUM(1,LOOP)  = RECORD(SUMKOL+LOOP)
            PGAIN(1,LOOP) = RECORD(GKOL+LOOP)
 60         CONTINUE
C                                       Two polarizations
         IF (NUMPOL.GE.2) THEN
            DIFKOL = SYKOLS(11) - 1
            SUMKOL = SYKOLS(12) - 1
            GKOL   = SYKOLS(13) - 1
C                                       second polarization
            DO 70 LOOP = 1,NUMIF
               PDIFF(2,LOOP) = RECORD(DIFKOL+LOOP)
               PSUM(2,LOOP)  = RECORD(SUMKOL+LOOP)
               PGAIN(2,LOOP) = RECORD(GKOL+LOOP)
 70            CONTINUE
            END IF
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, ISYRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABSY: TABIO ERROR',I3)
      END
