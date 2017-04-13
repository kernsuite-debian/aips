      SUBROUTINE CATAB (OPCODE, BUFFER, IERR)
C-----------------------------------------------------------------------
C! I/O to CAlibration tables (following initialization by CAINI)
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   Does I/O to calibration (CA) tables after setup by CAINI.
C   Inputs:
C      OPCODE   C*4        Operation code:
C                          'READ' = read entry from table.
C                          'WRIT' = write entry in table.
C                          'CLOS' = close file, flush on write
C      BUFFER   I(512)     I/O buffer and related storage, also defines
C                          file if open. Should have been returned by
C                          CAINI.
C   Inputs from common:
C      ICARNO   I          Next scan number to read or write.
C      CAKOLS   I(MAXCAC)  The column pointer array in order:
C                          TIME, TIME INTERVAL, SOURCE ID, ANTENNA NO.,
C                          ARRAY, FREQID,
C                          TSYS1, TANT1, SENSITIVITY1, PHASE1, RATE1,
C                          DELAY1, REAL1, IMAG1, WEIGHT1, REFANT1,
C                          TSYS2, TANT2, SENSITIVITY 2, PHASE2, RATE2,
C                          DELAY2, REAL2, IMAG2, WEIGHT2, REFANT2
C      CANUMV   I(MAXCAC)  Element count in each column.
C
C   Input/output: (written to or read from calibration file)
C      TIME     D          Time.
C      TINT     R          Time interval for calibration.
C      SOURID   I          Source id.
C      ANTNO    I          Antenna id.
C      ARRNO    I          Array id.
C      FREQID   I          Unique frequency code.
C                            --- Polarisation 1 ---
C      TSYS1    R(MAXIF)   On-source system temperature for poln 1.
C      TANT1    R(MAXIF)   On-source antenna temperature for poln 1.
C      SENS1    R(MAXIF)   Sensitivity in (K/Jy) for poln 1.
C      PHASE1   R(MAXIF)   Phase part of complex antenna gain for poln 1.
C      RATE1    R(MAXIF)   Residual rate for polarisation 1.
C      DELAY1   R(MAXIF)   Residual delay for polarisation 1.
C      REAL1    R(MAXIF)   Real part of gain calibration for poln 1.
C      IMAG1    R(MAXIF)   Imaginary part of gain calibration for poln 1.
C      WGHT1    R(MAXIF)   Weight for gain calibration of poln 1.
C      REFAN1   I          Reference antenna for gain soln for poln 1.
C                            --- Polarisation 2 ---
C      TSYS1    R(MAXIF)   On-source system temperature for poln 2.
C      TANT1    R(MAXIF)   On-source antenna temperature for poln 2.
C      SENS1    R(MAXIF)   Sensitivity in (K/Jy) for poln 2.
C      PHASE1   R(MAXIF)   Phase part of complex antenna gain for poln 2.
C      RATE1    R(MAXIF)   Residual rate for polarisation 2.
C      DELAY1   R(MAXIF)   Residual delay for polarisation 2.
C      REAL1    R(MAXIF)   Real part of gain calibration for poln 2.
C      IMAG1    R(MAXIF)   Imaginary part of gain calibration for poln 2.
C      WGHT1    R(MAXIF)   Weight for gain calibration of poln 2.
C      REFAN1   I          Reference antenna for gain soln for poln 2.
C
C   Output:
C      ICARNO   I          Next row number.
C      IERR     I          Error code, 0=>OK else TABIO error.
C                          Note: -1=> read but record deselected.
C   Usage NOTE: use the include 'DCAV.INC' for the declarations in
C   CAINI and CATAB.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), IERR
C
      LOGICAL   DOREAD, DOCLOS
      INTEGER   NCOPY
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAV.INC'
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
C                                       Time
         RECD(CAKOLS(KTIM)) = TIME
C                                       Interval
         RECORD(CAKOLS(KINT)) = TINT
C                                       Source id
         RECI(CAKOLS(KSID)) = SOURID
C                                       Antenna #
         RECI(CAKOLS(KANT)) = ANTNO
C                                       Array #
         RECI(CAKOLS(KARR)) = ARRNO
C                                       Frequency id
         RECI(CAKOLS(KFID)) = FREQID
C                                       --- Polarisation 1 ---
C                                       Tsys 1
         NCOPY = CANUMV(KTS1)
         CALL RCOPY (NCOPY, TSYS1, RECORD(CAKOLS(KTS1)))
C                                       Tant 1
         NCOPY = CANUMV(KTA1)
         CALL RCOPY (NCOPY, TANT1, RECORD(CAKOLS(KTA1)))
C                                       Sensitivity 1
         NCOPY = CANUMV(KSE1)
         CALL RCOPY (NCOPY, SENS1, RECORD(CAKOLS(KSE1)))
C                                       Phase 1
         NCOPY = CANUMV(KPH1)
         CALL RCOPY (NCOPY, PHASE1, RECORD(CAKOLS(KPH1)))
C                                       Rate 1
         NCOPY = CANUMV(KRT1)
         CALL RCOPY (NCOPY, RATE1, RECORD(CAKOLS(KRT1)))
C                                       Delay 1
         NCOPY = CANUMV(KDY1)
         CALL RCOPY (NCOPY, DELAY1, RECORD(CAKOLS(KDY1)))
C                                       Real 1
         NCOPY = CANUMV(KRL1)
         CALL RCOPY (NCOPY, REAL1, RECORD(CAKOLS(KRL1)))
C                                       Imaginary 1
         NCOPY = CANUMV(KIM1)
         CALL RCOPY (NCOPY, IMAG1, RECORD(CAKOLS(KIM1)))
C                                       Weight 1
         NCOPY = CANUMV(KWT1)
         CALL RCOPY (NCOPY, WGHT1, RECORD(CAKOLS(KWT1)))
C                                       Reference antenna 1
         RECI(CAKOLS(KRF1)) = REFAN1
C
C                                       --- Polarisation 2 ---
C
C                                       Tsys 2
         NCOPY = CANUMV(KTS2)
         CALL RCOPY (NCOPY, TSYS2, RECORD(CAKOLS(KTS2)))
C                                       Tant 2
         NCOPY = CANUMV(KTA2)
         CALL RCOPY (NCOPY, TANT2, RECORD(CAKOLS(KTA2)))
C                                       Sensitivity 2
         NCOPY = CANUMV(KSE2)
         CALL RCOPY (NCOPY, SENS2, RECORD(CAKOLS(KSE2)))
C                                       Phase 2
         NCOPY = CANUMV(KPH2)
         CALL RCOPY (NCOPY, PHASE2, RECORD(CAKOLS(KPH2)))
C                                       Rate 2
         NCOPY = CANUMV(KRT2)
         CALL RCOPY (NCOPY, RATE2, RECORD(CAKOLS(KRT2)))
C                                       Delay 2
         NCOPY = CANUMV(KDY2)
         CALL RCOPY (NCOPY, DELAY2, RECORD(CAKOLS(KDY2)))
C                                       Real 2
         NCOPY = CANUMV(KRL2)
         CALL RCOPY (NCOPY, REAL2, RECORD(CAKOLS(KRL2)))
C                                       Imaginary 2
         NCOPY = CANUMV(KIM2)
         CALL RCOPY (NCOPY, IMAG2, RECORD(CAKOLS(KIM2)))
C                                       Weight 2
         NCOPY = CANUMV(KWT2)
         CALL RCOPY (NCOPY, WGHT2, RECORD(CAKOLS(KWT2)))
C                                       Reference antenna 2
         RECI(CAKOLS(KRF2)) = REFAN2
         ENDIF
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, ICARNO, RECORD, BUFFER, IERR)
      ICARNO = ICARNO + 1
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABIO', 'CATAB', IERR)
         GO TO 990
         END IF
      IF (IERR.LT.0) GO TO 50
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
C                                       Time
         TIME = RECD(CAKOLS(KTIM))
C                                       Time interval
         TINT = RECORD(CAKOLS(KINT))
C                                       Source id
         SOURID = RECI(CAKOLS(KSID))
C                                       Antenna #
         ANTNO = RECI(CAKOLS(KANT))
C                                       Array #
         ARRNO = RECI(CAKOLS(KARR))
C                                       Frequency id
         FREQID = RECI(CAKOLS(KFID))
C                                       --- Polarisation 1 ---
C
C                                       Tsys 1
         NCOPY = CANUMV(KTS1)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KTS1)), TSYS1)
C                                       Tant 1
         NCOPY = CANUMV(KTA1)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KTA1)), TANT1)
C                                       Sensitivity 1
         NCOPY = CANUMV(KSE1)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KSE1)), SENS1)
C                                       Phase 1
         NCOPY = CANUMV(KPH1)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KPH1)), PHASE1)
C                                       Rate 1
         NCOPY = CANUMV(KRT1)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KRT1)), RATE1)
C                                       Delay 1
         NCOPY = CANUMV(KDY1)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KDY1)), DELAY1)
C                                       Real 1
         NCOPY = CANUMV(KRL1)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KRL1)), REAL1)
C                                       Imaginary 1
         NCOPY = CANUMV(KIM1)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KIM1)), IMAG1)
C                                       Weight 1
         NCOPY = CANUMV(KWT1)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KWT1)), WGHT1)
C                                       Reference antenna 1
         REFAN1 = RECI(CAKOLS(KRF1))
C
C                                       --- Polarisation 2 ---
C
C                                       Tsys 2
         NCOPY = CANUMV(KTS2)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KTS2)), TSYS2)
C                                       Tant 2
         NCOPY = CANUMV(KTA2)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KTA2)), TANT2)
C                                       Sensitivity 2
         NCOPY = CANUMV(KSE2)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KSE2)), SENS2)
C                                       Phase 2
         NCOPY = CANUMV(KPH2)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KPH2)), PHASE2)
C                                       Rate 2
         NCOPY = CANUMV(KRT2)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KRT2)), RATE2)
C                                       Delay 2
         NCOPY = CANUMV(KDY2)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KDY2)), DELAY2)
C                                       Real 2
         NCOPY = CANUMV(KRL2)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KRL2)), REAL2)
C                                       Imaginary 2
         NCOPY = CANUMV(KIM2)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KIM2)), IMAG2)
C                                       Weight 2
         NCOPY = CANUMV(KWT2)
         CALL RCOPY (NCOPY, RECORD(CAKOLS(KWT2)), WGHT2)
C                                       Reference antenna 2
         REFAN2 = RECI(CAKOLS(KRF2))
C
         END IF
         GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, ICARNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'CATAB', IERR)
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('CATAB: TABIO ERROR',I5)
      END
