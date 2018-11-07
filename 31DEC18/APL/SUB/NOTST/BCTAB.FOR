      SUBROUTINE BCTAB (OPCODE, BUFFER, IERR)
C-----------------------------------------------------------------------
C! I/O to baseline correction tables (following initialization by BCINI)
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2008
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
C   Does I/O to array geometry tables after setup by BCINI.
C   Inputs:
C      OPCODE   C*4        Operation code:
C                          'READ' = read entry from table.
C                          'WRIT' = write entry in table.
C                          'CLOS' = close file, flush on write
C      BUFFER   I(*)       I/O buffer and related storage, also defines
C                          file if open. Should have been returned by
C                          BCINI.
C   Inputs via common:
C      IBCRNO   I          Next scan number to read or write.
C      BCKOLS   I(MAXBCC)  The column pointer array in order:
C                          TIME, SOURCE ID, ARRAY, ANTENNA NOS.,
C                          FREQID, REAL_M, IMAG_M, REAL_A, IMAG_A
C      BCNUMV   I(MAXBCC)  Element count in each column.
C      NUMBND   I          # band's (IF's) in data
C   Input/output: (written to or read from antenna file)
C      TIME     R          Time of centre of interval since 0hr on
C                          reference day (days)
C      ISRC     I          Source number
C      ANTS     I(2)       1st and 2nd antennas of baseline to be
C                          corrected.
C      IARRAY   I          Array number
C      IFQID    I          Freq ID number
C      RLMUL    R(*)       Real part of multiplicative correction
C      IMMUL    R(*)       Imag part of multiplicative correction
C      RLADD    R(*)       Real part of additive correction
C      IMADD    R(*)       Imag part of additive correction
C                          where * is  one-dimensional implementing
c                          (NOSTKD,NOBAND)
C   Output:
C      IBCRNO   I          Next row number.
C      IERR     I          Error code, 0=>OK else TABIO error.
C                          Note: -1=> read but record deselected.
C   Usage NOTE: use the include 'DBCV.INC' for the declarations in
C   BCINI and BCTAB.
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
      INCLUDE 'INCS:DBCV.INC'
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
         RECORD(BCKOLS(KTIM)) = TIME
C                                       Source name
         RECI(BCKOLS(KSOU)) = ISRC
C                                       Antenna number
         RECI(BCKOLS(KANO)) = ANTS(1)
         RECI(BCKOLS(KANO) + 1) = ANTS(2)
C                                       Array number
         RECI(BCKOLS(KARR)) = IARRAY
C                                       Freqid number
         RECI(BCKOLS(KFQI)) = IFQID
C                                       REAL_M
         NCOPY = BCNUMV(KRM)
         CALL RCOPY (NCOPY, RLMUL, RECORD(BCKOLS(KRM)))
C                                       IMAG_M
         NCOPY = BCNUMV(KIM)
         CALL RCOPY (NCOPY, IMMUL, RECORD(BCKOLS(KIM)))
C                                       REAL_A
         NCOPY = BCNUMV(KRA)
         CALL RCOPY (NCOPY, RLADD, RECORD(BCKOLS(KRA)))
C                                       IMAG_A
         NCOPY = BCNUMV(KIA)
         CALL RCOPY (NCOPY, IMADD, RECORD(BCKOLS(KIA)))
         END IF
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, IBCRNO, RECORD, BUFFER, IERR)
      IBCRNO = IBCRNO + 1
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABIO', 'BCTAB', IERR)
         GO TO 990
         END IF
      IF (IERR.LT.0) GO TO 50
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
C                                       Time
         TIME = RECORD(BCKOLS(KTIM))
C                                       Source name
         ISRC = RECI(BCKOLS(KSOU))
C                                       Antenna number
         ANTS(1) = RECI(BCKOLS(KANO))
         ANTS(2) = RECI(BCKOLS(KANO) + 1)
C                                       Array number
         IARRAY = RECI(BCKOLS(KARR))
C                                       Freqid number
         IFQID = RECI(BCKOLS(KFQI))
C                                       REAL_M
         NCOPY = BCNUMV(KRM)
         CALL RCOPY (NCOPY, RECORD(BCKOLS(KRM)), RLMUL)
C                                       IMAG_M
         NCOPY = BCNUMV(KIM)
         CALL RCOPY (NCOPY, RECORD(BCKOLS(KIM)), IMMUL)
C                                       REAL_A
         NCOPY = BCNUMV(KRA)
         CALL RCOPY (NCOPY, RECORD(BCKOLS(KRA)), RLADD)
C                                       IMAG_A
         NCOPY = BCNUMV(KIA)
         CALL RCOPY (NCOPY, RECORD(BCKOLS(KIA)), IMADD)
         END IF
         GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, IBCRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'BCTAB', IERR)
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('BCTAB: TABIO ERROR',I5)
      END
