      SUBROUTINE ATTAB (OPCODE, BUFFER, NUMBND, IERR)
C-----------------------------------------------------------------------
C! I/O to array geometry tables (following initialization by ATINI)
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2006, 2009
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
C   Does I/O to array geometry tables. Usually used after setup by
C   ATINI.
C   Inputs:
C      OPCODE   C*4        Operation code:
C                          'READ' = read entry from table.
C                          'WRIT' = write entry in table.
C                          'CLOS' = close file, flush on write
C      BUFFER   I(*)       I/O buffer and related storage, also defines
C                          file if open. Should have been returned by
C                          ATINI.
C      IATRNO   I          Next scan number to read or write.
C      ATKOLS   I(MAXATC)  The column pointer array in order:
C                          TIME, TIME INTERVAL, ANNAME, ANTENNA NO.,
C                          ARRAY, FREQID, NO_LEVELS, POLTYA, POLAA,
C                          POLCALA, POLTYB, POLAB, POLCALB.
C      ATNUMV   I(MAXATC)  Element count in each column.
C      NOPCAL   I          # polzn cal. constants
C      NUMBND   I          # band's (IF's) in data
C   Input/output: (written to or read from antenna file)
C      TIME     D          Time of centre of interval since 0hr on
C                          reference day (days)
C      TINT     R          Interval over which calibration constants
C                          apply (days)
C      ANNAME   C*8        Station name
C      NOSTA    I          Station number
C      IARRAY   I          Array number
C      IFQID    I          Freq ID number
C      NOLEVL   I          # levels in the digitizer at the antenna,
C                          VLBA 2 or 4, MkII/MkIII 2
C      POLTYA   C*1        Feed A polzn type ('R','L','X','Y')
C      POLAA    R(MAXIF)   Feed A posn angle (degrees)
C      POLCA    R(2*MAXIF) Feed A polzn cal parameter
C      POLTYB   C*1        Feed B polzn type ('R','L','X','Y')
C      POLAB    R(MAXIF)   Feed B posn angle (degrees)
C      POLCB    R(2*MAXIF) Feed B polzn cal parameter
C   Output:
C      IATRNO   I          Next row number.
C      IERR     I          Error code, 0=>OK else TABIO error.
C                          Note: -1=> read but record deselected.
C   Usage NOTE: use the include 'DATV.INC' for the declarations in
C   ATINI and ATTAB.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), IERR, NUMBND
C
      LOGICAL   DOREAD, DOCLOS
      INTEGER   NCOPY, I
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DATV.INC'
      INTEGER   RECI(MAXREC)
      HOLLERITH RECH(MAXREC)
      REAL      RECORD(MAXREC)
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
         RECD(ATKOLS(KTIM)) = TIME
C                                       Interval
         RECORD(ATKOLS(KINT)) = TINT
C                                       Antenna name
         CALL CHR2H (8, ANNAME, 1, RECH(ATKOLS(KANN)))
C                                       Antenna number
         RECI(ATKOLS(KANO)) = NOSTA
C                                       Array number
         RECI(ATKOLS(KARR)) = IARRAY
C                                       Freqid number
         RECI(ATKOLS(KFRE)) = IFQID
C                                       Number of levels
         RECI(ATKOLS(KNLV)) = NOLEVL
C                                       Feed A polzn type
         CALL CHR2H (1, POLTYA, 1, RECH(ATKOLS(KTYA)))
C                                       Feed B polzn type
         CALL CHR2H (1, POLTYB, 1, RECH(ATKOLS(KTYB)))
C                                       IF & NOPCAL dependent arrays
         NCOPY = ATNUMV(KPAA)
         CALL RCOPY (NCOPY, POLAA, RECORD(ATKOLS(KPAA)))
         NCOPY = ATNUMV(KPAB)
         CALL RCOPY (NCOPY, POLAB, RECORD(ATKOLS(KPAB)))
         DO 10 I = 1,NUMBND
            IF (NOPCAL.GE.1) THEN
               RECORD(ATKOLS(KCLA)+I-1) = POLCA(I,1)
               RECORD(ATKOLS(KCLB)+I-1) = POLCB(I,1)
               END IF
            IF (NOPCAL.GE.2) THEN
               RECORD(ATKOLS(KCLA)+I)   = POLCA(I,2)
               RECORD(ATKOLS(KCLB)+I)   = POLCB(I,2)
               END IF
 10         CONTINUE
         NCOPY = ATNUMV(KFWH)
         CALL RCOPY (NCOPY, ANTFWH, RECORD(ATKOLS(KFWH)))
         END IF
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, IATRNO, RECORD, BUFFER, IERR)
      IATRNO = IATRNO + 1
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABIO', 'ATTAB', IERR)
         GO TO 990
         END IF
      IF (IERR.LT.0) GO TO 50
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
C                                       Time
         TIME = RECD(ATKOLS(KTIM))
C                                       Interval
         TINT = RECORD(ATKOLS(KINT))
C                                       Antenna name
         CALL H2CHR (8, 1, RECH(ATKOLS(KANN)), ANNAME)
C                                       Antenna number
         NOSTA = RECI(ATKOLS(KANO))
C                                       Array number
         IARRAY = RECI(ATKOLS(KARR))
C                                       Freqid number
         IFQID = RECI(ATKOLS(KFRE))
C                                       Number of levels
         NOLEVL = RECI(ATKOLS(KNLV))
C                                       Feed A polzn type
         CALL H2CHR (1, 1, RECH(ATKOLS(KTYA)), POLTYA)
C                                       Feed B polzn type
         CALL H2CHR (1, 1, RECH(ATKOLS(KTYB)), POLTYB)
C                                       IF & NOPCAL dependent arrays
         NCOPY = ATNUMV(KPAA)
         CALL RCOPY (NCOPY, RECORD(ATKOLS(KPAA)), POLAA)
         NCOPY = ATNUMV(KPAB)
         CALL RCOPY (NCOPY, RECORD(ATKOLS(KPAB)), POLAB)
         DO 60 I = 1,NUMBND
            IF (NOPCAL.GE.1) THEN
               POLCA(I,1) = RECORD(ATKOLS(KCLA)+I-1)
               POLCB(I,1) = RECORD(ATKOLS(KCLB)+I-1)
               END IF
            IF (NOPCAL.GE.2) THEN
               POLCA(I,2) = RECORD(ATKOLS(KCLA)+I)
               POLCB(I,2) = RECORD(ATKOLS(KCLB)+I)
               END IF
 60         CONTINUE
         NCOPY = ATNUMV(KFWH)
         IF (NCOPY.GT.0) THEN
            CALL RCOPY (NCOPY, RECORD(ATKOLS(KFWH)), ANTFWH)
         ELSE
            CALL RFILL (NOBAND, 0.0, ANTFWH)
            END IF
         END IF
         GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, IATRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'ATTAB', IERR)
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('ATTAB: TABIO ERROR',I5)
      END
