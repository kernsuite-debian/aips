      SUBROUTINE BTTAB (OPCODE, BUFFER, NUMBND, WEIGHT, BNDPAS, IERR)
C-----------------------------------------------------------------------
C! I/O to array geometry tables (following initialization by BTINI)
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2002, 2007
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
C   Does I/O to correlator bandpass tables. Usually used after setup by
C   BTINI.
C   Inputs:
C      OPCODE   C*4        Operation code:
C                          'READ' = read entry from table.
C                          'WRIT' = write entry in table.
C                          'CLOS' = close file, flush on write
C      NUMBND   I          # band's (IF's) in data
C   In/out
C      BUFFER   I(512)     I/O buffer and related storage, also defines
C                          file if open. Should have been returned by
C                          BTINI.
C      WEIGHT   R(*)       Weights: (nif, npol) - returns the
C                          integration time in seconds on read, ignores
C                          on writ.
C      BNDPAS   C(*)       Complex channel gains: (nchan, nif, npol)
C   Inputs via common:
C      IBTRNO   I          Next scan number to read or write.
C      BTKOLS   I(MAXBTC)  The column pointer array in order:
C                          TIME, TIME INTERVAL, SOURCE ID, ANTENNA NO.,
C                          ARRAY, FREQID, BANDWIDTH, BAND FREQ.
C                          REFANT1, BREAL1, BIMAG1.
C                          The following columns are only present if
C                          NOPOLZ = 2:
C                          REFANT2, BREAL2, BIMAG2.
C      BTNUMV   I(MAXBTC)  Element count in each column.
C      NOPCAL   I          # polzn cal. constants
C      NOBTCH   I          # channels in table
C   Input/output: (written to or read from bandpass file)
C      TIME     D          Time of centre of interval since 0hr on
C                          reference day (days)
C      TINT     R          Interval over which calibration constants
C                          apply (days)
C      ISRC     I          Source number
C      NOSTA    I          Station number
C      IARRAY   I          Array number
C      IFQID    I          Freq ID number
C      REFAN1   I          Reference antenna for polzn 1
C      REFAN2   I          Reference antenna for polzn 2
C   Output:
C      IBTRNO   I          Next row number.
C      IERR     I          Error code, 0=>OK else TABIO error.
C                          Note: -1=> read but record deselected.
C   Usage NOTE: use the include 'DBTV.INC' for the declarations in
C   BTINI and BTTAB.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), IERR, NUMBND
      REAL      WEIGHT(*), BNDPAS(2,*)
C
      LOGICAL   DOREAD, DOCLOS
      INTEGER   I, ICOP, INX
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBTV.INC'
      INTEGER   RECI(MAXREC)
      REAL      RECORD(MAXREC), WT
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
         RECD(BTKOLS(KTIM)) = TIME
C                                       Interval
         RECORD(BTKOLS(KINT)) = TINT
C                                       Source name
         RECI(BTKOLS(KSOU)) = ISRC
C                                       Antenna number
         RECI(BTKOLS(KANO)) = NOSTA
C                                       Array number
         RECI(BTKOLS(KARR)) = IARRAY
C                                       Freqid number
         RECI(BTKOLS(KFQI)) = IFQID
C                                       Channel bandwidth
         RECORD(BTKOLS(KBWD)) = CHBW
C                                       Band freq.
         DO 10 I = 1,NUMBND
            RECD(BTKOLS(KBFR)+I-1) = BANDFR(I)
 10         CONTINUE
C                                       Refant for polzn 1
         RECI(BTKOLS(KRF1)) = REFAN1
C                                       Bandpass function for polzn 1
         ICOP = NUMBND * NOBTCH
         INX = 0
         DO 20 I = 1,ICOP
            INX = INX + 1
            RECORD(BTKOLS(KBR1)+I-1) = BNDPAS(1,INX)
            RECORD(BTKOLS(KBI1)+I-1) = BNDPAS(2,INX)
 20         CONTINUE
C                                       2nd polzn
         IF (NOPOLZ.GT.1) THEN
C                                       Refant for polzn 2
            RECI(BTKOLS(KRF2)) = REFAN2
C                                       Bandpass function
            DO 30 I = 1,ICOP
               INX = INX + 1
               RECORD(BTKOLS(KBR2)+I-1) = BNDPAS(1,INX)
               RECORD(BTKOLS(KBI2)+I-1) = BNDPAS(2,INX)
 30            CONTINUE
            END IF
         END IF
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, IBTRNO, RECORD, BUFFER, IERR)
      IBTRNO = IBTRNO + 1
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABIO', 'BTTAB', IERR)
         GO TO 990
         END IF
      IF (IERR.LT.0) GO TO 50
C                                       If READ pick data from RECORD.
      IF (DOREAD) THEN
C                                       Time
         TIME = RECD(BTKOLS(KTIM))
C                                       Interval
         TINT = RECORD(BTKOLS(KINT))
         WT = TINT * 24. * 360.
C                                       Source number
         ISRC = RECI(BTKOLS(KSOU))
C                                       Antenna number
         NOSTA = RECI(BTKOLS(KANO))
C                                       Array number
         IARRAY = RECI(BTKOLS(KARR))
C                                       Freqid number
         IFQID = RECI(BTKOLS(KFQI))
C                                       Channel bandwidth
         CHBW = RECORD(BTKOLS(KBWD))
C                                       Band freq.
         DO 110 I = 1,NUMBND
            BANDFR(I) = RECD(BTKOLS(KBFR)+I-1)
 110        CONTINUE
         CALL RFILL (2*NUMBND, WT, WEIGHT)
C                                       Refant for polzn 1
         REFAN1 = RECI(BTKOLS(KRF1))
C                                       Bandpass function for polzn 1
         ICOP = NUMBND * NOBTCH
         INX = 0
         DO 120 I = 1,ICOP
            INX = INX + 1
            BNDPAS(1,INX) = RECORD(BTKOLS(KBR1)+I-1)
            BNDPAS(2,INX) = RECORD(BTKOLS(KBI1)+I-1)
 120        CONTINUE
C                                       2nd polzn
         IF (NOPOLZ.GT.1) THEN
C                                       Refant for polzn 2
            REFAN2 = RECI(BTKOLS(KRF2))
C                                       Bandpass function for polzn 2
            DO 130 I = 1,ICOP
               INX = INX + 1
               BNDPAS(1,INX) = RECORD(BTKOLS(KBR2)+I-1)
               BNDPAS(2,INX) = RECORD(BTKOLS(KBI2)+I-1)
 130           CONTINUE
            END IF
         END IF
      GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, IBTRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'BTTAB', IERR)
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('BTTAB: TABIO ERROR',I5)
      END
