      SUBROUTINE TABFQ (OPCODE, BUFFER, IFQRNO, FQKOLS, FQNUMV,
     *   NUMIF, FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IERR)
C-----------------------------------------------------------------------
C! Does I/O to frequency (FQ) table opened by FQINI
C# EXT-util UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2005, 2012
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
C   Does I/O to frequency (FQ) extension tables. Usually used after
C   setup by FQINI.
C   Inputs:
C      OPCODE   C*4    Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C      BUFFER   I(*)   I/O buffer and related storage, also defines file
C                      if open. Should have been returned by FQINI or
C                      TABINI.
C      IFQRNO   I      Next entry number to read or write.
C      FQKOLS   I(*)   The column pointer array in order,
C                      FQID, IFFREQ, IFCHW, IFTBW, IFSIDE
C      FQNUMV   I(*)   Element count in each column.
C      NUMIF    I      Number of IF's
C   Input/output: (written to or read from frequency table)
C      FQID     I      Frequency ID number, is random parameter
C                      in uv-data.
C      IFFREQ   D(*)   Reference frequency for each IF (Hz)
C      IFCHW    R(*)   Bandwidth of an individual channel (Hz)
C                      Now always written and read as a signed value.
C      IFTBW    R(*)   Total bandwidth of the IF (Hz).  Now written
C                      and read as an unsigned value.
C      IFSIDE   I(*)   Sideband of the IF (-1 => lower, +1 => upper)
C                      Now always written and read as +1
C      BNDCOD   C*8(*) Band code - code for receiver/feed used
C   Output:
C      IFQRNO   I      Next row number.
C      IERR     I      Error code, 0=>OK else TABIO error.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, BNDCOD(*)*(*)
      INTEGER   BUFFER(*), IFQRNO, FQKOLS(*), FQNUMV(*), NUMIF, FQID,
     *   IFSIDE(*), IERR
      DOUBLE PRECISION IFFREQ(*)
      REAL      IFCHW(*), IFTBW(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   RECI(1+MAXIF*5), KOLS(MAXFQC), IDKOL, FRQKOL, CHKOL,
     *   TBWKOL, SIDKOL, BNDKOL, IFLP, NDATA, ISIGN, JTRIM, I
      REAL      RECORD(1+MAXIF*7)
      HOLLERITH RECH(1+MAXIF*7)
      DOUBLE PRECISION RECD(MAXIF*4)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KOLS(1), IDKOL), (KOLS(2), FRQKOL),
     *   (KOLS(3), CHKOL), (KOLS(4), TBWKOL), (KOLS(5), SIDKOL),
     *   (KOLS(6), BNDKOL)
      EQUIVALENCE (RECD, RECORD, RECI, RECH)
C-----------------------------------------------------------------------
C                                       set local pointers
      NDATA = MAXFQC
      CALL COPY (NDATA, FQKOLS, KOLS)
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, IFQRNO, RECORD, BUFFER, IERR)
C                                       If write fill RECORD
      ELSE IF (OPCODE.EQ.'WRIT') THEN
         RECI(IDKOL) = FQID
         DO 10 IFLP = 1, NUMIF
C                                       Work out sign of increment
            ISIGN = +1
            IF ((IFCHW(IFLP).LT.0.0) .OR. (IFSIDE(IFLP).LT.0))
     *         ISIGN = -1
            RECD(FRQKOL) = IFFREQ(IFLP)
            FRQKOL = FRQKOL + 1
            RECORD(CHKOL) = ISIGN * ABS(IFCHW(IFLP))
            CHKOL = CHKOL + 1
            RECORD(TBWKOL) = ABS(IFTBW(IFLP))
            TBWKOL = TBWKOL + 1
            RECI(SIDKOL) = +1
            SIDKOL = SIDKOL + 1
            IF (BNDKOL.GT.0) THEN
               I = JTRIM (BNDCOD(IFLP))
               CALL CHR2H (8, BNDCOD(IFLP), 1, RECH(BNDKOL))
               BNDKOL = BNDKOL + 2
               END IF
 10         CONTINUE
         CALL TABIO ('WRIT', 0, IFQRNO, RECORD, BUFFER, IERR)
         IFQRNO = IFQRNO + 1
C                                       READ
      ELSE
         CALL TABIO (OPCODE, 0, IFQRNO, RECORD, BUFFER, IERR)
         IFQRNO = IFQRNO + 1
C                                       If READ pick data from RECORD.
         IF (IERR.LE.0) THEN
            FQID = RECI(IDKOL)
            DO 20 IFLP = 1, NUMIF
C                                       Work out sign of increment
               ISIGN = +1
               IF ((RECORD(CHKOL).LT.0.0) .OR. (RECI(SIDKOL).LT.0))
     *            ISIGN = -1
               IFFREQ(IFLP) = RECD(FRQKOL)
               FRQKOL = FRQKOL + 1
               IFCHW(IFLP) = ISIGN * ABS(RECORD(CHKOL))
               CHKOL = CHKOL + 1
               IFTBW(IFLP) = ABS(RECORD(TBWKOL))
               TBWKOL = TBWKOL + 1
               IFSIDE(IFLP) = +1
               SIDKOL = SIDKOL + 1
               IF (BNDKOL.GT.0) THEN
                  CALL H2CHR (8, 1, RECH(BNDKOL), BNDCOD(IFLP))
                  I = JTRIM (BNDCOD(IFLP))
                  BNDKOL = BNDKOL + 2
               ELSE
                  BNDCOD(IFLP) = ' '
                  END IF
 20            CONTINUE
            END IF
         END IF
C                                       Error
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('TABFQ: TABIO ERROR',I3)
      END

