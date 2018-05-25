      SUBROUTINE TABPC (OPCODE, BUFFER, PCROW, PCCOLS, PCNUMV, NUMPOL,
     *   TIME, TIMINT, SOURCE, ANTNUM, SUBARR, FREQID, CABCAL, STATE,
     *   PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
C-----------------------------------------------------------------------
C! Read, write or close AIPS phase-cal tables opened with PCINI.
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2013
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
C   Read, write or close AIPS phase-cal (PC) tables opened using PCINI.
C
C   Inputs:
C      OPCODE    C*4          Operation code:
C                                'READ' - read a row
C                                'WRIT' - write a row
C                                'CLOS' - close table
C      PCCOLS    I(MAXPCC)    Column pointers from PCINI
C      PCNUMV    I(MAXPCC)    Column dimensions from PCINI
C      NUMPOL    I            Number of polarizations in PC table
C                                (1 or 2)
C
C   Input/output:
C      BUFFER    I(512)       Table I/O block initialized by PCINI
C      PCROW     I            Row to read or write; incremented by one
C                                if the row is read successfully;
C                                ignored on 'CLOS'.
C      TIME      D            Centre-time of PC record in days from
C                                0h on the reference day
C      TIMINT    R            Duration of the time interval covered
C                                by the PC record in days
C      SOURCE    I            Source number (0 if the record applies
C                                to any source)
C      ANTNUM    I            Antenna number to which the record applies
C      SUBARR    I            Subarray number
C      FREQID    I            Frequency group ID number
C      CABCAL    D            Cable calibration delay measurement in
C                                seconds
C      STATE     R(2, 4, MAXIF)
C                             Percentage of time the digitizer spent
C                                in the lowest (1), medium-low (2),
C                                medium-high (3) and highest (4) states
C                                for each IF and polarization
C      PCFREQ    D(2, MAXTON, MAXIF)
C                             Phase tone frequencies for each IF and
C                                polarization
C      PCREAL    R(2, MAXTON, MAXIF)
C                             The real part of each phase cal
C                                measurement in each IF and polarization
C      PCIMAG    R(2, MAXTON, MAXIF)
C                             The imaginary part of each phase cal
C                                measurement in each IF and polarization
C      PCRATE    R(2, MAXTON, MAXIF)
C                             The rate of change of the phase cal phase
C                                in Hz divided by the phase cal
C                                frequency for each tone in each IF
C                                and polarization
C
C   Output:
C      IRET                   Status
C                                0 - operation completed
C                                1 - attempted to read beyond end of
C                                    table
C                                2 - I/O error detected
C                              999 - Invalid call to TABPC
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:PPCV.INC'
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), PCROW, PCCOLS(MAXPCC), PCNUMV(MAXPCC),
     *          NUMPOL, SOURCE, ANTNUM, SUBARR, FREQID, IRET
      DOUBLE PRECISION TIME, CABCAL, PCFREQ(2, MAXTON, MAXIF)
      REAL      TIMINT, STATE(2, 4, MAXIF), PCREAL(2, MAXTON, MAXIF),
     *          PCIMAG(2, MAXTON, MAXIF), PCRATE(2, MAXTON, MAXIF)
C
C     Local variables
C
C     IREC      Integer I/O buffer
C     RREC      Single-precision floating-point I/O buffer
C     DREC      Double-precision floating-point I/O buffer
C     NUMIF     Number of IFs in table
C     NUMTON    Number of tones in table
C     I         Loop index
C     J         Loop index
C     K         Loop index
C
      INTEGER   IREC(PCRSIZ)
      REAL      RREC(PCRSIZ)
      DOUBLE PRECISION DREC(PCRSIZ/2)
      EQUIVALENCE (IREC, RREC, DREC)
      INTEGER   NUMIF, NUMTON
      INTEGER   I, J, K
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NUMIF  = PCNUMV(8) / 4
      NUMTON = PCNUMV(9) / NUMIF
      CALL CHECK ('TABPC ', 1, ((NUMPOL .EQ. 1) .OR. (NUMPOL .EQ. 2)),
     *            IRET)
      IF (IRET .NE. 0) GO TO 999
C
      IF (OPCODE .EQ. 'READ') THEN
         CALL TABIO (OPCODE, TIOROW, PCROW, IREC, BUFFER, IRET)
         IF (IRET.LE.0) THEN
            PCROW  = PCROW + 1
            TIME   = DREC(PCCOLS(1))
            TIMINT = RREC(PCCOLS(2))
            SOURCE = IREC(PCCOLS(3))
            ANTNUM = IREC(PCCOLS(4))
            SUBARR = IREC(PCCOLS(5))
            FREQID = IREC(PCCOLS(6))
            CABCAL = DREC(PCCOLS(7))
            DO 40 I = 1,NUMPOL
               DO 30 K = 1,NUMIF
                  DO 10 J = 1,4
                     STATE(I,J,K) = RREC(PCCOLS(8 + 5 * (I - 1))
     *                  + 4 * (K - 1) + (J - 1))
 10                  CONTINUE
                  DO 20 J = 1,NUMTON
                     PCFREQ(I,J,K) = DREC(PCCOLS(9 + 5 * (I - 1))
     *                  + NUMTON * (K - 1) + (J - 1))
                     PCREAL(I,J,K) = RREC(PCCOLS(10 + 5 * (I - 1))
     *                  + NUMTON * (K - 1) + (J - 1))
                     PCIMAG(I,J,K) = RREC(PCCOLS(11 + 5 * (I - 1))
     *                  + NUMTON * (K - 1) + (J - 1))
                     PCRATE(I,J,K) = RREC(PCCOLS(12 + 5 * (I - 1))
     *                  + NUMTON * (K - 1) + (J - 1))
 20                  CONTINUE
 30               CONTINUE
 40            CONTINUE
         ELSE IF (IRET.EQ.4) THEN
            WRITE (MSGTXT, 9040) PCROW, BUFFER(5)
            CALL MSGWRT (8)
            IRET = 1
         ELSE
            CALL TABERR (OPCODE, 'TABIO ', 'TABPC ', IRET)
            IRET = 2
            END IF
      ELSE IF (OPCODE.EQ.'WRIT') THEN
         DREC(PCCOLS(1)) = TIME
         RREC(PCCOLS(2)) = TIMINT
         IREC(PCCOLS(3)) = SOURCE
         IREC(PCCOLS(4)) = ANTNUM
         IREC(PCCOLS(5)) = SUBARR
         IREC(PCCOLS(6)) = FREQID
         DREC(PCCOLS(7)) = CABCAL
         DO 80 I = 1,NUMPOL
            DO 70 K = 1,NUMIF
               DO 50 J = 1,4
                  RREC(PCCOLS(8 + 5 * (I - 1)) + 4 * (K - 1) + (J - 1))
     *               = STATE(I,J,K)
 50               CONTINUE
               DO 60 J = 1,NUMTON
                  DREC(PCCOLS(9 + 5 * (I - 1))
     *               + NUMTON * (K - 1) + (J - 1)) = PCFREQ(I,J,K)
                  RREC(PCCOLS(10 + 5 * (I - 1))
     *               + NUMTON * (K - 1) + (J - 1)) = PCREAL(I,J,K)
                  RREC(PCCOLS(11 + 5 * (I - 1))
     *               + NUMTON * (K - 1) + (J - 1)) = PCIMAG(I,J,K)
                  RREC(PCCOLS(12 + 5 * (I - 1))
     *               + NUMTON * (K - 1) + (J - 1)) = PCRATE(I,J,K)
 60               CONTINUE
 70            CONTINUE
 80         CONTINUE
         CALL TABIO (OPCODE, TIOROW, PCROW, IREC, BUFFER, IRET)
         IF (IRET.EQ.0) THEN
            PCROW = PCROW + 1
         ELSE
            CALL TABERR (OPCODE, 'TABIO ', 'TABPC ', IRET)
            IRET = 2
            END IF
      ELSE IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, PCROW, IREC, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            CALL TABERR (OPCODE, 'TABIO ', 'TABPC ', IRET)
            IRET = 2
            END IF
      ELSE
         WRITE (MSGTXT, 9080) OPCODE
         CALL MSGWRT (8)
         IRET = 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 9040 FORMAT ('TABPC: ATTEMPTED TO READ ROW ', I6, ' ONLY ', I6,
     *   ' PRESENT')
 9080 FORMAT ('TABPC: UNRECOGNIZED OPCODE ''', A, '''')
      END
