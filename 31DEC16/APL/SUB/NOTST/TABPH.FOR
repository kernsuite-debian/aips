      SUBROUTINE TABPH (OPCODE, BUFFER, PHROW, PHCOLS, PHNUMV, NUMPOL,
     *   TIME, TIMINT, SOURCE, ANTNUM, SUBARR, FREQID, CABCAL, STATE,
     *   PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
C-----------------------------------------------------------------------
C! Read a record from a FITS interchange phase-cal table
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2013, 2015
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
C   Read a single record from a FITS interferometry data interchange
C   phase cal (PH) table that has been opened using PHINI or close a PH
C   table that was opened using PHINI.
C
C   Inputs:
C      OPCODE   C*4          Operation to perform:
C                              'READ' - read a record
C                              'CLOS' - close table
C      PHCOLS   I(MAXPHC)    Column index array from PHINI (ignored
C                              if OPCODE = 'CLOS')
C      PHNUMV   I(MAXPHC)    Column dimensions array from PHINI
C                              (ignored if OPCODE = 'CLOS')
C      NUMPOL   I            Number of polarizations from PHINI (ignored
C                              if OPCODE = 'CLOS').
C   Input/output:
C      BUFFER   I(512)       Table I/O control block and buffers
C                              (will be modified)
C      PHROW    I            Table row to be read if OPCODE = 'READ';
C                              incremented by 1 on exit if the record
C                              was read successfully. Ignored if
C                              OPCODE = 'CLOS'.
C
C   Output:
C      TIME     D            Centre time of record in days
C      TIMINT   R            Interval covered by record in days
C      ANTNUM   I            Antenna number if OPCODE = 'READ'
C      SUBARR   I            Subarray number if OPCODE = 'READ'
C      FREQID   I            Frequency ID if OPCODE = 'READ'
C      CABCAL   D            Cable cal delay measurement in seconds
C      STATE    R(2, 4, MAXIF)
C                            Percentage of time the digitizer spent
C                               in the lowest (1), medium-low (2),
C                               medium-high (3) and highest (4) states
C                               for each IF and polarization
C      PCFREQ   D(2, MAXTON, MAXIF)
C                            Phase tone frequencies for each IF and
C                               polarization
C      PCREAL   R(2, MAXTON, MAXIF)
C                            The real part of each phase cal
C                               measurement in each IF and polarization
C      PCIMAG   R(2, MAXTON, MAXIF)
C                            The imaginary part of each phase cal
C                               measurement in each IF and polarization
C      PCRATE   R(2, MAXTON, MAXIF)
C                            The rate of change of the phase cal phase
C                               in Hz divided by the phase cal
C                               frequency for each tone in each IF
C                               and polarization
C      IRET     I            Return code:
C                              0 - record read (OPCODE = 'READ') or
C                                  table closed (OPCODE = 'CLOS)
C                              1 - PHROW beyond end of table
C                                  (OPCODE = 'WRIT')
C                              2 - I/O error detected
C                            999 - Invalid call to TABPH
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:PPHV.INC'
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), PHROW, PHCOLS(MAXPHC), PHNUMV(MAXPHC),
     *   NUMPOL, SOURCE, ANTNUM, SUBARR, FREQID, IRET
      DOUBLE PRECISION TIME, CABCAL, PCFREQ(2, MAXTON, MAXIF)
      REAL      TIMINT, STATE(2, 4, MAXIF), PCREAL(2, MAXTON, MAXIF),
     *   PCIMAG(2, MAXTON, MAXIF), PCRATE(2, MAXTON, MAXIF)
C                                       Local Variables
C     IREC       Integer valued buffer
C     RREC       Real valued buffer
C     NUMIF      Number of IFs in table
C     NUMTON     Number of tabulated vales in table
C     I          Loop index
C     J          Loop index
C
      INTEGER   IREC(PHRSIZ)
      REAL      RREC(PHRSIZ)
      DOUBLE PRECISION DREC(PHRSIZ / 2)
      INTEGER   NUMIF, NUMTON, I, J, K
      EQUIVALENCE (IREC, RREC, DREC)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (OPCODE .EQ. 'READ') THEN
         CALL TABIO (OPCODE, TIOROW, PHROW, IREC, BUFFER, IRET)
         IF (IRET.LE.0) THEN
            PHROW  = PHROW + 1
            NUMIF  = PHNUMV(8) / 4
            NUMTON = PHNUMV(9) / NUMIF
C
            TIME   = DREC(PHCOLS(1))
            TIMINT = RREC(PHCOLS(2))
            SOURCE = IREC(PHCOLS(3))
            ANTNUM = IREC(PHCOLS(4))
            SUBARR = IREC(PHCOLS(5))
            FREQID = IREC(PHCOLS(6))
            CABCAL = DREC(PHCOLS(7))
            DO 40 I = 1,NUMPOL
               DO 30 K = 1,NUMIF
                  DO 10 J = 1,4
                     STATE(I,J,K) = RREC(PHCOLS(8 + 5 * (I - 1))
     *                  + 4 * (K - 1) + (J - 1))
 10                  CONTINUE
                  DO 20 J = 1,NUMTON
                     PCFREQ(I,J,K) = DREC(PHCOLS(9 + 5 * (I - 1))
     *                  + NUMTON * (K - 1) + (J - 1))
                     PCREAL(I,J,K) = RREC(PHCOLS(10 + 5 * (I - 1))
     *                  + NUMTON * (K - 1) + (J - 1))
                     PCIMAG(I,J,K) = RREC(PHCOLS(11 + 5 * (I - 1))
     *                  + NUMTON * (K - 1) + (J - 1))
                     PCRATE(I,J,K) = RREC(PHCOLS(12 + 5 * (I - 1))
     *                  + NUMTON * (K - 1) + (J - 1))
 20                  CONTINUE
 30               CONTINUE
 40            CONTINUE
         ELSE IF (IRET.EQ.4) THEN
            WRITE (MSGTXT, 9040) PHROW, BUFFER(5)
            CALL MSGWRT (6)
            IRET = 1
         ELSE
            CALL TABERR (OPCODE, 'TABIO ', 'TABPH ', IRET)
            IRET = 2
            END IF
      ELSE IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO (OPCODE, 0, PHROW, IREC, BUFFER, IRET)
         IF (IRET .NE. 0) THEN
            CALL TABERR (OPCODE, 'TABIO ', 'TABPH ', IRET)
            IRET = 2
            END IF
      ELSE
         WRITE (MSGTXT,9041) OPCODE
         CALL MSGWRT (8)
         IRET = 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 9040 FORMAT ('TABPH: ROW ', I6, ' BEYOND END OF TABLE AT ', I6,
     *        ' ROWS')
 9041 FORMAT ('TABPH: UNRECOGNIZED OPCODE ''', A, '''')
      END
