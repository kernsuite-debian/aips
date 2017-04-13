      SUBROUTINE TABFL (OPCODE, BUFFER, FLROW, FLCOLS, FLNUMV, TABREV,
     *   NUMIF, SOURID, SUBARR, ANTS, FREQID, TIMRAN, IFFLGS, CHNRAN,
     *   PFLAGS, REASON, SEVRTY, IRET)
C-----------------------------------------------------------------------
C! Read a record from a FITS interchange flag table
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
C   Read a single record from a FITS interferometry data interchange
C   flag (FL) table that has been opened using FLINI or close an FL
C   table that was opened using FLINI.
C
C   Inputs:
C      OPCODE   C*4          Operation to perform:
C                              'READ' - read a record
C                              'CLOS' - close table
C      FLCOLS   I(MAXFLC)    Column index array from FLINI (ignored
C                              if OPCODE = 'CLOS')
C      FLNUMV   I(MAXFLC)    Column dimensions array from FLINI
C                              (ignored if OPCODE = 'CLOS')
C      TABREV   I            Table revision number from FLINI (ignored
C                              if OPCODE = 'CLOS')
C      NUMIF    I            Number of IFs from FLINI (ignored if
C                              OPCODE = 'CLOS'). NOTE: not the number
C                              of IFs in parent data file.
C
C   Input/output:
C      BUFFER   I(512)       Table I/O control block and buffers
C                              (will be modified)
C      FLROW    I            Table row to be read if OPCODE = 'READ';
C                              incremented by 1 on exit if the record
C                              was read successfully. Ignored if
C                              OPCODE = 'CLOS'.
C
C   Output:
C      SOURID   I            Source ID number if OPCODE = 'READ'
C      SUBARR   I            Subarray number if OPCODE = 'READ'
C      ANTS     I(2)         Antennae numbers if OPCODE = 'READ'
C      FREQID   I            Frequency ID if OPCODE = 'READ'
C      TIMRAN   R(2)         Timerange in days if OPCODE = 'READ'
C      IFFLGS   L(MAXIF)     IF flags if OPCODE = 'READ'
C      CHNRAN   I(2)         Channel range if OPCODE = 'READ'
C      PFLAGS   L(4)         Polarization flags
C      REASON   C*40         Reason if OPCODE = 'READ'
C      SEVRTY   I            Severity level if OPCODE = 'READ'
C      IRET     I            Return code:
C                              0 - record read (OPCODE = 'READ') or
C                                  table closed (OPCODE = 'CLOS)
C                              1 - FLROW beyond end of table
C                                  (OPCODE = 'WRIT')
C                              2 - I/O error detected
C                            999 - Invalid call to TABFL
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:PFLV.INC'
C
      CHARACTER OPCODE*4, REASON*40
      INTEGER   BUFFER(512), FLROW, FLCOLS(MAXFLC), FLNUMV(MAXFLC),
     *          TABREV, NUMIF, SOURID, SUBARR, ANTS(2), FREQID,
     *          CHNRAN(2), SEVRTY, IRET
      REAL      TIMRAN(2)
      LOGICAL   IFFLGS(MAXIF), PFLAGS(4)
C
C     Local Variables
C
C     IREC       Integer valued buffer
C     RREC       Real valued buffer
C     BANDS      Band range (revision 1 tables)
C     NCAR       Number of characters to read from REASON field
C     I          Loop index
C
      INTEGER   IREC(FLRSIZ)
      REAL      RREC(FLRSIZ)
      INTEGER   BANDS(2), NCAR, I
      EQUIVALENCE (IREC, RREC)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (OPCODE .EQ. 'READ') THEN
         CALL TABIO (OPCODE, TIOROW, FLROW, IREC, BUFFER, IRET)
         IF (IRET.LE.0) THEN
            FLROW = FLROW + 1
            SOURID = IREC(FLCOLS(1))
            SUBARR = IREC(FLCOLS(2))
            CALL COPY (2, IREC(FLCOLS(3)), ANTS)
            FREQID = IREC(FLCOLS(4))
            CALL RCOPY (2, RREC(FLCOLS(5)), TIMRAN)
            IF (TABREV .EQ. 1) THEN
               CALL COPY (2, IREC(FLCOLS(6)), BANDS)
               DO 10 I = 1, NUMIF
                  IFFLGS(I) = ((BANDS(1) .LE. I)
     *               .AND. (I .LE. BANDS(2)))
   10             CONTINUE
            ELSE IF (TABREV .EQ. 2) THEN
               DO 20 I = 1, NUMIF
                  IFFLGS(I) = IREC(FLCOLS(6) + I - 1) .NE. 0
   20             CONTINUE
            ELSE
               WRITE (MSGTXT, 9020) TABREV
               CALL MSGWRT (8)
               IRET = 999
            END IF
            CALL COPY (2, IREC(FLCOLS(7)), CHNRAN)
            DO 30 I = 1, 4
               PFLAGS(I) = IREC(FLCOLS(8) + I - 1) .NE. 0
   30          CONTINUE
            REASON = ' '
            NCAR = MIN(LEN(REASON), FLNUMV(9))
            CALL H2CHR(NCAR, 1, IREC(FLCOLS(9)), REASON(1:NCAR))
            SEVRTY = IREC(FLCOLS(10))
         ELSE IF (IRET.EQ.4) THEN
            WRITE (MSGTXT, 9030) FLROW, BUFFER(5)
            CALL MSGWRT (6)
            IRET = 1
         ELSE
            CALL TABERR (OPCODE, 'TABIO', 'TABFL', IRET)
            IRET = 2
            END IF
      ELSE IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO (OPCODE, 0, FLROW, IREC, BUFFER, IRET)
         IF (IRET .NE. 0) THEN
            CALL TABERR (OPCODE, 'TABIO', 'TABFL', IRET)
            IRET = 2
            END IF
      ELSE
         WRITE (MSGTXT,9031) OPCODE
         CALL MSGWRT (8)
         IRET = 999
         END IF
C-----------------------------------------------------------------------
 9020 FORMAT ('FLINI: CAN NOT READ FL TABLE REVISION ', I3)
 9030 FORMAT ('FLINI: ROW ', I6, ' BEYOND END OF TABLE AT ', I6,
     *        ' ROWS')
 9031 FORMAT ('FLINI: UNRECOGNIZED OPCODE ''', A, '''')
      END


