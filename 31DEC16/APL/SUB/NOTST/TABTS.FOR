      SUBROUTINE TABTS (OPCODE, BUFFER, TSROW, TSCOLS, TSNUMV, NUMPOL,
     *   TIME, TIMINT, SOURCE, ANTNUM, SUBARR, FREQID, TSYS, TANT, IRET)
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
C      TSCOLS   I(MAXTSC)    Column index array from TSINI (ignored
C                              if OPCODE = 'CLOS')
C      TSNUMV   I(MAXTSC)    Column dimensions array from TSINI
C                              (ignored if OPCODE = 'CLOS')
C      NUMPOL    I           Number of polarizations from TSINI (ignored
C                              if OPCODE = 'CLOS').
C
C   Input/output:
C      BUFFER   I(512)       Table I/O control block and buffers
C                              (will be modified)
C      TSROW    I            Table row to be read if OPCODE = 'READ';
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
C      TSYS     R(2, MAXIF)  System temperature for each IF and
C                            polarization in kelvin
C      TANT     R(2, MAXIF)  Antenna temperature for each IF and
C                            polarization in kelvin
C      IRET     I            Return code:
C                              0 - record read (OPCODE = 'READ') or
C                                  table closed (OPCODE = 'CLOS)
C                              1 - TSROW beyond end of table
C                                  (OPCODE = 'WRIT')
C                              2 - I/O error detected
C                            999 - Invalid call to TABTS
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:PTSV.INC'
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), TSROW, TSCOLS(MAXTSC), TSNUMV(MAXTSC),
     *   NUMPOL, SOURCE, ANTNUM, SUBARR, FREQID, IRET
      DOUBLE PRECISION TIME
      REAL      TIMINT, TSYS(2, MAXIF), TANT(2, MAXIF)
C                                       Local Variables
C     IREC       Integer valued buffer
C     RREC       Single-precision floating-point buffer
C     DREC       Double-precision floating-point buffer
C     NUMIF      Number of IFs in table
C     I          Loop index
C     J          Loop index
C
      INTEGER   IREC(TSRSIZ)
      REAL      RREC(TSRSIZ)
      DOUBLE PRECISION DREC(TSRSIZ / 2)
      INTEGER   NUMIF, I, J
      EQUIVALENCE (IREC, RREC, DREC)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (OPCODE.EQ.'READ') THEN
         CALL TABIO (OPCODE, TIOROW, TSROW, IREC, BUFFER, IRET)
         IF (IRET.LE.0) THEN
            TSROW  = TSROW + 1
            NUMIF  = TSNUMV(7)
C
            TIME   = DREC(TSCOLS(1))
            TIMINT = RREC(TSCOLS(2))
            SOURCE = IREC(TSCOLS(3))
            ANTNUM = IREC(TSCOLS(4))
            SUBARR = IREC(TSCOLS(5))
            FREQID = IREC(TSCOLS(6))
            DO 20 I = 1,NUMPOL
               DO 10 J = 1,NUMIF
                  TSYS(I,J) = RREC(TSCOLS(7 + 2 * (I - 1)) + (J - 1))
                  TANT(I,J) = RREC(TSCOLS(8 + 2 * (I - 1)) + (J - 1))
 10               CONTINUE
 20            CONTINUE
         ELSE IF (IRET.EQ.4) THEN
            WRITE (MSGTXT, 9020) TSROW, BUFFER(5)
            CALL MSGWRT (6)
            IRET = 1
         ELSE
            CALL TABERR (OPCODE, 'TABIO ', 'TABTS ', IRET)
            IRET = 2
            END IF
      ELSE IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO (OPCODE, 0, TSROW, IREC, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            CALL TABERR (OPCODE, 'TABIO ', 'TABTS ', IRET)
            IRET = 2
            END IF
      ELSE
         WRITE (MSGTXT, 9021) OPCODE
         CALL MSGWRT (8)
         IRET = 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 9020 FORMAT ('TABTS: ROW ', I6, ' BEYOND END OF TABLE AT ', I6,
     *        ' ROWS')
 9021 FORMAT ('TABTS: UNRECOGNIZED OPCODE ''', A, '''')
      END

