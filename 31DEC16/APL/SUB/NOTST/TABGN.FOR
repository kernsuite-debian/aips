      SUBROUTINE TABGN (OPCODE, BUFFER, GNROW, GNCOLS, GNNUMV, NUMPOL,
     *   ANTNUM, SUBARR, FREQID, TYPE, NTERM, XTYPE, YTYPE, XVALUE,
     *   YVALUE, GAIN, SENS, IRET)
C-----------------------------------------------------------------------
C! Read a record from a FITS interchange gain curve table
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2007, 2013
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
C   gain curve (GN) table that has been opened using GNINI or close a GN
C   table that was opened using GNINI.
C
C   Inputs:
C      OPCODE   C*4          Operation to perform:
C                              'READ' - read a record
C                              'CLOS' - close table
C      GNCOLS   I(MAXGNC)    Column index array from GNINI (ignored
C                              if OPCODE = 'CLOS')
C      GNNUMV   I(MAXGNC)    Column dimensions array from GNINI
C                              (ignored if OPCODE = 'CLOS')
C      NUMPOL    I           Number of polarizations from GNINI (ignored
C                              if OPCODE = 'CLOS').
C
C   Input/output:
C      BUFFER   I(512)       Table I/O control block and buffers
C                              (will be modified)
C      GNROW    I            Table row to be read if OPCODE = 'READ';
C                              incremented by 1 on exit if the record
C                              was read successfully. Ignored if
C                              OPCODE = 'CLOS'.
C
C   Output:
C      ANTNUM   I            Antenna number if OPCODE = 'READ'
C      SUBARR   I            Subarray number if OPCODE = 'READ'
C      FREQID   I            Frequency ID if OPCODE = 'READ'
C      TYPE     I(2, MAXIF)  Gain curve type for each polarization and
C                              IF if OPCODE = 'READ'
C      NTERM    I(2, MAXIF)  Number of terms for each polarization and
C                              IF if OPCODE = 'READ'
C      XTYPE    I(2, MAXIF)  X-value type for each polarization and
C                              IF if OPCODE = 'READ'
C      YTYPE    I(2, MAXIF)  Y-value type for each polarization and
C                              IF if OPCODE = 'READ'
C      XVALUE   R(2, MAXIF)  Tabulated x-values in degrees for each
C                              polarization and IF if OPCODE = 'READ'
C      YVALUE   R(2, MXTBGN, MAXIF)
C                            Tabulated y-values in degrees for each
C                              polarization and IF if OPCODE = 'READ'
C      GAIN     R(2, MXTBGN, MAXIF)
C                            Tabulated gains corresponding to
C                              tabulated y-values if OPCODE = 'READ'
C      SENS     R(2, MAXIF)  Sensitivities in K/Jy for each polarization
C                              and IF if OPCODE = 'READ'
C      IRET     I            Return code:
C                              0 - record read (OPCODE = 'READ') or
C                                  table closed (OPCODE = 'CLOS)
C                              1 - GNROW beyond end of table
C                                  (OPCODE = 'WRIT')
C                              2 - I/O error detected
C                            999 - Invalid call to TABGN
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:PGNV.INC'
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), GNROW, GNCOLS(MAXGNC), GNNUMV(MAXGNC),
     *          NUMPOL, ANTNUM, SUBARR, FREQID, TYPE(2, MAXIF),
     *          NTERM(2, MAXIF), XTYPE(2, MAXIF), YTYPE(2, MAXIF), IRET
      REAL      XVALUE(2, MAXIF), YVALUE(2, MXTBGN, MAXIF),
     *          GAIN(2, MXTBGN, MAXIF), SENS(2, MAXIF)
C
C     Local Variables
C
C     IREC       Integer valued buffer
C     RREC       Real valued buffer
C     NUMIF      Number of IFs in table
C     NTABS      Number of tabulated vales in table
C     I          Loop index
C     J          Loop index
C
      INTEGER   IREC(GNRSIZ)
      REAL      RREC(GNRSIZ)
      INTEGER   NUMIF, NTABS, I, J
      EQUIVALENCE (IREC, RREC)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (OPCODE .EQ. 'READ') THEN
         CALL TABIO (OPCODE, TIOROW, GNROW, IREC, BUFFER, IRET)
         IF (IRET.LE.0) THEN
            GNROW = GNROW + 1
            NUMIF = GNNUMV(4)
            NTABS = GNNUMV(9) / NUMIF
            ANTNUM = IREC(GNCOLS(1))
            SUBARR = IREC(GNCOLS(2))
            FREQID = IREC(GNCOLS(3))
            DO 20 I = 1,NUMIF
               TYPE(1,I)   = IREC(GNCOLS(4) + I - 1)
               NTERM(1,I)  = IREC(GNCOLS(5) + I - 1)
               XTYPE(1,I)  = IREC(GNCOLS(6) + I - 1)
               YTYPE(1,I)  = IREC(GNCOLS(7) + I - 1)
               XVALUE(1,I) = RREC(GNCOLS(8) + I - 1)
               DO 10 J = 1, NTABS
                  YVALUE(1,J,I) = RREC(GNCOLS(9) + NTABS * (I - 1)
     *               + J - 1)
                  GAIN(1,J,I)   = RREC(GNCOLS(10) + NTABS * (I - 1)
     *               + J - 1)
   10             CONTINUE
               SENS(1,I)   = RREC(GNCOLS(11) + I - 1)
   20          CONTINUE
            IF (NUMPOL.EQ.2) THEN
               DO 40 I = 1,NUMIF
                  TYPE(2,I)   = IREC(GNCOLS(12) + I - 1)
                  NTERM(2,I)  = IREC(GNCOLS(13) + I - 1)
                  XTYPE(2,I)  = IREC(GNCOLS(14) + I - 1)
                  YTYPE(2,I)  = IREC(GNCOLS(15) + I - 1)
                  XVALUE(2,I) = RREC(GNCOLS(16) + I - 1)
                  DO 30 J = 1, NTABS
                     YVALUE(2,J,I) = RREC(GNCOLS(17) + NTABS * (I - 1)
     *                  + J - 1)
                     GAIN(2,J,I)   = RREC(GNCOLS(18) + NTABS * (I - 1)
     *                  + J - 1)
   30                CONTINUE
                  SENS(2,I)   = RREC(GNCOLS(19) + I - 1)
   40             CONTINUE
               END IF
         ELSE IF (IRET.EQ.4) THEN
            WRITE (MSGTXT,9040) GNROW, BUFFER(5)
            CALL MSGWRT (6)
            IRET = 1
         ELSE
            CALL TABERR (OPCODE, 'TABIO ', 'TABGN ', IRET)
            IRET = 2
            END IF
      ELSE IF (OPCODE .EQ. 'CLOS') THEN
         CALL TABIO (OPCODE, 0, GNROW, IREC, BUFFER, IRET)
         IF (IRET .NE. 0) THEN
            CALL TABERR (OPCODE, 'TABIO ', 'TABGN ', IRET)
            IRET = 2
            END IF
      ELSE
         WRITE (MSGTXT, 9041) OPCODE
         CALL MSGWRT (8)
         IRET = 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 9040 FORMAT ('TABGN: ROW ', I6, ' BEYOND END OF TABLE AT ', I6,' ROWS')
 9041 FORMAT ('TABGN: UNRECOGNIZED OPCODE ''', A, '''')
      END
