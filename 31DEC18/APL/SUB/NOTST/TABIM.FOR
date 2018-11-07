      SUBROUTINE TABIM (OPCODE, BUFFER, ROW, COLIDX, COLDIM, NUMPOL,
     *   TIME, TIMINT, SOURID, ANTNUM, SUBARR, FREQID, IFR, FREQVR,
     *   PDELAY, GDELAY, PRATE, GRATE, DISP, DDISP, IRET)
C-----------------------------------------------------------------------
C! Read, write, or close an open interferometer model table
C# EXT-appl VLBI Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2000, 2008-2009
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
C   Read from, write to, or close an open interferometer model table.
C
C   If OPCODE is 'READ', BUFFER, COLIDX, and COLDIM were initialized
C   by IMINIT, and row number ROW can be read from the table then read
C   row number ROW, increment ROW by 1, and set IRET to 0 if row number
C   ROW is not flagged or to a negative value if row number ROW is
C   flagged.
C
C   If OPCODE is 'READ', BUFFER, COLIDX, and COLDIM were initialized
C   by IMINIT, the table was opened for writing, and row number ROW can
C   be written to the table then write row number ROW, increment ROW by
C   1, and set IRET to 0.
C
C   If OPCODE is 'CLOS', BUFFER, COLIDX, and COLDIM were initialized
C   by IMINIT, and the table can be closed then close the table and set
C   IRET to 0.
C
C   Otherwise, issue one or more error messages and set IRET to a
C   positive value.
C
C   Inputs:
C      OPCODE  C*4            'READ', 'WRIT', or 'CLOS'
C      COLIDX  I(20)          index of first data word for each column
C                             in table record
C      COLDIM  I(20)          array dimension for each column
C      NUMPOL  I              number of polarizations in table (1 or 2)
C
C   Input/Output:
C      BUFFER  I(512)         table I/O control structures and buffer:
C                             updated
C      ROW     I              row number to read or write: incremented
C                             by 1 on successful read or write
C      TIME    D              time in days
C      TIMINT  R              time interval in days
C      SOURID  I              source ID number
C      ANTNUM  I              antenna number
C      SUBARR  I              subarray number
C      FREQID  I              frequency ID number
C      IFR     R              ionospheric Faraday rotation in radians
C                             per meter squared
C      FREQVR  R(*)           time-variable frequency offset for each IF
C                             in Hz
C      PDELAY  D(2, MAXIF, *) phase delay polynomials for each
C                             polarization and IF giving delay in
C                             turns
C      GDELAY  D(2, *)        group delay polynomials for each
C                             polarization giving delay in seconds
C      PRATE   D(2, MAXIF, *) phase rate polynomials for each
C                             polarization and IF giving rate in Hz
C      GRATE   D(2, *)        group rate polynomials for each
C                             polarization giving rate in seconds per
C                             second
C      DISP    R              dispersive delay in seconds / meter^2
C                             scales as wavelength squared
C      DDISP   R              rate of change of dispersive delay
C                             in seconds per second per meter squared
C   Output:
C      IRET    I              status: negative - flagged row read
C                                     0 - unflagged row read or written
C                                     positive - failed
C   Notes:
C
C     Data from the interferometer model table is normally incorporated
C     in the calibration (CL) table.
C
C     See AIPS Memo 102 for more information about the interferometer
C     model table.
C
C     The interferometer model table has dispersive delays for both
C     polarizations. These cannot be different and this routine will
C     write the same values to both polarizations when NUMPOL is 2.
C
C     NOTE: There are also IMINI, IMTAB, and DIMV.INC files constituting
C     a separate way to read/write IM tables.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
C
      CHARACTER        OPCODE*4
      INTEGER          BUFFER(512)
      INTEGER          ROW
      INTEGER          COLIDX(20)
      INTEGER          COLDIM(20)
      INTEGER          NUMPOL
      DOUBLE PRECISION TIME
      REAL             TIMINT
      INTEGER          SOURID
      INTEGER          ANTNUM
      INTEGER          SUBARR
      INTEGER          FREQID
      REAL             IFR
      REAL             FREQVR(*)
      DOUBLE PRECISION PDELAY(2, MAXIF, *)
      DOUBLE PRECISION GDELAY(2, *)
      DOUBLE PRECISION PRATE(2, MAXIF, *)
      DOUBLE PRECISION GRATE(2, *)
      REAL             DISP
      REAL             DDISP
      INTEGER          IRET
C
C     Local variables:
C
C     MAXPLY   maximum polynomial degree
C
C     RECORD   table record viewed as real
C     RECI     table record viewed as integer
C     RECD     table record viewed as double precision
C
C     TERM     polynomial term
C     IFNUM    IF number
C
      INTEGER          MAXPLY
      PARAMETER        (MAXPLY = 20)
C
      REAL             RECORD(2 * (2 + 6 + MAXIF + 4 * MAXIF * MAXPLY
     *                             + 4 * MAXPLY + 4))
      INTEGER          RECI(2 * (2 + 6 + MAXIF + 4 * MAXIF * MAXPLY
     *                           + 4 * MAXPLY + 4))
      DOUBLE PRECISION RECD(2 + 6 + MAXIF + 4 * MAXIF * MAXPLY
     *                      + 4 * MAXPLY + 4)
      EQUIVALENCE (RECORD, RECI, RECD)
C
      INTEGER   TERM
      INTEGER   IFNUM
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (OPCODE .EQ. 'READ') THEN
C
C        If row number ROW can be read then read it into RECORD and set
C        IRET to a negative value if the row is flagged or to 0 if the
C        row is not flagged. If row number ROW can not be read then set
C        IRET to a positive value.
C
         CALL TABIO ('READ', 0, ROW, RECORD, BUFFER, IRET)
C
         IF (IRET .LE. 0) THEN
C
C           The row was read. Unpack the data.
C
            TIME   = RECD(COLIDX(1))
            TIMINT = RECORD(COLIDX(2))
            SOURID = RECI(COLIDX(3))
            ANTNUM = RECI(COLIDX(4))
            SUBARR = RECI(COLIDX(5))
            FREQID = RECI(COLIDX(6))
            IFR    = RECORD(COLIDX(7))
            DISP   = RECORD(COLIDX(13))
            DDISP  = RECORD(COLIDX(14))
            DO 10 TERM = 1, COLDIM(10)
               GDELAY(1, TERM) = RECD(COLIDX(10) + TERM - 1)
               GRATE(1, TERM)  = RECD(COLIDX(12) + TERM - 1)
               IF (NUMPOL .EQ. 2) THEN
                  GDELAY(2, TERM) = RECD(COLIDX(16) + TERM - 1)
                  GRATE(2, TERM)  = RECD(COLIDX(18) + TERM - 1)
                  END IF
   10          CONTINUE
            DO 30 IFNUM = 1, COLDIM(8)
               FREQVR(IFNUM) = RECORD(COLIDX(8) + IFNUM - 1)
               DO 20 TERM = 1, COLDIM(10)
                  PDELAY(1, IFNUM, TERM)
     *               = RECD(COLIDX(9) + COLDIM(10) * (IFNUM - 1)
     *                      + TERM - 1)
                  PRATE(1, IFNUM, TERM)
     *               = RECD(COLIDX(11) + COLDIM(10) * (IFNUM - 1)
     *                      + TERM - 1)
                  IF (NUMPOL .EQ. 2) THEN
                     PDELAY(2, IFNUM, TERM)
     *                  = RECD(COLIDX(15) + COLDIM(10) * (IFNUM - 1)
     *                         + TERM - 1)
                     PRATE(2, IFNUM, TERM)
     *                  = RECD(COLIDX(17) + COLDIM(10) * (IFNUM - 1)
     *                         + TERM - 1)
                     END IF
   20             CONTINUE
   30          CONTINUE
C
            ROW = ROW + 1
         ELSE
C
C           Failed to read row.
C
            WRITE (MSGTXT, 7030) IRET
            CALL MSGWRT (7)
            END IF
      ELSE IF (OPCODE .EQ. 'WRIT') THEN
C
C        Pack values into RECORD:
C
         RECD(COLIDX(1))    = TIME
         RECORD(COLIDX(2))  = TIMINT
         RECI(COLIDX(3))    = SOURID
         RECI(COLIDX(4))    = ANTNUM
         RECI(COLIDX(5))    = SUBARR
         RECI(COLIDX(6))    = FREQID
         RECORD(COLIDX(7))  = IFR
         RECORD(COLIDX(13)) = DISP
         RECORD(COLIDX(14)) = DDISP
         IF (NUMPOL .EQ. 2) THEN
            RECORD(COLIDX(19)) = DISP
            RECORD(COLIDX(20)) = DDISP
            END IF
         DO 110 TERM = 1, COLDIM(10)
            RECD(COLIDX(10) + TERM - 1) = GDELAY(1, TERM)
            RECD(COLIDX(12) + TERM - 1) = GRATE(1, TERM)
            IF (NUMPOL .EQ. 2) THEN
               RECD(COLIDX(16) + TERM - 1) = GDELAY(2, TERM)
               RECD(COLIDX(18) + TERM - 1) = GRATE(2, TERM)
               END IF
  110       CONTINUE
         DO 130 IFNUM = 1, COLDIM(8)
            RECORD(COLIDX(8) + IFNUM - 1) = FREQVR(IFNUM)
            DO 120 TERM = 1, COLDIM(10)
               RECD(COLIDX(9) + COLDIM(10) * (IFNUM - 1) + TERM - 1)
     *            = PDELAY(1, IFNUM, TERM)
               RECD(COLIDX(11) + COLDIM(10) * (IFNUM - 1) + TERM - 1)
     *            = PRATE(1, IFNUM, TERM)
               IF (NUMPOL .EQ. 2) THEN
                  RECD(COLIDX(15) + COLDIM(10) * (IFNUM - 1) + TERM - 1)
     *               = PDELAY(2, IFNUM, TERM)
                  RECD(COLIDX(17) + COLDIM(10) * (IFNUM - 1) + TERM - 1)
     *               = PRATE(2, IFNUM, TERM)
                  END IF
  120          CONTINUE
  130       CONTINUE
C
C        If row number ROW can be written the write it and set IRET to
C        zero otherwise set IRET to a positive number:
C
         CALL TABIO ('WRIT', 0, ROW, RECORD, BUFFER, IRET)
C
         IF (IRET .EQ. 0) THEN
            ROW = ROW + 1
         ELSE
            WRITE (MSGTXT, 7130) IRET
            CALL MSGWRT (7)
            END IF
      ELSE IF (OPCODE .EQ. 'CLOS') THEN
C
C        If the table can be closed then close it and set IRET to zero,
C        otherwise set IRET to a positive number:
C
         CALL TABIO ('CLOS', 0, ROW, RECORD, BUFFER, IRET)
C
         IF (IRET .NE. 0) THEN
            WRITE (MSGTXT, 7200) IRET
            CALL MSGWRT (7)
            END IF
      ELSE
C
C        Unrecognized OPCODE.
C
         WRITE (MSGTXT, 7300) OPCODE
         CALL MSGWRT (7)
         IRET = 1
         END IF
C-----------------------------------------------------------------------
 7030 FORMAT ('TABIM: FAILED TO READ ROW (ERROR ', I4, ')')
 7130 FORMAT ('TABIM: FAILED TO WRITE ROW (ERROR ', I4, ')')
 7200 FORMAT ('TABIM: FAILED TO CLOSE TABLE (ERROR ', I4, ')')
 7300 FORMAT ('TABIM: UNKNOWN OPCODE ''', A4, '''')
      END
