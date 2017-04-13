      SUBROUTINE TABBS (OPCODE, BUFFER, BSRNO, BSCOLS, BSNUMV, NUMIF,
     *   TIME, TIMINT, BASELN, SUBARR, STOKES, SOURCE, VAMP, SAMP, RMBD,
     *   MBDERR, MBDAMB, RSBD, SBDERR, SBDAMB, RRATE, RTERR, RTAMB,
     *   RACCEL, ACCERR, RPHASE, PHSERR, IRET)
C-----------------------------------------------------------------------
C! Read from or write to a baseline fringe solution (BS) table
C# Ext-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   Read record number BSRNO from an open baseline fringe solution (BS)
C   table if OPCODE is 'READ', write record number BSRNO in an open
C   BS table if OPCODE is 'WRIT' or close an open BS table if OPCODE is
C   'CLOS'.
C
C   Inputs:
C      OPCODE   C*4           Operation code:
C                                'READ' - read a row from the table
C                                'WRIT' - write a row to the table
C                                'CLOS' - close the table
C      BSCOLS   I(*)          Column index from BSINI.
C      BSNUMV   I(*)          Column dimensions from BSINI.
C      NUMIF    I             Number of IFs in table.
C
C   Input/output:
C      BUFFER   I(1024)       I/O buffer and control structures.
C                              This is created by BSINI.
C      BSRNO    I             On input: record number to read or write
C                             On output: next record to read or write
C      TIME     D             Central time for record in days since
C                              0h UT on the reference date.
C      TIMINT   R             Time interval covered by record in days.
C      BASELN   I(2)          Antenna in interferemeter baseline.
C      SUBARR   I             Subarray number
C      STOKES   I             Stokes code: -1 - RR   -2 - LL
C                                          -3 - RL   -4 - LR
C                                          -5 - XX   -6 - YY
C                                          -7 - XY   -8 - YX
C      SOURCE   I             Source ID number
C      VAMP     R(*)          Vector amplitude/Jy
C      SAMP     R(*)          Scalar amplitude/Jy
C      RMBD     R             Residual multiband delay/sec
C      MBDERR   R             Multiband delay error/sec
C      MBDAMB   R             Multiband delay ambiguity/sec
C      RSBD     R(*)          Residual single-band delay/sec
C      SBDERR   R(*)          Single-band delay error/sec
C      SBDAMB   R             Single-band delay ambiguity/sec
C      RRATE    R(*)          Residual fringe rate/Hz
C      RTERR    R(*)          Fringe rate error/Hz
C      RTAMB    R             Fringe rate ambiguity/Hz
C      RACCL    R(*)          Residual fringe acceleration/Hz**2
C      ACCERR   R(*)          Fringe acceleration error/Hz**2
C      RPHASE   R(*)          Residual phase/degrees
C      PHSERR   R(*)          Phase error/degrees
C
C   Output:
C      IRET     I             Status:  0 - OK
C                                     -1 - row flagged (on READ)
C                                      1 - attempt to read beyond
C                                          end of table
C                                      2 - error detected
C                                      3 - unknown opcode
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(1024), BSRNO, BSCOLS(*), BSNUMV(*), NUMIF,
     *   BASELN(2), SUBARR, STOKES, SOURCE, IRET
      DOUBLE PRECISION TIME
      REAL      TIMINT, VAMP(*), SAMP(*), RMBD, MBDERR, MBDAMB, RSBD(*),
     *   SBDERR(*), SBDAMB, RRATE(*), RTERR(*), RTAMB, RACCEL(*),
     *   ACCERR(*), RPHASE(*), PHSERR(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTAB.INC'
C                                        DREC = record interpreted as
C                                               double precision
C                                        RREC = record interpreted as
C                                               real
C                                        IREC = record interpreted as
C                                               integers
      DOUBLE PRECISION DREC(5)
      REAL      RREC(11 + 3 * MAXIF)
      INTEGER   IREC(11 + 3 * MAXIF)
      EQUIVALENCE (IREC, RREC, DREC)
C
      INTEGER   I
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (OPCODE.EQ.'READ') THEN
         CALL TABIO ('READ', TIOROW, BSRNO, IREC, BUFFER, IRET)
         IF (IRET.EQ.4) THEN
            IRET = 1
         ELSE IF (IRET.GT.0) THEN
            IRET = 2
            WRITE (MSGTXT, 1000)
            CALL MSGWRT(8)
         ELSE
            BSRNO = BSRNO + 1
            TIME = DREC(BSCOLS(1))
            TIMINT = RREC(BSCOLS(2))
            DO 10 I = 1, 2
               BASELN(I) = IREC(BSCOLS(3) + I - 1)
   10          CONTINUE
            SUBARR = IREC(BSCOLS(4))
            STOKES = IREC(BSCOLS(5))
            SOURCE = IREC(BSCOLS(6))
            RMBD = RREC(BSCOLS(9))
            MBDERR = RREC(BSCOLS(10))
            MBDAMB = RREC(BSCOLS(11))
            SBDAMB = RREC(BSCOLS(14))
            RTAMB = RREC(BSCOLS(17))
            DO 20 I = 1, NUMIF
               VAMP(I) = RREC(BSCOLS(7) + I - 1)
               SAMP(I) = RREC(BSCOLS(8) + I - 1)
               RSBD(I) = RREC(BSCOLS(12) + I - 1)
               SBDERR(I) = RREC(BSCOLS(13) + I - 1)
               RRATE(I) = RREC(BSCOLS(15) + I - 1)
               RTERR(I) = RREC(BSCOLS(16) + I - 1)
               RACCEL(I) = RREC(BSCOLS(18) + I - 1)
               ACCERR(I) = RREC(BSCOLS(19) + I - 1)
               RPHASE(I) = RREC(BSCOLS(20) + I - 1)
               PHSERR(I) = RREC(BSCOLS(21) + I - 1)
   20          CONTINUE
            IRET = 0
            END IF
      ELSE IF (OPCODE.EQ. 'WRIT') THEN
         DREC(BSCOLS(1)) = TIME
         RREC(BSCOLS(2)) = TIMINT
         DO 30 I = 1, 2
            IREC(BSCOLS(3) + I - 1) = BASELN(I)
   30       CONTINUE
         IREC(BSCOLS(4)) = SUBARR
         IREC(BSCOLS(5)) = STOKES
         IREC(BSCOLS(6)) = SOURCE
         RREC(BSCOLS(9)) = RMBD
         RREC(BSCOLS(10)) = MBDERR
         RREC(BSCOLS(11)) = MBDAMB
         RREC(BSCOLS(14)) = SBDAMB
         RREC(BSCOLS(17)) = RTAMB
         DO 40 I = 1, NUMIF
            RREC(BSCOLS(7) + I - 1) = VAMP(I)
            RREC(BSCOLS(8) + I - 1) = SAMP(I)
            RREC(BSCOLS(12) + I - 1) = RSBD(I)
            RREC(BSCOLS(13) + I - 1) = SBDERR(I)
            RREC(BSCOLS(15) + I - 1) = RRATE(I)
            RREC(BSCOLS(16) + I - 1) = RTERR(I)
            RREC(BSCOLS(18) + I - 1) = RACCEL(I)
            RREC(BSCOLS(19) + I - 1) = ACCERR(I)
            RREC(BSCOLS(20) + I - 1) = RPHASE(I)
            RREC(BSCOLS(21) + I - 1) = PHSERR(I)
   40       CONTINUE
         CALL TABIO ('WRIT', TIOROW, BSRNO, IREC, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT, 1040)
            CALL MSGWRT (8)
            END IF
         BSRNO = BSRNO + 1
      ELSE IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', TIOROW, BSRNO, IREC, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT, 1041)
            CALL MSGWRT (8)
            END IF
      ELSE
         IRET = 3
         WRITE (MSGTXT, 1042) OPCODE
         CALL MSGWRT (8)
         END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TABBS: ERROR READING FROM BASELINE SOLUTION TABLE')
 1040 FORMAT ('TABBS: ERROR WRITING TO BASELINE SOLUTION TABLE')
 1041 FORMAT ('TABBS: ERROR CLOSING BASELINE SOLUTION TABLE')
 1042 FORMAT ('TABBS: DO NOT RECOGNISE OPCODE ''', A4, '''')
      END
