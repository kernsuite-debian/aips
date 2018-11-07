      SUBROUTINE UWRITE (FDVEC, TBIND, TAPBUF, BUF, IERR)
C-----------------------------------------------------------------------
C! writes summary of UV Export-format tape's first records
C# Tape UV-util
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
C   UWRITE writes a little about an Export UV tape's first records to
C   the message file.  First record has been read - but for safety we
C   BAKF the tape anyway and reset FDVEC parms for this variable length
C   format.
C   In/Out:
C      FDVEC   I(50)       TAPIO control parms - 1st record read
C      TBIND   I           Pointer to data record in TAPBUF
C      TAPBUF  I(*)        TAPIO I/O buffer - input with 1st record
C   Output:
C      BUF     I(2004)     Work buffer: 1 header record
C      IERR    I           Error from IO or 10 => bad data in rec.
C-----------------------------------------------------------------------
      INTEGER   FDVEC(50), TBIND, TAPBUF(1), BUF(2004), IERR
C
      CHARACTER SOURCE*8, OBS*4, DATE*12, CHSIGN*1, CHSLGN*1
      INTEGER   NWD, IS, IN, ITYPES(2,2), ITYP, RM(2), DM(2)
      REAL      RSEC, DSEC, RTEMP(2)
      DOUBLE PRECISION    XDAT
      LOGICAL   FIRST
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Init pointers
      IS = 20
      IN = -3
      NWD = 0
      FIRST = .TRUE.
      CALL ZCLC8 (4, 'RUN ', 1, BUF)
      CALL ZCLC8 (4, 'SOUR', 5, BUF)
      CALL ZI16IL (4, 1, BUF, ITYPES)
C                                       Init tape read
      CALL TAPIO ('BAKF', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      FDVEC(2) = 4008
      FDVEC(6) = 1
      FDVEC(31) = 1
      FDVEC(32) = 0
C                                       Locate logical record
 50   IF (FIRST) GO TO 60
         ITYP = 7
         IF ((BUF(IS-4).EQ.ITYPES(1,1)) .AND. (BUF(IS-3).EQ.
     *      ITYPES(2,1))) ITYP = 1
         IF ((BUF(IS-4).EQ.ITYPES(1,2)) .AND. (BUF(IS-3).EQ.
     *      ITYPES(2,2))) ITYP = 2
         IF (BUF(IS-6).LE.1) GO TO 990
         IS = IS + BUF(IS-6) / 2
C                                       Read tape record
 60   IF ((FIRST) .OR. (IS.GT.NWD)) THEN
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZI16IL (1, 1, TAPBUF(TBIND), IN)
         NWD = IN / 2
         IF ((NWD.LE.1) .OR. (NWD.GT.2004)) GO TO 990
         CALL ZI16IL (NWD, 1, TAPBUF(TBIND), BUF)
         IS = 9
         IF (FIRST) THEN
            ITYP = 7
            IF ((BUF(IS-4).EQ.ITYPES(1,1)) .AND. (BUF(IS-3).EQ.
     *         ITYPES(2,1))) ITYP = 1
            IF ((BUF(IS-4).EQ.ITYPES(1,2)) .AND. (BUF(IS-3).EQ.
     *         ITYPES(2,2))) ITYP = 2
            IS = 15
            FIRST = .FALSE.
            END IF
         END IF
C                                       Branch to type
 90   IF (ITYP.EQ.1) GO TO 100
      IF (ITYP.EQ.2) GO TO 200
      GO TO 50
C                                       RUN records
 100  CONTINUE
         CALL ZILI16 (2, BUF(IS), 1, RTEMP)
         CALL ZC8CL (4, 1, RTEMP, OBS)
         CALL ZR8P4 ('4IB8', BUF(IS+4), XDAT)
         XDAT = XDAT + 2400000.5D0
         CALL GREG (XDAT, SOURCE)
         CALL DATDAT (SOURCE, DATE)
         WRITE (MSGTXT,1100) OBS, DATE
         CALL MSGWRT (3)
         GO TO 50
C                                       SOUR records: do first
 200  CONTINUE
         CALL ZILI16 (4, BUF(IS), 1, RTEMP)
         CALL ZC8CL (8, 1, RTEMP, SOURCE)
         IN = BUF(IS+4)
         CALL ZR8P4 ('4IB8', BUF(IS+18), XDAT)
         XDAT = 360.0D0 * (2.0D0 ** (-31)) * XDAT
         CALL COORDD (1, XDAT, CHSIGN, RM, RSEC)
         CALL ZR8P4 ('4IB8', BUF(IS+20), XDAT)
         XDAT = 360.0D0 * (2.0D0 ** (-31)) * XDAT
         CALL COORDD (2, XDAT, CHSLGN, DM, DSEC)
         CALL ZR8P4 ('4IB8', BUF(IS+22), XDAT)
         XDAT = XDAT * 1.D-6
         WRITE (MSGTXT,1200) SOURCE, IN, CHSIGN, RM, RSEC, CHSLGN, DM,
     *      DSEC, XDAT
         IF (MSGTXT(28:28).EQ.' ') MSGTXT(28:28) = '0'
         IF (MSGTXT(42:42).EQ.' ') MSGTXT(42:42) = '0'
         CALL MSGWRT (3)
         GO TO 999
C                                       Format error
 990  WRITE (MSGTXT,1990)
      CALL MSGWRT (8)
      IERR = 10
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('RUN observer = ''',A4,'''   date = ',A12)
 1200 FORMAT ('First ',A8,I4,' :',A1,I2.2,I3.2,F7.3,1X,A1,I2.2,I3.2,
     *   F6.2,2X,'F=',F10.6,' GHZ')
 1990 FORMAT ('ERROR IN POINTERS INTERNAL TO DATA RECORD')
      END
