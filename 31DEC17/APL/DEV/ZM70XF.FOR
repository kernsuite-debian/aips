      SUBROUTINE ZM70XF (OPER, NBYTES, HEADER, BUFFER, IERR)
C-----------------------------------------------------------------------
C! read/write data to IIS Model 70 TV with buffering
C# Z IO-TV
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
C   Transfer data between an IIS Model 70 TV device (writes are buffered
C   unless OP = FLSH, reads cause a FLSH first).
C   Inputs:
C      OPER     C*4    Operation code 'READ', 'WRIT' or 'FLSH'
C                      'FLSH' ignores NBYTES, HEADER, BUFFER and just
C                      writes whatever has accumulated
C      NBYTES   I      Number AIPS bytes to transfer (can be zero)
C      HEADER   I(8)   IIS Model 70 control header
C   In/Out:
C      BUFFER   I(*)   I/O buffer
C   Output:
C      IERR     I      Error return code: 0 => no error
C                         1 => file not open
C                         2 => input error
C                         3 => I/O error
C                         4 => end of file
C                         7 => timeout/inoperative
C   Generic version; handles buffering, calls ZM70X2 for actual IO
C   Conversion to 16-bit integers is done by ZIPACK, called from here
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   NBYTES, HEADER(8), BUFFER(*), IERR
C
      INTEGER   SUM, FCBOFF, IOP, NBHDR, I, NWORD, NBBUF
      INTEGER   NXB, MXB, XB(4200)
      LOGICAL   MAP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DZCH.INC'
      COMMON /XBM70/ NXB, MXB, XB
      DATA MAP /.FALSE./
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 2
C                                       Valid operation code?
      IF ((OPER.EQ.'READ') .OR. (OPER.EQ.'WRIT') .OR. (OPER.EQ.'FLSH'))
     *   GO TO 10
         WRITE (MSGTXT,1000) OPER
         GO TO 995
C                                       Valid byte request?
 10   IF ((OPER.EQ.'FLSH') .OR. ((NBYTES.GE.0) .AND.
     *   (MOD (NBYTES, 2).EQ.0))) GO TO 20
         WRITE (MSGTXT,1010) NBYTES
         GO TO 995
C                                       Device open?
 20   IF (FTAB(TVIND).EQ.TVLUN) GO TO 30
         IERR = 1
         WRITE (MSGTXT,1020)
         GO TO 995
C                                       Okay, so far.
 30   IERR = 0
      NWORD = 0
      FCBOFF = TVIND + NMOFF
C                                       Set checksum word.
      IF (OPER.NE.'FLSH') THEN
         HEADER(4) = 1
         SUM = 0
         DO 40 I = 1,8
            SUM = SUM - HEADER(I)
 40         CONTINUE
         HEADER(4) = SUM
         NBHDR = 16
         NWORD = 8
         IF (OPER.EQ.'WRIT') NWORD = 8 + NBYTES/2
         END IF
C                                       Time to dump buffer?
 50   IF ((OPER.NE.'WRIT') .OR. (NXB+NWORD.GT.MXB)) THEN
C                                       Read command fits in buffer.
         IF ((OPER.EQ.'READ') .AND. (NXB+NWORD.LE.MXB)) THEN
            NXB = NXB + 1
            CALL ZIPACK ('LTOS', NWORD, HEADER, NXB, XB)
            NXB = NXB + NWORD - 1
            NWORD = 0
            END IF
C                                       Write buffer
         IOP = 1
         IF (NXB.GT.0) THEN
            NBBUF = 2 * NXB
            CALL ZM70X2 ('WRIT', FTAB(FCBOFF), XB, NBBUF, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         NXB = 0
         IF (OPER.EQ.'FLSH') GO TO 999
         IF ((OPER.EQ.'READ') .AND. (NWORD.GT.0)) GO TO 50
         END IF
C                                       Read data.
      IF (OPER.EQ.'READ') THEN
         IOP = 2
         IF (NBYTES.GT.0) THEN
            CALL ZM70X2 ('READ', FTAB(FCBOFF), XB, NBYTES, IERR)
            IF (IERR.NE.0) GO TO 910
            NWORD = NBYTES / 2
            CALL ZIPACK ('STOL', NWORD, BUFFER, 1, XB)
            END IF
C                                       WRITE:
      ELSE
C                                       Copy into buffer for later
         IF (NWORD+NBYTES/2.LE.MXB) THEN
            NXB = NXB + 1
            CALL ZIPACK ('LTOS', 8, HEADER, NXB, XB)
            NXB = NXB + 7
            NWORD = NBYTES / 2
            NXB = NXB + 1
            CALL ZIPACK ('LTOS', NWORD, BUFFER, NXB, XB)
            NXB = NXB + NWORD - 1
            IERR = 0
C                                       do long buffer now
         ELSE
            NBBUF = 16
            CALL ZIPACK ('LTOS', 8, HEADER, 1, HEADER)
            CALL ZM70X2 ('WRIT', FTAB(FCBOFF), HEADER, NBBUF, IERR)
            IF (IERR.NE.0) GO TO 900
            NWORD = NBYTES / 2
            CALL ZIPACK ('LTOS', NWORD, BUFFER, 1, BUFFER)
            CALL ZM70X2 ('WRIT', FTAB(FCBOFF), BUFFER, NBYTES, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         END IF
      GO TO 999
C                                       Error handling.
C                                       Buffer write error.
 900  IF (IOP.EQ.2) GO TO 910
         WRITE (MSGTXT,1900) IERR
         GO TO 920
C                                       Data read error.
 910  CONTINUE
         WRITE (MSGTXT,1910) IERR, OPER
C                                       Print message.
 920  CALL MSGWRT (7)
C                                       Process system error.
      CALL ZERROR ('ZM70X2', FTAB(FCBOFF+FCBERR), ' ', FTAB(FCBOFF),
     *   MAP)
C                                       Master clear.
      CALL ZM70MC (FTAB(FCBOFF))
      GO TO 999
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZM70XF: INVALID OPERATION CODE = ',A4)
 1010 FORMAT ('ZM70XF: INVALID BYTE REQUEST = ',I6)
 1020 FORMAT ('ZM70XF: TV DEVICE NOT OPEN')
 1900 FORMAT ('ZM70XF: ZM70X2 RETURNS ERROR ',I1,' ON WRIT BUFFER')
 1910 FORMAT ('ZM70XF: ZM70X2 RETURNS ERROR ',I1,' ON ',A4,' DATA')
      END
