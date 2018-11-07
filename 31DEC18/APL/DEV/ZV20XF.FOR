      SUBROUTINE ZV20XF (OPER, NBYTES, COMST, COMDB, BUFFER, IERR)
C-----------------------------------------------------------------------
C! read/write data to Comtal Vision 1/20 TV device
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
C   Transfer data between a Comtal Vision 1/20 TV device
C   Inputs:
C      OPER     C*4    Operation code 'READ', 'WRIT'
C      NBYTES   I      Number AIPS bytes to transfer (can be zero)
C      COMST    I      Value in the CSR word of the ZB interface.
C      COMDB    I      The value to be placed on the R lines.
C   In/Out:
C      BUFFER   I(*)   I/O buffer
C   Output:
C      IERR     I      Error return code: 0 => no error
C                         1 => file not open
C                         2 => input error
C                         3 => I/O error
C                         4 => end of file
C                         7 => timeout/inoperative
C   Generic version; calls ZV20X2 for actual IO
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   NBYTES, COMST, COMDB, BUFFER(*), IERR
C
      INTEGER   FCBOFF, NWORD
      LOGICAL   MAP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA MAP /.FALSE./
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 2
C                                       Valid operation code?
      IF ((OPER.EQ.'READ') .OR. (OPER.EQ.'WRIT'))
     *   GO TO 10
         WRITE (MSGTXT,1000) OPER
         GO TO 995
C                                       Valid byte request?
 10   IF ((NBYTES.GE.0) .AND. (MOD (NBYTES, 2).EQ.0)) GO TO 20
         WRITE (MSGTXT,1010) NBYTES
         GO TO 995
C                                       Device open?
 20   IF (FTAB(TVIND).EQ.TVLUN) GO TO 30
         IERR = 1
         WRITE (MSGTXT,1020)
         GO TO 995
C                                       Okay, so far.
 30   IERR = 0
      NWORD = NBYTES / 2
      FCBOFF = TVIND + NMOFF
C                                       Pack buffer to send to TV
      IF (OPER.EQ.'WRIT') CALL ZIPACK ('LTOS', NWORD, BUFFER, 1, BUFFER)
C                                       Do I/O
      CALL ZV20X2 (OPER, FTAB(FCBOFF), NBYTES, COMST, COMDB, BUFFER,
     *   IERR)
C                                       Unpack read buffer
      IF (OPER.EQ.'READ') CALL ZIPACK ('STOL', NWORD, BUFFER, 1, BUFFER)
C                                       Error handling.
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1910) IERR, OPER
         CALL MSGWRT (7)
C                                       Process system error.
         CALL ZERROR ('ZV20X2', FTAB(FCBOFF+FCBERR), ' ', FTAB(FCBOFF),
     *      MAP)
C                                       Master clear.
         CALL ZV20MC (FTAB(FCBOFF))
         GO TO 999
         END IF
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZV20XF: INVALID OPERATION CODE = ',A4)
 1010 FORMAT ('ZV20XF: INVALID BYTE REQUEST = ',I6)
 1020 FORMAT ('ZV20XF: TV DEVICE NOT OPEN')
 1910 FORMAT ('ZV20XF: ZV20X2 RETURNS ERROR ',I1,' ON ',A4,' DATA')
      END
