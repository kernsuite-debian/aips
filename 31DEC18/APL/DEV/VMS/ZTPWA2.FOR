      SUBROUTINE ZTPWA2 (FCB, IERR)
C-----------------------------------------------------------------------
C! wait for read/write from/to a tape device
C# Z2 IO-basic Tape
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
C   Wait for the completion of an asynchronous tape I/O operation and
C   returns its completion status and 8-bit byte count.
C   Inputs:
C      FCB    I(*)   File I/O control block
C   Output:
C      IERR   I      Error return  0 => ok
C                       3 => IO error
C                       4 => end of file
C                       5 => beginning of tape
C                       6 => end of tape
C                       7 => wait service error
C   VMS version
C-----------------------------------------------------------------------
      INTEGER   FCB(*), IERR
C
      INTEGER   JERR, SYS$WAITFR, ITEMP(2), ZAND
      INTEGER*2 I2TEMP(4)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE '($MTDEF)'
      INCLUDE '($SSDEF)'
      EQUIVALENCE (ITEMP, I2TEMP)
C-----------------------------------------------------------------------
      IERR = 0
C                                       Wait for IO completion
      JERR = SYS$WAITFR (%VAL(FCB(1+FCBSP1)))
C                                       Check error in service
      IF (JERR.NE.1) THEN
         IERR = 7
         FCB(1+FCBERR) = JERR
         FCB(1+FCBXFR) = 0
C                                       check IO errors
      ELSE
         ITEMP(1) = FCB(1+FCBSP2)
         ITEMP(2) = FCB(2+FCBSP2)
         JERR = I2TEMP(1)
         FCB(1+FCBXFR) = I2TEMP(2)
         IF (FCB(1+FCBXFR).LT.0) FCB(1+FCBXFR) = FCB(1+FCBXFR) + 65536
C                                       apparent no error, check status

         IF (JERR.EQ.1) THEN
            IERR = 0
            FCB(1+FCBERR) = 0
            IF (ZAND(MT$M_EOF,ITEMP(2)).NE.0) IERR = 4
            IF (ZAND(MT$M_BOT,ITEMP(2)).NE.0) IERR = 5
            IF (ZAND(MT$M_EOT,ITEMP(2)).NE.0) IERR = 6
C                                       trap long tape record
         ELSE IF (JERR.EQ.SS$_DATAOVERUN) THEN
            IERR = 0
            FCB(1+FCBERR) = 0
            FCB(1+FCBXFR) = FCB(1+FCBXFR) + 1
            WRITE (MSGTXT,1000)
            CALL MSGWRT (6)
         ELSE
            IERR = 3
            FCB(1+FCBERR) = JERR
            IF (JERR.EQ.SS$_ENDOFFILE) IERR = 4
            IF (JERR.EQ.SS$_ENDOFTAPE) IERR = 6
            IF (JERR.EQ.SS$_ENDOFVOLUME) IERR = 4
            IF (ZAND(MT$M_EOF,ITEMP(2)).NE.0) IERR = 4
            IF (ZAND(MT$M_BOT,ITEMP(2)).NE.0) IERR = 5
            IF (ZAND(MT$M_EOT,ITEMP(2)).NE.0) IERR = 6
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTPWA2 WARNING: TAPE RECORD LONGER THAN REQUESTED')
      END
