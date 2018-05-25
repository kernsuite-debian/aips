      SUBROUTINE VBOUT (N, IDATA, FDVEC, NUM, IERR)
C-----------------------------------------------------------------------
C! writes variable length, blocked records of 16-bit integers to tape
C# Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   VBOUT writes variable blocked records of data to tape.  Maximum
C   block size on the tape is 4008 bytes.  Tape may be opened by TAPIO
C   (OP='OPWT') before first call.  For overlaid programs COMMON /VBCOM/
C   should be kept in a segment which is core-resident from the first
C   to the last call to VBOUT.  A call with N = 0 will cause all data
C   remaining in the buffer to be written.  Character data must be in
C   ASCII two characters per integer as local integers: ie call
C   ZCLC8 followed by ZI16IL on such data before calling VBOUT.
C   Inputs:
C      N      I       Number of words in array IDATA.
C                     If N = 0 the buffer is flushed.
C      IDATA  I       Array containing data to be written.
C      FDVEC  I(50)   Field descriptor vector for TAPIO
C      NUM    I       The record number to be written, must be 1 on
C                     the first and only the first record in a file.
C   Output:
C      IERR   I       Return error code 0 => OK, else TAPIO error.
C   Programmer: W. D. Cotton, July 1981.
C-----------------------------------------------------------------------
      INTEGER   N, IDATA(*), FDVEC(50), NUM, IERR
C
      INTEGER   NLREC, VBUFF(4096), VTBIND, VLIND, ICB(2)
      INCLUDE 'INCS:DMSG.INC'
      COMMON /VBCOM/ VTBIND, VLIND, VBUFF
      DATA NLREC /2004/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Check if this is first call
C                                       Initialize record pointers
      IF (NUM.GT.1) GO TO 30
         VLIND = 3
         VTBIND = 1
C                                       Check if time for a write.
 30   IF ((N.GT.0) .AND. (VLIND+N+2.LE.NLREC+1)) GO TO 100
C                                       Add block length info
         ICB(1) = VLIND*2 - 2
         ICB(2) = 0
         CALL ZILI16 (2, ICB, 1, VBUFF(VTBIND))
C                                       Write
         CALL TAPIO ('WRIT', FDVEC, VBUFF, VTBIND, IERR)
         IF (IERR.EQ.0) GO TO 50
            WRITE (MSGTXT,1030) IERR
            GO TO 980
C                                       Reset record pointer.
 50      VLIND = 3
C                                       See if this was last call
         IF (N.LE.0) GO TO 999
C                                       Put record into buffer
C                                       Control bytes.
 100  ICB(1) = N*2 + 4
      ICB(2) = 0
      CALL ZILI16 (2, ICB, VLIND, VBUFF(VTBIND))
      VLIND = VLIND + 2
C                                       Copy record to buffer.
      CALL ZILI16 (N, IDATA, VLIND, VBUFF(VTBIND))
      VLIND = VLIND + N
      GO TO 999
C                                       Error occured.
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('VBOUT: TAPIO ERROR',I7,' WRITING OUTPUT FILE')
      END
