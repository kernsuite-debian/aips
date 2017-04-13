      SUBROUTINE ZTREAD (LUN, FIND, RBUFF, IERR)
C-----------------------------------------------------------------------
C! read next 80-character record in sequential text file (ZTOPEN type)
C# Text
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
C   Read the next sequential 80-character card image from a text file.
C   Inputs:
C      LUN     I       Logical unit number
C      FIND    I       Index in FTAB for LUN
C   Output:
C      RBUFF   C*(*)   I/O buffer for card image
C      IERR    I       Error return code: 0 => no error
C                         1 => file not open
C                         2 => end of file
C                         4 => other I/O error
C   Generic version - assumes simple Fortran IO.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
      CHARACTER RBUFF*(*)
C
      INTEGER   IOSVAL
      LOGICAL   OPND
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Is LUN open in FTAB?
      IF (FTAB(FIND).EQ.LUN) GO TO 10
         IERR = 1
         WRITE (MSGTXT,1000) LUN
         GO TO 995
C                                       Is LUN open?
 10   INQUIRE (UNIT=LUN, OPENED=OPND)
      IF (OPND) GO TO 20
         IERR = 1
         WRITE (MSGTXT,1010) LUN
         GO TO 995
C                                       Read next card image.
 20   READ (LUN,1020,IOSTAT=IOSVAL) RBUFF
      IF (IOSVAL.EQ.0) GO TO 999
C                                       End of file (no message).
      IF (IOSVAL.LT.0) THEN
         IERR = 2
         GO TO 999
C                                       Other I/O error.
      ELSE
         IERR = 4
         WRITE (MSGTXT,1030) IOSVAL
         END IF
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTREAD: LUN = ',I6,' NOT OPEN IN FTAB')
 1010 FORMAT ('ZTREAD: LUN = ',I6,' NOT OPEN')
 1020 FORMAT (A)
 1030 FORMAT ('ZTREAD: FORTRAN READ ERROR = ',I6)
      END
