      SUBROUTINE ZTXIO (OPER, LUN, FIND, LINE, IERR)
C-----------------------------------------------------------------------
C! read/write a line to a text file
C# Z Text
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
C   Read/write the next sequential line from/to a  text file.
C   Inputs:
C      OPER   C*4     Operation code ('READ' or 'WRIT')
C      LUN    I       Logical unit number
C      FIND   I       Index in FTAB for LUN
C   Input/output:
C      LINE   C*(*)   Line of text.  For WRIT, ZTXIO writes the full
C                     string including any trailing blanks.  Use ITRIM
C                     and substring notation in the call if you desire
C                     only up to the last non-blank (which is usually
C                     preferable!).  On READ, adequate size must be
C                     declared in calling routine.
C   Output:
C      IERR   I       Error return code: 0 => no error
C                        1 => file not open
C                        2 => end of file
C                        3 => input error
C                        4 => other I/O error
C   Generic version.
C-----------------------------------------------------------------------
      CHARACTER OPER*4, LINE*(*)
      INTEGER   LUN, FIND, IERR
C
      INTEGER   IOSVAL
      LOGICAL   OPND
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IOSVAL = 0
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
C                                       READ
 20   IF (OPER.EQ.'READ') THEN
         READ (LUN,1020,IOSTAT=IOSVAL) LINE
C                                       End of file (no message).
         IF (IOSVAL.LT.0) THEN
            IERR = 2
C                                       Other I/O error.
         ELSE IF (IOSVAL.GT.0) THEN
            IERR = 4
            WRITE (MSGTXT,1040) IOSVAL
            END IF
C                                       WRITE
      ELSE IF (OPER.EQ.'WRIT') THEN
         WRITE (LUN,1020,IOSTAT=IOSVAL) LINE
         IF (IOSVAL.NE.0) THEN
            IERR = 4
            WRITE (MSGTXT,1050) IOSVAL
            END IF
C                                       Invalid operation code.
      ELSE
         WRITE (MSGTXT,1060) OPER
         IERR = 3
         END IF
C
 995  IF ((IERR.NE.0) .AND. (IERR.NE.2)) CALL MSGWRT (7)
      IF (IOSVAL.GT.0) CALL ZERROR ('ZTXIO', IOSVAL, ' ', -999, .FALSE.)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTXIO: LUN = ',I6,' NOT OPEN IN FTAB')
 1010 FORMAT ('ZTXIO: LUN = ',I6,' NOT OPEN')
 1020 FORMAT (A)
 1040 FORMAT ('ZTXIO: FORTRAN READ ERROR = ',I6)
 1050 FORMAT ('ZTXIO: FORTRAN WRITE ERROR = ',I6)
 1060 FORMAT ('ZTXIO: INVALID OPERATION CODE = ',A4)
      END
