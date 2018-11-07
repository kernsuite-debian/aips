      SUBROUTINE ZTTYIO (OPER, LUN, FIND, NCHARS, BUFF, IERR)
C-----------------------------------------------------------------------
C! read/write buffer to terminal
C# Terminal
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
C   Perform I/O to a terminal.  Used by MSGWRT so cannot use MSGWRT.
C   Inputs:
C      OPER     C*4   Operation code 'READ' or 'WRIT'
C      LUN      I     Logical unit number
C      FIND     I     Index in FTAB to file control block for LUN
C      NCHARS   I     # characters to transfer (<= 132)
C   In/out:
C      BUFF     C*(*) I/O buffer containing characters (1-256)
C   Output:
C      IERR     I     Error return code: 0 => no error
C                        1 => file not open
C                        2 => input error
C                        3 => I/O error
C                        4 => end of file
C   Generic version: formatted IO does carriage control on output based
C   on parameter TTYCAR.
C-----------------------------------------------------------------------
      CHARACTER OPER*4, BUFF*(*)
      INTEGER   LUN, FIND, NCHARS, IERR
C
      INTEGER   IOSVAL
      LOGICAL   OPND
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 0
C                                       Valid LUN?
      IF ((LUN.EQ.5) .OR. (LUN.EQ.6)) GO TO 10
         IERR = 2
         WRITE (6,*) 'ZTTYIO: INVALID LUN = ',LUN
         GO TO 999
C                                       LUN open in FTAB?
 10   IF (FTAB(FIND).EQ.LUN) GO TO 20
         IERR = 1
         WRITE (6,*) 'ZTTYIO: LUN = ',LUN,' NOT OPEN IN FTAB'
         GO TO 999
C                                       LUN open?
 20   INQUIRE (UNIT=LUN, OPENED=OPND)
      IF (OPND) GO TO 30
         IERR = 1
         WRITE (6,*) 'ZTTYIO: LUN = ',LUN,' NOT OPEN'
         GO TO 999
C                                       Valid NCHARS?
 30   IF ((NCHARS.GT.0) .AND. (NCHARS.LE.MIN(256,LEN(BUFF)))) GO TO 40
         IERR = 2
         WRITE (6,*) 'ZTTYIO: INVALID NCHARS = ',NCHARS
         GO TO 999
C                                       Do read
 40   IF (OPER.EQ.'READ') THEN
         READ (LUN,1000,IOSTAT=IOSVAL) BUFF(1:NCHARS)
C                                       End of file (no message).
         IF (IOSVAL.LT.0) THEN
            IERR = 4
C                                       I/O error.
         ELSE IF (IOSVAL.NE.0) THEN
            IERR = 3
            WRITE (6,*) 'ZTTYIO: FORTRAN READ ERROR = ',IOSVAL,
     *         ' ON LUN = ',LUN
            CALL ZMSGER ('ZTTYIO', IOSVAL, ' ', -999, .FALSE.)
            END IF
C                                       Write to terminal.
      ELSE IF (OPER.EQ.'WRIT') THEN
         IF (TTYCAR.GT.0) THEN
            WRITE (LUN,1010,IOSTAT=IOSVAL) BUFF(1:NCHARS)
         ELSE
            WRITE (LUN,1000,IOSTAT=IOSVAL) BUFF(1:NCHARS)
            END IF
         IF (IOSVAL.NE.0) THEN
            IERR = 3
            WRITE (6,*) 'ZTTYIO: FORTRAN WRITE ERROR = ',IOSVAL,
     *         ' ON LUN = ',LUN
            CALL ZMSGER ('ZTTYIO', IOSVAL, ' ', -999, .FALSE.)
            END IF
C                                       Invalid operation.
      ELSE
         IERR = 2
         WRITE (6,*) 'ZTTYIO: INVALID OPERATION'
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A)
 1010 FORMAT (' ',A)
      END
