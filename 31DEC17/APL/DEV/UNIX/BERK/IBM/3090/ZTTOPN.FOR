      SUBROUTINE ZTTOPN (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! open a terminal device
C# Z2 Terminal
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
C   Open a terminal - does not use MSGWRT because it is called by
C   ZMSGOP.  Does do the FTAB open here (not to be done in ZOPEN).
C   Inputs:
C      LUN    I   Logical unit number (5 or 6)
C   Output:
C      FIND   I   Position in FTAB
C      IERR   I   Error return code: 0 => no error
C                    1 => Fortran open error
C                    2 => input error
C   Generic UNIX version - has UNIX-dependent assumptions that LUN 0 is
C   standard error output and that LUN 6 is already open as the output
C   terminal (but is reassigned via ZTTOP2 if a message terminal is
C   specified).
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   XLEN, XLNB, JERR, IOSVAL, I
      CHARACTER DEVTTY*16, TASKTT*8, XLATED*128
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:VSAT.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Check inputs.
      FIND = 0
      IF ((LUN.EQ.5) .OR. (LUN.EQ.6)) GO TO 10
         IERR = 2
         GO TO 999
C                                       Don't reopen if already open
 10   CALL LSERCH ('SRCH', LUN, FIND, F, IERR)
      IF (IERR.EQ.0) GO TO 999
      IERR = 0
C                                       Open in FTAB
      CALL LSERCH ('OPEN', LUN, FIND, F, JERR)
      IF (JERR.NE.0) FIND = 0
C                                       Default terminal.
      DEVTTY = '/dev/tty'
C                                       Always use the default terminal
C                                       for LUN = 5.
      IERR = 0
      GO TO 900
      IF (LUN.EQ.6) THEN
C                                       If stand-alone, don't reassign
         GO TO 900
         DO 15 I = 1,NSANAM
            IF (TSKNAM.EQ.SATASK(I)) GO TO 900
 15         CONTINUE
C                                       Get message terminal device
C                                       assignment (if any).
         TASKTT = 'TASKTT'
         CALL ZEHEX (NPOPS, 1, TASKTT(7:7))
         XLEN = LEN (XLATED)
         CALL ZTRLOG (7, TASKTT, XLEN, XLATED, XLNB, JERR)
C                                       If not defined, use default
C                                       error not checked further
         IF (JERR.NE.0) THEN
            OPEN (UNIT=LUN, FILE=DEVTTY(:8), STATUS='OLD',
     *         IOSTAT=IOSVAL)
            IOSVAL = 0
         ELSE
            DEVTTY = XLATED(1:XLNB)
C                                       Terminal reassignment requires
C                                       special treatment.
            CALL ZTTOP2 (XLNB, DEVTTY, IOSVAL)
            END IF
C                                       LUN 5
      ELSE
C                                       Make sure LUN is closed.
C         CLOSE (LUN)
C                                       Then re-open it (this makes it
C                                       open read/write).
C         OPEN (UNIT=LUN, FILE=DEVTTY(:8), STATUS='OLD', IOSTAT=IOSVAL)
         END IF
C                                       Error?
      IF (IOSVAL.NE.0) THEN
         IERR = 1
C                                       Only to "stderr".
         WRITE (0,1020) IOSVAL, LUN
         WRITE (0,1021) DEVTTY
C                                       Close in FTAB
         CALL LSERCH ('CLOS', LUN, FIND, F, JERR)
         END IF
C
 900  IF (IERR.EQ.0) FTAB(FIND+1+NFCBER) = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (' ZTTOPN: FORTRAN OPEN ERROR = ',I6,' FOR LUN = ',I1)
 1021 FORMAT (' ZTTOPN: DEVICE = ',A)
      END
