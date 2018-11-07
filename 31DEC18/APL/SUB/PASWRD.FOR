      SUBROUTINE PASWRD (BUF, IRET)
C-----------------------------------------------------------------------
C! prompts for and checks password if the user has a non-blank PW entry
C# System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002, 2004
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
C   PASWRD interogates the password file for the existence of a non-
C   blank password for user NLUSER.  If there is one, it prompts the
C   user for the password and reads the answer (via ZPASWD) and then
C   compares it to the one stored on disk.
C   Outputs:
C      BUF   I(256)    Scratch buffer
C      IRET  I         Error code: 0 => you may continue
C                               1 => I/O error
C                               2 => input error
C                               3 => password fault
C-----------------------------------------------------------------------
      INTEGER   BUF(256), IRET
C
      CHARACTER BLANK*12, PHNAME*48, UPASS*12
      HOLLERITH APASS(3), TPASS(3)
      INTEGER   NLPR, LUN, IND, IERR, IER, I, J, IREC
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN /15/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 2
      IF ((NLUSER.LT.1) .OR. (NLUSER.GT.USELIM)) GO TO 999
C                                       Find encoded password
      J = 3
      NLPR = 256 / J
      IREC = (NLUSER - 1) / NLPR + 1
      IRET = 1
      CALL ZPHFIL ('PW', 1, 0, 0, PHNAME, IERR)
      CALL ZOPEN (LUN, IND, 1, PHNAME, F, F, T, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZFIO ('READ', LUN, IND, IREC, BUF, IERR)
      CALL ZCLOSE (LUN, IND, IER)
      IF (IERR.NE.0) GO TO 999
      I = NLUSER - (IREC-1) * NLPR
      I = (I - 1) * J + 1
      CALL COPY (J, BUF(I), APASS)
C                                       is it blank?
      BLANK = ' '
      CALL PASENC (BLANK, TPASS)
      IRET = 0
      IF (TPASS(1).NE.APASS(1)) GO TO 20
      IF (TPASS(2).NE.APASS(2)) GO TO 20
      IF (TPASS(3).EQ.APASS(3)) GO TO 999
C                                       NO: read a password
 20   IRET = 1
      CALL ZPASWD (UPASS, BUF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL PASENC (UPASS, TPASS)
      IRET = 0
      IF (TPASS(1).NE.APASS(1)) GO TO 30
      IF (TPASS(2).NE.APASS(2)) GO TO 30
      IF (TPASS(3).EQ.APASS(3)) GO TO 999
C                                       Bad password: try again
 30   WRITE (MSGTXT,1030)
      CALL MSGWRT (2)
      IRET = 1
      CALL ZPASWD (UPASS, BUF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL PASENC (UPASS, TPASS)
      IRET = 0
      IF (TPASS(1).NE.APASS(1)) GO TO 40
      IF (TPASS(2).NE.APASS(2)) GO TO 40
      IF (TPASS(3).EQ.APASS(3)) GO TO 999
C                                       Bad password: quit
 40   WRITE (MSGTXT,1040)
      CALL MSGWRT (9)
      IRET = 3
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('PASSWORD DOES NOT MATCH --- TRY AGAIN')
 1040 FORMAT ('PASSWORD DOES NOT MATCH --- QUITTING')
      END
