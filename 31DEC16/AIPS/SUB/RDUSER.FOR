      SUBROUTINE RDUSER (BUF, IRET)
C-----------------------------------------------------------------------
C! reads the user number from the terminal
C# POPS-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2004
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
C   RDUSER reads the user ID # from the terminal
C   Outputs:
C      BUF   I(256)     Scratch buffer
C      IRET  I          Error code: none 0 => die!!!
C   Common:
C      /MSGCOM/ NLUSER is set
C-----------------------------------------------------------------------
      INTEGER   BUF(256), IRET
C
      CHARACTER IXT*1
      INTEGER   IERR, KBP, NC, JTRIM
      DOUBLE PRECISION    X
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DIO.INC'
C-----------------------------------------------------------------------
      NC = 0
      IRET = 1
 10   NC = NC + 1
      IF (NC.GT.10) GO TO 999
         NLUSER = 1
         ERRNUM = 0
         ERRLEV = 0
C                                       request number
         WRITE (MSGTXT,1010)
         CALL MSGWRT (1)
         IXT = '?'
         CALL ZPROMP (IXT, JBUFF, IERR)
         IF (IERR.NE.0) GO TO 10
         KBP = JTRIM (JBUFF)
         KARBUF = JBUFF
         KBP = 1
         CALL GETNUM (KARBUF, NCHLIN, KBP, X)
         IF (ERRNUM.NE.0) GO TO 10
         IF (X.LE.0.) GO TO 10
         NLUSER = X + 0.5
         IF ((NLUSER.LE.0) .OR. (NLUSER.GT.USELIM)) GO TO 10
C                                       Check password
      CALL PASWRD (BUF, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Enter user ID number')
      END
