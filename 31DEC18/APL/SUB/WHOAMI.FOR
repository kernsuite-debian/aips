      SUBROUTINE WHOAMI (INAME, INUM, IERR)
C-----------------------------------------------------------------------
C! given root task name, gets actual task name and finds NPOPS number
C# System
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
C   WHOAMI obtains the actual task name, compares it to the root task
C   name, and determines the POPS number.
C   Inputs:
C      INAME  C*6   Root task name
C   Output:
C      INUM   I     POPS number
C      IERR   I     Error code: 0 => ok
C                      1 => illegal root
C                      2 => roots don't match
C                      3 => INUM out of range, illegal
C                      4 => other
C-----------------------------------------------------------------------
      CHARACTER INAME*6
      INTEGER   INUM, IERR
C
      INTEGER   IT, IP
      CHARACTER CNAME*6, CTEST*1
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Legal root?
      DO 10 IP = 1,6
         IF (INAME(IP:IP).EQ.' ') GO TO 20
 10      CONTINUE
      IERR = 1
      GO TO 999
C                                       Get real name
 20   IERR = 4
      CALL ZGNAME (CNAME, IT)
      IF (IT.NE.0) GO TO 999
C                                       Roots match?
      IP = IP - 1
      IERR = 2
      IF (IP.LT.1) GO TO 999
      IF (CNAME(1:IP).NE.INAME(1:IP)) GO TO 999
C                                       Get number
      IP = IP + 1
      CTEST = CNAME(IP:IP)
      IP = NINTRN
      IF (NBATQS.GT.0) IP = NINTRN + 1 + NBATQS
      CALL ZHEX10 (CTEST, INUM, IERR)
      IF ((INUM.LT.1) .OR. (INUM.GT.IP) .OR. (IERR.NE.0)) IERR = 3
C
 999  RETURN
      END
