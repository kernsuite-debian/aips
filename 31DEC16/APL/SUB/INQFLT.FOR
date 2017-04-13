      SUBROUTINE INQFLT (TTY, PROMPT, NUM, DVAL, IERR)
C-----------------------------------------------------------------------
C! inquire of the user for specified number of floating-point values
C# Service
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
C   INQire from the user for FLoaTing point input with initial PROMPT
C   and with more prompting to get all requested values.
C   Inputs:
C      TTY     I(2)     (1) TTY LUN of terminal opened with ZOPEN.
C                       (2) TTY FTAB index of open terminal.
C      PROMPT  C*72     Prompt.
C      NUM     I        Number of values on an input line.
C   Output:
C      DVAL    D(NUM)   Results
C      IERR    I       > 0 => terminal I/O error;  0 => ok;
C                      < 0 => format error in on DVAL(-IERR)
C-----------------------------------------------------------------------
      INTEGER   TTY(2), NUM, IERR
      CHARACTER PROMPT*72
      DOUBLE PRECISION DVAL(*)
C
      DOUBLE PRECISION X
      CHARACTER KB*72
      INTEGER   I, KBP, IN, NMATCC
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (IERR.NE.0) GO TO 999
      KBP = 1
      DO 20 I = 1,NUM
 10      IN = NMATCC (KBP, ' ', KB)
         IF (IN.GT.72) THEN
            KB = '****  MORE VALUES PLEASE : '
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, KB, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
            IF (IERR.NE.0) GO TO 999
            KBP = 1
            GO TO 10
            END IF
         KBP = IN
         CALL GETNUM (KB, 72, KBP, X)
         IF (X.NE.DBLANK) THEN
            DVAL(I) = X
         ELSE
            IERR = -I
            GO TO 999
            END IF
 20      CONTINUE
C
 999  RETURN
      END
