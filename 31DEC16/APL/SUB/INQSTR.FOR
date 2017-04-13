      SUBROUTINE INQSTR (TTY, PROMPT, LEN, OUTSTR, IERR)
C-----------------------------------------------------------------------
C! request character string from user (1st N characters of line)
C# Service
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2010
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
C   INQire from the user for a STRing input - not in quotes, just the
C   first LEN characters of the input line
C   Inputs:
C      TTY      I(2)   (1) TTY LUN of terminal opened with ZOPEN.
C                      (2) TTY FTAB index of open terminal.
C      PROMPT   C*72   Prompt string.
C      LEN      I      Requested number of characters
C  Output:
C      OUTSTR   C*(*)  Resultant string, 1st LEN chars from user
C                      terminal and any others blank filled.
C      IERR     I      Error code: 10 => input longer than requested
C-----------------------------------------------------------------------
      INTEGER   TTY(2), LEN, IERR
      CHARACTER PROMPT*72, OUTSTR*(*)
C
      CHARACTER INPBUF*256
      INTEGER   J, JTRIM
C-----------------------------------------------------------------------
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
      IF (IERR.EQ.0) CALL ZTTYIO ('READ', TTY(1), TTY(2), 256, INPBUF,
     *   IERR)
      IF (IERR.EQ.0) THEN
         J = JTRIM (INPBUF)
         OUTSTR = INPBUF(1:LEN)
         IF (J.GT.LEN) IERR = 10
      ELSE
         OUTSTR = ' '
         END IF
C
 999  RETURN
      END
