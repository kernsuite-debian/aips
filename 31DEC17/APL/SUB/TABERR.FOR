      SUBROUTINE TABERR (OPCODE, ROUTIN, CALLRT, IERR)
C-----------------------------------------------------------------------
C! provides a standard error message for the table routines
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2001, 2006
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
C   Provides a standard error message for the temporary table routines.
C   Messages will be suppressed if MSGSUP=32000 has been set
C   Inputs:
C      OPCODE   C*4   OPCODE being used in the table handling routine.
C      ROUTIN   C*6   Subroutine which returned the error code.
C      CALLRT   C*6   Name of calling subroutine
C      IERR     I     The error code returned by said subroutine.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*(*), ROUTIN*(*), CALLRT*(*)
      INTEGER   IERR
C
C     INTEGER   MSGKEP
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C     MSGKEP = MSGSUP
C     MSGSUP = 0
      WRITE (MSGTXT,1000) CALLRT, IERR, ROUTIN, OPCODE
      CALL MSGWRT (6)
C     MSGSUP = MSGKEP
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A,': ERROR ',I3,' FROM ',A,' OPCODE = ',A4)
      END
