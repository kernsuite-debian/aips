      SUBROUTINE ZDELA2 (SECS, IERR)
C-----------------------------------------------------------------------
C! delay current process a specified interval
C# Z2 Service
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
C   Cause an execution delay for a specified time interval.
C   Inputs:
C      SECS   R   Number of seconds to delay
C   Output:
C      IERR   I   Error return code: 0 => no error
C                    1 => error
C   VMS version - error 2 if > 1 hour.
C-----------------------------------------------------------------------
      REAL      SECS
      INTEGER   IERR
C
      INTEGER   ISEC(2), EFN, JERR, SYS$SETIMR, SYS$WAITFR
      REAL      RSEC, LSEC
C-----------------------------------------------------------------------
      IERR = 2
      IF ((SECS.LE.0.0005) .OR. (SECS.GT.3599.)) GO TO 999
      IERR = 0
      LSEC = SECS
C                                       loop no more than 200 seconds
 10   RSEC = MIN (200.0, LSEC)
C                                       quad integer
         ISEC(1) = RSEC * 1000.0 + 0.5
         ISEC(2) = -1
         ISEC(1) = -10000 * ISEC(1)
         EFN = 0
         JERR = SYS$SETIMR (%VAL(EFN), ISEC, , )
         IF (JERR.EQ.1) JERR = SYS$WAITFR (%VAL(EFN))
         IF (JERR.NE.1) IERR = 1
         LSEC = LSEC - RSEC
         IF ((IERR.EQ.0) .AND. (LSEC.GT.0.0005)) GO TO 10
C
 999  RETURN
      END
