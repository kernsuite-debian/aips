      SUBROUTINE BLDTNM (NAME, IPOPS, ANAME, IERR)
C-----------------------------------------------------------------------
C! constructs full task name by appending NPOPS to task root name
C# Utility
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
C   BLDTNM builds a task name unique to each initiator from the root
C   Inputs:  NAME   C*6     root task name (<= 5 char)
C            IPOPS  I       pops number
C   Outputs: ANAME  C*6     actual name
C            IERR   I       error code: 0 -> ok
C                                       1 -> name too long
C                                       2 -> IPOPS out of range
C   concatanates pops # to root name
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), ANAME*(*)
      INTEGER   IPOPS, IERR
C
      INTEGER   IJK
C-----------------------------------------------------------------------
C                                       Check IPOPS valid
      IERR = 2
      IF ((IPOPS.LT.1) .OR. (IPOPS.GT.35)) GO TO 999
C                                       Find last non-blank
      IJK = INDEX (NAME, ' ')
      IF (IJK.EQ.0) IJK = LEN (NAME) + 1
C                                       Check for 1-5 chars
      IERR = 1
      IF ((IJK.LT.2) .OR. (IJK.GT.6)) GO TO 999
C                                       Build name
      ANAME = NAME(1:IJK-1)
      CALL ZEHEX (IPOPS, 1, ANAME(IJK:IJK))
      IERR = 0
C
 999  RETURN
      END
