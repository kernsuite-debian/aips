      SUBROUTINE ZSETUP
C-----------------------------------------------------------------------
C! performs system-level operations after VERNAM, TSKNAM, NPOPS known
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
C   ZSETUP performs any needed system level operations which can be
C   performed only after VERNAM, TSKNAM, and NPOPS have been determined.
C
C   Generic version  - only lowers priority of batch jobs from that of
C   interactive.  If batch are started at the interactive priority,
C   then they can usually go back to interactive whne needed such as
C   while they possess a valuable device (e.g., a real AP).
C-----------------------------------------------------------------------
      INTEGER   IER
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Batch job - Lower priority for
C                                       everything except AIPSB, QMNGR
      IF ((NPOPS.GT.NINTRN+1) .OR. (ISBTCH.EQ.32000)) THEN
         IF ((TSKNAM.NE.'AIPSB') .AND. (TSKNAM.NE.'QMNGR'))
     *      CALL ZPRIO ('DOWN', IER)
         END IF
C
 999  RETURN
      END
