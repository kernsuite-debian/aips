      SUBROUTINE PTIME (TIME, RADS, IT, SEC)
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
C  Routine to translate the time to a form that can be printed.
C  Inputs:
C    PTIME       R        Time in days (or radians)
C    RADS        L        .True. if in radians
C  Outputs:
C    IT          I(3)     Array with days, hours, mins
C    SEC         R        Seconds
C-----------------------------------------------------------------------
      REAL      TIME, SEC
      INTEGER   IT(3)
      LOGICAL   RADS
C
      REAL      DTIME, TIMEX
      DOUBLE PRECISION TPI
C-----------------------------------------------------------------------
      TPI = 2.0 * 3.14159265358979323846D0
      IF (RADS) THEN
         DTIME = TIME / TPI
      ELSE
         DTIME = TIME
         END IF
      IT(1) = DTIME
      TIMEX = (DTIME-IT(1)) * 24.
      IT(2) = TIMEX
      TIMEX = (TIMEX-IT(2)) * 60.
      IT(3) = TIMEX
      TIMEX = (TIMEX-IT(3)) * 60.
      SEC = TIMEX
      IF (SEC.EQ.60.0) THEN
         SEC = 0.0
         IT(3) = IT(3) + 1
         END IF
      IF (IT(3).EQ.60) THEN
         IT(3) = 0
         IT(2) = IT(2) + 1
         END IF
      IF (IT(2).EQ.24) THEN
         IT(2) = 0
         IT(1) = IT(1) + 1
         END IF
C
 999  RETURN
      END
