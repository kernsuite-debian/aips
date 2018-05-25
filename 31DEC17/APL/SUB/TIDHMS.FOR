      SUBROUTINE TIDHMS (TIMEIN, NDIG, TIME, RTIME)
C-----------------------------------------------------------------------
C! Converts time to printable form
C# Printer
C-----------------------------------------------------------------------
C;  Copyright (C) 2011
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
C   Convert from Time to Days Hours Minutes Seconds format
C   Input:
C      TIMEIN   R       Input:  Time
C      NDIG     I       Number digits in seconds display
C   Output:
C      TIME     I*(3)   Output Time in Days Hours Minutes
C      RTIME    R       SECONDS
C-----------------------------------------------------------------------
      REAL     TIMEIN, RTIME
      INTEGER  NDIG, TIME(3)
C
      REAL     T
      INTEGER  I, J
C-----------------------------------------------------------------------
      T = TIMEIN
      IF (TIMEIN.LT.0.0) T = -T
C
      TIME(1) = T
      T = (T - TIME(1)) * 24.0
      TIME(2) = T
      T = (T - TIME(2)) * 60.0
      TIME(3) = T
      T = (T - TIME(3)) * 60.0
      J = 10 ** NDIG
      J = MAX (1, J)
      I = J*T + 0.5
      RTIME = FLOAT (I) / FLOAT (J)
C                                       Now Remove 60 seconds
      IF (I.GE.J*60) THEN
         RTIME = RTIME - 60.0
         TIME(3) = TIME(3) + 1
         END IF
C                                       Now Remove 60 minutes
      IF (TIME(3).GE.60) THEN
         TIME(3) = TIME(3) - 60
         TIME(2) = TIME(2) + 1
         END IF
C                                       Now Remove 24 hours
      IF (TIME(2).GE.24) THEN
         TIME(2) = TIME(2) - 24
         TIME(1) = TIME(1) + 1
         END IF
C                                       Sign
      IF (TIMEIN.LT.0.0) TIME(1) = -TIME(1)
C
 999  RETURN
      END
