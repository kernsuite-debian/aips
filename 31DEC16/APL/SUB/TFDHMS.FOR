      SUBROUTINE TFDHMS (TIMEIN, NDIG, TSIGN, TIME, SECNDS)
C-----------------------------------------------------------------------
C! Converts time to print format of greater accuracy than integer sec
C# Utility coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
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
C   Convert from Time to Sign Days Hours Minutes Seconds format
C   Input:
C      TIMEIN   R      Time in days
C      NDIG     I      Number of digits after decimal in Seconds display
C   Output:
C      TSIGN    C*1    Sign of time
C      TIME     I(3)   Time in Days Hours Minutes
C      SECNDS   R      Time in seconds
C-----------------------------------------------------------------------
      REAL      TIMEIN, SECNDS
      CHARACTER TSIGN
      INTEGER   NDIG, TIME(3)
C
      INTEGER  I
      REAL     T, X
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
      IF ((NDIG.GT.0) .AND. (NDIG.LE.6)) THEN
         X = 10.0 ** NDIG
      ELSE
         X = 1.0
         END IF
      I = T * X + 0.5
      SECNDS = I / X
C                                       Now Remove 60 seconds
      IF (SECNDS.GE.59.999) THEN
         SECNDS = SECNDS - 60.
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
      IF (TIMEIN.LT.0.0) THEN
         TSIGN = '-'
      ELSE
         TSIGN = ' '
         END IF
C
 999  RETURN
      END
