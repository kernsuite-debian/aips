      SUBROUTINE DAYNUM (YEAR, DAY, MONTH, DNUM)
C-----------------------------------------------------------------------
C! Determine day number of given date.
C# General
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2012, 2015
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
C  Routine to determine the daynumber of a particular calendar date.
C
C  Inputs:
C     YEAR       I       Year
C     DAY        I       Day of the month
C     MONTH      I       Month of the year
C
C  Output:
C     DNUM       I       Day number
C-----------------------------------------------------------------------
      INTEGER   YEAR, DAY, MONTH, DNUM
C
      INTEGER   J, MONT, IMONT, DAYMTH(12,2), I
      DATA DAYMTH /31,28,31,30,31,30,31,31,30,31,30,31,
     *             31,29,31,30,31,30,31,31,30,31,30,31/
C-----------------------------------------------------------------------
      IF (MOD(YEAR,400).EQ.0) THEN
         I = 2
      ELSE IF (MOD(YEAR,100).EQ.0) THEN
         I = 1
      ELSE IF (MOD(YEAR,4).EQ.0) THEN
         I = 2
      ELSE
         I = 1
         END IF
C
      MONT = 0
      IMONT = MONTH - 1
      DO 100 J = 1,IMONT
         MONT = MONT + DAYMTH(J,I)
 100     CONTINUE
      DNUM = MONT + DAY
C
 999  RETURN
      END
