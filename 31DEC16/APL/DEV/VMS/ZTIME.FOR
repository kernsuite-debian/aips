      SUBROUTINE ZTIME (ITIME)
C-----------------------------------------------------------------------
C! return the local time of day
C# Z Service
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
C   returns the local time of day
C   Output:
C      ITIME   I*2(3)   Local time where: ITIME(1) = hour (0-24)
C                                         ITIME(2) = minute (0-60)
C                                         ITIME(3) = second (0-60)
C   VMS version
C   The above values are calculated from the seconds since midnight.
C-----------------------------------------------------------------------
      INTEGER   ITIME(3)
C
      INTEGER   IT
      REAL      TIME, SECNDS
C-----------------------------------------------------------------------
C                                       Get seconds since midnight.
      TIME = SECNDS (0.0)
C                                       convert to INTEGER
      IT = TIME
C                                       Find hrs, min, sec
      ITIME(1) = IT / 3600
      IT = IT - 3600 * ITIME(1)
      ITIME(2) = IT / 60
      ITIME(3) = IT - 60*ITIME(2)
C
 999  RETURN
      END
