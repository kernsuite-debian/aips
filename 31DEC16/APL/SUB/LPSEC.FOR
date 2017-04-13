      SUBROUTINE LPSEC (UTC, SECS, DATE)
C-----------------------------------------------------------------------
C! Calculate TAI - UTC
C# Util
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-1998, 2000, 2007, 2010, 2012
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
C  Calculate the difference between TAI and UTC in seconds.  Returns
C  a fixed offset of 10 seconds for dates prior to Jan 1 1972 (NB:
C  the Astronomical Almanac states that TAI-UTC should be extrapolated
C  prior to 1972).
C
C  Note that this routine should be updated every time there is a leap
C  second.  Currently includes leap seconds up to July 1 1997.
C
C   Input:
C      UTC    D      UTC as a modified JD (JD-2400000.5)
C
C   Output:
C      SECS   D      TAI-UTC in seconds
C      DATE   C*11   Date of most recent encoded leap second IN
C                    YYYY-MMM-DD format
C-----------------------------------------------------------------------
      DOUBLE PRECISION UTC, SECS
      CHARACTER        DATE*(*)
C
      INTEGER   NUMLP
      PARAMETER (NUMLP = 25)
C                                       Number of leap seconds
C
      DOUBLE PRECISION LEAPS(NUMLP)
      SAVE             LEAPS
C                                       Dates of leap seconds
C
      INTEGER   I
C
C                  1972-07-01, 1973-01-01, 1974-01-01, 1975-01-01,
      DATA LEAPS / 41499.0D0,  41683.0D0,  42048.0D0,  42413.0D0,
C
C                  1976-01-01, 1977-01-01, 1978-01-01, 1979-01-01,
     *             42778.0D0,  43144.0D0,  43509.0D0,  43874.0D0,
C
C                  1980-01-01, 1981-07-01, 1982-07-01, 1983-07-01,
     *             44239.0D0,  44786.0D0,  45151.0D0,  45516.0D0,
C
C                  1985-07-01, 1988-01-01, 1990-01-01, 1991-01-01,
     *             46247.0D0,  47161.0D0,  47892.0D0,  48257.0D0,
C
C                  1992-07-01, 1993-07-01, 1994-01-01, 1996-01-01,
     *             48804.0D0,  49169.0D0,  49534.0D0,  50083.0D0,
C
C                  1997-07-01, 1999-01-01, 2006-01-01, 2009-01-01,
     *             50630.0D0,  51179.0D0,  53736.0D0,  54832.0D0,
C
C                  2012-07-01
     *             56109.0D0 /
C-----------------------------------------------------------------------
      SECS = 10.0D0
      DO 10 I = 1,NUMLP
         IF (UTC.GE.LEAPS(I)) SECS = SECS + 1.0D0
 10      CONTINUE
C
      DATE = '2012-JUL-01'
C
 999  RETURN
      END
