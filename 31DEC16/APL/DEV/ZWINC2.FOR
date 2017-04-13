      SUBROUTINE ZWINC2 (COLS, LINES)
C-----------------------------------------------------------------------
C! detects current window size
C# Z2 System
C-----------------------------------------------------------------------
C;  Copyright (C) 1998-1999
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
C   Determines number of lines and columns in terminal
C   Output:
C      COLS    I   Number of columns in terminal
C      LINES   I   Number of lines in terminal
C   Stub version; does nothing except return a usable number of lines
C-----------------------------------------------------------------------
      INTEGER   COLS, LINES
C-----------------------------------------------------------------------
C                                       Use default value
      COLS = 72
      LINES = 24
C
 999  RETURN
      END
