      SUBROUTINE GREG (JD, CLD)
C-----------------------------------------------------------------------
C! converts Julian day number to date in character form
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   GREG converts a REAL*8 Julian day number to the calendar date
C   in the character form 'YYYYMMDD'
C   Inputs:
C      JD    D     Julian date.
C   Output:
C      CLD   C*8   Character coded calender day.
C   Programmer: F. Schwab,   Apdapted from ASM Algorithm no. 199
C-----------------------------------------------------------------------
      CHARACTER CLD*8
      DOUBLE PRECISION JD
C
      INTEGER   IT(6)
C-----------------------------------------------------------------------
      CALL JD2DAT (JD, IT)
C                                       finally encode it
 20   WRITE (CLD,1000) IT(1), IT(2), IT(3)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I4.4,I2.2,I2.2)
      END
