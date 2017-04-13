      SUBROUTINE JD2DAT (JD, IT)
C-----------------------------------------------------------------------
C! converts Julian day number to calendar date and time
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
C   JD2DAT converts a REAL*8 Julian day number to the calendar date
C   and time in the form YY, MM, DD, HH, MM, SS
C   Inputs:
C      JD      D     Julian date.
C   Output:
C      IT(6)   I     calender day and time.
C   Programmer: F. Schwab and EWG
C   Apdapted from ASM Algorithm no. 199
C-----------------------------------------------------------------------
      INTEGER   IT(6)
      DOUBLE PRECISION JD
C
      INTEGER   ID, IM, IY, IC
      DOUBLE PRECISION J, Y, D, M
C-----------------------------------------------------------------------
      J = AINT (JD + 0.5D0 - 1721119.D0)
      Y = AINT ((4.D0*J - 1.0D0) / 146097.D0)
      IC = Y + 0.0001D0
      J = 4.D0*J - 1.0D0 - 146097.D0*Y
      D = AINT (J * 0.25D0)
      J = AINT ((4.0D0*D + 3.0D0) / 1461.0D0)
      D = 4.0D0*D + 3.0D0 - 1461.0D0*J
      D = AINT ((D+4.0D0) * 0.25D0)
      M = AINT ((5.0D0*D - 3.0D0) / 153.0D0)
      ID = 5.0D0*D - 3.0D0 - 153.0D0*M
      ID = (ID + 5) / 5
      IY = J + 100*IC
      IM = M
      IF (IM.LT.10) THEN
         IM = IM + 3
      ELSE
         IM = IM - 9
         IY = IY + 1
         END IF
C                                       date part
      IT(1) = IY
      IT(2) = IM
      IT(3) = ID
C                                       time of day part
      J = AINT (JD + 0.5D0)
      J = (JD + 0.5D0 - J) * 24.0D0
      IT(4) = J
      J = (J - IT(4)) * 60.0D0
      IT(5) = J
      IT(6) = (J - IT(5)) * 60.0D0 + 0.5D0
C
 999  RETURN
      END
