      SUBROUTINE ISUM (NINT, AIN, SUM)
C-----------------------------------------------------------------------
C! sums an integeer array
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1999
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
C   ISUM sums up an integer array
C   Inputs:
C      NINT   I         Number of integer variables to sum
C      AIN    I(NINT)   Input array
C   Output:
C      SUM    I         Output sum
C-----------------------------------------------------------------------
      INTEGER   NINT, AIN(*), SUM
C
      INTEGER   I
C-----------------------------------------------------------------------
      SUM = 0
      IF (NINT.GT.0) THEN
         DO 10 I = 1,NINT
            SUM = SUM + AIN(I)
 10         CONTINUE
         END IF
C
 999  RETURN
      END
