      SUBROUTINE CATIME (IOP, CATREC, IT)
C-----------------------------------------------------------------------
C! stores current, or recovers previous, date and time in packed format
C# Utility Catalog
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
C   CATIME stores the current date and time in CATREC and IT (IOP=1) or
C   fills in IT from CATREC (IOP=2).
C   Inputs:
C      IOP     I      1 => current date, time to CATREC and IT
C                     2 => CATREC translated to IT
C   In/out:
C      CATREC  I(2)   YY+MM+DD, HH+MM+SS
C   Outputs:
C      IT      I(6)   YY (all digits), MM, DD, HH, MM, SS
C-----------------------------------------------------------------------
      INTEGER    IOP, CATREC(2), IT(6)
      INTEGER    J
C-----------------------------------------------------------------------
C                                       time -> CATREC and IT
      IF (IOP.EQ.1) THEN
         CALL ZDATE (IT(1))
         CALL ZTIME (IT(4))
         IF (IT(1).GT.200) IT(1) = IT(1) - 1900
         CATREC(1) = 256 * (256 * IT(1) + IT(2)) + IT(3)
         CATREC(2) = 256 * (256 * IT(4) + IT(5)) + IT(6)
         IT(1) = IT(1) + 1900
C                                       CATREC sets time in IT: date
      ELSE IF (IOP.EQ.2) THEN
         J = CATREC(1)
         IT(3) = MOD (J, 256)
         J = J / 256
         IT(2) = MOD (J, 256)
         IT(1) = J / 256 + 1900
C                                       CATREC sets time in IT: time
         J = CATREC(2)
         IT(6) = MOD (J, 256)
         J = J / 256
         IT(5) = MOD (J, 256)
         IT(4) = J / 256
C                                       Bad IOP
      ELSE
         CALL FILL (6, 0, IT)
         END IF
C
 999  RETURN
      END
