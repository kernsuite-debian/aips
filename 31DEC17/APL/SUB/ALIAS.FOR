      SUBROUTINE ALIAS (ANTNO, AARRAY)
C-----------------------------------------------------------------------
C! implements antenna aliasing
C# Utility
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
C   The antenna number in ANTNO, if it appears in AARRAY(2:*) then it
C   is reset to AARRAY(2), AARRAY(1) should be the number of positive
C   elements in AARRAY inclusively.  e.g. AARRAY = 2 4 5 0 would be
C   incorrect; AARRAY = 3 4 5 0 would treat antenna 5 as if it were
C   antenna 4.
C   Inputs:
C      AARRAY   I(*)   (1) length of alias list
C                      (2) antenna to which others are aliased
C                      (3:AARRAY(1)) list of antennas aliased to (2)
C   In/Out:
C      ANTNO    I      in: antenna number to be (possibly) aliased
C                      out:(possibly) aliased antenna number
C-----------------------------------------------------------------------
      INTEGER   ANTNO, AARRAY(*)
C
      INTEGER   I
C-----------------------------------------------------------------------
      IF (AARRAY(1).GE.3) THEN
         DO 10 I = 3,AARRAY(1)
            IF (ANTNO.EQ.AARRAY(I)) ANTNO = AARRAY(2)
 10         CONTINUE
         END IF
C
 999  RETURN
      END
