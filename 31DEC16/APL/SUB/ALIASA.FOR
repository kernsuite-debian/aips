      SUBROUTINE ALIASA (AARRAY)
C-----------------------------------------------------------------------
C! implements antenna aliasing array
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
C   This routine processes a list of specified antennas.  The first
C   antenna specified is to be treated as the alias antenna.  All
C   other antennas on the list are treated as being identical to the
C   first one.  e.g. if AARRAY comes in with 2 3 6 7 0, antennas
C   3,6,7 are treated as if they were actually antenna 2.
C   In/out:
C      AARRAY   I(31)   in: list of antennas to be aliased, 1st <= 0
C                           ends list (no more than 30 > 0)
C                       out: (1) length of alias list incl counter
C                            (2) antenna to which others are aliased
C                            (3:AARRAY(1)) list of antennas aliased to
C                                AARRAY(2)
C-----------------------------------------------------------------------
      INTEGER   AARRAY(31)
C
      INTEGER   I, J
C-----------------------------------------------------------------------
C                                       first element MUST be positive
      IF (AARRAY(1).LE.0) GO TO 999
C                                       find first nonpositive entry
      DO 10 I = 1,30
         IF (AARRAY(I).LE.0) GO TO 15
 10      CONTINUE
      I = 31
C                                       shift up entries one notch
 15   DO 20 J = I,2,-1
         AARRAY(J) = AARRAY(J-1)
 20      CONTINUE
C                                       count positive entries
      AARRAY(1) = 0
      DO 30 I = 2,31
         IF (AARRAY(I).GT.0) AARRAY(1) = AARRAY(1) + 1
 30      CONTINUE
C                                       include counter itself
      AARRAY(1) = AARRAY(1) + 1
C
 999  RETURN
      END
