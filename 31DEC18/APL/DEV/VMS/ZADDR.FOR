      SUBROUTINE ZADDR (ADDR1, ADDR2, IERR)
C-----------------------------------------------------------------------
C! determine if 2 addresses inside computer are the same
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
C   Determine if two addresses are the same.
C   Inputs:
C      addr1   I*2   Address 1
C      addr2   I*2   Address 2
C   Output:
C      ierr    I     Error return code:
C                       0 => addresses are the same
C                       1 => addresses are different
C   VMS version.
C-----------------------------------------------------------------------
      INTEGER*2 ADDR1(*), ADDR2(*)
      INTEGER   IERR
C-----------------------------------------------------------------------
C                                       test addresses with %LOC func
      IF (%LOC(ADDR1(1)).EQ.%LOC(ADDR2(1))) THEN
         IERR = 0
      ELSE
         IERR = 1
         END IF
C
 999  RETURN
      END
