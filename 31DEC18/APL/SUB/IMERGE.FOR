      SUBROUTINE IMERGE (NIN, IA, NOUT)
C-----------------------------------------------------------------------
C! merges duplicate members of a list and sorts in ascending order
C# Utility
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
C   IMERGE returns a pointer to the location in a large array at which a
C   given smaller array begins.  NOTE: do not use Hollerith.
C   Inputs:
C      NIN    I        number words in smaller array
C   In/Out:
C      IA     I(NIN)   array to sort; on output IA(NOUT+1) - IA(NIN) set
C                      to zero, rest sorted.
C   Output:
C      NOUT   I        number of unique members of the array
C-----------------------------------------------------------------------
      INTEGER   NIN, IA(*), NOUT
C
      INTEGER   I, NTEMP
C-----------------------------------------------------------------------
      NTEMP = NIN
C                                       Elements to merge
      IF (NIN.GT.1) THEN
C                                       now sort list
         CALL ISORT (NIN, IA)
C                                       now remove duplicates
         NTEMP = 1
         DO 10 I=2, NIN
            IF (IA(I) .NE. IA(NTEMP)) THEN
               NTEMP = NTEMP + 1
               IA(NTEMP) = IA(I)
               END IF
 10         CONTINUE
C                                       Zero the "merged" elements
         DO 20 I=NTEMP+1, NIN
            IA(I) = 0
 20         CONTINUE
         END IF
C
      NOUT = NTEMP
C
 999  RETURN
      END

