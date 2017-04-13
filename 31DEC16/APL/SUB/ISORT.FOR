      SUBROUTINE ISORT (N, IA)
C-----------------------------------------------------------------------
C! sorts a list of integers into ascending order
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
C   ISORT returns a pointer to the location in a large array at which a
C   given smaller array begins.  NOTE: do not use Hollerith.
C   Inputs:
C      N     I       number words in smaller array
C   In/Out:
C      IA    I(N)    array to sort
C-----------------------------------------------------------------------
      INTEGER   N, IA(*)
C
      INTEGER   I, J, ITEMP, II, JJ
C-----------------------------------------------------------------------
C                                       Elements to sort
      IF (N.GT.1) THEN
C                                       compare all elements in array
         II = N - 1
         DO 20 I = 1,II
C                                       with all other elements
            JJ = I + 1
            DO 10 J = JJ,N
C                                       if bigger switch
               IF (IA(I).GT.IA(J)) THEN
                  ITEMP = IA(I)
                  IA(I) = IA(J)
                  IA(J) = ITEMP
                  END IF
 10            CONTINUE
 20         CONTINUE
C                                       End if elements
         END IF
C
 999  RETURN
      END
