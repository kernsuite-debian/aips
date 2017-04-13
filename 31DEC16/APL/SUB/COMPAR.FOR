      SUBROUTINE COMPAR (N, K1, K2, LR)
C-----------------------------------------------------------------------
C! compares two integer arrays and returns .TRUE. if they are equal
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
C   COMPAR compares N integer words in K1 and K2.  LR is .TRUE. if
C      they are equal, .FALSE. otherwise
C   Inputs:  N      I        number of words to be copied
C            K1     I(>=N)   input array
C            K2     I(>=N)   2nd input array
C   Outputs: LR     L        Comparison value
C-----------------------------------------------------------------------
      INTEGER   I, N, K1(*), K2(*)
      LOGICAL   LR
C-----------------------------------------------------------------------
      LR = .FALSE.
      DO 10 I = 1,N
         IF (K1(I).NE.K2(I)) GO TO 999
 10      CONTINUE
      LR = .TRUE.
C
 999  RETURN
      END
