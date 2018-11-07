      SUBROUTINE COPY (N, KFROM, KTO)
C-----------------------------------------------------------------------
C! copies integer words from one array to another
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
C   COPY transfers N integer words from KFROM to KTO
C   Inputs:
C      N      I        number of words to be copied
C      KFROM  I(N)     input array
C   Outputs:
C      KTO    I(N)     output array
C-----------------------------------------------------------------------
      INTEGER   N, KFROM(*), KTO(*)
C
      INTEGER   I
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
         DO 10 I = 1,N
            KTO(I) = KFROM(I)
 10         CONTINUE
C
 999  RETURN
      END
