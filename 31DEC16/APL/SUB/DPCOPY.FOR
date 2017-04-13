      SUBROUTINE DPCOPY (NDPRE, AIN, AOUT)
C-----------------------------------------------------------------------
C! copies one double precision array into another
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   DPCOPY copies one double precision array into another
C   To avoid any conversion of format, the arrays are treated as integer
C   internally.  ZDCHIN must have been called prior to the use of this
C   routine.
C   Inputs:
C      NDPRE   I          Number of double precision variables to copy
C      AIN     D(NDPRE)   Input array
C   Output:
C      AOUT    D(NDPRE)   Output array
C-----------------------------------------------------------------------
      INTEGER   NDPRE, AIN(*), AOUT(*)
C
      INTEGER   I, J
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (NDPRE.GT.0) THEN
         J = NDPRE * MAX (1, NWDPDP)
         DO 10 I = 1,J
            AOUT(I) = AIN(I)
 10         CONTINUE
         END IF
C
 999  RETURN
      END
