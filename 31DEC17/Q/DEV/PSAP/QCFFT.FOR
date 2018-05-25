      SUBROUTINE QCFFT (C, N, F)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Complex 1-D FFT.
C# AP-FFT
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2006, 2011, 2015
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
C   Pseudo-AP version
C   Full complex 1-D FFT.
C   Inputs:
C      C  I  Base address (0-rel) of complex array to transform
C      N  I  Number of points in array (must be power of two).
C      F  I  Transform direction; 1 -> Forward
C                                -1 -> Backward
C-----------------------------------------------------------------------
      INTEGER   C, N, F
C
      INTEGER   IF
      LONGINT   JC
      INCLUDE 'INCS:PMAD.INC'
      DOUBLE PRECISION DWORK(2,(2*MAXIMG)+16)
      SAVE DWORK
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF = F
      JC = C + PSAPOF
      CALL QXFOUR (APCORE(JC), N, IF, DWORK)
C
      RETURN
      END
