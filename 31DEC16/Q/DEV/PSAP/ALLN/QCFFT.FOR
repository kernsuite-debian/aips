      SUBROUTINE QCFFT (C, N, F)
C-----------------------------------------------------------------------
C! Alliant Pseudo AP routine: Full complex FFT routine.
C# AP-FFT
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
C   Full complex 1-D FFT.
C   Inputs:
C      C  I  Base address (0-rel) of complex array to transform
C      N  I  Number of points in array (must be power of two).
C      F  I  Transform direction; 1 -> Forward
C                                -1 -> Backward
C   Alliant version (uses FX/SERIES Scientific Library routine CFFT1D)
C-----------------------------------------------------------------------
      INTEGER   C, N, F, IF
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                        CFFT1D is an Alliant FX/series
C                                        scientific library routine:
C                                           -2 => forward transform
C                                           -1 => forward transform
C                                            0 => initialize work area
C                                           +1 => inverse transform
C                                           +2 => inverse transform w/o
C                                                 normalization
      IF (N.EQ.FFTSZE) GO TO 10
C                                        Initialize FFT work area
         CALL CFFT1D (N, APCORE(C+1), 0, WKVEC9)
         FFTSZE = N
C                                        CFFT1D's transform direction
C                                        convention opposite from AIPS's
 10   IF = -F * 2
      CALL CFFT1D (N, APCORE(C+1), IF, WKVEC9)
C
 999  RETURN
      END
