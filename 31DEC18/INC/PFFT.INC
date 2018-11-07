C                                                          Include PFFT
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
C   Constants defining the size requirements for the work buffers used
C   by QCFT1D and the maximum sizes for multidimensional FFTs.
C
C   The WSAVE array should be dimensioned with at least
C   FTWMUL * N + FTWADD elements for an FFT of length N.
C
C   Multidimensional FFTs should not exceed FFTMAX along any single
C   dimension.
C
C   This version for FFTs based on the FFTPACK routines.
C-----------------------------------------------------------------------
      INTEGER   FTWMUL
      INTEGER   FTWADD
      PARAMETER (FTWMUL = 4)
      PARAMETER (FTWADD = 15)
C
      INTEGER   FFTMAX
      PARAMETER (FFTMAX = 16 * 1024)
C                                                              End PFFT.
