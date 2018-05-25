      SUBROUTINE PAD (NIFRQ, NXTTWO, DATA, NFRQ)
C-----------------------------------------------------------------------
C! Zero pad a spectrum
C# Spectral Calibration
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
C  If a spectrum has a prime number of channels this routine will
C  pad it with zeroes up to the next power of two to speed up
C  the FFT process.
C
C  Inputs:
C    NIFRQ         I     Number of channels in input spectrum (prime)
C    NXTTWO        I     The next power of two above NIFRQ
C    DATA (2,*)    R     The spectrum
C
C  Outputs:
C    DATA (2,*)    R     The padded spectrum
C    NFRQ          I     The # channels in the padded spectrum
C                        = NXTTWO
C-----------------------------------------------------------------------
      INTEGER   NIFRQ, NXTTWO, NFRQ, I, DIFF
      REAL      DATA (2,*)
C-----------------------------------------------------------------------
      NFRQ = NXTTWO
      DIFF = NXTTWO - NIFRQ
      DO 100 I = 1, DIFF
         DATA (1,NIFRQ+I) = 0.0
         DATA (2,NIFRQ+I) = 0.0
 100     CONTINUE
C
      RETURN
      END
