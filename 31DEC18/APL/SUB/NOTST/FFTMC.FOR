      SUBROUTINE FFTMC (DIR, L, M, N, VIS)
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
C     FFTMC does a fast Fourier transform of an input array.
C     Given:
C         DIR          I     Sign of the complex phase in the Fourier
C                            integral.
C         L,M,N        I     Dimensions of the array.
C     Given and returned:
C         VIS(L,M,N)   CX    Array containing the complex visibilities.
C                            Returned as the Fourier transform of the
C                            input array.
C     Called:
C          APLNOT: {FFTV2}
C     Algorithm:
C          Singleton's FFT.
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1987/Dec     Code last modified; 1989/Nov/01.
C-----------------------------------------------------------------------
      INTEGER    DIR, ISN, L, M, N
      REAL       VIS(2*L*M*N)
C-----------------------------------------------------------------------
C     Direction of the FFT.
      ISN = SIGN(2, DIR)

C     Transform along lines of constant V and W.
      IF (L.GT.1) CALL FFTV2 (VIS(1), VIS(2), N*M, L, 1, ISN)

C     Now along lines of constant U and W.
      IF (M.GT.1) CALL FFTV2 (VIS(1), VIS(2), N, M, L, ISN)

C     Now along lines of constant U and V.
      IF (N.GT.1) CALL FFTV2 (VIS(1), VIS(2), 1, N, M*L, ISN)

      RETURN
      END
