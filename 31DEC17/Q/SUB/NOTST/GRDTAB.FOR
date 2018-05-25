      SUBROUTINE GRDTAB (N, ICENT, CTYPE, CPARM, ITAB, IWORK, BUFFER)
C-----------------------------------------------------------------------
C! Computes Fourier transform of gridding convolution function.
C# AP-util Map Math
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
C   GRDTAB computes the fourier transform of the gridding function.
C     Inputs:
C      N           I    The number of cells in the grid.
C      ICENT       I    The grid cell number (1 rel) of the center of
C                       of the grid
C      CTYPE       I    The type of the gridding function.
C      CPARM(10)   R    The array containing the gridding fn. parameters
C      ITAB        I    AP base address of FT of gridding function.
C      IWORK       I    AP base address of a large work area.
C      BUFFER      R    A work buffer.
C-----------------------------------------------------------------------
      INTEGER   N, ICENT, CTYPE, BIAS
      INTEGER   ITAB, IWORK, IPH0, JCMPLX, NUMB, WRK, JCENT, NN, ITEMP
      REAL      CPARM(10), PI, BUFFER(8), FAC
      DATA PI /3.1415926/
C-----------------------------------------------------------------------
C                                       Determine the number of tabulate
C                                       values of the gridding fn. kept
C                                       in the AP.
      NUMB = MAX (CPARM(1), 1.0) + 0.1
      NUMB = NUMB * 2 + 1
      BIAS = 50 * NUMB + 1
      NUMB = 100 * NUMB + 1
      NN = N
C                                       Compute gridding function
      CALL CONVFN (IWORK, CTYPE, CPARM, BUFFER)
C                                       Convert to complex.
      JCMPLX = IWORK + NUMB
      ITEMP = NUMB * 2
      CALL QVCLR (JCMPLX, 1, ITEMP)
      CALL QVMOV (IWORK, 1, JCMPLX, 2, NUMB)
C                                       Determine phase rotation values
      IPH0 = IWORK
      FAC = 2.0*PI / (100.0*N)
      BUFFER(1) = COS (FAC * (1.0-ICENT-BIAS + ICENT * REAL (BIAS)))
      BUFFER(2) = SIN (FAC * (1.0-ICENT-BIAS + ICENT * REAL (BIAS)))
      BUFFER(3) = COS (FAC * (1.0-ICENT))
      BUFFER(4) = SIN (FAC * (1.0-ICENT))
      BUFFER(5) = COS (FAC * (1.0-BIAS))
      BUFFER(6) = SIN (FAC * (1.0-BIAS))
      BUFFER(7) = COS (FAC)
      BUFFER(8) = SIN (FAC)
      CALL QWR
      CALL QPUT (BUFFER, IPH0, 8, 2)
      CALL QWD
C                                       Fourier Transform.
      CALL QRFT (JCMPLX, ITAB, IPH0, NN, NUMB)
C                                       Make result as symmetric as
C                                       possible.
      WRK = N - ICENT
      IF (WRK.GT.(ICENT-1)) WRK = ICENT - 1
      JCENT = ITAB + ICENT - 2
      ITEMP = ITAB + ICENT
      CALL QVMOV (JCENT, -1, ITEMP, 1, WRK)
C                                       Normalize to center
      JCENT = ITAB + ICENT - 1
      CALL QVFILL (JCENT, JCMPLX, 1, NN)
      CALL QVDIV (ITAB, 1, JCMPLX, 1, ITAB, 1, NN)
      CALL QWR
C
  999 RETURN
      END
