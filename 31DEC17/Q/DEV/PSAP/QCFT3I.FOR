      SUBROUTINE QCFT3I (LEN1, LEN2, LEN3, WSAVE)
C-----------------------------------------------------------------------
C! Pseudo AP routine: initialize complex-to-complex 3D FFT
C# AP-FFT
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2006
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
C   Initialize a workspace for a complex-to-complex FFT of LEN1 by LEN2
C   by LEN3 elements. The following conditions must hold where FTWMUL,
C   FTWADD and FFTMAX are defined in INCS:PFFT.INC (and may change from
C   machine to machine)
C
C   0 < LEN1
C   0 < LEN2 <= FFTMAX
C   0 < LEN3 <= FFTMAX
C   length of WSAVE >= FTWMUL * (LEN1 + LEN2 + LEN3) + 3 * FTWADD
C
C   Once initialized by QCFT3I, WSAVE may be used in any number of calls
C   to the Fourier transform routine QCFT3D for FFTs of the same size.
C
C   Input:
C      LEN1        I         Number of rows to be transformed
C      LEN2        I         Number of columns to be transformed
C      LEN3        I         Number of planes to be transformed
C   Output:
C      WSAVE      R(*)       Initialized workspace for QCFT3D
C-----------------------------------------------------------------------
      INTEGER   LEN1, LEN2, LEN3
      REAL      WSAVE(*)
C
      INTEGER   IDX2, IDX3
      INCLUDE 'INCS:PFFT.INC'
C-----------------------------------------------------------------------
C                                       Generic pseudo AP version uses
C                                       adapted FFTPACK routines: WSAVE
C                                       is divided into 3 workspaces for
C                                       1D FFTs.
      IDX2 = FTWMUL * LEN1 + FTWADD + 1
      IDX3 = IDX2 + FTWMUL * LEN2 + FTWADD
      CALL QCFT1I (LEN1, WSAVE(1))
      CALL QCFT1I (LEN2, WSAVE(IDX2))
      CALL QCFT1I (LEN3, WSAVE(IDX3))
C
 999  RETURN
      END
