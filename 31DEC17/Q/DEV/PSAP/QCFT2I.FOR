      SUBROUTINE QCFT2I (LEN1, LEN2, WSAVE)
C-----------------------------------------------------------------------
C! Pseudo AP routine: initialize complex-to-complex 2D FFT
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
C   elements. There are no restrictions on LEN1 and LEN2  save that they
C   be positive and that LEN2 not exceed FFTMAX elements (defined in
C   INCS:PFFT.INC).  WSAVE must have at least FTWMUL * (LEN1 + LEN2) +
C   2 * FTWADD elements where FTWMUL and FTWADD are defined in
C   INC:PFFT.INC.  Use the constants from this include files to
C   dimension the work array since different machines may use different
C   FFT algorithms with different workspace requirements for reasons of
C   efficiency; PFFT.INC will always define the constants appropriately
C   for the machine your program is compiled on.
C
C   Once initialized by QCFT2I, WSAVE may be used in any number of calls to
C   the Fourier transform routine QCFT2D for LEN1 by LEN2 FFTs.
c
C   Input:
C      LEN1        I         Number of rows to be transformed
C      LEN2        I         Number of columns to be transformed
C   Output:
C      WSAVE      R(*)       Initialized workspace for QCFT2D
C-----------------------------------------------------------------------
      INTEGER   LEN1, LEN2
      REAL      WSAVE(*)
C
      INTEGER   IDX2
      INCLUDE 'INCS:PFFT.INC'
C-----------------------------------------------------------------------
C                                       Generic pseudo AP version uses
C                                       adapted FFTPACK routines: WSAVE
C                                       is divided into 2 workspaces for
C                                       1D FFTs.
      IDX2 = FTWMUL * LEN1 + FTWADD + 1
      CALL QCFT1I (LEN1, WSAVE(1))
      CALL QCFT1I (LEN2, WSAVE(IDX2))
C
 999  RETURN
      END
