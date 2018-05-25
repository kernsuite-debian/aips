      SUBROUTINE QCFT1I (LEN, WSAVE)
C-----------------------------------------------------------------------
C! Pseudo AP routine: initialize complex-to-complex 1D FFT
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
C   Initialize a workspace for a complex-to-complex FFT of length LEN.
C   There are no restrictions on LEN save that it be positive.  WSAVE
C   must have at least FTWMUL * LEN + FTWADD elements where FTWMUL
C   and FTWADD are defined in INC:PFFT.INC.  Use the constants from
C   this include files to dimension the work array since different
C   machines may use different FFT algorithms with different
C   workspace requirements for reasons of efficiency; PFFT.INC will
C   always define the constants appropriately for the machine your
C   program is compiled on.
C
C   Once initialized by QCFT1I may be used in any number of calls to
C   the Fourier transform routine QCFT1D for FFTs of length LEN.  It
C   is the responsibility of the calling routine to make sure that
C   WSAVE is large enough and that LEN is positive.
C   Input:
C      LEN        I          The length of the FFTs
C   Output:
C      WSAVE      R(*)       Initialized workspace for QCFT1D
C-----------------------------------------------------------------------
      INTEGER   LEN
      REAL      WSAVE(*)
C-----------------------------------------------------------------------
C                                       Generic pseudo AP version uses
C                                       adapted FFTPACK routines
      CALL CFFTI (LEN, WSAVE)
C
      END
