      SUBROUTINE ACSHFT (DATA, ISB, NIFRQ, DELI, WORK, ISMTH)
C-----------------------------------------------------------------------
C! Frequency shift spectra
C# UV IO-appl Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-1999
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
C   Routine to perform the frequency shift for AC data.
C   The imaginary part is zeroed on entry.
C   Inputs:
C      ISB           I        Sideband
C      NFRQ          I        No. freq. channels in spectrum
C      DELI          D        No. channels to shift spectrum
C      WORK(*)       R        Work array (same size as data array)
C      ISMTH         I        0 => no smoothing
C                             1 => hanning smooth while shifting
C   Input/output:
C      DATA(2,*)     R        The total-power spectrum to be shifted.
C                             DATA(1,*) is the real part DATA(2,*) is
C                             the imaginary part (0).
C                             Will also contain shifted result
C-----------------------------------------------------------------------
      REAL    DATA(2,*), WORK(*)
      DOUBLE PRECISION  DELI
      INTEGER   ISB, NIFRQ, ISMTH
C
      REAL    ADELI, STORE, DELA, CDA, SDA, CA, SA, RFACT, AVG
      INTEGER   NFRQ, NFRQ2, NSHIFT, I, N, K, NTRANS, FFTDIR
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NFRQ = NIFRQ

      RFACT = 1.0
      NFRQ2 = NFRQ * 2
C                                       Reflect spectrum
      NSHIFT = NFRQ2 + 2
      AVG = DATA(1,1)
      DATA(2,1) = 0.0
      DO 10 I = 2,NFRQ
         N = NSHIFT - I
         AVG = AVG + DATA(1,I)
         DATA(1,N) = DATA(1,I)
         DATA(2,I) = 0.
         DATA(2,N) = 0.
 10      CONTINUE
      AVG = AVG / NFRQ
      DATA(1,NFRQ+1) = AVG
      DATA(2,NFRQ+1) = 0.
C                                       Do FFT
      NTRANS = NFRQ2
      FFTDIR = ISB
      CALL FOURG (DATA, NTRANS, FFTDIR, WORK)
C
      ADELI = DELI
C                                       Determine shift parms
C                                       Set normalization
C                                       (ie divide by NFRQ2)
      DELA  = TWOPI * ADELI / NFRQ2
      CDA   = COS( DELA )
      SDA   = SIN( DELA )
      CA    = 1.0 / NFRQ2
      SA    = 0.0
C                                       Multiply correlation function
C                                       by the complex exponent
C                                       corresponding to the spectrum
C                                       shift and smoothing function.
      DO 200 I = 1,NFRQ
         K = NFRQ2 + 1 - I
C
         STORE     = DATA(1,I)
         DATA(1,I) = RFACT * (DATA(1,I) * CA  -  DATA(2,I) * SA)
         DATA(2,I) = RFACT * (    STORE * SA  +  DATA(2,I) * CA)
C
         STORE = CA
         CA    =    CA * CDA  -  SA * SDA
         SA    = STORE * SDA  +  SA * CDA
C
         IF (ISMTH.EQ.1) RFACT = 0.5*(1.0+COS(PI*I/NFRQ))
         STORE = DATA(1,K)
         DATA(1,K) = RFACT * (DATA(1,K) * CA + DATA(2,K) * SA)
         DATA(2,K) = RFACT * (-STORE * SA    + DATA(2,K) * CA)
C
  200 CONTINUE
C                                       Transform to spectrum
      FFTDIR = -FFTDIR
      CALL FOURG ( DATA, NTRANS, FFTDIR, WORK )
C                                       Zero image part
      DO 300 I = 1,NFRQ
C         DATA(1,I) = DATA(1,I) + 1.0
         DATA(2,I) = 0.0
  300    CONTINUE
C
 999  RETURN
      END
