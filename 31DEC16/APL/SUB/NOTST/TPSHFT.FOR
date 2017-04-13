      SUBROUTINE TPSHFT (DATA, NIFRQ, DELI, WORK, ISMTH)
C-----------------------------------------------------------------------
C! Frequency shift total power spectra
C# UV IO-appl Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2015
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
C  Routine to perform the frequency shift for TP data, with optional
C  Hanning smoothing
C  Inputs:
C      NFRQ    I      No. freq. channels in spectrum
C      DELI    D      No. channels to shift spectrum
C      ISMTH   I      0 => no smoothing
C                     1 => hanning smooth while shifting
C   In/out
C      DATA    R(*)   The total-power spectrum to be shifted.
c                     Will also contain shifted result on output
C   Output:
C      WORK    R(*)   Work array (same size as data array)
C-----------------------------------------------------------------------
      INTEGER   NFRQ, ISMTH
      REAL      DATA(*), WORK(*)
      DOUBLE PRECISION DELI
c
      REAL      DELA, RFACT, ARG, XRE, XIM
      INTEGER   NFRQ2, I, NIFRQ, N2, JF, JBIN, NTRANS, FFTDIR
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NFRQ = NIFRQ
      RFACT = 1.0
C                                       Reflect spectrum
      NFRQ2 = NFRQ * 2
      NTRANS = NFRQ
      FFTDIR = -1
      CALL FOURG (DATA, NTRANS, FFTDIR, WORK)
C                                       Determine shift parms
      DELA  = -TWOPI * DELI / NFRQ
C                                       Shift AC spectrum.
      N2 = NFRQ / 2
      DO 20 I = 1,NFRQ
         IF (I.LE.N2) THEN
            JF = I - 1
         ELSE
            JF = -NFRQ + I
            END IF
         JBIN = JF + N2
         IF (ISMTH.EQ.1) RFACT = 0.5*(1.0-COS(2*PI*JBIN/(NFRQ-1)))
C
         ARG = DELA * JF
         XRE = DATA(2*I-1)
         XIM = DATA(2*I)
         DATA(2*I-1) = RFACT * (COS (ARG) * XRE - SIN (ARG) * XIM)
         DATA(2*I) = RFACT * (SIN (ARG) * XRE + COS (ARG) * XIM)
  20     CONTINUE
C                                       Transform to spectrum
      FFTDIR = -FFTDIR
      CALL FOURG (DATA, NTRANS, FFTDIR, WORK)
C                                       Normalize
      DO 30 I = 1,NFRQ2
         DATA(I) = DATA(I) / NFRQ
  30     CONTINUE
C                                       Form real only
      DO 40 I = 1,NFRQ
         DATA(2*I-1) = SQRT(DATA(2*I-1)*DATA(2*I-1) +
     *      DATA(2*I)*DATA(2*I))
         DATA(2*I) = 0.0
  40     CONTINUE
C
      RETURN
      END
