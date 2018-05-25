      SUBROUTINE QMTYP (NFIELD, NCC, NVIS, NCHAN, NX, NY, TIMDFT,
     *   TIMFFT)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Chose DFT or gridded interpolation method.
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2006
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
C   Version pseudo-AP
C   QMTYP computes the times to do a DFT and interpolated GRID model
C   computation.  The algorithm is a crude, parameterized model which
C   allows a decision about which method is faster; high accuracy is not
C   needed.  The comparison is between VISDFT which does a DFT model
C   computation and GRDSUB which does the gridded interpolation.
C   It is simple to compute the cost of VISDFT since it is a constant
C   times NVIS * SUM(NCC) * NCHAN.  GRDSUB depends on the number of
C   fields (one pass per field per channel is required), the size of
C   each field, NCC(field), and NVIS.  Three scaling factors
C   TPVGRD, TFFT, TPCGRD are used to parameterize the dependence of
C   NVIS, (NX(field)*NY(field)) and NCC(field).
C      The algorithm may be adjusted to the accounting scheme of the
C   local system.  VISDFT will generally be CPU bound and GRDSUB will
C   be I/O bound.
C   Input:
C      NFIELD  I    Number of fields
C      NCC(*)  I    Number of components per field.
C      NVIS    I    Number of visibility records.
C      NCHAN   I    Number of frequency channels to subtract.
C      NX,NY   I(*)    Size of each field
C   Output:
C      TIMDFT  R    Estimate of time for DFT in sec.
C      TIMFFT  R    Estimate of time for FFT/grid. interpolation.
C-----------------------------------------------------------------------
      INTEGER   NFIELD
      INTEGER   NCC(NFIELD), NX(NFIELD), NY(NFIELD), NVIS, NCHAN
      REAL      TIMDFT, TIMFFT
C
      INTEGER   SUMCC, I, TIMFF1, TIMFF2, TIMFF3, X
      REAL      TPVGRD, TFFT, TPCGRD, TPVPC
C                                       Time/vis to interpolate
C                                       (ALGSUB)
      DATA TPVGRD /1.E-5/
C                                       Time/NX/NY for GRID (CCSGRD)
C                                       Dependency on grid size.
      DATA TFFT /0.8E-6/
C                                       Time/comp to grid (CCSGRD)
C                                       dependency on no. comp.
      DATA TPCGRD /1.0E-4/
C                                       Time/vis/comp DFT (VISDFT)
      DATA TPVPC /6.6E-7/
C-----------------------------------------------------------------------
C                                       Compute Grid-interpolation
C                                       Loop over fields
      TIMFF1 = 0.0
      TIMFF2 = 0.0
      TIMFF3 = 0.0
      SUMCC = 0
      DO 50 I = 1,NFIELD
         IF (NCC(I).GT.0) THEN
            X = 4
            IF (NX(I).GT.4096) X = X / 2
            IF (NY(I).GT.4096) X = X / 2
            SUMCC = SUMCC + NCC(I)
            TIMFF1 = TIMFF1 + NVIS
            TIMFF2 = TIMFF2 + X * NX(I) * NY(I)
            TIMFF3 = TIMFF3 + NCC(I)
            END IF
 50      CONTINUE
      TIMFFT = (TIMFF1 * TPVGRD + TIMFF2 * TFFT + TIMFF3 * TPCGRD) *
     *   NCHAN
C                                       Compute DFT time
      TIMDFT = TPVPC * NVIS * SUMCC * NCHAN
C
 999  RETURN
      END
