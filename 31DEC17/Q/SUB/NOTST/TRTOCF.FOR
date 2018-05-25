      SUBROUTINE TRTOCF (ISB, NFRQ, DATA, WORK)
C-----------------------------------------------------------------------
C! FFT spectra -> correlation functions
C# UV IO-appl Calibration Spectral
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
C  Routine to transform a complex spectrum DATA of length NFRQ
C  to a complex correlation function.
C
C  Inputs:
C     ISB          I         Sideband
C     NFRQ         I         No. frq channels in input spectrum
C     DATA(2,*)    R         Complex spectrum
C     WORK(2,*)    R         Work array (same size as data array)
C
C  Outputs:
C     DATA(2,*)    R         Complex cross correlation function
C-----------------------------------------------------------------------
      INTEGER   ISB, NFRQ, KSTART, KSTOP, K, KK, LL, NOUT
      INTEGER   FFTDIR, KPTS
      REAL      WORK(2,*), DATA(2,*), TEMP1, TEMP2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DCVL.INC'
C-----------------------------------------------------------------------
C                                       Set some parameters
      KPTS = NFRQ*2
      NOUT = NFRQ*2
C                                       Fill lower sideband array
C                                       slots with zeroes
      KSTART = NFRQ + 1
      KSTOP  = KPTS
      DO 10 K=KSTART,KSTOP
         DATA(1,K) = 0.
 10      DATA(2,K) = 0.
C                                       Transform to XCF
      FFTDIR = -ISB
      IF (POWRTO .OR. PRIME) THEN
C                                       Use AP routines
         CALL QPUT (DATA(1,1), APBEG, NCMPLX, APTYPE)
         CALL QWD
         FFTDIR = -FFTDIR
         CALL QCFFT (APBEG, KPTS, FFTDIR)
         CALL QWR
         CALL QGET (DATA(1,1), APBEG, NCMPLX, APTYPE)
         CALL QWD
      ELSE
         CALL FOURG (DATA, KPTS, FFTDIR, WORK)
         END IF
C                                       Flip data around to
C                                       centre correlation function
C                                       in first half of array
      KSTOP = NFRQ
      DO 20 K =1, KSTOP
         KK = KPTS - KSTOP + K
         LL = NOUT - KSTOP + K
         TEMP1 = DATA(1,K)
         TEMP2 = DATA(2,K)
         DATA(1,K) = DATA(1,KK) / KPTS
         DATA(2,K) = DATA(2,KK) / KPTS
         DATA(1,LL) = TEMP1 / KPTS
 20      DATA(2,LL) = TEMP2 / KPTS
C
      RETURN
      END
