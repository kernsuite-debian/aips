      SUBROUTINE TRTOCS (ISB, NCORR, DATA, WORK)
C-----------------------------------------------------------------------
C! FFT correlation functions -> spectra
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
C  This routine transforms a complex correlation function DATA of
C  length NCORR to a single sideband, complex spectrum of length
C  NSPEC=NCORR/2.
C  Inputs:
C      DATA(2,NCORR)    R     array containing the delay lag function
C      WORK(2,NCORR)    R     work array
C      NCORR            I     length of complex correlation function
C      ISB              I     sideband of output data (1=upper,-1=lower)
C  Outputs:
C      DATA(2,NSPEC)    R     array containing the transformed data
C  P. Diamond   18 March 1987
C-----------------------------------------------------------------------
      INTEGER   NCORR, ISB, K, I, NSPEC
      INTEGER   FFTDIR
      REAL      DATA(2,*), WORK(2,*), TEMP1, TEMP2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DCVL.INC'
C-----------------------------------------------------------------------
      NSPEC = NCORR / 2
C
C                            Rearrange the correlation function so that
C                            the center channel (ncorr/2 + 1) winds up
C                            in the first element of the array to be
C                            transformed.  the correlation function
C                            to be transformed should be (nspec * 2)
C                            points long.
      DO 10 I = 1,NSPEC
         K = I + NSPEC
         TEMP1 = DATA(1,I)
         TEMP2 = DATA(2,I)
         DATA(1,I) = DATA(1,K)
         DATA(2,I) = DATA(2,K)
         DATA(1,K) = TEMP1
   10    DATA(2,K) = TEMP2
C
C                            transform
C                            isign determines which sideband will end
C                            up in first half of data
      FFTDIR = ISB
      IF (POWRTO .OR. PRIME) THEN
C                                       Use AP routines
         CALL QPUT (DATA(1,1), APBEG, NCMPLX, APTYPE)
         CALL QWD
         FFTDIR = -FFTDIR
         CALL QCFFT (APBEG, NCORR, FFTDIR)
         CALL QWR
         CALL QGET (DATA(1,1), APBEG, NCMPLX, APTYPE)
         CALL QWD
      ELSE
         CALL FOURG (DATA, NCORR, FFTDIR, WORK)
         END IF
C
      RETURN
      END
