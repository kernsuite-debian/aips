      SUBROUTINE XCSHNQ (DATA, ISB, NIFRQ, DELI, WORK)
C-----------------------------------------------------------------------
C! Frequency shift cross power spectra
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
C  Routine to perform the frequency shift for XC data, this version
C  does not use Q routines.
C
C  Inputs:
C      DATA(2,*)     R        The cross-power spectrum to be
C                             shifted. Will also contain shifted
C                             result
C      ISB           I        Sideband
C      NIFRQ         I        No. freq. channels in spectrum
C      DELI          D        No. channels to shift by
C      WORK          R        Work array (same size as data array)
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXCH2
      PARAMETER (MAXCH2=MAXCHA*2)
      REAL    DATA(2,MAXCH2), WORK(2,1), DEL, DEL1, CD, SD, C, S,
     *   STORE
      DOUBLE PRECISION DELI
      INTEGER   ISB, NFRQ, NXCF, I, NIFRQ
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCVL.INC'
C-----------------------------------------------------------------------
      NFRQ = NIFRQ
C                                       FFT to xc-function
      NXCF = NFRQ * 2
      CALL NQTOCF (ISB, NFRQ, DATA, WORK)
C                                        Minus sign in following
C                                        equation for "del" is because
C                                        increasing channel #
C                                        corresponds to decreasing lag
C                                        values.
      DEL  = -1.0 * ISB * TWOPI * DELI / NXCF
      DEL1 = -0.5 *  NXCF  * DEL
      CD   = COS( DEL )
      SD   = SIN( DEL )
      C    = COS( DEL1 )
      S    = SIN( DEL1 )
C                                        Shift XC spectrum.
C                                        At this point the array DATA
C                                        should contain a correlation
C                                        function with the zero delay
C                                        in the NFRQ+1'th channel.
      DO 50 I = 1, NXCF
         STORE     = DATA(1,I)
         DATA(1,I) = DATA(1,I)*C - DATA(2,I)*S
         DATA(2,I) =     STORE*S + DATA(2,I)*C
         STORE = C
         C     = C*CD - S*SD
         S     = STORE*SD + S*CD
   50    CONTINUE
C                                       FFT back to spectrum
      CALL NQTOCS (ISB, NXCF, DATA, WORK)
C
      RETURN
      END
