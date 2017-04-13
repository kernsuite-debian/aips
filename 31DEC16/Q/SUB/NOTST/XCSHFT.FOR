      SUBROUTINE XCSHFT (DATA, ISB, NIFRQ, DELI, WORK, ISMTH)
C-----------------------------------------------------------------------
C! Frequency shift cross power spectra
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
C   Routine to perform the frequency shift for XC data.
C   Inputs:
C      ISB           I        Sideband
C      NIFRQ         I        No. freq. channels in spectrum
C      DELI          D        No. channels to shift by
C      WORK          R        Work array (same size as data array)
C      ISMTH         I        0 => no smoothing
C                             1 => hanning smooth while shifting
C   Input/output:
C      DATA(2,*)     R        The cross-power spectrum to be shifted.
C                             Will also contain shifted result
C-----------------------------------------------------------------------
      REAL      DATA(2,*), WORK(2,*)
      INTEGER   ISB, NIFRQ, ISMTH
      DOUBLE PRECISION DELI
C
      REAL    DEL, DEL1, CD, SD, C, S, STORE, RFACT
      INTEGER   NFRQ, NXCF, I, PHAS0, PHAS1, NN1, NN2, DELPH0, DELPH1,
     *   APLOC, NROTAT
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCVL.INC'
      DATA NN1, NN2 /1, 2/
C-----------------------------------------------------------------------
      NFRQ = NIFRQ
C                                       If Prime number of
C                                       channels pad it
      IF (PRIME) CALL PAD (NIFRQ, NXTTWO, DATA, NFRQ)
C                                       FFT to xc-function
      NXCF = NFRQ * 2
      CALL TRTOCF (ISB, NFRQ, DATA, WORK)
      RFACT = 1.0
C                                       Minus sign in following
C                                       equation for "del" is because
C                                       increasing channel #
C                                       corresponds to decreasing lag
C                                       values.
C                                       C, S shift for I=1
C                                       CD, SD increment of shift
      DEL  = -1.0 * ISB * TWOPI * DELI / NXCF
      DEL1 = -0.5 *  NXCF  * DEL
      CD   = COS( DEL )
      SD   = SIN( DEL )
      C    = COS( DEL1 )
      S    = SIN( DEL1 )
C                                       multiply the correlation by
C                                       Hanning smoothing factor.
C                                       I=NFRQ+1 corresponds to delay=0
      IF (ISMTH.EQ.1) THEN
         DO 20 I = 1, NXCF
            RFACT = 0.5 * (1.0 + COS(PI* (I - NFRQ - 1)/NFRQ))
            DATA(1,I) = RFACT * DATA(1,I)
            DATA(2,I) = RFACT * DATA(2,I)
   20       CONTINUE
         END IF
C                                       Shift XC spectrum.
C                                       At this point the array DATA
C                                       should contain a correlation
C                                       function with the zero delay
C                                       in the NFRQ+1'th channel.
      IF (POWRTO .OR. PRIME) THEN
         PHAS0 = APBEG + NCMPLX
         PHAS1 = PHAS0 + 1
         CALL QPUT (C, PHAS0, NN1, APTYPE)
         CALL QWD
         CALL QPUT (S, PHAS1, NN1, APTYPE)
         CALL QWD
         DELPH0 = PHAS1 + 1
         DELPH1 = DELPH0 + 1
         CALL QPUT (CD, DELPH0, NN1, APTYPE)
         CALL QWD
         CALL QPUT (SD, DELPH1, NN1, APTYPE)
         CALL QWD
         APLOC = DELPH1 + 1
         CALL QPUT (DATA(1,1), APBEG, NCMPLX, APTYPE)
         CALL QWD
         NROTAT = NCMPLX/2
         CALL QPHSRO (APBEG, NN2, APLOC, NN2, PHAS0, DELPH0, NROTAT)
         CALL QWR
         CALL QGET (DATA(1,1), APLOC, NCMPLX, APTYPE)
         CALL QWD
      ELSE
         DO 50 I = 1, NXCF
            STORE     = DATA(1,I)
            DATA(1,I) = DATA(1,I)*C - DATA(2,I)*S
            DATA(2,I) =     STORE*S + DATA(2,I)*C
            STORE = C
            C     = C*CD - S*SD
            S     = STORE*SD + S*CD
   50       CONTINUE
         END IF
C                                       FFT back to spectrum
      CALL TRTOCS (ISB, NXCF, DATA, WORK)
C
 999  RETURN
      END
