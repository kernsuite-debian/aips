      SUBROUTINE AFRFI (DATA, NNN, AMPP, RATE, FI, DDAMP, DDRATE,
     *   DDFI, IERR)
C-----------------------------------------------------------------------
C   Estimates the values of frequency, initial phase and amplitude of
C   a complex exponent given (with noise) in the data set. A three
C   step process is used, first an FFT stage with zero padding.
C   The errors of the parameters determined in this stage are less than
C   0.5/N for the frequency; 45 degrees for the phase and 10% for the
C   amplitude, where N is the number of points in the data.
C   In the second stage a search of the frequency and  phase inside of
C   the ranges mentioned above is performed. The theoretically known
C   dependence of the phase change on the frequency change is used here.
C   The final solution of the amplitude, frequency and phase is found by
C   least square method. The output of the frequency is dimentionless.
C   It has to be devided by period of data samples to obtain the correct
C   value.
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
C   Programmer  L.R. Kogan
C-----------------------------------------------------------------------
C   Inputs:
C      DATA     R(2*NNN) input array
C      NNN        I      Number of complex points in the DATA
C   Outputs
C      AMPP     found amplitude
C      RATE     found frequency in parts of window. -0.5<RATE<0.5
C               Dimensional RATE can be obtained having devided
C               RATE by period of samples in data.
C      FI       found phase of the most left point of the data, in radian
C      DDAMP    rms of amplitude
C      DDRATE   rms of frequency
C      DDFI     rms of phase
C      IERR     error
C-----------------------------------------------------------------------
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INTEGER   NNN, I, NNN2, ISIGN, LESOL, IK, K, ITER, NRATE,
     *   IRATE, KRATE, IERR
      REAL DATA(4096), WORK(4096), RATE, FI, AMPP,
     *   INPDAT(2048), TAMP, R(3), MATR(9), RATM, FIM,
     *   NOBS, SUM, SSQ, SOL(3),VX(3), SSQRES, VARRES, VARY, FIT,
     *   DRE, DIM, ARG, TWOP, SINK, COSK, DDAMP, DDRATE,
     *   DDFI, TWOPP, STRATE, COEFF, DDSUM, DRATE, DSUM
      IERR = 0
      IF (NNN .GT. 1024)  THEN
         IERR = 1
         WRITE (MSGTXT,1000) NNN
         CALL MSGWRT (8)
         GO TO 999
         END IF
      ISIGN = -1
      NNN2 = 2*NNN
C                                       prepare DATA for FFT with
C                                       zero padding
      DO 20 I = 1, NNN
         INPDAT(2*I-1) = DATA(2*I-1)
         INPDAT(2*I) = DATA(2*I)
         DATA(2*(I+NNN)-1) = 0.0
         DATA(2*(I+NNN)) = 0.0
  20    CONTINUE
C                                       FFT
      CALL FOURG (DATA, NNN2, ISIGN, WORK)
C                                       find max of amplitude
      AMPP = SQRT (DATA(1)*DATA(1) + DATA(2)*DATA(2))
      DO 40 I = 1, NNN2
         TAMP = SQRT (DATA(2*I-1)*DATA(2*I-1) + DATA(2*I)*DATA(2*I))
         IF (TAMP .GE. AMPP) THEN
            AMPP = TAMP
            RATE = (I-1.0) / (2.0*NNN)
            IF (I .GT. NNN) RATE = RATE - 1.0
            FI = ATAN2 (DATA(2*I), DATA(2*I-1))
            END IF
   40    CONTINUE
      AMPP = AMPP / NNN
C                                       RATE is in parts of window
      RATE = RATE*2.0*NNN
C                                       RATE is numbers of resolution
C                                       elements
C-----------------------------------------------------------------------
C                                       precise AMP, RATE and FI
C                                       using dependence
C                                       FI upon RATE
      RATM = RATE
      FIM = FI
      NRATE = 5
      STRATE = 1.0 / NRATE
      COEFF = (PI/2)*(1 - 1.0/NNN)
      TWOPP = TWOPI / (2.0*NNN)
      DDSUM = 1.0E10
      DO 80 IRATE = 1, NRATE+3
         DRATE = (IRATE-2)*STRATE - 0.5
         RATE = RATM + DRATE
         FI = FIM - COEFF * DRATE
         DSUM = 0.0
         DO 60 I = 1, NNN
C                                       take only the points.NE.0
            IF ((INPDAT(2*I-1).EQ.0.0) .AND.
     *         (INPDAT(2*I).EQ.0.0)) GO TO 60
            DRE = INPDAT(2*I-1)
            DIM = INPDAT(2*I)
C                                       add the corrected component
            ARG = TWOPP * (I - 1) * RATE + FI
            DRE = DRE - AMPP * COS(ARG)
            DIM = DIM - AMPP * SIN(ARG)
            DSUM = DSUM + DRE * DRE + DIM * DIM
   60       CONTINUE
         IF (DSUM.LT.DDSUM) THEN
            DDSUM = DSUM
            KRATE = IRATE
            END IF
   80    CONTINUE
         DRATE = (KRATE-2)*STRATE - 0.5
         RATE = RATM + DRATE
         FI = FIM - COEFF * DRATE
C --------------------------------------------------------------------
C                   L E A S T    S Q U A R E
C
C        precise AMP, RATE and FI using non linear least square
C              fitting a  complex exponent to the given data.
C                We have NNN2 = 2*NNN points of data
C             (real and image for each point of data)
C         We have 3 unknown magnitudes: ampl., rate and phase
C --------------------------------------------------------------------


      ITER = 1
  100 CONTINUE
C                                       Force result vector R(3),
C                                       matrix M(3*3) to zero
      DO 120 I = 1, 3
         R(I) = 0.0
         DO 110 K = 1, 3
            IK = K + (I - 1)*3
            MATR (IK) = 0.0
 110        CONTINUE
 120     CONTINUE
      SUM = 0.0
      SSQ = 0.0
      NOBS = 0.0
C                                       Prepare result vector R(3)
C                                       and matrix MATR(3*3)
C                                       for routine LEASQR

      DO 140 I = 1, NNN
C                                       take only the points.NE.0
         IF ((INPDAT(2*I-1).EQ.0.0) .AND.
     *      (INPDAT(2*I).EQ.0.0)) GOTO 140
         NOBS = NOBS + 1
         DRE = INPDAT (2*I -1)
         DIM = INPDAT (2*I)
         TWOP = TWOPP * (I-1)
C                                       difference of DATA and model
         ARG = TWOP * RATE + FI
         SINK = SIN(ARG)
         COSK = COS(ARG)
         DRE = DRE - AMPP * COSK
         DIM = DIM - AMPP * SINK
         SUM = SUM + DRE + DIM
         SSQ = SSQ + DRE*DRE + DIM*DIM
C
C                                       1-ampl; 2-rate; 3-fi.
         R(1) =  R(1) + DRE * COSK
     *      + DIM * SINK
         R(2) =  R(2) - DRE * AMPP * SINK * TWOP
     *      + DIM * AMPP * COSK * TWOP
         R(3) =  R(3) - DRE * AMPP*SINK
     *      + DIM * AMPP*COSK
C                                       calculate upper/right
C                                       triangle of MATR
         MATR(1) = MATR(1) + COSK*COSK + SINK*SINK
C         MATR(2) = MATR(2) - COSK*AMPP*SINK*TWOP
C     *      + SINK*AMPP*COSK*TWOP
C         MATR(3) = MATR(3) - COSK*AMPP*SINK + SINK*AMPP*COSK
         MATR(4) = MATR(4) - AMPP*SINK*TWOP*COSK
     *      + AMPP*COSK*TWOP*SINK
         MATR(5) = MATR(5) + AMPP*SINK*TWOP*AMPP*SINK*TWOP
     *      + AMPP*COSK*TWOP*AMPP*COSK*TWOP
C         MATR(6) = MATR(6) + AMPP*SINK*TWOP*AMPP*SINK
C     *      + AMPP*COSK*TWOP*AMPP*COSK
         MATR(7) = MATR(7) - AMPP*SINK*COSK + AMPP*COSK*SINK
         MATR(8) = MATR(8) + AMPP*SINK*AMPP*SINK*TWOP
     *      + AMPP*COSK*AMPP*COSK*TWOP
         MATR(9) = MATR(9) + AMPP*SINK*AMPP*SINK
     *      + AMPP*COSK*AMPP*COSK
 140     CONTINUE
      NOBS = 2*NOBS
C                                       NOBS need to be real for LEASQR
      CALL LEASQR (3, NOBS, SUM, SSQ, R, MATR, SOL, VX, SSQRES,
     *   VARRES, VARY, FIT, LESOL)
C                                       find the solution
      AMPP = AMPP + SOL(1)
      RATE = RATE + SOL(2)
      FI = FI + SOL(3)
      ITER = ITER + 1
      IF (ITER .LE. 2) GO TO 100
      RATE = RATE / (2.0*NNN)
      DDAMP = SQRT (VX(1))
      DDRATE = SQRT (VX(2)) / (2.0*NNN)
      DDFI = SQRT (VX(3))
C-----------------------------------------------------------------------
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AFRFI: The number of points ',I6,' is more MAX=1024')
      END
