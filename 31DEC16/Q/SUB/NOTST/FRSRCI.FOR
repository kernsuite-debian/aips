      SUBROUTINE FRSRCI (NF, NT, MF, MT, ND, NR, DATA, D, R, PA, PP,
     *   IRET)
C-----------------------------------------------------------------------
C! 2-D fringe search with second-order interpolation
C# Util
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
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
C   Perform an FFT search for fringes in delay and rate.  If the peak
C   is not at the edge of the delay or rate window then refine the
C   delay and rate by fitting a second order polynomial to the peak
C   and its neighbouring points for each dimension separately.
C
C   When this routine is called, the raw data should be stored as
C   an NT x NF array (time varying fastest) starting at AP address
C   data.  The next 2 * NR * NF words in the AP are used to store
C   the results of the time->rate transforms as an NR x NF array of
C   complex numbers.  The next 2 * NR * ND words hold the results
C   of the 2D transform as an ND x NR (delay varies fastest) array
C   of complex numbers.  The next 2 * MAX(MF, MT) words of the AP
C   are used as scratch space for the FFTs.  The AP size must,
C   therefore be greater than or equal to 2 * (NT * NF + NR * NF +
C   NR * ND + MAX(MF, MT)) + DATA words.  Words 1 through 6 of the
C   AP are used as scratch space.
C
C   This routine is derived from QSEARC.
C
C   Inputs:
C      NF    I  No. frequency channels.  Must be greater than 0.
C      NT    I  No. time points.  Must be greater than 0.
C      MF    I  No. points for freq. FFT.  This must be a power of 2
C               and greater than or equal to NF.
C      MT    I  No. points for time FFT.  This must be a power of 2
C               and greater than or equal to NT.
C      ND    I  No. delay channels to search. 0 < ND <= MF and
C               ND should be odd
C      NR    I  No. rate channels to search.  0 < NR <= MT and
C               NR should be odd
C      DATA  I  Base address of data array (NT,NF)
C
C    Outputs:
C      D     R  Delay value in cells
C      R     R  Rate value in cells
C      PA    R  Peak amplitude (NB: not interpolated)
C      PP    R  Phase at peak in radians
C      IRET  I  Status: 0 -> rate and delay positions interpolated
C                       1 -> rate not interpolated because the peak
C                            was too close to the edge of the rate
C                            window or there were too few points in
C                            the rate window.
C                       10 -> delay not interpolated because the peak
C                             was too close to the edge of the delay
C                             window or there were too few points in
C                             the delay window.
C                       11 -> neither delay nor rate were interpolated
C-----------------------------------------------------------------------
      INTEGER   NF, NT, MF, MT, ND, NR, DATA, IRET
      REAL      D, R, PA, PP
C
C                                       Base address of intermediate
C                                       array
      INTEGER   INTER
C                                       Base address of final array
      INTEGER   FINAL
C                                       Base address of FFT scratch
C                                       array
      INTEGER   FFT
C                                       Mid-point of FFT
      INTEGER   FFT2
C                                       AP location pointers
      INTEGER   LOC, LOC1
C                                       Number of FFTs to perform
      INTEGER   COUNT
C                                       Current FFT number
      INTEGER   LOOP
C                                       Half length of FFT
      INTEGER   HALF
C                                       Pointer to source data for
C                                       FFT
      INTEGER   DDATA
C                                       1-D array index of peak
      INTEGER   PKIDX
C                                       Delay and rate indices
      INTEGER   DIDX, RIDX
C                                       Amplitudes at the peak and its
C                                       neighbouring points (overwritten
C                                       by polynomial coefficients)
      REAL      Y(3)
C                                       Cell coordinates of the peak
C                                       and its neighbours.
      REAL      X(3)
C                                       Phases around peak.
      REAL      PHASE(3)
C                                       Phase gradient
      REAL      PGRAD
C                                       Phase offsets from
C                                       interpolation in delay and rate
      REAL      POD, POR
C
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                        Set pointers.
      INTER = DATA + 2 * NF * NT
      FINAL = INTER + 2 * NF * NR
      FFT = FINAL + 2 * NR * ND
C                                       (Should look into unswapping
C                                       the FFT results as they are
C                                       read back in the following
C                                       blocks to simplify later
C                                       logic -- CF)
C                                       Transform time to rate:
      DDATA = DATA
      LOC   = INTER
      COUNT = NF
      FFT2  = FFT + 2 * MT - NR + 1
      DO 100 LOOP = 1,COUNT
C                                       Clear scratch space
         CALL QVCLR (FFT, 1, 2 * MT)
         CALL QWR
C                                       Move data to scratch array
         CALL QVMOV (DDATA, 1, FFT, 1, 2 * NT)
         CALL QWR
C                                       Do FFT
         CALL QCFFT (FFT, MT, -1)
C                                       Move transformed data to the
C                                       intermediate array
         HALF = NR / 2 + 1
         CALL QWR
         CALL QCVMOV (FFT, 2, LOC, 2, HALF)
         LOC = LOC + 2 * HALF
         HALF = NR / 2
         IF (HALF .GT. 0) THEN
            CALL QWR
            CALL QCVMOV (FFT2, 2, LOC, 2, HALF)
         END IF
C                                       Update pointers
         DDATA = DDATA + 2 * NT
         LOC = LOC + 2 * HALF
         CALL QWR
  100 CONTINUE
C                                       Transform frequency to delay
      LOC = FINAL
      DDATA = INTER
      COUNT = NR
      FFT2 = FFT + 2 * MF - ND + 1
      DO 200 LOOP = 1,COUNT
C                                       Clear scratch space
         CALL QVCLR (FFT, 1, 2 * MF)
         CALL QWR
C                                       Move data to scratch array
         CALL QCVMOV (DDATA, 2 * NR, FFT, 2, NF)
         CALL QWR
C                                       Do FFT
         CALL QCFFT (FFT, MF, -1)
C                                       Move transformed data to the
C                                       intermediate array
         HALF = ND / 2 + 1
         CALL QCVMOV (FFT, 2, LOC, 2, HALF)
         LOC = LOC + 2 * HALF
         HALF = ND / 2
         IF (HALF .GT. 0) THEN
            CALL QWR
            CALL QCVMOV (FFT2, 2, LOC, 2, HALF)
         END IF
C                                       Update pointers
         LOC = LOC + 2 * HALF
         DDATA = DDATA + 2
         CALL QWR
 200     CONTINUE
C                                       Find peak
      CALL QCVMMA (FINAL, 2, 0, NR * ND)
      CALL QWR
      CALL QGSP (PKIDX, 15)
      CALL QWD
C                                       Convert peak to polar form and
C                                       read back the amplitude and
C                                       phase using AP locations 1 and 2
      LOC = FINAL + 2 * (PKIDX - 1)
      CALL QPOLAR (LOC, 2, 1, 2, 1)
      CALL QWR
      CALL QGET(PA, 1, 1, 0)
      CALL QGET(PP, 2, 1, 0)
      CALL QWD
C                                       Extract delay and rate
C                                       coordinates
      IRET = 0
      RIDX = (PKIDX - 1) / ND
      IF (RIDX .GT. NR / 2) THEN
         RIDX = RIDX - NR
      END IF
      IF (NR .EQ. 1) THEN
         RIDX = 0
      END IF
      DIDX = MOD(PKIDX, ND) - 1
      IF (DIDX .GT. ND / 2) THEN
         DIDX = DIDX - ND
      END IF
      IF (ND .EQ. 1) THEN
         DIDX = 0
      END IF
C                                       Interpolate in rate if possible
      IF ((NR .GE. 3) .AND. (RIDX .GT. -NR / 2)
     *   .AND. (RIDX .LT. NR / 2))  THEN
         DO 300 LOOP = -1, 1
            X(LOOP + 2) = LOOP
            LOC1 = LOC + 2 * LOOP * ND
C                                       Fix LOC1 to account for the
C                                       "corners-in" data order of the
C                                       FINAL array.
            IF ((LOOP .EQ. -1) .AND. (RIDX .EQ. 0)) THEN
               LOC1 = LOC1 + 2 * NR * ND
            ELSE IF ((LOOP .EQ. 1) .AND. (RIDX .EQ. -1)) THEN
               LOC1 = LOC1 - 2 * NR * ND
            END IF
            CALL QPOLAR(LOC1, 2, 1, 2, 1)
            CALL QWR
            CALL QGET(Y(LOOP + 2), 1, 1, 0)
            CALL QGET(PHASE(LOOP + 2), 2, 1, 0)
            CALL QWD
  300    CONTINUE
         CALL SVANDT(2, X, Y)
         R = RIDX - 0.5 * Y(2) / Y(3)
         IF ((R - RIDX) .LT. 0.0) THEN
            PGRAD = PHASE(2) - PHASE(1)
         ELSE
            PGRAD = PHASE(3) - PHASE(2)
         END IF
C                                       Assume gradient should not
C                                       exceed half a turn per cell
         IF (PGRAD .GT. PI) THEN
            PGRAD = PGRAD - TWOPI
         ELSE IF (PGRAD .LT. -PI) THEN
            PGRAD = PGRAD + TWOPI
         END IF
         POR = PGRAD * (R - RIDX)
      ELSE
         IRET = IRET + 1
         R = RIDX
         POR = 0.0
      END IF
C                                       Interpolate in delay if possible
      IF ((ND .GE. 3) .AND. (DIDX .GT. -ND / 2)
     *   .AND. (DIDX .LT. ND / 2))  THEN
         DO 400 LOOP = -1, 1
            X(LOOP + 2) = LOOP
            LOC1 = LOC + 2 * LOOP
C                                       Fix LOC1 to account for the
C                                       "corners-in" data order of the
C                                       FINAL array.
            IF ((LOOP .EQ. -1) .AND. (DIDX .EQ. 0)) THEN
               LOC1 = LOC1 + 2 * ND
            ELSE IF ((LOOP .EQ. 1) .AND. (DIDX .EQ. -1)) THEN
               LOC1 = LOC1 - 2 * ND
            END IF
            CALL QPOLAR(LOC1, 2, 1, 2, 1)
            CALL QWR
            CALL QGET(Y(LOOP + 2), 1, 1, 0)
            CALL QGET(PHASE(LOOP + 2), 2, 1, 0)
            CALL QWD
  400    CONTINUE
         CALL SVANDT(2, X, Y)
         D = DIDX - 0.5 * Y(2) / Y(3)
         IF ((D - DIDX) .LT. 0.0) THEN
            PGRAD = PHASE(2) - PHASE(1)
         ELSE
            PGRAD = PHASE(3) - PHASE(2)
         END IF
C                                       Assume gradient should not
C                                       exceed half a turn per cell
         IF (PGRAD .GT. PI) THEN
            PGRAD = PGRAD - TWOPI
         ELSE IF (PGRAD .LT. -PI) THEN
            PGRAD = PGRAD + TWOPI
         END IF
         POD = PGRAD * (D - DIDX)
      ELSE
         IRET = IRET + 1
         D = DIDX
         POD = 0.0
      END IF
C                                       Fix up phase
      PP = PP + POR + POD
C
      END

