      SUBROUTINE RFFTF (C, N, F, W)
C-----------------------------------------------------------------------
C! Vectorizable, table lookup Fast Fourier transform (non-AP)
C# Math
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
C   Vector table lookup version (Non AP)
C   Does an in-place real-to-complex forward or complex-to-real inverse
C   In the complex domain C is in a packed format; the real part of
C   C(N/2+1) is in the imaginary part of C(1). The imaginary portions
C   of these words are always zero and this convention allows the size
C   of C to be the same in either domain.
C    Inputs:
C      C(2,N/2)  R    Vector to be FFTed if complex, or
C      C(N)      R    Vector to be FFTed if real.
C      N         I    Real element count (power of 2)
C      F         I    flag, 1=>forward FFT, -1=> reverse FFT.
C                     Forward = real-to-complex
C                     Reverse = complex to real
C    Outputs:
C      C(2,N/2)  R    FFTed Vector if complex, or
C      C(N)      R    FFTed Vector if real.
C      W(2,N)    R    Work Vector.
C    Subroutines called: FOURYF
C-----------------------------------------------------------------------
      INTEGER   N, F, H, Q, LOOP, JC, JI, JJ
      REAL      C(2,N), W(2,N), TEMPR, TWOPI, PHASE, PHAS0, DPHAS,
     *   SUMR, SUMI, DIFFR, DIFFI
C                                       TABLR, TABLI for 4096 FFTs
      REAL      TABLR(1024), TABLI(1024)
      INTEGER   NOLD
      SAVE      NOLD, TABLR, TABLI
      DATA TWOPI, NOLD /6.2831852,0/
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Setup
      H = N / 2
      Q = H / 2
      JC = 1
      JI = 2
      JJ = H
C                                       See if table to be initialized
      IF (N.EQ.NOLD) GO TO 50
         NOLD = N
         PHAS0 = -TWOPI / 4.0
         DPHAS = -TWOPI / N
         DO 20 LOOP = 1,Q
            PHASE = PHAS0 + LOOP * DPHAS
            TABLR(LOOP) = COS (PHASE)
            TABLI(LOOP) = SIN (PHASE)
 20         CONTINUE
C                                       Separate code for for./rev. FFT
 50   IF (F.LT.0) GO TO 200
C                                       Forward (real to complex)
C                                       Complex FFT
         CALL FOURYF (C, H, F, W)
      INCLUDE 'INCS:ZVND.INC'
         DO 100 LOOP = 1,Q
            SUMR = C(1,JI) + C(1,JJ)
            SUMI = C(2,JI) - C(2,JJ)
            DIFFR = C(1,JI) - C(1,JJ)
            DIFFI = C(2,JI) + C(2,JJ)
            C(1,JI) = SUMR + (TABLR(LOOP) * DIFFR - TABLI(LOOP) *
     *         DIFFI)
            C(2,JI) = SUMI + (TABLI(LOOP) * DIFFR + TABLR(LOOP) *
     *         DIFFI)
            C(1,JJ) = SUMR - (TABLR(LOOP) * DIFFR - TABLI(LOOP) *
     *         DIFFI)
            C(2,JJ) = -SUMI + (TABLI(LOOP) * DIFFR + TABLR(LOOP) *
     *         DIFFI)
            JI = JI + 1
            JJ = JJ - 1
 100        CONTINUE
C                                       Pack
         TEMPR = 2.0 * (C(1,JC) + C(2,JC))
         C(2,JC) = 2.0 * (C(1,JC) - C(2,JC))
         C(1,JC) = TEMPR
         GO TO 999
C                                       Reverse (complex to real)
      INCLUDE 'INCS:ZVND.INC'
 200     DO 300 LOOP = 1,Q
            SUMR = C(1,JI) + C(1,JJ)
            SUMI = C(2,JI) - C(2,JJ)
            DIFFR = C(1,JI) - C(1,JJ)
            DIFFI = C(2,JI) + C(2,JJ)
            C(1,JI) = SUMR + (TABLR(LOOP) * DIFFR + TABLI(LOOP) *
     *         DIFFI)
            C(2,JI) = SUMI - (TABLI(LOOP) * DIFFR - TABLR(LOOP) *
     *         DIFFI)
            C(1,JJ) = SUMR - (TABLR(LOOP) * DIFFR + TABLI(LOOP) *
     *         DIFFI)
            C(2,JJ) = -SUMI - (TABLI(LOOP) * DIFFR - TABLR(LOOP) *
     *         DIFFI)
            JI = JI + 1
            JJ = JJ - 1
 300        CONTINUE
C                                       Pack
         TEMPR = (C(1,JC) + C(2,JC))
         C(2,JC) = (C(1,JC) - C(2,JC))
         C(1,JC) = TEMPR
         CALL FOURYF (C, H, F, W)
C
 999  RETURN
      END
