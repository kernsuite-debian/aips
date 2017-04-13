      SUBROUTINE QRFFT (C, N, F)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Real-half plane complex FFT
C# AP-FFT
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2005-2006, 2010-2012, 2015
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
C   Vector table lookup version
C   Does an in-place real-to-complex forward or complex-to-real inverse
C   In the complex domain C is in a packed format; the real part of
C   C(N/2+1) is in the imaginary part of C(1). The imaginary portions
C   of these words are always zero and this convention allows the size
C   of C to be the same in either domain.
C   Inputs:
C      C  I  Base address of source and destiantion vector
C      N  I  Real element count (power of 2)
C      F  I  flag, 1=>forward FFT, -1=> reverse FFT.
C                 Forward = real-to-complex
C                 Reverse = complex to real
C---------------------------------------------------------------------
      INTEGER   C, N, F
C
      INTEGER   H, Q, LOOP
      LONGINT   JC, JI, JJ
      DOUBLE PRECISION TEMPR, PHASE, PHAS0, DPHAS, SUMR, SUMI, DIFFR,
     *   DIFFI
C                                       TABLR, TABLI for MAXIMG FFTs
      INCLUDE 'INCS:PMAD.INC'
      DOUBLE PRECISION TABLR(MAXIMG/2), TABLI(MAXIMG/2)
      INTEGER   NOLD
      INCLUDE 'INCS:DAPC.INC'
      INCLUDE 'INCS:PSTD.INC'
      SAVE NOLD, TABLR, TABLI
      DATA NOLD /0/
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Setup
      H = N / 2
      Q = H / 2
      JC = C + PSAPOF
      JI = JC + 2
      JJ = JC + 2 * H - 2
C                                       See if table to be initilized
      IF (N.NE.NOLD) THEN
         NOLD = N
         PHAS0 = -TWOPI / 4.0D0
         DPHAS = -TWOPI / N
         DO 20 LOOP = 1,Q
            PHASE = PHAS0 + LOOP * DPHAS
            TABLR(LOOP) = COS (PHASE)
            TABLI(LOOP) = SIN (PHASE)
 20         CONTINUE
         END IF
C                                       Separate code for for./rev. FFT
C                                       Forward (real to complex)
C                                       Complex FFT
      IF (F.GE.0) THEN
         CALL QCFFT (C, H, F)
      INCLUDE 'INCS:ZVND.INC'
         DO 100 LOOP = 1,Q
            SUMR = APCORE(JI) + APCORE(JJ)
            SUMI = APCORE(JI+1) - APCORE(JJ+1)
            DIFFR = APCORE(JI) - APCORE(JJ)
            DIFFI = APCORE(JI+1) + APCORE(JJ+1)
            APCORE(JI) = SUMR + (TABLR(LOOP) * DIFFR - TABLI(LOOP) *
     *         DIFFI)
            APCORE(JI+1) = SUMI + (TABLI(LOOP) * DIFFR + TABLR(LOOP) *
     *         DIFFI)
            APCORE(JJ) = SUMR - (TABLR(LOOP) * DIFFR - TABLI(LOOP) *
     *         DIFFI)
            APCORE(JJ+1) = -SUMI + (TABLI(LOOP) * DIFFR + TABLR(LOOP) *
     *         DIFFI)
            JI = JI + 2
            JJ = JJ - 2
 100        CONTINUE
C                                       Pack
         TEMPR = 2.0D0 * (APCORE(JC) + APCORE(JC+1))
         APCORE(JC+1) = 2.0D0 * (APCORE(JC) - APCORE(JC+1))
         APCORE(JC) = TEMPR
C                                       Reverse (complex to real)
      ELSE
      INCLUDE 'INCS:ZVND.INC'
         DO 300 LOOP = 1,Q
            SUMR = APCORE(JI) + APCORE(JJ)
            SUMI = APCORE(JI+1) - APCORE(JJ+1)
            DIFFR = APCORE(JI) - APCORE(JJ)
            DIFFI = APCORE(JI+1) + APCORE(JJ+1)
            APCORE(JI) = SUMR + (TABLR(LOOP) * DIFFR + TABLI(LOOP) *
     *         DIFFI)
            APCORE(JI+1) = SUMI - (TABLI(LOOP) * DIFFR - TABLR(LOOP) *
     *         DIFFI)
            APCORE(JJ) = SUMR - (TABLR(LOOP) * DIFFR + TABLI(LOOP) *
     *         DIFFI)
            APCORE(JJ+1) = -SUMI - (TABLI(LOOP) * DIFFR - TABLR(LOOP) *
     *         DIFFI)
            JI = JI + 2
            JJ = JJ - 2
 300        CONTINUE
C                                       Pack
         TEMPR = (APCORE(JC) + APCORE(JC+1))
         APCORE(JC+1) = (APCORE(JC) - APCORE(JC+1))
         APCORE(JC) = TEMPR
C                                       Complex FFT
         CALL QCFFT (C, H, F)
         END IF
C
 999  RETURN
      END
