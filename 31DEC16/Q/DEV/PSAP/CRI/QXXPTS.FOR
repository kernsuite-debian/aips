      SUBROUTINE QXXPTS (C, U, VS, INCVS, INCF, INCS, NCOMP, NVIS,
     *   NF, NS, FLAG)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Subtract point model visibility from uv data.
C# AP-appl UV
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
C   Subtracts the model visibility derived from CLEAN components from
C   visibility records.  Arbitrary numbers of frequencies and
C   polarizations can be processed.
C     Inputs:
C       C     I    Base address of CLEAN components, increment=7
C                  0 = Amplitude
C                  1 = used (PHASE)
C                  2 = used (REAL)
C                  3 = used (IMAG)
C                  4 = -2 * PI * X
C                  5 = -2 * PI * Y
C                  6 = -2 * PI * Z
C       U     I    Base address of U's, assumed followed by V, W
C       VS    I    Base address of vis rec. (real, imag, wt)
C       INCVS I    Increment of VS for next visibility
C       INCF  I    Increment of VS for next frequency
C       INCS  I    Increment of VS for next IF (RR of LL)
C       NCOMP I    Number of CLEAN components.
C       NVIS  I    Number of visibilities.
C       NF    I    Number of frequencies.
C       NS    I    Number of IF (usually 1 or 2)
C       FLAG  I    If FLAG < 0 multiply model vis by i (SQRT(-1))
C     Also uses AP locations 0 and 1 and expects an array of length NS
C     beginning in location 2+NF composed of the correlator factors.
C     Beginning in location 2 should be an array of length NF :
C              Freq(0) / Freq(ref) - 1.0
C              Freq(1) / Freq(0)   - 1.0
C                      .
C                      .
C                      .
C              Freq(NF-1) / Freq(NF-2) - 1.0
C   Note: all addresses are 0 relative and needed to be incremented by
C   1 to work in Fortran.
C   Note: the important loops to vectorize are 50, 100, and 130.
C   CRI version.
C-----------------------------------------------------------------------
      INTEGER   C, U, VS, INCVS, INCS, INCF, NCOMP, NVIS, NF, NS,
     *   PHASE, REAL, IMAG, X, JVS, F, IU, IVS, IF, JREAL, JIMAG,
     *   IV, FLAG, S, IS, JA, JX, JPHAS, JAMP, ICOMP, LOOP, N7
      REAL   FREQF, SUMRE, SUMIM, REMOD, IMMOD, SSUM
      INCLUDE 'INCS:DAPC.INC'
      DATA N7 /7/
C-----------------------------------------------------------------------
C                                        Make sure that there is data.
      IF ((NCOMP.LE.0) .OR. (NVIS.LE.0) .OR. (NF.LE.0) .OR.
     *   (NS.LE.0))  GO TO 999
C                                       Setup array addresses
      PHASE = C + 1
      REAL = PHASE + 1
      IMAG = REAL + 1
      X = IMAG + 1
      IU = U
      IVS = VS
C                                        Begin visibility loop
      DO 300 IV = 1,NVIS
C                                        Compute phase at ref. freq.
         JX = X + 1
         JPHAS = PHASE + 1
      INCLUDE 'INCS:ZVND.INC'
         DO 50 LOOP = 1,NCOMP
            APCORE(JPHAS) = APCORE(JX) * APCORE(IU+1)
     *                    + APCORE(JX+1) * APCORE(IU+2)
     *                    + APCORE(JX+2) * APCORE(IU+3)
C                                       Update pointers.
            JPHAS = JPHAS + 7
            JX = JX + 7
 50         CONTINUE
C                                        Get ready for freq. loop.
         JVS = IVS
         F = 2
         S = NF + 2
         FREQF = 1.0
C                                        Begin frequency loop.
      INCLUDE 'INCS:ZVND.INC'
         DO 200 IF = 1,NF
C                                       Loop over component
            SUMRE = 0.0
            SUMIM = 0.0
            JPHAS = PHASE + 1
            JAMP = C + 1
            JREAL = REAL + 1
            JIMAG = IMAG + 1
C                                       Do first pass differently
            IF (IF.GT.1) GO TO 120
      INCLUDE 'INCS:ZVND.INC'
               DO 100 ICOMP = 1,NCOMP
C                                       Do DFT
                  APCORE(JREAL) = COS (APCORE(JPHAS)) * APCORE(JAMP)
                  APCORE(JIMAG) = SIN (APCORE(JPHAS)) * APCORE(JAMP)
                  JPHAS = JPHAS + 7
                  JAMP = JAMP + 7
                  JREAL = JREAL + 7
                  JIMAG = JIMAG + 7
 100              CONTINUE
               GO TO 140
C                                       Subsequent frequency
 120           FREQF = FREQF * (1.0 + APCORE(F+1))
      INCLUDE 'INCS:ZVND.INC'
               DO 130 ICOMP = 1,NCOMP
C                                       Do DFT
                  APCORE(JREAL) = COS (FREQF*APCORE(JPHAS)) *
     *               APCORE(JAMP)
                  APCORE(JIMAG) = SIN (FREQF*APCORE(JPHAS)) *
     *            APCORE(JAMP)
                  JPHAS = JPHAS + 7
                  JAMP = JAMP + 7
                  JREAL = JREAL + 7
                  JIMAG = JIMAG + 7
 130              CONTINUE
C                                       Correct visibility
 140        JA = JVS + 1
C                                       Sum real and imaginary
            SUMRE = SSUM (NCOMP, APCORE(REAL+1), N7)
            SUMIM = SSUM (NCOMP, APCORE(IMAG+1), N7)
            REMOD = SUMRE
            IMMOD = SUMIM
            IF (FLAG.LT.0) REMOD = -SUMIM
            IF (FLAG.LT.0) IMMOD = SUMRE
C                                       Loop over Stokes' (1 or 2)
            DO 150 IS = 1,NS
C                                       Subtract model
               APCORE(JA) = APCORE(JA) - REMOD * APCORE(S+IS)
               APCORE(JA+1) = APCORE(JA+1) - IMMOD * APCORE(S+IS)
               JA = JA + INCS
 150           CONTINUE
C                                        Update vis pointer.
            JVS = JVS + INCF
            F = F + 1
 200        CONTINUE
C                                        Update pointers.
         IVS = IVS + INCVS
         IU = IU + INCVS
 300     CONTINUE
C
 999  RETURN
      END
