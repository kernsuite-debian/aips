@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QSPDIV (MODL, U, VS, INCVS, INCF, INCS, NCOMP, NVIS,
     *   NF, NS)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Divide Gaussian model  visibility into uv data.
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
C   Vector compiler version
C   Divides the model visibility derived from Gaussian components into
C   visibility records.  Arbitrary numbers of frequencies and
C   polarizations can be processed.  The weights of the visibilities
C   are multiplied times  the amplitude of the model visibility.
C   Inputs:
C      MODL  I    Base address of Gaussian components, increment=11
C                 0 = used (model amplitude)
C                 1 = used (PHASE)
C                 2 = used (REAL)
C                 3 = used (IMAG)
C                 4 = -2 * PI * X (radians)
C                 5 = -2 * PI * Y
C                 6 = -2 * PI * Z
C                 7 = R = radius * 3.0462740E-5
C                 8 = 3 * Amplitude
C                 9 = MIN = min arg (0.1 good)
C                10 = used (ARG)
C      U     I    Base address of U's, assumed followed by V, W
C      VS    I    Base address of vis rec. (real, imag, wt)
C      INCVS I    Increment of VS for next visibility
C      INCF  I    Increment of VS for next frequency
C      INCS  I    Increment of VS for next Stokes (RR or LL)
C      NCOMP I    Number of Gaussian components.
C      NVIS  I    Number of visibilities.
C      NF    I    Number of frequencies.
C      NS    I    Number of Stokes (usually 1 or 2)
C    Also uses AP locations 0 and 1 and expects an array
C    beginning in location 2 should be an array of length NF :
C             Freq(0) / Freq(ref) - 1.0
C             Freq(1) / Freq(0)   - 1.0
C                     .
C                     .
C                     .
C            Freq(NF-1) / Freq(NF-2) - 1.0
C   Note: all addresses are 0 relative and needed to be incremented by
C   1 to work in Fortran.
C   Note: the important loops to vectorize are 50,90,100,125 and 130.
C   Calls QRECT.
C-----------------------------------------------------------------------
      INTEGER   MODL, U, VS, INCVS, INCS, INCF, NCOMP, NVIS, NF, NS,
     *   PHASE, REAL, IMAG, X, JVS, F, IU, IVS, IF,
     *   IV, IS, JX, JPHAS, JAMP, ICOMP, LOOP, KAMP,
     *   KREAL, A, JA, AMP, ARG, JARG, R, JR, IMIN, JMIN
      REAL   FREQF, SUMRE, SUMIM, REMOD, IMMOD, WT, WTI, TEMP
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                        Make sure that there is data.
      IF ((NCOMP.LE.0) .OR. (NVIS.LE.0) .OR. (NF.LE.0) .OR.
     *   (NS.LE.0))  GO TO 999
C                                       Setup array addresses
      PHASE = MODL + 1
      REAL = PHASE + 1
      IMAG = REAL + 1
      X = MODL + 4
      R = MODL + 7
      AMP = MODL + 8
      IMIN = MODL + 9
      ARG = MODL + 10
      IU = U
      IVS = VS
C                                       Begin visibility loop
      DO 300 IV = 1,NVIS
C                                       Compute phase and amp argument
C                                       at ref. freq.
         JX = X + 1
         JR = R + 1
         JPHAS = PHASE + 1
         JARG = ARG + 1
         JMIN = IMIN + 1
C@DIR IGNORE RECRDEPS(APCORE)
         DO 50 LOOP = 1,NCOMP
C                                       Phase
            APCORE(JPHAS) = APCORE(JX) * APCORE(IU+1)
     *                    + APCORE(JX+1) * APCORE(IU+2)
     *                    + APCORE(JX+2) * APCORE(IU+3)
C                                       Amplitude
            APCORE(JARG) = APCORE(JR) *
     *         SQRT (APCORE(IU+1)*APCORE(IU+1) +
     *         APCORE(IU+2)*APCORE(IU+2))
C                                       Avoid numerical instability.
            APCORE(JARG) = MAX (APCORE(JARG), APCORE(JMIN))
C                                       Update pointers.
            JPHAS = JPHAS + 11
            JX = JX + 11
            JARG = JARG + 11
            JR = JR + 11
            JMIN = JMIN + 11
 50         CONTINUE
C                                        Get ready for freq. loop.
         JVS = IVS
         F = 2
         FREQF = 1.0
C                                        Begin frequency loop.
         DO 200 IF = 1,NF
C                                       Loop over component
            KREAL = MODL + 2
C                                       Check scaling
            IF (ABS (APCORE(F+1)).GT.1.0E-20) GO TO 110
C                                       Finish amplitudes
               JARG = ARG + 1
               JAMP = MODL + 1
               KAMP = AMP + 1
C@DIR IGNORE RECRDEPS(APCORE)
               DO 90 ICOMP = 1,NCOMP
                  APCORE(JAMP) = APCORE(KAMP) *
     *               (SIN (APCORE(JARG)) / (APCORE(JARG)**3) -
     *                COS (APCORE(JARG)) / (APCORE(JARG)**2))
                  JARG = JARG + 11
                  JAMP = JAMP + 11
                  KAMP = KAMP + 11
 90               CONTINUE
C                                       Convert to polar
               JAMP = MODL
               CALL QRECT (JAMP, 11, KREAL, 11, NCOMP)
               GO TO 130
C                                       Subsequent frequency
 110           FREQF = 1.0 + APCORE(F+1)
C                                       Scale frequency
               JARG = ARG + 1
               JPHAS = PHASE + 1
C@DIR IGNORE RECRDEPS(APCORE)
               DO 120 ICOMP = 1,NCOMP
                  APCORE(JPHAS) = FREQF * APCORE(JPHAS)
                  APCORE(JARG) = FREQF * APCORE(JARG)
                  JPHAS = JPHAS + 11
                  JARG = JARG + 11
 120              CONTINUE
C                                       Finish amplitudes
               JARG = ARG + 1
               JAMP = MODL + 1
               KAMP = AMP + 1
C@DIR IGNORE RECRDEPS(APCORE)
               DO 125 ICOMP = 1,NCOMP
                  APCORE(JAMP) = APCORE(KAMP) *
     *               (SIN (APCORE(JARG)) / (APCORE(JARG)**3) -
     *                COS (APCORE(JARG)) / (APCORE(JARG)**2))
                  JARG = JARG + 11
                  JAMP = JAMP + 11
                  KAMP = KAMP + 11
 125              CONTINUE
C                                       Convert to polar
               JAMP = MODL
               CALL QRECT (JAMP, 11, KREAL, 11, NCOMP)
C                                       Do sum
 130           SUMRE = 0.0
               SUMIM = 0.0
               REAL = MODL + 3
               IMAG = MODL + 4
C@DIR IGNORE RECRDEPS(APCORE)
               DO 140 ICOMP = 1,NCOMP
                  SUMRE = SUMRE + APCORE(REAL)
                  SUMIM = SUMIM + APCORE(IMAG)
                  REAL = REAL + 11
                  IMAG = IMAG + 11
 140              CONTINUE
C                                       Correct visibility
            JA = JVS + 1
C                                        Setup
            WT = (SUMRE * SUMRE + SUMIM * SUMIM)
            WT = MAX (1.0E-15, WT)
            WTI = 1.0 / WT
            REMOD = SUMRE * WTI
            IMMOD = SUMIM * WTI
            WT = SQRT (WT)
C                                       Loop over Stokes' (1 or 2,
C                                       no point in vectorizing)
            DO 150 IS = 1,NS
C                                       Divide model
               TEMP = REMOD * APCORE(JA) + IMMOD * APCORE(JA+1)
               APCORE(JA+1) = REMOD * APCORE(JA+1) - IMMOD * APCORE(JA)
               APCORE(JA) = TEMP
C                                       Modify weight
               APCORE(JA+2) = APCORE(JA+2) * WT
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
