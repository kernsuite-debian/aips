      SUBROUTINE QPTDIV (C, U, VS, INCVS, INCF, INCS, NCOMP, NVIS,
     *   NF, NS)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Divide point model visibility into uv data.
C# AP-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2006, 2008, 2012
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
C   Vector compiler version
C   Divides the model visibility derived from CLEAN components into
C   visibility records.  Arbitrary numbers of frequencies and
C   polarizations can be processed.  The weights of the visibilities
C   are multiplied times  the amplitude of the model visibility.
C   Inputs:
C      C     I  Base address of CLEAN components, increment = 4
C               0 = Amplitude
C               1 = -2 * PI * X
C               2 = -2 * PI * Y
C               3 = -2 * PI * Z
C      U     I  Base address of U's, assumed followed by V, W
C      VS    I  Base address of vis rec. (real, imag, wt)
C      INCVS I  Increment of VS for next visibility
C      INCF  I  Increment of VS for next frequency
C      INCS  I  Increment of VS for next IF (RR of LL)
C      NCOMP I  Number of CLEAN components.
C      NVIS  I  Number of visibilities.
C      NF    I  Number of frequencies.
C      NS    I  Number of IF (usually 1 or 2)
C   Also uses AP locations 0 and 1 and expects an array
C   beginning in location 2 should be an array of length NF :
C              Freq(0) / Freq(ref) - 1.0
C              Freq(1) / Freq(ref) - 1.0
C                      .
C                      .
C                      .
C              Freq(NF-1) / Freq(ref) - 1.0
C   Note: all addresses are 0 relative and needed to be incremented by
C   1 to work in Fortran.
C-----------------------------------------------------------------------
      INTEGER   C, U, VS, INCVS, INCF, INCS, NCOMP, NVIS, NF, NS
C
      INTEGER   X, JVS, IVS, IF, IV, S, IS, ICOMP
      LONGINT   F, IU, JX, JAMP, JA
      DOUBLE PRECISION FREQF, SUMRE, SUMIM, REMOD, IMMOD, WT, TEMP, PHS
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                        Make sure that there is data.
      IF ((NCOMP.LE.0) .OR. (NVIS.LE.0) .OR. (NF.LE.0) .OR.
     *   (NS.LE.0))  GO TO 999
C                                       Setup array addresses
      X = C + 1
      IU = U + PSAPOF
      IVS = VS
C                                        Begin visibility loop
C$OMP PARALLEL DO PRIVATE (IV, JVS, F, S, IF, SUMRE, SUMIM, JX, JAMP,
C$OMP+                     FREQF, ICOMP, PHS, REMOD, IMMOD, WT, IS,
C$OMP+                     TEMP, JA, IVS, IU), SHARE(APCORE)
      DO 300 IV = 1,NVIS
C                                        Get ready for freq. loop
         JVS = IVS
         F = 1 + PSAPOF
         S = NF + 2
C                                        Begin frequency loop
         DO 200 IF = 1,NF
C                                       Loop over component
            SUMRE = 0.0D0
            SUMIM = 0.0D0
            JX = X + PSAPOF
            JAMP = C + PSAPOF
            IF (ABS (APCORE(F+1)).GT.1.0D-20) THEN
               FREQF = 1.0D0 + APCORE(F+1)
            ELSE
               FREQF = 1.0D0
            END IF
            DO 100 ICOMP = 1, NCOMP
               PHS = FREQF * (APCORE(JX) * APCORE(IU)
     *                      + APCORE(JX+1) * APCORE(IU+1)
     *                      + APCORE(JX+2) * APCORE(IU+2))
               SUMRE = SUMRE + APCORE(JAMP) * COS(PHS)
               SUMIM = SUMIM + APCORE(JAMP) * SIN(PHS)
               JX = JX + 4
               JAMP = JAMP + 4
  100          CONTINUE
C                                       Correct visibility
            JA = JVS + PSAPOF
C                                        Setup
            WT = (SUMRE * SUMRE + SUMIM * SUMIM)
            WT = MAX (1.0D-15, WT)
            REMOD = SUMRE / WT
            IMMOD = SUMIM / WT
C                                       Loop over Stokes' (1 or 2,
C                                       no point in vectorizing)
            DO 150 IS = 1,NS
C                                       Divide model
               TEMP = REMOD * APCORE(JA) + IMMOD * APCORE(JA+1)
               APCORE(JA+1) = REMOD * APCORE(JA+1) - IMMOD * APCORE(JA)
               APCORE(JA) = TEMP
C                                       Modify weight by amp squared
               APCORE(JA+2) = APCORE(JA+2) * WT
               JA = JA + INCS
 150           CONTINUE
C                                        Update vis pointer
            JVS = JVS + INCF
            F = F + 1
 200        CONTINUE
C                                        Update pointers
         IVS = IVS + INCVS
         IU = IU + INCVS
 300     CONTINUE
C$OMP END PARALLEL DO
C
 999  RETURN
      END
