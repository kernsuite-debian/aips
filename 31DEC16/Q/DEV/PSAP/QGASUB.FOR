      SUBROUTINE QGASUB (MODL, U, VS, INCVS, INCF, INCS, NCOMP, NVIS,
     *   NF, NS, FLAG)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Subtract Gaus. model vis. from uv data.
C# AP-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2000, 2006-2007, 2012
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
C   Subtracts the model visibility derived from Gaussian components from
C   visibility records.  Arbitrary numbers of frequencies and
C   polarizations can be processed.
C   Inputs:
C      MODL   I  Base address of Gaussian components, increment = 7
C                0 = Amplitude
C                1 = -2 * PI * X (radians)
C                2 = -2 * PI * Y
C                3 = -2 * PI * Z
C                4 = A = -0.5*((COS(PA)*SIG(MAJ))**2 +
C                        (SIN(PA)*SIG(MIN))**2)
C                5 = B = -0.5*((SIN(PA)*SIG(MAJ))**2 +
C                        (COS(PA)*SIG(MIN))**2)
C                6 = C = -(SIN(PA)*COS(PA)*
C                         (SIG(MAJ)**2 - SIG(MIN)**2)
C      U     I  Base address of U's, assumed followed by V, W
C      VS    I  Base address of vis rec. (real, imag, wt)
C      INCVS I  Increment of VS for next visibility
C      INCF  I  Increment of VS for next frequency
C      INCS  I  Increment of VS for next Stokes (RR or LL)
C      NCOMP I  Number of Gaussian components.
C      NVIS  I  Number of visibilities.
C      NF    I  Number of frequencies.
C      NS    I  Number of Stokes (usually 1 or 2)
C      FLAG  I  If FLAG < 0 multiply model vis by i (SQRT(-1))
C   Also uses AP locations 0 and 1 and expects an array of length NS
C   beginning in location 2+NF composed of the correlator factors.
C   Beginning in location 2 should be an array of length NF :
C              Freq(0) / Freq(ref) - 1.0
C              Freq(1) / Freq(ref) - 1.0
C                      .
C                      .
C                      .
C              Freq(NF-1) / Freq(ref) - 1.0
C   Note: all addresses are 0 relative and needed to be incremented by
C   1 to work in Fortran.
C-----------------------------------------------------------------------
      INTEGER   MODL, U, VS, INCVS, INCS, INCF, NCOMP, NVIS, NF, NS,
     *   FLAG
C
      INTEGER   X, JVS, IVS, IF, IV, IS, ICOMP, A, AMP
      LONGINT   IU, F, JX, JA, JAMP, S
      DOUBLE PRECISION FREQF, SUMRE, SUMIM, REMOD, IMMOD, FREQ2, PHS,
     *   AMPL, UU, VV, UV
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                        Make sure that there is data.
      IF ((NCOMP.LE.0) .OR. (NVIS.LE.0) .OR. (NF.LE.0) .OR.
     *   (NS.LE.0))  GO TO 999
C                                       Setup array addresses
      X = MODL + 1
      A = MODL + 4
      AMP = MODL
      IU = U + PSAPOF - 1
      IVS = VS
C                                       Begin visibility loop
C$OMP PARALLEL DO PRIVATE (IV, JVS, F, S, IF, SUMRE, SUMIM, JX, JAMP,
C$OMP+              FREQF, ICOMP, PHS, REMOD, IMMOD, IS, FREQ2, UU,
C$OMP+              VV, UV, JA, IVS, IU, AMPL), SHARE(APCORE)
      DO 300 IV = 1,NVIS
C                                       Get ready for freq. loop
         JVS = IVS
         F = 1 + PSAPOF
         S = NF + 1 + PSAPOF
         UU = APCORE(IU+1) * APCORE(IU+1)
         VV = APCORE(IU+2) * APCORE(IU+2)
         UV = APCORE(IU+1) * APCORE(IU+2)
C                                       Begin frequency loop
         DO 200 IF = 1,NF
            IF (ABS (APCORE(F+1)).GT.1.0D-20) THEN
               FREQF = 1.0D0 + APCORE(F+1)
            ELSE
               FREQF = 1.0D0
               END IF
            FREQ2 = FREQF * FREQF
C                                       Loop over component
            SUMRE = 0.0D0
            SUMIM = 0.0D0
            JX = X + PSAPOF
            JA = A + PSAPOF
            JAMP = AMP + PSAPOF
            DO 100 ICOMP = 1,NCOMP
               PHS = FREQF * (APCORE(JX) * APCORE(IU+1)
     *                      + APCORE(JX+1) * APCORE(IU+2)
     *                      + APCORE(JX+2) * APCORE(IU+3))
               AMPL = FREQ2 * (APCORE(JA) * VV
     *            + APCORE(JA+1) * UU + APCORE(JA+2) * UV)
               AMPL = APCORE(JAMP) * EXP (AMPL)
               SUMRE = SUMRE + AMPL * COS(PHS)
               SUMIM = SUMIM + AMPL * SIN(PHS)
               JX = JX + 7
               JA = JA + 7
               JAMP = JAMP + 7
 100           CONTINUE
C                                       Correct visibility
            JA = JVS + PSAPOF
C                                        Setup
            IF (FLAG.LT.0) THEN
               REMOD = -SUMIM
               IMMOD = SUMRE
            ELSE
               REMOD = SUMRE
               IMMOD = SUMIM
               END IF
C                                       Loop over Stokes' (1 or 2,
C                                       no point in vectorizing)
C                                       return model
            IF (APCORE(S+3).EQ.0.0D0) THEN
               DO 150 IS = 1,NS
                  APCORE(JA) = REMOD * APCORE(S+IS)
                  APCORE(JA+1) = IMMOD * APCORE(S+IS)
                  JA = JA + INCS
 150              CONTINUE
C                                       Subtract model from data
            ELSE
               DO 160 IS = 1,NS
                  APCORE(JA) = APCORE(JA) - REMOD * APCORE(S+IS)
                  APCORE(JA+1) = APCORE(JA+1) - IMMOD * APCORE(S+IS)
                  JA = JA + INCS
 160              CONTINUE
               END IF
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
