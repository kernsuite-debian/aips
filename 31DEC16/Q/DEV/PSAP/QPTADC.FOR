      SUBROUTINE QPTADC (C, U, VS, BWCOR, INCVS, INCF, INCS, NCOMP,
     *   NVIS, NF, NS, FLAG)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Add corrected model to uv data.
C# AP-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2006, 2012
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
C   Fourier transforms and corrects point models and adds to a set of
C   visibility data.  Corrections are made for "W" and bandwidth
C   smearing.  Uses table lookup for the bandwidth smearing correction.
C   Arbitrary numbers of frequencies and polarizations can be processed.
C   No changes are made to the weights.
C   Inputs:
C      C     I  Base address of CLEAN components, increment=9
C               0 = Amplitude (true)
C               1 = used (BW smearing offset)
C               2 = used (corrected amplitude)
C               3 = used (PHASE)
C               4 = used (REAL)
C               5 = used (IMAG)
C               6 = -2 * PI * X
C               7 = -2 * PI * Y
C               8 = -2 * PI * Z
C      U     I  Base address of U's, assumed followed by V, W
C      VS    I  Base address of vis rec. (real, imag, wt)
C      BWCOR I  Base address of Bandwidth smearing parameters.
C               0 = Bandwidth smearing factor = delta_nu / nu
C                  No correction applied if = 0.0
C               1 = Total X offset from delay position (radians)
C               2 = Total Y offset from delay position (radians)
C               3 = Total Z offset from delay position (radians)
C               4 = Maximum correction factor. 0=>1.0
C      INCVS I  Increment of VS for next visibility
C      INCF  I  Increment of VS for next frequency
C      INCS  I  Increment of VS for next IF (RR of LL)
C      NCOMP I  Number of CLEAN components.
C      NVIS  I  Number of visibilities.
C      NF    I  Number of frequencies.
C      NS    I  Number of Stokes' (usually 1 or 2)
C      FLAG  I  If FLAG < 0 multiply model vis by i (SQRT(-1))
C   Also uses AP locations 0 and 1 and expects an array of length NS
C   beginning in location 2+NF composed of the correlator factors.
C   Beginning in location 2 should be an array of length NF :
C            Freq(0) / Freq(ref) - 1.0
C            Freq(1) / Freq(ref) - 1.0
C                    .
C                    .
C                    .
C            Freq(NF-1) / Freq(ref) - 1.0
C   Note: all addresses are 0 relative and needed to be incremented by
C   1 to work in Fortran.
C-----------------------------------------------------------------------
      INTEGER   C, U, VS, BWCOR, INCVS, INCF, INCS, NCOMP, NVIS, NF, NS,
     *   FLAG
C
      INTEGER LENCC, TABSIZ
C                                       LENCC = length of CC structure
      PARAMETER (LENCC = 9)
C                                       TABSIZ = size of sinc table.
      PARAMETER (TABSIZ=1001)
      INTEGER   BWOFF, CAMP, PHASE, REAL, IMAG, X, JVS, IVS, IF, IV,
     *    IS, ICOMP, LOOP, NSC, ICEN, ITRUNC, IBWC
      LONGINT   JX, JPHAS, IU, JBWO, JBWC, F, JAMP, JCAMP, JA, S
      LOGICAL   DOBWC
      DOUBLE PRECISION FREQF, SUMRE, SUMIM, REMOD, IMMOD, BWFACT,
     *   BWCFAC, ITWOPI, TEMP, DELTA, ARG, SINTBI(TABSIZ), MAXBWC, PHS
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DAPC.INC'
      SAVE NSC, SINTBI
      DATA NSC /-1/
C-----------------------------------------------------------------------
C                                        Make sure that there is data.
      IF ((NCOMP.LE.0) .OR. (NVIS.LE.0) .OR. (NF.LE.0) .OR.
     *   (NS.LE.0))  GO TO 999
C                                       BW corr. table
      IF (NSC.NE.TABSIZ) THEN
         NSC = TABSIZ
         ICEN =  1
C                                       Full turn in table.
         DELTA = 2.0D0 / (NSC - 1)
         DO 10 LOOP = 1,NSC
            ARG = PI * (LOOP - ICEN) * DELTA
            TEMP = SIN (ARG)
            IF (ABS(TEMP).GT.1.0D-20) THEN
               SINTBI(LOOP) = PI / TEMP
            ELSE
               SINTBI(LOOP) = 2000.0D0
               END IF
 10         CONTINUE
         END IF
C                                       Setup array addresses
      BWOFF = C + 1
      CAMP = BWOFF + 1
      PHASE = CAMP + 1
      REAL = PHASE + 1
      IMAG = REAL + 1
      X = IMAG + 1
      IU = U + PSAPOF - 1
      IVS = VS
      ITWOPI = 1.0D0 / TWOPI
C                                       Bandwidth smearing correction?
      BWCFAC = APCORE(BWCOR+PSAPOF)
      DOBWC = BWCFAC .GT. 1.0D-20
      MAXBWC = MAX (1.0D0, APCORE(BWCOR+4+PSAPOF))
C                                        Begin visibility loop
      DO 300 IV = 1,NVIS
C                                        Compute phase at ref. freq.
         JX = X + PSAPOF
         JPHAS = PHASE + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
         DO 50 LOOP = 1,NCOMP
            APCORE(JPHAS) = APCORE(JX) * APCORE(IU+1)
     *                    + APCORE(JX+1) * APCORE(IU+2)
     *                    + APCORE(JX+2) * APCORE(IU+3)
C                                       Update pointers.
            JPHAS = JPHAS + LENCC
            JX = JX + LENCC
 50         CONTINUE
C                                       Total phase offset from pointing
C                                       position.
         JBWO = BWOFF + PSAPOF
         JBWC = BWCOR + PSAPOF
         JPHAS = PHASE + PSAPOF
         IF (DOBWC .AND. ((ABS (APCORE(JBWC+1)).GT.1.0D-20) .OR.
     *      (ABS (APCORE(JBWC+1)).GT.1.0D-20) .OR.
     *      (ABS (APCORE(JBWC+1)).GT.1.0D-20))) THEN
      INCLUDE 'INCS:ZVND.INC'
            DO 60 LOOP = 1,NCOMP
               TEMP = APCORE(JPHAS)
     *            + APCORE(JBWC+1) * APCORE(IU+1)
     *            + APCORE(JBWC+2) * APCORE(IU+2)
     *            + APCORE(JBWC+3) * APCORE(IU+3)
C                                       Total phase in turns
               APCORE(JBWO) = TEMP * ITWOPI
               JBWO = JBWO + LENCC
               JPHAS = JPHAS + LENCC
 60            CONTINUE
         ELSE IF (DOBWC) THEN
      INCLUDE 'INCS:ZVND.INC'
            DO 70 LOOP = 1,NCOMP
               APCORE(JBWO) = APCORE(JPHAS) * ITWOPI
               JBWO = JBWO + LENCC
               JPHAS = JPHAS + LENCC
 70            CONTINUE
            END IF
C                                        Get ready for freq. loop.
         JVS = IVS
         F = 1 + PSAPOF
         S = NF + 1 + PSAPOF
         FREQF = 1.0D0
C                                        Begin frequency loop
         DO 200 IF = 1,NF
C                                       Loop over component
            JPHAS = PHASE + PSAPOF
            JBWO = BWOFF + PSAPOF
C                                       Check scaling
            IF (ABS(APCORE(F+1)).GT.1.0D-20) FREQF = 1.0D0 + APCORE(F+1)
C                                       Copy amplitudes/correct for bw
C                                       smearing
            JAMP = C + PSAPOF
            JCAMP = CAMP + PSAPOF
            JBWO = BWOFF + PSAPOF
            IF (DOBWC) THEN
               DELTA = (NSC - 1) / 2.0D0
      INCLUDE 'INCS:ZVND.INC'
               DO 140 ICOMP = 1,NCOMP
C                                       Bandwidth smearing factor
C                                       This technique gets flaky as
C                                       arg approaches 0.
                  ARG = ABS (BWCFAC * FREQF * APCORE(JBWO))
                  IF (ARG.GT.0.05D0) THEN
                     ITRUNC = ARG / 2.0D0
                     IBWC = (ARG - ITRUNC*2.0D0) * DELTA + 1.5D0
                     BWFACT = ARG * SINTBI(IBWC)
                     IF (BWFACT.GT.MAXBWC) BWFACT = MAXBWC
                     IF (BWFACT.LT.0.98D0) BWFACT = MAXBWC
                     APCORE(JCAMP) = APCORE(JAMP) * BWFACT
                  ELSE
                     APCORE(JCAMP) = APCORE(JAMP)
                     END IF
                  JCAMP = JCAMP + LENCC
                  JAMP = JAMP + LENCC
                  JBWO = JBWO + LENCC
 140              CONTINUE
            ELSE
C                                       No smearing correction
      INCLUDE 'INCS:ZVND.INC'
               DO 150 ICOMP = 1,NCOMP
                  APCORE(JCAMP) = APCORE(JAMP)
                  JCAMP = JCAMP + LENCC
                  JAMP = JAMP + LENCC
 150              CONTINUE
               END IF
C                                       Do sum
            SUMRE = 0.0D0
            SUMIM = 0.0D0
            JAMP = C + PSAPOF
            JPHAS = PHASE + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
            DO 160 ICOMP = 1,NCOMP
               PHS = APCORE(JPHAS) * FREQF
               SUMRE = SUMRE + APCORE(JAMP) * COS(PHS)
               SUMIM = SUMIM + APCORE(JAMP) * SIN(PHS)
               JAMP  = JAMP + LENCC
               JPHAS = JPHAS + LENCC
 160           CONTINUE
C                                       Correct visibility
            JA = JVS + PSAPOF
            REMOD = SUMRE
            IMMOD = SUMIM
            IF (FLAG.LT.0) THEN
               REMOD = -SUMIM
               IMMOD = SUMRE
               END IF
C                                       Loop over Stokes' (1 or 2)
            DO 170 IS = 1,NS
C                                       Add model
               APCORE(JA) = APCORE(JA) + REMOD * APCORE(S+IS)
               APCORE(JA+1) = APCORE(JA+1) + IMMOD * APCORE(S+IS)
               JA = JA + INCS
 170           CONTINUE
C                                        Update vis pointer
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
