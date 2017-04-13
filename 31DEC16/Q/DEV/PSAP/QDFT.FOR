      SUBROUTINE QDFT (UV, VIS, WT, L, INCF, NVIS, NFREQ, NX, NY, MAP,
     *   BEAM)
C-----------------------------------------------------------------------
C! Pseudo AP routine: DFT uv data.
C# AP-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002, 2006, 2012
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
C   Vectorizing compiler version
C   DFT a set of uvdata making a dirty image and a beam.  The "w-term"
C   is included in making the image.
C   If requested, positions are shifted and then a taper is applied to
C   the weights if requested before gridding. Multiple frequency
C   channels may be imaged together for bandwidth synthesis.
C      Two work words are assumed before the first correlation in each
C   visibility.
C      Note: this version uses a sine/cosine lookup table to improve
C   performance.
C   Inputs:
C      UV    I  base address of u,v vector
C      VIS   I  base address of visibility
C      WT    I  base address of weights
C      L     I  length of visibility record
C      INCF  I  Increment between visibilities and weights,
C               stepping in frequency.
C      NVIS  I  number of visibility points.
C      NFREQ I  number of frequencies to grid.
C      NX    I  Number of images cells in the X (RA or long.) dir.
C      NY    I  Number of images cells in the Y (Dec. or lat.) dir.
C      MAP   I  Base address of image (cell 1,1).
C      BEAM  I  Base address of beam (cell 1,1).
C               If > 0 then no beam is required.
C   Expects necessary constants in following AP locations:
C           9 = -SIG(U)**2 (lamda**-2) for taper
C          10 = -SIG(V)**2 (lamda**-2) for taper
C               No taper applied if = 0.0
C          11 = X cells to radian scaling
C          12 = Y cells to radian scaling
C          13
C          14 = DXC = - delta X (radians)
C          15 = DYC = - delta Y
C          16 = DZC = - delta Z
C               No shift applied if = 0.0
C          17 = XCEN = central cell in X
C          18 = YCEN = central cell in Y
C          20...20+NFREQ-1 = differential frequency scaling table
C               FREQ(n) = (1+MD(19+n-1)) * FREQ(n-1) for n>1
C   NOTE: Input addresses are 0 relative to PSAPOF, must be converted to
C   LONGINT before use.
C-----------------------------------------------------------------------
      INTEGER   UV, VIS, WT, L, INCF, NVIS, NFREQ, NX, NY, MAP, BEAM
C
      INTEGER   TABSIZ
C                                       TABSIZ = size of sine/cosine table.
      PARAMETER (TABSIZ=1001)
      LONGINT   JA, JB, JC, IWRK, MAPPNT, BEMPNT
      INTEGER   VVIS, WWT, IFRQ, JNFREQ, JNVIS, LOOP, WORK, I, J, K,
     *   NSC, ICEN, ITRUNC, IPHASE
      LOGICAL   DOBEAM, TAPER, SHIFT
      DOUBLE PRECISION VALUE1, VALUE2, VALUE3, TEMPR1, TEMPR2, TEMPI1,
     *   TEMPI2, XCEN, YCEN, X, Y, Z, SUM, SUMB, PHASE, ARG, TWOPI, PI,
     *   SINTAB(TABSIZ), COSTAB(TABSIZ), DELTA, RE, IM, TEXP
      INCLUDE 'INCS:DAPC.INC'
      SAVE NSC, SINTAB, COSTAB
      DATA NSC /-1/
C-----------------------------------------------------------------------
      TWOPI = 8.0D0 * ATAN (1.0D0)
      PI = 4.0D0 * ATAN (1.0D0)
C                                       Sine/cosine tables (+/- pi)
      IF (NSC.NE.TABSIZ) THEN
         NSC = TABSIZ
         ICEN = (NSC / 2) + 1
         DELTA = 1.0D0 / (NSC - 1)
         DO 10 LOOP = 1,NSC
            ARG = TWOPI * (LOOP - ICEN) * DELTA
            SINTAB(LOOP) = SIN (ARG)
            COSTAB(LOOP) = COS (ARG)
 10         CONTINUE
         END IF
C                                       Save variables to be changed
      JNVIS = NVIS
      JNFREQ = NFREQ
      VVIS = VIS
      WWT = WT
      WORK = VIS - 2
C                                       Make beam?
      DOBEAM = BEAM.GT.0
C                                       Check if tapered
      TAPER =  (ABS (APCORE(9+PSAPOF)).GT.1.0D-20) .OR.
     *   (ABS (APCORE(10+PSAPOF)).GT.1.0D-20)
C                                       Check shift
      SHIFT = (ABS (APCORE(14+PSAPOF)).GT.1.0D-20) .OR.
     *   (ABS (APCORE(15+PSAPOF)).GT.1.0D-20) .OR.
     *   (ABS (APCORE(15+PSAPOF)).GT.1.0D-20)
      IF (JNVIS.LE.0) GO TO 999
C                                       Central cell
      XCEN = APCORE(17+PSAPOF)
      YCEN = APCORE(18+PSAPOF)
C                                       Frequency looping, rescale uv
      DO 300 IFRQ = 1,JNFREQ
         VALUE1 = APCORE(IFRQ+19+PSAPOF)
         JA = UV + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
         DO 100 LOOP = 1,JNVIS
            APCORE(JA) = (VALUE1 * APCORE(JA)) + APCORE(JA)
            APCORE(JA+1) = (VALUE1 * APCORE(JA+1)) + APCORE(JA+1)
            APCORE(JA+2) = (VALUE1 * APCORE(JA+2)) + APCORE(JA+2)
            JA = JA + L
 100        CONTINUE
C                                       Taper
         IF (TAPER) THEN
            JA = UV + PSAPOF
            JC = WWT + PSAPOF
            VALUE1 = APCORE(9+PSAPOF)
            VALUE2 = APCORE(10+PSAPOF)
      INCLUDE 'INCS:ZVND.INC'
            DO 120 LOOP = 1,JNVIS
               TEXP = VALUE1 * APCORE(JA) * APCORE(JA) +
     *            VALUE2 * APCORE(JA+1) * APCORE(JA+1)
               IF (TEXP.GT.-14.0D0) THEN
                  APCORE(JC) = APCORE(JC) * EXP (TEXP)
               ELSE
                  APCORE(JC) = 0.0D0
                  END IF
               JA = JA + L
               JC = JC + L
 120           CONTINUE
C                                       end of taper
            END IF
C                                       Shift position.
         IF (SHIFT) THEN
            VALUE1 = APCORE(14+PSAPOF) * TWOPI
            VALUE2 = APCORE(15+PSAPOF) * TWOPI
            VALUE3 = APCORE(16+PSAPOF) * TWOPI
            JA = UV + PSAPOF
            IWRK = WORK + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
            DO 150 LOOP = 1,JNVIS
C                                       Set phase to shift
               APCORE(IWRK+1) = APCORE(JA) * VALUE1 +
     *            APCORE(JA+1) * VALUE2 + APCORE(JA+2) * VALUE3
               JA = JA + L
               IWRK = IWRK + L
C
 150           CONTINUE
C                                       Calc Cos and Sin of Phase
            CALL QRECT1 (WORK, L, WORK, L, JNVIS)
            JC = VVIS + PSAPOF
            IWRK = WORK + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
            DO 160 LOOP = 1,JNVIS
C                                       Shift positions
               TEMPR1 = APCORE(IWRK)
               TEMPI1 = APCORE(IWRK+1)
               TEMPR2 = APCORE(JC)
               TEMPI2 = APCORE(JC+1)
               APCORE(JC)   = TEMPR1 * TEMPR2 - TEMPI1 * TEMPI2
               APCORE(JC+1) = TEMPR1 * TEMPI2 + TEMPI1 * TEMPR2
               IWRK = IWRK + L
               JC = JC + L
 160           CONTINUE
C                                       end of shifting
            END IF
C                                       Only need cosine transform
      INCLUDE 'INCS:ZVND.INC'
         DO 250 J = 1,NY
            Y = (J - YCEN) * APCORE(12+PSAPOF)
      INCLUDE 'INCS:ZVND.INC'
            DO 240 I = 1,NX
               X = (I - XCEN) * APCORE(11+PSAPOF)
               Z = - (SQRT (1.0D0 - X*X - Y*Y) - 1.0D0)
               MAPPNT = MAP + (J-1) * NX + I + PSAPOF - 1
               BEMPNT = BEAM + (J-1) * NX + I + PSAPOF - 1
               IWRK = PSAPOF + WORK
               JA = PSAPOF + WWT
               JB = PSAPOF + VVIS
               JC = PSAPOF + UV
               DELTA = NSC - 1
               SUM = 0.0D0
               SUMB = 0.0D0
      INCLUDE 'INCS:ZVND.INC'
               DO 200 K = 1,NVIS
C                                       Phase in turns
                  PHASE = APCORE(JC)*X + APCORE(JC+1)*Y +
     *               APCORE(JC+2)*Z
                  IF (PHASE.GT.0.0D0) THEN
                     ITRUNC = PHASE + 0.5D0
                  ELSE
                     ITRUNC = PHASE - 0.5D0
                     END IF
                  IPHASE = ((PHASE - ITRUNC) + 0.5D0) * DELTA + 1.5D0
                  RE = APCORE(JA) * COSTAB(IPHASE)
                  IM = APCORE(JA) * SINTAB(IPHASE)
C                                       Sums
                  SUM = SUM + APCORE(JB)*RE - APCORE(JB+1)*IM
                  SUMB = SUMB + RE
                  JA = JA + L
                  JB = JB + L
                  JC = JC + L
                  IWRK = IWRK + L
 200           CONTINUE
C                                        Sum image
               APCORE(MAPPNT) = APCORE(MAPPNT) + SUM
C                                        Sum beam
               IF (DOBEAM) APCORE(BEMPNT) = APCORE(BEMPNT) + SUMB
 240           CONTINUE
 250        CONTINUE
C                                        Update pointers for next
C                                        frequency.
         WWT = WWT + INCF
         VVIS = VVIS + INCF
 300     CONTINUE
C
 999  RETURN
      END
