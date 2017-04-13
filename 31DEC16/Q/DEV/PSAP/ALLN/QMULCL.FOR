      SUBROUTINE QMULCL (COMP, FLDECS, ABEAM, BBEAM, NCOMP)
C-----------------------------------------------------------------------
C! Alliant Pseudo AP routine: High level Clark CLEAN routine
C# AP-appl
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
C   Calls B. Clark's QCLNSU routine multiple times to accumulate a
C   number of CLEAN components in one AP call.  This version copies the
C   residuals and the beam into work arrays in order to vectorize the
C   CLEANing.
C   Inputs:
C      COMP   I  Base address of component vector 4 words per
C                component:
C                   1 = component intensity.
C                   2 = x location (cells)
C                   3 = y location (cells)
C                   4 = field number
C                NOTE: this vector is the output of this routine.
C      FLDECS I  Field descriptor vector base address.
C                0 = Number of fields (REAL value)
C                 for each field:
C                0 = Start address of residuals (INTEGER value) (AMAP)
C                   (location,intensity), intensity=2**14*x + y
C                   NOTE INTEGER FORMAT FOR THIS WORD ONLY
C                1 = Number of points in the map. (REAL value) (LMAP)
C                2 = Current max. flux density (loc = FCOMP)
C                3 = X position of current max.
C                4 = Y position of current max.
C                5 = CLEAN gain
C      ABEAM  I  Base address of the square (folded) beam patch.
C                The Y direction is most rapid variable:-BY<Y<BY,
C                X varies less rapidly, -1<X<BX
C      BBEAM  I  Beam description vector (BX,BY).
C                In the current QCLNSU BX must equal BY.
C      NCOMP  I  Number of components to CLEAN.
C   Usage notes:
C     The following common work vectors are used:
C        IWVEC1   = KYF   => array of first residual in beam patch
C        IWVEC2   = KYL   => array of last residual in beam patch
C        IWVEC3   = IX    => X pixel location of residuals
C        IWVEC4   = IY    => Y pixel location of residuals
C        WKVEC5   = FLUX  => Residual flux density
C        WKVEC6,7 = BEAM  => BEAM patch.
C   Alliant version (similar to Cray version).
C-----------------------------------------------------------------------
      INTEGER   MAXFLD
      PARAMETER (MAXFLD=16)
C
      INTEGER   COMP, FLDECS, ABEAM, BBEAM, NCOMP, NFIELD, JFDV, ILOOP,
     *   JLMAP, JCOMP, I, IFIELD, JFDVT, JLOOP, JFIELD, ADDR, ITEMP,
     *   LENT, APOFF, NP(MAXFLD), LF(MAXFLD), INDRES(MAXFLD), IBX, IBY,
     *   JA, JB, JX, JY, MAXY, ISAMAX, LOOP, IBY21, LIMIT, LIMIT1,
     *   LIMIT2, IXMXMN, MAX, INDEX, JNDEX
      REAL      AXMAX
      INCLUDE 'INCS:DAPC.INC'
      INTEGER   KYF(APSIZE/2+1), KYL(APSIZE/2+1), IX(APSIZE/2+1),
     *   IY(APSIZE/2+1)
      REAL      FLUX(APSIZE/2+1), BEAM(APSIZE/2+1)
      EQUIVALENCE (KYF, IWVEC1), (KYL, IWVEC2), (IX, IWVEC3),
     *   (IY, IWVEC4), (FLUX, WKVEC5), (BEAM, WKVEC6)
       DATA LENT /3/
C-----------------------------------------------------------------------
C                                       Get Beam Patch size
      IBX = APCORE(BBEAM+1)
      IBY = APCORE(BBEAM+2)
      IBY21 = IBY * 2 - 1
C                                       Addresses to 1-rel etc.
      JFDV = FLDECS + 1
      JCOMP = COMP + 1
      NFIELD = APCORE(JFDV) + 0.3
C                                       Loop over fields rewriting
      DO 100 IFIELD = 1,NFIELD
         JNDEX = JFDV + (IFIELD-1)*6 + 1
         APOFF = APCORI(JNDEX)
         NP(IFIELD) = 1
         IF(IFIELD.GT.1) NP(IFIELD) = NP(IFIELD-1) + LF(IFIELD-1)
         LF(IFIELD) = APCORE(JNDEX+1) + 0.3
         JLMAP = LF(IFIELD)
C                                       Get X, Y, FLUX
         JY = APOFF + LENT
         JX = APOFF + 1
         JA = NP(IFIELD) - 1
      INCLUDE 'INCS:ZVND.INC'
         DO 10 ILOOP = 1,JLMAP
            IX(JA+ILOOP) = APCORE(JX) + 0.3
            IY(JA+ILOOP) = APCORE(JX+1) + 0.3
            FLUX(JA+ILOOP) = APCORE(JY)
            JX = JX + LENT
            JY = JY + LENT
 10         CONTINUE
C                                       Get max. Y
         INDEX = NP(IFIELD) + JLMAP - 1
         MAXY = IY(INDEX)
C                                        Clear IWVEC6
         DO 20 ILOOP = 1,MAXY
            IWVEC6(ILOOP) = 0
 20         CONTINUE
C                                        Fill next IBY entries
C                                        with JLMAP+1
         LIMIT1 = MAXY
         LIMIT2 = LIMIT1 + IBY + 1
         DO 30 ILOOP = LIMIT1,LIMIT2
            IWVEC6(ILOOP) = JLMAP + 1
 30         CONTINUE
C                                        Find first occurence of Y
C                                        Use SCATTER
         JY = JLMAP
         DO 40 ILOOP = 1,JLMAP
            ADDR = IY(JY+JA)
            IWVEC6(ADDR) = JY
            JY = JY - 1
 40         CONTINUE
C                                        Fill zero entries in IWVEC6.
C                                        Contains recursion - DON'T
C                                        Vectorize!!
         JY = MAXY - 1
         DO 50 ILOOP = 2,MAXY
            IF (IWVEC6(JY).EQ.0) IWVEC6(JY) = IWVEC6(JY+1)
            JY = JY - 1
 50         CONTINUE
C                                        Set first resid in window and
C                                        set last resid in Beam patch
C                                        (Find first occurance of next)
         JA = NP(IFIELD) - 1
      INCLUDE 'INCS:ZVND.INC'
         DO 60 ILOOP = 1,JLMAP
            ITEMP = IY(JA+ILOOP) - IBY + 1
            IXMXMN = MAX (1, ITEMP)
            KYF(JA+ILOOP) = IWVEC6(IXMXMN)
            ITEMP = IY(JA+ILOOP) + IBY
            KYL(JA+ILOOP) = IWVEC6(ITEMP) - 1
            KYL(JA+ILOOP) = MIN (KYL(JA+ILOOP), JLMAP)
 60         CONTINUE
 100     CONTINUE
C                                        Form full beam, X .GE. 0
      JA = (IBX-1) * IBY21
      JB = ABEAM
      LIMIT = IBX * IBY21
      INCLUDE 'INCS:ZVND.INC'
      DO 110 ILOOP = 1,LIMIT
         BEAM(JA+ILOOP) = APCORE(JB+ILOOP)
 110     CONTINUE
C                                        X .LT. half of beam
      LIMIT1 = (IBX-1) * IBY21
      JA = ABEAM + IBX * IBY21
      INCLUDE 'INCS:ZVND.INC'
      DO 120 LOOP = 1,LIMIT1
         BEAM(LOOP) = APCORE(JA)
         JA = JA - 1
 120     CONTINUE
C                                       Find maxima in fields
      DO 300 IFIELD = 1,NFIELD
C                                        Use ISAMAX to find max. abs.
C                                        FLUX.
         JNDEX = JFDV + (IFIELD-1) * 6 + 1
         JLMAP = LF(IFIELD)
         JFDVT = JNDEX + 2
         INDEX = NP(IFIELD)
         INDRES(IFIELD) = ISAMAX (JLMAP, FLUX(INDEX), 1)
C                                        Save location
         ITEMP = INDRES(IFIELD) + INDEX - 1
         APCORE(JFDVT) = FLUX(ITEMP)
         APCORE(JFDVT+1) = IX(ITEMP)
         APCORE(JFDVT+2) = IY(ITEMP)
 300     CONTINUE
C                                       Loop
      DO 600 JLOOP = 1,NCOMP
C                                       Find field with max.
         AXMAX = -10.0
         IFIELD = 1
         JFDVT = JFDV + 3
         JFIELD = JFDVT
         DO 420 I = 1,NFIELD
            IF (ABS (APCORE(JFDVT)).LE.AXMAX) GO TO 410
C                                       New max
               AXMAX = ABS (APCORE(JFDVT))
               IFIELD = I
               JFIELD = JFDVT
 410           CONTINUE
            JFDVT = JFDVT + 6
 420        CONTINUE
C                                       Copy result to output vector
         APCORE(JCOMP) = APCORE(JFIELD)
         APCORE(JCOMP+1) = APCORE(JFIELD+1)
         APCORE(JCOMP+2) = APCORE(JFIELD+2)
         APCORE(JCOMP+3) = IFIELD
C                                       Prepare to call QCLNSU
         JNDEX = NP(IFIELD) - 1
         INDEX = INDRES(IFIELD) + JNDEX
C                                       Subtract / find max.
        CALL QCLNSU (APCORE(JFIELD), LF(IFIELD), KYF(INDEX),
     *      KYL(INDEX), IBX, IBY, JNDEX, INDRES(IFIELD))
C                                       Update JCOMP
         JCOMP = JCOMP + 4
 600     CONTINUE
C                                       Restore residuals to APCORE
C                                       Get total no. residuals.
      LIMIT = 0
      DO 700 ILOOP = 1,NFIELD
         LIMIT = LIMIT + LF(ILOOP)
 700     CONTINUE
      JA = APCORI(JFDV+1) + LENT
      INCLUDE 'INCS:ZVND.INC'
      DO 750 ILOOP = 1,LIMIT
         APCORE(JA) = FLUX(ILOOP)
         JA = JA + LENT
 750     CONTINUE
C
 999  RETURN
      END
