      SUBROUTINE GET1VS (MODE, MVIS, JADR, JINC, SFACT, ALLWT, STOKES,
     *   DATA, WT, VIS, IRET)
C-----------------------------------------------------------------------
C! Extract desired uv data, 1 value per freq. channel.
C# UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003
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
C   GET1VS gets and reformats uv data. Returns one stokes' type per
C   frequency channel.  Requires setup by SET1VS.
C   Inputs:
C      MODE          I    Operation number (see SET1VS).
C                         When MODE = 3 and RL and LR are given,
C                         the U visibility is multiplied by i.
C      MVIS          I    Number of visibilities wanted.
C      JADR(2)       I    Pointers set by SET1VS.
C      JINC          I    Increment between vis.
C      SFACT(2)      R    Factors set by SET1VS.
C      ALLWT         L    If true all vis are required.
C      STOKES        L    True if input data true Stokes'.
C                         Used for UPOL only.
C      DATA(3,*)     R    Visibility portion of input data.
C   Outputs:
C      WT            R    Average weight.
C      VIS(MVIS)     CMPX Visibilities.
C      IRET          I    Error code, 0=>OK,
C                                  1 => bad weights.(data flagged).
C                                  2 = bad input.
C-----------------------------------------------------------------------
      INTEGER   MODE, MVIS, JADR(2), JINC, IRET
      LOGICAL   ALLWT, STOKES
      REAL      SFACT(2), DATA(3,*), WT
      COMPLEX   VIS(*)
C
      INTEGER   I, IP1, IP2, KINC
      REAL      SUM, AWT, WT1, WT2, TMPRI(2)
      COMPLEX   CMPLX, TMP
      EQUIVALENCE (TMP, TMPRI)
C-----------------------------------------------------------------------
      WT = 0.0
      VIS(1) = CMPLX (0.0, 0.0)
C                                       Check input.
      IRET = 2
      IF (MVIS.LE.0) GO TO 999
      IRET = 1
      SUM = 0.0
      KINC = -JINC
C                                       Loop checking and summing
C                                       weights and getting
C                                       visibilities.
      DO 50 I = 1,MVIS
C                                       Set pointers.
         KINC = KINC + JINC
         IP1 = JADR(1) + KINC
         IP2 = JADR(2) + KINC
C                                       Get weights.
         WT1 = MAX (0.0, DATA(3,IP1))
         WT2 = MAX (0.0, DATA(3,IP2))
         AWT = WT1 + WT2
C                                       Check weights.
         IF (ALLWT .AND. ((WT1.LE.0.0) .OR. (WT2.LE.0.0)))
     *      GO TO 999
         IF (AWT.LE.0.0) GO TO 999
C                                       Set visibility.
         VIS(I) = CMPLX (0.0, 0.0)
         IF (WT1.GT.0.0) VIS(I) = VIS (I) +
     *      SFACT(1) * CMPLX (DATA(1,IP1), DATA(2,IP1))
         IF (WT2.GT.0.0)  VIS(I) = VIS(I) +
     *      SFACT(2) * CMPLX (DATA(1,IP2), DATA(2,IP2))
C                                       If one wt .le.0 (should
C                                       be one vis only) double VIS.
         IF ((WT1.LE.0.0) .OR. (WT2.LE.0.0)) THEN
            VIS(I) = 2.0 * VIS(I)
         ELSE IF (IP1.NE.IP2) THEN
            AWT = 4.0 * WT1 * WT2 / (WT1 + WT2)
         ELSE
            AWT = AWT / 2.0
            END IF
         SUM = SUM + AWT
C                                       If using Upol multiply by i
         IF ((.NOT.STOKES) .AND. (MODE.EQ.3)) THEN
            TMP = VIS(I)
            VIS(I) = CMPLX (-TMPRI(2), TMPRI(1))
            END IF
 50      CONTINUE
C                                       average weight
      WT = SUM / MVIS
      IRET = 0
C
 999  RETURN
      END
