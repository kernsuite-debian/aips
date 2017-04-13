      SUBROUTINE GETVIS (MODE, MVIS, JADR, SFACT, ALLWT, DATA, WT,
     *   VIS, IERR)
C-----------------------------------------------------------------------
C! uses setup from SETVIS to get and reformat a visibility sample
C# UV-util
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
C   GETVIS gets and reformats uv data. Requires setup by SETVIS.
C   Inputs:
C      MODE          I    Operation number (see SETVIS).
C                         When MODE = 2 or 3 and RL and LR are given
C                         the U visibility is multiplied by i.
C      MVIS          I    Number of visibilities wanted.
C      JADR(2,MVIS)  I    Pointers set by SETVIS.
C      SFACT(2,MVIS) R    Factors set by SETVIS.
C      ALLWT         L    Flag set by SETVIS, if .TRUE. all relevant
C                         weights must be positive.
C      DATA(3,*)     R    Visibility portion of input data.
C   Outputs:
C      WT            R    Average weight.
C      VIS(MVIS)     CMPX Visibilities.
C      IERR          I    Error code, 0=>OK,
C                                  1 => bad weights.(data flagged).
C                                  2 = bad input.
C-----------------------------------------------------------------------
      INTEGER   MODE, MVIS, JADR(2,*), IERR
      LOGICAL   ALLWT
      REAL      SFACT(2,*), DATA(3,*), WT
      COMPLEX   VIS(*)
C
      INTEGER   I, IP1, IP2
      REAL      SUM, AWT, WT1, WT2, TMPRI(2), XFACT1, XFACT2
      COMPLEX   CMPLX, TMP
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (TMP, TMPRI)
C-----------------------------------------------------------------------
      WT = 0.0
      VIS(1) = CMPLX (0.0, 0.0)
C                                       Check input.
      IERR = 2
      IF (MVIS.LE.0) GO TO 999
      IERR = 1
      SUM = 0.0
C                                       Loop checking
C                                       and summing weights
C                                       and getting visibilities.
      DO 50 I = 1,MVIS
C                                       Set pointers.
         IP1 = JADR(1,I)
         IP2 = JADR(2,I)
C                                       Get weights.
         WT1 = MAX (0.0, DATA(3,IP1))
         WT2 = MAX (0.0, DATA(3,IP2))
         AWT = WT1 + WT2
C                                       Check weights.
         IF ((ALLWT) .AND. ((WT1.LE.0.0) .OR. (WT2.LE.0.0)))
     *      GO TO 999
C                                       Set visibility.
         XFACT1 = 0.0
         XFACT2 = 0.0
         IF (WT1.GT.0.0) XFACT1 = SFACT(1,I)
         IF (WT2.GT.0.0) XFACT2 = SFACT(2,I)
         VIS(I) = XFACT1 * CMPLX (DATA(1,IP1), DATA(2,IP1)) +
     *      XFACT2 * CMPLX (DATA(1,IP2), DATA(2,IP2))
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
 50      CONTINUE
C                                       Check summed weight.
      IF (SUM.LE.0.0) GO TO 999
      WT = SUM / MVIS
      IERR = 0
C                                       If using Upol multiply by i
      IF ((ICOR0.GT.0) .OR. ((MODE.NE.2).AND.(MODE.NE.3))) GO TO 999
         TMP = VIS(3)
         VIS(3) = CMPLX (-TMPRI(2), TMPRI(1))
C
 999  RETURN
      END
