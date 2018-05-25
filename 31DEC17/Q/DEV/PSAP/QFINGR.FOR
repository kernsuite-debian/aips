      SUBROUTINE QFINGR (U, ROW, MO2, LROW, TYPE)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Finish gridding row of uv data.
C# AP-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2012
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
C   Vectorizing compiler version (also OK for Scalar).
C   FINGRD Does various tasks associated with completion of gridding
C   a row.  If U is within 1/2 support of 0 the symmetric row is
C   conjugated, flipped and added.  Next two conjugate rows are
C   formed, vis1 + i*vis2 (for beams, vis2 is the sampling function=
C   the sum of the weights).  Finally rows are rotated so that
C   zero column (assumed LROW/2+1) goes to the first column.
C   If U=0 the space for the next row down is used.
C   Inputs:
C      U     I  U in cells (non-negative)
C      ROW   I  Base address of Grid row of interest
C      MO2   I  Half the number of rows kept in the AP.
C      LROW  I  Length of row (no. vis)
C      TYPE  I  1 for IPOL,IBEM maps
C               2 for Q,U maps
C               3 for V maps.
C               4 for two line maps.
C   Also expects necessary constants in following AP locations:
C           0 = COS(PHASE0)           to shift map center
C           1 = SIN(PHASE0)
C           2 = COS(DELPHR)           for rotating down rows
C           3 = SIN(DELPHR)
C           4 = COS(DELPHC)           for rotating down columns
C           5 = SIN(DELPHC)
C           6 = 1.0
C           7 = 0.0
C-----------------------------------------------------------------------
      INTEGER   U, ROW, MO2, LROW, TYPE
C
      INTEGER   ROW2, CROW, CRWI, SYM, REND, LROW1, CROW2, LL, LROWT4,
     *   LROWT2, LOOP, NO2
      LONGINT   JA, JB, JC, JD
      DOUBLE PRECISION DAR, DAI, CPHS, SPHS, AR, BR
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      LROWT2 = LROW + LROW
      LROWT4 = LROWT2 + LROWT2
      LL = LROW
      CROW = ROW + LROWT2
      CROW2 = CROW + 2
      LROW1 = LROW - 1
C                                       For VPOL zero weights
      IF (TYPE.NE.3) GO TO 100
         CRWI = CROW + 1
         CALL QVMOV (CRWI, 2, CROW, 2, LROW)
C                                       Check if near origin.
 100  IF (U.GT.MO2) GO TO 300
C                                       Near origin, add symmetric
C                                       points.
         ROW2 = ROW + 2
         SYM = ROW - (LROWT4 * (U + U))
C                                       If U=0 move 0 row 1 slot lower
C                                       and work from there.
         IF (U.GT.0) GO TO 200
            SYM = ROW - LROWT4
            JA = ROW + PSAPOF
            JC = SYM + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
            DO 140 LOOP = 1,LROWT4
               APCORE(JC) = APCORE(JA)
               JA = JA + 1
               JC = JC + 1
140           CONTINUE
C           CALL QVMOV (ROW, 1, SYM, 1, LROWT4)
 200     REND = SYM + LROWT2 - 2
C                                       First visibility.
         JA = ROW2 + PSAPOF
         JC = REND + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
         DO 150 LOOP = 1,LROW1
            APCORE(JA) = APCORE(JA) + APCORE(JC)
            APCORE(JA+1) = APCORE(JA+1) - APCORE(JC+1)
            JA = JA + 2
            JC = JC - 2
 150        CONTINUE
C                                       End cell
         APCORE(ROW+PSAPOF) = APCORE(ROW+PSAPOF) + APCORE(SYM+PSAPOF)
         APCORE(ROW+1+PSAPOF) = APCORE(ROW+1+PSAPOF) -
     *       APCORE(SYM+1+PSAPOF)
C         CALL QCVJAD (ROW2, 2, REND, -2, ROW2, 2, LROW1)
C         CALL QCVJAD (ROW, 2, SYM, 2, ROW, 2, 1)
C                                       Second visibility
         REND = REND + LROWT2
         SYM = SYM + LROWT2
         JA = CROW2 + PSAPOF
         JC = REND + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
         DO 160 LOOP = 1,LROW1
            APCORE(JA) = APCORE(JA) + APCORE(JC)
            APCORE(JA+1) = APCORE(JA+1) - APCORE(JC+1)
            JA = JA + 2
            JC = JC - 2
 160        CONTINUE
C                                       End cell
         APCORE(CROW+PSAPOF) = APCORE(CROW+PSAPOF) + APCORE(SYM+PSAPOF)
         APCORE(CROW+1+PSAPOF) = APCORE(CROW+1+PSAPOF) -
     *      APCORE(SYM+PSAPOF+1)
C         CALL QCVJAD (CROW2, 2, REND, -2, CROW2, 2, LROW1)
C         CALL QCVJAD (CROW, 2, SYM, 2, CROW, 2, 1)
C                                       Form conjugate rows.
 300  CALL QGRDMI (ROW, CROW, LROW)
C                                       Rotate zero to first column.
      SYM = CROW + LL
      JA = CROW + PSAPOF - 1
      JB = ROW + PSAPOF - 1
      JC = SYM + PSAPOF - 1
      JD = ROW + LL + PSAPOF - 1
      INCLUDE 'INCS:ZVND.INC'
      DO 350 LOOP = 1,LROW
         AR = APCORE(JA+LOOP)
         BR = APCORE(JB+LOOP)
         APCORE(JA+LOOP) = APCORE(JC+LOOP)
         APCORE(JB+LOOP) = APCORE(JD+LOOP)
         APCORE(JC+LOOP) = AR
         APCORE(JD+LOOP) = BR
 350     CONTINUE
C      CALL QVSWAP (CROW, 1, SYM, 1, LROW)
      SYM = ROW + LL
C      CALL QVSWAP (ROW, 1, SYM, 1, LROW)
C                                       Rotate map center.
      CALL QPHSRO (ROW, 2, ROW, 2, 0, 2, LROW)
      APCORE(1+PSAPOF) = -APCORE(1+PSAPOF)
      APCORE(3+PSAPOF) = -APCORE(3+PSAPOF)
C      CALL QVNEG (1, 2, 1, 2, 2)
      CALL QPHSRO (CROW, 2, CROW, 2, 0, 2, LROW)
C                                       Reverse Conjugate row.
      CRWI = CROW + 3
C      CALL QVRVRS (CROW2, 2, LROW1)
C      CALL QVRVRS (CRWI, 2, LROW1)
      JC = CROW2 + PSAPOF
      JD = CRWI + PSAPOF
      JA = JC + (LROW1 - 1) * 2
      JB = JD + (LROW1 - 1) * 2
      NO2 = LROW1 / 2
      INCLUDE 'INCS:ZVND.INC'
      DO 360 LOOP = 1,NO2
         AR = APCORE(JC)
         BR = APCORE(JD)
         APCORE(JC) = APCORE(JA)
         APCORE(JD) = APCORE(JB)
         APCORE(JA) = AR
         APCORE(JB) = BR
         JA = JA - 2
         JB = JB - 2
         JC = JC + 2
         JD = JD + 2
 360     CONTINUE
C                                       Update for next row
      APCORE(1+PSAPOF) = -APCORE(1+PSAPOF)
      APCORE(3+PSAPOF) = -APCORE(3+PSAPOF)
C      CALL QVNEG (1, 2, 1, 2, 2)
      DAR = APCORE(PSAPOF)
      DAI = APCORE(1+PSAPOF)
      CPHS = APCORE(4+PSAPOF)
      SPHS = APCORE(5+PSAPOF)
      APCORE(PSAPOF) = DAR * CPHS - DAI * SPHS
      APCORE(1+PSAPOF) = DAI * CPHS + DAR * SPHS
C      CALL QPHSRO (0, 2, 0, 2, 4, 6, 1)
C
 999  RETURN
      END
