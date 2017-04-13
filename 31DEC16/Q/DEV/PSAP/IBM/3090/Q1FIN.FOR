@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE Q1FIN (U, ROW, MO2, LROW)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Finish gridding a row of uv data.
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
C   Vectorizing compiler version
C   Does various tasks associated with completion of gridding a row.
C   If U is within 1/2 support of 0 the symmetric row is conjugated,
C   flipped and added.  Finally rows are rotated so that zero column
C   (assumed LROW/2+1) goes to the first column.
C   If U=0 the space for the next row down is used.
C   Inputs:
C      U     I  U in cells (non-negative)
C      ROW   I  Base address of Grid row of interest
C      MO2   I  Half the number of rows kept in the AP.
C      LROW  I  Length of row (no. vis)
C   Expects necessary constants in following AP locations:
C           0 = COS(PHASE0)           to shift map center
C           1 = SIN(PHASE0)
C           2 = COS(DELPHR)           for rotating down rows
C           3 = SIN(DELPHR)
C           4 = COS(DELPHC)           for rotating down columns
C           5 = SIN(DELPHC)
C           6 = 1.0
C           7 = 0.0
C-----------------------------------------------------------------------
      INTEGER   U, ROW, MO2, LROW,  ROW2,  SYM, REND, LROW1, LROWT2,
     *   LOOP, JA, JB, JC
      REAL     TEMP
      DOUBLE PRECISION
     *     SPHS, CPHS, SDEL, CDEL
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      LROW1 = LROW - 1
      LROWT2 = LROW + LROW
C                                       If near origin, add symmetric
C                                       points.
      IF (U.GT.MO2) GO TO 300
         ROW2 = ROW + 2
         SYM = ROW - (4 * LROW * U)
C                                       If U=0 move 0 row 1 slot lower
C                                       and work from there.
         IF (U.GT.0) GO TO 200
            SYM = ROW - LROWT2
         JA = ROW
         JC = SYM
CC@DIR IGNORE RECRDEPS(APCORE)
C        DO 100 LOOP = 1,LROWT2
C           APCORE (JC+LOOP) = APCORE (JA+LOOP)
C100        CONTINUE
         CALL SCOPY(LROWT2,APCORE(JA+1),1,APCORE(JC+1),1)
 200     REND = SYM + LROWT2 - 2
         JA = ROW2 + 1
         JB = REND + 1
C@DIR IGNORE RECRDEPS(APCORE)
         DO 150 LOOP = 1,LROW1
            APCORE(JA) = APCORE(JA) + APCORE(JB)
            APCORE(JA+1) = APCORE(JA+1) - APCORE(JB+1)
            JA = JA + 2
            JB = JB - 2
 150        CONTINUE
C                                       End cell
         APCORE(ROW+1) = APCORE(ROW+1) + APCORE(SYM+1)
         APCORE(ROW+2) = APCORE(ROW+2) - APCORE(SYM+2)
C                                       Rotate zero to first col.
 300  SYM = ROW + LROW
      JA = ROW
      JC = SYM
CC@DIR IGNORE RECRDEPS(APCORE)
C     DO 350 LOOP = 1,LROW
C        TEMP = APCORE(JA+LOOP)
C        APCORE(JA+LOOP) = APCORE(JC+LOOP)
C        APCORE(JC+LOOP) = TEMP
C350     CONTINUE
      CALL SSWAP(LROW,APCORE(JA+1),1,APCORE(JC+1),1)
C                                       Rotate map center.
      CALL QPHSRO (ROW, 2, ROW, 2, 0, 2, LROW)
C                                       Update for next row
      CPHS = APCORE(1)
      SPHS = APCORE(2)
      CDEL = APCORE(5)
      SDEL = APCORE(6)
      APCORE(1) = CPHS * CDEL - SPHS * SDEL
      APCORE(2) = CPHS * SDEL + SPHS * CDEL
C
 999  RETURN
      END
