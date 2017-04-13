      SUBROUTINE Q1FIN (U, ROW, MO2, LROW)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Finish gridding a row of uv data.
C# AP-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2006, 2008, 2012
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
C           4 = COS(DELPHC)           for rotating down columns
C           5 = SIN(DELPHC)
C   Old comments say the following, but they are NOT used:
C           2 = COS(DELPHR)           for rotating down rows
C           3 = SIN(DELPHR)
C           6 = 1.0
C           7 = 0.0
C   6,7 appropriated for use in QNGRD
C-----------------------------------------------------------------------
      INTEGER   U, ROW, MO2, LROW
C
      INTEGER   ROW2,  SYM, REND, LROW1, LROWT2, LOOP
      LONGINT   JA, JB, JC
      DOUBLE PRECISION TEMP, SPHS, CPHS, SDEL, CDEL
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      LROW1 = LROW - 1
      LROWT2 = LROW + LROW
C                                       If near origin, add symmetric
C                                       points.
      IF (U.LE.MO2) THEN
         ROW2 = ROW + 2
         SYM = ROW - (4 * LROW * U)
         IF (SYM.GE.APCORI(PSAPOF+19)) THEN
C                                       If U=0 move 0 row 1 slot lower
C                                       and work from there.
            IF (U.EQ.0) THEN
               SYM = ROW - LROWT2
               JA = ROW + PSAPOF - 1
               JC = SYM + PSAPOF - 1
      INCLUDE 'INCS:ZVND.INC'
               DO 20 LOOP = 1,LROWT2
                  APCORE (JC+LOOP) = APCORE (JA+LOOP)
 20               CONTINUE
               END IF
            REND = SYM + LROWT2 - 2
            JA = ROW2 + PSAPOF
            JB = REND + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
            DO 30 LOOP = 1,LROW1
               APCORE(JA) = APCORE(JA) + APCORE(JB)
               APCORE(JA+1) = APCORE(JA+1) - APCORE(JB+1)
               JA = JA + 2
               JB = JB - 2
 30            CONTINUE
C                                       End cell
            APCORE(ROW+PSAPOF) = APCORE(ROW+PSAPOF) + APCORE(SYM+PSAPOF)
            APCORE(ROW+PSAPOF+1) = APCORE(ROW+PSAPOF+1) -
     *         APCORE(SYM+PSAPOF+1)
            END IF
         END IF
C                                       Rotate zero to first col.
      SYM = ROW + LROW
      JA = ROW + PSAPOF - 1
      JC = SYM + PSAPOF - 1
      INCLUDE 'INCS:ZVND.INC'
      DO 100 LOOP = 1,LROW
         TEMP = APCORE(JA+LOOP)
         APCORE(JA+LOOP) = APCORE(JC+LOOP)
         APCORE(JC+LOOP) = TEMP
 100     CONTINUE
C                                       Rotate map center.
      CALL QPHSRO (ROW, 2, ROW, 2, 0, 2, LROW)
C                                       Update for next row
      CPHS = APCORE(PSAPOF)
      SPHS = APCORE(PSAPOF+1)
      CDEL = APCORE(PSAPOF+4)
      SDEL = APCORE(PSAPOF+5)
      APCORE(PSAPOF) = CPHS * CDEL - SPHS * SDEL
      APCORE(PSAPOF+1) = CPHS * SDEL + SPHS * CDEL
C
 999  RETURN
      END
