      SUBROUTINE QGRDFI (U, ROW, M, LROW, TYPE)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Finish griding a row of uv data.
C# AP-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006-2007
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
C   Pseudo-AP version
C   QGRDFI Does various tasks assiciated with completion of gridding
C   a row.  If U is within 1/2 support of 0 the symmetric row is
C   conjugated, flipped and added.  Next
C   any taper is applied followed (for IPOL maps only) by the
C   addition of the weighting function to the gridded visibilities
C   to produce the beam map.  Finally rows are rotated so that
C   zero column (assumed LROW/2+1) goes to the first column.
C   If U=0 the space for the next row down is used.
C   Inputs:
C      U      I  U in cells (non-negative)
C      ROW    I  Base address of Grid row of interest
C      M      I  Number of rows kept in the AP.
C      LROW   I  Length of row (no. reals)
C      TYPE   I  1 for IPOL,IBEM maps
C                2 for Q, U maps
C                3 for V maps.
C   Also expects necessary constants in following AP locations:
C           0 = COS(PHASE0)           to shift map center
C           1 = SIN(PHASE0)
C           2 = COS(DELPHR)           for rotating down rows
C           3 = SIN(DELPHR)
C           4 = COS(DELPHC)           for rotationg down columns
C           5 = SIN(DELPHC)
C           6 = 1.0
C           7 = 0.0
C-----------------------------------------------------------------------
      INTEGER   U, ROW, M, LROW, TYPE
C
      INTEGER   STCNT, STWT, MO2, SYM, REND, LROW1, STCNT2, STWT2, JROW,
     *   ROW1, LROWT4, LROWT2
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                       Set pointers etc.
      JROW = ROW
      LROWT2 = LROW + LROW
      LROWT4 = LROWT2 + LROWT2
      STCNT = JROW + LROWT2
      STWT = STCNT + 1
      STCNT2 = STCNT + 2
      STWT2 = STWT + 2
      MO2 = M / 2
      LROW1 = LROW - 1
      ROW1 = JROW + 2
C                                        Check if Q,U maps
      IF (TYPE.NE.2) THEN
C                                        Check if IMAP and near orig.
         IF (U.LE.MO2) THEN
C                                        Near orig. add sym pts.
            SYM = JROW - 4 * LROWT2 * U
            REND = SYM + LROWT2
C                                        If u=0 move 0 row 1 slot
C                                        lower and work from there.
            IF (U.LE.0) THEN
               SYM = JROW - LROWT4
               REND = SYM + LROWT2
               LROW1 = LROWT2
               CALL QVMOV (JROW, 1, SYM, 1, LROW1)
               CALL QVMOV (STCNT, 1, REND, 1, LROW1)
               LROW1 = LROW - 1
               END IF
C                                        Add rows
            REND = REND - 2
            CALL QCVJAD (ROW1, 2, REND, -2, ROW1, 2, LROW1)
            CALL QCVJAD (JROW, 2, SYM, 2, JROW, 2, 1)
            REND = REND + LROWT2
C                                         Add counts.
            CALL QVADD (REND, -2, STCNT2, 2, STCNT2, 2, LROW1)
            SYM = SYM + LROWT2
            CALL QVADD (SYM, 2, STCNT, 2, STCNT, 2, 1)
C                                        Add weights
            REND = REND + 1
            CALL QVADD (REND, -2, STWT2, 2, STWT2, 2, LROW1)
            SYM = SYM + 1
            CALL QVADD (SYM, 2, STWT, 2, STWT, 2, 1)
            END IF
         ROW1 = JROW + 1
C                                        Form conjugate row for IBEM
C                                        maps. Add weights to imag.
C                                        Move weights to STCNT
         CALL QVMOV (STWT, 2, STCNT, 2, LROW)
C                                        Sub. imag. from conj. wt.
         CALL QVSUB (ROW1, 2, STCNT, 2, STCNT, 2, LROW)
C                                         Add weight to imag.
         CALL QVADD (STWT, 2, ROW1, 2, ROW1, 2, LROW)
C                                        Move conj imag to correct loc
         CALL QVMOV (STCNT, 2, STWT, 2, LROW)
C                                       Copy real to conj. row
         CALL QVMOV (JROW, 2, STCNT, 2, LROW)
C                                       Rotate zero to first col
         SYM = STCNT + LROW
         CALL QVSWAP (STCNT, 1, SYM, 1, LROW)
         SYM = JROW + LROW
         CALL QVSWAP (JROW, 1, SYM, 1, LROW)
C                                       Rotate map center.
         CALL QPHSRO (JROW, 2, JROW, 2, 0, 2, LROW)
C                                       Conj. phase rotation
         APCORE(1+PSAPOF) = -APCORE(1+PSAPOF)
         APCORE(3+PSAPOF) = -APCORE(3+PSAPOF)
C                                       Rotate conjugate row
         CALL QPHSRO (STCNT, 2, STCNT, 2, 0, 2, LROW)
C                                       Reverse conj. row -real
         CALL QVRVRS (STCNT2, 2, LROW1)
C                                       Imag.
         CALL QVRVRS (STWT2, 2, LROW1)
C                                       Undo conj of phase.
         APCORE(1+PSAPOF) = -APCORE(1+PSAPOF)
         APCORE(4+PSAPOF) = -APCORE(3+PSAPOF)
C                                       Rotate phase0 for next row
         CALL QPHSRO (0, 2, 0, 2, 4, 6, 1)
C                                       Q and U map.
C                                       Use ROW for QPOL
C                                       Use STCNT for UPOL
      ELSE
C                                       Rotate zero to first col.
         SYM = STCNT + LROW
C                                       Switch UPOL.
         CALL QVSWAP (STCNT, 1, SYM, 1, LROW)
         SYM = JROW + LROW
C                                       Switch QPOL
         CALL QVSWAP (JROW, 1, SYM, 1, LROW)
C                                       Rotate map center, QPOL
         CALL QPHSRO (JROW, 2, JROW, 2, 0, 2, LROW)
C                                       Negate phase.
         APCORE(1+PSAPOF) = -APCORE(1+PSAPOF)
         APCORE(3+PSAPOF) = -APCORE(3+PSAPOF)
C                                       Rotate UPOL
         CALL QPHSRO (STCNT, 2, STCNT, 2, 0, 2, LROW)
C                                       Unnegate phase.
         APCORE(1+PSAPOF) = -APCORE(1+PSAPOF)
         APCORE(3+PSAPOF) = -APCORE(3+PSAPOF)
C                                       Rotate phase0 for next row.
         CALL QPHSRO (0, 2, 0, 2, 4, 6, 1)
C                                       Flip VIS2 row, real
         CALL QVRVRS (STCNT2, 2, LROW1)
         STCNT2 = STCNT2 + 1
C                                       Flip imag.
         CALL QVRVRS (STCNT2, 2, LROW1)
         END IF
C
 999  RETURN
      END
