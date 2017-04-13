      SUBROUTINE APSUM ( USTRT, USTOP, UZERO, ROWSIZ, LOGRID, APSMWT,
     *   LROW, SUMWT)
C-----------------------------------------------------------------------
C! AP routine: Load AP sum weights
C# AP-util UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999
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
C AP Sum Weights: Sum the real component of the gridded uv data.
C For the Beam imaginary comp is zero and real is 1.0 * Weight.
C   Inputs:
C      USTRT   I    Start U row to write
C      USTOP   I    Stop  U row to write
C      UZERO   I    Row number of minimum row in AP to be processed
C                   (Often Location of row U=0 in AP)
C      ROWSIZ  I    Length of a Row in AP, number of reals
C      LOGRID  I    Index of start of rows in AP (0 relative)
C      APSMWT  I    Index to AP location to use for summing weights
C      LROW    I    Number of Cells in Row
C   Output:
C      SUMWT   R    Sum of Weights
C-----------------------------------------------------------------------
      INTEGER   USTRT, USTOP, UZERO, ROWSIZ, LOGRID, APSMWT, LROW
      REAL      SUMWT
C
      INTEGER IU, U, EROW
      REAL    APXTMP
C-----------------------------------------------------------------------
C                                       init sum
      SUMWT = 0
C                                       for all u to sum
      DO 100 IU = USTRT,USTOP
C                                       Convert to decreasing U
         U = USTOP - IU + USTRT
C                                       calc start of row for U
         IF (U.GE.UZERO) THEN
            EROW = LOGRID + ((U-UZERO)*ROWSIZ)
            CALL QWAIT
C                                       Sum Vector Elements (SVE)
            CALL QSVE (EROW, 2, APSMWT, LROW)
C                                       Read sum of weights
            CALL QWR
C                                       Get weigth out of AP
            CALL QGET (APXTMP, APSMWT, 1, 2)
            CALL QWD
C                                       accumulate sum
            SUMWT = SUMWT + APXTMP
            END IF
 100     CONTINUE
C
 999  RETURN
      END
