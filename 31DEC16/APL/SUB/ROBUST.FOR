      SUBROUTINE ROBUST (A, LEN, AVG, RMS)
C-----------------------------------------------------------------------
C! Find robust mean and rms of a list of values
C# Service
C-----------------------------------------------------------------------
C;  Copyright (C) 2004, 2011
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
C   Finds the robust average and rms of a data list
C   Inputs:
C      A     R(*)   Data array
C      LEN   I      length of array
C   Output:
C      AVG   R      Average
C      RMS   R      RMS
C-----------------------------------------------------------------------
      INTEGER   LEN
      REAL      A(*), AVG, RMS
C
      DOUBLE PRECISION SUM, SUMS, CUT, FACTOR(11), RR, AA
      INTEGER   I, ITER, J
      DATA FACTOR /8.D0, 7.D0, 6.D0, 5.D0, 4.5D0, 4.D0, 4.D0, 3.5D0,
     *   3*3.D0/
C-----------------------------------------------------------------------
      RR = 1.D12
      CUT = RR
      AA = 0.D0
      DO 50 ITER = 1,11
         SUM = 0.D0
         SUMS = 0.D0
         J = 0
         DO 10 I = 1,LEN
            IF (ABS(A(I)-AA).LT.CUT) THEN
               SUM = SUM + A(I)
               SUMS = SUMS + A(I) * A(I)
               J = J + 1
               END IF
 10         CONTINUE
         IF (J.GT.0) THEN
            SUM = SUM / J
            SUMS = SUMS / J - SUM * SUM
            AA = SUM
            RR = SQRT (MAX (0.0D0, SUMS))
         ELSE
            RR = 1.D10
            END IF
         CUT = FACTOR(ITER) * RR
 50      CONTINUE
C                                       return REAL*4
      AVG = AA
      RMS = RR
C
 999  RETURN
      END
