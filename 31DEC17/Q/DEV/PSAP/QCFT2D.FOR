      SUBROUTINE QCFT2D (SIGN, LEN1, LEN2, A, LDA, WSAVE)
C-----------------------------------------------------------------------
C! Pseudo AP routine: complex-to-complex 2D FFT
C# AP-FFT
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2006
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
C   Replace A(1:LEN1, 1:LEN2) with its two dimensional DFT (see QCFT1D).
C   Calling QCFT1D with SIGN = +1 and then with SIGN = -1 on has the
C   effect of multiplying each element of the array by LEN1 by LEN2.
C
C   The following conditions must be true (FFTMAX is defined in
C   INCS:PFFT.INC)
C
C   #  0 < LEN2 <= FFTMAX
C   #  LEN1 <= LDA
C   #  SIGN = +1 or SIGN = -1
C   #  WSAVE has been initialized by calling QCFT2I
C
C   This routine may be optimized for parallel execution on machines
C   that support this.
C
C   Input:
C      SIGN       I          Direction of transform (see QCFT1D)
C      LEN1       I          Rows to be transformed
C      LEN2       I          Columns to be transformed
C      LDA        I          Leading dimension of A
C      WSAVE      R(*)       Initialized workspace (from QCFT1I)
C
C   Input/Output:
C      ARRAY      C(LDA, *)  Array to be transformed: replaced by its
C-----------------------------------------------------------------------
      INTEGER   SIGN, LEN1, LEN2, LDA
      COMPLEX   A(LDA, *)
      REAL      WSAVE(*)
C
      INCLUDE 'INCS:PFFT.INC'
      COMPLEX   SCRTCH(FFTMAX)
      INTEGER   IDX2
      INTEGER   ROW, COL
C-----------------------------------------------------------------------
      IDX2 = FTWMUL * LEN1 + FTWADD + 1
C                                       Transform columns:
C                                       Good candidate for parallel
C                                       execution.
      IF (LEN1.GT.1) THEN
         DO 10 COL = 1,LEN2
            CALL QCFT1D (SIGN, LEN1, A(1, COL), WSAVE(1))
 10         CONTINUE
         END IF
C                                       Transform rows:
      IF (LEN2.GT.1) THEN
         DO 40 ROW = 1,LEN1
C                                       Gather row into SCRTCH:
            DO 20 COL = 1,LEN2
               SCRTCH(COL) = A(ROW,COL)
 20            CONTINUE
            CALL QCFT1D (SIGN, LEN2, SCRTCH, WSAVE(IDX2))
C                                       Restore transformed row:
            DO 30 COL = 1,LEN2
               A(ROW,COL) = SCRTCH(COL)
 30            CONTINUE
 40         CONTINUE
         END IF
C
 999  RETURN
      END
