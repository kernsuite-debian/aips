      SUBROUTINE QCFT3D (SIGN, LEN1, LEN2, LEN3, A, LDA1, LDA2, WSAVE)
C-----------------------------------------------------------------------
C! Pseudo AP routine: complex-to-complex 3D FFT
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
C   Replace A(1:LEN1, 1:LEN2, 1:LEN3) with its two dimensional DFT
C   (see QCFT1D). Calling QCFT1D with SIGN = +1 and then with SIGN = -1
C   on the same array has the effect of multiplying each element of the
C   array by LEN1*LEN2*LEN3.
C
C   The following conditions must be true (FFTMAX is defined in
C   INCS:PFFT.INC)
C
C   0 < LEN2 <= FFTMAX
C   0 < LEN3 <= FFTMAX
C   LEN1 <= LDA1
C   LEN2 <= LDA2
C   SIGN = +1 or SIGN = -1
C   WSAVE has been initialized by calling QCFT2I
C
C   This routine may be optimized for parallel execution on machines
C   that support this.
C
C   Input:
C      SIGN       I          Direction of transform (see QCFT1D)
C      LEN1       I          Rows to be transformed
C      LEN2       I          Columns to be transformed
C      LEN3       I          Planes to be transformed
C      LDA1       I          Leading dimension of A
C      LDA2       I          Second dimension of A
C      WSAVE      R(*)       Initialized workspace (from QCFT3I)
C   Input/Output:
C      ARRAY      C(LDA1, LDA2, *)
C                            Array to be transformed: replaced by its
C                            transform
C-----------------------------------------------------------------------
      INTEGER   SIGN, LEN1, LEN2, LEN3, LDA1, LDA2
      COMPLEX   A(LDA1,LDA2,*)
      REAL      WSAVE(*)
C
      INCLUDE 'INCS:PFFT.INC'
      COMPLEX   SCRTCH(FFTMAX)
      INTEGER   IDX3
      INTEGER   I, J, K
C-----------------------------------------------------------------------
      IDX3 = FTWMUL * (LEN1 + LEN2) + 2 * FTWADD + 1
C                                       Transform planes:
      IF ((LEN1.GT.1) .OR. (LEN2.GT.1)) THEN
         DO 10 K = 1,LEN3
            CALL QCFT2D (SIGN, LEN1, LEN2, A(1, 1, K), LDA1, WSAVE(1))
 10         CONTINUE
         END IF
C                                       Transform third dimension
      IF (LEN3.GT.1) THEN
         DO 50 J = 1,LEN2
            DO 40 I = 1,LEN1
               DO 20 K = 1,LEN3
                  SCRTCH(K) = A(I,J,K)
 20               CONTINUE
               CALL QCFT1D (SIGN, LEN3, SCRTCH, WSAVE(IDX3))
               DO 30 K = 1,LEN3
                  A(I,J,K) = SCRTCH(K)
 30               CONTINUE
 40            CONTINUE
 50         CONTINUE
         END IF
C
 999  RETURN
      END
