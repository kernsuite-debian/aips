      SUBROUTINE QCVCMU (A, I, B, C, J, N)
C-----------------------------------------------------------------------
C! FPS AP version: Multiply conj. of complex vector by complex scalar
C# AP-util
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
C   FPS AP version
C   QCVCMU multiplies a scalar complex value times the complex
C   conjugate of a vector producing a real vector:
C        C(K) = REAL(B)*A(K)+IMAG(B)*A(K+1)  K = 1,N
C   Inputs:
C    A   I  Source complex vector base address.
C    I   I  Increment of A
C    B   I  Address of scalar (real part)
C    C   I  Destination real vector base address.
C    J   I  Increment of C
C    N   I  Element count (reals)
C-----------------------------------------------------------------------
      INTEGER A, I, B, C, J, N
      INTEGER*2 IA, II, IB, IC, IJ, IN
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (A.LT.32768) IA = A
      IF (A.GE.32768) IA = A - 65536
      IF (I.LT.32768) II = I
      IF (I.GE.32768) II = I - 65536
      IF (B.LT.32768) IB = B
      IF (B.GE.32768) IB = B - 65536
      IF (C.LT.32768) IC = C
      IF (C.GE.32768) IC = C - 65536
      IF (J.LT.32768) IJ = J
      IF (J.GE.32768) IJ = J - 65536
      IF (N.LT.32768) IN = N
      IF (N.GE.32768) IN = N - 65536
C                                        Call FPS routine.
      CALL CVCMUL (IA, II, IB, IC, IJ, IN)
C
 999  RETURN
      END
