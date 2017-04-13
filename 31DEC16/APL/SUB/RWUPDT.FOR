      SUBROUTINE RWUPDT (N, R, LDR, W, B, ALPHA, COS, SIN)
C-----------------------------------------------------------------------
C! computes the QR decomposition of an upper triangular matrix + a row
C# Math Modeling
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
C   Given an N by N upper triangular matrix R, this subroutine computes
C   the QR decomposition of the matrix formed when a row is added to R.
C   If the row is specified by the vector W, then RWUPDT determines an
C   orthogonal matrix Q such that when the N+1 by N matrix composed of R
C   augmented by W is premultiplied by (Q transpose), the resulting
C   matrix is upper trapezoidal.  The matrix (Q transpose) is the
C   product of N transformations
C
C           G(N)*G(N-1)* ... *G(1)
C
C   where G(I) is a Givens rotation in the (I,N+1) plane which
C   eliminates elements in the (N+1)-st plane. RWUPDT also computes the
C   product (Q transpose)*C where C is the (N+1)-vector (B,ALPHA). Q
C   itself is not accumulated, rather the information to recover the G
C   rotations is supplied.
C   Argonne National Laboratory. Minpack Project. March 1980.
C   Burton S. Garbow, Dudley V. Goetschel, Kenneth E. Hillstrom,
C   Jorge J. More
C   Inputs:
C      N      I       the order of R (adj. array dim.).
C      LDR    I       positive variable not less than N which
C                     specifies the leading dimension of the array R
C                     (adj. array dim.).
C      W      D(N)    contains the row vector to be added to R.
C  In/out:
C      R      D(N,N)  On input the upper triangular part must contain
C                     the matrix to be updated. On output, R contains
C                     the updated triangular matrix.
C      B      D(N)    On input B contains the first N elements
C                     of the vector C. On output B contains the first N
C                     elements of the vector (Q transpose)*C.
C      ALPHA  D       On input ALPHA contains the (n+1)st element
C                     of the vector C. On output ALPHA contains the
C                     The (N+1)-st element of the vector (Q transpose)*C
C   Outputs:
C      COS    D(N)    contains the cosines of the transforming
C                     Givens rotations.
C      SIN    D(N)    contains the sines of the transforming
C                     Givens rotations.
C-----------------------------------------------------------------------
      INTEGER   N, LDR
      DOUBLE PRECISION ALPHA, R(LDR,N), W(N), B(N), COS(N), SIN(N)
C
      INTEGER   I, J, JM1
      DOUBLE PRECISION COTAN, ONE, P5, P25, ROWJ, TAN, TEMP, ZERO
      DATA ONE, P5, P25, ZERO /1.0D0, 5.0D-1, 2.5D-1, 0.0D0/
C-----------------------------------------------------------------------
C
      DO 60 J = 1,N
         ROWJ = W(J)
         JM1 = J - 1
C                                       Apply the previous
C                                       transformations to
C                                       R(I,J), I=1,2,...,J-1,
C                                       and to W(J).
         IF (JM1.GE.1) THEN
            DO 10 I = 1,JM1
               TEMP = COS(I)*R(I,J) + SIN(I)*ROWJ
               ROWJ = -SIN(I)*R(I,J) + COS(I)*ROWJ
               R(I,J) = TEMP
   10          CONTINUE
            END IF
C                                       Determine a Givens rotation
C                                       which eliminates W(J).
         COS(J) = ONE
         SIN(J) = ZERO
         IF (ROWJ.EQ.0) GO TO 60
            IF (ABS(R(J,J)).LT.ABS(ROWJ)) THEN
               COTAN = R(J,J) / ROWJ
               SIN(J) = P5 / SQRT (P25+P25*COTAN**2)
               COS(J) = SIN(J) * COTAN
            ELSE
               TAN = ROWJ / R(J,J)
               COS(J) = P5 / SQRT (P25+P25*TAN**2)
               SIN(J) = COS(J) * TAN
               END IF
C                                       Apply the current transformation
C                                       to R(J,J), B(J), and ALPHA.
            R(J,J) = COS(J)*R(J,J) + SIN(J)*ROWJ
            TEMP = COS(J)*B(J) + SIN(J)*ALPHA
            ALPHA = -SIN(J)*B(J) + COS(J)*ALPHA
            B(J) = TEMP
   60    CONTINUE
C
 999  RETURN
      END
