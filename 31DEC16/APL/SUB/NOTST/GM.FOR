      SUBROUTINE GM (N, SHESS, HESS, WORK, LDH, GRAD, IER)
C-----------------------------------------------------------------------
C! Gain Soln: Greenstadt's modification to Newton's method.
C# UV Calibration
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
C   GM does Greenstadt's modification to Newton's method.
C-----------------------------------------------------------------------
      INTEGER   LDH, N
      DOUBLE PRECISION SHESS(LDH,*), HESS(LDH,*), WORK(*), GRAD(*),
     *   E(100), EPMACH, DELTA, BETA, XX
      INTEGER   I1, JER, I, K, J, IER
      INCLUDE 'INCS:DMSG.INC'
      DATA EPMACH /1.39D-17/,       I1 /1/
C-----------------------------------------------------------------------
      DELTA = SQRT (EPMACH)
C                                       Reduce the Hessian to
C                                       tridiagonal form:
      CALL TRED2 (LDH, N, SHESS, WORK, E, HESS)
C                                       Get the eigenvalues in WORK and
C                                       the eigenvectors in HESS,
C                                       from a call to IMTQL2:
      CALL IMTQL2 (LDH, N, WORK, E, HESS, JER)
      IER = JER
C      CALL TQL2 (LDH, N, WORK, E, HESS, IER)
      WRITE (MSGTXT,1000) IER
      IF (IER.NE.0) CALL MSGWRT (8)
      IF (IER.NE.0) GO TO 999
C                                       Now compute the modified Newton
C                                       direction:
      DO 10 I = 1,N
         E(I) = 0.0D0
 10      CONTINUE
      DO 20 K = 1,N
         BETA = MAX (ABS (WORK(K)), DELTA)
         CALL DDOT (N, HESS(1,K), I1, GRAD, I1, XX)
         XX = XX / BETA
         DO 20 J = 1,N
            E(J) = E(J) + XX * HESS(J,K)
 20         CONTINUE
C                                       Place the Newton direction
C                                       in GRAD:
      DO 30 I = 1,N
         GRAD(I) = E(I)
 30      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GM: ENCOUNTERED ERROR',I3,' IN IMTQL2')
      END
