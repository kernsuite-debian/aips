      SUBROUTINE GETERR (NGAUSS, IPVT, FJAC, PARERR, MPARMS, NDATA,
     *   MDATA, FNORM, WA, TOL)
C-----------------------------------------------------------------------
C! calculates the errors on the fitted parameters
C# Modeling
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
C   This subroutine for GAUSS calculates the errors on the fitted
C   parameters.
C   Inputs:
C      NGAUSS  I        the no. of gaussian components in the fitted
C                       function.
C      IPVT    I(MPARMS)   Defines a permutation matrix P such that
C                       JAC*P = Q*R, where JAC is the final calculated
C                       Jacobian, Q is
C                   orthogonal (not stored), and R is upper triangular
C                   with diagonal elements of nonincreasing magnitude.
C                   column J of P is column IPVT(J) of the identity
C                   matrix. (See FJAC below).
C           FJAC    D(MDATA,MPARMS)   The upper MPARMS by MPARMS sub-
C                   matrix of FJAC contains an upper triangular matrix
C                   R with diagonal elements of nonincreasing magnitude
C                   such that
C
C                    T     T           T
C                   P *(JAC *JAC)*P = R *R,
C
C                   where P is a permutation matrix and JAC is the final
C                   calculated Jacobian. Column J of P is column IPVT(J)
C                   (see above) of the identity matrix.
C           MPARMS  I Number of parameters in fitted function (adj.
C                   array dim.).
C           NDATA   I   Number of data points fitted.
C           MDATA   I Maximum no. of data points allowed for in
C                   FJAC (adj. array dim.).
C           FNORM   D   Euclidian norm of solution vector.
C           WA      D(MPARMS)   work array.
C  OUTPUT:  PARERR  D(MPARMS)   error in fitted parameters.
C           TOL     D   tolerance used in call to LMDER1.
C-----------------------------------------------------------------------
      INTEGER   NGAUSS, MDATA, MPARMS, NDATA, IPVT(MPARMS)
      DOUBLE PRECISION FJAC(MDATA,MPARMS), PARERR(MPARMS), FNORM,
     *   WA(MPARMS), TOL
C
      INTEGER   IPARMS, J, NPARMS
      DOUBLE PRECISION EPSILN
C-----------------------------------------------------------------------
C                                       Calculate error following
C                                       Argonne write up
      NPARMS = NGAUSS * 3
C                                       Is this right ??????
      EPSILN = FNORM / SQRT(REAL(NDATA-NPARMS))
      IPARMS = NPARMS
      CALL COVAR (IPARMS, FJAC, MDATA, IPVT, TOL, WA)
      DO 10 J = 1,NPARMS
         PARERR(J) = EPSILN * SQRT(FJAC(J,J))
 10      CONTINUE
C
      RETURN
      END
