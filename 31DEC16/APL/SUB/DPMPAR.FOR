      DOUBLE PRECISION FUNCTION DPMPAR (I)
C-----------------------------------------------------------------------
C! returns machine precision or smallest or largest magnitude
C# Math
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
C   This function provides double precision machine parameters
C   when the appropriate set of data statements is activated (by
C   removing the C from column 1) and all other data statements are
C   rendered inactive. Most of the parameter values were obtained
C   from the corresponding Bell Laboratories port library function.
C   Argonne National Laboratory. Minpack Project. March 1980.
C   Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C   INPUT:  I   I   set to 1, 2, or 3 which selects the desired
C               machine parameter. If the machine has T base B digits
C               and its smallest and largest exponents are EMIN and
C               EMAX, respectively, then these parameters are
C
C         DPMPAR(1) = B**(1 - T), the machine precision,
C
C         DPMPAR(2) = B**(EMIN - 1), the smallest magnitude,
C
C         DPMPAR(3) = B**EMAX*(1 - B**(-T)), the largest magnitude.
C-----------------------------------------------------------------------
      DOUBLE PRECISION    DMACH(3)
      INTEGER   I
C                                       For VAX:
      DATA DMACH /2.7755576D-17,  .2938735877055719D-38,
     *   .1701411834604692D39/
C-----------------------------------------------------------------------
      DPMPAR = DMACH(I)
C
 999  RETURN
      END
