      SUBROUTINE QCLNSU (COMP, LMAP, L1MAP, L2MAP, IBX, IBY, JNDEX,
     *   INDEX)
C-----------------------------------------------------------------------
C! Alliant Pseudo AP routine:  Low level Clark CLEAN routine
C# AP-appl
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
C   QCLNSU does a CLEAN on a list of residuals using a given beam
C   patch
C   Inputs:
C      COMP     R(4)  Component vector:
C                     0 => intensity
C                     1 => x in cells
C                     2 => Y in cells
C                     3 => CLEAN loop gain (fractional)
C      LMAP     I    number of residuals
C      L1MAP    I    First residual in Y window
C      L2MAP    I    Last residual in Y window
C      IBX,IBY  I    Beam patch half width in X and Y
C                    Residuals with delta X or Y from the component
C                    position in COMP .GE. IBX,IBY are to be ignored.
C      JNDEX    I    0 - rel Index in FLUX, IX, IY of next residual
C                    to sub.
C   Input From Vector Work common:
C      IWVEC3   I    = IX, X pixel location of residuals
C      IWVEC4   I    = IY, Y pixel location of residuals
C      WKVEC5   R    = FLUX, Residual flux density
C      WKVEC6,7 R    = BEAM, BEAM patch.
C   Output:
C      INDEX    I    Index in FLUX of next residual.
C   Useage notes:
C     The following common work vectors are used:
C      IWVEC8   = IDXRAY  => array of x pixel offsets
C      IWVEC9   = IB      => indirect address array
C
C   Alliant version (same as Convex version).
C-----------------------------------------------------------------------
      INTEGER  	ISAMAX, KMAX, INCX, IDXRAY(1), IB(1), IX(1), IY(1)
      INTEGER  	L21MAP, NIB, JNDEX
      INTEGER   LMAP, IBX, IBY, L1MAP, L2MAP, INDEX, BOFF, BEMADR,
     *   LROW, ADDR, LOOP, XCOMP, YCOMP
      REAL      SUBT, COMP(4), BEAM(1), FLUX(1)
      INCLUDE 'INCS:DAPC.INC'
      EQUIVALENCE (IX, IWVEC3), (IY, IWVEC4), (FLUX, WKVEC5),
     *   (BEAM, WKVEC6)
      EQUIVALENCE (IDXRAY, IWVEC8), (IB, IWVEC9)
C-----------------------------------------------------------------------
      LROW = 2 * IBY - 1
      BOFF = (IBX-1) * LROW + IBY
      IF (LMAP.LE.0) GO TO 999
C                                        Get component to be CLEANed
      XCOMP = COMP(2) + 0.5
      YCOMP = COMP(3) + 0.5
      SUBT = COMP(1) * COMP(4)
C					Get residual offsets
      INCLUDE 'INCS:ZVND.INC'
      DO 100 LOOP = L1MAP,L2MAP
         IDXRAY(LOOP) = IX(LOOP+JNDEX) - XCOMP
 100     CONTINUE
C                                        Compress x window
      L21MAP = L2MAP - L1MAP + 1
      CALL WHNALT (L21MAP, IDXRAY(L1MAP), 1, IBX, IB, NIB)
C                                        Subtraction loop
      INCLUDE 'INCS:ZVND.INC'
      DO 200 LOOP = 1,NIB
         ADDR = IB(LOOP) + L1MAP - 1
C                                        Get beam address
         BEMADR = BOFF + (IDXRAY(ADDR) * LROW)
     *      + (IY(ADDR+JNDEX) - YCOMP)
         FLUX(ADDR+JNDEX) = FLUX(ADDR+JNDEX) - SUBT * BEAM(BEMADR)
 200     CONTINUE
C                                        Find largest mag. residual
      INCX = 1
C                                       Call Alliant provided Linpack
C                                       routine = ISAMAX.
      KMAX = ISAMAX (LMAP, FLUX(1+JNDEX), INCX)
      COMP(1) = FLUX (KMAX+JNDEX)
      COMP(2) = IX(KMAX+JNDEX)
      COMP(3) = IY(KMAX+JNDEX)
C                                        Save index of next max.
      INDEX = KMAX
C
 999  RETURN
      END
