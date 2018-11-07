      SUBROUTINE QCLNSU (COMP, LMAP, L1MAP, L2MAP, IBX, IBY, JNDEX,
     *   INDEX)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Low level Clark CLEAn routine
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
C    Inputs:
C      COMP(4)  R    Component vector:
C                 0 => intensity
C                 1 => x in cells
C                 2 => Y in cells
C                 3 => CLEAN loop gain (fractional)
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
C                 also FLXTMP => temporary flux array
C      IWVEC9   = IDYRAY  => array of y pixel offsets
C      IWVECA   = IB      => indirect address array
C      IWVECB   = BEMADR  => Beam address array
C      WKVECC   = BEMTMP  => Beam address array
C   CRI version.
C-----------------------------------------------------------------------
      INTEGER   ISAMAX, KMAX, INCX, IDXRAY(1),  IDYRAY(1), IADX(1)
      INTEGER   L21MAP, NIB, IB(1)
      INTEGER   AMAP, LMAP, ABEAM, BBEAM, IBX, IBY, IDX, IDY,
     *   L1MAP, L2MAP, INDEX, BOFF, BEMADR(1), LROW, LIMIT, ADDR,
     *   JCOMP, JAMAP, JBBEAM, LOOP, XCOMP, YCOMP, X, Y, N1, JNDEX
      INTEGER   IX(1), IY(1)
      REAL      SUBT, COMP(4), BEAM(1), FLUX(1), BEMTMP(1),
     *   FLXTMP(1), XMAX, AXMAX
      INCLUDE 'INCS:DAPC.INC'
      EQUIVALENCE (IDXRAY, FLXTMP, IWVEC8), (IDYRAY, IADX, IWVEC9),
     *   (IB, IWVECA), (BEMADR, IWVECB), (BEMTMP, WKVECC)
      EQUIVALENCE (IX, IWVEC3),
     *   (IY, IWVEC4), (FLUX, WKVEC5), (BEAM, WKVEC6)
      DATA N1 /1/
C-----------------------------------------------------------------------
      LROW = 2 * IBY - 1
      BOFF = (IBX-1) * LROW + IBY
      IF (LMAP.LE.0) GO TO 999
C                                        Get component to be CLEANed
      XCOMP = COMP(2) + 0.5
      YCOMP = COMP(3) + 0.5
      SUBT = COMP(1) * COMP(4)
C                                       Get residual x offsets
      INCLUDE 'INCS:ZVND.INC'
      DO 50 LOOP = L1MAP,L2MAP
         IDXRAY(LOOP) = IX(LOOP+JNDEX) - XCOMP
         IADX(LOOP) = ABS (IDXRAY(LOOP))
 50      CONTINUE
C                                        Compress x window
      L21MAP = L2MAP - L1MAP + 1
      CALL WHENILT (L21MAP, IADX(L1MAP), N1, IBX, IB, NIB)
C                                       Compress IDXRAY, IY
      CALL GATHER (NIB, IDXRAY, IDXRAY(L1MAP), IB)
      CALL GATHER (NIB, IDYRAY, IY(L1MAP+JNDEX), IB)
C                                       Get residual y offsets
      INCLUDE 'INCS:ZVND.INC'
      DO 60 LOOP = 1,NIB
         IDYRAY(LOOP) = IDYRAY(LOOP) - YCOMP
 60      CONTINUE
C                                        Compute beam address
      INCLUDE 'INCS:ZVND.INC'
      DO 110 LOOP = 1,NIB
         BEMADR(LOOP) = BOFF + IDXRAY(LOOP) * LROW + IDYRAY(LOOP)
 110     CONTINUE
C                                       Gather beam
      CALL GATHER (NIB, BEMTMP, BEAM, BEMADR)
C                                       Gather fluxes, overwriting
C                                       IDXRAY.
      CALL GATHER (NIB, FLXTMP, FLUX(L1MAP+JNDEX), IB)
C                                        Subtraction loop
      INCLUDE 'INCS:ZVND.INC'
      DO 120 LOOP = 1,NIB
         FLXTMP(LOOP) = FLXTMP(LOOP) - SUBT * BEMTMP(LOOP)
 120     CONTINUE
C                                       Scatter new residuals back
      CALL SCATTER (NIB, FLUX(L1MAP+JNDEX), IB, FLXTMP)
C                                        Find largest mag. residual
      INCX = 1
      KMAX = ISAMAX (LMAP, FLUX(1+JNDEX), INCX)
      COMP(1) = FLUX (KMAX+JNDEX)
      COMP(2) = IX(KMAX+JNDEX)
      COMP(3) = IY(KMAX+JNDEX)
C                                        Save index of next max.
      INDEX = KMAX
C
 999  RETURN
      END
