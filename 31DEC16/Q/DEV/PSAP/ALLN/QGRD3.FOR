      SUBROUTINE QGRD3 (UV, VIS, WT, GRID, CONX, CONY, NO2, M, LROW)
C-----------------------------------------------------------------------
C! Alliant Pseudo AP routine: Convolves uv data onto a grid.
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
C   Convolves visibility data onto a grid.
C   Works for total intensity mapping, accumulating weighted
C   gridded visibility values and the sum of the weights
C   accumulated into a cell.
C   Inputs:
C      UV    I  Location of (u,v) values in cells.
C      VIS   I  Location of (complex) visibilities.
C      WT    I  Weight for data. Assumes any tapering
C               has already been done.
C      GRID  I  base address of gridded data.
C               Order assumed to be the following
C               for each of the M rows:
C               1) 2 * LROW visibilities
C               2) 2 * LROW (sum weights,0)
C      CONX  I  base address of X convolving fn.
C      CONY  I  base address of Y convolving fn.
C      NO2   I  INT( (# cells used on a row) / 2 )
C      M     I  number of rows kept in the AP.
C      LROW  I  length of a row ( max. X).
C   In the above, X refers to rows and y to columns in the gridded data,
C   NOT on the sky.  The total numbers of rows and cells used on a row
C   should be odd.  All AP memory I/O values are assumed floating.  It
C   is assumed that all values of v correspond to row M/2.
C   Alliant version (similar to pseudo AP version).
C-----------------------------------------------------------------------
      INTEGER   UV, VIS, WT, GRID, CONX, CONY, NO2, M, LROW
C
      INTEGER   JUV, JVIS, JWT, JGRID, JCONX, JCONY, JCX, JCY, JA, JG,
     *   JJCX, IRND, JA0, JG0, INCR2, N, INCR, IADD, IX, IY
      REAL    AIM, CWT, RE, X, XX, XWT, Y, TCWT
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IRND(XX) = INT (XX + SIGN (0.5, XX))
C                                        Convert addresses to 1 rel.
      JUV = UV + 1
      JVIS = VIS + 1
      JWT = WT + 1
      JGRID = GRID + 1
      JCONX = CONX + 1
      JCONY = CONY + 1
      N = NO2 * 2 + 1
C                                        Check weight.
      XWT = APCORE(JWT)
      IF (XWT.LE.0.0) GO TO 999
C                                        Determine location.
      X = APCORE(JUV+1)
      Y = APCORE(JUV)
C                                        Deter. conv. fn loc.
      JCX = IRND (100. * (IRND (X)-X-0.5))
      JCY = IRND (100. * (IRND (Y)-Y-0.5))
      JCX = JCONX + JCX + 100
      JCY = JCONY + JCY + 100
C                                        Determine grid loc.
      JG = JGRID
      JA = JGRID + 2 * LROW
C                                        Check end of row.
      IX = IRND (X) + LROW / 2 + 1
      IADD = LROW - NO2 - IX
      IF (IADD.GE.0) GO TO 10
C                                        Will hit end of row.
         N = N + IADD
         IF (N.LE.0) GO TO 999
C                                        Check front of row.
 10      CONTINUE
      IX = IX - NO2 - 1
      IF (IX.GE.0) GO TO 20
C                                        Will hit front of row
         N = N + IX
         IF (N.LE.0) GO TO 999
         JCX = JCX - IX * 100
         GO TO 30
C                                        Finish grid address.
 20      CONTINUE
      JA = JA + 2 * IX
      JG = JG + 2 * IX
 30   CONTINUE
C                                        Save JCX
      JJCX = JCX
C                                        Get visibility
      RE = APCORE(JVIS)
      AIM = APCORE(JVIS+1)
      INCR = 4 * LROW - 2 * N
      INCR2 = 4 * LROW
      JG0 = JG
      JA0 = JA
C                                        Gridding loop
      INCLUDE 'INCS:ZVND.INC'
      DO 200 IY = 1,M
         JCX = JJCX
         TCWT = APCORE(JCY) * XWT
         JG = JG0
         JA = JA0
      INCLUDE 'INCS:ZVND.INC'
         DO 100 IX = 1,N
C                                        Compute weight.
            CWT = APCORE(JCX) * TCWT
C                                        Sum to grid.
            APCORE(JG) = APCORE(JG) + CWT * RE
            APCORE(JG+1) = APCORE(JG+1) + CWT * AIM
            APCORE(JA) = APCORE(JA) + CWT
C                                        Update pointers.
            JCX = JCX + 100
            JG = JG + 2
            JA = JA + 2
 100        CONTINUE
C                                        Update pointers.
         JCY = JCY + 100
         JG0 = JG0 + INCR2
         JA0 = JA0 + INCR2
 200     CONTINUE
C
 999  RETURN
      END
