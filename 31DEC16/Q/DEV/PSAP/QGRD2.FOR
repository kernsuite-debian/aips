      SUBROUTINE QGRD2 (UV, VIS, WT, GRID, CONX, CONY, NO2, M, LROW)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Convolves linear polarization data onto a grid.
C# AP-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2012
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
C   Pseudo-AP version
C   Convolves linear polarization visibility data into a grid.
C   It accumulates two independent visibility channels with the
C   convolution function and weighting.
C   Data is ignored if weight non-positive or the datum is too far off
C   the edge of the grid.
C   Inputs:
C      UV    I  Location of (u,v) values in cells.
C      VIS   I  Location of (complex) visibilities.
C               Expects Re1, Im1, Re2, Im2
C      WT    I  Weight for data. Assumes any tapering
C               has already been done.
C      GRID  I  base address of gridded data.
C               Order assumed to be the following
C               for each of the M rows:
C                1) 2 * LROW visibilities
C                2) 2 * LROW visibilities, Vis 2
C      CONX  I  base address of X convolving fn.
C      CONY  I  base address of Y convolving fn.
C      NO2   I  INT( (# cells used on a row) / 2 )
C      M     I  number of rows kept in the AP.
C      LROW  I  length of a row ( max. X).
C   In the above, X refers to rows and y to columns
C   in the gridded data, NOT on the sky.  The total
C   numbers of rows and cells used on a row should be odd.
C   All AP memory I/O values are assumed floating.
C   It is assumed that all values of v correspond to row M/2.
C-----------------------------------------------------------------------
      INTEGER   UV, VIS, WT, GRID, CONX, CONY, NO2, M, LROW
C
      LONGINT   JUV, JVIS, JWT, JGRID, JCONX, JCONY, JCX, JCY, JA, JG,
     *   JJCX
      INTEGER   IRND, N, IADD, IX, IY, INCR
      DOUBLE PRECISION AIM1, AIM2, CWT, RE1, RE2, X, XX, XWT, Y, TCWT
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IRND(XX) = INT (XX + SIGN (0.5D0, XX))
C-----------------------------------------------------------------------
C                                        Convert addresses to 1 rel.
      JUV = UV + PSAPOF
      JVIS = VIS + PSAPOF
      JWT = WT + PSAPOF
      JGRID = GRID + PSAPOF
      JCONX = CONX + PSAPOF
      JCONY = CONY + PSAPOF
      N = NO2 * 2 + 1
C                                        Check weight.
      XWT = APCORE(JWT)
      IF (XWT.LE.0.0D0) GO TO 999
C                                        Determine loaction.
      X = APCORE(JUV+1)
      Y = APCORE(JUV)
C                                        Deter. conv. fn loc.
      JCX = IRND (100.D0 * (IRND (X)-X-0.5D0))
      JCY = IRND (100.D0 * (IRND (Y)-Y-0.5D0))
      JCX = JCX + JCONX + 100
      JCY = JCY + JCONY + 100
C                                        Determine grid loc.
      JG = JGRID
      JA = JGRID + 2 * LROW
C                                        Check end of row.
      IX = IRND (X) + LROW / 2 + 1
      IADD = LROW - NO2 - IX
C                                        Will hit end of row.
      IF (IADD.LT.0) THEN
         N = N + IADD
         IF (N.LE.0) GO TO 999
         END IF
C                                        Check front of row.
      IX = IX - NO2 - 1
C                                        Will hit front of row
      IF (IX.LT.0) THEN
         N = N + IX
         IF (N.LE.0) GO TO 999
         JCX = JCX - IX * 100
C                                        Finish grid address.
      ELSE
         JA = JA + 2 * IX
         JG = JG + 2 * IX
         END IF
C                                        Save JCX
      JJCX = JCX
C                                        Get visibility
      RE1 = APCORE(JVIS)
      AIM1 = APCORE(JVIS+1)
      RE2 = APCORE(JVIS+2)
      AIM2 = APCORE(JVIS+3)
C                                        Set increment
      INCR = 4 * LROW - 2 * N
C                                        Gridding loop
      INCLUDE 'INCS:ZVND.INC'
      DO 200 IY = 1,M
         JCX = JJCX
         TCWT = APCORE(JCY) * XWT
      INCLUDE 'INCS:ZVND.INC'
         DO 100 IX = 1,N
C                                        Compute weight.
            CWT = APCORE(JCX) * TCWT
C                                        Sum to grid.
            APCORE(JG) = APCORE(JG) + CWT * RE1
            APCORE(JG+1) = APCORE(JG+1) + CWT * AIM1
            APCORE(JA) = APCORE(JA) + CWT * RE2
            APCORE(JA+1) = APCORE(JA+1) + CWT * AIM2
C                                        Update pointers.
            JCX = JCX + 100
            JG = JG + 2
            JA = JA + 2
 100        CONTINUE
C                                        Update pointers.
         JCY = JCY + 100
         JG = JG + INCR
         JA = JA + INCR
 200     CONTINUE
C
 999  RETURN
      END
