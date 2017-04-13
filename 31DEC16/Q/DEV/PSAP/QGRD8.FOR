      SUBROUTINE QGRD8 (UV, VIS, WT, GRID, CONX, CONY, CONI, NO2, M,
     *   LROW)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Convolve data onto a grid: sigma**2 estimate
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
C   Convolves pseudo-visibility (real only) data onto a grid.
C   Works for total intensity mapping, accumulating weighted
C   gridded visibility values and the sum of the weights
C   accumulated into a cell.
C   Inputs:
C      UV    I  Location of (u,v) values in cells, uniform weight FACTOR
C      VIS   I  Location of (complex) visibilities.
C      WT    I  Weight for data. Assumes any tapering
C               has already been done.
C      GRID  I  base address of gridded data.
C               Order assumed to be the following
C               for each of the M rows:
C                1) 2 * LROW visibilities (sum gridded vis, sum weights)
C      CONX  I  base address of X convolving fn.
C      CONY  I  base address of Y convolving fn.
C      CONI  I  increment in convolving func = 1/CONI cells
C      NO2   I  INT( (# cells used on a row) / 2 )
C      M     I  number of rows kept in the AP.
C      LROW  I  length of a row ( max. X).
C   In the above, X refers to rows and y to columns in the gridded data,
C   NOT on the sky.  The total numbers of rows and cells used on a row
C   should be odd.  All AP memory I/O values are assumed floating.
C   It is assumed that all values of v correspond to row M/2.
C-----------------------------------------------------------------------
      INTEGER   UV, VIS, WT, GRID, CONX, CONY, CONI, NO2, M, LROW
C
      LONGINT   JUV, JVIS, JWT, JGRID, JCONX, JCONY, JCX, JCY, JG, JJCX
      INTEGER   N, INCR, IADD, IX, IY, IRND, LINC
      DOUBLE PRECISION ARE, AIM, X, XX, XWT, Y, UWT, XCONI
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
      XCONI = CONI
C                                        Check weight.
      XWT = APCORE(JWT)
      IF (XWT.LE.0.0D0) GO TO 999
      UWT = APCORE(JUV+2)
      IF (UWT.LE.0.0D0) GO TO 999
C                                        Determine location.
      X = APCORE(JUV+1)
      Y = APCORE(JUV)
C                                        Separable conv functions
      IF (JCONY.NE.JCONX) THEN
C                                        Deter. conv. fn loc.
         JCX = IRND (XCONI * (IRND (X)-X-0.5D0))
         JCY = IRND (XCONI * (IRND (Y)-Y-0.5D0))
         JCX = JCX + JCONX + CONI
         JCY = JCY + JCONY + CONI
C                                        Determine grid loc.
         JG = JGRID
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
            JCX = JCX - IX * CONI
C                                        Finish grid address.
         ELSE
            JG = JG + 2 * IX
            END IF
C                                        Save JCX
         JJCX = JCX
C                                        Get visibility
         INCR = 2 * LROW - 2 * N
C                                        Gridding loop
      INCLUDE 'INCS:ZVND.INC'
         DO 30 IY = 1,M
            JCX = JJCX
            AIM = APCORE(JCY) * XWT * UWT
            ARE = APCORE(JCY) * AIM * UWT
      INCLUDE 'INCS:ZVND.INC'
            DO 20 IX = 1,N
C                                        Sum to grid.
               APCORE(JG) = APCORE(JG) + APCORE(JCX) * APCORE(JCX) * ARE
               APCORE(JG+1) = APCORE(JG+1) + APCORE(JCX) * AIM
C                                        Update pointers.
               JCX = JCX + CONI
               JG = JG + 2
 20            CONTINUE
C                                        Update pointers.
            JCY = JCY + CONI
            JG = JG + INCR
 30         CONTINUE
C                                       circular function
      ELSE
C                                        Deter. conv. fn loc.
         JCX = IRND (XCONI * (IRND (X)-X-0.5D0))
         JCY = IRND (XCONI * (IRND (Y)-Y-0.5D0))
         JCX = JCX + JCONX + CONI
         JCY = JCY + CONI
C                                        Determine grid loc.
         JG = JGRID + 2 * (IRND (X) + LROW / 2 - NO2)
C                                        Save JCX
         JJCX = JCX
C                                        Get visibility
         INCR = 2 * LROW - 2 * N
         LINC = CONI * M + 1
C                                        Gridding loop
         AIM = XWT * UWT
         ARE = AIM * UWT
      INCLUDE 'INCS:ZVND.INC'
         DO 130 IY = 1,M
            JCX = JJCX + JCY
      INCLUDE 'INCS:ZVND.INC'
            DO 120 IX = 1,N
C                                        Sum to grid.
               APCORE(JG) = APCORE(JG) + APCORE(JCX) * APCORE(JCX) * ARE
               APCORE(JG+1) = APCORE(JG+1) + APCORE(JCX) * AIM
C                                        Update pointers.
               JCX = JCX + CONI
               JG = JG + 2
 120           CONTINUE
C                                        Update pointers.
            JCY = JCY + CONI * LINC
            JG = JG + INCR
 130        CONTINUE
         END IF
C
 999  RETURN
      END
