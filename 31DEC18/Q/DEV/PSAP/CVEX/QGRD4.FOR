      SUBROUTINE QGRD4 (UV, VIS, WT, GRID, CONX, CONY, NO2, M, LROW,
     *   ROW, INC, NVIS)
C-----------------------------------------------------------------------
C! Convex Pseudo AP routine: Convolve UV data onto a grid.
C# AP-appl UV
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
C   Vector compiler version.
C   Depends on gather/scatter and will probably be slower than the
C   scalar version on scalar machines.
C   The vector length in the vectorized loop here will typically be 49
C   but will never be less than 9.
C   Convolves visibility data onto a grid.
C   A single channel is gridded at a time.
C   It assumes that NO points lie within one half the convolving
C   function support size of the outside edge.
C   Also assumes that the convolving support function contains no more
C   than 512 pixels (32*16).
C   Inputs:
C      UV    I  Location of (u,v) values in cells.
C      VIS   I  Location of (complex) visibilities.
C      WT    I  Weight for data. Assumes any tapering
C               has already been done.
C      GRID  I  base address of gridded data.
C               Order assumed to be the following
C               for each of the M rows:
C                1) 2 * LROW visibilities
C      CONX  I  base address of X convolving fn.
C      CONY  I  base address of Y convolving fn.
C      NO2   I  INT( (# cells used on a row) / 2 )
C      M     I  number of rows kept in the AP.
C      LROW  I  length of a row ( max. X).
C      ROW   I  address of lowest central row number.
C               (contents of ROW subtracted from Y)
C      INC   I  increment for UV, VIS and WT
C      NVIS  I  number of visibilities to grid.
C   In the above, X refers to rows and y to columns
C   in the gridded data, NOT on the sky.  The total
C   numbers of rows and cells used on a row should be odd.
C   All AP memory I/O values are assumed floating.
C   It is assumed that all values of v correspond to row M/2.
C-----------------------------------------------------------------------
      INTEGER   UV, VIS, WT, GRID, CONX,  CONY, NO2, M, LROW, ROW, INC,
     *   NVIS,  N, INCR, HAF, IX, IY, LOOP, LIMIT
      INTEGER   JUV, JVIS, JWT, JGRID, JCONX, JCONY, JCX, JCY,
     *   JG, JJLOOP, IRND, ICX, ICY, IG
      INTEGER   INCX(121), INCY(121), INCG(121), OLDM, OLDN, OLDLRO
      REAL      AIM, RE, X, XX, XWT, Y, WWT, RROW
      INCLUDE 'INCS:DAPC.INC'
      SAVE OLDM, OLDN, OLDLRO, INCX, INCY, INCG
C-----------------------------------------------------------------------
      IRND(XX) = INT (XX + SIGN (0.5, XX))
C                                        Convert addresses to 1 rel.
      JUV = UV + 1
      JVIS = VIS + 1
      JWT = WT + 1
      JGRID = GRID + 1
      JCONX = CONX + 1
      JCONY = CONY + 1
      RROW = APCORE(ROW+1)
      N = NO2 * 2 + 1
      HAF = LROW / 2 - NO2
      INCR = 2 * LROW
      LIMIT = N * M
      IF ((OLDN.EQ.N) .AND. (OLDM.EQ.M) .AND. (OLDLRO.EQ.LROW)) GO TO 70
C                                       Fill increment tables
      OLDN = N
      OLDM = M
      OLDLRO = LROW
      DO 50 LOOP = 1,LIMIT
         IY = (LOOP-1) / N
         IX = (LOOP-1) - IY * N
         INCX(LOOP) = IX * 100
         INCY(LOOP) = IY * 100
         INCG(LOOP) = IX * 2 + IY * INCR
 50      CONTINUE
C                                       Loop over visibilities.
C                                       This loop contains dependencies.
      INCLUDE 'INCS:ZVD.INC'
 70   DO 300 JJLOOP = 1,NVIS
C                                        Check weight.
         XWT = MAX (0.0, APCORE(JWT))
C                                        Determine location.
         X = APCORE(JUV+1)
         Y = APCORE(JUV)
C                                        Deter. conv. fn loc.
         JCX = IRND (100. * (IRND (X) - X - 0.5))
         JCY = IRND (100. * (IRND (Y) - Y - 0.5))
         JCX = JCONX + 100 + JCX
         JCY = JCONY + 100 + JCY
C                                        Determine grid loc.
         Y = Y - RROW
         JG = IRND (X) + IRND (Y) * LROW
         JG = JGRID + 2 * (JG + HAF)
C                                        Get visibility
         RE = APCORE(JVIS) * XWT
         AIM = APCORE(JVIS+1) * XWT
C                                        Gridding loop
      INCLUDE 'INCS:ZVND.INC'
         DO 200 LOOP = 1,LIMIT
            ICX = JCX + INCX(LOOP)
            ICY = JCY + INCY(LOOP)
            IG = JG + INCG(LOOP)
C                                        Sum to grid.
            WWT = APCORE(ICX) * APCORE(ICY)
            APCORE(IG) = APCORE(IG) + WWT * RE
            APCORE(IG+1) = APCORE(IG+1) + WWT * AIM
 200        CONTINUE
C                                       Update for next vis.
         JUV = JUV + INC
         JVIS = JVIS + INC
         JWT = JWT + INC
 300     CONTINUE
C
 999  RETURN
      END
