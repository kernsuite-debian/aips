      SUBROUTINE QINTG (UV, MOD, PHAS, GRID, INTP, M, LROW, INC, NMOD,
     *   ROW)
C-----------------------------------------------------------------------
C! Convex Pseudo AP routine: Interpolate model visibilities from a grid.
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   Vector complier version
C   Will be slower on scalar machines than the scalar version.
C   QINTG Interpolates model visibilities from a grid,
C   corrects the phase and returns the value in the array
C   pointer to by MOD.
C   Assumes that the observations are never within M/2
C   of the outside edge of the grid.  Assumes that the interpolation
C   function is never more than 32x16 (total of 512 elements).
C   Inputs:
C      UV    I  Base address of (u,v) values in cells.
C      MOD   I  Base address for resultant model. (increment=4)
C      PHAS  I  Phase correction, complex value to be multiplied by
C               model vis. Increment = 4.
C      GRID  I  GRID = base address of gridded model vis.
C               Order assumed to be the  following  for each of the
C               M rows:
C                  1) 2 * LROW visibilities
C      INTP  I  base address of interpolation function.
C      M     I  number of rows kept in the AP. Also support size of
C               interp. fn
C      LROW  I  length of a row.
C      INC   I  increment for UV.
C      NMOD  I  number of model values to compute.
C      ROW   I  Address of lowest central row number.
C   All AP memory values are assumed floating.
C   It is assumed that all values of v correspond to row M/2.
C-----------------------------------------------------------------------
      INTEGER   UV, MOD, PHAS, GRID, INTP, M, LROW, INC, NMOD, ROW,
     *   INCR, HAF, IRND, IX, IY, LOOP, LIMIT
      INTEGER   JUV, JMOD, JPHAS, JGRID, JINTP, JCX, JCY,
     *   JG, JJLOOP, JNO2, JN, ICX, ICY, IG
      INTEGER   INCX(121), INCY(121), INCG(121), OLDM, OLDLRO
      REAL      X, XX, Y, SUMRE, SUMIM, WT, REMOD, IMMOD
      INCLUDE 'INCS:DAPC.INC'
      SAVE OLDM, OLDLRO, INCX, INCY, INCG
C-----------------------------------------------------------------------
      IRND(XX) = INT (XX + SIGN (0.5, XX))
      IF (NMOD.LE.0) GO TO 999
C                                       Get 1 rel. locations
      JUV = UV + 1
      JMOD = MOD + 1
      JPHAS = PHAS + 1
      JGRID = GRID + 1
      JINTP = INTP + 1
      JNO2 = M / 2
      JN = M
      HAF = LROW / 2 - JNO2
      INCR = 2 * LROW
      LIMIT = M * JN
      IF ((OLDM.EQ.M) .AND. (OLDLRO.EQ.LROW)) GO TO 70
C                                       Fill increment tables
      OLDM = M
      OLDLRO = LROW
      DO 50 LOOP = 1,LIMIT
         IY = (LOOP-1) / JN
         IX = (LOOP-1) - IY * JN
         INCX(LOOP) = IX * 200
         INCY(LOOP) = IY * 200
         INCG(LOOP) = IX * 2 + IY * INCR
 50      CONTINUE
C                                       Loop over visibilities.
 70   DO 300 JJLOOP = 1,NMOD
C                                        Determine location.
         X = APCORE(JUV+1)
         Y = APCORE(JUV)
C                                        Deter. interp. fn loc.
         JCX = IRND (200. * (IRND (X) - X - 0.5))
         JCY = IRND (200. * (IRND (Y) - Y - 0.5))
         JCX = JINTP + 200 + JCX
         JCY = JINTP + 200 + JCY
C                                        Determine grid loc.
         Y = Y - ROW
         JG = IRND (X) + IRND (Y) * LROW
         JG = JGRID + 2 * (JG + HAF)
C                                        Init. sums.
         SUMRE = 0.0
         SUMIM = 0.0
C                                       Unroll inter loop with gather.
C                                       Set pointer arrays for gather.
         DO 100 LOOP = 1,LIMIT
            ICX = JCX + INCX(LOOP)
            ICY = JCY + INCY(LOOP)
            IG = JG + INCG(LOOP)
C                                        Interpolation
            WT = APCORE(ICX) * APCORE(ICY)
            SUMRE = SUMRE + APCORE(IG) * WT
            SUMIM = SUMIM + APCORE(IG+1) * WT
 100        CONTINUE
C                                        Correct phase.
         REMOD = APCORE(JPHAS) * SUMRE - APCORE(JPHAS+1) * SUMIM
         IMMOD = APCORE(JPHAS) * SUMIM + APCORE(JPHAS+1) * SUMRE
         APCORE(JMOD) = REMOD
         APCORE(JMOD+1) = IMMOD
C                                       Update for next model.
         JUV = JUV + INC
         JMOD = JMOD + 4
         JPHAS = JPHAS + 4
 300     CONTINUE
C
 999  RETURN
      END
