@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QINTG (UV, MOD, PHAS, GRID, INTP, M, LROW, INC, NMOD,
     *   ROW)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Interpolates model visibilityes from a grid.
C# AP-appl UV
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
C   FORTRAN version of FPS Array processor microcode.
C   QINTG Interpolates model visibilities from a grid,
C   corrects the phase and returns the value in the array
C   pointed to by MOD.
C   Assumes that the observations are never within M/2
C   of the outside edge of the grid.
C   Inputs:
C      UV    I  Base address of (u,v) values in cells.
C      MOD   I  Base address for resultant model. (increment=4)
C      PHAS  I  Phase correction, complex value to be multiplied by
C               model vis. Increment = 4.
C      GRID  I  GRID = address of base address of gridded model vis.
C               Order assumed to be the  following  for each of the
C               M rows:
C                  1) 2 * LROW visibilities
C      INTP  I  address of base address of interpolation function.
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
     *   INCR, HAF, IFIX, IRND, IX, IY
      INTEGER   JUV, JMOD, JPHAS, JGRID, JINTP, JCX, JCY,
     *   JG, JJCX, JJLOOP, JNO2, JN
      REAL      X, XX, Y, SUMRE, SUMIM, RROW,
     *   REMOD, IMMOD, SUMR, SUMI
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C     IRND(XX) = INT (XX + SIGN (0.5, XX))
      IRND(XX) = NINT(XX)
C                                       Get 1 rel. locations
      JUV = UV + 1
      JMOD = MOD + 1
      JPHAS = PHAS + 1
      JGRID = GRID + 1
      JGRID = APCORE (JGRID) + 1.5
      JINTP = INTP+1
      JINTP = APCORE (JINTP) + 1.5
      RROW = APCORE(ROW+1)
      JNO2 = M / 2
      JN = M
      HAF = LROW / 2 - JNO2
      INCR = 2 * LROW - 2 * JN
C                                        Loop over visibilities.
      DO 300 JJLOOP = 1,NMOD
C                                        Determine location.
         X = APCORE(JUV+1)
         Y = APCORE(JUV)
C                                        Deter. interp. fn loc.
         JCX = JINTP + IRND (200. * (IRND (X) - X - 0.5)) + 200
         JCY = JINTP + IRND (200. * (IRND (Y) - Y - 0.5)) + 200
C                                        Determine grid loc.
         Y = Y - RROW
         JG = JGRID + 2 * (IRND (X) + HAF) + IRND (Y) * LROW * 2
C                                        Save JCX
         JJCX = JCX
C                                        Init. sums.
         SUMRE = 0.0
         SUMIM = 0.0
C                                        Interpolation loop
         DO 200 IY = 1,M
            JCX = JJCX
            SUMR = 0.0
            SUMI = 0.0
C@DIR PREFER SCALAR
            DO 100 IX = 1,JN
C                                        Interpolate.
               SUMR = SUMR + APCORE(JG) * APCORE(JCX)
               SUMI = SUMI + APCORE(JG+1) * APCORE(JCX)
C                                        Update pointers.
               JCX = JCX + 200
               JG = JG + 2
 100           CONTINUE
C                                       Update sums
            SUMRE = SUMRE + SUMR * APCORE(JCY)
            SUMIM = SUMIM + SUMI * APCORE(JCY)
C                                        Update pointers.
            JCY = JCY + 200
            JG = JG + INCR
 200        CONTINUE
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
