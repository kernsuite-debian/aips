      SUBROUTINE QINTG (UV, MOD, PHAS, GRID, INTP, M, LROW, INC, NMOD,
     *   ROW)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Interpolates model visibilityes from a grid.
C# AP-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2006, 2012
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
      INTEGER   UV, MOD, PHAS, GRID, INTP, M, LROW, INC, NMOD, ROW
C
      INTEGER   INCR, HAF, IRND, IX, IY, MO2, JJLOOP
      LONGINT   JUV, JMOD, JPHAS, JGRID, JINTP, JCX, JCY, JG, JJCX
      DOUBLE PRECISION X, XX, Y, SUMR, SUMI, REMOD, IMMOD, SUMRE, SUMIM
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IRND(XX) = INT (XX + SIGN (0.5D0, XX))
C-----------------------------------------------------------------------
C                                       Get 1 rel. locations
      JUV = UV + PSAPOF
      JMOD = MOD + PSAPOF
      JPHAS = PHAS + PSAPOF
      JGRID = GRID + PSAPOF
      JINTP = INTP + PSAPOF
      MO2 = M / 2
      HAF = LROW / 2 - MO2
      INCR = 2 * LROW - 2 * M
C                                        Loop over visibilities.
      INCLUDE 'INCS:ZVND.INC'
      DO 300 JJLOOP = 1,NMOD
C                                        Determine location.
         X = APCORE(JUV+1)
         Y = APCORE(JUV)
C                                        Deter. interp. fn loc.
         JCX = IRND (200.D0 * (IRND (X) - X - 0.5D0))
         JCY = IRND (200.D0 * (IRND (Y) - Y - 0.5D0))
         JCX = JCX + JINTP + 200
         JCY = JCY + JINTP + 200
C                                        Determine grid loc.
         Y = Y - ROW
         JG = 2 * (IRND (X) + HAF) + IRND (Y) * LROW * 2
         JG = JG + JGRID
C                                        Save JCX
         JJCX = JCX
C                                        Init. sums.
         SUMRE = 0.0D0
         SUMIM = 0.0D0
C                                        Interpolation loop
      INCLUDE 'INCS:ZVND.INC'
         DO 200 IY = 1,M
            JCX = JJCX
            SUMR = 0.0D0
            SUMI = 0.0D0
      INCLUDE 'INCS:ZVND.INC'
            DO 100 IX = 1,M
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
C                                        If phase is non-zero, Cos != 1
         IF (APCORE(JPHAS).NE.1.0) THEN
C                                        Correct phase.
            REMOD = (DBLE( APCORE(JPHAS))   * SUMRE) -
     *              (DBLE( APCORE(JPHAS+1)) * SUMIM)
            IMMOD = (DBLE( APCORE(JPHAS))   * SUMIM) +
     *              (DBLE( APCORE(JPHAS+1)) * SUMRE)
            APCORE(JMOD)   = REMOD
            APCORE(JMOD+1) = IMMOD
C                                       else no phase correction
         ELSE
            APCORE(JMOD)   = SUMRE
            APCORE(JMOD+1) = SUMIM
            END IF
C                                       Update for next model.
         JUV = JUV + INC
         JMOD = JMOD + 4
         JPHAS = JPHAS + 4
 300     CONTINUE
C
 999  RETURN
      END
