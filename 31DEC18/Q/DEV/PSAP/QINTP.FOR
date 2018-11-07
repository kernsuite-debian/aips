      SUBROUTINE QINTP (UV, VIS, PHAS, GRID, INTP, M, LROW, INC, NVIS)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Interpolates model visibilities from a grid.
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
C   FORTRAN version of FPS Array processor microcode.
C   QINTP Interpolates model visibilities from a grid,
C   corrects the phase and subtracts from the observations.
C   Assumes that the observations are never within M/2
C   of the outside edge of the grid.
C   Inputs:
C      UV    I  Base address of (u,v) values in cells.
C      VIS   I  Base address of (complex) visibilities.
C      PHAS  I  Base address of phase correction, complex value
C               to be multiplied by model vis.
C      GRID  I  Base address of gridded model vis. Order assumed to
C               be the following  for each of the M rows:
C                              1) 2 * LROW visibilities
C      INTP  I  Base address of interpolation function.
C      M     I  Number of rows kept in the AP.  Also support size of
C               interp. fn.
C      LROW  I  Length of a row.
C      INC   I  Increment for UV,VIS and PHAS
C      NVIS  I  Number of visibility points to grid.
C   All AP memory values are assumed floating.
C   It is assumed that all values of v correspond to row M/2.
C-----------------------------------------------------------------------
      INTEGER   UV, VIS, PHAS, GRID, INTP, M, LROW, INC, NVIS
C
      LONGINT   JUV, JVIS, JPHAS, JGRID, JINTP, JCX, JCY, JG, JJCX
      INTEGER   INCR, HAF, IRND, IX, IY, JNO2, JN, JJLOOP
      DOUBLE PRECISION X, XX, Y, SUMRE, SUMIM, REMOD, IMMOD, SUMR, SUMI
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IRND(XX) = INT (XX + SIGN (0.5D0, XX))
C-----------------------------------------------------------------------
C                                       Convert addresses to 1 rel.
      JUV = UV + PSAPOF
      JVIS = VIS + PSAPOF
      JPHAS = PHAS + PSAPOF
      JGRID = GRID + PSAPOF
      JINTP = INTP + PSAPOF
      JNO2 = M / 2
      JN = M
      HAF = LROW / 2 - JNO2
      INCR = 2 * LROW - 2 * JN
C                                        Loop over visibilities.
      INCLUDE 'INCS:ZVND.INC'
      DO 300 JJLOOP = 1,NVIS
C                                        Determine location.
         X = APCORE(JUV+1)
         Y = APCORE(JUV)
C                                        Deter. interp. fn loc.
         JCX = IRND (200.D0 * (IRND (X) - X - 0.5D0))
         JCY = IRND (200.D0 * (IRND (Y) - Y - 0.5D0))
         JCX = JCX + JINTP + 200
         JCY = JCY + JINTP + 200
C                                        Determine grid loc.
         JG = 2 * (IRND (X) + HAF)
         JG = JG + JGRID
C                                        Save JCX
         JJCX = JCX
C                                        Init. sums.
         SUMRE = 0.0
         SUMIM = 0.0
C                                        Interpolation loop
      INCLUDE 'INCS:ZVND.INC'
         DO 200 IY = 1,M
            JCX = JJCX
            SUMR = 0.0D0
            SUMI = 0.0D0
      INCLUDE 'INCS:ZVND.INC'
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
C                                        Subtract from data
         APCORE(JVIS) = APCORE(JVIS) - REMOD
         APCORE(JVIS+1) = APCORE(JVIS+1) - IMMOD
C                                       Update for next vis.
         JUV = JUV + INC
         JVIS = JVIS + INC
         JPHAS = JPHAS + INC
 300     CONTINUE
C
 999  RETURN
      END
