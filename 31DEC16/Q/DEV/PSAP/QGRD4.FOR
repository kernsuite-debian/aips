      SUBROUTINE QGRD4 (UV, VIS, WT, GRID, CONX, CONY, NO2, M, LROW,
     *   ROW, INC, NVIS)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Convolves visibility data onto a grid.
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
C   Convolves visibility data onto a grid.
C   A single channel is gridded at a time.
C   It assumes that NO points lie within one half the
C   convolving function support size of the outside edge.
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
C      LROW  I  length of a (in reals) row ( max. X).
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
      INTEGER   UV, V  IS, WT, GRID, CONX, CONY, NO2, M, LROW, ROW, INC,
     *   NVIS
C
      LONGINT   JUV, JVIS, JWT, JGRID, JCONX, JCONY, JCX, JCY,
     *   JG, JJCX, JJLOOP
      INTEGER   IRND, N, INCR, HAF, IX, IY
      DOUBLE PRECISION AIM, RE, RRE, AAIM, X, XX, XWT, Y, RROW
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
      RROW = APCORE(ROW+PSAPOF)
      N = NO2 * 2 + 1
      HAF = LROW / 2 - NO2
      INCR = 2 * LROW - 2 * N
C                                        Loop over visibilities.
      INCLUDE 'INCS:ZVND.INC'
      DO 300 JJLOOP = 1,NVIS
C                                        Check weight.
         XWT = APCORE(JWT)
         IF (XWT.GT.0.0D0) THEN
C                                        Determine location.
            X = APCORE(JUV+1)
            Y = APCORE(JUV)
C                                        Deter. conv. fn loc.
            JCX = IRND (100.D0 * (IRND (X) - X - 0.5D0))
            JCY = IRND (100.D0 * (IRND (Y) - Y - 0.5D0))
            JCX = JCX + JCONX + 100
            JCY = JCY + JCONY + 100
C                                        Determine grid loc.
            Y = Y - RROW
            JG = 2 * (IRND (X) + HAF) + IRND (Y) * LROW * 2
            JG = JG + JGRID
C                                        Save JCX
            JJCX = JCX
C                                        Get visibility
            RE = APCORE(JVIS) * XWT
            AIM = APCORE(JVIS+1) * XWT
C                                        Gridding loop
      INCLUDE 'INCS:ZVND.INC'
            DO 200 IY = 1,M
               JCX = JJCX
               RRE = RE * APCORE(JCY)
               AAIM = AIM * APCORE(JCY)
      INCLUDE 'INCS:ZVND.INC'
               DO 100 IX = 1,N
C                                        Sum to grid.
                  APCORE(JG) = APCORE(JG) + APCORE(JCX) * RRE
                  APCORE(JG+1) = APCORE(JG+1) + APCORE(JCX) * AAIM
C                                        Update pointers.
                  JCX = JCX + 100
                  JG = JG + 2
 100              CONTINUE
C                                        Update pointers.
               JCY = JCY + 100
               JG = JG + INCR
 200           CONTINUE
            END IF
C                                       Update for next vis.
         JUV = JUV + INC
         JVIS = JVIS + INC
         JWT = JWT + INC
 300     CONTINUE
C
 999  RETURN
      END
