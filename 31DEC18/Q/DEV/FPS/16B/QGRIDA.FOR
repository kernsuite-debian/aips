      SUBROUTINE QGRIDA (UV, VIS, WT, L, G, CX, CY, CONI, NO2, M, LROW,
     *   CNT, TY)
C-----------------------------------------------------------------------
C! FPS AP version: Grids UV data
C# AP-util UV
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
C   FPS AP version
C   Grids visibility data that has been loaded into the AP
C   previously.  If requested, positions are shifted and then
C   a taper is applied to the weights if requested before gridding.
C   when taper is requested and TY=2 or 3 locations VIS-2 and
C   VIS-1 are used for work space. If TY=1 locations VIS+2 and
C   VIS+3 are used.
C    Inputs:
C     UV    I  Base address of U,V vector
C     VIS   I  Base address of visibility
C     WTP   I  Base address of weights
C     L     I  Length of visibility record
C     G     I  Base address of grid
C     CX    I  Base address of row convolving fn. (Y on sky)
C     CY    I  Base address of col. convolving fn. (X on sky)
C                Convolving fns. tabulated every 1/CONI cell
C     CONI  I  Increment of conv function tabulation (assumed 100 for
C               true visibility data)
C     NO2   I  INT ( (no. cells used on a row)/2 )
C     M     I  Number of rows kept in AP (must be odd)
C     LROW  I  Length of a row (V)
C     CNT   I  Number of visibility points.
C     TY    I  Type of visibility data.
C                       1 = I maps
C                       2 = Q,U maps, or two line maps
C                       3 = V maps
C                 If TY is negative then tapering is requested.
C                 If CNT is neg, do not shift data.
C     Also expects necessary constants in following AP locations:
C           8 = -SIG(U)**2 (CELLS**2) for taper
C           9 = -SIG(V)**2 (CELLS**2) for taper
C          10 = U scaling to cells
C          11 = V scaling to cells
C          12 = W scaling to cells
C          13 = DXC = -2 * Pi * delta RA (in 1/cells)
C          14 = DYC = -2 * Pi * delta DEC
C          15 = DZC = -2 * Pi * delta Z
C-----------------------------------------------------------------------
      INTEGER UV, VIS, WT, L, G, CX, CY, CONI, NO2, M, LROW, CNT, TY
      INTEGER*2 IUV, IVIS, IWT, IL, IG, ICX, ICY, INO2, IM, ILROW,
     *   ICNT, ITY
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (UV.LT.32768) IUV = UV
      IF (UV.GE.32768) IUV = UV - 65536
      IF (VIS.LT.32768) IVIS = VIS
      IF (VIS.GE.32768) IVIS = VIS - 65536
      IF (WT.LT.32768) IWT = WT
      IF (WT.GE.32768) IWT = WT - 65536
      IF (L.LT.32768) IL = L
      IF (L.GE.32768) IL = L - 65536
      IF (G.LT.32768) IG = G
      IF (G.GE.32768) IG = G - 65536
      IF (CX.LT.32768) ICX = CX
      IF (CX.GE.32768) ICX = CX - 65536
      IF (CY.LT.32768) ICY = CY
      IF (CY.GE.32768) ICY = CY - 65536
      IF (NO2.LT.32768) INO2 = NO2
      IF (NO2.GE.32768) INO2 = NO2 - 65536
      IF (M.LT.32768) IM = M
      IF (M.GE.32768) IM = M - 65536
      IF (LROW.LT.32768) ILROW = LROW
      IF (LROW.GE.32768) ILROW = LROW - 65536
      IF (CNT.LT.32768) ICNT = CNT
      IF (CNT.GE.32768) ICNT = CNT - 65536
      ITY = TY
C                                        Call FPS routine.
      CALL GRIDAP (IUV, IVIS, IWT, IL, IG, ICX, ICY, INO2, IM, ILROW,
     *   ICNT, ITY)
C
 999  RETURN
      END
