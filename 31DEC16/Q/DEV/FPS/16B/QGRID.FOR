      SUBROUTINE QGRID (UV, VIS, WT, L, G, CX, CY, NO2, M, LROW, CNT,
     *   TY)
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
C-----------------------------------------------------------------------
C   FPS AP version
C   Grids visibility data that has been loaded into the AP
C   previously.  Data is floated and scaled; a taper is applied
C   to the weights if requested before gridding.
C   When taper is requested and TY=2 or 3 locations VIS-2 and
C   VIS-1 are used for work space.  If TY=1 locations VIS+2 and
C   VIS+3 are used.
C    Inputs:
C     UV    I   Location of (u,v) values in cells.
C     VIS   I   Location of (complex) visibilities.
C     WT    I   Weight for data.
C     L     I   Length of visibility record.
C     G     I   base address of gridded data.
C     CX    I   base address of X convolving fn. (Y on sky)
C     CY    I   base address of Y convolving fn. (X on sky)
C     NO2   I   INT( (# cells used on a row) / 2 )
C     M     I   number of rows kept in the AP.
C     LROW  I   length of a row ( max. X).
C     CNT   I   Number of visibility points.
C     TY    I   Type of visibility data.
C                  1 = I MAPS
C                  2 = Q,U MAPS
C                  3 = V MAPS
C                 If TY is negative then tapering is requested.
C                 If CNT is neg., do not float or scale.
C
C     Expects necessary constants in following AP locations:
C
C           8 = -SIG(U)**2 (CELLS**2) FOR TAPER
C           9 = -SIG(V)**2 (CELLS**2) FOR TAPER
C          13 = UVSC (scaling for u,v)
C          14 = SCVIS  (scaling for visibilities)
C          15 = SCWT   (scaling for weights)
C     It is assumed that all values of v correspond to row M/2.
C-----------------------------------------------------------------------
      INTEGER UV, VIS, WT, L, G, CX, CY, NO2, M, LROW, CNT, TY
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
      IF (TY.LT.32768) ITY = TY
      IF (TY.GE.32768) ITY = TY - 65536
C                                        Call FPS routine.
      CALL APGRID (IUV, IVIS, IWT, IL, IG, ICX, ICY, INO2, IM, ILROW,
     *   ICNT, ITY)
C
 999  RETURN
      END
