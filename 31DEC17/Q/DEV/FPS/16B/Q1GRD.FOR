      SUBROUTINE Q1GRD (UV, VIS, WT, L, INCF, NT2, M,
     *   LROW, CNT, NFREQ)
C-----------------------------------------------------------------------
C! FPS AP version: Grids row of UV data
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
C   FPS AP 120B version
C   Grids visibility data that has been loaded into the AP
C   previously,  If requested, positions are shifted and then
C   a taper is applied to the weights if requested before gridding.
C   multiple frequency channels may be gridded together for
C   bandwidth synthesis.  Two work words are assumed before the
C   first visibility.
C     Inputs:
C       I   UV = base address of u,v vector
C       I   VIS = base address of visibility
C       I   WT = base address of weights
C       I   L  = length of visibility record
C       I   INCF = Increment between visibility, stepping in
C                    frequency.
C       I   NT2 = INT ( (no. cells used on a row)/2 )
C       I   M = number of rows kept in AP (MUST be odd)
C       I   LROW = length of a row (V)
C       I   CNT =  number of visibility points.
C       I   NFREQ  =  number of frequencies to grid.
C              If NFREQ is negative then tapering is requested.
C              If CNT is neg, do not shift data.
C     Expects necessary constants in following AP locations:
C           8 = Row number of lowest central row to grid
C           9 = -SIG(U)**2 (CELLS**2) for taper
C          10 = -SIG(V)**2 (CELLS**2) for taper
C          11 = U scaling to cells
C          12 = V scaling to cells
C          13 = W scaling to cells
C          14 = DXC = -2 * PI * delta RA (in 1/cells) for 1ST
C          15 = DYC = -2 * PI * delta DEC         frequency channel
C          16 = DZC = -2 * PI * delta Z
C          17 = AP address containing address of base address of row
C               convolving fn. (Y on sky)
C          18 = AP address containing address of base address of col.
C               convolving fn. (X on sky)
C          19 = AP address containing address of base address of grid.
C          20...20+NFREQ-2 = differential frequency scaling table
C               for channels after the first one.
C               FREQ(n) = (1+MD(16+n-1)) * FREQ(n-1) for n>1
C-----------------------------------------------------------------------
      INTEGER UV, VIS, WT, L, INCF, NT2, M, LROW, CNT, NFREQ
      INTEGER*2 IUV, IVIS, IWT, IL, IINCF, INT2, IM, ILROW,
     *   ICNT, INFREQ
C-----------------------------------------------------------------------
C                                       Convert to 2-comp. unsigned I*2
      IF (UV.LT.32768) IUV = UV
      IF (UV.GE.32768) IUV = UV - 65536
      IF (VIS.LT.32768) IVIS = VIS
      IF (VIS.GE.32768) IVIS = VIS - 65536
      IF (WT.LT.32768) IWT = WT
      IF (WT.GE.32768) IWT = WT - 65536
      IF (L.LT.32768) IL = L
      IF (L.GE.32768) IL = L - 65536
      IF (INCF.LT.32768) IINCF = INCF
      IF (INCF.GE.32768) IINCF = INCF - 65536
      IF (NT2.LT.32768) INT2 = NT2
      IF (NT2.GE.32768) INT2 = NT2 - 65536
      IF (M.LT.32768) IM = M
      IF (M.GE.32768) IM = M - 65536
      IF (LROW.LT.32768) ILROW = LROW
      IF (LROW.GE.32768) ILROW = LROW - 65536
      IF (CNT.LT.32768) ICNT = CNT
      IF (CNT.GE.32768) ICNT = CNT - 65536
      IF (NFREQ.LT.32768) INFREQ = NFREQ
      IF (NFREQ.GE.32768) INFREQ = NFREQ - 65536
C                                       Call FPS routine
      CALL AP1GRD (IUV, IVIS, IWT, IL, IINCF, INT2, IM, ILROW,
     *   ICNT, INFREQ)
C
 999  RETURN
      END
