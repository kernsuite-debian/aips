      SUBROUTINE QUVINT (UV, VIS, WORK, L, G, CX, M, LROW, NFREQ,
     *   CNT, SCR)
C-----------------------------------------------------------------------
C! FPS AP version: Interpolate model visibility from grid
C# AP-util
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
C   Interpolates model visibility values from a grid, corrects the model
C   for the phase of the z*w term and subtracts the model from the data.
C   Will loop over several frequency channels.  Two words of work space
C   are assumed in front of the visibilities.
C    Inputs:
C      UV    I  Base address of U,V vector
C      VIS   I  Base address of visibility
C      WORK  I  Base address of complex work vector length CNT
C      L     I  Length of visibility record
C      G     I  Base address of grid
C      CX    I  Base address of row interpolation function
C                 interpolation fns tabulated every 1/200 cell.
C      M     I  Width of interpolation fn support size,  also
C                 the number of rows kept in AP (must be odd)
C      LROW  I  length of a row (V)
C      NFREQ I  Number of frequencies to process.
C      CNT   I  Number of visibility points.
C      SCR   I  Base address of scratch complex vector
C                 in each vis rec, inc=L.
C   NOTE: only really works for one frequency (unless they are
C   very close)
C     Also expects necessary constants in following AP locations:
C          0 = U scaling to cells
C          1 = V scaling to cells
C          2 = DXC = -2 * Pi * delta RA (in cells)
C          3 = DYC = -2 * Pi * delta DEC (cells)
C          4 = DZC = -2 * Pi * delta Z (in radians) for 1st
C              frequency channel.
C          5 = Used as work space.
C          6 = Used as work space.
C          16...16+NFREQ-2 = Differential frequency scaling table
C               for channels after the first one.
C               FREQ(n) = (1+MD(16+n-1)) * FREQ(n-1) FOR n>1
C-----------------------------------------------------------------------
      INTEGER UV, VIS, WORK, L, G, CX, M, LROW, NFREQ, CNT, SCR
      INTEGER*2 IUV, IVIS, IWORK, IL, IG, ICX, IM, ILROW, INFREQ,
     *   ICNT, ISCR
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (UV.LT.32768) IUV = UV
      IF (UV.GE.32768) IUV = UV - 65536
      IF (VIS.LT.32768) IVIS = VIS
      IF (VIS.GE.32768) IVIS = VIS - 65536
      IF (WORK.LT.32768) IWORK = WORK
      IF (WORK.GE.32768) IWORK = WORK - 65536
      IF (L.LT.32768) IL = L
      IF (L.GE.32768) IL = L - 65536
      IF (G.LT.32768) IG = G
      IF (G.GE.32768) IG = G - 65536
      IF (CX.LT.32768) ICX = CX
      IF (CX.GE.32768) ICX = CX - 65536
      IF (M.LT.32768) IM = M
      IF (M.GE.32768) IM = M - 65536
      IF (LROW.LT.32768) ILROW = LROW
      IF (LROW.GE.32768) ILROW = LROW - 65536
      IF (NFREQ.LT.32768) INFREQ = NFREQ
      IF (NFREQ.GE.32768) INFREQ = NFREQ - 65536
      IF (CNT.LT.32768) ICNT = CNT
      IF (CNT.GE.32768) ICNT = CNT - 65536
      IF (SCR.LT.32768) ISCR = SCR
      IF (SCR.GE.32768) ISCR = SCR - 65536
C                                        Call FPS routine.
      CALL UVINTP (IUV, IVIS, IWORK, IL, IG, ICX, IM, ILROW, INFREQ,
     *   ICNT, ISCR)
C
 999  RETURN
      END
