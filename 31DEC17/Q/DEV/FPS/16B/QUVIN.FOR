      SUBROUTINE QUVIN (UV, VIS, WORK, L, M, LROW, NS, INCS, NF, INCF,
     *   CNT, FLAG)
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
C   Interpolates model visibility values from a grid, corrects the
C   model for the phase of the z*w term and adds the model to the data.
C    Inputs:
C      UV    I  Base address of U,V vector
C      VIS   I  Base address of visibility
C      WORK  I  Base address of work vector length CNT*4 words
C      L     I  Length of visibility record
C      M     I  Width of interpolation fn support size,  also
C                 the number of rows kept in AP (must be odd)
C      LROW  I  length of a row (V)
C      NS    I  Number of correlators (Stokes' parm.) to process.
C      INCS  I  Increment between correlators.
C      NF    I  Number of frequencies to process.
C      INCF  I  Increment between frequencies.
C      CNT   I  Number of visibility points.
C      FLAG  I  1 => add model, -1 => add i (SQRT(-1)) times
C                 model.
C   Also expects necessary constants in following AP locations:
C          0 = U scaling to cells.
C          1 = V scaling to cells
C          2 = DXC = -2 * Pi * delta RA (in cells)
C          3 = DYC = -2 * Pi * delta DEC (cells)
C          4 = DZC = -2 * Pi * delta Z (in radians) for 1st
C              frequency channel.
C          5 = Used as work space.
C          6 = Used as work space.
C          7 = ROW = lowest central row of the interpolation.
C          8 = G = Address of base address of grid (floating)
C          9 = CX = Address of base address of row interpolation
C              function (floating)
C          10 = Used as work space.
C          11 = Used as work space.
C          16...16+NS = an array of correlator factors.
C              To subtract the model these should be negative.
C          20...20+NF-2 = differential frequency scaling table
C               for channels after the first one.
C               FREQ(n) = (1+MD(19+n-1)) * FREQ(n-1) for n>1
C-----------------------------------------------------------------------
      INTEGER UV, VIS, WORK, L, M, LROW, NS, INCS, NF, INCF, CNT, FLAG
      INTEGER*2 IUV, IVIS, IWORK, IL, IM, ILROW, INS, IINCS,
     *   INF, IINCF, ICNT, IFLAG
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
      IF (M.LT.32768) IM = M
      IF (M.GE.32768) IM = M - 65536
      IF (LROW.LT.32768) ILROW = LROW
      IF (LROW.GE.32768) ILROW = LROW - 65536
      IF (NS.LT.32768) INS = NS
      IF (NS.GE.32768) INS = NS - 65536
      IF (INCS.LT.32768) IINCS = INCS
      IF (INCS.GE.32768) IINCS = INCS - 65536
      IF (NF.LT.32768) INF = NF
      IF (NF.GE.32768) INF = NF - 65536
      IF (INCF.LT.32768) IINCF = INCF
      IF (INCF.GE.32768) IINCF = INCF - 65536
      IF (CNT.LT.32768) ICNT = CNT
      IF (CNT.GE.32768) ICNT = CNT - 65536
      IFLAG = FLAG
C                                        Call FPS routine.
      CALL UVINT (IUV, IVIS, IWORK, IL, IM, ILROW, INS, IINCS,
     *   INF, IINCF, ICNT, IFLAG)
C
 999  RETURN
      END
