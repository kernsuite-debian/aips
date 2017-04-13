      SUBROUTINE QUVINT (UV, VIS, WORK, L, G, CX, M, LROW, NFREQ,
     *   CNT, SCR)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Interpolate model visibility from grid.
C# AP-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006
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
C   Interpolates model visibility values from a grid, corrects the model
C   for the phase of the z*w term and subtracts the model from the data.
C   Will loop over several frequency channels.  Two words of work space
C   are assumed in front of the visibilities.
C   Inputs:
C      UV    I  Base address of U,V vector
C      VIS   I  Base address of visibility
C      WORK  I  Base address of complex work vector length CNT
C      L     I  Length of visibility record
C      G     I  Base address of grid
C      CX    I  Base address of row interpolation function
C               interpolation fns tabulated every 1/200 cell.
C      M     I  Width of interpolation fn support size,  also
C               the number of rows kept in AP (must be odd)
C      LROW  I  length of a row (V)
C      NFREQ I  Number of frequencies to process.
C      CNT   I  Number of visibility points.
C      SCR   I  Base address of scratch complex vector
C               in each vis rec, inc=L.
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
      INTEGER   UV, VIS, WORK, L, G, CX, M, LROW, NFREQ, CNT, SCR
C
      INTEGER   UV1, UV2, IWORK1, IWORK2, IFQ, VVIS, LOOP
C-----------------------------------------------------------------------
      IF (CNT.LE.0) GO TO 999
C                                       Setup
      UV1 = UV + 1
      UV2 = UV1 + 1
      VVIS = VIS
      IWORK1 = SCR
      IWORK2 = IWORK1 + 1
C                                       Save u,v
      CALL QCVMOV (UV, L, WORK, 2, CNT)
C                                       Init. w scaling.
      CALL QCVMOV (4, 2, 5, 2, 1)
C                                       Scale u,v,w to cells.
      CALL QVSMUL (UV, L, 0, UV, L, CNT)
      CALL QVSMUL (UV1, L, 1, UV1, L, CNT)
C                                       Frequency looping, rescale uv
      DO 200 LOOP = 1,NFREQ
C                                       Don't rescale on first pass
         IF (LOOP.NE.1) THEN
            IFQ = 14 + LOOP
            CALL QVSMA (UV, L, IFQ, UV, L, UV, L, CNT)
            CALL QVSMA (UV1, L, IFQ, UV1, L, UV1, L, CNT)
            CALL QVSMA (5, 1, IFQ, 5, 1, 5, 1, 1)
            END IF
C                                       Shift position (z*w) term.
         CALL QVSMUL (UV2, L, 5, IWORK1, L, CNT)
         CALL QVSMA (UV, L, 2, IWORK1, L, IWORK1, L, CNT)
         CALL QVSMA (UV1, L, 3, IWORK1, L, IWORK1, L, CNT)
         CALL QVSIN (IWORK1, L, IWORK2, L, CNT)
         CALL QVCOS (IWORK1, L, IWORK1, L, CNT)
C                                       Interpolate
         CALL QINTP (UV, VVIS, IWORK1, G, CX, M, LROW, L, CNT)
         VVIS = VVIS + 2
 200     CONTINUE
C                                       Restore u,v
      CALL QCVMOV (WORK, 2, UV, L, CNT)
C
 999  RETURN
      END
