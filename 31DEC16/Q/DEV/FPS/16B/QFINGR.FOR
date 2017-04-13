      SUBROUTINE QFINGR (U, ROW, MO2, LROW, TYPE)
C-----------------------------------------------------------------------
C! FPS AP version: Finished gridding row
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
C   FINGRD Does various tasks associated with completion of gridding
C   a row.  If U is within 1/2 support of 0 the symmetric row is
C   conjugated, flipped and added.  Next two conjugate rows are
C   formed, vis1 + i*vis2 (for beams, vis2 is the sampling function=
C   the sum of the weights).  Finally rows are rotated so that
C   zero column (assumed LROW/2+1) goes to the first column.
C   If U=0 the space for the next row down is used.
C    Inputs:
C     U     I  U in cells (non-negative)
C     ROW   I  Base address of Grid row of interest
C     MO2   I  Half the number of rows kept in the AP.
C     LROW  I  Length of row (no. vis)
C     TYPE  I  1 for IPOL,IBEM maps
C                2 for Q,U maps
C                3 for V maps.
C                4 for two line maps.
C     Also expects necessary constants in following AP locations:
C           0 = COS(PHASE0)           to shift map center
C           1 = SIN(PHASE0)
C           2 = COS(DELPHR)           for rotating down rows
C           3 = SIN(DELPHR)
C           4 = COS(DELPHC)           for rotating down columns
C           5 = SIN(DELPHC)
C           6 = 1.0
C           7 = 0.0
C-----------------------------------------------------------------------
      INTEGER U, ROW, MO2, LROW, TYPE
      INTEGER*2 IU, IROW, IMO2, ILROW, ITYPE
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (U.LT.32768) IU = U
      IF (U.GE.32768) IU = U - 65536
      IF (ROW.LT.32768) IROW = ROW
      IF (ROW.GE.32768) IROW = ROW - 65536
      IF (MO2.LT.32768) IMO2 = MO2
      IF (MO2.GE.32768) IMO2 = MO2 - 65536
      IF (LROW.LT.32768) ILROW = LROW
      IF (LROW.GE.32768) ILROW = LROW - 65536
      ITYPE = TYPE
C                                        Call FPS routine.
      CALL FINGRD (IU, IROW, IMO2, ILROW, ITYPE)
C
 999  RETURN
      END
