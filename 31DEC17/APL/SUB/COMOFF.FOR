      SUBROUTINE COMOFF (NDIM, NAX, DEPTH, BLKOF, IERR)
C-----------------------------------------------------------------------
C! determines start block number of a plane in an N-dimensional image
C# Map-util IO-util
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
C   Compute the block offset of a 2-D map plane in a NDIM-dimensional
C   map from the beginning of the map.
C   Inputs:
C      NDIM      I   Number of axes in map
C      NAX(7)    I   Number of pixels on each axis
C      DEPTH(5 ) I   Depth of required plane along other axes
C   Outputs:
C      BLKOF     I   Block offset
C      IERR      I   Error return 0 = OK, 1= error in NDIM
C-----------------------------------------------------------------------
      INTEGER   NDIM, NAX(7), DEPTH(5), BLKOF, IERR
      INTEGER   WRK, WRK2, I
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       check NDIM
      IERR = 1
      IF ((NDIM.GE.1) .AND. (NDIM.LE.7)) GO TO 10
         WRITE (MSGTXT,1000) NDIM
         CALL MSGWRT (8)
         GO TO 999
C                                       2-dim: BLKOF = 0
 10   IERR = 0
      BLKOF = 0
      IF ((NDIM.LE.2) .OR. (DEPTH(1).EQ.0)) GO TO 999
C                                       # sectors/plane
      I = NBPS / (NAX(1) * 2)
      IF (I.GT.0) THEN
         WRK2 = (NAX(2) - 1)/ I + 1
      ELSE
         WRK = (NAX(1) * 2 - 1) / NBPS + 1
         WRK2 = NAX(2)
         WRK2 = WRK * WRK2
         END IF
C                                       # planes deep
 30   I = NDIM
      BLKOF = DEPTH(I-2) - 1
      IF (BLKOF.LT.0) BLKOF = 0
 35   I = I - 1
      IF (I.LE.2) GO TO 40
         WRK = NAX(I)
         IF (WRK.LE.0) WRK = 1
         BLKOF = BLKOF * WRK
         WRK = DEPTH(I-2) - 1
         IF (WRK.LT.0) WRK = 0
         BLKOF = BLKOF + WRK
         GO TO 35
C                                       # sectors deep
 40   BLKOF = BLKOF * WRK2
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COMOFF: I DONT WORK FOR NDIM =',I6)
      END
