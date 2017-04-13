      SUBROUTINE REIMIO (BLC, TRC, IBSIZE, CATBLK, IMSTUF)
C-----------------------------------------------------------------------
C! Reinitialize for image I/O using INTMIO
C# Map IO-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2015
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
C   This routine will reinitialize the counters in IMSTUF for reading
C   another subimage of a map opened and set up with INTMIO.  All
C   IMSTUF values that can be found in the header are re-initialized
C   even if they are not changed by the standard routines.
C   Inputs:
C      BLC      R(7)    Bottom left corner of map.
C      TRC      R(7)    Top right corner of map
C      IBSIZE   I       Size of IO buffer in words.
C      CATBLK   I(256)  Map header.
C      IMSTUF   I(*)    (1) LUN
C                       (2) FTAB index
C                       (5) Catalog slot of image.
C                       (6) Size of IO buffer in bytes of all things.
C                       (7) Volume number of image.
C   Outputs:
C      IMSTUF   I(*)    (8) Number of dimensions in image.
C                       (9) Number of values read per row of image.
C                       (10-16)  Number of values along all 7 axes
C                       (17-30)  Window in BLC TRC pairs along all 7
C                                axes.
C                       (31-36)  Current position on last six axis.
C                       (37) 1 if read forward, -1 if backward read on
C                            2nd axis.
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7)
      INTEGER   IMSTUF(37), CATBLK(256), IBSIZE
C
      INTEGER   I0, I1, I
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Set window defaults. Vectors
C                                       (blcx>trcx) considered ok.
      CALL VECWIN (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC)
C                                       Set values for IMSTUF
      IMSTUF(6) = IBSIZE * 2
      IMSTUF(8) = CATBLK(KIDIM)
      CALL COPY (7, CATBLK(KINAX), IMSTUF(10) )
      I0 = 17
      I1 = 18
      DO 10 I = 1,7
         IMSTUF(I0) = BLC(I) + .0001
         IMSTUF(I1) = TRC(I) + .9999
         I0 = I0 + 2
         I1 = I1 + 2
 10      CONTINUE
      DO 20 I = 1,6
         IMSTUF(30+I) = BLC(I+1) + .0001
 20      CONTINUE
      IMSTUF(37) = 1
      IF (IMSTUF(19).GT.IMSTUF(20)) IMSTUF(37) = -1
C                                       Fix window for IO if this is
C                                       a vector.
      IF (IMSTUF(17).GT.IMSTUF(18)) THEN
         I = IMSTUF(18)
         IMSTUF(18) = IMSTUF(17)
         IMSTUF(17) = I
         END IF
      IMSTUF(9) = ABS(IMSTUF(18) - IMSTUF(17)) + 1
C                                       Set counters so that an MINIT
C                                       occurs on first call to IO.
      IMSTUF(31) = IMSTUF(20)
      IF (IMSTUF(21).LE.IMSTUF(22)) IMSTUF(32) = IMSTUF(32) - 1
      IF (IMSTUF(21).GT.IMSTUF(22)) IMSTUF(32) = IMSTUF(32) + 1
C
 999  RETURN
      END
