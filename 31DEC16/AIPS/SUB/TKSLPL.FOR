      SUBROUTINE TKSLPL (ISLUN, ISFIND, RANGE2, BLC, TRC, IERR)
C-----------------------------------------------------------------------
C! plot a slice on graphics device
C# Slice Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2007
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
C   Given an open slice file and an open TEK device, TKSLPL will draw
C   the slice.
C   Inputs:
C      ISLUN   I       logical unit no. of open slice file.
C      ISFIND  I       FTAB index for slice file.
C      RANGE2  R(2)    min and max pixel value allowed for plot.
C      BLC     R(7)    bottom left corner of the plot.
C      TRC     R(7)    top right corner of plot; value 1 - 4000
C   Output:
C      IERR    I       error code: 0 = none.
C                         1 = error during graphing. Try finish.
C                         2 = fatal error. Give up.
C   Common
C      /WORK/ SBLK  R(256)  I/O buffer for slice file.
C-----------------------------------------------------------------------
      INTEGER   ISLUN, ISFIND, IERR
      REAL      RANGE2(2), BLC(7), TRC(7)
C
      REAL      X, Y, SCALEF, OFFSET, SBLK(1)
      INTEGER   ISTART, IEND, IRRN, IPOS, ILOW, INP, ISL, NRPBLK
      LOGICAL   BLAST, BNEXT
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /WORK/ SBLK
C-----------------------------------------------------------------------
C                                       Draw plot.
      NRPBLK = 256
      SCALEF = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      OFFSET = 40000.0 - SCALEF * CATR(IRRAN+1)
      ISTART = BLC(1) + .5
      IEND = TRC(1) + .5
      IRRN = 3 + (ISTART - 1) / NRPBLK
      IPOS = ISTART - ((IRRN - 3) * NRPBLK)
C                                       Set range parameters.
      ILOW = RANGE2(1) + .5
C                                       Read first data.
      CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, SBLK, IERR)
      IF (IERR.NE.0) GO TO 930
C                                       Draw first data pt.
      X = ISTART
      Y = SCALEF * SBLK(IPOS)  +  OFFSET
      BLAST = SBLK(IPOS).EQ.FBLANK
      Y = MIN (Y, RANGE2(2))
      Y = MAX (Y, RANGE2(1))
      CALL TEKVEC (X, Y, 1, IERR)
      IF (IERR.NE.0) GO TO 950
C                                       Do rest of data points.
      ISTART = ISTART + 1
      DO 20 ISL = ISTART,IEND
         IPOS = IPOS + 1
C                                       Read from slice file.
         IF (IPOS.LE.NRPBLK) GO TO 10
            IRRN = IRRN + 1
            CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, SBLK, IERR)
            IF (IERR.NE.0) GO TO 930
            IPOS = 1
C                                       Draw to next slice pt.
 10      X = ISL
         Y = SCALEF * SBLK(IPOS)  +  OFFSET
         BNEXT = SBLK(IPOS).EQ.FBLANK
         Y = MIN (Y, RANGE2(2))
         Y = MAX (Y, RANGE2(1))
         INP = 2
         IF ((BNEXT) .OR. (BLAST)) INP = 1
         BLAST = BNEXT
         CALL TEKVEC (X, Y, INP, IERR)
         IF (IERR.NE.0) GO TO 950
 20      CONTINUE
C                                       Force alpha mode to allow
C                                       screen to auto-dim
      ISTART = 31
      CALL ZTKBUF (ISTART, 1, IERR)
C                                       Write final TEK buffer.
      CALL TEKFLS (IERR)
      IF (IERR.EQ.0) GO TO 999
      GO TO 950
C                                       Slice read error.
 930  IERR = 1
      WRITE (MSGTXT,1930)
      CALL MSGWRT (6)
      GO TO 999
C                                       TEK write error.
 950  IERR = 2
      WRITE (MSGTXT,1950)
      CALL MSGWRT (7)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1930 FORMAT ('SLICE FILE READ ERROR.')
 1950 FORMAT ('TEKTRONIX 4012 WRITE ERROR.')
      END
