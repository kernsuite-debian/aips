      SUBROUTINE TKRSPL (INPTS, IFMOD, IMOD, ISLUN, ISFIND, RANGE2, BLC,
     *   TRC, IERR)
C-----------------------------------------------------------------------
C! plots residuals between slice and its model on Graphics device
C# Slice Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2007, 2016
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
C   Given an open slice file and an open TEK device, TKRSPL will draw
C   the residuals of the slice and the slice model.
C   Inputs:
C      INPTS   I     number of points in the slice.
C      IFMOD   I     first 512 byte block containing model data.
C      IMOD    I     number of gaussian model data blocks in file.
C      ISLUN   I     logical unit no. of open slice file.
C      ISFIND  I     FTAB index for slice file.
C      ITLUN   I     logical unit no. of open TEKTRONIX device.
C      ITFIND  I     FTAB index for TEKTRONIX.
C      RANGE2  R(2)  min and max pixel value allowed for plot.
C      BLC     R(7)  the bottom left corner of the plot.
C      TRC     R(7)  the top right corner of the plot.
C   Output:
C      IERR    I     error code 0= none.
C                       1 = error during graphing. Try finish.
C                       2 = fatal error. Give up.
C-----------------------------------------------------------------------
      INTEGER   INPTS, IFMOD, IMOD, ISLUN, ISFIND, IERR
      REAL      RANGE2(2), BLC(7), TRC(7)
C
      REAL      X, Y, SCALEF, OFFSET, SBLK(256), PARM(12), YSLICE, XCEN,
     *   BASLIN(3)
      INTEGER   ISTART, IEND, IRRN, IPOS, INP, ISBLK(256), I, I0, I1,
     *   IC, IMAX, INDEX, IS, ISL, IWTH, NGAUSS, NRPBLK
      LOGICAL   BLAST, T, F
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /WORK/ ISBLK
      EQUIVALENCE (ISBLK, SBLK)
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Init some variables.
      NRPBLK = 256
      SCALEF = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN) )
      OFFSET = 40000.0 - SCALEF * CATR(IRRAN+1)
      ISTART = BLC(1) + .5
      IEND = TRC(1) + .5
      XCEN = (ISTART + IEND) / 2.0
C                                       Read model data.
      IRRN = IFMOD + IMOD - 1
      CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, SBLK, IERR)
      IF (IERR.NE.0) GO TO 930
C                                       Move model data from buffer.
      IS = 25
      I0 = ISBLK(IS)
      I1 = INPTS - ISBLK(IS+1)
      NGAUSS = ISBLK(IS+2)
      DO 10 I = 1,12
         PARM(I) = SBLK(I)
 10      CONTINUE
      CALL RCOPY (3, SBLK(83), BASLIN)
C                                       Read 1st block of slice pts.
      IRRN = 3 + (ISTART - 1) / NRPBLK
      CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, SBLK, IERR)
      IF (IERR.NE.0) GO TO 930
      INDEX = ISTART - ((IRRN - 3) * NRPBLK)
      BLAST = T
C                                       Plot residual data points.
      DO 50 ISL = ISTART,IEND
C                                       Read another block of data.
         IF (INDEX.LE.NRPBLK) GO TO 20
            IRRN = IRRN + 1
            CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, SBLK, IERR)
            IF (IERR.NE.0) GO TO 930
            INDEX = 1
 20      YSLICE = SBLK(INDEX)
         INDEX = INDEX + 1
C                                       Blanked pixel found. Dont show
         IF (YSLICE.NE.FBLANK) GO TO 30
            BLAST = T
            GO TO 50
 30      X = ISL
C                                       Add up all components
         Y = 0.0
         DO 40 IC = 1,NGAUSS
            IMAX = 3 * IC - 2
            IPOS = IMAX + 1
            IWTH = IPOS + 1
            Y = Y + PARM(IMAX) * EXP (-2.772 * (PARM(IPOS) - X)
     *         ** 2  /  (PARM(IWTH) * PARM(IWTH)))
 40         CONTINUE
         Y = Y + BASLIN(1) + (X-XCEN) * BASLIN(2) +
     *      (X-XCEN) * (X-XCEN) * BASLIN(3)
C                                       Plot difference
         Y = SCALEF * (YSLICE - Y)  +  OFFSET
         Y = MIN (Y, RANGE2(2))
         Y = MAX (Y, RANGE2(1))
         INP = 2
         IF (BLAST) INP = 1
         CALL TEKVEC (X, Y, INP, IERR)
         IF (IERR.NE.0) GO TO 950
         BLAST = F
 50      CONTINUE
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
