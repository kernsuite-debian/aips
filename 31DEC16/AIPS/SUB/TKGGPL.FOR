      SUBROUTINE TKGGPL (NGAUSS, GPOS, GMAX, GWTH, RANGE2, BLC, TRC,
     *   IERR)
C-----------------------------------------------------------------------
C! plots model slice on Graphics
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
C   Given an open TEK device, TKGGPL will draw a model slice using
C   gaussian components given in the inputs.  The relevant header must
C   be in common /MAPHDR/ and the position common initialized.
C   Inputs:
C      NGAUSS  I       number of gaussians in model
C      GPOS    R(2,4)  Position in units of arc sec or whatever
C      GMAX    R(4)    Peak value: physical units
C      GWTH    R(4)    Width: arc sec or whatever
C      RANGE2  R(2)    min and max pixel value allowed for plot.
C      BLC     R(7)    the bottom left corner of the plot.
C      TRC     R(7)    the top right corner of the plot.
C   Output:
C      IERR    I       error code 0= none.
C                         1 = error during graphing. Try finish.
C                         2 = fatal error. Give up.
C-----------------------------------------------------------------------
      INTEGER   NGAUSS, IERR
      REAL      GPOS(2,4), GMAX(4), GWTH(3,4), RANGE2(2), BLC(7), TRC(7)
C
      DOUBLE PRECISION    XVAL, YVAL
      REAL      X, Y, OFF1, OFF2, SCALE1, SCALE2, XPIX, YPIX, PARM(12)
      INTEGER   ISTART, IEND, IPOS, INP, I, IC, IMAX, ISL, IWTH, J
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Init some variables.
      ISTART = BLC(1) + .5
C                                       Scale parms to go from header
C                                       max min to 1 to 4000 pixel type
C                                       units.
      IEND = TRC(1) + .5
      SCALE1 = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      OFF1 = 1.0 - SCALE1 * CATR(IRRAN)
C                                       Scale parms to go from 1 to
C                                       4000 units to header max min.
      SCALE2 = (CATR(IRRAN+1) - CATR(IRRAN)) / 39999.0
      OFF2 = CATR(IRRAN) - SCALE2
C                                       Convert user units to internal
C                                       units.
      J = 1
      DO 20 I = 1,NGAUSS
C                                       Position & maximum.
         XVAL = GPOS(1,I)
         YVAL = GMAX(I)
         CALL XYPIX (XVAL, YVAL, XPIX, YPIX, IERR)
         IF (IERR.NE.0) GO TO 10
         PARM(J) = SCALE2 * YPIX  +  OFF2
         PARM(J+1) = XPIX
C                                       Half width.
         XVAL = GPOS(1,I) - (GWTH(1,I) / 2.0 )
         CALL XYPIX (XVAL, YVAL, XPIX, YPIX, IERR)
         IF (IERR.NE.0) GO TO 10
         PARM(J+2) = 2.0 * ABS (PARM(J+1) - XPIX)
         J = J + 3
         GO TO 20
 10      WRITE (MSGTXT,1010) IERR, I
         CALL MSGWRT (7)
 20      CONTINUE
C                                       Plot data points.
      INP = 1
      DO 50 ISL = ISTART,IEND
         X = ISL
C                                       Add up all components
         Y = 0.0
         DO 30 IC = 1,NGAUSS
            IMAX = 3 * IC - 2
            IPOS = IMAX + 1
            IWTH = IPOS + 1
            IF (PARM(IWTH).EQ. 0.0) GO TO 980
            Y = Y + PARM(IMAX) * EXP ( - 2.772 * (PARM(IPOS) - X)
     *         ** 2  /  (PARM(IWTH) * PARM(IWTH)))
 30         CONTINUE
C
         Y = SCALE1 * Y  +  OFF1
         Y = MIN (Y, RANGE2(2))
         Y = MAX (Y, RANGE2(1))
         CALL TEKVEC (X, Y, INP, IERR)
         IF (IERR.NE.0) GO TO 950
         INP = 2
 50      CONTINUE
C                                       Force alpha mode to allow
C                                       screen to auto-dim
      ISTART = 31
      CALL ZTKBUF (ISTART, 1, IERR)
C                                       Write final TEK buffer.
      CALL TEKFLS (IERR)
      IF (IERR.EQ.0) GO TO 999
      GO TO 950
C                                       TEK write error.
 950  IERR = 2
      WRITE (MSGTXT,1950)
      CALL MSGWRT (7)
      GO TO 999
 980  IERR = 2
      WRITE (MSGTXT,1980) IC
      CALL MSGWRT (7)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR',I3,' CONVERTING COMPONENT',I3,' TO PIXEL COORDS')
 1950 FORMAT ('TEKTRONIX 4012 WRITE ERROR.')
 1980 FORMAT ('ZERO HALF WIDTH IN COMPONENT ',I2,'. STOPPING.')
      END
