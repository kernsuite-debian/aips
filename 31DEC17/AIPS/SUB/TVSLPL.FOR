      SUBROUTINE TVSLPL (OP, ISLUN, ISFIND, RANGE2, BLC, TRC, IFMOD,
     *   IMOD, GM, GP, GW, IERR)
C-----------------------------------------------------------------------
C! plot a slice on graphics device
C# Slice Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 2000, 2007, 2016
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
C   Given an open slice file and an open TEK device, TVSLPL will draw
C   the slice.
C   Inputs:
C      OP      A*4      'SLIC', "RESI', 'MODL', 'GUES'
C      ISLUN   I        logical unit no. of open slice file.
C      ISFIND  I        FTAB index for slice file.
C      RANGE2  R(2)     min and max pixel value allowed for plot.
C      BLC     R(7)     bottom left corner of the plot.
C      TRC     R(7)     top right corner of plot; value 1 - 4000
C      IFMOD   I        First block of slice model data
C      IMOD    I        Model number, # gaussians on GUES
C      GM      R(4)     Peak values
C      GP      R(2,4)   GP(1,*) positions
C      GW      R(3,4)   GW(1,*) fwhm of Gaussian guesses
C   Output:
C      IERR    I        error code: 0 = none.
C                         1 = error reading slice
C                         2 = error writing TV
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   ISLUN, ISFIND, IFMOD, IMOD, IERR
      REAL      RANGE2(2), BLC(7), TRC(7), GM(4), GP(2,4), GW(3,4)
C
      DOUBLE PRECISION XVAL, YVAL
      REAL      X, Y, SCALEF, OFFSET, PARM(12), YSLICE, SCALE2, OFF2,
     *   BASLIN(3), XCEN
      INTEGER   ISTART, IEND, IRRN, INP, ISL, NRPBLK, J, NGAUSS, INDEX,
     *   IC, I
      LOGICAL   BLAST
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DTVS.INC'
C-----------------------------------------------------------------------
C                                       Draw plot.
      NRPBLK = 256
      SCALEF = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      OFFSET = 40000.0 - SCALEF * CATR(IRRAN+1)
      ISTART = BLC(1) + .5
      IEND = TRC(1) + .5
C                                       Read model data.
      IF ((OP.EQ.'RESI') .OR. (OP.EQ.'MODL') .OR. (OP.EQ.'COMP')) THEN
         IRRN = IFMOD + IMOD - 1
         CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, ISBLK, IERR)
         IF (IERR.NE.0) GO TO 930
         NGAUSS = ISBLK(25+2)
         CALL RCOPY (12, RSBLK, PARM)
         CALL RCOPY (3, RSBLK(83), BASLIN)
         XCEN = (ISTART + IEND) / 2.0
      ELSE IF (OP.EQ.'GUES') THEN
         NGAUSS = IMOD
C                                       Scale parms to go from 1 to
C                                       4000 units to header max min.
         SCALE2 = (CATR(IRRAN+1) - CATR(IRRAN)) / 39999.0
         OFF2 = CATR(IRRAN) - SCALE2
C                                       Convert user units to internal
C                                       units.
         J = 1
         DO 20 I = 1,NGAUSS
C                                       Position & maximum.
            XVAL = GP(1,I)
            YVAL = GM(I)
            CALL XYPIX (XVAL, YVAL, X, Y, IERR)
            IF (IERR.EQ.0) THEN
               PARM(J) = SCALE2 * Y  +  OFF2
               PARM(J+1) = X
C                                       Half width.
               XVAL = GP(1,I) - (GW(1,I) / 2.0 )
               CALL XYPIX (XVAL, YVAL, X, Y, IERR)
               IF (IERR.EQ.0) THEN
                  PARM(J+2) = 2.0 * ABS (PARM(J+1) - X)
                  IF (PARM(J+2).LE.0.0) GO TO 960
                  END IF
               END IF
            J = J + 3
            IF (IERR.NE.0) GO TO 960
 20         CONTINUE
         END IF
C                                       plot with data
      IF (OP.NE.'COMP') THEN
C                                       Read 1st block of slice pts.
         IRRN = 3 + (ISTART - 1) / NRPBLK
         CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, ISBLK, IERR)
         IF (IERR.NE.0) GO TO 930
         INDEX = ISTART - ((IRRN - 3) * NRPBLK)
         BLAST = .TRUE.
C                                       Plot residual data points.
         DO 50 ISL = ISTART,IEND
C                                       Read another block of data.
            IF (INDEX.GT.NRPBLK) THEN
               IRRN = IRRN + 1
               CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, ISBLK, IERR)
               IF (IERR.NE.0) GO TO 930
               INDEX = 1
               END IF
            YSLICE = RSBLK(INDEX)
            INDEX = INDEX + 1
C                                       Blanked pixel found. Dont show
            IF (YSLICE.EQ.FBLANK) THEN
               BLAST = .TRUE.
C                                       Plot this one
            ELSE
C                                       Find model value
               X = ISL
               IF ((OP.EQ.'MODL') .OR. (OP.EQ.'RESI') .OR.
     *            (OP.EQ.'GUES')) THEN
C                                       Add up all components
                  Y = 0.0
                  DO 40 IC = 1,NGAUSS
                     J = 3 * IC - 2
                     Y = Y + PARM(J) * EXP (-2.772 * (PARM(J+1) - X)**2
     *                  / (PARM(J+2) * PARM(J+2)))
 40                  CONTINUE
                  Y = Y + BASLIN(1) + (X - XCEN) * BASLIN(2) +
     *               (X - XCEN) * (X - XCEN) * BASLIN(3)
                  IF (OP.EQ.'RESI') Y = YSLICE - Y
               ELSE
                  Y = YSLICE
                  END IF
C                                       scale
               Y = SCALEF * Y + OFFSET
               Y = MIN (Y, RANGE2(2))
               Y = MAX (Y, RANGE2(1))
               INP = 2
               IF (BLAST) INP = 1
               CALL TVVEC (X, Y, INP, IERR)
               IF (IERR.NE.0) GO TO 950
               BLAST = .FALSE.
               END IF
 50         CONTINUE
C                                       components only (ignore data)
      ELSE
         DO 80 IC = 1,NGAUSS
            J = 3 * IC - 2
            BLAST = .TRUE.
            DO 70 ISL = ISTART,IEND
               X = ISL
               Y  = 2.772 * (PARM(J+1) - X)**2 / (PARM(J+2) * PARM(J+2))
               IF (Y.GT.12) THEN
                  BLAST = .TRUE.
               ELSE
                  Y = PARM(J) * EXP (-Y)
C                                       scale
                  Y = SCALEF * Y + OFFSET
                  Y = MIN (Y, RANGE2(2))
                  Y = MAX (Y, RANGE2(1))
                  INP = 2
                  IF (BLAST) INP = 1
                  CALL TVVEC (X, Y, INP, IERR)
                  IF (IERR.NE.0) GO TO 950
                  BLAST = .FALSE.
                  END IF
 70            CONTINUE
 80         CONTINUE
         BLAST = .TRUE.
         IF ((BASLIN(1).NE.0.0) .OR. (BASLIN(2).NE.0.0) .OR.
     *      (BASLIN(3).NE.0.0)) THEN
            DO 90 ISL = ISTART,IEND
               X = ISL
               Y = X - XCEN
               Y = BASLIN(1) + Y * BASLIN(2) + Y * y * BASLIN(3)
C                                       scale
               Y = SCALEF * Y + OFFSET
               Y = MIN (Y, RANGE2(2))
               Y = MAX (Y, RANGE2(1))
               INP = 2
               IF (BLAST) INP = 1
               CALL TVVEC (X, Y, INP, IERR)
               IF (IERR.NE.0) GO TO 950
               BLAST = .FALSE.
 90            CONTINUE
            END IF
         END IF
      GO TO 999
C                                       Slice read error.
 930  WRITE (MSGTXT,1930) IERR
      CALL MSGWRT (6)
      IERR = 1
      GO TO 999
C                                       TV write error.
 950  WRITE (MSGTXT,1950) IERR
      CALL MSGWRT (7)
      IERR = 2
      GO TO 999
C                                       Parameter error
 960  WRITE (MSGTXT,1960) I, IERR
      IF (IERR.EQ.0) WRITE (MSGTXT,1961) I
      CALL MSGWRT (7)
      IERR = 3
C
 999  RETURN
C-----------------------------------------------------------------------
 1930 FORMAT ('SLICE FILE READ ERROR',I5)
 1950 FORMAT ('TV WRITE ERROR',I5)
 1960 FORMAT ('ERROR',I3,' CONVERTING COMPONENT',I3,' TO PIXEL COORDS')
 1961 FORMAT ('ZERO HALF WIDTH IN COMPONENT ',I2,'. STOPPING.')
      END
