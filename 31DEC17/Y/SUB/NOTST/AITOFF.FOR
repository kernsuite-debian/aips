      SUBROUTINE AITOFF (X0, L2GRD, B2GRD, IGBLK, IERR)
C-----------------------------------------------------------------------
C! writes vectors for Aitoff projection grid to plot file
C# Plot-appl
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
C   AITOFF writes vectors for Aitoff equal area projection grid to open
C   PL file.  Not used anywhere???
C   Inputs
C      X0       R        Semi-major axis length
C      L2GRD    I        Longitude grid spacing in degrees (0-> 30)
C      B2GRD    I        Latitude grid spacing in degrees (0-> 30)
C      IGBLK    I(256)   Graphics work buffer
C      IERR     I        Error
C-----------------------------------------------------------------------
      CHARACTER LABLE*132
      INTEGER   L2GRD, B2GRD, IGBLK(256), IERR
      INTEGER   LIIGRD, BIIGRD, IANGL, INC, I, J, JTEMP
      REAL      X1(181), Y1(181), X0, Y0, DX, PHI, TETA, X, Y, DY
      DATA INC /3/
C-----------------------------------------------------------------------
      LIIGRD = L2GRD
      BIIGRD = B2GRD
      IF (LIIGRD.EQ.0) LIIGRD = 30
      IF (BIIGRD.EQ.0) BIIGRD = 30
      Y0 = X0 / 2.
      DX = 0.0
      IANGL = 0
C                                      Plot latitude grid lines
      DO 100 I = 1,91,BIIGRD
         PHI = (I-1) * 0.01745329
         DO 20 J = 1,181,INC
            TETA = (J-1) * 0.01745329
            X1(J)=  X0 * COS(PHI) * SIN(TETA/2.) / SQRT(1. +
     *         COS(PHI) * COS(TETA/2.))
            Y1(J)=  Y0 * SIN(PHI) / SQRT(1. + COS(PHI) * COS(TETA/2.))
 20         CONTINUE
C                                     Upper right quad
         X = X0 + X1(1)
         Y = Y0 + Y1(1)
         CALL GPOS (X, Y, IGBLK, IERR)
         DO 30 J = 1,181,INC
            X = X0 + X1(J)
            Y = Y0 + Y1(J)
            CALL GVEC (X, Y, IGBLK, IERR)
 30         CONTINUE
         JTEMP = I - 1
         WRITE (LABLE,1000) JTEMP
         DY = 0.2
         CALL GCHAR (3, IANGL, DX, DY, LABLE, IGBLK, IERR)
C                                     Upper left quad
         X = X0 - X1(1)
         Y = Y0 - Y1(1)
         CALL GPOS (X, Y, IGBLK, IERR)
         DO 40 J = 1,181,INC
            X = X0 - X1(J)
            Y = Y0 - Y1(J)
            CALL GVEC (X, Y, IGBLK, IERR)
 40         CONTINUE
C                                      Lower right quad
         X = X0 + X1(1)
         Y = Y0 - Y1(1)
         CALL GPOS (X, Y, IGBLK, IERR)
         DO 50 J = 1,181,INC
            X = X0 + X1(J)
            Y = Y0 - Y1(J)
            CALL GVEC (X, Y, IGBLK, IERR)
 50         CONTINUE
         DY = -1.2
         JTEMP = 1 - I
         WRITE (LABLE,1000) JTEMP
         CALL GCHAR (3, IANGL, DX, DY, LABLE, IGBLK, IERR)
C                                      Lower left quad
         X = X0 - X1(1)
         Y = Y0 + Y1(1)
         CALL GPOS (X, Y, IGBLK, IERR)
         DO 60 J = 1,181,INC
            X = X0 - X1(J)
            Y = Y0 + Y1(J)
            CALL GVEC (X, Y, IGBLK, IERR)
 60         CONTINUE
 100     CONTINUE
C                                       Plot longitude grid lines
      DY = 0.2
      DO 200 I = 1,181,LIIGRD
         TETA = (I-1) * 0.01745329
         DO 120 J = 1,181,INC
            PHI = (J-1) * 0.01745329/2.
            X1(J) = X0 * COS(PHI) * SIN(TETA/2.) / SQRT(1.
     *         + COS(PHI) * COS(TETA/2.))
            Y1(J) = Y0 * SIN(PHI) / SQRT(1. + COS(PHI) * COS(TETA/2.))
 120        CONTINUE
C                                      Upper right quad
         X = X0 + X1(1)
         Y = Y0 + Y1(1)
         CALL GPOS (X, Y, IGBLK, IERR)
         IF (I.EQ.1) GO TO 125
            JTEMP = 361 - I
            WRITE (LABLE,1000) JTEMP
            CALL GCHAR (3, IANGL, DX, DY, LABLE, IGBLK, IERR)
 125        CONTINUE
         DO 130 J = 1,181,INC
            X = X0 + X1(J)
            Y = Y0 + Y1(J)
            CALL GVEC (X, Y, IGBLK, IERR)
 130        CONTINUE
C                                      Upper left quad
         X = X0 - X1(1)
         Y = Y0 + Y1(1)
         CALL GPOS (X, Y, IGBLK, IERR)
         IF (I.EQ.1) GO TO 135
            JTEMP = I - 1
            WRITE (LABLE,1000) JTEMP
            CALL GCHAR (3, IANGL, DX, DY, LABLE, IGBLK, IERR)
 135        CONTINUE
         DO 140 J = 1,181,INC
            X = X0 - X1(J)
            Y = Y0 + Y1(J)
            CALL GVEC (X, Y, IGBLK, IERR)
 140        CONTINUE
C                                      Lower right quad
         X = X0 + X1(1)
         Y = Y0 - Y1(1)
         CALL GPOS (X, Y, IGBLK, IERR)
         DO 150 J = 1,181,INC
            X = X0 + X1(J)
            Y = Y0 - Y1(J)
            CALL GVEC (X, Y, IGBLK, IERR)
 150        CONTINUE
C                                      Lower left quad
         X = X0 - X1(1)
         Y = Y0 - Y1(1)
         CALL GPOS (X, Y, IGBLK, IERR)
         DO 160 J = 1,181,INC
            X = X0 - X1(J)
            Y = Y0 - Y1(J)
            CALL GVEC (X, Y, IGBLK, IERR)
 160        CONTINUE
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I3)
      END
