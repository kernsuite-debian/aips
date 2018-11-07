      SUBROUTINE PLOTDA (X, Y, NPTS, NTYP1, SIZE, BUFFER, IERR)
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
C   Data plotting routine
C   Inputs:
C      X,Y   R      Arrays with X-and Y-coordinates in users system
C      NPTS  I      Number of points in the arrays
C      NTYP1 I      Plotting type:
C                      0 => Plots symbols
C                      1 => Draws a full line between the points
C                      2 => Full line but for phase (no vectors > 180)
C      SIZE  R      Size of symbol:
C                     =0 => Standard size
C                     >0 => Size is given in users Y-coordinates
C                     <0 => Size is given in cm
C   Outputs:
C      BUFFER R     Work buffer
C      IERR   I     Return code, 0=>OK
C   The user must have called the subroutine PLTSET (sets up CPLOTU)
C-----------------------------------------------------------------------
      INTEGER   BUFFER(1), IERR, NPLO, NTYP1, I, NPTS, J,  NPLA
      REAL   X(1), Y(2,1), XTEMP, XMIN, XMAX, YMIN, YMAX, XX, XOFF, DY,
     *   XSCAL, ZXOFF, YY, YOFF, YSCAL, ZYOFF, X1, Y1, DX, SIZE, YOLD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /CPLOTU/ XSCAL, YSCAL, XOFF, YOFF, XMIN, XMAX, YMIN,
     *   YMAX, ZXOFF, ZYOFF
C-----------------------------------------------------------------------
      IERR = 0
C                                     plot curve
      IF (NTYP1.EQ.0) GO TO 100
         DO 30 J = 1,2
            NPLA = 0
            YOLD = Y(J,1)
            DO 20 I = 1,NPTS
               NPLO = 1
               IF (Y(J,I).EQ.FBLANK) NPLO = 0
               IF ((X(I).LT.XMIN) .OR. (X(I).GT.XMAX) .OR.
     *            (Y(J,I).LT.YMIN) .OR. (Y(J,I).GT.YMAX)) NPLO = 0
               IF (NPLO.EQ.0) GO TO 10
                  DY = ABS (Y(J,I)-YOLD)
                  IF ((NTYP1.EQ.2) .AND. (DY.GT.180.000)) NPLA = 0
                  YOLD = Y(J,I)
                  XX = (X(I)-XOFF) * XSCAL + ZXOFF
                  YY = (Y(J,I)-YOFF) * YSCAL + ZYOFF
                  IF (NPLA.EQ.0) CALL GPOS (XX, YY, BUFFER, IERR)
                  IF (NPLA.EQ.1) CALL GVEC (XX, YY, BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 990
 10            NPLA = NPLO
 20            CONTINUE
 30         CONTINUE
         GO TO 999
C                                      plot symbol
 100  CONTINUE
         DO 120 J = 1,2
            DO 110 I = 1,NPTS
               IF ((X(I).LT.XMIN) .OR. (X(I).GT.XMAX) .OR.
     *            (Y(J,I).LT.YMIN) .OR. (Y(J,I).GT.YMAX)) GO TO 110
                  X1 = (X(I)-XOFF) * XSCAL + ZXOFF
                  Y1 = (Y(J,I)-YOFF) * YSCAL + ZYOFF
                  DX = SIZE * XSCAL / 2.0
                  DY = SIZE * YSCAL / 2.0
                  IF (DX.LT.1.0E-9) DX = 1.0
                  IF (DY.LT.1.0E-9) DY = 1.0
                  XTEMP = X1 + DX
                  CALL GPOS (XTEMP, Y1, BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 990
                  XTEMP = X1 - DX
                  CALL GVEC (XTEMP, Y1, BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 990
                  XTEMP = Y1 + DX
                  CALL GPOS (X1, XTEMP, BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 990
                  XTEMP = Y1 - DX
                  CALL GVEC (X1, XTEMP, BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 990
 110           CONTINUE
 120        CONTINUE
C                                     error return
 990  WRITE (MSGTXT,1990) IERR
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('PLOTDA: ERROR IN PLOTTING. IERR = ',I5)
      END
