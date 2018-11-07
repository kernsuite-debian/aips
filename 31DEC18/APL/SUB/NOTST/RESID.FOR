      SUBROUTINE RESID (IFIELD, IN, OUT, IERR)
C-----------------------------------------------------------------------
C! Tim Cornwell routine: Computes residual image.
C# Map IO-appl Math
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
C   Resid calculates the residuals of the current image by subtracting
C   the dirty map from the convolution of the current image with the
C   beam ( calculated by CONV ).
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      INTEGER   IN, OUT, IFIELD, FIL
      REAL      RTEMP1, RTEMP2
      INTEGER   AKOPEN, AKCESS, AKCLOS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
      FIL = DAT(IFIELD)
      RESMAX = -1E20
      RESMIN =  1E20
      RTEMP2 = 1 / RVAR(IFIELD)
      RTEMP1 = (RVAR(IFIELD) / FRES(IFIELD)) ** 2
C                                        Read directly from cataloged
C                                        file
      IF (AKOPEN (FIL, 1, 'READ', BUFFR1) .NE.0) GO TO 990
      IF (AKOPEN (IN, 2, 'READ', BUFFR2) .NE.0) GO TO 990
      IF (AKOPEN (SCR, 3, 'WRIT', BUFFR3) .NE.0) GO TO 990
C                                       Loop thru map.
 70   DO 150 IY = 1,HNY
         IF (AKCESS (SCR, BUFFR3) .NE.0) GO TO 990
         CALL RFILL (HNX, 0.0, BUFFR3(BIND(3)))
         IF ((IY.LT.YBEG) .OR. (IY.GT.YEND)) GO TO 150
            IF (AKCESS (FIL, BUFFR1) .NE.0) GO TO 990
            IF (AKCESS (IN, BUFFR2) .NE.0) GO TO 990
            I1 = BIND(1)
            I2 = BIND(2)
            I3 = BIND(3) + XBEG - 1
            DO 140 IX = XBEG, XEND
               BUFFR1(I1) = BUFFR1(I1) * RTEMP2
               BUFFR3(I3) = 2 * (BUFFR2(I2) - BUFFR1(I1)) * RTEMP1
               RESMAX = MAX( RESMAX, BUFFR3(I3))
               RESMIN = MIN( RESMIN, BUFFR3(I3))
               CHISQ = CHISQ +
     *            0.5 * BUFFR3(I3) * (BUFFR2(I2) - BUFFR1(I1))
               I1 = I1 + 1
               I2 = I2 + 1
               I3 = I3 + 1
 140           CONTINUE
 150     CONTINUE
      IF (AKCLOS (FIL, BUFFR1) .NE.0) GO TO 990
      IF (AKCLOS (IN, BUFFR2) .NE.0) GO TO 990
      IF (AKCLOS (SCR, BUFFR3) .NE.0) GO TO 990
C
      CALL FILSWP (OUT, SCR)
      GO TO 999
C
 990  WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RESID')
      END
