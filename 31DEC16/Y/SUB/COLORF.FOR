      SUBROUTINE COLORF (NLEVS, ICOLR, XBRK, GAMMA, BUFFER, IERR)
C-----------------------------------------------------------------------
C! continuous flame colors from red-orange-white (or rotations thereof)
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   COLOR creates a continuous flame-like coloring
C   Inputs:
C      NLEVS   I      # of intensities (usually 256 or 1024)
C      ICOLR   I      initial color R,G,B = 1,2,3
C                     < 0 => go RBG order instead
C      XBRK    I      break point between colors (-1->1) shift
C      GAMMA   R      gamma correction power
C   Output:
C      BUFFER  R(*)   scratch buffer
C      IERR    I      error code of ZM70XF: 0 - ok
C-----------------------------------------------------------------------
      INTEGER   NLEVS, ICOLR, IERR
      REAL      XBRK, GAMMA, BUFFER(*)
C
      INTEGER   MAGICX(7), MAGICY(7), ICI, INC, NEND(4), KOLOR, JERR
      REAL      GAMINV, SLOPE(4), OFFSET(4), X0, X1, X2, Y0, Y1, Y2
      LOGICAL   WASYNC
      INCLUDE 'INCS:DTVC.INC'
      DATA MAGICX / 0, 100, 160, 170, 170, 255, 255/
      DATA MAGICY / 0,   0,   0, 131, 255, 255, 255/
C-----------------------------------------------------------------------
      GAMINV = GAMMA
      IF (GAMINV.LE.0.0) GAMINV = 1.0
      GAMINV = 1.0 / GAMINV
      IF (NLEVS.GT.OFMINP+1) NLEVS = OFMINP + 1
      XBRK = MAX (-1.0, MIN (1.0, XBRK))
      ICI = ABS (ICOLR)
      INC = 1
      IF (ICOLR.LT.0) INC = -1
      IF ((ICI.LT.1) .OR. (ICI.GT.3)) ICI = 1
      WASYNC = ISYNCH.EQ.0
      IF (WASYNC) CALL YHOLD ('ONNN', JERR)
C                                        first color (red normally)
      KOLOR = 2 ** (3-ICI)
      X0 = (MAGICX(1) + 40. * XBRK) / 255.0
      X1 = (MAGICX(5) * (1.0 + XBRK / 3.0)) / 255.0
      Y0 = MAGICY(1) / 255.0
      Y1 = MAGICY(5) / 255.0
      X0 = MAX (0.0, MIN (1.0, X0))
      X1 = MAX (0.0, MIN (1.0, X1))
      NEND(1) = X0 * (NLEVS - 1.) + 1.5
      NEND(2) = X1 * (NLEVS - 1.) + 1.5
      NEND(3) = NLEVS
      SLOPE(1) = 0.0
      OFFSET(1) = Y0
      SLOPE(2) = (Y1 - Y0) / (NEND(2) - NEND(1))
      OFFSET(2) = Y0 - SLOPE(2) * (NEND(1) - 1.)
      SLOPE(3) = 0.0
      OFFSET(3) = Y1
      CALL ILNCLR (KOLOR, 3, NEND, SLOPE, OFFSET, GAMINV, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        second color (green normally)
      ICI = ICI + INC
      IF (ICI.EQ.4) ICI = 1
      IF (ICI.EQ.0) ICI = 3
      KOLOR = 2 ** (3-ICI)
      X0 = (MAGICX(2) * (1.0 + XBRK / 3.0)) / 255.0
      X1 = (MAGICX(4) * (1.0 + XBRK / 3.0)) / 255.0
      X2 = (MAGICX(6) + 40. * XBRK) / 255.0
      Y0 = MAGICY(2) / 255.0
      Y1 = MAGICY(4) / 255.0
      Y2 = MAGICY(6) / 255.0
      X0 = MAX (0.0, MIN (1.0, X0))
      X1 = MAX (0.0, MIN (1.0, X1))
      X2 = MAX (0.0, MIN (1.0, X2))
      NEND(1) = X0 * (NLEVS - 1.) + 1.5
      NEND(2) = X1 * (NLEVS - 1.) + 1.5
      NEND(3) = X2 * (NLEVS - 1.) + 1.5
      NEND(4) = NLEVS
      SLOPE(1) = 0.0
      OFFSET(1) = Y0
      SLOPE(2) = (Y1 - Y0) / (NEND(2) - NEND(1))
      OFFSET(2) = Y0 - SLOPE(2) * (NEND(1) - 1.)
      SLOPE(3) = (Y2 - Y1) / (NEND(3) - NEND(2))
      OFFSET(3) = Y1 - SLOPE(3) * (NEND(2) - 1.)
      SLOPE(4) = 0.0
      OFFSET(4) = Y2
      CALL ILNCLR (KOLOR, 4, NEND, SLOPE, OFFSET, GAMINV, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        third color (blue usually)
      ICI = ICI + INC
      IF (ICI.EQ.4) ICI = 1
      IF (ICI.EQ.0) ICI = 3
      KOLOR = 2 ** (3-ICI)
      X0 = (MAGICX(3) * (1.0 + XBRK / 3.0)) / 255.0
      X1 = (MAGICX(7) + 40. * XBRK) / 255.0
      Y0 = MAGICY(3) / 255.0
      Y1 = MAGICY(7) / 255.0
      X0 = MAX (0.0, MIN (1.0, X0))
      X1 = MAX (0.0, MIN (1.0, X1))
      NEND(1) = X0 * (NLEVS - 1.) + 1.5
      NEND(2) = X1 * (NLEVS - 1.) + 1.5
      NEND(3) = NLEVS
      SLOPE(1) = 0.0
      OFFSET(1) = Y0
      SLOPE(2) = (Y1 - Y0) / (NEND(2) - NEND(1))
      OFFSET(2) = Y0 - SLOPE(2) * (NEND(1) - 1.)
      SLOPE(3) = 0.0
      OFFSET(3) = Y1
      CALL ILNCLR (KOLOR, 3, NEND, SLOPE, OFFSET, GAMINV, BUFFER, IERR)
C
 990  IF (WASYNC) CALL YHOLD ('OFFF', JERR)
C
 999  RETURN
      END
