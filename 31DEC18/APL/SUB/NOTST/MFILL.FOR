      SUBROUTINE MFILL (ILUN, FILVAL, DATA, IERR)
C-----------------------------------------------------------------------
C! Fill a window in an image with a given value.
C# IO-WaWa Map
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
C   MFILL (for use with WAWA IO only) will fill a windowed section of
C   an open map file with a given value.
C    Inputs:
C     INLUN   I      map LUN.
C     FILVAL  R      fill value.
C     DATA    R(*)   IO buffer big enough for 1 row.
C Output:
C     IERR    I      standard WAWA IO error code.
C-----------------------------------------------------------------------
      REAL      DATA(*), FILVAL, XMIN, XMAX
      INTEGER   IBLC(7), ITRC(7), IERR, INODIM
      INTEGER   ILUN, I7, I6, I5, I4, I3, I2, I1
      INTEGER   IPIXS1, IPIXS2, IPIXS3, IPIXS4, IPIXS5, IPIXS6, IPIXS7
      INCLUDE 'INCS:DITB.INC'
C-----------------------------------------------------------------------
C                                       Initialize XMIN and XMAX
      XMIN = 1.0E30
      XMAX = - XMIN
C                                       Get window information
      CALL GETWIN (ILUN, INODIM, IBLC, ITRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Do fill.
      IPIXS1 = ITRC(1) - IBLC(1) + 1
      IPIXS2 = ITRC(2) - IBLC(2) + 1
      IPIXS3 = ITRC(3) - IBLC(3) + 1
      IPIXS4 = ITRC(4) - IBLC(4) + 1
      IPIXS5 = ITRC(5) - IBLC(5) + 1
      IPIXS6 = ITRC(6) - IBLC(6) + 1
      IPIXS7 = ITRC(7) - IBLC(7) + 1
      DO 700 I7 = 1,IPIXS7
      DO 600 I6 = 1,IPIXS6
      DO 500 I5 = 1,IPIXS5
      DO 400 I4 = 1,IPIXS4
      DO 300 I3 = 1,IPIXS3
      DO 200 I2 = 1,IPIXS2
         DO 100 I1 = 1,IPIXS1
            DATA(I1) = FILVAL
 100        CONTINUE
         CALL MAPIO ('WRIT', ILUN, DATA, IERR)
         IF (IERR.NE.0) GO TO 999
 200     CONTINUE
 300     CONTINUE
 400     CONTINUE
 500     CONTINUE
 600     CONTINUE
 700     CONTINUE
C
 999  RETURN
      END
