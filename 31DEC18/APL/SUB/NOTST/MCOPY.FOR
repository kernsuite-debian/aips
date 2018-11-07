      SUBROUTINE MCOPY (INLUN, OUTLUN, FACTOR, DATA, XMIN, XMAX,
     *   BLANK, IERR)
C-----------------------------------------------------------------------
C! Copies a window in one image to another.
C# IO-wawa Map
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
C   MCOPY (for use with WAWA IO only) will copy one windowed section of
C   an open map fill to another windowed section of an open map file.
C    Inputs:
C     INLUN   I   input map LUN.
C     OUTLUN  I   output map LUN.
C     FACTOR  R   multiplication factor for copied data.
C     DATA    R(?)   IO buffer big enough for 1 row.
C    Outputs:
C     XMIN    R   map min after scaling of windowed output map.
C     XMAX    R   map max after scaling of windowed output map.
C     BLANK   L   True if blank pixel found, else false.
C     IERR    I   standard WAWA IO error code.
C-----------------------------------------------------------------------
      REAL      DATA(*), FACTOR, XMAX, XMIN
      INTEGER   IBLC1(7), ITRC1(7), IBLC2(7), ITRC2(7), IERR
      INTEGER   INLUN, OUTLUN, INODI1, INODI2, IDMAX, IDIM
      INTEGER   IPIXS1, IPIXS2, IPIXS3, IPIXS4, IPIXS5, IPIXS6, IPIXS7
      INTEGER   I7, I6, I5, I4, I3, I2, I1
      LOGICAL   BLANK
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Initialize XMIN and XMAX
      XMIN = 1.0E30
      XMAX = - XMIN
      BLANK = .FALSE.
C                                       Get window information
      CALL GETWIN (INLUN, INODI1, IBLC1, ITRC1, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETWIN (OUTLUN, INODI2, IBLC2, ITRC2, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Verify windows match.
      IDMAX = MAX (INODI1, INODI2)
      DO 50 IDIM = 1,IDMAX
         IF ((ITRC1(IDIM)-IBLC1(IDIM)).NE.(ITRC2(IDIM)-IBLC2(IDIM)))
     *      GO TO 980
 50      CONTINUE
C                                       Do copy.
      IPIXS1 = ITRC1(1) - IBLC1(1) + 1
      IPIXS2 = ITRC1(2) - IBLC1(2) + 1
      IPIXS3 = ITRC1(3) - IBLC1(3) + 1
      IPIXS4 = ITRC1(4) - IBLC1(4) + 1
      IPIXS5 = ITRC1(5) - IBLC1(5) + 1
      IPIXS6 = ITRC1(6) - IBLC1(6) + 1
      IPIXS7 = ITRC1(7) - IBLC1(7) + 1
      DO 700 I7 = 1,IPIXS7
      DO 600 I6 = 1,IPIXS6
      DO 500 I5 = 1,IPIXS5
      DO 400 I4 = 1,IPIXS4
      DO 300 I3 = 1,IPIXS3
      DO 200 I2 = 1,IPIXS2
         CALL MAPIO ('READ', INLUN, DATA, IERR)
         IF (IERR.NE.0) GO TO 999
         DO 100 I1 = 1,IPIXS1
            IF (DATA(I1).EQ.FBLANK) THEN
C                                       Blank pixel.
               BLANK = .TRUE.
            ELSE
C                                       Pixel not blank.
               DATA(I1) = FACTOR * DATA(I1)
               XMIN = MIN (XMIN, DATA(I1))
               XMAX = MAX (XMAX, DATA(I1))
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', OUTLUN, DATA, IERR)
         IF (IERR.NE.0) GO TO 999
 200     CONTINUE
 300     CONTINUE
 400     CONTINUE
 500     CONTINUE
 600     CONTINUE
 700     CONTINUE
      GO TO 999
C                                       Window mis-match error.
 980  CONTINUE
      IERR = 16
C
 999  RETURN
      END
