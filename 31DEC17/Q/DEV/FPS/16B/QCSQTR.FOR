      SUBROUTINE QCSQTR (CORNER, SIZE, ROW)
C-----------------------------------------------------------------------
C! FPS AP version: inplace transpose of complex, square matrix
C# AP-util
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
C   FPS AP version
C   QCSQTR does an inplace transpose of square matrices of complex
C   values.
C    Inputs:
C     CORNER  I  AP location of first corner of matrix encountered.
C     SIZE    I  Size (number of reals) of a row or column.
C     ROW     I  Number of locations in AP between beginnings
C                  of the rows.
C-----------------------------------------------------------------------
      INTEGER CORNER, SIZE, ROW
      INTEGER*2 ICORN, ISIZE, IROW
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (CORNER.LT.32768) ICORN = CORNER
      IF (CORNER.GE.32768) ICORN = CORNER - 65536
      IF (SIZE.LT.32768) ISIZE = SIZE
      IF (SIZE.GE.32768) ISIZE = SIZE - 65536
      IF (ROW.LT.32768) IROW = ROW
      IF (ROW.GE.32768) IROW = ROW - 65536
C                                        Call FPS routine.
      CALL CSQTRN (ICORN, ISIZE, IROW)
C
 999  RETURN
      END
