      SUBROUTINE QMAXMI (A, I, MAX, MIN, N)
C-----------------------------------------------------------------------
C! FPS AP version: Find max. and min values of a vector
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
C   Searches the given vector for maximum and minimum values.
C    Inputs:
C     A    I  Source vector base address
C     I    I  Increment of A
C     MAX  I  Location for maximum.
C     MIN  I  Location for minimum.
C     N    I  Element count.
C-----------------------------------------------------------------------
      INTEGER A, I, MAX, MIN, N
      INTEGER*2 IA, II, IMAX, IMIN, IN
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (A.LT.32768) IA = A
      IF (A.GE.32768) IA = A - 65536
      IF (I.LT.32768) II = I
      IF (I.GE.32768) II = I - 65536
      IF (MAX.LT.32768) IMAX = MAX
      IF (MAX.GE.32768) IMAX = MAX - 65536
      IF (MIN.LT.32768) IMIN = MIN
      IF (MIN.GE.32768) IMIN = MIN - 65536
      IF (N.LT.32768) IN = N
      IF (N.GE.32768) IN = N - 65536
C                                        Call FPS routine.
      CALL MAXMIN (IA, II, IMAX, IMIN, IN)
C
 999  RETURN
      END
