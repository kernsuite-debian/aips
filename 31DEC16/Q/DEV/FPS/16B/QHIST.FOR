      SUBROUTINE  QHIST (A, I, C, N, NB, AMAX, AMIN)
C-----------------------------------------------------------------------
C! FPS AP version: Histogram of values in a vector
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
C   Computes the histogram of a vector.
C   Histogram element (NB-1)*(DATA-MIN)/(MAX-MIN) where DATA
C   is the data value is incremented.
C    Inputs:
C     A     I  Source vector base address.
C     I     I  A address increment.
C     C     I  Histogram base address
C                  Histogram must be cleared before first call.
C     N     I  Element count for A
C     NB    I  Number of bins in histogram
C     AMAX  I  Address of histogram maximum.
C     AMIN  I  Address of histogram minimum.
C-----------------------------------------------------------------------
      INTEGER A, I, C, N, NB, AMAX, AMIN
      INTEGER*2 IA, II, IC, IN, INB, IAMAX, IAMIN
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (A.LT.32768) IA = A
      IF (A.GE.32768) IA = A - 65536
      IF (I.LT.32768) II = I
      IF (I.GE.32768) II = I - 65536
      IF (C.LT.32768) IC = C
      IF (C.GE.32768) IC = C - 65536
      IF (N.LT.32768) IN = N
      IF (N.GE.32768) IN = N - 65536
      IF (NB.LT.32768) INB = NB
      IF (NB.GE.32768) INB = NB - 65536
      IF (AMAX.LT.32768) IAMAX = AMAX
      IF (AMAX.GE.32768) IAMAX = AMAX - 65536
      IF (AMIN.LT.32768) IAMIN = AMIN
      IF (AMIN.GE.32768) IAMIN = AMIN - 65536
C                                        Call FPS routine.
      CALL HIST (IA, II, IC, IN, INB, IAMAX, IAMIN)
C
 999  RETURN
      END
