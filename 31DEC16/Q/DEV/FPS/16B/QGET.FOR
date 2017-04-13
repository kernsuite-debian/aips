      SUBROUTINE QGET (HOST, AP, N, TYPE)
C-----------------------------------------------------------------------
C! FPS AP version: Transfer data from AP to Host memory.
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
C      Transfer data from AP core into "Host"  core.
C   Inputs:
C      HOST(*) R/I     Data array in "host"
C      AP      I       Target area in AP; 0 - relative
C      N       I       Number of elements
C      TYPE    I       Data type:
C                      0   data is I in host
C                      1   data is I*2 in host; no longer allowed.
C                      2   data is R in host
C-----------------------------------------------------------------------
      REAL      HOST(*)
      INTEGER AP, N, TYPE
      INTEGER*2 IAP, IN, ITYPE
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (AP.LT.32768) IAP = AP
      IF (AP.GE.32768) IAP = AP - 65536
      IF (N.LT.32768) IN = N
      IF (N.GE.32768) IN = N - 65536
      ITYPE = TYPE
C                                        I*2 data no longer allowed
      IF (ITYPE.EQ.1) ITYPE = 0
C                                        Call FPS routine.
      CALL APGET (HOST, IAP, IN, ITYPE)
C
 999  RETURN
      END
