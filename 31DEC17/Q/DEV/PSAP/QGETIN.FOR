      SUBROUTINE QGETIN (HOST, J, AP, L, N, TYPE)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Move data from pseudo-AP to host w increments
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2006, 2012
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
C   Transfer data from pseudo-AP core to normal core with an increment
C   Inputs:
C      J      I  Increment in host memory: 0 -> 1
C      AP     I  Target area in AP; 0-relative
C      L      I  Increment in AP memory: 0 -> 1
C      N      I  Number of elements returned
C      TYPE   I  Data type:
C                 1     data is I   in host
C                 else  data is R   in host
C   Output:
C      HOST   R/I(*)  Data array in "host"
C   NOTE WELL: the data in the AP is DOUBLE PRECISION and INTEGER*8
C   DO NOT LIE ABOUT THE TYPE YOU WANT OR YOU WILL GET BAD VALUES
C-----------------------------------------------------------------------
      INTEGER   J, AP, L, N, TYPE
      REAL      HOST(*)
C
      INTEGER   I, IL, JI, IJ
      LONGINT   JAP
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (TYPE.GT.2) GO TO 999
      IF (N.LE.0) GO TO 999
C                                       Integer data
      IF (TYPE.EQ.1) THEN
         CALL QGETI2 (HOST, J, AP, L, N)
C                                       Real data
      ELSE
         IL = L
         IF (IL.EQ.0) IL = 1
         IJ = J
         IF (IJ.EQ.0) IJ = 1
         JI = 1
         JAP = AP + PSAPOF
         DO 10 I = 1,N
            HOST(JI) = APCORE(JAP)
            JI = JI + IJ
            JAP = JAP + IL
 10         CONTINUE
         END IF
C
 999  RETURN
      END
