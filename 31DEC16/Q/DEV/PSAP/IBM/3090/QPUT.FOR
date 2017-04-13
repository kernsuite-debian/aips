@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QPUT (HOST, AP, N, TYPE)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Move data from "host" to "AP" memory.
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
C   Transfer data from "Host" core into pseudo-AP  core, i.e. APCORE
C   Inputs:
C      HOST R/I(*)  Data array in "host"
C      AP   I       Target area in AP; 0 - relative
C      N    I       Number of elements
C      TYPE I       Data type:
C                      0   data is I   in host
C                      1   data is I   in host
C                      2   data is R   in host
C-----------------------------------------------------------------------
      REAL      HOST(*)
      INTEGER   AP, N, TYPE, I
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                       Check that type valid.
      IF (TYPE.GT.2) GO TO 999
C                                       Check for no data.
      IF (N.LE.0) GO TO 999
      IF (TYPE.EQ.1) GO TO 100
C                                       Real
C        DO 50 I = 1, N
C           APCORE(AP+I) = HOST(I)
C50         CONTINUE
         CALL SCOPY(N,HOST,1,APCORE(AP+1),1)
         GO TO 999
C                                       integer
 100     CALL QPUT2 (HOST, AP, N)
C
 999  RETURN
      END
