      SUBROUTINE FILNUM (LUN, IFIL, ERROR)
C-----------------------------------------------------------------------
C! finds the FILTABle entry number for an open file
C# IO-WaWa
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
C   WaWa IO system: find the FILTAB entry for a file
C   Inputs:
C      LUN     I       Logical unit number of file
C   Outputs:
C      IFIL    I       Entry number (2nd subscript to FILTAB)
C      ERROR   I       Error code: 0 => ok, 1 => file not open
C-----------------------------------------------------------------------
      INTEGER   LUN, IFIL, ERROR
C
      INTEGER   NOTOPN
      INCLUDE 'INCS:DITB.INC'
      DATA NOTOPN /1/
C-----------------------------------------------------------------------
      IF (LUN.LE.0) GO TO 20
C                                       Is file open: get ID
         ERROR = 0
         DO 10 IFIL = 1,EFIL
            IF (FILTAB(POLUN,IFIL).EQ.LUN) GO TO 999
 10         CONTINUE
C
 20   ERROR = NOTOPN
      IFIL = 0
C
 999  RETURN
      END
