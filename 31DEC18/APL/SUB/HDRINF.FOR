      SUBROUTINE HDRINF (LUN, WTYPE, SITEM, NITEM, OUTPUT, ERROR)
C-----------------------------------------------------------------------
C! returns consecutivew items of specified type from header for WaWa
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
C   WaWa IO system:   Return a number of items from the header block
C   of an open, catalogd file.
C   Inputs:
C      LUN       I       Logical Unit No. of file
C      WTYPE     I       Data type: 1 = I,   2 = R   3 = D   6 = C*8
C      SITEM     I       Index # of 1st item wanted, indexed in a
C                        system appropriate to WTYPE (R   for C*8)
C      NITEM     I       Number of items requested
C   Outputs:
C      OUTPUT(*) ???     Array into which items go
C      ERROR     I       Error code: 0 => ok
C                                    1 => file not open
C                                    2 => nonsense input parms
C                                   10 => catlg read error
C   Common /MAPHDR/ receives the header read from catlg file
C   Written by WaWa (4/21/81)
C-----------------------------------------------------------------------
      INTEGER   LUN, WTYPE, SITEM, NITEM, OUTPUT(*), ERROR
C
      INTEGER   NI2, SI2, NB, NONSEN
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NONSEN /2/
C-----------------------------------------------------------------------
C                                       How many Integers to transfer
      ERROR = NONSEN
      IF ((WTYPE.LT.1) .OR. (WTYPE.GT.6)) GO TO 999
C                                       C*8 special case
      IF (WTYPE.NE.6) GO TO 20
         NB = 4
         NI2 = NITEM * NB / 2
         SI2 = SITEM
         GO TO 30
C                                       Normal data
 20   CONTINUE
         NB = 2
         IF (WTYPE.EQ.3) NB = 2 * NWDPDP
         NI2 = NITEM * NB / 2
         SI2 = 1 + NB * (SITEM - 1)/2
 30   IF ((SI2.LE.0) .OR. (SI2+NI2.GT.257)) GO TO 999
C                                       Get the header
      CALL GETHDR (LUN, CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 999
C                                       Move them doggies
      CALL COPY (NI2, CATBLK, OUTPUT)
C
 999  RETURN
      END
