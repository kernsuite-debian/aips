      FUNCTION IKEY (LKEY, LPARS, NPARS)
C-----------------------------------------------------------------------
C! Routine to search array LPARS for keyword LKEY
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2007
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
C   Routine to search array LPARS for string LKEY
C   Inputs:
C      LKEY    C*8     String to search for
C      LPARS   C*8(*)  Array to be searched
C      NPARS   I       Length of LPARS
C   Outputs:
C      IKEY    I       Index in LPARS; 0 if not found.
C-----------------------------------------------------------------------
      INTEGER IKEY, NPARS
      CHARACTER*8 LKEY, LPARS(NPARS)
C
      INCLUDE 'INCS:DMSG.INC'
      INTEGER I
C-----------------------------------------------------------------------
      I = 1
C                                       While (I < N) and (Not Found) do
20    IF ((I.GT.NPARS).OR.(LKEY.EQ.LPARS(I))) GO TO 50
         I = I + 1
         GO TO 20
C
50    IF (I.GT.NPARS) THEN
         WRITE (MSGTXT,1000) LKEY
         CALL MSGWRT (8)
         I = 1
         END IF
C                                       Exit
      IKEY = I
      RETURN
C----------------------------------------------------------------------
1000  FORMAT ('FATAL ERROR: CONTACT AIPS ADMIN; LKEY=',A8)
      END
