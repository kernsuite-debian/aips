      SUBROUTINE IMOPEN (ERROR)
C-----------------------------------------------------------------------
C! Open the TV under the system set up by IOSET
C# IO-wawa TV-appl
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
C   Open the TV under the system set up by IOSET
C   No inputs.
C   Outputs:
C      ERROR    I        TVOPEN error
C-----------------------------------------------------------------------
      INTEGER   ERROR
      INTEGER   NOROOM, IFIL
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DTVD.INC'
      DATA NOROOM /6/
C-----------------------------------------------------------------------
      ERROR = NOROOM
      DO 20 IFIL = 6,EFIL
         IF (FILTAB(POLUN,IFIL).EQ.9) GO TO 990
         IF (FILTAB(POLUN,IFIL).EQ.0) GO TO 30
 20      CONTINUE
      GO TO 999
 30   FILTAB(POLUN,IFIL) = 9
      CALL TVOPEN (WBUFF, ERROR)
      IF (ERROR.NE.0) GO TO 999
 990  ERROR = 0
      FILTAB(POIOP,IFIL) = 3
      FILTAB(POFIN,IFIL) = TVIND
 999  RETURN
      END
