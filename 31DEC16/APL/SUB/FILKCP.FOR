      SUBROUTINE FILKCP (ILUN, OLUN, IERR)
C-----------------------------------------------------------------------
C! copy keywords from one open file to another
C# IO-WaWa
C-----------------------------------------------------------------------
C;  Copyright (C) 2006
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
C   WaWa IO system: copy all header keywords from input to output files
C   Inputs:
C      ILUN   I   Input file (open) logical unit number
C      OLUN   I   Output file (open) logical unit number
C   Outputs:
C      IERR   I   Error code
C-----------------------------------------------------------------------
      INTEGER   ILUN, OLUN, IERR
C
      INTEGER   IFIL, OFIL
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 10
C                                       find input file
      DO 10 IFIL = 1,EFIL
         IF (FILTAB(1,IFIL).EQ.ILUN) GO TO 15
 10      CONTINUE
      WRITE (MSGTXT,1000) 'Input', ILUN
      CALL MSGWRT (7)
      GO TO 999
C                                       find output file
 15   DO 20 OFIL = 1,EFIL
         IF (FILTAB(1,OFIL).EQ.OLUN) GO TO 30
 20      CONTINUE
      WRITE (MSGTXT,1000) 'Output', OLUN
      CALL MSGWRT (7)
      GO TO 999
C                                       so do it
 30   CALL KEYCOP (FILTAB(POVOL,IFIL), FILTAB(POCAT,IFIL),
     *   FILTAB(POVOL,OFIL), FILTAB(POCAT,OFIL), IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FILKCP: ',A,' FILE LUN',I4,' NOT OPEN IN FILTAB')
      END


