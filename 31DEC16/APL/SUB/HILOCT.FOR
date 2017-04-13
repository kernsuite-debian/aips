      SUBROUTINE HILOCT (OP, HLUN, HPTR, IERR)
C-----------------------------------------------------------------------
C! manipulates the history table, opening, closing, located an entry
C# History
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
C   HILOCT locates a history file in HITAB in /HICOM /
C   Inputs:
C      OP    C*4   'OPEN' assign vacant space
C                  'CLOS' remove from table
C                  'SRCH' find in table only
C      HLUN  I     lun of file
C   Output:
C      HPTR  I     pointer to first word of table entry
C      IERR  I     error code 0 => ok
C                             1 => LUN in use (OPEN)
C                             2 => no room (OPEN)
C                             3 => not found (CLOS< SRCH)
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   HLUN, HPTR, IERR
C
      INTEGER   NC, NP, I
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                        locate in HITAB
      NP = 1 - NHIWRD
      NC = 0
      DO 10 I = 1,NHIFIL
         NP = NP + NHIWRD
         IF (HLUN.EQ.HITAB(NP)) GO TO 20
         IF ((NC.EQ.0) .AND. (HITAB(NP).EQ.0)) NC = NP
 10      CONTINUE
C                                        OPEN
      IF (OP.NE.'OPEN') GO TO 900
         IF (NC.EQ.0) GO TO 910
            HPTR = NC
            HITAB(NC) = HLUN
            GO TO 999
C                                        found LUN in table
 20   IF (OP.EQ.'OPEN') GO TO 920
         HPTR = NP
         IF (OP.EQ.'CLOS') CALL FILL (NHIWRD, 0, HITAB(NP))
         GO TO 999
C                                        errors
C                                        LUN not found
 900  IERR = 3
      WRITE (MSGTXT,1900) HLUN
      GO TO 990
C                                        no room
 910  IERR = 2
      WRITE (MSGTXT,1910) HLUN
      GO TO 990
C                                        in use
 920  IERR = 1
      WRITE (MSGTXT,1920) HLUN
 990  CALL MSGWRT (7)
      HPTR = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('HILOCT: FILE',I6,' NOT OPEN IN HITAB')
 1910 FORMAT ('HILOCT: NO ROOM IN HITAB FOR', I6)
 1920 FORMAT ('HILOCT: FILE',I6,' ALREADY IN USE IN HITAB')
      END
