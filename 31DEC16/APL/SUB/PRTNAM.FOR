      SUBROUTINE PRTNAM (NAMS, MSGPRI)
C-----------------------------------------------------------------------
C! prints the contents of a WaWa-IO file Namestring
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
C   WaWa IO system: Print a file NAMESTRING at priority MSGPRI
C   Inputs:
C      NAMS     C*36   a namestring
C      MSGPRI   I      priority
C   Written by WaWa (4/21/81)
C-----------------------------------------------------------------------
      CHARACTER NAMS*36
      INTEGER   MSGPRI
C
      CHARACTER DNAMS*36, STAT*4, CNAMS*18, NAME*12, CLASS*6, PTYPE*2
      INTEGER   ERROR, CNO, SEQ, USEID, DISK
      LOGICAL   LSC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBUF.INC'
C-----------------------------------------------------------------------
C                                      Copy into scratch
      CALL PRENAM (NAMS, DNAMS, LSC)
      CALL WAWA2A (DNAMS, NAME, CLASS, SEQ, PTYPE, DISK, USEID)
C                                      Look for cat slot
      CNO = 1
      CALL CATDIR ('SRCH', DISK, CNO, NAME, CLASS, SEQ, PTYPE, USEID,
     *   STAT, WBUFF, ERROR)
      IF (MSGPRI.GT.5) THEN
         WRITE (MSGTXT,1000) DISK, CNO, USEID, PTYPE
      ELSE
         WRITE (MSGTXT,1001) DISK, CNO, USEID, PTYPE
         END IF
C                                       Packed vers of name
      CNAMS = NAME // CLASS
      CALL NAMEST (CNAMS, SEQ, MSGTXT(40:), ERROR)
      CALL MSGWRT (MSGPRI)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DISK',I2,' SLOT',I4,'  USER',I6,'  (',A2,')FILE: ')
 1001 FORMAT ('Disk',I2,' slot',I4,'  user',I6,'  (',A2,')file: ')
      END
