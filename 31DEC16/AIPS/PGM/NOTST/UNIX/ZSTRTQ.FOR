      PROGRAM ZSTRTQ
C-----------------------------------------------------------------------
C! determine parameters under which qmngr is to run
C# Z System
C-----------------------------------------------------------------------
C;  Copyright (C) 2010
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
C   ZSTRTQ determines what 'AIPSx' we will be (if any)
C-----------------------------------------------------------------------
      CHARACTER VERSIN*24, AIPSN*6, PRGNAM*6, POPSNO*1
      INTEGER   IERR, IPOPS, PID(4), MYPRIO
      LOGICAL   ACTIVE
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGNAM /'ZSTRTQ'/
C-----------------------------------------------------------------------
C                                       Set values especially NINTRN.
      CALL ZDCHIN (.TRUE.)
C                                       Copy PRGNAM into TSKNAM.
      TSKNAM = PRGNAM
C                                       Get default version.
      CALL ZMYVER
      VERSIN = VERNAM(1:3)
      IPOPS = NINTRN + 2
      CALL ZEHEX (IPOPS, 1, POPSNO)
      AIPSN = 'QMNGR' // POPSNO
      CALL FILL (4, 0, PID)
      CALL ZTACTQ (AIPSN, PID(2), ACTIVE, IERR)
      IF (ACTIVE) THEN
         MSGTXT = AIPSN // ' IS ALREADY ACTIVE'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Set execution priority.
 50   CALL ZSTRTP (IPOPS, MYPRIO, IERR)
      WRITE (MSGTXT,1050) POPSNO, RLSNAM
      CALL MSGWRT (4)
C                                       Activate AIPSx (x = MYPOPS).
      AIPSN = 'QMNGR'
      CALL ZACTV8 (AIPSN, IPOPS, VERSIN, PID, IERR)
C                                       Never expect to get here, but
C                                       just in case.
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         CALL MSGWRT (8)
         END IF
C
 999  STOP
C-----------------------------------------------------------------------
 1050 FORMAT ('Begin QMNGR number ',A1,' (release of ',A7,')')
 1060 FORMAT ('ZSTRTQ: ACTIVATION ERROR = ',I6)
      END
