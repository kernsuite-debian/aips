      SUBROUTINE DTTIME (POINT, COMENT)
C-----------------------------------------------------------------------
C! displays elapsed CPU and real times since last call to DTINIT
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2012
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
C   DTTIME will print the elapsed real time, cpu time and IO count since
C   an init call, along with a comment or a call number supplied by
C   the calling routine.
C   Inputs:
C      POINT    I      Number to appear in print: < 0 => init
C                         0 means print COMENT instead.
C      COMENT   C*8    String comment to appear if POINT is 0.
C-----------------------------------------------------------------------
      INTEGER   POINT
      CHARACTER COMENT*(*)
C
      CHARACTER LOMENT*8
      REAL      LSTTIM, LSTCPU, TIME, CPU
      INTEGER   LSTIOC, NOW(3), IOCNT
      INCLUDE 'INCS:DMSG.INC'
      SAVE LSTTIM, LSTCPU, LSTIOC
      DATA LSTTIM, LSTCPU, IOCNT /0.0, 0.0, 0/
C-----------------------------------------------------------------------
C                                       init
      IF ((LSTTIM.LE.0.0) .OR. (LSTCPU.LE.0.0) .OR. (POINT.LT.0)) THEN
         CALL ZTIME (NOW)
         LSTTIM = NOW(3) + 60.0 * NOW(2) + 3600.0 * NOW(1)
         CALL ZCPU (LSTCPU, LSTIOC)
C                                       display
      ELSE
         LOMENT = COMENT
         CALL ZTIME (NOW)
         TIME = NOW(3) + 60.0 * NOW(2) + 3600.0 * NOW(1)
         TIME = TIME - LSTTIM
         CALL ZCPU (CPU, IOCNT)
         IOCNT = IOCNT - LSTIOC
         CPU = CPU - LSTCPU
         IF (POINT.EQ.0) THEN
            WRITE (MSGTXT,1000) LOMENT, TIME, CPU, IOCNT
         ELSE
            WRITE (MSGTXT,1001) POINT, TIME, CPU, IOCNT
            END IF
         CALL MSGWRT (5)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A8,': SECS REAL TIME=',F9.0,' CPU=',F9.2,
     *   ' IO CNT=',I8)
 1001 FORMAT ('PT',I6,': SECS REAL TIME=',F9.0,' CPU=',F9.2,
     *   ' IO CNT=',I8)
      END
