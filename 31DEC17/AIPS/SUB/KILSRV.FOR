      SUBROUTINE KILSRV
C-----------------------------------------------------------------------
C! Function to shut down X-windows type TEK, MSG, TV servers
C# POPS Graphics TV
C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2002
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
C   Kills TEK, MSG, TV servers
C-----------------------------------------------------------------------
      INTEGER   IERR, I, WORK(2), MSGTER, MSGIND, MSGSAV
      CHARACTER MESSAG*80, TMPNAM*6
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTKS.INC'
      DATA MSGTER /6/
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      MSGSUP = 32000
C                                       TEKtronix emulator
      IF (NTKDEV.GT.0) THEN
C                                       Open the TEK
         CALL ZTKOPN (IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'TKKILL: TEKTRONIX OPEN ERROR'
            CALL MSGWRT (6)
C                                       only server implementations
         ELSE
            IF (TKTYPE.NE.0) THEN
               I = 3
               CALL ZPUTCH (I, WORK(1), 1)
               CALL ZPUTCH (I, WORK(1), 2)
               CALL ZFIO ('WRIT', TKLUN, TKFIND, 2, WORK, IERR)
               IF (IERR.NE.0) THEN
                  MSGTXT = 'TKKILL: TEKTRONIX WRITE ERROR'
                  CALL MSGWRT (6)
                  END IF
               END IF
C                                       close
            CALL ZTKCLS (IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'TKKILL: TEKTRONIX CLOSE ERROR'
               CALL MSGWRT (6)
               END IF
            END IF
         END IF
C                                       kill TV servers too
      IF (NTVDEV.GT.0) CALL YDIE
C                                       Message server
      TMPNAM = TSKNAM
      TSKNAM = 'KILSRV'
      CALL ZMSGOP (MSGTER, MSGIND, 0, MESSAG, IERR)
      TSKNAM = TMPNAM
      IF (IERR.EQ.0) THEN
         MESSAG = ' '
         MESSAG(1:1) = CHAR (3)
         MESSAG(2:2) = CHAR (3)
         CALL ZTTYIO ('WRIT', MSGTER, MSGIND, 72, MESSAG, IERR)
         CALL ZMSGCL (MSGTER, MSGIND, IERR)
         END IF
C
      MSGSUP = MSGSAV
C
 999  RETURN
      END
