      SUBROUTINE EVTRDY (LOPCOD, ILUN, IFIND, IBUF, NBUF, IRET)
C----------------------------------------------------------------------
C! Send or receive a 'READY' XDR event
C# IO-remote
C----------------------------------------------------------------------
C;  Copyright (C) 1997
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
C   Send or receive a 'READY' XDR event
C   Inputs:
C      LOPCOD      C*4      Opcode ('SEND' or 'RECV')
C      ILUN        I        LUN for socket I/O (assumed already open)
C      IFIND       I        FTAB index for socket
C      IBUF        I(*)     XDR work buffer
C      NBUF        I        Dimension of IBUF
C   Output:
C      IRET        I        Return code (0=> ok; else error)
C----------------------------------------------------------------------
      CHARACTER LOPCOD*4
      INTEGER   ILUN, IFIND, NBUF, IRET
      INTEGER   IBUF(NBUF)
C
      CHARACTER LREADY*8, LEVENT*8
      REAL      VERS, VERR
      INTEGER   NBYTES, NREAD, IERR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LREADY /'READYEVT'/
      DATA VERS /1.0/
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Case opcode of:
C                                       a) 'SEND'
      IF (LOPCOD.EQ.'SEND') THEN
C                                       Encode XDR header
         CALL EVTHDR ('ENCODE', LREADY, VERS, IBUF, NBUF, NBYTES, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Send event
         CALL ZRWRIT (ILUN, IFIND, NBYTES, IBUF, IERR)
         IF (IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
C                                       b) 'RECV'
      ELSE IF (LOPCOD.EQ.'RECV') THEN
C                                       Receive next event
         NREAD = NBITWD * NBUF / 8 - 4
         CALL ZRREAD (ILUN, IFIND, NREAD, IBUF, IERR)
         IF (IERR.NE.0) THEN
            IRET = 3
            WRITE (MSGTXT,1200) IERR
            GO TO 990
            END IF
C                                       Decode XDR header
         CALL EVTHDR ('DECODE', LEVENT, VERR, IBUF, NBUF, NBYTES, IERR)
         IF (IERR.NE.0) THEN
            IRET = 4
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Check event type and version
         IF ((LEVENT.NE.LREADY).OR.(VERS.NE.VERR)) THEN
            IRET = 5
            WRITE (MSGTXT,1500) LEVENT, VERR
            GO TO 990
            END IF
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C                                       Exit
 999  RETURN
C----------------------------------------------------------------------
 1050 FORMAT ('EVTRDY: ERROR',I4,' RETURNED BY EVTHDR')
 1100 FORMAT ('EVTRDY: ERROR',I4,' SENDING XDR READY EVENT')
 1200 FORMAT ('EVTRDY: ERROR',I4,' READING XDR EVENT')
 1500 FORMAT ('EVTRDY: UNEXPECTED XDR EVENT: ',A8,' VER: ',F5.2)
      END
