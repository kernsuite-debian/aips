      PROGRAM NOBAT
C-----------------------------------------------------------------------
C! Task to grab the AP for a specified time
C# AP-util Batch Service
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
C   Task NOBAT suspends itself for an amount of time specified by the
C   adverb DETIME.  The purpose of this is that AP tasks think that
C   NOBAT uses the AP and while it is running tasks with higher POPS
C   numbers have reduced access to the AP.
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      DETIME         DETIME        No. min to suspend before quiting.
C-----------------------------------------------------------------------
      CHARACTER ATASKS(2)*6, CTASK*6, PRGM*6
      LOGICAL   RQUICK, ACTIVE
      INTEGER   NPARM, IRET, IERR, I, PID(4)
      REAL      DETIME, DELAY, BUFF(256), XDELAY
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGM /'NOBAT '/
      DATA ATASKS /'AIPS  ', 'AIPSB '/
C-----------------------------------------------------------------------
C                                       Init I/O
      CALL ZDCHIN (.TRUE., BUFF)
C                                       Get input parameters.
      NPARM = 1
      CALL GTPARM (PRGM, NPARM, RQUICK, DETIME, BUFF, IRET)
      IF (IRET.EQ.0) GO TO 10
         IF (IRET.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IRET
            CALL MSGWRT (8)
C                                       Restart AIPS.
 10   IF (RQUICK) CALL RELPOP (IRET, BUFF, IERR)
      IF (IRET.NE.0) GO TO 990
      IRET = 0
      I = 1
      IF (NPOPS.GT.NINTRN+1) I = 2
      CALL BLDTNM (ATASKS(I), NPOPS, CTASK, IERR)
      PID(1) = NLUSER
      PID(2) = 0
C                                       Delay task.
      DELAY = DETIME * 60.0
      XDELAY = 60.0
 20   CONTINUE
         CALL ZDELAY (XDELAY, IERR)
         DELAY = DELAY - XDELAY
         IF (DELAY.LE.0.0) GO TO 990
            CALL ZTACTQ (CTASK, PID, ACTIVE, IERR)
            IF ((ACTIVE) .AND. (IERR.EQ.0)) GO TO 20
               WRITE (MSGTXT,1020)
               CALL MSGWRT (8)
               IRET = 1
C                                        Write end message
 990  CALL DIETSK (IRET, RQUICK, BUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR:',I7,'GETTING INPUT PARAMETERS')
 1020 FORMAT ('Stopping since my parent process is no longer active')
      END
