      SUBROUTINE ZTQSPY (DOALL, TLIST)
C-----------------------------------------------------------------------
C! display AIPS account or all processes running on the system
C# Z System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998
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
C   Displays information on the user's terminal regarding AIPS account
C   originated processes or all processes running on the system.
C   Inputs:
C      DOALL   R        > 0.0 => display all processes
C   Output:
C      TLIST   I(256)   Scratch buffer (not used in generic)
C   Generic version - uses ZTQSP2.
C-----------------------------------------------------------------------
      REAL      DOALL
      INTEGER   TLIST(256)
C
      INTEGER   DAT(3), TIM(3), MSLEV, I, JTRIM, NPROC
      CHARACTER DATE*12, TIME*8
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSLEV = 1
      IF (NPOPS.GT.NINTRN) MSLEV = 2
      IF (ISBTCH.EQ.32000) MSLEV = 2
C                                       Get local date and time.
      CALL ZDATE (DAT)
      CALL ZTIME (TIM)
      CALL TIMDAT (TIM, DAT, TIME, DATE)
C                                       Just AIPS or all processes.
      I = JTRIM (HSTNAM)
      IF (DOALL.GT.0.0) THEN
         WRITE (MSGTXT,1000) HSTNAM(:I), DATE, TIME
      ELSE
         WRITE (MSGTXT,1001) HSTNAM(:I), DATE, TIME
         END IF
C                                       Display header (terminal only)
      CALL MSGWRT (MSLEV)
C                                       Figure out max pops number
      NPROC = NINTRN + NBATQS + 1
C                                       Display processes
      CALL ZTQSP2 (DOALL, MSLEV, NPROC)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('All  processes on ',A,3X,A,'at ',A8)
 1001 FORMAT ('AIPS processes on ',A,3X,A,'at ',A8)
      END
