      SUBROUTINE FINDUV (STIME, DTIME, LASTVS, IRET)
C-----------------------------------------------------------------------
C! Finds the next time interval in UV data
C# PLOT CALIBRATION
C-----------------------------------------------------------------------
C;  Copyright (C) 1999, 2015
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
C   Finds the next time range of UV data.
C   Inputs:
C      DTIME    R   Time interval length (days)
C   In/Out:
C      STIME    R   Start time of next interval
C   Output:
C      LASTVS   I   Last vis number to search
C      IRET     I   Error: > 0 serious
C                      -1 no data found
C   DSEL.INC common
C      INITVS   I   In/out: first vis number to read
C-----------------------------------------------------------------------
      REAL      STIME, DTIME
      INTEGER   LASTVS, IRET
C
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   INFSTV, INANTS(MAXANT), INNANT, IERR, MSGSAV
      REAL      INTIME(8), RPARM(20), VIS(3,MAXCIF), ETIME
      SAVE VIS
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       save and reset input DSEL.incs
      CALL COPY (MAXANT, ANTENS, INANTS)
      CALL FILL (MAXANT, 0, ANTENS)
      INNANT = NANTSL
      NANTSL = 0
      INFSTV = INITVS
      LASTVS = 0
      CALL RCOPY (8, TIMRNG, INTIME)
      CALL RFILL (8, 0.0, TIMRNG)
      TIMRNG(1) = STIME
      TIMRNG(5) = 1.E10
      MSGSAV = MSGSUP
C                                       init I/O - get first record
      MSGSUP = 32000
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      MSGSUP = MSGSAV
      IF (IRET.GT.0) CALL MSGWRT (7)
      IF (IRET.EQ.0) CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) GO TO 999
      IF (IRET.EQ.-1) GO TO 900
C                                       set time range
      INFSTV = FSTVS3
      LASTVS = MAX (LASTVS, FSTVS3)
      STIME = RPARM(1+ILOCT)
      TEND = STIME + DTIME
      ETIME = TEND
C                                       read until exhausted
 10   CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) GO TO 999
      IF (IRET.EQ.0) THEN
         IF (RPARM(1+ILOCT).GT.ETIME) IRET = -1
         END IF
      IF (IRET.EQ.0) THEN
         LASTVS = MAX (LASTVS, FSTVS3)
         GO TO 10
         END IF
      IRET = 0
C                                       clos and restore
 900  CALL UVGET ('CLOS', RPARM, VIS, IERR)
      CALL RCOPY (8, INTIME, TIMRNG)
      CALL COPY (MAXANT, INANTS, ANTENS)
      NANTSL = INNANT
      INITVS = INFSTV
C
 999  RETURN
      END
