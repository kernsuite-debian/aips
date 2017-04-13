      SUBROUTINE QINIT (I1, I2, I3)
C-----------------------------------------------------------------------
C! FPS AP version: Initialize AP, waits for higher priority tasks.
C# AP-util
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
C   FPS AP version
C   APINIT with a delay for all jobs other than NPOPS = 1
C   if there are any AP tasks of lower NPOPS value currently running.
C   Inputs:
C      I1   I   Dummy
C      I2   I   Dummy
C   Output:
C      I3   I   AP assigned. (Positive indicates a real AP, i.e. to be
C               rolled)
C   Output to common /BPROLC/
C           TRUEAP   L  True if a real AP (to be rolled)
C           XTLAST   R  Real time AP assigned (min).
C           DELTIM   R  Time interval between rolls (min).
C           DELAY    R  Time to delay task (seconds).
C-----------------------------------------------------------------------
      INTEGER   I1, I2, I3
C
      INTEGER*2 J1, J2, J3
      INTEGER   ICOUNT, IERR, LP, I, J, MCNT(15), MAXCNT, ITIME(3),
     *   PID(4), II, JJ3
      CHARACTER ANAME*6
      LOGICAL  ACTIVE
      INCLUDE 'INCS:DAPT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBPR.INC'
      INCLUDE 'INCS:VAPT.INC'
C-----------------------------------------------------------------------
C                                       release AP just in case we own
      CALL APRLSE
C                                       true AP no 2nd memory
      KAP2WD = 0
C                                       delay for FPS init cycle time
      DELAY = 1.05
      CALL ZDELAY (DELAY, IERR)
C                                       check priority
      DELAY = 10.0
      LP = NPOPS - 1
C                                       Check for higher priority jobs
      IF (LP.LE.0) GO TO 50
         IF (TIMEAP(2).LE.1.) TIMEAP(2) = 1.0
         IF (TIMEAP(3).LT.0.0) TIMEAP(3) = 0.0
         DO 10 II = 1,LP
            I = NPOPS - II
            MCNT(II) = 6. * (TIMEAP(2)*I + TIMEAP(3)*(I*I - 1)) + 0.1
            IF (II.EQ.NINTRN+1) MCNT(II) = 0
 10         CONTINUE
         MAXCNT = MCNT(1)
         DO 30 ICOUNT = 1,MAXCNT
            DO 20 I = 1,LP
               IF (MCNT(I).LE.0) GO TO 20
                  DO 15 J = 1,NAPNAM
                     CALL BLDTNM (APTASK(J), I, ANAME, IERR)
                     CALL FILL (4, 0, PID)
                     CALL ZTACTQ (ANAME, PID, ACTIVE, IERR)
                     IF (ACTIVE) GO TO 25
 15                  CONTINUE
 20            CONTINUE
C                                       none: go do it
            GO TO 50
C                                       delay and try again
 25         CALL ZDELAY (DELAY, IERR)
            DO 30 I = 1,LP
               MCNT(I) = MCNT(I) - 1
 30            CONTINUE
 35         CONTINUE
C                                       Ask system for device
 50   J1 = 0
      J2 = 0
      CALL APINIT (J1, J2, J3)
      JJ3 = J3
C                                       Check for multiple AP request.
      IF (JJ3.EQ.-3) GO TO 60
      IF (JJ3.LT.0) GO TO 50
C                                       Got it apparently
 60   I3 = ABS (JJ3)
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   CALL ZPRIO ('UPPP',IERR)
C                                       Fill values into /BPROLC/
      TRUEAP = .TRUE.
C                                       Delay time = 10*NPOPS sec.
      DELAY = 2.0 * NPOPS + 8.0
C                                       Time between rolls = 5 min.
      DELTIM = TIMEAP(1)
      IF (DELTIM.LT.1.0) DELTIM = 1.0
      IF (DELTIM.GT.60.0) DELTIM = 60.0
C                                       Set time.
      CALL ZTIME (ITIME)
      XTLAST = (ITIME(1)) * 60.0 + ITIME(2) + ITIME(3) / 60.0
C
 999  RETURN
      END
