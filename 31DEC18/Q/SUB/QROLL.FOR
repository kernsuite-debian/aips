      SUBROUTINE QROLL (NWORD, BUFFER, BUFSZ, IRET)
C-----------------------------------------------------------------------
C! Determines if time to roll AP, if so calls APROLL.
C# AP-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   QROLL copies the first NWORDs of AP MD memory to a scratch file,
C   gives up the AP, does a task delay for DELAY, grabs an AP and loads
C   the scratch file back into the AP.  If NWORD <= 0, then the AP is
C   not rolled and the AP is given up and then reassigned.
C   NOTE: APROLL is called by QROLL and uses commom /CFILES/ for the
C   scratch file.  NOTE: LUN 8 is used for I/O and a AIPS "map" I/O
C   slot is opened if the AP memory is actually rolled.
C***********************************************************************
C   IMPORTANT NOTE:  QROLL (and APROLL) work properly only for floating
C   point data.  Integer values rolled will not be restored correctly.
C***********************************************************************
C   Inputs: NWORD    I       Number of words of AP memory to save.
C                            If <= 0 the contents of the AP memory are
C                            not saved.
C           BUFFER   R(*)    Work buffer.
C           BUFSZ    I       Size of BUFFER in bytes.
C   Inputs from COMMON /BPROLC/  (set by QINIT)
C           TRUEAP   L       True if a real AP (to be rolled)
C           XTLAST   D       Real time AP assigned (min).
C           DELTIM   D       Time interval between rolls (min).
C           DELAY    R       Time to delay task (seconds).
C   Output: IRET     I       Return error code, 0 => OK
C                               2 => couldn't reload AP.
C-----------------------------------------------------------------------
      INTEGER   BUFSZ, NWORD, IRET
      REAL      BUFFER(*)
      INTEGER   LUN, ITIME(3)
      DOUBLE PRECISION    XTIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN /8/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if true AP
      IF (.NOT.TRUEAP) GO TO 999
C                                       Check if time to roll out AP.
         CALL ZTIME (ITIME)
         XTIME = (ITIME(1)) * 60.00 + ITIME(2) + ITIME(3) / 60.0
C                                       Check if crossed midnight.
         IF (XTIME.LT.XTLAST) XTLAST = XTLAST - 1440.
         IF ((XTIME-XTLAST).LT.DELTIM) GO TO 999
C                                       Time to roll.
C                                       Reset last time.
         XTLAST = XTIME
C                                       Roll AP
C                                       Make sure /CFILES/ values OK.
 100     IF (NSCR.LE.0) NSCR = 0
         IF (NSCR.GT.21) NSCR = 0
         CALL APROLL (LUN, NWORD, BUFFER, BUFSZ, DELAY, IRET)
         IF (IRET.LE.1) GO TO 110
            WRITE (MSGTXT,1100) IRET
            GO TO 990
 110     IRET = 0
         GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('QROLL: APROLL ERROR',I3)
      END
