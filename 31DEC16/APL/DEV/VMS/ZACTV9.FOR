      SUBROUTINE ZACTV9 (FLEN, FULNAM, PLEN, CPNAME, IDBG, PID, SYSERR,
     *   IERR)
C-----------------------------------------------------------------------
C! starts requested load module under requested process name
C# Z2 System
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
C   Creates a sub process with a name equal to CPNAME a detached process
C   named 'QMNGR'.  Uses load module FULNAM.
C   Inputs:
C      FLEN     I          Length in characters of "FULNAM"
C      FULNAM   C*(FLEN)   Full path name of the required executable
C                          module
C      PLEN     I          Length in characters of "CPNAME"
C      CPNAME   C*(plen)   Desired process name
C      IDBG     I          1 => start in DEBUGger if possible
C                          0 => use no DEBUGger if possible
C                          Ignore on CPNAME='AIPSn','BATERn'
C   In/Out:
C      PID      I(4)       Process identification information:
C                          In:  PID(1) user number for systems which
C                                      use it (= 0 otherwise and on all
C                                      AIPSB invocations)
C                          Out: PID(2) process ID number from "fork"
C                               PID(3) not used
C                               PID(4) not used
C   Output:
C      SYSERR   I          System error code (i.e., errno) if error
C      IERR     I          Error return code: 0 => no error
C                              1 => error
C   VMS Version
C-----------------------------------------------------------------------
      INTEGER   FLEN, PLEN, IDBG, PID(4), SYSERR, IERR
      CHARACTER FULNAM*(*), CPNAME*(*)
C
      CHARACTER VERS*64
      INTEGER   BASPRI, SYS$CREPRC, LIB$SPAWN, ILEN, I
      INCLUDE '($PRCDEF)'
C-----------------------------------------------------------------------
      IERR = 2
C                                       Batch system control program
      IF (CPNAME(1:5).EQ.'QMNGR') THEN
C                                       Translate AIPS_VERSION as far
C                                       as possible
         CALL ZTRLOG (12, 'AIPS_VERSION', 64, VERS, ILEN, IERR)
C                                       Trim closing bracket
         ILEN = ILEN - 1
C                                       Create a detached process
C                                       Use interactive priority for
C                                       now. It will drop when a task
C                                       starts.
         BASPRI = 4
         SYSERR = SYS$CREPRC (PID(2), 'SYS$SYSTEM:LOGINOUT.EXE',
     *      VERS(:ILEN)//'SYSTEM.VMS]QMNGR.COM',
     *      VERS(:ILEN)//'ERRORS]QMNGR.LOG', , , ,
     *      CPNAME(1:PLEN),%VAL(BASPRI), , ,
     *      %VAL(PRC$M_DETACH))
C                                       Subprocess.
      ELSE
C                                       Spawn subprocess.
C                                       Use parent's SYS$INPUT and
C                                       SYS$OUTPUT to prevent
C                                       terminal allocation.
         IF (IDBG.LE.0) THEN
            SYSERR = LIB$SPAWN ('RUN/NODEBUG '//FULNAM(1:FLEN),,, 1,
     *         CPNAME(1:PLEN),,,,,,,)
         ELSE
            SYSERR = LIB$SPAWN ('RUN '//FULNAM(1:FLEN),,, 1,
     *         CPNAME(1:PLEN),,,,,,,)
            END IF
         END IF
      IF (SYSERR.EQ.1) THEN
         IERR = 0
         SYSERR = 0
         END IF
C
 999  RETURN
      END
