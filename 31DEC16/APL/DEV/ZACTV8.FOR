      SUBROUTINE ZACTV8 (NAME, INPOPS, RVERSN, PID, IERR)
C-----------------------------------------------------------------------
C! activate the requested program, returning process ID information
C# Z System
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
C   Activate the specified program and return the activated process
C   identification information.
C   Inputs:
C      NAME     C*6    Program name
C      INPOPS   I      POPS # for the task to use
C      RVERSN   C*48   Logical name or fully qualified name of the
C                      directory from which to get the required
C                      executable module
C   In/Out:
C      PID      I(4)   Process identification information used directly
C                      by subsequent calls to ZTACTQ where
C                      In:  PID(1)   = user number for systems that use
C                                      it (= 0 otherwise and on all
C                                      AIPSB invocations)
C                      Out: PID(2-4) = process ID number(s) as needed
C   Output:
C      IERR     I      Error return code: 0 => no error
C                         1 => program name too long
C                         2 => activation error
C   Common: DMSG.INC
C      DBGAIP   I      > 10 => start tasks in DEBUG mode if possible
C   Generic version = stub
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), RVERSN*(*)
      INTEGER   INPOPS, PID(4), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZACTV8: REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END
