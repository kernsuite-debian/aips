      SUBROUTINE ZV20C2 (FCB, IERR)
C-----------------------------------------------------------------------
C! close Comtal Vision 1/20 TV device
C# Z2 TV-IO
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
C   ZV20C2 closes Comtal Vision 1/20 TV device called by ZV20CL which
C   does the FTAB stuff.
C   In/out
C      FCB    I(*)   File control block
C   Output:
C      IERR   I      error code: 0 -> no error
C                       1 -> Deaccess or Deassign error
C   VMS version  - Andy Lubenow, Jan. 1985
C   A.L. version opened TVLUN to logical TVDEVn and never did anything
C   with it except close it.  He then passed a channel assignment around
C   in a special COMMON.  I think he did this to avoid AIPS' FTAB etc.
C   He hard-coded the device name as ZBA0: - use this as TVDEV1. (Eric)
C-----------------------------------------------------------------------
      INTEGER   FCB(*), IERR
C
      INTEGER   JERR, SYS$DASSGN
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
      JERR = SYS$DASSGN (%VAL(FCB(1+FCBFD)))
      IF (JERR.EQ.1) THEN
         FCB(1+FCBERR) = 0
         IERR = 0
      ELSE
         FCB(1+FCBERR) = JERR
         IERR = 1
         END IF
      FCB(1+FCBFD) = 0
C
 999  RETURN
      END
