      SUBROUTINE ZTPCL2 (FCB, IERR)
C-----------------------------------------------------------------------
C! close a tape device
C# Z2 Tape
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
C   Close a tape drive - note this is always double buffer ("MAP")
C   Inputs:
C      FCB    I(*)   File control block for opened tape drive
C   Output:
C      IERR   I      Error return code: 0 => ok
C                       1 => error
C   VMS version
C-----------------------------------------------------------------------
      INTEGER FCB(*), IERR
C
      INTEGER   JERR, SYS$DASSGN
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
      JERR = SYS$DASSGN (%VAL(FCB(1+FCBFD)))
      IF (JERR.EQ.1) THEN
         FCB(1+FCBERR) = 0
         FCB(1+MFCB+FCBERR) = 0
         IERR = 0
      ELSE
         FCB(1+FCBERR) = JERR
         FCB(1+MFCB+FCBERR) = JERR
         IERR = 1
         END IF
      FCB(1+FCBFD) = 0
      FCB(1+MFCB+FCBFD) = 0
C
 999  RETURN
      END
