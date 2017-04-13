      SUBROUTINE ZARGC2 (FCB, IERR)
C-----------------------------------------------------------------------
C! close an ARGS TV device
C# Z2 IO-TV
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
C   Close the ARGS TV device associated with File Control Block
C   Input/output:
C      FCB    I(*)   File control block: in - Contains channel number
C                    out -  channel slot zeroed.
C   Outputs:
C      IERR   I      Error return code. 0=success, 1 = error.
C----------------------------------------------------------------------
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
