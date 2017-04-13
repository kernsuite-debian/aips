      SUBROUTINE ZTTCLS (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! close a terminal device
C# Z2 Terminal
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
C   Close a terminal - to avoid recursion, don't call MSGWRT
C   Inputs:
C      LUN    I   Logical unit number (only 5 or 6 valid)
C      FIND   I   Pointer into FTAB
C   Output:
C      IERR   I   Error return code: 0 => no error
C                    1 => error - bad LUN
C   VMS version - can return Fortran error in FTAB(find+1)
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   JERR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
      CLOSE (UNIT=LUN, IOSTAT=JERR)
      IF (JERR.EQ.0) THEN
         IERR = 0
         FTAB(FIND+1+NFCBER) = 0
      ELSE
         IERR = 1
         FTAB(FIND+1+NFCBER) = JERR
         END IF
 
C
 999  RETURN
      END
