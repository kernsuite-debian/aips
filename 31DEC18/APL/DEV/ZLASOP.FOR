      SUBROUTINE ZLASOP (OP, LUN, FILNAM, IERR)
C-----------------------------------------------------------------------
C! open a laser printer print/plot file
C# Z2 Printer Plot-util
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
C   Open a file for printing a plot on a laser printer device.
C   Inputs:
C      OP       C*4     Operation code: 'OPEN', 'POPN'
C      LUN      I       Logical unit number for the laser device
C      FILNAM   C*(*)   File name
C   Output
C      IERR     I       Error code: 0 => okay
C   Generic version - simple Fortran open.
C-----------------------------------------------------------------------
      CHARACTER OP*4, FILNAM*(*)
      INTEGER   LUN, IERR
C
C-----------------------------------------------------------------------
C                                        long (RECL = 160) lines
      IF (OP.EQ.'POPN') THEN
         OPEN (UNIT=LUN, STATUS='NEW', FILE=FILNAM, IOSTAT=IERR)
C                                        regular lines
      ELSE
         OPEN (UNIT=LUN, STATUS='NEW', FILE=FILNAM, IOSTAT=IERR)
         END IF
C
 999  RETURN
      END
