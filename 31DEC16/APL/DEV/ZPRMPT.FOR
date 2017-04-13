      SUBROUTINE ZPRMPT (IPC, BUFF, IERR)
C-----------------------------------------------------------------------
C! prompt user and read 80-characters from CRT screen
C# Z Terminal
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
C   ZPRMPT prompts user on CRT screen and reads a line.
C   Input:
C      IPC     I      prompt character.
C   Output:
C      BUFF    C*80   line of user input.
C      IERR    I      error code:   0 => ok.
C                        1 => read/write error.
C   Generic version - stub
C-----------------------------------------------------------------------
      CHARACTER IPC*1, BUFF*(*)
      INTEGER   IERR
C
C-----------------------------------------------------------------------
      IERR = 2
      WRITE (6,1000)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (' ZPRMPT REQUIRES LOCAL DEVELOPMENT')
      END
