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
C   VAX version.  Uses $ to avoid CR after prompt.
C-----------------------------------------------------------------------
      CHARACTER IPC*1, BUFF*(*)
C
      INTEGER   IERR, LUNCRT, OUTLUN
      DATA LUNCRT, OUTLUN /5, 6/
C-----------------------------------------------------------------------
      IERR = 0
      WRITE (OUTLUN,1000,ERR=90) IPC
      READ (LUNCRT,1010,ERR=90) BUFF
      GO TO 999
C                                       Read/write error.
 90   IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
C                                       The $ suppreses CR LF
 1000 FORMAT (1X,A1,$)
 1010 FORMAT (A)
      END
