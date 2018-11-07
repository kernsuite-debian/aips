      SUBROUTINE ZXHELP (TAG, IERR)
C-----------------------------------------------------------------------
C! display hypertext help
C# Z
C-----------------------------------------------------------------------
C;  Copyright (C) 1996
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
C   Display hypertext help referenced by TAG in a suitable browser
C   Inputs:
C      TAG      C*(*)   Help tag
C   Output:
C      IERR     I       Error return code: 0 => no error
C                          1 => error displaying help
C   Unix version - calls ZXHLP2
C-----------------------------------------------------------------------
      CHARACTER TAG*(*)
      INTEGER   IERR
C
      CHARACTER CMD*64
C
      INTEGER   ITRIM
      EXTERNAL  ITRIM
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Assemble command line
      CMD = 'ZXHLP2 ' // TAG
C                                       Inform the user what is
C                                       happening (since this may take
C                                       some time)
      MSGTXT = 'Displaying help on ' // TAG // ' in hypertext viewer'
      CALL MSGWRT (3)
      MSGTXT = 'This may take a few seconds'
      CALL MSGWRT (3)
C                                       Do it
      CALL ZSHCMD (ITRIM (CMD), CMD, 8, '/dev/tty', IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'COULD NOT LOAD HELP TEXT INTO THE VIEWER'
         CALL MSGWRT (8)
         IERR = 1
         END IF
C
      END


