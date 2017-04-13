      SUBROUTINE ZPASWD (PASS, BUFF, IERR)
C-----------------------------------------------------------------------
C! prompt user and read 12-character password (invisible) from CRT
C# Z Terminal System
C-----------------------------------------------------------------------
C;  Copyright (C) 2004
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
C   ZPASWD prompts the user on his terminal with the prompt string
C   "Password: " and then reads back a 12-character "password" without
C   anything being visible on the screen.
C   Outputs:
C      PASS   C*12     Password - 12 unpacked characters: left
C                                 justigied and blank filled.
C      BUFF   I(256)   Scratch buffer (if needed)
C      IERR   I        Error code: 0 => ok
C                         ??? => I/O error of some sort
C   Generic version - calls ZPRPAS with HOLLERITH or CHARACTER (VMS)
C-----------------------------------------------------------------------
      CHARACTER PASS*12
      INTEGER   BUFF(256), IERR
C
      HOLLERITH HPASS(3)
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (SYSTYP.EQ.'VMS') THEN
         CALL ZPRPAS (PASS, BUFF, IERR)
      ELSE
         CALL ZPRPAS (HPASS, BUFF, IERR)
         CALL H2CHR (12, 1, HPASS, PASS)
         END IF
C
 999  RETURN
      END
