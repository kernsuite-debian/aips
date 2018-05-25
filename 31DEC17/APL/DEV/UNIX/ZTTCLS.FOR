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
C   Close a terminal - don't call MSGWRT to avoid recursion.
C   Inputs:
C      LUN    I   Logical unit number (only 5 or 6 valid)
C      FIND   I   FTAB pointer - set fortran error code in FTAB
C   Output:
C      IERR   I   Error return code: 0 => no error
C                    1 => error - bad LUN
C   Generic UNIX version - does not open/close terminals.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   IER
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
      FTAB(FIND+NFCBER) = 0
C                                       Check inputs.
      IF ((LUN.NE.5) .AND. (LUN.NE.6)) THEN
         IERR = 1
C                                       Don't keep opening and closing
C                                       the terminal (it's unnecessary
C                                       under UNIX; see ZTTOPN).
      ELSE
C                                       But check for MSGSERVER!
         IF (FTAB(FIND+NFCBFD).NE.0) THEN
            CALL ZTTCL2 (FTAB(FIND), IER)
            IERR = 0
         ELSE
            IERR = 0
            END IF
         END IF
C
 999  RETURN
      END
