      SUBROUTINE YTCOMP (OLD, NEW, UPDATE)
C-----------------------------------------------------------------------
C! decide if a parameter has changed
C# Y0 TV-util
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
C   Check whether a value in a soft register has to be changed.  If so,
C   change it and set a flag indicating that the hard register must be
C   updated too.
C   Inputs:
C      OLD      I   Current value in soft register
C      NEW      I   New value
C   Output:
C      OLD      I   Put new value here too if necessary
C      UPDATE   L   Set true if update is needed, else leave alone
C   Generic - used only in DeAnza, not really a Y at all.
C-----------------------------------------------------------------------
      INTEGER   OLD, NEW
      LOGICAL   UPDATE
C-----------------------------------------------------------------------
      IF (OLD.NE.NEW) THEN
         OLD = NEW
         UPDATE = .TRUE.
         END IF
C
 999  RETURN
      END
