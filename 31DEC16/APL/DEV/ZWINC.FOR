      SUBROUTINE ZWINC (WCOLS)
C-----------------------------------------------------------------------
C! handles detection of window-size changes
C# IO-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1998-1999
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
C   Finds the width and height of the current xterm (or other terminal)
C   window and sets the height into gloabl common if requested.
C   Output:
C      WCOLS   I   Width of window in characters,
C                  returns values from 72 through 132 only
C   Programmer = Jeff Uphoff, April-July 1998.  Eric Greisen 7/99.
C-----------------------------------------------------------------------
      INTEGER   WCOLS
C
      INTEGER   WLINES
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      CALL ZWINC2 (WCOLS, WLINES)
C                                        if <=0 save in common
      IF (CRTMAX.LT.1) THEN
         IF (WLINES.GT.0) THEN
            CRTMAX = -WLINES
         ELSE
            IF (CRTMAX.GT.-10) CRTMAX = -24
            END IF
         END IF
      WCOLS = MIN (132, MAX (WCOLS, 72))
C
 999  RETURN
      END
