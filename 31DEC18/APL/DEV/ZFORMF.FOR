      SUBROUTINE ZFORMF (STR)
C-----------------------------------------------------------------------
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
C   Returns a string which will cause a form feed (page eject) to occur
C   when printed from within a text file.  Used by PRTLIN.
C   In/out (Common LPINIT) :
C      LPEXST   L       In: if false, return a ' ' for the FF
C                       Out: true
C   Output:
C      STR      C*(*)   String filled with trailing blanks.
C   Generic version - for ASCII computers, returns a value of 12, base
C   10 (014, base 8) in the first character.  Used to avoid a form feed
C   for the first call, but this leads to appended print files not being
C   separated by real form feeds.  The AIPS F2PS filter will not produce
C   a blank page when the file begins with a form feed.
C-----------------------------------------------------------------------
      CHARACTER STR*(*)
C
      INTEGER   IFF
C     LOGICAL   LPEXST
C     COMMON /LPINIT/ LPEXST
      DATA IFF /12/
C-----------------------------------------------------------------------
C                                       blank fill
      STR = ' '
C                                       form feed
      STR(:1) = CHAR (IFF)
C
 999  RETURN
      END
