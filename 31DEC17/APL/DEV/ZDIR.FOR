      SUBROUTINE ZDIR (TYPE, RVERSN, FILE, FULNAM, IERR)
C-----------------------------------------------------------------------
C! build a full path name to files in AIPS-standard areas (HE, RU, ...)
C# Z2 Text IO-util
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
C   Given a file type (e.g., 'HE' for HELP, 'RU' for RUN, etc.), a
C   version (e.g., 'OLD:', 'NEW:', 'TST:', 'MYAREA' as a logical,
C   /aippgmr/khilldru as a literal, etc.) and a file name (e.g., UVSRT,
C   MX, etc.), build the full path name of the file (e.g.,
C   /AIPS/15JUL87/HELP/MX.HLP).
C   Inputs:
C      TYPE     C*2    File type
C      RVERSN   C*48   Version - directory path name)
C      FILE     C*(*)  File name (if a '.' is not found in the name, a
C                      default extension determined from TYPE)
C   Output:
C      FULNAM   C*(*)  Full path name of the file
C      IERR     I      Error return code: 0 => no error
C                         1 => invalid file type
C                         2 => no such logical
C                         3 => error in directory name
C   Generic version - stub
C-----------------------------------------------------------------------
      CHARACTER TYPE*2, RVERSN*48, FILE*(*), FULNAM*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 1
      MSGTXT = 'ZDIR REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END
