      SUBROUTINE ZPATH (LLEN, LOGFIL, FLEN, FULNAM, FACT, IERR)
C-----------------------------------------------------------------------
C! convert a file name 'Logical:file' to full path name
C# Z2 IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002, 2004
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
C   Translate a file specification of the form "logical:file" to a
C   full path name.  "/..." absolute path name is also allowed.
C   Inputs:
C      LLEN     I          Length of "logfil" (1-relative)
C      LOGFIL   H*(*)      File specification (may contain a valid,
C                          colon terminated logicalvariable)
C      FLEN     I          Length of "fulnam" (1-relative)
C   Output:
C      FULNAM   H*(flen)   Full path name (blank filled)
C      FACT     I          Position of last non-blank in "fulnam"
C                          (1-relative)
C      IERR     I          Error return code: 0 => no error
C                              1 => improper combination of inputs
C                              2 => no translation for "logical"
C                              3 => "fulnam" too short
C                              4 => logical name required
C   Generic version  - stub
C-----------------------------------------------------------------------
      INTEGER   LLEN, FLEN, FACT, IERR
      HOLLERITH LOGFIL(*), FULNAM(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 1
      MSGTXT = 'ZPATH REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
      CALL CHR2H (FLEN, MSGTXT, 1, FULNAM)
C
 999  RETURN
      END
