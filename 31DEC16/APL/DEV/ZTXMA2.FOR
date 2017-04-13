      SUBROUTINE ZTXMA2 (FLEN, FILSPC, NMAX, IEXT, NNAM, NAMES, IERR)
C-----------------------------------------------------------------------
C! find all file names matching a given wildcard specification
C# Z2 Text
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C  Find all file names that match a given wild card specification.
C  Inputs:
C     FLEN     I          Length in characters of "filspc"
C     FILSPC   H*(FLEN)   File specification
C     NMAX     I          Maximum number of names
C     IEXT     I          Extension disposition indicator:
C                            0 => remove extension
C                            1 => retain extension
C  Output:
C     NNAM     I          Number of matching names found
C     NAMES    H*12(NMAX)  Array of matching names (H for Macro, C)
C     IERR     I          Error return code: 0 => no error
C                            1 => no matching names found
C                            2 => error opening or closing directory
C  Generic version - stub
C-----------------------------------------------------------------------
      INTEGER   FLEN, NMAX, IEXT, NNAM, IERR
      HOLLERITH FILSPC(*), NAMES(*)
C
      INCLUDE 'INCS:DMSG.INC'
C--------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZTXMA2: REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (8)
C
 999  RETURN
      END
