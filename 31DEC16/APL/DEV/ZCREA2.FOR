      SUBROUTINE ZCREA2 (PNAME, ABLOKS, QCREAT, ASIZE, SYSERR, IERR)
C-----------------------------------------------------------------------
C! create the specified disk file
C# Z2 IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2011
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
C  Create a disk file of the specified name and size with read/write
C  permission.
C  Inputs:
C     PNAME    H*48   Physical file name (H for lower level languages)
C     ABLOKS   I      Requested size of file in AIPS-blocks
C                        (256 local integers)
C     QCREAT   I      > 0 use quick file creation, else fill file w 0's
C  Output:
C     ASIZE    I      Actual size of file created in AIPS-bytes
C     SYSERR   I      System error code
C     IERR     I      Error return code: 0 => no error
C                        1 => file already exists
C                        2 => volume not found
C                        3 => insufficient space
C                        4 => other
C  Generic version - stub.
C-----------------------------------------------------------------------
      HOLLERITH PNAME(12)
      INTEGER   ABLOKS, QCREAT, ASIZE, SYSERR, IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZCREA2: REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END
