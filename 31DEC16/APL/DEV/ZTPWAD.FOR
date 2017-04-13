      SUBROUTINE ZTPWAD (FCB, IERR)
C-----------------------------------------------------------------------
C! "wait" for IO operation to complete on pseudo-tape disk file (ZTPMID)
C# Tape FITS
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
C   Wait until an I/O operation is complete for sequential pseudo-tape
C   disk files and return its status.
C   Inputs:
C      FCB    I(*)   File control block for opened pseudo-tape disk
C                    file involved
C   Output:
C      IERR   I      Error return code: 0 => no error
C                       3 => I/O error
C                       4 => end of file
C                       7 => wait service error
C   NOTE: EOFERR value will vary between systems.  If synchronous I/O,
C   no need to wait.
C   Generic version (stubbed).
C-----------------------------------------------------------------------
      INTEGER   FCB(*), IERR
C
      INTEGER   JERR, EOFERR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
C                                       System dependent EOFERR value:
C                                       VMS =  36
C                                       COS => doesn't work this way
C                                       UNIX (Alliant, SUN) = -1
C                                       UNIX (Convex) = 134
      DATA EOFERR /-1/
C-----------------------------------------------------------------------
C                                       Return wait service error until
C                                       this routine is developed.
      IERR = 7
      WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
      IF (IERR.NE.0) GO TO 999
C                                       Generic code:
      IERR = 0
C                                       System error code from the file
C                                       control block.
      JERR = FCB(1+FCBERR)
C                                       I/O error?
      IF (JERR.NE.0) THEN
         IERR = 3
C                                       End of file?
         IF ((JERR.LT.0). OR. (JERR.EQ.EOFERR)) IERR = 4
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTPWAD: THIS ROUTINE REQUIRES LOCAL DEVELOPMENT')
      END
