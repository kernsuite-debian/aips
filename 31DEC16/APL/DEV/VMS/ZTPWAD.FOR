      SUBROUTINE ZTPWAD (FCB, IERR)
C-----------------------------------------------------------------------
C! "wait" for IO operation to complete on pseudo-tape disk file (ZTPMID)
C# Z2 Tape FITS
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
C   Wait until I/O operation is complete: for sequential pseudo-tape
C   disk files.
C   Inputs:
C      FCB    I(*)   File I/O control block
C   Output:
C      IERR   I      Error return  0 => ok
C                       3 => IO error
C                       4 => end of file
C   VMS version: does not have to wait for disks
C   IOSTAT < 0 => EOF; = 36 => attempt to access nonexistent record
C-----------------------------------------------------------------------
      INTEGER   FCB(*), IERR
C
      INTEGER   JERR, EOFERR
      INCLUDE 'INCS:DZCH.INC'
      DATA EOFERR /36/
C-----------------------------------------------------------------------
C                                       I/O error
      JERR = FCB(1+FCBERR)
      IF (JERR.EQ.0) THEN
         IERR = 0
      ELSE
         IERR = 3
         IF ((JERR.LT.0). OR. (JERR.EQ.EOFERR)) IERR = 4
         END IF
C
 999  RETURN
      END
