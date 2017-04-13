      SUBROUTINE ZTPCLD (LUN, FCB, IERR)
C-----------------------------------------------------------------------
C! close pseudo-tape disk file
C# Tape
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
C   Close a sequential disk file "device".
C   Inputs:
C      LUN   I      Logical unit number
C      FCB   I(*)   File control block for opened disk file involved
C   Output:
C      IERR  I      Error return code; 0 => no error
C   Generic version (uses simple Fortran CLOSE).
C-----------------------------------------------------------------------
      INTEGER   LUN, FCB(*), IERR
C
      INTEGER   IOSVAL
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
C                                       Close the file.
      CLOSE (UNIT=LUN, IOSTAT=IOSVAL)
      IERR = IOSVAL
C                                       Record IOSTAT return value in
C                                       the file control block.
      FCB(1+FCBERR) = IOSVAL
C
 999  RETURN
      END
