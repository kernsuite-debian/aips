      SUBROUTINE ZMOUN2 (MOUNT, IDRIVE, IDENS, NTAPED, NPOPS, NUSER,
     *   MSG, SYSERR, IERR)
C-----------------------------------------------------------------------
C! mount or dismount magnetic tape device
C# Z2 Tape
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
C   Issue software mount or dismount for a given tape drive - the lowest
C   level Z routine called by ZMOUNT.
C   Inputs:
C      MOUNT    I      Operation: 1 => mount, 0 => dismount
C      IDRIVE   I      Drive number
C      IDENS    I      Density
C      NTAPED   I      Number of tape drives
C      NPOPS    I      Pops number
C      NUSER    I      User AIPS number
C   Output:
C      MSG      H*80   Error message buffer
C      SYSERR   I      System error code (for ZERROR call)
C      IERR     I      Error return code: 0 => no error
C                        1 => error
C   Generic UNIX version - always successful
C-----------------------------------------------------------------------
      INTEGER   MOUNT, IDRIVE, IDENS, NTAPED, NPOPS, SYSERR, IERR,
     *          NUSER
      HOLLERITH MSG(*)
C-----------------------------------------------------------------------
      IERR = 0
      CALL CHR2H (80, ' ', 1, MSG)
      SYSERR = 0
C
 999  RETURN
      END
