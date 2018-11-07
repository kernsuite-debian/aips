      SUBROUTINE ZLPCL2 (LLEN, LPRFIL, DELFIL, PRTMAX, SYSERR, IERR)
C-----------------------------------------------------------------------
C! queue a file to the line printer and delete
C# Z2 Printer
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
C   Queue a line printer file to a printer, then delete it
C   Inputs:
C      LLEN     I       Length of "lprfil" in characters
C      LPRFIL   H*(*)   Name of the line printer file to queue
C      DELFIL   I       Disposition indicator code:
C                          0 => keep after printing
C                         >0 => delete after printing and delay of
C                               DELFIL (seconds)
C      PRTMAX   I       Length of a line printer page
C   Output:
C      SYSERR   I       System error code
C      IERR     I       Error return code: 0 => no error
C                          1 => error
C   Generic version  - stub
C-----------------------------------------------------------------------
      INTEGER   LLEN, DELFIL, PRTMAX, SYSERR, IERR
      HOLLERITH LPRFIL(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'ZLPCL2: REQUIRES LOCAL DEVELOPMENT'
      IERR = 2
      CALL MSGWRT (9)
C
 999  RETURN
      END
