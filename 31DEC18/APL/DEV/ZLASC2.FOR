      SUBROUTINE ZLASC2 (FLEN, FILNAM, DELFIL, IERR)
C-----------------------------------------------------------------------
C! spool a closed laser printer print/plot file
C# Z2 Printer Plot-util
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
C   close and spool a file for printing a plot on a laser printer
C   Inputs:
C      FLEN     I       length of FILNAM to use (for Macro, C, ...)
C      FILNAM   H*(*)   print/plot file name
C      DELFIL   I       0 => keep the file after printing
C                       > 0 => delete the file afetr DELFIL seconds
C                              delay
C   Output
C      IERR     I       error code: 0 => okay, else system error
C   Generic version - stub
C-----------------------------------------------------------------------
      HOLLERITH FILNAM*(*)
      INTEGER   FLEN, DELFIL, IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZLASC2 REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END
