      SUBROUTINE ZSSSL2 (FCB, PNAME, LOCK, IERR)
C-----------------------------------------------------------------------
C! Calls lock server to lock/unlock TV device
C# TV-IO
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2004
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
C   Contact TV server to lock/unlock the Tv device
C   Assign an i/o channel for TVlock device, send lock/unlock command
C   and then deassign the channel,
C   Inputs:
C      PNAME  H*48  'logical' name of TV device. Contains a pathname
C                   for Unix domain sockets or a name of the form
C                   sssin:machine_name for Internet domain sockets.
C                   Must begin with the string "TVDEV".
C      LOCK   I     1 => lock, 0 => unlock
C   Outputs:
C      FCB    I(*)  File descriptor (part of FTAB)
C      IERR   I     Error code: 0 => success
C                      1 => failure
C                      2 => invalid device name
C   Generic version - stubbed
C-----------------------------------------------------------------------
      INTEGER   FCB(*), LOCK, IERR
      HOLLERITH PNAME(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'ZSSSL2 REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (8)
      IERR = 4
C
 999  RETURN
      END

