      SUBROUTINE ZM70X2 (OPER, FCB, BUFF, NBYTES, IERR)
C-----------------------------------------------------------------------
C! read/write from/to IIS Model 70/75 device
C# TV-IO
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
C   Transfer "nbytes" of data between an IIS Model 70 TV device.
C   Inputs:
C      oper     C*4    Operation code "READ" or "WRIT"
C      fcb      I(*)   File control block for opened TV device
C      nbytes   I      Number of 8-bit bytes to be transferred
C   In/out:
C      buff     I(*)   I/O buffer
C   Output:
C      ierr     I*2    Error return code: 0 => no error
C                         2 => bad opcode
C                         3 => I/O error
C                         4 => end of file
C   Generic version - stub
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   FCB(*), NBYTES, BUFF(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZM70X2: REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END
