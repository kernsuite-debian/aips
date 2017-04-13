      SUBROUTINE ZBKTP1 (ZLUN, ZBKNAM, BAKTXT, NTAPE, ZMT0N, LZMT0N,
     *   IERR)
C-----------------------------------------------------------------------
C! initialize BACKUP to tape operation for BAKTP
C# Z Tape
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
C   Initialize the host environment in preparation for ZBKTP2.  If
C   OPCODE = 'INIT', initializes the tape too.
C   (used only by task BAKTP)
C   Inputs:
C      NTAPE    I       Tape drive number
C      BAKTXT   C*(*)   lowest file name for file listing (not used
C                       here, only in VMS now)
C   Outputs:
C      ZLUN     I       LUN to use
C      ZBKNAM   C*(*)   Command file name
C      ZMT0N    C*(*)   Tape name (translation of MT0n) n=NTAPE-1
C      LZMT0N   I       Actual length used in ZMT0N
C      IERR     I       Error code: 0 => okay
C   Generic stub
C-----------------------------------------------------------------------
      INTEGER   ZLUN, NTAPE, LZMT0N, IERR
      CHARACTER ZBKNAM*(*), BAKTXT*(*), ZMT0N*(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZBKTP1: REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (8)
C
 999  RETURN
      END
