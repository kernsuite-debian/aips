      SUBROUTINE ZBKTP2 (ZLUN, ZBKNAM, BAKTXT, BAKREC, IVOL, ICNO,
     *   NTAPE, ZMT0N, LZMT0N, IERR)
C-----------------------------------------------------------------------
C! write a cataloged file plus extensions to BACKUP tape in BAKTP
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
C   Host-dependent subroutine to write a file and its extensions to
C   tape using the hosts "backup" utility.
C   Inputs:
C      ZLUN     I       LUN for command file
C      ZBKNAM   C*(*)   Name of command file
C      BAKTXT   C*(*)   lowest file name for file listing
C      BAKREC   C*60    ?
C      IVOL     I       Disk number
C      ICNO     I       Catalog number
C      NTAPE    I       Tape drive number
C      ZMT0N    C*(*)   Tape name (translation of MT0n) n=NTAPE-1
C      LZMT0N   I       Actual length used in ZMT0N
C   Output:
C      IERR     I       Error code: 0 => okay, else ZSHCMD
C   Generic version - stub
C-----------------------------------------------------------------------
      INTEGER   ZLUN, IVOL, ICNO, NTAPE, LZMT0N, IERR
      CHARACTER ZBKNAM*(*), BAKTXT*(*), BAKREC*(*), ZMT0N*(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZBKTP2: REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (8)
C
 999  RETURN
      END
