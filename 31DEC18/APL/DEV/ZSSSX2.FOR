      SUBROUTINE ZSSSX2 (FCB, OP, OPSWAP, DAT, NWORDS, BUFFER, NSWORD,
     *   ISTAT)
C-----------------------------------------------------------------------
C! write to/ read from SUN-Screen-Server device
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
C   zsssx2 - send data and receive status from the sss "device".
C   Input:
C      fcb    I       file descriptor to write to
C      op     I       opcode
C      opswap I       opcode swapped
C      dat    I*2(4)  data
C      nwords I       number of words of buffer data to send
C   Output:
C      nsword I       number of words of data returned (often 0)
C      istat  I       status returned, 0 == success normally
C                        3,4 = write, read error
C   In/out:
C      buffer I*2(*)  on input contains nwords of I*2 data to send
C                     (if any), on return contains nsword of variable
C                     length data, if any.
C                     data sent should have values <= 255 since they
C                     are sent in byte form (IMRD is sent as word)
C                     data returned in word form can have larger value
C-----------------------------------------------------------------------
      INTEGER   FCB(*), OP, OPSWAP, NWORDS, NSWORD, ISTAT
      INTEGER   DAT(*), BUFFER(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 8
      MSGTXT = 'ZSSSX2: REQUIRES DEVELOPMENT; GENERIC IS A STUB'
      CALL MSGWRT (8)
C
 999  RETURN
      END
