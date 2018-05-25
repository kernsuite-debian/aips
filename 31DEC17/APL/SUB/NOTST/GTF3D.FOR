      SUBROUTINE GTF3D (FDVEC, TBIND, NEXT, BUFFER, NBYTE, LINE, IERR)
C-----------------------------------------------------------------------
C! Copies real-world bytes from a tape buffer, reading if necessary.
C# IO-util Tape
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
C   copies the next NBYTE bytes (8-bit, NOT half short integer) from the
C   input TAPIO stream pointed to by FDVEC to LINE.  A new tape record
C   will be read if necessary.
C   Inputs:
C      FDVEC(50)   I    File Descriptor vector used by TAPIO.  File
C                       must be in active read state.
C      NBYTE       I    Number of bytes to move.
C   Input/Output:
C      TBIND       I    Buffer pointer from TAPIO
C      NEXT        I    Pointer in BUFFER for next read (bytes).
C      BUFFER(*)   I    TAPIO buffer
C   Output:
C      LINE(*)     I    Output byte stream.
C      IERR        I    Error code, 0=> OK, else TAPIO error.
C                       10 => encountered problem with pointers
C-----------------------------------------------------------------------
      INTEGER   FDVEC(50), NBYTE, TBIND, NEXT, BUFFER(*), LINE(*), IERR
      INTEGER   NCOPY, NMOV, POINT, NBYTLF, EOB
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (NBYTE.LE.0) GO TO 999
C                                       Setup
      POINT = 1
      NMOV = NBYTE
C                                       Determine number of bytes in
C                                       current record.
 100     EOB = 1 + FDVEC(2)
         NBYTLF = EOB - NEXT
         NCOPY = NMOV
         IF (NBYTLF.LT.NCOPY) NCOPY = NBYTLF
C                                       Trap problem
         IF (NEXT.GT.EOB) GO TO 900
C                                       Copy bytes
         CALL ZBYMOV (NCOPY, NEXT, BUFFER(TBIND), POINT, LINE)
         NEXT = NEXT + NCOPY
         IF (NCOPY.GE.NMOV) GO TO 999
C                                       More to do
         POINT = POINT + NCOPY
         NMOV = NMOV - NCOPY
C                                       Read next record
         CALL TAPIO ('READ', FDVEC, BUFFER, TBIND, IERR)
         NEXT = 1
         IF (IERR.EQ.0) GO TO 100
      GO TO 999
C                                       Problem
 900  IERR = 10
      WRITE (MSGTXT,1900) NEXT, EOB
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('GTF3D: NEXT (',I6,') AND EOB (',I6,') ARE INCONSISTENT')
      END
