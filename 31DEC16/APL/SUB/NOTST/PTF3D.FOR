      SUBROUTINE PTF3D (FDVEC, TBIND, NEXT, LINE, BUFFER, NBYTE, IERR)
C-----------------------------------------------------------------------
C! Copies 8-bit bytes to tape.
C# Tape FITS
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
C   copies the next NBYTE bytes (8-bit) from LINE to the
C   TAPIO stream pointed to by FDVEC.  A tape record will be written
C   if necessary.  The buffer is zeroed after the data is written.
C    Inputs:
C     FDVEC(50)   I    File Descriptor vector used by TAPIO.  File
C                      must be in active read state.
C     NBYTE       I    Number of bytes to move.
C   Input/Output:
C     TBIND       I    Pointer in BUFFER returned by TAPIO.
C     NEXT        I    Pointer in BUFFER for next read (bytes).
C     LINE(*)     I    Output byte stream
C     BUFFER(*)   I    TAPIO buffer
C   Output:
C     IERR        I    Error code, 0=> OK, else TAPIO error.
C                      10 => encountered problem with pointers
C-----------------------------------------------------------------------
      INTEGER   FDVEC(50), NBYTE, TBIND, NEXT, BUFFER(*), LINE(*),
     *   IERR
      INTEGER   NCOPY, NMOV, LOOP, NZERO, POINT, NBYTLF, EOB
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (NBYTE.LE.0) GO TO 999
C                                       Setup
      POINT = 1
      NMOV = NBYTE
C                                       Copy as many as needed
 10   EOB = 1 +  FDVEC(2)
         NBYTLF = EOB - NEXT
         NCOPY = NMOV
         IF (NBYTLF.LT.NCOPY) NCOPY = NBYTLF
C                                       Trap problem
         IF (NEXT.GT.EOB) GO TO 900
C                                       Copy bytes
         CALL ZBYMOV (NCOPY, POINT, LINE, NEXT, BUFFER(TBIND))
         NEXT = NEXT + NCOPY
         IF (NCOPY.GE.NMOV) GO TO 999
C                                       More to do
         POINT = POINT + NCOPY
         NMOV = NMOV - NCOPY
C                                       Write record
         CALL TAPIO ('WRIT', FDVEC, BUFFER, TBIND, IERR)
         NEXT = 1
C                                       Zero buffer.
         NZERO = (FDVEC(2) * 8 - 1) / NBITWD + 1
         DO 50 LOOP = 1,NZERO
            BUFFER(TBIND+LOOP-1) = 0
 50         CONTINUE
         IF (IERR.EQ.0) GO TO 10
      GO TO 999
C                                       Problem
 900  IERR = 10
      WRITE (MSGTXT,1900) NEXT, EOB
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('PTF3D: NEXT (',I6,') AND EOB (',I6,') ARE INCONSISTENT')
      END
