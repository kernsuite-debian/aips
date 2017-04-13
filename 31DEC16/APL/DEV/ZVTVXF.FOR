      SUBROUTINE ZVTVXF (BUFSW, BUFSR, HBUF, IERR)
C-----------------------------------------------------------------------
C! sends data from the client (virtual TV) to server (real TV)
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
C   ZVTVXF performs "I/O" over some communication mechanism to a
C   cooperating program with the intention of driving a real TV device
C   controlled by the cooperating program.
C   Inputs:
C      BUFSW  I      Number 16-bit integers extra data to send
C      BUFSR  I      # 16-bit integers of extra data to receive
C   In/Out:
C      HBUF   I(*)   Buffer:  in FITS standard 16 bit
C                       (1,2) :  READ or WRIT
C                       (3,5) :  subroutine name packed ASCII
C                       (6)   :  BUFSW
C                       (7)   :  BUFSR (TV IERR returned)
C                       (8)   : error return
C                       (9..) :  extra data
C   Output:
C      IERR   I      Error code:  0 => ok.
C   Generic version - does little except call ZVTVX2
C-----------------------------------------------------------------------
      INTEGER   BUFSW, BUFSR, HBUF(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DTVD.INC'
C-----------------------------------------------------------------------
      CALL ZVTVX2 (FTAB(TVIND+NMOFF), BUFSW, BUFSR, HBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
      ELSE
         CALL ZI16IL (1, 8, HBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (7)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZVTVX2 RETURNS ERROR CODE',I7)
 1010 FORMAT ('REMOTE TV RETURNS ERROR CODE',I7)
      END
