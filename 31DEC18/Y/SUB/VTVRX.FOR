      SUBROUTINE VTVRX (FIND, BUFSW, HBUF, IERR)
C-----------------------------------------------------------------------
C! does IO for server (real TV) to client (Virtual-TV) incl close/reopen
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
C   VTVRX performs "I/O" over some communication mechanism to a
C   cooperating program with the intention of driving a real TV device
C   controlled by this program.
C   Inputs:
C      FIND    I      FTAB location for socket to remote machine
C      BUFSW   I      Number 16-bit integers total data to send
C                     If BUFSW > 0, write to client; if <= 0, read
C                     from client machine.
C   In/Out:
C      HBUF    I(*)   Buffer: in FITS standard 16 bit
C                        (1,2) : READ or WRIT
C                        (3,5) : subroutine name packed ASCII
C                        (6)   : BUFSW - 8
C                        (7)   : BUFSR
C                        (8)   : error returned to client
C                        (9..) : extra data
C   Output:
C      IERR    I      Error code: 0 => ok.
C   Generic version - does little except call ZVTVX3 except on error.
C   Then it must close the TV via TVCLOS and reopen with ZVTVGC.  This
C   is why it is in the Y subroutine area not a Z routine.  It should be
C   portable, the gory stuff is in the lower routines.
C-----------------------------------------------------------------------
      INTEGER   FIND, BUFSW, HBUF(*), IERR
C
      INTEGER   ICOUNT, SBUF(256), IER
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DTVD.INC'
C-----------------------------------------------------------------------
      ICOUNT = 0
 10   CALL ZVTVX3 (FTAB(FIND+NMOFF), BUFSW, HBUF, IERR)
      IF (IERR.EQ.0) GO TO 999
C                                       EOF
      IF ((IERR.EQ.4) .AND. (ICOUNT.LT.25)) THEN
         ICOUNT = ICOUNT + 1
C                                       Close TV: lost client didn't
         IF ((TVIND.GT.0) .AND. (TVIND2.GT.0) .AND. (TVLUN.EQ.9) .AND.
     *      (TVLUN2.EQ.8)) THEN
            CALL TVCLOS (SBUF, IER)
            IF (IER.GT.0) THEN
               WRITE (MSGTXT,1000) IER
               CALL MSGWRT (7)
               END IF
            END IF
C                                       close connection, get new client
         CALL ZVTVGC (FTAB(FIND+NMOFF), IERR)
         IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1010) IERR
      ELSE
         WRITE (MSGTXT,1011) IERR
         END IF
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VTVRX: FORCED CALL TO TVCLOS RETURNS ERROR',I7)
 1010 FORMAT ('VTVRX: ZVTVGC (GET CLIENT) RETURNS ERROR CODE',I7)
 1011 FORMAT ('VTVRX: ZVTVX3 RETURNS ERROR CODE',I7)
      END
