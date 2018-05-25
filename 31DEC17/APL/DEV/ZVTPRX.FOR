      SUBROUTINE ZVTPRX (FIND, BUFSW, NTPDEV, THOSTS, THLEN, NTH,
     *                   TPFIND, HBUF, IERR)
C-----------------------------------------------------------------------
C! does IO for server (real tape) to client (Virtual-tape), close/reopen
C# Tape Z
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   ZVTPRX performs "I/O" over some communication mechanism to a
C   cooperating program with the intention of driving a real tape device
C   controlled by this program.
C   Inputs:
C      FIND    I      FTAB location for socket to remote machine
C      BUFSW   I      Number 32-bit integers total data to send
C                     If BUFSW > 0, write to client; if <= 0, read
C                     from client machine.
C      NTPDEV  I      Tape device number (used on error close only)
C      THOSTS  C*(*)  Names or wildcards for authorized hosts
C      THLEN   I      Length of a hostname (fixed)
C      NTH     I      Number of hosts to check
C                       See TPMON.FOR local include for details
C   In/Out:
C      TPFIND  I      FTAB pointer for tape device: > 0 => it needs
C                     to be closed on error.  Returns 0, if that done.
C      HBUF    I(*)   Buffer: in FITS standard 16 bit
C                        (1,2) : subroutine name packed ASCII
C                        (3)   : BUFSW - 5
C                        (4)   : BUFSR
C                        (5)   : error returned to client
C                        (6..) : extra data
C   Output:
C      IERR    I      Error code: 0 => ok.
C   Generic version - does little except call ZVTPX3
C-----------------------------------------------------------------------
      INTEGER   FIND, BUFSW, NTPDEV, TPFIND, HBUF(*), THLEN, NTH, IERR
C                                       See also TPMON.FOR local include
      HOLLERITH THOSTS(*)
C
      INTEGER   ICOUNT, IER, TPLUN, JER
      CHARACTER MSG*80
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
      ICOUNT = 0
 10   CALL ZVTPX3 (FTAB(FIND+NMOFF), BUFSW, HBUF, IERR)
      IF (IERR.EQ.0) GO TO 999
C                                       try to survive
      IF (IERR.NE.4) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (7)
         END IF
C                                       EOF or read error
      IF (ICOUNT.LT.100) THEN
         ICOUNT = ICOUNT + 1
C                                       Close TP: lost client didn't
C                                       close the device
         IF (TPFIND.GT.0) THEN
            MSGTXT = 'ZVTPRX: UNEXPECTED EOF'
            CALL MSGWRT (7)
            MSGTXT = 'ZVTPRX: Having to close open Tape device'
            CALL MSGWRT (7)
            TPLUN = FTAB(TPFIND)
            IF ((TPLUN.GT.0) .AND. (TPLUN.LE.50) .AND. ((NTPDEV.LE.0)
     *         .OR. (TPLUN.EQ.NTPDEV+30))) THEN
               CALL ZTPCLS (TPLUN, TPFIND, IER)
               IF (IER.GT.0) THEN
                  WRITE (MSGTXT,1015) 'ZTPCLS', IER
                  CALL MSGWRT (7)
C                                       dismount on non-eof error
               ELSE IF ((IERR.NE.4) .AND. (NTPDEV.GT.0)) THEN
                  ICOUNT = ICOUNT + 9
                  MSGTXT = 'ZVTPRX: Dismount tape device on error'
                  CALL MSGWRT (7)
                  CALL ZMOUNT (.FALSE., NTPDEV, 0, ' ', 0, MSG, JER,
     *               IER)
                  IF ((JER.NE.0) .OR. (IER.NE.0)) THEN
                     MSGTXT = MSG
                     IF (JER.NE.0) CALL MSGWRT (7)
                     WRITE (MSGTXT,1015) 'ZMOUNT', IER
                     CALL MSGWRT (7)
                  END IF
                     END IF
               TPFIND = 0
               END IF
            END IF
C                                       close connection, get new client
         CALL ZVTPGC (THOSTS, THLEN, NTH, FTAB(FIND+NMOFF), IERR)
         IF (IERR.EQ.0) GO TO 10
            WRITE (MSGTXT,1020) IERR
            CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ZVTPX3 RETURNS ERROR CODE',I7)
 1015 FORMAT ('ZVTPRX: FORCED CALL ',A,' RETURNS ERROR',I7)
 1020 FORMAT ('ZVTPGC (GET CLIENT) RETURNS ERROR CODE',I7)
      END
