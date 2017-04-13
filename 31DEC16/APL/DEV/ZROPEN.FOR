      SUBROUTINE ZROPEN (LUN, FIND, SERVER, HOST, IRET)
C-----------------------------------------------------------------------
C! request a connection to a server
C# IO-remote Z
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2004
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
C   Request a connection to a server on the given LUN.
C   Inputs:
C      LUN      I        Logical unit number to use.
C      SERVER   C*(*)    Name of server program.  This is actually a
C                        logical name which evaluates to a service
C                        specification is system dependent form.
C      HOST     C*(*)    Name of the machine on which the server is
C                        running.  This is actually a logical name which
C                        evaluates to a network address in system
C                        dependent form.
C   Outputs:
C      FIND     I        Index of the FTAB entry for the opened channel.
C      IRET     I        Error return status:
C                           0 => channel opened successfully
C                           1 => server unknown
C                           2 => host unknown
C                           3 => invalid LUN
C                           4 => no room for new FTAB entry
C                           5 => LUN already opened
C                           6 => system error
C                          99 => other error
C  Generic version.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IRET
      CHARACTER SERVER*(*), HOST*(*)
C
      CHARACTER CHNAME*256, SRVNAM*128, HOSTNM*128
      INTEGER   HSTLEN, SRVLEN, FCBOFF, ITYP, INUM, IERR, I, ITRIM
      HOLLERITH HHOST(32), HSERV(32)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER    MAXLUN
      PARAMETER (MAXLUN = 50)
C-----------------------------------------------------------------------
      IRET = 0
C                                       Translate the server name:
      CALL ZTRLOG (LEN (SERVER), SERVER, LEN (SRVNAM), SRVNAM, I, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT, 1000) SERVER
         CALL MSGWRT (6)
         END IF
C                                       Translate the host name:
      IF (IRET.EQ.0) THEN
         CALL ZTRLOG (LEN (HOST), HOST, LEN (HOSTNM), HOSTNM, I, IERR)
         IF (IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT,1001) HOST
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Check LUN range:
      IF (IRET.EQ.0) THEN
         IF ((LUN.LT.1) .OR. (LUN.GT.MAXLUN)) THEN
            IRET = 3
            WRITE (MSGTXT, 1002) LUN, MAXLUN
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Check device type for the
C                                       specified LUN (an FCB is
C                                       required):
      IF (IRET.EQ. 0)THEN
         IF ((DEVTAB(LUN).EQ.1) .OR. (DEVTAB(LUN).EQ.3)) THEN
            IRET = 3
            WRITE (MSGTXT, 1003) LUN, DEVTAB(LUN)
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Form a name for the channel of
C                                       the form SERVER@HOST:
      IF (IRET.EQ.0) THEN
         SRVLEN = ITRIM (SERVER)
         HSTLEN = ITRIM (HOST)
C                                         Make sure that the channel
C                                         name will fit in the storage
C                                         allocated for it:
         IF ((SRVLEN+HSTLEN+1).GT.256) THEN
            IRET = 99
            WRITE (MSGTXT,1004)
            CALL MSGWRT (6)
         ELSE
            CHNAME = SERVER(1:SRVLEN) // '@' // HOST(1:HSTLEN)
            END IF
         END IF
C                                       Create an entry for the channel
C                                       in the AIPS common file table:
      IF (IRET.EQ.0) THEN
         CALL LSERCH ('OPEN', LUN, FIND, .FALSE., IERR)
C                                       Non-mapped (ie. single-buffered)
C                                       I/O is assumed.
         IF (IERR.EQ.3) THEN
            IRET = 4
            WRITE (MSGTXT, 1005)
            CALL MSGWRT (6)
         ELSE IF (IERR.EQ.2) THEN
            IRET = 5
            WRITE (MSGTXT, 1006) LUN
            CALL MSGWRT (6)
         ELSE IF (IERR.NE.0) THEN
            IRET = 99
            WRITE (MSGTXT, 1007) IERR
            CALL MSGWRT (6)
C                                       Calculate starting index of FCB:
         ELSE
            FCBOFF = FIND + NMOFF
            END IF
         END IF
C                                       Make the request:
      IF (IRET.EQ.0) THEN
         SRVLEN = ITRIM (SRVNAM)
         HSTLEN = ITRIM (HOSTNM)
         CALL CHR2H (128, SRVNAM, 1, HSERV)
         CALL CHR2H (128, HOSTNM, 1, HHOST)
         CALL ZROPN2 (FTAB(FCBOFF), SRVLEN, HSERV, HSTLEN, HHOST, IERR)
         IF (IERR.NE.0) THEN
            IRET = 6
            WRITE (MSGTXT, 1008)
            CALL MSGWRT (6)
            CALL ZERROR ('ZROPN2', FTAB(FCBOFF+FCBERR), CHNAME,
     *         FTAB(FCBOFF), .TRUE.)
            END IF
         END IF
C                                       Save the channel name:
      IF (IRET.EQ.0) THEN
         CALL FSERCH (FIND, ITYP, INUM, IERR)
         IF (IERR.NE.0) THEN
            IRET = 99
            WRITE (MSGTXT, 1009) IERR
            CALL MSGWRT (6)
         ELSE IF (ITYP.NE.2) THEN
            IRET = 99
            WRITE (MSGTXT, 1010) ITYP
            CALL MSGWRT (6)
         ELSE
            NONNAM(INUM) = CHNAME
            END IF
         END IF

C                                       If any errors occurred then
C                                       force the FTAB entry to be
C                                       cleared:
      IF (IRET.NE.0) THEN
         CALL LSERCH ('CLOS', LUN, FIND, .FALSE., IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZROPEN: UNKNOWN SERVER ', A20)
 1001 FORMAT ('ZROPEN: UNKNOWN HOST ', A20)
 1002 FORMAT ('ZROPEN: LUN ', I4, ' IS OUT OF RANGE (1 TO ', I2, ')')
 1003 FORMAT ('ZROPEN: LUN ', I2, ' HAS WRONG TYPE (', I1, ')')
 1004 FORMAT ('ZROPEN: INSUFFICIENT STORAGE FOR CHANNEL NAME')
 1005 FORMAT ('ZROPEN: NO SPACE LEFT IN FILE TABLE')
 1006 FORMAT ('ZROPEN: LUN ', I2, ' ALREADY OPEN')
 1007 FORMAT ('ZROPEN: ERROR ', I3, ' FROM LSERCH')
 1008 FORMAT ('ZROPEN: COULD NOT OPEN CHANNEL')
 1009 FORMAT ('ZROPEN: ERROR ', I3, ' FROM FSERCH')
 1010 FORMAT ('ZROPEN: WRONG FILE TYPE (', I1, ')')
      END
