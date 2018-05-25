      SUBROUTINE ZMOUNR (MOUNT, IDRIVE, RDRIVE, IDENS, LPOPS, NUSER,
     *   MSG, SYSERR, IERR)
C-----------------------------------------------------------------------
C! mount or dismount remote magnetic tape device
C# Z2 Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004, 2006
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
C   Issue software mount or dismount for a given tape drive - the lowest
C   level Z routine called by ZMOUNT.
C   Inputs:
C      MOUNT    I      Operation: 1 => mount, 0 => dismount
C      IDRIVE   I      Drive number - pseudo number used on local
C                      machine
C      RDRIVE   I      Drive number (on remote machine)
C      IDENS    I      Density
C      LPOPS    I      Pops number
C      NUSER    I      User AIPS number
C   Output:
C      MSG      C*80   Error message buffer - DO NOT USE MSGTXT
C      SYSERR   I      System error code (for ZERROR call)
C      IERR     I      Error return code: 0 => no error
C                         1 => error
C   Generic version - opens a virtual tape connection, sends the mount
C   or dismount command and closes the connection.
C-----------------------------------------------------------------------
      INTEGER   MOUNT, IDRIVE, RDRIVE, IDENS, LPOPS, SYSERR, IERR, NUSER
      CHARACTER MSG*80
C
      INTEGER   LUN, FIND, IBUF(26), JERR, I, JTRIM, ITRIM, J, NBACK,
     *   NBACKE, LRDRIV
      CHARACTER PNAME*48, XLATED*132, ICHR*1
      HOLLERITH HBUF(26), HLATED(32)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      EQUIVALENCE (IBUF, HBUF)
      DATA NBACKE /21/
C-----------------------------------------------------------------------
      MSG = ' '
      SYSERR = 0
C                                       open the tape
      LUN = 129 - IDRIVE
C                                       Allocate area in FTAB for LUN.
      CALL LSERCH ('OPEN', LUN, FIND, .TRUE., JERR)
      IF (JERR.NE.0) THEN
C                                       LUN already in use.
         IF (JERR.EQ.2) THEN
            IERR = 1
            WRITE (MSGTXT,1001) LUN
C                                       No room in FTAB for LUN.
         ELSE IF (JERR.EQ.3) THEN
            IERR = 5
            WRITE (MSGTXT,1002) LUN
         ELSE
            IERR = 6
            WRITE (MSGTXT,1000) JERR, 'OPEN', LUN
            END IF
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Real tape
      PNAME = 'AMT0x:'
      CALL ZEHEX (IDRIVE, 1, PNAME(5:5))
      I = ITRIM (PNAME)
      CALL ZTRLOG (I, PNAME, 128, XLATED, J, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ZMOUNR: UNABLE TO TRANSLATE LOGICAL NAME ='
         CALL MSGWRT (7)
         MSGTXT = '''' // PNAME(:I) // ''''
         CALL MSGWRT (7)
         MSGTXT = 'ZMOUNR: ZMOUNT/ZCRLOG MUST HAVE FAILED'
         CALL MSGWRT (7)
         GO TO 990
      ELSE IF (XLATED(:10).EQ.'DISMOUNTED') THEN
         IERR = 2
         MSGTXT =  'ZMOUNR: TAPE WAS NOT MOUNTED'
         CALL MSGWRT (7)
         GO TO 990
         END IF
C                                       remote drive number
      IF (MOUNT.GT.0) THEN
         LRDRIV = RDRIVE
      ELSE
         ICHR = XLATED(7:7)
         J = 0
         CALL ZHEX10 (ICHR, J, IERR)
         IF ((J.LE.0) .OR. (IERR.NE.0)) THEN
            IERR = 2
            MSGTXT = 'UNABLE TO DETERMINE REMOTE TAPE DRIVE NUMBER'
     *         // ' FROM LOGICAL NAME:'
            CALL MSGWRT (7)
            MSGTXT= XLATED
            CALL MSGWRT (7)
            GO TO 990
            END IF
         LRDRIV = J
         END IF
C                                       call ZVTPO2: open socket
      I = JTRIM (XLATED)
      CALL CHR2H (132, XLATED, 1, HLATED)
      CALL ZVTPO2 (FTAB(FIND+MOFF), I, HLATED, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (7)
         GO TO 990
         END IF
C                                       pack buffer: header
      CALL ZCLC8 (8, 'ZMOUNT  ', 1, HBUF)
      CALL ZILI32 (1, 5, 3, IBUF)
      CALL ZILI32 (1, 21, 4, IBUF)
      CALL ZILI32 (1, 0, 5, IBUF)
C                                       pack buffer: data
      CALL ZILI32 (1, MOUNT, 6, IBUF)
      CALL ZILI32 (1, LRDRIV, 7, IBUF)
      CALL ZILI32 (1, IDENS, 8, IBUF)
      CALL ZILI32 (1, LPOPS, 9, IBUF)
      CALL ZILI32 (1, NUSER, 10, IBUF)
C
      NBACK = NBACKE
      CALL ZVTPX2 (FTAB(FIND+MOFF), 5, NBACK, IBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (7)
      ELSE
         CALL ZI32IL (1, 5, IBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1025) IERR
            CALL MSGWRT (7)
            END IF
         END IF
      IF ((IERR.EQ.0) .AND. (NBACK.NE.NBACKE)) THEN
         WRITE (MSGTXT,1026) NBACK, NBACKE
         CALL MSGWRT (7)
         END IF
C                                       outputs
      CALL ZI32IL (1, 6, IBUF, SYSERR)
      CALL ZC8CL (80, 25, HBUF, MSG)
C                                       close
      CALL ZVTPC2 (FTAB(FIND+MOFF), JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1030) JERR
         CALL MSGWRT (7)
         IF (IERR.EQ.0) IERR = JERR
         END IF
C
 990  CALL LSERCH ('CLOS', LUN, FIND, .TRUE., JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZMOUNR: LSERCH ERROR ',I1,' FOR ',A4,' ON LUN = ',I3)
 1001 FORMAT ('ZMOUNR: LUN = ',I3,' ALREADY OPEN IN FTAB')
 1002 FORMAT ('ZMOUNR: NO ROOM IN FTAB FOR LUN = ',I3)
 1010 FORMAT ('ZMOUNR: UNABLE TO OPEN SOCKET TO REMOTE MACHINE, ERROR',
     *   I5)
 1020 FORMAT ('ZMOUNR: UNABLE TO TALK TO REMOTE TAPE DEVICE, ERROR',I5)
 1025 FORMAT ('ZMOUNR: UNABLE TO MOUNT REMOTE TAPE DEVICE, ERROR',I5)
 1026 FORMAT ('ZMOUNR: WARNING - GOT BACK',I3,' WORDS, EXPECTED',I3)
 1030 FORMAT ('ZMOUNR: UNABLE TO CLOSE SOCKET TO REMOTE MACHINE, ERROR',
     *   I5)
      END
