      SUBROUTINE ZTPOPR (LUN, FIND, IVOL, PNAME, OPER, IERR)
C-----------------------------------------------------------------------
C! open  remote tape or pseudo-tape device
C# Tape Z2
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2002, 2004
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
C   Open a remote tape drive on a computer supporting the TPMON protocol
C   for sequential, "map" (double buffered, asynchronous) I/O or open
C   a pseudo-tape sequential disk file.  Exclusive use and wait to open
C   are assumed.
C   Inputs:
C      LUN     I       Logical unit number (129 > LUN >= 129 - NTAPED
C                         => tape, else disk)
C      FIND    I       Pointer into FTAB (open)
C      IVOL    I       Tape drive #
C      PNAME   C*48    tape logical name (AMT0n) or
C                      disk physical file name (logical:file)
C      OPER    C*4     'READ' => read only or 'WRIT' => read/write
C   Output:
C      IERR    I       Error return code: 0 => no error
C                         2 => file not found
C                         4 => exclusive use denied
C                         6 => other open errors
C   Common:
C      TPDNAM  C       Remote machine name for pseudo-tapes
C   Generic version - checks logical (real tapes) then calls ZVTPO2.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IVOL, IERR
      CHARACTER PNAME*48, OPER*4
C
      INTEGER   ITRIM, I, J, IBUF(20), NBACK, NBACKE
      CHARACTER XLATED*256, LNAME*48
      HOLLERITH HLATED(64), HBUF(20)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IBUF, HBUF)
      DATA NBACKE /0/
C-----------------------------------------------------------------------
C                                       tape
      IF ((LUN.LT.129) .AND. (LUN.GE.129-NTAPED)) THEN
         I = ITRIM (PNAME)
         CALL ZTRLOG (I, PNAME, 256, XLATED, J, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'ZMOUNT: UNABLE TO TRANSLATE LOGICAL NAME ='
            CALL MSGWRT (7)
            MSGTXT = '''' // PNAME(:I) // ''''
            CALL MSGWRT (7)
            MSGTXT = 'DID YOU DO A MOUNT???'
            CALL MSGWRT (7)
            GO TO 999
            END IF
         LNAME = ' '
C                                       pseudo-tape
      ELSE
         XLATED = 'aipsmt0:' // TPDNAM
         LNAME = PNAME
         END IF
C                                       call ZVTPO2: open socket
      I = ITRIM (XLATED)
      CALL CHR2H (256, XLATED, 1, HLATED)
      CALL ZVTPO2 (FTAB(FIND+MOFF), I, HLATED, IERR)
      IF (IERR.NE.0) THEN
          WRITE (MSGTXT,1000) IERR
          CALL MSGWRT (7)
          GO TO 999
          END IF
C                                       Open tape device
C                                       pack buffer: header
      CALL ZCLC8 (8, 'ZTPOPN  ', 1, HBUF)
      CALL ZILI32 (1, 15, 3, IBUF)
      CALL ZILI32 (1, 0, 4, IBUF)
      CALL ZILI32 (1, 0, 5, IBUF)
C                                       pack buffer: data
      CALL ZILI32 (1, LUN, 6, IBUF)
      CALL ZILI32 (1, IVOL, 7, IBUF)
      CALL ZCLC8 (4, OPER, 29, HBUF)
      CALL ZCLC8 (48, LNAME, 33, HBUF)
C
      NBACK = NBACKE
      CALL ZVTPX2 (FTAB(FIND+MOFF), 15, NBACK, IBUF, IERR)
      IF (IERR.EQ.4) THEN
         WRITE (MSGTXT,1025)
         CALL MSGWRT (7)
      ELSE IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1005) IERR
         CALL MSGWRT (7)
      ELSE
         CALL ZI32IL (1, 5, IBUF, IERR)
         IF (IERR.NE.0) THEN
            IF (XLATED(7:7).EQ.'0' .AND. IERR.EQ.8) THEN
               WRITE (MSGTXT, 1030)
            ELSE
               WRITE (MSGTXT,1010) IERR
               END IF
            CALL MSGWRT (7)
            END IF
         END IF
      IF ((IERR.EQ.0) .AND. (NBACK.NE.NBACKE)) THEN
         WRITE (MSGTXT,1015) NBACK, NBACKE
         CALL MSGWRT (6)
         END IF
C                                       Close on error
      IF (IERR.NE.0) THEN
         CALL ZVTPC2 (FTAB(FIND+MOFF), I)
         IF (I.NE.0) THEN
            WRITE (MSGTXT,1020) I
            CALL MSGWRT (7)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTPOPR: ZVTPO2 RETURNS ERROR CODE',I7)
 1005 FORMAT ('ZTPOPR: ZVTPX2 RETURNS ERROR CODE',I7)
 1010 FORMAT ('ZTPOPR: REMOTE TAPE SYSTEM RETURNS ERROR CODE',I7)
 1015 FORMAT ('ZTPOPR: WARNING - GOT BACK',I3,' WORDS, EXPECTED',I3)
 1020 FORMAT ('ZTPOPR: ERROR',I5,' CLOSING THE SOCKET TOO')
 1025 FORMAT ('ZTPOPR: AIPS TPHOSTS PERMISSION DENIED ON REMOTE HOST')
 1030 FORMAT ('ZTPOPR: FILE EXISTS ON REMOTE DISK, CANNOT OVERWRITE IT')
      END
