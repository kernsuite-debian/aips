      SUBROUTINE ZRMTAP (MACHIN, NT, TAPES, IERR)
C-----------------------------------------------------------------------
C! read tape list of remote machine
C# Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1999-2000, 2002
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
C   Open a link to TPMON1 on a remote machine, ask it for the list of
C   tapes supported on that machine, and close the link.
C   Inputs:
C      MACHIN   C*(*)      tape disk physical file name
C   Output:
C      NT       I          Number of non-blank strings
C      TAPES    C(36)*80   List of tape strings
C      IERR     I          Error return code: 0 => no error
C                            1 => LUN already in use
C                            2 => file not found
C                            3 => volume not found
C                            4 => exclusive use denied
C                            5 => no room for LUN in FTAB
C                            6 => other open errors
C   Generic version.
C-----------------------------------------------------------------------
      INTEGER   NT, IERR
      CHARACTER MACHIN*(*), TAPES(36)*80
C
      INTEGER   JERR, I, J, JTRIM, LUN, FIND, IBUF(725), IP, NBACK
      HOLLERITH HBUF(725), HLATED(64)
      LOGICAL   T
      CHARACTER XLATED*256, LNAME*48
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      EQUIVALENCE (IBUF, HBUF)
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Check inputs.
      NT = 0
      IERR = 6
      LUN = 30
C                                       Allocate area in FTAB for LUN.
      CALL LSERCH ('OPEN', LUN, FIND, T, JERR)
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
            WRITE (MSGTXT,1000) JERR, 'OPEN', LUN
            END IF
         GO TO 990
         END IF
C                                       Finally, open the tape drive.
      IERR = 0
      LNAME = MACHIN
      CALL CHUTOL (48, LNAME)
      XLATED = 'aipsmt0:' // LNAME
      LNAME = 'NET0:TPDEVS.LIST'
      I = JTRIM (XLATED)
      CALL CHR2H (256, XLATED, 1, HLATED)
      CALL ZVTPO2 (FTAB(FIND+MOFF), I, HLATED, IERR)
      IF (IERR.NE.0) THEN
          WRITE (MSGTXT,1005) IERR
          GO TO 990
          END IF
C                                       Open tape device
C                                       pack buffer: header
      NBACK = 720
      CALL ZCLC8 (8, 'ZTPOPN  ', 1, HBUF)
      CALL ZILI32 (1, 15, 3, IBUF)
      CALL ZILI32 (1, NBACK, 4, IBUF)
      CALL ZILI32 (1, 0, 5, IBUF)
C                                       pack buffer: data
      CALL ZILI32 (1, LUN, 6, IBUF)
      CALL ZILI32 (1, 1, 7, IBUF)
      CALL ZCLC8 (4, 'READ', 29, HBUF)
      CALL ZCLC8 (48, LNAME, 33, HBUF)
      CALL ZVTPX2 (FTAB(FIND+MOFF), 15, NBACK, IBUF, IERR)
C                                       interpret return
      IF (IERR.EQ.4) THEN
         WRITE (MSGTXT,1010)
         CALL MSGWRT (7)
      ELSE IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1011) IERR
         CALL MSGWRT (7)
      ELSE
         CALL ZI32IL (1, 5, IBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1012) IERR
            CALL MSGWRT (7)
         ELSE IF (NBACK.NE.720) THEN
            WRITE (MSGTXT,1015) NBACK, 720
            CALL MSGWRT (6)
C                                       OKAY! return answers
         ELSE
            NT = 0
            IP = 21
            DO 20 I = 1,36
               CALL ZC8CL (80, IP, HBUF, TAPES(NT+1))
               IP = IP + 80
               J = JTRIM (TAPES(NT+1))
               IF (J.GT.0) NT = NT + 1
 20            CONTINUE
            END IF
         END IF
C                                       Close
      CALL ZVTPC2 (FTAB(FIND+MOFF), I)
      IF (I.EQ.0) GO TO 995
      WRITE (MSGTXT,1020) I
C
 990  CALL MSGWRT (6)
C
 995  CALL LSERCH ('CLOS', LUN, FIND, T, JERR)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZRMTAP: LSERCH ERROR ',I1,' FOR ',A4,' ON LUN = ',I2)
 1001 FORMAT ('ZRMTAP: LUN = ',I2,' ALREADY OPEN IN FTAB')
 1002 FORMAT ('ZRMTAP: NO ROOM IN FTAB FOR LUN = ',I2)
 1005 FORMAT ('ZRMTAP: ZVTPO2 RETURNS ERROR CODE',I7)
 1010 FORMAT ('ZRMTAP: AIPS TPHOSTS PERMISSION DENIED ON REMOTE HOST')
 1011 FORMAT ('ZRMTAP: ZVTPX2 RETURNS ERROR CODE',I7)
 1012 FORMAT ('ZRMTAP: REMOTE TAPE SYSTEM RETURNS ERROR CODE',I7)
 1015 FORMAT ('ZRMTAP: WARNING - GOT BACK',I3,' WORDS, EXPECTED',I3)
 1020 FORMAT ('ZRMTAP: ERROR',I5,' CLOSING THE SOCKET')
      END
