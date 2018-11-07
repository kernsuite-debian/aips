      SUBROUTINE ZTPWAT (LUN, FIND, IBUFF, LBYTES, IERR)
C-----------------------------------------------------------------------
C! wait for asynchronous IO to finish on tape or pseudo-tape disk
C# Z Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   Wait until an asynchronous tape or sequential pseudo-tape disk file
C   I/O operation completes.
C   Inputs:
C      LUN      I   Logical unit number
C      FIND     I   Index in FTAB to file control block for LUN
C      IBUFF    I   Buffer # to wait for (1 or 2)
C   Output:
C      LBYTES   I   Number 8-bit bytes read/written (+1 if tape tape
C                   record longer than requested)
C      IERR     I   Error return code: 0 => no error
C                      1 => LUN not open in FTAB
C                      3 => I/O error
C                      4 => end of file
C                      7 => wait service error
C   Generic version - calls ZTPWA2, ZTPWAD, ZERROR.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IBUFF, LBYTES, IERR
C
      INTEGER   FCBOFF, IEREOF
      LOGICAL   TAPE, T
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA T /.TRUE./
      DATA IEREOF /4/
C-----------------------------------------------------------------------
C                                       LUN open in FTAB?
      IF (FTAB(FIND).EQ.LUN) GO TO 10
         IERR = 1
         WRITE (MSGTXT,1000) LUN
         GO TO 990
C                                       Check buffer number.
 10   IF ((IBUFF.EQ.1) .OR. (IBUFF.EQ.2)) GO TO 20
         IERR = 1
         WRITE (MSGTXT,1010) LUN, IBUFF
         GO TO 990
C                                       Calculate offset in FTAB to
C                                       file control block for IBUFF.
 20   FCBOFF = FIND + MOFF + (IBUFF - 1) * MFCB
C                                       Tape
      TAPE = (LUN.LT.129) .AND. (LUN.GE.129-NTAPED)
      IF (TAPE) THEN
C                                       Remote tape
         IF (TPNAME(129-LUN).NE.'LOCAL') THEN
            CALL ZTPWAR (FTAB(FCBOFF), IERR)
            IF ((IERR.NE.0) .AND. (IERR.NE.IEREOF)) THEN
               WRITE (MSGTXT,1020) IERR, LUN, IBUFF, 'REMOTE'
               CALL MSGWRT (7)
               MSGTXT = 'The following may or may NOT apply:'
               CALL MSGWRT (7)
               CALL ZERROR ('ZTPWAR', FTAB(FCBOFF+FCBERR), ' ',
     *            FTAB(FIND+MOFF), T)
               END IF
C                                       Local tape
         ELSE
            CALL ZTPWA2 (FTAB(FCBOFF), IERR)
            IF ((IERR.NE.0) .AND. (IERR.NE.IEREOF)) THEN
               WRITE (MSGTXT,1020) IERR, LUN, IBUFF, 'TAPE'
               CALL MSGWRT (7)
               CALL ZERROR ('ZTPWA2', FTAB(FCBOFF+FCBERR), ' ',
     *            FTAB(FIND+MOFF), T)
               END IF
            END IF
C                                       Pseudo-tape
      ELSE
C                                       Remote tape
         IF (TPDNAM.NE.'LOCAL') THEN
            CALL ZTPWAR (FTAB(FCBOFF), IERR)
            IF ((IERR.NE.0) .AND. (IERR.NE.IEREOF)) THEN
               WRITE (MSGTXT,1020) IERR, LUN, IBUFF, 'REMOTE'
               CALL MSGWRT (7)
               MSGTXT = 'The following may or may NOT apply:'
               CALL MSGWRT (7)
               CALL ZERROR ('ZTPWAR', FTAB(FCBOFF+FCBERR), ' ',
     *            FTAB(FIND+MOFF), T)
               END IF
C                                       Local tape
         ELSE
            CALL ZTPWAD (FTAB(FCBOFF), IERR)
            IF ((IERR.NE.0) .AND. (IERR.NE.IEREOF)) THEN
               WRITE (MSGTXT,1020) IERR, LUN, IBUFF, 'DISK'
               CALL MSGWRT (7)
               CALL ZERROR ('ZTPWAD', FTAB(FCBOFF+FCBERR), ' ',
     *            FTAB(FIND+MOFF), T)
               END IF
            END IF
         END IF
      LBYTES = FTAB(FCBOFF+FCBXFR)
      IF (IERR.EQ.3) THEN
         MSGTXT = 'This usually means PARITY ERROR'
      ELSE IF (IERR.EQ.5) THEN
         MSGTXT = 'This usually means unexpected BEGINNING-OF-TAPE'
      ELSE IF (IERR.EQ.6) THEN
         MSGTXT = 'This usually means you''re at END-OF-TAPE'
      ELSE
         GO TO 999
         END IF
C
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTPWAT: LUN = ',I3,' NOT OPEN IN FTAB')
 1010 FORMAT ('ZTPWAT: LUN = ',I3,' BUFFER NUMBER',I7,' ILLEGAL')
 1020 FORMAT ('ZTPWAT: ERROR ',I3,' FOR LUN = ',I3,' IBUFF = ',I1,
     *   ' TO ',A)
      END
