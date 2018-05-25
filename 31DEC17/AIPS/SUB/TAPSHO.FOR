      SUBROUTINE TAPSHO (MACHIN, IERR)
C-----------------------------------------------------------------------
C! Shows all possible tape drives
C# Print
C-----------------------------------------------------------------------
C;  Copyright (C) 1998-1999
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
C   TAPSHO prints the device name and description of all local tape
C   drives, similar to the summary one receives on starting AIPS.
C   Input:
C      MACHIN   C*(*)   Nachine for which tapes are printed
C   Output:
C      IERR     I       Zero if all is well, 1 otherwise
C   PPM 19981202: cloned from PRINTR.
C-----------------------------------------------------------------------
      CHARACTER MACHIN*(*)
      INTEGER   IERR
C
      INTEGER   IER2, TPNLEN, XLNB, IKB, ITP, ITP2, ICTP, LUN, FIND,
     *   TLEN, JTRIM, I, IHDR, J
      LOGICAL   DUM, LOCAL
      DOUBLE PRECISION X
      CHARACTER TAPNAM*8, TPDEV*20, TPVAL*8, MYHOST*12, LINE*80,
     *   AWORD*12, TAPES(36)*80
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
C-----------------------------------------------------------------------
      LOCAL = MACHIN.EQ.' '
C                                       Get number of tape drives
      IF (LOCAL) THEN
         TAPNAM = 'NAIPST'
         TPNLEN = 6
         CALL ZTRLOG (TPNLEN, TAPNAM, TPNLEN, TPVAL, XLNB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) TAPNAM, IERR
            GO TO 990
            END IF
C                                       and make sure we got it
         IF (XLNB.LE.0) THEN
            WRITE (MSGTXT,1005) TAPNAM, XLNB
            GO TO 990
            END IF
C                                       Extract integer from it
         IKB = 1
         ERRNUM = 0
         X = 0.D0
         CALL GETNUM (TPVAL, XLNB, IKB, X)
         IF (ERRNUM.EQ.27) THEN
            WRITE (MSGTXT,1010) TAPNAM, TPVAL
            GO TO 990
            END IF
         ICTP = INT (X)
C                                       Get name of this host (upcase)
         MYHOST = HSTNAM
      ELSE
         MYHOST = MACHIN
         ICTP = 0
         END IF
      CALL CHLTOU (12, MYHOST)
C                                       For all requests, we try to get
C                                       info from TPDEVS.LIST in NET0:
      LUN = 3
      CALL ZTXOPN ('QRED', LUN, FIND, 'NET0:TPDEVS.LIST', DUM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR
         IERR = 1
         GO TO 990
         END IF
C                                       Initialise line counter
      ITP2 = 0
      IHDR = 0
C                                       Read loop
 100  CONTINUE
         CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
C                                       End of file?
         IF (IERR.EQ.2) GO TO 200
C                                       Other problem?
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) 1+ITP2, IERR
            CALL MSGWRT (7)
            GO TO 200
            END IF
C                                       Get rid of nulls
         TLEN = JTRIM (LINE)
C                                       Skip white space and comments
         IF (LINE.EQ.' ') GO TO 100
         IF (LINE(1:1).EQ.'#') GO TO 100
C                                       If this is not the right host,
C                                       skip it.
         CALL WORD (LINE, AWORD)
         CALL CHLTOU (12, AWORD)
         IF (AWORD.NE.MYHOST) GO TO 100
C                                       Otherwise, save details
         ITP2 = ITP2 + 1
         TAPES(ITP2) = LINE
         GO TO 100
C                                       End of loop: close the file
C                                       Reset EOF (expected) to no error
 200  IF (IERR.EQ.2) IERR = 0
      IER2 = 0
      CALL ZTXCLS (LUN, FIND, IER2)
      IF (IER2.NE.0) THEN
         WRITE (MSGTXT,1200) IER2
         CALL MSGWRT (7)
         END IF
C                                       Really remote:
      IF ((ITP2.EQ.0) .AND. (.NOT.LOCAL)) THEN
         CALL ZRMTAP (MACHIN, ITP2, TAPES, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1205) IERR, MACHIN
            GO TO 990
            END IF
         END IF
C                                       Now parse and print
      ICTP = MAX (ICTP, ITP2)
      DO 220 ITP = 1,ICTP
C                                       parse a line
         IF (ITP.LE.ITP2) THEN
            LINE = TAPES(ITP)
C                                       extract host name
            CALL WORD (LINE, AWORD)
            TLEN = JTRIM (LINE)
            J = JTRIM (AWORD)
            I = TLEN - J
            CALL CHTRIM (LINE(J+1:), I, LINE, TLEN)
C                                       get device name
            CALL WORD (LINE, TPDEV)
            J = JTRIM (TPDEV)
            I = TLEN - J
            CALL CHTRIM (LINE(J+1:), I, LINE, TLEN)
C                                       Rest of line is description.
C                                       Assume any leftovers are remote.
         ELSE
            TPDEV = '(remote)'
            LINE = 'REMOTE TAPE'
            END IF
C                                       Check if this is the first...
         IF (IHDR.EQ.0) THEN
            IHDR = 1
            MSGTXT = 'AIPS tapes on ' // MYHOST
            CALL MSGWRT (3)
            MSGTXT = 'Tape Device name            Description'
            CALL MSGWRT (3)
            MSGTXT = '---------------------------------------' //
     *         '-------------------------'
            CALL MSGWRT (3)
            END IF
C                                       Write out the summary
         WRITE (MSGTXT,1210) ITP, TPDEV, LINE(1:36)
         CALL MSGWRT (3)
         IF (LINE(37:72).NE. ' ') THEN
            WRITE (MSGTXT,1211) LINE(37:72)
            CALL MSGWRT (3)
            END IF
 220     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANNOT TRANSLATE LOGICAL ',A,'??  ERROR ',I5)
 1005 FORMAT ('BAD STRING LENGTH FROM LOGICAL ',A,': ',I5)
 1010 FORMAT ('UNEXPECTED NON-NUMERIC VALUE IN ',A,'?: ',A)
 1015 FORMAT ('CANNOT OPEN TPDEVS.LIST FILE IN NET0, ERROR ',I5)
 1100 FORMAT ('ERROR READING TPDEVS.LIST LINE ',I5,' CODE ',I5)
 1200 FORMAT ('ERROR ',I5,' CLOSING TPDEVS.LIST, HOPE THIS IS OK')
 1205 FORMAT ('ERROR',I5,' RETURNED FROM ',A)
 1210 FORMAT (I3,'. ',A20,3X,A)
 1211 FORMAT (28X,A)
      END
