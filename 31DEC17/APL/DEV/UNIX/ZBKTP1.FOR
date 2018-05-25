      SUBROUTINE ZBKTP1 (ZLUN, ZBKNAM, BAKTXT, NTAPE, ZMT0N, LZMT0N,
     *   IERR)
C-----------------------------------------------------------------------
C! initialize BACKUP to tape operation for BAKTP
C# Z Tape
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
C   Initialize the host environment in preparation for ZBKTP2.  If
C   OP = 'INIT', initializes the tape too.
C   (used only by task BAKTP)
C   Inputs:
C      NTAPE    I       Tape drive number
C      BAKTXT   C*(*)   lowest file name for file listing (not used
C                       here, only in VMS now)
C   Outputs:
C      ZLUN     I       LUN to use
C      ZBKNAM   C*(*)   Command file name
C      ZMT0N    C*(*)   Tape name (translation of MT0n) n=NTAPE-1
C      LZMT0N   I       Actual length used in ZMT0N
C      IERR     I       Error code: 0 => okay
C   (Unix version)
C-----------------------------------------------------------------------
      INTEGER   ZLUN, NTAPE, LZMT0N, IERR
      CHARACTER ZBKNAM*(*), BAKTXT*(*), ZMT0N*(*)
C
      INTEGER   JERR, NP, ITEMP, XLEN, CLEN, ITRIM, II
      CHARACTER COMMND*40, GMT0N*10, CUSER*3, CTEMP*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBTP.INC'
C-----------------------------------------------------------------------
C                                       ZLUN is host-dependent:
      ZLUN = 79
C                                       translate logical MT0n:
      ITEMP = NTAPE
      CALL ZEHEX (ITEMP, 1, CTEMP)
      GMT0N = 'AMT0' // CTEMP
      XLEN = LEN (ZMT0N)
      CALL ZTRLOG (5, GMT0N, XLEN, ZMT0N, LZMT0N, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1000) GMT0N, JERR
         IERR = 2
         GO TO 990
         END IF
C                                       create private command file:
      CALL ZEHEX (NPOPS, 1, CTEMP)
      CALL ZEHEX (USER, 3, CUSER)
      ZBKNAM = '/tmp/BAKTP' // CTEMP // '.' // CUSER
      IERR = 0
      OPEN (ZLUN, FILE=ZBKNAM, STATUS='UNKNOWN', ERR=900)
C                                       dummy contents initially:
      REWIND ZLUN
      WRITE (ZLUN,1001,ERR=910) 'set -x'
      CLOSE (ZLUN)
C                                       make file executable:
      COMMND = 'chmod +x ' // ZBKNAM
      CLEN = ITRIM (COMMND)
      CALL ZSHCMD (CLEN, COMMND, 8, 'TASK_OUT', IERR)
C                                       Tape initialize:
C                                       to end-of-information for BACK:
      II = 1
      IF (OP.NE.'INIT') THEN
         CALL ZTAPE ('AEOI', TLUN, TIND, II, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1005) IERR
            GO TO 990
            END IF
         NP = ABS (II) - 1
         IF (II.GT.0) WRITE (MSGTXT,1010) NP
         IF (II.LT.0) WRITE (MSGTXT,1011) NP
         IF (II.NE.0) CALL MSGWRT (3)
C                                       to BOT for INIT:
      ELSE
         CALL ZTAPE ('REWI', TLUN, TIND, II, IERR)
         CALL ZTAPE ('WEOF', TLUN, TIND, II, IERR)
         CALL ZTAPE ('REWI', TLUN, TIND, II, IERR)
         MSGTXT = 'Tape initialized for Unix-tar BAKTP use.'
         END IF
      CALL MSGWRT (4)
C                                       Must close tape under Unix.
C                                       Perhaps this means we are
C                                       undefended against simultaneous
C                                       access while BAKTP is running?
      CALL ZTPCLS (TLUN, TIND, IERR)
      TIND = 0
      GO TO 999
C                                       can't create, fatal error:
 900  MSGTXT = 'ZBKTP1: CAN''T OPEN ' // ZBKNAM
      IERR = 1
      GO TO 990
C                                       can't write, fatal error:
 910  MSGTXT = 'ZBKTP1: CAN''T WRITE ' // ZBKNAM
      IERR = 1
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZBKTP1: CANNOT TRANSLATE ''',A,''' IERR',I5)
 1001 FORMAT (A)
 1005 FORMAT ('ERROR WHILE TRYING TO SKIP TO EOI',I6)
 1010 FORMAT ('Advanced to end-of-information after file',I5)
 1011 FORMAT ('Advanced to end-of-information after skipping',I5,'
     *   files')
      END
