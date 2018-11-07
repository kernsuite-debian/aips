      SUBROUTINE ZMI2 (OPER, FCB, BUFF, NBLOCK, NBYTES, IERR)
C-----------------------------------------------------------------------
C! read/write large blocks of data from/to disk, quick return
C# Z2 IO-basic
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
C   Read/write "nbytes" of data from/to disk starting at virtual block
C   "nblock" using "map" I/O (i.e., double buffered, asynchronous I/O).
C   Block sizes are determined by the value assigned to NBPS as
C   AIPS-bytes in ZDCHIN.
C   Inputs:
C      OPER     C*4    Operation code "READ" or "WRIT"
C      FCB      I(*)   File control block for open map disk file
C                      and buffer involved
C      NBLOCK   I      Virtual starting block (1-relative)
C      NBYTES   I      Number of AIPS-bytes (1/2 of a local integer)
C                      to be transferred
C   In/out:
C      BUFF     I(*)   I/O buffer
C   Output:
C      IERR     I      Error return code: 0 => no error
C                         2 => bad opcode
C                         3 => I/O error (also see ZWAI2)
C                         4 => end of file (see ZWAI2)
C   VMS version.
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   FCB(*), NBLOCK, NBYTES, BUFF(*), IERR
C
      INTEGER   SYS$QIO, SYS$SETEF, JERR, LREC, FCODE, ERRCNT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE '($IODEF)'
      INCLUDE '($SSDEF)'
      SAVE ERRCNT
      DATA ERRCNT/0/
C-----------------------------------------------------------------------
C                                       Go to 512 8-bit-byte records
      LREC = ((2 * NBPS) / 512) * (NBLOCK - 1) + 1
C                                       Read
      FCB(1+FCBREQ) = 2 * NBYTES
      IF (OPER.EQ.'READ') THEN
         FCODE = IO$_READVBLK
      ELSE IF (OPER.EQ.'WRIT') THEN
         FCODE = IO$_WRITEVBLK
      ELSE
         IERR = 2
         GO TO 999
         END IF
C                                       Do it
      IF (NBYTES.GT.0) THEN
C                                       Event flags range 32 to 63
         IF (FCB(1+FCBSP1).LT.32 .OR. FCB(1+FCBSP1) .GT. 63) THEN
C                                       print only a few error messages
            IF (ERRCNT .LT. 10) THEN
               WRITE(*,1000) OPER, NBLOCK, NBYTES, FCB(1+FCBSP1)
               CALL MSGWRT(5)
               ERRCNT = ERRCNT + 1
               END IF
C                                       Just use first event flag
            FCB(1+FCBSP1) = 32
C                                       end if event flag OK
            END IF
C                                       do I/O (pass values = %VAL)
         JERR = SYS$QIO (%VAL(FCB(1+FCBSP1)), %VAL(FCB(1+FCBFD)),
     *      %VAL(FCODE), FCB(1+FCBSP2), , , BUFF, %VAL(FCB(1+FCBREQ)),
     *      %VAL(LREC), , , )
      ELSE
C                                       else just set event flag
         FCB(1+FCBSP2) = 1
         JERR = SYS$SETEF (%VAL(FCB(1+FCBSP1)))
         IF ((JERR.EQ.SS$_WASCLR) .OR. (JERR.EQ.SS$_WASSET)) JERR = 1
         END IF
C                                       error testing
      IF (JERR.NE.1) THEN
         IERR = 3
         FCB(1+FCBERR) = JERR
         FCB(1+FCBSP2) = JERR
         FCB(1+FCBXFR) = 0
C                                       print only a few error messages
         IF (ERRCNT .LT. 11) THEN
            WRITE(MSGTXT,1000) OPER, NBLOCK, NBYTES, FCB(1+FCBSP1)
            CALL MSGWRT(5)
            ERRCNT = ERRCNT + 1
            END IF
C                                       use first event flag
         FCB(1+FCBSP1) = 32
C                                       try again
         JERR = SYS$QIO (%VAL(FCB(1+FCBSP1)), %VAL(FCB(1+FCBFD)),
     *      %VAL(FCODE), FCB(1+FCBSP2), , , BUFF, %VAL(FCB(1+FCBREQ)),
     *      %VAL(LREC), , , )
         END IF
C                                       if last I/O was OK
      IF (JERR.EQ.1) THEN
         IERR = 0
         FCB(1+FCBERR) = 0
         END IF
C
 999  RETURN
C----------------------------------------------------------------------------
1000  FORMAT(' ZMI2: ',A,' ERROR at NBLK',I6,', NBYT',I6,', EVENT',I6)
      END
