      SUBROUTINE ZTPMI2 (OP, FCB, BUFF, NBYTES, IERR)
C-----------------------------------------------------------------------
C! tape read/write
C# Z2 Tape
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
C   Low level SEQUENTIAL access, large record, double buffered tape I/O.
C   Inputs:
C      OP       C*4    Operation - 'READ', 'WRIT'
C      FCB      I(*)   I/O control block
C      NBYTES   I      Number of bytes to transfer.
C   In/out:
C      BUFF     I(*)   The i/o buffer.
C   Outputs:
C      IERR     I      Error return code: 0 = Success.
C                         1 = File not open.
C                         2 = Operation incorrectly specified.
C                         3 = I/O error.
C                         4 = end of file (no messages)
C   VMS version
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   FCB(*), NBYTES, BUFF(*), IERR
C
      INTEGER   SYS$QIO, SYS$SETEF, JERR, LREC, FCODE
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE '($IODEF)'
      INCLUDE '($SSDEF)'
C-----------------------------------------------------------------------
C                                       Read
      LREC = 1
      FCB(1+FCBREQ) = NBYTES
      IF (OP.EQ.'READ') THEN
         FCODE = IO$_READVBLK
      ELSE IF (OP.EQ.'WRIT') THEN
         FCODE = IO$_WRITEVBLK
      ELSE
         IERR = 2
         GO TO 999
         END IF
C                                       Do it
      IF (NBYTES.GT.0) THEN
         JERR = SYS$QIO (%VAL(FCB(1+FCBSP1)), %VAL(FCB(1+FCBFD)),
     *      %VAL(FCODE), FCB(1+FCBSP2), , , BUFF, %VAL(FCB(1+FCBREQ)),
     *      %VAL(LREC), , , )
C                                       just set event flag
      ELSE
         FCB(1+FCBERR) = 1
         JERR = SYS$SETEF (%VAL(FCB(1+FCBSP1)))
C                                       other initialisations
         FCB(1+FCBSP2) = SS$_NORMAL
         JERR = 1
         END IF
C                                       error testing
      IF ((JERR.NE.SS$_DATAOVERUN) .AND. (JERR.NE.1)) THEN
         IERR = 3
         IF (JERR.EQ.SS$_ENDOFFILE) IERR = 4
         IF (JERR.EQ.SS$_ENDOFTAPE) IERR = 6
         IF (JERR.EQ.SS$_ENDOFVOLUME) IERR = 4
         FCB(1+FCBERR) = JERR
         FCB(1+FCBSP2) = JERR
         FCB(1+FCBXFR) = 0
      ELSE
         IERR = 0
         FCB(1+FCBERR) = 0
         END IF
C
 999  RETURN
      END
