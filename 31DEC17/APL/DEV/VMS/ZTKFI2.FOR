      SUBROUTINE ZTKFI2 (OPER, FCB, BUFF, NBYTES, IERR)
C-----------------------------------------------------------------------
C! read/write from/to a Tektronix device
C# Z2 Graphics
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
C   Read/write NBYTES of data from/to a Tektronix terminal.
C   Inputs:
C      OPER     C*4    Operation code "READ" or "WRIT"
C      NBYTES   I      Number of 8-bit bytes to be transferred
C   In/out:
C      FCB      I(*)   File control block for opened Tektronix
C      BUFF     I(*)   I/O buffer
C   Output:
C      IERR     I      Error return code: 0 => no error
C                         2 => bad opcode
C                         3 => I/O error
C                         4 => end of file
C   VMS version - is ZFI2 with option IO$M_NOFORMAT and without virtual
C   block numbers.
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   FCB(*), NBYTES, BUFF(*), IERR
C
      INTEGER   SYS$QIOW, JERR, LREC, ITEMP, FCODE, ZOR
      INTEGER*2 I2TEMP(2)
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE '($IODEF)'
      INCLUDE '($SSDEF)'
      EQUIVALENCE (I2TEMP, ITEMP)
C-----------------------------------------------------------------------
C                                       Read
      FCB(1+FCBREQ) = NBYTES
      IF (OPER.EQ.'READ') THEN
         FCODE = IO$_READVBLK
      ELSE IF (OPER.EQ.'WRIT') THEN
         FCODE = IO$_WRITEVBLK
      ELSE
         IERR = 2
         GO TO 999
         END IF
      FCODE = ZOR (FCODE, IO$M_NOFORMAT)
C                                       Do it
      JERR = SYS$QIOW (%VAL(FCB(1+FCBSP1)), %VAL(FCB(1+FCBFD)),
     *   %VAL(FCODE), FCB(1+FCBSP2), , , BUFF, %VAL(FCB(1+FCBREQ)),
     *   , , , )
C                                       error testing
      ITEMP = FCB(1+FCBSP2)
      IF (JERR.NE.1) THEN
         IERR = 3
         FCB(1+FCBERR) = JERR
         FCB(1+FCBXFR) = 0
      ELSE
         FCB(1+FCBXFR) = I2TEMP(2)
         IF (I2TEMP(1).NE.1) THEN
            FCB(1+FCBERR) = I2TEMP(1)
            IERR = 3
            IF (I2TEMP(1).EQ.SS$_ENDOFFILE) IERR = 4
         ELSE
            IERR = 0
            FCB(1+FCBERR) = 0
            END IF
         END IF
C
 999  RETURN
      END
