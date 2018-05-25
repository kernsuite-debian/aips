      SUBROUTINE ZFI2 (OPER, FCB, BUFF, NREC, MSGBUF, IERR)
C-----------------------------------------------------------------------
C! read/write one 256-integer record from/to a non-map disk file
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
C   Transfer one 256 local integer record between an I/O buffer and
C   disk starting at random access record "nrec" using "non-map" I/O
C   (i.e., single buffered, synchronous I/O).
C   Inputs:
C      OPER   C*4      Operation code "READ" or "WRIT"
C      FCB    I(*)     File control block for opened non-map disk file
C      NREC   I        Disk record number (1-relative, units of
C                      256-integer records)
C   In/out:
C      BUFF   I(256)   I/O buffer
C   Output:
C      MSGBUF C*80     extra error message (null terminated)
C      IERR   I        Error return code: 0 => no error
C                         2 => bad opcode
C                         3 => I/O error
C                         4 => end of file
C   Must NOT use MSGWRT.
C   VMS version
C--------------------------------------------------------------------
      CHARACTER OPER*4, MSGBUF*(*)
      INTEGER   FCB(*), NREC, BUFF(256), IERR
C
      INTEGER   SYS$QIOW, JERR, LREC, ITEMP, FCODE
      INTEGER*2 I2TEMP(2)
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE '($IODEF)'
      INCLUDE '($SSDEF)'
      EQUIVALENCE (I2TEMP, ITEMP)
C-----------------------------------------------------------------------
      MSGBUF = ' '
C                                       Go to 512 8-bit-byte records
      LREC = 2 * NREC - 1
C                                       Read
      FCB(1+FCBREQ) = 1024
      IF (OPER.EQ.'READ') THEN
         FCODE = IO$_READVBLK
      ELSE IF (OPER.EQ.'WRIT') THEN
         FCODE = IO$_WRITEVBLK
      ELSE
         IERR = 2
         GO TO 999
         END IF
C                                       Do it
      JERR = SYS$QIOW (%VAL(FCB(1+FCBSP1)), %VAL(FCB(1+FCBFD)),
     *   %VAL(FCODE), FCB(1+FCBSP2), , , BUFF, %VAL(FCB(1+FCBREQ)),
     *   %VAL(LREC), , , )
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
