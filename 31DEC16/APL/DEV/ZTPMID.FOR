      SUBROUTINE ZTPMID (OPER, FIND, FCB, BUFF, NBYTES, IERR)
C-----------------------------------------------------------------------
C! pseudo-tape disk read/write for 2880-bytes records
C# Z2 Tape FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2001
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
C   Low level sequential access, large record, double buffered
C   pseudo-tape disk I/O.  Actual implementation is often via Fortran
C   IO and hence is not actually done with quick return.
C
C   NOTE: This is for TAPIO type operations only, i.e., LRECL=2880,
C         FTAB(FIND+5) = I   logical record number.
C
C   Inputs:
C      OPER     C*4    Operation code "READ" or "WRIT"
C      FIND     I      Index in FTAB to file control block for LUN
C      FCB      I(*)   File control block for open map disk file and
C                      buffer involved
C      NBYTES   I      Number of 8-bit bytes to transfer: 0, 2880 okay
C   In/out:
C      BUFF     I(*)   I/O buffer
C   Output:
C      IERR     I      Error return code: 0 => no error
C                         FCB has syserr for ZTPWAD to return error
C   Generic version: (assumes that an INTEGER is 4 8-bit bytes)
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   FIND, FCB(*), BUFF(720), NBYTES, IERR
C
      INTEGER   LUN, BLKNO, IOSVAL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IOSVAL = 0
C                                       Record I/O request in the file
C                                       control block.
      FCB(1+FCBREQ) = NBYTES
C                                       Assume I/O transfer count as
C                                       I/O request since there is no
C                                       way to get it from Fortran I/O).
      FCB(1+FCBXFR) = NBYTES
C                                       Zero error status in the file
C                                       control block.
      FCB(1+FCBERR) = 0
C                                       Zero transfer request just
C                                       initializes double buffered I/O.
      IF (NBYTES.LE.0) GO TO 999
C                                       Extract logical record number
C                                       from the "map" file control
C                                       area.
      BLKNO = FTAB(FIND+5)
C                                       Extract the logical unit number
C                                       from FTAB.
      LUN = FTAB(FIND)
C                                       Wrong record length request
      IF (NBYTES.NE.2880) THEN
         IERR = 2
C                                       Read.
      ELSE IF (OPER.EQ.'READ') THEN
         READ (LUN,REC=BLKNO,IOSTAT=IOSVAL) BUFF
         FCB(1+FCBERR) = IOSVAL
C                                       Write.
      ELSE IF (OPER.EQ.'WRIT') THEN
         WRITE (LUN,REC=BLKNO,IOSTAT=IOSVAL) BUFF
         FCB(1+FCBERR) = IOSVAL
C                                       Invalid operation requested.
      ELSE
         IERR = 2
         END IF
C
 999  RETURN
      END
