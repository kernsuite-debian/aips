      SUBROUTINE ZWAI2 (FCB, IERR)
C-----------------------------------------------------------------------
C! wait for read/write large blocks of data from/to disk
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
C   Waits for the completion of an asynchronous I/O operation and
C   returns its completion status and transfer count in AIPS-bytes
C   (1/2 of a local integer).
C   In/out:
C      FCB    I(*)   File control block for open disk file & buffer
C   Output:
C      IERR   I      Error return code: 0 => no error
C                       3 => I/O error
C                       4 => end of file
C   VMS version
C-----------------------------------------------------------------------
      INTEGER   FCB(*), IERR
C
      INTEGER   JERR, SYS$WAITFR, ITEMP(2)
      INTEGER*2 I2TEMP(4)
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE '($SSDEF)'
      EQUIVALENCE (ITEMP, I2TEMP)
C-----------------------------------------------------------------------
      IERR = 0
C                                       Wait for IO completion
      JERR = SYS$WAITFR (%VAL(FCB(1+FCBSP1)))
C                                       Check error in service
      IF (JERR.NE.1) THEN
         IERR = 7
         FCB(1+FCBERR) = JERR
         FCB(1+FCBXFR) = 0
C                                       check IO errors
      ELSE
         ITEMP(1) = FCB(1+FCBSP2)
         ITEMP(2) = FCB(2+FCBSP2)
         JERR = I2TEMP(1)
C                                       long word byte count
         I2TEMP(4) = I2TEMP(3)
         I2TEMP(3) = I2TEMP(2)
         FCB(1+FCBXFR) = ITEMP(2)
C                                       no error
         IF ((JERR.EQ.1) .OR. (JERR.EQ.0)) THEN
            IERR = 0
            FCB(1+FCBERR) = 0
C                                       general IO error
         ELSE
            IERR = 3
            FCB(1+FCBERR) = JERR
            IF (JERR.EQ.SS$_ENDOFFILE) IERR = 4
            END IF
         END IF
C
 999  RETURN
      END
