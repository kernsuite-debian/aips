      SUBROUTINE APLOAD ( USTRT, USTOP, UZERO, ROWSIZ, LOGRID, APSIZE,
     *   FLIST, BUFFER, IRET)
C-----------------------------------------------------------------------
C! AP routine: Load AP with Grid file from disk
C# AP-util UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C AP LOAD: Loads the AP with rows of GRIDDED UV data
C   Inputs:
C      USTRT   I    Start U row to load
C      USTOP   I    Stop  U row to load
C      UZERO   I    Row number of minimum row in AP to be processed
C                   (Often Location of row U=0 in AP)
C      ROWSIZ  I    Length of a Row in AP, number of reals
C      LOGRID  I    Index of start of rows in AP (0 relative)
C      APSIZE  I    Size of AP in REALs
C      FLIST   I(*) File description array
C   Outputs:
C      BUFFER  R(*) Scratch Buffer containing
C      IRET    I    return code, O=OK else APIO return code
C-----------------------------------------------------------------------
      INTEGER   USTRT, USTOP, UZERO, ROWSIZ, LOGRID, APSIZE, FLIST(*),
     *          IRET
      REAL BUFFER(*)
C
      INTEGER IU, U, EROW
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (UZERO.GT.USTRT) THEN
         IRET = UZERO*1000 + USTRT
         WRITE (MSGTXT,1000,ERR=999) USTRT, UZERO
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       wait for AP to FINISH
      CALL QWAIT
C                                       load previous values in AP
      DO 100 IU = USTRT, USTOP
C                                       convert to decreasing U
         U = USTOP - IU + USTRT
C                                       load AP from top down
         EROW = LOGRID + ((U-UZERO)*ROWSIZ)
C                                       keep output below AP top
         EROW = MIN( EROW, APSIZE-ROWSIZ-1)
C                                       Copy row to AP
         CALL APIO ('READ', FLIST, EROW, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1070,ERR=999) IRET
            CALL MSGWRT (8)
            WRITE (MSGTXT,1080,ERR=999) USTRT, U, USTOP
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       wait for AP to FINISH
         CALL QWAIT
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('APLOAD: ERROR! AP START,',I6,', BEFORE ZERO',I6)
 1070 FORMAT ('APLOAD: ERROR=',I5,' READING GRID FILE')
 1080 FORMAT ('APLOAD: USTRT=',I5,', U=',I5,', USTOP=',I5)
      END

