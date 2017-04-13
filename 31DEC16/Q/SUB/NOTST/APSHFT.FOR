      SUBROUTINE APSHFT ( MO2, USTOP, UZERO, ROWSIZ, LOGRID, APSIZE)
C-----------------------------------------------------------------------
C! AP routine: Shift Gridded UV rows in AP
C# AP-util UV
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
C AP SHiFT: Shift a group of 2*MO2 rows from LOGRID to near top of AP
C   Inputs:
C      MO2     I    Half number of rows to shift
C      USTOP   I    Stop  U row to write
C      UZERO   I    Row number of minimum row in AP to be processed
C                   (Often Location of row U=0 in AP)
C      ROWSIZ  I    Length of a Row in AP, number of reals
C      LOGRID  I    Index of start of rows in AP (0 relative)
C      APSIZE  I    Size of Ap (number of REALs)
C   Outputs:
C      Only AP memory is affected
C-----------------------------------------------------------------------
      INTEGER   MO2, USTOP, UZERO, ROWSIZ, LOGRID, APSIZE
C
      INTEGER IU, U, EROW, END1
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       wait for AP to FINISH
      CALL QWAIT
C                                       must move from top to bottom
      DO 100 IU = 1, 2*MO2
C                                       convert to decreasing U
         U = USTOP - IU + 1
C                                       determine start of M-1 rows
         END1 = LOGRID + (((2*MO2)-IU)*ROWSIZ)
         EROW = LOGRID + ((U-UZERO)*ROWSIZ)
C                                       if start after end of AP
         IF (EROW.GT.APSIZE-ROWSIZ+1) THEN
            WRITE(MSGTXT,1080,ERR=999) EROW, APSIZE-ROWSIZ+1
            CALL MSGWRT(7)
            EROW = APSIZE-ROWSIZ-1
            END IF
C                                       shift rows
            CALL QVMOV (END1, 1, EROW, 1, ROWSIZ)
            CALL QWAIT
 100     CONTINUE
C                                       clear rest for gridding
      CALL QVCLR (LOGRID, 1, EROW-LOGRID)
      CALL QWAIT
C
 999  RETURN
C-----------------------------------------------------------------------
 1080 FORMAT ('APSHFT: SHIFT ERROR, START=',I9,' >APSIZE=',I9)
      END
