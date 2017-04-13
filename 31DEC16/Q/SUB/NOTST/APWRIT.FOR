      SUBROUTINE APWRIT ( USTRT, USTOP, UZERO, ROWSIZ, LOGRID, FLIST,
     *   APSIZE, BUFFER, IRET)
C-----------------------------------------------------------------------
C! AP routine: Writes rows of GRIDDED UV data from AP to disk
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
C   AP WRITe: Writes Rows of Gridded UV data from AP to disk
C   Inputs:
C      USTRT   I    Start U row to write
C      USTOP   I    Stop  U row to write
C      UZERO   I    Row number of minimum row in AP to be processed
C                   (Often Location of row U=0 in AP)
C      ROWSIZ  I    Length of a Row in AP, number of REALs
C      LOGRID  I    Index of start of rows in AP (0 relative)
C      FLIST   I(*) File description array
C      APSIZE  I    Size of AP in REALs
C   Outputs:
C      BUFFER  R(*) Scratch Buffer containing
C      IRET    I    return code, O=OK, 10=Row error,
C                   else APIO return code
C-----------------------------------------------------------------------
      INTEGER   USTRT, USTOP, UZERO, ROWSIZ, LOGRID, FLIST(*), APSIZE,
     *          IRET
      REAL      BUFFER(*)
      INTEGER IU, U, EROW
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       for rows to write from AP
      DO 100 IU = USTRT, USTOP
C                                       convert to decreasing U
         U = USTOP - IU + USTRT
C                                       load AP from top down
         EROW = LOGRID + ((U-UZERO)*ROWSIZ)
C                                       rows above top of AP are zero
         IF (EROW+ROWSIZ .GE. APSIZE) EROW = APSIZE-ROWSIZ-1
C                                       check for AP index errors
         IF (EROW.LT.LOGRID) THEN
            WRITE(MSGTXT,1200,ERR=999) EROW, U
            IRET = 10
            GO TO 995
            END IF
C                                       Write.
         CALL QWAIT
         CALL APIO ('WRIT', FLIST, EROW, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1210,ERR=999) IRET, IU
            GO TO 995
            END IF
 100     CONTINUE
         CALL QWAIT
C                                       jump over error message
      GO TO 999
C                                       jump here on error
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('APWRIT: ERROR DURING U=',I4,' WRITE: AP LOC= ',I8)
 1210 FORMAT ('APWRIT: ERROR',I3,' WRITING GRID ROW ',I5)
      END
