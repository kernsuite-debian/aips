      SUBROUTINE APVCHK (LABEL, INDEX, ROWNUM, ROW, ROWSIZ, APTEMP)
C-----------------------------------------------------------------------
C! AP routine: Find and print maximum value element of a vector.
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
C AP Vector CHecK: Find and print maximum value of a vector and address
C of the maximum.
C   Inputs:
C      LABEL   C*20 Label for maximum output
C      INDEX   I    Index label
C      ROWNUM  I    Row number in cells
C      ROW     I    Location of the Row in the AP
C      ROWSIZ  I    Number of elements in Row
C      APTEMP  I    Work index in AP for max value and location.
C                   AP locations APTEMP and APTEMP+1 will be modified
C-----------------------------------------------------------------------
      CHARACTER*20 LABEL
      INTEGER   INDEX, ROWNUM, ROW, ROWSIZ, APTEMP
C
      INTEGER   VLOC
      REAL      VMAX
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       wait for AP
      CALL QWD
C                                       put maximum in the row at APTEMP
      CALL QMAXV (ROW, 1, APTEMP, ROWSIZ)
      CALL QWR
C                                       read maximum from AP loc APTEMP
      CALL QGET  (VMAX, APTEMP, 1, 2)
      CALL QWD
C                                       read location from AP APTEMP+1
      CALL QGET  (VLOC, APTEMP+1, 1, 1)
      CALL QWD
C                                       calc cell number
      VLOC = ((VLOC - ROW)/2) + 1
      WRITE(MSGTXT,1000,ERR=999) LABEL, INDEX, ROWNUM, VMAX, VLOC
C                                       if none zero, write
      IF (VMAX.NE.0.0) CALL MSGWRT(3)
C
 999  RETURN
 1000 FORMAT ('APVCHK: ',A,' I=',I5,', ROW=',I5,', MAX=',1PE11.3,
     *        ', AT=',I5)
      END
