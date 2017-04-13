      SUBROUTINE HDRWIN (BLC, TRC, HDR, ERROR)
C-----------------------------------------------------------------------
C! sets image corners via WINDOW, revises header to that of output
C# IO-WaWa Map-util
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
C   HDRWIN normalizes input window parameters and converts the window
C   portion of the input header to that of the desired output map.
C   In/out:
C      BLC   R(7)      Bottom left corner
C      TRC   R(7)      Top right corner
C      HDR   I(256)    Header: may be /MAPHDR/ CATBLK
C   Output:
C      ERROR I         Error code: 0 => ok
C                                 16 => incorrect window
C   Common: /MAPHDR/ CATBLK  used as scratch
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7)
      INTEGER   HDR(256), ERROR
C
      INTEGER   WINER, IERR
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA WINER /16/
C-----------------------------------------------------------------------
C                                       Normalize window
      CALL COPY (256, HDR, CATBLK)
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IERR)
      ERROR = WINER
      IF (IERR.NE.0) GO TO 999
      ERROR = 0
C                                       Fix header
      CALL SUBHDR (BLC, TRC, 1.0, 1.0)
C                                       Copy back to input header.
      CALL COPY (256, CATBLK, HDR)
C
 999  RETURN
      END
