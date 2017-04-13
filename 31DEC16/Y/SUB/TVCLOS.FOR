      SUBROUTINE TVCLOS (BUF, IERR)
C-----------------------------------------------------------------------
C! does error checks on device open, then closes the TV via YTVCLS
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2011
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
C   TVCLOS closes the TV device and the TV status disk file, updating
C   the information on the disk.  Does this all by call to YTVCLS.
C   Outputs:
C      BUF   I(256)     Scratch buffer
C      IERR  I          Error code : 0 => ok,   2 => not open in parms,
C                       else as returned by YTVCLS
C-----------------------------------------------------------------------
      INTEGER   IERR, BUF(256)
C
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Check inputs
      IERR = 2
      IF ((TVIND.LE.0) .OR. (TVIND2.LE.0)) GO TO 900
      IF ((TVLUN.NE.9) .OR. (TVLUN2.NE.8)) GO TO 900
C                                       Close TV device
      CALL YTVCLS (BUF, IERR)
      IF (TVBROK.EQ.0) TVBROK = -1
      GO TO 999
C                                       error
 900  WRITE (MSGTXT,1900) TVLUN, TVLUN2, TVIND, TVIND2
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('TVCLOS: BAD PARAMETERS: LUNS, INDS =',4I6)
      END
