      SUBROUTINE TVOPEN (BUF, IERR)
C-----------------------------------------------------------------------
C! sets LUNs, calls YTVOPN to open the TV device, does error messages
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
C   Open the TV, passing pointers through common /TVDEV/.  Almost all
C   except error checking done by YTVOPN these days.
C   Outputs:
C      BUF   I(256)     Scratch buffer
C      IERR  I          Error return from YTVOPN
C                       = 10 TV unavailable to this version
C-----------------------------------------------------------------------
      INTEGER   BUF(256), IERR
C
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 10
      TVIND2 = 0
      TVLUN = 9
      TVLUN2 = 8
C                                       Do open
      IF (NTVDEV.GT.0) THEN
         TVBROK = MAX (0, TVBROK)
         CALL YTVOPN (BUF, IERR)
         IF (IERR.EQ.0) THEN
            GO TO 999
         ELSE
            IF (TVBROK.EQ.0) TVBROK = -1
            WRITE (MSGTXT,1030) IERR
            END IF
C                                       TV unavailable messages
      ELSE IF (NPOPS.LE.NINTRN) THEN
         MSGTXT = 'YOU HAVE NOT BEEN ASSIGNED A TV DEVICE'
      ELSE
         MSGTXT = 'TVS ARE NOT AVAILABLE TO BATCH AIPS'
         END IF
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('TV OPEN ERROR',I7)
      END
