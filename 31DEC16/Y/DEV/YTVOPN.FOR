      SUBROUTINE YTVOPN (BUF, IERR)
C-----------------------------------------------------------------------
C! open the TV device and the TV disk control/parameter file.
C# Y1 TV-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000
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
C   YTVOPN opens the TV including TV lock/parameter file and reads the
C   parameters, placing them in commons.
C   Outputs:
C      BUF   I(256)   Scratch buffer
C      IERR  I        Error return from YTVOP2
C                        = 10 TV unavailable to this version
C   Generic version - uses YDTVC to get TV device common
C-----------------------------------------------------------------------
      INTEGER   BUF(256), IERR
C
      INTEGER   IER
      LOGICAL   F
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IERR = 10
      IF (NTVDEV.LE.0) GO TO 999
      TVLUN = 9
      TVIND = 0
      TVMAP = F
      TVLUN2 = 8
      TVIND2 = 0
C                                       Is TV available?
      CALL YDTVC ('OPEN', BUF, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.4) THEN
            WRITE (MSGTXT,1010) IERR
         ELSE
            WRITE (MSGTXT,1011) NTVDEV
            END IF
         GO TO 990
         END IF
C                                       Open actual TV device
 20   CALL YTVOP2 (TVLUN, TVIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         TVIND = 0
         GO TO 990
         END IF
C                                       get current status
 40   CALL YTVMC
      CALL YDTVC ('GET ', BUF, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1040) IERR
C                                       error
 990  CALL MSGWRT (7)
      IF (TVIND.GT.0) CALL YTVCL2 (TVLUN, TVIND, IER)
      TVIND = 0
      IF (TVIND2.GT.0) CALL YDTVC ('CLOS', BUF, IER)
      TVIND2 = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('YTVOPN: TV LOCK OPEN ERROR',I7)
 1011 FORMAT ('TV DEVICE',I3,' CURRENTLY BEING USED BY SOMEONE ELSE')
 1030 FORMAT ('YTVOPN: TV OPEN ERROR',I7)
 1040 FORMAT ('YTVOPN: YDTVC RETURNS ERROR',I7)
      END
