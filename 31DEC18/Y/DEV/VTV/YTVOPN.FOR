      SUBROUTINE YTVOPN (BUF, IERR)
C-----------------------------------------------------------------------
C! open the TV device and the TV disk control/parameter file.
C# Y1 TV-basic
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
C   YTVOPN opens the TV including TV lock/parameter file and reads the
C   parameters, placing them in commons.
C   Outputs:
C      BUF   I(256)   Scratch buffer
C      IERR  I        Error return from YTVOP2
C                        = 10 TV unavailable to this version
C   Virtual TV version - from the wire.
C-----------------------------------------------------------------------
      INTEGER   BUF(256), IERR
C
      INTEGER   IER, I
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 10
      TVIND2 = 0
      TVLUN = 9
      TVMAP = .FALSE.
      TVLUN2 = 8
C                                       Open actual TV device
      CALL YTVOP2 (TVLUN, TVIND, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IERR
         GO TO 990
C                                       get actual parms
 40   BUFSR = 256
      BUFSW = 0
      CALL MKYBUF ('READ', 'YTVOPN', BUFSW, BUFSR, NGRAY, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
      IF (IERR.EQ.0) THEN
         CALL UNYBUF (BUFSR, NGRAY, HEADER)
         TVIND2 = TVIND
         GO TO 999
      ELSE
         CALL ZVTVCL (TVLUN, TVIND, IER)
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       error
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('YTVOPN: TV OPEN ERROR',I7)
 1040 FORMAT ('YTVOPN: ERROR',I5,' GETTING TV PARAMETERS OVER THE WIRE')
      END
