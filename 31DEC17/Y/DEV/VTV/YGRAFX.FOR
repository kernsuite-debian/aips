      SUBROUTINE YGRAFX (OP, GRFX, RED, GREEN, BLUE, IERR)
C-----------------------------------------------------------------------
C! read/write TV graphics colors
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
C   YGRAFX modifies the colour of the graphics or cursor.
C   Inputs:
C      OP       C*4   READ or WRITE graphics colour assignment.
C      GRFX     I     Cursor (0), or graphics plane number.
C   In/Out:
C      RED      R     Red,   0.0 - 1.0
C      GREEN    R     Green, 0.0 - 1.0
C      BLUE     R     Blue,  0.0 - 1.0
C   Output:
C      IERR     I     Error status, 0 means success.
C   Version for Virtual TV devices.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   GRFX, IERR
      REAL      BLUE, GREEN, RED
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 0
      IF ((GRFX.LT.0) .OR. (GRFX.GT.NGRAPH)) THEN
         WRITE (MSGTXT,1000) GRFX
         IERR = 2
         GO TO 990
         END IF
C                                       Unrecognized OP.
      IF ((OP.NE.'READ') .AND. (OP.NE.'WRIT')) THEN
         MSGTXT = 'YGRAFX: Unrecognized OP code - ' // OP
         IERR = 2
         GO TO 990
         END IF
C                                       Change colors
      BUFFER(1) = GRFX
      IF (OP.EQ.'WRIT') THEN
         BUFFER(2) = MAX (0.0, MIN (1.0, RED)) * 10000.0 + 0.5
         BUFFER(3) = MAX (0.0, MIN (1.0, GREEN)) * 10000.0 + 0.5
         BUFFER(4) = MAX (0.0, MIN (1.0, BLUE)) * 10000.0 + 0.5
         BUFSW = 4
         BUFSR = 0
C                                       RGRFX opcode.
      ELSE
         BUFSW = 1
         BUFSR = 4
         END IF
C                                       Do IO
      CALL MKYBUF (OP, 'YGRAFX', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
      IF ((IERR.EQ.0) .AND. (OP.EQ.'READ')) THEN
         CALL UNYBUF (BUFSR, BUFFER, HEADER)
         RED = BUFFER(2) / 10000.0
         GREEN = BUFFER(3) / 10000.0
         BLUE = BUFFER(4) / 10000.0
         END IF
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('YGRAFX: INVALID GRAPHICS PLANE NUMBER',I4)
      END
