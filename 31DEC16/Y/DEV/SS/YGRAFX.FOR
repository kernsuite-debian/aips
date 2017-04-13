      SUBROUTINE YGRAFX (OP, GRFX, RED, GREEN, BLUE, IERR)
C-----------------------------------------------------------------------
C! read/write TV graphics colors
C# Y1 TV-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   Version for Screen Servers.
C   Author: Mark Calabretta, Australia Telescope.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   GRFX, IERR
      REAL      BLUE, GREEN, RED
C
      INTEGER   DAT(4), NWSD, NWVD, OPCODE, BUFFER(3)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       Check inputs.
      IF ((GRFX.LT.0) .OR. (GRFX.GT.NGRAPH)) THEN
         WRITE (MSGTXT,1000) GRFX
         IERR = 2
C                                       Send instructions to the SS
      ELSE IF (OP.EQ.'WRIT') THEN
         OPCODE = 51
         DAT(1) = GRFX
         DAT(2) = RED * 255.0 + 0.5
         DAT(3) = GREEN * 255.0 + 0.5
         DAT(4) = BLUE * 255.0 + 0.5
         NWVD = 0
         CALL ZSSSXF (OPCODE, DAT, NWVD, BUFFER, NWSD, IERR)
         IF (IERR.NE.0) WRITE (MSGTXT,1020) IERR
C                                       RGRFX opcode.
      ELSE IF (OP.EQ.'READ') THEN
         OPCODE = 52
         DAT(1) = GRFX
         NWVD = 0
         CALL ZSSSXF (OPCODE, DAT, NWVD, BUFFER, NWSD, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR
         ELSE IF (NWSD.NE.3) THEN
            WRITE (MSGTXT,1031) NWSD
            IERR = 1
         ELSE
            CALL ZI8IL (3, 1, BUFFER, BUFFER)
            RED   = BUFFER(1) / 255.0
            GREEN = BUFFER(2) / 255.0
            BLUE  = BUFFER(3) / 255.0
            END IF
C                                       Unrecognized OP.
      ELSE
         MSGTXT = 'YGRAFX: Unrecognized OP code - ' // OP
         IERR = 2
         END IF
C
 990  IF (IERR.NE.0) CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('YGRAFX: INVALID GRAPHICS PLANE NUMBER',I4)
 1020 FORMAT ('YGRAFX: UNSUCCESSFUL WRITE TO SSS, ERROR',I3,'.')
 1030 FORMAT ('YGRAFX: ERROR',I4,' RETURNED ON READ WITH ZSSSXF')
 1031 FORMAT ('YGRAFX: UNSUCCESSFUL READ,',I4,' WORDS RECEIVED.')
      END
