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
C   Version for IIS Models 70 and 75.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   GRFX, IERR
      REAL      BLUE, GREEN, RED
C
      INTEGER   CODE, IR, IG, IB, SCRTCH(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 0
      IF ((GRFX.LT.0) .OR. (GRFX.GT.NGRAPH)) THEN
         WRITE (MSGTXT,1000) GRFX
         IERR = 2
         GO TO 990
         END IF
C                                       Change colors
      IF (OP.EQ.'WRIT') THEN
         IR = MAX (0.0, MIN (1.0, RED)) * 31.0 + 0.5
         IG = MAX (0.0, MIN (1.0, GREEN)) * 31.0 + 0.5
         IB = MAX (0.0, MIN (1.0, BLUE)) * 31.0 + 0.5
         CODE = (IR * 32 + IG) * 32 + IB
         IF (GRFX.EQ.0) THEN
            IF (CODE.LE.0) THEN
               MSGTXT = 'YGRAFX: CURSOR COLOR = 0 NOT ALLOWED'
               IERR = 2
               GO TO 990
               END IF
            YBUFF(8) = CODE
            CALL YGRAPH ('ONNN', 8, SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR
               GO TO 990
               END IF
         ELSE
            YBUFF(GRFX) = CODE
            IB = 2 ** (GRFX + NGRAY - 1)
            IF (MOD(TVLIMG(1)/IB, 2).EQ.1) THEN 
               CALL YGRAPH ('ONNN', GRFX, SCRTCH, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1015) IERR, GRFX
                  GO TO 990
                  END IF
               END IF
            END IF

C                                       RGRFX opcode.
      ELSE IF (OP.EQ.'READ') THEN
         IF (YBUFF(8).EQ.0) CALL YGRAPH ('INIT', GRFX, SCRTCH, IERR)
         IF (GRFX.EQ.0) THEN
            CODE = YBUFF(8)
         ELSE
            CODE = YBUFF(GRFX)
            END IF
         IB = MOD (CODE, 32)
         CODE = CODE / 32
         IG = MOD (CODE, 32)
         CODE = CODE / 32
         IR = MOD (CODE, 32)
         RED   = IR / 31.0
         GREEN = IG / 31.0
         BLUE  = IB / 31.0
C                                       Unrecognized OP.
      ELSE
         MSGTXT = 'YGRAFX: Unrecognized OP code - ' // OP
         IERR = 2
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('YGRAFX: INVALID GRAPHICS PLANE NUMBER',I4)
 1010 FORMAT ('YGRAFX: ERROR',I3,' CHANGING CURSOR COLOR')
 1015 FORMAT ('YGRAFX: ERROR',I3,' CHANGING COLOR GRAPHICS CHANNEL',I3)
      END
