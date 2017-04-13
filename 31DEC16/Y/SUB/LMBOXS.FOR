      SUBROUTINE LMBOXS (TVAREA, CIRCLE, IL, IB, IT, RPOS, IBUT, IERR)
C-----------------------------------------------------------------------
C! keeps cursor where it belongs for GRBOXS
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2009
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
C   LMBOXS restrains the TV cursor for GRBOXS
C   Inputs:
C      TVAREA   R(4)   Box must fit in this TV pixel area
C      CIRCLE   L      This is a circle
C      IL       I      Progress in setting: 1 first blc, 2, trc, 3 2nd
C                      blc, 0 => okay box
C                      < 0 => turn off, else turn on
C      IB       I(2)   Bottom left corner or (1,2) = X,Y radius
C      IT       I(2)   Top right corner or center
C   In/out:
C      RPOS     R(2)   Cursor position
C   Output:
C      IBUT     I      Button read if any
C      IERR     I      Error code
C-----------------------------------------------------------------------
      INTEGER   IL, IB(2), IT(2), IBUT, IERR
      LOGICAL   CIRCLE
      REAL      TVAREA(4), RPOS(2)
C
      INTEGER   QUAD
      REAL      PPOS(2), R, R1
C-----------------------------------------------------------------------
      CALL RCOPY (2, RPOS, PPOS)
      IBUT = 0
      IERR = 0
      QUAD = -1
      RPOS(1) = MAX (TVAREA(1), MIN (TVAREA(3), RPOS(1)))
      RPOS(2) = MAX (TVAREA(2), MIN (TVAREA(4), RPOS(2)))
C                                       Circle: setting center
      IF ((CIRCLE) .AND. (IL.EQ.3)) THEN
         RPOS(1) = MAX (TVAREA(1)+IB(1), MIN (TVAREA(3)-IB(1), RPOS(1)))
         RPOS(2) = MAX (TVAREA(2)+IB(2), MIN (TVAREA(4)-IB(2), RPOS(2)))
C                                       Circle settin radius
      ELSE IF ((CIRCLE) .AND. (IL.EQ.2)) THEN
         R = SQRT ((RPOS(1)-IT(1))**2 + (RPOS(2)-IT(2))**2)
         R1 = MIN (IT(1) - TVAREA(1), IT(2) - TVAREA(2))
         R1 = MIN (R1, TVAREA(3) - IT(1))
         R1 = MIN (R1, TVAREA(4) - IT(2))
         IF (R.GT.R1) THEN
            RPOS(1) = IT(1) + (RPOS(1) - IT(1)) * R1 / R
            RPOS(2) = IT(2) + (RPOS(2) - IT(2)) * R1 / R
            END IF
         END IF
      IF ((ABS(RPOS(1)-PPOS(1)).GE.1.) .OR.
     *   (ABS(RPOS(2)-PPOS(2)).GE.1.)) THEN
         CALL YCURSE ('ONNN', .FALSE., .TRUE., RPOS, QUAD, IBUT, IERR)
         END IF
C
 999  RETURN
      END
