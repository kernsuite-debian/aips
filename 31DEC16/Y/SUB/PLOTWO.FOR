      SUBROUTINE PLOTWO (BLC, TRC, BX, BY, TX, TY, BLACKN, COLOR, PLBUF,
     *   GERR)
C-----------------------------------------------------------------------
C! plot a line segment, limiting the end points to be within BLC,TRC
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002-2004
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
C   Plot two points of a line, limiting the lines to within BLC, TRC
C   Inputs:
C      BLC      R(2)      Plot lower left corner (pixels)
C      TRC      R(2)      Plot upper right corner (pixels)
C      BX       R(2)      X coordinates of two points
C      BY       R(2)      Y coordinates of two points
C      BLACKN   L         Write as a dark vector?
C      COLOR    L         Write as 3-color vector
C   In/Out:
C      TX       R         Last X coordinate
C      TY       R         Last Y coordinate
C      PLBUF    I(256)    Plot IO buffer
C   Output:
C      GERR     I         Plot error code: 0 => okay
C-----------------------------------------------------------------------
      REAL      BLC(2), TRC(2), BX(2), BY(2), TX, TY
      INTEGER   PLBUF(256), GERR
      LOGICAL   BLACKN, COLOR
C
      INTEGER   IER
      REAL      LX(2), LY(2)
C-----------------------------------------------------------------------
      GERR = 0
      LX(1) = BX(1)
      LX(2) = BX(2)
      LY(1) = BY(1)
      LY(2) = BY(2)
C                                       Limit lines to inside border
      CALL LINLIM (BLC, TRC, LX(1), LY(1), IER)
      IF (IER.EQ.0) THEN
C                                       Move to begin line, if needed
         IF ((TX.NE.LX(1)) .OR. (TY.NE.LY(1))) THEN
            CALL GPOS (LX(1), LY(1), PLBUF, GERR)
            IF (GERR.EQ.0) THEN
               TX = LX(1)
               TY = LY(1)
               END IF
            END IF
C                                       Draw line section
         IF (GERR.EQ.0) THEN
            IF (BLACKN) THEN
               CALL GDVEC (LX(2), LY(2), PLBUF, GERR)
            ELSE IF (COLOR) THEN
               CALL G3VEC (LX(2), LY(2), PLBUF, GERR)
            ELSE
               CALL GVEC (LX(2), LY(2), PLBUF, GERR)
               END IF
            IF (GERR.EQ.0) THEN
               TX = LX(2)
               TY = LY(2)
               END IF
            END IF
         END IF
C
 999  RETURN
      END
