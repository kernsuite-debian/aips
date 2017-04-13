      SUBROUTINE TVSCRL (IC, IERR)
C-----------------------------------------------------------------------
C! interactive TV scroll
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-1999, 2015
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
C   TVSCRL does the simple TV interactive scrolling operation
C   Inputs:
C      IC     I   Channel bit mask 1-NGREY, any larger => graphics too
C   Output
C      IERR   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IC, IERR
C
      INTEGER   IX, IY, IBUT, QUAD, ITW(3)
      REAL      RPOS(2), PPOS(2)
      LOGICAL   T, F, DOIT
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      MSGTXT = 'Cursor position controls image position'
      CALL MSGWRT (1)
      MSGTXT = 'Hit any button to exit'
      CALL MSGWRT (1)
      IX = 0
      IY = 0
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL ZTIME (ITW)
C                                        force scroll
 10   CALL YSCROL (IC, IX, IY, T, IERR)
      IF (IERR.NE.0) GO TO 900
         PPOS(1) = RPOS(1)
         PPOS(2) = RPOS(2)
C                                        read until cursor moves
 20      CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF (.NOT.DOIT) GO TO 20
            IX = RPOS(1) - (WINDTV(1) + WINDTV(3)) / 2
            IY = RPOS(2) - (WINDTV(2) + WINDTV(4)) / 2
            IF (IBUT.EQ.0) GO TO 10
C                                        close down
C                                        cursor off, TV closed
 900  CALL YCURSE ('OFFF', F, F, RPOS, QUAD, IBUT, IX)
C
 999  RETURN
      END
