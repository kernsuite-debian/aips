      SUBROUTINE TVZOME (IERR)
C-----------------------------------------------------------------------
C! interactive TV zooming
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2015
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
C   Interactive TV zooming
C   Outputs:
C      IERR     I      Error code
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   IMAG, MAG, IX, IY, IBUT, QUAD, ITW(3), I, LMAGL
      REAL      RPOS(2), PPOS(2)
      LOGICAL   T, F, DOIT
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                        general parameters
      QUAD = -1
      RPOS(1) = 0.0
      RPOS(2) = 0.0
      CALL ZTIME (ITW)
C                                        TVZOOM
      MSGTXT = 'Cursor position controls center point of zoom'
      CALL MSGWRT (1)
      MSGTXT = 'Hit buttons A or B to increment magnification'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button C to decrement magnification'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button D to exit'
      CALL MSGWRT (1)
C                                        set up for 0 zoom
      IMAG = 0
      IX = (WINDTV(1) + WINDTV(3)) / 2 + 1
      IY = (WINDTV(2) + WINDTV(4)) / 2
      RPOS(1) = IX - 1
      RPOS(2) = IY
      PPOS(1) = RPOS(1)
      PPOS(2) = RPOS(2)
      LMAGL = ABS(MXZOOM) + 1
C                                        reset zoom and cursor so that
C                                        cursor is centered and on the
C                                        zoom center
 20   CALL YZOOMC (IMAG, IX, IY, F, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (IBUT.GT.7) GO TO 900
      I = 2
C                                        read cursor until moves
 30   CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (IBUT.GT.7) GO TO 900
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF ((DOIT) .AND. ((IBUT.NE.0) .OR. (IMAG.NE.0))) THEN
C                                        new zoom center & factor
            IF ((IBUT.GT.0) .AND. (IBUT.LT.4)) IMAG = IMAG + 1
            IF (IBUT.GE.4) IMAG = IMAG - 1
            IF (IMAG.LT.0) IMAG = LMAGL - 1
            IMAG = MOD (IMAG, LMAGL)
            IF (IBUT.GT.0) THEN
               CALL YWINDO ('READ', WINDTV, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
            IX = (WINDTV(1) + WINDTV(3)) / 2
            IY = (WINDTV(2) + WINDTV(4)) / 2
            IF (IMAG.EQ.0) THEN
               MAG = 1
               IX = IX + 1
            ELSE
               MAG = IMAG + 1
               IF (MXZOOM.GT.0) MAG = 2 ** IMAG
               IX = (MAG*(RPOS(1)-1.0) - IX) / (MAG-1) + 1.0
               IX = MAX (1, MIN (MAXXTV(1), IX))
               IY = MAXXTV(2) - (MAG * (MAXXTV(2)-RPOS(2)) - IY)
     *            / (MAG-1)
               IY = MAX (1, MIN (MAXXTV(2), IY))
               END IF
            GO TO 20
         ELSE
            GO TO 30
            END IF
C                                        messages on error
 900  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1910) IERR
         CALL MSGWRT (7)
         END IF
C
C
 999  RETURN
C-----------------------------------------------------------------------
 1910 FORMAT ('TVZOME: TV ACTION ERROR CODE',I7)
      END
