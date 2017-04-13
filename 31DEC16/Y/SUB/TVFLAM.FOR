      SUBROUTINE TVFLAM (NLEVS, BUFFER, IERR)
C-----------------------------------------------------------------------
C! interactive TV color enhancement of "flame" type
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2015
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
C   Flame like color enhancement (calls IMFCLR)
C   Inputs:
C      NLEVS    I      color levels (use 0)
C   Output:
C      BUFFER   I(*)   scratch * >=
C      IERR     I      error code
C-----------------------------------------------------------------------
      INTEGER   NLEVS, BUFFER(*), IERR
C
      INTEGER   IBUT, QUAD, ITW(3), ICOL, IMLEVS, ISZX, ISZY
      REAL      RPOS(2), PPOS(2), PGAMMA, X
      LOGICAL   DOIT, F
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      QUAD = -1
      RPOS(1) = 0.0
      RPOS(2) = 0.0
      IMLEVS = MAX (LUTOUT+1, NLEVS)
      IF (IMLEVS.GT.OFMINP+1) IMLEVS = OFMINP + 1
      CALL ZTIME (ITW)
C                                        TVPHLAME
C                                        Button A: RGB triangles
C                                        Button B: Loops in hue
C                                        Button C: color contours
      MSGTXT = 'Hit Buttons A or B to cycle starting color'
      CALL MSGWRT (1)
      MSGTXT = 'Hit buttons C or D to exit'
      CALL MSGWRT (1)
      MSGTXT = 'Cursor X position controls break between low & high'
     *   // ' colors'
      CALL MSGWRT (1)
      MSGTXT = 'Cursor Y position controls color intensity (Gamma)'
      CALL MSGWRT (1)
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      ICOL = 1
      ISZX = WINDTV(3) - WINDTV(1) + 1
      ISZY = WINDTV(4) - WINDTV(2) + 1
C                                        init vals, on cursor
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
      RPOS(2) = ISZY * 0.6 + WINDTV(2) - 1
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 900
C                                        read until moves
 20   CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (IBUT.GT.3) GO TO 900
      IF (IBUT.GT.0) THEN
         CALL YWINDO ('READ', WINDTV, IERR)
         IF (IERR.NE.0) GO TO 900
         ISZX = WINDTV(3) - WINDTV(1) + 1
         ISZY = WINDTV(4) - WINDTV(2) + 1
         END IF
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
C                                        new color
      IF (DOIT) THEN
         IF (IBUT.GT.0) THEN
            ICOL = ICOL + 1
            IF (ICOL.EQ.0) ICOL = 1
            IF (ICOL.EQ.4) ICOL = -3
            END IF
         X = 2.0 * (RPOS(1)-WINDTV(1)+1.) / REAL (ISZX) - 1.0
         PGAMMA = 2.5 * (RPOS(2)+13-WINDTV(2)) / ISZY
         CALL COLORF (IMLEVS, ICOL, X, PGAMMA, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      GO TO 20
C                                        messages on error
 900  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1910) IERR
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1910 FORMAT ('TVFLAM: TV ACTION ERROR CODE',I7)
      END
