      SUBROUTINE TVPSUD (NLEVS, BUFFER, IERR)
C-----------------------------------------------------------------------
C! various interactive pseudo-colorings
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2004, 2008-2009, 2015
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
C   Inputs:
C      NLEVS    I      color levels (use 0)
C   Output:
C      BUFFER   I(*)   scratch * >= TVMOFM
C      IERR     I      error code
C-----------------------------------------------------------------------
      INTEGER   NLEVS, BUFFER(*), IERR
C
      INTEGER  IX, IBUT, NCONT, QUAD, ICONT, ITW(3), ICOL, IMLEVS,
     *   ITYCON, ISZX, ISZY
      REAL      RPOS(2), PPOS(2), PGAMMA, PHUE, PSAT(2), PLIT(2)
      LOGICAL   F, DOIT
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                        general parameters
      QUAD = -1
      RPOS(1) = 0.0
      RPOS(2) = 0.0
      IMLEVS = MAX (LUTOUT+1, NLEVS)
      IF (IMLEVS.GT.OFMINP+1) IMLEVS = OFMINP + 1
      CALL ZTIME (ITW)
C                                        TVPSEUDO
C                                        Button A: RGB triangles
C                                        Button B: Loops in hue
C                                        Button C: color contours
      MSGTXT = 'Hit button A for RGB color triangles'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button B for loops in hue'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button C for color contours'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button D to exit'
      CALL MSGWRT (1)
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      ICOL = 3
      ITYCON = 1
      ISZX = WINDTV(3) - WINDTV(1) + 1
      ISZY = WINDTV(4) - WINDTV(2) + 1
C                                        continuous RGB triangles
 20   MSGTXT = 'Cursor X position controls break between low & high'
     *   // ' colors'
      CALL MSGWRT (1)
      MSGTXT = 'Cursor Y position controls color intensity (Gamma)'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button A to cycle starting color'
      CALL MSGWRT (1)
C                                        init vals, on cursor
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
      RPOS(2) = ISZY * 0.6 + WINDTV(2) - 1
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 900
C                                        read until moves
 30   CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (IBUT.GT.7) GO TO 900
         IF (IBUT.GT.0) THEN
            CALL YWINDO ('READ', WINDTV, IERR)
            IF (IERR.NE.0) GO TO 900
            ISZX = WINDTV(3) - WINDTV(1) + 1
            ISZY = WINDTV(4) - WINDTV(2) + 1
            END IF
         IF (IBUT.EQ.2) GO TO 40
         IF (IBUT.EQ.4) GO TO 60
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
C                                        new color
         IF (DOIT) THEN
            PGAMMA = 2.5 * (RPOS(2)+13-WINDTV(2)) / ISZY
            IX = IMLEVS * (RPOS(1) - WINDTV(1) + 1.) / ISZX + 0.6
            IF (IBUT.GT.0) THEN
               ICOL = ICOL - 1
               IF (ICOL.EQ.0) ICOL = -1
               IF (ICOL.EQ.-4) ICOL = 3
               END IF
            CALL COLORL (IMLEVS, ICOL, IX, PGAMMA, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         GO TO 30
C                                        loops in hue
 40   MSGTXT = 'Cursor X position controls number of loops'
      CALL MSGWRT (1)
      MSGTXT = 'Cursor Y position controls starting hue'
      CALL MSGWRT (1)
C                                        init & fixed values, on curs
      RPOS(1) = ISZX / 10.0 + WINDTV(1) - 1
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2.0
      PGAMMA = 1.0
      PSAT(1) = 100.0
      PSAT(2) = 100.0
      PLIT(1) = 65.0
      PLIT(2) = 65.0
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 900
C                                        read until move
 50   CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (IBUT.GT.7) GO TO 900
         IF (IBUT.GT.0) THEN
            CALL YWINDO ('READ', WINDTV, IERR)
            IF (IERR.NE.0) GO TO 900
            ISZX = WINDTV(3) - WINDTV(1) + 1
            ISZY = WINDTV(4) - WINDTV(2) + 1
            END IF
         IF (IBUT.EQ.1) GO TO 20
         IF (IBUT.EQ.4) GO TO 60
            CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
C                                        new color
            IF (DOIT) THEN
               PHUE = 360.0 * (RPOS(2) - WINDTV(2) + 1) / ISZY
               NCONT = 8.0 * (RPOS(1) - WINDTV(1)) / ISZX + 1.0
               CALL COLORH (IMLEVS, NLEVS, NCONT, PLIT, PSAT, PHUE,
     *            PGAMMA, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
            GO TO 50
C                                        Color contours
 60   MSGTXT = 'Hit button C to cycle 5 different color contours'
      CALL MSGWRT (1)
      MSGTXT = 'Cursor X position controls start level of contours'
      CALL MSGWRT (1)
      MSGTXT = 'Cursor Y position controls width of contours'
      CALL MSGWRT (1)
C                                        init vals, on cursor:
C                                        PPOS 0 forces action 1st time
      RPOS(1) = 7.0 + WINDTV(1)
      RPOS(2) = WINDTV(4) - 7.0
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 900
C                                        read loop
 70   CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (IBUT.GT.7) GO TO 900
         IF (IBUT.GT.0) THEN
            CALL YWINDO ('READ', WINDTV, IERR)
            IF (IERR.NE.0) GO TO 900
            ISZX = WINDTV(3) - WINDTV(1) + 1
            ISZY = WINDTV(4) - WINDTV(2) + 1
            END IF
         IF (IBUT.EQ.1) GO TO 20
         IF (IBUT.EQ.2) GO TO 40
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
C                                        cursor moved: do it
         IF (DOIT) THEN
            IF (IBUT.GT.0) ITYCON = MOD (ITYCON, 5) + 1
            PGAMMA = 1.0
C                                       experiment
C            PGAMMA = 2.5 * (RPOS(2)+13-WINDTV(2)) / ISZY
            ICONT = ((RPOS(1)-WINDTV(1)+1.)/ISZX) * IMLEVS
            ICONT = MAX (0, ICONT)
            NCONT = IMLEVS * (RPOS(2) - WINDTV(2) + 1.) / ISZY
            CALL COLORC (ITYCON, IMLEVS, ICONT, NCONT, PGAMMA, BUFFER,
     *         IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         GO TO 70
C                                        messages on error
 900  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1910) IERR
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1910 FORMAT ('TVPSUD: TV ACTION ERROR CODE',I7)
      END
