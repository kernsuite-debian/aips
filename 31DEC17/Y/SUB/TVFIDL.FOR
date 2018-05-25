      SUBROUTINE TVFIDL (LCHAN, NLEVS, INBUF, IERR)
C-----------------------------------------------------------------------
C! standard, simple interactive B&W LUT and color enhancements, zooming
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2008, 2015
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
C   TVFIDL does an interactive run with button A selecting alternately
C   TVTRANSF and TVPSEUDO (color contour type 2 only), button B
C   incrementing the zoom and C decrementing the zoom.
C   Inputs:
C      LCHAN    I      Selected gray-scale channels: bit mask
C      NLEVS    I      Number of gray levels (usually LUTOUT+1)
C   Output:
C      INBUF    I      Scratch buffer >3072
C      IERR     I      Error code: 0 -> ok;  else set by ZM70XF
C-----------------------------------------------------------------------
      INTEGER   LCHAN, NLEVS, INBUF(*), IERR
C
      INCLUDE 'INCS:PTVC.INC'
      INTEGER   JERR, TYPE, I, IZX, IZY, ITW(3), ITRTYP, IBUT, QUAD,
     *   ICONT, NCONT, ITYCON, IMAG, J, MAG, IT, II, JJ
      LOGICAL   T, F, DOIT, SWITCH, DOZOOM
      REAL      X, RPOS(2), PPOS(2), RZPOS(2), PZPOS(2), PGAMMA,
     *    RBPOS(2), RSZX, RSZY, OFMBUF(TVMOFM), OFFS
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IF (NLEVS.LE.10) NLEVS = LUTOUT + 1
      IF (NLEVS.GT.OFMINP) NLEVS = OFMINP + 1
C                                       initial zoom parameters
      IZX = (WINDTV(1) + WINDTV(3)) / 2 + 1
      IZY = (WINDTV(2) + WINDTV(4)) / 2
      IMAG = TVZOOM(1)
      IMAG = MAX (0, MIN (IMAG, ABS(MXZOOM)))
      MAG = 1 + IMAG
      IF (MXZOOM.GT.0) MAG = 2 ** IMAG
      RZPOS(1) = ((TVZOOM(2) - 1.) * (MAG - 1.) + IZX) / MAG + 1.
      RZPOS(2) = MAXXTV(2) - ((MAXXTV(2) - TVZOOM(3)) * (MAG - 1.) +
     *   IZY) / MAG
      IF ((RZPOS(1).LE.WINDTV(1)) .OR. (RZPOS(1).GE.WINDTV(3)))
     *   RZPOS(1) = IZX
      IF ((RZPOS(2).LE.WINDTV(2)) .OR. (RZPOS(2).GE.WINDTV(4)))
     *   RZPOS(2) = IZY
      PZPOS(1) = RZPOS(1)
      PZPOS(2) = RZPOS(2)
C                                       Interactive section
      WRITE (MSGTXT,1200)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1201)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1202)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1203)
      CALL MSGWRT (1)
      RSZX = WINDTV(3) - WINDTV(1) + 1
      RSZY = WINDTV(4) - WINDTV(2) + 1
C                                       initial color cursor
      RPOS(1) = 7.0 + WINDTV(1)
      RPOS(2) = WINDTV(4) - 7.0
      PPOS(1) = 0.
      PPOS(2) = 0.
      RBPOS(1) = 0.
      RBPOS(2) = 0.
      CALL ZTIME (ITW)
C                                       Wait for initial button
      ITRTYP = 1
      SWITCH = F
      DOZOOM = IMAG.EQ.0
      QUAD = -1
      CALL YCURSE ('ONNN', T, F, RZPOS, QUAD, IBUT, IERR)
      IF (IERR.EQ.2) THEN
         RZPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
         RZPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
         CALL YCURSE ('ONNN', T, F, RZPOS, QUAD, IBUT, IERR)
         END IF
 210  IF (IERR.NE.0) GO TO 900
      IF (IBUT.GT.7) GO TO 900
      IF (IBUT.GT.0) THEN
         CALL YWINDO ('READ', WINDTV, IERR)
         IF (IERR.NE.0) GO TO 900
         RSZX = WINDTV(3) - WINDTV(1) + 1
         RSZY = WINDTV(4) - WINDTV(2) + 1
         END IF
C                                       Transfer functions
      IF (IBUT.LE.1) THEN
         IF (SWITCH) ITRTYP = MOD (ITRTYP, 2) + 1
C                                       Black and white: clear colors
         IF (ITRTYP.NE.2) THEN
            IT = OFMINP + 1
            CALL RFILL (IT, 0.0, OFMBUF)
            OFFS = 1.0 / REAL (OFMOUT)
            X = OFFS * (OFMOUT + 1.0) / REAL(NLEVS)
            DO 215 I = 1,NLEVS
               OFMBUF(I) = I * X - OFFS
 215           CONTINUE
            JJ = NLEVS
            IT = IT / JJ
            DO 216 II = 2,IT
               CALL RCOPY (NLEVS, OFMBUF, OFMBUF(JJ+1))
               JJ = JJ + NLEVS
 216           CONTINUE
            CALL YOFM ('WRIT', 7, F, OFMBUF, IERR)
            PPOS(1) = 0.0
            PPOS(2) = 0.0
            WRITE (MSGTXT,1215)
            CALL MSGWRT (1)
            WRITE (MSGTXT,1216)
            CALL MSGWRT (1)
            TYPE = 3
            CALL IENHNS (LCHAN, 7, TYPE, RBPOS, INBUF, IERR)
            IBUT = TYPE
            SWITCH = T
            DOZOOM = IMAG.EQ.0
C                                       Color: clear LUT
         ELSE
            J = MAXINT + 1
            X = (LUTOUT+1.0) / (MAXINT+1.0)
            DO 220 I = 1,J
               INBUF(I) = I * X - 0.5
 220           CONTINUE
            CALL YLUT ('WRIT', LCHAN, 7, F, INBUF, IERR)
            IF (IERR.NE.0) GO TO 900
            WRITE (MSGTXT,1220)
            CALL MSGWRT (1)
            WRITE (MSGTXT,1221)
            CALL MSGWRT (1)
C                                        init vals, on cursor:
C                                        PPOS 0 forces action 1st time
            CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
            IF (IERR.EQ.2) THEN
               RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
               RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
               CALL YCURSE ('ONNN', T, F, RPOS, QUAD, IBUT, IERR)
               END IF
            IF (IERR.NE.0) GO TO 900
            SWITCH = T
            DOZOOM = IMAG.EQ.0
            PPOS(2) = RPOS(2) - 3.
            PPOS(1) = RPOS(1) - 1.
C                                        read loop
 235        CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
            IF ((IERR.NE.0) .OR. (IBUT.GT.7)) GO TO 900
            IF (IBUT.EQ.0) THEN
               CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
               IF (DOIT) THEN
C                                        cursor moved: do it
                  ITYCON = 1
                  PGAMMA = 1.0
                  ICONT = ((RPOS(1) - WINDTV(1) + 1.) / RSZX) * NLEVS
                  ICONT = MAX (0, ICONT)
                  NCONT = NLEVS * (RPOS(2) - WINDTV(2) + 1.0) / RSZY
                  CALL COLORC (ITYCON, NLEVS, ICONT, NCONT, PGAMMA,
     *               OFMBUF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  END IF
               GO TO 235
               END IF
            END IF
      ELSE
         DOIT = (DOZOOM) .AND. ((IBUT.EQ.2) .OR. (IBUT.EQ.4))
         IF ((IBUT.EQ.2) .AND. (DOZOOM)) IMAG = IMAG + 1
         IF ((IBUT.EQ.4) .AND. (DOZOOM)) IMAG = IMAG - 1
         IMAG = MAX (0, MIN (IMAG, ABS(MXZOOM)))
         QUAD = -1
         CALL YCURSE ('ONNN', F, T, RZPOS, QUAD, IBUT, IERR)
         IF (IERR.EQ.2) THEN
            RZPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
            RZPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
            CALL YCURSE ('ONNN', T, F, RZPOS, QUAD, IBUT, IERR)
            END IF
         IF (IERR.NE.0) GO TO 900
         IF (DOIT) GO TO 255
C                                       read cursor until moves
 250     CALL YCURSE ('READ', F, T, RZPOS, QUAD, IBUT, IERR)
         IF ((IBUT.GT.7) .OR. (IERR.NE.0)) GO TO 900
         IF (IBUT.EQ.1) GO TO 210
         CALL DLINTR (RZPOS, IBUT, PZPOS, ITW, DOIT)
         DOIT = DOIT .AND. ((IMAG.GT.0) .OR. (IBUT.GT.0))
C                                       new zoom center & factor
 255     IF (DOIT) THEN
            IF ((IBUT.EQ.2) .AND. (DOZOOM)) IMAG = IMAG + 1
            IF ((IBUT.EQ.4) .AND. (DOZOOM)) IMAG = IMAG - 1
            IMAG = MAX (0, MIN (IMAG, ABS(MXZOOM)))
            IF (IBUT.GT.0) THEN
               CALL YWINDO ('READ', WINDTV, IERR)
               IF (IERR.NE.0) GO TO 900
               RSZX = WINDTV(3) - WINDTV(1) + 1
               RSZY = WINDTV(4) - WINDTV(2) + 1
               END IF
            IZX = (WINDTV(1) + WINDTV(3)) / 2
            IZY = (WINDTV(2) + WINDTV(4)) / 2
            IF (IMAG.EQ.0) THEN
               MAG = 1
               IZX = IZX + 1
            ELSE
               MAG = 1 + IMAG
               IF (MXZOOM.GT.0) MAG = 2 ** IMAG
               IZX = (MAG * (RZPOS(1)-1.0) - IZX) / (MAG-1) + 1.0
               IZX = MAX (1, MIN (MAXXTV(1), IZX))
               IZY = MAXXTV(2) - (MAG * (MAXXTV(2)-RZPOS(2)) - IZY)
     *            / (MAG-1)
               IZY = MAX (1, MIN (MAXXTV(2), IZY))
               END IF
C                                        reset zoom and cursor so that
C                                        cursor is centered and on the
C                                        zoom center
            CALL YZOOMC (IMAG, IZX, IZY, F, IERR)
            IF (IERR.NE.0) GO TO 900
            SWITCH = F
            DOZOOM = T
            QUAD = -1
            CALL YCURSE ('ONNN', F, T, RZPOS, QUAD, IBUT, IERR)
            IF (IERR.EQ.2) THEN
               RZPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
               RZPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
               CALL YCURSE ('ONNN', T, F, RZPOS, QUAD, IBUT, IERR)
               END IF
            IF ((IBUT.GT.7) .OR. (IERR.NE.0)) GO TO 900
            END IF
         GO TO 250
         END IF
      GO TO 210
C-----------------------------------------------------------------------
C                                        close down
C                                        cursor off, TV closed
 900  CALL YCURSE ('OFFF', F, F, RZPOS, QUAD, IBUT, JERR)
C                                        messages on error
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1900) IERR
         CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('Hit button A to enhance B/W or color alternately')
 1201 FORMAT ('Hit button B to increment zoom & set zoom center')
 1202 FORMAT ('Hit button C to decrement zoom & set zoom center')
 1203 FORMAT ('Hit button D to exit')
 1215 FORMAT ('Cursor X controls intercept')
 1216 FORMAT ('Cursor Y controls slope')
 1220 FORMAT ('Cursor X controls position of first contour')
 1221 FORMAT ('Cursor Y controls width of color contours')
 1900 FORMAT ('ERROR WITH TV: CODE #',I7)
      END
