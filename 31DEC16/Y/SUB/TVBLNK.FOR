      SUBROUTINE TVBLNK (ENTRY, ICS, BUFFER, IERR)
C-----------------------------------------------------------------------
C! blinks two TV channels, cursor controls rate; also enhancements
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-1999
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
C   TVBLNK alternates 2 channels at a rate set by the cursor position.
C   It returns whenever a button is pushed in autoblink (ENTRY=1)
C   mode.  In manual control mode (ENTRY=2), button A selects
C   alternate planes, button B selects autoblink (temporarily) and
C   the other buttons cause a return.
C   Inputs:
C      ENTRY   I        Type of blinking (1 -> auto, 2 -> man)
C      ICS     I(2)     Channel numbers: 1 - NGRAY
C   Output:
C      IBUT    I        Button(s) pushed: (1 - 15)
C      BUFFER  I(256)   Work buffer
C      IERR    I        Error code of ZM70XF: 0 => ok,  2 => input error
C-----------------------------------------------------------------------
      INTEGER   ENTRY, ICS(2), BUFFER(*), IERR
C
      INTEGER   IQ, IER, I, ION, ICBS(4,2), ICOLOR, ISM, ITM, IXS, IYS,
     *   J, ITYPE, ICHA(4), IBUT
      REAL      RPOS(2), S, X, D1, D2, PPOS(2,3), RSZX, RSZY
      LOGICAL   F, T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                       check inputs
      IERR = 2
      IF ((ICS(1).LT.1) .OR. (ICS(1).GT.NGRAY)) GO TO 999
      IF ((ICS(2).LT.1) .OR. (ICS(2).GT.NGRAY)) GO TO 999
C                                       set parameters
      ICOLOR = 7
      ISM = 2
      IF (TYPSPL.EQ.2) ISM = 1
      IF ((TYPSPL.LE.0) .OR. (TVIMPC.LE.0)) ISM = 3
      ITM = 2
      ICBS(1,1) = 2 ** (ICS(1)-1)
      ICBS(1,2) = 2 ** (ICS(2)-1)
      CALL FILL (3, ICBS(1,1), ICBS(2,1))
      CALL FILL (3, ICBS(1,2), ICBS(2,2))
      ION = 2
      RSZX = WINDTV(3) - WINDTV(1) + 1
      RSZY = WINDTV(4) - WINDTV(2) + 1
      CALL RFILL (6, 0.0, PPOS)
      CALL YHOLD ('ONNN', IERR)
C                                       All channels off
      DO 10 I = 1,NGRAY
         CALL YSLECT ('OFFF', I, 0, BUFFER, IERR)
 10      CONTINUE
C                                       General instructions
 20   IF (ENTRY.EQ.1) THEN
         WRITE (MSGTXT,1020) ICS(1)
         CALL MSGWRT (1)
         WRITE (MSGTXT,1021) ICS(2)
      ELSE
         MSGTXT = 'Hit button A to switch manually between images'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button B to do automatic blink'
         END IF
      CALL MSGWRT (1)
      IF (ISM.GE.3) THEN
         MSGTXT = 'Hit button C to set next transfer function'
      ELSE
         MSGTXT = 'Hit button C to set both transfer functions together'
         END IF
      CALL MSGWRT (1)
C                                       Cursor on
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
      PPOS(1,1) = RPOS(1)
      PPOS(2,1) = RPOS(2)
      CALL YCURSE ('ONNN', F, F, RPOS, IQ, IBUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Blinking
 100  MSGTXT = 'Hit button D to exit'
         CALL MSGWRT (1)
C                                       Instructions
         MSGTXT = 'Cursor X position controls blink rate'
         CALL MSGWRT (1)
         MSGTXT = 'Cursor Y position controls proportion of time on'
     *      // ' each plane'
         CALL MSGWRT (1)
C                                       Blink loop: plane on, delay,
C                                       read cursor, plane off
 120     DO 130 I = 1,2
            CALL YSPLIT ('WRIT', TVSPLT(1), TVSPLT(2), ICBS(1,I),
     *         ICBS(1,I), ICBS(1,I), T, IERR)
            IF (IERR.NE.0) GO TO 900
            CALL YHOLD ('OFFF', IERR)
            X = (RPOS(1) - WINDTV(1) + 1.) / RSZX
            S = 5.8 * X * X  -  0.72 * X  -  0.08
            X = 0.5 * (RPOS(2) - WINDTV(2) + 1.) / RSZY  + 0.25
            IF (I.EQ.2) X = 1.0 - X
            S = S * X
            IF (S.GT.0.0) THEN
               CALL ZDELAY (S, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
            CALL YCURSE ('READ', F, F, RPOS, IQ, IBUT, IERR)
            IF (IERR.NE.0) GO TO 900
            IF (IBUT.NE.0) THEN
               CALL YHOLD ('ONNN', IERR)
               CALL YSLECT ('OFFF', ICS(I), 0, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 900
               GO TO 150
               END IF
            D1 = ABS (RPOS(1)-PPOS(1,1)) / (0.75*MAXXTV(1))
            D2 = ABS (RPOS(2)-PPOS(2,1)) / (0.75*MAXXTV(2))
            IF ((D1.GE.1.0) .OR. (D2.GE.1.0)) THEN
               IF (D1.GE.1.0) RPOS(1) = PPOS(1,1)
               IF (D2.GE.1.0) RPOS(2) = PPOS(2,1)
               CALL YCURSE ('ONNN', F, F, RPOS, IQ, IBUT, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            PPOS(1,1) = RPOS(1)
            PPOS(2,1) = RPOS(2)
 130        CONTINUE
         GO TO 120
C                                       Manual mode?
 150  IF ((ENTRY.EQ.2) .AND. (IBUT.LT.4)) THEN
         CALL YWINDO ('READ', WINDTV, IERR)
         IF (IERR.NE.0) GO TO 900
         RSZX = WINDTV(3) - WINDTV(1) + 1
         RSZY = WINDTV(4) - WINDTV(2) + 1
C                                       Switch planes
         IF (IBUT.GE.2) THEN
            RPOS(1) = PPOS(1,1)
            RPOS(2) = PPOS(2,1)
            GO TO 120
         ELSE
            ION = 3 - ION
            CALL YSLECT ('ONNN', ICS(ION), 0, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 900
            CALL YHOLD ('OFFF', IERR)
            CALL YCURSE ('READ', T, F, RPOS, IQ, IBUT, IERR)
            IF (IERR.NE.0) GO TO 900
            CALL YHOLD ('ONNN', IERR)
            CALL YSLECT ('OFFF', ICS(ION), 0, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 900
            GO TO 150
            END IF
         END IF
C                                       Done blink
      IF ((ENTRY.EQ.2) .AND. (IBUT.LT.8)) IBUT = 4
C                                       Image enhancement
 200  IF (IBUT.LT.8) THEN
         IF (IBUT.EQ.1) THEN
            ITM = 1
         ELSE IF (IBUT.EQ.2) THEN
            ITM = 2
         ELSE IF (IBUT.GT.2) THEN
            IF (ISM.GE.3) THEN
               ITM = 3 - ITM
            ELSE
               ITM = 3
               END IF
            END IF
C                                       Turn on plane(s): single
         IF (ISM.GE.3) THEN
            CALL YHOLD ('ONNN', IERR)
            CALL YSLECT ('OFFF', ICS(3-ITM), 0, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 900
            CALL YSLECT ('ONNN', ICS(ITM), 0, BUFFER, IERR)
C                                       Vertical split
         ELSE IF (TYPSPL.NE.2) THEN
            IXS = MAXXTV(1)/2
            IYS = 1
            ICHA(1) = 2 ** (ICS(ISM)-1)
            ICHA(2) = 2 ** (ICS(3-ISM)-1)
            ICHA(3) = ICHA(2)
            ICHA(4) = ICHA(1)
            CALL YSPLIT ('WRIT', IXS, IYS, ICHA, ICHA, ICHA, T, IERR)
C                                       Horizontal split
         ELSE
            IYS = MAXXTV(2)/2
            IXS = 1
            ICHA(1) = 2 ** (ICS(ISM)-1)
            ICHA(3) = 2 ** (ICS(3-ISM)-1)
            ICHA(2) = ICHA(1)
            ICHA(4) = ICHA(3)
            CALL YSPLIT ('WRIT', IXS, IYS, ICHA, ICHA, ICHA, T, IERR)
            END IF
         IF (IERR.NE.0) GO TO 900
         CALL YHOLD ('OFFF', IERR)
C                                       Instructions: enhance channel
         IF (ITM.LE.2) THEN
            WRITE (MSGTXT,1200) ICS(ITM)
         ELSE
            WRITE (MSGTXT,1201) ICS(1), ICS(2)
            END IF
         CALL MSGWRT (1)
         IF (ITM.GE.2) WRITE (MSGTXT,1020) ICS(1)
         IF (ITM.GT.2) CALL MSGWRT (1)
         IF (ITM.NE.2) WRITE (MSGTXT,1021) ICS(2)
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to reverse sign of slope'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to resume blinking'
         CALL MSGWRT (1)
         IF (ITM.LE.2) THEN
            I = 2 ** (ICS(ITM) - 1)
            J = ITM + 1
         ELSE
            I = 2 ** (ICS(1) - 1) + 2 ** (ICS(2) - 1)
            J = 2
            END IF
         ITYPE = 2
         CALL IENHNS (I, ICOLOR, ITYPE, PPOS(1,J), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
         IBUT = ITYPE
         IF (IBUT.LE.2) GO TO 200
         GO TO 20
         END IF
C                                       Final state: ch ICS(1) on
 900  CALL YHOLD ('ONNN', IER)
      CALL YSLECT ('OFFF', ICS(2), 0, BUFFER, IER)
      CALL YSLECT ('ONNN', ICS(1), 0, BUFFER, IER)
      CALL YHOLD ('OFFF', IER)
C                                       Off cursor
      CALL YCURSE ('OFFF', F, F, RPOS, IQ, I, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Hit button A to set transfer function of channel',I2)
 1021 FORMAT ('Hit button B to set transfer function of channel',I2)
 1200 FORMAT ('Setting transfer function for channel',I2)
 1201 FORMAT ('Setting transfer functions for channels',I2,' and',I2)
      END
