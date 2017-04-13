      SUBROUTINE TVHELX (NLEVS, DOPRNT, IERR)
C-----------------------------------------------------------------------
C! OFM helix in color intensity space - interactive uses CUBHLX
C# POPS=appl TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2010-2011, 2015
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
C   Does a helix in OFM colors, which is monotonically increasing in
C   perceived intensity (since R, G, B are appropriately weighted).
C   Dave Green -- MRAO
C-----------------------------------------------------------------------
      INTEGER   NLEVS, DOPRNT, IERR
C
      INTEGER   IBUT, ISIZEX, ISIZEY, QUAD, ITW(3), I, II, JJ, NLO, NHI
      REAL      START, ROTS, ROTATE, HUE, GAMMA, RPOS(2), PPOS(2),
     *   RED(32752), GRN(32752), BLU(32752)
      LOGICAL   PROT, VRTRTC, F, DOIT
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA F/.FALSE./
C-----------------------------------------------------------------------
      PROT = .FALSE.
      CALL ZTIME (ITW)
C                                       Give instructions...
      MSGTXT = 'Press button A to toggle rotation direction'
      CALL MSGWRT (1)
      MSGTXT = 'Press button B to increment number of rotations'
      CALL MSGWRT (1)
      MSGTXT = 'Press button C to increment hue saturation (by 0.2)'
      CALL MSGWRT (1)
      MSGTXT = 'Press button D to exit'
      CALL MSGWRT (1)
      MSGTXT = 'Cursor X controls starting color'
      CALL MSGWRT(1)
      MSGTXT = 'Cursor Y controls gamma (0.3 to 3.0)'
      CALL MSGWRT (1)
      START = 0.5
      ROTS = 1.5
      HUE = 1.0
      GAMMA = 1.0
C                                       Cursor on...
      QUAD = -1
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
      ISIZEX = WINDTV(3) - WINDTV(1) + 1
      ISIZEY = WINDTV(4) - WINDTV(2) + 1
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 930
C                                       Read cursor position...
 100  CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 930
C
C Buttons: 8 = D exit, otherwise read position, 4=C, 2=B, 1=A
C
      IF (IBUT.LE.7) THEN
         IF (IBUT.GT.0) THEN
            CALL YWINDO ('READ', WINDTV, IERR)
            IF (IERR.NE.0) GO TO 930
            ISIZEX = WINDTV(3) - WINDTV(1) + 1
            ISIZEY = WINDTV(4) - WINDTV(2) + 1
            END IF
C                                       hue > 1 clipped at ends
         IF (IBUT.GE.4) THEN
            HUE = HUE + 0.2
            IF (HUE.GT.2.1) HUE = 0.0
         ELSE IF (IBUT.GE.2) THEN
            ROTS = ROTS + 0.5
            IF (ROTS.GT.4.0) ROTS = 0.0
         ELSE IF (IBUT.EQ.1) THEN
            PROT = .NOT.PROT
            END IF
C
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF (DOIT) THEN
            START = 3.0 * (RPOS(1) - WINDTV(1) + 1.0) / ISIZEX - 1.0
            GAMMA = 10.0 ** ((RPOS(2) - WINDTV(2) + 1.0) / ISIZEY - 0.5)
            IF (PROT) THEN
               ROTATE = ROTS
            ELSE
               ROTATE = -ROTS
               END IF
C                                       compute the helix
            CALL CUBHLX (START, ROTATE, HUE, GAMMA, NLEVS, RED, GRN,
     *         BLU, NLO, NHI)
C                                       Extend to fill and load
            I = OFMINP + 1
            JJ = NLEVS
            I = I / JJ
            DO 110 II = 2,I
               CALL RCOPY (NLEVS, RED, RED(JJ+1))
               CALL RCOPY (NLEVS, GRN, GRN(JJ+1))
               CALL RCOPY (NLEVS, BLU, BLU(JJ+1))
               JJ = JJ + NLEVS
 110           CONTINUE
            VRTRTC = .FALSE.
            CALL YOFM ('WRIT', 4, VRTRTC, RED, IERR)
            IF (IERR.NE.0) GO TO 930
            CALL YOFM ('WRIT', 2, VRTRTC, GRN, IERR)
            IF (IERR.NE.0) GO TO 930
            CALL YOFM ('WRIT', 1, VRTRTC, BLU, IERR)
            IF (IERR.NE.0) GO TO 930
C                                       display
            IF (DOPRNT.GT.0) THEN
               WRITE (MSGTXT,1110) START, ROTATE, HUE, GAMMA
               CALL MSGWRT (1)
               IF (NLO+NHI.GT.0) THEN
                  WRITE (MSGTXT,1111) NLO, NHI, NLEVS
                  CALL MSGWRT (1)
                  END IF
               END IF
            END IF
C                                       Loop back...
         GO TO 100
C                                       done
      ELSE
         IF (DOPRNT.NE.0) THEN
            WRITE (MSGTXT,1110) START, ROTATE, HUE, GAMMA
            CALL MSGWRT (3)
            IF (NLO+NHI.GT.0) THEN
               WRITE (MSGTXT,1111) NLO, NHI, NLEVS
               CALL MSGWRT (3)
               END IF
            END IF
         END IF
      GO TO 999
C                                       TV error
 930  WRITE (MSGTXT,1000) IERR, 'IN TV OPERATION'
      CALL MSGWRT (8)
C                                       Exit...
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVHELX ERROR',I4,' ON ',A)
 1110 FORMAT ('TVHELIX: start=',F4.1,' rotations=',F4.1,' hue=',F4.1,
     *   ' gamma=',F4.1)
 1111 FORMAT ('TVHELIX: clipped (low/high): ',I4,'/',I4,' out of ',I4)
      END
