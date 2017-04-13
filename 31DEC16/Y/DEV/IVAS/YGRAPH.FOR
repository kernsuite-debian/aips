      SUBROUTINE YGRAPH (OP, CHAN, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! turns TV graphics planes on and off
C# TV-basic
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
C   YGRAPH is used to turn graphics overlay planes on & off by
C   altering the graphics color look up table.  The color pattern is:
C      CHAN = 1    insert   yellow            drawing plots
C             2    insert   green+.05 red     axis labels
C             3    insert   blue + 0.6 green  blotch
C                           + red
C             4    insert   black             label backgrounds
C             5-7  add      nothing           null channels
C             8    insert   purple            cursor
C   by default.  It may be altered by YGRAFX.
C   Inputs:
C      OP      C*4       'ONNN' or 'OFFF'
C                        'INIT' sets default colors in common
C      CHAN    I         channel number (1 - 8)
C   Output:
C      SCRTCH  I(256)    scratch buffer
C      IERR    I         error code of Z...XF: 0 => ok
C                                              2 => input error
C   IVAS version: uses and sets YBUFF(13-16).  Colors in YBUFF(18-22).
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHAN, SCRTCH(256), IERR
C
      LOGICAL   VRTRTC
      INTEGER   I, LCOLOR, II, DCOLOR(5), J, IC, ZAND, ZEOR, JJ, IV,
     *   MODE, MASK, JERR
      INTEGER   FIVASGMDEFGRAPHIC, FIVASGMFREEZE, FIVASCSCOLOR
      INCLUDE 'INCS:DTVC.INC'
      DATA VRTRTC /.FALSE./
C     DATA DCOLOR /Z00FF, Z00F1, Z0FAF, Z0000, Z0F0F/
      DATA DCOLOR /  255,   241,  4015,     0,  3855/
C-----------------------------------------------------------------------
C                                       Initialization only
      IF (OP.EQ.'INIT') THEN
         IERR = 0
         CALL COPY (5, DCOLOR, YBUFF(18))
         GO TO 999
         END IF
C                                        check input
      IERR = 2
      IF ((CHAN.LT.1) .OR. (CHAN.GT.8)) GO TO 999
      IF ((CHAN.GE.5) .AND. (CHAN.LE.7)) GO TO 999
      IF ((OP.NE.'ONNN') .AND. (OP.NE.'OFFF')) GO TO 999
      IF (YBUFF(22).EQ.0) CALL COPY (5, DCOLOR, YBUFF(18))
C                                       restrict off
 20   CALL YVRTR (VRTRTC, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (CHAN.EQ.8) GO TO 50
C                                       leave display fixed
      MASK = 1
      JERR = FIVASGMFREEZE (MASK)
      IF (JERR.NE.0) GO TO 900
      IF (OP.EQ.'ONNN') THEN
         YBUFF(12+CHAN) = 1
      ELSE
         YBUFF(12+CHAN) = 0
         END IF
C                                       do selections
      MASK = 0
      DO 30 I = 1,16
         LCOLOR = 0
         II = I - 1
         IC = 0
         DO 25 J = 1,4
            JJ = 2 ** (J-1)
            IF ((YBUFF(12+J).LE.0) .OR. (ZAND(II,JJ).EQ.0)) GO TO 25
               IC = IC + 1
               LCOLOR = ZEOR (LCOLOR, YBUFF(17+J))
 25         CONTINUE
         IV = II
         MODE = 1
         IF (IC.LE.0) MODE = -1
         JERR = FIVASGMDEFGRAPHIC (IV, MODE, LCOLOR, MASK)
         IF (JERR.NE.0) GO TO 800
 30      CONTINUE
C                                       Unfreeze display
      MASK = 0
      JERR = FIVASGMFREEZE (MASK)
      IF (JERR.NE.0) GO TO 900
      GO TO 999
C                                       Cursor color
 50   IF (OP.EQ.'ONNN') THEN
         LCOLOR = YBUFF(22)
      ELSE
         LCOLOR = 0
         END IF
      JERR = FIVASCSCOLOR (LCOLOR)
      IF (JERR.EQ.0) GO TO 999
C                                       Errors
 800  CALL YDOERR ('YGRAPH', JERR, IERR)
      MASK = 0
      JERR = FIVASGMFREEZE (MASK)
      IF (JERR.EQ.0) GO TO 999
C
 900  CALL YDOERR ('YGRAPH', JERR, IERR)
C
 999  RETURN
      END
