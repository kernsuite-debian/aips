      SUBROUTINE YGRAPH (OP, CHAN, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! turns TV graphics planes on and off
C# Y1 TV-basic
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
*>                                      MRC 87/Apr/07: Further colours.
*             5    insert   white             miscellaneous.
*             6    insert   aquamarine        miscellaneous.
*             7    insert   red               miscellaneous.
*=
*<
C             8    insert   purple            cursor
C   Inputs:
C      OP      C*4       'ONNN' or 'OFFF'
C      CHAN    I         channel number (1 - 8)
C   Output:
C      SCRTCH  I(256)    scratch buffer
C      IERR    I         error code of Z...XF: 0 => ok
C                                              2 => input error
C   DeAnza version (W. Jaffe)
C   I have substituted a slower but simpler algorithim than the original
C   IIS code.  That code was near impossible to impliment on DEANZA.
*>                                      MRC 90/May/16:
*   MRC 87/Apr/07: Modifications for the default graphics colours and
*                  for changing them interactively.
*=
*<
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHAN, SCRTCH(256), IERR
C
      LOGICAL   VRTRTC, NOCUR, LON
      INTEGER   DCOLOR(8), ANSERT(8), BITS(16), IJ, ZEOR, ZOR, I,
     *   ICOLOR, IDX, INSERT, J, LCOLOR, ENABLE, ITEMP
      INCLUDE 'INCS:YDEA.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA ANSERT /1,1,1,1,0,0,0,1/
C     BGR values   00:15:15, 00:15:00, 15:03:03, 00:00:00,
C                  15:15:15, 00:15:15, 15:15:00, 15:00:15
      DATA DCOLOR /  17407,    17392,    32371,    16912,
     :               32767,    17407,    32752,    32287/
      DATA VRTRTC, NOCUR /.FALSE., .FALSE./
C                                       For BLACK overlay need to set
C                                       enable bits on, but intensity
C                                       values to zero.  Z4210=16912
      DATA ENABLE /16912/
C-----------------------------------------------------------------------
C                                       Initialization only
      IF (OP.EQ.'INIT') THEN
         IERR = 0
         CALL COPY (8, DCOLOR, YBUFF(GRPHCL))
         GO TO 999
         END IF
C                                        check input
      IERR = 2
      IF ((CHAN.LT.1) .OR. (CHAN.GT.8)) GO TO 999
      IF ((OP.NE.'ONNN') .AND. (OP.NE.'OFFF')) GO TO 999
      IF (YBUFF(GRPHCL+7).EQ.0) CALL COPY (8, DCOLOR, YBUFF(GRPHCL))
C                                        color = 0 => nothing to do
 20   IERR = 0
      INSERT = ANSERT(CHAN)
      ICOLOR = YBUFF(GRPHCL+CHAN-1)
      IF ((ICOLOR.EQ.0) .AND. (INSERT.EQ.0)) GO TO 999
C                                        Cursor
      IF (CHAN.EQ.8) THEN
         CALL FILL (256, ICOLOR, SCRTCH)
         CALL YGGRAM ('WRIT', .TRUE., SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR
            CALL MSGWRT (6)
            END IF
         GO TO 999
         END IF
C                                        read existing graphics LUT
      CALL YGGRAM ('READ', NOCUR, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      IDX = (2 ** (CHAN-1)) + 1
C                                       Set no-intersection color on/off
      IF (OP.EQ.'ONNN') THEN
         SCRTCH(IDX) = ICOLOR
      ELSE
         SCRTCH(IDX) = 0
         END IF
C                                        plane intersections: use XOR
C                                        of colors presently on.
      DO 120 I = 1,256
         ITEMP = I - 1
         CALL ZGTBIT (16, ITEMP, BITS)
C                                        CHAN intersects this level
         IF (BITS(CHAN).EQ.1) THEN
            LCOLOR = 0
            LON = .FALSE.
            DO 110 J = 1,8
               IF (BITS(J).NE.0) THEN
                  IJ = (2**(J-1))
                  LON = LON .OR. (SCRTCH(IJ+1).NE.0)
                  LCOLOR= ZEOR (LCOLOR, SCRTCH(IJ+1))
                  END IF
 110           CONTINUE
C                                       If on fix up enable bits
            IF (LON) THEN
               SCRTCH(I) = ZOR (LCOLOR, ENABLE)
            ELSE
               SCRTCH(I) = 0
               END IF
            END IF
 120     CONTINUE
C                                        return the lut to graphics ram
      CALL YGGRAM ('WRIT', NOCUR, SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('YGRAPH: ERROR',I3,' WRITING THE CURSOR LUT.')
      END
