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
C             8    insert   purple            cursor
C   by default.  It may be altered by YGRAFX.
C   Inputs:
C      OP      C*4       'ONNN' or 'OFFF',
C                        'INIT' sets default colors in common
C      CHAN    I         channel number (1 - 8)
C   Output:
C      SCRTCH  I(256)    scratch buffer
C      IERR    I         error code of Z...XF: 0 => ok
C                                              2 => input error
C   Version for IIS Models 70 & 75 - calls YGGRAM
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHAN, SCRTCH(256), IERR
C
      LOGICAL   VRTRTC
      INTEGER   NOP, DCOLOR(8), ANSERT(8), BITS(16), IJ, INMASK, ZEOR,
     *   ZOR, ZAND, I, ICOLOR, IDX, INSERT, J, LCOLOR, ITEMP
      INCLUDE 'INCS:DTVC.INC'
      DATA ANSERT /1,1,1,1,0,0,0,1/
C     DATA DCOLOR /Z7FE0, Z0BE0, Z7E9F, 0, 0, 0, 0, Z7C1F/
      DATA DCOLOR /32736,  3040, 32415, 0, 0, 0, 0, 31775/
      DATA VRTRTC, NOP /.FALSE.,2/
C-----------------------------------------------------------------------
C                                       Initialization only
      IF (OP.EQ.'INIT') THEN
         IERR = 0
         CALL COPY (8, DCOLOR, YBUFF)
         GO TO 999
         END IF
C                                       Check input
      IERR = 2
      IF ((CHAN.LT.1) .OR. (CHAN.GT.8)) GO TO 999
      IF ((OP.NE.'ONNN') .AND. (OP.NE.'OFFF')) GO TO 999
C                                       Color = 0 => nothing to do
      IF (YBUFF(8).EQ.0) CALL COPY (8, DCOLOR, YBUFF)
      IERR = 0
      INSERT = ANSERT(CHAN)
      ICOLOR = YBUFF(CHAN)
      IF ((ICOLOR.EQ.0) .AND. (INSERT.EQ.0)) GO TO 999
C                                       Read existing graphics LUT
      CALL YGGRAM ('READ', VRTRTC, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      IDX = (2 ** (CHAN-1)) + 1
C                                       Adjustment for cursor plane
C                                       ON
      IF (OP.EQ.'ONNN') THEN
         SCRTCH(IDX) = ICOLOR
C                                       Plane intersections: use XOR
C                                       of colors
         DO 20 I = 1,256
            ITEMP = I - 1
            CALL ZGTBIT (16, ITEMP, BITS)
            IF (BITS(CHAN).EQ.1) THEN
C                                       CHAN intersects this level
               LCOLOR = 0
               DO 10 J = 1,8
                  IF (BITS(J).EQ.1) THEN
                     IJ = 2 ** (J-1)
                     LCOLOR= ZEOR (LCOLOR, SCRTCH(IJ+1))
                     END IF
 10               CONTINUE
C                                       Force insert
               CALL ZGTBIT (16, LCOLOR, BITS)
               BITS(16) = INSERT
               CALL ZPTBIT (16, SCRTCH(I), BITS)
               END IF
 20         CONTINUE
C                                       OFF
C                                       Build mask of planes which
C                                       are on + inserted
      ELSE
         INMASK = 0
         DO 30 I = 1,8
            IF (I.NE.CHAN) THEN
               IJ = 2 ** (I-1)
               CALL ZGTBIT (16, SCRTCH(IJ+1), BITS)
               IF (BITS(16).EQ.1) INMASK = ZOR (INMASK, IJ)
               END IF
 30         CONTINUE
C                                       Remove plane intersections
         ICOLOR = SCRTCH(IDX)
         DO 40 I = 1,256
            ITEMP = I - 1
            CALL ZGTBIT (16, ITEMP, BITS)
            IF (BITS(CHAN).EQ.1) THEN
               LCOLOR = ZEOR (ICOLOR, SCRTCH(I))
               IF (ZAND(ITEMP, INMASK).NE.0) THEN
                  CALL ZGTBIT (16, LCOLOR, BITS)
                  BITS(16) = 1
                  CALL ZPTBIT (16, LCOLOR, BITS)
                  END IF
               SCRTCH(I) = LCOLOR
               END IF
 40         CONTINUE
         END IF
C                                        Return the LUT to graphics RAM
 200  CALL YGGRAM ('WRIT', VRTRTC, SCRTCH, IERR)
C
 999  RETURN
      END
