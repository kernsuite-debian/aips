      SUBROUTINE HIENH (HCHAN, ICHAN, ITYPE, RPOS, IERR)
C-----------------------------------------------------------------------
C! interactive linear enhancement of 2-image hue-intensity TV display
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2008, 2015
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
C   HIENH performs an interactive linear enhancement of the hue and
C   intensity assignments for displaying two images as hue/intensity.
C   X cursor => intercept, Y cursor => slope.
C   Button A: flip which image determines hue and which determines int.
C   Button B: flip whether hue or intensity is being enhanced.
C   Button C: flip sign of enhancement.
C   Button D: exit.
C   Inputs:
C      HCHAN   I          channel select for hue image (1-NGRAY)
C      ICHAN   I          channel select for int. image (1-NGRAY)
C      ITYPE   I          type of color table: 0 -> spectrum
C                                 1 -> circular
C   In/Out:
C      RPOS    R(2)       Cursor position: initial -> final
C   Output:
C      IERR    I          error code of ZM70XF: 0 => ok
C   Common output:
C      /AIPSCR/ BUFFER  I(3072)    scratch buffer
C   NOTE - these dimensions work for MAXINT <= 511 only.  There are no
C   TVs other than IIS M70 and IIS M75 which can do this algorithm at
C   present.
C-----------------------------------------------------------------------
      INTEGER   HCHAN, ICHAN, ITYPE, IERR
      REAL      RPOS(2)
C
      INCLUDE 'INCS:PTVC.INC'
      INTEGER   IX, IX0, IY0, IBUT, IFLIP, MXI, HCC, ICC, SFLIP, IPOS,
     *   QUAD, JERR, ITW(3), I, LOGLUT(TVMLUT), LUT(TVMLUT), IC, HC,
     *   RBUF(TVMLUT), GBUF(TVMLUT), BBUF(TVMLUT), TEMPC, BUFFER(TVMLUT)
      LOGICAL   F, DOIT, HCHANG
      REAL      PPOS(2), A, SAVPOS(2), TMPPOS
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /AIPSCR/ BUFFER, LUT, LOGLUT, RBUF, GBUF, BBUF
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      ICC = ICHAN
      IC = 2 ** (ICHAN-1)
      HCC = HCHAN
      HC = 2 ** (HCHAN-1)
      HCHANG = .FALSE.
C                                       calculate log look up table
      A = LUTOUT / LOG10 (REAL(MAXINT))
      MXI = MAXINT + 1
      DO 10 I = 1,MAXINT
         LOGLUT(I+1) =  A * LOG10 (REAL(I)) + 0.5
 10      CONTINUE
      LOGLUT(1) = 0
C                                       Initialize two channels
C                                       uses MAXINT here, fixed to
C                                       LUTOUT at label 35
C                                       Red table: spectrum
 20   IF (ITYPE.LT.1) THEN
         IX0 = 0.26 * MAXINT
         IX = 0.41 * MAXINT
         CALL STRLIN (0, 0, IX0, 0, RBUF)
         CALL STRLIN (IX0, 0, IX, MAXINT, RBUF)
         CALL STRLIN (IX, MAXINT, MAXINT, MAXINT, RBUF)
C                                       Green table
         IY0 = 0.267 * MAXINT
         IX0 = 0.353 * MAXINT
         IX = 0.479 * MAXINT
         CALL STRLIN (0, IY0, IX0, MAXINT, GBUF)
         CALL STRLIN (IX0, MAXINT, IX, MAXINT, GBUF)
         IX0 = 0.981 * MAXINT
         CALL STRLIN (IX, MAXINT, IX0, 0, GBUF)
         CALL STRLIN (IX0, 0, MAXINT, 0, GBUF)
C                                       blue table
         IX0 = 0.063 * MAXINT
         IX = 0.334 * MAXINT
         CALL STRLIN (0, MAXINT, IX0, MAXINT, BBUF)
         CALL STRLIN (IX0, MAXINT, IX, 0, BBUF)
         CALL STRLIN (IX, 0, MAXINT, 0, BBUF)
C                                       Circular: red table
      ELSE
         IX0 = 0.24 * MAXINT
         IX = 0.36 * MAXINT
         CALL STRLIN (0, 0, IX0, 0, RBUF)
         CALL STRLIN (IX0, 0, IX, MAXINT, RBUF)
         IX0 = 0.77 * MAXINT
         CALL STRLIN (IX, MAXINT, IX0, MAXINT, RBUF)
         IY0 = 0.25 * MAXINT
         CALL STRLIN (IX0, MAXINT, MAXINT, IY0, RBUF)
C                                       Green table
         IX0 = 0.16 * MAXINT
         IX = 0.37 * MAXINT
         CALL STRLIN (0, IY0, IX0, MAXINT, GBUF)
         CALL STRLIN (IX0, MAXINT, IX, MAXINT, GBUF)
         IX0 = 0.66 * MAXINT
         CALL STRLIN (IX, MAXINT, IX0, 0, GBUF)
         CALL STRLIN (IX0, 0, MAXINT, 0, GBUF)
C                                       Blue table
         IX0 = 0.13 * MAXINT
         IX = 0.286 * MAXINT
         CALL STRLIN (0, MAXINT, IX0, MAXINT, BBUF)
         CALL STRLIN (IX0, MAXINT, IX, 0, BBUF)
         IX0 = 0.57 * MAXINT
         CALL STRLIN (IX, 0, IX0, 0, BBUF)
         IX = 0.8 * MAXINT
         CALL STRLIN (IX0, 0, IX, MAXINT, BBUF)
         CALL STRLIN (IX, MAXINT, MAXINT, MAXINT, BBUF)
         END IF
C                                       Take log via lookup
      DO 30 I = 1,MXI
         RBUF(I) = LOGLUT (RBUF(I) + 1)
         GBUF(I) = LOGLUT (GBUF(I) + 1)
         BBUF(I) = LOGLUT (BBUF(I) + 1)
 30      CONTINUE
      RBUF(1) = 0
      GBUF(1) = 0
      BBUF(1) = 0
C                                       init  position values
      IF ((RPOS(1).LT.WINDTV(1)) .OR. (RPOS(1).GT.WINDTV(3)) .OR.
     *   (RPOS(2).LT.WINDTV(2)) .OR. (RPOS(2).GT.WINDTV(4))) THEN
         RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2.0
         RPOS(2) = (WINDTV(4) - WINDTV(2)) / 3.0 + WINDTV(2)
         SAVPOS(1) = RPOS(1)
         SAVPOS(2) = RPOS(2)
         END IF
      IFLIP = 1
      SFLIP = 1
C                                       Init hue LUTs (HCHANG=F) or
C                                       Init intensity LUTs
      DOIT = .NOT.HCHANG
      CALL HILUT (HC, IC, DOIT, SAVPOS, IFLIP, IERR)
      IPOS = 1
      IF (IERR.NE.0) GO TO 900
      IF (HCHANG) THEN
         WRITE (MSGTXT,1040) HCC, ICC
      ELSE
         WRITE (MSGTXT,1041) ICC, HCC
         END IF
      CALL MSGWRT (1)
      WRITE (MSGTXT,1042)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1043)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1044)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1045)
      CALL MSGWRT (1)
C                                       turn on cursor
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL ZTIME (ITW)
      QUAD = -1
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
      IPOS = 2
      IF (IERR.NE.0) GO TO 900
C                                        read loop
 50   CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
      IPOS = 3
      IF ((IBUT.LE.7) .AND. (IERR.EQ.0)) THEN
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF (DOIT) THEN
C                                        Check for buttons
C                                        Flip hue/intensity assignments
            IF (IBUT.EQ.1) THEN
               TEMPC = IC
               IC = HC
               HC = TEMPC
               TEMPC = ICC
               ICC = HCC
               HCC = TEMPC
               GO TO 20
C                                        Flip which is being enhanced
            ELSE IF (IBUT.EQ.2) THEN
               TMPPOS = SAVPOS(1)
               SAVPOS(1) = RPOS(1)
               RPOS(1) = TMPPOS
               TMPPOS = SAVPOS(2)
               SAVPOS(2) = RPOS(2)
               RPOS(2) = TMPPOS
               TEMPC = IFLIP
               IFLIP = SFLIP
               SFLIP = TEMPC
               CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
               IPOS = 4
               IF (IERR.NE.0) GO TO 900
               HCHANG = .NOT. HCHANG
               IF (HCHANG) WRITE (MSGTXT,1060) HCC
               IF (.NOT.HCHANG) WRITE (MSGTXT,1061) ICC
               CALL MSGWRT (1)
C                                       Flip sign of slope
            ELSE IF (IBUT.EQ.4) THEN
               IFLIP = -IFLIP
               END IF
C                                        new LUT
            CALL HILUT (HC, IC, HCHANG, RPOS, IFLIP, IERR)
            IPOS = 5
            IF (IERR.NE.0) GO TO 900
            END IF
         GO TO 50
         END IF
C                                        off graphics & cursor
 900  CALL YCURSE ('OFFF', F, F, RPOS, QUAD, IBUT, JERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1900) IERR, IPOS
         CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('Enhance hue channel',I2,' with intensity channel',I2)
 1041 FORMAT ('Enhance intensity channel',I2,' with hue channel',I2)
 1042 FORMAT ('Cursor X controls intercept, Y slope of the LUT')
 1043 FORMAT ('Press button A to switch roles of the channels')
 1044 FORMAT ('Press button B to switch which is enhanced')
 1045 FORMAT ('Press button C to reverse slope, button D to exit')
 1060 FORMAT ('Enhance hue channel',I2)
 1061 FORMAT ('Enhance intensity channel',I2)
 1900 FORMAT ('HIENH: TV ERROR',I4,' AT',I4)
      END
