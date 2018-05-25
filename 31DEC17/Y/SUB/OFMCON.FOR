      SUBROUTINE OFMCON (DOWEDG, IG, NLEVS, DONEW, RPOS, N, OFM, BUFFER,
     *   IERR)
C-----------------------------------------------------------------------
C! interactive making of OFM as step/flat color contours
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2002, 2009, 2015
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
C   interactive creation/modification of color OFMs as step wedge or
C   flat intensity contours
C   Inputs:
C      DOWEDG   R        Number of wedges or contours:
C                           <= 0 => contours; -1,0,1 => use 8
C      IG       I        Graphics channel to use (1 - NGRAPH)
C      NLEVS    I        Number of OFM levels actually used
C      DONEW    R        > 0 => do a new OFM, <= 0 => use old
C      N        I        First dimension of OFM
C   In/out:
C      OFM      R(N,3)   Initial OFM in, Final OFM out
C   Output:
C      RPOS     R(2)     Final cursor position
C      BUFFER   I(*)     scratch buffer
C      IERR     I        error code suitable for POPS
C-----------------------------------------------------------------------
      INTEGER   IG, NLEVS, N, BUFFER(*), IERR
      REAL      DOWEDG, OFM(N,3), RPOS(2), DONEW
C
      INTEGER   QUAD, ITW(3), I, J, IBUT, LG, LMODE,
     *   NEDGE, MAG, IX0, IY0, IX1, IY1, POTERR, IX, IY, IXP(9), IYP(9),
     *   THEWED, NVAL, INCR, MODE, INWEDG, IYR, IYG, IYB, NXP, LXR,
     *   LXG, LXB, IXR, IXG, IXB, IYV, IXV, IXC, IYC, MXP
      REAL      PPOS(2), RBUF(2048), FR, FG, FB, PI, WEDLEV(100), D60,
     *   WEDR(100), WEDG(100), WEDB(100), XC, YC, ALPHA, RC, RR, THETA,
     *   XPOS(2)
      LOGICAL   T, F, DOIT, INVERS, DOPLOT, NOPLOT
      CHARACTER STRING*20, BARS*80
      INCLUDE 'INCS:DCNT.INC'
      INTEGER   BUF0(MAXIMG), BUF1(MAXIMG)
      EQUIVALENCE (BUF0, ILROW), (BUF1, IBLROW)
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                        Common TV parms
      BARS = '----------------------------------------------------'
      LXR = 0
      LXG = 0
      LXB = 0
      QUAD = -1
      INWEDG = ABS (DOWEDG) + 0.1
      IF (INWEDG.LT.2) INWEDG = 8
      IF (INWEDG.GT.99) INWEDG = 99
      POTERR = 49
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL ZTIME (ITW)
      CALL RFILL (100, 1.0, WEDLEV)
      CALL RFILL (100, 1.0, WEDR)
      CALL RFILL (100, 1.0, WEDG)
      CALL RFILL (100, 1.0, WEDB)
      CALL IMANOT ('INIT', IG, IX, IY, 0, 0, 'GREEN', BUFFER, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
      LG = IG + NGRAY
C                                       get visible corners
      MAG = 1 + TVZOOM(1)
      IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
      IX0 = WINDTV(1) - (MAG-1)/2
      IX1 = WINDTV(3) - (MAG-1)/2
      IY0 = WINDTV(2) - (MAG-1)/2
      IY1 = WINDTV(4) - (MAG-1)/2
      IF (MAG.GT.1) IY0 = IY0 + MAG
      IF (MAG.GT.1) IY1 = IY1 + MAG
      IX0 = (IX0 - TVZOOM(2))/MAG + TVZOOM(2)
      IX1 = (IX1 - TVZOOM(2))/MAG + TVZOOM(2)
      IY0 = (IY0 - TVZOOM(3))/MAG + TVZOOM(3)
      IY1 = (IY1 - TVZOOM(3))/MAG + TVZOOM(3)
      IX0 = MAX (1, MIN (MAXXTV(1), IX0))
      IX1 = MAX (1, MIN (MAXXTV(1), IX1))
      IY0 = MAX (1, MIN (MAXXTV(1), IY0))
      IY1 = MAX (1, MIN (MAXXTV(1), IY1))
C                                       Color points
      INVERS = F
      DOPLOT = F
      PI = 3.14159265358
      D60 = PI / 3.0
      ALPHA = 0.80
      XC = (IX0 + IX1) / 2.0
      YC = (IY0 + IY1) / 2.0
      RC = 0.45 * MIN (IX1-IX0, IY1-IY0)
      IXP(1) = XC + 0.5
      IYP(1) = YC + RC + 0.5
      IXP(2) = XC - RC * SIN (D60) + 0.5
      IYP(2) = YC + RC * COS (D60) + 0.5
      IXP(3) = IXP(2)
      IYP(3) = YC - RC * COS (D60) + 0.5
      IXP(4) = IXP(1)
      IYP(4) = YC - RC + 0.5
      IXP(5) = XC + RC * SIN (D60) + 0.5
      IYP(5) = IYP(3)
      IXP(6) = IXP(5)
      IYP(6) = IYP(2)
      IXP(7) = IXP(1)
      IYP(7) = IYP(1)
      IXP(8) = XC + 0.5
      IYP(8) = YC + 0.5
      IXP(9) = IXP(2)
      IYP(9) = IYP(2)
C                                        Graphics drawing: colors
      IX = IXP(1) - 2.5 * CSIZTV(1)
      IY = IYP(1) - 0.5 * CSIZTV(2)
      IX = MAX (1, MIN (MAXXTV(1)-5*CSIZTV(1), IX))
      IY = MAX (1, MIN (MAXXTV(2) - CSIZTV(2), IY))
      CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'GREEN', BUFFER, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
      IX = IXP(2) - 2.0 * CSIZTV(1)
      IY = IYP(2) - 0.5 * CSIZTV(2)
      IX = MAX (1, MIN (MAXXTV(1)-4*CSIZTV(1), IX))
      IY = MAX (1, MIN (MAXXTV(2) - CSIZTV(2), IY))
      CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'CYAN', BUFFER, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
      IX = IXP(3) - 2 * CSIZTV(1)
      IY = IYP(3) - 0.5 * CSIZTV(2)
      IX = MAX (1, MIN (MAXXTV(1)-4*CSIZTV(1), IX))
      IY = MAX (1, MIN (MAXXTV(2) - CSIZTV(2), IY))
      CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'BLUE', BUFFER, IERR)
      IX = IXP(4) - 3.5 * CSIZTV(1)
      IY = IYP(4) - 0.5 * CSIZTV(2)
      IX = MAX (1, MIN (MAXXTV(1)-7*CSIZTV(1), IX))
      IY = MAX (1, MIN (MAXXTV(2) - CSIZTV(2), IY))
      CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'MAGENTA', BUFFER, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
      IX = IXP(5) - 0.5 * CSIZTV(1)
      IY = IYP(5) - 0.5 * CSIZTV(2)
      IX = MAX (1, MIN (MAXXTV(1)-3*CSIZTV(1), IX))
      IY = MAX (1, MIN (MAXXTV(2) - CSIZTV(2), IY))
      CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'RED', BUFFER, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
      IX = IXP(6) - 3.0 * CSIZTV(1)
      IY = IYP(6) - 0.5 * CSIZTV(2)
      IX = MAX (1, MIN (MAXXTV(1)-6*CSIZTV(1), IX))
      IY = MAX (1, MIN (MAXXTV(2) - CSIZTV(2), IY))
      CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'YELLOW', BUFFER, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
C                                        Graphics drawing: lines
      CALL IMVECT ('ONNN', LG, 9, IXP, IYP, BUFFER, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
      IXP(2) = IXP(8)
      IYP(2) = IYP(8)
      IXP(1) = IXP(3)
      IYP(1) = IYP(3)
      IXP(3) = IXP(4)
      IYP(3) = IYP(4)
      CALL IMVECT ('ONNN', LG, 3, IXP, IYP, BUFFER, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
      IXP(1) = IXP(5)
      IYP(1) = IYP(5)
      IXP(3) = IXP(6)
      IYP(3) = IYP(6)
      CALL IMVECT ('ONNN', LG, 3, IXP, IYP, BUFFER, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
C                                        Graphics drawing: numbers
      NEDGE = (4 / MAG) * MAXXTV(1) / 480
      NEDGE = MAX (1, NEDGE)
      INCR = (IY1 - IY0 + 1 - 2*NEDGE - CSIZTV(2)) / MAX (1, INWEDG-1)
      DOIT = INCR.LE.(CSIZTV(2)+1)
      DO 10 I = 1,INWEDG
         IX = IX0 + NEDGE
         IF ((DOIT) .AND. ((I/2)*2.EQ.I)) IX = IX + 2.2*CSIZTV(1)
         IY = IY0 + NEDGE + (I-1) * INCR
         WRITE (STRING,1000) I
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, STRING(:2), BUFFER,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
 10      CONTINUE
C                                        Prepare plot
      CALL FILL (MAXIMG, 0, BUF0)
      CALL FILL (MAXIMG, 255, BUF1)
      IYB = IY1 - 4 * (NEDGE + CSIZTV(2)) + 1
      IYG = IYB + NEDGE + CSIZTV(2)
      IYR = IYG + NEDGE + CSIZTV(2)
      IYV = IYR + NEDGE + CSIZTV(2)
      IXC = IX0 + NEDGE + 4 * CSIZTV(1)
      IYC = IYG
      IX = IXC + 3 * CSIZTV(1)
      NXP = IX + 2 * CSIZTV(1) - CSIZTV(1)/2
      MXP = IX1 - NEDGE - CSIZTV(1) + 1
      IXV = MXP - 4.5 * CSIZTV(1)
      NOPLOT = (MXP.LT.NXP+10) .OR. (IYB.LT.2) .OR.
     *   (IXV.LE.NXP+CSIZTV(1))
      DOPLOT = .NOT.NOPLOT
      IF (DOPLOT) THEN
         I = (MXP - NXP - (IX1-IX0)*0.5) / 2
         IF (I.GT.2) THEN
            IXC = IXC + I
            IX = IX + I
            NXP = NXP + I
            MXP = MXP - I
            IXV = IXV - I
            END IF
         IXP(1) = MXP
         IXP(2) = MXP
         IYP(1) = IYB
         IYP(2) = IYR + CSIZTV(2) - 1
         CALL IMVECT ('ONNN', LG, 2, IXP, IYP, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IXP(1) = NXP + 0.75 * (MXP - NXP) + 0.5
         IXP(2) = IXP(1)
         CALL IMVECT ('ONNN', LG, 2, IXP, IYP, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IXP(1) = NXP + 0.50 * (MXP - NXP) + 0.5
         IXP(2) = IXP(1)
         CALL IMVECT ('ONNN', LG, 2, IXP, IYP, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IXP(1) = NXP + 0.25 * (MXP - NXP) + 0.5
         IXP(2) = IXP(1)
         CALL IMVECT ('ONNN', LG, 2, IXP, IYP, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IXP(1) = NXP
         IXP(2) = NXP
         CALL IMVECT ('ONNN', LG, 2, IXP, IYP, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         CALL IMANOT ('WRIT', IG, IX, IYB, 0, 0, 'B', BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         CALL IMANOT ('WRIT', IG, IX, IYG, 0, 0, 'G', BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         CALL IMANOT ('WRIT', IG, IX, IYR, 0, 0, 'R', BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IYB = IYB + CSIZTV(2) / 2
         IYG = IYG + CSIZTV(2) / 2
         IYR = IYR + CSIZTV(2) / 2
         IX = NXP - CSIZTV(1) + CSIZTV(1) / 2
         CALL IMANOT ('WRIT', IG, IX, IYV, 0, 0, '0', BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         END IF
C                                        Initial OFMs
      NVAL = NLEVS / INWEDG
      IF (DOWEDG.GT.0.0) THEN
         DO 15 I = 1,NVAL
            RBUF(I) = (I - 1.0) / (NVAL - 1.0)
 15         CONTINUE
      ELSE
         CALL RFILL (NVAL, 1.0, RBUF)
         END IF
      IF (DONEW.GT.0.0) THEN
         J = OFMINP + 1
         CALL RFILL (J, 1.0, OFM(1,1))
         DO 20 I = 1,INWEDG
            IX = (I-1) * NVAL + 1
            CALL RCOPY (NVAL, RBUF, OFM(IX,1))
 20         CONTINUE
         CALL RCOPY (J, OFM(1,1), OFM(1,2))
         CALL RCOPY (J, OFM(1,1), OFM(1,3))
         CALL OFMIO ('WRIT', N, NLEVS, F, OFM, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                        Instructions
 25   MSGTXT = BARS
      CALL MSGWRT (1)
      MSGTXT = 'Hit button A to select a contour to color'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button B to select a contour to vary intensity'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button C to set a contour to black & white'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button D to exit'
      CALL MSGWRT (1)
      XPOS(1) = XC
      XPOS(2) = YC
C                                       turn on cursor
      RPOS(1) = (IX0 + IX1) / 2.0
      RPOS(2) = (IY0 + IY1) / 2.0
      CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 980
      THEWED = 0
      MODE = 0
C                                       init plot
      IF (.NOT.NOPLOT) THEN
         STRING = ' '
         CALL IMANOT ('WRIT', IG, IXC, IYC, 0, 0, STRING(:2), BUFFER,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         CALL IMANOT ('WRIT', IG, IXV, IYV, 0, 0, STRING(:5), BUFFER,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IF (LXB.GT.0) THEN
            CALL YIMGIO ('WRIT', LG, NXP, IYB, 0, LXB, BUF0, IERR)
            IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
            END IF
         IF (LXG.GT.0) THEN
            CALL YIMGIO ('WRIT', LG, NXP, IYG, 0, LXG, BUF0, IERR)
            IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
            END IF
         IF (LXR.GT.0) THEN
            CALL YIMGIO ('WRIT', LG, NXP, IYR, 0, LXR, BUF0, IERR)
            IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
            END IF
         LXR = 0
         LXG = 0
         LXB = 0
         END IF
C                                       read loop
 30   CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
C                                       set a gamma
         LMODE = MODE
         IF (DOIT) THEN
            IF (IBUT.GT.0) THEN
C                                       select a step
               IF (MODE.LE.0) THEN
                  THEWED = (RPOS(2) - IY0 - 1) /INCR + 1
                  THEWED = MAX (1, MIN (INWEDG, THEWED))
                  END IF
               IF (IBUT.EQ.1) THEN
C                                       start hue and saturation
                  IF (MODE.NE.1) THEN
                     MSGTXT = BARS
                     CALL MSGWRT (1)
                     MSGTXT = 'Cursor position sets hue and saturation'
                     CALL MSGWRT (1)
                     MSGTXT = 'Hit button A to invert the colors''' //
     *                   ' intensities'
                     CALL MSGWRT (1)
                     MSGTXT = 'Hit button B to change intensity instead'
                     CALL MSGWRT (1)
                     MSGTXT = 'Hit button C to stop changing the ' //
     *                  'contour'
                     CALL MSGWRT (1)
                     MODE = 1
                     RPOS(1) = XPOS(1)
                     RPOS(2) = XPOS(2)
                     CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
                     IF (IERR.NE.0) GO TO 980
                     PPOS(1) = 0.0
                     PPOS(2) = 0.0
C                                       invert center: black vs white
                  ELSE
                     INVERS = .NOT.INVERS
                     PPOS(1) = 0.0
                     PPOS(2) = 0.0
                     END IF
C                                       start intensity changing
               ELSE IF (IBUT.LE.3) THEN
                  IF (MODE.NE.2) THEN
                     MSGTXT = BARS
                     CALL MSGWRT (1)
                     MSGTXT = 'Cursor Y position controls intensity'
                     CALL MSGWRT (1)
                     MSGTXT = 'Hit button A to do hue/saturation ' //
     *                  ' instead'
                     CALL MSGWRT (1)
                     MSGTXT = 'Hit button C to stop changing the ' //
     *                  'contour'
                     CALL MSGWRT (1)
                     MODE = 2
                     RPOS(1) = XC
                     RPOS(2) = IY0 + WEDLEV(THEWED) * (IY1 - IY0)
                     CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
                     IF (IERR.NE.0) GO TO 980
                     PPOS(1) = 0.0
                     PPOS(2) = 0.0
                     END IF
               ELSE IF (IBUT.LE.7) THEN
C                                       revert to black and white
                  IF (MODE.LE.0) THEN
                     IX = (THEWED - 1) * NVAL + 1
                     WEDR(THEWED) = 1.0
                     WEDG(THEWED) = 1.0
                     WEDB(THEWED) = 1.0
                     FG = WEDLEV(THEWED)
                     IX = (THEWED - 1) * NVAL
                     DO 40 I = 1,NVAL
                        OFM(IX+I,1) = FG * RBUF(I)
                        OFM(IX+I,2) = FG * RBUF(I)
                        OFM(IX+I,3) = FG * RBUF(I)
 40                     CONTINUE
                     CALL OFMIO ('WRIT', N, NLEVS, F, OFM, IERR)
                     IF (IERR.NE.0) GO TO 980
C                                       freeze the contour
                  ELSE
                     MODE  = 0
                     PPOS(1) = 0.0
                     PPOS(2) = 0.0
                     END IF
               ELSE
                  POTERR = 0
                  GO TO 980
                  END IF
C                                       init the plot
               IF ((DOPLOT) .AND. (LMODE.LE.0)) THEN
                  WRITE (STRING,1000) THEWED
                  CALL IMANOT ('WRIT', IG, IXC, IYC, 0, 0, STRING(:2),
     *               BUFFER, IERR)
                  IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
                  WRITE (STRING,1040) WEDLEV(THEWED)
                  CALL IMANOT ('WRIT', IG, IXV, IYV, 0, 0, STRING(:5),
     *               BUFFER, IERR)
                  IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
                  IXB = WEDB(THEWED) * (MXP - NXP) + 1
                  CALL YIMGIO ('WRIT', LG, NXP, IYB, 0, IXB, BUF1, IERR)
                  IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
                  IXG = WEDG(THEWED) * (MXP - NXP) + 1
                  CALL YIMGIO ('WRIT', LG, NXP, IYG, 0, IXG, BUF1, IERR)
                  IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
                  IXR = WEDR(THEWED) * (MXP - NXP) + 1
                  CALL YIMGIO ('WRIT', LG, NXP, IYR, 0, IXR, BUF1, IERR)
                  IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
                  LXB = IXB
                  LXG = IXG
                  LXR = IXR
                  END IF
            ELSE IF (MODE.EQ.1) THEN
               XPOS(1) = RPOS(1)
               XPOS(2) = RPOS(2)
               RR = SQRT ((RPOS(1)-XC)**2 + (RPOS(2)-YC)**2) / RC
               IF (RR.LE.0.0) THEN
                  FR = 1.0
                  FG = 1.0
                  FB = 1.0
               ELSE
                  RR = RR ** ALPHA
                  FR = MAX (0.0, 1.0 - RR)
                  FG = FR
                  FB = FR
                  THETA = ATAN2 (XC - RPOS(1), RPOS(2) - YC)
                  IF (INVERS) THETA = THETA - PI
                  IF (THETA.LT.0.0) THETA = THETA + 2.0 * PI
                  IF (THETA.LE.D60) THEN
                     THETA = THETA * 1.5
                     FG = 1.0
                     FB = MAX (0.0, MIN (1.0, FB + RR * SIN (THETA)))
                  ELSE IF (THETA.LE.2.*D60) THEN
                     THETA = (THETA - D60) * 1.5
                     FG = MAX (0.0, MIN (1.0, FG + RR * COS (THETA)))
                     FB = 1.0
                  ELSE IF (THETA.LE.3.*D60) THEN
                     THETA = (THETA - 2.*D60) * 1.5
                     FB = 1.0
                     FR = MAX (0.0, MIN (1.0, FR + RR * SIN (THETA)))
                  ELSE IF (THETA.LE.4.*D60) THEN
                     THETA = (THETA - 3.*D60) * 1.5
                     FB = MAX (0.0, MIN (1.0, FB + RR * COS (THETA)))
                     FR = 1.0
                  ELSE IF (THETA.LE.5.*D60) THEN
                     THETA = (THETA - 4.*D60) * 1.5
                     FR = 1.0
                     FG = MAX (0.0, MIN (1.0, FG + RR * SIN (THETA)))
                  ELSE
                     THETA = (THETA - 5.*D60) * 1.5
                     FR = MAX (0.0, MIN (1.0, FR + RR * COS (THETA)))
                     FG = 1.0
                     END IF
                  END IF
               IF (INVERS) THEN
                  WEDR(THEWED) = 1. - FR
                  WEDG(THEWED) = 1. - FG
                  WEDB(THEWED) = 1. - FB
               ELSE
                  WEDR(THEWED) = FR
                  WEDG(THEWED) = FG
                  WEDB(THEWED) = FB
                  END IF
               IF (DOPLOT) THEN
                  IXB = WEDB(THEWED) * (MXP - NXP) + 1
                  IF (IXB.GT.LXB) THEN
                     IX = NXP + LXB - 1
                     IY = IXB - LXB
                     CALL YIMGIO ('WRIT', LG, IX, IYB, 0, IY, BUF1,
     *                  IERR)
                  ELSE IF (IXB.LT.LXB) THEN
                     IX = NXP + IXB - 1
                     IY = LXB - IXB
                     CALL YIMGIO ('WRIT', LG, IX, IYB, 0, IY, BUF0,
     *                  IERR)
                     END IF
                  IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
                  IXG = WEDG(THEWED) * (MXP - NXP) + 1
                  IF (IXG.GT.LXG) THEN
                     IX = NXP + LXG - 1
                     IY = IXG - LXG
                     CALL YIMGIO ('WRIT', LG, IX, IYG, 0, IY, BUF1,
     *                  IERR)
                  ELSE IF (IXG.LT.LXG) THEN
                     IX = NXP + IXG - 1
                     IY = LXG - IXG
                     CALL YIMGIO ('WRIT', LG, IX, IYG, 0, IY, BUF0,
     *                  IERR)
                     END IF
                  IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
                  IXR = WEDR(THEWED) * (MXP - NXP) + 1
                  IF (IXR.GT.LXR) THEN
                     IX = NXP + LXR - 1
                     IY = IXR - LXR
                     CALL YIMGIO ('WRIT', LG, IX, IYR, 0, IY, BUF1,
     *                  IERR)
                  ELSE IF (IXR.LT.LXR) THEN
                     IX = NXP + IXR - 1
                     IY = LXR - IXR
                     CALL YIMGIO ('WRIT', LG, IX, IYR, 0, IY, BUF0,
     *                  IERR)
                     END IF
                  IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
                  LXB = IXB
                  LXG = IXG
                  LXR = IXR
                  END IF
            ELSE IF (MODE.EQ.2) THEN
               WEDLEV(THEWED) = MAX (0., MIN (1., (RPOS(2) - IY0) /
     *               (IY1 - IY0)))
               IF (DOPLOT) THEN
                  WRITE (STRING,1040) WEDLEV(THEWED)
                  CALL IMANOT ('WRIT', IG, IXV, IYV, 0, 0, STRING(:5),
     *               BUFFER, IERR)
                  IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
                  END IF
               END IF
            IF (MODE.GT.0) THEN
               FR = WEDR(THEWED) * WEDLEV(THEWED)
               FG = WEDG(THEWED) * WEDLEV(THEWED)
               FB = WEDB(THEWED) * WEDLEV(THEWED)
               IX = (THEWED - 1) * NVAL
               DO 50 I = 1,NVAL
                  OFM(IX+I,1) = FB * RBUF(I)
                  OFM(IX+I,2) = FG * RBUF(I)
                  OFM(IX+I,3) = FR * RBUF(I)
 50               CONTINUE
               CALL OFMIO ('WRIT', N, NLEVS, F, OFM, IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            END IF
C                                       loop
         IF ((MODE.EQ.0) .AND. (LMODE.NE.0)) GO TO 25
         GO TO 30
C                                       close down
C                                       messages on TV error
 980  IF (IERR.EQ.0) GO TO 990
         WRITE (MSGTXT,1980) IERR
         CALL MSGWRT (7)
C                                       error management
 990  IERR = POTERR
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I2.2)
 1040 FORMAT (F5.3)
 1980 FORMAT ('TV ACTION ERROR CODE',I5,' RETURNED')
      END
