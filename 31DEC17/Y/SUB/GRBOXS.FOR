      SUBROUTINE GRBOXS (IG, MBOX, NBOX, BBLC, BTRC, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! sets rectangular boxes or diagonal line with TV cursor and graphics
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-2000, 2003, 2008-2009, 2015
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
C   GRBOXS uses a graphics plane to show the user rectangular and
C   circular boxes as they are displayed, set, and/or reset with the
C   cursor.
C   Inputs:
C      IG      I        graphics plane to use
C      MBOX    I        Maximum number boxes allowed:
C                          if < 0, draw NBOX boxes and exit
C   In/Out:
C      NBOX    I        in/out: number set on input/output
C      BBLC    R(7,*)   Bottom left corners
C      BTRC    R(7,*)   Top right corners
C   Output:
C      SCRTCH  I(*)     Scratch buffer: > X dimension (>1280)
C      IERR    I        Error code > 0 => a problem
C   Input in common:
C      CATBLK  I(256)   Image catalog header for image being used
C   For circular: BBLC(1,i) = -1, BBLC(2,i) = radius, BTRC(,i) center
C   For elliptical (drawing only): BBLC(1,i) = -X radius, BBLC(2,i) =
C      -Y radius, BTRC(,i) = center
C-----------------------------------------------------------------------
      INTEGER   IG, NBOX, MBOX, SCRTCH(*), IERR
      REAL      BBLC(7,*), BTRC(7,*)
C
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   ICH, ITW(3), LBO, IL, QUAD, IBUT, IBO, I, JERR, IPOS,
     *   LTVSC(2), ITEMP, SCROLX, SCROLY, INBOX, IC(2,MXNBOX), J,
     *   IR(2,MXNBOX), IB(2,MXNBOX), IT(2,MXNBOX), JL, EVERYC, IPASS,
     *   LBUT, EVERYR, ISONTV(MXNBOX), NOFF
      REAL      PPOS(2), RPOS(2), DLIM, IMWIND(4), TVAREA(4), RDEP(5),
     *   RADIUS, SX, SY, SR
      LOGICAL   T, F, DOIT, ONGR, RESET, NXTBOX, WARN, CIRCLE(MXNBOX)
      PARAMETER (EVERYC = 20, EVERYR = EVERYC/4)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (IT, IC), (IB, IR)
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
      WARN = T
      QUAD = 0
      ICH = NGRAY + IG
      IF ((MBOX.LT.0) .AND. (NBOX.LE.0)) THEN
         CALL YZERO (ICH, IERR)
         GO TO 999
         END IF
      CALL FILL (MXNBOX, 0, ISONTV)
C                                       Check inputs
      IERR = 2
      IF ((IG.LT.1) .OR. (IG.GT.NGRAPH)) GO TO 999
      RESET = F
      INBOX = NBOX
      IF (MBOX.GT.MXNBOX) GO TO 999
      IF ((INBOX.GT.MBOX) .AND. (MBOX.GT.0)) GO TO 999
      CALL FILL (MXNBOX, .FALSE., CIRCLE)
C                                       windows
      IF ((CATBLK(IIWIN+2).LE.CATBLK(IIWIN)) .OR.
     *   (CATBLK(IIWIN+3).LE.CATBLK(IIWIN+1))) GO TO 999
      IF ((CATBLK(IICOR+2).LE.CATBLK(IICOR)) .OR.
     *   (CATBLK(IICOR+3).LE.CATBLK(IICOR+1))) GO TO 999
      IMWIND(1) = CATBLK(IIWIN)
      IMWIND(2) = CATBLK(IIWIN+1)
      IMWIND(3) = CATBLK(IIWIN+2)
      IMWIND(4) = CATBLK(IIWIN+3)
      TVAREA(1) = CATBLK(IICOR)
      TVAREA(2) = CATBLK(IICOR+1)
      TVAREA(3) = CATBLK(IICOR+2)
      TVAREA(4) = CATBLK(IICOR+3)
      RDEP(1) = CATBLK(IIDEP+0)
      RDEP(2) = CATBLK(IIDEP+1)
      RDEP(3) = CATBLK(IIDEP+2)
      RDEP(4) = CATBLK(IIDEP+3)
      RDEP(5) = CATBLK(IIDEP+4)
      IL = 0
C                                       Init
      NBOX = 1
      IBO = 0
      CALL YHOLD ('ONNN', IERR)
      CALL YZERO (ICH, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZTIME (ITW)
      LBO = 0
C                                       turn on graphics to be certain
      LTVSC(1) = TVSCGX
      LTVSC(2) = TVSCGY
      I = 2 ** (NGRAY+IG-1)
      ONGR = MOD (TVLIMG(1)/I, 2) .EQ. 1
      CALL YSLECT ('ONNN', ICH, 0, SCRTCH, IERR)
      IPOS = 1
      IF (IERR.NE.0) GO TO 980
C                                       no scroll on graphics now
      ITEMP = 2 ** NGRAY
      SCROLX = TVSCRX(1)
      SCROLY = TVSCRY(1)
      CALL YSCROL (ITEMP, SCROLX, SCROLY, T, IERR)
      IPOS = 2
      IF (IERR.NE.0) GO TO 980
      CALL YHOLD ('OFFF', IERR)
      SX = (TVAREA(3) - TVAREA(1)) / (IMWIND(3) - IMWIND(1))
      SY = (TVAREA(4) - TVAREA(2)) / (IMWIND(4) - IMWIND(2))
C                                       Init values provided
      IF (INBOX.GT.0) THEN
         IERR = 2
C                                       Convert image to TV pixels
         DO 20 I = 1,INBOX
            CIRCLE(I) = BBLC(1,I).LT.-0.99
            IF (CIRCLE(I)) THEN
               IF (BBLC(2,I).GT.0.99) THEN
                  IR(1,I) = NINT (BBLC(2,I) * SX)
                  IR(2,I) = NINT (BBLC(2,I) * SY)
               ELSE
                  IR(1,I) = NINT (-BBLC(1,I) * SX)
                  IR(2,I) = NINT (-BBLC(2,I) * SY)
                  END IF
               IR(1,I) = MAX (1, IR(1,I))
               IR(2,I) = MAX (1, IR(2,I))
               IC(1,I) = NINT (TVAREA(1) + (BTRC(1,I)-IMWIND(1)) * SX)
               IC(2,I) = NINT (TVAREA(2) + (BTRC(2,I)-IMWIND(2)) * SY)
            ELSE
               IB(1,I) = NINT (TVAREA(1) + (BBLC(1,I) - IMWIND(1)) * SX)
               IT(1,I) = NINT (TVAREA(1) + (BTRC(1,I) - IMWIND(1)) * SX)
               IB(2,I) = NINT (TVAREA(2) + (BBLC(2,I) - IMWIND(2)) * SY)
               IT(2,I) = NINT (TVAREA(2) + (BTRC(2,I) - IMWIND(2)) * SY)
               END IF
 20         CONTINUE
         IERR = 0
         NXTBOX = .TRUE.
         IPASS = 0
C         QUAD = -1
         NBOX = INBOX
         LBO = INBOX
         IF (CIRCLE(NBOX)) THEN
            RPOS(1) = IC(1,NBOX)
            RPOS(2) = IC(2,NBOX)
         ELSE
            RPOS(1) = IB(1,NBOX)
            RPOS(2) = IB(2,NBOX)
            END IF
         IF (MBOX.GE.0) THEN
            IPOS = 30
            CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
            IF (IERR.EQ.2) THEN
               DO 25 I = 1,NBOX
                  J = NBOX - I
                  IF (J.EQ.0) THEN
                     RPOS(1) = MAXXTV(1) / 2
                     RPOS(2) = MAXXTV(2) / 2
                     CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
                  ELSE
                     IF (CIRCLE(J)) THEN
                        RPOS(1) = IC(1,J)
                        RPOS(2) = IC(2,J)
                     ELSE
                        RPOS(1) = IB(1,J)
                        RPOS(2) = IB(2,J)
                        END IF
                     CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
                     END IF
                  IF (IERR.EQ.0) GO TO 30
                  IF (IERR.NE.2) GO TO 980
 25               CONTINUE
               END IF
            END IF
 30      IF (IERR.NE.0) GO TO 980
         IBUT = 4
         IL = 2
         GO TO 70
         END IF
      IBO = 1
C                                       Init BLC of new box
 40   IL = 1
      IPASS = 0
      IF (MBOX.GT.1) THEN
         WRITE (MSGTXT,1040) NBOX
         CALL MSGWRT (1)
         END IF
C                                       Circle possible
      IF (MBOX.GT.1) THEN
         IF (CIRCLE(IBO)) THEN
            MSGTXT = 'Set center: button A or B to change radius'
         ELSE
            MSGTXT = 'Set B.L.C. : button A or B to change to T.R.C.'
            END IF
         CALL MSGWRT (1)
         MSGTXT = 'Button C to switch between circle and box, ' //
     *      'D to kill and exit'
C                                       new circle not allowed
      ELSE
         IF (CIRCLE(IBO)) THEN
            MSGTXT = 'Set center: button A, B, or C to change radius'
         ELSE
            MSGTXT = 'Set B.L.C. : button A, B, or C to change to' //
     *         ' T.R.C.'
            END IF
         CALL MSGWRT (1)
         MSGTXT = 'Button D to kill and exit'
         END IF
      CALL MSGWRT (1)
      RESET = F
      IF (CIRCLE(IBO)) THEN
         IC(1,IBO) = (TVAREA(1) + TVAREA(3)) / 2
         IC(2,IBO) = (TVAREA(2) + TVAREA(4)) / 2
         IR(1,IBO) = 1
         IR(2,IBO) = 1
         RPOS(1) = IC(1,IBO)
         RPOS(2) = IC(2,IBO)
      ELSE
         IB(1,IBO) = (TVAREA(1) + TVAREA(3)) / 2
         IB(2,IBO) = (TVAREA(2) + TVAREA(4)) / 2
         IT(1,IBO) = IB(1,IBO) + 10
         IT(2,IBO) = IB(2,IBO) + 10
         RPOS(1) = IB(1,IBO)
         RPOS(2) = IB(2,IBO)
         END IF
C                                       No scroll correction
C      QUAD = -1
C                                       ON cursor at desired position
 45   PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
      IPOS = 31
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
      IF ((IERR.NE.0) .AND. (RESET)) THEN
         MSGTXT = 'UNABLE TO REACH REQUESTED CORNER: RESET ' //
     *      'ANOTHER CORNER'
         CALL MSGWRT (7)
         GO TO 100
         END IF
      IF (IERR.EQ.2) CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Cursor read loop
 50   CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IERR)
         IPOS = 4
         IF (IERR.NE.0) GO TO 980
C                                       keep in bounds
         CALL LMBOXS (TVAREA, CIRCLE(IBO), IL, IB(1,IBO), IT(1,IBO),
     *      RPOS, LBUT, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (IBUT.EQ.0) IBUT = LBUT
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF (.NOT.DOIT) GO TO 50
C                                       Erase current box
         CALL YHOLD ('ONNN', IERR)
         JL = -IL
         CALL DRBOXS (ICH, CATBLK(IICOR), CIRCLE(IBO), JL,
     *      IB(1,IBO), IT(1,IBO), SCRTCH, IERR)
         IPOS = 5
         IF (IERR.GT.0) GO TO 980
         CALL YHOLD ('OFFF', IERR)
C                                       New corners
         IF (IL.NE.2) THEN
C                                       center
            IF (CIRCLE(IBO)) THEN
               IC(1,IBO) = NINT (RPOS(1))
               IC(2,IBO) = NINT (RPOS(2))
C                                       BLC
            ELSE
               IB(1,IBO) = NINT (RPOS(1))
               IB(2,IBO) = NINT (RPOS(2))
               END IF
C                                       top: regular boxes
         ELSE
C                                       radius
            IF (CIRCLE(IBO)) THEN
               SR = SQRT (((RPOS(1)-IC(1,IBO))/SX)**2 +
     *            ((RPOS(2)-IC(2,IBO))/SY)**2)
               IR(1,IBO) = NINT (SR * SX)
               IR(1,IBO) = MAX (1, IR(1,IBO))
               IR(2,IBO) = NINT (SR * SY)
               IR(2,IBO) = MAX (1, IR(2,IBO))
C                                       TRC
            ELSE
               IT(1,IBO) = NINT (RPOS(1))
               IT(2,IBO) = NINT (RPOS(2))
               END IF
            END IF
C                                       Respond to buttons
         NXTBOX = .FALSE.
         IF (IBUT.GT.0) THEN
            IPASS = 0
            IF ((IL.EQ.1) .OR. (IBUT.EQ.1) .OR. ((IBUT.EQ.2) .AND.
     *         (MBOX.LE.1))) THEN
C                                       switch to TRC all buttons
               IF (IL.EQ.1) THEN
                  IF (IBUT.GT.7) THEN
                     IF (IBO.EQ.NBOX) NBOX = NBOX - 1
                     NXTBOX = .TRUE.
                     GO TO 70
                  ELSE IF ((IBUT.GT.3) .AND. (MBOX.GT.1)) THEN
                     CIRCLE(IBO) = .NOT.CIRCLE(IBO)
                     GO TO 40
                     END IF
                  RESET = F
                  IL = 2
                  IF (MBOX.GT.1) THEN
                     MSGTXT = 'Set T.R.C. : button A to repeat B.L.C.'
                     IF (CIRCLE(IBO)) MSGTXT = 'Set radius: button'
     *                   // ' A to repeat center'
                     CALL MSGWRT (1)
                     MSGTXT = 'Button B for new box, button C for ' //
     *                 'previous box, D to exit'
                  ELSE
                     MSGTXT = 'Set T.R.C. : button A or B to repeat ' //
     *                  'B.L.C.'
                     IF (CIRCLE(IBO)) MSGTXT = 'Set radius: button'
     *                   // ' A or B to reset center'
                     CALL MSGWRT (1)
                     MSGTXT = 'Button C or D to exit'
                     END IF
                  IF (CIRCLE(IBO)) THEN
                     IR(2,IBO) = 10 * SY
                     IR(1,IBO) = 10 * SX
                     RPOS(2) = IC(2,IBO) + IR(2,IBO)
                     RPOS(1) = IC(1,IBO)
                  ELSE
                     RPOS(1) = RPOS(1) + 10.0 * SX
                     RPOS(2) = RPOS(2) + 10.0 * SY
                     END IF
                  RPOS(1) = MIN (TVAREA(3), RPOS(1))
                  RPOS(2) = MIN (TVAREA(4), RPOS(2))
C                                       switch to other corn, but A
               ELSE
                  RESET = T
                  IF (IL.EQ.2) THEN
                     IL = 3
                     IF (CIRCLE(IBO)) THEN
                        RPOS(1) = IC(1,IBO)
                        RPOS(2) = IC(2,IBO)
                        MSGTXT = 'Reset center: buttons as for radius'
                     ELSE
                        RPOS(1) = IB(1,IBO)
                        RPOS(2) = IB(2,IBO)
                        MSGTXT = 'Reset B.L.C. : buttons as for T.R.C.'
                        END IF
                  ELSE
                     IL = 2
                     IF (CIRCLE(IBO)) THEN
                        RPOS(1) = IC(1,IBO) + IR(1,IBO)
                        RPOS(2) = IC(2,IBO)
                        MSGTXT = 'Reset radius'
                     ELSE
                        RPOS(1) = IT(1,IBO)
                        RPOS(2) = IT(2,IBO)
                        MSGTXT = 'Reset T.R.C.'
                        END IF
                     END IF
                  END IF
               CALL MSGWRT (1)
            ELSE
               NXTBOX = .TRUE.
               END IF
            END IF
C                                       draw all boxes
 70      IERR = 0
         CALL YHOLD ('ONNN', IERR)
         NOFF = 0
         DO 75 I = 1,NBOX
            JL = 0
            IF (I.EQ.IBO) JL = IL
            IERR = 0
            IF ((MOD(IPASS,EVERYC).EQ.0) .OR. (I.EQ.IBO)) THEN
               CALL DRBOXS (ICH, CATBLK(IICOR), CIRCLE(I), JL, IB(1,I),
     *            IT(1,I), SCRTCH, IERR)
            ELSE IF ((.NOT.CIRCLE(I)) .AND. (MOD(IPASS,EVERYR).EQ.0))
     *         THEN
               CALL DRBOXS (ICH, CATBLK(IICOR), CIRCLE(I), JL, IB(1,I),
     *            IT(1,I), SCRTCH, IERR)
               END IF
            IPOS = 6 + 100*I
            IF (IERR.GT.0) GO TO 980
            ISONTV(I) = IERR
            IF (IERR.NE.0) NOFF = NOFF + 1
 75         CONTINUE
         CALL YHOLD ('OFFF', IERR)
         IBO = MAX (1, IBO)
         IF ((WARN) .AND. (NOFF.GT.0)) THEN
            WRITE (MSGTXT,1075) NOFF, NBOX
            CALL MSGWRT (6)
            MSGTXT = 'These boxes may not be changed'
            IF ((MBOX.GT.0) .AND. (INBOX.GT.0)) CALL MSGWRT (6)
            WARN = .FALSE.
            END IF
         IF (MBOX.LT.0) GO TO 999
         IF (IBUT.GT.7) GO TO 800
         IPASS = MOD (IPASS + 1, EVERYC)
C                                       same box
         IF (.NOT.NXTBOX) THEN
            IF (IBUT.EQ.0) GO TO 50
            GO TO 45
C                                       next box
         ELSE
            IF (LBO.GT.0) IBO = NBOX
            LBO = 0
            IF ((IBUT.GE.4) .AND. ((NBOX.GT.1) .OR. (INBOX.EQ.1)))
     *         GO TO 100
            NBOX = NBOX + 1
            IBO = NBOX
            IF (NBOX.GT.MBOX) GO TO 800
            GO TO 40
            END IF
C                                       some already set box
C                                       wait for indication which
 100  MSGTXT = '**********  Push C to go to a new box or'
      CALL MSGWRT (1)
      MSGTXT = 'or move cursor to a corner to be reset and then push'
      CALL MSGWRT (1)
      MSGTXT = 'button A or B to do resetting.  Push D to exit'
      CALL MSGWRT (1)
      CALL YCURSE ('READ', T, T, RPOS, QUAD, IBUT, IERR)
      IPOS = 7
      IF (IERR.NE.0) GO TO 980
      IF (IBUT.GT.7) GO TO 800
      IF (IBUT.GE.4) THEN
         NBOX = NBOX + 1
         IBO = NBOX
         IF (NBOX.GT.MBOX) GO TO 800
         GO TO 40
         END IF
C                                       Find nearest corner
      DLIM = 0.5
 105  DO 110 I = 1,NBOX
         IF (ISONTV(I).EQ.0) THEN
            IL = 2
            IF (CIRCLE(I)) THEN
               RADIUS = SQRT (((RPOS(1)-IC(1,I))*SY/SX)**2 +
     *            (RPOS(2)-IC(2,I))**2)
               IF (ABS(RADIUS-IR(2,I)).LE.DLIM) GO TO 120
            ELSE
               IF ((ABS(RPOS(1)-IT(1,I)).LE.DLIM) .AND. (ABS(RPOS(2)
     *            -IT(2,I)).LE.DLIM)) GO TO 120
               END IF
            IL = 3
            IF (CIRCLE(I)) THEN
               IF ((ABS(RPOS(1)-IC(1,I)).LE.2*DLIM) .AND.
     *            (ABS(RPOS(2)-IC(2,I)).LE.2*DLIM)) GO TO 120
            ELSE
               IF ((ABS(RPOS(1)-IB(1,I)).LE.DLIM) .AND. (ABS(RPOS(2)
     *            -IB(2,I)).LE.DLIM)) GO TO 120
               END IF
            END IF
 110     CONTINUE
      DLIM = DLIM + 1.5
      IF (DLIM.LE.4.0) GO TO 105
      GO TO 100
C                                       Got one
 120  LBO = NBOX
      IBO = I
      RESET = F
      IF (CIRCLE(IBO)) THEN
         IF (IL.EQ.2) THEN
            RPOS(1) = IC(1,IBO) + IR(1,IBO)
            RPOS(2) = IC(2,IBO)
            WRITE (MSGTXT,1120) 'radius', IBO
            CALL MSGWRT (1)
            MSGTXT = 'Push button A to switch to center of box'
         ELSE
            RPOS(1) = IC(1,IBO)
            RPOS(2) = IC(2,IBO)
            WRITE (MSGTXT,1120) 'center', IBO
            CALL MSGWRT (1)
            MSGTXT = 'Push button A to switch to radius of box'
            END IF
      ELSE
         IF (IL.EQ.2) THEN
            RPOS(1) = IT(1,IBO)
            RPOS(2) = IT(2,IBO)
            WRITE (MSGTXT,1120) 'T.R.C.', IBO
            CALL MSGWRT (1)
            MSGTXT = 'Push button A to switch to B.L.C. of box'
         ELSE
            RPOS(1) = IB(1,IBO)
            RPOS(2) = IB(2,IBO)
            WRITE (MSGTXT,1120) 'B.L.C.', IBO
            CALL MSGWRT (1)
            MSGTXT = 'Push button A to switch to T.R.C. of box'
            END IF
         END IF
      CALL MSGWRT (1)
      MSGTXT = 'Button B to set new box, button C to reset ' //
     *   'old box, D to exit'
      CALL MSGWRT (1)
      GO TO 45
C                                       DONE: fill in real boxes
 800  NBOX = MAX (0, MIN (MBOX, NBOX))
      DO 840 I = 1,NBOX
C                                       force real BLC, TRC
         IF (.NOT.CIRCLE(I)) THEN
            IF (IB(1,I).GT.IT(1,I)) THEN
               JERR = IT(1,I)
               IT(1,I) = IB(1,I)
               IB(1,I) = JERR
               END IF
            IF (IB(2,I).GT.IT(2,I)) THEN
               JERR = IT(2,I)
               IT(2,I) = IB(2,I)
               IB(2,I) = JERR
               END IF
            END IF
C                                       circle
         IF (CIRCLE(I)) THEN
            BBLC(1,I) = -1.0
            BBLC(2,I) = SQRT (IR(1,I)*IR(2,I)/SX/SY)
            BTRC(1,I) = (IC(1,I) - TVAREA(1)) / SX + IMWIND(1)
            BTRC(2,I) = (IC(2,I) - TVAREA(2)) / SY + IMWIND(2)
C                                       rectangle
         ELSE
            BBLC(1,I) = (IB(1,I) - TVAREA(1)) / SX + IMWIND(1)
            BBLC(2,I) = (IB(2,I) - TVAREA(2)) / SY + IMWIND(2)
            BTRC(1,I) = (IT(1,I) - TVAREA(1)) / SX + IMWIND(1)
            BTRC(2,I) = (IT(2,I) - TVAREA(2)) / SY + IMWIND(2)
            END IF
         CALL RCOPY (5, RDEP, BBLC(3,I))
         CALL RCOPY (5, RDEP, BTRC(3,I))
 840     CONTINUE
      NBOX = MIN (NBOX, MBOX)
      IERR = 0
C                                       Off cursor, graphics, scroll
C                                       leave graphics on
 980  CALL YCURSE ('OFFF', F, T, RPOS, QUAD, IBUT, JERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1980) IERR, IPOS
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('==========  Setting box number',I3)
 1075 FORMAT ('Warning:',I4,' of',I4,' boxes not fully on displayed',
     *   ' image')
 1120 FORMAT ('----------  Reset ',A,' of box',I3)
 1980 FORMAT ('GRBOXS: ERROR CODE',I7,' AT',I5)
      END
