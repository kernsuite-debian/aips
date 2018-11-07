      SUBROUTINE GRBOXD (IG, NBOX, BBLC, BTRC, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! takes list of boxes and does deletion
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2006, 2009
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
C      IG      I(2)     graphics planes to use - 2nd background
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
C-----------------------------------------------------------------------
      INTEGER   IG(2), NBOX, SCRTCH(*), IERR
      REAL      BBLC(7,*), BTRC(7,*)
C
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   ICH1, ICH2, IL, QUAD, IBUT, J, I, JERR, IPOS, ITEMP,
     *   SCROLX, SCROLY, INBOX, IC(2,MXNBOX), IR(2,MXNBOX), NMISS,
     *   IB(2,MXNBOX), IT(2,MXNBOX), JL, IPASS, ISONTV(MXNBOX), NOFF
      REAL      RPOS(2), DLIM, IMWIND(4), TVAREA(4), RDEP(5), RADIUS,
     *   SX, SY
      LOGICAL   T, F, RESET, CIRCLE(MXNBOX), NXTBOX, WARN
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
      ICH1 = NGRAY + IG(1)
      ICH2 = NGRAY + IG(2)
C                                       Check inputs
      IERR = 2
      IF ((IG(1).LT.1) .OR. (IG(1).GT.NGRAPH)) GO TO 999
      IF ((IG(2).LT.1) .OR. (IG(2).GT.NGRAPH)) GO TO 999
      IF (NBOX.LE.0) GO TO 999
      RESET = F
      INBOX = NBOX
      IF (NBOX.GT.MXNBOX) GO TO 999
      CALL FILL (NBOX, 0, ISONTV)
      CALL FILL (NBOX, .FALSE., CIRCLE)
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
      CALL YHOLD ('ONNN', IERR)
      CALL YZERO (ICH1, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL YZERO (ICH2, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       turn on graphics to be certain
      CALL YSLECT ('ONNN', ICH1, 0, SCRTCH, IERR)
      IPOS = 1
      IF (IERR.NE.0) GO TO 980
      CALL YSLECT ('ONNN', ICH2, 0, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       no scroll on graphics now
 10   ITEMP = 2 ** NGRAY
      SCROLX = TVSCRX(1)
      SCROLY = TVSCRY(1)
      CALL YSCROL (ITEMP, SCROLX, SCROLY, T, IERR)
      IPOS = 2
      IF (IERR.NE.0) GO TO 980
      CALL YHOLD ('OFFF', IERR)
C                                       Init values provided
      IERR = 2
      SX = (TVAREA(3) - TVAREA(1)) / (IMWIND(3) - IMWIND(1))
      SY = (TVAREA(4) - TVAREA(2)) / (IMWIND(4) - IMWIND(2))
C                                       Convert image to TV pixels
      DO 20 I = 1,INBOX
         CIRCLE(I) = (BBLC(1,I).GT.-1.01) .AND. (BBLC(1,I).LT.-0.99)
         IF (CIRCLE(I)) THEN
            IR(1,I) = NINT (BBLC(2,I) * SX)
            IR(1,I) = MAX (1, IR(1,I))
            IR(2,I) = NINT (BBLC(2,I) * SY)
            IR(2,I) = MAX (1, IR(2,I))
            IC(1,I) = NINT (TVAREA(1) + (BTRC(1,I)-IMWIND(1)) * SX)
            IC(2,I) = NINT (TVAREA(2) + (BTRC(2,I)-IMWIND(2)) * SY)
         ELSE
            IB(1,I) = NINT (TVAREA(1) + (BBLC(1,I) - IMWIND(1)) * SX)
            IT(1,I) = NINT (TVAREA(1) + (BTRC(1,I) - IMWIND(1)) * SX)
            IB(2,I) = NINT (TVAREA(2) + (BBLC(2,I) - IMWIND(2)) * SY)
            IT(2,I) = NINT (TVAREA(2) + (BTRC(2,I) - IMWIND(2)) * SY)
            END IF
 20      CONTINUE
      IERR = 0
      NXTBOX = .TRUE.
      IPASS = 0
C      QUAD = -1
      IF (CIRCLE(NBOX)) THEN
         RPOS(1) = IC(1,NBOX)
         RPOS(2) = IC(2,NBOX)
      ELSE
         RPOS(1) = IB(1,NBOX)
         RPOS(2) = IB(2,NBOX)
         END IF
      IPOS = 3
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
 25         CONTINUE
         END IF
 30   IF (IERR.NE.0) GO TO 980
      IBUT = 4
      IL = 2
C                                       draw all boxes
 40   IERR = 0
      CALL YHOLD ('ONNN', IERR)
      NOFF = 0
      NMISS = 0
      JL = 0
      DO 45 I = 1,NBOX
         IERR = 0
         CALL DRBOXS (ICH1, CATBLK(IICOR), CIRCLE(I), JL, IB(1,I),
     *      IT(1,I), SCRTCH, IERR)
         IPOS = 100 + I
         IF (IERR.GT.0) GO TO 980
         ISONTV(I) = IERR
         IF (IERR.EQ.-1) NOFF = NOFF + 1
         IF (IERR.EQ.-2) NMISS = NMISS + 1
 45      CONTINUE
      CALL YHOLD ('OFFF', IERR)
      IF ((WARN) .AND. (NOFF+NMISS.GT.0)) THEN
         IF (NOFF.GT.0) THEN
            WRITE (MSGTXT,1045) NOFF, NBOX
            CALL MSGWRT (6)
            MSGTXT = 'These boxes may be deleted'
            CALL MSGWRT (6)
            END IF
         IF (NMISS.GT.0) THEN
            WRITE (MSGTXT,1046) NMISS, NBOX
            CALL MSGWRT (6)
            MSGTXT = 'These boxes may NOT be deleted'
            CALL MSGWRT (6)
            END IF
         WARN = .FALSE.
         END IF
      IF ((IBUT.GT.7) .OR. (NMISS.GE.NBOX)) GO TO 800
C                                       Find a box
 50   MSGTXT = 'Select box with button A or B'
      CALL MSGWRT (1)
      MSGTXT = 'Exit with button C or D'
      CALL MSGWRT (1)
      CALL YCURSE ('READ', T, T, RPOS, QUAD, IBUT, IERR)
      IPOS = 4
      IF (IERR.NE.0) GO TO 980
      IF (IBUT.GT.3) GO TO 800
C                                       Find nearest corner
      DO 60 J = 1,3
         DLIM = J * 1.5 - 1.0
         DO 55 I = 1,NBOX
            IF (ISONTV(I).GT.-2) THEN
               IL = 2
               IF (CIRCLE(I)) THEN
                  RADIUS = SQRT (((RPOS(1)-IC(1,I))*SY/SX)**2 +
     *               (RPOS(2)-IC(2,I))**2)
                  IF (ABS(RADIUS-IR(2,I)).LE.DLIM) GO TO 70
               ELSE
                  IF ((ABS(RPOS(1)-IT(1,I)).LE.DLIM) .AND.
     *               (ABS(RPOS(2)-IT(2,I)).LE.DLIM)) GO TO 70
                  END IF
               IL = 3
               IF (CIRCLE(I)) THEN
                  IF ((ABS(RPOS(1)-IC(1,I)).LE.2*DLIM) .AND.
     *               (ABS(RPOS(2)-IC(2,I)).LE.2*DLIM)) GO TO 70
               ELSE
                  IF ((ABS(RPOS(1)-IB(1,I)).LE.DLIM) .AND.
     *               (ABS(RPOS(2)-IB(2,I)).LE.DLIM)) GO TO 70
                  END IF
               END IF
 55         CONTINUE
 60      CONTINUE
      GO TO 50
C                                       found one
 70   CALL DRBOXS (ICH1, CATBLK(IICOR), CIRCLE(I), -3, IB(1,I),
     *   IT(1,I), SCRTCH, IERR)
      CALL DRBOXS (ICH2, CATBLK(IICOR), CIRCLE(I), JL, IB(1,I),
     *   IT(1,I), SCRTCH, IERR)
      IPOS = 5
      IF (IERR.GT.0) GO TO 980
      MSGTXT = 'Hit button A to delete this box'
      CALL MSGWRT (1)
      MSGTXT = 'Hit buttons B or C to select a different box'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button D to exit'
      CALL MSGWRT (1)
C                                       read for button
      CALL YCURSE ('READ', T, T, RPOS, QUAD, IBUT, IERR)
      IPOS = 6
      IF (IERR.NE.0) GO TO 980
C                                       erase highlight
      CALL DRBOXS (ICH2, CATBLK(IICOR), CIRCLE(I), -3, IB(1,I),
     *   IT(1,I), SCRTCH, IERR)
      IPOS = 7
      IF (IERR.GT.0) GO TO 980
C                                       restore if not flag
      IF (IBUT.GT.1) THEN
         CALL DRBOXS (ICH1, CATBLK(IICOR), CIRCLE(I), JL, IB(1,I),
     *      IT(1,I), SCRTCH, IERR)
         IPOS = 8
         IF (IERR.GT.0) GO TO 980
         IF (IBUT.GT.7) GO TO 800
         GO TO 50
         END IF
C                                       drop this box
      DO 80 J = I+1,NBOX
         CIRCLE(J-1) = CIRCLE(J)
         IB(1,J-1) = IB(1,J)
         IB(2,J-1) = IB(2,J)
         IT(1,J-1) = IT(1,J)
         IT(2,J-1) = IT(2,J)
 80      CONTINUE
      NBOX = NBOX - 1
      IF (NBOX.LE.0) GO TO 800
      GO TO 40
C                                       DONE: fill in real boxes
 800  NBOX = MAX (0, MIN (INBOX, NBOX))
      DO 810 I = 1,NBOX
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
 810     CONTINUE
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
 1045 FORMAT ('Warning:',I4,' of',I4,' boxes not fully on displayed',
     *   ' image')
 1046 FORMAT ('Warning:',I4,' of',I4,' boxes fully off displayed',
     *   ' image')
 1980 FORMAT ('GRBOXD: ERROR CODE',I7,' AT',I5)
      END
