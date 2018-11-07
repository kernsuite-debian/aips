      SUBROUTINE TVMENU (TYPE, NCOL, NROWS, GRCHS, TOPSEP, SIDSEP,
     *   ISHELP, CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHOICE, TVBUTT,
     *   SCRTCH, IERR)
C-----------------------------------------------------------------------
C! displays a menu, reads the user choise with time limit
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999-2000, 2009, 2013-2015
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
C   Displays a menu on a graphics channel, interactively highlights the
C   currently selected item, provides help, returns if item selected or
C   time limit exceeded.
C   Inputs:
C      TYPE     I      How to arrange columns: 0 left justified,
C                         1 right justified, 2 centered,
C                        -1 split to edges (NCOL=2)
C      NCOL     I      Number of columns
C      NROWS    I(*)   Number rows in each column
C      GRCHS    I(2)   Graphics channels to use for (1) menu (default 1)
C                         (2) highlight (default omit)
C                         If GRCHS(1) < 0, use abs(GRCHS(1)) w no init
C                         i.e. this menu is already written there
C      TOPSEP   I      > 0 => separate menu from top by TOPSEP pixels
C      SIDSEP   I      > 0 => separate left edge (type 0) in pixels
C                             separate right edge (type 1) in pixels
C      CHOICS   C(*)*?  Strings of menu items
C      TIMLIM   I      Return after TIMLIM seconds even if no button
C                         0 => infinite time limit
C      LEAVE    L(*)   T => leave the 2 graphics planes on, subscriot
C                      same as selected CHOICS.  Do not zoom.
C      NTITLE   I      Number of lines in TITLE  0 -> none
C      TITLE    C(*)*(*)  TitleS to menu ' ' => none
C   In/out:
C      ISHELP   C*6    Name of interactive help file = 'HLPishelp.HLP'
C                      ' ' -> none, set to ' ' if an error occurs
C   Outputs:
C      CHOICE   I      Chosen item from the list
C      TVBUTT   I      Selected button: 0 => timed out
C      SCRTCH   I(*)   buffer
C      IERR     I      Error code: 2 input error
C   Limits: 10 columns, 40 rows/column
C-----------------------------------------------------------------------
      INTEGER   TYPE, NCOL, NROWS(*), GRCHS(2), TOPSEP, SIDSEP, TIMLIM,
     *   CHOICE, NTITLE, TVBUTT, SCRTCH(*), IERR
      CHARACTER CHOICS(*)*(*), ISHELP*(*), TITLE(*)*(*)
      LOGICAL   LEAVE(*)
C
      INTEGER   LMAX
      PARAMETER (LMAX = 100)
      INTEGER   IX, IY, QUAD, IBUT, ITW(3), CTW(3), GR1, GR2, IXC(2,10),
     *   NCH(LMAX,10), NCHM(10), IYC(LMAX), RMAX, MCH(2), PROW, PCOL,
     *   NEDGE, IS, TIME0, TIMEC, LTIME, TIMOFF, SVZOOM(3), ISC(10), I,
     *   J, MWIN(4), LX, LY, IXP(5), IYP(5), ICOL, IROW, LCOL, LROW, NC,
     *   MENU0, JTRIM, NROW, IROUND, JERR, MSWIN(4), MSCHAR, NNCHAR(10),
     *   LTITLE, SIDS, IYD, IY0
      REAL      PPOS(2), RPOS(2)
      LOGICAL   DOMENU, DOTITL, DOIT, F, TIMED
      CHARACTER ROUTIN*6, PS*128
      SAVE MSWIN, MSCHAR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA F /.FALSE./
      DATA MSWIN, MSCHAR /4*0, 0/
C-----------------------------------------------------------------------
      IERR = 2
      IF ((NCOL.LE.0) .OR. (NCOL.GT.10)) GO TO 999
      SIDS = MAX (0, SIDSEP)
      IF (TYPE.EQ.2) SIDS = 0
      CALL COPY (3, TVZOOM, SVZOOM)
      IYD = 1.5*CSIZTV(2) + 0.5
      TIMED = .FALSE.
      LTITLE = MAX (0, NTITLE)
      DO 5 I = LTITLE,1,-1
         NNCHAR(I) = JTRIM (TITLE(I))
         IF (TITLE(I).EQ.' ') NNCHAR(I) = 0
         IF ((I.EQ.LTITLE) .AND. (NNCHAR(I).LE.0)) LTITLE = LTITLE - 1
 5       CONTINUE
C                                       channels
      GR1 = ABS (GRCHS(1))
      IF ((GR1.LT.1) .OR. (GR1.GT.NGRAPH)) GR1 = 1
      GR2 = ABS (GRCHS(2))
      GR2 = MIN (GR2, NGRAPH)
      IF (GR2.EQ.GR1) GR2 = 0
      GR1 = GR1 + NGRAY
      GR2 = GR2 + NGRAY
      TVBUTT = 0
      CHOICE = 0
C                                       plan menu
      IS = 1
      NC = 0
      RMAX = 0
      IF ((NNCHAR(1).GT.0) .OR. (MSCHAR.GT.0)) RMAX = 2
      DO 20 I = 1,NCOL
         IX = NROWS(I)
         IF ((IX.LE.0) .OR. (IX.GT.LMAX)) GO TO 999
         NCHM(I) = 1
         RMAX = MAX (RMAX, IX)
         ISC(I) = IS - 1
         DO 10 J = 1,IX
            NCH(J,I) = JTRIM (CHOICS(IS))
            IF (CHOICS(IS).EQ.' ') NCH(J,I) = 1
            NCHM(I) = MAX (NCHM(I), NCH(J,I))
            IS = IS + 1
 10         CONTINUE
         NC = NC + NCHM(I)
 20      CONTINUE
      NEDGE = MAX (2, (CSIZTV(1)+1)/2)
 25   MCH(1) = 2 * (2 + NEDGE + (NCOL-1) * (1+NEDGE)) + CSIZTV(1) * NC
      IF (MCH(1).GT.MAXXTV(1)) THEN
         NEDGE = NEDGE - 1
         IF (NEDGE.GT.0) GO TO 25
            MSGTXT = 'TV SCREEN TOO NARROW.  BUY A BIGGER ONE'
            CALL MSGWRT (9)
            IERR = 8
            GO TO 999
         END IF
C                                       init
      PROW = 0
      PCOL = 0
C                                       offzoom
      TVZOOM(1) = 0
      TVZOOM(2) = MAXXTV(1)/2
      TVZOOM(3) = MAXXTV(2)/2
      IF ((SVZOOM(1).NE.TVZOOM(1)) .OR. (SVZOOM(2).NE.TVZOOM(2)) .OR.
     *   (SVZOOM(3).NE.TVZOOM(3))) THEN
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .FALSE., IERR)
         ROUTIN = 'YZOOMC'
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       check window of TV
      DOMENU = GRCHS(1).GE.0
      DOTITL = GRCHS(2).GE.0
      CALL YWINDO ('READ', WINDTV, IERR)
      ROUTIN = 'YWINDO'
      IF (IERR.NE.0) GO TO 990
      IF ((WINDTV(1).NE.MSWIN(1)) .OR. (WINDTV(2).NE.MSWIN(2)) .OR.
     *   (WINDTV(3).NE.MSWIN(3)) .OR. (WINDTV(4).NE.MSWIN(4))) THEN
         DOMENU = .TRUE.
         DOTITL = .TRUE.
         END IF
      IF (MSCHAR.GT.NNCHAR(1)) THEN
         DOTITL = .TRUE.
         IF (GR2.LE.0) DOMENU = .TRUE.
         END IF
      CALL COPY (4, WINDTV, MSWIN)
C                                       clear, init, select
      CALL YHOLD ('ONNN', I)
      IF (DOMENU) THEN
         CALL YZERO (GR1, IERR)
         ROUTIN = 'YZERO'
         IF (IERR.NE.0) GO TO 900
         END IF
      CALL YSLECT ('ONNN', GR1, 0, SCRTCH, IERR)
      ROUTIN = 'YSLECT'
      IF (IERR.NE.0) GO TO 900
      IF (GR2.GT.0) THEN
         IF (DOTITL) THEN
            CALL YZERO (GR2, IERR)
            ROUTIN = 'YZERO'
            IF (IERR.NE.0) GO TO 900
            END IF
         CALL YSLECT ('ONNN', GR2, 0, SCRTCH, IERR)
         ROUTIN = 'YSLECT'
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       plan menu
      IF (MCH(1).LT.MSWIN(3)-MSWIN(1)) THEN
         MWIN(1) = MSWIN(1)
         MWIN(3) = MSWIN(3)
      ELSE
         MWIN(1) = (MAXXTV(1) - MCH(1)) / 2 + 1
         MWIN(3) = MWIN(1) + MCH(1) - 1
         END IF
      MCH(2) = MAX (0, TOPSEP) + IYD * (LTITLE + RMAX) + 2*NEDGE
      IF (MCH(2).LT.MSWIN(4)-MSWIN(2)) THEN
         MWIN(2) = MSWIN(2)
         MWIN(4) = MSWIN(4)
      ELSE
         MWIN(2) = (MAXXTV(2) - MCH(2)) / 2 + 1
         MWIN(4) = MWIN(2) + MCH(2) - 1
         END IF
      IF ((MWIN(1).LT.1) .OR. (MWIN(2).LT.1) .OR. (MWIN(3).GT.MAXXTV(1))
     *   .OR. (MWIN(4).GT.MAXXTV(2))) THEN
         MSGTXT = 'MENU IS TOO LARGE FOR THE TV SCREEN'
         IERR = 8
         CALL YHOLD ('OFFF', I)
         CALL MSGWRT (8)
         GO TO 999
         END IF
      MENU0 = MWIN(1) + 4 + SIDS
      IF (TYPE.EQ.1) MENU0 = MWIN(3) - MCH(1) - SIDS
      IF (TYPE.EQ.2) MENU0 = (MWIN(3) - MWIN(1) + 1 - MCH(1)) / 2 + 1
C                                       write menu if needed, get
C                                       points
      ROUTIN = 'IMCHAR'
      IX = 3 + NEDGE + MENU0
      IY = MWIN(4) - IYD - MAX (0, TOPSEP)
      IF (LTITLE.GT.0) THEN
         DO 27 I = 1,LTITLE
            IF (GR2.GT.0) THEN
               CALL IMCHAR (GR2, IX, IY, 0, 0, TITLE(I)(:NNCHAR(I)),
     *            SCRTCH, IERR)
            ELSE
               CALL IMCHAR (GR1, IX, IY, 0, 0, TITLE(I)(:NNCHAR(I)),
     *            SCRTCH, IERR)
               END IF
            IF (IERR.NE.0) GO TO 900
            IY = IY - IYD
 27         CONTINUE
         END IF
      IY = IY - 2*NEDGE
      IY0 = IY
      MSCHAR = NNCHAR(1)
      IS = 1
      DO 35 LCOL = 1,NCOL
         NROW = NROWS(LCOL)
         IXC(1,LCOL) = IX
         IXC(2,LCOL) = IX + NCHM(LCOL)*CSIZTV(1) - 1
         IY = IY0
         DO 30 LROW = 1,NROW
            IYC(LROW) = IY
            IF (DOMENU) THEN
               NC = NCH(LROW,LCOL)
               PS = CHOICS(IS)(1:NC)
               CALL IMCHAR (GR1, IX, IY, 0, 0, PS(1:NC), SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               IS = IS + 1
               END IF
            IY = IY - IYD
 30         CONTINUE
         IF ((LCOL.EQ.1) .AND. (TYPE.LT.0) .AND. (NCOL.EQ.2)) THEN
            IX = MWIN(3) - NCHM(2)*CSIZTV(1) - 2 - NEDGE - SIDS
         ELSE
            IX = IX + 2 + 2*NEDGE + NCHM(LCOL)*CSIZTV(1)
            END IF
 35      CONTINUE
C                                       write border lines
      IF (DOMENU) THEN
         RPOS(1) = MAXXTV(1) / 2
         RPOS(2) = MAXXTV(2) / 2
         IY = IY0 + IYD
         DO 40 LCOL = 1,NCOL
            IX = IXC(1,LCOL) - 2 - NEDGE
            LX = 4 + 2*NEDGE + NCHM(LCOL)*CSIZTV(1)
            LY = NROWS(LCOL) * IYD + (CSIZTV(2)+1)/2
            IXP(1) = IX
            IYP(1) = IY
            IXP(2) = IX + LX - 1
            IYP(2) = IYP(1)
            IXP(3) = IXP(2)
            IYP(3) = IY - LY + 1
            IXP(4) = IXP(1)
            IYP(4) = IYP(3)
            IXP(5) = IXP(1)
            IYP(5) = IYP(1)
            CALL IMVECT ('ONNN', GR1, 5, IXP, IYP, SCRTCH, IERR)
            ROUTIN = 'IMVECT'
            IF (IERR.NE.0) GO TO 900
            IX = IX + 1
            IY = IY - 1
            LX = LX - 2
            LY = LY - 2
            IXP(1) = IX
            IYP(1) = IY
            IXP(2) = IX + LX - 1
            IYP(2) = IYP(1)
            IXP(3) = IXP(2)
            IYP(3) = IY - LY + 1
            IXP(4) = IXP(1)
            IYP(4) = IYP(3)
            IXP(5) = IXP(1)
            IYP(5) = IYP(1)
            CALL IMVECT ('ONNN', GR1, 5, IXP, IYP, SCRTCH, IERR)
            ROUTIN = 'IMVECT'
            IF (IERR.NE.0) GO TO 900
            IX = IX + LX - 1
            IY = IY + 1
 40         CONTINUE
         END IF
      CALL YHOLD ('OFFF', I)
      CALL ZTIME (ITW)
      TIME0 = ITW(3) + 60 * (ITW(2) + 24*ITW(1))
      LTIME = TIME0
      TIMOFF = 0
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      IF (TIMLIM.GT.0) THEN
         WRITE (MSGTXT,1040) TIMLIM
         CALL MSGWRT (1)
         END IF
      IF (ISHELP.NE. ' ') THEN
         MSGTXT = 'Press buttons A, B, or C to choose an operation'
         CALL MSGWRT (1)
         MSGTXT = 'Press button D for on-line help'
         CALL MSGWRT (1)
      ELSE
         MSGTXT = 'Press ANY button to choose an operation'
         CALL MSGWRT (1)
         END IF
C                                       read cursor
 50   CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
      ROUTIN = 'YCURSE'
      IF (IERR.NE.0) GO TO 800
C                                       check time out
      CALL ZTIME (CTW)
      TIMEC = CTW(3) + 60 * (CTW(2) + 24*CTW(1)) + TIMOFF
      IF (TIMEC-LTIME.LT.-1) THEN
         TIMOFF = TIMOFF + 86400
         TIMEC = TIMEC + 86400
         END IF
      LTIME = TIMEC
      IF ((TIMLIM.GT.0) .AND. (LTIME-TIME0.GT.TIMLIM)) THEN
         TIMED = .TRUE.
         GO TO 800
         END IF
C                                       did something happen
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (.NOT.DOIT) GO TO 50
C                                       find the choice
         IX = IROUND (RPOS(1))
         IY = IROUND (RPOS(2))
         ICOL = 0
         DO 55 LCOL = 1,NCOL
            IF ((IX.GE.IXC(1,LCOL)-2) .AND. (IX.LE.IXC(2,LCOL)+2))
     *         ICOL = LCOL
 55         CONTINUE
         IF (ICOL.LE.0) GO TO 50
         IROW = 0
         NROW = NROWS(ICOL)
         DO 60 LROW = 1,NROW
            IF (IY.GT.IYC(NROW+1-LROW)-2) IROW = NROW + 1 - LROW
 60         CONTINUE
         IF (IROW.EQ.0) GO TO 50
         IF (NCH(IROW,ICOL).EQ.0) GO TO 50
         IF (GR2.GT.0) THEN
            IF ((PCOL.NE.ICOL) .OR. (PROW.NE.IROW)) THEN
C                                       restore choice
               ROUTIN = 'IMCHAR'
               IF ((PCOL.GT.0) .AND. (PROW.GT.0)) THEN
                  CALL YHOLD ('ONNN', I)
                  NC = NCH(PROW,PCOL)
                  PS(1:NC) = ' '
                  CALL IMCHAR (GR2, IXC(1,PCOL), IYC(PROW), 0, 0,
     *               PS(1:NC), SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 800
                  PS(1:NC) = CHOICS(PROW+ISC(PCOL))(1:NC)
                  CALL IMCHAR (GR1, IXC(1,PCOL), IYC(PROW), 0, 0,
     *               PS(1:NC), SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 800
                  CALL YHOLD ('OFFF', I)
                  END IF
C                                       highlight choice
               NC = NCH(IROW,ICOL)
               PS(1:NC) = CHOICS(IROW+ISC(ICOL))(1:NC)
               CALL IMCHAR (GR2, IXC(1,ICOL), IYC(IROW), 0, 0, PS(1:NC),
     *            SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 800
               PCOL = ICOL
               PROW = IROW
               END IF
            END IF
C                                       help if D and can
         IF ((IBUT.GE.8) .AND. (ISHELP.NE.' ')) THEN
            NC = NCH(IROW,ICOL)
            PS = CHOICS(IROW+ISC(ICOL))(1:NC)
            JERR = 0
            IF (PS.NE.' ') CALL TSKHLP (PS, NC, ISHELP, JERR)
            IBUT = 0
            IF (JERR.LE.0) THEN
               TIME0 = LTIME
            ELSE
               ISHELP = ' '
               END IF
C                                       leave on button A, B, C
         ELSE IF (IBUT.GT.0) THEN
            TVBUTT = IBUT
            CHOICE = PROW + ISC(PCOL)
            GO TO 800
            END IF
         GO TO 50
C                                       turn off the cursor
 800  CALL YCURSE ('OFFF', F, F, RPOS, QUAD, IBUT, JERR)
C                                       turn off the choices
 900  CALL YHOLD ('ONNN', I)
      IF ((.NOT.LEAVE(CHOICE)) .OR. (TIMED)) THEN
         CALL YSLECT ('OFFF', GR1, 0, SCRTCH, JERR)
         IF (GR2.GT.0) CALL YSLECT ('OFFF', GR2, 0, SCRTCH, JERR)
         END IF
C                                       fix menu
      IF ((GR2.GT.0).AND. (PCOL.GT.0) .AND. (PROW.GT.0)) THEN
         ROUTIN = 'IMCHAR'
         NC = NCH(PROW,PCOL)
         PS(1:NC) = ' '
         CALL IMCHAR (GR2, IXC(1,PCOL), IYC(PROW), 0, 0, PS(1:NC),
     *      SCRTCH, JERR)
         PS(1:NC) = CHOICS(PROW+ISC(PCOL))(1:NC)
         CALL IMCHAR (GR1, IXC(1,PCOL), IYC(PROW), 0, 0, PS(1:NC),
     *      SCRTCH, JERR)
         END IF
      IF (IERR.EQ.0) THEN
         CALL COPY (3, SVZOOM, TVZOOM)
         IF (.NOT.LEAVE(CHOICE)) CALL YZOOMC (TVZOOM(1), TVZOOM(2),
     *      TVZOOM(3), F, JERR)
         END IF
      IF (LTITLE.GT.0) THEN
         IX = 3 + NEDGE + MENU0
         IY = MWIN(4) - 12 - CSIZTV(2) - MAX (0, TOPSEP)
            PS = ' '
         DO 910 I = 1,LTITLE
            NC = JTRIM (TITLE(I))
            IF (GR2.GT.0) THEN
               CALL IMCHAR (GR1, IX, IY, 0, 0, PS(:NC), SCRTCH, JERR)
            ELSE
               CALL IMCHAR (GR2, IX, IY, 0, 0, PS(:NC), SCRTCH, JERR)
               END IF
            IF (IERR.NE.0) GO TO 990
            IY = IY - IYD
 910        CONTINUE
         END IF
      CALL YHOLD ('OFFF', I)
C                                       force buffer to TV
      CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, JERR)
C
 990  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1990) IERR, ROUTIN
         CALL MSGWRT (6)
         CALL YHOLD ('OFFF', I)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('You have',I6,' seconds to select a menu item by:')
 1990 FORMAT ('TVMENU: TV I/O ERROR',I7,' FROM ',A)
      END
