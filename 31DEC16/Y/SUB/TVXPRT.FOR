      SUBROUTINE TVXPRT (TYPE, NCOL, NROWS, GRCHS, TOPSEP, CHOICS,
     *   LEAVE, TTY, N, CHOICX, SHORTX, ALPHAX, OPTOKX, CHOICE,
     *   SCRTCH, IERR)
C-----------------------------------------------------------------------
C! displays a menu, reads the user choice from terminal
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1999-2000, 2010
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
C   Displays a menu on a graphics channel, highlights the allowed 1-char
C   codes, provides help, reads 1 char selection from TTY, returns if
C   item selected.
C   Inputs:
C      TYPE     I         How to arrange columns: 0 left justified,
C                          1 right justified, 2 centered,
C                         -1 split to edges (NCOL=2)
C      NCOL     I         Number of columns
C      NROWS    I(*)      Number rows in each column
C      GRCHS    I(2)      Graphics channels to use for (1) menu (default
C                         1) (2) highlight (default omit)
C                          If GRCHS(1) < 0, use abs(GRCHS(1)) w no init
C                          i.e. this menu is already written there
C      TOPSEP   I          > 0 => separate menu from top by TOPSEP
C                          pixels
C      CHOICS   C(*)*?     Strings of menu items
C      LEAVE    L(*)       T => leave the 2 graphics planes on,
C                          subscript same as selected CHOICS
C      TTY      I(2)       TTY LUN and Find
C      N        I          Number of choices
C      CHOICX   C(N)*(*)   Long string list of choices
C      SHORTX   C(N)*4     Short string list of choices in the form of 1
C                          uppercase letter and 1-3 lowercase ones.  The
C                          single uppercase letter will be recognized.
C      ALPHAX   C(N)*1     List of upper case letters to recognize.
C                          Letters should not repeat and must not
C                          contain X (exit expert mode) or H (help).
C      OPTOKX   I(N)       Flag > 0 => option is currently allowed
C   Outputs:
C      CHOICE   C*(*)      Chosen item from the list
C      SCRTCH   I(*)       buffer
C      IERR     I          Error code: 2 input error
C   Limits: 10 columns, 40 rows/column
C-----------------------------------------------------------------------
      INTEGER   TYPE, NCOL, NROWS(*), GRCHS(2), TOPSEP, TTY(2), N,
     *   OPTOKX(N), SCRTCH(*), IERR
      CHARACTER CHOICS(*)*(*), CHOICX(N)*(*), SHORTX(N)*(*),
     *   ALPHAX(N)*1, CHOICE*(*)
      LOGICAL   LEAVE(*)
C
      INTEGER   IX, IY, QUAD, IBUT, K,  GR1, GR2, IXC(2,10), NCH(40,10),
     *   NCHM(10), IYC(40), RMAX, MCH(2), PROW, PCOL, NEDGE,  IS,
     *   SVZOOM(3), ISC(10), I, J, MWIN(4), LX, LY, IXP(5), IYP(5),
     *   LCOL, LROW, NC, MENU0, JTRIM, NROW, JERR, MSWIN(4)
      REAL      RPOS(2)
      LOGICAL   DOMENU, F
      CHARACTER ROUTIN*6, PS*128, PROMPT*72, CHRCTR*1, LSTCHR*1
      SAVE MSWIN, LSTCHR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA F /.FALSE./
      DATA MSWIN, LSTCHR /4*0, 'X'/
C-----------------------------------------------------------------------
C                                       empty list
      IF (N.LE.0) THEN
         CHOICE = 'USE EXPERT MODE'
         IERR = 0
         GO TO 999
         END IF
      IERR = 2
      IF ((NCOL.LE.0) .OR. (NCOL.GT.10)) GO TO 999
      CALL COPY (3, TVZOOM, SVZOOM)
C                                       channels
      GR1 = ABS (GRCHS(1))
      IF ((GR1.LT.1) .OR. (GR1.GT.NGRAPH)) GR1 = 1
      GR2 = ABS (GRCHS(2))
      GR2 = MIN (GR2, NGRAPH)
      IF (GR2.EQ.GR1) GR2 = 0
      GR1 = GR1 + NGRAY
      GR2 = GR2 + NGRAY
      CHOICE = ' '
C                                       plan menu
      IS = 1
      NC = 0
      RMAX = 0
      DO 20 I = 1,NCOL
         IX = NROWS(I)
         IF ((IX.LE.0) .OR. (IX.GT.40)) GO TO 999
         NCHM(I) = 1
         RMAX = MAX (RMAX, IX)
         ISC(I) = IS - 1
         DO 10 J = 1,IX
            NCH(J,I) = JTRIM (CHOICS(IS))
            NCH(J,I) = MAX (1, NCH(J,I))
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
      DOMENU = (GRCHS(1).GE.0) .OR. (GRCHS(2).GT.0)
      CALL YWINDO ('READ', WINDTV, IERR)
      ROUTIN = 'YWINDO'
      IF (IERR.NE.0) GO TO 990
      IF (WINDTV(1).NE.MSWIN(1)) DOMENU = .TRUE.
      IF (WINDTV(2).NE.MSWIN(2)) DOMENU = .TRUE.
      IF (WINDTV(3).NE.MSWIN(3)) DOMENU = .TRUE.
      IF (WINDTV(4).NE.MSWIN(4)) DOMENU = .TRUE.
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
         IF (DOMENU) THEN
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
      MCH(2) = 2 * (2 + NEDGE + (RMAX-1) * (1+NEDGE)) + CSIZTV(2) * RMAX
      IF (MCH(2).LT.MSWIN(4)-MSWIN(2)) THEN
         MWIN(2) = MSWIN(2)
         MWIN(4) = MSWIN(4)
      ELSE
         MWIN(2) = (MAXXTV(2) - MCH(2)) / 2 + 1
         MWIN(4) = MWIN(2) + MCH(2) - 1
         END IF
      MENU0 = MWIN(1) + 4
      IF (TYPE.EQ.1) MENU0 = MWIN(3) - MCH(1)
      IF (TYPE.EQ.2) MENU0 = (MWIN(3) - MWIN(1) + 1 - MCH(1)) / 2 + 1
C                                       write menu if needed, get points
      ROUTIN = 'IMCHAR'
      IX = 3 + NEDGE + MENU0
      IS = 1
      DO 45 LCOL = 1,NCOL
         IY = MWIN(4) - 12 - NEDGE - CSIZTV(2) - MAX (0, TOPSEP)
         NROW = NROWS(LCOL)
         IXC(1,LCOL) = IX
         IXC(2,LCOL) = IX + NCHM(LCOL)*CSIZTV(1) - 1
         DO 40 LROW = 1,NROW
            IYC(LROW) = IY
            IF (DOMENU) THEN
               NC = NCH(LROW,LCOL)
               PS = CHOICS(IS)(1:NC)
               CALL IMCHAR (GR1, IX, IY, 0, 0, PS(1:NC), SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               DO 30 I = 1,N
                  IF (CHOICS(IS).EQ.CHOICX(I)) THEN
                     K = INDEX (CHOICS(IS), ALPHAX(I))
                     IF (K.GT.0) THEN
                        K = IX + (K-1)*CSIZTV(1)
                        CALL IMCHAR (GR2, K, IY, 0, 0, ALPHAX(I),
     *                     SCRTCH, IERR)
                        IF (IERR.NE.0) GO TO 900
                        END IF
                     GO TO 35
                     END IF
 30                  CONTINUE
               END IF
 35            IS = IS + 1
            IY = IY - 2*NEDGE - CSIZTV(2)
 40         CONTINUE
         IF ((LCOL.EQ.1) .AND. (TYPE.LT.0) .AND. (NCOL.EQ.2)) THEN
            IX = MWIN(3) - NCHM(2)*CSIZTV(1) - 2 - NEDGE
         ELSE
            IX = IX + 2 + 2*NEDGE + NCHM(LCOL)*CSIZTV(1)
            END IF
 45      CONTINUE
C                                       write border lines
      IF (DOMENU) THEN
         RPOS(1) = MAXXTV(1) / 2
         RPOS(2) = MAXXTV(2) / 2
         IY = MWIN(4) - 11 - MAX (0, TOPSEP)
         DO 50 LCOL = 1,NCOL
            IX = IXC(1,LCOL) - 2 - NEDGE
            LX = 4 + 2*NEDGE + NCHM(LCOL)*CSIZTV(1)
            LY = NROWS(LCOL) * (2*NEDGE + CSIZTV(2)) + 4
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
 50         CONTINUE
         END IF
      CALL YHOLD ('OFFF', I)
C                                       Initial terminal prompt
 100  J = 1
      CHOICE = ' '
      PROMPT = ' '
      DO 120 I = 1,N
         IF (OPTOKX(I).GT.0) THEN
            K = JTRIM (SHORTX(I))
            IF (J+K.GT.73) THEN
               CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
               IF (IERR.NE.0) GO TO 999
               J = 1
               PROMPT = ' '
               END IF
            PROMPT(J:) = SHORTX(I)
            J = J + K + 1
            END IF
 120     CONTINUE
      K = 4
      IF (J+K.GT.73) THEN
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
         IF (IERR.NE.0) GO TO 999
         J = 1
         PROMPT = ' '
         END IF
      PROMPT(J:) = 'eXit'
      J = J + K + 1
      IF (J+K.GT.73) THEN
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
         IF (IERR.NE.0) GO TO 999
         J = 1
         PROMPT = ' '
         END IF
      PROMPT(J:) = 'Help'
      CALL INQSTR (TTY, PROMPT, 1, CHRCTR, IERR)
      IF (IERR.GT.0) GO TO 999
      CALL CHLTOU (1, CHRCTR)
C                                       if nothing was inputted, use
C                                       last or exit.
      IF (CHRCTR.EQ.' ') CHRCTR = LSTCHR
      LSTCHR = CHRCTR
C                                       implement help right here
      IF (CHRCTR.EQ.'H') THEN
         PROMPT = 'Code' // '  Prompt  ' // 'Full menu OP name'
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
         IF (IERR.NE.0) GO TO 999
         DO 140 I = 1,N
            IF (OPTOKX(I).GT.0) THEN
               PROMPT = '   ' // ALPHAX(I) // '   ' // SHORTX(I) //
     *            '   ' // CHOICX(I)
               CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
 140        CONTINUE
         PROMPT = '   ' // 'X' // '   ' // 'eXit' // '   ' //
     *      'EXIT EXPERT MODE'
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
         IF (IERR.NE.0) GO TO 999
         PROMPT = '   ' // 'H' // '   ' // 'Help' // '   ' //
     *      'THIS HELP LIST'
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Exit
      ELSE IF (CHRCTR.EQ.'X') THEN
         CHOICE = 'USE EXPERT MODE'
C                                       Search list
      ELSE
         DO 150 I = 1,N
            IF ((OPTOKX(I).GT.0) .AND. (CHRCTR.EQ.ALPHAX(I)))
     *         CHOICE = CHOICX(I)
 150        CONTINUE
         END IF
      IF (CHOICE.EQ.' ') GO TO 100
C                                       turn off the choices
 900  CALL YHOLD ('ONNN', I)
      DO 910 I = 1,IS
         IF (CHOICE.EQ.CHOICS(I)) THEN
            IF (.NOT.LEAVE(I)) THEN
               CALL YSLECT ('OFFF', GR1, 0, SCRTCH, JERR)
               IF (GR2.GT.0) CALL YSLECT ('OFFF', GR2, 0, SCRTCH, JERR)
               END IF
            END IF
 910     CONTINUE
      IF (CHOICE.EQ.'USE EXPERT MODE') CALL YZERO (GR2, JERR)
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
 1990 FORMAT ('TVXPRT: TV I/O ERROR',I7,' FROM ',A)
      END
