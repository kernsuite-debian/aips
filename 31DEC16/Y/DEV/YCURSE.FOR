      SUBROUTINE YCURSE (OP, WAIT, CORR, RPOS, QUAD, BUTTON, IERR)
C-----------------------------------------------------------------------
C! read and control TV cursor
C# Y0 TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2004, 2008
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
C   YCURSE reads cursor positions and controls the blink and visibility
C   of the TV cursor.
C   Inputs:
C      OP      C*4     'READ' read cursor position
C                      'ONNN' place cursor at RPOS & leave on
C                      'OFFF' turn cursor off
C                      'BLNK' reverse sense of cursor blink
C                      'FXIT' fix RPOS for zoom scroll, no IO
C      WAIT    L       wait for event; then return RPOS & BUTTON
C                      (done on all OPs)
C      CORR    L       T => correct RPOS for zoom & scroll
C   In/Out:
C      RPOS    R(2)    X,Y screen pos before zoom & scroll
C      QUAD    I       TV quadrant to use for scrolls
C                         In: if <1 >4, no scroll
C                         Out: if in=-1, no scroll, else find
C                              quadrant (needs real TV pos)
C   Output:
C      BUTTON  I       event # (0 none, 1-7 low buttons,
C                      8-15 the "quit" button)
C      IERR    I       error code of Z...XF : 0 - ok
C                                             2 - input error
C   Generic version
C-----------------------------------------------------------------------
      CHARACTER  OP*4
      REAL       RPOS(2)
      INTEGER    BUTTON, IERR, QUAD
      LOGICAL    WAIT, CORR
C
      CHARACTER  OPCODE(5)*4
      REAL       SRX, SRY, CURDEL, TMAG, TEMP
      INTEGER    NOP, MAG, ISX, ISY, IX, IY, IRBLNK, IBUT, J, NSCR, I,
     *   IOP, ZAND, IXC, IYC, ISCRX, ISCRY
      LOGICAL    OLDON, LINKX, LINKY, F, T, ON
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NOP, OPCODE /5, 'READ','ONNN','OFFF','BLNK','FXIT'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      BUTTON = 0
C                                        get opcode
      IERR = 2
      DO 10 IOP = 1,NOP
         IF (OP.EQ.OPCODE(IOP)) GO TO 20
 10      CONTINUE
      GO TO 999
C                                        ON: alters cursor pos
C                                        go from real to screen
 20   IF (IOP.NE.2) GO TO 40
         IF (CORR) GO TO 25
            ISX = RPOS(1) + 0.5
            ISY = RPOS(2) + 0.5
            GO TO 40
C                                        what scroll to use
 25      ISCRX = 0
         ISCRY = 0
         IF ((QUAD.GE.1) .AND. (QUAD.LE.4)) THEN
            NSCR = NGRAY + NGRAPH
            DO 30 I = 1,NSCR
               J = 2 ** (I-1)
               IF (ZAND(J,TVLIMG(QUAD)).NE.0) THEN
                  IF (I.LE.NGRAY) THEN
                     ISCRX = TVSCRX(I)
                     ISCRY = TVSCRY(I)
                  ELSE
                     ISCRX = TVSCGX
                     ISCRY = TVSCGY
                     END IF
                  GO TO 35
                  END IF
 30            CONTINUE
            END IF
 35      MAG = 1 + TVZOOM(1)
         IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
         TMAG = MAG
         SRX = MOD (RPOS(1) - 1.0 + ISCRX + 10.0*MAXXTV(1),
     *      REAL(MAXXTV(1))) + 1.0
         SRY = MOD (RPOS(2) - 1.0 + ISCRY + 10.0*MAXXTV(2),
     *      REAL(MAXXTV(2))) + 1.0
         IXC = (TVZOOM(2) - 1.0) * (TMAG - 1.0) / TMAG + 1.
         ISX = TMAG * (SRX - IXC) + 1.001
         IYC = (MAXXTV(2) - TVZOOM(3)) * (TMAG - 1.) / TMAG
         TEMP = MAXXTV(2) - IYC - MAXXTV(2) / TMAG + 1.0
         ISY = TMAG * (SRY - TEMP) + 1.001
         ISX = ISX + (MAG-1)/2
         ISY = ISY + (MAG-1)/2
C                                        read current status
 40   IF (IOP.EQ.5) THEN
         IX = RPOS(1) + 0.5
         IY = RPOS(2) + 0.5
C                                       limit check
         IX = MAX (1, MIN (MAXXTV(1), IX))
         IY = MAX (1, MIN (MAXXTV(2), IY))
         GO TO 70
         END IF
      CALL YCRCTL ('READ', OLDON, IX, IY, LINKX, LINKY, IRBLNK, IBUT,
     *   F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       limit check - should not be
C                                       needed but maybe
      IX = MAX (1, MIN (MAXXTV(1), IX))
      IY = MAX (1, MIN (MAXXTV(2), IY))
C                                        quick read is nearly done
      IF ((IOP.EQ.1) .AND. (.NOT.WAIT)) GO TO 70
C                                        ON: set new position
         IF (IOP.EQ.2) THEN
            IX = ISX
            IY = ISY
            END IF
C                                        BLNK: reverse blink rate
 45      IF (IOP.EQ.4) IRBLNK = MOD (IRBLNK+1, 2)
C                                        if reading set to ON
C                                        else set requested status
         ON = T
         IF ((IOP.EQ.3) .AND. (.NOT.WAIT)) ON = F
         IF ((IOP.EQ.4) .AND. (.NOT.WAIT)) ON = OLDON
C                                        write new pars to curs reg.
         CALL YCRCTL ('WRIT', ON, IX, IY, LINKX, LINKY, IRBLNK, IBUT,
     *      F, IERR)
         IF ((IERR.NE.0).OR.(.NOT.WAIT)) GO TO 999
C                                        wait for event read cycle
 50      IF (IBUT.NE.0) GO TO 60
            CURDEL = 0.2
            IF (AIPSMK.GT.0.2) CURDEL = 0.4 / AIPSMK
            CALL ZDELAY (CURDEL, IERR)
            CALL YCRCTL ('READ', ON, IX, IY, LINKX, LINKY, IRBLNK, IBUT,
     *         F, IERR)
            IF (IERR.NE.0) GO TO 999
            GO TO 50
C                                        event: reset status if need
 60      IF (IOP.EQ.3) ON = F
         IF ((IOP.EQ.1) .OR. (IOP.EQ.4)) ON = OLDON
         IF (.NOT.ON) THEN
            CALL YCRCTL ('WRIT', ON, IX, IY, LINKX, LINKY, IRBLNK, IBUT,
     *         F, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                        return answers
 70   IF (IOP.NE.5) BUTTON = IBUT
      IF (CORR) GO TO 75
         RPOS(1) = IX
         RPOS(2) = IY
         GO TO 999
C                                        correct for zoom & scroll
 75   CONTINUE
C                                        Corr half cell
         MAG = 1 + TVZOOM(1)
         IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
         IX = IX - (MAG-1)/2
         IY = IY - (MAG-1)/2
C                                        What quadr. scroll
         ISCRX = 0
         ISCRY = 0
         IF (QUAD.NE.-1) THEN
            QUAD = 4
            IF ((IX.GE.TVSPLT(1)) .AND. (IY.GE.TVSPLT(2))) QUAD = 1
            IF ((IX.LT.TVSPLT(1)) .AND. (IY.GE.TVSPLT(2))) QUAD = 2
            IF ((IX.LT.TVSPLT(1)) .AND. (IY.LT.TVSPLT(2))) QUAD = 3
            NSCR = NGRAY + NGRAPH
            DO 80 I = 1,NSCR
               J = 2 ** (I-1)
               IF (ZAND(J,TVLIMG(QUAD)).NE.0) THEN
                  IF (I.LE.NGRAY) THEN
                     ISCRX = TVSCRX(I)
                     ISCRY = TVSCRY(I)
                  ELSE
                     ISCRX = TVSCGX
                     ISCRY = TVSCGY
                     END IF
                  GO TO 90
                  END IF
 80            CONTINUE
            END IF
 90      TMAG = MAG
         IXC = (TVZOOM(2) - 1.0) * (TMAG - 1.0) / TMAG + 1.
         RPOS(1) = (IX - 1.0) / TMAG + IXC
         IYC = (MAXXTV(2) - TVZOOM(3)) * (TMAG - 1.) / TMAG
         TEMP = MAXXTV(2) - IYC - MAXXTV(2) / TMAG + 1.0
         RPOS(2) = (IY - 1.0) / TMAG + TEMP
C                                       correct for scroll
 100  RPOS(1) = MOD (RPOS(1) - 1.0 - ISCRX + 10.0*MAXXTV(1),
     *   REAL(MAXXTV(1))) +  1.0
      RPOS(2) = MOD (RPOS(2) - 1.0 - ISCRY + 10.0*MAXXTV(2),
     *   REAL(MAXXTV(2)))  +  1.0
      IERR = 0
C
 999  RETURN
      END
