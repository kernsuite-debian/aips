      SUBROUTINE YCURSE (OP, WAIT, CORR, RPOS, QUAD, BUTTON, IERR)
C-----------------------------------------------------------------------
C! read and control TV cursor
C# TV-util
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
C      RPOS    R(2)    X,Y screen pos before  zoom & scroll
C      QUAD    I       TV quadrant to use for scrolls
C                         In: if <1 >4, no scroll
C                         Out: if in=-1, no scroll, else find
C                              quadrant (needs real TV pos)
C   Output:
C      BUTTON  I       event # (0 none, 1-7 low buttons,
C                      8-15 the "quit" button)
C      IERR    I       error code of Z...XF : 0 - ok
C                                             2 - input error
C   IVAS version
C   IVAS version - generic plus bug in 12-bit version 2.n IVAS  ??????
C-----------------------------------------------------------------------
      CHARACTER  OP*4
      REAL       RPOS(2)
      INTEGER    BUTTON, IERR, QUAD
      LOGICAL    WAIT, CORR
C
      CHARACTER  OPCODE(5)*4
      REAL       SRX, SRY, CURDEL, TMAG, TEMP
      INTEGER    ISCRX, ISCRY, NOP, MAG, ISX, ISY, IX, IY, IRBLNK, IBUT,
     *   J, NSCR, I, IOP, ZAND, IXC, IYC
      LOGICAL    OLDON, LINKX, LINKY, F, T, ON
      INCLUDE 'INCS:DTVC.INC'
      DATA NOP, OPCODE /5, 'READ','ONNN','OFFF','BLNK','FXIT'/
      DATA CURDEL /0.2/
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
         IF ((QUAD.LT.1) .OR. (QUAD.GT.4)) GO TO 35
            NSCR = NGRAY + NGRAPH
            DO 30 I = 1,NSCR
               J = 2 ** (I-1)
               IF (ZAND(J,TVLIMG(QUAD)).NE.0) GO TO 31
 30            CONTINUE
            GO TO 35
 31         IF (I.GT.NGRAY) I = NGRAY + 1
            ISCRX = TVSCRX(I)
            ISCRY = TVSCRY(I)
 35      MAG = 1 + TVZOOM(1)
         IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
         TMAG = MAG
C                                       IVAS 12-bit bug V2.n
         IF (NGRAY.EQ.2) THEN
            SRX = MOD (RPOS(1) - 1.0 + ISCRX + 10.0*MAXXTV(1)
     *         - 2.0 + 2.0/TMAG,
     *         REAL(MAXXTV(1))) + 1.0
            SRY = MOD (RPOS(2) - 1.0 + ISCRY + 10.0*MAXXTV(2)
     *         + 1.0/TMAG,
     *         REAL(MAXXTV(2))) + 1.0
         ELSE
            SRX = MOD (RPOS(1) - 1.0 + ISCRX + 10.0*MAXXTV(1),
     *         REAL(MAXXTV(1))) + 1.0
            SRY = MOD (RPOS(2) - 1.0 + ISCRY + 10.0*MAXXTV(2),
     *         REAL(MAXXTV(2))) + 1.0
            END IF
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
         GO TO 70
         END IF
      CALL YCRCTL ('READ', OLDON, IX, IY, LINKX, LINKY, IRBLNK, IBUT,
     *   F, IERR)
      IF (IERR.NE.0) GO TO 999
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
         IF ((IERR.NE.0) .OR. (.NOT.WAIT)) GO TO 999
C                                        wait for event read cycle
 50      IF (IBUT.NE.0) GO TO 60
            CALL ZDELAY (CURDEL, IERR)
            CALL YCRCTL ('READ', ON, IX, IY, LINKX, LINKY, IRBLNK, IBUT,
     *         F, IERR)
            IF (IERR.NE.0) GO TO 999
            GO TO 50
C                                        event: reset status if need
 60      IF (IOP.EQ.3) ON = F
         IF ((IOP.EQ.1) .OR. (IOP.EQ.4)) ON = OLDON
         IF (.NOT.ON) CALL YCRCTL ('WRIT', ON, IX, IY, LINKX, LINKY,
     *      IRBLNK, IBUT, F, IERR)
         IF (IERR.NE.0) GO TO 999
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
         IF (QUAD.EQ.-1) GO TO 90
            QUAD = 4
            IF ((IX.GE.TVSPLT(1)) .AND. (IY.GE.TVSPLT(2))) QUAD = 1
            IF ((IX.LT.TVSPLT(1)) .AND. (IY.GE.TVSPLT(2))) QUAD = 2
            IF ((IX.LT.TVSPLT(1)) .AND. (IY.LT.TVSPLT(2))) QUAD = 3
            NSCR = NGRAY + NGRAPH
            DO 80 I = 1,NSCR
               J = 2 ** (I-1)
               IF (ZAND(J,TVLIMG(QUAD)).NE.0) GO TO 85
 80            CONTINUE
            GO TO 90
 85         IF (I.GT.NGRAY) I = NGRAY + 1
            ISCRX = TVSCRX(I)
            ISCRY = TVSCRY(I)
 90      TMAG = MAG
         IXC = (TVZOOM(2) - 1.0) * (TMAG - 1.0) / TMAG + 1.
         IYC = (MAXXTV(2) - TVZOOM(3)) * (TMAG - 1.) / TMAG
         TEMP = MAXXTV(2) - IYC - MAXXTV(2) / TMAG + 1.0
C                                       IVAS 12-bit bug V2.n
         IF (NGRAY.EQ.2) THEN
            RPOS(1) = (IX - 1.0) / TMAG + IXC  + 2.0 - 2.0/TMAG
            RPOS(2) = (IY - 1.0) / TMAG + TEMP - 1.0 / TMAG
         ELSE
            RPOS(1) = (IX - 1.0) / TMAG + IXC
            RPOS(2) = (IY - 1.0) / TMAG + TEMP
            END IF
C                                       correct for scroll
 100  RPOS(1) = MOD (RPOS(1) - 1.0 - ISCRX + 10.0*MAXXTV(1),
     *   REAL(MAXXTV(1))) +  1.0
      RPOS(2) = MOD (RPOS(2) - 1.0 - ISCRY + 10.0*MAXXTV(2),
     *   REAL(MAXXTV(2)))  +  1.0
      IERR = 0
C
 999  RETURN
      END
