      SUBROUTINE AU6A (BRANCH)
C-----------------------------------------------------------------------
C! verbs to set the TV blank and white LUT linearly and to blink planes
C# POPS-appl TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-1999, 2001, 2008-2009, 2015
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
C   AU6A performs linear interactive enhancement of TV images
C   BRANCH = 1   OFFTRAN  (LUTs of IMCHAN to linear)
C            2   TVTRAN   (interactive set LUTs of IMCHAN)
C            3   TVBLINK  (Blink 2 images, allow TVTRANs in middle)
C            4   TVMBLINK (Blink 2 images manually or auto)
C            5   TVLUT    (Set LUT as NPOINTS piecewise linear using
C                          graphics)
C            6   TVMLUT   (as TVLUT, but any number pieces)
C            7   TVSPLIT  (interactive shift of split pt 2 planes)
C            8   GRBLINK  (Blink 2 graphics planes)
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INCLUDE 'INCS:PTVC.INC'
      CHARACTER PRGNAM*6, CDUM*1
      INTEGER   POTERR, ICOLOR, BUFFER(4096), IC, IERR, QUAD, ICS(2),
     *   ITM, IBUT, IXS, IYS, ICHA(4), I, ENTRY, IG, ITYPE, J, JERR,
     *   ISMOD, ITW(3), ISMAX, ISMIN, GC, IQ, ICHAN, LUTBUF(TVMLUT), II,
     *   ZOR
      REAL      RPOS(2,2), SLOPE, XPOS(2), PPOS(2), RDUM, S, X, RSZX,
     *   RSZY, D1, D2
      LOGICAL   T, F, DOIT
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (LUTBUF, BUFFER)
      COMMON /AIPSCR/ BUFFER
      DATA T, F /.TRUE.,.FALSE./
      DATA PRGNAM /'AU6A '/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.8)) GO TO 999
C                                        general parameters
      POTERR = 49
      IF (BRANCH.EQ.8) THEN
         CALL ADVERB ('GRCHAN', 'I', 1, 0, GC, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
      ELSE
         CALL ADVERB ('COLORS', 'I', 1, 0, ICOLOR, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('TVCHAN', 'I', 1, 0, ICHAN, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         IC = 0
         IF ((ICHAN.GT.0) .AND. (ICHAN.LE.NGRAY)) IC = 2 ** (ICHAN-1)
         END IF
C                                        open TV device
      CALL TVOPEN (BUFFER, IERR)
      IF (IERR.NE.0) THEN
         POTERR = 101
         GO TO 980
         END IF
C                                       check TV state
      IF ((TVLIMG(1).NE.TVLIMG(2)) .OR. (TVLIMG(1).NE.TVLIMG(3)) .OR.
     *   (TVLIMG(1).NE.TVLIMG(4))) THEN
         IF ((BRANCH.GE.3) .AND. (BRANCH.LE.7)) THEN
            POTERR = 77
            GO TO 900
            END IF
         END IF
C                                        interpret adverbs
      IXS = TVSPLM / 100
      IYS = TVSPLM - 100 * IXS
      IXS = IXS * IYS
      IF (IXS.LE.0) THEN
         II = ZOR (TVLIMG(1), TVLIMG(2))
         II = ZOR (II, TVLIMG(3))
         II = ZOR (II, TVLIMG(4))
      ELSE
         II = (2 ** IXS) - 1
         END IF
      IF (BRANCH.EQ.8) THEN
         J = 2 ** NGRAY
         IC = MOD (II, J)
      ELSE
         IF ((ICOLOR.LT.1) .OR. (ICOLOR.GT.3)) ICOLOR = 0
         IF (ICOLOR.GT.0) ICOLOR = 2 ** (3-ICOLOR)
         IF (ICOLOR.EQ.0) ICOLOR = 7
         J = 2**NGRAY
         IF (IC.EQ.0) IC = MOD (II, J)
         IF (IC.EQ.0) IC = 1
         END IF
      RPOS(1,1) = 0.0
      RPOS(1,2) = 0.0
      RPOS(2,1) = 0.0
      RPOS(2,2) = 0.0
C                                        branch to Operation
      GO TO (100, 200, 300, 300, 500, 500, 700, 800), BRANCH
C-----------------------------------------------------------------------
C                                        OFFTRAN
C                                        linear lookup = null
C-----------------------------------------------------------------------
 100  J = MAXINT + 1
      SLOPE = (LUTOUT+1.0) / (MAXINT+1.0)
      DO 110 I = 1,J
         LUTBUF(I) = I * SLOPE - 0.5
 110     CONTINUE
      CALL YLUT ('WRIT', IC, ICOLOR, F, LUTBUF, IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 900
C-----------------------------------------------------------------------
C                                        TVTRAN
C                                        interactive linear lookup tab
C-----------------------------------------------------------------------
 200  WRITE (MSGTXT,1200)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1201)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1202)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1203)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1204)
      CALL MSGWRT (1)
C                                        hide this mess in subroutine
      ITYPE = 1
      CALL IENHNS (IC, ICOLOR, ITYPE, RPOS, BUFFER, IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 900
C-----------------------------------------------------------------------
C                                        BLINK
C                                        interactive linear lookup tab
C                                        between blinking
C-----------------------------------------------------------------------
 300  CALL ADVERB ('TV2CHAN', 'I', 1, 0, ICS(2), RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 900
      POTERR = 31
      IF (NGRAY.LT.2) GO TO 900
      POTERR = 49
      ICS(1) = ICHAN
      ENTRY = BRANCH - 2
      IF ((ICS(1).LE.0) .OR. (ICS(1).GT.NGRAY) .OR. (ICS(2).LE.0)
     *   .OR. (ICS(2).GT.NGRAY) .OR. (ICS(1).EQ.ICS(2))) THEN
         ICS(1) = 1
         ICS(2) = 2
         END IF
C                                       Do blink etc
      CALL TVBLNK (ENTRY, ICS, BUFFER, IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 900
C-----------------------------------------------------------------------
C                                       TVLUT
C                                       TVMLUT
C                                       interactive graphics to make
C                                       piecewise linear LUT
C-----------------------------------------------------------------------
 500  POTERR = 31
      IG = MIN (3, NGRAPH)
      IF (IG.LE.0) GO TO 900
      J = NGRAY + IG
      CALL YCINIT (J, BUFFER)
      IF (BRANCH.EQ.5) THEN
         CALL ADVERB ('NPOINTS', 'I', 1, 0, J, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 900
         J = MAX (1, MIN (127, J)) + 2
      ELSE
         J = 0
         END IF
      CALL GRLUTS (J, IG, IC, ICOLOR, LUTBUF, IERR)
      POTERR = 49
      IF (IERR.EQ.0) POTERR = 0
      GO TO 900
C-----------------------------------------------------------------------
C                                        TVSPLIT
C                                        interactive linear lookup tab
C                                        between shifting split pt
C-----------------------------------------------------------------------
 700  CALL ADVERB ('TV2CHAN', 'I', 1, 0, ICS(2), RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 900
      ICS(1) = ICHAN
      CALL ZTIME (ITW)
      POTERR = 31
      IF (NGRAY.LT.2) GO TO 900
      IF (TYPSPL.LE.0) GO TO 900
      POTERR = 49
      ICOLOR = 7
      IF ((ICS(1).LE.0) .OR. (ICS(1).GT.NGRAY) .OR. (ICS(2).LE.0) .OR.
     *   (ICS(2).GT.NGRAY) .OR. (ICS(1).EQ.ICS(2))) THEN
         ICS(1) = 1
         ICS(2) = 2
         END IF
C                                       Do messages
      XPOS(1) = MAXXTV(1) / 2.0 + 0.5
      XPOS(2) = MAXXTV(2) / 2.0 + 0.5
      PPOS(1) = 0.0
      PPOS(2)  = 0.0
      ISMIN = 1
      ISMAX = 4
      IF (TYPSPL.EQ.4) ISMAX = 6
      IF (TYPSPL.EQ.1) ISMAX = 2
      IF (TYPSPL.EQ.2) ISMIN = 3
      ISMOD = ISMIN
      QUAD = -1
 715  WRITE (MSGTXT,1310) ICS(1)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1311) ICS(2)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1715)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1204)
      CALL MSGWRT (1)
      CALL YCURSE ('ONNN', F, F, XPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Set split mode: vertical
 720  IF (ISMOD.LE.2) THEN
         ICHA(1) = 2 ** (ICS(3-ISMOD)-1)
         ICHA(2) = 2 ** (ICS(ISMOD)-1)
         ICHA(3) = ICHA(2)
         ICHA(4) = ICHA(1)
C                                       Set split mode: horizontal
      ELSE IF (ISMOD.LE.4) THEN
         ICHA(1) = 2 ** (ICS(ISMOD-2)-1)
         ICHA(2) = ICHA(1)
         ICHA(3) = 2 ** (ICS(5-ISMOD)-1)
         ICHA(4) = ICHA(3)
C                                       Set split mode: diagonal
      ELSE
         ICHA(1) = 2 ** (ICS(ISMOD-4)-1)
         ICHA(2) = 2 ** (ICS(7-ISMOD)-1)
         ICHA(3) = ICHA(1)
         ICHA(4) = ICHA(2)
         END IF
C                                       Split point
 730  IXS = XPOS(1) + 0.5
      IYS = XPOS(2) + 0.5
      CALL YSPLIT ('WRIT', IXS, IYS, ICHA, ICHA, ICHA, T, IERR)
      IF (IERR.NE.0) GO TO 790
C                                        read loop
 740  CALL YCURSE ('READ', F, F, XPOS, QUAD, IBUT, IERR)
      IF ((IBUT.GT.7) .OR. (IERR.NE.0)) GO TO 790
         CALL DLINTR (XPOS, IBUT, PPOS, ITW, DOIT)
         IF (.NOT.DOIT) GO TO 740
         IF (IBUT.LE.0) GO TO 730
C                                       Pop mode
         IF (IBUT.LT.4) GO TO 750
            ISMOD = ISMOD + 1
            IF (ISMOD.GT.ISMAX) ISMOD = ISMIN
            GO TO 720
C                                       Enhancement
 750     ITM = IBUT
         IF (IBUT.EQ.3) ITM = 2
         WRITE (MSGTXT,1340) ICS(ITM)
         CALL MSGWRT (1)
         IF (ITM.GE.2) WRITE (MSGTXT,1310) ICS(1)
         IF (ITM.NE.2) WRITE (MSGTXT,1311) ICS(2)
         CALL MSGWRT (1)
         WRITE (MSGTXT,1342)
         CALL MSGWRT (1)
         WRITE (MSGTXT,1750)
         CALL MSGWRT (1)
         I = 2 ** (ICS(ITM) - 1)
         ITYPE = 2
         CALL IENHNS (I, ICOLOR, ITYPE, RPOS(1,ITM), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 790
         IBUT = ITYPE
         IF (IBUT.LE.2) GO TO 750
         GO TO 715
C                                       turn it off
 790  CALL YHOLD ('ONNN', JERR)
      CALL YCURSE ('OFFF', F, F, XPOS, QUAD, IBUT, JERR)
      DO 795 I = 1,NGRAY
         CALL YSLECT ('OFFF', I, 0, BUFFER, JERR)
 795     CONTINUE
      CALL YSLECT ('ONNN', ICS(1), 0, BUFFER, JERR)
      IF (JERR.EQ.0) POTERR = 0
      GO TO 900
C-----------------------------------------------------------------------
C                                        GRBLINK
C                                        graphics blink
C-----------------------------------------------------------------------
 800  POTERR = 31
      IF (NGRAPH.LT.2) GO TO 900
      POTERR = 49
      ICS(1) = GC
      CALL ADVERB ('GR2CHAN', 'I', 1, 0, ICS(2), RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 900
      IF ((ICS(1).LE.0) .OR. (ICS(1).GT.NGRAPH) .OR. (ICS(2).LE.0) .OR.
     *   (ICS(2).GT.NGRAPH) .OR. (ICS(1).EQ.ICS(2))) THEN
         ICS(1) = 1
         ICS(2) = 2
         END IF
      ICS(1) = ICS(1) + NGRAY
      ICS(2) = ICS(2) + NGRAY
      MSGTXT = 'Hit any button to stop blinking'
      CALL MSGWRT (1)
C                                       Cursor on
      RSZX = WINDTV(3) - WINDTV(1) + 1
      RSZY = WINDTV(4) - WINDTV(2) + 1
      RPOS(1,1) = (WINDTV(1) + WINDTV(3)) / 2
      RPOS(2,1) = (WINDTV(2) + WINDTV(4)) / 2
      PPOS(1) = RPOS(1,1)
      PPOS(2) = RPOS(2,1)
      CALL YCURSE ('ONNN', F, F, RPOS, IQ, IBUT, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Blinking
 810  DO 820 I = 1,2
         CALL YHOLD ('ONNN', IERR)
         CALL YSLECT ('OFFF', ICS(3-I), 0, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL YSLECT ('ONNN', ICS(I), 0, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL YHOLD ('OFFF', IERR)
         X = (RPOS(1,1) - WINDTV(1) + 1.) / RSZX
         S = 5.8 * X * X  -  0.72 * X  -  0.08
         X = 0.5 * (RPOS(2,1) - WINDTV(2) + 1.) / RSZY  + 0.25
         IF (I.EQ.2) X = 1.0 - X
         S = S * X
         IF (S.GT.0.0) THEN
            CALL ZDELAY (S, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         CALL YCURSE ('READ', F, F, RPOS, IQ, IBUT, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (IBUT.NE.0) GO TO 830
         D1 = ABS (RPOS(1,1)-PPOS(1)) / (0.75*MAXXTV(1))
         D2 = ABS (RPOS(2,1)-PPOS(2)) / (0.75*MAXXTV(2))
         IF ((D1.GE.1.0) .OR. (D2.GE.1.0)) THEN
            IF (D1.GE.1.0) RPOS(1,1) = PPOS(1)
            IF (D2.GE.1.0) RPOS(2,1) = PPOS(2)
            CALL YCURSE ('ONNN', F, F, RPOS, IQ, IBUT, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         PPOS(1) = RPOS(1,1)
         PPOS(2) = RPOS(2,1)
 820     CONTINUE
      GO TO 810
C                                       Button hit
 830  CALL YSLECT ('OFFF', ICS(2), 0, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL YSLECT ('ONNN', ICS(1), 0, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 900
      POTERR = 0
      GO TO 900
C-----------------------------------------------------------------------
C                                        close down
C                                        cursor off, TV closed
 900  CALL TVCLOS (BUFFER, JERR)
C                                        messages on error
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1900) IERR
         CALL MSGWRT (7)
         END IF
C                                       POPS error stack
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('Cursor X position controls intercept')
 1201 FORMAT ('Cursor Y position controls slope')
 1202 FORMAT ('Hit buttons A or B to turn plot off or back on')
 1203 FORMAT ('Hit button C to reverse sign of slope')
 1204 FORMAT ('Hit button D to exit')
 1310 FORMAT ('Hit button A to set transfer function of channel',I2)
 1311 FORMAT ('Hit button B to set transfer function of channel',I2)
 1340 FORMAT ('Setting transfer function for channel',I2)
 1342 FORMAT ('Hit button C to reverse sign of slope')
 1715 FORMAT ('Hit button C to step the split mode')
 1750 FORMAT ('Hit button D to resume split movement')
 1900 FORMAT ('TV ACTION ERROR CODE',I7)
      END
