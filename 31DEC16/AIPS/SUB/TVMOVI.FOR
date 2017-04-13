      SUBROUTINE TVMOVI (ICHAN, JTY, JMAG, SMAG, IFR, LFR, DOREV,
     *   TIMEIT, INBUF, IERR)
C-----------------------------------------------------------------------
C! runs movie algorithm on pre-loaded images, with interactions
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2003, 2008, 2015
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
C   TVMOVI runs a movie algorithm:  Button A turns movie on or off,
C   B switches direction, C goes to interactive enhancement mode and
C   D exits.  Cursor X controls frame rate or selection of frame.
C   Inputs:
C      ICHAN   I      Lowest channel number to use (1 - NGRAY)
C      JMAG    I      Magnification step: linear if MXZOOM < 0, else
C                     the power of 2.  Controls the size and spacing of
C                     the subimages and is the default magnification.
C      SMAG    I      Requested magnification of display <= JMAG
C      IFR     I      Initial frame number
C      LFR     I      Final frame number
C      DOREV   L      T => reverse frame direction at each end
C      TIMEIT  L      T => run as fast as possible and report frame rate
C   Output:
C      INBUF   I(*)   >(3072) scratch buffer
C      IERR    I      TV IO error : 0 => ok.
C-----------------------------------------------------------------------
      INTEGER   ICHAN, JTY, JMAG, SMAG, IFR, LFR, INBUF(*), IERR
      LOGICAL   DOREV, TIMEIT
C
      INCLUDE 'INCS:PTVC.INC'
      INTEGER   MAG, MXX, MYY, KX0, KY0, MP, I, J, CHMASK(16), IX, IY,
     *   ID, OIPL, IPL, N, IBUT, IX0, IY0, ICH(4), ZCH(4), JERR, KMAG,
     *   MY, CHTMSK, JP, ITRTYP, NLEVS, ITW(3), QUAD, ICONT, NCONT,
     *   ITYPE, LX, IT, JX, JY, JR, JR0, LX0, LY0, COR8(8), COR4(4),
     *   TWIN(4), MX, LMAG, LLMAG, TVZINI(3), SAVSCX(16), SAVSCY(16),
     *   XSC, YSC, ITIM(6), NFRAME, JJ, II
      REAL      RS, RRATE, CURDEL, XP, X, PPOS(2), RPOS(2), RBPOS(2),
     *   PGAMMA, RSZX, RSZY, RBUF(TVMOFM), OFFS
      LOGICAL   T, F, L1, L2, L3, ONEFRM, SWITCH, DOIT, M70E, WRKSTN
      DOUBLE PRECISION JD0, JD1, FRATE
      CHARACTER ROUTIN*6
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA ZCH, CURDEL /4*0, 0.2/
      DATA COR8 /3,3,3,2,2,1,1,0/
      DATA COR4 /1,1,1,0/
C-----------------------------------------------------------------------
C                                       Check inputs
      IERR = 2
      IF ((ICHAN.LT.1) .OR. (ICHAN.GT.NGRAY)) GO TO 999
      IF ((JMAG.LT.0) .OR. (JMAG.GT.IABS(MXZOOM))) GO TO 999
      IF ((IFR.LT.1) .OR. (LFR.LT.IFR)) GO TO 999
      CALL COPY (3, TVZOOM, TVZINI)
      CALL COPY (16, TVSCRX, SAVSCX)
      CALL COPY (16, TVSCRY, SAVSCY)
C                                       IIS Model 70E, special setup
      M70E = (MAXXTV(2).EQ.480) .AND. (SCXINC.NE.1)
C                                       Init TV parameters
      CALL YWINDO ('READ', TWIN, JERR)
      MAG = 1 + JMAG
      IF (MXZOOM.GT.0) MAG = 2 ** JMAG
      JR0 = 0
      IF (MOD(ICHAN,2).EQ.0) JR0 = MAG
      IF (MAG.EQ.1) THEN
         MX = MAXXTV(1) - 1
         MY = MAXXTV(2) - 1
      ELSE
         I = 1 - MXZOOM
         IF (MXZOOM.GT.0) I = 2 ** MXZOOM
         MXX = MAXXTV(1) / (MAG * (MAG-1))
         MX = MXX * (MAG - 1)
         MXX = MXX * MAG
         IF ((MAG-1)*MXX.GE.MAXXTV(1)) THEN
            MXX = (MAXXTV(1)-I) / (MAG * (MAG-1))
            MX = MXX * (MAG - 1)
            MXX = MXX * MAG
            END IF
         MYY = MAXXTV(2) / (MAG * (MAG-1))
         MY = MYY * (MAG-1)
         MYY = MYY * MAG
         IF (MAG*MY.GE.MAXXTV(2)) THEN
            MYY = (MAXXTV(2)-I) / (MAG * (MAG-1))
            MY = MYY * (MAG-1)
            MYY = MYY * MAG
            END IF
         END IF
      KX0 = MAXXTV(1) / 2
      KY0 = MAXXTV(2) / 2
      LX0 = 0
      LY0 = 0
      MP = MAG * MAG
      IPL = (LFR - 1) / MP + ICHAN
      CHTMSK = 0
      DO 15 I = ICHAN,IPL
         CHMASK(I) = 2 ** (I-1)
         CHTMSK = CHTMSK + CHMASK(I)
 15      CONTINUE
C                                       test for work station windows
      KMAG = MIN (SMAG, JMAG)
      WRKSTN = .TRUE.
      I = 1 + KMAG
      IF (MXZOOM.GT.0) I = 2 ** KMAG
      JX = I * MX
      WINDTV(1) = 1
      WINDTV(3) = WINDTV(1) + JX + 1
      WINDTV(3) = MIN (MAXXTV(1), WINDTV(3))
      JY = I * MY
      WINDTV(2) = MAXXTV(2) - JY - 1
      WINDTV(2) = MAX (1, WINDTV(2))
      WINDTV(4) = MAXXTV(2)
      WINDTV(4) = MIN (MAXXTV(2), WINDTV(4))
      CALL YWINDO ('WRIT', WINDTV, IERR)
      IF (IERR.NE.0) THEN
         KMAG = JMAG
         WRKSTN = .FALSE.
         END IF
      LMAG = 1 + KMAG
      IF (MXZOOM.GT.0) LMAG = 2 ** KMAG
      LLMAG = MAX (1, LMAG - 1)
      IF (LMAG.EQ.1) THEN
         CALL YZOOMC (0, KX0, KY0, F, IERR)
         ROUTIN = 'YZOOMC'
         IF (IERR.NE.0) GO TO 900
         END IF
      RSZX = WINDTV(3) - WINDTV(1) + 1
      RSZY = WINDTV(4) - WINDTV(2) + 1
C                                       init timing parameters
      RS = 0.33 * RSZX
      RRATE = 32.0
      IX = RS * (2.0 - LOG10 (RRATE)) + WINDTV(1) - 1.0
      IY = RSZY / 4.0 + WINDTV(2) - 1.0
      ID = 1
      ONEFRM = F
      XP = REAL (LFR - IFR + 1) / RSZX
C                                       Init enhancement parms
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      RBPOS(1) = 0.0
      RBPOS(2) = 0.0
      RPOS(1) = 7.0 + WINDTV(1)
      RPOS(2) = WINDTV(4) - 7.0
      ITRTYP = 2
      NLEVS = LUTOUT + 1
      IF (NLEVS.GT.OFMINP) NLEVS = OFMINP + 1
      SWITCH = T
C                                       Instructions
      WRITE (MSGTXT,1020)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1021)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1022)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1023)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1024)
      CALL MSGWRT (1)
C                                       On cursor, init frame
      J = IFR
      OIPL = 0
      L1 = T
      L2 = T
      L3 = T
      N = 0
      CALL YCRCTL ('WRIT', L1, IX, IY, L2, L3, N, IBUT, T, IERR)
      ROUTIN = 'YCRCTL'
      IF (IERR.NE.0) GO TO 900
      LX = IX
      CALL ZDATE (ITIM(1))
      CALL ZTIME (ITIM(4))
      CALL DAT2JD (ITIM, JD0)
      NFRAME = 0
      GO TO 120
C                                       STILL frame: wait for button
 100  CALL YCRCTL ('READ', L1, IX, IY, L2, L3, N, IBUT, T, IERR)
         ROUTIN = 'YCRCTL'
         IF (IERR.NE.0) GO TO 900
         IF (IBUT.GT.0) LX = IX
         IF (IBUT.GT.7) GO TO 900
         IF (IBUT.GT.0) THEN
            CALL YWINDO ('READ', WINDTV, IERR)
            ROUTIN = 'YWINDO'
            IF (IERR.NE.0) GO TO 900
            RSZX = WINDTV(3) - WINDTV(1) + 1
            RSZY = WINDTV(4) - WINDTV(2) + 1
            RS = 0.33 * RSZX
            XP = REAL (LFR - IFR + 1) / RSZX
            END IF
         IF (IBUT.GE.4) GO TO 300
         IF (IBUT.GE.2) ID = -ID
         IF (IBUT.EQ.1) THEN
            ONEFRM = F
            CALL ZDATE (ITIM(1))
            CALL ZTIME (ITIM(4))
            CALL DAT2JD (ITIM, JD0)
            NFRAME = 0
            GO TO 120
            END IF
         LX = IX
         JP = XP * (IX - WINDTV(1))  +  IFR
C                                       delay on no cursor move
         IF (JP.NE.J) GO TO 110
            CALL ZDELAY (CURDEL, IERR)
            GO TO 100
C                                       select new frame
 110     J = JP
         GO TO 120
C                                       Movie or single frame:
C                                       Select the frame
 120  CONTINUE
         IPL = (J - 1) / MP + ICHAN
         JX = MOD(J-1,MAG) + 1
         JY = (J-1)/MAG + 1 - ((J-1)/MP)*MAG
         IF (JTY.EQ.1) GO TO 122
            JR = (J-1)/MAG + 1 + JR0
            IF (MOD(JR,2).EQ.0) JX = MAG + 1 - JX
            JY = JR - ((JR-1)/MAG) * MAG
            IF (MOD(IPL,2).EQ.0) JY = MAG + 1 - JY
 122     IX0 = 1 + (MX * (JX - 1) * LMAG) / LLMAG
         IY0 = MAXXTV(2) - (MY * (JY - 1) * LMAG) / LLMAG
         IF ((M70E) .AND. (MAG.EQ.8)) IY0 = IY0 + COR8(JY)
         IF ((M70E) .AND. (MAG.EQ.4)) IY0 = IY0 + COR4(JY)
         XSC = - MX * (JX - 1) - 1
         YSC = MY * (JY - 1) + 1
C                                       requires a frame switch
         IF (IPL.NE.OIPL) THEN
            CALL YHOLD ('ONNN', IERR)
            CALL FILL (4, CHMASK(IPL), ICH)
C                                       on end - jump planes
            IF ((J*ID.EQ.IFR) .OR. (J*ID.EQ.-LFR) .OR. (ONEFRM) .OR.
     *         (LX0.NE.IX0) .OR. (LY0.NE.IY0)) THEN
               CALL YSPLIT ('WRIT', KX0, KY0, ZCH, ZCH, ZCH, T, IERR)
               ROUTIN = 'YSPLIT'
               IF (IERR.NE.0) GO TO 900
               IF (LMAG.GT.1) THEN
                  CALL YZOOMC (KMAG, IX0, IY0, F, IERR)
                  ROUTIN = 'YZOOMC'
               ELSE
                  CALL YSCROL (CHMASK(IPL), XSC, YSC, T, IERR)
                  ROUTIN = 'YSCROL'
                  END IF
               IF (IERR.NE.0) GO TO 900
               LX0 = IX0
               LY0 = IY0
            ELSE IF (LMAG.EQ.1) THEN
               CALL YSCROL (CHMASK(IPL), XSC, YSC, T, IERR)
               ROUTIN = 'YSCROL'
               END IF
            CALL YSPLIT ('WRIT', KX0, KY0, ICH, ICH, ICH, T, IERR)
            ROUTIN = 'YSPLIT'
            OIPL = IPL
            CALL YHOLD ('OFFF', IERR)
C                                       requires shift in same plane
         ELSE
            IF (LMAG.GT.1) THEN
               CALL YZOOMC (KMAG, IX0, IY0, T, IERR)
               ROUTIN = 'YZOOMC'
            ELSE
               CALL YSCROL (CHMASK(IPL), XSC, YSC, T, IERR)
               ROUTIN = 'YSCROL'
               END IF
            LX0 = IX0
            LY0 = IY0
            END IF
         IF (IERR.NE.0) GO TO 900
C                                       MOVIE part: delay by frame time
         IF (ONEFRM) GO TO 100
         NFRAME = NFRAME + 1
         RRATE = (RRATE - 0.5) / 100.0
         IF (TIMEIT) RRATE = -1.
         IF (RRATE.GT.0) CALL ZDELAY (RRATE, IERR)
C                                       Read cursor - one refresh
         CALL YCRCTL ('READ', L1, IX, IY, L2, L3, N, IBUT, T, IERR)
         ROUTIN = 'YCRCTL'
         IF (IERR.NE.0) GO TO 900
         IF (IBUT.GT.0) THEN
            LX = IX
            CALL ZDATE (ITIM(1))
            CALL ZTIME (ITIM(4))
            CALL DAT2JD (ITIM, JD1)
            JD0 = 24.0D0 * 3600.0D0 * (JD1 - JD0)
            FRATE = NFRAME / MAX (1.0D0, JD0)
            JD0 = JD1
            NFRAME = 0
            WRITE (MSGTXT,1122) FRATE
            IF (TIMEIT) CALL MSGWRT (3)
            END IF
C                                       Check buttons
         IF (IBUT.GT.7) GO TO 900
         IF (IBUT.GT.0) THEN
            CALL YWINDO ('READ', WINDTV, IERR)
            ROUTIN = 'YWINDO'
            IF (IERR.NE.0) GO TO 900
            RSZX = WINDTV(3) - WINDTV(1) + 1
            RSZY = WINDTV(4) - WINDTV(2) + 1
            RS = 0.33 * RSZX
            XP = REAL (LFR - IFR + 1) / RSZX
            END IF
         IF (IBUT.GE.4) GO TO 300
         IF (IBUT.GE.2) ID = -ID
         IF (IBUT.EQ.1) ONEFRM = T
         IF (IBUT.EQ.1) GO TO 100
C                                       Reset rate
         LX = IX
         RRATE = 10.0 ** (2.0 - (IX - WINDTV(1) + 1.0) / RS)
C                                       Movies and stills (again)
C                                       select next frame
 200  CONTINUE
         J = J + ID
C                                       off begin end
         IF (J.GE.IFR) GO TO 210
            IF (DOREV) GO TO 205
               J = LFR
               GO TO 120
 205        CONTINUE
               ID = -ID
               J = IFR + 1
               GO TO 120
C                                       off end end
 210     IF (J.LE.LFR) GO TO 120
            IF (DOREV) GO TO 215
               J = IFR
               GO TO 120
 215        CONTINUE
               ID = -ID
               J = LFR - 1
               GO TO 120
C                                       Enhancement section
 300  IF (SWITCH) ITRTYP = MOD (ITRTYP, 2) + 1
C                                       Black & white: clear color
      IF (ITRTYP.NE.2) THEN
         I = OFMINP + 1
         CALL RFILL (I, 0., RBUF)
         OFFS = 1.0 / REAL (OFMOUT)
         X = OFFS * (OFMOUT+1.0) / REAL (NLEVS)
         DO 310 I = 1,NLEVS
            RBUF(I) = X * I - OFFS
 310        CONTINUE
         I = OFMINP + 1
         I = I / NLEVS
         JJ = NLEVS
         DO 311 II = 2,I
            CALL RCOPY (NLEVS, RBUF, RBUF(JJ+1))
            JJ = JJ + NLEVS
 311        CONTINUE
         CALL YOFM ('WRIT', 7, F, RBUF, IERR)
         ROUTIN = 'YOFM'
         IF (IERR.NE.0) GO TO 900
         PPOS(1) = 0.
         PPOS(2) = 0.
         WRITE (MSGTXT,1310)
         CALL MSGWRT (1)
         WRITE (MSGTXT,1311)
         CALL MSGWRT (1)
         WRITE (MSGTXT,1312)
         CALL MSGWRT (1)
         IBUT = 2
         CALL IENHNS (CHTMSK, 7, IBUT, RBPOS, INBUF, IERR)
         ROUTIN = 'IENHNS'
         IF (IERR.NE.0) GO TO 900
         CALL YCRCTL ('WRIT', L1, IX, IY, L2, L3, N, IBUT, T, IERR)
         ROUTIN = 'YCRCTL'
         IF (IERR.NE.0) GO TO 900
C                                       Color: clear LUTs
      ELSE
         IT = MAXINT + 1
         X = (LUTOUT+1.0) / (MAXINT+1.0)
         DO 330 I = 1,IT
            INBUF(I) = I * X - 0.5
 330        CONTINUE
         CALL YHOLD ('ONNN', IERR)
         CALL YLUT ('WRIT', CHTMSK, 7, F, INBUF, IERR)
         ROUTIN = 'YLUT'
         IF (IERR.NE.0) GO TO 900
         CALL YHOLD ('OFFF', IERR)
         CALL ZTIME (ITW)
         WRITE (MSGTXT,1330)
         CALL MSGWRT (1)
         WRITE (MSGTXT,1312)
         CALL MSGWRT (1)
         CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
 340     CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
            ROUTIN = 'YCURSE'
            IF (IERR.NE.0) GO TO 900
C                                       revise color
            IF (IBUT.LE.0) THEN
               CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
               IF (DOIT) THEN
                  ITYPE = 0
                  PGAMMA = 1.0
                  ICONT = ((RPOS(1) - WINDTV(1) + 1.) / RSZX) * NLEVS
                  ICONT = MAX (0, ICONT)
                  NCONT = NLEVS * (RPOS(2) - WINDTV(2) + 1.0) / RSZY
                  CALL COLORC (ITYPE, NLEVS, ICONT, NCONT, PGAMMA,
     *               RBUF, IERR)
                  ROUTIN = 'COLORC'
                  IF (IERR.NE.0) GO TO 900
                  END IF
               GO TO 340
            ELSE
               CALL YWINDO ('READ', WINDTV, IERR)
               ROUTIN = 'YWINDO'
               IF (IERR.NE.0) GO TO 900
               RSZX = WINDTV(3) - WINDTV(1) + 1
               RSZY = WINDTV(4) - WINDTV(2) + 1
               RS = 0.33 * RSZX
               XP = REAL (LFR - IFR + 1) / RSZX
               END IF
         END IF
C                                       respond to buttons
      SWITCH = IBUT.LE.7
      IF (SWITCH) GO TO 300
C                                       back to movie or stills
      WRITE (MSGTXT,1020)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1021)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1022)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1023)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1024)
      CALL MSGWRT (1)
      IF (ONEFRM) GO TO 100
      CALL ZDATE (ITIM(1))
      CALL ZTIME (ITIM(4))
      CALL DAT2JD (ITIM, JD0)
      NFRAME = 0
      GO TO 200
C                                       DONE
 900  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1900) ROUTIN, IERR
         CALL MSGWRT (7)
         END IF
C                                       set channel select parms
      IF (OIPL.GT.0) CALL FILL (4, CHMASK(OIPL), TVLIMG)
C                                       off cursor
      CALL YHOLD ('ONNN', JERR)
      CALL YCRCTL ('WRIT', L1, IX, IY, L2, L3, N, IBUT, T, JERR)
C                                       restore the windows
      IF (WRKSTN) THEN
         CALL YWINDO ('WRIT', TWIN, JERR)
         CALL YWINDO ('READ', WINDTV, JERR)
         END IF
C                                       restore zoom and scroll
      CALL YZOOMC (TVZINI(1), TVZINI(2), TVZINI(3), T, JERR)
      IPL = (LFR - 1) / MP + ICHAN
      DO 910 I = ICHAN,IPL
         CALL YSCROL (CHMASK(I), SAVSCX(I), SAVSCY(I), T, JERR)
 910     CONTINUE
      CALL YHOLD ('OFFF', JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Cursor X position controls frame rate or selection')
 1021 FORMAT ('Press button A to start or stop the movie')
 1022 FORMAT ('Press button B to switch the direction of the movie')
 1023 FORMAT ('Press button C to enhance the image (color or B&W)')
 1024 FORMAT ('Press button D to exit')
 1122 FORMAT ('TVMOVI: Display rate',F7.2,' frames/sec')
 1310 FORMAT ('Press button A or B to switch to color')
 1311 FORMAT ('Press button C to reverse sign of slope')
 1312 FORMAT ('Press button D to resume the movie mode')
 1330 FORMAT ('Press button A, B, or C to switch to black and white')
 1900 FORMAT ('TVMOVI: ROUTINE ',A,' RETURNS ERROR CODE',I5)
      END
