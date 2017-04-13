      PROGRAM PLCUB
C-----------------------------------------------------------------------
C! Plots intensity vs x for a grid in y and z.
C# Plot-appl Map Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2002-2003, 2009, 2012, 2014-2015
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
C   Plots intensity as a function of the variable along the first axis
C   in panels, which are positioned according to the coordinate values
C   along axes # 2 and # 3.
C      Adverb    in program     description
C      USERID       USER        USER ID. ignored
C      INNAME        NAM        Image name (name)
C      INCLASS       CLS        Image name (class)
C      INSEQ        SQIN        Image name (seq. #)
C      INDISK       DKIN        Disk drive #
C      BLC           BLC        Bottom left corner image. 0=> all
C      TRC           TRC        Top right corner image.   0=> all
C      LTYPE        TYPE        Type of labeling. See PLCUB.HLP
C      APARM       APARM        Plotting parameters:
C                               (1) Number panels in x direction
C                               (2) Number panels in y direction
C                               (3) label type for panels
C                               (4) y value (map units) for x panel axis
C                               (5) x value (x units) for y panel axis
C                               (6) No. of points in Hanning smooth of
C                                   x-axis.   <=3 ==> no smooth, must be
C                                   odd integer.
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C-----------------------------------------------------------------------
C                                    max. no of points in Hanning smooth
C                                    kernel, MUST be odd
      INTEGER MAXCO
      PARAMETER (MAXCO = 15)
      INCLUDE 'INCS:PMAD.INC'
C
      CHARACTER PHNM*48, TEXT(2)*80, MTYPE*2, NAM*12, CLS*6, PGMNAM*6,
     *   STAT*4
      HOLLERITH XNAM(3), XCLS(2)
      INTEGER   VOL, SEQ, USR, LUN, IND, CNR, CATBLK(256), SCR(256),
     *   IER, NAX, PAX(7), DPT(5), BOF, WIN(4), GFIND, IBF(MABFSS), NBF,
     *   JTOT, KTOT, JP, KP, JPAG, KPAG, NPARM, IRET, IT, JT, KT,
     *   LABEL, VER, GLUN, LTP, I, NTEXT, KI, JI, IROUND, IYINC, IZINC,
     *   IPTYPE, IERR, NSMTH, GRCHN, TVCHN, TVCORN(4), PLTYPE
      REAL      CATR(256), RBF(MABFSS), USER, SQIN, DKIN, BLC(7),
     *   TRC(7), LP(2), RP(2), CHOUT(4), TYPE, AMX, AMN, APARM(10),
     *   LC(7), RC(7), UNITY, ARR(MAXIMG+8), YINC, ZINC, PIXR(2),
     *   RANGE(2), MBLC(7), MTRC(7), XTRA1, XTRA2, XTRA3, XTRA4, YGAP,
     *   ZOFF, ARRS(MAXIMG+8), COEFFS(MAXCO), XDOTV, XGRCH
      DOUBLE PRECISION    CATD(128), YX, XY
      LOGICAL   T, F, DONCR, SLICE, FBL, DOTV, FRLABL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DGPH.INC'
      COMMON /MAPHDR/ CATBLK
      COMMON /INPARM/ USER, XNAM, XCLS, SQIN, DKIN, BLC, TRC, YINC,
     *   ZINC, PIXR, TYPE, APARM, XDOTV, XGRCH
      EQUIVALENCE (CATBLK, CATR, CATD)
      EQUIVALENCE (IBF(1), RBF(1))
      DATA UNITY /1./
      DATA T, F /.TRUE.,.FALSE./
      DATA PGMNAM / 'PLCUB '/
      DATA LUN /16/
C-----------------------------------------------------------------------
C                                        fill common IDCH
C                                        enter parameter values
      DONCR = F
      NPARM = 39
      CALL SETUP (PGMNAM, NPARM, USER, SCR, IRET)
      IF (IRET.NE.0) GO TO 990
      IRET = 8
      NBF = MABFSS * 2
C                                        interpret input
      CALL H2CHR (12, 1, XNAM, NAM)
      CALL H2CHR (6, 1, XCLS, CLS)
      SEQ  = SQIN + 0.01
      VOL  = DKIN + 0.01
      USER = NLUSER
      USR  = NLUSER
      LTP  = IROUND (TYPE)
      I = MOD (ABS(LTP), 100)
      IF ((I.EQ.0) .OR. (I.GT.10)) THEN
         IF (LTP.LT.0) THEN
            LTP = (LTP/100) * 100 - 3
         ELSE
            LTP = (LTP/100)*100 + 3
            END IF
         END IF
      TYPE = LTP
      JTOT = APARM(1) + 0.01
      KTOT = APARM(2) + 0.01
      IYINC = YINC + 0.01
      IF (IYINC.LE.0) IYINC = 1
      YINC = IYINC
      IZINC = ZINC + 0.01
      IF (IZINC.LE.0) IZINC = 1
      ZINC = IZINC
      LABEL = IROUND (APARM(3))
      I = MOD (ABS(LABEL), 100)
      FRLABL = I.LT.11
      IF ((I.EQ.0) .OR. (I.GT.10)) LABEL = LTP
      APARM(3) = LABEL
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
C                                        generate Hanning smooth coeffs
      NSMTH = IROUND (APARM(6))
      IF (NSMTH.LE.2) THEN
        NSMTH = 1
      ELSE IF (NSMTH.GT.MAXCO) THEN
        NSMTH = MAXCO
        WRITE (MSGTXT,400) NSMTH
        CALL MSGWRT (8)
      ELSE IF (MOD(NSMTH,2).EQ.0) THEN
        NSMTH = NSMTH + 1
        WRITE (MSGTXT,500) NSMTH
        CALL MSGWRT (8)
        END IF
      IF (NSMTH.GE.3) CALL HCOEFF (NSMTH, COEFFS)
C                                        open map for read
      MTYPE = 'MA'
      STAT = 'HDWR'
      IF (DOTV) STAT = 'READ'
      CALL MAPOPN (STAT, VOL, NAM, CLS, SEQ, MTYPE, USR, LUN, IND,
     *   CNR, CATBLK, SCR, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1000) IER
         CALL MSGWRT (8)
         GO TO 990
         END IF
      NCFILE = 1
      FVOL(1) = VOL
      FCNO(1) = CNR
      FRW(1) = 1
      IF (DOTV) FRW(1) = 0
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IER)
      IF (IER.NE.0) GO TO 990
      SQIN = SEQ
      DKIN = VOL
      CALL RNGSET (PIXR, CATR(KRDMX), CATR(KRDMN), RANGE)
      PIXR(1) = RANGE(1)
      AMN = PIXR(1)
      PIXR(2) = RANGE(2)
      AMX = PIXR(2)
C                                        interpret header contents
      NAX = CATBLK(KIDIM)
C                                        fill some arrays
      DO 15 I = 1,NAX
         PAX(I) = CATBLK(KINAX+I-1)
 15      CONTINUE
      DO 20 I = 3,7
         DPT(I-2) = BLC(I)
 20      CONTINUE
C                                       I/O windows
      WIN(1) = BLC(1) + 0.001
      WIN(2) = BLC(2) + 0.001
      WIN(3) = TRC(1) + 0.001
      WIN(4) = TRC(2) + 0.001
C                                       total # panels in x, y
      KT = TRC(3) - BLC(3) + 1.001
      JT = TRC(2) - BLC(2) + 1.001
      IT = TRC(1) - BLC(1) + 1.001
      KT = (KT - 1) / IZINC + 1
      TRC(3) = BLC(3) + (KT - 1) * IZINC
      JT = (JT - 1) / IYINC + 1
      TRC(2) = BLC(2) + (JT - 1) * IYINC
C                                       # panels/page in x, y
      IF (JTOT.LE.0) JTOT = MIN (5, JT)
      IF (KTOT.LE.0) KTOT = JTOT
      JTOT = MIN (JTOT, JT)
      KTOT = MIN (KTOT, KT)
      APARM(1) = JTOT
      APARM(2) = KTOT
C                                        # pages in x, y
      JPAG = (JT - 1) / JTOT + 1
      KPAG = (KT - 1) / KTOT + 1
C                                        y-value x-axis
      YX = APARM(4)
      XY = APARM(5)
      IF (AMX.LT.YX) AMX = YX
      IF (AMN.GT.YX) AMN = YX
      CALL RCOPY (7, BLC, MBLC)
      CALL RCOPY (7, TRC, MTRC)
      ZOFF = ((YX - AMN) / (AMX - AMN)) * 0.9 * IZINC
      XTRA1 = 0.035 * JTOT * IYINC
      XTRA2 = 0.035 * KTOT * IZINC + ZOFF
      XTRA3 = (0.9 + 0.035 * JTOT) * IYINC
      XTRA4 = (0.9 + 0.035 * KTOT) * IZINC - ZOFF
      PLTYPE = MOD (ABS(LABEL), 100)
      IF (PLTYPE.EQ.2) THEN
         XTRA1 = XTRA1 + 0.0405 * JTOT * IYINC
         XTRA2 = XTRA2 + 0.0405 * KTOT * IZINC
      ELSE IF (PLTYPE.GT.2) THEN
         XTRA1 = XTRA1 + 0.0810 * JTOT * IYINC
         XTRA2 = XTRA2 + 0.0405 * KTOT * IZINC
         END IF
C                                        end of general initialization
C                                        start individual pages of plots
      DO 230 KP = 1,KPAG
C
         LC(3) = MBLC(3) + (KP-1) * KTOT * IZINC
         RC(3) = LC(3) + (KTOT - 1) * IZINC
         IF (RC(3).GT.MTRC(3)) LC(3) = LC(3) - RC(3) + MTRC(3)
         IF (RC(3).GT.MTRC(3)) RC(3) = MTRC(3)
         KT = RC(3) - LC(3) + 1.001
C
         DO 220 JP = 1,JPAG
C
            LC(2) = MBLC(2) + (JP-1) * JTOT * IYINC
            RC(2) = LC(2) + (JTOT - 1) * IYINC
            IF (RC(2).GT.MTRC(2)) LC(2) = LC(2) - RC(2) + MTRC(2)
            IF (RC(2).GT.MTRC(2)) RC(2) = MTRC(2)
            JT = RC(2) - LC(2) + 1.001
            WIN(2) = LC(2) + 0.001
            WIN(4) = RC(2) + 0.001
C                                       actual plot corners
            BLC(1) = WIN(2) - XTRA1
            BLC(2) = LC(3) - XTRA2
            TRC(1) = WIN(4) + XTRA3
            TRC(2) = RC(3) + XTRA4
C                                       avoid single pixel
 101        GPHIX1 = BLC(1) + 0.999
            GPHIX2 = TRC(1) + 0.001
            IF (GPHIX1.EQ.GPHIX2) THEN
               BLC(1) = BLC(1) - 0.1
               TRC(1) = TRC(1) + 0.1
               GO TO 101
               END IF
 102        GPHIY1 = BLC(2) + 0.999
            GPHIY2 = TRC(2) + 0.001
            IF (GPHIY1.EQ.GPHIY2) THEN
               BLC(2) = BLC(2) - 0.1
               TRC(2) = TRC(2) + 0.1
               GO TO 102
               END IF
            BLC(3) = MBLC(1)
            TRC(3) = MTRC(1)
C                                       add plot file to header
            VER = 0
            IF (.NOT.DOTV) THEN
               CALL MADDEX ('PL', VOL, CNR, CATBLK, SCR, T, 'WRIT', VER,
     *            IER)
               IF (IER.NE.0) THEN
                  WRITE (MSGTXT,1100) IER
                  CALL MSGWRT (6)
                  NCFILE = 0
                  GO TO 990
                  END IF
               END IF
C                                        physical plot file name
            CALL ZPHFIL ('PL', VOL, CNR, VER, PHNM, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,1110) IER
               CALL MSGWRT (5)
               GO TO 990
               END IF
C                                       swap axes
            CALL CYCPER
C                                       set labeling for page
            SLICE = .FALSE.
            CALL RFILL (4, 0.0, CHOUT)
            DPT(1) = (MTRC(1) + MBLC(1)) / 2.0
            LOCNUM = 1
            CALL LABINI (BLC, TRC, DPT, CHOUT, LTP, SLICE, YGAP, TEXT,
     *         NTEXT)
C                                       open plot file
            IPTYPE = 13
            CALL GINIT (VOL, CNR, PHNM, 0, IPTYPE, NPARM, USER, DOTV,
     *         TVCHN, GRCHN, TVCORN, CATBLK, SCR, GLUN, GFIND, IER)
            DONCR = IER.EQ.0
            IF (IER.EQ.0) CALL GINITL (BLC, TRC, UNITY, CHOUT, DPT, SCR,
     *         IER)
            IF (IER.EQ.0) CALL GLTYPE (1, SCR, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,1140) IER
               CALL MSGWRT (5)
               GO TO 980
               END IF
C                                       label page plot
            CALL LAB (BLC, TRC, LTP, TEXT, NTEXT, VER, CHOUT, YGAP,
     *         SCR, IER)
C                                       put axes back
            CALL CYCPER
            CALL CYCPER
C                                       go along third axis
            DO 170 KI = 1,KT,IZINC
               DPT(1) = LC(3) + (KI - 1)
               CALL COMOFF (NAX, PAX, DPT, BOF, IER)
               BOF = BOF + 1
               CALL MINIT ('READ', LUN, IND, PAX(1), PAX(2), WIN, IBF,
     *            NBF, BOF, IER)
               IF (IER.NE.0) GO TO 980
C                                        handle one plane
               LP(2) = LC(3) + (KI - 1) - ZOFF
               RP(2) = LP(2) + 0.9 * IZINC
               DO 160 JI = 1,JT
C                                       read one line
                  CALL LINIO ('READ', LUN, IND, RBF, IT, ARR, 1, FBLANK,
     *                IER)
                  IF (IER.NE.0) GO TO 980
                  IF (MOD(JI-1,IYINC).NE.0) GO TO 160
                  LP(1) = LC(2) + (JI - 1)
                  RP(1) = LP(1) + 0.9 * IYINC
C                                       ticks, axes current panel
                  IF (FRLABL) THEN
                     CALL FRAM (LP, RP, ZOFF, PIXR, MBLC, MTRC, JI, KI,
     *                  LABEL, YX, XY, SCR, IER)
                     IF (IER.NE.0) GO TO 980
                     END IF
C                                       Optionally smooth and plot data
                  IF (NSMTH.GE.3) CALL HANNSM (NSMTH, COEFFS, IT, ARR,
     *                                         ARRS, FBL)
                  IF ( (NSMTH.GE.3) .AND. (.NOT.FBL)) THEN
                     CALL PAR (LP, RP, IT, PIXR, ARRS, FBLANK, SCR, IER)
                  ELSE
                     CALL PAR (LP, RP, IT, PIXR, ARR, FBLANK, SCR, IER)
                     END IF
                  IF (IER.NE.0) THEN
                     WRITE (MSGTXT,1160) IER
                     CALL MSGWRT (5)
                     GO TO 980
                     END IF
 160              CONTINUE
C                                       go to next plane
 170           CONTINUE
C                                       close down this plot
            GPHPAG = (JP.LT.JPAG) .OR. (KP.LT.KPAG)
            CALL GFINIS (SCR, IER)
            IF (IER.LT.0) GO TO 240
            IF (IER.EQ.0) GO TO 210
               WRITE (MSGTXT,1200) IER
               CALL MSGWRT (5)
               GO TO 980
 210        IF (.NOT.DOTV) THEN
               WRITE (MSGTXT,1210) VER
               CALL MSGWRT (2)
               CALL HIPLOT (FVOL(1), FCNO(1), VER, SCR, IERR)
               END IF
            DONCR = .FALSE.
C
 220        CONTINUE
 230     CONTINUE
C                                       finished with cube
 240  IRET = 0
      GO TO 990
C                                       error
 980  IF ((DONCR) .AND. (.NOT.DOTV)) THEN
         CALL DELEXT ('PL', FVOL(1), FCNO(1), 'WRIT', CATBLK, SCR, VER,
     *      IER)
         CALL ZCLOSE (GLUN, GFIND, IER)
         CALL ZDESTR (VOL, PHNM, IER)
         NCFILE = NCFILE - 1
         END IF
C
 990  CALL DIE (IRET, SCR)
 999  STOP
C-----------------------------------------------------------------------
 400  FORMAT ('DECREASING HANNING SMOOTHING LENGTH TO MAXIMUM = ', I3)
 500  FORMAT ('INCREASING HANNING SMOOTHING LENGTH TO ', I3)
 1000 FORMAT ('MAPOPN: ERROR # ',I5)
 1100 FORMAT ('MADDEX: ERROR # ',I5)
 1110 FORMAT ('ZPHFIL: ERROR # ',I5)
 1140 FORMAT ('GINITL: ERROR # ',I5)
 1160 FORMAT ('PLOT X: ERROR # ',I5)
 1210 FORMAT ('Created PLot file version',I4)
 1200 FORMAT ('GFINIS: ERROR # ',I5)
      END
      SUBROUTINE LAB (BLC, TRC, ILTYPE, TEXT, NTEXT, IVER, CH, YGAP,
     *   IBUFF, IERR)
C-----------------------------------------------------------------------
C   LAB is a labeling routine, labeling the x- and y-axes either with
C   the coordinate designator or with the appropriate units + prefix.
C   Inputs:  BLC      R(2)   X and Y pixels to form bottom left hand
C                     corner of the graph.
C            TRC      R(2)   X and Y pixels to form the top right hand
C                     corner of the graph.
C            LTYPE    I   label type: 1 none, 2 no ticks, 3 RA/DEC
C                         4 center relative
C            TEXT     C(2)*80    lower line text
C            NTEXT    I          number lines lower text
C            IVER     I          plot file version number
C   In/out:  YGAP     R          place to put lower lines
C            IBUFF    I(256)   the updated graphics output buffer.
C            IERR     I   error indicator:
C                         0 = No error.
C-----------------------------------------------------------------------
      CHARACTER TEXT(2)*80, ATIME*8, ADATE*12, CHTM18*18
      HOLLERITH CATH(256)
      INTEGER   ILTYPE, IBUFF(256), IERR, NTEXT, IVER, ITCHAR
      REAL      BLC(2), TRC(2), YGAP, CH(4), CATR(256)
      REAL      DCX, DCY, UNITY
      INTEGER   ID(3), IT(3), I, IANGL, INCHAR, LTYPE, CATBLK(256)
      DOUBLE PRECISION    CATD(128)
      LOGICAL   F
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA UNITY /1.0/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      LTYPE = MOD (ABS (ILTYPE), 100)
C                                       Draw borders.
      CALL GPOS (BLC(1), BLC(2), IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (TRC(1), BLC(2), IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (TRC(1), TRC(2), IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (BLC(1), TRC(2), IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (BLC(1), BLC(2), IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (LTYPE.EQ.1) GO TO 999
C                                       Axis labels, ticks
      CALL CLAB1 (BLC, TRC, CH, ILTYPE, UNITY, F, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Extra lines (center rels)
      IF ((NTEXT.GT.0) .AND. (LTYPE.LT.7)) THEN
         DCX = 0.0
         IANGL = 0
         DO 10 I = 1,NTEXT
            CALL GPOS (BLC(1), BLC(2), IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
            DCY = -YGAP
            CALL CHTRIM (TEXT(I), 80, TEXT(I), INCHAR)
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, TEXT(I), IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
            YGAP = YGAP + 1.333
 10         CONTINUE
         END IF
C                                       Source name, stokes, freq.
      IF (LTYPE.LT.7) THEN
         CALL GPOS (BLC(1), TRC(2), IBUFF, IERR)
         IF (IERR.NE.0) GO TO 980
         DCX = 0.0
         DCY = 0.5
         IANGL = 0
         CALL H2CHR (8, 1, CATH(KHOBJ), TEXT(1))
         INCHAR = 11
         IF (TEXT(1).EQ.' ') INCHAR = 1
         IF (NCHLAB(1,LOCNUM).GT.0) THEN
            IF (INCHAR.GT.1) TEXT(1)(INCHAR-1:INCHAR-1) = '_'
            TEXT(1)(INCHAR:) = SAXLAB(1,LOCNUM)(1:NCHLAB(1,LOCNUM))
            INCHAR = INCHAR + 3 + NCHLAB(1,LOCNUM)
            END IF
         IF (NCHLAB(2,LOCNUM).GT.0) THEN
            IF (INCHAR.GT.1) TEXT(1)(INCHAR-1:INCHAR-1) = '_'
            TEXT(1)(INCHAR:) = SAXLAB(2,LOCNUM)(1:NCHLAB(2,LOCNUM))
            INCHAR = INCHAR + 3 + NCHLAB(2,LOCNUM)
            END IF
C                                       image name
         IF (INCHAR.GT.1) TEXT(1)(INCHAR-1:INCHAR-1) = '_'
         CALL H2CHR (12, KHIMNO, CATH(KHIMN), CHTM18(1:12))
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTM18(13:18))
         ITCHAR = INCHAR
         CALL NAMEST (CHTM18, CATBLK(KIIMS), TEXT(1)(INCHAR:), ITCHAR)
         CALL REFRMT (TEXT(1), '_', INCHAR)
         CALL GCHAR (INCHAR, IANGL, DCX, DCY, TEXT(1), IBUFF, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Date/time version
         IF (ILTYPE.GT.1) THEN
            CALL GPOS (BLC(1), TRC(2), IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
            DCY = DCY + 1.333
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, ATIME, ADATE)
            WRITE (TEXT(1),1020) IVER, ADATE, ATIME
            CALL REFRMT (TEXT(1), '_', INCHAR)
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, TEXT(1), IBUFF, IERR)
            END IF
         END IF
      IF (IERR.EQ.0) GO TO 999
C                                       Graph drawing error.
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('PLot file version',I4,'__created ',A12,A8)
 1980 FORMAT ('GRAPH LABEL WRITING ERROR. IERR =',I5)
      END
      SUBROUTINE FRAM (LBLC, LTRC, ZOFF, PIXR, MBLC, MTRC, JI, KI,
     *   LABEL, YX, XY, IBUFF, IERR)
C-----------------------------------------------------------------------
C   FRAM writes tick marks and tick labels to a plot file: 1 panel
C   Inputs: LBLC  R(2)     X AND Y pixels to form bottom left hand
C                          corner of the panel
C           LTRC  R(2)     X and Y pixels to form the top right hand
C                          corner of the panel.
C           ZOFF  R        offset in pixels to horiz axis
C           PIXR  R(2)     intensity range
C           MBLC  R(7)     map BLC
C           MTRC  R(7)     map TRC
C           JI    I        column number (left = 1)
C           KI    I        row number  (bottom = 1)
C           LABEL I        requested label type (revised as needed)
C           YX    D        y-value x-axis
C           XY    D        X-value Y-axis
C   In/out: IBUFF I(256)   buffer being used for output to
C                          the graphics file.
C   Output: IERR  I        error code: 0 => ok
C                             2 => graph drawing error
C                             3 => tic algorithm fails
C-----------------------------------------------------------------------
      CHARACTER TEXT(2)*80, PREFIX(11)*5
      DOUBLE PRECISION    YX, XY, AXY, FQFREQ
      REAL      LBLC(2), LTRC(2), ZOFF, PIXR(2), MBLC(7), MTRC(7)
      INTEGER   JI, KI, LABEL, IBUFF(256), IERR
      INTEGER   IDROP(2), INOSL, IDEPTH(5), NTEXT, I, J
      REAL      Y, PBLC(2), PTRC(2), XBLC(7), XTRC(7), YGAP, CH(4),
     *   UNITY, FQFINC
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA UNITY /1.0/
      DATA PREFIX /'FEMTO', 'ATO  ', 'PICO ', 'NANO ', 'MICRO','MILLI',
     *   '     ', 'KILO ', 'MEGA ', 'GIGA ', 'TERA '/
C-----------------------------------------------------------------------
C                                       set up label common: slice
      IDROP(1) = 0
      IDROP(2) = 0
      INOSL = MTRC(1) - MBLC(1) + 1.001
      CALL RCOPY (7, MBLC, XBLC)
      CALL RCOPY (7, MBLC, XTRC)
      XTRC(1) = MTRC(1)
      XBLC(2) = LBLC(1)
      XTRC(2) = LBLC(1)
      XBLC(3) = LBLC(2)
      XTRC(3) = LBLC(2)
      PBLC(2) = LBLC(2)
      PTRC(2) = LTRC(2)
      LOCNUM = 1
      FQFREQ = 0.0D0
      FQFINC = 0.0
      CALL SLBINI (IDROP, INOSL, PIXR, PBLC, PTRC, XBLC, XTRC, FQFREQ,
     *   FQFINC, IDEPTH, LABEL, YGAP, CH, TEXT, NTEXT)
      CALL GLTYPE (1, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       bugger x axis
      AXINC(1,LOCNUM) = AXINC(1,LOCNUM) * (PTRC(1) - PBLC(1)) / (LTRC(1)
     *   - LBLC(1))
      PTRC(1) = LTRC(1)
      PBLC(1) = LBLC(1)
      RPLOC(1,LOCNUM) = (LTRC(1) + LBLC(1)) / 2.0
      AXY = XY
      IF (CPREF(1,LOCNUM).NE.' ') THEN
         J = 0
         DO 10 I = 1,11
            IF (CPREF(1,LOCNUM).EQ.PREFIX(I)) J = I
 10         CONTINUE
         IF (J.GT.0) AXY = AXY * (1000.0D0 ** (7-J))
         END IF
      IF ((JI.NE.1) .OR. (KI.NE.1)) THEN
         CPREF(1,LOCNUM) = ' '
         CTYP(1,LOCNUM) = ' '
         CPREF(2,LOCNUM) = ' '
         CTYP(2,LOCNUM) = ' '
         END IF
      CALL CLAB2 (PBLC, PTRC, CH, LABEL, UNITY, YX, AXY, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       draw 2 axis lines
      Y = LBLC(1)
      IF (AXINC(1,LOCNUM).NE.0) Y = (AXY - RPVAL(1,LOCNUM)) /
     *   AXINC(1,LOCNUM) + RPLOC(1,LOCNUM)
      IF ((Y.LT.LBLC(1)) .OR. (Y.GT.LTRC(1))) Y = LBLC(1)
      CALL GPOS (Y, LBLC(2), IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (Y, LTRC(2), IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      Y = LBLC(2) + ZOFF
      CALL GPOS (LBLC(1), Y, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (LTRC(1), Y, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      GO TO 999
C                                       error
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('FRAM: PLOT ERROR',I6)
      END
      SUBROUTINE PAR (LBLC, LTRC, IT, PIXR, ARR, BL, IBUFF, IERR)
C-----------------------------------------------------------------------
C   PAR plots the row.
C-----------------------------------------------------------------------
      INTEGER   IT, IBUFF(256), IERR, I
      REAL      LBLC(2), LTRC(2), ARR(*), XP, YP, XPX, YPX, BL, PIXR(2),
     *   XS, YS
      LOGICAL   DOWN
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Clipping on intensities
      DO 10 I = 1,IT
         IF (ARR(I).NE.BL) ARR(I) = MAX (PIXR(1), MIN (PIXR(2),
     *      ARR(I)))
 10      CONTINUE
C                                       map pixels=>plot pixels
      XS = (LTRC(1) - LBLC(1)) / (IT - 1)
      XP = LBLC(1)
      YS = (LTRC(2) - LBLC(2)) / (PIXR(2) - PIXR(1))
      YP = LBLC(2)
C
      DOWN = .FALSE.
C
      CALL GLTYPE (2, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 30 I = 1,IT
         IF (ARR(I).EQ.BL) THEN
            DOWN = .FALSE.
         ELSE
            XPX = XS * (I-1) + XP
            YPX = YS * (ARR(I)-PIXR(1)) + YP
            IF (DOWN) THEN
               CALL GVEC (XPX, YPX, IBUFF, IERR)
            ELSE
               CALL GPOS (XPX, YPX, IBUFF, IERR)
               END IF
            IF (IERR.NE.0) GO TO 999
            DOWN = .TRUE.
            END IF
 30      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CYCPER
C-----------------------------------------------------------------------
C   Performs cyclic permutation of header elements pertaining to
C   axes # 1, 2, AND 3.
C-----------------------------------------------------------------------
      INTEGER   CATBLK(256), ITEM(4)
      REAL      CATR(256), RTEM(2)
      HOLLERITH CATH(256), HTEMP(2)
      DOUBLE PRECISION CATD(128), DTEM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      EQUIVALENCE (DTEM, RTEM(1), ITEM(1))
C-----------------------------------------------------------------------
C                                        random parameter types
      HTEMP(1) = CATH(KHPTP)
      HTEMP(2) = CATH(KHPTP+1)
      CATH(KHPTP) = CATH(KHPTP+2)
      CATH(KHPTP+1) = CATH(KHPTP+3)
      CATH(KHPTP+2) = CATH(KHPTP+4)
      CATH(KHPTP+3) = CATH(KHPTP+5)
      CATH(KHPTP+4) = HTEMP(1)
      CATH(KHPTP+5) = HTEMP(2)
C                                        coordinate type
      HTEMP(1) = CATH(KHCTP)
      HTEMP(2) = CATH(KHCTP+1)
      CATH(KHCTP) = CATH(KHCTP+2)
      CATH(KHCTP+1) = CATH(KHCTP+3)
      CATH(KHCTP+2) = CATH(KHCTP+4)
      CATH(KHCTP+3) = CATH(KHCTP+5)
      CATH(KHCTP+4) = HTEMP(1)
      CATH(KHCTP+5) = HTEMP(2)
C                                        coord. value ref. pixel
      DTEM         = CATD(KDCRV)
      CATD(KDCRV)   = CATD(KDCRV+1)
      CATD(KDCRV+1) = CATD(KDCRV+2)
      CATD(KDCRV+2) = DTEM
C                                        coord. value incr. along axis
      RTEM(1)      = CATR(KRCIC)
      CATR(KRCIC)   = CATR(KRCIC+1)
      CATR(KRCIC+1) = CATR(KRCIC+2)
      CATR(KRCIC+2) = RTEM(1)
C                                        coord. ref. pixel
      RTEM(1)      = CATR(KRCRP)
      CATR(KRCRP)   = CATR(KRCRP+1)
      CATR(KRCRP+1) = CATR(KRCRP+2)
      CATR(KRCRP+2) = RTEM(1)
C                                        coord. rotation angles
      RTEM(1)      = CATR(KRCRT)
      CATR(KRCRT)   = CATR(KRCRT+1)
      CATR(KRCRT+1) = CATR(KRCRT+2)
      CATR(KRCRT+2) = RTEM(1)
C                                        # pixels on each axis
      ITEM(1)      = CATBLK(KINAX)
      CATBLK(KINAX)   = CATBLK(KINAX+1)
      CATBLK(KINAX+1) = CATBLK(KINAX+2)
      CATBLK(KINAX+2) = ITEM(1)
C
 999  RETURN
      END
      SUBROUTINE HCOEFF (NSMTH, COEFFS)
C-----------------------------------------------------------------------
C   Work out binomial weights or coefficients
C   Input:
C      NSMTH   I    Number of points to smooth over
C   Output:
C      COEFFS  R    Weights for data points
C-----------------------------------------------------------------------
      INTEGER NSMTH
      REAL COEFFS(NSMTH)
      INTEGER N, K, I
      REAL NFAC, NKFAC, KFAC, SUM
C-----------------------------------------------------------------------
      N = NSMTH - 1
      CALL FACT (N, NFAC)
C
      SUM = 0.0
      DO 100 I = 1, NSMTH
         K = I - 1
         CALL FACT (N-K, NKFAC)
         CALL FACT (K, KFAC)
         COEFFS(I) = NFAC / (NKFAC * KFAC)
         SUM = COEFFS(I) + SUM
 100     CONTINUE
C
      DO 200 I = 1, NSMTH
         COEFFS(I) = COEFFS(I) / SUM
 200     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FACT (N, NFAC)
C-----------------------------------------------------------------------
C   Find n!  use real numbers so can have reasonable sized arguments
C   Input:
C      N      I     Number to find the factorial of
C   Output:
C      NFAC   R     Results
C-----------------------------------------------------------------------
      INTEGER N, I
      REAL NFAC
C-----------------------------------------------------------------------
      NFAC = 1.0
      I = N
C
      IF (I.EQ.0) THEN
         GO TO 999
      ELSE
 100     IF (I.EQ.1) THEN
            GO TO 999
         ELSE
            NFAC = I * NFAC
            I = I - 1
            END IF
         GO TO 100
         END IF
C
 999  RETURN
      END
      SUBROUTINE HANNSM (NSMTH, COEFFS, IT, ARR, ARRS, FBL)
C-----------------------------------------------------------------------
C   Hanning smooth array
C   Input:
C      NSMTH    I    Number of points to smooth over
C      COEFFS   R    Weights
C      IT       I    Number of data points
C      ARR      R    Data array
C   Output:
C      ARRS     R    Smoothed array
C      FBL      L    True if a blank was in this array, the
C                       unsmoothed data are plotted in this case.
C-----------------------------------------------------------------------
      INTEGER IT, NSMTH, IEND, I, J, K, IBL
      LOGICAL FBL
      REAL ARR(IT), ARRS(IT), COEFFS(NSMTH), SUM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IBL /0/
C-----------------------------------------------------------------------
      IEND = (NSMTH - 1) / 2
      FBL = .FALSE.
C
      DO 200 I = 1,IT
C                                       Don't try to handle blanks
         IF (ARR(I).EQ.FBLANK) THEN
            IF (IBL.EQ.0) THEN
               WRITE (MSGTXT,1000)
               CALL MSGWRT (8)
               IBL = 1
               END IF
            FBL = .TRUE.
            GO TO 999
            END IF
C
         SUM = 0.0
         K = 1
C                                      Make some attempt to deal with ends
         DO 100 J = I-IEND, I+IEND
            IF (J.LT.1) THEN
               SUM = SUM + COEFFS(K)*ARR(1)
            ELSE IF (J.GT.IT) THEN
               SUM = SUM + COEFFS(K)*ARR(IT)
            ELSE
               SUM = SUM + COEFFS(K)*ARR(J)
               END IF
            K = K + 1
 100        CONTINUE
         ARRS(I) = SUM
 200     CONTINUE
C
 999  RETURN
C-------------------------------------------------------------------
 1000 FORMAT ('HANNSM: WARNING, I DON''T HANDLE BLANKS, SOME
     * ROW(S) UNSMOOTHED')
      END
