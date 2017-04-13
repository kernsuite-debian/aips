LOCAL INCLUDE 'IRING.INC'
      INTEGER   IBLK(256), ICN, JVOL, LUN2, FIND, SCRTCH(256), IVER,
     *   GRCHN, TVCHN, TVCORN(4), ISYM
      LOGICAL   AXES, DOTV, CONECT, EBARS
      HOLLERITH XNMIN(3), XCLIN(2), KEYSTR(4), XOTEXT(12)
      REAL      USER, SQIN, DKIN, BLC(7), TRC(7), APM(10), CPM(10),
     *   SCX, OFX, SCY, OFY, XBLC(2), XTRC(2), XSYM, FACTOR, XDOTV,
     *   XGRCH
      CHARACTER OUTEXT*48
      COMMON /INPARM/ USER, XNMIN, XCLIN, SQIN, DKIN, BLC, TRC, APM,
     *   CPM, KEYSTR, XSYM, FACTOR, XDOTV, XGRCH, XOTEXT
      COMMON /PLOT/ SCX, OFX, SCY, OFY, XBLC, XTRC, AXES, IBLK, ICN,
     *   JVOL, LUN2, FIND, SCRTCH, IVER, GRCHN, TVCHN, TVCORN, DOTV,
     *   CONECT, EBARS, ISYM
      COMMON /PLOTCH/ OUTEXT
LOCAL END
      PROGRAM IRING
C-----------------------------------------------------------------------
C! Integrates an image in projected concentric rings.
C# Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2005, 2007-2009, 2013-2015
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
C   IRING integrates map in projected concentric rings.
C   adverb    variable  description
C    USERID     USER     User ID owner. ignored
C    INNAME     NMIN     Image name(name).  blank=>any
C    INCLASS    CLIN     Image name(class). blank=>any
C    INSEQ      SQIN     Image name(seq).  0=>any
C    INDISK     DKIN     Disk drive # of image.  0=>any
C    BLC        BLC      Bottom Left hand pixel of subimage
C    TRC        TRC      Top Right hand pixel of subimage
C    APARM      APM      Orientation parameters disk. (1),(2): x and
C                        y pixels of center, (3) pos. angle major
C                        axis, (4) inclination.
C                        (5) aximuthal extent of wedge, 0=>360 degrees
C                        (6) central azimuthal angle of wedge
C    CPARM      CPM      (1) radius inner ring, (2) radius outer ring,
C                        (3) width of one ring, (4) 1=>treat blanks as
C                        zero, (5) 1=>plot results. (6) 1=>1 pixel~ 1".
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   MAXRNG
      PARAMETER (MAXRNG=1024)
C
      CHARACTER PGMNAM*6, NMIN*12, CLIN*6, ARCS*8, CHARS(4)*8,
     *   XUN*8, YUN*8, SRCH*8, YUO*8, MTYPE*2, COORD*6, LINE*48,
     *   KEYU*16
      INTEGER   IPARMS, NRING, SCR(256), NPARMS, IRET, IER, ISEQ, IVOL,
     *   IUSR, L16, WIN(4), NBUF, NXW, NYW, I, J, NP(MAXRNG), IOUT, NAX,
     *   PAX(7), DPT(5), BOF, K, NNZ(MAXRNG), TLUN, TIND, JT, JTRIM,
     *   FSUM, FMEAN, FERR
      REAL    RBUF(MABFSS), ARR(MABFSS), BLNK, R, RR, RX, RY, CSP, SNP,
     *   CSH, CSI, LLMIN, LMOU, RMIN, DX, DY, AMX, AMN, ASX, SOM, BMA,
     *   BMI, PPB, SNI, TMP1, TMP2, RCEN(2), RRT, DPM(10), ASN, XRNG(2),
     *   AZ1, AZ2, AZ, DZ, AEN, AEX, PMEAN(MAXRNG), PDIST(MAXRNG),
     *   PERR(MAXRNG), PSUM(MAXRNG), PERRS(MAXRNG), PPBC, PARMS(62),
     *   XPRIME, YPRIME, RRIME
      DOUBLE PRECISION MEAN(MAXRNG), MEANSQ(MAXRNG), VAL, ERR(MAXRNG),
     *   ERRS(MAXRNG), SUM(MAXRNG), DIST(MAXRNG), SIGMA
      LOGICAL   T, F, RQUICK, REPL, PIXEL
      INCLUDE 'IRING.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (PARMS, USER)
      DATA PGMNAM,  NPARMS, TLUN /'IRING ' ,62, 3/
      DATA T, F /.TRUE.,.FALSE./
      DATA L16 /16/
      DATA ARCS /'Arcsec  '/
      DATA CHARS /'/BEAM ', '/Beam ', '/B ', '   ? '/
C-----------------------------------------------------------------------
      CALL ZDCHIN (T)
      CALL VHDRIN
      BLNK = FBLANK
      CALL GTPARM (PGMNAM, NPARMS, RQUICK, USER, SCR, IRET)
      IF (IRET.NE.0) THEN
         IF (IRET.EQ.1) GO TO 999
         IRET = 16
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
         END IF
      IF (RQUICK) CALL RELPOP (IRET, SCR, IER)
      IF (IRET.NE.0) GO TO 990
      IRET  = 8
C                                         interpret input
      ISEQ = SQIN + 0.01
      IVOL = DKIN + 0.01
      USER = NLUSER
      IUSR = NLUSER
      CALL H2CHR (12, 1, XNMIN, NMIN)
      CALL H2CHR (6, 1, XCLIN, CLIN)
      CONECT = FACTOR.LE.0.0
      IF (FACTOR.EQ.0.0) FACTOR = 1.0
      FACTOR = ABS (FACTOR)
      EBARS = XSYM.GT.0.0
      ISYM = ABS (XSYM) + 0.01
      ISYM = MAX (1, MIN (ISYM, 24))
      CALL H2CHR (48, 1, XOTEXT, OUTEXT)
C                                       if inclination = 90 degrees,
C                                       may ge 1.0/0.0 later, and this
C                                       projection is
C                                       meaningless anyway
      IF (APM(4).EQ.90.0) THEN
         WRITE (MSGTXT,1010)
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       bypass arcsec calculation
      PIXEL = (CPM(6).GT.0)
C                                         open input image
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
      MTYPE = 'MA'
      CALL MAPOPN ('READ', IVOL, NMIN, CLIN, ISEQ, MTYPE, IUSR, L16,
     *   FIND, ICN, CATBLK, SCR, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1030) IER
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                         # axes, # points/axis
      USER = IUSR
      DKIN = IVOL
      SQIN = ISEQ
      NAX = CATBLK(KIDIM)
      DO 35 I = 1,NAX
         PAX(I) = CATBLK(KINAX+I-1)
 35      CONTINUE
C                                         check, interpret constants
      CALL WINDOW (NAX, PAX, BLC, TRC, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1040) IER
         CALL MSGWRT (8)
         GO TO 980
         END IF
      DO 45 I = 1,5
         DPT(I) = BLC(I+2) + 0.01
 45      CONTINUE
      LOCNUM = 1
      CALL SETLOC (DPT, F)
C                                         position axes ?
      IF (PIXEL) THEN
C                           Non-astronomical images. "1 pixel = 1 arcsec"
         DX = 1.
         DY = 1.
         YUO = '        '
         CALL H2CHR (8, 1, CATH(KHBUN), YUN)
         COORD = 'pixels'
         XUN = COORD
         PPB = 1.0
         PPBC = 1.0
         RRT = 0.
      ELSE
         IF (AXTYP(LOCNUM).NE.1) THEN
            WRITE (MSGTXT,1050)
            CALL MSGWRT (5)
            GO TO 980
            END IF
         XUN = ARCS
         CALL H2CHR (8, 1, CATH(KHBUN), YUN)
         BMA = CATR(KRBMJ)
         BMI = CATR(KRBMN)
         DX = CATR(KRCIC)
         DY = CATR(KRCIC+1)
         RRT = CATR(KRCRT+1)
         COORD = 'arcsec'
C                                        PPB means points per beam
         IF ((BMA.EQ.0.0).OR.(BMI.EQ.0.0)) THEN
            PPB = 1.0
            YUO = CHARS(4)
            WRITE (MSGTXT,1070)
            CALL MSGWRT (5)
            PPBC = 1.0
         ELSE
            PPB = 1.133 * BMA / DX * BMI / DY
            PPB = ABS (PPB)
            PPBC = CPM(3) / 3600.0
            IF (PPBC.GT.BMI) THEN
               PPBC = PPB
            ELSE
               PPBC = PPB * PPBC / BMI
               END IF
            WRITE (MSGTXT,1071) PPB, PPBC
            IF (OUTEXT.NE.'NONE') CALL MSGWRT (5)
            SRCH = CHARS(1)
            CALL CHDELE (YUN, 8, 1, SRCH, 5, YUO, REPL)
            IF (.NOT.REPL) THEN
               SRCH = CHARS(2)
               CALL CHDELE (YUN, 8, 1, SRCH, 5, YUO, REPL)
               IF (.NOT.REPL) THEN
                  SRCH = CHARS(3)
                  CALL CHDELE (YUN, 8, 1, SRCH, 2, YUO, REPL)
                  IF (.NOT.REPL) THEN
                     YUO = CHARS(4)
                     WRITE (MSGTXT,1060)
                     CALL MSGWRT (5)
                     END IF
                  END IF
               END IF
            END IF
         DX = DX * 3600.0
         DY = DY * 3600.0
         END IF
      WIN(1) = BLC(1) + 0.01
      WIN(2) = BLC(2) + 0.01
      WIN(3) = TRC(1) + 0.01
      WIN(4) = TRC(2) + 0.01
      NXW = WIN(3) - WIN(1) + 1
      NYW = WIN(4) - WIN(2) + 1
      IF (NXW.GT.MABFSS) THEN
         WRITE(MSGTXT,1075) NXW
         CALL MSGWRT(2)
         GO TO 980
         END IF
      NBUF = 2*MABFSS
      IF (CPM(2).EQ.0.0) CPM(2)  = MIN (ABS(DX)*NXW/2.0,
     *   ABS(DY)*NYW/2.0)
      IF (CPM(3).EQ.0.0) CPM(3)  = (CPM(2) - CPM(1)) / 10.0
      NRING = (CPM(2) - CPM(1)) / CPM(3)
      IF (NRING.GT.MAXRNG) THEN
         WRITE (MSGTXT,1080) NRING, MAXRNG
         CALL MSGWRT (7)
         GO TO 980
         END IF
      DO 90 K = 1,NRING
         NP(K) = 0
         NNZ(K) = 0
         MEAN(K) = 0.0D0
         MEANSQ(K) = 0.0D0
         DIST(K) = 0.0D0
         ERR(K) = 0.0D0
         ERRS(K) = 0.0D0
 90      CONTINUE
      CALL RCOPY (10, CPM, DPM)
      RCEN(1) = CATR(KRCRP) + APM(1) + 1.0 - BLC(1)
      RCEN(2) = CATR(KRCRP+1) + APM(2) + 1.0 - BLC(2)
C                                          determine proper offset
      CALL COMOFF (NAX, PAX, DPT, BOF, IER)
      BOF = BOF + 1
C                                          initialize reading
      CALL MINIT ('READ', L16, FIND, PAX(1), PAX(2), WIN, RBUF, NBUF,
     *   BOF, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1090) IER
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       convert width of angle into
C                                       halfwidth
      IF (APM(5).LE.0.0) THEN
         APM(5) = 360.0
         APM(6) = 0.0
         END IF
      AZ1 = APM(6) - APM(5) / 2.0
      AZ2 = APM(6) + APM(5) / 2.0
      CSP = COS ((APM(3) + RRT) * DG2RAD)
      SNP = SIN ((APM(3) + RRT) * DG2RAD)
      SNI = SIN ((APM(4) + RRT) * DG2RAD)
      CSI = COS ((APM(4) + RRT) * DG2RAD)
      RMIN = 1.0E-3 * MIN (ABS(DX), ABS(DY))
C                                        read
      VAL = 0.0D0
      DO 200 J = 1,NYW
         CALL LINIO ('READ', L16, FIND, RBUF, NXW, ARR, 1, BLNK, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1100) IER
            CALL MSGWRT (8)
            GO TO 980
            END IF
         DO 180 I = 1,NXW
            IF (ARR(I).EQ.BLNK) THEN
               ARR(I) = 0.0
               IF (CPM(4).LE.0.0) GO TO 180
               END IF
C                                       RX, RY - position of the
C                                       given pixel relative to center
C                                       of the field
            RX = (I - RCEN(1)) * DX
            RY = (J - RCEN(2)) * DY
            AZ = ATAN2 (-RX, RY) / DG2RAD
            R = SQRT (RY ** 2 + RX ** 2)
            IF (((AZ.GE.AZ1) .AND. (AZ.LE.AZ2)) .OR. ((AZ+360.GE.AZ1)
     *         .AND. (AZ+360.LE.AZ2)) .OR. ((AZ-360.GE.AZ1) .AND.
     *         (AZ-360.LE.AZ2))) THEN
               RR = 0
               IF (R.GT.RMIN) THEN
C                                       not right somehow
                  CSH =  (CSP * RX / R + SNP * RY / R)
                  RR = R * SQRT (1.0 - CSH * CSH * SNI * SNI) / CSI
C                                       more straightforward
                  XPRIME = RX * SNP + RY * CSP
                  YPRIME = RY * SNP - RX * CSP
                  RRIME = SQRT (XPRIME*XPRIME + (YPRIME/CSI)**2)
                  RR = RRIME
                  END IF
               K = (RR - CPM(1)) / CPM(3) + 1.0
               IF ((K.GE.1).AND.(K.LE.NRING)) THEN
                  NP(K) = NP(K) + 1
                  IF (ARR(I).NE.0.0) NNZ(K) = NNZ(K) + 1
                  VAL = ARR(I)
                  MEAN(K) = MEAN(K) + VAL
                  MEANSQ(K) = MEANSQ(K) + VAL * VAL
                  DIST(K) = DIST(K) + RR
                  END IF
               END IF
 180        CONTINUE
 200     CONTINUE
C                                        close map
      CALL MAPCLS ('READ', IVOL, ICN, L16, FIND, CATBLK, T, SCR, IER)
C                                        output
      IF (OUTEXT.NE.'NONE') THEN
         WRITE (MSGTXT,1120)
         CALL MSGWRT (5)
         WRITE (MSGTXT,1130) COORD, YUN, YUN, YUO
         CALL MSGWRT (5)
         END IF
      AMN =  1.0E12
      AMX = -1.0E12
      ASN =  1.0E12
      ASX = -1.0E12
      AEX = -1.0E12
      AEN =  1.0E12
      SOM =  0.0
      IF ((OUTEXT.NE.' ') .AND. (OUTEXT.NE.'NONE')) THEN
         CALL ZTXOPN ('WRIT', TLUN, TIND, OUTEXT, .TRUE., IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1200) IER, 'OPEN'
            CALL MSGWRT (7)
            OUTEXT = ' '
            IER = 0
            END IF
         END IF
C                                       header
      IF ((OUTEXT.NE.' ') .AND. (OUTEXT.NE.'NONE')) THEN
         IF (CPM(7).GT.0.0) THEN
            CALL H2CHR (16, 1, KEYSTR, KEYU)
         ELSE
            KEYU = 'radius (asec)'
            END IF
         JT = JTRIM (KEYU)
         IF (CPM(5).GT.1.5) THEN
            WRITE (LINE,1201) KEYU(:JT), 'sum', YUN
         ELSE
            WRITE (LINE,1201) KEYU(:JT), 'mean', YUN
            END IF
         CALL ZTXIO ('WRIT', TLUN, TIND, LINE, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1200) IER, 'WRIT'
            CALL MSGWRT (7)
            CALL ZTXCLS (TLUN, TIND, IER)
            IER = 0
            OUTEXT = ' '
            END IF
         END IF
      DO 240 K = 1,NRING
         LLMIN = CPM(1) + (K-1) * CPM(3)
         LMOU = LLMIN + CPM(3)
         SUM(K) = SOM + MEAN(K) / PPB
         SOM = SUM(K)
         IF (NP(K).LT.1) THEN
            MEAN(K) = 0.0D0
            MEANSQ(K) = 0.0D0
            DIST(K) = 0.0D0
            ERR(K) = 0.0D0
            ERRS(K) = 0.0D0
         ELSE
            MEAN(K) = MEAN(K) / NP(K)
            MEANSQ(K) = MEANSQ(K) / NP(K)
            DIST(K) = DIST(K) / NP(K)
            IF (CPM(7).GT.0.0) DIST(K) = DIST(K) * CPM(7)
            SIGMA = SQRT (MAX (0.0D0, MEANSQ(K) - MEAN(K) * MEAN(K)))
            IF (NNZ(K).GT.1) SIGMA = SIGMA / SQRT ((NNZ(K)-1.0) / PPBC)
            ERR(K) = SIGMA
            ERRS(K) = SIGMA * SIGMA
            IF (K.GT.1) ERRS(K) = ERRS(K) + ERRS(K-1)**2
            ERRS(K) = SQRT (ERRS(K))
            END IF
         IF (EBARS) THEN
            IF (MEAN(K)+ERR(K).GT.AMX) AMX = MEAN(K) + ERR(K)
            IF (MEAN(K)-ERR(K).LT.AMN) AMN = MEAN(K) - ERR(K)
            IF (SUM(K)+ERRS(K).GT.ASX) ASX = SUM(K) + ERRS(K)
            IF (SUM(K)-ERRS(K).LT.ASN) ASN = SUM(K) - ERRS(K)
         ELSE
            IF (MEAN(K).GT.AMX) AMX = MEAN(K)
            IF (MEAN(K).LT.AMN) AMN = MEAN(K)
            IF (SUM(K).GT.ASX) ASX = SUM(K)
            IF (SUM(K).LT.ASN) ASN = SUM(K)
            END IF
         IF (ERR(K).GT.AEX) AEX = ERR(K)
         IF (ERR(K).LT.AEN) AEN = ERR(K)
         PMEAN(K) = MEAN(K)
         PERR(K) = ERR(K)
         PSUM(K) = SUM(K)
         PERRS(K) = ERRS(K)
         PDIST(K) = DIST(K)
         IF ((OUTEXT.NE.' ') .AND. (OUTEXT.NE.'NONE')) THEN
            IF (CPM(5).GT.1.5) THEN
               DY = SUM(K)
               DZ = ERRS(K)
            ELSE
               DY = MEAN(K)
               DZ = ERR(K)
               END IF
            WRITE (LINE,1210) DIST(K), DY, DZ
            CALL ZTXIO ('WRIT', TLUN, TIND, LINE, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,1200) IER, 'WRIT'
               CALL MSGWRT (7)
               CALL ZTXCLS (TLUN, TIND, IER)
               IER = 0
               OUTEXT = ' '
               END IF
            END IF
 240     CONTINUE
      IF ((OUTEXT.NE.' ') .AND. (OUTEXT.NE.'NONE')) CALL ZTXCLS (TLUN,
     *      TIND, IER)
C                                       decide on formats
      IF (CPM(8).GT.0.0) THEN
         AMN = CPM(9)
         AMX = CPM(10)
         ASN = CPM(9)
         ASX = CPM(10)
         END IF
      FMEAN = 1
      IF (AMX.GT.99999.) FMEAN = 2
      IF (AMX.GT.9999999.) FMEAN = 3
      IF (AMX.LT.0.1) FMEAN = 3
      FERR = 1
      IF (AEX.GT.99999.) FERR = 2
      IF (AEX.GT.9999999.) FERR = 3
      IF (AEX.LT.0.1) FERR = 3
      FSUM = 1
      IF (ASX.GT.99999.) FSUM = 2
      IF (ASX.GT.9999999.) FSUM = 3
      IF (ASX.LT.0.1) FSUM = 3
C                                       now print
      IF (OUTEXT.NE.'NONE') THEN
         DO 250 K = 1,NRING
            LLMIN = CPM(1) + (K-1) * CPM(3)
            LMOU = LLMIN + CPM(3)
            WRITE (MSGTXT,1140) K, LLMIN, LMOU, NNZ(K)
            IF (FMEAN.EQ.1) THEN
               WRITE (MSGTXT(30:),1141) MEAN(K)
            ELSE IF (FMEAN.EQ.2) THEN
               WRITE (MSGTXT(30:),1142) MEAN(K)
            ELSE
               WRITE (MSGTXT(30:),1143) MEAN(K)
               END IF
            IF (FERR.EQ.1) THEN
               WRITE (MSGTXT(42:),1141) ERR(K)
            ELSE IF (FERR.EQ.2) THEN
               WRITE (MSGTXT(42:),1142) ERR(K)
            ELSE
               WRITE (MSGTXT(42:),1143) ERR(K)
               END IF
            IF (FSUM.EQ.1) THEN
               WRITE (MSGTXT(54:),1141) SUM(K)
            ELSE IF (FSUM.EQ.2) THEN
               WRITE (MSGTXT(54:),1142) SUM(K)
            ELSE
               WRITE (MSGTXT(54:),1143) SUM(K)
               END IF
            CALL MSGWRT (5)
 250        CONTINUE
         END IF
      IER = 0
      AMN = MIN (AMN, 0.0)
      ASN = MIN (ASN, 0.0)
C                                         plot results
      IF (CPM(5).GT.0.0) THEN
         IPARMS = NPARMS
         CALL PLINIT (IVOL, NMIN, CLIN, ISEQ, IUSR, L16, IPARMS,
     *      PARMS, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,2060) IER
            CALL MSGWRT (8)
            GO TO 980
            END IF
         XRNG(1) = CPM(1)
         XRNG(2) = CPM(2)
         IF (CPM(7).GT.0.0) THEN
            CALL H2CHR (8, 1, KEYSTR, XUN)
            XRNG(1) = CPM(1) * CPM(7)
            XRNG(2) = CPM(2) * CPM(7)
            END IF
         IF (CPM(5).LT.1.5) THEN
            TMP1 = XRNG(2) + 0.07 * (XRNG(2) - XRNG(1))
            TMP2 = AMX + 0.07 * (AMX - AMN)
C                                       CHANGE????
            CALL PLAXES (XRNG(1), TMP1, AMN, TMP2, 1.0, XUN, YUN, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,2070) IER
               CALL MSGWRT (8)
               GO TO 980
               END IF
            CALL PLDATA (NRING, PDIST, PMEAN, PERR, IOUT, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,2080) IER
               CALL MSGWRT (8)
               GO TO 980
               END IF
         ELSE
            SRCH = CHARS(1)
            CALL CHDELE (YUN, 8, 1, SRCH, 5, YUO, REPL)
            IF (.NOT.REPL) THEN
               SRCH = CHARS(2)
               CALL CHDELE (YUN, 8, 1, SRCH, 5, YUO, REPL)
               IF (.NOT.REPL) THEN
                  SRCH = CHARS(3)
                  CALL CHDELE (YUN, 8, 1, SRCH, 2, YUO, REPL)
                  IF (.NOT.REPL) THEN
                     YUO = CHARS(4)
                     WRITE (MSGTXT,1060)
                     CALL MSGWRT (5)
                     END IF
                  END IF
               END IF
            TMP1 = XRNG(2) + 0.07 * (XRNG(2) - XRNG(1))
            TMP2 = ASX + 0.07 * (ASX - ASN)
            CALL PLAXES (XRNG(1), TMP1, ASN, TMP2, 1.0, XUN, YUO, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,2070) IER
               CALL MSGWRT (8)
               GO TO 980
               END IF
            CALL PLDATA (NRING, PDIST, PSUM, PERRS, IOUT, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,2080) IER
               CALL MSGWRT (8)
               GO TO 980
               END IF
            END IF
         CALL PLFINI (IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,2100) IER
            CALL MSGWRT (8)
            GO TO 980
            END IF
         END IF
      IRET = 0
      GO TO 990
C
 980  CALL MAPCLS ('READ', IVOL, ICN, L16, FIND, CATBLK, F, SCR, IER)
C
 990  CALL DIETSK (IRET, RQUICK, SCR)
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('GET PARAMETER ERROR ',I5)
 1010 FORMAT ('THE GALAXY IS EDGE ON, THIS IS NOT A MEANINGFUL ',
     *        'PROJECTION')
 1030 FORMAT ('MAPOPN ERROR: IER = ',I2)
 1040 FORMAT ('WINDOW ERROR: IER = ',I2)
 1050 FORMAT ('X- AND Y-AXIS ARE NOT A POSITION PAIR')
 1060 FORMAT ('**** Y-axis of plot in (original map units) * beam ',
     *   '****')
 1070 FORMAT ('**** No beam in header, last column contains sum ****')
 1071 FORMAT ('Beam area',F7.2,' pixels, correlation correction',F7.2,
     *   ' pixels')
 1075 FORMAT ('Error: Dimension in X: ', I5,'; exceeds compiled max')
 1080 FORMAT ('Error:',I6,' rings required; maximum is',I6)
 1090 FORMAT ('MINIT ERROR : IER = ',I2)
 1100 FORMAT ('LINIO ERROR : IER  = ',I2)
 1120 FORMAT ('Ring  Inner  Outer  Points    Average     Sigma    ',
     *   ' Cumul. Flux')
 1130 FORMAT ('         (',A6,')',4X,'Not 0',4X,'(',A8,')',2X,'(',A8,
     *   ') (',A8,')')
 1140 FORMAT (I3,1X,2(1X,F6.1),1X,I8)
 1141 FORMAT (F11.4)
 1142 FORMAT (F11.1)
 1143 FORMAT (1PE11.4)
 1200 FORMAT ('ERROR',I4,2X,A,'ING THE OUTTEXT FILE')
 1201 FORMAT (A16,A6,'(',A,')',5X,'uncertainty')
 1210 FORMAT (3F16.7)
 2060 FORMAT ('PLINIT: IER = ',I2)
 2070 FORMAT ('PLAXES: IER = ',I2)
 2080 FORMAT ('PLDATA: IER = ',I2)
 2100 FORMAT ('PLFINI: IER = ',I2)
      END
      SUBROUTINE CHDELE (STRI, NI, NB, STRD, ND, STRO, REPL)
C-----------------------------------------------------------------------
C   subroutine to delete a character string of ND characters
C   from another string of NI characters.
C     STRI    C(*)     input    String to be changed
C     NI      I        input    # characters in STRI
C     NB      I        input    begin search at char. NB of STRI
C     STRD    C(*)     input    String to be deleted from STRI
C     ND      I        input    # characters in STRD
C     STRO    C(*)    output    Changed string, if REPL true
C     REPL    L       output    If true, a replacement was done.
C-----------------------------------------------------------------------
      INTEGER   NI, ND, NB, IPNT
      CHARACTER STRI*(*), STRD*(*), STRO*(*)
      LOGICAL   REPL
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      REPL = .FALSE.
      IPNT = INDEX (STRI(NB:NI), STRD(1:ND)) + NB - 1
      IF (IPNT.LE.0) GO TO 999
      REPL = .TRUE.
C                                       Delete
      IF (IPNT.GT.1) THEN
         STRO = STRI(1:IPNT-1)
      ELSE
         STRO = '    '
         END IF
      IF ((IPNT+ND).LE.NI) STRO(IPNT:) = STRI(IPNT+ND:NI)
C
 999  RETURN
      END
      SUBROUTINE PLINIT (IVOL, NAM, CLAS, ISEQ, USID, LUN, IP,
     *   RPARM, IERR)
C-----------------------------------------------------------------------
C   opens map, creates plot extension file
C     IVOL   I      I  disk volume #
C     NAM    C*12   I  image name
C     CLAS   C*6    I  image class
C     ISEQ   I      I  image sequence #
C     USID   I      I  user #
C     LUN    I      I  map logical unit #
C     IP     I      I  # parameters
C     RPARM  R (IP) I  parameter list
C     IERR   I      O  output code. two digit, first digit indi-
C                      cates subroutine: 1: MAPOPN, 2: MADDEX,
C                      3: ZPHFIL, 4: GINIT, second digit indi-
C                      cates error code of that subroutine.
C-----------------------------------------------------------------------
      CHARACTER PHNAME*48, NAM*12, CLAS*6, MTYPE*2, OP*4
      INTEGER   IP, IVOL, ISEQ, USID, LUN, NP, IERR, BLOK(256), GLUN,
     *   GFIND, JERR
      REAL      RPARM(*)
      LOGICAL   T, F
      INCLUDE 'IRING.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                        open map
      IERR = 0
      AXES = .FALSE.
      JVOL = IVOL
      LUN2 = LUN
      MTYPE = 'MA'
      OP = 'HDWR'
      IF (DOTV) OP = 'READ'
      CALL MAPOPN (OP, IVOL, NAM, CLAS, ISEQ, MTYPE, USID, LUN,
     *    FIND, ICN, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         IERR = IERR + 10
         GO TO 990
         END IF
C                                        add plot file to header
      IVER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', IVOL, ICN, CATBLK, BLOK, T, 'READ', IVER,
     *      IERR)
         IF (IERR.NE.0) THEN
            IERR = IERR + 20
            GO TO 990
            END IF
         END IF
C                                        make physical filename
      CALL ZPHFIL ('PL', IVOL, ICN, IVER, PHNAME, IERR)
      IF (IERR.NE.0) THEN
         IERR = IERR + 30
         GO TO 980
         END IF
C                                        open plot file
      NP = IP
      CALL GINIT (IVOL, ICN, PHNAME, 0, 33, NP, RPARM, DOTV, TVCHN,
     *   GRCHN, TVCORN, CATBLK, IBLK, GLUN, GFIND, IERR)
      IF (IERR.EQ.0) GO TO 999
         IERR = IERR + 40
C
 980  CALL MAPCLS ('READ', JVOL, ICN, LUN2, FIND, CATBLK, F, SCRTCH,
     *   JERR)
      GO TO 999
C
 990  CALL MAPCLS ('WRIT', JVOL, ICN, LUN2, FIND, CATBLK, F, SCRTCH,
     *   JERR)
C
 999  RETURN
      END
      SUBROUTINE PLFINI (IERR)
C-----------------------------------------------------------------------
C     IERR   I      O  output code. two digit, first digit indi-
C                      cates subroutine: 1: GFINIS, 2: MAPCLS,
C                      second digit indicates error code of
C                      that subroutine.
C-----------------------------------------------------------------------
      INTEGER   IERR, JERR
      LOGICAL   F
      INCLUDE 'IRING.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                        close plot file
      CALL GFINIS (IBLK, IERR)
      IF (IERR.NE.0) IERR = IERR + 10
      IF (.NOT.DOTV) CALL HIPLOT (JVOL, ICN, IVER, SCRTCH, JERR)
C                                        close map file
      CALL MAPCLS ('READ', JVOL, ICN, LUN2, FIND, CATBLK, F, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) IERR = IERR + 20
C
 999  RETURN
      END
      SUBROUTINE PLAXES (XMN, XMX, YMN, YMX, XY, XUNIT, YUNIT, IERR)
C-----------------------------------------------------------------------
C   plots axes and ticks, MUST  be called prior to PLDATA or PLFUNC
C   parameters:
C     XMN    R         I  value of x-variable at llhc
C     XMX    R         I  value of x-variable at urhc
C     YMN    R         I  value of y-variable at llhc
C     YMX    R         I  value of y-variable at urhc
C     XY     R         I  ratio x-axis / y-axis
C     XUNIT  C*8      I  units along x-axis. (char. string)
C     YUNIT  C*8      I  units along y-axis. (char. string)
C     IERR   I        O  error code, IERR=-1 indicates illegal
C                        inputs, IERR<10 indicates errors in
C                        drawing, IERR>10 indicates error in
C                        GINITL, with IERR-10 the error number.
C-----------------------------------------------------------------------
      CHARACTER SPRTXT*80, ATIME*8, ADATE*12, XUNIT*8, YUNIT*8
      INTEGER   IERR, DEPT(5), I, LABEL, JERR, ID(3), IT(3), INCHAR,
     *   IANGL, INP, LTYPE
      REAL   XRANGE, YRANGE, XR, YR, XY, XMX, YMX, XMN, YMN, CHOUT(4)
      LOGICAL   F, PFLG
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'IRING.INC'
C
C        This common contains the necessary relations between pixels
C        and values of the x- and y-variables.  E.g. the relation
C        between x-pixel and x-variable is:
C           PIX  =  SCX * X  +  OFX
C        BLC and TRC are the lower left and upper right corner, res-
C        pectively, fixed at (1,1) and (1024,1024).
C
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                        fill common /LOCATI/
      DO 5 I = 1,5
         DEPT(I) = 1
 5       CONTINUE
      CALL SETLOC (DEPT, F)
      AXFUNC(1,LOCNUM) = 0
      AXFUNC(2,LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
C                                        proper scaling labels
      XRANGE = XMX - XMN
      YRANGE = YMX - YMN
      IF ((XRANGE.LE.0.0) .OR. (YRANGE.LE.0.0)) THEN
         IERR = -1
         GO TO 990
         END IF
      XR = XRANGE
      LTYPE = 103
      CALL METSCL (LTYPE, XR, CPREF(1,LOCNUM), PFLG)
      YR = YRANGE
      CALL METSCL (LTYPE, YR, CPREF(2,LOCNUM), PFLG)
C                                        proceed filling /LOCATI/
      XBLC(1) = 1.0
      XBLC(2) = 1.0
      XTRC(1) = 1024.0
      XTRC(2) = 1024.0
      SCX = (XTRC(1) - XBLC(1)) / XRANGE
      SCY = (XTRC(2) - XBLC(2)) / YRANGE
      OFX = XBLC(1) - XMN * SCX
      OFY = XBLC(2) - YMN * SCY
      RPLOC(1,LOCNUM) = 1.0
      RPLOC(2,LOCNUM) = 1.0
      RPVAL(1,LOCNUM) = XMN * XR / XRANGE
      RPVAL(2,LOCNUM) = YMN * YR / YRANGE
      AXINC(1,LOCNUM) = XR / XRANGE / SCX
      AXINC(2,LOCNUM) = YR / YRANGE / SCY
      CTYP(1,LOCNUM) = XUNIT
      CTYP(2,LOCNUM) = YUNIT
C                                        space around axes
      CALL CHNTIC (XBLC, XTRC, INP)
      CHOUT(1) = 4.0 + INP
      CHOUT(2) = 3.333
      CHOUT(3) = 0.5
      CHOUT(4) = 2.0
C                                        initialize line drawing
      CALL GINITL (XBLC, XTRC, XY, CHOUT, DEPT, IBLK, IERR)
      IF (IERR.NE.0) THEN
         IERR = IERR + 10
         GO TO 990
         END IF
      LABEL = 3
C                                        proper labeling
      CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XY, F, IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        draw rectangle
      CALL GLTYPE (1, IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL GPOS (XBLC(1), XBLC(2), IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL GVEC (XTRC(1), XBLC(2), IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL GVEC (XTRC(1), XTRC(2), IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL GVEC (XBLC(1), XTRC(2), IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL GVEC (XBLC(1), XBLC(2), IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Date/time version
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (SPRTXT,1020) IVER, ADATE, ATIME
      CALL REFRMT (SPRTXT, '_', INCHAR)
      YR = 0.5
      XR = 0.
      IANGL = 0
      CALL GPOS (XBLC(1), XTRC(2), IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL GCHAR (INCHAR, IANGL, XR, YR, SPRTXT, IBLK, IERR)
      IF (IERR.EQ.0) THEN
         AXES = .TRUE.
         GO TO 999
         END IF
C                                        close map in case of error
 990  CALL MAPCLS ('READ', JVOL, ICN, LUN2, FIND, CATBLK, F, SCRTCH,
     *   JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Plot file version',I4,'__created ',A12,A8)
      END
      SUBROUTINE PLDATA (N, X, Y, DY, IOUT, IERR)
C-----------------------------------------------------------------------
C   plots array of N points.
C   Inputs:
C      N      I      total number of points.
C      X      R(N)   x-values of the N points
C      Y      R(N)   y-values of the N points
C      DY     R(N)   uncertainty in Y
C   Outputs:
C      IOUT   I      number of points outside plot
C      IERR   I      error code. IERR=-1 indicates that
C                            there has not been a legal call to
C                            PLAXES, IERR>0 indicates errors in
C                            drawing routines.
C-----------------------------------------------------------------------
      INTEGER   N, IOUT, IERR
      REAL      X(N), Y(N), DY(N)
C
      INTEGER   JERR, I
      REAL      PX, PY, AX(6), AY(6)
      LOGICAL   XOUT, YOUT, F, GOOD
      INCLUDE 'IRING.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGPH.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
      IOUT = 0
      IF (.NOT.AXES) THEN
         IERR = -1
         GO TO 990
         END IF
C
      CALL GLTYPE (4, IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      DO 10 I = 1,N
         IF (Y(I).NE.FBLANK) THEN
            PX  = X(I) * SCX + OFX
            PY  = Y(I) * SCY + OFY
            XOUT = ((PX.LT.XBLC(1)) .OR. (PX.GT.XTRC(1)))
            YOUT = ((PY.LT.XBLC(2)) .OR. (PY.GT.XTRC(2)))
            IF ((XOUT) .OR. (YOUT)) THEN
               IOUT = IOUT + 1
            ELSE
               AX(1) = PX
               AX(2) = PX
               AX(3) = PX
               AX(4) = PX - 5 * FACTOR
               AX(5) = PX + 5 * FACTOR
               AY(1) = PY
               AY(2) = PY + 5 * FACTOR
               AY(3) = PY - 5 * FACTOR
               AY(4) = PY
               AY(5) = PY
               CALL PNTPLT (ISYM, AX, AY, XBLC, XTRC, .FALSE., .FALSE.,
     *            IBLK, IERR)
               IF (IERR.NE.0) GO TO 990
C                                       error bars
               IF (EBARS) THEN
                  AX(1) = PX + 2 * FACTOR
                  AX(2) = PX - 2 * FACTOR
                  AX(3) = PX
                  AX(4) = PX
                  AX(5) = PX + 2 * FACTOR
                  AX(6) = PX - 2 * FACTOR
                  PY = (Y(I)+DY(I)) * SCY + OFY
                  AY(1) = PY
                  AY(2) = PY
                  AY(3) = PY
                  PY = (Y(I)-DY(I)) * SCY + OFY
                  AY(4) = PY
                  AY(5) = PY
                  AY(6) = PY
                  CALL GPOS (AX(1), AY(1), IBLK, IERR)
                  IF (IERR.EQ.0) CALL GVEC (AX(2), AY(2), IBLK, IERR)
                  IF (IERR.EQ.0) CALL GPOS (AX(3), AY(3), IBLK, IERR)
                  IF (IERR.EQ.0) CALL GVEC (AX(4), AY(4), IBLK, IERR)
                  IF (IERR.EQ.0) CALL GPOS (AX(5), AY(5), IBLK, IERR)
                  IF (IERR.EQ.0) CALL GVEC (AX(6), AY(6), IBLK, IERR)
                  IF (IERR.NE.0) GO TO 990
                  END IF
               END IF
            END IF
 10      CONTINUE
      IF (CONECT) THEN
         CALL GLTYPE (2, IBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         GOOD = .FALSE.
         DO 20 I = 1,N
            IF (Y(I).NE.FBLANK) THEN
               PX  = X(I) * SCX + OFX
               PY  = Y(I) * SCY + OFY
               XOUT = ((PX.LT.XBLC(1)) .OR. (PX.GT.XTRC(1)))
               YOUT = ((PY.LT.XBLC(2)) .OR. (PY.GT.XTRC(2)))
               IF ((XOUT) .OR. (YOUT)) THEN
                  GOOD = .FALSE.
                  IOUT = IOUT + 1
               ELSE
                  IF (GOOD) THEN
                     CALL GVEC (PX, PY, IBLK, IERR)
                  ELSE
                     CALL GPOS (PX, PY, IBLK, IERR)
                     END IF
                  GOOD = .TRUE.
                  END IF
            ELSE
               GOOD = .FALSE.
               END IF
 20         CONTINUE
         END IF
      GO TO 999
C
 990  CALL MAPCLS ('READ', JVOL, ICN, LUN2, FIND, CATBLK, F, SCRTCH,
     *   JERR)
C
 999  RETURN
      END
