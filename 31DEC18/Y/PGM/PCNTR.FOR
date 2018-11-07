LOCAL INCLUDE 'PCNTR.INC'
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:PTVC.INC'
C                                       Input parms
      REAL      CONTUR, POLVEC, GRYSCL, PRUSER, SEQIN, DSKIN, SEQIN2,
     *   DSKIN2, SEQIN3, DSKIN3, SEQIN4, DSKIN4, XBLC(7), XTRC(7),
     *   X3COL, XYRATO, TLABEL, PLEV, CLEV, LEVS(30), FACTOR, PAROT,
     *   XINC, YINC, PCUT, ICUT, DOFRAC, P3COL, DOALIN, DOCIRC,
     *   RANGE(2), DOWDGE, INCOLR, XINVER, STMULT, XHPBP, XDOTV, XGRCH,
     *   XTVCH, DODARK, XDKLIN, RGBLEV(3,30), DRKLEV(3,30), XTVCRN(2)
      HOLLERITH XNAMIN(3), XCLSIN(2), XNAM2(3), XCLS2(2), XNAM3(3),
     *   XCLS3(2), XNAM4(3), XCLS4(2), XFUN, XOMFIL(12)
      CHARACTER NAMIN(5)*12, CLSIN(5)*6, IGFILE*48, OFMFIL*48
      INTEGER   PLBUF(256), IWBLK(256), IGFIND, IGLUN, IFIND(5),
     *   ILUN(5), ISLOT(5), ILABEL, ISEQ(5), IVER, IVOL(5), INVER,
     *   GRCHN, TVCHN, TVCORN(2), INPRMS, BMCORN, IXWDGE, IYWDGE,
     *   ILPVAL, IHPVAL, ZINC, ZMAX, RGBLAB, NOFM
      REAL      BLC(7,5), TRC(7,5), CHOUT(4), NBLC(2), NTRC(2), ABLC(2),
     *   ATRC(2), ROFM(TVMLOU), GOFM(TVMLOU), BOFM(TVMLOU), WRANGE(2)
      LOGICAL   CATUP, QUICK, DOCON, DOPOL, WASOPN(5), DOTV, DOCONV,
     *   DOOFM, DOCOLR, SPCOLR, BMBLNK, FORCEC
      DOUBLE PRECISION    GFAC, GOFF
      COMMON /INPARM/ CONTUR, POLVEC, GRYSCL, PRUSER, XNAMIN, XCLSIN,
     *   SEQIN, DSKIN, XNAM2, XCLS2, SEQIN2, DSKIN2, XNAM3, XCLS3,
     *   SEQIN3, DSKIN3, XNAM4, XCLS4, SEQIN4, DSKIN4, XBLC, XTRC,
     *   X3COL, XYRATO, TLABEL, PLEV, CLEV, LEVS, FACTOR, PAROT, XINC,
     *   YINC, PCUT, ICUT, DOFRAC, P3COL, DOALIN, DOCIRC, RANGE, XFUN,
     *   DOWDGE, XOMFIL, INCOLR, XINVER, STMULT, XHPBP, XDOTV, XGRCH,
     *   XTVCH, DODARK, XDKLIN, RGBLEV, DRKLEV, XTVCRN
      COMMON /TSTUF/ GFAC, GOFF, PLBUF, IWBLK, IGFIND, IGLUN, IFIND,
     *   ILUN, ISLOT, ILABEL, ISEQ, IVER, IVOL, INVER, GRCHN, TVCHN,
     *   TVCORN, INPRMS, CATUP, QUICK, DOCON, DOPOL, WASOPN, DOTV, BLC,
     *   TRC, CHOUT, DOCONV, BMCORN, NBLC, NTRC, ABLC, ATRC, IXWDGE,
     *   IYWDGE, DOOFM, DOCOLR, ILPVAL, IHPVAL, ROFM, GOFM, BOFM,
     *   WRANGE, ZINC, ZMAX, SPCOLR, BMBLNK, FORCEC, RGBLAB, NOFM
      COMMON /CHPARM/ NAMIN, CLSIN, IGFILE, OFMFIL
C                                       Header blocks etc
      INTEGER   PCATI(256,5)
      REAL      PCATR(256,5)
      HOLLERITH PCATH(256,5)
      DOUBLE PRECISION PCATD(128,5)
      REAL      RBLK(MABFSS,4)
      INTEGER   IBLK(MABFSS,4)
      COMMON /MAPHDR/ PCATI
      INCLUDE 'INCS:DCNT.INC'
      EQUIVALENCE (PCATI, PCATR, PCATH, PCATD)
      EQUIVALENCE (IBLK, RBLK, IBUFF)
LOCAL END
      PROGRAM PCNTR
C-----------------------------------------------------------------------
C! Task to generate an image plot with polarization vectors.
C# Map Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2002-2004, 2006-2009, 2011-2012, 2014-2017
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
C   PCNTR will write commands to a plot file for the execution
C   of a contour plot for a cataloged image file and/or a vector
C   polarization vector plot for the associated polarization
C   intensity and position angle maps.
C   First a cataloged map file is found.  Then the list of
C   associated files is searched for PLot files to find the highest
C   version number.  Then a PLot file is created for this map and the
C   catalog header is updated.  Next the graphics commands are
C   written to the plot file.
C    INPUTS:   (from AIPS)
C              DOCONT   R   true (1) means do contour plot.
C              DOVECT   R   true (1) means do polarization vectors.
C              USERID   R   user number, 0 means use logon user
C                       number, 32000 means any user can be accessed.
C              INNAME   R(3)   name of I map.
C              INCLASS  R(2)   class of I map.
C              INSEQ    R   sequence number of I map.
C              INDISK   R   disk volume number of I map.
C              IN2NAME  R(3)   name of P map.
C              IN2CLASS R(2)   class of P map.
C              IN2SEQ   R   sequence number of P map.
C              IN2DISK  R   disk volume number of P map.
C              IN3NAME  R(3)   name of Q map.
C              IN3CLASS R(2)   sequence number of Q map.
C              IN3SEQ   R   sequence number of Q map.
C              IN3DISK  R   the disk volume number of the Q map.
C              BLC      R(7)   the coordinate in the input file to
C                       become the left hand coordinate (1,1) of the
C                       contour plot.  BLC(1) is the X coordinate and
C                       BLC(2) is the Y coordinate.  The first
C                       coordinate in the input image is (1,1).
C              TRC      R(7)   the coordinate in the input file to
C                       become the top right hand corner of the
C                       contour plot. The conventions for BLC hold.
C              XYRATIO  R   the ratio between the scale factor to use
C                       for the X axis and the scale factor to use
C                       for the Y axis.
C              LTYPE    R   the type of axis labeling to use for this
C                          1 = no labels. Make map as big as possible.
C                          2 = no ticks, no tick labels, but rest
C                          3 = RA - DEC coordinates & labels
C                          4 = Center of field relative
C                          5 = Center of sub-image relative
C              PLEV     R   the percentage of the peak value to use as
C                       the multiplier for the contour levels.  If
C                       zero use CLEV below.
C              CLEV     R   The absolute value of the multiplier used
C                       for the contour levels.  This value is used
C                       only if PLEV is zero.
C              LEVS     R(30)   the contour levels.  An out of sequence
C                       level indicates 'end of levels'.  The real
C                       value of a particular level is the LEV value
C                       times CLEV or the value determined by PLEV.
C              FACTOR   R   the number pixels for a vector of
C                       length 1 P units.
C              XINC     R   X spacing in pixels between vectors.
C              YINC     R   Y spacing in pixels between vectors.
C              PCUT     R   do not print vector with lengths .LT.
C                       PCUT in P map units. 0 => 0.1 * Pmax.
C              ICUT     Do not draw vectors when pixel value of I
C                       map < ICUT in I map units. 0 => 0.1*Imax.
C              DOALIGN  R    >= 0. => require axes to line up
C              DOCIRCLE R    > 0 Plot coord grid rather than just ticks
C              INVERS   R    ST file version number.
C              STFACTOR R    scale star sizes in file for plotting:
C                              0 => no plot of stars.
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C      TVCORN   R(2)   TV pixel to use (both > 0 => pixel scale)
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGNAM*6
      INCLUDE 'PCNTR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGNAM /'PCNTR '/
C-----------------------------------------------------------------------
C                                       init the inputs
      CALL PCNTRI (PRGNAM, IRET)
C                                       do plot
      CALL PCNTRP (IRET)
C
      CALL DIETSK (IRET, QUICK, IWBLK)
C
 999  STOP
      END
      SUBROUTINE PCNTRI (PRGNAM, IRET)
C-----------------------------------------------------------------------
C   Startup routine.  Get parameters and check values and defaults, open
C   image and init PL file.
C   Inputs:
C      PRGNAM    C*6  Task name
C   Output:
C      IRET      I    Return code, 0=> OK else failed
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IRET
C
      CHARACTER TYPIN*2, CTEMP1*8, CTEMP2*8, OPCODE*4, STOKES*8, RGB*8
      DOUBLE PRECISION DAXV
      REAL   AXV, EPS, PEAK, X, XMULT, RANGE2(2), WRANG2(2), TEMP
      INTEGER  ICPNT, I, IUSER, INC, J, IROUND, IDEPTH(5), IERR, MLOCS,
     *   LLOCS, K, KK, IC, IG, LTYPE
      LOGICAL   T, F, REDUCE, ONEMAP
      INCLUDE 'PCNTR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA TYPIN /'  '/
      DATA T, F /.TRUE.,.FALSE./
      DATA EPS /0.2/
      DATA STOKES, RGB /'STOKES', 'RGB'/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      ILUN(1) = 16
      ILUN(2) = 17
      ILUN(3) = 18
      ILUN(4) = 19
      ILUN(5) = 20
      IGLUN = 26
      WASOPN(1) = .FALSE.
      WASOPN(2) = .FALSE.
      WASOPN(3) = .FALSE.
      WASOPN(4) = .FALSE.
      WASOPN(5) = .FALSE.
      IXWDGE = 0
      IYWDGE = 0
C                                       get adverbs
      INPRMS = 208
      IRET = 0
      CALL GTPARM (PRGNAM, INPRMS, QUICK, CONTUR, IWBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRET = 1
         END IF
      IF (QUICK) CALL RELPOP (IRET, IWBLK, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 8
      CATUP = F
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XNAMIN, NAMIN(1))
      CALL H2CHR (6, 1, XCLSIN, CLSIN(1))
      CALL H2CHR (12, 1, XNAM2, NAMIN(2))
      CALL H2CHR (6, 1, XCLS2, CLSIN(2))
      CALL H2CHR (12, 1, XNAM3, NAMIN(3))
      CALL H2CHR (6, 1, XCLS3, CLSIN(3))
      CALL H2CHR (12, 1, XNAM4, NAMIN(4))
      CALL H2CHR (6, 1, XCLS4, CLSIN(4))
      CALL H2CHR (48, 1, XOMFIL, OFMFIL)
      IF ((XDKLIN.LE.0.0) .OR. (XDKLIN.GT.1.0)) XDKLIN = 0.33
C
      ISEQ(1) = IROUND (SEQIN)
      IVOL(1) = IROUND (DSKIN)
      ISEQ(2) = IROUND (SEQIN2)
      IVOL(2) = IROUND (DSKIN2)
      ISEQ(3) = IROUND (SEQIN3)
      IVOL(3) = IROUND (DSKIN3)
      ISEQ(4) = IROUND (SEQIN4)
      IVOL(4) = IROUND (DSKIN4)
      INVER = IROUND (XINVER)
      ZINC = IROUND (X3COL)
      IF (X3COL.GT.0.0) THEN
         ZINC = MAX (1, ZINC)
      ELSE
         ZINC = 1
         END IF
C                                       which is which?
      IC = IROUND (CONTUR)
      IF (CONTUR.GT.0.0) THEN
         IC = MAX (IC, 1)
         NAMIN(5) = NAMIN(IC)
         CLSIN(5) = CLSIN(IC)
         ISEQ(5) = ISEQ(IC)
         IVOL(5) = IVOL(IC)
         END IF
      IG = IROUND (GRYSCL)
      IF ((GRYSCL.GT.0.0) .AND. (IG.NE.4)) THEN
         IG = MAX (IG, 1)
         NAMIN(4) = NAMIN(IG)
         CLSIN(4) = CLSIN(IG)
         ISEQ(4) = ISEQ(IG)
         IVOL(4) = IVOL(IG)
         END IF
      DOCON = CONTUR.GT.0.0
      SPCOLR = (.NOT.DOCON) .AND. (X3COL.GT.0.0)
      DOPOL = POLVEC.GT.0.0
      RGBLAB = IROUND (ABS(TLABEL))
      IF (MOD(RGBLAB,100).LE.0) RGBLAB = 3
      RGBLAB = (RGBLAB-1) / 10
      ILABEL = IROUND (TLABEL)
      LTYPE = MOD (ABS(ILABEL), 100)
      IF (LTYPE.LE.0) LTYPE = 3
      LTYPE = MOD (LTYPE-1,10) + 1
      IF (ILABEL.GT.0) THEN
         ILABEL = (ILABEL/100)*100 + LTYPE
      ELSE
         ILABEL = (ILABEL/100)*100 - LTYPE
         END IF
      PRUSER = NLUSER
      IUSER = NLUSER
      BMCORN = IROUND (XHPBP)
      BMBLNK = BMCORN.LT.0.0
      BMCORN = ABS (BMCORN)
      IF (BMCORN.GT.20) BMCORN = 1
      IF (MOD(BMCORN,5).EQ.0) BMCORN = BMCORN - 4
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = XTVCH + 0.01
      TVCORN(1) = IROUND (XTVCRN(1))
      TVCORN(2) = IROUND (XTVCRN(2))
C                                       Set some default values.
      IF (FACTOR.LE.0.000001) FACTOR = 1.0
      IF (XINC.LT.1.0) XINC = 1.0
      IF (YINC.LT.1.0) YINC = 1.0
C                                       Open I map file & get header.
      OPCODE = 'HDWR'
      IF (DOTV) OPCODE = 'READ'
      CALL MAPOPN (OPCODE, IVOL(1), NAMIN(1), CLSIN(1), ISEQ(1), TYPIN,
     *   IUSER, ILUN(1), IFIND(1), ISLOT(1), PCATI(1,1), IWBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      WASOPN(1) = .TRUE.
      SEQIN = ISEQ(1)
      DSKIN = IVOL(1)
      CALL CHR2H (12, NAMIN(1), 1, XNAMIN)
      CALL CHR2H (6, CLSIN(1), 1, XCLSIN)
      IF ((PCATR(KRBMJ,1).LE.1.E-9) .OR. (PCATR(KRBMN,1).LE.1.E-9))
     *   BMCORN = 0
C                                       Add extension file to header.
      IVER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', IVOL, ISLOT, PCATI, IWBLK, T, 'READ',
     *      IVER, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
      CALL WINDOW (PCATI(KIDIM,1), PCATI(KINAX,1), XBLC, XTRC, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       check ST plot parms
      I = 0
      IF (XINVER.LT.0.0) STMULT = 0.0
      IF (STMULT.NE.0.0) CALL FNDEXT ('ST', PCATI, I)
      IF (I.GT.0) THEN
         J = XINVER + 0.1
         IF (J.LE.0) J = I
         XINVER = J
      ELSE
         XINVER = 0.0
         STMULT = 0.0
         END IF
      INVER = IROUND (XINVER)
C                                       Default XYRATO: ratio of
C                                       incr if related.
      DO 15 I = 1,5
         IDEPTH(I) = XBLC(I+2) + 0.01
 15      CONTINUE
      LOCNUM = 1
      IF ((XYRATO.LE.0.01) .OR. (XYRATO.GT.320.0)) THEN
         CALL SETLOC (IDEPTH, T)
         IF ((AXTYP(LOCNUM).EQ.1) .AND. (AXINC(2,LOCNUM).NE.0.0)) XYRATO
     *      = ABS (AXINC(1,LOCNUM) / AXINC(2,LOCNUM))
         IF (((XYRATO.LE.0.04) .OR. (XYRATO.GT.25.)) .AND.
     *      (XTRC(1).NE.XBLC(1))) XYRATO = (XTRC(2)-XBLC(2)) /
     *      (XTRC(1)-XBLC(1))
         IF ((XYRATO.LE.0.04) .OR. (XYRATO.GT.25.)) XYRATO = 1.0
         END IF
C                                       Build file name.
      CALL ZPHFIL ('PL', IVOL, ISLOT, IVER, IGFILE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (7)
         GO TO 970
         END IF
C                                       fill in defaults in PARMS
      IF (X3COL.GT.0.0) THEN
         ZMAX = IROUND (XTRC(3))
      ELSE
         ZMAX = IROUND (XBLC(3))
         END IF
      CALL RCOPY (5, XBLC(3), XTRC(3))
      IF (NAMIN(2).EQ.' ') NAMIN(2) = NAMIN(1)
      IF (NAMIN(3).EQ.' ') NAMIN(3) = NAMIN(1)
      IF (NAMIN(4).EQ.' ') NAMIN(4) = NAMIN(1)
      IF (NAMIN(5).EQ.' ') NAMIN(5) = NAMIN(1)
      IF (CLSIN(2).EQ.' ') CLSIN(2) = 'PPOL  '
      IF (CLSIN(3).EQ.' ') CLSIN(3) = 'PANG  '
      CALL RCOPY (7, XBLC, BLC(1,1))
      CALL RCOPY (7, XTRC, TRC(1,1))
C                                       Check for pol maps = #1
      REDUCE = .FALSE.
      ONEMAP = .FALSE.
      IF (DOPOL) THEN
         POLVEC = 1.0
C                                       Do we have only one image?
         CALL AXEFND (8, STOKES, PCATI(KIDIM,1), PCATH(KHCTP,1), MLOCS,
     *      IERR)
         IF (IERR.EQ.0) THEN
            AXV = PCATD(KDCRV+MLOCS,1) + PCATR(KRCIC+MLOCS,1) *
     *         (1.0 - PCATR(KRCRP+MLOCS,1))
            PEAK = PCATD(KDCRV+MLOCS,1) +  PCATR(KRCIC+MLOCS,1) *
     *         (PCATI(KINAX+MLOCS,1) - PCATR(KRCRP+MLOCS,1))
            ONEMAP = ((AXV.LE.1.0) .AND. (PEAK.GE.3.0)) .OR.
     *         ((AXV.GE.3.0) .AND. (PEAK.LE.1.0))
            END IF
         IF (ONEMAP) THEN
            NAMIN(2) = NAMIN(1)
            CLSIN(2) = CLSIN(1)
            ISEQ(2) = ISEQ(1)
            IVOL(2) = IVOL(1)
            NAMIN(3) = NAMIN(1)
            CLSIN(3) = CLSIN(1)
            ISEQ(3) = ISEQ(1)
            IVOL(3) = IVOL(1)
            END IF
         END IF
C                                       open and check images
      DOCONV = ONEMAP
      DO 50 K = 2,5
         IF (((K.LT.4) .AND. (DOPOL)) .OR.
     *      ((K.EQ.4) .AND. (GRYSCL.GT.0.0)) .OR.
     *      ((K.EQ.5) .AND. (CONTUR.GT.0.0))) THEN
            CALL MAPOPN ('READ', IVOL(K), NAMIN(K), CLSIN(K), ISEQ(K),
     *         TYPIN, IUSER, ILUN(K), IFIND(K), ISLOT(K), PCATI(1,K),
     *         IWBLK, IERR)
            IF (IERR.GT.1) GO TO 970
            WASOPN(K) = .TRUE.
            IF ((K.EQ.2) .OR. ((K.EQ.4) .AND. (IG.EQ.2)) .OR.
     *         ((K.EQ.5) .AND. (IC.EQ.2))) THEN
               SEQIN2 = ISEQ(K)
               DSKIN2 = IVOL(K)
               CALL CHR2H (12, NAMIN(K), 1, XNAM2)
               CALL CHR2H (6, CLSIN(K), 1, XCLS2)
            ELSE IF ((K.EQ.3) .OR. ((K.EQ.4) .AND. (IG.EQ.3)) .OR.
     *         ((K.EQ.5) .AND. (IC.EQ.3))) THEN
               SEQIN3 = ISEQ(K)
               DSKIN3 = IVOL(K)
               CALL CHR2H (12, NAMIN(K), 1, XNAM3)
               CALL CHR2H (6, CLSIN(K), 1, XCLS3)
            ELSE IF ((K.EQ.4) .OR. ((K.EQ.5) .AND. (IC.EQ.4))) THEN
               SEQIN4 = ISEQ(K)
               DSKIN4 = IVOL(K)
               CALL CHR2H (12, NAMIN(K), 1, XNAM4)
               CALL CHR2H (6, CLSIN(K), 1, XCLS4)
               END IF
            DO 20 I = 3,7
               J = KINAX + I - 1
               BLC(I,K) = BLC(I,1)
               IF (PCATI(J,K).LE.1) BLC(I,K) = 1
               TRC(I,K) = BLC(I,K)
 20            CONTINUE
            DO 35 I = 1,2
               J = KRCRP + I - 1
               BLC(I,K) = PCATR(J,K) - PCATR(J,1) + BLC(I,1)
               TRC(I,K) = PCATR(J,K) - PCATR(J,1) + TRC(I,1)
               IF (DOALIN.LT.-1.5) THEN
                  BLC(I,K) = BLC(I,1)
                  TRC(I,K) = TRC(I,1)
                  END IF
               J = I - 1
               IF (DOALIN.GT.-0.10) THEN
                  DAXV = PCATD(KDCRV+J,1) + (BLC(I,1) -
     *               PCATR(KRCRP+J,1)) * PCATR(KRCIC+J,1)
                  IF (PCATR(KRCIC+J,K).EQ.0.0) GO TO 900
                  X = (DAXV - PCATD(KDCRV+J,K)) / PCATR(KRCIC+J,K) +
     *               PCATR(KRCRP+J,K)
                  BLC(I,K) = IROUND (X)
                  IF ((DOALIN.GE.0.1) .AND. (ABS(X-BLC(I,K)).GT.EPS))
     *               GO TO 900
                  TRC(I,K) = BLC(I,K) + TRC(I,1) - BLC(I,1)
                  END IF
C                                       smaller subimage needed?
               IF (BLC(I,K).LT.1.0) THEN
                  DO 25 KK = 1,K
                     BLC(I,KK) = BLC(I,KK) + 1.0 - BLC(I,K)
 25                  CONTINUE
                  REDUCE = .TRUE.
                  END IF
               IF (TRC(I,K).GT.PCATI(KINAX+J,K)) THEN
                  DO 30 KK = 1,K
                     TRC(I,KK) = TRC(I,KK) + PCATI(KINAX+J,K) - TRC(I,K)
 30                  CONTINUE
                  REDUCE = .TRUE.
                  END IF
               IF (BLC(I,K).GE.TRC(I,K)) GO TO 900
 35            CONTINUE
C                                       Check alignment
            IF (DOALIN.GE.0.1) THEN
               INC = 2
               DO 40 I = 1,2
                  J = I - 1
                  X = ABS (PCATR(KRCIC+J,1)) * EPS * EPS
                  ICPNT = KHCTP + J*INC
                  CALL H2CHR (8, 1, PCATH(ICPNT,1), CTEMP1)
                  CALL H2CHR (8, 1, PCATH(ICPNT,K), CTEMP2)
                  IF (CTEMP1.NE.CTEMP2) GO TO 900
                  IF (ABS(PCATR(KRCIC+J,1)-PCATR(KRCIC+J,K)).GT.X)
     *               GO TO 900
                  IF (ABS(PCATR(KRCRT+J,1)-PCATR(KRCRT+J,K)).GT.1.)
     *               GO TO 900
 40               CONTINUE
               END IF
            END IF
 50      CONTINUE
C                                       Levels defaults
      IF (CONTUR.GT.0.0) THEN
         PEAK = MAX (ABS(PCATR(KRDMX,5)), ABS(PCATR(KRDMN,5)))
         IF ((CLEV.EQ.0.0) .AND. (PLEV.EQ.0.0)) PLEV = 10.0
         XMULT = CLEV
         IF (PLEV.NE.0.0) XMULT = PEAK * PLEV / 100.0
         CLEV = XMULT
         PLEV = 0.0
         IF ((LEVS(1).EQ.0.0) .AND. (LEVS(2).LE.LEVS(1))) THEN
            DO 60 I = 1,10
               LEVS(I) = I - 11.
               LEVS(I+10) = I
               LEVS(I+20) = 0.0
 60            CONTINUE
            END IF
         END IF
C                                       INCOLOR ??
      MLOCS = -1
      IF ((GRYSCL.GT.0.0) .AND. (INCOLR.GT.0.0)) THEN
         CALL AXEFND (8, RGB, PCATI(KIDIM,4), PCATH(KHCTP,4), MLOCS,
     *      IERR)
         IF ((IERR.EQ.0) .AND. ((MLOCS.NE.2) .OR.
     *      (PCATI(KINAX+2,4).LT.3))) THEN
            MSGTXT = 'DOCOLR REQUESTED, BUT RGB AXIS NOT 3RD AXIS'
            CALL MSGWRT (7)
            MLOCS = 99
            END IF
         END IF
      IF (MLOCS.NE.2) INCOLR = -1.0
      IF (INCOLR.GT.0.0) THEN
         BLC(3,4) = 1.0
         TRC(3,4) = 1.0
         END IF
C                                       check conversion
      IF (DOPOL) THEN
         CALL AXEFND (8, STOKES, PCATI(KIDIM,3), PCATH(KHCTP,3), MLOCS,
     *      IERR)
         IF (IERR.NE.0) MLOCS = -1
         CALL AXEFND (8, STOKES, PCATI(KIDIM,2), PCATH(KHCTP,2), LLOCS,
     *      IERR)
         IF (IERR.NE.0) LLOCS = -1
         IF ((MLOCS.GE.0) .AND. (LLOCS.GE.0)) THEN
            TEMP = PCATD(KDCRV+MLOCS,3) +  PCATR(KRCIC+MLOCS,3) *
     *         (BLC(1+MLOCS,3) - PCATR(KRCRP+MLOCS,3))
            J = IROUND (TEMP)
            TEMP = PCATD(KDCRV+LLOCS,2) +  PCATR(KRCIC+LLOCS,3) *
     *         (BLC(1+LLOCS,2) - PCATR(KRCRP+LLOCS,2))
            I = IROUND (TEMP)
            DOCONV = (J.EQ.3) .AND. (I.EQ.2)
            END IF
         END IF
      IF (REDUCE) THEN
         MSGTXT = 'Input maps aligned on reduced subimage only'
         CALL MSGWRT (3)
         END IF
      IF (DOCONV) THEN
         IF (ONEMAP) THEN
            POLVEC = 3.0
            MSGTXT = 'First image IQU converted to I-P-angle'
         ELSE
            POLVEC = 2.0
            MSGTXT = 'Second image is Stokes Q and third image is U'
            END IF
         CALL MSGWRT (3)
         END IF
      IRET = 0
C                                       set gray scale range
      IF (GRYSCL.GT.0.0) THEN
         WRANGE(1) = 0.0
         WRANGE(2) = 0.0
         CALL RNGSET (WRANGE, PCATR(KRDMX,4), PCATR(KRDMN,4), WRANG2)
         CALL RNGSET (RANGE, PCATR(KRDMX,4), PCATR(KRDMN,4), RANGE2)
         GFAC = 2.0D0 ** MIN (30, NBITWD)  -  4.0D0
         IF (DOWDGE.LT.2.5) CALL RCOPY (2, RANGE2, WRANG2)
         GFAC = (WRANG2(2)-WRANG2(1)) / GFAC
         GOFF = (WRANG2(2)+WRANG2(1)) / 2.0D0
         RANGE2(2) = (RANGE2(2) - GOFF) / GFAC
         RANGE2(1) = (RANGE2(1) - GOFF) / GFAC
         IHPVAL = IROUND (RANGE2(2))
         ILPVAL = IROUND (RANGE2(1))
         RANGE(1) = (ILPVAL * GFAC + GOFF)
         RANGE(2) = (IHPVAL * GFAC + GOFF)
         WRANG2(2) = (WRANG2(2) - GOFF) / GFAC
         WRANG2(1) = (WRANG2(1) - GOFF) / GFAC
         I = IROUND (WRANG2(2))
         J = IROUND (WRANG2(1))
         WRANGE(1) = (J * GFAC + GOFF)
         WRANGE(2) = (I * GFAC + GOFF)
         IF (DOWDGE.GT.0.0) THEN
            IF ((DOWDGE.LT.1.5) .OR. ((DOWDGE.GE.2.5) .AND.
     *         (DOWDGE.LT.3.5))) THEN
               IYWDGE = MAX (1.0, (TRC(2,4)-BLC(2,4))/20.0) + 2.5
            ELSE
               IXWDGE = MAX (1.0, (TRC(1,4)-BLC(1,4))/20.) + 2.5
               END IF
            IF (INCOLR.GT.0.0) THEN
               IXWDGE = ((IXWDGE+2)/3) * 3
               IYWDGE = ((IYWDGE+2)/3) * 3
               END IF
            END IF
         END IF
      GO TO 999
C                                       Not aligned adequately
 900  MSGTXT = 'INPUT IMAGES NOT SUFFICIENTLY ALIGNED'
      CALL MSGWRT (8)
C
 970  IF (.NOT.DOTV) CALL DELEXT ('PL', IVOL, ISLOT, 'READ', PCATI,
     *   IWBLK, IVER, IERR)
 975  CALL ZCLOSE (ILUN, IFIND, IERR)
C                                       Close map file.
      DO 980 K = 1,5
         IF (WASOPN(K)) CALL MAPCLS ('READ', IVOL(K), ISLOT(K), ILUN(K),
     *      IFIND(K), PCATI(1,K), F, IWBLK, IERR)
 980     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. GTPARM ERR =',I5)
 1030 FORMAT ('COULD NOT BUILD GRAPH FILE NAME, IERR=',I5)
      END
      SUBROUTINE PCNTRP (IRET)
C-----------------------------------------------------------------------
C   Plots image as requested.
C   Inputs:
C   Outputs:
C      IRET   I    Return code, 0=OK.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      REAL      YGAP, VMUL, X, Y, TEMP, JBLC(7), JTRC(7), KBLC(7),
     *   KTRC(7), COL(3)
      CHARACTER PHNAME*48, SPRTXT*8, LEVTXT*200, TXTMSG*80
      INTEGER   IGSIZE, ITYPE, IDEPTH(5), IERR, K, KK, IBLCX, IBLCY,
     *   ITRCX, ITRCY, INPIXS, INPIXT, IROUND, ICOL, IROW, IANGLE, IPOS,
     *   JPOS, KPOS, JLUN, KLUN, JIND, KIND, IC(3), J, ZMIN, TLUN,
     *   PZMAX, PZMIN, BBLC(2), BTRC(2), BXOFF, BXSIZ, DO3C, LTYPE
      LOGICAL   T, F, DOGRID, RED2B, ISRGBL
      INCLUDE 'PCNTR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
      LTYPE = MOD (ABS(ILABEL), 100)
      IANGLE = 0
      IRET = 8
C                                       Init graph file.
      IGSIZE = 1
      ITYPE = 28
      DOGRID = DOCIRC.GT.0.0
      IF (GRYSCL.GT.0.0) THEN
         CALL H2CHR (2, 1, XFUN, GPHFUN)
         IF ((GPHFUN.NE.'LG') .AND. (GPHFUN.NE.'NG') .AND.
     *      (GPHFUN.NE.'SQ') .AND. (GPHFUN.NE.'NQ') .AND.
     *      (GPHFUN.NE.'NE') .AND. (GPHFUN.NE.'L2') .AND.
     *      (GPHFUN.NE.'N2')) GPHFUN = 'LN'
         CALL CHR2H (2, GPHFUN, 1, XFUN)
         END IF
      CALL GINIT (IVOL(1), ISLOT(1), IGFILE, IGSIZE, ITYPE, INPRMS,
     *   CONTUR, DOTV, TVCHN, GRCHN, TVCORN, PCATI, PLBUF, IGLUN,
     *   IGFIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
         GO TO 970
         END IF
      GPHCUT = XDKLIN
      IF (DODARK.LE.0.0) GPHCUT = 100.0
      IF (INCOLR.GT.0.0) OFMFIL = ' '
      IF (GRYSCL.GT.0.0) THEN
         IF (DOTV) GPHDOD = (DODARK.GT.0.0)
         CALL GETOFM (OFMFIL, GPHDOT, GPHTVC, DOOFM, DOCOLR, ROFM, GOFM,
     *      BOFM, NOFM, IERR)
         IF (IERR.NE.0) THEN
            DOOFM = .FALSE.
            IERR = 0
            END IF
         IF (.NOT.DOOFM) DOCOLR = .FALSE.
      ELSE
         INCOLR = -1.0
         GPHDOD = .FALSE.
         DOOFM = .FALSE.
         DOCOLR = .FALSE.
         END IF
      IF (INCOLR.GT.0.0) DOCOLR = .TRUE.
      IF (.NOT.DOOFM) OFMFIL = ' '
      CALL CHR2H (48, OFMFIL, 1, XOMFIL)
C                                       color forced
      ISRGBL = .FALSE.
      DO 20 K = 1,30
         DO 10 J = 1,3
            IF (RGBLEV(J,K).GT.0.0) ISRGBL = .TRUE.
 10         CONTINUE
 20      CONTINUE
      ZMIN = BLC(3,5)
      IF (ZMAX.GT.ZMIN) ISRGBL = .FALSE.
      FORCEC = ISRGBL .OR. (P3COL.GT.0.0) .OR. (X3COL.GT.0.0)
      IF (DOCOLR) FORCEC = .FALSE.
C                                       Character borders
      CALL RFILL (4, 0.0, CHOUT)
      IF (.NOT.DOPOL) PAROT = 0.0
      K = 0
      IF ((LTYPE.GT.1) .AND. (LTYPE.LE.6)) THEN
         IF (DOPOL) THEN
            K = 1
            IF (PAROT.NE.0.0) K = 2
            END IF
         IF (GRYSCL.GT.0.0) K = K + 1
         END IF
      CHOUT(2) = K * 1.333
C                                       Write init commands, external
C                                       labels
      IF (RGBLAB.GE.2) ISRGBL = .FALSE.
      CALL PGLAB (YGAP, ISRGBL, LEVTXT, IERR)
      IF (IERR.NE.0) GO TO 950
      IF (BMBLNK) THEN
         CALL PLTBSZ (BMCORN, BLC, TRC, PCATR, BBLC, BTRC)
      ELSE
         BBLC(1) = 0
         BBLC(2) = 0
         BTRC(1) = 0
         BTRC(2) = 0
         END IF
C                                       Grey scale before axis labeling
      IF (GRYSCL.GT.0.0) THEN
         TXTMSG = 'Do grey scale:'
         CALL GCOMNT (2, TXTMSG, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
C                                       open other planes
         IF (INCOLR.GT.0.0) THEN
            JLUN = ILUN(5) + 1
            KLUN = ILUN(5) + 2
            CALL ZPHFIL ('MA', IVOL(4), ISLOT(4), 1, PHNAME, IERR)
            CALL ZOPEN (JLUN, JIND, IVOL(4), PHNAME, T, T, T, IERR)
            IF (IERR.EQ.0) CALL ZOPEN (KLUN, KIND, IVOL(4), PHNAME, T,
     *         T, T, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1040) 'OPEN EXTRA PLANES', IERR
               CALL MSGWRT (8)
               GO TO 950
               END IF
            CALL RCOPY (7, BLC(1,4), JBLC)
            CALL RCOPY (7, BLC(1,4), KBLC)
            CALL RCOPY (7, TRC(1,4), JTRC)
            CALL RCOPY (7, TRC(1,4), KTRC)
            BLC(3,4) = (1.0D0 - PCATD(KDCRV+2,4)) / PCATR(KRCIC+2,4)
     *         + PCATR(KRCRP+2,4)
            JBLC(3) = (2.0D0 - PCATD(KDCRV+2,4)) / PCATR(KRCIC+2,4)
     *         + PCATR(KRCRP+2,4)
            KBLC(3) = (3.0D0 - PCATD(KDCRV+2,4)) / PCATR(KRCIC+2,4)
     *         + PCATR(KRCRP+2,4)
            TRC(3,4) = BLC(3,4)
            JTRC(3) = JBLC(3)
            KTRC(3) = KBLC(3)
            END IF
         IBLCX = IROUND (BLC(1,1))
         IBLCY = IROUND (BLC(2,1))
         ITRCX = IROUND (TRC(1,1))
         ITRCY = IROUND (TRC(2,1))
         INPIXS = ITRCX - IBLCX + 1
         INPIXT = INPIXS + IXWDGE
         BXOFF = BBLC(1) - IBLCX
         BXSIZ = BTRC(1) - BBLC(1) + 1
C                                       Top step wedge
         IF (IYWDGE.GT.0) THEN
            VMUL = (WRANGE(2) - WRANGE(1)) / (TRC(1,4) - BLC(1,4))
            DO 30 ICOL = 1,INPIXS
               RLROW(ICOL) = (ICOL - 1.) * VMUL + WRANGE(1)
 30            CONTINUE
            CALL GSCALE (GPHFUN, RANGE, INPIXS, 1, RLROW, ILROW)
            IF (INCOLR.GT.0.0) THEN
               CALL FILL (INPIXT, GPHTLO, IBBUFF)
               CALL FILL (INPIXT, GPHTLO, IBLROW)
            ELSE IF (DOOFM) THEN
               CALL G3SCAL (INPIXT, ILROW, NOFM, ROFM, GOFM, BOFM,
     *            ILROW, IBBUFF, IBLROW)
            ELSE IF (FORCEC) THEN
               CALL COPY (INPIXT, ILROW, IBBUFF)
               CALL COPY (INPIXT, ILROW, IBLROW)
               END IF
            DO 40 IROW = 1,IYWDGE
               X = IBLCX
               Y = ITRCY + IROW
               CALL GPOS (X, Y, PLBUF, IERR)
               IF (IERR.NE.0) GO TO 950
               IF (INCOLR.GT.0.0) THEN
                  IF (IROW.LE.IYWDGE/3) THEN
                     CALL G3COLR (INPIXT, IANGLE, ILROW, IBBUFF, IBLROW,
     *                  PLBUF, IERR)
                  ELSE IF (IROW.LE.(2*IYWDGE)/3) THEN
                     CALL G3COLR (INPIXT, IANGLE, IBBUFF, ILROW, IBLROW,
     *                  PLBUF, IERR)
                  ELSE
                     CALL G3COLR (INPIXT, IANGLE, IBBUFF, IBLROW, ILROW,
     *                  PLBUF, IERR)
                     END IF
               ELSE IF ((DOCOLR) .OR. (FORCEC)) THEN
                  CALL G3COLR (INPIXT, IANGLE, ILROW, IBBUFF, IBLROW,
     *               PLBUF, IERR)
               ELSE
                  CALL GRAYPX (INPIXT, IANGLE, ILROW, PLBUF, IERR)
                  END IF
               IF (IERR.NE.0) GO TO 950
 40            CONTINUE
            END IF
C                                       Loop over all rows.
         CALL DBINIT (ILUN(4), IFIND(4), PCATI(1,4), BLC(1,4), TRC(1,4),
     *      JBUFSZ, BUFF, IERR)
         IF ((INCOLR.GT.0.0) .AND. (IERR.EQ.0)) THEN
            CALL DBINIT (JLUN, JIND, PCATI(1,4), JBLC, JTRC, JBUFSZ,
     *         BBUFF, IERR)
            IF (IERR.EQ.0) CALL DBINIT (KLUN, KIND, PCATI(1,4), KBLC,
     *         KTRC, JBUFSZ, RLROW, IERR)
            END IF
         IF (IERR.NE.0) GO TO 950
         DO 50 IROW = IBLCY,ITRCY
C                                       Read proper row.
            CALL MDISK ('READ', ILUN(4), IFIND(4), BUFF, IPOS, IERR)
            IF ((IERR.EQ.0) .AND. (INCOLR.GT.0.0)) THEN
               CALL MDISK ('READ', JLUN, JIND, BBUFF, JPOS, IERR)
               IF (IERR.EQ.0) CALL MDISK ('READ', KLUN, KIND, RLROW,
     *            KPOS, IERR)
               END IF
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1040) 'READ', IERR
               CALL MSGWRT (7)
               GO TO 950
               END IF
C                                       Position.
            X = IBLCX
            Y = IROW
            CALL GPOS (X, Y, PLBUF, IERR)
            IF (IERR.NE.0) GO TO 950
            IF ((IROW.GE.BBLC(2)) .AND. (IROW.LE.BTRC(2))) THEN
               CALL RFILL (BXSIZ, FBLANK, BUFF(IPOS+BXOFF))
               IF ((INCOLR.GT.0.0) .OR. (FORCEC)) THEN
                  CALL RFILL (BXSIZ, FBLANK, BBUFF(JPOS+BXOFF))
                  CALL RFILL (BXSIZ, FBLANK, RLROW(KPOS+BXOFF))
                  END IF
               END IF
            CALL GSCALE (GPHFUN, RANGE, INPIXS, 1, BUFF(IPOS),
     *         IBUFF(IPOS))
            IF (INCOLR.GT.0.0) THEN
               CALL GSCALE (GPHFUN, RANGE, INPIXS, 1, BBUFF(JPOS),
     *            IBBUFF(JPOS))
               CALL GSCALE (GPHFUN, RANGE, INPIXS, 1, RLROW(KPOS),
     *            ILROW(KPOS))
            ELSE IF (DOOFM) THEN
               JPOS = IPOS
               KPOS = IPOS
               CALL G3SCAL (INPIXS, IBUFF(IPOS), NOFM, ROFM, GOFM, BOFM,
     *            IBUFF(IPOS), IBBUFF(JPOS), ILROW(KPOS))
            ELSE IF (FORCEC) THEN
               JPOS = IPOS
               KPOS = IPOS
               CALL RCOPY (INPIXS, IBUFF(IPOS), IBBUFF(JPOS))
               CALL RCOPY (INPIXS, IBUFF(IPOS), ILROW(KPOS))
               END IF
            IF ((DOCOLR) .OR. (FORCEC)) THEN
               CALL G3COLR (INPIXS, 0, IBUFF(IPOS), IBBUFF(JPOS),
     *            ILROW(KPOS), PLBUF, IERR)
            ELSE
               CALL GRAYPX (INPIXS, 0, IBUFF(IPOS), PLBUF, IERR)
               END IF
            IF (IERR.NE.0) GO TO 950
 50         CONTINUE
C                                       add wedge to right
         IF (IXWDGE.GT.0) THEN
            VMUL = (WRANGE(2) - WRANGE(1)) / (TRC(2,4) - BLC(2,4))
            J = IXWDGE / 3
            CALL FILL (IXWDGE, GPHTLO, IBUFF)
            CALL FILL (IXWDGE, GPHTLO, IBBUFF)
            CALL FILL (IXWDGE, GPHTLO, IBLROW)
            DO 60 IROW = IBLCY,ITRCY
               X = IBLCX + INPIXS
               Y = IROW
               CALL GPOS (X, Y, PLBUF, IERR)
               IF (IERR.NE.0) GO TO 950
               TEMP = (IROW - IBLCY) * VMUL + WRANGE(1)
               CALL GSCALE (GPHFUN, RANGE, 1, 1, TEMP, IC(1))
               IF (DOOFM) CALL G3SCAL (1, IC(1), NOFM, ROFM, GOFM, BOFM,
     *            IC(1), IC(2), IC(3))
               IF (INCOLR.GT.0.0) THEN
                  CALL FILL (J, IC(1), IBUFF(1))
                  CALL FILL (J, IC(1), IBBUFF(1+J))
                  CALL FILL (J, IC(1), IBLROW(1+J+J))
                  CALL G3COLR (IXWDGE, IANGLE, IBUFF, IBBUFF, IBLROW,
     *               PLBUF, IERR)
               ELSE IF (FORCEC) THEN
                  CALL FILL (IXWDGE, IC(1), IBUFF)
                  CALL FILL (IXWDGE, IC(1), IBBUFF)
                  CALL FILL (IXWDGE, IC(1), IBLROW)
                  CALL G3COLR (IXWDGE, IANGLE, IBUFF, IBBUFF, IBLROW,
     *               PLBUF, IERR)
               ELSE IF (DOCOLR) THEN
                  CALL FILL (IXWDGE, IC(1), IBUFF)
                  CALL FILL (IXWDGE, IC(2), IBBUFF)
                  CALL FILL (IXWDGE, IC(3), IBLROW)
                  CALL G3COLR (IXWDGE, IANGLE, IBUFF, IBBUFF, IBLROW,
     *               PLBUF, IERR)
               ELSE
                  CALL FILL (IXWDGE, IC(1), IBUFF)
                  CALL GRAYPX (IXWDGE, IANGLE, IBUFF, PLBUF, IERR)
                  END IF
               IF (IERR.NE.0) GO TO 950
 60            CONTINUE
            END IF
         END IF
C                                       Draw borders.
      TXTMSG = 'Draw labels'
      CALL GCOMNT (-1, TXTMSG, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 950

      CALL GLTYPE (1, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 950
      CALL GPOS (NBLC(1), NBLC(2), PLBUF, IERR)
      IF (IERR.NE.0) GO TO 950
      CALL GVEC (NTRC(1), NBLC(2), PLBUF, IERR)
      IF (IERR.NE.0) GO TO 950
      CALL GVEC (NTRC(1), NTRC(2), PLBUF, IERR)
      IF (IERR.NE.0) GO TO 950
      CALL GVEC (NBLC(1), NTRC(2), PLBUF, IERR)
      IF (IERR.NE.0) GO TO 950
      CALL GVEC (NBLC(1), NBLC(2), PLBUF, IERR)
      IF (IERR.NE.0) GO TO 950
C                                       Line off the wedge.
      IF (IXWDGE.NE.0) THEN
         TEMP = NTRC(1) - IXWDGE - 0.2
         CALL GPOS (TEMP, NBLC(2), PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL GVEC (TEMP, NTRC(2), PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
      IF (IYWDGE.NE.0) THEN
         TEMP = NTRC(2) - IYWDGE - 0.2
         CALL GPOS (NBLC(1), TEMP, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL GVEC (NTRC(1), TEMP, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
      CALL CLAB1 (ABLC, ATRC, CHOUT, ILABEL, XYRATO, DOGRID, PLBUF,
     *   IERR)
      IF (IERR.NE.0) GO TO 950
      CALL GTIC (ILABEL, BLC(1,1), TRC(1,1), NTRC, ATRC, WRANGE, PLBUF,
     *   IERR)
      IF (IERR.NE.0) GO TO 950
C                                       Recover true LOCATI info
C                                       Draw stars
      CALL GLTYPE (4, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 950
      CALL SETLOC (IDEPTH, T)
      IF (GRYSCL.LE.0.0) IFIND(4) = 0
      CALL STARPL (STMULT, IVOL, ISLOT, INVER, ABLC, ATRC, ILUN(4),
     *   IFIND(4), PCATI, BLC, 1, PLBUF, IERR)
      IF (IERR.GE.3) GO TO 950
      IF ((LTYPE.GT.1) .AND. (LTYPE.LE.6) .AND. (DOPOL))
     *   THEN
         CALL GLTYPE (1, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL PLNLAB (YGAP, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
C                                       Write contour plot.
      IF (DOCON) THEN
         CALL GLTYPE (2, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         TXTMSG = 'Start contouring'
         CALL GCOMNT (2, TXTMSG, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         ZMIN = BLC(3,5)
C                                       color=velocity contours
         IF (ZMAX.GT.ZMIN) THEN
            DO3C = 1
            CALL H2CHR (8, 1, PCATH(KHCTP+4,5), SPRTXT)
            IF (SPRTXT(:4).EQ.'FREQ') THEN
               RED2B = PCATR(KRCIC+2,5).GT.0.0
            ELSE IF (SPRTXT(:4).EQ.'VELO') THEN
               RED2B = PCATR(KRCIC+2,5).LT.0.0
            ELSE IF (SPRTXT(:4).EQ.'FELO') THEN
               RED2B = PCATR(KRCIC+2,5).LT.0.0
               END IF
C                                       other color contours or ?
         ELSE
            DO3C = 0
            DO 80 K = 1,30
               IF ((K.EQ.1) .OR. (LEVS(K).GT.LEVS(K-1))) THEN
                  DO 75 J = 1,3
                     IF (RGBLEV(J,K).GT.0.0) DO3C = 2
 75                  CONTINUE
                  END IF
 80            CONTINUE
            END IF
         IF ((DODARK.GT.0.0) .AND. (DO3C.GT.0)) THEN
            MSGTXT = 'DODARK is not used on color contours'
            CALL MSGWRT (2)
            END IF
         DO 90 K = ZMIN,ZMAX,ZINC
C                                       color contour
            IF (DO3C.EQ.1) THEN
               X = K - ZMIN
               X = X / (ZMAX - ZMIN)
               IF (.NOT.RED2B) X = 1.0 - X
               CALL COLOR3 (X, .FALSE., COL)
               CALL G3VCOL (COL(1), COL(2), COL(3), PLBUF, IERR)
               IF (IERR.NE.0) GO TO 950
               END IF
            BLC(3,5) = K
            TRC(3,5) = K
C                                       Init contour map for double buff
C                                       CONDRW uses buffers 1 and 2
            CALL DBINIT (ILUN(5), IFIND(5), PCATI(1,5), BLC(1,5),
     *         TRC(1,5), JBUFSZ, RBLK(1,1), IERR)
            IF (IERR.NE.0) GO TO 950
            IF ((GRYSCL.GT.0.0) .AND. (DODARK.GT.0.0) .AND. (DO3C.LE.0))
     *         THEN
               CALL DBINIT (ILUN(4), IFIND(4), PCATI(1,4), BLC(1,4),
     *            TRC(1,4), JBUFSZ, RBLK(1,2), IERR)
               IF (IERR.NE.0) GO TO 950
               TLUN = ILUN(4)
            ELSE
               TLUN = -1
               END IF
            CALL CONDRW (ILUN(5), IFIND(5), TLUN, IFIND(4), CLEV,
     *         BLC(1,5), TRC(1,5), LEVS, DO3C, BBLC, BTRC, RGBLEV,
     *         PLBUF, IERR)
            IF (IERR.GT.9) GO TO 960
            IF (IERR.NE.0) GO TO 200
 90         CONTINUE
         BLC(3,5) = ZMIN
         TRC(3,5) = ZMIN
         END IF
C                                       Write polarization vector plot.
      IF (DOPOL) THEN
         KK = 3
         IF ((GRYSCL.GT.0.0) .AND. (DODARK.GT.0.0)) KK = 4
C                                        Draw polarization vectors.
         TXTMSG = 'Start polarization lines'
         CALL GCOMNT (2, TXTMSG, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL GLTYPE (3, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         PZMIN = BLC(3,1)
         IF (SPCOLR) THEN
            PZMAX = ZMAX
         ELSE
            PZMAX = PZMIN
            END IF
         IF (PZMAX.GT.PZMIN) THEN
            DO3C = 1
            P3COL = -1.0
            CALL H2CHR (8, 1, PCATH(KHCTP+4,1), SPRTXT)
            IF (SPRTXT(:4).EQ.'FREQ') THEN
               RED2B = PCATR(KRCIC+2,1).GT.0.0
            ELSE IF (SPRTXT(:4).EQ.'VELO') THEN
               RED2B = PCATR(KRCIC+2,1).LT.0.0
            ELSE IF (SPRTXT(:4).EQ.'FELO') THEN
               RED2B = PCATR(KRCIC+2,1).LT.0.0
               END IF
         ELSE
            DO3C = 0
            END IF
         DO 110 K = PZMIN,PZMAX,ZINC
C                                       color contour
            IF (DO3C.EQ.1) THEN
               X = K - ZMIN
               X = X / (ZMAX - ZMIN)
               IF (.NOT.RED2B) X = 1.0 - X
               CALL COLOR3 (X, .FALSE., COL)
               CALL G3VCOL (COL(1), COL(2), COL(3), PLBUF, IERR)
               IF (IERR.NE.0) GO TO 950
               END IF
            BLC(3,1) = K
            TRC(3,1) = K
            BLC(3,2) = K
            TRC(3,2) = K
            BLC(3,3) = K
            TRC(3,3) = K

            DO 105 J = 1,KK
               CALL DBINIT (ILUN(J), IFIND(J), PCATI(1,J), BLC(1,J),
     *            TRC(1,J), JBUFSZ, RBLK(1,J), IERR)
               IF (IERR.NE.0) GO TO 200
 105           CONTINUE
            CALL POLDRW (BBLC, BTRC, IERR)
            IF (IERR.GT.9) GO TO 960
 110        CONTINUE
         END IF
C                                       beam
      IF (BMCORN.GT.0) THEN
         CALL GLTYPE (1, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL PLTBEM (BMCORN, BLC, TRC, PCATR, PLBUF, IERR)
         IF (IERR.GT.0) GO TO 960
         END IF
C                                       rgblev display
      IF ((DO3C.EQ.2) .AND. (RGBLAB.LE.1) .AND. (DOCON)) THEN
         X = NBLC(1) + 0.04 * (NTRC(1) - NBLC(1))
         IF (RGBLAB.LE.0) THEN
            Y = NBLC(2) + 0.037 * (NTRC(2) - NBLC(2))
         ELSE
            Y = NTRC(2) - 0.05 * (NTRC(2) - NBLC(2))
            END IF
         TXTMSG = 'Draw LEVS values in RGB colors'
         CALL GCOMNT (2, TXTMSG, PLBUF, IRET)
         IF (IRET.NE.0) GO TO 950
         CALL TXRGBL (X, Y, NBLC, NTRC, LEVTXT, RGBLEV, PLBUF, IERR)
         END IF
C                                       Write sucessful finish message.
 200  IF (IERR.NE.0) GO TO 950
         CALL GFINIS (PLBUF, IERR)
         IF (IERR.NE.0) GO TO 960
            IF (.NOT.DOTV) THEN
               CALL HIPLOT (IVOL, ISLOT, IVER, IWBLK, IERR)
               CATUP = T
               WRITE (MSGTXT,1200) IVER
               CALL MSGWRT (2)
               END IF
            IRET = 0
            GO TO 980
C-----------------------------------------------------------------------
C                                       Graph writing error.
 950  WRITE (MSGTXT,1950)
      CALL MSGWRT (8)
C                                       Try to do finish.
      CALL GFINIS (PLBUF, IERR)
      IF (IERR.NE.0) GO TO 960
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (IVOL, ISLOT, IVER, IWBLK, IERR)
            CATUP = T
            END IF
         IRET = 0
         GO TO 980
C                                       Finish not sucessful. Destroy.
 960  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (IGLUN, IGFIND, IERR)
         CALL ZDESTR (IVOL, IGFILE, IERR)
         END IF
 970  IF (.NOT.DOTV) CALL DELEXT ('PL', IVOL, ISLOT, 'READ', PCATI,
     *   IWBLK, IVER, IERR)
      CALL ZCLOSE (ILUN, IFIND, IERR)
C                                       Close map file.
 980  DO 985 K = 1,5
         IF (WASOPN(K)) CALL MAPCLS ('READ', IVOL(K), ISLOT(K), ILUN(K),
     *      IFIND(K), PCATI(1,K), F, IWBLK, IERR)
 985     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GRAPH FILE INIT ERROR. GINIT ERR =',I5)
 1040 FORMAT (A,' GREY-SCALE IMAGE ERROR =',I5)
 1200 FORMAT ('Successful plot file version',I5,'  created.')
 1950 FORMAT ('Error during graphing will try to finish partial graph')
      END
      SUBROUTINE PLNLAB (YGAP, IERR)
C-----------------------------------------------------------------------
C  Write character command to graph file that says 'Pol line : 1 pixel
C  = ...'
C   Inputs
C      YGAP    R    place to write pol. vector plot line
C   Output:
C      IERR    I    error code from GCHAR.
C-----------------------------------------------------------------------
      REAL      YGAP
      INTEGER   IERR
C
      CHARACTER BJUNK*8
      INTEGER   IANGL, INCHAR, ITRIM
      REAL      DCX, DCY, VECTOR, TEMP
      INCLUDE 'PCNTR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       'Pol line : 1 pixel = ...'
      DCX = 0.0
      DCY = -YGAP
      YGAP = YGAP + 1.333
      IANGL = 0
      CALL GPOS (NBLC(1), NBLC(2), PLBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      VECTOR = 0.5 / FACTOR
      CALL H2CHR (8, 1, PCATH(KHBUN,2), BJUNK)
      IF (DOFRAC.GT.0.0) BJUNK = 'Frac pol'
      WRITE (MSGTXT,1000) VECTOR, BJUNK
C                                       Arc seconds instead
      IF ((AXTYP(LOCNUM).EQ.1) .AND. (PCATR(KRCIC,2).NE.0.0) .AND.
     *   (PCATR(KRCIC+1,2).NE.0.0)) THEN
         TEMP = MAX (ABS(PCATR(KRCIC,2)), ABS (PCATR(KRCIC+1,2))) *
     *      3600.
         VECTOR = VECTOR / TEMP
         IF (TEMP.GT.0.01) THEN
            WRITE (MSGTXT,1001) VECTOR, BJUNK
         ELSE
            VECTOR = VECTOR / 1.0E3
            WRITE (MSGTXT,1002) VECTOR, BJUNK
            END IF
         END IF
      INCHAR = ITRIM (MSGTXT)
      CALL GCHAR (INCHAR, IANGL, DCX, DCY, MSGTXT, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Add info on ROTATE
      IF (PAROT.NE.0.0) THEN
         CALL GPOS (NBLC(1), NBLC(2), PLBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         WRITE (MSGTXT,1010) PAROT
         DCY = -YGAP
         YGAP = YGAP + 1.333
         CALL REFRMT (MSGTXT, ' ', INCHAR)
         CALL GCHAR (INCHAR, IANGL, DCX, DCY, MSGTXT, PLBUF, IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Pol line: 1 pixel =',1PE12.4,1X,A8)
 1001 FORMAT ('Pol line 1 arcsec =',1PE12.4,1X,A8)
 1002 FORMAT ('Pol line 1 milli arcsec =',1PE12.4,1X,A8)
 1010 FORMAT ('Rotated by',F7.1,' degrees')
      END
      SUBROUTINE POLDRW (BBLC, BTRC, IERR)
C-----------------------------------------------------------------------
C   POLDRW will read the I, P, and A maps and add the polarization
C   vectors to the graph file.
C   Output:
C      IERR    I   the error code. 0 = ok.
C                      9 => QUIT op received from TELL
C                     10 => ABOR op received from TELL
C-----------------------------------------------------------------------
      INTEGER   BBLC(2), BTRC(2), IERR
C
      REAL      CON, P, Q, X, X0, Y, Y0, IICUT, Z1, Z2, Z3, PLEN, PA,
     *   TEMP, COL(3), FACT
      INTEGER   I, IPOS(4), IM, INCOLS, INROWS, IXINC, IYINC, K, J, MJ,
     *   BCUT, BVAL, LCORN, IBLCY, ITRCY, IROUND, ITRCX, IBLCX, BXOFF,
     *   BXSIZ
      DOUBLE PRECISION RRA, DDE, DZ
      LOGICAL   DOBACK, PERLEY
      INCLUDE 'PCNTR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      DOBACK = (GRYSCL.GT.0.0) .AND. (DODARK.GT.0.0)
      IF (DOBACK) BCUT = GPHCUT * GPHTHI + (1.0-GPHCUT) * GPHTLO
      MJ = 3
      IF (DOBACK) MJ = 4
      IBLCX = IROUND (BLC(1,1))
      IBLCY = IROUND (BLC(2,1))
      ITRCX = IROUND (TRC(1,1))
      ITRCY = IROUND (TRC(2,1))
      INCOLS = ITRCX - IBLCX + 1
      INROWS = ITRCY - IBLCY + 1
      BXOFF = BBLC(1) - IBLCX
      BXSIZ = BTRC(1) - BBLC(1) + 1
      IBLCY = BLC(2,1)
      ITRCY = TRC(2,1) + 0.5
      INROWS = TRC(2,1) - BLC(2,1) + 1.5
      INCOLS = TRC(1,1) - BLC(1,1) + 1.5
      IXINC = XINC + .5
      IYINC = YINC + .5
      CON = 3.14159 / 180.0
      PERLEY = .FALSE.
      IF (AXTYP(LOCNUM).NE.1) THEN
         MSGTXT = 'AXES NOT COORDINATE PAIR: angles may be meaningless'
         CALL MSGWRT (8)
C         IERR = 2
C         GO TO 999
         PERLEY = (CTYP(1,LOCNUM).EQ.'L') .AND. (AXINC(1,LOCNUM).GT.0.0)
     *      .AND. (CTYP(2,LOCNUM).EQ.'M')
         END IF
C                                       Find pixel value cut off.
      IICUT = ICUT
      FACT = FACTOR * MAX (ABS(PCATR(KRCIC,1)), ABS(PCATR(KRCIC+1,1)))
C                                        Loop for all rows.
      DO 100 I = IBLCY,ITRCY
         DO 25 J = 1,MJ
            CALL MDISK ('READ', ILUN(J), IFIND(J), RBLK(1,J), IPOS(J),
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            IF ((I.GE.BBLC(2)) .AND. (I.LE.BTRC(2))) THEN
               CALL RFILL (BXSIZ, FBLANK, RBLK(IPOS(J)+BXOFF,J))
               END IF
 25         CONTINUE
         IF ((MOD(I-IBLCY,IYINC).EQ.0) .OR. (IYINC.EQ.1)) THEN
            IF (DOBACK) CALL GSCALE (GPHFUN, GPHRNG, INCOLS, 1,
     *         RBLK(IPOS(4),4), IBLK(IPOS(4),4))
C                                        Yes. Do for all pixels.
            IM = -1
            DO 90 K = IBLCX,ITRCX,IXINC
               IM = K - IBLCX
C                                       Check for blanked pixels.
               Z1 = RBLK(IPOS(1)+IM,1)
               Z2 = RBLK(IPOS(2)+IM,2)
               Z3 = RBLK(IPOS(3)+IM,3)
               IF (DOBACK) BVAL = IBLK(IPOS(4)+IM,4)
               IF (Z1.EQ.FBLANK) GO TO 90
               IF (Z2.EQ.FBLANK) GO TO 90
               IF (Z3.EQ.FBLANK) GO TO 90
C                                       Check user supplied cut off.
               IF (DOCONV) THEN
                  P = SQRT (Z2*Z2 + Z3*Z3)
                  PA = 28.64789 * ATAN2 (Z3, Z2+1.0E-20) + PAROT
               ELSE
                  P = Z2
                  PA = Z3 + PAROT
                  END IF
C                                       Use these pixels.
               IF ((Z1.GE.IICUT) .AND. (P.GE.PCUT)) THEN
                  X0 = K
                  Y0 = I
                  Q = CON * PA
                  IF (CORTYP (LOCNUM).EQ.1) THEN
                     CALL XYVAL (X0, Y0, RRA, DDE, DZ, IERR)
                  ELSE
                     CALL XYVAL (X0, Y0, DDE, RRA, DZ, IERR)
                     END IF
                  IF (IERR.NE.0) GO TO 980
C                                       fractional pol
                  IF (DOFRAC.GT.0.0) P = P / Z1
                  PLEN = FACT * P
                  IF (AXTYP(LOCNUM).NE.1) THEN
                     X = X0 + PLEN * SIN (Q) / AXINC(1,LOCNUM)
                     Y = Y0 + PLEN * COS (Q) / AXINC(2,LOCNUM)
                     IF (PERLEY) X = 2 * X0 - X
                  ELSE
                     RRA = RRA + PLEN * SIN (Q) / COS (DDE * CON)
                     DDE = DDE + PLEN * COS (Q)
                     IF (CORTYP(LOCNUM).EQ.1) THEN
                        CALL XYPIX (RRA, DDE, X, Y, IERR)
                     ELSE
                        CALL XYPIX (DDE, RRA, X, Y, IERR)
                        END IF
                     IF (IERR.NE.0) GO TO 980
                     END IF
                  CALL GPOS (X, Y, PLBUF, IERR)
                  IF (IERR.NE.0) GO TO 990
                  X = 2 * X0 - X
                  Y = 2 * Y0  - Y
                  IF ((DOBACK) .AND. (BVAL.GT.BCUT)) THEN
                     CALL GDVEC (X, Y, PLBUF, IERR)
                  ELSE IF (P3COL.GT.0.0) THEN
                     TEMP = PA - P3COL
                     TEMP = MOD (TEMP+3600.0, 180.0)
                     TEMP = TEMP / 180.0
                     CALL COLOR3 (TEMP, .TRUE., COL)
                     CALL G3VCOL (COL(1), COL(2), COL(3), PLBUF, IERR)
                     IF (IERR.NE.0) GO TO 990
                     CALL G3VEC (X, Y, PLBUF, IERR)
                  ELSE IF (SPCOLR) THEN
                     CALL G3VEC (X, Y, PLBUF, IERR)
                  ELSE
                     CALL GVEC (X, Y, PLBUF, IERR)
                     END IF
                  IF (IERR.NE.0) GO TO 990
                  END IF
 90            CONTINUE
            END IF
 100     CONTINUE
C                                       POL color wheel
      IF (P3COL.GT.0.0) THEN
         LCORN = MOD (BMCORN, 5)
         K = MAX (1, INROWS / 15)
         J = MAX (1, INCOLS / 15)
         P = K*K + J*J
         P = SQRT (P)
         IF (LCORN.LE.1) THEN
            Y0 = BLC(2,1) + INROWS - 3*K
            X0 = BLC(1,1) + INCOLS - 2*J
         ELSE IF (LCORN.EQ.2) THEN
            Y0 = BLC(2,1) + INROWS - 3*K
            X0 = BLC(1,1) + 2*J
         ELSE IF (LCORN.EQ.3) THEN
            P = -P
            Y0 = BLC(2,1) + 3*K
            X0 = BLC(1,1) + 2*J
         ELSE IF (LCORN.EQ.4) THEN
            P = -P
            Y0 = BLC(2,1) + 3*K
            X0 = BLC(1,1) + INCOLS - 2*J
            END IF
         DO 140 I = 1,180,2
            PA = (I - 90) * DG2RAD
            TEMP = I - 90 - P3COL
            TEMP = MOD (TEMP+3600.0, 180.0)
            TEMP = TEMP / 180.0
            CALL COLOR3 (TEMP, .TRUE., COL)
            CALL G3VCOL (COL(1), COL(2), COL(3), PLBUF, IERR)
            IF (IERR.NE.0) GO TO 990
            X = X0 - P * SIN (PA) / 3.0
            Y = Y0 + P * COS (PA) / 3.0
            CALL GPOS (X, Y, PLBUF, IERR)
            IF (IERR.NE.0) GO TO 990
            X = X0 - P * SIN (PA)
            Y = Y0 + P * COS (PA)
            CALL G3VEC (X, Y, PLBUF, IERR)
            IF (IERR.NE.0) GO TO 990
 140        CONTINUE
         END IF
      GO TO 999
C                                       coordinate error
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (6)
      GO TO 999
C                                       plot error
 990  WRITE (MSGTXT,1990) IERR
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('POLDRW: COORDINATE ERROR',I3,' FACTOR TOO LARGE??')
 1990 FORMAT ('POLDRW: PLOT ROUTINE ERROR',I3,' QUITTING')
      END
      SUBROUTINE PGLAB (YGAP, ISRGBL, LEVTXT, IERR)
C-----------------------------------------------------------------------
C   PGLAB is an axis labelling routine for use with grey scale plots.
C   Optional contour labels.
C   Output:
C      YGAP    R        Current used lines below
C      IERR    I        error indicator:  0 = No error.
C   Inputs COMMON:
C      BLC     R(7)     bottom left hand corner of contour map.
C      BTRC    R(7)     top right hand corner of contour map.
C   In/out COMMON:
C      CHOUT   R(4)     character spacing around plot.
C      NBLC    R(2)     BLC including wedge, extra space
C      NTRC    R(2)     TRC including wedge, extra space
C      ABLC    R(2)     BLC including edge of pixels
C      ATRC    R(2)     TRC including edge of pixels
C      PLBUF   I(256)   the updated graphics output buffer.
C-----------------------------------------------------------------------
      REAL      YGAP
      LOGICAL   ISRGBL
      CHARACTER LEVTXT*(*)
      INTEGER   IERR
C
      CHARACTER SPRTXT*80, ATIME*8, ADATE*12, CHTMP*8, NAMSTR*18,
     *   PREFIX*5, CORTXT(2)*80, LTEXT(5)*80, CHTEMP*20
      DOUBLE PRECISION    ZV(3)
      REAL      DCX, DCY, BJUNK(2), TEMP, RANGES(2,3)
      INTEGER   IDEPTH(5), NTEXT, I, IANGL, INCHAR, IT(3), ID(3), ITEMP,
     *   CATEMP(256), NL, IXL, IROUND, I1, I2, IS, LEVLEN, LTYPE
      LOGICAL   SLICE, T, LFLAG, CSAME
      INCLUDE 'PCNTR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DGPH.INC'
      DATA SLICE, T /.FALSE., .TRUE./
C-----------------------------------------------------------------------
      CSAME = (IVOL(5).EQ.IVOL(4)) .AND. (ISLOT(5).EQ.ISLOT(4)) .AND.
     *   (DOCON) .AND. (GRYSCL.GT.0.0)
      NBLC(1) = BLC(1,1) - 0.7
      NBLC(2) = BLC(2,1) - 0.7
      NTRC(1) = TRC(1,1) + 0.7 + IXWDGE
      NTRC(2) = TRC(2,1) + 0.7 + IYWDGE
      ABLC(1) = NBLC(1)
      ABLC(2) = NBLC(2)
      ATRC(1) = TRC(1,1) + 0.7
      ATRC(2) = TRC(2,1) + 0.7
C
      IDEPTH(1) = BLC(3,1) + .01
      IDEPTH(2) = BLC(4,1) + .01
      IDEPTH(3) = BLC(5,1) + .01
      IDEPTH(4) = BLC(6,1) + .01
      IDEPTH(5) = BLC(7,1) + .01
      LTYPE = MOD (ABS(ILABEL), 100)
C                                       Init for line drawing.
      LOCNUM = 1
      IF ((GRYSCL.GT.0.0) .AND. (.NOT.CSAME) .AND. (DOCON)) CHOUT(4) =
     *   CHOUT(4) + 1.333
      CALL LABINI (BLC, TRC, IDEPTH, CHOUT, ILABEL, SLICE, YGAP, CORTXT,
     *   NTEXT)
C                                       wedge labeling
      IF (LTYPE.GE.3) THEN
         IF (IXWDGE.GT.0) THEN
            CALL GTICNT (ILABEL, WRANGE, I)
            IF (I.GT.0) CHOUT(3) = CHOUT(3) + I + 0.5
            END IF
         IF (IYWDGE.GT.0) CHOUT(4) = CHOUT(4) + 1.333
         END IF
C                                       Prepare LEVS lines
      NL = 0
      LEVTXT = ' '
      LEVLEN = 0
      IF ((LTYPE.LT.7) .AND. (DOCON)) THEN
         NL = NL + 1
         WRITE (LTEXT(NL),1120) CLEV
         INCHAR = 22
         WRITE (LEVTXT,1121) CLEV
         LEVLEN = 19
         DO 10 I = 1,30
            I2 = 11
            IXL = IROUND (LEVS(I))
            IF (ABS(IXL-LEVS(I)).GT.0.0001) THEN
               IF ((LEVS(I).GE.-99.90) .AND. (LEVS(I).LE.999.90))
     *            I2 = 13
               IF ((LEVS(I).GE.-9.990) .AND. (LEVS(I).LE.99.990))
     *            I2 = 14
               IF ((LEVS(I).GE.-0.9990) .AND. (LEVS(I).LE.9.9990))
     *            I2 = 15
               DCX = 10.0 ** (I2-12)
               IXL = IROUND (LEVS(I) * DCX)
               TEMP = IXL / DCX
            ELSE
               TEMP = IXL
               END IF
            WRITE (CHTEMP,1122,ERR=5) TEMP
 5          IF (CHTEMP(10:11).EQ. ' -') CHTEMP(10:11) = '-0'
            IF (CHTEMP(10:11).EQ. '  ') CHTEMP(10:11) = ' 0'
            CHTEMP(I2+1:) = ' '
            CALL CHTRIM (CHTEMP, 20, CHTEMP, IXL)
            LTEXT(NL)(INCHAR:) = CHTEMP(:IXL) // ', '
            INCHAR = INCHAR + 2 + IXL
            LEVTXT(LEVLEN+1:) = CHTEMP(:IXL)
            LEVLEN = LEVLEN + IXL + 2
C                                       Print out this line.
            IF (I.EQ.30) GO TO 15
            IF (LEVS(I+1).LE.LEVS(I)) GO TO 15
            IF (INCHAR.GE.50) THEN
               INCHAR = 1
               NL = NL + 1
               END IF
 10         CONTINUE
 15      INCHAR = INCHAR - 2
         LTEXT(NL)(INCHAR:INCHAR) = ')'
         LEVLEN = LEVLEN - 1
         LEVTXT(LEVLEN:LEVLEN) = ')'
         IF (ISRGBL) NL = 0
         CHOUT(2) = CHOUT(2) + (NL + 1) * 1.333
         END IF
C                                       Init for plotting
      CALL GINITL (NBLC, NTRC, XYRATO, CHOUT, IDEPTH, PLBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Init for grey scale.
      IF (GRYSCL.GT.0.0) THEN
         CALL H2CHR (2, 1, XFUN, GPHFUN)
         IF ((GPHFUN.NE.'LG') .AND. (GPHFUN.NE.'NG') .AND.
     *      (GPHFUN.NE.'SQ') .AND. (GPHFUN.NE.'NQ') .AND.
     *      (GPHFUN.NE.'NE') .AND. (GPHFUN.NE.'L2') .AND.
     *      (GPHFUN.NE.'N2')) GPHFUN = 'LN'
         CALL CHR2H (2, GPHFUN, 1, XFUN)
         IF ((DOCOLR) .OR. (FORCEC)) THEN
            RANGES(1,1) = RANGE(1)
            RANGES(2,1) = RANGE(2)
            RANGES(1,2) = RANGE(1)
            RANGES(2,2) = RANGE(2)
            RANGES(1,3) = RANGE(1)
            RANGES(2,3) = RANGE(2)
            CALL GINITC (ILPVAL, IHPVAL, RANGES, PLBUF, IERR)
         ELSE
            CALL GINITG (ILPVAL, IHPVAL, RANGE, PLBUF, IERR)
            END IF
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (7)
            GO TO 999
            END IF
         END IF
C                                       Extra lines (center rels)
      CALL GLTYPE (1, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (LTYPE.EQ.1) GO TO 999
      IF ((NTEXT.GT.0) .AND. (LTYPE.LT.7)) THEN
         DCX = 0.0
         IANGL = 0
         DO 25 I = 1,NTEXT
            CALL GPOS (NBLC(1), NBLC(2), PLBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            DCY = -YGAP
            CALL CHTRIM (CORTXT(I), 80, CORTXT(I), INCHAR)
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, CORTXT(I), PLBUF,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
            YGAP = YGAP + 1.333
 25         CONTINUE
         END IF
C                                       Source name, stokes, freq.
C                                       for grey scale image
      IF (LTYPE.LT.7) THEN
         IANGL = 0
         DCX = 0.0
         DCY = 0.5
         IF ((LTYPE.GE.3) .AND. (IYWDGE.GT.0)) DCY = 1.833
         IF ((GRYSCL.LE.0.0) .OR. (CSAME) .OR. (DOCON)) THEN
            CALL GPOS (NBLC(1), NTRC(2), PLBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            SPRTXT = ' '
            INCHAR = 1
            IS = 1
            IF ((DOCON) .AND. (GRYSCL.GT.0.0) .AND. (.NOT.CSAME)) THEN
               INCHAR = 7
               SPRTXT(1:5) = 'CONT:'
               IS = 5
               END IF
            CALL H2CHR (8, 1, PCATH(KHOBJ,IS), CHTMP)
            IF (CHTMP.NE.' ') THEN
               SPRTXT(INCHAR:INCHAR+7) = CHTMP(1:8)
               INCHAR = INCHAR + 10
               END IF
            IF (NCHLAB(1,LOCNUM).GT.0) THEN
               IF (INCHAR.GT.1) SPRTXT(INCHAR-1:INCHAR-1) = '_'
               SPRTXT(INCHAR:) = SAXLAB(1,LOCNUM)(:NCHLAB(1,LOCNUM))
               INCHAR = INCHAR + 3 + NCHLAB(1,LOCNUM)
               END IF
            IF (NCHLAB(2,LOCNUM).GT.0) THEN
               IF (INCHAR.GT.1) SPRTXT(INCHAR-1:INCHAR-1) = '_'
               SPRTXT(INCHAR:) = SAXLAB(2,LOCNUM)(:NCHLAB(2,LOCNUM))
               INCHAR = INCHAR + 3 + NCHLAB(2,LOCNUM)
               END IF
C                                       image name
            IF (INCHAR.GT.1) SPRTXT(INCHAR-1:INCHAR-1) = '_'
            CALL H2CHR (12, KHIMNO, PCATH(KHIMN,IS), NAMSTR(1:12))
            CALL H2CHR (6, KHIMCO, PCATH(KHIMC,IS), NAMSTR(13:18))
            CALL NAMEST (NAMSTR, PCATI(KIIMS,IS), SPRTXT(INCHAR:),
     *         ITEMP)
            CALL REFRMT (SPRTXT, '_', INCHAR)
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, PLBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
C                                       Date/time version
         IF (ILABEL.GE.0) THEN
            CALL GPOS (NBLC(1), NTRC(2), PLBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            DCY = DCY + 1.333
            IF ((GRYSCL.GT.0.0) .AND. (.NOT.CSAME) .AND. (DOCON) .AND.
     *         (LTYPE.LT.7)) DCY = DCY + 1.333
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, ATIME, ADATE)
            WRITE (SPRTXT,1030) IVER, ADATE, ATIME
            CALL REFRMT (SPRTXT, '_', INCHAR)
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, PLBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
C                                       Peak flux for grey scale
         IF (GRYSCL.GT.0.0) THEN
            DCX = 0.0
            DCY = -YGAP
            YGAP = YGAP + 1.333
            IANGL = 0
            CALL GPOS (NBLC(1), NBLC(2), PLBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL H2CHR (8, 1, PCATH(KHBUN,4), CHTMP)
            TEMP = RANGE(2) - RANGE(1)
            CALL METSCL (ILABEL, TEMP, PREFIX, LFLAG)
            IF (LFLAG) THEN
               INCHAR = 55
               WRITE (SPRTXT,1040) RANGE, CHTMP
            ELSE
               BJUNK(1) = TEMP * RANGE(1) / (RANGE(2) - RANGE(1))
               BJUNK(2) = TEMP * RANGE(2) / (RANGE(2) - RANGE(1))
               TEMP = MAX (ABS(BJUNK(1)), ABS(BJUNK(2)))
               IF (TEMP.LT.9.99) THEN
                  WRITE (SPRTXT,1041) BJUNK, PREFIX, CHTMP
                  I1 = 25
                  I2 = 33
               ELSE IF (TEMP.LT.99.9) THEN
                  WRITE (SPRTXT,1042) BJUNK, PREFIX, CHTMP
                  I1 = 26
                  I2 = 34
               ELSE IF (TEMP.LT.9999.) THEN
                  WRITE (SPRTXT,1043) BJUNK, PREFIX, CHTMP
                  I1 = 27
                  I2 = 35
               ELSE IF (TEMP.LT.9999999.) THEN
                  WRITE (SPRTXT,1044) BJUNK, PREFIX, CHTMP
                  I1 = 30
                  I2 = 40
               ELSE
                  WRITE (SPRTXT,1040) RANGE, CHTMP
                  I1 = -1
                  END IF
               IF (I1.GT.0) THEN
                  IF (SPRTXT(I1:I1+1).EQ. ' -') SPRTXT(I1:I1+1) = '-0'
                  IF (SPRTXT(I1:I1+1).EQ. '  ') SPRTXT(I1:I1+1) = ' 0'
                  IF (SPRTXT(I2:I2+1).EQ. ' -') SPRTXT(I2:I2+1) = '-0'
                  IF (SPRTXT(I2:I2+1).EQ. '  ') SPRTXT(I2:I2+1) = ' 0'
                  END IF
               END IF
            CALL REFRMT (SPRTXT, '_', INCHAR)
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, PLBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
         END IF
C                                       With Grey scale
      IF (GRYSCL.GT.0.0) THEN
         IF (.NOT.CSAME) THEN
            CALL COPY (256, PCATI(1,1), CATEMP)
            CALL COPY (256, PCATI(1,4), PCATI(1,1))
            IDEPTH(1) = BLC(3,4) + .01
            IDEPTH(2) = BLC(4,4) + .01
            IDEPTH(3) = BLC(5,4) + .01
            IDEPTH(4) = BLC(6,4) + .01
            IDEPTH(5) = BLC(7,4) + .01
            LOCNUM = 2
            CALL SETLOC (IDEPTH, T)
            IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) THEN
               DCX = (BLC(1,4) + TRC(1,4)) / 2.0
               DCY = (BLC(2,4) + TRC(2,4)) / 2.0
               CALL XYVAL (DCX, DCY, ZV(1), ZV(2), ZV(3), IERR)
               CALL AXSTRN (CTYP(3,LOCNUM), ZV(3), KLOCA(LOCNUM),
     *            NCHLAB(1,LOCNUM), SAXLAB(1,LOCNUM))
               END IF
C                                       Source name, stokes, freq.
C                                       for grey image
             IF (LTYPE.LT.7) THEN
               CALL GPOS (NBLC(1), NTRC(2), PLBUF, IERR)
               IF (IERR.NE.0) GO TO 980
               DCX = 0.0
               DCY = 0.5
               IF ((LTYPE.GE.2) .AND. (IYWDGE.GT.0)) DCY = 1.833
               IF (DOCON) DCY = DCY + 1.333
               IANGL = 0
               SPRTXT = 'GREY: '
               INCHAR = 7
               CALL H2CHR (8, 1, PCATH(KHOBJ,4), CHTMP)
               IF (CHTMP.NE.' ') THEN
                  SPRTXT(INCHAR:) = CHTMP(1:8)
                  INCHAR = INCHAR + 10
                  END IF
               IF (NCHLAB(1,LOCNUM).GT.0) THEN
                  IF (INCHAR.GT.1) SPRTXT(INCHAR-1:INCHAR-1) = '_'
                  SPRTXT(INCHAR:) = SAXLAB(1,LOCNUM)(:NCHLAB(1,LOCNUM))
                  INCHAR = INCHAR + 3 + NCHLAB(1,LOCNUM)
                  END IF
               IF (NCHLAB(2,LOCNUM).GT.0) THEN
                  IF (INCHAR.GT.1) SPRTXT(INCHAR-1:INCHAR-1) = '_'
                  SPRTXT(INCHAR:) = SAXLAB(2,LOCNUM)(:NCHLAB(2,LOCNUM))
                  INCHAR = INCHAR + 3 + NCHLAB(2,LOCNUM)
                  END IF
C                                       image name
               IF (INCHAR.GT.1) SPRTXT(INCHAR-1:INCHAR-1) = '_'
               CALL H2CHR (12, KHIMNO, PCATH(KHIMN,4), NAMSTR(1:12))
               CALL H2CHR (6, KHIMCO, PCATH(KHIMC,4), NAMSTR(13:18))
               CALL NAMEST (NAMSTR, PCATI(KIIMS,4), SPRTXT(INCHAR:),
     *            ITEMP)
               CALL REFRMT (SPRTXT, '_', INCHAR)
               CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, PLBUF,
     *            IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL COPY (256, CATEMP, PCATI(1,1))
            LOCNUM = 1
            END IF
         END IF
C                                        Peak contour flux
      IF ((DOCON) .AND. (LTYPE.LT.7)) THEN
         DCX = 0.0
         DCY = -YGAP
         YGAP = YGAP + 1.333
         IANGL = 0
         INCHAR = 40
         CALL GPOS (NBLC(1), NBLC(2), PLBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         TEMP = PCATR(KRDMX,1)
         IF (ABS(TEMP).LT.ABS(PCATR(KRDMN,1))) TEMP = PCATR(KRDMN,1)
         CALL H2CHR (8, 1, PCATH(KHBUN,1), CHTMP)
         WRITE (SPRTXT,1110) TEMP, CHTMP
         CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Write levels.
         DCX = 0.0
         IANGL = 0
         DO 130 I = 1,NL
            DCY = -YGAP
            YGAP = YGAP + 1.333
            CALL GPOS (NBLC(1), NBLC(2), PLBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL REFRMT (LTEXT(I), ' ', INCHAR)
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, LTEXT(I), PLBUF, IERR)
            IF (IERR.NE.0) GO TO 980
 130        CONTINUE
         END IF
      GO TO 999
C                                       Graph drawing error.
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GRAPH FILE INITIALIZATION ERROR. GINITL ERR =',I5)
 1010 FORMAT ('GREY SCALE INITIALIZATION ERROR. GINITG ERR =',I5)
 1030 FORMAT ('Plot file version',I4,'__created ',A12,A8)
 1040 FORMAT ('Grey scale flux range=',2(1PE12.4),1X,A8)
 1041 FORMAT ('Grey scale flux range=',2F8.3,1X,A5,A8)
 1042 FORMAT ('Grey scale flux range=',2F8.2,1X,A5,A8)
 1043 FORMAT ('Grey scale flux range=',2F8.1,1X,A5,A8)
 1044 FORMAT ('Grey scale flux range=',2F10.0,1X,A5,A8)
 1110 FORMAT ('Peak contour flux =',1PE12.4,1X,A8)
 1120 FORMAT ('Levs = ',1PE10.3,' * (')
 1121 FORMAT ('LEVS=',1PE10.3,' * (')
 1122 FORMAT (F15.3)
 1980 FORMAT ('GRAPH LABEL WRITING ERROR. IERR =',I5)
      END
