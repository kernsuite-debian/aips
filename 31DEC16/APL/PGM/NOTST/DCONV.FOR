LOCAL INCLUDE 'DCONV.INC'
C                                       Local include for DCONV
      INTEGER   JHDR(256)
      REAL      SQIN, SQLN, SQOU, DKIN, DKLN, DKOU, BMA, BMI, BPA,
     *   FACT, BLC(7), TRC(7), AP(10), BADD(10), STD, AVG, NITR
      HOLLERITH XNMIN(3), XCLIN(2), XNMLN(3), XCLLN(2), XNMOU(3),
     *   XCLOU(2), XOPC
      CHARACTER NMIN*12, CLIN*6, NMLN*12, CLLN*6, NMOU*12, CLOU*6,
     *   OPC*4
      COMMON /INPARM/ XNMIN, XCLIN, SQIN, DKIN, XNMLN, XCLLN, SQLN,
     *   DKLN, XNMOU, XCLOU, SQOU, DKOU, BLC, TRC, XOPC, FACT, STD, AVG,
     *   NITR, BMA, BMI, BPA, AP, BADD
      COMMON /CHRCOM/ NMIN, CLIN, NMLN, CLLN, NMOU, CLOU, OPC
      COMMON /MAPHDR/ JHDR
LOCAL END
      PROGRAM DCONV
C-----------------------------------------------------------------------
C! Deconvolve a moment image(?).
C# Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2007-2009
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
C   Deconvolution of (moment) maps
C   Parameters :
C   Keyword name Program name
C      USERID       USER        User number
C      INNAME       NMIN        Name input image
C      INCLASS      CLIN        Class input image
C      INSEQ        SQIN        Seq. # input image
C      INDISK       DKIN        Disk # input image
C      IN2NAME      NMLN        Name second input image
C      IN2CLASS     CLLN        Class second input image
C      IN2SEQ       SQLN        Seq. # second input image
C      IN2DISK      DKLN        Disk # second input image
C      OUTNAME      NMOU        Name output image
C      OUTCLASS     CLOU        Class output image
C      OUTSEQ       SQOU        Seq. # output image
C      OUTDISK      DKOU        Disk # output image
C      BLC           BLC        Lower left corner coordinates
C      TRC           TRC        Top right corner coordinates
C      OPCODE        OPC        'IN2C','UNWT'
C      FACTOR       FACT        Isolation criterion
C      PIXSTD        STD        Stopping criterion
C      NITER        NITR        Max. # iterations
C      BMAJ          BMA        Major axis beam (arcsec)
C      BMIN          BMI        Minor axis beam (arcsec)
C      BPA           BPA        Position angle major axis (degrees,N=>E)
C      APARM          AP        Parameters gain function (see EXPLAIN
C                               file)
C      BADDISK       IBAD       Disks to avoid for scratch
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   NMAX
      PARAMETER (NMAX = 11 * MAXIMG)
C
      CHARACTER PGMNAM*6, READ*4, WRIT*4, FINI*4, MA*2, UPDT*4, REST*4,
     *   INIT*4, CUNWT*4, BLANK*4
      INTEGER   IVOL, JVOL, LVOL, ISEQ, JSEQ, LSEQ, ITEMP, ILUN, JLUN,
     *   KLUN, LLUN, MLUN, ICN, JCN, LCN, IIND, JIND, KIND, LIND, MIND,
     *   NIND, CATBLK(256), LHDR(256), SCR(256), IER, IUSR, MYM, NTOT,
     *   NIT, ITER, NMN, NPARM, IRET, NBUF, BOF, IBFI(MABFSS),
     *   IBFJ(MABFSS), IBFK(MABFSS), IBFL(MABFSS), IBFM(MABFSS),
     *   IBFN(MABFSS), MM, LL, JJ, NXL, NYL, IVU, JVU, IVM, ILO, MX, MY,
     *   WINI(4), WINJ(4), WINL(4), POINT, PINDX, MXH, MYH, NLUN, IX,
     *   IY, JX, JY, J, I, IAV, NVALU, NISOL, N, IW, IV, IROUND, L27,
     *   L28
      HOLLERITH THDRH(256)
      REAL  CATR(256), SHDR(256), THDR(256), WT(3600), SUMW, SUMV,
     *   SIGMA, DELTA, RMSN, RMS, RMSR, RBFI(MABFSS), RBFJ(MABFSS),
     *   RBFK(MABFSS), RBFL(MABFSS), RBFM(MABFSS), RBFN(MABFSS),
     *   VU(NMAX), H(NMAX), VM(MABFSS), DRPX, DRPY, DRVX, DRVY,
     *   VAV(MABFSS), VSG(MABFSS), SUMVV, WGHT, WVEL, DELTB, EXPON, PI,
     *   DX, DY, INDE, AVCOR, AVCORN, WHM, MID, GAIN, VRES, V1, DLTOT,
     *   AVCORR
      CHARACTER HILINE*72, PHNAME*48
      DOUBLE PRECISION CATD(128), EHDR(128), FHDR(128)
      LOGICAL   T, F, OKAY, UNWT, LMTS
      INCLUDE 'DCONV.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK(1), CATR(1), CATD(1))
      EQUIVALENCE (LHDR(1), SHDR(1), EHDR(1))
      EQUIVALENCE (JHDR(1), THDR(1), FHDR(1), THDRH(1))
      EQUIVALENCE (RBFI(1), IBFI(1))
      EQUIVALENCE (RBFJ(1), IBFJ(1))
      EQUIVALENCE (RBFK(1), IBFK(1))
      EQUIVALENCE (RBFL(1), IBFL(1))
      EQUIVALENCE (RBFM(1), IBFM(1))
      EQUIVALENCE (RBFN(1), IBFN(1))
      DATA CUNWT /'UNWT'/
      DATA PGMNAM, NPARM /'DCONV ', 63/
      DATA READ, WRIT, FINI, INIT, REST, UPDT
     *   /'READ','WRIT','FINI','INIT','REST','UPDT'/
      DATA MA, BLANK /'MA','    '/
      DATA T, F / .TRUE.,.FALSE. /
      DATA BOF /1/
      DATA ILUN, JLUN, KLUN, LLUN, MLUN, NLUN /16,17,18,19,20,21/
      DATA PI, L27, L28 /3.14159265, 27,28/
C-----------------------------------------------------------------------
C                                        fill common IDCH
      CALL ZDCHIN (T)
C                                        enter catalog pointers
      CALL VHDRIN
      INDE = FBLANK
      NSCR = 0
      NCFILE = 0
      NBUF = MABFSS * 2
C                                        Get parameter values
      CALL GTPARM (PGMNAM, NPARM, RQUICK, XNMIN, SCR, IRET)
      IF (IRET.EQ.0) GO TO 100
         IF (IRET.EQ.1) GO TO 999
         WRITE (MSGTXT,5050) IRET
         IRET = 16
         CALL MSGWRT (8)
 100  IF (RQUICK) CALL RELPOP (IRET, SCR, IER)
      IF (IRET.NE.0) GO TO 990
      IRET = 8
C                                        interpret input
      DO 10 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 10      CONTINUE
      ISEQ = IROUND (SQIN)
      IVOL = IROUND (DKIN)
      LSEQ = IROUND (SQLN)
      LVOL = IROUND (DKLN)
      JSEQ = IROUND (SQOU)
      JVOL = IROUND (DKOU)
      IUSR = NLUSER
      NIT  = IROUND (NITR)
      LMTS = (AP(1).NE.0.0) .OR. (AP(2).NE.0.0)
      MID  = (AP(2) + AP(1)) / 2.0
      WHM  = (AP(2) - AP(1)) / 2.0
C                                       Convert characters
      CALL H2CHR (12, 1, XNMIN, NMIN)
      CALL H2CHR (6, 1, XCLIN, CLIN)
      CALL H2CHR (12, 1, XNMLN, NMLN)
      CALL H2CHR (6, 1, XCLLN, CLLN)
      CALL H2CHR (12, 1, XNMOU, NMOU)
      CALL H2CHR (6, 1, XCLOU, CLOU)
      CALL H2CHR (4, 1, XOPC, OPC)
      UNWT = OPC.EQ.CUNWT
      IF (NIT.EQ.0) NIT  = 1
      IF (BMA.GT.0.0) GO TO 105
         WRITE (MSGTXT,5000)
         CALL MSGWRT (8)
         GO TO 990
 105  IF (BMI.LE.0.0) BMI = BMA
C                                        open input map
      CALL MAPOPN (READ, IVOL, NMIN, CLIN, ISEQ, MA, IUSR, ILUN, IIND,
     *   ICN, CATBLK, SCR, IER)
      IF (IER.EQ.0) GO TO 110
         WRITE (MSGTXT,5100) IER
         CALL MSGWRT (8)
         GO TO 980
 110  CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IER)
      IF (IER.EQ.0) GO TO 120
         GO TO 990
 120  IX = CATBLK(KINAX)
      IY = CATBLK(KINAX+1)
C
      IF (UNWT) GO TO 140
         CALL MAPOPN (READ, LVOL, NMLN, CLLN, LSEQ, MA, IUSR, LLUN,
     *      LIND, LCN, LHDR, SCR, IER)
         IF (IER.EQ.0) GO TO 130
            WRITE (MSGTXT,5100) IER
            CALL MSGWRT (8)
            GO TO 980
 130     OKAY = (CATBLK(KINAX).EQ.LHDR(KINAX)) .AND.
     *      (CATBLK(KINAX+1).EQ.LHDR(KINAX+1)) .AND.
     *      (CATR(KRCIC).EQ.SHDR(KRCIC)) .AND.
     *      (CATR(KRCIC+1).EQ.SHDR(KRCIC+1))
         DRPX = (SHDR(KRCRP) - CATR(KRCRP)) * CATR(KRCIC)
         DRPY = (SHDR(KRCRP+1) - CATR(KRCRP+1)) * CATR(KRCIC+1)
         DRVX = EHDR(KDCRV) - CATD(KDCRV)
         DRVY = EHDR(KDCRV+1) - CATD(KDCRV+1)
         OKAY = (OKAY) .AND. (ABS(DRPX-DRVX).LE.1.0E-6 * ABS(DRPX))
     *      .AND. (ABS(DRPY-DRVY).LE.1.0E-6 * ABS(DRPY))
         IF (OKAY) GO TO 140
            WRITE (MSGTXT,5120)
            CALL MSGWRT (8)
            GO TO 990
C                                        fill in output header
 140  CALL COPY (256, CATBLK, JHDR)
      IF (NMOU(1:4).EQ.BLANK(1:4)) NMOU = NMIN
      IF (CLOU(1:4).EQ.BLANK(1:4)) CLOU = CLIN
C                                        name, class, sequence
      CALL CHR2H (12, NMOU, KHIMNO, THDRH(KHIMN))
      CALL CHR2H (6, CLOU, KHIMCO, THDRH(KHIMC))
      JHDR(KIIMS) = JSEQ
C                                        points/axis, reference pixel
      JHDR(KINAX) = TRC(1) - BLC(1) + 1.0001
      JHDR(KINAX+1) = TRC(2) - BLC(2) + 1.0001
      THDR(KRCRP) = CATR(KRCRP)   - BLC(1) + 1.0
      THDR(KRCRP+1) = CATR(KRCRP+1) - BLC(2) + 1.0
      JX = JHDR(KINAX)
      JY = JHDR(KINAX+1)
C                                        map must be of type REAL
      THDR(KRBLK) = INDE
C                                        create output map using JHDR
      CALL MCREAT (JVOL, JCN, SCR, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,5150) IER
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       copy keywords
      CALL KEYPCP (IVOL, ICN, JVOL, JCN, 0, ' ', IER)
C                                     *  open output map. MAPOPN is not
C                                     *  used, since MAPOPN with WRITE
C                                     *  implies exclusive use.
 150  CALL ZPHFIL (MA, JVOL, JCN, 1, PHNAME, IER)
C                                        open output map NOT exclusive
      CALL ZOPEN (JLUN, JIND, JVOL, PHNAME, T, F, T, IER)
      IF (IER.EQ.0) GO TO 160
         WRITE (MSGTXT,5200) IER
         CALL MSGWRT (8)
         GO TO 980
C                                        initialize reading
 160  WINI(1) = BLC(1) + 0.001
      WINI(2) = BLC(2) + 0.001
      WINI(3) = TRC(1) + 0.001
      WINI(4) = TRC(2) + 0.001
      CALL MINIT (READ, ILUN, IIND, IX, IY, WINI, IBFI, NBUF, BOF, IER)
      IF (IER.EQ.0) GO TO 170
         WRITE (MSGTXT,5300) IER
         CALL MSGWRT (8)
         GO TO 980
C                                        initialize writing
 170  WINJ(1) = 1
      WINJ(2) = 1
      WINJ(3) = JX
      WINJ(4) = JY
      CALL MINIT (WRIT, JLUN, JIND, JX, JY, WINJ, IBFL, NBUF, BOF, IER)
      IF (IER.EQ.0) GO TO 180
         WRITE (MSGTXT,5300) IER
         CALL MSGWRT (8)
         GO TO 980
C                                        read and write
 180  DO 200 J = 1,JY
         CALL LINIO (READ, ILUN, IIND, RBFI, JX, VU, 1, INDE, IER)
         IF (IER.EQ.0) GO TO 190
            WRITE (MSGTXT,5400) IER
            CALL MSGWRT (8)
            GO TO 980
 190     CALL LINIO (WRIT, JLUN, JIND, RBFL, JX, VU, 1, INDE, IER)
         IF (IER.EQ.0) GO TO 200
            WRITE (MSGTXT,5400) IER
            CALL MSGWRT (8)
            GO TO 980
 200     CONTINUE
C                                        flush buffer
      CALL LINIO (FINI, JLUN, JIND, RBFL, JX, VU, 1, INDE, IER)
C                                        open output map using new LUN
C                                        to call MINIT with READ later
      CALL ZOPEN (KLUN, KIND, JVOL, PHNAME, T, F, T, IER)
      IF (IER.EQ.0) GO TO 210
         WRITE (MSGTXT,5200) IER
         CALL MSGWRT (8)
         GO TO 980
 210  CALL BUNDEL (BMA, BMI, BPA, WT, MX, MY)
      IF (JX*MX.LE.NMAX) GO TO 220
         ITEMP = JX * MX
         WRITE (MSGTXT,5510) ITEMP, NMAX
         CALL MSGWRT (5)
         GO TO 980
 220  MXH = MX / 2
      MYH = MY / 2
      NXL = JX - MX + 1
      NYL = JY - MY + 1
      MYM = MY - 1
C                                        define new windows
      DO 230 I = 1,4
         WINL(I) = WINI(I)
 230     CONTINUE
      WINI(2) = WINI(2) + MYH
      WINI(4) = WINI(4) - MYH
C                                        fill arrays VAV, VSG, and H
      DO 240 I = 1,JX
         VAV(I) = INDE
         VSG(I) = INDE
         DO 240 J = 1,MY
            H(I + (J - 1) * JX) = 1.0
 240     CONTINUE
      IF (LMTS) GO TO 455
C-----------------------------------------------------------------------
C                                        scratch file intermezzo
C                                        create REAL scratch files
      IAV = 1
      CALL MAPSNC (2, JHDR(KINAX), IAV, MLUN, MIND, IBFM, IER)
      CALL MAPSNC (2, JHDR(KINAX), IAV, NLUN, NIND, IBFN, IER)
      IF (IER.EQ.0) GO TO 250
         WRITE (MSGTXT,6000) IER
         CALL MSGWRT (5)
         GO TO 980
 250  IF (UNWT) GO TO 260
C                                        prepare reading HI map
         CALL MINIT (READ, LLUN, LIND, IX, IY, WINL, IBFL, NBUF, BOF,
     *      IER)
         IF (IER.EQ.0) GO TO 260
            WRITE (MSGTXT,5300) IER
            CALL MSGWRT (8)
            GO TO 980
C                                        prepare reading map VU
 260  CALL MINIT (READ, KLUN, KIND, JX, JY, WINJ, IBFK, NBUF, BOF, IER)
      IF (IER.EQ.0) GO TO 270
         WRITE (MSGTXT,5300) IER
         CALL MSGWRT (8)
         GO TO 980
C                                        read first MY-1 lines to H
 270  IF (UNWT) GO TO 300
         PINDX = 0
         POINT = PINDX * JX + 1
         DO 290 J = 1, MYM
            CALL LINIO (READ, LLUN, LIND, RBFL, JX, H, POINT, INDE, IER)
            IF (IER.EQ.0) GO TO 280
               WRITE (MSGTXT,5400) IER
               CALL MSGWRT (8)
               GO TO 980
C                                        new pointer and -index
 280        PINDX = MOD (PINDX + 1, MY)
            POINT = PINDX * JX + 1
 290        CONTINUE
C                                        read first MY-1 lines to VU
C                                        pointer index, pointer
 300  PINDX = 0
      POINT = PINDX * JX + 1
      DO 320 J = 1, MYM
         CALL LINIO (READ, KLUN, KIND, RBFK, JX, VU, POINT, INDE, IER)
         IF (IER.EQ.0) GO TO 310
            WRITE (MSGTXT,5400) IER
            CALL MSGWRT (8)
            GO TO 980
C                                        new pointer and -index
 310     PINDX = MOD (PINDX + 1, MY)
         POINT = PINDX * JX + 1
 320     CONTINUE
C                                        prepare writing scratch files
      CALL MINIT (WRIT, MLUN, MIND, JX, JY, WINJ, IBFM, NBUF, BOF, IER)
      CALL MINIT (WRIT, NLUN, NIND, JX, JY, WINJ, IBFN, NBUF, BOF, IER)
      IF (IER.EQ.0) GO TO 330
         WRITE (MSGTXT,5300) IER
         CALL MSGWRT (5)
         GO TO 980
C                                        min. # points in beam within
C                                        5% level. (1.08 = ln20 / ln16)
 330  DX = -CATR(KRCIC)  * 3600.0
      DY = CATR(KRCIC+1) * 3600.0
      NMN = 1.08 * PI * BMA * BMI / DX / DY * FACT
      NVALU = 0
      NISOL = 0
C                                        write blanks in first lines
      DO 350 JJ = 1,MYH
         CALL LINIO (WRIT, MLUN, MIND, RBFM, JX, VAV, 1, INDE, IER)
         CALL LINIO (WRIT, NLUN, NIND, RBFN, JX, VSG, 1, INDE, IER)
         IF (IER.EQ.0) GO TO 350
            WRITE (MSGTXT,5400) IER
            CALL MSGWRT (8)
            GO TO 980
 350     CONTINUE
C                                        begin loop
      DO 430 JJ = 1,NYL
C                                        m-value central line
         MM = JJ + BLC(2) - 0.9999 + MYH
         IF (UNWT) GO TO 360
C                                        read next line in H
            CALL LINIO (READ, LLUN, LIND, RBFL, JX, H, POINT, INDE, IER)
            IF (IER.EQ.0) GO TO 360
               WRITE (MSGTXT,5400) IER
               CALL MSGWRT (8)
               GO TO 980
C                                        read next line in VU
 360     CALL LINIO (READ, KLUN, KIND, RBFK, JX, VU, POINT, INDE, IER)
         IF (IER.EQ.0) GO TO 370
            WRITE (MSGTXT,5400) IER
            CALL MSGWRT (8)
            GO TO 980
C                                        slide along central line
 370     DO 410 ILO = 1,NXL
C                                        l-value central point
            LL = ILO + BLC(1) - 0.9999 + MXH
            SUMW = 0.0
            SUMV = 0.0
            SUMVV = 0.0
C                                        index central point in VM
            IVM = ILO + MXH
C                                        index start central line in VU
            JVU = 1 + JX * MOD (MYH + 1 + PINDX, MY)
C                                        index central point in VU,H
            IVU = IVM + JVU - 1
            VAV(IVM) = INDE
            IF ((VU(IVU).EQ.INDE) .OR. (H(IVU).EQ.INDE)) GO TO 410
C                                        calculation in one point
               N = 0
               DO 390 J = 1,MY
                  DO 380 I = 1,MX
C                                        index in array WT
                     IW = I + (J - 1) * MX
C                                        index in arrays VU,H
                     IV = ILO + I - 1 + JX * MOD (J + PINDX, MY)
                     IF ((VU(IV).EQ.INDE) .OR. (H(IV).EQ.INDE))
     *                  GO TO 380
                     IF (WT(IW).EQ.0.0) GO TO 380
C                                        omit central point
                     IF (IV.EQ.IVU) GO TO 380
                        N = N + 1
                        WGHT = H(IV) * WT(IW)
                        WVEL = WGHT  * VU(IV)
                        SUMW = SUMW  + WGHT
                        SUMV = SUMV  + WVEL
                        SUMVV = SUMVV + WVEL * VU(IV)
 380                 CONTINUE
 390              CONTINUE
               NVALU = NVALU + 1
               IF (N.LT.NMN) GO TO 400
                  VAV(IVM) = SUMV  / SUMW
                  VSG(IVM) = SUMVV / SUMW - VAV(IVM) ** 2
                  IF (VSG(IVM).LT.0.0) VSG(IVM) = 0.0
                  VSG(IVM) = SQRT (VSG(IVM))
                  GO TO 410
 400           NISOL = NISOL + 1
 410           CONTINUE
C                                        write line to scratch maps
         CALL LINIO (WRIT, MLUN, MIND, RBFM, JX, VAV, 1, INDE, IER)
         CALL LINIO (WRIT, NLUN, NIND, RBFN, JX, VSG, 1, INDE, IER)
         IF (IER.EQ.0) GO TO 420
            WRITE (MSGTXT,5400) IER
            CALL MSGWRT (8)
            GO TO 980
C                                        new pointer index
 420     PINDX = MOD (PINDX + 1, MY)
         POINT = PINDX * JX + 1
C
 430     CONTINUE
C                                        write blanks in last lines
      DO 440 I = 1,JX
         VAV(I) = INDE
         VSG(I) = INDE
 440     CONTINUE
      DO 450 JJ = 1,MYH
         CALL LINIO (WRIT, MLUN, MIND, RBFM, JX, VAV, 1, INDE, IER)
         CALL LINIO (WRIT, NLUN, NIND, RBFN, JX, VSG, 1, INDE, IER)
         IF (IER.EQ.0) GO TO 450
            WRITE (MSGTXT,5400) IER
            CALL MSGWRT (8)
            GO TO 980
 450     CONTINUE
C                                        flush buffers
      CALL LINIO (FINI, MLUN, MIND, RBFM, JX, VAV, 1, INDE, IER)
      CALL LINIO (FINI, NLUN, NIND, RBFN, JX, VSG, 1, INDE, IER)
      WRITE (MSGTXT,5600) NVALU,NISOL
      CALL MSGWRT (5)
C                                        end scratch file intermezzo
C-----------------------------------------------------------------------
 455  ITER  = 1
      WRITE (MSGTXT,5520)
      CALL MSGWRT (5)
C                                        start iterations ************
 460  CONTINUE
C                                        (re)set variables
      SIGMA  = 0.0
      NTOT   = 0
      AVCORN = 0.0
      IF (UNWT) GO TO 470
C                                        prepare reading HI map
         CALL MINIT (READ, LLUN, LIND, IX, IY, WINL, IBFL, NBUF, BOF,
     *      IER)
         IF (IER.EQ.0) GO TO 470
            WRITE (MSGTXT,5300) IER
            CALL MSGWRT (8)
            GO TO 980
C                                        prepare reading map VU
 470  CALL MINIT (READ, KLUN, KIND, JX, JY, WINJ, IBFK, NBUF, BOF, IER)
      IF (IER.EQ.0) GO TO 480
         WRITE (MSGTXT,5300) IER
         CALL MSGWRT (8)
         GO TO 980
C                                        read first MY-1 lines to H
 480  IF (UNWT) GO TO 510
         PINDX = 0
         POINT = PINDX * JX + 1
         DO 500 J = 1,MYM
            CALL LINIO (READ, LLUN, LIND, RBFL, JX, H, POINT, INDE, IER)
            IF (IER.EQ.0) GO TO 490
               WRITE (MSGTXT,5400) IER
               CALL MSGWRT (8)
               GO TO 980
C                                        new pointer and -index
 490        PINDX = MOD (PINDX + 1, MY)
            POINT = PINDX * JX + 1
 500        CONTINUE
C                                        read first MY-1 lines to VU
C                                        pointer index, pointer
 510  PINDX = 0
      POINT = PINDX * JX + 1
      DO 530 J = 1,MYM
         CALL LINIO (READ, KLUN, KIND, RBFK, JX, VU, POINT, INDE, IER)
         IF (IER.EQ.0) GO TO 520
            WRITE (MSGTXT,5400) IER
            CALL MSGWRT (8)
            GO TO 980
C                                        new pointer and -index
 520     PINDX = MOD (PINDX + 1, MY)
         POINT = PINDX * JX + 1
 530     CONTINUE
C                                        prepare reading orig. map VM
      CALL MINIT (READ, ILUN, IIND, IX, IY, WINI, IBFI, NBUF, BOF, IER)
      IF (IER.EQ.0) GO TO 540
         WRITE (MSGTXT,5300) IER
         CALL MSGWRT (8)
         GO TO 980
C                                        prepare reading scratch maps
 540  IF (LMTS) GO TO 545
         CALL MINIT (READ, MLUN, MIND, JX, JY, WINJ, IBFM, NBUF, BOF,
     *      IER)
         CALL MINIT (READ, NLUN, NIND, JX, JY, WINJ, IBFN, NBUF, BOF,
     *      IER)
C                                        prepare writing output map
 545  CALL MINIT (WRIT, JLUN, JIND, JX, JY, WINJ, IBFJ, NBUF, BOF, IER)
      IF (IER.EQ.0) GO TO 550
         WRITE (MSGTXT,5300) IER
         CALL MSGWRT (8)
         GO TO 980
C                                        write blanks in first lines
 550  DO 560 JJ = 1,MYH
         IF (LMTS) GO TO 555
            CALL LINIO (READ, MLUN, MIND, RBFM, JX, VAV, 1, INDE, IER)
            CALL LINIO (READ, NLUN, NIND, RBFN, JX, VSG, 1, INDE, IER)
 555     CALL LINIO (WRIT, JLUN, JIND, RBFJ, JX, VAV, 1, INDE, IER)
         IF (IER.EQ.0) GO TO 560
            WRITE (MSGTXT,5400) IER
            CALL MSGWRT (8)
            GO TO 980
 560        CONTINUE
C                                        begin loop
      DO 680 JJ = 1,NYL
C                                        m-value central line
         MM = JJ + BLC(2) - 0.9999 + MYH
         IF (UNWT) GO TO 570
C                                        read next line in H
            CALL LINIO (READ, LLUN, LIND, RBFL, JX, H, POINT, INDE, IER)
            IF (IER.EQ.0) GO TO 570
               WRITE (MSGTXT,5400) IER
               CALL MSGWRT (8)
               GO TO 980
C                                        read next line in VU
 570     CALL LINIO (READ, KLUN, KIND, RBFK, JX, VU, POINT, INDE, IER)
         IF (IER.EQ.0) GO TO 580
            WRITE (MSGTXT,5400) IER
            CALL MSGWRT (8)
            GO TO 980
C                                        read comp. line from VM
 580     CALL LINIO (READ, ILUN, IIND, RBFI, JX, VM, 1, INDE, IER)
         IF (IER.EQ.0) GO TO 590
            WRITE (MSGTXT,5400) IER
            CALL MSGWRT (8)
            GO TO 980
C                                        read line from scratch #1
 590     IF (LMTS) GO TO 610
            CALL LINIO (READ, MLUN, MIND, RBFM, JX, VAV, 1, INDE, IER)
            IF (IER.EQ.0) GO TO 600
               WRITE (MSGTXT,5400) IER
               CALL MSGWRT (8)
               GO TO 980
C                                        read line from scratch #2
 600        CALL LINIO (READ, NLUN, NIND, RBFN, JX, VSG, 1, INDE, IER)
            IF (IER.EQ.0) GO TO 610
               WRITE (MSGTXT,5400) IER
               CALL MSGWRT (8)
               GO TO 980
C                                        slide along central line
 610     DO 660 ILO = 1,NXL
C                                        l-value central point
            LL   = ILO + BLC(1) - 0.9999 + MXH
            SUMV = 0.0
            SUMW = 0.0
            N = 0
C                                        index corrected pixel VM,VAV
            IVM = ILO + MXH
C                                        index start central line VU
            JVU = 1 + JX * MOD (MYH + 1 + PINDX, MY)
C                                        index corrected pixel VU,H
            IVU = IVM + JVU - 1
C                                        test if correctable pixel
            IF ((VU(IVU).EQ.INDE) .OR. (H(IVU).EQ.INDE)) GO TO 660
            IF ((.NOT.LMTS) .AND. (VAV(IVM).EQ.INDE)) GO TO 660
            IF ((LMTS) .AND. ((VU(IVU).LT.AP(1)) .OR.
     *         (VU(IVU).GT.AP(2)))) GO TO 660
C                                        correct one pixel
               DO 630 J = 1,MY
                  DO 620 I = 1,MX
C                                        index in array WT
                     IW = I + (J - 1) * MX
C                                        index in array VU
                     IV = ILO + I - 1 + JX * MOD (J + PINDX, MY)
                     IF ((VU(IV).EQ.INDE) .OR. (H(IV).EQ.INDE))
     *                  GO TO 620
                     IF ((LMTS) .AND. ((VU(IV).LT.AP(1)) .OR.
     *                   (VU(IV).GT.AP(2))))  GO TO 620
                     IF (WT(IW).EQ.0.0) GO TO 620
                        N = N + 1
                        WGHT = H(IV) * WT(IW)
                        SUMV = SUMV + WGHT * VU(IV)
                        SUMW = SUMW + WGHT
 620                 CONTINUE
 630              CONTINUE
               IF (N.LT.1) GO TO 660
C                                        correction
               VRES  = SUMV / SUMW
               DELTA = VM(IVM) - VRES
               GAIN  = 1.0
C                                        modify correction
               IF (LMTS) GO TO 640
                  DELTB = VU(IVU) - VAV(IVM)
                  IF (VSG(IVM).EQ.0.0) GO TO 650
                  GAIN  = 0.0
                  EXPON = (DELTB/VSG(IVM))**2 / 2.0
                  IF (EXPON.LT.2.0)  GAIN = EXP (-EXPON)
                  GO TO 650
 640           IF (AP(3).EQ.1.0) GO TO 645
C                                        triangular gain
                  GAIN = MAX (1.0-ABS(VU(IVU)-MID)/WHM, 0.0)
                  GO TO 650
C                                        rectangular gain
 645           CONTINUE
                  V1 = MIN (VU(IVU)+DELTA, AP(2))
                  V1 = MAX (V1, AP(1))
                  DELTA = V1 - VU(IVU)
 650           DELTA = DELTA * GAIN
               VU(IVU) = VU(IVU) + DELTA
               SIGMA = SIGMA + DELTA * DELTA
               NTOT = NTOT + 1
               DLTOT = ABS (VM(IVM) - VU(IVU))
               AVCORN = ((NTOT - 1) * AVCORN + DLTOT) / NTOT
 660           CONTINUE
C                                        write one corrected line
         CALL LINIO (WRIT, JLUN, JIND, RBFJ, JX, VU, JVU, INDE, IER)
         IF (IER.EQ.0) GO TO 670
            WRITE (MSGTXT,5400) IER
            CALL MSGWRT (8)
            GO TO 980
C                                        new pointer index
 670     PINDX = MOD (PINDX + 1, MY)
         POINT = PINDX * JX + 1
 680     CONTINUE
C                                        write blanks in last lines
      DO 690 JJ = 1,MYH
         IF (LMTS) GO TO 685
            CALL LINIO (READ, MLUN, MIND, RBFM, JX, VAV, 1, INDE, IER)
            CALL LINIO (READ, NLUN, NIND, RBFN, JX, VSG, 1, INDE, IER)
 685     CALL LINIO (WRIT, JLUN, JIND, RBFJ, JX, VAV, 1, INDE, IER)
         IF (IER.EQ.0) GO TO 690
            WRITE (MSGTXT,5400) IER
            CALL MSGWRT (8)
            GO TO 980
 690     CONTINUE
C                                        flush buffer
      CALL LINIO (FINI, JLUN, JIND, RBFJ, JX, VU, JVU, INDE, IER)
      RMSN   = SQRT (SIGMA / NTOT)
      IF (ITER.EQ.1) GO TO 700
         RMSR   = 100.0 * (RMS / RMSN - 1.0)
         AVCORR = 100.0 * (1.0 - AVCOR / AVCORN)
C                                        intermediate output line
         WRITE (MSGTXT,5800)   ITER, RMSN, RMSR, AVCORN, AVCORR
         CALL MSGWRT (5)
C                                        end criterion reached ?
         IF (RMSN.LT.STD.OR.ITER.GE.NIT.OR.AVCORR.LE.AVG) GO TO 720
         GO TO 710
 700  CONTINUE
         WRITE (MSGTXT,5900) ITER,RMSN,AVCORN
         CALL MSGWRT (5)
         IF (ITER.GE.NIT) GO TO 720
 710  ITER  = ITER + 1
      RMS   = RMSN
      AVCOR = AVCORN
      GO TO 460
C                                        end iterations ***********
C                                        close maps
 720  CALL MAPCLS (READ, IVOL, ICN, ILUN, IIND, CATBLK, F, SCR, IER)
      CALL MAPCLS (INIT, JVOL, JCN, JLUN, JIND, JHDR, T, SCR, IER)
      CALL MAPCLS (READ, JVOL, JCN, KLUN, KIND, JHDR, F, SCR, IER)
      IF (.NOT.UNWT) CALL MAPCLS (READ, LVOL, LCN, LLUN, LIND, LHDR, F,
     *   SCR, IER)
C                                        add history file
 730  CALL HIINIT (3)
      CALL HISCOP (L27, L28, IVOL, JVOL, ICN, JCN, JHDR, SCR, LHDR,
     *   IER)
      IF (IER.GE.4) GO TO 740
      CALL HENCO1 (PGMNAM, NMIN, CLIN, ISEQ, IVOL, L28, LHDR, IER)
      IF (IER.NE.0) GO TO 740
      CALL HENCO2 (PGMNAM, NMLN, CLLN, LSEQ, LVOL, L28, LHDR, IER)
      IF (IER.NE.0) GO TO 740
      CALL HENCOO (PGMNAM, NMOU, CLOU, JSEQ, JVOL, L28, LHDR, IER)
      IF (IER.NE.0) GO TO 740
      WRITE (HILINE,6075) BMA,BMI,BPA,ITER
      CALL HIADD (L28, HILINE, LHDR, IER)
      IF (IER.NE.0) GO TO 740
      CALL HICLOS (L28, T, LHDR, IER)
      IF (IER.NE.0) GO TO 740
      CALL CATIO (UPDT, JVOL, JCN, JHDR, REST, SCR, IER)
      IF (IER.EQ.0) GO TO 750
C                                       problems with history file
 740     WRITE (MSGTXT,6100)
         CALL MSGWRT (8)
         GO TO 990
C
 750  IRET = 0
      GO TO 990
C
 980  CALL MAPCLS (READ, IVOL, ICN, ILUN, IIND, CATBLK, F, SCR, IER)
 981  CALL MAPCLS (WRIT, JVOL, JCN, JLUN, JIND, CATBLK, F, SCR, IER)
 982  CALL MAPCLS (READ, JVOL, JCN, KLUN, KIND, CATBLK, F, SCR, IER)
      IF (.NOT.UNWT) CALL MAPCLS (READ, LVOL, LCN, LLUN, LIND, LHDR, F,
     *   SCR, IER)
C
 990  CALL DIE (IRET, SCR)
 999  STOP
C-----------------------------------------------------------------------
 5000 FORMAT ('Illegal BMAJ :  fatal...')
 5050 FORMAT ('GTPARM : IRET = ',I2)
 5100 FORMAT ('MAPOPN : IER  = ',I2)
 5120 FORMAT ('DCONV  : input maps not equivalent - FATAL')
 5150 FORMAT ('MCREAT : IER  = ',I2)
 5200 FORMAT ('ZOPEN  : IER  = ',I2)
 5300 FORMAT ('MINIT  : IER  = ',I2)
 5400 FORMAT ('LINIO  : IER  = ',I2)
 5510 FORMAT ('size x-axis * size beam is ',I5,' cannot exceed ',I5)
 5520 FORMAT (' iteration  rms    decrease (%) tot.av.corr.',
     *        'increase (%)')
 5600 FORMAT (1X,I5,' non-blanked values, ',I5,' isolated')
 5800 FORMAT (4X,I3,2X,G11.4,2X,G9.2,2X,G11.4,2X,G9.2)
 5900 FORMAT (4X,I3,2X,G11.4,13X,G11.4)
 6000 FORMAT ('MAPSNC : IER  = ',I2)
 6075 FORMAT (' major/minor axes:',2(1X,F5.1),' pos. angle: ',F6.1,I3,
     *        ' iterations')
 6100 FORMAT ('error in creating of/adding to history file')
      END
      SUBROUTINE BUNDEL (BMA, BMI, BPA, WT, NX, NY)
C-----------------------------------------------------------------------
C   Fills an array WT with weights according to a gaussian beam
C   with a major axis of BMA arcsec and a position angle of BPA
C   degrees, and a minor axis of BMI arcsec.  The weights start
C   in the lower left corner, and first fill the first row with
C   NX values.  Eventually NY rows are filled this way. The sum
C   of all weights is normalized to unity.
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NXH, NYH, I, J, K, CATBLK(256)
      REAL      BMA, BMI, BPA, WT(3600), X, Y, C4LN2, CRAD, SPA, CPA,
     *   EX, DX, DY, CATR(256), CMA, CMI, SMA, SMI, SUM
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK(1), CATR(1), CATD(1))
      DATA C4LN2, CRAD /2.7725887, 57.295780/
C-----------------------------------------------------------------------
      DX  = -CATR(KRCIC) * 3600.0
      DY  = CATR(KRCIC+1) * 3600.0
      SPA = SIN (BPA / CRAD)
      CPA = COS (BPA / CRAD)
C                                        1.04 is sqrt (ln 20 / ln 16)
C                                        to define 5% level.
      CMA = BMA * CPA * 1.04
      CMI = BMI * CPA * 1.04
      SMA = BMA * SPA * 1.04
      SMI = BMI * SPA * 1.04
C                                        beam surface
      NX  = 2 * SQRT (CMA * CMA + SMI * SMI) / DX
      NY  = 2 * SQRT (CMI * CMI + SMA * SMA) / DY
      NXH = NX / 2
      NYH = NY / 2
      NX  = 2 * NXH + 1
      NY  = 2 * NYH + 1
      K   = 0
      SUM = 0.0
C                                        fill array WT
      DO 20 J = 1,NY
         Y = (J - NYH - 1) * DY
         DO 10 I = 1,NX
            K  = K + 1
            X  = (I - NXH - 1) * DX
            EX = C4LN2 * ((X / BMA * SPA - Y / BMA * CPA) ** 2 +
     *                    (X / BMI * CPA + Y / BMI * SPA) ** 2)
            WT(K) = 0.0
            IF (EX.LT.3.0)  WT(K) = EXP (-EX)
            SUM = SUM + WT(K)
 10         CONTINUE
 20      CONTINUE
C                                        normalize
      K = 0
      DO 40 J = 1,NY
         DO 30 I = 1,NX
            K = K + 1
            WT(K) = WT(K) / SUM
 30         CONTINUE
 40      CONTINUE
C
 999  RETURN
      END
