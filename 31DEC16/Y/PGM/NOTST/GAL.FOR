LOCAL INCLUDE 'GAL.INC'
C                                       Local include for GAL
      INTEGER   MAXP
      PARAMETER (MAXP = 1536*1536)
      INTEGER   MRING
      PARAMETER (MRING=500)
C
      CHARACTER NAMEIN*12, CLASIN*6, NAMEWT*12, CLASWT*6, NAMEOU*12,
     *   CLASOU*6, TYPE*2, INFILE*48, OUTFIL*48
      CHARACTER HED(8)*4
      HOLLERITH XNAMEI(3), XCLASI(2), XNAMEW(3), XCLASW(2), XNAMEO(3),
     *   XCLASO(2), XTYPE, XINFIL(12), XOUTFL(12)
      INTEGER   WIN(4), NXW, NYW, IT, JBL, FIX(8)
      REAL      PRUSER, SEQIN, DISKIN, SEQWT, DISKWT, SEQOU, DISKOU,
     *   BLC(7), TRC(7), TOL, APM(10), CPM(10), DPM(10), ERRV, WT(MAXP),
     *   XSYM, XFACT, DX, DY, RMIN, RMAX, CMIN, CMAX, SMIN, SMAX, XDOTV,
     *   XGRCH, VOBS(MAXP), SWT, SWT2, SWV, SWV2, AVRES, STDEV
      DOUBLE PRECISION XPAR(10)
      COMMON /CHRCOM/ NAMEIN, CLASIN, NAMEWT, CLASWT, NAMEOU, CLASOU,
     *   TYPE, HED, INFILE, OUTFIL
      COMMON /INPARM/ PRUSER, XNAMEI, XCLASI, SEQIN, DISKIN, XNAMEW,
     *   XCLASW, SEQWT, DISKWT, XNAMEO, XCLASO, SEQOU, DISKOU, BLC,
     *   TRC, TOL, XTYPE, APM, CPM, DPM, XINFIL, XOUTFL, ERRV, XSYM,
     *   XFACT, XDOTV, XGRCH
      COMMON /VELGAL/ XPAR, WT, DX, DY, RMIN, RMAX, CMIN, CMAX, SMIN,
     *   SMAX, VOBS, WIN, NXW, NYW, IT, JBL, FIX, SWT, SWT2, SWV,
     *   SWV2, AVRES, STDEV
LOCAL END
LOCAL INCLUDE 'GAL2.INC'
C                                       GAL plotting include
      INTEGER   CATGAL(256), ICN, JVOL, LUN, FIND, SCRTCH(256),
     *   IBLK(256), IVER
      LOGICAL   AXES, TV
      REAL      SCX, OFX, SCY, OFY, BLC(2), TRC(2)
      COMMON /PLOT/ CATGAL, SCRTCH, SCX, OFX, SCY, OFY, BLC, TRC, IBLK,
     *   ICN, JVOL, LUN, FIND, AXES, TV, IVER
LOCAL END
      PROGRAM GAL
C-----------------------------------------------------------------------
C! Performs velocity fitting
C# MAP Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2002, 2004-2006, 2008-2012, 2014-2015
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
C   Program GAL fits a model velocity field to an input frequency field
C   and uses a least squares algorithm to determine 5 parameters speci-
C   fying the orientation and position of the galaxy, and 3 parameters
C   specifying the rotation curve. It produces an outmap map which is
C   the difference of the observation and the fitted model.   Adverbs :
C      INNAME ... Image name(name).  blank=>any
C      INCLASS .. Image name(class). blank=>any
C      INSEQ .... Image name(seq).  0=>any
C      INDISK ... Disk drive # of image.  0=>any
C      OUTNAME... Output image name(name).   blank=>INNAME
C      OUTCLASS.. Output image name(class).  blank=>INCLASS
C      OUTSEQ.... Output image name(seq).    blank=>next unique
C      OUTDISK... Disk drive # output image.  0=>any
C      BLC ...... Bottom Left hand pixel of subimage. 0,0=>1,1
C      TRC ...... Top Right hand pixel of subimage. 0,0=> maximum
C      SOLCON ... Criterion to stop fitting iterations. 0=>0.001
C      FUNCTYPE . Type of rotation curve :
C                     'BR' fits 3 parameter 'Brandt' curve :
C                                         R / Rmax
C                   V / Vmax   =   ---------------------- n 3/2n
C                                  (1/3 + 2/3 * (R / Rmax) )
C                     'EX' fits 2 parameter flat curve :
C                                       - ln(100) * (R / Rmax)
C                   V / Vmax   =   1 - e
C                     'CC' fits 1 parameter flat curve :
C                   V / Vmax   =   1
C                     'SB' fits 1 parameter 'solid body' curve :
C                   V / Vmax   =   R / 60
C      APARM ....     1 - 8 : First guesses to eight unknown parameters.
C                 1 : x pixel of center, 2 : y pixel of center.
C                 3 : position angle receding part major axis (degr.).
C                 4 : inclination plane (degr.), 0 : face-on.
C                 5 : systemic velocity, 6 : Vmax, 7 : Rmax (arcsec),
C                 8 : exponent n.  0=>1.0
C                     9 - 10: Control type of output.
C      CPARM      (1) : 0 : no residual output map; 1 : residual map
C                 using (fitted) parameters, 2 : like 1, but limits
C                 in radius not used for residual map, 3: residual map
C                 using INFILE. (2) : 0 : no plot rotation curve; 1 :
C                 plot curve. (3) : parameters to be held fixed. (4):
C                 use IN2C as weight map. (5): copy HI file. (6) what
C                 to put in OUTFILE.
C      DPARM .... (1),(2) Limits in radius, (3),(4) Limits cos(azimuth
C                 angle), (5),(6) Limits sine(idem), (7) max. radius in
C                 plot, (8) max. rot. velocity in plot (9) 0: fit rota-
C                 tion curve, 1: use input parameters, (10) width
C                 annuli in drawing rotation curve in pixel increments.
C      INFILE ... Text file containing rotation curve informaton (only
C                 used when CPARM(1 = 3).
C      OUTFILE... File to write end results to.
C      PIXSTD ... Expected rms error in velocity at one pixel. 0=>10
C-----------------------------------------------------------------------
      INCLUDE 'GAL.INC'
C                                                 long integers
      CHARACTER PGMNAM*6, HILINE*72, STAT*4, CHSIGN*1, XUNIT*8, YUNIT*8,
     *   MTYPE*2
      INTEGER   M,MTOT,JMX,MX,MXNBL,J,JM,NFIT,NPAR,INF,NWA,FLG,JW
C                                                 declare second header
      INTEGER            IHED(256)
      REAL               RHED(256)
      DOUBLE PRECISION   DHED(256)
      INCLUDE 'INCS:PMAD.INC'
C                                                short integers
      INTEGER  NPARMS, IRET, SCR(256), ISEQ, JSEQ, JERR, USID, L16,
     *   PNTR, NX, NY, VIN(4), WSEQ, WVOL, BLKOF, IY, IND, IPARMS,
     *   IPVT(8), NA, NBUF, IPVT2(8), IM1, NFIX, L, BLOK(256), IVOL, MM,
     *   HMR(2), HMD(2), L27, L28, JCN, WCN, IERR, IMIN, IMAX, IX, I,
     *   IOUT, OSEQ, ICN, JVOL, TXLUN, TXLEN, LENOUT, TXFIND, JTRIM
      CHARACTER FCH(8), LINE*100
      REAL FMN, FMX, RBUF(MABFSS), ERRR, ERRD, PIX(2), RMX, RINC, VMX,
     *   XY, RMN, VMN, RSEC, DSEC, XB, XE, VMEN(MRING), VRMS(MRING),
     *   RMEN(MRING), RR, RX, RY, PARMS(97)
      DOUBLE PRECISION FVEC(MAXP), WA(MAXP), XFIT(8), FJAC(8, 8),
     *   DER(10), WA1(8), WA2(8), WA3(8), DTOL, POS(3), A2R
      LOGICAL   T, F, RQUICK, WGHT, CPHI, APPEND
C
      EXTERNAL FCN
C
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DDCH.INC'
C                                       commons & equivalences
      EQUIVALENCE (IHED, RHED, DHED)
      EQUIVALENCE (PARMS, PRUSER)
C                                       data statements
      DATA FMN, FMX /1.0E20, -1.0E20/
      DATA PGMNAM, NPARMS /'GAL  ', 97/
      DATA BLKOF /1/
      DATA TXLUN, L16, L27, L28 /10, 16, 27, 28/
      DATA STAT /'READ'/
      DATA T, F, APPEND /.TRUE., .FALSE., .TRUE./
      DATA A2R  /1.7453292519943296D-02/
C-----------------------------------------------------------------------
      MX = MAXP
      MXNBL = MX
      NWA = MX
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL GTPARM (PGMNAM, NPARMS, RQUICK, PRUSER, SCR, IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .FALSE.
         IF (IRET.EQ.1) GO TO 999
         IRET = 16
         WRITE (MSGTXT,1000)
         CALL MSGWRT (9)
         END IF
      IF (IRET.NE.0) GO TO 990
      IRET  = 8
      LOCNUM = 1
      PRUSER = NLUSER
C                                         Hollerith to Character
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLASI, CLASIN)
      CALL H2CHR (12, 1, XNAMEO, NAMEOU)
      CALL H2CHR (6, 1, XCLASO, CLASOU)
      CALL H2CHR (12, 1, XNAMEW, NAMEWT)
      CALL H2CHR (6, 1, XCLASW, CLASWT)
      CALL H2CHR (2, 1, XTYPE, TYPE)
      CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL H2CHR (48, 1, XOUTFL, OUTFIL)
C                                         Interpret other input
      ISEQ  = SEQIN   + 0.01
      IVOL  = DISKIN  + 0.01
      WSEQ  = SEQWT   + 0.01
      WVOL  = DISKWT  + 0.01
      USID  = PRUSER  + 0.01
      NFIX  = CPM(3)  + 0.01
      WGHT  = CPM(4).GT.0.0
      CPHI  = CPM(5).GT.0.0
      RMIN  = DPM(1)
      RMIN  = MAX (RMIN,0.0)
      RMAX  = DPM(2)
      CMIN  = DPM(3)
      CMAX  = DPM(4)
      SMIN  = DPM(5)
      SMAX  = DPM(6)
      IF (SMIN.GE.SMAX) THEN
         SMIN = -1.00001
         SMAX =  1.00001
         END IF
      IF (CMIN.GE.CMAX) THEN
         CMIN = -1.00001
         CMAX =  1.00001
         END IF
      IF (TOL.EQ.0.0)      TOL     = 0.001
      IF (APM(7).EQ.0.0)   APM(7)  = 120.0
      IF (APM(8).EQ.0.0)   APM(8)  = 1.0
      IF (ERRV.EQ.0.0)     ERRV    = 10.0
      IF (RMAX.EQ.0.0)     RMAX    = 9999.0
      IF (DPM(10).LE.0.0)  DPM(10) = 3.0
C                                         Use of INFILE implies no fit
      IF (CPM(1).GT.2.5)   CPM(7) = 1.0
C                                         items in header line
      HED(1) = '  x '
      HED(2) = '  y '
      HED(3) = ' pa '
      HED(4) = '  i '
      HED(5) = 'Vsys'
      HED(6) = 'Vmax'
      HED(7) = 'Rmax'
      HED(8) = '  n '
C                                         open input image
      MTYPE = 'MA'
      CALL MAPOPN ('READ', IVOL, NAMEIN, CLASIN, ISEQ, MTYPE, USID, L16,
     *   PNTR, ICN, CATBLK, SCR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 980
         END IF
C                                         store inputs in J---
      JVOL   = IVOL
      JSEQ   = ISEQ
      DTOL   = TOL
      DX     = CATR(KRCIC) * (-3600.0)
      DY     = CATR(KRCIC+1) * 3600.0
      NA     = CATBLK(KIDIM)
      JCN    = ICN
      NBUF   = 2 * MABFSS
C
      CALL WINDOW (NA, CATBLK(KINAX), BLC ,TRC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 980
         END IF
C
      NX     = CATBLK(KINAX)
      NY     = CATBLK(KINAX + 1)
      WIN(1) = BLC(1)
      WIN(2) = BLC(2)
      WIN(3) = TRC(1)
      WIN(4) = TRC(2)
      NXW    = WIN(3) - WIN(1) + 1
      NYW    = WIN(4) - WIN(2) + 1
C
      IF (NXW*NYW.GT.MX) THEN
         NX = NXW * NYW
         NY = SQRT (MX+0.1)
         WRITE (MSGTXT,1030) NX, MX, NY, NY
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 980
         END IF
C                                          initialize reading
      MTOT = NXW * NYW
      CALL MINIT (STAT, L16, PNTR, NX, NY, WIN, RBUF, NBUF, BLKOF,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 980
         END IF
C                                         read image line by line
      J  = 0
      JM = 0
      DO 130 IY = 1,NYW
C
         CALL MDISK (STAT, L16, PNTR, RBUF, IND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            CALL MSGWRT (8)
            RQUICK = .FALSE.
            GO TO 980
            END IF
C
         DO 120 IX = 1,NXW
            JM = JM + 1
            IF (RBUF(IND+IX-1).EQ.FBLANK) THEN
               VOBS(JM) = FBLANK
            ELSE
               VOBS(JM) = RBUF(IND+IX-1) / 1000.0
               J = J + 1
               END IF
 120        CONTINUE
 130     CONTINUE
C                                         M (index J) # not blanked.
C                                         MTOT (index JM) total #.
      M = J
      WRITE (MSGTXT,1060) MTOT, M
      CALL MSGWRT (5)
      IF (M.GT.MXNBL) THEN
         WRITE (MSGTXT,1065) MXNBL
         CALL MSGWRT (8)
         END IF
C                                         close input image
      CALL MAPCLS (STAT, IVOL, ICN, L16, PNTR, CATBLK, F, SCR, IERR)
      IMIN = MAX (INT (RMIN), 0)
      IMAX = MIN (INT (RMAX), 9999)
      WRITE (MSGTXT,1070)   TYPE, IMIN, IMAX
      CALL MSGWRT (5)
      WRITE (MSGTXT,1075)   CMIN, CMAX
      CALL MSGWRT (5)
      WRITE (MSGTXT,1078)   SMIN, SMAX
      CALL MSGWRT (5)
C                                         temporary header storage
      DO 5900 J = 1, 64
         DHED(J) = CATD(J)
 5900    CONTINUE
C                                         open weight image
      IF (WGHT) THEN
C
         WRITE (MSGTXT,3200)
         CALL MSGWRT (8)
C
         CALL MAPOPN ('READ', WVOL, NAMEWT, CLASWT, WSEQ, MTYPE, USID,
     *                L16, PNTR, WCN, CATBLK, SCR, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (8)
            RQUICK = .FALSE.
            GO TO 980
            END IF
C                                         Catch gross mistakes only:
C                                         check dimensions and pixel
C                                         increments.
         IF (IHED(KIDIM)  .NE.CATBLK(KIDIM)                  .OR.
     *       IHED(KINAX)  .NE.CATBLK(KINAX)                  .OR.
     *       IHED(KINAX+1).NE.CATBLK(KINAX+1)                .OR.
     *       ABS(RHED(KRCIC)  /CATR(KRCIC)  -1.).GT.1.E-5  .OR.
     *       ABS(RHED(KRCIC+1)/CATR(KRCIC+1)-1.).GT.1.E-5 ) THEN
            WRITE (MSGTXT,2200)
            CALL MSGWRT (8)
            GO TO 980
            END IF
         CALL MINIT (STAT, L16, PNTR, NX, NY, WIN, RBUF, NBUF, BLKOF,
     *               IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR
            CALL MSGWRT (8)
            RQUICK = .FALSE.
            GO TO 980
            END IF
C                                         read image line by line
         JM  = 0
         JW  = 0
         SWT = 0.0
         DO 6130 IY = 1, NYW
            CALL MDISK (STAT, L16, PNTR, RBUF, IND, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1050) IERR
               CALL MSGWRT (8)
               RQUICK = .FALSE.
               GO TO 980
               END IF
            DO 6120 IX = 1, NXW
               JM = JM + 1
               IF (RBUF(IND+IX-1).NE.FBLANK) THEN
                  WT(JM) = RBUF(IND+IX-1)
                  SWT    = SWT  + WT(JM)
                  JW     = JW   + 1
               ELSE
                  WT(JM) = 0.0
                  END IF
 6120          CONTINUE
C
 6130       CONTINUE
         CALL MAPCLS (STAT, WVOL, WCN, L16, PNTR, CATBLK, F, SCR, IERR)
C                                         let weights be of order unity
         DO 6140 J = 1, JM
            WT(J) = WT(J) * JW / SWT
 6140       CONTINUE
C
      ELSE
C                                         no weight map specified
         WRITE (MSGTXT,3201)
         CALL MSGWRT (8)
         DO 6145 J = 1, JM
            WT(J) = 1.0
 6145       CONTINUE
            END IF
C                                         finish condition WT map yes/no
C
C A few remarks about the parameters used: the contents of APARM are
C stored in the array APM, which is copied to the array XPAR (REAL*8).
C The real reason for this copying is to make it possible to use the
C values in a COMMON block. This is not possible for APM, since APM
C already has to be in the common INPARM. Furthermore, APM stays the
C same throughout the program, whereas XPAR gets updated at every
C iteration. XPAR contains NPAR values, where NPAR depends on the
C type of rotation curve specfified. The array to be fitted (this is
C XPAR without the parameters to be held fixed) is called XFIT, and
C contains NFIT values. Clearly XFIT and NFIT are supplied to the
C fitting procedure; internally (in FCN) the updated values of XFIT
C are used to update XPAR.
C                                         APM, index J: whole list
C                                         XFIT, index I: to be fitted
      IF (TYPE.EQ.'SB') THEN
         NPAR = 6
C                                        NFIX = NFIX.OR.11: at least
C                                        parms 1,2 and 4 are fixed.
         NFIX = NFIX + MOD (NFIX+1,2)
         NFIX = NFIX + MOD (NFIX/2+1,2) * 2
         NFIX = NFIX + MOD (NFIX/8+1,2) * 8
      ELSE IF (TYPE.EQ.'CC'.OR.TYPE.EQ.'DC') THEN
         NPAR = 6
C         IF (TYPE.EQ.'DC') CALL INTEGRATE (RINC,XPAR,RMEN,VMEN,VRMS,
C     1                                     RMX,VMX,JMX)
      ELSE IF (TYPE.EQ.'EX') THEN
         NPAR = 7
      ELSE
         TYPE = 'BR'
         NPAR = 8
         END IF
C
      IF (NFIX.EQ.2**NPAR-1)  CPM(7) = 1.0
      IF (CPM(7).GT.0.0)      NFIX   = 0
      I      = 0
      J      = 0
 138  J      = J + 1
C                           Now use only XPAR (REAL*8), not APM
      XPAR(J) = APM(J)
      MM      = NFIX / 2
      FIX(J)  = NFIX - 2 * MM
      IF (FIX(J).NE.1) THEN
         I        = I + 1
         HED(I)   = HED(J)
         XFIT (I) = XPAR(J)
         FCH(J)   = ' '
      ELSE
         WRITE (MSGTXT,1005) HED(J),XPAR(J)
         CALL MSGWRT (5)
         FCH(J)   = '*'
         END IF
      IF (J.LT.NPAR) THEN
         NFIX  = MM
         GO TO 138
         END IF
C                                         # parms to be fitted
      NFIT = I
      IF (NFIT.LT.1) THEN
         MSGTXT = ' no parameters left to fit:'
         CALL MSGWRT (8)
         CPM(7) = 1.0
         END IF
C                                         want fit?
      IT = 0
      IF (CPM(7).LE.0.0) THEN
         CALL LMSTR1 (FCN, M, NFIT, XFIT, FVEC, FJAC, 8, DTOL, INF,
     *      IPVT, WA, NWA)
C                                         output results, update XPAR
         I = 0
         J = 0
 148     I = I + 1
 149     J = J + 1
         IF (FIX(J).EQ.1)  GO TO 149
         XPAR(J) = XFIT(I)
         IF (I.LT.NFIT) GO TO 148
C
         WRITE (MSGTXT,1080) INF
         CALL MSGWRT (5)
         WRITE (MSGTXT,1090) IT
         CALL MSGWRT (5)
         WRITE (MSGTXT,1100) (XFIT(I), I = 1, NFIT)
         CALL MSGWRT (5)
C                                         computation covariance matrix,
C                                         prepare calling of COVAR,
C                                         zero lower triangle of FJAC
         DO 155 I = 2, NFIT
            IM1 = I - 1
            DO 150 J = 1, IM1
               FJAC(I,J) = 0.0
 150            CONTINUE
 155         CONTINUE
C                                         QR factorization of FJAC
         CALL QRFAC (NFIT, NFIT, FJAC, 8, T, IPVT2, NFIT, WA1, WA2, WA3)
C
         DO 160 J = 1, NFIT
            FJAC(J,J) = WA1(J)
            L = IPVT2(J)
            IPVT2(J) = IPVT(L)
 160        CONTINUE
C
         DTOL = 1.0E-6
         CALL COVAR (NFIT, FJAC, 8, IPVT2, DTOL, WA1)
         DO 170 I = 1, NFIT
            FJAC(I,I) = ERRV * SQRT (FJAC(I,I))
 170        CONTINUE
C
         WRITE (MSGTXT,1100) (FJAC(J,J), J = 1, NFIT)
         CALL MSGWRT (5)
         PIX(1) = XPAR(1)
         PIX(2) = XPAR(2)
         CALL MP2SKY (PIX, POS, JERR)
         IF (JERR.EQ.0) THEN
            CALL COORDD (1, POS(1), CHSIGN, HMR, RSEC)
            CALL COORDD (2, POS(2), CHSIGN, HMD, DSEC)
            WRITE (MSGTXT,1120) HMR,RSEC,FCH(1),CHSIGN,HMD,DSEC,FCH(2)
            CALL MSGWRT (5)
            IF (FIX(1).NE.1.AND.FIX(2).NE.1) THEN
               ERRR = FJAC(1,1) * DX / 15.0 / COS (POS(2) * A2R)
               ERRD = FJAC(2,2) * DY
               WRITE (MSGTXT,1125) ERRR,ERRD
               CALL MSGWRT (5)
               END IF
            END IF
C                        no fit; remind user
      ELSE
         MSGTXT = ' *** no fitting; input parameters used ' //
     *      'throughout ***'
         CALL MSGWRT (8)
         END IF
      DO 175 IX = 1,NPAR
         APM(IX) = XPAR(IX)
 175     CONTINUE
C                                       resume now
      IF (RQUICK) THEN
         CALL PTPARM (10, APM, SCR, IERR)
         CALL RELPOP (0, SCR, IERR)
         END IF
C
      IF (CPM(1).GT.0.0) THEN
C                        residual map: fill FVEC with correct values.
C                        Use FCN if parameters are to be used, use HCN
C                        if information is to be read from INFILE.
         IF (CPM(1).LT.2.5) THEN
            FLG = CPM(1) - 2
            CALL FCN (M, NFIT, XFIT, FVEC, DER, FLG)
         ELSE
            CALL HCN (M, FVEC, -1)
            END IF
C                                       get header of first map back
         DO 5950 J = 1, 64
            CATD(J) = DHED(J)
 5950       CONTINUE
         STAT = 'WRIT'
         IVOL = DISKOU + 0.01
         OSEQ = SEQOU  + 0.01
         CALL MAKOUT (NAMEIN, CLASIN, ISEQ, '      ', NAMEOU, CLASOU,
     *      OSEQ)
         CALL CHR2H (12, NAMEOU(1:12),KHIMNO, CATR(KHIMN))
         CALL CHR2H (6, CLASOU(1:6),KHIMCO, CATR(KHIMC))
         CATBLK(KIIMS) = OSEQ
         CALL SUBHDR (BLC, TRC, 1.0, 1.0)
         CALL MCREAT (IVOL, ICN, SCR, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1130) IERR
            CALL MSGWRT (8)
            GO TO 980
            END IF
C                                       open new map
         ISEQ = CATBLK(KIIMS)
         CALL MAPOPN ('INIT', IVOL, NAMEOU, CLASOU, ISEQ, MTYPE, USID,
     *      L16, PNTR, ICN, CATBLK, SCR, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (8)
            GO TO 980
            END IF
C                                       initialize writing
         BLKOF  = 1
         VIN(1) = 1
         VIN(2) = 1
         VIN(3) = NXW
         VIN(4) = NYW
         CALL MINIT (STAT, L16, PNTR, NXW, NYW, VIN, RBUF, NBUF, BLKOF,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR
            CALL MSGWRT (8)
            GO TO 980
            END IF
C                                      find min and max, update header
         JBL = 0
         DO 230 J = 1, M
 210        JM = J + JBL
            IF (VOBS(JM).EQ.FBLANK) THEN
               JBL = JBL + 1
               GO TO 210
            ELSE
               IF (FVEC(J).NE.FBLANK) THEN
                  FVEC(J)  = - FVEC(J) * 1000.0
                  IF (FMN.GT.FVEC(J)) FMN = FVEC(J)
                  IF (FMX.LT.FVEC(J)) FMX = FVEC(J)
                  END IF
               END IF
 230        CONTINUE
         CATR(KRDMN) = FMN
         CATR(KRDMX) = FMX
C                                       write line by line
         J  = 0
         JM = 0
         DO 280 IY = 1, NYW
            CALL MDISK (STAT, L16, PNTR, RBUF, IND, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1050) IERR
               CALL MSGWRT (8)
               GO TO 980
               END IF
            DO 270 IX = 1, NXW
               JM = JM + 1
               IF (VOBS(JM).NE.FBLANK) THEN
                  J = J + 1
                  RBUF(IND+IX-1) = FVEC(J)
               ELSE
                  RBUF(IND+IX-1) = FBLANK
                  END IF
 270           CONTINUE
 280        CONTINUE
C
         CALL MDISK  ('FINI', L16, PNTR, RBUF, IND, IERR)
         CALL MAPCLS ('INIT', IVOL, ICN, L16, PNTR, CATBLK, T, SCR,
     *      IERR)
C                                      add history file
         CALL KEYPCP (JVOL, JCN, IVOL, ICN, 0, ' ', IERR)
         CALL HIINIT (3)
         IF (CPHI) THEN
            CALL HISCOP (L27, L28, JVOL, IVOL, JCN, ICN, CATBLK, SCR,
     *         BLOK, IERR)
         ELSE
            CALL HICREA (L28,IVOL,ICN,CATBLK,BLOK,IERR)
            END IF
         IF (IERR.GE.4) GO TO 290
         CALL HENCO1 (PGMNAM, NAMEIN, CLASIN, JSEQ, JVOL, L28, BLOK,
     *      IERR)
         IF (IERR.NE.0) GO TO 290
         CALL HENCOO (PGMNAM, NAMEOU, CLASOU, ISEQ, IVOL, L28, BLOK,
     *      IERR)
         IF (IERR.NE.0) GO TO 290
         IF (CPM(7).LE.0.0) THEN
            WRITE (MSGTXT,2000) PGMNAM, TYPE
            HILINE = MSGTXT
            CALL HIADD (L28, HILINE, BLOK, IERR)
            WRITE (MSGTXT,2005) PGMNAM
            HILINE = MSGTXT
            CALL HIADD (L28, HILINE, BLOK, IERR)
         ELSE
            WRITE (MSGTXT,2008) PGMNAM
            HILINE = MSGTXT
            CALL HIADD (L28, HILINE, BLOK, IERR)
            END IF
         IF (IERR.NE.0) GO TO 290
         WRITE (MSGTXT,2010) PGMNAM, (XPAR(I)-WIN(I)+1.0,FCH(I), I=1,2),
     *      XPAR(3), FCH(3), XPAR(4), FCH(4)
         HILINE = MSGTXT
         CALL HIADD (L28, HILINE, BLOK, IERR)
         IF (IERR.NE.0) GO TO 290
         WRITE (MSGTXT,2020) PGMNAM, (XPAR(I),FCH(I),I=5,NPAR)
         HILINE = MSGTXT
         CALL HIADD (L28, HILINE, BLOK, IERR)
         IF (IERR.NE.0) GO TO 290
         WRITE (MSGTXT,2025) PGMNAM, HMR, RSEC, FCH(1), CHSIGN, HMD,
     *      DSEC, FCH(2)
         HILINE = MSGTXT
         CALL HIADD (L28, HILINE, BLOK, IERR)
         IF (IERR.NE.0) GO TO 290
         CALL HICLOS (L28, T, BLOK, IERR)
         IF (IERR.NE.0) GO TO 290
         CALL CATIO ('UPDT', IVOL, ICN, CATBLK, 'REST', SCR, IERR)
         IF (IERR.EQ.0) GO TO 300
 290        WRITE (MSGTXT,2030)
            CALL MSGWRT (8)
            GO TO 990
C                                      history written
 300     IRET = 0
C                                      end residual map
         END IF
C                                      output file
      LENOUT = JTRIM (OUTFIL)
      IF (LENOUT.GT.0) THEN
         CALL ZTXOPN ('WRIT', TXLUN, TXFIND, OUTFIL(1:LENOUT),
     *                 APPEND, IERR)
         IF (IERR.EQ.0) THEN
            IF (CPM(6).EQ.0) THEN
               WRITE (LINE,3600) DPM(1), DPM(2), (XPAR(I), I = 1,8),
     *            AVRES, STDEV
            ELSE
               RR = (DPM(1)+DPM(2))/2.0
C                                      RX and RY in pixels. receding
C                                      side: CPM(6)=1; approaching
C                                      side: CPM(6)=-1.
               RX = -RR * SIN (XPAR(3)*A2R) / DX
               RY =  RR * COS (XPAR(3)*A2R) / DY
               PIX(1) = XPAR(1) + RX
               PIX(2) = XPAR(2) + RY
               CALL MP2SKY (PIX, POS, JERR)
               IF (JERR.EQ.0) THEN
                  CALL COORDD (1, POS(1), CHSIGN, HMR, RSEC)
                  CALL COORDD (2, POS(2), CHSIGN, HMD, DSEC)
                  WRITE (LINE,3700) HMR, RSEC, CHSIGN, HMD, DSEC
                  TXLEN = JTRIM (LINE)
                  IF (TXLEN.GT.0) THEN
                     CALL ZTXIO ('WRIT', TXLUN, TXFIND, LINE(1:TXLEN),
     *                  IERR)
                     IF (IERR.NE.0) THEN
                        WRITE (MSGTXT,3400) TXLUN, IERR
                        CALL MSGWRT (7)
                        END IF
                     END IF
                  END IF
               RX = RR * SIN (XPAR(3)*A2R) / DX
               RY = -RR * COS (XPAR(3)*A2R) / DY
               PIX(1) = XPAR(1) + RX
               PIX(2) = XPAR(2) + RY
               CALL MP2SKY (PIX, POS, JERR)
               IF (JERR.EQ.0) THEN
                  CALL COORDD (1, POS(1), CHSIGN, HMR, RSEC)
                  CALL COORDD (2, POS(2), CHSIGN, HMD, DSEC)
                  WRITE (LINE,3700) HMR, RSEC, CHSIGN, HMD, DSEC
               ELSE
                  LINE = ' '
                  END IF
               END IF
            TXLEN = JTRIM (LINE)
            IF (TXLEN.GT.0) THEN
               CALL ZTXIO ('WRIT', TXLUN, TXFIND, LINE(1:TXLEN), IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,3400) TXLUN, IERR
                  CALL MSGWRT (7)
                  END IF
               END IF
         ELSE
            IRET = 4
            WRITE (MSGTXT,3500) IERR
            CALL MSGWRT (6)
            TXLUN = 0
            END IF
         END IF
C
      IF (CPM(2).GT.0.0) GO TO 330
         IRET = 0
         GO TO 990
C                                      calculate rotation curve.
 330  RINC = DPM(10) * DX * DY / SQRT ((DX**2 + DY**2) / 2.0)
      CALL INTGR (RINC, RMEN, VMEN, VRMS, RMX, VMN, VMX, JMX)
      IF ((LENOUT.GT.0) .AND. (CPM(6).NE.0.0)) THEN
         DO 331 I = 1,JMX
            WRITE (MSGTXT,1330) RMEN(I), VMEN(I)/1.E3, VRMS(I)/1.E3
            TXLEN = JTRIM (MSGTXT)
            IF (TXLEN.GT.0) THEN
               CALL ZTXIO ('WRIT', TXLUN, TXFIND, MSGTXT(1:TXLEN), IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,3400) TXLUN, IERR
                  CALL MSGWRT (7)
                  END IF
               END IF
 331        CONTINUE
         CALL ZTXCLS (TXLUN, TXFIND, IERR)
         END IF
      STAT  = 'WRIT'
      ISEQ  = SEQIN   + 0.01
      IVOL  = DISKIN  + 0.01
      IPARMS = NPARMS
C                                       defaults fill back in
      DPM(1) = RMIN
      DPM(2) = RMAX
      DPM(3) = CMIN
      DPM(4) = CMAX
      DPM(5) = SMIN
      DPM(6) = SMAX
      IF (DPM(8).LE.0.0) DPM(8) = VMX / 1000.0
      IF (DPM(9).EQ.0.0) DPM(9) = VMN / 1000.0
      CALL PLINIT (IVOL, NAMEIN, CLASIN, ISEQ, USID, L16, IPARMS,
     *   PARMS, XDOTV, XGRCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2060) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
      RMN    = 0.0
C      VMN    = 0.0
      RMX    = RMX * 1.1
      VMX    = VMX * 1.2
      XY     = 1.0
      XUNIT = 'arcsec  '
      YUNIT = 'meter/s '
      IF (DPM(7).GT.0.0)   RMX = DPM(7)
      IF (DPM(8).GT.0.0)   VMX = DPM(8) * 1000.0
      IF (DPM(9).NE.0.0)   VMN = DPM(9) * 1000.0
      CALL PLAXES (RMN, RMX, VMN, VMX, XY, XUNIT, YUNIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2070) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
      CALL PLDATA (JMX, RMEN, VMEN, VRMS, XFACT, XSYM, IOUT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2080) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
      XB     = MAX (RMN,RMIN)
      XE     = MIN (RMX,RMAX)
      CALL PLFUNC (XB, XE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2090) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
      CALL PLFINI (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2100) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
      IRET = 0
      GO TO 990
C
 980  CALL MAPCLS (STAT, IVOL, ICN, L16, PNTR, CATBLK, F, SCR, IERR)
C
 990  IF (.NOT.RQUICK) CALL PTPARM (10, APM, SCR, IERR)
      CALL DIETSK (IRET, RQUICK, SCR)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('get parameter error ',I5)
 1005 FORMAT (A4,' fixed at ',F6.1)
 1010 FORMAT ('MAPOPN error: IERR = ',I2)
 1020 FORMAT ('WINDOW error: IERR = ',I2)
 1030 FORMAT ('total # points',I8,' exceeds',I8,'(',I4,'*',I4,')')
 1040 FORMAT ('MINI3 error: IERR = ',I2)
 1050 FORMAT ('MDISK error: IERR = ',I2)
 1060 FORMAT (' # pixels:',I8,', # non-blanked :',I8)
 1065 FORMAT (' # unblanked pixels should not exceed ',I6)
 1070 FORMAT (A2,' curve, Rmin=',I4,' Rmax=',I4,' arcsec')
 1075 FORMAT ('COSmin = ',F5.2,' COSmax = ',F5.2)
 1078 FORMAT ('SINmin = ',F5.2,' SINmax = ',F5.2)
 1080 FORMAT ('GAL : information parameter # ',I2)
 1090 FORMAT (' after ',I3,' iterations the solution vector is ')
 1100 FORMAT (7(1X,F7.2),1X,F5.2)
 1120 FORMAT ('R.A. : ',I2.2,1X,I2.2,1X,F5.2,1X,A1,6X,
     *         'Dec : ',A1,I2.2,1X,I2.2,1X,F5.2,1X,A1)
 1125 FORMAT (15X,F5.2,22X,F5.2)
 1130 FORMAT ('MCREAT error: IERR = ',I2)
 1330 FORMAT (F9.2,F10.3,F10.5)
 2000 FORMAT (A6,' residual field, fit of ',A2,' rotation curve')
 2005 FORMAT (A6,' an * denotes a fixed parameter')
 2008 FORMAT (A6,' residual field; all values kept fixed (no fit)')
 2010 FORMAT (A6,' values(1=>4): ',4(2X,F8.2,A1))
 2020 FORMAT (A6,' values(5=>8): ',4(2X,F8.2,A1))
 2025 FORMAT (A6,' R.A. : ',I2.2,1X,I2.2,1X,F5.2,1X,A1,6X,
     *         'Dec : ',A1,I2.2,1X,I2.2,1X,F5.2,1X,A1)
 2030 FORMAT ('error in creating of/adding to history file')
 2060 FORMAT ('PLINIT: IERR = ',I2)
 2070 FORMAT ('PLAXES: IERR = ',I2)
 2080 FORMAT ('PLDATA: IERR = ',I2)
 2090 FORMAT ('PLFUNC: IERR = ',I2)
 2100 FORMAT ('PLFINI: IERR = ',I2)
 3200 FORMAT (' *** using weights from second image ***')
 2200 FORMAT ('input and weight map are too dissimilar')
 3201 FORMAT (' *** no special weighting, all weights unity ***')
 3400 FORMAT ('OUTFILE write error on LUN = ',I5,', ERR = ',I5)
 3500 FORMAT ('Error ',I5,' opening text file')
 3600 FORMAT (4(1X,F6.1),2(1X,F6.1,1X,F5.1),1X,F6.1,1X,F6.3,2(1X,F5.1))
 3700 FORMAT (I2.2,1X,I2.2,1X,F5.2,6X,A1,I2.2,1X,I2.2,1X,F5.2)
      END
      SUBROUTINE FCN (M, N, X, FNC, DER, FLG)
C-----------------------------------------------------------------------
C   FCN is the subroutine required by subroutine LMSTR1; the latter per-
C   forms the least squares fit in task GAL.
C     M      INTEGER       input    # data points (adj. array dim.).
C     N      INTEGER       input    # parameters (adj. array dim.).
C     X      REAL*8 (N)    input    current values of N parameters.
C     FNC    REAL*8 (M)   output    M function values using current X.
C     DER    REAL*8 (N)   output    derivatives w.r.t. X(1)...X(N) in
C                                   point # FLG - 1.
C     FLG    INTEGER       input    1: FNC is returned, >1 : DER is
C                                   returned. 0, -1: FNC is returned
C                                   with all weights 1 (for residual
C                                   map only).
C-----------------------------------------------------------------------
C                                       Variables used as adjustable
C                                       array dimension specifiers
C                                       should be declared simply as
C                                       INTEGER
      INTEGER   M,N,J,JM,JTOT,FLG,JBEG,JEND
      INTEGER   I,JCOL,JROW,K
      REAL     RX,RY,R2,R,RREL,CSP,SNP,CSH,SNI,CSI,CST,RR,
     *   RRX8,F,DRDX,DRDY,DCPDX,DCPDY,DENOM,DCTDCP,DRRDX,
     *   DRRDY,DVRDRR,DVODX,DVODY,DCPDP,DRRDCP,DVODP,DVRDN,DCTDI,
     *   DRRDI,DVRDI,DVODI,CSHSNI,CSTSNI,CSHCST,LN100,TEK,
     *   VR,VMOD,CWT,SNH,SN2H,VRSNI,DEN,DIFF
      DOUBLE PRECISION  X(N),FNC(M),DER(N)
      DOUBLE PRECISION  A2R
C
      INCLUDE 'GAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C
      DATA A2R  /1.7453292519943296D-02/
      DATA LN100 /4.60517/
C-----------------------------------------------------------------------
      IF (FLG.GT.1) GO TO 30
C                                         update XPAR.  XPAR is used for
C                                         calculations.  X is used to
C                                         communicate with LMSTR, and
C                                         for intermediate output.
         I  = 0
         J  = 0
 10      I  = I + 1
 20      J  = J + 1
         IF (FIX(J).EQ.1)  GO TO 20
         IF (J.EQ.7) X(I) = ABS (X(I))
         XPAR(J) = X(I)
         IF (I.LT.N) GO TO 10
C
         JBL  = 0
         JTOT = 0
         JBEG = 1
         JEND = M
         SWT  = 0.0
         SWT2 = 0.0
         SWV  = 0.0
         SWV2 = 0.0
      GO TO 40
 30      JBEG = FLG - 1
         JEND = JBEG
C                                         For FLG=1 this is done for
C                                       all M,
C                                         else for FLG-1 only.
 40   DO 220 J = JBEG, JEND
 50      JM = J + JBL
         IF (VOBS(JM).NE.FBLANK) GO TO 60
            JBL    = JBL + 1
            GO TO 50
 60      JROW   = (JM - 1) / NXW
         JCOL   = JM - JROW * NXW + WIN(1) - 1
         JROW   = JROW + WIN(2)
         RX     = (JCOL - XPAR(1)) * DX
         RY     = (JROW - XPAR(2)) * DY
         R2     = RY ** 2 + RX ** 2
         R      = SQRT (R2)
         IF (R .LT. 0.01) GO TO 70
            CSP    = COS (XPAR(3) * A2R)
            SNP    = SIN (XPAR(3) * A2R)
            CSH    = (CSP * RY / R - SNP * RX / R)
            SNH    = -(CSP * RX / R + SNP * RY / R)
C                         NB: for FLG=-1 (residual map) whole map is
C                         used irrespective of supplied limits
            IF ((CSH.LT.CMIN.OR.CSH.GT.CMAX.OR.SNH.LT.SMIN.OR.
     *          SNH.GT.SMAX).AND.(FLG.GE.0)) GO TO 70
               SNI    = SIN (XPAR(4) * A2R)
               CSI    = COS (XPAR(4) * A2R)
               TEK  = SIGN (1.0, SNI/CSI)
               SNI  = ABS (SNI)
               CSI  = ABS (CSI)
               CSHSNI= CSH * SNI
               CSHCST= 1.0 / CSI * SQRT (1.0 - CSHSNI * CSHSNI)
               CST    = CSH / CSHCST
               RR     = R * CSHCST
            GO TO 80
 70            RR = -1.0
 80         IF ((RR.GE.RMIN.AND.RR.LE.RMAX).OR.(FLG.EQ.-1))  GO TO 100
               FNC(J) = 0.0
               IF (FLG.EQ.0) FNC(J) = FBLANK
               DO 90 I = 1, N
                  DER(I) = 0.0D0
 90               CONTINUE
               GO TO 220
 100        JTOT   = JTOT + 1
            IF (TYPE.NE.'CC') GO TO 110
               VR  = XPAR(6)
               DVRDRR = 0.0
               GO TO 140
 110        IF (TYPE.NE.'SB') GO TO 120
               VR  = XPAR(6) * RR / 60.0
               DVRDRR = XPAR(6) / 60.0
               GO TO 140
 120        RREL   = RR / (XPAR(7))
            IF (TYPE.NE.'EX') GO TO 130
               VR  = XPAR(6) * (1.0 - EXP (- LN100 * RREL))
               DVRDRR = (XPAR(6) - VR) / XPAR(7) * LN100
               GO TO 140
 130        RREL = ABS (RREL)
            RRX8   = RREL ** XPAR(8)
            F      = 1.0 / 3.0 + 2.0 / 3.0 * RRX8
            IF (XPAR(8).NE.0.0) THEN
               VR      = XPAR(6) * RREL * F ** (-1.5 / XPAR(8))
            ELSE
               VR = 0.0
                  END IF
            DVRDRR = VR / XPAR(7) / RREL * (1.- RRX8) / (1.+ 2.* RRX8)
 140        VMOD     = XPAR(5) + VR * SNI * CST
            IF (FLG.LE.0) THEN
               FNC(J) = (VMOD - VOBS(JM))
            ELSE IF (FLG.EQ.1) THEN
               DIFF   = VOBS(JM) - VMOD
               FNC(J) = - DIFF * WT(JM)
               SWT    = SWT  + WT(JM)
               SWT2   = SWT2 + WT(JM) * WT(JM)
               SWV    = SWV  + WT(JM) * DIFF
               SWV2   = SWV2 + WT(JM) * DIFF * DIFF
            ELSE
C                                         partial derivatives # 1, 2
               VRSNI  = VR * SNI
               DRDX   = - DX * RX / R
               DRDY   = - DY * RY / R
               DCPDX  = RX / R2 * DX * CSH + DX * SNP / R
               DCPDY  = RY / R2 * DY * CSH - DY * CSP / R
               CSTSNI = CST * SNI
               DEN    = 1.0 - CSHSNI * CSHSNI
               DENOM  = DEN ** (-1.5)
               DCTDCP = CSI * DENOM
               DRRDCP = - RR * CSH * SNI * SNI / DEN
               DRRDX  = - RR * DX * RX / R2 + DRRDCP * DCPDX
               DRRDY  = - RR * DY * RY / R2 + DRRDCP * DCPDY
               DVODX  = DVRDRR*DRRDX*CSTSNI + VRSNI*DCTDCP*DCPDX
               DVODY  = DVRDRR*DRRDY*CSTSNI + VRSNI*DCTDCP*DCPDY
               K      = 0
C                                         K is derivative #
               IF (FIX(1).EQ.1) GO TO 150
                  K      = K + 1
                  DER(K) = DVODX * WT(JM)
 150           IF (FIX(2).EQ.1) GO TO 160
                  K      = K + 1
                  DER(K) = DVODY * WT(JM)
C                                         partial derivative # 3
 160           IF (FIX(3).EQ.1) GO TO 170
                  K      = K + 1
                  DCPDP  = - SNP * RY / R - CSP * RX / R
                  DVODP  = DVRDRR * DRRDCP * DCPDP * CSTSNI
     *                     + VRSNI * DCTDCP * DCPDP
                  DER(K) = DVODP * A2R * WT(JM)
C                                         partial derivatives # 4,5
 170           IF (FIX(4).EQ.1) GO TO 180
                  K      = K + 1
                  SN2H   = SNH * SNH
                  DCTDI  = - CSHSNI * SN2H * DENOM
                  DRRDI  = RR * SNI / CSI * SN2H / DEN
                  DVRDI  = DVRDRR * DRRDI
                  DVODI  = DVRDI*CSTSNI + VRSNI*DCTDI + VR* CST*CSI
                  DER(K) = DVODI * A2R * TEK * WT(JM)
 180           IF (FIX(5).EQ.1) GO TO 190
                  K      = K + 1
                  DER(K) = 1.0 * WT(JM)
 190           CWT    = CSTSNI * WT(JM)
C                                         partial derivatives # 6-8
               IF (FIX(6).EQ.1) GO TO 200
                  K      = K + 1
                  DER(K) = CWT * VR / XPAR(6)
 200           IF (TYPE.EQ.'CC'.OR.TYPE.EQ.'SB') GO TO 999
               IF (FIX(7).EQ.1) GO TO 210
                  K      = K + 1
                  DER(K) = - CWT * DVRDRR * RREL
 210           IF (TYPE.EQ.'EX') GO TO 999
               IF (FIX(8).EQ.1) GO TO 999
                  K      =   K + 1
                  DVRDN  =   VR / XPAR(8) * (LOG(F) / XPAR(8) * 1.5 -
     *                       RRX8 * LOG(RREL) / F)
                  DER(K) =   CWT * DVRDN
C
            END IF
 220     CONTINUE
C
      IF (FLG.EQ.1) THEN
         IT = IT + 1
         AVRES = SWV / SWT
         STDEV = SQRT (SWV2/SWT - AVRES**2)
         IF (IT.NE.1) GO TO 230
            WRITE (MSGTXT,1000) (HED(I), I= 1, N)
            CALL MSGWRT (5)
 230      WRITE (MSGTXT,1010) IT,JTOT,AVRES,STDEV,(X(I), I = 1, N)
         CALL MSGWRT (5)
         JBL = 0
      END IF
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('it points resid  rms ',7(2X,A4,1X),A4)
 1010 FORMAT (I2,1X,I6,1X,F5.1,1X,F5.1,7(1X,F6.1),1X,F4.1)
      END
      SUBROUTINE INTGR (RINC, RMEN, VMEN, VRMS, RMX, VMN, VMX, JMX)
C-----------------------------------------------------------------------
      INCLUDE 'GAL.INC'
      INTEGER   K,NV(MRING),JCOL,JROW,JRING,JM,JMX,MTOT
      REAL      SR(MRING),SW(MRING),SV(MRING),SV2(MRING),RMEN(MRING),
     *          VMEN(MRING),VRMS(MRING)
      REAL      RX,RY,R,R2,CSP,SNP,RINC,TEK,VMN,
     *          CSH,SNH,CSHCST,CST,ACST,RR,VCKL,VMX,RMX,CSI,SNI
      DOUBLE PRECISION A2R
      INCLUDE 'INCS:DDCH.INC'
      DATA A2R  /1.7453292519943296D-02/
C-----------------------------------------------------------------------
      JMX = 0
      DO 350 K = 1, MRING
         NV(K)  = 0
         SR(K)  = 0.0
         SW(K)  = 0.0
         SV(K)  = 0.0
         SV2(K) = 0.0
 350     CONTINUE
C                                      loop through field
      MTOT = NXW * NYW
      DO 500 JM = 1, MTOT
         IF (VOBS(JM).EQ.FBLANK) GO TO 500
         JROW  = (JM - 1) / NXW
         JCOL = JM - JROW * NXW + WIN(1) - 1
         JROW = JROW + WIN(2)
         RX   = (JCOL - XPAR(1)) * DX
         RY   = (JROW - XPAR(2)) * DY
         R2   = RY ** 2 + RX ** 2
         R    = SQRT (R2)
         IF (R.LT.0.01) GO TO 500
         CSP  = COS (XPAR(3) * A2R)
         SNP  = SIN (XPAR(3) * A2R)
         CSH  =  (CSP * RY - SNP * RX) / R
         SNH  = -(CSP * RX + SNP * RY) / R
         IF (CSH.LT.CMIN.OR.CSH.GT.CMAX.OR.
     *       SNH.LT.SMIN.OR.SNH.GT.SMAX) GO TO 500
         SNI  = SIN (XPAR(4) * A2R)
         CSI  = COS (XPAR(4) * A2R)
         TEK  = SIGN (1.0, SNI/CSI)
         SNI  = ABS (SNI)
         CSI  = ABS (CSI)
         CSHCST = 1.0 / CSI * SQRT (1.0 - CSH**2 * SNI**2)
         CST    = CSH / CSHCST
         RR     = R * CSHCST
         ACST   = ABS (CST)
         IF (ACST.LT.0.10)  GO TO 500
         IF (RR.LT.RMIN.OR.RR.GT.RMAX)  GO TO 500
         VCKL   = (VOBS(JM) - XPAR(5)) / SNI / CST
         IF (VCKL.GT.0.0) THEN
            VMX = VCKL
            END IF
         IF (ABS(VCKL).GT.100.0) THEN
            VMX = ABS (VCKL)
            END IF
C                                      find ring #, fill bins
         JRING  = RR / RINC + 1
C                                      array bound check
         IF (JRING.GT.MRING) GO TO 500
         IF (JMX.LT.JRING)  JMX = JRING
         WT(JM)     = WT(JM) * ACST
         NV(JRING)  = NV(JRING)  + 1
         SR(JRING)  = SR(JRING)  + RR
         SW(JRING)  = SW(JRING)  + WT(JM)
         SV(JRING)  = SV(JRING)  + WT(JM) * VCKL
         SV2(JRING) = SV2(JRING) + WT(JM) * VCKL**2
 500     CONTINUE
C                                      results
      VMX = 0.0
      VMN = 0.0
      RMX = 0.0
      DO 550 K = 1, JMX
         IF (NV(K).GE.2) GO TO 530
            RMEN(K) = FBLANK
            VMEN(K) = FBLANK
            VRMS(K) = FBLANK
         GO TO 550
 530        RMEN(K) = SR(K) / NV(K)
            VMEN(K) = SV(K) / SW(K)
C                                      rms mean, see Bevington p. 73
            VRMS(K) = SQRT ((SV2(K)/SW(K)-VMEN(K)**2) / (NV(K)-1))
            VMEN(K) = VMEN(K) * 1.0E3
            VRMS(K) = VRMS(K) * 1.0E3
C                                      update max and min
            IF (RMX.LT.RMEN(K))   RMX = RMEN(K)
            IF (VMX.LT.VMEN(K))   VMX = VMEN(K)
            VMN = MIN (VMN, VMEN(K))
 550     CONTINUE
C
      RETURN
      END
      SUBROUTINE PLINIT (IVOL, NAM, CLAS, ISEQ, USID, ILUN, NP, RPARM,
     *   XDOTV, XGRCH, IERR)
C-----------------------------------------------------------------------
C  Opens map, creates plot extension file
C     IVOL   INTEGER        I  disk volume #
C     NAM    REAL   (3)     I  image name
C     CLAS   REAL   (2)     I  image class
C     ISEQ   INTEGER        I  image sequence #
C     USID   INTEGER        I  user #
C     ILUN   INTEGER        I  map logical unit #
C     NP     INTEGER        I  # parameters
C     RPARM  REAL   (NP)    I  parameter list
C     IERR   INTEGER        O  output code. two digit, first digit indi-
C                              cates subroutine: 1: MAPOPN, 2: MADDEX,
C                              3: ZPHFIL, 4: GINIT, second digit indi-
C                              cates error code of that subroutine.
C    XDOTV   REAL
C    XGRCH   REAL
C-----------------------------------------------------------------------
      CHARACTER PHNAME*48, NAM*12, CLAS*6, MTYPE*2, STAT*4
      INTEGER   NP, IVOL, ISEQ, USID, ILUN, IERR, BLOK(256),
     *   GLUN, GFIND, JERR, TVCH, GRCH
      REAL      RPARM(NP), CRN(4), XDOTV, XGRCH
      LOGICAL   T, F
      INCLUDE 'GAL2.INC'
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                        open map
      IERR  = 0
      AXES  = .FALSE.
      JVOL  = IVOL
      LUN   = ILUN
      MTYPE = 'MA'
      TV    = XDOTV.GT.0.0
      STAT = 'WRIT'
      IF (TV) STAT = 'READ'
      CALL MAPOPN (STAT, IVOL, NAM, CLAS, ISEQ, MTYPE, USID, LUN,
     *    FIND, ICN, CATGAL, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         IERR = IERR + 10
         GO TO 990
         ENDIF
C                                        add plot file to header
      IF (.NOT.TV) THEN
         CALL MADDEX ('PL', IVOL, ICN, CATGAL, BLOK, T, 'READ',
     *      IVER, IERR)
         IF (IERR.NE.0) THEN
            IERR = IERR + 20
            GO TO 980
            ENDIF
         ENDIF
C                                        make physical filename
      CALL ZPHFIL ('PL', IVOL, ICN, IVER, PHNAME, IERR)
      IF (IERR.NE.0) THEN
         IERR = IERR + 30
         GO TO 980
         ENDIF
C                                        open plot file
      TVCH   = 1
      GRCH   = XGRCH + 0.01
      CALL FILL (4, 0, CRN)
      CALL GINIT (IVOL, ICN, PHNAME, 0, 43, NP, RPARM, TV, TVCH,
     *    GRCH, CRN, CATGAL, IBLK, GLUN, GFIND, IERR)
      IF (IERR.EQ.0) GO TO 999
C
      IERR = IERR + 40
C
 980  IF (.NOT.TV) CALL DELEXT ('PL', JVOL, ICN, 'READ', CATGAL, SCRTCH,
     *   IVER, JERR)
 990  CALL MAPCLS ('READ', JVOL, ICN, LUN, FIND, CATGAL, F, SCRTCH,
     *   JERR)
C
 999  RETURN
      END
      SUBROUTINE PLFINI (IERR)
C-----------------------------------------------------------------------
C     IERR   INTEGER        O  output code. two digit, first digit indi-
C                              cates subroutine: 1: GFINIS, 2: MAPCLS,
C                              second digit indicates error code of
C                              that subroutine.
C-----------------------------------------------------------------------
      INTEGER   IERR, JERR
      LOGICAL   F
      INCLUDE 'GAL2.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                        close plot file
      CALL GFINIS (IBLK, IERR)
      IF (IERR.NE.0) IERR = IERR + 10
      IF (IERR.NE.0) GO TO 980
         CALL HIPLOT (JVOL, ICN, IVER, SCRTCH, IERR)
         IERR = 0
         GO TO 990
C                                        close map file
 980  IF (.NOT.TV) CALL DELEXT ('PL', JVOL, ICN, 'READ', CATGAL, SCRTCH,
     *   IVER, JERR)
 990  CALL MAPCLS ('READ', JVOL, ICN, LUN, FIND, CATGAL, F, SCRTCH,
     *   JERR)
      IF (JERR.NE.0)   IERR = IERR + 20
C
      RETURN
      END
      SUBROUTINE PLAXES (XMN, XMX, YMN, YMX, XY, XUNIT, YUNIT, IERR)
C-----------------------------------------------------------------------
C  Plots axes and ticks, MUST  be called prior to PLDATA or PLFUNC
C     parameters:
C     XMN    REAL           I  value of x-variable at llhc
C     XMX    REAL           I  value of x-variable at urhc
C     YMN    REAL           I  value of y-variable at llhc
C     YMX    REAL           I  value of y-variable at urhc
C     XY     REAL           I  ratio x-axis / y-axis
C     XUNIT  C*8            I  units along x-axis
C     YUNIT  C*8            I  units along y-axis.
C     IERR   INTEGER        O  error code, IERR=-1 indicates illegal
C                              inputs, IERR<10 indicates errors in
C                              drawing, IERR>10 indicates error in
C                              GINITL, with IERR-10 the error number.
C-----------------------------------------------------------------------
      CHARACTER SPRTXT*132,  ATIME*8, ADATE*12, XUNIT*8, YUNIT*8,
     *   CHTMP*20
      INTEGER   IERR, DEPT(5), I, LABEL, JERR, IANGL, INCHAR, IT(3),
     *   ID(3), JNCHAR, INP, LTYPE
      HOLLERITH CATGH(256)
      REAL      XRANGE, YRANGE, XR, YR, XY, XMX, YMX, DCX, DCY, XMN,
     *   YMN, CATGR(256), CHOUT(4)
      LOGICAL   F, PFLG
      INCLUDE 'GAL2.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (CATGAL, CATGR, CATGH)
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
      DO 5 I = 1, 5
         DEPT(I) = 1
 5      CONTINUE
      LOCNUM = 1
      CALL SETLOC (DEPT, F)
      AXFUNC(1,LOCNUM) = 0
      AXFUNC(2,LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
C                                        proper scaling labels
      XRANGE = XMX - XMN
      YRANGE = YMX - YMN
      IF (XRANGE.GT.0.0.AND.YRANGE.GT.0.0) GO TO 15
         WRITE (MSGTXT,1000) XRANGE, YRANGE
         CALL MSGWRT (8)
         IERR = -1
         GO TO 990
 15   XR     = XRANGE
      LTYPE = 103
      CALL METSCL (LTYPE, XR, CPREF(1,LOCNUM), PFLG)
      YR     = YRANGE
      CALL METSCL (LTYPE, YR, CPREF(2,LOCNUM), PFLG)
C                                        proceed filling /LOCATI/
      BLC(1)   = 1.0
      BLC(2)   = 1.0
      TRC(1)   = 1024.0
      TRC(2)   = 1024.0
      SCX    = (TRC(1) - BLC(1)) / XRANGE
      SCY    = (TRC(2) - BLC(2)) / YRANGE
      OFX    = BLC(1) - XMN * SCX
      OFY    = BLC(2) - YMN * SCY
      RPLOC(1,LOCNUM) = 1.0
      RPLOC(2,LOCNUM) = 1.0
      RPVAL(1,LOCNUM) = XMN * XR / XRANGE
      RPVAL(2,LOCNUM) = YMN * YR / YRANGE
      AXINC(1,LOCNUM) = XR / XRANGE / SCX
      AXINC(2,LOCNUM) = YR / YRANGE / SCY
      CTYP(1,LOCNUM)= XUNIT
      CTYP(2,LOCNUM)= YUNIT
C                                        space around axes
      CALL CHNTIC (BLC, TRC, INP)
      CHOUT(1) = 4.0 + INP
      CHOUT(2) = 3.333
      CHOUT(3) = 0.5
      CHOUT(4) = 3.333
C                                        initialize line drawing
      CALL GINITL (BLC, TRC, XY, CHOUT, DEPT, IBLK, IERR)
      IF (IERR.EQ.0) GO TO 10
         IERR = IERR + 10
         GO TO 990
 10   LABEL = 3
C                                        proper labeling
      CALL GLTYPE (1, IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL CLAB1 (BLC, TRC, CHOUT, LABEL, XY, F, IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        draw rectangle
      CALL GPOS (BLC(1), BLC(2), IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL GVEC (TRC(1), BLC(2), IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL GVEC (TRC(1), TRC(2), IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL GVEC (BLC(1), TRC(2), IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL GVEC (BLC(1), BLC(2), IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      AXES = .TRUE.
C                                       Source name
      CALL GPOS (BLC(1), TRC(2), IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      DCX = 0.0
      DCY =  .5
      IANGL = 0
      SPRTXT = '          '
      CALL H2CHR (8, 1, CATGH(KHOBJ), SPRTXT(1:8))
      INCHAR = 11
C                                       image name
      CALL H2CHR (12, KHIMNO, CATGH(KHIMN), CHTMP)
      CALL H2CHR (6, KHIMCO, CATGH(KHIMC), CHTMP(13:18))
      CALL NAMEST (CHTMP(1:18), CATGAL(KIIMS), SPRTXT(INCHAR:), JNCHAR)
      INCHAR = INCHAR + JNCHAR
      CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Date/time version
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (SPRTXT,1020) IVER, ADATE, ATIME
      INCHAR = 51
      DCY = DCY + 1.333
      CALL GPOS (BLC(1), TRC(2), IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      MSGTXT = SPRTXT
      CALL MSGWRT (3)
      GO TO 999
C                                        close map in case of error
 990  IF (.NOT.TV) CALL DELEXT ('PL', JVOL, ICN, 'READ', CATGAL, SCRTCH,
     *   IVER, JERR)
      CALL MAPCLS ('READ', JVOL, ICN, LUN, FIND, CATGAL, F, SCRTCH,
     *   JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('X, Y RANGES',2(1PE13.4),' IMPROPER')
 1020 FORMAT ('Plot file version',I4,'  created ',A,A)
      END
      SUBROUTINE PLDATA (N, X, Y, S, FACT, sym, IOUT, IERR)
C-----------------------------------------------------------------------
C     plots array of N points.
C     N    INTEGER        I  total number of points (adj. array dim.)
C     X    REAL(N)        I  x-values of the N points
C     Y    REAL(N)        I  y-values of the N points
C     S    REAL(N)        I  y-rms    of the N points
C     FACT REAL           I  scale plot symbol
C     IOUT INTEGER        O  number of points outside plot
C     IERR INTEGER        O  error code. IERR=-1 indicates that
C                            there has not been a legal call to
C                            PLAXES, IERR>0 indicates errors in
C                            drawing routines.
C-----------------------------------------------------------------------
      REAL      FACT, SYM
      INTEGER   N, IERR, IOUT, JERR, I, ISYM
      REAL      X(N), Y(N), S(N), PX, PY, AX(5), AY(5), DX, DY
      LOGICAL   XOUT, YOUT, F
      INCLUDE 'GAL2.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
      IOUT = 0
      IF (.NOT.AXES) THEN
         IERR = -1
         GO TO 990
         END IF
      ISYM = SYM + 0.01
      IF ((ISYM.LT.1) .OR. (ISYM.GT.24)) ISYM = 3
      IF (ISYM.EQ.23) ISYM = 3
      CALL GLTYPE (4, IBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      IF ((FACT.LT.0.03) .OR. (FACT.GT.33.)) FACT = 1.0
      DX = (TRC(1) - BLC(1)) / 250.0 * FACT
      DY = (TRC(2) - BLC(2)) / 250.0 * FACT
      DO 10 I = 1, N
         IF (Y(I).NE.FBLANK) THEN
            PX  = X(I) * SCX + OFX
            PY  = Y(I) * SCY + OFY
            XOUT = (PX.LT.BLC(1).OR.PX.GT.TRC(1))
            YOUT = (PY.LT.BLC(2).OR.PY.GT.TRC(2))
            IF (XOUT.OR.YOUT) THEN
               IOUT = IOUT + 1
            ELSE
               CALL GPOS  (PX, PY, IBLK, IERR)
               AX(1) = PX
               AY(1) = PY
               AX(2) = PX
               AX(3) = PX
               AX(4) = PX - DX
               AX(5) = PX + DX
               AY(2) = PY + DY
               AY(3) = PY - DY
               AY(4) = PY
               AY(5) = PY
               CALL PNTPLT (ISYM, AX, AY, BLC, TRC, .FALSE., .FALSE.,
     *            IBLK, IERR)
               IF (IERR.NE.0) GO TO 999
               PY = MIN (TRC(2), (Y(I)+S(I))*SCY+OFY)
               CALL GPOS (PX, PY, IBLK, IERR)
               IF (IERR.NE.0) GO TO 999
               PY = MAX (BLC(2), (Y(I)-S(I))*SCY+OFY)
               CALL GVEC (PX, PY, IBLK, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            END IF
 10      CONTINUE
      GO TO 999
C
 990  IF (.NOT.TV) CALL DELEXT ('PL', JVOL, ICN, 'READ', CATGAL, SCRTCH,
     *   IVER, JERR)
      CALL MAPCLS ('READ', JVOL, ICN, LUN, FIND, CATGAL, F, SCRTCH,
     *   JERR)
C
 999  RETURN
      END
      SUBROUTINE PLFUNC (XB, XE, IERR)
C-----------------------------------------------------------------------
C  Plots function
C     XB    REAL             I   first x-value of function
C     XE    REAL             I   last  x-value of function
C     IERR INTEGER           O      error code. IERR=-1 indicates that
C                                there has not been a legal call to
C                                PLAXES, IERR>0 indicates errors in
C                                drawing routines.
C-----------------------------------------------------------------------
      INTEGER   IERR, IXB, IXE, IX, JERR
      REAL      XB, XE, PXE, PX, PY, VCIR
      LOGICAL   OUT, UIT, F
      INCLUDE 'GAL2.INC'
      INCLUDE 'INCS:DGPH.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C
      IERR = 0
      IF (AXES) GO TO 5
         IERR = -1
         GO TO 990
 5    PX  = XB * SCX + OFX
      PXE = XE * SCX + OFX
      IXB = PX
      IXE = PXE
      PY  = (SCY * VCIR (XB) + OFY)
      OUT = (PX.LT.BLC(1).OR.PX.GT.TRC(1)
     *       .OR.PY.LT.BLC(2).OR.PY.GT.TRC(2))
      IF (.NOT.OUT)  CALL GPOS (PX, PY, IBLK, IERR)
C                                 draw vector only if both inside plot
      CALL GLTYPE (2, IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      DO 10 IX = IXB, IXE
         PX = IX
         PY = (SCY * VCIR ((IX - OFX) / SCX) + OFY)
         UIT = OUT
         OUT = (PX.LT.BLC(1).OR.PX.GT.TRC(1)
     *          .OR.PY.LT.BLC(2).OR.PY.GT.TRC(2))
         IF (.NOT.OUT.AND..NOT.UIT) THEN
            CALL GVEC (PX, PY, IBLK, IERR)
         ELSE IF (.NOT.OUT) THEN
            CALL GPOS (PX, PY, IBLK, IERR)
            END IF
         IF (IERR.NE.0) GO TO 990
 10      CONTINUE
      GO TO 999
C
 990  IF (.NOT.TV) CALL DELEXT ('PL', JVOL, ICN, 'READ', CATGAL, SCRTCH,
     *   IVER, JERR)
      CALL MAPCLS ('READ', JVOL, ICN, LUN, FIND, CATGAL, F, SCRTCH,
     *   JERR)
C
 999  RETURN
      END
      REAL FUNCTION VCIR (R)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INCLUDE 'GAL.INC'
      INCLUDE 'INCS:DCAT.INC'
      REAL   R, DENOM
C-----------------------------------------------------------------------
C
      IF (TYPE.NE.'CC') GO TO 10
         VCIR = XPAR(6)
         GO TO 50
 10   IF (TYPE.NE.'SB') GO TO 20
         VCIR = XPAR(6) * R / 60.0
         GO TO 50
 20   R = R / XPAR(7)
      IF (TYPE.NE.'EX') GO TO 30
         R     = - LOG (100.0) * R
         VCIR  = XPAR(6) * (1.0 - EXP (R))
         GO TO 50
 30   IF (R.GT.0.0) THEN
         DENOM = 1.0/3.0 + 2.0/3.0 * R ** XPAR(8)
         DENOM = DENOM ** (3.0/2.0 / XPAR(8))
         VCIR  = XPAR(6) * R / DENOM
      ELSE
         VCIR = 0.0
         END IF
 50   VCIR = VCIR * 1000.0
C
 999  RETURN
      END
      SUBROUTINE HCN (M, FNC, FLG)
C-----------------------------------------------------------------------
C HCN is equivalent to FCN. It, however, does not use an array X which
C contains the parameters specifying the velocity field, but instead
C uses a user supplied text file to be specified in INFILE. This is a
C table containing the values of x, y, pa, i, Vsys, and Vmax for concen-
C tric rings. INFILE is opened, read, and closed completely within this
C subroutine. A sample INFILE:
C     M      INTEGER       input    # data points (adj. array dim.).
C     FNC    REAL*8 (M)   output    M function values using current X.
C     FLG    INTEGER*2     input    1: FNC is returned, >1 : DER is
C                                   returned. 0, -1: FNC is returned
C                                   with all weights 1 (for residual
C                                   map only).
C-----------------------------------------------------------------------
      INTEGER   M, FLG
      DOUBLE PRECISION  FNC(M)
c
      INCLUDE 'GAL.INC'
      INTEGER   J, JM, LUN, FIND, IERR, I, JCOL, JROW, NRING, IRING,
     *   INT, KBP, ZERO, JT, JTRIM
      REAL     RX, RY, R2, R, CSP, SNP, CSH, SNI, CSI, CST, RR, CSHSNI,
     *   CSHCST, TEK, VMOD, SNH, INTX, INTY, INTP, INTI, INTS, INTV,
     *   RNN, RXX, DENOM, LN100
      REAL    RING(2,MRING), XC(MRING), YC(MRING), PA(MRING), IN(MRING),
     *   VS(MRING), VM(MRING), RMX(MRING), BRN(MRING)
      DOUBLE PRECISION  XIN, A2R
      LOGICAL   F
      CHARACTER LINE*100
      PARAMETER (F = .FALSE.)
      PARAMETER (LUN = 10)
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
C
      DATA A2R  /1.7453292519943296D-02/
      DATA LN100 /4.60517/
C-----------------------------------------------------------------------
      ZERO = 0
      RXX = 0.0
      RNN = 1.E10
C                                   Read text file with curve info
C                                   Open file
      CALL ZTXOPN ('READ', LUN, FIND, INFILE, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'Opening'
         GO TO 990
         END IF
      I = 0
C
 10      CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
C                                   EOF
         IF (IERR.EQ.2) GO TO 20
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'Reading'
            GO TO 990
            END IF
         JT = JTRIM (LINE)
         I = I + 1
C                                   Process LINE
C                                   Free format reads illegal
C        READ (LINE,*) RING(1,I), RING(2,I), XC(I), YC(I), PA(I), IN(I),
C           VS(I), VM(I), RMX(I)
         KBP = 1
         ERRNUM = 0
         CALL GETNUM (LINE, 80, KBP, XIN)
         IF (ERRNUM.NE.0) GO TO 980
         RING(1,I) = XIN
         CALL GETNUM (LINE, 80, KBP, XIN)
         IF (ERRNUM.NE.0) GO TO 980
         RING(2,I) = XIN
         RNN = MIN (RNN, RING(1,I))
         RNN = MIN (RNN, RING(2,I))
         RXX = MAX (RXX, RING(1,I))
         RXX = MAX (RXX, RING(2,I))
         CALL GETNUM (LINE, 80, KBP, XIN)
         IF (ERRNUM.NE.0) GO TO 980
         XC(I) = XIN
         CALL GETNUM (LINE, 80, KBP, XIN)
         IF (ERRNUM.NE.0) GO TO 980
         YC(I) = XIN
         CALL GETNUM (LINE, 80, KBP, XIN)
         IF (ERRNUM.NE.0) GO TO 980
         PA(I) = XIN
         CALL GETNUM (LINE, 80, KBP, XIN)
         IF (ERRNUM.NE.0) GO TO 980
         IN(I) = XIN
         CALL GETNUM (LINE, 80, KBP, XIN)
         IF (ERRNUM.NE.0) GO TO 980
         VS(I) = XIN
         CALL GETNUM (LINE, 80, KBP, XIN)
         IF (ERRNUM.NE.0) GO TO 980
         VM(I) = XIN
         CALL GETNUM (LINE, 80, KBP, XIN)
         IF (ERRNUM.NE.0) GO TO 980
         RMX(I) = XIN
         IF (RMX(I).LE.0.0) ZERO = ZERO + 1
         IF (TYPE.EQ.'BR') THEN
            CALL GETNUM (LINE, 80, KBP, XIN)
            IF (ERRNUM.NE.0) GO TO 980
            BRN(I) = XIN
            END IF
C                                   Loop for next line
         GO TO 10
C                                   make sure array bounds correct
 20   NRING = MIN (I, MRING)
      IF (RMIN.GE.RMAX) THEN
         MSGTXT = 'AREA TAKEN FROM INPUT TEXT FILE'
         CALL MSGWRT (6)
         RMIN = RNN
         RMAX = RXX
         END IF
      IF ((ZERO.GT.0) .AND. ((TYPE.EQ.'BR') .OR. (TYPE.EQ.'EX'))) THEN
         RXX = (RNN + RXX) / 3.0
         WRITE (MSGTXT,1020) ZERO, RXX
         CALL MSGWRT (6)
         DO 30 I = 1,NRING
            IF (RMX(I).LE.0.0) RMX(I) = RXX
 30         CONTINUE
         END IF
C
      JBL  = 0
      DO 220 J = 1, M
 50      JM = J + JBL
         IF (VOBS(JM).EQ.FBLANK) THEN
            JBL    = JBL + 1
            GO TO 50
            END IF
         JROW   = (JM - 1) / NXW
         JCOL   = JM - JROW * NXW + WIN(1) - 1
         JROW   = JROW + WIN(2)
C
C Now find proper ring for this point. First try ring 1, and calculate
C RR. If RR exceeds the radius of ring 1, try ring 2, and repeat this
C until the right ring is found.
C
         I = 0
 65      I = I + 1
C                                   Failed to find ring: blank pixel
         IF (I.GT.NRING) THEN
            FNC(J) = FBLANK
            GO TO 220
            END IF
         RX = (JCOL - XC(I)) * DX
         RY = (JROW - YC(I)) * DY
         R2 = RY ** 2 + RX ** 2
         R  = SQRT (R2)
         RR = -1.0
         IF (R.GE.0.01) THEN
            CSP    = COS (PA(I) * A2R)
            SNP    = SIN (PA(I) * A2R)
            CSH    = (CSP * RY / R - SNP * RX / R)
            SNH    = -(CSP * RX / R + SNP * RY / R)
C                                   NB: for FLG=-1 (residual map)
C                                   whole map is used irrespective of
C                                   supplied limits
            IF (((CSH.GE.CMIN) .AND. (CSH.LE.CMAX) .AND. (SNH.GE.SMIN)
     *         .AND. (SNH.LE.SMAX)) .OR. (FLG.LT.0)) THEN
               SNI    = SIN (IN(I) * A2R)
               CSI    = COS (IN(I) * A2R)
               TEK  = SIGN (1.0, SNI/CSI)
               SNI  = ABS (SNI)
               CSI  = ABS (CSI)
               CSHSNI= CSH * SNI
               CST = 0.0
               CSHCST = 0.0
               IF (CSI.NE.0.0) THEN
                  CSHCST= 1.0 / CSI * SQRT (1.0 - CSHSNI * CSHSNI)
                  IF (CSHCST.NE.0.0) CST    = CSH / CSHCST
                  END IF
               RR     = R * CSHCST
               END IF
            END IF
         IF (((RR.LT.RMIN) .OR. (RR.GT.RMAX)) .AND. (FLG.NE.-1)) THEN
            FNC(J) = FBLANK
            GO TO 220
            END IF
         IF ((RR.LT.RING(1,I)) .OR. (RR.GE.RING(2,I))) GO TO 65
C                                   Proper ring - IRING - is found.
         IRING  = I
         INT = IRING
         INTX = XC(INT)
         INTY = YC(INT)
         INTP = PA(INT)
         INTI = IN(INT)
         INTS = VS(INT)
C
         RX = (JCOL - INTX) * DX
         RY = (JROW - INTY) * DY
         R2 = RY ** 2 + RX ** 2
         R  = SQRT (R2)
         RR = -1.0
         IF (R.GE.0.01) THEN
            CSP = COS (INTP * A2R)
            SNP = SIN (INTP * A2R)
            CSH = (CSP * RY / R - SNP * RX / R)
            SNH = -(CSP * RX / R + SNP * RY / R)
C                                   NB: for FLG=-1 (residual map)
C                                   whole map is used irrespective of
C                                   supplied limits
            IF (((CSH.GE.CMIN) .AND. (CSH.LE.CMAX) .AND. (SNH.GE.SMIN)
     *         .AND. (SNH.LE.SMAX)) .OR. (FLG.LT.0)) THEN
               SNI = SIN (INTI * A2R)
               CSI = COS (INTI * A2R)
               TEK = SIGN (1.0, SNI/CSI)
               SNI = ABS (SNI)
               CSI = ABS (CSI)
               CSHSNI= CSH * SNI
               CST = 0.0
               CSHCST = 0.0
               IF (CSI.NE.0.0) THEN
                  CSHCST= 1.0 / CSI * SQRT (1.0 - CSHSNI * CSHSNI)
                  IF (CSHCST.NE.0.0) CST = CSH / CSHCST
                  END IF
               RR = R * CSHCST
               END IF
            END IF
         IF (((RR.LT.RMIN) .OR. (RR.GT.RMAX)) .AND. (FLG.GE.0)) THEN
            FNC(J) = FBLANK
         ELSE
            IF (TYPE.EQ.'CC') THEN
               INTV = VM(INT)
            ELSE IF (TYPE.EQ.'SB') THEN
               INTV = VM(INT) * RR / 60.0
            ELSE IF (TYPE.EQ.'EX') THEN
               INTV = VM(INT) * (1.0 - EXP (-LN100 * RR / RMX(INT)))
            ELSE
               DENOM = 1.0/3.0 + 2.0/3.0 * ((RR/RMX(INT)) ** BRN(INT))
               DENOM = DENOM ** (3.0/2.0 / BRN(INT))
               INTV  = VM(INT) * RR / DENOM / RMX(INT)
               END IF
            VMOD   = INTS + INTV * SNI * CST
            FNC(J) = (VMOD - VOBS(JM))
            END IF
 220     CONTINUE
      GO TO 999
C                                    problems
 980  WRITE (MSGTXT,1980) I
      IERR = ERRNUM
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ',I3,1X,A,' Text File')
 1020 FORMAT ('WARNING:',I4,' VALUES OF RMAX WERE <= 0.0.',F7.1,' USED')
 1980 FORMAT ('ERROR PARSING INFILE LINE',I3)
      END
