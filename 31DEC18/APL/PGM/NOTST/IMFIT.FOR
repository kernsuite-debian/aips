LOCAL INCLUDE 'IMFPTS.INC'
C   The maximum number of pixels that JMFIT uses is encapsulated right
C   here.  Just increase MAXPTS to whatever level is desired.  The only
C   proper way to do this however is to rearrange FXDVD so that the
C   double precision vector RESID is no longer required
      INTEGER   MAXPTS
      PARAMETER (MAXPTS=10000)
LOCAL END
LOCAL INCLUDE 'IMFIT.INC'
      INCLUDE 'INCS:PMAD.INC'
C                                       Local include for IMFIT
      CHARACTER TITL1*132, TITL2*132, SCRTCH*132, LPNAME*48, INNA*36,
     *   OUTNA*36
      INTEGER   HOI(256), LUN1, INSL, OUTSL, DEPTH(5), NGAUSS, NITER,
     *   IVAR(24), JVAR(24), XTYPE(4), NX, NY, NPTS, PTMAX, PTMIN,
     *   NPARM, NNGOOD, LUNP, INDP, ILINE, IPAGE, OUTVER, NACROS, INVOL,
     *   FREQAX, INBLK(256)
      LOGICAL   NWIDTH(4), XPR
      INCLUDE 'IMFPTS.INC'
      REAL      WIN(4), ACTRMS, G(6,4), E(6,4), CB(3), DATA(MAXPTS),
     *   DMAX, DMIN, OFFSET, DOCRT, SUMSQ, RESSUM, RESMAX, XCEN, YCEN,
     *   LINE(MAXIMG), XU, HCB(3), UCB(3), HCBP(3)
      DOUBLE PRECISION XRA, XDEC, XFREQ
      COMMON /IMFDA/ XRA, XDEC, XFREQ, HOI, INBLK, DATA, DMAX, DMIN,
     *   OFFSET, NX, NY, NPTS, PTMAX, PTMIN, NPARM, NNGOOD, WIN, ACTRMS,
     *   LUN1, INSL, OUTSL, DEPTH, OUTVER, DOCRT, LUNP, INDP, ILINE,
     *   IPAGE, NGAUSS, NITER, XTYPE, G, E, CB, IVAR, JVAR, NWIDTH,
     *   SUMSQ, RESSUM, RESMAX, XCEN, YCEN, LINE, XU, HCB, UCB, HCBP,
     *   FREQAX, INVOL
      COMMON /CHRCOM/ TITL1, TITL2, SCRTCH, INNA, OUTNA, LPNAME
C                                       inputs
      REAL      XSE, XDI, XBL(7), XTR(7), XOS, XOD, XNG, XTY(4), GM(4),
     *   GP(2,4), GW(3,4), XDM(4), XDP(2,4), XDW(3,4), BWS, RADIUS, XNI,
     *   XCRT, XNDIG, XDOO, XOFS, XDOM, XOVER, XSTVER, PBPARM(7), FACTOR
      HOLLERITH XNA(3), XCL(2), XON(3), XOC(2), XOP(12)
      COMMON /INPARM/ XNA, XCL, XSE, XDI, XBL, XTR, XON, XOC, XOS,
     *   XOD, XNG, XTY, GM, GP, GW, XDM, XDP, XDW, BWS, RADIUS, XNI,
     *   XCRT, XNDIG, XOP, XDOO, XOFS, XDOM, XOVER, XSTVER, PBPARM,
     *   NACROS, XPR, FACTOR
C                                       outputs
      REAL      FGM(4), FGP(2,4), FGW(3,4), FSHIFT(2), FDM(4), FDP(2,4),
     *   FDW(3,4)
      COMMON /OUPARM/ FGM, FGP, FGW, FSHIFT, FDM, FDP, FDW
LOCAL END
LOCAL INCLUDE 'ORFIT.INC'
      CHARACTER REFRA*14, REFDEC*14
      REAL      ORA(4), ORERRA(4), ORERRX(4), ORERRY(4), ORERMA(4),
     *   ORERMI(4), ORERFI(4), ORBMAJ(4), ORBMIN(4), ORBPA(4)
      LOGICAL   OREXIS
      COMMON /VORFIT/ ORA, ORERRA, ORERRX, ORERRY, ORERMA, ORERMI,
     *   ORERFI, ORBMAJ, ORBMIN, ORBPA, OREXIS
      COMMON /CORFIT/ REFRA, REFDEC
LOCAL END
      PROGRAM IMFIT
C-----------------------------------------------------------------------
C! Fit Gaussians to an image
C# Map Modeling EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2001-2005, 2007-2015, 2018
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
C   IMFIT is a two-dimensional gaussian fitting program for image
C   modelling.  Up to four gaussian components can be fit to a part of
C   an image.
C   INPUTS:
C      INNAME(3)                Image name (name)
C      INCLASS(2)               Image name (class)
C      INSEQ                    Image name (seq. #)
C      INDISK                   Disk unit # of image
C      BLC(7)                   Bottom left corner to model
C      TRC(7)                   Top right corner to model
C      OUTNAME(3)               Image outname (name)
C      OUTCLASS(2)              Image outname (class)
C      OUTSEQ                   Image outname (seq. #)
C      OUTDISK                  Disk unit # of output
C      NGAUSS                   Number of gaussians to fit
C      XTYPE(4)                 Type of model component
C      GMAX(4)                  Peak intensity
C      GPOS(2,4)                (X,Y) Position in pixels
C      GWIDTH(3,4)              (BMAJ,BMIN,BPA) in pixels, pixels
C                                   degrees
C      NITER                    Number of loop iterations
C      DOMAX(4)                 Peak intensity variability
C      DOPOS(2,4)               (X,Y) Position variability
C      DOWIDTH(3,4)             (BMAJ,BMIN,BPA) variability
C      DOCRT                    Print output on Lineprinter?
C      DOOUTPUT                 Catalog residual map?
C      OFFSET                   Ignore values below OFFSET*PEAK
C      DOMODEL                  Make a CC file with input image
C
C     COMMON /IMFIO/ HOI, INNA, OUTNA, LUN1, WIN, INSL, OUTSL, DEPTH
C        HOI(256)      I          Output image header
C        WIN(4)        R          Window from MAPWIN
C        LUN1          I          Logical unit # for input
C        INSL          I          Slot number of INNA
C        OUTSL         I          Slot number of OUTNA
C        DEPTH(5)      I          Axis position axes on 3-7
C
C     COMMON /CHRCOM/ TITL1, TITL2, SCRTCH, INNA, OUTNA
C        INN           C          WaWa input map name
C        OUTNA         C          WaWa output map name
C
C     COMMON /IMMOD/ NGAUSS, NITER, XTYPE, G, E, CB, IVAR, JVAR
C        NGAUSS        I          Number of Gaussian components
C        NITER         I          Maximum number of iterations
C        XTYPE(4)      I          Component type
C        G(1,4)        R          Component intensities
C        G(2:3,4)      R          Component positions
C        G(4:6,4)      R          Component diameters
C        E(1,4)        R          Component intensity errors
C        E(2:3,4)      R          Component position errors
C        E(4:6,4)      R          Component diameter errors
C        CB(3)         R          Clean beam diameter
C        IVAR(24)      I          Comp. number of ith variable
C        JVAR(24)      I          Type number of ith variable
C
C     COMMON /IMFDA/ NX, NY, NPTS, DATA, DMAX, DMIN,
C                    BLNK, PTMAX, PTMIN, NPARM
C        DATA(MAXPTS)  R          Data point values
C        DMAX          R          Maximum value of data
C        DMIN          R          Minimum value of data
C        OFFSET        R          Disregard points below OFFSET*DMAX
C        NX            I          Number of columns
C        NY            I          Number of rows
C        NPTS          I          Number of points
C        BLNK          I          Blanking value 0-> none
C        PTMAX         I          Data point at maximum
C        PTMIN         I          Data point at minimum
C        NPARM         I          Number of fitting iterations
C   FCN is the subroutine which calculates the model and derivatives
C      maps where the RA increment was not equal to the DEC increment.
C-----------------------------------------------------------------------
      INCLUDE 'IMFIT.INC'
      CHARACTER PRGNAM*6
      INTEGER   INN, INVAR, LDJAC, NVAR, IER, IERR, INF, IPVT(24), NK,
     *   I, IRET, NSUMSQ
      REAL      BUFF(MAXPTS), RSTO, DSCALE, DOMODL
      DOUBLE PRECISION    VALVAR(24), FVEC(MAXPTS), FJAC(24,24),
     *   WA(3120), TOL, FJROW(24)
      EXTERNAL FCN
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGNAM /'IMFIT '/
C-----------------------------------------------------------------------
      IRET = 8
C                                        Some initialization
      NPARM = 111
      LDJAC = 24
      NNGOOD = 0
      IER = 0
C                                        Initialize POPS
      CALL TINBEG (PRGNAM, NPARM, XNA, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 900
         END IF
C                                       output adverbs
      CALL RCOPY (24, XDM, FDM)
      CALL RCOPY (24, GM, FGM)
      CALL RFILL (2, 0.0, FSHIFT)
      IF (FACTOR.LE.0.0) FACTOR = 1.3
C                                       Get inputs, open map
      XU = NLUSER
      CALL IMFOPN (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 900
         END IF
C                                       Print and store flags
      RSTO = XDOO
      OFFSET = XOFS
      DOMODL = XDOM
      OUTVER = XOVER + 0.1
      IF (XOVER.LT.0.0) OUTVER = -1
      CALL IMFDAT (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 900
         END IF
C                                       Insert defaults
      CALL IMFDEF
C                                       Determine scale factor
      DSCALE = MAX(DMAX, -DMIN)
      DSCALE = 10.0 ** (AINT (LOG10 (DSCALE/2.5) + 25.0) - 26.0)
      IF (-DMIN.GT.DMAX) DSCALE = -DSCALE
C                                        Print out input model
      XPR = XCRT.NE.0.0
      CALL IMFPRT (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 900
         END IF
C                                        Print out map data
      IF (XPR) CALL IMPLOT (1, DSCALE)
C                                       Close input map
      CALL FILCLS (LUN1)
C                                        Store variables properly
      CALL IMFVST (NVAR, VALVAR)
C                                        Save map data in a buffer
      NNGOOD = 0
      DO 60 I = 1,NPTS
         BUFF(I) = DATA(I)
         IF (DATA(I).NE.FBLANK) THEN
            DATA(I) = 0.0
            NNGOOD = NNGOOD + 1
            END IF
 60      CONTINUE
C                                        Calculate model data
      NPARM = 0
      INN = NNGOOD
      INVAR = NVAR
      CALL FCN (INN, INVAR, VALVAR, FVEC, FJROW, 0)
C                                        Print out model data
      NK = 0
      DO 65 I = 1,NPTS
         IF (DATA(I).NE.FBLANK) THEN
            NK = NK + 1
            DATA(I) = FVEC(NK)
            END IF
 65      CONTINUE
      IF (XPR) CALL IMPLOT (2, DSCALE)
C                                        Restore data from buffer
      NSUMSQ = 0
      SUMSQ = 0.0
      DO 70 I = 1,NPTS
         IF (BUFF(I).NE.FBLANK) THEN
            NSUMSQ = NSUMSQ + 1
            SUMSQ = SUMSQ + (DATA(I) - BUFF(I)) ** 2
            END IF
         DATA(I) = BUFF(I)
 70      CONTINUE
      IF (NSUMSQ.GT.0) SUMSQ = SQRT (SUMSQ / NSUMSQ)
      MSGTXT = ' '
      IF (DOCRT.GT.-2.5) THEN
         CALL DOPRT
      ELSE
         CALL MSGWRT (4)
         END IF
      WRITE (MSGTXT,1070) SUMSQ, NSUMSQ
      CALL DOPRT
C                                        Call fitting subroutine
      TOL = 1.0D-16
      SUMSQ = 0.0
      RESSUM = 0.0
      RESMAX = 0.0
      IF (NVAR.GT.0) THEN
         INN = NNGOOD
         INVAR = NVAR
         CALL LMSTR1 (FCN, INN, INVAR, VALVAR, FVEC, FJAC, LDJAC,
     *      TOL, INF, IPVT, WA, 3120)
C                                        Termination type
         CALL IMFMSG (INF, NPARM)
         IF ((INF.EQ.-11) .OR. (INF.EQ.4)) THEN
            WRITE (MSGTXT,1071)
            GO TO 900
            END IF
C                                        Get residual data
         NK = 0
         DO 80 I = 1,NPTS
            IF (DATA(I).NE.FBLANK) THEN
               NK = NK + 1
               DATA(I) = -FVEC(NK)
               SUMSQ = SUMSQ + DATA(I)**2
               RESSUM = RESSUM + DATA(I)
               RESMAX = MAX (RESMAX, ABS (DATA(I)))
               END IF
 80         CONTINUE
         IF (CB(1)*CB(2).NE.0.0) THEN
            RESSUM = RESSUM / (1.1331 * CB(1) * CB(2))
         ELSE IF (NK.NE.0) THEN
            RESSUM = RESSUM / NK
            END IF
         IF (NK.GT.0) SUMSQ = SQRT (SUMSQ / NK)
         END IF
C                                        Calculate error estimate
      INN = NNGOOD
      CALL IMFERR (BUFF, INN, NVAR, VALVAR, FVEC, FJAC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1080) IERR
         GO TO 900
         END IF
C                                        Residual plot?
      IF (XPR) CALL IMPLOT (3, DSCALE)
C                                       Store residual map
      IF (RSTO.GT.0.0) THEN
         CALL IMFSTO (IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1090) IERR
            GO TO 900
            END IF
         END IF
C                                        Convert to useful form
      IF (NVAR.NE.0) THEN
         CALL IMFOUT (RSTO, DOMODL, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR
            GO TO 900
            END IF
         END IF
C                                        Finished, no errors
      IRET = 0
      GO TO 980
C                                        Error return
 900  IER = 1
      CALL MSGWRT (8)
C                                       Close line printer
 980  IF (XCRT.NE.0.0) CALL LPCLOS (LUNP, INDP, ILINE, IERR)
C                                        Normal ending
      IF (.NOT.RQUICK) CALL PTPARM (50, FGM, BUFF, IERR)
      CALL TSKEND (IRET)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('COULD NOT INITIATE TASK.  IER=',I7)
 1020 FORMAT ('ERROR FROM IMFDEF.  IER=',I7)
 1030 FORMAT ('COUNT NOT STORE MAP DATA.  IER=',I7)
 1040 FORMAT ('COULD NOT PRINT INPUT MODEL.  IER=',I7)
 1070 FORMAT ('Initial guess RMS',1PE12.4,' in',I5,' usable pixels')
 1071 FORMAT ('CHECK INPUT MODEL.  MODEL VALUE IS CRAZY')
 1080 FORMAT ('COULD NOT GET MODEL ERRORS.  IER=',I7)
 1090 FORMAT ('COULD NOT STORE RESID MAP.  IER=',I7)
 1100 FORMAT ('COULD NOT PRINT CONVERTED MODEL.  IER=',I7)
      END
      SUBROUTINE IMFOPN (IER)
C-----------------------------------------------------------------------
C   IMFOPN checks the inputs, opens the input map, finds the true rms,
C   and then initializes the I/O
C   Outputs:
C      IER     I       Error return  0-> okay, 1-> error return
C-----------------------------------------------------------------------
      INTEGER   IER
C
      INCLUDE 'IMFIT.INC'
      INTEGER   IERR, IROUND, I, J, LOCS, NUMKEY, KEYTYP, MSGSAV, IFIL,
     *   CGBUFF(512), LUNCG, IRNO, CGKOLS(4), CGNUMV(4), NRNO, NWORDS
      LOGICAL   F
      HOLLERITH MA
      CHARACTER KEYWRD*8
      REAL      BPA0, BLC(7), TRC(7), VALUE, XC, YC, BMAJ, BMIN, BPA,
     *   RMSMAP(2)
      LONGINT   RMSPTR
      DOUBLE PRECISION X, XMIN
      EXTERNAL  FCN
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      CALL FILL (5, 1, DEPTH)
      IER = 0
      LUN1 = 16
      CALL RFILL (3, 0.0, CB)
      CALL H2CHR (48, 1, XOP, LPNAME)
C                                       Store name for easy I/O
      CALL CHR2H (4, 'MA  ', 1, MA)
      CALL H2WAWA (XNA, XCL, XSE, MA, XDI, XU, INNA)
C                                       Open input map
      CALL OPENCF (LUN1, INNA, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 900
         END IF
C                                       Get header values
      CALL GETHDR (LUN1, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1005) IERR
         GO TO 900
         END IF
      CALL COPY (256, CATBLK, INBLK)
C                                       check header for rms
      CALL FILNUM (LUN1, IFIL, IERR)
      INVOL = FILTAB (POVOL,IFIL)
      INSL = FILTAB (POCAT,IFIL)
C                                       set noise to use
C                                       direct adverb
      IF (RADIUS.LT.0.0) THEN
         ACTRMS = -RADIUS
         WRITE (MSGTXT,1006) ACTRMS
C                                       header or fit
      ELSE
         NUMKEY = 1
         KEYWRD = 'ACTNOISE'
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL CATKEY ('READ', INVOL, INSL, KEYWRD, NUMKEY, LOCS, VALUE,
     *      KEYTYP, LINE, IERR)
         MSGSUP = MSGSAV
         IF ((IERR.EQ.0) .AND. (VALUE.GT.0.0) .AND. (RADIUS.EQ.0.)) THEN
            ACTRMS = VALUE
            WRITE (MSGTXT,1007) ACTRMS
C                                       no get from data
         ELSE
C                                       init whole image plane
            CALL RCOPY (7, XBL, BLC)
            CALL RCOPY (7, XTR, TRC)
            BLC(1) = 1.0
            BLC(2) = 1.0
            TRC(1) = 0.0
            TRC(2) = 0.0
            XC = (XBL(1) + XTR(1)) / 2.0
            YC = (XBL(2) + XTR(2)) / 2.0
            IF (RADIUS.EQ.0.0) RADIUS = 1.E6
            RADIUS = RADIUS * RADIUS
C                                       Whole plane window
            CALL MAPWIN (LUN1, BLC, TRC, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR
               GO TO 900
               END IF
C                                       Get dimension of output
            WIN(1) = FILTAB(POBLC,6)
            WIN(2) = FILTAB(POBLC+1,6)
            WIN(3) = FILTAB(POTRC,6)
            WIN(4) = FILTAB(POTRC+1,6)
            NX = WIN(3) - WIN(1) + 1.5
            NY = WIN(4) - WIN(2) + 1.5
C                                       allocate memory
            NWORDS = (NX * NY - 1) / 1024 + 4
            CALL ZMEMRY ('GET ', 'JMFOPN', NWORDS, RMSMAP, RMSPTR, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'FAILED TO GET MEMORY FOR REAL RMS'
               CALL MSGWRT (8)
               GO TO 999
               END IF
C                                       get full rms
            CALL FNDRMS (NX, NY, RMSMAP(1+RMSPTR), LUN1, XC, YC,
     *         RADIUS, ACTRMS, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL ZMEMRY ('FREE', 'JMFOPN', NWORDS, RMSMAP, RMSPTR, IERR)
            IF (RADIUS.GT.1.E10) THEN
               WRITE (MSGTXT,1040) ACTRMS
            ELSE
               RADIUS = SQRT (RADIUS)
               WRITE (MSGTXT,1041) ACTRMS, RADIUS
               END IF
            END IF
         END IF
      CALL MSGWRT (3)
C                                       Window of map
      CALL MAPWIN (LUN1, XBL, XTR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 900
         END IF
C                                       Get dimension of output
      WIN(1) = FILTAB(POBLC,6)
      WIN(2) = FILTAB(POBLC+1,6)
      DEPTH(1) = FILTAB(POBLC+2,6)
      DEPTH(2) = FILTAB(POBLC+3,6)
      DEPTH(3) = FILTAB(POBLC+4,6)
      DEPTH(4) = FILTAB(POBLC+5,6)
      DEPTH(5) = FILTAB(POBLC+6,6)
      WIN(3) = FILTAB(POTRC,6)
      WIN(4) = FILTAB(POTRC+1,6)
      NX = WIN(3) - WIN(1) + 1.5
      NY = WIN(4) - WIN(2) + 1.5
      LOCNUM = 1
      CALL SETLOC (DEPTH, F)
      NPTS = NX * NY
      IF (NPTS.GT.MAXPTS) THEN
         WRITE (MSGTXT,1080) NX, NY, NPTS
         GO TO 900
         END IF
      XCEN = CATR(KRCRP)
      YCEN = CATR(KRCRP+1)
      XRA = 0.00D0
      XDEC = 0.0D0
      IF (AXTYP(LOCNUM).EQ.1) THEN
         XRA = CATD(KDORA)
         XDEC = CATD(KDODE)
         IF ((XDEC.NE.0.0D0) .OR. (XRA.NE.0.0D0)) THEN
            IF (CORTYP(LOCNUM).EQ.1) THEN
               CALL XYPIX (XRA, XDEC, XCEN, YCEN, IERR)
            ELSE
               CALL XYPIX (XDEC, XRA, XCEN, YCEN, IERR)
               END IF
            IF (IERR.NE.0) THEN
               XCEN = CATR(KRCRP)
               YCEN = CATR(KRCRP+1)
               IF (CORTYP(LOCNUM).EQ.1) THEN
                  XRA = CATD(KDCRV)
                  XDEC = CATD(KDCRV+1)
               ELSE
                  XRA = CATD(KDCRV+1)
                  XDEC = CATD(KDCRV)
                  END IF
               END IF
         ELSE
            IF (CORTYP(LOCNUM).EQ.1) THEN
               XRA = CATD(KDCRV)
               XDEC = CATD(KDCRV+1)
            ELSE
               XRA = CATD(KDCRV+1)
               XDEC = CATD(KDCRV)
               END IF
            END IF
      ELSE IF (BWS.GT.0.0) THEN
         MSGTXT = 'COORDINATES NOT RIGHT FOR BANDWIDTH SMEARING'
     *      // ' CORRECTION'
         CALL MSGWRT (6)
         BWS = 0.0
         END IF
C                                       Get slot number
      INSL = FILTAB(POCAT, 6)
C                                       Model defaults
      NGAUSS = IROUND (XNG)
      IF (NGAUSS.EQ.0) NGAUSS = 1
      NITER = IROUND (XNI)
      IF (NITER.EQ.0) NITER = 200 * NGAUSS
C                                       Loop over component
      DO 160 I = 1,NGAUSS
         XTYPE(I) = IROUND (XTY(I))
         IF (XTYPE(I).LT.1) XTYPE(I) = 1
         G(1,I) = GM(I)
         E(1,I) = XDM(I)
         DO 120 J = 2,3
            G(J,I) = GP(J-1,I)
            E(J,I) = XDP(J-1,I)
 120        CONTINUE
         NWIDTH(I) = .TRUE.
         DO 140 J = 4,6
            G(J,I) = GW(J-3,I)
            E(J,I) = XDW(J-3,I)
            NWIDTH(I) = NWIDTH(I) .AND. (E(J,I).LE.0)
 140        CONTINUE
 160     CONTINUE
C                                       Header Clean beam in pixels
      HCB(1) = CATR(KRBMJ)
      HCB(2) = CATR(KRBMN)
      HCB(3) = CATR(KRBPA)
      BPA0 = HCB(3) - ROT(LOCNUM)
      CALL ELIPSQ (HCB(1), HCB(2), BPA0, -AXINC(1,LOCNUM),
     *   AXINC(2,LOCNUM), HCBP(1), HCBP(2), HCBP(3))
C                                       Actual Clean beam
      CB(1) = -1.0
      XFREQ = 0.0D0
      CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATBLK(KHCTP), FREQAX,
     *   IERR)
      IF (IERR.EQ.0) THEN
         XFREQ = CATD(KDCRV+FREQAX) + CATR(KRCIC+FREQAX) *
     *      (DEPTH(FREQAX-1) - CATR(KRCRP+FREQAX))
         CALL FNDEXT ('CG', CATBLK, J)
C                                       find Clean beam from CG table
         IF (J.GT.0) THEN
            LUNCG = 57
            CALL CGINI ('READ', CGBUFF, INVOL, INSL, J, CATBLK, LUNCG,
     *         IRNO, CGKOLS, CGNUMV, I, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1160) IERR, 'OPENING CG TABLE'
               CALL MSGWRT (7)
               GO TO 180
               END IF
            XMIN = 1.D16
            NRNO = CGBUFF(5)
            DO 170 I = 1,NRNO
               IRNO = I
               CALL TABCG ('READ', CGBUFF, IRNO, CGKOLS, CGNUMV, X,
     *            BMAJ, BMIN, BPA, IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1160) IERR, 'READING CG TABLE'
                  GO TO 900
               ELSE IF (IERR.EQ.0) THEN
                  IF (ABS(X-XFREQ).LT.XMIN) THEN
                     XMIN = ABS (X - XFREQ)
                     UCB(1) = BMAJ
                     UCB(2) = BMIN
                     UCB(3) = BPA
                     END IF
                  END IF
 170           CONTINUE
            BPA0 = UCB(3) - ROT(LOCNUM)
            CALL ELIPSQ (UCB(1), UCB(2), BPA0, -AXINC(1,LOCNUM),
     *         AXINC(2,LOCNUM), CB(1), CB(2), CB(3))
            CALL TABCG ('CLOS', CGBUFF, IRNO, CGKOLS, CGNUMV, X, BMAJ,
     *         BMIN, BPA, IERR)
            BMAJ = UCB(1) * 3600.
            BMIN = UCB(2) * 3600.
            BPA = UCB(3)
            WRITE (MSGTXT,1170) BMAJ, BMIN, BPA, 'CG table'
            END IF
         END IF
C                                       did not find in CG table
 180  IF (CB(1).LT.0.0) THEN
         CALL RCOPY (3, HCB, UCB)
         CALL RCOPY (3, HCBP, CB)
         BMAJ = UCB(1) * 3600.
         BMIN = UCB(2) * 3600.
         BPA = UCB(3)
         WRITE (MSGTXT,1170) BMAJ, BMIN, BPA, 'image header'
         END IF
      CALL MSGWRT (3)
      GO TO 999
C                                       Error return
 900  IER = 1
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMFOPN: COULD NOT OPEN INPUT MAP.  IER=',I7)
 1005 FORMAT ('IMFOPN: COULD NOT GET HEADER.  IER=',I7)
 1006 FORMAT ('True rms taken from RADIUS adverb =',1PE10.3)
 1007 FORMAT ('True rms taken from ACTNOISE in header =',1PE10.3)
 1010 FORMAT ('IMFOPN: COULD NOT MAPWIN.  IER=',I7)
 1040 FORMAT ('Robust solution gives RMS =',1PE10.3)
 1041 FORMAT ('Robust solution gives RMS =',1PE10.3,' in radius',
     *   0PF7.1,' pixels')
 1080 FORMAT ('IMFOPN: TOO MANY POINTS',I4,'X',I4,'=',I7,' POINTS')
 1160 FORMAT ('IMFOPN ERROR',I3,' ON ',A)
 1170 FORMAT ('Using Clean beam',2F10.5,F8.2,' from ',A)
      END
      SUBROUTINE FNDRMS (NX, NY, IMAGE, LUN, XC, YC, RADIUS, ACTRMS,
     *   IERR)
C-----------------------------------------------------------------------
C   Does robust rms on full image plane within a radius
C   Inputs:
C      NX       I      Number X pixels
C      NY       I      Number Y pixels
C      LUN      I      LUN to read image
C      XC       R      center X pixel
C      YC       R      center Y pixel
C      RADIUS   R      circle to do rms (squared in pixels)
C   Outputs:
C      IMAGE    R(*)   full image data
C      ACTRMS   R      actual robust rms
C      IERR     I      error code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, LUN, IERR
      REAL      IMAGE(NX,*), XC, YC, RADIUS, ACTRMS
C
      INTEGER   NITER
      PARAMETER (NITER=8)
C
      INTEGER   I, J, NTRY, IT
      REAL      WS(NITER), VP, VM, R, V
      DOUBLE PRECISION SV, SSV, NV
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA WS /5.0, 4.0, 3.5, 3.0, 2.7, 2.6, 2.5, 3.5/
C-----------------------------------------------------------------------
      NTRY = 0
      DO 20 J = 1,NY
         CALL MAPIO ('READ', LUN, IMAGE(1,J), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING IMAGE FOR RMS'
            CALL MSGWRT (8)
            GO TO 999
            END IF
 20      CONTINUE
C                                       ROBUST mean, rms
 25   VP = 1.E5
      VM = -VP
      DO 80 IT = 1,NITER
         SV = 0.0D0
         SSV = 0.0D0
         NV = 0.0D0
         DO 40 J = 1,NY
            DO 30 I = 1,NX
               V = IMAGE(I,J)
               IF ((V.NE.FBLANK) .AND. (V.NE.0.0)) THEN
                  R = (XC-I)*(XC-I) + (YC-J)*(YC-J)
                  IF (R.LE.RADIUS) THEN
                     IF ((V.GT.VM) .AND. (V.LT.VP)) THEN
                        SV = SV + V
                        SSV = SSV + V * V
                        NV = NV + 1.0D0
                        END IF
                     END IF
                  END IF
 30            CONTINUE
 40         CONTINUE
         IF (NV.GT.0.0D0) THEN
            SV = SV / NV
            SSV = SSV / NV - SV * SV
            SSV = SQRT (MAX (0.0D0, SSV))
            IF (IT.LT.NITER) THEN
               VP = SV + WS(IT+1) * SSV
               VM = SV - WS(IT+1) * SSV
               END IF
         ELSE
            VP = 1.E4
            VM = -1.E4
            END IF
 80      CONTINUE
C                                       okay??
      IF (NV.LE.5.0D0) THEN
         IF ((NTRY.EQ.0) .AND. (RADIUS.LE.NX*NY)) THEN
            MSGTXT = 'FNDRMS TRYING 2ND TIME WITH LARGE RADIUS'
            CALL MSGWRT (7)
            RADIUS = 1.E10
            GO TO 25
            END IF
         MSGTXT = 'FNDRMS FAILS TO FIND ROBUST RMS'
         CALL MSGWRT (8)
         IERR = 10
         END IF
C-
      ACTRMS = SSV
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FNDRMS: ERROR',I4,' ON ',A)
      END
      SUBROUTINE IMFDAT (IER)
C-----------------------------------------------------------------------
C   Subroutine IMFDAT reads the input data into the array DATA.
C   INPUTS:        none
C   OUTPUTS:
C     IER       I         Error return 0 -> okay
C                            1 -> error
C-----------------------------------------------------------------------
      INTEGER   IER, IERR, PTS, I, J
      LOGICAL   FPOS
      INCLUDE 'IMFIT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
C-----------------------------------------------------------------------
C                                       Initialize
      IER = 0
      DMAX = -1.0E25
      DMIN = 1.0E25
      FPOS = .TRUE.
      PTS = 0
C                                       Line loop
      DO 40 J = 1,NY
         CALL MAPIO ('READ', LUN1, LINE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) I, IERR
            GO TO 900
            END IF
C                                       Row loop, fill data
         DO 30 I = 1,NX
            PTS = PTS + 1
            DATA(PTS) = LINE(I)
C                                       Look for a blank pixel
            IF (LINE(I).NE.FBLANK) THEN
C                                       Look for extremum
               IF (DMAX.LE.LINE(I)) THEN
                  DMAX = LINE(I)
                  PTMAX = PTS
                  END IF
               IF (DMIN.GE.LINE(I)) THEN
                  DMIN = LINE(I)
                  PTMIN = PTS
                  END IF
               END IF
 30         CONTINUE
 40      CONTINUE
      IF (PTS.NE.NPTS) THEN
         WRITE (MSGTXT,1040) PTS, NPTS
         GO TO 900
         END IF
C                                        Apply cutoff
      IF (OFFSET.NE.0.0) THEN
         IF (DMAX.LT.-DMIN) FPOS = .FALSE.
         IF (FPOS) THEN
            OFFSET = OFFSET * DMAX
         ELSE
            OFFSET = OFFSET * DMIN
            END IF
         DO 70 I = 1,NPTS
            IF (DATA(I).EQ.FBLANK) GO TO 70
            IF (FPOS .AND. (DATA(I).GE.OFFSET)) GO TO 70
            IF (.NOT.FPOS .AND. (DATA(I).LE.OFFSET)) GO TO 70
               DATA(I) = FBLANK
 70         CONTINUE
         END IF
      GO TO 999
C                                       Error return
 900  IER = 1
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMFDAT: COULD NOT READ LINE ',I7,'  IER=',I7)
 1040 FORMAT ('JMFDAT: # point read=',I7,'  NX x NY=',I7)
      END
      SUBROUTINE IMFDEF
C-----------------------------------------------------------------------
C   IMFDEF is a subroutine for IMFIT which inserts defaults for
C   the input parameters.
C-----------------------------------------------------------------------
      INTEGER   ITEMP, IBTEMP, I, J, IERR
      REAL      TEMP, DX, DY, SMCB(3)
      LOGICAL   FMAX, FIRST
      DOUBLE PRECISION XD, X(3)
      INCLUDE 'IMFIT.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                        Initialize
C                                        Min or max higher?
      FMAX = .TRUE.
      FIRST = .TRUE.
      IF (DMAX.LT.-DMIN) FMAX=.FALSE.
C                                        Component loop
      DO 300 I = 1,NGAUSS
C                                        XTYPE 1 = Gaussian
C                                              2 = Zero offset
C                                              3 = Zero, slope
C                                              4 = Zero, slope, curv
C                                              5 = General six param
         IBTEMP = XTYPE(I)
C                                        Gaussian component
         IF (IBTEMP.EQ.1) THEN
            IF (G(1,I).EQ.0.0) THEN
C                                           Amplitude.  Use max
               TEMP = 0.1
               IF (FIRST) TEMP = 1.0
               IF (FMAX) THEN
                  G(1,I) = DMAX * TEMP
               ELSE
                  G(1,I) = DMIN * TEMP
                  END IF
               FIRST = .FALSE.
               END IF
C                                           Position.  Use max posit.
            IF ((G(2,I).EQ.0.0) .AND. (G(3,I).EQ.0.0)) THEN
               IF (FMAX) THEN
                  G(2,I) = AINT(WIN(1) + MOD(PTMAX-1,NX) + 0.5)
                  G(3,I) = AINT(WIN(2) + AINT((PTMAX-0.5)/NX) + 0.5)
               ELSE
                  G(2,I) = AINT(WIN(1) + MOD(PTMIN-1,NX) + 0.5)
                  G(3,I) = AINT(WIN(2) + AINT((PTMIN-0.5)/NX) + 0.5)
                  END IF
               END IF
            IF (G(4,I).EQ.0.0) THEN
C                                       Widths
               IF (CB(1).NE.0.0) THEN
C                                       Use clean beam if avail.
C                                       BW smearing here
C                                       Radius
                  XD = 0.0D0
                  IF (((CORTYP(LOCNUM).EQ.1) .OR. (CORTYP(LOCNUM).EQ.2))
     *               .AND. (BWS.GT.0.0)) THEN
                     DX = G(2,I)
                     DY = G(3,I)
                     CALL XYVAL (DX, DY, X(1), X(2), X(3), IERR)
                     IF (IERR.NE.0) THEN
                        XD = 0.0D0
                     ELSE IF (CORTYP(LOCNUM).EQ.1) THEN
                        XD = SIN (DG2RAD*X(2)) * SIN (DG2RAD*XDEC) +
     *                     COS (DG2RAD*X(2)) * COS (DG2RAD*XDEC) *
     *                     COS (DG2RAD * (X(1) - XRA))
                     ELSE
                        XD = SIN (DG2RAD*X(1)) * SIN (DG2RAD*XDEC) +
     *                     COS (DG2RAD*X(1)) * COS (DG2RAD*XDEC) *
     *                     COS (DG2RAD * (X(2) - XRA))
                        END IF
                     IF (XD.GT.1.0D0) THEN
                        XD = 0.0D0
                     ELSE
                        XD = RAD2DG * ACOS (XD)
                        END IF
                     END IF
                  DX = G(2,I) - XCEN
                  DY = G(3,I) - YCEN
                  CALL BWSMCB (DX, DY, XD, BWS, CB, SMCB)
                  G(4,I) = SMCB(1)
                  G(5,I) = SMCB(2)
                  G(6,I) = SMCB(3)
C                                       Use 2 pixels
               ELSE
                  G(4,I) = 2.0
                  G(5,I) = 2.0
                  G(6,I) = 0.0
                  END IF
               END IF
C                                        Baseline
         ELSE IF ((IBTEMP.GE.2) .AND. (IBTEMP.LE.4)) THEN
            ITEMP = XTYPE(I) - 0.9
            XTYPE(I) = 2
            DO 150 J = 1,6
               G(J,I) = 0.0
               E(J,I) = -1.0
 150           CONTINUE
C                                        Zero offset only
            IF (IBTEMP.EQ.2) THEN
               E(1,I) = 1.0
C                                        Zero and slope
            ELSE IF (IBTEMP.EQ.3) THEN
               DO 190 J = 1,3
                  E(J,I) = 1.0
 190              CONTINUE
C                                        Zero, slope and curvature
            ELSE
               DO 210 J = 1,6
                  E(J,I) = 1.0
 210              CONTINUE
               END IF
            END IF
 300     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE IMFPRT (IER)
C-----------------------------------------------------------------------
C   IMFPRT is a subroutine of IMFIT which prints out models on the line
C   printer and the message terminal.
C   Outputs:
C      IER       I      Error status   0=> okay
C                             1=> problem
C-----------------------------------------------------------------------
      CHARACTER RSTR(2)*20, HFIX*2, MODEL(3)*8, CNAME*12, CCLAS*6,
     *   CUNIT*8, COBJ*8, CTYPX*2, FAST(6)*2, FILSPC*256
      INTEGER   IER, IERR, IBTEMP, CVOL, CUID, PASUM, SEQ, I, J, ILEN,
     *   I2TMP, PERR, IBUFF(256), ITRIM, FLEN, JERR
      REAL      DXREF, DYREF, RCONST, XREF, YREF, TEMP
      DOUBLE PRECISION    X(3)
      LOGICAL   ATEST
      INCLUDE 'IMFIT.INC'
      INCLUDE 'ORFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA HFIX  /'**'/
      DATA MODEL /'Gaussian', 'Baseline', '????????'/
C-----------------------------------------------------------------------
C                                        Initialize
      IER = 0
      ATEST = .FALSE.
      PASUM = 90.
      RCONST = 3.14159265 / 180.0
      CALL WAWA2A (INNA, CNAME, CCLAS, SEQ, CTYPX, CVOL, CUID)
      CALL H2CHR (8, 1, CATH(KHBUN), CUNIT)
      CALL H2CHR (8, 1, CATH(KHOBJ), COBJ)
C                                       Line printer initialization
      PERR = 0
      OREXIS = .FALSE.
      DOCRT = 1.0
      IF (XPR) THEN
         IF (LPNAME.EQ.' ') THEN
            DOCRT = MAX (-1.0, XCRT)
         ELSE
            DOCRT = MIN (-1.0, XCRT)
            END IF
         IF ((DOCRT.LT.-3.5) .AND. (LPNAME.NE.' ')) THEN
            CALL ZFULLN (LPNAME, ' ', ' ', FILSPC, JERR)
            IF (JERR.EQ.0) THEN
               FLEN = ITRIM (FILSPC)
               INQUIRE (FILE=FILSPC, EXIST=OREXIS)
               END IF
            END IF
         ILINE = 900
         IPAGE = 0
         CALL LPOPEN (LPNAME, DOCRT, LUNP, INDP, NACROS, IBUFF, PERR)
         IF ((PERR.EQ.0) .AND. (DOCRT.GT.-3.5)) THEN
            WRITE (TITL1,1020) CNAME, CCLAS, SEQ, COBJ, CUNIT
            TITL2 = ' '
            IF (DOCRT.LE.-2.5) CALL PRTLIN (LUNP, INDP, DOCRT, NACROS,
     *         TITL1, TITL2, TITL1, ILINE, IPAGE, SCRTCH, PERR)
            END IF
         IF (PERR.NE.0) XPR = .FALSE.
         END IF
C                                        Some header information
      WRITE (MSGTXT,1021) CNAME, CCLAS, SEQ
      CALL MSGWRT (4)
      WRITE (MSGTXT,1022) COBJ, CUNIT
      CALL MSGWRT (4)
      XREF = CATR(KRCRP)
      YREF = CATR(KRCRP+1)
      DXREF = CATR(KRCIC)
      DYREF = CATR(KRCIC+1)
      CALL XYVAL (XREF, YREF, X(1), X(2), X(3), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (8)
         GO TO 115
         END IF
C                                        Check axes types
C                                        Normal RA and DEC
      IF (AXTYP(LOCNUM).EQ.1) THEN
         DXREF = 3600.0 * DXREF
         DYREF = 3600.0 * DYREF
         DO 105 J = 1,2
            I2TMP = J - 1
            CALL AXSTRN (CTYP(J,LOCNUM), X(J), I2TMP, ILEN, RSTR(J))
 105        CONTINUE
         WRITE (MSGTXT,1105) XREF, RSTR(1), DXREF
         CALL DOPRT
         WRITE (MSGTXT,1107) YREF, RSTR(2), DYREF
         CALL DOPRT
         REFRA = RSTR(1)(4:17)
         IF (RSTR(2)(5:5).EQ.'-') THEN
            REFDEC = RSTR(2)(5:18)
         ELSE
            REFDEC = RSTR(2)(4:17)
            END IF
C                                        Not RA and DEC
      ELSE
         WRITE (MSGTXT,1110) XREF, X(1), DXREF
         CALL DOPRT
         WRITE (MSGTXT,1112) YREF, X(2), DYREF
         CALL DOPRT
         END IF
C                                       Other axes
 115  WRITE (MSGTXT,1115) DEPTH
      IF (DOCRT.GT.-2.5) THEN
         CALL DOPRT
      ELSE
         CALL MSGWRT (4)
         END IF
      DO 116 J = 1,2
         IF (SAXLAB(J,LOCNUM).NE.' ') THEN
            WRITE (MSGTXT,1116) J+2, SAXLAB(J,LOCNUM)
            IF (DOCRT.GT.-2.5) THEN
               CALL DOPRT
            ELSE
               CALL MSGWRT (4)
               END IF
            END IF
 116     CONTINUE
      WRITE (MSGTXT,1120)
      IF (DOCRT.GT.-2.5) THEN
         CALL DOPRT
      ELSE
         CALL MSGWRT (4)
         END IF
      WRITE (MSGTXT,1122)
      CALL DOPRT
C                                        Print input model
      DO 400 I = 1,NGAUSS
         FAST(1) = '  '
         FAST(2) = '  '
         FAST(3) = '  '
         FAST(4) = '  '
         FAST(5) = '  '
         FAST(6) = '  '
C                                        Gaussian comp
         IBTEMP = XTYPE(I)
         IF (IBTEMP.NE.2) THEN
            DO 140 J = 1,6
               IF (E(J,I).LE.0.0) FAST(J) = HFIX
 140           CONTINUE
            WRITE (MSGTXT,1140) I, MODEL(IBTEMP)
            CALL DOPRT
            WRITE (MSGTXT,1141) G(1,I), FAST(1), CUNIT
            CALL DOPRT
            WRITE (MSGTXT,1142) (G(J,I), FAST(J), J = 2,3)
            CALL DOPRT
            WRITE (MSGTXT,1144) (G(J,I), FAST(J), J = 4,6)
            CALL DOPRT
C                                        Fix diameters
            G(4,I) = ABS (G(4,I))
            G(5,I) = ABS (G(5,I))
            IF (G(4,I).LT.G(5,I)) THEN
               TEMP = G(5,I)
               G(5,I) = G(4,I)
               G(4,I) = TEMP
               G(6,I) = G(6,I) + 90.0
               END IF
C                                        Diameter circularity
            IF ((G(4,I)-G(5,I).LE.G(4,I)/10.0) .AND. (E(4,I).GT.0.0)
     *         .AND. (E(5,I).GT.0.0)) THEN
               G(4,I) = G(4,I)*1.10
               G(5,I) = G(5,I)/1.10
               WRITE (MSGTXT,1150)
               IF (DOCRT.GT.-2.5) THEN
                  CALL DOPRT
               ELSE
                  CALL MSGWRT (4)
                  END IF
               END IF
C                                        Fix position angle
            G(6,I) = (G(6,I) - PASUM) * RCONST
C                                        Baseline term
         ELSE
            WRITE (MSGTXT,1230) I, MODEL(IBTEMP)
            CALL DOPRT
C                                        Zero offset?
            IF (E(1,I).GT.0.0) THEN
               WRITE (MSGTXT,1232) G(1,I), FAST(1), CUNIT
               CALL DOPRT
               END IF
C                                        Slope?
            IF ((E(2,I).GT.0.0) .OR. (E(3,I).GT.0.0)) THEN
               WRITE (MSGTXT,1260) (G(J,I), FAST(J), J = 2,3)
               CALL DOPRT
               END IF
C                                        Curvature?
            IF ((E(4,I).GT.0.0) .OR. (E(5,I).GT.0.0) .OR.
     *         (E(6,I).GT.0.0)) THEN
               WRITE (MSGTXT,1290) (G(J,I), FAST(J), J = 4,6)
               CALL DOPRT
               END IF
C                                        Fix up ampl and angles
            G(3,I) = (G(3,I) - PASUM) * RCONST
            G(6,I) = (G(6,I) - PASUM) * RCONST
            END IF
         DO 370 J = 1,6
            IF (FAST(J).EQ.HFIX) ATEST = .TRUE.
 370        CONTINUE
 400     CONTINUE
C                                        Any parameters fixed?
      IF (ATEST) THEN
         WRITE (MSGTXT,1415)
         IF (DOCRT.GT.-2.5) THEN
            CALL DOPRT
         ELSE
            CALL MSGWRT (4)
            END IF
         END IF
C                                        Clean beam information
      IF (CB(1).GT.0.0) THEN
         WRITE (MSGTXT,1120)
         IF (DOCRT.GT.-2.5) THEN
            CALL DOPRT
         ELSE
            CALL MSGWRT (4)
            END IF
         WRITE (MSGTXT,1418) CB(1), CB(2), CB(3)
      ELSE
         WRITE (MSGTXT,1420)
         END IF
      CALL DOPRT
C                                        Offset?
      IF (OFFSET.GT.0.0) THEN
         WRITE (MSGTXT,1430) OFFSET, CUNIT
         CALL DOPRT
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Model fit to ',A12,'.',A6,'.',I4,'  Source= ',A8,' in ',
     *   A8)
 1021 FORMAT ('Model fit to ',A12,'.',A6,'.',I4)
 1022 FORMAT ('Source= ',A8,' in units ',A8)
 1100 FORMAT ('ERROR',I3,' CONVERTING TO REFERENCE COORDINATES')
 1105 FORMAT ('X-ref pix=',F6.1,2X,A,' Pix sep (asec)',F8.4)
 1107 FORMAT ('Y-ref pix=',F6.1,2X,A,' Pix sep (asec)',F8.4)
 1110 FORMAT ('X-ref pix=',F6.1,'  Value=',1PE14.5,'  Pix sep=',
     *   1PE13.5)
 1112 FORMAT ('Y-ref pix=',F6.1,'  Value=',1PE14.5,'  Pix sep=',
     *   1PE13.5)
 1115 FORMAT ('Axes 3-7 pixels =',5I7)
 1116 FORMAT ('Axis',I2,' : ',A)
 1120 FORMAT (1X,30('- '))
 1122 FORMAT ('******** Input Model ',2('********************'))
 1140 FORMAT ('Component=',I3,2X,A)
 1141 FORMAT (6X,'Peak=',1PE11.4,1X,A2,1X,A8)
 1142 FORMAT (6X,'Xpos=',F8.2,1X,A2,'   Ypos=',F8.2,1X,A2,'  pixels')
 1144 FORMAT (6X,'Size=',F7.2,1X,A2,' x',F7.2,1X,A2,' Pixels at pa',
     *   F7.2,1X,A2,' deg')
 1150 FORMAT (6X,'Component made 10% elliptical for better fitting')
 1230 FORMAT ('Component=',I3,2X,A8)
 1232 FORMAT (6X,'Zero lev=',1PE11.4,1X,A2,1X,A8)
 1260 FORMAT (6X,'Slope=',1PE11.4,1X,A2,' Ampl/pixel:   Angle=',0PF6.2,
     *   1X,A2)
 1290 FORMAT (6X,'Curvature=',1PE11.4,1X,A2,'   Eccen=',F5.3,1X,A2,
     *   '   Angle=',F6.2,1X,A2)
 1415 FORMAT (6X,13X,'** means parameter is held fixed')
 1418 FORMAT (6X,'Beam=',F7.2,3X,' X',F7.2,3X,' Pixels: in pa',
     *   F6.2,3X,' Deg')
 1420 FORMAT (6X,'No CLEAN beam')
 1430 FORMAT (6X,'Clipping below',1PE11.4,1X,A8)
      END
      SUBROUTINE IMPLOT (N, DSCALE)
C-----------------------------------------------------------------------
C   Subroutine IMPLOT prints a digital representation of the maps from
C   IMFIT on the line printer.
C   Inputs:
C      N        I   Type of map
C      DSCALE   R   Scale of display
C-----------------------------------------------------------------------
      CHARACTER  BLANKS*132, ALINE*132, WORD(3)*12, CTEMP*8
      INTEGER   N, WIN1, WIN2, WIN3, WIN4, WINH, WINL, IPTS, I, J, IL,
     *   PLINE, PERR, IROUND, NXX, IC, IP, IS
      REAL      DSCALE, TEMP, DTEMP, DSAVE, LSCALE, RMIN, RMAX
      INCLUDE 'IMFIT.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE DSAVE
      DATA BLANKS /' '/
      DATA WORD /'Input map   ', 'Input model ', 'Residual map'/
C-----------------------------------------------------------------------
      IF (XCRT.LT.-3.5) GO TO 999
      WIN1 = WIN(1) + 0.01
      WIN2 = WIN(2) + 0.01
      WIN3 = WIN(3) + 0.01
      WIN4 = WIN(4) + 0.01
      NXX = WIN3 - WIN1 + 1
      IC = (NACROS - 6) / NXX
      IC = MIN (6, MAX (IC, 3))
      IF ((XNDIG.GE.3.0) .AND. (XNDIG.LT.IC)) IC = XNDIG + 0.01
      IL = (NACROS - 6) / IC
      LSCALE = DSCALE
      IF (IC.GT.3) LSCALE = DSCALE / (10.0 ** (IC-3))
      RMAX = (10.0**IC) - 0.51
      RMIN = -(10.0**(IC-1)) + 0.51
C                                       Write map scale
      WRITE (ALINE,1000) WORD(N), LSCALE
      IF ((N.GT.1) .AND. (DSAVE.NE.LSCALE)) WRITE (ALINE,1001)
     *   WORD(N), LSCALE
      IF (DOCRT.GT.-2.5) THEN
         CALL PRTLIN (LUNP, INDP, DOCRT, NACROS, TITL1, TITL2, BLANKS,
     *      ILINE, IPAGE, SCRTCH, PERR)
         IF (PERR.NE.0) GO TO 900
         END IF
      CALL PRTLIN (LUNP, INDP, DOCRT, NACROS, TITL1, TITL2, ALINE,
     *   ILINE, IPAGE, SCRTCH, PERR)
      IF (PERR.NE.0) GO TO 900
      IF (DOCRT.GT.-2.5) THEN
         CALL PRTLIN (LUNP, INDP, DOCRT, NACROS, TITL1, TITL2, BLANKS,
     *      ILINE, IPAGE, SCRTCH, PERR)
         IF (PERR.NE.0) GO TO 900
         END IF
C                                       Map headings
      DTEMP = LSCALE
      DSAVE = LSCALE
      WINL = WIN1
      WINH = WIN3
      IF (WINH-WINL.GE.IL) THEN
         WINL = WINL + (WINH - WINL - IL) / 2
         WINH = WINL + IL - 1
         END IF
      ALINE = ' '
      IP = IC + 1
      DO 20 I = WINL,WINH,2
          WRITE (CTEMP,1020) I
          ALINE(IP:) = CTEMP
          IP = IP + 2 * IC
 20       CONTINUE
      CALL PRTLIN (LUNP, INDP, DOCRT, NACROS, TITL1, TITL2, ALINE,
     *   ILINE, IPAGE, SCRTCH, PERR)
      IF (PERR.NE.0) GO TO 900
      IL = WIN4 + 1
C                                       Line loop
      IS = 9 - IC
      DO 40 I = WIN2,WIN4
         IL = IL - 1
         IPTS = (IL - WIN2) * (WIN3 - WIN1 + 1) + (WINL - WIN1)
C                                       Row loop
         IP = 6
         WRITE (ALINE,1020) IL
         DO 30 J = WINL,WINH
            IPTS = IPTS + 1
            TEMP = DATA(IPTS)
            IF (TEMP.NE.FBLANK) THEN
               TEMP = TEMP / DTEMP
               TEMP = MAX (RMIN, MIN (RMAX, TEMP))
               PLINE = IROUND (TEMP)
               WRITE (CTEMP,1030) PLINE
               ALINE(IP:) = CTEMP(IS:)
               END IF
            IP = IP + IC
 30         CONTINUE
C                                       Write a line of data
         CALL PRTLIN (LUNP, INDP, DOCRT, NACROS, TITL1, TITL2, ALINE,
     *      ILINE, IPAGE, SCRTCH, PERR)
         IF (PERR.NE.0) GO TO 900
 40      CONTINUE
      GO TO 999
C
 900  XPR = .FALSE.
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A12,10X,'1 Map unit=',1PE10.3)
 1001 FORMAT (A12,10X,'1 Map unit=',1PE10.3,'  Changed  ********')
 1020 FORMAT (I5)
 1030 FORMAT (I8)
      END
      SUBROUTINE IMFVST (NVAR, VALVAR)
C-----------------------------------------------------------------------
C   INFVST is a subroutine for IMFIT which stores the variable
C   parameters in the proper arrays for use in FCN and LMSTR1.
C   INPUTS:  none  see COMMONs
C   OUTPUTS:
C      NVAR            I       The number of variables
C      VALVAR(24)      D       The value of this variable
C-----------------------------------------------------------------------
      INTEGER   NVAR, I, J
      DOUBLE PRECISION    VALVAR(24)
      INCLUDE 'IMFIT.INC'
C-----------------------------------------------------------------------
C                                       Initialize
      NVAR = 0
C                                       Loop over components
      DO 50 I = 1,NGAUSS
         DO 40 J = 1,6
C                                       not holding parameter fixed
            IF (E(J,I).GT.0.0) THEN
               NVAR = NVAR + 1
               VALVAR(NVAR) = G(J,I)
               IVAR(NVAR) = I
               JVAR(NVAR) = J
               E(J,I) = 1.0
            ELSE
               E(J,I) = -1.0
               END IF
 40         CONTINUE
 50      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FCN (INN, INVAR, VALVAR, FVEC, FJAC, IFLAG)
C-----------------------------------------------------------------------
C   FCN calculates the difference between the model and the data.  This
C   subroutine is used in the task IMFIT.
C   Inputs:
C      INN         I   The number of data points (adj. array dim.)
C      INVAR       I   The number of variable parameters (adj. array
C                      dim.)
C      VALVAR(24)  D   The value of the variable param.
C      IFLAG       I   Type of operation
C                        0 = Calculate function outside LMDER
C                        1 = Calculate function inseide LMDER
C                        N >1 Calculate Jacobian of row N-1
C                             where N not equal to 1
C   Outputs:
C      FVEC(MAXPTS) D   Model-Data for each grid point used
C      FJAC(24)     D   The function partials
C-----------------------------------------------------------------------
      INCLUDE 'IMFIT.INC'
      INTEGER   INN, INVAR, IFLAG
      DOUBLE PRECISION    VALVAR(24), FVEC(*), FJAC(24)
C
      INTEGER   NVAR, K, II, I, J
      REAL      FUNC(MAXPTS)
      INTEGER   X1, Y1, NK, KK
      SAVE X1, Y1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (INN.GT.NPTS) THEN
         MSGTXT = 'MORE POINTS IN CALL TO FUNCTION THAN WE HAVE'
         CALL MSGWRT (6)
         END IF
      NVAR = INVAR
C                                       Test on IFLAG
      IF (IFLAG.LE.1) THEN
C                                        X pixel and Y pixel range
         X1 = WIN(1) + 0.5
         Y1 = WIN(2) + 0.5
C                                        Put data function array
         NK = 0
         DO 10 K = 1,NPTS
            IF (DATA(K).NE.FBLANK) THEN
               NK = NK + 1
               FVEC(NK) = -DATA(K)
               END IF
 10         CONTINUE
C                                        Fill in new variable values
         DO 20 II = 1,NVAR
            G(JVAR(II),IVAR(II)) = VALVAR(II)
 20         CONTINUE
C                                        Loop over components
         DO 40 I = 1,NGAUSS
C                                        Evaluate function
C
            CALL IMFMOD (XTYPE(I), I, I, 0, G(1,I), E(1,I), X1, Y1,
     *         NPARM, NX, NY, FUNC, FJAC(1))
C                                        Sum up solution
            NK = 0
            DO 30 K = 1,NPTS
               IF (DATA(K).NE.FBLANK) THEN
                  NK = NK + 1
                  FVEC(NK) = FUNC(K) + FVEC(NK)
                  END IF
 30            CONTINUE
 40         CONTINUE
         IF (IFLAG.EQ.0) GO TO 999
C                                        Terminate due to conv.
         IF (NPARM.GE.15) THEN
            CALL IMFCHK (NVAR, JVAR, IVAR, VALVAR, XTYPE, WIN, DMAX,
     *         DMIN, IFLAG)
            IF (IFLAG.LE.-10) THEN
               NITER = NPARM
               GO TO 999
               END IF
            END IF
C                                        Terminate due to NITER?
         IF (NPARM.GE.NITER) IFLAG = -9
C                                       Calculate the Jacobian
C                                       Get data point
      ELSE
         NK = IFLAG - 1
         KK = 0
         DO 105 I = 1,NPTS
            IF (DATA(I).NE.FBLANK) THEN
               KK = KK + 1
               IF (KK.GE.NK) THEN
                  K = I
                  GO TO 108
                  END IF
               END IF
 105        CONTINUE
C                                       Loop over variables
 108     DO 110 II = 1,NVAR
            I = IVAR(II)
            J = JVAR(II)
C                                             Calculate Jacobian
            CALL IMFMOD (XTYPE(I), I, J, K, G(1,I), E(1,I), X1, Y1,
     *         NPARM, NX, NY, FUNC, FJAC(II))
 110        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE IMFMOD (TYPE, NC, JV, IC, G, E, X1, Y1, NPARM, NX, NY,
     *    FUNC, FJAC)
C-----------------------------------------------------------------------
C   IMFMOD is a subroutine of IMFIT.  This subroutine is called by FCN
C   and supplied the value of the function at all data points and the
C   value of the Jacobian at each data point.  It can be generalized to
C   include other function types.
C   Inputs:
C      TYPE            I     The component type
C                              1=Gaussian, 2=Baseline
C      NC              I     The component number
C      JV              I     The variable number of the component
C      IC              I     Type of calculation
C                              0=Calculate function at all points
C                              N>0=Calculate Jacobian for Nth point
C      G(6)            R     Parameter values
C      E(6)            R     Parameter errors (0=hold fixed)
C      X1              I     Smallest X pixel
C      Y1              I     Smallest Y pixel
C      NPARM           I     Iteration number
C      NX              I     Number of X pixels
C      NY              I     Number of Y pixels
C   Outputs:
C      FUNC(MAXPTS)    R     The function value at all points
C      FJAC            D     The Jacobian for (IC-1) point
C-----------------------------------------------------------------------
      INTEGER   TYPE, NC, JV, IC, X1, Y1, NX, NY, NPARM
      REAL      G(6), E(6), FUNC(*), X2, Y2, X0, Y0
      DOUBLE PRECISION    FJAC
      INTEGER   NPTS, KP, IP, K
      REAL      CON, X, Y
C                                        Working arrays
      INCLUDE 'IMFPTS.INC'
      REAL      SINTH2(4), COSTH2(4), SIN2TH(4), COS2TH(4)
      REAL      VA(4), VB(4), VC(4), VD(4), MJ(4), MN(4)
      REAL      F(MAXPTS,4)
      REAL      SINP, COSP, SINP2, COSP2, SIN2P, SINT, COST
      REAL      EL, ELC, CURV, SL, ZERO
      SAVE SINTH2, COSTH2, SIN2TH, COS2TH, VA, VB, VC, VD, MJ, MN,
     *   F, SINP, COSP, SINP2, COSP2, SIN2P, SINT, COST,
     *   EL, ELC, CURV, SL, ZERO, X2, Y2, X0, Y0, con, x, y, NPTS, KP,
     *   IP, K
      INCLUDE 'INCS:DMSG.INC'
      DATA CON /2.772589/
C-----------------------------------------------------------------------
C                                        Gaussian
      IF (TYPE.EQ.1) THEN
C              G(1) = Peak of Gaussian component
C              G(2) = X-pixel location of peak
C              G(3) = Y-pixel location of peak
C              G(4) = Full width-half intensity of major axis in pixels
C              G(5) = Full width-half intensity of minor axis in pixels
C              G(6) = Position angle of major axis in radians
C                     Measured from vertical, increasing CCW
C                                        Function or Jacobian
         IF (IC.LE.0) THEN
C                                        Function
C                                        First time through?
            IF (NPARM.LT.NC) THEN
C                                        Initialize fixed constants
               NPTS = NX * NY
               IF (E(6).LT.0.0) THEN
                  SINTH2(NC) = SIN(G(6))**2
                  COSTH2(NC) = COS(G(6))**2
                  SIN2TH(NC) = -SIN(2.0*G(6))
                  COS2TH(NC) = COS(2.0*G(6))
                  END IF
               IF (E(4).LT.0.0) MJ(NC) = G(4)*G(4)/CON
               IF (E(5).LT.0.0) MN(NC) = G(5)*G(5)/CON
               IF (E(4).LT.0.0 .AND. E(5).LT.0.0 .AND. E(6).GE.0.0)
     *            THEN
                  VA(NC) = COSTH2(NC)/MJ(NC) + SINTH2(NC)/MN(NC)
                  VB(NC) = SINTH2(NC)/MJ(NC) + COSTH2(NC)/MN(NC)
                  VC(NC) = SIN2TH(NC) * (1.0/MN(NC) - 1.0/MJ(NC))
                  VD(NC) = COS2TH(NC) * (1.0/MN(NC) - 1.0/MJ(NC))
                  END IF
               END IF
            NPARM = NPARM + 1
C                                        Initialize variable constants
            IF (E(6).GE.0.0) THEN
               SINTH2(NC) = SIN(G(6))**2
               COSTH2(NC) = COS(G(6))**2
               SIN2TH(NC) = -SIN(2.0*G(6))
               COS2TH(NC) = COS(2.0*G(6))
               END IF
            IF (E(4).GE.0.0) MJ(NC) = G(4)*G(4)/CON
            IF (E(5).GE.0.0) MN(NC) = G(5)*G(5)/CON
            IF (E(4).GE.0.0 .OR. (E(5).GE.0.0) .OR. (E(6).LT.0.0))
     *         THEN
               VA(NC) = COSTH2(NC)/MJ(NC) + SINTH2(NC)/MN(NC)
               VB(NC) = SINTH2(NC)/MJ(NC) + COSTH2(NC)/MN(NC)
               VC(NC) = SIN2TH(NC) * (1.0/MN(NC) - 1.0/MJ(NC))
               VD(NC) = COS2TH(NC) * (1.0/MN(NC) - 1.0/MJ(NC))
               END IF
C                                        Cycle through grid points
            DO 100 K = 1,NPTS
               X = X1 + MOD(K-1,NX) - G(2)
               Y = Y1 + AINT((K-0.5)/NX) - G(3)
               FUNC(K) = G(1) * EXP(-(VA(NC) * X**2 +
     *            VB(NC) * Y**2 + VC(NC) * X * Y))
C                                        Store value for later use
               F(K,NC) = FUNC(K)
 100           CONTINUE
C                                        Set counter for Jacobian
            KP = 0
            IP = 0
C                                        Calculate Jacobian
      ELSE
C                                        Data point
         K = IC
C                                        New data point of component?
         IF ((K.NE.KP) .OR. (IP.NE.NC)) THEN
            KP = K
            IP = NC
            X = X1 + MOD (K-1, NX) - G(2)
            Y = Y1 + AINT ((K-0.5)/NX) - G(3)
            X2 = X * X
            Y2 = Y * Y
            END IF
C                                        Amplitude
         IF (JV.EQ.1) THEN
            IF (G(1).EQ.0.0) G(1)=1.0E-10
            FJAC = F(K,NC) / G(1)
C                                        X-position
         ELSE IF (JV.EQ.2) THEN
            FJAC = F(K,NC) * (2.0 * X * VA(NC) + Y * VC(NC))
C                                        Y-position
         ELSE IF (JV.EQ.3) THEN
            FJAC = F(K,NC) * (2.0 * Y * VB(NC) + X * VC(NC))
C                                        Major axis
         ELSE IF (JV.EQ.4) THEN
            FJAC = 2.0 * CON * F(K,NC) / G(4)**3 *
     *         (X2 * COSTH2(NC) + Y2 * SINTH2(NC) - X * Y * SIN2TH(NC))
C                                        Minor axis
         ELSE IF (JV.EQ.5) THEN
            FJAC = 2.0 * CON * F(K,NC) / G(5)**3 *
     *         (X2 * SINTH2(NC) + Y2 * COSTH2(NC) + X * Y * SIN2TH(NC))
C                                        Position angle
         ELSE IF (JV.EQ.6) THEN
            FJAC = F(K,NC) * (VC(NC) * (X2 - Y2) + VD(NC) * 2.0 * X * Y)
            END IF
         END IF
C                         General baseline
      ELSE IF (TYPE.EQ.2) THEN
C              G(1) = Zero level offset
C              G(2) = Baseline slope in ampl/pixel
C              G(3) = Orientation of slope in radians
C              G(4) = Major axis curvature in ampl**2/pixel
C              G(5) = Curvature eccentricity
C              G(6) = Orientation of major axis curvature
C                                        Function or Jacobian
         IF (IC.LE.0) THEN
C                                        Function
C                                        First time through?
            IF (NPARM.LT.NC) THEN
C                                       Initialize fixed constants
               NPTS = NX * NY
               X0 = X1 + (NX-1) / 2.0
               Y0 = Y1 + (NY-1) / 2.0
               IF (E(6).LT.0) THEN
                  SINP = SIN(G(6))
                  COSP = COS(G(6))
                  SINP2 = SINP * SINP
                  COSP2 = COSP * COSP
                  SIN2P = 2.0 * SINP * COSP
                  END IF
               IF (E(5).LT.0.0) THEN
                  EL = G(5)
                  ELC = 1 - EL
                  END IF
               IF (E(4).LT.0.0) CURV = G(4)
               IF (E(3).LT.0.0) THEN
                  SINT = SIN(G(3))
                  COST = COS(G(3))
                  END IF
               IF (E(2).LT.0.0) SL = G(2)
               IF (E(1).LT.0.0) ZERO = G(1)
               END IF
            NPARM = NPARM + 1
C                                        Initialize variables
            IF (E(6).GE.0) THEN
               SINP = SIN(G(6))
               COSP = COS(G(6))
               SINP2 = SINP * SINP
               COSP2 = COSP * COSP
               SIN2P = 2.0 * SINP * COSP
               END IF
            IF (E(5).GE.0.0) THEN
               EL = G(5)
               ELC = 1 - EL
               END IF
            IF (E(4).GE.0.0) CURV = G(4)
            IF (E(3).GE.0.0) THEN
               SINT = SIN(G(3))
               COST = COS(G(3))
               END IF
            IF (E(2).GE.0.0) SL = G(2)
            IF (E(1).GE.0.0) ZERO = G(1)
C                                        Evaluate function
            DO 340 K = 1,NPTS
               X = X1 + MOD(K-1,NX) - X0
               Y = Y1 + AINT((K-0.5)/NX) - Y0
               F(K,NC) = X * X * (COSP2 + EL * SINP2) +
     *            Y * Y * (SINP2 + EL * COSP2) +
     *            X * Y * 2.0 * SIN2P * ELC
               FUNC(K) = ZERO + SL * (X*COST + Y*SINT) + CURV * F(K,NC)
 340           CONTINUE
C                                        Set counter for Jacobian
            KP = 0
            IP = 0
C                                        Calculate Jacobian
C                                        Data point
         ELSE
            K = IC
C                                        New data point of component?
            IF ((K.NE.KP) .OR. (IP.NE.NC)) THEN
               KP = K
               IP = NC
               X = X1 + MOD (K-1,NX) - X0
               Y = Y1 + AINT ((K-0.5)/NX) - Y0
               END IF
C                                        Go to correct variable type
            IF (JV.EQ.1) THEN
               FJAC = 1.0
            ELSE IF (JV.EQ.2) THEN
               FJAC = X * COST + Y * SINT
            ELSE IF (JV.EQ.3) THEN
               FJAC = SL * (Y * COST - X * SINT)
            ELSE IF (JV.EQ.4) THEN
               FJAC = F(K,NC)
            ELSE IF (JV.EQ.5) THEN
               FJAC = CURV * (X*X * SINP2 + Y*Y * COSP2 - 2.0 * X * Y *
     *            SIN2P)
            ELSE IF (JV.EQ.6) THEN
               FJAC = CURV*ELC * (SIN2P*(Y*Y - X*X) + (COSP2 -
     *            SINP2)*4.0*X*Y)
               END IF
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE IMFCHK (NVAR, JVAR, IVAR, VALVAR, XTYPE, WIN, DMAX,
     *   DMIN, IFLAG)
C-----------------------------------------------------------------------
C   Subroutine IMFCHK, a subroutine for IMFIT, compared two consecutive
C   solutions and checks if the difference between all of the fitted
C   parameters have changed insignificantly.
C   Inputs:
C      NVAR        I       The number of variables
C      JVAR(24)    I       The variable type
C      IVAR(24)    I       The variable component
C      VALVAR(24)  D       The variable value
C      XTYPE(4)    I       The component type
C      WIN(4)      R       The fitting window
C      DMAX        R       The data maximum
C      DMIN        R       The data minimum
C   Output:
C      IFLAG       I       1-> Solution still changing
C                          -10 -> Solution has converged
C-----------------------------------------------------------------------
      INTEGER   NVAR, JVAR(24), IVAR(24), XTYPE(4), IFLAG
      REAL      WIN(4), DMAX, DMIN
      DOUBLE PRECISION    VALVAR(24)

      INTEGER   IBTEMP, IB2T, NCOUNT, I, JV(24), IV(24), II, PNVAR
      REAL      TEMP
      DOUBLE PRECISION    VAL(2,24)
      SAVE TEMP, JV, IV, PNVAR, VAL, NCOUNT
      INCLUDE 'INCS:DMSG.INC'
      DATA NCOUNT, PNVAR /0,0/
C-----------------------------------------------------------------------
      IF (NVAR.NE.PNVAR) NCOUNT = 0
      PNVAR = NVAR
      NCOUNT = NCOUNT + 1
C                                        First time through
      IF (NCOUNT.LE.1) THEN
         TEMP = MAX(DMAX, -DMIN)
         DO 20 I = 1,NVAR
            JV(I) = JVAR(I)
            IV(I) = IVAR(I)
            VAL(2,I)= VALVAR(I)
 20         CONTINUE
C                                        Store next set of values
      ELSE
         DO 40 I = 1,NVAR
            II = MOD(NCOUNT,2) + 1
            VAL(II,I) = VALVAR(I)
 40         CONTINUE
C                                        Make comparisons
         DO 200 I = 1,NVAR
            IBTEMP = XTYPE(IV(I))
            IB2T = JV(I)
            IF (IBTEMP.NE.2) THEN
               IF (IB2T.LE.1) THEN
                  IF (ABS((VAL(1,I)-VAL(2,I)) / TEMP).GT.0.0001D0)
     *               GO TO 210
               ELSE IF (IB2T.NE.6) THEN
                  IF (ABS(VAL(1,I)-VAL(2,I)).GT.0.001D0) GO TO 210
               ELSE
                  IF (ABS(VAL(1,I)-VAL(2,I)).GT.0.0001D0) GO TO 210
                  END IF
            ELSE
               IF ((IB2T.LE.2) .OR. (IB2T.EQ.4)) THEN
                  IF (ABS((VAL(1,I)-VAL(2,I)) / TEMP).GT.0.0001D0)
     *               GO TO 210
               ELSE IF (IB2T.EQ.5) THEN
                  IF (ABS(VAL(1,I)-VAL(2,I)).GT.0.001D0) GO TO 210
               ELSE
                  IF (ABS(VAL(1,I)-VAL(2,I)).GT.0.0001D0) GO TO 210
                  END IF
               END IF
 200        CONTINUE
C                                        Nothing much has changed
         IFLAG = -10
C                                        Check on ridiculous val.
 210     IF (MOD(NCOUNT,10).NE.9) THEN
            DO 300 I = 1,NVAR
               IBTEMP = XTYPE(IV(I))
               IF (IBTEMP.EQ.1) THEN
                  IB2T = JV(I)
C                                        Gaussian
                  IF (IB2T.EQ.1) THEN
                     IF (ABS(VAL(1,I)).GT.5.0*TEMP) GO TO 310
                  ELSE IF (IB2T.EQ.2) THEN
                     IF (VAL(1,I).LE.(WIN(1)-10.0D0) .OR.
     *                  (VAL(1,I).GE.(WIN(3)+10.0D0))) GO TO 310
                  ELSE IF (IB2T.EQ.3) THEN
                     IF (VAL(1,I).LE.(WIN(2)-10.0D0) .OR.
     *                  (VAL(1,I).GE.(WIN(4)+10.0D0))) GO TO 310
                  ELSE IF (IB2T.EQ.4) THEN
                     IF (VAL(1,I).GE.30.0D0) GO TO 310
                  ELSE IF (IB2T.EQ.5) THEN
                     IF (VAL(1,I).GE.30.0D0) GO TO 310
                     END IF
                  END IF
 300           CONTINUE
            GO TO 999
C                                        Bad value
 310        IFLAG = -11
            WRITE (MSGTXT,1310) IV(I), JV(I),VAL(1,I)
            CALL MSGWRT (4)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1310 FORMAT ('STRANGE VALUE FOR COMP=',I2,' PARAM=',I2,' VALUE=',
     *   1PE11.3)
      END
      SUBROUTINE IMFMSG (INFO, MITER)
C-----------------------------------------------------------------------
C   Subroutine IMFMSG writes out the termination messages from LMDER1,
C   the basic fitting routine in IMFIT.
C   INPUTS:
C      INFO     I      Number from LMDER1
C      MITER    I      Number of iterations
C-----------------------------------------------------------------------
      INTEGER   INFO, I, MITER
      INCLUDE 'IMFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      I = INFO + 1
      IF (I.EQ.-8) I = 9
      IF (I.EQ.-9) I = 10
      IF (I.EQ.-10) I = 11
      IF (I.EQ.1) THEN
         WRITE (MSGTXT,1010) MITER
      ELSE IF (I.EQ.2) THEN
         WRITE (MSGTXT,1020) MITER
      ELSE IF (I.EQ.3) THEN
         WRITE (MSGTXT,1030) MITER
      ELSE IF (I.EQ.4) THEN
         WRITE (MSGTXT,1040) MITER
      ELSE IF (I.EQ.5) THEN
         WRITE (MSGTXT,1050) MITER
      ELSE IF (I.EQ.6) THEN
         WRITE (MSGTXT,1060) MITER
      ELSE IF (I.EQ.7) THEN
         WRITE (MSGTXT,1070) MITER
      ELSE IF (I.EQ.8) THEN
         WRITE (MSGTXT,1080) MITER
      ELSE IF (I.EQ.9) THEN
         WRITE (MSGTXT,1090) MITER
      ELSE IF (I.EQ.10) THEN
         WRITE (MSGTXT,1100) MITER
      ELSE IF (I.EQ.11) THEN
         WRITE (MSGTXT,1110) MITER
      ELSE
         WRITE (MSGTXT,1950) MITER
         END IF
      CALL DOPRT
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('IMPROPER INPUT PARAMETERS.  NITER=',I5)
 1020 FORMAT ('Sum of squares is less than tolerance.  niter=',I5)
 1030 FORMAT ('Solution errors is less than tolerance.  niter=',I5)
 1040 FORMAT ('RMS of fit and errors less than tolerance.  niter=',I5)
 1050 FORMAT ('SOLUTION IS SINGULAR; CHECK PARAMETERS.  NITER=',I5)
 1060 FORMAT ('Reached iteration limit in algorithm spec.  niter=',I5)
 1070 FORMAT ('Tolerance is too small-no further reduction.  niter=',
     *   I5)
 1080 FORMAT ('Tolerance is too small for better solution.  niter=',I5)
 1090 FORMAT ('Terminated by niter=',I5)
 1100 FORMAT ('Solution unchanging.  niter=',I5)
 1110 FORMAT ('RIDICULOUS VALUE FOR SOME PARAMETER AT NITER=',I5)
 1950 FORMAT ('INPROPER ERROR CONDITION.  I=',I8)
      END
      SUBROUTINE IMFERR (BUFF, NN, NVAR, VALVAR, FVEC, FJAC, IER)
C-----------------------------------------------------------------------
C   IMFERR is a subroutine of IMFIT which determines the parameter
C   errors for the fit.
C   INPUTS:
C      BUFF     R(*)       Original data
C      NVAR     I          Number of independent variables
C      NN       I          Number of data points
C      VALVAR   D(24)      The parameter values
C      FVEC     D(*)       The residual data
C      FJAC     D(24,24)   Jacobian array for FCN
C   OUTPUTS:
C      IER      I          Error return  0-> okay
C                              1-> error
C-----------------------------------------------------------------------
      REAL      BUFF(*)
      INTEGER   NN, NVAR, IER
      DOUBLE PRECISION FJAC(24,24), FVEC(*), VALVAR(24)
C
      INTEGER   IERR, IBTEMP, INN, INVAR, NLO, NHI, NK, I, II, J, K1,
     *   K2, K3, K4
      REAL      SUM2, RMS, CBAREA, TEMP, RM(3), DIFF, ERR, CURV, NIND,
     *   NW(4), CNST
      INCLUDE 'IMFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                        Initialization
      IER = 0
      IF (NVAR.LE.0) THEN
         WRITE (MSGTXT,1000)
         CALL DOPRT
         GO TO 615
         END IF
      CNST = 3.14159265 / 180.0
      SUM2 = 0.0
      NIND = 1.0
      CBAREA = CB(1) * CB(2)
      IF (CBAREA.LT.0.5) CBAREA = ABS (G(4,1)*G(5,1))
      IF (CBAREA.GT.25.) CBAREA = 25.0
C                                        Find RMS residual
      NK = 0
      DO 20 I = 1,NPTS
         IF (DATA(I).NE.FBLANK) THEN
            NK = NK + 1
            SUM2 = SUM2 + DATA(I)*DATA(I)
            END IF
 20      CONTINUE
      IF (NK.LT.2) THEN
         WRITE (MSGTXT,1020) NK
         GO TO 900
         END IF
      RMS = SQRT (SUM2 / NK)
C                                        Check scale factor
      WRITE (MSGTXT,1030) RMS
      CALL DOPRT
      WRITE (MSGTXT,1034)
      CALL DOPRT
C                                        Get approximate
C                                        component errors
      TEMP = NVAR
C      RMS = RMS * SQRT(TEMP)
      CALL RFILL (24, 0.0, E)
      DO 400 II = 1,NVAR
         I = IVAR(II)
         J = JVAR(II)
         IBTEMP = XTYPE(I)
C                                        Gaussian fit
         IF (IBTEMP.EQ.1) THEN
            TEMP = ABS (G(1,I))
            IF (J.EQ.1) THEN
               E(1,I) = RMS * 2.0
            ELSE IF (J.EQ.2) THEN
               E(2,I) = RMS / TEMP / 1.5 * G(4,I)
               IF (E(2,I).GT.G(4,I)) E(2,I) = G(4,I)
            ELSE IF (J.EQ.3) THEN
               E(3,I) = RMS / TEMP / 1.5 * G(4,I)
               IF (E(3,I).GT.G(4,I)) E(3,I) = G(4,I)
            ELSE IF (J.EQ.4) THEN
               E(4,I) = RMS / TEMP / 1.5 * G(4,I)
               IF (E(4,I).GT.G(4,I)) E(4,I) = G(4,I)
            ELSE IF (J.EQ.5) THEN
               E(5,I) = RMS / TEMP / 1.5 * G(5,I)
               IF (E(5,I).GT.G(5,I)) E(5,I) = G(5,I)
            ELSE IF (J.EQ.6) THEN
               E(6,I) = (E(4,I)/G(4,I) + E(5,I)/G(5,I))
               IF (E(6,I).GT.1.5) E(6,I) = 1.5
               END IF
C                                        Background fit
         ELSE IF (IBTEMP.EQ.2) THEN
            IF (J.EQ.1) THEN
               E(1,I) = RMS
            ELSE IF (J.EQ.2) THEN
               E(2,I) = RMS
            ELSE IF (J.EQ.3) THEN
               E(3,I) = RMS / ABS (G(2,I))
               IF (E(3,I).GT.0.7)  E(3,I) = 0.7
            ELSE IF (J.EQ.4) THEN
               E(4,I) = RMS
            ELSE IF (J.EQ.5) THEN
               E(5,I) = RMS / ABS (G(4,I))
               IF (E(5,I).GT.1.0) E(5,I) = 1.0
            ELSE IF (J.EQ.6) THEN
               E(6,I) = RMS / ABS (G(4,I))
               IF (E(6,I).GT.0.7) E(6,I) = 0.7
               END IF
            END IF
 400     CONTINUE
C                                        Store original data
      DO 410 K1 = 1,NPTS
         DATA(K1) = BUFF(K1)
 410     CONTINUE
C                                        Get more accurate errors
      DO 600 II = 1,NVAR
         I = IVAR(II)
         J = JVAR(II)
C                                        Set variable value for loop
 420     VALVAR(II) = VALVAR(II) - 2*E(J,I)
C                                        Find relevant area
C                                        Gaussian
         IF (XTYPE(I).EQ.1) THEN
            TEMP = 0.75 * MAX (ABS (G(4,I)), ABS (G(5,I)))
            IF (TEMP.GT.100.0) TEMP = 100.0
            NW(1) = G(2,I) - TEMP
            NW(2) = G(3,I) - TEMP
            NW(3) = G(2,I) + TEMP
            NW(4) = G(3,I) + TEMP
            IF (NW(1).LT.WIN(1)) NW(1) = WIN(1)
            IF (NW(2).LT.WIN(2)) NW(2) = WIN(2)
            IF (NW(3).GT.WIN(3)) NW(3) = WIN(3)
            IF (NW(4).GT.WIN(4)) NW(4) = WIN(4)
            NLO = 1 + NX*(NW(2) - WIN(2)) + NW(1) - WIN(1)
            NHI = NPTS - NX*(WIN(4) - NW(4)) - NW(3) + WIN(3)
C                                        Baseline
         ELSE
            NLO = 1
            NHI = NPTS
            END IF
C                                        Find RMS
         IF (NIND.LT.1.0) NIND = 1.0
         K4 = 0
         DO 490 K2 = 1,3
C                                        Change values
            VALVAR(II) = VALVAR(II) + E(J,I)
            INN = NN
            INVAR = NVAR
            CALL FCN (INN, INVAR, VALVAR, FVEC, FJAC, 0)
            SUM2 = 0.
            NK = 0.
C                                        Find the RMS
            DO 480 K3 = 1,NPTS
               IF (DATA(K3).NE.FBLANK) THEN
                  NK = NK + 1
                  IF ((K3.GE.NLO) .AND. (K3.LE.NHI)) SUM2 = SUM2 +
     *               FVEC(NK)*FVEC(NK)
                  END IF
 480           CONTINUE
            IF (NK.LE.1) THEN
               WRITE (MSGTXT,1020) NK
               GO TO 900
               END IF
            RM(K2) = SQRT (SUM2 / NK)
 490        CONTINUE
         VALVAR(II) = VALVAR(II) - E(J,I)
C                                        Find RMS well
         CALL IMFSOL (RM, DIFF, ERR, CURV, IERR)
         IF (IERR.NE.0) THEN
            DIFF = 0.0
            ERR = 0.0
            CURV = 1.0E-10
            WRITE (MSGTXT,1490) I, J
            CALL DOPRT
            GO TO 500
            END IF
C                                        Get value at RMS minimum
         IF (DIFF.GT.1.5) DIFF = 1.50
         IF (DIFF.LT.-1.5) DIFF = -1.50
         VALVAR(II) = VALVAR(II) + DIFF * E(J,I)
C                                        Too far from minimum
         K4 = K4 + 1
         IF (K4.GE.5) THEN
            DIFF = 0.0
            ERR = 0.0
            CURV = 1.0E-10
            WRITE (MSGTXT,1490) I, J
            CALL DOPRT
            GO TO 500
            END IF
         IF (ABS(DIFF).GE.1.50) GO TO 420
C                                        What is error?
 500     NIND = 4.0 * NK / CBAREA
         DIFF = ERR / NIND
         E(J,I) = SQRT (DIFF/CURV) * E(J,I)
 600     CONTINUE
C                                        Get residual data
      DO 610 I=1,NPTS
         DATA(I) = BUFF(I)
 610     CONTINUE
 615  INN = NN
      INVAR = NVAR
      CALL FCN (INN, INVAR, VALVAR, FVEC, FJAC, 0)
      NK = 0
      SUM2 = 0.0
      DO 620 I = 1,NPTS
         IF (DATA(I).EQ.FBLANK) GO TO 620
            NK = NK+1
            SUM2 = SUM2 + FVEC(NK) * FVEC(NK)
            DATA(I) = -FVEC(NK)
 620     CONTINUE
C                                        Final RMS
      ERR = SQRT (SUM2 / NK)
      WRITE (MSGTXT,1600) ERR
      CALL DOPRT
      GO TO 999
C                                        Error return
 900  IER = 1
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NO PARAMETER TO FIT.  WILL SUBTRACT MODEL FROM DATA')
 1020 FORMAT ('TOO FEW POINTS FOR RMS.  #=',I3)
 1030 FORMAT ('******  RMS of fit=',1PE12.4,' per point  ******')
 1034 FORMAT ('Begin tweaking each parameter')
 1490 FORMAT ('ERROR FOR COMP',I2,' VARIABLE',I2,' UNDEFINED')
 1600 FORMAT ('******  New RMS of fit=',1PE12.4,' per point  ******')
      END
      SUBROUTINE IMFSTO (IER)
C-----------------------------------------------------------------------
C   IMFSTO, a subroutine of IMFIT, creates, opens and stores the
C   residual map after fitting.
C   Outputs:
C      IER             I        Error return  0-> okay
C                                  1-> error
C-----------------------------------------------------------------------
      HOLLERITH MA, HOH(256)
      INTEGER   IER, IERR, J, I1, I
      REAL      HOR(256), MAX, MIN, RRMAX, RRMIN
      DOUBLE PRECISION    HOD(128)
      INCLUDE 'IMFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (HOI, HOR, HOD, HOH)
C-----------------------------------------------------------------------
C                                       Initialize
      IER = 0
      CALL COPY (256, CATBLK, HOI)
C                                       Fill in outname
      CALL CHR2H (4, 'MA  ', 1, MA)
      CALL H2WAWA (XON, XOC, XOS, MA, XOD, XU, OUTNA)
C                                       Fill in header values
      HOI(KINAX) = NX
      HOI(KINAX+1) = NY
      HOI(KINAX+2) = 1
      HOI(KINAX+3) = 1
      HOI(KINAX+4) = 1
      HOI(KINAX+5) = 1
      HOI(KINAX+6) = 1
      HOR(KRCRP) = CATR(KRCRP) - WIN(1) + 1.0
      HOR(KRCRP+1) = CATR(KRCRP+1) - WIN(2) + 1.0
      HOR(KRCRP+2) = CATR(KRCRP+2) - DEPTH(1) + 1.0
      HOR(KRCRP+3) = CATR(KRCRP+3) - DEPTH(2) + 1.0
      HOR(KRCRP+4) = CATR(KRCRP+4) - DEPTH(3) + 1.0
      HOR(KRCRP+5) = CATR(KRCRP+5) - DEPTH(4) + 1.0
      HOR(KRCRP+6) = CATR(KRCRP+6) - DEPTH(5) + 1.0
      CALL CATCLR (HOI)
C                                       Create map
      CALL MAPCR (INNA, OUTNA, HOI, IERR)
      IF (IERR.EQ.0) GO TO 60
         WRITE (MSGTXT,1050) IERR
         GO TO 900
C                                       Open map
 60   CALL OPENCF (LUN1, OUTNA, IERR)
      IF (IERR.EQ.0) GO TO 70
         WRITE (MSGTXT,1060) IERR
         GO TO 900
C                                       Write data line by line
 70   RRMAX = -1.E20
      RRMIN = -RRMAX
      DO 80 J = 1,NY
         I1 = (J-1) * NX
         DO 75 I = 1,NX
            I1 = I1 + 1
            IF (DATA(I1).EQ.FBLANK) GO TO 75
               RRMAX = MAX (RRMAX, DATA(I1))
               RRMIN = MIN (RRMIN, DATA(I1))
 75         CONTINUE
         I1 = (J-1) * NX + 1
         CALL MAPIO ('WRIT', LUN1, DATA(I1), IERR)
         IF (IERR.EQ.0) GO TO 80
            WRITE (MSGTXT,1070) J, IERR
            GO TO 900
 80      CONTINUE
C                                        Get HI files
      OUTSL = FILTAB(POCAT,6)
      XOD = FILTAB(POVOL,6)
      CALL H2WAWA (XON, XOC, XOS, MA, XOD, XU, OUTNA)
      HOR(KRDMX) = RRMAX
      HOR(KRDMN) = RRMIN
      CALL FILCLS (LUN1)
      CALL IMFHI (IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1080) IERR
         CALL MSGWRT (8)
         GO TO 999
C                                       Error return
 900  IER = 1
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('IMFSTO: COULD NOT CREATE MAP.  IER=',I7)
 1060 FORMAT ('IMFSTO: COULD NOT OPEN MAP.  IER=',I7)
 1070 FORMAT ('IMFSTO: WRITE ERROR LINE ',I6,'  IER=',I7)
 1080 FORMAT ('IMFSTO: HI FILE GENERATION ERROR.  IER=',I7)
      END
      SUBROUTINE IMFSOL (RM, DIFF, ERR, CURV, IER)
C-----------------------------------------------------------------------
C   IMFSOL calculates the best first parabola through three points of
C   value RM(1), RM(2), and RM(3); located at -1, 0 and +1.  DIFF is
C   the location of the minimum; ERR is the values at the minimum; CURV
C   is the curvature (rms units/unit) of the fit.  This subroutine is
C   used with IMFERR and IMFIT.
C   INPUTS:
C      RM(3)         R     The input triad of points
C   OUTPUTS:
C      DIFF          R     Location of minimum
C      ERR           R     Value of RM at minimum
C      CURV          R     The curvature
C      IER           I     Error return
C-----------------------------------------------------------------------
      INTEGER   IER
      REAL   RM(3), DIFF, ERR, CURV
      REAL   TEMP
C-----------------------------------------------------------------------
      IER = 0
      TEMP = RM(1) - 2.0*RM(2) + RM(3)
      IF (TEMP.LE.0.0) GO TO 950
         DIFF = 0.5 * (RM(1) - RM(3)) / TEMP
         ERR = RM(2) - 0.125*(RM(1) - RM(3))**2 / TEMP
         CURV = 0.5 * TEMP
         GO TO 999
C                                        Error return
 950  IER = 1
C
 999  RETURN
      END
      SUBROUTINE IMFHI (IER)
C-----------------------------------------------------------------------
C   IMFHI creates and writes the HI file for the task IMFIT.
C   Outputs:
C      IER            I     Error return  0->okay
C                             1->uh-oh
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, HILINE*72, MODEL(3)*8, CNAME*12, CCLAS*6,
     *   CTYP*2, ONAME*12, OCLAS*6
      INTEGER   IER, IERR, NHISTF, LHIN, LHOUT, IVOL, OVOL, IBTEMP
      INTEGER   IBUFF1(256), IBUFF2(256), INSEQ, OSEQ, CUSEID,
     *   ITEMP(7), I, J
      LOGICAL   T
      INCLUDE 'IMFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA NHISTF, LHIN, LHOUT /2,27,28/
      DATA T /.TRUE./
      DATA PRGNAM /'IMFIT '/
      DATA MODEL /'Gaussian','Baseline','????????'/
C-----------------------------------------------------------------------
C                                       Initialize HI
      CALL HIINIT (NHISTF)
      IER = 0
C                                       Create and open output HI file
      CALL WAWA2A (OUTNA, ONAME, OCLAS, OSEQ, CTYP, OVOL, CUSEID)
      CALL WAWA2A (INNA, CNAME, CCLAS, INSEQ, CTYP, IVOL, CUSEID)
C                                       copy keywords
      CALL KEYPCP (IVOL, INSL, OVOL, OUTSL, 0, ' ', IERR)
C                                       copy HI
      CALL HISCOP (LHIN, LHOUT, IVOL, OVOL, INSL, OUTSL, HOI,
     *   IBUFF1, IBUFF2, IERR)
      IF (IERR.LE.2) GO TO 50
         WRITE (MSGTXT,1000) IERR
         GO TO 900
C                                       Add new HI entries
C                                       Input name
 50   CALL HENCO1 (PRGNAM, CNAME, CCLAS, INSEQ, IVOL, LHOUT,
     *   IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Output name
      CALL HENCOO (PRGNAM, ONAME, OCLAS, OSEQ, OVOL, LHOUT,
     *   IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 900
C                                        Rest of inputs
      DO 60 I = 1,7
         ITEMP(I) = XBL(I) + 0.5
 60      CONTINUE
      WRITE (HILINE,1060) PRGNAM, ITEMP
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      DO 70 I = 1,7
         ITEMP(I) = XTR(I) + 0.5
 70      CONTINUE
      WRITE (HILINE,1070) PRGNAM, ITEMP
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      WRITE (HILINE,1072) PRGNAM, NGAUSS, (XTYPE(I), I = 1,NGAUSS)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      WRITE (HILINE,1074) PRGNAM, (GM(I), I = 1,NGAUSS)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      WRITE (HILINE,1076) PRGNAM, ((GP(J,I), J = 1,2),
     *   I = 1,NGAUSS)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      WRITE (HILINE,1078) PRGNAM, ((GW(J,I), J = 1,3),
     *   I = 1,NGAUSS)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      WRITE (HILINE,1080) PRGNAM, NITER
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      WRITE (HILINE,1082) PRGNAM, (XDM(I), I = 1,NGAUSS)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      WRITE (HILINE,1084) PRGNAM, ((XDP(J,I), J = 1,2),
     *   I = 1,NGAUSS)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      WRITE (HILINE,1086) PRGNAM, ((XDW(J,I), J = 1,3),
     *   I = 1,NGAUSS)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      WRITE (HILINE,1088) PRGNAM, XCRT, XDOO
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      WRITE (HILINE,1090) PRGNAM, XOFS
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
C                                        Solution
      WRITE (HILINE,1100) PRGNAM
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      DO 250 I = 1,NGAUSS
         WRITE (HILINE,1105) PRGNAM, MODEL(XTYPE(I))
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IBTEMP = XTYPE(I)
         GO TO (110, 170), IBTEMP
C                                        Gaussian
 110        WRITE (HILINE,1110) PRGNAM, G(1,I), E(1,I)
            CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
            WRITE (HILINE,1112) PRGNAM, (G(J,I), E(J,I), J = 2,3)
            CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
            WRITE (HILINE,1114) PRGNAM, (G(J,I), E(J,I), J = 4,6)
            CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
            GO TO 250
C                                        Baseline
 170        WRITE (HILINE,1170) PRGNAM, G(1,I), E(1,I)
            CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
            WRITE (HILINE,1172) PRGNAM, (G(J,I), E(J,I), J = 2,3)
            CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
            WRITE (HILINE,1174) PRGNAM, (G(J,I), E(J,I), J = 4,5)
            CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
            WRITE (HILINE,1176) PRGNAM, G(6,I), E(6,I)
            GO TO 250
 250     CONTINUE
      GO TO 950
C
 900  IER = 1
      CALL MSGWRT (8)
C                                       Close HI file
 950  CALL HICLOS (LHOUT, T, IBUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANNOT COPY HI FILE.  IER=',I8)
 1060 FORMAT (A6,'BLC=',7I6)
 1070 FORMAT (A6,'TRC=',7I6)
 1072 FORMAT (A6,'NGAUSS=',I3,',  CTYPE=',4(I3,','))
 1074 FORMAT (A6,'GMAX=',4(1PE11.4,','))
 1076 FORMAT (A6,'GPOS=',4(F6.2,',',F6.2,','))
 1078 FORMAT (A6,'GWID=',4(F4.1,',',F4.1,',',F4.1,','))
 1080 FORMAT (A6,'NITER=',I5)
 1082 FORMAT (A6,'DOMAX=',4F4.1)
 1084 FORMAT (A6,'DOPOS=',8F4.1)
 1086 FORMAT (A6,'DOWID=',12F4.1)
 1088 FORMAT (A6,'DOVECT=',F4.1,',  DOCAT=',F4.1)
 1090 FORMAT (A6,'OFFSET=',F5.2)
 1100 FORMAT (A6,'/Solution')
 1105 FORMAT (A6,'Model type= ',A8)
 1110 FORMAT (A6,'PEAK=',1PE13.3,',',1PE12.3)
 1112 FORMAT (A6,'POS=',F6.2,F5.2,',',F6.2,F5.2)
 1114 FORMAT (A6,'SIZE=',3(F6.1,',',F6.1,4X))
 1170 FORMAT (A6,'ZERO LEVEL=',1PE13.3,',',1PE12.3)
 1172 FORMAT (A6,'SLOPE=',1PE13.3,',',1PE12.2,' ANGLE=',0PF6.1,',',
     *   F5.1)
 1174 FORMAT (A6,'CURV=',1PE13.3,',',1PE12.3,'  ECCEN=',0PF6.4,',',
     *   F5.4)
 1176 FORMAT (A6,'ANGLE OF CURV=',F6.1,',',F5.1)
      END
      SUBROUTINE IMFOUT (RSTO, DOMODL, IER)
C-----------------------------------------------------------------------
C   IMFOUT is a subroutine of IMFIT which prints out the parameter
C   solutions and converts them into useful coordinates.
C   Inputs:
C      RSTO       R         > 0 => write a CC file, > 1.5 use convolved
C                              components, not deconvolved
C      DOMODL     R         Add a CC file also to input image
C   Outputs:
C      IER        I         Error return  0-> okay, 1-> error
C-----------------------------------------------------------------------
      CHARACTER RSTR(2)*20, DUM*12, WORD(3)*8, MODEL(3)*8, CNAME*12,
     *   CCLAS*6, CCTYP*2, CUNIT*8, AXIS*8, FUNIT*8, KEYWRD*8, ARRAY*8,
     *   CAX(3)*14, STRING*24, TUNIT*8
      REAL      TEMP, PASUM, DOMODL, DCONV(3,3), RSTO, RECORD(7),
     *   RICORD(7), CAXINC(2), INT, XXT, YYT, BMAJS, BMINS, BPAS, COSC,
     *   SINC, ERRINT, LN, A, X0, Y0, IMAJ, IMIN, FI, BMAJ, BMIN, RINC,
     *   ARBEAM, ARIMAG, ERRA, FRERRA, ERRMAJ, ERRMIN, ERRFI, ERRX0,
     *   ERRY0, ERRMAS, ERRMIS, ERRAA, XCONV(3,3), RTITLE(8), DX, DY,
     *   SMCB(3), BMFACT, DLFACT, ARHEAD, STWID(3), RDUM, R, XYSH(2),
     *   CORA, CORINT, CORINE, SNR
      HOLLERITH HTITLE(8)
      DOUBLE PRECISION    X(3), XX, YY, ZZ, LAMBDA, XD, STXY(2), DEC1
      LOGICAL   DOCC, CRIT1, CRIT2, SESTAT, OUTSID
      INTEGER   IER, I2TMP, CCLUN, CCVOL, CCVER, NREC, LREC, CCBUF(768),
     *   CILUN, CIVOL, CIVER, CIBUF(768), IRNO, IERR, I, J, IBTEMP,
     *   MFBUF(512), PLANE, MFRNO, ITITLE(8), CSEQ, MFLUN,CUSEID, K,
     *   ILEN, STBUF(512), STKOLS(7), STNUMV(7), STTYPE, STVER, LUNTMP,
     *   STLUN, ISTRNO, IROUND, RPT
      INCLUDE 'INCS:PMFC.INC'
      REAL      ROW(NUMCOL)
      INCLUDE 'IMFIT.INC'
      INCLUDE 'ORFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (HTITLE, ITITLE, RTITLE)
      DATA WORD /'Major ax','Minor ax','Pos ang '/
      DATA MODEL /'Gaussian','Baseline','????????'/
      DATA CAX /'Major axis','Minor axis','Position angle'/
C-----------------------------------------------------------------------
C                                        Initialization
      IER = 0
      PASUM = 90.0
      CALL H2CHR (8, 1, CATH(KHBUN), CUNIT)
      CALL H2CHR (8, 1, CATH(KHTEL), ARRAY)
      TUNIT = CUNIT
      CALL CHLTOU (8, TUNIT)
      FUNIT = CUNIT
      IF (TUNIT.EQ.'JY/BEAM') FUNIT = 'JANSKYS'
      IF (CUNIT.EQ.'Jy/beam') FUNIT = 'Janskys'
      CALL RFILL (NUMCOL, 0.0, ROW)
      PLANE = 1
      SESTAT = .FALSE.
      CALL FNDEXT ('ST', INBLK, I)
      STVER = -1
      IF (XSTVER.GE.0.0) THEN
         STVER = XSTVER + 0.5
         IF ((STVER.EQ.0) .OR. (STVER.GT.I)) STVER = I + 1
         STLUN = LUNTMP (1)
         CALL STINI ('WRITE', STBUF, INVOL, INSL, STVER, INBLK, STLUN,
     *      ISTRNO, STKOLS, STNUMV, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'CANNOT OPEN ST FILE FOR OUTPUT'
            CALL MSGWRT (7)
            STVER = -1
         ELSE
            WRITE (MSGTXT,1001) 'ST', STVER
            CALL MSGWRT (4)
            END IF
         STTYPE = 3
         END IF
C                                       find frequency
      IF (XFREQ.GT.0.0D0) THEN
         LAMBDA = VELITE / XFREQ
      ELSE
         PBPARM(1) = 0.0
         END IF
C                                       Create CC files
      DOCC = (RSTO.GT.0.0) .OR. (DOMODL.GT.0.0)
      IF ((DOCC) .OR. (OUTVER.GE.0)) THEN
         MFLUN = 29
         CCLUN = 28
         CILUN = 27
         CALL WAWA2A (INNA, CNAME, CCLAS, CSEQ, CCTYP, CIVOL, CUSEID)
         CALL WAWA2A (OUTNA, CNAME, CCLAS, CSEQ, CCTYP, CCVOL, CUSEID)
         NREC = 10
         LREC = 7
         CCVER = 0
         CIVER = 0
         IF (RSTO.GT.0.0) THEN
            CALL CCINI (CCLUN, LREC, CCVOL, OUTSL, CCVER, HOI, CCBUF,
     *         IERR)
            IF (IERR.NE.0) THEN
               RSTO = -1.0
            ELSE
               WRITE (MSGTXT,1000) CCVER
               CALL MSGWRT (4)
               END IF
            END IF
         END IF
      IF (.NOT.DOCC) DOMODL = -1.0
C                                       Open with input image
      IF ((DOMODL.GT.0.0) .OR. (OUTVER.GE.0)) THEN
         CALL CATIO ('READ', CIVOL, INSL, CATBLK, 'WRIT', CIBUF, IERR)
         IF (IERR.EQ.0) THEN
            SESTAT = .TRUE.
            IF (DOMODL.GT.0.0) THEN
               CALL CCINI (CILUN, LREC, CIVOL, INSL, CIVER, CATBLK,
     *            CIBUF, IERR)
               IF (IERR.NE.0) THEN
                     DOMODL = -1.0
               ELSE
                  WRITE (MSGTXT,1001) 'CC', CIVER
                  CALL MSGWRT (4)
                  END IF
               END IF
            IF (OUTVER.GE.0) THEN
C                                       get plane number
               PLANE = 1
               IF (CATBLK(KIDIM).GE.7) PLANE = MAX (DEPTH(5), 1)
               IF (CATBLK(KIDIM).GE.6) PLANE = MAX (DEPTH(4), 1) +
     *            (PLANE-1) * MAX (CATBLK(KINAX+5), 1)
               IF (CATBLK(KIDIM).GE.5) PLANE = MAX (DEPTH(3), 1) +
     *            (PLANE-1) * MAX (CATBLK(KINAX+4), 1)
               IF (CATBLK(KIDIM).GE.4) PLANE = MAX (DEPTH(2), 1) +
     *            (PLANE-1) * MAX (CATBLK(KINAX+3), 1)
               IF (CATBLK(KIDIM).GE.3) PLANE = MAX (DEPTH(1), 1) +
     *            (PLANE-1) * MAX (CATBLK(KINAX+2), 1)
               CALL MFINI (MFLUN, CIVOL, INSL, DEPTH, PLANE, OUTVER,
     *            CATBLK, MFBUF, IERR)
               IF (IERR.GT.0) THEN
                  OUTVER = -1
               ELSE
                  MFRNO = MFBUF(5)
                  WRITE (MSGTXT,1001) 'MF', OUTVER
                  CALL MSGWRT (4)
C                                       Update the Keyword
                  KEYWRD = 'REALRMS'
                  CALL CHR2H (8, KEYWRD, 1, HTITLE)
                  RTITLE(3) = ACTRMS
                  ITITLE(4) = 0
                  ITITLE(5) = 2
                  CALL TABIO ('WRIT', 5, 7, HTITLE, MFBUF, IERR)
                  IF (IERR.NE.0) OUTVER = -1
                  END IF
               END IF
            END IF
         END IF
C
      IF (DOCRT.GT.-2.5) THEN
         MSGTXT = ' '
         CALL DOPRT
         END IF
      WRITE (MSGTXT,1005)
      CALL DOPRT
      WRITE (MSGTXT,1010) ACTRMS, CUNIT
      CALL DOPRT
      IF (DOCRT.GT.-2.5) THEN
         MSGTXT = ' '
         CALL DOPRT
         END IF
      ROW(COLPLN) = PLANE
      ROW(CORRMS) = SUMSQ
      ROW(CORPEK) = RESMAX
      ROW(CORFLX) = RESSUM
      IRNO = 0
      DO 800 I = 1,NGAUSS
         DO 10 J = 1,6
            E(J,I) = ABS (E(J,I))
 10         CONTINUE
         IBTEMP = XTYPE(I)
         GO TO (15, 215, 415), IBTEMP
C                                        GAUSSIAN COMPONENT
C                                        First, scale amplitude
 15      RECORD(1) = G(1,I)
         E(1,I) = ABS (E(1,I))
C                                       MF out
         MFRNO = MFRNO + 1
         IF (.NOT.NWIDTH(I)) ROW(COLTYP) = 1.0
C                                        Fix diameters
         G(6,I) = G(6,I)/DG2RAD + PASUM
         E(6,I) = E(6,I)/DG2RAD
         G(4,I) = ABS (G(4,I))
         G(5,I) = ABS (G(5,I))
         IF (G(4,I).GE.G(5,I)) GO TO 20
            TEMP = G(5,I)
            G(5,I) = G(4,I)
            G(4,I) = TEMP
            G(6,I) = G(6,I) - 90.0
 20      IF (G(6,I).LE.180.0) GO TO 30
            G(6,I) = G(6,I) - 180.0
            GO TO 20
 30      IF (G(6,I).GT.0.0) GO TO 40
            G(6,I) = G(6,I) + 180.0
            GO TO 30
C                                        Print out header
 40      WRITE (MSGTXT,1042) I, MODEL(XTYPE(I))
         CALL DOPRT
C                                       implement better designation fo
C                                       the fitting parameters
         A = G(1,I)
         CORA = A
         X0 = G(2,I)
         Y0 = G(3,I)
         IMAJ = G(4,I)
         IMIN = G(5,I)
         FI = G(6,I)
         BMAJ = CB(1)
         BMIN = CB(2)
         ARBEAM = BMAJ*BMIN
         ARHEAD = HCBP(1)*HCBP(2)
         IF (ARHEAD.LE.0.0) ARHEAD = ARBEAM
         ARIMAG = IMAJ*IMIN
         CRIT1 = ARBEAM .LT. 0.1*ARIMAG
         CRIT2 = ARBEAM .GT. 0.9*ARIMAG
         LN = SQRT (8 * ALOG (2.0))
         RPT = 4
C                                       FSHIFT output
         IF ((I.EQ.1) .AND. (AXTYP(LOCNUM).EQ.1)) THEN
            FSHIFT(1) = (X0 - IROUND (X0)) * AXINC(1,LOCNUM)*3600.
            FSHIFT(2) = (Y0 - IROUND (Y0)) * AXINC(2,LOCNUM)*3600.
            DEC1 = RPVAL(3-CORTYP(LOCNUM),LOCNUM)
            XYSH(1) = CATR(KRXSH) * COS(DG2RAD*DEC1) * 3600.
            XYSH(2) = CATR(KRYSH) * 3600.
            IF (ABS(ROT(LOCNUM)).GT.1.E-3) THEN
               R = ROT(LOCNUM) * DG2RAD
               RDUM = XYSH(1)
               XYSH(1) = RDUM * COS(R) + XYSH(2) * SIN(R)
               XYSH(2) = XYSH(2) * COS(R) - RDUM * SIN(R)
               END IF
            FSHIFT(1) = XYSH(1) + FSHIFT(1)
            FSHIFT(2) = XYSH(2) + FSHIFT(2)
            END IF
C                                       Implement the error analysis of
C                                       J. Condon, 'Errors in elliptical
C                                       gausian fits', AJ, 1996
C                                       fitted gaussian is much wider
C                                       than beam size
         IF (CRIT1) THEN
            ERRA = 2.0 * ACTRMS
            IF ((ARIMAG.GT.0.0) .AND. (ARBEAM.GT.0.0)) ERRA =
     *         SQRT (8.0*ARBEAM/ARIMAG) * ACTRMS
C                                       fitted gaussian near beam size.
         ELSE IF (CRIT2) THEN
            ERRA = ACTRMS
C                                       The source is partially
C                                       resolved. The errors formulas
C                                       are not garanteed correct!!!
         ELSE
            ERRA = 2.0 * ACTRMS
            IF ((ARIMAG.GT.0.0) .AND. (ARBEAM.GT.0.0)) ERRA =
     *         SQRT (0.8 + (0.2/0.8)*(ARBEAM/ARIMAG-0.1)) * ACTRMS
            END IF
         ORA(I) = A
         ORERRA(I) = ERRA
         FRERRA = 1
         IF (A.NE.0.0) FRERRA = ABS (ERRA / A)
         ERRMAJ = ABS (IMAJ * FRERRA)
         ERRMIN = ABS (IMIN * FRERRA)
         ERRFI = SQRT (2.0) * (IMAJ*IMIN) / MAX (1.E-6, IMAJ**2-IMIN**2)
     *      * FRERRA * RAD2DG
         ERRFI = MIN (360.0, ABS(ERRFI))
         ORERFI(I) = ERRFI
         COSC = COS (DG2RAD * FI)
         SINC = SIN (DG2RAD * FI)
C                                       SINC&COSC switch because
C                                       the angle is measured from Y
         ERRX0 = SQRT ((ERRMAJ*SINC)**2 + (ERRMIN*COSC)**2) / LN
         ERRY0 = SQRT ((ERRMAJ*COSC)**2 + (ERRMIN*SINC)**2) / LN
C                                        Amplitude
         ERRAA = ERRA
         ROW(COLPEK) = RECORD(1)
         ROW(COEPEK) = ERRAA
         FGM(I) = A
         IF (XDM(I).GT.0.0) FDM(I) = ERRAA
         IF (((ARHEAD/ARBEAM.LT.0.99) .OR. (ARHEAD/ARBEAM.GT.1.01))
     *      .AND. (TUNIT.EQ.'JY/BEAM')) THEN
            WRITE (MSGTXT,1144) A, ERRAA
            CALL DOPRT
            WRITE (MSGTXT,1145) A*ARBEAM/ARHEAD, ERRAA*ARBEAM/ARHEAD
            CALL DOPRT
         ELSE
            SNR = 0.0
            IF (ERRAA.GT.0.0) SNR = MIN (999.0, A/ERRAA)
            WRITE (MSGTXT,1044) A, ERRAA, CUNIT, SNR
            CALL DOPRT
            END IF
C                                       Radius
         BMFACT = 1.0
         DLFACT = 1.0
         XD = 0.0D0
         IF (((CORTYP(LOCNUM).EQ.1) .OR. (CORTYP(LOCNUM).EQ.2)) .AND.
     *      ((PBPARM(1).GT.0.0) .OR. (BWS.GT.0.0))) THEN
            CALL XYVAL (G(2,I), G(3,I), X(1), X(2), X(3), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1051) IERR, I
               CALL MSGWRT (7)
               XD = 0.0D0
            ELSE IF (CORTYP(LOCNUM).EQ.1) THEN
               XD = SIN (DG2RAD*X(2)) * SIN (DG2RAD*XDEC) +
     *           COS (DG2RAD*X(2)) * COS (DG2RAD*XDEC) *
     *           COS (DG2RAD * (X(1) - XRA))
            ELSE
               XD = SIN (DG2RAD*X(1)) * SIN (DG2RAD*XDEC) +
     *           COS (DG2RAD*X(1)) * COS (DG2RAD*XDEC) *
     *           COS (DG2RAD * (X(2) - XRA))
               END IF
            IF (XD.GT.1.0D0) THEN
               XD = 0.0D0
            ELSE
               XD = RAD2DG * ACOS (XD)
               END IF
            END IF
C                                       primary beam
         IF (((CORTYP(LOCNUM).EQ.1) .OR. (CORTYP(LOCNUM).EQ.2)) .AND.
     *      (PBPARM(1).GT.0.0)) THEN
            CALL PBCALC (XD, LAMBDA, ARRAY, PBPARM(2), BMFACT, OUTSID)
            BMFACT = MAX (BMFACT, PBPARM(1))
            IF (OUTSID) BMFACT = PBPARM(1)
            END IF
C                                       BW smearing here
         IF ((CB(1).GT.0.0) .AND. (BWS.GT.0.0)) THEN
            DX = G(2,I) - XCEN
            DY = G(3,I) - YCEN
            CALL BWSMCB (DX, DY, XD, BWS, CB, SMCB)
            DLFACT = SMCB(1) * SMCB(2) / (CB(1) * CB(2))
            END IF
C                                       correct amplitude
         IF ((DLFACT.NE.1.0) .OR. (BMFACT.NE.1.0)) THEN
            ERRAA = ERRAA * DLFACT / BMFACT
            CORA = A  * DLFACT / BMFACT
            SNR = 0.0
            IF (ERRAA.GT.0.0) SNR = MIN (999.0, CORA/ERRAA)
            IF (((ARHEAD/ARBEAM.LT.0.99) .OR. (ARHEAD/ARBEAM.GT.1.01))
     *         .AND. (TUNIT.EQ.'JY/BEAM')) THEN
               WRITE (MSGTXT,1045) CORA, ERRAA, CUNIT, SNR
            ELSE
               WRITE (MSGTXT,1046) CORA, ERRAA, CUNIT
               END IF
            CALL DOPRT
            END IF
C                                       flux is scaled with header beam
C                                       not actual beam
         IF (ARHEAD.GT.0.0) THEN
            INT = ARIMAG / ARHEAD * A
            IF (NWIDTH(I)) THEN
               ERRINT = FRERRA * INT
            ELSE
               ERRINT = FRERRA * SQRT(1 + 2*(ARHEAD/ARIMAG)) * INT
               END IF
            ERRINT = ABS (ERRINT)
            CORINT = INT
            CORINE = ERRINT
            WRITE (MSGTXT,1047) INT, ERRINT, FUNIT
            CALL DOPRT
            RECORD(1) = INT
            ROW(COEFLX) = ERRINT
C                                       correct flux
            IF (BMFACT.NE.1.0) THEN
               CORINE = ERRINT / BMFACT
               CORINT = INT / BMFACT
               WRITE (MSGTXT,1048) CORINT, CORINE, FUNIT
               CALL DOPRT
               END IF
C                                       resolution issue
            IF (CORA.GT.0.0) THEN
               IF (CORINT-CORINE.GT.CORA) THEN
                  RPT = 3
               ELSE
                  RPT = 2
                  END IF
            ELSE IF (CORA.LT.0.0) THEN
               IF (CORINT+CORINE.LT.CORA) THEN
                  RPT = 3
               ELSE
                  RPT = 2
                  END IF
               END IF
         ELSE
            ROW(COEFLX) = ROW(COEPEK)
            END IF
         ROW(COLFLX) = RECORD(1)
         ROW(COBMFA) = 1.0 / MAX (1.E-12, BMFACT)
         ROW(CODLFA) = DLFACT
C                                        Positions
         WRITE (MSGTXT,1050) 'X', X0, ERRX0
         CALL DOPRT
         FGP(1,I) = X0
         IF (XDP(1,I).GT.0.0) FDP(1,I) = ERRX0
         WRITE (MSGTXT,1050) 'Y', Y0, ERRY0
         CALL DOPRT
         ORERRX(I) = ERRX0
         ORERRY(I) = ERRY0
         FGP(2,I) = Y0
         IF (XDP(2,I).GT.0.0) FDP(2,I) = ERRY0
         RECORD(2) = (G(2,I) - RPLOC(1,LOCNUM)) * AXINC(1,LOCNUM)
         RECORD(3) = (G(3,I) - RPLOC(2,LOCNUM)) * AXINC(2,LOCNUM)
         CALL RCOPY (5, G(2,I), ROW(COPCEX))
C                                       get local pixel pair
         CALL XYVAL (G(2,I), G(3,I), X(1), X(2), X(3), IERR)
         STXY(1) = X(1)
         STXY(2) = X(2)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1051) IERR, I
            CALL MSGWRT (7)
            CAXINC(1) = AXINC(1,LOCNUM)
            CAXINC(2) = AXINC(2,LOCNUM)
            GO TO 80
         ELSE
            CALL XYVAL (G(2,I)+1.0, G(3,I), XX, YY, ZZ, IERR)
            XXT = (XX - X(1)) * COS (DG2RAD*X(2))
            YYT = YY - X(2)
            CAXINC(1) = XXT * COS (DG2RAD*ROT(LOCNUM)) + YYT *
     *         SIN (DG2RAD*ROT(LOCNUM))
            CALL XYVAL (G(2,I), G(3,I)+1.0, XX, YY, ZZ, IERR)
            XXT = (XX - X(1)) * COS (DG2RAD*X(2))
            YYT = YY - X(2)
            CAXINC(2) = YYT * COS (DG2RAD*ROT(LOCNUM)) -
     *         XXT * SIN (DG2RAD*ROT(LOCNUM))
            END IF
         ROW(COLDLX) = RECORD(2)
         ROW(COLDLY) = RECORD(3)
         ROW(COEDLX) = ABS (ERRX0 * CAXINC(1))
         ROW(COEDLY) = ABS (ERRY0 * CAXINC(2))
C                                       Coordinate labels
C                                       RA and DEC
         IF (AXTYP(LOCNUM).EQ.1) THEN
            RINC = MAX (ABS (CAXINC(1)), ABS (CAXINC(2))) * 3600.0
            DO 60 J = 1,2
               I2TMP = J - 1
               CALL AXSTRN (CTYP(J,LOCNUM), X(J), I2TMP, ILEN, RSTR(J))
 60            CONTINUE
            ERRX0 = ABS (ERRX0 * CAXINC(1) * 240.0 / COS (DG2RAD*X(2)))
            ERRY0 = ABS (ERRY0 * CAXINC(2) * 3600.0)
            IF (RINC.GT.1.0) THEN
               WRITE (MSGTXT,1060) RSTR(1), ERRX0
            ELSE IF (RINC.GT.0.01) THEN
               WRITE (MSGTXT,1061) RSTR(1), ERRX0
            ELSE IF (RINC.GT.0.0001) THEN
               WRITE (MSGTXT,1062) RSTR(1), ERRX0
            ELSE
               WRITE (MSGTXT,1063) RSTR(1), ERRX0
               END IF
            CALL DOPRT
            IF (RINC.GT.1.0) THEN
               WRITE (MSGTXT,1065) RSTR(2), ERRY0
            ELSE IF (RINC.GT.0.01) THEN
               WRITE (MSGTXT,1066) RSTR(2), ERRY0
            ELSE IF (RINC.GT.0.0001) THEN
               WRITE (MSGTXT,1067) RSTR(2), ERRY0
            ELSE
               WRITE (MSGTXT,1068) RSTR(2), ERRY0
               END IF
C                                       Other type
         ELSE
            ERRX0 = ABS (ERRX0 * CAXINC(1))
            ERRY0 = ABS (ERRY0 * CAXINC(2))
            CALL H2CHR (8, 1, CATH(KHCTP), AXIS)
            WRITE (MSGTXT,1070) AXIS, X(1), ERRX0
            CALL DOPRT
            CALL H2CHR (8, 1, CATH(KHCTP+2), AXIS)
            WRITE (MSGTXT,1070) AXIS, X(2), ERRY0
            END IF
         CALL DOPRT
         IF (NWIDTH(I)) THEN
            ERRMAJ = 0.0
            ERRMIN = 0.0
            ERRFI = 0.0
            END IF
C                                       Diameters
 80      IF (E(6,I).GT.90.0) E(6,I) = 90.0
         IF (ERRFI.GT.90.0) ERRFI = 90.0
         DO 85 J = 4,5
            IF (G(J,I).GE.99.5) G(J,I) = 99.5
            IF (E(J,I).GE.99.5) E(J,I) = 99.5
 85         CONTINUE
         IF (ERRMAJ.GE.99.5) ERRMAJ = 99.5
         IF (ERRMIN.GE.99.5) ERRMIN = 99.5
         WRITE (MSGTXT,1080) CAX(1), IMAJ, ERRMAJ, ' pixels'
         CALL DOPRT
         FGW(1,I) = IMAJ
         IF (XDW(1,I).GT.0.0) FDW(1,I) = ERRMAJ
         WRITE (MSGTXT,1080) CAX(2), IMIN, ERRMIN, ' pixels'
         CALL DOPRT
         FGW(2,I) = IMIN
         IF (XDW(2,I).GT.0.0) FDW(2,I) = ERRMIN
         WRITE (MSGTXT,1081) CAX(3), FI, ERRFI, ' degrees'
         CALL DOPRT
         FGW(3,I) = FI
         IF (XDW(3,I).GT.0.0) FDW(3,I) = ERRFI
C                                       convert from pixels
         CALL GAUSPS ('P2S', G(2,I), G(3,I), G(4,I), G(5,I), G(6,I),
     *      BMAJS, BMINS, BPAS, IERR)
         STWID(1) = BMAJS
         STWID(2) = BMINS
         STWID(3) = BPAS
         WRITE (STRING,1075) I
C                                       stars info now known
         IF (STVER.GT.0) THEN
            CALL TABST ('WRIT', STBUF, ISTRNO, STKOLS, STNUMV, STXY,
     *         STWID, STTYPE, STRING, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'ERROR WRITING STARS FILE'
               CALL MSGWRT (7)
               STVER = 0
               END IF
            END IF
         RECORD(4) = BMAJS
         RECORD(5) = BMINS
         RECORD(6) = BPAS
         RECORD(7) = 2.0
         ROW(COLMJX) = RECORD(4)
         ROW(COLMNX) = RECORD(5)
         ROW(COLPAN) = RECORD(6)
         IF (ROW(COLPAN).LT.0.0) ROW(COLPAN) = ROW(COLPAN) + 180.0
         CALL RCOPY (7, RECORD, RICORD)
         RICORD(7) = 1.0
C                                       non RA/Dec
         IF (AXTYP(LOCNUM).NE.1) THEN
            ROW(COLMJX) = G(4,I)
            ROW(COEMJX) = ERRMAJ
            ROW(COLMNX) = G(5,I)
            ROW(COEMNX) = ERRMIN
            ROW(COLPAN) = G(6,I)
            ROW(COEPAN) = ERRFI
C                                       RA, Dec display
         ELSE
C                                       Put in arcsec
            BMAJS = BMAJS * 3600.
            BMINS = BMINS * 3600.
            IF (G(4,I).NE.0.0) THEN
               ERRMAS = ERRMAJ * BMAJS / G(4,I)
            ELSE
               ERRMAS = -1.0
               END IF
            IF (G(5,I).NE.0.0) THEN
               ERRMIS = ERRMIN * BMINS / G(5,I)
            ELSE
               ERRMIS = -1.0
               END IF
            ORERMA(I) = ERRMAS
            ORERMI(I) = ERRMIS
            ORBMAJ(I) = BMAJS
            ORBMIN(I) = BMINS
            ORBPA(I) = BPAS
            ROW(COEMJX) = ERRMAS / 3600.
            ROW(COEMNX) = ERRMIS / 3600.
            ROW(COEPAN) = ERRFI
            IF (RINC.GT.1.0) THEN
               WRITE (MSGTXT,1085) CAX(1), BMAJS, ERRMAS, ' asec'
            ELSE IF (RINC.GT.0.01) THEN
               WRITE (MSGTXT,1086) CAX(1), BMAJS, ERRMAS, ' asec'
            ELSE IF (RINC.GT.0.0001) THEN
               WRITE (MSGTXT,1087) CAX(1), BMAJS, ERRMAS, ' asec'
            ELSE
               WRITE (MSGTXT,1088) CAX(1), BMAJS, ERRMAS, ' asec'
               END IF
            CALL DOPRT
            IF (RINC.GT.1.0) THEN
               WRITE (MSGTXT,1085) CAX(2), BMINS, ERRMIS, ' asec'
            ELSE IF (RINC.GT.0.01) THEN
               WRITE (MSGTXT,1086) CAX(2), BMINS, ERRMIS, ' asec'
            ELSE IF (RINC.GT.0.0001) THEN
               WRITE (MSGTXT,1087) CAX(2), BMINS, ERRMIS, ' asec'
            ELSE
               WRITE (MSGTXT,1088) CAX(2), BMINS, ERRMIS, ' asec'
               END IF
            CALL DOPRT
            WRITE (MSGTXT,1081) CAX(3), BPAS, ERRFI, ' degrees'
            CALL DOPRT
            IF (I.EQ.1) THEN
               WRITE (MSGTXT,1082) FSHIFT
               CALL DOPRT
               END IF
            END IF
C                                       Deconvolve if possible
         IF (CB(1).GT.0.0) THEN
C                                       BW smearing here
            DX = G(2,I) - XCEN
            DY = G(3,I) - YCEN
            CALL BWSMCB (DX, DY, XD, BWS, CB, SMCB)
            CALL BMVAL (G(4,I), G(5,I), G(6,I), ERRMAJ, ERRMIN, ERRFI,
     *         SMCB(1), SMCB(2), SMCB(3), FACTOR, DCONV)
            WRITE (MSGTXT,1090)
            IF (DOCRT.GT.-2.5) THEN
               CALL DOPRT
            ELSE
               CALL MSGWRT (4)
               END IF
            WRITE (MSGTXT,1092)
            CALL DOPRT
            WRITE (MSGTXT,1094)
            CALL DOPRT
            DO 100 J = 1,3
               WRITE (MSGTXT,1096) WORD(J), (DCONV(J,K), K = 1,3)
               CALL DOPRT
 100           CONTINUE
C                                       resolved??
            IF (DCONV(1,1).LE.0.0) RPT = 1
            IF (RPT.EQ.3) THEN
               IF (DCONV(1,2).LE.0) RPT = 2
            ELSE IF (RPT.EQ.2) THEN
               IF (DCONV(1,2).LE.0.0) RPT = 1
               END IF
C                                       RA, Dec display
            IF (AXTYP(LOCNUM).EQ.1) THEN
               WRITE (MSGTXT,1105)
               CALL DOPRT
               WRITE (MSGTXT,1094)
               CALL DOPRT
               DO 105 J = 1,3
C                                       convert from pixels
                  IF (DCONV(1,J).GT.0.0) THEN
                     CALL GAUSPS ('P2S', G(2,I), G(3,I), DCONV(1,J),
     *                  DCONV(2,J), DCONV(3,J), BMAJS, BMINS, BPAS,
     *                  IERR)
                     XCONV(1,J) = BMAJS
                     XCONV(2,J) = BMINS
                     XCONV(3,J) = BPAS
                  ELSE
                     XCONV(1,J) = DCONV(1,J) * ABS (CAXINC(1))
                     XCONV(2,J) = DCONV(2,J) * ABS (CAXINC(1))
                     XCONV(3,J) = DCONV(3,J) - ROT(LOCNUM)
                     END IF
                  IF (XCONV(3,J).GT.180.) XCONV(3,J)=XCONV(3,J)-180.
                  IF (XCONV(3,J).LT.0.) XCONV(3,J) = XCONV(3,J)+180.
                  DCONV(1,J) = XCONV(1,J) * 3600.
                  DCONV(2,J) = XCONV(2,J) * 3600.
                  DCONV(3,J) = XCONV(3,J)
 105              CONTINUE
               DO 110 J = 1,3
                  WRITE (MSGTXT,1107) WORD(J), (DCONV(J,K), K = 1,3)
                  CALL DOPRT
 110              CONTINUE
               RICORD(4) = DCONV(1,1) / 3600.0
               RICORD(5) = DCONV(2,1) / 3600.0
               RICORD(6) = DCONV(3,1)
               IF ((RSTO.GT.0.0) .AND. (RSTO.LE.1.5)) THEN
                  RECORD(4) = RICORD(4)
                  RECORD(5) = RICORD(5)
                  RECORD(6) = RICORD(6)
                  RECORD(7) = 1.0
                  END IF
               END IF
            IF ((RPT.GE.1) .AND. (RPT.LE.3)) THEN
               IF (RPT.EQ.1) THEN
                  MSGTXT = '  Component appears unresolved, use peak as'
     *               // ' total flux'
               ELSE IF (RPT.EQ.3) THEN
                  MSGTXT = '  Component appears resolved, use integral'
     *               // ' as total flux'
               ELSE
                  MSGTXT = '  Component may be unresolved or resolved,'
     *               // ' use caution'
                  END IF
               CALL DOPRT
               END IF
            END IF
         CALL RCOPY (9, XCONV, ROW(COLD0J))
         IF (OUTVER.GE.0) THEN
            CALL TABIO ('WRIT', 0, MFRNO, ROW, MFBUF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1110) IERR, 'MF', MFRNO
               CALL MSGWRT (7)
               OUTVER = -1
               END IF
            END IF
C                                       write CC file(s)
         IF (DOCC) THEN
            IRNO = IRNO + 1
            IERR = 0
            IF (RSTO.GT.0.0) THEN
               CALL TABIO ('WRIT', 0, IRNO, RECORD, CCBUF, IERR)
               IF (IERR.NE.0) RSTO = -1.0
               END IF
            IF (DOMODL.GT.0.0) THEN
               CALL TABIO ('WRIT', 0, IRNO, RICORD, CIBUF, IERR)
               IF (IERR.NE.0) DOMODL = -1.0
               END IF
            END IF
         GO TO 790
C                                       Baseline solution
 215     WRITE (MSGTXT,1042) I, MODEL(XTYPE(I))
         CALL DOPRT
C                                        Zero level
         IF (E(1,I).EQ.0.0) GO TO 220
            E(1,I) = ABS (E(1,I))
            WRITE (MSGTXT,1215) G(1,I), E(1,I)
            CALL DOPRT
            FGM(I) = G(1,I)
            IF (XDM(I).GT.0.0) FDM(I) = E(1,I)
C                                        Slope
 220     IF ((E(2,I).EQ.0.0) .AND. (E(3,I).EQ.0.0)) GO TO 260
            E(2,I) = ABS (E(2,I))
            G(3,I) = G(3,I) / DG2RAD + PASUM
            E(3,I) = E(3,I) / DG2RAD
 230        IF (G(3,I).LE.180.0) GO TO 240
               G(3,I) = G(3,I) - 180.0
               GO TO 230
 240        IF (G(3,I).GT.0.0) GO TO 250
               G(3,I) = G(3,I) + 180.0
               GO TO 240
 250        IF (E(2,I).GT.1.0) E(2,I) = 1.0
            IF (E(3,I).GT.90.0) E(3,I) = 90.0
            WRITE (MSGTXT,1245) G(2,I), E(2,I)
            CALL DOPRT
            WRITE (MSGTXT,1246) G(3,I), E(3,I)
            CALL DOPRT
            FGP(1,I) = G(2,I)
            FGP(2,I) = G(3,I)
            IF (XDP(1,I).GT.0.0) FDP(1,I) = E(2,I)
            IF (XDP(2,I).GT.0.0) FDP(2,I) = E(3,I)
C                                        Curvature
 260     IF ((E(4,I).EQ.0.0) .AND. (E(5,I).EQ.0.0) .AND.
     *      (E(6,I).EQ.0.0)) GO TO 300
            E(4,I) = ABS (E(4,I))
            G(5,I) = ABS (G(5,I))
            G(6,I) = G(6,I) / DG2RAD + PASUM
            E(6,I) = E(6,I) / DG2RAD
            IF (E(5,I).GT.2.0) E(5,I) = 2.0
            IF (E(6,I).GT.90.0) E(6,I) = 90.0
 270        IF (G(6,I).LE.180.0) GO TO 280
               G(6,I) = G(6,I) - 180.0
               GO TO 270
 280        IF (G(6,I).GT.0.0) GO TO 290
               G(6,I) = G(6,I) + 180.0
               GO TO 280
 290        WRITE (MSGTXT,1290) G(4,I), E(4,I)
            CALL DOPRT
            WRITE (MSGTXT,1291) G(6,I), E(6,I)
            CALL DOPRT
            WRITE (MSGTXT,1292) G(5,I), E(5,I)
            CALL DOPRT
            FGW(1,I) = G(4,I)
            FGW(2,I) = G(5,I)
            FGW(3,I) = G(6,I)
            IF (XDW(1,I).GT.0.0) FDW(1,I) = E(4,I)
            IF (XDW(2,I).GT.0.0) FDW(2,I) = E(5,I)
            IF (XDW(3,I).GT.0.0) FDW(3,I) = E(6,I)
 300        CONTINUE
         GO TO 790
C
 415     CONTINUE
 790     WRITE (MSGTXT,1090)
         IF (DOCRT.GT.-2.5) THEN
            CALL DOPRT
         ELSE
            CALL MSGWRT (4)
            END IF
 800     CONTINUE
      IF (DOCRT.LE.-3.5) CALL ORPRT
      IF (OUTVER.GE.0) CALL TABIO ('CLOS', 0, MFRNO, ROW, MFBUF, IERR)
      IF (RSTO.GT.0.) CALL TABIO ('CLOS', 0, IRNO, RECORD, CCBUF, IERR)
      IF (DOMODL.GT.0.0) CALL TABIO ('CLOS', 0, IRNO, RICORD, CIBUF,
     *   IERR)
      IF (STVER.GT.0) CALL TABST ('CLOS', STBUF, ISTRNO, STKOLS, STNUMV,
     *   STXY, STWID, STTYPE, STRING, IERR)
      IF (SESTAT) CALL CATDIR ('CSTA', CIVOL, INSL, DUM, DUM, DUM, DUM,
     *   DUM, 'CLWR', CIBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Writing CC file version',I4,' attached to output image')
 1001 FORMAT ('Writing ',A,' file version',I4,
     *   ' attached to input image')
 1005 FORMAT ('********* Solution from IMFIT ',3('***********'))
 1010 FORMAT ('Error estimate based on rms',1PE10.3,1X,A)
 1042 FORMAT ('Component ',I3,' - ',A8)
 1044 FORMAT (2X,'Peak intensity    =',1PE11.4,' +/-',1PE9.2,1X,A8,2X,
     *   '(',0PF6.2,')')
 1144 FORMAT (2X,'Peak intensity    =',1PE11.4,' +/-',1PE9.2,
     *   ' Jy/headerbeam')
 1145 FORMAT (2X,'Peak intensity    =',1PE11.4,' +/-',1PE9.2,
     *   ' Jy/channelbeam')
 1045 FORMAT (2X,'Corrected peak int=',1PE11.4,' +/-',1PE9.2,1X,A8,2X,
     *   '(',0PF6.2,')')
 1046 FORMAT (2X,'Corrected peak int=',1PE11.4,' +/-',1PE9.2,1X,A8)
 1047 FORMAT (2X,'Integral intensity=',1PE11.4,' +/-',1PE9.2,1X,A8)
 1048 FORMAT (2X,'Corrected flux    =',1PE11.4,' +/-',1PE9.2,1X,A8)
 1050 FORMAT (2X,A,'-position        =  ',F9.3,' +/-',F9.4, ' pixels')
 1051 FORMAT ('ERROR',I3,' CONVERTING TO SKY POSITION OF COMPONENT',I3)
 1060 FORMAT (2X,20X,A20,' +/- ',F8.3)
 1061 FORMAT (2X,20X,A20,' +/- ',F10.7)
 1062 FORMAT (2X,20X,A20,' +/- ',F12.9)
 1063 FORMAT (2X,20X,A20,' +/- ',F14.11)
 1065 FORMAT (2X,20X,A20,' +/- ',F8.2)
 1066 FORMAT (2X,20X,A20,' +/- ',F10.6)
 1067 FORMAT (2X,20X,A20,' +/- ',F12.8)
 1068 FORMAT (2X,20X,A20,' +/- ',F14.10)
 1070 FORMAT (2X,A8,11X,'=',1PE13.5,' +/- ',1PE10.3)
 1075 FORMAT ('IMFIT_',I1)
 1080 FORMAT (2X,A14,'    =',F8.3,' +/- ',F7.4, A)
 1081 FORMAT (2X,A14,'    =',F8.3,' +/- ',F6.3, A)
 1082 FORMAT ('  RASHIFT=',F11.6,' DECSHIFT=',F11.6,
     *   ' to center on pixel')
 1085 FORMAT (2X,A14,'    =',F8.3,' +/-',F8.3, A)
 1086 FORMAT (2X,A14,'    =',F9.5,' +/-',F9.5, A)
 1087 FORMAT (2X,A14,'    =',F10.7,' +/-',F10.7, A)
 1088 FORMAT (2X,A14,'    =',F11.9,' +/-',F11.9, A)
 1090 FORMAT (2X,30('- '))
 1092 FORMAT (18X,'Deconvolution of component in pixels')
 1094 FORMAT (23X, 'Nominal',5X,'Minimum',5X,'Maximum')
 1096 FORMAT (4X,A8,6X,3F12.3)
 1105 FORMAT (18X,'Deconvolution of component in asec')
 1107 FORMAT (4X,A8,6X,3F12.6)
 1110 FORMAT ('ERROR',I5,' WRITING ',A,' ROW',I7)
 1215 FORMAT (2X,'Zero level=',1PE12.4,'(',1PE11.4,')  Ampl. units')
 1245 FORMAT (2X,'Slope=',1PE11.3,'(',1PE10.3,') Ampl/pixl')
 1246 FORMAT (5X,'in angle',0PF7.2,'(',0PF6.2,') Deg')
 1290 FORMAT (2X,'Curv.=',1PE11.3,'(',1PE10.3,')  Ampl/pixel**2')
 1291 FORMAT (5X,'in angle',F7.2,'(',F6.2,') deg')
 1292 FORMAT (5X,'Eccentricity=',F8.3,'(',F7.3,')')
      END
      SUBROUTINE BMVAL (BMAJ, BMIN, BPA, BMAJE, BMINE, BPAE, CBMAJ,
     *   CBMIN, CBPA, FACTOR, R)
C-----------------------------------------------------------------------
C   Subroutine BMVAL deconvoles the fitted beam from the clean beam and
C   also generates appropriate errors.  This is used with IMFIT.
C   Inputs:
C      BMAJ       R       Fitter major axis
C      BMIN       R       Fitted minor axis
C      BPA        R       Fitted pos. angle (deg)   WRT Y
C      BMAJE      R       Fitted major axis error
C      BMINE      R       Fitted minor axis error
C      BPAE       R       Fitted pos. angle error (deg)
C      CBMAJ      R       Clean beam major axis
C      CBMIN      R       Clean beam minor axis
C      CBPA       R       Clean beam pos. angle (deg)  WRT Y
C   Outputs:
C      R(3,3)     R       RMAJ, RMIN, RPA array
C-----------------------------------------------------------------------
      REAL      BMAJ, BMIN, BPA, BMAJE, BMINE, BPAE, CBMAJ, CBMIN, CBPA,
     *   FACTOR, R(3,3)
C
      INTEGER   IERR, I, J, K, IC
      REAL      B1, B2, B3, DELT(3), MAJOR, MINOR, PA, TEMP
      DATA DELT /-0.7,0.0,0.7/
C-----------------------------------------------------------------------
C                                       Deconvolve the fit
      CALL DECONV (BMAJ, BMIN, BPA, CBMAJ, CBMIN, CBPA, MAJOR, MINOR,
     *   PA, IERR)
      R(1,1) = MAJOR
      R(2,1) = MINOR
      R(3,1) = PA
C                                       initial minimum
      R(1,2) = MAJOR
      R(2,2) = MINOR
      R(3,2) = PA
C                                       Initial maximum
      R(1,3) = MAJOR
      R(2,3) = MINOR
      R(3,3) = PA
C                                      Set up looping
      IC = 0
      DO 50 K = 1,3
         B3 = BPA + DELT(K) * BPAE * FACTOR
         DO 50 J = 1,3
            B2 = BMIN + DELT(J) * BMINE * FACTOR
            DO 50 I = 1,3
               B1 = BMAJ + DELT(I) * BMAJE * FACTOR
               IC = IC + 1
               CALL DECONV (B1, B2, B3, CBMAJ, CBMIN, CBPA, MAJOR,
     *            MINOR, PA, IERR)
C                                       If FIT worked sort of::
               IF (R(1,1).GT.0.0) THEN
C                                       first look at PA
                  PA = MOD((R(3,1) - PA +720.0), 180.0)
                  IF (PA.LT.45) THEN
                     PA = PA + R(3,1)
                  ELSE IF (PA.GT.135) THEN
                     PA = PA + R(3,1) - 180.0
C                                       Switch major, minor axes
                  ELSE
                     TEMP = MINOR
                     MINOR = MAJOR
                     MAJOR = TEMP
                     PA = PA + R(3,1) -90.0
                     END IF
                  END IF
C                                       Upper and lower bounds
               R(1,2) = MIN (R(1,2), MAJOR)
               R(1,3) = MAX (R(1,3), MAJOR)
               R(2,2) = MIN (R(2,2), MINOR)
               R(2,3) = MAX (R(2,3), MINOR)
               R(3,2) = MIN (R(3,2), PA)
               R(3,3) = MAX (R(3,3), PA)
 50            CONTINUE
C
 999  RETURN
      END
      SUBROUTINE BWSMCB (DX, DY, DR, BWS, CB, SMCB)
C-----------------------------------------------------------------------
C   convolves the Clean beam for bandwidth smearing
C   Inputs:
C      DX     R      Component X pixel from ref
C      DY     R      Component Y pixel from ref
C      DR     D      Component radius from reference
C      BWS    R      Smearing factor
C      CB     R(3)   Central Clean beam
C   Output
C      SMCB   R(3)   Effective Clean beam at component
C-----------------------------------------------------------------------
      REAL     DX, DY, BWS, CB(3), SMCB(3)
      DOUBLE PRECISION DR
C
      REAL     SMGAUS(3), SMGAUD(3)
      INTEGER  IERR
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
C                                       No smearing
      IF ((BWS.LE.0.0) .OR. ((DX.EQ.0.0) .AND. (DY.EQ.0.0))) THEN
         SMCB(1) = CB(1)
         SMCB(2) = CB(2)
         SMCB(3) = CB(3)
C                                       Smearing
      ELSE
         SMGAUD(1) = DR * BWS
         SMGAUD(2) = SMGAUD(1) / 1000.0
         SMGAUD(3) = ATAN2 (DX * AXINC(1,LOCNUM), DY * AXINC(2,LOCNUM))
     *      * RAD2DG
         CALL ELIPSQ (SMGAUD(1), SMGAUD(2), SMGAUD(3), -AXINC(1,LOCNUM),
     *      AXINC(2,LOCNUM), SMGAUS(1), SMGAUS(2), SMGAUS(3))
         SMGAUS(2) = 0.0
         CALL RECONV (CB(1), CB(2), CB(3), SMGAUS(1), SMGAUS(2),
     *      SMGAUS(3), SMCB(1), SMCB(2), SMCB(3), IERR)
         IF (IERR.NE.0) THEN
            SMCB(1) = CB(1)
            SMCB(2) = CB(2)
            SMCB(3) = CB(3)
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE DOPRT
C-----------------------------------------------------------------------
C   DOPRT calls MSGWRT and optionally PRTLIN with MSGTXT
C   In/Out - COMMON
C      XPR   L   Do printing?
C-----------------------------------------------------------------------
C
      INTEGER   PERR
      INCLUDE 'IMFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       message file in any case
      CALL MSGWRT (4)
C                                       print file
      IF ((XPR) .AND. (XCRT.GE.-3.5)) THEN
         CALL PRTLIN (LUNP, INDP, DOCRT, NACROS, TITL1, TITL2, MSGTXT,
     *      ILINE, IPAGE, SCRTCH, PERR)
         IF (PERR.NE.0) XPR = .FALSE.
         END IF
C
 999  RETURN
      END
      SUBROUTINE ORPRT
C-----------------------------------------------------------------------
C   ORPRT does the DOCRT=-4 special print format to file
C-----------------------------------------------------------------------
      INTEGER   SEQ, CVOL, CUID, JTRIM, IL, IPA, IPE, IBPA, IERR, I,
     *   IROUND, J
      CHARACTER CNAME*12, CCLAS*6, CPTYPE*2, TLINE*156, PCODE*1,
     *   ACODE*1, AUNIT*2, RCODE*1
      REAL      PEAK, XINC, YINC, XP, YP, AXMAJ, AXMIN, BMAJ, BMIN,
     *   PRMS, SCALE, BMFACT
      INCLUDE 'IMFIT.INC'
      INCLUDE 'ORFIT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      BMFACT = 0.0
      IF (HCB(1)*HCB(2).GT.0.0) BMFACT = UCB(1)*UCB(2) / (HCB(1)*HCB(2))
      IF (BMFACT.LE.0.0) BMFACT = 1.0
C                                       name manipulation
      CALL WAWA2A (INNA, CNAME, CCLAS, SEQ, CPTYPE, CVOL, CUID)
      IL = JTRIM (CNAME)
      DO 10 I = 1,IL
         IF (CNAME(I:I).EQ.' ') CNAME(I:I) = '.'
 10      CONTINUE
      IF (CCLAS(5:6).NE.' ') CNAME(11:12) = CCLAS(5:6)
      XINC = CATR(KRCIC)
      YINC = CATR(KRCIC+1)
      ACODE = ' '
      SCALE = 1.0
      AUNIT = ' '
      IF (AXTYP(LOCNUM).EQ.1) THEN
         XINC = XINC * 3600.0
         YINC = YINC * 3600.0
         UCB(1) = UCB(1) * 3600.0
         UCB(2) = UCB(2) * 3600.0
         IF (MAX(ABS(XINC),ABS(YINC)).LT.0.1) THEN
            SCALE = 1000.0
            ACODE = 'm'
            END IF
         AUNIT = 'as'
         END IF
      BMAJ = UCB(1) * SCALE
      BMIN = UCB(2) * SCALE
      IBPA = IROUND (UCB(3))
      PRMS = ACTRMS * BMFACT
      IF (PRMS.LT.1.E-3) THEN
         RCODE = 'u'
         PRMS = PRMS * 1.E6
      ELSE IF (PRMS.LT.1.0) THEN
         RCODE = 'm'
         PRMS = PRMS * 1.E3
      ELSE IF (PRMS.LT.1.E3) THEN
         RCODE = ' '
      ELSE
         RCODE = 'k'
         PRMS = PRMS * 1.E-3
         END IF
      J = 0
      IF (.NOT.OREXIS) THEN
         WRITE (TLINE,1000) ACODE, AUNIT, ACODE, AUNIT, ACODE, AUNIT,
     *      ACODE, AUNIT
         CALL ZTXIO ('WRIT', LUNP, INDP, TLINE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1015) IERR, 0
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
      DO 50 I = 1,NGAUSS
         IF (NGAUSS.GT.1) J = I
         PEAK = G(1,I) * BMFACT
         IF (PEAK.GT.0.0) THEN
            IF ((PEAK.LT.1.E-3) .AND. (ORERRA(I).LT.1.E-4)) THEN
               PCODE = 'u'
               PEAK = PEAK * 1.E6
               ORERRA(I) = ORERRA(I) * 1.E6
            ELSE IF ((PEAK.LT.1.0) .AND. (ORERRA(I).LT.0.1)) THEN
               PCODE = 'm'
               PEAK = PEAK * 1.E3
               ORERRA(I) = ORERRA(I) * 1.E3
            ELSE IF ((PEAK.LT.1.E3) .AND. (ORERRA(I).LT.100.)) THEN
               PCODE = ' '
            ELSE
               PCODE = 'k'
               PEAK = PEAK * 1.E-3
               ORERRA(I) = ORERRA(I) * 1.E-3
               END IF
         ELSE
            IF ((ABS(PEAK).LT.1.E-4) .AND. (ORERRA(I).LT.1.E-4)) THEN
               PCODE = 'u'
               PEAK = PEAK * 1.E6
               ORERRA(I) = ORERRA(I) * 1.E6
            ELSE IF ((ABS(PEAK).LT.0.1) .AND. (ORERRA(I).LT.0.1)) THEN
               PCODE = 'm'
               PEAK = PEAK * 1.E3
               ORERRA(I) = ORERRA(I) * 1.E3
            ELSE IF ((ABS(PEAK).LT.1.E2) .AND. (ORERRA(I).LT.100.)) THEN
               PCODE = ' '
            ELSE
               PCODE = 'k'
               PEAK = PEAK * 1.E-3
               ORERRA(I) = ORERRA(I) * 1.E-3
               END IF
            END IF
         XP = (G(2,I) - CATR(KRCRP)) * XINC * SCALE
         YP = (G(3,I) - CATR(KRCRP+1)) * YINC * SCALE
         ORERRX(I) = ORERRX(I) * ABS(XINC) * SCALE
         ORERRY(I) = ORERRY(I) * ABS(YINC) * SCALE
         AXMAJ = ORBMAJ(I) * SCALE
         AXMIN = ORBMIN(I) * SCALE
         ORERMA(I) = ORERMA(I) * SCALE
         ORERMI(I) = ORERMI(I) * SCALE
         IPA = IROUND (ORBPA(I))
         IPE = IROUND (ORERFI(I))
         WRITE (TLINE,1010) CNAME, DEPTH(1), PEAK, ORERRA(I), PCODE, XP,
     *      ORERRX(I), YP, ORERRY(I), REFRA(:2), REFRA(4:5),
     *      REFRA(7:13), REFDEC(:3), REFDEC(5:6), REFDEC(8:14),
     *      AXMAJ, ORERMA(I), AXMIN, ORERMI(I), IPA, IPE, BMAJ, BMIN,
     *      IBPA, PRMS, RCODE, J
         CALL ZTXIO ('WRIT', LUNP, INDP, TLINE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1015) IERR, I
            CALL MSGWRT (8)
            GO TO 999
            END IF
 50      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT('!Source       Ch   Fnu _Jy Err    E-W Offset (',A1,A2,
     +   ')     N-S Offset (',A1,A2,
     *   ')       RA          DEC      Major(',A1,A2,')   Minor(',A1,A2,
     *   ') PA(deg)       Beam         RMS  #')
 1010 FORMAT (A12,I4,1X,2F7.3,A1,2(F12.3,F8.3),1X,2A2,A7,A3,A2,A7,
     *   2(F7.2,F6.2),I4,I3,F7.2,F6.2,I4,F7.2,A1,I2)
 1015 FORMAT ('ZTXIO ERROR',I4,' WRITING COMPONENT',I2)
      END
