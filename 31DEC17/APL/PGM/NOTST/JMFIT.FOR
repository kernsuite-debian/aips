LOCAL INCLUDE 'JMFIT.INC'
C                                       Local include for JMFIT
      CHARACTER TITL1*132, TITL2*132, SCRTCH*132, INNA*36, OUTNA*36,
     *   LPNAME*48
      INTEGER   OUTBLK(256), LUN1, INSL, OUTSL, DEPTH(5), NGAUSS, NITER,
     *   XTYPE(4), IVAR(24), JVAR(24), NX, NY, NPTS, PTMAX, PTMIN,
     *   NPARM, NVAR, LUNPRT, INDPRT, LUNP, INDP, ILINE, IPAGE, OUTVER,
     *   NACROS, FREQAX, INVOL, INBLK(256)
C   The maximum number of pixels that JMFIT uses is encapsulated right
C   here. Just increase MAXPTS to whatever level is desired. The only
C   proper way to do this however is to rearrange FXDVD so that the
C   double precision vector RESID is no longer required
      INTEGER   MAXPTS
      PARAMETER (MAXPTS=10000)
      INCLUDE 'INCS:PMAD.INC'
      REAL      WIN(4), E(6,4), CB(3), DMAX, DMIN, OFFSET, DOCRT,
     *   ACTRMS, DATA(MAXPTS), RSCALE, SUMSQ, RESSUM, RESMAX,
     *   XCEN, YCEN, LINE(MABFSS), XU, HCB(3), UCB(3), HCBP(3), FACTOR
      DOUBLE PRECISION RESID(MAXPTS), G(6,4), XRA, XDEC, XFREQ
      LOGICAL   NWIDTH(4), XPR
      COMMON /JMFCHR/ INNA, OUTNA, LPNAME, TITL1, TITL2, SCRTCH
      COMMON /JMFPRM/ RESID, G, XRA, XDEC, XFREQ, OUTBLK, INBLK, DATA,
     *   NVAR, INDPRT, DMAX, DMIN, OFFSET, DOCRT, LUNP, INDP, ILINE,
     *   IPAGE, NX, NY, NPTS, PTMAX, PTMIN, NPARM, ACTRMS, WIN, LUN1,
     *   INSL, OUTSL, INVOL, DEPTH, E, CB, NGAUSS, NITER, XTYPE, IVAR,
     *   JVAR, NWIDTH, RSCALE, OUTVER, SUMSQ, RESSUM, RESMAX, XCEN,
     *   YCEN, LINE, XU, HCB, UCB, HCBP, FREQAX, LUNPRT, NACROS, XPR
C                                       inputs
      REAL      XSE, XDI, XBL(7), XTR(7), XOS, XOD, XNG, XTY(4), GM(4),
     *   GP(2,4), GW(3,4), XDM(4), XDP(2,4), XDW(3,4), BWS, RADIUS, XNI,
     *   XCRT, XDOO, XOFS, XDOM, XOVER, XSTVER, XPLV, PBPARM(7)
      HOLLERITH XNA(3), XCL(2), XON(3), XOC(2), XOP(12)
      COMMON /INPARM/ XNA, XCL, XSE, XDI, XBL, XTR, XON, XOC, XOS,
     *   XOD, XNG, XTY, GM, GP, GW, XDM, XDP, XDW, BWS, RADIUS, XNI,
     *   XCRT, XOP, XDOO, XOFS, XDOM, XOVER, XSTVER, XPLV, PBPARM,
     *   FACTOR
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
      PROGRAM JMFIT
C-----------------------------------------------------------------------
C! Fits Gaussians to an image
C# Map Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2001-2005, 2007-2015
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
C   JMFIT is a two-dimensional Gaussian fitting program for image
C   modelling.  Up to four Gaussian components can be fit to a part of
C   an image.
C   Inputs:
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
C      NGAUSS                   Number of Gaussians to fit
C      XTYPE(4)                 Type of model component
C      GMAX(4)                  Peak intensity
C      GPOS(2,4)                (X,Y) Position in pixels
C      GWIDTH(3,4)              (BMAJ,BMIN,BPA) in pixels, pixels
C                                   degrees
C      NITER                    Number of loop iterations
C      DoMAX(4)                 Peak intensity variability
C      DOPOS(2,4)               (X,Y) Position variability
C      DOWIDTH(3,4)             (BMAJ,BMIN,BPA) variability
C      DOCRT                    Print output on Lineprinter?
C      DOOUTPUT                 Catalog residual map?
C      OFFSET                   Ignore values below OFFSET*PEAK
C      DOMODEL                  Make a CC with input file
C
C     COMMON /IMFIO/ OUTBLK, INNA, OUTNA, LUN1, WIN, INSL, OUTSL, DEPTH
C        OUTBLK(256)   I          Output catalog header
C        INNA          C*36       WaWa input map name
C        OUTNA         C*36       WaWa output map name
C        WIN(4)        R          Window from MAPWIN
C        LUN1          I          Logical unit # for input
C        INSL          I          Slot number of INNA
C        OUTSL         I          Slot number of OUTNA
C        DEPTH(5)      I          Axis position axes on 3-7
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
C                    PTMAX, PTMIN, NPARM
C        DATA(MAXPTS)  R          Data point values
C        DMAX          R          Maximum value of data
C        DMIN          R          Minimum value of data
C        OFFSET        R          Disregard points below OFFSET*DMAX
C        NX            I          Number of columns
C        NY            I          Number of rows
C        MAXPTS        I          Compiled limit on number of points
C        NPTS          I          Number of points
C        PTMAX         I          Data point at maximum
C        PTMIN         I          Data point at minimum
C        NPARM         I          Number of fitting iterations
C-----------------------------------------------------------------------
      EXTERNAL  FXDVD
      CHARACTER PRGNAM*6
      INTEGER   IER, IERR, INF, NK, I, J, K, IRET, NPR, NTOT, IPTLEV
      REAL      DOMODL, RSTO, DSCALE, RMS, FAC
      DOUBLE PRECISION    VALVAR(24), TEMP(24), EPS, FOPT, GNOPT,
     *   ERRDVD(24), DNRM2
      INCLUDE 'JMFIT.INC'
      REAL      BUFF(MAXPTS)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGNAM /'JMFIT '/
C-----------------------------------------------------------------------
      IRET = 8
C                                        Some initialization
      NPARM = 111
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
C                                        Get inputs, open map
      XU = NLUSER
      CALL JMFOPN (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 900
         END IF
C                                        Print and store flags
      RSTO = XDOO
      OFFSET = XOFS
      DOMODL = XDOM
      CALL JMFDAT (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 900
         END IF
C                                        Insert defaults
      CALL JMFDEF
C                                        Determine scale factor
      DSCALE = ABS (DMAX / RSCALE)
      DSCALE = 10.0 ** (AINT (LOG10 (DSCALE/2.5) + 25.0) - 26.0)
      IF (RSCALE.LT.0) DSCALE = -DSCALE
      OUTVER = XOVER + 0.1
      IF (XOVER.LT.0.0) OUTVER = -1
C                                        Print out input model
      XPR = XCRT.NE.0.0
      CALL JMFPRT (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 900
         END IF
C                                        Print out map data
      IF (XPR) CALL JMPLOT (1, DSCALE)
C                                       Close input map
      CALL FILCLS (LUN1)
C                                        Store variables properly
      CALL JMFVST (NGAUSS, G, E, IVAR, JVAR, NVAR, VALVAR)
C                                        Save map data in a buffer
      DO 60 I = 1,NPTS
         BUFF(I) = DATA(I)
 60      CONTINUE
C                                        Calculate model data
      CALL FXDVD (VALVAR, FOPT, TEMP, 2)
C                                        Print out model data
C                                        and count points included
C                                        in the fit:
      NK = 0
      DO 65 I = 1,NPTS
         IF (DATA(I).NE.FBLANK) THEN
            NK = NK + 1
            DATA(I) = RESID(NK) + DATA(I)
            END IF
 65      CONTINUE
      NTOT = NK
      IF (XPR) CALL JMPLOT (2, DSCALE)
C                                        Restore data from buffer
      SUMSQ = 0.0
      DO 70 I = 1,NPTS
         IF (DATA(I).NE.FBLANK) SUMSQ = SUMSQ + (DATA(I) - BUFF(I))**2
         DATA(I) = BUFF(I)
 70      CONTINUE
      IF (NTOT.GT.0) SUMSQ = SQRT (SUMSQ / NTOT)
      SUMSQ = SUMSQ / RSCALE
      MSGTXT = ' '
      IF (DOCRT.GT.-2.5) THEN
         CALL DOPRT
      ELSE
         CALL MSGWRT (4)
         END IF
      WRITE (MSGTXT,1070) SUMSQ, NTOT
      CALL DOPRT
C                                        Print value of chi-sq.,
C                                        r.m.s., and gradient
C                                        norm that correspond to
C                                        initial guess:
      IPTLEV = XPLV + 0.1
      FAC = 1.0
      IF (NTOT.GT.NVAR) FAC = SQRT (REAL(NTOT) / REAL(NTOT-NVAR))
      WRITE (MSGTXT,1190) NTOT, NVAR
      IF (DOCRT.GT.-2.5) THEN
         CALL DOPRT
      ELSE
         CALL MSGWRT (4)
         END IF
      GNOPT = DNRM2 (NVAR, TEMP, 1)
      GNOPT = GNOPT / RSCALE
      FOPT = FOPT / (RSCALE * RSCALE)
      RMS = SQRT (FOPT/NTOT) * FAC / RSCALE
      MSGTXT = 'Initial Chi-squared, R.M.S., and gradient norm:'
      IF (DOCRT.GT.-2.5) THEN
         CALL DOPRT
      ELSE
         CALL MSGWRT (4)
         END IF
      WRITE (MSGTXT,1120) FOPT, RMS, GNOPT
      IF (DOCRT.GT.-2.5) THEN
         CALL DOPRT
      ELSE
         CALL MSGWRT (4)
         END IF
C                                        Call fitting subroutine.
C                                        Its progress report messages
C                                        are sent to the line printer
C                                        if the user has set DOVECT
C                                        to true; otherwise they're
C                                        sent to the terminal. These
C                                        messages are important, so
C                                        there's no way to turn them
C                                        off.
C                                        (I use my own fitting rou-
C                                        tine, DVDMIN, because of its
C                                        simplicity, but others - e.g.
C                                        MINPACK routines - could be
C                                        used instead.   -- F. Schwab)
      DO 71 I = 1,NVAR
         ERRDVD(I) = 1.0D0
 71      CONTINUE
      EPS = 1.0D-15
      NPR = 2
      CALL DVDMIN (FXDVD, VALVAR, ERRDVD, NVAR, EPS, NITER, FOPT, GNOPT,
     *   INF, NPR, IPTLEV)
C                                        Stop if fitting routine
C                                        wasn't happy
      IF (INF.NE.0) GO TO 800
C                                        Get residual data
      NK = 0
      SUMSQ = 0.0
      RESSUM = 0.0
      RESMAX = 0.0
      DO 80 I = 1,NPTS
         IF (DATA(I).NE.FBLANK) THEN
            NK = NK + 1
            DATA(I) = -RESID(NK)
            SUMSQ = SUMSQ + DATA(I)**2
            RESSUM = RESSUM + DATA(I)
            RESMAX = MAX (RESMAX, ABS (DATA(I)))
            END IF
 80      CONTINUE
      RESSUM = RESSUM / RSCALE
      RESMAX = RESMAX / RSCALE
      IF (CB(1)*CB(2).NE.0.0) THEN
         RESSUM = RESSUM / (1.1331 * CB(1) * CB(2))
      ELSE IF (NK.NE.0) THEN
         RESSUM = RESSUM / NK
         END IF
      IF (NK.GT.0) SUMSQ = SQRT (SUMSQ / NK)
      SUMSQ = SUMSQ / RSCALE
      WRITE (MSGTXT,1080) SUMSQ, NK
      CALL DOPRT
C                                        Print goodness-of-fit info
C                                        and apply scale factor to
C                                        obtain standard error
C                                        estimates.
      FOPT = FOPT / (RSCALE * RSCALE)
      RMS = SQRT (FOPT/NTOT) * FAC
      WRITE (MSGTXT,1130)
      IF (DOCRT.GT.-2.5) THEN
         CALL DOPRT
      ELSE
         CALL MSGWRT (4)
         END IF
      GNOPT = GNOPT / RSCALE
      WRITE (MSGTXT,1120) FOPT, RMS, GNOPT
      IF (DOCRT.GT.-2.5) THEN
         CALL DOPRT
      ELSE
         CALL MSGWRT (4)
         END IF
      K = 0
      DO 84 I = 1,NGAUSS
         DO 83 J = 1,6
            IF (E(J,I).GT.0.) THEN
               K = K + 1
               E(J,I) = RMS * ERRDVD(K)
            ELSE
               E(J,I) = 0.0
               END IF
 83         CONTINUE
 84      CONTINUE
C                                        Residual plot?
      IF (XPR) CALL JMPLOT (3, DSCALE)
C                                       Store residual map
      IF (RSTO.GT.0.0) THEN
         CALL JMFSTO (IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1090) IERR
            GO TO 900
            END IF
         END IF
C                                       restore data for tweak method
C                                       of estimating errors
      CALL JMFERR (BUFF, VALVAR, FOPT, TEMP)
C                                        Plot the final model. Have
C                                        to recalculate - otherwise
C                                        we would need another data
C                                        array:
C
C                                        First, zero the data:
      IF (XPR) THEN
         DO 110 I = 1,NPTS
            DATA(I) = 0.0
 110        CONTINUE
C                                        Get the model & stuff it
C                                        in appropriate array:
         CALL FXDVD (VALVAR, FOPT, TEMP, 1)
         DO 120 I = 1,NPTS
            DATA(I) = RESID(I)
 120        CONTINUE
C                                        Plot it:
         CALL JMPLOT (4, DSCALE)
         END IF
C                                        Convert to useful form
      IF (NVAR.NE.0) THEN
         DO 125 I = 1,NGAUSS
            G(1,I) = G(1,I) / RSCALE
            E(1,I) = E(1,I) / RSCALE
            IF (XTYPE(I).EQ.2) THEN
               G(2,I) = G(2,I) / RSCALE
               G(4,I) = G(4,I) / RSCALE / RSCALE
               E(2,I) = E(2,I) / RSCALE
               E(4,I) = E(4,I) / RSCALE / RSCALE
               END IF
 125        CONTINUE
         CALL JMFOUT (RSTO, DOMODL, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR
            GO TO 900
            END IF
         END IF
C                                        Finished, no errors
      IRET = 0
      GO TO 980
 800  MSGTXT = '***  FAILURE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  ***'
      CALL MSGWRT (8)
      WRITE (MSGTXT,1800) NITER
      CALL MSGWRT (8)
      MSGTXT = '   INCREASE NITER, ALTER YOUR INITIAL GUESSES, CHANGE'
      CALL MSGWRT (8)
      MSGTXT = '   THE SIZE OF THE DATA WINDOW, OR CHANGE THE VALUE'
      CALL MSGWRT (8)
      MSGTXT = '   OF CUTOFF  -  AND TRY AGAIN!!'
      CALL MSGWRT (8)
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
 1020 FORMAT ('ERROR FROM JMFOPN.  IER=',I7)
 1030 FORMAT ('COUNT NOT STORE MAP DATA.  IER=',I7)
 1040 FORMAT ('COULD NOT PRINT INPUT MODEL.  IER=',I7)
 1070 FORMAT ('Initial guess RMS',1PE12.4,' in',I5,' usable pixels')
 1080 FORMAT ('Solution RMS',1PE12.4,' in',I5,' usable pixels')
 1090 FORMAT ('COULD NOT STORE RESID MAP.  IER=',I7)
 1100 FORMAT ('COULD NOT PRINT CONVERTED MODEL.  IER=',I7)
 1120 FORMAT (10X,1P3E15.5)
 1130 FORMAT ('Post-fit Chi-squared, R.M.S., and gradient norm:')
 1800 FORMAT ('   CONVERGENCE WASN''T ACHIEVED IN',I5,' ITERATIONS.')
 1190 FORMAT ('Fitting to',I5,' data points for',I3,' parameters')
      END
      SUBROUTINE JMFERR (BUFF, VALVAR, FOPT, TEMP)
C-----------------------------------------------------------------------
C   Does estimate of uncertainties for baseline fits only by trying
C   plus/minus one guessed sigma with each parameter
C   Inputs:
C      BUFF     R(*)    Original data - gets restored to DATA
C      VALVAR   D(24)   Parameter values
C   Output:
C      FOPT     D       Chi squared
C      TEMP     D(24)   Gradient of chi-squared
C   Common
C      DATA     R(*)    Residual on input, initial data on output
C-----------------------------------------------------------------------
      REAL      BUFF(*)
      DOUBLE PRECISION VALVAR(*), FOPT, TEMP(*)
C
      INTEGER   I, J, NK, II, K2, K3
      REAL      RM(3), DIFF, ERR, CURV, RTEMP, CBAREA, RMS, SUM2, NIND
      INCLUDE 'JMFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       find rms
      CBAREA = CB(1) * CB(2)
      IF (CBAREA.LT.0.5) CBAREA = 10.
      SUM2 = 0.0
      NK = 0
      DO 20 I = 1,NPTS
         IF (DATA(I).NE.FBLANK) THEN
            NK = NK + 1
            SUM2 = SUM2 + DATA(I)*DATA(I)
            END IF
 20      CONTINUE
C                                       restore image data
      DO 30 I = 1,NPTS
         DATA(I) = BUFF(I)
 30      CONTINUE
C                                       bail if nothing
      IF ((NVAR.LE.0) .OR. (NK.LT.2) .OR. (NK.LE.NVAR)) GO TO 999
      RMS = SQRT (SUM2 / (NK - NVAR))
C                                       loop over parameters
      DO 100 II = 1,NVAR
         I = IVAR(II)
         J = JVAR(II)
C                                       do only baselines
         IF (XTYPE(I).EQ.2) THEN
C                                       guess rms
            IF (J.EQ.1) THEN
               E(J,I) = RMS
            ELSE IF (J.EQ.2) THEN
               E(J,I) = RMS / 5.
            ELSE IF (J.EQ.3) THEN
               IF (G(2,I).NE.0.0) E(J,I) = RMS / ABS (G(2,I))
               E(J,I) = MIN (1.0, E(J,I))
            ELSE IF (J.EQ.4) THEN
               E(J,I) = RMS / 10.0
            ELSE IF (J.EQ.5) THEN
               IF (G(4,I).NE.0.0) E(J,I) = RMS / ABS (G(4,I))
               E(J,I) = MIN (1.0, E(J,I))
            ELSE IF (J.EQ.6) THEN
               IF (G(4,I).NE.0.0) E(J,I) = RMS / ABS (G(4,I))
               E(J,I) = MIN (0.7, E(J,I))
               END IF
C                                        Set variable value for loop
            VALVAR(II) = VALVAR(II) - 2*E(J,I)
C                                        Find RMS
            DO 50 K2 = 1,3
C                                        Change values
               VALVAR(II) = VALVAR(II) + E(J,I)
               CALL FXDVD (VALVAR, FOPT, TEMP, 2)
C                                        Find the RMS
               SUM2 = 0.
               NK = 0.
               DO 40 K3 = 1,NPTS
                  IF (DATA(K3).NE.FBLANK) THEN
                     NK = NK + 1
                     SUM2 = SUM2 + RESID(NK)*RESID(NK)
                     END IF
 40               CONTINUE
               RM(K2) = SQRT (SUM2 / NK)
 50            CONTINUE
            VALVAR(II) = VALVAR(II) - E(J,I)
C                                        Find RMS well
            DIFF = 10.0
            RTEMP = RM(1) - 2.0 * RM(2) + RM(3)
            IF (RTEMP.GT.0.0) THEN
               DIFF = 0.5 * (RM(1) - RM(3)) / RTEMP
               ERR = RM(2) - 0.125 * (RM(1) - RM(3))**2 / RTEMP
               CURV = 0.5 * RTEMP
               END IF
C                                        Get value at RMS minimum
            IF (ABS(DIFF).LT.1.5) THEN
               NIND = 4.0 * NK / CBAREA
               DIFF = ERR / NIND
               E(J,I) = SQRT (DIFF/CURV) * E(J,I)
            ELSE
               WRITE (MSGTXT,1050) I, J
               CALL DOPRT
               END IF
            END IF
 100     CONTINUE
      CALL FXDVD (VALVAR, FOPT, TEMP, 2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('JMFERR: CANNOT FIND MINIMUM AT COMP',I2,' PARM',I2)
      END
      SUBROUTINE FXDVD (P, F, GRAD, IFLAG)
C-----------------------------------------------------------------------
C  Given the vector P of solution parameters, this subroutine computes
C  the value of the chi-squared function F (a sum of squared residuals),
C  and, optionally, the gradient, GRAD, of F w.r.t. P.  When IFLAG=1,
C  only F is computed.  Otherwise F and GRAD are both computed.  Note
C  that P is to contain only the parameters which are being solved for
C  --- not the parameters that are to be held fixed.  This subroutine is
C  called by the minimization routine DVDMIN.
C
C  Additionally, the residuals (model minus data) are stored in the
C  labeled COMMON/FRED/ array RESID for use outside the minimization
C  routine proper.  (The minimization routine DVDMIN doesn't need to
C  know the residuals, it only needs F and GRAD).  The data points, the
C  information on which parameters are being held fixed, etc., come
C  into this routine through labeled COMMONs.
C
C  Inputs:
C    P(NVAR)    D    Vector of least-squares solution pararameters.
C    IFLAG      I    IFLAG=1 ==> compute just F,
C                    IFLAG.NE.1 ==> compute both F and GRAD.
C
C  Outputs:
C    F          D    The value of the chi-squared function corresponding
C                    to the given P.
C    GRAD(NVAR) D    The gradient of the chi-squared function.  I.e.,
C                    GRAD(I) = derivative of F w.r.t. P(I).
C
C  Outputs (in labeled COMMON):
C    RESID(NPTS) D   The residuals.
C-----------------------------------------------------------------------
      INTEGER   NK, IFLAG, I, J, K, IX1, IY1, NTOT, L
      REAL   STH2, CTH2, S2TH, C2TH, MJ, MN, VA, VB, VC, VD, F1,
     *   X, Y, X2, Y2, XY, CON, TWORFV, TWOCON, SINP, COSP, SINP2,
     *   COSP2, SIN2P, EL, ELC, CURV, SINT, COST, SL, ZERO, FV, G4C,
     *   G5C, TWORSD
      DOUBLE PRECISION P(*), F, GRAD(*), GTEMP(24), DNRM2
      INCLUDE 'JMFIT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA CON/2.772589/
C-----------------------------------------------------------------------
      TWOCON=2.0*CON
      NPTS = NX*NY
      NK = 0
      DO 10 K = 1, NPTS
         IF (DATA(K).EQ.FBLANK) GO TO 10
            NK = NK+1
            RESID(NK) = -DATA(K)
 10      CONTINUE
      NTOT = NK
      DO 20 I = 1, NVAR
         G(JVAR(I),IVAR(I)) = P(I)
 20      CONTINUE
C
C  For the Ith Gaussian component,
C         G(1,I) = the peak amplitude of the component,
C         G(2,I) = x-position,
C         G(3,I) = y-position,
C         G(4,I) = major axis fwhm,
C         G(5,I) = minor axis fwhm,
C    and, G(6,I) = position angle of the major axis, normally
C                  measured from North through East.
C  For the baseline component (say, the Jth component),
C         G(1,J) = zero-order term,
C         G(2,J) = baseline slope (ampl/pixel) (Fomalonts's nomencl.),
C         G(3,J) = orientation of slope (radians),
C         G(4,J) = major axis curvature (ampl**2/pixel),
C         G(5,J) = ``curvature eccentricity,''
C    and, G(6,J) = orientation of major axis curvature.
C
      IX1 = WIN(1) + 0.5
      IY1 = WIN(2) + 0.5
      K = -6
      DO 80  I=1, NGAUSS
         K = K+6
         IF (XTYPE(I).NE.1) GO TO 30
            IF (G(1,I).EQ.0.0) G(1,I) = 1.E-3
            IF (G(4,I).EQ.0.0) G(5,I) = 1.E-3
            IF (G(5,I).EQ.0.0) G(4,I) = 1.E-3
            STH2 = SIN(G(6,I))**2
            CTH2 = COS(G(6,I))**2
            S2TH = -SIN(2.0*G(6,I))
            C2TH = COS(2.0*G(6,I))
            MJ = G(4,I)**2/CON
            MN = G(5,I)**2/CON
            VA = CTH2/MJ+STH2/MN
            VB = STH2/MJ+CTH2/MN
            VC = S2TH*(1.0/MN-1.0/MJ)
            GO TO 40
 30      CONTINUE
            SINP = SIN(G(6,I))
            COSP = COS(G(6,I))
            SINP2 = SINP**2
            COSP2 = COSP**2
            SIN2P = 2.0*SINP*COSP
            EL = G(5,I)
            ELC = 1.0-EL
            CURV = G(4,I)
            SINT = SIN(G(3,I))
            COST = COS(G(3,I))
            SL = G(2,I)
            ZERO = G(1,I)
 40      NK = 0
         DO 70  L = 1, NPTS
            IF (DATA(L).EQ.FBLANK) GO TO 70
               NK = NK+1
               IF (XTYPE(I).NE.1) GO TO  50
                  X = IX1+MOD(L-1,NX)-G(2,I)
                  Y = IY1+AINT((L-0.5)/NX)-G(3,I)
                  FV = G(1,I)*EXP(-((VA*X+VC*Y)*X+VB*Y**2))
                  GO TO 60
 50            CONTINUE
                  X = MOD(L-1,NX) - 0.5*(NX-1)
                  Y = AINT((L-0.5)/NX) - 0.5*(NY-1)
                  FV = ZERO+SL*(X*COST+Y*SINT)
     *                +CURV*(X**2*(COSP2+EL*SINP2)+2.0*X*Y*SIN2P*ELC
     *                      +Y**2*(SINP2+EL*COSP2))
 60            RESID(NK) = RESID(NK)+FV
 70         CONTINUE
 80      CONTINUE
      F = DNRM2 (NTOT, RESID, 1)**2
      IF (IFLAG.EQ.1) RETURN
      DO 90 I = 1, 24
         GTEMP(I) = 0D0
 90      CONTINUE
      K = -6
      DO 140 I=1, NGAUSS
         K = K+6
         IF (XTYPE(I).NE.1) GO TO 100
            STH2 = SIN(G(6,I))**2
            CTH2 = COS(G(6,I))**2
            S2TH = -SIN(2.0*G(6,I))
            C2TH = COS(2.0*G(6,I))
            MJ = G(4,I)**2/CON
            MN = G(5,I)**2/CON
            G4C = TWOCON/G(4,I)**3
            G5C = TWOCON/G(5,I)**3
            VA = CTH2/MJ+STH2/MN
            VB = STH2/MJ+CTH2/MN
            VC = S2TH*(1.0/MN-1.0/MJ)
            VD = C2TH*(1.0/MN-1.0/MJ)
            GO TO 110
 100     CONTINUE
            SINP = SIN(G(6,I))
            COSP = COS(G(6,I))
            SINP2 = SINP**2
            COSP2 = COSP**2
            SIN2P = 2.0*SINP*COSP
            EL = G(5,I)
            ELC = 1.0-EL
            CURV = G(4,I)
            SINT = SIN(G(3,I))
            COST = COS(G(3,I))
            SL = G(2,I)
            ZERO = G(1,I)
 110     NK = 0
         DO 130 L = 1, NPTS
            IF (DATA(L).EQ.FBLANK) GO TO 130
               NK = NK+1
               IF (XTYPE(I).NE.1) GO TO 120
                  X = IX1+MOD(L-1,NX)-G(2,I)
                  Y = IY1+AINT((L-0.5)/NX)-G(3,I)
                  X2 = X**2
                  Y2 = Y**2
                  XY = X*Y
                  FV = G(1,I)*EXP(-(VA*X2+VB*Y2+VC*XY))
                  TWORFV = 2.0*RESID(NK)*FV
                  IF (E(1,I).GE.0.) GTEMP(K+1) = GTEMP(K+1)
     *               +TWORFV/G(1,I)
                  IF (E(2,I).GE.0.) GTEMP(K+2) = GTEMP(K+2)
     *               +TWORFV*(2.0*X*VA+Y*VC)
                  IF (E(3,I).GE.0.) GTEMP(K+3) = GTEMP(K+3)
     *               +TWORFV*(2.0*Y*VB+X*VC)
                  IF (E(4,I).GE.0.) GTEMP(K+4) = GTEMP(K+4) + TWORFV *
     *               G4C*(X2*CTH2+Y2*STH2-XY*S2TH)
                  IF (E(5,I).GE.0.) GTEMP(K+5) = GTEMP(K+5) + TWORFV *
     *               G5C*(X2*STH2+Y2*CTH2+XY*S2TH)
                  IF (E(6,I).GE.0.) GTEMP(K+6) = GTEMP(K+6) + TWORFV *
     *               (VC*(X2-Y2)+2.0*VD*XY)
                  GO TO 130
 120           CONTINUE
                  X = MOD(L-1,NX) - 0.5*(NX-1)
                  Y = AINT((L-0.5)/NX) - 0.5*(NY-1)
                  X2 = X**2
                  XY = X*Y
                  Y2 = Y**2
                  F1 = X2*(COSP2+EL*SINP2)+2.0*XY*SIN2P*ELC
     *                +Y2*(SINP2+EL*COSP2)
                  TWORSD = 2.0*RESID(NK)
                  IF (E(1,I).GE.0.) GTEMP(K+1) = GTEMP(K+1) + TWORSD
                  IF (E(2,I).GE.0.) GTEMP(K+2) = GTEMP(K+2)
     *               +TWORSD*(X*COST+Y*SINT)
                  IF (E(3,I).GE.0.) GTEMP(K+3) = GTEMP(K+3)
     *               +TWORSD*SL*(Y*COST-X*SINT)
                  IF (E(4,I).GE.0.) GTEMP(K+4) = GTEMP(K+4)
     *               +TWORSD*F1
                  IF (E(5,I).GE.0.) GTEMP(K+5) = GTEMP(K+5)
     *               +TWORSD*CURV*(X2*SINP2-2.0*XY*SIN2P+Y2*COSP2)
                  IF (E(6,I).GE.0.) GTEMP(K+6) = GTEMP(K+6)
     *               +TWORSD*CURV*ELC*(SIN2P*(Y2-X2)+
     *               4.0*XY*(COSP2-SINP2))
 130        CONTINUE
 140     CONTINUE
      K = 0
      L = 0
      DO 160 I=1,NGAUSS
         DO 150 J=1,6
            L = L+1
            IF (E(J,I).LT.0.) GO TO 150
               K = K+1
               GRAD(K) = GTEMP(L)
 150        CONTINUE
 160     CONTINUE
      RETURN
      END
      SUBROUTINE JMFOPN (IER)
C-----------------------------------------------------------------------
C   checks the inputs, opens the input map, finds the true rms, and then
C   initializes the I/O
C   Outputs:
C      IER     I       Error return  0-> okay 1-> error return
C-----------------------------------------------------------------------
      INTEGER   IER
C
      INTEGER   IERR, IROUND, I, J, NS, IC, RHIS(1026), RHM, IX, IY,
     *   INF, NPR, LOCS, NUMKEY, KEYTYP, MSGSAV, IFIL, NRNO,
     *   CGBUFF(512), LUNCG, IRNO, CGKOLS(4), CGNUMV(4)
      LOGICAL   F
      HOLLERITH HOLMA
      CHARACTER KEYWRD*8
      REAL      BPA0, BLC(7), TRC(7), SUM, SUMS, VALUE, XC, YC, R, BMAJ,
     *   BMIN, BPA
      DOUBLE PRECISION VALVAR(3), EPS, ERRDVD(3), GNOPT, FOPT, X, XMIN
      EXTERNAL  FXDVD
      INCLUDE 'JMFIT.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IER = 0
      LUN1 = 16
      CALL RFILL (3, 0.0, CB)
      CALL H2CHR (48, 1, XOP, LPNAME)
C                                       Store name for easy I/O
      CALL CHR2H (4, 'MA  ', 1, HOLMA)
      CALL H2WAWA (XNA, XCL, XSE, HOLMA, XDI, XU, INNA)
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
      IF (XBL(3).GT.0) THEN
         WRITE (MSGTXT,2006) XBL(3)
         CALL MSGWRT (2)
         END IF
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
C                                       get full rms
            NS = 0
            SUM = 0.0
            SUMS = 0.0
            DO 20 J = 1,NY
               CALL MAPIO ('READ', LUN1, LINE, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1015) I, IERR
                  GO TO 900
                  END IF
C                                       Row loop, fill data
               DO 15 I = 1,NX
                  R = (I-XC)**2 + (J-YC)**2
                  IF ((LINE(I).NE.FBLANK) .AND. (LINE(I).NE.0.0) .AND.
     *               (R.LE.RADIUS)) THEN
                     SUM = SUM + LINE(I)
                     SUMS = SUMS + LINE(I) * LINE(I)
                     NS = NS + 1
                     END IF
 15               CONTINUE
 20            CONTINUE
            IF (NS.LE.1) THEN
               MSGTXT =
     *            'ENTIRE IMAGE PLANE IS BLANKED OR ZERO: STOPPING'
               IERR = 999
               GO TO 900
               END IF
            SUM = SUM / NS
            SUMS = SUMS / NS
            SUMS = SQRT (MAX (0.0, SUMS - SUM*SUM)) / 60.0
C                                       Now do a histogram
            CALL MAPWIN (LUN1, BLC, TRC, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR
               GO TO 900
               END IF
            CALL RFILL (1026, 0.0, RHIS)
            RHM = 0
            IC = 0
            DO 30 J = 1,NY
               CALL MAPIO ('READ', LUN1, LINE, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1015) I, IERR
                  GO TO 900
                  END IF
C                                       Row loop, fill data
               DO 25 I = 1,NX
                  R = (I-XC)**2 + (J-YC)**2
                  IF ((LINE(I).NE.FBLANK) .AND. (LINE(I).NE.0.0) .AND.
     *               (R.LE.RADIUS)) THEN
                     NS = (LINE(I) - SUM) / SUMS + 514.5
                     NS = MAX (1, MIN (1026, NS))
                     RHIS(NS) = RHIS(NS) + 1
                     END IF
 25               CONTINUE
 30            CONTINUE
C                                       locate peak
            DO 35 I = 2,1025
               IF (RHIS(I).GT.RHM) THEN
                  IC = I
                  RHM = RHIS(I)
                  END IF
               DATA(I-1) = RHIS(I)
 35            CONTINUE
C                                       half power
            RHM = RHM / 2
            IX = 0
            IY = 0
            NS = MIN (1025-IC, IC-1)
            DO 40 I = 1,NS
               IF (RHIS(IC-I).GE.RHM) IY = I
               IF (RHIS(IC+I).GE.RHM) IX = I
 40            CONTINUE
C                                       Fit it
            G(1,1) = 2 * RHM
            G(2,1) = IC - 1
            G(3,1) = 1.0
            G(4,1) = (IX + IY)
            IF (G(4,1).LE.0.0) G(4,1) = 1.0
            G(5,1) = 1.0
            G(6,1) = 0.0
            VALVAR(1) = G(1,1)
            VALVAR(2) = G(2,1)
            VALVAR(3) = G(4,1)
            JVAR(1) = 1
            JVAR(2) = 2
            JVAR(3) = 4
            CALL FILL (3, 1, IVAR)
            E(1,1) = 1.0
            E(2,1) = 1.0
            E(3,1) = -1.0
            E(4,1) = 1.0
            E(5,1) = -1.0
            E(6,1) = -1.0
            NGAUSS = 1
            NVAR = 3
            ERRDVD(1) = 10.
            ERRDVD(2) = 10.
            ERRDVD(3) = 10.
C                                       Call fitting routine
            NPR = 0
            NITER = 40
            EPS = 1.D-10
            NX = 1025
            NY = 1
            WIN(1) = 1
            WIN(2) = 1
            XTYPE(1) = 1
            CALL DVDMIN (FXDVD, VALVAR, ERRDVD, NVAR, EPS, NITER, FOPT,
     *         GNOPT, INF, NPR, IER)
C                                       Termination type
            IF (INF.NE.0) THEN
               MSGTXT = 'GET TRUE RMS: FIT FAILS!'
               CALL MSGWRT (7)
               ACTRMS = SUMS * 60.
            ELSE
               ACTRMS = SUMS * ABS(G(4,1)) / SQRT (8.0 * LOG(2.0))
                END IF
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
      IF (NITER.EQ.0) NITER = 20 * NGAUSS
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
C                                       fit maj, min, pos. ang.?
            NWIDTH(I) = NWIDTH(I) .AND. (E(J,I).LE.0)
 140        CONTINUE
 160     CONTINUE
C                                       Header Clean beam to pixels
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
 1000 FORMAT ('JMFOPN: COULD NOT OPEN INPUT MAP.  IER=',I7)
 1005 FORMAT ('JMFOPN: COULD NOT GET HEADER.  IER=',I7)
 1006 FORMAT ('True rms taken from RADIUS adverb =',1PE10.3)
 1007 FORMAT ('True rms taken from ACTNOISE in header =',1PE10.3)
 1010 FORMAT ('IMFOPN: COULD NOT MAPWIN.  IER=',I7)
 1015 FORMAT ('IMFDAT: COULD NOT READ LINE ',I7,'  IER=',I7)
 1040 FORMAT ('Fit to histogram gives RMS =',1PE10.3)
 1041 FORMAT ('Fit to histogram gives RMS =',1PE10.3,' in radius',
     *   0PF7.1,' pixels')
 1080 FORMAT ('JMFOPN: TOO MANY POINTS',I4,'X',I4,'=',I7,' POINTS')
 1160 FORMAT ('JMFOPN ERROR',I3,' ON ',A)
 1170 FORMAT ('Using Clean beam',2F10.5,F8.2,' from ',A)
 2006 FORMAT (10X, 'CHANNEL ', F4.0)
      END
      SUBROUTINE JMFDAT (IER)
C-----------------------------------------------------------------------
C   Subroutine JMFDAT reads the input data into the array DATA.
C   INPUTS:        none
C   OUTPUTS:
C     IER       I         Error return 0 -> okay
C                            1 -> error
C-----------------------------------------------------------------------
      INTEGER   IER, IERR, PTS, I, J
      INCLUDE 'JMFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Initialize
      IER = 0
      DMAX = -1.0E25
      DMIN = 1.0E25
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
C                                       Look for extremum
            IF (LINE(I).NE.FBLANK) THEN
               IF (DMAX.LT.LINE(I)) THEN
                  DMAX = LINE(I)
                  PTMAX = PTS
                  END IF
               IF (DMIN.GT.LINE(I)) THEN
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
C                                       scaling
      IF (DMAX.GT.-DMIN) THEN
         RSCALE = 5.0 / DMAX
      ELSE
         RSCALE = 5.0 / DMIN
         LINE(1) = DMAX
         DMAX = DMIN
         DMIN = LINE(1)
         I = PTMAX
         PTMAX = PTMIN
         PTMIN = I
         END IF
      DMAX = DMAX * RSCALE
      DMIN = DMIN * RSCALE
      DO 50 I = 1,NPTS
         IF (DATA(I).NE.FBLANK) DATA(I) = DATA(I) * RSCALE
 50      CONTINUE
C                                        Apply cutoff
      IF (OFFSET.NE.0.0) THEN
         OFFSET = OFFSET * DMAX
         DO 70 I = 1,NPTS
            IF ((DATA(I).NE.FBLANK) .AND. (DATA(I).LT.OFFSET))
     *         DATA(I) = FBLANK
 70         CONTINUE
         OFFSET = OFFSET / RSCALE
         END IF
      GO TO 999
C                                       Error return
 900  IER = 1
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('JMFDAT: COULD NOT READ LINE ',I7,'  IER=',I7)
 1040 FORMAT ('JMFDAT: # point read=',I7,'  NX x NY=',I7)
      END
      SUBROUTINE JMFDEF
C-----------------------------------------------------------------------
C   JMFDEF is a subroutine for JMFIT which inserts defaults for
C   the input parameters.
C-----------------------------------------------------------------------
      INTEGER   I, J, IERR
      REAL      TEMP, DX, DY, SMCB(3)
      LOGICAL   FIRST
      DOUBLE PRECISION XD, X(3)
      INCLUDE 'JMFIT.INC'
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
      FIRST = .TRUE.
C                                        Component loop
      DO 300 I = 1,NGAUSS
C                                        XTYPE 1 = Gaussian
C                                              2 = Zero offset
C                                              3 = Zero, slope
C                                              4 = Zero, slope, curv
C                                              5 = General six param
C                                       Gaussian component
         IF (XTYPE(I).EQ.1) THEN
C                                       Amplitude.  Use max
            G(1,I) = G(1,I) * RSCALE
            IF (G(1,I).EQ.0.0) THEN
               TEMP = 0.1
               IF (FIRST) TEMP = 1.0
               IF (ABS(DMIN).GT.ABS(DMAX)) THEN
                  G(1,I) = DMIN * TEMP
               ELSE
                  G(1,I) = DMAX * TEMP
                  END IF
               FIRST = .FALSE.
               END IF
C                                       Position.  Use max posit.
            IF ((G(2,I).EQ.0.0) .AND. (G(3,I).EQ.0.0)) THEN
               G(2,I) = AINT(WIN(1) + MOD(PTMAX-1,NX) + 0.5)
               G(3,I) = AINT(WIN(2) + AINT((PTMAX-0.5)/NX) + 0.5)
               END IF
C                                       Widths
            IF (G(4,I).LE.0.0) THEN
C                                       Use clean beam if avail.
               IF (CB(1).GT.0.0) THEN
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
C                                       Baseline
         ELSE IF ((XTYPE(I).GE.2) .AND. (XTYPE(I).LE.4)) THEN
            DO 150 J = 1,6
               G(J,I) = 0.0
               E(J,I) = -1.0
 150           CONTINUE
C                                       Zero offset only
            IF (XTYPE(I).EQ.2) THEN
               E(1,I) = 1.0
C                                       Zero and slope
            ELSE IF (XTYPE(I).EQ.3) THEN
               CALL RFILL (3, 1.0, E(1,I))
C                                       Zero, slope and curvature
            ELSE
               CALL RFILL (6, 1.0, E(1,I))
               END IF
            XTYPE(I) = 2
            END IF
 300     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE JMFPRT (IER)
C-----------------------------------------------------------------------
C   JMFPRT is a subroutine of JMFIT which prints out models on the line
C   printer and the message terminal.
C   INPUTS - COMMON
C      XPR       L      Flag   <=0=No line printer output
C                              >0=Line printer output
C   OUTPUTS:
C      IER       I      Error status   0=> okay
C                             1=> problem
C-----------------------------------------------------------------------
      INTEGER   IER
C
      CHARACTER RSTR(2)*20, FAST(6)*2, HFIX*2, MODEL(3)*8, CNAME*12,
     *   CCLAS*6, CUNIT*8, CPTYPE*2, COBJ*8, FILSPC*256
      INTEGER   IERR, SEQ, I, J, ILEN, I2TMP, CVOL, CUID, PERR, JERR,
     *    IBUFF(256), ITRIM, FLEN
      REAL      DXREF, DYREF, RCONST, PASUM, TEMP, XREF, YREF
      DOUBLE PRECISION    X(3)
      LOGICAL   ATEST
      INCLUDE 'JMFIT.INC'
      INCLUDE 'ORFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA HFIX  /'**'/
      DATA MODEL /'Gaussian','Baseline','????????'/
C-----------------------------------------------------------------------
C                                        Initialize
      IER = 0
      ATEST = .FALSE.
      PASUM = 90.
      RCONST = 3.14159265 / 180.0
      CALL WAWA2A (INNA, CNAME, CCLAS, SEQ, CPTYPE, CVOL, CUID)
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
C                                        Normal RA and DEC
      ELSE IF (AXTYP(LOCNUM).EQ.1) THEN
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
      WRITE (MSGTXT,1115) DEPTH
      IF (DOCRT.GT.-2.5) THEN
         CALL DOPRT
      ELSE
         CALL MSGWRT (4)
         END IF
      DO 115 J = 1,2
         IF (SAXLAB(J,LOCNUM).NE.' ') THEN
            WRITE (MSGTXT,1116) J+2, SAXLAB(J,LOCNUM)
            IF (DOCRT.GT.-2.5) THEN
               CALL DOPRT
            ELSE
               CALL MSGWRT (4)
               END IF
            END IF
 115     CONTINUE
      WRITE (MSGTXT,1120)
      IF (DOCRT.GT.-2.5) THEN
         CALL DOPRT
      ELSE
         CALL MSGWRT (4)
         END IF
      WRITE (MSGTXT,1122)
      CALL DOPRT
C                                       Print input model
      DO 400 I = 1,NGAUSS
         FAST(1) = '  '
         FAST(2) = '  '
         FAST(3) = '  '
         FAST(4) = '  '
         FAST(5) = '  '
         FAST(6) = '  '
C                                       Gaussian comp
         IF (XTYPE(I).EQ.1) THEN
            DO 140 J = 1,6
               IF (E(J,I).LE.0.0) FAST(J) = HFIX
 140           CONTINUE
            WRITE (MSGTXT,1140) I, MODEL(1)
            CALL DOPRT
            TEMP = G(1,I) / RSCALE
            WRITE (MSGTXT,1141) TEMP, FAST(1), CUNIT
            CALL DOPRT
            WRITE (MSGTXT,1142) (G(J,I), FAST(J), J = 2,3)
            CALL DOPRT
            WRITE (MSGTXT,1144) (G(J,I), FAST(J), J = 4,6)
            CALL DOPRT
C                                       Fix diameters
            G(4,I) = ABS (G(4,I))
            G(5,I) = ABS (G(5,I))
            IF (G(4,I).LT.G(5,I)) THEN
               TEMP = G(5,I)
               G(5,I) = G(4,I)
               G(4,I) = TEMP
               G(6,I) = G(6,I) + 90.0
               END IF
C                                       Diameter circularity
            IF ((G(4,I)-G(5,I).LT.G(4,I)/10.0) .AND. (E(4,I).GT.0.0)
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
C                                       Fix position angle
            G(6,I) = (G(6,I) - PASUM) * RCONST
C                                       Baseline term
         ELSE
            WRITE (MSGTXT,1230) I, MODEL(2)
            CALL DOPRT
C                                       Zero offset?
            IF (E(1,I).GT.0.0) THEN
               WRITE (MSGTXT,1232) G(1,I), FAST(1), CUNIT
               CALL DOPRT
               END IF
C                                       Slope?
            IF ((E(2,I).GT.0.) .OR. (E(3,I).GT.0.)) THEN
               WRITE (MSGTXT,1260) (G(J,I), FAST(J), J = 2,3)
               CALL DOPRT
               END IF
C                                       Curvature?
            IF ((E(4,I).GT.0.) .OR. (E(5,I).GT.0.)) THEN
               WRITE (MSGTXT,1290) (G(J,I), FAST(J), J = 4,6)
               CALL DOPRT
               END IF
C                                       Fix up ampl and angles
            G(3,I) = (G(3,I) - PASUM) * RCONST
            G(6,I) = (G(6,I) - PASUM) * RCONST
            END IF
C                                       set atest
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
      IF (OFFSET.NE.0.0) THEN
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
 1105 FORMAT ('X-ref pix=',F6.1,2X,A20,' pix sep (Asec)',F8.4)
 1107 FORMAT ('Y-ref pix=',F6.1,2X,A20,' pix sep (Asec)',F8.4)
 1110 FORMAT ('X-ref pix=',F6.1,'  value=',1PE14.5,'  pix sep=',
     *   1PE13.5)
 1112 FORMAT ('Y-ref pix=',F6.1,'  value=',1PE14.5,'  pix sep=',
     *   1PE13.5)
 1115 FORMAT ('Axes 3-7 pixels =',5I7)
 1116 FORMAT ('Axis',I2,' : ',A20)
 1120 FORMAT (1X,30('- '))
 1122 FORMAT ('******** Input Model ',2('********************'))
 1140 FORMAT ('Component=',I3,2X,A8)
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
 1415 FORMAT (6X,13X,'** Means parameter is held fixed')
 1418 FORMAT (6X,'Beam=',F7.2,3X,' X',F7.2,3X,' pixels in PA',
     *   F7.1,3X,' deg')
 1420 FORMAT (6X,'NO CLEAN BEAM')
 1430 FORMAT (6X,'Clipping below',1PE11.4,1X,A8)
      END
      SUBROUTINE JMPLOT (N, DSCALE)
C-----------------------------------------------------------------------
C   Subroutine JMPLOT prints a digital representation of the maps from
C   JMFIT on the line printer.
C   INPUTS:
C      N       I        Type of map (1->input map, 2->input model,
C                      3->residual map, 4->final model)
C      DSCALE  R        Scale of display
C   OUTPUT - COMMON
C      XPR     L        Set false if printer error (only)
C-----------------------------------------------------------------------
      CHARACTER BLANKS*132, ALINE*132, WORD(4)*12, CTEMP*8
      INTEGER   N, WIN1, WIN2, WIN3, WIN4, WINL, IPTS, WINH, I, J, IL,
     *   PLINE, PERR, IROUND, NXX, IC, IP, IS
      REAL      DSCALE, TEMP, DTEMP, DSAVE, LSCALE, RMIN, RMAX
      INCLUDE 'JMFIT.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE DSAVE
      DATA BLANKS /' '/
      DATA WORD /'Input map   ', 'Input model ', 'Residual map',
     *   'Final model '/
C-----------------------------------------------------------------------
      IF (XCRT.LT.-3.5) GO TO 999
      WIN1 = WIN(1) + 0.01
      WIN2 = WIN(2) + 0.01
      WIN3 = WIN(3) + 0.01
      WIN4 = WIN(4) + 0.01
      NXX = WIN3 - WIN1 + 1
      IC = (NACROS - 6) / NXX
      IC = MIN (6, MAX (IC, 3))
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
      DSAVE = LSCALE
      DTEMP = LSCALE * RSCALE
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
            IF (DATA(IPTS).NE.FBLANK) THEN
               TEMP = DATA(IPTS) / DTEMP
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
      SUBROUTINE JMFVST (NGAUSS, G, E, IVAR, JVAR, NVAR, VALVAR)
C-----------------------------------------------------------------------
C   INFVST is a subroutine for JMFIT which stores the variable
C   parameters in the proper arrays.
C   INPUTS:  none  see COMMONs
C   OUTPUTS:
C      NVAR            I       The number of variables
C      VALVAR(24)      D       The value of this variable
C-----------------------------------------------------------------------
      INTEGER   NVAR, NGAUSS, IVAR(24), JVAR(24), I, J
      REAL      E(6,4)
      DOUBLE PRECISION    VALVAR(24), G(6,4)
C-----------------------------------------------------------------------
C                                       Initialize
      NVAR = 0
C                                       Loop over components
      DO 50 I = 1,NGAUSS
         DO 40 J = 1,6
C                                       holding parameter fixed?
            IF (E(J,I).LE.0.0) GO TO 30
C                                       NO
               NVAR = NVAR + 1
               VALVAR(NVAR) = G(J,I)
               IVAR(NVAR) = I
               JVAR(NVAR) = J
               E(J,I) = 1.0
               GO TO 40
 30         E(J,I) = -1.0
 40         CONTINUE
 50      CONTINUE
 999  RETURN
      END
      SUBROUTINE JMFSTO (IER)
C-----------------------------------------------------------------------
C   JMFSTO, a subroutine of JMFIT, creates, opens and stores the
C   residual map after fitting.
C   Outputs:
C      IER             I        Error return  0-> okay
C                                  1-> error
C-----------------------------------------------------------------------
      CHARACTER CNAME*12, CCLAS*6, CPTYPE*2
      INTEGER   IER, IERR, I, I1, J, CSEQ, CVOL, CUID
      REAL      OUTR(256), MAX, MIN, RRMAX, RRMIN
      HOLLERITH OUTH(256), BLANK4
      DOUBLE PRECISION    OUTD(128)
      INCLUDE 'JMFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (OUTBLK, OUTR, OUTD, OUTH)
C-----------------------------------------------------------------------
C                                       Initialize
      IER = 0
      CALL COPY (256, CATBLK, OUTBLK)
C                                       Fill in outname
      CNAME = ' '
      CALL CHR2H (4, CNAME, 1, BLANK4)
      CALL H2WAWA (XON, XOC, XOS, BLANK4, XOD, XU, OUTNA)
C                                       Fill in header values
      OUTBLK(KINAX) = NX
      OUTBLK(KINAX+1) = NY
      OUTBLK(KINAX+2) = 1
      OUTBLK(KINAX+3) = 1
      OUTBLK(KINAX+4) = 1
      OUTBLK(KINAX+5) = 1
      OUTBLK(KINAX+6) = 1
      OUTR(KRCRP) = CATR(KRCRP) - WIN(1) + 1.0
      OUTR(KRCRP+1) = CATR(KRCRP+1) - WIN(2) + 1.0
      OUTR(KRCRP+2) = CATR(KRCRP+2) - DEPTH(1) + 1.0
      OUTR(KRCRP+3) = CATR(KRCRP+3) - DEPTH(2) + 1.0
      OUTR(KRCRP+4) = CATR(KRCRP+4) - DEPTH(3) + 1.0
      OUTR(KRCRP+5) = CATR(KRCRP+5) - DEPTH(4) + 1.0
      OUTR(KRCRP+6) = CATR(KRCRP+6) - DEPTH(5) + 1.0
      CALL CATCLR (OUTBLK)
C                                       Create map
      CALL MAPCR (INNA, OUTNA, OUTBLK, IERR)
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
               DATA(I1) = DATA(I1) / RSCALE
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
      CALL WAWA2A (OUTNA, CNAME, CCLAS, CSEQ, CPTYPE, CVOL, CUID)
      CVOL = FILTAB(POVOL,6)
      CALL A2WAWA (CNAME, CCLAS, CSEQ, CPTYPE, CVOL, CUID, OUTNA)
      OUTR(KRDMX) = RRMAX
      OUTR(KRDMN) = RRMIN
      CALL FILCLS (LUN1)
      CALL JMFHI (IERR)
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
 1050 FORMAT ('JMFSTO: COULD NOT CREATE MAP.  IER=',I7)
 1060 FORMAT ('JMFSTO: COULD NOT OPEN MAP.  IER=',I7)
 1070 FORMAT ('JMFSTO: WRITE ERROR LINE ',I6,'  IER=',I7)
 1080 FORMAT ('JMFSTO: HI FILE GENERATION ERROR.  IER=',I7)
      END
      SUBROUTINE JMFHI (IER)
C-----------------------------------------------------------------------
C   JMFHI creates and writes the HI file for the task JMFIT.
C   Outputs:
C      IER            I     Error return  0->okay
C                             1->uh-oh
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, HILINE*72, CHTMP*2, MODEL(3)*8, CONAME*12,
     *   CINAME*12, COCLAS*6, CICLAS*6
      INTEGER   IER, IERR, NHISTF, LHIN, LHOUT, IVOL, OVOL, IBTEMP,
     *   IBUFF1(256), IBUFF2(256), INSEQ, OUTSEQ, ITEMP(7), I,
     *   J, CUID
      LOGICAL   T
      INCLUDE 'JMFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA NHISTF, LHIN, LHOUT /2,27,28/
      DATA T /.TRUE./
      DATA PRGNAM /'JMFIT '/
      DATA MODEL /'Gaussian','Baseline','????????'/
C-----------------------------------------------------------------------
C                                       Initialize HI
      CALL HIINIT (NHISTF)
      IER = 0
C                                       Create and open output HI file
      CALL WAWA2A (INNA, CINAME, CICLAS, INSEQ, CHTMP, IVOL, CUID)
      CALL WAWA2A (OUTNA, CONAME, COCLAS, OUTSEQ, CHTMP, OVOL, CUID)
C                                       copy keywords
      CALL KEYPCP (IVOL, INSL, OVOL, OUTSL, 0, ' ', IERR)
C                                       copy HI
      CALL HISCOP (LHIN, LHOUT, IVOL, OVOL, INSL, OUTSL, OUTBLK,
     *   IBUFF1, IBUFF2, IERR)
      IF (IERR.LE.2) GO TO 50
         WRITE (MSGTXT,1000) IERR
         GO TO 900
C                                       Add new HI entries
C                                       Input name
 50   CALL HENCO1 (PRGNAM, CINAME, CICLAS, INSEQ, IVOL, LHOUT,
     *   IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Output name
      CALL HENCOO (PRGNAM, CONAME, COCLAS, OUTSEQ, OVOL, LHOUT,
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
            CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
            GO TO 250
 250     CONTINUE
      GO TO 950
C
 900  IER = 1
      CALL MSGWRT (8)
C                                       Close HI file
 950  CALL HICLOS(LHOUT, T, IBUFF2, IERR)
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
 1110 FORMAT (A6,'Peak=',1PE13.3,',',1PE12.3)
 1112 FORMAT (A6,'Pos=',F6.2,F5.2,',',F6.2,F5.2)
 1114 FORMAT (A6,'Size=',3(F6.1,',',F6.1,4X))
 1170 FORMAT (A6,'Zero level=',1PE13.3,',',1PE12.3)
 1172 FORMAT (A6,'Slope=',1PE13.3,',',1PE12.2,' Angle=',0PF6.1,',',
     *   F5.1)
 1174 FORMAT (A6,'Curv=',1PE13.3,',',1PE12.3,'  Eccen=',0PF6.4,',',
     *   F5.4)
 1176 FORMAT (A6,'Angle of curv=',F6.1,',',F5.1)
      END
      SUBROUTINE JMFOUT (RSTO, DOMODL, IER)
C-----------------------------------------------------------------------
C   JMFOUT is a subroutine of JMFIT which prints out the parameter
C   solutions and converts them into useful coordinates.
C   Inputs:
C      RSTO       R         > 0 => write a CC file, > 1.5 use convolved
C                              components, not deconvolved
C      DOMODL     R         Add a CC file also to input image
C   Outputs:
C      IER        I         Error return  0-> okay
C                               1-> error
C-----------------------------------------------------------------------
      CHARACTER RSTR(2)*20, CDUM*12,  WORD(3)*8, MODEL(3)*8, CONAME*12,
     *   CINAME*12, COCLAS*6, CICLAS*6, CHTMP*2, CUNIT*8, AXIS*8,
     *   FUNIT*8, KEYWRD*8, CAX(3)*14, ARRAY*8, STRING*24, TUNIT*8
      INTEGER   IER, I2TMP, IBTEMP, I, J, K, ILEN, IERR, INSEQ, OUTSEQ,
     *   CUID, CCLUN, CCVOL, CCVER, NREC, LREC, MFLUN, CCBUF(768),
     *   CILUN, CIVOL, CIVER, CIBUF(768), IRNO, MFBUF(512), PLANE,
     *   MFRNO, ITITLE(8), STBUF(512), STKOLS(7), STNUMV(7), STTYPE,
     *   STVER, LUNTMP, STLUN, ISTRNO, IROUND, RPT
      LOGICAL   DOCC, CRIT1, CRIT2, SESTAT, OUTSID
      REAL      PASUM, CAXINC(2), DUM, INT, INTE, DCONV(3,3), DOMODL,
     *   RSTO, RECORD(7), RICORD(7), XXT, YYT, BMAJS, BMINS, BPAS, COSC,
     *   SINC, ERRINT, LN, A, X0, Y0, IMAJ, IMIN, FI, BMAJ, BMIN,
     *   ARBEAM, ARIMAG, ERRA, FRERRA, ERRMAJ, ERRMIN, ERRFI, ERRX0,
     *   ERRY0, ERRMAS, ERRMIS, GG(6,4), ERRAA, XCONV(3,3), RTITLE(8),
     *   DX, DY, SMCB(3), BMFACT, DLFACT, RINC, ARHEAD, STWID(3), RDUM,
     *   R, XYSH(2), CORA, CORINT, CORINE, SNR
      HOLLERITH HTITLE(8)
      DOUBLE PRECISION    X(3), XX, YY, ZZ, DTEMP, LAMBDA, XD, STXY(2),
     *   DEC1
      INCLUDE 'INCS:PMFC.INC'
      REAL      ROW(NUMCOL)
      INCLUDE 'JMFIT.INC'
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
C                                       stars file?
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
C                                       Create CC file
      DOCC = (RSTO.GT.0.0) .OR. (DOMODL.GT.0.0)
      IF ((DOCC) .OR. (OUTVER.GE.0)) THEN
         MFLUN = 29
         CCLUN = 28
         CILUN = 27
         CALL WAWA2A (INNA, CINAME, CICLAS, INSEQ, CHTMP, CIVOL, CUID)
         CALL WAWA2A (OUTNA, CONAME, COCLAS, OUTSEQ, CHTMP, CCVOL, CUID)
         NREC = 10
         LREC = 7
         CCVER = 0
         CIVER = 0
         IERR = 0
         IF (RSTO.GT.0.0) THEN
            CALL CCINI (CCLUN, LREC, CCVOL, OUTSL, CCVER, OUTBLK, CCBUF,
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
      IF (DOCRT.GT.-2.5) THEN
         MSGTXT = ' '
         CALL DOPRT
         END IF
      ROW(COLPLN) = PLANE
      ROW(CORRMS) = SUMSQ
      ROW(CORPEK) = RESMAX
      ROW(CORFLX) = RESSUM
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
            DTEMP = G(5,I)
            G(5,I) = G(4,I)
            G(4,I) = DTEMP
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
C
         CRIT1 = ARBEAM .LT. 0.1*ARIMAG
         CRIT2 = ARBEAM .GT. 0.9*ARIMAG
C
         LN = SQRT (8. * ALOG (2.0))
         RPT = 4
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
         IF (A.NE.0.0) FRERRA = ABS(ERRA / A)
         ERRMAJ = ABS (IMAJ * FRERRA)
         ERRMIN = ABS (IMIN * FRERRA)
         ERRFI = SQRT (2.) * (IMAJ*IMIN) / MAX (1.E-6, IMAJ**2-IMIN**2)
     *      * FRERRA * RAD2DG
         ERRFI = MIN (360.0, ABS(ERRFI))
         ORERFI(I) = ERRFI
         COSC = COS (DG2RAD * FI)
         SINC = SIN (DG2RAD * FI)
C                                       SINC&COSC switch because
C                                       the angle is measured from Y
         ERRX0 = SQRT ((ERRMAJ*SINC)**2 + (ERRMIN*COSC)**2) / LN
         ERRY0 = SQRT ((ERRMAJ*COSC)**2 + (ERRMIN*SINC)**2) / LN
         ROW(COLPEK) = RECORD(1)
         ROW(COEPEK) = ERRA
         BMFACT = 1.0
         DLFACT = 1.0
C                                        Amplitude
         ERRAA = ERRA
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
C                                       single precision
         DO 55 J = 1,6
            GG(J,I) = G(J,I)
 55         CONTINUE
C                                       Radius
         XD = 0.0D0
         IF (((CORTYP(LOCNUM).EQ.1) .OR. (CORTYP(LOCNUM).EQ.2)) .AND.
     *      ((PBPARM(1).GT.0.0) .OR. (BWS.GT.0.0))) THEN
            CALL XYVAL (GG(2,I), GG(3,I), X(1), X(2), X(3), IERR)
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
            IF (OUTSID) BMFACT = PBPARM(1)
            BMFACT = MAX (BMFACT, PBPARM(1))
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
         IF (ARHEAD.GT.0.0) THEN
            INT = ARIMAG / ARHEAD * A
            INTE = (E(1,I)/G(1,I)) * INT
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
C                                        Get the local pixel incr
         CALL RCOPY (5, GG(2,I), ROW(COPCEX))
         CALL XYVAL (GG(2,I), GG(3,I), X(1), X(2), X(3), IERR)
         STXY(1) = X(1)
         STXY(2) = X(2)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1051) IERR, I
            CALL MSGWRT (7)
            CAXINC(1) = AXINC(1,LOCNUM)
            CAXINC(2) = AXINC(2,LOCNUM)
            GO TO 80
         ELSE
            CALL XYVAL (GG(2,I)+1.0, GG(3,I), XX, YY, ZZ, IERR)
            XXT = (XX - X(1)) * COS (DG2RAD*X(2))
            YYT = YY - X(2)
            CAXINC(1) = XXT * COS (DG2RAD*ROT(LOCNUM)) +
     *         YYT * SIN (DG2RAD*ROT(LOCNUM))
            CALL XYVAL (GG(2,I), GG(3,I)+1.0, XX, YY, ZZ, IERR)
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
         IF (AXTYP(LOCNUM).EQ.1) THEN
            RINC = MAX (ABS (CAXINC(1)), ABS (CAXINC(2))) * 3600.0
C                                       RA and DEC
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
         IF (ERRMAJ .GE. 99.5) ERRMAJ = 99.5
         IF (ERRMIN .GE. 99.5) ERRMIN = 99.5
         WRITE (MSGTXT,1580) CAX(1), IMAJ, ERRMAJ, ' pixels'
         CALL DOPRT
         FGW(1,I) = IMAJ
         IF (XDW(1,I).GT.0.0) FDW(1,I) = ERRMAJ
         WRITE (MSGTXT,1580) CAX(2), IMIN, ERRMIN, ' pixels'
         CALL DOPRT
         FGW(2,I) = IMIN
         IF (XDW(2,I).GT.0.0) FDW(2,I) = ERRMIN
         WRITE (MSGTXT,1581) CAX(3), FI, ERRFI, ' degrees'
         CALL DOPRT
         FGW(3,I) = FI
         IF (XDW(3,I).GT.0.0) FDW(3,I) = ERRFI
C                                       convert from pixels
         CALL GAUSPS ('P2S', GG(2,I), GG(3,I), GG(4,I), GG(5,I),
     *      GG(6,I), BMAJS, BMINS, BPAS, IERR)
         STWID(1) = BMAJS
         STWID(2) = BMINS
         STWID(3) = BPAS
         WRITE (STRING,1085) I
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
               WRITE (MSGTXT,1585) CAX(1), BMAJS, ERRMAS, ' asec'
            ELSE IF (RINC.GT.0.01) THEN
               WRITE (MSGTXT,1586) CAX(1), BMAJS, ERRMAS, ' asec'
            ELSE IF (RINC.GT.0.0001) THEN
               WRITE (MSGTXT,1587) CAX(1), BMAJS, ERRMAS, ' asec'
            ELSE
               WRITE (MSGTXT,1588) CAX(1), BMAJS, ERRMAS, ' asec'
               END IF
            CALL DOPRT
            IF (RINC.GT.1.0) THEN
               WRITE (MSGTXT,1585) CAX(2), BMINS, ERRMIS, ' asec'
            ELSE IF (RINC.GT.0.01) THEN
               WRITE (MSGTXT,1586) CAX(2), BMINS, ERRMIS, ' asec'
            ELSE IF (RINC.GT.0.0001) THEN
               WRITE (MSGTXT,1587) CAX(2), BMINS, ERRMIS, ' asec'
            ELSE
               WRITE (MSGTXT,1588) CAX(2), BMINS, ERRMIS, ' asec'
               END IF
            CALL DOPRT
            WRITE (MSGTXT,1581) CAX(3), BPAS, ERRFI, ' degrees'
            CALL DOPRT
            IF (I.EQ.1) THEN
               WRITE (MSGTXT,1582) FSHIFT
               CALL DOPRT
               END IF
            END IF
C                                       Deconvolve if possible
         IF (CB(1).GT.0.0) THEN
C                                       BW smearing here
            DX = GG(2,I) - XCEN
            DY = GG(3,I) - YCEN
            CALL BWSMCB (DX, DY, XD, BWS, CB, SMCB)
            DLFACT = SMCB(1) * SMCB(2) / (CB(1) * CB(2))
            CALL BMVAL (GG(4,I), GG(5,I), GG(6,I), ERRMAJ, ERRMIN,
     *         ERRFI, SMCB(1), SMCB(2), SMCB(3), FACTOR, DCONV)
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
            CALL RCOPY (9, DCONV, XCONV)
            IF (AXTYP(LOCNUM).EQ.1) THEN
               DO 105 J = 1,3
C                                       convert from pixels
                  IF (DCONV(1,J).GT.0.0) THEN
                     CALL GAUSPS ('P2S', GG(2,I), GG(3,I), DCONV(1,J),
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
               RICORD(4) = DCONV(1,1) / 3600.0
               RICORD(5) = DCONV(2,1) / 3600.0
               RICORD(6) = DCONV(3,1)
               IF ((RSTO.GT.0.0) .AND. (RSTO.LE.1.5)) THEN
                  RECORD(4) = RICORD(4)
                  RECORD(5) = RICORD(5)
                  RECORD(6) = RICORD(6)
                  RECORD(7) = 1.0
                  END IF
               WRITE (MSGTXT,1105)
               CALL DOPRT
               WRITE (MSGTXT,1094)
               CALL DOPRT
               DO 110 J = 1,3
                  WRITE (MSGTXT,1107) WORD(J), (DCONV(J,K), K = 1,3)
                  CALL DOPRT
 110              CONTINUE
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
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1110) IERR, 'CC', IRNO
                  CALL MSGWRT (7)
                  RSTO = -1
                  END IF
               END IF
            IF (DOMODL.GT.0.0) THEN
               CALL TABIO ('WRIT', 0, IRNO, RICORD, CIBUF, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1110) IERR, 'CC', IRNO
                  CALL MSGWRT (7)
                  DOMODL = -1.0
                  END IF
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
            IF (XDM(I).GT.0.0) XDM(I) = E(1,I)
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
            IF (XDP(1,I).GT.0.0) XDP(1,I) = E(2,I)
            IF (XDP(2,I).GT.0.0) XDP(2,I) = E(3,I)
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
      IF (SESTAT) CALL CATDIR ('CSTA', CIVOL, INSL, CDUM, CDUM, DUM,
     *   CDUM, DUM, 'CLWR', CIBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Writing CC file version',I4,' attached to output image')
 1001 FORMAT ('Writing ',A,' file version',I4,
     *   ' attached to input image')
 1005 FORMAT ('********* Solution from JMFIT ',3('***********'))
 1042 FORMAT ('Component ',I3,'-',A8)
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
 1110 FORMAT ('ERROR',I5,' WRITING ',A,' ROW',I7)
 1085 FORMAT ('JMFIT_',I1)
 1580 FORMAT (2X,A,'    =',F8.3,' +/- ',F7.4, A)
 1581 FORMAT (2X,A,'    =',F8.3,' +/- ',F6.3, A)
 1582 FORMAT ('  RASHIFT=',F11.6,' DECSHIFT=',F11.6,
     *   ' to center on pixel')
 1585 FORMAT (2X,A,'    =',F8.3,' +/-',F8.3, A)
 1586 FORMAT (2X,A,'    =',F9.5,' +/-',F9.5, A)
 1587 FORMAT (2X,A,'    =',F10.7,' +/-',F10.7, A)
 1588 FORMAT (2X,A,'    =',F11.9,' +/-',F11.9, A)
 1090 FORMAT (2X,30('- '))
 1092 FORMAT (18X,'Deconvolution of component in pixels')
 1094 FORMAT (23X, 'Nominal',5X,'minimum',5X,'maximum')
 1096 FORMAT (4X,A8,6X,3F12.3)
 1105 FORMAT (18X,'Deconvolution of component in asec')
 1107 FORMAT (4X,A8,6X,3F12.6)
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
C   also generates appropriate errors.  This is used with JMFIT.
C   INPUTS:
C      BMAJ       R       Fitter major axis
C      BMIN       R       Fitted minor axis
C      BPA        R       Fitted pos. angle (deg)
C      BMAJE      R       Fitted major axis error
C      BMINE      R       Fitted minor axis error
C      BPAE       R       Fitted pos. angle error (deg)
C      CBMAJ      R       Clean beam major axis
C      CBMIN      R       Clean beam minor axis
C      CBPA       R       Clean beam pos. angle (deg)
C   Outputs:
C      R(3,3)     R       RMAJ, RMIN, RPA array
C-----------------------------------------------------------------------
      REAL      BMAJ, BMIN, BPA, BMAJE, BMINE, BPAE, CBMAJ, CBMIN, CBPA,
     *   FACTOR, R(3,3)
C
      INTEGER   IERR, I, J, K, IC
      REAL      B1, B2, B3, DELT(3), MAJOR, MINOR, PA, TEMP
      DATA DELT /-1.0,0.0,1.0/
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
                     PA = PA + R(3,1) - 90.0
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
      SUBROUTINE DVDMIN (FX, XI, ERR, N, EPS, ITMAX, FOPT, GNOPT,
     *    IER, NPR, IPTLEV)
C-----------------------------------------------------------------------
C    This is a Fortran implementation of Davidon's optimally conditioned
C  variable metric (quasi-Newton) method for function minimization.  It
C  is based on the algorithm given in W. C. Davidon:  Optimally condi-
C  tioned optimization algorithms without line searches, Mathematical
C  Programming, vol. 9 (1975) pp. 1-30.  One should refer to that re-
C  ference for the algorithmic details.  Here, the steps of the
C  algorithm which are delineated by COMMENT lines correspond to the
C  numbered steps in Davidon's paper.  The user must supply a subroutine
C  FX to calculate the objective function and its gradient at a given
C  point.  The objective function F is assumed to be a real-valued
C  function of N real variables.  Here, 0 is assumed to be a lower
C  bound for F.  If F can assume negative values, Step 2 must be modi-
C  fied in one of two different ways, depending on whether a lower
C  bound is known (see Davidon for details).
C
C  Inputs:
C    FX      ENTRY     A user-supplied subroutine of the form
C                      FX (X, F, G, K) which is used to calculate the
C                      value of the objective function F at X and, op-
C                      tionally, the gradient G of F at X.  When K=1, FX
C                      need only compute F.  When K=2, both F and G are
C                      required.
C    XI(N)   D         An initial estimate for the location of a mini-
C                      mum.
C    ERR(N)  D         An initial estimate of the square roots of the
C                      diagonal elements of the inverse of the Hessian
C                      matrix of the objective function evaluated at XI.
C                      When no estimates are known, it should suffice to
C                      set ERR(I)=1.0D0, for all I.
C    N       I         The number of unknowns.
C    EPS     D         A small positive number used in tests to set a
C                      lower bound on the squared Euclidean norm of
C                      vectors considered significantly different from
C                      0.  EPS is used in the convergence test.  Usually
C                      setting EPS in the range 10**(-12) to 10**(-8) is
C                      reasonable.  Very close to a minimum, the algo-
C                      rithm generally exhibits a quadratic rate of con-
C                      vergence, so setting EPS a few orders of magni-
C                      tude too small usually is not too costly.
C    ITMAX   I         The maximum number of iterations.  On average, a
C                      few evaluations of F and slightly more than one
C                      evaluation of G are required at each iteration.
C    NPR     I         A print flag.  When NPR=0, there is no printout;
C                      for NPR=1, the value of F and the Euclidean norm
C                      of G, both evaluated at the location of the best
C                      minimum found so far, are printed at each itera-
C                      tion; for NPR=2, the latter information, together
C                      with the location of the best minimum, is print-
C                      ed at each iteration.
C    IPTLEV  I         Print level
C  Outputs:
C    XI(N)   I         The user-supplied initial guess is replaced by
C                      the location of the best minimum found by the al-
C                      gorithm.
C    ERR(N)  D         The initial estimate supplied by the user is re-
C                      placed by an estimate of the square roots of the
C                      diagonal elements of the Hessian matrix evaluated
C                      at the best minimum found.  In least-squares ap-
C                      plications, assuming that F is the sum of squared
C                      residuals, estimates of the standard errors of
C                      the unknowns can be obtained by multiplying ERR
C                      by the r.m.s. residual.
C    FOPT     D        The value of F evaluated at the location of the
C                      best minimum that was found.
C    GNOPT    D        The Euclidean norm of the gradient of the objec-
C                      tive function, evaluated at the location of the
C                      best minimum that was found.
C    IER      I        An error flag.  When IER=0, convergence was
C                      achieved in ITMAX or fewer iterations; other-
C                      wise not.
C
C  Remarks:
C  1) This algorithm can be used for under-determined problems.
C  2) It maintains an approximation, in factored form J*transpose(J),
C     to the inverse Hessian of F.  At each iteration, a rank two update
C     is added to this approximation.  This approximation remains posi-
C     tive definite throughout the iteration.  In cases where an un-
C     known, say the Ith unknown, is ill-determined, ERR(I) will be
C     finite on exit from this routine. So, in least-squares applica-
C     tions, the error estimates for ill-determined parameters are like-
C     ly to be too small.
C  2.5) In the case of an under-determined problem (i.e., when the
C     Hessian matrix is singular) J*transpose(J) is a non-singular
C     matrix whose inverse is close to the Hessian matrix.
C  3) Furthermore, in cases where an excellent initial guess is supplied
C     by the user, DVDMIN is likely to converge before it has iterated
C     long enough to get a good approximation to the inverse Hessian.
C     (Understand that it is trying to estimate this second-order in-
C     formation only from the first-order information that is supplied
C     by FX.)  So, in least-squares applications, when convergence oc-
C     curs in just a couple of iterations, the derived error estimates
C     may be inaccurate.
C  4) Another Fortran implementation is given in the technical report
C     by W. C. Davidon and L. Nazareth:  DRVOCR - A Fortran implementa-
C     tion of Davidon's optimally conditioned method, Argonne National
C     Lab., Applied Math. Div. Technical Memo. No. 306, August 1977.
C  5) Comparisons of Davidon's algorithm with other quasi-Newton mini-
C     mization algorithms are given in  J. N. Lyness:  A bench mark
C     experiment for minimization algorithms, Math. of Computation,
C     vol. 33 (1979) pp. 249-264.  This algorithm compares quite favor-
C     ably with others, including the routine QNMDER of Gill et al.,
C     and the Harwell Library routine VA13AD.
C  6) Argonne Lab.'s MINPACK routines (non-proprietary) or NAG Library
C     routines (proprietary) could be used in place of DVDMIN.  They
C     would provide somewhat more flexibility.  They're a bit more con-
C     servative (and therefore more robust, but perhaps less efficient).
C-----------------------------------------------------------------------
      EXTERNAL FX
      INTEGER   NF, NG, IT, I, N, J, NPR, ITMAX, IER, L, I1, I2, IPTLEV
C                                       (24 = max number of unknowns)
      DOUBLE PRECISION XI(*), ERR(*), DDOT, DMACH, DNRM2, LAMBDA,
     *   MSQ, MU, NSQ, NU, XJ(24,24), X0(24), X(24), K0(24), K(24),
     *   S(24), GG(24), M(24), P(24), Q(24), WUN(24), AX(24), TINYC,
     *   F, GN, F0P, XX, EPS, FP, B0, UTU, UTS, B, GAMMA, F0,
     *   DELTA, A, C, ALF, T1, T2, T3, T4, T5, T6, QTK0, FOPT,
     *   GNOPT, DT1, DT2
      CHARACTER ATPT*8
      REAL      RPEAK
      INCLUDE 'JMFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        Initialization:
      TINYC = 1.0D-3*SQRT(DMACH(2))
      NF = 1
      NG = 1
      IT = -1
      DO 20 I = 1,N
         X(I) = XI(I)
         X0(I) = XI(I)
         DO 10 J = 1,N
            XJ(I,J) = 0.0D0
 10         CONTINUE
         XJ(I,I) = ERR(I)
 20      CONTINUE
      CALL FX (X, F, GG, 2)
      F0 = F
      DO 40 I = 1,N
         DO 30 J = 1,N
            AX(J) = XJ(J,I)
 30         CONTINUE
         WUN(I) = DDOT (N, AX, 1, GG, 1)
         K0(I) = WUN(I)
 40      CONTINUE
C
C                                       Step 1:
 100  IT = IT + 1
      GN = DNRM2 (N, GG, 1)
      IF ((NPR.GE.1) .AND. (IPTLEV.GE.1)) THEN
         DT1 = F0 / RSCALE / RSCALE
         DT2 = GN / RSCALE
         WRITE (MSGTXT,1010) IT, DT1, DT2
         CALL DOPRT
         END IF
      IF ((NPR.GE.1) .AND. (IPTLEV.GE.2)) THEN
         MSGTXT = 'Parameters:'
         CALL DOPRT
         I1 = 1
 102     I2 = I1 + 5
            IF (I2.GT.N) I2 = N
            WRITE (MSGTXT,1021) (X0(I), I = I1,I2)
            CALL DOPRT
            I1 = I2 + 1
            IF (I1.LE.N) GO TO 102
         END IF
      IF (IT.GE.ITMAX) THEN
         IER = 1
         GO TO 900
         END IF
      DO 120 I = 1,N
         S(I) = -K0(I)
 120     CONTINUE
      F0P = DDOT (N, K0, 1, S, 1)
      LAMBDA = 2.0D0
      IF (4.0D0*F0.GE.-F0P) GO TO 200
         IF (F0P.EQ.0) THEN
            ATPT = 'F0P  120'
            GO TO 890
            END IF
         XX = -4.0D0*F0 / F0P
         DO 130 I = 1,N
            S(I) = XX * S(I)
 130        CONTINUE
         F0P = -4.0D0 * F0
C
C                                       Step 2:
 200  DO 220 I = 1,N
         DO 210 J = 1,N
            AX(J) = XJ(I,J)
 210        CONTINUE
         X(I) = X0(I) + DDOT (N, AX, 1, S, 1)
 220     CONTINUE
      IF (-F0P.GE.EPS) GO TO 230
         IER = 0
         GO TO 900
 230  CALL FX (X, F, GG, 1)
      NF = NF + 1
      IF (F.LT.F0) GO TO  300
         DO 240 I = 1,N
            S(I) = 0.5D0 * S(I)
 240        CONTINUE
         F0P = 0.5D0 * F0P
         LAMBDA = 0.5D0
         GO TO 200
C
C                                       Step 3:
 300  CALL FX (X, F, GG, 2)
      NF = NF + 1
      NG = NG + 1
      DO 320 I = 1,N
         DO 310 J = 1,N
            AX(J) = XJ(J,I)
 310        CONTINUE
         K(I) = DDOT (N, AX, 1, GG, 1)
         M(I) = S(I) + K0(I) - K(I)
         K0(I) = K(I)
         X0(I) = X(I)
 320     CONTINUE
      FP = DDOT (N, K, 1, S, 1)
      B0 = FP - F0P
      F0 = F
      F0P = FP
      IF (B0.GE.EPS) GO TO 400
         DO 330 I = 1,N
            S(I) = LAMBDA * S(I)
 330        CONTINUE
         F0P = LAMBDA * F0P
         GO TO 200
C
C                                       Step 4:
 400  MSQ = DNRM2(N,M,1)**2
      IF (MSQ.LT.EPS) GO TO 100
         NU = DDOT (N, M, 1, S, 1)
         MU = NU - MSQ
         XX = DDOT (N, M, 1, WUN, 1) / MSQ
         DO 410 I = 1,N
            WUN(I) = WUN(I) - XX * M(I)
 410        CONTINUE
         UTU = DNRM2(N,WUN,1)**2
         XX = DDOT (N, M, 1, WUN, 1)
         IF ((XX.LT.TINYC) .OR. ((1D3*XX)**2.LT.MSQ*UTU)) GO TO 450
            DO 420 I = 1,N
               WUN(I) = 0.0D0
 420           CONTINUE
            NSQ = 0.0D0
            GO TO 500
C
C                                       Step 4A:
 450     UTS = DDOT (N, WUN, 1, S, 1)
         IF (UTU.EQ.0.0) THEN
            ATPT = 'UTU  450'
            GO TO 890
            END IF
         XX = UTS / UTU
         DO 460 I = 1,N
            WUN(I) = XX * WUN(I)
 460        CONTINUE
         NSQ = UTS * XX
C
C                                       Step 5:
 500  IF (MSQ.EQ.0.0) THEN
         ATPT = 'MSQ  650'
         GO TO 890
         END IF
      XX = NU / MSQ
      B = NSQ + MU * XX
      IF (B.GE.EPS) GO TO 600
         DO 510 I = 1,N
            WUN(I) = S(I) - XX * M(I)
 510        CONTINUE
         NSQ = B0 - MU * XX
         B = B0
C
C                                       Step 6:
 600  IF (MU*NU.LT.MSQ*NSQ) GO TO 650
         GAMMA = 0.0D0
         IF (MU.EQ.0.0) THEN
            ATPT = 'MU   650'
            GO TO 890
            END IF
         DELTA = SQRT (NU/MU)
         GO TO 700
C                                       Step 6A:
 650  A = B - MU
      C = B + NU
      IF (A*B.EQ.0.0) THEN
         ATPT = 'A B  650'
         GO TO 890
         END IF
      IF (MSQ*NSQ.EQ.0.0) THEN
         ATPT = 'MSQ  650'
         GO TO 890
         END IF
      GAMMA = SQRT ((1.0D0-MU*NU/(MSQ*NSQ))/(A*B))
      DELTA = SQRT (C/A)
      IF (C.LT.A) GAMMA = -GAMMA
C                                       Step 7:
 700  XX = NSQ * GAMMA
      ALF = NU + MU * DELTA + MSQ * XX
      IF (ALF.EQ.0.0) THEN
         ATPT = 'ALF  700'
         GO TO 890
         END IF
      T1 = DELTA - XX
      T2 = GAMMA * NU
      T3 = (1.0D0+XX) / ALF
      T4 = -GAMMA * MU / ALF
      XX = MU*NU/ALF
      T5 = NSQ * (1.0D0 + GAMMA*XX)
      T6 = -(1.0D0+DELTA) * XX
      DO 710 I = 1,N
         P(I) = T1*M(I) + T2*WUN(I)
         Q(I) = T3*M(I) + T4*WUN(I)
         WUN(I) = T5*M(I) + T6*WUN(I)
 710     CONTINUE
      QTK0 = DDOT (N, Q, 1, K0, 1)
      DO 730 I = 1,N
         K0(I) = K0(I) + QTK0*P(I)
         DO 720 L = 1,N
            AX(L) = XJ(I,L)
 720        CONTINUE
         XX = DDOT (N, AX, 1, Q, 1)
         DO 730 J = 1,N
            XJ(I,J) = XJ(I,J) + XX * P(J)
 730        CONTINUE
      IF (NSQ.GT.0.0D0) GO TO 100
         DO 740 I = 1,N
            WUN(I) = K0(I)
 740        CONTINUE
         GO TO 100
C                                       zero divide
 890  IER = 2
      RPEAK = 5.0 / RSCALE
      WRITE (MSGTXT,1890) ATPT, RPEAK
      CALL MSGWRT (6)
C                                       Exit:
 900  DO 920 I = 1,N
         XI(I) = X0(I)
         DO 910 J = 1,N
            AX(J) = XJ(I,J)
 910        CONTINUE
         ERR(I) = DNRM2(N,AX,1)
 920     CONTINUE
      FOPT = F0
      GNOPT = GN
      IF (NPR.GT.0) THEN
         IF (IER.EQ.0) THEN
            MSGTXT = '***  Convergence achieved.'
         ELSE
            MSGTXT = '***  Maximum number of iterations reached.'
            END IF
         IF (IER.NE.2) CALL DOPRT
         IF ((IER.NE.0) .OR. (IPTLEV.GE.1)) THEN
            WRITE (MSGTXT,1050) NF, NG
            CALL DOPRT
            END IF
         IF ((IER.NE.0) .OR. (IPTLEV.GE.2)) THEN
            MSGTXT = 'Solution parameters:'
            CALL DOPRT
            I1 = 1
 930        I2 = I1 + 5
               IF (I2.GT.N) I2 = N
               WRITE (MSGTXT,1021) (XI(I), I = I1,I2)
               CALL DOPRT
               I1 = I2 + 1
               IF (I1.LE.N) GO TO 930
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Iteration #',I4,3X,' F=',1PD16.8,' Gradient=',1PD16.8)
 1021 FORMAT (1PD12.5,5(1PD13.5))
 1050 FORMAT (I4,' Function evaluations and ',I4,
     *   ' gradient evaluations.')
 1890 FORMAT ('DVDMIN: ZERO DIVIDE AT ',A,' AVOIDED, PEAK',1PE11.4)
      END
      DOUBLE PRECISION FUNCTION DDOT (N, DX, INCX, DY, INCY)
C-----------------------------------------------------------------------
C     Forms the dot product of two vectors.
C     uses unrolled loops for increments equal to one.
C     Jack Dongarra, LINPACK, 3/11/78.
C-----------------------------------------------------------------------
      DOUBLE PRECISION DX(*), DY(*), DTEMP
      INTEGER   I, INCX, INCY, IX, IY, M, MP1, N
C-----------------------------------------------------------------------
      DDOT = 0.0D0
      DTEMP = 0.0D0
      IF (N.LE.0) RETURN
      IF ((INCX.EQ.1) .AND. (INCY.EQ.1)) GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DTEMP = DTEMP + DX(IX)*DY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      DDOT = DTEMP
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DTEMP = DTEMP + DX(I)*DY(I)
   30 CONTINUE
      IF( N .LT. 5 ) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DTEMP = DTEMP + DX(I)*DY(I) + DX(I + 1)*DY(I + 1) +
     *   DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE
   60 DDOT = DTEMP
      RETURN
      END
      DOUBLE PRECISION FUNCTION DMACH(JOB)
C-----------------------------------------------------------------------
      INTEGER   JOB
C
C     SMACH COMPUTES MACHINE PARAMETERS OF FLOATING POINT
C     ARITHMETIC FOR USE IN TESTING ONLY.  NOT REQUIRED BY
C     LINPACK PROPER.
C
C     IF TROUBLE WITH AUTOMATIC COMPUTATION OF THESE QUANTITIES,
C     THEY CAN BE SET BY DIRECT ASSIGNMENT STATEMENTS.
C     ASSUME THE COMPUTER HAS
C
C        B = BASE OF ARITHMETIC
C        T = NUMBER OF BASE  B  DIGITS
C        L = SMALLEST POSSIBLE EXPONENT
C        U = LARGEST POSSIBLE EXPONENT
C
C     THEN
C
C        EPS = B**(1-T)
C        TINY = 100.0*B**(-L+T)
C        HUGE = 0.01*B**(U-T)
C
C     DMACH SAME AS SMACH EXCEPT T, L, U APPLY TO
C     DOUBLE PRECISION.
C
C     CMACH SAME AS SMACH EXCEPT IF COMPLEX DIVISION
C     IS DONE BY
C
C        1/(X+I*Y) = (X-I*Y)/(X**2+Y**2)
C
C     THEN
C
C        TINY = SQRT(TINY)
C        HUGE = SQRT(HUGE)
C
C
C     JOB IS 1, 2 OR 3 FOR EPSILON, TINY AND HUGE, RESPECTIVELY.
C
      DOUBLE PRECISION EPS,TINY,HUGE,S
C-----------------------------------------------------------------------
      EPS = 1.0D0
   10 EPS = EPS/2.0D0
      S = 1.0D0 + EPS
      IF (S .GT. 1.0D0) GO TO 10
      EPS = 2.0D0*EPS
C
      S = 1.0D0
   20 TINY = S
      S = S/16.0D0
      IF (S*1.0 .NE. 0.0D0) GO TO 20
      TINY = (TINY/EPS)*100.0
      HUGE = 1.0D0/TINY
C
      IF (JOB.EQ.2) THEN
         DMACH = TINY
      ELSE IF (JOB.EQ.3) THEN
         DMACH = HUGE
      ELSE
         DMACH = EPS
         END IF
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DNRM2 (N, DX, INCX)
C-----------------------------------------------------------------------
      INTEGER   N, INCX, NEXT, NN, I, J
      DOUBLE PRECISION   DX(*), CUTLO, CUTHI, HITEST, SUM, XMAX
C-----------------------------------------------------------------------
C     EUCLIDEAN NORM OF THE N-VECTOR STORED IN DX() WITH STORAGE
C     INCREMENT INCX .
C     IF    N .LE. 0 RETURN WITH RESULT = 0.
C     IF N .GE. 1 THEN INCX MUST BE .GE. 1
C
C           C.L.LAWSON, 1978 JAN 08
C
C     FOUR PHASE METHOD     USING TWO BUILT-IN CONSTANTS THAT ARE
C     HOPEFULLY APPLICABLE TO ALL MACHINES.
C         CUTLO = MAXIMUM OF  DSQRT(U/EPS)  OVER ALL KNOWN MACHINES.
C         CUTHI = MINIMUM OF  DSQRT(V)      OVER ALL KNOWN MACHINES.
C     WHERE
C         EPS = SMALLEST NO. SUCH THAT EPS + 1. .GT. 1.
C         U   = SMALLEST POSITIVE NO.   (UNDERFLOW LIMIT)
C         V   = LARGEST  NO.            (OVERFLOW  LIMIT)
C
C     BRIEF OUTLINE OF ALGORITHM..
C
C     PHASE 1    SCANS ZERO COMPONENTS.
C     MOVE TO PHASE 2 WHEN A COMPONENT IS NONZERO AND .LE. CUTLO
C     MOVE TO PHASE 3 WHEN A COMPONENT IS .GT. CUTLO
C     MOVE TO PHASE 4 WHEN A COMPONENT IS .GE. CUTHI/M
C     WHERE M = N FOR X() REAL AND M = 2*N FOR COMPLEX.
C
C     VALUES FOR CUTLO AND CUTHI..
C     FROM THE ENVIRONMENTAL PARAMETERS LISTED IN THE IMSL CONVERTER
C     DOCUMENT THE LIMITING VALUES ARE AS FOLLOWS..
C     CUTLO, S.P.   U/EPS = 2**(-102) FOR  HONEYWELL.  CLOSE SECONDS ARE
C                   UNIVAC AND DEC AT 2**(-103)
C                   THUS CUTLO = 2**(-51) = 4.44089E-16
C     CUTHI, S.P.   V = 2**127 FOR UNIVAC, HONEYWELL, AND DEC.
C                   THUS CUTHI = 2**(63.5) = 1.30438E19
C     CUTLO, D.P.   U/EPS = 2**(-67) FOR HONEYWELL AND DEC.
C                   THUS CUTLO = 2**(-33.5) = 8.23181D-11
C     CUTHI, D.P.   SAME AS S.P.  CUTHI = 1.30438D19
C     DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
C     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /
      DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
C-----------------------------------------------------------------------
      NEXT = 1
      IF (N .GT. 0) GO TO 10
         DNRM2  = 0.0D0
         GO TO 300
C
 10   NEXT = 1
      SUM = 0.0D0
      NN = N * INCX
C                                                 BEGIN MAIN LOOP
      I = 1
 20      GO TO (30, 50, 70, 110), NEXT
 30   IF (ABS(DX(I)) .GT. CUTLO) GO TO 85
      NEXT = 2
      XMAX = 0.0D0
C
C                        PHASE 1.  SUM IS ZERO
C
 50   IF (DX(I) .EQ. 0) GO TO 200
      IF (ABS(DX(I)) .GT. CUTLO) GO TO 85
C
C                                PREPARE FOR PHASE 2.
      NEXT = 3
      GO TO 105
C
C                                PREPARE FOR PHASE 4.
C
 100  I = J
      NEXT = 4
      SUM = (SUM / DX(I)) / DX(I)
 105  XMAX = ABS(DX(I))
      GO TO 115
C
C                   PHASE 2.  SUM IS SMALL.
C                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
C
   70 IF( ABS(DX(I)) .GT. CUTLO ) GO TO 75
C
C                     COMMON CODE FOR PHASES 2 AND 4.
C                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
C
  110 IF( ABS(DX(I)) .LE. XMAX ) GO TO 115
         SUM = 1.0D0 + SUM * (XMAX / DX(I))**2
         XMAX = ABS(DX(I))
         GO TO 200
C
  115 SUM = SUM + (DX(I)/XMAX)**2
      GO TO 200
C
C
C                  PREPARE FOR PHASE 3.
C
   75 SUM = (SUM * XMAX) * XMAX
C
C
C     FOR REAL OR D.P. SET HITEST = CUTHI/N
C     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
C
   85 HITEST = CUTHI/REAL( N )
C
C                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
C
      DO 95 J =I,NN,INCX
      IF(ABS(DX(J)) .GE. HITEST) GO TO 100
   95    SUM = SUM + DX(J)**2
      DNRM2 = SQRT( SUM )
      GO TO 300
C
  200 CONTINUE
      I = I + INCX
      IF ( I .LE. NN ) GO TO 20
C
C              END OF MAIN LOOP.
C
C              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
C
      DNRM2 = XMAX * SQRT(SUM)
  300 CONTINUE
      RETURN
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
      INCLUDE 'JMFIT.INC'
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
      INCLUDE 'JMFIT.INC'
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
