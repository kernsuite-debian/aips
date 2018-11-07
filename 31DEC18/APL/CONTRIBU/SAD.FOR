LOCAL INCLUDE 'SAD.INC'
C                                       Local include for SAD
      INTEGER BOXSIZ, MAXFND
      PARAMETER (BOXSIZ=4096, MAXFND=400)
      CHARACTER TITL1*132, TITL2*132, SCRTCH*132, INNA*36, OUTNA*36,
     &   SORT*1
      INTEGER   OUTBLK(256), LUN1, INSL, OUTSL, DEPTH(5), NGAUSS, NITER,
     *   XTYPE(4), IVAR(24), JVAR(24), NX, NY, NPTS, PTMAX, PTMIN,
     *   NPARM, NVAR, LUNPRT, INDPRT, LUNP, INDP, ILINE, IPAGE,
     *   LUN2, DOCC, DOMSG, DOCONV, ISORT(MAXFND)
      REAL      WIN(4), G(6,4), E(6,4), CB(3), DATA(BOXSIZ), DMAX, DMIN,
     *   OFFSET, DOCRT, CUTT, OUTR(256), GLIST(6, MAXFND), ELIST(6,
     *   MAXFND), YLOW(MAXFND), YHI(MAXFND)
      HOLLERITH OUTH(256)
      LOGICAL NOCLN, DOPNT, DOOUT, MULTI
      DOUBLE PRECISION RESID(BOXSIZ), OUTD(128)
      COMMON /IMFIO/ WIN, OUTBLK, LUN1, LUN2, INSL, OUTSL, DEPTH
      COMMON /IMFCHR/ INNA, OUTNA, SORT
      COMMON /IMMOD/ G, E, CB, NGAUSS, NITER, XTYPE, IVAR, JVAR,
     &               NOCLN, DOCC, DOMSG, DOCONV, DOOUT, DOPNT,
     &               MULTI
      COMMON /IMFDA/ DATA, DMAX, DMIN, OFFSET,
     *   NX, NY, NPTS, PTMAX, PTMIN, NPARM, CUTT, GLIST, ELIST,
     *   YLOW, YHI, ISORT
      COMMON /FRED/ RESID, NVAR, LUNPRT, INDPRT
      COMMON /PRTCOM/ DOCRT, LUNP, INDP, ILINE, IPAGE
      COMMON /PRTCHR/ TITL1, TITL2, SCRTCH
      EQUIVALENCE (OUTR, OUTBLK, OUTH, OUTD)
LOCAL END
      PROGRAM SAD
C-----------------------------------------------------------------------
C! Fits Gaussians to an image
C# Map Modeling
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
C   SAD (Search and Destroy) is a two-dimensional Gaussian
C   fitting program for image
C   modelling.  Up to four Gaussian components can be fit to a part of
C   an image.
C   Inputs:
C      USERID                   Owner of the image
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
C      NGAUSS                   Max. Number of Gaussians to fit
C      PCUT                     Min. Search Level
C      SORT                     Sort order of output listing
C      DOCRT                    Where to print output
C      DORESID                  Create a residual file
C      DOMODEL                  Create a CC file with Input file
C      DOALL                    Fit multiple peaks
C      DOWIDTH                  Fit extended rather than point sources
C
C-----------------------------------------------------------------------
      EXTERNAL FXDVD
      CHARACTER PRGNAM*6
      INTEGER   IER, IERR, INF, I, IRET, NPR, NTOT, NPK, NPKMAX, NFAIL,
     *   NGOOD, DELPK, IPK, JPK, IPT
      REAL      SUMSQ, RPARM(37), RPRT, RMS, LOCBLC(7), LOCTRC(7),
     *   RMSLIM, A, B, SINE, COSINE, DELTAX, DELTAY, DELXY, BLC(7),
     *   TRC(7), PI, CBPA, SBPA
      DOUBLE PRECISION    VALVAR(24), EPS, FOPT, GNOPT, ERRDVD(24)
      INCLUDE 'SAD.INC'
      INTEGER PKWIN(4, MAXFND)
      LOGICAL CONVRG (MAXFND)
C
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGNAM /'SAD   '/
      DATA PI /3.1415629/
C-----------------------------------------------------------------------
      IRET  = 8
      NPARM = 37
      IERR  = 0
C                                        Initialize POPS
      CALL TSKBEG (PRGNAM, NPARM, RPARM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 900
         END IF
C                                        Get inputs, open map
      IF (RPARM(33).GT.0.5) THEN
         DOMSG = 1
      ELSE IF (RPARM(33).LT.-0.5) THEN
         DOMSG = -1
      ELSE
         DOMSG = 0
         END IF
      DOOUT  = RPARM(34).GT.0.
      DOPNT  = RPARM(37).LT.0.
      MULTI  = RPARM(36).GE.0.
      DOCC   = RPARM(35) + .01
      CALL H2CHR (1, 1, RPARM(32), SORT)
C
      LUN1 = 16
      LUN2 = 17
      CALL SADOPN (RPARM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 900
         END IF
      IF (DOPNT .AND. NOCLN) THEN
         MSGTXT = 'Cant fit point sources if I dont know CLEAN BEAM'
         IERR = 10
         GO TO 900
         END IF
C                                       Window
      CALL RCOPY (7, RPARM(9),  BLC)
      CALL RCOPY (7, RPARM(16), TRC)
C                                       Cutoff level for search
C                                       is determined either by
C                                       by user value or rms.
      RMSLIM = 3.0
C??      SIZE = 2.0
      CUTT = RPARM(31)
      IF (CUTT.LE.0.) THEN
         CALL GETRMS (BLC, TRC, RMS, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1030) IERR
            GO TO 900
         ELSE
            WRITE (MSGTXT, 1031) RMSLIM, RMS
            END IF
         CUTT = RMSLIM * RMS
      ELSE
         WRITE (MSGTXT, 1032) CUTT
         END IF
      CALL MSGWRT (4)
C                               How may sources to look for?
C                               Print it out along with subfield
      NPKMAX = RPARM(30) + .01
      IF (NPKMAX.LE.0) NPKMAX = 10
      IF (NPKMAX.GT.MAXFND) NPKMAX = MAXFND
      WRITE (MSGTXT, 1034) NPKMAX
      CALL MSGWRT (4)
      WRITE (MSGTXT, 1035) 'BLC: ', BLC
      CALL MSGWRT (4)
      WRITE (MSGTXT, 1035) 'TRC: ', TRC
      CALL MSGWRT (4)
C                               Look for continuous regions above CUTT
      CALL ISLAND (BLC, TRC, PKWIN, NPK, NPKMAX, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'SAD: ISLAND ERROR'
         GO TO 900
      ELSE
         WRITE (MSGTXT, 1040) NPK
         CALL MSGWRT (4)
         END IF
C                                        Clean beam
      CB(1) = ABS (CATR(KRBMJ))
      NOCLN = CB(1).LE.0.0
      IF (.NOT.NOCLN) THEN
         CB(2) = ABS (CATR(KRBMN))
         CB(3) = CATR(KRBPA) - ROT(LOCNUM)
         CBPA  = COS (COND2R * CB(3))
         SBPA  = SIN (COND2R * CB(3))
C                                       Change from degrees to pixels
         CB(1) = CB(1) * SQRT ((SBPA/AXINC(1,LOCNUM))**2 +
     *      (CBPA/AXINC(2,LOCNUM))**2)
         CB(2) = CB(2) * SQRT ((CBPA/AXINC(1,LOCNUM))**2 +
     *      (SBPA/AXINC(2,LOCNUM))**2)
         CB(3) = (180./PI)*ATAN2 (SBPA/ABS(AXINC(1,LOCNUM)),
     *      CBPA/ABS(AXINC(2,LOCNUM)))
         IF (CB(1).LT.CB(2)) CB(3) = 90. + CB(3)
         IF (CB(3).GT.90.) CB(3) = CB(3) - 180.
C                                       If not cleaned use 2 pixels
      ELSE
         CB(1) = 2.
         CB(2) = 2.
         CB(3) = 0.
         CBPA  = 1.
         SBPA  = 0.
         END IF
C                                       If islands are too small use
C                                       default search area of Clean
C                                       beam DELTAX and Y are extents in
C                                       X and Y where Clean beam falls
C                                       by about 2.
      A = CB(1)
      B = CB(2)
      DELTAX = ((SBPA/A)**2 + (CBPA/B)**2)
      DELTAY = ((CBPA/A)**2 + (SBPA/B)**2)
      DELXY  = ((1./A)**2 - (1/B)**2)*(SBPA*CBPA)**2
      A      = SQRT (0.25/(DELTAY - DELXY**2/DELTAX))
      DELTAX = SQRT (0.25/(DELTAX - DELXY**2/DELTAY))
      DELTAY = A
C                                       Loop over potential sources
      CALL RFILL (7, 1.0, LOCBLC)
      CALL RFILL (7, 1.0, LOCTRC)
      NFAIL = 0
      DELPK = NPK
      DO 200 IPK = 1,NPK
         LOCBLC(1) = PKWIN(1, IPK)
         LOCBLC(2) = PKWIN(2, IPK)
         LOCTRC(1) = PKWIN(3, IPK)
         LOCTRC(2) = PKWIN(4, IPK)
C                                       Set minimum and max sizes
         IF (LOCTRC(1) - LOCBLC(1).LT.2.*DELTAX) THEN
            LOCTRC(1) = LOCTRC(1) + DELTAX
            LOCBLC(1) = LOCBLC(1) - DELTAX
            END IF
         IF (LOCTRC(2) - LOCBLC(2).LT.2.*DELTAY) THEN
            LOCTRC(2) = LOCTRC(2) + DELTAY
            LOCBLC(2) = LOCBLC(2) - DELTAY
            END IF
         DO 150 I = 1,2
            IF (LOCTRC(I) - LOCBLC(I).GT.64) THEN
               A = 0.5 * (LOCTRC(I) + LOCBLC(I))
               LOCTRC(I) = A + 31.
               LOCBLC(I) = A - 31.
               END IF
 150        CONTINUE
C                               Read in data in subwindow
         CALL SADDAT (LOCBLC, LOCTRC, DATA, NTOT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1070) IERR
            GO TO 900
            END IF
C                                        Insert defaults
C                                        Store variables properly
         CALL SADVST (VALVAR)
         NPR = 0
         DO 160 I = 1, NVAR
            ERRDVD(I) = 1.0D1
 160        CONTINUE
         EPS = 1.D-10
         NITER = 40 * NGAUSS
C                                       Call fitting routine
         CALL DVDMIN (FXDVD, VALVAR, ERRDVD, NVAR, EPS, NITER, FOPT,
     *      GNOPT, INF, NPR, RPRT)
C                               If convergence, calculate RMS
C                               find errors in fit values
C                               and save parameters in GLIST,ELIST
         CONVRG(IPK) = INF.EQ.0
         IF (CONVRG(IPK)) THEN
            SUMSQ = 0.
            DO 170 IPT = 1,NPTS
               IF (DATA(IPT).NE.FBLANK) SUMSQ = SUMSQ + RESID(IPT)**2
 170           CONTINUE
            IF (NTOT.GT.0) SUMSQ = SQRT (SUMSQ /(NTOT - NVAR))
C                               If NGAUSS is 1, simply copy G
C                               into GLIST, and scale ERRDVD into
C                               ELIST by SUMSQ
C                               If NGAUSS > 1, mark the current
C                               value of IPK as invalid, and
C                               write the multiple found sources
C                               at the end of the list
            IF (NGAUSS.EQ.1) THEN
               JPK = IPK - 1
C                               DELPK points to end of current list
            ELSE
               JPK   = DELPK
               DELPK = DELPK + NGAUSS
               END IF
C                               Mark current position false
            CONVRG(IPK) = .FALSE.
C
C                               Copy errors into ELIST
            IF (NTOT.GT.0) THEN
               DO 180 I = 1, NVAR
                  ELIST(JVAR(I), JPK+IVAR(I)) = ERRDVD(I) * SUMSQ
 180              CONTINUE
               END IF
C                               Copy estimates into GLIST
            DO 190 I = 1,NGAUSS
               JPK = JPK + 1
               CALL RCOPY (6, G(1, I), GLIST(1, JPK))
C                               If we've added new sources
C                               onto end of list, copy
C                               search window too.
               CALL RCOPY (4, PKWIN(1, IPK), PKWIN(1, JPK))
C                               Reset validity flag
               CONVRG(JPK) = .TRUE.
 190           CONTINUE
         ELSE
            NFAIL = NFAIL + 1
C                               Tell user this source didnt work
            IF (NFAIL.LE.25) THEN
               WRITE (MSGTXT, 1075) IPK, LOCBLC(1), LOCBLC(2),
     &            LOCTRC(1), LOCTRC(2)
               CALL MSGWRT (4)
               END IF
            END IF
 200     CONTINUE
C                               Reset end of list
      NPK = DELPK
C                               Tell user how he fared.
      WRITE (MSGTXT, 1080) NPK - NFAIL, NFAIL
      CALL MSGWRT (4)
      NGAUSS = 1
C                               We can make a residual map
      IF (DOOUT) THEN
C                               Create output file
         CALL MAKO (RPARM, BLC, TRC, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1200) IERR
            GO TO 900
            END IF
         NGOOD = 0
C                               Subtract out components
         DO 210 I = 1,NPK
            IF (.NOT.CONVRG(I)) GO TO 210
            NGOOD = NGOOD + 1
C                               How big an area do we need
            SINE = SIN(GLIST(6,I))
            COSINE = COS(GLIST(6, I))
            A = GLIST(4, I)
            B = GLIST(5, I)
            DELTAX = ((SINE/A)**2 + (COSINE/B)**2)
            DELTAY = ((COSINE/A)**2 + (SINE/B)**2)
            DELXY  = ((1./A)**2 - (1/B)**2)*(SINE*COSINE)**2
C                               This pushes down the exp by 10**-4
            DELTAY = SQRT (3.3/(DELTAY - DELXY**2/DELTAX))
            YLOW(I) = MAX (GLIST(3, I) - DELTAY, WIN(2))
            YHI(I)  = MIN (GLIST(3, I) + DELTAY, WIN(4))
            ISORT(NGOOD)= I
 210        CONTINUE
C                               Sort lower limits
         CALL BUBS (NGOOD, ISORT, YLOW)
C                               Calculate and subtract gaussians
         CALL SUBMOD (NGOOD, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1220) IERR
            GO TO 900
            END IF
         CALL FILCLS (LUN2)
         END IF
C                               Put output on screen or CC files
      CALL SADOUT (CONVRG, NPK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1300) IERR
         GO TO 900
         END IF
      CALL FILCLS (LUN1)
C                               Copy history files
      IF (DOOUT) CALL SADHI (RPARM, NPKMAX, NPK, NGOOD, IERR)
C                                        Finished, no errors
      IRET = 0
      GO TO 980
C                                        Error return
 900  IER = 1
      CALL MSGWRT (8)
C                                        Normal ending
 980  CALL TSKEND (IRET)
C
 999  STOP
C----------------------------------------------------------------------
 1010 FORMAT ('TSKBEG error ',I6)
 1020 FORMAT ('SADOPN error ',I6)
 1030 FORMAT ('GETRMS error ',I6)
 1031 FORMAT ('Setting Search limit at ',F3.1,' x RMS of ',F6.4)
 1032 FORMAT ('Searching down to ',F10.3)
 1034 FORMAT ('Looking for up to',I4,' sources in subfield:')
 1035 FORMAT (A,7F8.0)
 1040 FORMAT ('Found ',I6,' Islands')
 1070 FORMAT ('SADDAT Error ',I6)
 1075 FORMAT ('No convergence for island',I4,' Box ',4F7.0)
 1080 FORMAT ('Sucessfully solved ',I6,' sources,',I6,
     *   ' FAILED TO CONVERGE')
 1200 FORMAT ('MAKOUT Error ',I6)
 1220 FORMAT ('SUBMOD Error ',I6)
 1300 FORMAT ('SAD: Error ',I6,' writing output')
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
      REAL      STH2, CTH2, S2TH, C2TH, MJ, MN, VA, VB, VC, VD, X, Y,
     *   X2, Y2, XY, CON, TWORFV, TWOCON, FV, G4C, G5C
      DOUBLE PRECISION P(*), F, GRAD(*), GTEMP(24), DNRM2
      INCLUDE 'SAD.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA CON /2.772589/
C-----------------------------------------------------------------------
      TWOCON = 2.0*CON
      NPTS = NX*NY
      NK = 0
      DO 10 K = 1,NPTS
         IF (DATA(K).EQ.FBLANK) GO TO 10
            NK = NK+1
            RESID(NK) = -DATA(K)
 10      CONTINUE
      NTOT = NK
      DO 20 I = 1,NVAR
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
C
      IX1 = WIN(1) + 0.5
      IY1 = WIN(2) + 0.5
      K = -6
      DO 80 I = 1,NGAUSS
         K = K+6
         STH2 = SIN(G(6,I))**2
         CTH2 = COS(G(6,I))**2
         S2TH = -SIN(2.0*G(6,I))
         C2TH = COS(2.0*G(6,I))
         MJ = G(4,I)**2/CON
         MN = G(5,I)**2/CON
         VA = CTH2/MJ+STH2/MN
         VB = STH2/MJ+CTH2/MN
         VC = S2TH*(1.0/MN-1.0/MJ)
         NK = 0
         DO 70 L = 1,NPTS
            IF (DATA(L).EQ.FBLANK) GO TO 70
               NK = NK+1
               X = IX1+MOD(L-1,NX)-G(2,I)
               Y = IY1+AINT((L-0.5)/NX)-G(3,I)
               X = ((VA*X+VC*Y)*X+VB*Y**2)
C                               limit accuracy to 10**-4 to save time
               IF (X.LT.9.2) THEN
                  FV = G(1,I)*EXP(-X)
               ELSE
                  FV = 0.0
                  END IF
               RESID(NK) = RESID(NK)+FV
 70         CONTINUE
 80      CONTINUE
      F = DNRM2 (NTOT, RESID, 1)**2
      IF (IFLAG.EQ.1) RETURN
      DO 90 I = 1,24
         GTEMP(I) = 0D0
 90      CONTINUE
      K = -6
      DO 140 I = 1,NGAUSS
         K = K + 6
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
         NK = 0
         DO 130 L = 1,NPTS
            IF (DATA(L).EQ.FBLANK) GO TO 130
               NK = NK+1
               X = IX1+MOD(L-1,NX)-G(2,I)
               Y = IY1+AINT((L-0.5)/NX)-G(3,I)
               X2 = X**2
               Y2 = Y**2
               XY = X*Y
               FV = G(1,I)*EXP(-(VA*X2+VB*Y2+VC*XY))
               TWORFV = 2.0*RESID(NK)*FV
               IF (E(1,I).GE.0.) GTEMP(K+1) = GTEMP(K+1)
     *            +TWORFV/G(1,I)
               IF (E(2,I).GE.0.) GTEMP(K+2) = GTEMP(K+2)
     *            +TWORFV*(2.0*X*VA+Y*VC)
               IF (E(3,I).GE.0.) GTEMP(K+3) = GTEMP(K+3)
     *            +TWORFV*(2.0*Y*VB+X*VC)
               IF (E(4,I).GE.0.) GTEMP(K+4) = GTEMP(K+4) + TWORFV *
     *            G4C*(X2*CTH2+Y2*STH2-XY*S2TH)
               IF (E(5,I).GE.0.) GTEMP(K+5) = GTEMP(K+5) + TWORFV *
     *            G5C*(X2*STH2+Y2*CTH2+XY*S2TH)
               IF (E(6,I).GE.0.) GTEMP(K+6) = GTEMP(K+6) + TWORFV *
     *            (VC*(X2-Y2)+2.0*VD*XY)
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
C
 999  RETURN
      END
      SUBROUTINE SADOPN (RPARM, IERR)
C-----------------------------------------------------------------------
C   Open the input map and get the header
C   Inputs:
C      RPARM(35)  R      Input parameters
C   Outputs:
C      IERR       I      Error return  0-> okay
C                           1-> error return
C-----------------------------------------------------------------------
      INTEGER   IERR
      LOGICAL   T, F
      REAL      RPARM(*)
      HOLLERITH HOLMA
      INCLUDE 'SAD.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBUF.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                        Store name for easy I/O
      CALL CHR2H (4, 'MA  ', 1, HOLMA)
      CALL H2WAWA (RPARM(2), RPARM(5), RPARM(7), HOLMA, RPARM(8),
     *   RPARM(1), INNA)
C                                        Open input map
      CALL OPENCF (LUN1, INNA, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 900
            END IF
C                                        Get header values
      CALL GETHDR (LUN1, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 900
            END IF
C                                        Error return
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
      RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SADOPN: COULD NOT OPEN INPUT MAP.  IER=',I7)
 1020 FORMAT ('SADOPN: COULD NOT GET HEADER.  IER=',I7)
      END
      SUBROUTINE SADDAT (BLC, TRC, ARRAY, NGOOD, IER)
C-----------------------------------------------------------------------
C   Subroutine SADDAT reads the input data window into the ARRAY
C   It finds the value and relative position of the maximum in the
C   Array.  If MULTI is .TRUE. it will also see if there are
C   multiple maxima above the cuttoff level CUTT.  If there is
C   only 1 maximum, it calls SADDEF to fix the positions and
C   widths of the initial guess to the gaussian fit.  If there
C   are 2 to 4 maxima, it calls MULDEF for similar purposes.
C
C   INPUTS:
C     BLC       R(7)      Specify bottom left corner of subarray
C     TRC       R(7)      Specify top right corner of subarry
C   FROM COMMONS:
C     CUTT      R         Search level for peaks
C     MULTI     L         Look for multiple peaks
C   OUTPUTS:
C     ARRAY     R(*)      Returned subarray
C     NGOOD     I         Number of unflagged points in subarray
C     IER       I         Error return 0 -> okay
C                            1 -> error
C                            2 -> ill window
C   COMMON OUTPUTS:
C     NX        I         Size of subarray in X
C     NY        I         Size of subarray in Y
C     WIN(4)    R         Actual window region (clipped by edges of map)
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7), ARRAY(*)
      INTEGER   IER, NGOOD
C
      INTEGER   IERR, PTS, I, J, IXMAX, IYMAX, XPK(4), YPK(4), NMPK,
     *   IPTS, IDY, IDX, IPT, IFIL, KMPK
      REAL      SPK(4)
      LOGICAL   ISMAX
      INCLUDE 'SAD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Initialize
      IER = 2
      CALL MAPWIN (LUN1, BLC, TRC, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'SADDAT: MAPWIN ERROR'
         GO TO 900
         END IF
      DMAX = -1.0E25
      PTS = 1
C                                      get real values of win from filtab
      CALL FILNUM (LUN1, IFIL, IERR)
      WIN(1) = FILTAB(POBLC,   IFIL)
      WIN(2) = FILTAB(POBLC+1, IFIL)
      WIN(3) = FILTAB(POTRC,   IFIL)
      WIN(4) = FILTAB(POTRC+1, IFIL)
      NX = WIN(3) - WIN(1) + 1
      NY = WIN(4) - WIN(2) + 1
C                                       Line loop
      IER = 1
      NGOOD = 0
      DO 40 J = 1,NY
         CALL MAPIO ('READ', LUN1, ARRAY(PTS), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) J, IERR
            GO TO 900
            END IF
C                                       Row loop, fill data
         DO 30 I = 1,NX
C                                       Look for extremum
            IF (ARRAY(PTS).NE.FBLANK) THEN
               NGOOD = NGOOD + 1
               IF (DMAX.LT.ARRAY(PTS)) THEN
                  DMAX = ARRAY(PTS)
                  PTMAX = PTS
                  IXMAX = I
                  IYMAX = J
                  END IF
               END IF
         PTS = PTS + 1
 30      CONTINUE
 40   CONTINUE
      PTS = PTS - 1
C                               Look for multiple local maxima
      IF (MULTI) THEN
         NMPK = 0
C                               Loop over pixels excluding edges
         DO 80 J = 2, NY-1
             DO 70 I = 2, NX - 1
C                               Quit if there are already 4
                IF (NMPK.LT.4) THEN
                   ISMAX = .TRUE.
C                               Position in ARRAY
                   IPTS = (J-1)*NX + I
C                               Only count points above cutt
                   IF (ARRAY(IPTS).LT.CUTT) GO TO 68
C                               Bigger than surrounding points?
                   DO 60 IDY = -NX, NX, NX
                      DO 50 IDX = -1, 1, 1
                         IPT = IPTS + IDY + IDX
                         ISMAX = ISMAX.AND.
     &                      (ARRAY(IPTS).GE.ARRAY(IPT))
C                               Jump out if not a maximum
                        IF (.NOT.ISMAX) GO TO 68
 50                  CONTINUE
 60               CONTINUE
               ELSE
C                               Quit if there are already 4
                  GO TO 90
                  END IF
C                               This is an acceptable maximum
C                               but check that there's no adjacent
C                               point (can happen with 2 equal points)
               DO 65 KMPK = 1,NMPK
                  IF (ABS(XPK(KMPK)-I).LE.1 .AND.
     &                ABS(YPK(KMPK)-J).LE.1) GO TO 68
 65               CONTINUE
C                               Record relative position and flux
               NMPK = NMPK + 1
               XPK(NMPK) = I
               YPK(NMPK) = J
               SPK(NMPK) = ARRAY(IPTS)
C                               Jump here if you have good reason
C                               to believe this isn't a maximum
 68            CONTINUE
 70            CONTINUE
 80         CONTINUE
         END IF
C                               Jump here if we're done
C                               For single sources use 2nd moments
C                               for starting estimates
 90   IF (.NOT.MULTI .OR. NMPK.LE.1) THEN
         CALL SADDEF (ARRAY, IXMAX, IYMAX)
         NGAUSS = 1
C                               For mutiple peaks use several points
      ELSE
         NGAUSS = NMPK
         CALL MULDEF (NMPK, XPK, YPK, SPK)
         END IF
      IER = 0
C                                       Error return
 900  IF (IER.NE.0) CALL MSGWRT (8)
C
      RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SADDAT: COULD NOT READ LINE ',I7,'  IER=',I7)
      END
      SUBROUTINE SCDMOM (DMAX, SUM2, SUMD2, SUM4, A, B, THETA, SINGUL)
C----------------------------------------------------------------------
C   From the various 2nd moments find the best least-squares
C   quadratic fit to the values near the peak.  The assumed
C   form of the fit is I = dmax - a*x*x - b*x*y - c*y*y
C   If there are too few points to make a fit, or if there are other
C   problems, SINGUL will be set to be .TRUE.
C   Inputs:
C      DMAX      R       Maximum in array
C      SUM2(0:2) R       Vector of moments, SUM(I) is the sum over
C                        valid pixels of X**I * Y **(2-I)
C      SUMD2(0:2)        Sum of Flux * X**I * Y**(2-I)
C      SUM4(0:4)         Sum of X**I * Y**(4-I)
C   Outputs:
C      A         R       Estimate of major axis
C      B         R       Estimate of minor axis
C      THETA     R       Estimate of position angle (radians)
C      SINGUL    L       If .FALSE. couldn't find a decent solution
C-----------------------------------------------------------------------
      REAL      DMAX, SUM2(0:2), SUMD2(0:2), SUM4(0:4), A, B, THETA
      LOGICAL   SINGUL
C
      REAL      MAT(0:2,0:3), BMIN, BPLUS, X, SOL(0:2), TEMP, W
      INTEGER   I, J, K, PIVOT(0:2)
C-----------------------------------------------------------------------
C                                       Set up matrix for least square
C                                       solution
      DO 10 I = 0,2
         MAT(I,3) = SUMD2(I) - DMAX * SUM2(I)
         DO 5 J = 0,2
            MAT(I,J) = SUM4(I-J+2)
  5         CONTINUE
 10      CONTINUE
C                                       Pivoted Gaussian elimination
      SINGUL = .FALSE.
C                                       Reduce to Right triangular
      DO 40 I = 0,2
         X = 0.
C                                       Find pivot
         DO 20 J = 0,2
            IF (ABS(MAT(I,J)).GT.X) THEN
               X = ABS (MAT(I,J))
               PIVOT(I) = J
               END IF
 20         CONTINUE
         SINGUL = X.EQ.0.0
         IF (.NOT.SINGUL) THEN
            DO 30 J = I+1,2
               X = MAT(J,PIVOT(I)) / MAT(I,PIVOT(I))
               DO 25 K = 0,3
                  TEMP      = MAT(J,K)
                  MAT(J,K) = TEMP - MAT(I,K) * X
C                                        Anything that eliminates too
C                                        well is zero
                  IF ((K.LT.3) .AND. (ABS(MAT(J,K)).LT.1.E-4*ABS(TEMP)))
     *               MAT(J,K) = 0.
 25               CONTINUE
 30            CONTINUE
         ELSE
            GO TO 999
            END IF
 40      CONTINUE
C                                        From Right triangle find
C                                        solutions
      DO 60 I = 2,0,-1
         DO 50 J = 2,I+1,-1
            MAT(I,3) = MAT(I,3) - MAT(J,3) * MAT(I,PIVOT(J))
 50         CONTINUE
         MAT(I,3) = MAT(I,3) / MAT(I, PIVOT(I))
 60      CONTINUE
C                               Unpivot
      DO 70 I = 0,2
         SOL(PIVOT(I)) = -MAT(I,3)/DMAX
 70      CONTINUE
C                       Does solution for a,b,c make sense?
      IF (SOL(0).LT.0.  .OR.
     &    SOL(2).LT.0.  .OR.
     &    SOL(1)**2.GT.4. * SOL(0) * SOL(2)) THEN
C         SINGL = .TRUE.
         GO TO 999
         END IF
C                       Convert to bmaj, bmin, bpa
      BMIN  = SOL(2) - SOL(0)
      BPLUS = SOL(2) + SOL(0)
      W = SQRT (BMIN**2 + SOL(1)**2)
C                       empirical fudge factor
      X = 1.6 * ALOG(16.)
      B = SQRT (X/(BPLUS + W ))
      A = SQRT (X/(BPLUS - W))
      THETA = ATAN2 (SOL(1), -BMIN)
C
 999  RETURN
      END
      SUBROUTINE SADDEF (ARRAY, IXMAX, IYMAX)
C-----------------------------------------------------------------------
C     Set the beginning parameters for the fitting routine, using
C     the estimates based on moment fits of the top of the region.
C     INPUTS
C        ARRAY(*)     R     Contains data we're trying to fit
C        IXMAX        I     X-coord of max in Array
C        IYMAX        I     Y-coord of max in Array
C     COMMON
C        DMAX         R     Value of max in Array
C        WIN(4)       R     Boundaries of search area
C        G(6,1)       R     Where to put estimates
C-----------------------------------------------------------------------
      REAL      ARRAY(*)
      INTEGER   IXMAX, IYMAX
C
      INTEGER   I, J, K, PTS
      REAL      SUM2(0:2), SUMD2(0:2), SUM4(0:4), A, B, THETA, CRATIO,
     *   SLIT, X, Y, TEMP
      LOGICAL   SING
C
      INCLUDE 'SAD.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA CRATIO /0.6/
C-----------------------------------------------------------------------
C                                        For point source use clean beam
      IF (DOPNT) THEN
          CALL RCOPY (3, CB, G(4,1))
C                                        Loop around, find a lot of
C                                        moments
      ELSE
         CALL RFILL (3, 0., SUM2)
         CALL RFILL (3, 0., SUMD2)
         CALL RFILL (5, 0., SUM4)
         SLIT = CRATIO * DMAX
         PTS = 0
         DO 60 J = 1,NY
            DO 50 I = 1,NX
               PTS = PTS + 1
               IF ((ARRAY(PTS).NE.FBLANK) .AND. (ARRAY(PTS).GE.SLIT))
     *            THEN
                  X = I - IXMAX
                  Y = J - IYMAX
                  DO 20 K = 0,2
                     TEMP = 1.
                     IF (K.GT.0) TEMP = TEMP * X**K
                     IF (K.LT.2) TEMP = TEMP * Y**(2-K)
                     SUM2(K) = SUM2(K) + TEMP
                     SUMD2(K) = SUMD2(K) + ARRAY(PTS)*TEMP
 20                  CONTINUE
                  DO 30 K = 0,4
                     TEMP = 1.
                     IF (K.GT.0) TEMP = TEMP * X**K
                     IF (K.LT.4) TEMP = TEMP * Y**(4-K)
                     SUM4(K) = SUM4(K) + TEMP
 30                  CONTINUE
                  END IF
 50            CONTINUE
 60         CONTINUE
C                                       Convert moments into bmaj, bmin,
C                                       bpa
         CALL SCDMOM (DMAX, SUM2, SUMD2, SUM4, A, B, THETA, SING)
         IF (.NOT.SING) THEN
            G(4, 1) = A
            G(5, 1) = B
            G(6, 1) = THETA
C                                        Didn't work, use point spread
C                                        fn.
         ELSE
            CALL RCOPY (3, CB, G(4, 1))
            END IF
         END IF
C                                        Fill in Peak flux and position
      G(1,1) = DMAX
      G(2,1) = IXMAX + WIN(1) - 1
      G(3,1) = IYMAX + WIN(2) - 1
C
 999  RETURN
      END
      SUBROUTINE MULDEF (NMPK, XPK, YPK, SPK)
C-----------------------------------------------------------------------
C   SADDAT has found NMPK, > 1 peaks above the cuttoff
C   Set the initial guesses to be points at each peak
C   Inputs:
C      NMPK     I       Number of peaks
C      XPK(*)   R       X-coord of peaks relative to window
C      YPK(*)   R       Y-coord of peaks relative to window
C      SPK(*)   R       Flux at each peak
C
C      CB(3)    R       Size of Point Spread Function in COMMON /IMMOD/
C      WIN(4)   R       Search window, in COMMON /IMFIO/
C   Outputs:
C      G(6, *)  R       In COMMON /IMMOD/ Initial guesses
C-----------------------------------------------------------------------
      INTEGER NMPK, XPK(NMPK), YPK(NMPK)
      REAL SPK(NMPK)
C
      INTEGER I
C
      INCLUDE 'SAD.INC'
C----------------------------------------------------------------------
      DO 100 I = 1,NMPK
         G(1, I) = SPK(I)
         G(2, I) = XPK(I) + WIN(1) - 1
         G(3, I) = YPK(I) + WIN(2) - 1
         G(4, I) = CB(1)
         G(5, I) = CB(2)
         G(6, I) = CB(3)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SADVST (VALVAR)
C-----------------------------------------------------------------------
C   SADVST is a subroutine for SAD which stores the variable
C   parameters in the proper arrays.  In particular the
C   values of the G array that are not held fixed are packed
C   into VALVAR, and NVAR is set to the number of nonfixed
C   parameters.  The E array is set -1.0 for fixed variables
C   and +1.0 for variables.
C   INPUTS:  none  see COMMONs
C   OUTPUTS:
C      NVAR            I       The number of variables
C      VALVAR(24)      D       A list of initial guesses of
C                              non-fixed parameters
C   COMMONS:
C      JVAR            I       Pointers telling which parameters are
C                              are in VALVAR
C      IVAR            I       Pointers to which gaussians are in VALVAR
C      E(6,*)          R       >0. -> variable, <0. -> fixed parameter
C-----------------------------------------------------------------------
      INTEGER   I, J
      DOUBLE PRECISION    VALVAR(24)
      INCLUDE 'SAD.INC'
C-----------------------------------------------------------------------
C                                       Initialize
      NVAR = 0
      DO 40 I = 1,NGAUSS
         CALL RFILL (6, -1.0, E(1, I))
C                                       Move flux and position estimates
         DO 35 J = 1,3
            NVAR         = NVAR + 1
            VALVAR(NVAR) = G(J,I)
            JVAR(NVAR)   = J
            IVAR(NVAR)   = I
            E(J,I)       = 1.0
 35         CONTINUE
C                                       Move BMAJ, BMIN and BPA
         IF (.NOT.DOPNT) THEN
            DO 36 J = 4,6
               NVAR         = NVAR + 1
               VALVAR(NVAR) = G(J,I)
               JVAR(NVAR)   = J
               IVAR(NVAR)   = I
               E(J,I)       = 1.0
 36            CONTINUE
            END IF
  40     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SADHI (RPARM, NPKMAX, NPK, NGOOD, IER)
C-----------------------------------------------------------------------
C   SADHI creates and writes the HI file for the task SAD
C   INPUTS:
C      RPARM(99)      R     Input parameters
C   OUTPUTS:
C      IER            I     Error return  0->okay
C                             1->uh-oh
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, HILINE*72, CHTMP*2, CONAME*12,
     *   CINAME*12, COCLAS*6, CICLAS*6
      INTEGER   IER, IERR, NHISTF, LHIN, LHOUT, IVOL, OVOL, IBUFF1(256),
     *   IBUFF2(256), INSEQ, OUTSEQ, ITEMP(7), I, CUID, NPK, NPKMAX,
     *   NGOOD
      LOGICAL   T
      REAL      RPARM(*)
      INCLUDE 'SAD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA NHISTF, LHIN, LHOUT /2,27,28/
      DATA T /.TRUE./
      DATA PRGNAM /'SAD   '/
C-----------------------------------------------------------------------
C                                       Initialize HI
      CALL HIINIT (NHISTF)
      IER = 0
C                                       Create and open output HI file
      CALL WAWA2A (INNA, CINAME, CICLAS, INSEQ, CHTMP, IVOL, CUID)
      CALL WAWA2A (OUTNA, CONAME, COCLAS, OUTSEQ, CHTMP, OVOL, CUID)
      CALL HISCOP (LHIN, LHOUT, IVOL, OVOL, INSL, OUTSL, OUTBLK,
     *   IBUFF1, IBUFF2, IERR)
      IF (IERR.LE.2) GO TO 50
         WRITE (MSGTXT,1010) IERR
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
         ITEMP(I) = RPARM(I+8) + 0.5
 60      CONTINUE
      WRITE (HILINE,1060) PRGNAM, ITEMP
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      DO 70 I = 1,7
         ITEMP(I) = RPARM(I+15) + 0.5
 70      CONTINUE
      WRITE (HILINE,1070) PRGNAM, ITEMP
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      WRITE (HILINE, 1080) PRGNAM,NPKMAX, NPK, NGOOD
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      WRITE (HILINE, 1090) PRGNAM, CUTT
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (DOPNT) THEN
         HILINE = PRGNAM//'FITTED AND REMOVED POINT SOURCES ONLY'
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         END IF
      GO TO 950
C
 900  IER = 1
      CALL MSGWRT (8)
C                                       Close HI file
 950  CALL HICLOS (LHOUT, T, IBUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CANNOT COPY HI FILE.  IER=',I8)
 1060 FORMAT (A6,'BLC=',7I6)
 1070 FORMAT (A6,'TRC=',7I6)
 1080 FORMAT (A,'MAX SOURCES ',I4,' ISLANDS FOUND',I4,' SOURCES FOUND',
     *   I4)
 1090 FORMAT (A,'SEARCH LIMIT WAS',F12.3)
      END
      SUBROUTINE SADOUT (CONVRG, LPK, IER)
C-----------------------------------------------------------------------
C   SADOUT is a subroutine of SAD which prints out the parameter
C   solutions and converts them into useful coordinates.
C   Inputs:
C      CONVRG     L(*)      Validity list
C      LPK        I         How many sources, including invalid
C   From commons:
C      DOCC       I         > 0. -> add CC list to input file,
C                           > 1.5-> write convolved CCs even
C                           if deconvolving is possible
C      DOMSG      I         1-> write to crt;-1->write to msgfil;0->both
C      DOPNT      L         If .true. we fit point sources so
C                           only put points in CC file, don't deconvolve
C      NOCLN      L         There is no clean beam information available
C      GLIST(6,*) R         Fit parameter estimates
C      ELIST(6,*) R         Fit error estimates
C   Also:
C                           All kinds of coordinate information in DLOC
C   Outputs:
C      IER        I         Error return  0-> okay
C                               1-> error
C-----------------------------------------------------------------------
      LOGICAL CONVRG(*)
      INTEGER LPK
      CHARACTER RSTR(2)*20, CDUM*12, CINAME*12, CICLAS*6, CHTMP*2,
     *   UNIT*8, PREFIX(0:2)*5, MARK(3)*1, ESGTXT*80
      INTEGER   IER, I2TMP, I, J, K, ILEN(2), IERR, INSEQ, CUID, NREC,
     *   LREC, CILUN, CIVOL, CIVER, CIBUF(768), IRNO, MSGL(-1:1),
     *   LEVMIN, INEXT, KEY1, KEY2, IPK, IUS, IMARK
      LOGICAL   T, F, TRYCON
      REAL      TEMP, CON, PASUM, CAXINC(2), DUM, INT, INTE, ERR1, ERR2,
     *   DCONV(3,3), PAC, RECORD(7), SINC, COSC, RICORD(7), XXT, YYT,
     *   ITOT, ITOTE, SMAX, SMIN, SCALE, ERRA, ERRB
      DOUBLE PRECISION    X(3), XX, YY, ZZ
      INCLUDE 'SAD.INC'
      REAL      WK(2, MAXFND+2)
      INTEGER   IP(MAXFND+2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA MSGL /0, 4, 1/
      DATA PREFIX /'     ','milli','micro'/, MARK /' ','*','!'/
C-----------------------------------------------------------------------
C                                        Initialization
      IER = 0
      CON = 3.14159/180.0
      PASUM = 0.0
      IF (AXINC(1,LOCNUM)*AXINC(2,LOCNUM).LT.0.0) PASUM = 90.0
      TRYCON = (.NOT.(NOCLN .OR. DOPNT))
C                                       Create CC file
      IF (DOCC.GT.0) THEN
         CILUN = 27
         CALL WAWA2A (INNA, CINAME, CICLAS, INSEQ, CHTMP, CIVOL, CUID)
         NREC = 10
         IF (DOPNT) THEN
            LREC = 3
         ELSE
            LREC = 7
            END IF
         CIVER = 0
         IERR = 0
C                                       Open with input image
         CALL CATDIR ('CSTA', CIVOL, INSL, CDUM, CDUM, DUM, CDUM, DUM,
     *      'CLRD', CIBUF, IERR)
         CALL CATIO ('READ', CIVOL, INSL, CATBLK, 'WRIT', CIBUF, IERR)
         IF (IERR.NE.0) THEN
            DOCC = DOCC - 1
         ELSE
            CALL CCINI (CILUN, LREC, CIVOL, INSL, CIVER, CATBLK, CIBUF,
     *         IERR)
            IF (IERR.NE.0) THEN
               DOCC = DOCC - 1
               WRITE (MSGTXT,1001) CIVER
               CALL MSGWRT (4)
               END IF
            END IF
         END IF
C                                       Find max and min flux
      SMAX = -1.E20
      SMIN = -SMAX
      DO 100 I = 1,LPK
         IF (CONVRG(I)) THEN
            SMAX = MAX (SMAX, ABS(GLIST(1, I)))
            SMIN = MIN (SMIN, ABS(GLIST(1, I)))
C                                       keep osort out of trouble
         ELSE
            CALL RFILL (6, 0.0, GLIST(1, I))
            END IF
 100     CONTINUE
C                                       scale minimum so that
C                                       1. < smin < 1000.
C                                       Max rescaling is 10**6
      IF (SMIN.GE.0.001 .AND. SMIN.LT.1.0) THEN
         LEVMIN = 1
      ELSE IF (SMIN.GE.1.E-6 .AND. SMIN.LT.1.E-3) THEN
         LEVMIN = 2
      ELSE
         LEVMIN = 0
         END IF
      SMIN = 10.**(3*LEVMIN)
C                                       Tell user about units/scaling
      CALL H2CHR (8, 1, CATH(KHBUN), UNIT)
      MSGTXT = 'Fluxes expressed in units of ' // PREFIX(LEVMIN) // UNIT
      CALL MSGWRT (MSGL(DOMSG))
C                                       Warn about possible overflow
      MSGTXT = 'NOTE: Fluxes marked by * have been divided by 1000.'
      CALL MSGWRT (MSGL(DOMSG))
      MSGTXT = 'Sky Coordinates are ' // CTYP(1,LOCNUM) // ' and ' //
     *   CTYP(2,LOCNUM) 
      CALL MSGWRT (MSGL(DOMSG))
      MSGTXT = 'File: ' // INNA
      CALL MSGWRT (MSGL(DOMSG))
      IF (TRYCON) THEN
         MSGTXT = '"D" line gives best fit, min(-),& max(+) 1 sigma'
         CALL MSGWRT (MSGL(DOMSG))
         MSGTXT = 'bounds on BMAX (arcsec), BMIN(arcsec) and BPA(deg)'
         CALL MSGWRT (MSGL(DOMSG))
         END IF
      CALL XYVAL (RPLOC(1,LOCNUM), RPLOC(2,LOCNUM), X(1), X(2), X(3),
     *   IERR)
C                                       RA and DEC
      IF (AXTYP(LOCNUM).EQ.1) THEN
         DO 50 J = 1,2
            I2TMP = J - 1
            CALL AXSTRN (CTYP(J,LOCNUM), X(J), I2TMP, ILEN(J), RSTR(J))
 50         CONTINUE
         WRITE (MSGTXT,1010) RSTR(1)(3:15), RSTR(2)(4:15)
      ELSE
         WRITE (MSGTXT, 1011) X(1), X(2)
         END IF
      CALL MSGWRT (MSGL(DOMSG))
      MSGTXT = ' '
      CALL MSGWRT (MSGL(DOMSG))
C                                       Create header
      MSGTXT = '   #    Flux '
      INEXT = 14
      IF ((.NOT.DOPNT) .AND. (.NOT.NOCLN)) THEN
         MSGTXT(INEXT:) = 'Int Flux'
         INEXT = INEXT + 8
         END IF
      IF (AXTYP(LOCNUM).EQ.1) THEN
         MSGTXT(INEXT:) = '     ' // CTYP(1,LOCNUM) // '     ' //
     *      CTYP(2,LOCNUM)
         INEXT = INEXT + 26
      ELSE
         MSGTXT(INEXT:) = '  X(deg)  Y(deg)'
         INEXT = INEXT + 16
         END IF
      IF (.NOT.DOPNT) THEN
         MSGTXT(INEXT:) = ' Bmaj Bmin   PA'
         END IF
      CALL MSGWRT (MSGL(DOMSG))
C                                       Sort output
      IF (SORT.EQ.'Y') THEN
         KEY1 = 3
      ELSE IF (SORT.EQ.'S') THEN
         KEY1 = 1
      ELSE
         KEY1 = 2
         END IF
      KEY2 = KEY1
C
      CALL OSORTI (GLIST, LPK, LPK+2, KEY1, KEY2, 6, WK, IP, IER)
      IPK = 0
C                                       Loop over sources
      DO 800 IUS = 1,LPK
C                                       SORT; S descending, X,Y ascending
         IF (((KEY1.EQ.2) .AND. (AXINC(1,LOCNUM).LT.0.)) .OR.
     *      ((KEY1.EQ.3) .AND. (AXINC(2,LOCNUM).LT.0.)) .OR.
     *      (KEY1.EQ.1)) THEN
            I = IP(LPK + 2 -IUS)
         ELSE
            I = IP(IUS+1)
            END IF
C                                       Skip if flagged
         IF (.NOT.CONVRG(I)) GO TO 800
         IPK = IPK + 1
         RECORD(1) = GLIST(1,I)
         ELIST(1,I) = ABS(ELIST(1,I))
         SCALE      = SMIN
         IMARK = 1
C                                       Rescale if overflow
         DO 10 K = 1,2
            IF (ABS(SCALE * RECORD(1)).GE.1000) THEN
               SCALE = .001 * SCALE
               IMARK = IMARK + 1
               END IF
 10         CONTINUE
C                                        FIx dIameters
         GLIST(6,I) = GLIST(6,I)/CON + PASUM
         ELIST(6,I) = ELIST(6,I)/CON
         GLIST(4,I) = ABS(GLIST(4,I))
         GLIST(5,I) = ABS(GLIST(5,I))
         IF (GLIST(4,I).LT.GLIST(5,I)) THEN
            TEMP = GLIST(5,I)
            GLIST(5,I) = GLIST(4,I)
            GLIST(4,I) = TEMP
            GLIST(6,I) = GLIST(6,I) - 90.0
            END IF
         IF (GLIST(6,I).GT.180.0) THEN
            GLIST(6,I) = GLIST(6,I) - 180.0
         ELSE IF (GLIST(6,I).LT.0.0) THEN
            GLIST(6,I) = GLIST(6,I) + 180.0
            END IF
C                       If you can calculate total flux
         IF (.NOT.DOPNT .AND. .NOT.NOCLN) THEN
            RECORD(1) =
     &          GLIST(1,I)*GLIST(4,I)*GLIST(5,I)/CB(1)/CB(2)
            ITOT = RECORD(1) * SCALE
            ITOTE =ELIST(5,I)/GLIST(5,I) +ELIST(4,I)/GLIST(4,I)
     &          + ELIST(1,I)/GLIST(1,I)
            ITOTE = ITOTE * ITOT
         ELSE
            ITOT  = 0.
            ITOTE = 0.
            END IF
C
         RECORD(2) = (GLIST(2,I) - RPLOC(1,LOCNUM)) * AXINC(1,LOCNUM)
         RECORD(3) = (GLIST(3,I) - RPLOC(2,LOCNUM)) * AXINC(2,LOCNUM)
C         RADIUS = SQRT (RECORD(2)**2 + RECORD(3)**2)
C                               If you can calculate extensIons
         IF (.NOT.DOPNT) THEN
            SINC = SIN (CON*GLIST(6,I))
            COSC = COS (CON*GLIST(6,I))
            RECORD(4) = GLIST(4,I) * SQRT ((AXINC(1,LOCNUM)*SINC)**2 +
     *         (AXINC(2,LOCNUM)*COSC)**2)
            RECORD(5) = GLIST(5,I) * SQRT ((AXINC(2,LOCNUM)*SINC)**2 +
     *         (AXINC(1,LOCNUM)*COSC)**2)
            RECORD(7) = ABS(AXINC(2,LOCNUM)) * COSC
            IF (RECORD(7).EQ.0.) RECORD(6) = GLIST(6,I) + ROT(LOCNUM)
            IF (RECORD(7).NE.0.) RECORD(6) = ROT(LOCNUM)  + 
     *         ATAN (ABS(AXINC(1,LOCNUM))*SINC / RECORD(7)) / CON
            RECORD(7) = 2.0
            END IF
         CALL RCOPY (7, RECORD, RICORD)
C                               Convert pixel pos to sky pos
         CALL XYVAL (GLIST(2,I), GLIST(3,I), X(1), X(2), X(3), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR, I
            CALL MSGWRT (7)
            CAXINC(1) = AXINC(1,LOCNUM)
            CAXINC(2) = AXINC(2,LOCNUM)
C                                        Get the local pixel
C                                        increments:
         ELSE
            CALL XYVAL (GLIST(2,I)+1.0, GLIST(3,I), XX, YY, ZZ, IERR)
            XXT = (XX - X(1))*COS(CON*X(2))
            YYT = YY - X(2)
            CAXINC(1) = XXT * COS (CON*ROT(LOCNUM)) +
     *         YYT * SIN(CON*ROT(LOCNUM))
            CALL XYVAL (GLIST(2,I), GLIST(3,I)+1.0, XX, YY, ZZ, IERR)
            XXT = (XX - X(1))*COS(CON*X(2))
            YYT = YY - X(2)
            CAXINC(2) = YYT * COS (CON*ROT(LOCNUM)) - 
     *         XXT * SIN(CON*ROT(LOCNUM))
            END IF
C                                        RA and DEC
         IF (AXTYP(LOCNUM).EQ.1) THEN
            DO 60 J = 1,2
               I2TMP = J - 1
               CALL AXSTRN (CTYP(J,LOCNUM), X(J), I2TMP, ILEN(J),
     *            RSTR(J))
 60            CONTINUE
            ERR1 = ABS (ELIST(2,I)*CAXINC(1)*480.0 / COS(CON*X(2)))
            ERR2 = ABS (ELIST(3,I)*CAXINC(2)*3600.0)
C                                        Other type
         ELSE
            ERR1 = ABS(ELIST(2,I) * CAXINC(1))
            ERR2 = ABS(ELIST(3,I) * CAXINC(2))
            END IF
C                                        Diameters
         IF (.NOT.DOPNT) THEN
            IF (ELIST(6,I).GT.90.0) ELIST(6,I) = 90.0
            DO 85 J = 4,5
               IF (GLIST(J,I).GE.99.5) GLIST(J,I) = 99.5
               IF (ELIST(J,I).GE.99.5) ELIST(J,I) = 99.5
 85            CONTINUE
C                                        Put in arcsec
            IF (AXTYP(LOCNUM).EQ.1) THEN
               TEMP = 3.6E3*SQRT( (SIN(CON*GLIST(6,I))*CAXINC(1))**2
     *                           +(COS(CON*GLIST(6,I))*CAXINC(2))**2)
               INT = GLIST(4,I) * TEMP
               ERRA = ELIST(4,I) * TEMP
               TEMP = 3.6E3*SQRT( (COS(CON*GLIST(6,I))*CAXINC(1))**2
     *                           +(SIN(CON*GLIST(6,I))*CAXINC(2))**2)
               INTE = GLIST(5,I) * TEMP
               ERRB = ELIST(5,I) * TEMP
               PAC = ATAN2( SIN(CON*GLIST(6,I))*ABS(CAXINC(1)),
     *                      COS(CON*GLIST(6,I))*ABS(CAXINC(2)) )/CON
               TEMP = 3.6E3*ABS(CAXINC(1))
               END IF
            END IF
C
         GLIST(1,I) = GLIST(1,I) * SCALE
         ELIST(1,I) = ELIST(1,I) * SCALE
C                       Start formatting output record
         WRITE (MSGTXT,1050) IPK, MARK(IMARK), GLIST(1,I)
         WRITE (ESGTXT,1051) MARK(IMARK), ELIST(1,I)
         INEXT = 14
         IF (.NOT.DOPNT .AND. .NOT.NOCLN) THEN
            WRITE (MSGTXT(INEXT:),1052) ITOT
            WRITE (ESGTXT(INEXT:),1052) ITOTE
            INEXT = INEXT + 8
            END IF
C
         IF (AXTYP(LOCNUM).EQ.1) THEN
            MSGTXT(INEXT:) = RSTR(1)(3:15)//' '//RSTR(2)(4:15)
            WRITE (ESGTXT(INEXT:),1053) ERR1, ERR2
            INEXT = INEXT + 26
         ELSE
            WRITE (MSGTXT(INEXT:),1052) X(1), X(2)
            WRITE (ESGTXT(INEXT:),1052) ERR1, ERR2
            INEXT = INEXT + 16
            END IF
         IF (.NOT.DOPNT) THEN
            WRITE (MSGTXT(INEXT:),1054) INT, INTE, PAC
            WRITE (ESGTXT(INEXT:),1054) ERRA, ERRB, ELIST(6, I)
            END IF
         CALL MSGWRT (MSGL(DOMSG))
         MSGTXT = ESGTXT
         CALL MSGWRT (MSGL(DOMSG))
C                                        Deconvolve if possible
         IF (TRYCON) THEN
            CALL BMVAL (GLIST(4,I), GLIST(5,I), GLIST(6,I),
     &          ELIST(4,I), ELIST(5,I), ELIST(6,I),
     &          CB(1), CB(2), CB(3), DCONV, IERR)
C                                        Convert to degrees
            IF (TEMP.NE.0.0) THEN
               SINC = DCONV(1,1) * TEMP / 3600.
               COSC = DCONV(2,1) * TEMP / 3600.
               RICORD(4) = SINC
               RICORD(5) = COSC
               RICORD(6) = DCONV(3,1)
C  if 0 <docc < 1.5 replace diameters with deconvolved values
C  and indicate that CC values are deconvolved
C  if > 1.5 use convolved alues
               IF (DOCC.GT.0. .AND. DOCC.LT.1.5) THEN
                  RECORD(4) = RICORD(4)
                  RECORD(5) = RICORD(5)
                  RECORD(6) = RICORD(6)
                  RECORD(7) = 1.0
                  END IF
               DO 105 J = 1,2
                  DO 104 K = 1,3
                     DCONV(J,K) = DCONV(J,K) * TEMP
 104                 CONTINUE
 105              CONTINUE
               END IF
            WRITE (MSGTXT, 1100) 'D', DCONV
            CALL MSGWRT (MSGL(DOMSG))
            END IF
         IF (DOCC.GT.0) THEN
            IRNO = IRNO + 1
            IERR = 0
            CALL TABIO ('WRIT', 0, IRNO, RECORD, CIBUF, IERR)
            IF (IERR.NE.0) DOCC = - 1
            END IF
C                                        End of loop over sources
 800     CONTINUE
C                                        close CC file
      IF (DOCC.GT.0) THEN
         CALL TABIO ('CLOS', 0, IRNO, RICORD, CIBUF, IERR)
         CALL CATDIR ('CSTA', CIVOL, INSL, CDUM, CDUM, DUM, CDUM, DUM,
     *      'CLWR', CIBUF, IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('SADOUT: Cannot initialize intput CC file ver:',I3)
 1010 FORMAT ('Reference Center: ',A,' ',A)
 1011 FORMAT ('Reference Center: ',2F13.5)
 1040 FORMAT ('ERROR',I3,' CONVERTING TO SKY POSITION OF SOURCE',I5)
 1050 FORMAT (I4,A1,F8.3)
 1051 FORMAT (4X,A1,F8.3)
 1052 FORMAT (F8.3)
 1053 FORMAT (F13.3,F13.2)
 1054 FORMAT (2F5.1,F6.1)
 1100 FORMAT (A1,4X,3F6.1,'-',3F6.1,'+',3F6.1)
      END
      SUBROUTINE BMVAL (BMAJ, BMIN, BPA, BMAJE, BMINE, BPAE, CBMAJ,
     *   CBMIN, CBPA, R, IER)
C-----------------------------------------------------------------------
C   Subroutine BMVAL deconvoles the fitted beam from the clean beam and
C   also generates appropriate errors.
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
C      IER        I       Error return 0-> Can completely deconvolve
C                           1-> Cannot deconv some limits
C                           2-> Cannot deconv fitted source
C-----------------------------------------------------------------------
      INTEGER   IER, IERR, I, J, K, IC
      REAL      BMAJ, BMIN, BPA, BMAJE, BMINE, BPAE, B1, B2, B3,
     *   DELT(3), MAJOR, MINOR, PA, TEMP, CBMAJ, CBMIN, CBPA, R(3,3)
      DATA DELT /-0.7,0.0,0.7/
C-----------------------------------------------------------------------
C                                       Deconvolve the fit
      IER = 0
      R(1,2) = 1.0E20
      R(2,2) = 1.0E20
      R(3,2) = 1.0E20
      R(1,3) = -1.0E20
      R(2,3) = -1.0E20
      R(3,3) = -1.0E20
      CALL DECONV (BMAJ, BMIN, BPA, CBMAJ, CBMIN, CBPA, MAJOR, MINOR,
     *   PA, IERR)
C                                       Could not deconvolve
      IF (IERR.NE.0) THEN
         IER = 2
         R(1,1) = 0.
         R(1,2) = 0.
         R(1,3) = MAJOR
         R(2,1) = 0.
         R(2,2) = 0.
         R(2,3) = MAJOR
         R(3,1) = 0.
         R(3,2) = 0.
         R(3,3) = 180.00
C                                      Put in deconvolved size
      ELSE
         R(1,1) = MAJOR
         R(2,1) = MINOR
         R(3,1) = MOD ((PA+720.0), 180.0)
         END IF
C                                      Set up looping
 25   IC = 0
      DO 50 K = 1,3
         B3 = BPA + DELT(K) * BPAE
         DO 50 J = 1,3
            B2 = BMIN + DELT(J) * BMINE
            DO 50 I = 1,3
               B1 = BMAJ + DELT(I) * BMAJE
               IC = IC + 1
               CALL DECONV (B1, B2, B3, CBMAJ, CBMIN, CBPA, MAJOR,
     *            MINOR, PA, IERR)
               IF (IER.GT.0) GO TO 40
               IF ((IER.EQ.0) .AND. (IERR.EQ.0)) GO TO 22
C                                       Hit an impossible deconv
                  R(1,2) = 0.
                  R(2,2) = 0.
                  R(3,2) = 0.
                  R(3,3) = 180.000
                  IER = 1
                  GO TO 40
C                                       Get upper and lower bounds
C                                       But first look at PA
 22            PA = MOD((R(3,1) - PA +720.0), 180.0)
               IF (PA.LT.45) GO TO 24
               IF (PA.GT.135) GO TO 26
C                                       Switch major, minor axes
                  TEMP = MINOR
                  MINOR = MAJOR
                  MAJOR = TEMP
                  PA = PA + R(3,1) -90.0
                  GO TO 30
 24            PA = PA + R(3,1)
               GO TO 30
 26            PA = PA + R(3,1) - 180.0
C                                       Upper and lower bounds
 30            R(1,2) = MIN(R(1,2), MAJOR)
               R(1,3) = MAX(R(1,3), MAJOR)
               R(2,2) = MIN(R(2,2), MINOR)
               R(2,3) = MAX(R(2,3), MINOR)
               R(3,2) = MIN(R(3,2), PA)
               R(3,3) = MAX(R(3,3), PA)
               GO TO 50
C                                       No conv. look for max
 40            R(1,3) = MAX(R(1,3), MAJOR)
               R(2,3) = R(1,3)
 50            CONTINUE
C
      RETURN
      END
C
      SUBROUTINE DVDMIN (FX, XI, ERR, N, EPS, ITMAX, FOPT, GNOPT,
     *    IER, NPR, RPRT)
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
C    RPRT    R         > 0 => use line printer
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
      INTEGER   FX
      EXTERNAL FX
      INTEGER   NF, NG, IT, I, N, J, NPR, ITMAX, IER, L, I1, I2
C                                       (24 = max number of unknowns)
      DOUBLE PRECISION XI(1), ERR(1), DDOT, DMACH, DNRM2, LAMBDA,
     *   MSQ, MU, NSQ, NU, XJ(24,24), X0(24), X(24), K0(24), K(24),
     *   S(24), GG(24), M(24), P(24), Q(24), WUN(24), AX(24), TINYC,
     *   F, GN, F0P, XX, EPS, FP, B0, UTU, UTS, B, GAMMA, F0,
     *   DELTA, A, C, ALF, T1, T2, T3, T4, T5, T6, QTK0, FOPT,
     *   GNOPT
      INTEGER  NACROS, PERR
      REAL     RPRT
      LOGICAL   XPR
      INCLUDE 'SAD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      XPR = RPRT.GT.0.0
      NACROS = 72
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
      WRITE (MSGTXT,1010) IT, F0, GN
      IF (NPR.GE.1) CALL MSGWRT (4)
      IF ((XPR) .AND. (NPR.GE.1)) CALL PRTLIN (LUNP, INDP, DOCRT,
     *   NACROS, TITL1, TITL2, MSGTXT, ILINE, IPAGE, SCRTCH, PERR)
      IF (PERR.NE.0) XPR = .FALSE.
      IF (NPR.LE.1) GO TO 105
         WRITE (MSGTXT,1020)
         CALL MSGWRT (4)
         IF (XPR) CALL PRTLIN (LUNP, INDP, DOCRT, NACROS, TITL1, TITL2,
     *      MSGTXT, ILINE, IPAGE, SCRTCH, PERR)
         IF (PERR.NE.0) XPR = .FALSE.
         I1 = 1
 102     I2 = I1 + 5
            IF (I2.GT.N) I2 = N
            WRITE (MSGTXT,1021) (X0(I), I = I1,I2)
            CALL MSGWRT (4)
            IF (XPR) CALL PRTLIN (LUNP, INDP, DOCRT, NACROS, TITL1,
     *         TITL2, MSGTXT, ILINE, IPAGE, SCRTCH, PERR)
            IF (PERR.NE.0) XPR = .FALSE.
            I1 = I2 + 1
            IF (I1.LE.N) GO TO 102
 105  IF (IT.LT.ITMAX) GO TO 110
         IER = 1
         GO TO 900
 110  DO 120 I = 1,N
         S(I) = -K0(I)
 120     CONTINUE
      F0P = DDOT (N, K0, 1, S, 1)
      LAMBDA = 2.0D0
      IF (4.0D0*F0.GE.-F0P) GO TO 200
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
         XX = UTS / UTU
         DO 460 I = 1,N
            WUN(I) = XX * WUN(I)
 460        CONTINUE
         NSQ = UTS * XX
C
C                                       Step 5:
 500  XX = NU / MSQ
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
         DELTA = SQRT (NU/MU)
         GO TO 700
C                                       Step 6A:
 650  A = B - MU
      C = B + NU
      GAMMA = SQRT ((1.0D0-MU*NU/(MSQ*NSQ))/(A*B))
      DELTA = SQRT (C/A)
      IF (C.LT.A) GAMMA = -GAMMA
C                                       Step 7:
 700  XX = NSQ * GAMMA
      ALF = NU + MU * DELTA + MSQ * XX
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
      IF (NPR.LE.0) GO TO 995
         IF (IER.EQ.0) WRITE (MSGTXT,1030)
         IF (IER.EQ.1) WRITE (MSGTXT,1040)
         CALL MSGWRT (4)
         IF (XPR) CALL PRTLIN (LUNP, INDP, DOCRT, NACROS, TITL1, TITL2,
     *      MSGTXT, ILINE, IPAGE, SCRTCH, PERR)
         IF (PERR.NE.0) XPR = .FALSE.
         WRITE (MSGTXT,1050) NF, NG
         CALL MSGWRT (4)
         IF (XPR) CALL PRTLIN (LUNP, INDP, DOCRT, NACROS, TITL1, TITL2,
     *      MSGTXT, ILINE, IPAGE, SCRTCH, PERR)
         IF (PERR.NE.0) XPR = .FALSE.
         WRITE (MSGTXT,1060)
         CALL MSGWRT (4)
         IF (XPR) CALL PRTLIN (LUNP, INDP, DOCRT, NACROS, TITL1, TITL2,
     *      MSGTXT, ILINE, IPAGE, SCRTCH, PERR)
         IF (PERR.NE.0) XPR = .FALSE.
         I1 = 1
 930     I2 = I1 + 5
            IF (I2.GT.N) I2 = N
            WRITE (MSGTXT,1021) (XI(I), I = I1,I2)
            CALL MSGWRT (4)
            IF (XPR) CALL PRTLIN (LUNP, INDP, DOCRT, NACROS, TITL1,
     *         TITL2, MSGTXT, ILINE, IPAGE, SCRTCH, PERR)
            IF (PERR.NE.0) XPR = .FALSE.
            I1 = I2 + 1
            IF (I1.LE.N) GO TO 930
 995  IF (.NOT.XPR) RPRT = -1.0
C
      RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Iteration #',I4,3X,' F=',1PD16.8,' Gradient=',1PD16.8)
 1020 FORMAT ('Parameters:')
 1021 FORMAT (1PD12.5,5(1PD13.5))
 1030 FORMAT ('***  Convergence achieved.')
 1040 FORMAT ('***  Maximum number of iterations reached.')
 1050 FORMAT (I4,' Function evaluations and ',I4,
     *   ' gradient evaluations.')
 1060 FORMAT ('Solution parameters:')
      END
C
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
      IF( M.EQ.0 ) GO TO 40
      DO 30 I = 1,M
        DTEMP = DTEMP + DX(I)*DY(I)
   30 CONTINUE
      IF( N.LT.5 ) GO TO 60
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
 10   EPS = EPS/2.0D0
      S = 1.0D0 + EPS
      IF (S.GT.1.0D0) GO TO 10
      EPS = 2.0D0*EPS
C
      S = 1.0D0
   20 TINY = S
      S = S/16.0D0
      IF (S*1.0.NE.0.0D0) GO TO 20
      TINY = (TINY/EPS)*100.0
      HUGE = 1.0D0/TINY
C
      IF (JOB.EQ.1) DMACH = EPS
      IF (JOB.EQ.2) DMACH = TINY
      IF (JOB.EQ.3) DMACH = HUGE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DNRM2 (N, DX, INCX)
C-----------------------------------------------------------------------
      INTEGER   N, INCX, NEXT, NN, I, J
      DOUBLE PRECISION   DX(*), CUTLO, CUTHI, HITEST, SUM, XMAX
C-----------------------------------------------------------------------
C     EUCLIDEAN NORM OF THE N-VECTOR STORED IN DX() WITH STORAGE
C     INCREMENT INCX .
C     IF    N.LE.0 RETURN WITH RESULT = 0.
C     IF N.GE.1 THEN INCX MUST BE.GE.1
C
C           C.L.LAWSON, 1978 JAN 08
C
C     FOUR PHASE METHOD     USING TWO BUILT-IN CONSTANTS THAT ARE
C     HOPEFULLY APPLICABLE TO ALL MACHINES.
C         CUTLO = MAXIMUM OF  DSQRT(U/EPS)  OVER ALL KNOWN MACHINES.
C         CUTHI = MINIMUM OF  DSQRT(V)      OVER ALL KNOWN MACHINES.
C     WHERE
C         EPS = SMALLEST NO. SUCH THAT EPS + 1..GT.1.
C         U   = SMALLEST POSITIVE NO.   (UNDERFLOW LIMIT)
C         V   = LARGEST  NO.            (OVERFLOW  LIMIT)
C
C     BRIEF OUTLINE OF ALGORITHM..
C
C     PHASE 1    SCANS ZERO COMPONENTS.
C     MOVE TO PHASE 2 WHEN A COMPONENT IS NONZERO AND.LE.CUTLO
C     MOVE TO PHASE 3 WHEN A COMPONENT IS.GT.CUTLO
C     MOVE TO PHASE 4 WHEN A COMPONENT IS.GE.CUTHI/M
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
      IF (N.GT.0) GO TO 10
         DNRM2  = 0.0D0
         GO TO 300
C
 10   NEXT = 1
      SUM = 0.0D0
      NN = N * INCX
C                                                 BEGIN MAIN LOOP
      I = 1
 20      GO TO (30, 50, 70, 110), NEXT
 30   IF (ABS(DX(I)).GT.CUTLO) GO TO 85
      NEXT = 2
      XMAX = 0.0D0
C
C                        PHASE 1.  SUM IS ZERO
C
 50   IF (DX(I).EQ.0) GO TO 200
      IF (ABS(DX(I)).GT.CUTLO) GO TO 85
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
   70 IF( ABS(DX(I)).GT.CUTLO ) GO TO 75
C
C                     COMMON CODE FOR PHASES 2 AND 4.
C                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
C
  110 IF( ABS(DX(I)).LE.XMAX ) GO TO 115
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
      IF(ABS(DX(J)).GE.HITEST) GO TO 100
   95    SUM = SUM + DX(J)**2
      DNRM2 = SQRT( SUM )
      GO TO 300
C
  200 CONTINUE
      I = I + INCX
      IF ( I.LE.NN ) GO TO 20
C
C              END OF MAIN LOOP.
C
C              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
C
      DNRM2 = XMAX * SQRT(SUM)
 300  CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ISLAND (BLC, TRC, PKWIN, NPK, NPKMAX, IERR)
C-------------------------------------------------------------------
C       Find points in the residual map whose values are greater
C       than CUTT.  All such points which touch each other, either
C       along grid lines or diagnols, are considered to be one
C       "island".  Give each distinct island its own number
C       and compute a rectangular window that just contains
C       that island.
C       INPUTS
C          BLC(7)             R   Search area boundary
C          TRC(7)             R   Search area upper boundary
C          NPKMAX             I   Maximum allowed number of islands
C       COMMON:
C          CUTT               R   Cuttoff level of islands
C       OUTPUTS:
C          PKWIN(4, NPKMAX)   R   A list of minX,minY,maxX,maxY
C                                 window corners for islands
C          NPK                I   Number of islands found
C          IERR               I   I/O errors
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7)
      INTEGER   PKWIN(4,*), NPK, NPKMAX, IERR
C
      INTEGER   OLD(4096), NEW(4096), I, J, LOCWIN(4), IFIL
      INCLUDE 'SAD.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      NPK = 0
C                                       Initialize previous ine
      CALL FILL (NX, 0, OLD)
      CALL MAPWIN (LUN1, BLC, TRC, IERR)
C                                       get real values of win from
C                                       filtab
      CALL FILNUM (LUN1, IFIL, IERR)
      LOCWIN(1) = FILTAB(POBLC,   IFIL)
      LOCWIN(2) = FILTAB(POBLC+1, IFIL)
      LOCWIN(3) = FILTAB(POTRC,   IFIL)
      LOCWIN(4) = FILTAB(POTRC+1, IFIL)
      CALL COPY (5, FILTAB(POBLC+2, IFIL), DEPTH)
C                                       Get standard coord info
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
      NX     = LOCWIN(3) - LOCWIN(1) + 1
      NY     = LOCWIN(4) - LOCWIN(2) + 1
      INSL = FILTAB (POCAT, IFIL)
C
      DO 200 J = LOCWIN(2),LOCWIN(4)
         CALL MAPIO ('READ', LUN1, DATA(LOCWIN(1)), IERR)
         IF (IERR.NE.0) GO TO 900
         DO 100 I = LOCWIN(1),LOCWIN(3)
C                               Is point above cutoff?
            IF ((DATA(I).LT.CUTT) .OR. (DATA(I).EQ.FBLANK)) THEN
               NEW(I) = 0
C                               Are any adjacent points, on currnt
C                               Line or previous line already marked?
            ELSE IF ((I.GT.LOCWIN(1)) .AND. (NEW(I-1).GT.0)) THEN
               NEW(I) = NEW(I-1)
            ELSE IF ((I.GT.LOCWIN(1)) .AND. (OLD(I-1).NE.0)) THEN
               NEW(I) = OLD(I-1)
C                               Is this point a new link between
C                               two previous distinct islands?
               IF ((I.LT.LOCWIN(3)) .AND. (OLD(I+1).GT.0) .AND.
     *            (OLD(I+1).NE.OLD(I-1))) THEN
                  CALL MERGPK (PKWIN, NEW(I), OLD(I+1))
                  END IF
            ELSE IF (OLD(I).NE.0) THEN
               NEW(I) = OLD(I)
            ELSE IF ((I.LT.LOCWIN(3)) .AND. (OLD(I+1).NE.0)) THEN
               NEW(I) = OLD(I+1)
            ELSE
C                               Totally new island
               IF (NPK.LT.NPKMAX) THEN
                  NEW(I) = NPK + 1
                  NPK  = NEW(I)
                  PKWIN(1, NPK) = I
                  PKWIN(2, NPK) = J
                  PKWIN(3, NPK) = I
                  PKWIN(4, NPK) = J
                  GO TO 100
               ELSE
                  NEW(I) = 0
                  END IF
               END IF
C                               New addition to old island
            IF (NEW(I).NE.0) CALL ADDPK (PKWIN, I, J, NEW(I))
 100        CONTINUE
C                               Get ready for next line.
         CALL COPY (NX, NEW(LOCWIN(1)), OLD(LOCWIN(1)))
 200     CONTINUE
C
 900  IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1100) IERR
         CALL MSGWRT (4)
         END IF
C
 999  RETURN
C----------------------------------------------------------------------
 1100 FORMAT ('ISLAND: ERROR ',I3)
      END
      SUBROUTINE ADDPK (PKWIN, I, J, IPK)
C-----------------------------------------------------------------------
C   Add a single point with coordinates I,J to an existing island by
C   adjusting the boundaries of the Island
C-----------------------------------------------------------------------
      INTEGER PKWIN(4, *), I, J, IPK
C-----------------------------------------------------------------------
      PKWIN(1, IPK) = MIN (PKWIN(1, IPK), I)
      PKWIN(3, IPK) = MAX (PKWIN(3, IPK), I)
      PKWIN(4, IPK) = J
C
 999  RETURN
      END
      SUBROUTINE MERGPK (PKWIN, IPK, JPK)
C-----------------------------------------------------------------------
C      Merge two Islands together by finding max boundaries
C-----------------------------------------------------------------------
      INTEGER   IPK, JPK
      REAL      PKWIN(4, *)
C
      INTEGER   I, J
C-----------------------------------------------------------------------
      I = MIN (IPK, JPK)
      J = MAX (IPK, JPK)
C
      PKWIN(1,I) = MIN(PKWIN(1,IPK), PKWIN(1,JPK))
      PKWIN(2,I) = MIN(PKWIN(2,IPK), PKWIN(2,JPK))
      PKWIN(3,I) = MAX(PKWIN(3,IPK), PKWIN(3,JPK))
      PKWIN(4,I) = MAX(PKWIN(4,IPK), PKWIN(4, JPK))
      PKWIN(1,J) = 0
C
 999  RETURN
      END
      SUBROUTINE GETRMS (BLC, TRC, RMS, IERR)
C-----------------------------------------------------------------------
C   Find RMS in windowed area of input map
C   Inputs:
C      BLC(7)  R       Lower area boundries
C      TRC(7)  R       Upper area boundries
C   Outputs:
C      RMS     R       root man square in window
C      IERR    R       I/O errors
C-----------------------------------------------------------------------
      INTEGER   IERR
      REAL      BLC(7), TRC(7), RMS
C
      INTEGER   IFIL, NPT, I, J
      REAL      SUM, SUM2
      INCLUDE 'SAD.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C----------------------------------------------------------------------
      IERR = 2
C                                       Correct mapsize in header for
C                                       windowing
      CALL MAPWIN (LUN1, BLC, TRC, IERR)
C                                       get real values of win from
C                                       filtab
      CALL FILNUM (LUN1, IFIL, IERR)
      WIN(1) = FILTAB(POBLC,   IFIL)
      WIN(2) = FILTAB(POBLC+1, IFIL)
      WIN(3) = FILTAB(POTRC,   IFIL)
      WIN(4) = FILTAB(POTRC+1, IFIL)
      CALL COPY (5, FILTAB(POBLC+2, IFIL), DEPTH)
      NX     = WIN(3) - WIN(1) + 1
      NY     = WIN(4) - WIN(2) + 1
      INSL = FILTAB (POCAT, IFIL)
      SUM   = 0.
      SUM2  = 0.
      NPT   = 0
      DO 90 I = 1,NY
         CALL MAPIO ('READ', LUN1, DATA, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1070) IERR
            GO TO 900
            END IF
         DO 80 J = 1,NX
             IF (DATA(J).NE.FBLANK) THEN
                SUM   = SUM + DATA(J)
                SUM2  = SUM2 + DATA(J)**2
                NPT   = NPT + 1
                END IF
 80         CONTINUE
 90      CONTINUE
C                               compute input rms
      IERR = 0
      IF (NPT.GT.0) THEN
         SUM  = SUM/NPT
         SUM2 = SUM2/NPT
         RMS  = SQRT(SUM2 - SUM**2)
         END IF
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C----------------------------------------------------------------------
 1070 FORMAT ('GETRMS: READ ERROR ',I6)
      END
      SUBROUTINE MAKO (RPARM, BLC, TRC, IERR)
C----------------------------------------------------------------------
C   Make the output file
C  INPUTS:
C       RPARM(37)       R       Input parm list, contains names
C                               Classes etc. of output file
C       BLC(7)          R       Boundaries of output file
C       TRC(7)          R       More boundaries
C       IERR            I       I/O errors
C----------------------------------------------------------------------
      INTEGER   IERR
      REAL      RPARM(*), BLC(7), TRC(7)
C
      INTEGER   IFIL, I
      HOLLERITH HOLMA
      INCLUDE 'SAD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      CALL MAPWIN (LUN1, BLC, TRC, IERR)
C                                       get real values of win from
C                                       filtab
      CALL FILNUM (LUN1, IFIL, IERR)
      WIN(1) = FILTAB(POBLC,   IFIL)
      WIN(2) = FILTAB(POBLC+1, IFIL)
      WIN(3) = FILTAB(POTRC,   IFIL)
      WIN(4) = FILTAB(POTRC+1, IFIL)
      NX = WIN(3) - WIN(1) + 1
      NY = WIN(4) - WIN(2) + 1
C                                   Create Output name and file
      CALL COPY (256, CATBLK, OUTBLK)
      CALL CHR2H (4, 'MA  ', 1, HOLMA)
      CALL H2WAWA (RPARM(23), RPARM(26), RPARM(28), HOLMA, RPARM(29),
     *   RPARM(1), OUTNA)
      IF (OUTNA(1:1).EQ.' ') OUTNA(1:12) = INNA(1:12)
      IF (OUTNA(13:13).EQ.' ') OUTNA(13:18) = 'RESID '
C
C                                       modify output file header
      OUTBLK(KINAX)   = NX
      OUTBLK(KINAX+1) = NY
      CALL FILL (5, 1, OUTBLK(KINAX+2))
      OUTR(KRCRP)   = CATR(KRCRP)   - WIN(1) + 1.0
      OUTR(KRCRP+1) = CATR(KRCRP+1) - WIN(2) + 1.0
      DO 20 I = 2,6
         OUTR(KRCRP+I) = CATR(KRCRP+I) - DEPTH(I+1) + 1.0
 20      CONTINUE
      DO 30 I = 1, KIEXTN
         CALL CHR2H (2, '  ', 1, OUTH(KHEXT+I-1))
         OUTBLK(KIVER+I-1) = 0
 30      CONTINUE
C                                       Create output map
      CALL MAPCR (INNA, OUTNA, OUTBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1000) IERR
         GO TO 900
         END IF
C                                       Open it
      CALL OPENCF (LUN2, OUTNA, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 900
         END IF
      CALL FILNUM (LUN2, IFIL, IERR)
      OUTSL = FILTAB (POCAT, IFIL)
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAKO: Error creating output file ', I6)
 1010 FORMAT ('MAKO: Error opening output file ', I6)
      END
      SUBROUTINE BUBS (NS, ILIST, VAL)
C-----------------------------------------------------------------------
C   Simple minded bubble sort of ILIST so that VAL(ILIST(I)) are
C   monotonically increasing.  This works very efficiently because
C   VAL is almost correctly sorted to begin with
C   Inputs:
C      NS        I       Number of items in ILIST
C      ILIST     I(*)    Array of indices that have to be sorted
C      VAL       R(*)    Array of values corresponding to ILIST
C   Outputs:
C      ILIST     I(*)    When done, VAL(ILIST(I)) should be increasing
C-----------------------------------------------------------------------
      INTEGER   NS, ILIST(*)
      REAL      VAL(*)
C
      INTEGER   I1, I2, IT
C-----------------------------------------------------------------------
      DO 200 I1 = 2,NS
         IF (VAL(ILIST(I1)).LT.VAL(ILIST(I1-1))) THEN
            IT = ILIST(I1)
            ILIST(I1) = ILIST(I1-1)
            ILIST(I1-1) = IT
            DO 100 I2 = I1-1,1,-1
               IF (VAL(ILIST(I2-1)).LT.VAL(ILIST(I2))) GO TO 200
               IT = ILIST(I2)
               ILIST(I2) = ILIST(I2-1)
               ILIST(I2-1) = IT
 100           CONTINUE
            END IF
 200     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SUBMOD (NGOOD, IERR)
C-----------------------------------------------------------------------
C   Subtract all the found gaussians from the input map and write them
C   into the output map.  We go through the relevant part of the input
C   map line by line, and subtract all components with a reasonable
C   contribution from this line before writing it out.  The arrays YLOW,
C   and YHI contain the limits of the lines to which each component
C   contributes.  We assume YLOW is sorted in ascending order, so that,
C   for a given line, once we reach a component for which YLOW is larger
C   than the current line number, we can skip the rest of the
C   components.  While you're at it compute the min/max of output map
C   and write into header.
C   Inputs:
C      NGOOD   I       Number of components
C   Commons:
C      ISORT(NGOOD) I  A list of indicies so that YLOW(ISORT(I))
C                      is strictly increasing
C      YLOW(NGOOD)     List of lower boundaries for subtracting
C      YHI(NGOOD)      List of upper boundaries
C      GLIST           List of gaussian parameters
C----------------------------------------------------------------------
      INTEGER   NGOOD, IERR
C
      DOUBLE PRECISION VALVAR(6), DUMMY
      INTEGER   I, IY, ILOW, IBOX, LOCWIN(4)
      REAL      RRMIN, RRMAX
      INCLUDE 'SAD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Set up copy loop
      NGAUSS = 1
      NVAR   = 6
      RRMAX = -1.E20
      RRMIN = -RRMAX
C
      ILOW = 1
      DO 10 I = 1,4
         LOCWIN(I) = WIN(I)
 10      CONTINUE
C                                       Start copying
      DO 200 IY = LOCWIN(2),LOCWIN(4)
         CALL MAPIO ('READ', LUN1, DATA, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1000) IERR
            GO TO 900
            END IF
C                                       Do any remaining boxes cover
C                                       this line? If not just copy
         IF ((IY.LT.YLOW(ISORT(ILOW))) .OR. (ILOW.GT.NGOOD)) GO TO 180
C                                       change WIN to fool FXDVD
         WIN(2) = IY
         NY = 1
C                                       Loop over possible boxes
         DO 100 IBOX = ILOW,NGOOD
C                                       Does this box cover line? Since
C                                       boxes are sorted by YLOW, once
C                                       we hit a box thats too high we
C                                       can quit on this line
            IF (YLOW(ISORT(IBOX)).GT.IY) GO TO 150
            IF (YHI(ISORT(IBOX)).GE.IY) THEN
C                                        Copy parameters into VALVAR
               DO 90 I = 1,6
                  VALVAR(I) = GLIST(I, ISORT(IBOX))
 90               CONTINUE
C                                       Compute residuals
               CALL FXDVD (VALVAR, DUMMY, DUMMY, 1)
               DO 95 I = 1,NX
                  IF (DATA(I).NE.FBLANK) DATA(I) = - RESID(I)
  95              CONTINUE
               END IF
 100        CONTINUE
C                                       Drop completed boxes off bottom
 150     DO 170 IBOX = ILOW, NGOOD
            IF (IY+1.GT.YHI(ISORT(IBOX))) THEN
               ILOW = IBOX + 1
            ELSE
               GO TO 180
               END IF
 170        CONTINUE
C                                       Compute max,min of residules
 180     DO 190 I = 1,NX
            RRMAX = MAX (RRMAX, DATA(I))
            RRMIN = MIN (RRMIN, DATA(I))
 190        CONTINUE
         CALL MAPIO ('WRIT', LUN2, DATA, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1100) IERR
            GO TO 900
            END IF
 200     CONTINUE
C                               Done, put minmax in header
      OUTR (KRDMX) = RRMAX
      OUTR (KRDMN) = RRMIN
      CALL SAVHDR (LUN2, OUTBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1200) IERR
         GO TO 900
         END IF
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUBMOD: Read error ', I6)
 1100 FORMAT ('SUBMOD: Write error ', I6)
 1200 FORMAT ('SUBMOD: Error ', I6, ' Updating output catalog')
      END
      SUBROUTINE OSORTI (A, N, NP2, KEY1, KEY2, LEN, WK, IP, IER)
C-----------------------------------------------------------------------
C   OSORTI does a quick sort on the keys. Data sorted into ascending
C   order of the keys.
C   does quick sort on array of vectors, then reorders by calling PERMAT
C   Input:
C      A     R(LEN,N)   Array of data to be sorted.
C      N     I         Number of records to be sorted
C      NP2   I         Number of words in work arrays = N+2
C      KEY1  I         Word in record of slower varying key
C      KEY2  I         Word in record of faster varying key
C      LEN   I         Length of record in R words (must be.LE.2048)
C   Output:
C      WK    R(2,NP2)  Work buffer for keys.
C      IP    I(NP2)    Work buffer for permutation vector.
C      IER   I         Error code,   0 => OK
C                                    1 => input error (LEN > 2048)
C   Adapted from ACM alg. #347 - June 1981
C-----------------------------------------------------------------------
      INTEGER   N, LEN, NP2, IP(NP2), KEY1, KEY2, IER
      REAL      A(LEN,N), WK(2,NP2)
C
      INTEGER   TP, TTP, IU(20), IL(20), I, IJ, J, K, KK, L, M
      REAL      T(2), TT(2)
C-----------------------------------------------------------------------
C                                       Check length
      IER = 1
      IF (LEN.GT.2048) GO TO 999
      IER = 0
C                                       Build array of keys to sort
C                                       Minus sign makes order descend.
C                                       Note "blockers" put at ends.
      DO 5 KK = 1,N
         I = KK + 1
         IP(I) = KK
         WK(1,I) = A(KEY1,KK)
         WK(2,I) = A(KEY2,KK)
 5       CONTINUE
      WK(1,1) = -1.0E20
      WK(1,N+2) = 1.E20
      M = 1
      I = 2
      J = N + 1
C                                       Sort subgroup
 10   CONTINUE
         IF (I.GE.J) GO TO 240
C
 20      CONTINUE
            K = I
            IJ = (J+I)/2
            T(1) = WK(1,IJ)
            T(2) = WK(2,IJ)
            TP = IP(IJ)
            IF (WK(1,I)-T(1)) 70,40,50
 40            IF (WK(2,I).LE.T(2)) GO TO 70
 50               DO 60 KK = 1,2
                     WK(KK,IJ) = WK(KK,I)
                     WK(KK,I) = T(KK)
                     T(KK) = WK(KK,IJ)
 60                  CONTINUE
                  IP(IJ) = IP(I)
                  IP(I) = TP
                  TP = IP(IJ)
C
 70         L = J
            IF (WK(1,J)-T(1)) 90,80,160
 80            IF (WK(2,J).GE.T(2)) GO TO 160
 90               DO 100 KK = 1,2
                     WK(KK,IJ) = WK(KK,J)
                     WK(KK,J) = T(KK)
                     T(KK) = WK(KK,IJ)
 100                 CONTINUE
                  IP(IJ) = IP(J)
                  IP(J) = TP
                  TP = IP(IJ)
                  IF (WK(1,I)-T(1)) 160,110,120
 110                 IF (WK(2,I).LE.T(2)) GO TO 160
 120                    DO 130 KK = 1,2
                           WK(KK,IJ) = WK(KK,I)
                           WK(KK,I) = T(KK)
                           T(KK) = WK(KK,IJ)
 130                       CONTINUE
                        IP(IJ) = IP(I)
                        IP(I) = TP
                        TP = IP(IJ)
                        GO TO 160
C
 140                 CONTINUE
                        DO 150 KK = 1,2
                           WK(KK,L) = WK(KK,K)
                           WK(KK,K) = TT(KK)
 150                       CONTINUE
                        IP(L) = IP(K)
                        IP(K) = TTP
C                                       Put substring on stack
 160        L = L - 1
               IF (L.GT.0) GO TO 165
                  IER = 3
                  GO TO 999
 165           IF (WK(1,L)-T(1)) 180,170,160
 170           IF (WK(2,L).GT.T(2)) GO TO 160
 180              TT(1) = WK(1,L)
                  TT(2) = WK(2,L)
                  TTP = IP(L)
 200              K = K + 1
                     IF (WK(1,K)-T(1)) 200,210,220
 210                 IF (WK(2,K).LT.T(2)) GO TO 200
 220                      IF (K.LE.L) GO TO 140
                              IF (L-I.LE.J-K) GO TO 230
                                 IL(M) = I
                                 IU(M) = L
                                 I = K
                                 M = M + 1
                                 GO TO 250
 230                          CONTINUE
                                 IL(M) = K
                                 IU(M) = J
                                 J = L
                                 M = M + 1
                                 GO TO 250
C                                       Next substring from stack
 240        CONTINUE
               M = M - 1
               IF (M.EQ.0) GO TO 900
               I = IL(M)
               J = IU(M)
C                                       Straight insertion sort done
C                                       if <= 10 elements in group
 250        IF (J-I.GE.11) GO TO 20
         IF (I.EQ.2) GO TO 10
      I = I-1
 260  I = I+1
         IF (I.EQ.J) GO TO 240
            T(1) = WK(1,I+1)
            T(2) = WK(2,I+1)
            TP = IP(I+1)
            IF (WK(1,I)-T(1)) 260,280,290
 280           IF (WK(2,I).LE.T(2)) GO TO 260
 290              K = I
C
 300              CONTINUE
                     WK(1,K+1) = WK(1,K)
                     WK(2,K+1) = WK(2,K)
                     IP(K+1) = IP(K)
                     K = K-1
                     IF (K.GT.0) GO TO 310
                        IER = 4
                        GO TO 999
 310                 IF (T(1)-WK(1,K)) 300,320,330
 320                    IF (T(2).LE.WK(2,K)) GO TO 300
 330                       WK(1,K+1) = T(1)
                           WK(2,K+1) = T(2)
                           IP(K+1) = TP
                           GO TO 260
C                                       Permute full matrix finally
 900  CONTINUE
C
 999  RETURN
      END
