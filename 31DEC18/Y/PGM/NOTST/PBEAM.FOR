LOCAL INCLUDE 'PBEAM.INC'
C                                                          Include PBEAM
C                                       Local include for PBEAM
      INCLUDE 'INCS:PSTD.INC'
      INTEGER   MAXPOI, MAXFIT, MAXFI2, NINLI, NPL
      PARAMETER (MAXFIT = 14)
      PARAMETER (MAXFI2 = MAXFIT*MAXFIT)
C                                       number of points in each line
C                                       of the measurments
      PARAMETER (NINLI = 101)
C                                       Number of the lines(plots)
C                                       Number of plots should be equal
C                                       NINLI*NREF; Take NREF_max=NINLI
      PARAMETER (NPL = NINLI*NINLI)
C                                       MAXPOI max number of measurments
      PARAMETER (MAXPOI = NINLI*NPL)
C
      HOLLERITH XNAMEO(3), XIFILE(12), X2IFIL(12), XOFILE(12), XOPTY
      REAL      XSOU, XDISOU, VPARM(30), BPARM(10), DOPLOT, XPRTL,
     *   XDOTV, XGRCHN, XBAD(10)
C                                       Inputs
      COMMON /INPARM/ XNAMEO, XSOU, XDISOU, XIFILE, X2IFIL, XOFILE,
     *   VPARM, BPARM, DOPLOT, XOPTY, XPRTL, XDOTV, XGRCHN, XBAD
C
      INTEGER   SEQOU, DISKOU, CNOUT(3), TVCHN, GRCHN, TVCORN(4), NPARM,
     *   PRTLEV, IANT, NREF, REFANT(150), NANT, NANTS(150), NMEAS,
     *   ISTART(NPL), NPO(NPL), NITER, NFIT, NNFIT
      LOGICAL   DOTV, DOPHAS, DOCOMB, DOSWIT
      DOUBLE PRECISION XMEAS(MAXPOI), YMEAS(MAXPOI), AMPLS(MAXPOI),
     *   FITPAR(MAXFIT), FREQU, DIAM, TOLER, THRES, AMMAX, XYMAX(2),
     *   XYMIN(2), PHCUT, PHMAX
      CHARACTER INFILE*48, IN2FIL*48, OFILE*48, NAMEOU*12, CLASOU*6,
     *   OPTYPE*4, STOKE*2
C                                       Buffers
      INTEGER   BUFFER(1024)
      COMMON /BUFRS/ BUFFER
C                                       general info
      COMMON /OTHPRM/ SEQOU, DISKOU, CNOUT, TVCHN, NPARM, DOTV, PRTLEV,
     *   DOPHAS, DOCOMB, DOSWIT
      COMMON /EL/     GRCHN, TVCORN
      COMMON /CHRCOM/ INFILE, IN2FIL, OFILE, NAMEOU, CLASOU, STOKE,
     *   OPTYPE
C                                       Important constants
C                                       Internal storage
      COMMON /MEAS/ XMEAS, YMEAS, AMPLS, FITPAR, FREQU, DIAM, TOLER,
     *   THRES, AMMAX, XYMAX, XYMIN, PHCUT, PHMAX, NMEAS, ISTART,
     *   NPO, NITER, NFIT, NNFIT, IANT, NREF, REFANT, NANT,
     *   NANTS
C                                                          End PBEAM
LOCAL END
LOCAL INCLUDE 'IMAGE.PBEAM'
      INTEGER   IMSIZE
      PARAMETER (IMSIZE=512)
LOCAL END
      PROGRAM PBEAM
C-----------------------------------------------------------------------
C! Fits an analitic function to the measured values of the beam
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2000-2003, 2005, 2007, 2012, 2015-2018
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
C   PBEAM fits the analytic function to the measured values of the beam
C   Inputs:
C      INNAME.....UV file name (name).       Standard defaults.
C      INCLASS....UV file name (class).      Standard defaults.
C      INSEQ......UV file name (seq. #).     0 => highest.
C      INDISK.....Disk unit #.               0 => any.
C      INFILE.....The name of a file with the measured points of the
C                 beam (first polarization)
C      IN2FIL.....The name of a file with the measured points of the
C                 beam (second polarization)
C      OUTFILE....Output file to record the fitted parameters and data.
C      VPARM..... 1. Minimum normalized power level used.
C                 2. Diameter of antenna, meters.
C                 3. Number of iterations for nonlinear least square.
C                 4. Degree of the fitted polynomial.
C                 5. Exclude points with abs(phase) > VPARM(5) degrees
C                 6. 0 => combine the two input files and fit to Stokes 'I'
C                    1 => Fit the model to both input files separately,
C                     and compute the beam squint parameters.
C                 7. 0 => do not change the input files
C                     1 => switch the first two columns of the input files
C      BPARM......Beam simulation parameters.
C                 1. Coefficient of G1, the first degree term;
C                 2. Coefficient of G2, the second degree term;
C                 3. Coefficient of G3, the third degree term;
C                 4. XSHIFT; (Beam offset in azimuth)
C                 5. YSHIFT; (Beam offset in elevation)
C                 6. Eccentricity;
C                 7. Error of the eccentricity's guess
C                 8. Position angle, degrees;
C                 9. Error of the position angle guess.
C      OPTYPE.....'    ' use actual data, do not fit ellipticity;
C                 'ELLI' use actual data, do fit ellipticity;
C                 'SIMU' use simulated data, do fit ellipticity;
C      PRTLEV.....0 => print only the coefficients and errors of the fit.
C                 1 => print observed and fitted data
C                      X,Y coordinates use
C                 2 => print observed and fitted data
C                      RO, PHI coordinates use
C                      Header is not printed
C      DOTV.......> 0 => TV, else plot file
C      GRCHAN.....Graphics channel 0 => 1.
C      BADDISK....A list of disks on which scratch files are not to
C                 be placed.
C
C programmer: Leonia Kogan, Jan 2001
C   In plot file header:
C      VPARM(18)  is the plot code number: 1-3 contours, 4-5 IRING for
C                    dat, model, 6-8 for difference
C      VPARM(19,20) is YMIN, YMAX for plots types 4-8
C      VPARM(21-26) are the fit values for up to 6
C      VPARM(27,28) are XSHIFT, YSHIFT
C      VPARM(29,30) are eccentricity, position angle
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'PBEAM.INC'
      INTEGER  ITRY, NTRY
      DATA PRGM /'PBEAM '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL PBEAIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
      IF ((IN2FIL(1:1).EQ.' ') .OR. DOCOMB) THEN
         NTRY = 1
      ELSE
         NTRY = 2
         END IF
C                                       do fit(s)
      DO 10 ITRY = 1,NTRY
         IF (ITRY.EQ.2) INFILE = IN2FIL
C                                       prepare data for BEFIT and
C                                       PLTFIT
         CALL PREPD (IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Fit the function to the
C                                       measurments
         CALL BEFIT (ITRY, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       plot the beam contours
         CALL PLTFIT (IRET)
         IRET = MAX (0, IRET)
 10      CONTINUE
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE PBEAIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   PBEAIN gets input parameters for PBEAM.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                1 => Invalid request
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   JERR
C
      INCLUDE 'PBEAM.INC'
      INTEGER   IERR, I, IROUND
      LOGICAL   T
C
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T, BUFFER)
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 96
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEO, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) THEN
            GO TO 999
         ELSE
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQOU = IROUND (XSOU)
      DISKOU = IROUND (XDISOU)
      PRTLEV = IROUND (XPRTL)
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCHN + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
      DO 15 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 15      CONTINUE
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEO, NAMEOU)
      CALL H2CHR (48, 1, XIFILE, INFILE)
      CALL H2CHR (48, 1, X2IFIL, IN2FIL)
      CALL H2CHR (48, 1, XOFILE, OFILE)
C                                       0 fill vparm for extlist
      CALL RFILL (19, 0.0, VPARM(12))
C                                       diameter of the antenna, m
      DIAM = VPARM(2)
      IF (DIAM.LT.0.01) DIAM = 25
      VPARM(2) = DIAM
      IF (VPARM(9).LE.0.0) VPARM(9) = 1.4
C                                       number of iteration at the
C                                       nonlinear least square
      NITER = VPARM(3)
      IF (NITER.EQ.0) NITER = 4
      VPARM(3) = NITER
C                                       Degree of the fit polinom
      NNFIT = VPARM(4)
      IF (NNFIT.EQ.0) NNFIT = 3
      VPARM(4) = NNFIT
      IF (NNFIT.GT.(MAXFIT-5)) THEN
         WRITE (MSGTXT,1050) NNFIT, MAXFIT-5
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       total numbe of the polinom's
C                                       coefficients
      NNFIT = NNFIT + 1
C                                       threshold
      THRES = VPARM(1)
      IF (THRES.LT.0.0001) THRES = 0.05
      VPARM(1) = THRES
C                                       consider phase to select amps?
      DOPHAS = VPARM(5).LT.-0.1
C                                       switch the first two columns of
C                                       the input files?
      DOSWIT = VPARM(7).GT.0.1
C                                       default for the phase CUTOFF
      PHCUT = VPARM(5)
      IF ((PHCUT.GT.-0.01) .AND. (PHCUT.LT.0.01)) PHCUT = 60
      VPARM(5) = PHCUT
C                                       combine the data of the two
C                                       input files?
      DOCOMB = VPARM(6).LT.0.01
C                                       must have a file to attach
C                                       the plot files to
      IF (NAMEOU.EQ.' ') THEN
         IF (.NOT. DOTV) THEN
            MSGTXT = '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            CALL MSGWRT (6)
            MSGTXT = 'OUTNAME='' '', DOTV.LE.0=> I switch to DOTV>0'
            CALL MSGWRT (6)
            MSGTXT = '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            CALL MSGWRT (6)
            END IF
         DOTV = .TRUE.
         END IF
      JERR = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PBEAIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1050 FORMAT ('You selected the polinom degree =',I2,' > max =',I2)
      END
      SUBROUTINE PREPD (IRET)
C-----------------------------------------------------------------------
C   PREPD prepares the data for BEFIT and PLTFIT.
C   Output:  IRET    I         Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IRET
      INCLUDE 'PBEAM.INC'
C
      INTEGER   IMEAS, KMEAS, I, ITHRES, NPOI, IAMPL(10),
     *   K, NMEAS1, NMEAS2
      DOUBLE PRECISION DIFFX, DIFFY, RRE, IMA, XMEAS1(MAXPOI),
     *   YMEAS1(MAXPOI), RREAL1(MAXPOI), IIMAG1(MAXPOI), XMEAS2(MAXPOI),
     *   YMEAS2(MAXPOI), RREAL2(MAXPOI), IIMAG2(MAXPOI), TOLER0, G0, G1,
     *   G2, G3, X, Y, X0, Y0, EX, PHI, X1, Y1, AR
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      ITHRES = 1
      AMMAX = 1.0
      PHMAX = 0.0
C---------------------------begin the data reading------------------
C                                       read the input file twice
C                                       1. to find the maximum of the
C                                          data
C                                       2. to devide the data by the
C                                          found maximum
 20   CALL READF (INFILE, NMEAS1, XMEAS1, YMEAS1, RREAL1, IIMAG1,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       read only one file if not
C                                       combine the two input files
      IF ((IN2FIL.EQ.' ') .OR. (.NOT.DOCOMB)) THEN
         NMEAS = NMEAS1
         IF (ITHRES.EQ.1) AMMAX = 0.0D0
         DO 30 IMEAS = 1,NMEAS
            XMEAS(IMEAS) = XMEAS1(IMEAS)
            YMEAS(IMEAS) = YMEAS1(IMEAS)
            RRE = RREAL1(IMEAS)
            IMA = IIMAG1(IMEAS)
            AMPLS(IMEAS) = RRE*RRE + IMA*IMA
            IF (VPARM(11).GT.0.0) AMPLS(IMEAS) = SQRT (AMPLS(IMEAS))
            IF ((ITHRES.EQ.1) .AND. (AMMAX.LT.AMPLS(IMEAS))) THEN
               AMMAX = AMPLS(IMEAS)
               PHMAX = ATAN2 (IMA, RRE)
               END IF
 30         CONTINUE
      ELSE
         TOLER0 = 1.0D-5
         CALL READF (IN2FIL, NMEAS2, XMEAS2, YMEAS2, RREAL2, IIMAG2,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         STOKE = 'I '
         NMEAS = 0
         IF (ITHRES.EQ.1) AMMAX = 0.0D0
         DO 60 IMEAS = 1,NMEAS1
            DO 40 KMEAS = 1,NMEAS2
               DIFFX = ABS (XMEAS2(KMEAS) - XMEAS1(IMEAS))
               DIFFY = ABS (YMEAS2(KMEAS) - YMEAS1(IMEAS))
               IF ((DIFFX.LT.TOLER0) .AND. (DIFFY.LT.TOLER0)) THEN
                  NMEAS = NMEAS + 1
                  XMEAS(NMEAS) = XMEAS1(IMEAS)
                  YMEAS(NMEAS) = YMEAS1(IMEAS)
                  RRE = (RREAL1(IMEAS) + RREAL2(KMEAS)) / 2
                  IMA = (IIMAG1(IMEAS) + IIMAG2(KMEAS)) / 2
                  AMPLS(NMEAS) = RRE*RRE + IMA*IMA
                  IF (VPARM(11).GT.0.0) AMPLS(IMEAS) =
     *               SQRT (AMPLS(IMEAS))
                  IF ((ITHRES.EQ.1) .AND. (AMMAX.LT.AMPLS(NMEAS))) THEN
                     AMMAX = AMPLS(NMEAS)
                     PHMAX = ATAN2 (IMA, RRE)
                     END IF
                  GO TO 60
                  END IF
 40            CONTINUE
 60         CONTINUE
         END IF
C                                       re-read to do normalization
      ITHRES = ITHRES + 1
      IF (ITHRES.LE.2) GO TO 20
C                                       data now read and normalized
      CALL H2CHR (4, 1, XOPTY, OPTYPE)
C                                       simulate or not simulate?
      IF (OPTYPE.EQ.'SIMU') THEN
         G0 = 1
C                                       G1
         IF (ABS(BPARM(1)).LT.1.0E-6) BPARM(1) = -0.1
C                                       G2
         IF (ABS(BPARM(2)).LT.1.0E-6) BPARM(2) = 0.0034
C                                       G3
         IF (ABS(BPARM(3)).LT.1.0E-6) BPARM(3) = -4.60E-05
C                                       Xshift
         IF (ABS(BPARM(4)).LT.1.0E-6) BPARM(4) = 0.1
C                                       Yshift
         IF (ABS(BPARM(5)).LT.1.0E-6) BPARM(5) = 0.05
C                                       excentricity
         IF (ABS(BPARM(6)).LT.1.0E-6) BPARM(6) = 0.1
C                                       error of excentricity
         IF (ABS(BPARM(7)).LT.1.0E-6) BPARM(7) = 0.0
C                                       position angle
         IF (ABS(BPARM(8)).LT.1.0E-6) BPARM(8) = 60
C                                       error of position angle
         IF (ABS(BPARM(9)).LT.1.0E-6) BPARM(9) = 5
         G1 = BPARM(1)
         G2 = BPARM(2)
         G3 = BPARM(3)
         X0 = BPARM(4)
         Y0 = BPARM(5)
         EX = BPARM(6)
         PHI = BPARM(8)*DG2RAD
         NPOI = 7
         NMEAS = 0
         DO 90 I = 1,NPOI
            DO 80 K = 1,NPOI
               NMEAS = NMEAS + 1
               X = -3.6 + (I-1)*1.2
               Y = -3.6 + (K-1)*1.2
               XMEAS(NMEAS) = X
               YMEAS(NMEAS) = Y
               X1 =  (X-X0)*COS(PHI) + (Y-Y0)*SIN(PHI)
               Y1 = -(X-X0)*SIN(PHI) + (Y-Y0)*COS(PHI)
               AR = (X1*EX)**2 + Y1**2
               AMPLS(NMEAS) = G0 + G1*AR + G2*AR*AR + G3*AR*AR*AR
               IAMPL(K) = 100*AMPLS(NMEAS)
   80          CONTINUE
            WRITE (MSGTXT,1050) (IAMPL(K), K = 1,NPOI)
            CALL MSGWRT (4)
   90       CONTINUE
         TOLER = (YMEAS(2) - YMEAS(1))/2
         END IF
C-------------------------------END the data reading------------------

      XYMAX(1) = -10000.
      XYMAX(2) = -10000.
      XYMIN(1) = 10000.
      XYMIN(2) = 10000.
      DO 130 I = 1,NMEAS
         XYMAX(1) = MAX (XYMAX(1), XMEAS(I))
         XYMIN(1) = MIN (XYMIN(1), XMEAS(I))
         XYMAX(2) = MAX (XYMAX(2), YMEAS(I))
         XYMIN(2) = MIN (XYMIN(2), YMEAS(I))
 130     CONTINUE
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT (10I3)
      END
      SUBROUTINE READF (INPFIL, NNMEAS, XXMEAS, YYMEAS, RREAL, IIMAG,
     *   IRET)
C-----------------------------------------------------------------------
C   Routine to read the input file
C   Input:
C      INPFIL    C(*)  The file name
C   Input in common:
C      THRES     R     The threshold of selected amplitudes
C      AMMAX     R     Maximum of the data. It is used to normalize
C                      the data
C   Output:
C      NNMEAS    I     Number of selected measurments
C      XXMEAS    R(*)  Array of selected Xs in minutes
C      YYMEAS    R(*)  Array of selected Ys in minutes
C      RREAL     R(*)  Array of real part of amplitude
C      IIMAG     R(*)  Array of image part of amplitude
C      IRET      I     Error; 0 => OK
C-----------------------------------------------------------------------
      INCLUDE 'PBEAM.INC'
      CHARACTER INPFIL*48
      INTEGER   NNMEAS, IRET
      DOUBLE PRECISION XXMEAS(*), YYMEAS(*), RREAL(*), IIMAG(*)
C
      CHARACTER SYM*8
      LOGICAL   T, F, FA, FIRST, NEXT, EOF, DOPH
      INTEGER   LUNPR, PFIND, NIDENT, N2, IREF, KANT, KBPLIM, JTRIM,
     *   KBP, IERR, I, J
      DOUBLE PRECISION XT, YT, AMPSQR, RD2MI, PHADEG, TOLER0, XINIT,
     *   KFREQU, YINIT, DIFFX, DIFFY, PHAS, AMPI, AMPR, AMPP, XTP, YTP,
     *   XLAST, YLAST, RRE, IMA, XX
      CHARACTER LINE*80, KSTOKE*2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       read the file
      LUNPR = 10
      FA = F
      CALL ZTXOPN ('READ', LUNPR, PFIND, INPFIL, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         KBP = JTRIM (INPFIL)
         WRITE (MSGTXT,1000) INPFIL(:KBP)
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
      N2 = 1
      NNMEAS = 0
      RD2MI = 180.0D0 / PI * 60.0D0
C
      TOLER0 = 1.0D-06
      FIRST = T
      AMPR = 0
      AMPI = 0
      NIDENT = 0
C
      NREF = 0
      NANT = 0
      EOF = .FALSE.
C                                       read the file data
 10   CALL ZTXIO ('READ', LUNPR, PFIND, LINE, IRET)
      IF (IRET.EQ.2) THEN
         EOF = .TRUE.
         GO TO 20
      ELSE IF (IRET.GT.0) THEN
         GO TO 999
      ELSE
         CALL CHTRIM (LINE, 80, LINE, KBPLIM)
C                                       read array of ref. antennas,
C                                       antenna number, polarization
C                                       and frequency
         IF (LINE(:3).EQ.'#! ') THEN
            CALL CHLTOU (80, LINE)
            IF (LINE(:9).EQ.'#! REFANT') THEN
               KBP = 3
               CALL GETSYM (LINE, KBP, SYM, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL GETNUM (LINE, KBPLIM, KBP, XX)
               IREF = -1
               IF (XX.NE.DBLANK) THEN
                  IREF = XX + 0.01
                  NREF = NREF + 1
                  REFANT(NREF) = IREF
               ELSE
                  IREF = -1
                  END IF
               CALL GETSYM (LINE, KBP, SYM, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL GETNUM (LINE, KBPLIM, KBP, XX)
               IF (XX.NE.DBLANK) THEN
                  KANT = XX + 0.01
                  NANT = NANT + 1
                  NANTS(NANT) = KANT
               ELSE
                  KANT = -1
                  END IF
               CALL GETSYM (LINE, KBP, SYM, IERR)
               IF (IERR.NE.0) GO TO 999
               I = INDEX (LINE(KBP:), '''')
               KBP = KBP + I
               KSTOKE = LINE(KBP:KBP+1)
               I = INDEX (LINE(KBP:), '''')
               KBP = KBP + I
               CALL GETSYM (LINE, KBP, SYM, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL GETNUM (LINE, KBPLIM, KBP, XX)
               KFREQU = 1.0D0
               IF (XX.NE.DBLANK) KFREQU = XX
               IANT = KANT
               STOKE = KSTOKE
               FREQU = KFREQU
            ELSE IF (LINE(:20).EQ.'#! AVERAGED REF-ANTS') THEN
               KBP = 23
 15            CALL GETNUM (LINE, KBPLIM, KBP, XX)
               IF (XX.NE.DBLANK) THEN
                  KANT = XX + 0.01
                  IF (KANT.GT.0) THEN
                     NREF = NREF + 1
                     REFANT(NREF) = KANT
                     GO TO 15
                     END IF
                  END IF
            ELSE IF (LINE(:20).EQ.'#! AVERAGED ANTENNAS') THEN
               KBP = 23
 16            CALL GETNUM (LINE, KBPLIM, KBP, XX)
               IF (XX.NE.DBLANK) THEN
                  KANT = XX + 0.01
                  IF (KANT.GT.0) THEN
                     NANT = NANT + 1
                     NANTS(NANT) = KANT
                     GO TO 16
                     END IF
                  END IF
               END IF
            END IF
C                                       loop if header card or blank
         IF ((KBPLIM.LE.0) .OR. (LINE(:1).EQ.'#')) GO TO 10
C                                       parse the data card
         KBP = 1
         CALL GETNUM (LINE, KBPLIM, KBP, XX)
         IF (XX.EQ.DBLANK) GO TO 900
         IF (DOSWIT) THEN
            YTP = XX
         ELSE
            XTP = XX
            END IF
         CALL GETNUM (LINE, KBPLIM, KBP, XX)
         IF (XX.EQ.DBLANK) GO TO 900
         IF (DOSWIT) THEN
            XTP = XX
         ELSE
            YTP = XX
            END IF
         CALL GETNUM (LINE, KBPLIM, KBP, XX)
         IF (XX.EQ.DBLANK) GO TO 900
         AMPP = XX
         CALL GETNUM (LINE, KBPLIM, KBP, XX)
         IF (XX.EQ.DBLANK) GO TO 900
         PHAS = XX * DG2RAD
C                                       normalize the data
         AMPP = AMPP / SQRT(AMMAX)
         IF (VPARM(8).GT.0.0) PHAS = PHAS - PHMAX
C                                       really ASIN
         XTP = ASIN (XTP)
         YTP = ASIN (YTP)
         IF (FIRST) THEN
            XINIT = XTP
            YINIT = YTP
            FIRST = .FALSE.
            END IF
C                                       accumulate the identical points
         DIFFX = ABS(XTP - XINIT)
         DIFFY = ABS(YTP - YINIT)
         END IF
C                                       EOF comes here also
 20   NEXT = (DIFFX.GT.TOLER0) .OR. (DIFFY.GT.TOLER0) .OR. (EOF)
      IF (NEXT) THEN
         XINIT = XTP
         YINIT = YTP
         RRE = AMPR / NIDENT
         IMA = AMPI / NIDENT
         AMPSQR = (RRE*RRE + IMA*IMA)
         PHADEG = ABS(ATAN2(IMA,RRE)*RAD2DG)
         XT = XLAST
         YT = YLAST
C
         AMPR = AMPP * COS(PHAS)
         AMPI = AMPP * SIN(PHAS)
         XLAST = XTP
         YLAST = YTP
         NIDENT = 1
      ELSE
         AMPR = AMPR + AMPP * COS(PHAS)
         AMPI = AMPI + AMPP * SIN(PHAS)
         XLAST = XTP
         YLAST = YTP
         NIDENT = NIDENT + 1
         GO TO 10
         END IF
C                                       select the points with big
C                                       amplitude; exclude points with
C                                       big phase(sidelobes)
      DOPH = DOPHAS .OR. PHADEG.LT.PHCUT
      IF ((AMPSQR.GT.THRES) .AND. DOPH) THEN
         NNMEAS = NNMEAS + 1
C                                       take the point exceeded the
C                                       threshold
         RREAL(NNMEAS) = RRE
         IIMAG(NNMEAS) = IMA
         XXMEAS(NNMEAS) = XT * RD2MI
         YYMEAS(NNMEAS) = YT * RD2MI
         END IF
      IF (.NOT.EOF) GO TO 10
C                                       close the infile
      CALL ZTXCLS (LUNPR, PFIND, IRET)
C                                       trim lists
      IF (NANT.GT.1) THEN
         KBP = 1
         DO 50 I = 2,NANT
            DO 40 J = 1,I-1
               IF (NANTS(I).EQ.NANTS(J)) GO TO 50
 40            CONTINUE
            KBP = KBP + 1
            NANTS(KBP) = NANTS(I)
 50         CONTINUE
         NANT = KBP
         END IF
      IF (NREF.GT.1) THEN
         KBP = 1
         DO 70 I = 2,NREF
            DO 60 J = 1,I-1
               IF (REFANT(I).EQ.REFANT(J)) GO TO 70
 60            CONTINUE
            KBP = KBP + 1
            REFANT(KBP) = REFANT(I)
 70         CONTINUE
         NREF = KBP
         END IF
      GO TO 999
C                                       data error
 900  MSGTXT = 'BAD DATA RECORD ='
      CALL MSGWRT (8)
      MSGTXT = LINE
      CALL MSGWRT (8)
      IRET = 9
C
 999  RETURN
C-----------------------------------------------------------------
 1000 FORMAT ('ERROR OPENING FILE ''',A,'''')
      END
      SUBROUTINE BEFIT (ITRY, IRET)
C-----------------------------------------------------------------------
C   BEFIT fits a function determined by OPCODE to the data VALU
C   given as a function of YNEAS
C   Input:
C      ITRY    I     the sequence number of the input file
C   Input in common:
C      XMEAS   R(*)  Array of X coordinates
C      YMEAS   R(*)  Array of Y coordinates
C      AMPLS   R(*)  Array of relevant amplitude (square)
C      NMEAS   I     Number of measurements
C   Output in common
C      FITPAR  R(*)  Array of found parameters of fitting function
C                    FITPAR(1) = G0
C                    FITPAR(2) = G1
C                    FITPAR(3) = G2
C                    FITPAR(4) = G3
C                    FITPAR(5) = X0
C                    FITPAR(6) = Y0
C                    FITPAR(7) = E (excentricy)
C                    FITPAR(8) = PHI(direction of the major axis)
C                    ..............
C      IRET    I     Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INCLUDE 'PBEAM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER  ITRY, IRET
C
      INTEGER   I, K, L, IK, ITER, LUNPR, PFIND, JTRIM, KFIT, J, J1, J2,
     *   IKFIT, IFIT, LESOL, NCH
      REAL      BESSJ1, BT
      DOUBLE PRECISION XSHIFT, YSHIFT, AMPMAX, DVALU, POLIN, POLIN1,
     *   FUNC(MAXPOI), X0, Y0, EX, PHI, X1, Y1, FITPOL, FITBES,
     *   XSH, YSH, ROSH, PHISH, VARX, VARY, RMSX, RMSY, RMSRO, RMSPHI,
     *   RO, DX, DY, PHIXY, FQ2, A, B, G0
      DOUBLE PRECISION  R(MAXFIT), MATR(MAXFI2), NOBS, SUM, SSQ,
     *   SOL(MAXFIT), VX(MAXFIT), SSQRES, VARRES, VARYY, FIT,
     *   ARG(MAXPOI), ROW(MAXFIT), AR, FITPAD
      CHARACTER LINE*80
      LOGICAL   T, F
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C      ----------------------------------------------------------------
C      AMPLS(I; I=1...NMEAS) = G0 + G1*U + G2*U*U + G3*U*U*U + ...
C      U = (X1*E)**2 + Y1**2
C      X1=(X(I)-X0)*COS(PHI) + (Y(I)-Y0)*SIN(PHI);
C      Y1=-(X(I)-X0)*SIN(PHI) + (Y(I)-Y0)*COS(PHI);
C      G0, G1, G2, G3...X0, Y0, E, PHI - unknown parameters
C      ------------ ---------------------------------------------------
C                                       Find the maximum and its
C                                       position in the data
      AMPMAX = 0
      DO 5 I =  1,NMEAS
         IF (AMPLS(I).GT.AMPMAX) THEN
            XSHIFT = XMEAS(I)
            YSHIFT = YMEAS(I)
            AMPMAX = AMPLS(I)
            END IF
 5       CONTINUE
C                                       find initial solution using
C                                       zero shift; XSHIFT; YSHIFT
C                                       and G0 = AMPMAX
      DO 10 I = 1, NMEAS
         ARG(I) = (XMEAS(I)-XSHIFT)**2 + (YMEAS(I)-YSHIFT)**2
         FUNC(I) = AMPLS(I)
 10      CONTINUE
      CALL SOLIN (ARG, FUNC, NNFIT, NMEAS, FITPAR, IRET)
C                                       Initial shift for X and Y
      FITPAR(NNFIT+1) = XSHIFT
      FITPAR(NNFIT+2) = YSHIFT
C                                       Number of fit parameters is
C                                       more now by 2 - the number of
C                                       two shift coordinates;
      NFIT = NNFIT + 2
C                                       Initial excentricity and
C                                       position angle of the ellipce
C
C                                       simulate or not simulate?
      IF (OPTYPE.EQ.'SIMU') THEN
         FITPAR(NNFIT+3) = BPARM(6) + BPARM(7)
         FITPAR(NNFIT+4) = (BPARM(8) + BPARM(9)) * DG2RAD
      ELSE
         FITPAR(NNFIT+3) = 1
         FITPAR(NNFIT+4) = 0
         END IF
C                                       Number of fit parameters is
C                                       more now by 2 - excentricity
C                                       position angle of the ellipce
      IF (OPTYPE.NE.' ') NFIT = NNFIT + 4
C                                       Now FITPAR(1) = G0,
C                                       FITPAR(2)=G1, FITPAR(3)=G2..
C                                       FITPAR(NNFIT+1)=XSHIFT
C                                       FITPAR(NNFIT+2)=YSHIFT
C                                       FITPAR(NNFIT+3)=E
C                                       FITPAR(NNFIT+4)=PHI
C
C
C                                       start iterration to find
C                                       solutions for both G1,G2,G3
C                                       and two shift coordinates
      ITER = 1
 100  IF (OPTYPE.EQ.' ') THEN
         X0 = FITPAR(NNFIT+1)
         Y0 = FITPAR(NNFIT+2)
         EX = 1
         PHI = 0
      ELSE
         X0 = FITPAR(NNFIT+1)
         Y0 = FITPAR(NNFIT+2)
         EX = FITPAR(NNFIT+3)
         PHI = FITPAR(NNFIT+4)
         END IF
C                                       Force result vector R(NFIT),
C                                       matrix M(NFIT*NFIT) to zero
      DO 130 L = 1,NFIT
         R(L) = 0.0
         DO 120 K = 1,NFIT
            IK = K + (L - 1)*NFIT
            MATR(IK) = 0.0
 120        CONTINUE
 130     CONTINUE

      SUM = 0.0
      SSQ = 0.0
      NOBS = 0.0
C                                       Prepare result vector R(NFIT)
C                                       and matrix MATR(NFIT*NFIT)
C                                       for routine DLESQR
      DO 170 I = 1, NMEAS
         X1 = (XMEAS(I)-X0)*COS(PHI) + (YMEAS(I)-Y0)*SIN(PHI)
         Y1 = -(XMEAS(I)-X0)*SIN(PHI) + (YMEAS(I)-Y0)*COS(PHI)
         AR = (X1*EX)**2 + Y1**2
         POLIN = FITPAR(1)
         POLIN1 = 0
         ROW(1) = 1
         DO 140 L = 2, NNFIT
            FITPAD = FITPAR(L)
            POLIN = POLIN + FITPAD*(AR**(L-1))
            POLIN1 = POLIN1 + FITPAD*(L-1)*(AR**(L-2))
C                                       prepare the first (NFIT-2)
C                                       elements of row of matrix
            ROW(L) = AR**(L-1)
  140       CONTINUE
         DVALU = AMPLS(I) - POLIN
C                                       prepare the last 4
C                                       elements of row of matrix
C                                       DF/DX0
         ROW(NNFIT+1) = (2 * X1 * (EX**2) * (-COS(PHI)) +
     *      2 * Y1 * SIN(PHI)) * POLIN1
C                                       DF/DY0
         ROW(NNFIT+2) = (2 * X1 * (EX**2) * (-SIN(PHI)) +
     *      2 * Y1 * (-COS(PHI))) * POLIN1
C                                       DF/DEX
         ROW(NNFIT+3) = 2 * X1 * X1 * EX * POLIN1
C                                       DF/DPHI
         ROW(NNFIT+4) = (2 * X1 * (EX**2) * ((XMEAS(I)-X0) *(-SIN(PHI))
     *      + (YMEAS(I)-Y0) * COS(PHI)) +
     *      2 * Y1 * ((XMEAS(I)-X0) * (-COS(PHI)) +
     *      (YMEAS(I)-Y0) * (-SIN(PHI)))) * POLIN1
C
         NOBS = NOBS + 1
         SUM = SUM + DVALU
         SSQ = SSQ + DVALU*DVALU
C
C                                       Prepare result vector R=A*RAT
C                                       and upper/right triangle of
C                                       matrix M = A_T * A
         DO 160 IFIT = 1,NFIT
            R(IFIT) = R(IFIT) + DVALU*ROW(IFIT)
            DO 150 KFIT = IFIT, NFIT
               IKFIT = IFIT + (KFIT-1)*NFIT
               MATR(IKFIT) = MATR(IKFIT) + ROW(IFIT)*ROW(KFIT)
 150           CONTINUE
 160        CONTINUE
 170     CONTINUE
C
      CALL DLESQR (NFIT, NOBS, SUM, SSQ, R, MATR, SOL, VX, SSQRES,
     *   VARRES, VARYY, FIT, LESOL)
      IRET = LESOL
C                                       find new solutions
      DO 180 IFIT = 1,NFIT
         FITPAR(IFIT) = FITPAR(IFIT) + SOL(IFIT)
 180     CONTINUE
      ITER = ITER + 1
      IF (ITER.LE.NITER) GO TO 100
C
      VARRES = SQRT(VARRES)
C                                       calculate the relative shift of
C                                       the two polarization beams
      IF (ITRY.EQ.1) THEN
         XSH = FITPAR(NNFIT + 1)
         YSH = FITPAR(NNFIT + 2)
         VARX = VX(NNFIT + 1)
         VARY = VX(NNFIT + 2)
      ELSE
C                                       Rick's definition. X is inverted
         XSH = -(FITPAR(NNFIT + 1) - XSH)
         YSH = FITPAR(NNFIT + 2) - YSH
C                                       calculate variation of X and Y
         VARX = VARX + VX(NNFIT + 1)
         VARY = VARY + VX(NNFIT + 2)
         ROSH = SQRT (XSH*XSH + YSH*YSH)
         PHISH = ATAN2(YSH, XSH)
C                                       rms of XSH, YSH
         RMSX = SQRT(VARX)
         RMSY = SQRT(VARY)
C                                       rms of ROSH, PHISH
         RMSRO = SQRT(VARX*(COS(PHISH))**2 + VARY*(SIN(PHISH))**2)
         RMSPHI = SQRT(VARX*(SIN(PHISH))**2 + VARY*(COS(PHISH))**2)
     *      / ROSH
         PHISH = PHISH * RAD2DG
         RMSPHI = RMSPHI * RAD2DG
         END IF
C                                       ambiguity of Pos. angle
C                                       Position angle is determined
C                                       relative to X
      PHI = FITPAR(NNFIT+4) * RAD2DG
      PHI = MOD (PHI, 180.0D0)

      EX = FITPAR(NNFIT+3)
C                                       Recalculate position angle
C                                       relative to Y
      IF (EX.GT.1) THEN
         EX = 1.0 / EX
      ELSE
         PHI = PHI - 90.0D0
         END IF
      IF (PHI.GT.90.0) THEN
         PHI = PHI - 180.0D0
      ELSE
         IF (PHI.LT.-90.0) PHI = PHI + 180.0D0
         END IF
      IF (OPTYPE.NE.' ') THEN
         VPARM(29) = EX
         VPARM(30) = PHI
         END IF
C                                       open the output file
      G0 = FITPAR(1)
      IF (VPARM(10).GT.0.0) G0 = 1.0D0
      DO 181 I = 1,NNFIT
         FITPAR(I) = FITPAR(I) / G0
         IF (I.LE.6) VPARM(20+I) = FITPAR(I)
         VX(I) = VX(I) / G0 / G0
 181     CONTINUE
      VPARM(27) = -FITPAR(NNFIT+1)
      VPARM(28) = FITPAR(NNFIT+2)
      IF (OFILE(1:1).NE.' ') THEN
         LUNPR = 10
         CALL ZTXOPN ('WRIT', LUNPR, PFIND, OFILE, .TRUE., IRET)
         IF (IRET.NE.0) GO TO 199
C                                       Do not print the header if
C                                       PRTLEV=2
         IF (PRTLEV.LE.1.5) THEN
C                                       record the parameters of the
C                                       fitted function in the file
            WRITE (LINE,1010) (REFANT(I), I = 1,NREF)
            NCH = JTRIM (LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            IF (IRET.NE.0) GO TO 199
            IF (NANT.EQ.1) THEN
               WRITE (LINE,1020) NANTS(1), FREQU, STOKE
            ELSE
               J1 = 1
 185           J2 = MIN (NANT, J1+9)
               IF (J2.GE.J1) THEN
                  WRITE (LINE,1021) (NANTS(J), J = J1,J2)
                  IF (J1.EQ.1) LINE(:23) = 'Fit averaged antennas ='
                  NCH = JTRIM (LINE)
                  CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
                  IF (IRET.NE.0) GO TO 199
                  J1 = J2 + 1
                  GO TO 185
                  END IF
               WRITE (LINE,1022) NANTS(1), FREQU, STOKE
               END IF
            NCH = JTRIM (LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            IF (IRET.NE.0) GO TO 199
C                                       Write the normalization
            WRITE (LINE,1030) AMMAX, PHMAX*RAD2DG
            NCH = JTRIM (LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            IF (IRET.NE.0) GO TO 199
C                                       Write the number of data points
            WRITE (LINE,1040) NMEAS
            NCH = JTRIM (LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            IF (IRET.NE.0) GO TO 199
            FQ2 = 1.0
            DO 190 I = 1,NNFIT
               A = FITPAR(I)
               B = SQRT(VX(I))
               WRITE (LINE,1050) I-1, A, B, A/FQ2, B/FQ2
               NCH = JTRIM (LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               IF (IRET.NE.0) GO TO 199
               FQ2 = FQ2 * FREQU * FREQU
 190           CONTINUE
C                                       The minus because of Rick's
C                                       convention
            WRITE (LINE,1060) -FITPAR(NNFIT+1), SQRT(VX(NNFIT+1))
            NCH = JTRIM (LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            IF (IRET.NE.0) GO TO 199
            WRITE (LINE,1070) FITPAR(NNFIT+2), SQRT(VX(NNFIT+2))
            NCH = JTRIM (LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            IF (IRET.NE.0) GO TO 199
            IF (OPTYPE.NE.' ') THEN
               WRITE (LINE,1080) EX, SQRT(VX(NNFIT+3))
               NCH = JTRIM (LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               IF (IRET.NE.0) GO TO 199
               WRITE (LINE,1090) PHI, SQRT(VX(NNFIT+4)*RAD2DG)
               NCH = JTRIM (LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               IF (IRET.NE.0) GO TO 199
               END IF
            WRITE (LINE,1100) VARRES
            NCH = JTRIM (LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            IF (IRET.NE.0) GO TO 199
C                                       relative shift of the two beam
C                                       polarization
            IF (ITRY.EQ.2) THEN
               WRITE (LINE,1140) XSH, RMSX, YSH, RMSY
               NCH = JTRIM (LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               IF (IRET.NE.0) GO TO 199
               WRITE (LINE,1150) ROSH, RMSRO, PHISH, RMSPHI
               NCH = JTRIM (LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               IF (IRET.NE.0) GO TO 199
               END IF
            END IF
         END IF
      GO TO 200
C                                       text file fails
 199  CALL ZTXCLS (LUNPR, PFIND, IRET)
      OFILE = ' '
C                                       record the parameters of the
C                                       fitted function in display
C                                       Do not print the header if
C                                       PRTLEV=2
 200  IF (PRTLEV.LE.1.5) THEN
         WRITE (MSGTXT,1010) (REFANT(I), I = 1, NREF)
         CALL MSGWRT (4)
         IF (NANT.EQ.1) THEN
            WRITE (MSGTXT,1020) NANTS(1), FREQU, STOKE
         ELSE
            J1 = 1
 205        J2 = MIN (NANT, J1+9)
            IF (J2.GE.J1) THEN
               WRITE (MSGTXT,1021) (NANTS(J), J = J1,J2)
               IF (J1.EQ.1) MSGTXT(:23) = 'Fit averaged antennas ='
               CALL MSGWRT (4)
               J1 = J2 + 1
               GO TO 205
               END IF
            WRITE (MSGTXT,1023) FREQU, STOKE
            END IF
         CALL MSGWRT (4)
         WRITE (MSGTXT,1030) AMMAX, PHMAX*RAD2DG
         CALL MSGWRT (4)
C                                       Write the number of data points
         WRITE (MSGTXT,1040) NMEAS
         CALL MSGWRT (4)
         FQ2 = 1.0
         DO 210 I = 1,NNFIT
            A = FITPAR(I)
            B = SQRT(VX(I))
            WRITE (MSGTXT,1050) I-1, A, B, A/FQ2, B/FQ2
            CALL MSGWRT (4)
            FQ2 = FQ2 * FREQU * FREQU
 210        CONTINUE
C                                       The minus because of Rick's
C                                       convention
         WRITE (MSGTXT,1060) -FITPAR(NNFIT+1), SQRT(VX(NNFIT+1))
         CALL MSGWRT (4)
         WRITE (MSGTXT,1070) FITPAR(NNFIT+2), SQRT(VX(NNFIT+2))
         CALL MSGWRT (4)
         IF (OPTYPE.NE.' ') THEN
            WRITE (MSGTXT,1080) EX, SQRT(VX(NNFIT+3))
            CALL MSGWRT (4)
            WRITE (MSGTXT,1090) PHI, SQRT(VX(NNFIT+4)*RAD2DG)
            CALL MSGWRT (4)
            END IF
         WRITE (MSGTXT, 1100) VARRES
         CALL MSGWRT (4)
C                                       relative shift of the two beam
C                                       polarization
         IF (ITRY.EQ.2) THEN
            WRITE (MSGTXT,1140) XSH, RMSX, YSH, RMSY
            CALL MSGWRT (4)
            WRITE (MSGTXT,1150) ROSH, RMSRO, PHISH, RMSPHI
            CALL MSGWRT (4)
            END IF
         END IF
C                                       record the used data if
C                                       prtlev .gt. 0
      IF (PRTLEV.GT.0) THEN
C                                       print the header of the table
         IF (PRTLEV.LT.1.5) THEN
            WRITE (MSGTXT,1110)
            CALL MSGWRT (4)
            IF (OFILE.NE.' ') THEN
               LINE = MSGTXT
               NCH = JTRIM (LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               END IF
            END IF
C
         X0 = FITPAR(NNFIT+1)
         Y0 = FITPAR(NNFIT+2)
         EX = FITPAR(NNFIT+3)
         PHI = FITPAR(NNFIT+4)
         IRET = 0
         SUM = 0.0D0
         DO 240 I = 1,NMEAS
            X1 = (XMEAS(I)-X0)*COS(PHI) + (YMEAS(I)-Y0)*SIN(PHI)
            Y1 = -(XMEAS(I)-X0)*SIN(PHI) + (YMEAS(I)-Y0)*COS(PHI)
            AR = (X1*EX)**2 + Y1**2
C
            FITPOL = FITPAR(1)
            DO 220 L = 2,NNFIT
               FITPOL = FITPOL + FITPAR(L)*(AR**(L-1))
  220          CONTINUE
C                                       calculate the predicted beam
C                                       using Bessel function
C
C                                       AR in radians
            AR = SQRT(AR) * PI/(180*60)
            AR = AR * PI * DIAM * (FREQU*1.0E9) / VELITE
            IF (AR.LT.0.000001) THEN
               FITBES = 1
            ELSE
               BT = AR
               BT = BESSJ1 (BT)
               FITBES = (2.0 * BT / AR) **2
               END IF
C                                       standard output
            IF (PRTLEV.LT.1.5) THEN
               WRITE (MSGTXT,1130) XMEAS(I), YMEAS(I), AMPLS(I), FITPOL,
     *            FITBES, AMPLS(I)-FITPOL
               CALL MSGWRT (4)
               SUM = SUM + AMPLS(I) - FITPOL
C                                       write output file data
               IF (OFILE.NE.' ') THEN
                  LINE = MSGTXT
                  NCH = JTRIM (LINE)
                  CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
                  END IF
C                                       new format of the output file
C                                       without separation for plots
C                                       and in polar coordinates
            ELSE
               DX = XMEAS(I)-X0
               DY = YMEAS(I)-Y0
               RO = SQRT (DX*DX + DY*DY)
               PHIXY = ATAN2(DY, DX) * RAD2DG
               WRITE (MSGTXT,1130) RO, PHIXY, AMPLS(I), FITPOL, FITBES,
     *            AMPLS(I)-FITPOL
               CALL MSGWRT (4)
               SUM = SUM + AMPLS(I) - FITPOL
C                                       write output file data
               IF (OFILE.NE.' ') THEN
                  LINE = MSGTXT
                  NCH = JTRIM (LINE)
                  CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
                  END IF
               END IF
            IF (IRET.NE.0) GO TO 900
 240        CONTINUE
         SUM = SUM / NMEAS
         WRITE (MSGTXT,1160) SUM, NMEAS
         CALL MSGWRT (4)
C                                       write output file data
         IF (OFILE.NE.' ') THEN
            LINE = MSGTXT
            NCH = JTRIM (LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            END IF
         END IF
C
 900  IF (OFILE(1:1).NE.' ') CALL ZTXCLS (LUNPR, PFIND, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Reference antennas ', 30I3)
 1020 FORMAT ('Fitting parms for Antenna ',I2,' Freq = ',F6.3,
     *   ' GHz  Pol = ',A2)
 1021 FORMAT (23X,10I4)
 1022 FORMAT ('Fit ant',I3,' at Freq = ',F6.3,' GHz  Pol = ',A2)
 1023 FORMAT ('Fit at Freq = ',F6.3,' GHz  Pol = ',A2)
 1030 FORMAT ('The data are divided by',F10.4,'  phase',F7.3,' degrees')
 1040 FORMAT ('Number of data points', I5)
 1050 FORMAT ('G',I1,'=',1PE10.3,' +- ',1PE10.3,'  with freq',1PE11.3,
     *   ' +-',1PE10.3)
 1060 FORMAT ('XSHIFT=',F8.3,' +- ',F6.3)
 1070 FORMAT ('YSHIFT=',F8.3,' +- ',F6.3)
 1080 FORMAT ('ECCENTRICITY=',F5.3,' +- ',F5.3)
 1090 FORMAT ('POS. ANGLE=',F6.1, ' +- ',F6.1)
 1100 FORMAT ('RMS of fitting =',F10.5)
 1110 FORMAT (T5,'X,min',T15,'Y,min',T25,'AMPSQR',T35,'FITPOL',T45,
     *   'BESSEL',T55,'Differ')
 1130 FORMAT (6F10.4)
 1140 FORMAT ('DXSHIFT=',F6.3,' +-',F6.3,'  DYSHIFT=',F6.3,' +-',F6.3)
 1150 FORMAT ('ROSHIFT=',F6.3,' +-',F6.3,'  PHISHIFT= (', F6.1,' +-',
     *   F5.1,') degrees')
 1160 FORMAT ('Average difference',F10.5,' over',I6,' measurements')
      END
      SUBROUTINE SOLIN (ARG, FUNC, NFITT, ITIM, FITPR, IRET)
C-----------------------------------------------------------------------
C   Routine to fit a polinomial to the data
C   Input:
C      ARG     D(*)  Array of data arguments
C      FUNC    D(*)  Array of data function
C      NFITT   I     Number of parameters to fit (the polinom degree +1)
C      ITIM    I     Total number of points at arrays ARG and FUNC
C   Output:
C      FITPR   R(*)  Array of found parameters of fitting function
C      IRET    I     Error; 0 => OK
C-----------------------------------------------------------------------
      INTEGER  NFITT, ITIM, I, LESOL, IK, K, L, IRET
      DOUBLE PRECISION ARG(*), FUNC(*),  FITPR(*)
      INCLUDE 'PBEAM.INC'
C
      INTEGER  MAXFT, MAXFT2
      PARAMETER (MAXFT = MAXFIT-4)
      PARAMETER (MAXFT2 = MAXFT*MAXFT)
C
      DOUBLE PRECISION  R(MAXFT), MATR(MAXFT2), NOBS, SUM, SSQ,
     *   SOL(MAXFT), VX(MAXFT), SSQRES, VARRES, VARYY, FIT
C-----------------------------------------------------------------------
C                                       Force result vector R(NFITT),
C                                       matrix M(NFITT*NFITT) to zero
      DO 20 L = 1,NFITT
         R(L) = 0.0
         DO 10 K = 1,NFITT
            IK = K + (L - 1)*NFITT
            MATR(IK) = 0.0
 10         CONTINUE
 20      CONTINUE
      SUM = 0.0
      SSQ = 0.0
      NOBS = 0.0
C                                       Prepare result vector R(NFITT)
C                                       and matrix MATR(NFITT*NFITT)
C                                       for routine DLESQR

      DO 60 I = 1, ITIM
C                                       exclude blank points
         NOBS = NOBS + 1
         SUM = SUM + FUNC(I)
         SSQ = SSQ + FUNC(I)*FUNC(I)
         DO 40  L = 1, NFITT
C
            R(L) = R(L) + FUNC(I) * ARG(I)**(L-1)
C                                       calculate MATR(NFITT*NFITT)
            DO 30 K = L, NFITT
               IK = L + (K - 1)*NFITT
               MATR(IK) = MATR(IK) + ARG(I)**(L+K-2)
 30            CONTINUE
 40         CONTINUE
 60      CONTINUE
C
      CALL DLESQR (NFITT, NOBS, SUM, SSQ, R, MATR, SOL, VX, SSQRES,
     *   VARRES, VARYY, FIT, LESOL)
C                                       solution for gain's coefficients
      DO 80 I = 1, NFITT
         FITPR(I) = SOL(I)
 80      CONTINUE
      IRET = LESOL
C
 999  RETURN
      END
      SUBROUTINE PLTFIT (IRET)
C-----------------------------------------------------------------------
C   PLTFIT plots the data through calls to PLTEL
C   Output:
C      IRET     I  Return code, 0=OK else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'PBEAM.INC'
      INCLUDE 'IMAGE.PBEAM'
      DOUBLE PRECISION DIMAG(IMSIZE,IMSIZE), MIMAG(IMSIZE,IMSIZE), X0,
     *   Y0, MODLS(MAXPOI), X1, Y1, AR, FITPOL, EX, PHI, SUM, DD,
     *   MRING(IMSIZE), DRING(IMSIZE), MRSUM(IMSIZE), DRSUM(IMSIZE),
     *   DIFFS(MAXPOI), RRING(IMSIZE), RRSUM(IMSIZE)
      INTEGER   I, L
      LOGICAL   CONTUR, IRING, DFRING
      REAL      SD, SM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      I = DOPLOT + 0.1
      CONTUR = MOD (I,2).EQ.1
      IRING = MOD (I/2,2).EQ.1
      DFRING = MOD (I/4,2).EQ.1
C                                       compute model, residual
      X0 = FITPAR(NNFIT+1)
      Y0 = FITPAR(NNFIT+2)
      EX = FITPAR(NNFIT+3)
      PHI = FITPAR(NNFIT+4)
      SUM = 0.0
      DO 20 I = 1,NMEAS
         X1 = (XMEAS(I)-X0)*COS(PHI) + (YMEAS(I)-Y0)*SIN(PHI)
         Y1 = -(XMEAS(I)-X0)*SIN(PHI) + (YMEAS(I)-Y0)*COS(PHI)
         AR = (X1*EX)**2 + Y1**2
         FITPOL = FITPAR(1)
         DO 10 L = 2,NNFIT
            FITPOL = FITPOL + FITPAR(L)*(AR**(L-1))
 10         CONTINUE
         MODLS(I) = FITPOL
         DIFFS(I) = AMPLS(I) - FITPOL
         IF (I.GT.1) THEN
            DD = MAX (ABS(XMEAS(I)-XMEAS(I-1)),
     *         ABS(YMEAS(I)-YMEAS(I-1)))
            SUM = SUM + DD
            END IF
 20      CONTINUE
      DD = SUM / MAX (1, NMEAS-1)
C                                       make an image header
      CALL CATINI (CATBLK)
      CALL CHR2H (8, 'BEAM POW', 1, CATH(KHBUN))
      CALL CHR2H (8, 'H-offset', 1, CATH(KHCTP))
      CALL CHR2H (8, 'V-offset', 1, CATH(KHCTP+2))
      X1 = MAX (ABS(XYMAX(1)), ABS(XYMIN(1)))
      Y1 = MAX (ABS(XYMAX(2)), ABS(XYMIN(2)))
      X1 = MAX (X1, Y1)
      X1 = 1.15 * X1 / 256.0
      CATR(KRCIC) = X1 / 60.0
      CATR(KRCIC+1) = X1 / 60.0
      CATBLK(KIDIM) = 2
      CATBLK(KINAX) = IMSIZE
      CATBLK(KINAX+1) = IMSIZE
      CATR(KRCRP) = 256.0
      CATR(KRCRP+1) = 257.0
C                                       plot data
      SD = 0.0
      CALL MAKIMG (NMEAS, VPARM(9), XMEAS, YMEAS, X0, Y0, AMPLS, DIMAG,
     *   DD, SD)
      WRITE (MSGTXT,1020) 'Data', SD
      CALL MSGWRT (2)
      IF (NAMEOU.NE.' ') THEN
         CALL OUTMAP (1, DIMAG, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      IF (CONTUR) THEN
         CALL PLTMAP (1, NMEAS, XMEAS, YMEAS, X0, Y0, DIMAG, IRET)
         IF (IRET.LT.0) CONTUR = .FALSE.
         IF (IRET.GT.0) GO TO 999
         END IF
      IF ((IRING) .OR. (DFRING)) CALL DORING (DIMAG, DRING, DRSUM)
C                                       plot MODEL
      SM = 0.0
      CALL MAKIMG (NMEAS, VPARM(9), XMEAS, YMEAS, X0, Y0, MODLS, MIMAG,
     *   DD, SM)
      WRITE (MSGTXT,1020) 'Model', SM
      CALL MSGWRT (2)
      IF (NAMEOU.NE.' ') THEN
         CALL OUTMAP (2, MIMAG, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      IF (CONTUR) THEN
         CALL PLTMAP (2, NMEAS, XMEAS, YMEAS, X0, Y0, MIMAG, IRET)
         IF (IRET.LT.0) CONTUR = .FALSE.
         IF (IRET.GT.0) GO TO 999
         END IF
      IF ((IRING) .OR. (DFRING)) CALL DORING (MIMAG, MRING, MRSUM)
C                                       plot residual
      SD = (SD + SM) / 2.0
      CALL MAKIMG (NMEAS, VPARM(9), XMEAS, YMEAS, X0, Y0, DIFFS, DIMAG,
     *   DD, SD)
      IF (DFRING) CALL DORING (DIMAG, RRING, RRSUM)
      WRITE (MSGTXT,1021) 'Resdual', SD
      CALL MSGWRT (2)
      IF (NAMEOU.NE.' ') THEN
         CALL OUTMAP (3, DIMAG, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      IF (CONTUR) THEN
         I = 4
         IF (IRING) I = 3
         CALL PLTMAP (I, NMEAS, XMEAS, YMEAS, X0, Y0, DIMAG, IRET)
         IF (IRET.LT.0) CONTUR = .FALSE.
         IF (IRET.GT.0) GO TO 999
         END IF
C                                       plot IRING
      IF (IRING) THEN
         CALL PLRING (DFRING, DRING, MRING, DRSUM, MRSUM, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       plot difference IRING
      IF (DFRING) THEN
         CALL PLDING (RRING, RRSUM, DRING, MRING, IRET)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (A,' image normalized by factor',F6.3)
 1021 FORMAT (A,' image scaled by factor',F6.3)
      END
      SUBROUTINE MAKIMG (NMEAS, VPARM9, XMEAS, YMEAS, X0, Y0, VALS,
     *   IMAG, DD, XN)
C-----------------------------------------------------------------------
C   grid data to make an image
C   Inputs:
C      NMEAS   I      Number samples
C      XMEAS   D(*)   X values
C      YMEAS   D(*)   Y values
C      X0      D      X offset
C      Y0      D      Y offset
C      VALS    D(*)   data to grid
C      DD      D      Typical sample spacing
C   In/out:
C      XN      R
C   Output:
C      IMAG    R()    Image
C   In/out in common
C      CATBLK  I(256)   Image header
C-----------------------------------------------------------------------
      INCLUDE 'IMAGE.PBEAM'
      INTEGER   NMEAS
      REAL      VPARM9, XN
      DOUBLE PRECISION XMEAS(*), YMEAS(*), X0, Y0, VALS(*),
     *   IMAG(IMSIZE,IMSIZE), DD
C
      INTEGER   M, I, J, I1, I2, J1, J2, R
      DOUBLE PRECISION X, Y, W, WT(IMSIZE,IMSIZE), RX, CV(1000)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       compute convolving function
      IF (VPARM9.LE.0.0) VPARM9 = 1.4
      W = DD / ABS (60.0D0*CATR(KRCIC)) / VPARM9
      W = W / SQRT (LOG(3.0D0))
      RX = 0.0
      DO 10 I = 1,1000
         CV(I) = EXP (-((I-1) / W)**2)
         IF (CV(I).GT.0.0005D0) RX = I
 10      CONTINUE
C                                       zero
      I = IMSIZE * IMSIZE
      CALL DFILL (I, 0.0D0, IMAG)
      CALL DFILL (I, 0.0D0, WT)
C                                       loop over points
      DO 50 M = 1,NMEAS
         X = (XMEAS(M) - X0) / CATR(KRCIC) / 60.0 + CATR(KRCRP)
         Y = (YMEAS(M) - Y0) / CATR(KRCIC+1) / 60.0 + CATR(KRCRP+1)
         I1 = X - RX
         J1 = Y - RX
         I2 = X + RX + 0.99
         J2 = Y + RX + 0.99
         I1 = MAX (1, I1)
         J1 = MAX (1, J1)
         I2 = MIN (IMSIZE, I2)
         J2 = MIN (IMSIZE, J2)
         DO 30 J = J1,J2
            DO 20 I = I1,I2
               R = SQRT ((X-I)*(X-I) + (Y-J)*(Y-J)) + 0.5
               IMAG(I,J) = IMAG(I,J) + VALS(M) * CV(R)
               WT(I,J) = WT(I,J) + CV(R)
 20            CONTINUE
 30         CONTINUE
 50      CONTINUE
C                                       normalize
      X = -1.E6
      Y = 1.E6
      DO 70 J = 1,IMSIZE
         DO 60 I = 1,IMSIZE
            IF (WT(I,J).LE.0.0) THEN
               IMAG(I,J) = FBLANK
            ELSE
               IMAG(I,J) = IMAG(I,J) / WT(I,J)
               X = MAX (X, IMAG(I,J))
               Y = MIN (Y, IMAG(I,J))
               END IF
 60         CONTINUE
 70      CONTINUE
      IF (XN.LE.0.0) XN = X
      Y = 1.E6
      X = -Y
      DO 90 J = 1,IMSIZE
         DO 80 I = 1,IMSIZE
            IF (WT(I,J).GT.0.0) THEN
               IMAG(I,J) = IMAG(I,J) / XN
               X = MAX (X, IMAG(I,J))
               Y = MIN (Y, IMAG(I,J))
               END IF
 80         CONTINUE
 90      CONTINUE
C                                       header
      CATR(KRDMX) = X
      CATR(KRDMN) = Y
      CATR(KRBLK) = FBLANK
C
 999  RETURN
      END
      SUBROUTINE OUTMAP (IPLT, IMAG, IRET)
C-----------------------------------------------------------------------
c   OUTMAP creates and fills the output image file
C   Inputs
C      IPLT   I      1,2,3 => data. model. residual
C      IMAG   R      Image(IMSIZE,IMSIZE)
C   Outputs:
C      IRET   I      > 0 => failed.
C-----------------------------------------------------------------------
      INCLUDE 'IMAGE.PBEAM'
      INTEGER   IPLT, IRET
      DOUBLE PRECISION IMAG(IMSIZE,IMSIZE)
C
      INCLUDE 'PBEAM.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   WIN(4), LUN, IND, JBUFSZ, IBIND, IY, DATE(3), TIME(3),
     *   IX
      REAL      BUFF1(MABFSS)
      CHARACTER IPTYPE(3)*1, PHNAME*48, CTIME*8, CDATE*12, HILINE*72
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA IPTYPE /'D','M','R'/
      DATA LUN /28/
C-----------------------------------------------------------------------
      CALL CHR2H (12, NAMEOU, KHIMNO, CATH(KHIMN))
      CLASOU = 'PBEAM' // IPTYPE(IPLT)
      CALL CHR2H (6, CLASOU, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      DISKOU = XDISOU + 0.1
      SEQOU = XSOU + 0.1
      CATBLK(KIIMS) = SEQOU
C                                       Create new cataloged file.
      CALL MCREAT (DISKOU, CNOUT(IPLT), BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IPLT, IRET, 'CREATE IMAGE FILE'
         GO TO 990
         END IF
      SEQOU = CATBLK(KIIMS)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKOU
      FCNO(NCFILE) = CNOUT(IPLT)
      FRW(NCFILE) = 2
C                                       open output
      CALL ZPHFIL ('MA', DISKOU, CNOUT(IPLT), 1, PHNAME, IRET)
      CALL ZOPEN (LUN, IND, DISKOU, PHNAME, .TRUE., .TRUE., .TRUE.,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IPLT, IRET, 'OPEN IMAGE FILE'
         GO TO 990
         END IF
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = IMSIZE
      WIN(4) = IMSIZE
      JBUFSZ = 2 * MABFSS
      CALL MINIT ('WRIT', LUN, IND, IMSIZE, IMSIZE, WIN, BUFF1, JBUFSZ,
     *   1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IPLT, IRET, 'INIT IO TO IMAGE'
         GO TO 990
         END IF
      DO 20 IY = 1,IMSIZE
         CALL MDISK ('WRIT', LUN, IND, BUFF1, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IPLT, IRET, 'WRITE IMAGE ROW'
            GO TO 990
            END IF
         DO 10 IX = 1,IMSIZE
            BUFF1(IBIND+IX-1) = IMAG(IX,IY)
 10         CONTINUE
 20      CONTINUE
      CALL MDISK ('FINI', LUN, IND, BUFF1, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IPLT, IRET, 'WRITE LAST IMAGE ROW'
         GO TO 990
         END IF
      CALL ZCLOSE (LUN, IND, IRET)
C                                       make a history file
      CALL HIINIT (2)
      CALL HICREA (LUN, DISKOU, CNOUT(IPLT), CATBLK, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IPLT, IRET, 'CREATE HISTORY FILE'
         CALL MSGWRT (7)
         END IF
      IF (IRET.EQ.0) THEN
         CALL ZDATE (DATE)
         CALL ZTIME (TIME)
         CALL TIMDAT (TIME, DATE, CTIME, CDATE)
         WRITE (HILINE,1100) TSKNAM, RLSNAM, CDATE, CTIME
         CALL HIADD (LUN, HILINE, BUFFER, IRET)
         CALL HICLOS (LUN, .TRUE., BUFFER, IRET)
         END IF
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OUTMAP IMAGE',I2,' ERROR',I4,' ON ',A)
 1100 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',A12,2X,A8)
      END
      SUBROUTINE PLTMAP (IPLT, NM, XM, YM, X0, Y0, IMAG, IRET)
C-----------------------------------------------------------------------
C   Makes a TV or plot file of an image
C   Inputs:
C      IPLT    I      Which image: 1-3 -> data, model, residual
C      NM      I      Number samples: values below to make + signs
C      XM      R(*)   X values
C      YM      R(*)   Y values
C      X0      R      X offset
C      Y0      R      Y offset
C      IMAG    R(*)   image
C   Outputs:
C      IRET    I      error code
C-----------------------------------------------------------------------
      INCLUDE 'IMAGE.PBEAM'
      INTEGER   IPLT, NM, IRET
      DOUBLE PRECISION XM(*), YM(*), X0, Y0, IMAG(IMSIZE,IMSIZE)
C
      INCLUDE 'PBEAM.INC'
      INTEGER   PLBUFF(256), VER, I, IPSIZE, ITYPE, LUNPL, FINDPL, INP,
     *   LABEL, LTYPE, DEPTH(5), INCHAR, ID(3), IT(3), IERR, NLINE, JPLT
      REAL      BLC(2), TRC(2), XYRATO, CHOUT(4), DX, DY, Y, X
      CHARACTER PFILE*48, TEXT*128, IMGTYP(3)*8, ATIME*8, ADATE*12
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA DEPTH /5*1/
      DATA IMGTYP /'Data', 'Model', 'Residual'/
C-----------------------------------------------------------------------
      JPLT = MIN (3, IPLT)
      BLC(1) = 0.5
      BLC(2) = 0.5
      TRC(1) = IMSIZE + 0.5
      TRC(2) = IMSIZE + 0.5
      XYRATO = 1.0
C                                       fool with location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      CPREF(1,LOCNUM) = ' '
      CPREF(2,LOCNUM) = ' '
      CTYP(1,LOCNUM) = 'H-offset'
      CTYP(2,LOCNUM) = 'V-offset'
      DO 10 I = 1,2
         RPLOC(I,LOCNUM) = CATR(KRCRP+I-1)
         RPVAL(I,LOCNUM) = 0.0D0
         AXINC(I,LOCNUM) = CATR(KRCIC+I-1) * 60.0
10       CONTINUE
C                                       create plot object
      VER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', DISKOU, CNOUT(JPLT), CATBLK, PLBUFF, .TRUE.,
     *      'WRIT', VER, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ZPHFIL ('PL', DISKOU, CNOUT(JPLT), VER, PFILE, IRET)
         IF (IRET.NE.0) GO TO 960
         END IF
      IPSIZE = 0
      ITYPE = 54
      VPARM(18) = JPLT
C
      CALL GINIT (DISKOU, CNOUT(JPLT), PFILE, IPSIZE, ITYPE, NPARM,
     *   XNAMEO, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, PLBUFF, LUNPL,
     *   FINDPL, IRET)
      IF (IRET.NE.0) GO TO 960
C                                       off graphics
      IF ((DOTV) .AND. (GRCHN.LE.0)) THEN
         DO 5 I = 1,4
            CALL YSLECT ('OFFF', I+NGRAY, 7, BUFFER, IRET)
 5          CONTINUE
         END IF
C                                       Number of characters on each
C                                       side of the plot
      CALL RFILL (4, 0.5, CHOUT)
C                                       Not fully initialized, may make
C                                       INP too large which is okay.
      CALL CHNTIC (BLC, TRC, INP)
      INP = MAX (INP, 3)
C                                       standard labeling
      LABEL = 3
      LTYPE = 3
      CHOUT(1) = INP + 4.0
      CHOUT(2) = 3.333
      NLINE = 3
      IF (NANT.GT.1) THEN
         NLINE = NLINE + 1
         IF (NREF.GT.15) NLINE = NLINE + 1
      ELSE
         IF (NREF.GT.11) NLINE = NLINE + 1
         END IF
      CHOUT(4) = 1.0 + 1.333*NLINE
C                                       Init for line drawing.
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, DEPTH, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1000) VER
         CALL MSGWRT (3)
         END IF
C                                       Draw border
      CALL GLTYPE (1, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Top labels: type & name
      DX = 0.0
C                                       Date/time/version
      DY = CHOUT(4) - 1.833
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (TEXT,1100) VER, ADATE, ATIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       The second line of the header
      DY = DY - 1.333
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      TEXT = 'Power beam ' // IMGTYP(JPLT)
      CALL REFRMT (TEXT, ' ', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       the third line of header
      DY = DY - 1.333
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       model
      IF ((NANT.LE.1) .AND. (NREF.LE.11)) THEN
         WRITE (TEXT,1050) IANT, FREQU, STOKE, (REFANT(I), I = 1,NREF)
      ELSE IF (NANT.LE.1) THEN
         WRITE (TEXT,1051) IANT, FREQU, STOKE
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1052) (REFANT(I), I = 1,MIN(24,NREF))
      ELSE IF (NREF.LE.15) THEN
         WRITE (TEXT,1053) (NANTS(I), I = 1,MIN(23,NANT))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1054) FREQU, STOKE, (REFANT(I), I = 1,MIN(24,NREF))
      ELSE
         WRITE (TEXT,1053) (NANTS(I), I = 1,MIN(23,NANT))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1052) (REFANT(I), I = 1,MIN(24,NREF))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1055) FREQU, STOKE
         END IF
      INP = 7
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Put on labels and ticks
      CALL CLAB1 (BLC, TRC, CHOUT, LABEL, XYRATO, .FALSE., PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       little pluses
      CALL GLTYPE (4, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DX = 1.0
      DY = 1.0
      DO 20 I = 1,NM
         X = (XM(I) - X0) / CATR(KRCIC) / 60.0 + CATR(KRCRP)
         Y = (YM(I) - X0) / CATR(KRCIC+1) / 60.0  + CATR(KRCRP+1)
         CALL GPOS (X-DX, Y, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (X+DX, Y, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GPOS (X, Y-DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (X, Y+DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
 20      CONTINUE
C                                       contours
      CALL GLTYPE (2, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL IMGDRW (IMAG, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Done: finish plot
      GPHPAG = IPLT.NE.4
      CALL GFINIS (PLBUFF, IRET)
      IF (IRET.GT.0) GO TO 975
      IF (.NOT.DOTV) THEN
         CALL HIPLOT (DISKOU, CNOUT(JPLT), VER, BUFFER, IRET)
         IRET = 0
         END IF
      GO TO 999
C                                       ZPHFIL or GINIT failure.
 960  WRITE (MSGTXT,2000)
      CALL MSGWRT (8)
C
      IF (.NOT.DOTV) THEN
         CALL DELEXT ('PL', DISKOU, CNOUT(JPLT), 'WRIT', CATBLK, BUFFER,
     *      VER, IERR)
         END IF
      GO TO 999
C                                       Try to finish partial graph
 970  MSGTXT = 'PLOT ERROR OCCURRED: TRY TO FINISH PARTIAL PLOT'
      CALL MSGWRT (7)
      GPHPAG = IPLT.NE.4
      CALL GFINIS (PLBUFF, IERR)
      IF (IERR.NE.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKOU, CNOUT(JPLT), VER, BUFFER, IERR)
            IERR = 0
            END IF
         GO TO 999
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKOU, PFILE, IERR)
         CALL DELEXT ('PL', DISKOU, CNOUT(JPLT), 'WRIT', CATBLK, BUFFER,
     *      VER, IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Plot file version',I4,'  created.')
 1050 FORMAT ('Antenna',I3,' Freq =',F7.3,' GHz Pol = ',A2,
     *   ' RefAnts=',11I3)
 1051 FORMAT ('Antenna',I3,' Freq =',F7.3,' GHz Pol = ',A2)
 1052 FORMAT ('RefAnt =',24I3)
 1053 FORMAT ('Antennas =',23I3)
 1054 FORMAT ('Freq =',F7.3,' GHz Pol = ',A2,' RefAnts=',15I3)
 1055 FORMAT ('Frequency =',F7.3,' GHz Pol = ',A2)
 1100 FORMAT ('Plot file version',I4,'__created ',A, A)
 2000 FORMAT ('PLTEL: ERROR DURING GRAPH FILE CREATION')
      END
      SUBROUTINE IMGDRW (IMAG, PLBUFF, IRET)
C-----------------------------------------------------------------------
C   Draw contours of IMAG in plot buffer
C   Inputs:
C      IMAG     R(IMSIZE,IMSIZE)   Image
C   In/Out:
C      PLBUFF   I(256)       Plot buffer
C   Outputs:
C      IRET     I            Error code
C-----------------------------------------------------------------------
      INCLUDE 'IMAGE.PBEAM'
      INTEGER   PLBUFF(*), IRET
      DOUBLE PRECISION IMAG(IMSIZE,IMSIZE)
C
      REAL      VAL(3), XPOS(3), YPOS(3), TEMP, VC, VL, VM, VS, XA, XB,
     *   XL, XLAST, XM, XS, YA, YB, YL, YLAST, YM, YS, DELTAX, DELTAY,
     *   TLEV, XLEV, ALEVS(30), XP, YP, XSCALE, YSCALE, DMIN, DMAX,
     *   XMIN, YMIN, XMAX, YMAX
      INTEGER   IPERM(3,6), IBLCX, IBLCY, ICOL, II, INDEX, INLEVS,
     *   INPIXS, IPLUS, IROW, ITRCX, ITRCXM, ITRCY, ITRI, I, MININT,
     *   LOCINT, IROUND, ISLEV, JJ, J, IX
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DCNT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IPERM /1,3,2, 3,2,1, 3,1,2, 2,1,3, 1,2,3, 2,3,1/
C-----------------------------------------------------------------------
      XLAST = -1000.
      YLAST = -1000.
      TEMP = IMSIZE - 1
      TEMP = 10 - 3 * LOG10 (TEMP)
      LOCINT = IROUND (TEMP)
      IF (LOCINT.LT.2) LOCINT = 2
      IBLCY = 1
      ITRCY = IMSIZE
      ITRCX = IMSIZE
      IBLCX = 1
      INPIXS = ITRCX - IBLCX + 1
      XMIN = (1.0 - CATR(KRCRP)) * CATR(KRCIC) * 60.0
      YMIN = (1.0 - CATR(KRCRP+1)) * CATR(KRCIC+1) * 60.0
      XMAX = (IMSIZE - CATR(KRCRP)) * CATR(KRCIC) * 60.0
      YMAX = (IMSIZE - CATR(KRCRP+1)) * CATR(KRCIC+1) * 60.0
      XSCALE = (XMAX - XMIN) / (IMSIZE -1.0)
      YSCALE = (YMAX - YMIN) / (IMSIZE -1.0)
C                                       magic parms for dashed lines
      XLEV = 256.0 / INPIXS
      TLEV = 256.0 / (ITRCY - IBLCY + 1.0)
      ISLEV = SQRT (1.0 / (XLEV * TLEV)) + 0.1
      IF (ISLEV.LT.1) ISLEV = 1
      IF (XLEV.LT.1.0) XLEV = (SQRT (XLEV) + 3.0*XLEV) / 4.0
C                                       contour levels
      DMIN = MIN (0.0, CATR(KRDMN))
      DMAX = CATR(KRDMX)
      INLEVS = 0
C                                       data, model
      IF (DMAX.GT.0.8) THEN
         IF (DMIN.LE.-0.05) THEN
            ALEVS(1) = -0.95
            DO 10 J = 2,20
               ALEVS(J) = ALEVS(J-1) + 0.1
 10            CONTINUE
            INLEVS = 20
         ELSE
            ALEVS(1) = 0.05
            DO 15 J = 2,10
               ALEVS(J) = ALEVS(J-1) + 0.1
 15            CONTINUE
            INLEVS = 10
            END IF
C                                       residual
      ELSE
         ALEVS(1) = -0.145
         DO 20 J = 2,30
            ALEVS(J) = ALEVS(J-1) + 0.01
20          CONTINUE
         INLEVS = 20
         END IF
C                                       Save first row.
      DO 30 IX = 1,IMSIZE
         RLROW(IX) = IMAG(IX,1)
 30      CONTINUE
C                                       loop over all rows
      DO 300 IROW = 2,IMSIZE
C                                       Loop over all pixels in row.
         IPLUS = 0
         ITRCXM = IMSIZE - 1
         DO 110 IX = 1,IMSIZE
            BUFF(IX) = IMAG(IX,IROW)
 110        CONTINUE
         DO 250 ICOL = 1,ITRCXM
            IPLUS = IPLUS + 1
C                                       Init values
            VAL(1) = BUFF(IPLUS)
            VAL(2) = BUFF(1+IPLUS)
            VAL(3) = RLROW(IPLUS)
            IF ((ICOL.EQ.256) .AND. (IROW.EQ.256)) THEN
               MSGTXT = 'WE ARE HERE'
               END IF
C                                       Init positions.
            XPOS(1) = ICOL
            XPOS(2) = ICOL + 1
            XPOS(3) = ICOL
            YPOS(1) = IROW
            YPOS(2) = IROW
            YPOS(3) = IROW - 1
C                                       Loop for both triangles.
            DO 200 ITRI = 1,2
C                                       Changes for 2nd triangle.
               IF (ITRI.EQ.2) THEN
                  VAL(1) = RLROW(IPLUS+1)
                  XPOS(1) = ICOL + 1
                  YPOS(1) = IROW - 1
                  END IF
C                                       Order points in triangle.
               DO 130 II = 1,3
                  IF (VAL(II).EQ.FBLANK) GO TO 200
 130              CONTINUE
               INDEX = 0
               IF (VAL(1).GT.VAL(2)) INDEX = 1
               IF (VAL(3).GE.VAL(1)) INDEX = INDEX + 2
               IF (VAL(2).GE.VAL(3)) INDEX = INDEX + 4
C                                       find large, med, small
C                                       values and X,Y positions.
               II = IPERM(1,INDEX)
               VL = VAL(II)
               XL = XPOS(II)
               YL = YPOS(II)
C
               II = IPERM(2,INDEX)
               VM = VAL(II)
               XM = XPOS(II)
               YM = YPOS(II)
C
               II = IPERM(3,INDEX)
               VS = VAL(II)
               XS = XPOS(II)
               YS = YPOS(II)
C                                       Loop for all levels.
               DO 190 II = 1,INLEVS
                  VC = ALEVS(II)
C                                       Cut down negatives
                  IF (VC.GE.0.0) GO TO 140
                     IF ((XLEV.LT.2.85) .AND. (ITRI.EQ.2)) GO TO 190
                     IF (XLEV.GE.1.0) GO TO 140
                        JJ = IROW + ICOL + II
                        IF (MOD(JJ, ISLEV).NE.0) GO TO 190
 140              IF ((VC.GT.VL) .OR. ((VL-VS).LE.0.0)) GO TO 200
C                                       If level not right, next lev.
                  IF (VC.LE.VS) GO TO 190
C                                       Interpolate btwn max two corns.
                  TEMP = (VC-VS) / (VL-VS)
                  XA = TEMP * (XL-XS) + XS
                  YA = TEMP * (YL-YS) + YS
C                                       See which corners 2nd pt. btwn.
                  IF (VC.GT.VM) GO TO 150
                  IF (VM.EQ.VS) GO TO 150
C                                       Level btwn med & small corners.
                     TEMP = (VC-VS) / (VM-VS)
                     XB = TEMP * (XM-XS) + XS
                     YB = TEMP * (YM-YS) + YS
                     GO TO 160
C                                       Level btwn large & med corners.
 150                 TEMP = (VC-VM) / (VL-VM)
                     XB = TEMP * (XL-XM) + XM
                     YB = TEMP * (YL-YM) + YM
C                                       Issue position & write commands
C                                       We can avoid position command
C                                       if we switch A and B.
 160              IF ((XLAST.EQ.XB) .AND. (YLAST.EQ.YB)) THEN
                     TEMP = XA
                     XA = XB
                     XB = TEMP
                     TEMP = YA
                     YA = YB
                     YB = TEMP
C                                       See if we need to position.
                  ELSE IF ((XLAST.NE.XA) .OR. (YLAST.NE.YA)) THEN
                     XP = (XMIN + (XA - 1.) * XSCALE) / CATR(KRCIC)/60.
     *                  + CATR(KRCRP)
                     YP = (YMIN + (YA - 1.) * YSCALE) / CATR(KRCIC+1)
     *                  / 60. + CATR(KRCRP+1)
                     CALL GPOS (XP, YP, PLBUFF, IRET)
                     IF (IRET.NE.0) GO TO 999
                     END IF
C                                       Draw vector.
                  IF (VC.GE.0.0) THEN
                     XP = (XMIN + (XB - 1.) * XSCALE) / CATR(KRCIC)/60.
     *                  + CATR(KRCRP)
                     YP = (YMIN + (YB - 1.) * YSCALE) / CATR(KRCIC+1)
     *                  / 60.0 + CATR(KRCRP+1)
                     CALL GVEC (XP, YP, PLBUFF, IRET)
                     IF (IRET.NE.0) GO TO 999
                     XLAST = XB
                     YLAST = YB
                     GO TO 190
C                                       Negative contours broken
                  ELSE
                     TEMP = LOCINT * SQRT (((XB-XA)**2 + (YB-YA)**2)
     *                  / 2.0)
                     MININT = IROUND (TEMP)
                     IF (MININT.LT.2) MININT = 2
                     DELTAX = (XB - XA) / MININT
                     DELTAY = (YB - YA) / MININT
                     DO 185 I = 1,MININT,2
                        XB = XA + DELTAX
                        YB = YA + DELTAY
                        XP = (XMIN + (XB - 1.) * XSCALE) / CATR(KRCIC)
     *                     /60. + CATR(KRCRP)
                        YP = (YMIN + (YB - 1.) * YSCALE) / CATR(KRCIC+1)
     *                     /60. + CATR(KRCRP+1)
                        CALL GVEC (XP, YP, PLBUFF, IRET)
                        IF (IRET.NE.0) GO TO 999
                        IF (I.LT.MININT-1) THEN
                           XA = XB + DELTAX
                           YA = YB + DELTAY
                           XP = (XMIN + (XA - 1.) * XSCALE) /
     *                        CATR(KRCIC)/60. + CATR(KRCRP)
                           YP = (YMIN + (YA - 1.) * YSCALE) /
     *                        CATR(KRCIC+1)/60. + CATR(KRCRP+1)
                           CALL GPOS (XP, YP, PLBUFF, IRET)
                           IF (IRET.NE.0) GO TO 999
                           END IF
 185                    CONTINUE
                     XLAST = XB
                     YLAST = YB
                     END IF
 190              CONTINUE
 200           CONTINUE
 250        CONTINUE
         CALL RCOPY (INPIXS, BUFF, RLROW)
 300     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE DORING (IMAG, RING, RSUM)
C-----------------------------------------------------------------------
C   DORING sums up IMAG in rings
C   Inputs
C      IMAG   R(*,*)   Image
C   Outputs:
C      RING   R(*)     Average in rings
C      RSUM   R(*)     Sum inside ring
C-----------------------------------------------------------------------
      INCLUDE 'IMAGE.PBEAM'
      DOUBLE PRECISION IMAG(IMSIZE,IMSIZE), RING(*), RSUM(*)
C
      INCLUDE 'PBEAM.INC'
      INTEGER   NP(IMSIZE), I, J, K, NRING
      DOUBLE PRECISION RX, RY, XPRIME, YPRIME, RR, SNP, CSP, EX, PHI
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NRING = IMSIZE / 2
      CALL FILL (IMSIZE, 0, NP)
      CALL DFILL (IMSIZE, 0.0D0, RING)
      EX = FITPAR(NNFIT+3)
      PHI = FITPAR(NNFIT+4)
C                                       geometry from PLTFIT, not IRING
      CSP = COS (PHI)
      SNP = SIN (PHI)
      DO 30 J = 1,IMSIZE
         DO 20 I = 1,IMSIZE
            IF (IMAG(I,J).NE.FBLANK) THEN
               RX = I - CATR(KRCRP)
               RY = J - CATR(KRCRP+1)
               XPRIME = RX * CSP + RY * SNP
               YPRIME = RY * CSP - RX * SNP
               RR = SQRT ((XPRIME*EX)**2 + (YPRIME)**2)
               K = RR + 1.0
               IF ((K.GE.1) .AND. (K.LE.NRING)) THEN
                  NP(K) = NP(K) + 1
                  RING(K) = RING(K) + IMAG(I,J)
                  END IF
               END IF
 20         CONTINUE
 30      CONTINUE
      DO 40 I = 1,NRING
         IF (I.EQ.1) THEN
            RSUM(I) = RING(I)
         ELSE
            RSUM(I) = RSUM(I-1) + RING(I)
            END IF
         IF (NP(I).GT.1) RING(I) = RING(I) / NP(I)
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PLRING (DFRING, DRING, MRING, DRSUM, MRSUM, IRET)
C-----------------------------------------------------------------------
C   PLRING makes plots of the IRING-like data
C   Inputs
C      DFRING  L      more plots after this
C      DRING   R(*)   IRING averages of input data image
C      MRING   R(*)   IRING averages of model image
C   Outputs:
C      IRET    I      Error code
C-----------------------------------------------------------------------
      LOGICAL   DFRING
      INTEGER   IRET
      DOUBLE PRECISION DRING(*), MRING(*), DRSUM(*), MRSUM(*)
C
      INCLUDE 'IMAGE.PBEAM'
      INCLUDE 'PBEAM.INC'
      INTEGER   PLBUFF(256), MP, VER, I, IPSIZE, ITYPE, LUNPL, FINDPL,
     *   INP, LABEL, LTYPE, DEPTH(5), INCHAR, ID(3), IT(3), IERR, NLINE
      REAL      YMAX, BLC(2), TRC(2), XYRATO, CHOUT(4), DX, DY, Y, X,
     *   XSCALE, YSCALE, YMIN
      CHARACTER PFILE*48, TEXT*128, ATIME*8, ADATE*12
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA DEPTH /5*1/
C-----------------------------------------------------------------------
      MP = 0
      YMAX = -1000
      BLC(1) = 0
      BLC(2) = 0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      XYRATO = 1.414
      DO 10 I = 1,IMSIZE
         IF (DRING(I).GT.YMAX) YMAX = DRING(I)
         IF (MRING(I).GT.YMAX) YMAX = MRING(I)
         IF ((DRING(I).GT.0.0) .OR. (MRING(I).GT.0.0)) MP = I
 10      CONTINUE
C                                       fool with location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      CPREF(1,LOCNUM) = ' '
      CPREF(2,LOCNUM) = ' '
      DO 20 I = 1,2
         RPLOC(I,LOCNUM) = 0.0
         RPVAL(I,LOCNUM) = 0.0D0
20       CONTINUE
      YMIN = -0.03 * YMAX
      YMAX = 1.03 * YMAX
      RPVAL(2,LOCNUM) = YMIN
      CTYP(1,LOCNUM) = 'Radius'
      CTYP(2,LOCNUM) = 'Beam pow'
      AXINC(1,LOCNUM) = (MP + 1) / 1000.0 * CATR(KRCIC) * 60.0
      AXINC(2,LOCNUM) = (YMAX-YMIN) / 1000.0
      XSCALE = 1000. / (MP + 1)
      YSCALE = 1000.0 / (YMAX-YMIN)
C                                       create plot object
      VER = 0
      IF (.NOT.DOTV) THEN
         CALL CATIO ('READ', DISKOU, CNOUT(1), CATBLK, 'REST', PLBUFF,
     *      IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.4)) THEN
            WRITE (MSGTXT,1000) IRET, 'RE-READ CATALOG HEADER'
            GO TO 990
            END IF
         CALL MADDEX ('PL', DISKOU, CNOUT(1), CATBLK, PLBUFF, .TRUE.,
     *      'WRIT', VER, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ZPHFIL ('PL', DISKOU, CNOUT(1), VER, PFILE, IRET)
         IF (IRET.NE.0) GO TO 960
         END IF
      IPSIZE = 0
      ITYPE = 54
      VPARM(18) = 4.0
      VPARM(19) = YMIN
      VPARM(20) = YMAX
C
      CALL GINIT (DISKOU, CNOUT(1), PFILE, IPSIZE, ITYPE, NPARM,
     *   XNAMEO, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, PLBUFF, LUNPL,
     *   FINDPL, IRET)
      IF (IRET.NE.0) GO TO 960
C                                       off graphics
      IF ((DOTV) .AND. (GRCHN.LE.0)) THEN
         DO 5 I = 1,4
            CALL YSLECT ('OFFF', I+NGRAY, 7, BUFFER, IRET)
 5          CONTINUE
         END IF
C                                       Number of characters on each
C                                       side of the plot
      CALL RFILL (4, 0.5, CHOUT)
C                                       Not fully initialized, may make
C                                       INP too large which is okay.
      CALL CHNTIC (BLC, TRC, INP)
      INP = MAX (INP, 3)
C                                       standard labeling
      LABEL = 3
      LTYPE = 3
      CHOUT(1) = INP + 4.0
      CHOUT(2) = 3.333
      NLINE = 3
      IF (NANT.GT.1) THEN
         NLINE = NLINE + 1
         IF (NREF.GT.15) NLINE = NLINE + 1
      ELSE
         IF (NREF.GT.11) NLINE = NLINE + 1
         END IF
      CHOUT(4) = 1.0 + 1.333*NLINE
C                                       Init for line drawing.
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, DEPTH, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1001) VER
         CALL MSGWRT (3)
         END IF
C                                       Draw border
      CALL GLTYPE (1, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Top labels: type & name
      DX = 0.0
C                                       Date/time/version
      DY = CHOUT(4) - 1.833
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (TEXT,1100) VER, ADATE, ATIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       The second line of the header
      DY = DY - 1.333
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      TEXT = 'Power beam azimuthal average'
      CALL REFRMT (TEXT, ' ', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       the third line of header
      DY = DY - 1.333
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       model
      IF ((NANT.LE.1) .AND. (NREF.LE.11)) THEN
         WRITE (TEXT,1050) IANT, FREQU, STOKE, (REFANT(I), I = 1,NREF)
      ELSE IF (NANT.LE.1) THEN
         WRITE (TEXT,1051) IANT, FREQU, STOKE
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         WRITE (TEXT,1052) (REFANT(I), I = 1,MIN(24,NREF))
      ELSE IF (NREF.LE.15) THEN
         WRITE (TEXT,1053) (NANTS(I), I = 1,MIN(23,NANT))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1054) FREQU, STOKE, (REFANT(I), I = 1,MIN(24,NREF))
      ELSE
         WRITE (TEXT,1053) (NANTS(I), I = 1,MIN(23,NANT))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1052) (REFANT(I), I = 1,MIN(24,NREF))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1055) FREQU, STOKE
         END IF
      INP = 7
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Put on labels and ticks
      CALL CLAB1 (BLC, TRC, CHOUT, LABEL, XYRATO, .FALSE., PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DX = 5.0
      DY = 5.0
C                                       little pluses for model
      CALL GCOMNT (-1, 'Plot model rings as pluses', PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GLTYPE (3, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DO 50 I = 1,MP
         X = I * XSCALE
         Y = (MRING(I)-YMIN) * YSCALE
         CALL GPOS (X, Y+DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (X, Y-DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GPOS (X-DX, Y, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (X+DX, Y, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
 50      CONTINUE
C                                       little X's for data
      CALL GCOMNT (-1, 'Plot data rings as Xs', PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GLTYPE (4, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DO 60 I = 1,MP
         X = I * XSCALE
         Y = (DRING(I)-YMIN) * YSCALE
         CALL GPOS (X+DX, Y+DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (X-DX, Y-DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GPOS (X-DX, Y+DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (X+DX, Y-DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
 60      CONTINUE
C                                       little X's for data
      CALL GCOMNT (-1, 'Connect data rings', PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GLTYPE (2, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DO 70 I = 1,MP
         X = I * XSCALE
         Y = (DRING(I)-YMIN) * YSCALE
         IF (I.EQ.1) THEN
            CALL GPOS (X, Y, PLBUFF, IRET)
         ELSE
            CALL GVEC (X, Y, PLBUFF, IRET)
            END IF
         IF (IRET.NE.0) GO TO 970
 70      CONTINUE
C                                       Done: finish plot
      GPHPAG = .TRUE.
      CALL GFINIS (PLBUFF, IRET)
      IF (IRET.GT.0) GO TO 975
      IF (IRET.LT.0) GO TO 999
      IF (.NOT.DOTV) THEN
         CALL HIPLOT (DISKOU, CNOUT(1), VER, BUFFER, IRET)
         IRET = 0
         END IF
C                                       sum plot
      YMAX = -10000.
      DO 110 I = 2,MP
         IF (DRSUM(I).GT.YMAX) YMAX = DRSUM(I)
         IF (MRSUM(I).GT.YMAX) YMAX = MRSUM(I)
 110     CONTINUE
      DO 120 I = 1,MP
         MRSUM(I) = MRSUM(I) / YMAX
         DRSUM(I) = DRSUM(I) / YMAX
 120     CONTINUE
      YMAX = 1.03
      YMIN = -0.03
      CTYP(2,LOCNUM) = 'Beam sum'
      AXINC(1,LOCNUM) = (MP + 1) / 1000.0 * CATR(KRCIC) * 60.0
      AXINC(2,LOCNUM) = (YMAX-YMIN) / 1000.0
      XSCALE = 1000.0 / (MP + 1.0)
      YSCALE = 1000.0 / (YMAX-YMIN)
C                                       create plot object
      VER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', DISKOU, CNOUT(1), CATBLK, PLBUFF, .TRUE.,
     *      'WRIT', VER, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ZPHFIL ('PL', DISKOU, CNOUT(1), VER, PFILE, IRET)
         IF (IRET.NE.0) GO TO 960
         END IF
      IPSIZE = 0
      ITYPE = 54
      VPARM(18) = 5.0
      VPARM(19) = YMIN
      VPARM(20) = YMAX
C
      CALL GINIT (DISKOU, CNOUT(1), PFILE, IPSIZE, ITYPE, NPARM,
     *   XNAMEO, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, PLBUFF, LUNPL,
     *   FINDPL, IRET)
      IF (IRET.NE.0) GO TO 960
C                                       Number of characters on each
C                                       side of the plot
      CALL RFILL (4, 0.5, CHOUT)
C                                       Not fully initialized, may make
C                                       INP too large which is okay.
      CALL CHNTIC (BLC, TRC, INP)
      INP = MAX (INP, 3)
C                                       standard labeling
      LABEL = 3
      LTYPE = 3
      CHOUT(1) = INP + 4.0
      CHOUT(2) = 3.333
      NLINE = 3
      IF (NANT.GT.1) THEN
         NLINE = NLINE + 1
         IF (NREF.GT.15) NLINE = NLINE + 1
      ELSE
         IF (NREF.GT.11) NLINE = NLINE + 1
         END IF
      CHOUT(4) = 1.0 + 1.333*NLINE
C                                       Init for line drawing.
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, DEPTH, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1001) VER
         CALL MSGWRT (3)
         END IF
C                                       Draw border
      CALL GLTYPE (1, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Top labels: type & name
      DX = 0.0
C                                       Date/time/version
      DY = CHOUT(4) - 1.833
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (TEXT,1100) VER, ADATE, ATIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       The second line of the header
      DY = DY - 1.333
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      TEXT = 'Power beam azimuthal average'
      CALL REFRMT (TEXT, ' ', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       the third line of header
      DY = DY - 1.333
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       model
      IF ((NANT.LE.1) .AND. (NREF.LE.11)) THEN
         WRITE (TEXT,1050) IANT, FREQU, STOKE, (REFANT(I), I = 1,NREF)
      ELSE IF (NANT.LE.1) THEN
         WRITE (TEXT,1051) IANT, FREQU, STOKE
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1052) (REFANT(I), I = 1,MIN(24,NREF))
      ELSE IF (NREF.LE.15) THEN
         WRITE (TEXT,1053) (NANTS(I), I = 1,MIN(23,NANT))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1054) FREQU, STOKE, (REFANT(I), I = 1,MIN(24,NREF))
      ELSE
         WRITE (TEXT,1053) (NANTS(I), I = 1,MIN(23,NANT))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1052) (REFANT(I), I = 1,MIN(24,NREF))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1055) FREQU, STOKE
         END IF
      INP = 7
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Put on labels and ticks
      CALL CLAB1 (BLC, TRC, CHOUT, LABEL, XYRATO, .FALSE., PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DX = 5.0
      DY = 5.0
C                                       little pluses for model
      CALL GCOMNT (-1, 'Plot model rings as pluses', PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GLTYPE (3, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DO 150 I = 1,MP
         X = I * XSCALE
         Y = (MRSUM(I)-YMIN) * YSCALE
         CALL GPOS (X, Y+DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (X, Y-DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GPOS (X-DX, Y, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (X+DX, Y, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
 150     CONTINUE
C                                       little X's for data
      CALL GCOMNT (-1, 'Plot data rings as Xs', PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GLTYPE (4, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DO 160 I = 1,MP
         X = I * XSCALE
         Y = (DRSUM(I)-YMIN) * YSCALE
         CALL GPOS (X+DX, Y+DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (X-DX, Y-DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GPOS (X-DX, Y+DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (X+DX, Y-DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
 160     CONTINUE
C                                       little X's for data
      CALL GCOMNT (-1, 'Connect data rings', PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GLTYPE (2, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DO 170 I = 1,MP
         X = I * XSCALE
         Y = (DRSUM(I)-YMIN) * YSCALE
         IF (I.EQ.1) THEN
            CALL GPOS (X, Y, PLBUFF, IRET)
         ELSE
            CALL GVEC (X, Y, PLBUFF, IRET)
            END IF
         IF (IRET.NE.0) GO TO 970
 170     CONTINUE
C                                       Done: finish plot
      GPHPAG = DFRING
      CALL GFINIS (PLBUFF, IRET)
      IF (IRET.GT.0) GO TO 975
      IF (.NOT.DOTV) THEN
         CALL HIPLOT (DISKOU, CNOUT(1), VER, BUFFER, IRET)
         IRET = 0
         END IF
      GO TO 999
C                                       ZPHFIL or GINIT failure.
 960  WRITE (MSGTXT,2000)
      CALL MSGWRT (8)
C
      IF (.NOT.DOTV) THEN
         CALL DELEXT ('PL', DISKOU, CNOUT(1), 'WRIT', CATBLK, BUFFER,
     *      VER, IERR)
         END IF
      GO TO 999
C                                       Try to finish partial graph
 970  MSGTXT = 'PLOT ERROR OCCURRED: TRY TO FINISH PARTIAL PLOT'
      CALL MSGWRT (7)
      GPHPAG = .FALSE.
      CALL GFINIS (PLBUFF, IERR)
      IF (IERR.NE.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKOU, CNOUT(1), VER, BUFFER, IERR)
            IERR = 0
            END IF
         GO TO 999
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKOU, PFILE, IERR)
         CALL DELEXT ('PL', DISKOU, CNOUT(1), 'WRIT', CATBLK, BUFFER,
     *      VER, IERR)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PLRING ERROR',I4,' ON ',A)
 1001 FORMAT ('Plot file version',I4,'  created.')
 1050 FORMAT ('Antenna',I3,' Freq =',F7.3,' GHz Pol = ',A2,
     *   ' RefAnts=',11I3)
 1051 FORMAT ('Antenna',I3,' Freq =',F7.3,' GHz Pol = ',A2)
 1052 FORMAT ('RefAnt =',24I3)
 1053 FORMAT ('Antennas =',23I3)
 1054 FORMAT ('Freq =',F7.3,' GHz Pol = ',A2,' RefAnts=',15I3)
 1055 FORMAT ('Frequency =',F7.3,' GHz Pol = ',A2)
 1100 FORMAT ('Plot file version',I4,'__created ',A, A)
 2000 FORMAT ('PLTEL: ERROR DURING GRAPH FILE CREATION')
      END
      SUBROUTINE PLDING (RRING, RRSUM, DRING, MRING, IRET)
C-----------------------------------------------------------------------
C   PLDING makes plots of the IRING-like data: difference and diff/model
C   Inputs
C      RRING   D(*)   IRING averages of residual image
C      RRSUM   D(*)   IRING sums of residual image
C      DRING   D(*)   IRING averages of input data image
C      MRING   D(*)   IRING averages of model image
C   Outputs:
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
      DOUBLE PRECISION RRING(*), RRSUM(*), DRING(*), MRING(*)
C
      INCLUDE 'IMAGE.PBEAM'
      INCLUDE 'PBEAM.INC'
      INTEGER   PLBUFF(256), MP, VER, I, IPSIZE, ITYPE, LUNPL, FINDPL,
     *   INP, LABEL, LTYPE, DEPTH(5), INCHAR, ID(3), IT(3), IERR, NLINE
      REAL      YMAX, BLC(2), TRC(2), XYRATO, CHOUT(4), DX, DY, Y, X,
     *   XSCALE, YSCALE, YMIN
      CHARACTER PFILE*48, TEXT*128, ATIME*8, ADATE*12
      LOGICAL   UP
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA DEPTH /5*1/
C-----------------------------------------------------------------------
      MP = 0
      YMAX = -1000.
      YMIN = 1000.
      BLC(1) = 0
      BLC(2) = 0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      XYRATO = 1.414
C                                       true difference
C                                       compute ratios
C                                       find max/min of diffs
      DO 10 I = 1,IMSIZE
         IF ((DRING(I).GT.0.0) .OR. (MRING(I).GT.0.0)) MP = I
         IF (RRING(I).GT.YMAX) YMAX = RRING(I)
         IF (RRING(I).LT.YMIN) YMIN = RRING(I)
         IF (MRING(I).GT.0.0) THEN
            MRING(I) = RRING(I) / MRING(I)
         ELSE IF (DRING(I).GE.0.0) THEN
            MRING(I) = 1000.
         ELSE
            MRING(I) = -1000.
            END IF
 10      CONTINUE
C                                       fool with location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      CPREF(1,LOCNUM) = ' '
      CPREF(2,LOCNUM) = ' '
      DO 15 I = 1,2
         RPLOC(I,LOCNUM) = 0.0
         RPVAL(I,LOCNUM) = 0.0D0
 15      CONTINUE
      X = YMAX - YMIN
      YMIN = YMIN - 0.03*X
      YMAX = YMAX + 0.03*X
      RPVAL(2,LOCNUM) = YMIN
      CTYP(1,LOCNUM) = 'Radius'
      CTYP(2,LOCNUM) = 'Beam dif'
      AXINC(1,LOCNUM) = (MP + 1) / 1000.0 * CATR(KRCIC) * 60.0
      AXINC(2,LOCNUM) = (YMAX-YMIN) / 1000.0
      XSCALE = 1000. / (MP + 1)
      YSCALE = 1000.0 / (YMAX-YMIN)
C                                       create plot object
      VER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', DISKOU, CNOUT(1), CATBLK, PLBUFF, .TRUE.,
     *      'WRIT', VER, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ZPHFIL ('PL', DISKOU, CNOUT(1), VER, PFILE, IRET)
         IF (IRET.NE.0) GO TO 960
         END IF
      IPSIZE = 0
      ITYPE = 54
      VPARM(18) = 6.0
      VPARM(19) = YMIN
      VPARM(20) = YMAX
C
      CALL GINIT (DISKOU, CNOUT(1), PFILE, IPSIZE, ITYPE, NPARM,
     *   XNAMEO, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, PLBUFF, LUNPL,
     *   FINDPL, IRET)
      IF (IRET.NE.0) GO TO 960
C                                       off graphics
      IF ((DOTV) .AND. (GRCHN.LE.0)) THEN
         DO 5 I = 1,4
            CALL YSLECT ('OFFF', I+NGRAY, 7, BUFFER, IRET)
 5          CONTINUE
         END IF
C                                       Number of characters on each
C                                       side of the plot
      CALL RFILL (4, 0.5, CHOUT)
C                                       Not fully initialized, may make
C                                       INP too large which is okay.
      CALL CHNTIC (BLC, TRC, INP)
      INP = MAX (INP, 3)
C                                       standard labeling
      LABEL = 3
      LTYPE = 3
      CHOUT(1) = INP + 4.0
      CHOUT(2) = 3.333
      NLINE = 3
      IF (NANT.GT.1) THEN
         NLINE = NLINE + 1
         IF (NREF.GT.15) NLINE = NLINE + 1
      ELSE
         IF (NREF.GT.11) NLINE = NLINE + 1
         END IF
      CHOUT(4) = 1.0 + 1.333*NLINE
C                                       Init for line drawing.
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, DEPTH, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1001) VER
         CALL MSGWRT (3)
         END IF
C                                       Draw border
      CALL GLTYPE (1, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Top labels: type & name
      DX = 0.0
C                                       Date/time/version
      DY = CHOUT(4) - 1.833
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (TEXT,1100) VER, ADATE, ATIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       The second line of the header
      DY = DY - 1.333
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      TEXT = 'Power beam azimuthal average data-model'
      CALL REFRMT (TEXT, ' ', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       the third line of header
      DY = DY - 1.333
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       model
      IF ((NANT.LE.1) .AND. (NREF.LE.11)) THEN
         WRITE (TEXT,1050) IANT, FREQU, STOKE, (REFANT(I), I = 1,NREF)
      ELSE IF (NANT.LE.1) THEN
         WRITE (TEXT,1051) IANT, FREQU, STOKE
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1052) (REFANT(I), I = 1,MIN(24,NREF))
      ELSE IF (NREF.LE.15) THEN
         WRITE (TEXT,1053) (NANTS(I), I = 1,MIN(23,NANT))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1054) FREQU, STOKE, (REFANT(I), I = 1,MIN(24,NREF))
      ELSE
         WRITE (TEXT,1053) (NANTS(I), I = 1,MIN(23,NANT))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1052) (REFANT(I), I = 1,MIN(24,NREF))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1055) FREQU, STOKE
         END IF
      INP = 7
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Put on labels and ticks
      CALL CLAB1 (BLC, TRC, CHOUT, LABEL, XYRATO, .FALSE., PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DX = 5.0
      DY = 5.0
C                                       little X's for data
      CALL GCOMNT (-1, 'Plot data-model rings as Xs', PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GLTYPE (4, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DO 60 I = 1,MP
         X = I * XSCALE
         Y = (RRING(I)-YMIN) * YSCALE
         CALL GPOS (X+DX, Y+DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (X-DX, Y-DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GPOS (X-DX, Y+DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL GVEC (X+DX, Y-DY, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
 60      CONTINUE
C                                       little X's for data
      CALL GCOMNT (-1, 'Connect data-model rings', PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GLTYPE (2, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DO 70 I = 1,MP
         X = I * XSCALE
         Y = (RRING(I)-YMIN) * YSCALE
         IF (I.EQ.1) THEN
            CALL GPOS (X, Y, PLBUFF, IRET)
         ELSE
            CALL GVEC (X, Y, PLBUFF, IRET)
            END IF
         IF (IRET.NE.0) GO TO 970
 70      CONTINUE
C                                       Done: finish plot
      GPHPAG = .TRUE.
      CALL GFINIS (PLBUFF, IRET)
      IF (IRET.GT.0) GO TO 975
      IF (IRET.LT.0) GO TO 999
      IF (.NOT.DOTV) THEN
         CALL HIPLOT (DISKOU, CNOUT(1), VER, BUFFER, IRET)
         IRET = 0
         END IF
C                                       true diff sum plot
      YMAX = -10000.
      YMIN = 10000.
      DO 110 I = 2,MP
         IF (RRSUM(I).GT.YMAX) YMAX = RRSUM(I)
         IF (RRSUM(I).LT.YMIN) YMIN = RRSUM(I)
 110     CONTINUE
      X = YMAX - YMIN
      YMAX = YMAX + 0.03*X
      YMIN = YMIN - 0.03*X
      CTYP(2,LOCNUM) = 'diff sum'
      RPVAL(2,LOCNUM) = YMIN
      AXINC(1,LOCNUM) = (MP + 1) / 1000.0 * CATR(KRCIC) * 60.0
      AXINC(2,LOCNUM) = (YMAX-YMIN) / 1000.0
      XSCALE = 1000.0 / (MP + 1.0)
      YSCALE = 1000.0 / (YMAX-YMIN)
C                                       create plot object
      VER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', DISKOU, CNOUT(1), CATBLK, PLBUFF, .TRUE.,
     *      'WRIT', VER, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ZPHFIL ('PL', DISKOU, CNOUT(1), VER, PFILE, IRET)
         IF (IRET.NE.0) GO TO 960
         END IF
      IPSIZE = 0
      ITYPE = 54
      VPARM(18) = 7.0
      VPARM(19) = YMIN
      VPARM(20) = YMAX
C
      CALL GINIT (DISKOU, CNOUT(1), PFILE, IPSIZE, ITYPE, NPARM,
     *   XNAMEO, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, PLBUFF, LUNPL,
     *   FINDPL, IRET)
      IF (IRET.NE.0) GO TO 960
C                                       Number of characters on each
C                                       side of the plot
      CALL RFILL (4, 0.5, CHOUT)
C                                       Not fully initialized, may make
C                                       INP too large which is okay.
      CALL CHNTIC (BLC, TRC, INP)
      INP = MAX (INP, 3)
C                                       standard labeling
      LABEL = 3
      LTYPE = 3
      CHOUT(1) = INP + 4.0
      CHOUT(2) = 3.333
      NLINE = 3
      IF (NANT.GT.1) THEN
         NLINE = NLINE + 1
         IF (NREF.GT.15) NLINE = NLINE + 1
      ELSE
         IF (NREF.GT.11) NLINE = NLINE + 1
         END IF
      CHOUT(4) = 1.0 + 1.333*NLINE
C                                       Init for line drawing.
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, DEPTH, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1001) VER
         CALL MSGWRT (3)
         END IF
C                                       Draw border
      CALL GLTYPE (1, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Top labels: type & name
      DX = 0.0
C                                       Date/time/version
      DY = CHOUT(4) - 1.833
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (TEXT,1100) VER, ADATE, ATIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       The second line of the header
      DY = DY - 1.333
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      TEXT = 'Power beam azimuthal average sum of diff'
      CALL REFRMT (TEXT, ' ', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       the third line of header
      DY = DY - 1.333
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       model
      IF ((NANT.LE.1) .AND. (NREF.LE.11)) THEN
         WRITE (TEXT,1050) IANT, FREQU, STOKE, (REFANT(I), I = 1,NREF)
      ELSE IF (NANT.LE.1) THEN
         WRITE (TEXT,1051) IANT, FREQU, STOKE
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1052) (REFANT(I), I = 1,MIN(24,NREF))
      ELSE IF (NREF.LE.15) THEN
         WRITE (TEXT,1053) (NANTS(I), I = 1,MIN(23,NANT))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1054) FREQU, STOKE, (REFANT(I), I = 1,MIN(24,NREF))
      ELSE
         WRITE (TEXT,1053) (NANTS(I), I = 1,MIN(23,NANT))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1052) (REFANT(I), I = 1,MIN(24,NREF))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1055) FREQU, STOKE
         END IF
      INP = 7
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Put on labels and ticks
      CALL CLAB1 (BLC, TRC, CHOUT, LABEL, XYRATO, .FALSE., PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DX = 5.0
      DY = 5.0
C                                       little X's for data
      CALL GCOMNT (-1, 'Plot (data-model)/model rings as Xs', PLBUFF,
     *   IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GLTYPE (4, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DO 160 I = 1,MP
         IF ((RRSUM(I).GT.YMIN) .AND. (RRSUM(I).LT.YMAX)) THEN
            X = I * XSCALE
            Y = (RRSUM(I)-YMIN) * YSCALE
            CALL GPOS (X+DX, Y+DY, PLBUFF, IRET)
            IF (IRET.NE.0) GO TO 970
            CALL GVEC (X-DX, Y-DY, PLBUFF, IRET)
            IF (IRET.NE.0) GO TO 970
            CALL GPOS (X-DX, Y+DY, PLBUFF, IRET)
            IF (IRET.NE.0) GO TO 970
            CALL GVEC (X+DX, Y-DY, PLBUFF, IRET)
            IF (IRET.NE.0) GO TO 970
            END IF
 160     CONTINUE
C                                       little X's for data
      CALL GCOMNT (-1, 'Connect data rings', PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GLTYPE (2, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      UP = .TRUE.
      DO 170 I = 1,MP
         IF ((RRSUM(I).GT.YMIN) .AND. (RRSUM(I).LT.YMAX)) THEN
            X = I * XSCALE
            Y = (RRSUM(I)-YMIN) * YSCALE
            IF (UP) THEN
               CALL GPOS (X, Y, PLBUFF, IRET)
            ELSE
               CALL GVEC (X, Y, PLBUFF, IRET)
               END IF
            IF (IRET.NE.0) GO TO 970
            UP = .FALSE.
         ELSE
            UP = .TRUE.
            END IF
 170     CONTINUE
C                                       Done: finish plot
      GPHPAG = .TRUE.
      CALL GFINIS (PLBUFF, IRET)
      IF (IRET.GT.0) GO TO 975
      IF (IRET.LT.0) GO TO 999
      IF (.NOT.DOTV) THEN
         CALL HIPLOT (DISKOU, CNOUT(1), VER, BUFFER, IRET)
         IRET = 0
         END IF
C                                       diff/mod
      YMAX = -10000.
      YMIN = 10000.
      DO 210 I = 2,MP
         IF (MRING(I).GT.YMAX) YMAX = MRING(I)
         IF (MRING(I).LT.YMIN) YMIN = MRING(I)
 210     CONTINUE
      YMAX = MIN (5.0, YMAX)
      YMIN = MAX (-5.0, YMIN)
      X = YMAX - YMIN
      YMAX = YMAX + 0.03*X
      YMIN = YMIN - 0.03*X
      CTYP(2,LOCNUM) = 'diff/mod'
      RPVAL(2,LOCNUM) = YMIN
      AXINC(1,LOCNUM) = (MP + 1) / 1000.0 * CATR(KRCIC) * 60.0
      AXINC(2,LOCNUM) = (YMAX-YMIN) / 1000.0
      XSCALE = 1000.0 / (MP + 1.0)
      YSCALE = 1000.0 / (YMAX-YMIN)
C                                       create plot object
      VER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', DISKOU, CNOUT(1), CATBLK, PLBUFF, .TRUE.,
     *      'WRIT', VER, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ZPHFIL ('PL', DISKOU, CNOUT(1), VER, PFILE, IRET)
         IF (IRET.NE.0) GO TO 960
         END IF
      IPSIZE = 0
      ITYPE = 54
      VPARM(18) = 8.0
      VPARM(19) = YMIN
      VPARM(20) = YMAX
C
      CALL GINIT (DISKOU, CNOUT(1), PFILE, IPSIZE, ITYPE, NPARM,
     *   XNAMEO, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, PLBUFF, LUNPL,
     *   FINDPL, IRET)
      IF (IRET.NE.0) GO TO 960
C                                       Number of characters on each
C                                       side of the plot
      CALL RFILL (4, 0.5, CHOUT)
C                                       Not fully initialized, may make
C                                       INP too large which is okay.
      CALL CHNTIC (BLC, TRC, INP)
      INP = MAX (INP, 3)
C                                       standard labeling
      LABEL = 3
      LTYPE = 3
      CHOUT(1) = INP + 4.0
      CHOUT(2) = 3.333
      NLINE = 3
      IF (NANT.GT.1) THEN
         NLINE = NLINE + 1
         IF (NREF.GT.15) NLINE = NLINE + 1
      ELSE
         IF (NREF.GT.11) NLINE = NLINE + 1
         END IF
      CHOUT(4) = 1.0 + 1.333*NLINE
C                                       Init for line drawing.
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, DEPTH, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      IF (.NOT.DOTV) THEN
         WRITE (MSGTXT,1001) VER
         CALL MSGWRT (3)
         END IF
C                                       Draw border
      CALL GLTYPE (1, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Top labels: type & name
      DX = 0.0
C                                       Date/time/version
      DY = CHOUT(4) - 1.833
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (TEXT,1100) VER, ADATE, ATIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       The second line of the header
      DY = DY - 1.333
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      TEXT = 'Power beam azimuthal average (data-model)/model'
      CALL REFRMT (TEXT, ' ', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       the third line of header
      DY = DY - 1.333
      CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       model
      IF ((NANT.LE.1) .AND. (NREF.LE.11)) THEN
         WRITE (TEXT,1050) IANT, FREQU, STOKE, (REFANT(I), I = 1,NREF)
      ELSE IF (NANT.LE.1) THEN
         WRITE (TEXT,1051) IANT, FREQU, STOKE
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1052) (REFANT(I), I = 1,MIN(24,NREF))
      ELSE IF (NREF.LE.15) THEN
         WRITE (TEXT,1053) (NANTS(I), I = 1,MIN(23,NANT))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1054) FREQU, STOKE, (REFANT(I), I = 1,MIN(24,NREF))
      ELSE
         WRITE (TEXT,1053) (NANTS(I), I = 1,MIN(23,NANT))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1052) (REFANT(I), I = 1,MIN(24,NREF))
         INP = 7
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         DY = DY - 1.333
         CALL GPOS (BLC(1), TRC(2), PLBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (TEXT,1055) FREQU, STOKE
         END IF
      INP = 7
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Put on labels and ticks
      CALL CLAB1 (BLC, TRC, CHOUT, LABEL, XYRATO, .FALSE., PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DX = 5.0
      DY = 5.0
C                                       little X's for data
      CALL GCOMNT (-1, 'Plot (data-model)/model rings as Xs', PLBUFF,
     *   IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GLTYPE (4, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      DO 260 I = 1,MP
         IF ((MRING(I).GT.YMIN) .AND. (MRING(I).LT.YMAX)) THEN
            X = I * XSCALE
            Y = (MRING(I)-YMIN) * YSCALE
            CALL GPOS (X+DX, Y+DY, PLBUFF, IRET)
            IF (IRET.NE.0) GO TO 970
            CALL GVEC (X-DX, Y-DY, PLBUFF, IRET)
            IF (IRET.NE.0) GO TO 970
            CALL GPOS (X-DX, Y+DY, PLBUFF, IRET)
            IF (IRET.NE.0) GO TO 970
            CALL GVEC (X+DX, Y-DY, PLBUFF, IRET)
            IF (IRET.NE.0) GO TO 970
            END IF
 260     CONTINUE
C                                       little X's for data
      CALL GCOMNT (-1, 'Connect data rings', PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL GLTYPE (2, PLBUFF, IRET)
      IF (IRET.NE.0) GO TO 970
      UP = .TRUE.
      DO 270 I = 1,MP
         IF ((MRING(I).GT.YMIN) .AND. (MRING(I).LT.YMAX)) THEN
            X = I * XSCALE
            Y = (MRING(I)-YMIN) * YSCALE
            IF (UP) THEN
               CALL GPOS (X, Y, PLBUFF, IRET)
            ELSE
               CALL GVEC (X, Y, PLBUFF, IRET)
               END IF
            IF (IRET.NE.0) GO TO 970
            UP = .FALSE.
         ELSE
            UP = .TRUE.
            END IF
 270     CONTINUE
C                                       Done: finish plot
      GPHPAG = .FALSE.
      CALL GFINIS (PLBUFF, IRET)
      IF (IRET.GT.0) GO TO 975
      IF (.NOT.DOTV) THEN
         CALL HIPLOT (DISKOU, CNOUT(1), VER, BUFFER, IRET)
         IRET = 0
         END IF
      GO TO 999
C                                       ZPHFIL or GINIT failure.
 960  WRITE (MSGTXT,2000)
      CALL MSGWRT (8)
C
      IF (.NOT.DOTV) THEN
         CALL DELEXT ('PL', DISKOU, CNOUT(1), 'WRIT', CATBLK, BUFFER,
     *      VER, IERR)
         END IF
      GO TO 999
C                                       Try to finish partial graph
 970  MSGTXT = 'PLOT ERROR OCCURRED: TRY TO FINISH PARTIAL PLOT'
      CALL MSGWRT (7)
      GPHPAG = .FALSE.
      CALL GFINIS (PLBUFF, IERR)
      IF (IERR.NE.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKOU, CNOUT(1), VER, BUFFER, IERR)
            IERR = 0
            END IF
         GO TO 999
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKOU, PFILE, IERR)
         CALL DELEXT ('PL', DISKOU, CNOUT(1), 'WRIT', CATBLK, BUFFER,
     *      VER, IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('Plot file version',I4,'  created.')
 1050 FORMAT ('Antenna',I3,' Freq =',F7.3,' GHz Pol = ',A2,
     *   ' RefAnts=',11I3)
 1051 FORMAT ('Antenna',I3,' Freq =',F7.3,' GHz Pol = ',A2)
 1052 FORMAT ('RefAnt =',24I3)
 1053 FORMAT ('Antennas =',23I3)
 1054 FORMAT ('Freq =',F7.3,' GHz Pol = ',A2,' RefAnts=',15I3)
 1055 FORMAT ('Frequency =',F7.3,' GHz Pol = ',A2)
 1100 FORMAT ('Plot file version',I4,'__created ',A, A)
 2000 FORMAT ('PLTEL: ERROR DURING GRAPH FILE CREATION')
      END
