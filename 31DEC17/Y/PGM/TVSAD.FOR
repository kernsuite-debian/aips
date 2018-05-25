LOCAL INCLUDE 'TVSADG.INC'
      INTEGER   MAXGAU, MAXP
      PARAMETER (MAXGAU=8, MAXP=6*MAXGAU)
LOCAL END
LOCAL INCLUDE 'TVSAD.INC'
C                                       Local include for TVSAD
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'TVSADG.INC'
      INTEGER   BOXSIZ, MAXFND
      PARAMETER (BOXSIZ=16384, MAXFND=40000)
C
      CHARACTER TITL1*132, TITL2*132, SCRTCH*132, INNA*36, OUTNA*36,
     *   SORT*1, LINE*132, INNAME*12, INCLAS*6, OUTNAM*12, OUTCLS*6,
     *   INTYPE*2, IN2NAM*12, IN2CLS*6, UNIT*8
      INTEGER   OUTBLK(256), LUN1, INSL, OUTSL, DEPTH(5), NGAUSS, NITER,
     *   XTYPE(MAXGAU), IVAR(MAXP), JVAR(MAXP), NX, NY, NPTS, PTMAX,
     *   PTMIN,
     *   NPARM, NVAR, ILINE, IPAGE, NACROS, LUN2, DOCC, DOCONV, IPTLEV,
     *   ISORT(MAXFND), PRTLUN, PRTIND, MFLUN, MFVER, MFBUF(512), INSEQ,
     *   OUTSEQ, INDISK, OUTDSK, SCRBUF(256), PLANE, IN2BLK(256), LUNR,
     *   IN2SEQ, IN2DSK, FREQAX, INBLK(256)
      REAL      WIN(4), G(6,MAXGAU), E(6,MAXGAU), CB(3), FDATA(BOXSIZ),
     *   DMAX, DMIN, OFFSET, CUTT, RCUT, OUTR(256), GLIST(6,MAXFND),
     *   RMS, RSCALE, PLIST(4,MAXFND), XCEN, YCEN, LNOISE, HCB(3),
     *   HCBP(3), UCB(3), FRMS
      REAL      XUSER, XINSEQ, XINDSK, XINVER, XBLC(7), XTRC(7), XOUSEQ,
     *   XOUDSK, XGAUSS, XCUT(10), XICUT, BWS, DOCRT, XDORES, XOUVER,
     *   XSTVER, XDOALL, XDOWID(12), GAIN, DPARM(10), X2SEQ, X2DISK,
     *   XPRLEV, PBPARM(7), FACTOR, SBPA, CBPA
      HOLLERITH XINNAM(3), XINCLS(2), XOUNAM(3), XOUCLS(2), XSORT,
     *   XOUPRT(12), X2NAM(3), X2CLS(2)
      HOLLERITH OUTH(256)
      LOGICAL   NOCLN, DOPNT, DOOUT, MULTI, DOPA, GESWID, BWCORR, DORMSI
      DOUBLE PRECISION RESID(BOXSIZ), OUTD(128), XRA, XDEC, XFREQ
      COMMON /INPARM/ XUSER, XINNAM, XINCLS, XINSEQ, XINDSK, XINVER,
     *   XBLC, XTRC, XDORES, XOUNAM, XOUCLS, XOUSEQ, XOUDSK, XGAUSS,
     *   XCUT, XICUT, BWS, XSORT, DOCRT, XOUPRT, XOUVER, XSTVER, XDOALL,
     *   XDOWID, GAIN, DPARM, X2NAM, X2CLS, X2SEQ, X2DISK, XPRLEV,
     *   PBPARM, FACTOR
      COMMON /IMFCHR/ INNA, OUTNA, SORT, INNAME, INCLAS, OUTNAM, OUTCLS,
     *   TITL1, TITL2, LINE, SCRTCH, INTYPE, IN2NAM, IN2CLS, UNIT
      COMMON /IMFIO/ XRA, XDEC, XFREQ, OUTBLK, IN2BLK, INBLK, RESID,
     *   LUN1, LUN2, INSL, OUTSL, DEPTH, G, E, CB, NGAUSS, NITER, XTYPE,
     *   IVAR, JVAR, NOCLN, DOCC, DOCONV, DOOUT, DOPA, DOPNT, MULTI,
     *   FDATA, DMAX,DMIN, OFFSET, NX, NY, NPTS, PTMAX, PTMIN, NPARM,
     *   CUTT, GLIST, ISORT, NVAR, MFLUN, MFVER, MFBUF, ILINE, IPAGE,
     *   NACROS, PRTLUN,PRTIND, INSEQ, OUTSEQ, INDISK, OUTDSK, RMS,
     *   PLIST, RCUT, PLANE,IPTLEV, RSCALE, GESWID, XCEN, YCEN, BWCORR,
     *   LUNR, DORMSI, LNOISE, IN2DSK, IN2SEQ, FREQAX, HCB, HCBP, UCB,
     *   WIN, SCRBUF, FRMS, SBPA, CBPA
      EQUIVALENCE (OUTR, OUTBLK, OUTH, OUTD)
LOCAL END
LOCAL INCLUDE 'TVSADS.INC'
      INTEGER   NRJ(9), NPKMAX, NFAIL, NOMIT, NCFAIL, NCMPLX, NCMBAD,
     *   NREJCT
      REAL      PKMAX(MAXFND), RNX, RNY
      COMMON /TVSAD2/ PKMAX, RNX, RNY, NRJ, NPKMAX, NFAIL, NOMIT,
     *   NCFAIL, NCMPLX, NCMBAD, NREJCT
LOCAL END
LOCAL INCLUDE 'ORFIT.INC'
      CHARACTER REFRA*14, REFDEC*14
      REAL      ORRMS, ORA, ORERRA, ORERRX, ORERRY, ORERMA, ORERMI,
     *   ORERFI, ORBMAJ, ORBMIN, ORBPA
      LOGICAL   OREXIS
      COMMON /VORFIT/ ORRMS, ORA, ORERRA, ORERRX, ORERRY, ORERMA,
     *   ORERMI, ORERFI, ORBMAJ, ORBMIN, ORBPA, OREXIS
      COMMON /CORFIT/ REFRA, REFDEC
LOCAL END
      PROGRAM TVSAD
C-----------------------------------------------------------------------
C! Fits Gaussians to an image with TV interaction
C# Map Modeling Interactive TV
C-----------------------------------------------------------------------
C;  Copyright (C) 2014-2017
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
C   TVSAD (Search and Destroy) is a two-dimensional Gaussian fitting
C   program for image modelling.  Up to four Gaussian components can be
C   fit to each little part of an image under interactive control.
C   Inputs:
C      INNAME(3)            Image name (name)
C      INCLASS(2)           Image name (class)
C      INSEQ                Image name (seq. #)
C      INDISK               Disk unit # of image
C      BLC(7)               Bottom left corner to model
C      TRC(7)               Top right corner to model
C      OUTNAME(3)           Image outname (name)
C      OUTCLASS(2)          Image outname (class)
C      OUTSEQ               Image outname (seq. #)
C      OUTDISK              Disk unit # of output
C      NGAUSS               Max. Number of Gaussians to fit
C      PCUT                 Min. Search Level
C      SORT                 Sort order of output listing
C      DOCRT                Where to print output
C      DORESID              Create a residual file
C      DOMODEL              Create a CC file with Input file
C      DOALL                Fit multiple peaks
C      DOIDTH               Fit extended rather than point sources
C-----------------------------------------------------------------------
      INCLUDE 'TVSAD.INC'
      INCLUDE 'TVSADS.INC'
      INCLUDE 'INCS:PMFC.INC'
      EXTERNAL  FXDVD
      CHARACTER PRGNAM*6
      INTEGER   IER, IERR, I, IRET, NPK, NGOOD, IRNO, IXT, LPK, LCUT,
     *   NNPK, NXI, NYI, NXR, NYR, IFIL, NWORDS
      REAL      RMSLIM, ROW(NUMCOL), IMAGE(2), RMSIM(2), BLC(7), TRC(7)
      LONGINT   PIMAGE, PRMSIM
      INCLUDE 'ORFIT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGNAM /'TVSAD'/
C-----------------------------------------------------------------------
C                                       Init
      CALL TVSADI (PRGNAM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'TVSADI: INITIALIZATIONS'
         GO TO 900
         END IF
C                                       RMS
      IF (LUNR.GT.0) THEN
         CALL FILNUM (LUNR, IFIL, IERR)
         NXR = FILTAB(POTRC,IFIL) - FILTAB(POBLC,IFIL) + 1
         NYR = FILTAB(POTRC+1,IFIL) - FILTAB(POBLC+1,IFIL) + 1
         NWORDS = (NXR * NYR) / 1024 + 4
         CALL ZMEMRY ('GET ', 'TVSAD', NWORDS, RMSIM, PRMSIM, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
            GO TO 900
            END IF
         CALL IMGIO ('READ', LUNR, NXR, NYR, RMSIM(1+PRMSIM), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'IMGIO READ ON RMS IMAGE'
            GO TO 900
            END IF
         END IF
C                                       images in RAM
      CALL FILNUM (LUN1, IFIL, IERR)
      NXI = FILTAB(POTRC,IFIL) - FILTAB(POBLC,IFIL) + 1
      NYI = FILTAB(POTRC+1,IFIL) - FILTAB(POBLC+1,IFIL) + 1
      NWORDS = (NXI * NYI) / 1024 + 4
      CALL ZMEMRY ('GET ', 'TVSAD', NWORDS, IMAGE, PIMAGE, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
         GO TO 900
         END IF
      CALL IMGIO ('READ', LUN1, NXI, NYI, IMAGE(1+PIMAGE), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'IMGIO READ ON INPUT IMAGE'
         GO TO 900
         END IF
      CALL OPENCF (LUN1, INNA, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'REOPENING INPUT IMAGE'
         GO TO 900
         END IF
C                                       Cutoff level for search
C                                       is determined either by
C                                       by user value or rms.
      RMSLIM = 3.0
      CUTT = XCUT(1)
      LCUT = 0
      LPK = 0
      NNPK = 0
      CALL TVSADG (NXI, NYI, IMAGE(1+PIMAGE), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'TVSADG'
         GO TO 900
      ELSE IF (CUTT.EQ.0) THEN
         WRITE (MSGTXT,1031) RMSLIM, RMS
         CALL MSGWRT (4)
         IF (DORMSI) THEN
            CUTT = RMSLIM
         ELSE
            CUTT = RMSLIM * RMS
            END IF
         XCUT(1) = CUTT
         XCUT(2) = 0.0
         END IF
C                                       Make into read window in outfile
      CALL RFILL (7, 1.0, BLC)
      CALL RFILL (7, 1.0, TRC)
      CALL RCOPY (2, XBLC, BLC)
      CALL RCOPY (2, XTRC, TRC)
      CALL MAPWIN (LUN2, BLC, TRC, IERR)
C                                       Reset input window
      CALL RCOPY (7, XBLC, BLC)
      CALL RCOPY (7, XTRC, TRC)
      CALL MAPWIN (LUN1, BLC, TRC, IERR)
      DO 10 I = 1,7
         XBLC(I) = FILTAB(POBLC+I-1,IFIL)
         XTRC(I) = FILTAB(POTRC+I-1,IFIL)
 10      CONTINUE
      CALL RCOPY (5, XBLC(3), XTRC(3))
      CALL RCOPY (7, XBLC, BLC)
      CALL RCOPY (7, XTRC, TRC)
C                                       Do the real work down a layer
      CALL TVSADD (NXI, NYI, IMAGE(1+PIMAGE), NXR, RMSIM(1+PRMSIM),
     *   NNPK, NPK, IRET)
C                                       Done with passes
      IXT = NNPK - NFAIL
C                                       Tell user how he fared.
      IF (NFAIL.GT.0) THEN
         WRITE (MSGTXT,1100) IXT, NFAIL
      ELSE
         WRITE (MSGTXT,1101) IXT
         END IF
      CALL MSGWRT (4)
      IF (NOMIT.GT.0) THEN
         WRITE (MSGTXT,1102) NOMIT
         CALL MSGWRT (4)
         END IF
      IF (NCMPLX.GT.0) THEN
         WRITE (MSGTXT,1103) NCMPLX
         CALL MSGWRT (4)
         END IF
      IF (NCMBAD.GT.0) THEN
         WRITE (MSGTXT,1104) NCMBAD
         CALL MSGWRT (4)
         END IF
      IF (NCFAIL.GT.0) THEN
         WRITE (MSGTXT,1105) NCFAIL
         CALL MSGWRT (4)
         END IF
      IF (NREJCT.GT.0) THEN
         WRITE (MSGTXT,1106) NREJCT
         CALL MSGWRT (4)
         WRITE (MSGTXT,1107) NRJ(1), 'minimum peak'
         IF (NRJ(1).GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1107) NRJ(2), 'minimum flux'
         IF (NRJ(2).GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1107) NRJ(3), 'maximum rms'
         IF (NRJ(3).GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1107) NRJ(4), 'maximum size'
         IF (NRJ(4).GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1107) NRJ(5), 'maximum interior X excursion'
         IF (NRJ(5).GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1107) NRJ(6), 'maximum interior Y excursion'
         IF (NRJ(6).GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1107) NRJ(7), 'maximum exterior X excursion'
         IF (NRJ(7).GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1107) NRJ(8), 'maximum exterior Y excursion'
         IF (NRJ(8).GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1107) NRJ(9), 'maximum residual flux'
         IF (NRJ(9).GT.0) CALL MSGWRT (4)
         END IF
      WRITE (MSGTXT,1108) NPK
      CALL MSGWRT (4)
      NGAUSS = 1
C                                       Go back through for residuals
      CALL TVSADR (NXI, IMAGE(1+PIMAGE), NXR, RMSIM(1+PRMSIM), NPK,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1200) IERR
         GO TO 900
         END IF
C                                       output to screen & CC files
      CALL SADOUT (NXR, RMSIM(1+PRMSIM), NPK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1300) IERR
         GO TO 900
         END IF
      CALL FILCLS (LUN1)
C                                       Copy history files, keep output
      IRNO = 1
      CALL TABIO ('CLOS', 0, IRNO, ROW, MFBUF, IERR)
      IF (DOOUT) THEN
         CALL SADHI (NPKMAX, NPK, NGOOD, IERR)
C                                       Destroy output
      ELSE
         CALL FILCLS (LUN2)
         CALL FILDES (OUTNA, .FALSE., 'MA', 0, IERR)
         MSGTXT = 'Destroyed residual image as requested'
         CALL MSGWRT (3)
         END IF
C                                       Finished, no errors
      IRET = 0
      GO TO 980
C                                       Error return
 900  IER = 1
      CALL MSGWRT (8)
C                                       close line printer
 980  IF (PRTIND.GT.0) CALL LPCLOS (PRTLUN, PRTIND, ILINE, IERR)
C                                       Normal ending
      CALL TSKEND (IRET)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I6,' ON ',A)
 1031 FORMAT ('Setting Search limit at ',F3.1,' x RMS of ',F9.7)
 1100 FORMAT ('Sucessfully solved',I5,' islands.',I4,
     *   ' OTHERS FAILED TO CONVERGE')
 1101 FORMAT ('Successfully solved ',I6,' islands, all converged')
 1102 FORMAT (I6,' islands broken into multiple sources')
 1103 FORMAT (I6,' of these were complex rather than multi-peaked')
 1104 FORMAT (I6,' of these were rejected due to at least partial',
     *   ' failure')
 1105 FORMAT (I6,' apparently complex islands failed to break into 2')
 1106 FORMAT (I6,' total components had their fits rejected')
 1107 FORMAT (I6,' components rejected for ',A)
 1108 FORMAT ('Resulting number of sources fit is',I6)
 1200 FORMAT ('ERROR ',I6,' FINDING RESIDUALS')
 1300 FORMAT ('ERROR ',I6,' WRITING OUTPUT')
      END
      SUBROUTINE TVSADI (PRGNAM, IRET)
C-----------------------------------------------------------------------
C   TVSADI initializes things for TVSAD
C   Inputs:
C      PRGNAM   C*6   Task name
C   Outputs:
C      IRET     I     Error?
C   Lots more via commons
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*(*)
      INTEGER   IRET
C
      INCLUDE 'TVSAD.INC'
      INCLUDE 'TVSADS.INC'
      INCLUDE 'ORFIT.INC'
      INTEGER   IPK, LUNCG, CGBUFF(512), CGKOLS(4), CGNUMV(4), NRNO,
     *   ITRIM, FLEN, JERR, IERR, IROUND, J, IRNO, I
      REAL      BMAJ, BMIN, BPA, BLC(7), TRC(7)
      CHARACTER FILSPC*256, LPNAME*48
      DOUBLE PRECISION X, XMIN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
      IRET  = 8
      IERR  = 0
      PRTIND = -1
      IPK = 6 * MAXFND
      CALL RFILL (IPK, 0.0, GLIST)
      IPK = 4 * MAXFND
      CALL RFILL (IPK, 0.0, PLIST)
      CALL RFILL (MAXFND, 0.0, PKMAX)
      CALL FILL (9, 0, NRJ)
      LNOISE = 1.0
C                                       Initialize POPS
C                                       Set WaWa IO tables
      TSKNAM = PRGNAM
      CALL IOSET
C                                       Get task parms from AIPS
      NPARM = 99
      CALL GTPARM (PRGNAM, NPARM, RQUICK, XINNAM, SCRBUF, IERR)
      IF ((IERR.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         WRITE (MSGTXT,1000) IERR, 'GTPARM: GET PARMS'
         GO TO 900
         END IF
C                                       Resume AIPS if requested
      RQUICK = (RQUICK) .AND. (DOCRT.LE.0.0)
      IRET = 0
      IF ((RQUICK) .AND. (IERR.NE.1)) CALL RELPOP (IRET, SCRBUF, IERR)
      IRET = 8
      XUSER = NLUSER
C                                       Pass RQUICK via integer
      QUACK = 0
      IF (RQUICK) QUACK = 1
C                                       Get inputs, open map
      DOOUT = XDORES.GT.0.0
      DOPNT = XDOWID(1).LE.0.0
      GESWID = XDOWID(1).GT.1.00001
      MULTI = XDOALL.GT.0.0
      DOCC = IROUND (XOUVER)
      CALL H2CHR (1, 1, XSORT, SORT)
      IPTLEV = IROUND (XPRLEV)
      BWCORR = BWS.GT.0.0
      BWS = ABS (BWS)
C                                       Open the printer device
      IF (DOCRT.NE.0.0) THEN
         IPAGE = 0
         ILINE = 899
         CALL H2CHR (48, 1, XOUPRT, LPNAME)
         IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
         IF ((DOCRT.LT.-3.5) .AND. (LPNAME.NE.' ')) THEN
            CALL ZFULLN (LPNAME, ' ', ' ', FILSPC, JERR)
            IF (JERR.EQ.0) THEN
               FLEN = ITRIM (FILSPC)
               INQUIRE (FILE=FILSPC, EXIST=OREXIS)
               END IF
            END IF
         CALL LPOPEN (LPNAME, DOCRT, PRTLUN, PRTIND, NACROS, SCRBUF,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'LPOPEN'
            CALL MSGWRT (7)
            DOCRT = 0.0
            END IF
         END IF
C
C                                       Window
      LUN1 = 16
      LUN2 = 17
      LUNR = 20
      CALL SADOPN (BLC, TRC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'SADOPN'
         GO TO 900
         END IF
C                                       printer title
      WRITE (TITL1,1025) INNA(:12), INNA(13:18), INNA(21:27),
     *   INNA(28:29), PLANE, INNA(30:36)
      GAIN = MAX (0.0, MIN (1.0, GAIN))
      IF (DPARM(3).LE.0.0) DPARM(3) = 10000.
      IF (DPARM(4).LE.0.0) DPARM(4) = 10000.
      DPARM(10) = MAX (-3.0, MIN (3.0, DPARM(10)))
C                               How may sources to look for?
C                               Print it out along with subfield
      NPKMAX = XGAUSS + .01
      IF (NPKMAX.LE.0) NPKMAX = 10
      IF (NPKMAX.GT.MAXFND) NPKMAX = MAXFND
      WRITE (MSGTXT,1034) NPKMAX
      CALL MSGWRT (4)
      WRITE (MSGTXT,1035) 'BLC: ', BLC
      CALL MSGWRT (4)
      WRITE (MSGTXT,1035) 'TRC: ', TRC
      CALL MSGWRT (4)
C                                       Clean image?
      RNX = CATBLK(KINAX)
      RNY = CATBLK(KINAX+1)
      HCB(1) = CATR(KRBMJ)
      HCB(2) = CATR(KRBMN)
      HCB(3) = CATR(KRBPA)
      NOCLN = (HCB(1).GT.0.0) .AND. (HCB(2).GT.0.0) .AND. (DOPA)
      NOCLN = .NOT.NOCLN
      IF ((DOPNT) .AND. (NOCLN)) THEN
         MSGTXT = 'Can''t fit point sources if I don''t know CLEAN BEAM'
         IERR = 10
         GO TO 900
         END IF
C                                       find frequency
      CB(1) = -1.0
      XFREQ = 0.0D0
      CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATBLK(KHCTP), FREQAX,
     *   IERR)
      IF (IERR.EQ.0) XFREQ = CATD(KDCRV+FREQAX) + CATR(KRCIC+FREQAX) *
     *   (DEPTH(FREQAX-1) - CATR(KRCRP+FREQAX))
      IF (.NOT.NOCLN) THEN
C                                       header beam parameters
         HCBP(3) = CATR(KRBPA) - ROT(LOCNUM)
         CBPA  = COS (DG2RAD * HCBP(3))
         SBPA  = SIN (DG2RAD * HCBP(3))
C                                       Change from degrees to pixels
         HCBP(1) = HCB(1) * SQRT ((SBPA/AXINC(1,LOCNUM))**2 +
     *      (CBPA/AXINC(2,LOCNUM))**2)
         HCBP(2) = HCB(2) * SQRT ((CBPA/AXINC(1,LOCNUM))**2 +
     *      (SBPA/AXINC(2,LOCNUM))**2)
         HCBP(3) = RAD2DG * ATAN2 (SBPA/ABS(AXINC(1,LOCNUM)),
     *      CBPA/ABS(AXINC(2,LOCNUM))) + 90.0
         IF (HCBP(1).LT.HCBP(2)) HCBP(3) = 90. + HCBP(3)
         IF (HCBP(3).GT.90.) HCBP(3) = HCBP(3) - 180.
         IF (HCBP(3).LT.-90.) HCBP(3) = HCBP(3) + 180.
C                                       Beam parameters for this plane
         J = 0
         IF (XFREQ.GT.0) CALL FNDEXT ('CG', INBLK, J)
         IF (J.GT.0) THEN
            LUNCG = 57
            CALL CGINI ('READ', CGBUFF, INDISK, INSL, J, INBLK, LUNCG,
     *         IRNO, CGKOLS, CGNUMV, I, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPENING CG TABLE'
               CALL MSGWRT (7)
               GO TO 11
               END IF
            XMIN = 1.D16
            NRNO = CGBUFF(5)
            DO 10 I = 1,NRNO
               IRNO = I
               CALL TABCG ('READ', CGBUFF, IRNO, CGKOLS, CGNUMV, X,
     *            BMAJ, BMIN, BPA, IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READING CG TABLE'
                  GO TO 900
               ELSE IF (IERR.EQ.0) THEN
                  IF (ABS(X-XFREQ).LT.XMIN) THEN
                     XMIN = ABS (X - XFREQ)
                     UCB(1) = BMAJ
                     UCB(2) = BMIN
                     UCB(3) = BPA
                     END IF
                  END IF
 10            CONTINUE
            CALL TABCG ('CLOS', CGBUFF, IRNO, CGKOLS, CGNUMV, X, BMAJ,
     *         BMIN, BPA, IERR)
            UCB(3) = UCB(3) - ROT(LOCNUM)
            CBPA  = COS (DG2RAD * UCB(3))
            SBPA  = SIN (DG2RAD * UCB(3))
C                                       Change from degrees to pixels
            CB(1) = UCB(1) * SQRT ((SBPA/AXINC(1,LOCNUM))**2 +
     *         (CBPA/AXINC(2,LOCNUM))**2)
            CB(2) = UCB(2) * SQRT ((CBPA/AXINC(1,LOCNUM))**2 +
     *         (SBPA/AXINC(2,LOCNUM))**2)
            CB(3) = RAD2DG * ATAN2 (SBPA/ABS(AXINC(1,LOCNUM)),
     *         CBPA/ABS(AXINC(2,LOCNUM))) + 90.0
            IF (CB(1).LT.CB(2)) CB(3) = 90. + CB(3)
            IF (CB(3).GT.90.) CB(3) = CB(3) - 180.
            IF (CB(3).LT.-90.) CB(3) = CB(3) + 180.
            BMAJ = UCB(1) * 3600.
            BMIN = UCB(2) * 3600.
            BPA = UCB(3) + ROT(LOCNUM)
            WRITE (MSGTXT,1010) BMAJ, BMIN, BPA, 'CG table'
            CALL MSGWRT (3)
            END IF
C                                       If not cleaned use 3 pixels
      ELSE
         MSGTXT = 'WARNING: using 3 pixels as smoothing width for'
     *      // ' error estimates'
         CALL MSGWRT (6)
         CB(1) = 3.
         CB(2) = 3.
         CB(3) = 0.
         CBPA  = 1.
         SBPA  = 0.
         END IF
C                                       use header beam
 11   IF (CB(1).LT.0.0) THEN
         CALL RCOPY (3, HCB, UCB)
         CALL RCOPY (3, HCBP, CB)
         BMAJ = UCB(1) * 3600.
         BMIN = UCB(2) * 3600.
         BPA = UCB(3)
         WRITE (MSGTXT,1010) BMAJ, BMIN, BPA, 'image header'
         CALL MSGWRT (3)
         END IF
      IRET = 0
      GO TO 999
C
 900  CALL MSGWRT (8)
      IRET = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I6,' ON ',A)
 1010 FORMAT ('Using Clean beam',2F10.5,F8.2,' from ',A)
 1025 FORMAT (A12,'.',A6,'.',A7,5X,'Disk ',A2,5X,'Plane',I5,5X,'User ',
     *   A7)
 1034 FORMAT ('Looking for up to',I5,' sources in subfield:')
 1035 FORMAT (A,7F8.0)
      END
      SUBROUTINE IMGIO (OP, LUN, NX, NY, IMAGE, IRET)
C-----------------------------------------------------------------------
C   GETIMG reads/writes an image into/from ram and then closes the file
C   which flushes the buffers on write.
C   Inputs:
C      OP      C*4    Read or WRIT
C      LUN     I      Open LUN number
C      IFIL    I      WaWa IO pointer
C      NNX     I      Number X pixels
C      NNY     I      Number Y pixels
C   Outputs
C      IMAGE   R(*)   Image
C      IRET    I      Error code
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   LUN, NX, NY, IRET
      REAL      IMAGE(NX,*)
C
      INTEGER   IY
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      DO 10 IY = 1,NY
         CALL MAPIO (OP, LUN, IMAGE(1,IY), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) OP, IRET, IY
            CALL MSGWRT (8)
            GO TO 999
            END IF
 10      CONTINUE
      CALL FILCLS (LUN)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMGIO ',A,' MAPIO ERROR',I3,' ROW',I6)
      END
      SUBROUTINE TVSADG (NNX, NNY, IMAGE, IRET)
C-----------------------------------------------------------------------
C   TVSADG finds the RMS information, creates and initializes the output
C   file, and then subtracts any pre-existing components in the MF table
C   Inputs:
C      NNX     I      Number X pixels in IMAGE
C      NNY     I      Number Y pixels in IMAGE
C   In/out:
C      IMAGE   R(*)   Input image (in), residual image (out)
C   Outputs:
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   NNX, NNY, IRET
      REAL      IMAGE(NNX,*)
C
      EXTERNAL  FXDVD
      INCLUDE 'TVSAD.INC'
      CHARACTER KEYWRD*8
      INTEGER   NPT, IX, IY, I, J, IC, HIST(1025), NP, INF, NPR, LOCS,
     *   NUMKEY, KEYTYP, MSGSAV, INNY, NMFA, NMF, NYL, NYH, IFIL, IMF,
     *   NK
      REAL      SCALE, TEMP, RPRT, VALUE, SAVRMS, A, B, DELTAX, DELTAY,
     *   DELXY, SINE, COSINE, SUM, SUM2
      HOLLERITH HOLMA
      DOUBLE PRECISION VALVAR(MAXP), ERRDVD(3), EPS, FOPT, GNOPT,
     *   DUMMY(MAXP)
      INCLUDE 'INCS:PMFC.INC'
      REAL      ROW(NUMCOL)
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       **** get RMS section:
C                                       is it in image header
      NUMKEY = 1
      KEYWRD = 'ACTNOISE'
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL CATKEY ('READ', INDISK, INSL, KEYWRD, NUMKEY, LOCS, VALUE,
     *   KEYTYP, SCRBUF, IRET)
      MSGSUP = MSGSAV
      IF ((IRET.EQ.0) .AND. (VALUE.GT.0.0)) THEN
         RMS = VALUE
         WRITE (MSGTXT,1000) RMS
         CALL MSGWRT (3)
C                                       get robust RMS
      ELSE
         NP = 256
         CALL RMSFIT (NNX, NNY, IMAGE, NP, SCALE, HIST, NPT, SAVRMS,
     *      IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'TVSADG: RMSFIT FAILS'
            GO TO 900
            END IF
         IC = NP + 1
C                                       compute input rms
         IF (NPT.LE.0) THEN
            MSGTXT = 'TVSADG: (GETRMS) FOUND NO USABLE POINTS - ' //
     *         'CAN''T GET RMS'
            IRET = 8
C                                       fit histogram for rms
         ELSE
            IX = IC
            NPT = HIST(IC)
C                                       locate peak
            DO 10 I = 1,NP
               IF (HIST(IC-I).GT.NPT) THEN
                  IX = IC - I
                  NPT = HIST(IX)
                  END IF
               IF (HIST(IC+I).GT.NPT) THEN
                  IX = IC + I
                  NPT = HIST(IX)
                  END IF
 10            CONTINUE
C                                       is this a nutty histogram?
            IF ((HIST(IX-1).LT.NPT*0.6) .AND. (HIST(IX+1).LT.NPT*0.6))
     *         THEN
               NPT = (HIST(IX-1) + HIST(IX+1)) / 1.98
               HIST(IX) = NPT
               MSGTXT = 'Histogram adjusted for central spike'
               CALL MSGWRT (6)
               END IF
C                                       find half power
            J = NPT / 2
            IX = 0
            IY = 0
            DO 20 I = 1,NP
               IF (HIST(IC-I).GE.J) IY = I
               IF (HIST(IC+I).GE.J) IX = I
               FDATA(IC-I) = HIST(IC-I)
               FDATA(IC+I) = HIST(IC+I)
 20            CONTINUE
C                                       prepare data for fitting routine
            WIN(1) = 1
            WIN(2) = 1
            NY = 1
            NX = 2 * NP + 1
            FDATA(IC) = HIST(IC)
            G(1,1) = NPT
            G(2,1) = IC
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
            RPRT = -1.0
            NITER = 40
            EPS = 1.D-10
            CALL DVDMIN (FXDVD, VALVAR, ERRDVD, NVAR, EPS, NITER, FOPT,
     *         GNOPT, INF, NPR, RPRT)
C                                       save parms in GLIST
            IF (INF.NE.0) THEN
               MSGTXT = 'TVSADG (GETRMS): FIT FAILS!'
               IF (SAVRMS.LE.0.0) THEN
                  IRET = 7
               ELSE
                  RMS = SAVRMS
                  WRITE (MSGTXT,1020) RMS
                  CALL MSGWRT (3)
                  END IF
            ELSE
               RMS = G(4,1) / SQRT (8.0 * LOG(2.0)) / SCALE
               WRITE (MSGTXT,1021) RMS
               CALL MSGWRT (3)
               IRET = 0
C                                       should be at 0
               IF (ABS(G(2,1)-IC).GT.1.0) THEN
                  TEMP = (G(2,1) - IC) / SCALE
                  WRITE (MSGTXT,1022) TEMP
                  CALL MSGWRT (6)
                  END IF
               END IF
            END IF
         END IF
      IF (IRET.NE.0) GO TO 900
C                                       **** MAKO portion
C                                       get real values of win from
C                                       filtab (full plane)
      CALL FILNUM (LUN1, IFIL, IRET)
      WIN(1) = FILTAB(POBLC,   IFIL)
      WIN(2) = FILTAB(POBLC+1, IFIL)
      WIN(3) = FILTAB(POTRC,   IFIL)
      WIN(4) = FILTAB(POTRC+1, IFIL)
      NX = WIN(3) - WIN(1) + 1
      NY = WIN(4) - WIN(2) + 1
C                                   Create Output name and file
      CALL COPY (256, CATBLK, OUTBLK)
      CALL CHR2H (4, 'MA  ', 1, HOLMA)
      IF (.NOT.DOOUT) XOUSEQ = 0.0
      CALL H2WAWA (XOUNAM, XOUCLS, XOUSEQ, HOLMA, XOUDSK, XUSER, OUTNA)
      IF (OUTNA(1:1).EQ.' ') OUTNA(1:12) = INNA(1:12)
      IF (OUTNA(13:13).EQ.' ') OUTNA(13:18) = 'RESID '
C
C                                       modify output file header
      OUTBLK(KINAX)   = NX
      OUTBLK(KINAX+1) = NY
      CALL FILL (5, 1, OUTBLK(KINAX+2))
      OUTR(KRCRP)   = CATR(KRCRP)   - WIN(1) + 1.0
      OUTR(KRCRP+1) = CATR(KRCRP+1) - WIN(2) + 1.0
      DO 110 I = 2,6
         OUTR(KRCRP+I) = CATR(KRCRP+I) - DEPTH(I-1) + 1.0
 110     CONTINUE
      CALL CATCLR (OUTBLK)
C                                       Create output map
      CALL MAPCR (INNA, OUTNA, OUTBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET, 'CREATE OUTPUT IMAGE'
         GO TO 900
         END IF
C                                       Open it
      CALL OPENCF (LUN2, OUTNA, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET, 'OPEN OUTPUT IMAGE'
         GO TO 900
         END IF
      CALL FILNUM (LUN2, IFIL, IRET)
      OUTSL = FILTAB (POCAT, IFIL)
      CALL WAWA2A (OUTNA, OUTNAM, OUTCLS, OUTSEQ, INTYPE, OUTDSK, IY)
C                                       copy keywords
      CALL FILKCP (LUN1, LUN2, IRET)
C                                       Copy full input plane to output
      NPT = 0
      SUM = 0.0
      SUM2 = 0.0
      FRMS = 0.0
      DO 150 IY = 1,NY
         CALL MAPIO ('WRIT', LUN2, IMAGE(1,IY), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1130) IRET, 'WRITING OUTPUT', IY
            GO TO 900
            END IF
         DO 140 IX = 1,NX
            IF ((IMAGE(IX,IY).NE.0.0) .AND. (IMAGE(IX,IY).NE.FBLANK))
     *         THEN
               SUM   = SUM + IMAGE(IX,IY)
               SUM2  = SUM2 + IMAGE(IX,IY)**2
               NPT   = NPT + 1
               END IF
 140        CONTINUE
 150     CONTINUE
      IF (NPT.GT.0) THEN
         SUM  = SUM / NPT
         SUM2 = SUM2 / NPT
         FRMS  = SUM2 - SUM**2
         IF (FRMS.GT.0.0) FRMS = SQRT (FRMS)
         END IF
C                                       flush buffers, open for read
      CALL FILCLS (LUN2)
      CALL OPENCF (LUN2, OUTNA, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET, 'REOPEN RESIDUAL IMAGE FILE'
         GO TO 900
         END IF
      CALL FILNUM (LUN2, IFIL, IRET)
C                                       **** SUBINI portion
C                                       are there any to do
      NMF = MFBUF(5)
      IF (NMF.LE.0) GO TO 999
      NMFA = 0
      NGAUSS = 1
C                                       yes
      INNY = NY
C                                       loop over components
      DO 290 IMF = 1,NMF
         CALL TABIO ('READ', 0, IMF, ROW, MFBUF, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1200) IRET, 'TABIO READ', IMF
            GO TO 900
            END IF
C                                       component pixel parameters
         IF (IRET.LT.0) GO TO 290
         IF (ABS(ROW(COLPLN)-PLANE).GT.0.1) GO TO 290
         NMFA = NMFA + 1
         G(1,1) = ROW(COLPEK)
         G(2,1) = ROW(COPCEX)
         G(3,1) = ROW(COPCEY)
         G(4,1) = ROW(COPMAJ)
         G(5,1) = ROW(COPMIN)
         G(6,1) = (ROW(COPPAN) + 90.0) * DG2RAD
         CALL SADVST (VALVAR)
C                                       How big an area do we need
         SINE = SIN (G(6,1))
         COSINE = COS (G(6,1))
         A = G(4,1)
         B = G(5,1)
         IF (A.EQ.0.0) A = 1.0
         IF (B.EQ.0.0) B = 1.0
         DELTAX = ((SINE/A)**2 + (COSINE/B)**2)
         DELTAY = ((COSINE/A)**2 + (SINE/B)**2)
         DELXY  = ((1./A)**2 - (1/B)**2)*(SINE*COSINE)**2
C                                       This pushes down the exp by
C                                       10**-4
         DELTAY = SQRT (3.3/(DELTAY - DELXY**2/DELTAX))
         DELTAY = DELTAY * 2.8
         NYL = G(3,1) - DELTAY
         NYH = G(3,1) + DELTAY + 0.9
         NYL = MAX (1, NYL)
         NYH = MIN (NYH, INNY)
C                                       lie for FXDVD
         NY = 1
         WIN(1) = 1
         DO 230 IY = NYL,NYH
            WIN(2) = IY
            IF ((IY.EQ.NYL) .AND. (NMFA.EQ.1)) CALL CATDIR ('CSTA',
     *         OUTDSK, OUTSL, OUTNAM, OUTCLS, OUTSEQ, INTYPE, NLUSER,
     *         'CLRD', SCRBUF, IRET)
            CALL RCOPY (NX, IMAGE(1,IY), FDATA)
            CALL FXDVD (VALVAR, DUMMY(1), DUMMY, 1)
            NK = 0
            DO 220 I = 1,NX
               IF (IMAGE(I,IY).NE.FBLANK) THEN
                  NK = NK + 1
                  IMAGE(I,IY) = -RESID(NK)
                  END IF
 220           CONTINUE
 230        CONTINUE
 290     CONTINUE
      IF (NMFA.GT.0) CALL CATDIR ('CSTA', OUTDSK, OUTSL, OUTNAM, OUTCLS,
     *   OUTSEQ, INTYPE, NLUSER, 'READ', SCRBUF, IRET)
C
 900  IF (IRET.NE.0) THEN
         CALL MSGWRT (8)
      ELSE
         WRITE (MSGTXT,1900) NMFA
         CALL MSGWRT (3)
         END IF
      NY = INNY
C
 999  RETURN
C----------------------------------------------------------------------
 1000 FORMAT ('True rms taken from ACTNOISE in header =',1PE10.3)
 1020 FORMAT ('Fit to histogram fails, using RMS',1PE10.3,
     *   ' from robust fit')
 1021 FORMAT ('Fit to histogram gives RMS =',1PE10.3)
 1022 FORMAT ('TVSADG (GETRMS): HISTOGRAM PEAK AT',F9.5,' NOT 0.0')
 1100 FORMAT ('TVSADG (MAKO): ERROR',I4,' ON ',A)
 1130 FORMAT ('TVSADG (MAKO): ERROR',I4,' ON ',A,' ROW',I5)
 1200 FORMAT ('TVSADG (SUBINI) ERROR',I4, ' ON ',A,' FOR COMPONENT',I5)
 1900 FORMAT ('TVSADG (SUBINI) subtracted',I5,
     *   ' model components to make initial residual')
      END
      SUBROUTINE TVSADD (NXI, NYI, IMAGE, NXR, RMSIM, NNPK, JPK, IRET)
C-----------------------------------------------------------------------
C   TVSADD does all the work
C   Inputs:
C      NXI     I      X dimension of IMAGE
C      NYI     I      Y dimension of IMAGE
C      NXR     I      X dimension of RMSimage
C      RMSIM   R(*)   Image of RMSes
C   In/out:
C      IMAGE   R(*)   Residual image
C   Output:
C      NNPK    I      Number islands attempted
C      JPK     I      Number components fit
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   NXI, NYI, NXR, NNPK, JPK, IRET
      REAL      IMAGE(NXI,*), RMSIM(NXR,*)
C
      EXTERNAL  FXDVD
      INCLUDE 'TVSAD.INC'
      INCLUDE 'TVSADS.INC'
      CHARACTER IPRTC*9
      LOGICAL   TVISON, DONE
      INTEGER   NPK, LCUT, LPK, PKWIN(4,MAXFND), IIPK, IPK, IXT, KPK, I,
     *   NPR, INF, ISOK(MAXGAU), ALLOK, NK, IPT, NGTEMP, K, J, ZAND,
     *   IBLC(2), ITRC(2), IROUND, NTOT, TVBUF(256)
      REAL      A, B, DELTAX, DELTAY, DELXY, LOCBLC(7), LOCTRC(7), TEMP,
     *   GINIT(6,MAXGAU), FMAX, RPRT, SUMSQ, RESMAX, RESMIN, RESSUM,
     *   TCUT, BLC(7), TRC(7)
      DOUBLE PRECISION VALVAR(MAXP), ERRDVD(MAXP), EPS, FOPT, GNOPT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      TVISON = .TRUE.
      DONE = .FALSE.
      CALL TVOPEN (TVBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET, 'OPENING TV DISPLAY'
         CALL MSGWRT (8)
         GO TO 999
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
      NFAIL = 0
      NOMIT = 0
      NCFAIL = 0
      NCMPLX = 0
      NCMBAD = 0
      NREJCT = 0
      JPK = 0
      LCUT = 0
      LPK = 0
      NNPK = 0
      IF (DORMSI) THEN
         UNIT = 'Sig/Nois'
      ELSE
         CALL H2CHR (8, 1, CATH(KHBUN), UNIT)
         END IF
C                                       loop over CUTT levels
 10   LCUT = LCUT + 1
      IF (LCUT.LE.10) CUTT = XCUT(LCUT)
      IF ((CUTT.LE.0.0) .OR. (LCUT.GT.10)) THEN
         CUTT = XCUT(LCUT-1)
         GO TO 900
         END IF
      WRITE (MSGTXT,1010) CUTT, UNIT, LCUT
      CALL MSGWRT (4)
      RCUT = XICUT
      IF (RCUT.LE.0.0) RCUT = CUTT
      LPK = NNPK + 1
C                               Look for continuous regions above CUTT
      CALL ISLAND (NXI, IMAGE, NXR, RMSIM, PKWIN(1,LPK), PKMAX(LPK),
     *   NPK, NPKMAX-LPK+1)
      IF (NPK.GT.0) THEN
         WRITE (MSGTXT,1015) NPK, LCUT
         CALL MSGWRT (4)
         NNPK = NNPK + NPK
      ELSE
         WRITE (MSGTXT,1016) LCUT
         CALL MSGWRT (7)
         GO TO 10
         END IF

C                                       Loop over potential sources
      CALL RFILL (7, 1.0, LOCBLC)
      CALL RFILL (7, 1.0, LOCTRC)
      DO 100 IIPK = LPK,NNPK
C                                       find highest
         FMAX = 0.0
         IPK = 0
         DO 20 KPK = LPK,NNPK
            IF ((PKWIN(1,KPK).GT.0) .AND. (PKMAX(KPK).GT.FMAX)) THEN
               FMAX = PKMAX(KPK)
               IPK = KPK
               END IF
 20         CONTINUE
         IF (IPK.LE.0) GO TO 100
C                                       set island window
         LOCBLC(1) = PKWIN(1,IPK)
         LOCBLC(2) = PKWIN(2,IPK)
         LOCTRC(1) = PKWIN(3,IPK)
         LOCTRC(2) = PKWIN(4,IPK)
         PKWIN(1,IPK) = -1
C                                       Set minimum and max sizes
         IF (LOCTRC(1) - LOCBLC(1).LT.4.*DELTAX) THEN
            IXT = (4.*DELTAX - LOCTRC(1) + LOCBLC(1)) / 2.0 + 0.5
            LOCTRC(1) = LOCTRC(1) + IXT
            LOCBLC(1) = LOCBLC(1) - IXT
            IF (LOCTRC(1).GT.RNX) THEN
               LOCBLC(1) = LOCBLC(1) - (LOCTRC(1) - RNX)
               LOCTRC(1) = RNX
            ELSE IF (LOCBLC(1).LT.1.) THEN
               LOCTRC(1) = LOCTRC(1) + (1. - LOCBLC(1))
               LOCBLC(1) = 1
               END IF
            END IF
         IF (LOCTRC(2) - LOCBLC(2).LT.4.*DELTAY) THEN
            IXT = (4.*DELTAY - LOCTRC(2) + LOCBLC(2)) / 2.0 + 0.5
            LOCTRC(2) = LOCTRC(2) + IXT
            LOCBLC(2) = LOCBLC(2) - IXT
            IF (LOCTRC(2).GT.RNY) THEN
               LOCBLC(2) = LOCBLC(2) - (LOCTRC(2) - RNY)
               LOCTRC(2) = RNY
            ELSE IF (LOCBLC(2).LT.1.) THEN
               LOCTRC(2) = LOCTRC(2) + (1. - LOCBLC(2))
               LOCBLC(2) = 1
               END IF
            END IF
         TEMP = BOXSIZ
         I = SQRT (TEMP)
         TEMP = I
         DO 30 I = 1,2
            IF (LOCTRC(I) - LOCBLC(I).GT.TEMP) THEN
               A = 0.5 * (LOCTRC(I) + LOCBLC(I))
               LOCTRC(I) = A + TEMP/2. - 1.
               LOCBLC(I) = A - TEMP/2. + 1.
               END IF
 30         CONTINUE
         LOCBLC(1) = LOCBLC(1) - DPARM(10)
         LOCBLC(2) = LOCBLC(2) - DPARM(10)
         LOCTRC(1) = LOCTRC(1) + DPARM(10)
         LOCTRC(2) = LOCTRC(2) + DPARM(10)
         LOCBLC(1) = MAX (1.0, LOCBLC(1))
         LOCBLC(2) = MAX (1.0, LOCBLC(2))
         LOCTRC(1) = MIN (RNX, LOCTRC(1))
         LOCTRC(2) = MIN (RNY, LOCTRC(2))
C                               Read in data in subwindow
         CALL SADDAT (NXI, IMAGE, NXR, RMSIM, LOCBLC, LOCTRC, FDATA,
     *      NTOT, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1030) IRET, 'FINDING DATA FOR ISLAND'
            GO TO 900
            END IF
C                                        Let TV decide
 35      CALL SADTV (NXI, NYI, IMAGE, NXR, RMSIM, LOCBLC, LOCTRC,
     *      TVISON, NGAUSS, G, RSCALE, FDATA, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET, 'TV OPERATIONS'
            GO TO 900
         ELSE IF (IRET.EQ.-2) THEN
            DONE = .TRUE.
            IRET = 0
            GO TO 110
         ELSE IF (IRET.LT.0) THEN
            GO TO 100
            END IF
C                                        Store variables properly
         CALL SADVST (VALVAR)
         DO 40 I = 1,NVAR
            ERRDVD(I) = 1.0D1
 40         CONTINUE
         EPS = 1.D-15
         NITER = MAX (40 * NGAUSS, 300)
C                                       Call fitting routine
         NPR = 0
         RPRT = -1.0
         CALL DVDMIN (FXDVD, VALVAR, ERRDVD, NVAR, EPS, NITER, FOPT,
     *      GNOPT, INF, NPR, RPRT)
         IF (INF.NE.0) THEN
            NFAIL = NFAIL + 1
C                                       Tell user this source didnt work
            IF (NFAIL.LE.25) THEN
               WRITE (MSGTXT,1095) IPK, LOCBLC(1), LOCBLC(2),
     *            LOCTRC(1), LOCTRC(2)
               CALL MSGWRT (4)
               END IF
            GO TO 100
            END IF
C                                       check results
C                                       Are all Okay?
         CALL COMPOK (LOCBLC, LOCTRC, RNX, RNY, 0.0, 0.0, NXR, RMSIM,
     *      ISOK, ALLOK)
C                                       Fit quality
         SUMSQ = 0.0
         RESMAX = 0.0
         RESMIN = 0.0
         RESSUM = 0.0
         NK = 0
         DO 50 IPT = 1,NPTS
            IF (FDATA(IPT).NE.FBLANK) THEN
               NK = NK + 1
               TEMP = -RESID(NK) / RSCALE
               RESSUM = RESSUM + TEMP
               SUMSQ = SUMSQ + TEMP**2
               RESMAX = MAX (RESMAX, TEMP)
               RESMIN = MIN (RESMIN, TEMP)
               END IF
 50         CONTINUE
         IF (NTOT.GT.0) SUMSQ = SQRT (SUMSQ /NTOT)
         IF (NOCLN) THEN
            RESSUM = RESSUM / NK
         ELSE
            RESSUM = RESSUM / (1.1331 * CB(1) * CB(2))
            END IF
C                                       We need to retry with N+1
         IF (DORMSI) CALL SADRMS (NXR, RMSIM, G(2,1), G(3,1), LNOISE)
         IF (LNOISE.EQ.FBLANK) THEN
            TCUT = 1.E10
         ELSE
            TCUT = SQRT ((RCUT*LNOISE)**2 + (GAIN*G(1,1)/RSCALE)**2)
            END IF
         IF ((ABS(RESMAX).GT.TCUT) .OR.(ABS(RESMIN).GT.TCUT) .OR.
     *      (ALLOK.GT.0)) THEN
            RESMAX = MAX (ABS(RESMAX), ABS(RESMIN))
            WRITE (MSGTXT,1050) RESMAX, TCUT
            IF (RESMAX.GT.TCUT) CALL MSGWRT (7)
            WRITE (MSGTXT,1051) (ISOK(I), I = 1,NGAUSS)
            IF (ALLOK.GT.0) CALL MSGWRT (7)
            IF (TVISON) THEN
               MSGTXT = '**** Retry this island with the TV ****'
            ELSE
               MSGTXT = '**** TV turned on to retry this island'
     *            // ' ****'
               END IF
            CALL MSGWRT (7)
            TVISON = .TRUE.
            GO TO 35
            END IF
C                                       check residual image
         IF (TVISON) THEN
            NGTEMP = NGAUSS
            DO 55 I = 1,NGTEMP
               G(1,I) = G(1,I) / RSCALE
 55            CONTINUE
            CALL SUBFIT (NXI, IMAGE, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'COMPUTING RESIDUAL IMAGE'
               GO TO 900
               END IF
            CALL SADTV2 (NXI, NYI, IMAGE, LOCBLC, LOCTRC, TVISON,
     *         NGAUSS, G, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'TV EXAM OF RESIDUAL IMAGE'
               GO TO 900
               END IF
            K = IRET
C                                       restore comps in all cases
            DO 60 I = 1,NGTEMP
               G(1,I) = -G(1,I)
60             CONTINUE
            CALL SUBFIT (NXI, IMAGE, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RESTORING RESIDUAL IMAGE'
               GO TO 900
               END IF
C                                       put G back
            DO 65 I = 1,NGTEMP
               G(1,I) = -G(1,I) * RSCALE
 65            CONTINUE
C                                       branch
            IF (K.EQ.-2) THEN
               DONE = .TRUE.
               GO TO 110
            ELSE IF (K.EQ.-1) THEN
               GO TO 100
            ELSE IF (K.EQ.-3) THEN
               CALL RCOPY (MAXP, G, GINIT)
C                               Read in data in subwindow
               CALL SADDAT (NXI, IMAGE, NXR, RMSIM, LOCBLC, LOCTRC,
     *            FDATA, NTOT, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1030) IRET, 'FINDING DATA FOR ISLAND'
                  GO TO 900
                  END IF
               CALL RCOPY (MAXP, GINIT, G)
               NGAUSS = NGTEMP
               GO TO 35
               END IF
            END IF
C                                       done with this island
         IF (NGAUSS.GT.1) THEN
            NOMIT = NOMIT + 1
            IF (ALLOK.GT.0) NCMBAD = NCMBAD + 1
            END IF
C                                       Copy estimates into GLIST
         NGTEMP = NGAUSS
         NGAUSS = 1
         DO 95 I = 1,NGTEMP
            G(1,I) = G(1,I) / RSCALE
            IF (ALLOK.EQ.0) THEN
               JPK = JPK + 1
               CALL RCOPY (6, G(1,I), GLIST(1,JPK))
               IF (I.GT.1) CALL RCOPY (6, G(1,I), G(1,1))
               PLIST(1,JPK) = SUMSQ
               PLIST(2,JPK) = RESMAX
               PLIST(3,JPK) = RESMIN
               PLIST(4,JPK) = RESSUM
               CALL SUBFIT (NXI, IMAGE, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1070) IRET, JPK
                  CALL MSGWRT (7)
                  END IF
            ELSE
               IF ((IPTLEV.GT.0) .AND. (NREJCT.EQ.0)) THEN
                  MSGTXT = 'Debug display of rejection for,' //
     *               ' in order,'
                  CALL MSGWRT (6)
                  MSGTXT ='   peak, flux, rms, width, Xpos, Ypos,' //
     *               ' Xout, Yout, residual flux'
                  CALL MSGWRT (6)
                  IPRTC = ' '
                  DO 80 K = 1,9
                     J = 2 ** (K-1)
                     IF (ZAND(IPTLEV,J).NE.0) IPRTC(K:K) = '1'
 80                  CONTINUE
                  MSGTXT = '   Options selected are ''' // IPRTC // ''''
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,1080)
                  CALL MSGWRT (6)
                  END IF
               NREJCT = NREJCT + 1
               IPRTC = ' '
               DO 85 K = 1,9
                  J = 2 ** (K-1)
                  IF (ZAND(ISOK(I),J).NE.0) THEN
                     NRJ(K) = NRJ(K) + 1
                     IF (ZAND(IPTLEV,J).NE.0) IPRTC(K:K) = '1'
                     END IF
 85               CONTINUE
               IF ((ZAND(IPTLEV,1).EQ.0) .AND.
     *            (ZAND(ISOK(I),1).NE.0)) IPRTC = ' '
               IF (IPRTC.NE.' ') THEN
                  IBLC(1) = IROUND (LOCBLC(1))
                  IBLC(2) = IROUND (LOCBLC(2))
                  ITRC(1) = IROUND (LOCTRC(1))
                  ITRC(2) = IROUND (LOCTRC(2))
                  RESSUM = G(1,I)
                  RESMAX = MAX (G(4,I), G(5,I))
                  WRITE (MSGTXT,1085) RESSUM, SUMSQ, RESMAX, IBLC,
     *               ITRC, IPRTC
                  CALL MSGWRT (6)
                  END IF
               END IF
 95         CONTINUE
 100     CONTINUE
C                                       save residual to disk
 110  CALL RFILL (7, 1.0, BLC)
      CALL RFILL (7, 0.0, TRC)
      CALL MAPWIN (LUN2, BLC, TRC, IRET)
      CALL IMGIO ('WRIT', LUN2, NXI, NYI, IMAGE, IRET)
C                                       reopen LUN2, loop
      IF (DONE) THEN
         MSGTXT = 'Stopping at user''s request'
         CALL MSGWRT (4)
      ELSE IF (IRET.EQ.0) THEN
         CALL OPENCF (LUN2, OUTNA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1101) IRET
            GO TO 900
            END IF
         GO TO 10
      ELSE
         WRITE (MSGTXT,1100) IRET
         END IF

 900  IF (IRET.GT.0) CALL MSGWRT (8)
      CALL TVCLOS (TVBUF, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVSADD ERROR',I4,' ON ',A)
 1010 FORMAT ('Searching down to ',F10.6,' (',A,')  pass',I2)
 1015 FORMAT ('Found ',I6,' Islands in pass',I2)
 1016 FORMAT ('Found no new islands in pass',I2)
 1030 FORMAT ('TVSADD: ERROR',I4,' ON ',A)
 1050 FORMAT ('FIT BAD: PEAK RESIDUAL',F10.6,' >',F10.6)
 1051 FORMAT ('FIT BAD: FLAGS',4I5)
 1070 FORMAT ('ERROR',I6,' SUBTRACTING COMPONENT',I5,
     *   ' FROM RESIDUAL IMAGE')
 1080 FORMAT ('   Flux',4X,'RMS',5X,'Width',6X,'BLC',10X,'TRC',6X,
     *   '''Reasons ''')
 1085 FORMAT (F10.5,F8.5,F6.1,' (',2I5,' )(',2I5,' ) ''',A,'''')
 1095 FORMAT ('No convergence for island',I4,' Box ',4F7.0)
 1100 FORMAT ('TVSADD ERROR',I5,' BACKING UP THE RESIDUAL IMAGE')
 1101 FORMAT ('TVSADD ERROR',I5,' REOPENING RESIDUAL IMAGE FILE')
      END
      SUBROUTINE SADTV (NXI, NYI, IMAGE, NXR, RMSIM, LOCBLC, LOCTRC,
     *   TVISON, NGAUSS, G, RSCALE, FDATA, IRET)
C-----------------------------------------------------------------------
C   SADTV does interactive model display, allowing user to enter
C   initial guesses for up to MAXGAU components within an "island" and
C   then have the model fit.
C   Inputs:
C      NXI      I      Number X pixels in image
C      NYI      I      Number Y pixels in image
C      IMAGE    R(*)   Current residual image
C   In/out:
C      LOCBLC   R(7)   BLC of island
C      LOCTRC   R(7)   TRC of island
C      TVISON   L      Use TV: input return immediately on false
C                         output: can be turned off
C      NGAUSS   I      Number of Gaussians to fit
C      G        R(*)   Initial guess
C      RSCALE   R      Scaling factor for FDATA
C      FDATA    R(*)   Image data in LOCBLC/LOCTRC
C   Output:
C      IRET     I      Codes of whatr happened
C                        > 0  => TV error -> die
C                          0  => fits with current guess
C                         -1  => skip this island, go on
C                         -2  => done enough, close down gently
C-----------------------------------------------------------------------
      INTEGER   NXI, NYI, NGAUSS, NXR, IRET
      REAL      IMAGE(NXI,*), LOCBLC(*), LOCTRC(*), G(6,*), RSCALE,
     *   FDATA(*), RMSIM(NXR,*)
      LOGICAL   TVISON
C
      INCLUDE 'TVSADG.INC'
      INTEGER   NOPTS, NOPT1, NOPT2
      PARAMETER (NOPT1=8+MAXGAU, NOPT2=5)
      PARAMETER (NOPTS=NOPT1+NOPT2)
      INCLUDE 'INCS:PTVC.INC'
C
      INTEGER   MTYPE, NCOL, NROWS(2), GRCHS(5), TOPSEP, SIDSEP, TIMLIM,
     *   NTITLE, TVBUTT, CHOICE, TVSCR(8192), IBLC(2), ITRC(2), NP(2),
     *   LP(2), INCS(2), I, IX, IY, NLEVS, LCHAN, JJ, LUTBUF(TVMLUT),
     *   L, II, ICOLOR, NG, IBUT, NX, NY, WIN(4), J, ITVC(4), IWIN(4),
     *   ICH, IBUFF(8192), JBUFF(8192), TBUFF(8192), NPIX, MPIX, LPIX,
     *   IYTV, IXC(33), IYC(33), INCR, NBO, MBOX
      LOGICAL   LEAVE(NOPTS)
      CHARACTER CHOICS(NOPTS)*12, TITLE*72, ISHELP*6, CDUM*1, FUNC*2,
     *   CHOIC1(NOPT1)*12, CHOIC2(NOPT2)*12
      REAL      RMIN, RMAX, SLOPE, OFFS, OFMBUF(TVMOFM), GMAX(MAXGAU),
     *   GPOS(2,MAXGAU), GWID(3,MAXGAU), ARRAY(16,16), DX(2), RPOS(2),
     *   RBLC(7), RTRC(7), X, Y, GTEMP(MAXP)
      EQUIVALENCE (LUTBUF, OFMBUF, TVSCR)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA CHOIC1 /'OFF TRANS', 'OFF TVZOOM', 'TVFIDDLE', 'CURVALUE',
     *   'ZOOM IN', 'ZOOM OUT', 'REBOX', 'ENTER GUESS', 'REDO GAUSS 1',
     *   'REDO GAUSS 2', 'REDO GAUSS 3', 'REDO GAUSS 4', 'REDO GAUSS 5',
     *   'REDO GAUSS 6', 'REDO GAUSS 7', 'REDO GAUSS 8'/
      DATA CHOIC2 /'DO FIT', 'TVOFF', 'NEXT ISLAND', ' ', 'QUIT'/
      DATA LEAVE /NOPTS*.FALSE./
      DATA ISHELP /'TVSAD'/
C-----------------------------------------------------------------------
C                                       skip out if TV is off
      IRET = 0
      ICOLOR = 0
      IF (.NOT.TVISON) GO TO 999
C                                       menu parameters
      GRCHS(1) = 1
      GRCHS(2) = 2
      GRCHS(3) = 4
      GRCHS(4) = 3
      GRCHS(5) = 5
      MTYPE = -1
      NCOL = 2
      NROWS(1) = NOPT1
      NROWS(2) = NOPT2
      SIDSEP = 20
      TOPSEP = 25
      NTITLE = 1
      TITLE = ' '
      TIMLIM = 0
C                                       window parameters
      NP(1) = NXI
      NP(2) = NYI
      CALL YWINDO ('READ', WINDTV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FIND TV WINDOW'
         GO TO 990
         END IF
 5    CALL RCOPY (7, LOCBLC, RBLC)
      CALL RCOPY (7, LOCTRC, RTRC)
      WIN(1) = RBLC(1) + 0.01
      WIN(2) = RBLC(2) + 0.01
      WIN(3) = RTRC(1) + 0.01
      WIN(4) = RTRC(2) + 0.01
      DO 10 I = 1,2
         L = LOCTRC(I) - LOCBLC(I) + 1.1
         L = MAX (125 - L, 0)
         IBLC(I) = LOCBLC(I) - L/2.0
         ITRC(I) = LOCTRC(I) + L/2.0 + 0.5
         IBLC(I) = MAX (1, IBLC(I))
         ITRC(I) = MIN (NP(I), ITRC(I))
         LP(I) = ITRC(I) - IBLC(I) + 1
         INCS(I) = (WINDTV(2+I) - WINDTV(I)) * 0.8
         INCS(I) = INCS(I) / LP(I)
         INCS(I) = MAX (1, MIN (16, INCS(I)))
         RBLC(I) = IBLC(I)
         RTRC(I) = ITRC(I)
 10      CONTINUE
      IF (INCS(2).LT.INCS(1)) INCS(1) = INCS(2)
      INCR = INCS(1)
      RMAX = -1.E10
      RMIN = -RMAX
      DO 20 IY = IBLC(2),ITRC(2)
         DO 15 IX = IBLC(1),ITRC(1)
            IF (IMAGE(IX,IY).NE.FBLANK) THEN
               RMAX = MAX (RMAX, IMAGE(IX,IY))
               RMIN = MIN (RMIN, IMAGE(IX,IY))
               END IF
 15         CONTINUE
 20      CONTINUE
      WRITE (TITLE,1005) WIN, RMIN, RMAX
C                                       init TV
      CALL YINIT (TVSCR, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT THE TV'
         GO TO 990
         END IF
      CALL YSLECT ('ONNN', 1, ICOLOR, TVSCR, IRET)
      DO 25 I = 1,5
         J = NGRAY + I
         CALL YSLECT ('ONNN', J, ICOLOR, TVSCR, IRET)
 25      CONTINUE
C                                       initial TV load
      GO TO 200
C                                       menu
 50   NROWS(1) = NOPT1 - MAXGAU + NGAUSS
      DO 51 I = 1,NROWS(1)
         CHOICS(I) = CHOIC1(I)
 51      CONTINUE
      DO 52 I = 1,NROWS(2)
         CHOICS(NROWS(1)+I) = CHOIC2(I)
 52      CONTINUE
      CALL TVMENU (MTYPE, NCOL, NROWS, GRCHS, TOPSEP, SIDSEP, ISHELP,
     *   CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHOICE, TVBUTT, TVSCR,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'MENU MANAGER'
         GO TO 990
         END IF
      IF (CHOICS(CHOICE).EQ.' ') THEN
         GO TO 50
      ELSE IF (CHOICS(CHOICE).EQ.'QUIT') THEN
         IRET = -2
         GO TO 999
      ELSE IF (CHOICS(CHOICE).EQ.'NEXT ISLAND') THEN
         IRET = -1
         GO TO 999
      ELSE IF (CHOICS(CHOICE).EQ.'TVOFF') THEN
         IRET = 0
         TVISON = .FALSE.
         GO TO 999
      ELSE IF (CHOICS(CHOICE).EQ.'DO FIT') THEN
         IRET = 0
         GO TO 999
      ELSE IF (CHOICS(CHOICE).EQ.'TVFIDDLE') THEN
         NLEVS = LUTOUT + 1
         LCHAN = 1
         CALL TVFIDL (LCHAN, NLEVS, TVSCR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'ENHANCING TV IMAGE'
            GO TO 990
            END IF
         GO TO 50
      ELSE IF (CHOICS(CHOICE).EQ.'CURVALUE') THEN
         IBUT = -1
         CALL TVALUE (GRCHS(2), NXI, NYI, IMAGE, IBUT, RPOS, OFMBUF,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'DOING CURSOR VALUE'
            GO TO 990
            END IF
      ELSE IF (CHOICS(CHOICE).EQ.'OFF TVZOOM') THEN
         TVZOOM(1) = 0
         TVZOOM(2) = MAXXTV(1) / 2
         TVZOOM(3) = MAXXTV(2) / 2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .FALSE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RESETTING TVZOOM'
            GO TO 990
            END IF
         GO TO 50
      ELSE IF (CHOICS(CHOICE).EQ.'OFF TRANS') THEN
C                                       off pseudo
         I = OFMINP + 1
         CALL RFILL (I, 0.0, OFMBUF)
         NLEVS = LUTOUT + 1
         IF (I.LT.NLEVS) NLEVS = I
         OFFS = 1.0 / REAL (OFMOUT)
         SLOPE = OFFS * (OFMOUT + 1.0) / REAL(NLEVS)
         DO 60 I = 1,NLEVS
            OFMBUF(I) = I * SLOPE - OFFS
 60         CONTINUE
         I = (OFMINP + 1) / NLEVS
         JJ = NLEVS
         DO 65 II = 2,I
            CALL RCOPY (NLEVS, OFMBUF, OFMBUF(JJ+1))
            JJ = JJ + NLEVS
 65         CONTINUE
         ICOLOR = 7
         CALL YOFM ('WRIT', ICOLOR, .FALSE., OFMBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RESET COLOR TABLES'
            GO TO 990
            END IF
C                                       off transf
         NLEVS = MAXINT + 1
         SLOPE = (LUTOUT+1.0) / (MAXINT+1.0)
         DO 70 I = 1,NLEVS
            LUTBUF(I) = I * SLOPE - 0.5
 70         CONTINUE
         I = 1
         CALL YLUT ('WRIT', I, ICOLOR, .FALSE., LUTBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RESET LUT (B&W) TABLES'
            GO TO 990
            END IF
         GO TO 50
      ELSE IF (CHOICS(CHOICE).EQ.'ENTER GUESS') THEN
         J = NGRAY + GRCHS(3)
         CALL YCINIT (J, LUTBUF)
         CALL YZERO (J, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'ZERO GAUSSIAN PLOT'
            GO TO 990
            END IF
C                                       init
         NG = 0
         CALL RFILL (MAXP, 0.0, G)
C                                       loop
 75      IF (NG.LT.MAXGAU) THEN
            NG = NG + 1
            WRITE (CDUM,1075) NG
 76         MSGTXT = '*** Position cursor at peak of component ' // CDUM
            CALL MSGWRT (1)
            MSGTXT = 'Then hit button A, B, or C; hit D to quit'
            CALL MSGWRT (1)
            IBUT = 0
            CALL TVALUE (GRCHS(2), NXI, NYI, IMAGE, IBUT, RPOS, OFMBUF,
     *         IRET)
C            CALL TVWHER (QUAD, RPOS, IBUT, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'FINDING PIXEL ON TV IMAGE'
               CALL MSGWRT (7)
               GO TO 76
               END IF
            IF (IBUT.LT.8) THEN
C               CALL IMA2MP (RPOS, RPOS)
               IF ((RPOS(1).LT.LOCBLC(1)) .OR. (RPOS(1).GT.LOCTRC(1))
     *            .OR. (RPOS(2).LT.LOCBLC(2)) .OR.
     *            (RPOS(2).GT.LOCTRC(2))) THEN
                  MSGTXT = 'POSITION OUTSIDE WINDOW'
                  CALL MSGWRT (7)
                  GO TO 76
                  END IF
               NX = 5
               NY = 5
               WIN(1) = RPOS(1) - 0.5 * NX + 1.
               WIN(1) = MAX (1, WIN(1))
               WIN(2) = RPOS(2) - 0.5 * NY + 1.
               WIN(2) = MAX (1, WIN(2))
               WIN(3) = WIN(1) + NX - 1
               WIN(3) = MIN (WIN(3), CATBLK(KINAX))
               WIN(4) = WIN(2) + NY - 1
               WIN(4) = MIN (WIN(4), CATBLK(KINAX+1))
               NX = WIN(3) - WIN(1) + 1
               NY = WIN(4) - WIN(2) + 1
               DO 80 IY = 1,NY
                  DO 79 IX = 1,NX
                     ARRAY(IX,IY) = IMAGE(IX+WIN(1)-1,IY+WIN(2)-1)
 79                  CONTINUE
 80               CONTINUE
               DX(1) = RPOS(1) + 1.0 - WIN(1)
               DX(2) = RPOS(2) + 1.0 - WIN(2)
C                                       find maximum in array
               CALL CUBINT (ARRAY, DX, NX, NY, GMAX(NG), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'FINDING PEAK'
                  CALL MSGWRT (7)
                  GO TO 76
                  END IF
               GPOS(1,NG) = DX(1) + WIN(1) - 1.0
               GPOS(2,NG) = DX(2) + WIN(2) - 1.0
               MSGTXT = 'Position cursor at major axis half-power of '
     *            // 'component ' // CDUM
               CALL MSGWRT (1)
               MSGTXT = 'Then hit button A, B, or C; hit D to quit'
               CALL MSGWRT (1)
               IBUT = 0
               CALL TVALUE (GRCHS(2), NXI, NYI, IMAGE, IBUT, RPOS,
     *            OFMBUF, IRET)
C               CALL TVWHER (QUAD, RPOS, IBUT, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'FINDING PIXEL ON TV IMAGE'
                  CALL MSGWRT (7)
                  GO TO 76
                  END IF
               IF (IBUT.LT.8) THEN
C                  CALL IMA2MP (RPOS, RPOS)
                  GWID(1,NG) = 2.0 * SQRT ((RPOS(1)-GPOS(1,NG))**2 +
     *               (RPOS(2)-GPOS(2,NG))**2)
                  IF (GWID(1,NG).GT.0.0) THEN
                     GWID(3,NG) = -RAD2DG * ATAN2 ((GPOS(1,NG)-RPOS(1)),
     *                  (GPOS(2,NG)-RPOS(2))) + 90.0
                     IF (GWID(3,NG).LT.-90.0) GWID(3,NG) = GWID(3,NG)
     *                  + 180.0
                     IF (GWID(3,NG).GT.90.0) GWID(3,NG) = GWID(3,NG) -
     *                  180.0
                     GWID(3,NG) = GWID(3,NG) * DG2RAD
                     END IF
                  MSGTXT = 'Position cursor at minor axis half-power of'
     *                  // ' component ' // CDUM
                  CALL MSGWRT (1)
                  MSGTXT = 'Then hit button A, B, or C; hit D to quit'
                  CALL MSGWRT (1)
                  IBUT = 0
                  CALL TVALUE (GRCHS(2), NXI, NYI, IMAGE, IBUT, RPOS,
     *               OFMBUF, IRET)
C                  CALL TVWHER (QUAD, RPOS, IBUT, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET,
     *                  'FINDING PIXEL ON TV IMAGE'
                     CALL MSGWRT (7)
                     GO TO 76
                     END IF
                  IF (IBUT.LT.8) THEN
C                     CALL IMA2MP (RPOS, RPOS)
                     GWID(2,NG) = 2.0 * SQRT ((RPOS(1)-GPOS(1,NG))**2 +
     *                  (RPOS(2)-GPOS(2,NG))**2)
                     CALL ELLIPS (GRCHS(5), NG, GPOS(1,NG), GWID(1,NG),
     *                  IBUFF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'DRAW TEMP ELLIPS'
                        GO TO 990
                        END IF
                     END IF
                  END IF
               END IF
            IF (IBUT.LE.7) GO TO 75
            END IF
         IF (IBUT.GT.7) NG = NG - 1
         IF (NG.GT.0) THEN
            NGAUSS = NG
            DO 85 I = 1,NG
               G(1,I) = GMAX(I) * RSCALE
               G(2,I) = GPOS(1,I)
               G(3,I) = GPOS(2,I)
               G(4,I) = GWID(1,I)
               G(5,I) = GWID(2,I)
               G(6,I) = GWID(3,I)
 85            CONTINUE
            END IF
      ELSE IF (CHOICS(CHOICE)(:11).EQ.'REDO GAUSS ') THEN
         IF (CHOICS(CHOICE)(12:12).EQ.'1') NG = 1
         IF (CHOICS(CHOICE)(12:12).EQ.'2') NG = 2
         IF (CHOICS(CHOICE)(12:12).EQ.'3') NG = 3
         IF (CHOICS(CHOICE)(12:12).EQ.'4') NG = 4
         IF (CHOICS(CHOICE)(12:12).EQ.'5') NG = 5
         IF (CHOICS(CHOICE)(12:12).EQ.'6') NG = 6
         IF (CHOICS(CHOICE)(12:12).EQ.'7') NG = 7
         IF (CHOICS(CHOICE)(12:12).EQ.'8') NG = 8
         WRITE (CDUM,1075) NG
 90      MSGTXT = '*** Position cursor at peak of component ' // CDUM
         CALL MSGWRT (1)
         MSGTXT = 'Then hit button A, B, or C; hit D to quit'
         CALL MSGWRT (1)
         IBUT = 0
         CALL TVALUE (GRCHS(2), NXI, NYI, IMAGE, IBUT, RPOS, OFMBUF,
     *      IRET)
C         CALL TVWHER (QUAD, RPOS, IBUT, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'FINDING PIXEL ON TV IMAGE'
            CALL MSGWRT (7)
            GO TO 90
            END IF
         IF (IBUT.LT.8) THEN
C            CALL IMA2MP (RPOS, RPOS)
            IF ((RPOS(1).LT.LOCBLC(1)) .OR. (RPOS(1).GT.LOCTRC(1))
     *         .OR. (RPOS(2).LT.LOCBLC(2)) .OR.
     *         (RPOS(2).GT.LOCTRC(2))) THEN
               MSGTXT = 'POSITION OUTSIDE WINDOW'
               CALL MSGWRT (7)
               GO TO 90
               END IF
            NX = 5
            NY = 5
            WIN(1) = RPOS(1) - 0.5 * NX + 1.
            WIN(1) = MAX (1, WIN(1))
            WIN(2) = RPOS(2) - 0.5 * NY + 1.
            WIN(2) = MAX (1, WIN(2))
            WIN(3) = WIN(1) + NX - 1
            WIN(3) = MIN (WIN(3), CATBLK(KINAX))
            WIN(4) = WIN(2) + NY - 1
            WIN(4) = MIN (WIN(4), CATBLK(KINAX+1))
            NX = WIN(3) - WIN(1) + 1
            NY = WIN(4) - WIN(2) + 1
            DO 95 IY = 1,NY
               DO 94 IX = 1,NX
                  ARRAY(IX,IY) = IMAGE(IX+WIN(1)-1,IY+WIN(2)-1)
 94               CONTINUE
 95            CONTINUE
            DX(1) = RPOS(1) + 1.0 - WIN(1)
            DX(2) = RPOS(2) + 1.0 - WIN(2)
C                                       find maximum in array
            CALL CUBINT (ARRAY, DX, NX, NY, GMAX(NG), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'FINDING PEAK'
               CALL MSGWRT (7)
               GO TO 90
               END IF
            GPOS(1,NG) = DX(1) + WIN(1) - 1.0
            GPOS(2,NG) = DX(2) + WIN(2) - 1.0
            MSGTXT = 'Position cursor at major axis half-power of '
     *         // 'component ' // CDUM
            CALL MSGWRT (1)
            MSGTXT = 'Then hit button A, B, or C; hit D to quit'
            CALL MSGWRT (1)
            IBUT = 0
            CALL TVALUE (GRCHS(2), NXI, NYI, IMAGE, IBUT, RPOS, OFMBUF,
     *         IRET)
C            CALL TVWHER (QUAD, RPOS, IBUT, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'FINDING PIXEL ON TV IMAGE'
               CALL MSGWRT (7)
               GO TO 90
               END IF
            IF (IBUT.LT.8) THEN
C               CALL IMA2MP (RPOS, RPOS)
               GWID(1,NG) = 2.0 * SQRT ((RPOS(1)-GPOS(1,NG))**2 +
     *            (RPOS(2)-GPOS(2,NG))**2)
               IF (GWID(1,NG).GT.0.0) THEN
                  GWID(3,NG) = -RAD2DG * ATAN2 ((GPOS(1,NG)-RPOS(1)),
     *               (GPOS(2,NG)-RPOS(2))) + 90.0
                  IF (GWID(3,NG).LT.-90.0) GWID(3,NG) = GWID(3,NG)
     *               + 180.0
                  IF (GWID(3,NG).GT.90.0) GWID(3,NG) = GWID(3,NG) -
     *               180.0
                  GWID(3,NG) = GWID(3,NG) * DG2RAD
                  END IF
               MSGTXT = 'Position cursor at minor axis half-power of'
     *               // ' component ' // CDUM
               CALL MSGWRT (1)
               MSGTXT = 'Then hit button A, B, or C; hit D to quit'
               CALL MSGWRT (1)
               IBUT = 0
               CALL TVALUE (GRCHS(2), NXI, NYI, IMAGE, IBUT, RPOS,
     *            OFMBUF, IRET)
C               CALL TVWHER (QUAD, RPOS, IBUT, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET,
     *               'FINDING PIXEL ON TV IMAGE'
                  CALL MSGWRT (7)
                  GO TO 90
                  END IF
               IF (IBUT.LT.8) THEN
C                  CALL IMA2MP (RPOS, RPOS)
                  GWID(2,NG) = 2.0 * SQRT ((RPOS(1)-GPOS(1,NG))**2 +
     *               (RPOS(2)-GPOS(2,NG))**2)
                  CALL ELLIPS (GRCHS(5), NG, GPOS(1,NG), GWID(1,NG),
     *               IBUFF, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'DRAW TEMP ELLIPS'
                     GO TO 990
                     END IF
                  END IF
               END IF
            END IF
         I = NG
         G(1,I) = GMAX(I) * RSCALE
         G(2,I) = GPOS(1,I)
         G(3,I) = GPOS(2,I)
         G(4,I) = GWID(1,I)
         G(5,I) = GWID(2,I)
         G(6,I) = GWID(3,I)
      ELSE IF (CHOICS(CHOICE).EQ.'ZOOM IN') THEN
         INCR = INCR + 1
      ELSE IF (CHOICS(CHOICE).EQ.'ZOOM OUT') THEN
         INCR = INCR - 1
      ELSE IF (CHOICS(CHOICE).EQ.'REBOX') THEN
C                                       set box
         NBO = 0
         MBOX = 1
         CALL GRBOXS (GRCHS(4), MBOX, NBO, RBLC, RTRC, TVSCR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'SETTING NEW BOX'
            GO TO 990
            END IF
         LOCBLC(1) = RBLC(1) + 0.01
         LOCBLC(2) = RBLC(2) + 0.01
         LOCTRC(1) = RTRC(1) + 0.99
         LOCTRC(2) = RTRC(2) + 0.99
         CALL RCOPY (MAXP, G, GTEMP)
         I = NGAUSS
         CALL SADDAT (NXI, IMAGE, NXR, RMSIM, LOCBLC, LOCTRC, FDATA, JJ,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'FINDING DATA FOR ISLAND'
            GO TO 990
         ELSE
            NGAUSS = I
            CALL RCOPY (MAXP, GTEMP, G)
            GO TO 5
            END IF
         END IF
C                                       init the TV
 200  J = NGRAY + GRCHS(3)
      CALL YCINIT (J, LUTBUF)
      CALL YZERO (J, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'ZERO GAUSSIAN PLOT'
         GO TO 990
         END IF
      J = NGRAY + GRCHS(4)
      CALL YCINIT (J, LUTBUF)
      CALL YZERO (J, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'ZERO GAUSSIAN PLOT'
         GO TO 990
         END IF
      J = 1
      CALL YCINIT (J, LUTBUF)
      CALL YZERO (J, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'ZERO GAUSSIAN PLOT'
         GO TO 990
         END IF
C                                       set window
      J = -1
      ICH = 1
      ITVC(1) = 0
      ITVC(2) = 0
      INCS(1) = -INCR
      INCS(2) = -INCR
      CALL TVWIND (J, INCS, RBLC, RTRC, ICH, ITVC, IWIN, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'SET TVLOAD WINDOWS'
         GO TO 990
         END IF
C                                       image catalog
      WRITE (MSGTXT,1100) INCR
      CALL MSGWRT (2)
      CALL COPY (4, IWIN, CATBLK(IIWIN))
      CALL COPY (4, ITVC, CATBLK(IICOR))
      FUNC = ' '
      CALL CHR2H (2, FUNC, 1, CATH(IITRA))
      CATR(IRRAN) = RMIN
      CATR(IRRAN+1) = RMAX
      CALL YCWRIT (ICH, ITVC, CATBLK, TVSCR, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE IMAGE CATALOG'
         GO TO 990
         END IF
      NPIX = 1 + IWIN(3) - IWIN(1)
      MPIX = 1 + (ITVC(3) - ITVC(1)) / INCR
      LPIX = 1 + ITVC(3) - ITVC(1)
      IYTV = ITVC(2) - 1
      CALL ISCALE (FUNC, MAXINT, CATR(IRRAN), NPIX, INCS,
     *   IMAGE(IWIN(1),IWIN(2)), OFMBUF)
      CALL LINTER (MPIX, INCR, OFMBUF, IBUFF)
      DO 230 IY = IWIN(2),IWIN(4)-1
C                                       get interpolation row
         CALL ISCALE (FUNC, MAXINT, CATR(IRRAN), NPIX, INCS,
     *      IMAGE(IWIN(1),IY+1), OFMBUF)
         CALL LINTER (MPIX, INCR, OFMBUF, JBUFF)
C                                       write current row
         IYTV = IYTV + 1
         CALL YIMGIO ('WRIT', ICH, ITVC, IYTV, 0, LPIX, IBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE IMAGE ROW TO TV'
            GO TO 990
            END IF
C                                       interpolate rows
         IF (INCR.GT.1) THEN
            X = 1.0 / INCR
            DO 220 I = 2,INCR
               Y = (I-1) * X
               DO 210 IX = 1,LPIX
                  TBUFF(IX) = IBUFF(IX) + Y * (JBUFF(IX) - IBUFF(IX))
     *               + 0.4999
 210              CONTINUE
               IYTV = IYTV + 1
               CALL YIMGIO ('WRIT', ICH, ITVC, IYTV, 0, LPIX, TBUFF,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE IMAGE ROW TO TV'
                  GO TO 990
                  END IF
 220           CONTINUE
            END IF
         CALL COPY (LPIX, JBUFF, IBUFF)
 230     CONTINUE
C                                       end write row only
      IYTV = IYTV + 1
      CALL YIMGIO ('WRIT', ICH, ITVC, IYTV, 0, LPIX, IBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE IMAGE ROW TO TV'
         GO TO 990
         END IF
C                                       Draw boundary line
      CALL MP2IMA (LOCBLC, RPOS)
      IXC(1) = RPOS(1) + 0.01
      IYC(1) = RPOS(2) + 0.01
      IXC(1) = MAX (1, IXC(1))
      IYC(1) = MAX (1, IYC(1))
      CALL MP2IMA (LOCTRC, RPOS)
      IXC(3) = RPOS(1) + 0.99
      IYC(3) = RPOS(2) + 0.99
      IXC(3) = MIN (MAXXTV(1), IXC(3))
      IYC(3) = MIN (MAXXTV(2), IYC(3))
      IXC(5) = IXC(1)
      IXC(4) = IXC(1)
      IXC(2) = IXC(3)
      IYC(5) = IYC(1)
      IYC(4) = IYC(3)
      IYC(2) = IYC(1)
      I = NGRAY + GRCHS(4)
      CALL IMVECT ('ONNN', I, 5, IXC, IYC, IBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'DRAWING ISLAND BORDER LINE'
         GO TO 990
         END IF
C                                       draw Gaussians
      J = NGRAY + GRCHS(5)
      CALL YZERO (J, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'ZERO TEMP GAUSSIAN PLOT'
         GO TO 990
         END IF
      DO 250 NG = 1,NGAUSS
         CALL ELLIPS (GRCHS(3), NG, G(2,NG), G(4,NG), IBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'DRAWING GAUSSIAN HALF-WIDTH'
            GO TO 990
            END IF
 250     CONTINUE
      GO TO 50
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SADTV ERROR:',I4,' ON ',A)
 1005 FORMAT ('ISLAND BLC',2I5,'   TRC',2I5,'  PIXRANGE',2F10.6)
 1075 FORMAT (I1)
 1100 FORMAT ('Loading TV interpolated by factor',I3)
      END
      SUBROUTINE SADTV2 (NXI, NYI, IMAGE, LOCBLC, LOCTRC, TVISON,
     *   NGAUSS, G, IRET)
C-----------------------------------------------------------------------
C   SADTV2 displays the residual image and lets the user accept, reject
C   or ask for a Mulligan.
C   Inputs:
C      NXI      I      Number X pixels in image
C      NYI      I      Number Y pixels in image
C      IMAGE    R(*)   Current residual image
C      LOCBLC   R(7)   BLC of island
C      LOCTRC   R(7)   TRC of island
C      NGAUSS   I      Number of Gaussians fit
C      G        R(*)   Initial guess
C   Output:
C      IRET     I      Codes of whatr happened
C                        > 0  => TV error -> die
C                          0  => fit accepted
C                         -1  => skip this island, go on
C                         -2  => done enough, close down gently
C                         -3  => retry this island
C-----------------------------------------------------------------------
      INTEGER   NXI, NYI, NGAUSS, IRET
      REAL      IMAGE(NXI,*), LOCBLC(*), LOCTRC(*), G(6,*)
      LOGICAL   TVISON
C
      INTEGER   NOPTS
      PARAMETER (NOPTS = 11)
      INCLUDE 'INCS:PTVC.INC'
      INCLUDE 'TVSADG.INC'
C
      INTEGER   MTYPE, NCOL, NROWS(2), GRCHS(4), TOPSEP, SIDSEP, TIMLIM,
     *   NTITLE, TVBUTT, CHOICE, TVSCR(8192), IBLC(2), ITRC(2), NP(2),
     *   LP(2), INCS(2), I, IX, IY, NLEVS, LCHAN, JJ, LUTBUF(TVMLUT),
     *   L, II, ICOLOR, NG, WIN(4), J, ITVC(4), IWIN(4), ICH, IBUT,
     *   IBUFF(8192), JBUFF(8192), TBUFF(8192), NPIX, MPIX, LPIX, IYTV,
     *   IXC(33), IYC(33), INCR, NSUM, IX1, IX2
      LOGICAL   LEAVE(NOPTS)
      CHARACTER CHOICS(NOPTS)*12, TITLE(2)*72, ISHELP*6, FUNC*2
      REAL      RMIN, RMAX, SLOPE, OFFS, OFMBUF(TVMOFM), RPOS(2), X, Y,
     *   RBLC(7), RTRC(7), WMAX, WMIN, WSUM, WSUMS
      EQUIVALENCE (LUTBUF, OFMBUF, TVSCR)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA CHOICS /'OFF TRANS', 'OFF TVZOOM', 'TVFIDDLE', 'CURVALUE',
     *   'ZOOM IN', 'ZOOM OUT', 'GOOD', 'RE-TRY', 'NEXT ISLAND',
     *   ' ', 'QUIT'/
      DATA LEAVE /NOPTS*.FALSE./
      DATA ISHELP /'TVSAD'/
C-----------------------------------------------------------------------
C                                       skip out if TV is off
      IRET = 0
      ICOLOR = 0
      IF (.NOT.TVISON) GO TO 999
C                                       menu parameters
      GRCHS(1) = 1
      GRCHS(2) = 2
      GRCHS(3) = 4
      GRCHS(4) = 3
      MTYPE = -1
      NCOL = 2
      NROWS(1) = 6
      NROWS(2) = 5
      SIDSEP = 20
      TOPSEP = 25
      NTITLE = 2
      TITLE(1) = ' '
      TITLE(2) = ' '
      TIMLIM = 0
C                                       window parameters
      NP(1) = NXI
      NP(2) = NYI
      CALL YWINDO ('READ', WINDTV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FIND TV WINDOW'
         GO TO 990
         END IF
      CALL RCOPY (7, LOCBLC, RBLC)
      CALL RCOPY (7, LOCTRC, RTRC)
      WIN(1) = RBLC(1) + 0.01
      WIN(2) = RBLC(2) + 0.01
      WIN(3) = RTRC(1) + 0.01
      WIN(4) = RTRC(2) + 0.01
      DO 10 I = 1,2
         L = LOCTRC(I) - LOCBLC(I) + 1.1
         L = MAX (125 - L, 0)
         IBLC(I) = LOCBLC(I) - L/2.0
         ITRC(I) = LOCTRC(I) + L/2.0 + 0.5
         IBLC(I) = MAX (1, IBLC(I))
         ITRC(I) = MIN (NP(I), ITRC(I))
         LP(I) = ITRC(I) - IBLC(I) + 1
         INCS(I) = (WINDTV(2+I) - WINDTV(I)) * 0.8
         INCS(I) = INCS(I) / LP(I)
         INCS(I) = MAX (1, MIN (16, INCS(I)))
         RBLC(I) = IBLC(I)
         RTRC(I) = ITRC(I)
 10      CONTINUE
      IF (INCS(2).LT.INCS(1)) INCS(1) = INCS(2)
      INCR = INCS(1)
      RMAX = -1.E10
      RMIN = -RMAX
      WMAX = RMAX
      WMIN = RMIN
      NSUM = 0
      WSUM = 0.0
      WSUMS = 0.0
      DO 30 IY = IBLC(2),ITRC(2)
         DO 15 IX = IBLC(1),ITRC(1)
            IF (IMAGE(IX,IY).NE.FBLANK) THEN
               RMAX = MAX (RMAX, IMAGE(IX,IY))
               RMIN = MIN (RMIN, IMAGE(IX,IY))
               END IF
 15         CONTINUE
         IF ((IY.GE.LOCBLC(2)) .AND. (IY.LE.LOCTRC(2))) THEN
            IX1 = LOCBLC(1) + 0.1
            IX2 = LOCTRC(1) + 0.1
            DO 20 IX = IX1,IX2
               IF (IMAGE(IX,IY).NE.FBLANK) THEN
                  WMAX = MAX (WMAX, IMAGE(IX,IY))
                  WMIN = MIN (WMIN, IMAGE(IX,IY))
                  NSUM = NSUM + 1
                  WSUM = WSUM + IMAGE(IX,IY)
                  WSUMS = WSUMS + IMAGE(IX,IY) * IMAGE(IX,IY)
                  END IF
 20            CONTINUE
            END IF
 30      CONTINUE
      WRITE (TITLE(1),1005) WIN, RMIN, RMAX
      IF (NSUM.LE.0) THEN
         NTITLE = 1
      ELSE
         WSUM = WSUM / NSUM
         WSUMS = WSUMS / NSUM - WSUM * WSUM
         WSUMS = SQRT (MAX (0.0, WSUMS))
         WRITE (TITLE(2),1006) WMIN, WMAX, WSUMS
         END IF
C                                       init TV
      CALL YINIT (TVSCR, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT THE TV'
         GO TO 990
         END IF
      CALL YSLECT ('ONNN', 1, ICOLOR, TVSCR, IRET)
      DO 40 I = 1,4
         J = NGRAY + I
         CALL YSLECT ('ONNN', J, ICOLOR, TVSCR, IRET)
 40      CONTINUE
C                                       initial TV load
      GO TO 200
C                                       menu
 50   CALL TVMENU (MTYPE, NCOL, NROWS, GRCHS, TOPSEP, SIDSEP, ISHELP,
     *   CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHOICE, TVBUTT, TVSCR,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'MENU MANAGER'
         GO TO 990
         END IF
      IF (CHOICS(CHOICE).EQ.' ') THEN
         GO TO 50
      ELSE IF (CHOICS(CHOICE).EQ.'QUIT') THEN
         IRET = -2
         GO TO 999
      ELSE IF (CHOICS(CHOICE).EQ.'NEXT ISLAND') THEN
         IRET = -1
         GO TO 999
      ELSE IF (CHOICS(CHOICE).EQ.'GOOD') THEN
         IRET = 0
         GO TO 999
      ELSE IF (CHOICS(CHOICE).EQ.'RE-TRY') THEN
         IRET = -3
         GO TO 999
      ELSE IF (CHOICS(CHOICE).EQ.'TVFIDDLE') THEN
         NLEVS = LUTOUT + 1
         LCHAN = 1
         CALL TVFIDL (LCHAN, NLEVS, TVSCR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'ENHANCING TV IMAGE'
            GO TO 990
            END IF
         GO TO 50
      ELSE IF (CHOICS(CHOICE).EQ.'CURVALUE') THEN
         IBUT = -1
         CALL TVALUE (GRCHS(2), NXI, NYI, IMAGE, IBUT, RPOS, OFMBUF,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'DOING CURSOR VALUE'
            GO TO 990
            END IF
      ELSE IF (CHOICS(CHOICE).EQ.'OFF TVZOOM') THEN
         TVZOOM(1) = 0
         TVZOOM(2) = MAXXTV(1) / 2
         TVZOOM(3) = MAXXTV(2) / 2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .FALSE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RESETTING TVZOOM'
            GO TO 990
            END IF
         GO TO 50
      ELSE IF (CHOICS(CHOICE).EQ.'OFF TRANS') THEN
C                                       off pseudo
         I = OFMINP + 1
         CALL RFILL (I, 0.0, OFMBUF)
         NLEVS = LUTOUT + 1
         IF (I.LT.NLEVS) NLEVS = I
         OFFS = 1.0 / REAL (OFMOUT)
         SLOPE = OFFS * (OFMOUT + 1.0) / REAL(NLEVS)
         DO 60 I = 1,NLEVS
            OFMBUF(I) = I * SLOPE - OFFS
 60         CONTINUE
         I = (OFMINP + 1) / NLEVS
         JJ = NLEVS
         DO 65 II = 2,I
            CALL RCOPY (NLEVS, OFMBUF, OFMBUF(JJ+1))
            JJ = JJ + NLEVS
 65         CONTINUE
         ICOLOR = 7
         CALL YOFM ('WRIT', ICOLOR, .FALSE., OFMBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RESET COLOR TABLES'
            GO TO 990
            END IF
C                                       off transf
         NLEVS = MAXINT + 1
         SLOPE = (LUTOUT+1.0) / (MAXINT+1.0)
         DO 70 I = 1,NLEVS
            LUTBUF(I) = I * SLOPE - 0.5
 70         CONTINUE
         I = 1
         CALL YLUT ('WRIT', I, ICOLOR, .FALSE., LUTBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RESET LUT (B&W) TABLES'
            GO TO 990
            END IF
         GO TO 50
      ELSE IF (CHOICS(CHOICE).EQ.'ZOOM IN') THEN
         INCR = INCR + 1
      ELSE IF (CHOICS(CHOICE).EQ.'ZOOM OUT') THEN
         INCR = INCR - 1
         END IF
C                                       init the TV
 200  J = NGRAY + GRCHS(3)
      CALL YCINIT (J, LUTBUF)
      CALL YZERO (J, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'ZERO GAUSSIAN PLOT'
         GO TO 990
         END IF
      J = NGRAY + GRCHS(4)
      CALL YCINIT (J, LUTBUF)
      CALL YZERO (J, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'ZERO GAUSSIAN PLOT'
         GO TO 990
         END IF
      J = 1
      CALL YCINIT (J, LUTBUF)
      CALL YZERO (J, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'ZERO GAUSSIAN PLOT'
         GO TO 990
         END IF
C                                       set window
      J = -1
      ICH = 1
      ITVC(1) = 0
      ITVC(2) = 0
      INCS(1) = -INCR
      INCS(2) = -INCR
      CALL TVWIND (J, INCS, RBLC, RTRC, ICH, ITVC, IWIN, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'SET TVLOAD WINDOWS'
         GO TO 990
         END IF
C                                       image catalog
      WRITE (MSGTXT,1100) INCR
      CALL MSGWRT (2)
      CALL COPY (4, IWIN, CATBLK(IIWIN))
      CALL COPY (4, ITVC, CATBLK(IICOR))
      FUNC = ' '
      CALL CHR2H (2, FUNC, 1, CATH(IITRA))
      CATR(IRRAN) = RMIN
      CATR(IRRAN+1) = RMAX
      CALL YCWRIT (ICH, ITVC, CATBLK, TVSCR, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE IMAGE CATALOG'
         GO TO 990
         END IF
      NPIX = 1 + IWIN(3) - IWIN(1)
      MPIX = 1 + (ITVC(3) - ITVC(1)) / INCR
      LPIX = 1 + ITVC(3) - ITVC(1)
      IYTV = ITVC(2) - 1
      CALL ISCALE (FUNC, MAXINT, CATR(IRRAN), NPIX, INCS,
     *   IMAGE(IWIN(1),IWIN(2)), OFMBUF)
      CALL LINTER (MPIX, INCR, OFMBUF, IBUFF)
      DO 230 IY = IWIN(2),IWIN(4)-1
C                                       get interpolation row
         CALL ISCALE (FUNC, MAXINT, CATR(IRRAN), NPIX, INCS,
     *      IMAGE(IWIN(1),IY+1), OFMBUF)
         CALL LINTER (MPIX, INCR, OFMBUF, JBUFF)
C                                       write current row
         IYTV = IYTV + 1
         CALL YIMGIO ('WRIT', ICH, ITVC, IYTV, 0, LPIX, IBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE IMAGE ROW TO TV'
            GO TO 990
            END IF
C                                       interpolate rows
         IF (INCR.GT.1) THEN
            X = 1.0 / INCR
            DO 220 I = 2,INCR
               Y = (I-1) * X
               DO 210 IX = 1,LPIX
                  TBUFF(IX) = IBUFF(IX) + Y * (JBUFF(IX) - IBUFF(IX))
     *               + 0.4999
 210              CONTINUE
               IYTV = IYTV + 1
               CALL YIMGIO ('WRIT', ICH, ITVC, IYTV, 0, LPIX, TBUFF,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE IMAGE ROW TO TV'
                  GO TO 990
                  END IF
 220           CONTINUE
            END IF
         CALL COPY (LPIX, JBUFF, IBUFF)
 230     CONTINUE
C                                       end write row only
      IYTV = IYTV + 1
      CALL YIMGIO ('WRIT', ICH, ITVC, IYTV, 0, LPIX, IBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE IMAGE ROW TO TV'
         GO TO 990
         END IF
C                                       Draw boundary line
      CALL MP2IMA (LOCBLC, RPOS)
      IXC(1) = RPOS(1) + 0.01
      IYC(1) = RPOS(2) + 0.01
      IXC(1) = MAX (1, IXC(1))
      IYC(1) = MAX (1, IYC(1))
      CALL MP2IMA (LOCTRC, RPOS)
      IXC(3) = RPOS(1) + 0.99
      IYC(3) = RPOS(2) + 0.99
      IXC(3) = MIN (MAXXTV(1), IXC(3))
      IYC(3) = MIN (MAXXTV(2), IYC(3))
      IXC(5) = IXC(1)
      IXC(4) = IXC(1)
      IXC(2) = IXC(3)
      IYC(5) = IYC(1)
      IYC(4) = IYC(3)
      IYC(2) = IYC(1)
      I = NGRAY + GRCHS(4)
      CALL IMVECT ('ONNN', I, 5, IXC, IYC, IBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'DRAWING ISLAND BORDER LINE'
         GO TO 990
         END IF
C                                       draw Gaussians
      DO 250 NG = 1,NGAUSS
         CALL ELLIPS (GRCHS(3), NG, G(2,NG), G(4,NG), IBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CALL TO ELLIPS DRAWING'
            GO TO 990
            END IF
 250     CONTINUE
      GO TO 50
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SADTV2 ERROR:',I4,' ON ',A)
 1005 FORMAT ('ISLAND BLC',2I5,'   TRC',2I5,'  PIXRANGE',2F10.6)
 1006 FORMAT ('RESIDUAL MIN',F10.6,' MAX',F10.6,' RMS',F10.6)
 1100 FORMAT ('Loading TV interpolated by factor',I3)
      END
      SUBROUTINE ELLIPS (IGR, NG, GPOS, GWID, IBUFF, IRET)
C-----------------------------------------------------------------------
C   Draw an ellipse in a graphics plane
C   Inputs:
C      IGR    I      Graphics plane
C      NG     I      Gauussian number
C      GPOS   R(2)   Center coordinate
C      GWID   R(3)   Major, minor, PA
C   Outputs:
C      IBUFF  I(*)   SCRATCH BUFFER
C      IRET   I      error code
C-----------------------------------------------------------------------
      INTEGER   IGR, NG, IBUFF(*), IRET
      REAL      GPOS(2), GWID(3)
C
      INTEGER   I, IXC(33), IYC(33)
      REAL      RPOS(2), X, Y, SLOPE, OFFS, CG, SG
      CHARACTER CDUM*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      CALL MP2IMA (GPOS, RPOS)
      RPOS(1) = RPOS(1) - CSIZTV(1)/2
      RPOS(2) = RPOS(2) - CSIZTV(2)/2
      IXC(1) = RPOS(1) + 0.5
      IYC(1) = RPOS(2) + 0.5
      I = NGRAY + IGR
      WRITE (CDUM,1010) NG
      CALL IMCHAR (I, IXC(1), IYC(1), 0, 0, CDUM, IBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'DRAWING GAUSSIAN NUMBER'
         GO TO 990
         END IF
      SLOPE = TWOPI / 32.0D0
      OFFS = 0.0
      CG = COS (GWID(3)-PI/2.0D0)
      SG = SIN (GWID(3)-PI/2.0D0)
      DO 20 I = 1,33
         X = GWID(1) * COS (OFFS) / 2.0
         Y = GWID(2) * SIN (OFFS) / 2.0
         RPOS(1) = Y * CG - X * SG + GPOS(1)
         RPOS(2) = X * CG + Y * SG + GPOS(2)
         CALL MP2IMA (RPOS, RPOS)
         IXC(I) = RPOS(1) + 0.5
         IYC(I) = RPOS(2) + 0.5
         IXC(I) = MAX (1, MIN (MAXXTV(1), IXC(I)))
         IYC(I) = MAX (1, MIN (MAXXTV(2), IYC(I)))
         OFFS = OFFS + SLOPE
 20      CONTINUE
      I = NGRAY + IGR
      CALL IMVECT ('ONNN', I, 33, IXC, IYC, IBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'DRAWING GAUSSIAN HALF-WIDTH'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ELLIPS: ERROR',I4,' ON ',A)
 1010 FORMAT (I1)
      END
      SUBROUTINE TVSADR (NXI, IMAGE, NXR, RMSIM, NPK, IRET)
C-----------------------------------------------------------------------
C   TVSADD does all the work
C   Inputs:
C      NXI     I      X dimension of IMAGE
C      IMAGE   R(*)   Residual image
C      NXR     I      X dimension of RMSimage
C      RMSIM   R(*)   Image of RMSes
C      NPK     I      Number components fit
C   Output:
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   NXI, NXR, NPK, IRET
      REAL      IMAGE(NXI,*), RMSIM(NXR,*)
C
      EXTERNAL  FXDVD
      INCLUDE 'TVSAD.INC'
      INCLUDE 'TVSADS.INC'
      INTEGER   IPK, I, IROUND, NTOT, IPT, NK
      REAL      A, B, DELTAX, DELTAY, DELXY, LOCBLC(7),
     *   LOCTRC(7), TEMP, RESMAX, RESMIN, RESSUM, SUMSQ
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      CALL RFILL (7, 1.0, LOCBLC)
      CALL RFILL (7, 1.0, LOCTRC)
      DO 200 IPK = 1,NPK
         SBPA = SIN (GLIST(6,IPK))
         CBPA = COS (GLIST(6,IPK))
         A = GLIST(4,IPK)
         B = GLIST(5,IPK)
         IF (A.EQ.0.0) A = 1.0
         IF (B.EQ.0.0) B = 1.0
         DELTAX = ((SBPA/A)**2 + (CBPA/B)**2)
         DELTAY = ((CBPA/A)**2 + (SBPA/B)**2)
         DELXY  = ((1./A)**2 - (1/B)**2)*(SBPA*CBPA)**2
C                                       This pushes down the exp by
C                                       .01
         A      = SQRT (1.66/(DELTAY - DELXY**2/DELTAX))
         DELTAX = SQRT (1.66/(DELTAX - DELXY**2/DELTAY))
         DELTAY = A
         LOCBLC(1) = IROUND (GLIST(2,IPK) - DELTAX)
         LOCTRC(1) = IROUND (GLIST(2,IPK) + DELTAX)
         LOCBLC(2) = IROUND (GLIST(3,IPK) - DELTAY)
         LOCTRC(2) = IROUND (GLIST(3,IPK) + DELTAY)
         TEMP = BOXSIZ
         I = SQRT (TEMP)
         TEMP = I
         DO 110 I = 1,2
            IF (LOCTRC(I) - LOCBLC(I).GT.TEMP) THEN
               A = 0.5 * (LOCTRC(I) + LOCBLC(I))
               LOCTRC(I) = A + TEMP/2. - 1.
               LOCBLC(I) = A - TEMP/2. + 1.
               END IF
 110         CONTINUE
         LOCBLC(1) = MAX (1.0, LOCBLC(1))
         LOCBLC(2) = MAX (1.0, LOCBLC(2))
         LOCTRC(1) = MIN (RNX, LOCTRC(1))
         LOCTRC(2) = MIN (RNY, LOCTRC(2))
C                               Read in data in subwindow
         CALL SADDAT (NXI, IMAGE, NXR, RMSIM, LOCBLC, LOCTRC, FDATA,
     *      NTOT, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1030) 'SADDAT', IRET
            GO TO 900
            END IF
         SUMSQ = 0.0
         RESMAX = 0.0
         RESMIN = 0.0
         RESSUM = 0.0
         NK = 0
         NPTS = NX * NY
         DO 120 IPT = 1,NPTS
            IF (FDATA(IPT).NE.FBLANK) THEN
               NK = NK + 1
               FDATA(IPT) = FDATA(IPT) / RSCALE
               RESSUM = RESSUM + FDATA(IPT)
               SUMSQ = SUMSQ + FDATA(IPT)**2
               RESMAX = MAX (RESMAX, FDATA(IPT))
               RESMIN = MIN (RESMIN, FDATA(IPT))
               END IF
 120        CONTINUE
         IF (NTOT.GT.0) SUMSQ = SQRT (SUMSQ /NTOT)
         IF (NOCLN) THEN
            RESSUM = RESSUM / NK
         ELSE
            RESSUM = RESSUM / (1.1331 * CB(1) * CB(2))
            END IF
         PLIST(1,IPK) = SUMSQ
         PLIST(2,IPK) = RESMAX
         PLIST(3,IPK) = RESMIN
         PLIST(4,IPK) = RESSUM
 200     CONTINUE
C
 900  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('TVSADR ERROR FROM ',A,' WAS',I5)
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
      INCLUDE 'TVSAD.INC'
      INTEGER   NK, IFLAG, I, J, K, IX1, IY1, NTOT, L
      REAL      STH2, CTH2, S2TH, C2TH, MJ, MN, VA, VB, VC, VD, X, Y,
     *   X2, Y2, XY, CON, TWORFV, TWOCON, FV, G4C, G5C, CMIN, CMAX, ARG
      DOUBLE PRECISION P(*), F, GRAD(*), GTEMP(MAXP), DNRM2, FEXT
      INCLUDE 'INCS:DDCH.INC'
      DATA CON /2.772589/
C-----------------------------------------------------------------------
      TWOCON = 2.0*CON
      NPTS = NX*NY
      NK = 0
      DO 10 K = 1,NPTS
         IF (FDATA(K).EQ.FBLANK) GO TO 10
            NK = NK + 1
            RESID(NK) = -FDATA(K)
 10      CONTINUE
      NTOT = NK
      DO 20 I = 1,NVAR
         G(JVAR(I),IVAR(I)) = P(I)
 20      CONTINUE
      CMIN = MIN (CB(1), CB(2))
      CMAX = MAX (CB(1), CB(2))
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
      FEXT = 0.0
      DO 80 I = 1,NGAUSS
         IF (G(1,I).EQ.0.0) G(1,I) = 1.E-3
         IF (G(4,I).EQ.0.0) G(5,I) = 1.E-3
         IF (G(5,I).EQ.0.0) G(4,I) = 1.E-3
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
            IF (FDATA(L).EQ.FBLANK) GO TO 70
               NK = NK+1
               X = IX1+MOD(L-1,NX)-G(2,I)
               Y = IY1+AINT((L-0.5)/NX)-G(3,I)
               ARG = ((VA*X+VC*Y)*X+VB*Y**2)
C                               limit accuracy to 10**-4 to save time
               IF (ARG.LT.-1.0) THEN
                  FV = 1.E8
               ELSE IF (ARG.LT.9.2) THEN
                  FV = G(1,I)*EXP(-ARG)
               ELSE
                  FV = 0.0
                  END IF
               RESID(NK) = RESID(NK) + FV
 70         CONTINUE
 80      CONTINUE
      F = DNRM2 (NTOT, RESID, 1)**2
      F = F + FEXT ** 2
C                                       get gradient
      IF (IFLAG.NE.1) THEN
         DO 90 I = 1,MAXP
            GTEMP(I) = 0D0
 90         CONTINUE
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
               IF (FDATA(L).NE.FBLANK) THEN
                  NK = NK + 1
                  X = IX1 + MOD(L-1,NX) - G(2,I)
                  Y = IY1 + AINT((L-0.5)/NX) - G(3,I)
                  X2 = X**2
                  Y2 = Y**2
                  XY = X*Y
                  ARG = (VA*X2+VB*Y2+VC*XY)
                  IF (ARG.LT.-1.0) THEN
                     FV = 1.E8
                  ELSE
                     FV = G(1,I) * EXP(-ARG)
                     END IF
                  TWORFV = 2.0 * RESID(NK) * FV
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
                  END IF
 130           CONTINUE
 140        CONTINUE
         K = 0
         L = 0
         DO 160 I=1,NGAUSS
            DO 150 J=1,6
               L = L+1
               IF (E(J,I).GE.0.) THEN
                  K = K+1
                  GRAD(K) = GTEMP(L)
                  END IF
 150           CONTINUE
 160        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE SADOPN (BLC, TRC, IERR)
C-----------------------------------------------------------------------
C   Open the input map and get the header
C   Input from common:
C      users input adverbs
C   Outputs:
C      BLC    R(7)   Bottom left corner to search
C      TRC    R(7)   Top right corner of search area
C      IERR   I      Error return  0-> okay, 1-> error return
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7)
      INTEGER   IERR
C
      INTEGER   IFIL, IROUND, I, INVOL
      HOLLERITH HOLMA
      INCLUDE 'TVSAD.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
C                                       Store name for easy I/O
      INTYPE = 'MA'
      CALL CHR2H (4, 'MA  ', 1, HOLMA)
C                                       Is there are RMS image
      CALL H2CHR (12, 1, X2NAM, IN2NAM)
      IF ((IN2NAM.NE.' ') .AND. (DPARM(9).GT.0.0)) THEN
         CALL H2WAWA (X2NAM, X2CLS, X2SEQ, HOLMA, X2DISK, XUSER, INNA)
C                                       Open input map
         CALL OPENCF (LUNR, INNA, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) 'RMS', IERR
            GO TO 900
            END IF
         CALL WAWA2A (INNA, IN2NAM, IN2CLS, IN2SEQ, INTYPE, IN2DSK, I)
C                                       Get header values
         CALL GETHDR (LUNR, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) 'RMS', IERR
            GO TO 900
            END IF
C                                       set coordinates
         LOCNUM = 2
         CALL FILL (5, 1, DEPTH)
         CALL SETLOC (DEPTH, .FALSE.)
         CALL COPY (256, CATBLK, IN2BLK)
         DORMSI = DPARM(9).GE.2.0
      ELSE
         LUNR = 0
         DORMSI = .FALSE.
         END IF
C                                       INPUT image
      CALL H2WAWA (XINNAM, XINCLS, XINSEQ, HOLMA, XINDSK, XUSER, INNA)
C                                       Open input map
      CALL OPENCF (LUN1, INNA, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'INPUT', IERR
         GO TO 900
         END IF
C                                       Get header values
      CALL GETHDR (LUN1, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) 'INPUT', IERR
         GO TO 900
         END IF
C                                       Window full plane
      CALL COPY (256, CATBLK, INBLK)
C                                       set corners properly
      CALL RCOPY (7, XBLC, BLC)
      CALL RCOPY (7, XTRC, TRC)
      CALL MAPWIN (LUN1, BLC, TRC, IERR)
      CALL FILNUM (LUN1, IFIL, IERR)
      DO 20 I = 1,7
         XBLC(I) = FILTAB(POBLC+I-1,IFIL)
         XTRC(I) = FILTAB(POTRC+I-1,IFIL)
 20      CONTINUE
      CALL RCOPY (5, XBLC(3), XTRC(3))
C                                       reset to read all
      CALL RCOPY (7, XBLC, BLC)
      CALL RCOPY (7, XTRC, TRC)
      BLC(1) = 1.0
      BLC(2) = 1.0
      TRC(1) = 0.0
      TRC(2) = 0.0
      CALL MAPWIN (LUN1, BLC, TRC, IERR)
      CALL FILNUM (LUN1, IFIL, IERR)
      CALL COPY (5, FILTAB(POBLC+2, IFIL), DEPTH)
      INVOL = FILTAB (POVOL,IFIL)
      INSL = FILTAB (POCAT,IFIL)
C                                       get plane number
      PLANE = 1
      IF (CATBLK(KIDIM).GE.7) PLANE = MAX (DEPTH(5), 1)
      IF (CATBLK(KIDIM).GE.6) PLANE = MAX (DEPTH(4), 1) +
     *   (PLANE-1) * MAX (CATBLK(KINAX+5), 1)
      IF (CATBLK(KIDIM).GE.5) PLANE = MAX (DEPTH(3), 1) +
     *   (PLANE-1) * MAX (CATBLK(KINAX+4), 1)
      IF (CATBLK(KIDIM).GE.4) PLANE = MAX (DEPTH(2), 1) +
     *   (PLANE-1) * MAX (CATBLK(KINAX+3), 1)
      IF (CATBLK(KIDIM).GE.3) PLANE = MAX (DEPTH(1), 1) +
     *   (PLANE-1) * MAX (CATBLK(KINAX+2), 1)
C                                       Get standard coord info
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
      DOPA = AXTYP(LOCNUM).EQ.1
      XCEN = CATR(KRCRP)
      YCEN = CATR(KRCRP+1)
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
         BWCORR = .FALSE.
         END IF
C                                       MF file create/open
      CALL WAWA2A (INNA, INNAME, INCLAS, INSEQ, INTYPE, INDISK, I)
      MFVER = IROUND (XINVER)
      MFVER = MAX (0, MFVER)
      CALL CATIO ('READ', INDISK, INSL, CATBLK, 'WRIT', SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1025) IERR
         GO TO 900
         END IF
      MFLUN = 28
      CALL MFINI (MFLUN, INDISK, INSL, DEPTH, PLANE, MFVER, CATBLK,
     *   MFBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) MFVER, IERR
         GO TO 900
         END IF
      CALL CATDIR ('CSTA', INDISK, INSL, INNAME, INCLAS, INSEQ,
     *   INTYPE, NLUSER, 'CLWR', SCRBUF, I)
      CALL RCOPY (7, XBLC, BLC)
      CALL RCOPY (7, XTRC, TRC)
C                                       Error return
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SADOPN: COULD NOT OPEN ',A,' MAP.  IER=',I7)
 1020 FORMAT ('SADOPN: COULD NOT GET ',A,' HEADER.  IER=',I7)
 1025 FORMAT ('SADOPN: COULD NOT CHANGE TO WRITE STATUS, IER=',I7)
 1030 FORMAT ('SADOPN: COULD NOT OPEN/CREATE MF FILE VER',I4,' ERROR',
     *   I5)
      END
      SUBROUTINE SUBFIT (NXI, IMAGE, IERR)
C-----------------------------------------------------------------------
C   subtracts 1 new component from residual image file 2
C   INPUTS:
C      NXI     I      X dimension of IMAGE
C   In/Out
C      IMAGE   R(*)   Residual image
C   Input in Common:
C      G       R(6,NGAUSS)   Fit parameters
C   Output:
C      IERR    I      0 => okay, else error
C-----------------------------------------------------------------------
      INTEGER   NXI, IERR
      REAL      IMAGE(NXI,*)
C
      INCLUDE 'TVSAD.INC'
      INTEGER   I, IY, NYL, NYH, INNY, IFIL, IL, IH, NK
      REAL      SINE, COSINE, DELTAX, DELTAY, DELXY, A, B
      DOUBLE PRECISION  VALVAR(MAXP), DUMMY(MAXP)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       yes: get file pointer
C                                       get real values of win from
C                                       filtab
      INNY = NY
      CALL FILNUM (LUN2, IFIL, IERR)
      IF (NGAUSS.LE.0) GO TO 999
      NX = FILTAB(PONAX,IFIL)
      NY = FILTAB(PONAX+1,IFIL)
C                                       loop over components
      NYL = NY
      NYH = 1
C                                       How big an area do we need
      DO 10 I = 1,NGAUSS
         SINE = SIN (G(6,I))
         COSINE = COS (G(6,I))
         A = G(4,I)
         B = G(5,I)
         IF (A.EQ.0.0) A = 1.0
         IF (B.EQ.0.0) B = 1.0
         DELTAX = ((SINE/A)**2 + (COSINE/B)**2)
         DELTAY = ((COSINE/A)**2 + (SINE/B)**2)
         DELXY  = ((1./A)**2 - (1/B)**2)*(SINE*COSINE)**2
C                                       This pushes down the exp by
C                                       10**-4
         DELTAY = SQRT (3.3/(DELTAY - DELXY**2/DELTAX))
         DELTAY = DELTAY * 2.8
         IL = G(3,I) - DELTAY
         IH = G(3,I) + DELTAY + 0.9
         IL = MAX (1, IL)
         IH = MIN (IH, NY)
         NYL = MIN (IL, NYL)
         NYH = MAX (IH, NYH)
 10      CONTINUE
      CALL SADVST (VALVAR)
C                                       lie for FXDVD
      NY = 1
      WIN(1) = 1
      DO 30 IY = NYL,NYH
         WIN(2) = IY
         CALL RCOPY (NXI, IMAGE(1,IY), FDATA)
         CALL FXDVD (VALVAR, DUMMY(1), DUMMY, 1)
         NK = 0
         DO 20 I = 1,NX
            IF (FDATA(I).NE.FBLANK) THEN
               NK = NK + 1
               IMAGE(I,IY) = -RESID(NK)
               END IF
 20         CONTINUE
 30      CONTINUE
C
      NY = INNY
C
 999  RETURN
      END
      SUBROUTINE SADDAT (NXI, IMAGE, NXR, RMSIM, BLC, TRC, ARRAY, NGOOD,
     *   IER)
C-----------------------------------------------------------------------
C   Subroutine SADDAT reads the input data window into the ARRAY.  It
C   finds the value and relative position of the maximum in the Array.
C   If MULTI is .TRUE. it will also see if there are multiple maxima
C   above the cutoff level CUTT.  If there is only 1 maximum, it calls
C   SADDEF to fix the positions and widths of the initial guess to the
C   gaussian fit.  If there are 2 to 4 maxima, it calls MULDEF for
C   similar purposes.
C   Inputs:
C      NXI     I      X dimension of IMAGE
C      IMAGE   R(*)   Residual image
C      NXR     I      X dimension of RMSimage
C      RMSIM   R(*)   Image of RMSes
C      BLC     R(7)   Specify bottom left corner of subarray
C      TRC     R(7)   Specify top right corner of subarry
C   From commons:
C      CUTT    R      Search level for peaks
C      MULTI   L      Look for multiple peaks
C   Outputs:
C      ARRAY   R(*)   Returned subarray
C      NGOOD   I      Number of unflagged points in subarray
C      IER     I      Error return 0 -> okay
C                          1 -> error
C                          2 -> ill window
C   Common outputs:
C      NX      I      Size of subarray in X
C      NY      I      Size of subarray in Y
C      WIN(4)  R      Actual window region (clipped by edges of map)
C-----------------------------------------------------------------------
      INTEGER   NXI, NXR, IER, NGOOD
      REAL      IMAGE(NXI,*), RMSIM(NXR,*), BLC(7), TRC(7), ARRAY(*)
C
      INTEGER   IERR, PTS, I, J, IXMAX, IYMAX, XPK(4), YPK(4), NMPK,
     *   IPTS, IDY, IDX, IPT, IFIL, KMPK
      REAL      SPK(4)
      INTEGER   MAXPK
      PARAMETER (MAXPK = 500)
      INTEGER   NNMPK, XXPK(MAXPK), YYPK(MAXPK), IND, LWIN(4)
      REAL      SSPK(MAXPK), SMAX, SXMAX, SYMAX
      INCLUDE 'TVSAD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Initialize
      IER = 2
      DMAX = -1.0E25
      PTS = 1
C                                       get real values of win from
C                                       filtab
      CALL MAPWIN (LUN2, BLC, TRC, IERR)
      CALL FILNUM (LUN2, IFIL, IERR)
      WIN(1) = FILTAB(POBLC,   IFIL)
      WIN(2) = FILTAB(POBLC+1, IFIL)
      WIN(3) = FILTAB(POTRC,   IFIL)
      WIN(4) = FILTAB(POTRC+1, IFIL)
      LWIN(1) = FILTAB(POBLC,   IFIL)
      LWIN(2) = FILTAB(POBLC+1, IFIL)
      LWIN(3) = FILTAB(POTRC,   IFIL)
      LWIN(4) = FILTAB(POTRC+1, IFIL)
      NX = LWIN(3) - LWIN(1) + 1
      NY = LWIN(4) - LWIN(2) + 1
C                                       Line loop
      IER = 1
      NGOOD = 0
      DO 40 J = 1,NY
         CALL RCOPY (NX, IMAGE(LWIN(1),LWIN(2)+J-1), ARRAY(PTS))
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
 30         CONTINUE
 40      CONTINUE
      PTS = PTS - 1
C                               Look for multiple local maxima
      NNMPK = 0
      IF (MULTI) THEN
         IF (DORMSI) THEN
            SXMAX = IXMAX + LWIN(1) - 1
            SYMAX = IYMAX + LWIN(2) - 1
            CALL SADRMS (NXR, RMSIM, SXMAX, SYMAX, LNOISE)
            IF (LNOISE.EQ.FBLANK) GO TO 90
            END IF
C                               Loop over pixels excluding edges
         DO 80 J = 2,NY-1
            DO 70 I = 2,NX-1
C                               Quit if there are already MAXPK
               IF (NNMPK.LT.MAXPK) THEN
C                               Position in ARRAY
                  IPTS = (J-1)*NX + I
C                               Only count points above cutt
                  IF (ARRAY(IPTS).EQ.FBLANK) GO TO 68
                  IF (ARRAY(IPTS).LT.CUTT*LNOISE) GO TO 68
                  IF (ARRAY(IPTS).EQ.FBLANK) GO TO 68
C                               Bigger than surrounding points?
                  DO 60 IDY = -NX, NX, NX
                     DO 50 IDX = -1, 1, 1
                        IPT = IPTS + IDY + IDX
C                               Jump out if not a maximum
                        IF (ARRAY(IPT).NE.FBLANK) THEN
                           IF (ARRAY(IPTS).LT.ARRAY(IPT)) GO TO 68
                           END IF
 50                     CONTINUE
 60                  CONTINUE
C                               Quit if there are already MAXPK
               ELSE
                  GO TO 90
                  END IF
C                               This is an acceptable maximum
C                               but check that there's no adjacent
C                               point (can happen with 2 equal points)
               DO 65 KMPK = 1,NNMPK
                  IF ((ABS(XXPK(KMPK)-I).LE.1) .AND.
     *                (ABS(YYPK(KMPK)-J).LE.1)) GO TO 68
 65               CONTINUE
C                               Record relative position and flux
               NNMPK = NNMPK + 1
               XXPK(NNMPK) = I
               YYPK(NNMPK) = J
               SSPK(NNMPK) = ARRAY(IPTS)
C                               Jump here if you have good reason
C                               to believe this isn't a maximum
 68            CONTINUE
 70            CONTINUE
 80         CONTINUE
         END IF
C                                       rescale the image
 90   IPTS = 0
      RSCALE = 1.0
      IF (DMAX.NE.0.0) RSCALE = 5.0 / DMAX
      DO 110 J = 1,NY
         DO 100 I = 1,NX
            IPTS = IPTS + 1
            IF (ARRAY(IPTS).NE.FBLANK) ARRAY(IPTS) = ARRAY(IPTS) *
     *         RSCALE
 100        CONTINUE
 110     CONTINUE
      DMAX = DMAX * RSCALE
C                                       For single sources use 2nd
C                                       moments for starting estimates
      IF ((.NOT.MULTI) .OR. (NNMPK.LE.1)) THEN
         CALL SADDEF (ARRAY, IXMAX, IYMAX)
         NGAUSS = 1
C                                       Mutiple peaks: use 4 brightest
      ELSE
         NMPK = MIN (NNMPK, 4)
         DO 120 I = 1,NMPK
C                                       Find brightest remaining
            SMAX = ABS(SSPK(1))
            IND = 1
            DO 115 J = 2,NNMPK
               IF (ABS(SSPK(J)).GT.SMAX) THEN
                  IND = J
                  SMAX = ABS (SSPK(J))
                  END IF
 115           CONTINUE
            XPK(I) = XXPK(IND)
            YPK(I) = YYPK(IND)
            SPK(I) = SSPK(IND) * RSCALE
C                                       Drop this one
            SSPK(IND) = 0.0
 120        CONTINUE
         NGAUSS = NMPK
         CALL MULDEF (NMPK, XPK, YPK, SPK)
         END IF
      IER = 0
C
      RETURN
      END
      SUBROUTINE SCDMOM (DMAX, SUM2, SUMD2, SUM4, A, B, THETA, SINGUL)
C----------------------------------------------------------------------
C   From the various 2nd moments find the best least-squares quadratic
C   fit to the values near the peak.  The assumed form of the fit is
C      I = dmax - a*x*x - b*x*y - c*y*y
C   If there are too few points to make a fit, or if there are other
C   problems, SINGUL will be set to be .TRUE.
C   Inputs:
C      DMAX     R        Maximum in array
C      SUM2     R(0:2)   Vector of moments, SUM(I) is the sum over
C                        valid pixels of X**I * Y **(2-I)
C      SUMD2    R(0:2)   Sum of Flux * X**I * Y**(2-I)
C      SUM4     R(0:4)   Sum of X**I * Y**(4-I)
C   Outputs:
C      A        R        Estimate of major axis
C      B        R        Estimate of minor axis
C      THETA    R        Estimate of position angle (radians)
C      SINGUL   L        If .FALSE. couldn't find a decent solution
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
      IF ((SOL(0).LT.0.) .OR. (SOL(2).LT.0.) .OR.
     *   (SOL(1)**2.GE.4.*SOL(0)*SOL(2))) THEN
         SINGUL = .TRUE.
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
C   Set the beginning parameters for the fitting routine, using the
C   estimates based on moment fits of the top of the region.
C   Inputs
C      ARRAY    R(*)     Contains data we're trying to fit
C      IXMAX    I        X-coord of max in Array
C      IYMAX    I        Y-coord of max in Array
C   Common
C      DMAX     R        Value of max in Array
C      WIN      R(4)     Boundaries of search area
C      G        R(6,1)   Where to put estimates
C-----------------------------------------------------------------------
      REAL      ARRAY(*)
      INTEGER   IXMAX, IYMAX
C
      INTEGER   I, J, K, PTS, SUMPTS, IERR
      REAL      SUM2(0:2), SUMD2(0:2), SUM4(0:4), A, B, THETA, CRATIO,
     *   SLIT, X, Y, TEMP, SMCB(3)
      LOGICAL   SING
      DOUBLE PRECISION XD, XX(3)
      INCLUDE 'TVSAD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA CRATIO /0.6/
C-----------------------------------------------------------------------
C                                       BW smearing here
      XD = 0.0D0
      IF (((CORTYP(LOCNUM).EQ.1) .OR. (CORTYP(LOCNUM).EQ.2)) .AND.
     *   (BWS.GT.0.0)) THEN
         X = IXMAX + WIN(1) - 1
         Y = IYMAX + WIN(2) - 1
         CALL XYVAL (X, Y, XX(1), XX(2), XX(3), IERR)
         IF (IERR.NE.0) THEN
            XD = 0.0D0
         ELSE IF (CORTYP(LOCNUM).EQ.1) THEN
            XD = SIN (DG2RAD*XX(2)) * SIN (DG2RAD*XDEC) +
     *         COS (DG2RAD*XX(2)) * COS (DG2RAD*XDEC) *
     *         COS (DG2RAD * (XX(1) - XRA))
         ELSE
            XD = SIN (DG2RAD*XX(1)) * SIN (DG2RAD*XDEC) +
     *         COS (DG2RAD*XX(1)) * COS (DG2RAD*XDEC) *
     *         COS (DG2RAD * (XX(2) - XRA))
            END IF
         IF (XD.GT.1.0D0) THEN
            XD = 0.0D0
         ELSE
            XD = RAD2DG * ACOS (XD)
            END IF
         END IF
      X = IXMAX + WIN(1) - 1 - XCEN
      Y = IYMAX + WIN(2) - 1 - YCEN
      CALL BWSMCB (X, Y, XD, BWS, CB, SMCB)
C                                        For point source use clean beam
      IF (.NOT.GESWID) THEN
         G(4,1) = SMCB(1)
         G(5,1) = SMCB(2)
         G(6,1) = SMCB(3) * DG2RAD
C                                        Loop around, find a lot of
C                                        moments
      ELSE
         CALL RFILL (3, 0., SUM2)
         CALL RFILL (3, 0., SUMD2)
         CALL RFILL (5, 0., SUM4)
         SLIT = CRATIO * DMAX
         PTS = 0
         SUMPTS = 0
         DO 60 J = 1,NY
            DO 50 I = 1,NX
               PTS = PTS + 1
               IF ((ARRAY(PTS).NE.FBLANK) .AND. (ARRAY(PTS).GE.SLIT))
     *            THEN
                  X = I - IXMAX
                  Y = J - IYMAX
                  SUMPTS = SUMPTS + 1
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
         SING = SUMPTS.LE.8
         IF (.NOT.SING) CALL SCDMOM (DMAX, SUM2, SUMD2, SUM4, A, B,
     *      THETA, SING)
         IF (.NOT.SING) THEN
            G(4,1) = MAX (0.8*SMCB(1), MIN (4.0*SMCB(1), A))
            G(5,1) = MAX (0.8*SMCB(2), MIN (4.0*SMCB(2), B))
            G(6,1) = THETA
C                                        Didn't work, use point spread
C                                        fn.
         ELSE
            G(4,1) = SMCB(1)
            G(5,1) = SMCB(2)
            G(6,1) = SMCB(3) * DG2RAD
            END IF
         END IF
C                                        Fill in Peak flux and position
      G(1,1) = DMAX
      G(2,1) = IXMAX + WIN(1) - 1
      G(3,1) = IYMAX + WIN(2) - 1
C
      IF (.NOT.DOPA) G(6,1) = 0.0
C
 999  RETURN
      END
      SUBROUTINE MULDEF (NMPK, XPK, YPK, SPK)
C-----------------------------------------------------------------------
C   SADDAT has found NMPK, > 1 peaks above the cutoff.  Set the initial
C   guesses to be points at each peak.
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
      INTEGER   I, IERR
      REAL      X, Y, SMCB(3)
      DOUBLE PRECISION XD, XX(3)
      INCLUDE 'TVSAD.INC'
      INCLUDE 'INCS:DLOC.INC'
C----------------------------------------------------------------------
      DO 100 I = 1,NMPK
         G(1,I) = SPK(I)
         G(2,I) = XPK(I) + WIN(1) - 1
         G(3,I) = YPK(I) + WIN(2) - 1
C                                       BW smearing here
         XD = 0.0D0
         IF (((CORTYP(LOCNUM).EQ.1) .OR. (CORTYP(LOCNUM).EQ.2)) .AND.
     *      (BWS.GT.0.0)) THEN
            X = G(2,I)
            Y = G(3,I)
            CALL XYVAL (X, Y, XX(1), XX(2), XX(3), IERR)
            IF (IERR.NE.0) THEN
               XD = 0.0D0
            ELSE IF (CORTYP(LOCNUM).EQ.1) THEN
               XD = SIN (DG2RAD*XX(2)) * SIN (DG2RAD*XDEC) +
     *            COS (DG2RAD*XX(2)) * COS (DG2RAD*XDEC) *
     *            COS (DG2RAD * (XX(1) - XRA))
            ELSE
               XD = SIN (DG2RAD*XX(1)) * SIN (DG2RAD*XDEC) +
     *            COS (DG2RAD*XX(1)) * COS (DG2RAD*XDEC) *
     *            COS (DG2RAD * (XX(2) - XRA))
               END IF
            IF (XD.GT.1.0D0) THEN
               XD = 0.0D0
            ELSE
               XD = RAD2DG * ACOS (XD)
               END IF
            END IF
         X = G(2,I) - XCEN
         Y = G(3,I) - YCEN
         CALL BWSMCB (X, Y, XD, BWS, CB, SMCB)
         G(4,I) = SMCB(1)
         G(5,I) = SMCB(2)
         G(6,I) = SMCB(3) * DG2RAD
         IF (.NOT.DOPA) G(6,I) = 0.0
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SADRDS (IERR)
C-----------------------------------------------------------------------
C   SADRED prepares for redoing a fit trying 2 overlapping Gaussians
C   rather than a single one.
C   SAVE FOR THE MOMENT
C   Input:
C   Output:
C   Common In/out:
C      NGAUSS   I        Number gaussians set to NGAUSS + 1
C      FDATA    R(*)     Data to be fit
C      G        R(6,*)   In - old fit; out - new guess
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I, J, PTS, IFIL
      REAL      DATMAX, X, Y, SMCB(3)
      DOUBLE PRECISION XD, XX(3)
      INCLUDE 'TVSAD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
C                                       set ngauss, save old fit
      NGAUSS = 2
      CALL RCOPY (6, G(1,1), G(1,MAXGAU))
C                                       get real values of win from
C                                       filtab
      CALL FILNUM (LUN2, IFIL, IERR)
      WIN(1) = FILTAB(POBLC,   IFIL)
      WIN(2) = FILTAB(POBLC+1, IFIL)
      WIN(3) = FILTAB(POTRC,   IFIL)
      WIN(4) = FILTAB(POTRC+1, IFIL)
      NX = WIN(3) - WIN(1) + 1
      NY = WIN(4) - WIN(2) + 1
C                                       find maximum
      PTS = 0
      DATMAX = 0.
      DO 20 J = 1,NY
         DO 10 I = 1,NX
            PTS = PTS + 1
            IF ((FDATA(PTS).GT.DATMAX) .AND. (FDATA(PTS).NE.FBLANK))
     *         THEN
               DATMAX = FDATA(PTS)
               PTMAX = PTS
               END IF
 10         CONTINUE
 20      CONTINUE
C                                       1st = CB at max
      G(1,1) = 0.8 * DATMAX
      J = (PTMAX - 1) /NX + 1
      I = PTMAX - (J - 1) * NX
      G(2,1) = I + WIN(1) - 1
      G(3,1) = J + WIN(2) - 1
C                                       BW smearing here
      XD = 0.0D0
      IF (((CORTYP(LOCNUM).EQ.1) .OR. (CORTYP(LOCNUM).EQ.2)) .AND.
     *   (BWS.GT.0.0)) THEN
         X = G(2,1)
         Y = G(3,1)
         CALL XYVAL (X, Y, XX(1), XX(2), XX(3), IERR)
         IF (IERR.NE.0) THEN
            XD = 0.0D0
         ELSE IF (CORTYP(LOCNUM).EQ.1) THEN
            XD = SIN (DG2RAD*XX(2)) * SIN (DG2RAD*XDEC) +
     *         COS (DG2RAD*XX(2)) * COS (DG2RAD*XDEC) *
     *         COS (DG2RAD * (XX(1) - XRA))
         ELSE
            XD = SIN (DG2RAD*XX(1)) * SIN (DG2RAD*XDEC) +
     *         COS (DG2RAD*XX(1)) * COS (DG2RAD*XDEC) *
     *         COS (DG2RAD * (XX(2) - XRA))
            END IF
         IF (XD.GT.1.0D0) THEN
            XD = 0.0D0
         ELSE
            XD = RAD2DG * ACOS (XD)
            END IF
         END IF
      X = G(2,1) - XCEN
      Y = G(3,1) - YCEN
      CALL BWSMCB (X, Y, XD, BWS, CB, SMCB)
      G(4,1) = SMCB(1)
      G(5,1) = SMCB(2)
      G(6,1) = SMCB(3) * DG2RAD
      CALL RCOPY (3, G(4,1), G(4,2))
C                                       separated
      IF (ABS(G(2,1)-G(2,MAXGAU))+ABS(G(3,1)-G(3,MAXGAU)).GT.1.5) THEN
         G(1,2) = 0.8 * G(1,MAXGAU)
         G(2,2) = 2. * G(2,MAXGAU) - G(2,1)
         G(3,2) = 2. * G(3,MAXGAU) - G(3,1)
         IF (GESWID) THEN
            G(4,2) = (SMCB(1) + G(4,MAXGAU)) / 2.0
            G(5,2) = (SMCB(2) + G(5,MAXGAU)) / 2.0
            G(6,2) = (SMCB(3) * DG2RAD + G(6,MAXGAU)) / 2.0
            END IF
C                                       core-halo ?
      ELSE
         G(1,1) = 0.8 * G(1,1)
         G(1,2) = DATMAX - G(1,1)
         G(2,2) = 2. * G(2,MAXGAU) - G(2,1)
         G(3,2) = 2. * G(3,MAXGAU) - G(3,1)
         IF (GESWID) THEN
            G(4,2) = 2 * SMCB(1)
            G(5,2) = 2 * SMCB(2)
            G(6,2) = SMCB(3) * DG2RAD
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE SADRED (IERR)
C-----------------------------------------------------------------------
C   SADRED prepares for redoing a fit trying N+1 overlapping Gaussians
C   rather than a single one.
C   Input:
C   Output:
C   Common In/out:
C      NGAUSS   I        Number gaussians set to NGAUSS + 1
C      FDATA    R(*)     Data to be fit
C      G        R(6,*)   In - old fit; out - new guess
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      REAL      X, Y, SMCB(3)
      DOUBLE PRECISION XD, XX(3)
      INCLUDE 'TVSAD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
C                                       set ngauss, save old fit
      NGAUSS = NGAUSS + 1
C                                       1st = strongest assumed
      CALL RCOPY (6, G(1,1), G(1,NGAUSS))
      G(1,1) = 0.8 * G(1,1)
C                                       BW smearing here
      XD = 0.0D0
      IF (((CORTYP(LOCNUM).EQ.1) .OR. (CORTYP(LOCNUM).EQ.2)) .AND.
     *   (BWS.GT.0.0)) THEN
         X = G(2,1)
         Y = G(3,1)
         CALL XYVAL (X, Y, XX(1), XX(2), XX(3), IERR)
         IF (IERR.NE.0) THEN
            XD = 0.0D0
         ELSE IF (CORTYP(LOCNUM).EQ.1) THEN
            XD = SIN (DG2RAD*XX(2)) * SIN (DG2RAD*XDEC) +
     *         COS (DG2RAD*XX(2)) * COS (DG2RAD*XDEC) *
     *         COS (DG2RAD * (XX(1) - XRA))
         ELSE
            XD = SIN (DG2RAD*XX(1)) * SIN (DG2RAD*XDEC) +
     *         COS (DG2RAD*XX(1)) * COS (DG2RAD*XDEC) *
     *         COS (DG2RAD * (XX(2) - XRA))
            END IF
         IF (XD.GT.1.0D0) THEN
            XD = 0.0D0
         ELSE
            XD = RAD2DG * ACOS (XD)
            END IF
         END IF
      X = G(2,1) - XCEN
      Y = G(3,1) - YCEN
      CALL BWSMCB (X, Y, XD, BWS, CB, SMCB)
      G(1,NGAUSS) = 0.25 * G(1,1)
      G(4,NGAUSS) = 1.1 * SMCB(1)
      G(5,NGAUSS) = 1.1 * SMCB(2)
C
 999  RETURN
      END
      SUBROUTINE SADVST (VALVAR)
C-----------------------------------------------------------------------
C   SADVST is a subroutine for SAD which stores the variable parameters
C   in the proper arrays.  In particular the values of the G array that
C   are not held fixed are packed into VALVAR, and NVAR is set to the
C   number of nonfixed parameters.  The E array is set -1.0 for fixed
C   variables and +1.0 for variables.
C   Inputs:  none  see COMMONs
C   Outputs:
C      NVAR            I       The number of variables
C      VALVAR(*)      D       A list of initial guesses of
C                              non-fixed parameters
C   Commons:
C      JVAR            I       Pointers telling which parameters are
C                              are in VALVAR
C      IVAR            I       Pointers to which gaussians are in VALVAR
C      E(6,*)          R       >0. -> variable, <0. -> fixed parameter
C-----------------------------------------------------------------------
      DOUBLE PRECISION VALVAR(*)
C
      INTEGER   I, J, J2
      INCLUDE 'TVSAD.INC'
C-----------------------------------------------------------------------
C                                       Initialize
      NVAR = 0
C                                       Move flux and position estimates
      J2 = 3
C                                       Move BMAJ, BMIN and BPA
      IF (.NOT.DOPNT) THEN
         J2 = 5
         IF (DOPA) J2 = 6
         END IF
C                                       move the parameters
      DO 20 I = 1,NGAUSS
         CALL RFILL (6, -1.0, E(1,I))
         DO 10 J = 1,J2
            NVAR         = NVAR + 1
            VALVAR(NVAR) = G(J,I)
            JVAR(NVAR)   = J
            IVAR(NVAR)   = I
            E(J,I)       = 1.0
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SADHI (NPKMAX, NPK, NGOOD, IER)
C-----------------------------------------------------------------------
C   SADHI creates and writes the HI file for the task SAD, also gets the
C   max and min in the residual file
C   Inputs from common - users adverb values
C   Outputs:
C      IER            I     Error return  0->okay
C                             1->uh-oh
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   IER, IERR, NHISTF, LHIN, LHOUT, IBUFF2(256), ITEMP(7),
     *   I, NPK, NPKMAX, NGOOD, IX, IY
      REAL      RMAX, RMIN, BLC(7), TRC(7)
      LOGICAL   T
      INCLUDE 'TVSAD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NHISTF, LHIN, LHOUT /2,27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      CALL RFILL (7, 1.0, BLC)
      CALL RFILL (7, 0.0, TRC)
      CALL MAPWIN (LUN2, BLC, TRC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'SET WINDOW'
         GO TO 900
         END IF
C                                       Find max, min
      RMAX = -1.0E20
      RMIN = -RMAX
      NX = OUTBLK(KINAX)
      NY = OUTBLK(KINAX+1)
      DO 20 IY = 1,NY
         CALL MAPIO ('READ', LUN2, FDATA, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING RESIDUAL'
            GO TO 900
            END IF
         DO 10 IX = 1,NX
            IF (FDATA(IX).NE.FBLANK) THEN
               RMAX = MAX (RMAX, FDATA(IX))
               RMIN = MIN (RMIN, FDATA(IX))
               END IF
 10         CONTINUE
 20      CONTINUE
      OUTR(KRDMX) = RMAX
      OUTR(KRDMN) = RMIN
C                                       Copy MF file
      CALL CATDIR ('CSTA', OUTDSK, OUTSL, OUTNAM, OUTCLS, OUTSEQ,
     *   INTYPE, NLUSER, 'CLRD', SCRBUF, IERR)
      IF (IERR.EQ.10) IERR = 0
      CALL TABCOP ('MF', MFVER, 0, LHIN, LHOUT, INDISK, OUTDSK, INSL,
     *   OUTSL, OUTBLK, MFBUF, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1005) IERR
         CALL MSGWRT (7)
         END IF
C                                       Initialize HI
      CALL HIINIT (NHISTF)
      IER = 0
C                                       copy keywords
      CALL KEYPCP (INDISK, INSL, OUTDSK, OUTSL, 0, ' ', IERR)
C                                       Create and open output HI file
      CALL HISCOP (LHIN, LHOUT, INDISK, OUTDSK, INSL, OUTSL, OUTBLK,
     *   SCRBUF, IBUFF2, IERR)
      IF (IERR.LE.2) GO TO 50
         WRITE (MSGTXT,1010) IERR
         GO TO 900
C                                       Add new HI entries
C                                       Input name
 50   CALL HENCO1 (TSKNAM, INNAME, INCLAS, INSEQ, INDISK, LHOUT,
     *   IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 950
C                                       noise image
      IF (LUNR.GT.0) THEN
         HILINE = TSKNAM // '/ image used to define noise'
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL HENCO2 (TSKNAM, IN2NAM, IN2CLS, IN2SEQ, IN2DSK, LHOUT,
     *      IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
C                                       Output name
      CALL HENCOO (TSKNAM, OUTNAM, OUTCLS, OUTSEQ, OUTDSK, LHOUT,
     *   IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 950
C                                        Rest of inputs
      DO 60 I = 1,7
         ITEMP(I) = XBLC(I) + 0.5
 60      CONTINUE
      WRITE (HILINE,1060) TSKNAM, ITEMP
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 950
      DO 70 I = 1,7
         ITEMP(I) = XTRC(I) + 0.5
 70      CONTINUE
      WRITE (HILINE,1070) TSKNAM, ITEMP
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 950
      WRITE (HILINE,1080) TSKNAM, NPKMAX, NPK, NGOOD
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 950
      WRITE (HILINE,1090) TSKNAM, CUTT, UNIT
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 950
      IF (DOPNT) THEN
         HILINE = TSKNAM // '/ fitted and removed point sources only'
      ELSE IF (GESWID) THEN
         HILINE = TSKNAM // '/ guessed & fitted component widths'
      ELSE
         HILINE = TSKNAM // '/ fitted component widths from points'
         END IF
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 950
      IF (MULTI) THEN
         WRITE (HILINE,1100) TSKNAM, RCUT, GAIN
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
      WRITE (HILINE,1111) TSKNAM, DPARM(1), UNIT
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 950
      WRITE (HILINE,1112) TSKNAM, DPARM(2)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 950
      IF (DPARM(3).LT.9999.) THEN
         WRITE (HILINE,1113) TSKNAM, DPARM(3), GAIN
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
      IF (DPARM(4).LT.9999.) THEN
         WRITE (HILINE,1114) TSKNAM, DPARM(4)
         CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
      WRITE (HILINE,1115) TSKNAM, DPARM(5)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 950
      WRITE (HILINE,1116) TSKNAM, DPARM(6)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 950
      WRITE (HILINE,1117) TSKNAM, DPARM(7), GAIN
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 950
      GO TO 950
C
 900  IER = 1
      CALL MSGWRT (8)
C                                       Close HI file
 950  CALL HICLOS (LHOUT, T, IBUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SADHI: ERROR',I4,' DOING ',A)
 1005 FORMAT ('CANNOT COPY MF FILE TO OUTPUT RESIDUAL IMAGE, ERROR',I5)
 1010 FORMAT ('CANNOT COPY HI FILE.  IER=',I8)
 1060 FORMAT (A6,'BLC=',7I6)
 1070 FORMAT (A6,'TRC=',7I6)
 1080 FORMAT (A,'/ MAX SOURCES',I7,' ISLANDS FOUND',I7,
     *   ' SOURCES FOUND',I7)
 1090 FORMAT (A,'CUTOFF =',F11.6,3X,'/ search limit in ',A)
 1100 FORMAT (A,'ICUT = ',F11.6,' GAIN =',F6.3,2X,'/ Retry level/gain')
 1111 FORMAT (A,'DPARM(1) =',F9.6,3X,'/ Reject peaks below this ',A)
 1112 FORMAT (A,'DPARM(2) =',F9.6,3X,'/ Reject fluxes below this')
 1113 FORMAT (A,'DPARM(3) =',F9.6,' GAIN =',F6.3,3X,
     *   '/ Reject rms above this')
 1114 FORMAT (A,'DPARM(4) =',F6.1,3X,'/ Reject widths above this')
 1115 FORMAT (A,'DPARM(5) =',F6.1,3X,'/ Reject more than this outside')
 1116 FORMAT (A,'DPARM(6) =',F6.1,3X,
     *   '/ Reject more than this off image')
 1117 FORMAT (A,'DPARM(7) =',F9.6,' GAIN =',F6.3,3X,
     *   '/ Reject resid flux above this')
      END
      SUBROUTINE SADOUT (NXR, RMSIM, LPK, IER)
C-----------------------------------------------------------------------
C   SADOUT is a subroutine of SAD which prints out the parameter
C   solutions and converts them into useful coordinates.
C   Inputs:
C      NXR     I        X dimension of RMSimage
C      RMSIM   R(*)     Image of RMSes
C      LPK     I        How many sources - all valid
C   From commons:
C      DOCC    I        > 0. -> add CC list to input file,
C                          > 1.5-> write convolved CCs even
C                          if deconvolving is possible
C      DOCRT   R        > 0 to CRT, < 0 to line printer, 0 nowhere
C      DOPNT   L        If .true. we fit point sources so
C                          only put points in CC file, don't deconvolve
C      NOCLN   L        There is no clean beam information available
C      GLIST   R(6,*)   Fit parameter estimates
C   Also:
C      All kinds of coordinate information in DLOC
C   Outputs:
C      IER     I        Error return  0-> okay
C                          1-> error
C-----------------------------------------------------------------------
      INTEGER   NXR, LPK, IER
      REAL      RMSIM(NXR,*)
C
      INCLUDE 'TVSAD.INC'
      INCLUDE 'INCS:PMFC.INC'
      CHARACTER PREFIX(0:2)*5, MARK(6)*1, TSTR*27, CMARK*1, DMARK*1,
     *   ARRAY*8, XUNIT*8, RSTR*20, PMARK(2,2), STRING*24, RESOLV(4)*1
      INTEGER   IERR, I, J, K, NREC, LREC, CILUN, CIBUF(768), IRNO,
     *   LEVMIN, IROUND, KEY1, KEY2, IPK, IUS, IMARK, I1, I2, ITRIM,
     *   IRNO1, IRNO2, IBLC(7), ITRC(7), IP(MAXFND+2), ITITLE(8),
     *   ILEN, JMARK, STBUF(512), STKOLS(7), STNUMV(7), STTYPE, STVER,
     *   LUNTMP, STLUN, ISTRNO, RPT
      LOGICAL   TRYCON, DOVLB, CRIT1, CRIT2, WARN(MAXFND), WARNIN,
     *   WASWRN, OUTSID, HEADER
      REAL      TEMP, CAXINC(2), DCONV(3,3), RECORD(7), SINC, COSC,
     *   RICORD(7), XXT, YYT, ITOT, ITOTE, SMAX, SMIN, SCALE,
     *   ROW(NUMCOL), TCUT, RTITLE(8), BMAJS, BMINS, BPAS, ERRMAJ,
     *   ERRMIN, ERRFI, ERRA, A, X0, Y0, IMAJ, IMIN, FI,BMAJ, BMIN,
     *   ARBEAM, ARIMAG, LN, FRERRA, ERRX0, ERRY0, DX, DY, SMCB(3),
     *   BMFACT, DLFACT, ACTRMS, ARHEAD, STWID(3), RWK(4,MAXFND+2)
      DOUBLE PRECISION    WK(2,MAXFND+2), X(3), XD, LAMBDA, STXY(2)
      HOLLERITH HTITLE(8)
      INCLUDE 'ORFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (HTITLE, ITITLE, RTITLE), (WK, RWK)
      DATA PREFIX /'     ','milli','micro'/
      DATA MARK /' ', '*', '!', 'H', 'L', 'S'/
      DATA PMARK /'(',')','!','!'/
      DATA RESOLV /'U','?','R',' '/
C-----------------------------------------------------------------------
C                                        Initialization
      IER = 0
      TRYCON = (.NOT.(NOCLN .OR. DOPNT)) .AND. (DOPA)
      WASWRN = .FALSE.
      CALL H2CHR (8, 1, CATH(KHTEL), ARRAY)
C                                       stars file?
      CALL FNDEXT ('ST', INBLK, I)
      STVER = -1
      IF (XSTVER.GE.0.0) THEN
         CALL CATDIR ('CSTA', INDISK, INSL, INNAME, INCLAS, INSEQ,
     *      INTYPE, NLUSER, 'CLRD', SCRBUF, IERR)
         IF (IERR.EQ.10) IERR = 0
         IF (IERR.EQ.0) CALL CATIO ('READ', INDISK, INSL, CATBLK,
     *      'WRIT', SCRBUF, IERR)
         STVER = XSTVER + 0.5
         IF ((STVER.EQ.0) .OR. (STVER.GT.I)) STVER = I + 1
         STLUN = LUNTMP (1)
         CALL STINI ('WRITE', STBUF, INDISK, INSL, STVER, CATBLK, STLUN,
     *      ISTRNO, STKOLS, STNUMV, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'CANNOT OPEN ST FILE FOR OUTPUT'
            CALL MSGWRT (7)
            STVER = -1
         ELSE
            WRITE (MSGTXT,1005) 'ST', STVER
            CALL MSGWRT (4)
            END IF
         STTYPE = 3
         CALL CATDIR ('CSTA', INDISK, INSL, INNAME, INCLAS, INSEQ,
     *      INTYPE, NLUSER, 'CLWR', SCRBUF, IERR)
         IF (IERR.EQ.10) IERR = 0
         END IF
C                                       find frequency
      CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATBLK(KHCTP), FREQAX,
     *   IERR)
      IF (IERR.EQ.0) THEN
         LAMBDA = VELITE / MAX (1.D6, CATD(KDCRV+FREQAX))
      ELSE
         PBPARM(1) = 0.0
         END IF
C                                       Create CC file
      IF (DOCC.GE.0) THEN
         CILUN = 27
         NREC = 10
         IF (DOPNT) THEN
            LREC = 3
         ELSE
            LREC = 7
            END IF
C                                       Open with input image
         CALL CATDIR ('CSTA', INDISK, INSL, INNAME, INCLAS, INSEQ,
     *      INTYPE, NLUSER, 'CLRD', SCRBUF, IERR)
         IF (IERR.EQ.10) IERR = 0
         IF (IERR.EQ.0) CALL CATIO ('READ', INDISK, INSL, CATBLK,
     *      'WRIT', SCRBUF, IERR)
         IF (IERR.NE.0) THEN
            DOCC = - 1
            WRITE (MSGTXT,1000) IERR
            END IF
         END IF
      IF (DOCC.GE.0) THEN
         CALL CCINI (CILUN, LREC, INDISK, INSL, DOCC, CATBLK, CIBUF,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1001) DOCC
            CALL MSGWRT (6)
            END IF
         CALL CATDIR ('CSTA', INDISK, INSL, INNAME, INCLAS, INSEQ,
     *      INTYPE, NLUSER, 'CLWR', SCRBUF, IERR)
         IF (IERR.EQ.10) IERR = 0
         END IF
C                                       Find max and min flux
      SMAX = -1.E20
      SMIN = -SMAX
      DO 10 I = 1,LPK
         SMAX = MAX (SMAX, ABS(GLIST(1,I)))
         SMIN = MIN (SMIN, ABS(GLIST(1,I)))
 10      CONTINUE
C                                       scale minimum so that
C                                       1. < smin < 1000.
C                                       Max rescaling is 10**6
      IF (SMIN.LT.5*RMS) SMIN = RMS
      IF ((SMIN.GE.0.0002) .AND. (SMIN.LT.0.2)) THEN
         LEVMIN = 1
      ELSE IF ((SMIN.GE.0.2E-6) .AND. (SMIN.LT.0.2E-3)) THEN
         LEVMIN = 2
      ELSE
         LEVMIN = 0
         END IF
      SMIN = 10.**(3*LEVMIN)
C                                       OR print out
      REFRA = ' '
      REFDEC = ' '
      IF ((DOCRT.LT.-3.5) .AND. (AXTYP(LOCNUM).EQ.1)) THEN
         CALL XYVAL (CATR(KRCRP), CATR(KRCRP+1), X(1), X(2), X(3), IERR)
         IF (IERR.EQ.0) THEN
            CALL AXSTRN (CTYP(1,LOCNUM), X(1), 0, ILEN, RSTR)
            REFRA = RSTR(4:17)
            CALL AXSTRN (CTYP(2,LOCNUM), X(2), 1, ILEN, RSTR)
            IF (RSTR(5:5).EQ.'-') THEN
               REFDEC = RSTR(5:18)
            ELSE
               REFDEC = RSTR(4:17)
               END IF
            END IF
         END IF
C                                       Begin printing - set titles...
      HEADER = (DOCRT.NE.0.0) .AND. (DOCRT.GT.-2.5)
      IERR = 0
      IF (HEADER) THEN
         WRITE (TITL1,1010) INNA(:12), INNA(13:18), INNA(21:27),
     *      INNA(28:29), PLANE, INNA(30:36)
         TITL2 = ' '
         LINE = ' '
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
C                                       Window
      IF (HEADER) THEN
         DO 20 I = 1,7
            IBLC(I) = IROUND (XBLC(I))
            ITRC(I) = IROUND (XTRC(I))
 20         CONTINUE
         WRITE (LINE,1015) IBLC, ITRC
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
C                                       Flux limit
      IF (HEADER) THEN
         WRITE (LINE,1016) CUTT, UNIT
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
C                                       retry limit
      IF ((MULTI) .AND. (HEADER)) THEN
         WRITE (LINE,1017) RCUT, UNIT, GAIN
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
C                                       rms image
      IF ((HEADER) .AND. (DPARM(9).GT.0.0)) THEN
         WRITE (LINE,1018) IN2NAM, IN2CLS, IN2SEQ, IN2DSK
         CALL DEFRMT (LINE, '_', I)
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
C                                       Peak reject limit
      IF (HEADER) THEN
         WRITE (LINE,1021) DPARM(1), UNIT
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
C                                       Peak reject limit
      IF (HEADER) THEN
         WRITE (LINE,1022) DPARM(2)
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
C                                       Rms reject limit
      IF ((HEADER) .AND. (DPARM(3).LT.9999.)) THEN
         WRITE (LINE,1023) DPARM(3), UNIT, GAIN
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
C                                       Width reject limit
      IF ((HEADER) .AND. (DPARM(4).LT.9999.)) THEN
         WRITE (LINE,1024) DPARM(4)
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
C                                       Outside image reject limit
      IF (HEADER) THEN
         WRITE (LINE,1025) DPARM(5)
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
C                                       Outside image reject limit
      IF (HEADER) THEN
         WRITE (LINE,1026) DPARM(6)
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
C                                       Residual flux limit
      IF (HEADER) THEN
         WRITE (LINE,1027) DPARM(7), GAIN
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
C                                       Tell user about units/scaling
      IF (HEADER) THEN
         CALL H2CHR (8, 1, CATH(KHBUN), XUNIT)
         LINE = 'Fluxes expressed in units of ' // PREFIX(LEVMIN) //
     *      XUNIT
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
C                                      Warn about possible overflow
      IF (HEADER) THEN
         LINE = 'NOTE: Fluxes marked by * have been divided by 1000.'
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
      IF (HEADER) THEN
         IF (RMS.GT.1.E-3) THEN
            WRITE (LINE,1028) RMS
         ELSE
            RMS = RMS * 1.E6
            WRITE (LINE,1029) RMS
            RMS = RMS / 1.E6
            END IF
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
C                                       give reference coordinate
      DOVLB = (ABS(AXINC(1,LOCNUM)).LT.1.0/3.6E4) .AND.
     *   (ABS(AXINC(2,LOCNUM)).LT.1.0/3.6E4) .AND. (DOPA)
      I1 = ITRIM (CTYP(1,LOCNUM))
      I2 = ITRIM (CTYP(2,LOCNUM))
      IF (DOVLB) THEN
         I1 = MIN (I1, 10)
         I2 = MIN (I2, 10)
      ELSE
         I1 = MIN (I1, 13)
         I2 = MIN (I2, 13)
         END IF
C                                       RA and DEC
      IF (HEADER) THEN
         CALL XYVAL (RPLOC(1,LOCNUM), RPLOC(2,LOCNUM), X(1), X(2), X(3),
     *      IERR)
         IF (DOPA) THEN
            CALL SADSTR (X(1), X(2), TSTR)
            WRITE (LINE,1030) TSTR
         ELSE
            WRITE (LINE,1031) X(1), X(2)
            END IF
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
      IF ((DOPA) .AND. (HEADER)) THEN
         IF (DOVLB) THEN
            LINE = 'All source widths and coordinates and their errors'
     *         // ' are in arc seconds'
         ELSE
            LINE = 'All source widths, width errors, and both'
     *         // ' coordinate errors are in arc seconds'
            END IF
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
      IF (HEADER) THEN
         IF (BWS.GT.0.0) THEN
            WRITE (LINE,1035) BWS
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, ILINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) HEADER = .FALSE.
            END IF
         IF (BWCORR) THEN
            LINE = 'Source peak fluxes also corrected for this' //
     *         ' bandwidth smearing'
         ELSE IF (BWS.GT.0.) THEN
            LINE = 'Source peak fluxes NOT corrected for this'
     *         // ' bandwidth smearing'
         ELSE
            LINE = 'NO corrections for bandwidth smearing' //
     *         ' have been made'
            END IF
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF

      IF (HEADER) THEN
         IF (PBPARM(1).GT.0.0) THEN
            LINE = 'All source peaks and fluxes corrected for' //
     *         ' primary beam'
         ELSE
            LINE = 'Source peaks and fluxes NOT corrected for' //
     *         ' primary beam'
            END IF
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
      IF (HEADER) THEN
         LINE = ' '
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, ILINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) HEADER = .FALSE.
         END IF
      IF (IERR.NE.0) DOCRT = 0.0
C                                       Create header
      IF (DOCRT.NE.0.0) THEN
C                                       RA-Declination VLB accuracy
         IF (DOVLB) THEN
            IF (DOPNT) THEN
               TITL2 = '    #      Flux     Dflux   '
               TITL2(38-I1:38) = CTYP(1,LOCNUM)(:I1)
               TITL2(50-I2:49) = CTYP(2,LOCNUM)(:I2)
               TITL2(53:54) = 'Dx'
               TITL2(61:62) = 'Dy'
            ELSE IF (NOCLN) THEN
               IF (NACROS.GE.112) THEN
                  TITL2 = '    #      Flux     Dflux   '
                  TITL2(39-I1:38) = CTYP(1,LOCNUM)(:I1)
                  TITL2(50-I2:49) = CTYP(2,LOCNUM)(:I2)
                  TITL2(53:54) = 'Dx'
                  TITL2(61:62) = 'Dy'
                  TITL2(70:) = 'Maj     Min      PA    Dmaj' //
     *               '    Dmin    Dpa'
               ELSE
                  TITL2 = '    #      Flux'
                  TITL2(30-I1:29) = CTYP(1,LOCNUM)(:I1)
                  TITL2(39-I2:40) = CTYP(2,LOCNUM)(:I2)
                  TITL2(45:) = 'Maj     Min      PA'
                  END IF
            ELSE
               IF (NACROS.GE.128) THEN
                  TITL2 = '    #      Peak    Dpeak     Flux    Dflux'
                  TITL2(55-I1:54) = CTYP(1,LOCNUM)(:I1)
                  TITL2(66-I2:65) = CTYP(2,LOCNUM)(:I2)
                  TITL2(69:70) = 'Dx'
                  TITL2(77:78) = 'Dy'
                  TITL2(86:) = 'Maj     Min      PA    Dmaj' //
     *               '    Dmin    Dpa'
               ELSE
                  TITL2 = '    #      Peak      Flux'
                  TITL2(39-I1:38) = CTYP(1,LOCNUM)(:I1)
                  TITL2(50-I2:49) = CTYP(2,LOCNUM)(:I2)
                  TITL2(54:) = 'Maj     Min     PA'
                  END IF
               END IF
C                                       RA-Declination normal string
         ELSE IF (DOPA) THEN
            IF (DOPNT) THEN
               TITL2 = '    #      Flux     Dflux   '
               TITL2(42-I1:41) = CTYP(1,LOCNUM)(:I1)
               TITL2(56-I2:55) = CTYP(2,LOCNUM)(:I2)
               TITL2(60:61) = 'Dx'
               TITL2(68:69) = 'Dy'
            ELSE IF (NOCLN) THEN
               IF (NACROS.GE.115) THEN
                  TITL2 = '    #      Flux     Dflux   '
                  TITL2(42-I1:41) = CTYP(1,LOCNUM)(:I1)
                  TITL2(56-I2:55) = CTYP(2,LOCNUM)(:I2)
                  TITL2(60:61) = 'Dx'
                  TITL2(68:69) = 'Dy'
                  TITL2(78:) = 'Maj    Min    PA    Dmaj' //
     *               '   Dmin   Dpa'
               ELSE IF (NACROS.GE.102) THEN
                  TITL2 = '    #      Flux  Dflux   '
                  TITL2(38-I1:37) = CTYP(1,LOCNUM)(:I1)
                  TITL2(52-I2:51) = CTYP(2,LOCNUM)(:I2)
                  TITL2(55:56) = 'Dx'
                  TITL2(62:63) = 'Dy'
                  TITL2(71:) = 'Maj    Min   PA  Dmaj  Dmin Dpa'
               ELSE
                  TITL2 = '    #      Flux'
                  TITL2(34-I1:33) = CTYP(1,LOCNUM)(:I1)
                  TITL2(48-I2:47) = CTYP(2,LOCNUM)(:I2)
                  TITL2(53:) = 'Maj     Min     PA'
                  END IF
            ELSE
               IF (NACROS.GE.132) THEN
                  TITL2 = '    #      Peak    Dpeak     Flux    Dflux'
                  TITL2(58-I1:57) = CTYP(1,LOCNUM)(:I1)
                  TITL2(72-I2:71) = CTYP(2,LOCNUM)(:I2)
                  TITL2(76:77) = 'Dx'
                  TITL2(84:85) = 'Dy'
                  TITL2(94:) = 'Maj     Min     PA    Dmaj' //
     *               '   Dmin  Dpa'
               ELSE
                  TITL2 = '    #       Peak    Flux'
                  TITL2(39-I1:38) = CTYP(1,LOCNUM)(:I1)
                  TITL2(53-I2:52) = CTYP(2,LOCNUM)(:I2)
                  TITL2(56:) = 'Maj     Min    PA'
                  END IF
               END IF
C                                    non RA-dec coordinates
         ELSE
            I1 = MIN (I1, 12)
            I2 = MIN (I2, 12)
            IF (DOPNT) THEN
               TITL2 = '    #    Flux     Dflux   '
               TITL2(40-I1:39) = CTYP(1,LOCNUM)(:I1)
               TITL2(52-I2:51) = CTYP(2,LOCNUM)(:I2)
               TITL2(54:55) = 'Dx'
               TITL2(64:65) = 'Dy'
C                                       NOCLN true when DOPA false
            ELSE
               IF (NACROS.GE.116) THEN
                  TITL2 = '    #     Flux     Dflux   '
                  TITL2(40-I1:39) = CTYP(1,LOCNUM)(:I1)
                  TITL2(52-I2:51) = CTYP(2,LOCNUM)(:I2)
                  TITL2(54:55) = 'Dx'
                  TITL2(64:65) = 'Dy'
                  TITL2(78:) = 'Maj       Min      Dmaj     Dmin'
               ELSE IF (NACROS.GE.99) THEN
                  TITL2 = '    #     Flux     Dflux   '
                  TITL2(37-I1:36) = CTYP(1,LOCNUM)(:I1)
                  TITL2(48-I2:47) = CTYP(2,LOCNUM)(:I2)
                  TITL2(50:51) = 'Dx'
                  TITL2(59:60) = 'Dy'
                  TITL2(71:) = 'Maj     Min     Dmaj    Dmin'
               ELSE
                  TITL2 = '    #      Flux '
                  TITL2(31-I1:30) = CTYP(1,LOCNUM)(:I1)
                  TITL2(44-I2:42) = CTYP(2,LOCNUM)(:I2)
                  TITL2(48:) = 'Maj       Min'
                  END IF
               END IF
            END IF
C                                       write the header
         IF (DOCRT.GT.-3.5) THEN
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *         TITL2, ILINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) DOCRT = 0.0
            END IF
         END IF
C                                       Update the Keyword
      XUNIT = 'REALRMS'
      CALL CHR2H (8, XUNIT, 1, HTITLE)
      RTITLE(3) = RMS
      ITITLE(4) = 0
      ITITLE(5) = 2
      CALL TABIO ('WRIT', 5, 7, HTITLE, MFBUF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Sort output
      IF (SORT.EQ.'Y') THEN
         KEY1 = 3
      ELSE IF (SORT.EQ.'S') THEN
         KEY1 = 1
      ELSE IF (SORT.EQ.'X') THEN
         KEY1 = 2
      ELSE IF (SORT.EQ.'D') THEN
         KEY1 = 5
      ELSE
         KEY1 = 4
         END IF
      KEY2 = KEY1
      IF (KEY1.LT.4) THEN
         CALL OSORTI (GLIST, LPK, LPK+2, KEY1, KEY2, 6, RWK, IP, IER)
      ELSE
         CALL PSORTI (GLIST, LPK, LPK+2, KEY1, 6, WK, IP, IER)
         END IF
      IPK = 0
      IRNO1 = MFBUF(5) + 1
C                                       Loop over sources
      DO 200 IUS = 1,LPK
         BMFACT = 1.0
         DLFACT = 1.0
C                                       SORT; S descending,
C                                       ascending
         IF (((KEY1.EQ.2) .AND. (AXINC(1,LOCNUM).LT.0.)) .OR.
     *      ((KEY1.EQ.3) .AND. (AXINC(2,LOCNUM).LT.0.)) .OR.
     *      (KEY1.EQ.1)) THEN
            I = IP(LPK + 2 -IUS)
         ELSE
            I = IP(IUS+1)
            END IF
C                                       Fix position angle
         GLIST(6,I) = GLIST(6,I) * RAD2DG - 90.0
         IF (GLIST(6,I).GE.180.0) GLIST(6,I) = GLIST(6,I) - 180.0
         IF (GLIST(6,I).LT.0.0) GLIST(6,I) = GLIST(6,I) + 180.0
C                                       Skip if flagged
         ROW(COLPLN) = PLANE
         CALL RFILL (32, 0.0, ROW(COLPEK))
         CALL RCOPY (4, GLIST(2,I), ROW(COPCEX))
         ROW(COPPAN) = GLIST(6,I)
         IF (ROW(COPPAN).GE.180.0) ROW(COPPAN) = ROW(COPPAN) - 180.
         IF (ROW(COPPAN).LT.0.0) ROW(COPPAN) = ROW(COPPAN) + 180.
         IF (.NOT.DOPNT) ROW(COLTYP) = 1.0
         ROW(CORRMS) = PLIST(1,I)
         ROW(CORPEK) = MAX (ABS(PLIST(2,I)), ABS(PLIST(3,I)))
         ROW(CORFLX) = PLIST(4,I)
         IPK = IPK + 1
C                                        fix diameters
         GLIST(4,I) = ABS (GLIST(4,I))
         GLIST(5,I) = ABS (GLIST(5,I))
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
         SINC = SIN (DG2RAD*GLIST(6,I))
         COSC = COS (DG2RAD*GLIST(6,I))
C                                       Compute errors
C                                       implement better designation fo
C                                       the fitting parameters
         A = GLIST(1,I)
         X0 = GLIST(2,I)
         Y0 = GLIST(3,I)
         IMAJ = GLIST(4,I)
         IMIN = GLIST(5,I)
         FI = GLIST(6,I)
         BMAJ = CB(1)
         BMIN = CB(2)
         ARBEAM = BMAJ*BMIN
         ARIMAG = IMAJ*IMIN
         ARHEAD = HCBP(1)*HCBP(2)
C
         CRIT1 = ARBEAM .LT. 0.1*ARIMAG
         CRIT2 = ARBEAM .GT. 0.9*ARIMAG
C
         LN = SQRT (8.0 * ALOG (2.0))
C                                       Get the rms
         CALL SADRMS (NXR, RMSIM, X0, Y0, ACTRMS)
         IF (DORMSI) LNOISE = ACTRMS
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
C                                       The source is partitially
C                                       resolved. The errors formulas
C                                       are not garanteed correct!!!
         ELSE
            ERRA = 2.0 * ACTRMS
            IF ((ARIMAG.GT.0.0) .AND. (ARBEAM.GT.0.0)) ERRA =
     *         SQRT (0.8 + (0.2/0.8)*(ARBEAM/ARIMAG-0.1)) * ACTRMS
            END IF
         ORA = A
         ORERRA = ERRA
         ORRMS = ACTRMS
         FRERRA = 1.0
         IF (A.NE.0.0) FRERRA = ABS (ERRA / A)
         ERRMAJ = ABS (IMAJ * FRERRA)
         ERRMIN = ABS (IMIN * FRERRA)
         ERRFI = SQRT (2.) * (IMAJ*IMIN) / MAX (1.E-6, IMAJ**2-IMIN**2)
     *      * FRERRA * RAD2DG
         ERRFI = MIN (360.0, ABS (ERRFI))
         ORERFI = ERRFI
C                                       coordinate errors
C                                       SINC&COSC are switched,
C                                       corrected SAD angle by now
         ERRX0 = SQRT ((ERRMAJ*SINC)**2 + (ERRMIN*COSC)**2) / LN
         ERRY0 = SQRT ((ERRMAJ*COSC)**2 + (ERRMIN*SINC)**2) / LN
         CALL RFILL (7, 0.0, RECORD)
         RECORD(1) = GLIST(1,I)
         ROW(COLPEK) = RECORD(1)
         ROW(COEPEK) = ERRA
         SCALE      = SMIN
         IMARK = 1
C                                       Rescale if overflow
         DO 30 K = 1,2
            IF (ABS(SCALE * RECORD(1)).GE.1000) THEN
               SCALE = .001 * SCALE
               IMARK = IMARK + 1
               END IF
 30         CONTINUE
         IF (IMARK.EQ.1) THEN
            TCUT = SQRT ((CUTT*LNOISE)**2 + (GAIN*GLIST(1,I))**2)
            IF (PLIST(2,I).GT.TCUT) IMARK = 4
            IF (PLIST(3,I).LT.-TCUT) IMARK = 5
            IF (PLIST(1,I).GT.TCUT) IMARK = 6
            END IF
C                       If you can calculate total flux
         IF ((.NOT.DOPNT) .AND. (.NOT.NOCLN)) THEN
            RECORD(1) = GLIST(1,I) * ARIMAG / ARHEAD
            ITOT = RECORD(1) * SCALE
C            ITOTE = (ERRMIN/GLIST(5,I))**2 + (ERRMAJ/GLIST(4,I))**2
C     *         + (ERRA/GLIST(1,I))**2
C            ITOTE = SQRT (ITOTE) * ITOT
            ITOTE = ABS (ERRA / GLIST(1,I) * ITOT)
            ITOTE = ITOTE * SQRT (1. + 2 * ARHEAD/ARIMAG)
            ROW(COEFLX) = ITOTE / SCALE
         ELSE
            ITOT  = 0.
            ITOTE = 0.
            ROW(COEFLX) = ROW(COEPEK)
            END IF
         ROW(COLFLX) = RECORD(1)
C
         RECORD(2) = (GLIST(2,I) - RPLOC(1,LOCNUM)) * AXINC(1,LOCNUM)
         RECORD(3) = (GLIST(3,I) - RPLOC(2,LOCNUM)) * AXINC(2,LOCNUM)
         ROW(COLDLX) = RECORD(2)
         ROW(COLDLY) = RECORD(3)
C                               If you can calculate extensIons
         IF (.NOT.DOPNT) THEN
            RECORD(4) = GLIST(4,I) * SQRT ((AXINC(1,LOCNUM)*SINC)**2 +
     *         (AXINC(2,LOCNUM)*COSC)**2)
            RECORD(5) = GLIST(5,I) * SQRT ((AXINC(2,LOCNUM)*SINC)**2 +
     *         (AXINC(1,LOCNUM)*COSC)**2)
            RECORD(7) = AXINC(2,LOCNUM) * COSC
            IF (RECORD(7).EQ.0.) THEN
               RECORD(6) = GLIST(6,I) - ROT(LOCNUM)
               IF (RECORD(6).GT.180.) RECORD(6) = RECORD(6) - 180.
               IF (RECORD(6).LT.0.) RECORD(6) = RECORD(6) + 180.
            ELSE
               RECORD(6) = ATAN (AXINC(1,LOCNUM)*SINC / RECORD(7))
     *            / DG2RAD  -  ROT(LOCNUM)
               END IF
            RECORD(7) = 2.0
            ROW(COLMJX) = RECORD(4)
            ROW(COLMNX) = RECORD(5)
            ROW(COLPAN) = RECORD(6)
            END IF
         CALL RCOPY (7, RECORD, RICORD)
C                               Convert pixel pos to sky pos
         CAXINC(1) = AXINC(1,LOCNUM)
         CAXINC(2) = AXINC(2,LOCNUM)
         XD = 0.0
         JMARK = 1
         IF ((CORTYP(LOCNUM).EQ.1) .OR. (CORTYP(LOCNUM).EQ.2)) THEN
            CALL XYVAL (GLIST(2,I), GLIST(3,I), X(1), X(2), X(3), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1040) IERR, I
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
            IF (PBPARM(1).GT.0.0) THEN
               CALL PBCALC (XD, LAMBDA, ARRAY, PBPARM(2), BMFACT,
     *            OUTSID)
               BMFACT = MAX (BMFACT, PBPARM(1))
               IF (OUTSID) BMFACT = PBPARM(1)
               OUTSID = BMFACT.LE.PBPARM(1)
               IF (OUTSID) JMARK = 2
               END IF
         ELSE
            CALL XYVAL (GLIST(2,I), GLIST(3,I), X(1), X(2), X(3), IERR)
            END IF
         STXY(1) = X(1)
         STXY(2) = X(2)
         ROW(COBMFA) = 1.0 / MAX (1.E-12, BMFACT)
         CALL GAUSPS ('P2S', GLIST(2,I), GLIST(3,I), GLIST(4,I),
     *      GLIST(5,I), GLIST(6,I), BMAJS, BMINS, BPAS, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1040) IERR, I
            CALL MSGWRT (7)
         ELSE
            ROW(COLMJX) = BMAJS
            ROW(COLMNX) = BMINS
            ROW(COLPAN) = BPAS
            IF (ROW(COLPAN).LT.0.0) ROW(COLPAN) = ROW(COLPAN) + 180.0
            IF (ROW(COLPAN).GE.180.0) ROW(COLPAN) = ROW(COLPAN) - 180.0
C                                       stars info now known
            IF (STVER.GT.0) THEN
               STWID(1) = BMAJS
               STWID(2) = BMINS
               STWID(3) = BPAS
               WRITE (STRING,1085) I
               CALL DEFRMT (STRING, ' ', K)
               CALL TABST ('WRIT', STBUF, ISTRNO, STKOLS, STNUMV, STXY,
     *            STWID, STTYPE, STRING, IERR)
               IF (IERR.NE.0) THEN
                  MSGTXT = 'ERROR WRITING STARS FILE'
                  CALL MSGWRT (7)
                  STVER = 0
                  END IF
               END IF
            END IF
         ORBMAJ = BMAJS * 3600.0
         ORBMIN = BMINS * 3600.0
         ORBPA = BPAS
         IF (GLIST(4,I).NE.0.0) THEN
            ORERMA = ABS (ERRMAJ * ORBMAJ / GLIST(4,I))
         ELSE
            ORERMA = -1.0
            END IF
         IF (GLIST(5,I).NE.0.0) THEN
            ORERMI = ABS (ERRMIN * ORBMIN / GLIST(5,I))
         ELSE
            ORERMA = -1.0
            END IF
C                                        RA and DEC
         IF (DOPA) CALL SADSTR (X(1), X(2), TSTR)
         ROW(COEDLX) = ABS (ERRX0 * CAXINC(1))
         ROW(COEDLY) = ABS (ERRY0 * CAXINC(2))
         ORERRX = ERRX0
         ORERRY = ERRY0
C                                        Diameters
         IF (.NOT.DOPNT) THEN
C                                        Put in image units
            IF (DOPA) THEN
               TEMP = SQRT ((SINC*CAXINC(1))**2 + (COSC*CAXINC(2))**2)
               ROW(COEMJX) = ERRMAJ * TEMP
               TEMP = SQRT ((COSC*CAXINC(1))**2 + (SINC*CAXINC(2))**2)
               ROW(COEMNX) = ERRMIN * TEMP
               TEMP = ABS(CAXINC(1))
            ELSE
               TEMP = 1.0
               ROW(COLMJX) = GLIST(4,I)
               ROW(COEMJX) = ERRMAJ
               ROW(COLMNX) = GLIST(5,I)
               ROW(COEMNX) = ERRMIN
               ROW(COLPAN) = GLIST(6,I)
               END IF
            ROW(COEPAN) = ERRFI
            END IF
C                                       Deconvolve if possible
         IF (TRYCON) THEN
            IF (FACTOR.LE.0.0) FACTOR = 1.3
C                                       BW smearing here
            DX = GLIST(2,I) - XCEN
            DY = GLIST(3,I) - YCEN
            CALL BWSMCB (DX, DY, XD, BWS, CB, SMCB)
            IF (BWCORR) DLFACT = SMCB(1) * SMCB(2) / (CB(1) * CB(2))
            ROW(CODLFA) = DLFACT
            SMCB(3) = SMCB(3) - 90.0
            CALL BMVAL (GLIST(4,I), GLIST(5,I), GLIST(6,I), ERRMAJ,
     *         ERRMIN, ERRFI, SMCB(1), SMCB(2), SMCB(3), FACTOR, WARNIN,
     *         DCONV)
            IF (WARNIN) WASWRN = .TRUE.
C                                       Convert to degrees
            IF (TEMP.NE.0.0) THEN
               RICORD(4) = DCONV(1,1) * TEMP
               RICORD(5) = DCONV(2,1) * TEMP
               RICORD(6) = DCONV(3,1) - ROT(LOCNUM)
C                                       Replace w deconvolved where
C                                       possible
               IF ((DCONV(1,1).GT.0.0) .AND. (DCONV(2,1).GT.0.0)) THEN
                  RECORD(4) = RICORD(4)
                  RECORD(5) = RICORD(5)
                  RECORD(6) = RICORD(6)
               ELSE
                  CALL RFILL (3, 0.0, RECORD(4))
                  END IF
               RECORD(7) = 1.0
               DO 75 K = 1,3
                  IF (((CORTYP(LOCNUM).EQ.1) .OR. (CORTYP(LOCNUM).EQ.2))
     *               .AND. (DCONV(1,K).GT.0.0)) THEN
                     CALL GAUSPS ('P2S', GLIST(2, I), GLIST(3, I),
     *                  DCONV(1, K), DCONV(2, K), DCONV(3, K), BMAJS,
     *                  BMINS, BPAS, IERR)
                     DCONV(1, K) = BMAJS
                     DCONV(2, K) = BMINS
                     DCONV(3, K) = BPAS
                     IF (DCONV(3,K).GT.180.) DCONV(3,K)=DCONV(3,K)-180.
                     IF (DCONV(3,K).LT.0.) DCONV(3,K) = DCONV(3,K)+180.
                  ELSE
                     DCONV(3,K) = DCONV(3,K) - ROT(LOCNUM)
                     IF (DCONV(3,K).GT.180.) DCONV(3,K)=DCONV(3,K)-180.
                     IF (DCONV(3,K).LT.0.) DCONV(3,K) = DCONV(3,K)+180.
                     DCONV(1,K) = DCONV(1,K) * TEMP
                     DCONV(2,K) = DCONV(2,K) * TEMP
                     END IF
 75               CONTINUE
               END IF
            END IF
         CALL RCOPY (9, DCONV, ROW(COLD0J))
         IF (DOCC.GT.0) THEN
            IRNO = CIBUF(5) + 1
            IERR = 0
            CALL TABIO ('WRIT', 0, IRNO, RECORD, CIBUF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (1075) 'CC', IERR, IRNO
               CALL MSGWRT (7)
               DOCC = -1
               END IF
            END IF
C                                       write out model fit table
         IRNO = MFBUF(5) + 1
         WARN(IRNO-IRNO1+1) = WARNIN
         CALL TABIO ('WRIT', 0, IRNO, ROW, MFBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1075) 'MF', IERR, IRNO
            CALL MSGWRT (7)
            GO TO 999
            END IF
C                                       printer display
         IF (DOCRT.LT.-3.5) THEN
            CALL ORPRT (IUS, GLIST(1,I))
         ELSE IF (DOCRT.NE.0.0) THEN
            ROW(COLPEK) = ROW(COLPEK) * SCALE / BMFACT * DLFACT
            ROW(COLFLX) = ROW(COLFLX) * SCALE / BMFACT
            ROW(COEPEK) = ROW(COEPEK) * SCALE / BMFACT * DLFACT
            ROW(COEFLX) = ROW(COEFLX) * SCALE / BMFACT
            IF (DOPA) THEN
               ROW(COLMJX) = ROW(COLMJX) * 3600.0
               ROW(COLMNX) = ROW(COLMNX) * 3600.0
               ROW(COEDLX) = ROW(COEDLX) * 3600.0
               ROW(COEDLY) = ROW(COEDLY) * 3600.0
               ROW(COEMJX) = ROW(COEMJX) * 3600.0
               ROW(COEMNX) = ROW(COEMNX) * 3600.0
               END IF
C                                       VLB accuracy RA-Dec
            IF (DOVLB) THEN
               ROW(COLDLX) = ROW(COLDLX) * 3600.0
               ROW(COLDLY) = ROW(COLDLY) * 3600.0
               IF (DOPNT) THEN
                  WRITE (LINE,1100) IPK, MARK(IMARK), ROW(COLPEK),
     *               PMARK(1,JMARK), ROW(COEPEK), PMARK(2,JMARK),
     *               ROW(COLDLX), ROW(COLDLY), ROW(COEDLX), ROW(COEDLY)
               ELSE IF (NOCLN) THEN
                  IF (NACROS.GE.112) THEN
                     WRITE (LINE,1110) IPK, MARK(IMARK), ROW(COLPEK),
     *                  PMARK(1,JMARK), ROW(COEPEK), PMARK(2,JMARK),
     *                  ROW(COLDLX), ROW(COLDLY), ROW(COEDLX),
     *                  ROW(COEDLY), ROW(COLMJX), ROW(COLMNX),
     *                  ROW(COLPAN), ROW(COEMJX), ROW(COEMNX),
     *                  ROW(COEPAN)
                  ELSE
                     WRITE (LINE,1115) IPK, MARK(IMARK), ROW(COLPEK),
     *                  ROW(COLDLX), ROW(COLDLY), ROW(COLMJX),
     *                  ROW(COLMNX), ROW(COLPAN)
                     CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, ILINE, IPAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) DOCRT = 0.0
                     WRITE (LINE,1116) PMARK(1,JMARK), ROW(COEPEK),
     *                  PMARK(2,JMARK), ROW(COEDLX), ROW(COEDLY),
     *                  ROW(COEMJX), ROW(COEMNX), ROW(COEPAN)
                     END IF
               ELSE
                  IF (NACROS.GE.128) THEN
                     WRITE (LINE,1120) IPK, MARK(IMARK), ROW(COLPEK),
     *                  PMARK(1,JMARK), ROW(COEPEK), PMARK(2,JMARK),
     *                  ROW(COLFLX), '(', ROW(COEFLX), ')',
     *                  ROW(COLDLX), ROW(COLDLY), ROW(COEDLX),
     *                  ROW(COEDLY), ROW(COLMJX), ROW(COLMNX),
     *                  ROW(COLPAN), ROW(COEMJX), ROW(COEMNX),
     *                  ROW(COEPAN)
                  ELSE
                     K = IROUND (ROW(COLPAN))
                     WRITE (LINE,1121) IPK, MARK(IMARK), ROW(COLPEK),
     *                  ROW(COLFLX), ROW(COLDLX), ROW(COLDLY),
     *                  ROW(COLMJX), ROW(COLMNX), K
                     CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, ILINE, IPAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) DOCRT = 0.0
                     K = IROUND (ROW(COEPAN))
                     WRITE (LINE,1122) PMARK(1,JMARK), ROW(COEPEK),
     *                  PMARK(2,JMARK), ROW(COEFLX), ROW(COEDLX),
     *                  ROW(COEDLY), ROW(COEMJX), ROW(COEMNX), K
                     END IF
                  END IF
C                                       coordinates in RA-Dec string
            ELSE IF (DOPA) THEN
               IF (DOPNT) THEN
                  WRITE (LINE,1130) IPK, MARK(IMARK), ROW(COLPEK),
     *               PMARK(1,JMARK), ROW(COEPEK), PMARK(2,JMARK), TSTR,
     *               ROW(COEDLX), ROW(COEDLY)
               ELSE IF (NOCLN) THEN
                  IF (NACROS.GE.115) THEN
                     WRITE (LINE,1140) IPK, MARK(IMARK), ROW(COLPEK),
     *                  PMARK(1,JMARK), ROW(COEPEK), PMARK(2,JMARK),
     *                  TSTR, ROW(COEDLX), ROW(COEDLY), ROW(COLMJX),
     *                  ROW(COLMNX), ROW(COLPAN), ROW(COEMJX),
     *                  ROW(COEMNX), ROW(COEPAN)
                  ELSE IF (NACROS.GE.102) THEN
                     J = IROUND (ROW(COLPAN))
                     K = IROUND (ROW(COEPAN))
                     WRITE (LINE,1141) IPK, MARK(IMARK), ROW(COLPEK),
     *                  PMARK(1,JMARK), ROW(COEPEK), PMARK(2,JMARK),
     *                  TSTR, ROW(COEDLX), ROW(COEDLY), ROW(COLMJX),
     *                  ROW(COLMNX), J, ROW(COEMJX), ROW(COEMNX), K
                  ELSE
                     WRITE (LINE,1145) IPK, MARK(IMARK), ROW(COLPEK),
     *                  TSTR, ROW(COLMJX), ROW(COLMNX), ROW(COLPAN)
                     CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, ILINE, IPAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) DOCRT = 0.0
                     WRITE (LINE,1146) PMARK(1,JMARK), ROW(COEPEK),
     *                  PMARK(2,JMARK), ROW(COEDLX), ROW(COEDLY),
     *                  ROW(COEMJX), ROW(COEMNX), ROW(COEPAN)
                     END IF
               ELSE
                  IF (NACROS.GE.132) THEN
                     K = IROUND (ROW(COEPAN))
                     WRITE (LINE,1150) IPK, MARK(IMARK), ROW(COLPEK),
     *                  PMARK(1,JMARK), ROW(COEPEK), PMARK(2,JMARK),
     *                  ROW(COLFLX), '(', ROW(COEFLX), ')', TSTR,
     *                  ROW(COEDLX), ROW(COEDLY), ROW(COLMJX),
     *                  ROW(COLMNX), ROW(COLPAN), ROW(COEMJX),
     *                  ROW(COEMNX), K
                  ELSE
                     K = IROUND (ROW(COLPAN))
                     WRITE (LINE,1151) IPK, MARK(IMARK), ROW(COLPEK),
     *                  ROW(COLFLX), TSTR, ROW(COLMJX), ROW(COLMNX), K
                     CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, ILINE, IPAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) DOCRT = 0.0
                     K = IROUND (ROW(COEPAN))
                     WRITE (LINE,1152) PMARK(1,JMARK), ROW(COEPEK),
     *                  PMARK(2,JMARK), ROW(COEFLX), ROW(COEDLX),
     *                  ROW(COEDLY), ROW(COEMJX), ROW(COEMNX), K
                     END IF
                  END IF
C                                       non RA-dec coordinates
            ELSE
               IF (DOPNT) THEN
                  WRITE (LINE,1160) IPK, MARK(IMARK), ROW(COLPEK),
     *               PMARK(1,JMARK), ROW(COEPEK), PMARK(2,JMARK),
     *               ROW(COLDLX), ROW(COLDLY), ROW(COEDLX), ROW(COEDLY)
                  LINE(52:52) = '('
C                                       NOCLN true when DOPA false
               ELSE
                  IF (NACROS.GE.116) THEN
                     WRITE (LINE,1170) IPK, MARK(IMARK), ROW(COLPEK),
     *                  PMARK(1,JMARK), ROW(COEPEK), PMARK(2,JMARK),
     *                  ROW(COLDLX), ROW(COLDLY), ROW(COEDLX),
     *                  ROW(COEDLY), ROW(COLMJX), ROW(COLMNX),
     *                  ROW(COEMJX), ROW(COEMNX)
                     LINE(52:52) = '('
                  ELSE IF (NACROS.GE.99) THEN
                     WRITE (LINE,1171) IPK, MARK(IMARK), ROW(COLPEK),
     *                  PMARK(1,JMARK), ROW(COEPEK), PMARK(2,JMARK),
     *                  ROW(COLDLX), ROW(COLDLY), ROW(COEDLX),
     *                  ROW(COEDLY), ROW(COLMJX), ROW(COLMNX),
     *                  ROW(COEMJX), ROW(COEMNX)
                     LINE(48:48) = '('
                  ELSE
                     WRITE (LINE,1175) IPK, MARK(IMARK), ROW(COLPEK),
     *                  ROW(COLDLX), ROW(COLDLY), ROW(COLMJX),
     *                  ROW(COLMNX)
                     CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, ILINE, IPAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) DOCRT = 0.0
                     WRITE (LINE,1176) PMARK(1,JMARK), ROW(COEPEK),
     *                  PMARK(2,JMARK), ROW(COEDLX), ROW(COEDLY),
     *                  ROW(COEMJX), ROW(COEMNX)
                     LINE(20:20) = '('
                     END IF
                  END IF
               END IF
            END IF
         IF ((DOCRT.NE.0.0) .AND. (DOCRT.GT.-3.5)) THEN
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, ILINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) DOCRT = 0.0
            END IF
 200     CONTINUE
C                                       Print deconvolution
      HEADER = (DOCRT.NE.0.0) .AND. (DOCRT.GT.-2.5)
      IF ((TRYCON) .AND. (DOCRT.NE.0.0) .AND. (DOCRT.GT.-3.5)) THEN
         IRNO2 = MFBUF(5)
C                                       Title lines
         TITL1(56:) = '*** Deconvolution ***'
         TITL1(NACROS+1:) = ' '
         TITL2 = '   #'
         IF (NACROS.GE.106) THEN
            TITL2( 6:) = ' MAJ-fit MIN-fit PA-fit'
            TITL2(32:) = ' MAJ-dec MIN-dec PA-dec  R'
            TITL2(59:) = ' MAJ-low MIN-low PA-low'
            TITL2(84:) = '  MAJ-hi  MIN-hi  PA-hi'
            IF (NACROS.GE.132) TITL2(111:) = 'Xpix   Ypix   MAXresid'
         ELSE
            TITL2( 6:) = ' MAJ-dec MIN-dec   PA  R'
            TITL2(29:) = ' MAJ-low MIN-low   PA'
            TITL2(50:) = '  MAJ-hi  MIN-hi   PA'
            END IF
         IF (HEADER) THEN
            LINE = ' '
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, ILINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               IERR = 0
               GO TO 990
               END IF
            WRITE (STRING,1199) FACTOR
            IF (NACROS.LT.106) THEN
               LINE = 'Component widths & PA: deconvolved at fit' //
     *            ' & xx.xx sigma low & high from fit'
               LINE(45:49) = STRING(:5)
            ELSE
               LINE = 'Component widths & PA: fit, deconvolved at fit'
     *            // ' and xx.xx sigma low and high from fit'
               LINE(52:56) = STRING(:5)
               END IF
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, ILINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               IERR = 0
               GO TO 990
               END IF
            IF (WASWRN) THEN
               LINE = 'Formal deconvolution marked with * when smaller'
     *            // ' than believable'
               CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, ILINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) THEN
                  IERR = 0
                  GO TO 990
                  END IF
               END IF
            END IF
         IF (DOCRT.GT.-3.5) THEN
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *         TITL2, ILINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               IERR = 0
               GO TO 990
               END IF
            END IF
         IF (HEADER) THEN
            LINE = ' '
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, ILINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               IERR = 0
               GO TO 990
               END IF
            END IF
         DO 300 IRNO = IRNO1,IRNO2
            CALL TABIO ('READ', 0, IRNO, ROW, MFBUF, IERR)
            IF (IERR.LT.0) GO TO 300
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1200) IERR
               CALL MSGWRT (7)
               GO TO 990
               END IF
            ROW(COLMJX) = ROW(COLMJX) * 3600.0
            ROW(COLMNX) = ROW(COLMNX) * 3600.0
            ROW(COLD0J) = ROW(COLD0J) * 3600.0
            ROW(COLD0N) = ROW(COLD0N) * 3600.0
            ROW(COLDMJ) = ROW(COLDMJ) * 3600.0
            ROW(COLDMN) = ROW(COLDMN) * 3600.0
            ROW(COLDPJ) = ROW(COLDPJ) * 3600.0
            ROW(COLDPN) = ROW(COLDPN) * 3600.0
C                                       resolved
            RPT = 4
            IF (ROW(COLPEK).GT.0.0) THEN
               IF (ROW(COLFLX)-ROW(COEFLX).GT.ROW(COLPEK)) THEN
                  RPT = 3
               ELSE
                  RPT = 2
                  END IF
            ELSE IF (ROW(COLPEK).LT.0.0) THEN
               IF (ROW(COLFLX)+ROW(COEFLX).LT.ROW(COLPEK)) THEN
                  RPT = 3
               ELSE
                  RPT = 2
                  END IF
               END IF
            IF (ROW(COLD0J).LE.0.0) RPT = 1
            IF (RPT.EQ.3) THEN
               IF (ROW(COLDMJ).LE.0) RPT = 2
            ELSE IF (RPT.EQ.2) THEN
               IF (ROW(COLDMJ).LE.0.0) RPT = 1
               END IF
C                                       start writing
            J = IRNO - IRNO1 + 1
            WRITE (LINE,1205) J
            WARNIN = WARN(J)
            IF (WARNIN) THEN
               DMARK = '*'
            ELSE
               DMARK = ' '
               END IF
            IF (.NOT.DOVLB) THEN
               IF (NACROS.GE.106) THEN
                  WRITE (TSTR,1210) (ROW(I), I = 6,8)
                  LINE(6:) = TSTR
                  IF ((ROW(COLD0J).GT.0.0) .OR. (ROW(COLD0N).GT.0.0))
     *               THEN
                     WRITE (TSTR,1210) (ROW(I), I = COLD0J,COLD0P)
                  ELSE
                     WRITE (TSTR,1211)
                     END IF
                  LINE(32:) = TSTR
                  LINE(56:56) = DMARK
                  LINE(57:57) = RESOLV(RPT)
                  IF ((ROW(COLDMJ).GT.0.0) .OR. (ROW(COLDMN).GT.0.0))
     *               THEN
                     WRITE (TSTR,1210) (ROW(I), I = COLDMJ,COLDMP)
                  ELSE
                     WRITE (TSTR,1211)
                     END IF
                  LINE(59:) = TSTR
                  IF ((ROW(COLDPJ).GT.0.0) .OR. (ROW(COLDPN).GT.0.0))
     *               THEN
                     WRITE (TSTR,1210) (ROW(I), I = COLDPJ,COLDPP)
                  ELSE
                     WRITE (TSTR,1211)
                     END IF
                  LINE(84:) = TSTR
                  IF (NACROS.GE.132) THEN
                     CMARK = ' '
                     TCUT = SQRT ((CUTT*LNOISE)**2 +
     *                  (GAIN*ROW(COLPEK))**2)
                     IF ((ROW(CORRMS).GT.TCUT) .OR.
     *                  (ROW(CORPEK).GT.TCUT)) CMARK = '*'
                     XXT = ROW(COLDLX)/AXINC(1,LOCNUM) + RPLOC(1,LOCNUM)
                     YYT = ROW(COLDLY)/AXINC(2,LOCNUM) + RPLOC(2,LOCNUM)
                     WRITE (TSTR,1212) XXT, YYT, ROW(CORPEK), CMARK
                     LINE(108:) = TSTR
                     END IF
               ELSE
                  IF ((ROW(COLD0J).GT.0.0) .OR. (ROW(COLD0N).GT.0.0))
     *               THEN
                     J = IROUND (ROW(COLD0P))
                     WRITE (TSTR,1215) (ROW(I), I = COLD0J,COLD0N), J
                  ELSE
                     WRITE (TSTR,1216)
                     END IF
                  LINE(6:) = TSTR
                  LINE(27:27) = DMARK
                  LINE(28:28) = RESOLV(RPT)
                  IF ((ROW(COLDMJ).GT.0.0) .OR. (ROW(COLDMN).GT.0.0))
     *               THEN
                     J = IROUND (ROW(COLDMP))
                     WRITE (TSTR,1215) (ROW(I), I = COLDMJ,COLDMN), J
                  ELSE
                     WRITE (TSTR,1216)
                     END IF
                  LINE(29:) = TSTR
                  IF ((ROW(COLDPJ).GT.0.0) .OR. (ROW(COLDPN).GT.0.0))
     *               THEN
                     J = IROUND (ROW(COLDPP))
                     WRITE (TSTR,1215) (ROW(I), I = COLDPJ,COLDPN), J
                  ELSE
                     WRITE (TSTR,1216)
                     END IF
                  LINE(50:) = TSTR
                  END IF
            ELSE
               IF (NACROS.GE.106) THEN
                  WRITE (TSTR,1220) (ROW(I), I = 6,8)
                  LINE(6:) = TSTR
                  IF ((ROW(COLD0J).GT.0.0) .OR. (ROW(COLD0N).GT.0.0))
     *               THEN
                     WRITE (TSTR,1220) (ROW(I), I = COLD0J,COLD0P)
                  ELSE
                     WRITE (TSTR,1221)
                     END IF
                  LINE(32:) = TSTR
                  LINE(56:56) = DMARK
                  LINE(57:57) = RESOLV(RPT)
                  IF ((ROW(COLDMJ).GT.0.0) .OR. (ROW(COLDMN).GT.0.0))
     *               THEN
                     WRITE (TSTR,1220) (ROW(I), I = COLDMJ,COLDMP)
                  ELSE
                     WRITE (TSTR,1221)
                     END IF
                  LINE(59:) = TSTR
                  IF ((ROW(COLDPJ).GT.0.0) .OR. (ROW(COLDPN).GT.0.0))
     *               THEN
                     WRITE (TSTR,1220) (ROW(I), I = COLDPJ,COLDPP)
                  ELSE
                     WRITE (TSTR,1221)
                     END IF
                  LINE(84:) = TSTR
                  IF (NACROS.GE.132) THEN
                     CMARK = ' '
                     TCUT = SQRT ((CUTT*LNOISE)**2 +
     *                  (GAIN*ROW(COLPEK))**2)
                     IF ((ROW(CORRMS).GT.TCUT) .OR.
     *                  (ROW(CORPEK).GT.TCUT)) CMARK = '*'
                     XXT = ROW(COLDLX)/AXINC(1,LOCNUM) + RPLOC(1,LOCNUM)
                     YYT = ROW(COLDLY)/AXINC(2,LOCNUM) + RPLOC(2,LOCNUM)
                     WRITE (TSTR,1212) XXT, YYT, ROW(CORPEK), CMARK
                     LINE(108:) = TSTR
                     END IF
               ELSE
                  IF ((ROW(COLD0J).GT.0.0) .OR. (ROW(COLD0N).GT.0.0))
     *               THEN
                     J = IROUND (ROW(COLD0P))
                     WRITE (TSTR,1225) (ROW(I), I = COLD0J,COLD0N), J
                  ELSE
                     WRITE (TSTR,1226)
                     END IF
                  LINE(6:) = TSTR
                  LINE(27:27) = DMARK
                  LINE(28:28) = RESOLV(RPT)
                  IF ((ROW(COLDMJ).GT.0.0) .OR. (ROW(COLDMN).GT.0.0))
     *               THEN
                     J = IROUND (ROW(COLDMP))
                     WRITE (TSTR,1225) (ROW(I), I = COLDMJ,COLDMN), J
                  ELSE
                     WRITE (TSTR,1226)
                     END IF
                  LINE(29:) = TSTR
                  IF ((ROW(COLDPJ).GT.0.0) .OR. (ROW(COLDPN).GT.0.0))
     *               THEN
                     J = IROUND (ROW(COLDPP))
                     WRITE (TSTR,1225) (ROW(I), I = COLDPJ,COLDPN), J
                  ELSE
                     WRITE (TSTR,1226)
                     END IF
                  LINE(50:) = TSTR
                  END IF
               END IF
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, ILINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) THEN
                IERR = 0
                GO TO 990
                END IF
 300        CONTINUE
         END IF
C                                        close CC file
 990  IF (DOCC.GT.0) CALL TABIO ('CLOS', 0, IRNO, RICORD, CIBUF, IERR)
      IF (STVER.GT.0) CALL TABST ('CLOS', STBUF, ISTRNO, STKOLS, STNUMV,
     *   STXY, STWID, STTYPE, STRING, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SADOUT: COULD NOT CHANGE TO WRITE STATUS, IER=',I7)
 1001 FORMAT ('SADOUT: Cannot initialize input CC file ver:',I3)
 1005 FORMAT ('Writing ',A,' file version',I4,
     *   ' attached to input image')
 1010 FORMAT (A12,'.',A6,'.',A7,5X,'Disk ',A2,5X,'Plane',I5,5X,'User ',
     *   A7)
 1015 FORMAT ('Window BLC',2I5,2I4,3I3,' TRC',2I5,2I4,3I3)
 1016 FORMAT ('Sources found down to',F10.6,' in ',A)
 1017 FORMAT ('Retry level',F10.6,' (',A,') plus gain',F6.3)
 1018 FORMAT ('Using_RMS_image__',A,'.',A,'.',I5,'_disk_',I3)
 1021 FORMAT ('Reject components peak <',F9.5,' in ',A)
 1022 FORMAT ('Reject components flux <',F9.5)
 1023 FORMAT ('Reject components RMS >',F10.5,' (',A,') with gain',F6.3)
 1024 FORMAT ('Reject components width >',F6.1,' cells')
 1025 FORMAT ('Reject components outside window >',F6.1,' cells')
 1026 FORMAT ('Reject components outside image > ',F6.1,' cells')
 1027 FORMAT ('Reject residual flux >',F10.5,' with gain',F6.3)
 1028 FORMAT ('Errors determined by theory from RMS',F9.5)
 1029 FORMAT ('Errors determined by theory from RMS',F7.2,' microJy')
 1030 FORMAT ('Reference Center: ',A)
 1031 FORMAT ('Reference Center: ',2F13.5)
 1035 FORMAT ('Clean beam widths corrected for bandwidth smearing',F7.4)
 1040 FORMAT ('ERROR',I3,' CONVERTING TO SKY POSITION OF SOURCE',I5)
 1075 FORMAT (A,' FILE ERROR',I5,' WRITING ROW',I6)
 1085 FORMAT ('SAD_',I6)
 1100 FORMAT (I5,1X,A1,1X,F9.4,A1,F8.4,A1,2F11.5,'(',F8.5,F8.5,')')
 1110 FORMAT (I5,1X,A1,1X,F9.4,A1,F8.4,A1,2F11.5,'(',F8.5,F8.5,')',
     *   2F8.5,F6.1,'(',F8.5,F8.5,F5.1,')')
 1115 FORMAT (I5,1X,A1,1X,F9.4,F12.5,F11.5,F10.5,F8.5,F6.1)
 1116 FORMAT (8X,A1,F8.4,A1,' (',F9.5,F11.5,') (',F7.5,F8.5,F6.1,')')
 1120 FORMAT (I5,1X,A1,2(F9.3,A1,F7.3,A1),2F11.5,'(',F8.5,F8.5,')',
     *   2F8.5,F6.1,'(',F8.5,F8.5,F5.1,')')
 1121 FORMAT (I5,1X,A1,F9.3,F10.3,F12.5,F11.5,F10.5,F8.5,I4)
 1122 FORMAT (8X,A1,F7.3,A1,' (',F7.3,') (',F9.5,F11.5,') (',F7.5,F8.5,
     *   I4,')')
 1130 FORMAT (I5,1X,A1,1X,F9.4,A1,F8.4,A1,1X,A27,'(',2F8.3,')')
 1140 FORMAT (I5,1X,A1,1X,F9.4,A1,F8.4,A1,1X,A27,'(',2F8.3,')',
     *   F8.2,F7.2,F6.1,'(',2F7.3,F5.1,')')
 1141 FORMAT (I5,1X,A1,F8.2,A1,F6.2,A1,1X,A27,'(',2F7.3,')',
     *   2F7.2,I4,'(',2F6.2,I3,')')
 1145 FORMAT (I5,1X,A1,1X,F9.4,3X,A27,F10.3,F8.3,F6.1)
 1146 FORMAT (8X,A1,F8.4,A1,' (',F13.3,F14.3,') (',F7.3,F8.3,F6.1,')')
 1150 FORMAT (I5,1X,A1,2(F9.3,A1,F7.3,A1),1X,A27,'(',2F8.3,')',
     *   F9.3,F8.3,F6.1,'(',2F7.3,I4,')')
 1151 FORMAT (I5,1X,A1,F9.3,F8.2,1X,A27,F8.3,F8.3,I4)
 1152 FORMAT (8X,A1,F7.3,A1,'(',F6.2,')(',F12.3,F14.3,')(',F7.3,F7.3,
     *   I4,')')
 1160 FORMAT (I5,1X,A1,F10.4,A1,F8.4,A1,2(1PE12.4),1PE110.3,
     *   1PE10.3,')')
 1170 FORMAT (I5,1X,A1,F10.4,A1,F8.4,A1,2(1PE12.4),1PE10.3,1PE10.3,
     *   ')',F11.4,F10.4,'(',2F9.4,')')
 1171 FORMAT (I5,1X,A1,F9.3,A1,F7.3,A1,2(1PE11.3),1PE9.2,1PE9.2,
     *   ')',F9.3,F8.3,'(',2F7.3,')')
 1175 FORMAT (I5,1X,A1,F10.4,1X,2(1PE12.4),F11.4,F10.4)
 1176 FORMAT (8X,A1,F8.4,A1,1PE12.4,1PE12.4,') (',F8.4,F10.4,')')
 1199 FORMAT (F5.2)
 1200 FORMAT ('ERROR',I5,' READING BACK THE MF FILE')
 1205 FORMAT (I5)
 1210 FORMAT (2F8.2,F7.1)
 1211 FORMAT (4X,'---',5X,'---',5X,'---')
 1212 FORMAT (2F7.1,F10.5,A1)
 1215 FORMAT (2F8.2,I5)
 1216 FORMAT (4X,'---',5X,'---',3X,'---')
 1220 FORMAT (2F8.5,F7.1)
 1221 FORMAT (1X,'---',5X,'---',8X,'---')
 1225 FORMAT (2F8.5,I5)
 1226 FORMAT (1X,'---',5X,'---',6X,'---')
      END
      SUBROUTINE BMVAL (BMAJ, BMIN, BPA, BMAJE, BMINE, BPAE, CBMAJ,
     *   CBMIN, CBPA, FACTOR, WARN, R)
C-----------------------------------------------------------------------
C   Subroutine BMVAL deconvolves the fitted beam from the clean beam and
C   also generates appropriate errors.
C   INPUTS:
C      BMAJ       R       Fitted major axis
C      BMIN       R       Fitted minor axis
C      BPA        R       Fitted pos. angle (deg)  WRT Y
C      BMAJE      R       Fitted major axis error
C      BMINE      R       Fitted minor axis error
C      BPAE       R       Fitted pos. angle error (deg)
C      CBMAJ      R       Clean beam major axis
C      CBMIN      R       Clean beam minor axis
C      CBPA       R       Clean beam pos. angle (deg) WRT Y
C   Outputs:
C      WARN       L       True => formal solution major axis smaller
C                         than CB+err solution major  axis
C      R(3,3)     R       RMAJ, RMIN, RPA array
C-----------------------------------------------------------------------
      REAL      BMAJ, BMIN, BPA, BMAJE, BMINE, BPAE, CBMAJ, CBMIN, CBPA,
     *   FACTOR, R(3,3)
      LOGICAL   WARN
C
      INTEGER   I, J, K, IC, IERR
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
      WARN = R(1,1).LT.R(1,3)
C                                       Set up looping
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
      SUBROUTINE SADRMS (NXR, RMSIM, X, Y, ACTRMS)
C-----------------------------------------------------------------------
C   Returns RMS unless there is an image of rma - in which case it
C   finds the nearest pixel and interpolates an rms
C   Inputs:
C      NXR      I      X dimension of RMSIM
C      RMSIM    R(*)   RMS image
C      X        R      X pixel in input image (LOCNUM=1)
C      Y        R      Y pixel in input image (LOCNUM=1)
C   Output:
C      ACTRMS   R      RMS to use in error estimates
C-----------------------------------------------------------------------
      INTEGER   NXR
      REAL      RMSIM(NXR,*), X, Y, ACTRMS
C
      INCLUDE 'TVSAD.INC'
      DOUBLE PRECISION XV, YV, ZV
      REAL      XP, YP, VALS(2,2), V1, V2
      INTEGER   IERR, IX, IY
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       no image
      IF (LUNR.LE.0) THEN
         ACTRMS = RMS
         IERR = 1
C                                       there is an image
      ELSE
         LOCNUM = 1
         CALL XYVAL (X, Y, XV, YV, ZV, IERR)
         LOCNUM = 2
         IF (IERR.EQ.0) CALL XYPIX (XV, YV, XP, YP, IERR)
         LOCNUM = 1
         IX = MAX (1.0, XP)
         IY = MAX (1.0, YP)
         IF (IX.GT.IN2BLK(KINAX)-1) IX = IN2BLK(KINAX)-1
         IF (IY.GT.IN2BLK(KINAX+1)-1) IY = IN2BLK(KINAX+1)-1
         VALS(1,1) = RMSIM(IX,IY)
         VALS(2,1) = RMSIM(IX+1,IY)
         VALS(1,2) = RMSIM(IX,IY+1)
         VALS(2,2) = RMSIM(IX+1,IY+1)
         XP = XP - IX
         YP = YP - IY
         XP = MAX (0.0, MIN (1.0, XP))
         YP = MAX (0.0, MIN (1.0, YP))
         IF (VALS(1,1).EQ.FBLANK) THEN
            V1 = VALS(2,1)
         ELSE IF (VALS(2,1).EQ.FBLANK) THEN
            V1 = VALS(1,1)
         ELSE
            V1 = XP * VALS(2,1) + (1.0-XP) * VALS(1,1)
            END IF
         IF (VALS(1,2).EQ.FBLANK) THEN
            V2 = VALS(2,2)
         ELSE IF (VALS(2,2).EQ.FBLANK) THEN
            V2 = VALS(1,2)
         ELSE
            V2 = XP * VALS(2,2) + (1.0-XP) * VALS(1,2)
            END IF
         IF (V1.EQ.FBLANK) THEN
            ACTRMS = V2
         ELSE IF (V2.EQ.FBLANK) THEN
            ACTRMS = V1
         ELSE
            ACTRMS = YP * V2 + (1.0-YP) * V1
            END IF
         IF ((IERR.NE.0) .OR. (ACTRMS.EQ.FBLANK)) ACTRMS = RMS
         END IF
C
 999  RETURN
      END
      SUBROUTINE DVDMIN (FX, XI, ERR, N, EPS, ITMAX, FOPT, GNOPT, IER,
     *   NPR, RPRT)
C-----------------------------------------------------------------------
C   This is a Fortran implementation of Davidon's optimally conditioned
C   variable metric (quasi-Newton) method for function minimization.  It
C   is based on the algorithm given in W. C. Davidon:  Optimally
C   conditioned optimization algorithms without line searches,
C   Mathematical Programming, vol. 9 (1975) pp. 1-30.  One should refer
C   to that reference for the algorithmic details.  Here, the steps of
C   the algorithm which are delineated by COMMENT lines correspond to
C   the numbered steps in Davidon's paper.  The user must supply a
C   subroutine FX to calculate the objective function and its gradient
C   at a given point.  The objective function F is assumed to be a
C   real-valued function of N real variables.  Here, 0 is assumed to be
C   a lower bound for F.  If F can assume negative values, Step 2 must
C   be modified in one of two different ways, depending on whether a
C   lower bound is known (see Davidon for details).
C   Inputs:
C      FX      ENTRY     A user-supplied subroutine of the form
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
      EXTERNAL FX
      INTEGER   NF, NG, IT, I, N, J, NPR, ITMAX, IER, L, I1, I2
C                                       (24 = max number of unknowns)
      INCLUDE 'TVSAD.INC'
      DOUBLE PRECISION XI(1), ERR(1), DDOT, DMACH, DNRM2, LAMBDA,
     *   MSQ, MU, NSQ, NU, XJ(MAXP,MAXP), X0(MAXP), X(MAXP), K0(MAXP),
     *   K(MAXP), S(MAXP), GG(MAXP), M(MAXP), P(MAXP), Q(MAXP),
     *   WUN(MAXP), AX(MAXP), TINYC, F, GN, F0P, XX, EPS, FP, B0, UTU,
     *   UTS, B, GAMMA, F0, DELTA, A, C, ALF, T1, T2, T3, T4, T5, T6,
     *   QTK0, FOPT, GNOPT, DT1, DT2
      CHARACTER ATPT*8
      INTEGER  PERR
      REAL     RPRT, RPEAK
      LOGICAL  XPR
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      XPR = RPRT.GT.0.0
      IF (XPR) THEN
         TITL2 = '   DVDMIN     debugging information    DVDMIN'
         END IF
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
         CALL MSGWRT (4)
         IF (XPR) CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1,
     *      TITL2, MSGTXT, ILINE, IPAGE, SCRTCH, PERR)
         IF (PERR.NE.0) XPR = .FALSE.
         END IF
      IF (NPR.GT.1) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (4)
         IF (XPR) THEN
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *         MSGTXT, ILINE, IPAGE, SCRTCH, PERR)
            IF (PERR.NE.0) XPR = .FALSE.
            END IF
         I1 = 1
 102     I2 = I1 + 5
            IF (I2.GT.N) I2 = N
            WRITE (MSGTXT,1021) (X0(I), I = I1,I2)
            CALL MSGWRT (4)
            IF (XPR) THEN
               CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *            MSGTXT, ILINE, IPAGE, SCRTCH, PERR)
               IF (PERR.NE.0) XPR = .FALSE.
               END IF
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
      IF (NPR.LE.0) GO TO 995
         IF (IER.EQ.0) WRITE (MSGTXT,1920)
         IF (IER.EQ.1) WRITE (MSGTXT,1921)
         IF (IER.NE.2) THEN
            CALL MSGWRT (4)
            IF (XPR) THEN
               CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *            MSGTXT, ILINE, IPAGE, SCRTCH, PERR)
               IF (PERR.NE.0) XPR = .FALSE.
               END IF
            END IF
         WRITE (MSGTXT,1922) NF, NG
         CALL MSGWRT (4)
         IF (XPR) THEN
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *         MSGTXT, ILINE, IPAGE, SCRTCH, PERR)
            IF (PERR.NE.0) XPR = .FALSE.
            END IF
         WRITE (MSGTXT,1923)
         CALL MSGWRT (4)
         IF (XPR) THEN
            CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *         MSGTXT, ILINE, IPAGE, SCRTCH, PERR)
            IF (PERR.NE.0) XPR = .FALSE.
            END IF
         I1 = 1
 930     I2 = I1 + 5
            IF (I2.GT.N) I2 = N
            WRITE (MSGTXT,1021) (XI(I), I = I1,I2)
            CALL MSGWRT (4)
            IF (XPR) THEN
               CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITL1, TITL2,
     *            MSGTXT, ILINE, IPAGE, SCRTCH, PERR)
               IF (PERR.NE.0) XPR = .FALSE.
               END IF
            I1 = I2 + 1
            IF (I1.LE.N) GO TO 930
C
 995  IF (.NOT.XPR) THEN
         IF (RPRT.GT.0.0) DOCRT = 0.0
         RPRT = -1.0
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Iteration #',I4,3X,' F=',1PD16.8,' Gradient=',1PD16.8)
 1020 FORMAT ('Parameters:')
 1021 FORMAT (1PD12.5,5(1PD13.5))
 1890 FORMAT ('DVDMIN: ZERO DIVIDE AT ',A,' AVOIDED, PEAK',1PE11.4)
 1920 FORMAT ('***  Convergence achieved.')
 1921 FORMAT ('***  Maximum number of iterations reached.')
 1922 FORMAT (I4,' Function evaluations and ',I4,
     *   ' gradient evaluations.')
 1923 FORMAT ('Solution parameters:')
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
 20   TINY = S
      S = S/16.0D0
      IF (S*1.0.NE.0.0D0) GO TO 20
      TINY = (TINY/EPS)*100.0
      HUGE = 1.0D0/TINY
C
      DMACH = EPS
      IF (JOB.EQ.2) DMACH = TINY
      IF (JOB.EQ.3) DMACH = HUGE
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
      SUBROUTINE ISLAND (NXI, IMAGE, NXR, RMSIM, PKWIN, PKMAX, NPK,
     *   NPKMAX)
C-----------------------------------------------------------------------
C   Find points in the residual map whose values are greater than CUTT.
C   All such points which touch each other, either along grid lines or
C   diagonals, are considered to be one "island".  Give each distinct
C   island its own number and compute a rectangular window that just
C   contains that island.
C   Inputs
C      NXI     I      X dimension of IMAGE
C      NYI     I      Y dimension of IMAGE
C      IMAGE   R(*)   Resdiual image
C      NXR     I      X dimension of RMSimage
C      NYR     I      Y dimension of RMSimage
C      RMSIM   R(*)   Image of RMSes
C      NPKMAX   I      Maximum allowed number of islands
C   Common:
C      CUTT     R      Cutoff level of islands
C   Outputs:
C      PKWIN    R(4,NPKMAX)   A list of minX,minY,maxX,maxY window
C                      corners for islands
C      PKMAX    R(NPKMAX)     A list of the peak flux in islands
C      NPK      I      Number of islands found
C      IERR     I      I/O errors
C-----------------------------------------------------------------------
      INTEGER   NXI, NXR, PKWIN(4,*), NPK, NPKMAX
      REAL      IMAGE(NXI,*), RMSIM(NXR,*), PKMAX(*)
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   OLD(MAXIMG), NEW(MAXIMG), I, J, LOCWIN(4), IRET, IFIL
      REAL      BLC(7), TRC(7)
      INCLUDE 'TVSAD.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NPK = 0
      LNOISE = 1.0
      CALL RCOPY (7, XBLC, BLC)
      CALL RCOPY (7, XTRC, TRC)
      CALL MAPWIN (LUN2, BLC, TRC, IRET)
C                                       get real values of win from
C                                       filtab
      CALL FILNUM (LUN2, IFIL, IRET)
      LOCWIN(1) = FILTAB(POBLC,   IFIL)
      LOCWIN(2) = FILTAB(POBLC+1, IFIL)
      LOCWIN(3) = FILTAB(POTRC,   IFIL)
      LOCWIN(4) = FILTAB(POTRC+1, IFIL)
      NX     = LOCWIN(3) - LOCWIN(1) + 1
      NY     = LOCWIN(4) - LOCWIN(2) + 1
C                                       Initialize previous ine
      CALL FILL (MAXIMG, 0, OLD)
C
      DO 200 J = LOCWIN(2),LOCWIN(4)
         DO 100 I = LOCWIN(1),LOCWIN(3)
            IF (DORMSI) LNOISE = RMSIM(I,J)
C                                       Is point above cutoff?
            IF ((LNOISE.EQ.FBLANK) .OR. (IMAGE(I,J).EQ.FBLANK) .OR.
     *         (IMAGE(I,J).LT.CUTT*LNOISE)) THEN
               NEW(I) = 0
C                                       Are any adjacent points, on
C                                       currnt Line or previous line
C                                       already marked?
            ELSE IF ((I.GT.LOCWIN(1)) .AND. (NEW(I-1).GT.0)) THEN
               NEW(I) = NEW(I-1)
C                                       link to previous now?
               IF ((I.LT.LOCWIN(3)) .AND. (OLD(I+1).GT.0))
     *            CALL MERGPK (NEW(I), OLD(I+1), LOCWIN, NPK, PKWIN,
     *            PKMAX, OLD, NEW)
            ELSE IF ((I.GT.LOCWIN(1)) .AND. (OLD(I-1).NE.0)) THEN
               NEW(I) = OLD(I-1)
C                                       Is this A link between two
C                                       previous distinct islands?
               IF ((I.LT.LOCWIN(3)) .AND. (OLD(I+1).GT.0))
     *            CALL MERGPK (NEW(I), OLD(I+1), LOCWIN, NPK, PKWIN,
     *            PKMAX, OLD, NEW)
            ELSE IF (OLD(I).NE.0) THEN
               NEW(I) = OLD(I)
            ELSE IF ((I.LT.LOCWIN(3)) .AND. (OLD(I+1).NE.0)) THEN
               NEW(I) = OLD(I+1)
C                                       Totally new island
            ELSE
               IF (NPK.LT.NPKMAX) THEN
                  NEW(I) = NPK + 1
                  NPK  = NEW(I)
                  PKWIN(1,NPK) = I
                  PKWIN(2,NPK) = J
                  PKWIN(3,NPK) = I
                  PKWIN(4,NPK) = J
                  PKMAX(NPK) = IMAGE(I,J)
                  GO TO 100
               ELSE
                  NEW(I) = 0
                  END IF
               END IF
C                                       New addition to old island
            IF (NEW(I).NE.0) CALL ADDPK (PKWIN, PKMAX, I, J, IMAGE(I,J),
     *         NEW(I))
 100        CONTINUE
C                                       Get ready for next line.
         CALL COPY (NX, NEW(LOCWIN(1)), OLD(LOCWIN(1)))
 200     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ADDPK (PKWIN, PKMAX, I, J, FLUX, IPK)
C-----------------------------------------------------------------------
C   Add a single point with coordinates I,J to an existing island by
C   adjusting the boundaries of the Island
C-----------------------------------------------------------------------
      INTEGER PKWIN(4,*), I, J, IPK
      REAL    PKMAX(*), FLUX
C-----------------------------------------------------------------------
      PKWIN(1,IPK) = MIN (PKWIN(1,IPK), I)
      PKWIN(3,IPK) = MAX (PKWIN(3,IPK), I)
      PKWIN(4,IPK) = J
      PKMAX(IPK) = MAX (PKMAX(IPK), FLUX)
C
 999  RETURN
      END
      SUBROUTINE MERGPK (IPK, JPK, WIN, NPK, PKWIN, PKMAX, OLD, NEW)
C-----------------------------------------------------------------------
C      Merge two Islands together by finding max boundaries
C   Inputs:
C      IPK     I        first island number
C      JPK     I        second island number
C      WIN     I(4)     full blc,trc window
C   In/Out:
C      NPK     I        Maximum island number - reduced if = higher
C      PKWIN   I(4,*)   windows for islands - higher set to zero
C      PKMAX   R(*)     max intensity for islands
C      OLD     I(*)     window number for previous row - higher set to
C                       lower
C      NEW     I(*)     window number for current row - higher set to
C                       lower
C-----------------------------------------------------------------------
      INTEGER   IPK, JPK, WIN(4), NPK, PKWIN(4,*), OLD(*), NEW(*)
      REAL      PKMAX(*)
C
      INTEGER   I, IL, IH
C-----------------------------------------------------------------------
      IF (IPK.NE.JPK) THEN
         IL = MIN (IPK, JPK)
         IH = MAX (IPK, JPK)
C                                       get extrema in lower # island
         PKWIN(1,IL) = MIN (PKWIN(1,IPK), PKWIN(1,JPK))
         PKWIN(2,IL) = MIN (PKWIN(2,IPK), PKWIN(2,JPK))
         PKWIN(3,IL) = MAX (PKWIN(3,IPK), PKWIN(3,JPK))
         PKWIN(4,IL) = MAX (PKWIN(4,IPK), PKWIN(4,JPK))
         PKMAX(IL) = MAX (PKMAX(IL), PKMAX(IH))
C                                       null upper number island
         PKWIN(1,IH) = 0
         PKWIN(2,IH) = 0
         PKWIN(3,IH) = 0
         PKWIN(4,IH) = 0
         PKMAX(IH) = 0.0
C                                       drop upper island
         IF (NPK.EQ.IH) NPK = NPK - 1
         DO 20 I = WIN(1),WIN(3)
            IF (OLD(I).EQ.IH) OLD(I) = IL
            IF (NEW(I).EQ.IH) NEW(I) = IL
 20         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE RMSFIT (NX, NY, IMAGE, NP, SCALE, HIST, NPT, RMS, IERR)
C-----------------------------------------------------------------------
C   RMSFIT does a robust rms fit to the image data and then loads a
C   histogram for Gaussian fitting
C   Inputs:
C      NX      I      Number X pixels
C      NY      I      Number Y pixels
C      NP      I      Half number of histogram points
C   Outputs:
C      IMAGE   R(*)   Big memory for image
C      SCALE   R      Scaling used in histogram
C      HIST    I(*)   histogram
C      NPT     I      Number points in histogram
C      IERR    I      Error code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NP, HIST(*), NPT, IERR
      REAL      IMAGE(NX,*), SCALE, RMS
C
      INTEGER   IY, IX, IROUND, I, J, IC, NPASS
      REAL      T
      DOUBLE PRECISION RSP, RSM, TT, SS, SQ, RM, RS, SN, FACT(7)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA FACT /5.0D0, 4.0D0, 3.5D0, 3.2D0, 2.8D0, 2.4D0, 3.0D0/
C-----------------------------------------------------------------------
      IERR = 0
C                                       robust rms
      RSP = 1.D5
      RSM = -RSP
      RS = 10.0D0 * RSP
      DO 40 NPASS = 1,7
         SS = 0.0D0
         SQ = 0.0D0
         SN = 0.0D0
         DO 30 IY = 1,NY
            DO 20 IX = 1,NX
               IF ((IMAGE(IX,IY).NE.FBLANK) .AND. (IMAGE(IX,IY).NE.0.0))
     *            THEN
                  TT = IMAGE(IX,IY)
                  IF ((TT.LT.RSP) .AND. (TT.GT.RSM)) THEN
                     SS = SS + TT
                     SQ = SQ + TT * TT
                     SN = SN + 1.0D0
                     END IF
                  END IF
 20            CONTINUE
 30         CONTINUE
         IF (SN.LE.0.0D0) THEN
            RSP = RSP + 3.0D0 * RS
            RSM = RSM - 3.0D0 * RS
         ELSE
            RM = SS / SN
            SQ = SQ / SN - RM * RM
            RS = SQRT (MAX(0.0D0, SQ))
            RSP = RM + FACT(NPASS) * RS
            RSM = RM - FACT(NPASS) * RS
            END IF
 40      CONTINUE
      RMS = RS
      IF (RS.LE.0.0D0) RS = 0.001 * ABS (RM)
      IF (RS.LE.0.0D0) RS = 0.0001
      SCALE = NP / (3.0D0 * RS)
      I = 4 * NP + 1
      IC = NP + 1
      CALL FILL (I, 0, HIST)
      DO 60 IY = 1,NY
         DO 50 IX = 1,NX
            IF ((IMAGE(IX,IY).NE.FBLANK) .AND. (IMAGE(IX,IY).NE.0.0))
     *         THEN
               T = IMAGE(IX,IY) * SCALE
               J = IROUND (T) + IC
               IF ((J.GE.1) .AND. (J.LE.I)) THEN
                  NPT = NPT + 1
                  HIST(J) = HIST(J) + 1
                  END IF
               END IF
 50         CONTINUE
 60      CONTINUE
C
 999  RETURN
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
C      LEN   I         Length of record in R words
C   Output:
C      WK    R(2,NP2)  Work buffer for keys.
C      IP    I(NP2)    Work buffer for permutation vector.
C      IER   I         Error code,   0 => OK
C   Adapted from ACM alg. #347 - June 1981
C-----------------------------------------------------------------------
      INTEGER   N, LEN, NP2, IP(NP2), KEY1, KEY2, IER
      REAL      A(LEN,N), WK(2,NP2)
C
      INTEGER   TP, TTP, IU(20), IL(20), I, IJ, J, K, KK, L, M
      REAL      T(2), TT(2)
C-----------------------------------------------------------------------
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
      SUBROUTINE PSORTI (A, N, NP2, KEY1, LEN, WK, IP, IER)
C-----------------------------------------------------------------------
C   OSORTI does a quick sort on the keys. Data sorted into ascending
C   order of the keys.  Converts A(2,*)/A(3,*) to Ra and Dec and then
C   sorts of one of them.
C   Input:
C      A     R(LEN,N)   Array of data to be sorted.
C      N     I          Number of records to be sorted
C      NP2   I          Number of words in work arrays = N+2
C      KEY1  I          Word in record of slower varying key
C                          = 5 => make latitude swap with longitude
C      LEN   I          Length of record in R words
C   Output:
C      WK    D(2,NP2)   Work buffer for keys.
C      IP    I(NP2)     Work buffer for permutation vector.
C      IER   I          Error code,   0 => OK
C                                    1 => input error
C   Adapted from ACM alg. #347 - June 1981
C-----------------------------------------------------------------------
      INTEGER   N, LEN, NP2, IP(NP2), KEY1, IER
      REAL      A(LEN,N)
      DOUBLE PRECISION WK(2,NP2)
C
      INTEGER   TP, TTP, IU(20), IL(20), I, IJ, J, K, KK, L, M, LK1,
     *   LK2, IERR
      DOUBLE PRECISION XX(3), T(2), TT(2)
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
      IER = 0
C                                       Which of the 3 coordinates
      LK1 = 1
      LK2 = 2
      IF (CORTYP(LOCNUM).EQ.1) THEN
         LK1 = 1
         LK2 = 2
      ELSE IF (CORTYP(LOCNUM).EQ.2) THEN
         LK1 = 2
         LK2 = 1
      ELSE IF (CORTYP(LOCNUM).EQ.3) THEN
         LK1 = 1
         LK2 = 3
      ELSE IF (CORTYP(LOCNUM).EQ.4) THEN
         LK1 = 3
         LK2 = 1
      ELSE IF (CORTYP(LOCNUM).EQ.5) THEN
         LK1 = 2
         LK2 = 3
      ELSE IF (CORTYP(LOCNUM).EQ.6) THEN
         LK1 = 3
         LK2 = 2
         END IF
C                                       sort on Latitude
      IF (KEY1.EQ.5) THEN
         I = LK1
         LK1 = LK2
         LK2 = I
         END IF
C                                       Build array of keys to sort
C                                       Minus sign makes order descend.
C                                       Note "blockers" put at ends.
      DO 5 KK = 1,N
         I = KK + 1
         CALL XYVAL (A(2,KK), A(3,KK), XX(1), XX(2), XX(3), IERR)
         IP(I) = KK
         WK(1,I) = XX(LK1)
         WK(2,I) = XX(LK2)
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
      SUBROUTINE SADSTR (X, Y, STR)
C-----------------------------------------------------------------------
C   Returns a string encoding the 1st 2 axes
C   Inputs:
C      X     D      Value on X axis
C      Y     D      Value on Y axis
C   Output:
C      STR   C*27   Axis value string, left justified
C   Common: /MAPHDR/ CATBLK
C           location common
C-----------------------------------------------------------------------
      CHARACTER STR*(*)
      DOUBLE PRECISION  X, Y
C
      DOUBLE PRECISION  Z
      INTEGER   NTY, IT, IX, IY, I, ID, IM, J
      REAL      SEC
      CHARACTER SXTYP(10)*4, CSIGN*1
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA NTY, SXTYP /10, 'LL  ','RA  ','RA--','GLON','ELON',
     *   'MM  ','DEC ','DEC-','GLAT','ELAT'/
C-----------------------------------------------------------------------
      STR = ' '
C                                       is it a special type?
      IX = 0
      IY = 0
      DO 10 IT = 1,NTY
         IF (CTYP(1,LOCNUM)(:4).EQ.SXTYP(IT)) IX = IT
         IF (CTYP(2,LOCNUM)(:4).EQ.SXTYP(IT)) IY = IT
 10      CONTINUE
      IF ((IX.LE.0) .OR. (IY.LE.0)) GO TO 999
C                                       loop over axes
      Z = X
      IT = IX
      DO 100 I = 1,2
         J = 1 + (I - 1) * 14
C                                       Right ascension
         IF (IT.LE.3) THEN
            IF (Z.LT.0.0D0) Z = Z + 36000.0D0
            Z = MOD (Z, 360.0D0)
            Z = Z / 15.0D0 + 0.00005D0 / (3600.0D0)
            ID = Z
            Z = (Z - ID) * 60.0D0
            IM = Z
            SEC = (Z - IM) * 60.0D0
            WRITE (STR(J:),1000) ID, IM, SEC
            J = J + 6
C                                       Longitude
         ELSE IF (IT.LE.5) THEN
            IF (Z.LT.0.0D0) Z = Z + 36000.0D0
            Z = MOD (Z, 360.0D0)
            Z = Z  + 0.0005D0 / (3600.0D0)
            ID = Z
            Z = (Z - ID) * 60.0D0
            IM = Z
            SEC = (Z - IM) * 60.0D0
            WRITE (STR(J:),1010) ID, IM, SEC
            J = J + 7
C                                       latitude
         ELSE
            CSIGN = ' '
            IF (Z.LT.0.0D0) THEN
               CSIGN = '-'
               Z = -Z
               END IF
            Z = MOD (Z, 360.0D0)
            Z = Z  + 0.0005D0 / (3600.0D0)
            ID = Z
            Z = (Z - ID) * 60.0D0
            IM = Z
            SEC = (Z - IM) * 60.0D0
            ID = MIN (ID, 99)
            WRITE (STR(J:),1020) CSIGN, ID, IM, SEC
            J = J + 7
            END IF
C                                       leading zeros
         IF (STR(J:J).EQ.' ') STR(J:J) = '0'
         J = J + 1
         IF (STR(J:J).EQ.' ') STR(J:J) = '0'
C                                       prepare for next axis
         Z = Y
         IT = IY
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I2.2,I3.2,F8.4)
 1010 FORMAT (I3.3,I3.2,F7.3)
 1020 FORMAT (A1,I2.2,I3.2,F7.3)
      END
      SUBROUTINE COMPOK (LOCBLC, LOCTRC, RNX, RNY, LOCRMS, LOCSUM, NXR,
     *   RMSIM, ISOK, ALLOK)
C-----------------------------------------------------------------------
C   checks the values of the fit component against the user-controlled
C   limits.
C   Inputs:
C      LOCBLC   R(7)    Window BLC
C      LOCTRC   R(7)    Window TRC
C      RNX      R       Number points of full image in X
C      RNY      R       Number points of full image in Y
C      LOCRMS   R       Fit RMS in residual window
C      LOCSUM   R       Sum of residual image
C      NXR      I       I dimension of RMSIM
C      RMSIM    R(*)    Image of RMS
C   Output:
C      ISOK     I(*)    Bit pattern of parameters that fail (0 => ok)
C                       one for each component
C      ALLOK    I       Bit pattern of all comps OR'ed
C-----------------------------------------------------------------------
      INTEGER   NXR, ISOK(*), ALLOK
      REAL      LOCBLC(7), LOCTRC(7), RNX, RNY, LOCRMS, LOCSUM,
     *   RMSIM(NXR,*)
C
      INTEGER   I, ZOR
      REAL      PEAK, FSUM, FLUX
      INCLUDE 'TVSAD.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       loop over components
      ALLOK = 0
      FSUM = 0.0
      DO 20 I = 1,NGAUSS
         PEAK = G(1,I) / RSCALE
         FLUX = PEAK * ABS (G(4,I) * G(5,I)) / (CB(1) * CB(2))
C                                       parameters in S/N units?
         IF (DORMSI) THEN
            CALL SADRMS (NXR, RMSIM, G(2,I), G(3,I), LNOISE)
            IF (LNOISE.EQ.FBLANK) THEN
               ISOK(I) = 5
               ALLOK = ZOR (ISOK(I), ALLOK)
               GO TO 20
               END IF
            END IF
         ISOK(I) = 0
         IF (PEAK.LE.LNOISE*DPARM(1)) ISOK(I) = ZOR (ISOK(I), 1)
         IF (FLUX.LE.DPARM(2)) ISOK(I) = ZOR (ISOK(I), 2)
         IF (LOCRMS.GE.SQRT((LNOISE*DPARM(3))**2 + (GAIN*PEAK)**2))
     *      ISOK(I) = ZOR (ISOK(I), 4)
         IF (G(4,I).GT.DPARM(4)) ISOK(I) = ZOR (ISOK(I), 8)
         IF (G(5,I).GT.DPARM(4)) ISOK(I) = ZOR (ISOK(I), 8)
         IF (ABS(LOCBLC(1)-1.0).LT.0.05) THEN
            IF (G(2,I).LE.LOCBLC(1)-DPARM(6)) ISOK(I) = ZOR(ISOK(I),64)
         ELSE
            IF (G(2,I).LE.LOCBLC(1)-DPARM(5)) ISOK(I) = ZOR(ISOK(I),16)
            END IF
         IF (ABS(LOCBLC(2)-1.0).LT.0.05) THEN
            IF (G(3,I).LE.LOCBLC(2)-DPARM(6)) ISOK(I) = ZOR(ISOK(I),128)
         ELSE
            IF (G(3,I).LE.LOCBLC(2)-DPARM(5)) ISOK(I) = ZOR(ISOK(I),32)
            END IF
         IF (ABS(LOCTRC(1)-RNX).LT.0.05) THEN
            IF (G(2,I).GE.LOCTRC(1)+DPARM(6)) ISOK(I) = ZOR(ISOK(I),64)
         ELSE
            IF (G(2,I).GE.LOCTRC(1)+DPARM(5)) ISOK(I) = ZOR(ISOK(I),16)
            END IF
         IF (ABS(LOCTRC(2)-RNY).LT.0.05) THEN
            IF (G(3,I).GE.LOCTRC(2)+DPARM(6)) ISOK(I) = ZOR(ISOK(I),128)
         ELSE
            IF (G(3,I).GE.LOCTRC(2)+DPARM(5)) ISOK(I) = ZOR(ISOK(I),32)
            END IF
         IF (ISOK(I).EQ.0) FSUM = FSUM + PEAK
         ALLOK = ZOR (ISOK(I), ALLOK)
 20      CONTINUE
C                                       component sum vs resid sum
      IF ((FSUM.NE.0.0) .AND.
     *   (ABS(LOCSUM).GT.SQRT(DPARM(7)**2+(GAIN*FSUM)**2))) THEN
         ALLOK = ZOR (ALLOK, 256)
         DO 30 I = 1,NGAUSS
            ISOK(I) = ZOR (ISOK(I), 256)
 30         CONTINUE
         END IF
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
     *      * RAD2DG + 90.0
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
      SUBROUTINE ORPRT (J, GG)
C-----------------------------------------------------------------------
C   ORPRT does the DOCRT=-4 special print format to file
C-----------------------------------------------------------------------
      INTEGER   J
      REAL      GG(6)
C
      INTEGER   SEQ, CVOL, CUID, JTRIM, IL, IPA, IPE, IBPA, IERR, I,
     *   IROUND, JT
      CHARACTER CNAME*12, CCLAS*6, CPTYPE*2, TLINE*160, PCODE*1,
     *   ACODE*1, AUNIT*2, RCODE*1
      REAL      PEAK, XINC, YINC, XP, YP, AXMAJ, AXMIN, BMAJ,
     *   BMIN, PRMS, SCALE, BSCALE, BMFACT
      INCLUDE 'TVSAD.INC'
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
      BSCALE = 1.0
      AUNIT = ' '
      IF (AXTYP(LOCNUM).EQ.1) THEN
         XINC = XINC * 3600.0
         YINC = YINC * 3600.0
         BSCALE = 3600.0
         IF (MAX(ABS(XINC),ABS(YINC)).LT.0.1) THEN
            SCALE = 1000.0
            ACODE = 'm'
            END IF
         AUNIT = 'as'
         END IF
      BMAJ = UCB(1) * BSCALE * SCALE
      BMIN = UCB(2) * BSCALE * SCALE
      IBPA = IROUND (HCB(3))
      PRMS = ORRMS * BMFACT
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
      IF (.NOT.OREXIS) THEN
         WRITE (TLINE,1000) ACODE, AUNIT, ACODE, AUNIT, ACODE, AUNIT,
     *      ACODE, AUNIT
         JT = JTRIM (TLINE)
         CALL ZTXIO ('WRIT', PRTLUN, PRTIND, TLINE(:JT), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1015) IERR, 0
            CALL MSGWRT (8)
            GO TO 999
            END IF
         OREXIS = .TRUE.
         END IF
      PEAK = GG(1) * BMFACT
      IF (PEAK.GT.0.0) THEN
         IF ((PEAK.LT.1.E-3) .AND. (ORERRA.LT.1.E-4)) THEN
            PCODE = 'u'
            PEAK = PEAK * 1.E6
            ORERRA = ORERRA * 1.E6
         ELSE IF ((PEAK.LT.1.0) .AND. (ORERRA.LT.0.1)) THEN
            PCODE = 'm'
            PEAK = PEAK * 1.E3
            ORERRA = ORERRA * 1.E3
         ELSE IF ((PEAK.LT.1.E3) .AND. (ORERRA.LT.100.)) THEN
            PCODE = ' '
         ELSE
            PCODE = 'k'
            PEAK = PEAK * 1.E-3
            ORERRA = ORERRA * 1.E-3
            END IF
      ELSE
         IF ((ABS(PEAK).LT.1.E-4) .AND. (ORERRA.LT.1.E-4)) THEN
            PCODE = 'u'
            PEAK = PEAK * 1.E6
            ORERRA = ORERRA * 1.E6
         ELSE IF ((ABS(PEAK).LT.0.1) .AND. (ORERRA.LT.0.1)) THEN
            PCODE = 'm'
            PEAK = PEAK * 1.E3
            ORERRA = ORERRA * 1.E3
         ELSE IF ((ABS(PEAK).LT.1.E2) .AND. (ORERRA.LT.100.)) THEN
            PCODE = ' '
         ELSE
            PCODE = 'k'
            PEAK = PEAK * 1.E-3
            ORERRA = ORERRA * 1.E-3
            END IF
         END IF
      XP = (GG(2) - CATR(KRCRP)) * XINC * SCALE
      YP = (GG(3) - CATR(KRCRP+1)) * YINC * SCALE
      ORERRX = ORERRX * ABS(XINC) * SCALE
      ORERRY = ORERRY * ABS(YINC) * SCALE
      AXMAJ = ORBMAJ * SCALE
      AXMIN = ORBMIN * SCALE
      ORERMA = ORERMA * SCALE
      ORERMI = ORERMI * SCALE
      IPA = IROUND (ORBPA)
      IPE = IROUND (ORERFI)
      WRITE (TLINE,1010) CNAME, DEPTH(1), PEAK, ORERRA, PCODE, XP,
     *   ORERRX, YP, ORERRY, REFRA(:2), REFRA(4:5), REFRA(7:13),
     *   REFDEC(:3), REFDEC(5:6), REFDEC(8:14), AXMAJ, ORERMA, AXMIN,
     *   ORERMI, IPA, IPE, BMAJ, BMIN, IBPA, PRMS, RCODE, J
      CALL REFRMT (TLINE(155:), ' ', I)
      JT = JTRIM (TLINE)
      CALL ZTXIO ('WRIT', PRTLUN, PRTIND, TLINE(:JT), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR, J
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT('!Source       Ch   Fnu _Jy Err    E-W Offset (',A1,A2,
     +   ')     N-S Offset (',A1,A2,
     *   ')       RA          DEC      Major(',A1,A2,')   Minor(',A1,A2,
     *   ') PA(deg)       Beam         RMS  #')
 1010 FORMAT (A12,I4,1X,2F7.3,A1,2(F12.3,F8.3),1X,2A2,A7,A3,A2,A7,
     *   2(F7.2,F6.2),I4,I3,F7.2,F6.2,I4,F7.2,A1,I6)
 1015 FORMAT ('ZTXIO ERROR',I4,' WRITING COMPONENT',I6)
      END
      SUBROUTINE TVALUE (GR, NX, NY, IMAGE, IBUT, IMGPOS, BUFFER, IRET)
C-----------------------------------------------------------------------
C   TVALUE performs interactive displays of map image values:
C   Special version - image values in core
C   Inputs:
C      GR        I        Graphics plane for lettering
C      NX        I        Number X pixels in image
C      NY        I        Number Y pixels in image
C      IMAGE     R(*)     Image values
C   In/out:
C      IBUT      I        < 0 => give messages, init cursor position
C                         returns button hit
C   Output:
C      IMGPOS    R(2)     Image pixel coords indicated
C      BUFFER    I(*)     Scratch buffer
C      IRET      I        Basic TV error code
C-----------------------------------------------------------------------
      INTEGER   GR, NX, NY, IBUT, IRET
      REAL      IMAGE(NX,NY), IMGPOS(2), BUFFER(*)
C
      CHARACTER STRING*16, PREFIX*5, ITRTYP(8)*2, LMTYPS(2)*2, LMTYPE*2
      INTEGER   MIND, IG, IG1, IG2, ITW(3), NPIX, NROW, MAG, IX0, IY0,
     *   IX, IY, IP, ECOUNT, QUAD, ITR, ICMASK, ZAND, ISCX, ISCY, I,
     *   INCNO, INVOL, LDEP(5), ITG1, ITG2, ITEMP, IX1, IY1, MSGSAV,
     *   LBUT
      REAL      PPOS(2), RPOS(2), PIXVAL, PV1, PV2
      LOGICAL   T, F, EQUAL, DOIT, BLNKD, NOQUAD, NOCURS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA ITRTYP /'LN','LG','L2','SQ','NE','NG','N2','NQ'/
      DATA LMTYPS /'WE','ZZ'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      MIND = 0
      QUAD = 0
      CALL ZTIME (ITW)
      MSGSAV = MSGSUP
      NOQUAD = .TRUE.
      NOCURS = IBUT.LT.0
C                                       Turn on graphics
      IG1 = MIN (GR, NGRAPH)
      IF (IG1.LE.0) IG1 = MIN (2, NGRAPH)
      IG2 = 0
      IF (NGRAPH.GE.4) IG2 = NGRAPH
      ITG1 = NGRAY + IG1
      ITG2 = NGRAY + IG2
      CALL YSLECT ('ONNN', ITG1, 0, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL YZERO (ITG1, IRET)
      IF (IRET.NE.0) GO TO 970
      IF (IG2.NE.0) THEN
         CALL YSLECT ('ONNN', ITG2, 0, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL YZERO (ITG2, IRET)
         IF (IRET.NE.0) GO TO 970
         END IF
C                                       Display area: location, size
C                                       Approx corr for zoom
      NPIX = 13 * CSIZTV(1)
      NROW = 4 * CSIZTV(2)
      ISCX = 0
      ISCY = 0
      MAG = 1 + TVZOOM(1)
      IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
      IX0 = WINDTV(1) - (MAG-1)/2
      IY0 = WINDTV(4) - MAG*NROW + 1 - (MAG-1)/2
      IF (MAG.GT.1) IY0 = IY0 + MAG
      IX0 = (IX0 - TVZOOM(2)) / MAG + TVZOOM(2) - TVSCGX
      IY0 = (IY0 - TVZOOM(3)) / MAG + TVZOOM(3) - TVSCGY
      IF (IX0+NPIX-1.GT.MAXXTV(1)) IX0 = 1
      IF (IY0+NROW-1.GT.MAXXTV(2)) IY0 = MAXXTV(2) - NROW + 1
      IX1 = IX0 + NPIX - 1
      IY1 = IY0 + NROW - 1
      CALL YHOLD ('ONNN', IRET)
      CALL YFILL (ITG1, IX0, IY0, IX1, IY1, 0, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      IF (IG2.GT.0) THEN
         CALL YFILL (ITG2, IX0, IY0, IX1, IY1, 1, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         END IF
      CALL YHOLD ('OFFF', IRET)
C                                       CURVALUE (from disk file)
C                                       no image yet
      CALL FILL (4, 0, CATBLK(IICOR))
      CATBLK(IICNO) = 0
      LDEP(1) = -10000
      IF (NOCURS) THEN
         WRITE (MSGTXT,1100)
         CALL MSGWRT (1)
         WRITE (MSGTXT,1101)
         CALL MSGWRT (1)
C                                       turn on cursor
         RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
         RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
         CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IRET)
         IF (IRET.NE.0) GO TO 970
         END IF
      IP = 0
      ECOUNT = 0
      IG = IG1 + NGRAY
      PPOS(1) = 0.0
      PPOS(2) = 0.0
C                                       Cursor read loop point
 110  CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IRET)
      IF ((IBUT.GE.1) .OR. (IRET.NE.0)) GO TO 970
      LBUT = IBUT
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (.NOT.DOIT) GO TO 110
C                                       Find new image catalog block
         QUAD = 0
         CALL YCURSE ('FXIT', F, T, RPOS, QUAD, I, IRET)
         IX = RPOS(1) + 0.51
         IY = RPOS(2) + 0.51
         INCNO = CATBLK(IICNO)
         INVOL = CATBLK(IIVOL)
         IF ((IX.LT.CATBLK(IICOR)) .OR. (IX.GT.CATBLK(IICOR+2)) .OR.
     *      (IY.LT.CATBLK(IICOR+1)) .OR. (IY.GT.CATBLK(IICOR+3))) THEN
            DO 115 IP = 1,NGRAY
               ITEMP = 2 ** (IP - 1)
               IF (ZAND(TVLIMG(QUAD),ITEMP).NE.0) THEN
                  CALL YCREAD (IP, IX, IY, CATBLK, IRET)
                  IF (IRET.EQ.0) GO TO 120
                  IF (IRET.NE.1) GO TO 960
                  END IF
 115           CONTINUE
C                                       No or invalid image here
 116        ECOUNT = ECOUNT + 1
            CALL FILL (4, 0, CATBLK(IICOR))
            CATBLK(IICNO) = 0
            IF (ECOUNT.LT.1) THEN
               WRITE (MSGTXT,1116) IX, IY
               CALL MSGWRT (1)
               END IF
            GO TO 110
C                                       Set up image reads
 120        CALL H2CHR (2, KHPTYO, CATH(KHPTY), LMTYPE)
            IF (LMTYPE.EQ.LMTYPS(2)) GO TO 116
            BLNKD = .FALSE.
C                                       Scaling parms for TV pixvals
            CALL COPY (5, CATBLK(IIDEP), LDEP)
            ICMASK = 2 ** (IP-1)
            ITR = 1
            CALL H2CHR (2, 1, CATH(IITRA), LMTYPE)
            DO 135 I = 1,8
               IF (LMTYPE.EQ.ITRTYP(I)) ITR = I
 135           CONTINUE
            ECOUNT = 0
            END IF
C                                       From TV for wedges
         CALL IMA2MP (RPOS, IMGPOS)
C                                       interpolate
         IX = IMGPOS(1) + 0.0001
         IY = IMGPOS(2) + 0.0001
         BLNKD = (IMAGE(IX,IY).EQ.FBLANK) .OR.
     *      (IMAGE(IX+1,IY).EQ.FBLANK) .OR.(IMAGE(IX,IY+1).EQ.FBLANK)
     *      .OR.(IMAGE(IX+1,IY+1).EQ.FBLANK)
         IF (.NOT.BLNKD) THEN
            PIXVAL = IMGPOS(1) - IX
            PV1 = (1.0-PIXVAL)*IMAGE(IX,IY) + PIXVAL*IMAGE(IX+1,IY)
            PV2 = (1.0-PIXVAL)*IMAGE(IX,IY+1) + PIXVAL*IMAGE(IX+1,IY+1)
            PIXVAL = IMGPOS(2) - IY
            PIXVAL = (1.0-PIXVAL) * PV1 + PIXVAL * PV2
            END IF
C                                       Write text to TV
         IF ((IX.LE.9999) .AND. (IY.LE.9999)) THEN
            WRITE (STRING,1170) IX, IY
         ELSE
            WRITE (STRING,1171) IX, IY
            END IF
         IY = IY0 + 3*CSIZTV(2)
         CALL YHOLD ('ONNN', IRET)
         CALL YSLECT ('OFFF', ITG1, 0, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL IMCHAR (IG, IX0, IY, 0, 0, STRING(:13), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         IF (.NOT.BLNKD) THEN
            CALL METSCA (PIXVAL, PREFIX, EQUAL)
            WRITE (STRING,1172) PIXVAL
            IY = IY - 1.5*CSIZTV(2)
            CALL IMCHAR (IG, IX0, IY, 0, 0, STRING(:10), BUFFER, IRET)
            IF (IRET.NE.0) GO TO 970
            STRING = PREFIX
            CALL H2CHR (8, 1, CATH(KHBUN), STRING(6:))
            CALL IMCHAR (IG, IX0, IY0, 0, 0, STRING(:13), BUFFER, IRET)
            IF (IRET.NE.0) GO TO 970
         ELSE
            STRING = 'B  BLANKED'
            IY = IY - 1.5*CSIZTV(2)
            CALL IMCHAR (IG, IX0, IY, 0, 0, STRING(:10), BUFFER, IRET)
            IF (IRET.NE.0) GO TO 970
            STRING = ' '
            CALL IMCHAR (IG, IX0, IY0, 0, 0, STRING(:13), BUFFER, IRET)
            IF (IRET.NE.0) GO TO 970
            END IF
         IF (IG2.GT.0) CALL YFILL (ITG2, IX0, IY0, IX1, IY1, 1,
     *      BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL YSLECT ('ONNN', ITG1, 0, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL YHOLD ('OFFF', IRET)
         GO TO 110
C-----------------------------------------------------------------------
C                                       Close downs
C                                       Img Catlg error
 960  WRITE (MSGTXT,1960) IRET
      CALL MSGWRT (6)
      GO TO 975
C                                       TV error possibly
 970  IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1970) IRET
         CALL MSGWRT (6)
         END IF
C                                       Close things
 975  IF (NOCURS) CALL YCURSE ('OFFF', F, F, RPOS, QUAD, I, I)
      ITEMP = 2 ** NGRAY
      IF ((ISCX.NE.0) .OR. (ISCY.NE.0)) CALL YSCROL (ITEMP, ISCX,
     *   ISCY, F, I)
      CALL YZERO (ITG1, I)
      IF (IG2.NE.0) CALL YZERO (ITG2, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Cursor selects which pixel is displayed')
 1101 FORMAT ('Hit any button to exit')
 1116 FORMAT ('TVALUE: ',2I7,' NOT IN VALID IMAGE')
 1170 FORMAT ('X=',I4,' Y=',I4)
 1171 FORMAT (I6,I7)
 1172 FORMAT ('B=',F8.3)
 1960 FORMAT ('TVALUE: IMAGE CAT FILE IO ERROR',I7)
 1970 FORMAT ('TVALUE: TV ACTION IO ERROR',I7)
      END
