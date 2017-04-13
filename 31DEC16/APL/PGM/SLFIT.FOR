LOCAL INCLUDE 'SLFIT.INC'
C                                       Local include for SLFIT
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   IDOPOS(4), IDOMAX(4), IDOWTH(4), ITTER, IDROP(2), IVOL,
     *   ISLOT, IVER, JJC
      REAL      GM(48), SLPOS(MAXIMG), RANGE(2), XCEN
      LOGICAL   QUICK, SPECTR
      DOUBLE PRECISION DATA(MAXIMG)
      COMMON /GDATA/ DATA, SLPOS, QUICK, IDOPOS, IDOMAX, IDOWTH, ITTER,
     *   IDROP, IVOL, ISLOT, IVER, SPECTR, GM, RANGE, JJC, XCEN
LOCAL END
      PROGRAM SLFIT
C-----------------------------------------------------------------------
C! Task to fit up to 4 gaussians to portions of a slice (SL) file
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2005, 2007, 2009, 2011-2016
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
C   SLFIT fits up to 4 gaussian components to portions of a slice file
C   INPUTS:   (from AIPS)
C      INNAME    R(3)     name of image file.
C      INCLASS   R(2)     class of image file.
C      INSEQ     R        sequence number of image file.
C      INDISK    R        disk volume number. 0 means try all.
C      INVERS    R        version number of slice file, 0 means try
C                            latest version.
C      BDROP     R        # Slice points not fitted on left
C      EDROP     R        # Slice points not fitted on right
C      NGAUSS    R        # of gaussian components in fitted curve.
C      GPOS      R(2,4)   position of gaussian components. Only
C                            GPOS(1,*) used in 1D fitting.  Units are
C                            those printed by TKSLICE with label type of
C                            LTYPE.
C      GMAX      R(4)     maximum value of gaussian components in
C                            units printed by TKSLICE.
C      GWIDTH    R(3,4)   width of gaussian components in units
C                            printed by TKSLICE.  Only GWIDTH(1,*) used
C      LTYPE     R        labeling type used when selecting units for
C                            initial guess. 1, 2, 3 RA/dec
C                            4=center rel., 5=subslice center-rel
C      DOPOS     R(2,4)   < 0 (false) means hold position constant.
C      DOMAX     R(4)     < 0 (false) means hold position constant.
C      DOWIDTH   R(3,4)   < 0 (false) means hold position constant.
C-----------------------------------------------------------------------
C                                       Subroutine being passed
      INCLUDE 'INCS:PMAD.INC'
      EXTERNAL  GFUNC
C
      INTEGER   INPTS, INPARM, ISLHDR(256), IPVT(15), ISFIND, ISLUN,
     *   IERR, INFO, NPTS, NPARM, WSIZE
      DOUBLE PRECISION PARMS(15), FJAC(15,15), WORK(MAXIMG+200),
     *   FVEC(MAXIMG), TOL
      INCLUDE 'SLFIT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA ISLUN /27/
C-----------------------------------------------------------------------
C                                       Get map header, open slice file
C                                       set location common. Read
C                                       slice header. Convert to more
C                                       reasonable units for GPOS, etc.
      CALL GAUINI (ISLUN, ISFIND, NPARM, ISLHDR, NPTS, PARMS,
     *   TOL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Fit gaussians using initial
C                                       guess.
      INPTS = NPTS
      INPARM = NPARM + JJC
      WSIZE = MAXIMG+200
      CALL LMSTR1 (GFUNC, INPTS, INPARM, PARMS, FVEC, FJAC, 15,
     *   TOL, INFO, IPVT, WORK, WSIZE)
C                                       Put answers in proper form.
C                                       Update slice header, save info
C                                       at the end of the slice file.
C                                       Calculate error.
C                                       Print results. Close down.
      CALL GAUFIN (ISLUN, ISFIND, INFO, NPARM, ISLHDR, PARMS, IPVT,
     *   FJAC, FVEC)
C
 999  STOP
      END
      SUBROUTINE GAUINI (ISLUN, ISFIND, NPARM, ISLHDR, NPTS, PARMS,
     *   TOL, IERR)
C-----------------------------------------------------------------------
C   GAUINI will initialize the location common for gaussian fitting
C   and convert the position, max, width from physical (user input)
C   units to slice points.
C   INPUTS:  ISLUN   I   logical unit number of open slice file.
C   OUTPUTS: ISFIND  I   FTAB indicator of the slice file.
C            NPARM   I   number of parameters in the fitted curve
C                    (3 * no. of gaussian components).
C            ISLHDR  I(256)   slice extension file header.
C            NPTS    I   no. of slice data points to be fitted.
C            PARMS   D(15)   parameters for fitted curve.  Max, pos,
C                    width, max, ...
C            TOL     D   tolerance parameter for fitting.
C            IERR    I   error code. 0=ok.
C  COMMON    /GDATA/ DATA D(8192)   contains the slice data points.
C                    IDOPOS I(4)   -1 means hold position constant.
C                    IDOMAX I(4)   -1 means hold max amp constant.
C                    IDOWTH I(4)   -1 means hold half width constant.
C                    ITTER  I   no. of iterations initialized to zero.
C                    IDROP I(2)   no. of slice points dropped from
C                    beginning and end of slice.
C-----------------------------------------------------------------------
      INTEGER   ISLUN, ISFIND, NPARM, ISLHDR(*), NPTS, IERR
      DOUBLE PRECISION PARMS(15), TOL
C
      CHARACTER PRGNAM*6, INNAM*12, INCLS*6, INTYP*2, TEXT(2)*80,
     *   JY*8, OPTYPE*4
      HOLLERITH XINNAM(3), XINCLS(2)
      DOUBLE PRECISION XVAL, YVAL, DFBLK(128), FQFREQ, SCALEF, OFFSET
      REAL      INSEQ, INDSK, INVER, BDROP, EDROP, NGAUSS, XPOS(2,4),
     *   XMAX(4), XWIDTH(3,4), LTYPE, DOPOS(2,4), DOMAX(4), DOWTH(3,4),
     *   BLC(2), TRC(2), CH(4), FBLK(256), RBLK(256), GPOS(2,4),
     *   GMAX(4), GWIDTH(3,4), XPIX, YGAP, YPIX, FQFINC, ORDER
      INTEGER  IDEPTH(5), IFBLK(256), ILABEL, INGAUS, IS, IXBLC, IXTRC,
     *   NTEXT, I, IERR2, IMBLK, IMFIND, IMLUN, INPRMS, IPOS, IRETCD,
     *   ISEQ, IUSER, J, NRPBLK, NRPFP, IRRN, IROUND
      LOGICAL   NOSAVE, NOEXCL, WAIT, ISOLD
      INCLUDE 'SLFIT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      COMMON /INPUT/ XINNAM, XINCLS, INSEQ, INDSK, INVER, BDROP, EDROP,
     *   NGAUSS, LTYPE, ORDER, XMAX, XPOS, XWIDTH, DOMAX, DOPOS, DOWTH
      EQUIVALENCE (NOEXCL, NOSAVE)
      EQUIVALENCE (IFBLK, FBLK, DFBLK)
      DATA PRGNAM /'SLFIT '/
      DATA NOSAVE, WAIT /.FALSE.,.TRUE./
      DATA IMLUN /17/
      DATA JY /'JY'/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE., CATBLK)
      CALL VHDRIN
C                                       Get input values from AIPS.
C                                       Fixed PPM 1996.09.30: was 59
      INPRMS = 61
      IRETCD = 0
      CALL GTPARM (PRGNAM, INPRMS, QUICK, XINNAM, CATBLK, IERR)
      QUICK = .FALSE.
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRETCD = 8
         END IF
      IF (IRETCD.NE.0) GO TO 990
      IRETCD = 8
      CALL RCOPY (48, XMAX, GM)
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XINNAM, INNAM)
      CALL H2CHR (6, 1, XINCLS, INCLS)
      INTYP = ' '
C                                       Convert R   to I.
      ISEQ = INSEQ + 0.5
      IUSER = NLUSER
      IDROP(1) = BDROP + 0.5
      IDROP(2) = EDROP + 0.5
      INGAUS = NGAUSS + 0.5
      IVER = INVER + 0.5
      IVOL = INDSK + 0.5
      ILABEL = IROUND (LTYPE)
      I = MOD (ABS(ILABEL), 100)
      IF (I.LT.2) THEN
         I = 3
         IF (ILABEL.LT.0) THEN
            ILABEL = (ILABEL/100)*100 - I
         ELSE
            ILABEL = (ILABEL/100)*100 + I
            END IF
         END IF
      DO 20 I = 1,4
         IDOPOS(I) = 1
         IDOMAX(I) = 1
         IDOWTH(I) = 1
         IF (DOPOS(1,I).LE.0.0) IDOPOS(I) = -1
         IF (DOMAX(I).LE.0.0) IDOMAX(I) = -1
         IF (DOWTH(1,I).LE.0.0) IDOWTH(I) = -1
 20      CONTINUE
C                                       Check inputs
      JJC = IROUND (ORDER) + 1
      JJC = MAX (0, MIN (3, JJC))
      IF (INGAUS.LT.1) INGAUS = 1
      IF (INGAUS.GT.4) INGAUS = 4
      DO 30 I = 1,INGAUS
         IF (XWIDTH(1,I).EQ.0) GO TO 985
 30      CONTINUE
C                                       Open image, get header, close.
      CALL MAPOPN ('READ', IVOL, INNAM, INCLS, ISEQ, INTYP, IUSER,
     *   IMLUN, IMFIND, ISLOT, CATBLK, DATA, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MAPCLS ('READ', IVOL, ISLOT, IMLUN, IMFIND, CATBLK, NOSAVE,
     *   DATA, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open slice extension file.
      NRPFP = 256
      CALL OPEXT ('SL', IVOL, ISLOT, IVER, ISLUN, NOEXCL, WAIT,
     *   ISFIND, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Read slice header.
      IRRN = 1
      CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, ISLHDR, IERR)
      IF (IERR.NE.0) GO TO 980
      NPARM = 3 * INGAUS
      NPTS = ISLHDR(57) - IDROP(1) - IDROP(2)
C                                       No of points too small.
      IF (NPTS.LT.NPARM+JJC) THEN
         WRITE (MSGTXT,1030) NPTS, IDROP
         CALL MSGWRT (5)
         IDROP(1) = 0
         IDROP(2) = 0
         NPTS = ISLHDR(57)
         END IF
      XCEN = (IDROP(1) + 1.0 + ISLHDR(57) - IDROP(2)) / 2.0
C                                       # points too large
      IF (NPTS.GT.8192) THEN
         MSGTXT = 'CAN ONLY FIT A 8192-POINT PORTION OF A SLICE'
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       Read SLICE inputs stuff.
      IRRN = 2
      CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, FBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      IS = 11
      IXBLC = IS + 8
      IXTRC = IS + 15
      RANGE(1) = FBLK(34)
      RANGE(2) = FBLK(35)
      CALL H2CHR (4, 1, FBLK(33), OPTYPE)
      IF ((OPTYPE.EQ.'AVER') .OR. (OPTYPE.EQ.'FLUX') .OR.
     *   (OPTYPE.EQ.'LGFL') .OR. (OPTYPE.EQ.'LNFL') .OR.
     *   (OPTYPE.EQ.'ADER') .OR. (OPTYPE.EQ.'FDER') .OR.
     *   (OPTYPE.EQ.'LGAV') .OR. (OPTYPE.EQ.'LNAV')) THEN
         ISOLD = .FALSE.
      ELSE
         ISOLD = .TRUE.
         END IF
C                                       old - force FQ to zero on disk
      IF (ISOLD) THEN
         FQFINC = 0.0
         FQFREQ = 0.0D0
         FBLK(36) = 0.0
         DFBLK(19) = 0.0D0
         IRRN = 2
         CALL ZFIO ('WRIT', ISLUN, ISFIND, IRRN, FBLK, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       new pick up FQ if any
      ELSE
         FQFINC = FBLK(36)
         FQFREQ = DFBLK(19)
         END IF
      IF (OPTYPE.EQ.'FLUX') CALL CHR2H (8, JY, 1, CATH(KHBUN))
      IF (OPTYPE.EQ.'LGFL') CALL CHR2H (8, 'LOG10 JY', 1, CATH(KHBUN))
      IF (OPTYPE.EQ.'LNFL') CALL CHR2H (8, 'LN (JY)', 1, CATH(KHBUN))
      IF (OPTYPE.EQ.'FDER') CALL CHR2H (8, 'dJY / dx', 1, CATH(KHBUN))
      BLC(2) = 1.0
      TRC(2) = 40000.0
C                                       Initialize location common.
      LOCNUM = 1
      SPECTR = ABS(FBLK(IXBLC+2)-FBLK(IXTRC+2)).GE.1.0
      CALL SLBINI (IDROP, ISLHDR(57), RANGE, BLC, TRC, FBLK(IXBLC),
     *   FBLK(IXTRC), FQFREQ, FQFINC, IDEPTH, ILABEL, YGAP, CH, TEXT,
     *   NTEXT)
C                                       Convert to slice pt. units.
      SCALEF = (RANGE(2)-RANGE(1)) / 39999.0D0
      OFFSET = RANGE(1) - SCALEF
C                                       Spectral slice (ISPEC, BLSUM)
      IF (SPECTR) THEN
         DO 40 I = 1,INGAUS
C                                       Position and maximum.
            XVAL = XPOS(1,I) - RPVAL(1,LOCNUM)
            YVAL = XMAX(I) - RPVAL(2,LOCNUM)
            XPIX = XVAL / AXINC(1,LOCNUM) + RPLOC(1,LOCNUM)
            YPIX = YVAL / AXINC(2,LOCNUM) + RPLOC(2,LOCNUM)
            GPOS(1,I) = XPIX
            GMAX(I) = SCALEF * YPIX  +  OFFSET
C                                       Half width.
            XVAL = XPOS(1,I) - (XWIDTH(1,I) / 2.0) - RPVAL(1,LOCNUM)
            XPIX = XVAL / AXINC(1,LOCNUM) + RPLOC(1,LOCNUM)
            GWIDTH(1,I) = 2.0 * ABS (GPOS(1,I) - XPIX)
 40         CONTINUE
C                                       slice on 1st 2 axes
      ELSE
         DO 50 I = 1,INGAUS
C                                       Position and maximum.
            XVAL = XPOS(1,I)
            YVAL = XMAX(I)
            CALL XYPIX (XVAL, YVAL, XPIX, YPIX, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1040) IERR, I
               CALL MSGWRT (8)
               GO TO 980
               END IF
            XVAL = XPOS(1,I) - (XWIDTH(1,I) / 2.0)
            GPOS(1,I) = XPIX
            GMAX(I) = SCALEF * YPIX  +  OFFSET
C                                       Half width.
            CALL XYPIX (XVAL, YVAL, XPIX, YPIX, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1042) IERR, I
               CALL MSGWRT (8)
               GO TO 980
               END IF
            GWIDTH(1,I) = 2.0 * ABS (GPOS(1,I) - XPIX)
 50         CONTINUE
         END IF
C                                       Calc no. REALs per IO block.
      NRPBLK = 256
C                                       Load 1st record.
      IPOS = MOD (IDROP(1), NRPBLK) + 1
      IRRN = IDROP(1) / NRPBLK  +  3
      CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, RBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Calculate actual slice values.
      I = 0
      DO 70 J = 1,NPTS
C                                       Read new buffer of slice values
         IF (IPOS.GT.NRPBLK) THEN
            IRRN = IRRN + 1
            CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, RBLK, IERR)
            IF (IERR.NE.0) GO TO 980
            IPOS = 1
            END IF
C                                       Move data into array
         IF (RBLK(IPOS).NE.FBLANK) THEN
            I = I + 1
            DATA(I) = RBLK(IPOS)
            SLPOS(I) = J
            END IF
         IPOS = IPOS + 1
 70      CONTINUE
      NPTS = I
C                                       Convert parms to right form.
      J = 1
      CALL DFILL (15, 0.0D0, PARMS)
      DO 80 I = 1,INGAUS
         PARMS(J) = GMAX(I)
         PARMS(J+1) = GPOS(1,I)
         PARMS(J+2) = GWIDTH(1,I)
         J = J + 3
 80      CONTINUE
C                                       Calculate other parameters
C                                       needed by LMSTR1.
      TOL = 1.0D-5
      ITTER = 0
      GO TO 999
C                                       Error after slice file opened.
 980  CALL ZCLOSE (ISLUN, ISFIND, IERR2)
      GO TO 990
C                                       Zero half widths not allowed.
 985  WRITE (MSGTXT,1985) INGAUS
      CALL MSGWRT (6)
C                                       Error before slice file opened.
 990  MSGTXT = 'FATAL ERROR IN SLFIT'
      CALL MSGWRT (6)
C                                       Close down procedure.
C                                        Normal ending
      CALL DIETSK (IRETCD, QUICK, IMBLK)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TROUBLE GETTING PARMS FROM AIPS. IERR=',I5)
 1030 FORMAT ('Ignoring BDROP, EDROP',2I6,' too big for no. pts.',I6)
 1040 FORMAT ('ERROR',I3,' FINDING PIXEL POSITION OF COMPONENT',I3,
     *   ' PEAK')
 1042 FORMAT ('ERROR',I3,' FINDING PIXEL POSITION OF COMPONENT',I3,
     *   ' HALF POINT')
 1985 FORMAT ('Zero half width for component',I3)
      END
      SUBROUTINE GFUNC (M, N, PARMS, FVEC, FJROW, IFLAG)
C-----------------------------------------------------------------------
C   This routine is called by the Argonne package to calculate the
C   difference between the current fit and the actual data OR the
C   Jacobian for this difference.
C     INPUTS:  M        I Number of data points in slice (adj. array.
C                       dim.).
C              N        I No. of parameters (adj. array. dim.
C                       NGAUSS * 3)
C              PARMS    D(N)   parameters of gaussian components,
C                       GMAX(1), GPOS(1), GWIDTH(1), GMAX(2), ...
C              IFLAG    I   1=calculate difference for current guess.
C                           2=calculate jacobian for current guess.
C    COMMON:   GDATA
C              DATA     R(?)   Origional slice data points.
C              IDOPOS   I(4)   -1 means hold corresponding position
C                       parameter constant.
C              IDOMAX   I(4)   -1 means hold corresponding maximum
C                       amplitude parameter constant.
C              IDOWTH   I(4)   -1 means hold corresponding half
C                       width parameter constant.
C              ITTER    I   number of calls to evaluate FVEC.
C    OUTPUTS:  FVEC     D(M)   Slice data points minus data points
C                       evaluated for current guess.
C              FJROW    D(N)   Row (IFLAG - 1) of Jacobian.
C-----------------------------------------------------------------------
      INTEGER   N, M, IFLAG
      DOUBLE PRECISION PARMS(N), FVEC(M), FJROW(N)
C
      DOUBLE PRECISION AMP, POS, SIG, EFACT, RES2, TSIG2, X
      INTEGER   IGAUSS, IDATA, IAMP, IPOS, ISIG, K, NGAUS
      INCLUDE 'SLFIT.INC'
C-----------------------------------------------------------------------
C                                       Determine difference between
C                                       data and current fit.
      NGAUS = N - JJC
      IF (IFLAG.LE.1) THEN
         ITTER = ITTER + 1
         DO 20 IDATA = 1,M
            FVEC(IDATA) = DATA(IDATA)
            DO 15 IGAUSS = 3,NGAUS,3
               IAMP = IGAUSS-2
               IPOS = IGAUSS-1
               ISIG = IGAUSS
               AMP = PARMS(IAMP)
               POS = PARMS(IPOS)
               SIG = PARMS(ISIG)
               X = SLPOS(IDATA) + IDROP(1)
               RES2 = 2.772D0 * (X - POS)**2
               TSIG2 = SIG**2
               EFACT = EXP (-1.D0 * RES2 / TSIG2)
               FVEC(IDATA) = FVEC(IDATA) - AMP*EFACT
   15          CONTINUE
            IF (JJC.GE.1) FVEC(IDATA) = FVEC(IDATA) - PARMS(NGAUS+1)
            IF (JJC.GE.2) FVEC(IDATA) = FVEC(IDATA) - PARMS(NGAUS+2) *
     *         (IDATA - XCEN)
            IF (JJC.GE.3) FVEC(IDATA) = FVEC(IDATA) - PARMS(NGAUS+3) *
     *         (IDATA - XCEN) * (IDATA - XCEN)
   20       CONTINUE
C                                       Calculate Jacobian.
      ELSE
         IDATA = IFLAG - 1
         K = 0
         DO 110 IGAUSS = 3,NGAUS,3
            K = K + 1
            IAMP = IGAUSS-2
            IPOS = IGAUSS-1
            ISIG = IGAUSS
            AMP = PARMS(IAMP)
            POS = PARMS(IPOS)
            SIG = PARMS(ISIG)
            X = SLPOS(IDATA) + IDROP(1)
            RES2 = 2.772 * (X - POS)**2
            TSIG2 = SIG**2
            EFACT = EXP (-1.D0 * RES2 / TSIG2)
            FJROW(IAMP) = 0.0D0
            IF (IDOMAX(K).GE.0) FJROW(IAMP) = -EFACT
            FJROW(IPOS) = 0.0D0
            IF (IDOPOS(K).GE.0) FJROW(IPOS) = -5.544D0 * AMP * EFACT *
     *         (X - POS) / (SIG*SIG)
            FJROW(ISIG) = 0.0D0
            IF (IDOWTH(K).GE.0) FJROW(ISIG) = -2.D0 * AMP * EFACT *
     *         RES2 / (SIG ** 3)
 110        CONTINUE
         IF (JJC.GE.1) FJROW(NGAUS+1) = -1.0
         IF (JJC.GE.2) FJROW(NGAUS+2) = -(IDATA - XCEN)
         IF (JJC.GE.3) FJROW(NGAUS+3) = -(IDATA - XCEN) * (IDATA - XCEN)
         END IF
C
 999  RETURN
      END
      SUBROUTINE GAUFIN (ISLUN, ISIND, INFO, NPARM, ISLHDR, PARMS,
     *   IPVT, FJAC, FVEC)
C-----------------------------------------------------------------------
C   GAUFIN will write the final fitted slice pt units into the slice
C   file, then the answers will be converted into physical units for
C   printing.
C   INPUTS: ISLUN   I   logical unit number of a slice file opened
C                       with EXTINI.
C           ISIND   I   FTAB index of the slice file.
C           NGAUSS  I   No. of gaussian components in fitted curve.
C           ISLHDR  I(256)   standard extension file header.
C           PARMS   D(15)   fitted parameters in slice pt. units.
C           IPVT    I(15)    info from LMSTR1 for error computation.
C           FJAC    D(15,15)   info from LMSTR1 for error computation.
C           FVEC    D(8192)
C   COMMON /GDATA/  IDROP   I(2)   slice points at beginning and end of
C                   the slice file that were not fitted.
C                   IDOPOS  I(4)   1's mean position for component held
C                   constant.
C                   IDOMAX  I(4)   1's mean max for component held
C                   constant.
C                   IDOWTH  I(4)   1;s mean halfwidths held constant.
C                   ITTER   I   no. of function evaluations.
C-----------------------------------------------------------------------
      INTEGER   ISLUN, ISIND, INFO, NPARM, ISLHDR(*), IPVT(15)
      DOUBLE PRECISION PARMS(15), FJAC(15,15), FVEC(*)
C
      DOUBLE PRECISION EPARMS(15), SCALEF, XVAL, YVAL, ZVAL, FNORM,
     *   ENORM, WORK(15), TOL, OFFSET, XPIX, YPIX, TEMP, SFJAC(15,15),
     *   RMS
      REAL      XPOS(4), XMAX(4), XWTH(4), RBLK(256), EPOS(4), EMAX(4),
     *   EWTH(4), XP, YP
      INTEGER   IBLK(256), IERR, IS, NEXP, NPTS, NGAUSS, I, IRETCD, J,
     *   INPTS, INPARM, IRRN
      CHARACTER MUNITS*8, PNAME*48, REASON*30
      INCLUDE 'SLFIT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      EQUIVALENCE (RBLK, IBLK)
C-----------------------------------------------------------------------
      IRETCD = 8
      CALL H2CHR (8, 1, CATH(KHBUN), MUNITS)
C                                       Get errors.
      NPTS = ISLHDR(57) - IDROP(1) - IDROP(2)
      NGAUSS = NPARM / 3
      INPTS = NPTS
      FNORM = ENORM (INPTS, FVEC)
      INPARM = NPARM + JJC
      CALL DPCOPY (225, FJAC, SFJAC)
      CALL GETERR (IPVT, SFJAC, EPARMS, INPARM, NPTS, 15, FNORM, WORK,
     *   TOL)
C                                       real rms
      RMS = 0.0D0
      J = 0
      DO 10 I = 1,NPTS
         IF (DATA(I).NE.FBLANK) THEN
            RMS = RMS + FVEC(I) * FVEC(I)
            J = J + 1
            END IF
 10      CONTINUE
      RMS = SQRT (RMS / J)
C                                       Move answers into the buffer.
      CALL FILL (256, 0, IBLK)
      DO 20 I = 1,NPARM
         RBLK(I) = PARMS(I)
         RBLK(I+12) = EPARMS(I)
 20      CONTINUE
      IF (JJC.GT.0) THEN
         RBLK(83) = PARMS(NPARM+1)
         RBLK(84) = PARMS(NPARM+2)
         RBLK(85) = PARMS(NPARM+3)
         RBLK(86) = EPARMS(NPARM+1)
         RBLK(87) = EPARMS(NPARM+2)
         RBLK(88) = EPARMS(NPARM+3)
         END IF
C                                       Move user parms into buffer
      IS = 25
      IBLK(IS) = IDROP(1)
      IBLK(IS+1) = IDROP(2)
      IBLK(IS+2) = NGAUSS
      CALL ZDATE (IBLK(IS+3))
      CALL ZTIME (IBLK(IS+6))
      CALL COPY (4, IDOPOS, IBLK(IS+9))
      CALL COPY (4, IDOMAX, IBLK(IS+13))
      CALL COPY (4, IDOWTH, IBLK(IS+17))
      RBLK(IS+21) = RMS
      WRITE (MSGTXT,1010) RMS
      CALL MSGWRT (5)
C                                       Save in next avail block.
C                                       (first block + no of slices)
      IRRN = ISLHDR(59) + ISLHDR(58)
C                                       Expand file, update header.
      IF (IRRN.GT.ISLHDR(3)) THEN
         NEXP = 1
         CALL ZPHFIL ('SL', IVOL, ISLOT, IVER, PNAME, IERR)
         CALL ZEXPND (ISLUN, ISLHDR(29), PNAME, NEXP, IERR)
         REASON = 'EXPANDING SL FILE'
         IF (IERR.NE.0) GO TO 980
         CALL CHR2H (48, PNAME, 1, ISLHDR(17))
         ISLHDR(3) = ISLHDR(3) + NEXP
         END IF
C                                       Update slice specific header
C                                       info.
      ISLHDR(58) = ISLHDR(58) + 1
      CALL ZFIO ('WRIT', ISLUN, ISIND, 1, ISLHDR, IERR)
      IF (IERR.NE.0) GO TO 980
      WRITE (MSGTXT,1030) ISLHDR(58)
      CALL MSGWRT (5)
C                                       Convert answers to more
C                                       meaningful units.
      SCALEF = 39999.0D0 / (RANGE(2) - RANGE(1))
      OFFSET = 1.0D0 - SCALEF * RANGE(1)
      J = 1
      CALL RFILL (4, 0.0, EMAX)
      CALL RFILL (4, 0.0, EPOS)
      CALL RFILL (4, 0.0, EWTH)
      REASON = ' '
C                                       spectral (ISPEC, BLSUM)
      IF (SPECTR) THEN
         DO 30 I = 1,NGAUSS
            XPIX = PARMS(J+1)
            YPIX = SCALEF * PARMS(J)  +  OFFSET
            XVAL = RPVAL(1,LOCNUM) + (XPIX - RPLOC(1,LOCNUM)) *
     *         AXINC(1,LOCNUM)
            YVAL = RPVAL(2,LOCNUM) + (YPIX - RPLOC(2,LOCNUM)) *
     *         AXINC(2,LOCNUM)
            XPOS(I) = XVAL
            XMAX(I) = YVAL
            IF (PARMS(J).NE.0.0) EMAX(I) = EPARMS(J) * (YVAL / PARMS(J))
            TEMP = XPIX
C                                       Halfwidth.
            XPIX = PARMS(J+1) - PARMS(J+2) / 2.0
            XVAL = RPVAL(1,LOCNUM) + (XPIX - RPLOC(1,LOCNUM)) *
     *         AXINC(1,LOCNUM)
            XWTH(I) = 2.0 * ABS (XPOS(I) - XVAL)
            IF (TEMP-XPIX.NE.0.0) THEN
               EPOS(I) = EPARMS(J+1) * ABS ((XPOS(I)-XVAL)/(TEMP-XPIX))
               EWTH(I) = EPARMS(J+2) * ABS ((XPOS(I)-XVAL)/(TEMP-XPIX))
               END IF
            J = J + 3
 30         CONTINUE
C                                       XY slice (SLICE)
      ELSE
         DO 40 I = 1,NGAUSS
            XP = PARMS(J+1)
            YP = SCALEF * PARMS(J)  +  OFFSET
            CALL XYVAL (XP, YP, XVAL, YVAL, ZVAL, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1031) IERR, I
               CALL MSGWRT (6)
               GO TO 980
               END IF
            XPOS(I) = XVAL
            XMAX(I) = YVAL
            IF (PARMS(J).NE.0.0) EMAX(I) = EPARMS(J) * (YVAL / PARMS(J))
            TEMP = PARMS(J+1)
C                                       Halfwidth.
            XPIX = PARMS(J+1) - PARMS(J+2) / 2.0
            XP = XPIX
            CALL XYVAL (XP, YP, XVAL, YVAL, ZVAL, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1033) IERR, I
               CALL MSGWRT (6)
               GO TO 980
               END IF
            XWTH(I) = 2.0 * ABS (XPOS(I) - XVAL)
            IF (TEMP-XPIX.NE.0.0) THEN
               EPOS(I) = EPARMS(J+1) * ABS ((XPOS(I)-XVAL)/(TEMP-XPIX))
               EWTH(I) = EPARMS(J+2) * ABS ((XPOS(I)-XVAL)/(TEMP-XPIX))
               END IF
            J = J + 3
 40         CONTINUE
         END IF
C                                       store scaled values
      J = 50
      DO 45 I = 1,NGAUSS
         RBLK(J+1) = XMAX(I)
         RBLK(J+2) = EMAX(I)
         RBLK(J+3) = XPOS(I)
         RBLK(J+4) = EPOS(I)
         RBLK(J+5) = XWTH(I)
         RBLK(J+6) = EWTH(I)
         J = J + 6
 45      CONTINUE
      PNAME = CPREF(2,LOCNUM) // CTYP(2,LOCNUM)
      CALL CHR2H (16, PNAME, 1, RBLK(75))
      PNAME = CPREF(1,LOCNUM) // CTYP(1,LOCNUM)
      CALL CHR2H (16, PNAME, 1, RBLK(79))
C                                       Write model record.
      CALL ZFIO ('WRIT', ISLUN, ISIND, IRRN, RBLK, IERR)
      REASON = 'WRITING SL FILE'
      IF (IERR.NE.0) GO TO 980
C                                       print results.
      WRITE (MSGTXT,1040) INFO, ITTER
      CALL MSGWRT (3)
      MSGTXT = 'Results in SLice units'
      CALL MSGWRT (5)
      MSGTXT = 'Cmp  Parameter    Value       Error'
      CALL MSGWRT (5)
      J = 1
      DO 50 I = 1,NGAUSS
         WRITE (MSGTXT,1110) I, 'Peak    ', PARMS(J), EPARMS(J), MUNITS
         CALL MSGWRT (5)
         WRITE (MSGTXT,1110) I, 'Position', PARMS(J+1), EPARMS(J+1),
     *      'SL points'
         CALL MSGWRT (5)
         WRITE (MSGTXT,1110) I, 'Fullwhm ', PARMS(J+2), EPARMS(J+2),
     *      'SL points'
         CALL MSGWRT (5)
         J = J + 3
 50      CONTINUE
      IF (JJC.GE.1) THEN
         WRITE (MSGTXT,1111) 'Constant', PARMS(J), EPARMS(J), MUNITS
         CALL MSGWRT (5)
         J = J + 1
         END IF
      IF (JJC.GE.2) THEN
         WRITE (MSGTXT,1111) 'Slope   ', PARMS(J), EPARMS(J),
     *      '/channel'
         CALL MSGWRT (5)
         J = J + 1
         END IF
      IF (JJC.GE.3) THEN
         WRITE (MSGTXT,1111) 'Curvature', PARMS(J), EPARMS(J),
     *      '/channel^2'
         CALL MSGWRT (5)
         J = J + 1
         END IF
      MSGTXT = 'Results in physical units'
      CALL MSGWRT (5)
      MSGTXT = 'Cmp  Parameter    Value         Error '
      CALL MSGWRT (5)
      J = 1
      DO 60 I = 1,NGAUSS
         PNAME = CPREF(2,LOCNUM) // CTYP(2,LOCNUM)
         CALL CHTRIM (PNAME, 25, PNAME, IS)
         WRITE (MSGTXT,1110) I, 'Peak    ', XMAX(I), EMAX(I), PNAME(:IS)
         CALL MSGWRT (5)
         PNAME = CPREF(1,LOCNUM) // CTYP(1,LOCNUM)
         CALL CHTRIM (PNAME, 25, PNAME, IS)
         WRITE (MSGTXT,1110) I, 'Position', XPOS(I), EPOS(I), PNAME(:IS)
         CALL MSGWRT (5)
         WRITE (MSGTXT,1110) I, 'Fullwhm ', XWTH(I), EWTH(I), PNAME(:IS)
         CALL MSGWRT (5)
         J = J + 3
 60      CONTINUE
      IRETCD = 0
      GO TO 990
C                                       Fatal error.
 980  WRITE (MSGTXT,1980) REASON
      CALL MSGWRT (6)
C                                       Close down procedure.
 990  CALL ZCLOSE (ISLUN, ISIND, IERR)
C                                       return answers
      IF (.NOT.QUICK) THEN
         DO 995 I = 1,NGAUSS
            GM(I) = XMAX(I)
            GM(24+I) = EMAX(I)
            GM(3+2*I) = XPOS(I)
            GM(27+2*I) = EPOS(I)
            GM(10+3*I) = XWTH(I)
            GM(34+3*I) = EWTH(I)
 995        CONTINUE
         CALL PTPARM (48, GM, IBLK, IERR)
         END IF
      CALL DIETSK (IRETCD, QUICK, IBLK)
      STOP
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Solution RMS =',1PE12.4)
 1030 FORMAT ('Successful slice model version',I4,' saved.')
 1031 FORMAT ('ERROR',I3,' FINDING COORDINATES OF COMPONENT',I3,' PEAK')
 1033 FORMAT ('ERROR',I3,' FINDING COORDINATES OF COMPONENT',I3,
     *   ' HALF POINT')
 1040 FORMAT ('Info, function evaluations =',2I8)
 1110 FORMAT (I3,2X,A8,2X,1PE14.6,1PE12.4,2X,A)
 1111 FORMAT (' BL',2X,A8,2X,1PE14.6,1PE12.4,2X,A)
 1980 FORMAT ('FATAL ERROR',1X,A)
      END
      SUBROUTINE GETERR (IPVT, FJAC, PARERR, MPARMS, NDATA, MDATA,
     *   FNORM, WA, TOL)
C-----------------------------------------------------------------------
C   This subroutine for GAUSS calculates the errors on the fitted
C   parameters.
C   Inputs:
C      IPVT    I(MPARMS)   Defines a permutation matrix P such that
C                       JAC*P = Q*R, where JAC is the final calculated
C                       Jacobian, Q is
C                   orthogonal (not stored), and R is upper triangular
C                   with diagonal elements of nonincreasing magnitude.
C                   column J of P is column IPVT(J) of the identity
C                   matrix. (See FJAC below).
C           FJAC    D(MDATA,MPARMS)   The upper MPARMS by MPARMS sub-
C                   matrix of FJAC contains an upper triangular matrix
C                   R with diagonal elements of nonincreasing magnitude
C                   such that
C
C                    T     T           T
C                   P *(JAC *JAC)*P = R *R,
C
C                   where P is a permutation matrix and JAC is the final
C                   calculated Jacobian. Column J of P is column IPVT(J)
C                   (see above) of the identity matrix.
C           MPARMS  I Number of parameters in fitted function (adj.
C                   array dim.).
C           NDATA   I   Number of data points fitted.
C           MDATA   I Maximum no. of data points allowed for in
C                   FJAC (adj. array dim.).
C           FNORM   D   Euclidian norm of solution vector.
C           WA      D(MPARMS)   work array.
C  OUTPUT:  PARERR  D(MPARMS)   error in fitted parameters.
C           TOL     D   tolerance used in call to LMDER1.
C-----------------------------------------------------------------------
      INTEGER   MDATA, MPARMS, NDATA, IPVT(MPARMS)
      DOUBLE PRECISION FJAC(MDATA,MPARMS), PARERR(MPARMS), FNORM,
     *   WA(MPARMS), TOL
C
      INTEGER   IPARMS, J, NPARMS
      DOUBLE PRECISION EPSILN
C-----------------------------------------------------------------------
C                                       Calculate error following
C                                       Argonne write up
      NPARMS = MPARMS
C                                       Is this right ??????
      EPSILN = FNORM / SQRT(REAL(NDATA-NPARMS))
      IPARMS = NPARMS
      CALL COVAR (IPARMS, FJAC, MDATA, IPVT, TOL, WA)
      DO 10 J = 1,NPARMS
         PARERR(J) = EPSILN * SQRT(FJAC(J,J))
 10      CONTINUE
C
      RETURN
      END
