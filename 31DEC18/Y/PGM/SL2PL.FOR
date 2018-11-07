      PROGRAM SL2PL
C-----------------------------------------------------------------------
C! Task to create a plot (PL) file from a slice (SL) file
C# Plot-util EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2002, 2005-2007, 2009, 2011-2012
C;  Copyright (C) 2014-2016
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
C   SL2PL will find an SLice extension file and create a plot file
C   representation.
C   Inputs:   (from AIPS)
C      DOSLICE   R      > 0 means plot slice.
C      DOMODEL   R      > 0 means plot model.
C      DORESID   R      > 0 means plot residuals.
C      USERID    R      user number ignored
C      INNAME    H(3)   name of primary file.
C      INCLASS   H(2)   class of primary file.
C      INSEQ     R      sequence number of primary file.
C      INDISK    R      disk volume number. 0 means try all.
C      INVERS    R      version number of slice file, 0 means try
C                       latest version.
C      IN2VERS   R      version number of gaussian model if
C                       DOSLICE or DOMODEL is true.
C      LTYPE     R      1=no labeling 2 no ticks, 3 RA/dec
C                       4=center rel., 5=subslice center-rel
C      PIXRANGE  R(2)   The minimum and maximum values allowed for the
C                       plot.  All other values will be clipped
C                       If IRANGE(1).GE.IRANGE(2) then the slice max and
C                       min will be used.
C      BDROP     R    # Slice points not plotted on left
C      EDROP     R    # Slice points not plotted on right
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C      TVCORN   R(4)   TV pixel to use (both > 0 => pixel scale)
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, NAMIN*12, CLSIN*6, TYPIN*2, PFILE*48, OPCODE*4
      HOLLERITH XNAMIN(3), XCLSIN(2)
      REAL      BLC(7), TRC(7), IRANGE(2), BDROP, DSKIN, VERSIN, LABEL,
     *   SEQIN, PRUSER, EDROP, DOSLIC, DOMODL, DORES, IN2VER, SBLK(256),
     *   RDUMMY(25), XDOTV, XGRCH
      INTEGER   IDROP(2), IPBLK(256), ISBLK(256), IROUND, ISLHDR(256),
     *   IERR, ISFIND, ILABEL, IPLUN, ISLUN, IMFIND, IRETCD, ISEQ,
     *   IMLUN, INPRMS, ISLOT, IUSER, IVER, IVOL, IMOD, IPFIND, IERR1,
     *   IERR2, IERR3, IERR4, GRCHN, TVCHN, TVCORN(4), LTYPE
      LOGICAL   T, F, CATUP, NOSAVE, QUICK, SAVE, NOEXCL, WAIT, NOMAP,
     *   IDOMOD, DOTV
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INPARM/ DOSLIC, DOMODL, DORES, PRUSER, XNAMIN, XCLSIN,
     *   SEQIN, DSKIN, VERSIN, IN2VER, LABEL, IRANGE, BDROP, EDROP,
     *   XDOTV, XGRCH
      COMMON /CHPARM/ NAMIN, CLSIN
      COMMON /SLHEAD/ RDUMMY
      COMMON /WORK/ SBLK
      EQUIVALENCE (T, SAVE, WAIT),      (F, NOEXCL, NOMAP, NOSAVE)
      EQUIVALENCE (ISBLK, SBLK)
      DATA IMLUN, ISLUN, IPLUN /16, 27, 26/
      DATA PRGNAM /'SL2PL '/
      DATA TYPIN /'  '/
      DATA F, T /.FALSE., .TRUE./
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE., ISBLK)
      CALL VHDRIN
C                                       Get input values from AIPS.
      INPRMS = 20
      IRETCD = 0
      CALL GTPARM (PRGNAM, INPRMS, QUICK, DOSLIC, ISBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRETCD = 1
         END IF
      IF (QUICK) CALL RELPOP (IRETCD, ISBLK, IERR)
      IF (IRETCD.NE.0) GO TO 995
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
C
      ISEQ = IROUND (SEQIN)
      IVOL = IROUND (DSKIN)
      IVER = IROUND (VERSIN)
      IMOD = IROUND (IN2VER)
      PRUSER = NLUSER
      IUSER = NLUSER
      ILABEL = IROUND (LABEL)
      LTYPE = MOD (ABS(ILABEL), 100)
      IF ((LTYPE.EQ.0) .OR. (LTYPE.GT.10)) THEN
         LTYPE = 3
         IF (ILABEL.GE.0) THEN
            ILABEL = (ILABEL/100)*100 + LTYPE
         ELSE
            ILABEL = (ILABEL/100)*100 - LTYPE
            END IF
         END IF
      LABEL = ILABEL
      IDROP(1) = BDROP
      IDROP(1) = MAX (0, IDROP(1))
      IDROP(2) = EDROP
      IDROP(2) = MAX (0, IDROP(2))
      CATUP = SAVE
      IDOMOD = (DOMODL.GT.0.) .OR. (DORES.GT.0.)
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
C                                       Open map file & get header.
      OPCODE = 'HDWR'
      IF (DOTV) OPCODE = 'READ'
      CALL MAPOPN (OPCODE, IVOL, NAMIN, CLSIN, ISEQ, TYPIN, IUSER,
     *   IMLUN, IMFIND, ISLOT, CATBLK, ISBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         GO TO 995
         END IF
C                                       Open SL extension file.
      CALL OPEXT ('SL', IVOL, ISLOT, IVER, ISLUN, NOEXCL, WAIT,
     *   ISFIND, IERR)
      IF (IERR.NE.0) GO TO 975
      VERSIN = IVER
C                                       Add plot file to header.
      IVER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', IVOL, ISLOT, CATBLK, ISBLK, SAVE, 'READ',
     *      IVER, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Build file name.
      CALL ZPHFIL ('PL', IVOL, ISLOT, IVER, PFILE, IERR)
C                                       Initialize LOCATI common, do
C                                       labeling.
      CALL SPINIT (ISLUN, ISFIND, IPBLK, IDROP, ILABEL, IDOMOD, IMOD,
     *   IRANGE, IVER, BLC, TRC, ISLHDR, IVOL, ISLOT, PFILE, IPLUN,
     *   IPFIND, DOTV, TVCHN, GRCHN, TVCORN, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       Make sure a model exists before
C                                       trying to plot it:
      IF ((DOMODL.GT.0.0) .OR. (DORES.GT.0.0)) THEN
         IF (ISLHDR(58).EQ.0) THEN
            MSGTXT = '** WARNING ** NO MODELS EXIST IN THIS FILE'
            CALL MSGWRT (6)
            MSGTXT = 'Ignoring plot model and/or plot residual requests'
            CALL MSGWRT (6)
            DOMODL = -1.0
            DORES  = -1.0
            END IF
         END IF
C                                       Write selected plot if needed.
      IERR1 = 0
      IERR2 = 0
      IERR3 = 0
      IF (DOSLIC.GT.0.0) THEN
         CALL SPSLPL (ISLUN, ISFIND, IPBLK, BLC, TRC, IERR1)
         IF (IERR1.EQ.2) GO TO 960
         END IF
      IF (DOMODL.GT.0.0) THEN
         CALL SPGMPL (DOMODL, ISLHDR, IMOD, ISLUN, ISFIND, IPBLK, BLC,
     *      TRC, IERR2)
         IF (IERR2.EQ.2) GO TO 960
         END IF
      IF (DORES.GT.0.0) THEN
         CALL SPRSPL (ISLHDR, IMOD, ISLUN, ISFIND, IPBLK, BLC, TRC,
     *      IERR3)
         IF (IERR3.EQ.2) GO TO 960
         END IF
C                                       Write end code & final buffer.
      CALL GFINIS (IPBLK, IERR4)
      IF (((IERR1.EQ.1) .OR. (IERR2.EQ.1) .OR. (IERR3.EQ.1)) .AND.
     *   (IERR4.EQ.0)) WRITE (MSGTXT,1030) IVER
      IF (IERR4.NE.0) WRITE (MSGTXT,1032) IVER
      IF ((IERR1+IERR2+IERR3.EQ.0) .AND. (IERR4.EQ.0))
     *   WRITE (MSGTXT,1034) IVER
      CALL MSGWRT (2)
      CALL HIPLOT (IVOL, ISLOT, IVER, IPBLK, IERR)
      GO TO 980
C-----------------------------------------------------------------------
C                                       Slice writing error.
C                                       Fatal error. Destroy.
 960  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (IPLUN, IPFIND, IERR)
         CALL ZDESTR (IVOL, PFILE, IERR)
         END IF
      IF (.NOT.DOTV) CALL DELEXT ('PL', IVOL, ISLOT, 'READ', CATBLK,
     *   ISBLK, IVER, IERR)
 970  CALL ZCLOSE (IMLUN, IMFIND, IERR)
      CALL ZCLOSE (ISLUN, ISFIND, IERR)
      IRETCD = 8
      GO TO 995
C                                       before plot
 975  IRETCD = 16
C                                       Close map file.
 980  CALL MAPCLS ('READ', IVOL, ISLOT, IMLUN, IMFIND, CATBLK, NOSAVE,
     *   ISBLK, IERR)
C
 995  CALL DIETSK (IRETCD, QUICK, ISBLK)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. GTPARM ERR =',I5)
 1020 FORMAT ('ERROR OPENING MAP FILE. MAPOPN ERR =',I6)
 1030 FORMAT ('Partial plot finished. Version',I5)
 1032 FORMAT ('Partial plot. No finish record. Version',I5)
 1034 FORMAT ('Successful plot file version',I5,'  created.')
      END
      SUBROUTINE SPINIT (ISLUN, ISFIND, IPBLK, IDROP, LABEL, IDOMOD,
     *   IMOD, RANGE, IVER, BLC, TRC, ISLHDR, IVOL, ISLOT, PFILE, IPLUN,
     *   IPFIND, DOTV, TVCHN, GRCHN, TVCORN, IERR)
C-----------------------------------------------------------------------
C   Given an open slice file and an open graph file SPINIT will write
C   plot file commands labeling the slice into the plot file.
C   Inputs:
C      ISLUN   I        logical unit no. of open slice file.
C      ISFIND  I        FTAB index for slice file.
C      IPBLK   I(256)   I/O buffer for slice file.
C      IDROP   I(2)     # points to drop left, right
C      LABEL   I        type of labeling: 1=none, 2=no tick, 3=ra/dec,
C                       4=center-rel, 5=sub-slice center-rel
C      IDOMOD  L        T => do model
C      IMOD    I        If IDOMOD, the model version #
C      RANGE   R(2)     the minimum and maximum values allowed for
C                       the plot.  If RANGE(1) .GE. RANGE(2) then the
C                       MAP min and max will be used.
C      IVER    I        Plot file version number
C   Output:
C      BLC     R(7)     Plot BLC in funny units
C      TRC     R(7)     Plot TRC in ditto
C      ISLHDR  I(256)   Slice file record 1
C      IERR    I        error code 0= none.
C                          1 = bad input parms.
C                          2 = fatal error during graphing.
C-----------------------------------------------------------------------
      INTEGER   ISLUN, ISFIND, IPBLK(256), IDROP(2), LABEL, IMOD, IVER,
     *   ISLHDR(256), IVOL, ISLOT, IPLUN, IPFIND, TVCHN, GRCHN,
     *   TVCORN(4), IERR
      LOGICAL   IDOMOD, DOTV
      REAL      RANGE(2), BLC(7), TRC(7)
      CHARACTER PFILE*48
C
      CHARACTER TEXT(2)*132, NAMIN*12, CLSIN*6, OPTYPE*4, JY*8
      HOLLERITH XNAMIN(3), XCLSIN(2), XOPTYP
      REAL      RMAX, PIXMAX, PIXMIN, XFAC, XOFF, RANGE2(2), CH(4), DX,
     *   SEQIN, DSKIN, PRUSER, XBLC(7), XTRC(7), YGAP, XYRATO, YMAX,
     *   YMIN, RIMAGE(20), RINP(20), FQFINC, RSBLK(256)
      DOUBLE PRECISION FQFREQ, DSBLK(128)
      INTEGER   INOSL, NTEXT, IDEPTH(5), IMAGE(20), ISBLK(256),
     *   NRPBLK, IROUND, INPRMS, IPSIZE, IPTYPE
      LOGICAL   F, ISOLD
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      EQUIVALENCE (ISBLK, RSBLK, DSBLK)
      COMMON /SLHEAD/ PRUSER, XNAMIN, XCLSIN, SEQIN, DSKIN, XBLC, XTRC,
     *   XOPTYP, YMIN, YMAX
      COMMON /CHPARM/ NAMIN, CLSIN
      COMMON /WORK/ ISBLK
      COMMON /INPARM/ RINP
      EQUIVALENCE (IMAGE, RIMAGE)
      DATA F /.FALSE./
      DATA JY /'JY'/
C-----------------------------------------------------------------------
C                                       Calculate no. of floating pt.
C                                       values in a 512 byte block.
      NRPBLK = 256
C                                       Read first slice file record
      CALL ZFIO ('READ', ISLUN, ISFIND, 1, ISLHDR, IERR)
      IF (IERR.NE.0) GO TO 950
C                                       Test IMOD, set defaults.
      IF (IDOMOD) THEN
         IF (IMOD.LE.0) IMOD = ISLHDR(58)
         IF (IMOD.GT.ISLHDR(58)) THEN
            WRITE (MSGTXT,1000) IMOD, ISLHDR(58)
            CALL MSGWRT (5)
            IMOD = ISLHDR(58)
            END IF
         END IF
C
      INOSL = ISLHDR(57)
C                                       Read 2nd record to get
C                                       origional slice inputs.
      CALL ZFIO ('READ', ISLUN, ISFIND, 2, ISBLK, IERR)
      IF (IERR.NE.0) GO TO 950
      CALL COPY (25, ISBLK(11), PRUSER)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      IF ((OPTYPE.EQ.'AVER') .OR. (OPTYPE.EQ.'FLUX') .OR.
     *   (OPTYPE.EQ.'LGFL') .OR. (OPTYPE.EQ.'LNFL') .OR.
     *   (OPTYPE.EQ.'ADER') .OR. (OPTYPE.EQ.'FDER') .OR.
     *   (OPTYPE.EQ.'LGAV') .OR. (OPTYPE.EQ.'LNAV')) THEN
         ISOLD = .FALSE.
      ELSE
         ISOLD = .TRUE.
         END IF
      IF (OPTYPE.EQ.'FLUX') CALL CHR2H (8, JY, 1, CATH(KHBUN))
      IF (OPTYPE.EQ.'LGFL') CALL CHR2H (8, 'LOG10 JY', 1, CATH(KHBUN))
      IF (OPTYPE.EQ.'LNFL') CALL CHR2H (8, 'LN (JY)', 1, CATH(KHBUN))
      IF (OPTYPE.EQ.'FDER') CALL CHR2H (8, 'dJY / dx', 1, CATH(KHBUN))
      IF (ISOLD) THEN
         FQFINC = 0.0
         FQFREQ = 0.0D0
      ELSE
         FQFINC = RSBLK(36)
         FQFREQ = DSBLK(19)
         END IF
C                                       Range in pixel values.
      IF (RANGE(2).LE.RANGE(1)) THEN
         RANGE2(2) = YMAX
         RANGE2(1) = YMIN
      ELSE
         RANGE2(2) = RANGE(2)
         RANGE2(1) = RANGE(1)
         END IF
      IF (RANGE2(2)-RANGE2(1).GT.200.*(YMAX-YMIN)) THEN
         IF (RANGE2(1).LT.2*YMIN-YMAX) RANGE2(1) = YMIN
         IF (RANGE2(2).GT.2*YMAX-YMIN) RANGE2(2) = YMAX
         END IF
      IF (RANGE2(1).EQ.RANGE2(2)) THEN
         RANGE2(1) = RANGE2(1) - 0.001
         RANGE2(2) = RANGE2(2) + 0.001
         END IF
C                                       Calc fac & offset to keep BLC
C                                       TRC within range to prevent
C                                       overflow in graphics routines.
      DX = (RANGE2(2) - RANGE2(1)) * 0.03
      RANGE2(2) = RANGE2(2) + DX
      RANGE2(1) = RANGE2(1) - DX
      PIXMAX = RANGE2(2)
      PIXMIN = RANGE2(1)
      XFAC = 39999.0 / (PIXMAX - PIXMIN)
      XOFF = 40000.0 - XFAC * PIXMAX
      RANGE2(1) = XFAC * RANGE2(1) + XOFF
      RANGE2(2) = XFAC * RANGE2(2) + XOFF
      RMAX = 2.0 ** (NBITWD-1)  -  1
C                                      Have to truncate max.
      IF (RANGE2(2).GT.RMAX) THEN
         MSGTXT = '** WARNING ** TRUNCATING MAX Y VALUE INPUT BY USER'
         CALL MSGWRT (6)
         RANGE2(2) = RMAX
         END IF
C                                       Must increase users min.
      IF (RANGE2(1).LT.-RMAX) THEN
         MSGTXT = '** WARNING ** INCREASING Y MIN INPUT BY USER.'
         CALL MSGWRT (6)
         RANGE2(1) = -RMAX
         END IF
C                                       Round and back calc range.
      RANGE2(1) = IROUND (RANGE2(1))
      RANGE2(2) = IROUND (RANGE2(2))
      RANGE(1) = (RANGE2(1) - XOFF) / XFAC
      RANGE(2) = (RANGE2(2) - XOFF) / XFAC
C                                       Save original image pixels.
      RIMAGE(4) = XBLC(1)
      RIMAGE(5) = XBLC(2)
      RIMAGE(6) = XTRC(1)
      RIMAGE(7) = XTRC(2)
      RIMAGE(8) = XBLC(3)
      RIMAGE(9) = XTRC(3)
C                                       Initialize parms.
      BLC(2) = RANGE2(1)
      TRC(2) = RANGE2(2)
      LOCNUM = 1
      CALL RFILL (4, 0.0, CH)
      YGAP = 0.0
      CALL SLBINI (IDROP, INOSL, RANGE, BLC, TRC, XBLC, XTRC, FQFREQ,
     *   FQFINC, IDEPTH, LABEL, YGAP, CH, TEXT, NTEXT)
      CATR(IRRAN) = RANGE(1)
      CATR(IRRAN+1) = RANGE(2)
C                                       Init plot file incl defaults
      IPSIZE = 0
      IPTYPE = 5
      INPRMS = 18
      RINP(15) = RANGE(1)
      RINP(16) = RANGE(2)
      CALL GINIT (IVOL, ISLOT, PFILE, IPSIZE, IPTYPE, INPRMS, RINP,
     *   DOTV, TVCHN, GRCHN, TVCORN, CATBLK, IPBLK, IPLUN, IPFIND, IERR)
      IF (IERR.NE.0) GO TO 950
C                                       Initialize plot file line drw.
      XYRATO = (TRC(2) - BLC(2)) / (TRC(1) - BLC(1))
      CALL GINITL (BLC, TRC, XYRATO, CH, IDEPTH, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 950
      CALL GLTYPE (1, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 950
C                                       put init corners in img cat
      IMAGE(1) = ABS (LABEL)
      IMAGE(2) = IDROP(1)
      IMAGE(3) = IDROP(2)
      CALL GMCAT (9, IMAGE, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 950
C                                       Draw axis and labels
      CALL SLABEL (LABEL, IVER, YGAP, TEXT, NTEXT, BLC, TRC, IPBLK,
     *   IERR)
      IF (IERR.NE.0) GO TO 930
      CALL CLAB1 (BLC, TRC, CH, LABEL, XYRATO, F, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 930
      GO TO 999
C                                       Error. Partial file written.
 930  IERR = 1
      MSGTXT = 'ERROR DURING GRAPHING. PARTIAL PLOT FILE CREATED'
      CALL MSGWRT (6)
      GO TO 999
C                                       Fatal error during init.
 950  IERR = 2
      MSGTXT = 'FATAL ERROR. DESTROYING BAD PLOT FILE'
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('** WARNING ** Model vers being reduced',I5,' to',I5)
      END
      SUBROUTINE SPSLPL (ISLUN, ISFIND, IPBLK, BLC, TRC, IERR)
C-----------------------------------------------------------------------
C   SPSLPL plots slice data into the plot file
C   Inputs:
C      ISLUN   I        logical unit no. of open slice file.
C      ISFIND  I        FTAB index for slice file.
C      IPBLK   I(256)   I/O buffer for slice file.
C      BLC     R(7)     Bottom left hand corner in plot units.
C      TRC     R(7)     Top right hand corner in plot units.
C  Output:
C      IERR    I        error code. 0=ok, 1=plotting error, 2=fatal err
C-----------------------------------------------------------------------
      INTEGER   ISLUN, ISFIND, IPBLK(256), IERR
      REAL      BLC(7), TRC(7)
C
      REAL      SBLK(256), Y, SCALEF, OFFSET, X
      INTEGER   NRPBLK, ISTART, IEND, IRRN, IPOS, ISL
      LOGICAL   BLAST, BNEXT
      CHARACTER TXTMSG*80
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /WORK/ SBLK
C-----------------------------------------------------------------------
C                                       Draw plot.
      NRPBLK = 256
      ISTART = BLC(1) + 0.5
      IEND = TRC(1) + 0.5
      IRRN = 3 + (ISTART-1)/NRPBLK
      IPOS = ISTART - ((IRRN-3) * NRPBLK)
C                                       Scale factors to scale actual
C                                       max:min values to 4000:1.
      SCALEF = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      OFFSET = 40000.0 - SCALEF * CATR(IRRAN+1)
C                                       Read first data.
      CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, SBLK, IERR)
      IF (IERR.NE.0) GO TO 930
C                                       Draw first data pt.
      TXTMSG = 'Draw slice plot'
      CALL GCOMNT (2, TXTMSG, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 950
      CALL GLTYPE (2, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 950
      BLAST = .TRUE.
C                                       Do rest of data points.
      DO 100 ISL = ISTART,IEND
C                                       Read from slice file.
         IF (IPOS.GT.NRPBLK) THEN
            IRRN = IRRN + 1
            CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, SBLK, IERR)
            IF (IERR.NE.0) GO TO 930
            IPOS = 1
            END IF
C                                       Draw to next slice pt.
         X = ISL
         Y = SCALEF * SBLK(IPOS)  +  OFFSET
         BNEXT = SBLK(IPOS).EQ.FBLANK
         Y = MIN (Y, TRC(2))
         Y = MAX (Y, BLC(2))
         IPOS = IPOS + 1
         IF ((BNEXT) .OR. (BLAST)) THEN
            CALL GPOS (X, Y, IPBLK, IERR)
            BLAST = BNEXT
         ELSE
            CALL GVEC (X, Y, IPBLK, IERR)
            END IF
         IF (IERR.NE.0) GO TO 930
 100     CONTINUE
C                                       Succesful finish.
      GO TO 999
C                                       Error. Partial file written.
 930  IERR = 1
      WRITE (MSGTXT,1930)
      CALL MSGWRT (6)
      GO TO 999
C                                       Fatal error during init.
 950  IERR = 2
      WRITE (MSGTXT,1950)
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1930 FORMAT ('ERROR DURING GRAPHING. PARTIAL PLOT FILE CREATED.')
 1950 FORMAT ('FATAL ERROR. DESTROYING BAD PLOT FILE.')
      END
      SUBROUTINE SLABEL (LABEL, IVER, YGAP, TEXT, NTEXT, BLC, TRC,
     *   IPBLK, IERR)
C-----------------------------------------------------------------------
C   SLABEL does peculiar labeling for slices to plot files: border
C   lines, top label, bottom, lines
C   Inputs:
C      LABEL   I         Type of label
C      IVER    I         Plot file version number
C      YGAP    R         Place to start putting TEXT below plot
C      TEXT    C(2)*132  Text for bottom of plot
C      NTEXT   I         # lines to be used in TEXT
C      BLC     R(2)      BLC of plot file
C      TRC     R(2)      TRC of plot file
C   In/out:
C      IPBLK   I(256)    Plot work buffer
C   Output:
C      IERR    I         Error if not zero
C-----------------------------------------------------------------------
      INTEGER   LABEL, IVER, NTEXT, IPBLK(256), IERR
      REAL      YGAP, BLC(2), TRC(2)
      CHARACTER TEXT(2)*132
C
      CHARACTER TIME*8, CTEMP*18, DATE*12, SPTEXT*80
      INTEGER   IANGL, INCHAR, IT(3), ID(3), I, NCHAR, LTYPE
      REAL      DCX, DCY
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Draw border.
      CALL GPOS (BLC(1), BLC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (TRC(1), BLC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (TRC(1), TRC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (BLC(1), TRC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (BLC(1), BLC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      LTYPE = MOD (ABS(LABEL), 100)
      IF (LTYPE.EQ.1) GO TO 999
C                                       Source name, stokes, freq.
      IF (LTYPE.LT.7) THEN
         CALL GPOS (BLC(1), TRC(2), IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         DCX = 0.0
         DCY = 0.5
         IANGL = 0
         CALL H2CHR (8, 1, CATH(KHOBJ), SPTEXT)
         INCHAR = 12
         IF (SPTEXT.EQ.' ') INCHAR = 1
         IF (NCHLAB(1,LOCNUM).GT.0) THEN
            IF (INCHAR.GT.1) SPTEXT(INCHAR-1:INCHAR-1) = '_'
            SPTEXT(INCHAR:) = SAXLAB(1,LOCNUM)(1:NCHLAB(1,LOCNUM))
            INCHAR = INCHAR + 3 + NCHLAB(1,LOCNUM)
            END IF
         IF (NCHLAB(2,LOCNUM).GT.0) THEN
            IF (INCHAR.GT.1) SPTEXT(INCHAR-1:INCHAR-1) = '_'
            SPTEXT(INCHAR:) = SAXLAB(2,LOCNUM)(1:NCHLAB(2,LOCNUM))
            INCHAR = INCHAR + 3 + NCHLAB(2,LOCNUM)
            END IF
C                                       image name
         IF (INCHAR.GT.1) SPTEXT(INCHAR-1:INCHAR-1) = '_'
         CALL H2CHR (18, 1, CATH(KHIMN), CTEMP)
         CALL NAMEST (CTEMP, CATBLK(KIIMS), SPTEXT(INCHAR:), NCHAR)
         CALL REFRMT (SPTEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPTEXT, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Date/time, version number
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) THEN
         CALL GPOS (BLC(1), TRC(2), IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         DCY = DCY + 1.333
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, TIME, DATE)
         WRITE (SPTEXT,1000) IVER, DATE, TIME
         CALL REFRMT (SPTEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPTEXT, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Text at bottom
      IF ((NTEXT.GT.0) .AND. (LTYPE.LT.7)) THEN
         DCX = 0.
         IANGL = 0
         DCY = -YGAP
         DO 20 I = 1,NTEXT
            CALL GPOS (BLC(1), BLC(2), IPBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL CHTRIM (TEXT(I), 80, TEXT(I), INCHAR)
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, TEXT(I), IPBLK,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            DCY = DCY - 1.333
 20         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Plot file version',I4,'__created ',A,A)
      END
      SUBROUTINE SPGMPL (DOMODL, ISLHDR, IMOD, ISLUN, ISFIND, IPBLK,
     *   BLC, TRC, IERR)
C-----------------------------------------------------------------------
C   Given an open slice file and an open plot file, SPGMPL will write
C   a plot file representation of the slice model.
C   Inputs:
C      DOMODL  R       1,3 model sum, 2,3 comps
C      ISLHDR  I(256)  the slice file header.
C      IMOD    I       the version number of the Gaussian model.
C      ISLUN   I       logical unit no. of open slice file.
C      ISFIND  I       FTAB index for slice file.
C      IPBLK   I       plot file I/O buffer.
C      BLC     R(7)    the bottom left corner of the plot.
C      TRC     R(7)    the top right corner of the plot.
C   Output:
C      IERR    I       error code 0= none.
C                         1 = error during graphing. Try finish.
C                         2 = fatal error. Give up.
C-----------------------------------------------------------------------
      INTEGER   ISLHDR(256), IMOD, ISLUN, ISFIND, IPBLK(256), IERR
      REAL      DOMODL, BLC(7), TRC(7)
C
      REAL      X, Y, SCALEF, OFFSET, SBLK(256), PARM(12), BASLIN(3),
     *   XCEN
      INTEGER   ISTART, IEND, IRRN, IPOS, INP, ISBLK(256), IS, I0, I1,
     *   NGAUSS, I, IC, ISL, IMAX, IWTH, IDOMOD
      CHARACTER TXTMSG*80
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /WORK/ ISBLK
      EQUIVALENCE (ISBLK, SBLK)
C-----------------------------------------------------------------------
      IDOMOD = DOMODL + 0.5
      IF (IDOMOD.LE.0) IDOMOD = 1
C                                       Init some variables.
      SCALEF = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      OFFSET = 40000.0 - SCALEF * CATR(IRRAN+1)
      ISTART = BLC(1) + .5
      IEND = TRC(1) + .5
      XCEN = (ISTART + IEND) / 2.0
C                                       Read data.
      IRRN = ISLHDR(59) + IMOD - 1
      CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, SBLK, IERR)
      IF (IERR.NE.0) GO TO 930
C                                       Move data from buffer.
      IS = 24 + 1
      I0 = ISBLK(IS)
      I1 = ISLHDR(57) - ISBLK(IS+1)
      NGAUSS = ISBLK(IS+2)
      DO 10 I = 1,12
         PARM(I) = SBLK(I)
 10      CONTINUE
      DO 20 I = 1,NGAUSS
         IC = 3 * I - 2
         PARM(IC) = PARM(IC)
 20      CONTINUE
      CALL RCOPY (3, SBLK(83), BASLIN)
C                                       Plot data points.
      IF (MOD(IDOMOD,2).EQ.1) THEN
         TXTMSG = 'Draw slice sum model'
         CALL GCOMNT (2, TXTMSG, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL GLTYPE (3, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 950
         INP = 1
         DO 50 ISL = ISTART,IEND
            X = ISL
C                                       Add up all components
            Y = 0.0
            DO 40 IC = 1,NGAUSS
               IMAX = 3 * IC - 2
               IPOS = IMAX + 1
               IWTH = IPOS + 1
               Y = Y + PARM(IMAX) * EXP (- 2.772 * (PARM(IPOS) - X)
     *            ** 2  /  (PARM(IWTH) * PARM(IWTH)))
 40            CONTINUE
            Y = Y + BASLIN(1) + (X-XCEN)*BASLIN(2) +
     *         (X-XCEN)*(X-XCEN)*BASLIN(3)
C
            Y = SCALEF * Y  +  OFFSET
            Y = MIN (Y, TRC(2))
            Y = MAX (Y, BLC(2))
            IF (INP.EQ.1) THEN
               CALL GPOS (X, Y, IPBLK, IERR)
            ELSE
               CALL GVEC (X, Y, IPBLK, IERR)
               END IF
            IF (IERR.NE.0) GO TO 950
            INP = 2
 50         CONTINUE
         END IF
      IF (IDOMOD/2.GT.0) THEN
         TXTMSG = 'Draw slice components'
         CALL GCOMNT (2, TXTMSG, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL GLTYPE (4, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 950
         INP = 1
         DO 80 IC = 1,NGAUSS
            IMAX = 3 * IC - 2
            IPOS = IMAX + 1
            IWTH = IPOS + 1
            DO 70 ISL = ISTART,IEND
               X = ISL
               Y =  2.772 * (PARM(IPOS) - X) ** 2  /
     *            (PARM(IWTH) * PARM(IWTH))
               IF (Y.GT.12) THEN
                  INP = 1
               ELSE
                  Y = PARM(IMAX) * EXP (-Y)
                  Y = SCALEF * Y  +  OFFSET
                  Y = MIN (Y, TRC(2))
                  Y = MAX (Y, BLC(2))
                  IF (INP.EQ.1) THEN
                     CALL GPOS (X, Y, IPBLK, IERR)
                  ELSE
                     CALL GVEC (X, Y, IPBLK, IERR)
                     END IF
                  IF (IERR.NE.0) GO TO 950
                  INP = 2
                  END IF
 70            CONTINUE
 80         CONTINUE
         IF ((BASLIN(1).NE.0.0) .OR. (BASLIN(2).NE.0.0) .OR.
     *      (BASLIN(3).NE.0.0)) THEN
            INP = 1
            DO 90 ISL = ISTART,IEND
               X = ISL
               Y = BASLIN(1) + (X-XCEN)*BASLIN(2) +
     *            (X-XCEN)*(X-XCEN)*BASLIN(3)
               Y = SCALEF * Y  +  OFFSET
               Y = MIN (Y, TRC(2))
               Y = MAX (Y, BLC(2))
               IF (INP.EQ.1) THEN
                  CALL GPOS (X, Y, IPBLK, IERR)
               ELSE
                  CALL GVEC (X, Y, IPBLK, IERR)
                  END IF
               IF (IERR.NE.0) GO TO 950
               INP = 2
 90            CONTINUE
            END IF
         END IF
      GO TO 999
C                                       Slice read error.
 930  IERR = 1
      MSGTXT = 'SLICE FILE READ ERROR'
      CALL MSGWRT (6)
      GO TO 999
C                                       TEK write error.
 950  IERR = 2
      MSGTXT = 'PLOT FILE WRITE ERROR'
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE SPRSPL (ISLHDR, IMOD, ISLUN, ISFIND, IPBLK, BLC, TRC,
     *   IERR)
C-----------------------------------------------------------------------
C   Given an open slice file and an open plot file, SPRSPL will write
C   plot commands to display the difference between the slice data
C   and a slice model
C   Inputs:
C      ISLHDR  I(256)  the slice file header.
C      IMOD    I       the version number of the gaussian model.
C      ISLUN   I       logical unit no. of open slice file.
C      ISFIND  I       FTAB index for slice file.
C      IPBLK   I       plot file I/O buffer.
C      BLC     R(7)    the bottom left corner of the plot.
C      TRC     R(7)    the top right corner of the plot.
C   Output:
C      IERR    I       error code 0= none.
C                         1 = error during graphing. Try finish.
C                         2 = fatal error. Give up.
C-----------------------------------------------------------------------
      INTEGER   ISLHDR(256), IMOD, ISLUN, ISFIND, IPBLK(256), IERR
      REAL      BLC(7), TRC(7)
C
      REAL      X, Y, SCALEF, OFFSET, SBLK(256), PARM(12), YSLICE
      INTEGER   ISTART, IEND, IRRN, IPOS, ISBLK(256), IS, I0, I1,
     *   NGAUSS, I, IC, INDEX, ISL, IWTH, NRPBLK, IMAX
      LOGICAL   BLAST, T, F
      CHARACTER TXTMSG*80
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /WORK/ ISBLK
      EQUIVALENCE (ISBLK, SBLK)
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Init some variables.
      NRPBLK = 256
      SCALEF = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      OFFSET = 40000.0 - SCALEF * CATR(IRRAN+1)
      ISTART = BLC(1) + .5
      IEND = TRC(1) + .5
C                                       Read model data.
      IRRN = ISLHDR(59) + IMOD - 1
      CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, SBLK, IERR)
      IF (IERR.NE.0) GO TO 930
C                                       Move model data from buffer.
      IS = 24 + 1
      I0 = ISBLK(IS)
      I1 = ISLHDR(57) - ISBLK(IS+1)
      NGAUSS = ISBLK(IS+2)
      DO 10 I = 1,12
         PARM(I) = SBLK(I)
 10      CONTINUE
      DO 15 I = 1,NGAUSS
         IC = 3*I - 2
         PARM(IC) = PARM(IC)
 15      CONTINUE
C                                       Read 1st block of slice pts.
      IRRN = 3 + (ISTART - 1) / NRPBLK
      CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, SBLK, IERR)
      IF (IERR.NE.0) GO TO 930
      INDEX = ISTART - ((IRRN - 3) * NRPBLK)
      BLAST = T
C                                       Plot residual data points.
      TXTMSG = 'Draw slice minus model'
      CALL GCOMNT (2, TXTMSG, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 950
      CALL GLTYPE (4, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 950
      DO 50 ISL = ISTART,IEND
C                                       Get new block of data
         IF (INDEX.GT.NRPBLK) THEN
            IRRN = IRRN + 1
            CALL ZFIO ('READ', ISLUN, ISFIND, IRRN, SBLK, IERR)
            IF (IERR.NE.0) GO TO 930
            INDEX = 1
            END IF
C                                       Get actual value
         YSLICE = SBLK(INDEX)
         INDEX = INDEX + 1
C                                       Blanked pixel found. Dont show
         IF (YSLICE.EQ.FBLANK) THEN
            BLAST = T
         ELSE
            X = ISL
            YSLICE = YSLICE
C                                       Add up all components
            Y = 0.0
            DO 40 IC = 1,NGAUSS
               IMAX = 3 * IC - 2
               IPOS = IMAX + 1
               IWTH = IPOS + 1
               Y = Y + PARM(IMAX) * EXP (-2.772 * (PARM(IPOS) - X)
     *            ** 2  /  (PARM(IWTH) * PARM(IWTH)))
 40            CONTINUE
C
            Y = SCALEF * (YSLICE - Y)  +  OFFSET
            Y = MIN (Y, TRC(2))
            Y = MAX (Y, BLC(2))
            IF (BLAST) THEN
               CALL GPOS (X, Y, IPBLK, IERR)
               BLAST = F
            ELSE
               CALL GVEC (X, Y, IPBLK, IERR)
               END IF
            IF (IERR.NE.0) GO TO 950
            END IF
 50      CONTINUE
      GO TO 999
C                                       Slice read error.
 930  IERR = 1
      MSGTXT = 'SLICE FILE READ ERROR'
      CALL MSGWRT (6)
      GO TO 999
C                                       TEK write error.
 950  IERR = 2
      MSGTXT = 'PLOT FILE WRITE ERROR'
      CALL MSGWRT (7)
C
 999  RETURN
      END
