LOCAL INCLUDE 'GREYS.INC'
      INCLUDE 'INCS:DCNT.INC'
      INCLUDE 'INCS:PTVC.INC'
      CHARACTER NAMIN(4)*12, CLSIN(4)*6, IGFILE*48, OFMFIL*48,
     *   INFILE*48, GPHFNS(3)*2
      HOLLERITH XNAMIN(3), XCLSIN(2), XNAMI2(3), XCLSI2(2), XNAMI3(3),
     *   XCLSI3(2), XNAMI4(3), XCLSI4(2), XFUN, XOFMF(12), XINFIL(12)
      REAL      XDCONT, INCOLR, SEQIN, DSKIN, SEQIN2, DSKIN2, SEQIN3,
     *   DSKIN3, SEQIN4, DSKIN4, XBLC(7), XTRC(7), XYRATO, RANGE(2),
     *   APARM(10), TLABEL, DOGRID, PLEV, CLEV, LEVS(30), DOWDGE,
     *   DOCIRC, XSTVER, STMULT, XHPBP, SLST, SLEND, XNBOX, BOX(4,50),
     *   XDOTV, XGRCH, XTVCH, DODARK, XDKLIN, XTVCRN(2)
      LOGICAL   DOCONT, CSAME, DOTV, DOOFM, DOCOLR, BMBLNK
      DOUBLE PRECISION    GFAC, GOFF
      REAL      MULT, WRANGE(2), ROFM(TVMLOU), GOFM(TVMLOU),
     *   BOFM(TVMLOU), BLC(7,4), TRC(7,4), RANGES(2,3)
      INTEGER   IMLUN(4), IMFIND(4), IGLUN, IGFIND, IVOL(4), CNO(4),
     *   ILPVAL, IHPVAL, ILABEL, IVER, INPRMS, IXWDGE, IYWDGE, STVER,
     *   IPOINT, ISLST, ISLEND, NBOX, GRCHN, TVCHN, TVCORN(2), ISEQ(4),
     *   BMCORN, NOFM
      COMMON /INPARM/ XDCONT, INCOLR, XNAMIN, XCLSIN, SEQIN, DSKIN,
     *   XNAMI2, XCLSI2, SEQIN2, DSKIN2, XNAMI3, XCLSI3, SEQIN3, DSKIN3,
     *   XNAMI4, XCLSI4, SEQIN4, DSKIN4, XBLC, XTRC, XYRATO, RANGE,
     *   XFUN, APARM, TLABEL, DOGRID, PLEV, CLEV, LEVS, DOWDGE, DOCIRC,
     *   XSTVER, STMULT, XHPBP, SLST, SLEND, XNBOX, BOX, XDOTV, XGRCH,
     *   XTVCH, DODARK, XDKLIN, XTVCRN, XOFMF, XINFIL
      COMMON /CHARPM/ NAMIN, CLSIN, IGFILE, OFMFIL, INFILE, GPHFNS
      COMMON /GREYPR/ GFAC, GOFF, MULT, WRANGE, DOCONT, CSAME, IMLUN,
     *   IMFIND, IGLUN, IGFIND, IVOL, CNO, ILPVAL, IHPVAL, ILABEL,
     *   IVER, INPRMS, IXWDGE, IYWDGE, STVER, IPOINT, ISLST, ISLEND,
     *   NBOX, DOTV, GRCHN, TVCHN, TVCORN, DOOFM, DOCOLR, ROFM, GOFM,
     *   BOFM, BLC, TRC, RANGES, ISEQ, BMCORN, BMBLNK, NOFM
C                                       Header blocks etc
      INTEGER   PCATI(256,4)
      REAL      PCATR(256,4)
      HOLLERITH PCATH(256,4)
      DOUBLE PRECISION PCATD(128,4)
      REAL      RBLK(MABFSS,4)
      INTEGER   IBLK(MABFSS,4)
      COMMON /MAPHDR/ PCATI
      INCLUDE 'INCS:DCNT.INC'
      EQUIVALENCE (PCATI, PCATR, PCATH, PCATD)
      EQUIVALENCE (IBLK, RBLK, IBUFF)
LOCAL END
      PROGRAM GREYS
C-----------------------------------------------------------------------
C! Generates grey scale plotfile with optional superimposed contours
C# Graphics Map-util Plot-appl EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2002-2004, 2006-2008, 2011-2012,
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
C   GREYS will write commands to a plot file for the execution
C   of a grey scale plot for the first cataloged image file.  The 2nd
C   catalog image, if specified, will be represented as contours
C   superimposed on the grey scale. Null file name implies no contours.
C   The program runs as a detached task initiated from AIPS. First a
C   cataloged file is found using data passed from AIPS.  The list of
C   associated files is searched for PLot files to find the highest
C   version number.  Then a PLot file is created for this map and the
C   catalog header is updated.  Next the graphics commands are
C   written to the plot file.
C   INPUTS:   (from AIPS)
C      XDCONT   R      >= 0 => want contours.  Must also specify
C                      IN2NAME, IN2CLASS, IN2SEQ, and/or IN2DISK
C      USERID   R      user number, 0 means use logon user number,
C                      32000 means any user can be accessed.
C      INNAME   R(3)   name of grey scale image.
C      INCLASS  R(2)   class of grey scale image.
C      INSEQ    R      sequence number of grey scale image.
C      INDISK   R      disk volume number. 0 means try all.
C      IN2NAME  R(3)   name of contour image.
C      IN2CLASS R(2)   class of contour image.
C      IN2SEQ   R      sequence number of contour image.
C      IN2DISK  R      disk volume number. 0 means try all.
C      BLC      R(7)   the coordinate in the input file to become the
C                      left hand coordinate (1,1) of the contour plot.
C                      BLC(1) is the X coordinate and BLC(2) is the Y
C                      coordinate.  The first coordinate in the input
C                      image is (1,1).
C      TRC      R(7)   the coordinate in the input file to become the
C                      top right hand corner of the plot.
C      XYRATIO  R      the ratio between the scale factor to use for the
C                      X axis and the scale factor to use for the Y axis
C      PIXRANGE R(2)   the maximum and minimum values allowed for the
C                      map.  All other values will be clipped.  If
C                      IRANGE(1) .GE. IRANGE(2) then the map max and
C                      min will be used.
C      LTYPE    R      the type of axis labeling to use for this plot
C                          1 = none
C                          2 = leave out ticks and tick labels
C                          3 = RA - DEC coordinates & labels
C                          4 = Center relative
C                          5 = Subimage center-relative
C                          6 = pixel count
C      DOALIGN  R      >= 0 => contour image must align w grey
C      PLEV     R      the percentage of the peak value to use as the
C                      multiplier for the contour levels.  If zero use
C                      CLEV below.
C      CLEV     R      The absolute value of the multiplier used for the
C                      contour levels.  This value is used only if PLEV
C                      is zero.
C      LEVS     R(30)  the contour levels.  An out of sequence level
C                      indicates 'end of levels'.  The real value of a
C                      particular level is the LEV value times CLEV or
C                      the value determined by PLEV.
C      DOWEDGE  R      0 < DOWEDGE <= 1 -> grey wedge along top
C                      1 < DOWEDGE -> grey wedge along right edge
C      DOCIRCLE R      > 0 Plot coord grid rather than just ticks
C      STVERS   R      ST file version number.
C      STFACTOR R      scale star sizes in file for plotting:
C                         0 => no plot of stars.
C      IN2VERS  R      Start SL file version number to begin plotting
C                      locations of slices
C      IN3VERS  R      End SL file version number
C      NBOXES   R      Number of BOX regions to mark on plot
C      BOX      R(4,50)  Pixel ranges of BOXes to be plotted (need not
C                      fit fully in the plot area).
C      DOTV     R      > 0 => TV, else plot file
C      TVCHAN   R      grey-scale channel to use (0 => 1)
C      GRCHAN   R      graphics channel to use
C      TVCORN   R(2)   TV pixel to use (both > 0 => pixel scale)
C-----------------------------------------------------------------------
      REAL      X, TEMP, Y, CH(4), VMUL, NBLC(2), NTRC(2), ATRC(2),
     *   ABLC(2), DUM
      INTEGER   IROUND, PLBUF(256), IANGLE, IBLCX, IBLCY, IERR, INPIXS,
     *   IPOS, IRETCD, IROW, ITRCX, ITRCY, ICOL, ISNCUT, INPIXT, I, ICL,
     *   IDEPTH(5), JPOS, KPOS, IC(3), J, BBLC(2), BTRC(2), BXOFF, BXSIZ
      LOGICAL   DOGR, T
      CHARACTER TXTMSG*80
      INCLUDE 'GREYS.INC'
      REAL      XLROW(MABFSS)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IANGLE /0/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      JBUFSZ = 2 * MABFSS
C                                       GET THIS RIGHT!  CHECK against
C                                       the INPARM common in the local
C                                       include above.  Was incorrectly
C                                       set here to 121 which is fatal
C                                       on some systems as it causes
C                                       GTPARM to overwrite ZDCHI2 stuff
      INPRMS = 331
      ISNCUT = 0
      CALL GRYPRM (IRETCD, PLBUF)
      IF (IRETCD.NE.0) GO TO 999
C                                       Open files, set parms
      CALL GRYINI (CH, PLBUF, IRETCD)
      IF (IRETCD.NE.0) GO TO 995
      IRETCD = 8
C                                       Init grey scale for dB read
      IBLCX = IROUND (BLC(1,1))
      IBLCY = IROUND (BLC(2,1))
      ITRCX = IROUND (TRC(1,1))
      ITRCY = IROUND (TRC(2,1))
      INPIXS = ITRCX - IBLCX + 1
      INPIXT = INPIXS + IXWDGE
C                                       Write axis labeling commands.
      CALL GLAB (CH, NBLC, NTRC, ABLC, ATRC, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 950
      IF (BMBLNK) THEN
         CALL PLTBSZ (BMCORN, BLC, TRC, PCATR, BBLC, BTRC)
      ELSE
         BBLC(1) = 0
         BBLC(2) = 0
         BTRC(1) = 0
         BTRC(2) = 0
         END IF
      TXTmsg = 'Start grey scale'
      CALL GCOMNT (2, TXTmsg, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 950
C                                       Start of grey scale
C                                       Top step wedge
      IF (IYWDGE.GT.0) THEN
         ICL = 1
         VMUL = (WRANGE(2) - WRANGE(1)) / (INPIXT - 1)
         DO 10 ICOL = 1,INPIXS
            XLROW(ICOL) = (ICOL - 1.) * VMUL + WRANGE(1)
 10         CONTINUE
         CALL GSCALE (GPHFUN, RANGE, INPIXS, 1, XLROW, ILROW)
         IF (INCOLR.GT.0.0) THEN
            CALL FILL (INPIXT, GPHTLO, IBBUFF)
            CALL FILL (INPIXT, GPHTLO, IBLROW)
         ELSE IF (DOOFM) THEN
            CALL G3SCAL (INPIXT, ILROW, NOFM, ROFM, GOFM, BOFM, ILROW,
     *         IBBUFF, IBLROW)
            END IF
         DO 15 IROW = 1,IYWDGE
            X = IBLCX
            Y = ITRCY + IROW
            CALL GPOS (X, Y, PLBUF, IERR)
            IF (IERR.NE.0) GO TO 950
            IF (INCOLR.GT.0.0) THEN
               IF (IROW.LE.IYWDGE/3) THEN
                  CALL G3COLR (INPIXT, IANGLE, ILROW, IBBUFF, IBLROW,
     *               PLBUF, IERR)
               ELSE IF (IROW.LE.(2*IYWDGE)/3) THEN
                  IF (ICL.NE.2) THEN
                     CALL GSCALE (GPHFNS(2), RANGES(1,2), INPIXS, 1,
     *                  XLROW, ILROW)
                     ICL = 2
                     END IF
                  CALL G3COLR (INPIXT, IANGLE, IBBUFF, ILROW, IBLROW,
     *               PLBUF, IERR)
               ELSE
                  IF (ICL.NE.3) THEN
                     CALL GSCALE (GPHFNS(3), RANGES(1,3), INPIXS, 1,
     *                  XLROW, ILROW)
                     ICL = 3
                     END IF
                  CALL G3COLR (INPIXT, IANGLE, IBBUFF, IBLROW, ILROW,
     *               PLBUF, IERR)
                  END IF
            ELSE IF (DOCOLR) THEN
               CALL G3COLR (INPIXT, IANGLE, ILROW, IBBUFF, IBLROW,
     *            PLBUF, IERR)
            ELSE
               CALL GRAYPX (INPIXT, IANGLE, ILROW, PLBUF, IERR)
               END IF
            IF (IERR.NE.0) GO TO 950
 15         CONTINUE
         END IF
C                                       init I/O
      BXOFF = BBLC(1) - IBLCX
      BXSIZ = BTRC(1) - BBLC(1) + 1
      CALL DBINIT (IMLUN(1), IMFIND(1), PCATI(1,1), BLC(1,1), TRC(1,1),
     *   JBUFSZ, RBLK(1,1), IERR)
      IF ((IERR.EQ.0) .AND. (INCOLR.GT.0.0)) THEN
         CALL DBINIT (IMLUN(3), IMFIND(3), PCATI(1,3), BLC(1,3),
     *      TRC(1,3), JBUFSZ, RBLK(1,3), IERR)
         IF (IERR.EQ.0) CALL DBINIT (IMLUN(4), IMFIND(4), PCATI(1,4),
     *      BLC(1,4), TRC(1,4), JBUFSZ, RBLK(1,4), IERR)
         END IF
      IF (IERR.NE.0) GO TO 950
C                                       Loop over all rows.
      DO 50 IROW = IBLCY,ITRCY
C                                       Read proper row.
         CALL MDISK ('READ', IMLUN(1), IMFIND(1), RBLK(1,1), IPOS, IERR)
         IF ((IERR.EQ.0) .AND. (INCOLR.GT.0.0)) THEN
            CALL MDISK ('READ', IMLUN(3), IMFIND(3), RBLK(1,3), JPOS,
     *         IERR)
            IF (IERR.EQ.0) CALL MDISK ('READ', IMLUN(4), IMFIND(4),
     *         RBLK(1,4), KPOS, IERR)
            END IF
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1015) IERR
            CALL MSGWRT (7)
            GO TO 950
            END IF
         IF ((IROW.GE.BBLC(2)) .AND. (IROW.LE.BTRC(2))) THEN
            CALL RFILL (BXSIZ, FBLANK, RBLK(IPOS+BXOFF,1))
            IF (INCOLR.GT.0.0) THEN
               CALL RFILL (BXSIZ, FBLANK, RBLK(JPOS+BXOFF,3))
               CALL RFILL (BXSIZ, FBLANK, RBLK(KPOS+BXOFF,4))
               END IF
            END IF
C                                       Position.
         X = IBLCX
         Y = IROW
         CALL GPOS (X, Y, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL GSCALE (GPHFNS(1), RANGES(1,1), INPIXS, 1, RBLK(IPOS,1),
     *      IBLK(IPOS,1))
         IF (INCOLR.GT.0.0) THEN
            CALL GSCALE (GPHFNS(2), RANGES(1,2), INPIXS, 1,
     *         RBLK(JPOS,3), IBLK(JPOS,3))
            CALL GSCALE (GPHFNS(3), RANGES(1,3), INPIXS, 1,
     *         RBLK(KPOS,4), IBLK(KPOS,4))
         ELSE IF (DOOFM) THEN
            JPOS = IPOS
            KPOS = IPOS
            CALL G3SCAL (INPIXS, IBLK(IPOS,1), NOFM, ROFM, GOFM, BOFM,
     *         IBLK(IPOS,1), IBLK(JPOS,3), IBLK(KPOS,4))
               END IF
         IF (DOCOLR) THEN
            CALL G3COLR (INPIXS, IANGLE, IBLK(IPOS,1), IBLK(JPOS,3),
     *         IBLK(KPOS,4), PLBUF, IERR)
         ELSE
            CALL GRAYPX (INPIXS, IANGLE, IBLK(IPOS,1), PLBUF, IERR)
            END IF
         IF (IERR.NE.0) GO TO 950
 50      CONTINUE
C                                       add wedge to right
      IF (IXWDGE.GT.0) THEN
         VMUL = (WRANGE(2) - WRANGE(1)) / (TRC(2,1) - BLC(2,1))
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
            CALL GSCALE (GPHFNS(1), RANGE, 1, 1, TEMP, IC(1))
            IF (DOOFM) CALL G3SCAL (1, IC(1), NOFM, ROFM, GOFM, BOFM,
     *         IC(1), IC(2), IC(3))
            IF (INCOLR.GT.0.0) THEN
               CALL GSCALE (GPHFNS(2), RANGES(1,2), 1, 1, TEMP, IC(2))
               CALL GSCALE (GPHFNS(3), RANGES(1,3), 1, 1, TEMP, IC(3))
               CALL FILL (J, IC(1), IBUFF(1))
               CALL FILL (J, IC(2), IBBUFF(1+J))
               CALL FILL (J, IC(3), IBLROW(1+J+J))
               CALL G3COLR (IXWDGE, IANGLE, IBUFF, IBBUFF, IBLROW,
     *            PLBUF, IERR)
            ELSE IF (DOCOLR) THEN
               CALL FILL (IXWDGE, IC(1), IBUFF)
               CALL FILL (IXWDGE, IC(2), IBBUFF)
               CALL FILL (IXWDGE, IC(3), IBLROW)
               CALL G3COLR (IXWDGE, IANGLE, IBUFF, IBBUFF, IBLROW,
     *            PLBUF, IERR)
            ELSE
               CALL FILL (IXWDGE, IC(1), IBUFF)
               CALL GRAYPX (IXWDGE, IANGLE, IBUFF, PLBUF, IERR)
               END IF
            IF (IERR.NE.0) GO TO 950
 60         CONTINUE
         END IF
C                                       Draw borders.
      TXTMSG = 'Start labeling'
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
C                                       Axis labels, ticks
      DOGR = DOCIRC.GT.0.0
      CALL CLAB1 (ABLC, ATRC, CH, ILABEL, XYRATO, DOGR, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 950
      CALL GTIC (ILABEL, BLC, TRC, NTRC, ATRC, WRANGE, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 950
C                                       Draw stars
C                                       Much advantage from grey image
C                                       being the main plot image
      CALL FNDEXT ('ST', PCATI, I)
      IF ((STMULT.NE.0.0) .AND. (I.GT.0)) THEN
         DO 65 I = 1,5
            IDEPTH(I) = IROUND(BLC(I+2,1))
 65      CONTINUE
         CALL SETLOC (IDEPTH, T)
         CALL GLTYPE (4, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL STARPL (STMULT, IVOL, CNO, STVER, ABLC, ATRC, IMLUN,
     *      IMFIND, PCATI, BLC, 1, PLBUF, IERR)
         IF (IERR.GE.3) GO TO 950
         END IF
C                                       Draw slices
      IF (ISLST.GT.0 .AND. ISLEND.GT.0) THEN
         CALL GLTYPE (4, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL SLICPL (CNO(1), IVOL(1), ABLC, ATRC, ISLST, ISLEND, PLBUF,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1060)
            CALL MSGWRT (8)
            END IF
         END IF
C                                       Draw boxes
      IF (NBOX.GE.1) THEN
         CALL GLTYPE (4, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL BOXPL (ABLC, ATRC, NBOX, BOX, PLBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1061)
            CALL MSGWRT (8)
            END IF
         END IF
C                                       Draw boxes
      IF (INFILE.NE.' ') THEN
         CALL GLTYPE (4, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL LAYOUT (ABLC, ATRC, PCATR, INFILE, PLBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1061)
            CALL MSGWRT (8)
            END IF
         END IF
C                                       Do contours
      IF (DOCONT) THEN
         TXTMSG = 'Start contouring'
         CALL GCOMNT (2, TXTMSG, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL GLTYPE (2, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
C                                        Init map for contours
         CALL DBINIT (IMLUN(2), IMFIND(2), PCATI(1,2), BLC(1,2),
     *      TRC(1,2), JBUFSZ, RBLK(1,1), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1063)
            CALL MSGWRT (8)
            GO TO 950
            END IF
         CALL DBINIT (IMLUN(1), IMFIND(1), PCATI(1,1), BLC(1,1),
     *      TRC(1,1), JBUFSZ, RBLK(1,2), IERR)
         IF (IERR.NE.0) GO TO 950
C                                       Draw contours.
         CALL CONDRW (IMLUN(2), IMFIND(2), IMLUN(1), IMFIND(1), MULT,
     *      BLC, TRC, LEVS, 0, BBLC, BTRC, DUM, PLBUF, IERR)
         IF (IERR.GT.9) GO TO 960
         IF (IERR.NE.0) GO TO 950
         END IF
C                                       beam
      IF (BMCORN.GT.0) THEN
         I = LOCNUM
         LOCNUM = 3
         TXTMSG = 'Start contouring beam'
         CALL GCOMNT (-1, TXTMSG, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL GLTYPE (1, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 950
         CALL PLTBEM (BMCORN, BLC, TRC, PCATR, PLBUF, IERR)
         IF (IERR.GT.0) GO TO 960
         LOCNUM = I
         END IF
C                                       Finish up plot file
      CALL GFINIS (PLBUF, IERR)
C                                       Write sucessful finish message.
      IF (IERR.NE.0) GO TO 960
         IF (.NOT.DOTV) CALL HIPLOT (IVOL, CNO, IVER, PLBUF, IERR)
         IRETCD = 0
         WRITE (MSGTXT,1080) IVER
         CALL MSGWRT (2)
         GO TO 995
C-----------------------------------------------------------------------
C                                       Graph writing error.
 950  WRITE (MSGTXT,1950)
      CALL MSGWRT (8)
C                                       Try to do finish.
      CALL GFINIS (PLBUF, IERR)
      IF (IERR.NE.0) GO TO 960
         IF (.NOT.DOTV) CALL HIPLOT (IVOL, CNO, IVER, PLBUF, IERR)
         IRETCD = 0
         GO TO 995
C                                       Finish not sucessful. Destroy.
 960  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (IGLUN, IGFIND, IERR)
         CALL ZDESTR (IVOL, IGFILE, IERR)
         IF (IVER.GT.0) CALL DELEXT ('PL', IVOL(1), CNO(1), 'READ',
     *      PCATI(1,1), PLBUF, IVER, IERR)
         END IF
C                                       Close map file.
 995  CALL DIE (IRETCD, PLBUF)
C
 999  STOP
C-----------------------------------------------------------------------
 1015 FORMAT ('MREAD ERROR =',I5)
 1060 FORMAT ('ERROR OVERLAYING SLICES')
 1061 FORMAT ('ERROR OVERLAYING BOXES')
 1063 FORMAT ('ERROR INITING MAP FOR CONTOURING')
 1080 FORMAT ('Successful plot file version',I5,'  created.')
 1950 FORMAT ('Error during graphing will try to finish partial graph')
      END
      SUBROUTINE GRYPRM (IRETCD, PLBUF)
C-----------------------------------------------------------------------
C   Routine to get parameters for grey scale task
C   INPUTS
C       PLBUF  I(256)    Work buffer
C   OUTPUTS
C       QUICK   L         .TRUE. => AIPS already restarted
C       IRETCD  I         Return code 0=> OK, else just go to STOP
C   Task parameters are returned in common /INPARM/
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   PLBUF(*), IRETCD
C
      INTEGER   IERR, IROUND
      INCLUDE 'GREYS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGNAM /'GREYS '/
C-----------------------------------------------------------------------
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = MABFSS * 2
      NCFILE = 0
      NSCR = 0
C                                       Get input values from AIPS.
      CALL GTPARM (PRGNAM, INPRMS, RQUICK, XDCONT, PLBUF, IRETCD)
      IF (IRETCD.NE.0) RQUICK = .TRUE.
      IF (IRETCD.EQ.1) GO TO 999
      IF (RQUICK) CALL RELPOP (IRETCD, PLBUF, IERR)
      IF (IRETCD.NE.0) GO TO 999
C                                       AIPS Holleriths ->
C                                       characters
      CALL H2CHR (12, 1, XNAMIN, NAMIN(1))
      CALL H2CHR (6, 1, XCLSIN, CLSIN(1))
      CALL H2CHR (12, 1, XNAMI2, NAMIN(2))
      CALL H2CHR (6, 1, XCLSI2, CLSIN(2))
      CALL H2CHR (12, 1, XNAMI3, NAMIN(3))
      CALL H2CHR (6, 1, XCLSI3, CLSIN(3))
      CALL H2CHR (12, 1, XNAMI4, NAMIN(4))
      CALL H2CHR (6, 1, XCLSI4, CLSIN(4))
      CALL H2CHR (48, 1, XOFMF, OFMFIL)
      CALL H2CHR (48, 1, XINFIL, INFILE)
      ISEQ(1) = IROUND (SEQIN)
      ISEQ(2) = IROUND (SEQIN2)
      ISEQ(3) = IROUND (SEQIN3)
      ISEQ(4) = IROUND (SEQIN4)
      IVOL(1) = IROUND (DSKIN)
      IVOL(2) = IROUND (DSKIN2)
      IVOL(3) = IROUND (DSKIN3)
      IVOL(4) = IROUND (DSKIN4)
C                                       Plot beam corners
      BMCORN = IROUND (XHPBP)
      BMBLNK = BMCORN.LT.0.0
      BMCORN = ABS (BMCORN)
      IF (BMCORN.GT.20) BMCORN = 1
      IF (MOD(BMCORN,5).EQ.0) BMCORN = BMCORN - 4
C                                       Do contours?: request &
C                                       not all of name null
      DOCONT = XDCONT.GT.0.0
      IF (DOCONT) THEN
         IF ((NAMIN(2).EQ.' ') .AND. (CLSIN(2).EQ.' ') .AND.
     *      (ISEQ(2).LT.1) .AND. ((IVOL(2).LT.1) .OR.
     *      (IVOL(2).GT.NVOL))) THEN
            DOCONT = .FALSE.
            XDCONT = -1.0
C                                       Name defaults
         ELSE
            IF (NAMIN(2).EQ.' ') NAMIN(2) = NAMIN(1)
            IF (CLSIN(2).EQ.' ') CLSIN(2) = CLSIN(1)
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE GRYINI (CH, PLBUF, IRET)
C-----------------------------------------------------------------------
C   GRYINI performs the initialization of parameters and the creation
C   of the plot file.  It also opens the files.
C   Output:
C      CH       R(4)     Number chars outside plot - special here
C      PLBUF   I(256)   Plot buffer
C      IRET     I        0 => proceed, 1 => quit
C-----------------------------------------------------------------------
      REAL       CH(4)
      INTEGER    PLBUF(*), IRET
C
      CHARACTER TYPE*2, OPCODE*4, RGB*8, FUNCS(8)*2
      REAL      X, PEAK,  RANGE2(2), WRANG2(2)
      INTEGER   IROUND, IERR, IGSIZE, ITYPE, IUSER, I, J, IFUN, LTYPE
      LOGICAL   T
      INCLUDE 'GREYS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA T /.TRUE./
      DATA RGB /'RGB'/
      DATA FUNCS /'LN','LG','L2','SQ','NE','NG','N2','NQ'/
C-----------------------------------------------------------------------
      IMLUN(1) = 16
      IMLUN(2) = 17
      IMLUN(3) = 18
      IMLUN(4) = 19
      IGLUN = 26
      ILABEL = IROUND (TLABEL)
      LTYPE = MOD (ABS(ILABEL), 100)
      IF (LTYPE.LE.0) LTYPE = 3
      LTYPE = MOD (LTYPE-1,10) + 1
      IF (ILABEL.GT.0) THEN
         ILABEL = (ILABEL/100)*100 + LTYPE
      ELSE
         ILABEL = (ILABEL/100)*100 - LTYPE
         END IF
      TLABEL = ILABEL
      IUSER = NLUSER
      ISLST = IROUND (SLST)
      ISLEND = IROUND (SLEND)
      IF ((ISLST.LT.0) .OR. (ISLEND.LT.0)) THEN
         ISLST = 0
         ISLEND = 0
         END IF
      NBOX = IROUND (XNBOX)
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = XTVCH + 0.01
      TVCORN(1) = IROUND (XTVCRN(1))
      TVCORN(2) = IROUND (XTVCRN(2))
      IRET = 8
C                                       Open grey scale map file
      TYPE = 'MA'
      OPCODE = 'HDWR'
      IF (DOTV) OPCODE = 'READ'
      IVER = 0
      DO 10 I = 1,4
         IF ((I.EQ.1) .OR. ((I.EQ.2) .AND. (DOCONT)) .OR. ((I.GT.2)
     *      .AND. (INCOLR.GT.0.0))) THEN
            CALL MAPOPN (OPCODE, IVOL(I), NAMIN(I), CLSIN(I), ISEQ(I),
     *         TYPE, IUSER, IMLUN(I), IMFIND(I), CNO(I), PCATI(1,I),
     *         PLBUF, IERR)
            OPCODE = 'READ'
            IF (IERR.NE.0) GO TO 995
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = IVOL(I)
            FCNO(NCFILE) = CNO(I)
            FRW(NCFILE) = 0
C                                       Add extension file to header.
            IF ((I.EQ.1) .AND. (.NOT.DOTV)) THEN
               CALL MADDEX ('PL', IVOL, CNO, PCATI(1,1), PLBUF, .TRUE.,
     *            'READ', IVER, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
C                                       check for RGBC
            IF ((I.EQ.1) .AND. (INCOLR.GT.0.0)) THEN
               CALL AXEFND (8, RGB, PCATI(KIDIM,1), PCATH(KHCTP,1), J,
     *            IERR)
               IF ((IERR.EQ.0) .AND. (J.EQ.2) .AND.
     *            (PCATI(KINAX+2,1).GE.3)) THEN
                  NAMIN(3) = NAMIN(1)
                  CLSIN(3) = CLSIN(1)
                  ISEQ(3) = ISEQ(1)
                  IVOL(3) = IVOL(1)
                  NAMIN(4) = NAMIN(1)
                  CLSIN(4) = CLSIN(1)
                  ISEQ(4) = ISEQ(1)
                  IVOL(4) = IVOL(1)
                  INCOLR = 2.0
               ELSE
                  IF (((NAMIN(3).EQ.' ') .AND. (CLSIN(3).EQ.' ')) .OR.
     *               ((NAMIN(4).EQ.' ') .AND. (CLSIN(4).EQ.' '))) THEN
                     MSGTXT = 'DOCOLR REQUESTED, BUT RGB AXIS NOT' //
     *                  ' 3RD AXIS'
                     CALL MSGWRT (7)
                     INCOLR = -1.0
                  ELSE
                     INCOLR = 1.0
                     END IF
                  END IF
               END IF
            END IF
 10      CONTINUE
C                                       return adverbs
      SEQIN = ISEQ(1)
      SEQIN2 = ISEQ(2)
      SEQIN3 = ISEQ(3)
      SEQIN4 = ISEQ(4)
      DSKIN = IVOL(1)
      DSKIN2 = IVOL(2)
      DSKIN3 = IVOL(3)
      DSKIN4 = IVOL(4)
      CALL CHR2H (12, NAMIN(1), 1, XNAMIN)
      CALL CHR2H (12, NAMIN(2), 1, XNAMI2)
      CALL CHR2H (12, NAMIN(3), 1, XNAMI3)
      CALL CHR2H (12, NAMIN(4), 1, XNAMI4)
      CALL CHR2H (6, CLSIN(1), 1, XCLSIN)
      CALL CHR2H (6, CLSIN(2), 1, XCLSI2)
      CALL CHR2H (6, CLSIN(3), 1, XCLSI3)
      CALL CHR2H (6, CLSIN(4), 1, XCLSI4)
C                                       check ST plot parms
      I = 0
      IF (XSTVER.LT.0.0) STMULT = 0.0
      IF (STMULT.NE.0.0) CALL FNDEXT ('ST', PCATI(1,1), I)
      IF (I.GT.0) THEN
         J = XSTVER + 0.1
         IF (J.LE.0) J = I
         XSTVER = J
      ELSE
         XSTVER = 0
         STMULT = 0.0
         END IF
      STVER = IROUND (XSTVER)
      IF (STVER.LT.0) STMULT = 0.0
C                                       Check slices present if
C                                       requested
      IF (ISLST.GT.0) THEN
         CALL SLCHK (ISLST, ISLEND, IERR)
         IF (IERR.NE.0) THEN
            ISLST = 0
            ISLEND = 0
            END IF
         END IF
C                                       Check windows
      CALL ALIGN (IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get grey scale header info.
      IXWDGE = 0
      IYWDGE = 0
      IF (DOWDGE.GT.0.0) THEN
         IF ((DOWDGE.LT.1.5) .OR. ((DOWDGE.GE.2.5) .AND.
     *      (DOWDGE.LT.3.5))) THEN
            IYWDGE = MAX (1.0, (TRC(2,1)-BLC(2,1))/20.0) + 2.5
         ELSE
            IXWDGE = MAX (1.0, (TRC(1,1)-BLC(1,1))/20.) + 2.5
            END IF
         IF (INCOLR.GT.0.0) THEN
            IXWDGE = ((IXWDGE+2)/3) * 3
            IYWDGE = ((IYWDGE+2)/3) * 3
            END IF
         END IF
C                                       Build file name.
      CALL ZPHFIL ('PL', IVOL, CNO, IVER, IGFILE, IERR)
      CALL RCOPY (7, BLC(1,1), XBLC)
      CALL RCOPY (7, TRC(1,1), XTRC)
C                                       Grey scale range
      WRANGE(1) = 0.0
      WRANGE(2) = 0.0
      CALL RNGSET (RANGE, PCATR(KRDMX,1), PCATR(KRDMN,1), RANGE2)
      CALL RNGSET (WRANGE, PCATR(KRDMX,1), PCATR(KRDMN,1), WRANG2)
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
      J = IROUND (WRANG2(2))
      I = IROUND (WRANG2(1))
      WRANGE(1) = (I * GFAC + GOFF)
      WRANGE(2) = (J * GFAC + GOFF)
      RANGES(1,1) = RANGE(1)
      RANGES(2,1) = RANGE(2)
      IF (INCOLR.EQ.2.0) THEN
         IF (APARM(2).LE.APARM(1)) THEN
            RANGES(1,2) = RANGE(1)
            RANGES(2,2) = RANGE(2)
         ELSE
            CALL RNGSET (APARM(1), PCATR(KRDMX,1), PCATR(KRDMN,1),
     *         RANGES(1,2))
            END IF
         IF (APARM(4).LE.APARM(3)) THEN
            RANGES(1,3) = RANGE(1)
            RANGES(2,3) = RANGE(2)
         ELSE
            CALL RNGSET (APARM(3), PCATR(KRDMX,1), PCATR(KRDMN,1),
     *         RANGES(1,3))
            END IF
      ELSE IF (INCOLR.NE.1.0) THEN
         RANGES(1,2) = RANGE(1)
         RANGES(2,2) = RANGE(2)
         RANGES(1,3) = RANGE(1)
         RANGES(2,3) = RANGE(2)
      ELSE
         CALL RNGSET (APARM(1), PCATR(KRDMX,3), PCATR(KRDMN,3),
     *      RANGES(1,2))
         CALL RNGSET (APARM(3), PCATR(KRDMX,4), PCATR(KRDMN,4),
     *      RANGES(1,3))
         END IF
      CALL RCOPY (4, RANGES(1,2), APARM)
C                                       Default XYRATO: ratio of
C                                       incr if related.
      DO 60 I = 1,5
         PLBUF(I) = IROUND (BLC(I+2,1))
 60      CONTINUE
      LOCNUM = 3
      CALL SETLOC (PLBUF, T)
      LOCNUM = 2
      CALL SETLOC (PLBUF, T)
      LOCNUM = 1
      CALL SETLOC (PLBUF, T)
      IF ((XYRATO.LE.0.01) .OR. (XYRATO.GT.320.0)) THEN
         IF ((AXTYP(LOCNUM).EQ.1) .AND. (AXINC(2,LOCNUM).NE.0.0)) XYRATO
     *      = ABS (AXINC(1,LOCNUM) / AXINC(2,LOCNUM))
         IF (((XYRATO.LE.0.04) .OR. (XYRATO.GT.25.)) .AND.
     *      (TRC(1,1).NE.BLC(1,1))) XYRATO = (TRC(2,1)-BLC(2,1)) /
     *      (TRC(1,1)-BLC(1,1))
         IF ((XYRATO.LE.0.04) .OR. (XYRATO.GT.25.)) XYRATO = 1.0
         END IF
C                                       Determine approx file size.
      IGSIZE = 0
      X = (TRC(1,1)-BLC(1,1)+4+IXWDGE) * (TRC(2,1)-BLC(2,1)+1+IYWDGE) /
     *   256.0  +  5.0
      IF (X.GT.84.) IGSIZE = 1
      IF (X.GT.420.) IGSIZE = 2
      ITYPE = 27
C                                       Determine borders for dif labs.
C                                       extra border chars if necessary
      CALL RFILL (4, 0.0, CH)
      LTYPE = MOD (ABS(ILABEL),100)
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CH(2) = 1.333
C                                       Doing contours
      IF (DOCONT) THEN
C                                       Levels defaults
         PEAK = MAX (ABS(PCATR(KRDMX,2)), ABS(PCATR(KRDMN,2)))
         MULT = CLEV
         IF ((CLEV.EQ.0.0) .AND. (PLEV.EQ.0.0)) PLEV = 10.0
         IF (PLEV.NE.0.0) MULT = PEAK * PLEV / 100.0
         CLEV = MULT
         IF ((LEVS(1).EQ.0.0) .AND. (LEVS(2).LE.LEVS(1))) THEN
            DO 70 I = 1,10
               LEVS(I) = I - 11.
               LEVS(I+10) = I
               LEVS(I+20) = 0.0
 70            CONTINUE
            END IF
         IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
            CH(2) = 1.333
            IF (.NOT.CSAME) CH(4) = 1.333
            END IF
         END IF
C                                       grey scale function for extlist
      CALL H2CHR (2, 1, XFUN, GPHFUN)
      IFUN = 0
      DO 80 I = 1,8
         IF (GPHFUN.EQ.FUNCS(I)) IFUN = I
 80      CONTINUE
      IF (IFUN.LE.0) THEN
         IFUN = 1
         GPHFUN = FUNCS(IFUN)
         CALL CHR2H (2, GPHFUN, 1, XFUN)
         END IF
      GPHFNS(1) = GPHFUN
      I = APARM(5) + 0.1
      IF ((I.LE.0) .OR. (I.GT.8)) I = IFUN
      APARM(5) = I
      GPHFNS(2) = FUNCS(I)
      I = APARM(6) + 0.1
      IF ((I.LE.0) .OR. (I.GT.8)) I = IFUN
      APARM(6) = I
      GPHFNS(3) = FUNCS(I)
C                                        Write parms to PL file
      IF ((XDKLIN.LE.0.0) .OR. (XDKLIN.GT.1.0)) XDKLIN = 0.33
      IF (INCOLR.GT.0.0) OFMFIL = ' '
      CALL GINIT (IVOL, CNO, IGFILE, IGSIZE, ITYPE, INPRMS, XDCONT,
     *   DOTV, TVCHN, GRCHN, TVCORN, PCATI, PLBUF, IGLUN, IGFIND,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 990
         END IF
      IF (DOTV) GPHDOD = (DODARK.GT.0.0)
      GPHCUT = XDKLIN
      CALL GETOFM (OFMFIL, GPHDOT, GPHTVC, DOOFM, DOCOLR, ROFM, GOFM,
     *   BOFM, NOFM, IERR)
      IF (IERR.NE.0) THEN
         DOOFM = .FALSE.
         IERR = 0
         END IF
      IF (.NOT.DOOFM) DOCOLR = .FALSE.
      IF (INCOLR.GT.0.0) DOCOLR = .TRUE.
      IF (.NOT.DOOFM) OFMFIL = ' '
      CALL CHR2H (48, OFMFIL, 1, XOFMF)
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (7)
 995  IF (IVER.GT.0) CALL DELEXT ('PL', IVOL(1), CNO(1), 'READ',
     *   PCATI(1,1), PLBUF, IVER, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('GRAPH FILE INIT ERROR. GINIT ERR =',I5)
      END
      SUBROUTINE GLAB (CH, NBLC, NTRC, ABLC, ATRC, PLBUF, IERR)
C-----------------------------------------------------------------------
C   GLAB is an axis labelling routine for use with grey scale plots.
C   Optional contour labels.
C   In/out:
C      CH      R(4)     character spacing around plot.
C      NBLC    R(2)     BLC including wedge, extra space
C      NTRC    R(2)     TRC including wedge, extra space
C      ABLC    R(2)     BLC including edge of pixels
C      ATRC    R(2)     TRC including edge of pixels
C      PLBUF  I(256)   the updated graphics output buffer.
C   Output:
C      IERR    I        error indicator:  0 = No error.
C-----------------------------------------------------------------------
      REAL      NBLC(2), NTRC(2), CH(4), ATRC(2), ABLC(2)
      INTEGER   PLBUF(256), IERR
C
      CHARACTER SPRTXT*80, ATIME*8, ADATE*12, CHTMP*8, NAMSTR*18,
     *   PREFIX*5, CORTXT(2)*80, LTEXT(5)*80, CHTEMP*20
      DOUBLE PRECISION    ZV(3)
      REAL      DCX, DCY, BJUNK(2), YGAP, TEMP
      INTEGER   IDEPTH(5), NTEXT, I, IANGL, INCHAR, IT(3), ID(3), ITEMP,
     *   CATEMP(256), NL, IXL, IROUND, I1, I2, LTYPE
      LOGICAL   SLICE, T, LFLAG
      INCLUDE 'GREYS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DGPH.INC'
      DATA SLICE, T /.FALSE., .TRUE./
C-----------------------------------------------------------------------
      NBLC(1) = BLC(1,1) - 0.7
      NBLC(2) = BLC(2,1) - 0.7
      NTRC(1) = TRC(1,1) + 0.7 + IXWDGE
      NTRC(2) = TRC(2,1) + 0.7 + IYWDGE
      ABLC(1) = NBLC(1)
      ABLC(2) = NBLC(2)
      ATRC(1) = NTRC(1)
      ATRC(2) = NTRC(2)
      IF (IXWDGE.GT.0) ATRC(1) = TRC(1,1) + 0.5
      IF (IYWDGE.GT.0) ATRC(2) = TRC(2,1) + 0.5
C
      IDEPTH(1) = BLC(3,1) + .01
      IDEPTH(2) = BLC(4,1) + .01
      IDEPTH(3) = BLC(5,1) + .01
      IDEPTH(4) = BLC(6,1) + .01
      IDEPTH(5) = BLC(7,1) + .01
C                                       Init for line drawing.
      LOCNUM = 1
      CALL LABINI (BLC, TRC, IDEPTH, CH, ILABEL, SLICE, YGAP, CORTXT,
     *   NTEXT)
C                                       wedge labeling
      LTYPE = MOD (ABS(ILABEL),100)
      IF (LTYPE.GE.3) THEN
         IF (IXWDGE.GT.0) THEN
            CALL GTICNT (ILABEL, WRANGE, I)
            IF (I.GT.0) CH(3) = CH(3) + 0.5 + I
            END IF
         IF (IYWDGE.GT.0) CH(4) = CH(4) + 1.333
         END IF
C                                       Prepare LEVS lines
      NL = 0
      IF ((LTYPE.LT.7) .AND. (DOCONT)) THEN
         NL = NL + 1
         WRITE (LTEXT(NL),1120) MULT
         INCHAR = 22
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
            WRITE (CHTEMP,1121,ERR=5) TEMP
 5          IF (CHTEMP(10:11).EQ. ' -') CHTEMP(10:11) = '-0'
            IF (CHTEMP(10:11).EQ. '  ') CHTEMP(10:11) = ' 0'
            CHTEMP(I2+1:) = ' '
            CALL CHTRIM (CHTEMP, 20, CHTEMP, IXL)
            LTEXT(NL)(INCHAR:) = CHTEMP(:IXL) // ', '
            INCHAR = INCHAR + 2 + IXL
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
         CH(2) = CH(2) + (NL + 1) * 1.333
         END IF
C                                       Init for plotting
      CALL GINITL (NBLC, NTRC, XYRATO, CH, IDEPTH, PLBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       repeat GINIT kills this
      GPHFUN = GPHFNS(1)
C                                       Init for grey scale.
      IF (DOCOLR) THEN
         CALL GINITC (ILPVAL, IHPVAL, RANGES, PLBUF, IERR)
      ELSE
         CALL GINITG (ILPVAL, IHPVAL, RANGE, PLBUF, IERR)
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (7)
         GO TO 999
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
         CALL GPOS (NBLC(1), NTRC(2), PLBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         DCX = 0.0
         DCY = 0.5
         IF ((LTYPE.GE.3) .AND. (IYWDGE.GT.0)) DCY = 1.833
         SPRTXT = ' '
         INCHAR = 1
         IF ((DOCONT) .AND. (.NOT.CSAME)) THEN
            DCY = DCY + 1.333
            INCHAR = 7
            SPRTXT(1:5) = 'GREY:'
            END IF
         IANGL = 0
         CALL H2CHR (8, 1, PCATH(KHOBJ,1), CHTMP)
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
         CALL H2CHR (12, KHIMNO, PCATH(KHIMN,1), NAMSTR(1:12))
         CALL H2CHR (6, KHIMCO, PCATH(KHIMC,1), NAMSTR(13:18))
         CALL NAMEST (NAMSTR, PCATI(KIIMS,1), SPRTXT(INCHAR:), ITEMP)
         CALL REFRMT (SPRTXT, '_', INCHAR)
         CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Date/time version
         IF (ILABEL.GE.0) THEN
            CALL GPOS (NBLC(1), NTRC(2), PLBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            DCY = DCY + 1.333
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, ATIME, ADATE)
            WRITE (SPRTXT,1030) IVER, ADATE, ATIME
            CALL REFRMT (SPRTXT, '_', INCHAR)
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, PLBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
C                                       Peak flux for grey scale
         DCX = 0.0
         DCY = -YGAP
         YGAP = YGAP + 1.333
         IANGL = 0
         CALL GPOS (NBLC(1), NBLC(2), PLBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL H2CHR (8, 1, PCATH(KHBUN,1), CHTMP)
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
C                                       With Contours
      IF (DOCONT) THEN
         IF (.NOT.CSAME) THEN
            CALL COPY (256, PCATI(1,1), CATEMP)
            CALL COPY (256, PCATI(1,2), PCATI)
            IDEPTH(1) = BLC(3,2) + .01
            IDEPTH(2) = BLC(4,2) + .01
            IDEPTH(3) = BLC(5,2) + .01
            IDEPTH(4) = BLC(6,2) + .01
            IDEPTH(5) = BLC(7,2) + .01
            LOCNUM = 2
            CALL SETLOC (IDEPTH, T)
            CALL COPY (256, CATEMP, PCATI)
            IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) THEN
               DCX = (BLC(1,2) + TRC(1,2)) / 2.0
               DCY = (BLC(2,2) + TRC(2,2)) / 2.0
               CALL XYVAL (DCX, DCY, ZV(1), ZV(2), ZV(3), IERR)
               CALL AXSTRN (CTYP(3,LOCNUM), ZV(3), KLOCA(LOCNUM),
     *            NCHLAB(1,LOCNUM), SAXLAB(1,LOCNUM))
               END IF
C                                       Source name, stokes, freq.
C                                       for contour image
            IF (LTYPE.LT.7) THEN
               CALL GPOS (NBLC(1), NTRC(2), PLBUF, IERR)
               IF (IERR.NE.0) GO TO 980
               DCX = 0.0
               DCY = 0.5
               IF ((LTYPE.GE.2) .AND. (IYWDGE.GT.0)) DCY = 1.833
               IANGL = 0
               SPRTXT = 'CONT: '
               INCHAR = 7
               CALL H2CHR (8, 1, PCATH(KHOBJ,2), CHTMP)
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
               CALL H2CHR (12, KHIMNO, PCATH(KHIMN,2), NAMSTR(1:12))
               CALL H2CHR (6, KHIMCO, PCATH(KHIMC,2), NAMSTR(13:18))
               CALL NAMEST (NAMSTR, PCATI(KIIMS,2), SPRTXT(INCHAR:),
     *            ITEMP)
               CALL REFRMT (SPRTXT, '_', INCHAR)
               CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, PLBUF,
     *            IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            END IF
         LOCNUM = 1
C                                        Peak contour flux
         IF (LTYPE.LT.7) THEN
            DCX = 0.0
            DCY = -YGAP
            YGAP = YGAP + 1.333
            IANGL = 0
            INCHAR = 40
            CALL GPOS (NBLC(1), NBLC(2), PLBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            TEMP = PCATR(KRDMX,2)
            IF (ABS(TEMP).LT.ABS(PCATR(KRDMN,2))) TEMP = PCATR(KRDMN,2)
            CALL H2CHR (8, 1, PCATH(KHBUN,2), CHTMP)
            WRITE (SPRTXT,1110) TEMP, CHTMP
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, PLBUF, IERR)
            IF (IERR.NE.0) GO TO 980
C                                       Write levels.
            DCX = 0.0
            DCY = DCY - 1.333
            IANGL = 0
            DO 130 I = 1,NL
               CALL GPOS (NBLC(1), NBLC(2), PLBUF, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL REFRMT (LTEXT(I), ' ', INCHAR)
               CALL GCHAR (INCHAR, IANGL, DCX, DCY, LTEXT(I), PLBUF,
     *            IERR)
               IF (IERR.NE.0) GO TO 980
               DCY = DCY - 1.333
 130           CONTINUE
            END IF
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
 1121 FORMAT (F15.3)
 1980 FORMAT ('GRAPH LABEL WRITING ERROR. IERR =',I5)
      END
      SUBROUTINE SLCHK (IS1, IS2, IERR)
C-----------------------------------------------------------------------
C   Make sure there are slices to draw, and that the start and end slice
C   numbers are sensible
C   Input:
C      IS1   I    Start slice
C      IS2   I    End slice
C   Output:
C      IERR    I    0 => OK
C-----------------------------------------------------------------------
      INTEGER IS1, IS2, IERR, SLMAX
C
      INCLUDE 'GREYS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL FNDEXT ('SL', PCATI, SLMAX)
      IF (SLMAX.LE.0) THEN
         IERR = 1
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
      ELSE
         IF (IS2.LT.IS1) THEN
            WRITE (MSGTXT, 2000)
            CALL MSGWRT (8)
            IERR = 2
         ELSE IF ((IS1.GT.SLMAX) .OR. (IS2.GT.SLMAX)) THEN
            WRITE (MSGTXT, 2000)
            CALL MSGWRT (8)
            IERR = 2
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SLCHK: THERE ARE NO SLICES ASSOCIATED WITH THIS IMAGE')
 2000 FORMAT ('SLCHK: INVALID START AND END SLICE NUMBERS')
      END
      SUBROUTINE SLICPL (CNO, IVOL, BLC, TRC, ISLST, ISLEND, PLBUF,
     *   IERR)
C-----------------------------------------------------------------------
C   Draw slices on plot
C   Inputs:
C      CNO      I        Catalogue slot of image
C      IVOL     I        Disk number of image
C      BLC      R(2)     BLC of contour plot
C      TRC      R(2)     TRC of contour plot
C      ISLST    I        Start slice version
C      ISLEND   I        End slice version
C   In/out:
C      PLBUF    I(256)   Work area for graphics i/o
C   Output:
C      IERR     I        Error code: > 0 => plot error
C-----------------------------------------------------------------------
      INTEGER   CNO, IVOL, ISLST, ISLEND, PLBUF(256), IERR
      REAL      BLC(2), TRC(2)
C
      REAL      SBLK4(128), SLBLC(2), SLTRC(2)
      INTEGER   JERR, LUN, ISFIND, SBLK2(256), I, NPL
      LOGICAL   EXCL, WAIT
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (SBLK2, SBLK4)
      DATA LUN /27/
      DATA WAIT, EXCL /.TRUE., .TRUE./
C-----------------------------------------------------------------------
C                                           loop over slice files
      NPL = 0
      DO 100 I = ISLST,ISLEND
C                                           open slice file
         CALL OPEXT ('SL', IVOL, CNO, I, LUN, EXCL, WAIT, ISFIND,
     *      JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1000) I, JERR
            CALL MSGWRT (8)
            GO TO 100
            END IF
C                                           find slice BLC and TRC
         CALL ZFIO ('READ', LUN, ISFIND, 2, SBLK2, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1010) I, JERR
            CALL MSGWRT (8)
            GO TO 90
            END IF
         SLBLC(1) = SBLK4(19)
         SLBLC(2) = SBLK4(20)
         SLTRC(1) = SBLK4(26)
         SLTRC(2) = SBLK4(27)
C                                           plot slice
         CALL LINEPL (BLC, TRC, SLBLC, SLTRC, PLBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) I
            CALL MSGWRT (8)
            GO TO 90
            END IF
         NPL = NPL + 1
C                                           close slice file
 90      CALL ZCLOSE (LUN, ISFIND, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1090) I, JERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
 100     CONTINUE
C
 990  WRITE (MSGTXT,1990) NPL
      CALL MSGWRT (3)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SLICPL: ERROR OPENING SLICE FILE #', I3, ' IERR=', I3)
 1010 FORMAT ('SLICPL: ERROR READING REC. 2 OF SLICE #', I3,
     *        ' IERR=', I3)
 1020 FORMAT ('SLICPL: ERROR PLOTTING SLICE #', I3)
 1090 FORMAT ('SLICPL: ERROR CLOSING SLICE #', I3, ' IERR=', I3)
 1990 FORMAT ('SLICPL: Successfully plotted ', I3, ' slices')
      END
      SUBROUTINE LINEPL (BLC, TRC, P1, P2, PLBUF, IERR)
C-----------------------------------------------------------------------
C   Draw a line segment
C   Inputs:
C      BLC      R(2)     BLC of image being contoured
C      TRC      R(2)     TRC of image being contoured
C      P1       R(2)     Location (X,Y pixels) of line end 1
C      P2       R(2)     Location (X,Y pixels) of line end 2
C   In/out:
C      PLBUF   I(256)   i/o buffer
C   Outputs:
C      IERR     I      0 => OK
C-----------------------------------------------------------------------
      REAL      P1(2), P2(2), BLC(2), TRC(2)
      INTEGER   IERR, PLBUF(256)
C
      REAL      CPX(2), CPY(2)
      INTEGER   I
C-----------------------------------------------------------------------
      CPX(1) = P1(1)
      CPY(1) = P1(2)
      CPX(2) = P2(1)
      CPY(2) = P2(2)
      CALL LINLIM (BLC, TRC, CPX, CPY, I)
      IERR = 0
      IF (I.EQ.0) THEN
         CALL GPOS (CPX(1), CPY(1), PLBUF, IERR)
         IF (IERR.EQ.0) CALL GVEC (CPX(2), CPY(2), PLBUF, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE BOXPL (BLC, TRC, NBOX, BOX, PLBUF, IERR)
C-----------------------------------------------------------------------
C   Plot the boxes
C   Input:
C      BLC      R(2)      BLC of image being contoured
C      TRC      R(2)      TRC of image being contoured
C      NBOX     I         Number of boxes
C      BOX      R(4,50)   Boxes
C   In/out:
C      PLBUF   I(256)    i/o buffer
C   Output:
C      IERR     I         0 => OK
C-----------------------------------------------------------------------
      REAL      BLC(2), TRC(2), BOX(4,50)
      INTEGER   PLBUF(256), IERR, NBOX
C
      REAL      BBLC(2), BTRC(2)
      INTEGER   I, NPL
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (NBOX.GT.50) NBOX = 50
      NPL = 0
      DO 10 I = 1,NBOX
         IF ((BOX(3,I).LT.BLC(1)) .AND. (BOX(4,I).LT.BLC(2))) GO TO 10
         IF ((BOX(1,I).GT.TRC(1)) .AND. (BOX(2,I).GT.TRC(2))) GO TO 10
C                                       Bottom
         BBLC(1) = BOX(1,I)
         BBLC(2) = BOX(2,I)
         BTRC(1) = BOX(3,I)
         BTRC(2) = BOX(2,I)
         CALL LINEPL (BLC, TRC, BBLC, BTRC, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 20
C                                       Left
         BTRC(1) = BOX(1,I)
         BTRC(2) = BOX(4,I)
         CALL LINEPL (BLC, TRC, BBLC, BTRC, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 20
C                                       Top
         BBLC(1) = BOX(3,I)
         BBLC(2) = BOX(4,I)
         CALL LINEPL (BLC, TRC, BTRC, BBLC, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 20
C                                       Right
         BTRC(1) = BOX(3,I)
         BTRC(2) = BOX(2,I)
         CALL LINEPL (BLC, TRC, BBLC, BTRC, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 20
         NPL = NPL + 1
 10      CONTINUE
C
 20   WRITE (MSGTXT,1020) NPL
      IF (NPL.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('BOXPL: Successfully plotted ', I3, ' boxes')
      END
      SUBROUTINE ALIGN (IRET)
C-----------------------------------------------------------------------
C   ALIGN checks the alignment of the images
C   Output:
C      IRET   I   Error code: o okay, else die
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   NXX, I, J, IROUND, IPT, IP, LP
      REAL      X
      LOGICAL   REDUCE
      CHARACTER CHTMP*8, CHTMP1*8
      DOUBLE PRECISION DX
      INCLUDE 'GREYS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       basic window setting
      CALL RCOPY (7, XBLC, BLC(1,1))
      CALL RCOPY (7, XTRC, TRC(1,1))
      CALL WINDOW (PCATI(KIDIM,1), PCATI(KINAX,1), BLC(1,1),
     *   TRC(1,1), IRET)
      IF (IRET.NE.0) GO TO 999
      CALL RCOPY (5, BLC(3,1), TRC(3,1))
      CALL RCOPY (5, BLC(3,1), TRC(3,2))
      CALL RCOPY (5, BLC(3,1), TRC(3,3))
      CALL RCOPY (5, BLC(3,1), TRC(3,4))
      CALL RCOPY (5, BLC(3,1), BLC(3,2))
      CALL RCOPY (5, BLC(3,1), BLC(3,3))
      CALL RCOPY (5, BLC(3,1), BLC(3,4))
C                                       check
C                                       alignment
      DO 40 IP = 2,4
         IF (((IP.EQ.2) .AND. (DOCONT)) .OR. ((IP.GT.2) .AND.
     *      (INCOLR.GT.0.0))) THEN
C                                       Set Map X,Y corners
            DO 25 I = 1,2
               J = KRCRP + I - 1
               BLC(I,IP) = PCATR(J,IP) - PCATR(J,1) + BLC(I,1)
               TRC(I,IP) = PCATR(J,IP) - PCATR(J,1) + TRC(I,1)
               IF (DOGRID.LT.-1.5) BLC(I,IP) = BLC(I,1)
               IF (DOGRID.LT.-1.5) TRC(I,IP) = TRC(I,1)
               J = I - 1
               IF (DOGRID.GT.-0.1) THEN
                  DX = PCATD(KDCRV+J,1) + (BLC(I,1) - PCATR(KRCRP+J,1))
     *               * PCATR(KRCIC+J,1)
                  IF (PCATR(KRCIC+J,IP).EQ.0.0) GO TO 45
                  X = (DX - PCATD(KDCRV+J,IP)) / PCATR(KRCIC+J,IP) +
     *               PCATR(KRCRP+J,IP)
                  BLC(I,IP) = IROUND (X)
                  IF ((DOGRID.GE.0.1) .AND. (ABS(X-BLC(I,IP)).GT.0.2))
     *               GO TO 45
                  TRC(I,IP) = BLC(I,IP) + TRC(I,1) - BLC(I,1)
                  END IF
C                                       smaller subimage needed?
               IF (BLC(I,IP).LT.1.0) THEN
                  DO 10 LP = 1,IP
                     BLC(I,LP) = BLC(I,LP) + 1.0 - BLC(I,IP)
 10                  CONTINUE
                  REDUCE = .TRUE.
                  END IF
               IF (TRC(I,IP).GT.PCATI(KINAX+J,IP)) THEN
                  DO 15 LP = 1,IP
                     TRC(I,LP) = TRC(I,LP) + PCATI(KINAX+J,LP) -
     *                  TRC(I,IP)
 15                  CONTINUE
                  REDUCE = .TRUE.
                  END IF
               DO 20 LP = 1,IP
                  IF ((LP.NE.2) .OR. (DOCONT)) THEN
                     IF (BLC(I,LP).GE.TRC(I,LP)) GO TO 45
                     END IF
 20               CONTINUE
 25            CONTINUE
C                                       Check true coincidence
            IF (DOGRID.GE.0.1) THEN
               DO 30 I = 1,NXX
                  J = I - 1
                  IPT = KHCTP + J*2
                  CALL H2CHR (8, 1, PCATH(IPT,1), CHTMP)
                  CALL H2CHR (8, 1, PCATH(IPT,IP), CHTMP1)
                  IF (CHTMP.NE.CHTMP1) GO TO 45
                  X = 0.2 * 0.2 * ABS (PCATR(KRCIC+J,1))
                  IF (ABS(PCATR(KRCIC+J,1)-PCATR(KRCIC+J,IP)).GT.X)
     *               GO TO 45
                  IF (ABS(PCATR(KRCRT+J,1)-PCATR(KRCRT+J,IP)).GT.1.)
     *               GO TO 45
 30               CONTINUE
               END IF
            END IF
C                                       get axes 3-5 okay
         CALL WINDOW (PCATI(KIDIM,IP), PCATI(KINAX,IP), BLC(1,IP),
     *      TRC(1,IP), IRET)
         IF (IRET.NE.0) GO TO 999
 40      CONTINUE
      IF (REDUCE) THEN
         MSGTXT = 'Input maps coincident on reduced subimage only'
         CALL MSGWRT (6)
         END IF
C                                       RGB AXIS
      IF (INCOLR.EQ.2.0) THEN
         BLC(3,1) = (1.0D0 - PCATD(KDCRV+2,1)) / PCATR(KRCIC+2,1) +
     *      PCATR(KRCRP+2,1)
         BLC(3,3) = (2.0D0 - PCATD(KDCRV+2,1)) / PCATR(KRCIC+2,1) +
     *      PCATR(KRCRP+2,1)
         BLC(3,4) = (3.0D0 - PCATD(KDCRV+2,1)) / PCATR(KRCIC+2,1) +
     *      PCATR(KRCRP+2,1)
         TRC(3,1) = BLC(3,1)
         TRC(3,3) = BLC(3,3)
         TRC(3,4) = BLC(3,4)
         END IF
      GO TO 999
C                                        Maps not coincident
 45   WRITE (MSGTXT,1045) I
      CALL MSGWRT (7)
      IRET = I
C
 999  RETURN
C-----------------------------------------------------------------------
 1045 FORMAT ('INPUT MAPS ARE NOT COINCIDENT: AXIS',I2)
      END
      SUBROUTINE LAYOUT (BLC, TRC, PCATR, INFILE, PLBUF, IERR)
C-----------------------------------------------------------------------
C   Plot the boxes
C   Input:
C      BLC      R(2)     BLC of image being contoured
C      TRC      R(2)     TRC of image being contoured
C      PCATR    R(256)   Image header
C      LAYOUT   C*48     In file name
C   In/out:
C      PLBUF   I(256)   i/o buffer
C   Output:
C      IERR     I        0 => OK
C-----------------------------------------------------------------------
      REAL      BLC(2), TRC(2), PCATR(256)
      INTEGER   PLBUF(256), IERR
      CHARACTER INFILE*(*)
C
      INTEGER   JTRIM, MSGSAV, INC, LUN, FIND, KBPLIM, KBP, NC, I, NP,
     *   J, IP, VLNPL(6,2), VLNUM
      CHARACTER LFILE*64, LINE*80
      DOUBLE PRECISION D, THETA, DTHETA
      REAL      R1, R2, X(5), Y(5), DX, DY, PX0, PY0, P1(2), P2(2),
     *   VLRPL(2,6,2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUN /3/
      DATA VLNPL /12,16,24,40,40,40, 20,20,40,40,40,40/
      DATA VLRPL /1.983,3.683, 3.683,5.563, 5.563,7.391, 7.391,9.144,
     *   9.144,10.87, 10.87,12.5,
     *   1.676,3.518, 3.518,5.423, 5.423,7.277, 7.277,9.081,
     *   9.081,10.808, 10.808,12.500/
C-----------------------------------------------------------------------
      IERR = 0
      IF ((PCATR(KRCIC).EQ.0.0) .OR. (PCATR(KRCIC+1).EQ.0.0)) GO TO 999
      PX0 = PCATR(KRCRP)
      PY0 = PCATR(KRCRP+1)
      DX = 1.0 / PCATR(KRCIC)
      DY = 1.0 / PCATR(KRCIC+1)
      MSGSAV = MSGSUP
      VLNUM = 0
      IF (INFILE.EQ.'VLA') VLNUM = 1
      IF (INFILE.EQ.'VLBA') VLNUM = 2
C                                       open layout file
      IF (VLNUM.LE.0) THEN
         INC = JTRIM (INFILE)
         LFILE = INFILE(:INC) // '.layout'
         MSGSUP = 32000
         CALL ZTXOPN ('QRED', LUN, FIND, LFILE, .FALSE., IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.0) THEN
            CALL ZTXCLS (LUN, FIND, IERR)
            CALL ZTXOPN ('READ', LUN, FIND, LFILE, .FALSE., IERR)
            END IF
C                                       panel layout
         IF (IERR.EQ.0) THEN
C                                       read number of cards
            CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ LAYOUT LINE 1'
               GO TO 990
               END IF
            KBPLIM = JTRIM (LINE)
            KBP = 1
            CALL GETNUM (LINE, KBPLIM, KBP, D)
            IF (D.EQ.DBLANK) GO TO 980
            NC = D
            DO 40 J = 1,NC
               CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ LAYOUT DATA LINE'
                  GO TO 990
                  END IF
               KBPLIM = JTRIM (LINE)
               KBP = 1
               CALL GETNUM (LINE, KBPLIM, KBP, D)
               IF (D.EQ.DBLANK) GO TO 980
               NP = D
               CALL GETNUM (LINE, KBPLIM, KBP, D)
               IF (D.EQ.DBLANK) GO TO 980
               R1 = D
               CALL GETNUM (LINE, KBPLIM, KBP, D)
               IF (D.EQ.DBLANK) GO TO 980
               R2 = D
               DTHETA = 360.0D0 / NP * DG2RAD
               THETA = 0.0
               DO 30 IP = 1,NP
                  X(1) = R1 * SIN (THETA)
                  X(2) = R2 * SIN (THETA)
                  X(3) = R2 * SIN (THETA+DTHETA)
                  X(4) = R1 * SIN (THETA+DTHETA)
                  X(5) = X(1)
                  Y(1) = R1 * COS (THETA)
                  Y(2) = R2 * COS (THETA)
                  Y(3) = R2 * COS (THETA+DTHETA)
                  Y(4) = R1 * COS (THETA+DTHETA)
                  Y(5) = Y(1)
                  DO 20 I = 1,4
                     P1(1) = PX0 + DX * X(I)
                     P2(1) = PX0 + DX * X(I+1)
                     P1(2) = PY0 - DY * Y(I)
                     P2(2) = PY0 - DY * Y(I+1)
                     CALL LINEPL (BLC, TRC, P1, P2, PLBUF, IERR)
 20                  CONTINUE
                  THETA = THETA + DTHETA
 30               CONTINUE
 40            CONTINUE
            CALL ZTXCLS (LUN, FIND, IERR)
            END IF
C                                       bolts file
C                                       open layout file
         LFILE = INFILE(:INC) // '.bolts'
         MSGSUP = 32000
         CALL ZTXOPN ('QRED', LUN, FIND, LFILE, .FALSE., IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.0) THEN
            CALL ZTXCLS (LUN, FIND, IERR)
            CALL ZTXOPN ('READ', LUN, FIND, LFILE, .FALSE., IERR)
            END IF
C                                       panel layout
         IF (IERR.NE.0) THEN
            IERR = 0
C                                       read number of cards
         ELSE
            CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ BOLT LINE 1'
               GO TO 990
               END IF
            KBPLIM = JTRIM (LINE)
            KBP = 1
            CALL GETNUM (LINE, KBPLIM, KBP, D)
            IF (D.EQ.DBLANK) GO TO 980
            NC = D
            CALL GETNUM (LINE, KBPLIM, KBP, D)
            IF (D.EQ.DBLANK) GO TO 980
            R2 = D
            IF (R2.LE.0.0) R2 = 0.25
            DO 140 J = 1,NC
               CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ BOLT DATA LINE'
                  GO TO 990
                  END IF
               KBPLIM = JTRIM (LINE)
               KBP = 1
               CALL GETNUM (LINE, KBPLIM, KBP, D)
               IF (D.EQ.DBLANK) GO TO 980
               R1 = D
               CALL GETNUM (LINE, KBPLIM, KBP, D)
               IF (D.EQ.DBLANK) GO TO 980
               NP = D
               CALL GETNUM (LINE, KBPLIM, KBP, D)
               IF (D.EQ.DBLANK) GO TO 980
               THETA = D * DG2RAD
               DTHETA = 360.0D0 / NP * DG2RAD
               DO 130 IP = 1,NP
                  X(1) = R1 * SIN (THETA)
                  Y(1) = R1 * COS (THETA)
                  X(2) = R2 * SIN (THETA + PI/4.0D0)
                  Y(2) = R2 * COS (THETA + PI/4.0D0)
                  P1(1) = PX0 + DX * (X(1) + X(2))
                  P1(2) = PY0 + DY * (Y(1) + Y(2))
                  P2(1) = PX0 + DX * (X(1) - X(2))
                  P2(2) = PY0 + DY * (Y(1) - Y(2))
                  CALL LINEPL (BLC, TRC, P1, P2, PLBUF, IERR)
                  P1(1) = PX0 + DX * (X(1) - Y(2))
                  P1(2) = PY0 + DY * (Y(1) + X(2))
                  P2(1) = PX0 + DX * (X(1) + Y(2))
                  P2(2) = PY0 + DY * (Y(1) - X(2))
                  CALL LINEPL (BLC, TRC, P1, P2, PLBUF, IERR)
                  THETA = THETA + DTHETA
 130              CONTINUE
 140           CONTINUE
            CALL ZTXCLS (LUN, FIND, IERR)
            END IF
C                                       known arrays
      ELSE
         NC = 6
         DO 240 J = 1,NC
            NP = VLNPL(J,VLNUM)
            THETA = 0.0
            DTHETA = 360.0D0 / NP * DG2RAD
            R1 = VLRPL(1,J,VLNUM)
            R2 = VLRPL(2,J,VLNUM)
            DO 230 IP = 1,NP
               X(1) = R1 * SIN (THETA)
               X(2) = R2 * SIN (THETA)
               X(3) = R2 * SIN (THETA+DTHETA)
               X(4) = R1 * SIN (THETA+DTHETA)
               X(5) = X(1)
               Y(1) = R1 * COS (THETA)
               Y(2) = R2 * COS (THETA)
               Y(3) = R2 * COS (THETA+DTHETA)
               Y(4) = R1 * COS (THETA+DTHETA)
               Y(5) = Y(1)
               DO 220 I = 1,4
                  P1(1) = PX0 + DX * X(I)
                  P2(1) = PX0 + DX * X(I+1)
                  P1(2) = PY0 - DY * Y(I)
                  P2(2) = PY0 - DY * Y(I+1)
                  CALL LINEPL (BLC, TRC, P1, P2, PLBUF, IERR)
 220              CONTINUE
               THETA = THETA + DTHETA
 230           CONTINUE
 240        CONTINUE
         END IF
      GO TO 999
C                                       bad value
 980  IERR = 2
      MSGTXT = 'ILLEGAL VALUE READ'
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LAYOUT ERROR',I3,' ON ',A)
      END
