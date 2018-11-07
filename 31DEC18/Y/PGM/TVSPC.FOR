LOCAL INCLUDE 'PTVSPC.INC'
      INTEGER   NOPT, MAXCHR
      PARAMETER (NOPT = 13)
      PARAMETER (MAXCHR = 19)
LOCAL END
LOCAL INCLUDE 'TVSPC.INC'
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER NAMIN*12, CLSIN*6, NAMIN2*12, CLSIN2*6, FUNTYP*2,
     *   SAMTYP*4, NAMIN3*12, CLSIN3*6, NAMIN4*12, CLSIN4*6
      HOLLERITH XNAMIN(3), XCLSIN(2), XNAMI2(3), XCLSI2(2), XNAMI3(3),
     *   XCLSI3(2), XFUNC,XSAMP, XNAMI4(3), XCLSI4(2)
      REAL      SEQIN, DSKIN, SEQIN2, DSKIN2, SEQIN3, DSKIN3, SEQIN4,
     *   DSKIN4, TBLC(7), TTRC(7), XTVCH, RANGE(2), APARM(10)
      INTEGER   CATBLK(256), CATBL1(256), CATBL2(256), CATBL3(256),
     *   CATBL4(256), CATBLS(256), CATBLP(256), CATBS2(256)
      REAL      CATR(256), CATR1(256), CATR2(256), CATR3(256),
     *   CATR4(256), CATRS(256), PRANGE(2), CRANGE(2)
      DOUBLE PRECISION CATD(128), CATD1(128), CATD2(128), CATD3(128),
     *   CATD4(128), CATDS(128)
      HOLLERITH CATH(256), CATH1(256), CATH2(256), CATH3(256),
     *   CATH4(256), CATHS(256)
      REAL      BUFF1(MABFSS), SPECTR(MAXIMG), SFUNC(128), GAUSS(15,2),
     *   GRMS, SPECT2(MAXIMG), SLDATA(MAXIMG), SLPOS(MAXIMG), XCEN
      INTEGER   LUN1, LUN2, LUN3, IND1, IND2, IND3, VOL1, VOL2, VOL3,
     *   SLOT1, SLOT2, SLOT3, SEQ1, SEQ2, SEQ3, IUSER, TVCH, JBUFSZ,
     *   CURWIN(4,2), LWINTV(4), GRMENU, GRSPC1, GRSPC2, GRMEBG, GRLABL,
     *   GRMODL, GRESID, GRBLAC, IBUFF(MAXIMG), JBUFF(MAXIMG),
     *   IWINTV(4), SWINTV(4), LASTYZ(3), SMTYPE, SMWID, NGAUSS, ITTER,
     *   SCRTCH(MAXIMG), BCHAN, ECHAN, JJC, IDROP(2), LUN4, IND4, VOL4,
     *   SLOT4, SEQ4, JWINTV(4), CURPLN, CURCHN, SPECOK, DOLABL,
     *   SWIN1(4), SWIN2(4)
      LOGICAL   MENUOK, TWOCUB, TWOIMG
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      EQUIVALENCE (CATBL1, CATR1, CATH1, CATD1)
      EQUIVALENCE (CATBL2, CATR2, CATH2, CATD2)
      EQUIVALENCE (CATBL3, CATR3, CATH3, CATD3)
      EQUIVALENCE (CATBL4, CATR4, CATH4, CATD4)
      EQUIVALENCE (CATBLS, CATRS, CATHS, CATDS)
      COMMON /MAPHDR/ CATD, CATD1, CATD2, CATD3, CATD4, CATDS, CATBLP,
     *   CATBS2
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, TBLC, TTRC, XTVCH,
     *   RANGE, XFUNC, XNAMI2, XCLSI2, SEQIN2, DSKIN2, APARM, XSAMP,
     *   XNAMI3, XCLSI3, SEQIN3, DSKIN3, XNAMI4, XCLSI4, SEQIN4, DSKIN4
      COMMON /CHARPM/ NAMIN, CLSIN, NAMIN2, CLSIN2, NAMIN3, CLSIN3,
     *   NAMIN4, CLSIN4, SAMTYP, FUNTYP
      COMMON /BUFFRS/ BUFF1, IBUFF, JBUFF
      COMMON /TVSPCP/ LUN1, LUN2, LUN3, IND1, IND2, IND3, VOL1, VOL2,
     *   VOL3, SEQ1, SEQ2, SEQ3, SLOT1, SLOT2, SLOT3, TVCH, JBUFSZ,
     *   IUSER, CURWIN, PRANGE, LWINTV, IWINTV, SWINTV, GRMENU, GRSPC1,
     *   GRSPC2, GRMEBG, GRLABL, GRMODL, GRESID, GRBLAC, MENUOK, LASTYZ,
     *   SMTYPE,SMWID, SFUNC, NGAUSS, GRMS, ITTER, TWOCUB, TWOIMG,
     *   GAUSS, SPECTR, SPECT2, SLDATA, SLPOS, BCHAN, ECHAN, SPECOK,
     *   XCEN, JJC, IDROP, SCRTCH, LUN4, IND4, VOL4, SEQ4, SLOT4,
     *   JWINTV, CURPLN, CURCHN, CRANGE, DOLABL, SWIN1, SWIN2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
LOCAL END
      PROGRAM TVSPC
C-----------------------------------------------------------------------
C! displays spectrum under cursor from 1 or 2 cubes, incl Gaussian fit
C# TV Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2016-2017
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
C   TVSPC will display an image on the TV based on input images giving
C   separate red, green, and blue pictures.  It allows for interactive
C   scaling of the images.
C   INPUTS:   (from AIPS)
C      USERID   R      user number, 0 means use logon user number,
C                      32000 means any user can be accessed.
C      INNAME   R(3)   name of RGB/redimage.
C      INCLASS  R(2)   class of RGB/red image.
C      INSEQ    R      sequence number of RGB/red image.
C      INDISK   R      disk volume number. 0 means try all.
C      IN2NAME  R(3)   name of green image.
C      IN2CLASS R(2)   class of green image.
C      IN2SEQ   R      sequence number of green image.
C      IN2DISK  R      disk volume number. 0 means try all.
C      IN3NAME  R(3)   name of blue image.
C      IN3CLASS R(2)   class of blue image.
C      IN3SEQ   R      sequence number of blue image.
C      IN3DISK  R      disk volume number. 0 means try all.
C      TBLC      R(7)   the coordinate in the input file to become the
C                      left hand coordinate (1,1) of the contour plot.
C                      TBLC(1) is the X coordinate and TBLC(2) is the Y
C                      coordinate.  The first coordinate in the input
C                      image is (1,1).
C      TTRC      R(7)   the coordinate in the input file to become the
C                      top right hand corner of the plot.
C      DOALIGN  R      >= 0 => 2nd/3rd image must align with 1st
C      TVCHAN   R      Desired TV channel
C      PIXRANGE R(2)   the maximum and minimum values allowed for the
C                      map.  All other values will be clipped.  If
C                      IRANGE(1) .GE. IRANGE(2) then the map max and
C                      min will be used.
C      DPARM    R(10)  (1,2) pixrange of 2nd image, default = PIXRANGE
C                      (3,4) pixrange of 3rd image, default = PIXRANGE
C                      (5) # levels per R,G,B   < 8 or > 40 => 32.
C                      (6) # pixels in subimage <= 86000.  0 => 86000.
C                      (7) # levels/r,g,b during interaction <= (5)
C                      (8) > 0 => use 1-channel method on TVs capable of
C                          full RGB
C      DOOUTPUT R      > 0 => write RGB PostScript
C      OUTFILE  H(12)  Name of the output file. blank -> place in
C                      printer queue.
C      RGBGAMMA R(3)   RGB Gamma corrections
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   INPRMS, IRET, NWORDS, NX, NY, JERR, NX2, NY2, NZ2
      LONGINT   PIMAGE, PIMAG2
      REAL      IMAGE(2), IMAGE2(2)
      CHARACTER PRGNAM*6
      INCLUDE 'TVSPC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       init the task
      INPRMS = 57
      PRGNAM = 'TVSPC'
      CALL TVSPIN (PRGNAM, INPRMS, IRET)
      IF (IRET.NE.0) GO TO 910
C                                       allocate image
      NX = TTRC(1) - TBLC(1) + 1
      NY = TTRC(2) - TBLC(2) + 1
      NWORDS = (NX * NY - 1) / 1024 + 3
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, IMAGE, PIMAGE, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GETTING MEMORY FOR IMAGE'
         GO TO 900
         END IF
C                                       read image
      MSGTXT = 'read image 1 into memory'
      CALL MSGWRT (2)
      CALL TVSPIM (NX, NY, IMAGE(1+PIMAGE), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING IMAGE'
         GO TO 900
         END IF
C                                       allocate image
      IF (TWOIMG) THEN
         NX2 = CATBL4(KINAX)
         NY2 = CATBL4(KINAX+1)
         NZ2 = CATBL4(KINAX+2)
         NWORDS = (NX2 * NY2 * NZ2 - 1) / 1024 + 3
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, IMAGE2, PIMAG2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'GETTING MEMORY FOR IMAGE 2'
            GO TO 900
            END IF
         MSGTXT = 'read image cube 2 into memory'
         CALL MSGWRT (2)
         CALL TVSPI2 (NX2, NY2, NZ2, IMAGE2(1+PIMAG2), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING IMAGE'
            GO TO 900
            END IF
         END IF
C                                       do the function
      CALL TVSPDO (NX, NY, IMAGE(1+PIMAGE), NX2, NY2, NZ2,
     *   IMAGE2(1+PIMAG2), IRET)
      GO TO 910
C                                       error message
 900  IF (IRET.NE.0) CALL MSGWRT (8)
C                                       close TV
 910  IF ((TVIND.GT.0) .AND. (TVIND2.GT.0)) THEN
         CALL TVCLOS (SCRTCH, JERR)
         END IF
C                                       close files
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' ON ',A)
      END
      SUBROUTINE TVSPIN (PRGNAM, INPRMS, IRET)
C-----------------------------------------------------------------------
C   Routine to get parameters for TVSPC
C   Inputs:
C      PRGNAM   C*6    Program name
C      INPRMS   I      Number of data parameters from AIPS
C   Outputs:
C       SCRTCH  I(*)   Scratch buffer
C       IRET    I      Return code 0=> OK, else just go to DIE
C   Task parameters are returned in common /INPARM/
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   INPRMS, IRET
C
      INCLUDE 'PTVSPC.INC'
C
      INTEGER   IERR, IROUND, I, II, TVCODE, DEPTH(5)
      CHARACTER FTYPE*2, SUBR*8
      REAL      OFFS, SLOPE, RTEMP
      DOUBLE PRECISION DTEMP
      INCLUDE 'TVSPC.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      NSCR = 0
      NCFILE = 0
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      IRET = 0
      JBUFSZ = 2 * MABFSS
C                                       Get input values from AIPS.
      CALL GTPARM (PRGNAM, INPRMS, RQUICK, XNAMIN, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR, 'OBTAINING INPUT PARAMETERS'
            CALL MSGWRT (8)
      ELSE
         RQUICK = .FALSE.
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       AIPS Holleriths ->
C                                       characters
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      CALL H2CHR (12, 1, XNAMI2, NAMIN2)
      CALL H2CHR (6, 1, XCLSI2, CLSIN2)
      CALL H2CHR (12, 1, XNAMI3, NAMIN3)
      CALL H2CHR (6, 1, XCLSI3, CLSIN3)
      CALL H2CHR (12, 1, XNAMI4, NAMIN4)
      CALL H2CHR (6, 1, XCLSI4, CLSIN4)
      CALL H2CHR (2, 1, XFUNC, FUNTYP)
      CALL H2CHR (4, 1, XSAMP, SAMTYP)
      CALL TYPESM (SAMTYP, SMTYPE)
      CURPLN = 0
      CURCHN = 0
C
      LUN1 = 16
      LUN2 = 17
      LUN3 = 18
      LUN4 = 19
      SEQ1 = IROUND (SEQIN)
      SEQ2 = IROUND (SEQIN2)
      SEQ3 = IROUND (SEQIN3)
      SEQ4 = IROUND (SEQIN4)
      VOL1 = IROUND (DSKIN)
      VOL2 = IROUND (DSKIN2)
      VOL3 = IROUND (DSKIN3)
      VOL4 = IROUND (DSKIN4)
      IUSER = NLUSER
      TVCH = IROUND (XTVCH)
      IF (TVCH.LE.0) TVCH = 1
C                                       open 2nd spectrum image
      FTYPE = 'MA'
      TWOCUB = NAMIN3.NE.' '
      IF (TWOCUB) THEN
         CALL MAPOPN ('READ', VOL3, NAMIN3, CLSIN3, SEQ3, FTYPE, IUSER,
     *      LUN3, IND3, SLOT3, CATBLK, SCRTCH, IERR)
         IF (IERR.GE.2) THEN
            WRITE (MSGTXT,1000) IERR, 'OPENING IMAGE 3'
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = VOL3
         FCNO(NCFILE) = SLOT3
         FRW(NCFILE) = 0
         CALL COPY (256, CATBLK, CATBL3)
         END IF
      TWOIMG = NAMIN4.NE.' '
      IF (TWOIMG) THEN
         CALL MAPOPN ('READ', VOL4, NAMIN4, CLSIN4, SEQ4, FTYPE, IUSER,
     *      LUN4, IND4, SLOT4, CATBLK, SCRTCH, IERR)
         IF (IERR.GE.2) THEN
            WRITE (MSGTXT,1000) IERR, 'OPENING IMAGE 4'
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = VOL4
         FCNO(NCFILE) = SLOT4
         FRW(NCFILE) = 0
         CALL COPY (256, CATBLK, CATBL4)
         CATBLK(IIVOL) = VOL4
         CATBLK(IICNO) = SLOT4
         CRANGE(1) = CATR4(KRDMN)
         CRANGE(2) = CATR4(KRDMX)
         CATR(IRRAN) = CRANGE(1)
         CATR(IRRAN+1) = CRANGE(2)
         CALL CHR2H (2, FUNTYP, 1, CATH(IITRA))
         CALL COPY (256, CATBLK, CATBLP)
         END IF
C                                       open spectrum image
      CALL MAPOPN ('WRIT', VOL2, NAMIN2, CLSIN2, SEQ2, FTYPE, IUSER,
     *   LUN2, IND2, SLOT2, CATBLK, SCRTCH, IERR)
      IF (IERR.GE.2) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING IMAGE 2'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = VOL2
      FCNO(NCFILE) = SLOT2
      FRW(NCFILE) = 1
      CALL COPY (256, CATBLK, CATBL2)
      CALL FILL (5, 1, DEPTH)
      BCHAN = 1
      ECHAN = CATBL2(KINAX)
C                                       cheat on header to make easy
      DTEMP = CATD(KDCRV)
      CATD(KDCRV) = CATD(KDCRV+1)
      CATD(KDCRV+1) = CATD(KDCRV+2)
      CATD(KDCRV+2) = DTEMP
      RTEMP = CATD(KRCRP)
      CATR(KRCRP) = CATR(KRCRP+1)
      CATR(KRCRP+1) = CATR(KRCRP+2)
      CATR(KRCRP+2) = RTEMP
      RTEMP = CATR(KRCIC)
      CATR(KRCIC) = CATR(KRCIC+1)
      CATR(KRCIC+1) = CATR(KRCIC+2)
      CATR(KRCIC+2) = RTEMP
      I = CATBLK(KINAX)
      CATBLK(KINAX) = CATBLK(KINAX+1)
      CATBLK(KINAX+1) = CATBLK(KINAX+2)
      CATBLK(KINAX+2) = I
      RTEMP = CATR(KHCTP)
      CATR(KHCTP) = CATR(KHCTP+2)
      CATR(KHCTP+2) = CATR(KHCTP+4)
      CATR(KHCTP+4) = RTEMP
      RTEMP = CATR(KHCTP+1)
      CATR(KHCTP+1) = CATR(KHCTP+3)
      CATR(KHCTP+3) = CATR(KHCTP+5)
      CATR(KHCTP+5) = RTEMP
      LOCNUM = 2
      CALL SETLOC (DEPTH, .FALSE.)
C                                       Open TV image
      CALL MAPOPN ('READ', VOL1, NAMIN, CLSIN, SEQ1, FTYPE, IUSER,
     *   LUN1, IND1, SLOT1, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN INPUT IMAGE 1'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = VOL1
      FCNO(NCFILE) = SLOT1
      FRW(NCFILE) = 0
      CALL COPY (256, CATBLK, CATBL1)
C                                       Check windows
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), TBLC, TTRC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'SETTING TV WINDOW'
         GO TO 990
         END IF
      CALL RCOPY (5, TBLC(3), TTRC(3))
      DO 10 I = 1,5
         DEPTH(I) = TBLC(I+2) + 0.1
 10      CONTINUE
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
C                                       pix ranges
      CALL RNGSET (RANGE, CATR(KRDMX), CATR(KRDMN), PRANGE)
C                                       open the TV
      IRET = 8
      CALL TVOPEN (SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING TV'
         GO TO 990
         END IF
      CALL YHOLD ('ONNN', IERR)
      SUBR = 'YCINIT'
      CALL YCINIT (TVCH, SCRTCH)
      IF (IERR.NE.0) GO TO 980
      SUBR = 'YINIT'
      CALL YINIT (SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 980
      II = NGRAY + NGRAPH
      SUBR = 'YSLECT'
      DO 60 I = 1,II
         IF (I.EQ.TVCH) THEN
            CALL YSLECT ('ONNN', I, 0, SCRTCH, IERR)
         ELSE
            CALL YSLECT ('OFFF', I, 0, SCRTCH, IERR)
            END IF
         IF (IERR.NE.0) GO TO 980
 60      CONTINUE
C                                       Init OFM
      II = OFMINP + 1
      CALL RFILL (II, 0.0, BUFF1)
      II = LUTOUT + 1
      OFFS = 1.0 / REAL(OFMOUT)
      SLOPE = OFFS * (OFMOUT + 1.0) / REAL (II)
      DO 65 I = 2,II
         BUFF1(I) = I * SLOPE - OFFS
 65      CONTINUE
      BUFF1(1) = 0.0
      SUBR = 'YOFM'
      CALL YOFM ('WRIT', 7, .TRUE., BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       LUTs
      II = MAXINT + 1
      SLOPE = (LUTOUT + 1.0) / (MAXINT+1.0)
      DO 70 I = 1,II
         SCRTCH(I+1) = I * SLOPE - 0.5
 70      CONTINUE
      SCRTCH(1) = 0
      TVCODE = 2 ** (TVCH - 1)
      SUBR = 'YLUT'
      CALL YLUT ('WRIT', TVCODE, 7, .TRUE., SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 980
      CURWIN(1,1) = TBLC(1) + 0.1
      CURWIN(2,1) = TBLC(2) + 0.1
      CURWIN(3,1) = TTRC(1) + 0.1
      CURWIN(4,1) = TTRC(2) + 0.1
      CURWIN(1,2) = 1
      CURWIN(2,2) = 1
      CURWIN(3,2) = CATBL4(KINAX)
      CURWIN(4,2) = CATBL4(KINAX+1)
      CATBLK(IIVOL) = VOL1
      CATBLK(IICNO) = SLOT1
      CATR(IRRAN) = PRANGE(1)
      CATR(IRRAN+1) = PRANGE(2)
      CALL CHR2H (2, FUNTYP, 1, CATH(IITRA))
      DO 80 I = 3,7
         CATBLK(IIDEP+I-3) = IROUND (TBLC(I))
80       CONTINUE
      IRET = 0
      GO TO 999
C                                       TV error
 980  WRITE (MSGTXT,1000) IERR, 'TV FUNCTION ' // SUBR
C                                       all errors
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVSPIN: ERROR',I3,' ON ',A)
      END
      SUBROUTINE TVSPIM (NX, NY, IMAGE, IRET)
C-----------------------------------------------------------------------
C   TVSPIM reads in the image to core
C   Inputs
C      NX      I      Number x pixels
C      NY      I      Number y pixels
C   Outputs
C      IMAGE   R(*)   Image
C      IRET    I      error
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IRET
      REAL      IMAGE(NX,*)
C
      INCLUDE 'TVSPC.INC'
      INTEGER   J, OFFS, WIN(4), BIND
C-----------------------------------------------------------------------
      CALL COMOFF (CATBL1(KIDIM), CATBL1(KINAX), CATBLK(IIDEP), OFFS,
     *   IRET)
      OFFS = OFFS + 1
      WIN(1) = TBLC(1) + 0.1
      WIN(2) = TBLC(2) + 0.1
      WIN(3) = TTRC(1) + 0.1
      WIN(4) = TTRC(2) + 0.1
      CALL MINIT ('READ', LUN1, IND1, NX, NY, WIN, BUFF1, JBUFSZ, OFFS,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT IO TO TV IMAGE'
         GO TO 990
         END IF
      DO 20 J = 1,NY
         CALL MDISK ('READ', LUN1, IND1, BUFF1, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ TV IMAGE ROW'
            GO TO 990
            END IF
         CALL RCOPY (NX, BUFF1(BIND), IMAGE(1,J))
 20      CONTINUE
      CALL ZCLOSE (LUN1, IND1, J)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVSPIM: ERROR',I4,' ON ',A)
      END
      SUBROUTINE TVSPI2 (NX, NY, NZ, IMAGE, IRET)
C-----------------------------------------------------------------------
C   TVSPI2 reads in the 4th image cube to core
C   Inputs
C      NX      I      Number x pixels
C      NY      I      Number y pixels
C      NZ      I      Number Z pixels
C   Outputs
C      IMAGE   R(*)   Image
C      IRET    I      error
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, IRET
      REAL      IMAGE(NX,NY,*)
C
      INCLUDE 'TVSPC.INC'
      INTEGER   J, K, OFFS, WIN(4), BIND, DEPTH(5)
      DATA DEPTH /5*1/
C-----------------------------------------------------------------------
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
      DO 30 K = 1,NZ
         DEPTH(1) = K
         CALL COMOFF (CATBL4(KIDIM), CATBL4(KINAX), DEPTH, OFFS,
     *      IRET)
         OFFS = OFFS + 1
         CALL MINIT ('READ', LUN4, IND4, NX, NY, WIN, BUFF1, JBUFSZ,
     *      OFFS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT IO TO TV IMAGE CUBE'
            GO TO 990
            END IF
         DO 20 J = 1,NY
            CALL MDISK ('READ', LUN4, IND4, BUFF1, BIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ TV IMAGE CUBE ROW'
               GO TO 990
               END IF
            CALL RCOPY (NX, BUFF1(BIND), IMAGE(1,J,K))
 20         CONTINUE
 30      CONTINUE
      CALL ZCLOSE (LUN4, IND4, J)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVSPI2: ERROR',I4,' ON ',A)
      END
      SUBROUTINE TVSPDO (NX, NY, IMAGE, NX2, NY2, NZ2, IMAGE2, IRET)
C-----------------------------------------------------------------------
C   TVSPDO does the interaction control
C   Inputs
C      NX       I      Number x pixels
C      NY       I      Number y pixels
C      NX2      I      Number x pixels image 2
C      NY2      I      Number y pixels image 2
C      NZ2      I      Number z pixels image 2
C   Outputs
C      IMAGE    R(*)   Image
C      IMAGE2   R(*)   Image plane from #2
C      IRET     I      error
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NX2, NY2, NZ2, IRET
      REAL      IMAGE(NX,*), IMAGE2(NX2,NY2,*)
C
      INCLUDE 'TVSPC.INC'
      INTEGER   NOPT
      PARAMETER (NOPT = 24)
C
      INTEGER   I, II, IBUT, GRCS(2), CHS, NTITLE, TOPSEP, SIDSEP, MOPT,
     *   TTY(2), TIMLIM, ICOLOR, MBOX, NBO, NLEVS, TVCODE, CATSAV(256)
      LOGICAL   DOIT, LEAVE(NOPT), VERBOS, FIRST
      REAL      OFFS, BLCO(7), TRCO(7), RPOS(2), SLOPE, PPOS(2)
      DOUBLE PRECISION DTEMP(10)
      CHARACTER OPTION(NOPT)*20, SUBR*6, ISHELP*8, TITLE*8, MSGBUF*72,
     *   CTEMP*4
      DATA OPTION /'OFF TRANS', 'OFF PSEUDO', 'TVTRANSF', 'TVPSEUDO',
     *   'TVFLAME', 'SET APARM', 'LABEL IMAGES?', ' ', 'SET WINDOW',
     *   'RESET WINDOW', 'SET CHANNELS', 'RESET CHANNELS',
     *   'CURVALUE', 'PLOT SPECTRA', 'FIT SPECTRUM', 'SAVE SPECTRUM',
     *   ' ', 'LOAD PLANE', 'PLOT PL SPECTRA', 'SET PL WINDOW',
     *   'RESET PL WIN', 'SET PL RANGE', '  ', 'EXIT'/
      DATA LEAVE /NOPT*.FALSE./
C-----------------------------------------------------------------------
C                                        Initialize
      TTY(1) = 5
      TTY(2) = 0
      TITLE = ' '
      NTITLE = 0
      MENUOK = .FALSE.
      DOLABL = 0
      GRMENU = 1 + NGRAY
      GRSPC1 = 2 + NGRAY
      GRSPC2 = 3 + NGRAY
      GRMEBG = 4 + NGRAY
      GRLABL = 5 + NGRAY
      GRMODL = 6 + NGRAY
      GRESID = 7 + NGRAY
      GRBLAC = 8 + NGRAY
      TVCODE = 2 ** (TVCH - 1)
      SPECOK = 0
C                                       start by forcing a load
      DOIT = .TRUE.
      VERBOS = .FALSE.
      FIRST = .TRUE.
C                                       init smoothing function
      CALL SMFUNC
C                                       Menu interaction loop point
C                                       check window
 100  CALL YWINDO ('READ', WINDTV, IRET)
      SUBR = 'YWINDO'
      IF (IRET.NE.0) GO TO 990
      IF (WINDTV(1).NE.LWINTV(1)) MENUOK = .FALSE.
      IF (WINDTV(2).NE.LWINTV(2)) MENUOK = .FALSE.
      IF (WINDTV(3).NE.LWINTV(3)) MENUOK = .FALSE.
      IF (WINDTV(4).NE.LWINTV(4)) MENUOK = .FALSE.
      CALL COPY (4, WINDTV, LWINTV(1))
      CALL RCOPY (4, LWINTV, IWINTV)
      CALL RCOPY (4, LWINTV, SWINTV)
      IF ((APARM(5).LT.0.1) .OR. (APARM(5).GT.0.9)) APARM(5) = 0.5
      IF (APARM(4).LT.0.0) THEN
         SWINTV(4) = SWINTV(2) + APARM(5) * (SWINTV(4) - SWINTV(2))
         IWINTV(2) = SWINTV(4) + 1
         IF (TWOIMG) THEN
            CALL COPY (4, IWINTV, JWINTV)
            IWINTV(3) = IWINTV(1) + 0.5 * (IWINTV(3) - IWINTV(1))
            JWINTV(1) = IWINTV(3) + 1
            END IF
      ELSE IF (APARM(4).GT.0.0) THEN
         IWINTV(3) = IWINTV(1) + (1.0-APARM(5)) * (IWINTV(3)-IWINTV(1))
         SWINTV(1) = IWINTV(3) + 1
         IF (TWOIMG) THEN
            CALL COPY (4, IWINTV, JWINTV)
            IWINTV(4) = IWINTV(2) + 0.5 * (IWINTV(4) - IWINTV(2))
            JWINTV(2) = IWINTV(4) + 1
            END IF
      ELSE IF (TWOIMG) THEN
         CALL COPY (4, IWINTV, JWINTV)
         IWINTV(3) = IWINTV(1) + 0.5 * (IWINTV(3) - IWINTV(1))
         JWINTV(1) = IWINTV(3) + 1
         END IF
      IF (FIRST) GO TO 200
C                                       build menu
      GRCS(1) = GRMENU - NGRAY
      IF (MENUOK) GRCS(1) = -GRCS(1)
      GRCS(2) = GRMEBG - NGRAY
      ISHELP = TSKNAM
      TOPSEP = 3 * CSIZTV(2) + 1
      SIDSEP = 5
      TIMLIM = 0
      MOPT = NOPT
      IF (.NOT.TWOIMG) THEN
         OPTION(NOPT-6) = OPTION(NOPT)
         MOPT = MOPT - 6
         END IF
      CALL TVMENU (0, 1, MOPT, GRCS, TOPSEP, SIDSEP, ISHELP, OPTION,
     *   TIMLIM, LEAVE, NTITLE, TITLE, CHS, IBUT, SCRTCH, IRET)
      SUBR = 'TVMENU'
      IF (IRET.NE.0) GO TO 990
      MENUOK = .TRUE.
C                                       Something to do
C                                       off B&W transfer
      DOIT = .FALSE.
      IF (OPTION(CHS).EQ.'OFF TRANS') THEN
         II = MAXINT + 1
         SLOPE = (LUTOUT + 1.0) / (MAXINT+1.0)
         DO 120 I = 1,II
            SCRTCH(I+1) = I * SLOPE - 0.5
 120        CONTINUE
         SCRTCH(1) = 0
         SUBR = 'YLUT'
         CALL YLUT ('WRIT', TVCODE, 7, .TRUE., SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       off coloring
      ELSE IF (OPTION(CHS).EQ.'OFF PSEUDO') THEN
         II = OFMINP + 1
         CALL RFILL (II, 0.0, BUFF1)
         II = LUTOUT + 1
         OFFS = 1.0 / REAL(OFMOUT)
         SLOPE = OFFS * (OFMOUT + 1.0) / REAL (II)
         DO 125 I = 2,II
            BUFF1(I) = I * SLOPE - OFFS
 125        CONTINUE
         BUFF1(1) = 0.0
         SUBR = 'YOFM'
         CALL YOFM ('WRIT', 7, .TRUE., BUFF1, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       B&W transfer function
      ELSE IF (OPTION(CHS).EQ.'TVTRANSF') THEN
         I = 1
         ICOLOR = 7
         SUBR = 'IENHNS'
         CALL IENHNS (TVCODE, ICOLOR, I, RPOS, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       pseudo coloring
      ELSE IF (OPTION(CHS).EQ.'TVPSEUDO') THEN
         NLEVS = LUTOUT + 1
         SUBR = 'TVPSUD'
         CALL TVPSUD (NLEVS, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       flame coloring
      ELSE IF (OPTION(CHS).EQ.'TVFLAME') THEN
         NLEVS = LUTOUT + 1
         SUBR = 'TVFLAM'
         CALL TVFLAM (NLEVS, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       reset APARM values
      ELSE IF (OPTION(CHS).EQ.'SET APARM') THEN
C                                       Open terminal
         IF (TTY(2).LE.0) THEN
            TTY(2) = 0
            CALL ZOPEN (TTY(1), TTY(2), 1, MSGBUF, .FALSE., .TRUE.,
     *         .TRUE., IRET)
            IF (IRET.NE.0) THEN
               TTY(2) = 0
               WRITE (MSGTXT,1250) IRET
               CALL MSGWRT (6)
               GO TO 200
               END IF
            END IF
         MSGBUF = 'Spectral plot range min and max'
         CALL INQFLT (TTY, MSGBUF, 2, DTEMP, IRET)
         IF (IRET.LT.0) GO TO 200
         SUBR = 'INQFLT'
         IF (IRET.GT.0) GO TO 990
         APARM(1) = DTEMP(1)
         APARM(2) = DTEMP(2)
         MSGBUF = 'Spectral averaging radius in YZ pixels'
         CALL INQFLT (TTY, MSGBUF, 1, DTEMP, IRET)
         IF (IRET.LT.0) GO TO 200
         IF (IRET.GT.0) GO TO 990
         APARM(3) = DTEMP(1)
         MSGBUF = 'Split screen: < 0 vertical, > 0 horizontal, 0 none'
         CALL INQFLT (TTY, MSGBUF, 1, DTEMP, IRET)
         IF (IRET.LT.0) GO TO 200
         IF (IRET.GT.0) GO TO 990
         IF ((APARM(4).GT.0.0) .AND. (DTEMP(1).LE.0.0D0)) DOIT = .TRUE.
         IF ((APARM(4).EQ.0.0) .AND. (DTEMP(1).NE.0.0D0)) DOIT = .TRUE.
         IF ((APARM(4).LT.0.0) .AND. (DTEMP(1).GT.0.0D0)) DOIT = .TRUE.
         APARM(4) = DTEMP(1)
         IF (APARM(4).NE.0.0) THEN
            MSGBUF = 'Fraction of split screen for spectra'
            CALL INQFLT (TTY, MSGBUF, 1, DTEMP, IRET)
            IF (IRET.LT.0) GO TO 200
            IF (IRET.GT.0) GO TO 990
            IF ((DTEMP(1).LT.0.1D0) .OR. (DTEMP(1).GT.0.9D0))
     *         DTEMP(1) = 0.5D0
            IF (ABS(APARM(5)-DTEMP(1)).GT.0.03) DOIT = .TRUE.
            APARM(5) = DTEMP(1)
            END IF
         IF (TWOCUB) THEN
            MSGBUF = 'Split spectra: <= 0 no, > 0 yes'
            CALL INQFLT (TTY, MSGBUF, 1, DTEMP, IRET)
            IF (IRET.LT.0) GO TO 200
            IF (IRET.GT.0) GO TO 990
            APARM(6) = DTEMP(1)
            END IF
         MSGBUF = 'Label spectra in channels: > 0 -> yes'
         CALL INQFLT (TTY, MSGBUF, 1, DTEMP, IRET)
         IF (IRET.LT.0) GO TO 200
         IF (IRET.GT.0) GO TO 990
         IF ((APARM(8).GT.0.0) .AND. (DTEMP(1).LE.0.0)) DOIT = .TRUE.
         IF ((APARM(8).LE.0.0) .AND. (DTEMP(1).GT.0.0)) DOIT = .TRUE.
         APARM(8) = DTEMP(1)
         MSGBUF = 'Baseline fit order: > 0 -> yes'
         CALL INQFLT (TTY, MSGBUF, 1, DTEMP, IRET)
         IF (IRET.LT.0) GO TO 200
         IF (IRET.GT.0) GO TO 990
         APARM(9) = DTEMP(1)
         IF (DOIT) THEN
            CALL RCOPY (4, LWINTV, IWINTV)
            CALL RCOPY (4, LWINTV, SWINTV)
            IF (APARM(4).LT.0.0) THEN
               SWINTV(4) = SWINTV(2) + APARM(5) * (SWINTV(4)-SWINTV(2))
               IWINTV(2) = SWINTV(4) + 1
               IF (TWOIMG) THEN
                  CALL COPY (4, IWINTV, JWINTV)
                  IWINTV(3) = IWINTV(1) + 0.5 * (IWINTV(3) - IWINTV(1))
                  JWINTV(1) = IWINTV(3) + 1
                  END IF
            ELSE IF (APARM(4).GT.0.0) THEN
               IWINTV(3) = IWINTV(1) + (1.0-APARM(5)) *
     *            (IWINTV(3)-IWINTV(1))
               SWINTV(1) = IWINTV(3) + 1
               IF (TWOIMG) THEN
                  CALL COPY (4, IWINTV, JWINTV)
                  IWINTV(4) = IWINTV(2) + 0.5 * (IWINTV(4) - IWINTV(2))
                  JWINTV(2) = IWINTV(4) + 1
                  END IF
            ELSE IF (TWOIMG) THEN
               CALL COPY (4, IWINTV, JWINTV)
               IWINTV(3) = IWINTV(1) + 0.5 * (IWINTV(3) - IWINTV(1))
               JWINTV(1) = IWINTV(3) + 1
               END IF
            SUBR = 'YZERO'
            CALL YZERO (GRSPC1, IRET)
            IF (IRET.NE.0) GO TO 990
            SPECOK = 0
            CALL YZERO (GRMODL, IRET)
            IF (IRET.NE.0) GO TO 990
            CALL YZERO (GRESID, IRET)
            IF (IRET.NE.0) GO TO 990
            CALL YZERO (GRSPC2, IRET)
            IF (IRET.NE.0) GO TO 990
            END IF
         MSGBUF = 'Spectral smoothing function type (4 characters)'
         CALL INQSTR (TTY, MSGBUF, 4, CTEMP, IRET)
         IF (IRET.LT.0) GO TO 200
         SUBR = 'INQSTR'
         IF (IRET.GT.0) GO TO 990
         SAMTYP = CTEMP
         CALL TYPESM (SAMTYP, SMTYPE)
         IF (SMTYPE.GT.0) THEN
            SUBR = 'INQFLT'
            IF ((SMTYPE.EQ.1) .OR. (SMTYPE.EQ.2)) THEN
               MSGBUF ='Spectral smoothing support in channels'
            ELSE IF (SMTYPE.GT.2) THEN
               MSGBUF ='Spectral smoothing FWHM in channels'
               END IF
            CALL INQFLT (TTY, MSGBUF, 1, DTEMP, IRET)
            IF (IRET.LT.0) GO TO 200
            IF (IRET.GT.0) GO TO 990
            APARM(7) = DTEMP(1)
            CALL SMFUNC
            END IF
C                                       set a sub-image
      ELSE IF (OPTION(CHS).EQ.'SET WINDOW') THEN
         DOIT = .TRUE.
         NBO = 0
         MBOX = 1
         SUBR = 'GRBOXS'
         I = GRMEBG - NGRAY
         CALL GRBOXS (I, MBOX, NBO, BLCO, TRCO, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         CURWIN(1,1) = BLCO(1) + 0.1
         CURWIN(2,1) = BLCO(2) + 0.1
         CURWIN(3,1) = TRCO(1) + 0.1
         CURWIN(4,1) = TRCO(2) + 0.1
C                                       DEBUG
         CALL YSLECT ('OFFF', GRMEBG, 0, SCRTCH, IRET)
C                                       back to full image
      ELSE IF (OPTION(CHS).EQ.'RESET WINDOW') THEN
         DOIT = .TRUE.
         CURWIN(1,1) = TBLC(1) + 0.1
         CURWIN(2,1) = TBLC(2) + 0.1
         CURWIN(3,1) = TTRC(1) + 0.1
         CURWIN(4,1) = TTRC(2) + 0.1
C                                       back to full spectram
      ELSE IF (OPTION(CHS).EQ.'RESET CHANNELS') THEN
         BCHAN = 1
         ECHAN = CATBL2(KINAX)
C                                       set partial spectrum
      ELSE IF (OPTION(CHS).EQ.'SET CHANNELS') THEN
         IF (SPECOK.GT.0) THEN
            SUBR = 'TVSCHN'
            CALL TVSCHN (IRET)
            IF (IRET.GT.0) GO TO 990
         ELSE
            MSGTXT = 'YOU MUST PLOT SPECTRA FIRST'
            CALL MSGWRT (6)
            END IF
C                                       load plane from cube
      ELSE IF (OPTION(CHS).EQ.'LOAD PLANE') THEN
         IF (SPECOK.GT.0) THEN
            SUBR = 'TVPLAN'
            CALL TVPLAN (VERBOS, NX2, NY2, NZ2, IMAGE2, IRET)
            IF (IRET.GT.0) GO TO 990
            SPECOK = 2
         ELSE
            MSGTXT = 'YOU MUST PLOT SPECTRA FIRST'
            CALL MSGWRT (6)
            END IF
C                                       set a sub-image
      ELSE IF (OPTION(CHS).EQ.'SET PL WINDOW') THEN
         IF ((CURPLN.LT.1) .OR. (CURPLN.GT.NZ2)) THEN
            MSGTXT = 'YOU MUST LOAD A PLANE FIRST'
            CALL MSGWRT (6)
         ELSE
            DOIT = .TRUE.
            CALL COPY (256, CATBLK, CATSAV)
            CALL COPY (256, CATBLP, CATBLK)
            NBO = 0
            MBOX = 1
            SUBR = 'GRBOXS'
            I = GRMEBG - NGRAY
            CALL GRBOXS (I, MBOX, NBO, BLCO, TRCO, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 990
            CURWIN(1,2) = BLCO(1) + 0.1
            CURWIN(2,2) = BLCO(2) + 0.1
            CURWIN(3,2) = TRCO(1) + 0.1
            CURWIN(4,2) = TRCO(2) + 0.1
            CALL YSLECT ('OFFF', GRMEBG, 0, SCRTCH, IRET)
            CALL COPY (256, CATSAV, CATBLK)
            END IF
C                                       back to full image
      ELSE IF (OPTION(CHS).EQ.'RESET PL WIN') THEN
         DOIT = (CURPLN.GE.1) .AND. (CURPLN.LE.NZ2)
         CURWIN(1,2) = 1
         CURWIN(2,2) = 1
         CURWIN(3,2) = CATBL4(KINAX)
         CURWIN(4,2) = CATBL4(KINAX+1)
C                                       set plane pixrange
      ELSE IF (OPTION(CHS).EQ.'SET PL RANGE') THEN
C                                       Open terminal
         IF (TTY(2).LE.0) THEN
            TTY(2) = 0
            CALL ZOPEN (TTY(1), TTY(2), 1, MSGBUF, .FALSE., .TRUE.,
     *         .TRUE., IRET)
            IF (IRET.NE.0) THEN
               TTY(2) = 0
               WRITE (MSGTXT,1250) IRET
               CALL MSGWRT (6)
               GO TO 200
               END IF
            END IF
         WRITE (MSGBUF,1030) CATR4(KRDMN), CATR4(KRDMX)
         CALL INQFLT (TTY, MSGBUF, 2, DTEMP, IRET)
         IF (IRET.LT.0) GO TO 200
         SUBR = 'INQFLT'
         IF (IRET.GT.0) GO TO 990
         DOIT = .TRUE.
         IF (DTEMP(1).LT.DTEMP(2)) THEN
            CRANGE(1) = DTEMP(1)
            CRANGE(2) = DTEMP(2)
            CRANGE(1) = MAX (CRANGE(1), CATR4(KRDMN))
            CRANGE(2) = MIN (CRANGE(2), CATR4(KRDMX))
         ELSE
            CRANGE(1) = CATR4(KRDMN)
            CRANGE(2) = CATR4(KRDMX)
            END IF
C                                       verbose
      ELSE IF (OPTION(CHS).EQ.'VERBOSE?') THEN
         VERBOS = .NOT.VERBOS
C                                       curvalue
      ELSE IF (OPTION(CHS).EQ.'CURVALUE') THEN
         SUBR = 'TVSPCV'
         CALL TVSPCV (NX, NY, IMAGE, NX2, NY2, NZ2, IMAGE2, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       label pimages
      ELSE IF (OPTION(CHS).EQ.'LABEL IMAGES?') THEN
C                                       turn labels off
         IF (DOLABL.EQ.2) THEN
            SUBR = 'YSLECT'
            CALL YSLECT ('OFFF', GRLABL, 0, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 990
            SUBR = 'YZERO'
            CALL YZERO (GRLABL, IRET)
            IF (IRET.NE.0) GO TO 990
            DOLABL = 0
C                                       plot labels
         ELSE
            DOLABL = DOLABL + 1
            SUBR = 'TVSPLA'
            CALL TVSPLA (IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       interactive spectral plot
      ELSE IF (OPTION(CHS).EQ.'PLOT SPECTRA') THEN
         NGAUSS = 0
         SUBR = 'TVSPPL'
         CALL TVSPPL (VERBOS, IWINTV, PPOS, NX, NY, IMAGE, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       interactive spectral plot
      ELSE IF (OPTION(CHS).EQ.'PLOT PL SPECTRA') THEN
         IF ((CURPLN.LT.1) .OR. (CURPLN.GT.NZ2)) THEN
            MSGTXT = 'YOU MUST LOAD A PLANE FIRST'
            CALL MSGWRT (6)
         ELSE
            CALL COPY (256, CATBLK, CATSAV)
            CALL COPY (256, CATBLP, CATBLK)
            LOCNUM = 1
            CALL SETLOC (CATBLK(IIDEP), .FALSE.)
            NGAUSS = 0
            SUBR = 'TVSPPL'
            CALL TVSPPL (VERBOS, JWINTV, PPOS, NX2, NY2,
     *         IMAGE2(1,1,CURPLN), IRET)
            IF (IRET.NE.0) GO TO 990
            CALL COPY (256, CATSAV, CATBLK)
            LOCNUM = 1
            CALL SETLOC (CATBLK(IIDEP), .FALSE.)
            END IF
C                                       fit spectrum as slice Gaussians
      ELSE IF (OPTION(CHS).EQ.'FIT SPECTRUM') THEN
         SUBR = 'TVSFIT'
         CALL TVSFIT (IRET)
         IF (IRET.GT.0) GO TO 990
C                                       save spectrum as slice
      ELSE IF (OPTION(CHS).EQ.'SAVE SPECTRUM') THEN
         SUBR = 'TVSPSL'
         CALL TVSPSL (IRET)
         IF (IRET.NE.0) GO TO 990
C                                       blank
      ELSE IF (OPTION(CHS).EQ.' ') THEN
C                                       quit
      ELSE IF (OPTION(CHS).EQ.'EXIT') THEN
         IRET = 0
         GO TO 990
         END IF
C                                       load TV
 200  IF (DOIT) THEN
         CALL YZERO (TVCH, IRET)
         CALL TVSPLD (NX, NY, IMAGE, IRET)
         SUBR = 'TVSPLD'
         IF (IRET.NE.0) GO TO 990
         CALL TVSPL2 (DOIT, NX2, NY2, NZ2, IMAGE2, IRET)
         SUBR = 'TVSPL2'
         IF (IRET.NE.0) GO TO 990
         IF (DOLABL.GT.0) THEN
            SUBR = 'TVSPLA'
            CALL TVSPLA (IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         END IF
      FIRST = .FALSE.
      GO TO 100
C
 990  IF (IRET.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1990) IRET, SUBR
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('Enter image 4 pixrange: min, max',2(1PE11.3))
 1250 FORMAT ('TVSPDO ERROR',I5,' OPENING TERMINAL TO ASK QUESTIONS')
 1990 FORMAT ('TVSPDO: ERROR',I5,' RETURNED BY ROUTINE ',A)
      END
      SUBROUTINE TYPESM (SAMTYP, SMTYPE)
C-----------------------------------------------------------------------
C   TYPESM checks the SAMTYP string for allowed values
C   Input:
C      SAMTYP   C*4   Smoothing type specified
C   Output:
C      SMTYPE   I     Code: 0 none
C-----------------------------------------------------------------------
      CHARACTER SAMTYP*(*)
      INTEGER   SMTYPE
C
      INTEGER   NTYPES
      PARAMETER (NTYPES=5)
C
      INTEGER   I
      CHARACTER TYPES(NTYPES)*4
      DATA TYPES /'BOX','MWF','HANN','GAUS','EXP'/
C-----------------------------------------------------------------------
      CALL CHLTOU (4, SAMTYP)
      SMTYPE = 0
      DO 10 I = 1,NTYPES
         IF (SAMTYP.EQ.TYPES(I)) SMTYPE = I
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SMFUNC
C-----------------------------------------------------------------------
C   SMFUNC uses parameters in COMMON to compute a smoothing function
C   returned also in COMMON
C-----------------------------------------------------------------------
C
      INCLUDE 'TVSPC.INC'
      INTEGER   I
      REAL      W, Y
C-----------------------------------------------------------------------
      SMWID = 0
      CALL RFILL (128, 0.0, SFUNC)
      SFUNC(1) = 1.0
C                                       boxcar
      IF (SMTYPE.EQ.1) THEN
         I = APARM(7)/2.0 + 0.1
         CALL RFILL (I, 1.0, SFUNC)
         SMWID = I + 1
         SFUNC(SMWID) = 0.5
C                                       MWF
      ELSE IF (SMTYPE.EQ.2) THEN
         SMWID = APARM(7) + 1.1
C                                       hanning
      ELSE IF (SMTYPE.EQ.3) THEN
         W = APARM(7) / 2.0
         IF (W.GT.0.0) THEN
            DO 30 I = 1,128
               Y = 1.0 - (I - 1.0) / (2.0*W)
               IF (Y.GT.0.0) THEN
                  SFUNC(I) = Y
                  SMWID = I
               ELSE
                  GO TO 999
                  END IF
 30            CONTINUE
            END IF
C                                       Gaussian
      ELSE IF (SMTYPE.EQ.4) THEN
         W = APARM(7) / 2.0
         IF (W.GT.0.0) THEN
            W = LOG (2.0) / (W * W)
            DO 40 I = 1,128
               Y = EXP (-W * (I-1.0) * (I-1.0))
               IF (Y.GT.0.00001) THEN
                  SFUNC(I) = Y
                  SMWID = I
               ELSE
                  GO TO 999
                  END IF
 40            CONTINUE
            END IF
C                                       Exponential
      ELSE IF (SMTYPE.EQ.5) THEN
         W = APARM(7) / 2.0
         IF (W.GT.0.0) THEN
            W = LOG (2.0) / W
            DO 50 I = 1,128
               Y = EXP (-W * (I-1.0))
               IF (Y.GT.0.00001) THEN
                  SFUNC(I) = Y
                  SMWID = I
               ELSE
                  GO TO 999
                  END IF
 50            CONTINUE
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVSPLD (NX, NY, IMAGE, IRET)
C-----------------------------------------------------------------------
C   TVSPLD loads image #1 or its subimage to the TV
C   Inputs:
C      NX       I      Number x pixels
C      NY       I      Number y pixels
C      IMAGE    R(*)   Image
C   Outputs:
C      IRET     I      error code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IRET
      REAL      IMAGE(NX,*)
C
      INCLUDE 'TVSPC.INC'
      INTEGER   IX, IY, MX, MY, LX, LY, NXINT, NYINT, PLINC(2), NXP,
     *   NYP, I, J, IY1, IY2, IYTV, MPIX
      CHARACTER SUBR*8
      REAL      X, Y
C-----------------------------------------------------------------------
      MX = IWINTV(3) - IWINTV(1) + 1
      MY = IWINTV(4) - IWINTV(2) + 1
      LX = CURWIN(3,1) - CURWIN(1,1) + 1
      LY = CURWIN(4,1) - CURWIN(2,1) + 1
C                                       increments
      IF ((LX.GT.MX) .OR. (LY.GT.MY)) THEN
         NXINT = 1
         NYINT = 1
         PLINC(1) = (LX - 1) / MX + 1
         PLINC(2) = (LY - 1) / MY + 1
         PLINC(1) = MAX (PLINC(1), PLINC(2))
         PLINC(2) = PLINC(1)
         NXP = (LX - 1) / PLINC(1) + 1
         NYP = (LY - 1) / PLINC(2) + 1
         WRITE (MSGTXT,1000) PLINC(1)
      ELSE
         PLINC(1) = 1
         PLINC(2) = 1
         NXINT = (MX - 1) / LX
         NYINT = (MY - 1) / LY
         NXINT = MIN (NXINT, NYINT)
         NXINT = MAX (1, NXINT)
         NYINT = NXINT
         NXP = (LX - 1) * NXINT + 1
         NYP = (LY - 1) * NYINT + 1
         IF (NXINT.GT.1) THEN
            WRITE (MSGTXT,1001) NXINT
         ELSE
            MSGTXT = 'Loading every image pixel in window'
            END IF
         END IF
      CALL MSGWRT (2)
      WRITE (MSGTXT,1010) (CURWIN(J,1), J = 1,4)
      CALL MSGWRT (2)
C                                       corners
      CALL COPY (4, CURWIN(1,1), CATBLK(IIWIN))
      IX = (MX - NXP) / 2
      CATBLK(IICOR) = IX + IWINTV(1)
      CATBLK(IICOR+2) = IX + IWINTV(1) + NXP - 1
      IY = (MY - NYP) / 2
      CATBLK(IICOR+1) = IY + IWINTV(2)
      CATBLK(IICOR+3) = IY + IWINTV(2) + NYP - 1
      CALL YCWRIT (TVCH, CATBLK(IICOR), CATBLK, SCRTCH, IRET)
      SUBR = 'YCWRIT'
      IF (IRET.NE.0) GO TO 990
C                                       load to TV
      IYTV = IY + IWINTV(2) - 1
      MPIX = 1 + (NXP - 1) / NXINT
C                                       First row
      IX = CURWIN(1,1)
      IY = CURWIN(2,1)
      IY1 = IY + 1
      IY2 = CURWIN(4,1)
      CALL ISCALE (FUNTYP, MAXINT, CATR(IRRAN), LX, PLINC(1),
     *   IMAGE(IX,IY), SCRTCH)
      CALL LINTER (MPIX, NXINT, SCRTCH, IBUFF)
      SUBR = 'YIMGIO'
      DO 50 IY = IY1,IY2
         IF (MOD(IY-IY1+1,PLINC(2)).EQ.0) THEN
            CALL ISCALE (FUNTYP, MAXINT, CATR(IRRAN), LX, PLINC(1),
     *         IMAGE(IX,IY), SCRTCH)
            CALL LINTER (MPIX, NXINT, SCRTCH, JBUFF)
            IYTV = IYTV + 1
            CALL YIMGIO ('WRIT', TVCH, CATBLK(IICOR), IYTV, 0, NXP,
     *         IBUFF, IRET)
            IF (IRET.NE.0) GO TO 990
            IF (NYINT.GT.1) THEN
               X = 1.0 / NYINT
               DO 40 I = 2,NYINT
                  Y = (I-1) * X
                  DO 30 J = 1,NXP
                     SCRTCH(J) = IBUFF(J) + Y * (JBUFF(J)-IBUFF(J))
     *                  + 0.49999
 30                  CONTINUE
                  IYTV = IYTV + 1
                  CALL YIMGIO ('WRIT', TVCH, CATBLK(IICOR), IYTV, 0,
     *               NXP, SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 990
 40               CONTINUE
               END IF
            CALL COPY (NXP, JBUFF, IBUFF)
            END IF
 50      CONTINUE
      GO TO 999
C
 990  WRITE (MSGTXT,1990) IRET, SUBR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Loading every',I3,' pixel of image')
 1001 FORMAT ('Loading image interpolated by',I3)
 1010 FORMAT ('Image corners',4I6)
 1990 FORMAT ('TVSPLD ERROR',I5,' FROM ROUTINE ',A)
      END
      SUBROUTINE TVSPL2 (FIRST, NX, NY, NZ, IMAGE, IRET)
C-----------------------------------------------------------------------
C   TVSPL2 loads image #4 or its subimage to the TV
C   Inputs:
C      NX       I      Number x pixels
C      NY       I      Number y pixels
C      IMAGE    R(*)   Image
C   Outputs:
C      IRET     I      error code
C-----------------------------------------------------------------------
      LOGICAL   FIRST
      INTEGER   NX, NY, NZ, IRET
      REAL      IMAGE(NX,NY,*)
C
      INCLUDE 'TVSPC.INC'
      INTEGER   IX, IY, MX, MY, LX, LY, NXINT, NYINT, PLINC(2), NXP,
     *   NYP, I, J, K, IY1, IY2, IYTV, MPIX, DEPTH(5), CATSAV(256)
      CHARACTER SUBR*8
      REAL      X, Y
      DATA DEPTH /5*1/
C-----------------------------------------------------------------------
      IF ((CURPLN.LT.1) .OR. (CURPLN.GT.NZ)) GO TO 999
      CALL COPY (256, CATBLK, CATSAV)
      CALL COPY (256, CATBLP, CATBLK)
      MX = JWINTV(3) - JWINTV(1) + 1
      MY = JWINTV(4) - JWINTV(2) + 1
      LX = CURWIN(3,2) - CURWIN(1,2) + 1
      LY = CURWIN(4,2) - CURWIN(2,2) + 1
      K = CURPLN
      DEPTH(1) = K
C                                       increments
      IF ((LX.GT.MX) .OR. (LY.GT.MY)) THEN
         NXINT = 1
         NYINT = 1
         PLINC(1) = (LX - 1) / MX + 1
         PLINC(2) = (LY - 1) / MY + 1
         PLINC(1) = MAX (PLINC(1), PLINC(2))
         PLINC(2) = PLINC(1)
         NXP = (LX - 1) / PLINC(1) + 1
         NYP = (LY - 1) / PLINC(2) + 1
         WRITE (MSGTXT,1000) PLINC(1)
      ELSE
         PLINC(1) = 1
         PLINC(2) = 1
         NXINT = (MX - 1) / LX
         NYINT = (MY - 1) / LY
         NXINT = MIN (NXINT, NYINT)
         NXINT = MAX (1, NXINT)
         NYINT = NXINT
         NXP = (LX - 1) * NXINT + 1
         NYP = (LY - 1) * NYINT + 1
         IF (NXINT.GT.1) THEN
            WRITE (MSGTXT,1001) NXINT
         ELSE
            MSGTXT = 'Loading every image pixel in window'
            END IF
         END IF
      IF (FIRST) CALL MSGWRT (2)
      WRITE (MSGTXT,1010) (CURWIN(J,2), J = 1,4)
      IF (FIRST) CALL MSGWRT (2)
C                                       corners
      CALL COPY (5, DEPTH, CATBLK(IIDEP))
      CALL COPY (4, CURWIN(1,2), CATBLK(IIWIN))
      IX = (MX - NXP) / 2
      CATBLK(IICOR) = IX + JWINTV(1)
      CATBLK(IICOR+2) = IX + JWINTV(1) + NXP - 1
      IY = (MY - NYP) / 2
      CATBLK(IICOR+1) = IY + JWINTV(2)
      CATBLK(IICOR+3) = IY + JWINTV(2) + NYP - 1
      CATR(IRRAN) = CRANGE(1)
      CATR(IRRAN+1) = CRANGE(2)
      CALL YCWRIT (TVCH, CATBLK(IICOR), CATBLK, SCRTCH, IRET)
      SUBR = 'YCWRIT'
      IF (IRET.NE.0) GO TO 990
      IF (FIRST) THEN
         SUBR = 'YFILL'
         CALL YFILL (TVCH, JWINTV(1), JWINTV(2), JWINTV(3), JWINTV(4),
     *      0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       load to TV
      IYTV = IY + JWINTV(2) - 1
      MPIX = 1 + (NXP - 1) / NXINT
C                                       First row
      IX = CURWIN(1,2)
      IY = CURWIN(2,2)
      IY1 = IY + 1
      IY2 = CURWIN(4,2)
      CALL ISCALE (FUNTYP, MAXINT, CATR(IRRAN), LX, PLINC(1),
     *   IMAGE(IX,IY,K), SCRTCH)
      CALL LINTER (MPIX, NXINT, SCRTCH, IBUFF)
      SUBR = 'YIMGIO'
      DO 50 IY = IY1,IY2
         IF (MOD(IY-IY1+1,PLINC(2)).EQ.0) THEN
            CALL ISCALE (FUNTYP, MAXINT, CATR(IRRAN), LX, PLINC(1),
     *         IMAGE(IX,IY,K), SCRTCH)
            CALL LINTER (MPIX, NXINT, SCRTCH, JBUFF)
            IYTV = IYTV + 1
            CALL YIMGIO ('WRIT', TVCH, CATBLK(IICOR), IYTV, 0, NXP,
     *         IBUFF, IRET)
            IF (IRET.NE.0) GO TO 990
            IF (NYINT.GT.1) THEN
               X = 1.0 / NYINT
               DO 40 I = 2,NYINT
                  Y = (I-1) * X
                  DO 30 J = 1,NXP
                     SCRTCH(J) = IBUFF(J) + Y * (JBUFF(J)-IBUFF(J))
     *                  + 0.49999
 30                  CONTINUE
                  IYTV = IYTV + 1
                  CALL YIMGIO ('WRIT', TVCH, CATBLK(IICOR), IYTV, 0,
     *               NXP, SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 990
 40               CONTINUE
               END IF
            CALL COPY (NXP, JBUFF, IBUFF)
            END IF
 50      CONTINUE
      GO TO 995
C
 990  WRITE (MSGTXT,1990) IRET, SUBR
      CALL MSGWRT (8)
C
 995  CALL COPY (256, CATBLK, CATBLP)
      CALL COPY (256, CATSAV, CATBLK)
      FIRST = .FALSE.
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Loading every',I3,' pixel of image cube plane')
 1001 FORMAT ('Loading image cube plane interpolated by',I3)
 1010 FORMAT ('Image cube plane corners',4I6)
 1990 FORMAT ('TVSPL2 ERROR',I5,' FROM ROUTINE ',A)
      END
      SUBROUTINE TVSPPL (VERBOS, IWIN, PPOS, NX, NY, IMAGE, IRET)
C-----------------------------------------------------------------------
C   Interactive spectrum plot
C   Inputs:
C      VERBOS   L      tell pixel or no
C      IWIN     I(4)   TV window of image
C      NX       I      Number x pixels
C      NY       I      Number y pixels
C      IMAGE    R(*)   Image
C   In/out:
C      PPOS     R(2)   Cursor position of selected slice
C   Outputs:
C      IRET     I      error code
C-----------------------------------------------------------------------
      LOGICAL   VERBOS
      INTEGER   IWIN(4), NX, NY, IRET
      REAL      PPOS(2), IMAGE(NX,*)
C
      INCLUDE 'TVSPC.INC'
      INTEGER   QUAD, IBUT, ITW(3), IXY(2), I, NPIX, NROW, IX0, IY0,
     *   IX1, IY1, IX, IY, IROUND
      REAL      RPOS(2), PIXVAL
      CHARACTER SUBR*8, STRING*16, PREFIX*5
      DOUBLE PRECISION SKY(3)
      LOGICAL   F, DOIT, EQUAL, DOERR
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      SUBR = 'YSLECT'
      CALL YSLECT ('ONNN', GRSPC1, 0, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 990
      SUBR = 'YZERO'
      CALL YZERO (GRSPC1, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (GRMEBG.NE.GRSPC1) THEN
         CALL YZERO (GRMEBG, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      CALL YZERO (GRMODL, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL YZERO (GRESID, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (TWOCUB) THEN
         CALL YZERO (GRSPC2, IRET)
         IF (IRET.NE.0) GO TO 990
         SUBR = 'YSLECT'
         CALL YSLECT ('ONNN', GRSPC2, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       window for curvalue
      NPIX = 13 * CSIZTV(1)
      NROW = 4 * CSIZTV(2)
      IX0 = WINDTV(1) + 2*CSIZTV(1)
      IY0 = WINDTV(4) - NROW + 1 - 2*CSIZTV(2)
      IX1 = IX0 + NPIX - 1
      IY1 = IY0 + NROW - 1
      IF (NGRAPH.EQ.8) THEN
         CALL YZERO (GRBLAC, IRET)
         IF (IRET.NE.0) GO TO 990
         SUBR = 'YFILL'
         CALL YFILL (GRBLAC, IX0, IY0, IX1, IY1, 1, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         SUBR = 'YSLECT'
         CALL YSLECT ('ONNN', GRBLAC, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      SUBR = 'YSLECT'
      CALL YSLECT ('ONNN', GRMEBG, 0, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       cursor set up
      CALL ZTIME (ITW)
      IF ((PPOS(1).LT.IWIN(1)) .OR. (PPOS(1).GT.IWIN(3)) .OR.
     *   (PPOS(2).LT.IWIN(2)) .OR. (PPOS(2).GT.IWIN(4))) THEN
         PPOS(1) = (IWIN(1) + IWIN(3)) / 2.0
         PPOS(2) = (IWIN(2) + IWIN(4)) / 2.0
         END IF
      RPOS(1) = PPOS(1)
      RPOS(2) = PPOS(2)
C                                       on cursor
      SUBR = 'YCURSE'
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IRET)
      IF (IRET.NE.0) GO TO 990
      MSGTXT = 'Hit buttons C or D to stop'
      CALL MSGWRT (1)
      DOERR = .TRUE.
C                                       loop point
 20   SUBR = 'YCURSE'
      CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
C                                       do something
      IF (DOIT) THEN
C                                       exit
         IF (IBUT.GE.4) GO TO 900
C                                       keep in window
         IF ((PPOS(1).LT.CATBLK(IICOR)) .OR.
     *      (PPOS(1).GT.CATBLK(IICOR+2)) .OR.
     *      (PPOS(2).LT.CATBLK(IICOR+1)) .OR.
     *      (PPOS(2).GT.CATBLK(IICOR+3))) THEN
            IF (PPOS(1).LT.CATBLK(IICOR)) PPOS(1) = CATBLK(IICOR)
            IF (PPOS(1).GT.CATBLK(IICOR+2)) PPOS(1) = CATBLK(IICOR+2)
            IF (PPOS(2).LT.CATBLK(IICOR+1)) PPOS(2) = CATBLK(IICOR+1)
            IF (PPOS(2).GT.CATBLK(IICOR+3)) PPOS(2) = CATBLK(IICOR+3)
            SUBR = 'YCURSE'
            CALL YCURSE ('ONNN', F, F, PPOS, QUAD, IBUT, IRET)
            IF (IRET.NE.0) GO TO 990
            END IF
C                                       get image pixel coordinate
         CALL IMA2MP (PPOS, RPOS)
         LOCNUM = 1
         CALL XYVAL (RPOS(1), RPOS(2), SKY(1), SKY(2), SKY(3), IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'BAD SKY COORDINATE XYVAL - TRY AGAIN'
            IF (DOERR) CALL MSGWRT (6)
            DOERR = .FALSE.
            GO TO 20
            END IF
C                                       curvalue
         SUBR = 'YFILL'
         CALL YFILL (GRMEBG, IX0, IY0, IX1, IY1, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         IX = IROUND (RPOS(1))
         IY = IROUND (RPOS(2))
         PIXVAL = IMAGE(IX,IY)
         WRITE (STRING,1020) IX, IY
         IY = IY0 + 3 * CSIZTV(2)
         CALL YHOLD ('ONNN', IRET)
         SUBR = 'IMCHAR'
         CALL IMCHAR (GRMEBG, IX0, IY, 0, 0, STRING(:13), SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         IY = IY - 1.5 * CSIZTV(2)
         IF (PIXVAL.NE.FBLANK) THEN
            CALL METSCA (PIXVAL, PREFIX, EQUAL)
            WRITE (STRING,1021) PIXVAL
            CALL IMCHAR (GRMEBG, IX0, IY, 0, 0, STRING(:10), SCRTCH,
     *         IRET)
            IF (IRET.NE.0) GO TO 990
            STRING = PREFIX
            CALL H2CHR (8, 1, CATH(KHBUN), STRING(6:))
         ELSE
            STRING = 'B  BLANKED'
            CALL IMCHAR (GRMEBG, IX0, IY, 0, 0, STRING(:10), SCRTCH,
     *         IRET)
            IF (IRET.NE.0) GO TO 990
            STRING = ' '
            CALL IMCHAR (GRMEBG, IX0, IY0, 0, 0, STRING(:13), SCRTCH,
     *         IRET)
            STRING = ' '
            END IF
         CALL IMCHAR (GRMEBG, IX0, IY0, 0, 0, STRING(:13), SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         CALL YHOLD ('OFFF', IRET)
C                                       pixel in cube
         LOCNUM = 2
         IF (CORTYP(1).EQ.1) THEN
            IF (CORTYP(LOCNUM).EQ.1) THEN
               CALL XYPIX (SKY(1), SKY(2), RPOS(1), RPOS(2), IRET)
            ELSE
               CALL XYPIX (SKY(2), SKY(1), RPOS(1), RPOS(2), IRET)
               END IF
         ELSE IF (CORTYP(1).EQ.2) THEN
            IF (CORTYP(LOCNUM).EQ.1) THEN
               CALL XYPIX (SKY(2), SKY(1), RPOS(1), RPOS(2), IRET)
            ELSE
               CALL XYPIX (SKY(1), SKY(2), RPOS(1), RPOS(2), IRET)
               END IF
            END IF
         IF (IRET.NE.0) THEN
            MSGTXT = 'BAD SKY COORDINATE XYPIX - TRY AGAIN'
            CALL MSGWRT (6)
            GO TO 20
            END IF
         IXY(1) = RPOS(1) + 0.5
         IXY(2) = RPOS(2) + 0.5
         IF ((IXY(1).LT.1) .OR. (IXY(1).GT.CATBL2(KINAX+1)) .OR.
     *      (IXY(2).LT.1) .OR. (IXY(2).GT.CATBL2(KINAX+2))) THEN
            WRITE (MSGTXT,1000) IXY
            IF (DOERR) CALL MSGWRT (6)
            DOERR = .FALSE.
            GO TO 20
            END IF
         WRITE (MSGTXT,1005) IXY
         IF (VERBOS) CALL MSGWRT (2)
         CALL GETSPE (IXY, IRET)
         SUBR = 'GETSPE'
         IF (IRET.GT.0) GO TO 990
         IF (IRET.LT.0) THEN
            WRITE (MSGTXT,1001) IXY
            IF (DOERR) CALL MSGWRT (6)
            DOERR = .FALSE.
            GO TO 20
            END IF
         DOERR = .TRUE.
         LOCNUM = 3
         CALL PLOTSP (IXY, IRET)
         SUBR = 'PLOTSP'
         IF (IRET.NE.0) GO TO 990
         LASTYZ(1) = IXY(1)
         LASTYZ(2) = IXY(2)
         LASTYZ(3) = APARM(3) + 0.5
         END IF
      GO TO 20
C                                       shut down nicely
 900  CALL YCURSE ('OFFF', F, F, PPOS, QUAD, IBUT, I)
      SUBR = 'YSLECT'
      CALL YSLECT ('OFFF', GRMEBG, 0, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (NGRAPH.EQ.8) THEN
         CALL YSLECT ('OFFF', GRBLAC, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      SPECOK = MAX (1, SPECOK)
      GO TO 999
C                                       error
 990  WRITE (MSGTXT,1990) IRET, SUBR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('YZ pixel',2I6,' out of range')
 1001 FORMAT ('YZ pixel',2I6,' no valid data')
 1005 FORMAT ('Plotting YZ pixel',2I6,' from transposed cube')
 1020 FORMAT ('X=',I4,' Y=',I4)
 1021 FORMAT ('B=',F8.3)
 1990 FORMAT ('TVSPPL ERROR',I4,' RETURNED FROM ',A)
      END
      SUBROUTINE GETSPE (IXY, IRET)
C-----------------------------------------------------------------------
C   GETSPE places the spectrum at that pixel into common
C   Inputs:
C      IXY    I(2)   Y,Z pixel position
C   Outputs:
C      IRET   I      error code
C-----------------------------------------------------------------------
      INTEGER   IXY(2), IRET
C
      INCLUDE 'TVSPC.INC'
      INTEGER   WIN(4), DEPTH(5), OFFS, BIND, IY1, IY2, IZ1, IZ2, IY,
     *   IZ, I, NP, J, K, I1, I2, OK1, OK2
      REAL      RR, RLIM, SIBUFF(MAXIMG), SJBUFF(MAXIMG)
      DATA DEPTH /5*1/
C-----------------------------------------------------------------------
      APARM(3) = MAX (0.0, APARM(3))
      I = APARM(3) + 0.5
      RLIM = APARM(3) * APARM(3)
      IY1 = MAX (IXY(1) - I, 1)
      IY2 = MIN (IXY(1) + I, CATBL2(KINAX+1))
      IZ1 = MAX (IXY(2) - I, 1)
      IZ2 = MIN (IXY(2) + I, CATBL2(KINAX+2))
      NP = CATBL2(KINAX)
      WIN(1) = 1
      WIN(3) = NP
      WIN(2) = IY1
      WIN(4) = IY2
      CALL FILL (NP, 0, IBUFF)
      CALL RFILL (NP, 0.0, SPECTR)
      DO 50 IZ = IZ1,IZ2
         DEPTH(1) = IZ
         CALL COMOFF (CATBL2(KIDIM), CATBL2(KINAX), DEPTH, OFFS, IRET)
         OFFS = OFFS + 1
         CALL MINIT ('READ', LUN2, IND2, CATBL2(KINAX), CATBL2(KINAX+1),
     *      WIN, BUFF1, JBUFSZ, OFFS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT IO TO CUBE IMAGE'
            GO TO 990
            END IF
         DO 40 IY = IY1,IY2
            CALL MDISK ('READ', LUN2, IND2, BUFF1, BIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ TV IMAGE ROW'
               GO TO 990
               END IF
            RR = (IY - IXY(1))**2 + (IZ - IXY(2))**2
            IF (RR.LE.RLIM) THEN
               DO 30 I = 1,NP
                  IF (BUFF1(BIND+I-1).NE.FBLANK) THEN
                     IBUFF(I) = IBUFF(I) + 1
                     SPECTR(I) = SPECTR(I) + BUFF1(BIND+I-1)
                     END IF
 30               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
      OK1 = 0
      DO 60 I = 1,NP
         IF (IBUFF(I).GT.0) THEN
            SPECTR(I) = SPECTR(I) / IBUFF(I)
            OK1 = OK1 + 1
         ELSE
            SPECTR(I) = FBLANK
            END IF
 60      CONTINUE
C                                       Median window smooth
      IF (SMTYPE.EQ.2) THEN
         CALL SMWF (NP, SMWID, SPECTR, SIBUFF, SJBUFF)
C                                       function smooth
      ELSE IF ((SMTYPE.GT.0) .AND. (SMWID.GT.0)) THEN
         CALL RFILL (NP, 0.0, SIBUFF)
         CALL RFILL (NP, 0.0, SJBUFF)
         DO 80 J = 1,NP
            IF (SPECTR(J).NE.FBLANK) THEN
               I1 = MAX (1, J-SMWID)
               I2 = MIN (NP, J+SMWID)
               DO 70 I = I1,I2
                  K = ABS (I - J)
                  SIBUFF(I) = SIBUFF(I) + SFUNC(K) * SPECTR(J)
                  SJBUFF(I) = SJBUFF(I) + SFUNC(K)
 70               CONTINUE
               END IF
 80         CONTINUE
         OK1 = 0
         DO 90 J = 1,NP
            IF (SJBUFF(J).NE.0.0) THEN
               SPECTR(J) = SIBUFF(J) / SJBUFF(J)
               OK1 = OK1 + 1
            ELSE
               SPECTR(J) = FBLANK
               END IF
 90         CONTINUE
         END IF
C                                       read in image 4
      IF (TWOCUB) THEN
         CALL FILL (NP, 0, IBUFF)
         CALL RFILL (NP, 0.0, SPECT2)
         DO 150 IZ = IZ1,IZ2
            DEPTH(1) = IZ
            CALL COMOFF (CATBL3(KIDIM), CATBL3(KINAX), DEPTH, OFFS,
     *         IRET)
            OFFS = OFFS + 1
            CALL MINIT ('READ', LUN3, IND3, CATBL3(KINAX),
     *         CATBL3(KINAX+1), WIN, BUFF1, JBUFSZ, OFFS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'INIT IO TO 2nd CUBE IMAGE'
               GO TO 990
               END IF
            DO 140 IY = IY1,IY2
               CALL MDISK ('READ', LUN3, IND3, BUFF1, BIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READ 2nd CUBE IMAGE ROW'
                  GO TO 990
                  END IF
               RR = (IY - IXY(1))**2 + (IZ - IXY(2))**2
               IF (RR.LE.RLIM) THEN
                  DO 130 I = 1,NP
                     IF (BUFF1(BIND+I-1).NE.FBLANK) THEN
                        IBUFF(I) = IBUFF(I) + 1
                        SPECT2(I) = SPECT2(I) + BUFF1(BIND+I-1)
                        END IF
 130                 CONTINUE
                  END IF
 140           CONTINUE
 150        CONTINUE
         OK2 = 0
         DO 160 I = 1,NP
            IF (IBUFF(I).GT.0) THEN
               SPECT2(I) = SPECT2(I) / IBUFF(I)
               OK2 = OK2 + 1
            ELSE
               SPECT2(I) = FBLANK
               END IF
 160        CONTINUE
C                                       Median window smooth
         IF (SMTYPE.EQ.2) THEN
            CALL SMWF (NP, SMWID, SPECT2, SIBUFF, SJBUFF)
C                                       function smooth
         ELSE IF ((SMTYPE.GT.0) .AND. (SMWID.GT.0)) THEN
            CALL RFILL (NP, 0.0, SIBUFF)
            CALL RFILL (NP, 0.0, SJBUFF)
            DO 180 J = 1,NP
               IF (SPECT2(J).NE.FBLANK) THEN
                  I1 = MAX (1, J-SMWID)
                  I2 = MIN (NP, J+SMWID)
                  DO 170 I = I1,I2
                     K = ABS (I - J)
                     SIBUFF(I) = SIBUFF(I) + SFUNC(K) * SPECT2(J)
                     SJBUFF(I) = SJBUFF(I) + SFUNC(K)
 170                 CONTINUE
                  END IF
 180           CONTINUE
            OK2 = 0
            DO 190 J = 1,NP
               IF (SJBUFF(J).NE.0.0) THEN
                  SPECT2(J) = SIBUFF(J) / SJBUFF(J)
                  OK2 = OK2 + 1
               ELSE
                  SPECT2(J) = FBLANK
                  END IF
 190           CONTINUE
            END IF
         END IF
      IF (TWOCUB) THEN
         IF ((OK1.LE.2) .AND. (OK2.LE.2)) IRET = -1
      ELSE
         IF (OK1.LE.2) IRET = -1
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETSPE ERROR',I5,' ON 'A)
      END
      SUBROUTINE SMWF (NP, SMWID, SPECTR, SIBUFF, SJBUFF)
C-----------------------------------------------------------------------
C   SMWF does a median window filter on the spectrum
C   Input:
C      NP        I      Size of SPECTR
C      SMWID     I      Support size pixels
C   In/Out:
C      SPECTR    R(*)   Spectrum: out is smoothed
C   Outputs:
C      SIBUFF    R(*)   scratch buffer
C      SJBUFF    R(*)   scratch buffer
C-----------------------------------------------------------------------
      INTEGER   NP, SMWID
      REAL      SPECTR(*), SIBUFF(*), SJBUFF(*)
C
      INTEGER   N, I1, I2, I, J, K
      REAL      MEDIAN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      K = SMWID / 2
      DO 30 J = 1,NP
         I1 = MAX (1, J-K)
         I2 = MIN (NP, J+K)
         N = 0
         DO 20 I = I1,I2
            IF (SPECTR(I).NE.FBLANK) THEN
               N = N + 1
               SIBUFF(N) = SPECTR(I)
               END IF
 20         CONTINUE
         IF (N.LE.0) THEN
            SJBUFF(J) = FBLANK
         ELSE
            SJBUFF(J) = MEDIAN (N, SIBUFF)
            END IF
 30      CONTINUE
      CALL RCOPY (NP, SJBUFF, SPECTR)
C
 999  RETURN
      END
      SUBROUTINE PLOTSP (IYZ, IRET)
C-----------------------------------------------------------------------
C   PLOTSP plots the spectrum in common onto graphics channel GRSPC1/2
C   Inputs:
C      IYZ      I(2)   y/z pixels
C   Outputs:
C      IRET     I      error code
C-----------------------------------------------------------------------
      INTEGER   IYZ(2), IRET
C
      INCLUDE 'TVSPC.INC'
      INTEGER   I, NP, CATSAV(256), JDROP(2), TVSIZE(2), NXA, NYA,
     *   I4XTRA, ICHB, ICHL, ICHR, ICHT, IX1, IX2, IY1, IY2, LABEL,
     *   NTEXT, IROUND, INP, JTRIM
      LOGICAL   BLAST, SPLIT, SVERT
      REAL      SRANGE(2), FQFINC, PIXMIN, PIXMAX, RANGE2(2), RMAX,
     *   XBLC(7), XTRC(7), BLC(2), TRC(2), CH(4), YGAP, DX, X, Y, XFAC,
     *   XOFF, XYRATO, XX
      CHARACTER TEXT(2)*80, SUBR*8, XTEXT*80
      DOUBLE PRECISION FQFREQ
      INCLUDE 'INCS:DTVS.INC'
      DATA XBLC, XTRC /14*1.0/
      DATA JDROP /2*0/
C-----------------------------------------------------------------------
C                                       protect TV image catalog
      CALL COPY (256, CATBLK, CATSAV)
      CALL COPY (256, CATBL2, CATBLK)
      CALL COPY (4, SWINTV, SWIN1)
      CALL COPY (4, SWINTV, SWIN2)
C                                       split spectra plots
      SPLIT = (TWOCUB) .AND. (APARM(6).GT.0.0)
      SVERT = .FALSE.
      IF (SPLIT) THEN
C                                       split horizontally
         IF (APARM(4).LT.0.0) THEN
            SWIN1(3) = SWIN1(1) + (SWINTV(3) - SWINTV(1)) / 2
            SWIN2(1) = SWIN1(3) + 1
C                                       split vertically
         ELSE
            SVERT = .TRUE.
            SWIN2(4) = SWIN2(2) + (SWINTV(4) - SWINTV(2)) / 2
            SWIN1(2) = SWIN2(4) + 1
            END IF
         END IF
      LABEL = 3
      IF (APARM(8).GT.0.0) LABEL = 6
      IGR = GRSPC1
      NP = ECHAN - BCHAN + 1
      XBLC(1) = BCHAN
      XBLC(2) = IYZ(1)
      XBLC(3) = IYZ(2)
      XTRC(1) = ECHAN
      XTRC(2) = IYZ(1)
      XTRC(3) = IYZ(2)
C                                       fixed scale
      IF (APARM(2).GT.APARM(1)) THEN
         SRANGE(1) = APARM(1)
         SRANGE(2) = APARM(2)
C                                       full scale
      ELSE IF (APARM(2).LT.APARM(1)) THEN
         SRANGE(1) = CATR(KRDMN)
         SRANGE(2) = CATR(KRDMX)
         IF ((TWOCUB) .AND. (APARM(6).LE.0.0)) THEN
            SRANGE(1) = MIN (SRANGE(1), CATR4(KRDMN))
            SRANGE(2) = MAX (SRANGE(2), CATR4(KRDMX))
            END IF
C                                       self-scale
      ELSE
         SRANGE(1) = 1.E10
         SRANGE(2) = -1.E10
         DO 20 I = BCHAN,ECHAN
            IF (SPECTR(I).NE.FBLANK) THEN
               SRANGE(1) = MIN (SRANGE(1), SPECTR(I))
               SRANGE(2) = MAX (SRANGE(2), SPECTR(I))
               END IF
 20         CONTINUE
         IF ((TWOCUB) .AND. (APARM(6).LE.0.0)) THEN
            DO 25 I = BCHAN,ECHAN
               IF (SPECT2(I).NE.FBLANK) THEN
                  SRANGE(1) = MIN (SRANGE(1), SPECT2(I))
                  SRANGE(2) = MAX (SRANGE(2), SPECT2(I))
                  END IF
 25            CONTINUE
            END IF
         IF (SRANGE(1).GT.SRANGE(2)) THEN
            SRANGE(1) = CATR(KRDMN)
            SRANGE(2) = CATR(KRDMX)
            IF ((TWOCUB) .AND. (APARM(6).LE.0.0)) THEN
               SRANGE(1) = MIN (SRANGE(1), CATR4(KRDMN))
               SRANGE(2) = MAX (SRANGE(2), CATR4(KRDMX))
               END IF
            END IF
         END IF
      FQFREQ = 0.0D0
      FQFINC = 0.0
C                                       mess about a bit
 30   DX = (SRANGE(2) - SRANGE(1)) * 0.03
      RANGE2(2) = SRANGE(2) + DX
      RANGE2(1) = SRANGE(1) - DX
C                                       Calc fac & offset to keep BLC
C                                       TRC within range to prevent
C                                       overflow in graphics routines.
      PIXMAX = RANGE2(2)
      PIXMIN = RANGE2(1)
      XFAC = 39999.0 / (PIXMAX - PIXMIN)
      XOFF = 40000.0 - XFAC * PIXMAX
      RANGE2(1) = XFAC * RANGE2(1) + XOFF
      RANGE2(2) = XFAC * RANGE2(2) + XOFF
      RMAX = 2.0 ** (NBITWD-1) - 1
C                                       Must reduce users max value.
      IF (RANGE2(2).GT.RMAX) RANGE2(2) = RMAX
C                                       Must increase users min.
      IF (RANGE2(1).LT.-RMAX) RANGE2(1) = -RMAX
C                                       Round and back calc range.
      RANGE2(1) = IROUND (RANGE2(1))
      RANGE2(2) = IROUND (RANGE2(2))
      IF (RANGE2(1).GE.RANGE2(2)) THEN
         SRANGE(1) = CATR(KRDMN)
         SRANGE(2) = CATR(KRDMX)
         GO TO 30
         END IF
      SRANGE(1) = (RANGE2(1) - XOFF) / XFAC
      SRANGE(2) = (RANGE2(2) - XOFF) / XFAC
      BLC(2) = RANGE2(1)
      TRC(2) = RANGE2(2)
C                                       Initialize plot file line drw.
      CALL SLBINI (JDROP, NP, SRANGE, BLC, TRC, XBLC, XTRC, FQFREQ,
     *   FQFINC, CATBLK(IIDEP), LABEL, YGAP, CH, TEXT, NTEXT)
      XYRATO = (TRC(2) - BLC(2)) / (TRC(1) - BLC(1))
      IX1 = BLC(1) + .5
      IY1 = BLC(2) + .5
      IX2 = TRC(1) + .5
      IY2 = TRC(2) + .5
      IF (SVERT) THEN
         CH(1) = MAX (9.0, CH(1))
         CH(2) = 0.5
         XTEXT = TEXT(1)
         NTEXT = 0
         CTYP(1,LOCNUM) = ' '
         CPREF(1,LOCNUM) = ' '
         END IF
      ICHL = CH(1) * CSIZTV(1) + .5
      ICHB = CH(2) * CSIZTV(2) + .5
      ICHR = CH(3) * CSIZTV(1) + .5
      ICHT = CH(4) * CSIZTV(2) + .5
      TVSIZE(1) = SWIN1(3) - SWIN1(1) + 1
      TVSIZE(2) = SWIN1(4) - SWIN1(2) + 1
      NYA = SWIN1(4) - SWIN1(2) - ICHT - ICHB
      NXA = SWIN1(3) - SWIN1(1) - ICHL - ICHR
C                                       compute scaling
      X = IX2
      X = ABS (X - IX1)
      XX = X * XYRATO
      Y = IY2
      Y = ABS (Y - IY1)
      IF ((XX.LE.0.0) .OR. (Y.LE.0.0)) THEN
         MSGTXT = 'SCALING ERROR'
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
         END IF
      SCALEY = (NYA - 1) / Y
      SCALEX = (NXA - 1) / X
      NXA = SCALEX * X + ICHL + ICHR
      IF (NXA.GE.TVSIZE(1)) THEN
         SCALEX = SCALEX * (FLOAT(TVSIZE(1)) / (NXA + 5.0))
         SCALEY = SCALEY * (FLOAT(TVSIZE(1)) / (NXA + 5.0))
         NXA = SCALEX * X + ICHL + ICHR
         END IF
      NYA = SCALEY * Y + ICHB + ICHT
      IF (NXA.GE.TVSIZE(1)) THEN
         SCALEX = SCALEX * (FLOAT(TVSIZE(2)) / (NYA + 5.0))
         SCALEY = SCALEY * (FLOAT(TVSIZE(2)) / (NYA + 5.0))
         NXA = SCALEX * X + ICHL + ICHR
         NYA = SCALEY * Y + ICHB + ICHT
         END IF
      RX0 = ICHL + MAX (0, TVSIZE(1)-NXA) / 2 + SWIN1(1)
      RY0 = ICHB + MAX (0, TVSIZE(2)-NYA) / 2 + SWIN1(2)
C                                       Put stuff in image catalog.
      CATBLK(IIWIN  ) = IX1 + BCHAN - 1
      CATBLK(IIWIN+1) = IY1
      CATBLK(IIWIN+2) = IX2 + BCHAN - 1
      CATBLK(IIWIN+3) = IY2
      CATBLK(IICOR  ) = RX0 + 0.5
      CATBLK(IICOR+1) = RY0 + 0.5
      CATBLK(IICOR+2) = RX0 + X * SCALEX + .5
      CATBLK(IICOR+3) = RY0 + Y * SCALEY + .5
      CATBLK(IIPLT) = 5
      CATBLK(IIOTH) = LABEL
      CATBLK(IIOTH+1) = JDROP(1)
      CATBLK(IIOTH+2) = JDROP(2)
      I4XTRA = IIOTH + 3
      CATR(I4XTRA  ) = XBLC(1)
      CATR(I4XTRA+1) = XBLC(2)
      CATR(I4XTRA+2) = XTRC(1)
      CATR(I4XTRA+3) = XTRC(2)
      CATR(I4XTRA+4) = XBLC(3)
      CATR(I4XTRA+5) = XTRC(3)
      I4XTRA = I4XTRA + 6
      I4XTRA = I4XTRA/2 + 1
      CATD(I4XTRA) = FQFREQ
      I4XTRA = 2*I4XTRA + 1
      CATR(I4XTRA) = FQFINC
      CALL CHR2H (2, 'SL', KHPTYO, CATBLK(KHPTY))
      CATR(IRRAN) = SRANGE(1)
      CATR(IRRAN+1) = SRANGE(2)
C
      RX0 = RX0 - BLC(1) * SCALEX + .5
      RY0 = RY0 - BLC(2) * SCALEY + .5
C                                       image catalog etc
      CALL YCINIT (GRSPC1, SCRTCH)
      SUBR = 'YZERO'
      CALL YZERO (GRSPC1, IRET)
      IF (IRET.NE.0) GO TO 990
      SUBR = 'YCWRIT'
      CALL YCWRIT (GRSPC1, CATBLK(IICOR), CATBLK, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 990
      IF ((TWOCUB) .AND. (APARM(6).LE.0.0)) THEN
         CALL YCINIT (GRSPC2, SCRTCH)
         SUBR = 'YZERO'
         CALL YZERO (GRSPC2, IRET)
         IF (IRET.NE.0) GO TO 990
         SUBR = 'YCWRIT'
         CALL YCWRIT (GRSPC2, CATBLK(IICOR), CATBLK, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      IF (NTEXT.GE.1) THEN
         INP = JTRIM (TEXT(1))
         WRITE (XTEXT,1030) IYZ, APARM(3)
         CALL REFRMT (XTEXT, '_', I)
         TEXT(1)(INP+3:) = XTEXT(:I)
         END IF
      SUBR = 'TVLAB'
      CALL TVLAB (BLC, TRC, LABEL, YGAP, TEXT, NTEXT, CH, .FALSE., IRET)
      IF (IRET.NE.0) GO TO 990
C                                       plot slice
      SUBR = 'TVVEC'
      BLAST = .TRUE.
      DO 50 I = BCHAN,ECHAN
         IF (SPECTR(I).EQ.FBLANK) THEN
            BLAST = .TRUE.
         ELSE
            X = I - BCHAN + 1
            Y = XFAC * SPECTR(I) + XOFF
            Y = MIN (Y, RANGE2(2))
            Y = MAX (Y, RANGE2(1))
            INP = 2
            IF (BLAST) INP = 1
            CALL TVVEC (X, Y, INP, IRET)
            IF (IRET.NE.0) GO TO 990
            BLAST = .FALSE.
            END IF
 50      CONTINUE
      IF ((TWOCUB) .AND. (APARM(6).LE.0.0)) THEN
         IGR = GRSPC2
         BLAST = .TRUE.
         DO 60 I = BCHAN,ECHAN
            IF (SPECT2(I).EQ.FBLANK) THEN
               BLAST = .TRUE.
            ELSE
               X = I - BCHAN + 1
               Y = XFAC * SPECT2(I) + XOFF
               Y = MIN (Y, RANGE2(2))
               Y = MAX (Y, RANGE2(1))
               INP = 2
               IF (BLAST) INP = 1
               CALL TVVEC (X, Y, INP, IRET)
               IF (IRET.NE.0) GO TO 990
               BLAST = .FALSE.
               END IF
 60         CONTINUE
         IGR = GRSPC1
         END IF
C                                       save 1st cube spectrum plot
      CALL COPY (256, CATBLK, CATBLS)
C                                       Split spectra
      IF (SPLIT) THEN
         CALL COPY (256, CATBL3, CATBLK)
         LABEL = 3
         IF (APARM(8).GT.0.0) LABEL = 6
         IGR = GRSPC2
         NP = ECHAN - BCHAN + 1
         XBLC(1) = BCHAN
         XBLC(2) = IYZ(1)
         XBLC(3) = IYZ(2)
         XTRC(1) = ECHAN
         XTRC(2) = IYZ(1)
         XTRC(3) = IYZ(2)
C                                       fixed scale
         IF (APARM(2).GT.APARM(1)) THEN
            SRANGE(1) = APARM(1)
            SRANGE(2) = APARM(2)
C                                       full scale
         ELSE IF (APARM(2).LT.APARM(1)) THEN
            SRANGE(1) = CATR(KRDMN)
            SRANGE(2) = CATR(KRDMX)
C                                       self-scale
         ELSE
            SRANGE(1) = 1.E10
            SRANGE(2) = -1.E10
            DO 120 I = BCHAN,ECHAN
               IF (SPECT2(I).NE.FBLANK) THEN
                  SRANGE(1) = MIN (SRANGE(1), SPECT2(I))
                  SRANGE(2) = MAX (SRANGE(2), SPECT2(I))
                  END IF
 120           CONTINUE
            END IF
         FQFREQ = 0.0D0
         FQFINC = 0.0
C                                       mess about a bit
 130     DX = (SRANGE(2) - SRANGE(1)) * 0.03
         RANGE2(2) = SRANGE(2) + DX
         RANGE2(1) = SRANGE(1) - DX
C                                       Calc fac & offset to keep BLC
C                                       TRC within range to prevent
C                                       overflow in graphics routines.
         PIXMAX = RANGE2(2)
         PIXMIN = RANGE2(1)
         XFAC = 39999.0 / (PIXMAX - PIXMIN)
         XOFF = 40000.0 - XFAC * PIXMAX
         RANGE2(1) = XFAC * RANGE2(1) + XOFF
         RANGE2(2) = XFAC * RANGE2(2) + XOFF
         RMAX = 2.0 ** (NBITWD-1) - 1
C                                       Must reduce users max value.
         IF (RANGE2(2).GT.RMAX) RANGE2(2) = RMAX
C                                       Must increase users min.
         IF (RANGE2(1).LT.-RMAX) RANGE2(1) = -RMAX
C                                       Round and back calc range.
         RANGE2(1) = IROUND (RANGE2(1))
         RANGE2(2) = IROUND (RANGE2(2))
         IF (RANGE2(1).GE.RANGE2(2)) THEN
            SRANGE(1) = CATR(KRDMN)
            SRANGE(2) = CATR(KRDMX)
            GO TO 130
            END IF
         SRANGE(1) = (RANGE2(1) - XOFF) / XFAC
         SRANGE(2) = (RANGE2(2) - XOFF) / XFAC
         BLC(2) = RANGE2(1)
         TRC(2) = RANGE2(2)
C                                       Initialize plot file line drw.
         IF (SVERT) THEN
            LABEL = 7
            IF (APARM(8).GT.0.0) LABEL = 10
            END IF
         CALL SLBINI (JDROP, NP, SRANGE, BLC, TRC, XBLC, XTRC, FQFREQ,
     *      FQFINC, CATBLK(IIDEP), LABEL, YGAP, CH, TEXT, NTEXT)
         XYRATO = (TRC(2) - BLC(2)) / (TRC(1) - BLC(1))
         IX1 = BLC(1) + .5
         IY1 = BLC(2) + .5
         IX2 = TRC(1) + .5
         IY2 = TRC(2) + .5
         IF (SVERT) THEN
            CH(1) = MAX (9.0, CH(1))
            CH(4) = 0.5
            NTEXT = 1
            TEXT(1) = XTEXT
            CH(2) = CH(2) + 1.5
            END IF
         ICHL = CH(1) * CSIZTV(1) + .5
         ICHB = CH(2) * CSIZTV(2) + .5
         ICHR = CH(3) * CSIZTV(1) + .5
         ICHT = CH(4) * CSIZTV(2) + .5
         TVSIZE(1) = SWIN2(3) - SWIN2(1) + 1
         TVSIZE(2) = SWIN2(4) - SWIN2(2) + 1
         NYA = SWIN2(4) - SWIN2(2) - ICHT - ICHB
         NXA = SWIN2(3) - SWIN2(1) - ICHL - ICHR
C                                       compute scaling
         X = IX2
         X = ABS (X - IX1)
         XX = X * XYRATO
         Y = IY2
         Y = ABS (Y - IY1)
         IF ((XX.LE.0.0) .OR. (Y.LE.0.0)) THEN
            MSGTXT = 'SCALING ERROR'
            CALL MSGWRT (8)
            IRET = 1
            GO TO 999
            END IF
         SCALEY = (NYA - 1) / Y
         SCALEX = (NXA - 1) / X
         NXA = SCALEX * X + ICHL + ICHR
         IF (NXA.GE.TVSIZE(1)) THEN
            SCALEX = SCALEX * (FLOAT(TVSIZE(1)) / (NXA + 5.0))
            SCALEY = SCALEY * (FLOAT(TVSIZE(1)) / (NXA + 5.0))
            NXA = SCALEX * X + ICHL + ICHR
            END IF
         NYA = SCALEY * Y + ICHB + ICHT
         IF (NXA.GE.TVSIZE(1)) THEN
            SCALEX = SCALEX * (FLOAT(TVSIZE(2)) / (NYA + 5.0))
            SCALEY = SCALEY * (FLOAT(TVSIZE(2)) / (NYA + 5.0))
            NXA = SCALEX * X + ICHL + ICHR
            NYA = SCALEY * Y + ICHB + ICHT
             END IF
         RX0 = ICHL + MAX (0, TVSIZE(1)-NXA) / 2 + SWIN2(1)
         RY0 = ICHB + MAX (0, TVSIZE(2)-NYA) / 2 + SWIN2(2)
C                                       Put stuff in image catalog.
         CATBLK(IIWIN  ) = IX1 + BCHAN - 1
         CATBLK(IIWIN+1) = IY1
         CATBLK(IIWIN+2) = IX2 + BCHAN - 1
         CATBLK(IIWIN+3) = IY2
         CATBLK(IICOR  ) = RX0 + 0.5
         CATBLK(IICOR+1) = RY0 + 0.5
         CATBLK(IICOR+2) = RX0 + X * SCALEX + .5
         CATBLK(IICOR+3) = RY0 + Y * SCALEY + .5
         CATBLK(IIPLT) = 5
         CATBLK(IIOTH) = LABEL
         CATBLK(IIOTH+1) = JDROP(1)
         CATBLK(IIOTH+2) = JDROP(2)
         I4XTRA = IIOTH + 3
         CATR(I4XTRA  ) = XBLC(1)
         CATR(I4XTRA+1) = XBLC(2)
         CATR(I4XTRA+2) = XTRC(1)
         CATR(I4XTRA+3) = XTRC(2)
         CATR(I4XTRA+4) = XBLC(3)
         CATR(I4XTRA+5) = XTRC(3)
         I4XTRA = I4XTRA + 6
         I4XTRA = I4XTRA/2 + 1
         CATD(I4XTRA) = FQFREQ
         I4XTRA = 2*I4XTRA + 1
         CATR(I4XTRA) = FQFINC
         CALL CHR2H (2, 'SL', KHPTYO, CATBLK(KHPTY))
         CATR(IRRAN) = SRANGE(1)
         CATR(IRRAN+1) = SRANGE(2)
C
         RX0 = RX0 - BLC(1) * SCALEX + .5
         RY0 = RY0 - BLC(2) * SCALEY + .5
C                                       image catalog etc
         CALL YCINIT (GRSPC2, SCRTCH)
         SUBR = 'YZERO'
         CALL YZERO (GRSPC2, IRET)
         IF (IRET.NE.0) GO TO 990
         SUBR = 'YCWRIT'
         CALL YCWRIT (GRSPC2, CATBLK(IICOR), CATBLK, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         IF (NTEXT.GE.1) THEN
            INP = JTRIM (TEXT(1))
            WRITE (XTEXT,1030) IYZ, APARM(3)
            CALL REFRMT (XTEXT, '_', I)
            TEXT(1)(INP+3:) = XTEXT(:I)
            END IF
         SUBR = 'TVLAB'
         CALL TVLAB (BLC, TRC, LABEL, YGAP, TEXT, NTEXT, CH, .FALSE.,
     *      IRET)
         IF (IRET.NE.0) GO TO 990
C                                       plot slice
         SUBR = 'TVVEC'
         BLAST = .TRUE.
         DO 150 I = BCHAN,ECHAN
            IF (SPECT2(I).EQ.FBLANK) THEN
               BLAST = .TRUE.
            ELSE
               X = I - BCHAN + 1
               Y = XFAC * SPECT2(I) + XOFF
               Y = MIN (Y, RANGE2(2))
               Y = MAX (Y, RANGE2(1))
               INP = 2
               IF (BLAST) INP = 1
               CALL TVVEC (X, Y, INP, IRET)
               IF (IRET.NE.0) GO TO 990
               BLAST = .FALSE.
               END IF
 150        CONTINUE
         IGR = GRSPC1
         CALL COPY (256, CATBLK, CATBS2)
         END IF
      GO TO 995
C
 990  WRITE (MSGTXT,1990) IRET, SUBR
      CALL MSGWRT (8)
      CALL COPY (256, CATBLK, CATBLS)
C
 995  CALL COPY (256, CATSAV, CATBLK)
      IF (IRET.EQ.0) CALL TVPMRK (IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('PIXEL',2I6,'___RADIUS',F4.1)
 1990 FORMAT ('PLOTSP ERROR',I5,' FROM ',A)
      END
      SUBROUTINE TVSCHN (IRET)
C-----------------------------------------------------------------------
C   TVSCHN interacts to set new values for BCHAN and ECHAN
C   Outputs:
C      IRET   I   > 0 => serious error
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'TVSPC.INC'
      INTEGER   IBCHAN, IECHAN, NCHAN, IWIN(4), IXC(4), IYC(2), IX0,
     *   IX1, IY0, IY1, NPIX, NROW, IVAL, QUAD, IBUT, ITW(3), LBCHAN,
     *   LECHAN, IY
      REAL      RPOS(2), PPOS(2), LPOS(2), X
      CHARACTER STRING*6
      LOGICAL   DOIT, T, F
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      CALL ZTIME (ITW)
      CALL COPY (4, CATBLS(IICOR), IWIN)
      IBCHAN = CATBLS(IIWIN)
      IECHAN = CATBLS(IIWIN+2)
      LBCHAN = IBCHAN
      LECHAN = IECHAN
      NCHAN = IECHAN - IBCHAN + 1
      IYC(1) = IWIN(2)
      IYC(2) = IWIN(4)
      IXC(1) = IWIN(1)
      IXC(2) = IWIN(1)
      IXC(3) = IWIN(3)
      IXC(4) = IWIN(3)
      CALL YZERO (GRMEBG, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       curvalue display
      NPIX = 6 * CSIZTV(1)
      NROW = 2.5 * CSIZTV(2) + 0.5
      IX0 = WINDTV(1) + 2*CSIZTV(1)
      IY0 = WINDTV(4) - NROW + 1 - 2*CSIZTV(2)
      IX1 = IX0 + NPIX - 1
      IY1 = IY0 + NROW - 1
      IF (NGRAPH.EQ.8) THEN
         CALL YZERO (GRBLAC, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL YFILL (GRBLAC, IX0, IY0, IX1, IY1, 1, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL YSLECT ('ONNN', GRBLAC, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         END IF
      CALL YSLECT ('ONNN', GRMEBG, 0, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       instructions
      MSGTXT = 'Hit buttons A or B to switch between lower and upper'
     *   // ' limits'
      CALL MSGWRT (1)
      MSGTXT = 'Hit buttons C or D to set new limits for next plot'
      CALL MSGWRT (1)
      RPOS(1) = IXC(3)
      RPOS(2) = (IYC(1) + IYC(2)) / 2.0
      IVAL = 2
 10   PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IRET)
      IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 970
C                                       cursor read loop point
 20   CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IRET)
      LPOS(1) = RPOS(1)
      LPOS(2) = RPOS(2)
      IF (RPOS(1).GT.IWIN(3)) RPOS(1) = IWIN(3)
      IF (RPOS(1).LT.IWIN(1)) RPOS(1) = IWIN(1)
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (.NOT.DOIT) THEN
         GO TO 20
      ELSE
         CALL IMVECT ('OFFF', GRMEBG, 2, IXC(1), IYC, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL IMVECT ('OFFF', GRMEBG, 2, IXC(3), IYC, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         IXC(2*IVAL-1) = RPOS(1) + 0.5
         IXC(2*IVAL) = RPOS(1) + 0.5
         CALL IMVECT ('ONNN', GRMEBG, 2, IXC(1), IYC, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL IMVECT ('ONNN', GRMEBG, 2, IXC(3), IYC, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         X = IBCHAN + ((RPOS(1) - IWIN(1)) / (IWIN(3) - IWIN(1))) *
     *      (IECHAN - IBCHAN)
         IF (IVAL.EQ.2) THEN
            LECHAN = X + 0.5
         ELSE
            LBCHAN = X + 0.5
            END IF
         WRITE (STRING,1020) LECHAN
         IY = IY0 + 1.5*CSIZTV(2) + 0.5
         CALL IMCHAR (GRMEBG, IX0, IY, 0, 0, STRING(:6), SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (STRING,1020) LBCHAN
         CALL IMCHAR (GRMEBG, IX0, IY0, 0, 0, STRING(:6), SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         IF (IBUT.LT.4) THEN
            IF (IBUT.GT.0) THEN
               IVAL = 3 - IVAL
               RPOS(1) = IXC(2*IVAL)
               END IF
            IF ((RPOS(1).NE.LPOS(1)) .OR. (RPOS(2).NE.LPOS(2))) GO TO 10
            GO TO 20
            END IF
         END IF
      BCHAN = MIN (LBCHAN, LECHAN)
      ECHAN = MAX (LBCHAN, LECHAN)
      GO TO 990
C                                       error
 970  WRITE (MSGTXT,1970) IRET
      CALL MSGWRT (8)
C
 990  CALL YZERO (GRMEBG, IVAL)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (I6)
 1970 FORMAT ('TVSCHN ERROR',I4,' ON BASIC TV OPERATION')
      END
      SUBROUTINE TVPLAN (VERBOS, NX, NY, NZ, IMAGE, IRET)
C-----------------------------------------------------------------------
C   TVPLAN interacts to load planes from cube
C   Outputs:
C      IRET   I   > 0 => serious error
C-----------------------------------------------------------------------
      LOGICAL   VERBOS
      INTEGER   NX, NY, NZ, IRET
      REAL      IMAGE(NX,NY,*)
C
      INCLUDE 'TVSPC.INC'
      INTEGER   IBCHAN, IECHAN, NCHAN, IWIN(4), IXC(2), IYC(2), IX0,
     *   IX1, IY0, IY1, NPIX, NROW, IVAL, QUAD, IBUT, ITW(3), LBCHAN,
     *   LECHAN, IY
      REAL      RPOS(2), PPOS(2), LPOS(2), X, XX
      CHARACTER STRING*6
      LOGICAL   DOIT, T, F, FIRST
      DOUBLE PRECISION VAL
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      CALL ZTIME (ITW)
      CALL COPY (4, CATBLS(IICOR), IWIN)
      IBCHAN = CATBLS(IIWIN)
      IECHAN = CATBLS(IIWIN+2)
      LBCHAN = IBCHAN
      LECHAN = IECHAN
      NCHAN = IECHAN - IBCHAN + 1
      IYC(1) = IWIN(2)
      IYC(2) = IWIN(4)
      IXC(1) = (IWIN(1) + IWIN(3)) / 2
      IXC(2) = IXC(1)
      CALL YZERO (GRMEBG, IRET)
      IF (IRET.NE.0) GO TO 970
      FIRST = .TRUE.
C                                       curvalue display
      NPIX = 6 * CSIZTV(1)
      NROW = 1.5 * CSIZTV(2) + 0.5
      IX0 = WINDTV(1) + 2*CSIZTV(1)
      IY0 = WINDTV(4) - NROW + 1 - 2*CSIZTV(2)
      IX1 = IX0 + NPIX - 1
      IY1 = IY0 + NROW - 1
      IF (NGRAPH.EQ.8) THEN
         CALL YZERO (GRBLAC, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL YFILL (GRBLAC, IX0, IY0, IX1, IY1, 1, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL YSLECT ('ONNN', GRBLAC, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         END IF
      CALL YSLECT ('ONNN', GRMEBG, 0, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       instructions
      MSGTXT = 'Drag mouse to select plane'
      CALL MSGWRT (1)
      MSGTXT = 'Hit any button to exit'
      CALL MSGWRT (1)
      RPOS(1) = IXC(1)
      RPOS(2) = (IYC(1) + IYC(2)) / 2.0
      IVAL = 2
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IRET)
      IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 970
C                                       cursor read loop point
 20   CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IRET)
      LPOS(1) = RPOS(1)
      LPOS(2) = RPOS(2)
      IF (RPOS(1).GT.IWIN(3)) RPOS(1) = IWIN(3)
      IF (RPOS(1).LT.IWIN(1)) RPOS(1) = IWIN(1)
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (.NOT.DOIT) THEN
         GO TO 20
      ELSE
         CALL IMVECT ('OFFF', GRMEBG, 2, IXC(1), IYC, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         IXC(1) = RPOS(1) + 0.5
         IXC(2) = IXC(1)
         CALL IMVECT ('ONNN', GRMEBG, 2, IXC(1), IYC, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         X = IBCHAN + ((RPOS(1) - IWIN(1)) / (IWIN(3) - IWIN(1))) *
     *      (IECHAN - IBCHAN)
         LECHAN = X + 0.5
         WRITE (STRING,1020) LECHAN
         IY = IY0 + 0.5*CSIZTV(2) + 0.5
         CALL IMCHAR (GRMEBG, IX0, IY, 0, 0, STRING(:6), SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 970
         VAL = CATD2(KDCRV) + (X-CATR2(KRCRP)) * CATR2(KRCIC)
         XX = (VAL - CATD4(KDCRV+2)) / CATR4(KRCIC+2) + CATR4(KRCRP+2)
         IF ((X.GE.0.5) .AND. (X.LT.NZ+0.5)) THEN
            CURPLN = XX + 0.5
            CURCHN = X + 0.5
            WRITE (MSGTXT,1025) CURPLN, VAL
            IF (VERBOS) CALL MSGWRT (2)
            CALL TVSPL2 (FIRST, NX, NY, NZ, IMAGE, IRET)
            IF (IRET.NE.0) GO TO 970
            END IF
         IF (IBUT.EQ.0) GO TO 20
         END IF
      GO TO 990
C                                       error
 970  WRITE (MSGTXT,1970) IRET
      CALL MSGWRT (8)
C
 990  CALL YZERO (GRMEBG, IVAL)
      IF (IRET.EQ.0) CALL TVPMRK (IRET)
      IF ((IRET.EQ.0) .AND. (DOLABL.GT.0)) CALL TVSPLA (IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (I6)
 1025 FORMAT ('Loading plane',I6,' value',1PE11.3)
 1970 FORMAT ('TVPLAN ERROR',I4,' ON BASIC TV OPERATION')
      END
      SUBROUTINE TVSPCV (NX, NY, IMAGE, NX2, NY2, NZ2, IMAGE2, IRET)
C-----------------------------------------------------------------------
C   TVSPDO does curvalue in any of the 4 picture areas
C   Inputs
C      NX       I      Number x pixels
C      NY       I      Number y pixels
C      NX2      I      Number x pixels image 2
C      NY2      I      Number y pixels image 2
C      NZ2      I      Number z pixels image 2
C      IMAGE    R(*)   Image
C      IMAGE2   R(*)   Image plane from #2
C   Outputs:
C      IRET     I      error
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NX2, NY2, NZ2, IRET
      REAL      IMAGE(NX,*), IMAGE2(NX2,NY2,*)
C
      INCLUDE 'TVSPC.INC'
      INTEGER   QUAD, IBUT, ITW(3), I, NPIX, NROW, IX0, IY0, IX1, IY1,
     *   IX, IY, IROUND, CATSAV(256), IMTYPE, LIMTYP, IWIN(4,4), IBCHAN,
     *   IECHAN
      REAL      PPOS(2), RPOS(2), PIXVAL, X
      CHARACTER SUBR*8, STRING*16, PREFIX*5
      DOUBLE PRECISION SKY(3)
      LOGICAL   F, DOIT, EQUAL, DOERR
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      CALL COPY (256, CATBLK, CATSAV)
      LOCNUM = 1
      LIMTYP = 0
      CALL COPY (4, CATBLK(IICOR), IWIN(1,1))
      CALL COPY (4, CATBLP(IICOR), IWIN(1,2))
      CALL COPY (4, CATBLS(IICOR), IWIN(1,3))
      CALL COPY (4, CATBS2(IICOR), IWIN(1,4))
C                                       window for curvalue
      NPIX = 13 * CSIZTV(1)
      NROW = 5.5 * CSIZTV(2)
      IX0 = WINDTV(1) + 2*CSIZTV(1)
      IY0 = WINDTV(4) - NROW + 1 - 2*CSIZTV(2)
      IX1 = IX0 + NPIX - 1
      IY1 = IY0 + NROW - 1
      SUBR = 'YZERO'
      CALL YZERO (GRMEBG, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (NGRAPH.EQ.8) THEN
         CALL YZERO (GRBLAC, IRET)
         IF (IRET.NE.0) GO TO 990
         SUBR = 'YFILL'
         CALL YFILL (GRBLAC, IX0, IY0, IX1, IY1, 1, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         SUBR = 'YSLECT'
         CALL YSLECT ('ONNN', GRBLAC, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      SUBR = 'YSLECT'
      CALL YSLECT ('ONNN', GRMEBG, 0, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       cursor set up
      CALL ZTIME (ITW)
      PPOS(1) = (CATBLK(IICOR) + CATBLK(IICOR+2)) / 2.0
      PPOS(2) = (CATBLK(IICOR+1) + CATBLK(IICOR+3)) / 2.0
      RPOS(1) = PPOS(1)
      RPOS(2) = PPOS(2)
C                                       on cursor
      SUBR = 'YCURSE'
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IRET)
      IF (IRET.NE.0) GO TO 990
      MSGTXT = 'Hit buttons C or D to stop'
      CALL MSGWRT (1)
      DOERR = .TRUE.
C                                       loop point
 20   SUBR = 'YCURSE'
      CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
C                                       do something
      IF (DOIT) THEN
C                                       exit
         IF (IBUT.GE.4) GO TO 900
C                                       which image?
         IMTYPE = 0
         IF ((SPECOK.GT.0) .AND. (TWOCUB)) THEN
            IF ((RPOS(1).GE.IWIN(1,4)) .AND. (RPOS(1).LE.IWIN(3,4))
     *         .AND. (RPOS(2).GE.IWIN(2,4)) .AND.
     *         (RPOS(2).LE.IWIN(4,4))) IMTYPE = 4
            END IF
         IF (SPECOK.GT.0) THEN
            IF ((RPOS(1).GE.IWIN(1,3)) .AND. (RPOS(1).LE.IWIN(3,3))
     *         .AND. (RPOS(2).GE.IWIN(2,3)) .AND.
     *         (RPOS(2).LE.IWIN(4,3))) IMTYPE = 3
            END IF
         IF ((TWOIMG) .AND.(CURPLN.GE.1) .AND. (CURPLN.LE.NZ2)) THEN
            IF ((RPOS(1).GE.IWIN(1,2)) .AND. (RPOS(1).LE.IWIN(3,2))
     *         .AND. (RPOS(2).GE.IWIN(2,2)) .AND.
     *         (RPOS(2).LE.IWIN(4,2))) IMTYPE = 2
            END IF
         IF ((RPOS(1).GE.IWIN(1,1)) .AND. (RPOS(1).LE.IWIN(3,1))
     *      .AND. (RPOS(2).GE.IWIN(2,1)) .AND.
     *       (RPOS(2).LE.IWIN(4,1))) IMTYPE = 1
         IF (IMTYPE.LE.0) GO TO 20
C                                       get coord system
         IF (IMTYPE.NE.LIMTYP) THEN
            IF (IMTYPE.EQ.4) THEN
               CALL COPY (256, CATBS2, CATBLK)
            ELSE IF (IMTYPE.EQ.3) THEN
               CALL COPY (256, CATBLS, CATBLK)
            ELSE IF (IMTYPE.EQ.2) THEN
               CALL COPY (256, CATBLP, CATBLK)
            ELSE
               CALL COPY (256, CATSAV, CATBLK)
               END IF
            CALL SETLOC (CATBLK(IIDEP), .FALSE.)
            LIMTYP = IMTYPE
            END IF
C                                       images
         IF (IMTYPE.LT.3) THEN
C                                       get image pixel coordinate
            CALL IMA2MP (PPOS, RPOS)
            LOCNUM = 1
            CALL XYVAL (RPOS(1), RPOS(2), SKY(1), SKY(2), SKY(3), IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'BAD SKY COORDINATE XYVAL - TRY AGAIN'
               IF (DOERR) CALL MSGWRT (6)
               DOERR = .FALSE.
               GO TO 20
               END IF
C                                       curvalue
            SUBR = 'YFILL'
            CALL YFILL (GRMEBG, IX0, IY0, IX1, IY1, 0, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 990
            IX = IROUND (RPOS(1))
            IY = IROUND (RPOS(2))
            IF (IMTYPE.EQ.1) THEN
               PIXVAL = IMAGE(IX,IY)
            ELSE
               PIXVAL = IMAGE2(IX,IY,CURPLN)
               END IF
            WRITE (STRING,1020) IX, IY
            IY = IY0 + 4.5 * CSIZTV(2)
            CALL YHOLD ('ONNN', IRET)
            SUBR = 'IMCHAR'
            CALL IMCHAR (GRMEBG, IX0, IY, 0, 0, STRING(:13), SCRTCH,
     *         IRET)
            IF (IRET.NE.0) GO TO 990
            IY = IY - 1.5 * CSIZTV(2)
            IF (PIXVAL.NE.FBLANK) THEN
               CALL METSCA (PIXVAL, PREFIX, EQUAL)
               WRITE (STRING,1021) PIXVAL
               CALL IMCHAR (GRMEBG, IX0, IY, 0, 0, STRING(:10), SCRTCH,
     *            IRET)
               IF (IRET.NE.0) GO TO 990
               STRING = PREFIX
               CALL H2CHR (8, 1, CATH(KHBUN), STRING(6:))
            ELSE
               STRING = 'B  BLANKED'
               CALL IMCHAR (GRMEBG, IX0, IY, 0, 0, STRING(:10), SCRTCH,
     *            IRET)
               IF (IRET.NE.0) GO TO 990
               STRING = ' '
               CALL IMCHAR (GRMEBG, IX0, IY0, 0, 0, STRING(:13), SCRTCH,
     *            IRET)
               STRING = ' '
               END IF
            IY = IY - 1.5 * CSIZTV(2)
            CALL IMCHAR (GRMEBG, IX0, IY, 0, 0, STRING(:13), SCRTCH,
     *         IRET)
            IF (IRET.NE.0) GO TO 990
            STRING = ' '
            IF (IMTYPE.EQ.2) WRITE (STRING,1022) CURPLN
            CALL IMCHAR (GRMEBG, IX0, IY0, 0, 0, STRING(:13), SCRTCH,
     *         IRET)
            IF (IRET.NE.0) GO TO 990
            CALL YHOLD ('OFFF', IRET)
C                                       spectra plots
         ELSE
            IBCHAN = CATBLK(IIWIN)
            IECHAN = CATBLK(IIWIN+2)
            X = IBCHAN + ((RPOS(1) - IWIN(1,IMTYPE)) /
     *         (IWIN(3,IMTYPE) - IWIN(1,IMTYPE))) * (IECHAN - IBCHAN)
            I = X + 0.5
            IF (IMTYPE.EQ.3) THEN
               PIXVAL = SPECTR(I)
            ELSE
               PIXVAL = SPECT2(I)
               END IF
            WRITE (STRING,1020) LASTYZ(1), LASTYZ(2)
            IY = IY0 + 4.5 * CSIZTV(2)
            CALL YHOLD ('ONNN', IRET)
            SUBR = 'IMCHAR'
            CALL IMCHAR (GRMEBG, IX0, IY, 0, 0, STRING(:13), SCRTCH,
     *         IRET)
            IF (IRET.NE.0) GO TO 990
            IY = IY - 1.5 * CSIZTV(2)
            IF (PIXVAL.NE.FBLANK) THEN
               CALL METSCA (PIXVAL, PREFIX, EQUAL)
               WRITE (STRING,1021) PIXVAL
               CALL IMCHAR (GRMEBG, IX0, IY, 0, 0, STRING(:10), SCRTCH,
     *            IRET)
               IF (IRET.NE.0) GO TO 990
               STRING = PREFIX
               CALL H2CHR (8, 1, CATH(KHBUN), STRING(6:))
            ELSE
               STRING = 'B  BLANKED'
               CALL IMCHAR (GRMEBG, IX0, IY, 0, 0, STRING(:10), SCRTCH,
     *            IRET)
               IF (IRET.NE.0) GO TO 990
               STRING = ' '
               CALL IMCHAR (GRMEBG, IX0, IY0, 0, 0, STRING(:13), SCRTCH,
     *            IRET)
               STRING = ' '
               END IF
            IY = IY - 1.5 * CSIZTV(2)
            CALL IMCHAR (GRMEBG, IX0, IY, 0, 0, STRING(:13), SCRTCH,
     *         IRET)
            IF (IRET.NE.0) GO TO 990
            WRITE (STRING,1023) I, LASTYZ(3)
            CALL IMCHAR (GRMEBG, IX0, IY0, 0, 0, STRING(:13), SCRTCH,
     *         IRET)
            IF (IRET.NE.0) GO TO 990
            CALL YHOLD ('OFFF', IRET)
            END IF
         END IF
      GO TO 20
C                                       shut down nicely
 900  CALL YCURSE ('OFFF', F, F, PPOS, QUAD, IBUT, I)
      SUBR = 'YSLECT'
      CALL YSLECT ('OFFF', GRMEBG, 0, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (NGRAPH.EQ.8) THEN
         CALL YSLECT ('OFFF', GRBLAC, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      CALL COPY (256, CATSAV, CATBLK)
      GO TO 999
C                                       error
 990  WRITE (MSGTXT,1990) IRET, SUBR
      CALL MSGWRT (8)
      CALL COPY (256, CATSAV, CATBLK)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('X=',I4,' Y=',I4)
 1021 FORMAT ('B=',F8.3)
 1022 FORMAT ('Z=',I4)
 1023 FORMAT ('Z=',I4,' R=',I4)
 1990 FORMAT ('TVSPCV ERROR',I4,' RETURNED FROM ',A)
      END
      SUBROUTINE TVSPLA (IRET)
C-----------------------------------------------------------------------
C   TVSPLA draws labels on the grey scale image(s)
C   Output:
C      IRET     I      Error return
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'TVSPC.INC'
      INTEGER   CATSAV(256), IBUFF1(MABFSS), ILAB, NZ2, IGR
      LOGICAL   DOGRID
      CHARACTER SUBR*8
      EQUIVALENCE (IBUFF1, BUFF1)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL COPY (256, CATBLK, CATSAV)
C                                       zero graphics
      SUBR = 'YZERO'
      CALL YZERO (GRLABL, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (NGRAPH.EQ.8) THEN
         CALL YZERO (GRBLAC, IRET)
         IF (IRET.NE.0) GO TO 990
         SUBR = 'YSLECT'
         CALL YSLECT ('ONNN', GRBLAC, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      SUBR = 'YSLECT'
      CALL YSLECT ('ONNN', GRLABL, 0, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       parameters, main image
      NZ2 = CATBL4(KINAX+2)
      ILAB = 7
      DOGRID = DOLABL.EQ.2
      IGR = GRLABL - NGRAY
      SUBR = 'IAXIS1'
      CALL IAXIS1 (IBUFF1, ILAB, IGR, 0, DOGRID, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       second image
      IF ((CURPLN.GE.1) .AND. (CURPLN.LE.NZ2)) THEN
         CALL COPY (256, CATBLP, CATBLK)
         LOCNUM = 1
         CALL SETLOC (CATBLK(IIDEP), .FALSE.)
         CALL IAXIS1 (IBUFF1, ILAB, IGR, 0, DOGRID, IRET)
         IF (IRET.NE.0) GO TO 990
         CALL COPY (256, CATSAV, CATBLK)
         LOCNUM = 1
         CALL SETLOC (CATBLK(IIDEP), .FALSE.)
         END IF
      GO TO 999
C                                       error
 990  WRITE (MSGTXT,1990) IRET, SUBR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('TVSPLA ERROR',I5,' FROM ROUTINE ',A)
      END
      SUBROUTINE TVPMRK (IRET)
C-----------------------------------------------------------------------
C   Place a mark on the spectra plot showing where the displayed plane
C   lies
C   Output:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'TVSPC.INC'
      CHARACTER SUBR*8
      INTEGER   IWIN(4), IXC(2), IYC(2), LXC(2,2), LYC(2,2)
      REAL      IBCHAN, IECHAN, X
      SAVE LXC, LYC
      DATA LXC, LYC /8 * 0/
C-----------------------------------------------------------------------
      IRET = 0
      IF ((CURPLN.LE.0) .OR. (CURCHN.LE.0)) GO TO 999
C                                       do the marking: simple
      IF (.NOT.TWOCUB) THEN
         SUBR = 'YZERO'
         CALL YZERO (GRSPC2, IRET)
         IF (IRET.NE.0) GO TO 990
         SUBR = 'YSLECT'
         CALL YSLECT ('ONNN', GRSPC2, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         IBCHAN = CATBLS(IIWIN)
         IECHAN = CATBLS(IIWIN+2)
         CALL COPY (4, CATBLS(IICOR), IWIN)
         IYC(1) = IWIN(2)
         IYC(2) = IWIN(4)
         IF ((CURCHN.GE.IBCHAN) .AND. (CURCHN.LE.IECHAN)) THEN
            X = (CURCHN-IBCHAN) * (IWIN(3)-IWIN(1)) / (IECHAN-IBCHAN) +
     *         IWIN(1)
            IXC(1) = X + 0.5
            IXC(2) = IXC(1)
            SUBR = 'IMVECT'
            CALL IMVECT ('ONNN', GRSPC2, 2, IXC(1), IYC, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 990
            END IF
C                                       mark on both
      ELSE
         SUBR = 'IMVECT'
         IF (LXC(1,1).GT.0) THEN
            CALL IMVECT ('OFFF', GRSPC2, 2, LXC(1,1), LYC(1,1), SCRTCH,
     *         IRET)
            IF (IRET.NE.0) GO TO 990
            LXC(1,1) = 0
            END IF
         IF (LXC(1,2).GT.0) THEN
            CALL IMVECT ('OFFF', GRSPC1, 2, LXC(1,2), LYC(1,2), SCRTCH,
     *         IRET)
            IF (IRET.NE.0) GO TO 990
            LXC(1,2) = 0
            END IF
         IBCHAN = CATBLS(IIWIN)
         IECHAN = CATBLS(IIWIN+2)
         IF ((CURCHN.GE.IBCHAN) .AND. (CURCHN.LE.IECHAN)) THEN
            CALL COPY (4, CATBLS(IICOR), IWIN)
            IYC(1) = IWIN(2)
            IYC(2) = IWIN(4)
            X = (CURCHN-IBCHAN) * (IWIN(3)-IWIN(1)) / (IECHAN-IBCHAN) +
     *         IWIN(1)
            IXC(1) = X + 0.5
            IXC(2) = IXC(1)
            CALL IMVECT ('ONNN', GRSPC2, 2, IXC(1), IYC, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 990
            LXC(1,1) = IXC(1)
            LXC(2,1) = IXC(2)
            LYC(1,1) = IYC(1)
            LYC(2,1) = IYC(2)
C                                       two overlap
            IF (APARM(6).LE.0.0) THEN
               CALL IMVECT ('ONNN', GRSPC1, 2, IXC(1), IYC, SCRTCH,
     *            IRET)
               IF (IRET.NE.0) GO TO 990
C                                       separated
            ELSE
               IBCHAN = CATBS2(IIWIN)
               IECHAN = CATBS2(IIWIN+2)
               CALL COPY (4, CATBS2(IICOR), IWIN)
               IYC(1) = IWIN(2)
               IYC(2) = IWIN(4)
               X = (CURCHN-IBCHAN) * (IWIN(3)-IWIN(1)) / (IECHAN-IBCHAN)
     *            + IWIN(1)
               IXC(1) = X + 0.5
               IXC(2) = IXC(1)
               SUBR = 'IMVECT'
               CALL IMVECT ('ONNN', GRSPC1, 2, IXC(1), IYC, SCRTCH,
     *            IRET)
               IF (IRET.NE.0) GO TO 990
               LXC(1,2) = IXC(1)
               LXC(2,2) = IXC(2)
               LYC(1,2) = IYC(1)
               LYC(2,2) = IYC(2)
               END IF
            END IF
         END IF
      GO TO 999
C
 990  WRITE (MSGTXT,1990) IRET, SUBR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('TVPMRK ERROR',I5,' ON TV FUNCTION ',A)
      END
      SUBROUTINE TVSPSL (IRET)
C-----------------------------------------------------------------------
C   TVSPSL takes the spectrum (with any Gaussian fit) and writes it to
C   a slice file
C   Outputs:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'TVSPC.INC'
      INTEGER   CATSAV(256), IWBLK(512), NRPBLK, NREC, INOSL, IVER, LUN,
     *   IND, IP, IREC, KREC, I, J, K
      REAL      RWBLK(512), DELTA, DMAX, DMIN
      DOUBLE PRECISION DWBLK(256)
      HOLLERITH HWBLK(512)
      CHARACTER UNITS*16
      EQUIVALENCE (DWBLK, RWBLK, IWBLK, HWBLK)
      DATA LUN /73/
C-----------------------------------------------------------------------
C                                       get cube's real header
      CALL COPY (256, CATBLK, CATSAV)
      CALL COPY (256, CATBL2, CATBLK)
C                                       init SL
      CALL FNDEXT ('SL', CATBLK, IVER)
      IVER = IVER + 1
      NRPBLK = 256
      INOSL = CATBLK(KINAX)
      NREC = (INOSL - 1) / NRPBLK + 3
      IF (NGAUSS.GT.0) NREC = NREC + 1
      CALL FILL (256, 0, IWBLK)
      CALL EXTINI ('WRIT', 'SL', VOL2, SLOT2, IVER, CATBLK, LUN, IND,
     *   NRPBLK, NREC, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT SL FILE'
         GO TO 990
         END IF
      WRITE (MSGTXT,1005) IVER
      CALL MSGWRT (3)
C                                       update header record
      CALL ZFIO ('READ', LUN, IND, 1, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ INITIAL SL HEADER'
         GO TO 990
         END IF
      CALL CHR2H (6, TSKNAM, 1, HWBLK(30))
      CALL ZDATE (IWBLK(33))
      CALL ZTIME (IWBLK(36))
      IWBLK(57) = INOSL
      IF (NGAUSS.GT.0) THEN
         IWBLK(58) = 1
      ELSE
         IWBLK(58) = 0
         END IF
      IWBLK(59) = NREC
      CALL ZFIO ('WRIT', LUN, IND, 1, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'UPDATE SL HEADER'
         GO TO 990
         END IF
C                                       Put inputs in 2nd record.
      CALL FILL (256, 0, IWBLK)
      CALL CHR2H (6, TSKNAM, 1, HWBLK(1))
      CALL ZDATE (IWBLK(4))
      CALL ZTIME (IWBLK(7))
      IWBLK(10) = 23
C                                       fake up SLICE adverbs
      RWBLK(11) = NLUSER
      CALL RCOPY (7, XNAMI2, RWBLK(12))
      CALL RFILL (14, 1.0, RWBLK(19))
      RWBLK(20) = LASTYZ(1)
      RWBLK(21) = LASTYZ(2)
      RWBLK(26) = INOSL
      RWBLK(27) = LASTYZ(1)
      RWBLK(28) = LASTYZ(2)
      CALL CHR2H (4, 'AVER', 1, HWBLK(33))
      CALL ZFIO ('WRIT', LUN, IND, 2, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE RECORD 2 IN SL FILE'
         GO TO 990
         END IF
      KREC = (INOSL - 1) / NRPBLK + 3
      IP = 1
      DMAX = -1.E10
      DMIN = 1.E10
      DO 20 IREC = 3,KREC
         DO 15 K = 1,256
            RWBLK(K) = SPECTR(IP)
            IF (SPECTR(IP).NE.FBLANK) THEN
               DMIN = MIN (DMIN, SPECTR(IP))
               DMAX = MAX (DMAX, SPECTR(IP))
               END IF
            IP = IP + 1
 15         CONTINUE
         CALL ZFIO ('WRIT', LUN, IND, IREC, IWBLK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE DATA RECORD IN SL FILE'
            GO TO 990
            END IF
 20      CONTINUE
C                                       Gaussian fit
      IF (NGAUSS.GT.0) THEN
         IREC = KREC + 1
         CALL FILL (256, 0, IWBLK)
         CALL RCOPY (24, GAUSS, RWBLK(1))
         DO 25 I = 1,JJC
            RWBLK(82+I) = GAUSS(12+I,1)
            RWBLK(85+I) = GAUSS(12+I,2)
 25         CONTINUE
         IWBLK(25) = IDROP(1)
         IWBLK(26) = IDROP(2)
         IWBLK(27) = NGAUSS
         CALL ZDATE (IWBLK(28))
         CALL ZTIME (IWBLK(31))
         RWBLK(46) = GRMS
         J = 50
         DELTA = CATR(KRCIC)
         DO 30 I = 1,NGAUSS
            K = (I - 1) * 3 + 1
            RWBLK(J+1) = GAUSS(K,1)
            RWBLK(J+2) = GAUSS(K,2)
            RWBLK(J+3) = (GAUSS(K+1,1) - CATR(KRCRP)) * DELTA +
     *         CATD(KDCRV)
            RWBLK(J+4) = GAUSS(K+1,2) * ABS(DELTA)
            RWBLK(J+5) = GAUSS(K+2,1) * ABS(DELTA)
            RWBLK(J+6) = GAUSS(K+2,2) * ABS(DELTA)
            J = J + 6
 30         CONTINUE
         UNITS = ' '
         CALL H2CHR (8, 1, CATH(KHBUN), UNITS)
         CALL CHR2H (16, UNITS, 1, HWBLK(75))
         UNITS = ' '
         CALL H2CHR (8, 1, CATH(KHCTP), UNITS)
         CALL CHR2H (16, UNITS, 1, HWBLK(79))
         CALL ZFIO ('WRIT', LUN, IND, IREC, IWBLK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE MODEL RECORD IN SL FILE'
            GO TO 990
            END IF
         END IF
C                                       update record 2
C                                       update header record
      CALL ZFIO ('READ', LUN, IND, 2, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ SL RECORD 2'
         GO TO 990
         END IF
      RWBLK(34) = DMIN
      RWBLK(35) = DMAX
      CALL ZFIO ('WRIT', LUN, IND, 2, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'UPDATE SL RECORD 2'
         GO TO 990
         END IF
      CALL ZCLOSE (LUN, IND, I)
      GO TO 995
C
 990  CALL MSGWRT (8)
C
 995  CALL COPY (256, CATBLK, CATBL2)
      CALL COPY (256, CATSAV, CATBLK)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVSPSL ERROR',I5,' ON ',A)
 1005 FORMAT ('Created SLice file version',I5)
      END
      SUBROUTINE TVSFIT (IRET)
C-----------------------------------------------------------------------
C   TVSFIT asks the user to point at the Gaussian peaks and half-widths
C   and then attempts a Gaussian fit.
C   Outputs:
C      IRET   I   > 0 -> hard error, -1 -> fit fails
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'TVSPC.INC'
      INTEGER   QUAD, BUTTON, NPTS, IPVT(15), WSIZE, I, INFO, J, INPARM,
     *   N, IBC, IEC
      REAL      RPOS(2), X, Y, W, XSC, YSC
      DOUBLE PRECISION PARMS(15), TOL, FJAC(15,15), FVEC(MAXIMG),
     *   WORK(MAXIMG+200), EPARMS(15), FNORM, ENORM
      EXTERNAL GFUNC
      CHARACTER SUBR*8, BLTYPE(3)*9
      DATA BLTYPE /'Constant', 'Slope', 'Curvature'/
C-----------------------------------------------------------------------
      NGAUSS = 0
      CALL RFILL (30, 0.0, GAUSS)
      CALL YZERO (GRMODL, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL YZERO (GRESID, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL DFILL (15, 0.0D0, PARMS)
C                                       Open, read, close Tektronix.
 10   WRITE (MSGTXT,1010) NGAUSS+1
      CALL MSGWRT (1)
      WRITE (MSGTXT,1011)
      CALL MSGWRT (1)
      CALL YCURSE ('READ', .TRUE., .TRUE., RPOS, QUAD, BUTTON, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ CURSOR'
         GO TO 990
         END IF
      IF (BUTTON.LT.4) THEN
         IF ((RPOS(1).LT.CATBLS(IICOR)) .OR.
     *      (RPOS(1).GT.CATBLS(IICOR+2)) .OR.
     *      (RPOS(2).LT.CATBLS(IICOR+1)) .OR.
     *      (RPOS(2).GT.CATBLS(IICOR+3))) THEN
            MSGTXT = 'CURSOR NOT IN SLICE'
            CALL MSGWRT (6)
            GO TO 10
         ELSE
            X = (CATBLS(IIWIN+2) - CATBLS(IIWIN)) *
     *         (RPOS(1) - CATBLS(IICOR)) /
     *         (CATBLS(IICOR+2) - CATBLS(IICOR)) + CATBLS(IIWIN)
            Y = (CATRS(IRRAN+1) - CATRS(IRRAN)) *
     *         (RPOS(2) - CATBLS(IICOR+1)) /
     *         (CATBLS(IICOR+3) - CATBLS(IICOR+1)) + CATRS(IRRAN)
            END IF
         WRITE (MSGTXT,1012) NGAUSS+1
         CALL MSGWRT (1)
         CALL YCURSE ('READ', .TRUE., .TRUE., RPOS, QUAD, BUTTON, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ CURSOR'
            GO TO 990
            END IF
         IF (BUTTON.LT.4) THEN
            IF ((RPOS(1).LT.CATBLS(IICOR)) .OR.
     *         (RPOS(1).GT.CATBLS(IICOR+2)) .OR.
     *         (RPOS(2).LT.CATBLS(IICOR+1)) .OR.
     *         (RPOS(2).GT.CATBLS(IICOR+3))) THEN
               MSGTXT = 'CURSOR NOT IN SLICE'
               CALL MSGWRT (6)
               GO TO 10
            ELSE
               W = (CATBLS(IIWIN+2) - CATBLS(IIWIN)) *
     *            (RPOS(1) - CATBLS(IICOR)) /
     *            (CATBLS(IICOR+2) - CATBLS(IICOR)) + CATBLS(IIWIN)
               W = 2.0 * ABS (X - W)
               J = 3 * NGAUSS
               PARMS(J+1) = Y
               PARMS(J+2) = X
               PARMS(J+3) = W
               NGAUSS = NGAUSS + 1
               IF (NGAUSS.LT.4) GO TO 10
               END IF
            END IF
         END IF
      IF (NGAUSS.LE.0) THEN
         MSGTXT = 'No Gaussians specified, return to menu'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       have read in comps
      IBC = CATBLS(IIWIN)
      IEC = CATBLS(IIWIN+2)
      N = IEC - IBC + 1
      IDROP(1) = IBC - 1
      IDROP(2) = CATBL2(KINAX) - IEC
      JJC = APARM(9) + 0.1
      JJC = MAX (0, MIN (3, JJC))
      XCEN = (IBC + IEC) / 2.0
      TOL = 1.D-5
      ITTER = 0
      NPTS = 0
      DO 110 I = IBC,IEC
         IF (SPECTR(I).NE.FBLANK) THEN
            NPTS = NPTS + 1
            SLDATA(NPTS) = SPECTR(I)
            SLPOS(NPTS) = I
            END IF
 110     CONTINUE
C                                       Fit gaussians using guess.
      INPARM = 3 * NGAUSS + JJC
      WSIZE = MAXIMG+200
      CALL LMSTR1 (GFUNC, NPTS, INPARM, PARMS, FVEC, FJAC, 15,
     *   TOL, INFO, IPVT, WORK, WSIZE)
      IF ((INFO.LT.1) .OR. (INFO.GT.3)) THEN
         WRITE (MSGTXT,1110) INFO
         IRET = -1
         NGAUSS = 0
         GO TO 990
         END IF
C                                       errors
      FNORM = ENORM (NPTS, FVEC)
      GRMS = FNORM
      CALL GETERR (IPVT, FJAC, EPARMS, INPARM, NPTS, 15, FNORM, WORK,
     *   TOL)
C                                       squirrel away answers
      J = 3*NGAUSS
      DO 120 I = 1,J
         GAUSS(I,1) = PARMS(I)
         GAUSS(I,2) = EPARMS(I)
 120     CONTINUE
      DO 121 I = 1,JJC
         GAUSS(12+I,1) = PARMS(J+I)
         GAUSS(12+I,2) = EPARMS(J+I)
 121     CONTINUE
C                                       display results
      WRITE (MSGTXT,1120)
      CALL MSGWRT (4)
      J = 0
      DO 130 I = 1,NGAUSS
         WRITE (MSGTXT,1121) I, GAUSS(J+1,1), GAUSS(J+1,2),
     *      GAUSS(J+2,1), GAUSS(J+2,2), GAUSS(J+3,1), GAUSS(J+3,2)
         CALL MSGWRT (4)
         J = J + 3
 130     CONTINUE
      DO 135 I = 1,JJC
         WRITE (MSGTXT,1122) BLTYPE(I), GAUSS(12+I,1), GAUSS(12+I,2)
         CALL MSGWRT (4)
 135     CONTINUE
C                                       plot model, residual
      DO 150 I = IBC,IEC
         X = 0.0
         DO 140 J = 1,INPARM,3
            Y = 2.7720 * (I - GAUSS(J+1,1))**2
            W = GAUSS(J+2,1)**2
            X = X + GAUSS(J,1) * EXP (-Y/W)
 140        CONTINUE
         IF (JJC.GT.0) X = X + GAUSS(13,1) + GAUSS(14,1) * (I-XCEN) +
     *      GAUSS(15,1) * (I-XCEN) * (I-XCEN)
         SLDATA(I) = X
 150     CONTINUE
      SUBR = 'YSLECT'
      CALL YSLECT ('ONNN', GRMODL, 0, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 980
      CALL YSLECT ('ONNN', GRESID, 0, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 980
      SUBR = 'YZERO'
      CALL YZERO (GRMODL, IRET)
      IF (IRET.NE.0) GO TO 980
      CALL YZERO (GRESID, IRET)
      IF (IRET.NE.0) GO TO 980
      XSC = REAL (CATBLS(IICOR+2) - CATBLS(IICOR)) /
     *   REAL (CATBLS(IIWIN+2) - CATBLS(IIWIN))
      YSC = REAL (CATBLS(IICOR+3) - CATBLS(IICOR+1)) /
     *   (CATRS(IRRAN+1) - CATRS(IRRAN))
      DO 160 I = IBC,IEC
         J = I - IBC + 1
         X = XSC * (I - IBC) + CATBLS(IICOR)
         Y = YSC * (SLDATA(I) - CATRS(IRRAN)) + CATBLS(IICOR+1)
         IBUFF(J) = X + 0.5
         JBUFF(J) = Y + 0.5
         JBUFF(J) = MAX (CATBLS(IICOR+1), MIN (CATBLS(IICOR+3),
     *      JBUFF(J)))
 160     CONTINUE
      CALL IMVECT ('ONNN', GRMODL, N, IBUFF, JBUFF, SCRTCH, IRET)
      SUBR = 'IMVECT'
      IF (IRET.NE.0) GO TO 980
      DO 170 I = IBC,IEC
         J = I - IBC + 1
         Y = YSC * (SPECTR(I) - SLDATA(I) - CATRS(IRRAN)) +
     *      CATBLS(IICOR+1)
         JBUFF(J) = Y + 0.5
         JBUFF(J) = MAX (CATBLS(IICOR+1), MIN (CATBLS(IICOR+3),
     *      JBUFF(J)))
 170     CONTINUE
      CALL IMVECT ('ONNN', GRESID, N, IBUFF, JBUFF, SCRTCH, IRET)
      SUBR = 'IMVECT'
      IF (IRET.NE.0) GO TO 980
      GO TO 999
C
 980  WRITE (MSGTXT,1980) IRET, SUBR
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVSFIT: ERROR',I5,' ON ',A)
 1010 FORMAT ('Position cursor at peak of Gaussian',I2)
 1011 FORMAT ('Hit buttons A or B to continue, hit C or D to quit')
 1012 FORMAT ('Position cursor at half-power of Gaussian',I2)
 1110 FORMAT ('FITTING ROUTINE RETURNS',I4,' RETURN TO MENU')
 1120 FORMAT ('N',4X,'Peak',6X,'+-',9X,'Center',4X,'+-',7X,'Width',
     *   2X,'+-')
 1121 FORMAT (I1,F12.6,F10.6,F10.3,F8.3,F9.3,F7.3)
 1122 FORMAT ('BL ',A9,2X,1PE13.4,' +-',1PE10.2)
 1980 FORMAT ('TVSFIT ERROR',I4,' ON TV FUNCTION ',A)
      END
      SUBROUTINE GFUNC (M, N, PARMS, FVEC, FJROW, IFLAG)
C-----------------------------------------------------------------------
C   This routine is called by the Argonne package to calculate the
C   difference between the current fit and the actual data OR the
C   Jacobian for this difference.
C   Inputs:
C      M        I      Number of data points in slice (adj. array.dim.).
C      N        I      No. of parameters (adj. array. dim. NGAUSS * 3 + JJC)
C      PARMS    D(N)   Parameters of Gaussian components,
C                         GMAX(1), GPOS(1), GWIDTH(1), GMAX(2), ...
C      IFLAG    I      1=calculate difference for current guess.
C                      2=calculate jacobian for current guess.
C   Common:
C      DATA     R(*)   Original slice data points.
C      ITTER    I      number of calls to evaluate FVEC.
C   Outputs:
C     FVEC     D(M)   Slice data points minus data points evaluated for
C                     current guess.
C     FJROW    D(N)   Row (IFLAG - 1) of Jacobian.
C-----------------------------------------------------------------------
      INTEGER   N, M, IFLAG
      DOUBLE PRECISION PARMS(N), FVEC(M), FJROW(N)
C
      DOUBLE PRECISION AMP, POS, SIG, EFACT, RES2, TSIG2, X
      INTEGER   IGAUSS, IDATA, IAMP, IPOS, ISIG, K, NGAUS
      INCLUDE 'TVSPC.INC'
C-----------------------------------------------------------------------
C                                       Determine difference between
C                                       data and current fit.
      NGAUS = N - JJC
      IF (IFLAG.LE.1) THEN
         ITTER = ITTER + 1
         DO 20 IDATA = 1,M
            FVEC(IDATA) = SLDATA(IDATA)
            DO 15 IGAUSS = 3,NGAUS,3
               IAMP = IGAUSS-2
               IPOS = IGAUSS-1
               ISIG = IGAUSS
               AMP = PARMS(IAMP)
               POS = PARMS(IPOS)
               SIG = PARMS(ISIG)
               X = SLPOS(IDATA)
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
            X = SLPOS(IDATA)
            RES2 = 2.772 * (X - POS)**2
            TSIG2 = SIG**2
            EFACT = EXP (-1.D0 * RES2 / TSIG2)
            FJROW(IAMP) = 0.0D0
            FJROW(IAMP) = -EFACT
            FJROW(IPOS) = 0.0D0
            FJROW(IPOS) = -5.544D0 * AMP * EFACT * (X - POS) / (SIG*SIG)
            FJROW(ISIG) = 0.0D0
            FJROW(ISIG) = -2.D0 * AMP * EFACT * RES2 / (SIG ** 3)
 110        CONTINUE
         IF (JJC.GE.1) FJROW(NGAUS+1) = -1.0
         IF (JJC.GE.2) FJROW(NGAUS+2) = -(IDATA - XCEN)
         IF (JJC.GE.3) FJROW(NGAUS+3) = -(IDATA - XCEN) * (IDATA - XCEN)
         END IF
C
 999  RETURN
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
