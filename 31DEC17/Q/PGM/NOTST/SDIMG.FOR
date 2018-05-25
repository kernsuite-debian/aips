LOCAL INCLUDE 'SDIMG.INC'
C                                       Local include for SDIMG
      INCLUDE 'INCS:DSDG.INC'
      INTEGER   BIGONE
      PARAMETER (BIGONE = 4*NUMBFS + NUMBUF*NUMBFS + 2*UVBFSS)
C
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, LRECI, NRPRMI,
     *   IWT, UNFBOX, MODE, NUMKEP, NUMOFF, INDL(NUMBUF), MBUFSZ,
     *   OBUFSZ, SBUFSZ, LUNL(NUMBUF), ISIZE, SCRTCH(512)
      LOGICAL   ISOLD, ISOLDU, NOSHFT, SORTOK, ISMALL
      CHARACTER NAMEIN*12, CLAIN*6, PROJ*4, NAMOUT*12, CLAOUT*6,
     *   NATWT*4, STOK*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XPROJ, XNAMOU(3), XCLAOU(2),
     *   XSTOK, XUVFUN
      REAL      XSIN, XDISIN, XDC, XGU, XFV, XBIF, XBCHAN, XECHAN,
     *   XSOUT, XDISO, APARM(10), XIMS(2), CELLSZ(2), ROTATE, XSHIFT,
     *   YSHIFT, XUVBOX, RWT(2), XTYPE, YTYPE, XPARMS(10), YPARMS(10),
     *   XBAD(10), XFLD, YFLD, XSH, YSH, MAXCWT, XNLIM, BUFFER(BIGONE)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XDC, XGU, XFV,
     *   XSTOK, XBIF, XBCHAN, XECHAN, XNAMOU, XCLAOU, XSOUT, XDISO,
     *   XPROJ, APARM, XIMS, CELLSZ, ROTATE, XSHIFT, YSHIFT, XUVFUN,
     *   XUVBOX, RWT, XTYPE, YTYPE, XPARMS, YPARMS, XBAD
      COMMON /SDINFO/ ISOLD, ISOLDU, SEQIN, SEQOUT, DISKIN, DISKO,
     *   LRECI, NRPRMI, UNFBOX, MODE, XFLD, YFLD, NOSHFT, XSH, YSH,
     *   NUMKEP, NUMOFF, INDL, MAXCWT, SORTOK, JBUFSZ, MBUFSZ, OBUFSZ,
     *   SBUFSZ, IWT, LUNL, ISMALL, ISIZE, XNLIM
      COMMON /CHRCOM/ NAMEIN, CLAIN, PROJ, NAMOUT, CLAOUT, NATWT, STOK
      REAL      BUFF1(UVBFSS), BUFF2(UVBFSS), BUFFM(4*NUMBFS),
     *   OUTBUF(NUMBFS,NUMBUF)
      COMMON /BUFGSD/ BUFFM, BUFF1, BUFF2, OUTBUF, SCRTCH
      EQUIVALENCE (BUFFER(1), BUFFM)
LOCAL END
      PROGRAM SDIMG
C-----------------------------------------------------------------------
C! Selects, projects and grids single-dish uv-like data
C# Sdish Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2005-2008, 2011-2012, 2015
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
C   SDIMG selects randomly sampled single-dish data in a uv format and
C   changes the positions to be relative to a specified position.  It
C   then convolves the data (up to 8 channels at a time) to an image.
C*****  SDIMG is the old SDGRD renamed to preserve its ability to do
C*****  large images.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      DOCALIB        XDC           If true calibrate
C      GAINUSE        XGU           CS table number
C      FLAGVER        XFV           Flag table version
C      STOKES         STOK          Stokes to use
C      BIF            XBIF          IF to select
C      BCHAN          XBCHAN        First spectral channel
C      ECHAN          XECHAN        Last spectral channel
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      OPTYPE         PROJ          Projection code.
C      APARM(10)      APARM         User specified array.
C                                   1,2,3=RA (h,m,s) of field center
C                                   3,4,5=Dec (d,m,s)
C                                   6,7=field of view ( arc sec)
C      IMSIZE         NX,NY            image size (pixels)
C      SHIFT          XSHIFT,YSHIFT    shift (asec)
C      UVWTFN         NATWT            'UN' weighting function
C      UVBOX          UNFBOX           box size for weighting
C      REWEIGHT       RWT              (1) interp, convolved, weight out
C                                      (2) min weight
C      XTYPE          CTYPX            5=Spheriodal - X grid function
C      YTYPE          CTYPY            XTYPE - Y grid function
C      XPARM          CPARMX           set in GRDFLT - X func parms
C      YPARM          CPARMY           set in GRDFLT - Y func parms
C      BADDISK        IBAD             disks to avoid
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SDIMG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'SDIMG '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL SDIMGI (PRGM, IRET)
C                                       project data to scratch
      IF (IRET.EQ.0) CALL SDIMGP (IRET)
C                                       large images require sort..
      IF (.NOT.ISMALL) THEN
C                                       sort data
         IF (IRET.EQ.0) CALL SDIMGS (IRET)
C                                       weight data
         IF (IRET.EQ.0) CALL SDIMGW (IRET)
C                                       grid data
         IF (IRET.EQ.0) CALL SDIMGG (IRET)
C                                       Unsorted fits in AP core
      ELSE
C                                       weight data
         IF (IRET.EQ.0) CALL SDIMSW (IRET)
C                                       grid data
         IF (IRET.EQ.0) CALL SDIMSG (IRET)
         END IF
C                                       fill in history file
      IF (IRET.EQ.0) CALL SDIMGH (IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE SDIMGI (PRGN, IRET)
C-----------------------------------------------------------------------
C   SDIMGI gets input parameters for SDIMG and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6   Program name
C   Output:
C      IRET    I     Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in SDIMG for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      CHARACTER STAT*4, UTYPE*2, RSTOKE(8)*4, RP1*4, RP2*4
      HOLLERITH CATH(256), CATUVH(256)
      INTEGER   IDDEG, NPARM, IERR, IROUND, OLDCNO, IIVER, NIF, I, M, N,
     *   WRK(3), DEPTH(5), APSIZ, INDEX, NEED, KAP
      REAL      CATR(256), CATUVR(256), XXROT, XPIX, YPIX, BEMSZ(2)
      DOUBLE PRECISION CATD(128), CATUVD(128), XDEC, XRA
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   ISBAND(MAXIF)
      DOUBLE PRECISION FOFF(MAXIF)
      REAL      FINC(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'SDIMG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DLOC.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      EQUIVALENCE (CATUV, CATUVR, CATUVH, CATUVD)
      DATA T /.TRUE./
      DATA RSTOKE /'I','Q','U','V','RR','LL','XX','YY'/
      DATA DEPTH /5*1/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T, SCRTCH)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      MBUFSZ = 4 * NUMBFS * 2
      OBUFSZ = NUMBFS * 2
      SBUFSZ = BIGONE * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 75
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XPROJ, PROJ)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XUVFUN, NATWT)
      CALL RCOPY (10, XPARMS, XPARM)
      CALL RCOPY (10, YPARMS, YPARM)
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   UTYPE, NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR
         GO TO 990
         END IF
C                                       Bad disks.
      DO 20 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
         SCRVOL(I) = 0
 20      CONTINUE
      DO 25 I = 1,NUMBUF
         LUNL(I) = 15 + I
         IF (LUNL(I).GT.30) LUNL(I) = LUNL(I) + 10
 25      CONTINUE
C                                       Get input info
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      NRPRMI = NRPARM
      LRECI = LREC
      ISOLD = TYPUVD.EQ.0
      ISOLDU = ISOLD .AND. (ILOCW.LT.0)
C                                       Warn if interferometer data
      IF (TYPUVD.LE.0) THEN
         MSGTXT = 'WARNING: DATA APPEARS TO BE INTERFEROMETER TYPE'
         CALL MSGWRT (6)
         END IF
C                                       Setup for SDGET
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      CALL COPY (256, CATBLK, CATUV)
C                                       freq and IF selection
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, BCHAN)
      IF (BCHAN.GT.CATBLK(KINAX+JLOCF)) THEN
         WRITE (MSGTXT,1020) 'BCHAN', BCHAN, CATBLK(KINAX+JLOCF)
         GO TO 990
         END IF
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      IF (ECHAN.GT.CATBLK(KINAX+JLOCF)) THEN
         WRITE (MSGTXT,1020) 'ECHAN', ECHAN, CATBLK(KINAX+JLOCF)
         GO TO 990
         END IF
      XBCHAN = BCHAN
      XECHAN = ECHAN
      BIF = IROUND (XBIF)
      BIF = MAX (1, BIF)
      IF (JLOCIF.LT.0) BIF = 1
      IF ((JLOCIF.GE.0) .AND. (BIF.GT.CATBLK(KINAX+JLOCIF))) THEN
         WRITE (MSGTXT,1020) 'BIF', BIF, CATBLK(KINAX+JLOCIF)
         GO TO 990
         END IF
      EIF = BIF
      XBIF = BIF
      IIVER = 1
      CALL CHNDAT ('READ', SCRTCH, DISKIN, OLDCNO, IIVER, CATBLK,
     *   LUNL(2), NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                  Set Stokes code (MODE)
      CALL H2CHR (4, 1, XSTOK, STOK)
      MODE = 1
      DO 30 I = 1,8
         IF (RSTOKE(I).EQ.STOK) MODE = I
 30      CONTINUE
      STOKES = RSTOKE(MODE)
C                                       caibration parameters
      DOCAL = XDC.GT.0.0
      FGVER = IROUND (XFV)
      CLVER = IROUND (XGU)
      CLUSE = IROUND (XGU)
C                                       Set gridding function defaults.
      CXTYPE = IROUND (XTYPE)
      CYTYPE = IROUND (YTYPE)
      BEMSZ(1) = CATUVR(KRBMJ) * 3600.
      BEMSZ(2) = CATUVR(KRBMN) * 3600.
      CALL GRDFIX (CXTYPE, CYTYPE, XPARM, YPARM, CELLSZ, BEMSZ)
C                                       imaging parms
      IF ((RWT(1).GT.0.0001) .AND. (RWT(1).LT.1.50)) THEN
         IWT = 0
      ELSE IF (ABS(RWT(1)-2.0).LE.0.5) THEN
         IWT = 1
      ELSE IF (ABS(RWT(1)-3.0).LE.0.5) THEN
         IWT = 2
      ELSE
         IWT = -1
         IF (RWT(2).EQ.0.0) RWT(2) = -0.01
         END IF
      UNFBOX = IROUND (XUVBOX)
      NX = IROUND (XIMS(1))
      NY = IROUND (XIMS(2))
      IF ((NX.LT.32) .OR. (NX.GT.MAXIMG) .OR. ((NX/2)*2.NE.NX) .OR.
     *   (NY.LT.32) .OR. (NY.GT.MAXIMG) .OR. ((NY/2)*2.NE.NY)) THEN
         WRITE (MSGTXT,1030) NX, NY
         GO TO 990
         END IF
C                                       Size of problem
      IF (XTYPE.LE.10) THEN
         M = MAX (XPARM(1), 1.0) + 0.1
         N = MAX (YPARM(1), 1.0) + 0.1
         APSIZ = 100 * (2*N+1) + 100*(2*M+1) + 300
      ELSE
         M = MAX (XPARM(1), 1.0) + 0.1
         N = MAX (XPARM(5), 4.0) + 0.1
         M = N * (2 * M + 1)
         APSIZ = M*M + 300
         END IF
      NEED = APSIZ + (ECHAN - BCHAN + 1) * (300 + 2 * NX * NY)
      NEED = NEED / 1024
      MSGSUP = 32000
      CALL QINIT (NEED, 0, KAP)
      MSGSUP = 0
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = APSIZ + 2 * (300 + 2 * NX * NY)
         NEED = NEED / 1024
         NEED = MIN (32 * 1024, NEED) + 4
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            NEED = 5 * 1024
            CALL QINIT (NEED, 0, KAP)
            IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
               MSGTXT = 'SDIMGI: CANNOT GET ANY USEFUL MEMORY'
               GO TO 990
               END IF
            END IF
         END IF
      APSIZ = 1024 * PSAPNW - APSIZ
      CALL QRLSE
      I = APSIZ / (300 + 2 * NX * NY)
      ISMALL = (I.GT.1) .AND. (APARM(10).LT.31999.)
C                                       Select ra, dec range
      XFLD = CELLSZ(1) * NX
      YFLD = CELLSZ(2) * NY
      UVRA(1) = XFLD / 3600.
      UVRA(2) = YFLD / 3600.
C                                       Set for shift
      NOSHFT = (XSHIFT.EQ.0.0) .AND. (YSHIFT.EQ.0.0)
C                                       Set position.
      XRA = (APARM(1) + APARM(2)/60. + APARM(3)/3600.) * 15.0
      IDDEG = ABS (APARM(4))
      XDEC = (IDDEG + APARM(5)/60. + (APARM(6)/3600.)) *
     *   SIGN (1.0, APARM(4))
C                                       0,0 normal for rel az/el
      IF (TYPUVD.NE.3) THEN
         IF ((XRA.EQ.0.0D0) .AND. (XDEC.EQ.0.0D0)) THEN
            XRA = RA
            XDEC = DEC
            END IF
         IF ((XRA.EQ.0.0D0) .AND. (XDEC.EQ.0.0D0)) THEN
            XRA = CATUVD(KDORA)
            XDEC = CATUVD(KDODE)
            END IF
         END IF
      RA = XRA
      DEC = XDEC
C                                       Get header shift and coor.
C                                       offset.
      CALL ROTFND (CATUVR, XXROT, IERR)
      XXROT = XXROT + ROTATE
      XSH = (COS (XXROT) * XSHIFT - SIN (XXROT) * YSHIFT) / 3600.
      YSH = (SIN (XXROT) * XSHIFT + COS (XXROT) * YSHIFT) / 3600.
C                                       Mark as projected posn.
C                                       check coords match
      IF (PROJ.EQ.' ') PROJ = '-SIN'
      INDEX = KHPTP + ILOCU * 2
      CALL H2CHR (4, 1, CATH(INDEX), RP1)
      IF (RP1(3:3).EQ.' ') RP1(3:3) = '-'
      IF (RP1(4:4).EQ.' ') RP1(4:4) = '-'
      INDEX = KHCTP + JLOCR * 2
      CALL H2CHR (4, 1, CATH(INDEX), STAT)
      IF (STAT(3:3).EQ.' ') STAT(3:3) = '-'
      IF (STAT(4:4).EQ.' ') STAT(4:4) = '-'
      IF (STAT.NE.RP1) THEN
         MSGTXT = 'COORDINATE AND RANDOM PARAMETER ARE NOT SAME'
         IRET = 10
         GO TO 990
         END IF
      INDEX = KHPTP + ILOCV * 2
      CALL H2CHR (4, 1, CATH(INDEX), RP2)
      IF (RP2(3:3).EQ.' ') RP2(3:3) = '-'
      IF (RP2(4:4).EQ.' ') RP2(4:4) = '-'
      INDEX = KHCTP + JLOCD * 2
      CALL H2CHR (4, 1, CATH(INDEX), STAT)
      IF (STAT(3:3).EQ.' ') STAT(3:3) = '-'
      IF (STAT(4:4).EQ.' ') STAT(4:4) = '-'
      IF (STAT.NE.RP2) THEN
         MSGTXT = 'COORDINATE AND RANDOM PARAMETER ARE NOT SAME'
         IRET = 10
         GO TO 990
         END IF
C                                       Construct an image header
      CALL CHR2H (4, RP1, 1, CATH(KHCTP))
      CALL CHR2H (4, PROJ, 5, CATH(KHCTP))
      CALL CHR2H (4, RP2, 1, CATH(KHCTP+2))
      CALL CHR2H (4, PROJ, 5, CATH(KHCTP+2))
      CALL CHR2H (8, 'FREQ    ', 1, CATH(KHCTP+4))
      CALL CHR2H (8, 'STOKES  ', 1, CATH(KHCTP+6))
      IF (IWT.EQ.1) CALL CHR2H (8, 'CONV WTS', 1, CATH(KHBUN))
      IF (IWT.EQ.2) CALL CHR2H (8, '/K/K', 1, CATH(KHBUN))
      CATD(KDCRV) = RA
      CATD(KDCRV+1) = DEC
      CATD(KDCRV+2) = FREQ + FOFF(BIF)
      CATD(KDCRV+3) = MODE
      IF (MODE.GT.4) CATD(KDCRV+3) = 4.0D0 - MODE
      IF (MODE.GT.6) CATD(KDCRV+3) = 2.0D0 - MODE
      CATR(KRCIC) = -XFLD / (3600. * NX)
      IF (TYPUVD.EQ.3) CATR(KRCIC) = -CATR(KRCIC)
      CATR(KRCIC+1) = YFLD / (3600. * NY)
      CATR(KRCIC+2) = FINC(BIF)
      CATR(KRCIC+3) = 1.0
      CATR(KRCRP) = NX / 2.0 - XSH / CATR(KRCIC)
      CATR(KRCRP+1) = NY / 2.0 + 1.0 - YSH / CATR(KRCIC+1)
      CATR(KRCRP+2) = CATUVR(KRCRP+JLOCF) - BCHAN + 1.0
      CATR(KRCRP+3) = 1.
      CATR(KRCRT) = 0.0
      CATR(KRCRT+1) = XXROT
      CATR(KRCRT+2) = 0.
      CATR(KRCRT+3) = 0.
      CATR(KRARP) = CATR(KRARP) - BCHAN + 1.0
      CATR(KRDMX) = 0.0
      CATR(KRDMN) = 0.0
      XSH = - XSH / COS (DEC * DG2RAD)
      CATR(KRXSH) = XSH
      CATR(KRYSH) = YSH
      CATBLK(KIGCN) = 0
      CATBLK(KIPCN) = 0
      CATBLK(KIDIM) = 4
      CATBLK(KINAX) = NX
      CATBLK(KINAX+1) = NY
      CATBLK(KINAX+2) = ECHAN - BCHAN + 1
      CATBLK(KINAX+3) = 1
      CATR(KRBLK) = FBLANK
C                                       Default output name
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
C                                       Put new values in CATBLK.
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATR(KHPTY))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      CCNO = 1
      IRET = 4
      CALL MCREAT (DISKO, CCNO, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = 2
C                                       save output names
      SEQOUT = CATBLK(KIIMS)
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMOUT)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLAOUT)
C                                       copy any keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IRET)
C                                       Interprete header
      LOCNUM = 1
C                                       determine ref pixel for
C                                       conventional ref value
      IF ((PROJ.EQ.'-GLS') .OR. (PROJ.EQ.'-AIT') .OR. (PROJ.EQ.'-MER'))
     *   THEN
         CATD(KDCRV+1) = 0.0D0
         CALL SETLOC (DEPTH, T)
         CALL XYPIX (RA, DEC, XPIX, YPIX, IRET)
         IF (IRET.EQ.0) THEN
            CATR(KRCRP) = 2.0 * CATR(KRCRP) - XPIX
            CATR(KRCRP+1) = 2.0 * CATR(KRCRP+1) - YPIX
         ELSE
            CATD(KDCRV+1) = DEC
            END IF
         END IF
      CALL SETLOC (DEPTH, T)
      CALL COPY (256, CATBLK, CATMAP)
C                                        Put input file in READ
      UTYPE = 'UV'
      STAT = 'READ'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   UTYPE, NLUSER, STAT, SCRTCH, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Create weight scratch file
      WRK(1) = NX
      WRK(2) = NY
      WRK(3) = 1
      CALL MAPSIZ (3, WRK, ISIZE)
      CALL SCREAT (ISIZE, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET, 'MA'
         GO TO 990
         END IF
C                                       Create UV scratch file
      WRK(1) = 3 + 2 * CATMAP(KINAX+2)
      WRK(2) = NVIS + 512
      CALL UVSIZE (WRK(1), WRK(2), ISIZE)
      CALL SCREAT (ISIZE, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET, 'UV'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
      IF (IRET.EQ.0) IRET = 5
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDIMGI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1015 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1020 FORMAT (A,' = ',I5,' GREATER THAN LIMIT',I5)
 1030 FORMAT ('NX, NY = ',2I5,' ARE ILLEGAL')
 1035 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1040 FORMAT ('ERROR',I4,' CREATING ',A,' SCRATCH FILE')
      END
      SUBROUTINE SDIMGP (IRET)
C-----------------------------------------------------------------------
C   SDIMGP reads the input UV data set and writes the UV scratch file
C   containing the desired data plus its image pixel location and weight
C   In/Out:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OFILE*48
      INTEGER   IPTRO, ILENBU, KBIND, NIOUT, NIOLIM, LRECO, VO, BO,
     *   NUMVIS, XCOUNT, NCHAN, I, LIMIT, IPIX, LPIX
      REAL      RPARM(12), VIS(3,MAXCIF)
      LOGICAL   T, F
      INCLUDE 'SDIMG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (OUTBUF, VIS)
      DATA VO, BO /0,1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
      MSGTXT = 'Selecting and projecting the data'
      CALL MSGWRT (2)
C                                       Init vis file for read.
      MAXCWT = 0.0
      CALL SDGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'INIT', IRET
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('SC', SCRVOL(2), SCRCNO(2), 1, OFILE, IRET)
      CALL ZOPEN (LUNL(2), INDL(2), SCRVOL(2), OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) 'OPEN', IRET
         GO TO 990
         END IF
C                                       Init vis file for write
C                                       LRECO = length of output rec.
      NCHAN = CATMAP(KINAX+2)
      LRECO = 3 + 2 * NCHAN
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNL(2), INDL(2), NVIS, VO, LRECO, ILENBU,
     *   JBUFSZ, BUFF1, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'INIT', IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
      NUMVIS = 0
      XCOUNT = 0
      LIMIT = NVIS
      SORTOK = T
      LPIX = 100000
C                                       Loop over data
      DO 190 I = 1,LIMIT
         NUMVIS = NUMVIS + 1
C                                       Get next record
         CALL SDGET ('READ', RPARM, VIS, IRET)
C                                       Check for end of data
         IF (IRET.LT.0) GO TO 200
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'READ', IRET
            GO TO 990
            END IF
C                                       Call selection routine.
         CALL SELDAT (NUMVIS, RPARM(1+ILOCU), RPARM(1+ILOCV), VIS,
     *      BUFF1(IPTRO), IRET)
C                                       Error (fatal)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
C                                       Write out data
         ELSE IF (IRET.EQ.0) THEN
            IPIX = BUFF1(IPTRO+1) + 0.50
            IF (IPIX.GT.LPIX) SORTOK = F
            LPIX = IPIX
            XCOUNT = XCOUNT + 1
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
            END IF
C                                       Write vis record.
         IF (NIOUT.GE.NIOLIM) THEN
            CALL UVDISK ('WRIT', LUNL(2), INDL(2), BUFF1, NIOLIM, KBIND,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1150) IRET
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
            END IF
 190     CONTINUE
C                                       Final call to SELDAT.
 200  NUMVIS = -1
      CALL SELDAT (NUMVIS, RPARM(1+ILOCU), RPARM(1+ILOCV), VIS,
     *   BUFF1(IPTRO), IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNL(2), INDL(2), BUFF1, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Close files
      CALL SDGET ('CLOS', BUFF1, BUFF1, IRET)
      CALL ZCLOSE (LUNL(2), INDL(2), IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDIMGP: SDGET ',A4,' ERROR ',I3)
 1010 FORMAT ('SDIMGP: ERROR',I3,1X,A,'-FOR-WRITE VIS FILE')
 1120 FORMAT ('SDIMGP: SELDAT ERROR',I3)
 1150 FORMAT ('SDIMGP: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE SELDAT (NUMVIS, U, V, VIS, OUTREC, IRET)
C-----------------------------------------------------------------------
C  SELDAT decides if data is in the field specified and changes the
C  "RA--..." and "DEC-..." random parameters to the requestd projection
C  about the specified position.
C  Inputs:
C     NUMVIS   I        Visibility number, -1.0=> final call, no data
C                       passed to allow any operations to be completed
C     U        R        RA in degrees
C     V        R        Dec in degrees
C     VIS      R(3,*)   Data array
C  Output:
C     OUTREC   R(*)     Output record: Xpix, Ypix, Wt (1),
C                          Nchan of Flux,Wt
C     IRET     I        Return code  -1 => don't write
C                          0 => OK, write
C                         >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
      REAL      U, V, VIS(3,*), OUTREC(*)
C
      REAL      XPIX, YPIX
      INTEGER   NCHAN, I, J
      DOUBLE PRECISION UU, VV, UVSCAL
      INCLUDE 'SDIMG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      SAVE UVSCAL
C-----------------------------------------------------------------------
      IF (NUMVIS.GT.0) THEN
         IF (NUMVIS.EQ.1) THEN
            NUMKEP = 0
            NUMOFF = 0
            UVSCAL = 1.0D0
            IF (ISOLDU) UVSCAL = 1.0D0 / FREQ
            END IF
         UU = U * UVSCAL
         VV = V * UVSCAL
C                                       Get position offset.
         CALL XYPIX (UU, VV, XPIX, YPIX, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET
            GO TO 990
            END IF
C                                       Does it fit?  - NO
         IF ((XPIX.LT.0.5+XPARM(1)) .OR. (XPIX.GT.NX+0.5-XPARM(1)) .OR.
     *      (YPIX.LT.0.5+YPARM(1)) .OR. (YPIX.GT.NY+0.5-YPARM(1))) THEN
            IRET = -1
            NUMOFF = NUMOFF + 1
C                                       YES - do output record
         ELSE
            OUTREC(1) = XPIX
            OUTREC(2) = YPIX
            OUTREC(3) = 1.0
            J = 4
            NCHAN = CATMAP(KINAX+2)
            DO 10 I = 1,NCHAN
               OUTREC(J) = VIS(1,I)
               OUTREC(J+1) = VIS(3,I)
               J = J + 2
 10            CONTINUE
            NUMKEP = NUMKEP + 1
            IRET = 0
            END IF
C                                       History
      ELSE
         WRITE (MSGTXT,1100) NUMKEP, NUMOFF
         CALL MSGWRT (4)
         IF (NUMKEP.LE.0.0) IRET = 9
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ',I3,' PROJECTING POSITION')
 1100 FORMAT (I9,' records kept;',I9,' others fell off the grid')
      END
      SUBROUTINE SDIMGS (IRET)
C-----------------------------------------------------------------------
C   SDIMGS sorts the data if needed to put it in descending order of the
C   Y pixel.
C   In/Out:
C      IRET     I     Error code: 0 procede, else quit
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LLREC, NBUFF, LENBU, I, SCNO(2), SVOL(2), IN, OUT, BO,
     *   NSORT, IERR, NIOUT, INIO, IBUFSZ, VO, INVIS, BIND1, BIND2, J,
     *   NVSUM, IPTR1, IPTR2, KEY1, KEY2, ILRECO, INVSUM, IBSTRT, LENIO,
     *   KOUT
      CHARACTER PHNAME*48
      LOGICAL   F, T, EOI
      INCLUDE 'SDIMG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA BO, VO /1,0/
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
      IF (SORTOK) GO TO 999
      MSGTXT = 'Sorting the data'
      CALL MSGWRT (2)
C                                       Create UV scratch files
      CALL SCREAT (ISIZE, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREAT', 'SORT SC'
         GO TO 990
         END IF
C                                       Set up parameters
      NBUFF = 4
      LLREC = 3 + 2 * CATMAP(KINAX+2)
      LENBU = (SBUFSZ - (4 * NBPS * (NBUFF+1))) /
     *   ((NBUFF+1) * LLREC * 2)
C                                       ICSORT takes <= 204800
      LENBU = MIN (204800/NBUFF, LENBU)
C                                       Set number pre sorted.
      NSORT = NBUFF * LENBU
      IN = 1
      OUT = IN + LLREC * LENBU + NBPS
      KEY1 = 2
      KEY2 = 1
C                                       Open input file.
      CALL ZPHFIL ('SC', SCRVOL(2), SCRCNO(2), 1, PHNAME, IERR)
      CALL ZOPEN (LUNL(1), INDL(1), SCRVOL(2), PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'INPUT DATA'
         GO TO 990
         END IF
C                                       Open output file.
      CALL ZPHFIL ('SC', SCRVOL(3), SCRCNO(3), 1, PHNAME, IERR)
      CALL ZOPEN (LUNL(2), INDL(2), SCRVOL(3), PHNAME, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'OUTPUT DATA'
         GO TO 990
         END IF
C                                       Initialize files, single buffer.
      INIO = LENBU
      IBUFSZ = LLREC * 2 * LENBU + 2 * NBPS
      INVIS = NUMKEP
      CALL UVINIT ('READ', LUNL(1), INDL(1), INVIS, VO, LLREC, INIO,
     *   IBUFSZ, BUFFER(IN), BO, BIND1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'INPUT DATA'
         GO TO 990
         END IF
      NIOUT = LENBU
      CALL UVINIT ('WRIT', LUNL(2), INDL(2), INVIS, VO, LLREC, NIOUT,
     *   IBUFSZ, BUFFER(OUT), BO, BIND2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'OUTPUT DATA'
         GO TO 990
         END IF
      EOI = F
C                                       Prepare pointers and counters.
C                                       Begin loop.
 100  CONTINUE
C                                       Read NBUFF buffers, add keys and
C                                       place in output buffer area.
C                                       Fill output buffer with
C                                       NBUFF input buffers.
         NVSUM = 0
         IPTR2 = OUT + BIND2 - 1
         DO 120 I = 1,NBUFF
            IF (.NOT.EOI) THEN
               CALL UVDISK ('READ', LUNL(1), INDL(1), BUFFER(IN), INIO,
     *            BIND1, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READ', 'INPUT DATA'
                  GO TO 990
                  END IF
C                                       Sum no. vis. records
               NVSUM = NVSUM + INIO
               IPTR1 = IN + BIND1 - 1
C                                       Check if all data read.
               IF (INIO.LE.0) EOI = T
               END IF
            IF (.NOT.EOI) THEN
C                                       Copy to output buffer
               DO 110 J = 1,INIO
                  CALL RCOPY (LLREC, BUFFER(IPTR1), BUFFER(IPTR2))
C                                       Update pointers.
                  IPTR1 = IPTR1 + LLREC
                  IPTR2 = IPTR2 + LLREC
 110              CONTINUE
               END IF
 120        CONTINUE
C                                       Output buffer now loaded ready
C                                       to sort.
         IF (NVSUM.GT.0) THEN
            KOUT = OUT + BIND2 - 1
            ILRECO = LLREC
            INVSUM = NVSUM
            CALL ICSORT (BUFFER(KOUT), ILRECO, INVSUM, KEY1, KEY2, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
               END IF
C                                       Dump output buffer to disk
C                                       Trick UVDISK by moving buffer
C                                       pointer adjusted to take care
C                                       of unwritten data.
            IBSTRT = BIND2 - 1
            DO 130 I = 1,NBUFF
               IF (NVSUM.LE.0) GO TO 150
               NIOUT = MIN (LENBU, NVSUM)
               NVSUM = NVSUM - NIOUT
               LENIO = (I-1) * LENBU * LLREC  +  IBSTRT
               KOUT = OUT + LENIO - MOD (LENIO, NBPS/2)
               CALL UVDISK ('FLSH', LUNL(2), INDL(2), BUFFER(KOUT),
     *            NIOUT, BIND2, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'FLSH', 'OUTPUT DATA'
                  GO TO 990
                  END IF
               IF (NIOUT.LE.0) GO TO 150
 130           CONTINUE
C                                        Copy any unwritten data
C                                        to start of buffer.
 150        IF (BIND2.GT.1) CALL RCOPY (BIND2, BUFFER(KOUT),
     *         BUFFER(OUT))
C                                       If not finished loop back
            IF (.NOT.EOI) GO TO 100
            END IF
C                                       Empty buffer.
      NIOUT = 0
      IF (BIND2.GT.1) THEN
         CALL UVDISK ('FLSH', LUNL(2), INDL(2), BUFFER(KOUT), NIOUT,
     *      BIND2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'FLSH', 'OUTPUT DATA'
            GO TO 990
            END IF
         END IF
C                                       Switch IN and OUT.
      I = IN
      IN = OUT
      OUT = I
C                                       Close files.
      CALL ZCLOSE (LUNL(2), INDL(2), IERR)
      CALL ZCLOSE (LUNL(1), INDL(1), IERR)
C                                       Now the file merger
      SCNO(1) = SCRCNO(3)
      SVOL(1) = SCRVOL(3)
      SCNO(2) = SCRCNO(2)
      SVOL(2) = SCRVOL(2)
C                                       Merge data.
      IF (INVIS.GE.NSORT) THEN
         CALL AMERGE (KEY1, KEY2, INVIS, LLREC, NSORT, NBUFF, LUNL,
     *      SVOL, SCNO, LENBU, BUFFER(IN), BUFFER(OUT), IRET)
         END IF
      IF (IRET.EQ.0) THEN
         CALL COPY (2, SCNO, SCRCNO(2))
         CALL COPY (2, SVOL, SCRVOL(2))
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDIMGS: ERROR',I4,1X,A,'ING ',A,' FILE')
 1120 FORMAT ('SDIMGS: ICSORT ERROR',I5)
      END
      SUBROUTINE SDIMGW (IRET)
C-----------------------------------------------------------------------
C   SDIMGW applies the uniform weighting correction to the weights.  The
C   visibility weights are divided by the number of visibilities
C   occuring in cells within a box of half width UNFBOX centered on the
C   cell in which a given visibility resides.
C   In/Out:
C      IRET     I     Error code: 0 okay, > 0 quit now.
C----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER PHNAME*48
      INTEGER   WIN(4), BO, VO, ILENBU, LRECIN, NVISIN, APSIZ, NERR,
     *   AMAX, AMIN, WMIN, WMAX, IX, APSCLV, KAP, NROW, GRID, ROW, ROW2,
     *   END1, END2, END3, NMOV, NUM, MAXREC, NIO, IBIND, INCNT, INPTR,
     *   I, IDATA, IDATA0, LIM, CNT, JNPTR, II, KTEMP, JNX, IY, YY, J,
     *   NPOINT, ITEMP, JBIND, LIMIT, IROW, KBIND, INIO, KROW, NIOUT,
     *   OPTR, INDEX, IER
      REAL      TEMP(10), YMIN, WT
      LOGICAL   T, F, ENDROW
      INCLUDE 'SDIMG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA WIN, IDATA0 /4*0, 10/
      DATA T, F /.TRUE., .FALSE./
      DATA BO, VO /1, 0/
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
      IF ((NATWT(:2).NE.'UN') .AND. (NATWT(:2).NE.'WT')) GO TO 999
      MSGTXT = 'Finding uniform weights'
      CALL MSGWRT (2)
      NERR = 0
C                                       Prepare for reads and writes.
C                                       Open visibility file.
      CALL ZPHFIL ('SC', SCRVOL(2), SCRCNO(2), 1, PHNAME, IRET)
      CALL ZOPEN (LUNL(1), INDL(1), SCRVOL(2), PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'UV SCRATCH', 'READ'
         GO TO 995
         END IF
C                                       Open grid file.
      CALL ZPHFIL ('SC', SCRVOL(1), SCRCNO(1), 1, PHNAME, IRET)
      CALL ZOPEN (LUNL(2), INDL(2), SCRVOL(1), PHNAME, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'GRID SCRATCH', 'WRITE'
         GO TO 995
         END IF
C                                       Init. vis file
      NVISIN = NUMKEP
      LRECIN = 3 + 2 * CATMAP(KINAX+2)
      ILENBU = 0
      CALL UVINIT ('READ', LUNL(1), INDL(1), NVISIN, VO, LRECIN, ILENBU,
     *   JBUFSZ, BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'UV SCRATCH', 'READ'
         GO TO 995
         END IF
C                                       Init. grid file.
      CALL MINIT ('WRIT', LUNL(2), INDL(2), NX, NY, WIN, BUFFM, MBUFSZ,
     *   BO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'GRID SCRATCH', 'WRITE'
         GO TO 995
         END IF
C                                       Load AP values
      JNX = NX
C                                       Maximum v.
      AMAX = 0
      TEMP(1) = NX
C                                       Minimum v.
      AMIN = 1
      TEMP(2) = 1
C                                       Minimum count.
      WMIN = 2
      TEMP(3) = 1.0
C                                       Maximum count.
      WMAX = 3
      TEMP(4) = 1.0E20
      IX = IDATA0
C                                       Scaling factors for u, v
C                                       to cells.
      APSCLV = 4
      TEMP(5) = 1.0
C                                       Grab AP - size is set already
      CALL QINIT (0, 0, KAP)
      CALL QPUT (TEMP, 0, 10, 2)
      CALL QWD
C                                       Set pointers for AP
      APSIZ = 1024 * PSAPNW
      NROW = 2 * UNFBOX + 1
      GRID = (APSIZ - 1) - NROW * NX
C                                       Set pointer for temporary row
      ROW = GRID - NX
C                                       Set pointers for output buffer
      ROW2 = ROW - NX
      END1 = ROW2 + NX - 1
C                                       Set pointer for shifting grid
      END2 = GRID + NROW * NX
      END3 = END2 - NX
C                                      Determine no. points to shift.
      NMOV = (NROW - 1) * NX
C                                       Clear AP
      NUM = NROW * NX
      CALL QVCLR (GRID, 1, NUM)
      CALL QWR
      CALL QWD
C                                       Determine max. no. of vis.
C                                       points which will fit in AP.
      MAXREC = (ROW2 - 10) / LRECIN - 5
C                                       Make sure MAXREC.GT.0
      IF (MAXREC.LE.0) THEN
         NUM = (1 - MAXREC) * LRECIN
         WRITE (MSGTXT,1010) NUM
         IRET = 1
         GO TO 995
         END IF
C                                       Read first visibility record.
      INCNT = 1
      CALL UVDISK ('READ', LUNL(1), INDL(1), BUFF1, NIO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET, 'READ', 'UV SCRATCH'
         GO TO 995
         END IF
      INPTR = IBIND
C                                       Begin counting loop.
      DO 160 I = 1,NY
         IY = NY - I + 1
         YMIN = IY - 0.5
         IDATA = IDATA0
         CNT = 0
C                                       Return to here if more than one
C                                       record is loaded at a time.
 100     ENDROW = T
         NPOINT = 0
C                                       Check if all data read.
         IF (NIO.LE.0) GO TO 140
C                                       Make sure there is some data on
C                                       this row.
         IF (BUFF1(INPTR+1).LT.YMIN) GO TO 140
C                                       Check if end of row occurs in
C                                       this record.
            LIM = INCNT + MAXREC - CNT - 1
            LIM = MIN (LIM, NIO)
            JNPTR = INPTR
            DO 110 II = INCNT,LIM
               IF (BUFF1(JNPTR+1).LT.YMIN) GO TO 120
                  NPOINT = NPOINT + 1
                  JNPTR = JNPTR + LRECIN
 110           CONTINUE
C                                       Rest of record is on same row.
            ENDROW = F
 120        CNT = CNT + NPOINT
C                                       Load into AP.
            CALL QWR
            KTEMP = NPOINT
            KTEMP = KTEMP * LRECIN
            CALL QPUT (BUFF1(INPTR), IDATA, KTEMP, 2)
            IDATA = IDATA + KTEMP
            INPTR = INPTR + NPOINT * LRECIN
            INCNT = INCNT + NPOINT
C                                       Check if AP full or row finished
            IF ((ENDROW) .OR. (CNT.GE.MAXREC)) GO TO 140
C                                       Read next record.
 130        INCNT = 1
C                                       Read and loop back
            IF (NIO.GT.0) THEN
               CALL UVDISK ('READ', LUNL(1), INDL(1), BUFF1, NIO, IBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1020) IRET, 'READ', 'UV SCRATCH'
                  GO TO 995
                  END IF
               INPTR = IBIND
               GO TO 100
            ELSE
               ENDROW = T
               END IF
C                                       Grid data
 140     IF (CNT.GT.0) THEN
            CALL QWD
C                                       Use HIST for counting.
            CALL QHIST (IX, LRECIN, GRID, CNT, JNX, AMAX, AMIN)
            IDATA = IDATA0
            CNT = 0
C                                       Check if row finished.
            IF (.NOT.ENDROW) THEN
               IF (INCNT.LT.NIO) GO TO 100
               GO TO 130
               END IF
            END IF
C                                       Row finished, process.
         YY = IY + NROW / 2
C                                       If YY = 0 conjugate row.
         IF (YY.EQ.0) THEN
            KTEMP = GRID + 1
            ITEMP = NX - 1
            CALL QVMOV (KTEMP, 1, END1, -1, ITEMP)
            CALL QVMOV (GRID, 1, ROW2, 1, 1)
            CALL QVADD (GRID, 1, ROW2, 1, GRID, 1, JNX)
            END IF
C                                       Don't write rows before the
C                                       start of the grid file.
         IF (YY.LE.NY) THEN
C                                       Sum rows.
            CALL MDISK ('WRIT', LUNL(2), INDL(2), BUFFM, JBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1140) IRET, 'WRIT', I
               GO TO 995
               END IF
            CALL QVMOV (GRID, 1, ROW, 1, JNX)
            IF (UNFBOX.GT.0) THEN
               LIMIT = NROW
               DO 150 J = 2,LIMIT
                  IROW = GRID + JNX * (J-1)
                  CALL QVADD (IROW, 1, ROW, 1, ROW, 1, JNX)
 150              CONTINUE
               END IF
C                                       Boxsum sum of rows.
            CALL QBOXSU (ROW, 1, NROW, ROW2, 1, JNX)
C                                       Make sure values reasonable.
            CALL QVCLIP (ROW2, 1, WMIN, WMAX, ROW2, 1, JNX)
C                                       Read out row.
            CALL QWR
            CALL QGET (BUFFM(JBIND), ROW2, JNX, 2)
            CALL QWD
            END IF
C                                       Prepare AP for next row.
         IF (I.NE.NY) THEN
            CALL QVMOV (END3, -1, END2, -1, NMOV)
            CALL QVCLR (GRID, 1, JNX)
            END IF
 160     CONTINUE
C                                       Finish reading out grid
      IF (UNFBOX.GT.0) THEN
         DO 190 I = 1,UNFBOX
            CALL MDISK ('WRIT', LUNL(2), INDL(2), BUFFM, JBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1140) IRET, 'WRIT', I
               GO TO 995
               END IF
C                                       Add conjugate to next row.
            IROW = GRID + JNX * I
            KTEMP = IROW + 1
            ITEMP = NX - 1
            CALL QVMOV (KTEMP, 1, END1, -1, ITEMP)
            CALL QVMOV (IROW, 1, ROW2, 1, 1)
            CALL QVADD (IROW, 1, ROW2, 1, IROW, 1, JNX)
C                                       Sum rows.
            CALL QVMOV (GRID, 1, ROW, 1, JNX)
            LIMIT = NROW - I
            DO 180 J = 2,LIMIT
               IROW = GRID + JNX * (J-1)
               CALL QVADD (IROW, 1, ROW, 1, ROW, 1, JNX)
 180           CONTINUE
C                                       Boxsum sum of rows.
            CALL QBOXSU (ROW, 1, NROW, ROW2, 1, JNX)
C                                       Make sure values reasonable.
            CALL QVCLIP (ROW2, 1, WMIN, WMAX, ROW2, 1, JNX)
C                                       Read row back out.
            CALL QWR
            CALL QGET (BUFFM(JBIND), ROW2, JNX, 2)
            CALL QWD
 190        CONTINUE
         END IF
C                                       Finish write
      CALL MDISK ('FINI', LUNL(2), INDL(2), BUFFM, JBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1140) IRET, 'FINISH', I
         GO TO 995
         END IF
      CALL QRLSE
C                                       Apply corrections.
C                                       Open visibility file for writing
      CALL ZPHFIL ('SC', SCRVOL(2), SCRCNO(2), 1, PHNAME, IRET)
      CALL ZOPEN (LUNL(3), INDL(3), SCRVOL(2), PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'UV SCRATCH', 'WRITE'
         GO TO 995
         END IF
C                                       Initialize vis. file for read.
      CALL UVINIT ('READ', LUNL(1), INDL(1), NVISIN, VO, LRECIN, ILENBU,
     *   JBUFSZ, BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'UV SCRATCH', 'READ'
         GO TO 995
         END IF
C                                       Initialize vis. file for write.
      CALL UVINIT ('WRIT', LUNL(3), INDL(3), NVISIN, VO, LRECIN, ILENBU,
     *   JBUFSZ, BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'UV SCRATCH', 'WRITE'
         GO TO 995
         END IF
      OPTR = KBIND
C                                       Initialize grid file for read.
      CALL MINIT ('READ', LUNL(2), INDL(2), NX, NY, WIN, BUFFM, MBUFSZ,
     *   BO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'GRID SCRATCH', 'READ'
         GO TO 995
         END IF
C                                       Read first row of grid.
      KROW = NY
      CALL MDISK ('READ', LUNL(2), INDL(2), BUFFM, JBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1140) IRET, 'READ', I
         GO TO 995
         END IF
      YMIN = KROW - 0.5
C                                       Begin weighting loop.
C                                       Read vis record.
 200  CALL UVDISK ('READ', LUNL(1), INDL(1), BUFF1, INIO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET, 'READ', 'UV SCRATCH'
         GO TO 995
         END IF
      INPTR = IBIND
C                                       Loop thru record.
      IF (INIO.GT.0) THEN
         DO 230 I = 1,INIO
C                                       Copy vis.
            CALL RCOPY (LRECIN, BUFF1(INPTR), BUFF2(OPTR))
C                                       See if new grid row needed,
C                                       if so find next grid row.
 210        IF (BUFF2(OPTR+1).GE.YMIN) GO TO 220
C                                       Read next grid row.
               KROW = KROW - 1
C                                       Check if gone too far.
               IF (KROW.LE.0) THEN
                  WRITE (MSGTXT,1210) KROW
                  CALL MSGWRT (7)
                  GO TO 220
                  END IF
               CALL MDISK ('READ', LUNL(2), INDL(2), BUFFM, JBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1140) IRET, 'READ', I
                  GO TO 995
                  END IF
               YMIN = KROW - 0.5
               GO TO 210
C                                       Apply natural weight correction
C                                       to weight.
 220        INDEX = BUFF2(OPTR) - 0.5
            WT = BUFFM(INDEX+JBIND)
            IF ((WT.LE.0.0) .OR. (INDEX.LT.0) .OR. (INDEX.GT.NX-1)) THEN
               WRITE (MSGTXT,1220) WT, INDEX, KROW
               IF (NERR.LE.5) CALL MSGWRT (7)
               WT = 1.0
               NERR = NERR + 1
               END IF
            BUFF2(OPTR+2) = BUFF2(OPTR+2) / WT
            OPTR = OPTR + LRECIN
            INPTR = INPTR + LRECIN
 230        CONTINUE
C                                      Write
         NIOUT = INIO
         CALL UVDISK ('WRIT', LUNL(3), INDL(3), BUFF2, NIOUT, KBIND,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET, 'WRIT', 'UV SCRATCH'
            GO TO 995
            END IF
         OPTR = KBIND
         GO TO 200
         END IF
C                                       Finish write.
      NIOUT = 0
      CALL UVDISK ('FLSH', LUNL(3), INDL(3), BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET, 'FLSH', 'UV SCRATCH'
         GO TO 995
         END IF
C                                       Close files.
      CALL ZCLOSE (LUNL(1), INDL(1), IER)
      CALL ZCLOSE (LUNL(2), INDL(2), IER)
      CALL ZCLOSE (LUNL(3), INDL(3), IER)
      IF (NERR.GT.5) THEN
         WRITE (MSGTXT,1230) NERR
         CALL MSGWRT (7)
         END IF
      GO TO 999
C                                       Error
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDIMGW: ERROR',I3,1X,A,'ING ',A,' FILE FOR',A)
 1010 FORMAT ('SDIMGW:',I8,' TOO FEW AP WORDS AVAILABLE')
 1020 FORMAT ('SDIMGW: ERROR',I3,1X,A,'ING ',A,' FILE')
 1140 FORMAT ('SDIMGW: ERROR',I3,1X,A,'ING GRID ROW',I5)
 1210 FORMAT ('SDIMGW: ATTEMPTED TO READ ROW',I6)
 1220 FORMAT ('SDIMGW: BAD WEIGHT',F5.1,' INDEX',I6,' IN ROW',I5)
 1230 FORMAT ('SDIMGW:',I8,' INDEX AND/OR WEIGHT ERRORS TOTAL')
      END
      SUBROUTINE SDIMGG (IRET)
C-----------------------------------------------------------------------
C   SDIMGG grids the data to the image
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   NCHAN, NC, IC, IERR, NPASS, IPASS, LLREC, IG, ILENBU,
     *   BO, VO, BLKOF, DEPTH(5), WIN(4), IFACT, IR, DLUN, DIND,
     *   NUM, NO2, M, LAPREC, NIO, IBIND, INPTR, INCNT,
     *   I, IY, IDATA, CNT, NPOINT, JNPTR, LIM, II, UVPTR, YY, VISPTR,
     *   WTPTR, BIND, TOLROW, INC, APSIZ, KAP, CX, CY, TYP, MAXREC,
     *   NMOV, ICHAN, KNPTR, JJ, KK, ITEMP, END1, END2, NVISIN, IROW,
     *   BADSRT, NOTSRT, CINC
      REAL      XTEMP(16), YMIN, YMAX, DMAX, DMIN, CATMAR(256), LROW, YV
      LOGICAL   T, F, ENDROW, DOABS
      CHARACTER PHNAME*48
      INCLUDE 'SDIMG.INC'
      INTEGER   GRID(NUMBUF), ROW(NUMBUF)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (CATMAP, CATMAR)
      DATA T, F /.TRUE.,.FALSE./
      DATA BO, VO, UVPTR, DLUN /1, 0, 16, 4/
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
C                                       Make sure an ODD number of rows
C                                       is being kept in the AP.
      NO2 = MAX (XPARM(1), 1.0) + 0.1
      M = MAX (YPARM(1), 1.0) + 0.1
      M = M * 2 + 1
      APSIZ = 1024 * PSAPNW
      IFACT = 2
      IF (CXTYPE.LE.10) THEN
         INC = 100 * (M + 1 + 2*NO2)
      ELSE
         INC = XPARM(5) + 0.1
         INC = INC * M
         INC = INC * INC
         END IF
      INC = (APSIZ - INC - 64) / (M*IFACT*NX + 45)
      IF (INC.LE.0) THEN
         MSGTXT = 'AP MEMORY TOO SMALL FOR SUCH LONG ROWS, WIDE SUPPORT'
         IRET = 1
         GO TO 990
         END IF
      BADSRT = 0
      NOTSRT = 0
C                                       Open files - image planes
      NCHAN = CATMAP(KINAX+2)
      NC = MIN (NUMBUF, NCHAN)
      NC = MIN (NC, INC)
      INC = NC
      DO 10 IC = 1,NC
         CALL ZPHFIL ('MA', FVOL(1), FCNO(1), 1, PHNAME, IERR)
         CALL ZOPEN (LUNL(IC), INDL(IC), FVOL(1), PHNAME, T, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN', IC
            GO TO 990
            END IF
 10      CONTINUE
C                                       Open UV data
      CALL ZPHFIL ('SC', SCRVOL(2), SCRCNO(2), 1, PHNAME, IERR)
      CALL ZOPEN (DLUN, DIND, SCRVOL(2), PHNAME, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'OPEN'
         GO TO 990
         END IF
C                                       Setup for AP griding
C                                       Set AP pointers
      IG = M * IFACT
      IR = (M - 1) * IFACT
      DO 20 IC = 1,NC
         GRID(IC) = APSIZ - (NC-IC+1) * IG * NX - 1
         ROW(IC) = GRID(IC) + IR * NX
 20      CONTINUE
C                                       Grab AP - size is set already
      CALL QINIT (0, 0, KAP)
      NUM = IFACT * M * NX * NC
      CALL QVCLR (GRID(1), 1, NUM)
      IF (CXTYPE.LE.10) THEN
         CINC = 100
         CX = GRID(1) - CINC * (2 * NO2 + 1) - 1
         CY = CX - CINC * M - 1
      ELSE
         CINC = XPARM(5) + 0.1
         CX = GRID(1) - CINC * CINC * M * M - 1
         CY = CX
         END IF
      TYP = 4
      IF (IWT.EQ.2) TYP = 5
C                                       Set constants in AP.
      XTEMP(1) = 0.0
      XTEMP(2) = 0.0
      XTEMP(3) = 0.0
      XTEMP(4) = 0.0
      XTEMP(5) = 0.0
      XTEMP(6) = 0.0
      XTEMP(7) = 1.0
      XTEMP(8) = 0.0
      XTEMP(9) = 0.0
      XTEMP(10) = 0.0
      XTEMP(11) = 1.0
      XTEMP(12) = 1.0
      XTEMP(13) = 1.0
      XTEMP(14) = 0.0
      XTEMP(15) = 0.0
      XTEMP(16) = 0.0
      CALL QPUT (XTEMP, 0, 16, 2)
      DOABS = RWT(2).LE.0.0
C                                       Determine the maximum number
C                                       of visibility points which
C                                       fit in the AP.
      LAPREC = 3 * (NC + 1)
      MAXREC = (CY - 16) / LAPREC - 5
C                                       Be sure MAXREC.GT.0
      IF (MAXREC.LE.0) THEN
         IG = - MAXREC * LAPREC
         WRITE (MSGTXT,1020) IG
         IRET = 1
         GO TO 990
         END IF
C                                       Set gridding convolution tables
      CALL CONVFN (CX, CXTYPE, XPARM, BUFF2)
      IF (CXTYPE.LE.10) CALL CONVFN (CY, CYTYPE, YPARM, BUFF2)
      NMOV = (M-1) * IFACT * NX
      TOLROW = 2 * NX
C                                       Write from top to bottom
      WIN(1) = 1
      WIN(2) = NY
      WIN(3) = NX
      WIN(4) = 1
      DMAX = -1.0E20
      DMIN = 1.0E20
C                                       loop over channels
      NPASS = (NCHAN - 1) / NC + 1
      LLREC = 3 + 2 * NCHAN
      CALL FILL (5, 1, DEPTH)
      NVISIN = NUMKEP
      BIND = 1
      DO 300 IPASS = 0,NPASS
         NUM = IFACT * M * NX * NC
         CALL QVCLR (GRID(1), 1, NUM)
         LROW = 100000.
         ICHAN = 1
         IF (IPASS.GT.1) ICHAN = (IPASS - 1) * NC + 1
         IC = ICHAN + NC - 1
         IF (IC.GT.NCHAN) THEN
            NC = NCHAN + 1 - ICHAN
            IC = NCHAN
            END IF
         II = ICHAN + XBCHAN - 0.99
         JJ = IC + XBCHAN - 0.99
         WRITE (MSGTXT,1015) II, JJ
         IF (IPASS.EQ.0) WRITE (MSGTXT,1016) II, JJ
         CALL MSGWRT (2)
         LAPREC = 3 * (NC + 1)
         MAXREC = (CY - 16) / LAPREC - 5
         IC = MBUFSZ / (2 * LAPREC)
         MAXREC = MIN (MAXREC, IC)
         IF ((IPASS.GT.0) .AND. (MAXCWT.LE.0.0)) MAXCWT = 1.0
         XNLIM = ABS (RWT(2)) * MAXCWT
C                                       Init UV read
         ILENBU = 0
         CALL UVINIT ('READ', DLUN, DIND, NVISIN, VO, LLREC,
     *      ILENBU, JBUFSZ, BUFF1, BO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'INIT'
            GO TO 990
            END IF
C                                       Init image planes
         IF (IPASS.GT.0) THEN
            DO 30 IC = 1,NC
               DEPTH(1) = ICHAN + IC - 1
               CALL COMOFF (CATMAP(KIDIM), CATMAP(KINAX), DEPTH, BLKOF,
     *            IERR)
               BLKOF = BLKOF + BO
               CALL MINIT ('WRIT', LUNL(IC), INDL(IC), NX, NY, WIN,
     *            OUTBUF(1,IC), OBUFSZ, BLKOF, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'INIT', IC
                  GO TO 990
                  END IF
 30            CONTINUE
            END IF
C                                       Read first visibility record
         CALL UVDISK ('READ', DLUN, DIND, BUFF1, NIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ'
            GO TO 990
            END IF
         INPTR = IBIND
         INCNT = 1
C                                       Loop through grid
         DO 200 I = 1,NY
            IY = NY - I + 1
            YMIN = IY - 0.5
            YMAX = IY + 0.5
            IDATA = UVPTR
            CNT = 0
C                                       Return to here if more than one
C                                       record is loaded at a time.
 100        ENDROW = T
            NPOINT = 0
C                                       Check if all data read.
            IF (NIO.LE.0) GO TO 140
C                                       Make sure there is some data on
C                                       this row.
            IF (BUFF1(INPTR+1).LT.YMIN) GO TO 140
C                                       Check if end of row occurs in
C                                       this record.
               LIM = INCNT + MAXREC - CNT - 1
               LIM = MIN (LIM, NIO)
               JNPTR = INPTR
               KNPTR = 1
               DO 110 II = INCNT,LIM
C                                       Translate data for gridding
                  YV = BUFF1(JNPTR+1)
                  IF (YV.LT.YMIN) GO TO 120
                     NPOINT = NPOINT + 1
                     IROW = YV + 0.50
                     IF (IROW.NE.IY) BADSRT = BADSRT + 1
                     IF (YV.GT.LROW) NOTSRT = NOTSRT + 1
                     LROW = YV
C                                       Y in u position
                     BUFFM(KNPTR) = YV
C                                       X offset in v position
                     BUFFM(KNPTR+1) = BUFF1(JNPTR) - NX/2 - 1.
C                                       w
                     BUFFM(KNPTR+2) = BUFF1(JNPTR+2)
C                                       Data as complex
                     IR = JNPTR + 2 * ICHAN + 1
                     KNPTR = KNPTR + 3
                     DO 105 IC = 1,NC
                        BUFFM(KNPTR) = BUFF1(IR)
                        BUFFM(KNPTR+1) = 0.0
                        BUFFM(KNPTR+2) = BUFF1(IR+1)
                        KNPTR = KNPTR + 3
                        IR = IR + 2
 105                    CONTINUE
                     JNPTR = JNPTR + LLREC
 110              CONTINUE
C                                       Rest of record is on same row.
               ENDROW = F
 120           CNT = CNT + NPOINT
C                                       Load into AP.
               CALL QWR
               ITEMP = NPOINT * LAPREC
               CALL QPUT (BUFFM, IDATA, ITEMP, 2)
               IDATA = IDATA + ITEMP
               INCNT = INCNT + NPOINT
               INPTR = INPTR + NPOINT * LLREC
C                                       Check if AP full or row finished
               IF ((ENDROW) .OR. (CNT.GE.MAXREC)) GO TO 140
C                                       Read next record.
 130           INCNT = 1
C                                       Check if all records read.
               IF (NIO.GT.0) THEN
                  CALL UVDISK ('READ', DLUN, DIND, BUFF1, NIO,
     *               IBIND, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1010) IRET, 'READ'
                     GO TO 990
                     END IF
                  INPTR = IBIND
                  GO TO 100
               ELSE
                  ENDROW = T
                  END IF
C                                       Grid data
 140        IF (CNT.GT.0) THEN
               CALL QWAIT
               ITEMP = -CNT
               DO 145 IC = 1,NC
                  VISPTR = UVPTR + 3 * IC
                  WTPTR = VISPTR + 2
                  CALL QGRIDA (UVPTR, VISPTR, WTPTR, LAPREC, GRID(IC),
     *               CX, CY, CINC, NO2, M, NX, ITEMP, TYP)
                  CALL QWR
 145              CONTINUE
               IDATA = UVPTR
               CNT = 0
C                                       Loop if row unfinished.
               IF (.NOT.ENDROW) THEN
                  IF (INCNT.LT.NIO) GO TO 100
                  GO TO 130
                  END IF
               END IF
C                                       Row finished, process.
            YY = IY + M / 2
C                                       Don't write rows before the
C                                       start of the grid file.
            IF (YY.LE.NY) THEN
               CALL QWAIT
               DO 180 IC = 1,NC
C                                       Write
                  IF (IPASS.GT.0) THEN
                     CALL MDISK ('WRIT', LUNL(IC), INDL(IC),
     *                  OUTBUF(1,IC), BIND, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'WRITE', IC
                        GO TO 990
                        END IF
                     END IF
C                                       Read out gridded data.
                  CALL QGET (BUFF2, ROW(IC), TOLROW, 2)
                  CALL QWAIT
                  JJ = BIND - 1
                  KK = -1
C                                       find max convolved weight
                  IF (IPASS.EQ.0) THEN
                     DO 149 II = 1,NX
                        JJ = JJ + 1
                        KK = KK + 2
                        MAXCWT = MAX (MAXCWT, ABS(BUFF2(KK+1)))
 149                    CONTINUE
C                                       Move data: validity image
                  ELSE IF (IWT.EQ.2) THEN
                     IF (DOABS) THEN
                        DO 150 II = 1,NX
                           JJ = JJ + 1
                           KK = KK + 2
                           IF ((ABS(BUFF2(KK+1)).GE.XNLIM) .AND.
     *                        (BUFF2(KK).GT.0.0)) THEN
                              OUTBUF(JJ,IC) = BUFF2(KK+1) * BUFF2(KK+1)
     *                           / BUFF2(KK)
                              DMAX = MAX (DMAX, OUTBUF(JJ,IC))
                              DMIN = MIN (DMIN, OUTBUF(JJ,IC))
                           ELSE
                              OUTBUF(JJ,IC) = FBLANK
                              END IF
 150                       CONTINUE
                     ELSE
                        DO 155 II = 1,NX
                           JJ = JJ + 1
                           KK = KK + 2
                           IF ((BUFF2(KK+1).GE.XNLIM) .AND.
     *                        (BUFF2(KK).GT.0.0)) THEN
                              OUTBUF(JJ,IC) = BUFF2(KK+1) * BUFF2(KK+1)
     *                           / BUFF2(KK)
                              DMAX = MAX (DMAX, OUTBUF(JJ,IC))
                              DMIN = MIN (DMIN, OUTBUF(JJ,IC))
                           ELSE
                              OUTBUF(JJ,IC) = FBLANK
                              END IF
 155                       CONTINUE
                        END IF
C                                       Move data: Convolution
                  ELSE IF (IWT.GE.0) THEN
                     IF (DOABS) THEN
                        DO 160 II = 1,NX
                           JJ = JJ + 1
                           KK = KK + 2
                           IF (ABS(BUFF2(KK+1)).GE.XNLIM) THEN
                              OUTBUF(JJ,IC) = BUFF2(KK+IWT) / MAXCWT
                              DMAX = MAX (DMAX, OUTBUF(JJ,IC))
                              DMIN = MIN (DMIN, OUTBUF(JJ,IC))
                           ELSE
                              OUTBUF(JJ,IC) = FBLANK
                              END IF
 160                       CONTINUE
                     ELSE
                        DO 165 II = 1,NX
                           JJ = JJ + 1
                           KK = KK + 2
                           IF (BUFF2(KK+1).GE.XNLIM) THEN
                              OUTBUF(JJ,IC) = BUFF2(KK+IWT) / MAXCWT
                              DMAX = MAX (DMAX, OUTBUF(JJ,IC))
                              DMIN = MIN (DMIN, OUTBUF(JJ,IC))
                           ELSE
                              OUTBUF(JJ,IC) = FBLANK
                              END IF
 165                       CONTINUE
                        END IF
C                                       Move data: Interpolation
                  ELSE
                     IF (DOABS) THEN
                        DO 170 II = 1,NX
                           JJ = JJ + 1
                           KK = KK + 2
                           IF (ABS(BUFF2(KK+1)).GE.XNLIM) THEN
                              OUTBUF(JJ,IC) = BUFF2(KK) / BUFF2(KK+1)
                              DMAX = MAX (DMAX, OUTBUF(JJ,IC))
                              DMIN = MIN (DMIN, OUTBUF(JJ,IC))
                           ELSE
                              OUTBUF(JJ,IC) = FBLANK
                              END IF
 170                       CONTINUE
                     ELSE
                        DO 175 II = 1,NX
                           JJ = JJ + 1
                           KK = KK + 2
                           IF (BUFF2(KK+1).GE.XNLIM) THEN
                              OUTBUF(JJ,IC) = BUFF2(KK) / BUFF2(KK+1)
                              DMAX = MAX (DMAX, OUTBUF(JJ,IC))
                              DMIN = MIN (DMIN, OUTBUF(JJ,IC))
                           ELSE
                              OUTBUF(JJ,IC) = FBLANK
                              END IF
 175                       CONTINUE
                        END IF
                     END IF
 180              CONTINUE
               END IF
C                                       Prepare AP for next row.
            IF (IY.NE.1) THEN
               DO 190 IC = 1,NC
                  ITEMP = IFACT * NX
                  END1 = ROW(IC) - 1
                  END2 = END1 + ITEMP
                  CALL QVMOV (END1, -1, END2, -1, NMOV)
                  CALL QWAIT
                  CALL QVCLR (GRID(IC), 1, ITEMP)
 190              CONTINUE
               END IF
 200        CONTINUE
C                                       Finish reading out grid
         DO 260 IC = 1,NC
            LIM = M / 2
            DO 250 I = 1,LIM
               IROW = GRID(IC) + IFACT * (M - I - 1) * NX
C                                       Write
               IF (IPASS.GT.0) THEN
                  CALL MDISK ('WRIT', LUNL(IC), INDL(IC), OUTBUF(1,IC),
     *               BIND, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITE', IC
                     GO TO 990
                     END IF
                  END IF
               CALL QWAIT
               CALL QGET (BUFF2, IROW, TOLROW, 2)
               CALL QWAIT
               JJ = BIND - 1
               KK = -1
C                                       find max cwt
               IF (IPASS.EQ.0) THEN
                  DO 205 II = 1,NX
                     JJ = JJ + 1
                     KK = KK + 2
                     MAXCWT = MAX (MAXCWT, ABS(BUFF2(KK+1)))
 205                 CONTINUE
C                                       Move data: validity image
               ELSE IF (IWT.EQ.2) THEN
                  IF (DOABS) THEN
                     DO 210 II = 1,NX
                        JJ = JJ + 1
                        KK = KK + 2
                        IF ((ABS(BUFF2(KK+1)).GE.XNLIM) .AND.
     *                     (BUFF2(KK).GT.0.0)) THEN
                           OUTBUF(JJ,IC) = BUFF2(KK+1) * BUFF2(KK+1)
     *                        / BUFF2(KK)
                           DMAX = MAX (DMAX, OUTBUF(JJ,IC))
                           DMIN = MIN (DMIN, OUTBUF(JJ,IC))
                        ELSE
                           OUTBUF(JJ,IC) = FBLANK
                           END IF
 210                    CONTINUE
                  ELSE
                     DO 215 II = 1,NX
                        JJ = JJ + 1
                        KK = KK + 2
                        IF ((BUFF2(KK+1).GE.XNLIM) .AND.
     *                     (BUFF2(KK).GT.0.0)) THEN
                           OUTBUF(JJ,IC) = BUFF2(KK+1) * BUFF2(KK+1)
     *                        / BUFF2(KK)
                           DMAX = MAX (DMAX, OUTBUF(JJ,IC))
                           DMIN = MIN (DMIN, OUTBUF(JJ,IC))
                        ELSE
                           OUTBUF(JJ,IC) = FBLANK
                           END IF
 215                    CONTINUE
                     END IF
C                                       Move data: Convolution
               ELSE IF (IWT.GE.0) THEN
                  IF (DOABS) THEN
                     DO 220 II = 1,NX
                        JJ = JJ + 1
                        KK = KK + 2
                        IF (ABS(BUFF2(KK+1)).GE.XNLIM) THEN
                           OUTBUF(JJ,IC) = BUFF2(KK+IWT) / MAXCWT
                           DMAX = MAX (DMAX, OUTBUF(JJ,IC))
                           DMIN = MIN (DMIN, OUTBUF(JJ,IC))
                        ELSE
                           OUTBUF(JJ,IC) = FBLANK
                           END IF
 220                    CONTINUE
                  ELSE
                     DO 225 II = 1,NX
                        JJ = JJ + 1
                        KK = KK + 2
                        IF (BUFF2(KK+1).GE.XNLIM) THEN
                           OUTBUF(JJ,IC) = BUFF2(KK+IWT) / MAXCWT
                           DMAX = MAX (DMAX, OUTBUF(JJ,IC))
                           DMIN = MIN (DMIN, OUTBUF(JJ,IC))
                        ELSE
                           OUTBUF(JJ,IC) = FBLANK
                           END IF
 225                    CONTINUE
                     END IF
C                                       Move data: Interpolation
               ELSE
                  IF (DOABS) THEN
                     DO 230 II = 1,NX
                        JJ = JJ + 1
                        KK = KK + 2
                        IF (ABS(BUFF2(KK+1)).GE.XNLIM) THEN
                           OUTBUF(JJ,IC) = BUFF2(KK) / BUFF2(KK+1)
                           DMAX = MAX (DMAX, OUTBUF(JJ,IC))
                           DMIN = MIN (DMIN, OUTBUF(JJ,IC))
                        ELSE
                           OUTBUF(JJ,IC) = FBLANK
                           END IF
 230                    CONTINUE
                  ELSE
                     DO 235 II = 1,NX
                        JJ = JJ + 1
                        KK = KK + 2
                        IF (BUFF2(KK+1).GE.XNLIM) THEN
                           OUTBUF(JJ,IC) = BUFF2(KK) / BUFF2(KK+1)
                           DMAX = MAX (DMAX, OUTBUF(JJ,IC))
                           DMIN = MIN (DMIN, OUTBUF(JJ,IC))
                        ELSE
                           OUTBUF(JJ,IC) = FBLANK
                           END IF
 235                    CONTINUE
                     END IF
                  END IF
 250           CONTINUE
C                                       Finish writes
            IF (IPASS.GT.0) THEN
               CALL MDISK ('FINI', LUNL(IC), INDL(IC), OUTBUF(1,IC),
     *            BIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'FINISH', IC
                  GO TO 990
                  END IF
               END IF
 260        CONTINUE
 300     CONTINUE
C                                       Close files
      CALL QRLSE
      CATMAR(KRDMX) = DMAX
      CATMAR(KRDMN) = DMIN
      DO 310 I = 1,INC
         CALL ZCLOSE (LUNL(I), INDL(I), IERR)
 310     CONTINUE
      CALL ZCLOSE (DLUN, DIND, IERR)
      IF ((BADSRT.GT.0) .OR. ((NOTSRT.GT.0) .AND. (.NOT.SORTOK))) THEN
         WRITE (MSGTXT,1310) NOTSRT
         CALL MSGWRT (6)
         WRITE (MSGTXT,1311) BADSRT
         CALL MSGWRT (6)
         END IF
      WRITE (MSGTXT,1315) MAXCWT
      CALL MSGWRT (5)
      IF ((IWT.LT.0) .OR. (IWT.GE.2)) THEN
         WRITE (MSGTXT,1316) XNLIM
      ELSE
         WRITE (MSGTXT,1317) XNLIM
         END IF
      CALL MSGWRT (5)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDIMGG: ERROR',I4,1X,A,'ING OUTPUT AS FILE',I2)
 1010 FORMAT ('SDIMGG: ERROR',I4,1X,A,'ING UV SCRATCH FILE')
 1015 FORMAT ('Begin gridding channels',I5,' through',I5)
 1016 FORMAT ('Begin finding max convolved weight using channels',I5,
     *   ' thru',I5)
 1020 FORMAT ('SDIMGG:',I8,' TOO FEW AP WORDS AVAILABLE')
 1310 FORMAT ('SDIMGG: Warning:',I8,' samples not in strict sort order')
 1311 FORMAT ('SDIMGG:',I8,' OF THESE GRIDDED ON WRONG ROW')
 1315 FORMAT ('Maximum convolved weight =',1PE12.4)
 1316 FORMAT ('   used to scale cutoff to',1PE12.4)
 1317 FORMAT ('   used to scale cutoff to',1PE12.4,
     *   ' and to scale output image')
      END
      SUBROUTINE SDIMSW (IRET)
C-----------------------------------------------------------------------
C   SDIMSW applies the uniform weighting correction to the weights.  The
C   visibility weights are divided by the number of visibilities
C   occuring in cells within a box of half width UNFBOX centered on the
C   cell in which a given visibility resides.
C   This version assumes that the weight image resides in the AP memory
C   and uses APCORE directly.
C   In/Out:
C      IRET     I     Error code: 0 okay, > 0 quit now.
C----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER PHNAME*48
      INTEGER   BO, VO, ILENBU, LRECIN, NVISIN, APSIZ, NERR, IX, NUM,
     *   NIO, IBIND, INPTR, I, JNPTR, II, IY, IX1, IX2, IY1, IY2, KBIND,
     *   INIO, NIOUT, OPTR, IERR, IRND, NCHAN
      LONGINT   LPTR
      REAL      WT, XX
      LOGICAL   T, F
      INCLUDE 'SDIMG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DAPC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T, F /.TRUE., .FALSE./
      DATA BO, VO /1, 0/
C-----------------------------------------------------------------------
      IRND(XX) = INT (XX + SIGN (0.5, XX))
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
      IF ((NATWT(:2).NE.'UN') .AND. (NATWT(:2).NE.'WT')) GO TO 999
      MSGTXT = 'Finding uniform weights in core'
      CALL MSGWRT (2)
      NERR = 0
C                                       Prepare for reads and writes.
C                                       Open visibility file.
      CALL ZPHFIL ('SC', SCRVOL(2), SCRCNO(2), 1, PHNAME, IRET)
      CALL ZOPEN (LUNL(1), INDL(1), SCRVOL(2), PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'UV SCRATCH', 'READ'
         GO TO 995
         END IF
C                                       Init. vis file
      NVISIN = NUMKEP
      NCHAN = CATMAP(KINAX+2)
      LRECIN = 3 + 2 * NCHAN
      ILENBU = 0
      CALL UVINIT ('READ', LUNL(1), INDL(1), NVISIN, VO, LRECIN, ILENBU,
     *   JBUFSZ, BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'UV SCRATCH', 'READ'
         GO TO 995
         END IF
C                                       Set pointers for AP
      APSIZ = 1024 * PSAPNW
C                                       Clear AP
      NUM = NX * NY
      CALL DFILL (NUM, 0.0D0, APCORE(PSAPOF))
C                                       Make sure MAXREC.GT.0
      IF (NUM.GT.APSIZ) THEN
         MSGTXT = 'BLEW IT SOMEHOW - NOT ENOUGH AP LEFT'
         IRET = 1
         GO TO 995
         END IF
C                                       Read first visibility record.
 100  CALL UVDISK ('READ', LUNL(1), INDL(1), BUFF1, NIO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET, 'READ', 'UV SCRATCH'
         GO TO 995
         END IF
      IF (NIO.GT.0) THEN
         INPTR = IBIND
         DO 125 II = 1,NIO
            IX = IRND (BUFF1(INPTR))
            IY = IRND (BUFF1(INPTR+1))
C                                       weight by weights
            IF (NATWT(:2).EQ.'WT') THEN
               JNPTR = INPTR + 5
               WT = 0.0
               DO 110 I = 1,NCHAN
                  WT = MAX (WT, BUFF1(JNPTR))
                  JNPTR = JNPTR + 3
 110              CONTINUE
               WT = WT
C                                       weight by count
            ELSE
               WT = 1.0
               END IF
            INPTR = INPTR + LRECIN
C                                       Grid it
            IF (WT.GT.0.0) THEN
               IX1 = MAX (1, IX-UNFBOX)
               IY1 = MAX (1, IY-UNFBOX)
               IX2 = MIN (NX, IX+UNFBOX)
               IY2 = MIN (NY, IY+UNFBOX)
               DO 120 IY = IY1,IY2
                  I = (IY - 1) * NX + IX1
                  LPTR = I - 1 + PSAPOF
                  DO 115 IX = IX1,IX2
                     APCORE(LPTR) = APCORE(LPTR) + WT
                     LPTR = LPTR + 1
 115                 CONTINUE
 120              CONTINUE
               END IF
 125        CONTINUE
         GO TO 100
         END IF
C                                       Apply corrections.
C                                       Open visibility file for writing
      CALL ZPHFIL ('SC', SCRVOL(2), SCRCNO(2), 1, PHNAME, IRET)
      CALL ZOPEN (LUNL(3), INDL(3), SCRVOL(2), PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'UV SCRATCH', 'WRITE'
         GO TO 995
         END IF
C                                       Initialize vis. file for read.
      CALL UVINIT ('READ', LUNL(1), INDL(1), NVISIN, VO, LRECIN, ILENBU,
     *   JBUFSZ, BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'UV SCRATCH', 'READ'
         GO TO 995
         END IF
C                                       Initialize vis. file for write.
      CALL UVINIT ('WRIT', LUNL(3), INDL(3), NVISIN, VO, LRECIN, ILENBU,
     *   JBUFSZ, BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'UV SCRATCH', 'WRITE'
         GO TO 995
         END IF
      OPTR = KBIND
C                                       Begin weighting loop.
C                                       Read vis record.
 200  CALL UVDISK ('READ', LUNL(1), INDL(1), BUFF1, INIO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET, 'READ', 'UV SCRATCH'
         GO TO 995
         END IF
      INPTR = IBIND
C                                       Loop thru record.
      IF (INIO.GT.0) THEN
         DO 230 II = 1,INIO
C                                       Copy vis.
            CALL RCOPY (LRECIN, BUFF1(INPTR), BUFF2(OPTR))
            IX = IRND (BUFF2(OPTR))
            IY = IRND (BUFF2(OPTR+1))
            LPTR = (IY - 1) * NX + IX - 1 + PSAPOF
            WT = APCORE(LPTR)
            IF (WT.LE.0.0) THEN
               WRITE (MSGTXT,1220) WT, IX, IY
               IF (NERR.LE.5) CALL MSGWRT (7)
               WT = 1.0
               NERR = NERR + 1
               END IF
            BUFF2(OPTR+2) = BUFF2(OPTR+2) / WT
            OPTR = OPTR + LRECIN
            INPTR = INPTR + LRECIN
 230        CONTINUE
C                                      Write
         NIOUT = INIO
         CALL UVDISK ('WRIT', LUNL(3), INDL(3), BUFF2, NIOUT, KBIND,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET, 'WRIT', 'UV SCRATCH'
            GO TO 995
            END IF
         OPTR = KBIND
         GO TO 200
         END IF
C                                       Finish write.
      NIOUT = 0
      CALL UVDISK ('FLSH', LUNL(3), INDL(3), BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET, 'FLSH', 'UV SCRATCH'
         GO TO 995
         END IF
C                                       Close files.
      CALL ZCLOSE (LUNL(1), INDL(1), IERR)
      CALL ZCLOSE (LUNL(3), INDL(3), IERR)
      IF (NERR.GT.5) THEN
         WRITE (MSGTXT,1230) NERR
         CALL MSGWRT (7)
         END IF
      GO TO 999
C                                       Error
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDIMSW: ERROR',I3,1X,A,'ING ',A,' FILE FOR',A)
 1020 FORMAT ('SDIMSW: ERROR',I3,1X,A,'ING ',A,' FILE')
 1220 FORMAT ('SDIMSW: BAD WEIGHT',F5.1,' COLUMN',I5,' IN ROW',I5)
 1230 FORMAT ('SDIMSW:',I8,' INDEX AND/OR WEIGHT ERRORS TOTAL')
      END
      SUBROUTINE SDIMSG (IRET)
C-----------------------------------------------------------------------
C   SDIMSG grids the data to the image
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'SDIMG.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
C                                       Change names a bit
      SDBCHN = XBCHAN + 0.1
      SDECHN = XECHAN + 0.1
      VISNUM = NUMKEP
      CALL SDGRID (LUNL(1), 0, 2, FVOL(1), FCNO(1), .FALSE., IWT,
     *   RWT(2), JBUFSZ, BUFF1, BUFF2, MBUFSZ, BUFFM, OBUFSZ, OUTBUF,
     *   MAXCWT, XNLIM, IRET)
C
 999  RETURN
      END
      SUBROUTINE SDIMGH (IRET)
C-----------------------------------------------------------------------
C   SDIMGH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER NOTTYP(5)*2, HILINE*72, CHCONV(5)*8, OTYPE(4)*16
      INTEGER   LUN1, LUN2, NONOT, IERR, ITRIM, I, J, IROUND
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SDIMG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA LUN1, LUN2 /27,28/
      DATA NONOT, NOTTYP /5, 'NX','CS','FG','CH','AN'/
      DATA T /.TRUE./
      DATA CHCONV /'Pill box', 'Expontl ', 'SinX / X', 'Exp*Sinc',
     *   'Spheroid'/
      DATA OTYPE /'Interpolated', 'Convolved', 'Conv weight',
     *   'Sigma**(-2)'/
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
      CALL COPY (256, CATMAP, CATBLK)
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, SCRTCH(257), SCRTCH, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 50
         END IF
C                                       New history
C                                       file names
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) GO TO 50
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Calibration
      IF (DOCAL) THEN
         WRITE (HILINE,1010) TSKNAM, CLUSE
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         END IF
C                                       Editing
      IF (DOFLAG) THEN
         WRITE (HILINE,1011) TSKNAM, FGVER
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         END IF
C                                       Max no. vis.
      WRITE (HILINE,1015) TSKNAM, NUMKEP
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
      WRITE (HILINE,1016) TSKNAM, NUMOFF
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Data selection
      I = IROUND (XBIF)
      WRITE (HILINE,1020) TSKNAM, I
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
      I = IROUND (XBCHAN)
      WRITE (HILINE,1021) TSKNAM, I
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
      I = IROUND (XECHAN)
      WRITE (HILINE,1022) TSKNAM, I
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Weighting
      IF ((NATWT(:2).NE.'UN') .AND. (NATWT(:2).NE.'WT')) THEN
         WRITE (HILINE,1025) TSKNAM
      ELSE
         WRITE (HILINE,1026) TSKNAM, NATWT(:2), UNFBOX
         END IF
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
      IF (RWT(2).GT.0.0) THEN
         WRITE (HILINE,1027) TSKNAM, RWT(2)
      ELSE
         RWT(2) = -RWT(2)
         WRITE (HILINE,1028) TSKNAM, RWT(2)
         END IF
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
      IF (RWT(2).GT.0.0) THEN
         WRITE (HILINE,1029) TSKNAM, XNLIM
      ELSE
         WRITE (HILINE,1030) TSKNAM, XNLIM
         END IF
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
      I = ITRIM (OTYPE(IWT+2))
      WRITE (HILINE,1031) TSKNAM, OTYPE(IWT+2)(:I)
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
      IF ((IWT.EQ.0) .OR. (IWT.EQ.1)) THEN
         WRITE (HILINE,1032) TSKNAM, MAXCWT
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         END IF
C                                       Convolving function.
      IF (CXTYPE.LE.10) THEN
         WRITE (HILINE,1040) TSKNAM, CHCONV(CXTYPE), CHCONV(CYTYPE)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,1041) TSKNAM, CXTYPE, (XPARM(J), J = 1,3)
         CALL HIADD (LUN2, HILINE,  SCRTCH,  IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,1042) TSKNAM, (XPARM(J), J = 4,7)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,1043) TSKNAM, CYTYPE, (YPARM(J), J = 1,3)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,1042) TSKNAM, (YPARM(J), J = 4,7)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
      ELSE
         WRITE (HILINE,1045) TSKNAM, CHCONV(CXTYPE-10)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,1046) TSKNAM, CXTYPE, (XPARM(J), J = 1,3)
         CALL HIADD (LUN2, HILINE,  SCRTCH,  IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,1042) TSKNAM, (XPARM(J), J = 4,7)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         END IF
C                                       Close HI file
 50   CALL HICLOS (LUN2, T, SCRTCH, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO,
     *   FCNO(NCFILE), FCNO(NCFILE-1), CATBLK, SCRTCH(257), SCRTCH,
     *   IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1050) IERR
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST', SCRTCH,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDIMGH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'GAINUSE =',I9,' / CS (calibration) table applied')
 1011 FORMAT (A6,'FLAGVER =',I9,' / FG (flag) table applied')
 1015 FORMAT (A6,'NUMKEPT =',I9,' / Samples gridded in image')
 1016 FORMAT (A6,'NUMLOST =',I9,' / Samples lost, outside image')
 1020 FORMAT (A6,'BIF     =',I9,' / IF selected')
 1021 FORMAT (A6,'BCHAN   =',I9,' / First spectral channel selected')
 1022 FORMAT (A6,'ECHAN   =',I9,' / Last spectral channel selected')
 1025 FORMAT (A6,'UVWTFN  = ''NA''      /Natural weighting')
 1026 FORMAT (A6,'UVWTFN  = ''',A2,'''  UVBOX =',I5,' /Uniform weight')
 1027 FORMAT (A6,'FWEIGHT =',F9.4,' / Min. sum of convolved weights')
 1028 FORMAT (A6,'FWEIGHT =',F9.4,
     *   ' / Min. ABS(sum of convolved weights)')
 1029 FORMAT (A6,'WEIGHT  =',F9.3,
     *   ' / Scaled min sum of convolved weights')
 1030 FORMAT (A6,'WEIGHT  =',F9.3,
     *   ' / Scaled min ABS(sum of convolved weights)')
 1031 FORMAT (A6,'OUTTYPE =''',A,'''',3X,' / Form of output')
 1032 FORMAT (A6,'SCALE   =',F9.3,' / scaled down by max conv weight')
 1040 FORMAT (A6,'/Convolution functions - U: ',A8,3X,'V: ',A8)
 1041 FORMAT (A6,'XTYPE =',I3,' XPARM =',3(1PE12.5,', '))
 1042 FORMAT (A6,1X,3(1PE12.5,', '),1PE12.5)
 1043 FORMAT (A6,'YTYPE =',I3,' YPARM =',1PE12.5,2(', ',E12.5))
 1045 FORMAT (A6,'/Convolution functions - Circular: ',A8)
 1046 FORMAT (A6,'RTYPE =',I3,' RPARM =',3(1PE12.5,', '))
 1050 FORMAT ('SDIMGH: ERROR',I4,' COPYING TABLES')
      END
