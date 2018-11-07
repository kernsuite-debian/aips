LOCAL INCLUDE 'PGAUS.INC'
      INTEGER   MAXGAU, MAXPRM, MXPAIR, MAXLIS, NPLIM, NMXIMG
      PARAMETER (MAXGAU=8)
      PARAMETER (MAXPRM=2+3*MAXGAU)
      PARAMETER (MXPAIR=(((MAXGAU-1)*MAXGAU)/2))
      PARAMETER (MAXLIS = 1000)
      PARAMETER (NPLIM=8192)
      PARAMETER (NMXIMG=8)
LOCAL END
LOCAL INCLUDE 'XGAUS.INC'
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'PGAUS.INC'
C                                       Local include for XGAUS
      REAL      XSEQIN, XDISKI, XINVER, XSEQO, XDISKO, UBLC(7), UTRC(7),
     *   YINC, ZINC, FCUT, ORDER, XDOCAT, DOTV, DORESI, DOMODL,
     *   RANGE(2), PLTYPE, PIXVAL, XNIT, XGAUSS, RMSLIM, BADD(10)
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6,
     *   FUNCTY(NMXIMG*MAXGAU)*2, REFTYP*8
      DOUBLE PRECISION CATD(128), OLDD(128), XVOFF, REFVAL
      REAL      CATR(256), OLDR(256), BUFF1(MABFSS), BUFF2(MABFSS),
     *   PRCLIP, GMAX(MAXGAU), GPOS(MAXGAU), GWIDTH(MAXGAU), BLC(7),
     *   TRC(7), REFPIX, REFINC, VPEAK
      HOLLERITH CATH(256), OLDH(256)
      LOGICAL   XGNEW
      INTEGER   CATBLK(256), CATOLD(256), SEQIN, SEQOUT, DISKIN, DISKO,
     *   NEWCNO, OLDCNO, JBUFSZ, ICODE, JCODE, DOCOMP(MAXPRM), GCODE,
     *   SCRTCH(512), XGVERS, IYINC, IZINC, IBLC(2), ITRC(2), LBLC(2),
     *   LTRC(2), XGBUFF(512), PSTART, XGROWS, IXGRNO, XGKOLS(11),
     *   XGNUMV(11), TVSUP, DONROW, DOCAT, PIXLIS(2,MAXLIS), NLIST,
     *   IPL(2), NGAUSS, KGAUSS, SUBWIN(4), IBUFF1(MABFSS)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XINVER, XNAMOU,
     *   XCLAOU, XSEQO, XDISKO, UBLC, UTRC, YINC, ZINC, FCUT, ORDER,
     *   XDOCAT, DOTV, DORESI, DOMODL, RANGE, PLTYPE, PIXVAL, XNIT,
     *   XGAUSS, RMSLIM, BADD
      COMMON /XGACHR/ NAMEIN, CLAIN, NAMOUT, CLAOUT, REFTYP, FUNCTY
      COMMON /PARMS/ CATOLD, XGBUFF, XVOFF, REFVAL, REFPIX, REFINC,
     *   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO, JBUFSZ, ICODE,
     *   JCODE, DOCOMP, GCODE, XGVERS, IYINC, IZINC, IBLC, ITRC, LBLC,
     *   LTRC, XGNEW, PSTART, XGROWS, PRCLIP, IXGRNO, XGKOLS, XGNUMV,
     *   TVSUP, DONROW, DOCAT, PIXLIS, NLIST, IPL, NGAUSS, GMAX, GPOS,
     *   GWIDTH, KGAUSS, BLC, TRC, SUBWIN, VPEAK
      COMMON /BUFRS/ BUFF1, BUFF2, SCRTCH
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      EQUIVALENCE (CATOLD, OLDR, OLDD, OLDH)
      EQUIVALENCE (IBUFF1, BUFF1)
C                                                          End XGAUS
LOCAL END
LOCAL INCLUDE 'XGAUSD.INC'
      INCLUDE 'PGAUS.INC'
C
      DOUBLE PRECISION DATA(NPLIM), TDATA(NPLIM), LPARMS(MAXPRM)
      REAL      XBAR, UCHAN, THERMS
      INTEGER   NITTER, ITTER, JJC, LLCOMP(MAXPRM), IGR1, IGR2, IGR3,
     *   IGR4, IGR5, IGR7, TTYLUN, TTYIND, MVAR, IVAR(MAXPRM),
     *   JVAR(MAXPRM), PLTRMS(2), CATTAU(256)
      LOGICAL   DOEVEN
      COMMON /GDATA/ DATA, TDATA, LPARMS, XBAR, NITTER, ITTER, JJC,
     *   LLCOMP, UCHAN, IGR1, IGR2, IGR3, IGR4, IGR5, IGR7, DOEVEN,
     *   TTYLUN, TTYIND, MVAR, IVAR, JVAR, THERMS, PLTRMS, CATTAU
LOCAL END
      PROGRAM AGAUS
C-----------------------------------------------------------------------
C! Fits 1-D Gaussians to rows of an image.
C# Map Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 2017
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
C   AGAUS fits 1-dimensional Gaussians to rows of an image.
C   It fits up to 8 components plus a linear baseline and writes out
C   an n-dim residual cube (DOCAT > 0) and 8*Ngauss + 4  n-1
C   dimensional images of the results and errors.  It will display
C   the data, initial guess, model, and residual for each row on the
C   TV.  After each fit so displayed, it asks for permission to keep
C   the results.  If NGAUSS > 1, it asks for permission to keep the
C   initial guess and will accept TV cursor input of a new initial
C   guess on that.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input image.
C      INCLASS        CLAIN         Class of input image.
C      INSEQ          SEQIN         Seq. of input image.
C      INDISK         DISKIN        Disk number of input image.
C      INVERS         XGVERS        XG table version number in use
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTCLASS       CLAOUT        Class of the output image.
C                                   Default is input class.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      UBLC(7)        UBLC          Bottom left corner of subimage
C                                   of input image.
C      UTRC(7)        UTRC          Top right corner of subimage.
C      YINC           YINC          Pixel increment on 2nd axis
C      ZINC           ZINC          Pixel increment on 3rd axis
C      FLUX           FCUT          Flux cutoff: > 2 consecutive
C                                   points must > FLUX to fit
C                                   Also in initial auto-guesses
C      ORDER          ORDER         > 0 -> fit a baseline
C      DOCAT          DOCAT         Catalog the residual map
C      DOTV           DOTV          Plot data on TV
C      LTYPE                        Type of labeling: 1 border,
C                                   2 no ticks, 3 standard, 4 rel
C                                   to center, 5 rel to subim cen
C                                   6 map pixels
C      PIXRANGE                     Min,Max of image intensity
C                                   Max <= Min => entire range
C      PIXVAL         PIXVAL        Display only if peak < PIXVAL
C      NITER          XNIT          Limit on iterations in fit
C      NGAUSS         NGAUSS        Number of Gaussians
C      GMAX(4)        GMAX          Peak value: comps 1 - 4
C      GPOS(8)        GPOS          Center pixel: "
C      GWIDTH(12)     GWIDTH        FWHM (pixels) : "
C      DOMAX(4)       DOMAX         <= 0. -> hold fixed
C      DOPOS(8)       DOPOS         <= 0. -> hold fixed; also (5)
C                                   > 0. => USE OLD answer for init
C                                   guess
C      DOWIDTH(8)     DOWIDT        <= 0. -> hold fixed
C      BADD(10)       IBAD          Disk numbers to avoid.
C   Programmer Eric W. Greisen based on Cotton's Taffy and Fickling's
C   SLFIT.   July 1983
C   Major overhaul April 2013 to table basis
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, IERR, DEVON, I
      INCLUDE 'XGAUS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'AGAUS '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL XGAUIN (PRGM, IRET)
      DEVON = 0
      IF (IRET.NE.0) GO TO 990
C                                       inits, open TV
      IF (ICODE.GE.1) THEN
         CALL TVOPEN (SCRTCH, IRET)
         IF (IRET.GT.1) GO TO 990
         DEVON = 2
         TVSUP = 0
         CALL YINIT (BUFF2, IRET)
         IF (IRET.GT.0) GO TO 990
      ELSE
         TVSUP = 1
         END IF
C                                       routine that goes through
C                                       whole input cube
      IF ((IRET.EQ.0) .AND. (PSTART.LE.XGROWS)) THEN
         CALL XGAUDO (IRET)
         IF ((IRET.EQ.0) .AND. ((IYINC.GT.1) .OR. (IZINC.GT.1)))
     *      CALL XGAUD1 (IRET)
         END IF
C                                       interactive routine to polish
      IF ((IRET.EQ.0) .AND. (DEVON.EQ.2)) CALL XGAUTV (IRET)
C                                       close devices
      IF (DEVON.EQ.2) CALL TVCLOS (SCRTCH, IERR)
      DEVON = 0
C                                       Resume AIPS
      IF ((RQUICK) .AND. (ICODE.GT.0)) CALL RELPOP (IRET, SCRTCH, IERR)
C                                       write out images
      IF ((IRET.EQ.0) .AND. (DOCAT.GT.0)) CALL XGAUOU (IRET)
C                                       close XG table
      CALL TABKEY ('WRIT', 'PIX FIT ', 1, XGBUFF, 1, DONROW, 4, I)
      CALL TABIO ('CLOS', 0, IXGRNO, XGBUFF, XGBUFF, I)
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE XGAUIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   XGAUIN gets input parameters for XGAUS and creates an output table
C   if needed for the fitting results, filling it with flux values.
C   Inputs:
C      PRGN   C*6   Program name
C   Output:
C      IRET   I     Error code: 0 => ok
C                     4 => user routine detected error.
C                     5 => catalog troubles
C                     8 => can't start
C      /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      CHARACTER STAT*4, MTYPE*2, FCHARS(3)*4, CHTM12*12
      INTEGER   IERR, NPARM, IROUND, I
      INCLUDE 'XGAUS.INC'
      INCLUDE 'XGAUSD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      DONROW = 0
      DOEVEN = .FALSE.
      TTYLUN = 5
      TTYIND = 0
C                                       Get input parameters.
      NPARM = 55
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C                                       Using the TV?
      ICODE = -1
      IF ((NPOPS.GT.NINTRN) .AND. (NTVDEV.LE.0)) THEN
         DOTV = -1.0
         DOMODL = -1.0
         DORESI = -1.0
      ELSE IF (DOTV.LE.0.0) THEN
         DOMODL = -1.0
         DORESI = -1.0
      ELSE
         ICODE = 1
         IF (DOTV.GT.1.5) ICODE = 2
         END IF
C                                       Restart AIPS
      IF ((RQUICK) .AND. ((ICODE.LE.0) .OR. (IRET.NE.0))) CALL RELPOP
     *   (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
      DOCAT = IROUND (XDOCAT)
      DOCAT = MAX (0, MIN (3, DOCAT))
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      DO 10 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 10      CONTINUE
      CALL RFILL (MAXGAU, 0.0, GMAX)
      CALL RFILL (MAXGAU, 0.0, GPOS)
      CALL RFILL (MAXGAU, 0.0, GWIDTH)
      IF (RMSLIM.LE.0.0) RMSLIM = 1000000.
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'WRIT', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 1
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD, CATBLK)
C                                       Set defaults on BLC,TRC
      CALL RFILL (7, 1.0, BLC)
      CALL RFILL (7, 0.0, TRC)
      CALL WINDOW (CATOLD(KIDIM), CATOLD(KINAX), BLC, TRC, IERR)
C                                       users sub window
      CALL WINDOW (CATOLD(KIDIM), CATOLD(KINAX), UBLC, UTRC, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check input axes
      DO 20 I = 1,3
         CALL H2CHR (4, 1, CATH(KHCTP), CHTM12)
         IF (FCHARS(I).EQ.CHTM12(:4)) GO TO 25
 20      CONTINUE
      MSGTXT = 'WARNING: FIRST AXIS NOT FREQUENCY OR VELOCITY'
      CALL MSGWRT (6)
C                                       XG table keywords
 25   I = YINC + 0.01
      IF (I.LE.0) I = 1
      YINC = I
      I = ZINC + 0.01
      IF (I.LE.0) I = 1
      ZINC = I
      IYINC = YINC + 0.1
      IZINC = ZINC + 0.1
      LBLC(1) = UBLC(2) + 0.1
      LBLC(2) = UBLC(3) + 0.1
      LTRC(1) = UTRC(2) + 0.1
      LTRC(2) = UTRC(3) + 0.1
      XVOFF =  OLDD(KDCRV)
C                                       Baseline
C                                       always fit continuum
      ORDER = MAX (0.0, ORDER)
      JCODE = 2
      IF (IROUND(ORDER).EQ.0) JCODE = 1
c     IF (ORDER.LT.0.0) JCODE = 0
      IF (FCUT.LE.0.0) FCUT = 0.0005
      IF (PIXVAL.LE.FCUT) PIXVAL = 1.0E9
C                                       Gaussian parms
      NGAUSS = XGAUSS + 0.01
      NGAUSS = MIN (MAXGAU, MAX (1, NGAUSS))
      XGAUSS = NGAUSS
      CALL FILL (MAXPRM, -1, DOCOMP)
      CALL FILL (JCODE+3*NGAUSS, 1, DOCOMP)
C                                       Check input size
      IRET = 0
      IF (XNIT.LT.10.) XNIT = 100 * XGAUSS
      IF (UTRC(1)-UBLC(1).GE.NPLIM) THEN
         IRET = 10
         WRITE (MSGTXT,1045) NPLIM
         GO TO 990
         END IF
      I = 3 * NGAUSS + JCODE + 1
      IF (UTRC(1)-UBLC(1)+1.LE.I) THEN
         IRET = 10
         WRITE (MSGTXT,1050) I
         GO TO 990
         END IF
C                                       XG version
      CALL FNDEXT ('XG', CATOLD, I)
      XGVERS = XINVER + 0.1
      IF (XGVERS.LE.0) THEN
         XGVERS = I + 1
      ELSE
         XGVERS = MIN (I+1, XGVERS)
         END IF
      XGNEW = XGVERS.GT.I
C                                       fill XG table
      CALL XGFILL (IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C                                       Restart AIPS
 995  IF ((RQUICK) .AND. (IRET.NE.0) .AND. (ICODE.GT.0)) CALL RELPOP
     *   (IRET, SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XGAUIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATALOG HEADER ')
 1045 FORMAT ('WORKS ONLY ON (SUB)ROWS <=',I5,' PIXELS')
 1050 FORMAT ('REQUIRES AT LEAST',I4,' PIXELS TO DO FIT')
      END
      SUBROUTINE XGFILL (IRET)
C-----------------------------------------------------------------------
C   XGFILL checks pre-existing XG files and builds new ones (filling
C   them with spectrum peak values
C   Output:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'XGAUS.INC'
      INTEGER   IY, IZ, XGLUN, LUNI, INDI, NXI, NYI, WINI(4), IROUND,
     *   LIM3, LIM2, LIM1, I3, I2, I1, BOI, IPOS(7), IBIND, ABSORP
      REAL      RESULT(2*MAXPRM), VAL
      DOUBLE PRECISION VOFF
      CHARACTER PHNAME*48
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       pre-existing check
      XGLUN = 97
      IF (.NOT.XGNEW) THEN
         CALL XGINI ('READ', XGBUFF, DISKIN, OLDCNO, XGVERS, CATOLD,
     *      XGLUN, IXGRNO, XGKOLS, XGNUMV, IBLC, ITRC, IY, IZ, PRCLIP,
     *      VOFF, PSTART, REFVAL, REFPIX, REFINC, REFTYP, ABSORP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING PRE-EXISTING XG TABLE'
            GO TO 990
            END IF
         CALL TABIO ('CLOS', 0, IXGRNO, XGBUFF, XGBUFF, IRET)
         IF (ABSORP.LE.0) THEN
            MSGTXT = 'EXISTING XG TABLE IS FOR EMISSION: QUITTING'
            IRET = 10
            GO TO 990
            END IF
         IF ((IBLC(1).GT.LBLC(1)) .OR. (IBLC(2).GT.LBLC(2)) .OR.
     *      (ITRC(1).LT.LTRC(1)) .OR. (ITRC(2).LT.LTRC(2))) THEN
            MSGTXT = 'OLD XG TABLE DOES NOT MATCH CURRENT ADVERBS'
            IRET = 10
            GO TO 990
         ELSE IF ((VOFF.NE.XVOFF) .OR. (REFPIX.NE.OLDR(KRCRP)) .OR.
     *      (REFINC.NE.OLDR(KRCIC))) THEN
            MSGTXT = 'AXIS 1 VALUES HAVE CHANGED: USING OLDER VALUES'
            CALL MSGWRT (6)
            END IF
         XGROWS = XGBUFF(5)
C                                       reopen write
         CALL XGINI ('WRIT', XGBUFF, DISKIN, OLDCNO, XGVERS, CATOLD,
     *      XGLUN, IXGRNO, XGKOLS, XGNUMV, IBLC, ITRC, IY, IZ, PRCLIP,
     *      VOFF, PSTART, REFVAL, REFPIX, REFINC, REFTYP, ABSORP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RE-OPENING OLD XG TABLE'
            GO TO 990
            END IF
         PSTART = MAX (PSTART + 1, 1)
C                                       from beginning if lower cutoff
         IF (PRCLIP.GT.FCUT) THEN
            PSTART = 1
            VOFF = 0.0D0
            VOFF = FCUT
            CALL TABKEY ('WRIT', 'VCLIP   ', 1, XGBUFF, 1, VOFF, 1, I1)
            END IF
C                                       new one
      ELSE
         IBLC(1) = BLC(2) + 0.1
         IBLC(2) = BLC(3) + 0.1
         ITRC(1) = TRC(2) + 0.1
         ITRC(2) = TRC(3) + 0.1
         PSTART = 0
         REFVAL = OLDD(KDCRV)
         REFPIX = OLDR(KRCRP)
         REFINC = OLDR(KRCIC)
         CALL H2CHR (8, 1, OLDH(KHCTP), REFTYP)
         ABSORP = 1
         CALL XGINI ('WRIT', XGBUFF, DISKIN, OLDCNO, XGVERS, CATOLD,
     *      XGLUN, IXGRNO, XGKOLS, XGNUMV, IBLC, ITRC, IYINC, IZINC,
     *      FCUT, XVOFF, PSTART, REFVAL, REFPIX, REFINC, REFTYP, ABSORP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING NEW XG TABLE'
            GO TO 990
            END IF
         CALL RFILL (2*MAXPRM, FBLANK, RESULT)
         MSGTXT = 'Reading image cube to find maxima in rows'
         CALL MSGWRT (2)
         MSGTXT = 'and initialize the XG table with these values'
         CALL MSGWRT (2)
C                                       Open and init for read
         LUNI = 33
         CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, PHNAME, IRET)
         CALL ZOPEN (LUNI, INDI, DISKIN, PHNAME, .TRUE., .FALSE.,
     *      .TRUE., IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INPUT IMAGE'
            GO TO 990
            END IF
         NXI = CATOLD(KINAX)
         NYI = CATOLD(KINAX+1)
         WINI(1) = IROUND (UBLC(1))
         WINI(2) = IROUND (BLC(2))
         WINI(3) = IROUND (UTRC(1))
         WINI(4) = IROUND (TRC(2))
         LIM3 = TRC(3) - BLC(3) + 1.01
         LIM2 = TRC(2) - BLC(2) + 1.01
         LIM1 = UTRC(1) - UBLC(1) + 1.01
         CALL FILL (7, 1, IPOS)
         DO 100 I3 = 1,LIM3
            IPOS(3) = BLC(3) + I3 - 0.9
            CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3), BOI,
     *         IRET)
            BOI = BOI + 1
            CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF1,
     *         JBUFSZ, BOI, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'INIT READ OF INPUT IMAGE'
               GO TO 990
               END IF
            DO 90 I2 = 1,LIM2
               CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READ INPUT IMAGE'
                  GO TO 990
                  END IF
               VPEAK = 0.0
               DO 80 I1 = 1,LIM1-2
                  IF ((BUFF1(IBIND+I1-1).NE.FBLANK) .AND.
     *               (BUFF1(IBIND+I1).NE.FBLANK) .AND.
     *               (BUFF1(IBIND+I1+1).NE.FBLANK)) THEN
                     VAL = BUFF1(IBIND+I1-1) + BUFF1(IBIND+I1) +
     *                  BUFF1(IBIND+I1+1)
                     IF (VAL.GT.VPEAK) VPEAK = VAL
                     END IF
 80               CONTINUE
               IPOS(2) = BLC(2) + I2 - 0.9
               VPEAK = VPEAK / 3.0
               CALL TABXG ('WRIT', XGBUFF, IXGRNO, XGKOLS, XGNUMV,
     *            IPOS(2), 0, VPEAK, RESULT, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE NEW XG TABLE'
                  GO TO 990
                  END IF
 90            CONTINUE
 100        CONTINUE
         CALL ZCLOSE (LUNI, INDI, IRET)
         XGROWS = XGBUFF(5)
C                                       close table for safety
         CALL TABIO ('CLOS', 0, IXGRNO, XGBUFF, XGBUFF, IRET)
C                                       and reopen
         CALL XGINI ('WRIT', XGBUFF, DISKIN, OLDCNO, XGVERS, CATOLD,
     *      XGLUN, IXGRNO, XGKOLS, XGNUMV, IBLC, ITRC, IYINC, IZINC,
     *      FCUT, XVOFF, PSTART, REFVAL, REFPIX, REFINC, REFTYP, ABSORP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RE-OPENING NEW XG TABLE'
            GO TO 990
            END IF
         PSTART = 1
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XGFILL: ERROR',I4,' ON ',A)
      END
      SUBROUTINE XGAUDO (IRET)
C-----------------------------------------------------------------------
C   XGAUDO goes through the table on a IYINC, IZINC stride, sends
C   data to fitting routine using the input initial guess.
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'XGAUS.INC'
      INCLUDE 'XGAUSD.INC'
      CHARACTER PHNAME*48
      INTEGER   IROUND, LUNI, NYI, NXI, WINI(4), BOI, J, I1, IPOS(7),
     *   BOTEMP, IBIND, INDI, LIM1, IG, NGAU, IY, IZ, LXGRNO, FIRSTY,
     *   XXPOS(2), MGAU
      REAL      RESULT(2*MAXPRM+2*MAXGAU), TPEAK, TAU
      DOUBLE PRECISION PARMS(MAXPRM), UPARMS(MAXPRM), XPARMS(MAXPRM)
      LOGICAL   T, F, FIRSTZ
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI /16/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Note: CATOLD & CATBLK are
C                                       now the same
C                                       display parms
      UCHAN = 0
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, PHNAME, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, PHNAME, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE'
         GO TO 990
         END IF
C                                       Setup for I/O
      NXI = CATOLD(KINAX)
      NYI = CATOLD(KINAX+1)
      WINI(1) = IROUND (UBLC(1))
      WINI(2) = IROUND (UBLC(2))
      WINI(3) = IROUND (UTRC(1))
      WINI(4) = IROUND (UTRC(2))
C                                       Initial guess
      PARMS(1) = 0.0D0
      PARMS(2) = 0.0D0
      J = JCODE
      GCODE = 0
      DO 50 I1 = 1,NGAUSS
         PARMS(J+1) = GMAX(I1)
         PARMS(J+2) = GPOS(I1)
         PARMS(J+3) = GWIDTH(I1)
         IF (GWIDTH(I1).GT.0.01) GCODE = 1
         IF (PARMS(J+3).LE.0.0D0) PARMS(J+3) = 0.5
         J = J + 3
 50      CONTINUE
      IG = 3 * NGAUSS + JCODE
      DO 55 I1 = 1,IG
         UPARMS(I1) = PARMS(I1)
         XPARMS(I1) = PARMS(I1)
 55      CONTINUE
      CALL COPY (MAXPRM, DOCOMP, LLCOMP)
      IF ((IYINC.GT.1) .OR. (IZINC.GT.1)) THEN
         WRITE (MSGTXT,1055) IYINC, IZINC
      ELSE
         MSGTXT = 'XGAUDO: solving Gaussians at every pixel'
         END IF
      CALL MSGWRT (2)
C                                       Setup for looping
C                                       Loop
      LIM1 = UTRC(1) - UBLC(1) + 1.01
      CALL FILL (7, 1, IPOS)
      IPOS(1) = UBLC(1) + 0.01
      FIRSTY = 0
      IXGRNO = 1
      DO 200 IZ = LBLC(2),LTRC(2),IZINC
         FIRSTZ = .TRUE.
         DO 190 IY = LBLC(1),LTRC(1),IYINC
            IXGRNO = (IZ-IBLC(2)) * (ITRC(1)-IBLC(1)+1) + IY - IBLC(1) +
     *         1
            LXGRNO = IXGRNO
            CALL TABXG ('READ', XGBUFF, IXGRNO, XGKOLS, XGNUMV, IPOS(2),
     *         NGAU, VPEAK, RESULT, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ XG TABLE'
               GO TO 990
            ELSE IF ((IRET.EQ.0) .AND. (NGAU.LE.0) .AND.
     *         (VPEAK.GE.FCUT)) THEN
C                                       find last nearby solution
               IF ((FIRSTZ) .AND. (FIRSTY.GT.0) .AND. (NGAUSS.GT.1))
     *            THEN
                  IXGRNO = (IZ-IBLC(2)-IZINC) * (ITRC(1)-IBLC(1)+1) +
     *               FIRSTY - IBLC(1) + 1
                  CALL TABXG ('READ', XGBUFF, IXGRNO, XGKOLS, XGNUMV,
     *               XXPOS, MGAU, TPEAK, RESULT, IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READ XG TABLE'
                     GO TO 990
                     END IF
                  DO 60 I1 = 1,MAXPRM
                     IF (RESULT(I1).NE.FBLANK) XPARMS(I1) = RESULT(I1)
 60                  CONTINUE
                  FIRSTY = 0
                  END IF
               FIRSTZ = .FALSE.
C                                       Init. files, first input.
               CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3),
     *            BOTEMP, IRET)
               BOI = BOTEMP + 1
               WINI(2) = IPOS(2)
               WINI(4) = IPOS(2)
               CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF1,
     *            JBUFSZ, BOI, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'INIT READ INPUT IMAGE'
                  GO TO 990
                  END IF
C                                       Read.
               CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READING INPUT IMAGE'
                  GO TO 990
                  END IF
C                                       Copy to buffer.
               DO 110 I1 = 1,LIM1
                  DATA(I1) = BUFF1(IBIND+I1-1)
                  IF (DATA(I1).EQ.FBLANK) THEN
                     TDATA(I1) = FBLANK
                  ELSE
                     TAU = DATA(I1) / VPEAK
                     IF (TAU.LT.1.E-5) TAU = 1.E-5
                     TDATA(I1) = -LOG(TAU)
                     END IF
 110              CONTINUE
C                                       Call DO1FIT
               CALL DO1FIT (IPOS, UPARMS, PARMS, XPARMS, NGAU, RESULT,
     *            IRET)
               IF (IRET.EQ.99) THEN
                  MSGTXT = 'Quitting at user request'
                  CALL MSGWRT (5)
                  CALL ZCLOSE (LUNI, INDI, I1)
                  GO TO 999
               ELSE IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1180) IRET
                  GO TO 990
                  END IF
               IF ((RESULT(3).NE.FBLANK) .AND. (FIRSTY.EQ.0)) FIRSTY=IY
               IXGRNO = LXGRNO
               CALL TABXG ('WRIT', XGBUFF, IXGRNO, XGKOLS, XGNUMV,
     *            IPOS(2), NGAU, VPEAK, RESULT, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE XG TABLE'
                  GO TO 990
                  END IF
               DO 115 I1 = 1,MAXPRM
                  IF (RESULT(I1).NE.FBLANK) UPARMS(I1) = RESULT(I1)
 115              CONTINUE
C                                       pick up good solution as last
            ELSE IF (IRET.EQ.0) THEN
               DO 120 I1 = 1,MAXPRM
                  IF (RESULT(I1).NE.FBLANK) PARMS(I1) = RESULT(I1)
 120              CONTINUE
               END IF
 190        CONTINUE
 200     CONTINUE
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XGAUDO: ERROR',I3,' ON ',A)
 1055 FORMAT ('XGAUDO: solving Gaussians every',I3,' Y pixels and',I3,
     *   ' Z pixels')
 1180 FORMAT ('XGAUDO: DO1FIT ERROR',I3)
      END
      SUBROUTINE XGAUD1 (IRET)
C-----------------------------------------------------------------------
C   XGAUD1 goes through the table on a stride of 1, sends data to
C   fitting routine gets the initial guess from fit pixels.
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'XGAUS.INC'
      INCLUDE 'XGAUSD.INC'
      CHARACTER PHNAME*48
      INTEGER   IROUND, LUNI, NYI, NXI, WINI(4), BOI, J, I1, IPOS(7),
     *   BOTEMP, IBIND, INDI, LIM1, IG, NGAU, LXGRNO, FIRSTY, IY, IZ,
     *   XXPOS(2), MGAU
      REAL      RESULT(2*MAXPRM+2*MAXGAU), TPEAK, TAU
      DOUBLE PRECISION PARMS(MAXPRM), UPARMS(MAXPRM), XPARMS(MAXPRM)
      LOGICAL   T, F, FIRSTZ
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI /16/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IF (ICODE.GE.1) THEN
         MSGTXT = 'TV turned back on for this step'
         IF (TVSUP.EQ.1) CALL MSGWRT (2)
         TVSUP = 0
         END IF
C                                       Note: CATOLD & CATBLK are
C                                       now the same
C                                       display parms
      UCHAN = 0
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, PHNAME, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, PHNAME, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE'
         GO TO 990
         END IF
C                                       Setup for I/O
      NXI = CATOLD(KINAX)
      NYI = CATOLD(KINAX+1)
      WINI(1) = IROUND (UBLC(1))
      WINI(2) = IROUND (UBLC(2))
      WINI(3) = IROUND (UTRC(1))
      WINI(4) = IROUND (UTRC(2))
C                                       Initial guess
      PARMS(1) = 0.0D0
      PARMS(2) = 0.0D0
      J = JCODE
      GCODE = 0
      DO 50 I1 = 1,NGAUSS
         PARMS(J+1) = GMAX(I1)
         PARMS(J+2) = GPOS(I1)
         PARMS(J+3) = GWIDTH(I1)
         IF (GWIDTH(I1).GT.0.01) GCODE = 1
         IF (PARMS(J+3).LE.0.0D0) PARMS(J+3) = 0.5
         J = J + 3
 50      CONTINUE
      IG = 3 * NGAUSS + JCODE
      DO 55 I1 = 1,IG
         UPARMS(I1) = PARMS(I1)
 55      CONTINUE
      CALL COPY (MAXPRM, DOCOMP, LLCOMP)
      MSGTXT = 'XGAUD1: solving Gaussians at every pixel not yet done'
      CALL MSGWRT (2)
C                                       Setup for looping
C                                       Loop
      LIM1 = UTRC(1) - UBLC(1) + 1.01
      CALL FILL (7, 1, IPOS)
      IPOS(1) = UBLC(1) + 0.01
      FIRSTY = 0
      IXGRNO = 1
      DO 200 IZ = LBLC(2),LTRC(2)
         FIRSTZ = .TRUE.
         DO 190 IY = LBLC(1),LTRC(1)
            IXGRNO = (IZ-IBLC(2)) * (ITRC(1)-IBLC(1)+1) + IY - IBLC(1) +
     *         1
            LXGRNO = IXGRNO
            CALL TABXG ('READ', XGBUFF, IXGRNO, XGKOLS, XGNUMV, IPOS(2),
     *         NGAU, VPEAK, RESULT, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ XG TABLE'
               GO TO 990
            ELSE IF ((IRET.EQ.0) .AND. (NGAU.LE.0) .AND.
     *         (VPEAK.GE.FCUT)) THEN
C                                       Init. files, first input.
               IF ((FIRSTZ) .AND. (FIRSTY.GT.0) .AND. (NGAUSS.GT.1))
     *            THEN
                  IXGRNO = (IZ-IBLC(2)-1) * (ITRC(1)-IBLC(1)+1) +
     *               FIRSTY - IBLC(1) + 1
                  CALL TABXG ('READ', XGBUFF, IXGRNO, XGKOLS, XGNUMV,
     *               XXPOS, MGAU, TPEAK, RESULT, IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READ XG TABLE'
                     GO TO 990
                     END IF
                  DO 60 I1 = 1,MAXPRM
                     IF (RESULT(I1).NE.FBLANK) XPARMS(I1) = RESULT(I1)
 60                  CONTINUE
                  FIRSTY = 0
                  END IF
               FIRSTZ = .FALSE.
C                                       Init. files, first input.
               CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3),
     *            BOTEMP, IRET)
               BOI = BOTEMP + 1
               WINI(2) = IPOS(2)
               WINI(4) = IPOS(2)
               CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF1,
     *            JBUFSZ, BOI, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'INIT READ INPUT IMAGE'
                  GO TO 990
                  END IF
C                                       Read.
               CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READING INPUT IMAGE'
                  GO TO 990
                  END IF
C                                       Copy to buffer.
               DO 110 I1 = 1,LIM1
                  DATA(I1) = BUFF1(IBIND+I1-1)
                  IF (DATA(I1).EQ.FBLANK) THEN
                     TDATA(I1) = FBLANK
                  ELSE
                     TAU = DATA(I1) / VPEAK
                     IF (TAU.LT.1.E-5) TAU = 1.E-5
                     TDATA(I1) = -LOG(TAU)
                     END IF
 110              CONTINUE
C                                       Call DO1FIT
               CALL DO1FIT (IPOS, UPARMS, PARMS, XPARMS, NGAU, RESULT,
     *            IRET)
               IF (IRET.EQ.99) THEN
                  MSGTXT = 'Quitting at user request'
                  CALL MSGWRT (5)
                  CALL ZCLOSE (LUNI, INDI, I1)
                  GO TO 999
               ELSE IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1180) IRET
                  GO TO 990
                  END IF
               IF ((RESULT(3).NE.FBLANK) .AND. (FIRSTY.EQ.0)) FIRSTY=IY
               IXGRNO = LXGRNO
               DONROW = LXGRNO
               CALL TABXG ('WRIT', XGBUFF, IXGRNO, XGKOLS, XGNUMV,
     *            IPOS(2), NGAU, VPEAK, RESULT, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE XG TABLE'
                  GO TO 990
                  END IF
               DO 115 I1 = 1,MAXPRM
                  IF (RESULT(I1).NE.FBLANK) UPARMS(I1) = RESULT(I1)
 115              CONTINUE
            ELSE IF (IRET.EQ.0) THEN
               DONROW = LXGRNO
C                                       pick up good solution as last
               DO 120 I1 = 1,MAXPRM
                  IF (RESULT(I1).NE.FBLANK) PARMS(I1) = RESULT(I1)
 120              CONTINUE
               END IF
 190        CONTINUE
 200     CONTINUE
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XGAUD1: ERROR',I3,' ON ',A)
 1180 FORMAT ('XGAUD1: DO1FIT ERROR',I3)
      END
      SUBROUTINE DO1FIT (IPOS, UPARMS, PARMS, XPARMS, NGAU, RESULT,
     *   IRET)
C-----------------------------------------------------------------------
C   DO1FIT fits Gaussians to a row of an image and returns the
C   answers in RESULT.
C   Inputs:
C      IPOS     I(7)    BLC (input image) of first value in DATA
C      UPARMS   D(26)   Initial guess (input by user)
C      XPARMS   D(26)   Last fit in row below (0 -> do not use)
C   Values from commons:
C      DATA     D(*)    Input row, magic value blanked.
C      FBLANK   R       Value of blanked pixel.
C      CATBLK   I       Output catalog header (also CATR, CATD)
C      CATOLD   I       Input catalog header (also OLDR, OLDD)
C   In/out:
C      PARMS    D(26)   In: last answer, Out: Answer in fitting units
C   Output:
C      NGAU     I       Actual number Gaussians fit
C      RESULT   R(32)   Output row (parameter answers, errors).
C      IRET     I       Return code   0 => OK
C                               >0 => error, terminate.
C   Output in COMMON
C      CATBLK   I       Catalog header block - revised for slice header
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), NGAU, IRET
      REAL      RESULT(*)
      DOUBLE PRECISION UPARMS(*), PARMS(*), XPARMS(*)
C
      EXTERNAL  AGFUNC
C
      INCLUDE 'XGAUSD.INC'
      REAL      ORANGE(2)
      INTEGER   INFO, IPVT(MAXPRM), ING, INPARM, INPTS, LERR, LABEL,
     *   TERR, LCODE, IERR, NTRY, ITRY, LGAUSS, I, J, K, NVAR
      DOUBLE PRECISION  FJAC(MAXPRM,MAXPRM), TOL, FVEC(NPLIM),
     *   VALVAR(MAXPRM)
      INTEGER   JNPTS, JNPARM
      LOGICAL   REDO
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'XGAUS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVS.INC'
C-----------------------------------------------------------------------
      IRET = 0
      NTRY = 0
C                                       Not last call
      IF (IPOS(1).GE.0) THEN
         REDO = .FALSE.
         LGAUSS = NGAUSS
C                                       Get the initial guess
 10      NTRY = NTRY + 1
         LCODE = ICODE
         IF (TVSUP.GT.0) LCODE = 0
         ING = LGAUSS
         NGAU = LGAUSS
         INPARM = 3 * LGAUSS + JCODE
         INPTS = UTRC(1) - UBLC(1) + 1.01
         ITTER = 0
         NITTER = XNIT + 1.01
C        XBAR = IPOS(1) - 1 - OLDR(KRCRP)
         XBAR = IPOS(1) - 1 - REFPIX
         JJC = JCODE
         CALL XGAUGE (LGAUSS, INPTS, FCUT, LCODE, PIXVAL, UPARMS,
     *      XPARMS, DOCOMP, GCODE, VPEAK, PARMS, FVEC, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Plot it
         IF (LCODE.GE.1) THEN
            ITRY = 0
 20         ITRY = ITRY + 1
            LABEL = ABS (PLTYPE)
            IF (MOD(LABEL,100).LE.0) LABEL = (LABEL/100)*100 + 3
            CALL GTVINI (REDO, IPOS, INPTS, ING, LABEL, RANGE,
     *         DOTV, VPEAK, PARMS, ORANGE, FVEC, FJAC, TERR)
            REDO = .FALSE.
            IF ((TERR.GT.0) .AND. (TERR.LT.100)) THEN
               IRET = TERR
               WRITE (MSGTXT,1000) IRET, 'TV ERROR IN GTVINI'
               GO TO 990
            ELSE IF (TERR.EQ.102) THEN
               IRET = 99
               GO TO 990
            ELSE IF (TERR.EQ.101) THEN
               GO TO 900
C                                       Redo guess
            ELSE IF (TERR.GE.100) THEN
               TERR = 0
               CALL GTVGUS (IPOS, INPTS, ING, LABEL, DOTV, VPEAK, PARMS,
     *            FVEC, FJAC, DOCOMP, TERR)
               IF (TERR.GT.0) THEN
                  IRET = TERR
                  WRITE (MSGTXT,1000) IRET, 'TV ERROR IN GTVGUS'
                  GO TO 990
                  END IF
               IF (ITRY.LT.5) GO TO 20
               END IF
            END IF
C                                       index
 30      NVAR = JJC
         K = JJC
         IF (JJC.GT.0) VALVAR(1) = PARMS(1)
         IF (JJC.EQ.2) VALVAR(2) = PARMS(2)
         DO 50 I = 1,ING
            DO 40 J = 1,3
               K = K + 1
               IF ((LLCOMP(K).GT.0) .AND. (PARMS(K).NE.FBLANK)) THEN
                  NVAR = NVAR + 1
                  IVAR(NVAR) = I
                  JVAR(NVAR) = J
                  VALVAR(NVAR) = PARMS(K)
                  END IF
 40            CONTINUE
 50         CONTINUE
         MVAR = K
         CALL DPCOPY (MAXPRM, PARMS, LPARMS)
C                                       Fit Gaussians
         TOL = 1.D-5
         JNPTS = INPTS
         JNPARM = NVAR
         CALL XGALMS (AGFUNC, JNPTS, JNPARM, VALVAR, FVEC, FJAC, MAXPRM,
     *      TOL, INFO, IPVT)
         IF (INFO.EQ.-1) THEN
            MSGTXT = 'NUMBER OF ITERATIONS EXCEEDED WHEN TRYING TO FIT'
         ELSE
            WRITE (MSGTXT,1020) INFO
         END IF
         IF ((INFO.LE.0) .OR. (INFO.GT.3)) CALL MSGWRT (6)
         IF ((INFO.EQ.0) .OR. (INFO.EQ.4)) GO TO 900
C                                       Get errors and nice units
         ITTER  = ITTER - 1
         JNPTS = INPTS
         INPARM = 3 * ING + JCODE
         JNPARM = NVAR
         CALL REDOAN (JNPTS, JNPARM, VALVAR, PARMS, FVEC, FJAC, MAXPRM,
     *      NGAU, RESULT)
         CALL XGAUCH (ING, INPTS, FVEC, PARMS, LERR)
C                                       non-interactive: drop bad
         IF (LCODE.LT.1) THEN
C                                       restart TV
            IF (LERR.NE.0) THEN
               IF (ICODE.GE.1) THEN
                  TVSUP = 0
                  LCODE = ICODE
                  MSGTXT = 'Restart TV because of failure'
                  CALL MSGWRT (2)
                  J = JJC + 1
                  DO 100 I = 1,ING
                     WRITE (MSGTXT,1100) I, PARMS(J), PARMS(J+1),
     *                  PARMS(J+2)
                     CALL MSGWRT (3)
                     J = J + 3
 100                 CONTINUE
                  REDO = .TRUE.
                  GO TO 10
C                                       null solution
               ELSE
                  GO TO 900
                  END IF
               END IF
C                                       interactive
         ELSE
            CALL GTVMOD (DOTV, DORESI, DOMODL, INPTS, IPOS, ING, NGAUSS,
     *         RANGE, ORANGE, FVEC, PARMS, LERR, TERR)
            IF (TERR.EQ.101) THEN
               GO TO 900
            ELSE IF (TERR.EQ.102) THEN
               IRET = 99
               GO TO 999
            ELSE IF (TERR.EQ.103) THEN
               REDO = .TRUE.
               LGAUSS = ING
               GO TO 10
            ELSE IF (TERR.EQ.105) THEN
               REDO = .TRUE.
               LGAUSS = ING
               GO TO 30
            ELSE IF (TERR.EQ.104) THEN
               TVSUP = 1
               MSGTXT = 'TV turned off until next step'
               CALL MSGWRT (2)
               END IF
C                                       PARMS may have changed
            ITTER  = ITTER - 1
            JNPTS = INPTS
            INPARM = 3 * ING + JCODE
C                                       index
            NVAR = JJC
            K = JJC
            IF (JJC.GT.0) VALVAR(1) = PARMS(1)
            IF (JJC.EQ.2) VALVAR(2) = PARMS(2)
            DO 150 I = 1,ING
               DO 140 J = 1,3
                  K = K + 1
                  IF ((LLCOMP(K).GT.0) .AND. (PARMS(K).NE.FBLANK)) THEN
                     NVAR = NVAR + 1
                     IVAR(NVAR) = I
                     JVAR(NVAR) = J
                     VALVAR(NVAR) = PARMS(K)
                     END IF
 140              CONTINUE
 150           CONTINUE
            CALL DPCOPY (MAXPRM, PARMS, LPARMS)
            JNPARM = NVAR
            MVAR = K
            CALL REDOAN (JNPTS, JNPARM, VALVAR, PARMS, FVEC, FJAC,
     *         MAXPRM, NGAU, RESULT)
            END IF
         GO TO 999
C                                       Blank outputs
 900     CALL RFILL (2*MAXPRM+2*MAXGAU, FBLANK, RESULT)
         NGAU = -1
         END IF
C
 990  IF (IRET.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DO1FIT: ERROR',I4,' ON ',A)
 1020 FORMAT ('FIT ROUTINE RETURNS ERROR CODE',I3)
 1100 FORMAT ('Gaussian',I2,F11.6,F10.2,F10.3)
      END
      SUBROUTINE REDOAN (M, N, X, PARMS, FVEC, FJAC, LDFJAC, NGAU,
     *   RESULT)
C-----------------------------------------------------------------------
C   REDOAN computes a new residual vector, a new covariance, and redoes
C   the answers to be stored in the table
C   Inputs:
C      M        I        Number data points (adj. array dim.)
C      N        I        Number of unknowns (adj. array dim.)
C      X        D(N)     Vector of solutions
C      PARMS    D(*)     all solutions
C      LDFJAC   I        First index of FJAC
C   Output:
C      FVEC     D(M)     residuals
C      FJAC     D(*,*)   work matrix
C      RESULT   R(*)     baseline, slope, amp, center, width...
C-----------------------------------------------------------------------
      INTEGER   M, N, LDFJAC, NGAU
      DOUBLE PRECISION X(N), FVEC(M), FJAC(LDFJAC,N), PARMS(*)
      REAL      RESULT(*)
C
      INCLUDE 'XGAUSD.INC'
      INTEGER   I, J, IPVT(MAXPRM)
      DOUBLE PRECISION TEMP, WA1(MAXPRM), WA2(MAXPRM), WA3(MAXPRM),
     *   QTF(MAXPRM)
C-----------------------------------------------------------------------
C                                       get residual:
C                                       LPARMS better than PARMS!
      I = 1
C     CALL DPCOPY (MAXPRM, PARMS, LPARMS)
      CALL AGFUNC (M, N, X, FVEC, FJAC, I)
C                                       determine IPVT, FJAC
      DO 20 J = 1,N
         IPVT(J) = J
         DO 10 I = 1,N
            FJAC(I,J) = 0.0D0
 10         CONTINUE
 20      CONTINUE
      J = 2
      DO 30 I = 1,M
         CALL AGFUNC (M, N, X, FVEC, WA3, J)
         TEMP = FVEC(I)
         CALL RWUPDT (N, FJAC, LDFJAC, WA3, QTF, TEMP, WA1, WA2)
         J = J + 1
 30      CONTINUE
C                                       fit array into actual PARMS
C     CALL DFILL (MAXPRM, 0.0D0, PARMS)
      DO 40 I = JJC+1,N
         J = 3 * (IVAR(I) - 1) + JVAR(I) + JJC
         PARMS(J) = X(I)
 40      CONTINUE
      IF (JJC.GT.0) PARMS(1) = X(1)
      IF (JJC.EQ.2) PARMS(2) = X(2)
C                                       Get errors and squirrel away
      CALL XGAUFI (M, N, PARMS, IPVT, FJAC, FVEC, NGAU, RESULT)
C
 999  RETURN
      END
      SUBROUTINE XGALMS (FCN, M, N, X, FVEC, FJAC, LDFJAC, TOL, INFO,
     *   IPVT)
C-----------------------------------------------------------------------
C   XGALMS provides an extra interface to the math routine LMSTR1
C   and holds the WORK array (for overlay purposes)
C   Inputs:
C      FCN      EXT      Function to evaluate the model
C      M        I        Number data points (adj. array dim.)
C      N        I        Number of unknowns (adj. array dim.)
C      LDFJAC   I        Number points on first axis of FJAC (adj.
C                           array dim.)
C      TOL      D        Tolerance desired
C   In/out:
C      X        D(N)     Initial guess/ answer
C      FVEC     D(M)     Function (Data - model) evaluation
C      FJAC     D(N,N)   Work matrix
C      INFO     I        Error code: 1 - 3 good, 0 bad input,
C                           4 orthogonal, 5 - 7 poor fit
C      IPVT     D(N)     Permutation matrix
C   See precursor remarks to LMSTR1 or LMSTR for details.
C-----------------------------------------------------------------------
      EXTERNAL  FCN
      INTEGER   M, N, LDFJAC, INFO, IPVT(N)
      DOUBLE PRECISION X(N), FVEC(M), FJAC(LDFJAC,N), TOL
C
      INTEGER   LWA
      DOUBLE PRECISION WA(10000)
      DATA LWA /10000/
C-----------------------------------------------------------------------
C                                       It's just a dummy routine
      CALL LMSTR1 (FCN, M, N, X, FVEC, FJAC, LDFJAC, TOL, INFO, IPVT,
     *   WA, LWA)
C
 999  RETURN
      END
      SUBROUTINE XGAUGE (NG, ND, FC, LCODE, PIXVAL, UPARMS, XPARMS,
     *   DOCOMP, GCODE, VPEAK, RPARMS, FVEC, IERR)
C-----------------------------------------------------------------------
C   XGAUGE obtains an initial guess for the parameters on the
C   Gaussians.  For a single component, it uses moments.  For multiple
C   components, it gets a baseline guess, checks the data, and chooses
C   between the last solution and the user's initial guess.
C   Inputs:
C      NG       I        Number of Gaussians
C      ND       I        Number of data samples
C      FC       R        Flux cutoff
C      PIXVAL   R        No plot if peak > PIXVAL
C      UPARMS   D(26)    User's initial guess
C      DOCOMP   I(26)    > 0 -> do the parameter
C      GCODE    I        > 0 -> there is a user initial guess
C   In.out:
C      LCODE    I        in: 1,2 => plot possible, out: 1 => wanted
C      XPARMS   D(26)    another initial gues
C                           RETURNS 0.0 when XPARMS was considered
C      RPARMS   D(3NG)   In: previous solution
C                        Out:Guess to use
C   Output:
C      FVEC     D(*)     Buffer for computation
C      IERR     I        0 => ok, 1 => all data too low
C                        2 => input error
C   Common output:
C      LLCOMP   I(26)    copy DOCOMP unless previous guess is
C                                 used.  Then leave it alone.
C-----------------------------------------------------------------------
      INTEGER   NG, ND, LCODE, DOCOMP(*), GCODE, IERR
      REAL      FC, PIXVAL, VPEAK
      DOUBLE PRECISION UPARMS(*), XPARMS(*), RPARMS(*), FVEC(*)
C
      INCLUDE 'XGAUSD.INC'
      DOUBLE PRECISION PARMS(MAXPRM), FJAC(MAXPRM), VALVAR(MAXPRM)
      INTEGER   JD, IJ, IM, I, J, K, NVAR
      REAL      B, XM, TS, TVS, TWS, R, BLS, BLO, BL, BLP, BLM, RMS(3)
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Test inputs
      IERR = 2
      IF ((ND.LT.3*NG+JJC+6) .OR. (ND.GT.NPLIM)) GO TO 999
      DO 5 I = 1,MAXPRM
         PARMS(I) = UPARMS(I)
 5       CONTINUE
C                                       Baseline slope always zero
C                                       Value equal current peak value
      BLS = 0.0
      BLO = VPEAK
C                                       Clear summing variables
      TS = 0.
      TVS = 0.
      TWS = 0.
      IM = 0
      XM = 0.
C                                       FC replaced by 0.1
      DO 40 I = 1,ND
         IF (TDATA(I).NE.FBLANK) THEN
            R = I + XBAR
C            BL = DATA(I) - BLO - BLS * R
            BL = TDATA(I)
            IF (BL.GE.0.1) THEN
               IF (NG.LE.1) THEN
                  TS = TS + BL
                  TVS = TVS + BL * R
                  TWS = TWS + BL * R * R
                  END IF
               IF ((I.NE.1) .AND. (I.NE.ND) .AND. (TDATA(I-1).NE.FBLANK)
     *               .AND. (TDATA(I+1).NE.FBLANK)) THEN
C                  BLM = DATA(I-1) - BLO - BLS * (R-1.)
C                  BLP = DATA(I+1) - BLO - BLS * (R+1.)
                  BLM = TDATA(I-1)
                  BLP = TDATA(I+1)
                  B = (BLM + BLP + BL) / 3.
                  IF (B.GE.0.1) THEN
                     IF (B.GE.XM) THEN
                        XM = B
                        IM = I
                        END IF
                     END IF
                  END IF
               END IF
            END IF
 40      CONTINUE
C                                       try again
      IF (IM.LT.1) THEN
         DO 45 I = 1,ND
            IF (TDATA(I).NE.FBLANK) THEN
               R = I + XBAR
               BL = TDATA(I)
               IF (BL.GE.0.1) THEN
                  IF (NG.LE.1) THEN
                     TS = TS + BL
                     TVS = TVS + BL * R
                     TWS = TWS + BL * R * R
                     END IF
                  IF ((I.NE.1) .AND. (I.NE.ND) .AND.
     *               (TDATA(I-1).NE.FBLANK) .AND.
     *               (TDATA(I+1).NE.FBLANK)) THEN
                     BLM = TDATA(I-1)
                     BLP = TDATA(I+1)
                     B = (BLM + BLP + BL) / 3.
                     IF (B.GE.0.1) THEN
                        IF (B.GE.XM) THEN
                           XM = B
                           IM = I
                           END IF
                        END IF
                     END IF
                  END IF
               END IF
 45         CONTINUE
         END IF
C                                       Find anything?
      IERR = 1
      IF (IM.LT.1) GO TO 999
C                                       Yes: make a guess
      IF (NG.LE.1) THEN
C         RPARMS(1+JJC) = DATA(IM) - BLO - BLS * (IM+XBAR)
         RPARMS(1+JJC) = TDATA(IM)
         TVS = TVS / TS
         TWS = TWS / TS - TVS * TVS
         RPARMS(2+JJC) = 0.8 * TVS + 0.2 * (IM + XBAR)
         BL = 2.0 * SQRT (ABS(TWS))
         IF (BL.GT.0.3*ND) BL = 0.3*ND
         IF (BL.LT.1.5) BL = 1.5
         RPARMS(3+JJC) = BL
         TS = 0.
C                                       Fill in amplitudes
      ELSE IF (GCODE.GT.0) THEN
         DO 55 I = 1,NG
            K = JJC + 3 * I - 2
            IF (PARMS(K+2).LE.0.0D0) PARMS(K+2) = 1.D-3
            IF ((PARMS(K).LE.0.0D0) .AND. (DOCOMP(K).GT.0)) THEN
               J = PARMS(K+1) - XBAR + 0.5
               IF ((J.GT.0) .AND. (J.LE.ND)) PARMS(K) = TDATA(J)
               IF (PARMS(K).LT.0.0D0) PARMS(K) = 0.0D0
               END IF
 55         CONTINUE
         END IF
      IF (JJC.GT.0) THEN
         PARMS(1) = BLO
         RPARMS(1) = BLO
         END IF
      IF (JJC.EQ.2) THEN
         PARMS(2) = BLS
         RPARMS(2) = BLS
         END IF
C                                       Compute the RMSs
      IF (NG.NE.1) THEN
         TS = 0.0
         J = 3 * NG + JJC
C                                       no user guess
         IF (GCODE.LE.0) THEN
C                                       guess old one at current flux
            IF (XPARMS(JJC+1).NE.0.0D0) THEN
               CALL DPCOPY (MAXPRM, XPARMS, PARMS)
            ELSE
               CALL DPCOPY (MAXPRM, RPARMS, PARMS)
               END IF
            IF (JJC.GT.0) PARMS(1) = BLO
            DO 72 I = 1,NG
               K = JJC + 3 * I - 2
               IJ = PARMS(K+1) + 0.5D0 - XBAR
               IF ((IJ.GT.0) .AND. (IJ.LE.ND)) THEN
                  IF (TDATA(IJ).NE.FBLANK) PARMS(K) = 0.85 * TDATA(IJ)
                  END IF
 72            CONTINUE
            END IF
         I = 1
         ITTER = ITTER - 1
         JD = ND
         NVAR = JJC
         K = JJC
         IF (JJC.GT.0) VALVAR(1) = PARMS(1)
         IF (JJC.EQ.2) VALVAR(2) = PARMS(2)
         DO 74 I = 1,NG
            DO 73 J = 1,3
               K = K + 1
               IF ((LLCOMP(K).GT.0) .AND. (PARMS(K).NE.FBLANK)) THEN
                  NVAR = NVAR + 1
                  IVAR(NVAR) = I
                  JVAR(NVAR) = J
                  VALVAR(NVAR) = PARMS(K)
                  END IF
 73            CONTINUE
 74         CONTINUE
         IJ = NVAR
         MVAR = K
         I = 1
         CALL DPCOPY (MAXPRM, PARMS, LPARMS)
         CALL AGFUNC (JD, IJ, VALVAR, FVEC, FJAC, I)
         FJAC(1) = 0.0D0
         DO 75 I = 1,ND
            FJAC(1) = FJAC(1) + FVEC(I) * FVEC(I)
 75         CONTINUE
         RMS(1) = SQRT (FJAC(1) / ND)
C                                       last solution
         ITTER = ITTER - 1
         JD = ND
         NVAR = JJC
         K = JJC
         IF (JJC.GT.0) THEN
            RPARMS(1) = BLO
            VALVAR(1) = RPARMS(1)
            IF (JJC.EQ.2) VALVAR(2) = RPARMS(2)
            END IF
         DO 79 I = 1,NG
            DO 78 J = 1,3
               K = K + 1
               IF ((LLCOMP(K).GT.0) .AND. (RPARMS(K).NE.FBLANK)) THEN
                  NVAR = NVAR + 1
                  IVAR(NVAR) = I
                  JVAR(NVAR) = J
                  VALVAR(NVAR) = RPARMS(K)
                  END IF
 78            CONTINUE
 79         CONTINUE
         IJ = NVAR
         MVAR = K
         I = 1
         CALL DPCOPY (MAXPRM, RPARMS, LPARMS)
         CALL AGFUNC (JD, IJ, VALVAR, FVEC, FJAC, I)
         FJAC(1) = 0.0D0
         DO 80 I = 1,ND
            FJAC(1) = FJAC(1) + FVEC(I) * FVEC(I)
 80         CONTINUE
         RMS(2) = SQRT (FJAC(1) / ND)
C                                       extra guess
         IF (XPARMS(3).EQ.0.0D0) THEN
            RMS(3) = 1.E10
         ELSE
            ITTER = ITTER - 1
            JD = ND
            NVAR = JJC
            K = JJC
            IF (JJC.GT.0) THEN
               PARMS(1) = BLO
               VALVAR(1) = PARMS(1)
               IF (JJC.EQ.2) VALVAR(2) = PARMS(2)
               END IF
            DO 84 I = 1,NG
               DO 83 J = 1,3
                  K = K + 1
                  IF ((LLCOMP(K).GT.0) .AND. (XPARMS(K).NE.FBLANK)) THEN
                     NVAR = NVAR + 1
                     IVAR(NVAR) = I
                     JVAR(NVAR) = J
                     VALVAR(NVAR) = XPARMS(K)
                     END IF
 83               CONTINUE
 84            CONTINUE
            I = 1
            IJ = NVAR
            MVAR = K
            CALL DPCOPY (MAXPRM, XPARMS, LPARMS)
            CALL AGFUNC (JD, IJ, VALVAR, FVEC, FJAC, I)
            FJAC(1) = 0.0D0
            DO 85 I = 1,ND
               FJAC(1) = FJAC(1) + FVEC(I) * FVEC(I)
 85            CONTINUE
            RMS(3) = SQRT (FJAC(1) / ND)
            END IF
         IF ((RMS(3).LT.RMS(1)) .AND. (RMS(3).LT.RMS(2))) THEN
            DO 90 I = 1,MAXPRM
               RPARMS(I) = XPARMS(I)
               LLCOMP(I) = DOCOMP(I)
 90            CONTINUE
            GO TO 100
         ELSE IF (RMS(1).LT.RMS(2)) THEN
            DO 95 I = 1,MAXPRM
               RPARMS(I) = PARMS(I)
               LLCOMP(I) = DOCOMP(I)
 95            CONTINUE
            END IF
         CALL DPCOPY (MAXPRM, RPARMS, LPARMS)
         END IF
C                                       Return answers/guesses
C                                       test desire to plot
 100  IERR = 0
      IF ((XM/3.0.GT.PIXVAL) .AND. (TS.LT.(4.*FC+PIXVAL)/3.))
     *   LCODE = 0
      IF (RMS(3).LT.1.E9) CALL DFILL (MAXPRM, 0.0D0, XPARMS)
C
 999  RETURN
      END
      SUBROUTINE XGAUFI (INPTS, NP, PARMS, IPVT, FJAC, FVEC, NGAU,
     *   RESULT)
C-----------------------------------------------------------------------
C   XGAUFI determines the errors in the fit and converts the results
C   to normal units for output.
C   Inputs:
C      INPTS    I          Number of data samples
C      NP       I          Number of parameters
C      PARMS    D(26)      Answers from LMSTR1
C      IPVT     I(26)      from LMSTR1
C      FJAC     D(26,26)   from LMSTR1
C      FVEC     D(*)       from LMSTR1
C   Output:
C      NGAU     I          Actual # Gaussians with valid results
C      RESULT   R(36)      Answers then errors in PIXELS
C                             2 baseline, 3 each of 8 Gaussians
C                             Errors: 2 baseline 3 ea of 8 Gaussians
C                             8 of flux, error flux
C                             Physical units requires scale by
C                             * abs(OLDR(KRCIC)) for width
C                             * OLDR(KRCIC) for center
C                             + OLD(KDCRV) for center
C                             / CATR(KRCIC) for slope
C-----------------------------------------------------------------------
      INCLUDE 'XGAUSD.INC'
C
      INTEGER   INPTS, NP, IPVT(*), NGAU
      DOUBLE PRECISION PARMS(*), FJAC(MAXPRM,MAXPRM), FVEC(*)
      REAL      RESULT(*)
C
      DOUBLE PRECISION OLDD(64), EPARMS(MAXPRM), ENORM, FNORM, TOL,
     *   WORK(MAXPRM), SFJAC(MAXPRM,MAXPRM)
      REAL      OLDR(128), HFAC
      INTEGER   CATOLD(256), I, NG, JC, IL, JNPTS, JP, JJ
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATOLD
      EQUIVALENCE (CATOLD, OLDR, OLDD)
      DATA HFAC /1.064467/
C-----------------------------------------------------------------------
C                                       Get errors with LMSTR1 outputs
      TOL = 1.D-5
      JNPTS = INPTS
      FNORM = ENORM (JNPTS, FVEC)
      JP = MAXPRM * MAXPRM
      CALL DPCOPY (JP, FJAC, SFJAC)
      JP = NP
      CALL GETERR (IPVT, SFJAC, EPARMS, JP, INPTS, MAXPRM, FNORM, WORK,
     *   TOL, IVAR, JVAR, JJC)
C                                       Convert to output
      IF (OLDR(KRCIC).EQ.0.0) OLDR(KRCIC) = 1.0
      NG = (NP - JJC) / 3
      JJ = 2
      JC = JJC
      CALL RFILL (2*MAXPRM+2*MAXGAU, FBLANK, RESULT)
      NGAU = 0
      DO 10 I = 1,MAXGAU
         IF (((LLCOMP(1+JC).GT.0) .OR. (LLCOMP(2+JC).GT.0) .OR.
     *      (LLCOMP(3+JC).GT.0)) .AND. (PARMS(1+JC).NE.0.0D0)) THEN
            NGAU = NGAU + 1
            RESULT(1+JJ) = PARMS(1+JC)
            RESULT(2+JJ) = PARMS(2+JC)
            RESULT(3+JJ) = ABS (PARMS(3+JC))
            IF ((EPARMS(1+JC).LE.0.0D0) .AND. (I.GT.1)) THEN
               RESULT(MAXPRM+1+JJ) = RESULT(MAXPRM+1+JJ-3)
            ELSE
               RESULT(MAXPRM+1+JJ) = EPARMS(1+JC)
               END IF
            IF ((EPARMS(2+JC).LE.0.0D0) .AND. (I.GT.1)) THEN
               RESULT(MAXPRM+2+JJ) = RESULT(MAXPRM+2+JJ-3)
            ELSE
               RESULT(MAXPRM+2+JJ) = EPARMS(2+JC)
               END IF
            IF ((EPARMS(3+JC).LE.0.0D0) .AND. (I.GT.1)) THEN
               RESULT(MAXPRM+3+JJ) = RESULT(MAXPRM+3+JJ-3)
            ELSE
               RESULT(MAXPRM+3+JJ) = EPARMS(3+JC)
               END IF
         ELSE IF ((PARMS(1+JC).NE.0.0D0) .AND. (PARMS(2+JC).NE.0.0D0)
     *      .AND. (PARMS(3+JC).NE.0.0D0)) THEN
            RESULT(1+JJ) = PARMS(1+JC)
            RESULT(2+JJ) = PARMS(2+JC)
            RESULT(3+JJ) = ABS (PARMS(3+JC))
            RESULT(MAXPRM+1+JJ) = 0.0
            RESULT(MAXPRM+2+JJ) = 0.0
            RESULT(MAXPRM+3+JJ) = 0.0
            END IF
         JJ = JJ + 3
         JC = JC + 3
 10      CONTINUE
C                                       flux
      JC = 2
      IL = 2 * MAXPRM + 1
      DO 20 I = 1,MAXGAU
         IF ((RESULT(1+JC).NE.FBLANK) .AND. (RESULT(3+JC).NE.FBLANK))
     *      THEN
            RESULT(IL) = HFAC * RESULT(1+JC) * RESULT(3+JC)
            RESULT(IL+1) = HFAC * SQRT ((RESULT(3+JC) *
     *         RESULT(MAXPRM+1+JC)) ** 2 + (RESULT(1+JC) *
     *         RESULT(MAXPRM+3+JC)) ** 2)
            END IF
         IL = IL + 2
         JC = JC + 3
 20      CONTINUE
C                                       baseline
      IF (JJC.GT.0) THEN
         RESULT(1) = PARMS(1)
         RESULT(MAXPRM+1) = EPARMS(1)
         END IF
      IF (JJC.EQ.2) THEN
         RESULT(2) = PARMS(2)
         RESULT(MAXPRM+2) = EPARMS(2)
      ELSE IF (JJC.EQ.0) THEN
         RESULT(2) = FBLANK
         RESULT(MAXPRM+2) = FBLANK
      ELSE
         RESULT(2) = 0.0
         RESULT(MAXPRM+2) = 0.0
         END IF
C
 999  RETURN
      END
      SUBROUTINE GETERR (IPVT, FJAC, PARERR, MP, NDATA, MD, FNORM, WA,
     *   TOL, IVAR, JVAR, JJC)
C-----------------------------------------------------------------------
C   This subroutine calculates the errors on the fitted parameters.
C   Inputs:
C      IPVT    I(MP)   Defines a permutation matrix P such that
C                      JAC*P = Q*R, where JAC is the final calculated
C                      Jacobian, Q is orthogonal (not stored), and R is
C                      upper triangular with diagonal elements of
C                      nonincreasing magnitude column J of P is column
C                      IPVT(J) of the identity matrix. (See FJAC below)
C      FJAC    D(MD,MP)   The upper MP by MP submatrix of FJAC contains
C                      an upper triangular matrix R with diagonal
C                      elements of nonincreasing magnitude such that
C                           T     T           T
C                          P *(JAC *JAC)*P = R *R,
C                      where P is a permutation matrix and JAC is the
C                      final calculated Jacobian. Column J of P is
C                      column IPVT(J) (see above) of the identity
C                      matrix.
C      MP      I       Number of parameters in fitted function.
C      NDATA   I       Number of data points fitted.
C      MD      I       Maximum no. of data points allowed for in FJAC
C      FNORM   D       Euclidian norm of solution vector.
C      WA      D(MP)   work array.
C  Output:
C      FJAC    D       modified by COVAR
C      PARERR  D(MP)   error in fitted parameters.
C      TOL     D       tolerance used in call to LMDER1.
C-----------------------------------------------------------------------
      INTEGER   MD, MP, IPVT(MP), NDATA, IVAR(*), JVAR(*), JJC
      DOUBLE PRECISION FJAC(MD,MP), PARERR(MP), FNORM, WA(MP), TOL
C
      DOUBLE PRECISION EPSILN
      INTEGER   J, JC
C-----------------------------------------------------------------------
C                                       Calculate error following
C                                       Argonne write up
C      NPARMS = NGAUSS * 3
C                                       Is this right ??????
C                                       changed NPARMS to MP
      CALL DFILL (MP, 0.0D0, PARERR)
      EPSILN = FNORM / SQRT (REAL(NDATA-MP))
      CALL COVAR (MP, FJAC, MD, IPVT, TOL, WA)
      DO 100 J = JJC+1,MP
         JC = 3 * (IVAR(J) - 1) + JVAR(J) + JJC
         PARERR(JC) = EPSILN * SQRT (FJAC(J,J))
 100     CONTINUE
      IF (JJC.GT.0) PARERR(1) = EPSILN * SQRT (FJAC(1,1))
      IF (JJC.EQ.2) PARERR(2) = EPSILN * SQRT (FJAC(2,2))
C
 999  RETURN
      END
      SUBROUTINE AGFUNC (M, N, VALVAR, FVEC, FJROW, IFLAG)
C-----------------------------------------------------------------------
C   This routine is called by the Argonne package to calculate the
C   difference between the current fit and the actual data OR the
C   Jacobian for this difference.
C   Inputs:
C        M        I      Number of data points in slice (adj array dim)
C        N        I      No. of parameters (adj. array dim.;
C                        NGAUSS * 3)
C        VALVAR   D(N)   parameters of gaussian components,
C                        GMAX(1), GPOS(1), GWIDTH(1), GMAX(2), ...
C        IFLAG    I      1=calculate difference for current guess.
C                        2=calculate jacobian for current guess.
C    COMMON GDATA
C        DATA     R(?)   Origional slice data points.
C        IDOPOS   I(4)   -1 means hold corresponding position
C                           parameter constant.
C        IDOMAX   I(4)   -1 means hold corresponding maximum
C                           amplitude parameter constant.
C        IDOWTH   I(4)   -1 means hold corresponding half
C                           width parameter constant.
C        ITTER    I      number of calls to evaluate FVEC.
C    Outputs:
C        FVEC     D(M)   Slice data points minus data points
C                           evaluated for current guess.
C        FJROW    D(N)   Row (IFLAG - 1) of Jacobian.
C-----------------------------------------------------------------------
      INTEGER   N, M, IFLAG
      DOUBLE PRECISION VALVAR(N), FVEC(M), FJROW(N)
C
      INCLUDE 'XGAUSD.INC'
      DOUBLE PRECISION AMP, POS, SIG, EFACT, RES2, TSIG2, X, HALFAC,
     *   CONT, TAU, A
      INTEGER   IDATA, IAMP, IPOS, ISIG, I, K
      INCLUDE 'INCS:DDCH.INC'
      DATA HALFAC /2.77258872D0/
C-----------------------------------------------------------------------
C                                       Determine difference between
C                                       data and current fit.
      IF (IFLAG.LE.1) THEN
         ITTER = ITTER + 1
         IF (ITTER.GT.NITTER) THEN
            IFLAG = -1
            GO TO 999
            END IF
C                                       get full parms array
C                                       must be set in COMMON
C        CALL DFILL (MAXPRM, 0.0D0, LPARMS)
C                                       defend against blanks
         DO 5 I = 1,MAXPRM
            IF (LPARMS(I).EQ.FBLANK) LPARMS(I) = 0.0D0
 5          CONTINUE
C                                       transfer varying values
         DO 10 I = JJC+1,N
            K = 3 * (IVAR(I) - 1) + JVAR(I) + JJC
            LPARMS(K) = VALVAR(I)
 10         CONTINUE
         IF (JJC.GT.0) LPARMS(1) = VALVAR(1)
         IF (JJC.EQ.2) LPARMS(2) = VALVAR(2)
C                                       compute residual
         DO 20 IDATA = 1,M
            FVEC(IDATA) = DATA(IDATA)
            IF (FVEC(IDATA).EQ.FBLANK) THEN
               FVEC(IDATA) = 0.0D0
            ELSE
               X = IDATA + XBAR
               CONT = LPARMS(1)
               IF (JJC.EQ.2) CONT = CONT + LPARMS(2) * X
               TAU = 0.0D0
               DO 15 I = JJC+1,MVAR,3
                  IAMP = I
                  AMP = LPARMS(IAMP)
                  IF (AMP.NE.0.0D0) THEN
                     IPOS = IAMP + 1
                     ISIG = IAMP + 2
                     POS = LPARMS(IPOS)
                     SIG = LPARMS(ISIG)
                     RES2 = (X - POS) / SIG
                     RES2 = HALFAC * RES2 * RES2
                     IF (RES2.LE.69.0D0) TAU = TAU + AMP * EXP (-RES2)
                     END IF
 15               CONTINUE
               FVEC(IDATA) = FVEC(IDATA) - CONT * EXP(-TAU)
               END IF
 20         CONTINUE
C                                       Calculate Jacobian.
      ELSE
         IDATA = IFLAG - 1
         X = IDATA + XBAR
         CONT = LPARMS(1)
         IF (JJC.EQ.2) CONT = CONT + LPARMS(2) * X
         TAU = 0.0D0
         DO 110 I = JJC+1,MVAR,3
            IAMP = I
            AMP = LPARMS(IAMP)
            IF (AMP.NE.0.0D0) THEN
               IPOS = IAMP + 1
               ISIG = IAMP + 2
               POS = LPARMS(IPOS)
               SIG = LPARMS(ISIG)
               RES2 = (X - POS) / SIG
               RES2 = HALFAC * RES2 * RES2
               IF (RES2.LE.69.0D0) TAU = TAU + AMP * EXP (-RES2)
               END IF
110         CONTINUE
         A = CONT * EXP (-TAU)
         DO 120 I = JJC+1,N
            IAMP = 3 * (IVAR(I) - 1) + 1 + JJC
            IPOS = IAMP + 1
            ISIG = IAMP + 2
            FJROW(I) = 0.0D0
            AMP = LPARMS(IAMP)
            POS = LPARMS(IPOS)
            SIG = LPARMS(ISIG)
            RES2 = HALFAC * (X - POS) * (X - POS)
            TSIG2 = RES2 / (SIG * SIG)
            IF (TSIG2.LE.69.0D0) THEN
               EFACT = EXP (-TSIG2) * A
               IF (JVAR(I).EQ.1) THEN
                  FJROW(I) = EFACT
               ELSE
                  EFACT = 2.0D0 * EFACT * AMP / (SIG * SIG)
                  IF (JVAR(I).EQ.2) THEN
                     FJROW(I) = HALFAC * EFACT * (X-POS)
                  ELSE IF (JVAR(I).EQ.3) THEN
                     FJROW(I) = EFACT * RES2 / SIG
                     END IF
                  END IF
               END IF
 120        CONTINUE
         IF (JJC.GT.0) FJROW(1) = -EXP (-TAU)
         IF (JJC.EQ.2) FJROW(2) = X * FJROW(1)
         END IF
C
 999  RETURN
      END
      SUBROUTINE GTVINI (REDO, IPOS, INPTS, NG, LABEL, PIXR, DOTV,
     *   VPEAK, PARMS, ORANGE, FVEC, FJAC, IERR)
C-----------------------------------------------------------------------
C   GTVINI initializes the TV for a XGAUS plot, plots axis labels,
C   and, if requested, plots the data.
C   Inputs:
C      IPOS     I(7)     Position in cube first point in row.
C      INPTS    I        Number of points in row.
C      NG       I        Number of Gaussians
C      LABEL    I        Requested label type
C      PIXR     R(2)     Requested plot value range
C      DOTV     R        > 0. => plot data
C      PARMS    D(26)    Initial guess
C   Output:
C      ORANGE   R(2)     Actual plot range in plot units
C      FVEC     D(*)     Scratch buffer
C      FJAC     D(26)    Scratch buffer
C      IERR     I        > 0 => plot failed
C                        101 => bad initial guess
C                        102 => DIE
C-----------------------------------------------------------------------
      LOGICAL   REDO
      INTEGER   IPOS(7), INPTS, NG, LABEL, IERR
      REAL      PIXR(2), DOTV, ORANGE(2), VPEAK
      DOUBLE PRECISION PARMS(*), FVEC(*), FJAC(*)
C
      INCLUDE 'XGAUSD.INC'
      CHARACTER TEMP*1, TEXT(2)*80, MSGBUF*132, NTEXT*80, FIRSTC*1
      REAL      XBLC(7), XTRC(7), PBLC(2), PTRC(2), YGAP, CH(4), XYRATO,
     *   X, XX, Y, XFAC, XOFF, DX, DY, FQFINC
      INTEGER   IDROP(2), IX1, IX2, IY1, IY2, ICHL, ICHB, ICHR, ICHT,
     *   NXA, NYA, I, J, JERR, JNPTS, IJ, ISP(4), IDX, ITC1(MAXGAU),
     *   IT1, ITC2(MAXGAU), IT2, ITC3(MAXGAU), IT3, I4XTRA, IDY, K,
     *   TVWIND(4), TVSIZE(2), SCRTCH(4096)
      LOGICAL   T, F, BLAST, BNEXT, FIRST
      DOUBLE PRECISION FQFREQ, SUMS, NSUM, VALVAR(MAXPRM)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVS.INC'
      INCLUDE 'INCS:DCAT.INC'
      SAVE FIRST
      DATA T, F, FIRST /.TRUE.,.FALSE.,.TRUE./
C-----------------------------------------------------------------------
      CALL YHOLD ('ONNN', IERR)
      IF (FIRST) THEN
         IGR1 = UCHAN + 0.01
         IF ((IGR1.LE.0) .OR. (IGR1.GT.7)) THEN
            IGR1 = 1
            IGR2 = 2
            IGR3 = 3
            IGR4 = 4
            IGR5 = 5
            IGR7 = 7
         ELSE
            IGR2 = IGR1
            IGR3 = IGR1
            IGR4 = IGR1
            IGR5 = IGR1
            IGR7 = IGR1
            END IF
         IGR1 = IGR1 + NGRAY
         IGR2 = IGR2 + NGRAY
         IGR3 = IGR3 + NGRAY
         IGR4 = IGR4 + NGRAY
         IGR5 = IGR5 + NGRAY
         IGR7 = IGR7 + NGRAY
         DO 5 I = 1,NGRAY+NGRAPH
            CALL YSLECT ('OFFF', I, 0, INBUF, IERR)
            IF (IERR.NE.0) GO TO 999
 5          CONTINUE
         FIRST = .FALSE.
         END IF
      CALL YSLECT ('ONNN', IGR1, 0, INBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (IGR1.NE.IGR2) THEN
         CALL YSLECT ('ONNN', IGR2, 0, INBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL YSLECT ('ONNN', IGR3, 0, INBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL YSLECT ('ONNN', IGR4, 0, INBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL YSLECT ('ONNN', IGR7, 0, INBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       "Slice" corners
      IDROP(1) = 0
      IDROP(2) = 0
      DO 10 I = 1,7
         XBLC(I) = IPOS(I)
         XTRC(I) = XBLC(I)
 10      CONTINUE
      XTRC(1) = XBLC(1) + INPTS - 1
C                                       Set PIX ranges
      ORANGE(1) = PIXR(1)
      ORANGE(2) = PIXR(2)
C                                       Default: actual range
      IF (PIXR(2).LE.PIXR(1)) THEN
         ORANGE(1) = 1.0E10
         ORANGE(2) = -ORANGE(1)
         DO 15 I = 1,INPTS
            IF (DATA(I).NE.FBLANK) THEN
               IF (DATA(I).LT.ORANGE(1)) ORANGE(1) = DATA(I)
               IF (DATA(I).GT.ORANGE(2)) ORANGE(2) = DATA(I)
               END IF
 15         CONTINUE
         XFAC = ORANGE(2) - ORANGE(1)
         ORANGE(2) = ORANGE(2) + 0.10 * XFAC
         ORANGE(1) = ORANGE(1) - 0.10 * XFAC
         END IF
      CATR(IRRAN) = ORANGE(1)
      CATR(IRRAN+1) = ORANGE(2)
      XFAC = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      XOFF = 40000.0 - XFAC * CATR(IRRAN+1)
      PBLC(2) = ORANGE(1) * XFAC + XOFF
      PTRC(2) = ORANGE(2) * XFAC + XOFF
C                                       Label inits
      LOCNUM = 1
      FQFINC = 0.0
      FQFREQ = 0.0D0
      CALL SLBINI (IDROP, INPTS, ORANGE, PBLC, PTRC, XBLC, XTRC,
     *   FQFREQ, FQFINC, CATBLK(IIDEP), LABEL, YGAP, CH, TEXT, NTEXT)
      ORANGE(1) = XFAC*ORANGE(1) + XOFF
      ORANGE(2) = XFAC*ORANGE(2) + XOFF
      CALL YWINDO ('READ', TVWIND, IERR)
      IF (IERR.NE.0) THEN
         TVWIND(1) = 1
         TVWIND(2) = 1
         TVWIND(3) = MAXXTV(1)
         TVWIND(4) = MAXXTV(2)
         END IF
      TVSIZE(1) = TVWIND(3) - TVWIND(1) + 1
      TVSIZE(2) = TVWIND(4) - TVWIND(2) + 1
      XYRATO = (PTRC(2) - PBLC(2)) / (PTRC(1) - PBLC(1))
      IX1 = PBLC(1) + .5
      IY1 = PBLC(2) + .5
      IX2 = PTRC(1) + .5
      IY2 = PTRC(2) + .5
      ICHL = CH(1) * CSIZTV(1) + .5
      ICHB = CH(2) * CSIZTV(2) + .5
      ICHR = CH(3) * CSIZTV(1) + .5
      ICHT = CH(4) * CSIZTV(2) + .5
      NYA = TVSIZE(2) - ICHT -ICHB -1
      NXA = TVSIZE(1) - ICHL - ICHR - 1
C                                       compute scaling
      X = IX2
      X = ABS (X - IX1)
      XX = X * XYRATO
      Y = IY2
      Y = ABS (Y - IY1)
      IF ((XX.LE.0.0) .OR. (Y.LE.0.0)) THEN
         MSGTXT = 'SCALING ERROR'
         CALL MSGWRT (8)
         IERR = 1
         GO TO 999
         END IF
      IF ((XX/Y).LE.FLOAT(NXA)/FLOAT(NYA)) THEN
         SCALEY = NYA / Y
         SCALEX = SCALEY * Y / X * FLOAT(TVSIZE(1)) / FLOAT(TVSIZE(2))
      ELSE
         SCALEX = NXA / X
         SCALEY = SCALEX * X / Y
         END IF
C
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
      RX0 = ICHL + MAX (0, TVSIZE(1)-NXA) / 2 + TVWIND(1)
      RY0 = ICHB + MAX (0, TVSIZE(2)-NYA) / 2 + TVWIND(2)
C                                       Put stuff in image catalog.
      CATBLK(IIWIN  ) = IX1
      CATBLK(IIWIN+1) = IY1
      CATBLK(IIWIN+2) = IX2
      CATBLK(IIWIN+3) = IY2
      CATBLK(IICOR  ) = RX0 + 0.5
      CATBLK(IICOR+1) = RY0 + 0.5
      CATBLK(IICOR+2) = RX0 + X * SCALEX + 0.5
      CATBLK(IICOR+3) = RY0 + Y * SCALEY + 0.5
      CATBLK(IIPLT) = 5
      CATBLK(IIOTH) = LABEL
      CATBLK(IIOTH+1) = IDROP(1)
      CATBLK(IIOTH+2) = IDROP(2)
      I4XTRA = IIOTH + 3
      CATR(I4XTRA  ) = XBLC(1)
      CATR(I4XTRA+1) = XBLC(2)
      CATR(I4XTRA+2) = XTRC(1)
      CATR(I4XTRA+3) = XTRC(2)
C                                       Update image catalog
      CALL YCINIT (IGR1, INBUF)
      CALL YCWRIT (IGR1, CATBLK(IICOR), CATBLK, INBUF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TV IMAGE CATALOG ERROR'
         CALL MSGWRT (6)
         END IF
C                                       clear screen
      CALL YZERO (IGR1, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (IGR2.NE.IGR1) THEN
         CALL YCINIT (IGR2, INBUF)
         CALL YCINIT (IGR3, INBUF)
         CALL YCINIT (IGR4, INBUF)
         CALL YCINIT (IGR7, INBUF)
         CALL YZERO (IGR2, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL YZERO (IGR3, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL YZERO (IGR4, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL YZERO (IGR7, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       set scale for plot routines
      RX0 = RX0 - PBLC(1) * SCALEX + 0.5
      RY0 = RY0 - PBLC(2) * SCALEY + 0.5
C                                       label the plot
      IGR = IGR1
      CALL TVLAB (PBLC, PTRC, LABEL, YGAP, TEXT, NTEXT, CH, F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       add pixel coordinates
      DX = PTRC(1)*SCALEX + RX0 - 8.5 * CSIZTV(1)
      DY = PTRC(2)*SCALEY + RY0 - 2.0 * CSIZTV(2)
      IDX = DX + 0.5
      IDY = DY + 0.5
      WRITE (MSGBUF,1060) IPOS(2)
      CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:7), INBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      DY = DY - 1.5 * CSIZTV(2)
      IDY = DY + 0.5
      WRITE (MSGBUF,1061) IPOS(3)
      CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:7), INBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      DY = DY - 1.5 * CSIZTV(2)
      IDY = DY + 0.5
      PLTRMS(1) = IDX
      PLTRMS(2) = IDY
C                                       plot data
      IF (DOTV.GT.0.) THEN
         BLAST = .TRUE.
         DO 70 I = 1,INPTS
            BNEXT = DATA(I).EQ.FBLANK
            IF (.NOT.BNEXT) THEN
               X = I - 0.5
               Y = DATA(I) * XFAC + XOFF
               Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
               J = 2
               IF (BLAST) J = 1
               CALL TVVEC (X, Y, J, IERR)
               IF (IERR.NE.0) GO TO 999
               X = I + 0.5
               J = 2
               CALL TVVEC (X, Y, J, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            BLAST = BNEXT
 70         CONTINUE
         IGR = IGR2
         J = 3 * NG + JJC
         ITTER = ITTER - 1
         JNPTS = INPTS
         IJ = JJC
         K = JJC
         IF (JJC.GT.0) VALVAR(1) = PARMS(1)
         IF (JJC.EQ.2) VALVAR(2) = PARMS(2)
         DO 75 I = 1,NG
            DO 74 J = 1,3
               K = K + 1
               IF ((LLCOMP(K).GT.0) .AND. (PARMS(K).NE.FBLANK)) THEN
                  IJ = IJ + 1
                  IVAR(IJ) = I
                  JVAR(IJ) = J
                  VALVAR(IJ) = PARMS(K)
                  END IF
 74            CONTINUE
 75         CONTINUE
         CALL DPCOPY (MAXPRM, PARMS, LPARMS)
         I = 1
         CALL AGFUNC (JNPTS, IJ, VALVAR, FVEC, FJAC, I)
         DX = INPTS / 200.0
         DY = (ORANGE(2) - ORANGE(1)) / 200.0
         DO 80 I = 1,INPTS
            BNEXT = DATA(I).EQ.FBLANK
            IF (.NOT.BNEXT) THEN
               X = I
               Y = (DATA(I) - FVEC(I)) * XFAC + XOFF
               Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
               CALL TVVEC (X+DX, Y+DY, 1, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL TVVEC (X-DX, Y-DY, 2, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL TVVEC (X+DX, Y-DY, 1, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL TVVEC (X-DX, Y+DY, 2, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
 80         CONTINUE
C                                       which are used?
         IF (NG.GT.1) THEN
            J = JJC - 3
            DO 85 I = 1,NG
               J = J + 3
               ISP(I) = 1
               IF (LLCOMP(J+1).LE.0) THEN
                  ISP(I) = 2
                  IF (PARMS(J+1).EQ.0.0D0) ISP(I) = 3
                  END IF
 85            CONTINUE
            IT1 = 0
            IT2 = 0
            IT3 = 0
            DO 90 I = 1,NG
               IF (ISP(I).EQ.1) THEN
                  IT1 = IT1 + 1
                  ITC1(IT1) = I
               ELSE IF (ISP(I).EQ.2) THEN
                  IT2 = IT2 + 1
                  ITC2(IT2) = I
               ELSE IF (ISP(I).EQ.3) THEN
                  IT3 = IT3 + 1
                  ITC3(IT3) = I
                  END IF
 90            CONTINUE
            DX = PBLC(1)*SCALEX + RX0 + 1.5 * CSIZTV(1)
            DY = PTRC(2)*SCALEY + RY0 - 2.0 * CSIZTV(2)
            IDX = DX + 0.5
            IDY = DY + 0.5
            IF (IT1.GT.0) THEN
               WRITE (MSGBUF,1090) (ITC1(I), I = 1,IT1)
               J = 6 + 2 * IT1
               CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:J), INBUF,
     *            IERR)
               IF (IERR.NE.0) GO TO 999
               DY = DY - 1.5 * CSIZTV(2)
               IDY = DY + 0.5
               END IF
            IF (IT2.GT.0) THEN
               WRITE (MSGBUF,1092) (ITC2(I), I = 1,IT2)
               J = 6 + 2 * IT2
               CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:J), INBUF,
     *            IERR)
               IF (IERR.NE.0) GO TO 999
               DY = DY - 1.5 * CSIZTV(2)
               IDY = DY + 0.5
               END IF
            IF (IT3.GT.0) THEN
               WRITE (MSGBUF,1094) (ITC3(I), I = 1,IT3)
               J = 6 + 2 * IT3
               CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:J), INBUF,
     *            IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            END IF
         END IF
      CALL YHOLD ('OFFF', IERR)
C                                       Talk to user
      IF (REDO) IERR = 103
      IF ((DOTV.GT.0.0) .AND. ((NG.GT.1) .OR. (DOEVEN)) .AND.
     *   (.NOT.REDO)) THEN
         IF (TTYIND.LE.0) THEN
            CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, F, T, T, JERR)
            IF (JERR.NE.0) THEN
               WRITE (MSGTXT,1900) JERR
               CALL MSGWRT (6)
               GO TO 999
               END IF
            END IF
C                                       rms resid
         SUMS = 0.0D0
         NSUM = 0.0D0
         DO 910 I = 1,INPTS
            IF (DATA(I).NE.FBLANK) THEN
               NSUM = NSUM + 1.0D0
               SUMS = SUMS + FVEC(I) * FVEC(I)
               END IF
 910        CONTINUE
         IF (NSUM.GT.0.0D0) SUMS = SQRT (SUMS / NSUM)
         WRITE (MSGBUF,1911) SUMS, IPOS(2), IPOS(3)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 930
         IF (JJC.GT.0) THEN
            IF (JJC.EQ.2) THEN
               WRITE (MSGBUF,1912) PARMS(1), PARMS(2)
            ELSE
               WRITE (MSGBUF,1912) PARMS(1)
               END IF
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            IF (IERR.NE.0) GO TO 930
            END IF
         J = JJC + 1
         DO 920 I = 1,NG
            WRITE (MSGBUF,1913) I, PARMS(J), PARMS(J+1), PARMS(J+2)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            IF (IERR.NE.0) GO TO 930
            J = J + 3
 920        CONTINUE
         IF (DOTV.GT.1.5) THEN
            CALL XGMEN1 (MSGBUF, SCRTCH, JERR)
         ELSE
            WRITE (MSGBUF,1915)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, JERR)
            IF (JERR.NE.0) GO TO 930
            MSGBUF = '??? hit return or anything else to procede'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, JERR)
            IF (JERR.NE.0) GO TO 930
            CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, JERR)
            END IF
         IF (JERR.NE.0) GO TO 930
         TEMP = FIRSTC (MSGBUF)
         IF (TEMP.EQ.'B') THEN
            IERR = 101
         ELSE IF (TEMP.EQ.'Q') THEN
            IERR = 102
         ELSE IF (TEMP.EQ.'E') THEN
            IERR = 103
            END IF
         END IF
      GO TO 990
C                                       TTY error
 930  WRITE (MSGTXT,1930) JERR
      CALL MSGWRT (6)
 990  IF (TTYIND.GT.0) THEN
         CALL ZCLOSE (TTYLUN, TTYIND, JERR)
         TTYIND = 0
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('Y=',I5)
 1061 FORMAT ('Z=',I5)
 1090 FORMAT ('Solve:',8I2)
 1092 FORMAT ('Fixed:',8I2)
 1094 FORMAT ('Null :',8I2)
 1900 FORMAT ('OPEN TERMINAL ERROR',I7)
 1911 FORMAT ('RMS residual',F12.6,'   at Y=',I5,'   Z=',I5)
 1912 FORMAT ('Baseline',2(1PE12.4))
 1913 FORMAT ('Gaussian',I2,F12.6,F10.2,F10.3)
 1915 FORMAT ('??? Type ''E'' to enter a guess, ''BAD'' to flag row,',
     *   ' ''Q'' to quit')
 1930 FORMAT ('TERMINAL I/O ERROR',I7)
      END
      SUBROUTINE XGMEN1 (MSGBUF, SCRTCH, JERR)
C-----------------------------------------------------------------------
C   Does a TV menu for initial guess stage of user questions
C   Outputs:
C      MSGBUF   C*(*)   answer: E, B, Q, other
C      JERR     I       error
C-----------------------------------------------------------------------
      INTEGER   SCRTCH(*), JERR
      CHARACTER MSGBUF*(*)
C
      INTEGER   MTYPE, NCOL, NROWS, GRCHS(2), TOPSEP, SIDSEP, TIMLIM,
     *   NTITLE, TVBUTT, CHOICE
      LOGICAL   LEAVE(6)
      CHARACTER CHOICS(6)*8, TITLE*8, ISHELP*6
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA CHOICS /'DO FIT', ' ', 'RE-GUESS', 'BAD', ' ', 'QUIT'/
      DATA LEAVE /.FALSE., .TRUE., 2*.FALSE., .TRUE., .FALSE./
      DATA ISHELP /'aGAUS'/
C-----------------------------------------------------------------------
      GRCHS(1) = 6
      GRCHS(2) = 5
      MTYPE = 1
      NCOL = 1
      NROWS = 6
      SIDSEP = 6 * CSIZTV(1)
C                                       44 good in charmult 1
      TOPSEP = 44 * 9
      NTITLE = 0
      TITLE = ' '
      TIMLIM = 0
      MSGBUF = ' '
C                                       menu
 20   CALL TVMENU (MTYPE, NCOL, NROWS, GRCHS, TOPSEP, SIDSEP, ISHELP,
     *   CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHOICE, TVBUTT, SCRTCH,
     *   JERR)
      IF (JERR.NE.0) THEN
         MSGTXT = 'XGMEN1: ERROR FROM TV MENU'
         CALL MSGWRT (7)
      ELSE IF (CHOICS(CHOICE).EQ.' ') THEN
         GO TO 20
      ELSE IF (CHOICS(CHOICE).EQ.'RE-GUESS') THEN
         MSGBUF = 'E'
      ELSE IF (CHOICS(CHOICE).EQ.'BAD') THEN
         MSGBUF = 'B'
      ELSE IF (CHOICS(CHOICE).EQ.'QUIT') THEN
         MSGBUF = 'Q'
      ELSE IF (CHOICS(CHOICE).EQ.'DO FIT') THEN
         MSGBUF = 'D'
         END IF
C
 999  RETURN
      END
      SUBROUTINE GTVTAU  (IPOS, INPTS, NG, LABEL, DOTV, VPEAK, PARMS,
     *   FVEC, FJAC, IERR)
C-----------------------------------------------------------------------
C   GTVTAU makes a plot of the optical depth spectrum in a hidden
C   TV channel
C   Inputs:
C      IPOS     I(7)     Position in cube first point in row.
C      INPTS    I        Number of points in row.
C      NG       I        Number of Gaussians
C      LABEL    I        Requested label type
C      DOTV     R        > 0. => plot data
C      PARMS    D(26)    Initial guess
C   Output:
C      FVEC     D(*)     Scratch buffer
C      FJAC     D(26)    Scratch buffer
C      IERR     I        > 0 => plot failed
C                        101 => bad initial guess
C                        102 => DIE
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), INPTS, NG, LABEL, IERR
      REAL      DOTV, VPEAK
      DOUBLE PRECISION PARMS(*), FVEC(*), FJAC(*)
C
      INCLUDE 'XGAUSD.INC'
      CHARACTER TEXT(2)*80, MSGBUF*132, NTEXT*80
      REAL      XBLC(7), XTRC(7), PBLC(2), PTRC(2), YGAP, CH(4), XYRATO,
     *   X, XX, Y, XFAC, XOFF, DX, DY, FQFINC, TRANGE(2)
      INTEGER   IDROP(2), IX1, IX2, IY1, IY2, ICHL, ICHB, ICHR, ICHT,
     *   NXA, NYA, I, J, ISP(4), IDX, ITC1(MAXGAU),
     *   IT1, ITC2(MAXGAU), IT2, ITC3(MAXGAU), IT3, I4XTRA, IDY,
     *   TVWIND(4), TVSIZE(2), CATSAV(256)
      LOGICAL   T, F, BLAST, BNEXT, FIRST
      DOUBLE PRECISION FQFREQ, TEPS
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVS.INC'
      INCLUDE 'INCS:DCAT.INC'
      SAVE FIRST
      DATA T, F, FIRST /.TRUE.,.FALSE.,.TRUE./
      DATA TEPS /1.D-5/
C-----------------------------------------------------------------------
C                                       "Slice" corners
      IDROP(1) = 0
      IDROP(2) = 0
      DO 10 I = 1,7
         XBLC(I) = IPOS(I)
         XTRC(I) = XBLC(I)
 10      CONTINUE
      XTRC(1) = XBLC(1) + INPTS - 1
      CALL COPY (256, CATBLK, CATSAV)
C                                       Set PIX ranges
      TRANGE(1) = 1.0E10
      TRANGE(2) = -TRANGE(1)
      DO 15 I = 1,INPTS
         IF (TDATA(I).NE.FBLANK) THEN
C            TAU = DATA(I) / VPEAK
C            IF (TAU.LT.TEPS) TAU = TEPS
C            TDATA(I) = -LOG (TAU)
            IF (TDATA(I).LT.TRANGE(1)) TRANGE(1) = TDATA(I)
            IF (TDATA(I).GT.TRANGE(2)) TRANGE(2) = TDATA(I)
            END IF
 15      CONTINUE
      XFAC = TRANGE(2) - TRANGE(1)
      TRANGE(2) = TRANGE(2) + 0.10 * XFAC
      TRANGE(1) = TRANGE(1) - 0.10 * XFAC
      CATR(IRRAN) = TRANGE(1)
      CATR(IRRAN+1) = TRANGE(2)
      XFAC = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      XOFF = 40000.0 - XFAC * CATR(IRRAN+1)
      PBLC(2) = TRANGE(1) * XFAC + XOFF
      PTRC(2) = TRANGE(2) * XFAC + XOFF
C                                       Label inits
      LOCNUM = 2
      FQFINC = 0.0
      FQFREQ = 0.0D0
      CALL SLBINI (IDROP, INPTS, TRANGE, PBLC, PTRC, XBLC, XTRC,
     *   FQFREQ, FQFINC, CATBLK(IIDEP), LABEL, YGAP, CH, TEXT, NTEXT)
      TRANGE(1) = XFAC*TRANGE(1) + XOFF
      TRANGE(2) = XFAC*TRANGE(2) + XOFF
      CALL YWINDO ('READ', TVWIND, IERR)
      IF (IERR.NE.0) THEN
         TVWIND(1) = 1
         TVWIND(2) = 1
         TVWIND(3) = MAXXTV(1)
         TVWIND(4) = MAXXTV(2)
         END IF
      TVSIZE(1) = TVWIND(3) - TVWIND(1) + 1
      TVSIZE(2) = TVWIND(4) - TVWIND(2) + 1
      XYRATO = (PTRC(2) - PBLC(2)) / (PTRC(1) - PBLC(1))
      IX1 = PBLC(1) + .5
      IY1 = PBLC(2) + .5
      IX2 = PTRC(1) + .5
      IY2 = PTRC(2) + .5
      ICHL = CH(1) * CSIZTV(1) + .5
      ICHB = CH(2) * CSIZTV(2) + .5
      ICHR = CH(3) * CSIZTV(1) + .5
      ICHT = CH(4) * CSIZTV(2) + .5
      NYA = TVSIZE(2) - ICHT -ICHB -1
      NXA = TVSIZE(1) - ICHL - ICHR - 1
C                                       compute scaling
      X = IX2
      X = ABS (X - IX1)
      XX = X * XYRATO
      Y = IY2
      Y = ABS (Y - IY1)
      IF ((XX.LE.0.0) .OR. (Y.LE.0.0)) THEN
         MSGTXT = 'SCALING ERROR'
         CALL MSGWRT (8)
         IERR = 1
         GO TO 990
         END IF
      IF ((XX/Y).LE.FLOAT(NXA)/FLOAT(NYA)) THEN
         SCALEY = NYA / Y
         SCALEX = SCALEY * Y / X * FLOAT(TVSIZE(1)) /
     *      FLOAT(TVSIZE(2))
      ELSE
         SCALEX = NXA / X
         SCALEY = SCALEX * X / Y
         END IF
C
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
      RX0 = ICHL + MAX (0, TVSIZE(1)-NXA) / 2 + TVWIND(1)
      RY0 = ICHB + MAX (0, TVSIZE(2)-NYA) / 2 + TVWIND(2)
C                                       Put stuff in image catalog.
      CATBLK(IIWIN  ) = IX1
      CATBLK(IIWIN+1) = IY1
      CATBLK(IIWIN+2) = IX2
      CATBLK(IIWIN+3) = IY2
      CATBLK(IICOR  ) = RX0 + 0.5
      CATBLK(IICOR+1) = RY0 + 0.5
      CATBLK(IICOR+2) = RX0 + X * SCALEX + 0.5
      CATBLK(IICOR+3) = RY0 + Y * SCALEY + 0.5
      CATBLK(IIPLT) = 5
      CATBLK(IIOTH) = LABEL
      CATBLK(IIOTH+1) = IDROP(1)
      CATBLK(IIOTH+2) = IDROP(2)
      I4XTRA = IIOTH + 3
      CATR(I4XTRA  ) = XBLC(1)
      CATR(I4XTRA+1) = XBLC(2)
      CATR(I4XTRA+2) = XTRC(1)
      CATR(I4XTRA+3) = XTRC(2)
C                                       Update image catalog
      IGR = IGR5
      CALL YCINIT (IGR, INBUF)
      CALL YCWRIT (IGR, CATBLK(IICOR), CATBLK, INBUF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TV IMAGE CATALOG ERROR'
         CALL MSGWRT (6)
         END IF
C                                       clear screen
      CALL YZERO (IGR, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YSLECT ('ONNN', IGR5, 0, INBUF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       set scale for plot routines
      RX0 = RX0 - PBLC(1) * SCALEX + 0.5
      RY0 = RY0 - PBLC(2) * SCALEY + 0.5
C                                       label the plot
      CALL TVLAB (PBLC, PTRC, LABEL, YGAP, TEXT, NTEXT, CH, F, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       add pixel coordinates
      DX = PTRC(1)*SCALEX + RX0 - 8.5 * CSIZTV(1)
      DY = PTRC(2)*SCALEY + RY0 - 2.0 * CSIZTV(2)
      IDX = DX + 0.5
      IDY = DY + 0.5
      WRITE (MSGBUF,1060) IPOS(2)
      CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:7), INBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      DY = DY - 1.5 * CSIZTV(2)
      IDY = DY + 0.5
      WRITE (MSGBUF,1061) IPOS(3)
      CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:7), INBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      DY = DY - 1.5 * CSIZTV(2)
      IDY = DY + 0.5
      PLTRMS(1) = IDX
      PLTRMS(2) = IDY
C                                       plot data
      IF (DOTV.GT.0.) THEN
         BLAST = .TRUE.
         DO 70 I = 1,INPTS
            BNEXT = TDATA(I).EQ.FBLANK
            IF (.NOT.BNEXT) THEN
               X = I - 0.5
               Y = TDATA(I) * XFAC + XOFF
               Y = MIN (TRANGE(2), MAX (TRANGE(1), Y))
               J = 2
               IF (BLAST) J = 1
               CALL TVVEC (X, Y, J, IERR)
               IF (IERR.NE.0) GO TO 990
               X = I + 0.5
               J = 2
               CALL TVVEC (X, Y, J, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            BLAST = BNEXT
 70         CONTINUE
C                                       which are used?
         IF (NG.GT.1) THEN
            J = JJC - 3
            DO 85 I = 1,NG
               J = J + 3
               ISP(I) = 1
               IF (LLCOMP(J+1).LE.0) THEN
                  ISP(I) = 2
                  IF (PARMS(J+1).EQ.0.0D0) ISP(I) = 3
                  END IF
 85            CONTINUE
            IT1 = 0
            IT2 = 0
            IT3 = 0
            DO 90 I = 1,NG
               IF (ISP(I).EQ.1) THEN
                  IT1 = IT1 + 1
                  ITC1(IT1) = I
               ELSE IF (ISP(I).EQ.2) THEN
                  IT2 = IT2 + 1
                  ITC2(IT2) = I
               ELSE IF (ISP(I).EQ.3) THEN
                  IT3 = IT3 + 1
                  ITC3(IT3) = I
                  END IF
 90            CONTINUE
            DX = PBLC(1)*SCALEX + RX0 + 1.5 * CSIZTV(1)
            DY = PTRC(2)*SCALEY + RY0 - 2.0 * CSIZTV(2)
            IDX = DX + 0.5
            IDY = DY + 0.5
            IF (IT1.GT.0) THEN
               WRITE (MSGBUF,1090) (ITC1(I), I = 1,IT1)
               J = 6 + 2 * IT1
               CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:J), INBUF,
     *            IERR)
               IF (IERR.NE.0) GO TO 990
               DY = DY - 1.5 * CSIZTV(2)
               IDY = DY + 0.5
               END IF
            IF (IT2.GT.0) THEN
               WRITE (MSGBUF,1092) (ITC2(I), I = 1,IT2)
               J = 6 + 2 * IT2
               CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:J), INBUF,
     *            IERR)
               IF (IERR.NE.0) GO TO 990
               DY = DY - 1.5 * CSIZTV(2)
               IDY = DY + 0.5
               END IF
            IF (IT3.GT.0) THEN
               WRITE (MSGBUF,1094) (ITC3(I), I = 1,IT3)
               J = 6 + 2 * IT3
               CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:J), INBUF,
     *            IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            END IF
         END IF
 990  CALL YHOLD ('OFFF', IERR)
      CALL COPY (256, CATBLK, CATTAU)
      CALL COPY (256, CATSAV, CATBLK)
      LOCNUM = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('Y=',I5)
 1061 FORMAT ('Z=',I5)
 1090 FORMAT ('Solve:',8I2)
 1092 FORMAT ('Fixed:',8I2)
 1094 FORMAT ('Null :',8I2)
      END
      SUBROUTINE GTVGUS (IPOS, INPTS, NG, LABEL, DOTV, VPEAK, PARMS,
     *   FVEC, FJAC, DOCOMP, IERR)
C-----------------------------------------------------------------------
C   GTVGUS has the user point at the desired initial guess for each
C   Gaussian with the TV cursor.  It will accept that there is no
C   component "n" at this row.  It sets the PARMS and plots the new
C   guess with + signs.
C   Inputs:
C      INPTS    I       Number data points in row
C      NG       I       Number of Gaussians
C      DOCOMP   I(26)   User initial do fit flags
C   Output:
C      PARMS    D(26)   Initial guess
C      IERR     I       error code: 0 -> ok
C-----------------------------------------------------------------------
      INTEGER   IPOS(*), INPTS, NG, LABEL, DOCOMP(*), IERR
      REAL      DOTV, VPEAK
      DOUBLE PRECISION PARMS(*), FVEC(*), FJAC(*)
C
      CHARACTER MSGBUF*80
      REAL      XFAC, XOFF, RPOS(2), TRANGE(2), SCSAVE(6)
      INTEGER   I, J, IXT, IYT, IX, IY, JERR, IX0, IY0, QUAD, IBUT,
     *   CATSAV(256)
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVS.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'XGAUSD.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      CALL RCOPY (6, SCALEX, SCSAVE)
C                                       Plot Tau spectrum
      CALL GTVTAU (IPOS, INPTS, NG, LABEL, DOTV, VPEAK, PARMS, FVEC,
     *   FJAC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       set graphics
      CALL COPY (256, CATBLK, CATSAV)
      CALL COPY (256, CATTAU, CATBLK)
      LOCNUM = 2
      CALL YSLECT ('OFFF', IGR1, 0, INBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YSLECT ('OFFF', IGR2, 0, INBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YSLECT ('OFFF', IGR3, 0, INBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YSLECT ('OFFF', IGR4, 0, INBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YSLECT ('OFFF', IGR7, 0, INBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YSLECT ('ONNN', IGR5, 0, INBUF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Set scales
      XFAC = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      XOFF = 40000.0 - XFAC * CATR(IRRAN+1)
      TRANGE(1) = XFAC*CATR(IRRAN) + XOFF
      TRANGE(2) = XFAC*CATR(IRRAN+1) + XOFF
      IXT = RX0 + SCALEX * INPTS + 0.5
      IYT = RY0 + SCALEY * TRANGE(2) + 0.5
      IX0 = RX0 + SCALEX + 0.5
      IY0 = RY0 + SCALEY * TRANGE(1) + 0.5
C                                       Open terminal
      IF (TTYIND.LE.0) THEN
         CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, F, T, T, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (6)
            GO TO 999
            END IF
         END IF
C                                       Loop over components.
      J = JJC - 3
      DO 40 I = 1,NG
         J = J + 3
C                                       read peak and position
         IF ((DOCOMP(J+1).GT.0) .OR. (DOCOMP(J+2).GT.0)) THEN
            WRITE (MSGBUF,1020) I
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            IF (IERR.NE.0) GO TO 900
            CALL TVWHER (QUAD, RPOS, IBUT, IERR)
            IF (IERR.NE.0) GO TO 990
            IX = RPOS(1) + 0.5
            IY = RPOS(2) + 0.5
C                                       Set peak and position
            IF ((IX.GE.IX0) .AND. (IY.GE.IY0) .AND. (IX.LE.IXT) .AND.
     *         (IY.LE.IYT)) THEN
               LLCOMP(J+1) = DOCOMP(J+1)
               IF (LLCOMP(J+1).GT.0) PARMS(J+1) = ((IY - RY0) /
     *            SCALEY  - XOFF) / XFAC
               LLCOMP(J+2) = DOCOMP(J+2)
               IF (LLCOMP(J+2).GT.0) PARMS(J+2) = (IX - RX0) / SCALEX
     *            + XBAR
C               IF ((JJC.EQ.2) .AND. (LLCOMP(J+1).GT.0)) PARMS(J+1) =
C     *            PARMS(J+1) - PARMS(1) - PARMS(2) * PARMS(J+2)
C               IF ((JJC.EQ.1) .AND. (LLCOMP(J+1).GT.0)) PARMS(J+1) =
C     *            PARMS(J+1) - PARMS(1)
            ELSE
               GO TO 30
               END IF
            END IF
C                                       read half width
         IF (DOCOMP(J+3).GT.0) THEN
            WRITE (MSGBUF,1021) I
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            IF (IERR.NE.0) GO TO 900
            CALL TVWHER (QUAD, RPOS, IBUT, IERR)
            IF (IERR.NE.0) GO TO 990
            IX = RPOS(1) + 0.5
            IY = RPOS(2) + 0.5
C                                       Set width
            IF ((IX.GE.IX0) .AND. (IY.GE.IY0) .AND. (IX.LE.IXT) .AND.
     *         (IY.LE.IYT)) THEN
               LLCOMP(J+3) = DOCOMP(J+3)
               PARMS(J+3) = (IX - RX0) / SCALEX + XBAR
               PARMS(J+3) = 2.D0 * ABS (PARMS(J+3) - PARMS(J+2))
               IF (PARMS(J+3).LE.0.0D0) PARMS(J+3) = 0.5D0
               GO TO 40
               END IF
            END IF
C                                       Blank this component
 30      CONTINUE
            PARMS(J+1) = 0.0D0
            PARMS(J+2) = 0.0D0
            PARMS(J+3) = 4.0D0
            LLCOMP(J+1) = -1
            LLCOMP(J+2) = -1
            LLCOMP(J+3) = -1
 40      CONTINUE
C                                       Don't plot new guess
      GO TO 990
C                                       terminal error
 900  WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (6)
C                                       Close hold
 990  CALL YHOLD ('OFFF', JERR)
      IF (TTYIND.GT.0) THEN
          CALL ZCLOSE (TTYLUN, TTYIND, JERR)
          TTYIND = 0
          END IF
      CALL COPY (256, CATBLK, CATTAU)
      CALL COPY (256, CATSAV, CATBLK)
      CALL RCOPY (6, SCSAVE, SCALEX)
      LOCNUM = 1
      CALL YSLECT ('OFFF', IGR5, 0, INBUF, JERR)
      CALL YSLECT ('ONNN', IGR1, 0, INBUF, JERR)
      CALL YSLECT ('ONNN', IGR2, 0, INBUF, JERR)
      CALL YSLECT ('ONNN', IGR3, 0, INBUF, JERR)
      CALL YSLECT ('ONNN', IGR4, 0, INBUF, JERR)
      CALL YSLECT ('ONNN', IGR7, 0, INBUF, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' OPENING TERMINAL')
 1020 FORMAT ('Position cursor at center & height of gaussian ',
     *   'component',I2)
 1021 FORMAT ('Position cursor at halfwidth of gaussian component',I2)
 1900 FORMAT ('ERROR',I5,' WRITING TO TERMINAL')
      END
      SUBROUTINE GTVMOD (DOTV, DORESI, DOMODL, INPTS, IPOS, NG, NGAUSS,
     *   PIXR, ORANGE, FVEC, PARMS, PERR, IERR)
C-----------------------------------------------------------------------
C   GTVMOD plots the residual and model functions on the TV.  It asks
C   the user for permission to proceed.
C   Inputs:
C      DOTV     R      > 0. => plot model
C      DOMODL   R      > 0. => plot individual components
C      INPTS    I      Number of data points
C      IPOS     I(*)   Pixel location in cube
C      NGAUSS   I      Max number Gaussians allowed
C      ORANGE   R(2)   Plot intensity range (plot units)
C      FVEC     D(*)   data - model
C      PERR     I      > 0 => probable parameter bad
C   In/Out:
C      NG       I      Number of Gaussians: retry can ask for fewer
C      PARMS    D(*)   Model parameters
C   Output:
C      IERR     I      TV error code
C                         101 => blank this solution
C                         102 => User wants to quit
C                         103 => do a retry
C                         104 => TV off
C                         105 => Do fit with this guess
C-----------------------------------------------------------------------
      REAL      DOTV, DORESI, DOMODL, PIXR(2), ORANGE(2)
      INTEGER   INPTS, IPOS(*), NG, NGAUSS, PERR, IERR
      DOUBLE PRECISION FVEC(*), PARMS(*)
C
      CHARACTER TEMP*1, MSGBUF*80, FIRSTC*1, CHS*64
      INTEGER   I, J, K, JERR, JTRIM, NCHS, SCRTCH(4096), IDX,
     *   IDY, JNG, NPPL
      REAL      XFAC, XOFF, X, Y, TAU, CONT, DX, DY, TP, XP, YP
      LOGICAL   BLAST, T, F, WHAND
      DOUBLE PRECISION SUMS, NSUM, XX, AMP, POS, SIG, RES2, HALFAC
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'XGAUSD.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA HALFAC /2.77258872D0/
C-----------------------------------------------------------------------
      IERR = 0
      IF ((DOTV.LE.0.) .AND. (DOMODL.LE.0.) .AND. (DORESI.LE.0.0))
     *   GO TO 900
      WHAND = .FALSE.
      XFAC = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      XOFF = 40000.0 - XFAC * CATR(IRRAN+1)
      CHS = ' '
      IF (NGAUSS.GT.1) THEN
         WRITE (CHS,1000) (I, I = 1,NGAUSS)
         J = JTRIM (CHS)
         CHS(J:J) = ' '
         END IF
      NCHS = JTRIM (CHS)
C                                       find rms residual
 10   SUMS = 0.0D0
      NSUM = 0.0D0
      DO 15 I = 1,INPTS
         IF (DATA(I).NE.FBLANK) THEN
            SUMS = SUMS + FVEC(I) * FVEC(I)
            NSUM = NSUM + 1.0D0
            END IF
 15      CONTINUE
      IF (NSUM.GT.0.0D0) SUMS = SQRT (SUMS/NSUM)
C                                       Plot residuals
      IF (DORESI.GT.0.) THEN
         IGR = IGR3
         IF (IGR.NE.IGR1) THEN
            CALL YZERO (IGR, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         BLAST = .TRUE.
         DO 30 I = 1,INPTS
            IF (DATA(I).EQ.FBLANK) THEN
               BLAST = .TRUE.
            ELSE
               X = I
               Y = FVEC(I) * XFAC + XOFF
               XP = X * SCALEX + RX0 - RXL
               YP = Y * SCALEY + RY0 - RYL
               TP = MAX (ABS(XP), ABS(YP))
               NPPL = (TP + 9) / 16.0 + 0.5
               IF (BLAST) NPPL = 1
               XP = (RXL - RX0) / SCALEX
               YP = (RYL - RY0) / SCALEY
               DX = (X - XP) / NPPL
               DY = (Y - YP) / NPPL
               DO 25 K = 1,NPPL
                  X = XP + (K-0.6) * DX
                  Y = YP + (K-0.6) * DY
                  Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
                  J = 2
                  IF ((BLAST) .OR. (MOD(K,2).EQ.0)) J = 1
                  CALL TVVEC (X, Y, J, IERR)
                  IF (IERR.NE.0) GO TO 900
                  X = XP + K * DX
                  Y = YP + K * DY
                  Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
                  J = 1
                  CALL TVVEC (X, Y, J, IERR)
                  IF (IERR.NE.0) GO TO 900
 25               CONTINUE
               BLAST = .FALSE.
               END IF
 30         CONTINUE
         END IF
C                                       Plot model
      IF (DOTV.GT.0.) THEN
         IGR = IGR4
         BLAST = .TRUE.
         IF (IGR.NE.IGR1) THEN
            CALL YZERO (IGR, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         DO 60 I = 1,INPTS
            IF (DATA(I).EQ.FBLANK) THEN
               BLAST = .TRUE.
            ELSE
               X = I
               Y = (DATA(I) - FVEC(I)) * XFAC + XOFF
               Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
               J = 2
               IF (BLAST) J = 1
               CALL TVVEC (X, Y, J, IERR)
               IF (IERR.NE.0) GO TO 900
               BLAST = .FALSE.
               END IF
 60         CONTINUE
C                                       plot rms too
         WRITE (MSGBUF,1060) SUMS
         CALL REFRMT (MSGBUF, '_', J)
         I = INDEX (MSGBUF, '.')
         IF (SUMS.GT.99.9) THEN
            K = I - 1
         ELSE IF (SUMS.GT.9.99) THEN
            K = I + 1
         ELSE IF (SUMS.GT.0.999) THEN
            K = I + 2
         ELSE IF (SUMS.GT.0.0999) THEN
            K = I + 3
         ELSE IF (SUMS.GT.0.00999) THEN
            K = I + 4
         ELSE
            K = J
            END IF
         IDX = PLTRMS(1)
         IF (K.GT.7) IDX = IDX - (K-7) * CSIZTV(1)
         IDY = PLTRMS(2)
         CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:K), INBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Plot components
      JNG = (MVAR-JJC) / 3
      IF ((DOMODL.GT.0.0) .AND. (JNG.GT.1)) THEN
         IGR = IGR7
         IF (DORESI.LE.0.0) IGR = IGR3
         IF (IGR.NE.IGR1) THEN
            CALL YZERO (IGR, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         DO 100 K = JJC+1,MVAR,3
C                                       model different for absorption
            AMP = LPARMS(K)
            POS = LPARMS(K+1)
            SIG = LPARMS(K+2)
            IF (AMP.NE.0.0) THEN
               BLAST = .TRUE.
               DO 90 I = 1,INPTS
                  XX = I + XBAR
                  CONT = LPARMS(1)
                  IF (JJC.EQ.2) CONT = CONT + LPARMS(2) * XX
                  RES2 = (XX - POS) / SIG
                  RES2 = HALFAC * RES2 * RES2
                  IF (RES2.LE.10.0D0) THEN
                     TAU = AMP * EXP (-RES2)
                     X = I
                     Y = CONT * EXP (-TAU) * XFAC + XOFF
                     Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
                     J = 2
                     IF (BLAST) J = 1
                     CALL TVVEC (X, Y, J, IERR)
                     IF (IERR.NE.0) GO TO 900
                     BLAST = .FALSE.
                  ELSE
                     BLAST = .TRUE.
                     END IF
 90               CONTINUE
               END IF
 100        CONTINUE
         END IF
C                                       Open TTY
 900  IF (TTYIND.LE.0) THEN
         CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, F, T, T, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1900) IERR
            CALL MSGWRT (6)
            GO TO 999
            END IF
         END IF
      IF (PERR.GT.0) THEN
         WRITE (MSGBUF,1910)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      WRITE (MSGBUF,1911) SUMS, IPOS(2), IPOS(3)
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (JJC.GT.0) THEN
         IF (JJC.EQ.2) THEN
            WRITE (MSGBUF,1912) PARMS(1), PARMS(2)
         ELSE
            WRITE (MSGBUF,1912) PARMS(1)
            END IF
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      J = JJC + 1
      DO 905 I = 1,NG
         WRITE (MSGBUF,1913) I, PARMS(J), PARMS(J+1), PARMS(J+2)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         J = J + 3
 905     CONTINUE
      IF (DOTV.GT.1.5) THEN
         CALL XGMEN2 (NGAUSS, WHAND, MSGBUF, SCRTCH, JERR)
      ELSE
         MSGBUF = '**** Enter HAND to enter new numbers by hand'
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         WRITE (MSGBUF,1915)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (NCHS.GT.20) THEN
            WRITE (MSGBUF,1916) CHS(:NCHS)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            WRITE (MSGBUF,1917)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            IF (IERR.NE.0) GO TO 980
         ELSE IF (NCHS.GT.0) THEN
            WRITE (MSGBUF,1918) CHS(:NCHS)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
         IF (WHAND) THEN
            WRITE (MSGBUF,1919)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         WRITE (MSGBUF,1920)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         WRITE (MSGBUF,1921)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         END IF
      IF (IERR.NE.0) GO TO 980
      WHAND = .FALSE.
      TEMP = FIRSTC (MSGBUF)
      IF (TEMP.EQ.'B') THEN
         IERR = 101
      ELSE IF (TEMP.EQ.'Q') THEN
         IERR = 102
      ELSE IF (TEMP.EQ.'T') THEN
         IERR = 104
      ELSE IF (TEMP.EQ.'D') THEN
         IERR = 105
      ELSE IF (TEMP.EQ.'R') THEN
         IERR = 103
      ELSE IF (TEMP.EQ.'E') THEN
         IERR = 103
      ELSE IF (TEMP.EQ.'1') THEN
         IERR = 103
         NG = 1
      ELSE IF ((TEMP.EQ.'2') .AND. (NGAUSS.GE.2)) THEN
         IERR = 103
         NG = 2
      ELSE IF ((TEMP.EQ.'3') .AND. (NGAUSS.GE.3)) THEN
         IERR = 103
         NG = 3
      ELSE IF ((TEMP.EQ.'4') .AND. (NGAUSS.GE.4)) THEN
         IERR = 103
         NG = 4
      ELSE IF ((TEMP.EQ.'5') .AND. (NGAUSS.GE.5)) THEN
         IERR = 103
         NG = 5
      ELSE IF ((TEMP.EQ.'6') .AND. (NGAUSS.GE.6)) THEN
         IERR = 103
         NG = 6
      ELSE IF ((TEMP.EQ.'7') .AND. (NGAUSS.GE.7)) THEN
         IERR = 103
         NG = 7
      ELSE IF ((TEMP.EQ.'8') .AND. (NGAUSS.GE.8)) THEN
         IERR = 103
         NG = 8
      ELSE IF (TEMP.EQ.'H') THEN
         WHAND = .TRUE.
         CALL XGHAND (MSGBUF, NG, NGAUSS, INPTS, PARMS, FVEC, IERR)
         PERR = 0
         IF (IERR.EQ.0) GO TO 10
         IERR = MAX (IERR, 0)
         END IF
      GO TO 990
C                                       TTY error
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (6)
 990  IF (TTYIND.GT.0) THEN
         CALL ZCLOSE (TTYLUN, TTYIND, JERR)
         TTYIND = 0
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (8('''',I1,''', '))
 1060 FORMAT ('RMS',F12.6)
 1900 FORMAT ('OPEN TERMINAL ERROR',I7)
 1910 FORMAT ('>>>> PARAMETERS SEEM OUT OF RANGE.  SOLUTION PROBABLY ',
     *   'BAD! <<<<')
 1911 FORMAT ('RMS residual',F12.6,'   at Y=',I5,'   Z=',I5)
 1912 FORMAT ('Baseline',2(1PE12.4))
 1913 FORMAT ('Gaussian',I2,F12.6,F10.2,F10.3)
 1915 FORMAT ('**** Enter RETR to repeat the row (must enter a guess',
     *   ' on the TV)')
 1916 FORMAT ('**** Enter one of ',A)
 1917 FORMAT ('     to RETR with that number of Gaussians')
 1918 FORMAT ('**** Enter one of ',A,' to RETR with that',
     *   ' number of Gaussians')
 1919 FORMAT ('**** Enter DOFIT to do a new fit with this HAND guess')
 1920 FORMAT ('**** enter BAD to flag solution, QUIT to die, ',
     *   'TVOFF to stop TV display')
 1921 FORMAT ('     or hit return to continue')
 1980 FORMAT ('TERMINAL I/O ERROR',I7)
      END
      SUBROUTINE XGMEN2 (NG, WHAND, MSGBUF, SCRTCH, JERR)
C-----------------------------------------------------------------------
C   Does a TV menu for post-fit stage of user questions
C   Inputs:
C      NG       I       Number Gaussians max
C      WHAND    L       t => last command was HAND (add DO FIT to menu)
C   Outputs:
C      MSGBUF   C*(*)   answer: E, B, Q, other
C      JERR     I       error
C-----------------------------------------------------------------------
      INTEGER   NG, SCRTCH(*), JERR
      LOGICAL   WHAND
      CHARACTER MSGBUF*(*)
C
      INTEGER   NCHS
      PARAMETER (NCHS = 17)
C
      INTEGER   MTYPE, NCOL, NROWS, GRCHS(2), TOPSEP, SIDSEP, TIMLIM,
     *   NTITLE, TVBUTT, CHOICE, I, N
      LOGICAL   LEAVE(NCHS)
      CHARACTER CHOICS(NCHS)*8, TITLE*8, ISHELP*6, CHOICZ(NCHS)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA CHOICZ /'GOOD', ' ', 'RE-GUESS', 'TVOFF', 'HAND', 'BAD', '1',
     *   '2', '3', '4', '5', '6', '7', '8', ' ', 'QUIT', 'DO FIT'/
      DATA LEAVE /.FALSE., .TRUE., 13*.FALSE., .TRUE., .FALSE./
      DATA ISHELP /'AGAUS'/
C-----------------------------------------------------------------------
      NROWS = 6 + NG
      CHOICS(1) = CHOICZ(1)
      CHOICS(2) = CHOICZ(2)
      N = 2
      IF (WHAND) THEN
         NROWS = NROWS + 1
         CHOICS(3) = 'DO FIT'
         N = 3
         END IF
      DO 10 I = 3,NROWS
         N = N + 1
         CHOICS(N) = CHOICZ(I)
 10      CONTINUE
      CHOICS(NROWS+1) = ' '
      CHOICS(NROWS+2) = 'QUIT'
      LEAVE(NROWS+1) = .TRUE.
      LEAVE(NROWS+2) = .FALSE.
      NROWS = NROWS + 2
      GRCHS(1) = 6
      GRCHS(2) = 5
      MTYPE = 1
      NCOL = 1
      SIDSEP = 6 * CSIZTV(1)
C                                       44 good in charmult 1
      TOPSEP = 44 * 9
      NTITLE = 0
      TITLE = ' '
      TIMLIM = 0
      MSGBUF = ' '
C                                       menu
 20   CALL TVMENU (MTYPE, NCOL, NROWS, GRCHS, TOPSEP, SIDSEP, ISHELP,
     *   CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHOICE, TVBUTT, SCRTCH,
     *   JERR)
      IF (JERR.NE.0) THEN
         MSGTXT = 'XGMEN2: ERROR FROM TV MENU'
         CALL MSGWRT (7)
      ELSE IF (CHOICS(CHOICE).EQ.' ') THEN
         GO TO 20
      ELSE
         MSGBUF = CHOICS(CHOICE)(:1)
         END IF
C
 999  RETURN
      END
      SUBROUTINE XGAUCH (NG, INPTS, FVEC, PARMS, IERR)
C-----------------------------------------------------------------------
C   XGAUCH checks the answers for being at least vaguely reasonable.
C   Inputs:
C      NG      I      Number of Gaussians
C      INPTS   I      Number of data samples
C      FVEC    D(*)   Residuals
C      PARMS   D(*)   Answers
C   Output:
C      IERR    I   0 -> all in range, 1 -> some not
C-----------------------------------------------------------------------
      INTEGER   NG, INPTS, IERR
      DOUBLE PRECISION FVEC(*), PARMS(*)
C
      REAL      X, Y, DMAX, DMIN
      INTEGER   IAMP, I, IDBG
      DOUBLE PRECISION RMS, CNT
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'XGAUS.INC'
      INCLUDE 'XGAUSD.INC'
C-----------------------------------------------------------------------
C                                       get rms
      RMS = 0.0D0
      CNT = 0.0D0
      DO 10 I = 1,INPTS
         IF (DATA(I).NE.FBLANK) THEN
            RMS = RMS + FVEC(I)*FVEC(I)
            CNT = CNT + 1.0D0
            END IF
 10      CONTINUE
      IF (CNT.GE.1) RMS = SQRT (RMS/CNT)
      THERMS = RMS
      IERR = 1
      IF (RMS.GT.RMSLIM) GO TO 999
C                                       Check answers
      DMAX = OLDR(KRDMX)
      DMIN = OLDR(KRDMN)
      DMIN = 0.5 * DMIN
      IF (DMAX.GE.0.) THEN
         DMAX = 2. * DMAX
      ELSE
         DMAX = 0.5 * DMAX
         END IF
      Y = (INPTS - 1.0) / 20.0
      IF (JJC.EQ.2) THEN
         IDBG = 1
         X = PARMS(1) + PARMS(2) * (1.0 + XBAR)
         IF ((X.LT.DMIN) .OR. (X.GT.DMAX)) GO TO 990
         IDBG = 2
         X = PARMS(1) + PARMS(2) * (INPTS + XBAR)
         IF ((X.LT.DMIN) .OR. (X.GT.DMAX)) GO TO 990
         END IF
      IF (JJC.EQ.1) THEN
         IDBG = 1
         X = PARMS(1)
         IF ((X.LT.DMIN) .OR. (X.GT.DMAX)) GO TO 990
         END IF
      IDBG = 2
      DO 130 I = 1,NG
         IAMP = JJC + 3 * I - 2
         IF ((PARMS(IAMP).NE.0.0D0) .OR. (PARMS(IAMP+1).NE.0.0D0) .OR.
     *      (PARMS(IAMP+2).NE.0.0D0)) THEN
            X = PARMS(IAMP)
            IDBG = IDBG + 1
            IF (((X.LT.DMIN) .OR. (X.GT.DMAX)) .AND.
     *         (LLCOMP(IAMP).GT.0)) GO TO 990
            IDBG = IDBG + 1
            X = PARMS(IAMP+1) - XBAR
            IF (((X.LT.1.-Y) .OR. (X.GT.INPTS+Y)) .AND.
     *         (LLCOMP(IAMP+1).GT.0)) GO TO 990
            IDBG = IDBG + 1
            X = ABS (PARMS(IAMP+2))
            IF (((X.LT.1.25) .OR. (X.GT.INPTS/1.5)) .AND.
     *         (LLCOMP(IAMP+2).GT.0)) GO TO 990
         ELSE
            IDBG = IDBG + 3
            END IF
 130     CONTINUE
      IERR = 0
      GO TO 999
 990  WRITE (MSGTXT,1990) IDBG, X
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('XGAUCH PARAMETER',I3,' VALUE',1PE13.5,' OUT OF RANGE')
      END
      SUBROUTINE XGHAND (MSGBUF, NG, NGAUSS, INPTS, PARMS, FVEC, IERR)
C-----------------------------------------------------------------------
C   Enter guesses as hand numbers
C   Inputs:
C      NG       I       Number Gaussians: currently
C      NGAUSS   I       Max number of Gaussians
C      INPTS    I       Number points in FVEC
C   In/out:
C      MSGBUF   C*(*)   Message buffer
C      PARMS    D(*)    Parameters
C      FVEC     D(*)    Residuals
C   Output:
C      IERR     I       0 => carry on
C-----------------------------------------------------------------------
      INTEGER   NG, NGAUSS, INPTS, IERR
      CHARACTER MSGBUF*(*)
      DOUBLE PRECISION PARMS(*), FVEC(*)
C
      INCLUDE 'XGAUSD.INC'
      INTEGER   I, J, K, JTRIM, KBP, MP, NVAR
      DOUBLE PRECISION XX, OPARMS(MAXPRM), FJAC(MAXPRM,MAXPRM),
     *   VALVAR(MAXPRM)
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      J = JJC + 1
      CALL DPCOPY (MAXPRM, PARMS, OPARMS)
      DO 20 I = 1,NG
         WRITE (MSGBUF,1000) I
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         K = JTRIM (MSGBUF)
         KBP = 1
         CALL GETNUM (MSGBUF, K, KBP, XX)
         IF (XX.EQ.DBLANK) GO TO 900
         PARMS(J) = XX
         CALL GETNUM (MSGBUF, K, KBP, XX)
         IF (XX.EQ.DBLANK) GO TO 900
         PARMS(J+1) = XX
         CALL GETNUM (MSGBUF, K, KBP, XX)
         IF (XX.EQ.DBLANK) GO TO 900
         PARMS(J+2) = XX
C                                       flags
         LLCOMP(J) = 1
         LLCOMP(J+1) = 1
         LLCOMP(J+2) = 1
         CALL GETNUM (MSGBUF, K, KBP, XX)
         IF (XX.NE.DBLANK) THEN
            IF (XX.LE.0.0D0) LLCOMP(J) = -1
            CALL GETNUM (MSGBUF, K, KBP, XX)
            IF (XX.NE.DBLANK) THEN
               IF (XX.LE.0.0D0) LLCOMP(J+1) = -1
               CALL GETNUM (MSGBUF, K, KBP, XX)
               IF (XX.NE.DBLANK) THEN
                  IF (XX.LE.0.0D0) LLCOMP(J+2) = -1
                  END IF
               END IF
            END IF
         J = J + 3
 20      CONTINUE
      DO 30 I = NG+1,NGAUSS
         WRITE (MSGBUF,1020) I
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         K = JTRIM (MSGBUF)
         KBP = 1
         CALL GETNUM (MSGBUF, K, KBP, XX)
         IF (XX.EQ.DBLANK) GO TO 900
         IF (XX.LE.0.0D0) THEN
            GO TO 40
         ELSE
            NG = I
            PARMS(J) = XX
            CALL GETNUM (MSGBUF, K, KBP, XX)
            IF (XX.EQ.DBLANK) GO TO 900
            PARMS(J+1) = XX
            CALL GETNUM (MSGBUF, K, KBP, XX)
            IF (XX.EQ.DBLANK) GO TO 900
            PARMS(J+2) = XX
C                                       flags
            LLCOMP(J) = 1
            LLCOMP(J+1) = 1
            LLCOMP(J+2) = 1
            CALL GETNUM (MSGBUF, K, KBP, XX)
            IF (XX.NE.DBLANK) THEN
               IF (XX.LE.0.0D0) LLCOMP(J) = -1
               CALL GETNUM (MSGBUF, K, KBP, XX)
               IF (XX.NE.DBLANK) THEN
                  IF (XX.LE.0.0D0) LLCOMP(J+1) = -1
                  CALL GETNUM (MSGBUF, K, KBP, XX)
                  IF (XX.NE.DBLANK) THEN
                     IF (XX.LE.0.0D0) LLCOMP(J+2) = -1
                     END IF
                  END IF
               END IF
            J = J + 3
            END IF
 30      CONTINUE
C                                       evaluate residual
 40   ITTER = 0
      NITTER = 100
      NVAR = JJC
      K = JJC
      IF (JJC.GT.0) VALVAR(1) = PARMS(1)
      IF (JJC.EQ.2) VALVAR(2) = PARMS(2)
      DO 84 I = 1,NG
         DO 83 J = 1,3
            K = K + 1
            IF ((LLCOMP(K).GT.0) .AND. (PARMS(K).NE.FBLANK)) THEN
               NVAR = NVAR + 1
               IVAR(NVAR) = I
               JVAR(NVAR) = J
               VALVAR(NVAR) = PARMS(K)
               END IF
 83         CONTINUE
 84      CONTINUE
      MP = NVAR
      I = 1
      MVAR = K
      CALL DPCOPY (MAXPRM, PARMS, LPARMS)
      CALL AGFUNC (INPTS, MP, VALVAR, FVEC, FJAC, I)
      GO TO 999
C                                       bad value quiet exit
 900  IERR = -1
      CALL DPCOPY (MAXPRM, OPARMS, PARMS)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Enter Gaussian',I2,' parameters (3 values), flags')
 1020 FORMAT ('Enter Gaussian',I2,' parameters (3 values), flags',
     *   ', zero -> skip this one')
      END
      SUBROUTINE XGAUTV (IRET)
C-----------------------------------------------------------------------
C   XGAUTV implements a TV menu driven method to manipulate the
C   Gaussian fits results in order to re-try fits, rearrange fit
C   components, etc.
C   Outputs:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'XGAUS.INC'
      INCLUDE 'XGAUSD.INC'
      INTEGER   NOPT1, NOPT2, NOPTS
      PARAMETER (NOPT1=18+MXPAIR)
      PARAMETER (NOPT2=NMXIMG*MAXGAU)
      PARAMETER (NOPTS=NOPT1+NOPT2)
C
      INTEGER   MTYPE, MCOL, MROWS(8), GRCHS(2), TIMLIM, CHS, TVBUT,
     *   NX, NY, NP, I, J, IPOS(2), NWORDS, TOPSEP, IP, NG, I1, I2,
     *   JTRIM, IC, ICOLOR, NLEVS, JJ, II, IG, K, CATEMP(256), SIDSEP,
     *   MAXCH, WIND(4)
      CHARACTER CHOIC1(NOPT1)*16, CHOIC2(NOPT2)*16, CHOICS(NOPTS)*16,
     *   ISHELP*6, TITLE(1+2*MAXGAU)*128, MSGBUF*72, IMGTYP(NMXIMG)*2
      LOGICAL   IMGOK, LEAVE1(NOPT1), LEAVE2(NOPT2), LEAVE(NOPTS), DOIT
      LONGINT   PIMAGE
      REAL      IMAGES(2), SNMIN(MAXGAU), MAXRES, MAXWID(2,MAXGAU),
     *   MAXOFF(2,MAXGAU), MAXDWD(MAXGAU), SLOPE, OFFS, MAXPK(2,MAXGAU)
      DOUBLE PRECISION XX(8)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA MTYPE, TIMLIM, TOPSEP, SIDSEP, GRCHS /-1, 0, 3, 10, 2, 1/
      DATA LEAVE1 /.FALSE., 7*.TRUE., 2*.FALSE., 6*.TRUE., 2*.FALSE.,
     *   MXPAIR*.TRUE./
      DATA CHOIC1 /'EXIT', ' ', 'SET MIN S/N', 'SET MAX RES',
     *   'SET PEAK RANGE','SET OFFX RANGE', 'SET WIDTH RANGE',
     *   'SET MAX ERR WID', 'REDO ALL', 'FLAG ALL', ' ', 'OFF ZOOM',
     *   'OFF TRANSFER', 'SET DOOUTPUT', 'ADD TO LIST', 'SHOW LIST',
     *   'REDO LIST', 'FLAG LIST', MXPAIR*'SWAP LIST'/
      DATA IMGTYP /'A','C','W','F','EA','EC','EW','EF'/
C-----------------------------------------------------------------------
      IGR1 = 1
      IGR2 = 2
      IGR3 = 3
      IGR4 = 4
      IGR1 = IGR1 + NGRAY
      IGR2 = IGR2 + NGRAY
      IGR3 = IGR3 + NGRAY
      IGR4 = IGR4 + NGRAY
      DO 5 I = 1,NGRAY+NGRAPH
         CALL YSLECT ('OFFF', I, 0, IBUFF1, IRET)
         IF (IRET.NE.0) GO TO 980
 5       CONTINUE
      CALL YSLECT ('ONNN', IGR1, 0, IBUFF1, IRET)
      IF (IRET.NE.0) GO TO 980
      IF (IGR1.NE.IGR2) THEN
         CALL YSLECT ('ONNN', IGR2, 0, IBUFF1, IRET)
         IF (IRET.NE.0) GO TO 980
         CALL YSLECT ('ONNN', IGR3, 0, IBUFF1, IRET)
         IF (IRET.NE.0) GO TO 980
         CALL YSLECT ('ONNN', IGR4, 0, IBUFF1, IRET)
         IF (IRET.NE.0) GO TO 980
         END IF
      DO 10 I = 1,NMXIMG*MAXGAU
         FUNCTY(I) = ' '
 10      CONTINUE
C                                       prepare menu
      J = 0
      IP = NOPT1 - MXPAIR
      DO 15 I = 1,MAXGAU
         DO 12 K = 1,I-1
            IP = IP + 1
            WRITE (CHOIC1(IP)(11:),1010) K, I
 12         CONTINUE
         DO 13 K = 1,NMXIMG
            LEAVE2(J+K) = .FALSE.
            WRITE (CHOIC2(J+K),1015) IMGTYP(K)(:JTRIM(IMGTYP(K))), I
 13         CONTINUE
         J = J + NMXIMG
 15      CONTINUE
      ISHELP = TSKNAM
      IMGOK = .FALSE.
      NG = NGAUSS
      NP = NMXIMG * NG
      MROWS(1) = NOPT1 - MXPAIR + (((NGAUSS-1)*NGAUSS)/2)
      MROWS(2) = NP
      DO 20 I = 1,MROWS(1)
         LEAVE(I) = LEAVE1(I)
         CHOICS(I) = CHOIC1(I)
 20      CONTINUE
      J = MROWS(1)
      DO 25 I = 1,MROWS(2)
         J = J + 1
         LEAVE(J) = LEAVE2(I)
         CHOICS(J) = CHOIC2(I)
 25      CONTINUE
      CALL YWINDO ('READ', WIND, IRET)
      IF (IRET.NE.0) THEN
         WIND(1) = 1
         WIND(3) = MAXXTV(1)
         END IF
      MAXCH = (WIND(3) - WIND(1)) / CSIZTV(1)
      IF (MAXCH.GT.71) THEN
         J = NG + 1
      ELSE
         J = 2 * NG + 1
         END IF
      MAXCH = (WIND(4) - WIND(2)) / CSIZTV(2) / 1.5 - 1.5 * J
      MCOL = 2
      MTYPE = -1
      IF ((MAXCH.LE.MROWS(1)) .OR. (MAXCH.LE.MROWS(2))) THEN
         MTYPE = 0
         MROWS(1) = NOPT1 - MXPAIR
         MROWS(2) = (((NG-1) * NG) / 2)
         IF (MROWS(2).LT.13) THEN
            MROWS(1) = MROWS(1) - 4
            MROWS(2) = MROWS(2) + 4
         ELSE IF (MROWS(2).GT.21) THEN
            J = (MROWS(2) - MROWS(1)) / 2
            MROWS(1) = MROWS(1) + J
            MROWS(2) = MROWS(2) - J
            END IF
         J = MAXCH / NMXIMG
         IF ((J.GT.(NG+1)/2) .AND. (J.LT.NG)) J = (NG+1)/2
         I = NG
         MCOL = 2
 30      IF (I.GT.0) THEN
            MCOL = MCOL + 1
            MROWS(MCOL) = NMXIMG * MIN (I, J)
            I = I - J
            GO TO 30
            END IF
         MAXCH = (WIND(3) - WIND(1)) / CSIZTV(1) - MCOL - 2
         IF (14*MCOL.GE.MAXCH) THEN
            J = 0
            DO 32 I = 1,MAXGAU
               DO 31 K = 1,NMXIMG
                  LEAVE2(J+K) = .FALSE.
                  WRITE (CHOIC2(J+K),1016) IMGTYP(K)(:JTRIM(IMGTYP(K))),
     *               I
 31               CONTINUE
               J = J + NMXIMG
 32            CONTINUE
            J = MROWS(1) + MROWS(2)
            K = MROWS(3) + MROWS(4) + MROWS(5) + MROWS(6) + MROWS(7) +
     *         MROWS(8)
            DO 33 I = 1,K
               J = J + 1
               LEAVE(J) = LEAVE2(I)
               CHOICS(J) = CHOIC2(I)
33             CONTINUE
            END IF
      ELSE IF (NG.GT.4) THEN
         MCOL = 4
         MTYPE = 0
         MROWS(1) = NOPT1 - MXPAIR - 4
         MROWS(2) = 4 + (((NG-1) * NG) / 2)
         MROWS(3) = NMXIMG * ((NG - 1) / 2 + 1)
         MROWS(4) = NP - MROWS(3)
         END IF
      NX = ITRC(1) - IBLC(1) + 1
      NY = ITRC(2) - IBLC(2) + 1
      CALL RFILL (MAXGAU, 0.0, SNMIN)
      MAXRES = 0.0
      CALL RFILL (2*MAXGAU, 0.0, MAXWID)
      CALL RFILL (2*MAXGAU, 0.0, MAXPK)
      NLIST = 0
      CALL RFILL (2*MAXGAU, 0.0, MAXOFF)
      CALL RFILL (MAXGAU, 0.0, MAXDWD)
      SUBWIN(1) = 1
      SUBWIN(2) = 1
      SUBWIN(3) = NX
      SUBWIN(4) = NY
C                                       Get image memory
      NWORDS = (NX * NY * (NP+1) - 1) / 1024 + 1 + 4
      PIMAGE = 0
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, IMAGES, PIMAGE, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GETTING DYNAMIC MEMORY FOR IMAGES'
         GO TO 990
         END IF
C                                       build images
      IF (.NOT.IMGOK) THEN
         CALL GETIMG (NX, NY, NP, IMAGES(PIMAGE+1), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'BUILDING IMAGES IN RAM'
            GO TO 990
            END IF
         IMGOK = .TRUE.
         END IF
C                                       clear graphics 3 and 4
      IF (IGR3.NE.IGR1) THEN
         IP = IGR3
         CALL YZERO (IP, IRET)
         IF (IRET.NE.0) GO TO 980
         IP = IGR4
         CALL YZERO (IP, IRET)
         IF (IRET.NE.0) GO TO 980
         IP = IGR7
         CALL YZERO (IP, IRET)
         IF (IRET.NE.0) GO TO 980
         END IF
C                                       menu selection
 50   CALL YWINDO ('READ', WIND, IRET)
      IF (IRET.NE.0) THEN
         WIND(1) = 1
         WIND(3) = MAXXTV(1)
         END IF
      MAXCH = (WIND(3) - WIND(1)) / CSIZTV(1)
      IF ((NG.EQ.1) .AND. (MAXCH.GT.126)) THEN
         WRITE (TITLE(1),1050) DOCAT, MAXRES, SNMIN(1), MAXPK(1,1),
     *      MAXPK(2,1), MAXWID(1,1), MAXWID(2,1), MAXOFF(1,1),
     *      MAXOFF(2,1), MAXDWD(1)
         J = 1
      ELSE
         WRITE (TITLE(1),1051) DOCAT, MAXRES
         J = 1
         DO 51 I = 1,NG
            J = J + 1
            IF (MAXCH.GT.101) THEN
               WRITE (TITLE(J),1052) I, SNMIN(I), MAXPK(1,I),
     *            MAXPK(2,I), MAXWID(1,I), MAXWID(2,I), MAXOFF(1,I),
     *            MAXOFF(2,I), MAXDWD(I)
            ELSE IF (MAXCH.GT.71) THEN
               WRITE (TITLE(J),1053) I, SNMIN(I), MAXPK(1,I),
     *            MAXPK(2,I), MAXWID(1,I), MAXWID(2,I), MAXOFF(1,I),
     *            MAXOFF(2,I), MAXDWD(I)
            ELSE
               WRITE (TITLE(J),1054) I, SNMIN(I), MAXPK(1,I),
     *            MAXPK(2,I), MAXDWD(I)
               J = J + 1
               WRITE (TITLE(J),1055) MAXWID(1,I), MAXWID(2,I),
     *            MAXOFF(1,I), MAXOFF(2,I)
               END IF
 51         CONTINUE
         END IF
      IF (TTYIND.LE.0) THEN
         CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, .FALSE., .TRUE., .TRUE.,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN TTY FOR INPUTS'
            TTYIND = 0
            GO TO 990
            END IF
         END IF
      CALL TVMENU (MTYPE, MCOL, MROWS, GRCHS, TOPSEP, SIDSEP, ISHELP,
     *   CHOICS, TIMLIM, LEAVE, J, TITLE, CHS, TVBUT, BUFF2, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RETURNED FROM TVMENU'
         GO TO 990
         END IF
C                                       never start with TV surpressed
      TVSUP = 0
C                                       case statement
C                                       exit
      IF (CHOICS(CHS).EQ.'EXIT') THEN
         MSGTXT = 'Bye-bye'
         CALL MSGWRT (2)
         GO TO 990
C                                       blank
      ELSE IF (CHOICS(CHS).EQ.' ') THEN
C                                       min S/N
      ELSE IF (CHOICS(CHS).EQ.'SET MIN S/N') THEN
         MSGBUF = 'Enter minimum amplitude signal to noise ratio' //
     *      ' each Gaussian'
         CALL INQFLT (TTYLUN, MSGBUF, NG, XX, IRET)
         IF (IRET.EQ.0) THEN
            DO 53 I = 1,NG
               SNMIN(I) = XX(I)
 53         CONTINUE
            END IF
C                                       max residual
      ELSE IF (CHOICS(CHS).EQ.'SET MAX RES') THEN
         MSGBUF = 'Enter maximum residual in image units'
         CALL INQFLT (TTYLUN, MSGBUF, 1, XX, IRET)
         IF (IRET.EQ.0) MAXRES = XX(1)
C                                       max freq offset
      ELSE IF (CHOICS(CHS).EQ.'SET PEAK RANGE') THEN
         MSGBUF = 'Enter min and max peak value' //
     *      ' each Gaussian'
         CALL INQFLT (TTYLUN, MSGBUF, 2*NG, XX, IRET)
         IF (IRET.EQ.0) THEN
            DO 54 I = 1,NG
               MAXPK(1,I) = XX(2*I-1)
               MAXPK(2,I) = XX(2*I)
 54            CONTINUE
            END IF
C                                       max freq offset
      ELSE IF (CHOICS(CHS).EQ.'SET OFFX RANGE') THEN
         MSGBUF = 'Enter min and max X offset in pixels' //
     *      ' each Gaussian'
         CALL INQFLT (TTYLUN, MSGBUF, 2*NG, XX, IRET)
         IF (IRET.EQ.0) THEN
            DO 55 I = 1,NG
               MAXOFF(1,I) = XX(2*I-1)
               MAXOFF(2,I) = XX(2*I)
 55            CONTINUE
            END IF
C                                       width ranges
      ELSE IF (CHOICS(CHS).EQ.'SET WIDTH RANGE') THEN
         MSGBUF = 'Enter min and max width in pixels' //
     *      ' each Gaussian'
         CALL INQFLT (TTYLUN, MSGBUF, 2*NG, XX, IRET)
         IF (IRET.EQ.0) THEN
            DO 56 I = 1,NG
               MAXWID(1,I) = XX(2*I-1)
               MAXWID(2,I) = XX(2*I)
 56            CONTINUE
            END IF
C                                       max width error
      ELSE IF (CHOICS(CHS).EQ.'SET MAX ERR WID') THEN
         MSGBUF = 'Enter max width error in pixels' //
     *      ' each Gaussian'
         CALL INQFLT (TTYLUN, MSGBUF, NG, XX, IRET)
         IF (IRET.EQ.0) THEN
            DO 57 I = 1,NG
               MAXDWD(I) = XX(I)
 57            CONTINUE
            END IF
C                                       redo all
      ELSE IF (CHOICS(CHS).EQ.'REDO ALL') THEN
         DOIT = MAXRES.GT.0.0
         DO 58 IG = 1,NG
            IF (SNMIN(IG).GT.0.0) DOIT = .TRUE.
            IF (MAXDWD(IG).GT.0.0) DOIT = .TRUE.
            IF (MAXPK(1,IG).LT.MAXPK(2,IG)) DOIT = .TRUE.
            IF (MAXWID(1,IG).LT.MAXWID(2,IG)) DOIT = .TRUE.
            IF (MAXOFF(1,IG).LT.MAXOFF(2,IG)) DOIT = .TRUE.
 58         CONTINUE
         IF (.NOT.DOIT) THEN
            MSGTXT = 'Min S/N, max residual, max peak, max width,'
     *         // ' max offset must be set'
            CALL MSGWRT (6)
         ELSE
            CALL YZERO (IGR5, IRET)
            CALL YZERO (1, IRET)
            CALL COPY (256, CATBLK, CATEMP)
            CALL COPY (256, CATOLD, CATBLK)
            CALL UPDALL ('REDO', SNMIN, MAXRES, MAXOFF, MAXPK, MAXWID,
     *         MAXDWD, NX, NY, IMAGES(PIMAGE+1), IRET)
            CALL COPY (256, CATEMP, CATBLK)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RE-DOING ALL'
               GO TO 990
               END IF
            END IF
C                                       flag all
      ELSE IF (CHOICS(CHS).EQ.'FLAG ALL') THEN
         DOIT = MAXRES.GT.0.0
         DO 59 IG = 1,NG
            IF (SNMIN(IG).GT.0.0) DOIT = .TRUE.
            IF (MAXDWD(IG).GT.0.0) DOIT = .TRUE.
            IF (MAXPK(1,IG).LT.MAXPK(2,IG)) DOIT = .TRUE.
            IF (MAXWID(1,IG).LT.MAXWID(2,IG)) DOIT = .TRUE.
            IF (MAXOFF(1,IG).LT.MAXOFF(2,IG)) DOIT = .TRUE.
 59         CONTINUE
        IF (.NOT.DOIT) THEN
            MSGTXT = 'Min S/N, max residual, max peak, max width,'
     *         // ' max offset must be set'
            CALL MSGWRT (6)
         ELSE
            CALL UPDALL ('FLAG', SNMIN, MAXRES, MAXOFF, MAXPK, MAXWID,
     *         MAXDWD, NX, NY, IMAGES(PIMAGE+1), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'FLAGGING ALL'
               GO TO 990
               END IF
            END IF
C                                       offzoom
      ELSE IF (CHOICS(CHS).EQ.'OFF ZOOM') THEN
         TVZOOM(1) = 0
         TVZOOM(2) = MAXXTV(1) / 2
         TVZOOM(3) = MAXXTV(2) / 2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'TUNING OFF ZOOM'
            GO TO 990
            END IF
C                                       offtr
      ELSE IF (CHOICS(CHS).EQ.'OFF TRANSFER') THEN
         IC = 2 ** NGRAY - 1
         ICOLOR = 7
         NLEVS = MAXINT + 1
         SLOPE = (LUTOUT+1.0) / (MAXINT+1.0)
         DO 67 I = 1,NLEVS
            IBUFF1(I) = I * SLOPE - 0.5
 67         CONTINUE
         CALL YLUT ('WRIT', IC, ICOLOR, .FALSE., IBUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'TUNING OFF BLACK&WHITE TRANSFER'
            GO TO 990
            END IF
         I = OFMINP + 1
         ICOLOR = 7
         CALL RFILL (I, 0.0, BUFF1)
         NLEVS = LUTOUT + 1
         IF (NLEVS.GT.OFMINP) NLEVS = OFMINP + 1
         OFFS = 1.0 / REAL (OFMOUT)
         SLOPE = OFFS * (OFMOUT + 1.0) / REAL(NLEVS)
         DO 167 I = 1,NLEVS
            BUFF1(I) = I * SLOPE - OFFS
 167        CONTINUE
         I = OFMINP + 1
         JJ = NLEVS
         I = I / NLEVS
         DO 267 II = 2,I
            CALL RCOPY (NLEVS, BUFF1, BUFF1(JJ+1))
            JJ = JJ + NLEVS
 267        CONTINUE
         CALL YOFM ('WRIT', ICOLOR, .FALSE., BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'TUNING OFF PSEUDO-COLOR'
            GO TO 990
            END IF
C                                       do-output
      ELSE IF (CHOICS(CHS).EQ.'SET DOOUTPUT') THEN
         DOCAT = DOCAT + 1
         IF (DOCAT.GT.3) DOCAT = 0
C                                       add to list
      ELSE IF (CHOICS(CHS).EQ.'ADD TO LIST') THEN
         IF (NLIST.GE.MAXLIS) THEN
            MSGTXT = 'LIST IS FULL'
            CALL MSGWRT (6)
         ELSE
            MSGBUF = 'Enter X and Y pixels to add to list: 2 integers'
            CALL INQINT (TTYLUN, MSGBUF, 2, IPOS, IRET)
            IF (IRET.NE.0) GO TO 50
            IF ((IPOS(1).LE.-1) .AND. (IPOS(1).GE.-NX) .AND.
     *         (IPOS(2).LE.-1) .AND. (IPOS(2).GE.-NY)) THEN
               IPOS(1) = -IPOS(1)
               IPOS(2) = -IPOS(2)
               DO 65 I = 1,NLIST
                  IF ((IPOS(1).EQ.PIXLIS(1,I)) .AND.
     *               (IPOS(2).EQ.PIXLIS(2,I))) GO TO 165
 65               CONTINUE
 165           IF (I.GT.NLIST) THEN
                  WRITE (MSGTXT,1165) IPOS, NX, NY
                  CALL MSGWRT (6)
               ELSE
                  DO 265 I1 = I+1,NLIST
                     PIXLIS(1,I) = PIXLIS(1,I1)
                     PIXLIS(2,I) = PIXLIS(2,I1)
                     I = I + 1
 265                 CONTINUE
                  NLIST = NLIST - 1
                  END IF
            ELSE IF ((IPOS(1).LT.1) .OR. (IPOS(1).GT.NX) .OR.
     *         (IPOS(2).LT.1) .OR. (IPOS(2).GT.NY)) THEN
               WRITE (MSGTXT,1265) IPOS, NX, NY
               CALL MSGWRT (6)
            ELSE
               NLIST = NLIST + 1
               PIXLIS(1,NLIST) = IPOS(1)
               PIXLIS(2,NLIST) = IPOS(2)
               END IF
            END IF
C                                       list list
      ELSE IF (CHOICS(CHS).EQ.'SHOW LIST') THEN
         IF (NLIST.LE.0) THEN
            MSGTXT = 'List is empty'
            CALL MSGWRT (6)
         ELSE
            I1 = 1
 66         I2 = MIN (NLIST, I1+3)
            IF (I2.GE.I1) THEN
               WRITE (MSGTXT,1066) (PIXLIS(1,I), PIXLIS(2,I), I = I1,I2)
               IF (I2-I1.LT.3) THEN
                  J = JTRIM (MSGTXT)
                  IF (MSGTXT(J:J).EQ.'(') MSGTXT(J:) = ' '
                  END IF
               CALL MSGWRT (2)
               I1 = I2 + 1
               GO TO 66
               END IF
            END IF
C                                       redo list
      ELSE IF (CHOICS(CHS).EQ.'REDO LIST') THEN
         IF (NLIST.LE.0) THEN
            MSGTXT = 'List is empty'
            CALL MSGWRT (6)
         ELSE
            CALL YZERO (IGR5, IRET)
            CALL YZERO (1, IRET)
            CALL COPY (256, CATBLK, CATEMP)
            CALL COPY (256, CATOLD, CATBLK)
            CALL UPDLIS ('REDO', NX, NY, IMAGES(PIMAGE+1), IRET)
            CALL COPY (256, CATEMP, CATBLK)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'UPDATING FIT OF PIXEL LIST'
               GO TO 990
               END IF
            END IF
C                                       flag list
      ELSE IF (CHOICS(CHS).EQ.'FLAG LIST') THEN
         IF (NLIST.LE.0) THEN
            MSGTXT = 'List is empty'
            CALL MSGWRT (6)
         ELSE
            CALL UPDLIS ('FLAG', NX, NY, IMAGES(PIMAGE+1), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'UPDATING FIT OF PIXEL LIST'
               GO TO 990
               END IF
            END IF
C                                       swap list
      ELSE IF (CHOICS(CHS)(:10).EQ.'SWAP LIST ') THEN
         IF (NLIST.LE.0) THEN
            MSGTXT = 'List is empty'
            CALL MSGWRT (6)
         ELSE
            IP = CHS - NOPT1 + (((MAXGAU-1)*MAXGAU)/2)
            CALL LISWAP (IP, NX, NY, IMAGES(PIMAGE+1), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'SWAPPING PIXEL LIST'
               GO TO 990
               END IF
            END IF
C                                       display image
      ELSE
         IP = CHS - MROWS(1)
         IF (MCOL.GT.2) IP = IP - MROWS(2)
         CALL SHOIMG (IP, NX, NY, NP, IGR5, IMAGES(PIMAGE+1), IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'SHOWING AN IMAGE'
            GO TO 990
         ELSE IF (IRET.LT.0) THEN
            IMGOK = .FALSE.
            IRET = 0
            END IF
         END IF
      GO TO 50
C                                       TV function failure
 980  WRITE (MSGTXT,1000) IRET, 'TV INIT FUNCTIONS'
C
 990  IF ((IRET.NE.0) .AND. (IRET.NE.99)) CALL MSGWRT (8)
      IF (PIMAGE.NE.0) CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, IMAGES,
     *   PIMAGE, I)
      IF (TTYIND.GT.0) THEN
         CALL ZCLOSE (TTYLUN, TTYIND, J)
         TTYIND = 0
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XGAUTV: ERROR',I4,' ON ',A)
 1010 FORMAT (I1,'-',I1)
 1015 FORMAT ('SHOW IMAGE ',A,I1)
 1016 FORMAT ('IMAGE ',A,I1)
 1050 FORMAT ('DOOUT=',I1,2X,'MAXRES=',F8.5,2X,'GAUSS 1: SNMIN=',F4.1,
     *   F7.1,' < PEAK <',F6.1,F6.1,' < WIDTH <',F5.1,F7.1,
     *   ' < OFFX <',F6.1,2X,'MAXDWID=',F5.1)
 1051 FORMAT ('DOOUT=',I1,2X,'MAXRES=',F8.5)
 1052 FORMAT ('GAUSS',I2,': SNMIN=',F4.1,F7.1,' < PEAK <',F6.1,F6.1,
     *   ' < WIDTH <',F5.1,F7.1,' < OFFX <',F6.1,2X,'MAXDWID=',F5.1)
 1053 FORMAT ('G',I2,' SN=',F4.1,F6.1,'<PEAK<',F5.1,F5.1,'<WIDTH<',
     *   F4.1,F6.1,'<OFFX<',F5.1,1X,'MXDW=',F4.1)
 1054 FORMAT ('GAUSS',I2,': SNMIN=',F4.1,F7.1,' < PEAK <',F6.1,2X,
     *   'MAXDWID=',F5.1)
 1055 FORMAT (8X,F6.1,' < WIDTH <',F5.1,F7.1,' < OFFX <',F6.1)
 1165 FORMAT ('POSITION',2I6,' NOT FOUND IN THE PIXEL LIST')
 1265 FORMAT ('POSITION',2I6,' OUTSIDE 1-',I5,' 1-',I5)
 1066 FORMAT (4('(',I5,',',I5,')',3X))
      END
      SUBROUTINE LISWAP (IP, NX, NY, IMAGE, IRET)
C-----------------------------------------------------------------------
C   LISWAP swaps values at a list of pixels
C   Inputs:
C      IP       I      Index into array of comps to swap
C      NX       I      Number X pixels in image
C      NY       I      Number Y pixels in image
C   In/out:
C      IMAGE    R(*)   Images of NP parameters
C   In/out (common):
C      NLIST    I      Number of pairs in list -> 0 if all are done
C   Output:
C      IRET     I      Error: > 0 => quit
C-----------------------------------------------------------------------
      INTEGER   IP, NX, NY, IRET
      REAL      IMAGE(NX,NY,*)
C
      INCLUDE 'XGAUS.INC'
      INTEGER   ILIST, IP1, IP2, IX, IY, I, LXGRNO, NGAU, YZPOS(2), LP1,
     *   LP2, LGSWAP(2,MXPAIR), INLIST, J
      REAL      TEMP, RESULT(MAXPRM,2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       compute LGSWAP
      IX = 0
      DO 10 I = 2,MAXGAU
         DO 9 J = 1,I-1
            IX = IX + 1
            LGSWAP(1,IX) = J
            LGSWAP(2,IX) = I
 9          CONTINUE
 10      CONTINUE
      INLIST = NLIST
      IP1 = (LGSWAP(1,IP) - 1) * NMXIMG
      IP2 = (LGSWAP(2,IP) - 1) * NMXIMG
      LP1 = (LGSWAP(1,IP) - 1) * 3 + 2
      LP2 = (LGSWAP(2,IP) - 1) * 3 + 2
C                                       swap list
      DO 50 ILIST = 1,INLIST
         IX = PIXLIS(1,INLIST+1-ILIST)
         IY = PIXLIS(2,INLIST+1-ILIST)
         IF ((IX.GT.0) .AND. (IX.LE.NX) .AND. (IY.GT.0) .AND.
     *      (IY.LE.NY)) THEN
            LXGRNO = (IY-1) * NX + IX
            IXGRNO = LXGRNO
            CALL TABXG ('READ', XGBUFF, IXGRNO, XGKOLS, XGNUMV, YZPOS,
     *         NGAU, VPEAK, RESULT, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ XG TABLE'
               GO TO 990
            ELSE IF (IRET.EQ.0) THEN
C                                       swap image values
               DO 20 I = 1,NMXIMG
                  TEMP = IMAGE(IX,IY,IP1+I)
                  IMAGE(IX,IY,IP1+I) = IMAGE(IX,IY,IP2+I)
                  IMAGE(IX,IY,IP2+I) = TEMP
 20               CONTINUE
C                                       swap table values too
               DO 30 I = 1,3
                  TEMP = RESULT(LP1+I,1)
                  RESULT(LP1+I,1) = RESULT(LP2+I,1)
                  RESULT(LP2+I,1) = TEMP
                  TEMP = RESULT(LP1+I,2)
                  RESULT(LP1+I,2) = RESULT(LP2+I,2)
                  RESULT(LP2+I,2) = TEMP
 30               CONTINUE
               IXGRNO = LXGRNO
               CALL TABXG ('WRIT', XGBUFF, IXGRNO, XGKOLS, XGNUMV,
     *            YZPOS, NGAU, VPEAK, RESULT, IRET)
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE XG TABLE'
                  GO TO 990
                  END IF
               END IF
            END IF
         NLIST = NLIST - 1
 50      CONTINUE
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LISWAP ERROR',I4,' ON ',A)
      END
      SUBROUTINE GETIMG (NX, NY, NP, IMAGE, IRET)
C-----------------------------------------------------------------------
C   GETIMG reads the XG file and makes images of the parameters
C   Inputs:
C      NX      I      Number X pixels in image
C      NY      I      Number Y pixels in image
C      NP      I      Number Z pixels in image (# gaussian parms + flux)
C   Output:
C      IMAGE   R(*)   Images of NP parameters
C      IRET    I      Error code
C      CATBLK  I(*)   in COMMON - output file header for images
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NP, IRET
      REAL      IMAGE(NX,NY,*)
C
      INCLUDE 'XGAUS.INC'
      INTEGER   I1, I2, IP, MP, YZPOS(2), NGAU, I, NAX, J, K
      REAL      RESULT(MAXPRM,2), XBLC(7), XTRC(7)
      DOUBLE PRECISION HALFAC, FMULT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA HALFAC /2.77258872D0/
C-----------------------------------------------------------------------
      FMULT = SQRT (PI / HALFAC)
      MSGTXT = 'Reading XG table to fill images'
      CALL MSGWRT (2)
C                                       blank fill
      I1 = NX * NY * NP
      CALL RFILL (I1, FBLANK, IMAGE)
C                                       blotch plane
      I1 = NX * NY
      CALL RFILL (I1, 0.0, IMAGE(1,1,NP+1))
C                                       loop through table
      MP = NP / NMXIMG
      IXGRNO = 1
      DO 50 I2 = 1,NY
         DO 40 I1 = 1,NX
            CALL TABXG ('READ', XGBUFF, IXGRNO, XGKOLS, XGNUMV, YZPOS,
     *         NGAU, VPEAK, RESULT, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ XG TABLE'
               GO TO 990
            ELSE IF ((IRET.EQ.0) .AND. (NGAU.GT.0)) THEN
               J = 0
               K = 2
               DO 20 IP = 1,MP
                  IMAGE(I1,I2,J+1) = RESULT(K+1,1)
                  IMAGE(I1,I2,J+2) = RESULT(K+2,1)
                  IMAGE(I1,I2,J+3) = RESULT(K+3,1)
                  IMAGE(I1,I2,J+5) = RESULT(K+1,2)
                  IMAGE(I1,I2,J+6) = RESULT(K+2,2)
                  IMAGE(I1,I2,J+7) = RESULT(K+3,2)
                  IF ((RESULT(K+1,1).EQ.FBLANK) .OR.
     *               (RESULT(K+3,1).EQ.FBLANK)) THEN
                     IMAGE(I1,I2,J+4) = FBLANK
                     IMAGE(I1,I2,J+8) = FBLANK
                  ELSE
                     IMAGE(I1,I2,J+4) = FMULT * RESULT(K+1,1) *
     *                  RESULT(K+3,1)
                     IF ((RESULT(K+1,2).EQ.FBLANK) .OR.
     *                  (RESULT(K+3,2).EQ.FBLANK)) THEN
                        IMAGE(I1,I2,J+8) = FBLANK
                     ELSE
                        IMAGE(I1,I2,J+8) = SQRT ((RESULT(K+1,1) *
     *                     RESULT(K+3,2))** 2 + (RESULT(K+1,2) *
     *                     RESULT(K+3,1))** 2) * FMULT
                        END IF
                     END IF
                  J = J + 8
                  K = K + 3
 20               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       make a header
      CALL COPY (256, CATOLD, CATBLK)
C                                       Get user modification to CATBLK
      CALL RCOPY (7, BLC, XBLC)
      CALL RCOPY (7, TRC, XTRC)
      XBLC(2) = IBLC(1)
      XBLC(3) = IBLC(2)
      XTRC(2) = ITRC(1)
      XTRC(3) = ITRC(2)
      CALL SUBHD3 (XBLC, XTRC, 1.0, 1.0, 1.0)
C                                       Basic output header: results
      CATBLK(KIDIM) = CATBLK(KIDIM) - 1
      NAX = CATBLK(KIDIM)
      DO 80 I = 1,NAX
         CATBLK(KINAX+I-1) = CATBLK(KINAX+I)
         CATR(KRCRP+I-1) = CATR(KRCRP+I)
         CATR(KRCRT+I-1) = CATR(KRCRT+I)
         CATR(KRCIC+I-1) = CATR(KRCIC+I)
         CATD(KDCRV+I-1) = CATD(KDCRV+I)
         CALL CHCOPY (8, 1, CATH(KHCTP+I*2), 1,
     *      CATH(KHCTP+(I-1)*2))
 80      CONTINUE
      DO 85 I = NAX,6
         CATBLK(KINAX+I) = 1
 85      CONTINUE
C
 990  IF (IRET.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETIMG: ERROR',I4,' ON ',A)
      END
      SUBROUTINE SHOIMG (IP, NX, NY, NP, IGR5, IMAGE, IRET)
C-----------------------------------------------------------------------
C   SHOIMG displays an image plane on the TV screen and allows an
C   interactive transfer function, coloring, CURVALUE, and EXIT.
C   Inputs:
C      IP      I      Desired plane
C      NX      I      Number X pixels in image
C      NY      I      Number Y pixels in image
C      NP      I      Number Z pixels in image (# gaussian parms)
C      IMAGE   R(*)   Images of NP parameters
C   Output:
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   IP, NX, NY, NP, IGR5, IRET
      REAL      IMAGE(NX,NY,*)
C
      INCLUDE 'XGAUS.INC'
      INTEGER   NOPTS
      PARAMETER (NOPTS=11+MAXGAU)
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:PTVC.INC'
C
      INTEGER   IX, IY, TVWIN(4), IWIN(4), NPIX, PLINC, IYTV, HORIZ,
     *   IBUFF(MABFSS), NLEVS, MCOL, MROWS, MTYPE, TIMLIM, TOPSEP, I,
     *   GRCHS(2), TVBUT, CHS, ITR, LUTBUF(TVMLUT), JJ, II, LG, LTY,
     *   LGSWAP(2,MAXGAU), NW, EX(5), EY(5), NXFRAM, NYFRAM, CFRAME,
     *   TFRAME, PINC, LNX, LNY, IC(2), NPIXW, WXPOS, JTRIM, JT, J,
     *   MMCOL, POFF, NEDGE, SIDSEP, MINC, MPIX, JBUFF(MABFSS), JNX,
     *   JNY, NBO, MBOX, IGR
      CHARACTER TRANFN*2, CHOICS(NOPTS+1)*12, ISHELP*8, TITLE*132,
     *   TVALS(8)*16, FUNCS(4)*2, BUNITS*8, BTEMP*8
      REAL      PMIN, PMAX, RPOS(2), SLOPE, OFFS, TEMP, BLCO(7),
     *   TRCO(7)
      LOGICAL   LEAVE(NOPTS+1), DOWEDG, DOEDGE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (IBUFF, BUFF2, LUTBUF), (JBUFF, BUFF1)
      DATA MCOL, MTYPE, TIMLIM, TOPSEP, SIDSEP /1, -1, 0, 3, 10/
      DATA CHOICS /'RETURN', ' ','LOAD AS','SET WINDOW','RESET WINDOW',
     *   'OFF TRANSF', 'OFF COLOR', 'TVTRANSF', 'TVPSEUDO', 'TVPHLAME',
     *   'TVZOOM', 'CURVALUE', MAXGAU*' '/
      DATA LEAVE /3*.FALSE., .TRUE., .FALSE., 2*.TRUE., 4*.FALSE.,
     *   MAXGAU*.FALSE., .FALSE./
      DATA TVALS /'PEAK', 'CENTER (PIXELS)', 'WIDTH (PIXELS)',
     *   'FLUX (PEAK*PIX)', 'ERR PEAK', 'ERR CENTER (PIX)',
     *   'ERR WIDTH (PIX)', 'ERROR FLUX'/
      DATA FUNCS /'LN', 'SQ', 'LG', 'L2'/
C-----------------------------------------------------------------------
      MMCOL = 2
      IF (NP.GT.NMXIMG*4) MMCOL = 4
      LG = (IP-1) / NMXIMG + 1
      LTY = MOD (IP-1, NMXIMG) + 1
      CALL YWINDO ('READ', WINDTV, IRET)
C                                       compute swap lists
      JJ = 0
      II = NOPTS - MAXGAU + 1
      J = LG
      DO 5 I = 1,NGAUSS
         IF (I.NE.J) THEN
            JJ = JJ + 1
            II = II + 1
            LGSWAP(1,JJ) = J
            LGSWAP(2,JJ) = I
            WRITE (CHOICS(II),1005) J, I
            END IF
 5       CONTINUE
      MROWS = II
C                                       find max/min
 10   JNX = SUBWIN(3) - SUBWIN(1)
      JNY = SUBWIN(4) - SUBWIN(2)
      PMIN = 1.E15
      PMAX = -PMIN
      DO 20 IY = SUBWIN(2),SUBWIN(4)
         DO 15 IX = SUBWIN(1),SUBWIN(3)
            IF (IMAGE(IX,IY,IP).NE.FBLANK) THEN
               PMIN = MIN (PMIN, IMAGE(IX,IY,IP))
               PMAX = MAX (PMAX, IMAGE(IX,IY,IP))
               END IF
 15         CONTINUE
 20      CONTINUE
C                                       too big for TV?
      NXFRAM = (JNX - 1) / (MAXXTV(1)-33) + 1
      NYFRAM = (JNY - 1) / (MAXXTV(2)-33) + 1
      TFRAME = NXFRAM * NYFRAM
      CFRAME = 0
      PINC = MAX (NXFRAM, NYFRAM)
      LNX = JNX / PINC
      LNY = JNY / PINC
      MINC = 1
      IF (PINC.EQ.1) THEN
         JJ = 256
         IF ((MAXXTV(1).GT.650) .AND. (MAXXTV(2).GT.650)) JJ = 512
         IF ((2*JNX.LE.JJ) .AND. (2*JNY.LE.JJ)) THEN
            MINC = JJ / JNX
            IF (JJ/JNY.LT.MINC) MINC = JJ / JNY
            MINC = MIN (20, MINC)
            LNX = MINC * JNX
            LNY = MINC * JNY
            END IF
         END IF
      IC(1) = SUBWIN(1)
      IC(2) = SUBWIN(2)
C                                       menu list
      IF (TFRAME.GT.1) THEN
         MROWS = MROWS + 1
         CHOICS(MROWS) = 'NEXT WINDOW'
         POFF = 0
C                                       menu is on right and left sides
      ELSE IF (MMCOL.LE.2) THEN
         POFF = 0
C                                       offset image from menus
C                                       menu all on left
      ELSE
         NEDGE = (CSIZTV(1) + 1) / 2
         POFF = 2 * (2 + NEDGE + (MMCOL-1)*(1+NEDGE)) + 56 * CSIZTV(1)
         POFF = POFF + 7 + NEDGE
         IF (POFF+LNX.GT.WINDTV(3)-WINDTV(1)) POFF = 0
         END IF
C                                       width of wedge
      NW = MIN (JNY, 16)
C                                       no real image
      IF (PMAX.LT.PMIN) THEN
         MSGTXT = 'NO VALID PIXELS FOUND'
         CALL MSGWRT (7)
         IRET = -1
C                                       okay do it
      ELSE
         TRANFN = FUNCTY(IP)
         ITR = 1
         DO 30 I = 2,4
            IF (TRANFN.EQ.FUNCS(I)) ITR = I
 30         CONTINUE
         ITR = MOD (ITR, 4) + 1
C                                       header adjust
C         CALL H2CHR (8, 1, OLDH(KHBUN), BUNITS)
         BUNITS = 'OptDepth'
         BTEMP = BUNITS(:4) // '*PIX'
         IF (MOD(IP-1,4).EQ.0) THEN
            CALL CHR2H (8, BUNITS, 1, CATH(KHBUN))
         ELSE IF (MOD(IP-1,4).EQ.3) THEN
            CALL CHR2H (8, BTEMP, 1, CATH(KHBUN))
         ELSE
            CALL CHR2H (8, 'PIXELS  ', 1, CATH(KHBUN))
            END IF
         CATR(KRDMX) = PMAX
         CATR(KRDMN) = PMIN
         CATR(IRRAN+1) = PMAX
         CATR(IRRAN) = PMIN
         WRITE (MSGTXT,1020) LG, PMIN, PMAX, TVALS(LTY)
         CALL MSGWRT (2)
C                                       window
 50      DOWEDG = .FALSE.
         DOEDGE = (LNX.LT.MAXXTV(1)-2) .AND. (LNY.LT.MAXXTV(2)-2)
         IF (LNX.LE.MAXXTV(1)) THEN
            IWIN(1) = IC(1)
            IWIN(3) = IC(1) - 1 + (LNX/MINC) * PINC
            TVWIN(1) = (MAXXTV(1) - POFF - LNX) / 2 + POFF
            TVWIN(3) = TVWIN(1) + LNX - 1
            END IF
         IF (LNY.LE.MAXXTV(2)) THEN
            IF (LNY.LE.MAXXTV(2)-24) THEN
               IY = (NW * 2) / 3
               TVWIN(2) = (MAXXTV(2)-IY - LNY) / 2 + 16
               TVWIN(4) = TVWIN(2) + LNY - 1
               DOWEDG = .TRUE.
            ELSE
               TVWIN(2) = (MAXXTV(2) - LNY) / 2
               TVWIN(4) = TVWIN(2) + LNY - 1
               END IF
            IWIN(2) = IC(2)
            IWIN(4) = IC(2) - 1 + (LNY/MINC) * PINC
            END IF
         CALL COPY (4, IWIN, CATBLK(IIWIN))
         CALL COPY (4, TVWIN, CATBLK(IICOR))
C                                       not from disk
         CATBLK(IIVOL) = 0
         CATBLK(IICNO) = 0
         IPL(1) = 1
         IPL(2) = 1
         CALL YZERO (IGR5, IRET)
         CALL YZERO (IPL, IRET)
         IF (TFRAME.GT.1) THEN
            IF (PINC.EQ.1) THEN
               WRITE (MSGTXT,1050) CFRAME
            ELSE
               WRITE (MSGTXT,1051) PINC
               END IF
            CALL MSGWRT (2)
         ELSE IF (MINC.GT.1) THEN
            WRITE (MSGTXT,1052) MINC
            CALL MSGWRT (2)
            END IF
C                                       return here to reload
 60      CALL CHR2H (2, TRANFN, 1, CATH(IITRA))
         CHOICS(3)(9:10) = FUNCS(ITR)
         FUNCTY(IP) = TRANFN
         CALL YCWRIT (IPL(1), TVWIN, CATBLK, SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE IMAGE CATALOG'
            GO TO 990
            END IF
         CALL YHOLD ('ONNN', IRET)
         CALL YSLECT ('ONNN', IPL, 0, SCRTCH, IRET)
C                                       write TV image
         IYTV = TVWIN(2) - 1
         NPIX = (IWIN(3) - IWIN(1)) / PINC + 1
         MPIX = NPIX * MINC
         IF (MPIX.GT.17) THEN
            NPIXW = MPIX
            WXPOS = TVWIN(1)
         ELSE
            NPIXW = 17
            WXPOS = TVWIN(1) - (18-NPIX)/2
            WXPOS = MAX (1, WXPOS)
            END IF
         PLINC = 1
         HORIZ = 0
         DO 70 IY = IWIN(2),IWIN(4),PINC
            IYTV = IYTV + 1
            CALL ISCALE (TRANFN, MAXINT, CATR(IRRAN), NPIX*PINC, PINC,
     *         IMAGE(IWIN(1),IY,IP), IBUFF)
            IF (MINC.EQ.1) THEN
               CALL YIMGIO ('WRIT', IPL(1), TVWIN(1), IYTV, HORIZ, NPIX,
     *            IBUFF, IRET)
            ELSE
               DO 64 I = 1,NPIX
                  CALL FILL (MINC, IBUFF(I), JBUFF(MINC*(I-1)+1))
 64               CONTINUE
               IYTV = IYTV - 1
               DO 65 I = 1,MINC
                  IYTV = IYTV + 1
                  IF (IRET.EQ.0) CALL YIMGIO ('WRIT', IPL(1), TVWIN(1),
     *               IYTV, HORIZ, MPIX, JBUFF, IRET)
 65               CONTINUE
               END IF
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING IMAGE TO TV'
               GO TO 990
               END IF
 70         CONTINUE
         IF (DOWEDG) THEN
            IYTV = TVWIN(2) - (NW+1)/2
            SLOPE = (PMAX - PMIN) / (NPIXW - 1)
            DO 80 IY = 1,NPIXW
               BUFF1(IY) = (IY - 1.0) * SLOPE + PMIN
 80            CONTINUE
            CALL ISCALE (TRANFN, MAXINT, CATR(IRRAN), NPIXW, 1, BUFF1,
     *         IBUFF)
            DO 90 IY = 1,NW
               IYTV = IYTV - 1
               CALL YIMGIO ('WRIT', IPL(1), WXPOS, IYTV, HORIZ, NPIXW,
     *            IBUFF, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITING WEDGE TO TV'
                  GO TO 990
                  END IF
 90            CONTINUE
            END IF
C                                       line around
         IF (DOEDGE) THEN
            EX(1) = TVWIN(1) - 1
            EY(1) = TVWIN(2) - 1
            EX(3) = TVWIN(3) + 1
            EY(3) = TVWIN(4) + 1
            EX(2) = EX(3)
            EY(2) = EY(1)
            EX(4) = EX(1)
            EY(4) = EY(3)
            EX(5) = EX(1)
            EY(5) = EY(1)
            IGR5 = NGRAY + 5
            CALL YSLECT ('ONNN', IGR5, 0, IBUFF, IRET)
            IF (IRET.EQ.0) CALL IMVECT ('ONNN', IGR5, 5, EX, EY, IBUFF,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'DRAWING EDGE LINE AROUND' //
     *            ' THE IMAGE'
               GO TO 990
               END IF
            END IF
         CALL YHOLD ('OFFF', IRET)
         NLEVS = LUTOUT + 1
         ISHELP = TSKNAM
C                                       simple menu
C                                       menu selection
         IX = MOD (IP - 1, NMXIMG) + 1
         IY = (IP - 1) / NMXIMG + 1
         IF (IX.EQ.1) THEN
            TVALS(1) = 'PEAK (OptDepth)'
            END IF
         JT = JTRIM (TVALS(IX))
         TEMP = MAX (ABS(PMIN), ABS(PMAX))
         IF ((TEMP.LT.10000.) .AND. (TEMP.GT.0.001)) THEN
            WRITE (TITLE,1090) IY, CFRAME, TVALS(IX)(:JT), PMIN, PMAX
         ELSE
            WRITE (TITLE,1091) IY, CFRAME, TVALS(IX)(:JT), PMIN, PMAX
            END IF
         IF (TFRAME.LE.1) TITLE(22:25) = ' '
         CALL REFRMT (TITLE, '_', I)
         GRCHS(1) = 2
         GRCHS(2) = 1
         RPOS(1) = MAXXTV(1) / 2
         RPOS(2) = MAXXTV(2) / 2
 100     CALL TVMENU (MTYPE, MCOL, MROWS, GRCHS, TOPSEP, SIDSEP, ISHELP,
     *      CHOICS, TIMLIM, LEAVE, 1, TITLE, CHS, TVBUT, BUFF2, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RETURNED FROM TVMENU'
            GO TO 990
            END IF
C                                       return to main menu
         IF (CHOICS(CHS).EQ.'RETURN') THEN
            MSGTXT = 'Returning to main menu'
            CALL MSGWRT (2)
            GO TO 999
C                                       blank
         ELSE IF (CHOICS(CHS).EQ.' ') THEN
C                                       set window
         ELSE IF (CHOICS(CHS).EQ.'SET WINDOW') THEN
            CALL YCINIT (IGR5, BUFF2)
            NBO = 0
            MBOX = 1
            CALL RCOPY (7, BLC, BLCO)
            CALL RCOPY (7, TRC, TRCO)
            IGR = IGR5 - NGRAY
            CALL GRBOXS (IGR, MBOX, NBO, BLCO, TRCO, BUFF2, IRET)
            IF (IRET.EQ.0) THEN
               SUBWIN(1) = BLCO(1) + 0.1
               SUBWIN(2) = BLCO(2) + 0.1
               SUBWIN(3) = TRCO(1) + 0.1
               SUBWIN(4) = TRCO(2) + 0.1
               WRITE (MSGTXT,1110) SUBWIN
               CALL MSGWRT (2)
               END IF
            GO TO 10
C                                       reset window
         ELSE IF (CHOICS(CHS).EQ.'RESET WINDOW') THEN
            SUBWIN(1) = 1
            SUBWIN(2) = 1
            SUBWIN(3) = NX
            SUBWIN(4) = NY
            GO TO 10
C                                       change transfer function
         ELSE IF (CHOICS(CHS)(:8).EQ.'LOAD AS ') THEN
            TRANFN = FUNCS(ITR)
            ITR = MOD (ITR, 4) + 1
            GO TO 60
C                                       TV transfer func OFF
         ELSE IF (CHOICS(CHS).EQ.'OFF TRANSF') THEN
            IYTV = MAXINT + 1
            SLOPE = (LUTOUT+1.0) / (MAXINT+1.0)
            DO 110 I = 1,IYTV
               LUTBUF(I) = I * SLOPE - 0.5
 110           CONTINUE
            I = 2 ** (IPL(1)-1)
            CALL YLUT ('WRIT', I, 7, .FALSE., LUTBUF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RETURNED BY OFF TRANSF'
               GO TO 990
               END IF
C                                       TV color func OFF
         ELSE IF (CHOICS(CHS).EQ.'OFF COLOR') THEN
            I = OFMINP + 1
            CALL RFILL (I, 0.0, BUFf1)
            NLEVS = LUTOUT + 1
            IF (I.LT.NLEVS) NLEVS = I
            OFFS = 1.0 / REAL (OFMOUT)
            SLOPE = OFFS * (OFMOUT + 1.0) / REAL(NLEVS)
            DO 120 I = 1,NLEVS
               BUFF1(I) = I * SLOPE - OFFS
 120           CONTINUE
            I = (OFMINP + 1) / NLEVS
            JJ = NLEVS
            DO 130 II = 2,I
               CALL RCOPY (NLEVS, BUFF1, BUFF1(JJ+1))
               JJ = JJ + NLEVS
 130           CONTINUE
            CALL YOFM ('WRIT', 7, .FALSE., BUFF1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RETURNED BY OFF COLOR'
               GO TO 990
               END IF
C                                       TV transfer func
         ELSE IF (CHOICS(CHS).EQ.'TVTRANSF') THEN
            I = 2 ** (IPL(1)-1)
            IYTV = 1
            CALL IENHNS (I, 7, IYTV, RPOS, BUFF1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RETURNED BY TVTRANSF'
               GO TO 990
               END IF
C                                       TV pseudo colors
         ELSE IF (CHOICS(CHS).EQ.'TVPSEUDO') THEN
            CALL TVPSUD (NLEVS, BUFF1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RETURNED BY TVPSEUDO'
               GO TO 990
               END IF
C                                       TV flame colors
         ELSE IF (CHOICS(CHS).EQ.'TVPHLAME') THEN
            CALL TVFLAM (NLEVS, BUFF1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RETURNED BY TVPHLAME'
               GO TO 990
               END IF
C                                       TV zoom
         ELSE IF (CHOICS(CHS).EQ.'TVZOOM') THEN
            CALL TVZOME (IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RETURNED BY TVZOOM'
               GO TO 990
               END IF
C                                       Cursor value: local version
         ELSE IF (CHOICS(CHS).EQ.'CURVALUE') THEN
            CALL TVALUE (GRCHS(1), NX, NY, IMAGE(1,1,IP), NLIST, PIXLIS,
     *         BUFF1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RETURNED BY TVALUE'
               GO TO 990
               END IF
C                                       load next portion
         ELSE IF (CHOICS(CHS).EQ.'NEXT WINDOW') THEN
            CFRAME = CFRAME + 1
            IF (CFRAME.GT.TFRAME) THEN
               CFRAME = 0
               PINC = MAX (NXFRAM, NYFRAM)
               IC(1) = SUBWIN(1)
               IC(2) = SUBWIN(2)
               LNX = JNX / PINC
               LNY = JNY / PINC
            ELSE
               PINC = 1
               IF (NXFRAM.EQ.1) THEN
                  IC(1) = SUBWIN(1)
                  LNX = JNX
               ELSE
                  II = MOD (CFRAME-1, NXFRAM) + 1
                  IC(1) = (II - 1) * (MAXXTV(1) - 3) + SUBWIN(1)
                  IF (IC(1)+MAXXTV(1)-33.GT.NX) IC(1) = JNX - MAXXTV(1)
     *               + 33
                  LNX = MAXXTV(1) - 33
                  END IF
               IF (NYFRAM.EQ.1) THEN
                  IC(2) = SUBWIN(2)
                  LNY = JNY
               ELSE
                  II = (CFRAME-1) / NXFRAM + 1
                  IC(2) = (II - 1) * (MAXXTV(2) - 3) + SUBWIN(2)
                  IF (IC(2)+MAXXTV(2)-33.GT.NY) IC(2) = JNY - MAXXTV(2)
     *               + 33
                  LNY = MAXXTV(2) - 33
                  END IF
               END IF
            GO TO 50
C                                       blotch and swap
         ELSE IF (CHOICS(CHS)(:5).EQ.'SWAP ') THEN
            II = CHS - NOPTS + MAXGAU - 1
            CALL BLSWAP (LGSWAP(1,II), MINC, NX, NY, IMAGE, IBUFF1,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'TV BLOTCH AND SWAP GAUSSIANS'
               GO TO 990
               END IF
            GO TO 10
            END IF
         GO TO 100
         END IF
C
 990  IF (IRET.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SHOIMG: ERROR:',I4,' ON ',A)
 1005 FORMAT ('SWAP',I2,'-',I1)
 1020 FORMAT ('Loading Gauss',I2,' image',F10.5,' to',F10.5,' of ',
     *   A)
 1050 FORMAT ('Loading every pixel in subimage',I3)
 1051 FORMAT ('Loading full image with only every',I3,
     *   ' pixels in X and Y')
 1052 FORMAT ('Loading every pixel replicated by',I3)
 1090 FORMAT ('Gaussian component',I2,'_(',I2,') _',A,'_',F11.5,' TO',
     *   F11.5)
 1091 FORMAT ('Gaussian component',I2,'_(',I2,') _',A,'_',1PE11.3,' TO',
     *   1PE11.3)
 1110 FORMAT ('BLC/TRC=',4I7)
      END
      SUBROUTINE TVALUE (GR, NX, NY, IMAGE, NLIST, PIXLIS, BUFFER, IRET)
C-----------------------------------------------------------------------
C   TVALUE performs interactive displays of map image values:
C   Special version for XGAUS - allows picking pixels for list
C   Inputs:
C      GR        I        Graphics plane for lettering
C      NX        I        Number X pixels in image
C      NY        I        Number Y pixels in image
C      IMAGE     R(*)     Image values
C   In/out:
C      NLIST     I        Number entries in PIXLIS
C      PIXLIST   I(2,*)   List of pixels
C   Output:
C      BUFFER    I(*)     Scratch buffer
C      IRET      I        Basic TV error code
C-----------------------------------------------------------------------
      INTEGER   GR, NX, NY, NLIST, PIXLIS(2,*), IRET
      REAL      IMAGE(NX,NY), BUFFER(*)
C
      CHARACTER STRING*16, PREFIX*5, ITRTYP(8)*2, LMTYPS(2)*2, LMTYPE*2
      INTEGER   MIND, IG, IG1, IG2, ITW(3), NPIX, NROW, MAG, IX0, IY0,
     *   IX, IY, IP, ECOUNT, QUAD, IBUT, ITR, ICMASK, ZAND, ISCX,
     *   ISCY, I, INCNO, INVOL, LDEP(5), ITG1, ITG2, ITEMP, IX1, IY1,
     *   MSGSAV, LBUT
      REAL      PPOS(2), RPOS(2), PIXVAL, CORN(7)
      LOGICAL   T, F, EQUAL, DOIT, FROMTV, BLNKD, NOQUAD
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
      FROMTV = .TRUE.
      CALL ZTIME (ITW)
      MSGSAV = MSGSUP
      NOQUAD = .TRUE.
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
      WRITE (MSGTXT,1100)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1101)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1102)
      CALL MSGWRT (1)
C                                       turn on cursor
      IP = 0
      ECOUNT = 0
      IG = IG1 + NGRAY
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Cursor read loop point
 110  CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IRET)
      IF ((IBUT.GE.4) .OR. (IRET.NE.0)) GO TO 970
      LBUT = IBUT
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (.NOT.DOIT) GO TO 110
C                                       Find new image catalog block
         QUAD = 0
         CALL YCURSE ('FXIT', F, T, RPOS, QUAD, IBUT, IRET)
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
         CALL IMA2MP (RPOS, CORN)
         IX = CORN(1) + 0.51
         IY = CORN(2) + 0.51
         PIXVAL = IMAGE(IX,IY)
         BLNKD = IMAGE(IX,IY).EQ.FBLANK
C                                       Button A or B => add to lists
         IF (LBUT.GT.0) THEN
            NLIST = NLIST + 1
            PIXLIS(1,NLIST) = IX
            PIXLIS(2,NLIST) = IY
            WRITE (MSGTXT,1135) IX, IY
            CALL MSGWRT (2)
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
C                                       Button A or B => add to lists
         IF (IBUT.GT.0) THEN
            NLIST = NLIST + 1
            PIXLIS(1,NLIST) = IX
            PIXLIS(2,NLIST) = IY
            END IF
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
 975  CALL YCURSE ('OFFF', F, F, RPOS, QUAD, IBUT, IRET)
      ITEMP = 2 ** NGRAY
      IF ((ISCX.NE.0) .OR. (ISCY.NE.0)) CALL YSCROL (ITEMP, ISCX,
     *   ISCY, F, IRET)
      CALL YZERO (ITG1, I)
      IF (IG2.NE.0) CALL YZERO (ITG2, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Cursor selects which pixel is displayed')
 1101 FORMAT ('Hit button A or B to add the current pixel to list')
 1102 FORMAT ('Hit button C or D to exit')
 1116 FORMAT ('TVALUE: ',2I7,' NOT IN VALID IMAGE')
 1135 FORMAT ('Pixel',I5,',',I5,' added to list')
 1170 FORMAT ('X=',I4,' Y=',I4)
 1171 FORMAT (I6,I7)
 1172 FORMAT ('B=',F8.3)
 1960 FORMAT ('TVALUE: IMAGE CAT FILE IO ERROR',I7)
 1970 FORMAT ('TVALUE: TV ACTION IO ERROR',I7)
      END
      SUBROUTINE BLSWAP (LGSWAP, MINC, NX, NY, IMAGE, BUFF, IRET)
C-----------------------------------------------------------------------
C   BLSWAP prompts the user to mark a blotch region to swap the image
C   values and then does the swap.
C   Inputs:
C      LGSWAP   I(2)   Two Gaussians to be swapped
C      NX       I      Number X pixels in image
C      NY       I      Number Y pixels in image
C   In/out:
C      IMAGE    R(*)   Images of NP parameters
C   Output:
C      BUFF     I(*)   scratch buffer
C      IRET     I      Error: > 0 => quit
C-----------------------------------------------------------------------
      INTEGER   LGSWAP(2), MINC, NX, NY, BUFF(*), IRET
      REAL      IMAGE(NX,NY,*)
C
      INTEGER   MPOLY
      PARAMETER (MPOLY = 100)
C
      INCLUDE 'XGAUS.INC'
      INTEGER   NVERT(MPOLY), XV(10*MPOLY), YV(10*MPOLY), LNX, IP1, IP2,
     *   IXL(MPOLY), IXU(MPOLY), IX, IY, IGR, NPOLY, I, LXGRNO, NGAU,
     *   YZPOS(2), LP1, LP2
      LOGICAL   DOIT
      REAL      TEMP, RESULT(MAXPRM,2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      IGR = MIN (3, NGRAPH)
C                                       interactive polygons
      NPOLY = 0
      CALL FILL (MPOLY, 0, NVERT)
      I = 10 * MPOLY
      CALL FILL (I, 0, XV)
      CALL FILL (I, 0, YV)
      IF (MINC.LE.1) THEN
         CALL GRPOLY (MPOLY, IGR, NPOLY, NVERT, XV, YV, BUFF, IRET)
      ELSE
         CALL XGPOLY (MINC, MPOLY, IGR, NPOLY, NVERT, XV, YV, BUFF,
     *      IRET)
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'SETTING POLYGONS ON THE TV'
         CALL MSGWRT (7)
         IRET = 0
         GO TO 999
      ELSE IF (NPOLY.LE.0) THEN
         MSGTXT = 'No polygons selected'
         CALL MSGWRT (2)
C                                       do the swap
      ELSE
         IP1 = (LGSWAP(1) - 1) * NMXIMG
         IP2 = (LGSWAP(2) - 1) * NMXIMG
         LP1 = (LGSWAP(1) - 1) * 3 + 2
         LP2 = (LGSWAP(2) - 1) * 3 + 2
C                                       loop over rows
         DO 50 IY = 1,NY
C                                       list X ranges in this row
            CALL BLTLIS (1, 1, NPOLY, NVERT, XV, YV, IY, LNX, IXL, IXU)
            IF (LNX.GT.0) THEN
C                                       loop over columns
               DO 40 IX = 1,NX
                  DOIT = .FALSE.
                  DO 20 I = 1,LNX
                     IF ((IX.GE.IXL(I)) .AND. (IX.LE.IXU(I))) DOIT =
     *                  .TRUE.
 20                  CONTINUE
                  IF (DOIT) THEN
C                                       swap image values
                     DO 30 I = 1,NMXIMG
                        TEMP = IMAGE(IX,IY,IP1+I)
                        IMAGE(IX,IY,IP1+I) = IMAGE(IX,IY,IP2+I)
                        IMAGE(IX,IY,IP2+I) = TEMP
 30                     CONTINUE
C                                       swap table values too
                     LXGRNO = (IY-1) * NX + IX
                     IXGRNO = LXGRNO
                     CALL TABXG ('READ', XGBUFF, IXGRNO, XGKOLS, XGNUMV,
     *                  YZPOS, NGAU, VPEAK, RESULT, IRET)
                     IF (IRET.GT.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'READ XG TABLE'
                        GO TO 990
                     ELSE IF (IRET.EQ.0) THEN
                        DO 35 I = 1,3
                           TEMP = RESULT(LP1+I,1)
                           RESULT(LP1+I,1) = RESULT(LP2+I,1)
                           RESULT(LP2+I,1) = TEMP
                           TEMP = RESULT(LP1+I,2)
                           RESULT(LP1+I,2) = RESULT(LP2+I,2)
                           RESULT(LP2+I,2) = TEMP
 35                        CONTINUE
                        IXGRNO = LXGRNO
                        CALL TABXG ('WRIT', XGBUFF, IXGRNO, XGKOLS,
     *                     XGNUMV, YZPOS, NGAU, VPEAK, RESULT, IRET)
                        IF (IRET.GT.0) THEN
                           WRITE (MSGTXT,1000) IRET, 'WRITE XG TABLE'
                           GO TO 990
                           END IF
                        END IF
                     END IF
 40               CONTINUE
               END IF
 50         CONTINUE
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
      IGR = IGR + NGRAY
      CALL YZERO (IGR, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLSWAP ERROR',I4,' ON ',A)
      END
      SUBROUTINE UPDLIS (OPER, NX, NY, IMAGE, IRET)
C-----------------------------------------------------------------------
C   UPDLIS flags or re-does fitting on a list of pixels
C   Inputs:
C      OPER    C*4    'REDO', 'FLAG'
C      NX      I      Number X pixels in image
C      NY      I      Number Y pixels in image
C   In/out:
C      IMAGE   R(*)   Images of NP parameters - updated on output
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IRET
      CHARACTER OPER*4
      REAL      IMAGE(NX,NY,*)
C
      INCLUDE 'XGAUS.INC'
      INCLUDE 'XGAUSD.INC'
      CHARACTER PHNAME*48
      INTEGER   IROUND, LUNI, NYI, NXI, WINI(4), BOI, J, I1, IPOS(7),
     *   BOTEMP, IBIND, INDI, LIM1, IG, NGAU, I, IX, IY, LGRNO, INLIST,
     *   YZPOS(2), LZOOM(3), K, NG
      REAL      RESULT(2*MAXPRM+2*MAXGAU)
      DOUBLE PRECISION PARMS(MAXPRM), UPARMS(MAXPRM), XPARMS(MAXPRM),
     *   HALFAC, FMULT
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA HALFAC /2.77258872D0/
      DATA LUNI /16/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      FMULT = SQRT (PI / HALFAC)
C                                       display parms
      UCHAN = 0
      INLIST = NLIST
      NG = NGAUSS
      DOEVEN = .TRUE.
      CALL DFILL (MAXPRM, 0.0D0, XPARMS)
C                                       Open and init for read
      IF (OPER.EQ.'REDO') THEN
         CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, PHNAME, IRET)
         CALL ZOPEN (LUNI, INDI, DISKIN, PHNAME, T, F, T, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE'
            GO TO 990
            END IF
C                                       Setup for I/O
         NXI = CATOLD(KINAX)
         NYI = CATOLD(KINAX+1)
         WINI(1) = IROUND (UBLC(1))
         WINI(2) = IROUND (BLC(2))
         WINI(3) = IROUND (UTRC(1))
         WINI(4) = IROUND (TRC(2))
C                                       Initial guess
         PARMS(1) = 0.0D0
         PARMS(2) = 0.0D0
         J = JCODE
         GCODE = 0
         DO 50 I1 = 1,NG
            PARMS(J+1) = GMAX(I1)
            PARMS(J+2) = GPOS(I1)
            PARMS(J+3) = GWIDTH(I1)
            IF (GWIDTH(I1).GT.0.01) GCODE = 1
            IF (PARMS(J+3).LE.0.0D0) PARMS(J+3) = 0.5
            J = J + 3
 50         CONTINUE
         IG = 3 * NG + JCODE
         DO 55 I1 = 1,IG
            UPARMS(I1) = PARMS(I1)
 55         CONTINUE
         CALL COPY (MAXPRM, DOCOMP, LLCOMP)
C                                       Setup for looping
C                                       Loop
         LIM1 = UTRC(1) - UBLC(1) + 1.01
         CALL FILL (7, 1, IPOS)
         IPOS(1) = UBLC(1) + 0.01
C                                       TV in good state
         CALL COPY (3, TVZOOM, LZOOM)
         TVZOOM(1) = 0
         TVZOOM(2) = MAXXTV(1) / 2
         TVZOOM(3) = MAXXTV(2) / 2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'TURNING OFF TV ZOOM'
            GO TO 990
            END IF
         IF (IPL(1).GT.0) THEN
            CALL YSLECT ('OFFF', IPL(1), 0, SCRTCH, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'TURNING OFF TV IMAGE DISPLAY'
               GO TO 990
               END IF
            END IF
         IF (IPL(2).GT.0) THEN
            CALL YSLECT ('OFFF', IPL(2), 0, SCRTCH, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'TURNING OFF TV IMAGE DISPLAY'
               GO TO 990
               END IF
            END IF
         END IF
C                                       do the list
      DO 300 I = 1,INLIST
         IX = PIXLIS(1,INLIST+1-I)
         IY = PIXLIS(2,INLIST+1-I)
         LGRNO = (IY-1) * NX + IX
         IXGRNO = LGRNO
         CALL TABXG ('READ', XGBUFF, IXGRNO, XGKOLS, XGNUMV, YZPOS,
     *      NGAU, VPEAK, RESULT, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ XG TABLE'
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
C                                       Init. files, first input.
            IF (OPER.EQ.'REDO') THEN
               IPOS(2) = YZPOS(1)
               IPOS(3) = YZPOS(2)
               CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3),
     *            BOTEMP, IRET)
               BOI = BOTEMP + 1
               WINI(2) = YZPOS(1)
               WINI(4) = YZPOS(1)
               CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF1,
     *            JBUFSZ, BOI, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'INIT READ INPUT IMAGE'
                  GO TO 990
                  END IF
C                                       Read.
               CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READING INPUT IMAGE'
                  GO TO 990
                  END IF
C                                       Copy to buffer.
               DO 105 I1 = 1,LIM1
                  DATA(I1) = BUFF1(IBIND+I1-1)
 105           CONTINUE
C                                       previous fit = a guess
               DO 110 I1 = 1,MAXPRM
                  UPARMS(I1) = RESULT(I1)
 110              CONTINUE
C                                       Call DO1FIT
               CALL DO1FIT (IPOS, UPARMS, PARMS, XPARMS, NGAU, RESULT,
     *            IRET)
               IF (IRET.EQ.99) THEN
                  MSGTXT = 'Quitting at user request'
                  CALL MSGWRT (5)
                  CALL ZCLOSE (LUNI, INDI, I1)
                  GO TO 999
               ELSE IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1180) IRET
                  GO TO 990
                  END IF
               DO 115 I1 = 1,MAXPRM
                  IF (RESULT(I1).NE.FBLANK) XPARMS(I1) =
     *               RESULT(I1)
 115              CONTINUE
            ELSE IF (OPER.EQ.'FLAG') THEN
               CALL RFILL (2*MAXPRM+2*MAXGAU, FBLANK, RESULT)
               NGAU = 0
               END IF
            IXGRNO = LGRNO
            CALL TABXG ('WRIT', XGBUFF, IXGRNO, XGKOLS, XGNUMV, YZPOS,
     *         NGAU, VPEAK, RESULT, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE XG TABLE'
               GO TO 990
               END IF
C                                       update the image value
            J = 0
            K = 2
            DO 120 I1 = 1,NG
               IMAGE(IX,IY,J+1) = RESULT(K+1)
               IMAGE(IX,IY,J+2) = RESULT(K+2)
               IMAGE(IX,IY,J+3) = RESULT(K+3)
               IMAGE(IX,IY,J+5) = RESULT(K+1+MAXPRM)
               IMAGE(IX,IY,J+6) = RESULT(K+2+MAXPRM)
               IMAGE(IX,IY,J+7) = RESULT(K+3+MAXPRM)
               IF ((RESULT(K+1).EQ.FBLANK) .OR. (RESULT(K+3).EQ.FBLANK))
     *            THEN
                  IMAGE(IX,IY,J+4) = FBLANK
                  IMAGE(IX,IY,J+8) = FBLANK
               ELSE
                  IMAGE(IX,IY,J+4) = FMULT * RESULT(K+1) * RESULT(K+3)
                  IF ((RESULT(K+1+MAXPRM).EQ.FBLANK) .OR.
     *               (RESULT(K+3+MAXPRM).EQ.FBLANK)) THEN
                     IMAGE(IX,IY,J+8) = FBLANK
                  ELSE
                     IMAGE(IX,IY,J+8) = FMULT * SQRT ((RESULT(K+1) *
     *                  RESULT(K+3+MAXPRM))** 2 + (RESULT(K+1+MAXPRM) *
     *                  RESULT(K+3))** 2)
                     END IF
                  END IF
               J = J + NMXIMG
               K = K + 3
 120           CONTINUE
            NLIST = NLIST - 1
            END IF
 300     CONTINUE
C                                       Close files
      IF (OPER.EQ.'REDO') THEN
         CALL ZCLOSE (LUNI, INDI, I)
         CALL COPY (3, LZOOM, TVZOOM)
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., I)
         IF (IPL(1).GT.0) CALL YSLECT ('ONNN', IPL(1), 0, SCRTCH, I)
         IF (IPL(2).GT.0) CALL YSLECT ('ONNN', IPL(2), 0, SCRTCH, I)
         DO 310 J = 1,7
            CALL YZERO (NGRAY+J, I)
 310        CONTINUE
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UPDLIS: ERROR',I3,' ON ',A)
 1180 FORMAT ('UPDLIS: DO1FIT ERROR',I3)
      END
      SUBROUTINE UPDALL (OPER, SNMIN, MAXRES, MAXOFF, MAXPK, MAXWID,
     *   MAXDWD, NX, NY, IMAGE, IRET)
C-----------------------------------------------------------------------
C   UPDALL flags or re-does fitting on all pixels matching test
C   conditions
C   Inputs:
C      OPER     C*4    'REDO', 'FLAG'
C      SNMIN    R(*)   Minimum amp S/N (< SNMIN => redo)
C      MAXRES   R      Max allowed residual (> MAXRES => redo)
C      MAXOFF   R(*)   Min/Max allowed offset from the center
C      MAXWID   R(*)   Min/Max allowed Gaussian width
C      MAXDWD   R(*)   Max error in Gaussian width
C      NX       I      Number X pixels in image
C      NY       I      Number Y pixels in image
C   In/out:
C      IMAGE    R(*)   Images of NP parameters - updated on output
C   Output:
C      IRET     I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   NX, NY, IRET
      REAL      SNMIN(*), MAXRES, MAXOFF(2,*), MAXPK(2,*), MAXWID(2,*),
     *   MAXDWD(*), IMAGE(NX,NY,*)
C
      INCLUDE 'XGAUS.INC'
      INCLUDE 'XGAUSD.INC'
      CHARACTER PHNAME*48
      INTEGER   IROUND, LUNI, NYI, NXI, WINI(4), BOI, J, I1, IPOS(7),
     *   BOTEMP, IBIND, INDI, LIM1, IG, NGAU, I, IY, IZ, INLIST, NVAR,
     *   YZPOS(2), LZOOM(3), MP, IIZ, IIY, NG, K, LXGRNO
      REAL      RESULT(2*MAXPRM+2*MAXGAU)
      DOUBLE PRECISION PARMS(MAXPRM), UPARMS(MAXPRM), XPARMS(MAXPRM)
      LOGICAL   T, F, DOREAD, DOIT
      DOUBLE PRECISION  FJAC(MAXPRM,MAXPRM), FVEC(NPLIM), HALFAC, FMULT,
     *   VALVAR(MAXPRM)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA HALFAC /2.77258872D0/
      DATA LUNI /16/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      FMULT = SQRT (PI / HALFAC)
C                                       display parms
      UCHAN = 0
      INLIST = NLIST
      DOREAD = (MAXRES.GT.0.0) .OR. (OPER.EQ.'REDO')
      NG = NGAUSS
      DOEVEN = .TRUE.
      CALL DFILL (MAXPRM, 0.0D0, XPARMS)
C                                       Open and init for read
      IF (DOREAD) THEN
         CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, PHNAME, IRET)
         CALL ZOPEN (LUNI, INDI, DISKIN, PHNAME, T, F, T, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE'
            GO TO 990
            END IF
C                                       Setup for I/O
         NXI = CATOLD(KINAX)
         NYI = CATOLD(KINAX+1)
         WINI(1) = IROUND (UBLC(1))
         WINI(2) = IROUND (UBLC(2))
         WINI(3) = IROUND (UTRC(1))
         WINI(4) = IROUND (UTRC(2))
C                                       Initial guess
         PARMS(1) = 0.0D0
         PARMS(2) = 0.0D0
         J = JCODE
         GCODE = 0
         DO 50 I1 = 1,NG
            PARMS(J+1) = GMAX(I1)
            PARMS(J+2) = GPOS(I1)
            PARMS(J+3) = GWIDTH(I1)
            IF (GWIDTH(I1).GT.0.01) GCODE = 1
            IF (PARMS(J+3).LE.0.0D0) PARMS(J+3) = 0.5
            J = J + 3
 50         CONTINUE
         IG = 3 * NG + JCODE
         DO 55 I1 = 1,IG
            UPARMS(I1) = PARMS(I1)
 55         CONTINUE
         CALL COPY (MAXPRM, DOCOMP, LLCOMP)
C                                       Setup for looping
C                                       Loop
         LIM1 = UTRC(1) - UBLC(1) + 1.01
         CALL FILL (7, 1, IPOS)
         IPOS(1) = UBLC(1) + 0.01
C                                       TV in good state
         CALL COPY (3, TVZOOM, LZOOM)
         TVZOOM(1) = 0
         TVZOOM(2) = MAXXTV(1) / 2
         TVZOOM(3) = MAXXTV(2) / 2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'TURNING OFF TV ZOOM'
            GO TO 990
            END IF
         IF (IPL(1).GT.0) THEN
            CALL YSLECT ('OFFF', IPL(1), 0, SCRTCH, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'TURNING OFF TV IMAGE DISPLAY'
               GO TO 990
               END IF
            END IF
         IF (IPL(2).GT.0) THEN
            CALL YSLECT ('OFFF', IPL(2), 0, SCRTCH, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'TURNING OFF TV IMAGE DISPLAY'
               GO TO 990
               END IF
            END IF
         END IF
C                                       do the list
      DO 200 IZ = LBLC(2),LTRC(2)
         DO 190 IY = LBLC(1),LTRC(1)
            IXGRNO = (IZ-IBLC(2)) * (ITRC(1)-IBLC(1)+1) + IY - IBLC(1) +
     *         1
            LXGRNO = IXGRNO
            CALL TABXG ('READ', XGBUFF, IXGRNO, XGKOLS, XGNUMV, YZPOS,
     *         NGAU, VPEAK, RESULT, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ XG TABLE'
               GO TO 990
            ELSE IF ((IRET.EQ.0) .AND. (NGAU.GT.0)) THEN
C                                       Init. files, first input.
               IF (DOREAD) THEN
                  IPOS(2) = YZPOS(1)
                  IPOS(3) = YZPOS(2)
                  CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3),
     *               BOTEMP, IRET)
                  BOI = BOTEMP + 1
                  WINI(2) = YZPOS(1)
                  WINI(4) = YZPOS(1)
                  CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF1,
     *               JBUFSZ, BOI, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'INIT READ INPUT IMAGE'
                     GO TO 990
                     END IF
C                                       Read.
                  CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READING INPUT IMAGE'
                     GO TO 990
                     END IF
C                                       Copy to buffer.
                  DO 110 I1 = 1,LIM1
                     DATA(I1) = BUFF1(IBIND+I1-1)
 110                 CONTINUE
                  END IF
C                                       do we do this one? check comps
               DOIT = .FALSE.
               J = 0
               DO 120 IG = 1,MAXGAU
                  J = J + 3
                  IF ((SNMIN(IG).GT.0) .AND. (RESULT(J).NE.FBLANK)) THEN
                     IF (ABS(RESULT(J)).LT.
     *                  ABS(SNMIN(IG)*RESULT(J+MAXPRM))) DOIT = .TRUE.
                     END IF
                  IF ((MAXPK(1,IG).LT.MAXPK(2,IG)) .AND.
     *               (RESULT(J).NE.FBLANK)) THEN
                     IF (RESULT(J).LT.MAXPK(1,IG)) DOIT = .TRUE.
                     IF (RESULT(J).GT.MAXPK(2,IG)) DOIT = .TRUE.
                     END IF
                  IF ((MAXOFF(1,IG).LT.MAXOFF(2,IG)) .AND.
     *               (RESULT(J+1).NE.FBLANK)) THEN
                     IF (ABS(RESULT(J+1)).LT.MAXOFF(1,IG)) DOIT = .TRUE.
                     IF (ABS(RESULT(J+1)).GT.MAXOFF(2,IG)) DOIT = .TRUE.
                     END IF
                  IF ((MAXWID(1,IG).LT.MAXWID(2,IG)) .AND.
     *               (RESULT(J+2).NE.FBLANK)) THEN
                     IF (RESULT(J+2).LT.MAXWID(1,IG)) DOIT = .TRUE.
                     IF (RESULT(J+2).GT.MAXWID(2,IG)) DOIT = .TRUE.
                     END IF
                  IF ((MAXDWD(IG).GT.0) .AND.
     *                  (RESULT(J+2+MAXPRM).NE.FBLANK)) THEN
                     IF (RESULT(J+2+MAXPRM).GT.MAXDWD(IG))
     *                  DOIT = .TRUE.
                     END IF
 120              CONTINUE
C                                       parameters
               DO 130 I = 1,MAXPRM
                  PARMS(I) = RESULT(I)
                  UPARMS(I) = RESULT(I)
 130              CONTINUE
C                                       check residuals
               IF ((.NOT.DOIT) .AND. (MAXRES.GT.0.0)) THEN
                  ITTER = 0
                  NITTER = MAX (XNIT, 100.0)
                  NVAR = JJC
                  K = JJC
                  IF (JJC.GT.0) VALVAR(1) = PARMS(1)
                  IF (JJC.EQ.2) VALVAR(2) = PARMS(2)
                  DO 135 I = 1,NG
                     DO 134 J = 1,3
                        K = K + 1
                        IF ((LLCOMP(K).GT.0) .AND. (PARMS(K).NE.FBLANK))
     *                     THEN
                           NVAR = NVAR + 1
                           IVAR(NVAR) = I
                           JVAR(NVAR) = J
                           VALVAR(NVAR) = PARMS(K)
                           END IF
 134                    CONTINUE
 135                 CONTINUE
                  MP = NVAR
                  I = 1
                  MVAR = K
                  CALL DPCOPY (MAXPRM, PARMS, LPARMS)
                  CALL AGFUNC (LIM1, MP, VALVAR, FVEC, FJAC, I)
                  DO 140 I1 = 1,LIM1
                     IF (BUFF1(IBIND+I1-1).NE.FBLANK) THEN
                        IF (ABS(FVEC(I1)).GT.MAXRES) DOIT = .TRUE.
                        END IF
 140                 CONTINUE
                  END IF
C                                       Call DO1FIT
               IF (DOIT) THEN
                  IF (OPER.EQ.'REDO') THEN
                     CALL DO1FIT (IPOS, UPARMS, PARMS, XPARMS, NGAU,
     *                  RESULT, IRET)
                     IF (IRET.EQ.99) THEN
                        MSGTXT = 'Quitting at user request'
                        CALL MSGWRT (5)
                        CALL ZCLOSE (LUNI, INDI, I1)
                        GO TO 999
                     ELSE IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1180) IRET
                        GO TO 990
                        END IF
                     DO 145 I1 = 1,MAXPRM
                        IF (RESULT(I1).NE.FBLANK) XPARMS(I1) =
     *                     RESULT(I1)
 145                    CONTINUE
                  ELSE IF (OPER.EQ.'FLAG') THEN
                     CALL RFILL (2*MAXPRM+2*MAXGAU, FBLANK, RESULT)
                     NGAU = 0
                     END IF
                  IXGRNO = LXGRNO
                  CALL TABXG ('WRIT', XGBUFF, IXGRNO, XGKOLS, XGNUMV,
     *               YZPOS, NGAU, VPEAK, RESULT, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITE XG TABLE'
                     GO TO 990
                     END IF
C                                       update the image value
                  IIY = IY - IBLC(1) + 1
                  IIZ = IZ - IBLC(2) + 1
                  J = 0
                  K = 2
                  DO 150 I1 = 1,NG
                     IMAGE(IIY,IIZ,J+1) = RESULT(K+1)
                     IMAGE(IIY,IIZ,J+2) = RESULT(K+2)
                     IMAGE(IIY,IIZ,J+3) = RESULT(K+3)
                     IMAGE(IIY,IIZ,J+5) = RESULT(K+1+MAXPRM)
                     IMAGE(IIY,IIZ,J+6) = RESULT(K+2+MAXPRM)
                     IMAGE(IIY,IIZ,J+7) = RESULT(K+3+MAXPRM)
                     IF ((RESULT(K+1).EQ.FBLANK) .OR.
     *                  (RESULT(K+3).EQ.FBLANK)) THEN
                        IMAGE(IIY,IIZ,J+4) = FBLANK
                        IMAGE(IIY,IIZ,J+8) = FBLANK
                     ELSE
                        IMAGE(IIY,IIZ,J+4) = RESULT(K+1) * RESULT(K+3)
     *                     * FMULT
                        IF ((RESULT(K+1+MAXPRM).EQ.FBLANK) .OR.
     *                     (RESULT(K+3+MAXPRM).EQ.FBLANK)) THEN
                           IMAGE(IIY,IIZ,J+8) = FBLANK
                        ELSE
                           IMAGE(IIY,IIZ,J+8) = SQRT ((RESULT(K+1) *
     *                        RESULT(K+3+MAXPRM))** 2 + (RESULT(K+3) *
     *                        RESULT(K+1+MAXPRM))** 2) * FMULT
                           END IF
                        END IF
                     J = J + NMXIMG
                     K = K + 3
 150                 CONTINUE
                  END IF
               END IF
 190        CONTINUE
 200     CONTINUE
C                                       Close files
      IF (DOREAD) CALL ZCLOSE (LUNI, INDI, I)
      IF (OPER.EQ.'REDO') THEN
         CALL COPY (3, LZOOM, TVZOOM)
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., I)
         IF (IPL(1).GT.0) CALL YSLECT ('ONNN', IPL(1), 0, SCRTCH, I)
         IF (IPL(2).GT.0) CALL YSLECT ('ONNN', IPL(2), 0, SCRTCH, I)
         DO 310 J = 1,7
            CALL YZERO (NGRAY+J, I)
 310        CONTINUE
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UPDALL: ERROR',I3,' ON ',A)
 1180 FORMAT ('UPDALL: DO1FIT ERROR',I3)
      END
      SUBROUTINE XGAUOU (IRET)
C-----------------------------------------------------------------------
C   XGAUOU creates the output residual image and the parameter images
C   and then computes the residual map (if any) and fills (via PSCALE)
C   the individual Gaussian parameter images.  It calls XGAUHI for
C   history info for all images.
C   Output:
C      IRET    I   0 => ok,  4 => real trouble.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER SEQTYP*6
      INTEGER   NG, NP, NXO, NYO, WINO(4), IP, NCN, IG, IOFF
      LOGICAL   DORES, DOPARM
      INCLUDE 'XGAUS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      DOPARM = DOCAT.GE.2
      DORES  = MOD(DOCAT-1,2).EQ.0
      NCN = 1
      IF (DORES) NCN = 2
C                                       create output images
      CALL XGAUCR (DORES, DOPARM, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATING OUTPUT FILES'
         GO TO 990
         END IF
      IF (DORES) THEN
         CALL XGAURE (IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING THE RESIDUAL IMAGE'
            GO TO 990
            END IF
         CALL XGAUHI (IRET, NCN)
         END IF
C                                       loop limits etc.
      IF (DOPARM) THEN
         NG = NGAUSS
         NP = 3 * NG + JCODE
         WINO(1) = 1
         WINO(2) = 1
C                                       Output Gaussian parms
         DO 30 IG = 1,NG
            DO 20 IP = 1,6
               IOFF = MOD (IP-1, 3) + 1 + JCODE + 3 * (IG-1)
               IF (DOCOMP(IOFF).GT.0) THEN
                  IOFF = IOFF + 2 - JCODE
                  NCN = NCN + 1
                  NEWCNO = FCNO(NCN)
                  DISKO = FVOL(NCN)
                  CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'REST',
     *               SCRTCH, IRET)
                  IF ((IRET.NE.0) .AND. (IRET.NE.6)) THEN
                     WRITE (MSGTXT,1005) IRET, NCN
                     GO TO 990
                     END IF
                  CALL H2CHR (6, KHIMCO, CATH(KHIMC), SEQTYP)
                  WRITE (MSGTXT,1010) SEQTYP
                  CALL MSGWRT (1)
                  SEQOUT = CATBLK(KIIMS)
                  CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMOUT)
                  CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLAOUT)
                  NXO = CATBLK(KINAX)
                  NYO = CATBLK(KINAX+1)
                  WINO(3) = NXO
                  WINO(4) = NYO
C                                       Fill image
                  IF (IP.GE.4) IOFF = IOFF + MAXPRM
                  CALL PSCALE (IOFF, WINO, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1011) IRET, SEQTYP
                     GO TO 990
                     END IF
C                                       History, close
                  CALL XGAUHI (IOFF, NCN)
                  END IF
 20            CONTINUE
 30         CONTINUE
C                                       Baseline parms
         IF (JCODE.GT.0) THEN
            DO 50 IP = 1,4
               IOFF = MOD (IP-1, 2) + 1
               IF (IOFF.LE.JCODE) THEN
                  NCN = NCN + 1
                  NEWCNO = FCNO(NCN)
                  DISKO = FVOL(NCN)
                  CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'REST',
     *               SCRTCH, IRET)
                  IF ((IRET.NE.0) .AND. (IRET.NE.6)) THEN
                     WRITE (MSGTXT,1005) IRET, NCN
                     GO TO 990
                     END IF
                  CALL H2CHR (6, KHIMCO, CATH(KHIMC), SEQTYP)
                  WRITE (MSGTXT,1010) SEQTYP
                  CALL MSGWRT (1)
                  SEQOUT = CATBLK(KIIMS)
                  CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMOUT(1:12))
                  CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLAOUT(1:6))
                  NXO = CATBLK(KINAX)
                  NYO = CATBLK(KINAX+1)
                  WINO(3) = NXO
                  WINO(4) = NYO
C                                       Fill image
                  IF (IP.GE.3) IOFF = IOFF + MAXPRM
                  CALL PSCALE (IOFF, WINO, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1011) IRET, SEQTYP
                     GO TO 990
                     END IF
C                                       History, close
                  CALL XGAUHI (IOFF, NCN)
                  END IF
 50            CONTINUE
            END IF
C                                       Flux maps
         DO 80 IG = 1,NG
            DO 70 IP = 1,2
               IOFF = JCODE + 3 * (IG - 1) + 1
               IF ((DOCOMP(IOFF).GT.0) .AND. (DOCOMP(IOFF+2).GT.0)) THEN
                  NCN = NCN + 1
                  NEWCNO = FCNO(NCN)
                  DISKO = FVOL(NCN)
                  CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'REST',
     *               SCRTCH, IRET)
                  IF ((IRET.NE.0) .AND. (IRET.NE.6)) THEN
                     WRITE (MSGTXT,1005) IRET, NCN
                     GO TO 990
                     END IF
                  CALL H2CHR (6, KHIMCO, CATH(KHIMC), SEQTYP)
                  WRITE (MSGTXT,1010) SEQTYP
                  CALL MSGWRT (1)
                  SEQOUT = CATBLK(KIIMS)
                  CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMOUT(1:12))
                  CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLAOUT(1:6))
                  NXO = CATBLK(KINAX)
                  NYO = CATBLK(KINAX+1)
                  WINO(3) = NXO
                  WINO(4) = NYO
C                                       Fill image
                  IOFF = 2*MAXPRM + 2 * (IG - 1) + IP
                  CALL PSCALE (IOFF, WINO, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1011) IRET, SEQTYP
                     GO TO 990
                     END IF
C                                       History, close
                  CALL XGAUHI (IOFF, NCN)
                  END IF
 70            CONTINUE
 80         CONTINUE
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IRET = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XGAUOU: ERROR',I4,' ON ',A)
 1005 FORMAT ('ERROR',I5,' RECOVERING FILE HEADER NUMBER',I5)
 1010 FORMAT ('Begin writing file of type ',A)
 1011 FORMAT ('ERROR',I5,' MOVING DATA TO FILE TYPE ',A4,A2)
      END
      SUBROUTINE XGAUCR (DORES, DOPARM, IRET)
C-----------------------------------------------------------------------
C   XGAUCR creates the output files.
C   Inputs:
C      DORES    L   Create residual?
C      DOPARM   L   Create parameter images?
C   Output:
C      IRET     I   Error code: 0 => ok
C                     4 => user routine detected error.
C                     5 => catalog troubles
C                     8 => can't start
C      /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      LOGICAL   DORES, DOPARM
      INTEGER   IRET
C
      CHARACTER BLANK*6, CTYPE(19)*4, CUNITS(9)*8, BUNITS(9)*4,
     *   SEQTYP(6)*8, BASTYP(4)*8, FLXTYP(2)*8, ASTS*4, OTYPE*8,
     *   CTEMP*8
      INTEGER   IERR, NG, NAX, I, ITYP, NTYP, IG, IOFF, IP, INPSEQ, J,
     *   JTRIM
      REAL      XBLC(7), XTRC(7)
      INCLUDE 'XGAUS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA BLANK /'      '/
      DATA NTYP, CTYPE /19, 'TIME','FREQ','LAMB','VELO','FELO','    ',
     *   'PIXE','DIST','ANGL','RA  ','RA--','LL  ','DEC ','DEC-',
     *   'MM  ','GLON','GLAT','ELON','ELAT'/
      DATA CUNITS /'SECONDS ', 'HERTZ   ', 'METERS  ',
     *   'METR/SEC', 'METR/SEC', 'PIXELS  ', 'PIXELS  ',
     *   'DEGREES ', 'UNKNOWN?'/
      DATA BUNITS /'/SEC', '/HZ ', '/M  ', '/M/S', '/M/S', '/PIX',
     *   '/PIX', '/DEG', '/UNK'/
      DATA SEQTYP /'AMPL    ', 'CENTR   ', 'WIDTH   ',
     *             'DAMPL   ', 'DCENT   ', 'DWIDT   '/
      DATA BASTYP /'CONST   ', 'SLOPE   ',
     *             'DCONST  ', 'DSLOPE  '/
      DATA FLXTYP /'FLUX    ', 'DFLUX   '/
      DATA ASTS /'****'/
C-----------------------------------------------------------------------
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD, CATBLK)
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
      INPSEQ = SEQOUT
C                                       Get user modification to CATBLK
      CALL RCOPY (7, BLC, XBLC)
      CALL RCOPY (7, TRC, XTRC)
      XBLC(2) = IBLC(1)
      XBLC(3) = IBLC(2)
      XTRC(2) = ITRC(1)
      XTRC(3) = ITRC(2)
      CALL SUBHD3 (XBLC, XTRC, 1.0, 1.0, 1.0)
C                                       Create output file for residual
      NEWCNO = 0
      IRET = 4
      IF (DORES) THEN
         NEWCNO = 1
         CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO
         FCNO(NCFILE) = NEWCNO
         FRW(NCFILE) = 2
         END IF
C                                       save the residual header
      CALL COPY (256, CATBLK, BUFF2)
C                                       Make names, classes, disks OK.
      IF (DOPARM) THEN
         SEQOUT = CATBLK(KIIMS)
C                                       Basic output header: results
         CATBLK(KIDIM) = CATBLK(KIDIM) - 1
         NAX = CATBLK(KIDIM)
         DO 80 I = 1,NAX
            CATBLK(KINAX+I-1) = CATBLK(KINAX+I)
            CATR(KRCRP+I-1) = CATR(KRCRP+I)
            CATR(KRCRT+I-1) = CATR(KRCRT+I)
            CATR(KRCIC+I-1) = CATR(KRCIC+I)
            CATD(KDCRV+I-1) = CATD(KDCRV+I)
            CALL CHCOPY (8, 1, CATH(KHCTP+I*2), 1,
     *         CATH(KHCTP+(I-1)*2))
 80         CONTINUE
         DO 85 I = NAX,6
            CATBLK(KINAX+I) = 1
 85         CONTINUE
C                                       Find type of old axis
         CALL H2CHR (4, 1, OLDH(KHCTP), OTYPE)
         DO 90 ITYP = 1,NTYP
            IF (OTYPE.EQ.CTYPE(ITYP)) GO TO 100
 90         CONTINUE
         ITYP = 0
         WRITE (MSGTXT,1090) OTYPE
         CALL MSGWRT (6)
 100     IF (ITYP.GT.7) ITYP = 8
         IF (ITYP.EQ.0) ITYP = 9
C                                       loop limits etc.
         NG = NGAUSS
C                                       Output Gaussian parms
         DO 120 IG = 1,NG
            DO 110 IP = 1,6
               IOFF = MOD (IP-1, 3) + 1 + JCODE + 3 * (IG-1)
               IF (DOCOMP(IOFF).GT.0) THEN
                  CTEMP = SEQTYP(IP)
                  J = JTRIM (CTEMP)
                  WRITE (CTEMP(J+1:),1100) IG
                  CALL CHR2H (6, CTEMP, KHIMCO, CATH(KHIMC))
                  IF ((IP.EQ.1) .OR. (IP.EQ.4)) THEN
                     CALL CHR2H (8, 'OptDepth', 1, CATH(KHBUN))
                  ELSE
                     CALL CHR2H (8, CUNITS(ITYP), 1, CATH(KHBUN))
                     END IF
C                                       Create
                  DISKO = XDISKO + 0.01
                  NEWCNO = 1
                  CATBLK(KIIMS) = INPSEQ
                  CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1120) IERR, CTEMP
                     GO TO 990
                     END IF
C                                       Record the creation
                  NCFILE = NCFILE + 1
                  FVOL(NCFILE) = DISKO
                  FCNO(NCFILE) = NEWCNO
                  FRW(NCFILE) = 2
                  END IF
 110           CONTINUE
 120        CONTINUE
C                                       Baseline parms
         IF (JCODE.GT.0) THEN
            DO 140 IP = 1,4
               IOFF = MOD (IP-1, 2) + 1
               IF (IOFF.LE.JCODE) THEN
                  CALL CHR2H (6, BASTYP(IP), KHIMCO, CATH(KHIMC))
                  CALL CHCOPY (8, 1, OLDH(KHBUN), 1, CATH(KHBUN))
                  IF ((IP.EQ.2) .OR. (IP.EQ.4)) CALL CHR2H (4,
     *               BUNITS(ITYP), 5, CATH(KHBUN))
C                                       Create
                  DISKO = XDISKO + 0.01
                  NEWCNO = 1
                  CATBLK(KIIMS) = INPSEQ
                  CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1120) IERR, BASTYP(IP)
                     GO TO 990
                     END IF
C                                       Record the creation
                  NCFILE = NCFILE + 1
                  FVOL(NCFILE) = DISKO
                  FCNO(NCFILE) = NEWCNO
                  FRW(NCFILE) = 2
                  END IF
 140           CONTINUE
            END IF
C                                       Flux maps
         DO 170 IG = 1,NG
            DO 160 IP = 1,2
               IOFF = JCODE + 3 * (IG - 1) + 1
               IF ((DOCOMP(IOFF).GT.0) .AND. (DOCOMP(IOFF+2).GT.0)) THEN
                  CTEMP = FLXTYP(IP)
                  J = JTRIM (CTEMP)
                  WRITE (CTEMP(J+1:),1100) IG
                  CALL CHR2H (6, CTEMP, KHIMCO, CATH(KHIMC))
                  CALL CHR2H (4, 'OptD', 1, CATH(KHBUN))
                  CALL CHR2H (4, BUNITS(ITYP), 5, CATH(KHBUN))
                  CALL CHR2H (1, ASTS(1:1), 5, CATH(KHBUN))
C                                       Create
                  DISKO = XDISKO + 0.01
                  NEWCNO = 1
                  CATBLK(KIIMS) = INPSEQ
                  CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1120) IERR, CTEMP
                     GO TO 990
                     END IF
C                                       Record the creation
                  NCFILE = NCFILE + 1
                  FVOL(NCFILE) = DISKO
                  FCNO(NCFILE) = NEWCNO
                  FRW(NCFILE) = 2
                  END IF
 160           CONTINUE
 170        CONTINUE
         END IF
      IRET = 0
      CALL COPY (256, BUFF2, CATBLK)
      DISKO = FVOL(2)
      NEWCNO = FCNO(2)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT RESIDUALS FILE')
 1090 FORMAT ('AXIS TYPE ',A8,' DOES NOT HAVE KNOWN UNITS')
 1100 FORMAT (I1)
 1120 FORMAT ('ERROR',I5,' CREATING FILE TYPE ',A)
      END
      SUBROUTINE XGAURE (IRET)
C-----------------------------------------------------------------------
C   XGAURE reads the input data and the table to compute and write a
C   residual image.
C   Output:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'XGAUS.INC'
      INCLUDE 'XGAUSD.INC'
      INTEGER  LIM1, LIM2, LIM3, NXI, NYI, WINI(4), NXO, NYO, WINO(4),
     *   IPOS(7), BOI, IBIND, OBIND, LUNI, INDI, LUNO, INDO, NGAU,
     *   IROUND, I1, I2, I3, I, J, K, XYPOS(2), NVAR
      REAL      RMIN, RMAX, RESULT(MAXPRM,2)
      CHARACTER PHNAME*48
      LOGICAL   BLNKD
      DOUBLE PRECISION PARMS(MAXPRM), FJAC(MAXPRM,MAXPRM), FVEC(NPLIM),
     *   VALVAR(MAXPRM)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'Begin writing residual image cube'
      CALL MSGWRT (2)
      RMIN = 1.E15
      RMAX = -1.E15
C                                       Open and init for read
      LUNI = 33
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, PHNAME, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, PHNAME, .TRUE., .FALSE., .TRUE.,
     *   IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT IMAGE'
         GO TO 990
         END IF
      NXI = CATOLD(KINAX)
      NYI = CATOLD(KINAX+1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IBLC(1)
      WINI(3) = IROUND (TRC(1))
      WINI(4) = ITRC(1)
      LIM3 = ITRC(2) - IBLC(2) + 1.01
      LIM2 = ITRC(1) - IBLC(1) + 1.01
      LIM1 = TRC(1) - BLC(1) + 1.01
      CALL FILL (7, 1, IPOS)
C                                       Open and init for write
      LUNO = 34
      CALL ZPHFIL ('MA', FVOL(2), FCNO(2), 1, PHNAME, IRET)
      CALL ZOPEN (LUNO, INDO, FVOL(2), PHNAME, .TRUE., .FALSE., .TRUE.,
     *   IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT IMAGE'
         GO TO 990
         END IF
      NXO = CATBLK(KINAX)
      NYO = CATBLK(KINAX+1)
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
C                                       table read
      IXGRNO = 1
      BLNKD = .FALSE.
      CALL COPY (MAXPRM, DOCOMP, LLCOMP)
      DO 100 I3 = 1,LIM3
C                                       input
         IPOS(3) = IBLC(2) + I3 - 0.9
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3), BOI, IRET)
         BOI = BOI + 1
         CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF1, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT READ OF INPUT IMAGE'
            GO TO 990
            END IF
C                                       output
         IPOS(3) = I3
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3), BOI, IRET)
         BOI = BOI + 1
         CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT READ OF INPUT IMAGE'
            GO TO 990
            END IF
         DO 90 I2 = 1,LIM2
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ INPUT IMAGE'
               GO TO 990
               END IF
            CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE RESIDUAL IMAGE'
               GO TO 990
               END IF
            CALL TABXG ('READ', XGBUFF, IXGRNO, XGKOLS, XGNUMV,
     *         XYPOS, NGAU, VPEAK, RESULT, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE NEW XG TABLE'
               GO TO 990
               END IF
C                                       Copy to buffer.
            DO 20 I1 = 1,LIM1
               DATA(I1) = BUFF1(IBIND+I1-1)
 20            CONTINUE
C                                       parameters
            CALL DFILL (MAXPRM, 0.0D0, PARMS)
            IF ((JCODE.GT.0) .AND. (RESULT(1,1).NE.FBLANK)) PARMS(1) =
     *         RESULT(1,1)
            IF ((JCODE.EQ.2) .AND. (RESULT(2,1).NE.FBLANK)) PARMS(2) =
     *         RESULT(2,1)
            I1 = JCODE + 1
            J = 3
            DO 30 I = 1,NGAU
               IF (RESULT(J,1).NE.FBLANK) PARMS(I1) = RESULT(J,1)
               IF (RESULT(J+1,1).NE.FBLANK) PARMS(I1+1) = RESULT(J+1,1)
               IF (RESULT(J+2,1).NE.FBLANK) PARMS(I1+2) = RESULT(J+2,1)
               I1 = I1 + 3
               J = J + 3
 30            CONTINUE
            I1 = I1 - 1
            ITTER = 0
            NITTER = MAX (XNIT, 100.0)
            NVAR = JJC
            K = JJC
            IF (JJC.GT.0) VALVAR(1) = PARMS(1)
            IF (JJC.EQ.2) VALVAR(2) = PARMS(2)
            DO 39 I = 1,NGAU
               DO 38 J = 1,3
                  K = K + 1
                  IF ((LLCOMP(K).GT.0) .AND. (PARMS(K).NE.FBLANK)) THEN
                     NVAR = NVAR + 1
                     IVAR(NVAR) = I
                     JVAR(NVAR) = J
                     VALVAR(NVAR) = PARMS(K)
                     END IF
 38               CONTINUE
 39            CONTINUE
            I1 = NVAR
            I = 1
            MVAR = K
            CALL DPCOPY (MAXPRM, PARMS, LPARMS)
            CALL AGFUNC (LIM1, I1, VALVAR, FVEC, FJAC, I)
            DO 40 I1 = 1,LIM1
               IF (BUFF1(IBIND+I1-1).EQ.FBLANK) THEN
                  BUFF2(OBIND+I1-1) = FBLANK
                  BLNKD = .TRUE.
               ELSE
                  BUFF2(OBIND+I1-1) = FVEC(I1)
                  IF (FVEC(I1).GT.RMAX) RMAX = FVEC(I1)
                  IF (FVEC(I1).LT.RMIN) RMIN = FVEC(I1)
                  END IF
 40            CONTINUE
 90         CONTINUE
         CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'FINISH RESIDUAL IMAGE'
            GO TO 990
            END IF
 100     CONTINUE
      CATR(KRDMX) = RMAX
      CATR(KRDMN) = RMIN
      IF (BLNKD) THEN
         CATR(KRBLK) = FBLANK
      ELSE
         CATR(KRBLK) = 0.0
         END IF
C
 990  IF (IRET.GT.0) CALL MSGWRT (8)
      IF (INDI.GT.0) CALL ZCLOSE (LUNI, INDI, I)
      IF (INDO.GT.0) CALL ZCLOSE (LUNO, INDO, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XGAURE: ERROR',I4,' ON ',A)
      END
      SUBROUTINE PSCALE (IOFF, WINO, IRET)
C-----------------------------------------------------------------------
C   PSCALE reads a map file extracting one point per row
C   and writes an image out.
C   Inputs:
C      IOFF     I       Pixel in row to extract (1-rel)
C      WINO     I(4)    Output Window
C      JBUFSZ   I       Buffer size in bytes
C   Output:
C      IRET     I       0 -> ok, else IO error
C      CATBLK in common: change max/min and scaling and blanking
C      Buffers in common
C-----------------------------------------------------------------------
      INTEGER   IOFF, WINO(4), IRET
C
      INCLUDE 'XGAUS.INC'
      CHARACTER PHNAME*48
      LOGICAL   BLNKD, T
      REAL      PMIN, PMAX, HFAC, RESULT(2*MAXPRM)
      INTEGER   NXO, NYO, I2, LUNO, INDO, IPOS(7), BOTEMP, OBIND, L,
     *   JERR, JOFF, KOFF, NGAU, I1
      DOUBLE PRECISION POFF, PMULT
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA T /.TRUE./
      DATA LUNO /17/
      DATA HFAC /1.064467/
C-----------------------------------------------------------------------
C                                       loop limits
      NXO = WINO(3)
      NYO = WINO(4)
C                                       Open files
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, PHNAME, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, PHNAME, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT PARAMETER IMAGE'
         GO TO 990
         END IF
C                                       loop
      CALL FILL (7, 1, IPOS)
      IXGRNO = 1
      BLNKD = .FALSE.
      PMIN = 1.E15
      PMAX = -PMIN
      POFF = 0.0D0
      PMULT = 1.0D0
      IF (REFINC.EQ.0.0) REFINC = 1.0
      IF (IOFF.GT.2*MAXPRM) THEN
         PMULT = HFAC * ABS (REFINC)
         JOFF = MOD (IOFF-2*MAXPRM-1, 2) + 1
         KOFF = ((IOFF - 2*MAXPRM - 1) / 2 + 1) * 3
      ELSE
         I2 = MOD (IOFF-1, MAXPRM) + 1
         IF (I2.EQ.2) THEN
            PMULT = 1.0D0 / REFINC
         ELSE IF (I2.GT.2) THEN
            I1 = MOD (I2-3,3) + 1
            IF (I1.EQ.3) THEN
               PMULT = ABS (REFINC)
            ELSE IF (I1.EQ.2) THEN
               IF (IOFF.LE.MAXPRM) THEN
                  POFF = REFVAL
                  PMULT = REFINC
               ELSE
                  PMULT = ABS (REFINC)
                  END IF
               END IF
            END IF
         END IF
C                                       Init output
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3), BOTEMP, IRET)
      BOTEMP = BOTEMP + 1
      CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2, JBUFSZ,
     *   BOTEMP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT I/O TO PARAMETER IMAGE'
         GO TO 990
         END IF
C                                       Init a write
      DO 100 I2 = 1,NYO
         CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING PARAMETER IMAGE'
            GO TO 990
            END IF
C                                       Loop thru input plane
         L = OBIND - 1
         DO 90 I1 = 1,NXO
            CALL TABXG ('READ', XGBUFF, IXGRNO, XGKOLS, XGNUMV, IPOS(2),
     *         NGAU, VPEAK, RESULT, IRET)
            L = L + 1
            IF (IOFF.GT.2*MAXPRM) THEN
               IF (RESULT(KOFF).EQ.FBLANK) THEN
                  BUFF2(L) = FBLANK
                  BLNKD = .TRUE.
               ELSE
                  IF (JOFF.EQ.1) THEN
                     BUFF2(L) = PMULT * RESULT(KOFF) * RESULT(KOFF+2)
                  ELSE
                     BUFF2(L) = PMULT * SQRT ((RESULT(KOFF+2) *
     *                  RESULT(KOFF+MAXPRM))**2 + (RESULT(KOFF) *
     *                  RESULT(KOFF+MAXPRM+2))**2)
                     END IF
                  PMIN = MIN (PMIN, BUFF2(L))
                  PMAX = MAX (PMAX, BUFF2(L))
                  END IF
            ELSE
               IF (RESULT(IOFF).EQ.FBLANK) THEN
                  BUFF2(L) = FBLANK
                  BLNKD = .TRUE.
               ELSE
                  BUFF2(L) = RESULT(IOFF) * PMULT + POFF
                  PMIN = MIN (PMIN, BUFF2(L))
                  PMAX = MAX (PMAX, BUFF2(L))
                  END IF
               END IF
 90         CONTINUE
 100     CONTINUE
C                                       Flush output plane
      CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINISHING PARAMETER IMAGE'
         GO TO 990
         END IF
C                                       Set maxima, clear blanking
      CATR(KRDMX) = PMAX
      CATR(KRDMN) = PMIN
      CATR(KRBLK) = 0.0
C                                       Close down (error)
 990  IF (IRET.GT.0) CALL MSGWRT (8)
C                                       Close files
      IF (INDO.GT.0) CALL ZCLOSE (LUNO, INDO, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PSCALE: ERROR',I4,' ON ',A)
      END
      SUBROUTINE XGAUHI (ITYP, NCN)
C-----------------------------------------------------------------------
C   XGAUHI copies and updates history file.
C   Inputs:
C      ITYP   I   Output map type: 0 => residual
C                    1 => answers (get 1st axis info also)
C      NCN    I   Position in FILES common on catlgd file
C-----------------------------------------------------------------------
      INTEGER   ITYP, NCN
C
      CHARACTER HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IERR, IG, KBLC(7), KTRC(7), I
      LOGICAL   T
      INCLUDE 'XGAUS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       partial header keyword copy
      CALL KEYPCP (DISKIN, OLDCNO, DISKO, NEWCNO, 0, ' ', IERR)
C                                        Copy only the relevant table
      CALL TABCOP ('XG', XGVERS, XGVERS, LUN1, LUN2, DISKIN, DISKO,
     *   OLDCNO, NEWCNO, CATBLK, SCRTCH, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING GC TABLE'
         CALL MSGWRT (6)
         END IF
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   SCRTCH, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 50
         END IF
      DO 10 I = 1,7
         KBLC(I) = BLC(I) + 0.1
         KTRC(I) = TRC(I) + 0.1
 10      CONTINUE
      KBLC(2) = IBLC(1)
      KBLC(3) = IBLC(2)
      KTRC(2) = ITRC(1)
      KTRC(3) = ITRC(2)
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 50
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       BLC
      WRITE (HILINE,2000) TSKNAM, KBLC
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       TRC
      WRITE (HILINE,2001) TSKNAM, KTRC
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       other Parms
      WRITE (HILINE,2002) TSKNAM, IYINC, IZINC
      IF ((IYINC.GE.2) .OR. (IZINC.GE.2)) THEN
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         END IF
      IF (IERR.NE.0) GO TO 50
      CALL H2CHR (8, 1, OLDH(KHBUN), LABEL)
      WRITE (HILINE,2003) TSKNAM, FCUT, LABEL
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
      WRITE (HILINE,2004) TSKNAM, ORDER
      IF (ORDER.GE.0.0) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Gaussian guesses
      IG = NGAUSS
      WRITE (HILINE,2005) TSKNAM, IG
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Old axis 1
      IF (ITYP.GT.0) THEN
         WRITE (HILINE,2020) TSKNAM, REFTYP
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2021) TSKNAM, CATOLD(KINAX)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2022) TSKNAM, REFPIX
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2023) TSKNAM, REFINC
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2024) TSKNAM, REFVAL
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         END IF
C                                       Close HI file
 50   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Update CATBLK and close
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'CLWR', SCRTCH, IERR)
      FRW(NCN) = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XGAUHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BLC =',6(I6,','),I6)
 2001 FORMAT (A6,'TRC =',6(I6,','),I6)
 2002 FORMAT (A6,'YINC =',I5,'  ZINC =',I5)
 2003 FORMAT (A6,'FLUX =',1PE12.4,14X,'/ Flux cutoff in ',A)
 2004 FORMAT (A6,'ORDER =',F5.0,6X,'/ order of baseline fit')
 2005 FORMAT (A6,'NGAUSS =',I2,22X,'/ # Gaussians fit')
 2020 FORMAT (A6,'CTYPE1  = ''',A8,'''',12X,'/ Old axis 1')
 2021 FORMAT (A6,'NAXIS1  = ',I6,16X,'/ Old axis 1')
 2022 FORMAT (A6,'CRPIX1  = ',F9.3,13X,'/ Old axis 1')
 2023 FORMAT (A6,'CDELT1  = ',1PE13.5,9X,'/ Old axis 1')
 2024 FORMAT (A6,'CRVAL1  = ',1PE18.10,4X,'/ Old axis 1')
      END
      SUBROUTINE SUBHD3 (BLC, TRC, XINC, YINC, ZINC)
C-----------------------------------------------------------------------
C   SUBHD3 corrects the header for subimaging: changes number of points
C   on the axes, the reference pixels, and the alternate axis (freq vs
C   velocity) reference pixel.  It corrects the first 3 axes for use
C   of pixel increments - namely the number of pixels, the reference
C   pixel and the axis increment.
C   Inputs:
C      BLC    R(7)   Bottom left corner to use
C      TRC    R(7)   Top right corner to use
C      XINC   R      Pixel increment on first axis
C      YINC   R      Pixel increment on second axis
C      ZINC   R      Pxel increment on third axis
C   Common /MAPHDR/ CATBLK     map header (in/out)
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7), XINC, YINC, ZINC
C
      CHARACTER FCHARS(3)*4, CHTM12*12
      REAL      AINC(7)
      INTEGER   IPL, IPH, NAX, I, J
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
C                                       Regular axis parameters
      NAX = CATBLK(KIDIM)
      CALL RFILL (7, 1.0, AINC)
      IF (XINC.GT.0.0) AINC(1) = XINC
      IF (YINC.GT.0.0) AINC(2) = YINC
      IF (ZINC.GT.0.0) AINC(3) = ZINC
      DO 10 I = 1,NAX
         IPL = BLC(I) + 0.01
         IPH = TRC(I) + 0.01
         CATBLK(KINAX+I-1) = (IPH - IPL) / AINC(I) + 1
         CATR(KRCRP+I-1) = (CATR(KRCRP+I-1) - IPL) / AINC(I) + 1.
         CATR(KRCIC+I-1) = CATR(KRCIC+I-1) * AINC(I)
 10      CONTINUE
C                                       Alternate axis
      IF (CATBLK(KIALT).NE.0) THEN
         DO 25 I = 1,NAX
            IPL = KHCTP + (I-1)*2
            DO 20 J = 1,3
               CALL H2CHR (4, 1, CATH(IPL), CHTM12)
C                                       Found one
               IF (FCHARS(J)(1:4).EQ.CHTM12(1:4)) THEN
                  IPL = BLC(I) + 0.01
                  CATR(KRARP) = (CATR(KRARP) - IPL) / AINC(I) + 1.0
                  GO TO 999
                  END IF
 20            CONTINUE
 25         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE XGPOLY (MINC, MPOLY, IG, NPY, NV, XV, YV, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   GRPOLY uses a graphics plane to let the user develop a set of
C   closed polygons as a "blotch" region.  Handles pixel replicated
C   images
C   Inputs:
C      MPOLY   I          Dimension of NV, XV/YV 10 times this
C      IG      I          graphics plane to use
C   Output:
C      NPY     I          Number of polygons set
C      NV      I(MPOLY)   Number of vertices in each polygon
C      XV      I(*)       X-position of vertices in image
C      YV      I(*)       Y-position of vertices in image
C      SCRTCH  I(*)       Scratch buffer: > 1 line length (> 1280)
C      IERR    I          Error code
C   Common:
C      /MAPHDR/ CATBLK image catalog block of blotched image
C                      (actually used at lower level in YCUCOR)
C-----------------------------------------------------------------------
      INTEGER   MINC, MPOLY, IG, NPY, NV(MPOLY), XV(*), YV(*),
     *   SCRTCH(*), IERR
C
      INTEGER   ICH, ITW(3), IX(3), IY(3), IPOS, IB, IE, QUAD, IBUT,
     *   I, LB, LE, IBO, J, JERR, ITEMP, KERR, SCROLX, SCROLY
      REAL      PPOS(7), RPOS(2), DLIM
      LOGICAL   T, F, DOIT, ONGR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Check inputs
      IERR = 2
      IF ((IG.LT.1) .OR. (IG.GT.NGRAPH)) GO TO 999
C                                       Init
      CALL YHOLD ('ONNN', IERR)
      ICH = NGRAY + IG
      CALL YZERO (ICH, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZTIME (ITW)
C                                       turn on graphics to be sure
      I = 2 ** (ICH - 1)
      ONGR = MOD (TVLIMG(1)/I, 2) .EQ. 1
      CALL YSLECT ('ONNN', ICH, 0, SCRTCH, IERR)
      IPOS = 1
      IF (IERR.NE.0) GO TO 900
C                                       no scroll on graphics now
      ITEMP = 2 ** NGRAY
      SCROLX = TVSCRX(1)
      SCROLY = TVSCRY(1)
      CALL YSCROL (ITEMP, SCROLX, SCROLY, T, IERR)
      IPOS = 2
      IF (IERR.NE.0) GO TO 900
      NPY = 0
      IB = 0
      IE = 0
C                                       Start new polygon
 40   IF (NPY.GE.MPOLY) GO TO 800
      NPY = NPY + 1
      IB = IE + 1
      IE = IB
      NV(NPY) = 0
      WRITE (MSGTXT,1040) NPY
      CALL MSGWRT (1)
      MSGTXT = 'Press button A to set intermediate vertex'
      CALL MSGWRT (1)
      MSGTXT = 'Press buttons B, C, or D to set final vertex'
      CALL MSGWRT (1)
      MSGTXT = 'C => then reset a vertex,  D => then exit'
      CALL MSGWRT (1)
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
C                                       No scroll correction
C      QUAD = -1
C                                       ON cursor at desired position
      CALL YHOLD ('OFFF', IERR)
      CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 900
      IF (IERR.EQ.2) CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
      IPOS = 3
      IF (IERR.NE.0) GO TO 900
C                                       Cursor read loop
 50   CALL YCURSE ('READ', T, T, RPOS, QUAD, IBUT, IERR)
         IPOS = 4
         IF (IERR.NE.0) GO TO 900
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF (.NOT.DOIT) GO TO 50
C                                       No button -> no action
         IF (IBUT.EQ.0) GO TO 50
C                                       Time to quit marking
         IF ((IBUT.LT.4) .OR. (IB.LT.IE)) GO TO 60
            NPY = NPY - 1
            GO TO 200
C                                       check new vertex
 60      QUAD = 0
         CALL YCUCOR (RPOS, QUAD, PPOS, KERR)
         IF (KERR.NE.0) THEN
            MSGTXT = 'NOT ON IMAGE - TRY AGAIN'
            CALL MSGWRT (1)
            GO TO 50
            END IF
C                                       Mark new vertex
         NV(NPY) = NV(NPY) + 1
         XV(IE) = RPOS(1) + 0.51
         YV(IE) = RPOS(2) + 0.51
         IF (IB.EQ.IE) XV(IE+1) = XV(IE)
         IF (IB.EQ.IE) YV(IE+1) = YV(IE)
         I = MAX (IB, IE-1)
         IE = IE + 1
C                                       draw line
         CALL XGVECT ('ONNN', ICH, 2, XV(I), YV(I), SCRTCH, IERR)
         IPOS = 6
         IF (IERR.NE.0) GO TO 900
C                                       done with polygon
         IF ((IBUT.LT.2) .AND. (IE.LT.10*MPOLY)) GO TO 50
            NV(NPY) = NV(NPY) + 1
            XV(IE) = XV(IB)
            YV(IE) = YV(IB)
C                                       draw line
            CALL XGVECT ('ONNN', ICH, 2, XV(IE-1), YV(IE-1), SCRTCH,
     *         IERR)
            IPOS = 7
            IF (IERR.NE.0) GO TO 900
C                                       Respond further to buttons
         IF (IE.GE.10*MPOLY) GO TO 810
         IF (IBUT.LT.4) GO TO 40
C                                       Vertex correction area
         IF (IBUT.GE.8) GO TO 200
C                                       some already set box
C                                       wait for indication which
 100  MSGTXT = '*************  Move cursor to vertex to be reset'
      CALL MSGWRT (1)
      MSGTXT = 'Then push button A or B to do resetting -- or'
      CALL MSGWRT (1)
      MSGTXT = 'push C to go to next region  or D to exit'
      CALL MSGWRT (1)
      CALL YCURSE ('READ', T, T, RPOS, QUAD, IBUT, IERR)
      IPOS = 10
      IF (IERR.NE.0) GO TO 900
C                                       check button
      IF (IBUT.GE.8) GO TO 200
C                                       redraw polygons to be safe
      IF (IBUT.LT.4) GO TO 120
         LB = 1
         CALL YHOLD ('ONNN', IERR)
         DO 110 I = 1,NPY
            CALL XGVECT ('ONNN', ICH, NV(I), XV(LB), YV(LB), SCRTCH,
     *         IERR)
            IPOS = 10
            IF (IERR.NE.0) GO TO 900
            LB = LB + NV(I)
 110        CONTINUE
         CALL YHOLD ('OFFF', IERR)
         GO TO 40
C                                       Find nearest corner
 120  DLIM = MAX (0.5, MINC/3.0)
 125  LB = 1
      DO 135 I = 1,NPY
         LE = LB + NV(I) - 1
         DO 130 J = LB,LE
            IF ((ABS(RPOS(1)-XV(J)).LE.DLIM) .AND. (ABS(RPOS(2)
     *         -YV(J)).LE.DLIM)) GO TO 140
 130        CONTINUE
         LB = LE + 1
 135     CONTINUE
      DLIM = DLIM + 1.5
      IF (DLIM.LE.MINC-1) GO TO 125
      GO TO 100
C                                       Got one
 140  IF (J.EQ.LE) J = LB
      IBO = J
      RPOS(1) = XV(IBO)
      RPOS(2) = YV(IBO)
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      IX(2) = XV(IBO)
      IX(1) = XV(IBO-1)
      IX(3) = XV(IBO+1)
      IF (IBO.EQ.LB) IX(1) = XV(LE-1)
      IY(2) = YV(IBO)
      IY(1) = YV(IBO-1)
      IY(3) = YV(IBO+1)
      IF (IBO.EQ.LB) IY(1) = YV(LE-1)
C                                       Cursor read loop
 150  CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IERR)
         IPOS = 15
         IF (IERR.NE.0) GO TO 900
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF (.NOT.DOIT) GO TO 150
C                                       draw new lines
         CALL YHOLD ('ONNN', IERR)
         CALL XGVECT ('OFFF', ICH, 3, IX, IY, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         IX(2) = RPOS(1) + 0.5
         IY(2) = RPOS(2) + 0.5
         CALL XGVECT ('ONNN', ICH, 3, IX, IY, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL YHOLD ('OFFF', IERR)
C                                       mark this one
         IF (IBUT.EQ.0) GO TO 150
            XV(IBO) = IX(2)
            YV(IBO) = IY(2)
            IF (IBUT.LT.4) GO TO 100
            IF (IBUT.LT.8) GO TO 40
C                                       DONE: fill in real boxes
 200  IERR = 2
      IF (NPY.LT.1) GO TO 999
C                                       fill them in for fun
      CALL YHOLD ('ONNN', IERR)
      CALL BLTFIL (NPY, NV, XV, YV, IG, SCRTCH, IERR)
      IPOS = 20
      IF (IERR.NE.0) GO TO 900
      CALL YHOLD ('OFFF', IERR)
C                                       corners with scroll now
      TVSCRX(1) = 0
      TVSCRY(1) = 0
      LB = 1
      DO 230 I = 1,NPY
         LE = LB + NV(I) - 1
         DO 220 J = LB,LE
            RPOS(1) = XV(J)
            RPOS(2) = YV(J)
            QUAD = 0
            CALL YCUCOR (RPOS, QUAD, PPOS, KERR)
            IF (KERR.NE.0) THEN
               WRITE (MSGTXT,1200) KERR
               IF (KERR.EQ.1) WRITE (MSGTXT,1201) XV(J), YV(J)
               CALL MSGWRT (7)
               IF (KERR.NE.1) IERR = KERR
               IF (KERR.NE.1) GO TO 900
               IERR = IERR + KERR
               END IF
            XV(J) = PPOS(1) + 0.5
            YV(J) = PPOS(2) + 0.5
 220        CONTINUE
         LB = LE + 1
 230     CONTINUE
      IF (IERR.GT.0) IERR = 10 + IERR
      TVSCRX(1) = SCROLX
      TVSCRY(1) = SCROLY
      GO TO 900
C                                       Overflow problems
 800  WRITE (MSGTXT,1800) MPOLY
      CALL MSGWRT (7)
      GO TO 900
 810  WRITE (MSGTXT,1810) 10*MPOLY
      CALL MSGWRT (7)
      GO TO 200
C                                       Off cursor, graphics, scroll
C                                       leave graphics on
 900  CALL YCURSE ('OFFF', F, T, RPOS, QUAD, IBUT, JERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1900) IERR, IPOS
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('>>>>>>>>>>>>> Begin setting region number',I4)
 1200 FORMAT ('ERROR',I5,' CONVERTING TV TO PIXEL COORDINATES')
 1201 FORMAT (2I5,' NOT IN ANY VISIBLE IMAGE')
 1800 FORMAT ('REACHED LIMIT OF',I4,' POLYGONS')
 1810 FORMAT ('REACHED LIMIT OF',I5,' VERTICES')
 1900 FORMAT ('XGPOLY: ERROR CODE',I7,' AT',I5)
      END
      SUBROUTINE XGVECT (OP, CHAN, COUNT, XDATA, YDATA, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   IMVECT writes a connected sequence of line segments on a TV channel
C   for pixel-replicated image pixels
C   Inputs:
C      OP      C*4     'ONNN' line of ones (max intensity)
C                      'OFFF' line of zeros (min intensity)
C      CHAN    I       channel number (1 to NGRAY+NGRAPH)
C      COUNT   I       number of X,Y pairs ( > 1)
C      XDATA   I(COUNT)      X coordinates X1,X2,...
C      YDATA   I(COUNT)      Y coordinates Y1,Y2,...
C   Output:
C      SCRTCH  I(*)    scratch buffer (size MAXXTV)
C      IERR    I       error code of ZM70XF - 0 => ok;  2 => input error
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHAN, COUNT, XDATA(*), YDATA(*), SCRTCH(*), IERR
C
      INTEGER   INT, IC, IMGWIN(4), TVWIN(4), MINC, LENG, LPY, LPX,
     *   IPX, IPY, I, K
      REAL      RPOS(2), LX(100), LY(100), DX, DY
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       check inputs
      IERR = 2
      INT = -99
      IF ((OP.EQ.'ONNN') .AND. (CHAN.LE.NGRAY)) INT = MAXINT
      IF ((OP.EQ.'ONNN') .AND. (CHAN.GT.NGRAY)) INT = 2**NGRAPH - 1
      IF (OP.EQ.'OFFF') INT = 0
C                                       how big is a pixel
      CALL COPY (4, CATBLK(IIWIN), IMGWIN)
      CALL COPY (4, CATBLK(IICOR), TVWIN)
      MINC = (TVWIN(3) - TVWIN(1) + 1.0) / (IMGWIN(3) - IMGWIN(1) + 1.0)
     *   + 0.1
      CALL FILL (MINC, INT, SCRTCH)
      DO 10 IC = 1,COUNT
         LX(IC) = (XDATA(IC) - TVWIN(1)) / MINC
         LX(IC) = LX(IC) + IMGWIN(1)
         LY(IC) = (YDATA(IC) - TVWIN(2)) / MINC
         LY(IC) = IMGWIN(2) + LY(IC)
 10      CONTINUE
C                                       loop over line segments
      DO 100 IC = 1,COUNT-1
         DX = LX(IC+1) - LX(IC)
         DY = LY(IC+1) - LY(IC)
         LENG = MAX (ABS(DX), ABS(DY)) * 10.0 + 0.1
         LENG = MAX (LENG, 1)
         DX = DX / LENG
         DY = DY / LENG
         RPOS(1) = LX(IC) - DX
         RPOS(2) = LY(IC) - DY
         LPY = -1
         LPX = -1
         DO 90 K = 1,LENG
            RPOS(1) = RPOS(1) + DX
            RPOS(2) = RPOS(2) + DY
            IPX = RPOS(1) + 0.5
            IPY = RPOS(2) + 0.5
C                                       fill in pixel
            IF ((IPX.NE.LPX) .OR. (IPY.NE.LPY)) THEN
               LPX = IPX
               LPY = IPY
               IPX = TVWIN(1) + (LPX - IMGWIN(1)) * MINC
               IPY = TVWIN(2) + (LPY - IMGWIN(2)) * MINC - 1
               DO 20 I = 1,MINC
                  IPY = IPY + 1
                  CALL YIMGIO ('WRIT', CHAN, IPX, IPY, 0, MINC, SCRTCH,
     *               IERR)
                  IF (IERR.NE.0) GO TO 990
 20               CONTINUE
               END IF
 90         CONTINUE
 100     CONTINUE
      GO TO 999
C
 990  WRITE (MSGTXT,1990) IERR, IPX, IPY
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('XGVECT YIMGIO ERROR',I4,'AT TV X,Y',2I6)
      END

