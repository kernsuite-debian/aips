LOCAL INCLUDE 'PZEMAN.INC'
      INTEGER   MAXGAU, MAXPRM, MAXLIS, NPLIM, NMXIMG
      PARAMETER (MAXGAU=8)
      PARAMETER (MAXPRM=1+MAXGAU)
      PARAMETER (MAXLIS = 1000)
      PARAMETER (NPLIM=4096)
      PARAMETER (NMXIMG=2)
LOCAL END
LOCAL INCLUDE 'ZEMAN.INC'
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'PZEMAN.INC'
C                                       Local include for ZEMAN
      REAL      XSEQV, XDISKV, XSEQI, XDISKI, XSEQO, XDISKO, UBLC(7),
     *   UTRC(7), FCUT, XINV, XIN2V, XDOCAT, DOTV, RMSLIM, BADD(10)
      HOLLERITH XNAMEV(3), XCLASV(2), XNAMEI(3), XCLASI(2), XNAMOU(3),
     *   XCLAOU(2), XOPTYP
      CHARACTER NAMEV*12, CLASSV*6, NAMEI*12, CLASSI*6, NAMOUT*12,
     *   CLAOUT*6, OPTYPE*4, FUNCTY(NMXIMG*MAXGAU+2)*2, REFTYP*8
      REAL      BUFF1(MABFSS), BUFF2(MABFSS), BUFFO(MABFSS), PRCLIP,
     *   XNIT, BLC(7), TRC(7), REFPIX, REFINC, XEFPIX
      DOUBLE PRECISION REFVAL
      LOGICAL   ZENEW
      INTEGER   SEQI, SEQV, SEQOUT, DISKI, DISKV, DISKO, NEWCNO,
     *   OLDCNO(2), JBUFSZ, ICODE, JCODE, DOCOMP(MAXPRM), GCODE,
     *   SCRTCH(512), XGVERS, IYINC, IZINC, IBLC(2), ITRC(2),
     *   XGBUFF(512), PSTART, XGROWS, IXGRNO, XGKOLS(11), XGNUMV(11),
     *   TVSUP, DONROW, DOCAT, PIXLIS(2,MAXLIS), NLIST, IPL(2), NGAUSS,
     *   ZEBUFF(512), IZERNO, ZEKOLS(11), ZENUMV(11), ZEVERS, ZEROWS,
     *   LBLC(2), LTRC(2), SUBWIN(4)
      COMMON /INPARM/ XNAMEV, XCLASV, XSEQV, XDISKV, XNAMEI, XCLASI,
     *   XSEQI, XDISKI, XNAMOU, XCLAOU, XSEQO, XDISKO, UBLC, UTRC, FCUT,
     *   XINV, XOPTYP, XIN2V, XDOCAT, DOTV, RMSLIM, BADD
      COMMON /XGACHR/ NAMEI, CLASSI, NAMEV, CLASSV, NAMOUT, CLAOUT,
     *   FUNCTY, OPTYPE, REFTYP
      COMMON /PARMS/ XGBUFF, ZEBUFF, REFVAL, REFPIX, REFINC, SEQI, SEQV,
     *   SEQOUT, DISKI, DISKV, DISKO, NEWCNO, OLDCNO, JBUFSZ, ICODE,
     *   JCODE, DOCOMP, GCODE, XGVERS, IYINC, IZINC, IBLC, ITRC, ZENEW,
     *   PSTART, XGROWS, PRCLIP, IXGRNO, XGKOLS, XGNUMV, TVSUP, DONROW,
     *   DOCAT, PIXLIS, NLIST, IPL, NGAUSS, IZERNO, ZEKOLS, ZENUMV,
     *   ZEVERS, XNIT, ZEROWS, BLC, TRC, LBLC, LTRC, SUBWIN, XEFPIX
      COMMON /BUFRS/ BUFF1, BUFF2, BUFFO, SCRTCH
      INCLUDE 'INCS:DCAT.INC'
C                                                          End ZEMAN
LOCAL END
LOCAL INCLUDE 'ZEMAND.INC'
      INCLUDE 'PZEMAN.INC'
C
      DOUBLE PRECISION VDATA(NPLIM), IDATA(NPLIM), SLOPES(NPLIM,MAXGAU),
     *   VX(MAXPRM), VARRES
      REAL      XGAUSV(3*MAXGAU), ORANGE(2,2), XRANGE(2), XBAR, THERMS,
     *   XGAUSB(2)
      INTEGER   NITTER, ITTER, JJC, LLCOMP(MAXPRM), IGR1, IGR2, IGR3,
     *   IGR4, IGR5, TTYLUN, TTYIND, NGA, DOGAUS, IGLUN, IGFIND,
     *   PLTBLK(256), PLPOS(7)
      COMMON /GDATA/ VDATA, IDATA, SLOPES, VX, VARRES, NITTER, ITTER,
     *   JJC, LLCOMP, IGR1, IGR2, IGR3, IGR4, IGR5, TTYLUN, TTYIND, NGA,
     *   XGAUSV, XGAUSB, DOGAUS, ORANGE, IGLUN, IGFIND, PLTBLK, PLPOS,
     *   XRANGE, XBAR, THERMS
LOCAL END
LOCAL INCLUDE 'ZEMANO.INC'
      INTEGER   CATOLD(256,2)
      REAL      OLDR(256,2)
      HOLLERITH OLDH(256,2)
      DOUBLE PRECISION OLDD(128,2)
      EQUIVALENCE (CATOLD, OLDR, OLDD, OLDH)
      COMMON /OLDHDR/ CATOLD
LOCAL END
      PROGRAM ZEMAN
C-----------------------------------------------------------------------
C! Fits Zeeman models to spectra
C# Map Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 2013-2017
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
C   ZEMAN fits 1-dimensional Zeeman models to transposed spectral cubes.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEI         Name of input I image.
C      INCLASS        CLASSI        Class of input I image.
C      INSEQ          SEQI          Seq. of input I image.
C      INDISK         DISKI         Disk number of input I image.
C      IN2NAME        NAMEV         Name of input V image.
C      IN2CLASS       CLASSV        Class of input V image.
C      IN2SEQ         SEQV          Seq. of input V image.
C      IN2DISK        DISKV         Disk number of input V image.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTCLASS       CLAOUT        Class of the output image.
C                                   Default is input class.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      UBLC(7)        UBLC          Bottom left corner of subimage
C                                   of input image.
C      UTRC(7)        UTRC          Top right corner of subimage.
C      FLUX           FCUT          Flux cutoff: > 2 consecutive
C                                   points must > FLUX to fit
C                                   Also in initial auto-guesses
C      INVERS         ZEVERS        ZE table version number in use
C      DOOUTPUT       DOCAT         Catalog the residual map
C      DOTV           DOSLIC        Plot data on TV
C      BADD(10)       IBAD          Disk numbers to avoid.
C   Programmer Eric W. Greisen
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, IERR, DEVON, I
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMANO.INC'
      INCLUDE 'ZEMAND.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'ZEMAN '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL ZEMANI (PRGM, IRET)
      DEVON = 0
      IF (IRET.NE.0) GO TO 990
C                                       inits, open TV
      IF (ICODE.GE.1) THEN
         DEVON = 2
         TVSUP = 0
         CALL YINIT (BUFF2, IRET)
         IF (IRET.GT.0) GO TO 990
      ELSE
         TVSUP = 1
         END IF
C                                       routine that goes through
C                                       whole input cube
      IF ((IRET.EQ.0) .AND. (PSTART.LE.ZEROWS)) CALL ZEMAND (IRET)
C                                       interactive routine to polish
      IF ((IRET.EQ.0) .AND. (DEVON.EQ.2)) CALL ZMANTV (IRET)
C                                       close devices
      IF (DEVON.EQ.2) CALL TVCLOS (SCRTCH, IERR)
      DEVON = 0
C                                       Resume AIPS
      IF ((RQUICK) .AND. (ICODE.GT.0)) CALL RELPOP (IRET, SCRTCH, IERR)
C                                       write out images
      IF ((IRET.EQ.0) .AND. (DOCAT.GT.0)) CALL ZEMANO (IRET)
C                                       close ZE table
      CALL TABKEY ('WRIT', 'PIX FIT ', 1, ZEBUFF, 1, DONROW, 4, I)
      CALL TABIO ('CLOS', 0, IZERNO, ZEBUFF, ZEBUFF, I)
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE ZEMANI (PRGN, IRET)
C-----------------------------------------------------------------------
C   ZEMANI gets input parameters for ZEMAN and creates an output table
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
      CHARACTER STAT*4, MTYPE*2, CHTM12*12, PHNAME*48
      INTEGER   IERR, NPARM, IROUND, I, IY, IZ, IB(2), IT(2), XGLUN,
     *   PST, TVCORN(2), ABSORP
      DOUBLE PRECISION VOFF
      REAL      PRC
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMANO.INC'
      INCLUDE 'ZEMAND.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA TVCORN /2*0/
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
      TTYLUN = 5
      TTYIND = 0
C                                       Get input parameters.
      NPARM = 49
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEV, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR, 'OBTAINING INPUT PARAMETERS'
         CALL MSGWRT (8)
         END IF
C                                       Using the TV?
      ICODE = -1
      IF (DOTV.GT.0.) ICODE = 1
      IF (DOTV.GT.1.5) ICODE = 2
      IF ((NPOPS.GT.NINTRN) .AND. (NTVDEV.LE.0)) THEN
         ICODE = -1
         DOTV = -1.0
         END IF
C                                       Restart AIPS
      IF ((RQUICK) .AND. ((ICODE.LE.0) .OR. (IRET.NE.0))) CALL RELPOP
     *   (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQI = IROUND (XSEQI)
      SEQV = IROUND (XSEQV)
      SEQOUT = IROUND (XSEQO)
      DISKI = IROUND (XDISKI)
      DISKV = IROUND (XDISKV)
      DISKO = IROUND (XDISKO)
      DOCAT = IROUND (XDOCAT)
      DOCAT = MAX (0, MIN (7, DOCAT))
      IF (RMSLIM.LE.0.0) RMSLIM = 1000000.
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEI)
      CALL H2CHR (6, 1, XCLASI, CLASSI)
      CALL H2CHR (12, 1, XNAMEV, NAMEV)
      CALL H2CHR (6, 1, XCLASV, CLASSV)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (6, 1, XOPTYP, OPTYPE)
      DO 10 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 10      CONTINUE
      DOGAUS = 0
      IF (OPTYPE.EQ.'GAUS') THEN
         DOGAUS = 1
      ELSE IF (OPTYPE.EQ.'2SID') THEN
         DOGAUS = -1
         END IF
      GCODE = 0
      JCODE = 1
C                                       Get CATBLK from old file.
      OLDCNO(2) = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKI, OLDCNO(2), NAMEI, CLASSI, SEQI, MTYPE,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEI, CLASSI, SEQI, DISKI, NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DISKI, OLDCNO(2), CATOLD(1,2), 'READ', SCRTCH,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKI
      FCNO(NCFILE) = OLDCNO(2)
      FRW(NCFILE) = 0
C                                       Get CATBLK from old file.
      OLDCNO(1) = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKV, OLDCNO(1), NAMEV, CLASSV, SEQV, MTYPE,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEV, CLASSV, SEQV, DISKV, NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DISKV, OLDCNO(1), CATOLD(1,1), 'WRIT', SCRTCH,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKV
      FCNO(NCFILE) = OLDCNO(1)
      FRW(NCFILE) = 1
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD, CATBLK)
C                                       Set defaults on BLC,TRC
      CALL RFILL (7, 1.0, BLC)
      CALL RFILL (7, 0.0, TRC)
      CALL WINDOW (CATOLD(KIDIM,1), CATOLD(KINAX,1), BLC, TRC, IERR)
C                                       Set defaults on users subwindow
      CALL WINDOW (CATOLD(KIDIM,1), CATOLD(KINAX,1), UBLC, UTRC, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check input X axis
      CALL H2CHR (4, 1, CATH(KHCTP), CHTM12)
      IF ('FREQ'.NE.CHTM12(:4)) THEN
         MSGTXT = 'WARNING: FIRST AXIS IS NOT FREQUENCY'
         CALL MSGWRT (8)
         END IF
C                                       images must match  some
      IF ((CATOLD(KINAX,1).NE.CATOLD(KINAX,2)) .OR.
     *   (CATOLD(KINAX+1,1).NE.CATOLD(KINAX+1,2)) .OR.
     *   (CATOLD(KINAX+2,1).NE.CATOLD(KINAX+2,2))) THEN
         MSGTXT = 'I AND V IMAGES ARE NOT THE SAME SIZE'
         IERR = 10
         CALL MSGWRT (8)
         GO TO 995
         END IF
      IF ((OLDH(KHCTP,1).NE.OLDH(KHCTP,2)) .OR.
     *   (OLDH(KHCTP+2,1).NE.OLDH(KHCTP+2,2)) .OR.
     *   (OLDH(KHCTP+4,1).NE.OLDH(KHCTP+4,2))) THEN
         MSGTXT = 'I AND V IMAGES ARE NOT THE SAME TYPE'
         IERR = 10
         CALL MSGWRT (8)
         GO TO 995
         END IF
C                                       corners
      LBLC(1) = UBLC(2) + 0.1
      LBLC(2) = UBLC(3) + 0.1
      LTRC(1) = UTRC(2) + 0.1
      LTRC(2) = UTRC(3) + 0.1
      IBLC(1) = BLC(2) + 0.1
      IBLC(2) = BLC(3) + 0.1
      ITRC(1) = TRC(2) + 0.1
      ITRC(2) = TRC(3) + 0.1
      IF (FCUT.LE.0.0) FCUT = 0.0005
C                                       Gaussian parms
      IF (OPTYPE.EQ.'GAUS') THEN
         NGAUSS = MAXGAU
      ELSE
         NGAUSS = 1
         END IF
      CALL FILL (MAXPRM, 1, DOCOMP)
C                                       Check input size
      IRET = 0
      XNIT = 100
      IF (UTRC(1)-UBLC(1).GE.NPLIM) THEN
         IRET = 10
         WRITE (MSGTXT,1045) NPLIM
         GO TO 990
         END IF
C                                       GAUS check version, corners
      IF (OPTYPE.EQ.'GAUS') THEN
         CALL FNDEXT ('XG', CATOLD(1,2), I)
         XGVERS = XIN2V + 0.1
         IF (XGVERS.LE.0) THEN
            XGVERS = I
         ELSE
            XGVERS = MIN (I, XGVERS)
            END IF
         XGLUN = 97
         CALL XGINI ('READ', XGBUFF, DISKI, OLDCNO(2), XGVERS,
     *      CATOLD(1,2), XGLUN, IXGRNO, XGKOLS, XGNUMV, IB, IT, IY, IZ,
     *      PRC, VOFF, PST, REFVAL, REFPIX, REFINC, REFTYP, ABSORP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING XG TABLE FOR GAUS'
            GO TO 990
            END IF
         IF (ABSORP.GT.0) THEN
            MSGTXT = 'EXISTING XG TABLE IS FOR ABSORPTION: QUITTING'
            IRET = 10
            GO TO 990
            END IF
C                                       test corners
         IF ((IBLC(1).NE.IB(1)) .OR. (ITRC(1).NE.IT(1)) .OR.
     *      (IBLC(2).NE.IB(2)) .OR. (ITRC(2).NE.IT(2))) THEN
            WRITE (MSGTXT,1010) IBLC, ITRC
            CALL MSGWRT (8)
            WRITE (MSGTXT,1011) IB, IT
            IRET = 10
            GO TO 990
            END IF
         END IF
C                                       set up plotting
      IF (ICODE.GT.0) THEN
         PHNAME = ' '
         CALL GINIT (DISKV, OLDCNO, PHNAME, 0, 0, NPARM, XNAMEV, .TRUE.,
     *      0, 0, TVCORN, CATBLK, PLTBLK, IGLUN, IGFIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPENING TV PLOT'
            GO TO 990
            END IF
         END IF
C                                       ZE version
      CALL FNDEXT ('ZE', CATOLD, I)
      ZEVERS = XINV + 0.1
      IF (ZEVERS.LE.0) THEN
         ZEVERS = I + 1
      ELSE
         ZEVERS = MIN (I+1, ZEVERS)
         END IF
      ZENEW = ZEVERS.GT.I
C                                       fill ZE table
      CALL ZEFILL (DOGAUS, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C                                       Restart AIPS
 995  IF ((RQUICK) .AND. (IRET.NE.0) .AND. (ICODE.GT.0)) CALL RELPOP
     *   (IRET, SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZEMANI: ERROR',I3,' ON ',A)
 1010 FORMAT ('INPUT YZ WINDOW',4I5)
 1011 FORMAT ('DOES NOT MATCH XG WINDOW',4I6,' I QUIT')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATALOG HEADER ')
 1045 FORMAT ('WORKS ONLY ON (SUB)ROWS <=',I5,' PIXELS')
      END
      SUBROUTINE ZEFILL (DOGAUS, IRET)
C-----------------------------------------------------------------------
C   ZEFILL checks pre-existing ZE files and builds new ones filling
C   them with spectrum peak values
C   In/Out:
C      DOGAUS   I   Code for type of model (-1, 0, 1)
C   Output:
C      IRET     I   Error code
C-----------------------------------------------------------------------
      INTEGER   DOGAUS, IRET
C
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMANO.INC'
      INTEGER   IB(2), IT(2), ZELUN, LUNI, INDI, NXI, NYI, I, WINI(4),
     *   IROUND, LIM3, LIM2, LIM1, I3, I2, I1, BOI, IPOS(7), IBIND, NGA,
     *   YZPOS(2), NGAMAX, IGA, ZABSRP
      REAL      RESULT(2*MAXPRM), VPEAK, VAL, XGAUSV(3*MAXGAU), IPEAK,
     *   XGRES(4+6*MAXGAU), XEFINC, XGAUSB(2)
      DOUBLE PRECISION VOFF, XEFVAL
      CHARACTER PHNAME*48, NOPT(3)*4, XEFTYP*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA NOPT /'2SID', '1SID', 'GAUS'/
C-----------------------------------------------------------------------
      NGA = 0
      CALL RFILL (3*MAXGAU, FBLANK, XGAUSV)
      CALL RFILL (2, FBLANK, XGAUSB)
      CALL RFILL (2*MAXPRM, FBLANK, RESULT)
      XEFTYP = REFTYP
      XEFVAL = REFVAL
      XEFPIX = REFPIX
      XEFINC = REFINC
C                                       ZE file pre-existing check
      ZELUN = 98
      IF (.NOT.ZENEW) THEN
         CALL ZEINI ('READ', ZEBUFF, DISKV, OLDCNO(1), ZEVERS, CATOLD,
     *      ZELUN, IZERNO, ZEKOLS, ZENUMV, IB, IT, I, PRCLIP, PSTART,
     *      REFVAL, REFPIX, REFINC, REFTYP, ZABSRP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING PRE-EXISTING ZE TABLE'
            GO TO 990
            END IF
         CALL TABIO ('CLOS', 0, IZERNO, ZEBUFF, ZEBUFF, IRET)
         IF ((IB(1).GT.LBLC(1)) .OR. (IB(2).GT.LBLC(2)) .OR.
     *      (IT(1).LT.LTRC(1)) .OR. (IT(2).LT.LTRC(2))) THEN
            MSGTXT = 'OLD ZE TABLE DOES NOT MATCH CURRENT ADVERBS'
            IRET = 10
            GO TO 990
         ELSE IF ((REFPIX.NE.OLDR(KRCRP,1)) .OR.
     *      (REFINC.NE.OLDR(KRCIC,1)) .OR. (REFVAL.NE.OLDD(KDCRV,1)))
     *      THEN
            MSGTXT = 'AXIS 1 VALUES HAVE CHANGED: USING OLDER VALUES'
            CALL MSGWRT (6)
            END IF
         IF (ZABSRP.GT.0) THEN
            MSGTXT = 'EXISTING ZE TABLE IS FOR ABSORPTION: QUITTING'
            IRET = 10
            GO TO 990
            END IF
         IF (I.NE.DOGAUS) THEN
            MSGTXT = 'OLD ZE TABLE FORCES OPTYPE ' // NOPT(I+2)
            CALL MSGWRT (7)
            OPTYPE = NOPT(I+2)
            DOGAUS = I
            END IF
         ZEROWS = ZEBUFF(5)
C                                       reopen write
         CALL ZEINI ('WRIT', ZEBUFF, DISKV, OLDCNO(1), ZEVERS, CATOLD,
     *      ZELUN, IZERNO, ZEKOLS, ZENUMV, IB, IT, DOGAUS, PRCLIP,
     *      PSTART, REFVAL, REFPIX, REFINC, REFTYP, ZABSRP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RE-OPENING OLD ZE TABLE'
            GO TO 990
            END IF
         PSTART = MAX (PSTART + 1, 1)
C                                       from beginning if lower cutoff
         IF (PRCLIP.GT.FCUT) THEN
            PSTART = 1
            VOFF = 0.0D0
            VOFF = FCUT
            CALL TABKEY ('WRIT', 'ICLIP   ', 1, ZEBUFF, 1, VOFF, 1, I1)
            END IF
C                                       new one
      ELSE
         IBLC(1) = BLC(2) + 0.1
         IBLC(2) = BLC(3) + 0.1
         ITRC(1) = TRC(2) + 0.1
         ITRC(2) = TRC(3) + 0.1
         PSTART = 0
         REFVAL = OLDD(KDCRV,1)
         REFPIX = OLDR(KRCRP,1)
         REFINC = OLDR(KRCIC,1)
         CALL H2CHR (8, 1, OLDH(KHCTP,1), REFTYP)
         ZABSRP = -1
         CALL ZEINI ('WRIT', ZEBUFF, DISKV, OLDCNO(1), ZEVERS, CATOLD,
     *      ZELUN, IZERNO, ZEKOLS, ZENUMV, IBLC, ITRC, DOGAUS, FCUT,
     *      PSTART, REFVAL, REFPIX, REFINC, REFTYP, ZABSRP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING NEW ZE TABLE'
            GO TO 990
            END IF
         CALL RFILL (2*MAXPRM, FBLANK, RESULT)
         MSGTXT = 'Reading image cube to find maxima in rows'
         CALL MSGWRT (2)
         MSGTXT = 'and initialize the ZE table with these values'
         CALL MSGWRT (2)
C                                       Open and init for read
         LUNI = 33
         CALL ZPHFIL ('MA', DISKI, OLDCNO(2), 1, PHNAME, IRET)
         CALL ZOPEN (LUNI, INDI, DISKI, PHNAME, .TRUE., .FALSE.,
     *      .TRUE., IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INPUT I IMAGE'
            GO TO 990
            END IF
         NXI = CATOLD(KINAX,2)
         NYI = CATOLD(KINAX+1,2)
         WINI(1) = IROUND (UBLC(1))
         WINI(2) = IROUND (BLC(2))
         WINI(3) = IROUND (UTRC(1))
         WINI(4) = IROUND (TRC(2))
         LIM3 = TRC(3) - BLC(3) + 1.01
         LIM2 = TRC(2) - BLC(2) + 1.01
         LIM1 = TRC(1) - BLC(1) + 1.01
         CALL FILL (7, 1, IPOS)
         DO 100 I3 = 1,LIM3
            IPOS(3) = BLC(3) + I3 - 0.9
            CALL COMOFF (CATOLD(KIDIM,2), CATOLD(KINAX,2), IPOS(3), BOI,
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
               CALL TABZE ('WRIT', ZEBUFF, IZERNO, ZEKOLS, ZENUMV,
     *            IPOS(2), VPEAK, RESULT, NGA, XGAUSV, XGAUSB, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE NEW ZE TABLE'
                  GO TO 990
                  END IF
 90            CONTINUE
 100        CONTINUE
         CALL ZCLOSE (LUNI, INDI, IRET)
         ZEROWS = ZEBUFF(5)
C                                       close table for safety
         CALL TABIO ('CLOS', 0, IZERNO, ZEBUFF, ZEBUFF, IRET)
C                                       and reopen
         CALL ZEINI ('WRIT', ZEBUFF, DISKV, OLDCNO(1), ZEVERS, CATOLD,
     *      ZELUN, IZERNO, ZEKOLS, ZENUMV, IBLC, ITRC, DOGAUS, FCUT,
     *      PSTART, REFVAL, REFPIX, REFINC, REFTYP, ZABSRP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING NEW ZE TABLE'
            GO TO 990
            END IF
         PSTART = 1
         END IF
C                                       now copy XGAUS results to ZE
      NGAMAX = 1
      IF (OPTYPE.EQ.'GAUS') THEN
         IF ((XEFTYP.NE.REFTYP) .OR. (XEFVAL.NE.REFVAL) .OR.
     *      (XEFPIX.NE.REFPIX) .OR. (XEFINC.NE.REFINC)) THEN
            MSGTXT = 'XF FILE COORDINATES DO NOT MATCH ZE FILE' //
     *         ' COORDINATES'
            CALL MSGWRT (8)
            MSGTXT = 'CONTINUING - BUT THIS IS PROBABLY NOT RIGHT'
            CALL MSGWRT (8)
            END IF
         NGAMAX = 0
         DO 200 I = 1,ZEROWS
            IZERNO = I
            CALL TABZE ('READ', ZEBUFF, IZERNO, ZEKOLS, ZENUMV,
     *         IPOS(2), IPEAK, RESULT, NGA, XGAUSV, XGAUSB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ ZE TABLE'
               GO TO 990
               END IF
            IXGRNO = I
            CALL TABXG ('READ', XGBUFF, IXGRNO, XGKOLS, XGNUMV,
     *         YZPOS, NGA, VPEAK, XGRES, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ ZE TABLE'
               GO TO 990
               END IF
C                                       find highest number Gauss fit
            CALL RCOPY (3*MAXGAU, XGRES(3), XGAUSV)
            CALL RCOPY (2, XGRES, XGAUSB)
            DO 120 IGA = MAXGAU,1,-1
               IF ((XGAUSV(3*IGA-2).NE.FBLANK) .AND.
     *            (XGAUSV(3*IGA).NE.0.0)) NGAMAX = MAX (NGAMAX, IGA)
 120           CONTINUE
            IZERNO = I
            CALL TABZE ('WRIT', ZEBUFF, IZERNO, ZEKOLS, ZENUMV,
     *         IPOS(2), IPEAK, RESULT, NGA, XGAUSV, XGAUSB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE ZE TABLE'
               GO TO 990
               END IF
 200        CONTINUE
         END IF
      NGAUSS = NGAMAX
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZEFILL: ERROR',I4,' ON ',A)
      END
      SUBROUTINE ZEMAND (IRET)
C-----------------------------------------------------------------------
C   ZEMAND goes through the table on a stride of 1, sends data to
C   fitting routine gets the initial guess from fit pixels.
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMANO.INC'
      INCLUDE 'ZEMAND.INC'
      CHARACTER PHNAME*48
      INTEGER   IROUND, LUNI, NYI, NXI, WINI(4), BOI, J, I1, IPOS(7),
     *   BOTEMP, IBIND, INDI, LIM1, IG, LZERNO, FIRSTY, IY, IZ,
     *   XXPOS(2), MGAU, LUNV, INDV, VBIND
      REAL      RESULT(2*MAXPRM), IPEAK, TPEAK, XGAUSD(3*MAXGAU),
     *   XGAUSL(2)
      DOUBLE PRECISION PARMS(MAXPRM), UPARMS(MAXPRM), XPARMS(MAXPRM)
      LOGICAL   T, F, FIRSTZ
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI, LUNV /16, 17/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Note: CATOLD & CATBLK are
C                                       now the same
C                                       display parms
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKI, OLDCNO(2), 1, PHNAME, IRET)
      CALL ZOPEN (LUNI, INDI, DISKI, PHNAME, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT I FILE'
         GO TO 990
         END IF
      CALL ZPHFIL ('MA', DISKV, OLDCNO(1), 1, PHNAME, IRET)
      CALL ZOPEN (LUNV, INDV, DISKV, PHNAME, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT I FILE'
         GO TO 990
         END IF
C                                       Setup for I/O
      NXI = CATOLD(KINAX,1)
      NYI = CATOLD(KINAX+1,1)
      WINI(1) = IROUND (UBLC(1))
      WINI(2) = IROUND (UBLC(2))
      WINI(3) = IROUND (UTRC(1))
      WINI(4) = IROUND (UTRC(2))
C                                       Initial guess
      PARMS(1) = 0.0D0
      DO 50 J = 1,NGAUSS
         PARMS(J+1) = 0.0D0
 50      CONTINUE
      IG = NGAUSS + 1
      DO 55 I1 = 1,IG
         UPARMS(I1) = PARMS(I1)
 55      CONTINUE
      CALL COPY (MAXPRM, DOCOMP, LLCOMP)
      MSGTXT = 'ZEMAND: solving Zeeman at every pixel'
      CALL MSGWRT (2)
C                                       Setup for looping
C                                       Loop
      LIM1 = UTRC(1) - UBLC(1) + 1.01
      CALL FILL (7, 1, IPOS)
      IPOS(1) = UBLC(1) + 0.01
      FIRSTY = 0
      IZERNO = 1
      DO 200 IZ = LBLC(2),LTRC(2)
         FIRSTZ = .TRUE.
         DO 190 IY = LBLC(1),LTRC(1)
            IZERNO = (IZ-IBLC(2)) * (ITRC(1)-IBLC(1)+1) + IY - IBLC(1) +
     *         1
            LZERNO = IZERNO
            CALL TABZE ('READ', ZEBUFF, IZERNO, ZEKOLS, ZENUMV, IPOS(2),
     *         IPEAK, RESULT, NGA, XGAUSV, XGAUSB, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ ZE TABLE'
               GO TO 990
            ELSE IF ((IRET.EQ.0) .AND. (RESULT(1).EQ.FBLANK) .AND.
     *         (IPEAK.GE.FCUT)) THEN
C                                       Init. files, first input.
               IF ((FIRSTZ) .AND. (FIRSTY.GT.0)) THEN
                  IZERNO = (IZ-IBLC(2)-1) * (ITRC(1)-IBLC(1)+1) +
     *               FIRSTY - IBLC(1) + 1
                  CALL TABZE ('READ', ZEBUFF, IZERNO, ZEKOLS, ZENUMV,
     *               XXPOS, TPEAK, RESULT, MGAU, XGAUSD, XGAUSL, IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READ ZE TABLE'
                     GO TO 990
                     END IF
                  DO 60 I1 = 1,MAXPRM
                     IF (RESULT(I1).NE.FBLANK) XPARMS(I1) = RESULT(I1)
 60                  CONTINUE
                  FIRSTY = 0
                  END IF
               FIRSTZ = .FALSE.
C                                       Init. files, V input.
               CALL COMOFF (CATOLD(KIDIM,1), CATOLD(KINAX,1), IPOS(3),
     *            BOTEMP, IRET)
               BOI = BOTEMP + 1
               WINI(2) = IPOS(2)
               WINI(4) = IPOS(2)
               CALL MINIT ('READ', LUNV, INDV, NXI, NYI, WINI, BUFF1,
     *            JBUFSZ, BOI, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'INIT READ INPUT V IMAGE'
                  GO TO 990
                  END IF
C                                       Read.
               CALL MDISK ('READ', LUNV, INDV, BUFF1, VBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READING INPUT V IMAGE'
                  GO TO 990
                  END IF
C                                       Init. files, I input.
               CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF2,
     *            JBUFSZ, BOI, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'INIT READ INPUT V IMAGE'
                  GO TO 990
                  END IF
C                                       Read.
               CALL MDISK ('READ', LUNI, INDI, BUFF2, IBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READING INPUT V IMAGE'
                  GO TO 990
                  END IF
C                                       Copy to buffer.
               DO 110 I1 = 1,LIM1
                  VDATA(I1) = BUFF1(VBIND+I1-1)
                  IDATA(I1) = BUFF2(IBIND+I1-1)
 110              CONTINUE
C                                       Call DO1FIT
               CALL DO1FIT (IPOS, UPARMS, PARMS, XPARMS, RESULT, IRET)
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
               IZERNO = LZERNO
               DONROW = LZERNO
               CALL TABZE ('WRIT', ZEBUFF, IZERNO, ZEKOLS, ZENUMV,
     *            IPOS(2), IPEAK, RESULT, NGA, XGAUSV, XGAUSB, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE ZE TABLE'
                  GO TO 990
                  END IF
            ELSE IF (IRET.EQ.0) THEN
               DONROW = LZERNO
C                                       pick up good solution as last
               DO 120 I1 = 1,MAXPRM
                  IF (RESULT(I1).NE.FBLANK) PARMS(I1) = RESULT(I1)
 120              CONTINUE
               END IF
 190        CONTINUE
 200     CONTINUE
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNV, INDV, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZEMAND: ERROR',I3,' ON ',A)
 1180 FORMAT ('ZEMAND: DO1FIT ERROR',I3)
      END
      SUBROUTINE DO1FIT (IPOS, UPARMS, PARMS, XPARMS, RESULT, IRET)
C-----------------------------------------------------------------------
C   DO1FIT fits a Zeeman model to a row of an image and returns the
C   answers in RESULT.
C   Inputs:
C      IPOS     I(7)    BLC (input image) of first value in DATA
C      UPARMS   D(14)   Initial guess (input by user)
C      XPARMS   D(14)   Last fit in row below (0 -> do not use)
C   Values from commons:
C      IDATA    D(*)    Input I row, magic value blanked.
C      VDATA    D(*)    Input V row, magic value blanked.
C      FBLANK   R       Value of blanked pixel.
C      CATBLK   I       Output catalog header (also CATR, CATD)
C      CATOLD   I       Input catalog header (also OLDR, OLDD)
C   In/out:
C      PARMS    D(14)   In: last answer, Out: Answer in fitting units
C   Output:
C      RESULT   R(10)   Output row (parameter answers, errors).
C      IRET     I       Return code   0 => OK
C                               >0 => error, terminate.
C   Output in COMMON
C      CATBLK   I       Catalog header block - revised for slice header
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IRET
      REAL      RESULT(*)
      DOUBLE PRECISION UPARMS(*), PARMS(*), XPARMS(*)
C
      INCLUDE 'ZEMAND.INC'
      INCLUDE 'ZEMANO.INC'
      INTEGER   INFO, ING, INPARM, INPTS, LERR, LABEL, TERR, LCODE,
     *   IERR, NTRY, ITRY, I
      DOUBLE PRECISION  FJAC(MAXPRM,MAXPRM), FVEC(NPLIM)
      INTEGER   JNPTS, JNPARM
      LOGICAL   DIRECT
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVS.INC'
C-----------------------------------------------------------------------
      IRET = 0
      NTRY = 0
      DIRECT = NGAUSS.EQ.1
C                                       Not last call
      IF (IPOS(1).GE.0) THEN
         XBAR = IPOS(1) - 1 - XEFPIX
         CALL GSLOPE
C                                       Get the initial guess
 10      NTRY = NTRY + 1
         LCODE = ICODE
         IF (TVSUP.GT.0) LCODE = 0
         ING = NGAUSS
         INPARM = NGAUSS + 1
         INPTS = UTRC(1) - UBLC(1) + 1.01
         ITTER = 0
         NITTER = XNIT + 1.01
         JJC = 1
         CALL ZMANGE (NGAUSS, INPTS, FCUT, UPARMS, XPARMS, DOCOMP,
     *      PARMS, FVEC, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Plot it
         IF (LCODE.GE.1) THEN
            ITRY = 0
            ITRY = ITRY + 1
            LABEL = 3
            CALL ZTVINI (DOTV, IPOS, INPTS, PARMS, TERR)
            IF ((TERR.GT.0) .AND. (TERR.LT.100)) THEN
               IRET = TERR
               WRITE (MSGTXT,1000) IRET, 'TV ERROR IN ZTVINI'
               GO TO 990
            ELSE IF (TERR.EQ.102) THEN
               IRET = 99
               GO TO 990
            ELSE IF (TERR.EQ.101) THEN
               GO TO 900
               END IF
            END IF
C                                       Fit model
         JNPTS = INPTS
         JNPARM = INPARM
         CALL ZMANFI (JNPTS, JNPARM, PARMS, RESULT, INFO)
         IF (INFO.GT.0) THEN
            WRITE (MSGTXT,1020) INFO
            CALL MSGWRT (6)
            IF ((LCODE.NE.ICODE) .AND. (ICODE.GE.1)) THEN
               TVSUP = 0
               LCODE = ICODE
               MSGTXT = 'Restart TV because of failure'
               CALL MSGWRT (2)
               WRITE (MSGTXT,1100) (PARMS(I), I = 1,ING+1)
               CALL MSGWRT (3)
               GO TO 10
C                                       null solution
            ELSE
               GO TO 900
               END IF
            END IF
         CALL ZMANCH (INPTS, FVEC, PARMS, LERR)
C                                       non-interactive: drop bad
         IF (LCODE.LT.1) THEN
C                                       restart TV
            IF (LERR.NE.0) THEN
               IF (ICODE.GE.1) THEN
                  TVSUP = 0
                  LCODE = ICODE
                  MSGTXT = 'Restart TV because of failure'
                  CALL MSGWRT (2)
                  WRITE (MSGTXT,1100) (PARMS(I), I = 1,ING+1)
                  CALL MSGWRT (3)
                  GO TO 10
C                                       null solution
               ELSE
                  GO TO 900
                  END IF
               END IF
C                                       interactive
         ELSE
            CALL ZTVMOD (DOTV, INPTS, IPOS, ING, FVEC, PARMS, LERR,
     *         TERR)
            IF (TERR.EQ.101) THEN
               GO TO 900
            ELSE IF (TERR.EQ.102) THEN
               IRET = 99
               GO TO 999
            ELSE IF (TERR.EQ.104) THEN
               TVSUP = 1
               MSGTXT = 'TV turned off until error or next step'
               CALL MSGWRT (2)
               END IF
C                                       PARMS may have changed
            CALL REDOAN (JNPTS, JNPARM, PARMS, FVEC, FJAC, RESULT)
            END IF
         GO TO 999
C                                       Blank outputs
 900     CALL RFILL (2*MAXPRM+2*MAXGAU, FBLANK, RESULT)
         CALL DFILL (MAXPRM, 0.0D0, PARMS)
         END IF
C
 990  IF (IRET.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DO1FIT: ERROR',I4,' ON ',A)
 1020 FORMAT ('FIT ROUTINE RETURNS ERROR CODE',I3)
 1100 FORMAT (5F11.6)
      END
      SUBROUTINE GSLOPE
C-----------------------------------------------------------------------
C   computes the SLOPES array (1/2 of the slope since it is B/2 DI/Dnu)
C   Input in common: IDATA, DOGAUS, NGA, XGAUSV
C   Output in common: SLOPES
C-----------------------------------------------------------------------
C
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMAND.INC'
      INTEGER   I, INPTS, J
      DOUBLE PRECISION AMP, POS, SIG, X, R, HALFAC, V
      INCLUDE 'INCS:DDCH.INC'
      DATA HALFAC /2.77258872D0/
C-----------------------------------------------------------------------
      INPTS = UTRC(1) - UBLC(1) + 1.01
      R = FBLANK
      DO 10 I = 1,NGAUSS
         CALL DFILL (INPTS, R, SLOPES(1,I))
 10      CONTINUE
C                                       2 sided
      IF (DOGAUS.EQ.-1) THEN
         IF ((IDATA(1).NE.FBLANK) .AND. (IDATA(2).NE.FBLANK))
     *      SLOPES(1,1) = 0.5D0 * (IDATA(2) - IDATA(1))
         IF ((IDATA(INPTS-1).NE.FBLANK) .AND. (IDATA(INPTS).NE.FBLANK))
     *      SLOPES(INPTS,1) = 0.5D0 * (IDATA(INPTS) - IDATA(INPTS-1))
         DO 20 I = 2,INPTS-1
            IF ((IDATA(I+1).NE.FBLANK) .AND. (IDATA(I-1).NE.FBLANK))
     *         SLOPES(I,1) = 0.25D0 * (IDATA(I+1) - IDATA(I-1))
 20         CONTINUE
C                                       1 sided
      ELSE IF (DOGAUS.EQ.0) THEN
         IF ((IDATA(1).NE.FBLANK) .AND. (IDATA(2).NE.FBLANK))
     *      SLOPES(1,1) = 0.5D0 * (IDATA(2) - IDATA(1))
         IF ((IDATA(INPTS-1).NE.FBLANK) .AND. (IDATA(INPTS).NE.FBLANK))
     *      SLOPES(INPTS,1) = 0.5D0 * (IDATA(INPTS) - IDATA(INPTS-1))
         DO 30 I = 2,INPTS-1
            IF ((IDATA(I+1).NE.FBLANK) .AND. (IDATA(I).NE.FBLANK))
     *         SLOPES(I,1) = 0.5D0 * (IDATA(I+1) - IDATA(I))
 30         CONTINUE
C                                       Gaussians
      ELSE
         DO 50 J = 1,MAXGAU
            IF ((XGAUSV(3*J-2).NE.FBLANK) .AND. (XGAUSV(3*J).NE.0.0))
     *         THEN
               AMP = XGAUSV(3*J-2)
               POS = XGAUSV(3*J-1)
               SIG = ABS (XGAUSV(3*J))
               DO 40 I = 1,INPTS
                  X = I + XBAR
                  R = (X - POS) / SIG
                  R = HALFAC * R * R
                  IF (R.LE.69.0D0) THEN
                     V = AMP * EXP (-R)
                     SLOPES(I,J) = -V * HALFAC * (X - POS) / (SIG * SIG)
                     END IF
 40               CONTINUE
               END IF
 50         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE ZMANFI (M, N, X, RESULT, IRET)
C-----------------------------------------------------------------------
C   ZMANFI does the least-squares fit using DLESQR
C   Inputs:
C      M        I        Number data points (adj. array dim.)
C      N        I        Number of unknowns (adj. array dim.)
C   Outputs:
C      X        D(N)     Vector of solutions
C      RESULT   R(*,2)   Vector of solutions and errors
C      IRET     I        Error code
C-----------------------------------------------------------------------
      INTEGER   M, N, IRET
      DOUBLE PRECISION X(N)
      REAL      RESULT(*)
C
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMAND.INC'
      INTEGER   K, I, J, L, NFIT, LNDX(MAXPRM), NAFIT
      DOUBLE PRECISION SUM, SSQ, R(MAXPRM), MATR(MAXPRM*MAXPRM), CI, CJ,
     *   SSQRES, VARY, FIT, NOBS
      LOGICAL   FIRST
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE FIRST
      DATA FIRST /.FALSE./
C-----------------------------------------------------------------------
      SUM = 0.0D0
      SSQ = 0.0D0
      CALL DFILL (MAXPRM, 0.0D0, R)
      I = MAXPRM * MAXPRM
      CALL DFILL (I, 0.0D0, MATR)
      NOBS = 0.0D0
      NFIT = NGAUSS + 1
C                                       what things need solution?
      L = 0
      CALL FILL (NFIT, 0, LNDX)
      IF (LLCOMP(1).GT.0) THEN
         L = L + 1
         LNDX(1) = L
         END IF
C                                       non-GAUS always do field(1)
      IF (DOGAUS.LE.0) THEN
         L = L + 1
         LNDX(2) = L
      ELSE
         DO 10 K = 1,MAXGAU
            IF ((LLCOMP(K+1).GT.0) .AND. (XGAUSV(3*K-2).NE.FBLANK)) THEN
               L = L + 1
               LNDX(K+1) = L
               END IF
 10         CONTINUE
         END IF
      NAFIT = L
C                                       sum vector and matrix
      DO 50 K = 1,M
         IF ((IDATA(K).NE.FBLANK) .AND. (VDATA(K).NE.FBLANK)) THEN
            NOBS = NOBS + 1.0D0
            SUM = SUM + VDATA(K)
            SSQ = SSQ + VDATA(K) * VDATA(K)
            DO 30 I = 1,NFIT
               IF (LNDX(I).GT.0) THEN
                  CI = 0.0D0
                  IF (I.EQ.1) THEN
                     CI = IDATA(K)
                  ELSE
                     IF (SLOPES(K,I-1).NE.FBLANK) CI = SLOPES(K,I-1)
                     END IF
                  R(I) = R(I) + VDATA(K) * CI
                  DO 20 J = I,NFIT
                     IF (LNDX(J).GT.0) THEN
                        CJ = 0.0D0
                        IF (J.EQ.1) THEN
                           CJ = IDATA(K)
                        ELSE
                           IF (SLOPES(K,J-1).NE.FBLANK)
     *                        CJ = SLOPES(K,J-1)
                           END IF
                        L = LNDX(I) + (LNDX(J)-1) * NAFIT
                        MATR(L) = MATR(L) + CI * CJ
                        END IF
 20                  CONTINUE
                  END IF
 30            CONTINUE
            END IF
 50      CONTINUE
C                                       do the fit
      CALL DLESQR (NAFIT, NOBS, SUM, SSQ, R, MATR, X, VX, SSQRES,
     *   VARRES, VARY, FIT, IRET)
      IF (X(3).NE.0.0D0) FIRST = .TRUE.
C                                       return answers
      CALL RFILL (2*MAXPRM, FBLANK, RESULT)
      DO 100 I = 1,NFIT
         IF ((LLCOMP(I).GT.0) .AND. (LNDX(I).GT.0)) THEN
            RESULT(I) = X(LNDX(I))
            RESULT(MAXPRM+I) = SQRT (ABS (VX(LNDX(I))))
            END IF
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE REDOAN (M, N, X, FVEC, FJAC, RESULT)
C-----------------------------------------------------------------------
C   REDOAN computes a new residual vector, a new covariance, and redoes
C   the error bars answers to be stored in the table
C   Inputs:
C      M        I        Number data points (adj. array dim.)
C      N        I        Number of unknowns (adj. array dim.)
C      X        D(N)     Vector of solutions
C   Output:
C      FVEC     D(M)     residuals
C      FJAC     D(*,*)   work matrix
C      RESULT   R(*)     baseline, slope, amp, center, width...
C-----------------------------------------------------------------------
      INTEGER   M, N
      DOUBLE PRECISION X(N), FVEC(M), FJAC(*)
      REAL      RESULT(*)
C
      INCLUDE 'ZEMAND.INC'
      INTEGER   I
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       get residual
      I = 1
      CALL ZEFUNC (M, N, X, FVEC, FJAC, I)
C                                       store result
      DO 10 I = 1,N
         RESULT(I) = X(I)
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ZMANGE (NG, ND, FC, UPARMS, XPARMS, DOCOMP, RPARMS,
     *   FVEC, IERR)
C-----------------------------------------------------------------------
C   ZMANGE obtains an initial guess for the parameters of the Zeeman
C   model.  It gets a baseline guess, checks the data, and chooses
C   between the last solution and the user's initial guess.
C   Inputs:
C      NG       I        Number of Gaussians
C      ND       I        Number of data samples
C      FC       R        Flux cutoff
C      UPARMS   D(14)    User's initial guess
C      DOCOMP   I(14)    > 0 -> do the parameter
C   In.out:
C      XPARMS   D(14)    another initial guess 0.0D0 -> none
C                           RETURNS 0.0 when XPARMS was considered
C      RPARMS   D(3NG)   In: previous solution
C                        Out:Guess to use
C   Output:
C      FVEC     D(*)     Buffer for computation
C      IERR     I        0 => ok, 1 => all data too low
C                        2 => input error
C   Common output:
C      LLCOMP   I(14)    copy DOCOMP unless previous guess is
C                                 used.  Then leave it alone.
C-----------------------------------------------------------------------
      INTEGER   NG, ND, DOCOMP(*), IERR
      REAL      FC
      DOUBLE PRECISION UPARMS(*), XPARMS(*), RPARMS(*), FVEC(*)
C
      INCLUDE 'ZEMAND.INC'
      DOUBLE PRECISION PARMS(MAXPRM), FJAC(MAXPRM)
      INTEGER   JD, IJ, I, J, NS
      REAL      TS, RMS(3)
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Test inputs
      IERR = 2
      IF ((ND.LT.3*NG+JJC+6) .OR. (ND.GT.NPLIM)) GO TO 999
      DO 5 I = 1,MAXPRM
         PARMS(I) = UPARMS(I)
 5       CONTINUE
C                                       Baseline
      TS = 0.0
      NS = 0
      DO 10 I = 1,ND
         IF ((IDATA(I).NE.FBLANK) .AND. (IDATA(I).NE.0.0D0) .AND.
     *      (IDATA(I).GT.FC) .AND. (VDATA(I).NE.FBLANK)) THEN
            TS = TS + VDATA(I)/IDATA(I)
            NS = NS + 1
            END IF
 10      CONTINUE
      IF (NS.GE.1) TS = TS / NS
      IF ((DOCOMP(1).GT.0) .AND. (PARMS(1).EQ.0.0D0)) PARMS(1) = TS
      TS = 0.0
      J = NG + JJC
C                                       user guess
      I = 1
      ITTER = ITTER - 1
      JD = ND
      IJ = J
      CALL ZEFUNC (JD, IJ, PARMS, FVEC, FJAC, I)
      FJAC(1) = 0.0D0
      DO 75 I = 1,ND
         FJAC(1) = FJAC(1) + FVEC(I) * FVEC(I)
 75      CONTINUE
      RMS(1) = SQRT (FJAC(1) / ND)
C                                       last solution
      I = 1
      ITTER = ITTER - 1
      JD = ND
      IJ = J
      CALL ZEFUNC (JD, IJ, RPARMS, FVEC, FJAC, I)
      FJAC(1) = 0.0D0
      DO 80 I = 1,ND
         FJAC(1) = FJAC(1) + FVEC(I) * FVEC(I)
 80      CONTINUE
      RMS(2) = SQRT (FJAC(1) / ND)
C                                       extra guess
      IF (XPARMS(3).EQ.0.0D0) THEN
         RMS(3) = 1.E10
      ELSE
         I = 1
         ITTER = ITTER - 1
         JD = ND
         IJ = J
         CALL ZEFUNC (JD, IJ, XPARMS, FVEC, FJAC, I)
         FJAC(1) = 0.0D0
         DO 85 I = 1,ND
            FJAC(1) = FJAC(1) + FVEC(I) * FVEC(I)
 85         CONTINUE
         RMS(3) = SQRT (FJAC(1) / ND)
         END IF
      IF ((RMS(3).LT.RMS(1)) .AND. (RMS(3).LT.RMS(2))) THEN
         DO 90 I = 1,MAXPRM
            RPARMS(I) = XPARMS(I)
            LLCOMP(I) = DOCOMP(I)
 90         CONTINUE
      ELSE IF (RMS(1).LT.RMS(2)) THEN
         DO 95 I = 1,MAXPRM
            RPARMS(I) = PARMS(I)
            LLCOMP(I) = DOCOMP(I)
 95         CONTINUE
         END IF
C                                       Return answers/guesses
C                                       test desire to plot
      IERR = 0
      IF (RMS(3).LT.1.E9) CALL DFILL (MAXPRM, 0.0D0, XPARMS)
C
 999  RETURN
      END
      SUBROUTINE ZEFUNC (M, N, PARMS, FVEC, FJROW, IFLAG)
C-----------------------------------------------------------------------
C   This routine is called by the Argonne package to calculate the
C   difference between the current fit and the actual data OR the
C   Jacobian for this difference.
C   Inputs:
C        M        I      Number of data points in slice (adj array dim)
C        N        I      No. of parameters (adj. array dim.;
C                        NGAUSS * 3)
C        PARMS    D(N)   parameters of gaussian components,
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
      DOUBLE PRECISION PARMS(N), FVEC(M), FJROW(N)
C
      DOUBLE PRECISION ZZ, AMP, POS, SIG, X, HALFAC, SLOPE, R, V
      INTEGER   IGAUSS, ID, I
      REAL      PIX
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'ZEMAND.INC'
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
         DO 20 ID = 1,M
            FVEC(ID) = 0.0D0
            IF ((VDATA(ID).NE.FBLANK) .AND. (IDATA(ID).NE.FBLANK)) THEN
               PIX = ID
               CALL ZEEMOD (M, PIX, PARMS, ZZ)
               IF (ZZ.NE.FBLANK) FVEC(ID) = VDATA(ID) - ZZ
               END IF
   20       CONTINUE
C                                       Calculate Jacobian.
      ELSE
         ID = IFLAG - 1
         DO 105 I = 1,MAXPRM
            FJROW(I) = 0.0D0
 105        CONTINUE
C                                       2-sided
         IF (DOGAUS.EQ.-1) THEN
            IF (IDATA(ID).EQ.FBLANK) GO TO 999
            IF (ID.EQ.1) THEN
               IF (IDATA(ID+1).EQ.FBLANK) GO TO 999
               SLOPE = IDATA(ID+1) - IDATA(ID)
            ELSE IF (ID.EQ.M) THEN
               IF (IDATA(ID-1).EQ.FBLANK) GO TO 999
               SLOPE = IDATA(ID) - IDATA(ID-1)
            ELSE
               IF (IDATA(ID+1).EQ.FBLANK) GO TO 999
               IF (IDATA(ID-1).EQ.FBLANK) GO TO 999
               SLOPE = 0.5D0 * (IDATA(ID+1) - IDATA(ID-1))
               END IF
            IF (LLCOMP(1).GT.0) FJROW(1) = IDATA(ID)
            IF (LLCOMP(2).GT.0) FJROW(2) = SLOPE
C                                       1-sided
         ELSE IF (DOGAUS.EQ.0) THEN
            IF (IDATA(ID).EQ.FBLANK) GO TO 999
            IF (ID.EQ.1) THEN
               IF (IDATA(ID+1).EQ.FBLANK) GO TO 999
               SLOPE = IDATA(ID+1) - IDATA(ID)
            ELSE
               IF (IDATA(ID-1).EQ.FBLANK) GO TO 999
               SLOPE = IDATA(ID) - IDATA(ID-1)
               END IF
            IF (LLCOMP(1).GT.0) FJROW(1) = IDATA(ID)
            IF (LLCOMP(2).GT.0) FJROW(2) = SLOPE
C                                       gaussian model
         ELSE
            IF ((LLCOMP(1).GT.0) .AND. (IDATA(ID).NE.FBLANK))
     *         FJROW(1) = IDATA(ID)
            X = ID + XBAR
            DO 110 IGAUSS = 1,MAXGAU
               IF (LLCOMP(IGAUSS+1).GT.0) THEN
                  AMP = XGAUSV(3*IGAUSS-2)
                  POS = XGAUSV(3*IGAUSS-1)
                  SIG = XGAUSV(3*IGAUSS)
                  R = (X - POS) / SIG
                  R = HALFAC * R * R
                  IF (R.LE.69.0D0) THEN
                     V = AMP * EXP (-R)
                     V = -2.D0 * V * HALFAC * (X - POS) / (SIG * SIG)
                     FJROW(IGAUSS+1) = V
                     END IF
                  END IF
 110           CONTINUE
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE ZTVINI (DOTV, IPOS, INPTS, PARMS, IERR)
C-----------------------------------------------------------------------
C   ZTVINI initializes the TV for a ZEMAN plot, plots axis labels,
C   and, if requested, plots the data.
C   Inputs:
C      IPOS     I(7)     Position in cube first point in row.
C      INPTS    I        Number of points in row.
C      NG       I        Number of Gaussians
C      PARMS    D(14)    Initial guess
C   Output:
C      FVEC     D(*)     Scratch buffer
C      FJAC     D(14)    Scratch buffer
C      IERR     I        > 0 => plot failed
C                        101 => bad initial guess
C                        102 => DIE
C-----------------------------------------------------------------------
      REAL      DOTV
      INTEGER   IPOS(7), INPTS, IERR
      DOUBLE PRECISION PARMS(*)
C
      CHARACTER TEMP*1, MSGBUF*132, FIRSTC*1
      REAL      XFAC
      INTEGER   I, JERR, SCRTCH(256)
      LOGICAL   T, F, FIRST
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'ZEMAND.INC'
      INCLUDE 'ZEMANO.INC'
      SAVE FIRST
      DATA T, F, FIRST /.TRUE.,.FALSE.,.TRUE./
C-----------------------------------------------------------------------
C      CALL YHOLD ('ONNN', IERR)
      IF (FIRST) THEN
         IGR1 = 1
         IGR2 = 2
         IGR3 = 3
         IGR4 = 4
         IGR1 = IGR1 + NGRAY
         IGR2 = IGR2 + NGRAY
         IGR3 = IGR3 + NGRAY
         IGR4 = IGR4 + NGRAY
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
         END IF
C                                       Default: actual range
      ORANGE(1,1) = 1.0E10
      ORANGE(2,1) = -ORANGE(1,1)
      ORANGE(1,2) = ORANGE(1,1)
      ORANGE(2,2) = -ORANGE(1,1)
      DO 15 I = 1,INPTS
         IF (VDATA(I).NE.FBLANK) THEN
            IF (VDATA(I).LT.ORANGE(1,1)) ORANGE(1,1) = VDATA(I)
            IF (VDATA(I).GT.ORANGE(2,1)) ORANGE(2,1) = VDATA(I)
            END IF
         IF (IDATA(I).NE.FBLANK) THEN
            IF (IDATA(I).LT.ORANGE(1,2)) ORANGE(1,2) = IDATA(I)
            IF (IDATA(I).GT.ORANGE(2,2)) ORANGE(2,2) = IDATA(I)
            END IF
 15      CONTINUE
      XFAC = ORANGE(2,1) - ORANGE(1,1)
      ORANGE(2,1) = ORANGE(2,1) + 0.10 * XFAC
      ORANGE(1,1) = ORANGE(1,1) - 0.10 * XFAC
      XFAC = ORANGE(2,2) - ORANGE(1,2)
      ORANGE(2,2) = ORANGE(2,2) + 0.10 * XFAC
      ORANGE(1,2) = ORANGE(1,2) - 0.10 * XFAC
      XFAC = INPTS - 1
      XRANGE(1) = 1.0 - 0.05 * XFAC
      XRANGE(2) = INPTS + 0.05 * XFAC
C                                       do the plot
      CALL YZERO (IGR1, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (IGR2.NE.IGR1) THEN
         CALL YZERO (IGR2, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YZERO (IGR3, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YZERO (IGR4, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      CALL COPY (256, CATOLD(1,1), CATBLK)
      CALL ZTVPLT (INPTS, IPOS, PARMS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Talk to user
      IF (TTYIND.LE.0) THEN
         CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, F, T, T, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1900) JERR
            CALL MSGWRT (6)
            GO TO 999
            END IF
         END IF
      IF (DOTV.GT.1.5) THEN
         CALL ZEMEN1 (MSGBUF, SCRTCH, JERR)
      ELSE
         WRITE (MSGBUF,1910)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, JERR)
         IF (JERR.NE.0) GO TO 920
         MSGBUF = '??? hit return or anything else to procede'
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, JERR)
         IF (JERR.NE.0) GO TO 920
         CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, JERR)
         END IF
      IF (JERR.NE.0) GO TO 920
      TEMP = FIRSTC (MSGBUF)
      IF (TEMP.EQ.'B') THEN
         IERR = 101
      ELSE IF (TEMP.EQ.'Q') THEN
         IERR = 102
         END IF
      GO TO 990
C                                       TTY error
 920  WRITE (MSGTXT,1920) JERR
      CALL MSGWRT (6)
 990  IF (TTYIND.GT.0) THEN
         CALL ZCLOSE (TTYLUN, TTYIND, JERR)
         TTYIND = 0
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('OPEN TERMINAL ERROR',I7)
 1910 FORMAT ('??? ''BAD'' to flag row,',' ''Q'' to quit')
 1920 FORMAT ('TERMINAL I/O ERROR',I7)
      END
      SUBROUTINE ZEMEN1 (MSGBUF, SCRTCH, JERR)
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
      LOGICAL   LEAVE(5)
      CHARACTER CHOICS(5)*8, TITLE*8, ISHELP*6
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA CHOICS /'DO FIT', ' ', 'BAD', ' ', 'QUIT'/
      DATA LEAVE /.FALSE., .TRUE., .FALSE., .TRUE., .FALSE./
      DATA ISHELP /'ZEMAN'/
C-----------------------------------------------------------------------
      GRCHS(1) = 6
      GRCHS(2) = 5
      MTYPE = 1
      NCOL = 1
      NROWS = 5
      SIDSEP = 6 * CSIZTV(1)
      TOPSEP = 7 * CSIZTV(2)
      NTITLE = 0
      TITLE = ' '
      TIMLIM = 0
      MSGBUF = ' '
C                                       menu
 20   CALL TVMENU (MTYPE, NCOL, NROWS, GRCHS, TOPSEP, SIDSEP, ISHELP,
     *   CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHOICE, TVBUTT, SCRTCH,
     *   JERR)
      IF (JERR.NE.0) THEN
         MSGTXT = 'ZEMEN1: ERROR FROM TV MENU'
         CALL MSGWRT (7)
      ELSE IF (CHOICS(CHOICE).EQ.' ') THEN
         GO TO 20
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
      SUBROUTINE ZTVPLT (INPTS, IPOS, PARMS, IERR)
C-----------------------------------------------------------------------
C   Does the data plot - I above, V below
C   Inputs:
C      DOREIM   L        T => plot Q, U; F => convert tp amp/phase
C   Common: RMFITD.INC
C   Common in/out
C      CATBLK   I(*)     modified for TV catalog
C   Outputs:
C      IERR     I        error code
C-----------------------------------------------------------------------
      INTEGER   INPTS, IPOS(7), IERR
      DOUBLE PRECISION PARMS(*)
C
      INCLUDE 'ZEMAND.INC'
      INCLUDE 'ZEMANO.INC'
      CHARACTER PREF*8, XPREF*8, TEXT*132, CTEMP1*18
      INTEGER   TVWIND(4), TVSIZE(2), INCHAR, I, INP, JTRIM, K, LTYPE,
     *   PLPTS
      REAL      XYRATI, BLC(2), TRC(2), CHOUT(4), ATEMP, YMULT, XMULT,
     *   LINT, DX, DY, XBLC(2), XTRC(2), XP, YP, DP
      DOUBLE PRECISION QQ, UU
      LOGICAL   PFLG
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      CALL YWINDO ('READ', TVWIND, IERR)
      IF (IERR.NE.0) THEN
         TVWIND(1) = 1
         TVWIND(2) = 1
         TVWIND(3) = MAXXTV(1)
         TVWIND(4) = MAXXTV(2)
         END IF
      TVSIZE(1) = TVWIND(3) - TVWIND(1) + 1
      TVSIZE(2) = TVWIND(4) - TVWIND(2) + 1
      XYRATI = (WINDTV(3)-WINDTV(1)+1.0) / (WINDTV(4)-WINDTV(2)+1.0)
C                                       Initialize for plotting
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      CALL RFILL (4, 0.5, CHOUT)
C                                       labeling commons
      LOCNUM = 1
      CALL SETLOC (PLPOS(3), .FALSE.)
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      AXFUNC(1,LOCNUM) = 0
      AXFUNC(2,LOCNUM) = 0
      RPLOC(1,LOCNUM) = 0.5
      RPLOC(2,LOCNUM) = 0.5
C                                       I and V
      LINT = 600.
      ATEMP = ORANGE(2,1) - ORANGE(1,1)
      YMULT = ATEMP
      CALL METSCA (ATEMP, PREF, PFLG)
      YMULT = ATEMP / YMULT
      CTYP(2,LOCNUM) = 'V Jy/bem'
      CPREF(2,LOCNUM) = PREF
      RPVAL(2,LOCNUM) = ORANGE(1,1) * YMULT
      AXINC(2,LOCNUM) = (ORANGE(2,1) - ORANGE(1,1)) * YMULT /
     *   (LINT-1.0)
      ATEMP = XRANGE(2) - XRANGE(1)
      XMULT = ATEMP
      CALL METSCA (ATEMP, XPREF, PFLG)
      XMULT = ATEMP / XMULT
      RPVAL(1,LOCNUM) = XRANGE(1) * XMULT
      AXINC(1,LOCNUM) = (XRANGE(2) - XRANGE(1)) * XMULT / 999.0
      TEXT = 'V and I spectrum'
      CTYP(1,LOCNUM) = 'channel'
      CPREF(1,LOCNUM) = XPREF
C                                       lower plot
      XBLC(1) = BLC(1)
      XTRC(1) = TRC(1)
      XBLC(2) = BLC(2)
      XTRC(2) = LINT
C                                        Set text borders at L, B,
C                                        R & T in characters
      CALL CHNTIC (BLC, TRC, INP)
      INP = MAX (INP, 4)
      CHOUT(1) = INP + 4
      CHOUT(2) = 3.333
      CHOUT(4) = 2.0
C                                        Init. for line drawing
      CALL GINITL (BLC, TRC, XYRATI, CHOUT, PLPOS(3), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                        Draw the box
      CALL GLTYPE (1, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GPOS (BLC(1), BLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (TRC(1), BLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (TRC(1), TRC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (BLC(1), TRC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (BLC(1), BLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GPOS (BLC(1), LINT, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (TRC(1), LINT, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GPOS (BLC(1), TRC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      DX = 0.0
      DY = 0.333
      INCHAR = JTRIM (TEXT)
      TEXT(INCHAR+1:) = '____'
      INCHAR = INCHAR + 5
      CALL H2CHR (18, 1, CATH(KHIMN), CTEMP1)
      CALL NAMEST (CTEMP1, CATBLK(KIIMS), TEXT(INCHAR:), INCHAR)
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       label bottom plot
      LTYPE = 3
      CALL CLAB1 (XBLC, XTRC, CHOUT, LTYPE, XYRATI, .FALSE., PLTBLK,
     *   IERR)
      IF (IERR.NE.0) GO TO 980
C                                       pixel coordinates
      XP = TRC(1)
      YP = LINT
      CALL GPOS (XP, YP, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      DX = -9.5
      DY = -2.5
      WRITE (CTEMP1,1060) IPOS(2)
      CALL GCHAR (7, 0, DX, DY, CTEMP1, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      DY = DY - 1.5
      WRITE (CTEMP1,1061) IPOS(3)
      CALL GCHAR (7, 0, DX, DY, CTEMP1, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       top plot
      XBLC(2) = LINT
      XTRC(2) = TRC(2)
      RPLOC(2,LOCNUM) = LINT
C                                       I
      ATEMP = ORANGE(2,2) - ORANGE(1,2)
      YMULT = ATEMP
      CALL METSCA (ATEMP, PREF, PFLG)
      YMULT = ATEMP / YMULT
      CTYP(2,LOCNUM) = 'I Jy/bem'
      CPREF(2,LOCNUM) = PREF
      RPVAL(2,LOCNUM) = ORANGE(1,2) * YMULT
      AXINC(2,LOCNUM) = (ORANGE(2,2) - ORANGE(1,2)) * YMULT /
     *   (TRC(2)-LINT-1.0)
      CPREF(1,LOCNUM) = ' '
      CTYP(1,LOCNUM) = ' '
      LTYPE = 3
      CALL CLAB1 (XBLC, XTRC, CHOUT, LTYPE, XYRATI, .FALSE., PLTBLK,
     *   IERR)
      IF (IERR.NE.0) GO TO 980
C                                       now plot data
C                                       V
      CALL GLTYPE (1, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      PFLG = .TRUE.
      DO 110 I = 1,INPTS
         IF ((VDATA(I).NE.FBLANK) .AND. (VDATA(I).GE.ORANGE(1,1))
     *      .AND. (VDATA(I).LE.ORANGE(2,1))) THEN
            XP = 999.0 * (I - XRANGE(1)) /
     *         (XRANGE(2) - XRANGE(1)) + 0.5
            YP = (LINT - 1.0) * (VDATA(I) - ORANGE(1,1)) /
     *         (ORANGE(2,1) - ORANGE(1,1)) + 0.5
            IF (PFLG) THEN
               CALL GPOS (XP, YP, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               PFLG = .FALSE.
            ELSE
               CALL GVEC (XP, YP, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
         ELSE
            PFLG = .TRUE.
            END IF
 110     CONTINUE
C                                       I
      PFLG = .TRUE.
      DO 120 I = 1,INPTS
         IF ((IDATA(I).NE.FBLANK) .AND. (IDATA(I).GE.ORANGE(1,2))
     *      .AND. (IDATA(I).LE.ORANGE(2,2))) THEN
            XP = 999.0 * (I - XRANGE(1)) /
     *         (XRANGE(2) - XRANGE(1)) + 0.5
            YP = (999.0 - LINT) * (IDATA(I) - ORANGE(1,2)) /
     *         (ORANGE(2,2) - ORANGE(1,2)) + LINT + 0.5
            IF (PFLG) THEN
               CALL GPOS (XP, YP, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               PFLG = .FALSE.
            ELSE
               CALL GVEC (XP, YP, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
         ELSE
            PFLG = .TRUE.
            END IF
 120     CONTINUE
C                                       now plot current V guess
      CALL GLTYPE (2, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      PFLG = .TRUE.
      DO 130 I = 1,INPTS
         DP = I
         XP = 999.0 * (DP - XRANGE(1)) / (XRANGE(2) - XRANGE(1)) + 0.5
         CALL ZEEMOD (INPTS, DP, PARMS, QQ)
         IF ((QQ.NE.FBLANK) .AND. (QQ.GE.ORANGE(1,1)) .AND.
     *      (QQ.LE.ORANGE(2,1))) THEN
            YP = (LINT - 1.0) * (QQ - ORANGE(1,1)) /
     *         (ORANGE(2,1) - ORANGE(1,1)) + 0.5
            IF (PFLG) THEN
               CALL GPOS (XP, YP, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
            ELSE
               CALL GVEC (XP, YP, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            PFLG = .FALSE.
         ELSE
            PFLG = .TRUE.
            END IF
 130     CONTINUE
C                                       Gaussians in I
      IF (DOGAUS.GT.0) THEN
         PLPTS = MAX (1000., 1.3*INPTS)
         DX = (XRANGE(2) - XRANGE(1)) / (PLPTS - 1.0)
         DP = -DX + XRANGE(1)
         PFLG = .TRUE.
         DO 140 I = 1,PLPTS
            DP = DP + DX
            XP = 999.0 * (DP - XRANGE(1)) /
     *         (XRANGE(2) - XRANGE(1)) + 0.5
            IF ((DP.GE.XRANGE(1)) .AND. (DP.LE.XRANGE(2))) THEN
               CALL GAUMOD (DP, UU)
               IF ((UU.NE.FBLANK) .AND. (UU.GE.ORANGE(1,2)) .AND.
     *            (UU.LE.ORANGE(2,2))) THEN
                  YP = (999.0 - LINT) * (UU - ORANGE(1,2)) /
     *               (ORANGE(2,2) - ORANGE(1,2)) + LINT + 0.5
                  IF (PFLG) THEN
                     CALL GPOS (XP, YP, PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 980
                  ELSE
                     CALL GVEC (XP, YP, PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 980
                     END IF
                  PFLG = .FALSE.
               ELSE
                  PFLG = .TRUE.
                  END IF
            ELSE
               PFLG = .TRUE.
               END IF
 140        CONTINUE
         END IF
C
 980  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1980) IERR
         CALL MSGWRT (8)
         END IF
      CALL YHOLD ('OFFF', K)
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('Y=',I5)
 1061 FORMAT ('Z=',I5)
 1980 FORMAT ('ZTVPLT: ERROR',I4,' FROM TV PLOT ROUTINES')
      END
      SUBROUTINE ZEEMOD (M, X, PARMS, ZEVAL)
C-----------------------------------------------------------------------
C   ZEEMOD evaluates the Zeman model at 1 pixel
C   Inputs:
C      M       I      Number data points in row
C      X       R      pixel value
C      PARMS   D(*)   current parameters of model
C   Outputs
C      ZEVAL   D      Model value
C-----------------------------------------------------------------------
      INTEGER   M
      REAL      X
      DOUBLE PRECISION PARMS(*), ZEVAL
C
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMAND.INC'
      INTEGER   I, J
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       2-sided
      I = X + 0.5
      ZEVAL = FBLANK
      IF ((I.GE.1) .AND. (I.LE.M)) THEN
         IF (PARMS(1).NE.FBLANK) ZEVAL = PARMS(1) * IDATA(I)
         DO 10 J = 1,NGAUSS
            IF ((PARMS(J+1).NE.FBLANK) .AND. (SLOPES(I,J).NE.FBLANK))
     *         THEN
               IF (ZEVAL.EQ.FBLANK) ZEVAL = 0.0D0
               ZEVAL = ZEVAL + PARMS(J+1) * SLOPES(I,J)
               END IF
 10         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE GAUMOD (X, GAVAL)
C-----------------------------------------------------------------------
C   GAUMOD evaluates the I Gaussian model at a pixel
C   Inputs:
C      X       R   Pixel
C   Outputs:
C      GAVAL   D   Gaussian model at that pixel
C-----------------------------------------------------------------------
      REAL      X
      DOUBLE PRECISION GAVAL
C
      INCLUDE 'ZEMAND.INC'
      INTEGER   J
      DOUBLE PRECISION AMP, POS, SIG, HALFAC, R
      INCLUDE 'INCS:DDCH.INC'
      DATA HALFAC /2.77258872D0/
C-----------------------------------------------------------------------
      GAVAL = 0.0D0
      IF (XGAUSB(1).NE.FBLANK) GAVAL = XGAUSB(1)
      IF (XGAUSB(2).NE.FBLANK) GAVAL = GAVAL + XGAUSB(2) * (X + XBAR)
      DO 20 J = 1,MAXGAU
         AMP = XGAUSV(3*J-2)
         IF ((AMP.NE.FBLANK) .AND. (AMP.NE.0.0)) THEN
            POS = XGAUSV(3*J-1)
            SIG = XGAUSV(3*J)
            R = (X + XBAR - POS) / SIG
            R = HALFAC * R * R
            IF (R.LE.69.0D0) GAVAL = GAVAL + AMP * EXP (-R)
            END IF
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ZTVMOD (DOTV, INPTS, IPOS, NG, FVEC, PARMS, PERR, IERR)
C-----------------------------------------------------------------------
C   ZTVMOD plots the residual and model functions on the TV.  It asks
C   the user for permission to proceed.
C   Inputs:
C      INPTS    I      Number of data points
C      IPOS     I(*)   pixel position in cube
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
C-----------------------------------------------------------------------
      REAL      DOTV
      INTEGER   INPTS, IPOS(*), NG, PERR, IERR
      DOUBLE PRECISION FVEC(*), PARMS(*)
C
      CHARACTER TEMP*1, MSGBUF*80, FIRSTC*1
      INTEGER   I, JERR, SCRTCH(256)
      REAL      X, Y, LINT, DP
      LOGICAL   BLAST, T, F
      DOUBLE PRECISION DEBUG(14), YY
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'ZEMAND.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
 10   IERR = 0
      CALL DPCOPY (14, PARMS, DEBUG)
      LINT = 600.0
C                                       Plot model
      IGR = IGR4
      BLAST = .TRUE.
      CALL GLTYPE (4, PLTBLK, IERR)
      IF (IGR.NE.IGR1) THEN
         CALL YZERO (IGR, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      DO 60 I = 1,INPTS
         IF (VDATA(I).EQ.FBLANK) THEN
            BLAST = .TRUE.
         ELSE
            X = 999.0 * (I - XRANGE(1)) / (XRANGE(2) - XRANGE(1)) + 0.5
            IF ((I.GE.XRANGE(1)) .AND. (I.LE.XRANGE(2))) THEN
               DP = I
               CALL ZEEMOD (INPTS, DP, PARMS, YY)
C               Y = VDATA(I) - FVEC(I)
               IF ((YY.NE.FBLANK) .AND. (YY.GE.ORANGE(1,1)) .AND.
     *            (YY.LE.ORANGE(2,1))) THEN
                  Y = (LINT - 1.0) * (YY - ORANGE(1,1)) /
     *               (ORANGE(2,1) - ORANGE(1,1)) + 0.5
                  IF (BLAST) THEN
                     CALL GPOS (X, Y, PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 900
                     BLAST = .FALSE.
                  ELSE
                     CALL GVEC (X, Y, PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 900
                     END IF
               ELSE
                  BLAST = .TRUE.
                  END IF
            ELSE
               BLAST = .TRUE.
               END IF
            END IF
 60      CONTINUE
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
      WRITE (MSGBUF,1911) THERMS, IPOS(2), IPOS(3)
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      WRITE (MSGBUF,1912) (PARMS(I), I = 1,NG+1)
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (DOTV.GT.1.5) THEN
         CALL ZEMEN2 (MSGBUF, SCRTCH, JERR)
      ELSE
         MSGBUF = '**** Enter HAND to enter new numbers by hand'
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         WRITE (MSGBUF,1920)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         WRITE (MSGBUF,1921)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         END IF
      IF (IERR.NE.0) GO TO 980
      TEMP = FIRSTC (MSGBUF)
      IF (TEMP.EQ.'B') THEN
         IERR = 101
      ELSE IF (TEMP.EQ.'Q') THEN
         IERR = 102
      ELSE IF (TEMP.EQ.'T') THEN
         IERR = 104
      ELSE IF (TEMP.EQ.'H') THEN
         CALL ZEHAND (MSGBUF, INPTS, PARMS, FVEC, IERR)
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
 1900 FORMAT ('OPEN TERMINAL ERROR',I7)
 1910 FORMAT ('>>>> PARAMETERS SEEM OUT OF RANGE.  SOLUTION PROBABLY ',
     *   'BAD! <<<<')
 1911 FORMAT ('RMS =',F12.6,'   at Y=',I5,'   Z=',I5)
 1912 FORMAT ('P=   ',5(1PE12.4))
 1920 FORMAT ('**** enter BAD to flag solution, QUIT to die, ',
     *   'TVOFF to stop TV display')
 1921 FORMAT ('     or hit return to continue')
 1980 FORMAT ('TERMINAL I/O ERROR',I7)
      END
      SUBROUTINE ZEMEN2 (MSGBUF, SCRTCH, JERR)
C-----------------------------------------------------------------------
C   Does a TV menu for post-fit stage of user questions
C   Inputs:
C      NG       I       Number Gaussians max
C   Outputs:
C      MSGBUF   C*(*)   answer: E, B, Q, other
C      JERR     I       error
C-----------------------------------------------------------------------
      INTEGER   SCRTCH(*), JERR
      CHARACTER MSGBUF*(*)
C
      INTEGER   MTYPE, NCOL, NROWS, GRCHS(2), TOPSEP, SIDSEP, TIMLIM,
     *   NTITLE, TVBUTT, CHOICE
      LOGICAL   LEAVE(7)
      CHARACTER CHOICS(7)*8, TITLE*8, ISHELP*6
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA CHOICS /'GOOD', ' ', 'TVOFF', 'HAND', 'BAD', ' ', 'QUIT'/
      DATA LEAVE /.FALSE., .TRUE., 3*.FALSE., .TRUE., .FALSE./
      DATA ISHELP /'ZEMAN'/
C-----------------------------------------------------------------------
      NROWS = 7
      GRCHS(1) = 6
      GRCHS(2) = 5
      MTYPE = 1
      NCOL = 1
      SIDSEP = 6 * CSIZTV(1)
      TOPSEP = 7 * CSIZTV(2)
      NTITLE = 0
      TITLE = ' '
      TIMLIM = 0
      MSGBUF = ' '
C                                       menu
 20   CALL TVMENU (MTYPE, NCOL, NROWS, GRCHS, TOPSEP, SIDSEP, ISHELP,
     *   CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHOICE, TVBUTT, SCRTCH,
     *   JERR)
      IF (JERR.NE.0) THEN
         MSGTXT = 'ZEMEN2: ERROR FROM TV MENU'
         CALL MSGWRT (7)
      ELSE IF (CHOICS(CHOICE).EQ.' ') THEN
         GO TO 20
      ELSE
         MSGBUF = CHOICS(CHOICE)(:1)
         END IF
C
 999  RETURN
      END
      SUBROUTINE ZMANCH (INPTS, FVEC, PARMS, IERR)
C-----------------------------------------------------------------------
C   ZMANCH checks the answers for being at least vaguely reasonable.
C   Inputs:
C      INPTS   I      Number of data samples
C      FVEC    D(*)   residuals
C      PARMS   D(*)   Answers
C   Output:
C      IERR    I   0 -> all in range, 1 -> some not
C-----------------------------------------------------------------------
      INTEGER   INPTS, IERR
      DOUBLE PRECISION FVEC(*), PARMS(*)
C
      REAL      X
      INTEGER   IDBG, I
      DOUBLE PRECISION RMS
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMAND.INC'
C-----------------------------------------------------------------------
      RMS = 0.0D0
      DO 10 I = 1,INPTS
         RMS = RMS + FVEC(I)*FVEC(I)
 10      CONTINUE
      IF (INPTS.GT.0) RMS = SQRT (RMS/INPTS)
      THERMS = RMS
      IERR = 1
      IF (RMS.GT.RMSLIM) THEN
         WRITE (MSGTXT,1010) RMS, RMSLIM
         CALL MSGWRT (6)
         GO TO 999
         END IF
      IF (LLCOMP(1).GT.0) THEN
         IDBG = 1
         IF (ABS(PARMS(1)).GT.0.25) GO TO 990
         END IF
      IERR = 0
      GO TO 999
C
 990  WRITE (MSGTXT,1990) IDBG, X
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ZMANCH: RMS',1PE11.3,' > LIMIT',1PE11.3)
 1990 FORMAT ('ZMANCH PARAMETER',I3,' VALUE',1PE13.5,' OUT OF RANGE')
      END
      SUBROUTINE ZEHAND (MSGBUF, INPTS, PARMS, FVEC, IERR)
C-----------------------------------------------------------------------
C   Enter guesses as hand numbers
C   Inputs:
C      INPTS    I       Number points in FVEC
C   In/out:
C      MSGBUF   C*(*)   Message buffer
C      PARMS    D(*)    Parameters
C      FVEC     D(*)    Residuals
C   Output:
C      IERR     I       0 => carry on
C-----------------------------------------------------------------------
      INTEGER   INPTS, IERR
      CHARACTER MSGBUF*(*)
      DOUBLE PRECISION PARMS(*), FVEC(*)
C
      INCLUDE 'ZEMAND.INC'
      INTEGER   I, K, JTRIM, KBP, MP
      DOUBLE PRECISION XX, OPARMS(MAXPRM), FJAC(MAXPRM,MAXPRM)
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      CALL DPCOPY (MAXPRM, PARMS, OPARMS)
C                                       gain
      WRITE (MSGBUF,1000)
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      K = JTRIM (MSGBUF)
      KBP = 1
      CALL GETNUM (MSGBUF, K, KBP, XX)
      IF (XX.EQ.DBLANK) GO TO 900
      PARMS(1) = XX
C                                       single avg field
      IF (DOGAUS.LE.0) THEN
         MP = JJC + 1
         WRITE (MSGBUF,1001)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         K = JTRIM (MSGBUF)
         KBP = 1
         CALL GETNUM (MSGBUF, K, KBP, XX)
         IF (XX.EQ.DBLANK) GO TO 900
         PARMS(2) = XX
C                                       Gaussian model
      ELSE
         MP = JJC + NGA
         DO 20 I = 1,NGA
            WRITE (MSGBUF,1010) I
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            IF (IERR.NE.0) GO TO 999
            K = JTRIM (MSGBUF)
            KBP = 1
            CALL GETNUM (MSGBUF, K, KBP, XX)
            IF (XX.EQ.DBLANK) GO TO 900
            PARMS(1+I) = XX
 20         CONTINUE
         END IF
C                                       evaluate residuals
      I = 1
      ITTER = 0
      NITTER = 100
      CALL ZEFUNC (INPTS, MP, PARMS, FVEC, FJAC, I)
      GO TO 999
C                                       bad value quiet exit
 900  IERR = -1
      CALL DPCOPY (MAXPRM, OPARMS, PARMS)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Enter Gain value')
 1001 FORMAT ('Enter avg Field in channels')
 1010 FORMAT ('Enter Gaussian',I2,' Field in channels')
      END
      SUBROUTINE ZMANTV (IRET)
C-----------------------------------------------------------------------
C   ZMANTV implements a TV menu driven method to manipulate the
C   Gaussian fits results in order to re-try fits, rearrange fit
C   components, etc.
C   Outputs:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMAND.INC'
      INTEGER   NOPT1, NOPT2, NOPTS
      PARAMETER (NOPT1=17)
      PARAMETER (NOPT2=2+2*MAXGAU)
      PARAMETER (NOPTS=NOPT1+NOPT2)
C
      INTEGER   MTYPE, MCOL, MROWS(2), GRCHS(2), TIMLIM, CHS, TVBUT,
     *   NX, NY, NP, I, J,IPOS(2), NWORDS, TOPSEP, IP, NG, I1, I2,
     *   JTRIM, IC, ICOLOR, NLEVS, JJ, II, IBUFF1(MABFSS), IG, SIDSEP
      CHARACTER CHOIC1(NOPT1)*16, CHOIC2(NOPT2)*16, CHOICS(NOPTS)*16,
     *   ISHELP*6, TITLE(1+MAXGAU)*128, MSGBUF*72
      LOGICAL   IMGOK, LEAVE1(NOPT1), LEAVE2(NOPT2), LEAVE(NOPTS), DOIT
      LONGINT   PIMAGE
      REAL      IMAGES(2), MAXRES, MAXGN(2), MAXFLD(2,MAXGAU),
     *   MAXDFL(MAXGAU), SLOPE, OFFS
      DOUBLE PRECISION XX(8)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (IBUFF1, BUFF1)
      DATA MTYPE, TIMLIM, TOPSEP, SIDSEP, GRCHS /-1, 0, 3, 10, 2, 1/
      DATA MCOL /2/
      DATA LEAVE1 /.FALSE., 5*.TRUE., 2*.FALSE., 7*.TRUE., 2*.FALSE./
      DATA CHOIC1 /'EXIT', ' ', 'SET MAX RES', 'SET GAIN RANGE',
     *   'SET FIELD RANGE', 'SET MAX ERR FLD', 'REDO ALL', 'FLAG ALL',
     *   ' ', 'OFF ZOOM', 'OFF TRANSFER', 'SET DOOUTPUT', ' ',
     *   'ADD TO LIST', 'SHOW LIST', 'REDO LIST', 'FLAG LIST'/
C-----------------------------------------------------------------------
      IGR1 = 1
      IGR2 = 2
      IGR3 = 3
      IGR4 = 4
      IGR5 = 5
      IGR1 = IGR1 + NGRAY
      IGR2 = IGR2 + NGRAY
      IGR3 = IGR3 + NGRAY
      IGR4 = IGR4 + NGRAY
      IGR5 = IGR5 + NGRAY
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
      DO 10 I = 1,4*MAXGAU
         FUNCTY(I) = ' '
 10      CONTINUE
C                                       prepare menu
      J = 1
      LEAVE2(1) = .FALSE.
      CHOIC2(1) = 'SHOW IMAGE G'
      LEAVE2(2) = .FALSE.
      CHOIC2(2) = 'SHOW IMAGE EG'
      J = 2
      DO 15 I = 1,MAXGAU
         LEAVE2(J+1) = .FALSE.
         LEAVE2(J+2) = .FALSE.
         WRITE (CHOIC2(J+1),1015) 'F', I
         WRITE (CHOIC2(J+2),1015) 'EF', I
         J = J + 2
 15      CONTINUE
      ISHELP = TSKNAM
      IMGOK = .FALSE.
      NG = NGAUSS
      NP = 2 + NMXIMG * NG
      MROWS(1) = NOPT1
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
      I = BLC(2) + 0.1
      J = TRC(2) + 0.1
      NX = (J - I) + 1
      I = BLC(3) + 0.1
      J = TRC(3) + 0.1
      NY = (J - I) + 1
      MAXRES = 0.0
      CALL RFILL (2, 0.0, MAXGN)
      CALL RFILL (2*MAXGAU, 0.0, MAXFLD)
      NLIST = 0
      CALL RFILL (MAXGAU, 0.0, MAXDFL)
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
         END IF
C                                       menu selection
 50   IF (NG.EQ.1) THEN
         WRITE (TITLE(1),1050) DOCAT, MAXRES, MAXGN, MAXFLD(1,1),
     *      MAXFLD(2,1), MAXDFL(1)
         J = 1
      ELSE
         WRITE (TITLE(1),1051) DOCAT, MAXRES, MAXGN
         J = NG + 1
         END IF
      DO 51 I = 1,NG
         WRITE (TITLE(I+1),1052) I, MAXFLD(1,I), MAXFLD(2,I), MAXDFL(I)
 51      CONTINUE
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
C                                       max residual
      ELSE IF (CHOICS(CHS).EQ.'SET MAX RES') THEN
         MSGBUF = 'Enter maximum residual in image units'
         CALL INQFLT (TTYLUN, MSGBUF, 1, XX, IRET)
         IF (IRET.EQ.0) MAXRES = XX(1)
C                                       max gain range
      ELSE IF (CHOICS(CHS).EQ.'SET GAIN RANGE') THEN
         MSGBUF = 'Enter min and max gain'
         CALL INQFLT (TTYLUN, MSGBUF, 2, XX, IRET)
         IF (IRET.EQ.0) THEN
            MAXGN(1) = XX(1)
            MAXGN(2) = XX(2)
            END IF
C                                       width ranges
      ELSE IF (CHOICS(CHS).EQ.'SET FIELD RANGE') THEN
         MSGBUF = 'Enter min and max field in pixels' //
     *      ' each Gaussian'
         CALL INQFLT (TTYLUN, MSGBUF, 2*NG, XX, IRET)
         IF (IRET.EQ.0) THEN
            DO 56 I = 1,NG
               MAXFLD(1,I) = XX(2*I-1)
               MAXFLD(2,I) = XX(2*I)
 56            CONTINUE
            END IF
C                                       max width error
      ELSE IF (CHOICS(CHS).EQ.'SET MAX ERR FLD') THEN
         MSGBUF = 'Enter max field error in pixels' //
     *      ' each Gaussian'
         CALL INQFLT (TTYLUN, MSGBUF, NG, XX, IRET)
         IF (IRET.EQ.0) THEN
            DO 57 I = 1,NG
               MAXDFL(I) = XX(I)
 57            CONTINUE
            END IF
C                                       redo all
      ELSE IF (CHOICS(CHS).EQ.'REDO ALL') THEN
         DOIT = MAXRES.GT.0.0
         IF (MAXGN(1).LT.MAXGN(2)) DOIT = .TRUE.
         DO 58 IG = 1,NG
            IF (MAXDFL(IG).GT.0.0) DOIT = .TRUE.
            IF (MAXFLD(1,IG).LT.MAXFLD(2,IG)) DOIT = .TRUE.
 58         CONTINUE
         IF (.NOT.DOIT) THEN
            MSGTXT = 'Max residual, max gain, max field, max error'
     *         // ' must be set'
            CALL MSGWRT (6)
         ELSE
            CALL YZERO (IGR5, IRET)
            CALL UPDALL ('REDO', MAXRES, MAXGN, MAXFLD, MAXDFL, NX, NY,
     *         IMAGES(PIMAGE+1), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RE-DOING ALL'
               GO TO 990
               END IF
            END IF
C                                       flag all
      ELSE IF (CHOICS(CHS).EQ.'FLAG ALL') THEN
         DOIT = MAXRES.GT.0.0
         IF (MAXGN(1).LT.MAXGN(2)) DOIT = .TRUE.
         DO 59 IG = 1,NG
            IF (MAXDFL(IG).GT.0.0) DOIT = .TRUE.
            IF (MAXFLD(1,IG).LT.MAXFLD(2,IG)) DOIT = .TRUE.
 59         CONTINUE
        IF (.NOT.DOIT) THEN
            MSGTXT = 'Max residual, max gain, max field, max error'
     *         // ' must be set'
            CALL MSGWRT (6)
         ELSE
            CALL UPDALL ('FLAG', MAXRES, MAXGN, MAXFLD, MAXDFL, NX, NY,
     *         IMAGES(PIMAGE+1), IRET)
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
         IF (DOCAT.GT.7) DOCAT = 0
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
               WRITE (MSGTXT,1054) (PIXLIS(1,I), PIXLIS(2,I), I = I1,I2)
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
            CALL UPDLIS ('REDO', NX, NY, IMAGES(PIMAGE+1), IRET)
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
C                                       display image
      ELSE
         IP = CHS - MROWS(1)
         CALL SHOIMG (IP, NX, NY, IGR5, IMAGES(PIMAGE+1), IRET)
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
 1000 FORMAT ('ZMANTV: ERROR',I4,' ON ',A)
 1015 FORMAT ('SHOW IMAGE ',A,I1)
 1050 FORMAT ('DOOUT=',I1,2X,'MAXRES=',F8.5,2X,F7.3,' <GAIN< ',F7.3,2X,
     *   'GAUSS 1:',F7.3,' < FIELD <',F7.3,2X,'MAXDFLD=',F7.3)
 1051 FORMAT ('DOOUT=',I1,2X,'MAXRES=',F8.5,2X,F7.3,' <GAIN< ',F7.3)
 1052 FORMAT ('GAUSS',I2,F7.3,' < FIELD <',F7.3,2X,'MAXDFLD=',F7.3)
 1165 FORMAT ('POSITION',2I6,' NOT FOUND IN THE PIXEL LIST')
 1265 FORMAT ('POSITION',2I6,' OUTSIDE 1-',I5,' 1-',I5)
 1054 FORMAT (4('(',I5,',',I5,')',3X))
      END
      SUBROUTINE GETIMG (NX, NY, NP, IMAGE, IRET)
C-----------------------------------------------------------------------
C   GETIMG reads the ZE file and makes images of the parameters
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
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMANO.INC'
      INTEGER   I1, I2, IP, MP, YZPOS(2), I, NAX, J, K, NGA
      REAL      VPEAK, RESULT(MAXPRM,2), XGAUSV(3*MAXGAU), XGAUSB(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'Reading ZE table to fill images'
      CALL MSGWRT (2)
C                                       blank fill
      I1 = NX * NY * NP
      CALL RFILL (I1, FBLANK, IMAGE)
C                                       blotch plane
      I1 = NX * NY
      CALL RFILL (I1, 0.0, IMAGE(1,1,NP+1))
C                                       loop through table
      MP = NP / 2 - 1
      IZERNO = 1
      DO 50 I2 = 1,NY
         DO 40 I1 = 1,NX
            CALL TABZE ('READ', ZEBUFF, IZERNO, ZEKOLS, ZENUMV, YZPOS,
     *         VPEAK, RESULT, NGA, XGAUSV, XGAUSB, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ ZE TABLE'
               GO TO 990
            ELSE IF (IRET.EQ.0) THEN
               J = 2
               K = 2
               IMAGE(I1,I2,1) = RESULT(1,1)
               IMAGE(I1,I2,2) = RESULT(1,2)
               DO 20 IP = 1,MP
                  IMAGE(I1,I2,J+1) = RESULT(K,1)
                  IMAGE(I1,I2,J+2) = RESULT(K,2)
                  J = J + 2
                  K = K + 1
 20               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       make a header
      CALL COPY (256, CATOLD, CATBLK)
C                                       Get user modification to CATBLK
      CALL SUBHD3 (BLC, TRC, 1.0, 1.0, 1.0)
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
      SUBROUTINE SHOIMG (IP, NX, NY, IGR5, IMAGE, IRET)
C-----------------------------------------------------------------------
C   SHOIMG displays an image plane on the TV screen and allows an
C   interactive transfer function, coloring, CURVALUE, and EXIT.
C   Inputs:
C      IP      I      Desired plane
C      NX      I      Number X pixels in image
C      NY      I      Number Y pixels in image
C      IMAGE   R(*)   Images of NP parameters
C   Output:
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   IP, NX, NY, IGR5, IRET
      REAL      IMAGE(NX,NY,*)
C
      INCLUDE 'ZEMAN.INC'
      INTEGER   NOPTS
      PARAMETER (NOPTS=12)
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:PTVC.INC'
C
      INTEGER   IX, IY, TVWIN(4), IWIN(4), NPIX, PLINC, IYTV, HORIZ,
     *   IBUFF(MABFSS), NLEVS, MCOL, MROWS, MTYPE, TIMLIM, TOPSEP, I,
     *   GRCHS(2), TVBUT, CHS, ITR, LUTBUF(TVMLUT), JJ, II, LG, LTY,
     *   NW, EX(5), EY(5), NXFRAM, NYFRAM, CFRAME, TFRAME, PINC,
     *   LNX, LNY, IC(2), NPIXW, WXPOS, JTRIM, JT, SIDSEP, MINC, MPIX,
     *   JBUFF(MABFSS), IGR, JNX, JNY, MBOX, NBO
      CHARACTER TRANFN*2, CHOICS(NOPTS+1)*12, ISHELP*8, TITLE*132,
     *   TVALS(4)*20, FUNCS(4)*2, BUNITS*8
      REAL      PMIN, PMAX, RPOS(2), SLOPE, OFFS, TEMP, BLCO(7), TRCO(7)
      LOGICAL   LEAVE(NOPTS+1), DOWEDG, DOEDGE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (IBUFF, BUFF2, LUTBUF), (JBUFF, BUFFO)
      DATA MCOL, MTYPE, TIMLIM, TOPSEP, SIDSEP /1, -1, 0, 3, 10/
      DATA CHOICS /'RETURN', ' ','LOAD AS', 'SET WINDOW','RESET WINDOW',
     *   'OFF TRANSF', 'OFF COLOR', 'TVTRANSF', 'TVPSEUDO', 'TVPHLAME',
     *   'TVZOOM', 'CURVALUE',' '/
      DATA LEAVE /3*.FALSE., .TRUE., .FALSE., 2*.TRUE., 6*.FALSE./
      DATA TVALS /'Gain', 'Gain error', 'Field (pixels)',
     *   'Field error (pixels)'/
      DATA FUNCS /'LN', 'SQ', 'LG', 'L2'/
C-----------------------------------------------------------------------
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
      MROWS = NOPTS
      IF (TFRAME.GT.1) THEN
         MROWS = MROWS + 1
         CHOICS(MROWS) = 'NEXT WINDOW'
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
         IF (IP.LE.2) THEN
            BUNITS = ' '
         ELSE
            BUNITS = 'PIXELS'
            END IF
         CALL CHR2H (8, BUNITS, 1, CATH(KHBUN))
         CATR(KRDMX) = PMAX
         CATR(KRDMN) = PMIN
         CATR(IRRAN+1) = PMAX
         CATR(IRRAN) = PMIN
         LG = (IP-1) / NMXIMG
         LTY = MOD (IP-1, NMXIMG) + 1
         IF (LG.GT.0) LTY = 2 + LTY
         IF (LG.GT.0) THEN
            WRITE (MSGTXT,1020) TVALS(LTY), LG, PMIN, PMAX
         ELSE
            WRITE (MSGTXT,1021) TVALS(LTY), PMIN, PMAX
            END IF
         CALL REFRMT (MSGTXT, '_', I)
         CALL MSGWRT (2)
C                                       window
 50      DOWEDG = .FALSE.
         DOEDGE = (LNX.LT.MAXXTV(1)-2) .AND. (LNY.LT.MAXXTV(2)-2)
         IF (LNX.LE.MAXXTV(1)) THEN
            IWIN(1) = IC(1)
            IWIN(3) = IC(1) - 1 + (LNX/MINC) * PINC
            TVWIN(1) = (MAXXTV(1) - LNX) / 2
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
         MPIX = MINC * NPIX
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
         IY = (IP - 1) / NMXIMG
         IF (IY.GT.0) IX = 2 + IX
         JT = JTRIM (TVALS(IX))
         TEMP = MAX (ABS(PMIN), ABS(PMAX))
         IF ((TEMP.LT.10000.) .AND. (TEMP.GT.0.001)) THEN
            IF (IY.GT.0) THEN
               WRITE (TITLE,1090) TVALS(IX), IY, CFRAME, PMIN, PMAX
               IF (TFRAME.LE.1) TITLE(38:41) = ' '
            ELSE
               WRITE (TITLE,1091) TVALS(IX), CFRAME, PMIN, PMAX
               IF (TFRAME.LE.1) TITLE(23:26) = ' '
               END IF
         ELSE
            IF (IY.GT.0) THEN
               WRITE (TITLE,1095) TVALS(IX), IY, CFRAME, PMIN, PMAX
               IF (TFRAME.LE.1) TITLE(38:41) = ' '
            ELSE
               WRITE (TITLE,1096) TVALS(IX), CFRAME, PMIN, PMAX
               IF (TFRAME.LE.1) TITLE(23:26) = ' '
               END IF
            END IF
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
                  IC(1) = 1
                  LNX = JNX
               ELSE
                  II = MOD (CFRAME-1, NXFRAM) + 1
                  IC(1) = (II - 1) * (MAXXTV(1) - 3) + 1
                  IF (IC(1)+MAXXTV(1)-33.GT.NX) IC(1) = JNX - MAXXTV(1)
     *               + 33
                  LNX = MAXXTV(1) - 33
                  END IF
               IF (NYFRAM.EQ.1) THEN
                  IC(2) = 1
                  LNY = JNY
               ELSE
                  II = (CFRAME-1) / NXFRAM + 1
                  IC(2) = (II - 1) * (MAXXTV(2) - 3) + 1
                  IF (IC(2)+MAXXTV(2)-33.GT.NY) IC(2) = JNY - MAXXTV(2)
     *               + 33
                  LNY = MAXXTV(2) - 33
                  END IF
               END IF
            GO TO 50
            END IF
         GO TO 100
         END IF
C
 990  IF (IRET.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SHOIMG: ERROR:',I4,' ON ',A)
 1020 FORMAT ('Load ',A,' Gauss',I2,' image',F12.6,' to',F12.6)
 1021 FORMAT ('Load ',A,' image from',F12.6,' to',F12.6)
 1050 FORMAT ('Load every pixel in subimage',I3)
 1051 FORMAT ('Load full image with only every',I3,
     *   ' pixels in X and Y')
 1052 FORMAT ('Loading every pixel replicated by',I3)
 1090 FORMAT (A,' Gaussian comp',I2,'_(',I2,')_',F11.5,' to',
     *   F11.5)
 1091 FORMAT (A,'__(',I2,')__',F11.5,' to',F11.5)
 1095 FORMAT (A,' Gaussian comp',I2,'_(',I2,')__',1PE11.3,' to',1PE11.3)
 1096 FORMAT (A,'__(',I2,')__',1PE11.3,' to',1PE11.3)
 1110 FORMAT ('BLC/TRC=',4I7)
      END
      SUBROUTINE TVALUE (GR, NX, NY, IMAGE, NLIST, PIXLIS, BUFFER, IRET)
C-----------------------------------------------------------------------
C   TVALUE performs interactive displays of map image values:
C   Special version for ZEMAN - allows picking pixels for list
C   Inputs:
C      GR        I        Graphics plane for lettering
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
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMAND.INC'
      INCLUDE 'ZEMANO.INC'
      CHARACTER PHNAME*48
      INTEGER   IROUND, LUNI, NYI, NXI, WINI(4), BOI, J, I1, IPOS(7),
     *   BOTEMP, IBIND, INDI, LIM1, IG, I, IX, IY, LGRNO, INLIST,
     *   YZPOS(2), LZOOM(3), K, NG, LUNV, INDV, VBIND
      REAL      RESULT(2*MAXPRM), VPEAK
      DOUBLE PRECISION PARMS(MAXPRM), UPARMS(MAXPRM), XPARMS(MAXPRM)
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUNI, LUNV /16,17/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       display parms
      INLIST = NLIST
      NG = NGAUSS
      CALL DFILL (MAXPRM, 0.0D0, XPARMS)
C                                       Open and init for read
      IF (OPER.EQ.'REDO') THEN
         CALL ZPHFIL ('MA', DISKI, OLDCNO(2), 1, PHNAME, IRET)
         CALL ZOPEN (LUNI, INDI, DISKI, PHNAME, T, F, T, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INPUT I FILE'
            GO TO 990
            END IF
         CALL ZPHFIL ('MA', DISKV, OLDCNO(1), 1, PHNAME, IRET)
         CALL ZOPEN (LUNV, INDV, DISKV, PHNAME, T, F, T, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE'
            GO TO 990
            END IF
C                                       Setup for I/O
         NXI = CATOLD(KINAX,1)
         NYI = CATOLD(KINAX+1,1)
         WINI(1) = IROUND (UBLC(1))
         WINI(2) = IROUND (BLC(2))
         WINI(3) = IROUND (UTRC(1))
         WINI(4) = IROUND (TRC(2))
C                                       Initial guess
         PARMS(1) = 0.0D0
         DO 50 J = 1,NG
            PARMS(J+1) = 0.0D0
 50         CONTINUE
         IG = NG + 1
         DO 55 I1 = 1,IG
            UPARMS(I1) = PARMS(I1)
 55         CONTINUE
         CALL COPY (MAXPRM, DOCOMP, LLCOMP)
C                                       Setup for looping
C                                       Loop
         LIM1 = UTRC(1) - BLC(1) + 1.01
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
         IZERNO = LGRNO
         CALL TABZE ('READ', ZEBUFF, IZERNO, ZEKOLS, ZENUMV, YZPOS,
     *      VPEAK, RESULT, NGA, XGAUSV, XGAUSB, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ ZE TABLE'
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
C                                       Init. files, first input.
            IF (OPER.EQ.'REDO') THEN
               IPOS(2) = YZPOS(1)
               IPOS(3) = YZPOS(2)
               CALL COMOFF (CATOLD(KIDIM,1), CATOLD(KINAX,1), IPOS(3),
     *            BOTEMP, IRET)
               BOI = BOTEMP + 1
               WINI(2) = YZPOS(1)
               WINI(4) = YZPOS(1)
               CALL MINIT ('READ', LUNV, INDV, NXI, NYI, WINI, BUFF1,
     *            JBUFSZ, BOI, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'INIT READ INPUT V IMAGE'
                  GO TO 990
                  END IF
C                                       Read.
               CALL MDISK ('READ', LUNV, INDV, BUFF1, VBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READING INPUT V IMAGE'
                  GO TO 990
                  END IF
               CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF2,
     *            JBUFSZ, BOI, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'INIT READ INPUT I IMAGE'
                  GO TO 990
                  END IF
C                                       Read.
               CALL MDISK ('READ', LUNI, INDI, BUFF2, IBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READING INPUT I IMAGE'
                  GO TO 990
                  END IF
C                                       Copy to buffer.
               DO 110 I1 = 1,LIM1
                  VDATA(I1) = BUFF1(VBIND+I1-1)
                  IDATA(I1) = BUFF2(IBIND+I1-1)
 110              CONTINUE
C                                       Call DO1FIT
               CALL DO1FIT (IPOS, UPARMS, PARMS, XPARMS, RESULT, IRET)
               IF (IRET.EQ.99) THEN
                  MSGTXT = 'Quitting at user request'
                  CALL MSGWRT (5)
                  CALL ZCLOSE (LUNI, INDI, I1)
                  GO TO 999
               ELSE IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1180) IRET
                  GO TO 990
                  END IF
            ELSE IF (OPER.EQ.'FLAG') THEN
               CALL RFILL (2*MAXPRM, FBLANK, RESULT)
               END IF
            IZERNO = LGRNO
            CALL TABZE ('WRIT', ZEBUFF, IZERNO, ZEKOLS, ZENUMV, YZPOS,
     *         VPEAK, RESULT, NGA, XGAUSV, XGAUSB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE ZE TABLE'
               GO TO 990
               END IF
C                                       update the image value
            IMAGE(IX,IY,1) = RESULT(1)
            IMAGE(IX,IY,2) = RESULT(1+MAXPRM)
            K = 2
            J = 2
            DO 120 I1 = 1,NG
               IMAGE(IX,IY,J+1) = RESULT(K)
               IMAGE(IX,IY,J+2) = RESULT(K+MAXPRM)
               J = J + NMXIMG
               K = K + 1
 120           CONTINUE
            NLIST = NLIST - 1
            END IF
 300     CONTINUE
C                                       Close files
      IF (OPER.EQ.'REDO') THEN
         CALL ZCLOSE (LUNI, INDI, I)
         CALL ZCLOSE (LUNV, INDV, I)
         CALL COPY (3, LZOOM, TVZOOM)
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., I)
         IF (IPL(1).GT.0) CALL YSLECT ('ONNN', IPL(1), 0, SCRTCH, I)
         IF (IPL(2).GT.0) CALL YSLECT ('ONNN', IPL(2), 0, SCRTCH, I)
         DO 310 J = 1,4
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
      SUBROUTINE UPDALL (OPER, MAXRES, MAXGN, MAXFLD, MAXDFL, NX, NY,
     *   IMAGE, IRET)
C-----------------------------------------------------------------------
C   UPDALL flags or re-does fitting on all pixels matching test
C   conditions
C   Inputs:
C      OPER     C*4    'REDO', 'FLAG'
C      MAXRES   R      Max allowed residual (> MAXRES => redo)
C      MAXGN    R(*)   Min/Max allowed gain
C      MAXFLD   R(*)   Min/Max allowed field value
C      MAXDWD   R(*)   Max error in field
C      NX       I      Number X pixels in image
C      NY       I      Number Y pixels in image
C   In/out:
C      IMAGE    R(*)   Images of NP parameters - updated on output
C   Output:
C      IRET     I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   NX, NY, IRET
      REAL      MAXRES, MAXGN(2), MAXFLD(2,*), MAXDFL(*), IMAGE(NX,NY,*)
C
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMAND.INC'
      INCLUDE 'ZEMANO.INC'
      CHARACTER PHNAME*48
      INTEGER   IROUND, LUNI, NYI, NXI, WINI(4), BOI, J, I1, IPOS(7),
     *   BOTEMP, IBIND, INDI, LIM1, IG, I, IY, IZ, LGRNO, INLIST,
     *   YZPOS(2), LZOOM(3), MP, IIZ, IIY, NG, K, LUNV, INDV, VBIND
      REAL      RESULT(2*MAXPRM), VPEAK
      DOUBLE PRECISION PARMS(MAXPRM), UPARMS(MAXPRM), XPARMS(MAXPRM)
      LOGICAL   T, F, DOREAD, DOIT
      DOUBLE PRECISION  FJAC(MAXPRM,MAXPRM), FVEC(NPLIM)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUNI, LUNV /16,17/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       display parms
      INLIST = NLIST
      DOREAD = (MAXRES.GT.0.0) .OR. (OPER.EQ.'REDO')
      NG = NGAUSS
      CALL DFILL (MAXPRM, 0.0D0, XPARMS)
C                                       Open and init for read
      IF (DOREAD) THEN
         CALL ZPHFIL ('MA', DISKI, OLDCNO(2), 1, PHNAME, IRET)
         CALL ZOPEN (LUNI, INDI, DISKI, PHNAME, T, F, T, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INPUT I FILE'
            GO TO 990
            END IF
         CALL ZPHFIL ('MA', DISKV, OLDCNO(1), 1, PHNAME, IRET)
         CALL ZOPEN (LUNV, INDV, DISKV, PHNAME, T, F, T, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INPUT I FILE'
            GO TO 990
            END IF
C                                       Setup for I/O
         NXI = CATOLD(KINAX,1)
         NYI = CATOLD(KINAX+1,1)
         WINI(1) = IROUND (UBLC(1))
         WINI(2) = IROUND (UBLC(2))
         WINI(3) = IROUND (UTRC(1))
         WINI(4) = IROUND (UTRC(2))
C                                       Initial guess
         PARMS(1) = 0.0D0
         DO 50 J = 1,NG
            PARMS(J+1) = 0.0D0
 50         CONTINUE
         IG = NG + JCODE
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
C                                       do the checking
      DO 200 IZ = LBLC(2),LTRC(2)
         DO 190 IY = LBLC(1),LTRC(1)
            LGRNO = (IZ-IBLC(2)) * (ITRC(1)-IBLC(1)+1) + IY - IBLC(1) +
     *         1
            IZERNO = LGRNO
            CALL TABZE ('READ', ZEBUFF, IZERNO, ZEKOLS, ZENUMV, YZPOS,
     *         VPEAK, RESULT, NGA, XGAUSV, XGAUSB, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ ZE TABLE'
               GO TO 990
            ELSE IF (IRET.EQ.0) THEN
C                                       Init. files, first input.
               IF (DOREAD) THEN
                  IPOS(2) = YZPOS(1)
                  IPOS(3) = YZPOS(2)
                  CALL COMOFF (CATOLD(KIDIM,1), CATOLD(KINAX,1),
     *               IPOS(3), BOTEMP, IRET)
                  BOI = BOTEMP + 1
                  WINI(2) = YZPOS(1)
                  WINI(4) = YZPOS(1)
C                                       Read V image
                  CALL MINIT ('READ', LUNV, INDV, NXI, NYI, WINI, BUFF1,
     *               JBUFSZ, BOI, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'INIT READ INPUT V IMAGE'
                     GO TO 990
                     END IF
                  CALL MDISK ('READ', LUNV, INDV, BUFF1, VBIND, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READING INPUT V IMAGE'
                     GO TO 990
                     END IF
C                                       read I image
                  CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF2,
     *               JBUFSZ, BOI, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'INIT READ INPUT I IMAGE'
                     GO TO 990
                     END IF
C                                       Read.
                  CALL MDISK ('READ', LUNI, INDI, BUFF2, IBIND, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READING INPUT I IMAGE'
                     GO TO 990
                     END IF
C                                       Copy to buffer.
                  DO 110 I1 = 1,LIM1
                     VDATA(I1) = BUFF1(VBIND+I1-1)
                     IDATA(I1) = BUFF2(IBIND+I1-1)
 110                 CONTINUE
                  END IF
C                                       do we do this one? check comps
               DOIT = .FALSE.
               J = 1
               IF ((MAXGN(1).LT.MAXGN(2)) .AND. (RESULT(1).NE.FBLANK))
     *            THEN
                  IF (RESULT(1).LT.MAXGN(1)) DOIT = .TRUE.
                  IF (RESULT(1).GT.MAXGN(2)) DOIT = .TRUE.
                  END IF
               DO 120 IG = 1,NGAUSS
                  J = J + 1
                  IF ((MAXFLD(1,IG).LT.MAXFLD(2,IG)) .AND.
     *               (RESULT(J).NE.FBLANK)) THEN
                     IF (RESULT(J).LT.MAXFLD(1,IG)) DOIT = .TRUE.
                     IF (RESULT(J).GT.MAXFLD(2,IG)) DOIT = .TRUE.
                     END IF
                  IF ((MAXDFL(IG).GT.0) .AND.
     *                  (RESULT(J+MAXPRM).NE.FBLANK)) THEN
                     IF (RESULT(J+MAXPRM).GT.MAXDFL(IG))
     *                  DOIT = .TRUE.
                     END IF
 120              CONTINUE
C                                       parameters
               DO 130 I = 1,NGAUSS+1
                  PARMS(J) = RESULT(J)
 130              CONTINUE
               MP = NGA
C                                       check residuals
               IF ((.NOT.DOIT) .AND. (MAXRES.GT.0.0)) THEN
                  I = 1
                  ITTER = 0
                  NITTER = MAX (XNIT, 100.0)
                  CALL ZEFUNC (LIM1, MP, PARMS, FVEC, FJAC, I)
                  DO 140 I1 = 1,LIM1
                     IF (BUFF1(VBIND+I1-1).NE.FBLANK) THEN
                        IF (ABS(FVEC(I1)).GT.MAXRES) DOIT = .TRUE.
                        END IF
 140                 CONTINUE
                  END IF
C                                       Call DO1FIT
               IF (DOIT) THEN
                  IF (OPER.EQ.'REDO') THEN
                     CALL DO1FIT (IPOS, UPARMS, PARMS, XPARMS, RESULT,
     *                  IRET)
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
                     CALL RFILL (2*MAXPRM, FBLANK, RESULT)
                     END IF
                  IZERNO = LGRNO
                  CALL TABZE ('WRIT', ZEBUFF, IZERNO, ZEKOLS, ZENUMV,
     *               YZPOS, VPEAK, RESULT, NGA, XGAUSV, XGAUSB, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITE ZE TABLE'
                     GO TO 990
                     END IF
C                                       update the image value
                  IIY = IY - IBLC(1) + 1
                  IIZ = IZ - IBLC(2) + 1
                  IMAGE(IIY,IIZ,1) = RESULT(1)
                  IMAGE(IIY,IIZ,2) = RESULT(1+MAXPRM)
                  K = 2
                  J = 2
                  DO 150 I1 = 1,NG
                     IMAGE(IIY,IIZ,J+1) = RESULT(K)
                     IMAGE(IIY,IIZ,J+2) = RESULT(K+MAXPRM)
                     J = J + NMXIMG
                     K = K + 1
 150                 CONTINUE
                  END IF
               END IF
 190        CONTINUE
 200     CONTINUE
C                                       Close files
      IF (DOREAD) THEN
         CALL ZCLOSE (LUNI, INDI, I)
         CALL ZCLOSE (LUNV, INDV, I)
         END IF
      IF (OPER.EQ.'REDO') THEN
         CALL COPY (3, LZOOM, TVZOOM)
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., I)
         IF (IPL(1).GT.0) CALL YSLECT ('ONNN', IPL(1), 0, SCRTCH, I)
         IF (IPL(2).GT.0) CALL YSLECT ('ONNN', IPL(2), 0, SCRTCH, I)
         DO 310 J = 1,4
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
      SUBROUTINE ZEMANO (IRET)
C-----------------------------------------------------------------------
C   ZEMANO creates the output residual image and the parameter images
C   and then computes the residual map (if any) and fills (via PSCALE)
C   the individual Gaussian parameter images.  It calls ZMANHI for
C   history info for all images.
C   Output:
C      IRET    I   0 => ok,  4 => real trouble.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER SEQTYP*6
      INTEGER   NG, NXO, NYO, WINO(4), IP, NCN, IG, IOFF, I
      LOGICAL   DORES, DOPARM, DOCORR
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      I = DOCAT
      DORES = I.GE.4
      IF (DORES) I = I - 4
      DOPARM = I.GE.2
      DOCORR  = MOD(I-1,2).EQ.0
      NCN = 2
C                                       create output images
      CALL ZMANCR (DOCORR, DORES, DOPARM, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATING OUTPUT FILES'
         GO TO 990
         END IF
      IF (DOCORR) THEN
         NCN = NCN + 1
         CALL ZMANCO (NCN, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING THE CORRECTED IMAGE'
            GO TO 990
            END IF
         CALL ZMANHI (0, NCN)
         END IF
      IF (DORES) THEN
         NCN = NCN + 1
         NEWCNO = FCNO(NCN)
         DISKO = FVOL(NCN)
         CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'REST', SCRTCH,
     *      IRET)
         IF ((IRET.NE.0) .AND. (IRET.NE.6)) THEN
            WRITE (MSGTXT,1005) IRET, NCN
            GO TO 990
            END IF
         CALL ZMANRE (NCN, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING THE RESIDUAL IMAGE'
            GO TO 990
            END IF
         CALL ZMANHI (0, NCN)
         END IF
C                                       loop limits etc.
      IF (DOPARM) THEN
         NG = NGAUSS
         WINO(1) = 1
         WINO(2) = 1
C                                       Output model parms
         DO 30 IG = 1,NG+1
            DO 20 IP = 1,2
               IF (DOCOMP(IG).GT.0) THEN
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
                  IOFF = IG + (IP-1) * MAXPRM
                  CALL PSCALE (IOFF, WINO, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1011) IRET, SEQTYP
                     GO TO 990
                     END IF
C                                       History, close
                  CALL ZMANHI (IOFF, NCN)
                  END IF
 20            CONTINUE
 30         CONTINUE
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IRET = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZEMANO: ERROR',I4,' ON ',A)
 1005 FORMAT ('ERROR',I5,' RECOVERING FILE HEADER NUMBER',I5)
 1010 FORMAT ('Begin writing file of type ',A)
 1011 FORMAT ('ERROR',I5,' MOVING DATA TO FILE TYPE ',A4,A2)
      END
      SUBROUTINE ZMANCR (DOCORR, DORES, DOPARM, IRET)
C-----------------------------------------------------------------------
C   ZMANCR creates the output files.
C   Inputs:
C      DOCORR   L   Create corrected V cube
C      DORES    L   Create residual?
C      DOPARM   L   Create parameter images?
C   Output:
C      IRET     I   Error code: 0 => ok
C                     4 => user routine detected error.
C                     5 => catalog troubles
C                     8 => can't start
C      /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      LOGICAL   DOCORR, DORES, DOPARM
      INTEGER   IRET
C
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMANO.INC'
      CHARACTER BLANK*8, SEQTYP(2,MAXPRM)*8, OTYPE*8, BUNIT*8
      INTEGER   IERR, NG, NAX, I, IG, IP, INPSEQ
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA BLANK /'      '/
      DATA SEQTYP /'GAIN  ','DGAIN ', 'FIELD1','DFELD1',
     *             'FIELD2','DFELD2', 'FIELD3','DFELD3',
     *             'FIELD4','DFELD4', 'FIELD5','DFELD5',
     *             'FIELD6','DFELD6', 'FIELD7','DFELD7',
     *             'FIELD8','DFELD8'/
C-----------------------------------------------------------------------
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD, CATBLK)
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEV, CLASSV, SEQV, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
      INPSEQ = SEQOUT
C                                       Get user modification to CATBLK
      CALL SUBHD3 (BLC, TRC, 1.0, 1.0, 1.0)
C                                       Create output file for residual
      NEWCNO = 0
      IRET = 4
      IF (DOCORR) THEN
         NEWCNO = 1
         CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR, 'CORRECTED'
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO
         FCNO(NCFILE) = NEWCNO
         FRW(NCFILE) = 2
         END IF
C                                       save the residual header
      CALL COPY (256, CATBLK, BUFF2)
      IF (DORES) THEN
         CATBLK(KIIMS) = INPSEQ
         CALL CHR2H (6, 'VRESID', KHIMCO, CATH(KHIMC))
         NEWCNO = 1
         CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR, 'RESIDUALS'
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO
         FCNO(NCFILE) = NEWCNO
         FRW(NCFILE) = 2
         END IF
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
         CALL H2CHR (4, 1, OLDH(KHCTP,1), OTYPE)
         IF ((OTYPE.NE.'FREQ') .AND. (OTYPE.NE.'VELO') .AND.
     *      (OTYPE.NE.'FELO')) THEN
            MSGTXT = 'WARNING: OUTPUT FIELD PARM FILES HAVE INCORRECT'
     *         // ' UNITS'
            CALL MSGWRT (8)
            END IF
C                                       loop limits etc.
         NG = NGAUSS + 1
C                                       Output Gaussian parms
         DO 120 IG = 1,NG
            DO 110 IP = 1,2
               IF (DOCOMP(IG).GT.0) THEN
                  CALL CHR2H (6, SEQTYP(IP,IG), KHIMCO, CATH(KHIMC))
                  IF (IG.EQ.1) THEN
                     BUNIT = 'GAIN'
                  ELSE IF (OTYPE.EQ.'FREQ') THEN
                     BUNIT = 'HERTZ'
                  ELSE
                     BUNIT = 'M/SEC'
                     END IF
                  CALL CHR2H (8, BUNIT, 1, CATH(KHBUN))
C                                       Create
                  DISKO = XDISKO + 0.01
                  NEWCNO = 1
                  CATBLK(KIIMS) = INPSEQ
                  CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1100) IERR, SEQTYP(IP,IG)
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
         END IF
      IRET = 0
      CALL COPY (256, BUFF2, CATBLK)
      DISKO = FVOL(3)
      NEWCNO = FCNO(3)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT ',A,' FILE')
 1100 FORMAT ('ERROR',I5,' CREATING FILE TYPE ',A)
      END
      SUBROUTINE ZMANCO (NCN, IRET)
C-----------------------------------------------------------------------
C   ZMANCO reads the input data and the table to compute and write a
C   corrected image (Z = Z - P(1)*I)
C   Output:
C      IRET
C-----------------------------------------------------------------------
      INTEGER   NCN, IRET
C
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMAND.INC'
      INCLUDE 'ZEMANO.INC'
      INTEGER  LIM1, LIM2, LIM3, NXI, NYI, WINI(4), NXO, NYO, WINO(4),
     *   IPOS(7), BOI, IBIND, OBIND, LUNI, INDI, LUNO, INDO, IROUND, I1,
     *   I2, I3, I, XYPOS(2), LUNV, INDV, VBIND
      REAL      RMIN, RMAX, RESULT(MAXPRM,2), VPEAK, TEMP
      CHARACTER PHNAME*48
      LOGICAL   BLNKD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUNI, LUNV, LUNO /16,17,18/
C-----------------------------------------------------------------------
      MSGTXT = 'Begin writing corrected image cube'
      CALL MSGWRT (2)
      RMIN = 1.E15
      RMAX = -1.E15
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKI, OLDCNO(2), 1, PHNAME, IRET)
      CALL ZOPEN (LUNI, INDI, DISKI, PHNAME, .TRUE., .FALSE., .TRUE.,
     *   IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT I IMAGE'
         GO TO 990
         END IF
      CALL ZPHFIL ('MA', DISKV, OLDCNO(1), 1, PHNAME, IRET)
      CALL ZOPEN (LUNV, INDV, DISKV, PHNAME, .TRUE., .FALSE., .TRUE.,
     *   IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT V IMAGE'
         GO TO 990
         END IF
      NXI = CATOLD(KINAX,1)
      NYI = CATOLD(KINAX+1,1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM1 = TRC(1) - BLC(1) + 1.01
      CALL FILL (7, 1, IPOS)
C                                       Open and init for write
      CALL ZPHFIL ('MA', FVOL(NCN), FCNO(NCN), 1, PHNAME, IRET)
      CALL ZOPEN (LUNO, INDO, FVOL(NCN), PHNAME, .TRUE., .FALSE.,
     *   .TRUE., IRET)
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
      IZERNO = 1
      BLNKD = .FALSE.
      DO 100 I3 = 1,LIM3
C                                       input
         IPOS(3) = BLC(3) + I3 - 0.9
         CALL COMOFF (CATOLD(KIDIM,1), CATOLD(KINAX,1), IPOS(3), BOI,
     *      IRET)
         BOI = BOI + 1
         CALL MINIT ('READ', LUNV, INDV, NXI, NYI, WINI, BUFF1, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT READ OF INPUT V IMAGE'
            GO TO 990
            END IF
         CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF2, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT READ OF INPUT I IMAGE'
            GO TO 990
            END IF
C                                       output
         IPOS(3) = I3
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3), BOI, IRET)
         BOI = BOI + 1
         CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFFO, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT READ OF INPUT IMAGE'
            GO TO 990
            END IF
         DO 90 I2 = 1,LIM2
            CALL MDISK ('READ', LUNV, INDV, BUFF1, VBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ INPUT I IMAGE'
               GO TO 990
               END IF
            CALL MDISK ('READ', LUNI, INDI, BUFF2, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ INPUT I IMAGE'
               GO TO 990
               END IF
            CALL MDISK ('WRIT', LUNO, INDO, BUFFO, OBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE CORRECTED IMAGE'
               GO TO 990
               END IF
            CALL TABZE ('READ', ZEBUFF, IZERNO, ZEKOLS, ZENUMV,
     *         XYPOS, VPEAK, RESULT, NGA, XGAUSV, XGAUSB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ ZE TABLE'
               GO TO 990
               END IF
C                                       Copy to buffer.
            DO 20 I1 = 1,LIM1
               VDATA(I1) = BUFF1(VBIND+I1-1)
               IDATA(I1) = BUFF2(IBIND+I1-1)
 20            CONTINUE
C                                       parameters
            IF (RESULT(1,1).EQ.FBLANK) THEN
               DO 30 I1 = 1,LIM1
                  IF ((BUFF1(VBIND+I1-1).EQ.FBLANK) .OR.
     *               (BUFF2(IBIND+I1-1).EQ.FBLANK)) THEN
                     BUFFO(OBIND+I1-1) = FBLANK
                     BLNKD = .TRUE.
                  ELSE
                     TEMP = BUFF1(VBIND+I1-1)
                     BUFFO(OBIND+I1-1) = TEMP
                     IF (TEMP.GT.RMAX) RMAX = TEMP
                     IF (TEMP.LT.RMIN) RMIN = TEMP
                     END IF
 30               CONTINUE
            ELSE
               DO 40 I1 = 1,LIM1
                  IF ((BUFF1(VBIND+I1-1).EQ.FBLANK) .OR.
     *               (BUFF2(IBIND+I1-1).EQ.FBLANK)) THEN
                     BUFFO(OBIND+I1-1) = FBLANK
                     BLNKD = .TRUE.
                  ELSE
                     TEMP = BUFF1(VBIND+I1-1) - RESULT(1,1) *
     *                  BUFF2(IBIND+I1-1)
                     BUFFO(OBIND+I1-1) = TEMP
                     IF (TEMP.GT.RMAX) RMAX = TEMP
                     IF (TEMP.LT.RMIN) RMIN = TEMP
                     END IF
 40               CONTINUE
               END IF
 90         CONTINUE
         CALL MDISK ('FINI', LUNO, INDO, BUFFO, OBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'FINISH CORRECTED IMAGE'
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
      IF (INDV.GT.0) CALL ZCLOSE (LUNV, INDV, I)
      IF (INDO.GT.0) CALL ZCLOSE (LUNO, INDO, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZMANCO: ERROR',I4,' ON ',A)
      END
      SUBROUTINE ZMANRE (NCN, IRET)
C-----------------------------------------------------------------------
C   ZMANRE reads the input data and the table to compute and write a
C   residual image.
C   Output:
C      IRET
C-----------------------------------------------------------------------
      INTEGER   NCN, IRET
C
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMAND.INC'
      INCLUDE 'ZEMANO.INC'
      INTEGER  LIM1, LIM2, LIM3, NXI, NYI, WINI(4), NXO, NYO, WINO(4),
     *   IPOS(7), BOI, IBIND, OBIND, LUNI, INDI, LUNO, INDO, IROUND, I1,
     *   I2, I3, I, J, XYPOS(2), LUNV, INDV, VBIND
      REAL      RMIN, RMAX, RESULT(MAXPRM,2), VPEAK
      CHARACTER PHNAME*48
      LOGICAL   BLNKD
      DOUBLE PRECISION PARMS(MAXPRM), FJAC(MAXPRM,MAXPRM), FVEC(NPLIM)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUNI, LUNV, LUNO /16,17,18/
C-----------------------------------------------------------------------
      MSGTXT = 'Begin writing residual image cube'
      CALL MSGWRT (2)
      RMIN = 1.E15
      RMAX = -1.E15
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKI, OLDCNO(2), 1, PHNAME, IRET)
      CALL ZOPEN (LUNI, INDI, DISKI, PHNAME, .TRUE., .FALSE., .TRUE.,
     *   IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT I IMAGE'
         GO TO 990
         END IF
      CALL ZPHFIL ('MA', DISKV, OLDCNO(1), 1, PHNAME, IRET)
      CALL ZOPEN (LUNV, INDV, DISKV, PHNAME, .TRUE., .FALSE., .TRUE.,
     *   IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT V IMAGE'
         GO TO 990
         END IF
      NXI = CATOLD(KINAX,1)
      NYI = CATOLD(KINAX+1,1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM1 = TRC(1) - BLC(1) + 1.01
      CALL FILL (7, 1, IPOS)
C                                       Open and init for write
      CALL ZPHFIL ('MA', FVOL(NCN), FCNO(NCN), 1, PHNAME, IRET)
      CALL ZOPEN (LUNO, INDO, FVOL(NCN), PHNAME, .TRUE., .FALSE.,
     *   .TRUE., IRET)
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
      IZERNO = 1
      BLNKD = .FALSE.
      DO 100 I3 = 1,LIM3
C                                       input
         IPOS(3) = BLC(3) + I3 - 0.9
         CALL COMOFF (CATOLD(KIDIM,1), CATOLD(KINAX,1), IPOS(3), BOI,
     *      IRET)
         BOI = BOI + 1
         CALL MINIT ('READ', LUNV, INDV, NXI, NYI, WINI, BUFF1, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT READ OF INPUT V IMAGE'
            GO TO 990
            END IF
         CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF2, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT READ OF INPUT I IMAGE'
            GO TO 990
            END IF
C                                       output
         IPOS(3) = I3
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3), BOI, IRET)
         BOI = BOI + 1
         CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFFO, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT READ OF INPUT IMAGE'
            GO TO 990
            END IF
         DO 90 I2 = 1,LIM2
            CALL MDISK ('READ', LUNV, INDV, BUFF1, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ INPUT V IMAGE'
               GO TO 990
               END IF
            CALL MDISK ('READ', LUNI, INDI, BUFF2, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ INPUT I IMAGE'
               GO TO 990
               END IF
            CALL MDISK ('WRIT', LUNO, INDO, BUFFO, OBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE RESIDUAL IMAGE'
               GO TO 990
               END IF
            CALL TABZE ('READ', ZEBUFF, IZERNO, ZEKOLS, ZENUMV,
     *         XYPOS, VPEAK, RESULT, NGA, XGAUSV, XGAUSB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE NEW ZE TABLE'
               GO TO 990
               END IF
C                                       Copy to buffer.
            DO 20 I1 = 1,LIM1
               VDATA(I1) = BUFF1(VBIND+I1-1)
               IDATA(I1) = BUFF2(IBIND+I1-1)
 20            CONTINUE
C                                       parameters
            CALL DFILL (MAXPRM, 0.0D0, PARMS)
            IF (RESULT(1,1).NE.FBLANK) PARMS(1) = RESULT(1,1)
            I1 = JCODE + 1
            J = 2
            DO 30 I = 1,NGA
               IF (RESULT(J,1).NE.FBLANK) PARMS(I1) = RESULT(J,1)
               I1 = I1 + 1
               J = J + 1
 30            CONTINUE
            I1 = I1 - 1
            I = 1
            ITTER = 0
            NITTER = MAX (XNIT, 100.0)
            CALL ZEFUNC (LIM1, I1, PARMS, FVEC, FJAC, I)
            DO 40 I1 = 1,LIM1
               IF (BUFF1(IBIND+I1-1).EQ.FBLANK) THEN
                  BUFFO(OBIND+I1-1) = FBLANK
                  BLNKD = .TRUE.
               ELSE
                  BUFFO(OBIND+I1-1) = FVEC(I1)
                  IF (FVEC(I1).GT.RMAX) RMAX = FVEC(I1)
                  IF (FVEC(I1).LT.RMIN) RMIN = FVEC(I1)
                  END IF
 40            CONTINUE
 90         CONTINUE
         CALL MDISK ('FINI', LUNO, INDO, BUFFO, OBIND, IRET)
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
      IF (INDV.GT.0) CALL ZCLOSE (LUNV, INDV, I)
      IF (INDO.GT.0) CALL ZCLOSE (LUNO, INDO, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZMANRE: ERROR',I4,' ON ',A)
      END
      SUBROUTINE PSCALE (IOFF, WINO, IRET)
C-----------------------------------------------------------------------
C   PSCALE reads a map file extracting one point per row
C   and writes a image out.
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
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMANO.INC'
      CHARACTER PHNAME*48
      LOGICAL   BLNKD, T
      REAL      PMIN, PMAX, RESULT(2*MAXPRM), VPEAK, XGAUSV(3*MAXGAU),
     *   XGAUSB(2)
      INTEGER   NXO, NYO, I2, LUNO, INDO, IPOS(7), BOTEMP, OBIND, L,
     *   JERR, I1, NGA
      DOUBLE PRECISION PMULT
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA T /.TRUE./
      DATA LUNO /17/
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
      IZERNO = 1
      BLNKD = .FALSE.
      PMIN = 1.E15
      PMAX = -PMIN
      PMULT = 1.0D0
      IF (REFINC.NE.0.0) PMULT = REFINC
      IF ((IOFF.EQ.1) .OR. (IOFF.EQ.1+MAXPRM)) PMULT = 1.0
      IF (IOFF.GT.1+MAXPRM) PMULT = ABS (PMULT)
C                                       Init output
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3), BOTEMP, IRET)
      BOTEMP = BOTEMP + 1
      CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFFO, JBUFSZ,
     *   BOTEMP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT I/O TO PARAMETER IMAGE'
         GO TO 990
         END IF
C                                       Init a write
      DO 100 I2 = 1,NYO
         CALL MDISK ('WRIT', LUNO, INDO, BUFFO, OBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING PARAMETER IMAGE'
            GO TO 990
            END IF
C                                       Loop thru input plane
         L = OBIND - 1
         DO 90 I1 = 1,NXO
            CALL TABZE ('READ', ZEBUFF, IZERNO, ZEKOLS, ZENUMV, IPOS(2),
     *         VPEAK, RESULT, NGA, XGAUSV, XGAUSB, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ ZE TABLE'
               GO TO 990
               END IF
            L = L + 1
            IF ((IRET.LT.0) .OR. (RESULT(IOFF).EQ.FBLANK)) THEN
               BUFFO(L) = FBLANK
               BLNKD = .TRUE.
            ELSE
               BUFFO(L) = RESULT(IOFF) * PMULT
               PMIN = MIN (PMIN, BUFFO(L))
               PMAX = MAX (PMAX, BUFFO(L))
               END IF
 90         CONTINUE
 100     CONTINUE
C                                       Flush output plane
      CALL MDISK ('FINI', LUNO, INDO, BUFFO, OBIND, IRET)
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
      SUBROUTINE ZMANHI (ITYP, NCN)
C-----------------------------------------------------------------------
C   ZMANHI copies and updates history file.
C   Inputs:
C      ITYP   I   Output map type: 0 => residual
C                    1 => answers (get 1st axis info also)
C      NCN    I   Position in FILES common on catlgd file
C-----------------------------------------------------------------------
      INTEGER   ITYP, NCN
C
      CHARACTER HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IERR, KBLC(7), KTRC(7), I
      LOGICAL   T
      INCLUDE 'ZEMAN.INC'
      INCLUDE 'ZEMANO.INC'
      INCLUDE 'ZEMAND.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       partial header keyword copy
      NEWCNO = FCNO(NCN)
      DISKO = FVOL(NCN)
      CALL KEYPCP (DISKV, OLDCNO, DISKO, NEWCNO, 0, ' ', IERR)
C                                        Copy only the relevant table
      CALL TABCOP ('ZE', ZEVERS, ZEVERS, LUN1, LUN2, DISKV, DISKO,
     *   OLDCNO, NEWCNO, CATBLK, SCRTCH, BUFFO, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING GC TABLE'
         CALL MSGWRT (6)
         END IF
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKV, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   SCRTCH, BUFFO, IERR)
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
      CALL HENCO1 (TSKNAM, NAMEV, CLASSV, SEQV, DISKV, LUN2, BUFFO,
     *   IERR)
      IF (IERR.NE.0) GO TO 50
      CALL HENCO2 (TSKNAM, NAMEI, CLASSI, SEQI, DISKI, LUN2, BUFFO,
     *   IERR)
      IF (IERR.NE.0) GO TO 50
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFFO, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       BLC
      WRITE (HILINE,2000) TSKNAM, KBLC
      CALL HIADD (LUN2, HILINE, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       TRC
      WRITE (HILINE,2001) TSKNAM, KTRC
      CALL HIADD (LUN2, HILINE, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       other Parms
      CALL H2CHR (8, 1, OLDH(KHBUN,1), LABEL)
      WRITE (HILINE,2003) TSKNAM, FCUT, LABEL
      CALL HIADD (LUN2, HILINE, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 50
      WRITE (HILINE,2004) TSKNAM, ZEVERS
      CALL HIADD (LUN2, HILINE, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Gaussian guesses
C      WRITE (HILINE,2009) TSKNAM, DOMAX(1)
C      CALL HIADD (LUN2, HILINE, BUFFO, IERR)
C      IF (IERR.NE.0) GO TO 50
C                                       type
      IF (DOGAUS.EQ.-1) THEN
         WRITE (HILINE,2010) TSKNAM
      ELSE IF (DOGAUS.EQ.0) THEN
         WRITE (HILINE,2011) TSKNAM
      ELSE
         WRITE (HILINE,2012) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2013) TSKNAM, XGVERS
         END IF
      CALL HIADD (LUN2, HILINE, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Old axis 1
      IF (ITYP.GT.0) THEN
C        CALL H2CHR (8, 1, OLDH(KHCTP,1), LABEL)
C        WRITE (HILINE,2020) TSKNAM, LABEL
         WRITE (HILINE,2020) TSKNAM, REFTYP
         CALL HIADD (LUN2, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2021) TSKNAM, CATOLD(KINAX,1)
         CALL HIADD (LUN2, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 50
C        WRITE (HILINE,2022) TSKNAM, OLDR(KRCRP,1)
         WRITE (HILINE,2022) TSKNAM, REFPIX
         CALL HIADD (LUN2, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 50
C        WRITE (HILINE,2023) TSKNAM, OLDR(KRCIC,1)
         WRITE (HILINE,2023) TSKNAM, REFINC
         CALL HIADD (LUN2, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 50
C        WRITE (HILINE,2024) TSKNAM, OLDD(KDCRV,1)
         WRITE (HILINE,2024) TSKNAM, REFVAL
         CALL HIADD (LUN2, HILINE, BUFFO, IERR)
         END IF
C                                       Close HI file
 50   CALL HICLOS (LUN2, T, BUFFO, IERR)
C                                        Update CATBLK and close
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'CLWR', SCRTCH, IERR)
      FRW(NCN) = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZMANHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BLC =',6(I6,','),I6)
 2001 FORMAT (A6,'TRC =',6(I6,','),I6)
 2003 FORMAT (A6,'FLUX =',1PE12.4,14X,'/ Flux cutoff in ',A)
 2004 FORMAT (A6,'INVERS =',I5,14X,'/ ZE table version number')
C 2009 FORMAT (A6,'DOMAX   =',F5.1,10X,'/ >0 -> Solve for gain')
 2010 FORMAT (A6,'OPTYPE = ''2SID''',10X,' / use 2-sided derivative')
 2011 FORMAT (A6,'OPTYPE = ''1SID''',10X,' / use 1-sided derivative')
 2012 FORMAT (A6,'OPTYPE = ''GAUS''',10X,' / use XGAUS solution')
 2013 FORMAT (A6,'IN2VERS = ',I3,10X,' / XG table version')
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
