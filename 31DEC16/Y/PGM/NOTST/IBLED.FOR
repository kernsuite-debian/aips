LOCAL INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   MAXVIS, MXCLIP
C                                       Declares size of editing arrays
      PARAMETER (MAXVIS = 32768)
C                                       Max number clipping points
      PARAMETER (MXCLIP=30)
C
      DOUBLE PRECISION FREQIF, CATID(128)
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC, XXSTOK,
     *   XNAM3I(3), XCLA3I(2), CATIH(256)
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, LNAME3*12, LCLAS3*6,
     *   XSTOK*4, STKFLG*4, SCRNMA*48, OUTNAM*12, OUTCLS*6,
     *   TTIME(2)*12, STNS(MAXANT)*8, OUTYPE*2, PSTYPE*2, USTFLG*4
C                                       Input parameters
      REAL      XSIN, XDISIN, XDOCAT, XINS2, XDOHST, XINS3, XIND3,
     *   XINV3, XNMAPS, XNCOMP(MAXAFL), XFLUX, XTIME(8), XBAND, XFREQ,
     *   XFQID, XBIF, XEIF, XBCHAN, XECHAN, XANT(50), XBASE(50),
     *   XUVRA(2), XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG,
     *   XFGOUT, XDOBND, XBPVER, XSMOTH(3), DPARM(10), XBADD(10), XDISOU
C                                       Arrays, real parms
      REAL      BUFFER(256), BUFF1(UVBFSS), BUFF2(UVBFSS),
     *   CATIR(256), START, STOP, SOLINT, MAXY, MINY, SCALY,
     *   PIXRNG(2,5), DELTAT, INTIME, MAXDY, MINDY, EPSTIM, UVSCAL
C                                       Main data storage arrays
      REAL      YVAL(MAXVIS,3), YVALM(MAXVIS), ERRORB(MAXVIS),
     *   TIMES(MAXVIS)
      INTEGER   SOUVAL(MAXVIS), YPIX(MAXVIS), YPIXR(2,MAXVIS),
     *   MFLAGD(MAXVIS), YPIX2(MAXVIS), YPIXM(MAXVIS), XPIXMR(MAXVIS),
     *   YPIXMR(MAXVIS)
C                                       Integer file control
      INTEGER   SEQIN, DISKIN, CNOIN, SEQOUT, DISKOU, CNOOUT, JBUFSZ,
     *   CATIMG(256), SCRTCH(MAXIMG), IMVONN(MAXIMG), IMVOFF(MAXIMG)
C                                       Integer program control
      INTEGER   NXANT, NXBASL, IXANT(50), IXBASL(50), LENBU, NSORT,
     *   NBUFF, LTYPE, LIF(2), LSTOKS, PLSTOK, PLIF(2), LCHAN(2),
     *   PLCHAN(2), ILSTOK(4), NV, NTRUE, PBASPT, MAXSOU, NUMBAS, BASPT,
     *   NXANT1(MXBASE), NXANT2(MXBASE), TVWIN(4), TVWIN2(4), LWINTV(4),
     *   PLTYPE, SLIF(2), SLCHAN(2), GRDF, GRDA, GRCV, GRME, GRLI,
     *   PWIND(4), IXD01, IXD02, IXD11, IXD12, IYD01, IYD02, IYD11,
     *   IYD12, METYPE, NEDGE, MEDGE, DAWIN(4), DAINC2, SCALX, IN3SEQ,
     *   IN3DIS, IN3VER, NMAPS, NCOMP(MAXFLD), FGVERI, FGVERO
C                                       FC stuff
      INTEGER   FCLUN, FCVERS, FCBUF(512), FCNUMB, FCBASL(2), FCSOUR,
     *   FCIF(2), FCCHAN(2), NNFLAG
      REAL      FCTIME(2), FCLIPR(2)
      HOLLERITH FCOPER(2), FCSFLG, FCTYPE(3)
C
      LOGICAL   LQUICK, DESEL, DOCHAN, DOIFS, GRDFOK, GRDAOK, GRCVOK,
     *   GRMEOK, GRLIOK, IFAVG, DOERRB, SHOWVS, FLALL1, FLALL2, GOTCAT,
     *   DATAOK, SCALOK, DOSOUR, PDOERR, CURSON, WMODEL, DOMODL, UPTR
C                                       Commons
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XDOCAT, XINS2,
     *   XDISOU, XDOHST, XXSOUR, XXCALC, XNAM3I, XCLA3I, XINS3, XIND3,
     *   XINV3, XNMAPS, XNCOMP, XFLUX, XTIME, XXSTOK, XBAND, XFREQ,
     *   XFQID, XBIF, XEIF, XBCHAN, XECHAN, XANT, XBASE, XUVRA, XSUBA,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XFGOUT, XDOBND,
     *   XBPVER, XSMOTH, DPARM, XBADD
      COMMON /CHARPM/ NAMEIN, CLAIN, XSOUR, LNAME3, LCLAS3, XSTOK,
     *   STKFLG, SCRNMA, OUTNAM, OUTCLS, OUTYPE, PSTYPE, TTIME, STNS,
     *   USTFLG
      COMMON /BUFFS/ BUFFER, BUFF1, BUFF2
      COMMON /INDEX/ NXANT1, NXANT2, NUMBAS
      COMMON /FCNTRL/ SEQIN, DISKIN, CNOIN, SEQOUT, DISKOU, CNOOUT,
     *   FCLUN, SCRTCH, IN3SEQ, IN3DIS, IN3VER, IMVONN, IMVOFF
      COMMON /IBLDPR/ CATIMG, FREQIF, START, STOP, MAXY, MINY, MAXDY,
     *   MINDY, SOLINT, SCALY, NXANT, NXBASL, IXANT, IXBASL, LENBU,
     *   NBUFF, NSORT, LWINTV, LTYPE, PIXRNG, SCALOK, JBUFSZ, LIF, PLIF,
     *   LSTOKS, PLSTOK, LCHAN, PLCHAN, ILSTOK, MAXSOU, FCVERS,
     *   FCBUF, NV, NTRUE, BASPT, PBASPT, PLTYPE, TVWIN, TVWIN2, SLIF,
     *   SLCHAN, GRDF, GRDA, GRCV, GRME, GRLI, DOSOUR, DOCHAN, DOIFS,
     *   DESEL, LQUICK, GRDFOK, GRDAOK, GRCVOK, GRMEOK, GRLIOK, IFAVG,
     *   DOERRB, PDOERR, SHOWVS, FLALL1, FLALL2, GOTCAT, DELTAT, PWIND,
     *   IXD01, IXD02, IXD11, IXD12, IYD01, IYD02, IYD11, IYD12, METYPE,
     *   NEDGE, MEDGE, DAWIN, DAINC2, DATAOK, NNFLAG, SCALX, EPSTIM,
     *   NMAPS, NCOMP, CURSON, WMODEL, DOMODL, INTIME, UVSCAL, FGVERI,
     *   FGVERO, UPTR
      COMMON /FCTABL/ FCTIME, FCLIPR, FCOPER, FCSFLG, FCTYPE, FCNUMB,
     *   FCBASL, FCSOUR, FCCHAN, FCIF
      COMMON /DSTORE/ YVAL, YVALM, ERRORB, TIMES, SOUVAL, YPIX, YPIXR,
     *   MFLAGD, YPIX2, YPIXM, XPIXMR, YPIXMR
      EQUIVALENCE (CATIMG, CATIR, CATIH, CATID)
LOCAL END
LOCAL INCLUDE 'DMOD.INC'
C                                       Local include for model
C                                       computation (CC)
      INTEGER MXCSTK, MXCC
C                                       Stokes (I,Q,U,V,R,L)
      PARAMETER (MXCSTK = 6)
C                                       Max. no. CC components
      PARAMETER (MXCC = 20000)
C
      CHARACTER LCSTOK(MXCSTK)
      LOGICAL DOSPHE, DOGAUS
      REAL      CCPOS(3,MXCC), SFLUX(MXCC), GAUSA(MXCC), GAUSB(MXCC),
     *   GAUSC(MXCC)
      INTEGER   IMSTRT(MXCSTK), IMEND(MXCSTK), IMNEXT, NCSTOK
      COMMON /MODCC/ CCPOS, SFLUX, GAUSA, GAUSB, GAUSC, IMSTRT,
     *   IMEND, IMNEXT, NCSTOK, DOSPHE, DOGAUS
      COMMON /MODCH/ LCSTOK
LOCAL END
      PROGRAM IBLED
C-----------------------------------------------------------------------
C! Performs interactive editing of data baseline-by-baseline using TV
C# Utility UV UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2003, 2005-2012, 2014-2016
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
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      APARM(10)      APARM         User specified array.
C      BPARM(10)      BPARM         User specified array.
C      BOX(4,10)      BOX           User specified array.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   NBASE, IA1, IA2, IRET, IERR, NIFO, NWORDS
      LONGINT   OFFRE, OFFIM, OFFAM, OFFWT, OFFPRM
      REAL      RESUM(2), IMSUM(2), AMSUM(2), WTSUM(2), PARMS(2)
      LOGICAL   ONECHN(2)
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'IBLED '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL IBLDIN (PRGM, ONECHN, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Calibrate and select the
C                                       first requested baseline.
      IF ((IXANT(1).GT.0) .AND. (IXBASL(1).GT.0)) THEN
         IA1 = IXANT(1)
         IA2 = IXBASL(1)
      ELSE
         IA1 = NXANT1(1)
         IA2 = NXANT2(1)
         END IF
      NBASE = NUMBAS
      CALL DECPTR (IA1, IA2, NXANT1, NXANT2, NBASE, BASPT)
C                                       get data to work
      IF (.NOT.GOTCAT) THEN
C                                       use dynamic memory
         NIFO = EIF - BIF + 1
         IF (IFAVG) NIFO = 1
         NWORDS = (4 * NIFO * NUMBAS - 1) / 1024 + 1
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, RESUM,
     *      OFFRE, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, IMSUM,
     *      OFFIM, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, AMSUM,
     *      OFFAM, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, WTSUM,
     *      OFFWT, IRET)
         NWORDS = (20 * NUMBAS - 1) / 1024 + 1
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, PARMS,
     *      OFFPRM, IRET)
         IF (IRET.EQ.0) CALL CALAVG (NIFO, RESUM(1+OFFRE),
     *      IMSUM(1+OFFIM), AMSUM(1+OFFAM), WTSUM(1+OFFWT),
     *      PARMS(1+OFFPRM), IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, RESUM,
     *      OFFRE, IERR)
         END IF
C                                       Do the editing if not just
C                                       setting up work files
      IF ((.NOT.RQUICK) .AND. (IRET.EQ.0)) CALL IBFLGR (ONECHN, IRET)
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE IBLDIN (PRGM, ONECHN, IRET)
C-----------------------------------------------------------------------
C   IBLDIN gets the inputs for IBLED
C   Inputs:
C      PRGM   C*6   Task name
C   Output:
C      IRET   I     Error code: 0 ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      LOGICAL   ONECHN(2)
      INTEGER   IRET
C
      INCLUDE 'IBLED.INC'
      CHARACTER STAT*4, HDRKEY(4)*8, LSTOK*6
      INTEGER   NPARM, IERR, IROUND, I, LUN, HDRVAL(4), NHV, HDRTYP(4),
     *   HDRLOC(4), NN, JBUFFA(1024), JBUFFB(512), JLUNA, JLUNB
      LOGICAL   MATCH, WASOME
      REAL      CATUR(256), CATR(256)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
      EQUIVALENCE (CATUV, CATUR), (CATBLK, CATR)
      DATA HDRKEY /'BCHAN','ECHAN','BIF','EIF'/
      DATA JLUNA, JLUNB /40, 41/
C-----------------------------------------------------------------------
      FCLUN = 27
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      GOTCAT = .FALSE.
      UPTR = .TRUE.
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 292 + MAXAFL
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      LQUICK = RQUICK
      RQUICK = .FALSE.
      IF (IRET.NE.0) GO TO 999
      IF ((NPOPS.GT.NINTRN) .OR. (NTVDEV.LE.0)) THEN
         IRET = 4
         IF (NTVDEV.LE.0) MSGTXT =
     *      'YOU HAVE NOT BEEN ASSIGNED A TV'
         IF (NPOPS.GT.NINTRN) MSGTXT =
     *      'TV TASKS ARE RESERVED FOR INTERACTIVE USERS'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IRET = 5
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAM3I, LNAME3)
      CALL H2CHR (6, 1, XCLA3I, LCLAS3)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 10      CONTINUE
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      DISKOU = IROUND (XDISOU)
      IN3SEQ = IROUND (XINS3)
      IN3DIS = IROUND (XIND3)
      IN3VER = IROUND (XINV3)
      NMAPS = MIN (IROUND (XNMAPS), MAXFLD)
      IF (LNAME3.NE.' ') NMAPS = MAX (1, NMAPS)
      LIMFLX = XFLUX
      NONEG = .FALSE.
      WASOME = .FALSE.
      DO 15 I = 1,NMAPS
         IF (I.LE.MAXAFL) THEN
            NCOMP(I) = IROUND (ABS(XNCOMP(I)))
            IF (XNCOMP(I).LT.-0.5) NONEG = .TRUE.
            IF (NCOMP(I).GT.0) WASOME = .TRUE.
         ELSE
            NCOMP(I) = 0
            IF (WASOME) NCOMP(I) = 1000000000
            END IF
 15      CONTINUE
C                                       Get CATBLK.
      CNOIN = 1
      OUTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, OUTYPE,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.5) THEN
            WRITE (MSGTXT,1015) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *         NLUSER
         ELSE
            WRITE (MSGTXT,1016) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER
            END IF
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Is this image TB?
      IF (ISORT(:1).NE.'T') THEN
         IRET = 1
         WRITE (MSGTXT,1025) ISORT
         GO TO 990
         END IF
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      IUDISK = DISKIN
      IUCNO = CNOIN
      USEQ = SEQIN
      IUSEQ = SEQIN
      DO 30 I = 1,30
         SOURCS(I) = XSOUR(I)
 30      CONTINUE
      CALL RCOPY (8, XTIME, TIMRNG)
      CALL H2CHR (4, 1, XXCALC, SELCOD)
      UVRNG(1) = XUVRA(1)
      UVRNG(2) = XUVRA(2)
      STOKES = XSTOK
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      ONECHN(1) = CATBLK(KINAX+JLOCF).LE.1
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
         ONECHN(2) = .TRUE.
      ELSE
         BIF = IROUND (XBIF)
         BIF = MAX (1, BIF)
         BIF = MIN (BIF, CATBLK(KINAX+JLOCIF))
         EIF = IROUND (XEIF)
         IF ((EIF.LT.BIF) .OR. (EIF.GT.CATBLK(KINAX+JLOCIF)))
     *      EIF = CATBLK(KINAX+JLOCIF)
         ONECHN(2) = (CATBLK(KINAX+JLOCIF).LE.1)
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
C                                       Set cal flag
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
C                                       Get autocorrelations
      DOACOR = DPARM(2).GT.0.0
C                                       Antennas
C                                       IXANT(1) and IXBASL(1)
C                                       are the baseline(s)
C                                       required. If they are 0
C                                       then we start with NXANT1(1)
C                                       and NXANT2(1)
      CALL SETANT (50, XANT, XBASE, NXANT, NXBASL, IXANT, IXBASL,
     *   DESEL)
      CALL FILL (50, 0, ANTENS)
C                                       Fill in list of all antenna
C                                       - baseline pairs and names
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      CALL FILANT (DISKIN, CNOIN, CATBLK, LUN, IXANT, IXBASL, NXANT,
     *   NXBASL, DESEL, SUBARR, DOACOR, NUMBAS, NXANT1, NXANT2, STNS,
     *   SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 999
C
C                                       set flag versions
      CALL FNDEXT ('FG', CATBLK, I)
      FGVER = IROUND (XFLAG)
      IF ((FGVER.EQ.0) .OR. (FGVER.GT.I)) FGVER = I
      FGVERO = IROUND (XFGOUT)
      IF ((FGVERO.LE.0) .OR. (FGVERO.GT.I)) FGVERO = I + 1
      FGVERI = FGVER
      IF (FGVERO.LE.I) FGVERI = -ABS (FGVERI)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
C                                       Spectral smoothing
      CALL RCOPY (3, XSMOTH, SMOOTH)
C
      DO 80 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 80      CONTINUE
      CALL COPY (256, CATBLK, CATUV)
C                                       locate old master file
      SEQOUT = IROUND (XINS2)
      CNOOUT = 1
      IF ((XDOCAT.GT.0.0) .AND. (SEQOUT.GT.0)) THEN
         OUTCLS = 'IBLEDR'
         OUTNAM = NAMEIN
         OUTYPE = 'UV'
         CALL CATDIR ('SRCH', DISKOU, CNOOUT, OUTNAM, OUTCLS, SEQOUT,
     *      OUTYPE, NLUSER, STAT, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            IF (IERR.NE.5) THEN
               WRITE (MSGTXT,1015) IERR, OUTNAM, OUTCLS, SEQOUT,
     *            DISKOU, NLUSER
            ELSE
               WRITE (MSGTXT,1016) OUTNAM, OUTCLS, SEQOUT, DISKOU,
     *            NLUSER
               END IF
            CALL MSGWRT (6)
            SEQOUT = 0
         ELSE
            CALL CATIO ('READ', DISKOU, CNOOUT, CATBLK, 'WRIT', SCRTCH,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020) IERR
               IRET = 5
               GO TO 990
               END IF
            CALL COPY (256, CATBLK, CATIMG)
            GOTCAT = .TRUE.
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = DISKOU
            FCNO(NCFILE) = CNOOUT
            FRW(NCFILE) = 1
C                                       set BCHAN, BIF
            BCHAN = CATUR(KRCRP+JLOCF) - CATR(KRCRP+3) + 1.1
            ECHAN = CATBLK(KINAX+JLOCF) + BCHAN - 1
            IF (JLOCIF.GE.0) THEN
               BIF = CATUR(KRCRP+JLOCIF) - CATR(KRCRP+2) + 1.1
               EIF = CATBLK(KINAX+JLOCIF) + BIF - 1
            ELSE
               BIF = 1
               EIF = 1
               END IF
            NHV = 4
            CALL CATKEY ('READ', DISKOU, CNOOUT, HDRKEY, NHV, HDRLOC,
     *         HDRVAL, HDRTYP, SCRTCH, IRET)
            IF ((IRET.GT.0) .AND. (IRET.LE.20)) THEN
               WRITE (MSGTXT,1055) IRET
               GO TO 990
               END IF
            IF (HDRLOC(1).GT.0) BCHAN = HDRVAL(HDRLOC(1))
            IF (HDRLOC(2).GT.0) ECHAN = HDRVAL(HDRLOC(2))
            IF (HDRLOC(3).GT.0) BIF = HDRVAL(HDRLOC(3))
            IF (HDRLOC(4).GT.0) EIF = HDRVAL(HDRLOC(4))
            END IF
         END IF
C                                       Quick return mode?
      IF ((XDOCAT.GT.1.5) .AND. (SEQOUT.LE.0)) THEN
         RQUICK = .TRUE.
         CALL RELPOP (IRET, SCRTCH, IERR)
         END IF
C                                       IF averaging
      IFAVG = (DPARM(3).GT.0.0) .AND. (EIF.GT.BIF)
C                                       Time averaging
      SOLINT = MAX (0.0, DPARM(5)) / (24.0 * 60.0 * 60.0)
CCC      DPARM(5) = 0.
C                                       Error bars?
      DOERRB = (DPARM(6).GT.0.0)
C                                       Full vis fn.
      SHOWVS = (DPARM(9).LE.0.0)
C                                       Init start/stop arrays
      START = 1.0E10
      STOP = -1.0E10
C                                       Load CC model comp. if selected
      CALL CHBLNK (12, 1, LNAME3, NN)
      WMODEL = (NN.GT.0)
      IF (WMODEL) THEN
         LSTOK = 'IRL'
         CALL COPY (256, CATUV, CATBLK)
         CALL READCC (LSTOK, LNAME3, LCLAS3, IN3SEQ, IN3DIS, IN3VER,
     *      SOUWAN, JLUNB, JBUFFA, JBUFFB, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C
      GO TO 999
C                                       message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IBLDIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1015 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I5,' DISK=',
     *   I3,' USID=',I5)
 1016 FORMAT (A12,'.',A6,'.',I5,' DISK=',I3,' USID=',I5,' NOT FOUND')
 1020 FORMAT ('IBLDIN: ERROR',I3,' READING CATBLK ')
 1025 FORMAT ('DATA IN ',A2,' SORT ORDER, NOT THE REQUIRED T? ORDER')
 1055 FORMAT ('ERROR',I3,' READING KEYWORDS FROM OLD HEADER')
      END
      SUBROUTINE FILANT (DISK, CNO, CATBLK, LUN, IXANT, IXBASL, NXANT,
     *   NXBASL, DESEL, NSUBA, DOACOR, NBASE, ANT1, ANT2, STNS, SCRTCH,
     *   IRET)
C-----------------------------------------------------------------------
C   Determines the number of subarrays in a data set from the number
C   of AN files and returns the highest antennas number in each subarray
C   If no antennas are found, one subarray with 28 antennas assumed.
C   If an error occurs, information about subarrays from AN files found
C   is returned; although an error code is returned. Also fills in 2
C   arrays with all possible cominations of antenna numbers
C   Inputs:
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      CATBLK   I(256)   Catalog header block.
C      LUN      I        Logical unit number to use
C      NSUBA    I        Subarray used
C      DOACOR   L        Do autocorrelations?
C   Output:
C      NBASE    I        Max # baselines
C      ANT1     I(*)     1st antenna number of baseline pairs selected
C      ANT2     I(*)     2nd antenna number of baseline pairs selected
C      STNS     C(*)*8   station names
C      SCRTCH   I(512)   I/O buffer and related storage.
C      IRET     I        Return error code, 0 => ok,
C                           else TABINI or TABIO error.
C                           10 = no AN files.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(256), LUN, IXANT(50), IXBASL(50),
     *   NXANT, NXBASL, NSUBA, NBASE, ANT1(*), ANT2(*), SCRTCH(512),
     *   IRET
      LOGICAL   DESEL, DOACOR
      CHARACTER STNS(*)*8
C
      INTEGER   NBUFF, II, NUMREC, J, MXNSTA, I1, IERR
      LOGICAL   ACCEPT, REQBAS
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
C-----------------------------------------------------------------------
C                                       Set default results.
      CALL FILL (MXBASE, 0, ANT1)
      CALL FILL (MXBASE, 0, ANT2)
      NBUFF = 1024
C                                       read the antenna file
C                                       Open file
      CALL ANTINI ('READ', SCRTCH, DISK, CNO, NSUBA, CATBLK, LUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'NO AN FILE: USING DUMMY PARAMETERS'
         CALL MSGWRT (6)
         MXNSTA = 28
         DO 10 II = 1,28
            WRITE (STNS(II),1005) II
 10         CONTINUE
C                                       Get # of antennas in subarray.
      ELSE
         NUMREC = SCRTCH(5)
         MXNSTA = 1
         DO 20 II = 1,NUMREC
            CALL TABAN ('READ', SCRTCH, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ', NSUBA
               GO TO 990
               END IF
            MXNSTA = MAX (NOSTA, MXNSTA)
            STNS(NOSTA) = ANNAME
 20         CONTINUE
C                                       Close
         CALL TABIO ('CLOS', 0, II, SCRTCH, SCRTCH, IERR)
         END IF
C                                       Fill up the baseline arrays
      DO 40 I1 = 1,MXNSTA
         DO 30 J = I1,MXNSTA
            IF ((I1.LT.J) .OR. ((DOACOR) .AND. (I1.EQ.J))) THEN
               ACCEPT = REQBAS (I1, J, DESEL, IXANT, NXANT, IXBASL,
     *            NXBASL)
               IF (ACCEPT) THEN
                  NBASE = NBASE + 1
                  ANT1(NBASE) = I1
                  ANT2(NBASE) = J
                  END IF
               END IF
 30         CONTINUE
 40      CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FILANT: ERROR',I3,1X,A4,'ING AN FILE ',I5)
 1005 FORMAT ('DUMMY ',I2.2)
      END
      LOGICAL FUNCTION DOSTOK (ISTVAL, STKFLG, IST)
C-----------------------------------------------------------------------
C   returns whether the current Stokes flag pattern applies to this
C   stokes axis position
C   Inputs:
C      ISTVAL   I(4)   Stokes values in master file
C      STKFLG   C*4    Flag command string (1's and 0's only)
C      IST      I      Current stokes axis position
C   Output:
C      DOSTOK   L      Current Stokes included in STKFLG ?
C-----------------------------------------------------------------------
      INTEGER   ISTVAL(4), IST
      CHARACTER STKFLG*4
C
      INTEGER   I
C-----------------------------------------------------------------------
      I = ABS (ISTVAL(IST))
      IF (I.GT.4) I = I - 4
      DOSTOK = STKFLG(I:I).EQ.'1'
C
 999  RETURN
      END
      SUBROUTINE MKSTOK (ISTVAL, USTFLG, STKFLG, IRET)
C-----------------------------------------------------------------------
C   Converts the user's character string for Stokes flag into 1'a and
C   0's if possible and reasonable.  Note that the 1 and 0 string
C   assumes that the first correlator is I, RR, or XX.  Finding the
C   correct flag for the current actual Stokes is done by DOSTOK in
C   IBLED and by the flag routines in the calibration package.
C   Inputs:
C      ISTVAL   I(4)   Stokes values in master file
C      USTFLG   C*4    User's flag command string
C   Output:
C      STKFLG   C*4    Flag command string: 1s and 0s
C      IRET     I      Error code: 0 ok, 1 unrecognized string
C-----------------------------------------------------------------------
      INTEGER   ISTVAL(4), IRET
      CHARACTER USTFLG*4, STKFLG*4
C
      CHARACTER CHSTO1(15)*4, CHSTOI(7)*4, CHSTOR(9)*4, CHSTOX(9)*4
      INTEGER   I
      DATA CHSTO1 /'1000', '0100', '0010', '0001', '1100', '1010',
     *   '1001', '0110', '0101', '0011', '1110', '1101', '1011', '0111',
     *   '1111'/
      DATA CHSTOI /'I', 'Q', 'U', 'V', 'IQU', 'IQUV', 'IV'/
      DATA CHSTOR / 'RR', 'LL', 'RL', 'LR', 'HALF', 'NOLL', 'NORR',
     *   'RRLL', 'RLLR'/
      DATA CHSTOX / 'XX', 'YY', 'XY', 'YX', 'HALF', 'NOYY', 'NOXX',
     *   'XXYY', 'XYYX'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       already 1s and 0s
      DO 10 I = 1,15
         IF (USTFLG.EQ.CHSTO1(I)) THEN
            STKFLG = USTFLG
            GO TO 999
            END IF
 10      CONTINUE
C                                       FULL
      IF (USTFLG.EQ.'FULL') THEN
         STKFLG = '1111'
         GO TO 999
         END IF
C                                       check it out
      IF (ISTVAL(1).GT.0) THEN
         DO 20 I = 1,7
            IF (USTFLG.EQ.CHSTOI(I)) THEN
               STKFLG = CHSTO1(I)
               IF (I.EQ.5) STKFLG = CHSTO1(11)
               IF (I.EQ.6) STKFLG = CHSTO1(15)
               GO TO 999
               END IF
 20         CONTINUE
      ELSE IF (ISTVAL(1).LE.-5) THEN
         DO 30 I = 1,9
            IF (USTFLG.EQ.CHSTOX(I)) THEN
               STKFLG = CHSTO1(I)
               IF (I.EQ.6) STKFLG = CHSTO1(13)
               IF (I.EQ.7) STKFLG = CHSTO1(14)
               IF (I.EQ.8) STKFLG = CHSTO1(5)
               IF (I.EQ.9) STKFLG = CHSTO1(10)
               GO TO 999
               END IF
 30         CONTINUE
      ELSE
         DO 40 I = 1,7
            IF (USTFLG.EQ.CHSTOR(I)) THEN
               STKFLG = CHSTO1(I)
               IF (I.EQ.6) STKFLG = CHSTO1(13)
               IF (I.EQ.7) STKFLG = CHSTO1(14)
               IF (I.EQ.8) STKFLG = CHSTO1(5)
               IF (I.EQ.9) STKFLG = CHSTO1(10)
               GO TO 999
               END IF
 40         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE CVSTOK (INT, INSTOK, OUT, OUTSTK)
C-----------------------------------------------------------------------
C   converts a Stokes flag mask (0s and 1s) from 1st Stokes type to 2nd
C   Inputs:
C      INT      I     Input type: ICOR0 value
C      INSTOK   C*4   Input stokes flag mask
C      OUT      I     Output type: ICOR0 value for output
C   Outputs:
C     OUTSTK    C*4   Stokes flag mask to use with output type
C-----------------------------------------------------------------------
      INTEGER   INT, OUT
      CHARACTER INSTOK*4, OUTSTK*4
C
C-----------------------------------------------------------------------
      OUTSTK = '0000'
C                                       input I type
      IF (INT.GT.0) THEN
         IF (OUT.GT.0) THEN
            OUTSTK = INSTOK
         ELSE IF (OUT.GE.-4) THEN
            IF (INSTOK(1:1).EQ.'1') OUTSTK(1:2) = '11'
            IF (INSTOK(2:2).EQ.'1') OUTSTK(3:4) = '11'
            IF (INSTOK(3:3).EQ.'1') OUTSTK(3:4) = '11'
            IF (INSTOK(4:4).EQ.'1') OUTSTK(1:2) = '11'
         ELSE
            IF (INSTOK(1:3).NE.'000') OUTSTK(1:2) = '11'
            IF (INSTOK(2:4).NE.'000') OUTSTK(3:4) = '11'
            END IF
C                                       input RR type
      ELSE IF (INT.GE.-4) THEN
         IF (OUT.GT.0) THEN
            IF (INSTOK(1:2).EQ.'11') OUTSTK(1:1) = '1'
            IF ((INSTOK(1:1).EQ.'1') .OR. (INSTOK(2:2).EQ.'1'))
     *         OUTSTK(4:4) = '1'
            IF ((INSTOK(3:3).EQ.'1') .OR. (INSTOK(4:4).EQ.'1'))
     *         OUTSTK(2:3) = '11'
         ELSE IF (OUT.GE.-4) THEN
            OUTSTK = INSTOK
         ELSE
            IF (INSTOK.NE.'0000') OUTSTK = '1111'
            END IF
C                                       input XX type
      ELSE
         IF (OUT.GT.0) THEN
            IF ((INSTOK(1:1).EQ.'1') .OR. (INSTOK(2:2).EQ.'1'))
     *         OUTSTK(1:1) = '1'
            IF ((INSTOK(3:3).EQ.'1') .OR. (INSTOK(4:4).EQ.'1'))
     *         OUTSTK(4:4) = '1'
            IF (INSTOK.NE.'0000') OUTSTK(2:3) = '11'
         ELSE IF (OUT.GE.-4) THEN
            IF (INSTOK.NE.'0000') OUTSTK = '1111'
         ELSE
            OUTSTK = INSTOK
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE CALAVG (NIFO, RESUM, IMSUM, AMSUM, WTSUM, RPARMS, IRET)
C-----------------------------------------------------------------------
C   Calibrate, average data to work file
C   Inputs:
C      NIFO     I      Number output IFs
C   Scratch:
C      RESUM    R(*)   (4,NIFO,MaxBase) - real sum
C      IMSUM    R(*)   (4,NIFO,MaxBase) - imag sum
C      AMSUM    R(*)   (4,NIFO,MaxBase) - ampl sum
C      WTSUM    R(*)   (4,NIFO,MaxBase) - weight sum
C      RPARMS   R(*)   (4,MaxBase) - random parms by baseline
C   Output:
C      IRET     I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NIFO, IRET
      REAL      RESUM(4,NIFO,*), IMSUM(4,NIFO,*), AMSUM(4,NIFO,*),
     *   WTSUM(4,NIFO,*), RPARMS(20,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IPTRO, LUNSC, INDSC, LRECSC, ILENBU, KBIND, NIOUT, IV,
     *   NIOLIM, BO, VO, NUMVIS, XCOUNT, CATSAV(256), LVISSC,
     *   IANT1, IANT2, HDRVAL(4), NC, HDRTYP(4), HDRLOC(4), NIF, IFO,
     *   NS, NCH, IB, NOW(MXBASE), IIS, IIF, IIN, IISF, IIC,
     *   INDEX, IBL, JRET, ISOU, LSOU, I, IIV
      LOGICAL   T, F, ISAUTO
      REAL      RPARM(20), CATR(256), CATUVR(256), TIME, LTIME, RT, IT,
     *   AT, WT, DATB(4,4*MAXIF), CATSR(256)
      HOLLERITH CATH(256), CATUVH(256), CATSH(256)
      DOUBLE PRECISION    CATD(128), CATUVD(128), CATSD(128)
      CHARACTER HDRKEY(4)*8
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      EQUIVALENCE (CATSAV, CATSH, CATSR, CATSD)
      EQUIVALENCE (CATUV, CATUVH, CATUVR, CATUVD)
      DATA LUNSC /17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
      DATA HDRKEY /'BCHAN','ECHAN','BIF','EIF'/
C-----------------------------------------------------------------------
      MSGTXT = 'Copying'
      NC = 8
      IF (XDOCAL.GT.0.0) THEN
         MSGTXT(NC:) = '/calibrating'
         NC = NC + 12
         END IF
      IF ((ECHAN.GT.BCHAN) .OR. (IFAVG) .OR. (SOLINT.GT.0.0)) THEN
         MSGTXT(NC:) = '/averaging'
         NC = NC + 10
         END IF
      MSGTXT(NC:) = ' selected data to'
      NC = NC + 17
      IF (XDOCAT.LE.0.0) THEN
         MSGTXT(NC:) = ' temporary cataloged file'
      ELSE
         MSGTXT(NC:) = ' cataloged file'
         END IF
      CALL MSGWRT (3)
C                                       Initilize input file for
C                                       reading.
      LTIME = -1.0E6
      CALL UVGET ('INIT', RPARM, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT'
         GO TO 990
         END IF
      UVSCAL = FREQ / UVFREQ
C                                       Make master file header
      CALL COPY (256, CATBLK, CATSAV)
      CATD(KDCRV) = 3.0D0
      CATR(KRCRP) = 1.0
      CATR(KRCIC) = 1.0
      CATBLK(KINAX) = 4
      IF (KLOCIF.GE.0) THEN
         CALL RCOPY (2, CATUVH(KHCTP+2*KLOCIF), CATH(KHCTP+4))
         CATD(KDCRV+2) = CATUVD(KDCRV+KLOCIF)
         CATR(KRCRP+2) = CATR(KRCRP+KLOCIF) - BIF + 1
         CATR(KRCIC+2) = CATR(KRCIC+KLOCIF)
         IF (IFAVG) THEN
            CATBLK(KINAX+2) = 1
         ELSE
            CATBLK(KINAX+2) = EIF - BIF + 1
            END IF
      ELSE
         CALL CHR2H (8, 'IF      ', 1, CATH(KHCTP+4))
         CATD(KDCRV+2) = 1.0D0
         CATR(KRCRP+2) = 1.0
         CATR(KRCIC+2) = 1.0
         CATBLK(KINAX+2) = 1
         END IF
      CALL RCOPY (2, CATUVH(KHCTP+2*KLOCFY), CATH(KHCTP+6))
      CATD(KDCRV+3) = CATUVD(KDCRV+KLOCFY)
      CATR(KRCRP+3) = CATUVR(KRCRP+KLOCFY) - BCHAN + 1
      CATR(KRCIC+3) = CATUVR(KRCIC+KLOCFY)
      CATBLK(KINAX+3) = 1
      CALL RCOPY (2, CATSH(KHCTP+2*JLOCR), CATH(KHCTP+8))
      CATD(KDCRV+4) = CATSD(KDCRV+JLOCR)
      CATR(KRCRP+4) = CATSR(KRCRP+JLOCR)
      CATR(KRCIC+4) = CATSR(KRCIC+JLOCR)
      CATBLK(KINAX+4) = 1
      CALL RCOPY (2, CATSH(KHCTP+2*JLOCD), CATH(KHCTP+10))
      CATD(KDCRV+5) = CATSD(KDCRV+JLOCD)
      CATR(KRCRP+5) = CATSR(KRCRP+JLOCD)
      CATR(KRCIC+5) = CATSR(KRCIC+JLOCD)
      CATBLK(KINAX+5) = 1
      CATBLK(KIDIM) = 6
      CALL MAKOUT (UNAME, UCLAS, IUSEQ, ' ', OUTNAM, OUTCLS, SEQOUT)
      OUTCLS = 'IBLEDR'
      CALL CHR2H (12, OUTNAM, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, OUTCLS, KHIMCO, CATH(KHIMC))
      IF (XDOCAT.LE.0.0) SEQOUT = 0
      CATBLK(KIIMS) = SEQOUT
      OUTYPE = 'UV'
C                                       support BADDISK
      IIV = 0
      IF (DISKOU.GT.0) THEN
         IV = DISKOU
         IIV = IV
         CALL UVCREA (IV, CNOOUT, SCRTCH, IRET)
      ELSE
         DO 65 IV = 1,NVOL
            DO 60 I = 1,10
               IF (IV.EQ.IBAD(I)) GO TO 65
 60            CONTINUE
            MSGSUP = 32000
            IIV = IV
            CALL UVCREA (IV, CNOOUT, SCRTCH, IRET)
            MSGSUP = 0
            IF (IRET.NE.1) GO TO 70
 65         CONTINUE
         IRET = 1
         END IF
 70   SEQOUT = CATBLK(KIIMS)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         IF (IRET.EQ.1) THEN
            CALL MSGWRT (8)
            MSGTXT = 'NO DISK SPACE ON ALLOWED DISKS'
            IF (IIV.LE.0) MSGTXT = 'BADDISK LEAVES NO DISKS TO USE'
            END IF
         IRET = 3
         GO TO 990
         END IF
      DISKOU = IV
      IF (XDOCAT.GT.0.0) THEN
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKOU
         FCNO(NCFILE) = CNOOUT
         FRW(NCFILE) = 2
      ELSE
         NSCR = NSCR + 1
         SCRVOL(NSCR) = DISKOU
         SCRCNO(NSCR) = CNOOUT
         END IF
C                                       save B/Echan and IF
      HDRVAL(1) = BCHAN
      HDRVAL(2) = ECHAN
      HDRVAL(3) = BIF
      HDRVAL(4) = EIF
      HDRLOC(1) = 1
      HDRLOC(2) = 2
      HDRLOC(3) = 3
      HDRLOC(4) = 4
      CALL FILL (4, 4, HDRTYP)
      CALL CATKEY ('WRIT', DISKOU, CNOOUT, HDRKEY, 4, HDRLOC, HDRVAL,
     *   HDRTYP, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1055) IRET
         GO TO 990
         END IF
C                                       open output
      CALL ZPHFIL ('UV', DISKOU, CNOOUT, 1, SCRNMA, IRET)
      CALL ZOPEN (LUNSC, INDSC, DISKOU, SCRNMA, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
C                                       LRECSC = length of output rec.
      LVISSC = 4 * CATBLK(KINAX+1) * CATBLK(KINAX+2)
      LRECSC = LVISSC + NRPARM
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNSC, INDSC, NVIS, VO, LRECSC, ILENBU,
     *   JBUFSZ, BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
      NUMVIS = 0
      XCOUNT = 0
      NCH = ECHAN - BCHAN + 1
      NIF = EIF - BIF + 1
      CALL FILL (NUMBAS, 0, NOW)
      NS = CATBLK(KINAX+1)
      INTIME = DPARM(4) / (24.0 * 60.0 * 60.0)
      IF (SOLINT.LE.0.0) SOLINT = 10.0 / (24.0 * 60.0 * 60.0)
      IF (INTIME.LE.0.0) INTIME = 10.0 / (24.0 * 60.0 * 60.0)
      ISOU = -100
      IF (ILOCSU.LT.0) ISOU = 0
      LSOU = ISOU
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVGET ('READ', RPARM, BUFF1, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ'
            GO TO 990
            END IF
C                                       Determine antenna numbers
         IF (IRET.EQ.0) THEN
            IF (ILOCB.GE.0) THEN
               IANT1 = RPARM(ILOCB+1) / 256.0 + 0.1
               IANT2 = RPARM(ILOCB+1) - 256 * IANT1 + 0.1
            ELSE
               IANT1 = RPARM(ILOCA1+1) + 0.1
               IANT2 = RPARM(ILOCA2+1) + 0.1
               END IF
C                                       find baseline
            CALL DECPTR (IANT1, IANT2, NXANT1, NXANT2, NUMBAS, IBL)
            IF (IBL.LE.0) GO TO 100
            TIME = RPARM(1+ILOCT)
            IF (ILOCSU.GE.0) ISOU = RPARM(1+ILOCSU) + 0.5
            RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
            RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
            RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
C                                       force average
         ELSE
            TIME = 1.0E10
            END IF
         JRET = IRET
         IRET = 0
C                                       output time?
         IF ((TIME-LTIME.GT.SOLINT) .OR. (ISOU.NE.LSOU)) THEN
C                                       got data - average it
            DO 170 IB = 1,NUMBAS
               IIN = 0
               IF (NOW(IB).GT.0) THEN
                  ISAUTO = NXANT1(IB).EQ.NXANT2(IB)
                  DO 160 IIF = 1,NIFO
                     DO 155 IIS = 1,NS
                        IISF = IIS + (IIF - 1) * NS
                        IF (WTSUM(IIS,IIF,IB).GT.0.0) THEN
                           RT = RESUM(IIS,IIF,IB) / WTSUM(IIS,IIF,IB)
                           IT = IMSUM(IIS,IIF,IB) / WTSUM(IIS,IIF,IB)
                           AT = AMSUM(IIS,IIF,IB) / WTSUM(IIS,IIF,IB)
                           DATB(1,IISF) = WTSUM(IIS,IIF,IB)
                           IF (ISAUTO) THEN
                              DATB(2,IISF) = RT
                              DATB(3,IISF) = 0.0
                              DATB(4,IISF) = SQRT (MAX (AT - RT*RT, 0.))
                              IF (AT.NE.0.0) IIN = IIN + 1
                           ELSE
                              DATB(2,IISF) = SQRT (RT*RT + IT*IT)
                              IF ((RT.NE.0.0) .OR. (IT.NE.0.0)) THEN
                                 DATB(3,IISF) = ATAN2 (IT, RT) *
     *                              57.29578
                                 DATB(4,IISF) = DATB(2,IISF) / AT
                                 IIN = IIN + 1
                              ELSE
                                 DATB(3,IISF) = 0.0
                                 DATB(4,IISF) = 0.0
                                 END IF
                              END IF
                        ELSE
                           DATB(1,IISF) = -1.0
                           DATB(2,IISF) = FBLANK
                           DATB(3,IISF) = FBLANK
                           DATB(4,IISF) = FBLANK
                           END IF
 155                    CONTINUE
 160                 CONTINUE
                  END IF
C                                       got a record: "write it"
               IF (IIN.GT.0) THEN
                  CALL RCOPY (NRPARM, RPARMS(1,IB), BUFF2(IPTRO))
                  CALL RCOPY (LVISSC, DATB, BUFF2(IPTRO+NRPARM))
                  IPTRO = IPTRO + LRECSC
                  NIOUT = NIOUT + 1
                  NUMVIS = NUMVIS + 1
C                                       Write vis record.
                  IF (NIOUT.EQ.NIOLIM) THEN
                     CALL UVDISK ('WRIT', LUNSC, INDSC, BUFF2, NIOLIM,
     *                  KBIND, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1150) IRET
                        GO TO 990
                        END IF
                     IPTRO = KBIND
                     NIOUT = 0
                     END IF
                  END IF
 170           CONTINUE
C                                       zero counter
            CALL FILL (NUMBAS, 0, NOW)
            LTIME = MAX (LTIME + SOLINT, TIME - INTIME / 2.0)
            LSOU = ISOU
            END IF
C                                       exit signalled
         IF (JRET.EQ.-1) GO TO 300
C                                       init new baseline
         IB = IBL
         IF ((IB.GT.0) .AND. (NOW(IB).LE.0)) THEN
            IIN = 4 * NIFO
            CALL RFILL (IIN, 0.0, RESUM(1,1,IB))
            CALL RFILL (IIN, 0.0, IMSUM(1,1,IB))
            CALL RFILL (IIN, 0.0, AMSUM(1,1,IB))
            CALL RFILL (IIN, 0.0, WTSUM(1,1,IB))
            CALL RCOPY (NRPARM, RPARM, RPARMS(1,IB))
            RPARMS(1+ILOCT,IB) = LTIME + SOLINT / 2.0
            END IF
C                                       average data in
         ISAUTO = NXANT1(IB).EQ.NXANT2(IB)
         DO 200 IIF = 1,NIF
            IFO = IIF
            IF (IFAVG) IFO = 1
            DO 190 IIC = 1,NCH
               DO 180 IIS = 1,NS
                  INDEX = 1 + INCS * (IIS-1) + INCF * (IIC-1) +
     *               INCIF * (IIF-1)
                  IF (BUFF1(INDEX+2).GT.0.) THEN
                     NOW(IB) = 1
                     RT = BUFF1(INDEX)
                     IT = BUFF1(INDEX+1)
                     IF (ISAUTO) THEN
                        AT = RT * RT
                     ELSE
                        AT = SQRT (RT*RT + IT*IT)
                        END IF
                     WT = BUFF1(INDEX+2)
                     RESUM(IIS,IFO,IB) = RESUM(IIS,IFO,IB) + RT * WT
                     IMSUM(IIS,IFO,IB) = IMSUM(IIS,IFO,IB) + IT * WT
                     AMSUM(IIS,IFO,IB) = AMSUM(IIS,IFO,IB) + AT * WT
                     WTSUM(IIS,IFO,IB) = WTSUM(IIS,IFO,IB) + WT
                     END IF
 180              CONTINUE
 190           CONTINUE
 200        CONTINUE
         GO TO 100
C                                       Finish write
 300  NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNSC, INDSC, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                      Close input file
      CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1450) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = NUMVIS
      CALL UCMPRS (NVIS, DISKOU, CNOOUT, LUNSC, CATBLK, IRET)
      IF (NVIS.EQ.0) THEN
         MSGTXT = 'CALAVG: 0 VISIBILITIES SELECTED FROM UV DATA'
         IRET = 1
         GO TO 990
         END IF
C                                      Update catblk
      CALL CATIO ('UPDT', DISKOU, CNOOUT, CATBLK, 'REST', SCRTCH, IRET)
      CALL COPY (256, CATBLK, CATIMG)
      CALL COPY (256, CATSAV, CATBLK)
C                                       Close files
      CALL ZCLOSE (LUNSC, INDSC, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CALAVG: ERROR',I3,1X,A4,'ING INPUT FILE')
 1010 FORMAT ('CALAVG: ERROR',I3,' CREATING UV SCRATCH FILE')
 1020 FORMAT ('CALAVG: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1050 FORMAT ('CALAVG: ERROR',I3,' CREATING UV WORKFILE')
 1055 FORMAT ('CALAVG: ERROR',I3,' ADDING KEYWORDS TO HEADER')
 1150 FORMAT ('CALAVG: ERROR',I3,' WRITING VIS FILE')
 1450 FORMAT ('CALAVG: UNABLE TO CLOSE INPUT UV FILE - ERROR',I5)
      END
      SUBROUTINE IBCHKW (IRET)
C-----------------------------------------------------------------------
C   checks the TV window and sets the sub-window locations and the flags
C   indicating that displays are no longer okay.
C   Output:
C      IRET   I   Return code: ignore ?
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER  CHM(2,2)
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA CHM /15, 15, 8, 8/
C-----------------------------------------------------------------------
      CALL YWINDO ('READ', WINDTV, IRET)
      IF (IRET.NE.0) THEN
         WINDTV(1) = 1
         WINDTV(2) = 1
         WINDTV(3) = MAXXTV(1)
         WINDTV(4) = MAXXTV(2)
         IRET = 0
         END IF
      IF ((WINDTV(1).NE.LWINTV(1)) .OR. (WINDTV(2).NE.LWINTV(2)) .OR.
     *   (WINDTV(3).NE.LWINTV(3)) .OR. (WINDTV(4).NE.LWINTV(4))) THEN
         GRDFOK = .FALSE.
         GRDAOK = .FALSE.
         GRMEOK = .FALSE.
         SCALOK = .FALSE.
         CALL COPY (4, WINDTV, LWINTV(1))
C                                       X plot locations
         NEDGE = MAX (4, CSIZTV(1) + 1) / 2
         MEDGE = MAX (2, (2 * MAXXTV(1))/512)
         METYPE = 0
 10      IF (METYPE.LT.2) THEN
            METYPE = METYPE + 1
            IXD01 = WINDTV(1) + 2 * NEDGE + CHM(1,METYPE) * CSIZTV(1)
            IXD11 = WINDTV(3) - 9 - 2 * NEDGE - CHM(2,METYPE) *
     *         CSIZTV(1)
            IF (IXD11-IXD01.LT.0.6*(WINDTV(3)-WINDTV(1))) GO TO 10
            END IF
         IXD02 = WINDTV(1) + MEDGE + 11*CSIZTV(1)
         IXD12 = MAX (WINDTV(3) - MEDGE - 11*CSIZTV(1), IXD11)
C                                       Y plot locations
         IYD12 = WINDTV(4) - NEDGE
         IYD01 = 7 * (1 + NEDGE) + 3 * CSIZTV(2) + WINDTV(2)
         IYD11 = IYD01 + 0.82 * (IYD12 - IYD01)
         IYD02 = WINDTV(4) - 8 * MEDGE - 4 * CSIZTV(2) + 1
         IF (IYD02.GT.IYD11) THEN
            IYD02 = IYD11 + 1
         ELSE
            IYD11 = IYD02 - 1
            END IF
C                                       TV window of data plot
         TVWIN(1) = IXD01 + NEDGE + 6 * CSIZTV(1) + 1
         TVWIN(3) = IXD11 - NEDGE - 4 * CSIZTV(1) - 1
         TVWIN(2) = IYD01 + NEDGE + 2 * CSIZTV(2) + 1
         TVWIN(4) = IYD11 - NEDGE - 2 * CSIZTV(2) - 1
         TVWIN2(1) = IXD02 + NEDGE + 1
         TVWIN2(3) = IXD12 - NEDGE - 1
         TVWIN2(2) = IYD02 + NEDGE + 1
         TVWIN2(4) = IYD12 - NEDGE - 1
         END IF
C
 999  RETURN
      END
      SUBROUTINE IBFCHS (RPOS, SVZOOM, ICOL, IROW, IRET)
C-----------------------------------------------------------------------
C   handles the choice of operation to perform
C   In/out:
C      RPOS     R(2)   cursor position to start and end
C   Input:
C      IMGWIN   I(4)   window of next load
C      SVZOOM   I(3)   zoom parameters
C   Output:
C      ICOL     I      col number of choice
C      IROW     I      row number of choice
C      IRET     I      TV I/O error (no message)
C-----------------------------------------------------------------------
      REAL      RPOS(2)
      INTEGER   SVZOOM(3), IROW, ICOL, IRET
C
      INTEGER   NM
      PARAMETER (NM = 22)
      CHARACTER PS*16, CHTYPE(3)*13, FLGTIF(2)*6, CHOICE(NM,2,2)*16,
     *   ROUTIN*6, STRING*100, CHST(13)*2, ALTER1(NM)*16, ALTER2(NM)*16,
     *   MENU1(NM)*16, MENU2(NM)*16, FLGTCH(2)*11, FLGTSO(2)*10
      REAL      PPOS(2), TEMP
      INTEGER   IX, IY, NC, LCOL, LROW, NROW, NCOL, PCOL, IZOOM(3), LX,
     *   NROWS(2), NCHM(2,2), NCH(NM,2,2), IXC(2), IYC(NM), IXP(5), LY,
     *   IYP(5), QUAD, IBUT, ITW(3), IROUND, JERR, PROW, ITRIM, IT2,
     *   IT3, ZAND, MASK, I, MXCH(4)
      LOGICAL   F, DOIT, DOZOOM(NM,2)
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CHOICE(1,1,1), MENU1),  (CHOICE(1,2,1), MENU2)
      EQUIVALENCE (CHOICE(1,1,2), ALTER1),  (CHOICE(1,2,2), ALTER2)
      SAVE IXC, IYC, MXCH
      DATA F /.FALSE./
      DATA NCOL, NROWS /2, 18, NM/
      DATA NCHM /15, 15, 8, 8/
      DATA NCH
     *   /9,15, 9,15,15,15, 8,12,12,13,15,10,15,15,15,15,13,13, 0, 0,
     *    0, 0,
     *    9,15, 9,10,10,15,15,13,10,10,14,10,15,12,11,10,14,15,13,13,
     *    4, 4,
     *    7, 8, 8, 8, 8, 8, 6, 8, 8, 8, 8, 5, 8, 8, 8, 8, 8, 8, 0, 0,
     *    0, 0,
     *    7, 8, 7, 8, 8, 7, 8, 8, 8, 8, 7, 7, 7, 8, 7, 8, 8, 8, 8, 8,
     *    4, 4/
      DATA DOZOOM /2*.FALSE., .TRUE., 19*.FALSE.,
     *   7*.TRUE., 15*.FALSE./
      DATA MENU1
     *   /'OFF ZOOM        ', 'OFF ENHANCEMENT ',
     *    'TVFIDDLE        ', 'ENTER AMP RANGE ',
     *    'ENTER PHS RANGE ', 'ENTER DCR RANGE ',
     *    'ENTER IF        ', 'ENTER STOKES    ',
     *    'RATIO 2ND IF    ', 'SHOW TOP PLOT   ',
     *    'PLOT ERROR BARS ', 'PLOT MODEL      ',
     *    'SWTCH 1-CH FLAG ', 'SWTCH 1-IF FLAG ',
     *    'SWTCH 1-SO FLAG ', 'SET STOKES FLAG ',
     *    'FLAG STATION1   ', 'FLAG STATION2   ',
     *    '                ', '                ',
     *    '                ', '                '/
      DATA MENU2
     *   /'FLAG TIME       ', 'FLAG TIME RANGE ',
     *    'FLAG AREA       ', 'FLAG ABOVE      ',
     *    'FLAG BELOW      ', 'FLAG ABOUT MEAN ',
     *    'FLAG INTERACTIV ', 'FLAG ALL TIME   ',
     *    'LIST FLAGS      ', 'UNDO FLAGS      ',
     *    'SHOW AMPLITUDE  ', 'SHOW PHASE      ',
     *    'SHOW DECORRELAT ', 'SELECT FRAME    ',
     *    'FIRST FRAME     ', 'NEXT FRAME      ',
     *    'PREVIOUS FRAME  ', 'SELECT BASELINE ',
     *    'NEXT BASELINE   ', 'PREV BASELINE   ',
     *    'LOAD            ', 'EXIT            '/
      DATA ALTER1
     *   /'OFFZOOM         ', 'OFFENHAN        ',
     *    'TVFIDDLE        ', 'AMP RNGE        ',
     *    'PHS RNGE        ', 'DCR RNGE        ',
     *    'SEL IF          ', 'SEL STKS        ',
     *    'RATIO IF        ', 'TOP PLOT        ',
     *    'ERR BARS        ', 'MODEL           ',
     *    '1-CH FLG        ', '1-IF FLG        ',
     *    '1-SO FLG        ', 'STKS FLG        ',
     *    'FL STN01        ', 'FL STN02        ',
     *    '                ', '                ',
     *    '                ', '                '/
      DATA ALTER2
     *   /'FG TIME         ', 'FG T.RNG        ',
     *    'FG AREA         ', 'FG ABOVE        ',
     *    'FG BELOW        ', 'FG MEAN         ',
     *    'FG INTER        ', 'FG ALL T        ',
     *    'LST FLGS        ', 'UNDO FLG        ',
     *    'SHO AMP         ', 'SHO PHS         ',
     *    'SHO DCR         ', 'SLCT FRM        ',
     *    '1ST FRM         ', 'NEXT FRM        ',
     *    'PREV FRM        ', 'SEL BSLN        ',
     *    'NXT BSLN        ', 'PRV BSLN        ',
     *    'LOAD            ', 'EXIT            '/
      DATA CHST /'YX','XY','YY','XX','LR','RL','LL','RR','??','I','Q',
     *   'U ','V '/
      DATA CHTYPE /'AMPLITUDE', 'PHASE', 'DECORRELATION'/
      DATA FLGTIF, FLGTCH, FLGTSO /'ALL-IF','ONE-IF', 'ALL-CHANNEL',
     *   'ONE-CHANNEL', 'ALL-SOURCE','ONE-SOURCE'/
C-----------------------------------------------------------------------
      IROW = 0
      ICOL = 0
      CALL ZTIME (ITW)
      PROW = 0
      PCOL = 0
C                                       Set up special first
C                                       column of menu
      WRITE (CHOICE(17,1,1),1000) STNS(ANTENS(1))
      WRITE (CHOICE(18,1,1),1000) STNS(ANTENS(2))
      WRITE (CHOICE(17,1,2),1001) STNS(ANTENS(1))
      WRITE (CHOICE(18,1,2),1001) STNS(ANTENS(2))
C                                       Reset zoom if needed
      IZOOM(1) = 0
      IZOOM(2) = MAXXTV(1)/2
      IZOOM(3) = MAXXTV(2)/2
      IF ((TVZOOM(1).NE.IZOOM(1)) .OR. (TVZOOM(2).NE.IZOOM(2)) .OR.
     *   (TVZOOM(3).NE.IZOOM(3))) THEN
         CALL YZOOMC (IZOOM(1), IZOOM(2), IZOOM(3), F, IRET)
         ROUTIN = 'YZOOMC'
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       check window of TV
      CALL IBCHKW (IRET)
C                                       init the display
      CALL YHOLD ('ONNN', I)
      IF (.NOT.GRMEOK) THEN
         CALL YZERO (GRME, IRET)
         ROUTIN = 'YZERO'
         IF (IRET.NE.0) GO TO 900
         CALL FILL (4, 0, MXCH)
         END IF
      MASK = 2 ** (GRME - 1)
      MASK = ZAND (MASK, TVLIMG(1))
      IF (MASK.EQ.0) THEN
         CALL YSLECT ('ONNN', GRME, 0, SCRTCH, IRET)
         ROUTIN = 'YSLECT'
         IF (IRET.NE.0) GO TO 900
         END IF
      IF (GRME.NE.GRCV) THEN
         IF (.NOT.GRCVOK) THEN
            CALL YZERO (GRCV, IRET)
            ROUTIN = 'YZERO'
            IF (IRET.NE.0) GO TO 900
            GRCVOK = .TRUE.
            END IF
         MASK = 2 ** (GRCV - 1)
         MASK = ZAND (MASK, TVLIMG(1))
         IF (MASK.EQ.0) THEN
            CALL YSLECT ('ONNN', GRCV, 0, SCRTCH, IRET)
            ROUTIN = 'YSLECT'
            IF (IRET.NE.0) GO TO 900
            END IF
         END IF
C                                       turn on cursor
      QUAD = -1
      IF (CURSON) THEN
         CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IRET)
      ELSE
         IF ((RPOS(1).LE.WINDTV(1)) .OR. (RPOS(1).GT.WINDTV(3)))
     *      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
         IF ((RPOS(2).LE.WINDTV(2)) .OR. (RPOS(2).GT.WINDTV(4)))
     *      RPOS(2) = WINDTV(4) - 10 - 3 * CSIZTV(2)
         CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IRET)
         END IF
      ROUTIN = 'YCURSE'
      IF (IRET.NE.0) GO TO 900
      CURSON = .TRUE.
C                                       init the graphics display
      IF (.NOT.GRMEOK) THEN
C                                       test limits
         IY = IYD11 - 3 * CSIZTV(2)
         LY = MAX (NROWS(1), NROWS(2))
         LY = LY * (2*NEDGE + CSIZTV(2)) + 4
         IF (IY-LY.LE.0) THEN
            IF (CSIZTV(2).GT.9) THEN
               MSGTXT = 'MENU WILL NOT FIT WITH CURRENT CHARMULT'
            ELSE
               MSGTXT = 'TV IS TOO SMALL FOR THIS TASK''S MENUS'
               END IF
            IRET = 10
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       write to planes: strings
         ROUTIN = 'IMCHAR'
         IX = 7 + NEDGE + WINDTV(1)
         DO 120 LCOL = 1,NCOL
            IY = IYD11 - 2 - NEDGE - 4 * CSIZTV(2)
            NROW = NROWS(LCOL)
            IXC(LCOL) = IX
            DO 100 LROW = 1,NROW
               IYC(LROW) = IY
               NC = NCH(LROW,LCOL,METYPE)
               PS = CHOICE(LROW,LCOL,METYPE)(:NC)
               CALL IMCHAR (GRME, IX, IY, 0, 0, PS(:NC), SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 900
               IY = IY - 2*NEDGE - CSIZTV(2)
 100           CONTINUE
            IX = WINDTV(3) - 7 - NEDGE - NCHM(LCOL,METYPE)*CSIZTV(1)
 120        CONTINUE
C                                       write border lines
         IX = 5 + WINDTV(1)
         IY = IYD11 - 3 * CSIZTV(2)
         DO 130 LCOL = 1,NCOL
            LX = 4 + 2*NEDGE + NCHM(LCOL,METYPE)*CSIZTV(1)
            LY = NROWS(LCOL) * (2*NEDGE + CSIZTV(2)) + 4
            IXP(1) = IX
            IYP(1) = IY
            IXP(2) = IX + LX - 1
            IYP(2) = IYP(1)
            IXP(3) = IXP(2)
            IYP(3) = IY - LY + 1
            IXP(4) = IXP(1)
            IYP(4) = IYP(3)
            IXP(5) = IXP(1)
            IYP(5) = IYP(1)
            CALL IMVECT (GRME, 5, IXP, IYP, IMVONN, IRET)
            ROUTIN = 'IMVECT'
            IF (IRET.NE.0) GO TO 900
            IX = IX + 1
            IY = IY - 1
            LX = LX - 2
            LY = LY - 2
            IXP(1) = IX
            IYP(1) = IY
            IXP(2) = IX + LX - 1
            IYP(2) = IYP(1)
            IXP(3) = IXP(2)
            IYP(3) = IY - LY + 1
            IXP(4) = IXP(1)
            IYP(4) = IYP(3)
            IXP(5) = IXP(1)
            IYP(5) = IYP(1)
            CALL IMVECT (GRME, 5, IXP, IYP, IMVONN, IRET)
            ROUTIN = 'IMVECT'
            IF (IRET.NE.0) GO TO 900
            IX = WINDTV(3) - 9 - 2*NEDGE - NCHM(LCOL,METYPE)*CSIZTV(1)
            IY = IY + 1
 130        CONTINUE
C                                       init antenna lines anyway
      ELSE
         IX = IXC(1)
         IY = IYC(17)
         NC = NCH(17,1,METYPE)
         PS = CHOICE(17,1,METYPE)(:NC)
         ROUTIN = 'IMCHAR'
         CALL IMCHAR (GRME, IX, IY, 0, 0, PS(:NC), SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 900
         IY = IYC(18)
         NC = NCH(18,1,METYPE)
         PS = CHOICE(18,1,METYPE)(:NC)
         ROUTIN = 'IMCHAR'
         CALL IMCHAR (GRME, IX, IY, 0, 0, PS(:NC), SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 900
         END IF
C                                       status line
      LX = 2
      LY = 2
      IX = 2
      IF (DOCHAN) LX = 1
      IF (DOIFS) LY = 1
      IF (DOSOUR) IX = 1
      NC = ITRIM (CHTYPE(PLTYPE)) + 1
      STRING = CHTYPE(PLTYPE)
      IF ((PLIF(1).NE.PLIF(2)) .AND. (PLIF(2).NE.0)) THEN
         IF (PLTYPE.NE.2) THEN
            STRING(NC:) = ' RATIO'
            NC = NC + 6
         ELSE
            STRING(NC:) = ' DIFFERENCE'
            NC = NC + 11
            END IF
         END IF
      STRING(NC:) = '  FLAGGING ' // FLGTCH(LX) // '  ' // FLGTIF(LY)
     *   // '  ' // FLGTSO(IX) // '  STOKES=' // USTFLG
      NC = ITRIM (STRING)
      I = MAX (NC, MXCH(1))
      MXCH(1) = NC
      ROUTIN = 'IMCHAR'
      IY = 5 + 2*NEDGE + WINDTV(2)
      IX = 5 + 2*NEDGE + WINDTV(1)
      CALL IMCHAR (GRME, IX, IY, 0, 0, STRING(:I), SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 900
      IT3 = 1
C                                         2nd line
      DOIT = (PDOERR) .AND. (PLTYPE.LE.2)
      IF ((PLIF(2).NE.0) .AND. (PLIF(2).NE.PLIF(1))) DOIT = .FALSE.
      IF ((FLALL1) .OR. (FLALL2) .OR. (DOIT)) THEN
         IY = IY + 2*NEDGE + CSIZTV(2)
         IT3 = IT3 + 1
         IF ((FLALL1) .AND. (FLALL2)) THEN
            I = ITRIM (STNS(ANTENS(1)))
            STRING = 'Flag all to ' // STNS(ANTENS(1))(:I) //
     *         ' and ' // STNS(ANTENS(2))
         ELSE IF (FLALL1) THEN
            STRING = 'Flag all to ' // STNS(ANTENS(1))
         ELSE IF (FLALL2) THEN
            STRING = 'Flag all to ' // STNS(ANTENS(2))
         ELSE
            STRING = ' '
            END IF
         IF (DOIT) THEN
            NC = MAX (ITRIM (STRING), 1)
            IF (NC.GT.1) NC = NC + 4
            STRING(NC:) = 'Error bars are plotted'
            END IF
         NC = ITRIM (STRING)
         I = MAX (NC, MXCH(IT3))
         MXCH(IT3) = NC
         CALL IMCHAR (GRME, IX, IY, 0, 0, STRING(:I), SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 900
         END IF
      IF ((LIF(1).NE.PLIF(1)) .OR. (LIF(2).NE.PLIF(2)) .OR.
     *   ((PDOERR.NEQV.DOERRB) .AND. (PLTYPE.LE.2)) .OR.
     *   (LSTOKS.NE.PLSTOK)) THEN
         IY = IY + 2*NEDGE + CSIZTV(2)
         IT3 = IT3 + 1
         TEMP = CATID(KDCRV+1) + (LSTOKS - CATIR(KRCRP+1)) *
     *      CATIR(KRCIC+1)
         IT2 = IROUND (TEMP) + 9
         IF ((IT2.LT.1) .OR. (IT2.GT.13)) IT2 = 9
         STRING = 'NEXT LOAD SHOWS'
         NC = 16
         DO 140 I = 1,2
            IF (LIF(I).NE.PLIF(I)) THEN
               WRITE (PS,1130) LIF(I)
               CALL CHTRIM (PS, 6, PS, LX)
               WRITE (STRING(NC:),1131) ' IF',I,PS(:LX)
               NC = NC + 6 + LX
               END IF
 140        CONTINUE
         IF (LSTOKS.NE.PLSTOK) THEN
            STRING(NC:) = ' STOKES ' // CHST(IT2)
            NC = NC + 10
            END IF
         IF (PDOERR.NEQV.DOERRB) THEN
            IF (DOERRB) THEN
               STRING(NC:) = ' ERROR BARS'
            ELSE
               STRING(NC:) = ' NO ERROR BARS'
               END IF
            END IF
         NC = ITRIM (STRING)
         I = MAX (NC, MXCH(IT3))
         MXCH(IT3) = NC
         CALL IMCHAR (GRME, IX, IY, 0, 0, STRING(:I), SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 900
         END IF
C                                       Fourth line
      IF ((MINDY.LT.MINY) .OR. (MAXDY.GT.MAXY)) THEN
         IY = IY + 2*NEDGE + CSIZTV(2)
         IT3 = IT3 + 1
         STRING = 'DATA SAMPLES OMITTED DUE TO PLOT SCALE'
         NC = ITRIM (STRING)
         I = MAX (NC, MXCH(IT3))
         MXCH(IT3) = NC
         CALL IMCHAR (GRME, IX, IY, 0, 0, STRING(:I), SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 900
         END IF
C                                       erase old strings
      DO 150 LX = 1,4
         IF (IT3.LT.LX) THEN
            IY = IY + 2*NEDGE + CSIZTV(2)
            IT3 = IT3 + 1
            I = MXCH(IT3)
            STRING = ' '
            MXCH(IT3) = 0
            IF (I.GT.0) THEN
               CALL IMCHAR (GRME, IX, IY, 0, 0, STRING(:I), SCRTCH,
     *            IRET)
               IF (IRET.NE.0) GO TO 900
               END IF
            END IF
 150     CONTINUE

C                                       read a choice
      CALL YHOLD ('OFFF', I)
      GRMEOK = .TRUE.
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      MSGTXT = 'Press buttons A, B, or C to choose an operation'
      CALL MSGWRT (1)
      MSGTXT = 'Press button D for on-line help'
      CALL MSGWRT (1)
C                                        read until cursor moves
 200  CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IRET)
      ROUTIN = 'YCURSE'
      IF (IRET.NE.0) GO TO 900
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (.NOT.DOIT) GO TO 200
C                                        find the choice
         IX = IROUND (RPOS(1))
         IY = IROUND (RPOS(2))
         ICOL = (2 * IX - 1) / MAXXTV(1) + 1
         IROW = 0
         NROW = NROWS(ICOL)
         DO 220 LROW = 1,NROW
            IF (IY.GT.IYC(NROW+1-LROW)-2) IROW = NROW+1-LROW
 220        CONTINUE
         IF (IROW.EQ.0) GO TO 200
         IF (GRME.NE.GRCV) THEN
            IF ((PCOL.NE.ICOL) .OR. (PROW.NE.IROW) .OR. (IBUT.GT.0))
     *         THEN
C                                       restore choice
               ROUTIN = 'IMCHAR'
               IF ((PCOL.GT.0) .AND. (PROW.GT.0)) THEN
                  CALL YHOLD ('ONNN', I)
                  NC = NCH(PROW,PCOL,METYPE)
                  PS(:NC) = ' '
                  CALL IMCHAR (GRCV, IXC(PCOL), IYC(PROW), 0, 0,
     *               PS(:NC), SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 900
                  PS(:NC) = CHOICE(PROW,PCOL,METYPE)(:NC)
                  CALL IMCHAR (GRME, IXC(PCOL), IYC(PROW), 0, 0,
     *               PS(:NC), SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 900
                  CALL YHOLD ('OFFF', I)
                  END IF
C                                       highlight choice
               IF ((IBUT.LE.0) .OR. (IBUT.GE.8)) THEN
                  NC = NCH(IROW,ICOL,METYPE)
                  PS(:NC) = CHOICE(IROW,ICOL,METYPE)(:NC)
                  CALL IMCHAR (GRCV, IXC(ICOL), IYC(IROW), 0, 0,
     *               PS(:NC), SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 900
                  END IF
               PCOL = ICOL
               PROW = IROW
               END IF
            END IF
C                                       leave on button A, B, C
         IF (IBUT.GE.8) THEN
            NC = NCH(IROW,ICOL,METYPE)
            IF (((IROW.EQ.17) .OR. (IROW.EQ.18)) .AND. (ICOL.EQ.1)) THEN
               NC = 5
               IF (METYPE.EQ.2) NC = 3
               END IF
            PS = CHOICE(IROW,ICOL,METYPE)(1:NC)
            CALL TSKHLP (PS, NC, ' ', JERR)
            IBUT = 0
         ELSE IF (IBUT.GT.0) THEN
            GO TO 900
            END IF
         GO TO 200
C                                       turn off the choices
 900  IF ((IRET.EQ.0) .AND. (DOZOOM(IROW,ICOL)) .AND.
     *   (SVZOOM(1).NE.TVZOOM(1))) THEN
         CALL COPY (3, SVZOOM, TVZOOM)
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), F, JERR)
         END IF
C                                       force buffer to TV
      CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, JERR)
C
 990  IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1990) IRET, ROUTIN
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLAG ',A8)
 1001 FORMAT ('FL ',A5)
 1130 FORMAT (I6)
 1131 FORMAT (A,I1,'= ',A)
 1990 FORMAT ('IBFCHS: TV I/O ERROR',I7,' FROM ',A)
      END
      SUBROUTINE IBFUNC (BRANCH, IRET)
C-----------------------------------------------------------------------
C   performs TV enhancement functions for IBLED
C   Inputs:
C      BRANCH   I         1 => off zoom
C                         2 => off black and white transfer AND color
C                         3 => do TVFIDDLE
C   Output:
C      IRET     I         TV error (no message)
C-----------------------------------------------------------------------
      INTEGER   BRANCH, IRET
C
      LOGICAL   F
      INTEGER   I, IC, ICOLOR, NLEVS, II, JJ
      REAL      SLOPE, OFFS
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      ICOLOR = 7
C                                       off zoom
      IF (BRANCH.EQ.1) THEN
         TVZOOM(1) = 0
         TVZOOM(2) = MAXXTV(1) / 2
         TVZOOM(3) = MAXXTV(2) / 2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), F, IRET)
C                                       off transfer: channel 1
      ELSE IF (BRANCH.EQ.2) THEN
         IC = 2 ** NGRAY - 1
         NLEVS = MAXINT + 1
         SLOPE = (LUTOUT+1.0) / (MAXINT+1.0)
         DO 10 I = 1,NLEVS
            SCRTCH(I) = I * SLOPE - 0.5
 10         CONTINUE
         CALL YLUT ('WRIT', IC, ICOLOR, F, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       off pseudo (off colors)
         I = OFMINP + 1
         CALL RFILL (I, 0.0, BUFF2)
         NLEVS = MIN (LUTOUT, OFMINP) + 1
         OFFS = 1.0 / REAL (OFMOUT)
         SLOPE = OFFS * (OFMOUT + 1.0) / REAL(NLEVS)
         DO 20 I = 1,NLEVS
            BUFF2(I) = I * SLOPE - OFFS
 20         CONTINUE
         I = OFMINP + 1
         JJ = NLEVS
         I = I / NLEVS
         DO 21 II = 2,I
            CALL RCOPY (NLEVS, BUFF2, BUFF2(JJ+1))
            JJ = JJ + NLEVS
 21         CONTINUE
         CALL YOFM ('WRIT', ICOLOR, F, BUFF2, IRET)
C                                       TVFIDDLE
      ELSE IF (BRANCH.EQ.3) THEN
         NLEVS = MIN (LUTOUT, OFMINP) + 1
         CALL TVFIDL (1, NLEVS, SCRTCH, IRET)
         END IF
C
 999  RETURN
      END
      SUBROUTINE IBFCLI (NFC, MSGLEV, SNAME, T1, T2)
C-----------------------------------------------------------------------
C   displays contents of an FC table line on the message file/terminal
C   Inputs:
C      NFC      I   number FC records read of this type: 0 => header
C                   info, else trailer
C      MSGLEV   I   message level to use
C   In/out
C      T1       R   lowest time: output when NFC 0, else used
C      T2       R   highest time: output when NFC 0, else used
C   Common:
C      Must have the FC line in its common
C-----------------------------------------------------------------------
      INTEGER   NFC, MSGLEV
      REAL      T1, T2
      CHARACTER SNAME*16
C
      INTEGER   IT1(4), IT2(4)
      CHARACTER OP*8, STR*16, STK*4
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                        cleaning up old one
      IF (NFC.GT.0) THEN
         CALL TODHMS (T1, IT1)
         CALL TODHMS (T2, IT2)
         WRITE (MSGTXT,1000) IT1, IT2
         CALL MSGWRT (MSGLEV)
         WRITE (MSGTXT,1001) NFC
         IF (NFC.GT.1) CALL MSGWRT (MSGLEV)
         IF (SNAME.EQ.' ') THEN
            MSGTXT(9:) = 'Applies to all source names'
         ELSE
            MSGTXT(9:) = 'Applies only to source ' // SNAME
            END IF
         CALL MSGWRT (MSGLEV)
C                                        new one
      ELSE
         T1 = MAX (START, MIN (STOP, FCTIME(1)))
         T2 = MAX (START, MIN (STOP, FCTIME(2)))
         CALL H2CHR (8, 1, FCOPER, OP)
         CALL H2CHR (12, 1, FCTYPE, STR)
         CALL H2CHR (4, 1, FCSFLG, STK)
         IF ((OP(:5).EQ.'CLIPA') .OR. (OP(:5).EQ.'CLIPB') .OR.
     *      (OP(:4).EQ.'MEAN') .OR. (OP(:4).EQ.'AREA')) THEN
            WRITE (MSGTXT,1100) FCNUMB, OP, STR, FCLIPR
         ELSE
            WRITE (MSGTXT,1110) FCNUMB, OP, STR
            END IF
         CALL MSGWRT (MSGLEV)
         WRITE (MSGTXT,1150) FCCHAN, FCIF, FCBASL(1), FCBASL(2), STK
         CALL MSGWRT (MSGLEV)
         MSGTXT = ' '
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (8X,'Total time range',I3.2,'/',2(I2.2,':'),I2.2,' -',
     *   I3.2,'/',2(I2.2,':'),I2.2)
 1001 FORMAT (8X,'Required',I6,' individual flagging instructions')
 1100 FORMAT (I3,2X,A8,1X,'Type ',A12,' clip range',2(1PE11.3))
 1110 FORMAT (I3,2X,A8,1X,'Type ',A12)
 1150 FORMAT (8X,'Flag: Ch=',I5,'-',I4,' IF=',I3.2,'-',I2.2,
     *   ' Baseline',I3.2,'-',I2.2,' Stokes ',A4)
      END
      SUBROUTINE IBFCOP (LUN, VOL, CNO, VER, CATBLK, FCNUM, LASTR, BUF,
     *   IERR)
C-----------------------------------------------------------------------
C   creates and/or opens for writing (and reading) a specified FC table
C   for Flag Commands from IBLED
C   Inputs:
C      LUN     I         Logical unit number to use
C      VOL     I         Disk number
C      CNO     I         Catalog number
C   In/out:
C      VER     I         Input: desired version number 0 -> highest
C                           existing or new
C                        Output: that used
C      CATBLK  I(256)    File catalog header block
C   Output:
C      FCNUM   I         Highest current flag command number
C      LASTR   I         Highest current record written
C      BUF     I(512)    Required for later calls to TABIO
C      IERR    I         Error codes from TABINI or TABIO
C-----------------------------------------------------------------------
      INTEGER   LUN, VOL, CNO, VER, CATBLK(256), FCNUM, LASTR,
     *   BUF(512), IERR
C
      INTEGER   IRNO, NKEY, NREC, ITITLE(11), LBUF(256), CCODE(11),
     *   NCOL, RECORD(20), NUMBP
      HOLLERITH HTITLE(11)
      CHARACTER TTITLE*32, CTITLE(11)*8, UNITS(11)*8, TITLE*24
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (ITITLE, HTITLE)
      DATA TTITLE /'AIPS IBLED FLAG COMMAND TABLE   '/
      DATA CTITLE /'FLAGNUMB', 'FLAGOPER', 'FLAGTIME', 'FLAGANT1',
     *   'FLAGANT2', 'FLAGSOUR', 'FLAGCHAN', 'FLAGIF  ', 'FLAGSTOK',
     *   'CLIPRANG', 'FLUXTYPE'/
      DATA UNITS /2*' ', 'DAYS', 5*' ', 'STK CODE', 'FLUX',' '/
      DATA CCODE /14, 83, 22, 14, 14, 14, 24, 24, 43, 22, 123/
C-----------------------------------------------------------------------
C                                       Init parameters
      NCOL = 11
      NKEY = 1
      NREC = 500
      CALL FILL (256, 0, LBUF)
      CALL COPY (NCOL, CCODE, LBUF(129))
C                                       Version number
      IF (VER.LE.0) CALL FNDEXT ('FC', CATBLK, VER)
C                                       create/open
      CALL TABINI ('WRIT', 'FC', VOL, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, LBUF, BUF, IERR)
C                                       Error
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, VER
         CALL MSGWRT (8)
C                                       pre-existing file
      ELSE IF (IERR.EQ.0) THEN
         NUMBP = LBUF(1)
         IF (BUF(5).GT.0) THEN
            CALL TABIO ('READ', 0, BUF(5), RECORD, BUF, IERR)
            FCNUM = RECORD(NUMBP)
            LASTR = BUF(5)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR, VER, BUF(5)
               CALL MSGWRT (8)
               END IF
         ELSE
            FCNUM = 0
            LASTR = 0
            END IF
C                                       New file created
      ELSE
         FCNUM = 0
         LASTR = 0
C                                       write column titles
         DO 20 IRNO = 1,NCOL
            TITLE = CTITLE(IRNO)
            CALL CHR2H (24, TITLE, 1, HTITLE)
            CALL TABIO ('WRIT', 3, IRNO, HTITLE, BUF, IERR)
            IF (IERR.NE.0) GO TO 999
 20         CONTINUE
C                                       write units
         DO 30 IRNO = 1,NCOL
            TITLE = UNITS(IRNO)
            CALL CHR2H (24, TITLE, 1, HTITLE)
            CALL TABIO ('WRIT', 4, IRNO, HTITLE, BUF, IERR)
            IF (IERR.NE.0) GO TO 999
 30         CONTINUE
C                                       table title
         CALL CHR2H (32, TTITLE, 1, HTITLE)
         CALL COPY (8, ITITLE, BUF(101))
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' OPENING Flag-Command TABLE VERSION',I4)
 1010 FORMAT ('ERROR',I5,' READING Flag-Command TABLE VERSION',I4,
     *   ' RECORD',I8)
      END
      SUBROUTINE IBFLGR (ONECHN, IRET)
C-----------------------------------------------------------------------
C   IBFLGR is the main action routine of IBLED.  It displays the data
C   plotted on the TV, offers options to enhance the display or select a
C   new display, and to edit the data.
C   Input:
C      ONECHN   L(2)   One channel, one IF in input (or all averaged)
C   Output:
C      IRET   I   Error code: 0 => okay, else die.
C-----------------------------------------------------------------------
      LOGICAL   ONECHN(2)
      INTEGER   IRET
C
      INCLUDE 'INCS:DSEL.INC'
      CHARACTER CHTYPE(3)*8, CTEMP*4, ROUTIN*6, MSGBUF*72, TSTOK*4,
     *   SNAMES(XSTBSZ)*16, CHSDEF(8)*2
      INTEGER   TTY(2), JERR, ICOL, IROW, MTRY, NBASE, SVZOOM(3), NTRY,
     *   IX, I, ITEMP, KTEMP(2), IB1, SLUN, IROUND, DATE(3), TIME(3),
     *   CDAWIN(4)
      REAL      DTIME, PRPOS(2,10), TEMP, GCOL(3,4), RGB(3,4)
      LOGICAL   T, F, EQUAL, NOFLAG, DOSTOK
      DOUBLE PRECISION DTEMP(2)
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA SLUN /29/
      DATA T, F /.TRUE.,.FALSE./
      DATA DTIME /3.0/
      DATA CHTYPE /'AMPLTUDE', 'PHASE   ','DECORREL'/
      DATA CHSDEF /'RR','LL','RL','LR','XX','YY','XY','YX'/
      DATA RGB /0.1,1.0,0.1, 0.0,1.0,1.0, 1.0,0.5,1.0, 1.0,1.0,0.0/
C-----------------------------------------------------------------------
      TTY(1) = 5
      MTRY = 20
      NTRY = MTRY + 1
      CALL FILL (4, 0, LWINTV)
      CALL RFILL (20, 0.0, PRPOS)
      LTYPE = DPARM(1) + 1.0001
      IF ((LTYPE.LT.1) .OR. (LTYPE.GT.3)) LTYPE = 1
      PIXRNG(1,LTYPE) = DPARM(7)
      PIXRNG(2,LTYPE) = DPARM(8)
      LIF(1) = BIF
      LIF(2) = 0
      LCHAN(1) = BCHAN
      LCHAN(2) = BCHAN
      LSTOKS = 1
      BASPT = 1
      UPTR = .TRUE.
      PBASPT = 0
      PLSTOK = 0
      PLTYPE = 0
      PDOERR = .FALSE.
      PLIF(1) = 0
      PLIF(2) = 0
      PLCHAN(1) = 0
      PLCHAN(2) = 0
      DOMODL = WMODEL
C                                       DOCHAN = ONECHN(1)
      DOCHAN = .TRUE.
      DOSOUR = T
      DOIFS = ONECHN(2)
      FLALL1 = F
      FLALL2 = F
      SCALX = 1
C                                       Open terminal
      TTY(2) = 0
      CALL ZOPEN (TTY(1), TTY(2), 1, MSGBUF, F, T, T, IRET)
      IF (IRET.NE.0) THEN
         TTY(2) = 0
         WRITE (MSGTXT,1000) IRET
         GO TO 980
         END IF
C                                       open TV
 10   CALL TVOPEN (SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         IF (IRET.NE.4) GO TO 970
         IF (NTRY.GE.MTRY) THEN
            MSGBUF = 'TV is busy: do we wait? Y or N'
            CALL INQSTR (TTY, MSGBUF, 4, CTEMP, IRET)
            IF ((IRET.NE.0) .AND. (IRET.NE.10)) GO TO 950
            CALL CHLTOU (4, CTEMP)
            IF ('Y'.NE.CTEMP(:1)) GO TO 970
            NTRY = 0
            END IF
         NTRY = NTRY + 1
         CALL ZDELAY (DTIME, JERR)
         GO TO 10
         END IF
C                                       Set up graphics planes
      IF (NGRAPH.GE.4) THEN
         GRDA = NGRAY + 4
      ELSE
         GRDA = 1
         END IF
      GRDF = 1
      GRME = NGRAY + 1
      GRCV = NGRAY + MIN (3, NGRAPH)
      GRLI = NGRAY + MIN (2, NGRAPH)
C                                       init the TV
      CALL YINIT (SCRTCH, IRET)
      ROUTIN = 'YINIT'
      IF (IRET.NE.0) GO TO 940
      CURSON = .FALSE.
      GRDFOK = .FALSE.
      GRDAOK = .FALSE.
      GRMEOK = .FALSE.
      GRCVOK = .TRUE.
      GRLIOK = .TRUE.
      DATAOK = .FALSE.
      SCALOK = .FALSE.
      CALL COPY (3, TVZOOM, SVZOOM)
      IB1 = MIN (4, NGRAPH)
      ROUTIN = 'YGRAFX'
      DO 20 I = 1,IB1
         CALL YGRAFX ('READ', I, GCOL(1,I), GCOL(2,I), GCOL(3,I), IRET)
         IF (IRET.NE.0) GO TO 940
         CALL YGRAFX ('WRIT', I, RGB(1,I), RGB(2,I), RGB(3,I), IRET)
         IF (IRET.NE.0) GO TO 940
 20      CONTINUE
C                                       local imvect buffers
      CALL FILL (MAXIMG, 0, IMVOFF)
      CALL FILL (MAXIMG, MAXINT, IMVONN)
C                                       Stokes in data
      CALL FILL (4, 0, ILSTOK)
      IB1 = CATIMG(KINAX+1)
      DO 25 I = 1,IB1
         TEMP = (I - CATIR(KRCRP+1)) * CATIR(KRCIC+1) + CATID(KDCRV+1)
         ILSTOK(I) = IROUND (TEMP)
 25      CONTINUE
C                                       Default STKFLG
      IF (ICOR0.GT.0) THEN
         USTFLG = 'IQUV'
      ELSE
         USTFLG = 'FULL'
         IB1 = -ILSTOK(1)
         IF (CATIMG(KINAX+1).LE.2) THEN
            USTFLG = CHSDEF(IB1)
         ELSE
            IF (IB1.EQ.1) USTFLG = 'NOLL'
            IF (IB1.EQ.2) USTFLG = 'NORR'
            IF (IB1.EQ.5) USTFLG = 'NOYY'
            IF (IB1.EQ.6) USTFLG = 'NOXX'
            END IF
         END IF
C                                       get 1s and 0s flag
      CALL MKSTOK (ILSTOK, USTFLG, STKFLG, IRET)
C                                       get source names
      MAXSOU = 0
      IF (ILOCSU.GE.0) THEN
         DO 50 I = 1,XSTBSZ
            MSGSUP = 32000
            CALL GETSOU (I, IUDISK, IUCNO, CATUV, SLUN, JERR)
            MSGSUP = 0
            IF (JERR.NE.0) GO TO 60
            MAXSOU = MAXSOU + 1
            SNAMES(MAXSOU) = SNAME
 50         CONTINUE
         END IF
 60   CONTINUE
C                                       create/find the FC table
      FCVERS = 0
      CALL IBFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG, FCNUMB,
     *   NNFLAG, FCBUF, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NNFLAG.GT.0) THEN
         WRITE (MSGTXT,1070) FCNUMB, NNFLAG
         CALL MSGWRT (2)
         END IF
      CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       branch to load function to start
      ICOL = 2
      IROW = 21
      GO TO 95
C                                       Set new choice
 90   CALL IBFCHS (PRPOS(1,1), SVZOOM, ICOL, IROW, IRET)
      ROUTIN = 'IBFCHS'
      IF (IRET.NE.0) GO TO 940
 95   GO TO (100, 200), ICOL
C                                       enhancement functions
C                                       OFFZOOM, OFFENHANCE
C                                       TVFIDDLE
 100  IF ((IROW.GE.1) .AND. (IROW.LE.3)) THEN
         CALL IBFUNC (IROW, IRET)
         ROUTIN = 'IBFUNC'
         IF ((IROW.EQ.1) .OR. (IROW.EQ.3)) CALL COPY (3, TVZOOM, SVZOOM)
C                                       enter pixranges amp/phs/dcr
      ELSE IF ((IROW.GE.4) .AND. (IROW.LE.6)) THEN
         IX = IROW - 3
         IF (IX.NE.2) THEN
            WRITE (MSGBUF,1220) CHTYPE(IX)
         ELSE
            WRITE (MSGBUF,1221) CHTYPE(IX)
            END IF
         CALL INQFLT (TTY, MSGBUF, 2, DTEMP, IRET)
         IF (IRET.LT.0) GO TO 200
         IF (IRET.GT.0) GO TO 950
         PIXRNG(1,IX) = DTEMP(1)
         PIXRNG(2,IX) = DTEMP(2)
         IF (IX.EQ.PLTYPE) SCALOK = .FALSE.
C                                       IF select
      ELSE IF ((IROW.EQ.7) .OR. (IROW.EQ.9)) THEN
 110     IF (CATIMG(KINAX+2).GT.1) THEN
            IF (CATIMG(KINAX+2).EQ.2) THEN
               IF (IROW.EQ.7) THEN
                  LIF(1) = 1 + BIF - LIF(1) + BIF
               ELSE
                  LIF(2) = 1 + BIF - LIF(2) + BIF
                  END IF
            ELSE
               I = BIF + CATIMG(KINAX+2) - 1
               IF (IROW.EQ.7) THEN
                  WRITE (MSGBUF,1330) 'First', BIF, I
               ELSE
                  WRITE (MSGBUF,1330) 'Second', BIF, I
                  END IF
               CALL INQINT (TTY, MSGBUF, 1, ITEMP, IRET)
               IF (IRET.GT.0) GO TO 950
               IF (IRET.LT.0) GO TO 110
               IF (IROW.EQ.7) THEN
                  IF ((ITEMP.LT.BIF) .OR. (ITEMP.GT.I)) GO TO 110
                  LIF(1) = ITEMP
               ELSE
                  IF (((ITEMP.LT.BIF) .AND. (ITEMP.NE.0)) .OR.
     *               (ITEMP.GT.I)) GO TO 110
                  LIF(2) = ITEMP
                  END IF
               END IF
            END IF
C                                       Stokes select
      ELSE IF (IROW.EQ.8) THEN
 120     IF (NCOR.GT.2) THEN
            IF (ICOR0.GT.0) THEN
               MSGBUF = 'Enter Stokes string (I,Q,U or V) ' //
     *            'you wish to display'
            ELSE IF (ICOR0.LT.-4) THEN
               MSGBUF = 'Enter Stokes string (XX, YY, XY, YX) ' //
     *            'you wish to display'
            ELSE
               MSGBUF = 'Enter Stokes string (RR, LL, RL, LR) ' //
     *            'you wish to display'
               END IF
 121        CALL INQSTR (TTY, MSGBUF, 4, TSTOK, IRET)
            IF (IRET.EQ.10) THEN
               MSGTXT = 'STRING TOO LONG, TRY AGAIN'
               CALL MSGWRT (7)
               GO TO 121
               END IF
            IF (IRET.NE.0) GO TO 950
            CALL CHKSTK (TSTOK, IRET)
            IF (IRET.EQ.1) GO TO 120
         ELSE IF (NCOR.EQ.2) THEN
            LSTOKS = 3 - LSTOKS
            END IF
         IF (.NOT.DOSTOK (ILSTOK, STKFLG, LSTOKS)) THEN
            MSGTXT = '*****  CURRENT STOKES FLAG DOES NOT INCLUDE ' //
     *         'NEW STOKES  ****'
            CALL MSGWRT (7)
            END IF
C                                       Toggle 'show all vis' flag
      ELSE IF (IROW.EQ.10) THEN
         SHOWVS = .NOT.SHOWVS
         IF (SHOWVS) THEN
            IF ((GRDFOK) .AND. (GRDF.NE.GRDA)) THEN
               CALL YSLECT ('ONNN', GRDF, 0, SCRTCH, IRET)
               ROUTIN = 'YSLECT'
               IF (IRET.NE.0) GO TO 940
               END IF
         ELSE IF (GRDF.NE.GRDA) THEN
            CALL YSLECT ('OFFF', GRDF, 0, SCRTCH, IRET)
            ROUTIN = 'YSLECT'
            IF (IRET.NE.0) GO TO 940
            END IF
C                                       Error bars
C                                       All Ch/IF/Source flags
      ELSE IF ((IROW.GE.11) .AND. (IROW.LE.15)) THEN
         IF (IROW.EQ.11) DOERRB = .NOT.DOERRB
         IF (IROW.EQ.12) DOMODL = .NOT.DOMODL
         IF (IROW.EQ.13) DOCHAN = .NOT.DOCHAN .OR. ONECHN(1)
         IF (IROW.EQ.14) DOIFS = .NOT.DOIFS .OR. ONECHN(2)
         IF (IROW.EQ.15) DOSOUR = .NOT.DOSOUR
C                                       set Stokes mask
      ELSE IF (IROW.EQ.16) THEN
 130     MSGBUF = 'Enter Stokes string or flag mask, 4 chars start in'
     *      // ' col 1'
         CALL INQSTR (TTY, MSGBUF, 4, USTFLG, IRET)
         IF (IRET.EQ.10) THEN
            MSGTXT = 'STRING TOO LONG, TRY AGAIN'
            CALL MSGWRT (7)
            GO TO 130
            END IF
         IF (IRET.NE.0) GO TO 950
         CALL CHLTOU (4, USTFLG)
C                                       get 1s and 0s flag
         CALL MKSTOK (ILSTOK, USTFLG, STKFLG, IRET)
         IF (IRET.NE.0) THEN
             MSGTXT = 'STOKES FLAG ''' // USTFLG //
     *          ''' NOT RECOGNIZED OR INAPROPRIATE'
             CALL MSGWRT (6)
             GO TO 130
             END IF
         IF (.NOT.DOSTOK (ILSTOK, STKFLG, PLSTOK)) THEN
            MSGTXT = '****  NEW STOKES FLAG DOES NOT INCLUDE ' //
     *         'CURRENT STOKES  ****'
            CALL MSGWRT (7)
            IF (DOSTOK (ILSTOK, STKFLG, LSTOKS)) THEN
               MSGTXT = '****  new Stokes flag does include new ' //
     *            'Stokes - do a LOAD  ****'
               CALL MSGWRT (3)
               END IF
            END IF
C                                       Set up 'flag all baselines
C                                       to one antenna' flag
      ELSE IF (IROW.EQ.17) THEN
         FLALL1 = .NOT.FLALL1
      ELSE IF (IROW.EQ.18) THEN
         FLALL2 = .NOT.FLALL2
         END IF
      GO TO 90
C                                       Flagging
 200  IF ((IROW.GE.1) .AND. (IROW.LE.8)) THEN
         CALL IBFLAG (IROW, TTY, SNAMES, IRET)
         IF (IRET.GT.0) GO TO 990
C                                       unflagging functions
C                                       LIST FLAGS, UNDO FLAGS,
      ELSE IF ((IROW.EQ.9) .OR. (IROW.EQ.10)) THEN
         CALL IBFUNF (IROW-8, TTY, SNAMES, IRET)
         ROUTIN = 'IBFUNF'
         IF (IRET.GT.0) GO TO 940
         IF ((IRET.EQ.0) .AND. ((IROW.EQ.10))) DATAOK = .FALSE.
C                                       Select amp/phs/decor
      ELSE IF ((IROW.GE.11) .AND. (IROW.LE.13)) THEN
         LTYPE = IROW - 10
         IF ((NXANT1(BASPT).EQ.NXANT2(BASPT)) .AND. (LTYPE.EQ.2))
     *      LTYPE = 1
         IF (LTYPE.EQ.PLTYPE) IRET = -1
C                                       Select frame
      ELSE IF (IROW.EQ.14) THEN
         CALL COPY (4, DAWIN, CDAWIN)
         IF ((SHOWVS) .AND. (GRDFOK)) THEN
            CALL FRMSEL (IRET)
            ROUTIN = 'FRMSEL'
            IF (IRET.GT.0) GO TO 940
         ELSE
            MSGTXT = 'Make sure top plot is selected and updated first'
            CALL MSGWRT (6)
            END IF
         IF ((CDAWIN(1).EQ.DAWIN(1)) .AND. (CDAWIN(3).EQ.DAWIN(3)))
     *      IRET = -1
C                                       First frame
      ELSE IF (IROW.EQ.15) THEN
         CALL COPY (4, DAWIN, CDAWIN)
         IF (DAWIN(1).GT.1) THEN
            IF ((SCALX.LT.1) .OR. (SCALX.GT.20)) SCALX = 1
            DAWIN(1) = 1
            DAWIN(3) = DAWIN(1) + (TVWIN(3) - TVWIN(1)) / SCALX
            END IF
         IF ((CDAWIN(1).EQ.DAWIN(1)) .AND. (CDAWIN(3).EQ.DAWIN(3)))
     *      IRET = -1
C                                       Next frame
      ELSE IF (IROW.EQ.16) THEN
         CALL COPY (4, DAWIN, CDAWIN)
         IF ((DAWIN(1).GT.1) .OR. (DAWIN(3).LT.NV)) THEN
            IF (DAWIN(3).GE.NV) THEN
               MSGTXT = 'No data left on this baseline, try another' //
     *            ' command'
               CALL MSGWRT (6)
            ELSE
               I = DAWIN(3) - DAWIN(1)
               DAWIN(1) = DAWIN(3) + 1
               DAWIN(3) = DAWIN(1) + I
               END IF
            END IF
         IF ((CDAWIN(1).EQ.DAWIN(1)) .AND. (CDAWIN(3).EQ.DAWIN(3)))
     *      IRET = -1
C                                       Previous frame
      ELSE IF (IROW.EQ.17) THEN
         CALL COPY (4, DAWIN, CDAWIN)
         IF ((DAWIN(1).GT.1) .OR. (DAWIN(3).LT.NV)) THEN
            IF (DAWIN(1).LE.1) THEN
               MSGTXT = 'Already at first frame, try another command'
               CALL MSGWRT (6)
            ELSE
               I = DAWIN(3) - DAWIN(1)
               DAWIN(3) = DAWIN(1) - 1
               DAWIN(1) = DAWIN(3) - I
               END IF
            END IF
         IF ((CDAWIN(1).EQ.DAWIN(1)) .AND. (CDAWIN(3).EQ.DAWIN(3)))
     *      IRET = -1
C                                       Set baseline
      ELSE IF (IROW.EQ.18) THEN
 220     MSGBUF = 'Enter 2 antenna numbers to be displayed (integer)'
         CALL INQINT (TTY, MSGBUF, 2, KTEMP, IRET)
         IF (IRET.GT.0) GO TO 950
         IF (IRET.LT.0) GO TO 220
         IF (KTEMP(1).GT.KTEMP(2)) THEN
            ITEMP = KTEMP(1)
            KTEMP(1) = KTEMP(2)
            KTEMP(2) = ITEMP
            END IF
         IF ((KTEMP(1).LT.1) .OR. (KTEMP(2).GT.MAXANT)) GO TO 220
         NBASE = MXBASE
         CALL DECPTR (KTEMP(1), KTEMP(2), NXANT1, NXANT2, NBASE, I)
         IF ((I.GT.0) .AND. (I.NE.BASPT) .AND. (I.LE.NUMBAS)) THEN
            CALL FILL (4, 0, DAWIN)
            BASPT = I
         ELSE
            IRET = -1
            END IF
C                                       Next baseline
      ELSE IF (IROW.EQ.19) THEN
         UPTR = .TRUE.
         IF (NUMBAS.GT.1) THEN
            DATAOK = .FALSE.
            BASPT = MOD (BASPT, NUMBAS) + 1
            CALL FILL (4, 0, DAWIN)
         ELSE
            IRET = -1
            END IF
C                                       previous baseline
      ELSE IF (IROW.EQ.20) THEN
         UPTR = .FALSE.
         IF (NUMBAS.GT.1) THEN
            DATAOK = .FALSE.
            BASPT = BASPT - 1
            IF (BASPT.LE.0) BASPT = NUMBAS
            CALL FILL (4, 0, DAWIN)
         ELSE
            IRET = -1
            END IF
C                                       load forced
      ELSE IF (IROW.EQ.21) THEN
         DATAOK = .FALSE.
C                                       Do flagging and Exit
      ELSE IF (IROW.EQ.22) THEN
         CALL ZDATE (DATE)
         CALL ZTIME (TIME)
         DATE(1) = -DATE(1)
         CALL TIMDAT (TIME, DATE, TTIME(2), TTIME)
         WRITE (MSGBUF,1190) NNFLAG
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IRET)
         IF (IRET.NE.0) GO TO 950
         IB1 = 0
         IF (NNFLAG.LE.0) GO TO 990
 250     MSGBUF = 'Do you wish to enter them in the data?  Y/N'
         CALL INQSTR (TTY, MSGBUF, 4, CTEMP, IRET)
         IF (IRET.EQ.10) THEN
            MSGTXT = 'STRING TOO LONG, TRY AGAIN'
            CALL MSGWRT (7)
            GO TO 250
            END IF
         IF (IRET.NE.0) GO TO 950
         IB1 = IB1 + 1
         CALL CHLTOU (4, CTEMP)
         EQUAL = 'Y'.EQ.CTEMP(:1)
         NOFLAG = 'N'.EQ.CTEMP(:1)
         IF ((.NOT.EQUAL) .AND. (.NOT.NOFLAG)) GO TO 250
         IF (NOFLAG) THEN
            IF (XDOCAT.LE.0.0) THEN
               IF (IB1.LE.1) THEN
                  MSGBUF = 'WARNING: THESE COMMANDS ARE ABOUT TO BE'
     *               // ' LOST, so again:'
                  CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF,
     *               IRET)
                  IF (IRET.NE.0) GO TO 950
                  GO TO 250
               ELSE
                  MSGTXT = 'SO BE IT'
                  CALL MSGWRT (6)
                  END IF
               END IF
            NNFLAG = 0
            GO TO 990
         ELSE
            MSGTXT = 'Begin writing a flag table'
            CALL MSGWRT (2)
            IF (LQUICK) CALL RELPOP (IRET, SCRTCH, JERR)
            IB1 = MIN (4, NGRAPH)
            JERR = 0
            DO 255 I = 1,IB1
               IF (JERR.LE.0) CALL YGRAFX ('WRIT', I, GCOL(1,I),
     *            GCOL(2,I), GCOL(3,I), JERR)
 255           CONTINUE
            CALL TVCLOS (SCRTCH, JERR)
            RQUICK = LQUICK
            CALL IBFMRK (IRET)
C                                       history file write
            IF (IRET.EQ.0) CALL IBFLHI (SNAMES)
C                                       destroy master file
            IF ((XDOCAT.GT.0.0) .AND. (IRET.EQ.0)) THEN
               NNFLAG = 0
               NCFILE = NCFILE - 1
               TSTOK = 'CLWR'
               CALL CATDIR ('CSTA', DISKOU, CNOOUT, OUTNAM, OUTCLS,
     *            SEQOUT, OUTYPE, NLUSER, TSTOK, SCRTCH, JERR)
               IF (JERR.NE.0) THEN
                  WRITE (MSGTXT,1255) JERR
                  CALL MSGWRT (6)
                  END IF
               CALL MDESTR (DISKOU, CNOOUT, CATBLK, SCRTCH, I, JERR)
               IF (JERR.NE.0) THEN
                  WRITE (MSGTXT,1256) JERR
                  CALL MSGWRT (6)
               ELSE
                  MSGTXT = 'Master file destroyed'
                  CALL MSGWRT (3)
                  END IF
            ELSE
               MSGTXT = 'Temporary master file to be destroyed'
               CALL MSGWRT (3)
               END IF
            GO TO 995
            END IF
         END IF
C                                       Refresh the plot if needed
      IF ((IROW.GE.10) .AND. (IRET.EQ.0)) THEN
         CALL SETFRM (IRET)
         ROUTIN = 'SETFRM'
         IF (IRET.GT.0) GO TO 940
         END IF
      GO TO 90
C                                       error
 940  WRITE (MSGTXT,1940) IRET, ROUTIN
      CALL MSGWRT (8)
      GO TO 990
 950  WRITE (MSGTXT,1950) IRET
      CALL MSGWRT (8)
      ROUTIN = ' '
      GO TO 990
 970  CALL MSGWRT (8)
      CALL ZCLOSE (TTY(1), TTY(2), JERR)
      GO TO 999
 980  CALL MSGWRT (8)
      GO TO 999
C                                       closes
 990  IF ((ROUTIN.NE.'YINIT') .AND. (ROUTIN.NE.'YGRAFX')) THEN
         IB1 = MIN (4, NGRAPH)
         JERR = 0
         DO 991 I = 1,IB1
            IF (JERR.LE.0) CALL YGRAFX ('WRIT', I, GCOL(1,I),
     *         GCOL(2,I), GCOL(3,I), JERR)
 991     CONTINUE
         END IF
      CALL TVCLOS (SCRTCH, JERR)
C                                       destroy temporary master file
      IF (XDOCAT.LE.0.0) THEN
         MSGTXT = 'Temporary master file to be destroyed'
         CALL MSGWRT (3)
         END IF
C
 995  CALL ZCLOSE (TTY(1), TTY(2), JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANNOT OPEN YOUR TERMINAL: ERROR',I5)
 1010 FORMAT ('CANNOT OPEN THE TV DEVICE: ERROR',I5)
 1070 FORMAT ('Existing FC table has',I5,' flag commands, with',I6,
     *   ' total flags')
 1220 FORMAT ('Enter ',A8,' range to display (2 reals)')
 1221 FORMAT ('Enter ',A5,' range (degrees) to display (2 reals)')
 1255 FORMAT ('ERROR',I5,' CLEARING STATUS OF MASTER FILE')
 1256 FORMAT ('ERROR',I5,' DESTROYING MASTER FILE')
 1330 FORMAT ('Enter ',A,' IF number between',I3,' and',I4,
     *   '  (integer)')
 1190 FORMAT (I8,' Flagging commands have been prepared')
 1940 FORMAT ('TELEVISION I/O ERROR',I5,' FROM ',A)
 1950 FORMAT ('TERMINAL I/O ERROR',I5)
      END
      SUBROUTINE IBFLHI (SNAMES)
C-----------------------------------------------------------------------
C   IBFLHI adds to the history file of the input UV data set info on
C   what was flagged.  It then removes that flagging info from the
C   flag command file and from the master grid, when these are kept in
C   the image catalog.
C-----------------------------------------------------------------------
      CHARACTER SNAMES(*)*16
C
      CHARACTER HILINE*72
      INTEGER   HLUNI, IERR, I, I1, I2, PLCH, PIF, J, PLAR, IRET,
     *   ITPSL1(4), ITPSL2(4), PLSU, IFCBUF(18)
      REAL      DTEMP, RFCBUF(18)
      LOGICAL   SAVE
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (IFCBUF, RFCBUF)
      DATA HLUNI /28/
      DATA SAVE /.TRUE./
C-----------------------------------------------------------------------
C                                       history only if some flagged
      IF ((NNFLAG.LE.0) .OR. (XDOHST.LE.-9.5)) GO TO 999
      CALL HIINIT (3)
      CALL HIOPEN (HLUNI, DISKIN, CNOIN, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Task message
      WRITE (HILINE,1000) TSKNAM, RLSNAM, TTIME
      CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Task parms
      DO 10 I = 1,30
         IF ((SOURCS(I).NE.' ') .AND. ((I.LT.2) .OR.
     *      (SOURCS(I).NE.SOURCS(I-1)))) THEN
            I1 = 1
            IF (SOURCS(I)(1:1).EQ.'-') I1 = 2
            IF (DOSWNT) WRITE (HILINE,1001) TSKNAM, SOURCS(I)(I1:)
            IF (.NOT.DOSWNT) WRITE (HILINE,1002) TSKNAM, SOURCS(I)(I1:)
            CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
 10      CONTINUE
C                                       start and stop times
      CALL HITIME (START, STOP, HLUNI, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Stokes, subarray, IF, chan
      WRITE (HILINE,1015) TSKNAM, STOKES, SUBARR
      CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      IF ((BIF.NE.1) .OR. (EIF.NE.1)) THEN
         WRITE (HILINE,1016) TSKNAM, BIF, EIF
         IF (IFAVG) WRITE (HILINE,1017) TSKNAM, BIF, EIF
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF ((BCHAN.NE.1) .OR. (ECHAN.NE.1)) THEN
         WRITE (HILINE,1018) TSKNAM, BCHAN, ECHAN
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF ((UVRNG(1).GT.0.0) .OR. (UVRNG(2).GT.0.0)) THEN
         WRITE (HILINE,1019) TSKNAM, UVRNG
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       calibration tables
      IF (FGVER.GT.0) THEN
         WRITE (HILINE,1020) TSKNAM, FGVER
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (DOCAL) THEN
         WRITE (HILINE,1021) TSKNAM, CLUSE
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         IF (DOBL) THEN
            WRITE (HILINE,1022) TSKNAM, BLVER
            CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
         END IF
      I1 = 1
      IF ((DESEL) .AND. (NXANT.GT.0)) THEN
         WRITE (HILINE,1030) TSKNAM
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (NXANT.LE.0) THEN
         WRITE (HILINE,1031) TSKNAM
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
 35   I2 = I1 + 12
      IF (I2.GT.NXANT) I2 = NXANT
      IF (I2.GE.I1) THEN
         WRITE (HILINE,1035) TSKNAM, (IXANT(I), I = I1,I2)
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         I1 = I2 + 1
         GO TO 35
         END IF
      I1 = 1
      WRITE (HILINE,1040) TSKNAM
      IF (NXBASL.LE.0) WRITE (HILINE,1041) TSKNAM
      CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
 45   I2 = I1 + 12
      IF (I2.GT.NXBASL) I2 = NXBASL
      IF (I2.GE.I1) THEN
         WRITE (HILINE,1045) TSKNAM, (IXBASL(I), I = I1,I2)
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         I1 = I2 + 1
         GO TO 45
         END IF
C                                       flagging commands
      WRITE (HILINE,1055) TSKNAM, TSKNAM, TTIME
      CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      IF (XDOHST.GT.0.0) THEN
         PLCH = -1
         PIF = -1
         PLAR = -1
         PLSU = -1
         CALL IBFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG, FCNUMB,
     *      NNFLAG, FCBUF, IRET)
         IF (IRET.NE.0) GO TO 100
         CALL FILL (18, 0, IFCBUF)
         DO 80 I = 1,NNFLAG
            CALL TABIO ('READ', 0, I, FCTIME, FCBUF, IRET)
            IF (IRET.NE.0) GO TO 100
C                                       Duplicate?
            IF (ABS(FCTIME(1)-RFCBUF(1)).GT.EPSTIM/4.) GO TO 75
            IF (ABS(FCTIME(2)-RFCBUF(2)).GT.EPSTIM/4.) GO TO 75
            IF (FCSFLG.NE.RFCBUF(7)) GO TO 75
            IF (FCBASL(1).NE.IFCBUF(12)) GO TO 75
            IF (FCBASL(2).NE.IFCBUF(13)) GO TO 75
            IF (FCSOUR.NE.IFCBUF(14)) GO TO 75
            IF (FCCHAN(1).NE.IFCBUF(15)) GO TO 75
            IF (FCCHAN(2).NE.IFCBUF(16)) GO TO 75
            IF (FCIF(1).NE.IFCBUF(17)) GO TO 75
            IF (FCIF(2).NE.IFCBUF(18)) GO TO 75
            GO TO 80
C                                       Not duplicate
 75         CALL RCOPY (18, FCTIME, RFCBUF)
            J = FCCHAN(1)
            IF (J.NE.PLCH) THEN
               WRITE (HILINE,1061) TSKNAM, J
               IF (J.LE.0) WRITE (HILINE,1062) TSKNAM
               CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 100
               PLCH = J
               END IF
            J = FCIF(1)
            IF (J.NE.PIF) THEN
               WRITE (HILINE,1063) TSKNAM, FCIF
               IF (J.LE.0) WRITE (HILINE,1064) TSKNAM
               CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 100
               PIF = J
               END IF
            J = XSUBA + 0.01
            IF (J.NE.PLAR) THEN
               WRITE (HILINE,1065) TSKNAM, J
               IF (J.LE.0) WRITE (HILINE,1066) TSKNAM
               CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 100
               PLAR = J
               END IF
            IF (FCTIME(1).GT.-1.E5) THEN
               DTEMP = MAX (START, MIN (STOP, FCTIME(1)))
               CALL TODHMS (DTEMP, ITPSL1)
               DTEMP = MAX (START, MIN (STOP, FCTIME(2)))
               CALL TODHMS (DTEMP, ITPSL2)
               END IF
            IF (FCBASL(1).LE.0) THEN
               IF (FCTIME(1).GT.-1.E5) THEN
                  WRITE (HILINE,1067) TSKNAM, ITPSL1, ITPSL2
               ELSE
                  WRITE (HILINE,1068) TSKNAM
                  END IF
            ELSE
               IF (FCTIME(1).GT.-1.E5) THEN
                  WRITE (HILINE,1070) TSKNAM, FCBASL, ITPSL1,
     *               ITPSL2
               ELSE
                  WRITE (HILINE,1071) TSKNAM, FCBASL
                  END IF
               END IF
            CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
C                                       source number
            J = FCSOUR
            IF (J.NE.PLSU) THEN
               IF (J.GT.0) THEN
                  WRITE (HILINE,1075) TSKNAM, J, SNAMES(J)
               ELSE
                  WRITE (HILINE,1076) TSKNAM
                  END IF
               CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 100
               PLSU = J
               END IF
 80         CONTINUE
         CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IRET)
         END IF
C                                       Close HI file
 100  CALL HICLOS (HLUNI, SAVE, BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A9,2X,A8)
 1001 FORMAT (A6,'SOURCES=''',A16,'''',9X,'/ Source name included')
 1002 FORMAT (A6,'SOURCES=''',A16,'''',9X,'/ Source name excluded')
 1015 FORMAT (A6,'STOKES=''',A4,'''  SUBARRAY=',I3)
 1016 FORMAT (A6,'BIF=',I4,2X,'EIF=',I4,5X,'/ Range of IF axis')
 1017 FORMAT (A6,'BIF=',I4,2X,'EIF=',I4,5X,'/ Range of IF axis')
 1018 FORMAT (A6,'BCHAN=',I4,2X,'ECHAN=',I4,5X,'/ Channels averaged')
 1019 FORMAT (A6,'UVRANGE = ',2(1PE13.5),5X,
     *   '/ Range of uv kilo lambda')
 1020 FORMAT (A6,'FLAGVER=',I3,5X,'/ FLAG table used')
 1021 FORMAT (A6,'GAINUSE=',I3,5X,'/ CL table used')
 1022 FORMAT (A6,'BLVER=',I3,5X,'/ Baseline table used')
 1030 FORMAT (A6,'/ All antennas except:')
 1031 FORMAT (A6,'/ All antennas')
 1035 FORMAT (A6,'ANTENNAS=',I3,12(',',I3),',')
 1040 FORMAT (A6,'/ with')
 1041 FORMAT (A6,'/ with all antennas')
 1045 FORMAT (A6,'BASELINES=',I3,12(',',I3),',')
 1055 FORMAT (A6,' REASON = ''',A5,1X,A9,1X,A8,'''')
 1061 FORMAT (A6,' CHAN =',I5)
 1062 FORMAT (A6,' CHAN = 0',10X,'/ All channels')
 1063 FORMAT (A6,' IF =',I4,',',I8)
 1064 FORMAT (A6,' IF = 0',10X,'/ All IFs')
 1065 FORMAT (A6,' SUBARRAY =',I5)
 1066 FORMAT (A6,' SUBARRAY = 0',10X,'/ All subarrays')
 1067 FORMAT (A6,' ANT = 0, 0  TIMERANG=',I4.2,'/',2(I2.2,':'),I2.2,
     *   ',',I4.2,'/',2(I2.2,':'),I2.2)
 1068 FORMAT (A6,' ANT = 0,0  TIMERANG=  00/00:00:00,  00/00:00:00')
 1070 FORMAT (A6,' ANT = ',I2,',',I2,'   TIMERANG=',
     *   I4.2,'/',2(I2.2,':'),I2.2,',',I4.2,'/',2(I2.2,':'),I2.2)
 1071 FORMAT (A6,' ANT = ',I2,',',I2,
     *   '   TIMERANG=  00/00:00:00,  00/00:00:00')
 1075 FORMAT (A6,' SOURCE =',I5,10X,'/ Source number for ',A)
 1076 FORMAT (A6,' SOURCE =    0',10X,'/ All source numbers')
      END
      SUBROUTINE IBFLAG (BRANCH, TTY, SNAMES, IRET)
C-----------------------------------------------------------------------
C   does various forms of interactive TV flagging using the currently
C   displayed file
C   Inputs:
C      BRANCH   I      1 => flag time
C                      2 => flag timerange
C                      3 => flag area
C                      4 => clip above a level
C                      5 => clip below a level
C                      6 => clip based on rms
C                      7 => interactive clip
C      TTY      I(2)   LUN, IND for terminal interaction
C      SNAMES   C(*)   source list
C   Output:
C      IRET     I      error code
C-----------------------------------------------------------------------
      INTEGER   BRANCH, TTY(2), IRET
      CHARACTER SNAMES(*)*16
C
      INCLUDE 'IBLED.INC'
      CHARACTER FIXIT*4, ROUTIN*6, OPERS(8)*8, BLNAME*11, LFLUXS*12
      INTEGER   NPIX, NROW, MAG, IX0, IY0, IX, IY, QUAD, IBUT, ITW(3),
     *   TVX, TVY, I, NB, JERR, IXP(5), IYP(5), IXOFF, NCLIP, MAXCLP,
     *   DOBLC, IROUND, LBUT, NFLAGD(2), PFLAGD(2), MASK, LTVX, LSOU,
     *   IX1, IX2, IY1, IY2, ID, MCLIP, JXP, JYP(2), TVRMS(4,2),
     *   IBLC(2), ITRC(2), ZAND
      LOGICAL   DOIT, T, F, DOBOX, BOX1, BELOW, DOSTOK
      REAL      CATR(256), RPOS(2), PPOS(2), CORN(2),
     *   ICLIP(4,MXCLIP), FLUXCL(MAXVIS), FLUXB, FLUXA, LEVAL
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA FIXIT /'FXIT'/
      DATA T, F /.TRUE.,.FALSE./
      DATA OPERS /'TIME', 'TIMERANG', 'AREA', 'CLIPA', 'CLIPB','MEAN',
     *   'INT.CLIP','ALL TIME'/
      DATA MAXCLP /MXCLIP/
C-----------------------------------------------------------------------
C                                       non-interactive
      IF (BRANCH.EQ.8) THEN
         FCLIPR(1) = 0.0
         FCLIPR(2) = 0.0
         CALL CHR2H (8, OPERS(8), 1, FCOPER)
         IBLC(1) = TVWIN(1)
         ITRC(1) = TVWIN(3)
         IBLC(2) = TVWIN(2)
         ITRC(2) = TVWIN(4)
         GO TO 300
         END IF
C                                       interactive OPs
      IRET = 2
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.7)) GO TO 999
      BLNAME = ' '
C                                       check window of TV
      CALL IBCHKW (IRET)
C                                       if resize - redraw data
      IF (.NOT.GRDAOK) THEN
         CALL SETFRM (IRET)
         ROUTIN = 'SETFRM'
         IF (IRET.NE.0) GO TO 800
         END IF
C                                        clean up
      CALL YHOLD ('ONNN', I)
      MASK = 2 ** (GRDF - 1)
      MASK = ZAND (MASK, TVLIMG(1))
      IF (MASK.EQ.0) THEN
         CALL YSLECT ('ONNN', GRDF, 0, SCRTCH, IRET)
         ROUTIN = 'YSLECT'
         IF (IRET.NE.0) GO TO 800
         END IF
      MASK = 2 ** (GRDA - 1)
      MASK = ZAND (MASK, TVLIMG(1))
      IF (MASK.EQ.0) THEN
         CALL YSLECT ('ONNN', GRDA, 0, SCRTCH, IRET)
         ROUTIN = 'YSLECT'
         IF (IRET.NE.0) GO TO 800
         END IF
C                                       Initialization
      LTVX = -1
      LSOU = -1
      LEVAL = -2.0
      LFLUXS = ' '
      I = 4 * MAXCLP
      CALL RCOPY (I, 0.0, ICLIP(1,1))
      DOBOX = (BRANCH.GE.2) .AND. (BRANCH.LE.3)
      DOBLC = 0
      CALL FILL (5, 1, IXP)
      CALL FILL (5, 1, IYP)
      NB = 2
C                                       FC table
      FCLIPR(1) = 0.0
      FCLIPR(2) = 0.0
      CALL CHR2H (8, OPERS(BRANCH), 1, FCOPER)
C                                       Check the graphics
      ROUTIN = 'YZERO'
      IF (.NOT.GRCVOK) THEN
         CALL YZERO (GRCV, IRET)
         IF (IRET.NE.0) GO TO 800
         GRCVOK = .TRUE.
         IF (GRCV.EQ.GRLI) GRLIOK = .TRUE.
         END IF
      IF (.NOT.GRLIOK) THEN
         CALL YZERO (GRLI, IRET)
         IF (IRET.NE.0) GO TO 800
         GRLIOK = .TRUE.
         END IF
      ROUTIN = 'YSLECT'
      MASK = 2 ** (GRCV - 1)
      MASK = ZAND (MASK, TVLIMG(1))
      IF (MASK.EQ.0) THEN
         CALL YSLECT ('ONNN', GRCV, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 800
         END IF
      MASK = 2 ** (GRLI - 1)
      MASK = ZAND (MASK, TVLIMG(1))
      IF (MASK.EQ.0) THEN
         CALL YSLECT ('ONNN', GRLI, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 800
         END IF
      IF ((GRLI.EQ.GRME) .OR. (GRCV.EQ.GRME)) GRMEOK = .FALSE.
      CALL YHOLD ('OFFF', I)
C                                       CURVAL display location
      NPIX = 11 * CSIZTV(1)
      NROW = 3
      IF (MAXSOU.GT.0) NROW = NROW + 1
      NROW = NROW * (2*MEDGE + CSIZTV(2))
      MAG = 1 + TVZOOM(1)
      IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
      IX0 = WINDTV(1) - (MAG-1)/2
      IY0 = WINDTV(4) - MAG*NROW + 1 - (MAG-1)/2
      IF (MAG.GT.1) IY0 = IY0 + MAG
      IX0 = (IX0 - TVZOOM(2))/MAG + TVZOOM(2)
      IY0 = (IY0 - TVZOOM(3))/MAG + TVZOOM(3)
      IX0 = MAX (1, IX0)
      IY0 = MAX (1, IY0)
      IF (IX0+NPIX-1.GT.MAXXTV(1)) IX0 = MAXXTV(1) - NPIX + 1
      IF (IY0+NROW-1.GT.MAXXTV(2)) IY0 = MAXXTV(2) - NROW + 1
C                                       set to top row of text
      IX0 = IX0 + MEDGE
      IY0 = IY0 + 5*MEDGE + 2*CSIZTV(2)
      IF (MAXSOU.GT.0) IY0 = IY0 + 2*MEDGE + CSIZTV(2)
C                                       get image header
      IX = (WINDTV(1) + WINDTV(3)) / 2
      IY = (WINDTV(2) + WINDTV(4)) / 2
      CALL YCREAD (GRDA, IX, IY, CATBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1005) IRET
         GO TO 990
         END IF
C                                       cursor on
      QUAD = -1
      IF (CURSON) THEN
         CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IRET)
      ELSE
         RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
         RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
         CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IRET)
         END IF
      ROUTIN = 'YCURSE'
      IF (IRET.NE.0) GO TO 800
      CURSON = .TRUE.
      RPOS(1) = IX
      RPOS(2) = IY
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL ZTIME (ITW)
C                                       instructions: Buttons
C                                       time
 90   IF (BRANCH.EQ.1) THEN
         MSGTXT = 'Hit button A or B to mark flagged position,' //
     *      ' loop for more'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to mark flagged position, return' //
     *      ' to menu'
C                                       timerange
      ELSE IF (BRANCH.EQ.2) THEN
         MSGTXT = 'Hit button A to switch between start and stop time'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button B to mark stop time, loop for more'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to mark stop time, return to menu'
C                                       area
      ELSE IF (BRANCH.EQ.3) THEN
         MSGTXT = 'Hit button A to switch between BLC and TRC'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button B to mark final box, loop for more'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to mark final box, return to menu'
C                                       clip above/below
      ELSE IF ((BRANCH.EQ.4) .OR. (BRANCH.EQ.5)) THEN
         MSGTXT = 'Hit button A to switch between start and end' //
     *      ' times'
         CALL MSGWRT (1)
         MSGTXT = 'Hit buttons B or C to mark final time and level'
C                                       rms edit
      ELSE IF (BRANCH.EQ.6) THEN
         MSGTXT = 'Hit button A to switch between start and end' //
     *      ' points for mean estimation'
         CALL MSGWRT (1)
         MSGTXT = 'Hit buttons B or C to mark final point for mean ' //
     *      'estimation'
C                                       interactive clip
      ELSE IF (BRANCH.EQ.7) THEN
         MSGTXT = 'Hit button A to mark start point of interactive clip'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button B to mark intermediate point of' //
     *      ' interactive clip'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to mark final point of interactive clip'
         END IF
      CALL MSGWRT (1)
C                                       instructions: Button D
      MSGTXT = 'Hit button D to exit - no further flagging'
      CALL MSGWRT (1)
      NCLIP = 0
      PFLAGD(1) = 0
      PFLAGD(2) = 0
C                                        read until cursor moves
 100  CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IRET)
      ROUTIN = 'YCURSE'
      IF (IRET.NE.0) GO TO 800
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (.NOT.DOIT) GO TO 100
C                                       button D
         IF (IBUT.GE.8) GO TO 910
C                                       get TV and uv image pixels
         CALL YCURSE (FIXIT, F, T, RPOS, QUAD, LBUT, IRET)
         CALL IMA2MP (RPOS, CORN)
         CALL CORNER (CATBLK, IIWIN, CORN)
         TVX = CORN(1) + 0.49
         TVY = CORN(2) + 0.49
         CALL MP2IMA (CORN, RPOS)
C                                       Do CURVALUE anotation
         CALL TVANOT (TVX, TVY, IX0, IY0, LSOU, SNAMES, LTVX, LEVAL,
     *      LFLUXS, IRET)
         IF (IRET.NE.0) GO TO 900
C                                       clear old graph 3 lines
C                                       unless interactive clip
C                                       or 'drag a box'
         IF (BRANCH.NE.7) THEN
            I = 2
            IF (DOBOX) I = 5
            IF ((DOBOX) .AND. (DOBLC.EQ.0)) I = 3
            CALL IMVECT (GRLI, I, IXP, IYP, IMVOFF, IRET)
            ROUTIN = 'IMVECT'
            IF (IRET.NE.0) GO TO 800
C                                       A single time
            IF (BRANCH.EQ.1) THEN
               IXP(1) = RPOS(1) + 0.49
               IXP(2) = IXP(1)
               IYP(1) = TVWIN(4)
               IYP(2) = TVWIN(2)
C                                       Clip lines, rms estimation
            ELSE IF ((BRANCH.EQ.4) .OR. (BRANCH.EQ.5) .OR.
     *         (BRANCH.EQ.6)) THEN
               IF (DOBLC.EQ.2) THEN
                  IXP(2) = RPOS(1) + 0.49
               ELSE
                  IXP(1) = RPOS(1) + 0.49
                  IF (DOBLC.EQ.0) IXP(2) = TVWIN(3)
                  IYP(1) = RPOS(2) + 0.49
                  IYP(2) = IYP(1)
                  END IF
C                                       haven't yet defined box
            ELSE IF (DOBLC.EQ.0) THEN
               IXP(1) = TVWIN(3)
               IXP(2) = RPOS(1) + 0.49
               IXP(3) = IXP(2)
               IYP(1) = RPOS(2) + 0.49
               IF (BRANCH.EQ.2) IYP(1) = TVWIN(2)
               IYP(2) = IYP(1)
               IYP(3) = TVWIN(4)
C                                       have defined box
            ELSE IF (DOBLC.EQ.1) THEN
               IXP(1) = RPOS(1) + 0.49
               IXP(2) = IXP(1)
               IXP(5) = IXP(1)
               IYP(2) = RPOS(2) + 0.49
               IF (BRANCH.EQ.2) IYP(2) = TVWIN(2)
               IYP(3) = IYP(2)
            ELSE
               IXP(3) = RPOS(1) + 0.49
               IXP(4) = IXP(3)
               IYP(4) = RPOS(2) + 0.49
               IF (BRANCH.EQ.2) IYP(4) = TVWIN(4)
               IYP(1) = IYP(4)
               IYP(5) = IYP(4)
               END IF
            CALL IMVECT (GRLI, I, IXP, IYP, IMVONN, IRET)
            IF (IRET.NE.0) GO TO 800
            GRLIOK = .FALSE.
            END IF
C                                       act on buttons
         IF (IBUT.EQ.0) GO TO 100
C                                       button B, C = A w/o a TRC
         IF ((DOBOX) .AND. (DOBLC.LE.0)) IBUT = 1
         IF ((BRANCH.EQ.6) .AND. (DOBLC.LE.0)) IBUT = 1
C                                       interactive clip
         IF (BRANCH.EQ.7) THEN
C                                       define points
            IF (NCLIP.GE.MAXCLP) THEN
               MSGTXT = 'Too many points on interactive clip curve, '
     *            // 'try again'
               CALL MSGWRT (6)
               CALL YZERO (GRLI, JERR)
               IF (JERR.EQ.0) GRLIOK = .TRUE.
               NCLIP = 0
            ELSE IF ((NCLIP.GT.0) .AND. (TVX.LT.ICLIP(1,NCLIP))) THEN
               MSGTXT = 'The curve cannot go backwards in time,'
     *            // ' try again'
               CALL MSGWRT (6)
            ELSE
               NCLIP = NCLIP + 1
               ICLIP(1,NCLIP) = TVX
               ICLIP(2,NCLIP) = TVY
               ICLIP(3,NCLIP) = RPOS(1)
               ICLIP(4,NCLIP) = RPOS(2)
               IF (NCLIP.GE.2) THEN
                  IXP(1) = IROUND (ICLIP(3,NCLIP-1))
                  IXP(2) = IROUND (ICLIP(3,NCLIP))
                  IYP(1) = IROUND (ICLIP(4,NCLIP-1))
                  IYP(2) = IROUND (ICLIP(4,NCLIP))
                  CALL IMVECT (GRLI, 2, IXP, IYP, IMVONN, IRET)
                  ROUTIN = 'IMVECT'
                  GRLIOK = .FALSE.
                  IF (IRET.NE.0) GO TO 800
                  IF (IBUT.GE.4) GO TO 205
                  END IF
               END IF
            GO TO 100
C                                       switch BLC and TRC: clip by rms
         ELSE IF ((IBUT.EQ.1) .AND. (BRANCH.GE.4) .AND. (BRANCH.LE.6))
     *      THEN
            IF (DOBLC.LE.0) THEN
               DOBLC = 1
               ROUTIN = 'IMVECT'
               CALL IMVECT (GRLI, 2, IXP, IYP, IMVOFF, IRET)
               IF (IRET.NE.0) GO TO 800
               IXP(2) = IXP(1) + 10
               CALL IMVECT (GRLI, 2, IXP, IYP, IMVONN, IRET)
               IF (IRET.NE.0) GO TO 800
               END IF
            DOBLC = 3 - DOBLC
            RPOS(1) = IXP(DOBLC)
            RPOS(2) = IYP(DOBLC)
            CALL YCURSE ('ONNN', F, T, RPOS, QUAD, LBUT, IRET)
            IF (IRET.NE.0) THEN
               ROUTIN = 'YCURSE'
               IF (IRET.NE.2) GO TO 800
               CALL YCURSE ('ONNN', F, F, RPOS, QUAD, LBUT, IRET)
               IF (IRET.NE.0) GO TO 800
               END IF
            GO TO 100
C                                       switch BLC/TRC in box mode
      ELSE IF ((IBUT.EQ.1) .AND. (DOBOX)) THEN
         IF (DOBLC.LE.0) THEN
            DOBLC = 1
            CALL IMVECT (GRLI, 3, IXP, IYP, IMVOFF, IRET)
            IF (IRET.NE.0) GO TO 800
            IXP(1) = IXP(2)
            IXP(3) = IXP(2) + 10
            IXP(4) = IXP(3)
            IXP(5) = IXP(2)
            IF (BRANCH.NE.2) THEN
               IYP(4) = IYP(2) + 10
            ELSE
               IYP(4) = TVWIN(4)
               IYP(2) = TVWIN(2)
               END IF
            IYP(1) = IYP(4)
            IYP(5) = IYP(1)
            IYP(3) = IYP(2)
            CALL IMVECT (GRLI, 5, IXP, IYP, IMVONN, IRET)
            IF (IRET.NE.0) GO TO 800
            GRLIOK = .FALSE.
            END IF
         DOBLC = 3 - DOBLC
         I = 2*DOBLC
         RPOS(1) = IXP(I)
         RPOS(2) = IYP(I)
         IF (BRANCH.EQ.2) RPOS(2) = (IYP(3) + IYP(4)) / 2.0
         CALL YCURSE ('ONNN', F, T, RPOS, QUAD, LBUT, IRET)
         IF (IRET.NE.0) THEN
            ROUTIN = 'YCURSE'
            IF (IRET.NE.2) GO TO 800
            CALL YCURSE ('ONNN', F, F, RPOS, QUAD, LBUT, IRET)
            IF (IRET.NE.0) GO TO 800
            END IF
         GO TO 100
         END IF
C                                       Do some editing
C                                       erase graphics first
      I = 2
      IF (DOBOX) I = 5
      CALL IMVECT (GRLI, I, IXP, IYP, IMVOFF, IRET)
      ROUTIN = 'IMVECT'
      IF (IRET.NE.0) GO TO 800
C                                       rms editing
      IF (BRANCH.EQ.6) THEN
         CALL RMSEDT (TTY, GRLI, IXP, IYP, FLUXB, FLUXA, IRET)
         IF (IRET.GT.0) GO TO 800
         IF (IRET.EQ.-1) GO TO 850
         IF (IRET.EQ.-2) GO TO 910
         IF (IRET.EQ.-3) THEN
            IRET = 0
            DOBLC = 0
            GO TO 100
            END IF
         BOX1 = .FALSE.
         CALL COPY (4, IXP, TVRMS(1,1))
         CALL COPY (4, IYP, TVRMS(1,2))
         END IF
C                                       clear data from TV
C                                       flag time
      IF (BRANCH.EQ.1) THEN
         IBLC(1) = IXP(1)
         ITRC(1) = IXP(1)
         IBLC(2) = TVWIN(2)
         ITRC(2) = TVWIN(4)
C                                       areas
      ELSE IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.3)) THEN
         IBLC(1) = IXP(2)
         ITRC(1) = IXP(4)
         IBLC(2) = IYP(2)
         ITRC(2) = IYP(4)
C                                       clip above
      ELSE IF (BRANCH.EQ.4) THEN
         IBLC(1) = IXP(1)
         ITRC(1) = IXP(2)
         IBLC(2) = IYP(1)
         ITRC(2) = TVWIN(4)
C                                       clip below
      ELSE IF (BRANCH.EQ.5) THEN
         IBLC(1) = IXP(1)
         ITRC(1) = IXP(2)
         IBLC(2) = TVWIN(2)
         ITRC(2) = IYP(1)
C                                       rms clip: below
      ELSE IF (BRANCH.EQ.6) THEN
         IBLC(1) = TVRMS(1,1)
         ITRC(1) = TVRMS(2,1)
         IBLC(2) = TVWIN(2)
         ITRC(2) = TVRMS(2,2)
         BOX1 = T
         END IF
      IF (IBLC(1).GT.ITRC(1)) THEN
         I = IBLC(1)
         IBLC(1) = ITRC(1)
         ITRC(1) = I
         END IF
      IF (IBLC(2).GT.ITRC(2)) THEN
         I = IBLC(2)
         IBLC(2) = ITRC(2)
         ITRC(2) = I
         END IF
C                                       Edit box
 205  IF (BRANCH.NE.7) THEN
C                                       Translate corners
         RPOS(1) = IBLC(1)
         RPOS(2) = IBLC(2)
         CALL IMA2MP (RPOS, CORN)
         CALL CORNER (CATBLK, IIWIN, CORN)
         IX1 = CORN(1) + 0.49
         IY1 = CORN(2) + 0.49
         RPOS(1) = ITRC(1)
         RPOS(2) = ITRC(2)
         CALL IMA2MP (RPOS, CORN)
         CALL CORNER (CATBLK, IIWIN, CORN)
         IX2 = CORN(1) + 0.49
         IY2 = CORN(2) + 0.49
         IF ((BRANCH.EQ.1) .OR. (BRANCH.EQ.2) .OR. (BRANCH.EQ.4)) THEN
            FLUXA = MAXDY
         ELSE IF (BRANCH.NE.6) THEN
            FLUXA = MINY + (IY2 - 1.0) / SCALY
            END IF
         IF ((BRANCH.EQ.1) .OR. (BRANCH.EQ.2) .OR. (BRANCH.EQ.5)) THEN
            FLUXB = MINDY
         ELSE IF (BRANCH.NE.6) THEN
            FLUXB = MINY + (IY1 - 1.0) / SCALY
            END IF
         FCLIPR(1) = FLUXB
         FCLIPR(2) = FLUXA
         IF (BRANCH.GE.4) THEN
            IF (BRANCH.EQ.4) THEN
               WRITE (MSGTXT,1290) FLUXB
            ELSE IF (BRANCH.EQ.5) THEN
               WRITE (MSGTXT,1300) FLUXA
            ELSE
               IF (BOX1) THEN
                  WRITE (MSGTXT,1300) FLUXB
                  FCLIPR(1) = MINDY
                  FCLIPR(2) = FLUXB
               ELSE
                  WRITE (MSGTXT,1290) FLUXA
                  FCLIPR(1) = FLUXA
                  FCLIPR(2) = MAXDY
                  END IF
               END IF
            CALL MSGWRT (4)
            END IF
         IF (DOSTOK (ILSTOK, STKFLG, PLSTOK)) THEN
            ROUTIN = 'IMVECT'
            CALL YHOLD ('ONNN', I)
            DO 220 I = IX1,IX2
               IF ((MFLAGD(I).EQ.0) .AND. (YPIX(I).GT.0) .AND.
     *            (YVAL(I,PLTYPE).GE.FCLIPR(1)) .AND.
     *            (YVAL(I,PLTYPE).LE.FCLIPR(2))) THEN
                  CORN(1) = I
                  CORN(2) = YPIX(I)
                  CALL MP2IMA (CORN, RPOS)
                  JXP = IROUND (RPOS(1))
                  JYP(1) = YPIXR(1,I)
                  JYP(2) = YPIXR(2,I)
                  CALL DADRAW (GRDA, SCALX, JXP, JYP, IMVOFF, IRET)
                  IF (IRET.NE.0) GO TO 800
                  END IF
 220           CONTINUE
            CALL YHOLD ('OFFF', I)
            END IF
C                                        Do interactive clip
      ELSE IF (BRANCH.EQ.7) THEN
         CALL IEDIT (ICLIP, NCLIP, TTY, FLUXCL, BELOW, IXOFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 900
            END IF
C                                       Delete the vectors
         IBLC(2) = IROUND (ICLIP(3,1))
         ITRC(2) = IROUND (ICLIP(3,NCLIP))
         MCLIP = NCLIP - 1
         ROUTIN = 'IMVECT'
         DO 250 ID = 1,MCLIP
            IXP(1) = IROUND (ICLIP(3,ID))
            IXP(2) = IROUND (ICLIP(3,ID+1))
            IYP(1) = IROUND (ICLIP(4,ID))
            IYP(2) = IROUND (ICLIP(4,ID+1))
            CALL IMVECT (GRLI, 2, IXP, IYP, IMVOFF, IRET)
            IF (IRET.NE.0) GO TO 800
            IBLC(2) = MIN (IBLC(2), IYP(1), IYP(2))
            ITRC(2) = MAX (ITRC(2), IYP(1), IYP(2))
 250        CONTINUE
         GRLIOK = .TRUE.
         IBLC(1) = IROUND (ICLIP(3,1))
         ITRC(1) = IROUND (ICLIP(3,NCLIP))
         END IF
C                                       Now edit data in arrays
      DOBLC = 0
      RPOS(1) = PPOS(1)
      RPOS(2) = PPOS(2)
C                                       Write flagging array and
C                                       FC table
 300  DOIT = DOSTOK (ILSTOK, STKFLG, PLSTOK)
      CALL FLGWRT (BRANCH, IBLC, ITRC, BELOW, IXOFF, FLUXCL, DOIT,
     *   NFLAGD, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
C                                       If flagging based on rms
C                                       then flag second area
      IF ((BRANCH.EQ.6) .AND. (BOX1)) THEN
         PFLAGD(1) = NFLAGD(1)
         PFLAGD(2) = NFLAGD(2)
         IBLC(1) = TVRMS(3,1)
         ITRC(1) = TVRMS(4,1)
         IBLC(2) = TVRMS(3,2)
         ITRC(2) = TVWIN(4)
         BOX1 = F
         GO TO 205
         END IF
C                                       reinit the TV
      NFLAGD(1) = NFLAGD(1) + PFLAGD(1)
      IF (NFLAGD(1).GT.0) THEN
         IF (DOIT) THEN
            WRITE (MSGTXT,1330) NFLAGD(1)
         ELSE
            WRITE (MSGTXT,1331) NFLAGD(1)
            CALL MSGWRT (3)
            MSGTXT = 'EXCEPT Stokes flag prevented flags from applying'
            END IF
         CALL MSGWRT (3)
      ELSE
         MSGTXT = 'WARNING: No previously unflagged samples were ' //
     *      'flagged ********'
         CALL MSGWRT (6)
         END IF
      NFLAGD(2) = NFLAGD(2) + PFLAGD(2)
      IF (NFLAGD(2).GT.0) THEN
         WRITE (MSGTXT,1335) NFLAGD(2)
         CALL MSGWRT (3)
      ELSE
         MSGTXT = 'WARNING: No records written to the FC file ' //
     *      ' ********'
         CALL MSGWRT (6)
         END IF
C                                       leave abruptly non-interactive
      IF (BRANCH.EQ.8) GO TO 999
      CALL FILL (5, 1, IXP)
      CALL FILL (5, 1, IYP)
      PPOS(1) = 0.
      PPOS(2) = 0.
      CALL YCURSE ('READ', F, F, RPOS, QUAD, LBUT, IRET)
      ROUTIN = 'YCURSE'
      IF (IRET.NE.0) GO TO 800
      IF (IBUT.GE.4) GO TO 910
      GO TO 90
C                                       TV error
 800  WRITE (MSGTXT,1800) IRET, ROUTIN
      GO TO 900
C                                       TTY error
 850  IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1850) IRET
      ELSE IF (IRET.LT.0) THEN
         MSGTXT = 'Return to menu, non-numeric input'
         IRET = 0
         END IF
C                                       Error message first
 900  CALL MSGWRT (8)
C                                       erase graphics first
 910  CALL YHOLD ('ONNN', I)
      IF (.NOT.GRLIOK) THEN
         IF (BRANCH.NE.7) THEN
            I = 2
            IF (DOBOX) I = 5
            CALL IMVECT (GRLI, I, IXP, IYP, IMVOFF, JERR)
         ELSE
            CALL YZERO (GRLI, JERR)
            END IF
         IF (JERR.EQ.0) GRLIOK = .TRUE.
         END IF
      IY = IY0
      NROW = 3
      IF (MAXSOU.GT.0) NROW = 4
      JERR = 0
      DO 915 I = 1,NROW
         IF (JERR.EQ.0) CALL IMCHAR (GRCV, IX0, IY, 0, 0, BLNAME,
     *      SCRTCH, JERR)
         IY = IY - 2*MEDGE - CSIZTV(2)
 915     CONTINUE
      IF (JERR.EQ.0) GRCVOK = .TRUE.
      CALL YHOLD ('OFFF', I)
      GO TO 999
C                                       error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('IBFLAG: UNABLE TO READ IMAGE HEADER, ERROR',I5)
 1020 FORMAT ('IBFLAG: ERROR ',I3,' APPLYING INTERACTIVE CLIP')
 1040 FORMAT ('IBFLAG: ERROR ',I3,' FROM FLGWRT - FC TABLE ERROR')
 1290 FORMAT ('Flagging data with flux > ',F10.5,' Jy')
 1300 FORMAT ('Flagging data with flux < ',F10.5,' Jy')
 1330 FORMAT ('Flagged',I5,' samples in the displayed data')
 1331 FORMAT ('Would have flagged',I5,' samples in the displayed data')
 1335 FORMAT ('Wrote  ',I5,' records to the FC file')
 1800 FORMAT ('TV ERROR ',I6,' IN ',A)
 1850 FORMAT ('TERMINAL ERROR',I5)
      END
      SUBROUTINE IBFUNF (BRANCH, TTY, SNAMES, IRET)
C-----------------------------------------------------------------------
C   does unflag-related operations on the displayed data, deleting
C   commands in the FC (flag control) file.
C   Inputs:
C      BRANCH   I      =1 => list flag commands
C                      =2 => undo flag commands
C      TTY      I(2)   LUN, IND of open terminal to talk to user
C   Output:
C      IRET     I      Error return: > 0 => quit
C                         -1 => nothing really done
C-----------------------------------------------------------------------
      INTEGER   BRANCH, TTY(2), IRET
      CHARACTER SNAMES(*)*16
C
      INTEGER   NCC, I, J, IERR, J1, FCNLIM(2), FCNCNT, FCNFLG(50), I1,
     *   I2, K, NCF, J2, NCFLAG, NCFC, NCFS
      REAL      T1, T2
      CHARACTER OP*8, MSGBUF*72, SNAME*16
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.2)) GO TO 999
C                                       open the FC file
      CALL IBFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG, FCNUMB,
     *   NNFLAG, FCBUF, IRET)
      IF (IRET.NE.0) GO TO 999
      NCFLAG = FCNUMB + 1
      IF ((FCNUMB.LE.0) .OR. (NNFLAG.LE.0)) THEN
         MSGTXT = 'No entries in the FC table to LIST, UNDO, or REDO'
         CALL MSGWRT (6)
         IRET = -1
         GO TO 900
         END IF
C                                       list
      IF (BRANCH.EQ.1) THEN
         T1 = 999.0
         T2 = -999.0
         IF (FCNUMB.LE.1) THEN
            FCNLIM(1) = 1
            FCNLIM(2) = 1
         ELSE
            WRITE (MSGBUF,1100) FCNUMB
            CALL INQINT (TTY, MSGBUF, 2, FCNLIM, IRET)
            IF (IRET.NE.0) GO TO 850
            FCNLIM(1) = MAX (1, MIN (FCNLIM(1), FCNUMB))
            IF (FCNLIM(2).LT.FCNLIM(1)) FCNLIM(2) = FCNUMB
            FCNLIM(2) = MAX (1, MIN (FCNLIM(2), FCNUMB))
            END IF
         NCF = 0
         NCFC = 0
         NCFS = 0
         SNAME = ' '
         DO 150 I = 1,NNFLAG
            CALL TABIO ('READ', 0, I, FCTIME, FCBUF, IRET)
            CALL H2CHR (8, 1, FCOPER, OP)
            IF (IRET.LT.0) GO TO 150
            IF (IRET.NE.0) GO TO 900
            IF ((FCNUMB.LT.FCNLIM(1)) .OR. (FCNUMB.GT.FCNLIM(2)))
     *         GO TO 150
C                                       same flag and source
            IF ((FCNUMB.EQ.NCF).AND. (FCSOUR.EQ.NCFS)) THEN
               NCFC = NCFC + 1
               T1 = MIN (T1, STOP, MAX (START, FCTIME(1)))
               T2 = MAX (T2, START, MIN (STOP, FCTIME(2)))
C                                       new source
            ELSE IF (FCNUMB.EQ.NCF) THEN
               IF (NCFC.GT.0) CALL IBFCLI (NCFC, 3, SNAME, T1, T2)
               NCFC = 1
               NCFS = FCSOUR
               SNAME = ' '
               IF (NCFS.GT.0) SNAME = SNAMES(NCFS)
               T1 = MIN (STOP, MAX (START, FCTIME(1)))
               T2 = MAX (START, MIN (STOP, FCTIME(2)))
C                                       new flag kind
            ELSE
               IF (NCFC.GT.0) CALL IBFCLI (NCFC, 3, SNAME, T1, T2)
               NCFC = 1
               NCF = FCNUMB
               NCFS = FCSOUR
               SNAME = ' '
               IF (NCFS.GT.0) SNAME = SNAMES(NCFS)
               CALL IBFCLI (0, 3, SNAME, T1, T2)
               END IF
 150        CONTINUE
C                                       give number flags
         IF (NCFC.GT.0) CALL IBFCLI (NCFC, 3, SNAME, T1, T2)
C                                       UNDO 1 or more flags
      ELSE IF (BRANCH.EQ.2) THEN
         FCNLIM(1) = FCNUMB
         FCNLIM(2) = 1
         IF (FCNUMB.LE.1) THEN
            I = 2
            FCNFLG(1) = 1
         ELSE
            DO 210 I = 1,50
               WRITE (MSGBUF,1200) I, FCNUMB
 205           CALL INQINT (TTY, MSGBUF, 1, FCNFLG(I), IRET)
               IF (IRET.NE.0) GO TO 850
               IF (FCNFLG(I).GT.FCNUMB) GO TO 205
               IF (FCNFLG(I).LE.0) GO TO 215
               FCNLIM(1) = MIN (FCNLIM(1), FCNFLG(I))
               FCNLIM(2) = MAX (FCNLIM(2), FCNFLG(I))
 210           CONTINUE
            I = 51
            END IF
 215     FCNCNT = I - 1
         IF (FCNCNT.LE.0) GO TO 900
C                                       flag them in FC file
         NCC = 0
         I1 = 0
         J1 = 0
         NCF = 0
         NCFC = 0
         NCFS = 0
         SNAME = ' '
         DO 230 I = 1,NNFLAG
            CALL TABIO ('READ', 0, I, FCTIME, FCBUF, IRET)
            IF (IRET.LT.0) GO TO 230
            IF (IRET.NE.0) GO TO 900
            I2 = I1
            I1 = I
            J2 = J1
            J1 = FCNUMB
            IF ((FCNUMB.LT.FCNLIM(1)) .OR. (FCNUMB.GT.FCNLIM(2)))
     *         GO TO 230
               IF (FCNLIM(1).EQ.FCNLIM(2)) GO TO 225
                  DO 220 J = 1,FCNCNT
                     IF (FCNUMB.EQ.FCNFLG(J)) GO TO 225
 220                 CONTINUE
                  GO TO 230
C                                       flag the line
 225           NCC = NCC + 1
               CALL TABIO ('FLAG', 0, I, FCTIME, FCBUF, IRET)
               IF (IRET.NE.0) GO TO 900
               I1 = I2
               J1 = J2
C                                       same flag and source
               IF ((FCNUMB.EQ.NCF).AND. (FCSOUR.EQ.NCFS)) THEN
                  NCFC = NCFC + 1
                  T1 = MIN (T1, STOP, MAX (START, FCTIME(1)))
                  T2 = MAX (T2, START, MIN (STOP, FCTIME(2)))
C                                       new source
               ELSE IF (FCNUMB.EQ.NCF) THEN
                  IF (NCFC.GT.0) CALL IBFCLI (NCFC, 2, SNAME, T1, T2)
                  NCFC = 1
                  NCFS = FCSOUR
                  SNAME = ' '
                  IF (NCFS.GT.0) SNAME = SNAMES(NCFS)
                  T1 = MIN (STOP, MAX (START, FCTIME(1)))
                  T2 = MAX (START, MIN (STOP, FCTIME(2)))
C                                       new flag kind
               ELSE
                  IF (NCFC.GT.0) CALL IBFCLI (NCFC, 2, SNAME, T1, T2)
                  NCFC = 1
                  NCF = FCNUMB
                  NCFS = FCSOUR
                  SNAME = ' '
                  IF (NCFS.GT.0) SNAME = SNAMES(NCFS)
                  MSGTXT = '******** Undoing :'
                  CALL MSGWRT (2)
                  CALL IBFCLI (0, 2, SNAME, T1, T2)
                  END IF
 230        CONTINUE
         IF (NCFC.GT.0) CALL IBFCLI (NCFC, 2, SNAME, T1, T2)
         IF (NCC.LE.0) THEN
            MSGTXT = 'No lines deleted in the FC file, return to menu'
            CALL MSGWRT (6)
            IRET = -1
            GO TO 900
         ELSE
            WRITE (MSGTXT,1230) NCC
            CALL MSGWRT (2)
            END IF
C                                       reduce number of records
         IF (I1.LT.NNFLAG) THEN
            I2 = NNFLAG - I1
            WRITE (MSGTXT,1231) I2, I1
            CALL MSGWRT (3)
            FCBUF(5) = I1
            NNFLAG = I1
            J2 = NCFLAG
            NCFLAG = J1 + 1
            WRITE (MSGTXT,1233) J2, NCFLAG
            CALL MSGWRT (3)
            END IF
C                                       remove flagging info
         NCC = 0
         DO 245 I = 1,NV
            I2 = MFLAGD(I)
C                                        Make sure don't undo
C                                        initial flags
            IF (I2.LT.0) GO TO 245
            IF ((I2.LT.FCNLIM(1)) .OR. (I2.GT.FCNLIM(2))) GO TO 245
            IF (FCNLIM(1).EQ.FCNLIM(2)) GO TO 240
               DO 235 K = 1,FCNCNT
                  IF (I2.EQ.FCNFLG(K)) GO TO 240
 235              CONTINUE
               GO TO 245
 240        NCC = NCC + 1
            MFLAGD(I) = 0
 245        CONTINUE
         IF (NCC.LE.0) THEN
            MSGTXT = 'No pixels restored in displayed data,' //
     *         ' return to menu'
            CALL MSGWRT (6)
            IRET = -1
         ELSE
            WRITE (MSGTXT,1290) NCC
            CALL MSGWRT (2)
            MSGTXT = 'Later flags may now flag some of these, of course'
            CALL MSGWRT (1)
            END IF
         END IF
      GO TO 900
C                                       TTY error
 850  IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1850) IRET
         CALL MSGWRT (8)
      ELSE IF (IRET.LT.0) THEN
         MSGTXT = 'FORMAT ERROR: return to menu'
         CALL MSGWRT (6)
         END IF
C                                       close FC file
 900  CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IERR)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Enter range of flag commands to display (2 I) from 1 to',
     *   I4)
 1200 FORMAT ('Enter the',I3,'th flag number to delete =<',I4,
     *   ' (one I), 0 ends list')
 1230 FORMAT ('Deleted',I6,' lines in FC table')
 1231 FORMAT ('Dropping',I5,' lines from end of FC table, size now',
     *   I6,' lines')
 1233 FORMAT ('Next flag number changed from',I5,' to',I5)
 1290 FORMAT ('Restored ',I10,' samples in the displayed data')
 1850 FORMAT ('TERMINAL ERROR',I5)
      END
      SUBROUTINE IBFOAD (IRET)
C-----------------------------------------------------------------------
C   Routine called by IBLED
C   loads the image  converting to currently desired type of display
C   Output:
C      IRET    I       error code: > 0 => TV error, > 100 IO error
C                         = -99 => no valid data found (warning)
C   In/Out: INCLUDE 'IBLED.INC' - many parameters used
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER CUNITS(6)*8, XUNITS*8, ROUTIN*6
      HOLLERITH CATH(256)
      INTEGER   IR, IJ, DAWIN2(4), MASK, TVAWIN(4), ZAND
      REAL      CATR(256), BTIME, ETIME, SCALY2, TEMP
      DOUBLE PRECISION CATD(128)
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA CUNITS /'FLUX', 'DEGREES ','DECORREL','FL RATIO','PHS DIFF',
     *   'DECRATIO'/
      DATA XUNITS /'VIS NUMB'/
C-----------------------------------------------------------------------
C                                       build catalog header
      CALL COPY (256, CATIMG, CATBLK)
      CATBLK(KIDIM) = 1
      CATBLK(KINAX) = NV
      CATBLK(KINAX+1) = TVWIN(4) - TVWIN(2) + 1
      CALL FILL (5, 1, CATBLK(KINAX+2))
      CATR(KRCRP) = 1.0
      CATD(KDCRV) = 1.0
      CATR(KRCIC) = 1.0
C                                       center the image
      DAWIN(2) = 1
      DAWIN(4) = TVWIN(4) - TVWIN(2) + 1
      IF (DAWIN(1).GE.DAWIN(3)) THEN
         DAWIN(1) = 1
         DAWIN(3) = MAX (2, NV)
      ELSE IF (DAWIN(1).LT.1) THEN
         DAWIN(3) = MIN (DAWIN(3) + 1 - DAWIN(1), NV)
         DAWIN(1) = 1
      ELSE IF (DAWIN(3).GT.NV) THEN
         DAWIN(1) = MAX (DAWIN(1) + NV - DAWIN(3), 1)
         DAWIN(3) = NV
      ELSE
         DAWIN(3) = MIN (NV, DAWIN(3))
         DAWIN(1) = MAX (1, DAWIN(1))
         END IF
      CALL COPY (4, TVWIN, TVAWIN)
      SCALX = (TVAWIN(3) - TVAWIN(1)) / MAX (1, DAWIN(3) - DAWIN(1))
      SCALX = MAX (1, MIN (20, SCALX))
      IJ = SCALX * (DAWIN(3) - DAWIN(1)) - (TVAWIN(3) - TVAWIN(1))
      IF (IJ.LT.0) THEN
         IF (NV-DAWIN(3).LT.DAWIN(1)-1) THEN
            DAWIN(3) = MIN (DAWIN(3) - IJ/(2*SCALX), NV)
            DAWIN(1) = DAWIN(3) - (TVAWIN(3) - TVAWIN(1)) / SCALX
            IF (DAWIN(1).LT.1) THEN
               DAWIN(3) = MIN (NV, DAWIN(3) + 1 - DAWIN(1))
               DAWIN(1) = 1
               END IF
         ELSE
            DAWIN(1) = MAX (DAWIN(1) + IJ/(2*SCALX), 1)
            DAWIN(3) = DAWIN(1) + (TVAWIN(3) - TVAWIN(1)) / SCALX
            IF (DAWIN(3).GT.NV) THEN
               DAWIN(1) = MAX (1, DAWIN(1) + NV - DAWIN(3))
               DAWIN(3) = NV
               END IF
            END IF
         IJ = SCALX * (DAWIN(3) - DAWIN(1)) - (TVAWIN(3) - TVAWIN(1))
         IF (IJ.LT.0) THEN
            TVAWIN(1) = TVAWIN(1) - IJ / 2
            TVAWIN(3) = TVAWIN(1) + (DAWIN(3) - DAWIN(1)) * SCALX
            END IF
      ELSE IF (IJ.GT.0) THEN
         DAWIN(3) = DAWIN(1) + (TVWIN(3) - TVWIN(1)) / SCALX
         END IF
      IF (DAWIN(3).LT.NV) THEN
         MSGTXT = 'Not all data in this frame, NEXT FRAME option' //
     *   ' displays more'
         CALL MSGWRT (2)
         END IF
      WRITE (MSGTXT,1010) DAWIN(1), DAWIN(3)
      CALL MSGWRT (2)
C                                       Set max/min
      IF (.NOT.DATAOK) THEN
         MAXY = -1.0E10
         MINY = 1.0E10
         DO 20 IR = 1,NV
            IF (MFLAGD(IR).EQ.0) THEN
               MAXY = MAX (MAXY, YVAL(IR,LTYPE))
               MINY = MIN (MINY, YVAL(IR,LTYPE))
               IF (WMODEL) THEN
                  MAXY = MAX (MAXY, YVALM(IR))
                  MINY = MIN (MINY, YVALM(IR))
                  END IF
               END IF
 20         CONTINUE
C                                       Set up scaling for y axis
         IF (MINY.EQ.1.0E10) THEN
            MINY = 0.0
            MAXY = 1.0
            END IF
         WRITE (MSGTXT,1020) MINY, MAXY
         CALL MSGWRT (2)
         IF ((MINY.GT.0.0) .AND. (MINY.LT.0.1*MAXY)) MINY = 0.0
         TEMP = MAXY - MINY
         MAXY = MAXY + 0.03 * TEMP
         MINY = MINY - 0.03 * TEMP
         MINDY = MINY
         MAXDY = MAXY
         END IF
C                                       Obey user supplied pixel
C                                       range
      IF ((.NOT.DATAOK) .OR. (.NOT.SCALOK)) THEN
         MINY = MINDY
         MAXY = MAXDY
         IF (PIXRNG(1,LTYPE).LT.PIXRNG(2,LTYPE)) THEN
            MINY = PIXRNG(1,LTYPE)
            MAXY = PIXRNG(2,LTYPE)
            TEMP = MAXY - MINY
            MAXY = MAXY + 0.03 * TEMP
            MINY = MINY - 0.03 * TEMP
            END IF
         IF (MINY.GE.MAXY) THEN
            MSGTXT = 'IBFOAD: NO VALID DATA FOR THIS IF - POLARIZATION'
     *         // ' - BASELINE'
            IF (LTYPE.EQ.3) MSGTXT = 'IBFOAD: NO VALID DATA FOUND - '
     *         // 'DID YOU DO ANY AVERAGING?'
            CALL MSGWRT (6)
            IF (MINY.GT.MAXY) MINY = 0.
            MINY = MINY - (TVAWIN(4) - TVAWIN(2)) / 2.0
            MAXY = MINY + (TVAWIN(4) - TVAWIN(2))
            END IF
         SCALY = (TVAWIN(4) - TVAWIN(2)) / (MAXY - MINY)
         SCALY2 = (TVWIN2(4) - TVWIN2(2)) / (MAXY - MINY)
C                                       Fill up the YPIX array
         DO 300 IR = 1,NV
            IF ((YVAL(IR,LTYPE).EQ.FBLANK) .OR. (MFLAGD(IR).LT.0) .OR.
     *         (YVAL(IR,LTYPE).GT.MAXY) .OR. (YVAL(IR,LTYPE).LT.MINY))
     *         THEN
               YPIX(IR) = 0
               YPIX2(IR) = 0
               YPIXM(IR) = 0
            ELSE
               YPIX(IR) = (YVAL(IR,LTYPE) - MINY) * SCALY + 1.5
               YPIX2(IR) = (YVAL(IR,LTYPE) - MINY) * SCALY2 + 1.5
               YPIXM(IR) = (YVALM(IR) - MINY) * SCALY + 1.5
               END IF
 300        CONTINUE
         DATAOK = .TRUE.
         SCALOK = .TRUE.
         END IF
C                                       Finish image catalog header
      CATR(KRCRP+1) = 1.0
      CATD(KDCRV+1) = MINY
      CATR(KRCIC+1) = SCALY
      CATR(IRRAN) = 0.0
      CATR(IRRAN+1) = 10.0
      CATBLK(IIVOL) = 0
      CATBLK(IICNO) = 0
      CALL CHR2H (2, 'LI', 1, CATH(IITRA))
      IJ = LTYPE
      IF ((LIF(2).GT.0) .AND. (LIF(2).NE.LIF(1))) IJ = IJ + 3
      CALL CHR2H (8, CUNITS(IJ), 1, CATH(KHBUN))
      CALL CHR2H (8, CUNITS(IJ), 1, CATH(KHCTP+2))
      CALL CHR2H (8, XUNITS, 1, CATH(KHCTP+2))
C                                       On, clear TVchans
      CALL YHOLD ('ONNN', IJ)
      MASK = 2 ** (GRDA - 1)
      MASK = ZAND (MASK, TVLIMG(1))
      IF (MASK.EQ.0) THEN
         ROUTIN = 'YSLECT'
         CALL YSLECT ('ONNN', GRDA, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      CALL YZERO (GRDA, IRET)
      ROUTIN = 'YZERO'
      IF (IRET.NE.0) GO TO 990
      CALL YCINIT (GRDA, SCRTCH)
      IF (GRDF.EQ.GRDA) GRDAOK = .FALSE.
      IF ((GRDA.NE.GRDF) .AND. (SHOWVS)) THEN
         MASK = 2 ** (GRDF - 1)
         MASK = ZAND (MASK, TVLIMG(1))
         IF (MASK.EQ.0) THEN
            ROUTIN = 'YSLECT'
            CALL YSLECT ('ONNN', GRDF, 0, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 990
            END IF
         IF (.NOT.GRDFOK) THEN
            ROUTIN = 'YZERO'
            CALL YZERO (GRDF, IRET)
            IF (IRET.NE.0) GO TO 990
            CALL YCINIT (GRDF, SCRTCH)
            END IF
         END IF
C                                       Load image on TV.
      CALL IBLOAD (TVAWIN, IRET)
      ROUTIN = 'IBLOAD'
      IF (IRET.NE.0) GO TO 990
      CALL YHOLD ('OFFF', IJ)
      CALL YHOLD ('ONNN', IJ)
      BTIME = MAX (TIMES(DAWIN(1)), START)
      ETIME = MIN (TIMES(DAWIN(3)), STOP)
      ROUTIN = 'VISLAB'
      CALL VISLAB (GRDA, TVWIN, ANTENS(1), ANTENS(2), LTYPE, PSTYPE,
     *   SLIF, SLCHAN, SCALX, MINY, MAXY, BTIME, ETIME, STNS(ANTENS(1)),
     *   STNS(ANTENS(2)), TIMES, IMVONN, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (SHOWVS) THEN
         IF (.NOT.GRDFOK) THEN
            DAWIN2(1) = 1
            DAWIN2(2) = 1
            DAWIN2(3) = NV
            DAWIN2(4) = TVWIN2(4) - TVWIN2(2) + 1
            CATR(KRCIC+1) = SCALY2
            ROUTIN = 'IBLOD2'
            CALL IBLOD2 (DAWIN2, SCALY2, IRET)
            IF (IRET.NE.0) GO TO 990
            ROUTIN = 'VISLB2'
            END IF
         CALL VISLB2 (GRDA, TVWIN2, DAWIN(1), DAWIN(3), NV, IMVONN,
     *      IRET)
         END IF
      IF (IRET.EQ.0) GO TO 995
C
 990  WRITE (MSGTXT,1990) IRET, ROUTIN
      CALL MSGWRT (8)
C
 995  CALL YHOLD ('OFFF', IJ)
C
 999  RETURN
C----------------------------------------------------------------------
 1010 FORMAT ('Displaying pixels ',I6,' to ',I6)
 1020 FORMAT ('Data range ',2(1PE14.6))
 1990 FORMAT ('IBFOAD: ERROR',I5,' FROM ',A)
      END
      SUBROUTINE IBLOAD (TVAWIN, IRET)
C-----------------------------------------------------------------------
C   Subroutine to load a array determined in IBLED to one TV memory
C   plane.  IBLOAD puts TV and map windows in the image header and
C   writes it in the image catalog.  It assumes that the other parts of
C   the image header are already filled in (and uses them) and that the
C   windows are all computed.
C      TVAWIN   I(4)   TV corners of data
C   Input from common:
C      TVWIN    I(4)   TV corners: full area
C      DAWIN    I(4)   Data array corners
C   Outputs
C      IRET     I      Error code: 0 => ok
C                                  1 => TV errors
C                                  2 => YCWRIT errors
C                                  3 => IMVECT errors
C   Commons: /MAPHDR/ CATBLK  image header
C-----------------------------------------------------------------------
      INTEGER   TVAWIN(4), IRET
C
      LOGICAL   WFND, WEND
      INTEGER   JERR, IX, ITEMP, IX1, IX2, IXP, IXOFF, IYOFF, NMODEL,
     *   I, ISTRT, IEND, NBAD, N
      REAL      RADJ
      LOGICAL   DOER
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       fix up img cat block
      CALL COPY (4, DAWIN, CATBLK(IIWIN))
      CALL COPY (4, TVAWIN, CATBLK(IICOR))
C                                       write img cat block
      IRET = 2
      CALL YCWRIT (GRDA, TVWIN, CATBLK, SCRTCH, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       set up TV writing
      IX1 = DAWIN(1)
      IX2 = DAWIN(3)
      IRET = 3
C                                       Plot data
      IXOFF = TVAWIN(1) - IX1 * SCALX
      IYOFF = TVAWIN(2) - 1
      RADJ = 1.0
      ITEMP = 1
      DOER = (DOERRB) .AND. (LTYPE.LE.2) .AND. ((LIF(2).LE.0) .OR.
     *   (LIF(2).EQ.LIF(1)))
      DO 50 IX = IX1,IX2
         YPIXR(1,IX) = 0
         YPIXR(2,IX) = 0
         IXP = IX * SCALX + IXOFF
C                                       Fill model array
         XPIXMR(IX) = IXP
         YPIXMR(IX) = MAX (IYOFF + YPIXM(IX), TVAWIN(2))
         YPIXMR(IX) = MIN (IYOFF + YPIXM(IX), TVAWIN(4))
C
         IF ((MFLAGD(IX).EQ.0) .AND. (YPIX(IX).GT.0)) THEN
            IF (DOER) THEN
               IF (LTYPE.EQ.2) THEN
                  RADJ = DG2RAD * YVAL(IX,1)
                  IF (RADJ.EQ.0.0) RADJ = DG2RAD
                  END IF
               ITEMP = SCALY * ERRORB(IX) / (2. * RADJ) + 0.5
               END IF
            YPIXR(1,IX) = MAX (IYOFF + YPIX(IX) - ITEMP, TVAWIN(2))
            YPIXR(2,IX) = MIN (IYOFF + YPIX(IX) + ITEMP, TVAWIN(4))
            CALL DADRAW (GRDA, SCALX, IXP, YPIXR(1,IX), IMVONN, JERR)
            IF (JERR.NE.0) THEN
               WRITE (MSGTXT,1020) JERR
               CALL MSGWRT (8)
               GO TO 999
               END IF
            END IF
 50      CONTINUE
      GRDAOK = .TRUE.
C                                       Plot model in segments.
C                                       Necessary conditions are:
C                                       1) model CC file; 2) plot
C                                       model selected on menu;
C                                       3) amplitude plotting only.
      IF (WMODEL.AND.DOMODL.AND.(LTYPE.EQ.1)) THEN
C                                       No of model points
         NMODEL = IX2 - IX1 + 1
         I = IX1
C                                       While (NOT END) do:
100      IF (I.GT.IX2) GO TO 500
C                                       Find start of next section
            WFND = .FALSE.
125         IF ((I.GT.IX2).OR.WFND) GO TO 150
               WFND = (MFLAGD(I).GE.0)
               IF (.NOT.WFND) I = I + 1
               GO TO 125
150         WEND = (I.GT.IX2)
            IF (.NOT.WEND) THEN
C                                       Find end of next section
               ISTRT = I
               WFND = .FALSE.
               NBAD = 0
               I = ISTRT + 1
200            IF ((I.GT.IX2).OR.WFND) GO TO 250
C                                       End marked by >5 flagged points
                  IF (MFLAGD(I).GE.0) THEN
                     NBAD = 0
                  ELSE
                     NBAD = NBAD + 1
                     IF (NBAD.EQ.1) IEND = I - 1
                     WFND = (NBAD.GE.5)
                     END IF
                  IF (.NOT.WFND) I = I + 1
                  GO TO 200
250            WEND = (I.GT.IX2)
               IF (WEND) IEND = IX2
C                                       Plot if more than one point
               N = IEND - ISTRT + 1
               IF (N.GT.1) THEN
                  CALL IMVECT (GRDA, N, XPIXMR(ISTRT), YPIXMR(ISTRT),
     *               IMVONN, JERR)
                  IF (JERR.NE.0) THEN
                     WRITE (MSGTXT,1100) JERR
                     CALL MSGWRT (8)
                     GO TO 999
                     END IF
                  END IF
               I = IEND + 1
               END IF
C                                       Loop back for next segment
            GO TO 100
C                                       Endwhile
500      CONTINUE
         END IF
C
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('IBLOAD: IMVECT ERROR: ',I3,' DRAWING ERROR BARS')
 1100 FORMAT ('IBLOAD: IMVECT ERROR: ',I3,' PLOTTING MODEL')
      END
      SUBROUTINE IBLOD2 (DAWIN2, SCALY2, IRET)
C-----------------------------------------------------------------------
C   Subroutine to load a array determined in IBLED to one TV memory
C   plane.  IBLOD2 puts TV and map windows in the image header and
C   writes it in the image catalog.  It assumes that the other parts of
C   the image header are already filled in (and uses them) and that the
C   windows are all computed.
C   Inputs:
C      DAWIN2   I(4)   Data window
C      SCALY2   R      Display scale on 2nd window.
C   Input in common
C      TVWIN2   I(4)   TV corners: BLC x,y TRC x,y
C   Output
C      IRET     I      Error code: 0 => ok
C                                  1 => input errors
C   Commons: /MAPHDR/ CATBLK  image header
C-----------------------------------------------------------------------
      INTEGER   DAWIN2(4), IRET
      REAL      SCALY2
C
      INTEGER   JERR, IX, IXP(2), IYP(2), IYOFF
      REAL      SCALX2, RADJ, XOFF, YD, RDMULT
      LOGICAL   DOER
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       fix up img cat block
      CALL COPY (4, DAWIN2, CATBLK(IIWIN))
      CALL COPY (4, TVWIN2, CATBLK(IICOR))
C                                       write img cat block
      IF (GRDF.NE.GRDA) THEN
         IRET = 2
         CALL YCWRIT (GRDF, TVWIN, CATBLK, SCRTCH, JERR)
         IF (JERR.NE.0) GO TO 999
         END IF
      IRET = 3
C                                       Plot data
      SCALX2 = (TVWIN2(3) - TVWIN2(1)) / (NV - 1.0)
      XOFF = TVWIN2(1) + 0.5
      IYOFF = TVWIN2(2) - 1
      RADJ = SCALY2 / 2.0
      RDMULT = RADJ / DG2RAD
      YD = 0.5
      DOER = (DOERRB) .AND. (LTYPE.LE.2) .AND. ((LIF(2).LE.0) .OR.
     *   (LIF(2).EQ.LIF(1)))
      DO 20 IX = 1,NV
         IF ((MFLAGD(IX).EQ.0) .AND. (YPIX2(IX).GT.0)) THEN
            IXP(1) = SCALX2 * (IX - 1.0) + XOFF
            IXP(2) = IXP(1)
            IF (DOER) THEN
               IF (LTYPE.EQ.2) THEN
                  RADJ = YVAL(IX,1)
                  IF (RADJ.EQ.0.0) RADJ = 1.0
                  RADJ  = RDMULT / RADJ
                  END IF
               YD = ERRORB(IX) * RADJ
               END IF
            IYP(1) = IYOFF + YPIX2(IX) - YD
            IYP(2) = IYOFF + YPIX2(IX) + YD
            IF (IYP(2).EQ.IYP(1)) IYP(2) = IYP(2) + 1
            IYP(1) = MAX (IYOFF, IYP(1))
            IYP(2) = MIN (TVWIN2(4), IYP(2))
            CALL IMVECT (GRDF, 2, IXP, IYP, IMVONN, JERR)
            IF (JERR.NE.0) THEN
               WRITE (MSGTXT,1000) JERR
               GO TO 980
               END IF
            END IF
 20      CONTINUE
      IRET = 0
      GRDFOK = .TRUE.
      GO TO 999
C                                       write error message
 980  CALL MSGWRT (7)
C
 999   RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IBLOD2: IMVECT ERROR: ',I3,' DRAWING ERROR BARS')
      END
      SUBROUTINE VISLAB (TVCH, TVWIN, IA1, IA2, LTYPE, PSTYPE, SLIF,
     *   SLCHAN, SCALX, MINY, MAXY, START, STOP, STN1, STN2, TIMES,
     *   IMVONN, SCRTCH, IRET)
C-----------------------------------------------------------------------
C  Routine to draw the graphics vectors around the visibility
C  function on the TV screen and provide useful labelling.
C  Inputs:
C      TVCH     I      Graphics plane to use
C      TVWIN    I(4)   blc, trc corners of window on TV
C      IA1      I      First antenna of baseline being displayed.
C      IA2      I      Second antenna of baseline being displayed.
C      LTYPE    I      Type of data displayed, 1 = Ampl, 2 = Phase
C      PSTYPE   C*2    Stokes displayed
C      SLIF     I(2)   IF's displayed: SLIF(2) < 0 => IF ratio
C      SLCHAN   I(2)   Channels displayed
C      MINY     R      Min. y value on TV
C      MAXY     R      Max. y value on TV
C      START    R      Start time of data on TV
C      STOP     R      Stop time of data on TV
C      STN1     C*8    Station name of antenna 1
C      STN2     C*8    Station name of antenna 2
C     TIMES    R(*)   Data sample times
C      IMVONN   I(*)   Buffer of 1's for IMVECT use
C   Outputs:
C      SCRTCH   I(*)   Work array needed by IMVECT
C      IRET     I      Error code returned from IMVECT
C   Common
C      CATBLK with tv and image windows set
C-----------------------------------------------------------------------
      INTEGER   TVCH, TVWIN(4), SCRTCH(*), IA1, IA2, SLIF(2), SLCHAN(2),
     *   LTYPE, IMVONN(*), IRET, SCALX
      CHARACTER STN1*8, STN2*8, PSTYPE*2
      REAL      MINY, MAXY, START, STOP, TIMES(*)
C
      INTEGER   IXP(5), IYP(5), IX, IY, ITRIM, NCH, TIT(4), TIT1(4),
     *   LCH, IXC, IXLMIN, LTIC, IX1, IX2, IT, LT, DEPTH(5)
      REAL      BLC(2), TRC(2), MINT, MAXT, YMULT, TMAXY, TMINY, HOUR
      LOGICAL   F, PFLG, NOLABL
      CHARACTER ROUTIN*6, STRING*132, TSTRNG*12, YLABEL(6)*10
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA F /.FALSE./
      DATA YLABEL /'JY', 'DEGREES','DECORREL','AMP-RATIO','PHS-DIFF',
     *   'DEC-RATIO'/
C-----------------------------------------------------------------------
C                                       initialize
      TMAXY = MAXY
      TMINY = MINY
      NOLABL = (TMAXY.EQ.0.0) .AND. (TMINY.EQ.0.0)
      MINT = START
      MAXT = STOP
C                                       Border is drawn outside data
      IXP(1) = TVWIN(1) - 2
      IYP(1) = TVWIN(2) - 2
      IXP(2) = TVWIN(3) + 2
      IYP(2) = IYP(1)
      IXP(3) = IXP(2)
      IYP(3) = TVWIN(4) + 2
      IXP(4) = IXP(1)
      IYP(4) = IYP(3)
      IXP(5) = IXP(1)
      IYP(5) = IYP(1)
      CALL IMVECT (TVCH, 5, IXP, IYP, IMVONN, IRET)
      ROUTIN = 'IMVECT'
      IF (IRET.NE.0) GO TO 900
C                                       Data label at the top.
      WRITE (TSTRNG,1000) IA1
      CALL CHTRIM (TSTRNG, 6, TSTRNG, LCH)
      STRING = 'BASELINE ' // STN1(:ITRIM(STN1)) // '(' // TSTRNG(:LCH)
     *   // ')-' // STN2(:ITRIM(STN2)) // '('
      NCH = ITRIM(STRING) + 1
      WRITE (TSTRNG,1000) IA2
      CALL CHTRIM (TSTRNG, 6, TSTRNG, LCH)
      STRING(NCH:) = TSTRNG(:LCH) // '), CH'
      NCH = NCH + LCH + 5
      WRITE (TSTRNG,1000) SLCHAN(1)
      CALL CHTRIM (TSTRNG, 6, TSTRNG, LCH)
      IF (SLCHAN(1).EQ.SLCHAN(2)) THEN
         STRING(NCH:) = 'ANNEL ' // TSTRNG(:LCH) // ', IF'
         NCH = NCH + LCH + 10
      ELSE
         STRING(NCH:) = 'S ' // TSTRNG(:LCH) // '-'
         NCH = NCH + LCH + 3
         WRITE (TSTRNG,1000) SLCHAN(2)
         CALL CHTRIM (TSTRNG, 6, TSTRNG, LCH)
         STRING(NCH:) = TSTRNG(:LCH) // ', IF'
         NCH = NCH + LCH + 4
         END IF
      WRITE (TSTRNG,1000) SLIF(1)
      CALL CHTRIM (TSTRNG, 6, TSTRNG, LCH)
      IF ((SLIF(1).EQ.SLIF(2)) .OR. (SLIF(2).EQ.0)) THEN
         STRING(NCH:) = ' ' // TSTRNG(:LCH)
         NCH = NCH + LCH + 1
      ELSE IF (SLIF(2).GT.0) THEN
         STRING(NCH:) = 'S ' // TSTRNG(:LCH) // '-'
         NCH = NCH + LCH + 3
         WRITE (TSTRNG,1000) SLIF(2)
         CALL CHTRIM (TSTRNG, 6, TSTRNG, LCH)
         STRING(NCH:) = TSTRNG(:LCH)
         NCH = NCH + LCH
      ELSE
         STRING(NCH:) = 'S ' // TSTRNG(:LCH) // '/'
         NCH = NCH + LCH + 3
         LT = -SLIF(2)
         WRITE (TSTRNG,1000) LT
         CALL CHTRIM (TSTRNG, 6, TSTRNG, LCH)
         STRING(NCH:) = TSTRNG(:LCH)
         NCH = NCH + LCH
         END IF
      STRING(NCH:) = ', POL ' // PSTYPE
      NCH = NCH + 7
      IX = (IXP(1) + IXP(2)) / 2 - (NCH * CSIZTV(1)) / 2
      IY = IYP(3) + CSIZTV(2)/2.0 + 1.5
      CALL IMCHAR (TVCH, IX, IY, 0, 0, STRING(:NCH), SCRTCH, IRET)
      ROUTIN = 'IMCHAR'
      IF (IRET.NE.0) GO TO 900
C                                       Label the image
      CALL FILL (5, 1, DEPTH)
      LOCNUM = 1
      CALL SETLOC (DEPTH, F)
      ROT(LOCNUM) = 0.0
      LABTYP(LOCNUM) = 0
      CORTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      AXFUNC(1,LOCNUM) = 0
      AXFUNC(2,LOCNUM) = 0
C                                       Determine Y scale
      YMULT = TMAXY
      IF (.NOT.NOLABL) THEN
         CALL METSCA (TMAXY, CPREF(2,LOCNUM), PFLG)
      ELSE
         YMULT = 1.0
         END IF
      YMULT = TMAXY / YMULT
      TMINY = TMINY * YMULT
      BLC(1) = IXP(1)
      BLC(2) = IYP(1)
      TRC(1) = IXP(3)
      TRC(2) = IYP(3)
      RPVAL(1,LOCNUM) = MINT
      RPVAL(2,LOCNUM) = TMINY
      AXINC(1,LOCNUM) = (MAXT - MINT) / (TVWIN(3) - TVWIN(1))
      AXINC(2,LOCNUM) = (TMAXY - TMINY) / (TVWIN(4) - TVWIN(2))
      RPLOC(1,LOCNUM) = TVWIN(1)
      RPLOC(2,LOCNUM) = TVWIN(2)
      CALL IBTICS (TVCH, BLC, TRC, IMVONN, IXLMIN, LTIC, SCRTCH, IRET)
C                                       X-axis ticks
      HOUR = 1.0 / 24.0
      IX1 = CATBLK(IIWIN)
      IX2 = CATBLK(IIWIN+2)
      LT = TIMES(IX1) / HOUR
      LTIC = MAX (2, LTIC)
      IYP(1) = BLC(2)
      IYP(2) = IYP(1) + LTIC
      IYP(3) = TRC(2)
      IYP(4) = IYP(3) - LTIC
      ROUTIN = 'IMVECT'
      DO 20 IXC = IX1,IX2
 10      IT = TIMES(IXC) / HOUR
         IF (IT.GT.LT) THEN
            LT = LT + 1
            YMULT = (LT * HOUR - TIMES(IXC-1)) / MAX (1.E-10,
     *        TIMES(IXC) - TIMES(IXC-1)) + IXC - 1.0
            IXP(1) = (YMULT - IX1) * SCALX + CATBLK(IICOR) + 0.5
            IXP(2) = IXP(1)
            IXP(3) = IXP(1)
            IXP(4) = IXP(1)
            CALL IMVECT (TVCH, 2, IXP, IYP, IMVONN, IRET)
            IF (IRET.NE.0) GO TO 900
            CALL IMVECT (TVCH, 2, IXP(3), IYP(3), IMVONN, IRET)
            IF (IRET.NE.0) GO TO 900
            GO TO 10
            END IF
 20      CONTINUE
C                                       Put ordinate label along side.
      STRING = ' '
      CTYP(2,LOCNUM) = YLABEL(LTYPE)
      IF (SLIF(2).LT.0) CTYP(2,LOCNUM) = YLABEL(LTYPE+3)
      STRING = CPREF(2,LOCNUM) // CTYP(2,LOCNUM)
      IF (NOLABL) STRING = 'ALL DATA ZERO'
      NCH = ITRIM (STRING)
      IX = IXLMIN - CSIZTV(1) * 1.5 - 0.5
      IY = BLC(2) + (TRC(2) - BLC(2)) / 5 + (NCH * CSIZTV(2)) / 2
      CALL IMCHAR (TVCH, IX, IY, 3, 0, STRING(:NCH), SCRTCH, IRET)
      ROUTIN = 'IMCHAR'
      IF (IRET.NE.0) GO TO 900
C                                       Put the time label at bottom.
      IY = BLC(2) - CSIZTV(2) * 1.5 - 0.5
      ROUTIN = 'IMCHAR'
      IXC = (BLC(1) + TRC(1)) / 2
C                                       Label the abscissa
      CALL TODHMS (MINT, TIT1)
      WRITE (STRING,1020) TIT1
      CALL CHTRIM (STRING, 12, STRING, NCH)
      CALL TODHMS (MAXT, TIT)
      WRITE (TSTRNG,1020) TIT
      CALL CHTRIM (TSTRNG, 12, TSTRNG, LCH)
C                                       times
      IF ((LCH+NCH)*CSIZTV(1).LT.(TVWIN(3)-TVWIN(1)+1)) THEN
         IX = IXC - 2 * CSIZTV(1)
         CALL IMCHAR (TVCH, IX, IY, 0, 0, 'TIME', SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 900
         IX = IXC - (4.0 + NCH) * CSIZTV(1)
         IX = MIN (IX, CATBLK(IICOR) - (NCH*CSIZTV(1))/2)
         IX = MAX (IX, TVWIN(1) - 4*CSIZTV(1)+1)
         CALL IMCHAR (TVCH, IX, IY, 0, 0, STRING(:NCH), SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 900
         IX = IXC + 4.0 * CSIZTV(1)
         IX = MAX (IX, CATBLK(IICOR+2) - (LCH*CSIZTV(1))/2)
         IX = MIN (IX, TVWIN(3) - (LCH-4)*CSIZTV(1)-1)
         CALL IMCHAR (TVCH, IX, IY, 0, 0, TSTRNG(:LCH), SCRTCH, IRET)
C                                       Special narrow plot frame
      ELSE
         STRING(NCH+1:) = '  ' // TSTRNG(:LCH)
         NCH = ITRIM(STRING)
         IX = IXC - (NCH * CSIZTV(1)) / 2
         CALL IMCHAR (TVCH, IX, IY, 0, 0, STRING(:NCH), SCRTCH, IRET)
         END IF
      IF (IRET.EQ.0) GO TO 999
C
 900  WRITE (MSGTXT,1900) IRET, ROUTIN
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
C 1000 FORMAT ('Baseline ',A,'(',I2,')',' - ',A,'(',I2,')',
C     *   2X,'(Ch ',I3,'-',I3,' IF ',I2,'-',I2,1X,A2,')')
 1000 FORMAT (I6)
 1020 FORMAT (I3,'/',2(I2.2,':'),I2.2)
 1900 FORMAT ('VISLAB: ERROR ',I4,' RETURNED FROM ',A)
      END
      SUBROUTINE VISLB2 (TVCH, TVWIN, VSTART, VSTOP, NVISPX, IMVONN,
     *   IRET)
C-----------------------------------------------------------------------
C   Routine to draw the graphics vectors around the visibility
C   function on the TV screen and provide useful labelling.
C   Inputs:
C      TVCH     I      Graphics plane to use
C      TVWIN    I(4)   blc, trc corners of window on TV
C      VSTART   I      First vis # displayed in editing frame
C      VSTOP    I      Final vis # displayed in editing frame
C      NVISPX   I      Maximum number of vis's shown
C   Outputs:
C      IRET     I      Error code returned from IMVECT
C-----------------------------------------------------------------------
      INTEGER   TVCH, TVWIN(4), VSTART, VSTOP, NVISPX, IMVONN(*),
     *   IRET
C
      INTEGER   IXP(5), IYP(5)
      CHARACTER ROUTIN*6
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       outside data
      IXP(1) = TVWIN(1) - 2
      IYP(1) = TVWIN(2) - 2
      IXP(2) = TVWIN(3) + 2
      IYP(2) = IYP(1)
      IXP(3) = IXP(2)
      IYP(3) = TVWIN(4) + 2
      IXP(4) = IXP(1)
      IYP(4) = IYP(3)
      IXP(5) = IXP(1)
      IYP(5) = IYP(1)
      CALL IMVECT (TVCH, 5, IXP, IYP, IMVONN, IRET)
      ROUTIN = 'IMVECT'
      IF (IRET.NE.0) GO TO 900
C                                      Then delineate box
C                                      displayed in editing frame
      IXP(1) = TVWIN(1) + 1 + (VSTART - 1.0) *
     *    (TVWIN(3) - TVWIN(1) - 2.0) / (NVISPX - 1.0)
      IXP(2) = TVWIN(1) + 1 + (VSTOP - 1.0) *
     *    (TVWIN(3) - TVWIN(1) - 2.0) / (NVISPX - 1.0)
      IXP(3) = IXP(2)
      IXP(4) = IXP(1)
      IXP(5) = IXP(1)
      CALL IMVECT (TVCH, 4, IXP(2), IYP(2), IMVONN, IRET)
      IF (IRET.EQ.0) GO TO 999
C
 900  WRITE (MSGTXT,1900) IRET, ROUTIN
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('VISLB2: ERROR ',I4,' RETURNED FROM ',A)
      END
      SUBROUTINE IBFLSC (IRET)
C-----------------------------------------------------------------------
C   IBFLSC loads up the arrays needed to display the data on the screen.
C   Output:
C      IRET   I   Error code: 0 => okay, else die.
C                            -1 => O good points, try next baseline
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER LSTYPE*2
      DOUBLE PRECISION DAMP, DPHAS
      REAL      TDELTA, TIME, LTIME, U, V, W
      INTEGER   I, LUN1, FIND1, NIO, BIND1, IPT1, LY, ISOU, LSOU, LINE,
     *   BO, IROUND, J, IANT1, IANT2, INDEX, VISOFF, TIT1(4), TIT2(4),
     *   NPIXSK, MXVIS, IPTOFF, IPTOF2, INDEX2, IERR
      LOGICAL   T, F
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1, BO /16, 1/
      DATA T, F /.TRUE., .FALSE./
      DATA MXVIS /MAXVIS/
C-----------------------------------------------------------------------
      CALL COPY (256, CATIMG, CATBLK)
      CALL UVPGET (IRET)
      NPIXSK = 10
C                                       Which baseline
      ANTENS(1) = NXANT1(BASPT)
      ANTENS(2) = NXANT2(BASPT)
      TDELTA = 1.E8
      IF (DPARM(5).LE.0.0) THEN
         DELTAT = MAX (10., -DPARM(5)) / (24. * 3600.)
      ELSE
         DELTAT = DPARM(5) / (24. * 3600.)
         END IF
      IF (DPARM(4).LE.0.0) THEN
         INTIME = MAX (1., -DPARM(4)) / (24. * 3600.)
      ELSE
         INTIME = DPARM(4) / (24. * 3600.)
         END IF
      EPSTIM = 0.5 / (24. * 3600.)
      EPSTIM = MIN (EPSTIM, 0.05 * INTIME)
C                                       open UV file
      CALL ZPHFIL ('UV', DISKOU, CNOOUT, 1, SCRNMA, IRET)
      CALL ZOPEN (LUN1, FIND1, DISKOU, SCRNMA, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'OPEN', IRET
         GO TO 990
         END IF
      LENBU = 0
      VISOFF = 0
      CALL UVINIT ('READ', LUN1, FIND1, NVIS, VISOFF, LREC, LENBU,
     *   JBUFSZ, BUFF1, BO, BIND1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'INIT', IRET
         GO TO 990
         END IF
      LY = 0
      LSOU = -1
      LINE = 0
      NV = 0
      NTRUE = 0
      IPTOFF = NRPARM + (LSTOKS - 1) * INCS + (LIF(1) - BIF) * INCIF
      IPTOF2 = NRPARM + (LSTOKS - 1) * INCS + (LIF(2) - BIF) * INCIF
      IF ((LIF(2).LE.0) .OR. (LIF(2).EQ.LIF(1))) IPTOF2 = 0
      INDEX2 = 0
C                                       loop thru data:
C                                       read buffer
 30   CALL UVDISK ('READ', LUN1, FIND1, BUFF1, NIO, BIND1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'READ', IRET
         GO TO 990
         END IF
      IPT1 = BIND1
      IF (NIO.GT.0) THEN
C                                       loop thru buffer
         DO 90 J = 1,NIO
            U = BUFF1(ILOCU+IPT1)
            V = BUFF1(ILOCV+IPT1)
            W = BUFF1(ILOCW+IPT1)
            IF (ILOCB.GE.0) THEN
               IANT1 = BUFF1(ILOCB+IPT1) / 256.0 + 0.1
               IANT2 = BUFF1(ILOCB+IPT1) - 256 * IANT1 + 0.1
            ELSE
               IANT1 = BUFF1(ILOCA1+IPT1) + 0.1
               IANT2 = BUFF1(ILOCA2+IPT1) + 0.1
               END IF
            IF ((IANT1.NE.ANTENS(1)) .OR. (IANT2.NE.ANTENS(2))) GO TO 85
            IF (ILOCSU.GE.0) THEN
               ISOU = IROUND (BUFF1(IPT1+ILOCSU))
            ELSE
               ISOU = 0
               END IF
            TIME = BUFF1(IPT1+ILOCT)
            IF (NV.EQ.0) THEN
               DO 35 I = 1,5
                  NV = NV + 1
                  SOUVAL(NV) = -1
                  TIMES(NV) = TIME - (6.0 - I) * DELTAT
                  MFLAGD(NV) = -1
 35               CONTINUE
               LTIME = TIMES(NV)
               LSOU = ISOU
               END IF
C                                       If there is a gap
C                                       of > 5 Tint, then
C                                       fill NPIXSK elements
C                                       with blanks
            IF (((TIME - LTIME).GT.(DELTAT*5.0)) .OR.
     *         ((LSOU.GT.-1) .AND. (LSOU.NE.ISOU))) THEN
               DO 40 I = 1,NPIXSK
                  NV = NV + 1
                  SOUVAL(NV) = -1
                  TIMES(NV) = LTIME + (I * (TIME - LTIME)) / (NPIXSK+1.)
                  MFLAGD(NV) = -1
 40               CONTINUE
               END IF
C                                       actual delta T
            IF ((SOUVAL(NV).GE.0) .AND. (TIME.GT.TIMES(NV)))
     *         TDELTA = MIN (TDELTA, TIME-TIMES(NV))
C                                       Load the arrays
            NV = NV + 1
            IF (NV.GT.MXVIS) THEN
               WRITE (MSGTXT,1020) MXVIS
               CALL MSGWRT (6)
               MSGTXT = 'Try averaging or TIMERANG'
               IRET = 1
               GO TO 990
               END IF
            SOUVAL(NV) = ISOU
            TIMES(NV) = TIME
            INDEX = IPT1 + IPTOFF
            INDEX2 = IPT1 + IPTOF2
C                                       Blanked
            IF ((BUFF1(INDEX).LE.0.0) .OR. ((IPTOF2.GT.0) .AND.
     *         (BUFF1(INDEX2).LE.0.0))) THEN
               MFLAGD(NV) = -1
               YVAL(NV,1) = FBLANK
               YVAL(NV,2) = FBLANK
               YVAL(NV,3) = FBLANK
               ERRORB(NV) = FBLANK
               YVALM(NV) = 0.0
C                                       Good - straight
            ELSE IF (IPTOF2.LE.0) THEN
               MFLAGD(NV) = 0
               YVAL(NV,1) = BUFF1(INDEX+1)
               YVAL(NV,2) = BUFF1(INDEX+2)
               YVAL(NV,3) = BUFF1(INDEX+3)
               ERRORB(NV) = 1.0 / SQRT(BUFF1(INDEX))
C                                       Compute model if required
               IF (WMODEL) THEN
                  LSTYPE = 'I'
                  CALL MODCLC (U, V, W, LSTYPE, DAMP, DPHAS, IERR)
                  IF (IERR.EQ.0) THEN
                     YVALM(NV) = DAMP
                  ELSE
                     YVALM(NV) = 0.0
                     END IF
                  END IF
               NTRUE = NTRUE + 1
C                                       Good - IF ratio
            ELSE
               INDEX2 = IPT1 + IPTOF2
               MFLAGD(NV) = 0
               IF (BUFF1(INDEX2+1).NE.0.0) THEN
                  YVAL(NV,1) = BUFF1(INDEX+1) / BUFF1(INDEX2+1)
               ELSE
                  YVAL(NV,1) = 10.
                  END IF
               YVAL(NV,2) = BUFF1(INDEX+2) - BUFF1(INDEX2+2)
               IF (YVAL(NV,2).LT.-180.0) YVAL(NV,2) = YVAL(NV,2) + 360.0
               IF (YVAL(NV,2).GT.180.0) YVAL(NV,2) = YVAL(NV,2) - 360.0
               IF (BUFF1(INDEX2+3).NE.0.0) THEN
                  YVAL(NV,3) = BUFF1(INDEX+3) / BUFF1(INDEX2+3)
               ELSE
                  YVAL(NV,3) = 10.
                  END IF
               ERRORB(NV) = 1.0 / SQRT(BUFF1(INDEX))
               NTRUE = NTRUE + 1
               END IF
C                                       update pointers
            LTIME = TIME
            LSOU = ISOU
 85         IPT1 = IPT1 + LREC
 90         CONTINUE
         GO TO 30
         END IF
C                                       Close file.
      CALL ZCLOSE (LUN1, FIND1, I)
      DO 95 I = 1,5
         NV = NV + 1
         SOUVAL(NV) = -1
         TIMES(NV) = TIMES(NV-1) + DELTAT
         MFLAGD(NV) = -1
 95      CONTINUE
C                                       Got some data
      IF (NTRUE.GT.0) THEN
         WRITE (MSGTXT,1010) ANTENS(1), ANTENS(2)
         CALL MSGWRT (2)
C                                       check interval
         IF (((DPARM(4).LE.0.) .OR. (DPARM(5).LE.0.)) .AND.
     *      (NTRUE.GT.10)) THEN
            IF (TDELTA.LT.1.0) THEN
               IF (DPARM(5).LE.0.0) THEN
                  DPARM(5) = 24. * 3600. * TDELTA * 1.003
                  WRITE (MSGTXT,1095) DPARM(5)
                  CALL MSGWRT (4)
                  IF (DPARM(4).LE.0.) DPARM(4) = DPARM(5)
               ELSE IF (DPARM(4).LE.0.0) THEN
                  DPARM(4) = 24. * 3600. * TDELTA * 1.003
                  WRITE (MSGTXT,1095) DPARM(4)
                  CALL MSGWRT (4)
                  END IF
            ELSE
               MSGTXT = 'UNABLE TO SET INTEGRATION INTERVAL' //
     *            ' PLEASE SET DPARM(5)'
               CALL MSGWRT (7)
               END IF
            END IF
C                                       Load up the flagging array
         CALL FILFLG (IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1050) IRET
            GO TO 990
            END IF
C                                       Reset start & stop for
C                                       labelling
         START = TIMES(1)
         STOP = TIMES(NV)
         CALL TODHMS (TIMES(1), TIT1)
         CALL TODHMS (TIMES(NV), TIT2)
         WRITE (MSGTXT,1040) TIT1, TIT2
         CALL MSGWRT (3)
         WRITE (MSGTXT,1060) NTRUE
         CALL MSGWRT (3)
      ELSE
         IRET = -1
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CAN''T ',A,' THE INPUT UV FILE, ERROR',I6)
 1010 FORMAT ('Loading data for baseline ',I3,' - ',I3)
 1020 FORMAT ('More than ',I8,' vis. points on this baseline')
 1040 FORMAT ('Loading data from ',I3,'/',I2.2,2(':',I2.2),' to ',I3,
     *   '/',I2.2,2(':',I2.2))
 1050 FORMAT ('ERROR ',I4,' LOADING FLAGGING ARRAY FROM FC TABLE')
 1060 FORMAT ('There are ',I6,' valid vis. points on this baseline')
 1095 FORMAT ('**** Sample separation interval set to',F6.1,
     *   ' seconds ****')
      END
      SUBROUTINE SETFRM (IRET)
C-----------------------------------------------------------------------
C  Routine to set up the windows, grid the data and load it into the TV
C  Output:
C      IRET      I       error code 0 => OK
C                           -1 => NO baseline with data
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IROUND, IT2, NTRY
      REAL      TEMP
      CHARACTER ROUTIN*6, CHTYPE(6)*12, CHST(13)*2
      LOGICAL   F, PALL1, PALL2
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA F /.FALSE./
      DATA CHTYPE /'AMPLITUDE', 'PHASE   ','DECORRELAT', 'AMP RATIO',
     *   'PHS DIFF', 'DEC RATIO'/
      DATA CHST /'YX','XY','YY','XX','LR','RL','LL','RR','??','I','Q',
     *   'U ','V '/
C-----------------------------------------------------------------------
C                                       requested parms not in core:
      IF ((LIF(1).NE.PLIF(1)) .OR. (LCHAN(1).NE.PLCHAN(1)) .OR.
     *   (PBASPT.NE.BASPT) .OR. (PLSTOK.NE.LSTOKS) .OR. (.NOT.DATAOK)
     *   .OR. (LIF(2).NE.PLIF(2))) THEN
         GRDFOK = .FALSE.
         GRDAOK = .FALSE.
         DATAOK = .FALSE.
         SCALOK = .FALSE.
         NTRY = 0
C                                       Fetch the data from master file
 10      IF ((DPARM(4).LE.0.) .OR. (DPARM(5).LE.0.0)) THEN
            CALL IBFLSC (IRET)
C                                       refetch with reset time interval
            IF ((IRET.EQ.0) .AND. (DPARM(5).GT.0.0) .AND.
     *         (DPARM(4).GT.0.0)) THEN
               MSGTXT = 'Redo the data averaging with new interval'
               CALL MSGWRT (2)
               CALL IBFLSC (IRET)
               END IF
         ELSE
            CALL IBFLSC (IRET)
            END IF
         IF (IRET.EQ.-1) THEN
            NTRY = NTRY + 1
            IF (UPTR) THEN
               BASPT = MOD (BASPT, NUMBAS) + 1
            ELSE
               BASPT = BASPT - 1
               IF (BASPT.LE.0) BASPT = NUMBAS
               END IF
            IF (NTRY.LT.NUMBAS) GO TO 10
               MSGTXT = 'ALL BASELINES SEEM TO LACK DATA FOR THIS' //
     *            ' STOKES'
               CALL MSGWRT (7)
               GO TO 999
            END IF
         ROUTIN = 'IBFLSC'
         IF (IRET.NE.0) GO TO 990
C                                       check all antenna to this one
         IF ((PBASPT.NE.BASPT) .AND. ((FLALL1) .OR. (FLALL2))) THEN
            PALL1 = FLALL1
            PALL2 = FLALL2
            FLALL1 = F
            FLALL2 = F
            IF (NXANT1(BASPT).EQ.NXANT1(PBASPT)) FLALL1 = PALL1
            IF (NXANT2(BASPT).EQ.NXANT1(PBASPT)) FLALL2 = PALL1
            IF (NXANT1(BASPT).EQ.NXANT2(PBASPT)) FLALL1 = PALL2
            IF (NXANT2(BASPT).EQ.NXANT2(PBASPT)) FLALL2 = PALL2
            END IF
         END IF
C                                       check window of TV
      CALL IBCHKW (IRET)
C                                       requested type changes: reload
      IF ((NXANT1(BASPT).EQ.NXANT2(BASPT)) .AND. (LTYPE.EQ.2)) LTYPE = 1
      IF (PLTYPE.NE.LTYPE) THEN
         DATAOK = .FALSE.
         SCALOK = .FALSE.
         GRDFOK = .FALSE.
         END IF
C                                       init window
      IF (DAWIN(3).LE.DAWIN(1)) THEN
         DAWIN(1) = 1
         DAWIN(3) = MAX (2, MIN (NV, 1 + TVWIN(3) - TVWIN(1)))
         END IF
C                                       Messages about load
      IF (.NOT.DATAOK) THEN
         TEMP = CATID(KDCRV+1) + (LSTOKS - CATIR(KRCRP+1)) *
     *      CATIR(KRCIC+1)
         IT2 = IROUND (TEMP) + 9
         IF ((IT2.LT.1) .OR. (IT2.GT.13)) IT2 = 9
         PSTYPE = CHST(IT2)
         SLCHAN(1) = BCHAN
         SLCHAN(2) = ECHAN
         IF (IFAVG) THEN
            SLIF(1) = BIF
            SLIF(2) = EIF
         ELSE
            SLIF(1) = LIF(1)
            IF ((LIF(2).EQ.0) .OR. (LIF(2).EQ.LIF(1))) THEN
               SLIF(2) = LIF(1)
            ELSE
               SLIF(2) = -LIF(2)
               END IF
            END IF
         IF (SLIF(2).GE.0) THEN
            WRITE (MSGTXT,1020) CHTYPE(LTYPE)
            CALL MSGWRT (2)
            WRITE (MSGTXT,1030) CHST(IT2), SLCHAN, SLIF
         ELSE
            WRITE (MSGTXT,1020) CHTYPE(LTYPE+3)
            CALL MSGWRT (2)
            WRITE (MSGTXT,1031) CHST(IT2), SLCHAN, LIF
            END IF
         CALL MSGWRT (2)
         END IF
C                                       Just load
      CALL IBFOAD (IRET)
      ROUTIN = 'IBFOAD'
      IF (IRET.GT.0) GO TO 990
C                                        set common pointers
      PLTYPE = LTYPE
      PLSTOK = LSTOKS
      PBASPT = BASPT
      PLCHAN(1) = LCHAN(1)
      PLCHAN(2) = LCHAN(2)
      PLIF(1) = LIF(1)
      PLIF(2) = LIF(2)
      CALL COPY (4, DAWIN, PWIND)
      PDOERR = DOERRB
      GO TO 999
C
 990  WRITE (MSGTXT,1990) IRET, ROUTIN
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Loading ',A)
 1030 FORMAT ('with Stokes ',A2,' chs',I4,'-',I4,' IFs',I3,'-',I3)
 1031 FORMAT ('with Stokes ',A2,' chs',I4,'-',I4,' IFs',I3,'/',I3)
 1990 FORMAT ('LOADING ERROR',I5,' FROM ',A)
      END
      SUBROUTINE FRMSEL (IRET)
C-----------------------------------------------------------------------
C  Routine to select a frame from the total visibility plot for display
C  and editing. Essentially the user defines a start and stop point on
C  the total visibility display and that is then displayed in the main
C  editing frame.
C  Output in common:
C     DAWIN   I(4)   Window into data
C  Output:
C     IRET    I      0 => OK, >0 => TV error -1 => button D, no action
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER ROUTIN*6
      INTEGER   QUAD, IBUT, ITW(3), DOCORN, IXP(5), IYP(5), MASK, IX,
     *   ZAND
      REAL      RPOS(2), PPOS(2)
      LOGICAL   F, DOIT
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
      IF (.NOT.SHOWVS) THEN
         IRET = -1
         MSGTXT = 'Total visibility plot not displayed, use ' //
     *      'SHOW ALL VIS'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       check window of TV
      CALL IBCHKW (IRET)
C                                       if resize - redraw data
      IF (.NOT.GRDAOK) THEN
         CALL SETFRM (IRET)
         ROUTIN = 'SETFRM'
         IF (IRET.NE.0) GO TO 900
         END IF
C                                       init box
      DAWIN(1) = MAX (1, DAWIN(1))
      DAWIN(3) = MIN (NV, DAWIN(3))
      IF (DAWIN(3).LE.DAWIN(1)) THEN
         DAWIN(1) = 1
         DAWIN(3) = MAX (2, NV)
         IF (NV.LE.2) GO TO 999
         END IF
      IXP(1) = TVWIN2(1) + 0.5 + (DAWIN(1) - 1.0) *
     *    (TVWIN2(3) - TVWIN2(1)) / (NV - 1.0)
      IXP(2) = TVWIN2(1) + 0.5 + (DAWIN(3) - 1.0) *
     *    (TVWIN2(3) - TVWIN2(1)) / (NV - 1.0)
      IXP(3) = IXP(2)
      IXP(4) = IXP(1)
      IXP(5) = IXP(1)
      IYP(1) = TVWIN2(2)
      IYP(2) = TVWIN2(2)
      IYP(3) = TVWIN2(4)
      IYP(4) = TVWIN2(4)
      IYP(5) = TVWIN2(2)
C                                       cursor on
      QUAD = -1
      RPOS(1) = IXP(1)
      RPOS(2) = IYP(1)
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IRET)
      ROUTIN = 'YCURSE'
      IF (IRET.NE.0) GO TO 900
      CURSON = .TRUE.
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL ZTIME (ITW)
      DOCORN = 1
C                                       init graphics plane
      MASK = 2 ** (GRLI - 1)
      MASK = ZAND (MASK, TVLIMG(1))
      IF (MASK.EQ.0) THEN
         ROUTIN = 'YSLECT'
         CALL YSLECT ('ONNN', GRLI, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 900
         END IF
      IF (GRLI.EQ.GRME) GRMEOK = .FALSE.
      IF (.NOT.GRLIOK) THEN
         ROUTIN = 'YZERO'
         CALL YZERO (GRLI, IRET)
         IF (IRET.NE.0) GO TO 900
         GRLIOK = .TRUE.
         END IF
      GRLIOK = .FALSE.
C                                       instructions
      MSGTXT = 'Hit button A to switch between start and end of frame'
      CALL MSGWRT (1)
      MSGTXT = 'Hit buttons B or C to select the frame'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button D to abandon frame selection'
      CALL MSGWRT (1)
C                                        read until cursor moves
 100  CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IRET)
      ROUTIN = 'YCURSE'
      IF (IRET.NE.0) GO TO 900
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (.NOT.DOIT) GO TO 100
         CALL IMVECT (GRLI, 5, IXP, IYP, IMVOFF, IRET)
         ROUTIN = 'IMVECT'
         IF (IRET.NE.0) GO TO 900
C                                       button D quit
         IF (IBUT.GE.8) THEN
            IRET = -1
            GRLIOK = .TRUE.
            GO TO 999
            END IF
C                                       BLC
         IF (DOCORN.EQ.1) THEN
            IX = RPOS(1) + 0.5
CCC            IX = MIN (MAX (IX, TVWIN2(1) + 1), IXP(2) - 1)
            IX = MIN (IX,TVWIN2(3))
            IXP(1) = IX
            IXP(4) = IXP(1)
            IXP(5) = IXP(1)
C                                       TRC
         ELSE
            IX = RPOS(1) + 0.5
            IX = MIN (MAX (IX, IXP(1) + 1), TVWIN2(3) - 1)
            IXP(2) = IX
            IXP(3) = IXP(2)
            END IF
C                                       redraw
         CALL IMVECT (GRLI, 5, IXP, IYP, IMVONN, IRET)
         ROUTIN = 'IMVECT'
         IF (IRET.NE.0) GO TO 900
C                                       button A
         IF (IBUT.EQ.1) THEN
            DOCORN = 3 - DOCORN
            IF (DOCORN.EQ.1) THEN
               RPOS(1) = IXP(1)
               RPOS(2) = IYP(1)
            ELSE
               RPOS(1) = IXP(3)
               RPOS(2) = IYP(3)
               END IF
            PPOS(1) = RPOS(1)
            PPOS(2) = RPOS(2)
            CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IRET)
            ROUTIN = 'YCURSE'
            IF (IRET.NE.0) GO TO 900
            END IF
         IF (IBUT.LE.1) GO TO 100
C                                       set the window
      DAWIN(1) =  (IXP(1) - TVWIN2(1)) * (NV - 1.0) /
     *    (TVWIN2(3) - TVWIN2(1)) + 1.5
      DAWIN(3) =  (IXP(2) - TVWIN2(1)) * (NV - 1.0) /
     *    (TVWIN2(3) - TVWIN2(1)) + 1.5
      DAWIN(1) = MAX (1, DAWIN(1))
      DAWIN(3) = MIN (NV, DAWIN(3))
C                                       what we selected
      WRITE (MSGTXT,1100) DAWIN(1), DAWIN(3)
      CALL MSGWRT (1)
      CALL IMVECT (GRLI, 5, IXP, IYP, IMVOFF, IRET)
      IF (IRET.NE.0) GO TO 900
      GRLIOK = .TRUE.
      GO TO 999
C                                       TV error
 900  WRITE (MSGTXT,1900) IRET, ROUTIN
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('FRMSEL selects window',2I7)
 1900 FORMAT ('TV ERROR ',I6,' IN ',A)
      END
      SUBROUTINE DMEAN (YVAL, MFLAGD, IX1, IX2, MEAN, RMS, IRET)
C-----------------------------------------------------------------------
C  Routine to determine the mean and rms over a specified time range
C  Inputs:
C     YVAL          R(*)       Array of values
C     MFLAGD        I(*)       Flag numbers
C     IX1           I          First value to consider
C     IX2           I          Final value to consider
C  Outputs:
C     MEAN          R          Mean value
C     RMS           R          Rms value
C     IRET          I          Error code, 0 => OK
C-----------------------------------------------------------------------
      INTEGER MFLAGD(*), IX1, IX2, IRET
      REAL    YVAL(*), MEAN, RMS
C
      INTEGER ICNT, I
      REAL    SUM, SUMRMS, TEMP
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      ICNT = 0
      SUM = 0.0
      SUMRMS = 0.0
C
      DO 100 I = IX1, IX2
         IF ((YVAL(I).NE.FBLANK) .AND. (MFLAGD(I).EQ.0)) THEN
            SUM = SUM + YVAL(I)
            SUMRMS = SUMRMS + YVAL(I) * YVAL(I)
            ICNT = ICNT + 1
            END IF
 100     CONTINUE
C
      IF (ICNT.GT.0) THEN
         MEAN = SUM / ICNT
         TEMP = (ICNT * SUMRMS - (SUM * SUM))
         IF (TEMP.LT.0.0) TEMP = 0.0
         RMS = SQRT (TEMP) / ICNT
         GO TO 999
         END IF
C
      IRET = 1
C
 999  RETURN
      END
      SUBROUTINE DECPTR (ANT1, ANT2, NXANT1, NXANT2, NBASE, BASPT)
C-----------------------------------------------------------------------
C  Routine to compare the selected antenna numbers with those stored
C  in the antenna arrays and decide which 'baseline number' is being
C  used.
C  Inputs:
C      ANT1     I          1st antenna of baseline
C      ANT2     I          2nd antenna of baseline
C      NXANT1   I(*)       Array of 1st antenna numbers
C      NXANT2   I(*)       Array of 2nd antenna numbers
C      NBASE    I          Max. number of baselines
C  Outputs:
C      BASPT    I          Baseline pointer in the NXANTn
C                          arrays, if = 0 no match found.
C-----------------------------------------------------------------------
      INTEGER ANT1, ANT2, NXANT1(*), NXANT2(*), NBASE, BASPT
C
      INTEGER I
C-----------------------------------------------------------------------
      BASPT = 0
      DO 100 I = 1, NBASE
         IF ((ANT1.EQ.NXANT1(I)) .AND. (ANT2.EQ.NXANT2(I))) THEN
            BASPT = I
            GO TO 999
            END IF
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FILFLG (IRET)
C-----------------------------------------------------------------------
C   Loads up MFLAGD array from information in the FC table.
C   Output:
C      IRET       I    Return code, 0=OK, else TABIO error more or less
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   A1, A2, IV, I, NV1, NV2, NV3, JERR, IV1, IV2, NV4, NV5,
     *   NV6, NV7
      HOLLERITH LFCSTK
      CHARACTER STK*4
      LOGICAL   STKOK, DOSTOK
      REAL      T1, T2
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       open the FC file
      CALL IBFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG, FCNUMB,
     *   NNFLAG, FCBUF, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((FCNUMB.LE.0) .OR. (NNFLAG.LE.0)) THEN
         IRET = 0
         GO TO 980
         END IF
C                                       Read through the FC file
      A1 = ANTENS(1)
      A2 = ANTENS(2)
      STK = ' '
      CALL CHR2H (4, STK, 1, LFCSTK)
      NV1 = MAX (1, NV/8)
      NV2 = MAX (1, NV/4)
      NV3 = MAX (1, (3*NV)/8)
      NV4 = MAX (1, NV/2)
      NV5 = MAX (1, (5*NV)/8)
      NV6 = MAX (1, (6*NV)/8)
      NV7 = MAX (1, (7*NV)/8)
      DO 100 I = 1,NNFLAG
         CALL TABIO ('READ', 0, I, FCTIME, FCBUF, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, I
            CALL MSGWRT (8)
            GO TO 980
            END IF
C                                       Ignore if flagged
         IF (IRET.LT.0) GO TO 100
C                                       check antennas
         IF ((FCBASL(1).LE.0) .AND. (FCBASL(2).GT.0)) THEN
            FCBASL(1) = FCBASL(2)
            FCBASL(2) = 0
            END IF
         IF (FCBASL(1).GT.0) THEN
            IF (FCBASL(2).GT.0) THEN
               IF ((A1.NE.FCBASL(1)) .OR. (A2.NE.FCBASL(2))) GO TO 100
            ELSE
               IF ((A1.NE.FCBASL(1)) .AND. (A2.NE.FCBASL(1))) GO TO 100
               END IF
            END IF
C                                       check IF and channel
         FCIF(1) = MAX (1, FCIF(1))
         IF (FCIF(2).LE.0) FCIF(2) = 1000000
         IF (((LIF(1).LT.FCIF(1)) .OR. (LIF(1).GT.FCIF(2))) .AND.
     *      ((LIF(2).LT.FCIF(1)) .OR. (LIF(2).GT.FCIF(2)))) GO TO 100
         IF (((FCCHAN(1).GT.0) .AND. (LCHAN(1).LT.FCCHAN(1))) .OR.
     *      ((FCCHAN(2).GT.0) .AND. (LCHAN(1).GT.FCCHAN(2)))) GO TO 100
C                                       check Stokes
         IF (LFCSTK.NE.FCSFLG) THEN
            CALL H2CHR (4, 1, FCSFLG, STK)
            STKOK = DOSTOK (ILSTOK, STK, LSTOKS)
            LFCSTK = FCSFLG
            END IF
         IF (.NOT.STKOK) GO TO 100
C                                       times
         T1 = FCTIME(1)
         T2 = FCTIME(2)
         IF (T2.LT.TIMES(1)) GO TO 100
         IF (T1.GT.TIMES(NV)) GO TO 100
C                                       flag data: hash
         IF (T2.LT.TIMES(NV1)) THEN
            IV2 = NV1
         ELSE IF (T2.LT.TIMES(NV2)) THEN
            IV2 = NV2
         ELSE IF (T2.LT.TIMES(NV3)) THEN
            IV2 = NV3
         ELSE IF (T2.LT.TIMES(NV4)) THEN
            IV2 = NV4
         ELSE IF (T2.LT.TIMES(NV5)) THEN
            IV2 = NV5
         ELSE IF (T2.LT.TIMES(NV6)) THEN
            IV2 = NV6
         ELSE IF (T2.LT.TIMES(NV7)) THEN
            IV2 = NV7
         ELSE
            IV2 = NV
            END IF
         IF (T1.GE.TIMES(NV7)) THEN
            IV1 = NV7
         ELSE IF (T1.GE.TIMES(NV6)) THEN
            IV1 = NV6
         ELSE IF (T1.GE.TIMES(NV5)) THEN
            IV1 = NV5
         ELSE IF (T1.GE.TIMES(NV4)) THEN
            IV1 = NV4
         ELSE IF (T1.GE.TIMES(NV3)) THEN
            IV1 = NV3
         ELSE IF (T1.GE.TIMES(NV2)) THEN
            IV1 = NV2
         ELSE IF (T1.GE.TIMES(NV1)) THEN
            IV1 = NV1
         ELSE
            IV1 = 1
            END IF
         DO 20 IV = IV1,IV2
            IF ((TIMES(IV).GE.T1) .AND. (TIMES(IV).LE.T2) .AND.
     *         (MFLAGD(IV).EQ.0)) MFLAGD(IV) = FCNUMB
 20         CONTINUE
 100     CONTINUE
C                                       close FC file
 980  CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, JERR)
      IF (IRET.LE.0) IRET = JERR
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FILFLG: ERROR',I5,' READING FC TABLE RECORD',I5)
      END
      SUBROUTINE IEDIT (ICLIP, NCLIP, TTY, FLUXCL, BELOW, IXOFF, IRET)
C-----------------------------------------------------------------------
C   Routine to perform an interactive clip on the frame currently
C   displayed on the TV.
C   Inputs:
C      ICLIP    R(4,20)    Array containing the clip information;
C                          ICLIP(1,x) holds the x position in the
C                             data
C                          ICLIP(2,x) holds the y position in the
C                             data
C                          ICLIP(3,x) holds the x position on the TV
C                          ICLIP(4,x) holds the y position on the TV
C      NCLIP    I          Number of x,y pairs defining the curve
C      TTY      I(2)
C Outputs:
C      FLUXCL   R(*)       Array containing fluxes (or phases) as
C                          a function of x pixel number, below or
C                          above which data is to be flagged.
C      IXOFF    I          Offset between TV pixel number and
C                          data pixel number
C      BELOW    L          True if flag data below curve, false
C                          if flag data above.
C      IRET     I          Error code, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   NCLIP, TTY(2), IXOFF, IRET
      REAL      ICLIP(4,*), FLUXCL(*)
      LOGICAL   BELOW
C
      INTEGER   I, JXP, JYP(2), FRSTX, LASTX, IROUND, MCLIP, IXTV1,
     *   IXTV2, J, III
      REAL      FLX1, FLX2, FLXDF, FT, RPOS(2), CORN(2)
      CHARACTER MSGBUF*72, CHTEMP*4, ROUTIN*6
      LOGICAL   T, F, DOSTOK
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                       Ask if above or below
      BELOW = T
 10   MSGBUF = 'Do you wish to flag above or below the curve? [A/B]'
      CALL INQSTR (TTY, MSGBUF, 4, CHTEMP, IRET)
      IF (IRET.EQ.10) THEN
         MSGTXT = 'STRING TOO LONG, TRY AGAIN'
         CALL MSGWRT (7)
         GO TO 10
         END IF
      IF (IRET.NE.0) GO TO 950
      CALL CHLTOU (4, CHTEMP)
      IF (CHTEMP(:1).EQ.'A') THEN
         BELOW = F
      ELSE IF (CHTEMP(:1).EQ.'B') THEN
         BELOW = T
      ELSE
         GO TO 10
         END IF
C                                       Fill up the true clipping
C                                       array from the x,y pairs
      FRSTX = IROUND (ICLIP(3,1))
      LASTX = IROUND (ICLIP(3,NCLIP))
      MCLIP = NCLIP - 1
      DO 20 I = 1,MCLIP
         IXTV1 = IROUND (ICLIP(3,I))
         IXTV2 = IROUND (ICLIP(3,I+1))
         FLX1 = MINY + (ICLIP(2,I) - 1.0) / SCALY
         FLX2 = MINY + (ICLIP(2,I+1) -1.0) / SCALY
         FLXDF = (FLX2 - FLX1) / (IXTV2 - IXTV1)
         DO 15 J = IXTV1,IXTV2
            FLUXCL(J) = FLX1 + FLXDF * (J-IXTV1)
 15         CONTINUE
 20      CONTINUE
C                                       Loop and flag
      IXOFF = IROUND (ICLIP(1,1))
      IF (DOSTOK (ILSTOK, STKFLG, PLSTOK)) THEN
         CALL YHOLD ('ONNN', I)
         DO 30 I = FRSTX,LASTX,SCALX
            III = (I - FRSTX) / SCALX + IXOFF
            FT = YVAL(III,PLTYPE)
            IF ((MFLAGD(III).EQ.0) .AND. (FT.NE.FBLANK)) THEN
               IF (BELOW) THEN
                  IF (FT.GT.FLUXCL(I)) GO TO 30
               ELSE
                  IF (FT.LE.FLUXCL(I)) GO TO 30
                  END IF
               CORN(1) = III
               CORN(2) = YPIX(III)
               CALL MP2IMA (CORN, RPOS)
               JXP = IROUND (RPOS(1))
               JYP(1) = YPIXR(1,III)
               JYP(2) = YPIXR(2,III)
               CALL DADRAW (GRDA, SCALX, JXP, JYP, IMVOFF, IRET)
               ROUTIN = 'IMVECT'
               IF (IRET.NE.0) GO TO 900
               END IF
 30         CONTINUE
         CALL YHOLD ('OFFF', I)
         END IF
      GO TO 999
C                                       TV error
 900  WRITE (MSGTXT,1900) IRET, ROUTIN
      GO TO 990
C                                       TTY error
 950  IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1950) IRET
      ELSE IF (IRET.LT.0) THEN
         MSGTXT = 'Return to menu, non-numeric input'
         IRET = 0
         END IF
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('TV ERROR ',I6,' IN ',A)
 1950 FORMAT ('TERMINAL ERROR',I5)
      END
      SUBROUTINE RMSEDT (TTY, TVCH, IXP, IYP, FLUXL, FLUXH, IRET)
C-----------------------------------------------------------------------
C   Subroutine to set up the editing boxes when doing editing by RMS
C   and MEAN.
C-----------------------------------------------------------------------
      INTEGER   TVCH, TTY(2), IRET, IXP(5), IYP(5)
      REAL      FLUXH, FLUXL
C
      CHARACTER CHTEMP*4, MSGBUF*72
      INTEGER   IX1, IX2, IY1, IY2, JERR, IXX(2), IYX(2)
      REAL      RPOS(2), CORN(2), MEAN, RMS
      DOUBLE PRECISION DTEMP
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Determine the mean and rms
      RPOS(1) = IXP(1)
      RPOS(2) = IYP(1)
      CALL IMA2MP (RPOS, CORN)
      CALL CORNER (CATBLK, IIWIN, CORN)
      IX1 = CORN(1) + 0.49
      IY1 = CORN(2) + 0.49
      RPOS(1) = IXP(2)
      RPOS(2) = IYP(2)
      CALL IMA2MP (RPOS, CORN)
      CALL CORNER (CATBLK, IIWIN, CORN)
      IX2 = CORN(1) + 0.49
      IY2 = CORN(2) + 0.49
      CALL DMEAN (YVAL(1,PLTYPE), MFLAGD, IX1, IX2, MEAN, RMS, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'ERROR DETERMINING MEAN AND RMS - CHECK TV SCREEN'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       ask multiplier
      WRITE (MSGBUF,1010) MEAN, RMS
      CALL INQFLT (TTY, MSGBUF, 1, DTEMP, IRET)
      IF (IRET.NE.0) THEN
         IRET = -1
         GO TO 999
         END IF
      FLUXH = MEAN + RMS * DTEMP
      FLUXL = MEAN - RMS * DTEMP
C                                       draw lines
      IYP(1) = (FLUXL - MINY) * SCALY + 0.5 + TVWIN(2)
      IYP(2) = IYP(1)
      CALL IMVECT (TVCH, 2, IXP, IYP, IMVONN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Upper line
      IYP(3) = (FLUXH - MINY) * SCALY + 0.5 + TVWIN(2)
      IYP(4) = IYP(3)
      IXP(3) = IXP(1)
      IXP(4) = IXP(2)
      CALL IMVECT (TVCH, 2, IXP(3), IYP(3), IMVONN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       mean too
      IXX(1) = IXP(1)
      IXX(2) = IXP(2)
      IYX(1) = (MEAN - MINY) * SCALY + 0.5 + TVWIN(2)
      IYX(2) = IYX(1)
      CALL IMVECT (TVCH, 2, IXX, IYX, IMVONN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       confirm
      MSGBUF = 'Do you want data above and below these lines edited? '
     *   // '[Y/N]'
 898  CALL INQSTR (TTY, MSGBUF, 4, CHTEMP, IRET)
      IF (IRET.EQ.10) THEN
         MSGTXT = 'STRING TOO LONG, TRY AGAIN'
         CALL MSGWRT (7)
         GO TO 898
         END IF
      IF (IRET.NE.0) THEN
         IRET = -1
         GO TO 900
         END IF
      CALL CHLTOU (4, CHTEMP)
      IF (CHTEMP(:1).EQ.'N') THEN
         MSGBUF = 'Do you wish to redo the editing commands? [Y/N]'
 899     CALL INQSTR (TTY, MSGBUF, 4, CHTEMP, IRET)
         IF (IRET.EQ.10) THEN
            MSGTXT = 'STRING TOO LONG, TRY AGAIN'
            CALL MSGWRT (7)
            GO TO 899
            END IF
         IF (IRET.NE.0) THEN
            IRET = -1
            GO TO 900
            END IF
         CALL CHLTOU (4, CHTEMP)
         IF (CHTEMP(:1).EQ.'Y') THEN
            IRET = -3
            GO TO 900
            END IF
         IRET = -2
         END IF
C
 900  CALL IMVECT (TVCH, 2, IXP, IYP, IMVOFF, JERR)
      CALL IMVECT (TVCH, 2, IXP(3), IYP(3), IMVOFF, JERR)
      CALL IMVECT (TVCH, 2, IXX, IYX, IMVOFF, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Mean, RMS = ',2(1X,1PE10.4), ' Specify RMS multiplier')
      END
      SUBROUTINE TVANOT (TVX, TVY, IX0, IY0, LSOU, SNAMES, LTVX, LEVAL,
     *   LFLUXS, IRET)
C-----------------------------------------------------------------------
C  Routine to do the CURVAL type anotation for IBLED.
C  Inputs:
C    TVX        I        Current x position in data arrays
C    TVY        I        Current y position in data arrays
C    IX0        I        TV x pixel at which to start annotation
C    IY0        I        TV y pixel at which to start annotation
C    SNAMES     C(*)*16  Source names from Su table
C  Input/Output:
C    LSOU       I        Last source number displayed
C    LTVX       I        Last value of TVX
C    LEVAL      R        Last value of error bar
C    LFLUXS     C*12     Last value of flux string
C  Output:
C    IRET       I        ERROR CODE, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   TVX, TVY, IX0, IY0, LSOU, LTVX, IRET
      REAL      LEVAL
      CHARACTER SNAMES(*)*16, LFLUXS*12
C
      REAL      PIXVAL, TEMP, EVAL
      INTEGER   ISOU, IY, ITIME(4)
      LOGICAL   BLANKD, ONPIXL, DOE
      CHARACTER BLNAME*11, ROUTIN*6, STRING*12, HILIT*1
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      CALL YHOLD ('ONNN', IY)
      BLNAME = ' '
      PIXVAL = YVAL(TVX,PLTYPE)
      BLANKD = (PIXVAL.EQ.FBLANK) .OR. (MFLAGD(TVX).NE.0)
      ISOU = SOUVAL(TVX)
      IF ((ISOU.LE.0) .OR. (ISOU.GT.MAXSOU)) ISOU = 0
      IY = IY0
      DOE = (DOERRB) .AND. (PLTYPE.LE.2)
      IF ((PLIF(2).GT.0) .AND. (PLIF(2).NE.PLIF(1))) DOE = .FALSE.
C                                       source name
      IF (MAXSOU.GT.0) THEN
         IF (ISOU.NE.LSOU) THEN
            IF (ISOU.GT.0) THEN
               CALL IMCHAR (GRCV, IX0, IY, 0, 0, SNAMES(ISOU)(:11),
     *            SCRTCH, IRET)
            ELSE
               CALL IMCHAR (GRCV, IX0, IY, 0, 0, BLNAME, SCRTCH, IRET)
               END IF
            ROUTIN = 'IMCHAR'
            IF (IRET.NE.0) GO TO 900
            LSOU = ISOU
            END IF
         IY = IY - 2*NEDGE - CSIZTV(2)
         END IF
C                                       time
      IF (TVX.NE.LTVX) THEN
         TEMP = TIMES(TVX)
         CALL TODHMS (TEMP, ITIME)
         WRITE (STRING,1000) ITIME
         CALL IMCHAR (GRCV, IX0, IY, 0, 0, STRING(:11), SCRTCH, IRET)
         ROUTIN = 'IMCHAR'
         IF (IRET.NE.0) GO TO 900
         END IF
      IY = IY - 2*NEDGE - CSIZTV(2)
C                                       flux
      ONPIXL = (.NOT.BLANKD) .AND. (TVY.EQ.YPIX(TVX))
      HILIT = ' '
      IF (ONPIXL) HILIT = '*'
      IF (BLANKD) THEN
         STRING = 'BLANKED'
      ELSE IF (ABS(PIXVAL).LT.9.99) THEN
         WRITE (STRING,1020) HILIT, PIXVAL
      ELSE IF (ABS(PIXVAL).LT.1000.) THEN
         WRITE (STRING,1021) HILIT, PIXVAL
      ELSE
         WRITE (STRING,1022) HILIT, PIXVAL
         END IF
      IF (STRING.NE.LFLUXS) THEN
         LFLUXS = STRING
         CALL IMCHAR (GRCV, IX0, IY, 0, 0, STRING(:9), SCRTCH, IRET)
         ROUTIN = 'IMCHAR'
         IF (IRET.NE.0) GO TO 900
         END IF
C                                   Write the error bar
      EVAL = -1.0
      IF (DOE) THEN
         IY = IY - 2*NEDGE - CSIZTV(2)
         STRING = ' '
         IF (BLANKD) THEN
            STRING = BLNAME
            EVAL = -0.5
         ELSE
            EVAL = ERRORB(TVX)
            IF (PLTYPE.EQ.2) THEN
               IF (YVAL(TVX,1).NE.0) THEN
                  EVAL = EVAL / (DG2RAD * YVAL(TVX,1))
               ELSE
                  EVAL = EVAL / DG2RAD
                  END IF
               END IF
            IF (ABS(EVAL).LT.9.99) THEN
               WRITE (STRING,1030) EVAL
            ELSE IF (ABS(EVAL).LT.1000.0) THEN
               WRITE (STRING,1031) EVAL
            ELSE
               WRITE (STRING,1032) EVAL
               END IF
            END IF
         IF (ABS(LEVAL-EVAL).GE.0.0005) THEN
            CALL IMCHAR (GRCV, IX0, IY, 0, 0, STRING(:9), SCRTCH, IRET)
            ROUTIN = 'IMCHAR'
            IF (IRET.NE.0) GO TO 900
            END IF
         END IF
      LTVX = TVX
      LEVAL = EVAL
      GO TO 995
C
 900  WRITE (MSGTXT,1200) IRET, ROUTIN
      CALL MSGWRT (8)
C
 995  CALL YHOLD ('OFFF', IY)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I2.2,'/',I2.2,2(':',I2.2))
 1020 FORMAT (A1,F8.5)
 1021 FORMAT (A1,F8.3)
 1022 FORMAT (A1,F8.1)
 1030 FORMAT ('+/-',F6.3)
 1031 FORMAT ('+/-',F6.2)
 1032 FORMAT ('+/-',F6.1)
 1200 FORMAT ('TV ERROR ',I6,' IN ',A)
      END
      SUBROUTINE CORNER (CATBLK, IIWIN, CORN)
C-----------------------------------------------------------------------
C  Routine to ensure corners specified are within the allowed range
C  Inputs:
C    CATBLK       I(256)      Image catalogue
C    IIWIN        I           Pointer to image catalogue
C
C  Outputs:
C    CORN         R(2)        lower left x,y of pixel
C-----------------------------------------------------------------------
      INTEGER   CATBLK(256), IIWIN
      REAL      CORN(2)
C-----------------------------------------------------------------------
      IF (CORN(1).LT.CATBLK(IIWIN)) CORN(1) = CATBLK(IIWIN)
      IF (CORN(2).LT.CATBLK(IIWIN+1)) CORN(2) = CATBLK(IIWIN+1)
      IF (CORN(1).GT.CATBLK(IIWIN+2)) CORN(1) = CATBLK(IIWIN+2)
      IF (CORN(2).GT.CATBLK(IIWIN+3)) CORN(2) = CATBLK(IIWIN+3)
      RETURN
      END
      SUBROUTINE FLGWRT (BRANCH, IBLC, ITRC, BELOW, IXOFF, FLUXCL, DOIT,
     *   NFLAGD, IRET)
C-----------------------------------------------------------------------
C   Routine to write the flagging array and then dump the flagging
C   information into the FC table.
C   Inputs:
C      BRANCH   I        Flagging operation being performed
C      IBLC     I(2)     X,Y lower vertices of flagging area
C      ITRC     I(2)     X,Y upper vertices of flagging area
C      BELOW    L        Flag logical for interactive edit
C      IXOFF    I        Offset between TV and data pixels
C      FLUXCL   R(*)     Clipping curve
C      DOIT     L        Apply these flags to the current displayed data
C   Input via common:
C      FCLIPR   I(2)     Clip range to apply
C   Outputs:
C      NFLAGD   I(2)     No. flagging points, FC records
C      IRET     I        Error code, 0=> OK
C-----------------------------------------------------------------------
      INTEGER   BRANCH, IBLC(2), ITRC(2), IXOFF, NFLAGD(2), IRET
      LOGICAL   BELOW, DOIT
      REAL      FLUXCL(*)
C
      INTEGER   IXL, IXH, IX, LX, LSOUR, ISOUR, IXX, FRSTX
      LOGICAL   DOBOX
      REAL      RPOS(2), CORN(2), DVAL, TIML, TIMH
      CHARACTER CHTYPE(6)*12, STRING*12
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA CHTYPE /'-AMPLITUDE', '-PHASE','-DECORRELA', '-AMP RATIO',
     *   '-PHS DIFF', '-DEC RATIO'/
C-----------------------------------------------------------------------
      NFLAGD(1) = 0
      NFLAGD(2) = 0
      DOBOX = (BRANCH.EQ.3) .OR. (BRANCH.EQ.4) .OR. (BRANCH.EQ.5) .OR.
     *   (BRANCH.EQ.6)
C                                       set up corners
      IF (BRANCH.NE.8) THEN
         RPOS(1) = IBLC(1)
         RPOS(2) = IBLC(2)
         CALL IMA2MP (RPOS, CORN)
         CALL CORNER (CATBLK, IIWIN, CORN)
         IXL = CORN(1) + 0.49
         IF (IXL.LT.1) IXL = 1
         RPOS(1) = ITRC(1)
         RPOS(2) = ITRC(2)
         CALL IMA2MP (RPOS, CORN)
         CALL CORNER (CATBLK, IIWIN, CORN)
         IXH = CORN(1) + 0.49
         IF (IXH.GT.CATBLK(KINAX)) IXH = CATBLK(KINAX)
      ELSE
         IXL = 1
         IXH = CATBLK(KINAX)
         END IF
C                                       open FC table
      CALL IBFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG, FCNUMB,
     *   NNFLAG, FCBUF, IRET)
      FCNUMB = FCNUMB + 1
      IF (IRET.NE.0) GO TO 999
C                                       set up IF/ch ranges
      IF (DOCHAN) THEN
         FCCHAN(1) = 1
         FCCHAN(2) = 0
      ELSE
         FCCHAN(1) = BCHAN
         FCCHAN(2) = ECHAN
         END IF
      FCIF(1) = PLIF(1)
      FCIF(2) = PLIF(1)
      IF (IFAVG) THEN
         FCIF(1) = BIF
         FCIF(2) = EIF
         END IF
      IF (DOIFS) THEN
         FCIF(1) = 1
         FCIF(2) = 0
         END IF
      FCBASL(1) = ANTENS(1)
      FCBASL(2) = ANTENS(2)
      IF (FLALL1) THEN
         FCBASL(2) = 0
      ELSE IF (FLALL2) THEN
         FCBASL(1) = ANTENS(2)
         FCBASL(2) = 0
         END IF
      CALL CHR2H (4, STKFLG, 1, FCSFLG)
      FCSOUR = 0
      LX = PLTYPE
      IF ((PLIF(2).GT.0) .AND. (PLIF(2).NE.PLIF(1))) LX = LX + 3
      STRING = PSTYPE // CHTYPE(LX)
      CALL CHR2H (12, STRING, 1, FCTYPE)
      LSOUR = -1
C                                       Loop over areas of TV image
C                                       to be flagged
      TIML = 1.E10
      TIMH = -TIML
      LX = -1
      FRSTX = IBLC(1)
      DO 100 IX = IXL,IXH
         IF ((MFLAGD(IX).GE.0) .AND. (YVAL(IX,PLTYPE).NE.FBLANK)) THEN
            ISOUR = SOUVAL(IX) + 0.5
C                                       not continuous
            IF ((LX.GT.0) .AND.
     *         (((IX.NE.LX+1) .AND. (BRANCH.NE.2) .AND. (BRANCH.NE.8))
     *         .OR. ((.NOT.DOSOUR) .AND. (ISOUR.NE.LSOUR)))) THEN
               NNFLAG = NNFLAG + 1
               NFLAGD(2) = NFLAGD(2) + 1
               FCTIME(1) = TIML - EPSTIM
               FCTIME(2) = TIMH + EPSTIM
               IF (.NOT.DOSOUR) FCSOUR = LSOUR
               FCBUF(43) = 1
               CALL TABIO ('WRIT', 0, NNFLAG, FCTIME, FCBUF, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       if both do-antenna need 2nd
               IF ((FLALL1) .AND. (FLALL2)) THEN
                  FCBASL(1) = ANTENS(2)
                  FCBASL(2) = 0
                  NNFLAG = NNFLAG + 1
                  NFLAGD(2) = NFLAGD(2) + 1
                  CALL TABIO ('WRIT', 0, NNFLAG, FCTIME, FCBUF, IRET)
                  IF (IRET.NE.0) GO TO 999
                  FCBASL(1) = ANTENS(1)
                  FCBASL(2) = 0
                  END IF
               TIML = 1.E10
               TIMH = -TIML
               LX = -1
               END IF
C                                       Check flux
            DVAL = YVAL(IX,PLTYPE)
C                                       Clipping on amplitude
            IF (DOBOX) THEN
               IF ((DVAL.LT.FCLIPR(1)) .OR. (DVAL.GT.FCLIPR(2)))
     *            GO TO 100
C                                       interactive clip
            ELSE IF (BRANCH.EQ.7) THEN
               IXX = (IX - IXOFF) * SCALX + FRSTX
               IF (BELOW) THEN
                  IF (DVAL.GT.FLUXCL(IXX)) GO TO 100
               ELSE
                  IF (DVAL.LE.FLUXCL(IXX)) GO TO 100
                  END IF
               END IF
C                                       Determine timerange
            LX = IX
            LSOUR = ISOUR
            TIML = MIN (TIML, TIMES(IX) - DELTAT/2.)
            TIMH = MAX (TIMH, TIMES(IX) + DELTAT/2.)
C                                       Mark as flagged this time
            IF (MFLAGD(IX).EQ.0) THEN
               NFLAGD(1) = NFLAGD(1) + 1
               IF (DOIT) MFLAGD(IX) = FCNUMB
               END IF
            END IF
 100     CONTINUE
C                                       Some timerange left to write
      IF (LX.GT.0) THEN
         NNFLAG = NNFLAG + 1
         NFLAGD(2) = NFLAGD(2) + 1
         FCTIME(1) = TIML - EPSTIM
         FCTIME(2) = TIMH + EPSTIM
         IF (.NOT.DOSOUR) FCSOUR = LSOUR
         FCBUF(43) = 1
         CALL TABIO ('WRIT', 0, NNFLAG, FCTIME, FCBUF, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       if both do-antenna need 2nd
         IF ((FLALL1) .AND. (FLALL2)) THEN
            FCBASL(1) = ANTENS(2)
            FCBASL(2) = 0
            NNFLAG = NNFLAG + 1
            NFLAGD(2) = NFLAGD(2) + 1
            CALL TABIO ('WRIT', 0, NNFLAG, FCTIME, FCBUF, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         END IF
C                                       close down the FC table
      CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IRET)
C
 999  RETURN
      END
      SUBROUTINE IBFMRK (IRET)
C-----------------------------------------------------------------------
C   places the flagging commands of IBLED into the flag table
C   Input from common of DSEL.INC:
C      CATUV   I(256)  Catalogue header for main input file
C      FGVER   I       Version of flag file to write
C      FRQSEL  I       FQ ID being edited
C   Output:
C      IRET    I       error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER REASON*24
      INTEGER   LUN, LUN1, FCRNO, JERR, FLGCNT, KEY(2,2), LCOR0, IROUND,
     *   MMIF, MMCH, MMPOL, IFCBUF(18), NFLAG, NFG, KEYSUB(2,2)
      REAL      TIME, CATR(256), FKEY(2,2), RFCBUF(18)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      EQUIVALENCE (IFCBUF, RFCBUF)
      DATA LUN, LUN1 /29, 16/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
C                                       Check sizes
      NFLAG = 0
      MMPOL = MAX (1, CATBLK(KINAX+JLOCS))
      MMCH = MAX (1, CATBLK(KINAX+JLOCF))
      IF (JLOCIF.GT.0) THEN
         MMIF = MAX (1, CATBLK(KINAX+JLOCIF))
      ELSE
         MMIF = 1
         END IF
      IF ((MMCH.GT.MAXCHA) .OR. (MMIF.GT.MAXIF) .OR.
     *   (MMPOL*MMIF*MMCH.GT.MAXCIF)) THEN
         IRET = 1
         MSGTXT = 'IBFMRK: VISIBILITIES TOO BIG FOR BUFFER'
         GO TO 990
         END IF
C                                       sort the FC table
      KEY(1,1) = 3
      KEY(1,2) = 0
      KEY(2,1) = 0
      KEY(2,2) = 0
      FKEY(1,1) = 1.0
      FKEY(1,2) = 0.0
      FKEY(2,1) = 0.0
      FKEY(2,2) = 0.0
      CALL COPY (256, CATIMG, CATBLK)
      CALL TABSRT (DISKOU, CNOOUT, 'FC', FCVERS, FCVERS, KEY, KEYSUB,
     *   FKEY, FCBUF, CATBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'TABSRT', IRET
         GO TO 990
         END IF
      FLGCNT = 0
      TIME = CATD(KDCRV+1) + (1.0 - CATR(KRCRP+1)) * CATR(KRCIC+1)
      LCOR0 = IROUND (TIME)
      CALL FILL (18, 0, IFCBUF)
C                                       is input UV multisource
      CALL COPY (256, CATUV, CATBLK)
      CALL UVPGET (JERR)
      CALL IBFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG, FCNUMB,
     *   NNFLAG, FCBUF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       write flag table
      WRITE (REASON,1005) TSKNAM, TTIME
      WRITE (MSGTXT,1010) FGVERO
      CALL MSGWRT (4)
C                                       copy old FG table
      IF (FGVERI.GT.0) THEN
         CALL TABCOP ('FG', FGVERI, FGVERO, LUN1, LUN, DISKIN, DISKIN,
     *      CNOIN, CNOIN, CATBLK, BUFF1, FGBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1015) IRET, FGVERI, FGVERO
            GO TO 990
            END IF
         END IF
C                                       loop through flags
      DO 80 FCRNO = 1,NNFLAG
         CALL TABIO ('READ', 0, FCRNO, FCTIME, FCBUF, IRET)
         IF (IRET.LT.0) GO TO 80
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'TABIO READ', IRET
            GO TO 990
            END IF
C                                       Duplicate?
         IF (ABS(FCTIME(1)-RFCBUF(1)).GT.EPSTIM/4.) GO TO 75
         IF (ABS(FCTIME(2)-RFCBUF(2)).GT.EPSTIM/4.) GO TO 75
         IF (FCSFLG.NE.RFCBUF(7)) GO TO 75
         IF (FCBASL(1).NE.IFCBUF(12)) GO TO 75
         IF (FCBASL(2).NE.IFCBUF(13)) GO TO 75
         IF (FCSOUR.NE.IFCBUF(14)) GO TO 75
         IF (FCCHAN(1).NE.IFCBUF(15)) GO TO 75
         IF (FCCHAN(2).NE.IFCBUF(16)) GO TO 75
         IF (FCIF(1).NE.IFCBUF(17)) GO TO 75
         IF (FCIF(2).NE.IFCBUF(18)) GO TO 75
         GO TO 80
C                                       Not duplicate
 75      CALL RCOPY (18, FCTIME, RFCBUF)
         CALL H2CHR (4, 1, FCSFLG, USTFLG)
         CALL CVSTOK (LCOR0, USTFLG, ICOR0, STKFLG)
C                                       Do the flagging
         CALL FLAGUP ('FLAG', LUN, DISKIN, CNOIN, FGVERO, FGBUFF,
     *      IFGRNO, FGKOLS, FGNUMV, FCSOUR, 1, SUBARR, FRQSEL, 1,
     *      FCBASL(1), FCBASL(2), FCTIME(1), FCTIME(2), FCIF(1),
     *      FCIF(2), FCCHAN(1), FCCHAN(2), STKFLG, REASON, NFG, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'FLAGUP', IRET
            GO TO 990
            END IF
         NFLAG = NFLAG + NFG
 80      CONTINUE
      CALL FLAGUP ('CLOS', LUN, DISKIN, CNOIN, FGVERO, FGBUFF, IFGRNO,
     *   FGKOLS, FGNUMV, FCSOUR, 1, SUBARR, FRQSEL, 1, FCBASL(1),
     *   FCBASL(2), FCTIME(1), FCTIME(2), FCIF(1), FCIF(2),
     *   FCCHAN(1), FCCHAN(2), STKFLG, REASON, NFG, IRET)
      CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IRET)
      IRET = 0
      WRITE (MSGTXT,1080) NFLAG
      CALL MSGWRT (4)
C                                       Update CATBLK on disk
      CALL COPY (256, CATBLK, CATUV)
      CALL CATIO ('UPDT', DISKIN, CNOIN, CATUV, 'REST', SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IBFMRK: ',A,' RETURNS ERROR CODE',I6)
 1005 FORMAT (A6,A9,1X,A8)
 1010 FORMAT ('Writing flagging information in FG table ',I3)
 1015 FORMAT ('ERROR',I4,' COPYING FG VER',I3,' TO',I3)
 1020 FORMAT ('IBFMRK: ERROR =',I3,' UPDATING CATBLK')
 1080 FORMAT ('Wrote',I8,' flags in the FG table')
      END
      SUBROUTINE CHKSTK (TSTOK, IRET)
C-----------------------------------------------------------------------
C   Routine to set the pointer into the data array depending on the
C   Stokes string entered by the user..
C   Inputs:
C      TSTOK    C*4   User supplied Stokes value
C   Output:
C      IRET     I     error code, 0=> OK
C                        1 => no match at all, repeat
C                        2 => no match return to menu
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER TSTOK*4
C
      INTEGER   NSTOK, I, J, IROUND, JLSTOK
      REAL      TEMP
      CHARACTER CHSTOK(12)*4
      INCLUDE 'IBLED.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA CHSTOK /'I','Q','U','V', 'RR','LL','RL','LR', 'XX','YY',
     *   'XY','YX'/
C-----------------------------------------------------------------------
      IRET = 0
      NSTOK = 12
C                                       Do we have a match?
      CALL CHLTOU (4, TSTOK)
      DO 10 I = 1,NSTOK
         IF (TSTOK.EQ.CHSTOK(I)) GO TO 20
 10      CONTINUE
      IRET = 1
      WRITE (MSGTXT,1000) TSTOK
      CALL MSGWRT (6)
      GO TO 999
C                                       check I,Q,U,V
 20   IF (ICOR0.GT.0) THEN
         IF ((NCOR.EQ.1) .AND. (I.NE.ICOR0)) THEN
            WRITE (MSGTXT,1020) TSTOK, CHSTOK(ICOR0)
            GO TO 990
            END IF
         DO 30 J = 1,NCOR
            TEMP = (J - CATIR(KRCRP+1)) * CATIR(KRCIC+1) +
     *         CATID(KDCRV+1)
            JLSTOK = IROUND (TEMP)
            IF (I.EQ.JLSTOK) GO TO 900
 30         CONTINUE
C                                      or check RR,LL,RL,LR
      ELSE IF (ICOR0.LT.0) THEN
         I = 4 - I
         IF ((NCOR.EQ.1) .AND. (I.NE.ICOR0)) THEN
            J = ABS(ICOR0) + 4
            WRITE (MSGTXT,1020) TSTOK, CHSTOK(J)
            GO TO 990
            END IF
         DO 40 J = 1,NCOR
            TEMP = (J - CATIR(KRCRP+1)) * CATIR(KRCIC+1) +
     *         CATID(KDCRV+1)
            JLSTOK = IROUND (TEMP)
            IF (I.EQ.JLSTOK) GO TO 900
 40         CONTINUE
         WRITE (MSGTXT,1030) TSTOK
         GO TO 990
         END IF
C                                      found a match
 900  LSTOKS = J
      GO TO 999
C                                       error
 990  CALL MSGWRT (6)
      IRET = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Stokes ',A2,' does not make sense - try again')
 1020 FORMAT ('Stokes ',A2,' requested, only ',A2,' in selected data')
 1030 FORMAT ('Stokes ',A2,' does not match those in the header')
      END
      SUBROUTINE IBTICS (TVCH, BLC, TRC, IMVONN, IXLMIN, LTIC, SCRTCH,
     *   IERR)
C-----------------------------------------------------------------------
C   writes tick marks and tick labels to the TV directly: vertical only
C   Inputs:
C      TVCH     I      Graphics plane to use
C      BLC      R(2)   X AND Y pixels to form bottom left hand
C                      corner of the graph.
C      TRC      R(2)   X and Y pixels to form the top right hand
C                      corner of the graph.
C      IMVONN   I(*)   Buffer of 1's for IMVECT use
C   Outputs:
C      IXLMIN   I      left-most X pixel used
C      LTIC     I      Length of tick mark in pixels
C      SCRTCH   I(*)   scratch buffer.
C      IERR     I      error code: 0 => ok
C                         2 => graph drawing error
C                          => tic algorithm fails
C-----------------------------------------------------------------------
      INTEGER   TVCH, IMVONN(*), IXLMIN, LTIC, SCRTCH(*), IERR
      REAL      BLC(2), TRC(2)
C
      CHARACTER SPRTXT*18, CHDL*4
      DOUBLE PRECISION DEG, DEGC, DTX, DX, DTY, DY, PT5SEC, TICX, TICY,
     *   AYX, DEGC0, DEG0, DEGC1, LDX, LDY, LLDX, LLDY, UPLIM, LOLIM
      REAL      DCX, DSP, DCY, X, Y, XT, YT, TICL, XYRATO, TICT
      INTEGER   LX, LY, BAD, IX(2), IY(2), ILL, IX2L, I, IANGL, ILEN,
     *   ITRY, INCHAR, INOI, IXDIFF, COOTYP, JERR, NERR, HML(2)
      LOGICAL   NONUM, FIRST
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA ILL /0/
C-----------------------------------------------------------------------
      XYRATO = 1
      NERR = 0
      IXLMIN = MAXXTV(1)
      LTIC = 0
      CHDL = '$$$$'
      HML(1) = -100
      HML(2) = -100
C                                       Assign initial values.
      INCHAR = 10
      IERR = 0
      IANGL = 0
      IX2L = -1000
C                                       vertical
      AYX = 0.0D0
      CALL TICINC (2, BLC, TRC, XYRATO, AYX, DEGC, DEG, INOI, TICX,
     *   TICY, TICL, PT5SEC, ITRY, IERR)
      IF (IERR.NE.0) GO TO 999
      UPLIM = 1.E20
      LOLIM = -1.E20
      IF ((AXTYP(LOCNUM).EQ.1) .OR. (AXTYP(LOCNUM).EQ.3)) THEN
         IF ((CORTYP(LOCNUM).EQ.1) .OR. (CORTYP(LOCNUM).EQ.6)) THEN
            UPLIM = 90.0D0
            LOLIM = -90.0D0
            END IF
         IF ((CORTYP(LOCNUM).EQ.2) .OR. (CORTYP(LOCNUM).EQ.5)) THEN
            UPLIM = RPVAL(2,LOCNUM) + 180.0D0
            LOLIM = RPVAL(2,LOCNUM) - 180.0D0
            END IF
         END IF
      DCX = -.5 * CSIZTV(1)
      DCY = -.5 * CSIZTV(2)
      BAD = 0
      IF (AXFUNC(1,LOCNUM).GE.2) BAD = 2 * BAD
      IF ((AXFUNC(1,LOCNUM).EQ.4) .OR. (AXFUNC(1,LOCNUM).EQ.6) .OR.
     *   (AXFUNC(1,LOCNUM).EQ.8)) BAD = 2 * BAD
      IF (ABS(AXINC(1,LOCNUM)).GE.0.1) BAD = BAD * 1.51
      IF (ABS(AXINC(1,LOCNUM)).LE.0.005) BAD = BAD / 2
      TICL = (MAXXTV(1) + MAXXTV(2)) / 67.0
      TICL = MIN (TICL, 0.12 * (TRC(1)-BLC(1)))
      TICX = ABS (AXINC(1,LOCNUM) * TICL) / 1.2
      TICX = TICX / (BAD + 1.)
      TICL = TICL / (BAD + 1.)
      CALL TICCOR (TICX, TICY)
C                                       Determine possible tic intervls
      NONUM = (CPREF(2,LOCNUM).EQ.' ') .AND. (CTYP(2,LOCNUM).EQ.' ')
      LDX = -1.D10
      LLDX = -1.D10
      LDY = -1.D10
      LLDY = -1.D10
      COOTYP = 2
C                                       Draw tic marks and values.
      DEGC0 = DEGC
      FIRST = .TRUE.
      DEG0 = DEG
      DEGC1 = DEGC0 - DEG
      DO 290 I = 1,INOI
         DY = DEGC
         CALL FNDX (BLC(1), DY, DX, JERR)
         IF (JERR.NE.0) GO TO 285
C                                       Convert degrees to pixels.
         CALL XYPIX (DX, DY, X, Y, JERR)
         IF (JERR.NE.0) GO TO 285
         IF ((X.LT.BLC(1)-0.01) .OR. (X.GT.TRC(1)+0.01)) GO TO 285
         IF ((Y.LT.BLC(2)-0.01) .OR. (Y.GT.TRC(2)+0.01)) GO TO 285
         LX = X + 0.5
         LY = Y + 0.5
         IF (DX.NE.LDX) LLDX = LDX
         IF (DY.NE.LDY) LLDY = LDY
         LDX = DX
         LDY = DY
C                                       Find end of tic.
         DTX = DX + SIGN (1.0, AXINC(1,LOCNUM)) * TICX
         DTY = DY + SIGN (1.0, AXINC(2,LOCNUM)) * TICY
         CALL XYPIX (DTX, DTY, XT, YT, JERR)
         IF (JERR.NE.0) GO TO 235
         TICT = SQRT ((XT-X)**2 + (YT-Y)**2)
         IF (TICL.LE.0.) TICL = 1.
         IF ((TICT.LE.TICL) .AND. (TICT.GE.0.1*TICL)) GO TO 221
            IF (TICT.LE.0.0) GO TO 235
            DTX = DX + SIGN (1.0, AXINC(1,LOCNUM)) * TICX * TICL / TICT
            DTY = DY + SIGN (1.0, AXINC(2,LOCNUM)) * TICY * TICL / TICT
            CALL XYPIX (DTX, DTY, XT, YT, JERR)
            IF (JERR.NE.0) GO TO 235
 221     IF ((XT.LT.BLC(1)-0.01) .OR. (XT.GT.TRC(1)+0.01)) GO TO 235
         IF ((YT.LT.BLC(2)-0.01) .OR. (YT.GT.TRC(2)+0.01)) GO TO 235
C                                       Draw tick
         IX(2) = XT + 0.5
         IY(2) = YT + 0.5
         IX(1) = LX
         IY(1) = LY
         IXDIFF = IX(2) - IX(1)
         IXDIFF = IXDIFF / 4
         IX(2) = IX(1) + IXDIFF
         LTIC = MAX (LTIC, IX(2) - IX(1))
         CALL IMVECT (TVCH, 2, IX, IY, IMVONN, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IF (IERR.EQ.2) THEN
            NERR = NERR + 1
            IF (NERR.GT.10) GO TO 980
            END IF
         DEGC1 = DEGC - DEG
         IF (FIRST) DEGC0 = DEGC
         FIRST = .FALSE.
         IXLMIN = MIN (IXLMIN, IX(1))
         IXLMIN = MIN (IXLMIN, IX(2))
C                                       Convert degrees to DEC/RA.
 235     IF (.NOT.NONUM) THEN
            CALL TICSTR (ITRY, DEGC, PT5SEC, 0, 0, CHDL, HML, SPRTXT,
     *         ILEN)
            DSP = DCX - ILEN
            IX(1) = LX + DCX - ILEN*CSIZTV(1) + 0.5
            IY(1) = LY + DCY + 0.5
            IX(2) = IX(1) + ILEN*CSIZTV(1) - 1
            IY(2) = IY(1) + CSIZTV(2) - 1
            IF ((IX(1).LT.1) .OR. (IY(1).LT.1) .OR. (IX(2).GT.MAXXTV(1))
     *         .OR. (IY(2).GT.MAXXTV(2))) GO TO 285
               IX2L = IX(2)
               CALL IMCHAR (TVCH, IX, IY, IANGL, ILL, SPRTXT(:ILEN),
     *            SCRTCH, IERR)
               IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
               IF (IERR.EQ.2) THEN
                  NERR = NERR + 1
                  IF (NERR.GT.10) GO TO 980
                  END IF
               IXLMIN = MIN (IXLMIN, IX(1))
            END IF
 285     DEGC = DEGC - DEG
 290     CONTINUE
C                                       Draw tics for other side.
C                                       Same intervals but not
C                                       necessarily same values.
      CALL TICINC (6, BLC, TRC, XYRATO, AYX, DEGC, DEG, INOI, TICX,
     *   TICY, TICL, PT5SEC, ITRY, IERR)
      IF (IERR.NE.0) GO TO 395
      IF (INOI.LE.0) GO TO 395
C                                       Loop for other border.
      DO 390 I = 1,INOI
         DY = DEGC
         CALL FNDX (TRC(1), DY, DX, JERR)
         IF (JERR.NE.0) GO TO 380
C                                       Convert degrees to pixels.
         CALL XYPIX (DX, DY, X, Y, JERR)
         IF (JERR.NE.0) GO TO 380
         IF ((X.LT.BLC(1)-0.01) .OR. (X.GT.TRC(1)+0.01)) GO TO 380
         IF ((Y.LT.BLC(2)-0.01) .OR. (Y.GT.TRC(2)+0.01)) GO TO 380
         LX = X + 0.5
         LY = Y + 0.5
C                                       Find end of tic.
         DTX = DX - SIGN (1.0, AXINC(1,LOCNUM)) * TICX
         DTY = DY - SIGN (1.0, AXINC(2,LOCNUM)) * TICY
         CALL XYPIX (DTX, DTY, XT, YT, JERR)
         IF (JERR.NE.0) GO TO 380
         TICT = SQRT ((XT-X)**2 + (YT-Y)**2)
         IF ((TICT.GT.TICL) .OR. (TICT.LT.0.1*TICL)) THEN
            IF (TICT.LE.0.0) GO TO 380
            DTX = DX - SIGN (1.0, AXINC(1,LOCNUM)) * TICX * TICL/TICT
            DTY = DY - SIGN (1.0, AXINC(2,LOCNUM)) * TICY * TICL/TICT
            CALL XYPIX (DTX, DTY, XT, YT, JERR)
            IF (JERR.NE.0) GO TO 380
            END IF
         IF ((XT.LT.BLC(1)-0.01) .OR. (XT.GT.TRC(1)+0.01)) GO TO 380
         IF ((YT.LT.BLC(2)-0.01) .OR. (YT.GT.TRC(2)+0.01)) GO TO 380
C                                       Simple ticks
         IX(2) = XT + 0.5
         IY(2) = YT + 0.5
         IX(1) = LX
         IY(1) = LY
         IXDIFF = IX(1) - IX(2)
         IXDIFF = IXDIFF / 4
         IX(2) = IX(1) - IXDIFF
         LTIC = MAX (LTIC, IX(2) - IX(1))
         CALL IMVECT (TVCH, 2, IX, IY, IMVONN, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IF (IERR.EQ.2) THEN
            NERR = NERR + 1
            IF (NERR.GT.10) GO TO 980
            END IF
 380     DEGC = DEGC - DEG
 390     CONTINUE
 395  IERR = 0
      GO TO 999
C                                       Graph drawing error.
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (7)
      IERR = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('IBTICS: GRAPH LABEL WRITING ERROR. IERR =',I5)
      END
      SUBROUTINE DADRAW (CHAN, SCALX, XDATA, YDATA, VECVAL, IERR)
C-----------------------------------------------------------------------
C   DADRAW draws the data point to the main plot.  This is a simple call
C   to IMVECT with the ability to expand the point horizontally.
C   Inputs:
C      CHAN    I      channel number (1 to NGRAY+NGRAPH)
C      SCALX   I      expansion factor of plot
C      XDATA   I      X coordinates X1,X2,...
C      YDATA   I(2)   Y coordinates Y1,Y2,...
C      VECVAL  I(*)   scratch buffer (size MAXXTV), ALREADY FILLED
C   Output:
C      IERR    I       error code of ZM70XF - 0 => ok;  2 => input error
C-----------------------------------------------------------------------
      INTEGER   CHAN, SCALX, XDATA, YDATA(2), VECVAL(*), IERR
C
      INTEGER   XXDATA(2), I, J
C-----------------------------------------------------------------------
      IERR = 0
      IF ((YDATA(1).GT.0) .AND. (YDATA(2).GT.0)) THEN
         J = (SCALX + 1) / 2
         J = MIN (J, 6)
         XXDATA(1) = XDATA - 1
         XXDATA(2) = XDATA - 1
         DO 10 I = 1,J
            XXDATA(1) = XXDATA(1) + 1
            XXDATA(2) = XXDATA(2) + 1
            CALL IMVECT (CHAN, 2, XXDATA, YDATA, VECVAL, IERR)
            IF (IERR.NE.0) GO TO 999
 10         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE IMVECT (CHAN, COUNT, XDATA, YDATA, VECVAL, IERR)
C-----------------------------------------------------------------------
C   IMVECT writes a connected sequence of line segments on a TV channel
C   calling YCNECT.  Special IBLED version to avoid refilling the buffer
C   with max intensity points or zeros all the time.  VECVAL must be
C   filled first with at least the line length worth of values.
C   Inputs:
C      CHAN    I       channel number (1 to NGRAY+NGRAPH)
C      COUNT   I       number of X,Y pairs ( > 1)
C      XDATA   I(COUNT)      X coordinates X1,X2,...
C      YDATA   I(COUNT)      Y coordinates Y1,Y2,...
C      VECVAL  I(*)    scratch buffer (size MAXXTV), ALREADY FILLED
C   Output:
C      IERR    I       error code of ZM70XF - 0 => ok;  2 => input error
C-----------------------------------------------------------------------
      INTEGER   CHAN, COUNT, XDATA(*), YDATA(*), VECVAL(*), IERR
C
      INTEGER   IDX, NWORDS, X, Y, X1, Y1, IER
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       check inputs
      IERR = 2
      IF ((CHAN.LT.1) .OR. (CHAN.GT.NGRAY+NGRAPH)) GO TO 999
      IF (COUNT.LT.2) GO TO 999
      IERR = 0
      IER = 0
C                                       init buffer values
      IDX = 1
      NWORDS = COUNT
C                                       first point in a line
 20   IF (NWORDS.GE.IDX+1) THEN
         X = XDATA(IDX)
         Y = YDATA(IDX)
         IDX = IDX + 1
         IF ((X.LT.1) .OR. (X.GT.MAXXTV(1)) .OR. (Y.LT.1) .OR.
     *      (Y.GT.MAXXTV(2))) THEN
            IERR = 2
            GO TO 20
            END IF
C                                       second point in a line
 30      IF (NWORDS.GE.IDX) THEN
            X1 = X
            Y1 = Y
            X = XDATA(IDX)
            Y = YDATA(IDX)
            IDX = IDX + 1
            IF ((X.LT.1) .OR. (X.GT.MAXXTV(1)) .OR. (Y.LT.1) .OR.
     *         (Y.GT.MAXXTV(1))) THEN
               IERR = 2
               GO TO 20
C                                       draw a line segment
            ELSE
               CALL YCNECT (X1, Y1, X, Y, CHAN, VECVAL, IER)
               IF (IER.EQ.0) GO TO 30
               END IF
            END IF
         END IF
      IERR = MAX (IERR, IER)
C
 999  RETURN
      END
      SUBROUTINE READCC (LSTOK, LNAME2, LCLAS2, IN2SEQ, IN2DIS, INVER,
     *   SOUWAN, LUNB, JBUFFA, JBUFFB, IRET)
C----------------------------------------------------------------------
C   Read CC model component information into common
C   Inputs:
C      LSTOK        C*6        Stokes type to load (symbols allowed:
C                              (I, Q, U, V, R, L)
C      LNAME2       C*12       Model file name
C      LCLAS2       C*6        Model file class
C      IN2SEQ       I          Model file sequence number
C      IN2DIS       I          Model file disk
C      INVER        I          CC table version number
C      LUNB         I          LUN to use for table I/O
C      JBUFFA       I(1024)    I/O buffer
C      JBUFFB       I(512)     I/O buffer
C   Output to common:
C      /MODCC/
C   Output:
C      IRET         I          Return code (0=> ok; else error)
C----------------------------------------------------------------------
      CHARACTER LSTOK*6, LNAME2*12, LCLAS2*6
      INTEGER IN2SEQ, IN2DIS, INVER, LUNB, JBUFFA(*), JBUFFB(*),
     *   SOUWAN(*), IRET
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER LCLASS*6
      DOUBLE PRECISION DMAPR, DF, DAM, DAN, DTA, DTB, XRA,  XDEC
      REAL      CATCLR(256), X, Y, FLUX, PARMS(3), XYZ(3), XP(3),
     *   UMAT(3,3), PMAT(3,3), XXOFF, YYOFF, ZZOFF
      INTEGER   CATCLN(256), NREC, ISTOK, IMAP, IERR, ICCRNO,
     *   CCKOLS(MAXCCC), CCNUMV(MAXCCC), I, IREC, ITYPE, NUMCOL, MSGSAV,
     *   NITER(MAXFLD), BITER(MAXFLD), JTRIM, MODEL, METHOD
      LOGICAL   WASOME
      INCLUDE 'IBLED.INC'
      INCLUDE 'DMOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATCLN, CATCLR)
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      IMNEXT = 0
      CALL FILL (MAXFLD, 0, NITER)
      CALL FILL (MAXFLD, 0, BITER)
      MSGSAV = MSGSUP
      WASOME = .FALSE.
      DO 10 IMAP = 1,NMAPS
         IF (NCOMP(I).GT.0) WASOME = .TRUE.
 10      CONTINUE
C                                       Loop over Stokes type requested
      NCSTOK = JTRIM (LSTOK)
      DO 800 ISTOK = 1,NCSTOK
         LCSTOK(ISTOK) = LSTOK(ISTOK:ISTOK)
         IMSTRT(ISTOK) = IMNEXT + 1
         IF (ISTOK.NE.1) IMEND(ISTOK-1) = IMNEXT
C                                       Construct file name
         IF (NCSTOK.GT.0) THEN
            LCLASS = LSTOK(ISTOK:ISTOK) // LCLAS2(2:6)
C                                       Single field
         ELSE
            LCLASS = LCLAS2
            END IF
         MSGSUP = 32000
         MODEL = 1
         METHOD = -1
         CALL SETGDS (DISKIN, CNOIN, LNAME2, LCLASS, IN2SEQ, IN2DIS,
     *      NMAPS, INVER, NITER, BITER, MODEL, METHOD, JBUFFA, JBUFFB,
     *      I, IERR)
         MSGSUP = MSGSAV
C                                      Model file not found
         IF (IERR.EQ.3) GO TO 800
         FACGRD(1) = 1.0
         CALL FACSET (DISKIN, CNOIN, 1, SOUWAN(1), 1, 1.0, IERR)
C                                       Error opening model file
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT,1200) IERR, LNAME2, LCLASS
            GO TO 990
            END IF
         WRITE (MSGTXT,1225) LNAME2, LCLASS
         CALL MSGWRT (5)
C                                       Loop over fields
         DO 600 IMAP = 1,NMAPS
C                                       Read catalog block
            CALL CATIO ('READ', CCDISK(IMAP), CCCNO(IMAP), CATCLN,
     *         'REST', JBUFFA, IERR)
            IF (IERR.NE.0) GO TO 600
C                                       set rest parameters
            CALL GRDAT (.FALSE., IMAP, CATR, JBUFFA, IERR)
            IF (IERR.NE.0) GO TO 600
C                                       Set field center offsets.
            XXOFF = DXCG(IMAP) * CCROT + DYCG(IMAP) * SSROT
            YYOFF = DYCG(IMAP) * CCROT - DXCG(IMAP) * SSROT
            ZZOFF = DZCG(IMAP)
            IF (DO3DIM) THEN
               CALL XYSHFT (RA, DEC, XSHIFT(IMAP), YSHIFT(IMAP),
     *            MAPROT, XRA, XDEC)
               CALL PRJMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT,
     *            PMAT)
               END IF
C                                       Open CC table
            CALL CCMINI ('READ', JBUFFB, CCDISK(IMAP), CCCNO(IMAP),
     *         INVER, CATCLN, LUNB, ICCRNO, CCKOLS, CCNUMV, NUMCOL,
     *         IERR)
            IF (IERR.NE.0) THEN
               IRET = 2
               WRITE (MSGTXT,1250) IERR
               GO TO 990
               END IF
C
            NREC = MIN (JBUFFB(5), NCOMP(IMAP))
            IF ((NREC.EQ.0) .AND. (.NOT.WASOME)) NREC = JBUFFB(5)
C                                       Loop over CC components
            DO 500 IREC = 1,NREC
               ICCRNO = IREC
               CALL TABCCM ('READ', JBUFFB, ICCRNO, CCKOLS, CCNUMV,
     *            NUMCOL, X, Y, FLUX, ITYPE, PARMS, IERR)
               IF (IERR.NE.0) THEN
                  IRET = 3
                  WRITE (MSGTXT,1300) IERR
                  GO TO 990
                  END IF
C                                       Cut off
               IF (ABS(FLUX).LT.LIMFLX) GO TO 505
               IF ((NONEG) .AND. (FLUX.LT.0.0)) GO TO 505
C                                       Load components
               IMNEXT = IMNEXT + 1
               IF (IMNEXT.GT.MXCC) THEN
                  IRET = 4
                  WRITE (MSGTXT,1400)
                  GO TO 990
                  END IF
C
               XP(1) = (X + XPOFF(IMAP)) * DG2RAD * TWOPI
               XP(2) = (Y + YPOFF(IMAP)) * DG2RAD * TWOPI
               IF (DO3DIM) THEN
                  XP(3) = 0.0
                  CALL PRJMUL (2, XP, UMAT, XYZ)
               ELSE
                  XYZ(1) = CCROT * XP(1) + SSROT * XP(2)
                  XYZ(2) = CCROT * XP(2) - SSROT * XP(1)
                  XYZ(3) = 0.0
                  END IF
               CCPOS(1,IMNEXT) = XYZ(1) + XXOFF
               CCPOS(2,IMNEXT) = XYZ(2) + YYOFF
               CCPOS(3,IMNEXT) = XYZ(3) + ZZOFF
               SFLUX(IMNEXT) = FLUX * FACGRD(1)
               GAUSA(IMNEXT) = 0.0
               GAUSB(IMNEXT) = 0.0
               GAUSC(IMNEXT) = 0.0
C                                       Gaussian or spherical comp.
               IF ((ITYPE.EQ.1).OR.(ITYPE.EQ.3)) THEN
C                                       Convert to radians
                  GAUSA(IMNEXT) = PARMS(1) * DG2RAD
                  GAUSB(IMNEXT) = PARMS(2) * DG2RAD
                  GAUSC(IMNEXT) = PARMS(3) * DG2RAD
                  DOGAUS = (ITYPE.EQ.1)
                  DOSPHE = (ITYPE.EQ.3)
                  END IF
C
 500           CONTINUE
C                                       Close CC file
 505        CALL TABIO ('CLOS', 0, ICCRNO, JBUFFB, JBUFFB, IERR)
            IF (IERR.NE.0) THEN
               IRET = 5
               WRITE (MSGTXT,1500) IERR
               GO TO 990
               END IF
600         CONTINUE
800      CONTINUE
      IF (ISTOK.NE.1) IMEND(ISTOK-1) = IMNEXT
      CALL UNSETG (JBUFFB)
C                                       Convert Gaussian parameters
      IF (DOGAUS) THEN
         DF = SQRT (2 * LOG (2.0D0))
         DO 850 I = 1, IMNEXT
            DAM = COS ((GAUSC(I) + DMAPR) * DG2RAD)
            DAN = SIN ((GAUSC(I) + DMAPR) * DG2RAD)
            DTA = (GAUSA(I) * PI / DF) ** 2
            DTB = (GAUSB(I) * PI / DF) ** 2
            GAUSA(I) = -(DTA * DAM * DAM + DTB * DAN * DAN)
            GAUSB(I) = -((DTB - DTA) * DAN * DAM)
            GAUSC(I) = -(DTA * DAN * DAN + DTB * DAM * DAM)
 850        CONTINUE
         END IF
C                                       Convert sphere parameters
      IF (DOSPHE) THEN
         DO 875 I = 1, IMNEXT
            GAUSA(I) = GAUSA(I) * TWOPI
 875        CONTINUE
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C                                       Exit
 999  RETURN
C----------------------------------------------------------------------
 1200 FORMAT ('READCC: ERROR',I3,' OPENING ',A12,'.',A6)
 1225 FORMAT ('Found model file: ',A12,'.',A6)
 1250 FORMAT ('READCC: ERROR',I3,' RETURNED BY CCMINI')
 1300 FORMAT ('READCC: ERROR',I3,' RETURNED BY TABCCM')
 1400 FORMAT ('READCC: TOO MANY CC COMPONENTS; DECREASE NCOMP')
 1500 FORMAT ('READCC: ERROR',I3,' CLOSING CC TABLE')
      END
      SUBROUTINE MODCLC (U, V, W, LSTYPE, DAMP, DPHAS, IRET)
C----------------------------------------------------------------------
C   Compute model amplitude and phase
C   Inputs:
C      U        R       (u,v,w) coordinates
C      V        R
C      W        R
C      LSTYPE   C*2     Desired Stokes type (eg. 'I', 'RR', 'RL')
C   Outputs:
C      DAMP     D       Model amplitude
C      DPHAS    D       Model phase (deg)
C      IRET     I       Return code (0=>ok, else error)
C----------------------------------------------------------------------
      CHARACTER LSTYPE*2
      DOUBLE PRECISION DAMP, DPHAS
      REAL U, V, W
      INTEGER IRET
C
      INCLUDE 'INCS:PSTD.INC'
      INTEGER MAXTYP
      PARAMETER (MAXTYP = 3)
      CHARACTER LSRCH(MAXTYP)
      DOUBLE PRECISION DRE(MAXTYP), DIM(MAXTYP), DRSUM, DISUM, DCSUM,
     *   DSSUM, DAA, DARG, DTEMP, DFTEMP
      INTEGER NSRCH, ISRCH, JSTOK, IPTR(MAXTYP), J
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'DMOD.INC'
C----------------------------------------------------------------------
C                                       Intialization
      IRET = 0
      DAMP = 0.0D0
      DPHAS = 0.0D0
C                                       Decode Stokes type
C                                       Case LSTYPE of:
      NSRCH = 0
      IF (LSTYPE.EQ.'I') THEN
         LSRCH(1) = 'I'
         LSRCH(2) = 'R'
         LSRCH(3) = 'L'
         NSRCH = 3
      ELSE IF (LSTYPE.EQ.'RR') THEN
         LSRCH(1) = 'R'
         LSRCH(2) = 'I'
         LSRCH(3) = 'V'
         NSRCH = 3
      ELSE IF (LSTYPE.EQ.'LL') THEN
         LSRCH(1) = 'L'
         LSRCH(2) = 'I'
         LSRCH(3) = 'V'
         NSRCH = 3
         END IF
C                                       Search for Stokes type in
C                                       model component table
      DO 200 ISRCH = 1, NSRCH
         IPTR(ISRCH) = 0
         DO 150 JSTOK = 1, NCSTOK
            IF ((LCSTOK(JSTOK).EQ.LSRCH(ISRCH)).AND.
     *         (IMSTRT(JSTOK).LE.IMEND(JSTOK))) IPTR(ISRCH) = JSTOK
150         CONTINUE
200      CONTINUE
C                                       Decide which Stokes type
C                                       to compute
      IF (((LSTYPE.EQ.'I').AND.(IPTR(1).GT.0)).OR.
     *   ((LSTYPE.EQ.'R').AND.(IPTR(1).GT.0)).OR.
     *   ((LSTYPE.EQ.'L').AND.(IPTR(1).GT.0))) THEN
            IPTR(2) = 0
            IPTR(3) = 0
            END IF
C                                       Compute model
      CALL DFILL (MAXTYP, DBLANK, DRE)
      CALL DFILL (MAXTYP, DBLANK, DIM)
C                                       Loop over Stokes type
      DO 500 JSTOK = 1, NSRCH
C                                       Skip if not wanted
         IF (IPTR(JSTOK).LE.0) GO TO 500
C                                       Loop over components
         DCSUM = 0.0D0
         DSSUM = 0.0D0
         DO 400 J = IMSTRT(JSTOK), IMEND(JSTOK)
C                                       Case component type of:
C                                       1: Point
            IF ((.NOT.DOSPHE).AND.(.NOT.DOGAUS)) THEN
               DTEMP = U * CCPOS(1,J) + V * CCPOS(2,J) + W * CCPOS(3,J)
               DCSUM = DCSUM + SFLUX(J) * COS (DTEMP)
               DSSUM = DSSUM + SFLUX(J) * SIN (DTEMP)
               END IF
C                                       2: Gaussian
            IF (DOGAUS) THEN
               DARG = U * U * GAUSA(J) + U * V * GAUSB(J) +
     *            V * V * GAUSC(J)
               IF (DARG.GT.-8.0) THEN
                  DFTEMP = SFLUX(J) * EXP (DARG)
                  DTEMP = U * CCPOS(1,J) + V * CCPOS(2,J) + W *
     *               CCPOS(3,J)
                  DCSUM = DCSUM + DFTEMP * COS (DTEMP)
                  DSSUM = DSSUM + DFTEMP * SIN (DTEMP)
                  END IF
               END IF
C                                       3: Sphere (trap v. unresolved)
            IF (DOSPHE) THEN
               DAA = GAUSA(J) * SQRT (U * U + V * V)
               DAA = MAX (DAA, 6.28D-2)
               DFTEMP = 3.0D0 * SFLUX(J) *
     *            ((SIN (DAA) / (DAA * DAA * DAA)) -
     *            COS (DAA) / (DAA * DAA))
               DTEMP = U * CCPOS(1,J) + V * CCPOS(2,J) + W * CCPOS(3,J)
               DCSUM = DCSUM + DFTEMP * COS (DTEMP)
               DSSUM = DSSUM + DFTEMP * SIN (DTEMP)
               END IF
400         CONTINUE
C                                       Save (Re,Im) parts
         DRE(JSTOK) = DCSUM
         DIM(JSTOK) = DSSUM
500      CONTINUE
C                                       Construct final Stokes type
C                                       (Only I supported at present)
      IF (LSTYPE.EQ.'I') THEN
         DRSUM = DBLANK
         DISUM = DBLANK
         IF (IPTR(1).GT.0) THEN
C                                       I
            DRSUM = DRE(1)
            DISUM = DIM(1)
         ELSE IF ((IPTR(2).GT.0).AND.(IPTR(3).GT.0)) THEN
C                                       I= 0.5 (R + L)
            DRSUM = 0.5 * (DRE(2) + DRE(3))
            DISUM = 0.5 * (DIM(2) + DIM(3))
         ELSE IF (IPTR(2).GT.0) THEN
C                                       I= R (assume V=0)
            DRSUM = DRE(2)
            DISUM = DIM(2)
         ELSE IF (IPTR(3).GT.0) THEN
C                                       I= L (assume V=0)
            DRSUM = DRE(3)
            DISUM = DIM(3)
            END IF
         END IF
C                                       Amplitude and phase
      IF ((DRSUM.NE.DBLANK).AND.(DISUM.NE.DBLANK)) THEN
         DAMP = SQRT (DRSUM * DRSUM + DISUM * DISUM)
         IF (DAMP.GT.0.0) THEN
            DPHAS = ATAN2 (DISUM, DRSUM) / DG2RAD
         ELSE
            DPHAS = 0.0D0
            END IF
         END IF
      GO TO 999
C
999   RETURN
      END
