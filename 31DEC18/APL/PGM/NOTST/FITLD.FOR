LOCAL INCLUDE 'FITLD.INC'
C                                       Local include for FITLD
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:DCAT.INC'
      DOUBLE PRECISION PSCAL(20), POFF(20), BSC, BZE, WTSCAL, BXSTA(50),
     *   BYSTA(50), BZSTA(50)
      HOLLERITH XNAMOU(3), XCLAOU(2), XINFIL(12), XOPTYP, XXSOUR(4,30),
     *   XOPCOD, XANTNA(2,50), XREFDA(2)
      CHARACTER NAMOUT*12, CLAOUT*6, INFILE*48, OPTYPE*4, XSOUR(30)*16,
     *   XCALCO*4, OPCODE*4, NAMSTA(50)*8, PTYPES(40)*8, SFILE*48,
     *   KOUTCL*6, KOUTNM*12, INSTRU*24, CORREL*24, REFDAY*8
      REAL     TAPE, XFILES, XDOUVC, RFILES, XDOTAB, XDOCON, DOKEEP,
     *   XPIECE, XERROR, CLINT, XQUAL, TIMER(8), XBCHAN, XECHAN, XBIF,
     *   XEIF, XDOCOR, XSELB, XSELF, XFRQTL, THRESH, DOWGT
      REAL      XOUTS, DISO, CORVER, PCMATX(7,7), CDMATX(7,7),
     *   PVMATX(7,7)
      INTEGER   IBLANK, NUMVIS, FDVEC(50), TBIND, LENREC, NFILES,
     *   DISOUT, TAPEIN, GROUP, ICEND, TABLES, IBPP, MFILES, NPARMS,
     *   ITAB(20), JADR(4), NSTACT, SCRBUF(256), KLOCWT, TAPBUF(29184),
     *   IDLUN, IDFIND, FILSIZ, LRECO, FSTVIS, KOUTS, PRTLV, SETDEB,
     *   NMREAD, NMRDUN, SELQUA, NEXPRT, BCHAN, ECHAN, BIF, EIF, XINCS,
     *   XINCF, XINCIF, MAJERR, DOCORR, OUTSEQ, CONCAT, IPIECE, NPIECE,
     *   NPV(4), USED(300), CATSAV(256), DPIECE, KEEP, NZERO(3,2),
     *   PBASE, PANT1, PANT2, NSKIP
      INTEGER   IHDISK, IHCNO, CATHIS(256), HISSCR, IHLUN, IHBLK(256)
      LOGICAL   ISBLNK, LCMPLX, LSTOKE, DODISK, DOUVCM, IMAGE,
     *   DOTABL, BINTAB, DOCONC, DELCOR, PRTPOL, DELVT, UVTABL
C
      INTEGER   IBLNK, NTAPE, NBPIX, CNO, TAPEBP, UNKNWN
      LOGICAL   FUCKUP
      CHARACTER FITBLK*2880
      DOUBLE PRECISION POS11(2), SCALE, OFFSET, ISCALE, IZERO
C
      INTEGER   NXBUFF(512), NXKOLS(MAXNXC), NXNUMV(MAXNXC), LUNNX,
     *   NXVER, INXRNO, NXSOUR, NXSUBA, NXFSTV, NXLSTV, NXFQ
      REAL      NXTIME, NXDT
C
      COMMON /BUFRS/ TAPBUF, SCRBUF
      COMMON /INPARM/ TAPE, XFILES, XINFIL, XNAMOU, XCLAOU, XOUTS, DISO,
     *   XOPTYP, RFILES, XDOTAB, XDOUVC, XDOCON, DOKEEP, XPIECE, XERROR,
     *   CLINT, XXSOUR, XQUAL, TIMER, XBCHAN, XECHAN, XBIF, XEIF,
     *   XDOCOR, XREFDA, XSELB, XSELF, XFRQTL, THRESH, DOWGT, XOPCOD,
     *   XANTNA
      COMMON /SCRINF/ NUMVIS, NFILES, DISOUT, MFILES,
     *   TAPEIN, LENREC, FDVEC, TBIND
      COMMON /HISINF/ IHLUN, IHBLK, IHDISK, IHCNO, CATHIS, HISSCR,
     *   KOUTS
      COMMON /FITINF/ CATSAV, PSCAL, POFF, BSC, BZE, WTSCAL, CORVER,
     *   GROUP, ICEND, IBLANK, TABLES, IBPP, NPARMS, ITAB, JADR,
     *   NSTACT, KLOCWT, DODISK, IDLUN, IDFIND, FILSIZ, LRECO, FSTVIS,
     *   PRTLV, NMREAD, NMRDUN, SELQUA, NEXPRT, BCHAN, ECHAN, BIF, EIF,
     *   XINCS, XINCF, XINCIF, MAJERR,
     *   DOCORR, SETDEB, ISBLNK, LCMPLX, LSTOKE, DOUVCM,
     *   IMAGE,  DOTABL, BINTAB, DOCONC, DELCOR, PRTPOL, DELVT, OUTSEQ,
     *   IPIECE, NPIECE, NPV, CONCAT, UVTABL, PCMATX, CDMATX, PVMATX,
     *   USED, DPIECE, KEEP, NZERO, PBASE, PANT1, PANT2, NSKIP
      COMMON /STACOM/ BXSTA, BYSTA, BZSTA
      COMMON /CHRCOM/ NAMOUT, CLAOUT, INFILE, OPTYPE, OPCODE, NAMSTA,
     *   PTYPES, SFILE, KOUTNM, KOUTCL, XSOUR, XCALCO, INSTRU, CORREL,
     *   REFDAY
C
      COMMON /MLTAP/ SCALE, OFFSET, ISCALE, IZERO, POS11, IBLNK, FUCKUP,
     *   NTAPE, CNO, NBPIX, TAPEBP, UNKNWN
      COMMON /MLCHR/ FITBLK
C
      COMMON /NXSTUF/ NXBUFF, NXKOLS, NXNUMV, LUNNX, NXVER, INXRNO,
     *   NXSOUR, NXSUBA, NXFSTV, NXLSTV, NXFQ, NXTIME, NXDT
LOCAL END
LOCAL INCLUDE 'PFITLD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXFQI, LMXCHA, LMXCIF, LTARSZ
C                                       MAXFQI=max number FREQ ID's
      PARAMETER (MAXFQI = MAXFQ)
C                                       check the sizes carefully in
C                                       SCRTCH.INC below
C                                       MAXCHA for data read by FITLD
      PARAMETER (LMXCHA=MAXCHA)
C                                       MAXCIF for data read by FITLD
C                                       max (#pols * #chans * #IFs)
      PARAMETER (LMXCIF=MAXCIF)
C                                       LTARSZ = max local table size
C                                       is for BP tables (+6 extra)
      PARAMETER (LTARSZ=XBPRSZ)
C                                       and more
      INTEGER   MAXSUB, MAXSOU, MAXNCL
C                                       Allow up to 20 subarrays
      PARAMETER (MAXSUB=20)
      PARAMETER (MAXSOU=5000)
C                                       Max # columns allowed in any
C                                       table
      PARAMETER (MAXNCL = 100)
LOCAL END
LOCAL INCLUDE 'ALLOW.INC'
C
C     Information about files that match the user's output file
C     specifications (OUTNAME, OUTCLASS, OUTSEQ, and OUTDISK). The file
C     to which FITS-IDI data will be appended is chosen from the files
C     listed here by comparing header keywords in the FITS-IDI tables to
C     the characteristics of these files.
C
C     Initialized by: BTCAT
C     Modified by:    BTFHDR, UT2UV, BTFND, and FITLD
C
C     MXALL     The maximum number of output files in the list
C     ALLNO     The actual number of output files in the list
C               0 <= ALLNO <= MXALL
C     ALLDSK    The disk number of the output files
C               1 <= ALLDSK(1:ALLNO) <= NVOL
C     ALLCNO    The catalogue numbers of the output files
C               1 <= ALLCNO(1:ALLNO)
C     ALLNAM    The names of the output files
C     ALLCLS    The classes of the output files
C     ALLSEQ    The sequence numbers of the output files
C     ALLCAT    The catalogue headers for the output files
C     ALLFRQ    The reference frequencies for the output files in Hz.
C               Defined to be the value associated with the OLDRFQ
C               keyword if it is present in the header or the reference
C               value for the FREQ axis if it is not
C     ALLBW     The frequency channel increments for the FREQ axis in
C               the output files in Hz.
C     ALLRFC    The reference channel number for each output file,
C               defined to be the reference pixel for the FREQ axis
C     ALLNCH    The number of pixels on the FREQ axis for each output
C               file.
C     ALLNIF    The number of pixels on the IF axis if there is one or
C               1 if there is no IF axis for each output file
C     ALLNST    The number of pixels on the STOKES axis for each output
C               file.
C     ALLSTK    The reference value for the STOKES axis for each output
C               file
C     ALLNUM    The number of visibilities in each output file
C     ALLFQN    The number of frequency IDs for each output file, zero
C               where there is no FQ table
C     ALLFQS    A list of reference frequencies for the frequency IDs
C               in each output file
C     ALLMOD    A flag for each output file which is true if and only
C               if that file has been modifed by FITLD
C     ALLHIS    A flag for each output file which is true if and only
C               if a history file has been added to that file by FITLD.
C     CURALL    The index of the current output file
C               0 <= CURALL <= ALLNO
C
C
      INCLUDE 'INCS:PUVD.INC'
C
      INTEGER   MXALL
      PARAMETER (MXALL = 100)
C
      INTEGER   ALLNO, ALLDSK(MXALL), ALLCNO(MXALL), ALLNCH(MXALL),
     *   ALLNIF(MXALL), ALLNST(MXALL), ALLSTK(MXALL), ALLNUM(MXALL),
     *   ALLCAT(256,MXALL), ALLSEQ(MXALL), ALLFQN(MXALL), CURALL
      LOGICAL   ALLMOD(MXALL), ALLHIS(MXALL)
      REAL      ALLBW(MXALL), ALLRFC(MXALL)
      CHARACTER ALLNAM(MXALL)*12, ALLCLS(MXALL)*6
      DOUBLE PRECISION ALLFRQ(MXALL), ALLFQS(MAXFQ, MXALL)
      COMMON /ALLCOM/ ALLFRQ, ALLFQS, ALLBW, ALLRFC, ALLNO, CURALL,
     *   ALLDSK, ALLCNO, ALLNCH, ALLNIF, ALLNST, ALLSTK,
     *   ALLNUM, ALLCAT, ALLSEQ, ALLFQN, ALLMOD, ALLHIS
      COMMON /ALLCHR/ ALLNAM, ALLCLS
LOCAL END
LOCAL INCLUDE 'ORDER.INC'
      INCLUDE 'PFITLD.INC'
      DOUBLE PRECISION ADJRFQ, TABRFQ
      INTEGER FQINUM, DATSID(MAXIF,MAXFQI), FORDER(MAXIF,MAXFQI), FQIMAX
      LOGICAL LSBPRS, REORDR(MAXFQI)
      COMMON /FRQSWT/ ADJRFQ, TABRFQ, DATSID, FORDER, FQINUM, FQIMAX,
     *   LSBPRS, REORDR
LOCAL END
LOCAL INCLUDE 'DATSEL.INC'
      INCLUDE 'PFITLD.INC'
      INTEGER   CIMVER
C                                       Current revision of IM table
      PARAMETER (CIMVER = 2)
C
      INTEGER   PHASED, ANTMAP(MAXSUB,MAXANT), MXANTN(MAXSUB),
     *   SRCMAP(MAXSOU), MXSRCN, SOUWAN(MAXSOU), NSOUWD,
     *   SRCQUL(MAXSOU), IMROST, SOCOUN(MAXSOU), FQMAP(MAXFQI),
     *   FQSIDE(MAXIF,MAXFQI), MXFREX, NEWFQI, FQDUPS(MAXFQI),
     *   NUMACC, FQCOUN(MAXFQI), FRFQV(MAXFQI), SLCHNS, ORIGCH,
     *   ORLREC, SLIFS, ORIGIF, COLSEL(MAXNCL), FQINCO(MAXFQI),
     *   TMPMAP(MAXSUB,MAXANT), ANTYPE, NANTNM
      CHARACTER ANTNAM(MAXSUB,MAXANT)*8, SRCNAM(MAXSOU)*16,
     *   SRCCAL(MAXSOU)*4, TSRCN(MAXSOU)*16, USRANM(60)*8,
     *   TMPNAM(MAXSUB,MAXANT)*8, FQBCOD(MAXIF,MAXFQI)*8, USRNAM(50)*8
      LOGICAL   ANTLOD, ANTCHA, SRCLOD, SRCCHA, DOAMAP, DOSMAP, DOFMAP,
     *   DOSWNT, TIMSEL, ALLSKP, FRTLOD, FRTCHA, DOFSEL, REREF, DELEX,
     *   WRTFQ(MAXFQI), GOTSUB, GOTFUB, CHNSLT, IFSLT, MULINT, UNSORT,
     *   RFQDUP
      REAL      TSTART, TEND, LSTTIM, FQCHW(MAXIF,MAXFQI),
     *   FQTBW(MAXIF,MAXFQI), SELBAN, FRQTOL, OLDSBT(MAXSUB),
     *   OLDSBB(MAXSUB), CONTIM(3)
      DOUBLE PRECISION CURJLD, REFOFF, FQFREQ(MAXIF,MAXFQI), SELFRQ,
     *   FRVLS(MAXFQI), TRANGE(2)
      COMMON /MAPF1/ CURJLD, REFOFF, FQFREQ, SELFRQ, FRVLS, TRANGE,
     *   CONTIM, TSTART, TEND, LSTTIM, FQCHW, FQTBW, SELBAN, FRQTOL,
     *   OLDSBT, OLDSBB,
     *   ANTMAP, MXANTN, SRCMAP, MXSRCN, FQCOUN, FRFQV, SOUWAN,
     *   NSOUWD, SRCQUL, IMROST, SOCOUN, FQMAP, FQDUPS, FQSIDE, MXFREX,
     *   NEWFQI, SLCHNS, ORIGCH, ORLREC, SLIFS, ORIGIF, COLSEL, FQINCO,
     *   NUMACC, ANTLOD, ANTCHA, SRCLOD, SRCCHA, FRTLOD, FRTCHA,
     *   DOAMAP, DOSMAP, DOFMAP, DOSWNT, TIMSEL, ALLSKP, DOFSEL, REREF,
     *   WRTFQ, GOTSUB, GOTFUB, CHNSLT, IFSLT, MULINT, UNSORT, PHASED,
     *   DELEX, TMPMAP, RFQDUP, ANTYPE, NANTNM
      COMMON /MAPFC1/ ANTNAM, SRCNAM, SRCCAL, TSRCN, USRANM, TMPNAM,
     *   FQBCOD, USRNAM
LOCAL END
LOCAL INCLUDE 'DIGCOR.INC'
      INCLUDE 'PFITLD.INC'
      DOUBLE PRECISION OFFDAY
      INTEGER FFTSIZ, OVRSMP, ZEROPD, NLEVS(MAXANT), CURA1, CURA2,
     *   MPOL, MIF, MFRQ, PARIT(MAXIF), TWDVER
      CHARACTER TAPER*8, TWIDDL*8
      REAL    FFT0(LMXCHA), FFT1(LMXCHA)
      LOGICAL DOPR, ISVLBA
      COMMON /DIGI/ OFFDAY, FFTSIZ, OVRSMP, ZEROPD, TWDVER, NLEVS,
     *   CURA1, CURA2, MPOL, MIF, MFRQ, PARIT, FFT0, FFT1, DOPR, ISVLBA
      COMMON /DIGC/ TAPER, TWIDDL
LOCAL END
LOCAL INCLUDE 'TABLES.INC'
      INTEGER   TABUF1(512), TABUF2(512), CATIEQ(256), TDATP1(128,2),
     *   TDATP2(128,2)
      HOLLERITH CATHEQ(256)
      REAL      CATREQ(256)
      DOUBLE PRECISION CATDEQ(128)
      EQUIVALENCE (CATIEQ, CATHEQ, CATREQ, CATDEQ)
      COMMON /TABLES/ TABUF1, TABUF2, CATIEQ, TDATP1, TDATP2
LOCAL END
LOCAL INCLUDE 'SCRTCH.INC'
      INCLUDE 'PFITLD.INC'
      REAL      SCRDAT(4*LMXCHA), SCRWRK(4*LMXCHA)
      REAL      RECORD(LTARSZ), TRECR(LTARSZ)
      INTEGER   TRECI(LTARSZ)
      HOLLERITH TRECH(LTARSZ)
      LOGICAL   TRECL(LTARSZ)
      DOUBLE PRECISION TRECD(LTARSZ/2)
      EQUIVALENCE (TRECI, TRECR, TRECH, TRECL, TRECD)
C                                       check the sizes carefully
      EQUIVALENCE (TRECI, SCRDAT), (RECORD, SCRWRK)
      COMMON /SCRTCH/ TRECR, RECORD
LOCAL END
LOCAL INCLUDE 'SCRBFS.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      REAL      UVBUFF(UVBFSL), TUVBUF(UVBFSL), RECR(UVBFSL)
      COMMON /SCRBFS/ UVBUFF, TUVBUF, RECR
LOCAL END
LOCAL INCLUDE 'DUVV.INC'
      INCLUDE 'PFITLD.INC'
C                                       Generic data types
      INCLUDE 'INCS:PTAB.INC'
C                                       UV definitions
      INTEGER   MAXUVC, NKEYUV, MAXWUV
C                                       # cols in UV table
      PARAMETER (MAXUVC=12)
C                                       # keywords in UV table
      PARAMETER (NKEYUV=14)
C                                       max # words in header array
      PARAMETER (MAXWUV=13)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row.
      INTEGER   KUCR, KVCR, KWCR, KDAT, KTIM, KBAS, KARR, KSOU,
     *   KFRE, KTSP, KDWT, KSPC
C                                       UV specific keywords
      INTEGER   IUVRNO
C      CHARACTER RRDATE*8
      REAL      VISSCL
C                                       UV specific column variables
      INTEGER   IARRAY, IFQID, ISRC, IBASEL
      REAL      U, V, W, TSPAN, WEIGHT(MAXIF,4)
      DOUBLE PRECISION DATE, TIME
C                                       Generic table variables
      INTEGER   NKEY, NREC, NBUF, DATP(128,2), NCOL, NTT, IPOINT
      INTEGER   UVKOLS(MAXUVC), UVNUMV(MAXUVC), DTYP(MAXUVC)
      INTEGER   KLOCS(NKEYUV), KEYTYP(NKEYUV)
      INTEGER   KEYVAL(MAXWUV)
      REAL      KEYVAR(MAXWUV)
      HOLLERITH KEYVAH(MAXWUV)
      CHARACTER  KEYW(NKEYUV)*8, TITLE(MAXUVC)*24, UNITS(MAXUVC)*8,
     *   TTITLE*56, WTTYPE*8, GLBEQU*8
      EQUIVALENCE (KEYVAL, KEYVAR, KEYVAH)
C
      COMMON /URPPOS/ KUCR, KVCR, KWCR, KDAT, KTIM, KBAS, KARR, KSOU,
     *   KFRE, KTSP, KDWT, KSPC
      COMMON /URTABV/ NKEY, NREC, NBUF, DATP, NCOL, NTT, IPOINT,
     *   UVKOLS, UVNUMV, DTYP, KLOCS, KEYTYP, KEYVAL
      COMMON /URVALS/ DATE, TIME, VISSCL, U, V, W, TSPAN, WEIGHT,
     *   IUVRNO, IARRAY, IFQID, ISRC, IBASEL
C      COMMON /URCHAR/ RRDATE
      COMMON /URTABC/ WTTYPE, GLBEQU, KEYW, TITLE, UNITS, TTITLE
LOCAL END
LOCAL INCLUDE 'DFLT.INC'
C                                       Correlator filter parameters
C                                       Requires PUVD.INC
      REAL RCQINT(MAXCID)
      INTEGER ICQCOR(MAXCID), ICQFLT(MAXCID), CQBUFF(512), NCQCOR
      COMMON /CRFILT/ RCQINT, ICQCOR, ICQFLT, CQBUFF, NCQCOR
LOCAL END
LOCAL INCLUDE 'DWRV.INC'
C                                       (WR) table.
C                                       Generic data types
      INCLUDE 'INCS:PTAB.INC'
C                                       WR definitions
      INTEGER MAXWRC, NKEYWR, NKYWR2, NKYWWR, NKY2WR, MXSPWR, MXDPWR,
     *        IWRREV
C                                       # cols in WR table
      PARAMETER (MAXWRC = 12)
C                                       # keywords in WR table
      PARAMETER (NKEYWR = 3)
C                                       2 x # keywords
      PARAMETER (NKYWR2 = NKEYWR * 2)
C                                       word length of keyword array
      PARAMETER (NKYWWR = 5)
C                                       half word length of keyw array
      PARAMETER (NKY2WR = NKYWWR / 2 + 1)
C                                       Logical record length
C                                       in single precision words
      PARAMETER (MXSPWR = 14)
C                                       in double precision words
      PARAMETER (MXDPWR = MXSPWR / 2)
C                                       Current table revision no.
      PARAMETER (IWRREV = 2)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row. Third character
C                                       gives var. type (D=double;
C                                       R=real; I=integer).
      INTEGER WRDTIM, WRRINT, WRIANT, WRRTMP, WRRPRS, WRRDWP, WRRVEL,
     *   WRRDIR, WRRGUS, WRRPRE, WRRH2O, WRRION
C                                       WRDTIM = Time
      PARAMETER (WRDTIM = 1)
C                                       WRRINT = Integration time
      PARAMETER (WRRINT = WRDTIM + 1)
C                                       WRIANT = antenna
      PARAMETER (WRIANT = WRRINT + 1)
C                                       WRRTMP = surface temperature
      PARAMETER (WRRTMP = WRIANT + 1)
C                                       WRRPRS = surface pressure
      PARAMETER (WRRPRS = WRRTMP + 1)
C                                       WRRDWP = dew point
      PARAMETER (WRRDWP = WRRPRS + 1)
C                                       WRRVEL = wind velocity
      PARAMETER (WRRVEL = WRRDWP + 1)
C                                       WRRDIR = wind direction
      PARAMETER (WRRDIR = WRRVEL + 1)
C                                       WRRGUS = wind gust
      PARAMETER (WRRGUS = WRRDIR + 1)
C                                       WRRPRE = precipitation
      PARAMETER (WRRPRE = WRRGUS + 1)
C                                       WRRH2O = water column
      PARAMETER (WRRH2O = WRRPRE + 1)
C                                       WRRION = ion column
      PARAMETER (WRRION = WRRH2O + 1)
C                                       End include DWRV.INC
LOCAL END
      PROGRAM FITLD
C-----------------------------------------------------------------------
C! Read uv and image data from FITS format file
C# UV Tape FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2005, 2007-2018
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
C   FITLD will load a uv data base or an image from a FITS tape.
C   Inputs:
C      Adverb  Pgm. name         Comments.
C      INTAPE    TAPEIN     Input tape drive number
C      NFILES    NFILES     Number of files to skip.
C      INFILE    INFILE     Disk file name for FITS file
C      OUTNAME   NAMOUT     Output file name (default = NAMEIN)
C      OUTCLASS  CLAOUT     Output file class (default = 'UVDATA')
C      OUTSEQ    OUTSEQ     Output sequence number requested (<0 =>
C                           use one found on FITS tape header)
C      OUTDISK   DISOUT     Output disk volumn.
C      NPIECE    NPIECE     Maximum piece number to be loaded (uv table)
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER PRGM*6, ROUTIN*6, KEYVAL*16, CHTM12*12, CHTM6*6,
     *   CHTM2*2, BADKEY(2)*8
      INTEGER   IERR, JERR, ISLOT, IROUND, I, MFIL, ICARD, INDEST,
     *   DUM, K, VBVER, KBUFF(256), ITRIM, J, JDEST, NKEY
      LOGICAL   FITS, EOF, SKIP, IBTCAT, LAST
      INCLUDE 'FITLD.INC'
      INCLUDE 'ALLOW.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'SCRBFS.INC'
      DATA PRGM /'FITLD '/
      DATA NKEY, BADKEY /2, 'SYSNAME1', 'SYSNAME2'/
C-----------------------------------------------------------------------
      IDLUN = 16
      IHLUN = 27
      CHTM12 = ' '
      CHTM6 = ' '
      CHTM2 = ' '
      DUM = 0
      IBTCAT = .FALSE.
      MAJERR = 0
      CORVER = 0.0
C                                       Get input parameters
      CALL FTLIN (PRGM, FITS, IERR)
      IF (IERR.NE.0) GO TO 900
      UNKNWN = 0
      DPIECE = 0
C                                       How many files to load?
      NMREAD = IROUND (RFILES)
      IF (NMREAD.LE.0) THEN
         IF (DODISK) THEN
            NMREAD = 1
         ELSE
            NMREAD = 10000
            END IF
         END IF
C                                       Loop and load
      I = 1
 10   CONTINUE
         DOCONC = .FALSE.
C                                       Is this an image or uv data
C                                       or IDI correlator data
         CALL DECFIT (FITS, IERR)
C                                       Deal with end of tape
         IF ((IERR.EQ.4) .OR. (IERR.EQ.6)) THEN
            MSGTXT = 'Unexpected EOF or EOI encountered'
            IF (RFILES.GE.0.5) THEN
               CALL MSGWRT (6)
C                                       Print warning polzn labels?
            ELSE IF (BINTAB .AND. PRTPOL) THEN
               MSGTXT = '       !!!!!!!!!!!!!!!!!!!!!!!!!!!         '
               CALL MSGWRT (6)
               MSGTXT = 'The Stokes labelling is inconsistent between'
               CALL MSGWRT (6)
               MSGTXT = 'files. This happens when non-VLBA stations are'
               CALL MSGWRT (6)
               MSGTXT = 'involved in single-polarization observations.'
               CALL MSGWRT (6)
               MSGTXT = 'No harm results from this. If you really want'
               CALL MSGWRT (6)
               MSGTXT = 'the correct labels run PUTHEAD'
               CALL MSGWRT (6)
               MSGTXT = '       !!!!!!!!!!!!!!!!!!!!!!!!!!!         '
               CALL MSGWRT (6)
               END IF
            IERR = 0
            GO TO 850
            END IF
         IF (IERR.NE.0) GO TO 850
C                                       non-FITS file?
         IF (.NOT.FITS) THEN
            MSGTXT = 'Encountered non-FITS file, will skip'
            CALL MSGWRT (6)
            IF (.NOT.DODISK) THEN
               NEXPRT = NEXPRT + 1
               MFIL = 1
               CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40), MFIL, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1010) IERR
                  CALL MSGWRT (8)
                  GO TO 850
                  END IF
               END IF
            GO TO 100
            END IF
C                                       Do we want this type?
         IF ((IMAGE .AND. (OPTYPE(1:2).EQ.'UV')) .OR.
     *       ((.NOT.IMAGE) .AND. (OPTYPE(1:2).EQ.'IM'))) THEN
            MSGTXT = 'FITS file of wrong type encountered, skipping'
            CALL MSGWRT (6)
            IF (.NOT.DODISK) THEN
               MFIL = 1
               CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40), MFIL, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1015) IERR
                  CALL MSGWRT (8)
                  GO TO 850
                  END IF
               END IF
            GO TO 100
            END IF
         CALL CATINI (CATBLK)
         CATBLK(KIIMU) = NLUSER
         KEEP = 0
C                                       If Image then....
         IF (IMAGE) THEN
            CALL FILL (300, 0, USED)
            OUTSEQ = IROUND (XOUTS)
            CALL IMGTAP (IERR)
            IF (IERR.NE.0) THEN
               ROUTIN = 'IMGTAP'
               WRITE (MSGTXT,1015) IERR, ROUTIN
               CALL MSGWRT (8)
               GO TO 850
               END IF
C                                       If UV data then....
         ELSE IF (.NOT.BINTAB) THEN
C                                       Random group FITS
C                                       Default names
            OUTSEQ = IROUND (XOUTS)
            CALL H2CHR (12, 1, XNAMOU, NAMOUT)
            CALL H2CHR (6, 1, XCLAOU, CLAOUT)
            IF ((NMREAD.GT.1) .AND. (I.GT.1) .AND. (NAMOUT.NE.' ') .AND.
     *         (CLAOUT.NE.' ')) OUTSEQ = 0
            IPIECE = 0
            DPIECE = 0
            NPIECE = IROUND (XPIECE)
            IF (NPIECE.LE.0) NPIECE = 999
            IF (NMREAD.GT.1) NPIECE = 999
            IF (.NOT.UVTABL) NPIECE = 1
            CONCAT = -1
            NSKIP = 0
C           IF ((UVTABL) .AND. (XDOCON.GT.0.0)) CONCAT = 0
 20         IPIECE = IPIECE + 1
            IF (IPIECE.LE.NPIECE) THEN
               TABLES = 0
               LAST = IPIECE.GE.NPIECE
               CALL FILL (300, 0, USED)
               CALL COPY (256, CATBLK, CATSAV)
               CALL UVFHDR (ISLOT, IERR)
               IF (IERR.NE.0) THEN
                  ROUTIN = 'UVFHDR'
                  WRITE (MSGTXT,1015) IERR, ROUTIN
                  CALL MSGWRT (8)
                  GO TO 850
                  END IF
               CALL PCHDR (PCMATX, CDMATX, PVMATX)
               CALL UVFHIS (ISLOT, IERR)
               IF (IERR.NE.0) THEN
                  ROUTIN = 'UVFHIS'
                  WRITE (MSGTXT,1015) IERR, ROUTIN
                  CALL MSGWRT (8)
                  GO TO 850
                  END IF
               CALL FILL (6, 0, NZERO)
               IF (.NOT.UVTABL) THEN
                  CALL UVFDAT (ISLOT, IERR)
                  IF (IERR.NE.0) THEN
                     ROUTIN = 'UVFDAT'
                     WRITE (MSGTXT,1015) IERR, ROUTIN
                     CALL MSGWRT (8)
                     GO TO 850
                     END IF
                  END IF
C                                       Try table extension files
               CALL FITRXU (ISLOT, LAST, EOF, IERR)
               IF (IERR.NE.0) THEN
                  ROUTIN = 'FITRXU'
                  WRITE (MSGTXT,1015) IERR, ROUTIN
                  CALL MSGWRT (8)
                  GO TO 850
                  END IF
C                                       Old table format.
               IF (.NOT.EOF) THEN
                  CALL UVFEXT (ISLOT, IERR)
                  IF (IERR.NE.0) THEN
                     ROUTIN = 'UVFEXT'
                     WRITE (MSGTXT,1015) IERR, ROUTIN
                     CALL MSGWRT (8)
                     GO TO 850
                     END IF
                  END IF
C                                       loop for table parts
               CONCAT = 1
               IF (UVTABL) GO TO 20
               END IF
C                                       SU table check
            CALL SUCHCK (DISOUT, ISLOT)
C                                       SU table check
            CALL NXCHCK (DISOUT, ISLOT, UVBUFF)
C                                       Remove 'WRIT' status of
C                                       output file.
            IF (I.LT.NMREAD) THEN
               CALL CATDIR ('CSTA', DISOUT, ISLOT, CHTM12, CHTM6,
     *            DUM, CHTM2, DUM, 'CLWR', SCRBUF, IERR)
               IERR = 0
               END IF
         ELSE
            IF (NMREAD.EQ.0) NMREAD = 10000
C                                       Try to concatanate ?
            DOCONC = XDOCON.GT.0.0
            NMRDUN = I
            CONTIM(1) = -1000.0
            CONTIM(2) = -1000.0
C                                       Set up list of allowed
C                                       files in case is VLBA format
            IF ((DOCONC) .AND. (.NOT.IBTCAT)) THEN
               CALL BTCAT (DISOUT, NAMOUT, CLAOUT, OUTSEQ, DOUVCM, IERR)
               IF (IERR.NE.0) GO TO 850
               IBTCAT = .TRUE.
               END IF
C                                       VLBA binary tables
C                                       Reposition file
            CALL TAPIO ('BAKF', FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) GO TO 850
            CALL BTFHIS (IERR)
            IF (IERR.NE.0) THEN
               ROUTIN = 'BTFHIS'
               WRITE (MSGTXT,1015) IERR, ROUTIN
               CALL MSGWRT (8)
               GO TO 850
               END IF
C                                       Position tape at first
C                                       extension table
            CALL FINCRD (FDVEC, TBIND, TAPBUF, FITBLK, ICARD,
     *         'XTENSION', KEYVAL, EOF, IERR)
            IF (IERR.NE.0) THEN
               ROUTIN = 'FINCRD'
               WRITE (MSGTXT,1015) IERR, ROUTIN
               CALL MSGWRT (8)
               GO TO 850
               END IF
            CALL BTRTAB (ISLOT, EOF, IERR)
            IF (DODISK .AND. ((IERR.EQ.12) .OR. (IERR.EQ.13))) IERR = 0
            IF (IERR.NE.0) THEN
               ROUTIN = 'BTRTAB'
               WRITE (MSGTXT,1015) IERR, ROUTIN
               CALL MSGWRT (8)
               SKIP = .FALSE.
C                                       IERR=15 means BTFND error,
C                                       could not find unique file to
C                                       attach to. Die gracefully.
               IF (IERR.EQ.15) THEN
                  GO TO 850
                  END IF
C                                       IERR=12 means data out of
C                                       time order, die gracefully.
C                                       IERR=13 means day # screwed
C                                       up, also die gracefully.
               IF ((IERR.EQ.12) .OR. (IERR.EQ.13)) THEN
                  SKIP = .TRUE.
                  IERR = 0
                  IF (CATBLK(KIGCN).GT.0) GO TO 55
                  END IF
C                                       Remove tables, since will
C                                       not be entering BT2AIP which
C                                       usually does this, must do it
C                                       here
               VBVER = 0
               CALL RMEXT (DISOUT, ISLOT, 'AG', VBVER, CATBLK,
     *            SCRBUF, JERR)
               CALL RMEXT (DISOUT, ISLOT, 'FR', VBVER, CATBLK,
     *            SCRBUF, JERR)
               CALL RMEXT (DISOUT, ISLOT, 'SO', VBVER, CATBLK,
     *            SCRBUF, JERR)
               CALL RMEXT (DISOUT, ISLOT, 'BT', VBVER, CATBLK,
     *            SCRBUF, JERR)
               CALL RMEXT (DISOUT, ISLOT, 'BC', VBVER, CATBLK,
     *            SCRBUF, JERR)
               CALL RMEXT (DISOUT, ISLOT, 'FL', VBVER, CATBLK,
     *            SCRBUF, JERR)
               CALL RMEXT (DISOUT, ISLOT, 'CA', VBVER, CATBLK,
     *            SCRBUF, JERR)
               CALL RMEXT (DISOUT, ISLOT, 'UT', VBVER, CATBLK,
     *            SCRBUF, JERR)
               IF (SKIP) THEN
                  MSGTXT = 'Tape was advanced over suspect file'
                  CALL MSGWRT (6)
                  GO TO 60
                  END IF
               GO TO 850
               END IF
C                                       Construct AIPS tables
 55         CALL BT2AIP (ISLOT, IERR)
            IF (IERR.NE.0) THEN
               ROUTIN = 'BT2AIP'
               WRITE (MSGTXT,1015) IERR, ROUTIN
               CALL MSGWRT (8)
               GO TO 850
               END IF
C                                       # vis warning
 60         IF (DOCONC .AND. (CATBLK(KIGCN).LE.0)) THEN
               MSGTXT = '************ WARNING ************'
               CALL MSGWRT (6)
               MSGTXT = '   Zero visibilities loaded'
               CALL MSGWRT (6)
               MSGTXT = '*********************************'
               CALL MSGWRT (6)
               END IF
C                                       Deal with history
            IF ((I.EQ.1) .OR. (.NOT.DOCONC) .OR. (.NOT.ALLHIS(CURALL)))
     *         THEN
               CALL HISCOP (IHLUN, IDLUN, IHDISK, DISOUT, IHCNO, ISLOT,
     *            CATBLK, SCRBUF, KBUFF, IERR)
               CALL HICLOS (IDLUN, .TRUE., KBUFF, IERR)
               ALLHIS(CURALL) = .TRUE.
            ELSE
               CALL HIOPEN (IHLUN, IHDISK, IHCNO, SCRBUF, IERR)
               CALL HIOPEN (IDLUN, DISOUT, ISLOT, KBUFF, IERR)
               CALL HICOPY (IHLUN, SCRBUF, IDLUN, KBUFF, IERR)
               CALL HICLOS (IHLUN, .TRUE., SCRBUF, IERR)
               CALL HICLOS (IDLUN, .TRUE., KBUFF, IERR)
               END IF
            CALL KEYPCP (IHDISK, IHCNO, DISOUT, ISLOT, NKEY, BADKEY,
     *         IERR)
            CALL COPY (256, CATBLK, ALLCAT(1,CURALL))
C                                      Destroy HI scratch file
            DO 65 K = 1,NSCR
               FRW(K) = 2
 65            CONTINUE
            CALL COPY (256, CATBLK, KBUFF)
            CALL MAPCLR (NSCR, SCRVOL, SCRCNO, FRW, SCRBUF)
            CALL COPY (256, KBUFF, CATBLK)
C                                      Remove 'WRIT' status of
C                                      output file.
            CALL CATDIR ('CSTA', DISOUT, ISLOT, CHTM12, CHTM6, DUM,
     *         CHTM2, DUM, 'CLWR', SCRBUF, IERR)
            IF (IERR.EQ.10) IERR = 0
            IF (IERR.NE.0) THEN
               ROUTIN = 'CATDIR'
               WRITE (MSGTXT,1015) IERR, ROUTIN
               CALL MSGWRT (8)
               GO TO 850
               END IF
            END IF
C                                       Print header when necessary
         IF (.NOT.BINTAB) THEN
            IF (KEEP.GE.0) THEN
               CALL IMQUIT
            ELSE
               MSGTXT = 'Destroying dummy image'
               CALL MSGWRT (3)
               CALL MDESTR (DISOUT, CNO, CATBLK, SCRBUF, JDEST, IERR)
               END IF
            END IF
         IF (BINTAB .AND. (I.EQ.NMREAD)) THEN
C                                       Print warning polzn labels?
            IF (PRTPOL) THEN
               MSGTXT = '       !!!!!!!!!!!!!!!!!!!!!!!!!!!         '
               CALL MSGWRT (6)
               MSGTXT = 'The Stokes labelling is inconsistent between'
               CALL MSGWRT (6)
               MSGTXT = 'files. This happens when non-VLBA stations are'
               CALL MSGWRT (6)
               MSGTXT = 'involved in single-polarization observations.'
               CALL MSGWRT (6)
               MSGTXT = 'No harm results from this. If you really want'
               CALL MSGWRT (6)
               MSGTXT = 'the correct labels run PUTHEAD'
               CALL MSGWRT (6)
               MSGTXT = '       !!!!!!!!!!!!!!!!!!!!!!!!!!!         '
               CALL MSGWRT (6)
               END IF
             CALL IMQUIT
             END IF
C                                       Should the file be put down?
         IF (BINTAB .AND. (.NOT.DOCONC) .AND.
     *      (CATBLK(KIGCN).LE.0)) THEN
            MSGTXT = 'Sorry this file has no data - it must die'
            CALL MSGWRT (6)
            INDEST = 0
            CALL MDESTR (DISOUT, ISLOT, CATBLK, SCRBUF, INDEST, IERR)
            END IF
         IF (NMREAD.GT.1) THEN
            WRITE (MSGTXT,1000) I
            CALL MSGWRT (4)
            END IF
C                                       Did we have a major, but
C                                       recoverable error?
C                                       MAJERR = 1 => could not expand
C                                       file, ran out of disc space.
 100     IF (MAJERR.LE.0) THEN
            I = I + 1
            IF (I.LE.NMREAD) THEN
               IF (DODISK) THEN
                  CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
                  J = ITRIM (INFILE)
                  IF (I-1.GT.9) J = J - 1
                  IF (I-1.GT.99) J = J - 1
                  IF (I.LE.9) THEN
                     WRITE (INFILE(J:),1101) I
                  ELSE IF (I.LE.99) THEN
                     WRITE (INFILE(J:),1102) I
                  ELSE
                     WRITE (INFILE(J:),1103) I
                     END IF
                  CALL CHR2H (48, INFILE, 1, FDVEC(7))
                  CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1100) IERR, I
                     CALL MSGWRT (7)
                     IERR = 0
                     GO TO 855
                     END IF
                  END IF
               GO TO 10
               END IF
            END IF

C                                       Close input
 850  CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, JERR)
 855  IF (BINTAB .AND. (IERR.EQ.0)) THEN
C                                       Rationalize frequencies for
C                                       VLBA
         CALL FRQFID (DISOUT, ISLOT, IERR)
         IF (IERR.NE.0) THEN
            ROUTIN = 'FRQFID'
            WRITE (MSGTXT,1015) IERR, ROUTIN
            CALL MSGWRT (8)
            GO TO 900
            END IF
         CALL SUTIDY (DISOUT, ISLOT, IERR)
         IF (IERR.NE.0) THEN
            ROUTIN = 'SUTIDY'
            WRITE (MSGTXT,1015) IERR, ROUTIN
            CALL MSGWRT (8)
            GO TO 900
            END IF
         IF (I.LT.NMREAD) CALL IMQUIT
C                                       Does the file have any data?
         IF (CATBLK(KIGCN).LE.0) THEN
            MSGTXT = 'Sorry this file has no data - it must die'
            CALL MSGWRT (6)
            INDEST = 0
            CALL MDESTR (DISOUT, ISLOT, CATBLK, SCRBUF, INDEST, IERR)
            END IF
         END IF
C
      IF (NEXPRT.GT.0) THEN
         WRITE (MSGTXT,1030) NEXPRT
         CALL MSGWRT (4)
         END IF
C                                       Finished, some error handling
 900  IF (IERR.EQ.12) IERR = 0
      IF (IERR.EQ.13) IERR = 0
      IF (IERR.EQ.14) IERR = 0
      IF (IERR.EQ.15) IERR = 0
      IF (UNKNWN.GT.0) THEN
         WRITE (MSGTXT,1900) UNKNWN
         CALL MSGWRT (6)
         END IF
      CALL DIE (IERR, SCRBUF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT (10('-'),' FILE ',I3,' LOADED ',10('-'))
 1010 FORMAT ('ERROR',I7,' POSITIONING TAPE')
 1015 FORMAT ('ERROR',I7,' RETURNED FROM ',A6)
 1030 FORMAT ('Skipped ',I5,' non-FITS files')
 1100 FORMAT ('ERROR',I4,' opening disk file number',I4)
 1101 FORMAT (I1)
 1102 FORMAT (I2)
 1103 FORMAT (I3)
 1900 FORMAT ('WARNING:',I7,' unknown records skipped while reading',
     *   ' tape')
      END
      SUBROUTINE FTLIN (PRGM, FITS, JERR)
C-----------------------------------------------------------------------
C   FTLIN reads input parameters for Task FITLD.  See header comments
C   in PROGRAM FITLD for more details.
C   Inputs: PRGM   C*6      Program name
C   Output: FITS   L        T => FITS tape
C           JERR   I        Error code: 0 => ok, else quit
C   Common: /MAPHDR/ initial cat block header (heavily filled in if
C                    FITS is false)
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, CHTMP*400, TRYTWO*48, ANVLBA(10)*2,
     *   VLITE*8, EHEX*36
      LOGICAL   FITS, T
      INTEGER   IFIL, JERR, IERR, IRET, IT, J, NBYT, I, IROUND, ITRIM,
     *   IANT, II
      REAL      EPS
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'ALLOW.INC'
      INCLUDE 'DUVV.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T /.TRUE./
      DATA ANVLBA /'BR','FD','HN','KP','LA','MK','NL','OV','PT','SC'/
      DATA EHEX /'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C-----------------------------------------------------------------------
C                                       Initialize I/O
      CALL ZDCHIN (T)
      CALL HIINIT (3)
      CALL VHDRIN
      CALL FILL (50, 0, FDVEC)
      JERR = 0
      BSC = 1.0D0
      BZE = 0.0D0
C                                       Initialize COMMON /CFILES/
      NSCR = 0
      NCFILE = 0
      NEXPRT = 0
      ALLNO = 0
      CURALL = 0
      VISSCL = 0.0
      WTTYPE = ' '
      GLBEQU = ' '
C                                       Get input parameters.
      NPARMS = 272
      CALL GTPARM (PRGM, NPARMS, RQUICK, TAPE, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRET = 8
         GO TO 20
         END IF
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      CALL H2CHR (8, 1, XREFDA, REFDAY)
      XCALCO = ' '
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      DO 5 I = 1,30
         XSOUR(I) = ' '
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
5        CONTINUE
C                                       Initialize
      CALL FILL (MAXSUB, -1, MXANTN(1))
      DO 7 I = 1,MAXANT
         DO 6 II = 1,MAXSUB
            ANTNAM(II,I) = ' '
            ANTMAP(II,I) = 0
            TMPNAM(II,I) = ' '
            TMPMAP(II,I) = 0
 6          CONTINUE
 7       CONTINUE
      IT = 51
      ANTYPE = 0
      DO 11 I = 1,50
         CALL H2CHR (8, 1, XANTNA(1,I), USRNAM(I))
         IF (USRNAM(I).EQ.' ') IT = MIN (IT, I)
 11      CONTINUE
      NANTNM = IT
      IF (USRNAM(1).NE.' ') ANTYPE = 3
      IF (USRNAM(1).EQ.'VLITE') ANTYPE = 1
      IF (USRNAM(1).EQ.'VLBA') ANTYPE = 2
      IANT = 0
      MXANTN(1) = 0
      DOAMAP = .FALSE.
      ANTLOD = .FALSE.
      CALL CFILL (60, ' ', USRANM)
      IF (IT.GT.1) THEN
         DO 16 I = 1,IT-1
C                                       reject obvious duplicates
            DO 12 J = 1,I-1
               IF (USRNAM(I).EQ.USRNAM(J)) GO TO 16
 12            CONTINUE
            DO 13 J = 1,IANT
               IF (USRNAM(I).EQ.USRNAM(J)) GO TO 16
 13            CONTINUE
C                                       expand VLBA
            IF (USRNAM(I).EQ.'VLBA') THEN
               DO 14 J = 1,10
                  IANT = IANT + 1
                  TMPNAM(1,IANT) = ANVLBA(J)
                  TMPMAP(1,IANT) = IANT
                  USRANM(IANT) = TMPNAM(1,IANT)
 14               CONTINUE
            ELSE IF (USRNAM(I).EQ.'VLITE') THEN
               DO 15 J = 1,28
                  IANT = IANT + 1
                  IF (J.LT.11) THEN
                     WRITE (VLITE,1013) J-1
                  ELSE
                     WRITE (VLITE,1014) J-1
                     END IF
                  TMPNAM(1,IANT) = VLITE
                  TMPMAP(1,IANT) = IANT
                  USRANM(IANT) = TMPNAM(1,IANT)
 15               CONTINUE
            ELSE
               IANT = IANT + 1
               TMPNAM(1,IANT) = USRNAM(I)
               TMPMAP(1,IANT) = IANT
               USRANM(IANT) = TMPNAM(1,IANT)
               END IF
 16         CONTINUE
         MXANTN(1) = IANT
         ANTLOD = .TRUE.
         DOAMAP = .TRUE.
         END IF
      SELQUA = IROUND (XQUAL)
      CALL FILL (MAXSOU, 0, SOCOUN)
      OUTSEQ = IROUND (XOUTS)
C                                       Start, stop times
      TSTART = TIMER(1) + (TIMER(2) / 24.0) + (TIMER(3) / 1440.) +
     *   (TIMER(4) / 86400.)
      TEND = TIMER(5) + (TIMER(6) / 24.0) + (TIMER(7) / 1440.) +
     *   (TIMER(8) / 86400.)
      TIMSEL = ((TEND-TSTART).GE.1.0E-4)
      IF (.NOT.TIMSEL) THEN
         TSTART = -1.0E19
         TEND = 1.0E19
         END IF
C                                       Channel selection
      BCHAN = IROUND (XBCHAN)
      ECHAN = IROUND (XECHAN)
      CHNSLT = ((BCHAN.NE.0) .AND. (ECHAN.NE.0)) .OR.
     *   ((BCHAN.GT.1) .AND. (ECHAN.EQ.0))
      IF (BCHAN.EQ.0) BCHAN = 1
      SLCHNS = 0
      IF (ECHAN.NE.0) SLCHNS = ECHAN - BCHAN + 1
C                                       IF selection
      BIF = IROUND (XBIF)
      EIF = IROUND (XEIF)
      IFSLT = ((BIF.NE.0) .AND. (EIF.NE.0)) .OR.
     *   ((BIF.GT.1) .AND. (EIF.EQ.0))
      IF (BIF.EQ.0) BIF = 1
      SLIFS = 0
      IF (EIF.NE.0) SLIFS = EIF - BIF + 1
C                                       Weight based flagging
      IF (THRESH.GT.1.0) THRESH = 1.0
      IF (THRESH.LE.0.0) THRESH = 0.0
C                                       debug print
      PRTLV = 0
      SETDEB = 0
      IF (OPTYPE(1:1).EQ.'D') THEN
         PRTLV = -1
         IF ((OPTYPE(2:2).EQ.'E') .OR. (OPTYPE(2:2).EQ.'1'))
     *      PRTLV = 1
         IF (OPTYPE(2:2).EQ.'2') PRTLV = 2
         IF (OPTYPE(2:2).EQ.'3') THEN
            SETDEB = 1
            PRTLV = 0
            MSGTXT = 'Debug mode: VLBA data not divided by weights'
            CALL MSGWRT (6)
            END IF
         IF (PRTLV.GE.0) OPTYPE = 'UV'
         PRTLV = MAX (0, PRTLV)
         END IF
C                                       Compressed output?
      DOUVCM = XDOUVC.GT.0.0
C                                       Load tables for images
      DOTABL = XDOTAB.GT.0.0
C                                       Apply digital corrections
      DOCORR = IROUND(XDOCOR)
      IF (DOCORR.EQ.0) DOCORR = 1
C                                       Generate CQ table
      DELCOR = .TRUE.
C                                       Check for freq selection
      SELBAN = -1.0
      SELFRQ = -1.0D0
      IF (XSELB.GT.0.0) SELBAN = XSELB * 1.0E3
      IF (XSELF.GT.0.0) SELFRQ = XSELF * 1.0D6
      DOFSEL = (XSELB.GT.0.0) .OR. (XSELF.GT.0.0)
      IF (XSELF.GT.0.0) THEN
C                                       Frequency tolerance
         IF (XFRQTL.LE.0.0) FRQTOL = 1.0E4
         IF (XFRQTL.GT.0.0) FRQTOL = XFRQTL * 1.0E3
         END IF
      CALL FILL (MAXFQI, 0, FQCOUN)
C                                       Keep VT table?
      DELVT = .TRUE.
      IF (OPCODE.EQ.'VT') DELVT = .FALSE.
      IF (OPCODE.EQ.'VALL') DELVT = .FALSE.
      DELEX = .TRUE.
      IF (OPCODE.EQ.'ALL') DELEX = .FALSE.
      IF (OPCODE.EQ.'VALL') DELEX = .FALSE.
C                                       Check if disk file output
      DODISK = INFILE.NE.' '
      CALL CATINI (CATBLK)
      CALL FILL (49, 0.0, PCMATX)
      CALL FILL (49, 0.0, PVMATX)
      CALL FILL (49, 0.0, CDMATX)
      EPS = 0.1
C                                       Get TAPEIN
      TAPEIN = TAPE + EPS
C                                       Get NFILES
      NFILES = XFILES + EPS
      IF (XFILES.LT.-EPS) NFILES = XFILES - EPS
C                                       Get DISOUT
      DISOUT = DISO + EPS
C                                       Set user id in CATBLK
      CATBLK(KIIMU) = NLUSER
C                                       If RQUICK, restart AIPS
      IRET = 0
C                                       Test legality
      IF (NTAPED.EQ.1) TAPEIN = 1
      IF (.NOT.DODISK) THEN
         IF ((TAPEIN.LT.1) .OR. (TAPEIN.GT.NTAPED)) THEN
            WRITE (MSGTXT,1010) TAPEIN
            CALL MSGWRT (8)
            IRET = 8
         ELSE IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
            WRITE (MSGTXT,1015)
            CALL MSGWRT (8)
            IRET = 8
            END IF
         END IF
 20   JERR = IRET
      IF (RQUICK) CALL RELPOP (IRET, SCRBUF, IERR)
      IF (JERR.NE.0) GO TO 999
C                                       Setup for Tape I/O
C                                       Buffer size.
      FDVEC(3) = (29184 * NBITWD) / 8
C                                       Logical record size (FITS)
      FDVEC(2) = 2880
      CALL CHR2H (48, INFILE, 1, FDVEC(7))
C                                       Disk input.
      TRYTWO = ' '
      IF (DODISK) THEN
         FDVEC(1) = 25
         TAPEIN = 1
         FDVEC(5) = 1
         MSGSUP = 32000
         CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.EQ.0) THEN
            CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
         ELSE
            TRYTWO = INFILE
            I = ITRIM (INFILE)
            INFILE(I+1:) = '1'
            CALL CHR2H (48, INFILE, 1, FDVEC(7))
            END IF
         MSGSUP = 0
C                                       Tape input
      ELSE
         TAPEIN = TAPE + 0.5
         IF (TAPEIN.LE.0) TAPEIN = 1
         FDVEC(1) = 129 - TAPEIN
         FDVEC(5) = TAPEIN
         WRITE (MSGTXT,1024) TAPEIN
         CALL MSGWRT (3)
         END IF
C                                       Open tape
      CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.GT.1) THEN
         IF (TRYTWO.NE.' ') THEN
            MSGTXT = 'ALSO COULD NOT OPEN ' // TRYTWO
            CALL MSGWRT (8)
            END IF
         WRITE (MSGTXT,1025) IERR
         CALL MSGWRT (8)
         JERR = 8
         GO TO 999
         END IF
C                                       Skip to correct file.
      IF (.NOT.DODISK) THEN
         IFIL = 1 - NFILES
         IF (NFILES.GT.0) THEN
            CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40), NFILES, IERR)
            WRITE (MSGTXT,2010) 'Advancing', NFILES
         ELSE IF (NFILES.LT.0) THEN
            CALL ZTAPE ('BAKF', FDVEC(1), FDVEC(40), IFIL, IERR)
            IFIL = -NFILES
            WRITE (MSGTXT,2010) 'Backing up', IFIL
         ELSE
            MSGTXT = 'Reading tape at current position'
            IERR = 0
            END IF
         IF (IERR.EQ.0) THEN
            CALL MSGWRT (3)
         ELSE
            WRITE (MSGTXT,1026) IERR
            CALL MSGWRT (8)
            JERR = 8
            GO TO 999
            END IF
         END IF
C                                       Read first tape rec
 30   MSGSUP = 32000
      CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, JERR)
      MSGSUP = 0
      NBYT = FDVEC(42)
      IF ((JERR.EQ.0) .OR. (JERR.EQ.10)) GO TO 35
         WRITE (MSGTXT,1030) JERR
         CALL MSGWRT (8)
         GO TO 990
 35   CALL TAPIO ('BAKF', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) THEN
         JERR = IERR
         WRITE (MSGTXT,1035) JERR
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Look for "SIMPLE  = "
      CALL ZC8CL (400, 1, TAPBUF, CHTMP)
      FITS = CHTMP(:10).EQ.'SIMPLE  = '
      IF (FITS) THEN
         IF (JERR.NE.10) THEN
            IT = INDEX (CHTMP(:80), '/')
            IF (IT.LE.0) IT = 80
            DO 40 J = 11,IT
               IF (CHTMP(J:J).NE.' ') THEN
                  IF (CHTMP(J:J).NE.'T') JERR = 10
                  GO TO 45
                  END IF
 40            CONTINUE
            JERR = 10
            END IF
 45      IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1045) NBYT
            CALL MSGWRT (8)
            GO TO 990
            END IF
         UVTABL = (CHTMP(188:190).EQ.'  2') .AND.
     *      (CHTMP(348:350).EQ.'  0') .AND.
     *      (CHTMP(262:268).EQ.'7777777')
C                                       EXPORT format
      ELSE
         NEXPRT = NEXPRT + 1
         IF ((NEXPRT.GT.4) .OR. (.NOT.DODISK)) THEN
            WRITE (MSGTXT,1060) NEXPRT
            CALL MSGWRT (6)
            JERR = 1
            GO TO 990
            END IF
         WRITE (MSGTXT,1050)
         CALL MSGWRT (6)
         IT = 1
         CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40), IT, IERR)
         GO TO 30
         END IF
      GO TO 999
C
 990  CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FTLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS ')
 1010 FORMAT ('FTLIN: INTAPE =',I7,' ILLEGAL')
 1013 FORMAT ('V',I1)
 1014 FORMAT ('V',I2)
 1015 FORMAT ('TAPE JOBS ARE NOT ALLOWED IN BATCH')
 1024 FORMAT ('Reading tape drive number ',I3)
 1025 FORMAT ('ERROR',I7,' OPENING TAPE')
 1026 FORMAT ('ERROR',I7,' POSITIONING TAPE')
 1030 FORMAT ('ERROR',I7,' READING FIRST TAPE RECORD')
 1035 FORMAT ('ERROR',I7,' RE-POSITIONING TAPE WITH BAKF(1)')
 1045 FORMAT ('NON-STANDARD FITS HEADER OF',I7,' BYTES READ, QUITTING')
 1050 FORMAT ('Not FITS file, will skip and try the next')
 1060 FORMAT ('TRIED ',I2,' NON FITS FILES, TRY USING UVLOD OR IMLOD')
 2010 FORMAT (A,I5,' files')
      END
      SUBROUTINE DECFIT (FITS, IERR)
C-----------------------------------------------------------------------
C   DECFIT reads the tape which must be open and positioned at beginning
C   of file and decides if the file we are reading is an image or
C   uv-data. It then rewinds the tape in preparation for the proper
C   reading.
C   Output in common:
C      IMAGE L        T if file to be read is an image, F if is
C                     uv data.
C   Output:
C      FITS  L        T if FITS file, F if not
C      IERR  I        =0 => ok
C                      4 => end of tape
C                      6 => end of medium
C                     other => quit
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IERR
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL   FITS
      CHARACTER CHTMP*80
C-----------------------------------------------------------------------
      IERR = 0
      IMAGE = .TRUE.
C                                       Allow blocking
      FDVEC(6) = 10
C                                       Read record 1 from tape.
      CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
      IF ((IERR.EQ.4) .OR. (IERR.EQ.6)) GO TO 999
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Look for "SIMPLE  = "
      CALL ZC8CL (80, 1, TAPBUF, CHTMP)
      FITS = CHTMP(:10).EQ.'SIMPLE  = '
C                                       Translate current record
      CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
C                                       Decode required cards.
      CALL REQCD (ICEND, IERR)
      IMAGE = .TRUE.
      IF ((GROUP.EQ.1) .OR. (BINTAB) .OR. (UVTABL)) IMAGE = .FALSE.
C                                       Reposition tape
      CALL TAPIO ('BAKF', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DECFIT: ERROR ',I3,' RETURNED FROM TAPIO')
      END
      SUBROUTINE UVFHDR (ISLOT, IERR)
C-----------------------------------------------------------------------
C   UVFHDR reads the tape which must be open and positioned at beginning
C   of file and builds a catalog header and pointers from the tape
C   header records.  It then rewinds the tape in preparation for the
C   history subroutine.  It should work on any random-parameter FITS
C   tape, but is intended primarily for UV data.  UVFHDR also creates
C   the output file.
C   Output:
C      ISLOT  I     Catalog slot number for new UV file.
C      ERR   I        =0 => ok
C                     other => quit
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER DATOBS*11, CHTM8*8, DEFNAM*12, DEFCLS*6, LOCNAM*12,
     *   LOCCLS*6, UVW(3)*4, UVW2(3)*4, ULVLWL(3)*4, UVWL2(3)*8,
     *   TELTYP*4, UVTP*2, STAT*4, PNAME*48, CHTEMP*8
      DOUBLE PRECISION XFREQ, JDAY, NEWD(128)
      REAL      ATEMP, NEWR(256)
      INTEGER   ICARD, IERR, ISLOT, IREC, JJ, FPARMS, I, J, K, INC2,
     *   ISTVAL, ISTINC, ISTREF, IROUND, ISTNUM, NAXIS, IN, IS, IE, IAX,
     *   WTOFF, SCLOFF, LADR(4), LPIECE, IUSER, NEWBLK(256), ITRIM,
     *   ISIZE, LSIZE
      HOLLERITH NEWH(256)
      LOGICAL   END, F, T, TIMDON
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (NEWR, NEWBLK, NEWH, NEWD)
      DATA UVW    /'UU  ','VV  ','WW  '/
      DATA UVW2   /'UU--','VV--','WW--'/
      DATA ULVLWL /'UU-L','VV-L','WW-L'/
      DATA UVWL2 /'UU-L-NCP','VV-L-NCP','WW-L-NCP'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Default scalling parm for wt.
      WTSCAL = 1.0D0
C                                       Init. Stokes pointers
      JADR(1) = 1
      JADR(2) = 2
      JADR(3) = 3
      JADR(4) = 4
      CALL FILL (4, 0, NPV)
C                                       Initialize BLANK values flag
C                                       to false.
      ISBLNK = .FALSE.
C                                       Zero scaling values.
      DO 10 I = 1,20
         PSCAL(I) = 0.0D0
         POFF(I) = 0.0D0
         PTYPES(I) = ' '
 10      CONTINUE
C                                       Initialize header values.
      CALL FILL (KICTPN, 1, CATBLK(KINAX))
      JJ = (6 + KIPTPN + KICTPN) * 2
      CALL RFILL (JJ, HBLANK, CATH)
      CALL RFILL (5, HBLANK, CATH(KHIMN))
C                                       Allow blocking
      FDVEC(6) = 10
C                                       open new version
      IF (IPIECE.GT.1) THEN
C                                       close old
         IF (DODISK) THEN
            CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
            I = ITRIM (INFILE)
            IF (IPIECE-1.GT.9) I = I - 1
            IF (IPIECE-1.GT.99) I = I - 1
            IF (IPIECE.LE.9) THEN
               WRITE (INFILE(I:),1010) IPIECE
            ELSE IF (IPIECE.LE.99) THEN
               WRITE (INFILE(I:),1011) IPIECE
            ELSE
               WRITE (INFILE(I:),1012) IPIECE
               END IF
            CALL CHR2H (48, INFILE, 1, FDVEC(7))
            CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1015) IERR
               CALL MSGWRT (8)
               GO TO 999
               END IF
            END IF
         END IF
C                                       Read record 1 from tape.
      CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
C                                       Decode required cards.
      CALL REQCD (ICEND, IERR)
      IF ((GROUP.EQ.1) .AND. (UVTABL)) GO TO 980
      IF ((GROUP.NE.1) .AND. (.NOT.UVTABL)) GO TO 980
      IF (IERR.NE.0) GO TO 999
      ICARD = ICEND + 1
C                                       Loop until END card found.
      DO 90 IREC = 1,100000000
C                                       Parse card, put value in hdr.
         CALL PARSCD (ICARD, END, IERR)
         IF (END) GO TO 100
         IF (IERR.NE.0) GO TO 999
         ICARD = ICARD + 1
 90      CONTINUE
C                                       Read more cards than we counted
C                                       on.
      WRITE (MSGTXT,1090)
      GO TO 990
C                                       End card found.
 100  CONTINUE
C                                       Reposition file
      CALL TAPIO ('BAKF', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Check piece ID numbers
      LPIECE = IPIECE
      IF (UVTABL) THEN
         IF (NPV(1).GT.0) IPIECE = NPV(1)
         NPIECE = MIN (NPIECE, MAX (1, NPV(2)))
         IF (IPIECE.GT.NPIECE) THEN
            IERR = -1
            GO TO 999
            END IF
         IF (DPIECE.GT.0) CALL COPY (256, CATSAV, CATBLK)
         END IF
C                                       Check names.
C                                       Make axis increments non zero
C                                       to help out dumb programs.
      IN = KINAX
      IS = KRCIC
      IE = IS + CATBLK(KIDIM) - 1
      DO 101 IAX = IS,IE
         IF ((CATR(IAX).EQ.0.0).AND.(CATBLK(IN).EQ.1)) CATR(IAX) = 1.0
         IN = IN + 1
 101     CONTINUE
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), DEFNAM)
      IF (DEFNAM.EQ.' ') CALL H2CHR (8, 1, CATH(KHOBJ), DEFNAM)
      IF (DEFNAM.EQ.' ') DEFNAM(1:8) = 'NO NAME'
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), DEFCLS)
      IF (DEFCLS.EQ.' ') DEFCLS = 'UVDATA'
      IF (OUTSEQ.GE.0) CATBLK(KIIMS) = OUTSEQ
      LOCNAM = NAMOUT
      LOCCLS = CLAOUT
      CALL MAKOUT (DEFNAM, DEFCLS, 0, DEFCLS, LOCNAM, LOCCLS,
     *   CATBLK(KIIMS))
      CALL CHR2H (12, LOCNAM, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, LOCCLS, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
C                                       Message to user
      CALL H2CHR (8, 1, CATH(KHDOB), CHTM8)
      CALL DATDAT (CHTM8, DATOBS)
      CALL H2CHR (8, 1, CATH(KHOBJ), CHTM8)
      WRITE (MSGTXT,1100) CHTM8, DATOBS
      IF ((CHTM8.NE.' ') .OR. (DATOBS.NE.'BAD DATE')) CALL MSGWRT (5)
C                                       Check PCOUNT
      IF (CATBLK(KIPCN).GT.20) GO TO 960
C                                       Make a number of changes needed
C                                       for our internal UV format.
      XFREQ = -1.0D0
      LCMPLX = F
      LSTOKE = F
      NAXIS = CATBLK(KIDIM)
      DO 150 I = 1,NAXIS
         J = 2 * (I - 1)  +  KHCTP
C                                       Complex axis
         CALL H2CHR (8, 1, CATH(J), CHTM8)
         IF (CHTM8.NE.'COMPLEX ') GO TO 110
            IF (I.EQ.1) GO TO 103
               WRITE (MSGTXT,1101) I
               GO TO 990
C                                       Check for non-standard Complex
 103        IF (CATBLK(KINAX+I-1).EQ.3) GO TO 150
               IF (CATBLK(KINAX+I-1).EQ.2) GO TO 105
               IF (UVTABL) GO TO 150
                  WRITE (MSGTXT,1103) CATBLK(KINAX+I-1)
                  GO TO 990
C                                       Check for need to convert
C                                       2 value complex to 3 value
C                                       complex numbers.
 105        CONTINUE
               LCMPLX = T
               CATBLK(KINAX+I-1) = 3
               GO TO 150
C                                       Check for stokes parameter that
C                                       we need to convert to RR, etc.
 110     IF ((CHTM8.NE.'STOKES  ') .OR. (UVTABL)) GO TO 130
            ATEMP = CATD(KDCRV+I-1) + (1-CATR(KRCRP+I-1)) *
     *         CATR(KRCIC+I-1)
            ISTVAL = IROUND (ATEMP)
            ISTINC = IROUND (CATR(KRCIC+I-1))
            ISTREF = IROUND (CATR(KRCRP+I-1))
            ISTNUM = CATBLK(KINAX+I-1)
            NSTACT = CATBLK(KINAX) * CATBLK(KINAX+1)
C                                       Non-standard Stokes
            IF ((ISTNUM.EQ.1) .AND. ((ABS(ISTVAL).EQ.1) .OR.
     *         (ISTVAL.EQ.-2))) GO TO 150
            IF ((ISTNUM.EQ.3) .AND. (ISTVAL.EQ.1) .AND. (ISTINC.EQ.1))
     *         GO TO 150
            IF ((ISTNUM.EQ.4) .AND. (ISTVAL.EQ.1) .AND. (ISTINC.EQ.1))
     *         GO TO 150
            IF (((ISTNUM.EQ.2) .OR. (ISTNUM.EQ.4)) .AND. (ISTVAL.EQ.-1)
     *         .AND. (ISTINC.EQ.-1)) GO TO 150
            IF (((ISTNUM.EQ.2) .OR. (ISTNUM.EQ.4)) .AND. (ISTVAL.EQ.-5)
     *         .AND. (ISTINC.EQ.-1)) GO TO 150
            IF ((ISTNUM.EQ.2) .AND. (ISTVAL.EQ.1) .AND. (ISTINC.EQ.3))
     *         GO TO 150
               IF ((I.EQ.2) .AND. (ISTNUM.EQ.4)) GO TO 115
 111              WRITE (MSGTXT,1111)
                  CALL MSGWRT (6)
                  GO TO 150
C                                       Stokes look up table
 115           DO 120 J = 1,ISTNUM
                  LADR(J) = ISTVAL + (J-ISTREF) * ISTINC
                  LADR(J) = ABS(LADR(J))
                  IF ((LADR(J).LT.1) .OR. (LADR(J).GT.4)) GO TO 111
 120              CONTINUE
               CALL COPY (4, LADR, JADR)
               LSTOKE = T
               CATD(KDCRV+I-1) = SIGN (1, ISTVAL)
               CATR(KRCIC+I-1) = CATD(KDCRV+I-1)
               CATR(KRCRP+I-1) = 1.0
               CATBLK(KINAX+I-1) = 4
               GO TO 150
C                                       Look for frequency.
 130     IF (CHTM8.EQ.'FREQ ') XFREQ = CATD(KDCRV+I-1)
 150     CONTINUE
C                                       Check for the use of 2 PVALS
C                                       values to represent 1 PTYPE
      NPARMS = CATBLK(KIPCN)
      IF ((PTYPES(NPARMS-1).EQ.'WEIGHT') .AND.
     *   (PTYPES(NPARMS).EQ.'SCALE')) NPARMS = NPARMS - 2
      FPARMS = 1
      ITAB(1) = 1
      CALL CHR2H (8, PTYPES(1), 1, CATH(KHPTP))
      DO 180 I = 2,NPARMS
         DO 160 K = 1,FPARMS
            INC2 = 2 * (K-1) + KHPTP
            CALL H2CHR (8, 1, CATH(INC2), CHTM8)
            IF (PTYPES(I).EQ.CHTM8) GO TO 170
 160        CONTINUE
C                                       No equal PTYPEn's this pass.
         FPARMS = FPARMS + 1
         INC2 = 2 * (FPARMS-1) + KHPTP
         IF (FPARMS.LE.KIPTPN) CALL CHR2H (8, PTYPES(I), 1, CATH(INC2))
         K = FPARMS
C                                       Set pointer
 170     ITAB(I) = K
 180     CONTINUE
      CATBLK(KIPCN) = FPARMS
C                                       Loop through parameters
C                                       looking for DATE change
C                                       to TIME1 and UU, VV, WW
C                                       to fix up.
      TIMDON = .FALSE.
      PBASE = 0
      DO 220 I = 1,NPARMS
         INC2 = 2 * (ITAB(I) - 1) + KHPTP
         IF (PTYPES(I).EQ.'BASELINE') PBASE = I
C                                       Convert DATE to TIME1.
         IF ((.NOT.TIMDON) .AND. (PTYPES(I).EQ.'DATE')) THEN
            CALL H2CHR (8, 1, CATH(KHDOB), CHTM8)
            CALL JULDAY (CHTM8, JDAY)
            POFF(I) = POFF(I) - JDAY
            CHTM8 = 'TIME1'
            CALL CHR2H (8, CHTM8, 1, CATH(INC2))
            TIMDON = .TRUE.
C                                       Multiply UU, VV, WW random
C                                       parameters by freq.
         ELSE IF (XFREQ.GT.0.0D0) THEN
            DO 190 J = 1,3
C                                       Found one.
               IF ((UVW(J).EQ.PTYPES(I)(:4)) .OR.
     *            (UVW2(J).EQ.PTYPES(I)(:4))) THEN
                  PSCAL(I) = PSCAL(I) * XFREQ
                  POFF(I) = POFF(I) * XFREQ
                  CALL H2CHR (4, 1, CATH(KHTEL), TELTYP)
                  IF (TELTYP.EQ.'ATCA') THEN
                     CALL CHR2H (8, UVWL2(J), 1, CATH(INC2))
                  ELSE
                     CALL CHR2H (4, ULVLWL(J), 1, CATH(INC2))
                     END IF
                  GO TO 220
                  END IF
 190           CONTINUE
            END IF
 220     CONTINUE
      CATBLK(KIIMU) = NLUSER
C                                       Compressed output data?
      IF (UVTABL) DOUVCM = .FALSE.
C                                       expand baseline if allowed
      IF (PBASE.GT.0) THEN
         I = FPARMS
         IF (DOUVCM) I = I + 2
         IF (I+2.LE.KIPTPN) THEN
            PTYPES(PBASE) = 'SUBARRAY'
            PBASE = ITAB(PBASE)
            INC2 = 2 * (PBASE - 1) + KHPTP
            CALL CHR2H (8, 'SUBARRAY', 1, CATH(INC2))
            CALL APPRPM ('ANTENNA1')
            PANT1 = CATBLK(KIPCN)
            CALL APPRPM ('ANTENNA2')
            PANT2 = CATBLK(KIPCN)
         ELSE
            PBASE = 0
            END IF
         END IF
      IF (DOUVCM) THEN
         MSGTXT = 'UV data will be written in compressed format'
         CALL MSGWRT (4)
         CATBLK(KINAX) = 1
C                                       Make sure that the random
C                                       parameter list contains one
C                                       WEIGHT parameter immediately
C                                       followed by a SCALE parameter:
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), WTOFF,
     *      IERR)
         IF (IERR.NE.0) THEN
            WTOFF = -1
            IERR = 0
            END IF
C                                       WTOFF is now the offset of the
C                                       WEIGHT parameter if it already
C                                       exists and -1 otherwise.
         CALL AXEFND (8, 'SCALE   ', CATBLK(KIPCN), CATH(KHPTP), SCLOFF,
     *      IERR)
         IF (IERR.NE.0) THEN
            SCLOFF = -1
            IERR = 0
            END IF
C                                       SCLOFF is now the offset of the
C                                       SCALE parameter if it already
C                                       exists and -1 otherwise.
         IF ((WTOFF.GT.-1) .AND. (WTOFF.LT.NPARMS-1) .AND.
     *      (SCLOFF.NE.WTOFF+1)) THEN
            CALL DELRPM (WTOFF)
            WTOFF = -1
            END IF
         IF (WTOFF.EQ.-1) THEN
            CALL APPRPM ('WEIGHT  ')
            CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP),
     *         WTOFF, IERR)
C                                       AXEFND is not really necessary
C                                       here as APPRPM always adds a new
C                                       random parameter to the end of
C                                       the parameter list but leaves us
C                                       open for a more intelligent
C                                       APPRPM that re-uses deleted
C                                       parameters.
            END IF
         IF ((SCLOFF.NE.-1).AND.(SCLOFF.NE.WTOFF+1)) THEN
            CALL DELRPM (SCLOFF)
            SCLOFF = -1
            END IF
         IF (SCLOFF.EQ.-1) THEN
            CALL APPRPM ('SCALE   ')
            CALL AXEFND (8, 'SCALE   ', CATBLK(KIPCN), CATH(KHPTP),
     *         SCLOFF, IERR)
            END IF
         KLOCWT = WTOFF
         END IF
C                                       Does our file already exist
      ISLOT = 0
      IF (OUTSEQ.GT.0) THEN
         UVTP = 'UV'
         IUSER = NLUSER
         CALL CATDIR ('SRNH', DISOUT, ISLOT, LOCNAM, LOCCLS, OUTSEQ,
     *      UVTP, IUSER, STAT, SCRBUF, IERR)
         IF (IERR.EQ.5) THEN
            ISLOT = 0
            IERR = 0
            END IF
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1220) IERR, 'CATDIR'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
C                                       File not found
      IF (ISLOT.LE.0) THEN
C                                       But it is required
         IF (CONCAT.GT.0) THEN
            MSGTXT = 'OUTPUT CONCATANATION FILE NOT FOUND'
            CALL MSGWRT (8)
            IERR = -1
            GO TO 999
            END IF
C                                       Create the UV file.
         DPIECE = DPIECE + 1
         CALL UVCREA (DISOUT, ISLOT, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Tell common we have a UV file.
         NCFILE = 1
         FVOL(1) = DISOUT
         FCNO(1) = ISLOT
         FRW(1) = 2
         OUTSEQ = CATBLK(KIIMS)
C                                       File found
      ELSE
         IF (DPIECE.GT.0) CALL COPY (256, CATSAV, CATBLK)
         DPIECE = DPIECE + 1
C                                       But that is not allowed
         IF (CONCAT.EQ.-1) THEN
            MSGTXT = 'FILE ALREADY EXISTS'
            CALL MSGWRT (8)
            IERR = -1
            GO TO 999
            END IF
         CONCAT = 1
         CALL COPY (256, CATBLK, NEWBLK)
         CALL CATIO ('READ', DISOUT, ISLOT, CATBLK, 'WRIT', SCRBUF,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1220) IERR, 'CATBLK READ'
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       compare things
         IF (CATBLK(KIDIM).NE.NEWBLK(KIDIM)) THEN
            WRITE (MSGTXT,1230) CATBLK(KIDIM), NEWBLK(KIDIM),
     *          'AXIS COUNT'
            GO TO 990
            END IF
         IF ((CATBLK(KIPCN).NE.NEWBLK(KIPCN)) .AND.
     *      (CATBLK(KIPCN).NE.NEWBLK(KIPCN)-2) .AND.
     *      (CATBLK(KIPCN).NE.NEWBLK(KIPCN)+2)) THEN
            WRITE (MSGTXT,1230) CATBLK(KIPCN), NEWBLK(KIPCN),
     *          'PARAMETER COUNT'
            GO TO 990
            END IF
         K = MIN (CATBLK(KIPCN), NEWBLK(KIPCN))
         DO 230 I = 1,K
            J = KHPTP + 2 * I - 2
            CALL H2CHR (8, 1, CATH(J), CHTM8)
            CALL H2CHR (8, 1, NEWH(J), CHTEMP)
            IF (CHTEMP.NE.CHTM8) THEN
               WRITE (MSGTXT,1231) CHTM8, CHTEMP, 'RANDOM PARAMETER'
               GO TO 990
               END IF
 230        CONTINUE
         K = CATBLK(KIDIM)
         DO 240 I = 2,K
            J = KHCTP + 2 * I - 2
            CALL H2CHR (8, 1, CATH(J), CHTM8)
            CALL H2CHR (8, 1, NEWH(J), CHTEMP)
            IF (CHTEMP.NE.CHTM8) THEN
               WRITE (MSGTXT,1231) CHTM8, CHTEMP, 'AXIS PARAMETER'
               GO TO 990
               END IF
            IF (CATBLK(KINAX+I-1).NE.NEWBLK(KINAX+I-1)) THEN
               WRITE (MSGTXT,1230) CATBLK(KINAX+I-1), NEWBLK(KINAX+I-1),
     *            'AXIS DIMENSION'
               GO TO 990
               END IF
            IF (ABS(CATR(KRCRP+I-1)-NEWR(KRCRP+I-1)).GT.0.01) THEN
               WRITE (MSGTXT,1232) CATR(KRCRP+I-1), NEWR(KRCRP+I-1),
     *            'REFERENCE PIXEL'
               GO TO 990
               END IF
            IF (ABS(CATD(KDCRV+I-1)-NEWD(KDCRV+I-1)).GT.0.01) THEN
               WRITE (MSGTXT,1232) CATD(KDCRV+I-1), NEWD(KDCRV+I-1),
     *            'REFERENCE VALUE'
               GO TO 990
               END IF
            IF (ABS(CATR(KRCRT+I-1)-NEWR(KRCRT+I-1)).GT.0.01) THEN
               WRITE (MSGTXT,1232) CATR(KRCRT+I-1), NEWR(KRCRT+I-1),
     *            'ROTATION'
               GO TO 990
               END IF
 240        CONTINUE
C                                       expand file
         IF (.NOT.UVTABL) THEN
            CALL UVPGET (IERR)
            CALL ZPHFIL ('UV', DISOUT, ISLOT, 1, PNAME, IERR)
            CALL ZEXIST (DISOUT, PNAME, LSIZE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1220) IERR, 'FILE EXISTS CHECK'
               CALL MSGWRT (8)
               GO TO 999
               END IF
            CATBLK(KIGCN) = CATBLK(KIGCN) + NEWBLK(KIGCN)
            CALL UVSIZE (LREC, CATBLK(KIGCN), ISIZE)
            IF (ISIZE.GT.LSIZE) THEN
               CALL ZOPEN (IDLUN, IDFIND, DISOUT, PNAME, .TRUE., .TRUE.,
     *            .TRUE., IERR)
               IF (IERR.NE.0) GO TO 999
               I = ISIZE - LSIZE
               CALL ZEXPND (IDLUN, DISOUT, PNAME, I, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1220) IERR, 'EXPAND FILE'
                  CALL MSGWRT (8)
                  GO TO 999
                  END IF
               CALL ZCLOSE (IDLUN, IDFIND, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            END IF
C                                       Tell common we have a UV file.
         NCFILE = 1
         FVOL(1) = DISOUT
         FCNO(1) = ISLOT
         FRW(1) = 1
         END IF
      GO TO 999
C                                       Too many random parameters.
 960  WRITE (MSGTXT,1960) CATBLK(KIPCN)
      GO TO 990
C                                       Not random parameter structure.
 980  WRITE (MSGTXT,1980)
      CALL MSGWRT (7)
      WRITE (MSGTXT,1981)
C
 990  CALL MSGWRT (7)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (I1)
 1011 FORMAT (I2)
 1012 FORMAT (I3)
 1015 FORMAT ('ERROR OPENING NEW DISK FILE:',I7)
 1090 FORMAT ('READ MORE THAN 10**8 CARDS WITHOUT FINDING AN END CARD')
 1100 FORMAT ('Found ',A8,' observed on ',A)
 1101 FORMAT ('FOUND COMPLEX IN AXIS',I3,'. WE ONLY HANDLE AXIS 1.')
 1103 FORMAT ('FOUND ILLEGAL COMPLEX AXIS WITH',I7,' POINTS')
 1111 FORMAT ('WARNING! STOKES VALUES NOT CONVERTED TO NORMAL FORMS')
 1220 FORMAT ('ERROR',I4,' ON ',A)
 1230 FORMAT ('MISMATCH',2I12,2X,A)
 1231 FORMAT ('MISMATCH ''',A,''' ''',A,'''',2X,A)
 1232 FORMAT ('MISMATCH',2(1PE12.4),2X,A)
 1960 FORMAT ('FOUND',I7,' RANDOM PARAMETERS.  MAX = 20')
 1980 FORMAT ('EXPECTED RANDOM PARAMETER STRUCTURE NOT FOUND.')
 1981 FORMAT ('FITLD UVHDR SECTION CANNOT DO IMAGES.')
      END
      SUBROUTINE UVFHIS (ISLOT, IERR)
C-----------------------------------------------------------------------
C   UVFHIS reads the tape (which must be open and positioned at begin.
C   of file) and builds a history file from the FITS history and other
C   keywords in the FITS header.  The Antenna file is also created at
C   this time and the antenna information found in the history is put
C   in this file.  This program leaves the tape positioned at the
C   start of the binary data.  It should work on any random-parameter
C   FITS tape, but is intended primarily for UV data.
C   INPUT:  ISLOT  I        catalog slot of UV data file.
C           IHLUN  I        history file logical unit number (from
C                           common)
C   Output: IERR   I        =0 => ok
C                           other => quit
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER KL*80, SYMBOL*8, LNAM*12, LCLAS*6, CARD*80, HILINE*72,
     *   CHTMP*8
      DOUBLE PRECISION    DABLK(128)
      REAL      ABLK(256)
      INTEGER   IABLK(512), IERR, ISLOT, ICARD, IREC, ITABNO, IALUN,
     *   INC, IST, I, NOANT, NPNT, NN, NNSTR, IHERR, JERR, I4, ITRIM
      LOGICAL   UPDATE, END, ISHIST, EQUAL
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DFUV.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (DABLK, ABLK, IABLK)
      INCLUDE 'INCS:VFUV.INC'
      DATA UPDATE /.TRUE./
      DATA IALUN /30/
C-----------------------------------------------------------------------
C                                       Init number of ant records.
      NOANT = 0
      NN = NCT + NKT
      NNSTR = 1
C                                       Create HI file
      IF (CONCAT.EQ.1) MSGSUP = 32000
      CALL HICREA (IHLUN, DISOUT, ISLOT, CATBLK, IHBLK, IHERR)
      MSGSUP = 0
C                                       Header msg in HI

      WRITE (HILINE,1000) ('----', I = 1,17)
      IF (IHERR.EQ.0) CALL HIADD (IHLUN, HILINE, IHBLK, IHERR)
      IF (CONCAT.NE.1) THEN
         WRITE (HILINE,1002)
      ELSE
         WRITE (HILINE,1003)
         END IF
      IF (IHERR.EQ.0) CALL HIADD (IHLUN, HILINE, IHBLK, IHERR)
      IF (IHERR.NE.0) GO TO 950
C                                       Read record 1 from tape.
      CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 960
      CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
C                                       Skip required cards.
      ICARD = ICEND
C                                       Loop until END card found.
      DO 30 IREC = 1,100000000
         ICARD = ICARD + 1
C                                       Read next non-blank record.
         CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 960
C                                       Parse card, put unknown cards
C                                       in history file.
         INC = (ICARD-1) * 80 + 1
         CHTMP = FITBLK(INC:INC+7)
         EQUAL = ('HISTORY'.EQ.CHTMP) .OR. ('COMMENT'.EQ.CHTMP) .OR.
     *      (' '.EQ.CHTMP)
         IST = 1
         IF (EQUAL) IST = 9
         IF (CONCAT.EQ.1) THEN
            IF (CHTMP.EQ.'END     ') GO TO 40
            INC = (ICARD - 1) * 80  +  1
            CARD = FITBLK(INC:INC+79)
            IF (CARD(:15).EQ.'HISTORY AIPS   ') THEN
               IF ((CARD(16:21).EQ.'IPIECE') .OR.
     *            (CARD(16:21).EQ.'FIRSTV') .OR.
     *            (CARD(16:21).EQ.'LASTVI')) THEN
                  IF (IHERR.EQ.0) CALL HIAD80 (IHLUN, IST, CARD, IHBLK,
     *               IHERR)
                  END IF
               END IF
         ELSE
            NPNT = 1
            CALL GETCRD (ICARD, NN, NNSTR, AWORD, FITBLK, NPNT, KL,
     *         SYMBOL, ITABNO, ISHIST, END, JERR)
            IF (END) GO TO 40
            IF ((JERR.NE.0) .OR. (IST.NE.1)) THEN
               INC = (ICARD - 1) * 80  +  1
               CARD = FITBLK(INC:INC+79)
C                                       special header keywords
               IF (JERR.EQ.-1) THEN
                  CALL PUTCRD (CARD, DISOUT, ISLOT, JERR)
                  IF (JERR.GT.1) THEN
                     WRITE (MSGTXT,1010) JERR
                     CALL MSGWRT (7)
                     IERR = JERR
                     GO TO 999
                     END IF
C                                       Antenna record.
               ELSE IF ((ISHIST) .AND. (SYMBOL(:4).EQ.'ANT ')) THEN
                  CALL ANTREC (KL, NPNT, IALUN, ISLOT, NOANT, IABLK,
     *               JERR)
C                                       Add this unknown card to hist.
               ELSE
                  IF (IHERR.EQ.0) CALL HIAD80 (IHLUN, IST, CARD, IHBLK,
     *               IHERR)
                  END IF
               END IF
            END IF
 30      CONTINUE
C                                       Read more cards than expected.
      GO TO 970
C                                       End card found.
C                                       Clean up antenna file stuff.
 40   IF (NOANT.EQ.0) GO TO 70
C                                       Close ant file, save last recrd
      CALL TABIO ('CLOS', 1, I4, SCRBUF, IABLK, JERR)
C                                       Add history info
 70   IF (CONCAT.NE.1) THEN
         WRITE (HILINE,1070)
         IF (IHERR.EQ.0) CALL HIADD (IHLUN, HILINE, IHBLK, IHERR)
         WRITE (HILINE,1072) RLSNAM, ('----', I = 1,11)
         IF (IHERR.EQ.0) CALL HIADD (IHLUN, HILINE, IHBLK, IHERR)
         CALL H2CHR (12, KHIMNO, CATH(KHIMN), LNAM)
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), LCLAS)
         IF (IHERR.EQ.0) CALL HENCOO (TSKNAM, LNAM, LCLAS,
     *      CATBLK(KIIMS), FVOL(1), IHLUN, IHBLK, IHERR)
         END IF
C                                       If DISK file give name.
      IF (INFILE.NE.' ') THEN
         I = ITRIM (INFILE)
         WRITE (HILINE,1074) TSKNAM, INFILE(:I)
         IF (IHERR.EQ.0) CALL HIADD (IHLUN, HILINE, IHBLK, IERR)
         END IF
      CALL HICLOS (IHLUN, UPDATE, IHBLK, I)
      IF (IHERR.EQ.0) GO TO 999
C                                       Error handling.
C                                       History file error.
 950  WRITE (MSGTXT,1950)
      GO TO 990
C                                       Tape read problem.
 960  WRITE (MSGTXT,1960)
      GO TO 990
C                                       Read more cards than we
C                                       expected.
 970  WRITE (MSGTXT,1970)
C
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('/',17A4)
 1002 FORMAT ('/Begin "HISTORY" information found in FITS tape ',
     *   'header by FITLD')
 1003 FORMAT ('/Begin concatanated "HISTORY" information from FITS ',
     *   'header by UVLOD')
 1010 FORMAT ('ERROR',I5,' ADDING KEYWORD TO HEADER FILE')
 1070 FORMAT ('/End FITS tape header "HISTORY" information')
 1072 FORMAT ('FITLD RELEASE= ''',A7,' '' /',11A4)
 1074 FORMAT (A6,'INFILE=''',A,'''')
 1950 FORMAT ('HISTORY FILE PROBLEM.')
 1960 FORMAT ('TAPE PROBLEM DURING HISTORY FILE READ.')
 1970 FORMAT ('READ MORE THAN 10**8 CARDS WITHOUT AN END CARD.')
      END
      SUBROUTINE UVFDAT (ISLOT, IERR)
C-----------------------------------------------------------------------
C   UVFDAT reads the tape (which must be open and positioned at begin.
C   of the binary data) and reorders and floats the binary data,
C   writing it to an AIPS random parameter catalogd file.  It leaves
C   the tape at the start of the next file.  It should work on any
C   random-parameter FITS tape, but is intended primarily for UV data.
C   Input:
C      ISLOT   I   catalog slot number of UV file.
C   Output:
C      IERR    I   0 => ok
C                  other => quit
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER UVNAME*48
      REAL      RECBYR(UVBFSL), WTS(10), FITBR(720), XF, BASEL
      DOUBLE PRECISION FITBD(360), RECBYD(UVBFSL/2)
      INTEGER   NVISOF, NBLKOF, DVIS, VISMAX, NWDLF, FITBIN(1440), IERR,
     *   IDCTR, IVCTR, IVMAX, INDEXX, NCORR, MRPARM, NTOTAL, NAXIS,
     *   NAXVAL, ISTCNT, I, ISIZE, ISLOT, IWTCTR, NEXT, LOOP,
     *   RECBYT(UVBFSL), DATTYP, LIMIT, NW, IP, IP2, OUTP, NUMCPX,
     *   NCMPLX, VISPNT(MAXCIF), NCOPY, NMOV, POINT, NUMDO, NSTOKE,
     *   STKNUM, JNCS, STKCNT, ZTYPE, ISUB, IANT1, IANT2
      LOGICAL   MAP, EXCL, WAIT, GOOD
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'SCRBFS.INC'
      EQUIVALENCE (RECBYT, RECBYR, TUVBUF)
      EQUIVALENCE (FITBIN, FITBR)
      EQUIVALENCE (MAP, EXCL, WAIT)
      DATA MAP /.TRUE./
      DATA NBLKOF, NVISOF /1, 0/
C-----------------------------------------------------------------------
      IDLUN = 16
C                                       Crunch CATBLK
      MSGSUP = 32000
      CALL UVPGET (IERR)
      MSGSUP = 0
      CALL FILL (6, 0, NZERO)
C                                       Open UV file.
      CALL ZPHFIL ('UV', DISOUT, ISLOT, 1, UVNAME, IERR)
      CALL ZOPEN (IDLUN, IDFIND, DISOUT, UVNAME, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Determine number of groups to
C                                       process.
      VISMAX = CATBLK(KIGCN)
C                                       Data type
      DATTYP = ABS (IBPP) / 8
      NW = 2880 / DATTYP
      NUMCPX = CATBLK(KINAX)
      NCMPLX = 3
      IF (LCMPLX) NCMPLX = 2
C                                       Determine number of values in
C                                       a group.
      NAXIS = CATBLK(KIDIM)
      NAXVAL = NCMPLX
      DO 10 I = 2,NAXIS
         NAXVAL = NAXVAL * CATBLK(KINAX+I-1)
 10      CONTINUE
      NCORR = NAXVAL / NCMPLX
      MRPARM = CATBLK(KIPCN)
C                                      Total vals/visrec on tape.
      NTOTAL = NPARMS + NAXVAL
C                                       Set visibility pointers
C                                       Put Stokes' in proper order
      NSTOKE = 1
      IF (JLOCS.GE.0) NSTOKE = CATBLK(KINAX+JLOCS)
      IF (JLOCS.LT.0) INCS = NUMCPX
      JNCS = INCS / NUMCPX
      LIMIT = NAXVAL / NCMPLX
C                                       test our limitations
      IF ((NTOTAL.LE.UVBFSL) .AND. (LREC.LE.UVBFSL) .AND.
     *   (LIMIT.LE.MAXCIF)) GO TO 15
         WRITE (MSGTXT,1010) NTOTAL, LREC, LIMIT
         IERR = 2
         GO TO 995
C
15    DO 20 LOOP = 1,LIMIT
         STKCNT = (LOOP-1) / JNCS
         STKNUM = MOD (STKCNT, NSTOKE) + 1
         INDEXX = (LOOP-1) + (JADR(STKNUM) - STKNUM) * JNCS
         VISPNT(LOOP) = CATBLK(KIPCN) + INDEXX * 3
 20      CONTINUE
C                                       Initialize uv writing system.
      ISIZE = 2 * UVBFSL
      IVMAX = 0
      CALL UVINIT ('WRIT', IDLUN, IDFIND, CATBLK(KIGCN), NVISOF, LREC,
     *   IVMAX, ISIZE, UVBUFF, NBLKOF, IDCTR, IERR)
      IF (IERR.NE.0) GO TO 999
      IVCTR = 0
      IWTCTR = 1
      ISTCNT = 0
      DVIS = 0
C                                      Read first record
      CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 970
      NEXT = 1
C                                       Convert data types
      IF (IBPP.EQ.8) CALL ZI8IL (NW, 1, TAPBUF(TBIND), FITBIN)
      IF (IBPP.EQ.16) CALL ZI16IL (NW, 1, TAPBUF(TBIND), FITBIN)
      IF (IBPP.EQ.32) CALL ZI32IL (NW, 1, TAPBUF(TBIND), FITBIN)
      IF (IBPP.EQ.-32) CALL ZR32RL (NW, 1, TAPBUF(TBIND), FITBR)
      IF (IBPP.EQ.-64) CALL ZR64RL (NW, 1, TAPBUF(TBIND), FITBD)
C                                       Loop until we exhaust data.
 40   CONTINUE
         DVIS = DVIS + 1
C                                       Get next record from tape
         POINT = 1
         NMOV = NTOTAL
C                                       Determine number of words in
C                                       current record.
 50         NWDLF = NW + 1 - NEXT
            NCOPY = NMOV
            IF (NWDLF.LT.NCOPY) NCOPY = NWDLF
            NUMDO = NCOPY
C                                       integers
            IF (IBPP.GT.0) THEN
               CALL COPY (NUMDO, FITBIN(NEXT), RECBYT(POINT))
C                                       32-bit floating
            ELSE IF (IBPP.EQ.-32) THEN
               CALL RCOPY (NUMDO, FITBR(NEXT), RECBYR(POINT))
C                                       64-bit floating
            ELSE
               CALL DCOPY (NUMDO, FITBD(NEXT), RECBYD(POINT))
               END IF
C                                       Are we done with I/O for now?
            NEXT = NEXT + NCOPY
C                                       More to do
            IF (NCOPY.GE.NMOV) GO TO 100
               POINT = POINT + NCOPY
               NMOV = NMOV - NCOPY
C                                       Read next record
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
               IF (IERR.NE.0) GO TO 970
C                                       Convert data types
               IF (IBPP.EQ.8) CALL ZI8IL (NW, 1, TAPBUF(TBIND), FITBIN)
               IF (IBPP.EQ.16) CALL ZI16IL (NW, 1, TAPBUF(TBIND),
     *            FITBIN)
               IF (IBPP.EQ.32) CALL ZI32IL (NW, 1, TAPBUF(TBIND),
     *            FITBIN)
               IF (IBPP.EQ.-32) CALL ZR32RL (NW, 1, TAPBUF(TBIND),
     *            FITBR)
               IF (IBPP.EQ.-64) CALL ZR64RL (NW, 1, TAPBUF(TBIND),
     *            FITBD)
               NEXT = 1
               GO TO 50
C                                       Scale and offset to R:
C                                       Integer inputs
 100     IF (IBPP.GT.0) THEN
C                                       Random parms
            DO 120 LOOP = 1,NPARMS
               RECR(LOOP) = PSCAL(LOOP) * RECBYT(LOOP) + POFF(LOOP)
 120           CONTINUE
C                                       Regular data
            LIMIT = NPARMS + 1
            IF (.NOT.ISBLNK) THEN
               DO 125 LOOP = LIMIT,NTOTAL
                  RECR(LOOP) = BSC * RECBYT(LOOP) + BZE
 125              CONTINUE
               GO TO 200
C                                       Possibly blanked
            ELSE
               DO 130 LOOP = LIMIT,NTOTAL
                  IF (RECBYT(LOOP).EQ.IBLANK) THEN
                     RECR(LOOP) = FBLANK
                  ELSE
                     RECR(LOOP) = BSC * RECBYT(LOOP) + BZE
                     END IF
 130              CONTINUE
               END IF
C                                       32 bit floating input
         ELSE IF (IBPP.EQ.-32) THEN
C                                       Random parameters
            DO 140 LOOP = 1,NPARMS
               RECR(LOOP) = PSCAL(LOOP) * RECBYR(LOOP) + POFF(LOOP)
 140           CONTINUE
C                                       Possibly blanked
C                                       Regular data
            LIMIT = NPARMS + 1
            DO 145 LOOP = LIMIT,NTOTAL
               IF (RECBYR(LOOP).EQ.FBLANK) THEN
                  RECR(LOOP) = FBLANK
               ELSE
                  RECR(LOOP) = BSC * RECBYR(LOOP) + BZE
                  END IF
 145           CONTINUE
C                                       64 bit floating input
         ELSE
C                                       Random parameters
            DO 150 LOOP = 1,NPARMS
               RECR(LOOP) = PSCAL(LOOP) * RECBYD(LOOP) + POFF(LOOP)
 150           CONTINUE
C                                       Possibly blanked
C                                       Regular data
            LIMIT = NPARMS + 1
            DO 155 LOOP = LIMIT,NTOTAL
               IF (RECBYD(LOOP).EQ.DBLANK) THEN
                  RECR(LOOP) = FBLANK
               ELSE
                  RECR(LOOP) = BSC * RECBYD(LOOP) + BZE
                  END IF
 155           CONTINUE
            END IF
C                                       Other data type go here:
C                                       Sum to output record
 200     CALL RFILL (LREC, 0, UVBUFF(IDCTR))
         CALL RFILL (NUMCPX, 1, WTS)
         WTS(3) = WTSCAL
C                                       Random  parms
         DO 210 LOOP = 1,NPARMS
            IP = IDCTR + ITAB(LOOP) - 1
            UVBUFF(IP) = UVBUFF(IP) + RECR(LOOP)
 210        CONTINUE
         IF ((PBASE.GT.0) .OR. (ILOCB.GE.0)) THEN
            IF (PBASE.GT.0) THEN
               IP = IDCTR-1+PBASE
            ELSE
               IP = IDCTR+ILOCB
               END IF
            ZTYPE = UVBUFF(IP) + 0.01
            IF ((ZTYPE/257)*257.EQ.ZTYPE) THEN
               ZTYPE = 2
            ELSE
               ZTYPE = 1
               END IF
         ELSE IF ((ILOCA1.GE.0) .AND. (ILOCA2.GE.0)) THEN
            IF (UVBUFF(IDCTR+ILOCA1).EQ.UVBUFF(IDCTR+ILOCA2)) THEN
               ZTYPE = 2
            ELSE
               ZTYPE = 1
               END IF
            END IF
C                                       baseline -> subarray, antenna1/2
         IF (PBASE.GT.0) THEN
            BASEL = UVBUFF(IDCTR-1+PBASE)
            IANT1 = BASEL/256
            BASEL = BASEL - 256*IANT1
            IANT2 = BASEL
            ISUB = (BASEL - IANT2) * 100.0 + 1.05
            UVBUFF(IDCTR-1+PBASE) = ISUB
            UVBUFF(IDCTR-1+PANT1) = IANT1
            UVBUFF(IDCTR-1+PANT2) = IANT2
            END IF
C                                       Regular data
C                                       Normal Complex and unblanked
         LIMIT = NPARMS + 1
         IF ((LCMPLX) .OR. (ISBLNK)) GO TO 240
C                                       Compress?
            IP = 1
            IF (DOUVCM) THEN
               DO 220 LOOP = LIMIT,NTOTAL,3
                  IP2 = VISPNT(IP) - MRPARM
                  IF ((RECR(LOOP).EQ.FBLANK) .OR.
     *               (RECR(LOOP+1).EQ.FBLANK) .OR.
     *               (RECR(LOOP+2).EQ.FBLANK)) THEN
                     CALL RFILL (3, 0.0, TUVBUF(1+IP2))
                  ELSE
                     TUVBUF(1+IP2) = RECR(LOOP)
                     TUVBUF(2+IP2) = RECR(LOOP+1)
                     TUVBUF(3+IP2) = RECR(LOOP+2) * WTSCAL
                     END IF
                  IP = IP + 1
 220              CONTINUE
               CALL UVZERO (CATBLK, TUVBUF, NZERO(1,ZTYPE), GOOD)
               CALL ZUVPAK (NCORR, TUVBUF, UVBUFF(IDCTR+KLOCWT),
     *            UVBUFF(IDCTR+MRPARM))
            ELSE
               DO 230 LOOP = LIMIT,NTOTAL,3
                  IP2 = VISPNT(IP)
                  IF ((RECR(LOOP).EQ.FBLANK) .OR.
     *               (RECR(LOOP+1).EQ.FBLANK) .OR.
     *               (RECR(LOOP+2).EQ.FBLANK)) THEN
                     CALL RFILL (3, 0.0, UVBUFF(IDCTR+IP2))
                  ELSE
                     UVBUFF(IDCTR+IP2) = RECR(LOOP)
                     UVBUFF(IDCTR+IP2+1) = RECR(LOOP+1)
                     UVBUFF(IDCTR+IP2+2) = RECR(LOOP+2) * WTSCAL
                     END IF
                  IP = IP + 1
 230              CONTINUE
               CALL UVZERO (CATBLK, UVBUFF(IDCTR+CATBLK(KIPCN)),
     *            NZERO(1,ZTYPE), GOOD)
               END IF
            GO TO 290
C                                       Blanked and/or 2 complex axis
C                                       data.
 240     CONTINUE
            IP = 0
C                                       Compress?
            IF (DOUVCM) THEN
               DO 250 LOOP = LIMIT,NTOTAL,NCMPLX
                  IP = IP + 1
                  OUTP = VISPNT(IP) - MRPARM + 1
C                                       Check if blanked
                  IF ((RECR(LOOP).NE.FBLANK) .AND.
     *               (RECR(LOOP+1).NE.FBLANK)) THEN
                     TUVBUF(OUTP) = RECR(LOOP)
                     TUVBUF(OUTP+1) = RECR(LOOP+1)
                     IF (LCMPLX) THEN
                        TUVBUF(OUTP+2) = 1.0
                     ELSE
                        TUVBUF(OUTP+2) = RECR(LOOP+2) * WTSCAL
                        END IF
                  ELSE
C                                       Blanked
                     TUVBUF(OUTP) = 0.0
                     TUVBUF(OUTP+1) = 0.0
                     TUVBUF(OUTP+2) = 0.0
                     END IF
 250              CONTINUE
               CALL UVZERO (CATBLK, TUVBUF, NZERO(1,ZTYPE), GOOD)
               CALL ZUVPAK (NCORR, TUVBUF, UVBUFF(IDCTR+KLOCWT),
     *            UVBUFF(IDCTR+MRPARM))
            ELSE
C                                       Normal output data
               DO 260 LOOP = LIMIT,NTOTAL,NCMPLX
                  IP = IP + 1
                  OUTP = VISPNT(IP) + IDCTR
C                                       Check if blanked
                  IF ((RECR(LOOP).NE.FBLANK) .AND.
     *               (RECR(LOOP+1).NE.FBLANK)) THEN
                     UVBUFF(OUTP) = RECR(LOOP)
                     UVBUFF(OUTP+1) = RECR(LOOP+1)
                     IF (LCMPLX) THEN
                        UVBUFF(OUTP+2) = 1.0
                     ELSE
                        UVBUFF(OUTP+2) = RECR(LOOP+2) * WTSCAL
                        END IF
C                                       Blanked
                  ELSE
                     UVBUFF(OUTP) = 0.0
                     UVBUFF(OUTP+1) = 0.0
                     UVBUFF(OUTP+2) = 0.0
                     END IF
 260              CONTINUE
               CALL UVZERO (CATBLK, UVBUFF(IDCTR+CATBLK(KIPCN)),
     *            NZERO(1,ZTYPE), GOOD)
               END IF
C                                       Write
 290     IF ((DOKEEP.GT.0.0) .OR. (GOOD)) THEN
            IVCTR = IVCTR + 1
            IDCTR = IDCTR + LREC
C                                       Write this full buffer.
            IF (IVCTR.GE.IVMAX) THEN
               CALL UVDISK ('WRIT', IDLUN, IDFIND, UVBUFF, IVMAX, IDCTR,
     *            IERR)
               IF (IERR.NE.0) GO TO 980
               IVCTR = 0
               END IF
         ELSE
            NSKIP = NSKIP + 1
            END IF
C                                       Test to see if we continue main
C                                       loop.
         IF (DVIS.LT.VISMAX) GO TO 40
C                                       Finish up any pending disk I/O.
      IVCTR = -IVCTR
      CALL UVDISK ('FLSH', IDLUN, IDFIND, UVBUFF, IVCTR, IDCTR, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       not all written
      IF (NSKIP.GT.0) THEN
         VISMAX = VISMAX - NSKIP
         CALL UCMPRS (VISMAX, DISOUT, ISLOT, IDLUN, CATBLK, IERR)
         IERR = 0
         END IF
C                                       save the file on error now
      CALL ZCLOSE (IDLUN, IDFIND, IERR)
C                                       messages
      FRW(1) = 1
      DO 310 ZTYPE = 1,2
         IF (ZTYPE.EQ.1) THEN
            MSGTXT = 'UVFDAT: counts for cross-correlations'
         ELSE
            MSGTXT = 'UVFDAT: counts for auto-correlations'
            END IF
         IF ((NZERO(1,ZTYPE).GT.0) .OR. (NZERO(2,ZTYPE).GT.0))
     *      CALL MSGWRT (4)
         IF (NZERO(1,ZTYPE).GT.0) THEN
            WRITE (MSGTXT,1300) NZERO(1,ZTYPE)
            CALL MSGWRT (4)
            IF (NZERO(3,ZTYPE).GT.0) THEN
               XF = 100.0 * NZERO(1,ZTYPE)
               XF = XF / NZERO(3,ZTYPE)
               WRITE (MSGTXT,1301) XF
               CALL MSGWRT (4)
               END IF
            END IF
         IF (NZERO(2,ZTYPE).GT.0) THEN
            WRITE (MSGTXT,1302) NZERO(2,ZTYPE)
            CALL MSGWRT (4)
            IF (NZERO(3,ZTYPE).GT.0) THEN
               XF = 100.0 * NZERO(2,ZTYPE)
               XF = XF / NZERO(3,ZTYPE)
               WRITE (MSGTXT,1303) XF
               CALL MSGWRT (4)
               END IF
            END IF
 310     CONTINUE
      IF (NSKIP.GT.0) THEN
         WRITE (MSGTXT,1310) NSKIP
         CALL MSGWRT (4)
         END IF
      GO TO 999
C                                       Problem reading tape.
 970  WRITE (MSGTXT,1970) IERR
      GO TO 995
C                                       Problem writing data.
 980  WRITE (MSGTXT,1980) IERR
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('UVFDAT: BUFFERS TOO SMALL FOR THESE DATA; NEED',3I8)
 1300 FORMAT ('UVFDAT: flagged',I9,' valid spectra samples which',
     *   ' were pure 0')
 1301 FORMAT ('UVFDAT: flagged',F9.3,' percent valid samples',
     *   ' which were pure 0')
 1302 FORMAT ('UVFDAT: found',I11,' spectra samples already flagged')
 1303 FORMAT ('UVFDAT: found',F11.3,' percent samples already flagged')
 1310 FORMAT ('UVFDAT: omitted',I11,' totally flagged vis records')
 1970 FORMAT ('UVFDAT: ERROR READING TAPE. IERR=', I6)
 1980 FORMAT ('UVFDAT: ERROR WRITTING TO DISK. IERR=', I6)
      END
      SUBROUTINE FITRXU (ICNO, LAST, EOF, IERR)
C-----------------------------------------------------------------------
C  This routine will read all fits extension files associated with UV
C  data and process the ones it recognizes (XTENSION = 'TABLE'
C  and 'BINTABLE' (or temporary names 'A3DTABLE' &  '3D TABLE')).
C  Inputs:
C     ICNO    I    Catalog number of the UV file.
C     HLUN    I    History file LUN. Already open (from common).
C     LAST    L    last piece?
C  OUTPUTS:
C     EOF     L    An end of file was read during processing.
C     IERR    I    Error code. 0=ok.
C-----------------------------------------------------------------------
      INTEGER   ICNO, IERR
      LOGICAL   LAST, EOF
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'SCRBFS.INC'
C                                       MXTBKW=max. no. table keywords
      INTEGER   MXTBKW
      PARAMETER (MXTBKW = 200)
c
      CHARACTER KEYWRD(MXTBKW)*8, KEYCHR(MXTBKW)*8, TAB3D(3)*8,
     *   HILINE*72
      HOLLERITH KEYH(2)
      DOUBLE PRECISION  NBITS, AXCNT, KEYVAL(MXTBKW), KEYD
      REAL      KEYR
      INTEGER   I, II, ICARD, ANLUN, INBLK, IKEY, JERR,
     *   HBUFF(256), IVER, TABLUN, SRTORD, DATP(128,2), BUFFER(512),
     *   NUMKEY, KEYI, KEYTYP(MXTBKW), KEYLOC(MXTBKW), KEYV(2*MXTBKW),
     *   LENKEY(5), JT, JTRIM
      LOGICAL   EXTEN, KEYL, DOHDR, WASSU
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (KEYL, KEYI, KEYH, KEYR, KEYD)
      DATA TAB3D /'BINTABLE', 'A3DTABLE', '3D TABLE'/
      DATA ANLUN, TABLUN /28, 29/
C-----------------------------------------------------------------------
C                                       Length of keyword values
      LENKEY(1) = NWDPDP
      LENKEY(2) = 1
      LENKEY(3) = 2
      LENKEY(4) = 1
      LENKEY(5) = 1
      CALL HIOPEN (IHLUN, DISOUT, ICNO, HBUFF, IERR)
      WASSU = .FALSE.
C                                       Loop for all FITS extensions.
      DO 200 I = 1,32000
         NUMKEY = 0
C                                       Process all required FITS table
C                                       cards.
         CALL EXTREQ (FDVEC, TBIND, TAPBUF, FITBLK, ICARD, EXTEN, EOF,
     *      UNKNWN, IERR)
         IF (EOF) GO TO 900
         IF (.NOT.EXTEN) GO TO 900
         IF (IERR.NE.0) GO TO 910
C                                       Calculate no. of data blocks.
         AXCNT = 1.0D0
         DO 20 II = 1,NAXIS
            AXCNT = AXCNT * NAXISI(II)
 20         CONTINUE
         AXCNT = AXCNT + PCOUNT
         NBITS = ABS (BITPIX) * GCOUNT * AXCNT
         INBLK = INT ((NBITS + 23039.0D0) / 23040.0D0)
         DOHDR = .FALSE.
C                                       See if we have an ASCII table.
         IF (EXTTYP.NE.'TABLE') GO TO 40
            IF (NAXISI(1).LE.2880) GO TO 25
               WRITE (MSGTXT,1020) NAXISI(1)
               CALL MSGWRT (8)
               GO TO 100
C                                       initialize default values.
 25         CALL SETDEF
C                                       Process table file header.
            NUMKEY = MXTBKW
            CALL TABHDR (FDVEC, TBIND, ICARD, IHLUN, HBUFF, 0,
     *         NUMKEY, KEYWRD, KEYVAL, KEYCHR, KEYTYP, TAPBUF,
     *         FITBLK, IERR)
            IF (IERR.LT.0) GO TO 100
            IF (IERR.NE.0) GO TO 910
C                                       Process AN table and write
C                                       AN file records.
            IF (ITYPE.NE.'AN') GO TO 30
               CALL ANTAB (FDVEC, TBIND, DISOUT, ICNO, ANLUN, NAXISI,
     *            TAPBUF, IERR)
C                                       Add AIPS AN history
               WRITE (MSGTXT,1025)
               CALL EXTHIS (IHLUN, HBUFF, IERR)
               GO TO 200
C                                       Normal table files. Special
C                                       processing if its an AIPS style
C                                       table.
 30            SRTORD = 0
               IF (ITYPE.NE.'UK') CALL ATCONV (NUMKEY, KEYWRD,
     *            KEYVAL, KEYTYP, KEYCHR, SRTORD)
C                                       Create and initialize the table
C                                       header with data in common.
               IVER = 0
               CALL MAKTAB (SRTORD, DISOUT, ICNO, IVER, CATBLK, TABLUN,
     *            DATP, BUFFER, IERR)
               IF (IERR.GT.0) GO TO 110
C                                       Prepare keywords
               IKEY = 1
               DO 35 II = 1,NUMKEY
                  KEYLOC(II) = IKEY
                  IF (KEYTYP(II).EQ.2) KEYTYP(II) = 1
                  IF (KEYTYP(II).EQ.1) KEYD = KEYVAL(II)
                  IF (KEYTYP(II).EQ.3) THEN
                     JT = JTRIM (KEYCHR(II))
                     CALL CHR2H (8, KEYCHR(II), 1, KEYH)
                     END IF
                  IF (KEYTYP(II).EQ.4) KEYI = KEYVAL(II)
                  IF (KEYTYP(II).EQ.5) KEYL = KEYVAL(II).GT.0.0D0
                  CALL COPY (LENKEY(KEYTYP(II)), KEYD, KEYV(IKEY))
                  IKEY = IKEY + LENKEY(KEYTYP(II))
 35               CONTINUE
C                                       Write keywords
               CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER, KEYLOC,
     *            KEYV, KEYTYP, IERR)
               IF (IERR.NE.0) THEN
                  CALL TABIO ('CLOS', 0, II, BUFFER, BUFFER, IERR)
                  GO TO 110
                  END IF
C                                       Read the data from tape and
C                                       write to the table disk file.
               CALL RWTAB (FDVEC, TBIND, DATP, NAXISI, BUFFER, TAPBUF,
     *            IERR)
               EOF = EOF .OR. (IERR.EQ.4)
               IF (IERR.NE.0) GO TO 910
               GO TO 200
C                                       See if we have a 3-D table.
 40      IF ((EXTTYP.NE.TAB3D(1)) .AND. (EXTTYP.NE.TAB3D(2)) .AND.
     *      (EXTTYP.NE.TAB3D(3))) GO TO 100
C                                       initialize default values.
            CALL SETDEF
C                                       Process table file header.
            NUMKEY = MXTBKW
            CALL TABHDR (FDVEC, TBIND, ICARD, IHLUN, HBUFF, 1, NUMKEY,
     *         KEYWRD, KEYVAL, KEYCHR, KEYTYP, TAPBUF, FITBLK, IERR)
            IF (IERR.LT.0) GO TO 100
            IF (IERR.NE.0) GO TO 910
C                                       Normal table files. Special
C                                       processing if its an AIPS style
C                                       table.
            IF (ITYPE.NE.'UV') THEN
               SRTORD = 0
               IF (ITYPE.NE.'UK') CALL ATCONV (NUMKEY, KEYWRD, KEYVAL,
     *            KEYTYP, KEYCHR, SRTORD)
C                                       Create and initialize the table
C                                       header with data in common.
               IVER = 0
               IF (UVTABL) IVER = EXTVER
               CALL MAKTAB (SRTORD, DISOUT, ICNO, IVER, CATBLK, TABLUN,
     *            DATP, BUFFER, IERR)
               IF (IERR.GT.0) GO TO 110
C                                       file already exists
               IF ((UVTABL) .AND. (IERR.EQ.0)) THEN
                  IF ((ITYPE.NE.'CL') .AND. (ITYPE.NE.'SN') .AND.
     *               (ITYPE.NE.'TY') .AND. (ITYPE.NE.'MC') .AND.
     *               (ITYPE.NE.'IM') .AND. (ITYPE.NE.'BP')) THEN
                     CALL TABIO ('CLOS', 0, II, BUFFER, BUFFER, IERR)
                     GO TO 110
                     END IF
                  END IF
C                                       Prepare keywords
               IF ((IERR.EQ.-1) .OR. (.NOT.UVTABL)) THEN
                  IERR = 0
                  IKEY = 1
                  DO 80 II = 1,NUMKEY
                     KEYLOC(II) = IKEY
                     IF (KEYTYP(II).EQ.2) KEYTYP(II) = 1
                     IF (KEYTYP(II).EQ.1) KEYD = KEYVAL(II)
                     IF (KEYTYP(II).EQ.3) THEN
                        JT = JTRIM (KEYCHR(II))
                        CALL CHR2H (8, KEYCHR(II), 1, KEYH)
                        END IF
                     IF (KEYTYP(II).EQ.4) KEYI = KEYVAL(II)
                     IF (KEYTYP(II).EQ.5) KEYL = KEYVAL(II).GT.0.0D0
                     CALL COPY (LENKEY(KEYTYP(II)), KEYD, KEYV(IKEY))
                     IKEY = IKEY + LENKEY(KEYTYP(II))
 80                  CONTINUE
C                                       Write keywords
                  CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER, KEYLOC,
     *               KEYV, KEYTYP, IERR)
                  IF (IERR.NE.0) THEN
                     CALL TABIO ('CLOS', 0, II, BUFFER, BUFFER, IERR)
                     GO TO 110
                     END IF
                  END IF
C                                       Read the data from tape and
C                                       write to the table disk file.
               CALL R3DTAB (FDVEC, TBIND, DATP, NAXISI, BUFFER, TAPBUF,
     *            IERR)
               EOF = EOF .OR. (IERR.EQ.4)
               IF (IERR.NE.0) GO TO 910
               IF (ITYPE.EQ.'SU') WASSU = .TRUE.
C                                       UV data table
            ELSE
               CALL RUVTAB (NPV, FDVEC, TBIND, NAXISI, DISOUT, ICNO,
     *            LAST, NSKIP, DOKEEP, UVBUFF, TUVBUF, TAPBUF, IERR)
               EOF = EOF .OR. (IERR.EQ.4)
               IF (IERR.NE.0) GO TO 910
               END IF
            GO TO 200
C                                       Skip unknown extension file.
 100     CONTINUE
C                                       read rest header code
            DOHDR = .TRUE.
C                                       else header already read
 110        CALL SKPEXT (DOHDR, FDVEC, TBIND, IHLUN, ICARD, INBLK,
     *         HBUFF, TAPBUF, FITBLK, IERR)
            IF (IERR.NE.0) GO TO 910
 200     CONTINUE
C                                       Shouldn't get here.
      MSGTXT = 'MORE THAN 32000 EXTENSION FILES. SOME NOT PROCESSED'
      CALL MSGWRT (8)
C                                       zero records flagged
 900  IF (NZERO(1,1).GT.0) THEN
         WRITE (HILINE,1900) TSKNAM, NZERO(1,1), 'cross'
         CALL HIADD (IHLUN, HILINE, HBUFF, JERR)
         END IF
      IF (NZERO(1,2).GT.0) THEN
         WRITE (HILINE,1900) TSKNAM, NZERO(1,2), 'auto'
         CALL HIADD (IHLUN, HILINE, HBUFF, JERR)
         END IF
C
 910  CALL HICLOS (IHLUN, .TRUE., HBUFF, JERR)
C                                       source table apparent positions
      IF (WASSU) CALL SUPREC (DISOUT, ICNO, CATBLK, JERR)
      CALL CATIO ('UPDT', DISOUT, ICNO, CATBLK, 'CLWR', SCRBUF, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1910) JERR, 'CATBLK UPDATE'
         CALL MSGWRT (8)
         END IF
      NCFILE = 0
      IERR = MAX (0, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('FITRXU: ASCII TABLE ROW LENGTH',I7,' TOO LONG FOR ME')
 1025 FORMAT ('FITLD / AIPS antennas table')
 1900 FORMAT (A,'NZERO=',I10,' / ',A,'-corr spectra flagged as pure 0')
 1910 FORMAT ('ERROR',I5,' DOING ',A)
      END
      SUBROUTINE UVFEXT (ICNO, IRET)
C-----------------------------------------------------------------------
C   UVFEXT processes records following the normal FITS image.  If
C   TABLES <= 0, it simply counts the number of such records.  Else,
C   it parses through the Table records creating the appropriate
C   extension files and adding the table header cards to the
C   history file.  This is for the old tables of AIPS style.
C   Inputs:
C      ICNO    I         Catalog slot number of UV file.
C      HLUN   I         LUN of open history file (from common).
C   Output:
C      IRET   I         Error code: 0 => ok, 8 => some error
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  CHFREQ*8, ISTR*80, SYM*8, CARD*80, CTYPES(1)*4,
     *   SYMS(15)*8, TABNAM*8, TTYPE(10)*8
      DOUBLE PRECISION C, X, DABLK(1), JD, DEG2RD, DELDAT, GASTM
      REAL      ABLK(1)
      INTEGER   HBUFF(256), IRET, IOFF, IERR, TERR, TABVER, J,
     *   TABCNT, TABWID, TABCRD, NC, INC, NPNT, ITYP, NSYMS, NCHAR,
     *   ITABL, NTYPES, IT, LUN, BUFFER(512), IP, I, ICNO, IREC,
     *   NIF
      LOGICAL   HISERR, NODATA
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FITLD.INC'
      EQUIVALENCE (BUFFER, ABLK, DABLK)
      DATA NSYMS, NTYPES, LUN /15, 1, 28/
      DATA CHFREQ /'FREQ    '/
      DATA CTYPES /'  AN'/
      DATA SYMS /'TTYPE1  ', 'TTYPE2  ', 'TTYPE3  ', 'TTYPE4  ',
     *   'TTYPE5  ', 'TTYPE6  ', 'TTYPE7  ', 'TTYPE8  ',
     *   'TTYPE9  ', 'TTYPE10 ', 'TABNAME ', 'TABVER  ',
     *   'TABCOUNT', 'TABWIDTH', 'TABCARDS'/
      DATA C /.2997924562D0/
C-----------------------------------------------------------------------
      NIF = 1
      IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
      IERR = 0
      TERR = 0
      IRET = 0
      DELDAT = 0.1D0
      DEG2RD = 3.141592653589793D0 / 180.0D0
      IF (TABLES.LE.0) GO TO 910
      CALL HIOPEN (IHLUN, DISOUT, ICNO, HBUFF, IERR)
      HISERR = IERR.NE.0
      IRET = 8
C                                       Loop over tables
      DO 300 ITABL = 1,TABLES
         WRITE (MSGTXT,1000) ITABL
         IF (.NOT.HISERR) CALL HIAD80 (IHLUN, 1, MSGTXT, HBUFF, IERR)
         IF (IERR.NE.0) HISERR = .TRUE.
C                                       Init table parm values
         TABVER = 0
         TABCNT = 0
         TABWID = 0
         TABCRD = 0
         TABNAM = ' '
         DO 20 I = 1,10
            TTYPE(I) = '        '
 20         CONTINUE
C                                       Read and parse header
         DO 90 IREC = 1,100
C                                       1st Block read in EXTREQ.
            IF (IREC.EQ.1) GO TO 50
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
               IF (TERR.EQ.0) GO TO 30
                  WRITE (MSGTXT,1020) TERR
                  GO TO 890
 30            NC = 0
               CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
C                                       card loop
 50         NC = NC + 1
            IF (NC.GT.36) GO TO 90
C                                       card to history
            INC = (NC-1) * 80 + 1
            CARD = FITBLK(INC:INC+79)
            IF (HISERR) GO TO 55
               IF (CARD(1:4).NE.'END ') CALL HIAD80 (IHLUN, 1, CARD,
     *            HBUFF, IERR)
               HISERR = IERR.NE.0
C                                       Parse
 55         NPNT = 1
            CALL GETSYM (CARD, NPNT, SYM, ITYP)
            IF (SYM.EQ.'END ') GO TO 100
C                                       only keyword = value accepted
            IF (ITYP.NE.0) GO TO 50
            DO 60 I = 1,NSYMS
               IF (SYM.EQ.SYMS(I)) GO TO 70
 60            CONTINUE
            GO TO 50
C                                       Numeric keywords
 70         IF (I.LE.11) GO TO 80
               CALL GETNUM (CARD, 80, NPNT, X)
               IF (X.EQ.DBLANK) GO TO 875
               IF (I.EQ.12) TABVER = X + 0.01
               IF (I.EQ.13) TABCNT = X + 0.01
               IF (I.EQ.14) TABWID = X + 0.01
               IF (I.EQ.15) TABCRD = X + 0.01
               GO TO 50
C                                       Got a string variable
 80         CONTINUE
               CALL GETSTR (CARD, 80, 68, NPNT, ISTR, NCHAR)
               NCHAR = MIN (NCHAR, 8)
               IF (I.EQ.11) TABNAM = ISTR(1:NCHAR)
               IF (I.LT.11) TTYPE(I) = ISTR(1:NCHAR)
               GO TO 50
 90         CONTINUE
         WRITE (MSGTXT,1090) ITABL
         GO TO 890
C                                       END card found
C                                       null table
 100     IF ((TABCNT.GT.0) .AND. (TABWID.GT.0)) GO TO 105
            WRITE (MSGTXT,1100) ITABL
            CALL MSGWRT (6)
            GO TO 270
C                                       illegal format
 105     IF ((TABCRD.GT.0) .AND. (TABCRD.LE.40)) GO TO 110
            WRITE (MSGTXT,1105) TABCRD, ITABL
            GO TO 890
C                                       A recognized type?
 110     NODATA = .TRUE.
         IF (TABNAM(1:4).NE.'AIPS') GO TO 125
            DO 115 IT = 1,NTYPES
               IF (CTYPES(IT).EQ.TABNAM(5:8)) GO TO 120
 115           CONTINUE
            GO TO 125
C                                       Yes: do it - AN files only
 120     IF (IT.NE.1) GO TO 125
C                                       Setup for AN table initization
            NUMORB = 0
            NOPCAL = 2
            ANFQID = -1
            ANTNIF = NIF
C                                       Position of the earth's pole
            POLRXY(1) = 0.0
            POLRXY(2) = 0.0
            UT1UTC = 0.0
            DATUTC = 0.0
C                                       Array name
            CALL H2CHR (8, 1, CATH(KHTEL), ANAME)
C                                       Array center (rel to center of
C                                       earth)
            ARRAYC(1) = 0.0D0
            ARRAYC(2) = 0.0D0
            ARRAYC(3) = 0.0D0
C                                       Get GST0 and Earth rotation rate
            CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
            CALL JULDAY (RDATE, JD)
            CALL GSTROT (JD, GSTIA0, GASTM, DEGPDY)
C                                       Get frequency
            IOFF = 0
            CALL AXEFND (4, CHFREQ, KICTPN, CATH(KHCTP), IOFF, IERR)
            SAFREQ = CATD(KDCRV+IOFF)
C                                       init basic AN record
            ANNAME = ' '
            STAXOF = 0.0
            STAXYZ(1) = 0.0D0
            STAXYZ(2) = 0.0D0
            STAXYZ(3) = 0.0D0
            ORBPRM(1) = 0.0D0
            NOSTA = 0
            MNTSTA = 0
            POLAA = 0.0
            POLAB = 0.0
            CALL RFILL (3, 0.0, POLCA)
            CALL RFILL (3, 0.0, POLCB)
            POLTYA = 'R'
            POLTYB = 'L'
C                                       Create/init file
            CALL ANTINI ('WRIT', BUFFER, DISOUT, ICNO, TABVER, CATBLK,
     *         LUN, IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY,
     *         SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME,
     *         XYZHAN, TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
            IF (IERR.EQ.0) GO TO 125
               WRITE (MSGTXT,1120) IERR
               CALL MSGWRT (7)
C
 125     IP = TABCRD
         NODATA = .FALSE.
         DIAMAN = 0.0
         CALL RFILL (NIF, 0.0, FWHMAN)
         DO 260 I = 1,TABCNT
            DO 250 J = 1,TABWID
               IP = IP + 1
               IF (IP.LE.TABCRD) GO TO 140
                  NC = NC + 1
C                                       read a record
                  IF (NC.LE.36) GO TO 130
                     NC = 1
                     CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
                     CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
                     IF (TERR.EQ.0) GO TO 130
                        WRITE (MSGTXT,1020) TERR
                        GO TO 890
C                                       spread new card
 130              IP = 1
                  IF (NODATA) GO TO 140
                     INC = (NC-1) * 80 + 1
                     CARD = FITBLK(INC:INC+79)
                     NPNT = 1
 140              IF (NODATA) GO TO 250
C                                       Set proper Ant rec variable.
                     IF (J.NE.2) THEN
                        CALL GETNUM (CARD, 80, NPNT, X)
                        IF (X.EQ.DBLANK) GO TO 875
                        END IF
                     GO TO (150, 160, 170, 180, 190), J
C                                       Station number.
 150                    NOSTA = X + .5
                        GO TO 250
C                                       Station name.
 160                    CALL GETSTR (CARD, 80, 8, NPNT, ISTR, NCHAR)
                        ANNAME = ISTR(1:NCHAR)
C                                       STATION X
 170                    STAXYZ(1) = C * X
                        GO TO 250
C                                       STATION Y
 180                    STAXYZ(2) = C * X
                        GO TO 250
C                                       STATION Z
 190                    STAXYZ(3) = C * X
 250              CONTINUE
               IF (NODATA) GO TO 260
                  IANRNO = NOSTA
                  CALL TABAN ('WRIT', BUFFER, IANRNO, ANKOLS, ANNUMV,
     *               ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF,
     *               DIAMAN, FWHMAN, POLTYA, POLAA, POLCA, POLTYB,
     *               POLAB, POLCB, IERR)
                  IF (IERR.NE.0) GO TO 900
 260           CONTINUE
C                                       Close ant file.
               CALL TABIO ('CLOS', 1, IANRNO, BUFFER, BUFFER, IERR)
               WRITE (MSGTXT,1267) 'AN', TABVER
               CALL MSGWRT (2)
               GO TO 300
C                                       Data ignored
 270        CONTINUE
               WRITE (MSGTXT,1270) ITABL
               CALL MSGWRT (2)
               IF (HISERR) GO TO 300
                  CALL HIAD80 (IHLUN, 1, MSGTXT, HBUFF, IERR)
                  HISERR = IERR.NE.0
 300     CONTINUE
      IRET = 0
      GO TO 900
C
 875  MSGTXT = 'UVFEXT: ERROR PARSING NUMBER ON  ' // CARD
      IRET = 1
 890  CALL MSGWRT (8)
C                                       OK if EOF found
      IF (TERR.EQ.4) IRET = 0
C                                       Read rest of tape
 900  CALL HICLOS (IHLUN, .TRUE., HBUFF, IERR)
 910  IF (TERR.NE.4) CALL MLREOF (FDVEC, TBIND, UNKNWN, TAPBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FITLD  / Header for table',I7)
 1020 FORMAT ('UVFEXT: FITS IO ERROR',I7)
 1090 FORMAT ('UVFEXT: NO END TO TABLE HEADER #',I7)
 1100 FORMAT ('TABLE',I7,' HAS NO DATA')
 1105 FORMAT ('UVFEXT: TABCARDS=',I7,' ILLEGAL')
 1120 FORMAT ('UVFEXT: UNABLE TO CREATE EXTENSION FILE',I7)
 1267 FORMAT ('Extension file type ',A2,' version',I4,' written')
 1270 FORMAT ('FITLD / Table',I7,' skipped')
      END
      SUBROUTINE REQCD (ICARD, IERR)
C-----------------------------------------------------------------------
C   This routine will look for the required cards in a FIT header block
C   SIMPLE, BITPIX, NAXIS, NAXISn, and update a catalog header with the
C   information from these cards.
C   Inputs:  FITBLK    C*2880 a block of fit header data.
C   Outputs: ICARD        I   The number of the last card parsed.
C            IERR         I   0=ok, 1=messed up. An error message will
C                                     be printed.
C   COMMON /MAPHDR/ Axis dimension information will be filled in.
C   COMMON /FITINF/ Sets GROUP to 1 if NAXIS1 is zero, else 0.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER KL*80, SYMBOL*8
      DOUBLE PRECISION    X
      INTEGER   ICARD, NPNT, IERR, ITYP, NAXIS, ITABNO, IVAL, I, IAX,
     *   IKEYWD, ICLAST
      LOGICAL   ISHIST, END
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFUV.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:VFUV.INC'
C-----------------------------------------------------------------------
      BINTAB = .FALSE.
C                                       Look for SIMPLE=T card
      ICARD = 1
      IKEYWD = 1
      NPNT = 1
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
      USED(IKEYWD) = USED(IKEYWD) + 1
      CALL GETLG (KL, 80, NPNT, ITYP)
C                                       Not .TRUE.
      IF (ITYP.NE.1) GO TO 940
C                                       Look for BITPIX.
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
      USED(IKEYWD) = USED(IKEYWD) + 1
C                                       Check value of BITPIX
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      IF (X.GE.0.0) IVAL = X + 0.1
      IF (X.LT.0.0) IVAL = X - 0.1
      IBPP = IVAL
      IF ((IVAL.NE.8) .AND. (IVAL.NE.16) .AND. (IVAL.NE.32) .AND.
     *   (IVAL.NE.-32) .AND. (IVAL.NE.-64)) GO TO 950
      IF (IVAL.EQ.-64) THEN
         MSGTXT = 'WARNING: 64-bit input stored in 32 bits inside AIPS'
         CALL MSGWRT (6)
         END IF
C                                       Check NAXIS
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 980
      USED(IKEYWD) = USED(IKEYWD) + 1
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      NAXIS = X + .01
      GROUP = 0
C
      IF (NAXIS.GT.0) THEN
C                                       Check NAXIS1 for group format.
         ICARD = ICARD + 1
         IKEYWD = IKEYWD + 1
         NPNT = 1
         CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *      SYMBOL, ITABNO, ISHIST, END, IERR)
         IF (IERR.NE.0) GO TO 980
         USED(IKEYWD) = USED(IKEYWD) + 1
         CALL GETNUM (KL, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 975
         IVAL = X + .01
         IAX = KINAX
C                                       Set values for group structure.
         IF (IVAL.EQ.0) THEN
            GROUP = 1
            CATBLK(KIDIM) = NAXIS - 1
C                                       Reset values for non group.
         ELSE
            GROUP = 0
            CATBLK(KIDIM) = NAXIS
            CATBLK(IAX) = IVAL
            IAX = IAX + 1
            END IF
C                                       Check NAXISm
         DO 30 I = 2,NAXIS
            ICARD = ICARD + 1
            IKEYWD = IKEYWD + 1
            NPNT = 1
            CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *         SYMBOL, ITABNO, ISHIST, END, IERR)
            IF (IERR.NE.0) GO TO 970
            USED(IKEYWD) = USED(IKEYWD) + 1
            CALL GETNUM (KL, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 975
            CATBLK(IAX) = X + .01
            IAX = IAX + 1
 30         CONTINUE
         END IF
C                                       check if uv binary tables
      ICLAST = ICARD
      IF ((NAXIS.EQ.0) .AND. (IBPP.EQ.8)) BINTAB = .TRUE.
      UVTABL = (NAXIS.EQ.2) .AND. (CATBLK(KINAX)/100.EQ.7777777) .AND.
     *   (CATBLK(KINAX+1).EQ.0)
      GO TO 999
C                                       Not SIMPLE FITS tape.
 940  WRITE (MSGTXT,1940)
      GO TO 980
C                                       Invalid bits per pixel value.
 950  WRITE (MSGTXT,1950) IVAL
      GO TO 980
C                                       Expected keyword not found.
 970  WRITE (MSGTXT,1970) CWORD(IKEYWD), SYMBOL
      GO TO 980
 975  MSGTXT = 'REQCD: ERROR IN NUMBER ON ' // SYMBOL
C                                       Print error message set flag.
 980  CALL MSGWRT (6)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1940 FORMAT ('NOT SIMPLE FITS TAPE. PROGRAM STOPPING.')
 1950 FORMAT ('INVALID BITS PER PIXEL =',I6)
 1970 FORMAT ('EXPECTED KEYWORD ',A8,'. FOUND ',A8,'.')
      END
      SUBROUTINE PARSCD (ICARD, END, IERR)
C-----------------------------------------------------------------------
C   PARSCD (parse FITS card) will unpack and interpret a card image
C   from a block of FITS data and put that data into the internal AIPS
C   header.
C   Inputs:
C      ICARD   I         The card number (1-36) in block to interpret.
C      FITBLK  C*2880    A block of FITS header data.
C   Outputs:
C      END     L         True if end card found, else false.
C      IERR    I         error code 0=ok. 1=error.
C   COMMON /MAPHDR/
C   COMMON /FITINF/
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER KL*80, SYMBOL*8, STR*80, CHTEMP*24
      DOUBLE PRECISION X
      REAL      VAL
      INTEGER   KPNTR(58), PNTR, IPOFF, TABNO, JERR, IERR, ICARD, NPNT,
     *   KT, IL, IVAL, NCHAR, NBYT, NN, NNSTR, IT, JT, JTRIM, NPNTS
      LOGICAL   ISHIST, END, LHIST, FIRST
      INCLUDE 'INCS:DFUV.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FITLD.INC'
      EQUIVALENCE (KPNTR(1), KHOBJ)
      INCLUDE 'INCS:VFUV.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find next symbol on the card
C                                       and look for it in the table.
      NPNT = 1
      FIRST = .TRUE.
      NN = NKT + NCT
      NNSTR = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
 10   CALL GETCRD (ICARD, NN, NNSTR, AWORD, FITBLK, NPNT, KL, SYMBOL,
     *    TABNO, LHIST, END, JERR)
      IF (END) GO TO 999
      IF (FIRST) ISHIST = LHIST
      FIRST = .FALSE.
      IF (ISHIST) THEN
         IF (XERROR.GT.1.5) GO TO 999
         IF (JERR.EQ.1) GO TO 10
         IF ((USED(TABNO).GT.0) .AND. (JERR.EQ.0)) GO TO 10
         END IF
      IF ((JERR.EQ.1) .AND. ((SYMBOL(:2).EQ.'CD') .OR.
     *   (SYMBOL(:2).EQ.'PC') .OR. (SYMBOL(:2).EQ.'PV')))
     *   CALL PCCARD (GROUP, KL, PCMATX, CDMATX, PVMATX)
      IF (JERR.NE.0) GO TO 999
      IF (.NOT.ISHIST) USED(TABNO) = USED(TABNO) + 1
C                                       Header pointer for this
C                                       keyword, number bytes and
C                                       offset position from pointer
      PNTR = MOD (APOINT(TABNO), 1000)
      IPOFF = PNTR / 100
      PNTR = MOD (PNTR, 100)
      IF (PNTR.GT.0) PNTR = KPNTR(PNTR)
      NBYT = APOINT(TABNO) / 1000
C                                       Readjust axis pointers for grp
      IF ((GROUP.EQ.1) .AND. (TABNO.GE.21) .AND. (TABNO.LE.60))
     *   IPOFF = IPOFF - 1
C                                       Type value of keyword
C                                       1=LOGICAL
C                                       2=NUMBER
C                                       3=STRING
      KT = ATYPE(TABNO)
C                                       Logical value
      NPNTS = NPNT
      IF (KT.EQ.1) THEN
         CALL GETLG (KL, 80, NPNT, IL)
C                                       Illegal logical value.
         IF (IL.LT.0) THEN
            WRITE (MSGTXT,1100) SYMBOL
            GO TO 990
            END IF
C                                       Check for logical value
C                                       special cases.
         IF (AWORD(TABNO).EQ.'GROUPS') THEN
C                                       Structure indicated by NAXIS
C                                       and GROUP don't match.
            IF (GROUP.NE.IL) THEN
               IERR = 1
               WRITE (MSGTXT,1110)
               GO TO 990
               END IF
C                                       Handle normal logical cases.
         ELSE
            CATBLK(PNTR+IPOFF) = IL
            END IF
C                                       Number
      ELSE IF (KT.EQ.2) THEN
         CALL GETNUM (KL, 80, NPNT, X)
C                                       special parse for EQUINOX
         IF (X.EQ.DBLANK) THEN
            IF ((AWORD(TABNO).EQ.'EQUINOX') .OR.
     *         (AWORD(TABNO).EQ.'EPOCH')) THEN
               NPNT = NPNTS
               CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
               IF (INDEX(STR,'1950').GT.0) THEN
                  X = 1950.0D0
               ELSE IF (INDEX(STR,'2000').GT.0) THEN
                  X = 2000.0D0
                  END IF
               END IF
            END IF
         IF (X.EQ.DBLANK) THEN
            MSGTXT = 'PARSCD: NUMBER ERROR ON ' // SYMBOL
            CALL MSGWRT (7)
            X = 0.0D0
            END IF
C                                       Check for number special cases.
C                                       Blank pixel value.
         IF (AWORD(TABNO).EQ.'BLANK') THEN
            IF (X.EQ.-2147483648.0D0) THEN
               IBLANK = -2147483647 - 1
            ELSE
               IBLANK = X
               END IF
            ISBLNK = .TRUE.
C                                       Scaling factors and offsets for
C                                       random parms (used in IMGDAT).
         ELSE IF (AWORD(TABNO)(:5).EQ.'PSCAL') THEN
            CALL GETI (AWORD(TABNO), IT)
            PSCAL(IT) = X
         ELSE IF (AWORD(TABNO)(:5).EQ.'TSCAL') THEN
            CALL GETI (AWORD(TABNO), IT)
            PSCAL(IT) = X
         ELSE IF (AWORD(TABNO)(:5).EQ.'PZERO') THEN
            CALL GETI (AWORD(TABNO), IT)
            POFF(IT) = X
         ELSE IF (AWORD(TABNO)(:5).EQ.'TZERO') THEN
            CALL GETI (AWORD(TABNO), IT)
            POFF(IT) = X
C                                       Handle normal cases. Put value
C                                       into proper header slot.
C                                       4-byte integer
         ELSE IF (NBYT.EQ.2) THEN
            IVAL = X + SIGN (0.5D0, X)
            IF (AWORD(TABNO).EQ.'TABLES') TABLES = IVAL
            IF (PNTR.GT.0) CATBLK(PNTR+IPOFF) = IVAL
            IF (AWORD(TABNO).EQ.'IPIECE  ') THEN
               NPV(1) = IVAL
            ELSE IF (AWORD(TABNO).EQ.'NPIECE  ') THEN
               NPV(2) = IVAL
            ELSE IF (AWORD(TABNO).EQ.'FIRSTVIS') THEN
               NPV(3) = IVAL
            ELSE IF (AWORD(TABNO).EQ.'LASTVIS ') THEN
               NPV(4) = IVAL
               END IF
C                                       4-byte real
         ELSE IF (NBYT.EQ.4) THEN
            VAL = X
            IF (PNTR.GT.0) CATR(PNTR+IPOFF) = VAL
            IF (AWORD(TABNO).EQ.'WTSCAL') WTSCAL = X
C                                       8-byte real
         ELSE IF (NBYT.EQ.8) THEN
            IF (PNTR.GT.0) CATD(PNTR+IPOFF) = X
            IF (AWORD(TABNO).EQ.'BSCALE') BSC = X
            IF (AWORD(TABNO).EQ.'BZERO') BZE = X
            END IF
C                                       String
      ELSE IF (KT.EQ.3) THEN
         CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
C                                       Dates are special
         IF (AWORD(TABNO)(:4).EQ.'DATE') THEN
            CALL DATFST ('F2L', STR)
            NCHAR = 8
            END IF
         NCHAR = MIN (NBYT, NCHAR)
C                                       IMCLASS
         IF (AWORD(TABNO).EQ.'IMCLASS') THEN
            IPOFF = NBYT * IPOFF + 1
            CHTEMP = ' '
            CALL CHR2H (NBYT, CHTEMP, IPOFF, CATH(PNTR))
            JT = JTRIM (STR(:NCHAR))
            CALL CHR2H (NCHAR, STR(1:NCHAR), IPOFF, CATH(PNTR))
         ELSE
            IPOFF = ((NBYT+3)/4) * IPOFF
C                                       Start string on integer boundary
            IF (AWORD(TABNO)(:7).EQ.'SORTORD') THEN
               CATH(PNTR+IPOFF) = HBLANK
               JT = JTRIM (STR(:NCHAR))
               CALL CHR2H (NCHAR, STR(1:NCHAR), 1, CATH(PNTR+IPOFF))
C                                       Random parameter type
            ELSE IF (AWORD(TABNO)(:5).EQ.'PTYPE') THEN
               CALL GETI (AWORD(TABNO), IT)
               PTYPES(IT) = STR(1:NCHAR)
C                                       Start string on real boundary.
            ELSE
               CHTEMP = ' '
               CALL CHR2H (NBYT, CHTEMP, 1, CATH(PNTR+IPOFF))
               JT = JTRIM (STR(:NCHAR))
               CALL CHR2H (NCHAR, STR(1:NCHAR), 1, CATH(PNTR+IPOFF))
               END IF
            END IF
         END IF
C                                       Loop on History cards
      IF (ISHIST) GO TO 10
      GO TO 999
C                                       Error message
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A8,' LOGICAL VARIABLE HAS ILLEGAL VALUE')
 1110 FORMAT ('NAXIS AND GROUP STRUCTURE DO NOT MATCH')
      END
      SUBROUTINE ANTAB (FDVEC, TBIND, IVOL, ICNO, ANLUN, NAXIS,
     *   TAPBUF, IERR)
C-----------------------------------------------------------------------
C  This routine will read the data section of a FITS extension file
C  of type TABLE and with the EXTNAM of AIPS AN (antenna), decode this
C  information using data obtained from the header section of the
C  extension file, and write the AIPS version of the ANtenna file.
C  Inputs:
C     FDVEC    I(50)     File descriptor vector for TAPIO input
C     IVOL     I         Disk volume number of map.
C     ICNO     I         Catalog number of map.
C     ANLUN    I         AIPS LUN to use for AN file
C     NAXIS    I(2)      Length of card, number of cards in table.
C     IHDR     I(256)    Catalog header of map.
C  Input/output:
C     TAPBUF   I(*)      TAPIO buffer
C     TBIND    I         Buffer pointer in TAPBUF
C  Outputs:
C     IERR     I        Error code. 0=ok.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  CHFREQ*8, LINE*1024, TLINE*1024, ANTNO*8, STATON*8,
     *   LX*8, LY*8, LZ*8
      INTEGER   NAXIS(2), IVOL, ICNO, ANLUN, FDVEC(40), TBIND, IERR,
     *   TAPBUF(*), NIF
      DOUBLE PRECISION X, DABLK(128), JD, DEG2RD, DELDAT, GASTM
      REAL      ANBUF(1)
      INTEGER   NCTR, EXTVER, INDEXX, I, NAXIS1, NAXIS2, LCTR, NPNTR,
     *   IWIDTH, IFRAC, IANBUF(512), TORDER(5), IOFF, JT, JTRIM
      LOGICAL   EQUAL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (DABLK, ANBUF, IANBUF)
      DATA CHFREQ /'FREQ'/
      DATA ANTNO, STATON /'ANT NO. ','STATION '/
      DATA LX, LY, LZ /'LX      ','LY      ','LZ      '/
C-----------------------------------------------------------------------
      NIF = 1
      IF (JLOCIF.GE.0) NIF = MAX (1, CATBLK(KINAX+JLOCIF))
      NAXIS1 = NAXIS(1)
      NAXIS2 = NAXIS(2)
C
      DEG2RD = 3.141592653589793D0 / 180.0D0
      DELDAT = 0.1D0
C                                       Setup for AN table initization
      NUMORB = 0
      NOPCAL = 2
      ANTNIF = NIF
C                                       Position of the earth's pole
      POLRXY(1) = 0.0
      POLRXY(2) = 0.0
      UT1UTC = 0.0
      DATUTC = 0.0
C                                       Array name
      CALL H2CHR (8, 1, CATH(KHTEL), ANAME)
C                                       Array center (rel to center of
C                                       earth)
      ARRAYC(1) = 0.0D0
      ARRAYC(2) = 0.0D0
      ARRAYC(3) = 0.0D0
C                                       Get GST0 and Earth rotation rate
      CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
      CALL JULDAY (RDATE, JD)
      CALL GSTROT (JD, GSTIA0, GASTM, DEGPDY)
C                                       Get frequency
      IOFF = 0
      CALL AXEFND (4, CHFREQ, KICTPN, CATH(KHCTP), IOFF, IERR)
      SAFREQ = CATD(KDCRV+IOFF)
C                                       init basic AN record
      ANNAME = ' '
      STAXOF = 0.0
      STAXYZ(1) = 0.0D0
      STAXYZ(2) = 0.0D0
      STAXYZ(3) = 0.0D0
      ORBPRM(1) = 0.0D0
      NOSTA = 0
      MNTSTA = 0
      POLAA = 0.0
      POLAB = 0.0
      CALL RFILL (3, 0.0, POLCA)
      CALL RFILL (3, 0.0, POLCB)
      POLTYA = 'R'
      POLTYB = 'L'
      ANFQID = -1
C                                       Create/init file
      EXTVER = 0
      CALL ANTINI ('WRIT', ANBUF, IVOL, ICNO, EXTVER, CATBLK, ANLUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *   RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME,
     *   NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) GO TO 999
      NCTR = 2881
C                                       Determine order of standard
C                                       values on card.
      DIAMAN = 0.0
      CALL RFILL (ANTNIF, 0.0, FWHMAN)
      DO 100 I = 1,ITNCOL
         IF (TTYPE(I).EQ.ANTNO) TORDER(1) = I
         IF (TTYPE(I).EQ.STATON) TORDER(2) = I
         IF (TTYPE(I).EQ.LX) TORDER(3) = I
         IF (TTYPE(I).EQ.LY) TORDER(4) = I
         IF (TTYPE(I).EQ.LZ) TORDER(5) = I
 100     CONTINUE
C                                       Loop for all lines in table.
      DO 800 LCTR = 1, NAXIS2
C                                       Read a FITS table data line.
         CALL TABLIN (NAXIS1, FDVEC, TBIND, NCTR, TAPBUF, TLINE, IERR)
         LINE(1:NAXIS1) = TLINE(1:NAXIS1)
         IF (IERR.NE.0) GO TO 999
         DO 200 I = 1,5
C                                       Decode the 5 data fields.
            INDEXX = TORDER(I)
            NPNTR = TBCOL(INDEXX)
            IWIDTH = TWIDTH(INDEXX)
            GO TO (150, 120, 150, 150, 150) , I
 120        CONTINUE
C                                       Station name.
               ANNAME = TLINE(NPNTR:NPNTR+IWIDTH-1)
               JT = JTRIM (ANNAME)
               GO TO 200
C                                       Station No. or Position.
 150        CONTINUE
               IFRAC = TFRAC(INDEXX)
               X = 0.0D0
               EQUAL = TLINE(NPNTR:NPNTR+NAXIS1-1) .EQ.
     *            TNULL(INDEXX)(1:NAXIS1)
               IF (.NOT.EQUAL)
     *            CALL DCODEF (NPNTR, IWIDTH, IFRAC, LINE, X, IERR)
               IF (I.EQ.1) NOSTA = X + .01
               IF (I.EQ.3) STAXYZ(1) =  X
               IF (I.EQ.4) STAXYZ(2) =  X
               IF (I.EQ.5) STAXYZ(3) =  X
 200        CONTINUE
C                                       Write the AIPS AN record.
         IANRNO = NOSTA
         CALL TABAN ('WRIT', ANBUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
C
 800     CONTINUE
      CALL TABIO ('CLOS', 1, IANRNO, ANBUF, ANBUF, IERR)
C
 999  RETURN
      END
      SUBROUTINE ANTREC (KL, NPNT, IALUN, ISLOT, NOANT, IABLK, IERR)
C-----------------------------------------------------------------------
C   ANTREC will parse the part of a antenna record history card
C   after 'HISTORY AIPS_or_CVAX ANT' and put the values it finds in an
C   antenna record and write the antenna record to the antenna
C   extension file.  If no antenna file exists (NOANT equals zero)
C   then one will be created.
C   Inputs:
C      KL     C*80     history card.
C      NPNT   I        index into KL after HISTORY AIPS ANT has been
C                      parsed.
C      IALUN  I        logical unit number for antenna file.
C      ISLOT  I        Catalog slot number for UV catalog file.
C   In/out:
C      NOANT  I        Current number of antenna records.
C      IABLK  I(512)   Antenna file I/O buffer.
C   Output:
C      IERR   I        error code. 0=ok. A message will be printed if
C                      an error occurs.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  CHFREQ*8, ANTTAB(5)*8, IDUMMY*2, KL*80, SYMBOL*8,
     *   STR*80
      DOUBLE PRECISION X, C, JD, GASTM
      INTEGER   IABLK(512), NPNT, IERR, NOANT, INTDUM,
     *   IALUN, ITABNO, NCHAR, I, ISLOT, VER, JLOCF, KERR
      LOGICAL   ISHIST, END
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA CHFREQ /'FREQ    '/
      DATA C /.2997924562D0/
      DATA ANTTAB /'N       ','X       ','Y       ',
     *             'Z       ','ST      '/
C-----------------------------------------------------------------------
C                                       Create antenna file.
      IF (NOANT.GT.0) GO TO 100
C                                       Setup for AN table initization
         NUMORB = 0
         ANFQID = -1
C                                       Position of the earth's pole
         POLRXY(1) = 0.0
         POLRXY(2) = 0.0
         UT1UTC = 0.0
         DATUTC = 0.0
C                                       Array name
         CALL H2CHR (8, 1, CATH(KHTEL), ANAME)
C                                       Array center (rel to center of
C                                       earth)
         ARRAYC(1) = 0.0D0
         ARRAYC(2) = 0.0D0
         ARRAYC(3) = 0.0D0
C                                       Get GST0 and Earth rotation rate
         CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
         CALL JULDAY (RDATE, JD)
         CALL GSTROT (JD, GSTIA0, GASTM, DEGPDY)
         CALL AXEFND (4, CHFREQ, KICTPN, CATH(KHCTP), JLOCF, KERR)
         SAFREQ = CATD(KDCRV+JLOCF)
         ANTNIF = EIF - BIF + 1
         NOPCAL = 2
         NUMORB = 0
         VER = 1
C                                       Create/init file
         CALL ANTINI ('WRIT', IABLK, DISOUT, ISLOT, VER, CATBLK, IALUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       init basic AN record
         ANNAME = ' '
         STAXOF = 0.0
         STAXYZ(1) = 0.0D0
         STAXYZ(2) = 0.0D0
         STAXYZ(3) = 0.0D0
         ORBPRM(1) = 0.0D0
         NOSTA = 0
         MNTSTA = 0
         POLAA = 0.0
         POLAB = 0.0
         CALL RFILL (3, 0.0, POLCA)
         CALL RFILL (3, 0.0, POLCB)
         POLTYA = 'R'
         POLTYB = 'L'
C                                       Write NULL antenna record
         DO 10 I = 1,30
            IANRNO = NOSTA
            CALL TABAN ('WRIT', IABLK, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) GO TO 999
 10         CONTINUE
C                                       Parse rest of antenna card.
 100  DO 200 I = 1,5
         CALL GETCRD (INTDUM, 5, 1, ANTTAB, IDUMMY, NPNT, KL, SYMBOL,
     *      ITABNO, ISHIST, END, IERR)
         IF (IERR.EQ.1) GO TO 200
         IF (IERR.NE.0) GO TO 980
         IF (ITABNO.NE.5) THEN
            CALL GETNUM (KL, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 975
            END IF
         GO TO (110, 120, 130, 140, 150), ITABNO
C                                       Station number.
 110        CONTINUE
               NOSTA = X + .5
               GO TO 200
C                                       X
 120        CONTINUE
               STAXYZ(1) = X * C
               GO TO 200
C                                       Y
 130        CONTINUE
               STAXYZ(2) = X * C
               GO TO 200
C                                       Z
 140        CONTINUE
               STAXYZ(3) = X * C
               GO TO 200
C                                       Station name.
 150           CALL GETSTR (KL, 80, 4, NPNT, STR, NCHAR)
               NCHAR = MIN (4, NCHAR)
               ANNAME(1:NCHAR) = STR(1:NCHAR)
 200     CONTINUE
C                                       Write antenna record.
         IANRNO = NOSTA
         CALL TABAN ('WRIT', IABLK, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Increment no. of ant records.
      NOANT = NOANT + 1
      GO TO 999
C                                       Incorrect ant rec format.
 975  MSGTXT = 'ANTREC: BAD VALUE ON ANT REC CARD'
      IERR = 1
      GO TO 990
 980  MSGTXT = 'INVALID ANTENNA RECORD CARD FORMAT'
 990  CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE DELRPM (IOFF)
C-----------------------------------------------------------------------
C   Delete the random parameter at offset IOFF from the random parameter
C   list (0 <= IOFF < CATBLK(KIPCN)). If the indicated parameter is at
C   the end of the random parameter list then the number of random
C   parameters is decreased by one, otherwise the name of the random
C   parameter is set to 'REMOVED '.
C
C   Inputs:
C     IOFF       I       Offset of random parameter to delete
C
C   Input/Output in common:
C     CATBLK     I(*)    UV file header
C     CATH       H(*)    UV file header
C     KIPCN      I       CATBLK(KIPCN) is the number of random
C                        parameters
C     KHPTP      I       The list of random parameter names starts at
C                        CATH(KHPTP)
C-----------------------------------------------------------------------
      INTEGER   IOFF

      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IF (IOFF.EQ.CATBLK(KIPCN)-1) THEN
         CATBLK(KIPCN) = CATBLK(KIPCN) - 1
      ELSE
         CALL CHR2H (8, 'REMOVED ', 1, CATH(KHPTP+2*IOFF))
         END IF
C
 999  RETURN
      END
      SUBROUTINE APPRPM (NAME)
C-----------------------------------------------------------------------
C   Append a random parameter with name NAME to the end of the random
C   parameter list.
C
C   Input:
C     NAME       C*8       Name of new random parameter
C
C   Input/output in common:
C     CATBLK     I*(*)     UV data header
C     CATH       H*(*)     UV data header
C     KIPCN      I         CATBLK(KIPCN) is the number of random
C                          parameters
C     KHPTP      I         The list of random parameter names starts at
C                          CATH(KHPTP)
C-----------------------------------------------------------------------
      CHARACTER NAME*8

      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      CALL CHR2H (8, NAME, 1, CATH(KHPTP + 2 * CATBLK(KIPCN)))
      CATBLK(KIPCN) = CATBLK(KIPCN) + 1
C
 999  RETURN
      END
      SUBROUTINE SUPREC (DISK, CNO, CATBLK, IERR)
C-----------------------------------------------------------------------
C   SUPREC re-writes the SU table, precessing the coordinates of epoch
C   to approximate apparent coordinates when J2000 is being used.
C   Input:
C      DISK     I      Disk number
C      CNO      I      Catalog number
C      CATBLK   I(*)   Catalog header
C   Output:
C      IERR     I       Error code
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(256), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   BUFFER(512), LUN, NUMIF, FREQID, ISURNO, SUKOLS(MAXSUC),
     *   SUNUMV(MAXSUC), IDSOU, QUAL, VER, NREC, IREC
      REAL      FLUX(4,MAXIF), POLAR(2)
      DOUBLE PRECISION FREQO(MAXIF), BANDW, RAEPO, DECEPO, EQUINX, JD0,
     *   RAAPP, DECAPP, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC,
     *   OBSPOS(3), RAOBS, DECOBS
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4, RDATE*8
      LOGICAL   ISOPEN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA OBSPOS, POLAR /3*0.0D0, 2*0.0/
C-----------------------------------------------------------------------
      IERR = 0
      CALL FNDEXT ('SU', CATBLK, VER)
      IF (VER.LE.0) GO TO 999
      LUN = 57
      ISOPEN = .FALSE.
      CALL SOUINI ('WRIT', BUFFER, DISK, CNO, VER, CATBLK, LUN, NUMIF,
     *   VELTYP, VELDEF, FREQID, ISURNO, SUKOLS, SUNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT'
         GO TO 990
         END IF
      ISOPEN = .TRUE.
      NREC = BUFFER(5)
      CALL H2CHR (8, 1, CATBLK(KHDOB), RDATE)
      CALL JULDAY (RDATE, JD0)
      DO 100 IREC = 1,NREC
         ISURNO = IREC
         CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EQUINX, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ'
            GO TO 990
            END IF
C                                       compute apparents pos
         CALL JPRECS (JD0, EQUINX, 1.0D-6, 1, .TRUE., OBSPOS,
     *      POLAR, DG2RAD * RAEPO, DG2RAD * DECEPO, RAAPP, DECAPP)
         RAAPP = RAD2DG * RAAPP
         DECAPP = RAD2DG * DECAPP
         IF (RAAPP.LT.0.0D0) RAAPP = RAAPP + 360.0D0
C                                       put back in file
         ISURNO = IREC
         CALL TABSOU ('WRIT', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EQUINX, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRIT'
            GO TO 990
            END IF
 100     CONTINUE
      GO TO 995
C
 990  CALL MSGWRT (7)
 995  IF (ISOPEN) CALL TABSOU ('CLOS', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *   IDSOU,SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *   DECEPO, EQUINX, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *   PMRA, PMDEC, IREC)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUPREC ERROR',I4,' ON ',A,' FIXING APPARENT COORDINATES')
      END
      SUBROUTINE SUCHCK (DISK, CNO)
C-----------------------------------------------------------------------
C   SUCHCK re-writes the SU table changing duplicate source names
C   Input:
C      DISK   I   Disk number
C      CNO    I   Catalog number
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO
C
      INCLUDE 'PFITLD.INC'
C
      INTEGER   BUFFER(512), LUN, NUMIF, FREQID, ISURNO, SUKOLS(MAXSUC),
     *   SUNUMV(MAXSUC), IDSOU, QUAL, VER, NREC, IREC, IERR, JTRIM,
     *   SNUM(MAXSOU), LUNTMP, JREC, J, CHANGE, SQUAL(MAXSOU)
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION FREQO(MAXIF), BANDW, RAEPO, DECEPO, EQUINX,
     *   RAAPP, DECAPP, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC,
     *   RAOBS, DECOBS
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4,
     *   SNAM(MAXSOU)*16, SCALC(MAXSOU)*4
      LOGICAL   ISOPEN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IERR = 0
      CALL FNDEXT ('SU', CATBLK, VER)
      IF (VER.LE.0) GO TO 999
      LUN = LUNTMP (1)
      ISOPEN = .FALSE.
      CALL SOUINI ('WRIT', BUFFER, DISK, CNO, VER, CATBLK, LUN, NUMIF,
     *   VELTYP, VELDEF, FREQID, ISURNO, SUKOLS, SUNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT'
         GO TO 990
         END IF
      ISOPEN = .TRUE.
      NREC = BUFFER(5)
C                                       read in names
      DO 20 IREC = 1,NREC
         ISURNO = IREC
         CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EQUINX, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ'
            GO TO 990
            END IF
         J = JTRIM (SOUNAM)
         SNAM(IREC) = SOUNAM
         SNUM(IREC) = IDSOU
         SQUAL(IREC) = QUAL
         SCALC(IREC) = CALCOD
 20      CONTINUE
C                                       chck for duplicates
      CHANGE = 0
 30   DO 50 IREC = 1,NREC-1
         DO 40 JREC = IREC+1,NREC
            IF ((SNAM(IREC).EQ.SNAM(JREC)) .AND.
     *         (SNUM(IREC).NE.SNUM(JREC)) .AND.
     *         (SCALC(IREC).EQ.SCALC(JREC)) .AND.
     *         (SQUAL(IREC).EQ.SQUAL(JREC))) THEN
               ISURNO = JREC
               CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *            IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW,
     *            RAEPO, DECEPO, EQUINX, RAAPP, DECAPP, RAOBS, DECOBS,
     *            LSRVEL, LRESTF, PMRA, PMDEC, IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'RE-READ'
                  GO TO 990
                  END IF
               QUAL = QUAL + 1
               SQUAL(JREC) = QUAL
               ISURNO = JREC
               CALL TABSOU ('WRIT', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *            IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW,
     *            RAEPO, DECEPO, EQUINX, RAAPP, DECAPP, RAOBS, DECOBS,
     *            LSRVEL, LRESTF, PMRA, PMDEC, IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'RE-READ'
                  GO TO 990
                  END IF
               CHANGE = CHANGE + 1
               WRITE (MSGTXT,1030) IDSOU, SOUNAM, QUAL
               CALL MSGWRT (3)
               GO TO 30
               END IF
 40         CONTINUE
 50      CONTINUE
      IF (CHANGE.GT.0) THEN
         WRITE (MSGTXT,1050) CHANGE
         CALL MSGWRT (2)
         END IF
      GO TO 995
C
 990  CALL MSGWRT (7)
 995  IF (ISOPEN) CALL TABSOU ('CLOS', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *   IDSOU,SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *   DECEPO, EQUINX, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *   PMRA, PMDEC, IREC)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUCHCK ERROR',I4,' ON ',A,' CHECKING SOURCE NAMES')
 1030 FORMAT ('SUCHCK: source number',I5,' named ',A,' new qual',I5)
 1050 FORMAT ('SUCHCK: changed',I4,' total duplicate source names')
      END
      SUBROUTINE UVZERO (CATBLK, VIS, NZERO, GOOD)
C-----------------------------------------------------------------------
C   Checks for all freq channels pure zero in real and imag and flags
C   them
C   Inputs:
C      CATBLK   I(*)     Header
C   In/out
C      VIS      R(3,*)   data
C      NZERO    I        Number spectra all zero counter (3) total,
C                        (2) number previously flagged
C      GOOD     L        Some useable data?
C-----------------------------------------------------------------------
      INTEGER   CATBLK(256), NZERO(3)
      REAL      VIS(3,*)
      LOGICAL   GOOD
C
      INTEGER   NS, NIF, NF, NC, JNCIF, JNCF, JNCS, I, J, JIF, JS, JF
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       pointers to traverse the data
      NS = 1
      NIF = 1
      NF = 1
      NC = CATBLK(KINAX)
      JNCIF = INCIF / NC
      JNCF  = INCF / NC
      JNCS  = INCS / NC
      IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
      IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
      IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
      DO 40 JIF = 1,NIF
         DO 30 JS = 1,NS
            J = (JIF-1) * JNCIF + (JS-1) * JNCS + 1
            I = J - JNCF
            NZERO(3) = NZERO(3) + 1
C                                       all already flagged?
            DO 10 JF = 1,NF
               I = I + JNCF
               IF (VIS(3,I).GT.0.0) GO TO 15
 10            CONTINUE
            NZERO(2) = NZERO(2) + 1
            GO TO 30
C                                      all valid are zero?
 15         I = J - JNCF
            DO 20 JF = 1,NF
               I = I + JNCF
               IF (((VIS(1,I).NE.0.0) .OR. (VIS(2,I).NE.0.0)) .AND.
     *            (VIS(3,I).GT.0.0)) GO TO 30
 20            CONTINUE
C                                       all zero: flag
            NZERO(1) = NZERO(1) + 1
            I = J - JNCF
            DO 25 JF = 1,NF
               I = I + JNCF
               VIS(3,I) = MIN (0.0, -ABS(VIS(3,I)))
 25            CONTINUE
 30         CONTINUE
 40      CONTINUE
C                                       look again, anything good
      GOOD = .TRUE.
      DO 70 JIF = 1,NIF
         DO 60 JS = 1,NS
            J = (JIF-1) * JNCIF + (JS-1) * JNCS + 1
            I = J - JNCF
C                                       all already flagged?
            DO 50 JF = 1,NF
               I = I + JNCF
               IF (VIS(3,I).GT.0.0) GO TO 999
 50            CONTINUE
 60         CONTINUE
 70      CONTINUE
      GOOD = .FALSE.
C
 999  RETURN
      END
      SUBROUTINE IMGTAP (IERR)
C-----------------------------------------------------------------------
C  Process FITS type tape header, data, and extension files.
C  Outputs:
C     IERR   I        Error code, 0=ok.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IERR
      INTEGER   HLUN, HBUFF(256), ISLOT, SEQ, KVOL, NUMTAB, IOP, ITSAVE
      LOGICAL   NODATA, EOF, MORTAB
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA HLUN /27/
C-----------------------------------------------------------------------
      KVOL = DISOUT
      SEQ = OUTSEQ
      IOP = 1
      KEEP = 1
C                                       Does header & history using a
C                                       temporary name in catalog.
 10   CALL IMGHDR (IOP, KVOL, HLUN, HBUFF, ISLOT, NODATA, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get the data and store in file
      IF (NODATA) THEN
         KEEP = 0
      ELSE
         CALL IMGDAT (KVOL, ISLOT, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Add inputs file to history.
      ITSAVE = CATBLK(KIIMS)
      CATBLK(KIIMS) = SEQ
      CALL HISINP (KVOL, ISLOT, HLUN, HBUFF)
      CATBLK(KIIMS) = ITSAVE
C                                       Skip any tables.
      IF (.NOT.DOTABL) THEN
         CALL MLREOF (FDVEC, TBIND, UNKNWN, TAPBUF, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Standard fits extension records.
      ELSE
         CALL IMGRXM (KVOL, HLUN, HBUFF, NUMTAB, EOF, IERR)
         IF (IERR.GT.0) GO TO 999
         MORTAB = IERR.LT.0
         IERR = 0
C                                       Old tables records.
         IF (.NOT.EOF) CALL IMGTAB (KVOL, HLUN, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       If no data use table name for
C                                       image.
         IF (NODATA) CALL IMGFIX (KVOL, NUMTAB, IERR)
         END IF
C                                       Fill in default names if needed.
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL IMGDEF (NAMOUT, CLAOUT, SEQ)
C                                       Renames to the proper name.
      CALL RENAM (ISLOT, KVOL, IERR)
C                                       Trap case where there were too
C                                       many tables of a given type and
C                                       a dummy file is needed for the
C                                       excess tables.
      IF (MORTAB) THEN
         EOF = .FALSE.
         NODATA = .TRUE.
         IOP = 2
         IF (SEQ.GT.0) SEQ = SEQ + 1
         WRITE (MSGTXT,1050)
         CALL MSGWRT (6)
         GO TO 10
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('Too many tables for a single file, must create another')
      END
      SUBROUTINE IMGHDR (IOP, KVOL, HLUN, HBUFF, ISLOT, NODATA, IERR)
C-----------------------------------------------------------------------
C   IMGHDR reads the tape which must be open and positioned at begin.
C   of file) and builds a catalog header and pointers from the
C   tape header records.  After the required fits cards are read a
C   map file with a temporary name is created and the history records
C   are recognized and written to the history file as the other header
C   cards are processed.  The file is later renamed to the correct name.
C   Inputs:
C     IOP    I     Operation code 1=> read tape, 2=>just create dummy
C                  file
C     KVOL   I     Disk volume for cataloged map.
C     HLUN   I     History file logical unit number.
C     HBUFF  I(256)   work buffer.
C   Output:
C     ISLOT  I     Catalog slot number for new map file.
C     NODATA L     True if tape contains no data section, else false.
C     ERR    I     =0 => ok
C                   other => quit
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER LINE*80, HILINE*72, CHTEMP*8
      REAL      PIX11(2)
      INTEGER   IOP, HBUFF(256), ICARD, IE, IST, IERR, ISLOT, IREC, I,
     *   IN, IS, IAX, HLUN, IDEPTH(5), KVOL
      LOGICAL   END, F, T, ISHIST, NODATA, DOHI
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize BLANK values flag
C                                       to false.
      NODATA = .FALSE.
      ISBLNK = .FALSE.
      FUCKUP = .FALSE.
      CALL CATCLR (CATBLK)
C                                       redo
      IF (IOP.EQ.2) THEN
         CATBLK(KIDIM) = 0
C                                       tape read requested
      ELSE
C                                       Go to beginning of file
         CALL TAPIO ('BAKF', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       allow blocked tapes
         FDVEC(6) = 10
         FDVEC(32) = 0
C                                       Initialize header values.
         CALL CATINI (CATBLK)
         SCALE = 1.0D0
         OFFSET = 0.0D0
         ISCALE = 1.0D0
         IZERO = 0.0D0
         CALL RFILL (49, 0.0, PCMATX)
         CALL RFILL (49, 0.0, PVMATX)
         CALL RFILL (49, 0.0, CDMATX)
C                                       Read record 1 from tape.
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
C                                       Decode required cards.
         CALL IMREQC (ICEND, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       dummy image
      IF (CATBLK(KIDIM).LE.0) THEN
C                                       Make a 2x2 map.
         NODATA = .TRUE.
         ISBLNK = .TRUE.
         CATBLK(KIDIM) = 2
         CATBLK(KINAX) = 2
         CATBLK(KINAX+1) = 2
         END IF
C                                       More defaults.
      DO 10 I = 1,KICTPN
         CATR(KRCRP+I-1) = 0.0
         CATR(KRCIC+I-1) = 1.0
 10      CONTINUE
C                                       Create map with temporary name.
C                                       Map will be renamed later.
      CALL CHR2H (12, 'FITLD       ', KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, 'TEMP  ', KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = 0
      CATBLK(KIIMU) = NLUSER
      CALL MCREAT (KVOL, CNO, HBUFF, IERR)
C                                       Blank name back out.
      CALL CHR2H (12, '            ', KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, '      ', KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = 0
      ISLOT = CNO
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000)
         GO TO 990
         END IF
      NCFILE = 1
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 2
      FVOL(NCFILE) = KVOL
C                                       Create HI file
      CALL HICREA (HLUN, KVOL, CNO, CATBLK, HBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020)
         GO TO 990
         END IF
C                                       Header msg in HI
      LINE = '--------------------------------------------------' //
     *   '------------------'
      CALL HIAD80 (HLUN, 1, LINE, HBUFF, IERR)
      WRITE (LINE,1026)
      IF (IERR.EQ.0) CALL HIAD80 (HLUN, 1, LINE, HBUFF, IERR)
C                                       See if need to parse rest of
C                                       header.
      IF (IOP.NE.2) THEN
C
         ICARD = ICEND + 1
C                                       Loop until END card found.
         DO 90 IREC = 1,100000000
C                                       Read next record.
            IF (ICARD.GT.36) THEN
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
               ICARD = 1
               END IF
C                                       Parse card, put value in hdr.
            CALL IMPARS (ICARD, ISHIST, END, IERR)
            IF (END) GO TO 100
C                                       Add to history file.
            IF (IERR.GT.0) THEN
               IST = 80 * ICARD - 79
               CHTEMP = FITBLK(IST:)
               DOHI = (CHTEMP.EQ.'HISTORY') .OR. (CHTEMP.EQ.'COMMENT')
     *            .OR. (CHTEMP.EQ.' ')
               LINE = FITBLK(IST:)
               IST = 1
               IF (DOHI) IST = IST + 8
               CALL HIAD80 (HLUN, IST, LINE, HBUFF, IERR)
            ELSE IF (IERR.EQ.-1) THEN
               IS = (ICARD - 1) * 80 + 1
               IE = IS + 79
               CALL PUTCRD (FITBLK(IS:IE), KVOL, CNO, IERR)
               IF (IERR.GT.1) THEN
                  WRITE (MSGTXT,1080) IERR
                  CALL MSGWRT (7)
                  GO TO 999
                  END IF
               END IF
            ICARD = ICARD + 1
 90         CONTINUE
C                                       Read more cards than we expected
         WRITE (MSGTXT,1090)
         GO TO 990
C                                       End card found.
C                                       Make axis increments non zero
C                                       to help out dumb programs.
 100     IN = KINAX
         IS = KRCIC
         IE = IS + CATBLK(KIDIM) - 1
         DO 200 IAX = IS,IE
            IF ((CATR(IAX).EQ.0.0).AND.(CATBLK(IN).EQ.1))
     *         CATR(IAX) = 1.0
            IN = IN + 1
 200        CONTINUE
         END IF
C                                       End FITS header section in HI
      WRITE (HILINE,1110)
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      LINE ='--------------------------------------------------'//
     *   '------------------'
      IF (IERR.EQ.0) CALL HIAD80 (HLUN, 1, LINE, HBUFF, IERR)
C                                       Close history
      CALL HICLOS (HLUN, T, HBUFF, IERR)
C                                       PC -> CROTA
C                                       CD -> CDELT, CROTA
      CALL PCHDR (PCMATX, CDMATX, PVMATX)
C                                       Correct for PDP 11 values.
C                                       set common values
      IF (.NOT.FUCKUP) GO TO 999
         DO 310 I = 3,7
            IDEPTH(I-2) = 1
            IF (I.LE.CATBLK(KIDIM)) THEN
               IDEPTH(I-2) = CATR(KRCRP+I-1) + 0.5
               IDEPTH(I-2) = MAX (1, MIN (IDEPTH(I-2),
     *            CATBLK(KINAX+I-1)))
               END IF
 310        CONTINUE
         LOCNUM = 1
         CALL SETLOC (IDEPTH, F)
         IF ((ABS(POS11(1)-CATD(KDCRV+KLOCL(LOCNUM))).
     *      LT.ABS(0.01*CATR(KRCIC+KLOCL(LOCNUM)))) .AND.
     *      (ABS(POS11(2)-CATD(KDCRV+KLOCM(LOCNUM)))
     *      .LT.ABS(0.01*CATR(KRCIC+KLOCM(LOCNUM))))) GO TO 999
C                                       do conversion
         CALL LMPIX (POS11(1), POS11(2), PIX11(1), PIX11(2))
         IF ((ABS(PIX11(1)-CATR(KRCRP+KLOCL(LOCNUM))).LT.
     *      CATBLK(KINAX+KLOCL(LOCNUM))/2)
     *      .AND. (ABS(PIX11(2)-CATR(KRCRP+KLOCM(LOCNUM))).LT.
     *      CATBLK(KINAX+KLOCM(LOCNUM))/2)) GO TO 320
            WRITE (MSGTXT,1310) POS11
            CALL MSGWRT (6)
            WRITE (MSGTXT,1311) PIX11
            CALL MSGWRT (6)
            GO TO 999
 320     WRITE (MSGTXT,1320) CATR(KRCRP+KLOCL(LOCNUM)),
     *      CATR(KRCRP+KLOCM(LOCNUM)), PIX11
         CALL MSGWRT (3)
         CATR(KRCRP+KLOCL(LOCNUM)) = PIX11(1)
         CATD(KDCRV+KLOCL(LOCNUM)) = POS11(1)
         CATR(KRCRP+KLOCM(LOCNUM)) = PIX11(2)
         CATD(KDCRV+KLOCM(LOCNUM)) = POS11(2)
         GO TO 999
C
 990  CALL MSGWRT (7)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAP CREATE ERROR')
 1020 FORMAT ('HISTORY CREATE ERROR')
 1026 FORMAT ('/Begin "HISTORY" information found in fits tape ',
     *   'header by FITLD')
 1080 FORMAT ('ERROR',I5,' ADDING KEYWORD TO HEADER FILE')
 1090 FORMAT ('READ MORE THAN 10**8 CARDS WITHOUT FINDING AN END CARD')
 1110 FORMAT ('/END FITS tape header "HISTORY" information')
 1310 FORMAT ('PDP11/70 ERROR: PHASE REF. POS.',2E14.6)
 1311 FORMAT ('GIVES REF. PIXEL',2F9.2,' IGNORED')
 1320 FORMAT ('CORRECTING REF. PIXEL FROM',2F8.2,' TO',2F8.2)
      END
      SUBROUTINE IMGDAT (KVOL, KNO, IER)
C-----------------------------------------------------------------------
C   IMGDAT reads the input data file and scales the data to disk.
C   Inputs:
C      KVOL  I     desired map disk
C      KNO   I     Catalog number
C   Outputs:
C      IER   I     Error return:  0--> okay
C                                 1--> error condition
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   KVOL, KNO, IER
C
      INCLUDE 'FITLD.INC'
      CHARACTER MNAME*48
      INTEGER   BLKS, IERR, IWIN(4), NBKOF1, IOFF, NX, NY,
     *   INBUFF(UVBFSL), IDEPTH(5), NBYB, I, INX, INY, IBL, ITEMP,
     *   NXY, I3, I3B, I4, I4B, I5, I5B, I6, I6B, I7, I7B, DLUN, DIND,
     *   NTAPVL, III, L0, L1, L2, NXX, OUTIND
      REAL      MMAX, MMIN
      DOUBLE PRECISION    DPBUFR(4096), DTEMP
      LOGICAL   T, WASBLK
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'SCRBFS.INC'
      EQUIVALENCE (TUVBUF, INBUFF, DPBUFR)
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      DLUN = 16
      MMAX = -1.E20
      MMIN =  -MMAX
      WASBLK = .FALSE.
C                                       Open map file.
      CALL ZPHFIL ('MA', KVOL, KNO, 1, MNAME, IERR)
      CALL ZOPEN (DLUN, DIND, KVOL, MNAME, T, T, T, IERR)
      IF (IERR.EQ.0) GO TO 5
         WRITE (MSGTXT,1000) MNAME, IERR
         GO TO 980
C                                       Initialize
 5    IER = 0
      BSC = SCALE
      IF (BSC.EQ.0.0D0) BSC = 1.0D0
      BZE = OFFSET
      NBYB = UVBFSL * 2
C                                       Set window parms
      I3B = MAX (1, CATBLK(KINAX+2))
      I4B = MAX (1, CATBLK(KINAX+3))
      I5B = MAX (1, CATBLK(KINAX+4))
      I6B = MAX (1, CATBLK(KINAX+5))
      I7B = MAX (1, CATBLK(KINAX+6))
      CATBLK(KINAX+1) = MAX (1, CATBLK(KINAX+1))
      IWIN(1) = 1
      IWIN(2) = 1
      IWIN(3) = CATBLK(KINAX)
      IWIN(4) = CATBLK(KINAX+1)
      NY = IWIN(4)
      NX = IWIN(3)
      INX = CATBLK(KINAX)
      INY = CATBLK(KINAX+1)
C                                       Initialize tape
      NBPIX = TAPEBP
      BLKS = (ABS(NBPIX) / 8)
      NTAPVL = 2880 / BLKS
      IOFF = NTAPVL
      BLKS = BLKS * NX * NY * I3B
      BLKS = BLKS * I4B * I5B * I6B * I7B
      BLKS = (BLKS - 1) / 2880 + 1
      BLKS = BLKS - 1
      IF (IERR.NE.0) GO TO 970
C                                       Test for Kitt Peak "error"
      IF ((IBLNK.EQ.0) .AND. (NBPIX.EQ.8)) ISBLNK = .FALSE.
      DO 200 I7 = 1,I7B
      DO 200 I6 = 1,I6B
      DO 200 I5 = 1,I5B
      DO 200 I4 = 1,I4B
      DO 200 I3 = 1,I3B
C                                       Initialize disk
         IDEPTH(1) = I3
         IDEPTH(2) = I4
         IDEPTH(3) = I5
         IDEPTH(4) = I6
         IDEPTH(5) = I7
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, NBKOF1,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         NBKOF1 = NBKOF1 + 1
         CALL MINIT ('WRIT', DLUN, DIND, INX, INY, IWIN, UVBUFF, NBYB,
     *      NBKOF1, IERR)
         IF (IERR.EQ.0) GO TO 30
            WRITE (MSGTXT,1020) IERR
            GO TO 980
C                                       Begin read/write loop
 30      DO 195 I = 1,NY
C                                       Write a map line
            CALL MDISK ('WRIT', DLUN, DIND, UVBUFF, OUTIND, IERR)
            IF (IERR.EQ.0) GO TO 40
               WRITE (MSGTXT,1030) IERR, I
               GO TO 980
 40         NXY = NX
            IBL = 0
C                                       Copy and read until entire map
C                                       row filled.
 55         NXX = MIN (NXY, NTAPVL-IOFF)
C                                       Need more tape values.
               IF (NXX.GT.0) GO TO 60
                  BLKS = BLKS - 1
                  CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
                  IOFF = 0
                  IF (NBPIX.EQ.8) CALL ZI8IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.16) CALL ZI16IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.32) CALL ZI32IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.-32) CALL ZR32RL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.-64) CALL ZR64RL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (IERR.EQ.0) GO TO 55
                     GO TO 970
C                                       INT in: copy convert max/min
 60            IF ((NBPIX.EQ.8) .OR. (NBPIX.EQ.16) .OR. (NBPIX.EQ.32))
     *            THEN
                  L0 = IOFF
                  L2 = OUTIND + IBL - 1
                  IF (ISBLNK) THEN
                     DO 100 III = 1,NXX
                        L1 = L2 + III
                        ITEMP = INBUFF(L0+III)
C                                       Blank pixel found
                        IF (ITEMP.EQ.IBLNK) THEN
                           UVBUFF(L1) = FBLANK
                           WASBLK = .TRUE.
C                                       scale
                        ELSE
                           UVBUFF(L1) = BSC * ITEMP + BZE
                           MMIN = MIN (MMIN, UVBUFF(L1))
                           MMAX = MAX (MMAX, UVBUFF(L1))
                           END IF
 100                    CONTINUE
                  ELSE
                     DO 115 III = 1,NXX
                        L1 = L2 + III
                        UVBUFF(L1) = BSC * INBUFF(L0+III) + BZE
                        MMIN = MIN (MMIN, UVBUFF(L1))
                        MMAX = MAX (MMAX, UVBUFF(L1))
 115                    CONTINUE
                     END IF
C                                       IEEE 64-bit in
               ELSE IF (NBPIX.EQ.-64) THEN
                  L0 = IOFF
                  L2 = OUTIND + IBL - 1
                  DO 150 III = 1,NXX
                     L1 = L2 + III
                     DTEMP = DPBUFR(L0+III)
                     IF (DTEMP.EQ.DBLANK) THEN
                        WASBLK = .TRUE.
                        UVBUFF(L1) = FBLANK
                     ELSE
                        UVBUFF(L1) = BSC * DTEMP + BZE
                        MMIN = MIN (MMIN, UVBUFF(L1))
                        MMAX = MAX (MMAX, UVBUFF(L1))
                        END IF
 150                 CONTINUE
C                                       IEEE 32-bit in
               ELSE
                  L0 = IOFF
                  L2 = OUTIND + IBL - 1
                  DO 160 III = 1,NXX
                     L1 = L2 + III
                     UVBUFF(L1) = TUVBUF(L0+III)
                     IF (UVBUFF(L1).EQ.FBLANK) THEN
                        WASBLK = .TRUE.
                     ELSE
                        UVBUFF(L1) = BSC * UVBUFF(L1) + BZE
                        MMIN = MIN (MMIN, UVBUFF(L1))
                        MMAX = MAX (MMAX, UVBUFF(L1))
                        END IF
 160                 CONTINUE
                  END IF
C                                       Up the counters
               IBL = IBL + NXX
               IOFF = IOFF + NXX
               NXY = NXY - NXX
C                                       loop back if needed to finish
               IF (NXY.GT.0) GO TO 55
 195         CONTINUE
C                                       Flush this plane.
         CALL MDISK ('FINI', DLUN, DIND, UVBUFF, OUTIND, IERR)
         IF (IERR.NE.0) GO TO 970
 200     CONTINUE
C                                       close files
      CATR(KRDMX) = MMAX
      CATR(KRDMN) = MMIN
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
      CALL MAPCLS ('WRIT', KVOL, KNO, DLUN, DIND, CATBLK, T, SCRBUF,
     *   IERR)
      FRW(1) = 1
      NCFILE = NCFILE - 1
      GO TO 999
C                                       Error
 970  WRITE (MSGTXT,1970) IERR
 980  CALL MSGWRT (8)
      IF (IERR.EQ.4) THEN
         WRITE (MSGTXT,1980)
         CALL MSGWRT(8)
         END IF
 990  IER = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMGDAT: COULD NOT OPEN MAP ',6A4,' IER=',I4)
 1020 FORMAT ('IMGDAT: COULD NOT INITIALIZE DISK FILE.  IER=',I4)
 1030 FORMAT ('IMGDAT: COULD NOT WRITE DISK RECORD.  IER=',I3,
     *   ' LINE=',I4)
 1970 FORMAT ('IMGDAT: COULD NOT READ INPUT.  IER=',I4)
 1980 FORMAT ('IMGDAT: - MAYBE PREMATURE END OF FILE?  CHECK FILE SIZE')
      END
      SUBROUTINE HISINP (KVOL, KCNO, HLUN, HBUFF)
C-----------------------------------------------------------------------
C   Add inputs to the history file.
C   Inputs:
C     KVOL   I         Disk volume number of history file and map.
C     KCNO   I         Catalog number
C     HLUN   I         History file LUN.
C     HBUFF  I(256)    History I/O work buffer.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER HILINE*72
      INTEGER   KVOL, KCNO, HLUN, HBUFF(256), IERR, I, ITRIM
      LOGICAL   T
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      CALL HIOPEN (HLUN, KVOL, KCNO, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      WRITE (HILINE,1000) NAMOUT, CLAOUT
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      WRITE (HILINE,1001) CATBLK(KIIMS), NTAPE, KVOL
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (DODISK) THEN
         I = ITRIM (INFILE)
         WRITE (HILINE,1002) INFILE(:I)
         CALL HIADD (HLUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      WRITE (HILINE,1003) RLSNAM
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Close history
      CALL HICLOS (HLUN, T, HBUFF, IERR)
      IF (IERR.EQ.0) GO TO 999
C                                       Error.
 980  WRITE (MSGTXT,1980)
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FITLD OUTNAME =''',A12,'''',6X,'OUTCLASS =''',A6,
     *   '''')
 1001 FORMAT ('FITLD OUTSEQ =',I5,5X,'INTAPE =',I2,5X,'OUTDISK=',I2)
 1002 FORMAT ('FITLD INFILE = ''',A,'''')
 1003 FORMAT ('FITLD RELEASE = ''',A7,'''')
 1980 FORMAT ('ERROR IN ADDING TO HI FILE.  WARNING ONLY')
      END
      SUBROUTINE IMGRXM (IVOL, HLUN, HBUFF, NUMTAB, EOF, IERR)
C-----------------------------------------------------------------------
C  This routine will read all fits extension files associated with a map
C  and process the ones it recognizes (XTENSION = 'TABLES' as of now).
C  Inputs:
C     IVOL    I       Disk volume number of map and ext files.
C     HLUN    I       History file LUN to be opened
C     HBUFF   I(256)  History file I/O buffer.
C  Outputs:
C     NUMTAB  I       Number of extension files found.
C     EOF     L       An end of file was read during processing.
C     IERR    I       Error code. 0=ok. >0 => Error
C                     -1 => too many tables for one output file.
C-----------------------------------------------------------------------
      INTEGER   IVOL, HLUN, HBUFF(*), NUMTAB, IERR
      LOGICAL   EOF
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MXTBKW, MAXTAB
C                                       MXTBKW=max. no. table keywords
      PARAMETER (MXTBKW = 1000)
C                                       MAXTAB = max number of tables.
      PARAMETER (MAXTAB=46000)
      CHARACTER KEYWRD(MXTBKW)*8, KEYCHR(MXTBKW)*8, TABLE*8, TAB3D(3)*8
      LOGICAL   T
      DOUBLE PRECISION NBITS, AXCNT, KEYVAL(MXTBKW), KEYD
      REAL      KEYR(2)
      HOLLERITH KEYH(2)
      INTEGER   I, II, ICARD, INBLK, KEYTYP(MXTBKW), IVER, TABLUN,
     *   SRTORD, DATP(128,2), BUFFER(512), NUMKEY, KEYI, JERR, IKEY,
     *   KEYLOC(MXTBKW), KEYV(2*MXTBKW), LENKEY(5), JT, JTRIM,
     *   SAVCAT(256)
      LOGICAL   EXTEN, KEYL, DOHDR
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (KEYL, KEYI, KEYH, KEYR, KEYD)
      DATA TABLE, TAB3D /'TABLE   ', 'BINTABLE', 'A3DTABLE', '3D TABLE'/
      DATA TABLUN /29/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Length of keyword values
      LENKEY(1) = NWDPDP
      LENKEY(2) = 1
      LENKEY(3) = 2
      LENKEY(4) = 1
      LENKEY(5) = 1
C                                       Open history
      CALL HIOPEN (HLUN, IVOL, CNO, HBUFF, JERR)
      NUMTAB = 0
C                                       Loop for all FITS extensions.
      DO 200 I = 1,1000000
C                                       Process all required FITS table
C                                       cards.
         CALL EXTREQ (FDVEC, TBIND, TAPBUF, FITBLK, ICARD, EXTEN, EOF,
     *      UNKNWN, IERR)
         IF (EOF) GO TO 900
         IF ((IERR.NE.0) .OR. (.NOT.EXTEN)) GO TO 900
C                                       Calculate no. of data blocks.
         AXCNT = 1.0D0
         DO 20 II = 1,NAXIS
            AXCNT = AXCNT * NAXISI(II)
 20         CONTINUE
         AXCNT = AXCNT + PCOUNT
         NBITS = ABS (BITPIX) * GCOUNT * AXCNT
         INBLK = INT ((NBITS + 23039.0D0) / 23040.0D0)
         DOHDR = .FALSE.
C                                       See if we have an ASCII table.
         IF (EXTTYP.EQ.TABLE) THEN
            IF (NAXISI(1).GT.2880) THEN
               WRITE (MSGTXT,1020) NAXISI(1)
               CALL MSGWRT (6)
               GO TO 100
               END IF
C                                       initialize default values.
            CALL SETDEF
C                                       Process table file header.
            NUMKEY = MXTBKW
            CALL TABHDR (FDVEC, TBIND, ICARD, HLUN, HBUFF, 0, NUMKEY,
     *         KEYWRD, KEYVAL, KEYCHR, KEYTYP, TAPBUF, FITBLK, IERR)
            IF (IERR.LT.0) GO TO 100
            IF (IERR.NE.0) GO TO 900
C                                       Normal table files. Special
C                                       processing if its an AIPS style
C                                       table.
            SRTORD = 0
            IF (ITYPE.NE.'UK') CALL ATCONV (NUMKEY, KEYWRD, KEYVAL,
     *         KEYTYP, KEYCHR, SRTORD)
C                                       Create and initialize the table
C                                       header with data in common.
            IVER = 0
            CALL MAKTAB (SRTORD, IVOL, CNO, IVER, CATBLK, TABLUN, DATP,
     *         BUFFER, JERR)
            IF (JERR.GT.0) GO TO 110
C                                       Trap filling up table type.
            EOF = IVER.GT.MAXTAB
C                                       Prepare keywords
            IKEY = 1
            DO 30 II = 1,NUMKEY
               KEYLOC(II) = IKEY
               IF (KEYTYP(II).EQ.2) KEYTYP(II) = 1
               IF (KEYTYP(II).EQ.1) KEYD = KEYVAL(II)
               IF (KEYTYP(II).EQ.3) THEN
                  JT = JTRIM (KEYCHR(II))
                  CALL CHR2H (8, KEYCHR(II), 1, KEYH)
                  END IF
               IF (KEYTYP(II).EQ.4) KEYI = KEYVAL(II)
               IF (KEYTYP(II).EQ.5) KEYL = KEYVAL(II).GT.0.0D0
               CALL COPY (LENKEY(KEYTYP(II)), KEYD, KEYV(IKEY))
               IKEY = IKEY + LENKEY(KEYTYP(II))
 30            CONTINUE
C                                       Write keywords.
            JERR = 0
            CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER, KEYLOC,
     *         KEYV, KEYTYP, JERR)
            IF (JERR.NE.0) THEN
               CALL TABIO ('CLOS', 0, II, BUFFER, BUFFER, JERR)
               GO TO 110
               END IF
C                                       Read the data from tape and
C                                       write to the table disk file.
            CALL RWTAB (FDVEC, TBIND, DATP, NAXISI, BUFFER, TAPBUF,
     *         IERR)
            IF (IERR.NE.0) GO TO 900
            KEEP = MAX (1, KEEP)
C                                       See if we have a 3-D table.
         ELSE IF ((EXTTYP.EQ.TAB3D(1)) .OR. (EXTTYP.EQ.TAB3D(2)) .OR.
     *      (EXTTYP.EQ.TAB3D(3))) THEN
C                                       initialize default values.
            CALL SETDEF
C                                       Process table file header.
            NUMKEY = MXTBKW
            CALL TABHDR (FDVEC, TBIND, ICARD, HLUN, HBUFF, 1, NUMKEY,
     *         KEYWRD, KEYVAL, KEYCHR, KEYTYP, TAPBUF, FITBLK, IERR)
            IF (IERR.LT.0) GO TO 100
            IF (IERR.NE.0) GO TO 900
C                                       Normal table files. Special
C                                       processing if its an AIPS style
C                                       table.
            SRTORD = 0
            IF (ITYPE.NE.'UK') CALL ATCONV (NUMKEY, KEYWRD, KEYVAL,
     *         KEYTYP, KEYCHR, SRTORD)
C                                       Create and initialize the table
C                                       header with data in common.
            IVER = 0
            CALL MAKTAB (SRTORD, IVOL, CNO, IVER, CATBLK, TABLUN, DATP,
     *         BUFFER, JERR)
            IF (JERR.GT.0) GO TO 110
C                                       Trap filling up table type.
            EOF = IVER.GT.MAXTAB
C                                       Prepare keywords
            IKEY = 1
            DO 80 II = 1,NUMKEY
               KEYLOC(II) = IKEY
               IF (KEYTYP(II).EQ.2) KEYTYP(II) = 1
               IF (KEYTYP(II).EQ.1) KEYD = KEYVAL(II)
               IF (KEYTYP(II).EQ.3) THEN
                  JT = JTRIM (KEYCHR(II))
                  CALL CHR2H (8, KEYCHR(II), 1, KEYH)
                  END IF
               IF (KEYTYP(II).EQ.4) KEYI = KEYVAL(II)
               IF (KEYTYP(II).EQ.5) KEYL = KEYVAL(II).GT.0.0D0
               CALL COPY (LENKEY(KEYTYP(II)), KEYD, KEYV(IKEY))
               IKEY = IKEY + LENKEY(KEYTYP(II))
 80            CONTINUE
C                                       Write keywords.
            JERR = 0
            CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER, KEYLOC,
     *         KEYV, KEYTYP, JERR)
            IF (JERR.NE.0) THEN
               CALL TABIO ('CLOS', 0, II, BUFFER, BUFFER, JERR)
               GO TO 110
               END IF
C                                       Read the data from tape and
C                                       write to the table disk file.
            CALL R3DTAB (FDVEC, TBIND, DATP, NAXISI, BUFFER, TAPBUF,
     *         IERR)
            KEEP = MAX (KEEP, 1)
C                                       IMAGE extension
         ELSE IF (EXTTYP.EQ.'IMAGE') THEN
            CALL COPY (256, CATBLK, SAVCAT)
            CALL RIMAGE (IVOL, HLUN, HBUFF, ICARD, IERR)
            CALL COPY (256, SAVCAT, CATBLK)
            IF (IERR.LT.0) GO TO 100
            IF (IERR.GT.0) GO TO 900
            IF (KEEP.LE.0) KEEP = -1
         ELSE
            GO TO 100
            END IF
         NUMTAB = NUMTAB + 1
         GO TO 190
C                                       read rest header code
 100     DOHDR = .TRUE.
C                                       else header already read
 110     CALL SKPEXT (DOHDR, FDVEC, TBIND, HLUN, ICARD, INBLK, HBUFF,
     *      TAPBUF, FITBLK, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Quit if filled up tables.
 190     IF (EOF) GO TO 900
C                                       Change /CFILES/ not to destroy
C                                       on ERROR
         FRW(1) = 1
 200     CONTINUE
C                                       Shouldn't get here.
      WRITE (MSGTXT,1200)
      CALL MSGWRT (6)
C                                       Close history
 900  CALL HICLOS (HLUN, T, HBUFF, JERR)
C                                       Trap too many tables
      IF ((IVER.GT.MAXTAB) .AND. (IERR.EQ.0)) IERR = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('IMGRXM: ASCII TABLE ROW LENGTH',I7,' TOO LONG FOR ME')
 1200 FORMAT ('MORE THAN 32000 EXTENSION FILES. SOME NOT PROCESSED.')
      END
      SUBROUTINE IMGTAB (VOL, HLUN, HBUFF, IRET)
C-----------------------------------------------------------------------
C   IMGTAB processes records following the normal FITS image.  If
C   TABLES <= 0, it simply counts the number of such records.  Else,
C   it parses through the Table records creating the appropriate
C   extension files and adding the table header cards to the
C   history file.
C   Inputs:  VOL    I         Output disk volume #
C            HLUN   I         LUN of open history file
C   In/Out:  HBUFF  I(256)    HI work buffer
C   Output:  IRET   I         Error code: 0 => ok, 8 => some error
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   VOL, HLUN, HBUFF(256), IRET
C
      CHARACTER ISTR*80, SYM*8, CARD*80, CTYPES(2)*4, LLCHAR*4,
     *   SYMS(15)*8, CHTM12*12, TABNAM*8, TTYPE(10)*8
      INTEGER   TABCNT, IRNO, IERR, TERR, TABVER, TABWID, TABCRD, NC,
     *   NPNT, ITYP, NSYMS, NCHAR, KTAB, NTYPES, IT, LUN, INC,
     *   BUFFER(768), IP, I, IREC, J, NDIM, JJ, IST
      LOGICAL   HISERR, EQUAL, NODATA, T, ISHIS
      REAL      RDATA(10), XINC
      DOUBLE PRECISION    X
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA NSYMS, NTYPES, LUN /15, 2, 28/
      DATA CTYPES,     LLCHAR
     *   /' CCC','  CC','LL  '/
      DATA SYMS /'TTYPE1  ', 'TTYPE2  ', 'TTYPE3  ', 'TTYPE4  ',
     *   'TTYPE5  ', 'TTYPE6  ', 'TTYPE7  ', 'TTYPE8  ',
     *   'TTYPE9  ', 'TTYPE10 ', 'TABNAME ', 'TABVER  ',
     *   'TABCOUNT', 'TABWIDTH', 'TABCARDS'/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      CALL HIOPEN (HLUN, FVOL(1), FCNO(1), HBUFF, IERR)
      IERR = 0
      TERR = 0
      IRET = 0
      IF (TABLES.LE.0) GO TO 900
      HISERR = .FALSE.
      IRET = 8
C                                       Loop over tables
      NC = 0
      DO 200 KTAB = 1,TABLES
         WRITE (CARD,1000) KTAB
         IF (.NOT.HISERR) CALL HIAD80 (HLUN, 1, CARD, HBUFF, IERR)
         IF (IERR.NE.0) HISERR = .TRUE.
C                                       Init table parm values
         TABVER = 0
         TABCNT = 0
         TABWID = 0
         TABCRD = 0
         TABNAM = '        '
         DO 20 I = 1,10
            TTYPE(I) = '        '
 20         CONTINUE
C                                       Read and parse header
         DO 90 IREC = 1,100
            IF (IREC.EQ.1) GO TO 50
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
               IF (TERR.EQ.0) GO TO 30
                  WRITE (MSGTXT,1020) TERR
                  GO TO 890
 30            NC = 0
               CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
C                                       card loop
 50         NC = NC + 1
            IF (NC.GT.36) GO TO 90
C                                       card to history
            INC = (NC-1) * 80 + 1
            CARD = FITBLK(INC:)
            IF (HISERR) GO TO 55
               ISHIS = CARD(1:8).EQ.'HISTORY'
               IF (.NOT.ISHIS) ISHIS = CARD(1:8).EQ.'COMMENT'
               IF (.NOT.ISHIS) ISHIS = CARD(1:8).EQ.' '
               IST = 1
               IF (ISHIS) IST = 9
               IF (CARD(1:4).NE.'END ') CALL HIAD80 (HLUN, IST, CARD,
     *            HBUFF, IERR)
               HISERR = IERR.NE.0
C                                       Parse
 55         NPNT = 1
            CALL GETSYM (CARD, NPNT, SYM, ITYP)
            IF (SYM.EQ.'END ') GO TO 100
C                                       only keyword = value accepted
            IF (ITYP.NE.0) GO TO 50
            DO 60 I = 1,NSYMS
               IF (SYM.EQ.SYMS(I)) GO TO 70
 60            CONTINUE
            GO TO 50
C                                       Numeric keywords
 70         IF (I.LE.11) GO TO 80
               CALL GETNUM (CARD, 80, NPNT, X)
               IF (X.EQ.DBLANK) GO TO 875
               IF (I.EQ.12) TABVER = X + 0.01
               IF (I.EQ.13) TABCNT = X + 0.01
               IF (I.EQ.14) TABWID = X + 0.01
               IF (I.EQ.15) TABCRD = X + 0.01
               GO TO 50
C                                       Got a string variable
 80            CALL GETSTR (CARD, 80, 68, NPNT, ISTR, NCHAR)
               NCHAR = MIN (NCHAR, 8)
               IF (I.EQ.11) TABNAM = ISTR(1:NCHAR)
               IF (I.LT.11) TTYPE(I) = ISTR(1:NCHAR)
               GO TO 50
 90         CONTINUE
         WRITE (MSGTXT,1090) KTAB
         GO TO 890
C                                       END card found
C                                       null table
 100     IF ((TABCNT.GT.0) .AND. (TABWID.GT.0)) GO TO 105
            WRITE (MSGTXT,1100) KTAB
            CALL MSGWRT (6)
            GO TO 170
C                                       illegal format
 105     IF ((TABCRD.GT.0) .AND. (TABCRD.LE.40)) GO TO 110
            WRITE (MSGTXT,1105) TABCRD, KTAB
            GO TO 890
C                                       A recognized type?
 110     NODATA = .TRUE.
         IF (TABNAM(1:4).NE.'AIPS') GO TO 125
            DO 115 IT = 1,NTYPES
               IF (CTYPES(IT).EQ.TABNAM(5:8)) GO TO 120
 115           CONTINUE
            GO TO 125
C                                       Yes: do it - CC files only
 120     IF ((IT.NE.1) .AND. (IT.NE.2)) GO TO 125
            NODATA = .FALSE.
C                                       Set correction for old CC
            XINC = 0.0
            IF (IT.NE.2) GO TO 124
               NDIM = CATBLK(KIDIM)
               DO 122 I = 1,NDIM
                  J = (I-1) * 2 + KHCTP
                  CALL H2CHR (4, 1, CATH(J), CHTM12)
                  EQUAL = LLCHAR(1:4).EQ.CHTM12(1:4)
                  IF (EQUAL) XINC = CATR(KRCIC+I-1)
 122              CONTINUE
               WRITE (MSGTXT,1122)
               CALL MSGWRT (2)
 124        CALL CCINI (LUN, TABWID, VOL, CNO, TABVER, CATBLK, BUFFER,
     *         IERR)
            IF (IERR.EQ.0) GO TO 125
               NODATA = .TRUE.
               WRITE (MSGTXT,1124) IERR
               CALL MSGWRT (7)
 125     IP = TABCRD
         DO 160 IRNO = 1,TABCNT
            DO 150 J = 1,TABWID
               IP = IP + 1
               IF (IP.LE.TABCRD) GO TO 140
                  NC = NC + 1
C                                       read a record
                  IF (NC.LE.36) GO TO 130
                     NC = 1
                     CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
                     CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
                     IF (TERR.EQ.0) GO TO 130
                        WRITE (MSGTXT,1020) TERR
                        GO TO 890
C                                       spread new card
 130              IP = 1
                  IF (NODATA) GO TO 140
                     INC = (NC-1) * 80 + 1
                     CARD = FITBLK(INC:)
                     NPNT = 1
 140              IF (NODATA) GO TO 150
                     CALL GETNUM (CARD, 80, NPNT, X)
                     JJ = J
C                                       format correction
                     IF (IT.EQ.1) GO TO 145
                        IF (J.EQ.1) X = X + XINC
                        IF (J.LE.3) JJ = MOD (J, 3) + 1
 145                 RDATA(JJ) = X
 150              CONTINUE
               IF (NODATA) GO TO 160
                  CALL TABIO ('WRIT', 0, IRNO, RDATA, BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 900
 160           CONTINUE
            IF (NODATA) GO TO 170
               CALL TABIO ('CLOS', 0, IRNO, RDATA, BUFFER, IERR)
               WRITE (MSGTXT,1160) 'CC', TABVER
               CALL MSGWRT (2)
               GO TO 200
C                                       Data ignored
 170        CONTINUE
               WRITE (MSGTXT,1170) KTAB
               CALL MSGWRT (2)
               IF (HISERR) GO TO 200
                  CALL HIAD80 (HLUN, 1, MSGTXT, HBUFF, IERR)
                  HISERR = IERR.NE.0
 200     CONTINUE
      IRET = 0
      GO TO 900
C
 875  MSGTXT = 'IMGTAB: BAD NUMBER ON ' // SYM
      IRET = 1
 890  CALL MSGWRT (8)
C                                       Read rest of tape
 900  IF (TERR.NE.4) CALL MLREOF (FDVEC, TBIND, UNKNWN, TAPBUF, IERR)
      IF (IERR.NE.0) IRET = 6
C                                       Close history
      CALL HICLOS (HLUN, T, HBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FITLD  / HEADER FOR TABLE',I7)
 1020 FORMAT ('IMGTAB: TAPE IO ERROR',I7)
 1090 FORMAT ('IMGTAB: NO END TO TABLE HEADER #',I7)
 1100 FORMAT ('TABLE',I7,' HAS NO DATA')
 1105 FORMAT ('IMGTAB: TABCARDS=',I7,' ILLEGAL')
 1122 FORMAT ('Correcting old CC format X positions')
 1124 FORMAT ('IMGTAB: UNABLE TO CREATE EXTENSION FILE',I7)
 1160 FORMAT ('Extension file type ',A2,' version',I4,' written')
 1170 FORMAT ('FITLD / table',I7,' skipped')
      END
      SUBROUTINE IMGFIX (KVOL, NUMTAB, IERR)
C-----------------------------------------------------------------------
C  This routine will fix up the map header of a tape with no data
C  section, but with at least one extension table.
C  Inputs:
C     KVOL    I    Disk volume number.
C     NUMTAB  I    Number of tables files.  0 produces an error message.
C  Output:
C     IERR    I    Error code, 0=ok.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   KVOL, NUMTAB, IERR
C
      CHARACTER MNAME*48, CHTM12*12, CNAME*12, CCLASS*6, STAT*4
      REAL      BUFF(256)
      INTEGER   LBFSZ, DLUN, DIND, INX, INY, IWIN(4), NBKOF,
     *   OUTIND, JERR
      LOGICAL   T
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA DLUN, INX, INY, IWIN, NBKOF /16,1,1, 4*1, 1/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      IF (NUMTAB.LE.0) GO TO 980
C                                       Fix up header.
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), CHTM12)
      IF (CHTM12.EQ.' ') CALL CHR2H (12, EXTNAM, KHIMNO, CATH(KHIMN))
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTM12)
      IF (CHTM12(1:6).EQ.' ') CALL CHR2H (6, EXTTYP, KHIMCO,
     *   CATH(KHIMC))
C                                       Coordinates.
      CALL CHR2H (8, 'DUMMY   ', 1, CATH(KHCTP))
      CALL CHR2H (8, 'DUMMY   ', 1, CATH(KHCTP+2))
C                                       Max, min.
      CATR(KRDMX) = 1.0
      CATR(KRDMN) = 0.0
      CATR(KRBLK) = FBLANK
C                                       Open map file.
      CALL ZPHFIL ('MA', KVOL, CNO, 1, MNAME, IERR)
      CALL ZOPEN (DLUN, DIND, KVOL, MNAME, T, T, T, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Write 2x2 image
C                                       To hell with any write errors
      LBFSZ = 256 * 2
      CALL MINIT ('WRIT', DLUN, DIND, INX, INY, IWIN, BUFF, LBFSZ,
     *   NBKOF, JERR)
      IF (JERR.NE.0) GO TO 900
C                                       Write first row
      CALL MDISK ('WRIT', DLUN, DIND, BUFF, OUTIND, JERR)
      IF (JERR.NE.0) GO TO 900
      BUFF(OUTIND) = 1.0
      BUFF(OUTIND+1) = 0.0
C                                       Write last row
      CALL MDISK ('WRIT', DLUN, DIND, BUFF, OUTIND, JERR)
      IF (JERR.NE.0) GO TO 900
      BUFF(OUTIND) = 1.0
      BUFF(OUTIND+1) = 0.0
C                                       Flush buffer
      CALL MDISK ('FINI', DLUN, DIND, BUFF, OUTIND, JERR)
      IF (JERR.NE.0) GO TO 900
C                                       Close
 900  CALL ZCLOSE (DLUN, DIND, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Clear write status
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), CNAME)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CCLASS)
      STAT = 'CLWR'
      CALL CATDIR ('CSTA', KVOL, CNO, CNAME, CCLASS, CATBLK(KIIMS),
     *   'MA', NLUSER, STAT, BUFF, JERR)
C                                       Clear in /CFILES/
      FRW(1) = 1
      NCFILE = NCFILE - 1
      GO TO 999
C                                       Problem
 970  WRITE (MSGTXT,1970)
      GO TO 990
C                                       No tables.
 980  IERR = 1
      WRITE (MSGTXT,1980)
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('NO DATA AND NO TABLES FOUND. DELETING IMAGE.')
 1970 FORMAT ('FITNDT: ERROR WRITING DUMMY IMAGE')
      END
      SUBROUTINE RENAM (ISLOT, IVOL, IERR)
C-----------------------------------------------------------------------
C   Subroutine to set the catalog slot name, class, and sequence number
C   to match that found in the header.
C   Inputs:
C      ISLOT  I    Catalog slot number to rename.
C      IVOL   I    Disk containing the catalog.
C      COMMON /MAPHDR/
C         CATBLK(K4IMS)   I   Sequence number.
C   Output:
C      IERR   I    0=ok, other is error code from catalog routines.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER XNAMEX*12, CLAS*6, ITYPE*2, STAT*4, KEYWRD(2)*8
      INTEGER   SEQNO, SNO, IERR, ICNO, IVOL, IFIND,
     *   IMAX, IERR2, IMOD, ISLOT, CATLUN, IWORD, NLPR, NWPL,
     *   WBUFF(256), ITEMP, IREC, LOCS(2), KEYTYP(2)
      REAL      DVALUE(4)
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA CATLUN /15/
      DATA KEYWRD /'ISCALE','IZERO'/
C-----------------------------------------------------------------------
C                                       Add ISCALE and IZERO to CB file
      IF ((ISCALE.NE.1.0D0) .OR. (IZERO.NE.0.0D0)) THEN
         LOCS(1) = 1
         LOCS(2) = 1 + NWDPDP
         CALL RCOPY (NWDPDP, ISCALE, DVALUE(LOCS(1)))
         CALL RCOPY (NWDPDP, IZERO, DVALUE(LOCS(2)))
         KEYTYP(1) = 1
         KEYTYP(2) = 1
         CALL CATKEY ('WRIT', IVOL, ISLOT, KEYWRD, 2, LOCS, DVALUE,
     *      KEYTYP, WBUFF, IERR)
         END IF
C                                       Prepare variables.
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), XNAMEX)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLAS)
      ITYPE = '  '
      CATBLK(KIIMU) = NLUSER
C                                       Check for default seqno
      SEQNO = CATBLK(KIIMS)
C                                       Is user name unique?
      IF (SEQNO.GT.0) GO TO 10
         ICNO = 1
         SNO = 0
         ITEMP = 0
         CALL CATDIR ('SRCH', ITEMP, ICNO, XNAMEX, CLAS, SNO, ITYPE,
     *      CATBLK(KIIMU), STAT, WBUFF, IERR)
         IF ((IERR.GT.0) .AND. (IERR.NE.5)) GO TO 15
         SEQNO = SNO + 1
         IF (IERR.EQ.5) SEQNO = 1
 10   SNO = SEQNO
      IERR = 0
      ICNO = 1
      ITYPE = '  '
      CALL CATDIR ('SRNN', IVOL, ICNO, XNAMEX, CLAS, SNO, ITYPE,
     *   CATBLK(KIIMU), STAT, WBUFF, IERR)
      IF (IERR.EQ.5) GO TO 20
C                                       No. Print error message.
      IF (IERR.NE.0) GO TO 15
         IERR = 2
         WRITE (MSGTXT,1010) XNAMEX, CLAS, SNO
         CALL MSGWRT (6)
         GO TO 999
C                                       catlg error
 15   CONTINUE
         WRITE (MSGTXT,1015) IERR
         IERR = 3
         CALL MSGWRT (6)
         GO TO 999
C                                       Yes.
 20   CALL H2CHR (2, KHPTYO, CATH(KHPTY), ITYPE)
      WRITE (MSGTXT,1020) XNAMEX, CLAS, SNO, ITYPE, IVOL, ISLOT
      CALL MSGWRT (2)
      CALL CATOPN (IVOL, IFIND, WBUFF, IMAX, IERR)
      IF (IERR.NE.0) GO TO 15
C                                       Catalog entry location:
      NWPL = 10
      NLPR = 256 / NWPL
      IMOD = (ISLOT - 1) / NLPR
      IREC = 2 + IMOD
      IWORD = 1 + NWPL * (ISLOT - NLPR*IMOD - 1)
C                                       Load proper catalog record.
      CALL ZFIO ('READ', CATLUN, IFIND, IREC, WBUFF, IERR)
C                                       Make changes.
      CALL CHR2H (12, XNAMEX, 1, WBUFF(IWORD+5))
      CALL CHR2H (6, CLAS, 13, WBUFF(IWORD+5))
      WBUFF(IWORD+4) = SNO
C                                       Resave record.
      CALL ZFIO ('WRIT', CATLUN, IFIND, IREC, WBUFF, IERR)
      CALL ZCLOSE (CATLUN, IFIND, IERR2)
C                                       Update header with version no.
      CATBLK(KIIMS) = SNO
      CALL CATIO ('UPDT', IVOL, ISLOT, CATBLK, 'REST', WBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('RENAM: DUPLICATE NAME ',A12,'.',A8,'.',I4)
 1015 FORMAT ('RENAM: CATALOG ERROR',I6)
 1020 FORMAT ('Rename ',A12,'.',A6,'.',I4,' (',A2,')  on disk',I2,
     *   ' cno',I5)
      END
      SUBROUTINE RIMAGE (IVOL, HLUN, HBUFF, ICARD, IERR)
C-----------------------------------------------------------------------
C   RIMAGE reads an IMAGE extension.  It creates and fills that image,
C   makes a history file using the current contents of the master image,
C   and then closes the map file.
C   Inputs
C      IVOL    I      Disk to use
C      HLUN    I      HI file open in master
C   In/out:
C      HBUFF   I(*)   HI bufffer
C   Output:
C      IERR    I      error code
C-----------------------------------------------------------------------
      INTEGER   IVOL, HLUN, HBUFF(*), ICARD, IERR
C
      INTEGER   LCNO, LHBUFF(256), LHLUN, HERR, SEQ, I, J, IREC,
     *   IST, IS, IE, IN, IAX
      CHARACTER LINE*80, CHTEMP*8, NAMEXT*16
      LOGICAL   NODATA, ISHIST, END, DOHI
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LHLUN /29/
C-----------------------------------------------------------------------
      IF ((GCOUNT.GT.1) .OR. (PCOUNT.GT.0)) THEN
         WRITE (MSGTXT,1010) PCOUNT, GCOUNT
         CALL MSGWRT (8)
         IERR = -1
         GO TO 999
         END IF
C                                       close input HI file
      CALL HICLOS (HLUN, .TRUE., HBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CLOSING MASTER HI FILE'
         CALL MSGWRT (6)
         END IF
C                                       revise header for new image
      CATBLK(KIDIM) = NAXIS
      CALL COPY (KICTPN, NAXISI, CATBLK(KINAX))
      J = NAXISI(1)
      DO 10 I = 2,NAXIS
         J = J * NAXISI(I)
 10      CONTINUE
      IF (NAXIS.EQ.0) J = 0
      NODATA = J.LE.0
C                                       no extensions here yet
      CALL CATCLR (CATBLK)
C                                       Create map with temporary name.
C                                       Map will be renamed later.
      CALL CHR2H (12, 'FITLD       ', KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, 'TEMP  ', KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = 0
      CATBLK(KIIMU) = NLUSER
      CALL MCREAT (IVOL, LCNO, HBUFF, IERR)
C                                       Blank name back out.
      CALL CHR2H (12, '            ', KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, '      ', KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = 0
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CREATING MA FILE FOR IMAGE EXT'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FCNO(NCFILE) = LCNO
      FRW(NCFILE) = 2
      FVOL(NCFILE) = IVOL
C                                       copy old history
      CALL HISCOP (HLUN, LHLUN, IVOL, FVOL(NCFILE), CNO, LCNO,
     *   CATBLK, HBUFF, LHBUFF, HERR)
      IF (HERR.NE.0) THEN
         WRITE (MSGTXT,1000) HERR, 'ADDING NEW HI TO IMAGE EXT'
         CALL MSGWRT (7)
         END IF
      LINE = 'IMLOD / IMAGE extension header HIstory'
      IF (HERR.EQ.0) CALL HIAD80 (LHLUN, 1, LINE, LHBUFF, HERR)
C                                       parse rest of header.
      ICARD = ICARD + 1
      NAMEXT = ' '
C                                       Loop until END card found.
      DO 20 IREC = 1,100000000
C                                       Read next record.
         IF (ICARD.GT.36) THEN
            CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
            ICARD = 1
            END IF
C                                       Parse card, put value in hdr.
         CALL IMPARS (ICARD, ISHIST, END, IERR)
         IF (END) GO TO 50
         IST = 80 * ICARD - 79
         CHTEMP = FITBLK(IST:)
         IF (CHTEMP.EQ.'EXTNAME') THEN
            LINE = FITBLK(IST:)
            IS = INDEX (LINE, '''')
            IF (IS.GT.0) THEN
               IE = INDEX (LINE(IS+1:), '''')
               IF (IE.GT.1) THEN
                  IE = IE + IS - 1
                  NAMEXT = LINE(IS+1:IE)
                  END IF
               END IF
            END IF
C                                       Add to history file.
         IF (IERR.GT.0) THEN
            DOHI = (CHTEMP.EQ.'HISTORY') .OR. (CHTEMP.EQ.'COMMENT')
     *         .OR. (CHTEMP.EQ.' ')
            LINE = FITBLK(IST:)
            IST = 1
            IF (DOHI) IST = IST + 8
            IF (HERR.EQ.0) CALL HIAD80 (LHLUN, IST, LINE, LHBUFF, HERR)
         ELSE IF (IERR.EQ.-1) THEN
            IS = (ICARD - 1) * 80 + 1
            IE = IS + 79
            CALL PUTCRD (FITBLK(IS:IE), IVOL, LCNO, IERR)
            IF (IERR.GT.1) THEN
               WRITE (MSGTXT,1000) IERR, 'CALLING PUTCRD'
               CALL MSGWRT (7)
               GO TO 999
               END IF
            END IF
         ICARD = ICARD + 1
 20      CONTINUE
C                                       Read more cards than we expected
      MSGTXT = 'READ MORE THAN 10**8 CARDS WITHOUT FINDING AN END CARD'
      IERR = 1
      GO TO 990
C                                       End card found.
C                                       Make axis increments non zero
C                                       to help out dumb programs.
 50   IN = KINAX
      IS = KRCIC
      IE = IS + CATBLK(KIDIM) - 1
      DO 60 IAX = IS,IE
         IF ((CATR(IAX).EQ.0.0) .AND. (CATBLK(IN).EQ.1)) CATR(IAX) = 1.0
         IN = IN + 1
 60      CONTINUE
C                                       End FITS header section in HI
      LINE = '/END FITS IMAGE extension header "HISTORY" information'
      IF (HERR.EQ.0) CALL HIADD (LHLUN, LINE, LHBUFF, HERR)
      CALL HICLOS (LHLUN, .TRUE., LHBUFF, IERR)
      IF ((IERR.NE.0) .OR. (HERR.NE.0)) THEN
         HERR = MAX (IERR, HERR)
         WRITE (MSGTXT,1000) HERR, 'WRITING IMAGE EXT HI FILE'
         CALL MSGWRT (7)
         END IF
C                                       reopen main file HI
      CALL HIOPEN (HLUN, IVOL, CNO, HBUFF, HERR)
      IF (.NOT.NODATA) THEN
         TAPEBP = BITPIX
         CALL IMGDAT (IVOL, LCNO, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING IMAGE EXT DATA'
            GO TO 990
            END IF
         END IF
C                                       Fill in default names if needed.
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      IF (NAMOUT.EQ.' ') THEN
         NAMOUT = NAMEXT
      ELSE IF (CLAOUT.EQ.' ') THEN
         CLAOUT = NAMEXT
         END IF
      SEQ = 0
      XOUTS = 0
      CALL IMGDEF (NAMOUT, CLAOUT, SEQ)
C                                       Renames to the proper name.
      CALL RENAM (LCNO, IVOL, IERR)
      CALL IMQUIT
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RIMAGE ERROR:',I3,' ON ',A)
 1010 FORMAT ('RIMAGE FOUND ILLEGAL PCOUNT, GCOUNT =',2I5)
      END
      SUBROUTINE IMPARS (ICARD, ISHIST, END, IERR)
C-----------------------------------------------------------------------
C   IMPARS (parse FITS card) will unpack and interpret a card image
C   from a block of FITS data and put that data into the internal AIPS
C   header.
C   Inputs:
C      ICARD   I         The card number (1-36) in block to interpret.
C      FITBLK  C*2880    A block of FITS header data.
C   Outputs:
C      ISHIST  L         True iff a history card
C      END     L         True if end card found, else false.
C      IERR    I         error code 0=ok. 1=error, -1 => header keyword
C   COMMON /MAPHDR/
C   COMMON /FITINF/
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ICARD, IERR
      LOGICAL   ISHIST, END
C
      CHARACTER SYMBOL*8, STR*68, KL*80
      DOUBLE PRECISION X
      REAL      VAL
      LOGICAL   LHIST, FIRST
      INTEGER   KPNTR(58), PNTR, IPOFF, TABNO, NPNT, KT, IL, IVAL,
     *   NCHAR, NBYT, NN, NNSTR, JT, JTRIM, NPNTS
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIT.INC'
      EQUIVALENCE (KPNTR(1), KHOBJ)
      INCLUDE 'INCS:VFIT.INC'
C-----------------------------------------------------------------------
C                                       Find next symbol on the card
C                                       and look for it in the table.
      NPNT = 1
      NN = NKT + NCT
      NNSTR = NCT + 1
C                                       Loop for all possible values
C                                       on an AIPS HISTORY card.
      FIRST = .TRUE.
 10   CONTINUE
      CALL GETCRD (ICARD, NN, NNSTR, AWORD, FITBLK, NPNT, KL, SYMBOL,
     *    TABNO, LHIST, END, IERR)
      IF (END) GO TO 999
      IF (FIRST) ISHIST = LHIST
      FIRST = .FALSE.
      IF (ISHIST) THEN
         IF (IERR.EQ.1) GO TO 10
         IF ((USED(TABNO).GT.0) .AND. (IERR.EQ.0)) GO TO 10
         END IF
      IF ((IERR.EQ.1) .AND. ((SYMBOL(:2).EQ.'CD') .OR.
     *   (SYMBOL(:2).EQ.'PC') .OR. (SYMBOL(:2).EQ.'PV')))
     *   CALL PCCARD (0, KL, PCMATX, CDMATX, PVMATX)
      IF (IERR.NE.0) GO TO 999
      IF (.NOT.ISHIST) USED(TABNO) = USED(TABNO) + 1
C                                       Header pointer for this
C                                       keyword, number bytes and
C                                       offset position from pointer
      PNTR = MOD (APOINT(TABNO), 1000)
      IPOFF = PNTR / 100
      PNTR = MOD (PNTR, 100)
      IF (PNTR.GT.0) PNTR = KPNTR(PNTR)
      NBYT = APOINT(TABNO) / 1000
C                                       Type value of keyword
C                                       1=LOGICAL
C                                       2=NUMBER
C                                       3=STRING
      KT = ATYPE(TABNO)
C                                       Logical value
      NPNTS = NPNT
      IF (KT.EQ.1) THEN
         CALL GETLG (KL, 80, NPNT, IL)
C                                       Illegal logical value.
         IF (IL.LT.0) THEN
            MSGTXT = 'LOGICAL VARIABLE HAS ILLEGAL VALUE: ' // SYMBOL
            GO TO 990
            END IF
C                                       Handle normal logical cases.
         CATBLK(PNTR+IPOFF) = IL
C                                       Number
      ELSE IF (KT.EQ.2) THEN
         CALL GETNUM (KL, 80, NPNT, X)
C                                       special parse for EQUINOX
         IF (X.EQ.DBLANK) THEN
            IF ((AWORD(TABNO).EQ.'EQUINOX') .OR.
     *         (AWORD(TABNO).EQ.'EPOCH')) THEN
               NPNT = NPNTS
               CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
               IF (INDEX(STR,'1950').GT.0) THEN
                  X = 1950.0D0
               ELSE IF (INDEX(STR,'2000').GT.0) THEN
                  X = 2000.0D0
                  END IF
               END IF
            END IF
         IF (X.EQ.DBLANK) THEN
            MSGTXT = 'IMPARS: BAD NUMBER ON ' // SYMBOL
            CALL MSGWRT (7)
            X = 0.0D0
            END IF
C                                       Check for number special cases.
C                                       Blank pixel value.
         IF (AWORD(TABNO).EQ.'BLANK') THEN
            IF (X.EQ.-2147483648.0D0) THEN
               IBLNK = -2147483647 - 1
            ELSE
               IBLNK = X
               END IF
            ISBLNK = .TRUE.
C                                       PDP 11 Stuff
         ELSE IF ((AWORD(TABNO).EQ.'OPHRAE11') .OR.
     *      (AWORD(TABNO).EQ.'OPHDCE11')) THEN
            POS11(IPOFF) = X
            FUCKUP = .TRUE.
C                                       Handle normal cases. Put value
C                                       into proper header slot.
C                                       2-byte integer
         ELSE IF (NBYT.EQ.2) THEN
            IVAL = X + SIGN (0.5D0, X)
            IF (PNTR.GT.0) THEN
               CATBLK(PNTR+IPOFF) = IVAL
            ELSE
               IF (AWORD(TABNO).EQ.'BITPIX') TAPEBP = IVAL
               IF (AWORD(TABNO).EQ.'TABLES') TABLES = IVAL
               END IF
C                                       4-byte real
         ELSE IF (NBYT.EQ.4) THEN
            IF ((AWORD(TABNO).EQ.'HISTORY') .AND. (X.GT.1.0E30))
     *         X = 1.0E30
            VAL = X
            IF (PNTR.GT.0) CATR(PNTR+IPOFF) = VAL
C                                       8-byte real
         ELSE IF (NBYT.EQ.8) THEN
            IF (PNTR.GT.0) THEN
               CATD(PNTR+IPOFF) = X
            ELSE
               IF (AWORD(TABNO).EQ.'BSCALE') SCALE = X
               IF (AWORD(TABNO).EQ.'ISCALE') ISCALE = X
               IF (AWORD(TABNO).EQ.'BZERO') OFFSET = X
               IF (AWORD(TABNO).EQ.'IZERO') IZERO = X
               END IF
            END IF
C                                       String
      ELSE IF (KT.EQ.3) THEN
         CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
C                                       Dates are special
         IF (AWORD(TABNO)(:4).EQ.'DATE') THEN
            CALL DATFST ('F2L', STR)
            NCHAR = 8
            END IF
         NCHAR = MIN (NBYT, NCHAR)
C                                       Start string on integer boundary
C                                       IMCLASS
         IF (AWORD(TABNO).EQ.'IMCLASS') THEN
            IPOFF = NBYT * IPOFF + 1
            CALL CHFILL (NBYT, HBLANK, IPOFF, CATH(PNTR))
            JT = JTRIM (STR(:NCHAR))
            CALL CHR2H (NCHAR, STR, IPOFF, CATH(PNTR))
C                                       Start string on real boundary.
         ELSE
            IPOFF = (NBYT / 4) * IPOFF
            CALL CHFILL (NBYT, HBLANK, 1, CATH(PNTR+IPOFF))
            JT = JTRIM (STR(:NCHAR))
            CALL CHR2H (NCHAR, STR, 1, CATH(PNTR+IPOFF))
            END IF
         END IF
C                                       If this is a history card, look
C                                       for more values.
      IF (ISHIST) GO TO 10
      GO TO 999
C                                       Error message
 990  CALL MSGWRT (7)
      IERR = 1
C
 999  RETURN
      END
      SUBROUTINE IMREQC (ICARD, IERR)
C-----------------------------------------------------------------------
C   This routine will look for the required cards in a FIT header block
C   SIMPLE, BITPIX, NAXIS, NAXISn, and update a catalog header with the
C   information from these cards.
C   Inputs:
C      FITBLK  C*2880   a block of fit header data.
C   Outputs:
C      ICARD   I        The number of the last card parsed.
C      IERR    I        0=ok, 1=messed up. An error message will
C                                     be printed.
C   COMMON /MAPHDR/ Axis dimension information will be filled in.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ICARD, IERR
C
      CHARACTER SYMBOL*8, KL*80
      INTEGER   NPNT, ITYP, NAXIS, ITABNO, IVAL, IKEYWD, I, IAX
      LOGICAL   ISHIST, END
      DOUBLE PRECISION X
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFUV.INC'
      INCLUDE 'INCS:VFUV.INC'
C-----------------------------------------------------------------------
C                                       Look for SIMPLE=T card
      ICARD = 1
      IKEYWD = 1
      NPNT = 1
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GETLG (KL, 80, NPNT, ITYP)
      USED(ITABNO) = USED(ITABNO) + 1
C                                       Not .TRUE.
      IF (ITYP.NE.1) GO TO 940
C                                       Look for BITPIX.
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
      USED(ITABNO) = USED(ITABNO) + 1
C                                       Check value of BITPIX
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      IF (X.GE.0.) IVAL = X + 0.1
      IF (X.LT.0.) IVAL = X - 0.1
      TAPEBP = IVAL
      IF ((IVAL.NE.8) .AND. (IVAL.NE.16) .AND. (IVAL.NE.32) .AND.
     *   (IVAL.NE.-32) .AND. (IVAL.NE.-64)) GO TO 950
      IF (IVAL.EQ.-64) THEN
         MSGTXT = 'WARNING: 64-bit input stored in 32 bits inside AIPS'
         CALL MSGWRT (6)
         END IF
C                                       Check NAXIS
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 980
      USED(ITABNO) = USED(ITABNO) + 1
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      NAXIS = X + .01
C
      IAX = KINAX
      CATBLK(KIDIM) = NAXIS
C                                       Check for invalid no. of axis
C                                       for our header.
      IF (NAXIS.GT.7) GO TO 960
C                                       Check NAXISm
      IF (NAXIS.GE.1) THEN
         DO 30 I = 1,NAXIS
            ICARD = ICARD + 1
            IKEYWD = IKEYWD + 1
            NPNT = 1
            CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT,
     *         KL, SYMBOL, ITABNO, ISHIST, END, IERR)
            IF (IERR.NE.0) GO TO 970
            USED(ITABNO) = USED(ITABNO) + 1
            CALL GETNUM (KL, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 975
            CATBLK(IAX) = X + .01
            IAX = IAX + 1
 30         CONTINUE
         END IF
      IF (CATBLK(KINAX).GT.0) GO TO 999
C                                       Not SIMPLE FITS tape.
 940  WRITE (MSGTXT,1940)
      GO TO 980
C                                       Invalid bits per pixel value.
 950  WRITE (MSGTXT,1950) IVAL
      GO TO 980
C                                       Invalid number of axis.
 960  WRITE (MSGTXT,1960) NAXIS
      GO TO 980
C                                       Expected keyword not found.
 970  WRITE (MSGTXT,1970) CWORD(IKEYWD), SYMBOL
      GO TO 980
 975  MSGTXT = 'IMREQC: BAD NUMBER ON ' // SYMBOL
C                                       Print error message set flag.
 980  CALL MSGWRT (6)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1940 FORMAT ('NOT SIMPLE FITS TAPE. PROGRAM STOPPING.')
 1950 FORMAT ('INVALID BITS PER PIXEL =',I6)
 1960 FORMAT ('INVALID NUMBER OF AXIS =',I6)
 1970 FORMAT ('EXPECTED KEYWORD ',A8,'. FOUND ',A8,'.')
      END
      SUBROUTINE IMGDEF (NAMEX, CLASSX, SEQ)
C-----------------------------------------------------------------------
C   IMGDEF fills the image name (name,class,seq) with default values.
C   This subroutine is used with FITLD.
C   Inputs/Output:
C      NAMEX    C*12  Input image name
C      CLASSX   C*6   Input image class
C      SEQ      I     Input image Sequence
C   Outputs:
C      CATBLK(KIIMS) I     Image sequence
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER DNAME*12, DCLASS*6, NAMEX*12, CLASSX*6,
     *   NONE*8, STOKES*8, WTYP(5)*4,
     *   STOK(5)*2, STOK2(5)*4, CHTM12*12
      INTEGER   SEQ, STNUM, NAX, I, J, DSEQ
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA NONE, STOKES /'NONE    ','STOKES  '/
      DATA WTYP /'MAP ','CMP ','RES ','PNT ','CLN '/
      DATA STOK /'I ','I ','Q ','U ','V '/
      DATA STOK2 /'PPOL','FPOL','PANG','SPIX','OPTD'/
C-----------------------------------------------------------------------
C                                       check type
      IF ((CATBLK(KITYP).LE.1) .OR. (CATBLK(KITYP).GT.4))
     *   CATBLK(KITYP) = 1
C                                       name default
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), DNAME)
      IF (DNAME.NE.' ') GO TO 20
         CALL H2CHR (8, 1, CATH(KHOBJ), CHTM12)
         IF (CHTM12(1:8).EQ.' ') CALL CHR2H (8, NONE, 1, CATH(KHOBJ))
         CALL H2CHR (8, 1, CATH(KHOBJ), DNAME(1:8))
         DNAME(9:12) = ' '
C                                       class default
 20   CALL H2CHR (6, KHIMCO, CATH(KHIMC), DCLASS)
C                                       Stokes value 1st char
      IF (DCLASS.NE.' ') GO TO 30
         STNUM = 2
         NAX = CATBLK(KIDIM)
         DO 25 I = 1,NAX
            J = (I-1)*2 + KHCTP
            CALL H2CHR (8, 1, CATH(J), CHTM12)
            IF (STOKES.EQ.CHTM12(1:8)) STNUM = CATD(KDCRV+I-1) + 1.5
 25         CONTINUE
C                                       clean type : 2-4 chars
         IF (STNUM.LE.5) THEN
            J = CATBLK(KITYP)
            IF ((J.EQ.1) .AND. (CATBLK(KINIT).GT.0)) J = 5
            IF (STNUM.EQ.1) DCLASS = STOK(STNUM)(1:1) // 'BEM  '
            IF (STNUM.NE.1) DCLASS = STOK(STNUM)(1:1) // WTYP(J)(1:3)
     *         // '  '
         ELSE
C                                       Special "Stokes" values
            DCLASS = STOK2(STNUM-5)(1:4) // '  '
            END IF
C                                       sequence number
C                                       fill in cat block
 30   DSEQ = CATBLK(KIIMS)
      CATBLK(KIIMS) = SEQ
      CALL MAKOUT (DNAME, DCLASS, DSEQ, DCLASS, NAMEX, CLASSX,
     *   CATBLK(KIIMS))
      CALL CHR2H (12, NAMEX, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASSX, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
C
 999  RETURN
      END
      SUBROUTINE IMQUIT
C-----------------------------------------------------------------------
C  Print out map header summary.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FITLD.INC'
C-----------------------------------------------------------------------
C                                       display header
      CALL LSTHDR (CATBLK, CATH, CATR, CATD, ISCALE, IZERO)
C
 999  RETURN
      END
      SUBROUTINE BTFHIS (IERR)
C-----------------------------------------------------------------------
C   BTFHIS reads the tape (which must be open and positioned at begin.
C   of file) and builds a history file from the FITS history and other
C   keywords in the FITS header.  The history file is attached to a
C   scratch file and is copied over to the relevant data file after
C   the UV data have been loaded.
C   This routine leaves the tape positioned at the start of the binary
C   data.
C   INPUT:
C      IHLUN   I   history file logical unit number (from common).
C   Output:
C      IERR    I   =0 => ok, other => quit
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER KL*80, SYMBOL*8, LNAM*12, LCLAS*6, CARD*80, HILINE*72,
     *   CHTMP*8, STR*80, RTYPES(13)*8, TYPES(7)*8, PRIBND*24
      DOUBLE PRECISION    DABLK(128), X, CRVAL(7)
      REAL      ABLK(256), CRPIX(7), CRINC(7), VALUES(2)
      INTEGER   KST, CLEN, LST, NNDEX, IABLK(512), IERR, ICARD, ITRIM,
     *   IREC, TABNO, KPNTR(58), INC, IST, I, NOANT, ISIZE, WBUFF(512),
     *   NPNT, NN, NNSTR, MPNT, IHERR, JERR, I4, PNTR, IPOFF, NBYT, KT,
     *   IVAL, NCHAR, NAXIS, NRAN, NDIM(7), CATEQV(256), NPNTS, NUMKEY,
     *   LOCS, ITYPE
      LOGICAL   UPDATE, END, ISHIST, EQUAL
      REAL      CATRHI(256)
      HOLLERITH CATHHI(256)
      DOUBLE PRECISION CATDHI(128)
      EQUIVALENCE (CATEQV, CATRHI, CATHHI, CATDHI)
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DFUV.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (DABLK, ABLK, IABLK)
      EQUIVALENCE (KPNTR(1), KHOBJ)
      INCLUDE 'INCS:VFUV.INC'
      DATA UPDATE /.TRUE./
C                                         No. random parameters.
      DATA NRAN /11/
C                                         Rand. parm. names.
      DATA RTYPES /'UU-L-SIN','VV-L-SIN','WW-L-SIN',
     *   'SUBARRAY','TIME1   ','SOURCE  ','FREQSEL ',
     *   'INTTIM', 'ANTENNA1', 'ANTENNA2', 'CORR-ID ',
     *   'WEIGHT  ','SCALE   '/
C                                       Uniform axes.
C                                         No. axes.
      DATA NAXIS /6/
C                                         Axes names.
      DATA TYPES /'COMPLEX ','STOKES  ','FREQ    ',
     *   'IF      ','RA      ','DEC     ','        '/
C                                         Axis dimensions
      DATA NDIM /3,0,0,0,1,1,1/
C                                         Reference values
      DATA CRVAL /1.0D0, 0.0D0, 1.0D0, 1.0D0, 3*0.0D0/
C                                         Reference pixel.
      DATA CRPIX /7*1.0/
C                                         Coordinate increment.
      DATA CRINC /1.0, -1.0, 5*1.0/
C-----------------------------------------------------------------------
      INSTRU = ' '
      CORREL = ' '
C                                       Init number of ant records.
      NOANT = 0
      NN = NCT + NKT
      NNSTR = NCT + 1
C                                       Init output name
      KOUTNM = ' '
      KOUTCL = ' '
      KOUTS = 0
C                                       Create a scratch file
      ISIZE = 60
      CALL SCREAT (ISIZE, WBUFF, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
      IHDISK = SCRVOL(NSCR)
      IHCNO = SCRCNO(NSCR)
      HISSCR = NSCR
C                                       Create dummy HI catblk
      CALL FILL (256, 0, CATHIS)
      CALL FILL (256, 0, CATEQV)
      DO 10 I = 1,KIPTPN
         NNDEX = KHPTP + (I-1) * 2
         IF (I.LE.NRAN) THEN
            CALL CHR2H (8, RTYPES(I), 1, CATHHI(NNDEX))
         ELSE
            CALL CHR2H (8, '        ', 1, CATHHI(NNDEX))
            END IF
 10      CONTINUE
C                                       Uniform axes
      DO 30 I = 1,KICTPN
C                                       Init dimension
         CATEQV(KINAX+I-1) = NDIM(I)
C                                       Init. increment.
         CATRHI(KRCIC+I-1) = CRINC(I)
C                                       Init. rotation.
         CATRHI(KRCRT+I-1) = 0.0
C                                       Init. ref pixel.
         CATRHI(KRCRP+I-1) = CRPIX(I)
C                                       Init. ref value.
         CATDHI(KDCRV+I-1) = CRVAL(I)
C                                       Fill axis type from
C                                       TYPES
         NNDEX = KHCTP + (I-1) * 2
         CALL CHR2H (8, TYPES(I), 1, CATHHI(NNDEX))
 30      CONTINUE
C                                       Set number of axes.
      CATEQV(KIDIM) = NAXIS
      CATEQV(KIPCN) = NRAN
      CATEQV(KIIMU) = NLUSER
      CALL COPY (256, CATEQV, CATHIS)
C                                       Create HI file
      CALL HICREA (IHLUN, IHDISK, IHCNO, CATHIS, IHBLK, IHERR)
C                                       Header msg in HI

      WRITE (HILINE,1000) ('----', I = 1,17)
      IF (IHERR.EQ.0) CALL HIADD (IHLUN, HILINE, IHBLK, IHERR)
      WRITE (HILINE,1002)
      IF (IHERR.EQ.0) CALL HIADD (IHLUN, HILINE, IHBLK, IHERR)
C                                       Read record 1 from tape.
      CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 960
      CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
C                                       Skip required cards.
      ICARD = ICEND
C                                       Loop until END card found.
      DO 400 IREC = 1,100000000
         ICARD = ICARD + 1
C                                       Read next non-blank record.
         CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 960
C                                       Parse card, put unknown cards
C                                       in history file.
         INC = (ICARD-1) * 80 + 1
         CHTMP = FITBLK(INC:INC+7)
         EQUAL = ('HISTORY'.EQ.CHTMP) .OR. ('COMMENT'.EQ.CHTMP) .OR.
     *      (' '.EQ.CHTMP)
         IST = 1
         IF (EQUAL) IST = 9
         NPNT = 1
         IF ((TABNO.EQ.65) .OR. (TABNO.EQ.66)) THEN
            ICARD = ICARD - 1
            NPNT = MPNT
            END IF
         CALL GETCRD (ICARD, NN, NNSTR, AWORD, FITBLK, NPNT, KL,
     *      SYMBOL, TABNO, ISHIST, END, JERR)
         IF (END) GO TO 40
C                                       Correlator code version
         IF (CORVER.LT.4.17) CORVER = 4.16
         IF (CHTMP.EQ.'FXCORVER') THEN
            CALL GETSTR (KL, 80, 4, NPNT, STR, NCHAR)
            READ (STR,1075) CORVER
            IF (NMRDUN.EQ.1) THEN
               WRITE (MSGTXT,1076) CORVER
               CALL MSGWRT (2)
               END IF
            END IF
C                                       Trap instrument too
         IF (CHTMP.EQ.'INSTRUME') THEN
            CALL GETSTR (KL, 80, 24, NPNT, INSTRU, NCHAR)
            END IF
         IF (CHTMP.EQ.'CORRELAT') THEN
            CALL GETSTR (KL, 80, 24, NPNT, CORREL, NCHAR)
            IF (CORREL.NE.' ') THEN
               CALL CHR2H (8, CORREL, 1, VALUES)
               NUMKEY = 1
               LOCS = 1
               ITYPE = 3
               CALL CATKEY ('WRIT', IHDISK, IHCNO, CHTMP, NUMKEY, LOCS,
     *            VALUES, ITYPE, SCRBUF, I)
               END IF
            END IF
         IF (CHTMP.EQ.'PRIBAND') THEN
            CALL GETSTR (KL, 80, 24, NPNT, PRIBND, NCHAR)
            IF (PRIBND.NE.' ') THEN
               CALL CHR2H (8, PRIBND, 1, VALUES)
               NUMKEY = 1
               LOCS = 1
               ITYPE = 3
               CALL CATKEY ('WRIT', IHDISK, IHCNO, CHTMP, NUMKEY, LOCS,
     *            VALUES, ITYPE, SCRBUF, I)
               END IF
            END IF
C                                       special header keywords
         IF (JERR.EQ.-1) THEN
            CALL PUTCRD (CARD, IHDISK, IHCNO, JERR)
            IF (JERR.GT.1) THEN
               WRITE (MSGTXT,1020) JERR
               CALL MSGWRT (7)
               IERR = JERR
               GO TO 999
               END IF
            GO TO 400
            END IF
         IF ((JERR.EQ.0) .AND. (IST.EQ.1)) GO TO 400
C                                       Special case of file name
C                                       encoded after 'HISTORY AIPS'
C                                       keywords.
         IF ((TABNO.EQ.65) .OR. (TABNO.EQ.66) .OR.
     *      (TABNO.EQ.67)) THEN
            PNTR = MOD (APOINT(TABNO), 1000)
            IPOFF = PNTR / 100
            PNTR = MOD (PNTR, 100)
            IF (PNTR.GT.0) PNTR = KPNTR(PNTR)
            NBYT = APOINT(TABNO) / 1000
            KT = ATYPE (TABNO)
            GO TO (100, 200, 300), KT
C                                       Logical value
 100        GO TO 400
C                                       Number
 200        CALL GETNUM (KL, 80, NPNT, X)
C                                       special parse for EQUINOX
            IF (X.EQ.DBLANK) THEN
               IF ((AWORD(TABNO).EQ.'EQUINOX') .OR.
     *            (AWORD(TABNO).EQ.'EPOCH')) THEN
                  NPNT = NPNTS
                  CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
                  IF (INDEX(STR,'1950').GT.0) THEN
                     X = 1950.0D0
                  ELSE IF (INDEX(STR,'2000').GT.0) THEN
                     X = 2000.0D0
                     END IF
                  END IF
               END IF
            IF (X.EQ.DBLANK) THEN
               MSGTXT = 'BFTHIS: BAD NUMBER ON ' // SYMBOL
               CALL MSGWRT (7)
               X = 0.0D0
               END IF
C                                       Check for number special cases.
C                                       Blank pixel value.
            IF (AWORD(TABNO).NE.'BLANK') GO TO 220
            IF (X.EQ.-2147483648.0D0) THEN
               IBLANK = -2147483647 - 1
            ELSE
               IBLANK = X
               END IF
            GO TO 400
C                                       Handle normal cases. Put value
C                                       into proper header slot.
C                                       4-byte integer
 220        IF (NBYT.NE.2) GO TO 400
            IVAL = X + SIGN (0.5D0, X)
            IF (PNTR.GT.0) KOUTS = IVAL
            GO TO 400
C                                       String
 300        CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
C                                       Dates are special
            IF (AWORD(TABNO)(:4).EQ.'DATE') THEN
               CALL DATFST ('F2L', STR)
               NCHAR = 8
               END IF
            MPNT = NPNT
            NCHAR = MIN (NBYT, NCHAR)
            IF (AWORD(TABNO).NE.'IMCLASS') IPOFF = ((NBYT+3)/4) * IPOFF
C                                       Start string on integer boundary
C                                       IMCLASS
            IF (AWORD(TABNO).EQ.'IMCLASS') THEN
               IPOFF = NBYT * IPOFF + 1
               KOUTCL = STR(1:NCHAR)
               GO TO 400
               END IF
C                                       Start string on real boundary.
            KOUTNM = STR(1:NCHAR)
            END IF
C                                       Add this unknown card to hist.
         INC = (ICARD - 1) * 80  +  1
         CARD = 'FITLD '
         KST = INC
         CLEN = 79
         IF (IST.EQ.9) THEN
            KST = INC + 8
            CLEN = CLEN - 8
            END IF
         CARD(7:) = FITBLK(KST:KST+CLEN)
         LST = 1
         IF (IHERR.EQ.0) CALL HIAD80 (IHLUN, LST, CARD, IHBLK, IHERR)
 400     CONTINUE
C                                       Read more cards than expected.
      GO TO 970
C                                       End card found.
C                                       Clean up antenna file stuff.
 40   IF (NOANT.EQ.0) GO TO 70
C                                       Close ant file, save last recrd
      CALL TABIO ('CLOS', 1, I4, SCRBUF, IABLK, JERR)
C                                       Add history info
 70   WRITE (HILINE,1070)
      IF (IHERR.EQ.0) CALL HIADD (IHLUN, HILINE, IHBLK, IHERR)
      WRITE (HILINE,1072) RLSNAM, ('----', I = 1,11)
      IF (IHERR.EQ.0) CALL HIADD (IHLUN, HILINE, IHBLK, IHERR)
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), LNAM)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), LCLAS)
      IF (IHERR.EQ.0) CALL HENCOO (TSKNAM, LNAM, LCLAS, CATBLK(KIIMS),
     *   FVOL(1), IHLUN, IHBLK, IHERR)
C                                       If DISK file give name.
      IF (INFILE.NE.' ') THEN
         I = ITRIM (INFILE)
         WRITE (HILINE,1074) TSKNAM, INFILE(:I)
         IF (IHERR.EQ.0) CALL HIADD (IHLUN, HILINE, IHBLK, IERR)
         END IF
      CALL HICLOS (IHLUN, UPDATE, IHBLK, I)
      IF (IHERR.EQ.0) GO TO 999
C                                       Error handling.
C                                       History file error.
      WRITE (MSGTXT,1950)
      GO TO 990
C                                       Tape read problem.
 960  WRITE (MSGTXT,1960)
      GO TO 990
C                                       Read more cards than we
C                                       expected.
 970  WRITE (MSGTXT,1970)
C
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('/',17A4)
 1002 FORMAT ('/Begin "HISTORY" information found in FITS tape ',
     *   'header by FITLD')
 1010 FORMAT ('ERROR ',I3,' OPENING HISTORY SCRATCH FILE')
 1020 FORMAT ('ERROR',I5,' ADDING KEYWORD TO HEADER FILE')
 1070 FORMAT ('/End FITS tape header "HISTORY" information')
 1072 FORMAT ('FITLD RELEASE= ''',A7,' '' /',11A4)
 1074 FORMAT (A6,'INFILE=''',A,'''')
 1075 FORMAT (F4.2)
 1076 FORMAT ('Correlator code version: ',F4.2)
 1950 FORMAT ('HISTORY FILE PROBLEM.')
 1960 FORMAT ('TAPE PROBLEM DURING HISTORY FILE READ.')
 1970 FORMAT ('READ MORE THAN 10**8 CARDS WITHOUT AN END CARD.')
      END
      SUBROUTINE BTRTAB (ICNO, EOF, IERR)
C-----------------------------------------------------------------------
C  This routine will read all fits extension files associated with UV
C  data and process the ones it recognizes (XTENSION = 'TABLE'
C  and 'A3DTABLE' (or temporary name '3D TABLE') as of now).
C  This version of the routine has been adapted to deal with the VLBA
C  interchange format.
C  INPUTS:
C     ICNO    I    Catalog number of the UV file.
C  OUTPUTS:
C     EOF     L    An end of file was read during processing.
C     IERR    I    Error code. 0=ok.
C                              12 = > GET3DT time error, exit
C                                     without destroying file
C                              13 => day number on incoming
C                                    file incorrect, exit
C                                    without destroying file.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MXTBKW, MAXHED
C
      INCLUDE 'INCS:POBV.INC'
C                                       MXTBKW=max. no. table keywords
      PARAMETER (MXTBKW = 200)
      PARAMETER (MAXHED = 7)
      CHARACTER KEYWRD(MXTBKW)*8, KEYCHR(MXTBKW)*8, TAB3D(3)*8,
     *   OBSDAT*8, COLHED(MAXHED)*24
      HOLLERITH KEYH(2)
      INTEGER   ICNO, IERR, IRET, LOGKOL(MAXHED), NUMBND
      LOGICAL   EOF, RSKIP
      DOUBLE PRECISION  NBITS, AXCNT, KEYVAL(MXTBKW), KEYD, JULIAN
      REAL      KEYR
      INTEGER   I, II, ICARD, ANLUN, INBLK, IKEY, ROWF, IVER, TABLUN,
     *   SRTORD, DATP(128,2), BUFFER(512), NUMKEY, KEYI, KEYTYP(MXTBKW),
     *   KEYLOC(MXTBKW), KEYV(2*MXTBKW), LENKEY(5), BUFFC(256), INKEY,
     *   CHNKEY, IFKEY, IEXTRA, KEYSAV(2*MXTBKW), REFKEY, IROUND, JT,
     *   JTRIM, PDATE
      LOGICAL   EXTEN, KEYL, DOHDR, ISOLD, TIMCON, REFORM
      INCLUDE 'FITLD.INC'
      INCLUDE 'ALLOW.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'DFLT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'DIGCOR.INC'
      EQUIVALENCE (KEYL, KEYI, KEYH, KEYR, KEYD)
      DATA TAB3D /'BINTABLE', 'A3DTABLE', '3D TABLE'/
      DATA ANLUN, TABLUN /28, 29/
      DATA COLHED /'TIME                    ',
     *             'TIME_INTERVAL           ',
     *             'SOURCE_ID               ',
     *             'ANTENNA_NO              ',
     *             'ARRAY                   ',
     *             'FREQID                  ',
     *             'TIMERANG                '/
C-----------------------------------------------------------------------
C                                       Tell user their data may be
C                                       flagged
      IF (THRESH.GT.0.0) THEN
         WRITE (MSGTXT,1060) THRESH
         CALL MSGWRT (2)
         END IF
C                                       Length of keyword values
      REFDAT = ' '
      LENKEY(1) = NWDPDP
      LENKEY(2) = 1
      LENKEY(3) = 2
      LENKEY(4) = 1
      LENKEY(5) = 1
      CALL HIOPEN (IHLUN, IHDISK, IHCNO, IHBLK, IERR)
      ANTLOD = USRANM(1).NE.' '
      ANTCHA = .FALSE.
      SRCLOD = .FALSE.
      SRCCHA = .FALSE.
      FRTLOD = .FALSE.
      FRTCHA = .FALSE.
      DOAMAP = USRANM(1).NE.' '
      DOFMAP = .FALSE.
      DOSMAP = .FALSE.
      DOPR   = .TRUE.
      ALLSKP = .FALSE.
      REREF  = .FALSE.
      GOTSUB = .FALSE.
      GOTFUB = .FALSE.
      MULINT = .FALSE.
      UNSORT = .FALSE.
      PRTPOL = .FALSE.
      OBSDAT = ' '
      CURJLD = 0.D0
      NUMACC = 0
      MXFREX = MAXFQI
      DO 10 I = 1,MAXFQI
         WRTFQ(I) = .FALSE.
         FQMAP(I) = I
         FQDUPS(I) = I
 10      CONTINUE
      TRANGE(1) = 1.E10
      TRANGE(2) = -TRANGE(1)
C                                       Loop for all FITS extensions.
      DO 200 I = 1,32000
         IF (EOF) GO TO 900
         NUMKEY = 0
         RSKIP = .FALSE.
C                                       Process all required FITS table
C                                       cards.
         IF (I.EQ.1) RSKIP = .TRUE.
         CALL BTREQ (FDVEC, TBIND, TAPBUF, FITBLK, ICARD, EXTEN,
     *      EOF, RSKIP, IERR)
         IF (EOF) GO TO 900
         IF ((IERR.NE.0) .OR. (.NOT.EXTEN)) GO TO 890
C                                       Calculate no. of data blocks.
         AXCNT = 1.0D0
         DO 20 II = 1,NAXIS
            AXCNT = AXCNT * NAXISI(II)
 20         CONTINUE
         AXCNT = AXCNT + PCOUNT
         NBITS = ABS (BITPIX) * GCOUNT * AXCNT
         INBLK = INT ((NBITS + 23039.0D0) / 23040.0D0)
         DOHDR = .FALSE.
C                                       See if we have a 3-D table.
C                                       If so this is the start of the
C                                       binary table section.
         IF ((EXTTYP.NE.TAB3D(1)) .AND. (EXTTYP.NE.TAB3D(2)) .AND.
     *      (EXTTYP.NE.TAB3D(3))) GO TO 100
C                                       initialize default values.
         CALL SETDEF
C                                       Process table file header.
         NUMKEY = MXTBKW
         CALL TABHDR (FDVEC, TBIND, ICARD, IHLUN, IHBLK, 1, NUMKEY,
     *      KEYWRD, KEYVAL, KEYCHR, KEYTYP, TAPBUF, FITBLK, IERR)
         IF (IERR.LT.0) GO TO 100
         IF (IERR.NE.0) GO TO 900
C                                       Check keyword types
         CALL IDIHDR (NUMKEY, KEYWRD, KEYVAL, KEYTYP, IERR)
         IF (IERR.NE.0) THEN
            GO TO 900
         END IF
C                                       Check for non time variable
C                                       tables
         TIMCON = .FALSE.
         IF ((ITYPE.EQ.'AG') .OR. (ITYPE.EQ.'FR') .OR. (ITYPE.EQ.'SO')
     *      .OR. (ITYPE.EQ.'GC')) TIMCON = .TRUE.
C                                       Normal table files. Special
C                                       processing if its an AIPS style
C                                       table.
         SRTORD = 0
         IF (ITYPE.NE.'UT') THEN
            IF (ITYPE.NE.'UK') CALL ATCONV (NUMKEY, KEYWRD, KEYVAL,
     *         KEYTYP, KEYCHR, SRTORD)
            END IF
C                                       Prepare keywords
         IKEY = 1
         REFKEY = -1
         CHNKEY = -1
         IFKEY = -1
         PDATE = 0
         DO 80 II = 1,NUMKEY
            KEYLOC(II) = IKEY
            IF (KEYTYP(II).EQ.2) KEYTYP(II) = 1
            IF (KEYTYP(II).EQ.1) KEYD = KEYVAL(II)
            IF (KEYTYP(II).EQ.3) THEN
               JT = JTRIM (KEYCHR(II))
               IF (KEYWRD(II).EQ.'RDATE') PDATE = IKEY
               CALL CHR2H (8, KEYCHR(II), 1, KEYH)
               END IF
            IF (KEYTYP(II).EQ.4) KEYI = KEYVAL(II)
            IF (KEYTYP(II).EQ.5) KEYL = KEYVAL(II).GT.0.0D0
            CALL COPY (LENKEY(KEYTYP(II)), KEYD, KEYV(IKEY))
            IF (KEYWRD(II)(1:8).EQ.'NO_CHAN ') CHNKEY = IKEY
            IF (KEYWRD(II)(1:8).EQ.'NO_BAND ') IFKEY = IKEY
            IF (KEYWRD(II)(1:8).EQ.'REF_PIXL') REFKEY = IKEY
            IKEY = IKEY + LENKEY(KEYTYP(II))
 80         CONTINUE
         CALL GLBKEY (NUMKEY, KEYWRD, KEYV, KEYLOC, KEYTYP)
C                                       Check the channel and IF
C                                       selection criteria
C                                       Channel selection
         BCHAN = IROUND (XBCHAN)
         ECHAN = IROUND (XECHAN)
         CHNSLT = ((BCHAN.NE.0) .AND. (ECHAN.NE.0)) .OR.
     *      ((BCHAN.GT.1) .AND. (ECHAN.EQ.0))
         IF (BCHAN.EQ.0) BCHAN = 1
C                                       IF selection
         BIF = IROUND (XBIF)
         EIF = IROUND (XEIF)
         IFSLT = ((BIF.NE.0) .AND. (EIF.NE.0)) .OR.
     *      ((BIF.GT.1) .AND. (EIF.EQ.0))
         IF (BIF.EQ.0) BIF = 1
         ORIGCH = NOCHAN
         ORIGIF = NOBAND
         IF (ECHAN.EQ.0) ECHAN = NOCHAN
         IF (EIF.EQ.0) EIF = NOBAND
         IF (CHNSLT) THEN
            IF ((BCHAN.LT.1) .OR. (ECHAN.LT.1) .OR. (BCHAN.GT.NOCHAN)
     *         .OR. (ECHAN.GT.NOCHAN) .OR. (BCHAN.GT.ECHAN)) THEN
               MSGTXT = 'CHANNEL SELECTION ERROR: NO CHANNEL' //
     *            ' SELECTION TO BE DONE'
               CALL MSGWRT (6)
               BCHAN = 1
               ECHAN = NOCHAN
               END IF
            SLCHNS = ECHAN - BCHAN + 1
            IF (SLCHNS.EQ.NOCHAN) CHNSLT = .FALSE.
            IF (CHNSLT) THEN
               WRITE (MSGTXT,1040) BCHAN, ECHAN, ORIGCH
               CALL MSGWRT (4)
               END IF
            END IF
         IF (IFSLT) THEN
            IF ((BIF.LT.1) .OR. (EIF.LT.1) .OR. (BIF.GT.NOBAND)
     *         .OR. (EIF.GT.NOBAND) .OR. (BIF.GT.EIF)) THEN
               MSGTXT = 'IF SELECTION ERROR: NO IF SELECTION TO BE'
     *            // ' DONE'
               CALL MSGWRT (6)
               BIF = 1
               EIF = NOBAND
               END IF
            SLIFS = EIF - BIF + 1
            IF (SLIFS.EQ.NOBAND) IFSLT = .FALSE.
            IF (IFSLT) THEN
               WRITE (MSGTXT,1050) BIF, EIF, NOBAND
               CALL MSGWRT (4)
               IF (NMRDUN.EQ.1) THEN
                  MSGTXT = 'WARNING: Data must be in increasing'
                  CALL MSGWRT (4)
                  MSGTXT = 'frequency order for IF selection'
                  CALL MSGWRT (4)
                  END IF
               END IF
            END IF
C                                       Update KEYvalue array saving
C                                       an unmodified copy. The saved
C                                       copy is needed for calibration
C                                       transfer to work correctly.
         CALL COPY (2 * MXTBKW, KEYV, KEYSAV)
         IF (CHNSLT) THEN
            NOCHAN = SLCHNS
            KEYI = NOCHAN
            CALL COPY (1, KEYD, KEYV(CHNKEY))
            REFPIX = REFPIX - BCHAN + 1.0
            KEYD = REFPIX
            CALL COPY (NWDPDP, KEYD, KEYV(REFKEY))
            END IF
         IF (IFSLT) THEN
            NOBAND = SLIFS
            KEYI = NOBAND
            CALL COPY (1, KEYD, KEYV(IFKEY))
            END IF
C                                       If first file then reset the
C                                       correlation id. arrays
         IF (I.EQ.1) NCQCOR = 0
C                                       Decide which file to attach
C                                       table to.
         CALL BTFND (I, DOFSEL, DISOUT, ICNO, IERR)
         IF (IERR.EQ.15) GO TO 999
         ISOLD = IERR.EQ.0
         IF (IERR.NE.0) THEN
C                                       Create if necessary.
            CALL BTFHDR (ICNO, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000)
               CALL MSGWRT (6)
               GO TO 999
               END IF
            LSTTIM = 0.0
            END IF
C                                       If file already exists then
C                                       set up initial antenna,
C                                       source and freqid mapping
C                                       functions.
         IF (ISOLD .AND. (I.EQ.1)) THEN
            CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
            CALL JULDAY (OBSDAT, CURJLD)
            ANTLOD = .FALSE.
C                                       turn off adverb
            JT = 0
            DO 85 II = 1,60
               IF (USRANM(II).NE.' ') JT = JT + 1
               USRANM(II) = ' '
 85            CONTINUE
            IF (JT.GT.0) THEN
               MSGTXT = 'ANTNAME IGNORED SINCE CONCATENATING WITH'
     *            // 'PRE-EXISTING FILE'
               CALL MSGWRT (7)
               END IF
            CALL MAPANT (DISOUT, ICNO, CATBLK, TABLUN, IERR)
            IF (IERR.NE.0) GO TO 900
            DOAMAP = .TRUE.
            CALL MAPFQI (DISOUT, ICNO, CATBLK, TABLUN, IERR)
            IF (IERR.NE.0) GO TO 900
            DOFMAP = .TRUE.
C                                       Read the correlation id.
C                                       parameters from the
C                                       existing CQ table if delay
C                                       decorrelation corrections
C                                       will be applied
            IF (DELCOR) THEN
               CALL MAPCQI (DISOUT, ICNO, CATBLK, TABLUN, CQBUFF,
     *            ICQCOR, ICQFLT, RCQINT, NCQCOR, IERR)
               IF (IERR.NE.0) THEN
                  DELCOR = .FALSE.
                  IERR = 0
                  MSGTXT = 'IF THIS IS VLBA DATA YOU HAVE A PROBLEM'
                  CALL MSGWRT (6)
                  END IF
               END IF
            END IF
C                                       map source numbers even on new
         IF (I.EQ.1) THEN
            CALL MAPSRC (DISOUT, ICNO, CATBLK, TABLUN, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         DOSMAP = .TRUE.
C                                       Fill in the observing date from
C                                       the table reference date unless
C                                       we already have something better
         IF ((OBSDAT.EQ.' ') .AND. (REFDAY.NE.' ')) THEN
            OBSDAT = REFDAY
            CALL JULDAY (OBSDAT, CURJLD)
            CALL CHR2H (8, OBSDAT, 1, CATH(KHDOB))
            END IF
         IF ((OBSDAT.EQ.' ') .AND. (REFDAT.NE.' ')) THEN
            OBSDAT = REFDAT
            CALL JULDAY (OBSDAT, CURJLD)
            CALL CHR2H (8, OBSDAT, 1, CATH(KHDOB))
            END IF
C                                       Determine day offset for time
C                                       variable tables if necessary.
         OFFDAY = 0.0D0
         IF ((ISOLD) .AND. (.NOT.TIMCON) .AND. (REFDAT.NE.' ')) THEN
            CALL JULDAY (REFDAT, JULIAN)
            IF ((CURJLD.GT.0.D0) .AND. (JULIAN.GT.0.D0)) THEN
               OFFDAY = JULIAN - CURJLD
               IF (OFFDAY.LT.-0.01) THEN
                  WRITE (MSGTXT,1001) -OFFDAY
                  CALL MSGWRT (6)
                  END IF
               END IF
            END IF
C                                       Normal processing for
C                                       not the UV binary tables
         IF (ITYPE.NE.'UT') THEN
C                                       Create and initialize the table
C                                       header with data in common.
            IVER = EXTVER
            IF (TIMCON) IVER = 0
C                                       Maybe will have to reformat
C                                       table, if so create new version
C                                       don't add to existing table yet.
C                                       Only IM and OB tables need
C                                       reformatting at present time
C                                       and under some circumstances.
C                                       This list will grow.
            REFORM = .FALSE.
            IF (((ITYPE.EQ.'IM') .AND. (TABREV.LT.CIMVER)) .OR.
     *         ((ITYPE.EQ.'OB') .AND. (TABREV.LT.IOBREV)) .OR.
     *         (ITYPE.EQ.'CT')) THEN
               REFORM = .TRUE.
               IVER = 0
               END IF
C                                       If reformatting necessary use
C                                       scratch file as a temporary
C                                       storage space, do the
C                                       reformatting there then copy the
C                                       reformatted table to the master
C                                       file.
            INKEY = MAXHED
            IF (REFORM) THEN
               CALL CRETAB (SRTORD, IHDISK, IHCNO, IVER, CATHIS, TABLUN,
     *            DATP, INKEY, COLHED, LOGKOL, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 110
C                                       Update CATBLK
               CALL CATIO ('UPDT', IHDISK, IHCNO, CATHIS, 'REST',
     *            BUFFC, IERR)
            ELSE
               CALL CRETAB (SRTORD, DISOUT, ICNO, IVER, CATBLK, TABLUN,
     *            DATP, INKEY, COLHED, LOGKOL, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 110
C                                       Update CATBLK
               CALL CATIO ('UPDT', DISOUT, ICNO, CATBLK, 'REST',
     *            BUFFC, IERR)
               CALL COPY (256, CATBLK, ALLCAT(1,CURALL))
               END IF
C                                       Write keywords
            IF ((BUFFER(5).EQ.0) .OR. (TIMCON)) THEN
               CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER, KEYLOC,
     *            KEYSAV, KEYTYP, IERR)
               IF (IERR.NE.0) GO TO 110
               END IF
C                                       forced RDATE??
            IF ((PDATE.GT.0) .AND. (REFDAY.NE.' ')) THEN
               CALL CHR2H (8, REFDAY, 1, KEYH)
               CALL TABKEY ('WRIT', 'RDATE   ', 1, BUFFER, 1,
     *            KEYH, 3, IERR)
               IF (IERR.NE.0) GO TO 110
               END IF
C                                       Read the data from tape and
C                                       write to the table disk file.
            CALL RDVTAB (FDVEC, TBIND, DATP, NAXISI, BUFFER, TAPBUF,
     *         OFFDAY, LOGKOL, ROWF, BIF, IERR)
            EOF = EOF .OR. (IERR.EQ.4)
            IF (IERR.NE.0) GO TO 900
C
C                                       Reformat & copy to master file
            IF (REFORM) THEN
               CALL TABREF (ITYPE, IVER, EXTVER, IHDISK, IHCNO,
     *            CATHIS, ANLUN, ROWF, DISOUT, ICNO, CATBLK, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1010) ITYPE
                  CALL MSGWRT (6)
                  GO TO 999
                  END IF
C                                       Update CATBLK
               CALL CATIO ('UPDT', DISOUT, ICNO, CATBLK, 'REST',
     *            BUFFC, IERR)
               CALL COPY (256, CATBLK, ALLCAT(1,CURALL))
               END IF
C                                       Record start row for appending
C                                       IM data
            IF (ITYPE.EQ.'IM') IMROST = ROWF
C                                       Compare non time variable
C                                       tables
            IF ((TIMCON) .AND. (IVER.NE.EXTVER)) THEN
               CALL TABCOM (ITYPE, DISOUT, ICNO, IVER, EXTVER, CATBLK,
     *            TABLUN, ANLUN, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
C                                       Set up frequency ordering
C                                       table
            IF (ITYPE.EQ.'FR') THEN
               CALL FRQORD (DISOUT, ICNO, IVER, CATBLK, TABLUN, IERR)
               IF (IERR.NE.0) GO TO 900
               IF (DOFMAP) THEN
                  CALL MAPFQI (DISOUT, ICNO, CATBLK, TABLUN, IERR)
                  IF (IERR.NE.0) GO TO 900
                  END IF
               END IF
C                                       Set up antenna mapping
C                                       function
            IF (DOAMAP .AND. (ITYPE.EQ.'AG')) THEN
               CALL MAPANT (DISOUT, ICNO, CATBLK, TABLUN, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
C                                       Set up source mapping
C                                       function
            IF (DOSMAP .AND. (ITYPE.EQ.'SO')) THEN
               CALL MAPSRC (DISOUT, ICNO, CATBLK, TABLUN, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
            IF (ITYPE.EQ.'SO') THEN
               CALL SOUSEL (ICNO, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
C                                       Extract keywords for digital
C                                       corrections from MC table
            IF (ITYPE.EQ.'MC')
     *         CALL MCKEY (NUMKEY, KEYWRD, KEYV, KEYLOC)
C                                       Extract the no levels of
C                                       digitization for the AT table,
C                                       may be time variable.
            IF (ITYPE.EQ.'AT') THEN
               NUMBND = EIF - BIF + 1
               CALL DIGLEV (DISOUT, ICNO, TABLUN, NUMBND, CATBLK, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
C                                       Special processing for the
C                                       UV binary table.
         ELSE IF (ITYPE.EQ.'UT') THEN
            CALL UT2UV (NAXISI, DISOUT, ICNO, ISOLD, EOF, IERR)
            IF (IERR.NE.0) GO TO 900
            IF (MAJERR.NE.0) GO TO 900
            END IF
         GO TO 200
C                                       Skip unknown extension file.
 100     CONTINUE
C                                       read rest header code
         DOHDR = .TRUE.
C                                       else header already read
 110     CALL SKPEXT (DOHDR, FDVEC, TBIND, IHLUN, ICARD, INBLK, IHBLK,
     *      TAPBUF, FITBLK, IERR)
         IF (IERR.NE.0) GO TO 900
 200     CONTINUE
C                                       Shouldn't get here.
      WRITE (MSGTXT,1200)
      CALL MSGWRT (8)
C                                       Find EOF
 890  IEXTRA = 0
      DO 895 I = 1,200000
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.EQ.4) THEN
            EOF = .TRUE.
            IERR = 0
            GO TO 900
            END IF
         IEXTRA = IEXTRA + 1
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1890) IERR
            CALL MSGWRT (8)
            IERR = 0
            GO TO 900
            END IF
 895     CONTINUE
C
 900  CALL HICLOS (IHLUN, .TRUE., IHBLK, IRET)
C                                       update the CT table
      CALL CTUPDT (DISOUT, ICNO, TABLUN, TRANGE, CATBLK, BUFFER, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BTRTAB: COULD NOT FIND OR CREATE UV FILE FOR TABLES')
 1001 FORMAT ('WARNING: found a reference date ',F9.1,
     *   ' days before DATE-OBS')
 1010 FORMAT ('BTRTAB: UNABLE TO REFORMAT ',A2,' TABLE')
 1040 FORMAT ('Selecting spectral channels ',I4,'->',I4,' from ',I4)
 1050 FORMAT ('Selecting IFs ',I4,'->',I4,' from ',I4)
 1060 FORMAT ('Data with weight < ',F5.2,' will be permanently flagged')
 1200 FORMAT ('MORE THAN 32000 EXTENSION FILES. SOME NOT PROCESSED.')
 1890 FORMAT ('BTRTAB: ERROR ',I3,' ADVANCING TO EOF')
      END
      SUBROUTINE BT2AIP (ICNO, IERR)
C-----------------------------------------------------------------------
C  This is the main routine for the translation of the VLBA binary
C  tables to the standard AIPS tables. It will read the VLBA tables,
C  create the AIPS tables, translate the relevant quantities then
C  destroy the VLBA tables on disc.
C
C  INPUTS:
C     ICNO       I      Catalogue number of the UV file
C
C  OUTPUTS:
C     IERR       I      Error code, 0 => OK
C-----------------------------------------------------------------------
      INTEGER ICNO, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER SORT*2
      INTEGER   IVER, LUN, KBUFF(2048), VBVER, LUN2, KBUFFA(512), NUMBND
      LOGICAL   TABLE, EXIST, FITASC, EXIST2, ATEXIS
      INCLUDE 'FITLD.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'DIGCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN, LUN2 /40, 41/
C-----------------------------------------------------------------------
C                                       Change sort order if multiple
C                                       dump rates.
      IF (UNSORT) THEN
         SORT = '**'
         CALL CHR2H (2, SORT, 1, CATH(KITYP))
         MSGTXT = 'WARNING: data found out of time order'
         CALL MSGWRT (5)
         CALL CATIO ('UPDT', DISOUT, ICNO, CATBLK, 'REST', KBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1120) IERR
            CALL MSGWRT (6)
            END IF
         END IF
C                                        Copy FR table to FQ table
      IVER = 1
      CALL ISTAB ('FR', DISOUT, ICNO, IVER, LUN, KBUFF, TABLE, EXIST,
     *   FITASC, IERR)
      IF (EXIST) THEN
         CALL FR2FQ (DISOUT, ICNO, LUN, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
         END IF
      IERR = 0
C                                       Check that array geometry
C                                       and antenna characteristics
C                                       tables are present.
      IVER = 1
      CALL ISTAB ('AG', DISOUT, ICNO, IVER, LUN, KBUFF, TABLE, EXIST,
     *   FITASC, IERR)
      IF (.NOT.EXIST) THEN
         WRITE (MSGTXT,1000)
         IERR = 1
         GO TO 990
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, 'AG'
         GO TO 990
         END IF
C                                       Check for AT tables - not a
C                                       fatal error if missing
      CALL ISTAB ('AT', DISOUT, ICNO, IVER, LUN, KBUFF, TABLE, ATEXIS,
     *   FITASC, IERR)
      IF (.NOT.ATEXIS) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (5)
         WRITE (MSGTXT, 1021)
         CALL MSGWRT (5)
         IERR = 0
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, 'AT'
         GO TO 990
         END IF
C                                        Antenna table translation
      CALL AG2AN (DISOUT, ICNO, LUN, CATBLK, REFDAY, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
C                                        Load antenna characteristics
C                                        into antenna table
      NUMBND  = EIF - BIF + 1
      IF (ATEXIS) THEN
         CALL AT2AN (DISOUT, ICNO, LUN, NUMBND, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR
            GO TO 990
            END IF
         END IF
C                                        Copy SO table to SU table, if
C                                        it exists
      IVER = 1
      CALL ISTAB ('SO', DISOUT, ICNO, IVER, LUN, KBUFF, TABLE, EXIST,
     *   FITASC, IERR)
      IF (EXIST) THEN
         CALL SO2SU (DISOUT, ICNO, LUN, BIF, EIF, OFFDAY, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1060) IERR
            GO TO 990
            END IF
         END IF
      IERR = 0
C                                        Copy IM table to CL table
      IVER = 1
      CALL ISTAB ('IM', DISOUT, ICNO, IVER, LUN, KBUFF, TABLE, EXIST,
     *   FITASC, IERR)
      IVER = 1
      CALL ISTAB ('CL', DISOUT, ICNO, IVER, LUN, KBUFF, TABLE,
     *   EXIST2, FITASC, IERR)
      IF (EXIST .AND. EXIST2) THEN
         CALL IM2CL (DISOUT, ICNO, LUN, NUMBND, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1070) IERR
            GO TO 990
            END IF
         END IF
      IERR = 0
C                                        Copy MC table to CL table
      IVER = 1
      CALL ISTAB ('MC', DISOUT, ICNO, IVER, LUN, KBUFF, TABLE, EXIST,
     *   FITASC, IERR)
      IF ((EXIST) .AND. (RFQDUP)) CALL DFQTA ('MC', DISOUT, ICNO,
     *   FQDUPS, CATBLK, IERR)
      IVER = 1
      CALL ISTAB ('CL', DISOUT, ICNO, IVER, LUN, KBUFF, TABLE,
     *   EXIST2, FITASC, IERR)
      IF (EXIST .AND. EXIST2) THEN
         CALL MC2CL (DISOUT, ICNO, LUN, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1070) IERR
            GO TO 990
            END IF
         END IF
      IERR = 0
C                                        Copy BA table to BP table
      IVER = 1
      CALL ISTAB ('BT', DISOUT, ICNO, IVER, LUN, KBUFF, TABLE, EXIST,
     *   FITASC, IERR)
      IF (EXIST) THEN
         CALL BT2BP (DISOUT, ICNO, LUN, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
         END IF
      IERR = 0
C                                        Copy BC table to BL table
      IVER = 1
      CALL ISTAB ('BC', DISOUT, ICNO, IVER, LUN, KBUFF, TABLE, EXIST,
     *   FITASC, IERR)
      IF (EXIST) THEN
         CALL BC2BL (DISOUT, ICNO, LUN, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1110) IERR
            GO TO 990
            END IF
         END IF
      IERR = 0
C                                       Generate CQ table
      IF (DELCOR) THEN
         CALL CQADD (DISOUT, ICNO, CATBLK, KBUFF, KBUFFA, LUN, LUN2,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1240) IERR
            CALL MSGWRT (8)
            DELCOR = .FALSE.
            IERR = 0
            END IF
         END IF
C                                       Convert flag tables
      IF (IERR.EQ.0) THEN
         CALL FL2FG (DISOUT, ICNO, BIF, EIF, BCHAN, ECHAN, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 9000) IERR, 'FLAG'
            CALL MSGWRT (8)
            IERR = 0
            END IF
         END IF
C                                       Convert gain curve tables
      IF (IERR.EQ.0) THEN
         CALL GN2GC (DISOUT, ICNO, BIF, EIF, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 9000) IERR, 'GAIN_CURVE'
            CALL MSGWRT (8)
            IERR = 0
            END IF
         END IF
C                                       Convert phase-cal tables
      IF (IERR.EQ.0) THEN
         CALL PH2PC (DISOUT, ICNO, BIF, EIF, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 9000) IERR, 'PHASE-CAL'
            CALL MSGWRT (8)
            IERR = 0
            END IF
         END IF
C                                       Convert system temperature
      IF (IERR.EQ.0) THEN
         CALL TS2TY (DISOUT, ICNO, BIF, EIF, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 9000) IERR, 'SYSTEM_TEMPERATURE'
            CALL MSGWRT (8)
            IERR = 0
            END IF
         END IF
C                                       Convert weather tables
      IF (IERR.EQ.0) THEN
         CALL WR2WX (DISOUT, ICNO, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,9000) IERR, 'WEATHER'
            CALL MSGWRT (8)
            IERR = 0
            END IF
         END IF
C
C     Take a short cut to the end if errors have been detected
C     (temporary until the rest of BT2AIP has been overhauled - CF):
C
      IF (IERR.NE.0) GO TO 999
C                                       Remove them all
      VBVER = 0
      IF (DELEX) THEN
         CALL RMEXT (DISOUT, ICNO, 'AG', VBVER, CATBLK, KBUFF, IERR)
         CALL RMEXT (DISOUT, ICNO, 'FR', VBVER, CATBLK, KBUFF, IERR)
         CALL RMEXT (DISOUT, ICNO, 'SO', VBVER, CATBLK, KBUFF, IERR)
         CALL RMEXT (DISOUT, ICNO, 'BT', VBVER, CATBLK, KBUFF, IERR)
         CALL RMEXT (DISOUT, ICNO, 'BC', VBVER, CATBLK, KBUFF, IERR)
         CALL RMEXT (DISOUT, ICNO, 'CA', VBVER, CATBLK, KBUFF, IERR)
         CALL RMEXT (DISOUT, ICNO, 'UT', VBVER, CATBLK, KBUFF, IERR)
         END IF
      IF (DELVT) CALL RMEXT (DISOUT, ICNO, 'VT', VBVER, CATBLK,
     *   KBUFF, IERR)
C                                       Special case of subarrays
      IF (GOTSUB .OR. GOTFUB) THEN
         CALL RMEXT (DISOUT, ICNO, 'NX', VBVER, CATBLK, KBUFF, IERR)
         CALL RMEXT (DISOUT, ICNO, 'CL', VBVER, CATBLK, KBUFF, IERR)
         MSGTXT = '**************************************************'
         CALL MSGWRT (6)
         IF (GOTSUB .OR. GOTFUB) THEN
            MSGTXT = ' Subarray or data out of order condition found.'
            CALL MSGWRT (6)
            MSGTXT = ' NX/CL tables deleted.'
            CALL MSGWRT (6)
            IF (UNSORT) THEN
               MSGTXT = ' Use UVSRT or MSORT to sort data into TB '
     *            // 'order.'
               CALL MSGWRT (6)
               END IF
            MSGTXT = ' Use USUBA to set up subarrays.'
            CALL MSGWRT (6)
            MSGTXT = ' Rerun INDXR using CPARM(3) and (4)'
            CALL MSGWRT (6)
            END IF
         MSGTXT = '**************************************************'
         CALL MSGWRT (6)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BT2AIP: ARRAY GEOMETRY TABLE NOT PRESENT - ILLEGAL FILE')
 1010 FORMAT ('BT2AIP: ERROR ',I3,' FINDING ',A,' TABLE')
 1020 FORMAT ('WARNING: antenna characteristics table not present.')
 1021 FORMAT ('WARNING: antenna polarizations will not be labelled ',
     *        'correctly.')
 1030 FORMAT ('BT2AIP: ERROR ',I3,' FORMING ANTENNA TABLE')
 1040 FORMAT ('BT2AIP: ERROR ',I3,' LOADING ANTENNA TABLE')
 1050 FORMAT ('BT2AIP: ERROR ',I3,' FORMING FREQUENCY TABLE')
 1060 FORMAT ('BT2AIP: ERROR ',I3,' FORMING SOURCE TABLE')
 1070 FORMAT ('BT2AIP: ERROR ',I3,' FORMING CL TABLE FROM CA TABLE')
 1100 FORMAT ('BT2AIP: ERROR ',I3,' FORMING BP TABLE FROM BA TABLE')
 1110 FORMAT ('BT2AIP: ERROR ',I3,' FORMING BL TABLE FROM BC TABLE')
 1120 FORMAT ('BT2AIP: ERROR ',I3,' UPDATING SORT ORDER')
 1240 FORMAT ('BT2AIP: ERROR ',I3,' GENERATING CQ TABLE')
 9000 FORMAT ('BT2AIP: ERROR ', I3, ' CONVERTING ', A, ' TABLES')
      END
      SUBROUTINE DFQTA (TYPE, VOL, CNO, FQDUPS, CATBLK, IRET)
C-----------------------------------------------------------------------
C   DFQTA re-writes tables correcting the FQID numbers if any.
C   Inputs:
C      VOL      I        Disk number
C      CNO      I        Catalog slot number
C      FQDUPS   I(*)     New FQ = FQDUPS (old FQ)
C   Output:
C      IRET     I        Return error code  0 => ok
C                           1 => files the same, no copy.
C                           2 => no input files exist
C                           3 => failed
C                           4 => no output files created.
C                           5 => failed to update CATBLK
C-----------------------------------------------------------------------
      INTEGER   VOL, CNO, FQDUPS(*), CATBLK(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER FIDKEY(3)*24, TYPE*2
      REAL      RECR(XBPRSZ)
      DOUBLE PRECISION RECD(XBPRSZ/2)
      INTEGER   NVERS, NKEY, NREC, NCOL, DATP(128,2,2), IER, FQKOL,
     *   RECORD(XBPRSZ), FQID, NRECIN, INREC, OUTREC, LOOP, INVER,
     *   OUTVER, LUN1, LUN2, LUNTMP, BUFF1(512), BUFF2(512), FQIFNC
      LOGICAL   T, F, TABLE, EXIST, FITASC, OPEN1, OPEN2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RECORD, RECR, RECD)
      DATA FIDKEY /'FQID ', 'FREQID  ', 'FREQ ID '/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      CALL FNDEXT (TYPE, CATBLK, NVERS)
      DO 90 INVER = 1,NVERS
         OPEN1 = F
         OPEN2 = F
         EXIST = .FALSE.
         IRET = 0
         LUN1 = LUNTMP (1)
         IF ((TYPE.NE.'HI') .AND. (TYPE.NE.'PL') .AND. (TYPE.NE.'SL')
     *      .AND. (TYPE.NE.' ') .AND. (TYPE.NE.'FQ')) CALL ISTAB
     *      (TYPE, VOL, CNO, INVER, LUN1, BUFF1, TABLE, EXIST, FITASC,
     *      IRET)
         IF ((IRET.EQ.0) .AND. (EXIST) .AND. (TABLE)) THEN
C                                       Open first (input) file
            CALL TABINI ('READ', TYPE, VOL, CNO, INVER, CATBLK, LUN1,
     *         NKEY, NREC, NCOL, DATP(1,1,1), BUFF1, IER)
C                                       If not there - quit
            IF (IER.GT.0) GO TO 90
            OPEN1 = T
C                                       Find source id column.
            CALL FNDCOL (1, FIDKEY(1), 5, T, BUFF1, FQKOL, IER)
            IF (IER.NE.0) CALL FNDCOL (1, FIDKEY(2), 7, T, BUFF1,
     *         FQKOL, IER)
            IF (IER.NE.0) CALL FNDCOL (1, FIDKEY(3), 8, T, BUFF1,
     *         FQKOL, IER)
C                                       quit if no source column
            IF (IER.EQ.0) THEN
C                                       Open second (output) file
               OUTVER = INVER
               LUN2 = LUNTMP (1)
               CALL TABINI ('WRIT', TYPE, VOL, CNO, OUTVER, CATBLK,
     *            LUN2, NKEY, NREC, NCOL, DATP(1,1,2), BUFF2, IER)
               IF (IER.GT.0) GO TO 80
               OPEN2 = T
C                                       Get number of records in the
C                                       files.
               NRECIN = BUFF1(5)
               OUTREC = 1
C                                       Mark unsorted
               IF ((ABS(BUFF2(43)).EQ.FQKOL) .OR.
     *            (ABS(BUFF2(44)).EQ.FQKOL)) THEN
                  BUFF2(43) = 0
                  BUFF2(44) = 0
                  END IF
C                                       translate to physical plaice in
C                                       record
               FQKOL = DATP(FQKOL,1,1)
C                                       Copy
               DO 40 LOOP = 1,NRECIN
                  INREC = LOOP
                  CALL TABIO ('READ', 0, INREC, RECORD, BUFF1, IER)
                  IF (IER.GT.0) THEN
                     WRITE (MSGTXT,1070) IER, 'READ', TYPE
                     GO TO 990
                     END IF
C                                       Source translate
                  FQID = RECORD(FQKOL)
                  IF (FQID.GT.0) THEN
                     FQID = FQDUPS(FQID)
                     FQID = FQIFNC (FQID)
                     RECORD(FQKOL) = FQID
                     END IF
                  OUTREC = LOOP
                  CALL TABIO ('WRIT', 0, OUTREC, RECORD, BUFF2, IER)
                  IF (IER.GT.0) THEN
                     WRITE (MSGTXT,1070) IER, 'WRIT', TYPE
                     GO TO 990
                     END IF
 40               CONTINUE
               END IF
C                                       Close tables.
 80         IF (OPEN1) THEN
               CALL TABIO ('CLOS', 0, INREC, RECORD, BUFF1, IER)
               IF (IER.GT.0) THEN
                  WRITE (MSGTXT,1070) IER, 'CLOS', TYPE
                  GO TO 990
                  END IF
               END IF
            OPEN1 = F
            IF (OPEN2) THEN
               CALL TABIO ('CLOS', 0, OUTREC, RECORD, BUFF2, IER)
               IF (IER.GT.0) THEN
                  WRITE (MSGTXT,1070) IER, 'CLOS', TYPE
                  GO TO 990
                  END IF
               END IF
            END IF
 90      CONTINUE
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
      IF (OPEN1) CALL TABIO ('CLOS', 0, INREC, RECORD, BUFF1, IER)
      IF (OPEN2) CALL TABIO ('CLOS', 0, OUTREC, RECORD, BUFF2, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('DFQTA: ERROR ',I3,2X,A4,'ING ',A2,' TABLE')
      END
      SUBROUTINE UNFQTA (TYPE, VOL, CNO, FQDUPS, CATBLK, IRET)
C-----------------------------------------------------------------------
C   UNFQTA re-writes tables dropping unused FQ numbers
C   Inputs:
C      VOL      I        Disk number
C      CNO      I        Catalog slot number
C      FQDUPS   I(*)     New FQ = FQDUPS (old FQ)
C   Output:
C      IRET     I        Return error code  0 => ok
C                           1 => files the same, no copy.
C                           2 => no input files exist
C                           3 => failed
C                           4 => no output files created.
C                           5 => failed to update CATBLK
C-----------------------------------------------------------------------
      INTEGER   VOL, CNO, FQDUPS(*), CATBLK(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER FIDKEY(3)*24, TYPE*2
      REAL      RECR(XBPRSZ)
      DOUBLE PRECISION RECD(XBPRSZ/2)
      INTEGER   NVERS, NKEY, NREC, NCOL, DATP(128,2,2), IER, FQKOL,
     *   RECORD(XBPRSZ), FQID, NRECIN, INREC, OUTREC, LOOP, INVER,
     *   OUTVER, LUN1, LUN2, LUNTMP, BUFF1(512), BUFF2(512), FQIFNC
      LOGICAL   T, F, TABLE, EXIST, FITASC, OPEN1, OPEN2, DOIT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RECORD, RECR, RECD)
      DATA FIDKEY /'FQID ', 'FREQID  ', 'FREQ ID '/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      CALL FNDEXT (TYPE, CATBLK, NVERS)
      DO 90 INVER = 1,NVERS
         OPEN1 = F
         OPEN2 = F
         EXIST = .FALSE.
         IRET = 0
         LUN1 = LUNTMP (1)
         IF ((TYPE.NE.'HI') .AND. (TYPE.NE.'PL') .AND. (TYPE.NE.'SL')
     *      .AND. (TYPE.NE.' ') .AND. (TYPE.NE.'FQ')) CALL ISTAB
     *      (TYPE, VOL, CNO, INVER,LUN1, BUFF1, TABLE, EXIST, FITASC,
     *      IRET)
         IF ((IRET.EQ.0) .AND. (EXIST) .AND. (TABLE)) THEN
C                                       Open first (input) file
            CALL TABINI ('READ', TYPE, VOL, CNO, INVER, CATBLK, LUN1,
     *         NKEY, NREC, NCOL, DATP(1,1,1), BUFF1, IER)
C                                       If not there - quit
            IF (IER.GT.0) GO TO 90
            OPEN1 = T
C                                       Find source id column.
            CALL FNDCOL (1, FIDKEY(1), 5, T, BUFF1, FQKOL, IER)
            IF (IER.NE.0) CALL FNDCOL (1, FIDKEY(2), 7, T, BUFF1,
     *         FQKOL, IER)
            IF (IER.NE.0) CALL FNDCOL (1, FIDKEY(3), 8, T, BUFF1,
     *         FQKOL, IER)
C                                       quit if no source column
            IF (IER.EQ.0) THEN
C                                       Open second (output) file
               OUTVER = INVER
               LUN2 = LUNTMP (1)
               CALL TABINI ('WRIT', TYPE, VOL, CNO, OUTVER, CATBLK,
     *            LUN2, NKEY, NREC, NCOL, DATP(1,1,2), BUFF2, IER)
               IF (IER.GT.0) GO TO 80
               OPEN2 = T
C                                       Get number of records in the
C                                       files.
               NRECIN = BUFF1(5)
               OUTREC = 1
C                                       Mark unsorted
               IF ((ABS(BUFF2(43)).EQ.FQKOL) .OR.
     *            (ABS(BUFF2(44)).EQ.FQKOL)) THEN
                  BUFF2(43) = 0
                  BUFF2(44) = 0
                  END IF
C                                       translate to physical plaice in
C                                       record
               FQKOL = DATP(FQKOL,1,1)
C                                       Copy
               DO 40 LOOP = 1,NRECIN
                  INREC = LOOP
                  CALL TABIO ('READ', 0, INREC, RECORD, BUFF1, IER)
                  IF (IER.GT.0) THEN
                     WRITE (MSGTXT,1070) IER, 'READ', TYPE
                     GO TO 990
                     END IF
C                                       Source translate
                  FQID = RECORD(FQKOL)
                  DOIT = FQID.LE.0
                  IF (FQID.GT.0) THEN
                     IF (FQDUPS(FQID).GE.FQID) THEN
                        FQID = FQDUPS(FQID)
                        FQID = FQIFNC (FQID)
                        RECORD(FQKOL) = FQID
                        DOIT = .TRUE.
                        END IF
                     END IF
                  IF (DOIT) THEN
                     CALL TABIO ('WRIT', 0, OUTREC, RECORD, BUFF2, IER)
                     IF (IER.GT.0) THEN
                        WRITE (MSGTXT,1070) IER, 'WRIT', TYPE
                        GO TO 990
                        END IF
                     OUTREC = OUTREC + 1
                     END IF
 40               CONTINUE
               END IF
C                                       Close tables.
 80         IF (OPEN1) THEN
               CALL TABIO ('CLOS', 0, INREC, RECORD, BUFF1, IER)
               IF (IER.GT.0) THEN
                  WRITE (MSGTXT,1070) IER, 'CLOS', TYPE
                  GO TO 990
                  END IF
               END IF
            OPEN1 = F
            IF (OPEN2) THEN
               BUFF2(5) = OUTREC - 1
               CALL TABIO ('CLOS', 0, OUTREC, RECORD, BUFF2, IER)
               IF (IER.GT.0) THEN
                  WRITE (MSGTXT,1070) IER, 'CLOS', TYPE
                  GO TO 990
                  END IF
               END IF
            END IF
 90      CONTINUE
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
      IF (OPEN1) CALL TABIO ('CLOS', 0, INREC, RECORD, BUFF1, IER)
      IF (OPEN2) CALL TABIO ('CLOS', 0, OUTREC, RECORD, BUFF2, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('UNFQTA: ERROR ',I3,2X,A4,'ING ',A2,' TABLE')
      END
      SUBROUTINE BTFHDR (ISLOT, IERR)
C-----------------------------------------------------------------------
C   BTFHDR builds a (mostly virgin) catalog header and sets up some
C   pointers. It is intended for the new VLBA interchange format
C   binary tables. In this format the associated tables come first
C   followed by the UV data, so we don't know what the contents of
C   the header will be until later.
C   BTFHDR also creates the output file.
C   Output:
C      ISLOT  I     Catalog slot number for new UV file.
C      ERR   I        =0 => ok
C                     other => quit
C-----------------------------------------------------------------------
      INTEGER   ISLOT, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'ALLOW.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
C
      CHARACTER MULTI*8, RTYPES(13)*8, TYPES(7)*8, UNITS*8, TELE*8,
     *   INSTR*8, DEFNAM*12, DEFCLS*6, LOCNAM*12, LOCCLS*6,
     *   BLANK*8
      INTEGER   I, NAXIS, NRAN, NDIM(7), INDEX, IWORK(512), ITMP, IT1,
     *  IT2, IT3
      REAL      CRPIX(7), CRINC(7), EPOCH
      DOUBLE PRECISION CRVAL(7), OLDRFQ
C
      DATA MULTI /'MULTI '/
C                                         No. random parameters.
      DATA NRAN /11/
C                                         Rand. parm. names.
      DATA RTYPES /'UU-L-SIN','VV-L-SIN','WW-L-SIN',
     *   'SUBARRAY','TIME1   ','SOURCE  ','FREQSEL ',
     *   'INTTIM', 'ANTENNA1', 'ANTENNA2', 'CORR-ID ',
     *   'WEIGHT  ','SCALE   '/
C                                       Uniform axes.
C                                         No. axes.
      DATA NAXIS /6/
C                                         Axes names.
      DATA TYPES /'COMPLEX ','STOKES  ','FREQ    ',
     *   'IF      ','RA      ','DEC     ','        '/
C                                         Axis dimensions
      DATA NDIM /3,0,0,0,1,1,1/
C                                         Reference values
      DATA CRVAL /1.0D0, 0.0D0, 0.5D0, 1.0D0, 3*0.0D0/
C                                         Reference pixel.
      DATA CRPIX /7*1.0/
C                                         Coordinate increment.
      DATA CRINC /1.0, -1.0, 5*1.0/
C                                       Epoch of position.
      DATA EPOCH /2000.0/
C                                       Units
      DATA UNITS /'UNCALIB '/
C                                       Telescope/instrument
      DATA TELE, INSTR /'VLBA    ','VLBA    '/
      DATA BLANK /'        '/
C-----------------------------------------------------------------------
C                                       Zero fill CATBLK
      CALL CATINI (CATBLK)
C                                       Fill axis arrays.
C                                       Random axis names
C                                       Compressed output data?
      IF (DOUVCM) THEN
         NRAN = 10
         MSGTXT = 'UV data will be written in compressed format'
         CALL MSGWRT (4)
         END IF
      KLOCWT = NRAN - 2
      DO 10 I = 1,NRAN
         INDEX = KHPTP + (I-1) * 2
         CALL CHR2H (8, RTYPES(I), 1, CATH(INDEX))
 10      CONTINUE
C                                       Uniform axes
      DO 30 I = 1,KICTPN
C                                       Init dimension
         CATBLK(KINAX+I-1) = NDIM(I)
C                                       Init. increment.
         CATR(KRCIC+I-1) = CRINC(I)
C                                       Init. rotation.
         CATR(KRCRT+I-1) = 0.0
C                                       Init. ref pixel.
         CATR(KRCRP+I-1) = CRPIX(I)
C                                       Init. ref value.
         CATD(KDCRV+I-1) = CRVAL(I)
C                                       Fill axis type from
C                                       TYPES
         INDEX = KHCTP + (I-1) * 2
         CALL CHR2H (8, TYPES(I), 1, CATH(INDEX))
 30      CONTINUE
C                                       Set number of axes.
      CATBLK(KIDIM) = NAXIS
      CATBLK(KIPCN) = NRAN
C                                       Compressed?
      IF (DOUVCM) CATBLK(KINAX) = 1
C                                       User ID
      CATBLK(KIIMU) = NLUSER
C                                       Miscellaneous items.
C                                       Epoch.
      CATR(KREPO) = EPOCH
C                                       Convolving beam
      CATR(KRBMJ) = 0.0
      CATR(KRBMN) = 0.0
      CATR(KRBPA) = 0.0
      CATBLK(KINIT) = 0
C                                       Max. min.
      CATR(KRDMX) = 0.0
      CATR(KRDMN) = 0.0
C                                       Shift
      CATR(KRXSH) = 0.0
      CATR(KRYSH) = 0.0
C                                       "Old" (observed) position.
      CATD(KDORA) = 0.0
      CATD(KDODE) = 0.0
C                                       Sort order ('TB'=>time baseline)
      CALL CHR2H (2, '**', 1, CATH(KITYP))
C                                       No magic value blanking.
      CATR(KRBLK) = 0.0
C                                       Units
      CALL CHR2H (8, UNITS, 1, CATH(KHBUN))
C                                       Number of vis.
      CATBLK(KIGCN) = 10000
C                                       Position.
      CATD(KDCRV+4) = 0.0
      CATD(KDCRV+5) = 0.0
C                                       Rest Frequency
      CATD(KDRST) = 0.0D0
C                                       Alternate ref. value & pixel
      CATD(KDARV) = 0.0D0
      CATR(KRARP) = CATR(KRCRP+JLOCF)
      CATBLK(KIALT) = 0
C                                       Observing date.
      CATH(KHDOB) = HBLANK
C                                       Object.
      CALL CHR2H (8, MULTI, 1, CATH(KHOBJ))
C                                       Telescope.
      CALL CHR2H (8, TELE, 1, CATH(KHTEL))
C                                       Receiver
      IF (INSTRU.EQ.' ') INSTRU = INSTR
      CALL CHR2H (8, INSTRU, 1, CATH(KHINS))
C                                       Project name, set from tables
      CALL CHR2H (8, BLANK, 1, CATH(KHOBS))
C                                       Image type ='UV'
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
C                                       Get uv header info and
C                                       verify header structure.
      CALL UVPGET (IERR)
C                                       Reset frequency
      CATD(KDCRV+JLOCF) = 0.0D0
      FREQ = 0.0D0
C                                       File name
      DEFNAM = KOUTNM
      IF (DEFNAM.EQ.' ') CALL H2CHR (8, 1, CATH(KHOBJ), DEFNAM)
      IF (DEFNAM.EQ.' ') DEFNAM(1:8) = 'FITLD   '
      DEFCLS = KOUTCL
      IF (DEFCLS.EQ.' ') DEFCLS = 'UVDATA'
      IF (OUTSEQ.GE.0) CATBLK(KIIMS) = OUTSEQ
      LOCNAM = NAMOUT
      LOCCLS = CLAOUT
      CALL MAKOUT (DEFNAM, DEFCLS, 0, DEFCLS, LOCNAM, LOCCLS,
     *   CATBLK(KIIMS))
      CALL CHR2H (12, LOCNAM, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, LOCCLS, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = OUTSEQ
C                                       Create the UV file.
      CALL UVCREA (DISOUT, ISLOT, IWORK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Tell common we have a UV file.
      NCFILE = NCFILE + 1
      FVOL(1) = DISOUT
      FCNO(1) = ISLOT
      FRW(1) = 1
C                                       Record true number of vis
C                                       in file.
      CATBLK(KIGCN) = 0
      CALL CATIO ('UPDT', DISOUT, ISLOT, CATBLK, 'REST', IWORK, IERR)
C                                       Fill in info in ALLOW common
      ALLNO = ALLNO + 1
      ALLFRQ(ALLNO) = REFFRQ
      ALLNST(ALLNO) = NOSTKD
      ALLSTK(ALLNO) = STK1
      ALLNIF(ALLNO) = NOBAND
      ALLNCH(ALLNO) = NOCHAN
      ALLBW(ALLNO)  = CHNBW
      ALLRFC(ALLNO) = REFPIX
      ALLMOD(ALLNO) = .TRUE.
      ALLHIS(ALLNO) = .FALSE.
      ALLDSK(ALLNO) = DISOUT
      ALLCNO(ALLNO) = ISLOT
      ALLNUM(ALLNO) = CATBLK(KIGCN)
      ALLSEQ(ALLNO) = CATBLK(KIIMS)
      ALLNAM(ALLNO) = LOCNAM
      ALLCLS(ALLNO) = LOCCLS
      CALL COPY (256, CATBLK, ALLCAT(1,ALLNO))
      CURALL = ALLNO
C                                       Use keyword/value pair if exists
      IT1 = 1
      IT2 = 1
      IT3 = 1
      ITMP = MSGSUP
      MSGSUP = 32000
      CALL CATKEY ('READ', DISOUT, ISLOT, 'OLDRFQ  ', IT1, IT2, OLDRFQ,
     *   IT3, IWORK, IERR)
      MSGSUP = ITMP
      IF ((IERR.GT.0) .AND. (IERR.LT.20)) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 999
         END IF
      IF (IERR.EQ.0) ALLFRQ(ALLNO) = OLDRFQ
      IF (IERR.GT.20) THEN
         IERR = 0
         GO TO 999
         END IF
C                                       Finished.
      IERR = 0
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BTFHDR: ERROR ',I3,' READING KEYWORD/VALUE PAIR')
      END
      SUBROUTINE BTCAT (DISOUT, NAMOUT, CLAOUT, OUTSEQ, DOUVCM, IRET)
C-----------------------------------------------------------------------
C   Initialize ALLOW.INC with information about up to MXALL existing UV
C   data files that match the partial file specification DISOUT, NAMOUT,
C   CLAOUT, OUTSEQ, that are not write-locked, and that match the
C   compression state specified by DOUVCM. Exclude files that lack FREQ,
C   STOKES, or COMPLEX axes from the list.
C
C   Inputs:
C      DISOUT   I        Volume number for output files. If positive
C                        then only files on this disk will be listed in
C                        ALLOW.INC otherwise files from any disk will
C                        be listed.
C      NAMOUT   C*12     Name pattern for output files. This may contain
C                        wild-cards. Only files with names that match
C                        this pattern will be listed in ALLOW.INC.
C      CLAOUT   C*6      Class pattern for output files. This may
C                        contain wild-cards. Only files with classes
C                        that match this pattern will be listed in
C                        ALLOW.INC.
C      OUTSEQ   I        Sequence number for output files. If this is
C                        positive then only files with this sequence
C                        number will be listed in ALLOW.INC otherwise
C                        files with any sequence number will be listed.
C      DOUVCMP  L        If .TRUE. then only compressed UV data files
C                        will be listed in ALLOW.INC otherwise only
C                        uncompressed files will be listed in ALLOW.INC.
C
C   Output:
C      IRET     I        Error indicator
C                          0 - file list compiled
C                          1 - failed to construct list of files.
C
C
C   Output in ALLOW.INC:
C      ALLNO    I        Number of allowed output files
C                        0 <= ALLNO <= MXALL if IRET = 0
C      ALLDSK   I(*)     Output file disk numbers
C                        ALLDSK(1:ALLNO) = DISOUT if IRET = 0 and
C                        DISOUT > 0
C      ALLCNO   I(*)     Output file catalogue slot numbers
C      ALLNAM   C*12(*)  Output file names
C                        ALLNAM(1:ALLNO) match NAMOUT if IRET = 0
C      ALLCLS   C*6(*)   Output file classes
C                        ALLCLS(1:ALLNO) match CLAOUT if IRET = 0
C      ALLSEQ   I(*)     Output file sequence numbers
C                        ALLSEQ(1:ALLNO) = OUTSEQ if IRET = 0 and
C                        OUTSEQ > 0
C      ALLNCH   I(*)     Number of channels in each output file
C                        ALLNCH(1:ALLNO) > 0 if IRET = 0
C      ALLFRQ   D(*)     Reference frequencies in Hz for output files.
C                        If the I-th file (1 <= I <= ALLNO) has an
C                        OLDRFQ keyword tha ALLFRQ(I) is the value
C                        associated with this keyword otherwise
C                        ALLFRQ(I) is the reference value for the FREQ
C                        axis.
C      ALLBW    R(*)     Frequency channel increments for output files
C      ALLRFC   R(*)     Frequency reference channels for output files
C      ALLNIF   I(*)     Number of IFs in each output file. Files with
C                        no IF axis have one IF.
C      ALLNST   I(*)     Number of Stokes parameters for each output
C                        file.
C      ALLSTK   I(*)     Reference STOKES axis value for each output
C                        file
C      ALLNUM   I(*)     The number of visibilities in each output file
C      ALLFQN   I(*)     The number of frequence IDs for each output
C                        file, zero where there is no FQ table.
C      ALLFQS   D(MAXFQ, *)
C                        A list of reference frequencies for each
C                        frequency ID in each file.
C      ALLCAT   I(256, *)
C                        Catalogue header block for each output file
C      ALLMOD   L(*)     Modification flag for each output file
C                        ALLMOD(1:ALLNO) = .FALSE.
C      ALLHIS   L(*)     History flag for each output file
C                        ALLHIS(1:ALLNO) = .FALSE.
C-----------------------------------------------------------------------
      INTEGER   DISOUT
      CHARACTER NAMOUT*12, CLAOUT*6
      INTEGER   OUTSEQ
      LOGICAL   DOUVCM
      INTEGER   IRET
C
C     Local variables:
C
C     LODISK    Lowest disk number to search
C     HIDISK    Highest disk number to search
C     DISK      Disk being searched
C     CNO       Next catalogue number to examine
C     SEQ       Equal to OUTSEQ if OUTSEQ >= 0 or zero if OUTSEQ < 0
C     CATLUN    AIPS LUN for catalogue access
C     CATIND    AIPS FTAB index for catalogue I/O
C     BUFFER    Catalogue I/O buffer
C     ENDCAT    Has the end of the current catalogue been encountered
C     MSGSAV    Saved message suppression level
C     TYPE      File type returned by CATDIR
C     USERID    User ID returned by CATDIR
C     STATUS    File status returned by CATDIR
C     KEYS      List of keywords to search for
C     NUMKEY    Number of keywords to search for/number found
C     KEYLOC    Keyword location table
C     KEYVAL    Keyword value table
C     KEYTYP    Keyword value type table
C     FQLUN     AIPS LUN for FQ table I/O
C     IRET2     Return status from file close operation
C
      INTEGER   LODISK, HIDISK, DISK
      INTEGER   CNO
      INTEGER   SEQ
      INTEGER   CATLUN, CATIND
      PARAMETER (CATLUN = 16)
      INTEGER   BUFFER(512)
      LOGICAL   ENDCAT
      INTEGER   MSGSAV
      CHARACTER TYPE*2
      INTEGER   USERID
      CHARACTER STATUS*4
      CHARACTER KEYS(1)*8
      INTEGER   NUMKEY
      INTEGER   KEYLOC(1)
      DOUBLE PRECISION KEYVAL(1)
      INTEGER   KEYTYP(1)
      INTEGER   FQLUN
      PARAMETER (FQLUN = 42)
      INTEGER   IRET2
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ALLOW.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C
      DATA KEYS / 'OLDRFQ  ' /
C-----------------------------------------------------------------------
      IRET = 0
      ALLNO = 0
C
C     Establish range of disk numbers to search for matching files:
C
      IF (DISOUT.GT.0) THEN
         LODISK = DISOUT
         HIDISK = DISOUT
      ELSE
         LODISK = 1
         HIDISK = NVOL
      END IF
C
C     Force negative sequence numbers to 0 since NXTMAP will search for
C     highest sequence numbers if given a negative sequence number:
C
      IF (OUTSEQ.LT.0) THEN
         SEQ = 0
      ELSE
         SEQ = OUTSEQ
      END IF
C
C     Search each disk for potential output files:
C     Invariant: IRET = -1 implies that disks LODISK to DISK - 1 have
C                been searched
C     Bound: HIDISK + 1 - DISK
C
      DISK = LODISK
   10 IF ((IRET.EQ.0) .AND. (DISK.NE.HIDISK + 1)) THEN
         CNO = 1
         ENDCAT = .FALSE.
C
C        Search for matching files on the current disk:
C        Invariant: catalogue slots 1 to CNO - 1 have been searched
C        Bound: Maximum catalogue number for current disk - CNO + 1
C
   20    IF ((IRET.EQ.0) .AND. (.NOT. ENDCAT)) THEN
C
C           Search for next match:
C
            CALL NXTMAP ('READ', DISK, NAMOUT, CLAOUT, SEQ, 'UV',
     *                   NLUSER, CATLUN, CATIND, CNO, CATBLK, BUFFER,
     *                   ENDCAT, IRET)
            IF (.NOT. ENDCAT) THEN
               IF (IRET.EQ.0) THEN
C
C                 Found a matching file and set a READ lock on its
C                 header. Record its information if it is a multi-
C                 source file, is in the correct compression state,
C                 has the minimum required pointers, and there is
C                 still room in ALLOW.INC:
C
                  IF (ALLNO.NE.MXALL) THEN
                     MSGSAV = MSGSUP
                     MSGSUP = 32000
                     CALL UVPGET (IRET)
                     MSGSUP = MSGSAV
                     IF (IRET.EQ.0) THEN
C
C                       File has "normal" axes and random parameters.
C                       This includes COMPLEX, STOKES, and FREQ.
C                       Only record this file if it is a multi-source
C                       file and in the correct compression state:
C
                        IF ((ILOCSU.GE.0)
     *                           .AND. ((DOUVCM
     *                           .AND. (CATBLK(KINAX + JLOCC).EQ.1))
     *                           .OR. ((.NOT. DOUVCM)
     *                           .AND. (CATBLK(KINAX + JLOCC).EQ.3))))
     *                        THEN
                           ALLNO = ALLNO + 1
C
C                          Identify the file:
C
                           ALLDSK(ALLNO) = DISK
                           ALLCNO(ALLNO) = CNO
C
C                          Fill in the basic frequency information:
C
                           ALLNCH(ALLNO) = CATBLK(KINAX + JLOCF)
                           ALLBW(ALLNO)  = CATR(KRCIC + JLOCF)
                           ALLRFC(ALLNO) = CATR(KRCRP + JLOCF)
C
C                          Fill in the polarization information:
C
                           ALLNST(ALLNO) = CATBLK(KINAX + JLOCS)
                           ALLSTK(ALLNO) = NINT (CATD(KDCRV + JLOCS))
C
C                          Fill in the number of IFs:
C
                           IF (JLOCIF.GE.0) THEN
                              ALLNIF(ALLNO) = CATBLK(KINAX + JLOCIF)
                           ELSE
                              ALLNIF(ALLNO) = 1
                           END IF
C
C                          Fill in the number of visibilities:
C
                           ALLNUM(ALLNO) = CATBLK(KIGCN)
C
C                          Save catalogue header block:
C
                           CALL COPY (256, CATBLK, ALLCAT(1, ALLNO))
C
C                          Initialize flags:
C
                           ALLMOD(ALLNO) = .FALSE.
                           ALLHIS(ALLNO) = .FALSE.
C
C                          Retrieve complete file name:
C
                           CALL CATDIR ('INFO', DISK, CNO,
     *                                  ALLNAM(ALLNO), ALLCLS(ALLNO),
     *                                  ALLSEQ(ALLNO), TYPE, USERID,
     *                                  STATUS, BUFFER, IRET)
                           IF (IRET.EQ.0) THEN
C
C                             Fill in the reference frequency using the
C                             OLDRFQ keyword if it is present or the
C                             frequency axis definition if it is not:
C
                              MSGSAV = MSGSUP
                              MSGSUP = 32000
                              NUMKEY = 1
                              CALL CATKEY ('READ', DISK, CNO, KEYS,
     *                                     NUMKEY, KEYLOC, KEYVAL,
     *                                     KEYTYP, BUFFER, IRET)
                              MSGSUP = MSGSAV
                              IF ((IRET.EQ.0)
     *                            .OR. (IRET.GT.20)) THEN
                                 IF ((IRET.EQ.0)
     *                              .AND. (KEYTYP(KEYLOC(1)).EQ.1))
     *                              THEN
C
C                                    There is a OLDRFQ keyword with a
C                                    double precision value.
C
                                     CALL COPY (NWDPDP,
     *                                          KEYVAL(KEYLOC(1)),
     *                                          ALLFRQ(ALLNO))
                                  ELSE
C
C                                    Keyword missing or has wrong value
C                                    type so use axis definition:
C
                                     ALLFRQ(ALLNO) = CATD(KDCRV + JLOCF)
                                     IRET = 0
                                  END IF
C
C                                 Fill in frequency ID information if
C                                 there is an FREQID random parameter:
C
                                  IF (ILOCFQ.GE.0) THEN
                                     CALL FILLFQ (DISK, CNO, CATBLK,
     *                                            FQLUN,
     *                                            ALLFQS(1, ALLNO),
     *                                            ALLFQN(ALLNO), IRET)
                                     IF (IRET.NE.0) THEN
                                        WRITE (MSGTXT, 9020) IRET
                                        CALL MSGWRT (9)
                                        WRITE (MSGTXT, 9021) DISK,
     *                                     ALLNAM(ALLNO), ALLCLS(ALLNO),
     *                                     ALLSEQ(ALLNO)
                                        CALL MSGWRT (9)
                                        IRET = 1
                                     END IF
                                  END IF
                              ELSE
C
C                                Failure reading header keywords:
C
                                 WRITE (MSGTXT, 9022) IRET
                                 CALL MSGWRT (9)
                                 WRITE (MSGTXT, 9021) DISK,
     *                              ALLNAM(ALLNO), ALLCLS(ALLNO),
     *                              ALLSEQ(ALLNO)
                                 CALL MSGWRT (9)
                                 IRET = 1
                              END IF
                           ELSE
C
C                             Failure reading catalogue information.
C
                              WRITE (MSGTXT, 9023) IRET, CNO, DISK
                              CALL MSGWRT (9)
                              WRITE (MSGTXT, 9024)
                              CALL MSGWRT (9)
                              IRET = 1
                           END IF
                        END IF
                     ELSE
C
C                       File does not appear to be a valid UV file.
C                       Ignore it:
C
                        IRET = 0
                     END IF
                  END IF
C
C                 Close the file and relinquish the READ lock:
C                 Note that any previously flagged error status
C                 should be preserved.
C
                  CALL MAPCLS ('READ', DISK, CNO, CATLUN, CATIND,
     *                         CATBLK, .FALSE., BUFFER, IRET2)
                  IF (IRET2.NE.0) THEN
                     WRITE (MSGTXT, 9025) IRET2, CNO, DISK
                     CALL MSGWRT (9)
                     WRITE (MSGTXT, 9024)
                     CALL MSGWRT (9)
                     IRET = 1
                  END IF
               ELSE IF ((IRET.EQ.1) .OR. (IRET.EQ.2)) THEN
C
C                  File is busy so skip it:
C
                  IRET = 0
               ELSE IF (IRET.EQ.3) THEN
C
C                  Could not read catalogue for DISK so try the next:
C
                  IRET = 0
                  ENDCAT = .TRUE.
               ELSE
C
C                 Detected an error trying to read the catalogue for
C                 DISK so report it and quit.
C
                  WRITE (MSGTXT, 9026) IRET, DISK
                  CALL MSGWRT (9)
                  WRITE (MSGTXT, 9024)
                  CALL MSGWRT (9)
                  IRET = 1
               END IF
            END IF
C
            CNO = CNO + 1
            GO TO 20
         END IF
C
         DISK = DISK + 1
         GO TO 10
      END IF
C-----------------------------------------------------------------------
 9020 FORMAT ('ERROR ', I4, ' READING FREQUENCY INFORMATION FOR')
 9021 FORMAT ('FILE ', I2, ':', A12, '.', A6, '.', I4.4, ' IN BTCAT')
 9022 FORMAT ('ERROR ', I4, ' READING HEADER KEYWORDS FOR ')
 9023 FORMAT ('ERROR ', I4, ' READING INFO FOR FILE ', I4, ' ON DISK ',
     *        I2)
 9024 FORMAT ('IN BTCAT')
 9025 FORMAT ('ERROR ', I4, ' CLOSING FILE ', I4, ' ON DISK ', I2)
 9026 FORMAT ('ERROR ', I4, ' READING CATALOGUE FOR DISK ', I2)
      END
      SUBROUTINE BTFND (TABN, DOFSEL, DISKO, CNOOUT, IRET)
C-----------------------------------------------------------------------
C   Finds if the desired output file is already in allowed file table.
C   Input:
C      TABN     I        Table number in current FITS file, if = 1
C                        and DOCONC = .FALSE. ensure new file is
C                        opened up.
C      DOFSEL   L        .TRUE. if freq. selection enabled.
C   Input in common /ALLOW/:
C      CURALL   I        Pointer in ALL* arrays for file currently
C                        being filled.
C      ALLNO    I        Number of allowed output files
C      ALLDSK   I(*)     Output file disk numbers
C      ALLCNO   I(*)     Output file cat. slot numbers
C      ALLFRQ   D(*)     Output file reference frequency (Hz)
C      ALLBW    R(*)     Output file reference Bandwidth (ch. sep) (Hz)
C      ALLRFC   R(*)     Output file reference channel
C      ALLNCH   I(*)     Output file no. channels
C      ALLNIF   I(*)     Output file no. IFs
C      ALLNST   I(*)     Output file no. Stokes'
C      ALLNUM   I(*)     Output file, number vis currently present
C      ALLCAT   I(256,*) Output file, catalogue header
C      ALLSTK   I(*)     Output file first stokes type
C      ALLNAM   C*12(*)  Output file names
C      ALLCLS   C*6(*)   Output file classes
C      ALLSEQ   I(*)     Output file seq. number
C      ALLFQS   D(MAXFQI,*)  Output file freqs of IF1 for different
C                            FREQIDs
C      ALLFQN   I(*)     # FREQIDs in output file
C   Inputs in common /GLOB/:
C     OBSCOD   C*8        Observing code
C     NOSTKD   I          # stokes in data
C     STK1     I          Value of 1st Stokes
C     NOBAND   I          # of band's (IF's)
C     NOCHAN   I          # spectral channels
C     REFFRQ   D          Reference freq. (Hz)
C     CHNBW    R          Bandwidth of freq. channel (Hz)
C     REFPIX   R          Reference pixel
C   Output in common:
C      ALLMOD   L(*)     Output file modified flag.
C      ALLHIS   L(*)     Output file history flag, true if has HI.
C   Output:
C      DISKO    I        Output disk number
C      CNOOUT   I        Output catalog slot number
C      IRET     I        Return error code, 0=>found
C                                           1=>not found, must create
C                                          15=>too many choices
C-----------------------------------------------------------------------
      INTEGER   TABN, DISKO, CNOOUT, IRET
      LOGICAL DOFSEL
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'ALLOW.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
C
      INTEGER   LOOP, NPOSS, DUM, BUFFER(256), J
      DOUBLE PRECISION TOLERF
      CHARACTER CHTM12*12, CHTM6*6, CHTM2*2, STAT*4
      LOGICAL   ALLZER, MATCH, FMATCH
C-----------------------------------------------------------------------
      IRET = 1
      CHTM12 = ' '
      CHTM6 = ' '
      CHTM2 = ' '
      DUM = 0
      TOLERF = 2.1 * 1.0D6
      NPOSS = 0
C                                       Search list - checking (all)
C                                       ref.frq., # polns., 1st Stokes',
C                                       # IFs,
C                                       total # channels in this file.
      IF (DOCONC) THEN
         DO 100 LOOP = 1,ALLNO
            MATCH = .FALSE.
C
            MATCH = (ALLNST(LOOP).EQ.NOSTKD)
C                                       Deal with format deficiency.
C                                       If # Stokes = 1 we can't handle
C                                       the Stokes labelling changing in
C                                       or between jobs, so always
C                                       match but write a warning.
            IF (NOSTKD.GT.1) THEN
               MATCH = MATCH .AND. (ALLSTK(LOOP).EQ.STK1)
            ELSE IF (NOSTKD.EQ.1) THEN
               IF (MATCH .AND. (ALLSTK(LOOP).NE.STK1)) MATCH = .TRUE.
               END IF
            MATCH = MATCH .AND. (ALLNIF(LOOP).EQ.NOBAND)
            MATCH = MATCH .AND. (ALLNCH(LOOP).EQ.NOCHAN)
            MATCH = MATCH .AND. (ALLRFC(LOOP).EQ.REFPIX)
C
C            MATCH = (ALLNST(LOOP).EQ.NOSTKD) .AND.
C     *         (ALLSTK(LOOP).EQ.STK1)   .AND.
C     *         (ALLNIF(LOOP).EQ.NOBAND) .AND.
C     *         (ALLNCH(LOOP).EQ.NOCHAN) .AND.
C     *         (ALLRFC(LOOP).EQ.REFPIX)
            IF (.NOT.MATCH) GO TO 100
            FMATCH = (ABS (ALLFRQ(LOOP)-REFFRQ).LE.TOLERF)
C                                       If doing freq. selection must
C                                       search all files, can't just
C                                       rely on REFFRQ from incoming
C                                       file.
            IF ((.NOT.FMATCH) .AND. DOFSEL) GO TO 75
            IF (.NOT.FMATCH) THEN
               IF (ALLFQN(LOOP).EQ.1) THEN
                  GO TO 100
               ELSE IF (ALLFQN(LOOP).GT.1) THEN
                  DO 50 J = 1, ALLFQN(LOOP)
                     MATCH = ((ALLFQS(J,LOOP)-REFFRQ).LE.TOLERF)
                     IF (MATCH) GO TO 75
   50                CONTINUE
                  GO TO 100
                  END IF
               END IF
C                                       Found it
   75          ALLMOD(LOOP) = .TRUE.
               NPOSS = NPOSS + 1
               DISKO  = ALLDSK(LOOP)
               CNOOUT = ALLCNO(LOOP)
               CURALL = LOOP
               IRET = 0
 100        CONTINUE
C
         IF (NPOSS.GT.1) THEN
            MSGTXT = 'THERE ARE TOO MANY FILES AVAILABLE THAT'
            CALL MSGWRT (8)
            MSGTXT = 'THIS FITS FILE CAN BE APPENDED TO.'
            CALL MSGWRT (8)
            MSGTXT = 'SPECIFY EITHER OUTNAME, OUTCLASS,'
            CALL MSGWRT (8)
            MSGTXT = 'OUTDISK AND/OR FQTOL TO NARROW THE OPTIONS'
            CALL MSGWRT (8)
            IRET = 15
            GO TO 999
            END IF
C
         IF (NPOSS.EQ.0) CURALL = 0
C                                       Ensure creation if needed
         IF (NPOSS.EQ.0) IRET = 1
         IF (CURALL.GT.0) THEN
            ALLZER = .TRUE.
            CALL COPY (256, ALLCAT(1,CURALL), CATBLK)
            DO 150 LOOP = 1, 128
               IF ((FVOL(LOOP).EQ.DISKO) .AND.
     *            (FCNO(LOOP).EQ.CNOOUT)) THEN
                  FRW(LOOP) = 1
                  IF (NCFILE.LT.LOOP) NCFILE = LOOP
                  ALLZER = .FALSE.
                  CALL CATDIR ('INFO', DISKO, CNOOUT, CHTM12, CHTM6,
     *               DUM, CHTM2, DUM, STAT, BUFFER, IRET)
                  IF (STAT.NE.'WRIT') THEN
                     CALL STATCH (STAT, 'WRIT', DISKO, CNOOUT, CHTM2,
     *                  BUFFER, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET
                        GO TO 990
                        END IF
                     END IF
                  END IF
  150          CONTINUE
C                                      Have to set the write flag
            IF (ALLZER) THEN
               NCFILE = NCFILE + 1
               FVOL(1) = DISKO
               FCNO(1) = CNOOUT
               FRW(1) = 1
               CALL CATDIR ('INFO', DISKO, CNOOUT, CHTM12, CHTM6,
     *            DUM, CHTM2, DUM, STAT, BUFFER, IRET)
               IF (STAT.NE.'WRIT') THEN
                  CALL STATCH (STAT, 'WRIT', DISKO, CNOOUT, CHTM2,
     *               BUFFER, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET
                     GO TO 990
                     END IF
                  END IF
               END IF
            END IF
         END IF
C                                      If not concatanating check
C                                      have a file anyway
      IF (.NOT.DOCONC) THEN
         IF ((CURALL.GT.0) .AND. (ALLMOD(CURALL))) THEN
            DISKO  = ALLDSK(CURALL)
            CNOOUT = ALLCNO(CURALL)
            IRET = 0
            END IF
         IF (TABN.EQ.1) IRET = 1
         END IF
      GO TO 999
C
  990 CALL MSGWRT (8)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BTFND: ERROR ',I3,' WHILE CHANGING FILE STATUS')
      END
      SUBROUTINE BTEXPN (LUN, NVS, IRET)
C-----------------------------------------------------------------------
C   Expands output file.
C   Input:
C      LUN      I        LUN of file
C      NVS      I        Total number of vis wanted in file
C   Input/output in common:
C      FILSIZ   I(*)     Size of output file in vis. rec.
C      LRECO    I(*)     Length of output rec.
C   Output:
C      IRET     I        Return code, 0=>OK, else failed
C                                     2=> expansion error
C-----------------------------------------------------------------------
      INTEGER   LUN, NVS, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C
      DOUBLE PRECISION  DREC
      INTEGER   NREC, ZREC
C-----------------------------------------------------------------------
C                                       Expand file
      DREC = (NVS - FILSIZ)
      DREC = (DREC * LRECO) / 256.0D0 + 1.0D0
      NREC = DREC + 0.5D0
      ZREC = NREC
      CALL ZEXPND  (LUN, DISOUT, SFILE, NREC, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check got what we wanted
      IF (NREC.LT.ZREC) THEN
         IRET = 1
         WRITE (MSGTXT,1000) ZREC, NREC
         GO TO 990
         END IF
C                                       Get new file size
      CALL ZEXIST (DISOUT, SFILE, FILSIZ, IRET)
      FILSIZ = (FILSIZ * 256.0D0) / LRECO
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BTEXPN: REQUESTED ',I8,' BLOCKS, GOT ',I8)
      END
      SUBROUTINE UT2UV (KAXIS, DISK, ICNO, ISOLD, EOF, IERR)
C-----------------------------------------------------------------------
C  This routine will read the data section of a FITS 3-D table and copy
C  the data to the AIPS version of the table.
C  Inputs:
C     KAXIS    I(2)       Lengths of cols (in chars), number of rows
C     DISK     I          Volume number on which uv file resides
C     ICNO     I          Catalogue number of uv file.
C     ISOLD    L          True if file already exists and we are
C                         attaching data to it.
C  Outputs:
C     EOF      L          True if EOF found during processing
C     IERR     I          Error code. 0=ok.
C                                     12 => GET3DT time error, exit
C                                           without destroying file
C                                     13 => day number on incoming
C                                           file incorrect, exit
C                                           without destroying file.
C                                     14 => File being concatenated
C                                           to lacks CORR-ID random
C                                           parameter
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'DUVV.INC'
      INTEGER  DISK, ICNO, KAXIS(2), IERR
      LOGICAL  ISOLD, EOF
C
      INTEGER   I, J, K, NRAN, INDEX, MVIS, LENBU, JBUFSZ, BO, BIND,
     *   NWRIT, SIZE1, TABUFF(512), ID(3), DPTR(128,2), IVER, TABLUN,
     *   SRTORD, IRDNX, IRET, HERR, KBUFF(256), IT1, IT2, IT3, JTRIM
      REAL      ISIZE, JSIZE, SCLVIS, T1, T2
      CHARACTER RPTYP(60)*24, HILINE*72, LCORID*8
      LOGICAL   MATCH, T, F
C                                       Random parameters removed when
C                                       translating to AIPS format
      INTEGER   NRPREM
      PARAMETER (NRPREM = 4)
      CHARACTER RPREMV(NRPREM)*8
C                                       Random parameter substitutions
C                                       made when translating to AIPS format
      INTEGER   NRPTRN
      PARAMETER (NRPTRN = 22)
      CHARACTER RPTRAN(NRPTRN, 2)*8
C
      INCLUDE 'ALLOW.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'ORDER.INC'
      INCLUDE 'DIGCOR.INC'
      INCLUDE 'SCRBFS.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DBHD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA T, F /.TRUE., .FALSE./
      DATA RPREMV / 'DATE    ', 'ARRAY   ', 'FILTER  ', 'WEIGHT  ' /
      DATA RPTRAN /
C                                       Input values:
     *              'TIME    ', 'FREQID  ', 'SOURCE_I',
     *              'UU      ', 'VV      ', 'WW      ',
     *              'UU---SIN', 'VV---SIN', 'WW---SIN',
     *              'UU---NCP', 'VV---NCP', 'WW---NCP',
C                                       Flatters mis-spellings
     *              'UU--SIN ', 'VV--SIN ', 'WW--SIN ',
     *              'UU--NCP ', 'VV--NCP ', 'WW--NCP ',
C                                       UU-L etc are erroneous but used
C                                       by the VLBA
     *              'UU-L    ', 'VV-L    ', 'WW-L    ',
C                                       VLBA has also output SOURCE ID
C                                       instead of SOURCE_ID
     *              'SOURCE I',
C                                       Output values:
     *              'TIME1   ', 'FREQSEL ', 'SOURCE  ',
     *              'UU-L-SIN', 'VV-L-SIN', 'WW-L-SIN',
     *              'UU-L-SIN', 'VV-L-SIN', 'WW-L-SIN',
     *              'UU-L-NCP', 'VV-L-NCP', 'WW-L-NCP',
     *              'UU-L-SIN', 'VV-L-SIN', 'WW-L-SIN',
     *              'UU-L-NCP', 'VV-L-NCP', 'WW-L-NCP',
     *              'UU-L-SIN', 'VV-L-SIN', 'WW-L-SIN',
     *              'SOURCE  ' /
C-----------------------------------------------------------------------
      JBUFSZ = UVBFSL * 2
C                                       Make a dummy UT table because
C                                       we need the pointers below.
      IVER = 0
      TABLUN = 28
      SRTORD = 0
      CALL MAKUT (SRTORD, DISK, ICNO, IVER, CATBLK, TABLUN, DPTR,
     *   TABUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
C                                       Old file?
      IF (ISOLD) THEN
C                                       Get CATBLK
         CALL CATIO ('READ', DISK, ICNO, CATBLK, 'REST', KBUFF, IERR)
         IF ((IERR.NE.0) .AND. (IERR.LT.6)) THEN
            WRITE (MSGTXT,1040) IERR
            GO TO 990
            END IF
C                                       Number of vis
         IERR = 0
         FSTVIS = CATBLK(KIGCN)
         END IF
      ISOLD = ISOLD .AND. (FSTVIS.GT.0)
      IF (ISOLD) THEN
         WRITE (MSGTXT,1020) ALLNAM(CURALL), ALLCLS(CURALL),
     *      ALLSEQ(CURALL), DISK
         CALL MSGWRT (3)
         END IF
C                                       Insert variables into
C                                       catalogue header if they
C                                       don't already exist, if
C                                       they do then just compare them.
      REFOFF = 0.D0
      CALL CATMAT (CATBLK, MATCH)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1000) 'UT'
         CALL MSGWRT (6)
         END IF
C                                       File size (words)
      IF (.NOT.ISOLD) THEN
         ISIZE = 1.0
         DO 30 I = 1,CATBLK(KIDIM)
            ISIZE = ISIZE * CATBLK(KINAX+I-1)
 30         CONTINUE
C                                       Build up the catalogue header
C                                       from the keywords.
C                                       Sort order
         IF (SORT.EQ.'T*') SORT = 'TB'
         CALL CHR2H (2, SORT, 1, CATH(KITYP))
C                                       Observing date.
         IF (CATH(KHDOB).EQ.HBLANK) CALL CHR2H (8, DATOBS,
     *      1, CATH(KHDOB))
C                                       Telescope.
         CALL CHR2H (8, TELESC, 1, CATH(KHTEL))
C                                       equinox
         IF (GLBEQU.EQ.'1950.0B') THEN
            CATR(KREPO) = 1950.0
         ELSE IF (GLBEQU.EQ.'J2000') THEN
            CATR(KREPO) = 2000.0
            END IF
C                                       Uniform axes
         DO 40 I = 1,MAXIS
C                                       Dimension
            CATBLK(KINAX+I-1) = MAXISI(I)
            IF (CTYPE(I).EQ.'COMPLEX ') THEN
               IF (DOUVCM) CATBLK(KINAX+I-1) = 1
               IF (.NOT.DOUVCM) CATBLK(KINAX+I-1) = 3
               END IF
C                                       Check if freq selection
C                                       enabled
            IF ((CTYPE(I).EQ.'FREQ    ') .AND. (CHNSLT))
     *            CATBLK(KINAX+I-1) = SLCHNS
C                                       Check if freq selection
C                                       enabled
            IF (((CTYPE(I)(1:2).EQ.'IF') .OR.
     *         (CTYPE(I)(1:4).EQ.'BAND')) .AND. (IFSLT))
     *            CATBLK(KINAX+I-1) = SLIFS
C                                       Increment
            CATR(KRCIC+I-1) = CDELT(I)
C                                       Ref pixel.
            CATR(KRCRP+I-1) = CRPIX(I)
C                                       Ref value.
            CATD(KDCRV+I-1) = CRVAL(I)
C                                       Axis type
            INDEX = KHCTP + (I-1) * 2
C                                       Check for axis BAND
            IF (CTYPE(I).EQ.'BAND    ') CTYPE(I) = 'IF'
            CALL CHR2H (8, CTYPE(I), 1, CATH(INDEX))
 40         CONTINUE
C                                       Set number of axes.
         CATBLK(KIDIM) = MAXIS
C                                       How many random parameters?
         NRAN = 0
         DO 50 I = 1,ITNCOL
            IF (.NOT.TMATX(I)) NRAN = NRAN + 1
            RPTYP(I) = TTYPE(I)
 50         CONTINUE
         CATBLK(KIPCN) = NRAN
C                                       Fill in names
C                                       Must change the more general
C                                       ones in the interchange
C                                       format to the AIPS specific
C                                       ones. The r.p's affected are
C                                       DATE + TIME -> TIME1
C                                       ARRAY becomes encoded in
C                                               BASELINE
C                                       FREQID -> FREQSEL
C                                       SOURCE ID -> SOURCE
         DO 60 I = 1,KIPTPN
            INDEX = KHPTP + (I-1) * 2
            CALL CHR2H (8, '        ', 1, CATH(INDEX))
 60         CONTINUE
         K = 1
         DO 100 I = 1,NRAN
            INDEX = KHPTP + (K-1) * 2
C                                       Skip this iteration if parameter
C                                       should be removed
            DO 70 J = 1, NRPREM
               IF (RPTYP(I)(1:8).EQ.RPREMV(J)) THEN
                  GO TO 100
               END IF
   70       CONTINUE
C                                       Substitute some parameter names
            DO 80 J = 1, NRPTRN
               IF (RPTYP(I)(1:8).EQ.RPTRAN(J, 1)) THEN
                  RPTYP(I) = RPTRAN(J, 2)
                  GO TO 90
                  END IF
 80            CONTINUE
C
   90       IF (RPTYP(I).EQ.'BASELINE') THEN
               CALL CHR2H (8, 'SUBARRAY', 1, CATH(INDEX))
            ELSE
               CALL CHR2H (8, RPTYP(I), 1, CATH(INDEX))
               END IF
            K = K + 1
  100    CONTINUE
C                                       Add correlation_id at end.
         INDEX = KHPTP + (K-1) * 2
         LCORID = 'ANTENNA1'
         CALL CHR2H (8, LCORID, 1, CATH(INDEX))
         K = K + 1
         INDEX = KHPTP + (K-1) * 2
         LCORID = 'ANTENNA2'
         CALL CHR2H (8, LCORID, 1, CATH(INDEX))
C                                       Add correlation_id at end.
         K = K + 1
         INDEX = KHPTP + (K-1) * 2
         LCORID = 'CORR-ID '
         CALL CHR2H (8, LCORID, 1, CATH(INDEX))
C                                       Add WEIGHT and SCALE if comp.
         IF (DOUVCM) THEN
            K = K + 1
            INDEX = KHPTP + (K-1) * 2
            RPTYP(NRAN+1) = 'WEIGHT  '
            CALL CHR2H (8, RPTYP(NRAN+1), 1, CATH(INDEX))
            K = K + 1
            INDEX = KHPTP + (K-1) * 2
            RPTYP(NRAN+2) = 'SCALE   '
            CALL CHR2H (8, RPTYP(NRAN+2), 1, CATH(INDEX))
            END IF
         NRAN = K
         CATBLK(KIPCN) = NRAN
C                                       Adjust NVIS based on true
C                                       file dimensions
         JSIZE = 1.0
         DO 110 I = 1, CATBLK(KIDIM)
            JSIZE = JSIZE * CATBLK(KINAX+I-1)
  110    CONTINUE
         IF (NVIS.GT.0) THEN
            NVIS = NVIS * ISIZE/JSIZE
            END IF
         CATBLK(KIGCN) = NVIS
C                                       Assign true ref freq, i.e.
C                                       frequency of lowest IF based on
C                                       first file on disk
         CATD(KDCRV+JLOCF) = ADJRFQ
C                                       Record original ref.freq as
C                                       keyword/value pair.
         IT1 = 1
         IT2 = 1
         IT3 = 1
         CALL CATKEY ('WRIT', DISK, ICNO, 'OLDRFQ  ', IT1, IT2,
     *      TABRFQ, IT3, KBUFF, IERR)
         END IF
C                                       Get uv header info and
C                                       verify header structure.
      IF (CHNSLT .OR. IFSLT) THEN
         CATR(KRCRP+JLOCF) = CATR(KRCRP+JLOCF) - BCHAN + 1.0
         IF (CHNSLT) CATBLK(KINAX+JLOCF) = ORIGCH
         IF (IFSLT) CATBLK(KINAX+JLOCIF) = ORIGIF
         CALL UVPGET (IERR)
         ORLREC = LREC
         ID(1) = INCS
         ID(2) = INCF
         ID(3) = INCIF
         IF (CHNSLT) CATBLK(KINAX+JLOCF) = SLCHNS
         IF (IFSLT) CATBLK(KINAX+JLOCIF) = SLIFS
         END IF
      CALL UVPGET (IERR)
      IF ((.NOT.CHNSLT) .AND. (.NOT.IFSLT)) ORLREC = LREC
      XINCS  = INCS
      XINCF  = INCF
      XINCIF = INCIF
      IF (CHNSLT .OR. IFSLT) THEN
         INCS  = ID(1)
         INCF  = ID(2)
         INCIF = ID(3)
         END IF
C                                       Do not concatenate to existing
C                                       files which lack a CORR-ID
C                                       random parameter.
      IF (ILOCID.LT.0) THEN
         IERR = 14
         WRITE (MSGTXT,1005)
         GO TO 990
         END IF
C                                       Update CATBLK
      CALL CATIO ('UPDT', DISK, ICNO, CATBLK, 'REST', KBUFF, IERR)
      CALL COPY (256, CATBLK, ALLCAT(1,CURALL))
C                                       Open UV file
      CALL ZPHFIL ('UV', DISK, ICNO, 1, SFILE, IERR)
      CALL ZOPEN (IDLUN, IDFIND, DISK, SFILE, T, F, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
      LRECO = LREC
C                                       Init vis file for write
C                                       Force single buffering to avoid
C                                       problems with file expansion.
      LENBU = (JBUFSZ - 2*NBPS) / (2 * LRECO)
C                                       Could be lots of data
      MVIS = 100000000
      BO = 1
      CALL UVINIT ('WRIT', IDLUN, IDFIND, MVIS, FSTVIS, LREC, LENBU,
     *   JBUFSZ, UVBUFF, BO, BIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1010) IERR
         GO TO 990
         END IF
      FSTVIS = FSTVIS + 1
C                                       Create the index table (we
C                                       delete this if it turns out
C                                       that file is not multi-source)
      LUNNX = 29
      NXVER = 1
      CALL NDXINI ('WRIT', NXBUFF, DISK, ICNO, NXVER, CATBLK, LUNNX,
     *   INXRNO, NXKOLS, NXNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
C                                       Read previous entry.
      IF (INXRNO.GT.1) THEN
         IRDNX = INXRNO - 1
         CALL TABNDX ('READ', NXBUFF, IRDNX, NXKOLS, NXNUMV, NXTIME,
     *      NXDT, NXSOUR, NXSUBA, NXFSTV, NXLSTV, NXFQ, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1070) IERR
            GO TO 990
            END IF
      ELSE
         NXSOUR = -1
         NXSUBA = -1
         NXFSTV = -1
         NXLSTV = -1
         NXFQ = -1
         NXTIME = -999.0
         NXDT = 0.0
         END IF
C                                       Get 3D table row into array
C                                       for UV io routine
      SCLVIS = VISSCL
      IF (SCLVIS.LE.0.0) SCLVIS = 1.0
      CALL GET3DT (DPTR, KAXIS, BIND, LENBU, LREC, SCLVIS, NWRIT, ICNO,
     *   WTTYPE, EOF, IERR)
      IF ((IERR.GE.12) .AND. (IERR.LE.14)) THEN
         IRET = IERR
         IERR = 0
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
      IERR = 0
C                                       Add some history
      WRITE (HILINE,1080) TSKNAM, SCLVIS
      CALL HIADD (IHLUN, HILINE, IHBLK, HERR)
      WRITE (HILINE,1090) TSKNAM, DOCORR
      CALL HIADD (IHLUN, HILINE, IHBLK, HERR)
      IF (ISVLBA) THEN
         IF (TWDVER.EQ.0) THEN
            WRITE (HILINE,1150) TSKNAM
         ELSE IF (TWDVER.EQ.1) THEN
            WRITE (HILINE,1160) TSKNAM
            END IF
         CALL HIADD (IHLUN, HILINE, IHBLK, HERR)
         END IF
      IF (DOFSEL) THEN
         T1 = SELFRQ / 1.0E6
         WRITE (HILINE,1110) TSKNAM, T1
         IF (SELFRQ.GT.0.0) CALL HIADD (IHLUN, HILINE, IHBLK, HERR)
         T2 = FRQTOL / 1.0E3
         WRITE (HILINE,1120) TSKNAM, T2
         IF (FRQTOL.GT.0.0) CALL HIADD (IHLUN, HILINE, IHBLK, HERR)
         IF (SELFRQ.GT.0.0) THEN
            WRITE (MSGTXT,1130) T1, T2
            CALL MSGWRT (2)
            END IF
         T1 = SELBAN / 1.0E6
         WRITE (HILINE,1100) TSKNAM, T1
         IF (SELBAN.GT.0.0) CALL HIADD (IHLUN, HILINE, IHBLK, HERR)
         IF (SELBAN.GT.0.0) THEN
            WRITE (MSGTXT,1140) T1
            CALL MSGWRT (2)
            END IF
         END IF
      J = 0
      IT2 = 0
      DO 210 I = 1,60
         IF (USRANM(I).EQ.' ') GO TO 215
         J = J + 1
 210     CONTINUE
 215  IF (IT2.LT.J) THEN
         IT1 = IT2 + 1
         I = JTRIM (USRANM(IT1))
         WRITE (HILINE,1220) TSKNAM, IT1, USRANM(IT1)(:I)
         IT2 = IT1
 220     IT2 = IT2 + 1
         I = JTRIM (HILINE)
         IF ((I.GT.60) .OR. (IT2.GT.J)) THEN
            CALL HIADD (IHLUN, HILINE, IHBLK, HERR)
            GO TO 215
         ELSE
            K = JTRIM (USRANM(IT2))
            HILINE(I+1:) = ', ''' // USRANM(IT2)(:K) // ''''
            GO TO 220
            END IF
         END IF
      IRET = 0
C                                       Compress output file?
      SIZE1 = ALLNUM(CURALL) + NWRIT
      CALL UCMPRS (SIZE1, DISK, ICNO, IDLUN, CATBLK, IERR)
      ALLNUM(CURALL) = CATBLK(KIGCN)
      CALL COPY (256, CATBLK, ALLCAT(1,CURALL))
      GO TO 995
C                                       error message
 990  CALL MSGWRT (8)
C                                       Close files
 995  CALL CATIO ('UPDT', DISK, ICNO, CATBLK, 'REST', KBUFF, IERR)
      CALL ZCLOSE (IDLUN, IDFIND, IERR)
      CALL TABIO ('CLOS', 0, NWRIT, TABUFF, TABUFF, IERR)
      CALL TABIO ('CLOS', 0, INXRNO, NXBUFF, NXBUFF, IERR)
      IERR = IRET
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UT2UV: ',A,' KEYWORDS DO NOT MATCH CAT. HEADER')
 1005 FORMAT ('UT2UV: TARGET FILE FOR CONCATENATION LACKS A CORR-ID',
     *   ' RANDOM PARM')
 1010 FORMAT ('UT2UV: ERROR ',I3,' OPENING UV DISC FILE')
 1020 FORMAT ('Attaching data to ',A12,'.',A6,'.',I4,' Vol: ',I3)
 1030 FORMAT ('UT2UV: ERROR ',I3,' TRANSLATING UV BINARY TABLE')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK')
 1050 FORMAT ('ERROR',I3,' CREATING UT FILE')
 1060 FORMAT ('ERROR',I3,' CREATING NX TABLE')
 1070 FORMAT ('ERROR',I3,' READING NX TABLE')
 1080 FORMAT (A6,'SCLVIS = ',F10.5,' / Correlator scaling value')
 1090 FORMAT (A6,'DIGICOR = ',I3,' / VLBA correlator digital corrs')
 1100 FORMAT (A6,'SELBAN = ',F10.5,' / MHz, bandwidth selected')
 1110 FORMAT (A6,'SELFRQ = ',F10.5,' / MHz, IF 1 freq. selected')
 1120 FORMAT (A6,'FRQTOL = ',F10.5,' / kHz, tolerance on SELFRQ')
 1130 FORMAT ('Selecting data with freq of ',F8.2, ' MHz +/- ',
     *   F10.2,' kHz')
 1140 FORMAT ('Selecting data with bandwidth of ',F8.3, ' MHz')
 1150 FORMAT (A6,'/ Performed FFT artifact corrections on self-spectra')
 1160 FORMAT (A6,'/ FFT artifact corrections no longer necessary')
 1220 FORMAT (A6,'ANTNAME(',I2,') = ''',A,'''')
      END
      SUBROUTINE GET3DT (DPTR, NAXIS, BIND, NPIO, LRECD, SCLVIS, NWRIT,
     *   ICNO, WTTYPE, EOF, IERR)
C-----------------------------------------------------------------------
C  This routine will read the VLBA uv binary table and fill up a
C  buffer which is passed to the UV I/O routine.
C  Inputs:
C     DPTR     I(128,2)   Data Pointers, used in table file control.
C     NAXIS    I(2)       Length of columns (in char), number of rows.
C     NPIO     I          Number of vis records per I/O call.
C     LRECD    I          Length of output vis record in words
C     SCLVIS   R          Visibility scaling factor.
C     ICNO     I          Slot number
C     WTTYPE   C*8        How to do weightings
C  In/Out:
C     BIND     I          Pointer to start of data in buffer.
C   in/out in 'SCRBUFF.INC'
C     UVBUFF   R(*)       Output data buffer to be written to uv
C                         disc file.
C  Outputs:
C     EOF      L          True if EOF found during processing
C     NWRIT    I          Number of vis actually written to the file
C     IERR     I          Error code. 0 = ok.
C                                     12 = times < those in prev file.
C                                     13 = Day number screwed up for
C                                          incoming file
C                                     4 = premature end of file
C-----------------------------------------------------------------------
      INTEGER   BIND, NPIO, LRECD, NWRIT, ICNO
      LOGICAL   EOF
      CHARACTER WTTYPE*8
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'ALLOW.INC'
      INCLUDE 'ORDER.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'DIGCOR.INC'
      INCLUDE 'DFLT.INC'
C                                       The max row is 3 * MAXCIF for
C                                       the data plus overhead for
C                                       random parameters etc.
      CHARACTER CBUFF*2048
      HOLLERITH RECHOL(UVBFSL)
      INTEGER   NAXIS(2), TBYTCT, MXLREB, TLINE(UVBFSL), DPTR(128,2),
     *   IERR, DATMAP(LMXCIF), DATMP2(LMXCIF), LERR, WASSRC,
     *   NAXIS1, NAXIS2, LCTR, RECII(UVBFSL), BYTCNT(7), KTYPE,
     *   IOFF, IT0, II, TCOUNT(128), NEXT, NBYTE, TPTYPE(128), I,
     *   LIMIT, TOFF(128), IBAS, IARR, K, NRAN, NDAT, TB1(4), TB2(4),
     *   NPOINT, NCOUNT, IFBIND, NVS, CURSOU, ITMP,
     *   CURFQI, THISUB, CURFVS, CURLVS, IROUND, IA1, IA2, OOUTFQ,
     *   NUMPOL, NUMIF, NUMFRQ, NUMWT, ISRC, SRCFNC, OLDFQI,
     *   BADTIM, BADDAY, BADLIM, BADSRC, BADTRN, OLDANT,
     *   BADSRN, NUMTEL, OSRC(MAXSUB), JS, JIF, JF, INDEX, OUTFQI,
     *   FQIFNC, BADFQI, IJ, L, TBUFF(256), CURNSB, SUBSRC(MAXSUB),
     *   SUBFQI(MAXSUB), ISUB, NSUB, JSUB, JARR, IT1, IT2, IT3, IT4,
     *   IT5, IT6, BADWT, ITFILT, JT, JTRIM
      REAL      WAIT(LMXCIF), SCLVIS, RECRR(UVBFSL), OLDTIM, CURTIM,
     *   PSIGN, TIMGAP, TFRQ, INFTIM(2), CURINT, OLDINT, TMINT, SCALWT
      DOUBLE PRECISION RECRD(UVBFSL/2), JULIAN, DAYOFF, OLDOFF, MAINFQ
      LOGICAL   RECLL(UVBFSL), GOTWT, ANTSW, PREMEF, WANSRC,
     *   WANTIM, WANFQI, SMATCH, FMATCH, DOWRIT, WTFLAG, GOTFLT, XPLSUB
C                                       Declarations for CALINI
      INTEGER   CLKOLS(MAXCLC), CLNUMV(MAXCLC), ICLRNO, NUMANT, NTERM,
     *   CLPOL, CLBUFF(512), CLUN, CLVER
      REAL      GMMOD
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DBHD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INTEGER ANTUP(MAXANT,MAXSUB), MXANTS
      INCLUDE 'SCRBFS.INC'
      EQUIVALENCE (RECII, RECLL, RECRD, RECRR, RECHOL)
      INCLUDE 'INCS:DAGV.INC'
      DATA BYTCNT /8,4,1,4,1,2,1/
C-----------------------------------------------------------------------
C                                       Assume no explicit subarrays
      XPLSUB = .FALSE.
C
      IF (CLINT.LE.0.0) CLINT = 1.0
      WRITE (MSGTXT,1070) CLINT
      CALL MSGWRT (3)
      NUMPOL = CATBLK(KINAX+JLOCS)
      NUMIF = CATBLK(KINAX+JLOCIF)
      NUMFRQ = CATBLK(KINAX+JLOCF)
      NUMWT = NUMPOL * ORIGIF * ORIGCH
      IF (NUMWT.GT.LMXCIF) THEN
         WRITE (MSGTXT,1000) NUMWT
         IERR = 1
         GO TO 990
         END IF
      PREMEF = .FALSE.
      JULIAN = 0.D0
      TIMGAP = 30.0 / 86400.0
      EOF = .FALSE.
      MXANTS = MAXANT * MAXSUB
      CALL FILL (MXANTS, 0, ANTUP)
      MAINFQ = CATD(KDCRV+JLOCF)
      LERR = 0
      BADTIM = 0
      BADDAY = 0
      BADSRC = 0
      BADSRN = 0
      BADTRN = 0
      BADFQI = 0
      BADWT  = 0
C      CONTIM(1) = 0.0D0
C      CONTIM(2) = 0.0D0
      CONTIM(3) = 0.0D0
      INFTIM(1) = 0.0
      INFTIM(2) = 0.0
      OOUTFQ = -1
      MPOL = NUMPOL
      MIF  = ORIGIF
      MFRQ = ORIGCH
      CALL FILL (MAXSUB, -1, OSRC)
      CALL RFILL (MAXSUB, -1.0, OLDSBT)
      CALL RFILL (MAXSUB, -1.0, OLDSBB)
      CALL FILL (MAXSUB, -1, SUBSRC)
      CALL FILL (MAXSUB, -1, SUBFQI)
      OLDINT = -1.0
      JSUB = 1
      CURNSB = 1
      DOWRIT = .TRUE.
C                                       Determine # antennas
      CLVER = 1
      CLUN = 40
      CALL AGINI ('READ', CLBUFF, DISOUT, ICNO, CLVER, CATBLK, CLUN,
     *   IERR)
      NUMANT = CLBUFF(5)
      CALL TABIO ('CLOS', 1, IAGRNO, CLBUFF, CLBUFF, IERR)
      ISVLBA = ANAME(1:4).EQ.'VLBA'
      IF (ISVLBA) THEN
         IF (CORREL.EQ.' ') CORREL = 'VLBA'
         IF ((CORREL.NE.'VLBA') .AND. (CORREL.NE.'DIFX')) THEN
            MSGTXT = 'ARRAY IS VLBA, BUT CORRELATOR IS NOT!??'
            CALL MSGWRT (8)
            ISVLBA = .FALSE.
            MSGTXT = 'VLBA CORRECTIONS TURNED OFF!'
            CALL MSGWRT (8)
         ELSE IF (.NOT.DELCOR) THEN
            MSGTXT = 'DELAY CORRECTIONS ARE NOT BEING DONE FOR VLBA!!!'
            CALL MSGWRT (8)
            END IF
         END IF
      II = -1
      IF (WTTYPE.EQ.'NORMAL') II = 1
      IF (WTTYPE.EQ.'CORRTIME') II = 2
      IF (WTTYPE.EQ.'CORRELAT') II = 0
      IF (II.GE.0) SETDEB = II
      IF (.NOT.ISVLBA) THEN
         DOCORR = DOCORR - 2
         IF ((CORREL.EQ.'VLBA') .OR. (CORREL.EQ.'DIFX')) THEN
            MSGTXT = 'ARRAY IS NOT VLBA, BUT CORRELATOR IS!??'
            CALL MSGWRT (6)
         ELSE
            MSGTXT = 'DIGICOR BEING APPLIED WHEN CORREL= ' // CORREL
            IF (DOCORR.GE.0) CALL MSGWRT (6)
            END IF
         IF (II.EQ.-1) THEN
            SETDEB = 1
            IF (ANAME(1:4).NE.'EVN ') SETDEB = 2
            END IF
         TWDVER = -1
         END IF
C
      IF ((DOCORR.GT.0) .AND. (NMRDUN.EQ.1)) THEN
         MSGTXT = '           !!!!!!!!!!!'
         CALL MSGWRT (2)
         MSGTXT = 'Digital corrections being performed'
         CALL MSGWRT (2)
         IF (DOCORR.EQ.1) THEN
            MSGTXT = 'Total power corrected even if no zero-padding'
         ELSE IF (DOCORR.EQ.2) THEN
            MSGTXT = 'Total power corrected only if zero-padding used'
            END IF
         CALL MSGWRT (2)
         MSGTXT = '           !!!!!!!!!!!'
         CALL MSGWRT (2)
         END IF
C                                       Is this already a file with
C                                       subarrays?
      ITMP = MSGSUP
      MSGSUP = 32000
      IT1 = 1
      IT2 = 1
      IT3 = 4
      CALL CATKEY ('READ', DISOUT, ICNO, 'SUBARRAY', IT1, IT2, IT4, IT3,
     *   SCRBUF, IERR)
      MSGSUP = ITMP
      IF ((IERR.EQ.0) .AND. (IT4.GT.0)) THEN
         GOTSUB = .TRUE.
         GOTFUB = .TRUE.
         END IF
      IF (IERR.GT.20) IERR = 0
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,2271) IERR
         GO TO 990
         END IF
C                                       Set up a mapping function
C                                       for switching cross
C                                       -hand polzns if baseline
C                                       direction switched, or
C                                       for dealing with LSB data.
      CURFQI = 1
      CALL DATSWT (DOUVCM, CURFQI, NUMPOL, ORIGIF, ORIGCH, INCS, INCIF,
     *   INCF, ICOR0, DATMAP, DATMP2)
C                                       Create the artifact correction
C                                       spectrum
      IF ((TWDVER.EQ.0) .AND. ISVLBA)
     *   CALL ARTFCT (ORIGCH, CURFQI)
      OLDFQI = CURFQI
C                                       Calculate end & type
C                                       of Column values.
C                                       Use TFCODE to determine types
C                                       because DPTR holds output info.
      TBYTCT = 0
      DO 100 I = 1,ITNCOL
         TPTYPE(I) = MOD (TFCODE(I),10)
         TCOUNT(I) = DPTR(I,2) / 10
         TOFF(I) = DPTR(I,1)
C                                       Count real bytes
         KTYPE = TPTYPE(I)
         TBYTCT = TBYTCT + TCOUNT(I) * BYTCNT(KTYPE)
  100    CONTINUE
C                                       Check buffer size
      MXLREB = UVBFSL * NBITWD / 8
C                                       Record too big
      IF (TBYTCT.GT.MXLREB) THEN
         IERR = 5
         WRITE (MSGTXT,1010) TBYTCT, MXLREB
         GO TO 990
         END IF
C
      IF (NAXIS(2).GT.0) THEN
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         NEXT = 1
         IF ((IERR.EQ.4) .OR. (IERR.EQ.10)) THEN
            PREMEF = .TRUE.
            WRITE (MSGTXT,1050)
            CALL MSGWRT (6)
            END IF
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       How many rows?
      NCOUNT = 0
      NWRIT = 0
      NAXIS1 = NAXIS(1)
      NAXIS2 = NAXIS(2)
      WRITE (MSGTXT,2120) NAXIS2
      CALL MSGWRT (4)
      TFRQ = ADJRFQ / 1.0E6
      WRITE (MSGTXT,2070) TFRQ
      CALL MSGWRT (4)
C                                       Skip here if need be
      IF (ALLSKP) THEN
         CALL TAPIO ('ADVF', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2170) IERR
            GO TO 990
            END IF
         EOF = .TRUE.
         GO TO 900
         END IF
C                                       Expand file?
C                                       Get file size
      CALL ZEXIST (DISOUT, SFILE, FILSIZ, IERR)
      FILSIZ = (FILSIZ * 256.0D0) / LRECO
      IF (NAXIS2.GT.(FILSIZ-FSTVIS)) THEN
C                                       Add NAXIS2 vis.
         NVS = FILSIZ + NAXIS2 + 100
         CALL BTEXPN (IDLUN, NVS, IERR)
         IF (IERR.NE.0) THEN
C                                       Expansion failed - save what
C                                       you've got.
            MSGTXT = 'FILE EXPANSION FAILED - QUITTING'
            CALL MSGWRT (8)
            NCOUNT = NCOUNT - NPIO
            MAJERR = 1
            IERR = 0
            GO TO 900
            END IF
         END IF
C
      IFBIND = BIND
C                                       Set # bad points acceptable
      BADLIM = 1000000
      DO 800 LCTR = 1,NAXIS2
         DO 200 I = 1,ITNCOL
C                                       Read a FITS table data entry.
            KTYPE = TPTYPE(I)
            NBYTE = BYTCNT(KTYPE) * TCOUNT(I)
            IF (KTYPE.EQ.7) NBYTE = 1 + (TCOUNT(I)-1) / 8
            CALL GTF3D (FDVEC, TBIND, NEXT, TAPBUF, NBYTE, TLINE, IERR)
            IF (IERR.EQ.4) EOF = .TRUE.
            IF ((IERR.EQ.4) .OR. (IERR.EQ.10)) THEN
               PREMEF = .TRUE.
               WRITE (MSGTXT,1050)
               CALL MSGWRT (6)
               WRITE (MSGTXT,1060) NAXIS2, LCTR
               CALL MSGWRT (6)
               IERR = 0
               GO TO 900
               END IF
            IF (IERR.NE.0) GO TO 999
C                                       Go to correct type
            IOFF = TOFF(I)
            IT0 = TPTYPE(I)
            GO TO (110, 120, 130, 140, 150, 160, 170), IT0
C                                       Double precision
 110        CALL ZR64RL (TCOUNT(I), 1, TLINE, RECRD(IOFF))
            GO TO 200
C                                       Single precision
 120        CALL ZR32RL (TCOUNT(I), 1, TLINE, RECRR(IOFF))
            GO TO 200
C                                       Character.
 130        CALL ZC8CL (TCOUNT(I), 1, TLINE, CBUFF)
            JT = JTRIM (CBUFF(:TCOUNT(I)))
            CALL CHR2H (TCOUNT(I), CBUFF, 1, RECHOL(IOFF))
            GO TO 200
C                                       Long Integer.
C                                       Convert to local I
 140        CALL ZI32IL (TCOUNT(I), 1, TLINE, RECII(IOFF))
C                                        Trap blanked here
            GO TO 200
C                                       Logical
 150        LIMIT = TCOUNT(I)
            CALL ZC8CL (LIMIT, 1, TLINE, CBUFF)
            DO 155 II = 1,LIMIT
               RECLL(IOFF+II-1) = CBUFF(II:II).EQ.'T'
 155           CONTINUE
            GO TO 200
C                                       Short Integer.
C                                       Convert to INTEGER
 160        CALL ZI16IL (TCOUNT(I), 1, TLINE, RECII(IOFF))
C                                        Trap blanked here
            GO TO 200
C                                       bit array
 170        CALL ZX8XL (TCOUNT(I), TLINE, RECII(IOFF))
 200        CONTINUE
C                                       Load up the UV buffer
C                                       with this table row.
C
C                                       Set default values for the array
C                                       number and filter ID in case
C                                       they are not present in the
C                                       random parameter list
         IARR = 0
         ITFILT = 0
C
         K = 0
         CALL RFILL (NUMWT, 0.0, WAIT)
         GOTWT = .FALSE.
         GOTFLT = .FALSE.
         NRAN = ITNCOL - 1
         DO 300 I = 1, NRAN
            IOFF = TOFF(I)
            K = K + 1
            IF (PRTLV.GT.1) THEN
               WRITE (MSGTXT,2010) I, TTYPE(I)
               CALL MSGWRT (6)
               END IF
C                                       Check for generalized
C                                       random parameters which
C                                       AIPS doesn't know about
            IF (TTYPE(I)(1:4).EQ.'DATE') THEN
               IF (TPTYPE(I).EQ.1) JULIAN = RECRD(IOFF)
               IF (TPTYPE(I).EQ.2) JULIAN = RECRR(IOFF)
               K = K - 1
               GO TO 300
               END IF
C                                       Mark baseline position
            IF (TTYPE(I)(1:8).EQ.'BASELINE') IBAS = BIND+K-1
C                                       Array or FILTER ID
            IF (TTYPE(I)(1:5).EQ.'ARRAY') THEN
               IARR = RECII(IOFF)
C                                       If the array number is not
C                                       zero then subarrays are
C                                       explicitly defined and FITLD
C                                       should not look for them
               IF (IARR.GT.0) THEN
                  XPLSUB = .TRUE.
                  GOTSUB = .TRUE.
                  END IF
               K = K - 1
               GO TO 300
               END IF
C
            IF (.NOT.GOTFLT) ITFILT = 0
            IF (TTYPE(I)(1:6).EQ.'FILTER') THEN
               ITFILT = RECII(IOFF)
               GOTFLT = .TRUE.
               K = K - 1
               GO TO 300
               END IF
C                                       We do something special
C                                       with the weight
            IF (TTYPE(I)(1:6).EQ.'WEIGHT') THEN
               CALL WTFILL (RECRR(IOFF), NUMPOL, ORIGIF, ORIGCH, INCS,
     *            INCIF, INCF, DOUVCM, WAIT)
               GOTWT = .TRUE.
               K = K - 1
               GO TO 300
               END IF
C                                       Load up according to data types
            IF (TPTYPE(I).EQ.1) THEN
               IF (PRTLV.GT.1) THEN
                  WRITE (MSGTXT,2030) RECRD(IOFF)
                  CALL MSGWRT (6)
                  END IF
               UVBUFF(BIND+K-1) = RECRD(IOFF)
            ELSE IF (TPTYPE(I).EQ.2) THEN
               IF (PRTLV.GT.1) THEN
                  WRITE (MSGTXT,2030) RECRR(IOFF)
                  CALL MSGWRT (6)
                  END IF
               UVBUFF(BIND+K-1) = RECRR(IOFF)
            ELSE IF ((TPTYPE(I).EQ.4) .OR. (TPTYPE(I).EQ.6)) THEN
               IF (PRTLV.GT.1) THEN
                  WRITE (MSGTXT,2040) RECII(IOFF)
                  CALL MSGWRT (6)
                  END IF
               UVBUFF(BIND+K-1) = RECII(IOFF)
               END IF
 300        CONTINUE
C                                       Note: add extra element for
C                                       correlation_id random parameter
C                                       and for ANTENNA1/2
         NPOINT = K + 4
         IF (LCTR.EQ.1) INFTIM(1) = UVBUFF(BIND+ILOCT)
         IF (LCTR.EQ.NAXIS2) INFTIM(2) = UVBUFF(BIND+ILOCT)
C                                       Assign correlation id.
         TMINT = UVBUFF(BIND+ILOCIT)
         IF (DELCOR) THEN
            CALL CORID (ITFILT, TMINT, I, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'CORID RETURNS ERROR: DELCORR MADE FALSE'
               CALL MSGWRT (8)
               I = 1
               DELCOR = .FALSE.
               IERR = 0
               END IF
         ELSE
            I = 1
            END IF
         UVBUFF(BIND+ILOCID) = I
C                                       Check source renumbering
         ISRC = IROUND (UVBUFF(BIND+ILOCSU))
         WASSRC = ISRC
         IF (ISRC.LE.0) THEN
            BADSRN = BADSRN + 1
            GO TO 800
            END IF
         IF (ISRC.GT.MAXSOU) THEN
            WRITE (MSGTXT,2260) ISRC, MAXSOU
            LERR = 14
            GO TO 890
            END IF
         IF (SRCCHA) THEN
            ISRC = SRCFNC (ISRC)
            UVBUFF(BIND+ILOCSU) = ISRC
            IF (ISRC.LE.0) THEN
               BADSRN = BADSRN + 1
               GO TO 800
               END IF
            END IF
C                                       Do we want this source?
         IF (.NOT.WANSRC(ISRC)) THEN
            BADSRC = BADSRC + 1
            GO TO 800
            END IF
C                                       Determine if an offset
C                                       day needs to be added
         OLDOFF = DAYOFF
         DAYOFF = 0.0D0
         IF ((CURJLD.GT.0.D0) .AND. (JULIAN.GT.0.D0)) DAYOFF =
     *      JULIAN - CURJLD
         IF (DAYOFF.LT.-0.001D0) THEN
            IF (NWRIT.EQ.0) THEN
               WRITE (MSGTXT,2020) JULIAN, CURJLD
               CALL MSGWRT (8)
               MSGTXT = 'NEW FILE HAS DAY NO. < DAY NO. OF OLD FILE'
               CALL MSGWRT (8)
               LERR = 13
               GO TO 900
               END IF
            BADDAY = BADDAY + 1
            IF (BADDAY.EQ.1) THEN
               MSGTXT = 'Bad day number:'
               CALL MSGWRT (8)
               WRITE (MSGTXT,2020) JULIAN, CURJLD
               CALL MSGWRT (8)
               END IF
            IF ((BADDAY+BADTIM).GT.BADLIM) THEN
               MSGTXT = 'Too many entries with bad time stamps'
               CALL MSGWRT (8)
               MSGTXT = 'Will skip this file'
               CALL MSGWRT (8)
               LERR = 13
               GO TO 900
               END IF
            DAYOFF = OLDOFF
            GO TO 800
            END IF
         UVBUFF(BIND+ILOCT) = UVBUFF(BIND+ILOCT) + DAYOFF
C                                       Time range selection
         IF (.NOT.WANTIM(UVBUFF(BIND+ILOCT))) THEN
C                                       Exit if gone past end time
            IF (UVBUFF(BIND+ILOCT).GT.TEND) GO TO 900
            BADTRN = BADTRN + 1
            GO TO 800
            END IF
C                                       Check for multiple
C                                       integration times
         CURINT = UVBUFF(BIND+ILOCIT)
         IF (((OLDINT.NE.-1.0) .AND. (CURINT.NE.OLDINT)) .OR.
     *      (ITFILT.GT.0)) MULINT = .TRUE.
         OLDINT = CURINT
C                                       Check that time order is being
C                                       maintained
         IF ((LCTR.EQ.1) .AND. (INXRNO.GT.1)) LSTTIM =
     *      NXTIME + 0.5*NXDT - 0.1*(CURINT/86400.0)
         IF ((UVBUFF(BIND+ILOCT).LT.LSTTIM) .AND. (.NOT. UNSORT)) THEN
            UNSORT = .TRUE.
            END IF
C                                       FQID
C
         IF (ILOCFQ.GE.0) THEN
            CURFQI = IROUND(UVBUFF(BIND+ILOCFQ))
            CURFQI = FQDUPS(CURFQI)
            IF (DOFSEL .AND. (.NOT.WANFQI(CURFQI))) THEN
                BADFQI = BADFQI + 1
                GO TO 800
                END IF
            IF (CURFQI.LT.0) THEN
               WRITE (MSGTXT,2200) CURFQI
               LERR = 14
               GO TO 890
               END IF
            IF (CURFQI.GT.FQIMAX) THEN
               WRITE (MSGTXT,2210) CURFQI, FQIMAX
               LERR = 14
               GO TO 890
               END IF
            IF (CURFQI.NE.OLDFQI) THEN
C                                       Ensure data mapping function
C                                       is up to date.
               CALL DATSWT (DOUVCM, CURFQI, NUMPOL, ORIGIF, ORIGCH,
     *            INCS, INCIF, INCF, ICOR0, DATMAP, DATMP2)
C                                       Ensure the artifact correction
C                                       spectrum also up to date.
               IF ((TWDVER.EQ.0) .AND. ISVLBA)
     *            CALL ARTFCT (ORIGCH, CURFQI)
               OLDFQI = CURFQI
               END IF
C                                       Ensure that FREQID=1 exists
            IF ((CURFQI.GT.1) .AND. (FQCOUN(1).EQ.0)) THEN
               DO 310 IJ = 1,FQINUM
                  IF (FRFQV(IJ).EQ.CURFQI) L = IJ
  310             CONTINUE
               CALL COPY (FQINUM, FRFQV, FQMAP)
               K = FQMAP(1)
               FQMAP(1) = CURFQI
               FQMAP(L) = K
               FRTCHA = .TRUE.
               MXFREX = FQIMAX
C                                       Adjust ref. freq
               IF (.NOT.REREF) THEN
                  REREF = .TRUE.
                  MAINFQ = REFFRQ + FRVLS(L)
                  ITMP = MAINFQ / 1.0D4 + 0.5
                  MAINFQ = ITMP * 1.0D4
                  ADJRFQ = MAINFQ
                  CATD(KDCRV+JLOCF) = MAINFQ
                  CALL CATIO ('UPDT', DISOUT, ICNO, CATBLK, 'REST',
     *               TBUFF, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,2240) IERR
                     GO TO 990
                     END IF
C                                       also adjust keyword/value
C                                       pair.
                  IF (DABS(MAINFQ-REFFRQ) .NE.
     *               (CATBLK(KINAX+JLOCF) * CATR(KRCIC+JLOCF))) THEN
                     IT1 = 1
                     IT2 = 1
                     IT3 = 1
                     CALL CATKEY ('WRIT', DISOUT, ICNO, 'OLDRFQ  ',
     *                  IT1, IT2, ADJRFQ, IT3, TBUFF, IERR)
                     IF (IERR.NE.0) THEN
                        WRITE (MSGTXT,2241) IERR
                        GO TO 990
                        END IF
                     END IF
                  END IF
               END IF
            END IF
C                                       Renumber FQID if necesary
         OUTFQI = FQIFNC(CURFQI)
         IF (OUTFQI.LT.1) THEN
            BADFQI = BADFQI + 1
            GO TO 800
            END IF
         UVBUFF(BIND+ILOCFQ) = OUTFQI
C                                       Check for subarray condition
C                                       unless subarrays are defined
C                                       explicitly in the incoming
C                                       file.
         IF (.NOT. XPLSUB) THEN
            IARR = 1
            IF (MULINT .OR. UNSORT) GOTSUB = .TRUE.
            IF (.NOT.GOTSUB) THEN
               JARR = 1
               IF (UVBUFF(BIND+ILOCT).GT.OLDSBT(JARR))
     *            OLDSBT(JARR) = -1.0
               IF (OLDSBT(JARR).LT.0.0) THEN
                  OLDSBT(JARR) = UVBUFF(BIND+ILOCT)
                  OLDSBB(JARR) = UVBUFF(IBAS)
                  SUBSRC(JARR) = ISRC
                  SUBFQI(JARR) = IROUND(UVBUFF(BIND+ILOCFQ))
                  END IF
C                                       Loop through existing subarrays
               NSUB = CURNSB
               SMATCH = .FALSE.
               FMATCH = .FALSE.
               DO 350 ISUB = 1, NSUB
                  IF (UVBUFF(BIND+ILOCT).EQ.OLDSBT(ISUB)) THEN
                     IF ((SUBSRC(ISUB).GT.0) .AND.
     *                  (ISRC.EQ.SUBSRC(ISUB))) THEN
                        SMATCH = .TRUE.
                        IT5 = ISUB
                        END IF
                     IF ((SUBFQI(ISUB).GT.0) .AND.
     *                  (OUTFQI.EQ.SUBFQI(ISUB))) THEN
                        FMATCH = .TRUE.
                        IT6 = ISUB
                        END IF
                     END IF
                  IF (SMATCH .AND. FMATCH) THEN
                     JARR = ISUB
                     GO TO 360
                     END IF
 350              CONTINUE
               IF (.NOT.SMATCH) THEN
                  MSGTXT = '*******************************************'
                  CALL MSGWRT (6)
                  MSGTXT = '   SOURCE SUBARRAY CONDITION ENCOUNTERED'
                  CALL MSGWRT (6)
                  CALL TODHMS (UVBUFF(BIND+ILOCT), TB1)
                  IT1 = UVBUFF(IBAS) / 256.0 + 0.1
                  IT2 = UVBUFF(IBAS) - 256 * IT1 + 0.1
                  IT3 = OLDSBB(IT5) / 256.0 + 0.1
                  IT4 = OLDSBB(IT5) - 256 * IT3 + 0.1
                  WRITE (MSGTXT,2280) TB1
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,2281) SUBSRC(IT5), IT3, IT4
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,2282) ISRC, IT1, IT2
                  CALL MSGWRT (6)
                  MSGTXT = '*******************************************'
                  CALL MSGWRT (6)
                  GOTSUB = .TRUE.
                  JARR = JARR + 1
                  CURNSB = CURNSB + 1
                  END IF
               IF (.NOT.FMATCH) THEN
                  MSGTXT = '*******************************************'
                  CALL MSGWRT (6)
                  MSGTXT = ' FREQUENCY SUBARRAY CONDITION ENCOUNTERED'
                  CALL MSGWRT (6)
                  CALL TODHMS (UVBUFF(BIND+ILOCT), TB1)
                  IT1 = UVBUFF(IBAS) / 256.0 + 0.1
                  IT2 = UVBUFF(IBAS) - 256 * IT1 + 0.1
                  IT3 = OLDSBB(IT6) / 256.0 + 0.1
                  IT4 = OLDSBB(IT6) - 256 * IT3 + 0.1
                  WRITE (MSGTXT,2280) TB1
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,2283) SUBFQI(IT5), IT3, IT4
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,2284) OUTFQI, IT1, IT2
                  CALL MSGWRT (6)
                  MSGTXT = '*******************************************'
                  CALL MSGWRT (6)
                  GOTSUB = .TRUE.
                  GOTFUB = .TRUE.
                  END IF
               END IF
C                                       Mark as subarrayed file
            IF (GOTSUB .OR. GOTFUB) THEN
               IT1 = 1
               IT2 = 1
               IT3 = 4
               CALL CATKEY ('WRIT', DISOUT, ICNO, 'SUBARRAY', IT1, IT2,
     *            IT1, IT3, SCRBUF, IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,2270) IERR
                  GO TO 990
                  END IF
               END IF
            END IF
C                                       Print source number for user
 360     IF (GOTSUB .AND. (.NOT.WRTFQ(OUTFQI))) DOWRIT = .TRUE.
         IF ((ISRC.NE.OSRC(IARR)) .OR. (OUTFQI.NE.OOUTFQ)) THEN
            OSRC(IARR) = ISRC
            CALL TODHMS (UVBUFF(BIND+ILOCT), TB1)
            IF (DOSMAP) THEN
               IF (ISRC.NE.WASSRC) THEN
                  WRITE (MSGTXT,2180) SRCNAM(ISRC), TB1, ISRC, WASSRC
               ELSE
                  WRITE (MSGTXT,2190) SRCNAM(ISRC), TB1, ISRC
                  END IF
            ELSE
               WRITE (MSGTXT,2190) TSRCN(ISRC), TB1, ISRC
               END IF
            CALL MSGWRT (4)
            IF (OUTFQI.NE.OOUTFQ) THEN
               OOUTFQ = OUTFQI
               WRITE (MSGTXT,2230) OUTFQI
               IF (OUTFQI.NE.FQMAP(OUTFQI)) WRITE (MSGTXT,2220)
     *            OUTFQI, FQMAP(OUTFQI)
               IF (DOWRIT) CALL MSGWRT (4)
               END IF
            END IF
         IF (GOTSUB) THEN
            WRTFQ(OUTFQI) = .TRUE.
            DOWRIT = .FALSE.
            END IF
C                                       Sort out the baseline/subarray
         UVBUFF(IBAS) = UVBUFF(IBAS) + (0.01*(IARR-1))
         IF (ANTCHA) CALL ANTREL (UVBUFF(IBAS))
         IA1 = UVBUFF(IBAS) / 256.0 + 0.1
         IA2 = UVBUFF(IBAS) - 256 * IA1 + 0.1
         ANTSW = IA1.GT.IA2
         CURA1 = IA1
         CURA2 = IA2
         NUMANT = MAX(NUMANT, IA1)
         NUMANT = MAX(NUMANT, IA2)
         IF (ANTSW) THEN
            PSIGN = -1.0
            UVBUFF(BIND+ILOCSA) = IARR
            UVBUFF(BIND+ILOCA1) = IA2
            UVBUFF(BIND+ILOCA2) = IA1
         ELSE
            PSIGN = 1.0
            UVBUFF(BIND+ILOCSA) = IARR
            UVBUFF(BIND+ILOCA1) = IA1
            UVBUFF(BIND+ILOCA2) = IA2
            END IF
C                                       Change sign of u,v,w if
C                                       needed, also multiply
C                                       by ref freq to change from
C                                       seconds -> wavelengths
         UVBUFF(BIND+ILOCU) = PSIGN * UVBUFF(BIND+ILOCU) * MAINFQ
         UVBUFF(BIND+ILOCV) = PSIGN * UVBUFF(BIND+ILOCV) * MAINFQ
         UVBUFF(BIND+ILOCW) = PSIGN * UVBUFF(BIND+ILOCW) * MAINFQ
C                                       Load up the data array
         K = BIND + NPOINT - 1
         NDAT = TCOUNT(ITNCOL) / MAXISI(1)
         IOFF = TOFF(ITNCOL)
C                                       better data weights
         IF (.NOT.GOTWT) CALL RFILL (NUMWT, 1.0, WAIT)
         TMINT = 1.0
         IF (ISVLBA) THEN
            IF (CORVER.GE.4.17) TMINT = UVBUFF(BIND+ILOCIT)
         ELSE
            IF ((ILOCIT.GE.0) .AND. (UVBUFF(BIND+ILOCIT).GT.0.0))
     *         TMINT = UVBUFF(BIND+ILOCIT)
            END IF
         IF (DOWGT.GT.0.0) THEN
            SCALWT = 2.0 * CHNBW * CURINT / TMINT
         ELSE
            SCALWT = 1.0
            END IF
         CALL LOADUP (DOUVCM, K, NDAT, IOFF, WAIT, SCLVIS, TMINT, ANTSW,
     *      MAXISI(1), DATMAP, DATMP2, DOCORR, CORREL, SETDEB, RECRR,
     *      SCALWT, THRESH, WTFLAG)
C                                       Skip record if weight-based
C                                       flagging
         IF (WTFLAG) THEN
            BADWT = BADWT + 1
            WTFLAG = .FALSE.
            GO TO 800
            END IF
         NCOUNT = NCOUNT + 1
C                                       If channel/IF selection then
C                                       extract selected channels
         IF (CHNSLT .OR. IFSLT) THEN
            CALL COPY (ORLREC, UVBUFF(BIND), TUVBUF)
            IOFF = 0
            DO 400 JIF = BIF, EIF
               DO 390  JF = BCHAN, ECHAN
                  DO 380  JS = 1, NUMPOL
                     INDEX = (JIF-1) * INCIF + (JF-1) * INCF +
     *                  (JS-1) * INCS + NRPARM + 1
                     UVBUFF(BIND+NRPARM+IOFF) = TUVBUF(INDEX)
                     IF (.NOT.DOUVCM) THEN
                        UVBUFF(BIND+NRPARM+IOFF+1) = TUVBUF(INDEX+1)
                        UVBUFF(BIND+NRPARM+IOFF+2) = TUVBUF(INDEX+2)
                        IOFF = IOFF + 3
                        END IF
                     IF (DOUVCM) IOFF = IOFF + 1
  380                CONTINUE
  390             CONTINUE
  400          CONTINUE
            END IF
C                                       Check random parameters for
C                                       indexing.
         IF (NWRIT.EQ.0) THEN
            CURSOU = -1
            THISUB = -1
            CURFVS = NWRIT + 1 + ALLNUM(CURALL)
            CURLVS = CURFVS - 1
            OLDTIM = UVBUFF(BIND+ILOCT)
            LSTTIM = OLDTIM
C                                       Open/create CL table
            CLVER = 1
            CLPOL = MIN (NUMPOL, 2)
            GMMOD = 1.0
            NTERM = 6
            ICLRNO = 0
            OLDANT = NUMANT
            CALL CALINI ('WRIT', CLBUFF, DISOUT, ICNO, CLVER, CATBLK,
     *         CLUN, ICLRNO, CLKOLS, CLNUMV, OLDANT, CLPOL, NUMIF,
     *         NTERM, GMMOD, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1080) IERR
               GO TO 990
               END IF
C                                       Update antenna keyword?
            IF (OLDANT.LT.NUMANT) THEN
               CALL UPDKEY (CLBUFF, 'NO_ANT  ', 4, NUMANT, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,2110) IERR
                  GO TO 990
                  END IF
               END IF
            IF (CONTIM(1).LT.-999.) CONTIM(1) = OLDTIM
            CONTIM(3) = OLDTIM
            END IF
         CURLVS = CURLVS + 1
         CURTIM = UVBUFF(BIND+ILOCT)
         THISUB = IARR
         IF (ILOCSU.GE.0) CURSOU = IROUND(UVBUFF(BIND+ILOCSU))
         IF (ILOCFQ.GE.0) OUTFQI = IROUND(UVBUFF(BIND+ILOCFQ))
         SOCOUN(CURSOU) = SOCOUN(CURSOU) + 1
         FQCOUN(OUTFQI) = FQCOUN(OUTFQI) + 1
         IF (NWRIT.EQ.0) THEN
            NXSOUR = CURSOU
            NXSUBA = THISUB
            NXFSTV = CURFVS
            NXLSTV = CURLVS
            NXFQ = OUTFQI
            END IF
C                                       Are there any changes?
C                                       Source name, subarray,
C                                       FQID, > 30sec time gap,
         IF ((CURSOU.NE.NXSOUR) .OR. (THISUB.NE.NXSUBA) .OR.
     *      (OUTFQI.NE.NXFQ) .OR. ((CURTIM-LSTTIM).GT.TIMGAP)) THEN
            NXTIME = (LSTTIM + OLDTIM) * 0.5
            NXDT = LSTTIM - OLDTIM
            NXFSTV = CURFVS
            NXLSTV = CURLVS - 1
            CALL TABNDX ('WRIT', NXBUFF, INXRNO, NXKOLS, NXNUMV, NXTIME,
     *         NXDT, NXSOUR, NXSUBA, NXFSTV, NXLSTV, NXFQ, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1040) IERR
               GO TO 990
               END IF
C                                       Write CL entries
            NUMTEL = MXANTN(NXSUBA)
            IF (NUMTEL.EQ.0) NUMTEL = NUMANT
            CALL WRITCL (CLBUFF, ICLRNO, CLKOLS, CLNUMV, NXSUBA, OLDTIM,
     *         LSTTIM, CLPOL, NUMIF, NXSOUR, NXFQ, ANTUP, NUMTEL, CLINT,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,2000) IERR
               GO TO 990
               END IF
            CALL FILL (MXANTS, 0, ANTUP)
            NXSOUR = CURSOU
            NXSUBA = THISUB
            NXFSTV = CURLVS
            NXLSTV = CURLVS
            CURFVS = CURLVS
            NXFQ = OUTFQI
            OLDTIM = CURTIM
            END IF
         ANTUP (IA1, (IARR)) = 1
         ANTUP (IA2, (IARR)) = 1
         LSTTIM = CURTIM
C                                       Now write the damn thing
         NWRIT = NWRIT + 1
C                                       Message to user
         IF (MOD (NWRIT,10000).EQ.0) THEN
            WRITE (MSGTXT,2130) NWRIT
            CALL MSGWRT (4)
            END IF
C
         IF (NCOUNT.EQ.NPIO) THEN
            CALL UVDISK ('WRIT', IDLUN, IDFIND, UVBUFF, NPIO,
     *         IFBIND, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020) IERR
               GO TO 990
               END IF
            BIND = IFBIND
            NCOUNT = 0
         ELSE
            BIND = BIND + LRECD
            END IF
C
 800     CONTINUE
      GO TO 900
C                                       message and die gracefully
 890  CALL MSGWRT (8)
      IF (NWRIT.LT.100) THEN
         IERR = 1
         GO TO 999
         END IF
C                                       Flush the buffer
 900  NPIO = -NCOUNT
      CALL UVDISK ('FLSH', IDLUN, IDFIND, UVBUFF, NPIO, BIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1030) IERR
         GO TO 990
         END IF
C                                       Write the latest NX entry
      NXTIME = (LSTTIM + OLDTIM) * 0.5
      NXDT = LSTTIM - OLDTIM
      NXFSTV = CURFVS
      NXLSTV = CURLVS
      IF (NWRIT.GT.0) THEN
         CALL TABNDX ('WRIT', NXBUFF, INXRNO, NXKOLS, NXNUMV,
     *      NXTIME, NXDT, NXSOUR, NXSUBA, NXFSTV, NXLSTV,
     *      NXFQ, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR
            GO TO 990
            END IF
C                                       Write CL entries
         CALL WRITCL (CLBUFF, ICLRNO, CLKOLS, CLNUMV, NXSUBA,
     *      OLDTIM, LSTTIM, CLPOL, NUMIF, NXSOUR, NXFQ,
     *      ANTUP, NUMANT, CLINT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) IERR
            GO TO 990
            END IF
         CALL FILL (MXANTS, 0, ANTUP)
         CONTIM(2) = LSTTIM
C                                       Double check NO_ANT
C                                       keyword. Total # may be
C                                       the same but the highest
C                                       # may have changed.
         IF (OLDANT.LT.NUMANT) THEN
            CALL UPDKEY (CLBUFF, 'NO_ANT  ', 4, NUMANT, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,2110) IERR
               GO TO 990
               END IF
            END IF
C                                       Close the CL table
         CALL TABIO ('CLOS', 1, ICLRNO, CLBUFF, CLBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1090) IERR
            GO TO 990
            END IF
         END IF
C                                       Message to user
      IF (CONTIM(2).GT.CONTIM(3)) THEN
         IF (TRANGE(1).GT.CONTIM(3)) TRANGE(1) = CONTIM(3)
         IF (TRANGE(2).LT.CONTIM(2)) TRANGE(2) = CONTIM(2)
         END IF
      CALL TODHMS (CONTIM(3), TB1)
      CALL TODHMS (CONTIM(2), TB2)
      IF ((CONTIM(3).EQ.0.0) .AND. (CONTIM(2).EQ.0.0)) THEN
         CALL TODHMS (INFTIM(1), TB1)
         CALL TODHMS (INFTIM(2), TB2)
         END IF
      WRITE (MSGTXT,2060) TB1, TB2
      IF ((CONTIM(2).GT.CONTIM(3)) .OR. (INFTIM(2).GT.INFTIM(1)))
     *   CALL MSGWRT (4)
      WRITE (MSGTXT, 2080) BADTIM
      IF (BADTIM.GT.0) CALL MSGWRT (4)
      WRITE (MSGTXT, 2085) BADDAY
      IF (BADDAY.GT.0) CALL MSGWRT (4)
      WRITE (MSGTXT, 2090) BADSRC
      IF (BADSRC.GT.0) CALL MSGWRT (4)
      WRITE (MSGTXT, 2160) BADSRN
      IF (BADSRN.GT.0) CALL MSGWRT (4)
      WRITE (MSGTXT, 2100) BADTRN
      IF (BADTRN.GT.0) CALL MSGWRT (4)
      WRITE (MSGTXT, 2105) BADFQI
      IF (BADFQI.GT.0) CALL MSGWRT (4)
      WRITE (MSGTXT, 2106) BADWT
      IF (BADWT.GT.0) CALL MSGWRT (4)
      WRITE (MSGTXT, 2130) NWRIT
      CALL MSGWRT (4)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GET3DT: # pol * IF * chans ',I7,
     *   ' too many, get AIPS Manager')
 1010 FORMAT ('ERROR: TABLE ROW =',I6,' BYTES, BUFFER SIZE =',I6)
 1020 FORMAT ('GET3DT: ERROR ',I3,' WRITING UV DATA FILE')
 1030 FORMAT ('GET3DT: ERROR ',I3,' FLUSHING UV DATA FILE')
 1040 FORMAT ('GET3DT: ERROR ',I3,' WRITING INDEX TABLE')
 1050 FORMAT ('GET3DT: PREMATURE END OF FILE WHILE READING FITS DATA')
 1060 FORMAT ('GET3DT: ',I7,' VIS EXPECTED, ',I7,' READ')
 1070 FORMAT ('CL table interval set at ',F5.2, ' minutes')
 1080 FORMAT ('GET3DT: ERROR ',I3,' OPENING CL TABLE')
 1090 FORMAT ('GET3DT: ERROR ',I3,' CLOSING CL TABLE')
 2000 FORMAT ('GET3DT: ERROR ',I3,' WRITING CL TABLE')
 2010 FORMAT ('GET3DT: R.P # ',I3,' = ',A)
 2020 FORMAT ('NEW: ',F12.2, '   OLD:',F12.2)
 2030 FORMAT ('GET3DT: RAND. P = ',1PE15.6)
 2040 FORMAT ('GET3DT: RAND. P = ',I15)
 2060 FORMAT ('UV table spanned time: ',I3,'/',2(I2.2,':'),I2.2,' -',
     *   I3,'/',2(I2.2,':'),I2.2)
 2070 FORMAT ('File ref. freq: ',F11.2,' MHz')
 2080 FORMAT ('Rejected ',I6,' records due to mis-ordered times')
 2085 FORMAT ('Rejected ',I6,' records due to bad day numbers')
 2090 FORMAT ('Rejected ',I6,' records due to source selection')
 2100 FORMAT ('Rejected ',I6,' records due to time selection')
 2105 FORMAT ('Rejected ',I6,' records due to frequency selection')
 2106 FORMAT ('Rejected ',I6,' records based on weights')
 2110 FORMAT ('GET3DT: ERROR ',I3,' UPDATING CL TABLE KEYWORDS')
 2120 FORMAT ('Current file has ',I8,' visibilities')
 2130 FORMAT (I8,' vis. written')
 2160 FORMAT ('Rejected ',I6,' records due to bad source numbers')
 2170 FORMAT ('ERROR ',I3,' SKIPPING OVER CURRENT FILE')
 2180 FORMAT ('Found ',A16,' at ',I3,'/',2(I2.2,':'),I2.2,' src #',I4,
     *   ' was ',I4)
 2190 FORMAT ('Found ',A16,' at ',I3,'/',2(I2.2,':'),I2.2,' src #',I4)
 2200 FORMAT ('GET3DT: IMPOSSIBLE FREQID # = ',I15)
 2210 FORMAT ('GET3DT: FREQID = ',I3,' MAX IN THIS DATA = ',I3)
 2220 FORMAT (38X,' fqid #',I4,' was ',I4)
 2230 FORMAT (38X,' fqid #',I4)
 2240 FORMAT ('GET3DT: ERROR ',I3,' RE-REFERENCING FREQUENCY')
 2241 FORMAT ('GET3DT: ERROR ',I3,' UPDATING KEYWORD/VALUE PAIR')
 2260 FORMAT ('GET3DT: SOURCE ID = ',I8,' MAX ALLOWED = ',I6)
 2270 FORMAT ('GET3DT: ERROR ',I3,' WRITING SUBARRAY KEYWORD')
 2271 FORMAT ('GET3DT: ERROR ',I3,' SEARCHING FOR SUBARRAY KEYWORD')
 2280 FORMAT ('At ',I3,'/',2(I2.2,':'),I2.2)
 2281 FORMAT ('found source #',I4,' on baseline ',I2,'-',I2)
 2282 FORMAT ('  and source #',I4,' on baseline ',I2,'-',I2)
 2283 FORMAT ('found freqid #',I4,' on baseline ',I2,'-',I2)
 2284 FORMAT ('  and freqid #',I4,' on baseline ',I2,'-',I2)
      END
      SUBROUTINE BTREQ (FDVEC, TBIND, TAPBUF, FITBLK, ICARD, EXTEN,
     *   EOF, RSKIP, IERR)
C-----------------------------------------------------------------------
C   This routine will parse a block from a FITS tape and look for the
C   required cards of a FITS extension header block, namely XTENSION,
C   BITPIX, NAXIS, NAXISn, PCOUNT, GCOUNT.
C   Inputs:
C      FDVEC     I(50)   File descriptor for TAPIO input stream
C      RSKIP     L       If true, skip the TAPIO read.
C   Input/Output:
C      TBIND     I       Pointer in FITBLK
C      TAPBUF    I(*)    TAPIO i/o buffer
C      FITBLK    C*2880  a block of FITS header data.
C   Outputs:
C      ICARD     I       The number of the last card parsed.
C      EXTEN     L       T means extension record, F means no.
C      EOF       L       T means end of file on 1st record.
C      IERR      I       0=ok, 1=messed up. An error message will
C                                be printed.
C-----------------------------------------------------------------------
      CHARACTER FITBLK*2880
      INTEGER   FDVEC(50), TBIND, TAPBUF(*), ICARD, IERR
      LOGICAL   EXTEN, EOF, RSKIP
C
      DOUBLE PRECISION X
      CHARACTER EWORD(6)*8, STRING*80, CARD*80, SYMBOL*8, CVER*1
      INTEGER   IKEYWD, NPNT, ITABNO, NCHAR, I, NLEN, TERR, IFOUND,
     *   NFOUND
      LOGICAL   END
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA EWORD /'XTENSION', 'BITPIX  ',  'NAXIS   ', 'PCOUNT  ',
     *   'GCOUNT  ', ' '/
C-----------------------------------------------------------------------
C                                       Read 1st block.
      IF (.NOT.RSKIP) THEN
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
         IF (TERR.NE.0) THEN
            IF (TERR.EQ.4) GO TO 940
            WRITE (MSGTXT,1000)
            GO TO 980
            END IF
         EOF = .FALSE.
         CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
         END IF
C                                       Look for XTENSION= 'type' card
      ICARD = 1
      IKEYWD = 1
      NPNT = 1
      EXTEN = .TRUE.
      CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), FITBLK, NPNT, CARD,
     *   SYMBOL, ITABNO, NFOUND, IFOUND, CVER, END, IERR)
      IF (IERR.NE.0) GO TO 950
      NLEN = 16
      CALL GETSTR (CARD, 80, NLEN, NPNT, STRING, NCHAR)
      EXTTYP = STRING(1:NCHAR)
C
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), FITBLK, NPNT,
     *   CARD, SYMBOL, ITABNO, NFOUND, IFOUND, CVER, END, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Get value of BITPIX
      CALL GETNUM (CARD, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      IF (X.GE.0.0) BITPIX = X + 0.1
      IF (X.LT.0.0) BITPIX = X - 0.1
C                                       Warning-Non standard bits/pixel
      IF ((BITPIX.EQ.8) .OR. (BITPIX.EQ.16) .OR. (BITPIX.EQ.32) .OR.
     *   (BITPIX.EQ.-32)) GO TO 20
         WRITE (MSGTXT,1010) BITPIX
         CALL MSGWRT (6)
C                                       Get NAXIS
 20   ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), FITBLK, NPNT, CARD,
     *   SYMBOL, ITABNO, NFOUND, IFOUND, CVER, END, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GETNUM (CARD, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      NAXIS = X + .01
C                                       Check for invalid no. of axis
C                                       for our buffer.
      IF (NAXIS.GT.50) GO TO 960
C                                       Find NAXISi
      DO 50 I = 1,NAXIS
         ICARD = ICARD + 1
         NPNT = 1
         CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         STRING = ' '
         IF (I.LT.10) THEN
            WRITE (STRING,1020) I
         ELSE
            WRITE (STRING,1030) I
            END IF
         CALL GTWCRD (ICARD, 1, 0, STRING, FITBLK, NPNT, CARD,
     *      SYMBOL, ITABNO, NFOUND, IFOUND, CVER, END, IERR)
         IF (IERR.NE.0) THEN
            IKEYWD = 6
            EWORD(IKEYWD) = STRING(1:8)
            GO TO 970
            END IF
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 975
         NAXISI(I) = X + .01
 50      CONTINUE
C                                       PCOUNT
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), FITBLK, NPNT, CARD,
     *   SYMBOL, ITABNO, NFOUND, IFOUND, CVER, END, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GETNUM (CARD, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      PCOUNT = X + 0.01
C                                       Get GCOUNT
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), FITBLK, NPNT, CARD,
     *   SYMBOL, ITABNO, NFOUND, IFOUND, CVER, END, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GETNUM (CARD, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      GCOUNT = X + 0.01
      GO TO 999
C                                       End of file.
 940  IERR = 0
      EOF = .TRUE.
      GO TO 999
C                                       Not extension record.
 950  IERR = 0
      EXTEN = .FALSE.
      GO TO 999
C                                       Invalid number of axis.
 960  WRITE (MSGTXT,1960) NAXIS
      GO TO 980
C                                       Expected keyword not found.
 970  WRITE (MSGTXT,1970) EWORD(IKEYWD), SYMBOL
      GO TO 980
 975  MSGTXT = 'BTREQ: BAD NUMBER ON ' // SYMBOL
C                                       Print error message set flag.
 980  CALL MSGWRT (6)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR READING FITS TABLE')
 1010 FORMAT ('INVALID TABLE BITS PER PIXEL =',I6)
 1020 FORMAT ('NAXIS',I1)
 1030 FORMAT ('NAXIS',I2)
 1960 FORMAT ('NUMBER OF TABLE AXIS TOO LARGE FOR BUFFER =',I6)
 1970 FORMAT ('EXPECTED TABLE KEYWORD ',A8,'. FOUND ',A8,'.')
      END
      SUBROUTINE CATMAT (CATIN, MATCH)
C-----------------------------------------------------------------------
C  Routine to match the global keywords with the information in the
C  catalogue header.
C  Inputs:
C      CATIN        I(256)  Catalogue header
C  Input from common /GLOB/:
C      OBSCOD       C*8     Observing code
C      NOSTKD       I       No polzns in the data
C      STK1         I       First Stokes parameter in the data
C      NOBAND       I       The number of bands (IF's) in the data.
C      NOCHAN       I       The number of spectral channels in the data.
C      REFFRQ       D       Freq at ref pixel (Hz)
C      CHNBW        R       Bandwidth of single spectral channel (Hz)
C      REFPIX       R       Ref. pixel
C  Outputs:
C      MATCH        L       True if matches header, false if not.
C  Outputs in comon
C      REFOFF       D       Difference between catalog ref freq and
C                           incoming file ref freq.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
      INTEGER   CATIN(256)
      LOGICAL   MATCH
C
      CHARACTER CHTM8*8
      INTEGER   ITBAND, ITCHAN, ITCOR, ITCOR0, JTRIM, NCHAR
      REAL      TCHNBW, TREFPX
      DOUBLE PRECISION TREFFQ, TOLERF, T1, T2
      LOGICAL   NOTSET, CHART
      INCLUDE 'FITLD.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'ORDER.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL COPY (256, CATIN, CATIEQ)
      CALL H2CHR (8, 1, CATHEQ(KHOBS), CHTM8)
      NCHAR = JTRIM (CHTM8)
      CHART = (NCHAR.EQ.0) .OR. (CHTM8.EQ.' ')
      REFOFF = 0.D0
C
      IF (JLOCIF.GE.0) THEN
         ITBAND = CATIEQ(KINAX+JLOCIF)
      ELSE
         ITBAND = -999
         END IF
C
      IF (JLOCF.GE.0) THEN
         ITCHAN = CATIEQ(KINAX+JLOCF)
         TREFFQ = CATDEQ(KDCRV+JLOCF)
         TREFFQ = TABRFQ
         TOLERF = 2.0 * 1.0D6
         TCHNBW = CATREQ(KRCIC+JLOCF)
         TREFPX = CATREQ(KRCRP+JLOCF)
      ELSE
         ITCHAN = -999
         END IF
C
      IF (JLOCS.GE.0) THEN
         ITCOR = CATIEQ(KINAX+JLOCS)
         ITCOR0 = CATDEQ(KDCRV+JLOCS) + (1.0-CATREQ(KRCRP+JLOCS))*
     *      CATREQ(KRCIC+JLOCS)
      ELSE
         ITCOR = -999
         ITCOR0 = -999
         END IF
C
      NOTSET = (CHART    .AND. (ITBAND.EQ.0) .AND.
     *   (ITCHAN.EQ.0)   .AND. (ITCOR.EQ.0)  .AND.
     *   (TCHNBW.EQ.1.0) .AND. (ITCOR0.EQ.0) .AND.
     *   (TREFPX.EQ.1.0))
      IF (NOTSET) THEN
         MATCH = .TRUE.
         CALL CHR2H (8, OBSCOD, 1, CATHEQ(KHOBS))
         IF (JLOCIF.GE.0) CATIEQ(KINAX+JLOCIF) = NOBAND
         IF (JLOCS.GE.0) THEN
            CATIEQ(KINAX+JLOCS)= NOSTKD
            CATDEQ(KDCRV+JLOCS) = STK1
            END IF
         IF (JLOCF.GE.0) THEN
            CATIEQ(KINAX+JLOCF) = NOCHAN
            CATDEQ(KDCRV+JLOCF) = REFFRQ
            CATREQ(KRCIC+JLOCF) = CHNBW
            CATREQ(KRCRP+JLOCF) = REFPIX
            END IF
      ELSE
         REFOFF = REFFRQ - TREFFQ
         MATCH = .TRUE.
         IF (CHTM8.NE.OBSCOD) MATCH = .FALSE.
C                                       Handle IF selection
         IF (.NOT.IFSLT) THEN
            IF ((JLOCIF.GE.0) .AND. (NOBAND.NE.ITBAND)) MATCH = .FALSE.
            END IF
C                                       Handle Channel selection
         IF (.NOT.CHNSLT) THEN
            IF ((JLOCF.GE.0)  .AND. (TREFPX.NE.REFPIX)) MATCH = .FALSE.
            END IF
         IF ((JLOCF.GE.0)  .AND. (NOCHAN.NE.ITCHAN)) MATCH = .FALSE.
         IF ((JLOCS.GE.0)  .AND. (NOSTKD.NE.ITCOR)) MATCH = .FALSE.
C                                       Deal with format deficiency.
C                                       If # Stokes = 1 we can't handle
C                                       the Stokes labelling changing in
C                                       or between jobs, so always
C                                       match. A warning will be issued
         IF ((JLOCS.GE.0)  .AND. (STK1.NE.ITCOR0) .AND. (NOSTKD.GT.1))
     *      MATCH = .FALSE.
         IF ((JLOCS.GE.0)  .AND. (STK1.NE.ITCOR0) .AND. (NOSTKD.EQ.1)
     *      .AND. MATCH) MATCH = .TRUE.
         IF ((JLOCS.GE.0)  .AND. (STK1.NE.ITCOR0) .AND. (NOSTKD.EQ.1))
     *      PRTPOL = .TRUE.
         IF ((JLOCF.GE.0)  .AND. (((DABS(REFFRQ - TREFFQ)) .GT.
     *      TOLERF))) MATCH = .FALSE.
C
C
         IF (.NOT.MATCH) THEN
            MSGTXT = '           Table value      Cat. header value'
            CALL MSGWRT (6)
            WRITE (MSGTXT,1000) OBSCOD, CHTM8
            CALL MSGWRT (6)
            IF (JLOCIF.GE.0) THEN
               WRITE (MSGTXT,1010) NOBAND, ITBAND
               CALL MSGWRT (6)
               END IF
            IF (JLOCF.GE.0) THEN
               WRITE (MSGTXT,1020) NOCHAN, ITCHAN
               CALL MSGWRT (6)
               END IF
            IF (JLOCS.GE.0) THEN
               WRITE (MSGTXT,1030) NOSTKD, ITCOR
               CALL MSGWRT (6)
               END IF
            IF (JLOCS.GE.0) THEN
               WRITE (MSGTXT,1040) STK1, ITCOR0
               CALL MSGWRT (6)
               END IF
            IF (JLOCF.GE.0) THEN
               WRITE (MSGTXT,1050) CHNBW, TCHNBW
CCC               CALL MSGWRT (6)
               END IF
            IF (JLOCF.GE.0) THEN
               WRITE (MSGTXT,1060) REFPIX, TREFPX
               CALL MSGWRT (6)
               END IF
            IF (JLOCF.GE.0) THEN
               T1 = REFFRQ / 1.0D6
               T2 = TREFFQ / 1.0D6
               WRITE (MSGTXT,1070) T1, T2
               CALL MSGWRT (6)
               END IF
            END IF
         END IF
      CALL COPY (256, CATIEQ, CATIN)
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OBSCODE:   ',1X,A8,8X,A8)
 1010 FORMAT ('NOBAND:    ',1X,I8,8X,I8)
 1020 FORMAT ('NOCHAN:    ',1X,I8,8X,I8)
 1030 FORMAT ('NOSTKD:    ',1X,I8,8X,I8)
 1040 FORMAT ('STK1:      ',1X,I8,8X,I8)
 1050 FORMAT ('CHNBW:    ',G11.4,4X,G11.4)
 1060 FORMAT ('REFPIX:    ',F5.2,15X,F5.2)
 1070 FORMAT ('REFFRQ:    ',F10.4,5X,F10.4)
      END
      SUBROUTINE FINCRD (FDVEC, TBIND, TAPBUF, FITBLK, ICARD, KEYW,
     *   KEYVAL, EOF, IERR)
C-----------------------------------------------------------------------
C   This routine loop through a FITS file until it finds a given
C   keyword. It will then return the number of the card in which it
C   found said keyword, and value of keyword.
C   Inputs:
C      FDVEC     I(50)   File descriptor for TAPIO input stream
C      KEYW      C*8     Keyword to search for.
C   Input/Output:
C      TBIND     I       Pointer in FITBLK
C      TAPBUF    I(*)    TAPIO i/o buffer
C      FITBLK    C*2880  a block of FITS header data.
C   Outputs:
C      ICARD     I       The number of the last card parsed.
C      KEYVAL    C*16    Contents of keyword value string
C      EOF       L       T means end of file on 1st record.
C      IERR      I       0=ok, 1=messed up. An error message will
C                                be printed.
C-----------------------------------------------------------------------
      CHARACTER FITBLK*2880, KEYW*8, KEYVAL*16
      INTEGER   FDVEC(50), TBIND, TAPBUF(*), ICARD, IERR
      LOGICAL   EOF
C
      CHARACTER STRING*80, CARD*80, SYMBOL*8, CVER*1
      INTEGER   NPNT, ITABNO, NCHAR, NLEN, TERR, NFOUND, IFOUND
      LOGICAL   END
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Read 1st block.
      CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
      IF (TERR.NE.0) THEN
         IF (TERR.EQ.4) GO TO 940
         WRITE (MSGTXT,1000)
         GO TO 980
         END IF
      EOF = .FALSE.
      CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
C                                       Look for given keyword
      ICARD = 1
 10   NPNT = 1
      CALL GTWCRD (ICARD, 1, 0, KEYW, FITBLK, NPNT, CARD,
     *   SYMBOL, ITABNO, NFOUND, IFOUND, CVER, END, IERR)
      IF (IERR.EQ.0) THEN
         NLEN = 16
         CALL GETSTR (CARD, 80, NLEN, NPNT, STRING, NCHAR)
         KEYVAL = STRING(1:NCHAR)
         GO TO 999
         END IF
C                                       Loop again
      IF (IERR.GE.2) THEN
         ICARD = ICARD + 1
         CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         GO TO 10
         END IF
C                                       End of file.
 940  IERR = 0
      EOF = .TRUE.
      GO TO 999
C                                       Print error message set flag.
 980  CALL MSGWRT (6)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR READING FITS TABLE')
      END
      SUBROUTINE GLBKEY (NUMKEY, KEYWRD, KEYV, KEYLOC, KEYTP)
C-----------------------------------------------------------------------
C  This routine will extract the global keywords from those found in a
C  VLBA binary table header.
C  Inputs:
C     NUMKEY   I          Number of keywords in table
C     KEYWRD   C(*)*8     Keywords
C     KEYV     I(*)       Array of keyword values
C     KEYLOC   I(NUMKEY)  Location of keyword value in KEYV array
C     KEYTP    I(NUMKEY)  Type code of keywords.
C  Outputs in common /GLOB/:
C     OBSCOD   C*8        Observing code
C     NOSTKD   I          # stokes in data
C     STK1     I          Value of 1st Stokes
C     REFDAT   C*8        Current reference date of table (dd/mm/yy)
C     NOBAND   I          # of band's (IF's)
C     NOCHAN   I          # spectral channels
C     REFFRQ   D          Reference freq. (Hz)
C     CHNBW    R          Bandwidth of freq. channel (Hz)
C     REFPIX   R          Reference pixel
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'DUVV.INC'
      INTEGER   NUMKEY, KEYLOC(*), KEYTP(*), KEYV(*)
      CHARACTER KEYWRD(*)*8
C
      INTEGER   I, K, KPOS(NKEYUV), NOK, KEYVI
      CHARACTER KEYGLB(NKEYUV)*8
      REAL      SFREQ, KEYVR
      DOUBLE PRECISION KEYVAD
      INCLUDE 'INCS:DDCH.INC'
C
       DATA KEYGLB /'OBSCODE ', 'NO_STKD ', 'STK_1   ', 'RDATE   ',
     *   'NO_BAND ',
     *   'NO_CHAN ', 'REF_FREQ', 'CHAN_BW ', 'REF_PIXL', 'SORT    ',
     *   'TABREV  ', 'VIS_SCAL', 'WEIGHTYP', 'EQUINOX' /
C-----------------------------------------------------------------------
      NOK = NKEYUV
      CALL FILL (NOK, 0, KPOS)
      OBSCOD = ' '
      NOSTKD = 0
      STK1 = 0
C                                       do not lose current value
C      REFDAT = ' '
      NOBAND = 0
      NOCHAN = 0
      REFFRQ = 0.D0
      CHNBW = 0.0
      REFPIX = 0.0
      SORT = ' '
      TABREV = 0
      DO 20 I = 1,NUMKEY
         DO 10 K = 1,NOK
            IF (KEYGLB(K).EQ.KEYWRD(I)) KPOS(K) = I
 10         CONTINUE
 20      CONTINUE
      IF (KPOS(1).GT.0) CALL H2CHR (8, 1, KEYV(KEYLOC(KPOS(1))),
     *      OBSCOD)
      IF (KPOS(2).GT.0) NOSTKD = KEYV(KEYLOC(KPOS(2)))
      IF (KPOS(3).GT.0) STK1   = KEYV(KEYLOC(KPOS(3)))
      IF (KPOS(4).GT.0) CALL H2CHR (8, 1, KEYV(KEYLOC(KPOS(4))), REFDAT)
      IF (KPOS(5).GT.0) NOBAND = KEYV(KEYLOC(KPOS(5)))
      IF (KPOS(6).GT.0) NOCHAN = KEYV(KEYLOC(KPOS(6)))
      IF (KPOS(7).GT.0) THEN
         IF (KEYTP(KPOS(7)).EQ.1) THEN
            CALL COPY (NWDPDP, KEYV(KEYLOC(KPOS(7))), REFFRQ)
         ELSE IF (KEYTP(KPOS(7)).EQ.2) THEN
            CALL COPY (1, KEYV(KEYLOC(KPOS(7))), SFREQ)
            REFFRQ = SFREQ
            END IF
         END IF
      IF (KPOS(8).GT.0) THEN
         IF (KEYTP(KPOS(8)).EQ.1) THEN
            CALL COPY (NWDPDP, KEYV(KEYLOC(KPOS(8))), KEYVAD)
            CHNBW = KEYVAD
         ELSE
            CALL COPY (1, KEYV(KEYLOC(KPOS(8))), CHNBW)
            END IF
         END IF
      IF (KPOS(9).GT.0) THEN
         IF (KEYTP(KPOS(9)).EQ.1) THEN
            CALL COPY (NWDPDP, KEYV(KEYLOC(KPOS(9))), KEYVAD)
            REFPIX = KEYVAD
         ELSE
            CALL COPY (1, KEYV(KEYLOC(KPOS(9))), REFPIX)
            END IF
         END IF
      IF (KPOS(10).GT.0)
     *   CALL H2CHR (8, 1, KEYV(KEYLOC(KPOS(10))), SORT)
      IF (KPOS(11).GT.0)
     *   TABREV = KEYV(KEYLOC(KPOS(11)))
      IF (KPOS(12).GT.0) THEN
         IF (KEYTP(KPOS(12)).EQ.1) THEN
            CALL COPY (NWDPDP, KEYV(KEYLOC(KPOS(12))), KEYVAD)
            VISSCL = KEYVAD
         ELSE
            CALL COPY (1, KEYV(KEYLOC(KPOS(12))), VISSCL)
            END IF
         END IF
      IF (KPOS(13).GT.0)
     *   CALL H2CHR (8, 1, KEYV(KEYLOC(KPOS(10))), WTTYPE)
      IF (KPOS(14).GT.0) THEN
         KEYVAD = 0.0D0
         IF (KEYTP(KPOS(14)).EQ.1) THEN
            CALL COPY (NWDPDP, KEYV(KEYLOC(KPOS(14))), KEYVAD)
         ELSE IF (KEYTP(KPOS(14)).EQ.2) THEN
            CALL COPY (1, KEYV(KEYLOC(KPOS(14))), KEYVR)
            KEYVAD = KEYVR
         ELSE IF (KEYTP(KPOS(14)).EQ.4) THEN
            CALL COPY (1, KEYV(KEYLOC(KPOS(14))), KEYVI)
            KEYVAD = KEYVI
            END IF
         IF (KEYTYP(KPOS(14)).EQ.3) THEN
            CALL H2CHR (8, 1, KEYV(KEYLOC(KPOS(14))), GLBEQU)
         ELSE
            IF (ABS(KEYVAD-1950.0D0).LT.0.1) GLBEQU = '1950.0B'
            IF (ABS(KEYVAD-2000.0D0).LT.0.1) GLBEQU = 'J2000'
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE TABCOM (ITYPE, DISK, ISLOT, IVER, EXTVER, CATIN, LUN1,
     *   LUN2, IERR)
C-----------------------------------------------------------------------
C   This routine will read two tables of type ITYPE and determine if
C   they are identical or not. If they are it will remove version
C   EXTVER, if not, and version EXTVER is not empty, it will remove
C   version IVER and rename version EXTVER as IVER.
C   Inputs:
C      ITYPE    C*2   2-char code specifying table type.
C      DISK     I     Volume on which data reside
C      ISLOT    I     Catalogue number of data file
C      IVER     I     version number of file A
C      EXTVER   I     version number of file B
C      CATIN    I     Catalogue header for data file
C      LUN1     I     LUN to use for file A
C      LUN2     I     LUN to use for file B
C   Outputs:
C      IERR     I     Error code. 0=ok.
C-----------------------------------------------------------------------
      CHARACTER ITYPE*2
      INTEGER   DISK, ISLOT, IVER, EXTVER, LUN1, LUN2, CATIN(256), IERR
C
      INTEGER   NKEY1, NKEY2, NREC1, NREC2, NCOL1, NCOL2, NROWS1,
     *   NROWS2, DELVER, IRNO1, IRNO2, IRCODE, I, NCOLS1, NCOLS2, K,
     *   OTHRVR
      LOGICAL   IDENT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'SCRTCH.INC'
C-----------------------------------------------------------------------
C                                       Open both tables
      NREC1 = 500
      NKEY1 = 0
      NCOL1 = 0
      CALL TABINI ('READ', ITYPE, DISK, ISLOT, IVER, CATIN, LUN1,
     *   NKEY1, NREC1, NCOL1, TDATP1, TABUF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, IVER, ITYPE
         GO TO 990
         END IF
C
      NREC2 = 500
      NKEY2 = 0
      NCOL2 = 0
      CALL TABINI ('READ', ITYPE, DISK, ISLOT, EXTVER, CATIN, LUN2,
     *   NKEY2, NREC2, NCOL2, TDATP2, TABUF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, EXTVER, ITYPE
         GO TO 990
         END IF
C                                       # rows
      NROWS1 = TABUF1(5)
      NCOLS1 = TABUF1(10)
      NROWS2 = TABUF2(5)
      NCOLS2 = TABUF2(10)
      DELVER = IVER
      OTHRVR = EXTVER
      IF (NROWS1.EQ.0) THEN
         DELVER = IVER
         OTHRVR = EXTVER
         WRITE (MSGTXT,1020) ITYPE, DELVER
         CALL MSGWRT (6)
         GO TO 800
      ELSE IF (NROWS2.EQ.0) THEN
         DELVER = EXTVER
         OTHRVR = IVER
         WRITE (MSGTXT,1020) ITYPE, DELVER
         CALL MSGWRT (6)
         GO TO 800
         END IF
      IF (NROWS1.NE.NROWS2) THEN
         DELVER = IVER
         OTHRVR = EXTVER
         WRITE (MSGTXT,1010) ITYPE, DELVER
         CALL MSGWRT (6)
         GO TO 800
         END IF
      IF (NCOLS1.NE.NCOLS2) THEN
         DELVER = IVER
         OTHRVR = EXTVER
         WRITE (MSGTXT,1010) ITYPE, DELVER
         CALL MSGWRT (6)
         GO TO 800
         END IF
C                                       Same number of rows,
C                                       compare them
      IDENT = .TRUE.
      IRCODE = 0
      DO 200 I = 1,NROWS1
         IRNO1 = I
         IRNO2 = I
         CALL TABIO ('READ', IRCODE, IRNO1, RECORD, TABUF1, IERR)
         CALL TABIO ('READ', IRCODE, IRNO2, TRECR, TABUF2, IERR)
         DO 100 K = 1, NCOLS1
            IF (RECORD(K).NE.TRECR(K)) IDENT = .FALSE.
 100        CONTINUE
 200     CONTINUE
      IF (.NOT.IDENT) THEN
         DELVER = IVER
         OTHRVR = EXTVER
         WRITE (MSGTXT,1030) ITYPE, DELVER
         GO TO 800
         END IF
      CALL TABIO ('CLOS', IRCODE, IRNO1, RECORD, TABUF1, IERR)
      CALL TABIO ('CLOS', IRCODE, IRNO2, TRECR, TABUF2, IERR)
      CALL RMEXT (DISK, ISLOT, ITYPE, IVER, CATIN, TABUF1, IERR)
      GO TO 999
C                                       Delete specified table
 800  CALL TABIO ('CLOS', IRCODE, IRNO1, RECORD, TABUF1, IERR)
      CALL TABIO ('CLOS', IRCODE, IRNO2, TRECR, TABUF2, IERR)
      CALL RMEXT (DISK, ISLOT, ITYPE, DELVER, CATIN, TABUF1, IERR)
C                                       Do we need to copy
      IF (OTHRVR.GT.DELVER) THEN
         CALL TABCOP (ITYPE, OTHRVR, DELVER, LUN1, LUN2, DISK,
     *      DISK, ISLOT, ISLOT, CATIN, CATIN, TABUF1, TABUF2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR, ITYPE, OTHRVR, DELVER
            GO TO 990
            END IF
         CALL RMEXT (DISK, ISLOT, ITYPE, OTHRVR, CATIN, TABUF1, IERR)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TABCOM: ERROR ',I3,' OPENING VERSION ',I3,' OF TABLE ',
     *   A2)
 1010 FORMAT ('TABCOM: ',A2,' tables incompatible, deleting version ',
     *   I3)
 1020 FORMAT ('TABCOM: ',A2,' version ',I3,' table empty, deleting')
 1030 FORMAT ('TABCOM: in table ',A2,' rows do not match, delete ',
     *   'version ',I3)
 1040 FORMAT ('TABCOM: ERROR ',I3,' COPYING ',A2,I3,' -> ',I3)
      END
      SUBROUTINE AG2AN (DISK, CNO, LUN, CATIN, REFDAY, IERR)
C-----------------------------------------------------------------------
C   Routine which creates an AN table from the contents of the more
C   general AG VLBA interchange tables.  It has to generate one AN table
C   for each AG table in  order to deal with multiple arrays.
C   Inputs:
C      DISK    I        Volume on which data reside
C      CNO     I        Catalogue number of data
C      LUN     I        Main lun to use.
C      CATIN   I(256)   Catalogue header.
C   Outputs:
C      IERR    I        Error code, 0 => OK; anything else => problem
C   Note:  Routine uses lun 46 during its operation, that lun is freed
C   upon exit.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DISK, CNO, LUN, CATIN(256), IERR
      CHARACTER REFDAY*(*)
C
      INTEGER   NUMSUB, I, J, IVER, NANTAB, NANT, IANT, LUN2, NCHAR,
     *   NTEL, II, ANTFNC, NANWR, NANRNO, NIF
      DOUBLE PRECISION GASTM, GRATE, JD
      LOGICAL   MATCH, COMPAR
C                                       Declarations for ANTINI
      INTEGER   ANKOLS(MAXANC), ANNUMV(MAXANC), ANNCAL, IANRNO, ANFQID,
     *   ANNUMO, NIFANT
      CHARACTER TLNAME*8, RFDATE*8, SYSTIM*8, HANXYZ*8, FRAMET*8
      REAL      POLRXY(2), DATUTC, ANUT1U
      DOUBLE PRECISION  CARRAY(3), FREQSA, ANGSTI, ANDEGP
C                                       Declarations for TABAN
      INTEGER   ANNUMB, MNTTEL
      CHARACTER TNAME*8, APLTYA*2, APLTYB*2
      REAL      TELXOF, ANPLAA, ANPLCA(2*MAXIF), ANPLAB, ANFWHM(MAXIF),
     *   ANDIAM, ANPLCB(2*MAXIF)
      DOUBLE PRECISION  TELXYZ(3), ORBVAR(6)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'ORDER.INC'
      INCLUDE 'INCS:DAGV.INC'
C-----------------------------------------------------------------------
      CALL COPY (256, CATIN, CATIEQ)
      LUN2 = 46
      PHASED = 0
C                                       Determine # subarrays
      NUMSUB = 1
      CALL FNDEXT ('AG', CATIEQ, NUMSUB)
      IF (NUMSUB.EQ.0) GO TO 999
C                                       Check if AN table already
C                                       exists
      CALL FNDEXT ('AN', CATIEQ, NANTAB)
      NIF = 1
      IF (JLOCIF.GE.0) NIF = MAX (1, CATBLK(KINAX+JLOCIF))
C                                       Loop over subarrays and
C                                       write AG table
      DO 100 I = 1,NUMSUB
         IVER = I
C                                       Open AG tables
         CALL AGINI ('READ', TABUF2, DISK, CNO, IVER, CATIEQ, LUN,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, IVER
            GO TO 990
            END IF
C                                       Insert variables into
C                                       catalogue header if they
C                                       don't already exist, if
C                                       they do then just compare them.
CC         CALL GLBKEY (NKEYAG, KEYW, KEYVAL, KLOCS, KEYTYP)
         REFOFF = 0.D0
         CALL CATMAT (CATIEQ, MATCH)
         IF (.NOT.MATCH) THEN
            WRITE (MSGTXT,1040) 'AG'
            CALL MSGWRT (6)
            END IF
C                                       Number of antennas
         NANT = TABUF2(5)
C                                       Copy variables
         DO 20 J = 1, 3
            CARRAY(J) = ARRAYC(J)
 20         CONTINUE
         FREQSA = SAFREQ
C
         IF (REFDAY.NE.' ') RDATE = REFDAY
         CALL CHTRIM (RDATE, 8, RFDATE, NCHAR)
         CALL CHTRIM (ANAME, 8, TLNAME, NCHAR)
         CALL CHTRIM (TIMSYS, 8, SYSTIM, NCHAR)
         ANGSTI = GSTIA0
         IF ((ANGSTI.EQ.0.D0) .OR. (REFDAY.NE.' ')) THEN
            CALL JULDAY (RFDATE, JD)
            CALL GSTROT (JD, ANGSTI, GASTM, GRATE)
            END IF
         ANDEGP = DEGPDY
         ANUT1U = UT1UTC
CC         DATUTC = IATUTC
         DATUTC = 0.0
         POLRXY(1) = POLARX
         POLRXY(2) = POLARY
         ANNUMO = NUMORB
         ANNCAL = 2
         ANFQID = -1
         NIFANT = NIF
         HANXYZ = 'RIGHT'
         FRAMET = 'ITRF'
         ANDIAM = 0.
         CALL RFILL (NIF, 0.0, ANFWHM)
         IF (ADJRFQ.GT.0.0D0) FREQSA = ADJRFQ
C                                       Create/Open AN table(s)
         CALL ANTINI ('WRIT', TABUF1, DISK, CNO, IVER, CATIEQ, LUN2,
     *      IANRNO, ANKOLS, ANNUMV, CARRAY, ANGSTI, ANDEGP, FREQSA,
     *      RFDATE, POLRXY, ANUT1U, DATUTC, SYSTIM, TLNAME, HANXYZ,
     *      FRAMET, ANNUMO, ANNCAL, NIFANT, ANFQID, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, IVER
            GO TO 990
            END IF
C                                       Number of antennas in existing
C                                       AN table, may be 0
         NTEL = TABUF1(5)
         NANWR = NTEL
C                                       Open equivalent AG table
C                                       Loop over antennas for this
C                                       subarray
         DO 90 IANT = 1,NANT
C                                       Read AG entries
            CALL AGTAB ('READ', TABUF2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1030) IERR, IVER
               GO TO 990
               END IF
C                                       Copy necessary variables
            CALL CHTRIM (ANNAME, 8, TNAME, NCHAR)
            IF (ANTCHA) NOSTA = ANTFNC (NOSTA, IVER)
            ANNUMB = NOSTA
            MNTTEL = MNTSTA
C                                       Must not change sign of antenna
C                                       y - we stay with right-hand now
            TELXYZ(1) = STAXYZ(1)
            TELXYZ(2) = STAXYZ(2)
            TELXYZ(3) = STAXYZ(3)
            TELXOF = STAXOF(1)
            DO 40 J = 1, NUMORB
               ORBVAR(J) = ORBPRM(J)
 40            CONTINUE
C                                       Prepare for phased VLA
            IF (TNAME.EQ.'Y') PHASED = -ANNUMB
            ANDIAM = STADIA
C                                       If new AN table then just
C                                       write the data
            IF (NANTAB.EQ.0) THEN
C                                       Write AN entries
               CALL TABAN ('WRIT', TABUF1, IANRNO, ANKOLS, ANNUMV,
     *            TNAME, TELXYZ, ORBVAR, ANNUMB, MNTTEL, TELXOF,
     *            ANDIAM, ANFWHM, APLTYA, ANPLAA, ANPLCA, APLTYB,
     *            ANPLAB, ANPLCB, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1020) IERR, IVER
                  GO TO 990
                  END IF
C                                      Else compare and just write
C                                      new information
            ELSE IF (NANTAB.GT.0) THEN
               COMPAR = .FALSE.
               DO 80 II = 1, NTEL
                  IANRNO = II
                  CALL TABAN ('READ', TABUF1, IANRNO, ANKOLS, ANNUMV,
     *               TNAME, TELXYZ, ORBVAR, ANNUMB, MNTTEL, TELXOF,
     *               ANDIAM, ANFWHM, APLTYA, ANPLAA, ANPLCA, APLTYB,
     *               ANPLAB, ANPLCB, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1020) IERR, IVER
                     GO TO 990
                     END IF
C                                       Compare antenna name/number
                  COMPAR = (TNAME.EQ.ANNAME) .AND. (ANNUMB.EQ.NOSTA)
C                                      A match was made
                  IF (COMPAR) GO TO 90
   80             CONTINUE
C                                       If not then write the new
C                                       entry
C                                       Write AN entry on end of
C                                       existing table.
               ANNUMB = NOSTA
               MNTTEL = MNTSTA
               TELXYZ(1) = STAXYZ(1)
               TELXYZ(2) = STAXYZ(2)
               TELXYZ(3) = STAXYZ(3)
               TELXOF = STAXOF(1)
               DO 85 J = 1, NUMORB
                  ORBVAR(J) = ORBPRM(J)
   85             CONTINUE
               NANWR = NANWR + 1
               NANRNO = NANWR
               CALL TABAN ('WRIT', TABUF1, NANRNO, ANKOLS, ANNUMV,
     *            ANNAME, TELXYZ, ORBVAR, ANNUMB, MNTTEL, TELXOF,
     *            ANDIAM, ANFWHM, APLTYA, ANPLAA, ANPLCA, APLTYB,
     *            ANPLAB, ANPLCB, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1020) IERR, IVER
                  GO TO 990
                  END IF
               END IF
 90         CONTINUE
C                                       Close down current AN/AG
C                                       tables
            CALL TABIO ('CLOS', 1, IANRNO, TABUF1, TABUF1, IERR)
            CALL TABIO ('CLOS', 1, IAGRNO, TABUF2, TABUF2, IERR)
 100     CONTINUE
C                                       Update CATBLK
      CALL CATIO ('UPDT', DISK, CNO, CATIEQ, 'REST', TABUF1, IERR)
      CALL COPY (256, CATIEQ, CATIN)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AG2AN: ERROR ',I3,' OPENING AN TABLE # ',I4)
 1010 FORMAT ('AG2AN: ERROR ',I3,' OPENING AG TABLE # ',I4)
 1020 FORMAT ('AG2AN: ERROR ',I3,' READING AN TABLE # ',I4)
 1030 FORMAT ('AG2AN: ERROR ',I3,' WRITING AG TABLE # ',I4)
 1040 FORMAT ('AG2AN: ',A,' KEYWORDS DO NOT MATCH CAT. HEADER')
      END
      SUBROUTINE AT2AN (DISK, CNO, LUN, NUMBND, CATIN, IERR)
C-----------------------------------------------------------------------
C   Routine which loads up an existing AN table with the relevant
C   parameters from an AT table.
C   Inputs:
C      DISK    I        Volume on which data reside
C      CNO     I        Catalogue number of data
C      LUN     I        Main lun to use.
C      NUMBND  I        Number actual IFs
C      CATIN   I(256)   Catalogue header.
C   Outputs:
C      IERR    I        Error code, 0 => OK; anything else => problem
C   Note:  Routine uses lun 46 during its operation, that lun is freed
C   upon exit.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER    DISK, CNO, LUN, NUMBND, CATIN(256), IERR
C
      CHARACTER KEYWRD*8
      INTEGER   NUMSUB, I, IVER, NANT, IANT, IBAND, ICAL, LUN2, II,
     *   NUMKEY, KEYLOC, TYPKEY, NCHAR, ANTFNC, NENT, IENT, JJ
      HOLLERITH HTEMP(2)
      LOGICAL   WRTKEY
C                                       Declarations for ANTINI
      INTEGER   ANKOLS(MAXANC), ANNUMV(MAXANC), NUMORB, ANNCAL,
     *   IANRNO, ANFQID, NIFANT
      CHARACTER ANAME*8, RDATE*8, TIMSYS*8, HANXYZ*8, FRAMET*8
      REAL      POLRXY(2), UT1UTC, DATUTC
      DOUBLE PRECISION  ARRAYC(3), GSTIA0, DEGPDY, SAFREQ
C                                       Declarations for TABAN
      INTEGER   ANNUMB, MNTSTA
      CHARACTER TNAME*8, APLTYA*2, APLTYB*2
      REAL      STAXOF, ANPLAA, ANPLCA(2*MAXIF), ANPLAB, ANDIAM,
     *   ANPLCB(2*MAXIF), ANFWHM(MAXIF)
      DOUBLE PRECISION  STAXYZ(3), ORBPRM(6)
      INCLUDE 'ORDER.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'INCS:DATV.INC'
C-----------------------------------------------------------------------
      CALL COPY (256, CATIN, CATIEQ)
      LUN2 = 46
      WRTKEY = .FALSE.
C                                       Determine # subarrays
      NUMSUB = 1
      CALL FNDEXT ('AG', CATIEQ, NUMSUB)
      IF (NUMSUB.EQ.0) GO TO 999
C                                       drop unused FQs
      IF (RFQDUP) CALL UNFQTA ('AT', DISK, CNO, FQDUPS, CATIEQ, IERR)
C                                       Open the AT table
      IVER = 0
      CALL ATINI ('READ', TABUF2, DISK, CNO, IVER, CATIEQ, LUN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Loop over subarrays and
C                                       write AN table
      DO 100 I = 1,NUMSUB
         IVER = I
         CALL ANTINI ('READ', TABUF1, DISK, CNO, IVER, CATIEQ, LUN2,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, HANXYZ,
     *      FRAMET, NUMORB, ANNCAL, NIFANT, ANFQID, IERR)
         IF (IERR.EQ.0) CALL TABIO ('CLOS', 1, IANRNO, TABUF1, TABUF1,
     *      IERR)
         ANNCAL = MAX (ANNCAL, NOPCAL)
         IF (IERR.EQ.0) CALL ANTINI ('WRIT', TABUF1, DISK, CNO, IVER,
     *      CATIEQ, LUN2, IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0,
     *      DEGPDY, SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS,
     *      ANAME, HANXYZ, FRAMET, NUMORB, ANNCAL, NIFANT, ANFQID, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR, IVER
            GO TO 990
            END IF
C                                       Write extra keywords if
C                                       necessary
         IF (.NOT.WRTKEY) THEN
            WRTKEY = .TRUE.
            KEYWRD = 'POLTYPE'
            NUMKEY = 1
            KEYLOC = 1
            TYPKEY = 3
            CALL CHR2H (8, POLTYP, 1, HTEMP)
            CALL TABKEY ('WRIT', KEYWRD, NUMKEY, TABUF1, KEYLOC,
     *         HTEMP, TYPKEY, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1100) IERR
               CALL MSGWRT (7)
               END IF
            END IF
         NENT = TABUF2(5)
         NANT = TABUF1(5)
C                                       Loop over antennas for this
C                                       subarray
         DO 50 IENT = 1,NENT
C                                       Read AT entry
            IATRNO = IENT
            CALL ATTAB ('READ', TABUF2, NUMBND, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1060) IERR
               GO TO 990
               END IF
            TNAME = ANNAME
            CALL CHTRIM (TNAME, 8, ANNAME, NCHAR)
            IF (ANTCHA) NOSTA = ANTFNC (NOSTA, IVER)
C                                       read the AN table until find
C                                       matching antenna
            DO 40 IANT = 1,NANT
               IANRNO = IANT
               CALL TABAN ('READ', TABUF1, IANRNO, ANKOLS, ANNUMV,
     *            TNAME, STAXYZ, ORBPRM, ANNUMB, MNTSTA, STAXOF,
     *            ANDIAM, ANFWHM, APLTYA, ANPLAA, ANPLCA, APLTYB,
     *            ANPLAB, ANPLCB, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1050) IERR, IVER
                  GO TO 990
                  END IF
               IF ((TNAME.EQ.ANNAME) .AND. (ANNUMB.EQ.NOSTA)) THEN
                  IANRNO = IANRNO - 1
C                                       rearrange format of some
C                                       variables
                  ANPLAA = POLAA(1)
                  ANPLAB = POLAB(1)
                  CALL RFILL (2*NIFANT, 0.0, ANPLCA)
                  CALL RFILL (2*NIFANT, 0.0, ANPLCB)
                  DO 35 IBAND = 1,NIFANT
                     JJ = 2 * (IBAND-1)
                     II  = IBAND
                     IF (REORDR(1)) II = FORDER(IBAND,1)
                     DO 30 ICAL = 1,NOPCAL,2
                        ANPLCA(JJ+ICAL) = POLCA(1,II)
                        ANPLCA(JJ+ICAL+1) = POLCA(2,II)
                        ANPLCB(JJ+ICAL) = POLCB(1,II)
                        ANPLCB(JJ+ICAL+1) = POLCB(2,II)
 30                     CONTINUE
 35                  CONTINUE
C                                       Copy necessary variables
                  TNAME = ANNAME
                  ANNUMB = NOSTA
                  APLTYA = POLTYA
                  APLTYB = POLTYB
                  CALL RCOPY (NIFANT, ANTFWH, ANFWHM)
C                                       Update entry
                  CALL TABAN ('WRIT', TABUF1, IANRNO, ANKOLS, ANNUMV,
     *               TNAME, STAXYZ, ORBPRM, ANNUMB, MNTSTA, STAXOF,
     *               ANDIAM, ANFWHM, APLTYA, ANPLAA, ANPLCA, APLTYB,
     *               ANPLAB, ANPLCB, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1090) IERR, IVER
                     GO TO 990
                     END IF
                  GO TO 50
                  END IF
 40            CONTINUE
 50         CONTINUE
C                                       Close down current AN table
            CALL TABIO ('CLOS', 1, IANRNO, TABUF1, TABUF1, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1070) IERR, IVER
               GO TO 990
               END IF
 100     CONTINUE
C                                       Close down AT table
      CALL TABIO ('CLOS', 1, IATRNO, TABUF2, TABUF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1080) IERR
         GO TO 990
         END IF
C                                       Update CATBLK
      CALL CATIO ('UPDT', DISK, CNO, CATIEQ, 'REST', TABUF1, IERR)
      CALL COPY (256, CATIEQ, CATIN)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('AT2AN: ERROR ',I3,' OPENING AN TABLE # ',I4)
 1040 FORMAT ('AT2AN: ERROR ',I3,' OPENING AT TABLE')
 1050 FORMAT ('AT2AN: ERROR ',I3,' READING AN TABLE # ',I4)
 1060 FORMAT ('AT2AN: ERROR ',I3,' READING AT TABLE')
 1070 FORMAT ('AT2AN: ERROR ',I3,' CLOSING AN TABLE # ',I4)
 1080 FORMAT ('AT2AN: ERROR ',I3,' CLOSING AT TABLE')
 1090 FORMAT ('AT2AN: ERROR ',I3,' UPDATING AN TABLE # ',I4)
 1100 FORMAT ('AT2AN: ERROR ',I3,' UPDATING AN KEYWORDS')
      END
      SUBROUTINE CTUPDT (DISK, ICNO, LUN, TR, CATIN, BUFF, IERR)
C-----------------------------------------------------------------------
C   Updates the end of the CT table to have the time range
C   Inputs:
C      DISK   I        Disk
C      ICNO   I        Catalog number
C      LUN    I        Logical unit number to use
C      TR     D(2)     Time range
C      CATIN  I(256)   header
C   Outputs
C      BUFF   I(512)   Table buffer to use
C      IERR   I        Error code
C-----------------------------------------------------------------------
      INTEGER   DISK, ICNO, LUN, CATIN(256), BUFF(512), IERR
      DOUBLE PRECISION TR(2)
C
      INCLUDE 'INCS:DCTV.INC'
      INTEGER   VER, ICTRNO, CTKOLS(MAXCTC), CTNUMV(MAXCTC), NREC, IREC
      CHARACTER UT1TYP*1, WOBTYP*1
      DOUBLE PRECISION TIME, UT1UTC, IATUTC, A1IAT, WOBXY(2), DPSI,
     *   DDPSI, DEPS, DDEPS, TRANGE(2), PTIME
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       open file
      VER = 1
      CALL CTINI ('WRIT', BUFF, DISK, ICNO, VER, CATIN, LUN, ICTRNO,
     *   CTKOLS, CTNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'INIT', IERR
         GO TO 990
         END IF
      NREC = BUFF(5)
C                                       read backwards
      DO 20 IREC = 1,NREC
         ICTRNO = NREC + 1 - IREC
         CALL TABCT ('READ', BUFF, ICTRNO, CTKOLS, CTNUMV, TIME, UT1UTC,
     *      IATUTC, A1IAT, UT1TYP, WOBXY, WOBTYP, DPSI, DDPSI, DEPS,
     *      DDEPS, TRANGE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) 'READ', IERR
            GO TO 990
            END IF
         IF (IREC.EQ.1) PTIME = TIME
C                                       time to quit?
         IF ((PTIME.LT.TIME) .OR. (TRANGE(2).GT.TRANGE(1))) GO TO 30
         TRANGE(1) = TR(1)
         TRANGE(2) = TR(2)
         PTIME = TIME
C                                       re-write rec
         ICTRNO = NREC + 1 - IREC
         CALL TABCT ('WRIT', BUFF, ICTRNO, CTKOLS, CTNUMV, TIME, UT1UTC,
     *      IATUTC, A1IAT, UT1TYP, WOBXY, WOBTYP, DPSI, DDPSI, DEPS,
     *      DDEPS, TRANGE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) 'WRIT', IERR
            GO TO 990
            END IF
 20      CONTINUE
C                                       close
 30   ICTRNO = NREC
      CALL TABCT ('CLOS', BUFF, ICTRNO, CTKOLS, CTNUMV, TIME, UT1UTC,
     *   IATUTC, A1IAT, UT1TYP, WOBXY, WOBTYP, DPSI, DDPSI, DEPS,
     *   DDEPS, TRANGE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'CLOS', IERR
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CTUPDT: DOING ',A,' ERROR',I5)
      END
      SUBROUTINE FR2FQ (DISK, ICNO, LUN, CATIN, IERR)
C-----------------------------------------------------------------------
C   Routine which creates a FQ table from the contents of the more
C   general FR VLBA interchange tables.
C   Inputs:
C      DISK    I        Volume on which data reside
C      ICNO    I        Catalogue number of data
C      LUN     I        Main lun to use.
C      CATIN   I(256)   Catalogue header.
C   Outputs:
C      IERR    I        Error code, 0 => OK; anything else => problem
C   Note:  Routine uses lun 46 during its operation, that lun is freed
C   upon exit.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER    DISK, ICNO, LUN, CATIN(256), IERR
C
      INCLUDE 'FITLD.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'ORDER.INC'
      INTEGER   I, J, IVER, SIGN, FRORD, LOWIF, FTEMP(MAXIF,MAXFQI),
     *   NENTRY, LUN2, NUMFQ, NFQENT, II, FQIFNC, NIFFR
      DOUBLE PRECISION REFF, NREFF
      LOGICAL   MATCH, FQEXIS, COMPAR
      CHARACTER BNDCOD(MAXIF)*8
C                                       Declerations for FQ
      INTEGER IFQRNO, FQKOLS(MAXFQC), FQNUMV(MAXFQC), LFQID,
     *   IFSIDE(MAXIF), T4(MAXIF), OFQID, OFQRNO
      REAL    IFCHW(MAXIF), IFTBW(MAXIF), T2(MAXIF), T3(MAXIF)
      DOUBLE PRECISION IFFREQ(MAXIF), T1(MAXIF), DTMP
      DOUBLE PRECISION DEPS
      REAL             REPS
C                                       Maximum relative spacings
C                                       of IEEE 64- and 32-bit
C                                       floating point numbers
C                                       used in frequency comparisons
      PARAMETER (DEPS = 2.2204460492503131D-16)
      PARAMETER (REPS = 1.192092896E-7)
C
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'INCS:DFRV.INC'
      DATA BNDCOD /MAXIF * ' '/
C-----------------------------------------------------------------------
      CALL COPY (256, CATIN, CATIEQ)
      REFF = CATDEQ(KDCRV+JLOCF)
      LUN2 = 46
      NIFFR = EIF - BIF + 1
C                                       Open FR table
      IVER = 0
      CALL FRINI ('READ', TABUF2, DISK, ICNO, IVER, CATIEQ, LUN2,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Number of entries
      NENTRY = TABUF2(5)
C                                       Does FQ table already exist?
      CALL FNDEXT ('FQ', CATIEQ, NUMFQ)
      FQEXIS = NUMFQ.GT.0
C                                       Insert variables into
C                                       catalogue header if they
C                                       don't already exist, if
C                                       they do then just compare them.
      REFOFF = 0.D0
      CALL CATMAT (CATIEQ, MATCH)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1040) 'FR'
         CALL MSGWRT (6)
         END IF
C                                       Create/Open FQ table
      IVER = 1
      CALL FQINI ('WRIT', TABUF1, DISK, ICNO, IVER, CATIEQ, LUN,
     *   IFQRNO, FQKOLS, FQNUMV, NIFFR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                       Number of FQ entries
      NFQENT = TABUF1(5)
C                                       Copy entries if new FQ table
      IF (.NOT.FQEXIS) THEN
C                                       Find offset
         DO 20 I = 1,NENTRY
            IFRRNO = I
            CALL FRTAB ('READ', TABUF2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020) IERR
               GO TO 990
               END IF
C                                       Freq. ID selection, renumber
            LFQID = FQIFNC (IFQID)
            IF (LFQID.EQ.1) THEN
               DO 10 J = 1,NIFFR
                  FRORD = J
                  IF (REORDR(I)) FRORD = FORDER(J,I)
                  SIGN = 1
                  IF (ISIDEB(J).LT.0) SIGN = -1
                  IF (SIGN.LT.0) THEN
                     IFFREQ(FRORD) = BANDFR(J) + (2.0 * REFPIX - NOCHAN
     *                  - 1.0) * CHWID(J)
                  ELSE
                     IFFREQ(FRORD) = BANDFR(J)
                     END IF
 10               CONTINUE
C                                       Adjust so that lowest freq. is
C                                       now zero, since IF 1 is
C                                       reference IF
               DTMP = IFFREQ(1)
               GO TO 30
               END IF
 20         CONTINUE
C                                       now translate FR -> FQ
 30      DO 100 I = 1, NENTRY
            IFRRNO = I
            CALL FRTAB ('READ', TABUF2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020) IERR
               GO TO 990
               END IF
            DO 40 J = 1,NIFFR
               FRORD = J
               IF (REORDR(I)) FRORD = FORDER(J,I)
               SIGN = 1
               IF (ISIDEB(J).LT.0) SIGN = -1
               IF (SIGN.LT.0) THEN
                  IFFREQ(FRORD) = BANDFR(J) + (2.0 * REFPIX - NOCHAN -
     *               1.0) * CHWID(J)
               ELSE
                  IFFREQ(FRORD) = BANDFR(J)
                  END IF
               IFCHW(FRORD)  = CHWID(J)
               IFTBW(FRORD)  = TBANDW(J)
               IFSIDE(FRORD) = SIGN * ISIDEB(J)
               BNDCOD(FRORD) = ' '
 40            CONTINUE
C                                       Freq. ID selection, renumber
            LFQID = FQIFNC (IFQID)
            IF (LFQID.LT.1) GO TO 100
            IF (FQCOUN(LFQID).LE.0) GO TO 100
            DO 50 J = 1,NIFFR
               IFFREQ(J) = IFFREQ(J) - DTMP
 50            CONTINUE
C
            CALL TABFQ ('WRIT', TABUF1, IFQRNO, FQKOLS, FQNUMV, NIFFR,
     *         LFQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1030) IERR
               GO TO 990
               END IF
 100        CONTINUE
C                                       If FQ entry already exists
C                                       then we must compare entries,
C                                       and we may need to set up a new
C                                       FREQID
      ELSE IF (FQEXIS) THEN
         NREFF = TABRFQ
         OFQRNO = NFQENT + 1
C
         DO 200 I = 1, NENTRY
            IFRRNO = I
            CALL FRTAB ('READ', TABUF2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020) IERR
               GO TO 990
               END IF
            CALL DPCOPY (NIFFR, BANDFR, T1)
            CALL RCOPY (NIFFR, CHWID, T2)
            CALL RCOPY (NIFFR, TBANDW, T3)
            CALL COPY (NIFFR, ISIDEB, T4)
            DO 125 J = 1, NIFFR
               FRORD = J
               IF (REORDR(I)) FRORD = FORDER(J,I)
               SIGN = 1
               IF (T4(J).LT.0) SIGN = -1
               IF (SIGN.LT.0) THEN
                  T1(J) = T1(J) + (2.0 * REFPIX - NOCHAN - 1.0) *  T2(J)
               END IF
               BANDFR(FRORD) = T1(J) + NREFF
               CHWID(FRORD)  = T2(J)
               TBANDW(FRORD) = T3(J)
               ISIDEB(FRORD) = SIGN * T4(J)
  125          CONTINUE
C
            CALL DETORD (BANDFR, NIFFR, FTEMP, LOWIF)
C
            COMPAR = .FALSE.
            DO 150 II = 1, NFQENT
               IFQRNO = II
               CALL TABFQ ('READ', TABUF1, IFQRNO, FQKOLS, FQNUMV,
     *            NIFFR, LFQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD,
     *            IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1070) IERR
                  GO TO 990
                  END IF
C                                       Compare all variables
               DO 140 J = 1, NIFFR
                  IF (J.EQ.1) THEN
                    COMPAR = ABS (BANDFR(J) - (IFFREQ(J)+REFF))
     *                  .LE.8.0D0 * BANDFR(J) * DEPS
                  ELSE
                     COMPAR = COMPAR .AND.
     *                  (ABS (BANDFR(J) - (IFFREQ(J)+REFF))
     *                  .LE.8.0D0 * BANDFR(J) * DEPS)
                     END IF
                  COMPAR = COMPAR .AND.
     *               (ABS (CHWID(J) - IFCHW(J))
     *               .LE.2.0D0 * CHWID(J) * REPS)
                  COMPAR = COMPAR .AND.
     *                (ABS (TBANDW(J) - IFTBW(J))
     *                .LE.2.0D0 * TBANDW(J) * REPS)
                  COMPAR = COMPAR .AND. (ISIDEB(J).EQ.IFSIDE(J))
  140             CONTINUE
C                                       A match was made
               IF (COMPAR) GO TO 200
  150          CONTINUE
C                                       No match means we must add
C                                       another FQ entry to the existing
C                                       FQ table.
               DO 170 J = 1, NIFFR
                  IFFREQ(J) = BANDFR(J) - REFF
                  IFCHW(J)  = CHWID(J)
                  IFTBW(J)  = TBANDW(J)
                  IFSIDE(J) = ISIDEB(J)
  170          CONTINUE
C                                       Check whether we need it
               OFQID = FQIFNC (IFQID)
               IF (OFQID.LT.1) GO TO 200
               IF (FQCOUN(OFQID).LE.0) GO TO 200
               CALL TABFQ ('WRIT', TABUF1, OFQRNO, FQKOLS, FQNUMV,
     *            NIFFR, OFQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD,
     *            IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1080) IERR
                  GO TO 990
                  END IF
  200       CONTINUE
         END IF
C                                       Close tables
      CALL TABIO ('CLOS', 1, IFQRNO, TABUF1, TABUF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
      CALL TABIO ('CLOS', 1, IFRRNO, TABUF2, TABUF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
C                                       Update CATBLK
      CALL CATIO ('UPDT', DISK, ICNO, CATIEQ, 'REST', TABUF1, IERR)
      CALL COPY (256, CATIEQ, CATIN)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FR2FQ: ERROR ',I3,' OPENING FR TABLE # ',I4)
 1010 FORMAT ('FR2FQ: ERROR ',I3,' OPENING FQ TABLE # ',I4)
 1020 FORMAT ('FR2FQ: ERROR ',I3,' READING FR TABLE # ',I4)
 1030 FORMAT ('FR2FQ: ERROR ',I3,' WRITING FQ TABLE # ',I4)
 1040 FORMAT ('FR2FQ: ',A,' KEYWORDS DO NOT MATCH CAT. HEADER')
 1050 FORMAT ('FR2FQ: ERROR ',I3,' CLOSING FQ TABLE')
 1060 FORMAT ('FR2FQ: ERROR ',I3,' CLOSING FR TABLE')
 1070 FORMAT ('FR2FQ: ERROR ',I3,' READING FQ TABLE # ',I4)
 1080 FORMAT ('FR2FQ: ERROR ',I3,' ADDING TO FQ TABLE # ',I4)
      END
      SUBROUTINE SO2SU (DISK, CNO, LUN, BIF, EIF, OFFDAY, CATIN, IERR)
C-----------------------------------------------------------------------
C   Routine which generates an SU table from an SO table.
C   Inputs:
C      DISK    I        Volume on which data reside
C      CNO     I        Catalogue number of data
C      LUN     I        Main lun to use.
C      BIF     I        First IF selected
C      EIF     I        Last IF selected
C      OFFDAY  D        Day offset
C      CATIN   I(256)   Catalogue header.
C   Outputs:
C      IERR    I        Error code, 0 => OK; anything else => problem
C   Note:  Routine uses lun 46 during its operation, that lun is freed
C   upon exit.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION OFFDAY
      INTEGER    DISK, CNO, LUN, BIF, EIF, CATIN(256), IERR
C
      CHARACTER KEYWRD*8, CHTMP*8, C1*16, C2*16, CC1*4
      INTEGER   IVER, IFX, NCHAR, II, NSUEX, N1, N2, IFQ, LUN2, NSOU,
     *   ISOU, NUMKEY, KEYLOC, TYPKEY, SRCFNC, NSUWR, NSURNO, FRORD, J
      HOLLERITH HTEMP(2)
      LOGICAL   MATCH, COMPAR
C                                       Declarations for SOUINI
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC), ISURNO
C                                       Declarations for TABSOU
      CHARACTER SOUNAM*16, TCALC*4
      REAL      FLUX(4,MAXIF)
      INTEGER   IDSOU, TQUAL
      DOUBLE PRECISION  FREQO(MAXIF), LSRVEL(MAXIF), LRESTF(MAXIF),
     *   BANDW, TRAEPO, TDCEPO, TRAAPP, TDCAPP, TEQUIN, TPMRA, TPMDEC,
     *   DEPS, TRAOBS, TDECOB
      INTEGER   SUBAND
C                                       Julian day number of reference
C                                       date
      DOUBLE PRECISION JD0
C
      DOUBLE PRECISION OBSPOS(3)
      SAVE             OBSPOS
      REAL             POLAR(2)
      SAVE             POLAR
C
      INCLUDE 'DATSEL.INC'
      INCLUDE 'ORDER.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'INCS:DSOV.INC'
C
      DATA OBSPOS / 0.0, 0.0, 0.0 /
      DATA POLAR  / 0.0, 0.0 /
C-----------------------------------------------------------------------
      CALL COPY (256, CATIN, CATIEQ)
      LUN2 = 46
      DEPS = 1.0D0 / 3.6D7
      SUBAND = EIF - BIF + 1
C                                       Open SO table
      IVER = 1
      CALL SOINI ('READ', TABUF2, DISK, CNO, IVER, CATIEQ, LUN, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('WRIT', 'SOINI', 'SO2SU', IERR)
         GO TO 999
         ENDIF
C
      IF (CURJLD.LE.0.0D0) THEN
         CALL JULDAY (REFDAT, JD0)
         CURJLD = JD0
      ELSE
         JD0 = CURJLD
         END IF
      JD0 = JD0 + OFFDAY
C                                       Insert variables into
C                                       catalogue header if they
C                                       don't already exist, if
C                                       they do then just compare them.
      REFOFF = 0.D0
      CALL CATMAT (CATIEQ, MATCH)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1000) 'SO'
         CALL MSGWRT (6)
         END IF
C                                       Create/Open SU table
      IVER = 1
      CALL SOUINI ('WRIT', TABUF1, DISK, CNO, IVER, CATIEQ, LUN2,
     *   SUBAND, VELTYP, VELDEF, IFQ, ISURNO, SUKOLS, SUNUMV, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('READ', 'SOUINI', 'SO2SU', IERR)
         GO TO 999
         ENDIF
C                                       Number of sources
      NSOU = TABUF2(5)
      NSUEX = TABUF1(5)
      NSUWR = NSUEX
C                                       Mark pre-existing sources
      IF (NSUEX.GT.0) THEN
         DO 50 J = 1,NSUEX
            ISURNO = J
            CALL TABSOU ('READ', TABUF1, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *         SOUNAM, TQUAL, TCALC, FLUX, FREQO, BANDW, TRAEPO, TDCEPO,
     *         TEQUIN, TRAAPP, TDCAPP, TRAOBS, TDECOB, LSRVEL, LRESTF,
     *         TPMRA, TPMDEC, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('READ', 'TABSOU', 'SO2SU', IERR)
               GO TO 999
               END IF
            SOCOUN(IDSOU) = SOCOUN(IDSOU) + 1
 50         CONTINUE
         END IF

C                                       Get channel bandwidth
      BANDW = CATREQ(KRCIC+JLOCF)
C                                       Loop over sources and write
C                                       to SU table, if they do not
C                                       already exist.
      DO 100 ISOU = 1,NSOU
         COMPAR = .FALSE.
C                                       Read SO entries
         CALL SOTAB ('READ', TABUF2, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('WRIT', 'SOTAB', 'SO2SU', IERR)
            GO TO 999
            END IF
C                                       Source number changes?
         IF (SRCCHA) ISOUR = SRCFNC (ISOUR)
C                                       Write Su global keywords
C                                       read from first source in
C                                       more general SO table.
         IF (NSUEX.LE.0) THEN
            KEYWRD = 'VELTYP'
            NUMKEY = 1
            KEYLOC = 1
            TYPKEY = 3
            CALL CHTRIM (VELTYP, 8, CHTMP, NCHAR)
            CALL CHR2H (8, CHTMP, 1, HTEMP)
            CALL TABKEY ('WRIT', KEYWRD, NUMKEY, TABUF1, KEYLOC,
     *         HTEMP, TYPKEY, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR
               GO TO 999
               END IF
            KEYWRD = 'VELDEF'
            NUMKEY = 1
            KEYLOC = 1
            TYPKEY = 3
            CALL CHTRIM (VELDEF, 8, CHTMP, NCHAR)
            CALL CHR2H (8, CHTMP, 1, HTEMP)
            CALL TABKEY ('WRIT', KEYWRD, NUMKEY, TABUF1, KEYLOC, HTEMP,
     *         TYPKEY, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR
               GO TO 999
               END IF
            KEYWRD = 'FREQID'
            NUMKEY = 1
            KEYLOC = 1
            TYPKEY = 4
            CALL TABKEY ('WRIT', KEYWRD, NUMKEY, TABUF1, KEYLOC, FREQID,
     *         TYPKEY, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR
               GO TO 999
               END IF
            END IF
C                                      If SU table already exists
C                                      then compare and just write
C                                      new information
         CALL CHTRIM (SAUCE, 16, C1, N1)
         SAUCE = C1
         CALL CHTRIM (CALCOD, 4, CC1, N1)
         CALCOD = CC1
         COMPAR = .FALSE.
         IF (NSUEX.GT.0) THEN
            DO 40 II = 1,NSUEX
               ISURNO = II
               CALL TABSOU ('READ', TABUF1, ISURNO, SUKOLS, SUNUMV,
     *            IDSOU, SOUNAM, TQUAL, TCALC, FLUX, FREQO, BANDW,
     *            TRAEPO, TDCEPO, TEQUIN, TRAAPP, TDCAPP, TRAOBS,
     *            TDECOB, LSRVEL, LRESTF, TPMRA, TPMDEC, IERR)
               IF (IERR.NE.0) THEN
                  CALL TABERR ('READ', 'TABSOU', 'SO2SU', IERR)
                  GO TO 999
                  END IF
C                                       Compare name/qualifier
C                                       /calcode/id number/raepo
C                                       /decepo
               CALL CHTRIM (SOUNAM, 16, C2, N2)
               COMPAR = (C1.EQ.C2) .AND. (ABS(RAEPO-TRAEPO).LE.DEPS)
     *            .AND. (QUAL.EQ.TQUAL) .AND.
     *            (ABS(DECEPO-TDCEPO).LE.DEPS)
C                                       Warn if name and qualifier are
C                                       the same but the position
C                                       changes
               IF ((C1.EQ.C2) .AND. (QUAL.EQ.TQUAL) .AND.
     *            (.NOT. COMPAR)) THEN
                  WRITE (MSGTXT, 1037) IDSOU, ISOUR
                  CALL MSGWRT (6)
                  WRITE (MSGTXT, 1038) C1, QUAL
                  CALL MSGWRT (6)
                  WRITE (MSGTXT, 1039)
                  CALL MSGWRT (6)
                  END IF
               IF ((COMPAR) .AND. (IDSOU.NE.ISOUR)) THEN
                  WRITE (MSGTXT,1040) C1, QUAL
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,1041) ISOUR, IDSOU
                  CALL MSGWRT (6)
                  COMPAR = .FALSE.
               ELSE IF ((.NOT.COMPAR) .AND. (IDSOU.EQ.ISOUR)) THEN
                  WRITE (MSGTXT,1042) C1, QUAL, C2, TQUAL
                  CALL MSGWRT (8)
                  WRITE (MSGTXT,1043) ISOUR
                  CALL MSGWRT (8)
                  END IF
               IF (COMPAR) GO TO 100
 40            CONTINUE
            END IF
C                                       If no match, or Su table did not
C                                       exist then write the new entry
         IF (.NOT.COMPAR) THEN
C                                       Precess coordinates of epoch to
C                                       approximate apparent coordinates
C                                       if it looks as if this has not
C                                       been done already and there is
C                                       a good chance that a Julian
C                                       epoch is being used:
C
            IF ((ABS (RAEPO - RAAPP).LE.DEPS) .AND.
     *          (ABS (DECEPO - DECAPP).LE.DEPS) .AND.
     *          (EQUINX.GE.2000.0D0)) THEN
               CALL JPRECS (JD0, EQUINX, 1.0D-6, 1, .TRUE., OBSPOS,
     *            POLAR, DG2RAD * RAEPO, DG2RAD * DECEPO, RAAPP, DECAPP)
               RAAPP = RAD2DG * RAAPP
               DECAPP = RAD2DG * DECAPP
               IF (RAAPP.LT.0.0D0) RAAPP = RAAPP + 360.0D0
               END IF

C                                       Fill flux density arrays.
            DO 80 IFX = 1,SUBAND
               FRORD = IFX
               IF (REORDR(FREQID)) FRORD = FORDER(IFX,FREQID)
               FLUX(1,FRORD) = IFLUX(IFX)
               FLUX(2,FRORD) = QFLUX(IFX)
               FLUX(3,FRORD) = UFLUX(IFX)
               FLUX(4,FRORD) = VFLUX(IFX)
               FREQO(FRORD) = FRQOFF(IFX)
               LSRVEL(FRORD) = SYSVEL(IFX)
               LRESTF(FRORD) = RSTFRQ(IFX)
 80            CONTINUE
            IDSOU = ISOUR
            TQUAL = QUAL
            SOUNAM = SAUCE
            TCALC = CALCOD
            TRAEPO = RAEPO
            TDCEPO = DECEPO
            TEQUIN = EQUINX
            TRAAPP = RAAPP
            TDCAPP = DECAPP
            TRAOBS = RAOBS
            TDECOB = DECOBS
            TPMRA = PMRA
            TPMDEC = PMDEC
            NSUWR = NSUWR + 1
            NSURNO = NSUWR
C
            CALL TABSOU ('WRIT', TABUF1, NSURNO, SUKOLS, SUNUMV, IDSOU,
     *         SOUNAM, TQUAL, TCALC, FLUX, FREQO, BANDW, TRAEPO, TDCEPO,
     *         TEQUIN, TRAAPP, TDCAPP, TRAOBS, TDECOB, LSRVEL, LRESTF,
     *         TPMRA, TPMDEC, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('READ', 'TABSOU', 'SO2SU', IERR)
               GO TO 999
               END IF
            NSUEX = NSUEX + 1
            END IF
C
  100    CONTINUE
C                                       Close down SU table
      CALL TABIO ('CLOS', 1, ISURNO, TABUF1, TABUF1, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'SO2SU', IERR)
         GO TO 999
         ENDIF
C                                       Close down SO table
      CALL TABIO ('CLOS', 1, ISORNO, TABUF2, TABUF2, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'SO2SU', IERR)
         GO TO 999
         ENDIF
C                                       Update CATBLK
      CALL CATIO ('UPDT', DISK, CNO, CATIEQ, 'REST', TABUF1, IERR)
      CALL COPY (256, CATIEQ, CATIN)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SO2SU: ',A,' KEYWORDS DO NOT MATCH CAT. HEADER')
 1010 FORMAT ('SO2SU: ERROR ',I3,' UPDATING SU KEYWORDS')
 1037 FORMAT ('Warning: source numbers ', I4, ' and ', I4, ' both have')
 1038 FORMAT ('         name ', A, ' and qualifier ', I5, ' but')
 1039 FORMAT ('         have different positions')
 1040 FORMAT ('Warning: source ''',A,'''',I5,' appears under')
 1041 FORMAT ('         source numbers',2I5)
 1042 FORMAT ('ERROR: ''',A,'''',I7,' and ''',A,'''',I7)
 1043 FORMAT ('HAVE THE SAME SOURCE NUMBER =',I6)
      END
      SUBROUTINE IM2CL (DISK, CNO, LUN, NUMBND, CATIN, IERR)
C-----------------------------------------------------------------------
C   Routine which transfers contents of IM tables to CL tables if
C   possible. Copies the IM table first, then deletes the original and
C   restores the copy.  It also inserts some info in the AN tables.
C   Inputs:
C      DISK     I        Volume on which data reside
C      CNO      I        Catalogue number of data
C      LUN      I        Main lun to use.
C      NUMBND   I        Number of IFs
C      CATIN    I(256)   Catalogue header.
C   Inputs from common:
C      CONTIM   R(2)     Start, stop time of tables to be updated
C    Outputs:
C      IERR     I        Error code, 0 => OK, anything else => problem
C   Note:  Routine uses lun 46 during its operation, that lun is freed
C   upon exit.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DISK, CNO, LUN, NUMBND, CATIN(256), IERR
C
      CHARACTER KEYWRD*8
      INTEGER   NUMIMT, IVER, NUMKEY, KEYLOC, TYPKEY, NENTRY, IENT,
     *   LUN2, I, NCLENT, IIF, NUMREC
      DOUBLE PRECISION KEPREF
      LOGICAL   MATCH
C                                       Declerations for CALINI
      INTEGER   ICLRNO, CLKOLS(MAXCLC), CLNUMV(MAXCLC), CLANT, CLPOL,
     *   CLIF, CLTERM
      REAL      GMMOD
C                                       Declerations for TABCAL
      INTEGER   REFA(2,MAXIF), CLSTA, CLFQID, CLARR, CLSRC, IFAIL
      REAL      CLTINT, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2),
     *   CLOCK(2), DCLOCK(2), DISP(2), DDISP(2), CREAL(2,MAXIF),
     *   CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF)
      DOUBLE PRECISION GEODLY(10), CLTIME
C
      INCLUDE 'DATSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'INCS:DIMV.INC'
C-----------------------------------------------------------------------
      CALL COPY (256, CATIN, CATIEQ)
      LUN2 = 46
      KEPREF = REFFRQ
C                                       reorganize and edit IM table
      CALL IMORDR (DISK, CNO, LUN, NUMBND, CATIEQ, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Determine # tables
      NUMIMT= 1
      CALL FNDEXT ('IM', CATIEQ, NUMIMT)
      IF (NUMIMT.EQ.0) GO TO 999
      NUMREC = 0
      IFAIL = 0
C                                       Loop over tables
      DO 50 I = 1,NUMIMT
         IVER = I
C                                       Open IM table
         CALL IMINI ('READ', TABUF2, DISK, CNO, IVER, CATIEQ, LUN, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN', 'IM', I
            GO TO 990
            END IF
         NENTRY = TABUF2(5)
C                                       Insert variables into catalogue
C                                       header if they don't already
C                                       exist, else just compare them.
         IF (I.EQ.1) THEN
            REFOFF = 0.D0
            REFFRQ = KEPREF
            CALL CATMAT (CATIEQ, MATCH)
            IF (.NOT.MATCH) THEN
               WRITE (MSGTXT,1120) 'IM'
               CALL MSGWRT (6)
               END IF
            END IF
C                                       Open CL table
         IF (.NOT.GOTSUB) THEN
            CLPOL = NOPOLZ
            CLTERM = NPOLY
            CALL CALINI ('WRIT', TABUF1, DISK, CNO, IVER, CATIEQ, LUN2,
     *         ICLRNO, CLKOLS, CLNUMV, CLANT, CLPOL, CLIF, CLTERM,
     *         GMMOD, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN', 'CL', IVER
               GO TO 990
               END IF
            NCLENT = TABUF1(5)
C                                       Update polynomial order
            KEYWRD = 'NO_TERM'
            NUMKEY = 1
            KEYLOC = 1
            TYPKEY = 4
            CALL TABKEY ('WRIT', KEYWRD, NUMKEY, TABUF1, KEYLOC, NPOLY,
     *         TYPKEY, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR
               GO TO 990
               END IF
C                                       Loop over entries
            DO 40 IENT = 1,NCLENT
C                                       Read current CL entry
               ICLRNO = IENT
               CALL TABCAL ('READ', TABUF1, ICLRNO, CLKOLS, CLNUMV,
     *            CLPOL, CLIF, CLTIME, CLTINT, CLSRC, CLSTA, CLARR,
     *            CLFQID, IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *            CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *            WEIGHT, REFA, IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ', 'CL', IVER
                  GO TO 990
               ELSE IF (IERR.LT.0) THEN
                  IFAIL = IFAIL + 1
                  END IF
C                                       If within time range get
C                                       interpolated delay polynomial
               IF ((CLTIME.GE.CONTIM(1)) .AND. (CLTIME.LE.CONTIM(2)))
     *            THEN
                  CALL GETDEL (CLTIME, CLSRC, CLSTA, CLARR, CLFQID,
     *               TABUF2, GEODLY, DISP, DDISP, NUMREC, IERR)
                  IF (IERR.GT.0) THEN
                     WRITE (MSGTXT,1090) IERR
                     CALL MSGWRT (6)
                     IERR = 0
                     GO TO 40
                     END IF
C                                       Force dispersive delays to zero
C                                       They are applied by the cal
C                                       routines so we cannot keep
C                                       whatever was already applied
                  DISP(1) = 0.0
                  DISP(2) = 0.0
                  DDISP(1) = 0.0
                  DDISP(2) = 0.0
C                                       rearrange format of some
C                                       variables
                  DO 30 IIF = 1,CLIF
                     DOPOFF(IIF) = FREQVR(IIF) + REFOFF
                     IF (DOPOFF(IIF).LT.10000.0) DOPOFF(IIF) = 0.0
 30                  CONTINUE
C                                       Rewrite current CL entry
                  ICLRNO = IENT
                  CALL TABCAL ('WRIT', TABUF1, ICLRNO, CLKOLS, CLNUMV,
     *               CLPOL, CLIF, CLTIME, CLTINT, CLSRC, CLSTA, CLARR,
     *               CLFQID, IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *               CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY,
     *               RATE, WEIGHT, REFA, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1000) IERR, 'WRIT', 'CL', IVER
                     GO TO 990
                     END IF
                  END IF
 40            CONTINUE
C                                       Close down current CL table
            CALL TABIO ('CLOS', 1, ICLRNO, TABUF1, TABUF1, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOS', 'CL', IVER
               GO TO 990
               END IF
            END IF
C                                       Close down IM table
         CALL TABIO ('CLOS', 1, IIMRNO, TABUF2, TABUF2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'CLOS', 'IM', I
            GO TO 990
            END IF
 50      CONTINUE
C                                       Update CATBLK
      CALL CATIO ('UPDT', DISK, CNO, CATIEQ, 'REST', TABUF1, IERR)
      CALL COPY (256, CATIEQ, CATIN)
      IF (IFAIL.GT.0) THEN
         WRITE (MSGTXT,1050) IFAIL
         CALL MSGWRT (7)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IM2CL: ERROR',I4,1X,A,'ING ',A,' TABLE 3',I4)
 1010 FORMAT ('IM2CL: ERROR ',I3,' UPDATING CL KEYWORDS')
 1050 FORMAT ('IM2CL: WARNING',I10,' CL RECORDS DID NOT GET DELAY',
     *   ' POLYNOMIAL')
 1090 FORMAT ('IM2CL: ERROR ',I3,' DETERMINING CL GEOM. DELAY')
 1120 FORMAT ('IM2CL: ',A,' KEYWORDS DO NOT MATCH CAT. HEADER')
      END
      SUBROUTINE MC2CL (DISK, CNO, LUN, CATIN, IERR)
C-----------------------------------------------------------------------
C   Routine which transfers contents of MC tables to CL tables if
C   possible. Copies the MC table first, then deletes the original and
C   restores the copy.  It also inserts some info in the AN tables.
C   Inputs:
C      DISK     I        Volume on which data reside
C      CNO      I        Catalogue number of data
C      LUN      I        Main lun to use.
C      CATIN    I(256)   Catalogue header.
C   Inputs from common:
C      CONTIM   R(2)     Start, stop time of tables to be updated
C    Outputs:
C      IERR     I        Error code, 0 => OK, anything else => problem
C   Note:  Routine uses lun 46 during its operation, that lun is freed
C   upon exit.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DISK, CNO, LUN, CATIN(256), IERR
C
      INTEGER   NUMMCT, IVER, NENTRY, IENT, LUN2, I, NCLENT
      DOUBLE PRECISION KEPREF
      LOGICAL   MATCH
C                                       Declerations for CALINI
      INTEGER   ICLRNO, CLKOLS(MAXCLC), CLNUMV(MAXCLC), CLANT, CLPOL,
     *   CLIF, CLTERM, IFAIL
      REAL      GMMOD
C                                       Declerations for TABCAL
      INTEGER   REFA(2,MAXIF), CLSTA, CLFQID, CLARR, CLSRC
      REAL      CLTINT, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2),
     *   CLOCK(2), DCLOCK(2), DISP(2), DDISP(2), CREAL(2,MAXIF),
     *   CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF),
     *   IFR
      DOUBLE PRECISION CLTIME, GEODLY(10)
C                                       Declarations for MC table
      CHARACTER OBCOD*8, RDATE*8, TAPER*8
      INTEGER   MCROW, NSTOKE, STOKE1, NUMIF, NCHAN, NUMPOL, FFTSIZ,
     *   OVRSMP, ZEROPD, MCKOLS(21), MCNUMV(21), NUMREC
      REAL      CHANBW, RPIX, DELTAT
      DOUBLE PRECISION RFREQ
C
      INCLUDE 'DATSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TABLES.INC'
C-----------------------------------------------------------------------
      CALL COPY (256, CATIN, CATIEQ)
      LUN2 = 46
      KEPREF = REFFRQ
C                                       reorganize and edit IM table
      CALL MCORDR (DISK, CNO, CATIEQ, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Determine # tables
      NUMMCT= 1
      CALL FNDEXT ('MC', CATIEQ, NUMMCT)
      IF (NUMMCT.EQ.0) GO TO 999
      NUMREC = 0
      IFAIL = 0
C                                       Loop over tables
      DO 50 I = 1,NUMMCT
         IVER = I
C                                       Open IM table
         CALL MCINI ('READ', TABUF2, DISK, CNO, IVER, CATIEQ, LUN,
     *      MCROW, MCKOLS, MCNUMV, OBCOD, RDATE, NSTOKE, STOKE1, NUMIF,
     *      NCHAN, RFREQ, CHANBW, RPIX, NUMPOL, FFTSIZ, OVRSMP,
     *      ZEROPD, TAPER, DELTAT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN', 'MC', I
            GO TO 990
            END IF
         NENTRY = TABUF2(5)
C                                       Insert variables into catalogue
C                                       header if they don't already
C                                       exist, else just compare them.
         IF (I.EQ.1) THEN
            REFOFF = 0.D0
            REFFRQ = KEPREF
            CALL CATMAT (CATIEQ, MATCH)
            IF (.NOT.MATCH) THEN
               WRITE (MSGTXT,1120) 'MC'
               CALL MSGWRT (6)
               END IF
            END IF
C                                       Open CL table
         IF (.NOT.GOTSUB) THEN
            CLPOL = NUMPOL
            CLTERM = 6
            CALL CALINI ('WRIT', TABUF1, DISK, CNO, IVER, CATIEQ, LUN2,
     *         ICLRNO, CLKOLS, CLNUMV, CLANT, CLPOL, CLIF, CLTERM,
     *         GMMOD, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN', 'CL', IVER
               GO TO 990
               END IF
            NCLENT = TABUF1(5)
C                                       Loop over entries
            DO 40 IENT = 1,NCLENT
C                                       Read current CL entry
               ICLRNO = IENT
               CALL TABCAL ('READ', TABUF1, ICLRNO, CLKOLS, CLNUMV,
     *            CLPOL, CLIF, CLTIME, CLTINT, CLSRC, CLSTA, CLARR,
     *            CLFQID, IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *            CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *            WEIGHT, REFA, IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ', 'CL', IVER
                  GO TO 990
                  END IF
C                                       If within time range get clock
C                                       offsets and atmospheric delay
               IF ((CLTIME.GE.CONTIM(1)) .AND. (CLTIME.LE.CONTIM(2)))
     *            THEN
                  CALL GETMC (TABUF2, MCKOLS, MCNUMV, DELTAT, CLTIME,
     *               CLSRC, CLSTA, CLARR, CLFQID, CLOCK, DCLOCK, ATMOS,
     *               DATMOS, NUMREC, IERR)
                  IF (IERR.GT.0) THEN
                     WRITE (MSGTXT,1090) IERR
                     CALL MSGWRT (6)
                     IERR = 0
                     GO TO 40
                  ELSE IF (IERR.LT.0) THEN
                     IFAIL = IFAIL + 1
                     END IF
C                                       Rewrite current CL entry
                  ICLRNO = IENT
                  CALL TABCAL ('WRIT', TABUF1, ICLRNO, CLKOLS, CLNUMV,
     *               CLPOL, CLIF, CLTIME, CLTINT, CLSRC, CLSTA, CLARR,
     *               CLFQID, IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *               CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY,
     *               RATE, WEIGHT, REFA, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1000) IERR, 'WRIT', 'CL', IVER
                     GO TO 990
                     END IF
                  END IF
 40            CONTINUE
C                                       Close down current CL table
            CALL TABIO ('CLOS', 1, ICLRNO, TABUF1, TABUF1, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOS', 'CL', IVER
               GO TO 990
               END IF
            END IF
C                                       Close down IM table
         CALL TABIO ('CLOS', 1, MCROW, TABUF2, TABUF2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'CLOS', 'MC', I
            GO TO 990
            END IF
 50      CONTINUE
C                                       Update CATBLK
      CALL CATIO ('UPDT', DISK, CNO, CATIEQ, 'REST', TABUF1, IERR)
      CALL COPY (256, CATIEQ, CATIN)
      IF (IFAIL.GT.0) THEN
         WRITE (MSGTXT,1050) IFAIL
         CALL MSGWRT (7)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MC2CL: ERROR',I4,1X,A,'ING ',A,' TABLE #',I4)
 1050 FORMAT ('MC2CL: WARNING',I10,' CL RECORDS DID NOT GET CLOCK AND',
     *   ' ATMOSPHERE')
 1090 FORMAT ('MC2CL: ERROR ',I3,' DETERMINING CL GEOM. DELAY')
 1120 FORMAT ('MC2CL: ',A,' KEYWORDS DO NOT MATCH CAT. HEADER')
      END
      SUBROUTINE BC2BL (DISK, CNO, LUN, CATIN, IERR)
C-----------------------------------------------------------------------
C   Routine which generates a BL table from a BC table.
C     Note that a BC table can have 4 polarizations but a BL table has
C     only 2.
C   Inputs:
C      DISK    I        Volume on which data reside
C      CNO     I        Catalogue number of data
C      LUN     I        Main lun to use.
C      CATIN   I(256)   Catalogue header.
C   Outputs:
C      IERR    I        Error code, 0 => OK, anything else => problem
C   Note:  Routine uses lun 46 during its operation, that lun is freed
C   upon exit.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DISK, CNO, LUN, CATIN(256), IERR
C
      INTEGER   NUMBCT, I, J, IVER, NENTRY, IENT, LUN2, ANTFNC, SRCFNC,
     *   FQIFNC, OUTFQI
      LOGICAL   MATCH, WANSRC, WANTIM
C                                       Declarations for BLINI
      INTEGER   BLKOLS(MAXBLC), BLNUMV(MAXBLC), IBLRNO, BLANT,
     *   BLPOL, BLIF
C                                       Declarations for TABBL
      INTEGER   ANT1, ANT2, IPT
      REAL      FACMUL(2,2,MAXIF), FACADD(2,2,MAXIF)
      INCLUDE 'DATSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'INCS:DBCV.INC'
C-----------------------------------------------------------------------
      CALL COPY (256, CATIN, CATIEQ)
      LUN2 = 46
      MSGTXT = 'BC2BL: USING UNTESTED ROUTINE: SELECTION NOT' //
     *   ' IMPLEMENTED'
      CALL MSGWRT (8)
C                                       Determine # tables
      NUMBCT = 1
      CALL FNDEXT ('BC', CATIEQ, NUMBCT)
      IF (NUMBCT.EQ.0) GO TO 999
C                                       Loop over tables
C                                       write BL table
      DO 100 I = 1,NUMBCT
         IVER = I
C                                       Open BC table
         CALL BCINI ('READ', TABUF2, DISK, CNO, IVER, CATIEQ, LUN,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, IVER
            GO TO 990
            END IF
         NENTRY = TABUF2(5)
C                                       Insert variables into
C                                       catalogue header if they
C                                       don't already exist, if
C                                       they do then just compare them.
         IF (I.EQ.1) THEN
            REFOFF = 0.D0
            CALL CATMAT (CATIEQ, MATCH)
            IF (.NOT.MATCH) THEN
               WRITE (MSGTXT,1040) 'BC'
               CALL MSGWRT (6)
               END IF
            END IF
C                                       Copy necessary variables
         BLANT = NOANT
         BLPOL = MIN (2, NOSTKD)
C                                       Create and open BL table
         CALL BLINI ('WRIT', TABUF1, DISK, CNO, IVER, CATIEQ, LUN2,
     *      IBLRNO, BLKOLS, BLNUMV, BLANT, BLPOL, BLIF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, IVER
            GO TO 990
            END IF
C                                       Loop over entries
         DO 50 IENT = 1, NENTRY
C                                       Read BC entries
            CALL BCTAB ('READ', TABUF2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1030) IERR, IVER
               GO TO 990
               END IF
C                                       Source number changes?
            IF (SRCCHA) ISRC = SRCFNC (ISRC)
C                                       Do we want this source?
            IF (.NOT.WANSRC(ISRC)) GO TO 50
C                                       Timerange selection?
            IF (.NOT.WANTIM(TIME)) GO TO 50
            IF (TIME.GT.TEND) GO TO 60
C                                       Antenna number changes?
            IF (ANTCHA) THEN
               ANTS(1) = ANTFNC (ANTS(1), IARRAY)
               ANTS(2) = ANTFNC (ANTS(2), IARRAY)
               END IF
C                                       Copy necessary variables
            ANT1 = ANTS(1)
            ANT2 = ANTS(2)
            DO 40 J = 1,BLIF
               IPT = 1 + (J-1) * NOSTKD
               FACMUL(1,1,J) = RLMUL(IPT)
               FACMUL(2,1,J) = IMMUL(IPT)
               FACADD(1,1,J) = RLADD(IPT)
               FACADD(2,1,J) = IMADD(IPT)
               IF ((BLPOL.GT.1) .AND. (NOSTKD.GT.1)) THEN
                  FACMUL(1,2,J) = RLMUL(IPT+1)
                  FACMUL(2,2,J) = IMMUL(IPT+1)
                  FACADD(1,2,J) = RLADD(IPT+1)
                  FACADD(2,2,J) = IMADD(IPT+1)
               ELSE
                  FACMUL(1,2,J) = 1.0
                  FACMUL(2,2,J) = 0.0
                  FACADD(1,2,J) = 0.0
                  FACADD(2,2,J) = 0.0
                  END IF
 40            CONTINUE
C
C                                       Change FQID
            OUTFQI = FQIFNC(IFQID)
            IF (OUTFQI.LT.1) GO TO 50
C
            CALL TABBL ('WRIT', TABUF1, IBLRNO, BLKOLS, BLNUMV, BLPOL,
     *         TIME, ISRC, IARRAY, ANT1, ANT2, OUTFQI, FACMUL, FACADD,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020) IERR, IVER
               GO TO 990
               END IF
 50         CONTINUE
C                                       Close down current BL/BC
C                                       tables
 60      CALL TABIO ('CLOS', 1, IBLRNO, TABUF1, TABUF1, IERR)
         CALL TABIO ('CLOS', 1, IBCRNO, TABUF2, TABUF2, IERR)
 100     CONTINUE
C                                       Update CATBLK
      CALL CATIO ('UPDT', DISK, CNO, CATIEQ, 'REST', TABUF1, IERR)
      CALL COPY (256, CATIEQ, CATIN)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BC2BL: ERROR ',I3,' OPENING BL TABLE # ',I4)
 1010 FORMAT ('BC2BL: ERROR ',I3,' OPENING BC TABLE # ',I4)
 1020 FORMAT ('BC2BL: ERROR ',I3,' WRITING BL TABLE # ',I4)
 1030 FORMAT ('BC2BL: ERROR ',I3,' READING BC TABLE # ',I4)
 1040 FORMAT ('BC2BL: ',A,' KEYWORDS DO NOT MATCH CAT. HEADER')
      END
      SUBROUTINE FRQORD (DISK, ISLOT, IVER, CATIN, LUN, IERR)
C-----------------------------------------------------------------------
C   This routine reads the FR table just created and determines whether
C   a sideband is LSB or USB, generates a list of sidebands which are
C   LSB and determines the frequency order. It also checks to see
C   whether there are duplicate FQ entries and if so issues a warning.
C   Inputs:
C      DISK     I    Volume on which data reside
C      ISLOT    I    Catalogue number of data file
C      IVER     I    version number of FR file
C      CATIN    I    Catalogue header for data file
C      LUN      I    LUN
C   Outputs:
C      IERR     I    Error code. 0=ok.
C   Output in common
C      DATSID   I(MAXIF,MAXFQI)   List of sidebands as fn of FQID,
C                                 -1 => LSB, +1 => USB
C      LSBPRS   L   True if LSB present
C-----------------------------------------------------------------------
      INTEGER   DISK, ISLOT, IVER, LUN, CATIN(256), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ORDER.INC'
      INCLUDE 'DATSEL.INC'
      DOUBLE PRECISION HIGHF, LOWF, REFQ, BBCFRQ(MAXIF), FTEMP,
     *   SKYFRQ(MAXIF,MAXFQI), TBBC(MAXIF,MAXFQI)
      REAL   XBW(MAXIF,MAXFQI), XCHW(MAXIF,MAXFQI)
      INTEGER   FVER, NENTRY, I, J, K, IORD, NMBAND, IT, L,
     *   LOWIF(MAXFQI), DONE(MAXFQI)
      LOGICAL   WRTFRQ, MATCH
      CHARACTER LINE*80
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DFRV.INC'
      INCLUDE 'TABLES.INC'
C-----------------------------------------------------------------------
      IERR = 0
      LSBPRS = .FALSE.
      CALL COPY (256, CATIN, CATIEQ)
      REFQ = CATDEQ(KDCRV+JLOCF)
      IT = MAXIF*MAXFQI
      CALL DFILL (IT, 0.D0, SKYFRQ)
      ADJRFQ = 1.0D20
C                                           Are there any FR tables yet?
      CALL FNDEXT ('FR', CATIN, FVER)
      IF (FVER.EQ.0) THEN
         IERR = 1
         WRITE (MSGTXT,1000)
         GO TO 990
         END IF
C                                           Read the table
      CALL FRINI ('READ', TABUF1, DISK, ISLOT, IVER, CATIN, LUN, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('READ', 'FRINI', 'FRQORD', IERR)
         GO TO 990
         END IF
      NENTRY = TABUF1(5)
      NMBAND = FRNUMV(KBFQ)
      IORD = 1
      HIGHF = 1.0D15
      LOWF = -1.0D15
C                                       Read entries
      FQIMAX = -1
      DO 100 I = 1,NENTRY
         IFRRNO = I
         CALL FRTAB ('READ', TABUF1, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'FRTAB', 'FRQORD', IERR)
            GO TO 990
            END IF
C                                       Adjust freqs for sideband
         DO 50 K = 1,NMBAND
            DATSID(K,I) = ISIDEB(K)
            IF (ISIDEB(K).LT.0) LSBPRS = .TRUE.
            IF (ISIDEB(K).LT.0) THEN
               BBCFRQ(K) = BANDFR(K)
     *            + (2.0 * REFPIX - NOCHAN - 1.0) * CHWID(K)
            ELSE
               BBCFRQ(K) = BANDFR(K)
               END IF
            TBBC(K,I) = BBCFRQ(K)
            XBW(K,I) = TBANDW(K)
            XCHW(K,I) = CHWID(K)
            SKYFRQ(K,I) = (REFFRQ + BANDFR(K)) / 1.0D6
            IF (I.EQ.1) ADJRFQ = MIN (ADJRFQ, BBCFRQ(K))
   50       CONTINUE
         FRFQV(I) = IFQID
         FRVLS(I) = BBCFRQ(1)
         FQIMAX = MAX (FQIMAX, IFQID)
C                                       Determine order
         CALL DETORD (BBCFRQ(1), NMBAND, FORDER(1,I), LOWIF(I))
  100    CONTINUE
      IF (REFQ.GT.1.D0) THEN
         ADJRFQ = REFQ
         TABRFQ = REFFRQ
      ELSE
         ADJRFQ = REFFRQ + ADJRFQ
         TABRFQ = REFFRQ
         END IF
C                                       Close table
      CALL TABIO ('CLOS', 1, IFRRNO, TABUF1, TABUF1, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'FRQORD', IERR)
         GO TO 990
         END IF
C                                       Reordering necessary ?
      FQINUM = NENTRY
      DO 200 I = 1, NENTRY
         REORDR(I) = .FALSE.
         DO 150 J = 2,NMBAND
            K = J - 1
            IF (FORDER(J,I).NE.(FORDER(K,I) + 1)) REORDR(I) = .TRUE.
  150       CONTINUE
         L = LOWIF(I)
         IF (REORDR(I)) FRVLS(FRFQV(I)) = TBBC(L,I)
  200    CONTINUE
C                                       Build FQID acceptance list?
      IF (DOFSEL) THEN
         IT = SELFRQ / 1.0D4 + 0.5
         SELFRQ = IT * 1.0D4
         DO 250 I = 1, NENTRY
            J = 1
            IF (REORDR(I)) J = LOWIF(I)
            IF ((SELBAN.GT.0.0) .AND. (XBW(J,I).NE.SELBAN)) GO TO 250
            IF (SELFRQ.GT.0.0) THEN
               FTEMP = SELFRQ - TABRFQ
               IF ((FTEMP.GE.(TBBC(J,I)-FRQTOL)) .AND.
     *             (FTEMP.LE.(TBBC(J,I)+FRQTOL))) THEN
                  NUMACC = NUMACC + 1
                  FQINCO(NUMACC) = I
                  END IF
               END IF
  250       CONTINUE
         IF (NUMACC.EQ.0) ALLSKP = .TRUE.
         END IF
C                                       Message about freqs
      MSGTXT = 'Sky frequencies (MHz) before re-ordering occurs:'
      CALL MSGWRT (6)
      DO 350 L = 1, FQINUM
         WRTFRQ = .FALSE.
         WRITE (MSGTXT,1030) L
         CALL MSGWRT (3)
         LINE = ' '
         J = 0
         DO 300 K = 1, NMBAND
            IF (WRTFRQ) WRTFRQ = .FALSE.
            J = J + 1
            IF (SKYFRQ(K,L).EQ.0.D0) GO TO 300
            I = 11*(J-1)+1
            WRITE (LINE(I:),1010) SKYFRQ(K,L)
            IF ((MOD(K,4).EQ.0) .AND. (.NOT.WRTFRQ)) THEN
               WRTFRQ = .TRUE.
               MSGTXT = LINE
               CALL MSGWRT (3)
               LINE = ' '
               J = 0
               END IF
  300       CONTINUE
         IF (.NOT.WRTFRQ) THEN
            MSGTXT = LINE
            CALL MSGWRT (3)
            END IF
  350    CONTINUE
C                                      Check for duplicate FR entries
      L = 1
      CALL FILL (FQINUM, 0, DONE)
      RFQDUP = .FALSE.
      DO 500 K = 1, FQINUM
         DO 450 J = 1, FQINUM
            IF (J.LE.L) GO TO 450
            MATCH = .TRUE.
            DO 400 I = 1, NMBAND
               MATCH = MATCH .AND. (TBBC(I,J).EQ.TBBC(I,L))
     *                       .AND. (XBW(I,J).EQ.XBW(I,L))
     *                       .AND. (XCHW(I,J).EQ.XCHW(I,L))
 400           CONTINUE
            IF (MATCH) THEN
               WRITE (MSGTXT,1020) L, J
               IF (DONE(J).LE.0) CALL MSGWRT (6)
               FQDUPS(J) = FQDUPS(L)
               DONE(J) = 1
               RFQDUP = .TRUE.
               END IF
 450        CONTINUE
         L = L + 1
 500     CONTINUE
      IF (RFQDUP) THEN
         MSGTXT = 'DUPLICATES WILL BE RENUMBERED'
         CALL MSGWRT (6)
         END IF
C
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FRQORD: NO FR TABLES ATTACHED TO DATA')
 1010 FORMAT (F9.2)
 1020 FORMAT ('!!!!! WARNING: FREQIDS ',I3, ' AND ',I3,' ARE IDENTICAL')
 1030 FORMAT ('Incoming FREQID # ',I3)
      END
      SUBROUTINE MAKUT (SRTORD, VOL, CNO, VER, CATBLK, LUN, DATP,
     *   TABUFF, IERR)
C-----------------------------------------------------------------------
C  This routine will create a table file and initialize the header
C  values of the table from the data in common TABHDR.
C  Special version for UT (temporary UV binary) tables, sets NREC = 1
C  to create a dummy table
C  Inputs:
C     SRTORD   I          Logical column sorted on. 0=unknown,
C                         < 0 = descending
C     VOL      I          Disk volume number of parent file.
C     CNO      I          Catalog number of parent file.
C     VER      I          Version number of table file. 0=use latest+1
C     CATBLK   I(256)     Catalog header of parent file.
C     LUN      I          Logical unit number to open table file.
C  In/Out:
C  Output:
C     DATP     I(128,2)   Data pointers used in table file control.
C     TABUFF   I(512)     IO buffer for table file.
C     IERR     I          Error code.
C-----------------------------------------------------------------------
      INTEGER   SRTORD, VOL, CNO, VER, CATBLK(256), LUN, DATP(128,2),
     *   TABUFF(*), IERR, IRNO, TITLES, UNITS, I, NREC, TTCODE(60)
      HOLLERITH HSTRNG(6)
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      DATA TITLES, UNITS /3,4/
C-----------------------------------------------------------------------
C                                       Zero out control block.
      CALL FILL (256, 0, DATP)
C                                       Ensure small integer data
C                                       types don't get transferred
C                                       to output table type array
      CALL FILL (60, 0, TTCODE)
      DO 10 I = 1, ITNCOL
         TTCODE(I) = TFCODE(I)
         IF (MOD(TFCODE(I),10).EQ.6) THEN
            TTCODE(I) = TFCODE(I) - 2
            END IF
 10      CONTINUE
      CALL COPY (ITNCOL, TTCODE, DATP(1,2))
C                                       create/open
      NREC = MIN (NAXISI(2), 5000)
      NREC = 1
      CALL TABINI ('WRIT', ITYPE, VOL, CNO, VER, CATBLK, LUN, ITANKY,
     *   NREC, ITNCOL, DATP, TABUFF, IERR)
      IF (IERR.GT.0) GO TO 999
      IF (IERR.EQ.-1) THEN
C                                       Write column values
         DO 30 I = 1,ITNCOL
            IRNO = I
C                                       Write column titles.
            CALL CHR2H (24, TTYPE(I), 1, HSTRNG)
            CALL TABIO ('WRIT', TITLES, IRNO, HSTRNG, TABUFF, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       write units
            CALL CHR2H (8, TUNIT(I), 1, HSTRNG)
            CALL TABIO ('WRIT', UNITS, IRNO, HSTRNG, TABUFF, IERR)
            IF (IERR.NE.0) GO TO 999
 30         CONTINUE
C                                       sort order
         TABUFF(43) = SRTORD
C                                       table title
         CALL CHR2H (24, EXTNAM, 1, TABUFF(101))
         END IF
C
 999  RETURN
      END
      SUBROUTINE WTFILL (WEIGHT, NUMPOL, NUMIF, NUMFRQ, INCS, INCIF,
     *   INCF, DOUVCM, WAIT)
C-----------------------------------------------------------------------
C   Fill up the spectral weight array with the IF dependent weights
C
C   Inputs:
C     WEIGHT    R(*)  IF dependent weight array
C     NUMPOL    I     # polzns in data
C     NUMIF     I     # IFs in data
C     NUMFRQ    I     # freq channels in data
C     INCS      I     Stokes increment
C     INCIF     I     IF increment
C     INCF      I     channel increment
C     DOUVCM    L     True if output data compressed
C
C   Outputs:
C     WAIT      R(*)  Spectral weights
C-----------------------------------------------------------------------
      INTEGER   NUMPOL, NUMIF, NUMFRQ, INCS, INCIF, INCF
      LOGICAL   DOUVCM
      REAL      WEIGHT(*), WAIT(*)
C
      INTEGER   I1, I2, I3, INDEX, CS, CIF, CF, IMULT, WTPTR
C-----------------------------------------------------------------------
      IMULT = 3
      IF (DOUVCM) IMULT = 1
C                                 We want the weight in every
C                                 array element, not every 3rd
      CS = INCS / IMULT
      CIF = INCIF / IMULT
      CF = INCF / IMULT
C
      DO 30 I1 = 1,NUMIF
         DO 20 I2 = 1,NUMPOL
            DO 10 I3 = 1,NUMFRQ
               INDEX = 1+(I1-1)*CIF + (I2-1)*CS + (I3-1)*CF
               WTPTR = (I1-1)*NUMPOL + I2
               WAIT(INDEX) = WEIGHT(WTPTR)
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
C
      RETURN
      END
      SUBROUTINE DATSWT (DOUVCM, CURFQI, NUMPOL, NUMIF, NUMFRQ, INCS,
     *   INCIF, INCF, ICOR0, DATMAP, DATMP2)
C-----------------------------------------------------------------------
C Set up mapping functions in order to control the switching in the
C following ways:
C (1) if the IF's are out of frequency order, set up mapping function
C     to ensure that output is in frequency order
C (1) when lower sideband data read switch the position of the
C     channels within the data (i.e. chns 1-16 become 16-1), also need
C     to change sign of phase, again!
C (2) switch cross-hand polzns when a baseline direction is changed;
C
C  We need 2 mapping functions to do this:
C    DATMAP - baseline direction unchanged, does freq ordering
C             and sideband adjustment.
C    DATMP2 - baseline direction is changed AND cross hand polarized
C             data is present; deals with freq. ordering, sideband
C             adjustment and switches cross-hand polzns.
C  If there is no cross-polarized data DATMP2 = DATMAP
C
C   Inputs:
C     DOUVCM    L     If TRUE output data are compressed
C     CURFQI    I     Current FQID
C     NUMPOL    I     # polzns in data
C     NUMIF     I     # IFs in data
C     NUMFRQ    I     # freq channels in data
C     INCS      I     Stokes increment
C     INCIF     I     IF increment
C     INCF      I     channel increment
C     ICOR0     I     First STOKES value
C   Inputs from common:
C     DATSID    I(MAXIF,MAXFQI)
C                  List of sidebands as fn of FQID, -1 => LSB, +1 => USB
C     LSBPRS  L    True if LSB present
C
C   Outputs:
C     DATMAP    I(*)  Mapping function, value of element reflects
C                     position in vis array of that element. If index
C                     is negative then the sign of the phase must
C                     also be switched (on top of any other switching
C                     that may be done).
C     DATMP2    I(*)  2nd mapping function for switching cross
C                     hands when baseline direction switched, must also
C                     deal with LSB data in this case.
C-----------------------------------------------------------------------
      LOGICAL DOUVCM
      INTEGER CURFQI, NUMPOL, NUMIF, NUMFRQ, INCS, INCIF, INCF, ICOR0,
     *   DATMAP(*), DATMP2(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ORDER.INC'
      INTEGER I1, I2, I3, I4, INDEX, DEXPLS, DEXMIN, SIDEX, JNCS, JNCF,
     *   JNCIF, OUTEX, SIDMAP(LMXCIF), ITEMP
      LOGICAL POL34, POL23, POL12, POSSSW
C-----------------------------------------------------------------------
      JNCS = INCS
      JNCF = INCF
      JNCIF = INCIF
      IF (.NOT.DOUVCM) THEN
         JNCS = INCS / 3
         JNCF = INCF / 3
         JNCIF = INCIF / 3
         END IF
C
      I1 = NUMPOL * NUMIF * NUMFRQ
      CALL FILL (I1, 0, DATMAP)
      CALL FILL (I1, 0, DATMP2)
C                                       Fill in default value
      DO 30 I1 = 1,NUMIF
         DO 20 I2 = 1,NUMFRQ
            DO 10 I3 = 1,NUMPOL
               INDEX = 1 + (I1-1)*JNCIF + (I2-1)*JNCF + (I3-1)*JNCS
               DATMAP(INDEX) = INDEX
               DATMP2(INDEX) = INDEX
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
C
      IF (REORDR(CURFQI)) THEN
         DO 130 I1 = 1,NUMIF
            I4 = FORDER(I1,CURFQI)
            DO 120 I2 = 1,NUMFRQ
               DO 110 I3 = 1,NUMPOL
                  INDEX = 1 + (I1-1)*JNCIF + (I2-1)*JNCF + (I3-1)*JNCS
                  OUTEX = 1 + (I4-1)*JNCIF + (I2-1)*JNCF + (I3-1)*JNCS
                  DATMAP(INDEX) = OUTEX
                  DATMP2(INDEX) = OUTEX
 110              CONTINUE
 120           CONTINUE
 130        CONTINUE
         END IF
C                                       Set up LSB switching within
C                                       an IF
C                                       Need to reverse the spectrum
C                                       and negate the phases - shit!
      IF (LSBPRS) THEN
         DO 230 I1 = 1,NUMIF
C                                       DATSID reflects the order of
C                                       the IF's before they have been
C                                       ordered in frequency
            IF (DATSID(I1,CURFQI).LT.0) THEN
               DO 220 I2 = 1, NUMFRQ
                  I4 = NUMFRQ - I2 + 1
                  DO 210 I3 = 1, NUMPOL
                     INDEX = 1 + (I2-1)*JNCF + (I3-1)*JNCS
                     SIDEX = 1 + (I4-1)*JNCF + (I3-1)*JNCS
                     SIDMAP(INDEX) = -SIDEX
  210                CONTINUE
  220             CONTINUE
               END IF
  230       CONTINUE
C                                       Merge the LSB switch with
C                                       the frequency ordering switch.
         DO 330 I1 = 1, NUMIF
            IF (DATSID(I1,CURFQI).LT.0) THEN
               DO 320 I2 = 1, NUMFRQ
                  DO 310 I3 = 1, NUMPOL
                     INDEX = 1 + (I2-1)*JNCF + (I3-1)*JNCS
                     OUTEX = 1 + (I1-1)*JNCIF + (I2-1)*JNCF + (I3-1)
     *                  *JNCS
                     DATMAP(OUTEX) = DATMAP(OUTEX) +
     *                  ABS(SIDMAP(INDEX)) - INDEX
                     DATMP2(OUTEX) = DATMP2(OUTEX) +
     *                  ABS(SIDMAP(INDEX)) - INDEX
                     IF (SIDMAP(INDEX).LT.0) THEN
                        DATMAP(OUTEX) = -DATMAP(OUTEX)
                        DATMP2(OUTEX) = -DATMP2(OUTEX)
                        END IF
  310                CONTINUE
  320             CONTINUE
               END IF
  330       CONTINUE
         END IF
C                                       Check the location of the RL/LR
C                                       correlations
      POSSSW = .FALSE.
      POL34 = .FALSE.
      POL23 = .FALSE.
      POL12 = .FALSE.
      IF (ICOR0.LT.0) THEN
         IF ((ICOR0.EQ.-1) .AND. (NUMPOL.EQ.4)) POL34 = .TRUE.
         IF ((ICOR0.EQ.-2) .AND. (NUMPOL.EQ.3)) POL23 = .TRUE.
         IF ((ICOR0.EQ.-3) .AND. (NUMPOL.EQ.2)) POL12 = .TRUE.
         END IF
      POSSSW = POL34 .OR. POL23 .OR. POL12
      IF (POSSSW) THEN
C                                       Relies on the fact that the
C                                       cross hands will be together
         DO 530 I1 = 1, NUMIF
            DO 520 I2 = 1, NUMFRQ
               DO 510 I3 = 1, NUMPOL
                  INDEX = 1 + (I1-1)*JNCIF + (I2-1)*JNCF + (I3-1)*JNCS
                  DEXPLS = 1 + (I1-1)*JNCIF + (I2-1)*JNCF + (I3)*JNCS
                  DEXMIN = 1 + (I1-1)*JNCIF + (I2-1)*JNCF + (I3-2)*JNCS
                  IF (POL34) THEN
                     IF (I3.EQ.3) THEN
                        ITEMP = DATMP2(INDEX)
                        DATMP2(INDEX) = DATMP2(DEXPLS)
                        END IF
                     IF (I3.EQ.4) DATMP2(INDEX) = ITEMP
                     END IF
                  IF (POL23) THEN
                     IF (I3.EQ.2) THEN
                        ITEMP = DATMP2(INDEX)
                        DATMP2(INDEX) = DATMP2(DEXPLS)
                        END IF
                     IF (I3.EQ.3) DATMP2(INDEX) = ITEMP
                     END IF
                  IF (POL12) THEN
                     IF (I3.EQ.1) THEN
                        ITEMP = DATMP2(INDEX)
                        DATMP2(INDEX) = DATMP2(DEXPLS)
                        END IF
                     IF (I3.EQ.2) DATMP2(INDEX) = ITEMP
                     END IF
  510             CONTINUE
  520          CONTINUE
  530       CONTINUE
         END IF
C
  999 RETURN
      END
      SUBROUTINE LOADUP (DOUVCM, UVPTR, NDAT, TAPPTR, WAIT, SCLVIS,
     *   TMINT, ANTSW, MAXISI, DATMAP, DATMP2, DOCORR, CORREL, SETDEB,
     *   RECRR, SCALWT, THRESH, WTFLAG)
C-----------------------------------------------------------------------
C  Routine to load the uv buffer from the tape buffer, normalizing the
C  data on the way, reversing the baseline direction if needed, and if
C  baseline direction is reversed and data are cross-polzd, will reverse
C  location of the RL/LR correlations (using the DATMP2 mapping
C  function)
C
C  If lower sidebands are present the spectra need to be reversed
C  and the phases negated once more. This is done using the
C  DATMAP mapping function, a negative index record indicates that
C  the phase must be negated.
C
C  PHASE = -PHASE is also implemented here to correct the sign of the
C  phases.
C
C  Input:
C    DOUVCM      L            If TRUE output data are compressed
C    UVPTR       I            Pointer in UVBUFF to start loading data
C    NDAT        I            Number of data points to be read
C    TAPPTR      I            Pointer in tape buffer, RECRR, from
C                             where reading should start. Will be
C                             modified in subroutine
C    WAIT        R(*)         Weight array, data will be normalized by
C                             this array
C    SCLVIS      R            Global scaling factor for all vis values
C    RMINT       R            Integration time
C    ANTSW       L            If true then baseline direction changed
C                             so sign of phases must be changed on top
C                             of any other changes
C    MAXISI      I            Increment in tape buffer of vis data
C    DATMAP      I(*)         Mapping array for LSB switching in the
C                             case of no switching of baseline direction.
C    DATMP2      I(*)         Mapping array for cross-hand polzn
C                             location switching if baseline direction
C                             changes, also deals with LSB switching.
C    DOCORR      I            -1  => no digital corrections
C                             1   => all corrections
C                             2   => do cross-power + total power only
C                                    if zero-padding done in correlator.
C    CORREL      C*(*)        Correlator type (' ', 'DIFX')
C    SETDEB      I            Debug level, can do different things
C    RECRR       R(*)         Tape buffer
C    THRESH      R            Weight threshold below which to flag
C                             record
C    SCALWT      R            Additional scale to apply to weights
C  Output via 'SCRBFS.INC' common
C    UVBUFF      R(*)         Output uv buffer, data ready to be written
C                             to disc.
C    WTFLAG      L            True if record is to be skipped.
C-----------------------------------------------------------------------
      INTEGER   UVPTR, NDAT, TAPPTR, MAXISI, DATMAP(*), DATMP2(*),
     *   SETDEB, DOCORR
      REAL    WAIT(*), SCLVIS, TMINT, RECRR(*), THRESH, SCALWT
      LOGICAL DOUVCM, ANTSW, WTFLAG
      CHARACTER CORREL*(*)
C
      INTEGER   I, OUTPTR, DPTR
      REAL      ASIGN, SCL
      LOGICAL   WFLAG
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'DIGCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SCRBFS.INC'
C-----------------------------------------------------------------------
      WTFLAG = .TRUE.
      SCL = SCLVIS * TMINT
C                                       Load up the data array
C                                       Noncompressed
      IF (.NOT.DOUVCM) THEN
         DO 100 I = 1,NDAT
            ASIGN = -1.0
            IF (ANTSW) ASIGN = -ASIGN
C                                       Use mapping function to set
C                                       the output buffer pointer
            IF (.NOT.ANTSW) THEN
               OUTPTR = (ABS(DATMAP(I))-1)*3 + UVPTR
               IF (DATMAP(I).LT.0) ASIGN = -ASIGN
            ELSE IF (ANTSW) THEN
               OUTPTR = (ABS(DATMP2(I))-1)*3 + UVPTR
               IF (DATMP2(I).LT.0) ASIGN = -ASIGN
               END IF
C                                       Need the weight first
            IF (MAXISI.EQ.2) THEN
               UVBUFF(OUTPTR+2) = WAIT(I) * SCL
            ELSE
               UVBUFF(OUTPTR+2) = RECRR(TAPPTR+2) * SCL
               END IF
C                                       Check if weight-based flagging
C                                       being performed
            WFLAG = UVBUFF(OUTPTR+2).LE.0.0
            IF ((THRESH.GT.0.0) .AND.
     *         (UVBUFF(OUTPTR+2)/SCL.LE.THRESH)) WFLAG = .TRUE.
            IF (.NOT.WFLAG) THEN
               WTFLAG = .FALSE.
               IF (SETDEB.EQ.2) THEN
                  UVBUFF(OUTPTR) = RECRR(TAPPTR) / UVBUFF(OUTPTR+2) *
     *               TMINT
                  UVBUFF(OUTPTR+1) = ASIGN * RECRR(TAPPTR+1) /
     *               UVBUFF(OUTPTR+2) * TMINT
               ELSE IF (SETDEB.EQ.1) THEN
                  UVBUFF(OUTPTR) = RECRR(TAPPTR) / SCLVIS
                  UVBUFF(OUTPTR+1) =  ASIGN * RECRR(TAPPTR+1) / SCLVIS
                  UVBUFF(OUTPTR+2) = UVBUFF(OUTPTR+2) * SCLVIS
               ELSE
                  UVBUFF(OUTPTR) = RECRR(TAPPTR) / UVBUFF(OUTPTR+2)
                  UVBUFF(OUTPTR+1) = ASIGN * RECRR(TAPPTR+1) /
     *               UVBUFF(OUTPTR+2)
                  END IF
C                                       Flagged record
            ELSE
               UVBUFF(OUTPTR) = RECRR(TAPPTR)
               UVBUFF(OUTPTR+1) = ASIGN * RECRR(TAPPTR+1)
               UVBUFF(OUTPTR+2) = 0.0
               END IF
            UVBUFF(OUTPTR+2) = UVBUFF(OUTPTR+2) * SCALWT
            TAPPTR = TAPPTR + MAXISI
  100       CONTINUE
C                                       VLBA specific corrections
C        IF (ISVLBA) THEN
C                                       Remove FFT artifacts
            IF ((TWDVER.EQ.0) .AND. (CURA1.EQ.CURA2)) THEN
               CALL CORART (DOUVCM, UVPTR, UVBUFF)
               END IF
C                                       Digital corrections
            IF (DOCORR.GT.0) THEN
               CALL BFAC (UVBUFF(UVPTR), DOCORR, CORREL, DOUVCM, SCL)
               END IF
C           END IF
C
C                                      Compressed
      ELSE IF (DOUVCM) THEN
         UVBUFF(UVPTR) = 0.0
         UVBUFF(UVPTR+1) = 0.0
         DO 200 I = 1, NDAT
            ASIGN = -1.0
            IF (ANTSW) ASIGN = -ASIGN
C                                       Use mapping function to set
C                                       the output buffer pointer
            IF (.NOT.ANTSW) THEN
               OUTPTR = (ABS(DATMAP(I))-1)*3 + 1
               IF (DATMAP(I).LT.0) ASIGN = -ASIGN
            ELSE
               OUTPTR = (ABS(DATMP2(I))-1)*3 + 1
               IF (DATMP2(I).LT.0) ASIGN = -ASIGN
               END IF
C                                       Need the weight first
            IF (MAXISI.EQ.2) THEN
               TUVBUF(OUTPTR+2) = WAIT(I) * SCL
            ELSE
               TUVBUF(OUTPTR+2) = RECRR(TAPPTR+2) * SCL
               END IF
C                                       Check if weight-based flagging
C                                       being performed
            WFLAG = TUVBUF(OUTPTR+2).LE.0.0
            IF ((THRESH.GT.0.0) .AND.
     *         (TUVBUF(OUTPTR+2)/SCL.LE.THRESH)) WFLAG=.TRUE.
            IF (.NOT.WFLAG) THEN
               WTFLAG = .FALSE.
               IF (SETDEB.EQ.2) THEN
                  TUVBUF(OUTPTR) = RECRR(TAPPTR) / TUVBUF(OUTPTR+2) *
     *               TMINT
                  TUVBUF(OUTPTR+1) = ASIGN * RECRR(TAPPTR+1) /
     *               TUVBUF(OUTPTR+2) * TMINT
              ELSE IF (SETDEB.EQ.1) THEN
                  TUVBUF(OUTPTR) = RECRR(TAPPTR) / SCLVIS
                  TUVBUF(OUTPTR+1) = ASIGN * RECRR(TAPPTR+1) / SCLVIS
                  UVBUFF(OUTPTR+2) = UVBUFF(OUTPTR+2) * SCLVIS
              ELSE
                  TUVBUF(OUTPTR) = RECRR(TAPPTR) / TUVBUF(OUTPTR+2)
                  TUVBUF(OUTPTR+1) = ASIGN * RECRR(TAPPTR+1) /
     *               TUVBUF(OUTPTR+2)
                  END IF
C                                       Flagged record
            ELSE
               TUVBUF(OUTPTR) = RECRR(TAPPTR)
               TUVBUF(OUTPTR+1) = ASIGN * RECRR(TAPPTR+1)
               TUVBUF(OUTPTR+2) = 0.0
               END IF
            TUVBUF(OUTPTR+2) = TUVBUF(OUTPTR+2) * SCALWT
            TAPPTR = TAPPTR + MAXISI
  200       CONTINUE
C                                       VLBA specific corrections
C        IF (ISVLBA) THEN
C                                       Remove FFT artifacts
            DPTR = 1
            IF ((TWDVER.EQ.0) .AND. (CURA1.EQ.CURA2)) THEN
               CALL CORART (DOUVCM, DPTR, TUVBUF)
               END IF
C                                       Digital corrections
            IF (DOCORR.GT.0) THEN
               CALL BFAC (TUVBUF(DPTR), DOCORR, CORREL, DOUVCM, SCL)
               END IF
C           END IF
C
         CALL ZUVPAK (NDAT, TUVBUF, UVBUFF(UVPTR), UVBUFF(UVPTR+2))
         END IF
C
 999  RETURN
      END
      SUBROUTINE MAPANT (DISK, ISLOT, CATIN, LUN, IERR)
C-----------------------------------------------------------------------
C   This routine sets up the mapping function for antenna names.  If a
C   file already exists and the current file is being concatanated to it
C   the existing AN table defines the antenna name -> number mapping, if
C   no concatanation is occurring this operation is not necessary.
C   Inputs:
C      DISK     I        Volume on which data reside
C      ISLOT    I        Catalogue number of data file
C      CATIN    I        Catalogue header for data file
C      LUN      I        LUN
C   Outputs:
C      IERR     I        Error code. 0=ok.
C   Output in common
C      ANTMAP   I(*,*)   The antenna mapping function, first element is
C                        the subarray number, second is the new antenna
C                        number - value is the old antenna number.
C      ANTNAM  C*8(*,*)   Array of antenna names, first element is the
C                        subarray number, secons is the new antenna
C                        number.
C-----------------------------------------------------------------------
      INTEGER   DISK, ISLOT, LUN, CATIN(256), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'DATSEL.INC'
      INTEGER  ANVER, IVER, I, NENTRY, MXANT, II, AGVER, MXSUB, NEWNUM,
     *   NCHAR, USED(MAXANT)
      LOGICAL  MATCH, FMATCH
      CHARACTER ANVLBA(10)*2
C                                       Declarations for ANTINI
      INTEGER   ANKOLS(MAXANC), ANNUMV(MAXANC), ANNCAL, IANRNO, ANFQID,
     *   ANNUMO, NIFANT, J, K, JTRIM
      CHARACTER TLNAME*8, RFDATE*8, SYSTIM*8, HANXYZ*8, FRAMET*8
      REAL      POLRXY(2), DATUTC, ANUT1U
      DOUBLE PRECISION  CARRAY(3), FREQSA, ANGSTI, ANDEGP
C                                       Declarations for TABAN
      INTEGER   ANNUMB, MNTTEL
      CHARACTER TNAME*8, APLTYA*2, APLTYB*2
      REAL      TELXOF, ANPLAA, ANPLCA(2*MAXIF), ANPLAB, ANDIAM,
     *   ANFWHM(MAXIF), ANPLCB(2*MAXIF)
      DOUBLE PRECISION  TELXYZ(3), ORBVAR(6)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'INCS:DAGV.INC'
      SAVE USED
      DATA MXANT /MAXANT/
      DATA MXSUB /MAXSUB/
      DATA USED /MAXANT*0/
      DATA ANVLBA /'BR','FD','HN','KP','LA','MK','NL','OV','PT','SC'/
C-----------------------------------------------------------------------
      IERR = 0
C                                       If we are reading an AG table
C                                       now, jump to section that does
C                                       the antenna comparison and sets
C                                       up the full mapping function.
      IF (ANTLOD) GO TO 300
C                                       Are there any AN tables.
      CALL FNDEXT ('AN', CATIN, ANVER)
      IF (ANVER.EQ.0) THEN
         IERR = 1
         WRITE (MSGTXT,1000)
         GO TO 990
         END IF
      IF (ANVER.GT.MXSUB) THEN
         IERR = 1
         WRITE (MSGTXT,1040)
         GO TO 990
         END IF
C                                       Initialize
      CALL FILL (MAXANT, 0, USED)
      CALL FILL (MXSUB, -1, MXANTN(1))
      DO 110 I = 1,MXANT
         DO 100 II = 1,MXSUB
            ANTNAM(II,I) = ' '
            ANTMAP(II,I) = 0
            TMPNAM(II,I) = ' '
            TMPMAP(II,I) = 0
  100       CONTINUE
  110    CONTINUE
C                                       Loop over subarrays loading up
C                                       the relevant antenna
C                                       information.
      DO 200 IVER = 1, ANVER
         CALL ANTINI ('READ', TABUF1, DISK, ISLOT, IVER, CATIN, LUN,
     *      IANRNO, ANKOLS, ANNUMV, CARRAY, ANGSTI, ANDEGP, FREQSA,
     *      RFDATE, POLRXY, ANUT1U, DATUTC, SYSTIM, TLNAME, HANXYZ,
     *      FRAMET, ANNUMO, ANNCAL, NIFANT, ANFQID, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1010) IERR, IVER
            GO TO 990
            END IF
         NENTRY = TABUF1(5)
C                                       Read entries
         DO 150 I = 1, NENTRY
            IANRNO = I
            CALL TABAN ('READ', TABUF1, IANRNO, ANKOLS, ANNUMV, TNAME,
     *         TELXYZ, ORBVAR, ANNUMB, MNTTEL, TELXOF, ANDIAM, ANFWHM,
     *         APLTYA, ANPLAA, ANPLCA, APLTYB, ANPLAB, ANPLCB, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020) IERR, IVER
               GO TO 990
               END IF
            TMPNAM(IVER,ANNUMB) = TNAME
            TMPMAP(IVER,ANNUMB) = ANNUMB
            USED(ANNUMB) = ANNUMB
            MXANTN(IVER) = MAX (MXANTN(IVER), ANNUMB)
  150       CONTINUE
C                                       Close table
         CALL TABIO ('CLOS', 1, IANRNO, TABUF1, TABUF1, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('CLOS', 'TABIO', 'MAPANT', IERR)
            GO TO 990
            END IF
  200    CONTINUE
      ANTLOD = .TRUE.
      GO TO 999
C                                       Read in the AG table and
C                                       compare antennas, setting up the
C                                       renumbering lookup table if
C                                       necessary.
  300 CALL FNDEXT ('AG', CATIN, AGVER)
      IF (AGVER.EQ.0) THEN
         IERR = 1
         WRITE (MSGTXT,1030)
         GO TO 990
         END IF
      IF (AGVER.GT.MXSUB) THEN
         IERR = 1
         WRITE (MSGTXT,1040)
         GO TO 990
         END IF
C                                       Read AG tables
      DO 400 IVER = 1, AGVER
         CALL AGINI ('READ', TABUF1, DISK, ISLOT, IVER, CATIN,
     *      LUN, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('CLOS', 'AGINI', 'MAPANT', IERR)
            GO TO 990
            END IF
         NENTRY = TABUF1(5)
C
         DO 350 I = 1, NENTRY
            IAGRNO = I
            CALL AGTAB ('READ', TABUF1, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('CLOS', 'AGTAB', 'MAPANT', IERR)
               GO TO 990
               END IF
            CALL CHTRIM (ANNAME, 8, TNAME, NCHAR)
C                                       Does it match with an existing
C                                       antenna?
            DO 310 II = 1, MXANTN(IVER)
C                                       Name match?
               FMATCH = .FALSE.
               MATCH = TNAME.EQ.TMPNAM(IVER,II)
               IF (MATCH) THEN
                  NEWNUM = II
C                                       Number match?
                  FMATCH = NOSTA.EQ.TMPMAP(IVER,II)
                  GO TO 320
                  END IF
 310           CONTINUE
 320        IF (FMATCH) THEN
               ANTMAP(IVER,NOSTA) = TMPMAP(IVER,NOSTA)
               ANTNAM(IVER,NOSTA) = TMPNAM(IVER,NOSTA)
C                                       Name match but number
C                                       different
            ELSE
               ANTCHA = .TRUE.
               IF (MATCH) THEN
                  ANTMAP(IVER,NEWNUM) = NOSTA
                  ANTNAM(IVER,NEWNUM) = TNAME
C                                       New antenna, VLITE
               ELSE IF (ANTYPE.EQ.1) THEN
                  K = JTRIM (TNAME)
                  IF (K.EQ.2) THEN
                     READ (TNAME,1320) J
                  ELSE
                     READ (TNAME,1321) J
                     END IF
                  J = J + 1
                  IF (USED(J).LE.0) THEN
                     ANTNAM(IVER,J) = TNAME
                     ANTMAP(IVER,J) = NOSTA
                     USED(J) = J
                  ELSE
                     MXANTN(IVER) = MXANTN(IVER) + 1
                     ANTNAM(IVER,MXANTN(IVER)) = TNAME
                     ANTMAP(IVER,MXANTN(IVER)) = NOSTA
                     USED(MXANTN(IVER)) = MXANTN(IVER)
                     END IF
C                                       New antenna, VLBA
               ELSE IF (ANTYPE.EQ.2) THEN
                  DO 330 J = 1,10
                     IF ((TNAME.EQ.ANVLBA(J)) .AND. (USED(J).LE.0)) THEN
                        ANTNAM(IVER,J) = TNAME
                        ANTMAP(IVER,J) = NOSTA
                        USED(J) = J
                        GO TO 350
                        END IF
 330                 CONTINUE
                  MXANTN(IVER) = MXANTN(IVER) + 1
                  ANTNAM(IVER,MXANTN(IVER)) = TNAME
                  ANTMAP(IVER,MXANTN(IVER)) = NOSTA
                  USED(MXANTN(IVER)) = MXANTN(IVER)
C                                       New antenna, other
               ELSE IF (ANTYPE.EQ.3) THEN
                  DO 340 J = 1,NANTNM
                     IF ((TNAME.EQ.USRNAM(J)) .AND. (USED(J).LE.0)) THEN
                        ANTNAM(IVER,J) = TNAME
                        ANTMAP(IVER,J) = NOSTA
                        USED(J) = J
                        GO TO 350
                        END IF
 340                 CONTINUE
                  MXANTN(IVER) = MXANTN(IVER) + 1
                  ANTNAM(IVER,MXANTN(IVER)) = TNAME
                  ANTMAP(IVER,MXANTN(IVER)) = NOSTA
                  USED(MXANTN(IVER)) = MXANTN(IVER)
C                                       New antenna, assign number
               ELSE
                  MXANTN(IVER) = MXANTN(IVER) + 1
                  ANTNAM(IVER,MXANTN(IVER)) = TNAME
                  ANTMAP(IVER,MXANTN(IVER)) = NOSTA
                  USED(MXANTN(IVER)) = MXANTN(IVER)
                  END IF
               END IF
 350        CONTINUE
C                                       Close table
         CALL TABIO ('CLOS', 1, IAGRNO, TABUF1, TABUF1, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('CLOS', 'TABIO', 'MAPANT', IERR)
            GO TO 990
            END IF
  400    CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAPANT: NO AN TABLES ATTACHED TO DATA')
 1010 FORMAT ('MAPANT: ERROR ',I3,' OPENING AN TABLE # ',I4)
 1020 FORMAT ('MAPANT: ERROR ',I3,' READING AN TABLE # ',I4)
 1030 FORMAT ('MAPANT: NO AG TABLES FOUND')
 1040 FORMAT ('MAPANT: TOO MANY SUBARRAYS, SEE PROGRAMMER')
 1320 FORMAT (1X,I1)
 1321 FORMAT (1X,I2)
      END
      SUBROUTINE ANTREL (BASEN)
C-----------------------------------------------------------------------
C   Routine to check the antenna numbers and relabel them if necessary
C   based on the lookup table defined in MAPANT.
C  Input/Output:
C    BASEN      R       Baseline random parameter
C-----------------------------------------------------------------------
      REAL      BASEN
C
      INTEGER   IARR, ANT1, ANT2, ANTFNC
C-----------------------------------------------------------------------
C                                       Crack the antenna numbers and
C                                       subarray number.
      ANT1 = BASEN + 0.1
      IARR = 1.1 + 100.0 * (BASEN - ANT1)
      ANT1 = BASEN / 256.0 + 0.1
      ANT2 = BASEN - 256 * ANT1 + 0.1
C                                       Renumber ?
      ANT1 = ANTFNC (ANT1, IARR)
      ANT2 = ANTFNC (ANT2, IARR)
C                                       Rebuild random parameter
      BASEN = ANT1 * 256 + ANT2 + (0.01*(IARR-1))
C
 999  RETURN
      END
      INTEGER FUNCTION ANTFNC (ANT, ARR)
C-----------------------------------------------------------------------
C   ANTFNC looks through the antenna mapping function to determine if
C   the antenna is to be renumbered, if so it does it.
C  Input:
C    ANT     I       Antenna number
C    ARR     I       Subarray number
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER ANT, ARR, I
      INCLUDE 'DATSEL.INC'
C-----------------------------------------------------------------------
      ANTFNC = ANT
      DO 10 I = 1,MXANTN(ARR)
         IF (ANTMAP(ARR,I).EQ.ANT) ANTFNC = I
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE MAPSRC (DISK, ISLOT, CATIN, LUN, IERR)
C-----------------------------------------------------------------------
C   This routine sets up the mapping function for source names.  If a
C   file already exists and the current file is being concatanated to it
C   the existing SU table defines the source name -> number mapping, if
C   no concatanation is occurring this operation should not be
C   necessary.  Unfortunately, SO tables from the correlator may contain
C   > 1 entry for the same source, so we have to test for that too.
C   Inputs:
C      DISK     I         Volume on which data reside
C      ISLOT    I         Catalogue number of data file
C      CATIN    I         Catalogue header for data file
C      LUN      I         LUN
C   Outputs:
C      IERR     I         Error code. 0=ok.
C   Output in common
C      SRCMAP   I(*)      The source mapping function, array element is
C                         the OLD source number AND value is the NEW
C                         source number.
C      SRCNAM   C*16(*)   Array of source names, array element is the
C                         new source number.
C      SRCQUL   I(*)      Array of source qualifiers, array element is
C                         the new source number
C      SRCCAL   C*4(*)    Array of calcodes, array element is the
C                         new source number
C-----------------------------------------------------------------------
      INTEGER   DISK, ISLOT, LUN, CATIN(256), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'DATSEL.INC'
      INTEGER  SUVER, IVER, I, NENTRY, MXSRC, II, SOVER, NEWNUM, NCHAR,
     *   NUMBND, TMPSMP(MAXSOU), TMPQUL(MAXSOU)
      CHARACTER TNAME*16, TMPSNM(MAXSOU)*16, TMPCAL(MAXSOU)*4, TC*4
      DOUBLE PRECISION TMPRA(MAXSOU), TMPDEC(MAXSOU), DEPS,
     *   TMPRAO(MAXSOU), TMPDCO(MAXSOU)
C                                       Declarations for SOUINI
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC), ISURNO, IFQ
C                                       Declarations for TABSOU
      CHARACTER SOUNAM*16, TCALC*4
      REAL      FLUX(4,MAXIF)
      INTEGER   IDSOU, TQUAL
      DOUBLE PRECISION  FREQO(MAXIF), LSRVEL(MAXIF), LRESTF(MAXIF),
     *   BANDW, TRAEPO, TDCEPO, TRAAPP, TDCAPP, TEQUIN, TPMRA, TPMDEC,
     *   TRAOBS, TDECOB
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'INCS:DSOV.INC'
      SAVE TMPSMP, TMPQUL, TMPSNM, TMPCAL
      DATA MXSRC /MAXSOU/
C-----------------------------------------------------------------------
      IERR = 0
      DEPS = 1.0D0 / 3.6D7
C                                       If we are reading an SO table
C                                       now, jump to section that does
C                                       the source comparison and sets
C                                       up the full mapping function.
      IF (SRCLOD) GO TO 300
C                                       Initialize
      MXSRCN = 0
      DO 100 I = 1,MXSRC
         SRCNAM(I) = ' '
         SRCMAP(I) = 0
         SRCQUL(I) = 0
         SRCCAL(I) = ' '
         TMPSNM(I) = ' '
         TMPSMP(I) = 0
         TMPQUL(I) = 0
         TMPCAL(I) = ' '
         TMPRA(I)  = 0.D0
         TMPDEC(I) = 0.D0
         TMPRAO(I)  = 0.D0
         TMPDCO(I) = 0.D0
  100    CONTINUE
      SRCLOD = .TRUE.
C                                       Are there any SU tables.
      CALL FNDEXT ('SU', CATIN, SUVER)
C                                       Load up the relevant source
C                                       information.
      IF (SUVER.GT.0) THEN
         IVER = 1
         CALL SOUINI ('READ', TABUF1, DISK, ISLOT, IVER, CATIN, LUN,
     *      NUMBND, VELTYP, VELDEF, IFQ, ISURNO, SUKOLS, SUNUMV, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'SOUINI', 'SRCMAP', IERR)
            GO TO 999
            END IF
         NENTRY = TABUF1(5)
C                                       Read entries
         DO 150 I = 1,NENTRY
            ISURNO = I
            CALL TABSOU ('READ', TABUF1, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *         SOUNAM, TQUAL, TCALC, FLUX, FREQO, BANDW, TRAEPO, TDCEPO,
     *         TEQUIN, TRAAPP, TDCAPP, TRAOBS, TDECOB, LSRVEL, LRESTF,
     *         TPMRA,  TPMDEC,IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('READ', 'TABSOU', 'MAPSRC', IERR)
               GO TO 999
               END IF
            CALL CHTRIM (SOUNAM, 16, TNAME, NCHAR)
            CALL CHTRIM (TCALC, 4, TC, NCHAR)
            TMPSNM(IDSOU) = TNAME
            TMPSMP(IDSOU) = IDSOU
            TMPQUL(IDSOU) = TQUAL
            TMPCAL(IDSOU) = TC
            TMPRA(IDSOU)  = TRAEPO
            TMPDEC(IDSOU) = TDCEPO
            TMPRAO(IDSOU) = TRAOBS
            TMPDCO(IDSOU) = TDECOB
            MXSRCN = MAX (MXSRCN, IDSOU)
  150       CONTINUE
C                                       Close table
         CALL TABIO ('CLOS', 1, ISURNO, TABUF1, TABUF1, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('CLOS', 'TABIO', 'MAPSRC', IERR)
            GO TO 999
            END IF
         END IF
      GO TO 999
C                                       Read in the SO table and
C                                       compare sources, setting up the
C                                       renumbering lookup table if
C                                       necessary.
 300  CALL FNDEXT ('SO', CATIN, SOVER)
      IF (SOVER.EQ.0) THEN
         IERR = 1
         WRITE (MSGTXT,1030)
         GO TO 990
         END IF
C                                       Read SO table
      CALL SOINI ('READ', TABUF1, DISK, ISLOT, SOVER, CATIN, LUN, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('CLOS', 'SOINI', 'MAPSRC', IERR)
         GO TO 999
         END IF
      NENTRY = TABUF1(5)
C
      DO 350 I = 1,NENTRY
         ISORNO = I
         CALL SOTAB ('READ', TABUF1, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('CLOS', 'SOTAB', 'MAPSRC', IERR)
            GO TO 999
            END IF
         CALL CHTRIM (SAUCE, 16, TNAME, NCHAR)
         CALL CHTRIM (CALCOD, 4, TC, NCHAR)
C                                       Does it match with an existing
C                                       source?
         DO 330 II = 1,MXSRCN
C                                       Name & coords match?
            IF ((TNAME.EQ.TMPSNM(II)) .AND. (TMPQUL(II).EQ.QUAL) .AND.
     *         (ABS(TMPRA(II)-RAEPO).LT.DEPS) .AND.
     *         (ABS(TMPDEC(II)-DECEPO).LE.DEPS) .AND.
     *         (ABS(TMPRAO(II)-RAOBS).LT.DEPS) .AND.
     *         (ABS(TMPDCO(II)-DECOBS).LE.DEPS)) THEN
               NEWNUM = II
               TC = TMPCAL(II)
               GO TO 340
               END IF
 330        CONTINUE
         MXSRCN = MXSRCN + 1
         NEWNUM = MXSRCN
         TMPSNM(MXSRCN) = TNAME
         TMPQUL(MXSRCN) = QUAL
         TMPRA(MXSRCN) = RAEPO
         TMPDEC(MXSRCN) = DECEPO
         TMPRAO(MXSRCN) = RAOBS
         TMPDCO(MXSRCN) = DECOBS
C
 340     IF (ISOUR.NE.NEWNUM) SRCCHA = .TRUE.
         SRCNAM(NEWNUM) = TNAME
         SRCMAP(ISOUR) = NEWNUM
         SRCQUL(NEWNUM) = QUAL
         SRCCAL(NEWNUM) = TC
  350    CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 1, ISORNO, TABUF1, TABUF1, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'MAPSRC', IERR)
         GO TO 999
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('MAPSRC: NO SO TABLES FOUND')
      END
      INTEGER FUNCTION SRCFNC (SRC)
C-----------------------------------------------------------------------
C   SRCFNC looks through the source mapping function to determine if
C   the source is to be renumbered, if so it does it.
C   Input:
C      SRC   I   source number
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SRC
      INCLUDE 'DATSEL.INC'
C-----------------------------------------------------------------------
      IF (SRCMAP(SRC).GT.0) THEN
         SRCFNC = SRCMAP(SRC)
      ELSE
         SRCFNC = SRC
         END IF
C
 999  RETURN
      END
      SUBROUTINE MAPFQI (DISK, ISLOT, CATIN, LUN, IERR)
C-----------------------------------------------------------------------
C   This routine sets up the mapping function for frequency IDs.  If a
C   file already exists and the current file is being concatanated to it
C   the existing FQ table defines the freqid mapping.  If no
C   concatanation is occurring this operation is not necessary.
C   Inputs:
C      DISK    I        Volume on which data reside
C      ISLOT   I        Catalogue number of data file
C      CATIN   I        Catalogue header for data file
C      LUN     I        LUN
C   Outputs:
C      IERR    I        Error code. 0=ok.
C   Output in common
C      FQMAP   I(*)     The freqid mapping function, array element is
C                       the new freqid number and the value is the old
C                       freqid number.
C      FQFREQ  D(*,*)   Array of offset frequency values, 1st element is
C                       IF number, 2nd is the original freqid number
C      FQCHW   R(*,*)   Array of channel bandwidth values, 1st element
C                       is IF number, 2nd is the original freqid number
C      FQTBW   R(*,*)   Array of total bandwidth values, 1st element is
C                       IF number, 2nd is the original freqid number
C      FQSIDE  I(*,*)   Array of sideband values, 1st element is
C                       IF number, 2nd is the original freqid number
C-----------------------------------------------------------------------
      INTEGER   DISK, ISLOT, LUN, CATIN(256), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'ORDER.INC'
      INTEGER  FQVER, IVER, I, NENTRY, II, FRVER, FRORD, SIGN, J,
     *   FTEMP(MAXIF,MAXFQI), LOWIF
      DOUBLE PRECISION REFF, NREFF
      LOGICAL  MATCH, TMATCH(MAXFQI)
C                                       Declarations for FQ tables
      INTEGER   FQKOLS(MAXFQC), FQNUMV(MAXFQC), NUMIF, IFQRNO, FQID,
     *   TQSIDE(MAXIF), XSIDE(MAXIF)
      REAL      TQCHW(MAXIF), TQTBW(MAXIF), XCHW(MAXIF), XTBW(MAXIF)
      DOUBLE PRECISION TFREQ(MAXIF), XFREQ(MAXIF), DTMP
      DOUBLE PRECISION DEPS
      REAL             REPS
      CHARACTER BNDCOD(MAXIF)*8
C                                       Maximum relative spacings
C                                       of IEEE 64- and 32-bit
C                                       floating point numbers
C                                       used in frequency comparisons
      PARAMETER (DEPS = 2.2204460492503131D-16)
      PARAMETER (REPS = 1.192092896E-7)
      SAVE NUMIF, TMATCH
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DFRV.INC'
      INCLUDE 'TABLES.INC'
C-----------------------------------------------------------------------
      IERR = 0
      CALL COPY (256, CATIN, CATIEQ)
      REFF = CATDEQ(KDCRV+JLOCF)
C                                       If we are reading an FR table
C                                       now, jump to section that does
C                                       the FREQID comparison and sets
C                                       up the full mapping function.
      IF (FRTLOD) GO TO 300
C                                       Are there any FQ tables.
      CALL FNDEXT ('FQ', CATIN, FQVER)
      IF (FQVER.EQ.0) THEN
         IERR = 1
         WRITE (MSGTXT,1000)
         GO TO 990
         END IF
      IF (FQVER.GT.1) THEN
         IERR = 1
         WRITE (MSGTXT,1040)
         GO TO 990
         END IF
C                                       Initialize
      DO 110 I = 1, MAXFQI
         FQMAP(I) = 0
         TMATCH(I) = .FALSE.
         DO 100 II = 1, MAXIF
            FQFREQ(II,I) = 0.D0
            FQCHW(II,I)  = 0.0
            FQTBW(II,I)  = 0.0
            FQSIDE(II,I) = 0
            FQBCOD(II,I) = ' '
  100       CONTINUE
  110    CONTINUE
C                                       Read FQ table, loading up
C                                       the relevant information.
      IVER = 1
      CALL FQINI ('READ', TABUF1, DISK, ISLOT, IVER, CATIN, LUN,
     *   IFQRNO, FQKOLS, FQNUMV, NUMIF, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
      NENTRY = TABUF1(5)
      MXFREX = -1
C                                       Read entries
      DO 150 I = 1, NENTRY
         IFQRNO = I
         CALL TABFQ ('READ', TABUF1, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *      FQID, TFREQ, TQCHW, TQTBW, TQSIDE, BNDCOD, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR, IVER
            GO TO 990
            END IF
C
         MXFREX = MAX (MXFREX, FQID)
         FQCOUN(FQID) = 1
         DO 140 J = 1, NUMIF
            FQFREQ(J,FQID) = TFREQ(J) + REFF
            FQBCOD(J,FQID) = BNDCOD(J)
  140       CONTINUE
         CALL RCOPY (NUMIF, TQCHW, FQCHW(1,FQID))
         CALL RCOPY (NUMIF, TQTBW, FQTBW(1,FQID))
         CALL COPY  (NUMIF, TQSIDE, FQSIDE(1,FQID))
  150    CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 1, IFQRNO, TABUF1, TABUF1, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'MAPFQI', IERR)
         GO TO 990
         END IF
      FRTLOD = .TRUE.
      GO TO 999
C                                       Read in the FR table and
C                                       compare freqids, setting up the
C                                       renumbering lookup table if
C                                       necessary.
  300 CALL FNDEXT ('FR', CATIN, FRVER)
      IF (FRVER.EQ.0) THEN
         IERR = 1
         WRITE (MSGTXT,1030)
         GO TO 990
         END IF
      IF (FRVER.GT.1) THEN
         IERR = 1
         WRITE (MSGTXT,1050)
         GO TO 990
         END IF
C                                       Read FR tables
      CALL FRINI ('READ', TABUF1, DISK, ISLOT, FRVER, CATIN,
     *   LUN, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('CLOS', 'FRINI', 'MAPFQI', IERR)
         GO TO 990
         END IF
      NENTRY = TABUF1(5)
      NREFF = TABRFQ
C
      DO 370 I = 1, NENTRY
         IFRRNO = I
         CALL FRTAB ('READ', TABUF1, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('CLOS', 'FRTAB', 'MAPFQI', IERR)
            GO TO 990
            END IF
C                                       Reorder if necessary
         IF (I.EQ.1) DTMP = 1.0D20
         DO 330 J = 1, NUMIF
            FRORD = J
            IF (REORDR(I)) FRORD = FORDER(J,I)
            SIGN = 1
            IF (ISIDEB(J).LT.0) SIGN = -1
            IF (SIGN.LT.0) THEN
               XFREQ(FRORD) = BANDFR(J)
     *                        + (2.0 * REFPIX - NOCHAN - 1.0)
     *                        * CHWID(J)
            ELSE
               XFREQ(FRORD) = BANDFR(J)
            END IF
            XCHW(FRORD)  = CHWID(J)
            XTBW(FRORD)  = TBANDW(J)
            XSIDE(FRORD) = SIGN * ISIDEB(J)
            IF (I.EQ.1) DTMP = MIN (DTMP, XFREQ(FRORD))
  330       CONTINUE
C                                       Adjust to ensure that IF 1 has
C                                       the reference frequency
         CALL DETORD (XFREQ, NUMIF, FTEMP, LOWIF)
C                                       Does it match with an existing
C                                       freqid
         DO 360 II = 1,MXFREX
C                                       Do absolute frequencies match?
            MATCH = .TRUE.
            DO 350 J = 1,NUMIF
               IF (ABS(FQFREQ(J, II) - (XFREQ(J) + NREFF))
     *            .GT.(8.0D0 * FQFREQ(J, II) * DEPS)) THEN
                  MATCH = .FALSE.
               END IF
               IF (ABS(FQCHW(J, II) - XCHW(J))
     *            .GT.(2.0D0 * FQCHW(J, II) * REPS)) THEN
                  MATCH = .FALSE.
               END IF
               IF (ABS(FQTBW(J, II) - XTBW(J))
     *            .GT.(2.0D0 * FQTBW(J, II) * REPS)) THEN
                  MATCH = .FALSE.
               END IF
               IF (FQSIDE(J,II).NE.XSIDE(J)) MATCH = .FALSE.
 350           CONTINUE
            IF (MATCH) THEN
               IF (TMATCH(II)) GO TO 370
               TMATCH(II) = .TRUE.
               FQMAP(II) = IFQID
               IF (II.NE.IFQID) FRTCHA = .TRUE.
               GO TO 370
               END IF
 360        CONTINUE
C                                       New freqid, assign number
         IF (.NOT.MATCH) THEN
            IF (MXFREX.LT.MAXFQI) THEN
               MXFREX = MXFREX + 1
               FQMAP(MXFREX) = IFQID
               FRTCHA = .TRUE.
            ELSE
               WRITE (MSGTXT, 1360) MAXFQI
               CALL MSGWRT (8)
               IERR = 1
               GO TO 999
               END IF
            END IF
 370     CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 1, IFRRNO, TABUF1, TABUF1, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'MAPFQI', IERR)
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAPFQI: NO FQ TABLES ATTACHED TO DATA')
 1010 FORMAT ('MAPFQI: ERROR ',I3,' OPENING FQ TABLE')
 1020 FORMAT ('MAPFQI: ERROR ',I3,' READING FQ TABLE')
 1030 FORMAT ('MAPFQI: NO FR TABLES FOUND')
 1040 FORMAT ('MAPFQI: > 1 FQ TABLES, NOT LEGAL')
 1050 FORMAT ('MAPFQI: > 1 FR TABLES, NOT LEGAL')
 1360 FORMAT ('EXCEEDED LIMIT OF ', I4, ' FREQUENCY IDS: ABORTING')
      END
      SUBROUTINE MAPCQI (DISK, ISLOT, CATIN, LUN, JBUFF, ICORR, IFILT,
     *   TAVG, NCORR, IRET)
C----------------------------------------------------------------------
C   Set up a table of existing correlation modes in a uv-data file
C   Inputs:
C      DISK     I         Disk volume
C      ISLOT    I         Catalog slot number
C      CATIN    I(256)    Catalog header block of input file
C      LUN      I         LUN for table I/O
C   Input/output:
C      JBUFF    I(512)    Buffer for table I/O
C   Output:
C      ICORR    I(MAXCID) Correlation id.'s
C      IFILT    I(MAXCID) Filter id.'s
C      TAVG     R(MAXCID) Correlator filter averaging time
C      NCORR    I         No. of entries in (ICORR,IFILT,TAVG)
C      IRET     I         Return code (0=>ok, else error)
C----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      REAL      TAVG(MAXCID)
      INTEGER   CATIN(256), JBUFF(512), ICORR(MAXCID), IFILT(MAXCID),
     *   DISK, ISLOT, LUN, NCORR, IRET
C
      INCLUDE 'INCS:PCQV.INC'
      CHARACTER LTAPCQ(MAXIF)*8
      DOUBLE PRECISION DFRQCQ(MAXIF), DCBWCQ(MAXIF)
      REAL      TAVGCQ(MAXIF)
      INTEGER   ICQVER, IVER, ICQRNO, CQKOLS(MAXCQC), CQNUMV(MAXCQC),
     *   NOIFCQ, IERR, NREC, JREC, IFQDCQ, ISUBCQ, NFFTCQ(MAXIF),
     *   NCHCQ(MAXIF), NSAVCQ(MAXIF), NOVSCQ(MAXIF), NZPDCQ(MAXIF),
     *   IFLTCQ(MAXIF), NBITCQ(MAXIF), IOVLCQ(MAXIF), JIF, I, J, ID,
     *   JCOR
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      NCORR = 0
C                                       Is there a CQ table ?
      CALL FNDEXT ('CQ', CATIN, ICQVER)
      IF (ICQVER.LE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1020) DISK, ISLOT
         GO TO 990
         END IF
C                                       Read CQ table, loading the
C                                       relevant information
      IVER = 1
      CALL CQINI ('READ', JBUFF, DISK, ISLOT, IVER, CATIN, LUN,
     *   ICQRNO, CQKOLS, CQNUMV, NOIFCQ, IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C
      NREC = JBUFF(5)
C                                       Loop over all entries
      DO 30 JREC = 1,NREC
         ICQRNO = JREC
         CALL TABCQ ('READ', JBUFF, ICQRNO, CQKOLS, CQNUMV, NOIFCQ,
     *      IFQDCQ, ISUBCQ, NFFTCQ, NCHCQ, NSAVCQ, DFRQCQ, DCBWCQ,
     *      LTAPCQ, NOVSCQ, NZPDCQ, IFLTCQ, TAVGCQ, NBITCQ, IOVLCQ,
     *      IERR)
         IF (IERR.NE.0) THEN
            IRET = 3
            WRITE (MSGTXT,1250) IERR
            GO TO 990
            END IF
C                                       Loop over IF
         DO 20 JIF = 1,NOIFCQ
C                                       Extract correlation_id and
C                                       filter_id. Note: filter_id's
C                                       of less than zero are
C                                       interpreted as corr_id = 1
C                                       filter_id = 0 (no filter).
C                                       ICQFLT = 256 (corr_id - 1)
C                                          + filter_id
            ID = MAX (IFLTCQ(JIF), 0)
            J = MOD (ID, 256)
            I = (ID - J) / 256 + 1.1
C                                       First IF
            IF (JIF.EQ.1) THEN
C                                       Is this one known
               DO 10 JCOR = 1,NCORR
                  IF ((ICORR(JCOR).EQ.I) .AND.  (IFILT(JCOR).EQ.J) .AND.
     *               (ABS(TAVG(JCOR)-TAVGCQ(JIF)).LT.0.001)) GO TO 30
 10               CONTINUE
               NCORR = NCORR + 1
               ICORR(NCORR) = I
               IFILT(NCORR) = J
               TAVG(NCORR) = TAVGCQ(JIF)
            ELSE
C                                       No IF-dependence expected
               IF ((I.NE.ICORR(NCORR)).OR.(J.NE.IFILT(NCORR)).OR.
     *            (TAVGCQ(JIF).NE.TAVG(NCORR))) THEN
                  IRET = 4
                  WRITE (MSGTXT,1270) JREC, JIF
                  GO TO 990
                  END IF
               END IF
 20         CONTINUE
 30      CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, ICQRNO, JBUFF, JBUFF, IERR)
      IF (IERR.NE.0) THEN
         IRET = 5
         WRITE (MSGTXT,1350) IERR
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C                                       Exit
 999  RETURN
C----------------------------------------------------------------------
 1020 FORMAT ('MAPCQI: MISSING CQ TABLE ON DISK:',I3,' CNO:',I4)
 1040 FORMAT ('MAPCQI: ERROR',I4,' RETURNED BY CQINI')
 1250 FORMAT ('MAPCQI: ERROR',I4,' RETURNED BY TABCQ')
 1270 FORMAT ('MAPCQI: UNEXPECTED CQ DEPENDENCE: REC:',I4,' IF:',I4)
 1350 FORMAT ('MAPCQI: ERROR',I4,' CLOSING CQ TABLE')
      END
      INTEGER FUNCTION FQIFNC (FQI)
C-----------------------------------------------------------------------
C   FQIFNC looks through the freqid mapping function to determine if
C   the freqid is to be renumbered, if so it does it.  It also checks
C   to see if the FQI is wanted at all.
C   Input:
C      FQI     I       freqid number
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   FQI, I
      INCLUDE 'DATSEL.INC'
C-----------------------------------------------------------------------
      FQIFNC = -1
C                                       selected?
      IF (NUMACC.GT.0) THEN
         DO 10 I = 1,NUMACC
            IF (FQINCO(I).EQ.FQI) GO TO 20
 10         CONTINUE
         GO TO 999
         END IF
C                                       check map
 20   DO 30 I = 1,MXFREX
         IF (FQMAP(I).EQ.FQI) THEN
            FQIFNC = I
            GO TO 999
            END IF
 30      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE WRITCL (CLBUFF, ICLRNO, CLKOLS, CLNUMV, SUBARR, STIME,
     *   LTIME, NUMPOL, NUMIF, CURSOU, CURFQI, ANTUP, NUMANT, USINT,
     *   IRET)
C-----------------------------------------------------------------------
C   Routine to write the CL entries required to cover the time range
C   defined by the NX entries.
C   Inputs:
C      CLBUFF   I(512)      CL table buffer
C      ICLRNO   I           Next CL row number to be written
C      CLKOLS   I(MAXCLC)   Array defining the data types in the
C                           CL columns.
C      CLNUMV   I(MAXCLC)   Array defining the number of entries per
C                           column
C      SUBARR   I           Subarray number
C      STIME    R           Start time of NX record
C      LTIME    R           Stop time of NX record
C      NUMPOL   I           # polzns in data
C      NUMIF    I           # IF's in data
C      CURSOU   I           Source number of scan
C      CURFQI   I           Freq id number of scan
C      ANTUP    I(MAXANT,*) Array specifiying which antennas are used
C                           for which subarray.
C      NUMANT   I           max # antennas
C      USINT    R           User spec interval for CL entries (min)
C   Output:
C      IRET     I           Error code: 0 => OK
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   CLBUFF(512), ICLRNO, CLKOLS(*), CLNUMV(*), SUBARR,
     *   CURSOU, CURFQI, ANTUP(MAXANT,*), NUMPOL, NUMIF, NUMANT, IRET
      REAL    STIME, LTIME, USINT
C
      INTEGER   REFA(2,MAXIF), NENT, IANT, I
      REAL      DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2), CLOCK(2),
     *   DCLOCK(2), DISP(2), DDISP(2), ONES(2,MAXIF), ZEROS(2,MAXIF),
     *   IFR, TIMER, INTERV
      DOUBLE PRECISION    GEODLY(3)
      DOUBLE PRECISION DSTIME, DLTIME
      INCLUDE 'INCS:DMSG.INC'
      DATA REFA /MAXIF*0, MAXIF*0/
      DATA GEODLY /3*0.0D0/
      DATA DOPOFF, MBDELY, CLOCK, DCLOCK /MAXIF*0.0, 6*0.0/
      DATA ATMOS, DATMOS, DISP, DDISP /6*0.0/
      DATA ZEROS /MAXIF*0.0, MAXIF*0.0/
      DATA ONES  /MAXIF*1.0, MAXIF*1.0/
      DATA IFR /0.0/
C-----------------------------------------------------------------------
C                                       Determine time range of scan
C                                       in minutes
      TIMER = (LTIME - STIME) * 24. * 60.
      NENT = TIMER / USINT
      DSTIME = STIME
      DLTIME = LTIME
      INTERV = USINT / 60. / 24.
C                                       Ensure entry written at
C                                       start of scan.
      DO 100 IANT = 1,NUMANT
         IF (ANTUP(IANT,SUBARR).EQ.1) THEN
            CALL TABCAL ('WRIT', CLBUFF, ICLRNO, CLKOLS, CLNUMV,
     *         NUMPOL, NUMIF, DSTIME, INTERV, CURSOU, IANT, SUBARR,
     *         CURFQI, IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *         CLOCK, DCLOCK, DISP, DDISP, ONES, ZEROS, ZEROS, ZEROS,
     *         ONES, REFA, IRET)
            IF (IRET.NE.0) GO TO 990
            END IF
 100     CONTINUE
C                                       Write entries at specified
C                                       time intervals
      DO 400 I = 1,NENT
         DSTIME = DSTIME + INTERV
         IF (DSTIME.GT.DLTIME) GO TO 500
         DO 200 IANT = 1,NUMANT
            IF (ANTUP(IANT,SUBARR).EQ.1) THEN
               CALL TABCAL ('WRIT', CLBUFF, ICLRNO, CLKOLS, CLNUMV,
     *            NUMPOL, NUMIF, DSTIME, INTERV, CURSOU, IANT, SUBARR,
     *            CURFQI, IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *            CLOCK, DCLOCK, DISP, DDISP, ONES, ZEROS, ZEROS, ZEROS,
     *            ONES, REFA, IRET)
               IF (IRET.NE.0) GO TO 990
               END IF
 200        CONTINUE
 400     CONTINUE
C                                       Check to see if we have entry
C                                       at end of scan
 500  IF (DSTIME.EQ.DLTIME) GO TO 999
C                                       Otherwise write entry for end
C                                       of scan
      DO 600 IANT = 1, NUMANT
         IF (ANTUP(IANT,SUBARR).EQ.1) THEN
            CALL TABCAL ('WRIT', CLBUFF, ICLRNO, CLKOLS, CLNUMV,
     *         NUMPOL, NUMIF, DLTIME, INTERV, CURSOU, IANT, SUBARR,
     *         CURFQI, IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *         CLOCK, DCLOCK, DISP, DDISP, ONES, ZEROS, ZEROS, ZEROS,
     *         ONES, REFA, IRET)
            IF (IRET.NE.0) GO TO 990
            END IF
 600     CONTINUE
      GO TO 999
C
 990  WRITE (MSGTXT,1000) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WRITCL: ERROR ',I3,' WRITING CL ENTRY')
      END
      SUBROUTINE FRQFID (DISK, ISLOT, IERR)
C-----------------------------------------------------------------------
C  Routine to clean up the frequency offsets after VLBA data read in.
C  Its tricky to do this on the fly, so its done here. Algorithm is:
C  (1) Determine base freq for each IF (header + FQ table offset)
C  (2) Run through SU table, get FRQOFF's for each IF add them to
C      base freqs determined in (1) in a large array.
C  (3) Run through CL table, when first encounter a source get its
C      time dependent freq offset, add it to big array, note what it
C      was, set refoff to zero.
C  (4) Next encounter, subtract first refoff from new refoff & so on.
C  (5) At end of CL table, run through SU table again, subtract base
C      frequencies from big array, remainder are the new SU offsets.
C-----------------------------------------------------------------------
      INTEGER   DISK, ISLOT, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXSOU
      PARAMETER (MAXSOU=2500)
C
      INTEGER   TVER, FREQID, LUN, ISBAND(MAXIF), I, IIF, NUMIF, NENTRY
      DOUBLE PRECISION SUFREQ(MAXSOU, MAXIF), REFF, FOFF(MAXIF),
     *   BASEF(MAXIF)
      REAL      CLOFFS(MAXSOU, MAXIF), FINC(MAXIF)
      LOGICAL   SUFOUN(MAXSOU), EXIST, TABLE, FITASC
      CHARACTER BNDCOD(MAXIF)*8
C                                       Declarations for SOUINI
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC), ISURNO, IFQ
      CHARACTER VELDEF*8, VELTYP*8
C                                       Declarations for TABSOU
      CHARACTER SOUNAM*16, TCALC*4
      REAL      FLUX(4,MAXIF)
      INTEGER   IDSOU, TQUAL
      DOUBLE PRECISION  FREQO(MAXIF), LSRVEL(MAXIF), LRESTF(MAXIF),
     *   BANDW, TRAEPO, TDCEPO, TRAAPP, TDCAPP, TEQUIN, TPMRA, TPMDEC,
     *   TRAOBS, TDECOB
C                                       Declerations for CALINI
      INTEGER   ICLRNO, CLKOLS(MAXCLC), CLNUMV(MAXCLC), CLANT, CLPOL,
     *   CLTERM
      REAL      GMMOD
C                                       Declerations for TABCAL
      INTEGER   REFA(2,MAXIF), CLSTA, CLARR, SOURID
      REAL      CLTINT, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2),
     *   CLOCK(2), DCLOCK(2), DISP(2), DDISP(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF), IFR
      DOUBLE PRECISION GEODLY(10), CLTIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'TABLES.INC'
C-----------------------------------------------------------------------
      LUN = 45
C                                       Get base frequencies
      REFF = CATD(KDCRV+JLOCF)
C                                       Assume 1 FQID for now
      TVER = 1
      FREQID = 1
      CALL CHNDAT ('READ', TABUF1, DISK, ISLOT, TVER, CATBLK, LUN,
     *   NUMIF, FOFF, ISBAND, FINC, BNDCOD, FREQID, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('READ', 'CHNDAT', 'FRQFID', IERR)
         GO TO 999
         ENDIF
      DO 20 I = 1, NUMIF
         BASEF(I) = REFF + FOFF(I)
   20    CONTINUE
C                                       Read through SU table & get
C                                       source dependent offsets.
C                                       Maybe better to use SO table
C                                       because it deals with FREQID.
C                                       First, init table
      DO 110 I = 1,MAXSOU
         SUFOUN(I) = .FALSE.
         DO 100 IIF = 1, MAXIF
            SUFREQ(I,IIF) = -1.0D10
            CLOFFS(I,IIF) = 0.0
  100       CONTINUE
  110    CONTINUE
C
      TVER = 1
      CALL SOUINI ('READ', TABUF1, DISK, ISLOT, TVER, CATBLK, LUN,
     *   NUMIF, VELTYP, VELDEF, IFQ, ISURNO, SUKOLS, SUNUMV, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('READ', 'SOUINI', 'FRQFID', IERR)
         GO TO 999
         ENDIF
      NENTRY = TABUF1(5)
C                                       Read entries
      DO 150 I = 1, NENTRY
         ISURNO = I
         CALL TABSOU ('READ', TABUF1, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, TQUAL, TCALC, FLUX, FREQO, BANDW, TRAEPO, TDCEPO,
     *      TEQUIN, TRAAPP, TDCAPP, TRAOBS, TDECOB, LSRVEL, LRESTF,
     *      TPMRA, TPMDEC, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'TABSOU', 'FRQFID', IERR)
            GO TO 999
            END IF
C
         IF (IDSOU.GT.MAXSOU) THEN
            WRITE (MSGTXT,1000) IDSOU
            IERR = 1
            GO TO 990
            END IF
         DO 120 IIF = 1, NUMIF
            SUFREQ(IDSOU,IIF) = BASEF(IIF) + FREQO(IIF)
  120       CONTINUE
  150    CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 1, ISURNO, TABUF1, TABUF1, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'FRQFID', IERR)
         GO TO 999
         END IF
C                                       Now read through CL table
C                                       and examine CLTIME dependent
C                                       offsets.
      TVER = 1
      CALL ISTAB ('CL', DISK, ISLOT, TVER, LUN, TABUF1, TABLE,
     *   EXIST, FITASC, IERR)
      IF (EXIST) THEN
         CLANT = 0
         CLPOL = CATBLK(KINAX+JLOCS)
         IF (CLPOL.GT.2) CLPOL = 2
         NUMIF = 0
         CALL CALINI ('WRIT', TABUF1, DISK, ISLOT, TVER, CATBLK, LUN,
     *      ICLRNO, CLKOLS, CLNUMV, CLANT, CLPOL, NUMIF, CLTERM, GMMOD,
     *      IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'CALINI', 'FRQFID', IERR)
            GO TO 999
            END IF
         NENTRY = TABUF1(5)
C                                       Read entries
         DO 250 I = 1,NENTRY
            ICLRNO = I
            CALL TABCAL ('READ', TABUF1, ICLRNO, CLKOLS, CLNUMV, CLPOL,
     *         NUMIF, CLTIME, CLTINT, SOURID, CLSTA, CLARR, FREQID,
     *         IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK,
     *         DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *         REFA, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('WRIT', 'TABCAL', 'FRQFID', IERR)
               GO TO 999
               END IF
C                                       Never encountered before
            IF (.NOT.SUFOUN(SOURID)) THEN
               SUFOUN(SOURID) = .TRUE.
               DO 180 IIF = 1,NUMIF
                  CLOFFS(SOURID,IIF) = DOPOFF(IIF)
                  SUFREQ(SOURID,IIF) = SUFREQ(SOURID,IIF) + DOPOFF(IIF)
                  DOPOFF(IIF) = 0.0
  180             CONTINUE
C                                       If encountered this source
C                                       before just adjust dopoff
            ELSE
               DO 190 IIF = 1,NUMIF
                  DOPOFF(IIF) = DOPOFF(IIF) - CLOFFS(SOURID,IIF)
  190             CONTINUE
               END IF
C                                       Rewrite CL row
            ICLRNO = I
            CALL TABCAL ('WRIT', TABUF1, ICLRNO, CLKOLS, CLNUMV, CLPOL,
     *         NUMIF, CLTIME, CLTINT, SOURID, CLSTA, CLARR, FREQID,
     *         IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK,
     *         DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *         REFA, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('WRIT', 'TABCAL', 'FRQFID', IERR)
               GO TO 999
               END IF
  250       CONTINUE
C                                       Close CL table
         CALL TABIO ('CLOS', 1, ICLRNO, TABUF1, TABUF1, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('CLOS', 'TABIO', 'FRQFID', IERR)
            GO TO 999
            END IF
         END IF
C                                       Now run through SU table
C                                       again, subtracting base
C                                       frequencies from SUFREQ
C                                       array and writing remainder
C                                       out as SU offsets.
      TVER = 1
      CALL SOUINI ('WRIT', TABUF1, DISK, ISLOT, TVER, CATBLK, LUN,
     *   NUMIF, VELTYP, VELDEF, IFQ, ISURNO, SUKOLS, SUNUMV, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('READ', 'SOUINI', 'FRQFID', IERR)
         GO TO 999
         ENDIF
      NENTRY = TABUF1(5)
C                                       Read entries
      DO 350 I = 1, NENTRY
         ISURNO = I
         CALL TABSOU ('READ', TABUF1, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, TQUAL, TCALC, FLUX, FREQO, BANDW, TRAEPO, TDCEPO,
     *      TEQUIN, TRAAPP, TDCAPP, TRAOBS, TDECOB, LSRVEL, LRESTF,
     *      TPMRA, TPMDEC, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'TABSOU', 'FRQFID', IERR)
            GO TO 999
            END IF
C                                       Get new SU offset
         DO 300 IIF = 1, NUMIF
            FREQO(IIF) = SUFREQ(IDSOU,IIF) - BASEF(IIF)
  300       CONTINUE
C                                       Write corrected row
         ISURNO = I
         CALL TABSOU ('WRIT', TABUF1, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, TQUAL, TCALC, FLUX, FREQO, BANDW, TRAEPO, TDCEPO,
     *      TEQUIN, TRAAPP, TDCAPP, TRAOBS, TDECOB, LSRVEL, LRESTF,
     *      TPMRA, TPMDEC, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'TABSOU', 'FRQFID', IERR)
            GO TO 999
            END IF
  350    CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 1, ISURNO, TABUF1, TABUF1, IERR)
      IF (IERR.NE.0) CALL TABERR ('CLOS', 'TABIO', 'FRQFID', IERR)
      GO TO 999
C
  990 CALL MSGWRT (8)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FRQFID: SOURCE ID ',I8,' > MAX ALLOWED')
      END
      SUBROUTINE SOUSEL (ISLOT, IERR)
C-----------------------------------------------------------------------
C   Fills in arrays of source numbers to be included or excluded.
C   Inputs:
C      ISLOT        I    Catalogue slot number
C   Inputs from common
C      XSOUR(30)    C*16 Names of up to 30 XSOURs, *=>all
C                        First character of name '-' => all except those
C                        specified.
C      SELQUA       I    XSOUR qualifiers to be selected, -1=>any.
C      XCALCO       C*4  Calibrator codes to select.
C                        '    '  => any,
C                        '*   ' => any non blank calibrator code.
C                        '-CAL' => blank only (no calibrators)
C                        anything else => matching CALcodes.
C   Output:
C      IERR         I    Return code, 0=>OK, otherwise XSOUR file
C                        exists but cannot be read.
C                        1=TABIO problem, 2=no XSOURs or calibrators
C   Output to common
C      ALLSKP       L    If true, all sources in current file to be
C                        skipped.
C      NSOUWD       I    Number of XSOURs included or excluded; if
C                        0 all XSOURs are included.
C      DOSWNT       L    If .TRUE. then XSOURs in SOUWAN are included
C                        If .FALSE. then excluded.
C      SOUWAN(30)   I    The XSOUR numbers of XSOURs included or
C                        excluded.
C-----------------------------------------------------------------------
      INTEGER   ISLOT, IERR
C
      CHARACTER SOUNAM*16, TMPSNM*16
      INTEGER   JERR, J, K, LUN, NCHAR, SRCFNC, NSOU, NSOURC, I, IVER,
     *   BADCNT, YSTBSZ
      LOGICAL   T, F, TABLE, EXIST, FITASC, ALLSOU, DESLT, DOCALC,
     *   DOQUAL, ANYCC, NOCAL, TAOPEN, DOAPPL
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'INCS:DSOV.INC'
      INCLUDE 'TABLES.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA YSTBSZ /XSTBSZ/
C-----------------------------------------------------------------------
C                                       Setup for CALCODE and qualifier
C                                       selection.
      IERR = 0
      LUN = 29
      TAOPEN = .FALSE.
      DOAPPL = .FALSE.
      DOQUAL = SELQUA.NE.-1
      ANYCC = XCALCO.EQ.'*   '
      DOCALC = (XCALCO.NE.' ') .AND. ((XCALCO.NE.'* ') .AND.
     *   (XCALCO.NE.'-CAL'))
      NOCAL = XCALCO.EQ.'-CAL'
      ALLSOU = F
      NSOU = 0
      CALL CFILL (MAXSOU, ' ', TSRCN)
C                                       Check if XSOUR/calib excluded
C                                       or if all are included
      DO 30 J = 1,30
C                                       XSOURs
         ALLSOU = ALLSOU .OR. (XSOUR(J)(1:1).EQ.'*')
         DESLT = XSOUR(J)(1:1).EQ.'-'
C                                       Find number of XSOURs
         IF (XSOUR(J).NE.' ') NSOU = J
C                                       Remove any minus sign
         IF (DESLT) THEN
            DOSWNT = F
            TMPSNM = XSOUR(J)(2:16)
            XSOUR(J) = TMPSNM
            END IF
 30      CONTINUE
C                                       If not using source mapping
C                                       function then must use SO
C                                       table.
      IF (.NOT.DOSMAP) THEN
C                                       See if SO file exists.
         CALL ISTAB ('SO', DISOUT, ISLOT, 1, LUN, TABUF1, TABLE,
     *      EXIST, FITASC, JERR)
         IF ((.NOT.EXIST) .AND. (JERR.EQ.2)) GO TO 900
         IF ((.NOT.EXIST) .OR. (.NOT.TABLE) .OR. (JERR.NE.0)) THEN
            IERR = 1
            IF (.NOT.EXIST) THEN
               WRITE (MSGTXT,1000) JERR
            ELSE IF (TABLE) THEN
               WRITE (MSGTXT,1001) JERR
            ELSE
               MSGTXT = 'SO FILE DAMAGED - NO LONGER A TABLE'
               END IF
            GO TO 980
            END IF
C                                       Open SO table
         IVER = 1
         CALL SOINI ('READ', TABUF1, DISOUT, ISLOT, IVER, CATBLK,
     *      LUN, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('CLOS', 'SOINI', 'SOUSEL', IERR)
            GO TO 999
            END IF
C                                       Get number of XSOURs.
         NSOURC = TABUF1(5)
         TAOPEN = .TRUE.
C                                       Check if empty
         IF (NSOURC.LE.0) GO TO 900
         DOSWNT = T
         NSOUWD = 0
C                                       Loop through just to load up
C                                       TSRCN array for printing
         DO 50 I = 1,NSOURC
C                                       Read record
            CALL SOTAB ('READ', TABUF1, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('CLOS', 'SOTAB', 'SOUSEL', IERR)
               GO TO 999
               END IF
            CALL CHTRIM (SAUCE, 16, SOUNAM, NCHAR)
            TSRCN(ISOUR) = SOUNAM
   50       CONTINUE
         ISORNO = 1
C                                       Make sure need to look at table
         ALLSOU = ALLSOU .OR. (NSOU.LE.0)
         IF ((ALLSOU) .AND.
     *      (.NOT.(DOCALC.OR.ANYCC.OR.NOCAL.OR.DOQUAL))) GO TO 900
C                                       Loop through records
         IF (NSOU.LE.1) NSOU = 1
         BADCNT = 0
         DO 100 I = 1,NSOURC
C                                       Read record
            CALL SOTAB ('READ', TABUF1, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('CLOS', 'SOTAB', 'SOUSEL', IERR)
               GO TO 999
               END IF
            CALL CHTRIM (SAUCE, 16, SOUNAM, NCHAR)
            TSRCN(ISOUR) = SOUNAM
C                                       Search lists
            DO 80 J = 1,NSOU
C                                       XSOUR list
               CALL CHTRIM (XSOUR(J), 16, TMPSNM, NCHAR)
               IF ((.NOT.ALLSOU) .AND. (TMPSNM.NE.SOUNAM)) GO TO 80
C                                       Check qualifier
               IF ((DOQUAL) .AND. (QUAL.NE.SELQUA)) GO TO 80
C                                       Check CALCODE
               IF (.NOT.DOAPPL) THEN
                  IF ((ANYCC)  .AND. (CALCOD.EQ.' ')) GO TO 80
                  IF ((NOCAL)  .AND. (CALCOD.NE.' ')) GO TO 80
                  IF ((DOCALC) .AND. (CALCOD.NE.XCALCO)) GO TO 80
                  END IF
C                                       Redundancy check
               IF (NSOUWD.GE.1) THEN
                  DO 40 K = 1,NSOUWD
                     IF (SOUWAN(K).EQ.ISOUR) GO TO 80
   40             CONTINUE
                  END IF
C                                       Add XSOUR
               IF (NSOUWD.LT.YSTBSZ) THEN
                  NSOUWD = NSOUWD + 1
                  SOUWAN(NSOUWD) = ISOUR
C                                       Overflowed array
               ELSE
                  BADCNT = BADCNT + 1
                  END IF
   80          CONTINUE
  100       CONTINUE
         IF (BADCNT.GT.0) THEN
            WRITE (MSGTXT,1100) BADCNT, YSTBSZ
            CALL MSGWRT (6)
            END IF
C                                       No XSOURs found
         IF (NSOUWD.LE.0) THEN
            IERR = 0
            NSOUWD = 1
            ALLSKP = .TRUE.
            WRITE (MSGTXT,1101)
            GO TO 980
            END IF
         GO TO 900
         END IF
C                                       If data being appended to
C                                       existing file then we must
C                                       use the mapping functions
C                                       to determine source selection.
      IF (DOSMAP) THEN
C                                       Make sure need to look at table
         ALLSOU = ALLSOU .OR. (NSOU.LE.0)
         IF ((ALLSOU) .AND.
     *      (.NOT.(DOCALC.OR.ANYCC.OR.NOCAL.OR.DOQUAL))) GO TO 900
C                                       Loop through records
         IF (NSOU.LE.1) NSOU = 1
         NSOUWD = 0
         DO 200 I = 1, MXSRCN
            ISOUR = I
            IF (SRCCHA) ISOUR = SRCFNC (I)
            SOUNAM = SRCNAM(ISOUR)
            QUAL   = SRCQUL(ISOUR)
            CALCOD = SRCCAL(ISOUR)
C                                       Search lists
            DO 180 J = 1,NSOU
C                                       XSOUR list
               CALL CHTRIM (XSOUR(J), 16, TMPSNM, NCHAR)
               IF ((.NOT.ALLSOU) .AND. (TMPSNM.NE.SOUNAM)) GO TO 180
C                                       Check qualifier
               IF ((DOQUAL) .AND. (QUAL.NE.SELQUA)) GO TO 180
C                                       Check CALCODE
               IF (.NOT.DOAPPL) THEN
                  IF ((ANYCC)  .AND. (CALCOD.EQ.' ')) GO TO 180
                  IF ((NOCAL)  .AND. (CALCOD.NE.' ')) GO TO 180
                  IF ((DOCALC) .AND. (CALCOD.NE.XCALCO)) GO TO 180
                  END IF
C                                       Redundancy check
               IF (NSOUWD.GE.1) THEN
                  DO 140 K = 1,NSOUWD
                     IF (SOUWAN(K).EQ.ISOUR) GO TO 180
  140             CONTINUE
                  END IF
C                                       Add XSOUR
               IF (NSOUWD.LT.YSTBSZ) THEN
                  NSOUWD = NSOUWD + 1
                  SOUWAN(NSOUWD) = ISOUR
C                                       Overflowed array
               ELSE
                  BADCNT = BADCNT + 1
                  END IF
  180          CONTINUE
  200       CONTINUE
C                                       No XSOURs found
         IF (NSOUWD.LE.0) THEN
            IERR = 0
            NSOUWD = 1
            ALLSKP = .TRUE.
            WRITE (MSGTXT,1101)
            GO TO 980
            END IF
         END IF
C
 900  IF (TAOPEN) CALL TABIO ('CLOS', 0, I, TABUF1, TABUF1, JERR)
      GO TO 999
C                                       Error
 980  CALL MSGWRT (8)
      IF (TAOPEN) CALL TABIO ('CLOS', 0, I, TABUF1, TABUF1, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOUSEL OR ISTAB: ERROR',I3,' OPENING SOURCE TABLE')
 1001 FORMAT ('SOUSEL/ISTAB: ERROR',I4,' READING SOURCE TABLE')
 1100 FORMAT ('SOUSEL: ',I5,' MORE SOURCES SELECTED THAN ',I6,
     *   ' ALLOWED')
 1101 FORMAT ('All sources in this file rejected by selection criteria')
      END
      LOGICAL FUNCTION WANSRC (SRC)
C-----------------------------------------------------------------------
C   WANSRC looks through the list of acceptable sources to determine if
C   the source is wanted.
C  Input:
C    SRC     I       source number
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SRC, I
      INCLUDE 'DATSEL.INC'
C-----------------------------------------------------------------------
      WANSRC = .FALSE.
      DO 10 I = 1, NSOUWD
         IF (SOUWAN(I).EQ.SRC) THEN
            WANSRC = .TRUE.
            GO TO 999
            END IF
 10      CONTINUE
      IF (NSOUWD.EQ.0) WANSRC = .TRUE.
C
 999  RETURN
      END
      LOGICAL FUNCTION WANTIM (TIME)
C-----------------------------------------------------------------------
C   WANTIM checks the time of the record to see if it is wanted.
C  Input:
C    TIME     R        time of record (days)
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      REAL      TIME
      INCLUDE 'DATSEL.INC'
C-----------------------------------------------------------------------
      WANTIM = (TIME.GE.TSTART) .AND. (TIME.LE.TEND)
C
 999  RETURN
      END
      LOGICAL FUNCTION WANFQI (JFQ)
C-----------------------------------------------------------------------
C   WANFQI looks through the list of acceptable fqids to determine if
C   the incoming fqid is wanted.
C  Input:
C    JFQ     I       source number
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER JFQ, I
      INCLUDE 'DATSEL.INC'
C-----------------------------------------------------------------------
      WANFQI = .FALSE.
      DO 100 I = 1, NUMACC
         IF (FQINCO(I).EQ.JFQ) THEN
            WANFQI = .TRUE.
            GO TO 999
            END IF
 100     CONTINUE
      IF (NUMACC.EQ.0) WANFQI = .TRUE.
C
 999  RETURN
      END
      SUBROUTINE UPDKEY (BUFFER, KEYWRD, KEYTYP, KEYVAL, IERR)
C-----------------------------------------------------------------------
C   Routine which updates a keyword-value pairs of an existing
C   calibration table.
C   Inputs:
C      BUFFER   I(*)   Work buffer of open table
C      KEYWRD   C*8    Keyword name
C      KEYTYP   I      Keyword type
C      KEYVAL   I      Keyword value
C   Outputs:
C      IERR     I      Error code, 0 => OK, anything else => problem
C-----------------------------------------------------------------------
      CHARACTER KEYWRD*8
      INTEGER   LOCS, KEYTYP, KEYNUM
      INTEGER   KEYVAL, BUFFER(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
      LOCS = 1
      KEYNUM = 1
C
      CALL TABKEY ('WRIT', KEYWRD, KEYNUM, BUFFER, LOCS, KEYVAL, KEYTYP,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UPDKEY: ERROR ',I3,' UPDATING CL KEYWORDS')
      END
      SUBROUTINE IMORDR (DISK, CNO, LUN, NUMBND, CATIN, IERR)
C-----------------------------------------------------------------------
C   Routine to run through IM table from point at which new rows have
C   been added and correct the freqid numbers and also ensure that
C   arrays have been ordered in frequency.  Source and antenna numbers
C   are corrected as the IM table is read in, but it's easier to do the
C   FQID numbers here because I need the original fqid when I do the
C   frequency re-ordering.
C   Inputs:
C      DISK    I        Volume on which data reside
C      CNO     I        Catalogue number of data
C      LUN     I        Main lun to use.
C      CATIN   I(256)   Catalogue header.
C   Output
C      IERR    I        Error code
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER    DISK, CNO, LUN, NUMBND, CATIN(256), IERR
C
      INTEGER   NUMIMT, IVER, K, J, II, FQIFNC, NENTRY, I, ROWRED,
     *   FRORD
      REAL      RTEMP(MAXIF)
      DOUBLE PRECISION DTEMP(MAXIF,10)
      INCLUDE 'DATSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DIMV.INC'
      INCLUDE 'ORDER.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'SCRBFS.INC'
C-----------------------------------------------------------------------
      CALL COPY (256, CATIN, CATIEQ)
C                                       Determine # tables
      NUMIMT= 1
      CALL FNDEXT ('IM', CATIEQ, NUMIMT)
      IF (NUMIMT.EQ.0) GO TO 999
C                                       First, ensure that the IM
C                                       table has the corrected
C                                       source/antenna numbers and
C                                       has had its band related
C                                       information reordered.
      DO 500 I = 1,NUMIMT
         IVER = I
C                                       Open IM table for READ
         CALL IMINI ('READ', TABUF1, DISK, CNO, IVER, CATIEQ, LUN,
     *      IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'IMINI', 'IMORDR', IERR)
            GO TO 999
            END IF
         NENTRY = TABUF1(5)
         CALL TABIO ('CLOS', 1, IIMRNO, TABUF1, TABUF1, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('CLOS', 'TABIO', 'IMORDR', IERR)
            GO TO 999
            END IF
C                                       Open IM table for WRIT
         CALL IMINI ('WRIT', TABUF1, DISK, CNO, IVER, CATIEQ, LUN,
     *      IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'IMINI', 'IMORDR', IERR)
            GO TO 999
            END IF
         IIMRNO = IMROST
C                                       Loop over rows, reorganize
         DO 400 II = IMROST,NENTRY
            ROWRED = IIMRNO
            CALL IMTAB ('READ', TABUF1, NUMBND, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('READ', 'IMTAB', 'IMORDR', IERR)
               GO TO 999
               END IF
C
            IF (REORDR(IFQID)) THEN
               CALL COPY (NUMBND, FREQVR, RTEMP)
               IFQID = FQDUPS(IFQID)
               DO 10 K = 1,NUMBND
                  FRORD = FORDER(K,IFQID)
                  FREQVR(FRORD) = RTEMP(K)
 10               CONTINUE
               K = MAXPOL * MAXIF
               CALL DPCOPY (K, PDELA1, DTEMP)
               DO 30 K = 1,NUMBND
                  FRORD = FORDER(K,IFQID)
                  DO 20 J = 1, MAXPOL
                     PDELA1(FRORD,J) = DTEMP(K,J)
 20                  CONTINUE
 30               CONTINUE
               K = MAXPOL * MAXIF
               CALL DPCOPY (K, PDELA2, DTEMP)
               DO 50 K = 1,NUMBND
                  FRORD = FORDER(K,IFQID)
                  DO 40 J = 1, MAXPOL
                     PDELA2(FRORD,J) = DTEMP(K,J)
 40                  CONTINUE
 50               CONTINUE
               K = MAXPOL * MAXIF
               CALL DPCOPY (K, PRATE1, DTEMP)
               DO 70 K = 1,NUMBND
                  FRORD = FORDER(K,IFQID)
                  DO 60 J = 1, MAXPOL
                     PRATE1(FRORD,J) = DTEMP(K,J)
 60                  CONTINUE
 70               CONTINUE
               K = MAXPOL * MAXIF
               CALL DPCOPY (K, PRATE2, DTEMP)
               DO 90 K = 1,NUMBND
                  FRORD = FORDER(K,IFQID)
                  DO 80 J = 1, MAXPOL
                     PRATE2(FRORD,J) = DTEMP(K,J)
 80                  CONTINUE
 90               CONTINUE
               END IF
C                                       Update FQID.
            IFQID = FQIFNC(IFQID)
            IF (IFQID.LT.1) GO TO 400
C                                       Rewrite row
            IIMRNO = ROWRED
            CALL IMTAB ('WRIT', TABUF1, NUMBND, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('WRIT', 'IMTAB', 'IMORDR', IERR)
               GO TO 999
               END IF
 400        CONTINUE
C                                       Close table
         CALL TABIO ('CLOS', 1, IIMRNO, TABUF1, TABUF1, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('CLOS', 'TABIO', 'IMORDR', IERR)
            GO TO 999
            END IF
C                                       Ensure is in time-ant order
         CALL IMSORT (DISK, CNO, IVER, CATIEQ, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('SORT', 'IMSORT', 'IMORDR', IERR)
            GO TO 999
            END IF
 500     CONTINUE
C
  999 RETURN
      END
      SUBROUTINE MCORDR (DISK, CNO, CATIN, IERR)
C-----------------------------------------------------------------------
C   Ensure that MC table is in time order.  FQIDs should also be
C   corrected here but there is insufficient data to do this in model-
C   components tables generated by the VLBA correlator.
C   Inputs:
C      DISK    I        Volume on which data reside
C      CNO     I        Catalogue number of data
C      CATIN   I(256)   Catalogue header.
C   Output
C      IERR    I        Error code
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER    DISK, CNO, CATIN(256), IERR
C
      INTEGER   NUMMCT, IVER, I
      INCLUDE 'DATSEL.INC'
      INCLUDE 'ORDER.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'SCRBFS.INC'
C-----------------------------------------------------------------------
      CALL COPY (256, CATIN, CATIEQ)
C                                       Determine # tables
      NUMMCT= 1
      CALL FNDEXT ('MC', CATIEQ, NUMMCT)
      IF (NUMMCT.EQ.0) GO TO 999
C                                       Sort each table
      DO 500 I = 1,NUMMCT
         IVER = I
         CALL MCSORT (DISK, CNO, IVER, CATIEQ, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('SORT', 'MCSORT', 'MCORDR', IERR)
            GO TO 999
            END IF
  500    CONTINUE
C
  999 RETURN
      END
      SUBROUTINE SUTIDY (DISK, ISLOT, IERR)
C-----------------------------------------------------------------------
C   Routine to tidy up the SU table after the loading has finished.
C   During loading the array SOCOUN(*) keeps track of how many entries
C   a particular source has, if any have zero then they are removed
C   from the SU table.
C   Inputs:
C      DISK    I   Volume on which data reside
C      ISLOT   I   Catalogue number of file
C   Outputs:
C      IERR    I   Error code, 0 => OK.
C-----------------------------------------------------------------------
      INTEGER   DISK, ISLOT, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IN, OUT, LUNIN, LUNOUT, I, NSOU
C                                       Declarations for SOUINI
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC), ISURNO, NUMBND,
     *   IFQ, SOKOLS(MAXSUC), SONUMV(MAXSUC), ISORNO
      CHARACTER VELTYP*8, VELDEF*8
C                                       Declarations for TABSOU
      CHARACTER SOUNAM*16, TCALC*4
      REAL      FLUX(4,MAXIF)
      INTEGER   IDSOU, TQUAL
      DOUBLE PRECISION  FREQO(MAXIF), LSRVEL(MAXIF), LRESTF(MAXIF),
     *   BANDW, TRAEPO, TDCEPO, TRAAPP, TDCAPP, TEQUIN, TPMRA, TPMDEC,
     *   TRAOBS, TDECOB
      INCLUDE 'DATSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'TABLES.INC'
C-----------------------------------------------------------------------
C                                       Copy SU 1 -> SU 2
      IN = 1
      OUT = 2
      LUNIN = 45
      LUNOUT = 46
      I = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('SU', IN, OUT, LUNIN, LUNOUT, DISK, DISK, ISLOT,
     *   ISLOT, CATBLK, TABUF1, TABUF2, IERR)
      MSGSUP = I
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Delete SU 1
      CALL RMEXT (DISK, ISLOT, 'SU', IN, CATBLK, TABUF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                       Open SU table #2
      CALL SOUINI ('READ', TABUF2, DISK, ISLOT, OUT, CATBLK, LUNOUT,
     *   NUMBND, VELTYP, VELDEF, IFQ, ISURNO, SUKOLS, SUNUMV, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('READ', 'SOUINI', 'SUTIDY', IERR)
         GO TO 990
         ENDIF
      NSOU = TABUF2(5)
C                                       Open SU table #1
      CALL SOUINI ('WRIT', TABUF1, DISK, ISLOT, IN, CATBLK, LUNIN,
     *   NUMBND, VELTYP, VELDEF, IFQ, ISORNO, SOKOLS, SONUMV, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('WRIT', 'SOUINI', 'SUTIDY', IERR)
         GO TO 990
         ENDIF
C                                       Loop & copy
      DO 100 I = 1, NSOU
         ISURNO = I
         CALL TABSOU ('READ', TABUF2, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, TQUAL, TCALC, FLUX, FREQO, BANDW, TRAEPO, TDCEPO,
     *      TEQUIN, TRAAPP, TDCAPP, TRAOBS, TDECOB, LSRVEL, LRESTF,
     *      TPMRA, TPMDEC, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'TABSOU', 'SUTIDY', IERR)
            GO TO 999
            END IF
C                                       Check have valid data
         IF (SOCOUN(IDSOU).EQ.0) GO TO 100
C                                       If so, write
         CALL TABSOU ('WRIT', TABUF1, ISORNO, SOKOLS, SONUMV, IDSOU,
     *      SOUNAM, TQUAL, TCALC, FLUX, FREQO, BANDW, TRAEPO, TDCEPO,
     *      TEQUIN, TRAAPP, TDCAPP, TRAOBS, TDECOB, LSRVEL, LRESTF,
     *      TPMRA, TPMDEC, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'TABSOU', 'SUTIDY', IERR)
            GO TO 999
            END IF
  100    CONTINUE
C                                       Close tables
      CALL TABIO ('CLOS', 1, ISURNO, TABUF2, TABUF2, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'SUTIDY', IERR)
         GO TO 999
         END IF
      CALL TABIO ('CLOS', 1, ISORNO, TABUF1, TABUF1, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'SUTIDY', IERR)
         GO TO 999
         END IF
C                                       Delete SU 2
      CALL RMEXT (DISK, ISLOT, 'SU', OUT, CATBLK, TABUF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
      GO TO 999
C
  990 CALL MSGWRT (8)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUTIDY: ERROR',I3,' COPYING SU #1 -> SU #2')
 1010 FORMAT ('SUTIDY: ERROR',I3,' DELETING SU #1')
      END
      SUBROUTINE DETORD (BANDFR, NUMBND, ORDER, LOWIF)
C-----------------------------------------------------------------------
C  Routine to determine the order of frequencies.
C  Input:
C     BANDFR     D(*)      IF freq offsets
C     NUMBND     I         # IF'S
C  Output:
C     ORDER      I(*)      Order of frequencies
C     LOWIF      I         Input IF# with lowest frequency
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION BANDFR(MAXIF), T1
      REAL    HIGHF, LOWF
      INTEGER NUMBND, ORDER(MAXIF), LOWIF, TORDER(MAXIF), I, J, K, II
C-----------------------------------------------------------------------
C                                       Sort them into increasing order,
C                                       ensuring deal with offsets that
C                                       are the same.
      LOWIF = 1
      HIGHF = 1.0E15
      LOWF  = -1.0E15
      DO 10 I = 1, NUMBND
         ORDER(I) = I
         TORDER(I) = I
 10      CONTINUE
C
      K = NUMBND - 1
      DO 30 II = 1, NUMBND
         DO 20 I = 1, K
            IF (BANDFR(I+1).LT.BANDFR(I)) THEN
               J = TORDER(I)
               TORDER(I) = TORDER(I+1)
               TORDER(I+1) = J
               T1 = BANDFR(I)
               BANDFR(I) = BANDFR(I+1)
               BANDFR(I+1) = T1
               END IF
 20         CONTINUE
 30      CONTINUE
C                                       Swap order to reflect that
C                                       needed by switching routines
      LOWIF = TORDER(1)
      DO 40 I = 1,NUMBND
         J = TORDER(I)
         ORDER(J) = I
 40      CONTINUE
C
      RETURN
      END
      SUBROUTINE TABREF (ITYPE, NEWVER, ATTVER, DISK, CNO, CATBLK, LUN,
     *   ROWF, ODISK, OCNO, CATO, IERR)
C-----------------------------------------------------------------------
C   Routine to reformat tables when reading them in from VLBA
C   distribution tapes. If reformatting is necessary then the incoming
C   table will have been written to NEWVER but the reformmated table
C   needs to be attached to ATTVER. This routine will take care of
C   all the necessary bookkeeping.
C   Inputs:
C      ITYPE    C*2      Table type
C      NEWVER   I        Version number of old format table
C      ATTVER   I        Version number of output table, new format
C                        table may already exist and so reformatted
C                        table will be attached to the end of it.
C      DISK     I        Disk volume
C      CNO      I        Catalogue number
C      CATBLK   I(256)   Catalogue header
C      LUN      I        LUN to use
C      ODISK    I        Disk volume for output table
C      OCNO     I        Catalogue number for output table
C      CATO     I(256)   Catalogue header for output table
C   Outputs:
C      ROWF     I        Start row number in output table of
C                        reformatted data.
C      IERR     I        Error code, 0 => OK
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:POBV.INC'
      CHARACTER ITYPE*2
      DOUBLE PRECISION DJUL, TIMER(2)
      INTEGER   NEWVER, ATTVER, IERR, DISK, CNO, CATBLK(256), ROWF, LUN,
     *   OUTVER, ODISK, OCNO, CATO(256), OLUN, INKEY, INREC, INCOL,
     *   INROW, I, IR, OR, IOBRNO, OBKOLS(MAXOBC), OBNUMV(MAXOBC),
     *   ICTRNO, CTKOLS(12), CTNUMV(12)
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DIMV.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'SCRTCH.INC'
      INCLUDE 'DATSEL.INC'
C-----------------------------------------------------------------------
      OLUN = 48
C                                       Reformat by types
      OUTVER = 1
C                                       IM: Interferometer model
      IF (ITYPE.EQ.'IM') THEN
         IF (TABREV.EQ.1) CALL IMREF1 (DISK, CNO, NEWVER, OUTVER,
     *      CATBLK, LUN, ROWF, IERR)
C                                       OB: Spacecraft orbit
      ELSE IF (ITYPE.EQ.'OB') THEN
C                                       Reference Julian day number
         IF (CURJLD.EQ.0.0D0) THEN
            CALL JULDAY (REFDAT, DJUL)
         ELSE
            DJUL = CURJLD
            END IF
C
         IF (TABREV.EQ.1) CALL OBREFM (DISK, CNO, NEWVER, OUTVER,
     *      CATBLK, LUN, OLUN, ROWF, DJUL, IERR)
C                                       CT Calc table - add time range
      ELSE IF (ITYPE.EQ.'CT') THEN
         TIMER(1) = 0.0D0
         TIMER(2) = 0.0D0
         CALL CTREFM (DISK, CNO, NEWVER, OUTVER, CATBLK, LUN, OLUN,
     *      ROWF, TIMER, IERR)
         END IF
C                                       Copy reformatted table to output
C                                       disk
C                                       Open input
      NKEY = 0
      CALL TABINI ('READ', ITYPE, DISK, CNO, OUTVER, CATBLK, LUN,
     *   INKEY, INREC, INCOL, TDATP1, TABUF1, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('READ', 'TABINI', 'TABREF', IERR)
         GO TO 999
         END IF
      INROW = TABUF1(5)
      IR = 0
C                                       Open output table by type:
C                                       Open IM table for WRIT
      IF (ITYPE.EQ.'IM') THEN
         CALL IMINI ('WRIT', TABUF2, ODISK, OCNO, ATTVER, CATO, OLUN,
     *      IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'IMINI', 'TABREF', IERR)
            GO TO 999
            END IF
         ROWF = IIMRNO
         OR = IIMRNO
C                                       Open OB table for WRIT
      ELSE IF (ITYPE.EQ.'OB') THEN
         CALL OBINI ('WRIT', TABUF2, ODISK, OCNO, ATTVER, CATO, OLUN,
     *      IOBRNO, OBKOLS, OBNUMV, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'OBINI', 'TABREF', IERR)
            GO TO 999
            END IF
         ROWF = IOBRNO
         OR = IOBRNO
C                                       open CT table for WRIT
      ELSE IF (ITYPE.EQ.'CT') THEN
         CALL CTINI ('WRIT', TABUF2, ODISK, OCNO, ATTVER, CATO, OLUN,
     *      ICTRNO, CTKOLS, CTNUMV, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'CTINI', 'TABREF', IERR)
            GO TO 999
            END IF
         ROWF = ICTRNO
         OR = ICTRNO
         END IF
C                                       Copy input recs to output
      DO 20 I = 1,INROW
         IR = I
         CALL TABIO ('READ', 0, IR, RECORD, TABUF1, IERR)
         IF (IERR.GT.0) THEN
            CALL TABERR ('READ', 'TABIO', 'TABREF', IERR)
            GO TO 999
            END IF
C
         CALL TABIO ('WRIT', 0, OR, RECORD, TABUF2, IERR)
         OR = OR + 1
         IF (IERR.GT.0) THEN
            CALL TABERR ('WRIT', 'TABIO', 'TABREF', IERR)
            GO TO 999
            END IF
   20    CONTINUE
C                                       Close 'em down
      CALL TABIO ('CLOS', 0, IR, TABUF1, TABUF1, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'TABREF', IERR)
         GO TO 999
         END IF
      CALL TABIO ('CLOS', 0, OR, TABUF2, TABUF2, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'TABREF', IERR)
         GO TO 999
         END IF
C                                       Delete the tables attached to
C                                       the scratch file
      CALL RMEXT (DISK, CNO, ITYPE, OUTVER, CATBLK, TABUF2, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('DELE', 'RMEXT', 'TABREF', IERR)
         GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE MCKEY (NUMKEY, KEYWRD, KEYV, KEYLOC)
C-----------------------------------------------------------------------
C   This routine will extract the keywords needed for the digital
C   correction routine from the MC table.
C   Inputs:
C      NUMKEY   I          Number of keywords in table
C      KEYWRD   C(*)*8     Keywords
C      KEYV     I(*)       Array of keyword values
C      KEYLOC   I(NUMKEY)  Location of keyword value in KEYV array
C   Outputs in common /DIGI/:
C      FFTSIZ   I          # channels in correlator FFT
C      OVRSMP   I          Over sampling factor, 0 => no oversampling
C      ZEROPD   I          Zero padding factor(?), 0 => no padding
C      TAPER    C*8        Taper function used in FX correlator
C      TWIDDL   C*8        FFT_TWIDDLE version #
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'DIGCOR.INC'
      INTEGER   NUMKEY, KEYLOC(*), KEYV(*)
      CHARACTER KEYWRD(*)*8
C
      INTEGER    NKEYMC
      PARAMETER  (NKEYMC = 5)
      INTEGER   I, K, KPOS(NKEYMC), NOK
      CHARACTER KEYMC(NKEYMC)*8
C
      DATA KEYMC /'FFT_SIZE', 'OVERSAMP', 'ZERO_PAD', 'TAPER_FN',
     *   'FFT_TWID' /
C-----------------------------------------------------------------------
      NOK = NKEYMC
      CALL FILL (NOK, 0, KPOS)
      TAPER = ' '
      FFTSIZ = 0
      OVRSMP = 0
      ZEROPD = 0
      TWDVER = 0
C
      DO 20 I = 1,NUMKEY
         DO 10 K = 1,NOK
            IF (KEYMC(K).EQ.KEYWRD(I)) KPOS(K) = I
 10         CONTINUE
 20      CONTINUE
C
      IF (KPOS(1).GT.0) FFTSIZ = KEYV(KEYLOC(KPOS(1)))
      IF (KPOS(2).GT.0) OVRSMP = KEYV(KEYLOC(KPOS(2)))
      IF (KPOS(3).GT.0) ZEROPD = KEYV(KEYLOC(KPOS(3)))
      IF (KPOS(4).GT.0)
     *   CALL H2CHR (8, 1, KEYV(KEYLOC(KPOS(4))), TAPER)
      IF (KPOS(5).GT.0) TWDVER = KEYV(KEYLOC(KPOS(5)))
C
 999  RETURN
      END
      SUBROUTINE DIGLEV (DISK, CNO, LUN, NUMBND, CATIN, IERR)
C-----------------------------------------------------------------------
C  Routine which reads the AT table and extracts the # levels of
C  digitization for each antenna.
C   Inputs:
C     DISK        I          Volume on which data reside
C     CNO         I          Catalogue number of data
C     LUN         I          Main lun to use.
C     CATIN       I(256)     Catalogue header.
C   Outputs:
C     IERR        I          Error code, 0 => OK
C                            anything else => problem
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER    DISK, CNO, LUN, CATIN(256), IERR
C
      INTEGER   IVER, ANTFNC, NENT, IENT, NUMBND
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'DIGCOR.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'INCS:DATV.INC'
C-----------------------------------------------------------------------
      CALL COPY (256, CATIN, CATIEQ)
C                                       Open the AT table
      IVER = 1
      CALL ATINI ('READ', TABUF1, DISK, CNO, IVER, CATIEQ, LUN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      NENT = TABUF1(5)
C                                       Loop over antennas
      DO 50 IENT = 1,NENT
C                                       Read AT entry
         CALL ATTAB ('READ', TABUF1, NUMBND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
         IF (ANTCHA) NOSTA = ANTFNC (NOSTA, IVER)
C                                       # levels
         NLEVS(NOSTA) = NOLEVL
   50    CONTINUE
C                                       Close down AT table
      CALL TABIO ('CLOS', 1, IATRNO, TABUF1, TABUF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DIGLEV: ERROR ',I3,' OPENING AT TABLE')
 1010 FORMAT ('DIGLEV: ERROR ',I3,' WRITING AT TABLE')
 1020 FORMAT ('DIGLEV: ERROR ',I3,' CLOSING AT TABLE')
      END
      SUBROUTINE ARTFCT (LFRQ, CURFQI)
C-----------------------------------------------------------------------
C Routine to set up the correction the total power spectra from the
C VLBA correlator for the effects of the FFT artifacts introduced by
C the hardware FFTs (most of the artifacts are introduced in the
C radix-2 stage).
C  Inputs:
C     LFRQ      I     # freq channels in data
C     CURFQI    I     The current FQID of the data
C  Inputs in common:
C     DOUVCM    L     If true output data are compressed, value will
C                     affect data increments.
C     FFTSIZ    I     Number of points in the input of FFT
C     CURA1     I     The first antenna number
C     CURA2     I     The second antenna number
C  Outputs:
C     IERR      I          Error code.
C-----------------------------------------------------------------------
      INCLUDE 'PFITLD.INC'
      CHARACTER HILINE*72
      INTEGER   NF, JF, NAVG, KF, LF, I, LFRQ, CURFQI, NEWIF, HERR
      REAL      FFTEVN(LMXCHA), FFTODD(LMXCHA), SUM0, SUM1,
     *   OD2048(1024), OD1024(512),
     *   ODD512(256), EVN512(256), ODD256(128), EVN256(128),
     *   A2048(100), B2048(100), C2048(100), D2048(100), E2048(100),
     *   F2048(100), G2048(100), H2048(100), I2048(100), J2048(100),
     *   K2048(24),
     *   A1024(100), B1024(100), C1024(100), D1024(100), E1024(100),
     *   F1024(12),
     *   EA512(100), EB512(100), EC512(56),
     *   OA512(100), OB512(100), OC512(56),
     *   OA256(100), OB256(28), EA256(100), EB256(28),
     *   OA128(64), EA128(64), OA64(32), EA64(32)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FITLD.INC'
      INCLUDE 'DIGCOR.INC'
      INCLUDE 'ORDER.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (A2048, OD2048(1)),   (B2048, OD2048(101)),
     *            (C2048, OD2048(201)), (D2048, OD2048(301)),
     *            (E2048, OD2048(401)), (F2048, OD2048(501)),
     *            (G2048, OD2048(601)), (H2048, OD2048(701)),
     *            (I2048, OD2048(801)), (J2048, OD2048(901)),
     *            (K2048, OD2048(1001))
      EQUIVALENCE (A1024, OD1024(1)),   (B1024, OD1024(101)),
     *            (C1024, OD1024(201)), (D1024, OD1024(301)),
     *            (E1024, OD1024(401)), (F1024, OD1024(501))
      EQUIVALENCE (EA512, EVN512(1)),   (EB512, EVN512(101)),
     *            (EC512, EVN512(201))
      EQUIVALENCE (OA512, ODD512(1)),   (OB512, ODD512(101)),
     *            (OC512, ODD512(201))
      EQUIVALENCE (EA256, EVN256(1)),   (EB256, EVN256(101))
      EQUIVALENCE (OA256, ODD256(1)),   (OB256, ODD256(101))
      DATA A2048  / 1.07080,  0.962947, 0.930401, 0.943632, 0.942100,
     *              0.939961, 0.961237, 0.988500, 0.945708, 0.965711,
     *              1.00128,  0.964633, 0.990339, 1.01133,  0.994609,
     *              1.01033,  0.970047, 1.04389,  1.00401,  1.02134,
     *              0.994753, 0.980983, 0.985958, 1.01188,  0.993974,
     *              1.04679,  1.04539,  1.01766,  1.04013,  1.00524,
     *              0.992785, 0.969381, 0.986973, 0.983132, 1.03303,
     *              1.01124,  1.01706,  1.01090,  1.04190,  1.02836,
     *              1.02786,  1.02008,  1.00305,  0.975713, 0.995124,
     *              1.02323,  1.01117,  1.05384,  0.985001, 1.02930,
     *              1.01318,  1.04484,  1.02986,  1.01172,  1.06037,
     *              0.997350, 0.993229, 1.06878,  1.04296,  1.00374,
     *              0.999489, 0.963133, 0.948123, 0.928472, 0.974547,
     *              0.933355, 0.968713, 0.973471, 1.01601,  0.987332,
     *              1.00827,  1.03489,  0.986523, 1.01002,  1.03987,
     *              0.986195, 1.01611,  1.02990,  1.00799,  1.00499,
     *              0.984064, 1.04863,  1.01618,  0.994898, 1.00517,
     *              1.00088,  0.997691, 0.993504, 0.979663, 1.00567,
     *              1.00767,  0.973399, 1.01014,  1.00129,  1.01843,
     *              0.966434, 0.980974, 0.980776, 1.00909,  1.02853 /
      DATA B2048  / 1.04425,  1.02765,  1.05635,  1.05225,  1.01289,
     *              1.02628,  0.980119, 0.983557, 1.00286,  1.00184,
     *              1.00142,  1.03763,  0.976359, 1.02423,  0.979954,
     *              1.00142,  1.01094,  0.996907, 1.03450,  1.01495,
     *              1.01400,  1.04068,  0.984814, 0.958609, 0.967260,
     *              0.982701, 0.942331, 0.959324, 1.00400,  0.959544,
     *              0.942235, 0.987177, 0.970158, 0.961400, 0.986660,
     *              1.04195,  1.00484,  1.01801,  1.02336,  0.983984,
     *              1.00910,  1.00689,  0.990115, 1.02525,  0.984162,
     *              1.04234,  1.00208,  1.00658,  1.01859,  0.977624,
     *              0.982793, 1.03478,  1.02724,  1.05332,  1.06091,
     *              1.02265,  1.03562,  1.02774,  1.00856,  0.976858,
     *              0.977830, 0.971294, 1.02166,  1.00217,  1.00974,
     *              0.982165, 1.01369,  1.00884,  0.974651, 0.986198,
     *              0.995242, 1.00335,  1.00682,  0.998266, 1.01173,
     *              1.04143,  0.981996, 1.01366,  1.00379,  1.02457,
     *              1.01252,  0.986982, 1.04683,  0.999962, 0.992885,
     *              1.02958,  1.01048,  0.983169, 1.02036,  0.973436,
     *              0.964743, 0.931894, 0.981493, 0.935050, 0.948440,
     *              0.963905, 1.00062,  1.00207,  1.04020,  1.07248 /
      DATA C2048  / 0.992234, 1.00110,  1.05416,  1.00409,  1.03526,
     *              1.04559,  1.02360,  1.03433,  0.992946, 1.05039,
     *              1.02067,  1.02776,  1.00029,  0.973449, 0.999405,
     *              1.01199,  1.02444,  1.03570,  1.04536,  1.00619,
     *              1.00934,  1.00045,  1.02255,  0.973430, 0.975936,
     *              0.965334, 0.978646, 0.994459, 1.03833,  1.01729,
     *              1.03264,  1.04104,  0.992386, 1.00133,  0.975310,
     *              0.972934, 0.988129, 1.01477,  0.991247, 1.03330,
     *              0.959259, 1.00695,  0.982512, 0.997384, 0.988800,
     *              0.969972, 1.00543,  0.969265, 0.950669, 0.985609,
     *              0.960129, 0.939968, 0.945159, 0.955301, 0.930101,
     *              0.964051, 1.01333,  0.967378, 0.932369, 0.947622,
     *              0.943455, 0.938756, 0.958211, 0.984556, 0.947042,
     *              0.971298, 1.00347,  0.967982, 0.988210, 1.00784,
     *              0.989497, 1.00634,  0.962551, 1.03445,  0.991239,
     *              1.00875,  0.985781, 0.973823, 0.977649, 1.00594,
     *              0.988553, 1.03858,  1.04348,  1.01674,  1.03825,
     *              1.00185,  0.992697, 0.975065, 0.981443, 0.973147,
     *              1.02442,  1.00364,  1.00783,  1.00038,  1.03729,
     *              1.02901,  1.02580,  1.02018,  1.00282,  0.971486 /
      DATA D2048  / 0.990927, 1.02329,  1.00560,  1.04900,  0.985545,
     *              1.03169,  1.01347,  1.04895,  1.02894,  1.00695,
     *              1.05701,  0.997016, 0.998551, 1.06747,  1.03755,
     *              1.00087,  0.997284, 0.962086, 0.946021, 0.930218,
     *              0.974276, 0.933295, 0.966512, 0.974310, 1.02464,
     *              0.990382, 1.00615,  1.03422,  0.988175, 1.00029,
     *              1.03540,  0.983269, 1.01844,  1.02985,  1.01182,
     *              1.01220,  0.988452, 1.05010,  1.01639,  0.997533,
     *              1.00269,  1.00050,  0.999607, 0.992573, 0.980990,
     *              1.01205,  1.01335,  0.974157, 1.00949,  1.00117,
     *              1.01551,  0.967240, 0.977417, 0.975008, 1.01089,
     *              1.02736,  1.04342,  1.02615,  1.05525,  1.05399,
     *              1.01564,  1.02692,  0.975106, 0.981844, 1.00498,
     *              0.999399, 1.00238,  1.04021,  0.975292, 1.02313,
     *              0.980093, 0.995941, 1.00994,  0.992992, 1.03534,
     *              1.01343,  1.00933,  1.03315,  0.979967, 0.958650,
     *              0.970400, 0.979408, 0.939717, 0.952326, 1.00335,
     *              0.961708, 0.944531, 0.987304, 0.968407, 0.957927,
     *              0.980009, 1.03767,  1.00287,  1.01697,  1.02864,
     *              0.988062, 1.00974,  1.00266,  0.991563, 1.02744 /
      DATA E2048  / 0.982599, 1.03904,  1.00093,  1.00500,  1.01559,
     *              0.976199, 0.976444, 1.02858,  1.03023,  1.05404,
     *              1.05748,  1.02389,  1.04007,  1.02897,  1.01289,
     *              0.979608, 0.980632, 0.971234, 1.01945,  0.998390,
     *              1.00490,  0.980534, 1.01162,  1.00653,  0.979249,
     *              0.991620, 0.997628, 1.00674,  1.00438,  0.991005,
     *              1.00691,  1.04560,  0.983890, 1.00967,  1.00294,
     *              1.02608,  1.01351,  0.983431, 1.04644,  0.996961,
     *              0.989151, 1.02927,  1.01470,  0.984069, 1.02135,
     *              0.972605, 0.966039, 0.924649, 0.978532, 0.935350,
     *              0.948570, 0.967014, 1.00071,  0.997919, 1.03646,
     *              1.07126,  0.993062, 0.997798, 1.05586,  1.00717,
     *              1.03751,  1.04496,  1.01630,  1.02997,  0.991784,
     *              1.04854,  1.02371,  1.02978,  1.00238,  0.974632,
     *              1.00104,  1.01681,  1.02153,  1.03256,  1.03820,
     *              0.998722, 1.00146,  0.997026, 1.02385,  0.973399,
     *              0.973701, 0.969422, 0.982129, 0.997542, 1.03487,
     *              1.01843,  1.03334,  1.03832,  0.985116, 0.997132,
     *              0.968998, 0.966919, 0.981395, 1.00932,  0.990055,
     *              1.03391,  0.956736, 1.00843,  0.981018, 0.995396 /
      DATA F2048  / 0.987862, 0.967289, 1.00531,  0.969877, 0.952957,
     *              0.987471, 0.964447, 0.942051, 0.947593, 0.953325,
     *              0.931228, 0.961736, 1.01633,  0.968843, 0.932025,
     *              0.946293, 0.942672, 0.936374, 0.958655, 0.986735,
     *              0.944791, 0.968253, 1.00746,  0.969608, 0.992622,
     *              1.00939,  0.994039, 1.01002,  0.970120, 1.04300,
     *              1.00739,  1.02053,  0.999149, 0.986430, 0.987706,
     *              1.01374,  0.996988, 1.04355,  1.04466,  1.01992,
     *              1.03961,  1.00272,  0.992881, 0.967220, 0.981621,
     *              0.979384, 1.02915,  1.01026,  1.01640,  1.00591,
     *              1.04035,  1.03077,  1.02585,  1.01776,  1.00214,
     *              0.977833, 0.995312, 1.01920,  1.01057,  1.05091,
     *              0.980815, 1.02907,  1.01317,  1.04344,  1.02895,
     *              1.00887,  1.05984,  0.999086, 0.994368, 1.07017,
     *              1.04405,  1.00425,  1.00112,  0.967015, 0.944850,
     *              0.927906, 0.975612, 0.933490, 0.967072, 0.976057,
     *              1.01974,  0.986682, 1.00751,  1.03918,  0.987740,
     *              1.00624,  1.03809,  0.987495, 1.01758,  1.03082,
     *              1.01275,  1.00585,  0.983855, 1.05220,  1.01950,
     *              0.999688, 1.00579,  1.00055,  1.00130,  1.00037 /
      DATA G2048  / 0.988627, 1.01070,  1.01544,  0.975367, 1.01104,
     *              1.00164,  1.01901,  0.967290, 0.983420, 0.977662,
     *              1.00595,  1.02637,  1.04300,  1.02706,  1.05316,
     *              1.05169,  1.01694,  1.02468,  0.975799, 0.983355,
     *              1.00657,  1.00482,  1.00390,  1.03918,  0.975087,
     *              1.02372,  0.982237, 1.00057,  1.00872,  0.995764,
     *              1.03664,  1.01510,  1.00725,  1.03360,  0.984733,
     *              0.963434, 0.973538, 0.982906, 0.940853, 0.958783,
     *              1.00489,  0.963741, 0.941306, 0.987425, 0.970533,
     *              0.960215, 0.983995, 1.03615,  1.00573,  1.02348,
     *              1.02906,  0.985166, 1.00998,  1.00621,  0.988481,
     *              1.02392,  0.980948, 1.03968,  1.00287,  1.00685,
     *              1.01378,  0.978083, 0.982839, 1.03352,  1.02976,
     *              1.05604,  1.06262,  1.03127,  1.04521,  1.03458,
     *              1.01344,  0.980644, 0.981396, 0.968559, 1.01990,
     *              1.00060,  1.00265,  0.979175, 1.01013,  1.00695,
     *              0.977369, 0.989665, 0.999064, 1.00536,  1.00443,
     *              0.998746, 1.01103,  1.04393,  0.983133, 1.01130,
     *              1.00059,  1.02395,  1.00900,  0.986880, 1.04924,
     *              1.00115,  0.989962, 1.03396,  1.01815,  0.986367 /
      DATA H2048  / 1.02310,  0.976647, 0.961114, 0.927215, 0.981742,
     *              0.937409, 0.948576, 0.962754, 0.999380, 1.00107,
     *              1.03847,  1.07070,  0.989675, 1.00265,  1.05627,
     *              1.00589,  1.03798,  1.05468,  1.02492,  1.02738,
     *              0.992000, 1.05102,  1.02208,  1.02618,  0.999209,
     *              0.973320, 0.998105, 1.01058,  1.02042,  1.03240,
     *              1.03876,  0.998702, 1.00589,  1.00005,  1.02417,
     *              0.971463, 0.976270, 0.970925, 0.984028, 0.996728,
     *              1.04074,  1.02094,  1.03130,  1.03541,  0.988538,
     *              0.995389, 0.970378, 0.966575, 0.983016, 1.01121,
     *              0.987397, 1.03392,  0.958016, 1.00461,  0.982877,
     *              0.998421, 0.988511, 0.969928, 1.00606,  0.970552,
     *              0.954074, 0.985541, 0.960233, 0.943901, 0.948420,
     *              0.955936, 0.931661, 0.962287, 1.01525,  0.970701,
     *              0.932673, 0.949873, 0.941473, 0.936981, 0.959291,
     *              0.985890, 0.944059, 0.965330, 1.00328,  0.967973,
     *              0.989092, 1.00570,  0.985482, 0.998463, 0.955989,
     *              1.03434,  0.993194, 1.00618,  0.980359, 0.967738,
     *              0.978399, 1.00651,  0.987698, 1.03392,  1.03940,
     *              1.01917,  1.03878,  0.999955, 0.989376, 0.966518 /
      DATA I2048  / 0.979688, 0.974571, 1.02535,  1.00371,  1.01431,
     *              1.00297,  1.03556,  1.02639,  1.02351,  1.01443,
     *              1.00092,  0.976525, 0.990236, 1.01792,  1.00257,
     *              1.04721,  0.982757, 1.02875,  1.01204,  1.04607,
     *              1.02604,  1.00743,  1.06072,  0.999736, 0.995685,
     *              1.06576,  1.04378,  1.00391,  0.998987, 0.965688,
     *              0.947499, 0.932009, 0.976977, 0.935860, 0.968309,
     *              0.975590, 1.02366,  0.986775, 1.00766,  1.03497,
     *              0.987804, 1.00353,  1.03823,  0.983132, 1.01787,
     *              1.02904,  1.01295,  1.01007,  0.983504, 1.05151,
     *              1.01475,  0.989216, 0.999661, 1.00029,  0.994185,
     *              0.990359, 0.981903, 1.01177,  1.01315,  0.973830,
     *              1.00105,  0.997342, 1.01892,  0.966391, 0.977796,
     *              0.973901, 1.00601,  1.02574,  1.04301,  1.02721,
     *              1.05835,  1.05420,  1.01456,  1.02183,  0.973411,
     *              0.984249, 1.00430,  1.00324,  1.00316,  1.03849,
     *              0.974240, 1.01789,  0.975366, 1.00104,  1.00993,
     *              0.989957, 1.03489,  1.01404,  1.00747,  1.03562,
     *              0.979501, 0.959950, 0.972160, 0.981545, 0.942207,
     *              0.955014, 1.00557,  0.964621, 0.941218, 0.984152 /
      DATA J2048  / 0.966946, 0.956035, 0.980627, 1.04223,  1.00417,
     *              1.01823,  1.03037,  0.986168, 1.01006,  1.00331,
     *              0.990461, 1.02474,  0.980958, 1.04051,  1.00465,
     *              1.00770,  1.01402,  0.971455, 0.979554, 1.03283,
     *              1.02595,  1.05544,  1.05764,  1.02066,  1.03711,
     *              1.02516,  1.01333,  0.982014, 0.979490, 0.969627,
     *              1.01441,  0.997300, 1.00289,  0.975991, 1.00865,
     *              1.00463,  0.976410, 0.991448, 0.998195, 1.01195,
     *              1.00744,  0.997099, 1.01415,  1.04286,  0.983285,
     *              1.01333,  1.00299,  1.02170,  1.01053,  0.982062,
     *              1.04463,  0.999017, 0.990087, 1.03275,  1.01782,
     *              0.987350, 1.02354,  0.969539, 0.959993, 0.922295,
     *              0.976779, 0.932487, 0.947408, 0.965003, 0.997527,
     *              0.999283, 1.03915,  1.06709,  0.989488, 1.00021,
     *              1.05536,  1.00429,  1.03683,  1.04553,  1.01847,
     *              1.02968,  0.989169, 1.04727,  1.01727,  1.02641,
     *              1.00417,  0.981512, 1.00737,  1.01638,  1.02029,
     *              1.03264,  1.04126,  1.00289,  1.00553,  0.999905,
     *              1.02698,  0.975182, 0.981588, 0.968369, 0.980051,
     *              1.00513,  1.03809,  1.02184,  1.03486,  1.03897 /
      DATA K2048  / 0.991587, 0.999021, 0.975751, 0.968250, 0.981846,
     *              1.00924,  0.991136, 1.03259,  0.955585, 1.00535,
     *              0.982780, 0.998444, 0.988477, 0.967597, 1.00692,
     *              0.968017, 0.952058, 0.986302, 0.960422, 0.940265,
     *              0.949860, 0.950342, 0.928771, 0.959427 /
      DATA A1024  / 1.06055,  0.958055, 0.931118, 0.946827, 0.942854,
     *              0.939295, 0.966098, 0.992452, 0.963787, 0.984798,
     *              1.02442,  0.986043, 0.994289, 1.01763,  0.992101,
     *              1.00433,  0.964121, 1.03650,  0.999468, 1.01676,
     *              0.981234, 0.968743, 0.979454, 1.00821,  0.990329,
     *              1.03591,  1.03627,  1.01512,  1.04078,  1.00265,
     *              0.989878, 0.969707, 0.981390, 0.976289, 1.02301,
     *              1.00201,  1.01320,  1.00449,  1.03833,  1.02908,
     *              1.02686,  1.01519,  0.998126, 0.973679, 0.992629,
     *              1.01887,  1.00683,  1.04912,  0.984669, 1.02962,
     *              1.01015,  1.04526,  1.02494,  1.00396,  1.05333,
     *              0.994967, 0.997549, 1.06608,  1.03858,  1.00306,
     *              1.00061,  0.967098, 0.945175, 0.928486, 0.976126,
     *              0.936163, 0.970532, 0.978252, 1.02129,  0.987895,
     *              1.00645,  1.03630,  0.987574, 1.00479,  1.03529,
     *              0.984336, 1.01499,  1.02697,  1.01250,  1.00821,
     *              0.985393, 1.04567,  1.01327,  0.997070, 1.00370,
     *              0.999628, 1.00019,  0.993948, 0.982432, 1.01133,
     *              1.01244,  0.976025, 1.01013,  1.00346,  1.01796,
     *              0.969047, 0.982295, 0.977103, 1.01071,  1.03094 /
      DATA B1024  / 1.04072,  1.02677,  1.05637,  1.05240,  1.02044,
     *              1.03056,  0.979192, 0.983998, 1.00391,  1.00416,
     *              0.999661, 1.03722,  0.979827, 1.02575,  0.978958,
     *              0.997460, 1.00922,  0.996247, 1.03379,  1.01325,
     *              1.01041,  1.03714,  0.982613, 0.961711, 0.969400,
     *              0.980073, 0.938212, 0.956994, 1.00115,  0.957640,
     *              0.940492, 0.985399, 0.971894, 0.959532, 0.980329,
     *              1.03824,  1.00493,  1.02146,  1.02956,  0.986947,
     *              1.01166,  1.00426,  0.989587, 1.02518,  0.983510,
     *              1.04004,  1.00223,  1.00678,  1.01443,  0.975471,
     *              0.981749, 1.02898,  1.02296,  1.05134,  1.05678,
     *              1.02566,  1.03902,  1.02970,  1.01551,  0.979542,
     *              0.978422, 0.971187, 1.02039,  1.00092,  1.00165,
     *              0.975640, 1.01059,  1.00515,  0.973288, 0.986870,
     *              0.997058, 1.00706,  1.00585,  0.994629, 1.01022,
     *              1.04312,  0.981188, 1.01107,  1.00143,  1.02471,
     *              1.00916,  0.980087, 1.04368,  0.997457, 0.990230,
     *              1.03255,  1.01705,  0.985170, 1.02007,  0.974753,
     *              0.964110, 0.926635, 0.980826, 0.938408, 0.946952,
     *              0.962050, 0.995692, 1.00232,  1.04150,  1.07024 /
      DATA C1024  / 0.990798, 1.00102,  1.05306,  1.00443,  1.04093,
     *              1.04750,  1.02477,  1.03231,  0.992009, 1.04911,
     *              1.01977,  1.02491,  1.00032,  0.975165, 1.00553,
     *              1.01760,  1.02304,  1.03204,  1.03725,  1.00174,
     *              1.00672,  1.00107,  1.02349,  0.974292, 0.979284,
     *              0.968879, 0.981197, 0.997140, 1.03704,  1.01723,
     *              1.03392,  1.03947,  0.993364, 1.00340,  0.976896,
     *              0.970383, 0.982650, 1.01231,  0.988969, 1.03039,
     *              0.957863, 1.00514,  0.980653, 0.997304, 0.990507,
     *              0.971109, 1.00719,  0.972704, 0.956630, 0.989395,
     *              0.960322, 0.940659, 0.947399, 0.953138, 0.930412,
     *              0.966068, 1.01724,  0.968275, 0.934719, 0.953030,
     *              0.947230, 0.942957, 0.960091, 0.987836, 0.949599,
     *              0.967135, 1.00309,  0.967891, 0.987310, 1.00845,
     *              0.987813, 1.00193,  0.958165, 1.03172,  0.995178,
     *              1.01506,  0.984119, 0.969004, 0.976157, 1.00463,
     *              0.989044, 1.03671,  1.04046,  1.01942,  1.03937,
     *              0.996655, 0.989671, 0.971623, 0.980167, 0.978499,
     *              1.02577,  1.00636,  1.01243,  0.999985, 1.03124,
     *              1.02666,  1.02536,  1.01566,  0.998724, 0.976609 /
      DATA D1024  / 0.992829, 1.02059,  1.00829,  1.04963,  0.987374,
     *              1.03126,  1.01497,  1.04541,  1.02446,  1.00759,
     *              1.05692,  0.999354, 0.999486, 1.06715,  1.03839,
     *              1.00127,  0.993928, 0.962619, 0.948072, 0.928412,
     *              0.972271, 0.930805, 0.966051, 0.974982, 1.01994,
     *              0.987741, 1.00748,  1.03576,  0.991253, 1.00644,
     *              1.03287,  0.982784, 1.01535,  1.02714,  1.01365,
     *              1.00678,  0.983178, 1.04619,  1.01470,  0.999075,
     *              1.00029,  0.993713, 0.993870, 0.992130, 0.982416,
     *              1.00923,  1.01034,  0.972939, 1.00164,  0.994126,
     *              1.01261,  0.963965, 0.981869, 0.978630, 1.01048,
     *              1.02980,  1.04294,  1.02993,  1.05943,  1.05430,
     *              1.01983,  1.02807,  0.978366, 0.983529, 1.00474,
     *              1.00725,  1.00471,  1.03783,  0.977816, 1.02484,
     *              0.978878, 0.998168, 1.00844,  0.990784, 1.03363,
     *              1.01651,  1.00900,  1.03463,  0.980044, 0.960043,
     *              0.973561, 0.984068, 0.942541, 0.957186, 1.00127,
     *              0.961703, 0.945104, 0.989276, 0.978383, 0.966575,
     *              0.979667, 1.03738,  1.00308,  1.01847,  1.02909,
     *              0.986069, 1.00926,  1.00557,  0.992167, 1.02687 /
      DATA E1024  / 0.982044, 1.04088,  1.00481,  1.00852,  1.01664,
     *              0.977801, 0.978878, 1.02916,  1.02822,  1.05184,
     *              1.05501,  1.02657,  1.03890,  1.02814,  1.01260,
     *              0.977737, 0.979735, 0.970259, 1.01925,  1.00012,
     *              1.00106,  0.978439, 1.00913,  1.00584,  0.977316,
     *              0.994551, 1.00180,  1.00493,  1.00338,  0.998008,
     *              1.01306,  1.04303,  0.981611, 1.01282,  1.00309,
     *              1.02113,  1.01003,  0.983521, 1.04436,  1.00402,
     *              0.991822, 1.03471,  1.02061,  0.988729, 1.02140,
     *              0.972165, 0.960204, 0.925229, 0.978199, 0.936092,
     *              0.949774, 0.966369, 0.995285, 0.999749, 1.03592,
     *              1.06928,  0.994684, 1.00498,  1.05598,  1.01141,
     *              1.03876,  1.04786,  1.02430,  1.03274,  0.993279,
     *              1.05246,  1.02306,  1.02881,  1.00218,  0.974762,
     *              1.00378,  1.01482,  1.02534,  1.03421,  1.04300,
     *              1.00386,  1.00564,  0.999227, 1.02592,  0.974409,
     *              0.976615, 0.969378, 0.979346, 0.996392, 1.04083,
     *              1.01952,  1.03399,  1.03746,  0.984427, 0.996632,
     *              0.970071, 0.965618, 0.983139, 1.01444,  0.990400,
     *              1.03092,  0.956403, 1.00088,  0.979156, 0.997836 /
      DATA F1024  / 0.988830, 0.968106, 1.00748,  0.970011, 0.953348,
     *              0.991361, 0.963602, 0.940647, 0.948578, 0.957157,
     *              0.934273, 0.965066 /
      DATA EA512  / 1.06226,  0.956698, 0.970837, 0.984319, 0.983105,
     *              0.979593, 0.999461, 1.02633,  0.982332, 1.00491,
     *              1.03483,  0.989947, 1.01034,  1.02554,  1.00411,
     *              1.01160,  0.967922, 1.03692,  1.00130,  1.00378,
     *              1.00921,  0.987060, 0.991638, 1.00213,  0.985590,
     *              1.02952,  1.03330,  0.998542, 1.01507,  1.00033,
     *              1.01464,  0.980860, 0.996214, 0.980348, 1.01468,
     *              0.997933, 1.00528,  1.00052,  1.03901,  1.02806,
     *              0.982865, 0.998564, 0.981865, 0.994426, 1.00510,
     *              1.00567,  0.997382, 1.03531,  0.970805, 1.01755,
     *              0.998338, 1.01856,  1.00669,  0.993651, 1.04181,
     *              1.00192,  0.991941, 1.02899,  0.995351, 0.979307,
     *              0.984137, 0.982103, 0.963291, 0.958416, 1.00699,
     *              0.958217, 0.960870, 0.980416, 0.981566, 0.980689,
     *              1.00272,  1.02978,  0.985221, 1.00609,  1.03620,
     *              0.988909, 1.00594,  1.02390,  1.00566,  1.00928,
     *              0.964117, 1.03401,  1.00099,  1.00531,  1.00945,
     *              0.987783, 0.994845, 1.00239,  0.986309, 1.03104,
     *              1.03030,  0.996892, 1.00685,  0.995563, 1.01633,
     *              0.983207, 0.993525, 0.986556, 1.01517,  1.00182 /
      DATA EB512  / 1.00787,  1.00457,  1.03673,  1.02854,  0.979866,
     *              0.999292, 0.983400, 0.992562, 1.00452,  1.00716,
     *              0.999242, 1.03438,  0.965355, 1.01463,  0.999827,
     *              1.02087,  1.00478,  0.989774, 1.03845,  0.996210,
     *              0.983828, 1.02867,  0.992805, 0.977911, 0.981935,
     *              0.977754, 0.959027, 0.951013, 1.00881,  0.958063,
     *              0.961498, 0.980562, 0.980535, 0.973400, 1.00314,
     *              1.02829,  0.979155, 1.00570,  1.03893,  0.991976,
     *              1.00785,  1.02321,  1.00367,  1.01232,  0.967736,
     *              1.03739,  0.999181, 1.00316,  1.00717,  0.984888,
     *              0.985929, 1.00210,  0.983934, 1.03205,  1.03444,
     *              0.996719, 1.00897,  1.00092,  1.01576,  0.980960,
     *              0.992851, 0.972797, 1.01042,  1.00109,  1.00540,
     *              0.999830, 1.03592,  1.03179,  0.988049, 0.998575,
     *              0.988267, 0.995607, 1.00770,  1.00476,  0.999745,
     *              1.03510,  0.971851, 1.01861,  0.996435, 1.02402,
     *              1.00812,  0.991440, 1.03915,  0.998169, 0.983502,
     *              1.02843,  0.999372, 0.981820, 0.985780, 0.980549,
     *              0.964148, 0.953685, 1.00531,  0.958420, 0.961193,
     *              0.980199, 0.980447, 0.976685, 0.999370, 1.02825 /
      DATA EC512  / 0.979205, 1.00400,  1.03476,  0.989282, 1.00545,
     *              1.02369,  1.00649,  1.01227,  0.968790, 1.03757,
     *              1.00003,  0.999697, 1.00789,  0.987582, 0.988527,
     *              1.00179,  0.985935, 1.03191,  1.03272,  0.998346,
     *              1.00809,  1.00064,  1.01549,  0.980759, 0.994628,
     *              0.980061, 1.01480,  1.00095,  1.00772,  1.00080,
     *              1.03766,  1.03223,  0.984800, 0.998041, 0.982673,
     *              0.990703, 1.00349,  1.00342,  0.996258, 1.03275,
     *              0.970393, 1.01928,  1.00021,  1.02560,  1.00429,
     *              0.990615, 1.03727,  0.992365, 0.984508, 1.02976,
     *              0.999952, 0.978135, 0.983435, 0.979618, 0.960319,
     *              0.955105/
      DATA OA512  / 1.06243,  0.960791, 0.967937, 0.984130, 0.988731,
     *              0.977394, 1.00242,  1.02873,  0.981971, 1.00131,
     *              1.03695,  0.985894, 1.01325,  1.01861,  1.00716,
     *              1.00797,  0.966051, 1.03419,  1.00194,  1.00288,
     *              1.01248,  0.993181, 0.991889, 0.994298, 0.983398,
     *              1.02947,  1.03408,  1.00272,  1.00800,  1.00127,
     *              1.01899,  0.982139, 0.991639, 0.982332, 1.01846,
     *              0.997449, 1.00639,  0.998473, 1.03248,  1.02884,
     *              0.984723, 0.997487, 0.985365, 0.993409, 1.00760,
     *              1.00725,  0.998229, 1.03216,  0.969702, 1.01476,
     *              1.00323,  1.02033,  1.01542,  0.991306, 1.04116,
     *              1.00490,  0.990299, 1.02774,  0.999153, 0.975815,
     *              0.991038, 0.975246, 0.953915, 0.960692, 1.00484,
     *              0.962489, 0.958678, 0.977143, 0.985122, 0.976190,
     *              1.00526,  1.03027,  0.982758, 1.00267,  1.03760,
     *              0.982894, 1.01137,  1.02108,  1.00847,  1.01169,
     *              0.969368, 1.03291,  0.998014, 1.00516,  1.00836,
     *              0.991988, 0.994542, 0.994029, 0.982304, 1.02954,
     *              1.02914,  0.996647, 1.00646,  1.00131,  1.01662,
     *              0.984123, 0.991632, 0.988420, 1.01388,  0.999566 /
      DATA OB512  / 1.00778,  0.999996, 1.02937,  1.03245,  0.984185,
     *              0.996266, 0.987022, 0.990756, 1.00657,  1.00114,
     *              0.999427, 1.03368,  0.967831, 1.01251,  1.00420,
     *              1.02257,  1.01490,  0.989455, 1.03869,  1.00442,
     *              0.988233, 1.02977,  0.996602, 0.976390, 0.990743,
     *              0.972890, 0.951501, 0.955515, 1.01006,  0.962449,
     *              0.956602, 0.974402, 0.986434, 0.974873, 1.00354,
     *              1.02959,  0.980796, 1.00434,  1.03921,  0.988532,
     *              1.01246,  1.02114,  1.01115,  1.01070,  0.968601,
     *              1.03532,  1.00162,  1.00408,  1.00800,  0.993203,
     *              0.990762, 0.994174, 0.981345, 1.03294,  1.03377,
     *              0.998423, 1.00708,  1.00107,  1.01413,  0.981345,
     *              0.990493, 0.984075, 1.01799,  0.998249, 1.00552,
     *              0.999964, 1.02823,  1.03021,  0.984741, 0.998358,
     *              0.988039, 0.993262, 1.00702,  0.998766, 0.992641,
     *              1.03079,  0.968991, 1.01287,  1.00061,  1.02464,
     *              1.01624,  0.993228, 1.04099,  1.00502,  0.993262,
     *              1.03060,  1.00179,  0.971457, 0.989804, 0.974644,
     *              0.955522, 0.956949, 1.00436,  0.964444, 0.958748,
     *              0.976046, 0.983893, 0.973838, 1.00368,  1.02928 /
      DATA OC512  / 0.977306, 0.997290, 1.03495,  0.986230, 1.01222,
     *              1.02167,  1.00806,  1.00758,  0.965874, 1.03134,
     *              1.00048,  1.00480,  1.00806,  0.992292, 0.989054,
     *              0.996344, 0.983955, 1.03361,  1.03535,  1.00314,
     *              1.00784,  1.00137,  1.01648,  0.982375, 0.988047,
     *              0.981398, 1.01632,  0.997200, 1.00598,  1.00094,
     *              1.03048,  1.02778,  0.981778, 0.997652, 0.984429,
     *              0.988595, 1.00496,  1.00072,  0.994260, 1.02955,
     *              0.967973, 1.01248,  1.00339,  1.02375,  1.00886,
     *              0.988134, 1.03875,  1.00050,  0.989853, 1.02787,
     *              1.00058,  0.974549, 0.995072, 0.978601, 0.956070,
     *              0.957349 /
      DATA EA256  / 1.05327,  0.964406, 0.970796, 0.987399, 0.987119,
     *              0.976081, 1.00099,  1.02738,  0.981748, 1.00093,
     *              1.03589,  0.987794, 1.01243,  1.01824,  1.00724,
     *              1.00650,  0.968272, 1.03074,  0.998037, 1.00261,
     *              1.00554,  0.991803, 0.989406, 0.992276, 0.986340,
     *              1.03306,  1.03352,  1.00088,  1.00583,  1.00042,
     *              1.01769,  0.981759, 0.989495, 0.983560, 1.01504,
     *              0.998181, 1.00710,  0.999466, 1.03135,  1.03043,
     *              0.984835, 1.00001,  0.990090, 0.991381, 1.00844,
     *              1.00219,  0.991798, 1.02793,  0.968231, 1.01384,
     *              1.00495,  1.02106,  1.01276,  0.992309, 1.04067,
     *              1.00489,  0.987857, 1.02950,  1.00075,  0.973363,
     *              0.992325, 0.977653, 0.955176, 0.959877, 1.00473,
     *              0.962442, 0.960857, 0.979025, 0.985851, 0.972855,
     *              1.00390,  1.02940,  0.983266, 1.00140,  1.03428,
     *              0.985256, 1.00803,  1.02110,  1.00950,  1.00994,
     *              0.971485, 1.03068,  1.00195,  1.00536,  1.01080,
     *              0.986821, 0.988061, 0.991492, 0.982145, 1.02635,
     *              1.03260,  0.997700, 1.00490,  1.00454,  1.01601,
     *              0.983259, 0.991015, 0.978974, 1.01510,  1.00049 /
      DATA EB256  / 1.00353,  1.00236,  1.02970,  1.03213,  0.985028,
     *              0.999269, 0.986415, 0.996913, 1.00554,  1.00028,
     *              0.994999, 1.03222,  0.971401, 1.01434,  1.00283,
     *              1.02247,  1.00906,  0.988720, 1.03591,  1.00599,
     *              0.989918, 1.02687,  0.998699, 0.973223, 0.990852,
     *              0.978831, 0.950902, 0.957106 /
      DATA OA256  / 1.05105,  0.956046, 0.971141, 0.990535, 0.981796,
     *              0.980438, 1.00065,  1.02564,  0.979502, 1.00347,
     *              1.03347,  0.988997, 1.00740,  1.02004,  1.00112,
     *              1.00942,  0.967546, 1.03506,  1.00243,  1.00558,
     *              1.00747,  0.985830, 0.990280, 1.00258,  0.988672,
     *              1.03139,  1.02826,  0.997451, 1.00629,  0.996970,
     *              1.01470,  0.981654, 0.995250, 0.982382, 1.01441,
     *              1.00293,  1.00727,  1.00190,  1.03600,  1.02791,
     *              0.983053, 0.999879, 0.986592, 0.994289, 1.00675,
     *              1.00475,  0.993508, 1.03155,  0.969374, 1.01655,
     *              1.00051,  1.02234,  1.00733,  0.989871, 1.03629,
     *              0.998067, 0.987149, 1.03092,  0.999300, 0.976478,
     *              0.983642, 0.979422, 0.960831, 0.956117, 1.00603,
     *              0.959667, 0.964124, 0.984259, 0.981797, 0.977976,
     *              1.00304,  1.02880,  0.983425, 1.00541,  1.03423,
     *              0.989363, 1.00204,  1.02330,  1.00523,  1.01343,
     *              0.971131, 1.03252,  1.00094,  1.00592,  1.00795,
     *              0.979976, 0.990260, 1.00323,  0.984228, 1.02320,
     *              1.03084,  0.995212, 1.00833,  1.00343,  1.01339,
     *              0.980518, 0.995815, 0.975561, 1.01140,  1.00294 /
      DATA OB256  / 1.00397,  1.00259,  1.03196,  1.02870,  0.986682,
     *              0.999565, 0.984309, 1.00384,  1.00613,  1.00158,
     *              0.997305, 1.03261,  0.971316, 1.01691,  0.997464,
     *              1.02239,  1.00783,  0.989068, 1.03324,  1.00093,
     *              0.989825, 1.02991,  0.996003, 0.979113, 0.986929,
     *              0.985125, 0.955482, 0.954792 /
      DATA OA128  / 1.05606,  1.00146,  1.00590,  1.00639,  0.999286,
     *              1.00124,  0.995951, 1.00110,  0.997750, 1.00040,
     *              0.996622, 0.999919, 0.997436, 1.00021,  0.997958,
     *              1.00172,  0.998202, 1.00078,  0.998106, 1.00200,
     *              0.996396, 1.00027,  0.996716, 0.999550, 0.996174,
     *              1.00209,  0.996120, 0.997973, 0.995639, 1.00039,
     *              0.997197, 1.00161,  1.00306,  0.999653, 0.998182,
     *              1.00342,  0.998208, 0.999358, 0.995573, 1.00011,
     *              0.996144, 1.00054,  0.996735, 1.00215,  0.997820,
     *              1.00026,  0.997853, 1.00055,  0.997367, 0.999757,
     *              0.996927, 1.00128,  0.997119, 0.998549, 0.997212,
     *              1.00027,  0.996886, 1.00108,  0.998911, 0.998969,
     *              0.997248, 0.997180, 0.994855, 0.998176 /
      DATA EA128  / 1.05762,  1.00196,  1.00622,  1.00800,  0.999245,
     *              0.999806, 0.995800, 1.00023,  0.996619, 1.00055,
     *              0.997133, 0.998053, 0.995499, 0.998076, 0.997073,
     *              1.00227,  0.998141, 0.999329, 0.997187, 1.00241,
     *              0.997469, 1.00138,  0.996649, 0.999889, 0.997365,
     *              1.00090,  0.998004, 1.00121,  0.998078, 1.00287,
     *              0.998910, 1.00063,  1.00247,  0.998471, 0.998176,
     *              1.00332,  0.998496, 0.999841, 0.995941, 1.00017,
     *              0.997494, 1.00055,  0.996316, 0.999661, 0.997059,
     *              1.00024,  0.995416, 1.00075,  0.999028, 1.00044,
     *              0.995932, 0.999379, 0.997494, 0.999114, 0.996125,
     *              0.999581, 0.997460, 1.00068,  0.997861, 0.998375,
     *              0.997658, 0.996641, 0.995409, 0.999864 /
      DATA OA64   / 1.03980,  0.996614, 1.00517,  1.00807,  0.995845,
     *              1.00055,  0.995368, 0.999584, 0.997952, 0.999456,
     *              0.998114, 0.999359, 0.994940, 0.999630, 0.993892,
     *              0.999947, 0.996816, 1.00049,  0.999282, 1.00124,
     *              0.996190, 0.998997, 0.995863, 0.999179, 0.997118,
     *              0.999187, 0.997264, 1.00251,  0.995615, 0.997490,
     *              0.997901, 1.00058 /
      DATA EA64   / 1.04012,  0.997078, 1.00478,  1.00652,  0.994880,
     *              0.999488, 0.995171, 0.999027, 0.996738, 0.999074,
     *              0.997542, 0.999123, 0.996064, 0.998961, 0.993940,
     *              1.00138,  0.998121, 1.00098,  0.998663, 1.00147,
     *              0.996940, 0.998728, 0.996252, 0.999721, 0.998359,
     *              0.999660, 0.997980, 1.00211,  0.994675, 0.997835,
     *              0.998209, 1.00040 /
C---------------------------------------------------------------------
C                                       Determine correlator channel
C                                       order
      DO 10 I = 1, MIF
C                                       corr channels 0, 2, 4, 6
         IF (MOD(I,2).NE.0) PARIT(I) = 0
C                                       corr channels 1, 3, 5, 7
         IF (MOD(I,2).EQ.0) PARIT(I) = 1
   10    CONTINUE
C                                       If reordered IFs, that makes it
C                                       harder to determine which is odd
C                                       or even from the correlator's
C                                       perspective.
      IF (REORDR(CURFQI)) THEN
         DO 20 I = 1, MIF
            NEWIF = FORDER(I,CURFQI)
C                                       corr channels 0, 2, 4, 6
            IF (MOD(I,2).NE.0) PARIT(NEWIF) = 0
C                                       corr channels 1, 3, 5, 7
            IF (MOD(I,2).EQ.0) PARIT(NEWIF) = 1
   20       CONTINUE
         END IF
C
      IF ((FFTSIZ.GT.2048) .OR. (FFTSIZ.LT.64)) GO TO 999
C
      WRITE (HILINE,1000) TSKNAM, FFTSIZ
      CALL HIADD (IHLUN, HILINE, IHBLK, HERR)
      NF = FFTSIZ / 2
      IF (FFTSIZ.EQ.2048) THEN
         CALL RCOPY (NF, OD2048, FFTODD)
         CALL RCOPY (NF, OD2048, FFTEVN)
      ELSE IF (FFTSIZ.EQ.1024) THEN
         CALL RCOPY (NF, OD1024, FFTODD)
         CALL RCOPY (NF, OD1024, FFTEVN)
      ELSE IF (FFTSIZ.EQ.512) THEN
         CALL RCOPY (NF, ODD512, FFTODD)
         CALL RCOPY (NF, EVN512, FFTEVN)
      ELSE IF (FFTSIZ.EQ.256) THEN
         CALL RCOPY (NF, ODD256, FFTODD)
         CALL RCOPY (NF, EVN256, FFTEVN)
      ELSE IF (FFTSIZ.EQ.128) THEN
         CALL RCOPY (NF, OA128, FFTODD)
         CALL RCOPY (NF, EA128, FFTEVN)
      ELSE IF (FFTSIZ.EQ.64) THEN
         CALL RCOPY (NF, OA64, FFTODD)
         CALL RCOPY (NF, EA64, FFTEVN)
         END IF
C                                       Normalize
      SUM0 = 0.0
      SUM1 = 0.0
      DO 50 JF = 1, NF
         SUM0 = SUM0 + FFTEVN(JF)
         SUM1 = SUM1 + FFTODD(JF)
   50    CONTINUE
      SUM0 = SUM0 / NF
      SUM1 = SUM1 / NF
      DO 60 JF = 1, NF
         FFT0(JF) = FFTEVN(JF) * (1.0 / SUM0)
         FFT1(JF) = FFTODD(JF) * (1.0 / SUM1)
   60    CONTINUE
C                                       Any spectral averaging?
      IF (NF.NE.LFRQ) THEN
         CALL RCOPY (NF, FFT0, FFTEVN)
         CALL RCOPY (NF, FFT1, FFTODD)
         CALL RFILL (LMXCHA, 0.0, FFT0)
         CALL RFILL (LMXCHA, 0.0, FFT1)
         NAVG = NF / LFRQ
         LF = 1
         DO 80 JF = 1, NF, NAVG
            KF = JF
            DO 70 I = 1, NAVG
               FFT0(LF) = FFT0(LF) + FFTEVN(KF+I-1)
               FFT1(LF) = FFT1(LF) + FFTODD(KF+I-1)
   70          CONTINUE
            FFT0(LF) = FFT0(LF) / FLOAT(NAVG)
            FFT1(LF) = FFT1(LF) / FLOAT(NAVG)
            LF = LF + 1
   80       CONTINUE
         WRITE (HILINE,1010) TSKNAM, NAVG
         CALL HIADD (IHLUN, HILINE, IHBLK, HERR)
         END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,' FFT artifacts corrected for FFT size of ',I6)
 1010 FORMAT (A6,' FFT artifacts corrected for spectral average of ',I6)
      END
      SUBROUTINE CORART (DOUVCM, OUTPTR, UVBUFF)
C-----------------------------------------------------------------------
C Routine to correct the total power spectra from the VLBA correlator
C for the effects of the FFT artifacts introduced by the hardware
C FFTs (most of the artifacts are introduced in the radix-2 stage).
C The spectra are divided by an artifact spectrum.
C ---------------------------------------------------------------------
C  Inputs:
C     DOUVCM    L     If true output data are compressed, value will
C                     affect data increments.
C     OUTPTR    I     Pointer in buffer at which data start.
C  Inputs in common:
C     FFTSIZ    I     Number of points in the input of FFT
C     CURA1     I     The first antenna number
C     CURA2     I     The second antenna number
C     FFTO(*)   R     The FFT artifact correction array for odd
C                     IF's in AIPS, will be even IF's from the
C                     correlator. Must know which is which before
C                     frequency ordering occurs.
C     FFT1(*)   R     The FFT artifact correction array for even
C                     IF's in AIPS, will be odd IF's from the
C                     correlator. Must know which is which before
C                     frequency ordering occurs.
C  In/Out:
C     UVBUFF(*) R     Array of visibilities
C  Outputs:
C     IERR      I          Error code.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL   DOUVCM
      INTEGER   OUTPTR, NS, NIF, NF, INDEX, JS, JF, JIF, IM
      REAL      UVBUFF(*)
      INCLUDE 'DIGCOR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF ((FFTSIZ.GT.2048) .OR. (FFTSIZ.LT.64)) GO TO 999
C
      NS = MPOL
      NIF = MIF
      NF = MFRQ
      IM = 1
      IF (DOUVCM) IM = 3
C
      DO 70  JS = 1,NS
         DO 60 JIF = 1,NIF
            DO 50  JF = 1,NF
               INDEX = (JIF-1) * (INCIF*IM) + (JF-1) * (INCF*IM) +
     *            (JS-1) * (INCS*IM) + OUTPTR
C                                       Real and image part of spectrum
               IF (PARIT(JIF).EQ.0) THEN
                  UVBUFF(INDEX) = UVBUFF(INDEX) / FFT0(JF)
               ELSE IF (PARIT(JIF).EQ.1) THEN
                  UVBUFF(INDEX) = UVBUFF(INDEX) / FFT1(JF)
                  END IF
   50          CONTINUE
   60       CONTINUE
   70    CONTINUE
C
  999 RETURN
      END
      SUBROUTINE BFAC (UVBUFF, DOCORR, CORREL, DOUVCM, SCLVIS)
C-----------------------------------------------------------------------
C   Applies digital correction for both AUTO and CROSS correlation
C   spectrum. In the CROSS case the correction is applyed as a factor
C   because only a low cross correlation is considered.  The program
C   calculates the crosscorrelation spectrum's  component using the
C   formula:
C
C           R(L) = KSI(L) * BFACT
C           BFACT = (GAMMA**2) / (A*RM*ALFA*H(0))
C
C WHERE     KSI(L) and R(L) are the measured and estimated amplitude;
C           A is a coefficient applied to the data at the correlator;
C           RM is the maximum possible value of digital signals'
C           correlation, specified by the types of digitizers;
C           ALFA is a slope of the relation between amplitudes of input
C           analog signals' correlation and normalized  digit signals'
C           correlation;
C           H(0) is the value of the normalized auto convolution function
C           of a tapering function at FFT if the argument equals 0.
C           GAMMA is a known factor applied to the digitizers' levels at
C           the VLBA correlator.
C
C   In the AUTO case a full non linear correction is applied in the
C   delay domain after performing a Fourier transform from a measured
C   spectrum to a correlation function and then the inverse Fourier
C   transform of the corrected correlation function to the spectrum.  As
C   of version 8.1 the saturation effect is compensated for in the case
C   of self-spectra.
C   Input:
C      DOCORR   I      -1  => no digital corrections
C                      1,3 => all corrections
C                      2,4 => do cross-power + total power only if
C                           zero-padding done in correlator.
C      CORREL   C*(*)  If 'DIFX...' (not VLBA) SATUR = 1.0
C      DOUVCM   L      If TRUE output data are compressed.
C      SCLVIS   R      The visibility scaling factor. If a visibility
C                      measured by the VLBA has weight w then the
C                      actual fraction of the integration period over
C                      which the data were acculated was w/SCLVIS and
C                      0.0 <= |w| / SCLVIS <= 1.0.
C   Inputs in common:
C      FFTSIZ   I      Number of points in the input of FFT
C      OVRSMP   I      Factor of oversampling;
C                      0 => no oversampling, i.e. FSAMP = 2*DELTAF;
C                      1 => FSAMP = 4*DELTAF etc.
C      ZEROPD   I      Factor of zeropadding at FFT;
C                      0 => no zeropadding; 1 => N additional zeros;
C                      2 => 3*N additional zeros etc.
C      NLEV(*)  I      Array of numbers of bits in the digitisers
C      CURA1    I      The first antenna number
C      CURA2    I      The second antenna number
C      TAPER    C*8    Type of weighting function in FFT
C   In/Out:
C      UVBUFF   R(*)   Array of visibilities
C   Outputs:
C      IERR     I      Error code.
C-----------------------------------------------------------------------
      CHARACTER CORREL*(*)
      REAL      SCLVIS
      INTEGER   INDDIG, NS, NIF, NF, INDEX, JS, JF, JIF, IM, DOCORR,
     *   NSSAT
      REAL      H, RM, ALFAC, BFACTC, BFACTA, COEFA, UVBUFF(*),
     *   GAMMA, SATUR
      LOGICAL   CORREC, CROS, DOUVCM
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'DIGCOR.INC'
      INCLUDE 'SCRTCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       H is the value of auto
C                                       convolution of a tapering
C                                       function (argument equals 0)
C
C                                       If the taper is BOX.
C                                       H(0) = (15/16)**2
C                                       i.e the VLBA correlator
      H = 0.87890625
C                                       If the taper is Hanning.
C                                       The value obtained as a maximum
C                                       in the auto convolution function
C                                       of the step-representation of
C                                       the Hanning function in the
C                                       VLBA correlator. The mean value
C                                       between maxima for 64 and 512
C                                       FFT points is taken.
      IF (TAPER(:4).EQ.'HANN') H = 0.333
C                                       Determine RMAX and slope of
C                                       a normalized conversion function
C                                       corresponding to the digitizer's
C                                       type
      CROS = CURA1.NE.CURA2
      IF (NLEVS(CURA1).EQ.NLEVS(CURA2)) THEN
         IF (NLEVS(CURA1).EQ.4)THEN
            INDDIG = 44
            RM =  4.3048
            ALFAC = 0.882518
            GAMMA = 3.335875 * 64.0/ 63.0
         ELSE
            INDDIG =22
            RM = 1.0
            ALFAC = 2.0 / PI
            GAMMA = 1.0 * 64.0 / 63.0
            END IF
      ELSE
         INDDIG = 24
         RM =   5.8784
         ALFAC = 0.882518
         GAMMA = 3.335875 * 64.0 / 63.0
         END IF
C
C
C
      COEFA = 5.360
C                                       COEFFA is found from a test
C                                       experiment using data from
C                                       AUTO and CROS correlation
      BFACTC = COEFA * RM * ALFAC * H / (GAMMA*GAMMA)
      BFACTA = COEFA * RM * H / (GAMMA*GAMMA)
      NS = MPOL
      NIF = MIF
      NF = MFRQ
      NSSAT = 1
      IF (NS.EQ.4) NSSAT = 2
C                                       digital correction can be
C                                       applied to auto correlation
C                                       if zero padding exists
      CORREC = (ZEROPD.NE.0)
      IF (MOD(DOCORR,2).EQ.1) CORREC = .TRUE.
      IF (((.NOT.CROS) .AND. (.NOT.CORREC)) .AND. DOPR) THEN
         MSGTXT = 'Auto-correlation not corrected for digital '
         CALL MSGWRT (8)
         MSGTXT = 'representation of signals because is not ' //
     *      'zero padded'
         CALL MSGWRT (8)
         DOPR = .FALSE.
         END IF
      IF ((.NOT.CROS) .AND. (.NOT.CORREC)) GO TO 999
      IM = 1
      IF (DOUVCM) IM = 3
      DO 160  JS = 1,NS
         DO 140 JIF = 1,NIF
            DO 40  JF = 1,NF
               INDEX = (JIF-1) * (INCIF*IM) + (JF-1) * (INCF*IM) +
     *            (JS-1) * (INCS*IM) + 1
C                                       C R O S S  C O R E L A T I O N
               IF ((CROS) .OR. (JS.GT.2)) THEN
                  UVBUFF(INDEX) = UVBUFF(INDEX) / BFACTC
                  UVBUFF(INDEX+1) = UVBUFF(INDEX+1) / BFACTC
C                                       Real and imag part of spectrum
               ELSE
                  SCRDAT(2*JF-1) = UVBUFF(INDEX)
                  SCRDAT(2*JF)   = UVBUFF(INDEX+1)
                  SCRDAT(2*(JF+NF)-1) = 0.0
                  SCRDAT(2*(JF+NF)) = 0.0
                  END IF
 40            CONTINUE
C                                       A U T O  C O R R E L A T I O N
            IF ((.NOT.CROS) .AND. (JS.LE.2)) THEN
               CALL AUTOC (NF, INDDIG, SCRDAT, SCRWRK)
               SATUR = 1.0
               DO 100 JF = 1, NF
                  INDEX = (JIF-1) * (INCIF*IM) + (JF-1) * (INCF*IM) +
     *               (JS-1) * (INCS*IM) + 1
C                                       compensate for the saturation
C                                       effect in the VLBA correlator.
                  IF (CORREL(:4).EQ.'VLBA') SATUR = 0.125 * NSSAT *
     *               UVBUFF(INDEX+2) / SCLVIS + 1.0
                  UVBUFF(INDEX) = (SCRDAT(2*JF-1) / BFACTA) * SATUR
                  UVBUFF(INDEX+1) = (SCRDAT(2*JF) / BFACTA) * SATUR
 100              CONTINUE
               END IF
 140        CONTINUE
 160     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE AUTOC (NF, INDDIG, DATA, WORK)
C-----------------------------------------------------------------------
C   Routine performs the digital correction of auto correlation spectrum
C   transforming a measured spectrum to the delay domain, applying a
C   digital correction to the obtained auto correlation function and
C   transforming the corrected autocorrelation function to frequency
C   domain.
C   Inputs:
C      NF        I     Number of points in a measured spectrum
C                     a given visibility
C      INDDIG    I     Code of the digitizer's level
C   Inputs in common:
C      FFTSIZ    I     Number of points in the input of FFT
C      OVRSMP    I     Factor of oversampling;
C                     0 => no oversampling, i.e. FSAMP = 2*DELTAF;
C                     1 => FSAMP = 4*DELTAF etc.
C      ZEROPD    I     Factor of zeropadding at FFT;
C                     0 => no zeropadding; 1 => N additional zeros;
C                     2 => 3*N additional zeros etc.
C      NLEV(*)   I     Array of numbers of bits in the digitisers
C      CURA1     I     The first antenna number
C      CURA2     I     The second antenna number
C      TAPER    C*8  Type of weighting function in FFT
C   In/Out:
C      DATA     R(*)   Real&Imag part of In/Out spectrum
C   Output
C      WORK     R(*)   size of DATA (4*NF)
C   FFT from the spectrum to correlation using the formula:
C             R1(i)=-KSI(1)-KSI(N/2 + 1)*(-1)**i +
C        2 * SUM(1,N/2)[KSI(l) * COS(TWOPI*(i-1)*(l-1)/N)]
C   We don't know KSI(N/2 + 1), so we'll extrapolate it
C-----------------------------------------------------------------------
      INTEGER   INDDIG, NF, JF, TNF, JJ
      REAL      DATA(*), WORK(*), KSIBEG, KSIEND, KSIT, WEIGHT, JFN,
     *   ARG, CORMAX, RDIG, RO
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'DIGCOR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      TNF = 2 * NF
      KSIBEG = DATA(1)
      KSIEND = 2*DATA(TNF-1) - DATA(TNF-3)
C                                       DATA is measured spectrum
      CALL FOURG (DATA, TNF, 1, WORK)
C                                       KSIT is R(N/2+1) uncorrected
      KSIT = (2 * DATA(TNF+1) - KSIBEG + KSIEND * (-1)**NF) / TNF
C                                       Do correction
C                                       We consider ZEROPAD=1
      DO 60 JF = 1,NF+1
         JJ = 2*JF - 1
         DATA(JJ) = (2 * DATA(JJ) - KSIBEG - KSIEND * (-1)**JF) / TNF
C                                       DATA is correlation function
C                                       Do correction of DATA(2*JF-1)
         JFN = (JF-1.0)/NF
         IF (TAPER(:4).EQ.'HANN') THEN
            ARG = TWOPI * JFN
            WEIGHT = 0.25 * ((1 - JFN) * (1 + 0.5 * COS(ARG))
     *         + (1.5/TWOPI) * SIN(ARG))
C                                       TAPER.EQ.BOX
         ELSE
            WEIGHT = 1.0 - JFN
            END IF
         IF (ABS(WEIGHT).GT.1.0E-20) THEN
            DATA(JJ) = DATA(JJ) / WEIGHT
C                                       normalise DATA(1) to 1
            IF (JF.EQ.1) THEN
               CORMAX = DATA(1)
               IF (CORMAX.LT.1.0E-20) CORMAX = 1.0
               END IF
            RDIG = DATA(JJ) / CORMAX
            CALL RORC (RDIG, INDDIG, RO)
C
            DATA(JJ) = RO * CORMAX * WEIGHT
         ELSE
            DATA(JJ) = 0.0
            END IF
         DATA(JJ+1) = 0.0
         IF (JF.NE.1) DATA(JJ+TNF) = 0.0
         DATA(TNF+JJ+1) = 0.0
   60    CONTINUE
C                                       DATA is now corrected
C                                       correlation function.
C                                       FFT back to the spectrum
      KSIBEG = DATA(1)
C                                       KSIEND is R(N/2+1) corrected
      KSIEND = DATA(TNF+1)
      DATA(TNF+1) = 0.0
      DATA(TNF+2) = 0.0
C                                       The next line is used if
C                                       we desire to check exact resto-
C                                       ration of the original spectra
C                                       without correction
C        KSIEND = KSIT

      CALL FOURG (DATA, TNF, 1, WORK)
C
      DO 80 JF = 1,NF
         JJ = 2 * JF - 1
         DATA(JJ) = 2 * DATA(JJ) - KSIBEG - KSIEND * (-1)**JF
         DATA(JJ+1) = 0.0
 80      CONTINUE
C                                       DATA is now corrected spectrum
 999  RETURN
      END
      SUBROUTINE RORC (RDIG, INDDIG, RO)
C----------------------------------------------------------------------
C   RORC finds the value of analog correlation ( RO ) corresponding to
C   the value of the measured digital correlation (RDIG) and known pair
C   of digitisers. the conversion function is determined by the tables
C   given in data.
C   Input:
C      RDIG     R    correlation of digital signals
C      INDDIG   I    determine the pair of digitizers
C   Output:
C      RO       R    relevant analog correlation
C----------------------------------------------------------------------
      REAL  ROSTEP, RORO(1000), RR(1000), RMAX, RMIN, RDIG, RDIGM, RO
      REAL RR221(100), RR222(100), RR223(100), RR224(100), RR225(100),
     *     RR226(100), RR227(100), RR228(100), RR229(100), RR2210(100)
      REAL RR241(100), RR242(100), RR243(100), RR244(100), RR245(100),
     *     RR246(100), RR247(100), RR248(100), RR249(100), RR2410(100)
      REAL RR441(100), RR442(100), RR443(100), RR444(100), RR445(100),
     *     RR446(100), RR447(100), RR448(100), RR449(100), RR4410(100)
      INTEGER I, KMIN, KMAX, KMED, KKMAX, INDDIG
      DATA KKMAX, RMIN, RMAX /1000, 0.0, 1.0/
      DATA RR221 / 0.00064, 0.00127, 0.00191, 0.00255, 0.00318,
     *   0.00382, 0.00446, 0.00509, 0.00573, 0.00637,
     *   0.00700, 0.00764, 0.00828, 0.00891, 0.00955,
     *   0.01019, 0.01082, 0.01146, 0.01210, 0.01273,
     *   0.01337, 0.01401, 0.01464, 0.01528, 0.01592,
     *   0.01655, 0.01719, 0.01783, 0.01846, 0.01910,
     *   0.01974, 0.02038, 0.02101, 0.02165, 0.02229,
     *   0.02292, 0.02356, 0.02420, 0.02483, 0.02547,
     *   0.02611, 0.02675, 0.02738, 0.02802, 0.02866,
     *   0.02929, 0.02993, 0.03057, 0.03121, 0.03184,
     *   0.03248, 0.03312, 0.03376, 0.03439, 0.03503,
     *   0.03567, 0.03631, 0.03694, 0.03758, 0.03822,
     *   0.03886, 0.03950, 0.04013, 0.04077, 0.04141,
     *   0.04205, 0.04269, 0.04332, 0.04396, 0.04460,
     *   0.04524, 0.04588, 0.04651, 0.04715, 0.04779,
     *   0.04843, 0.04907, 0.04971, 0.05035, 0.05098,
     *   0.05162, 0.05226, 0.05290, 0.05354, 0.05418,
     *   0.05482, 0.05546, 0.05610, 0.05673, 0.05737,
     *   0.05801, 0.05865, 0.05929, 0.05993, 0.06057,
     *   0.06121, 0.06185, 0.06249, 0.06313, 0.06377 /
      DATA RR222 / 0.06441, 0.06505, 0.06569, 0.06633, 0.06697,
     *   0.06761, 0.06825, 0.06889, 0.06953, 0.07017,
     *   0.07081, 0.07145, 0.07209, 0.07273, 0.07337,
     *   0.07401, 0.07466, 0.07530, 0.07594, 0.07658,
     *   0.07722, 0.07786, 0.07850, 0.07914, 0.07979,
     *   0.08043, 0.08107, 0.08171, 0.08235, 0.08300,
     *   0.08364, 0.08428, 0.08492, 0.08556, 0.08621,
     *   0.08685, 0.08749, 0.08813, 0.08878, 0.08942,
     *   0.09006, 0.09071, 0.09135, 0.09199, 0.09264,
     *   0.09328, 0.09392, 0.09457, 0.09521, 0.09585,
     *   0.09650, 0.09714, 0.09779, 0.09843, 0.09908,
     *   0.09972, 0.10036, 0.10101, 0.10165, 0.10230,
     *   0.10294, 0.10359, 0.10423, 0.10488, 0.10552,
     *   0.10617, 0.10682, 0.10746, 0.10811, 0.10875,
     *   0.10940, 0.11005, 0.11069, 0.11134, 0.11199,
     *   0.11263, 0.11328, 0.11393, 0.11457, 0.11522,
     *   0.11587, 0.11651, 0.11716, 0.11781, 0.11846,
     *   0.11910, 0.11975, 0.12040, 0.12105, 0.12170,
     *   0.12235, 0.12299, 0.12364, 0.12429, 0.12494,
     *   0.12559, 0.12624, 0.12689, 0.12754, 0.12819 /
      DATA RR223 / 0.12884, 0.12949, 0.13014, 0.13079, 0.13144,
     *   0.13209, 0.13274, 0.13339, 0.13404, 0.13469,
     *   0.13534, 0.13600, 0.13665, 0.13730, 0.13795,
     *   0.13860, 0.13925, 0.13991, 0.14056, 0.14121,
     *   0.14186, 0.14252, 0.14317, 0.14382, 0.14448,
     *   0.14513, 0.14578, 0.14644, 0.14709, 0.14775,
     *   0.14840, 0.14905, 0.14971, 0.15036, 0.15102,
     *   0.15167, 0.15233, 0.15298, 0.15364, 0.15429,
     *   0.15495, 0.15561, 0.15626, 0.15692, 0.15758,
     *   0.15823, 0.15889, 0.15955, 0.16020, 0.16086,
     *   0.16152, 0.16218, 0.16283, 0.16349, 0.16415,
     *   0.16481, 0.16547, 0.16613, 0.16679, 0.16745,
     *   0.16810, 0.16876, 0.16942, 0.17008, 0.17074,
     *   0.17140, 0.17206, 0.17273, 0.17339, 0.17405,
     *   0.17471, 0.17537, 0.17603, 0.17669, 0.17736,
     *   0.17802, 0.17868, 0.17934, 0.18001, 0.18067,
     *   0.18133, 0.18200, 0.18266, 0.18332, 0.18399,
     *   0.18465, 0.18532, 0.18598, 0.18665, 0.18731,
     *   0.18798, 0.18864, 0.18931, 0.18997, 0.19064,
     *   0.19131, 0.19197, 0.19264, 0.19331, 0.19397 /
      DATA RR224 / 0.19464, 0.19531, 0.19598, 0.19664, 0.19731,
     *   0.19798, 0.19865, 0.19932, 0.19999, 0.20066,
     *   0.20133, 0.20200, 0.20267, 0.20334, 0.20401,
     *   0.20468, 0.20535, 0.20602, 0.20669, 0.20737,
     *   0.20804, 0.20871, 0.20938, 0.21006, 0.21073,
     *   0.21140, 0.21208, 0.21275, 0.21342, 0.21410,
     *   0.21477, 0.21545, 0.21612, 0.21680, 0.21747,
     *   0.21815, 0.21882, 0.21950, 0.22018, 0.22085,
     *   0.22153, 0.22221, 0.22289, 0.22356, 0.22424,
     *   0.22492, 0.22560, 0.22628, 0.22696, 0.22764,
     *   0.22832, 0.22900, 0.22968, 0.23036, 0.23104,
     *   0.23172, 0.23240, 0.23308, 0.23376, 0.23445,
     *   0.23513, 0.23581, 0.23650, 0.23718, 0.23786,
     *   0.23855, 0.23923, 0.23991, 0.24060, 0.24128,
     *   0.24197, 0.24266, 0.24334, 0.24403, 0.24471,
     *   0.24540, 0.24609, 0.24678, 0.24746, 0.24815,
     *   0.24884, 0.24953, 0.25022, 0.25091, 0.25160,
     *   0.25229, 0.25298, 0.25367, 0.25436, 0.25505,
     *   0.25574, 0.25643, 0.25713, 0.25782, 0.25851,
     *   0.25920, 0.25990, 0.26059, 0.26129, 0.26198 /
      DATA RR225 / 0.26267, 0.26337, 0.26407, 0.26476, 0.26546,
     *   0.26615, 0.26685, 0.26755, 0.26824, 0.26894,
     *   0.26964, 0.27034, 0.27104, 0.27174, 0.27244,
     *   0.27314, 0.27384, 0.27454, 0.27524, 0.27594,
     *   0.27664, 0.27734, 0.27805, 0.27875, 0.27945,
     *   0.28016, 0.28086, 0.28156, 0.28227, 0.28297,
     *   0.28368, 0.28438, 0.28509, 0.28580, 0.28650,
     *   0.28721, 0.28792, 0.28863, 0.28933, 0.29004,
     *   0.29075, 0.29146, 0.29217, 0.29288, 0.29359,
     *   0.29430, 0.29502, 0.29573, 0.29644, 0.29715,
     *   0.29787, 0.29858, 0.29929, 0.30001, 0.30072,
     *   0.30144, 0.30215, 0.30287, 0.30358, 0.30430,
     *   0.30502, 0.30574, 0.30645, 0.30717, 0.30789,
     *   0.30861, 0.30933, 0.31005, 0.31077, 0.31149,
     *   0.31221, 0.31294, 0.31366, 0.31438, 0.31510,
     *   0.31583, 0.31655, 0.31728, 0.31800, 0.31873,
     *   0.31945, 0.32018, 0.32091, 0.32163, 0.32236,
     *   0.32309, 0.32382, 0.32455, 0.32528, 0.32601,
     *   0.32674, 0.32747, 0.32820, 0.32893, 0.32966,
     *   0.33040, 0.33113, 0.33186, 0.33260, 0.33333 /
      DATA RR226 / 0.33407, 0.33480, 0.33554, 0.33628, 0.33702,
     *   0.33775, 0.33849, 0.33923, 0.33997, 0.34071,
     *   0.34145, 0.34219, 0.34293, 0.34367, 0.34442,
     *   0.34516, 0.34590, 0.34665, 0.34739, 0.34814,
     *   0.34888, 0.34963, 0.35037, 0.35112, 0.35187,
     *   0.35262, 0.35337, 0.35412, 0.35487, 0.35562,
     *   0.35637, 0.35712, 0.35787, 0.35862, 0.35938,
     *   0.36013, 0.36088, 0.36164, 0.36240, 0.36315,
     *   0.36391, 0.36467, 0.36542, 0.36618, 0.36694,
     *   0.36770, 0.36846, 0.36922, 0.36998, 0.37074,
     *   0.37151, 0.37227, 0.37303, 0.37380, 0.37456,
     *   0.37533, 0.37610, 0.37686, 0.37763, 0.37840,
     *   0.37917, 0.37994, 0.38071, 0.38148, 0.38225,
     *   0.38302, 0.38379, 0.38457, 0.38534, 0.38611,
     *   0.38689, 0.38766, 0.38844, 0.38922, 0.39000,
     *   0.39077, 0.39155, 0.39233, 0.39311, 0.39389,
     *   0.39468, 0.39546, 0.39624, 0.39703, 0.39781,
     *   0.39860, 0.39938, 0.40017, 0.40096, 0.40174,
     *   0.40253, 0.40332, 0.40411, 0.40490, 0.40570,
     *   0.40649, 0.40728, 0.40808, 0.40887, 0.40967 /
      DATA RR227 / 0.41046, 0.41126, 0.41206, 0.41285, 0.41365,
     *   0.41445, 0.41525, 0.41606, 0.41686, 0.41766,
     *   0.41846, 0.41927, 0.42007, 0.42088, 0.42169,
     *   0.42250, 0.42330, 0.42411, 0.42492, 0.42573,
     *   0.42655, 0.42736, 0.42817, 0.42899, 0.42980,
     *   0.43062, 0.43143, 0.43225, 0.43307, 0.43389,
     *   0.43471, 0.43553, 0.43635, 0.43718, 0.43800,
     *   0.43882, 0.43965, 0.44048, 0.44130, 0.44213,
     *   0.44296, 0.44379, 0.44462, 0.44545, 0.44629,
     *   0.44712, 0.44795, 0.44879, 0.44962, 0.45046,
     *   0.45130, 0.45214, 0.45298, 0.45382, 0.45466,
     *   0.45551, 0.45635, 0.45719, 0.45804, 0.45889,
     *   0.45974, 0.46058, 0.46143, 0.46229, 0.46314,
     *   0.46399, 0.46484, 0.46570, 0.46655, 0.46741,
     *   0.46827, 0.46913, 0.46999, 0.47085, 0.47171,
     *   0.47258, 0.47344, 0.47431, 0.47517, 0.47604,
     *   0.47691, 0.47778, 0.47865, 0.47952, 0.48040,
     *   0.48127, 0.48215, 0.48302, 0.48390, 0.48478,
     *   0.48566, 0.48654, 0.48742, 0.48831, 0.48919,
     *   0.49008, 0.49096, 0.49185, 0.49274, 0.49363 /
      DATA RR228 / 0.49453, 0.49542, 0.49631, 0.49721, 0.49811,
     *   0.49900, 0.49990, 0.50080, 0.50171, 0.50261,
     *   0.50351, 0.50442, 0.50533, 0.50624, 0.50715,
     *   0.50806, 0.50897, 0.50988, 0.51080, 0.51172,
     *   0.51263, 0.51355, 0.51447, 0.51540, 0.51632,
     *   0.51725, 0.51817, 0.51910, 0.52003, 0.52096,
     *   0.52189, 0.52283, 0.52376, 0.52470, 0.52564,
     *   0.52658, 0.52752, 0.52846, 0.52940, 0.53035,
     *   0.53130, 0.53225, 0.53320, 0.53415, 0.53510,
     *   0.53606, 0.53701, 0.53797, 0.53893, 0.53989,
     *   0.54086, 0.54182, 0.54279, 0.54376, 0.54473,
     *   0.54570, 0.54667, 0.54765, 0.54862, 0.54960,
     *   0.55058, 0.55156, 0.55255, 0.55353, 0.55452,
     *   0.55551, 0.55650, 0.55750, 0.55849, 0.55949,
     *   0.56049, 0.56149, 0.56249, 0.56349, 0.56450,
     *   0.56551, 0.56652, 0.56753, 0.56855, 0.56956,
     *   0.57058, 0.57160, 0.57262, 0.57365, 0.57467,
     *   0.57570, 0.57673, 0.57777, 0.57880, 0.57984,
     *   0.58088, 0.58192, 0.58296, 0.58401, 0.58506,
     *   0.58611, 0.58716, 0.58822, 0.58927, 0.59033 /
      DATA RR229 / 0.59140, 0.59246, 0.59353, 0.59460, 0.59567,
     *   0.59674, 0.59782, 0.59890, 0.59998, 0.60107,
     *   0.60215, 0.60324, 0.60433, 0.60543, 0.60653,
     *   0.60763, 0.60873, 0.60983, 0.61094, 0.61205,
     *   0.61317, 0.61428, 0.61540, 0.61652, 0.61765,
     *   0.61878, 0.61991, 0.62104, 0.62218, 0.62332,
     *   0.62446, 0.62561, 0.62676, 0.62791, 0.62906,
     *   0.63022, 0.63139, 0.63255, 0.63372, 0.63489,
     *   0.63607, 0.63724, 0.63843, 0.63961, 0.64080,
     *   0.64199, 0.64319, 0.64439, 0.64559, 0.64680,
     *   0.64801, 0.64922, 0.65044, 0.65166, 0.65289,
     *   0.65412, 0.65535, 0.65659, 0.65783, 0.65907,
     *   0.66032, 0.66158, 0.66283, 0.66410, 0.66536,
     *   0.66663, 0.66791, 0.66919, 0.67047, 0.67176,
     *   0.67306, 0.67435, 0.67566, 0.67696, 0.67828,
     *   0.67959, 0.68092, 0.68224, 0.68358, 0.68492,
     *   0.68626, 0.68761, 0.68896, 0.69032, 0.69168,
     *   0.69305, 0.69443, 0.69581, 0.69720, 0.69859,
     *   0.69999, 0.70140, 0.70281, 0.70423, 0.70565,
     *   0.70708, 0.70852, 0.70996, 0.71141, 0.71287 /
      DATA RR2210 / 0.71433, 0.71580, 0.71728, 0.71877, 0.72026,
     *   0.72176, 0.72327, 0.72478, 0.72631, 0.72784,
     *   0.72938, 0.73092, 0.73248, 0.73405, 0.73562,
     *   0.73720, 0.73879, 0.74039, 0.74200, 0.74362,
     *   0.74525, 0.74689, 0.74854, 0.75020, 0.75187,
     *   0.75355, 0.75524, 0.75695, 0.75866, 0.76039,
     *   0.76212, 0.76388, 0.76564, 0.76741, 0.76920,
     *   0.77100, 0.77282, 0.77465, 0.77649, 0.77835,
     *   0.78022, 0.78211, 0.78402, 0.78594, 0.78788,
     *   0.78983, 0.79181, 0.79380, 0.79581, 0.79783,
     *   0.79988, 0.80195, 0.80404, 0.80616, 0.80829,
     *   0.81045, 0.81263, 0.81484, 0.81707, 0.81933,
     *   0.82162, 0.82394, 0.82628, 0.82866, 0.83107,
     *   0.83352, 0.83600, 0.83851, 0.84107, 0.84367,
     *   0.84631, 0.84899, 0.85173, 0.85451, 0.85735,
     *   0.86024, 0.86320, 0.86622, 0.86930, 0.87246,
     *   0.87570, 0.87903, 0.88245, 0.88597, 0.88960,
     *   0.89335, 0.89724, 0.90128, 0.90549, 0.90989,
     *   0.91452, 0.91942, 0.92463, 0.93023, 0.93631,
     *   0.94304, 0.95068, 0.95973, 0.97153, 0.99973 /
      DATA RR241 / 0.00088, 0.00177, 0.00265, 0.00353, 0.00441,
     *   0.00530, 0.00618, 0.00706, 0.00794, 0.00883,
     *   0.00971, 0.01059, 0.01147, 0.01236, 0.01324,
     *   0.01412, 0.01500, 0.01589, 0.01677, 0.01765,
     *   0.01853, 0.01942, 0.02030, 0.02118, 0.02206,
     *   0.02295, 0.02383, 0.02471, 0.02559, 0.02648,
     *   0.02736, 0.02824, 0.02913, 0.03001, 0.03089,
     *   0.03177, 0.03266, 0.03354, 0.03442, 0.03530,
     *   0.03619, 0.03707, 0.03795, 0.03884, 0.03972,
     *   0.04060, 0.04148, 0.04237, 0.04325, 0.04413,
     *   0.04502, 0.04590, 0.04678, 0.04767, 0.04855,
     *   0.04943, 0.05032, 0.05120, 0.05208, 0.05296,
     *   0.05385, 0.05473, 0.05561, 0.05650, 0.05738,
     *   0.05826, 0.05915, 0.06003, 0.06091, 0.06180,
     *   0.06268, 0.06356, 0.06445, 0.06533, 0.06622,
     *   0.06710, 0.06798, 0.06887, 0.06975, 0.07063,
     *   0.07152, 0.07240, 0.07329, 0.07417, 0.07505,
     *   0.07594, 0.07682, 0.07770, 0.07859, 0.07947,
     *   0.08036, 0.08124, 0.08213, 0.08301, 0.08389,
     *   0.08478, 0.08566, 0.08655, 0.08743, 0.08832 /
      DATA RR242 / 0.08920, 0.09008, 0.09097, 0.09185, 0.09274,
     *   0.09362, 0.09451, 0.09539, 0.09628, 0.09716,
     *   0.09805, 0.09893, 0.09982, 0.10070, 0.10159,
     *   0.10247, 0.10336, 0.10424, 0.10513, 0.10601,
     *   0.10690, 0.10778, 0.10867, 0.10955, 0.11044,
     *   0.11132, 0.11221, 0.11310, 0.11398, 0.11487,
     *   0.11575, 0.11664, 0.11752, 0.11841, 0.11930,
     *   0.12018, 0.12107, 0.12195, 0.12284, 0.12373,
     *   0.12461, 0.12550, 0.12639, 0.12727, 0.12816,
     *   0.12905, 0.12993, 0.13082, 0.13171, 0.13259,
     *   0.13348, 0.13437, 0.13525, 0.13614, 0.13703,
     *   0.13791, 0.13880, 0.13969, 0.14058, 0.14146,
     *   0.14235, 0.14324, 0.14413, 0.14501, 0.14590,
     *   0.14679, 0.14768, 0.14856, 0.14945, 0.15034,
     *   0.15123, 0.15212, 0.15300, 0.15389, 0.15478,
     *   0.15567, 0.15656, 0.15745, 0.15833, 0.15922,
     *   0.16011, 0.16100, 0.16189, 0.16278, 0.16367,
     *   0.16456, 0.16545, 0.16634, 0.16722, 0.16811,
     *   0.16900, 0.16989, 0.17078, 0.17167, 0.17256,
     *   0.17345, 0.17434, 0.17523, 0.17612, 0.17701 /
      DATA RR243 / 0.17790, 0.17879, 0.17968, 0.18057, 0.18146,
     *   0.18235, 0.18324, 0.18414, 0.18503, 0.18592,
     *   0.18681, 0.18770, 0.18859, 0.18948, 0.19037,
     *   0.19126, 0.19216, 0.19305, 0.19394, 0.19483,
     *   0.19572, 0.19661, 0.19751, 0.19840, 0.19929,
     *   0.20018, 0.20107, 0.20197, 0.20286, 0.20375,
     *   0.20464, 0.20554, 0.20643, 0.20732, 0.20822,
     *   0.20911, 0.21000, 0.21090, 0.21179, 0.21268,
     *   0.21358, 0.21447, 0.21536, 0.21626, 0.21715,
     *   0.21805, 0.21894, 0.21983, 0.22073, 0.22162,
     *   0.22252, 0.22341, 0.22431, 0.22520, 0.22610,
     *   0.22699, 0.22789, 0.22878, 0.22968, 0.23057,
     *   0.23147, 0.23236, 0.23326, 0.23415, 0.23505,
     *   0.23595, 0.23684, 0.23774, 0.23863, 0.23953,
     *   0.24043, 0.24132, 0.24222, 0.24312, 0.24401,
     *   0.24491, 0.24581, 0.24671, 0.24760, 0.24850,
     *   0.24940, 0.25030, 0.25119, 0.25209, 0.25299,
     *   0.25389, 0.25479, 0.25568, 0.25658, 0.25748,
     *   0.25838, 0.25928, 0.26018, 0.26108, 0.26197,
     *   0.26287, 0.26377, 0.26467, 0.26557, 0.26647 /
      DATA RR244 / 0.26737, 0.26827, 0.26917, 0.27007, 0.27097,
     *   0.27187, 0.27277, 0.27367, 0.27457, 0.27547,
     *   0.27638, 0.27728, 0.27818, 0.27908, 0.27998,
     *   0.28088, 0.28178, 0.28269, 0.28359, 0.28449,
     *   0.28539, 0.28629, 0.28720, 0.28810, 0.28900,
     *   0.28990, 0.29081, 0.29171, 0.29261, 0.29352,
     *   0.29442, 0.29532, 0.29623, 0.29713, 0.29803,
     *   0.29894, 0.29984, 0.30075, 0.30165, 0.30256,
     *   0.30346, 0.30437, 0.30527, 0.30618, 0.30708,
     *   0.30799, 0.30889, 0.30980, 0.31070, 0.31161,
     *   0.31251, 0.31342, 0.31433, 0.31523, 0.31614,
     *   0.31705, 0.31795, 0.31886, 0.31977, 0.32067,
     *   0.32158, 0.32249, 0.32340, 0.32430, 0.32521,
     *   0.32612, 0.32703, 0.32794, 0.32885, 0.32975,
     *   0.33066, 0.33157, 0.33248, 0.33339, 0.33430,
     *   0.33521, 0.33612, 0.33703, 0.33794, 0.33885,
     *   0.33976, 0.34067, 0.34158, 0.34249, 0.34340,
     *   0.34431, 0.34522, 0.34613, 0.34704, 0.34796,
     *   0.34887, 0.34978, 0.35069, 0.35160, 0.35252,
     *   0.35343, 0.35434, 0.35525, 0.35617, 0.35708 /
      DATA RR245 / 0.35799, 0.35891, 0.35982, 0.36073, 0.36165,
     *   0.36256, 0.36347, 0.36439, 0.36530, 0.36622,
     *   0.36713, 0.36805, 0.36896, 0.36988, 0.37079,
     *   0.37171, 0.37262, 0.37354, 0.37445, 0.37537,
     *   0.37629, 0.37720, 0.37812, 0.37904, 0.37995,
     *   0.38087, 0.38179, 0.38270, 0.38362, 0.38454,
     *   0.38546, 0.38638, 0.38729, 0.38821, 0.38913,
     *   0.39005, 0.39097, 0.39189, 0.39281, 0.39372,
     *   0.39464, 0.39556, 0.39648, 0.39740, 0.39832,
     *   0.39924, 0.40016, 0.40109, 0.40201, 0.40293,
     *   0.40385, 0.40477, 0.40569, 0.40661, 0.40753,
     *   0.40846, 0.40938, 0.41030, 0.41122, 0.41215,
     *   0.41307, 0.41399, 0.41491, 0.41584, 0.41676,
     *   0.41768, 0.41861, 0.41953, 0.42046, 0.42138,
     *   0.42231, 0.42323, 0.42416, 0.42508, 0.42601,
     *   0.42693, 0.42786, 0.42878, 0.42971, 0.43063,
     *   0.43156, 0.43249, 0.43341, 0.43434, 0.43527,
     *   0.43619, 0.43712, 0.43805, 0.43898, 0.43990,
     *   0.44083, 0.44176, 0.44269, 0.44362, 0.44455,
     *   0.44548, 0.44640, 0.44733, 0.44826, 0.44919 /
      DATA RR246 / 0.45012, 0.45105, 0.45198, 0.45291, 0.45384,
     *   0.45478, 0.45571, 0.45664, 0.45757, 0.45850,
     *   0.45943, 0.46036, 0.46130, 0.46223, 0.46316,
     *   0.46409, 0.46503, 0.46596, 0.46689, 0.46782,
     *   0.46876, 0.46969, 0.47063, 0.47156, 0.47249,
     *   0.47343, 0.47436, 0.47530, 0.47623, 0.47717,
     *   0.47810, 0.47904, 0.47997, 0.48091, 0.48185,
     *   0.48278, 0.48372, 0.48466, 0.48559, 0.48653,
     *   0.48747, 0.48840, 0.48934, 0.49028, 0.49122,
     *   0.49216, 0.49309, 0.49403, 0.49497, 0.49591,
     *   0.49685, 0.49779, 0.49873, 0.49967, 0.50061,
     *   0.50155, 0.50249, 0.50343, 0.50437, 0.50531,
     *   0.50625, 0.50719, 0.50813, 0.50908, 0.51002,
     *   0.51096, 0.51190, 0.51284, 0.51379, 0.51473,
     *   0.51567, 0.51661, 0.51756, 0.51850, 0.51945,
     *   0.52039, 0.52133, 0.52228, 0.52322, 0.52417,
     *   0.52511, 0.52606, 0.52700, 0.52795, 0.52889,
     *   0.52984, 0.53078, 0.53173, 0.53268, 0.53362,
     *   0.53457, 0.53552, 0.53646, 0.53741, 0.53836,
     *   0.53931, 0.54025, 0.54120, 0.54215, 0.54310 /
      DATA RR247 / 0.54405, 0.54500, 0.54594, 0.54689, 0.54784,
     *   0.54879, 0.54974, 0.55069, 0.55164, 0.55259,
     *   0.55354, 0.55449, 0.55544, 0.55640, 0.55735,
     *   0.55830, 0.55925, 0.56020, 0.56115, 0.56211,
     *   0.56306, 0.56401, 0.56496, 0.56592, 0.56687,
     *   0.56782, 0.56878, 0.56973, 0.57068, 0.57164,
     *   0.57259, 0.57354, 0.57450, 0.57545, 0.57641,
     *   0.57736, 0.57832, 0.57927, 0.58023, 0.58119,
     *   0.58214, 0.58310, 0.58405, 0.58501, 0.58597,
     *   0.58692, 0.58788, 0.58884, 0.58980, 0.59075,
     *   0.59171, 0.59267, 0.59363, 0.59458, 0.59554,
     *   0.59650, 0.59746, 0.59842, 0.59938, 0.60034,
     *   0.60130, 0.60226, 0.60322, 0.60418, 0.60514,
     *   0.60610, 0.60706, 0.60802, 0.60898, 0.60994,
     *   0.61090, 0.61186, 0.61282, 0.61378, 0.61474,
     *   0.61571, 0.61667, 0.61763, 0.61859, 0.61955,
     *   0.62052, 0.62148, 0.62244, 0.62341, 0.62437,
     *   0.62533, 0.62630, 0.62726, 0.62822, 0.62919,
     *   0.63015, 0.63111, 0.63208, 0.63304, 0.63401,
     *   0.63497, 0.63594, 0.63690, 0.63787, 0.63883 /
      DATA RR248 / 0.63980, 0.64076, 0.64173, 0.64270, 0.64366,
     *   0.64463, 0.64559, 0.64656, 0.64753, 0.64849,
     *   0.64946, 0.65043, 0.65139, 0.65236, 0.65333,
     *   0.65429, 0.65526, 0.65623, 0.65720, 0.65816,
     *   0.65913, 0.66010, 0.66107, 0.66204, 0.66300,
     *   0.66397, 0.66494, 0.66591, 0.66688, 0.66785,
     *   0.66882, 0.66978, 0.67075, 0.67172, 0.67269,
     *   0.67366, 0.67463, 0.67560, 0.67657, 0.67754,
     *   0.67851, 0.67948, 0.68045, 0.68142, 0.68239,
     *   0.68336, 0.68433, 0.68530, 0.68627, 0.68724,
     *   0.68821, 0.68918, 0.69015, 0.69112, 0.69209,
     *   0.69306, 0.69403, 0.69500, 0.69598, 0.69695,
     *   0.69792, 0.69889, 0.69986, 0.70083, 0.70180,
     *   0.70277, 0.70374, 0.70471, 0.70569, 0.70666,
     *   0.70763, 0.70860, 0.70957, 0.71054, 0.71151,
     *   0.71248, 0.71346, 0.71443, 0.71540, 0.71637,
     *   0.71734, 0.71831, 0.71928, 0.72026, 0.72123,
     *   0.72220, 0.72317, 0.72414, 0.72511, 0.72608,
     *   0.72705, 0.72803, 0.72900, 0.72997, 0.73094,
     *   0.73191, 0.73288, 0.73385, 0.73482, 0.73580 /
      DATA RR249 / 0.73677, 0.73774, 0.73871, 0.73968, 0.74065,
     *   0.74162, 0.74259, 0.74356, 0.74453, 0.74550,
     *   0.74647, 0.74744, 0.74842, 0.74939, 0.75036,
     *   0.75133, 0.75230, 0.75327, 0.75424, 0.75521,
     *   0.75618, 0.75715, 0.75812, 0.75909, 0.76006,
     *   0.76103, 0.76199, 0.76296, 0.76393, 0.76490,
     *   0.76587, 0.76684, 0.76781, 0.76878, 0.76975,
     *   0.77072, 0.77169, 0.77265, 0.77362, 0.77459,
     *   0.77556, 0.77653, 0.77750, 0.77847, 0.77943,
     *   0.78040, 0.78137, 0.78234, 0.78331, 0.78428,
     *   0.78524, 0.78621, 0.78718, 0.78815, 0.78912,
     *   0.79008, 0.79105, 0.79202, 0.79299, 0.79395,
     *   0.79492, 0.79589, 0.79686, 0.79783, 0.79879,
     *   0.79976, 0.80073, 0.80170, 0.80267, 0.80364,
     *   0.80461, 0.80557, 0.80654, 0.80751, 0.80848,
     *   0.80945, 0.81042, 0.81139, 0.81236, 0.81333,
     *   0.81430, 0.81527, 0.81624, 0.81722, 0.81819,
     *   0.81916, 0.82013, 0.82111, 0.82208, 0.82306,
     *   0.82403, 0.82501, 0.82598, 0.82696, 0.82794,
     *   0.82891, 0.82989, 0.83087, 0.83185, 0.83284 /
      DATA RR2410 / 0.83382, 0.83480, 0.83579, 0.83677, 0.83776,
     *   0.83875, 0.83974, 0.84073, 0.84172, 0.84272,
     *   0.84371, 0.84471, 0.84571, 0.84671, 0.84771,
     *   0.84872, 0.84972, 0.85073, 0.85174, 0.85276,
     *   0.85377, 0.85479, 0.85582, 0.85684, 0.85787,
     *   0.85890, 0.85994, 0.86097, 0.86202, 0.86306,
     *   0.86411, 0.86517, 0.86622, 0.86729, 0.86835,
     *   0.86943, 0.87051, 0.87159, 0.87268, 0.87377,
     *   0.87488, 0.87598, 0.87710, 0.87822, 0.87935,
     *   0.88049, 0.88163, 0.88278, 0.88395, 0.88512,
     *   0.88630, 0.88749, 0.88869, 0.88991, 0.89113,
     *   0.89237, 0.89361, 0.89488, 0.89615, 0.89744,
     *   0.89874, 0.90006, 0.90140, 0.90275, 0.90412,
     *   0.90551, 0.90692, 0.90835, 0.90981, 0.91128,
     *   0.91278, 0.91431, 0.91586, 0.91744, 0.91905,
     *   0.92069, 0.92237, 0.92408, 0.92583, 0.92763,
     *   0.92946, 0.93135, 0.93329, 0.93529, 0.93735,
     *   0.93948, 0.94168, 0.94398, 0.94637, 0.94887,
     *   0.95149, 0.95427, 0.95723, 0.96041, 0.96386,
     *   0.96768, 0.97201, 0.97715, 0.98384, 0.99985 /
      DATA RR441 / 0.00088, 0.00177, 0.00265, 0.00353, 0.00441,
     *   0.00530, 0.00618, 0.00706, 0.00794, 0.00883,
     *   0.00971, 0.01059, 0.01147, 0.01236, 0.01324,
     *   0.01412, 0.01500, 0.01589, 0.01677, 0.01765,
     *   0.01853, 0.01942, 0.02030, 0.02118, 0.02206,
     *   0.02295, 0.02383, 0.02471, 0.02559, 0.02648,
     *   0.02736, 0.02824, 0.02912, 0.03001, 0.03089,
     *   0.03177, 0.03265, 0.03354, 0.03442, 0.03530,
     *   0.03619, 0.03707, 0.03795, 0.03883, 0.03972,
     *   0.04060, 0.04148, 0.04236, 0.04325, 0.04413,
     *   0.04501, 0.04589, 0.04678, 0.04766, 0.04854,
     *   0.04943, 0.05031, 0.05119, 0.05207, 0.05296,
     *   0.05384, 0.05472, 0.05561, 0.05649, 0.05737,
     *   0.05825, 0.05914, 0.06002, 0.06090, 0.06179,
     *   0.06267, 0.06355, 0.06443, 0.06532, 0.06620,
     *   0.06708, 0.06797, 0.06885, 0.06973, 0.07062,
     *   0.07150, 0.07238, 0.07326, 0.07415, 0.07503,
     *   0.07591, 0.07680, 0.07768, 0.07856, 0.07945,
     *   0.08033, 0.08121, 0.08210, 0.08298, 0.08386,
     *   0.08475, 0.08563, 0.08651, 0.08740, 0.08828 /
      DATA RR442 / 0.08916, 0.09005, 0.09093, 0.09181, 0.09270,
     *   0.09358, 0.09446, 0.09535, 0.09623, 0.09711,
     *   0.09800, 0.09888, 0.09976, 0.10065, 0.10153,
     *   0.10241, 0.10330, 0.10418, 0.10507, 0.10595,
     *   0.10683, 0.10772, 0.10860, 0.10948, 0.11037,
     *   0.11125, 0.11214, 0.11302, 0.11390, 0.11479,
     *   0.11567, 0.11656, 0.11744, 0.11832, 0.11921,
     *   0.12009, 0.12098, 0.12186, 0.12274, 0.12363,
     *   0.12451, 0.12540, 0.12628, 0.12716, 0.12805,
     *   0.12893, 0.12982, 0.13070, 0.13159, 0.13247,
     *   0.13335, 0.13424, 0.13512, 0.13601, 0.13689,
     *   0.13778, 0.13866, 0.13955, 0.14043, 0.14131,
     *   0.14220, 0.14308, 0.14397, 0.14485, 0.14574,
     *   0.14662, 0.14751, 0.14839, 0.14928, 0.15016,
     *   0.15105, 0.15193, 0.15282, 0.15370, 0.15459,
     *   0.15547, 0.15636, 0.15724, 0.15813, 0.15901,
     *   0.15990, 0.16078, 0.16167, 0.16255, 0.16344,
     *   0.16432, 0.16521, 0.16609, 0.16698, 0.16787,
     *   0.16875, 0.16964, 0.17052, 0.17141, 0.17229,
     *   0.17318, 0.17406, 0.17495, 0.17584, 0.17672 /
      DATA RR443 / 0.17761, 0.17849, 0.17938, 0.18027, 0.18115,
     *   0.18204, 0.18292, 0.18381, 0.18470, 0.18558,
     *   0.18647, 0.18735, 0.18824, 0.18913, 0.19001,
     *   0.19090, 0.19179, 0.19267, 0.19356, 0.19444,
     *   0.19533, 0.19622, 0.19710, 0.19799, 0.19888,
     *   0.19976, 0.20065, 0.20154, 0.20242, 0.20331,
     *   0.20420, 0.20509, 0.20597, 0.20686, 0.20775,
     *   0.20863, 0.20952, 0.21041, 0.21129, 0.21218,
     *   0.21307, 0.21396, 0.21484, 0.21573, 0.21662,
     *   0.21751, 0.21839, 0.21928, 0.22017, 0.22106,
     *   0.22194, 0.22283, 0.22372, 0.22461, 0.22550,
     *   0.22638, 0.22727, 0.22816, 0.22905, 0.22993,
     *   0.23082, 0.23171, 0.23260, 0.23349, 0.23438,
     *   0.23526, 0.23615, 0.23704, 0.23793, 0.23882,
     *   0.23971, 0.24059, 0.24148, 0.24237, 0.24326,
     *   0.24415, 0.24504, 0.24593, 0.24682, 0.24771,
     *   0.24859, 0.24948, 0.25037, 0.25126, 0.25215,
     *   0.25304, 0.25393, 0.25482, 0.25571, 0.25660,
     *   0.25749, 0.25838, 0.25927, 0.26015, 0.26104,
     *   0.26193, 0.26282, 0.26371, 0.26460, 0.26549 /
      DATA RR444 / 0.26638, 0.26727, 0.26816, 0.26905, 0.26994,
     *   0.27083, 0.27172, 0.27261, 0.27350, 0.27440,
     *   0.27529, 0.27618, 0.27707, 0.27796, 0.27885,
     *   0.27974, 0.28063, 0.28152, 0.28241, 0.28330,
     *   0.28419, 0.28508, 0.28597, 0.28687, 0.28776,
     *   0.28865, 0.28954, 0.29043, 0.29132, 0.29221,
     *   0.29311, 0.29400, 0.29489, 0.29578, 0.29667,
     *   0.29756, 0.29846, 0.29935, 0.30024, 0.30113,
     *   0.30202, 0.30292, 0.30381, 0.30470, 0.30559,
     *   0.30648, 0.30738, 0.30827, 0.30916, 0.31005,
     *   0.31095, 0.31184, 0.31273, 0.31362, 0.31452,
     *   0.31541, 0.31630, 0.31720, 0.31809, 0.31898,
     *   0.31988, 0.32077, 0.32166, 0.32256, 0.32345,
     *   0.32434, 0.32524, 0.32613, 0.32702, 0.32792,
     *   0.32881, 0.32971, 0.33060, 0.33149, 0.33239,
     *   0.33328, 0.33418, 0.33507, 0.33596, 0.33686,
     *   0.33775, 0.33865, 0.33954, 0.34044, 0.34133,
     *   0.34223, 0.34312, 0.34402, 0.34491, 0.34581,
     *   0.34670, 0.34760, 0.34849, 0.34939, 0.35028,
     *   0.35118, 0.35207, 0.35297, 0.35386, 0.35476 /
      DATA RR445 / 0.35566, 0.35655, 0.35745, 0.35834, 0.35924,
     *   0.36014, 0.36103, 0.36193, 0.36282, 0.36372,
     *   0.36462, 0.36551, 0.36641, 0.36731, 0.36820,
     *   0.36910, 0.37000, 0.37089, 0.37179, 0.37269,
     *   0.37358, 0.37448, 0.37538, 0.37628, 0.37717,
     *   0.37807, 0.37897, 0.37987, 0.38076, 0.38166,
     *   0.38256, 0.38346, 0.38436, 0.38525, 0.38615,
     *   0.38705, 0.38795, 0.38885, 0.38974, 0.39064,
     *   0.39154, 0.39244, 0.39334, 0.39424, 0.39514,
     *   0.39604, 0.39693, 0.39783, 0.39873, 0.39963,
     *   0.40053, 0.40143, 0.40233, 0.40323, 0.40413,
     *   0.40503, 0.40593, 0.40683, 0.40773, 0.40863,
     *   0.40953, 0.41043, 0.41133, 0.41223, 0.41313,
     *   0.41403, 0.41493, 0.41583, 0.41673, 0.41763,
     *   0.41853, 0.41943, 0.42034, 0.42124, 0.42214,
     *   0.42304, 0.42394, 0.42484, 0.42574, 0.42665,
     *   0.42755, 0.42845, 0.42935, 0.43025, 0.43115,
     *   0.43206, 0.43296, 0.43386, 0.43476, 0.43567,
     *   0.43657, 0.43747, 0.43837, 0.43928, 0.44018,
     *   0.44108, 0.44199, 0.44289, 0.44379, 0.44470 /
      DATA RR446 / 0.44560, 0.44650, 0.44741, 0.44831, 0.44921,
     *   0.45012, 0.45102, 0.45192, 0.45283, 0.45373,
     *   0.45464, 0.45554, 0.45645, 0.45735, 0.45825,
     *   0.45916, 0.46006, 0.46097, 0.46187, 0.46278,
     *   0.46368, 0.46459, 0.46549, 0.46640, 0.46731,
     *   0.46821, 0.46912, 0.47002, 0.47093, 0.47183,
     *   0.47274, 0.47365, 0.47455, 0.47546, 0.47637,
     *   0.47727, 0.47818, 0.47909, 0.47999, 0.48090,
     *   0.48181, 0.48271, 0.48362, 0.48453, 0.48544,
     *   0.48634, 0.48725, 0.48816, 0.48907, 0.48997,
     *   0.49088, 0.49179, 0.49270, 0.49361, 0.49452,
     *   0.49542, 0.49633, 0.49724, 0.49815, 0.49906,
     *   0.49997, 0.50088, 0.50179, 0.50269, 0.50360,
     *   0.50451, 0.50542, 0.50633, 0.50724, 0.50815,
     *   0.50906, 0.50997, 0.51088, 0.51179, 0.51270,
     *   0.51361, 0.51453, 0.51544, 0.51635, 0.51726,
     *   0.51817, 0.51908, 0.51999, 0.52090, 0.52181,
     *   0.52273, 0.52364, 0.52455, 0.52546, 0.52637,
     *   0.52729, 0.52820, 0.52911, 0.53002, 0.53093,
     *   0.53185, 0.53276, 0.53367, 0.53459, 0.53550 /
      DATA RR447 / 0.53641, 0.53733, 0.53824, 0.53915, 0.54007,
     *   0.54098, 0.54189, 0.54281, 0.54372, 0.54464,
     *   0.54555, 0.54647, 0.54738, 0.54830, 0.54921,
     *   0.55013, 0.55104, 0.55196, 0.55287, 0.55379,
     *   0.55470, 0.55562, 0.55653, 0.55745, 0.55837,
     *   0.55928, 0.56020, 0.56111, 0.56203, 0.56295,
     *   0.56386, 0.56478, 0.56570, 0.56662, 0.56753,
     *   0.56845, 0.56937, 0.57029, 0.57120, 0.57212,
     *   0.57304, 0.57396, 0.57488, 0.57579, 0.57671,
     *   0.57763, 0.57855, 0.57947, 0.58039, 0.58131,
     *   0.58223, 0.58315, 0.58407, 0.58499, 0.58591,
     *   0.58683, 0.58775, 0.58867, 0.58959, 0.59051,
     *   0.59143, 0.59235, 0.59327, 0.59419, 0.59511,
     *   0.59603, 0.59695, 0.59788, 0.59880, 0.59972,
     *   0.60064, 0.60156, 0.60249, 0.60341, 0.60433,
     *   0.60525, 0.60618, 0.60710, 0.60802, 0.60895,
     *   0.60987, 0.61079, 0.61172, 0.61264, 0.61357,
     *   0.61449, 0.61542, 0.61634, 0.61726, 0.61819,
     *   0.61911, 0.62004, 0.62097, 0.62189, 0.62282,
     *   0.62374, 0.62467, 0.62559, 0.62652, 0.62745 /
      DATA RR448 / 0.62837, 0.62930, 0.63023, 0.63115, 0.63208,
     *   0.63301, 0.63394, 0.63486, 0.63579, 0.63672,
     *   0.63765, 0.63858, 0.63951, 0.64043, 0.64136,
     *   0.64229, 0.64322, 0.64415, 0.64508, 0.64601,
     *   0.64694, 0.64787, 0.64880, 0.64973, 0.65066,
     *   0.65159, 0.65252, 0.65345, 0.65439, 0.65532,
     *   0.65625, 0.65718, 0.65811, 0.65905, 0.65998,
     *   0.66091, 0.66184, 0.66278, 0.66371, 0.66464,
     *   0.66558, 0.66651, 0.66745, 0.66838, 0.66931,
     *   0.67025, 0.67118, 0.67212, 0.67305, 0.67399,
     *   0.67492, 0.67586, 0.67680, 0.67773, 0.67867,
     *   0.67961, 0.68054, 0.68148, 0.68242, 0.68335,
     *   0.68429, 0.68523, 0.68617, 0.68711, 0.68805,
     *   0.68898, 0.68992, 0.69086, 0.69180, 0.69274,
     *   0.69368, 0.69462, 0.69556, 0.69650, 0.69744,
     *   0.69838, 0.69933, 0.70027, 0.70121, 0.70215,
     *   0.70309, 0.70404, 0.70498, 0.70592, 0.70687,
     *   0.70781, 0.70875, 0.70970, 0.71064, 0.71159,
     *   0.71253, 0.71348, 0.71442, 0.71537, 0.71631,
     *   0.71726, 0.71821, 0.71915, 0.72010, 0.72105 /
      DATA RR449 / 0.72200, 0.72295, 0.72389, 0.72484, 0.72579,
     *   0.72674, 0.72769, 0.72864, 0.72959, 0.73054,
     *   0.73149, 0.73244, 0.73340, 0.73435, 0.73530,
     *   0.73625, 0.73720, 0.73816, 0.73911, 0.74007,
     *   0.74102, 0.74198, 0.74293, 0.74389, 0.74484,
     *   0.74580, 0.74676, 0.74771, 0.74867, 0.74963,
     *   0.75059, 0.75154, 0.75250, 0.75346, 0.75442,
     *   0.75538, 0.75635, 0.75731, 0.75827, 0.75923,
     *   0.76019, 0.76116, 0.76212, 0.76309, 0.76405,
     *   0.76502, 0.76598, 0.76695, 0.76792, 0.76888,
     *   0.76985, 0.77082, 0.77179, 0.77276, 0.77373,
     *   0.77470, 0.77568, 0.77665, 0.77762, 0.77860,
     *   0.77957, 0.78055, 0.78152, 0.78250, 0.78348,
     *   0.78446, 0.78544, 0.78642, 0.78740, 0.78838,
     *   0.78936, 0.79035, 0.79133, 0.79232, 0.79330,
     *   0.79429, 0.79528, 0.79627, 0.79726, 0.79825,
     *   0.79924, 0.80024, 0.80123, 0.80223, 0.80323,
     *   0.80423, 0.80523, 0.80623, 0.80723, 0.80824,
     *   0.80924, 0.81025, 0.81126, 0.81227, 0.81328,
     *   0.81429, 0.81531, 0.81633, 0.81735, 0.81837 /
      DATA RR4410 / 0.81939, 0.82042, 0.82145, 0.82247, 0.82351,
     *   0.82454, 0.82558, 0.82662, 0.82766, 0.82870,
     *   0.82975, 0.83080, 0.83185, 0.83291, 0.83396,
     *   0.83503, 0.83609, 0.83716, 0.83823, 0.83931,
     *   0.84038, 0.84147, 0.84255, 0.84365, 0.84474,
     *   0.84584, 0.84694, 0.84805, 0.84917, 0.85029,
     *   0.85141, 0.85254, 0.85368, 0.85482, 0.85597,
     *   0.85712, 0.85828, 0.85945, 0.86062, 0.86180,
     *   0.86299, 0.86419, 0.86540, 0.86661, 0.86783,
     *   0.86906, 0.87031, 0.87156, 0.87282, 0.87409,
     *   0.87537, 0.87667, 0.87798, 0.87930, 0.88063,
     *   0.88197, 0.88333, 0.88471, 0.88610, 0.88751,
     *   0.88893, 0.89037, 0.89183, 0.89331, 0.89481,
     *   0.89632, 0.89787, 0.89943, 0.90102, 0.90263,
     *   0.90427, 0.90594, 0.90764, 0.90937, 0.91113,
     *   0.91293, 0.91477, 0.91664, 0.91856, 0.92053,
     *   0.92254, 0.92461, 0.92674, 0.92893, 0.93119,
     *   0.93352, 0.93594, 0.93846, 0.94108, 0.94382,
     *   0.94671, 0.94976, 0.95300, 0.95649, 0.96028,
     *   0.96448, 0.96924, 0.97488, 0.98224, 0.99983 /
C-----------------------------------------------------------------------
      ROSTEP = 1.0 / KKMAX
C                                       RORO is an array of analog
C                                       correlation relevant to digital
      DO 20 I = 1, KKMAX
         RORO (I) = ROSTEP * I
 20      CONTINUE
C                                       select the table corresponded
C                                       to the pair of digitizers
      IF (INDDIG.EQ.22) THEN
         DO 40 I = 1, 100
            RR(I)     = RR221(I)
            RR(I+100) = RR222(I)
            RR(I+200) = RR223(I)
            RR(I+300) = RR224(I)
            RR(I+400) = RR225(I)
            RR(I+500) = RR226(I)
            RR(I+600) = RR227(I)
            RR(I+700) = RR228(I)
            RR(I+800) = RR229(I)
            RR(I+900) = RR2210(I)
 40         CONTINUE
         END IF
      IF (INDDIG.EQ.24) THEN
         DO 60 I = 1, 100
            RR(I)     = RR241(I)
            RR(I+100) = RR242(I)
            RR(I+200) = RR243(I)
            RR(I+300) = RR244(I)
            RR(I+400) = RR245(I)
            RR(I+500) = RR246(I)
            RR(I+600) = RR247(I)
            RR(I+700) = RR248(I)
            RR(I+800) = RR249(I)
            RR(I+900) = RR2410(I)
 60         CONTINUE
         END IF
      IF (INDDIG.EQ.44) THEN
         DO 80 I = 1, 100
            RR(I)     = RR441(I)
            RR(I+100) = RR442(I)
            RR(I+200) = RR443(I)
            RR(I+300) = RR444(I)
            RR(I+400) = RR445(I)
            RR(I+500) = RR446(I)
            RR(I+600) = RR447(I)
            RR(I+700) = RR448(I)
            RR(I+800) = RR449(I)
            RR(I+900) = RR4410(I)
 80         CONTINUE
         END IF
C
      RDIGM = ABS(RDIG)
      IF ((RDIGM.LE.RMIN) .OR. (RDIGM.GE.RMAX)) THEN
C                                       miss correction if RDIGM
C                                       is out of range
         RO = RDIGM
      ELSE
         KMIN = 1
         KMAX = KKMAX
 100     CONTINUE
         KMED = (KMIN + KMAX) / 2
         IF (RDIGM.GT.RR(KMED)) THEN
            KMIN = KMED
         ELSE
            KMAX = KMED
            END IF
            IF ((KMAX - KMIN).GT.1) GO TO 100
         RO = RORO(KMIN) + (RORO(KMAX) - RORO(KMIN)) * (RDIGM -
     *      RR(KMIN))/(RR(KMAX) - RR(KMIN))
         END IF
      IF (RDIG.LT.0.0) RO = -RO
 999  RETURN
      END
      SUBROUTINE FILLFQ (DISK, CNO, CATFQ, LUN, FREQFQ, FQNUMB, IERR)
C-----------------------------------------------------------------------
C   Routine to look up frequency info for all freqids
C   Inputs:
C      DISK     I       Disk number
C      CNO      I       Catalog slot number
C      CATFQ   I(256)  Catalog header
C      LUN      I       LUN to use. (e.g. 25)
C   Output:
C      FREQFQ   D(*)    Frequency of IF1 for each FQID (Hz)
C      FQNUMB   I       # FQIDs in this FQ table
C   Output:
C      IERR     I       Return code. 0=OK, else failed.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DISK, CNO, CATFQ(256), LUN, SIDFQ(MAXIF), IERR
      DOUBLE PRECISION FREQFQ(*)
      REAL      TBWFQ(MAXIF), CHBWFQ(MAXIF)
C
      INTEGER   NUMIFQ, JERR, RNOFQ, KOLSFQ(MAXFQC), NUMVFQ(MAXFQC),
     *   FQNUMB, IFQN, I, FQID, MLOCF
      LOGICAL   TABLE, FQEXIS, FITASC
      DOUBLE PRECISION FFREQ(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'TABLES.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      IERR = 0
      CALL COPY (256, CATFQ, CATIEQ)
      CALL AXEFND (8, 'FREQ    ', CATIEQ(KIDIM), CATHEQ(KHCTP), MLOCF,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Does table exist
      CALL ISTAB ('FQ', DISK, CNO, 1, LUN, TABUF1, TABLE, FQEXIS,
     *   FITASC, IERR)
      IF (.NOT.FQEXIS) THEN
         MSGTXT = 'FILLFQ: NO FQ TABLE FOUND'
         IERR = 1
         GO TO 990
         END IF
C                                       Open table and fill arrays
      CALL FQINI ('READ', TABUF1, DISK, CNO, 1, CATFQ, LUN, RNOFQ,
     *   KOLSFQ, NUMVFQ, NUMIFQ, IERR)
      IF (IERR.NE.0) GO TO 999
      FQNUMB = TABUF1(5)
C                                       Load up FREQFQ array
      DO 100 I = 1,FQNUMB
         IFQN = I
         CALL TABFQ ('READ', TABUF1, IFQN, KOLSFQ, NUMVFQ, NUMIFQ, FQID,
     *      FFREQ, CHBWFQ, TBWFQ, SIDFQ, BNDCOD, IERR)
         IF (IERR.NE.0) GO TO 999
         FREQFQ(FQID) = CATDEQ(KDCRV+MLOCF) + FFREQ(1)
 100     CONTINUE
C
      CALL TABIO ('CLOS', 0, RNOFQ, TABUF1, TABUF1, JERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FILLFQ: ERROR ',I3,' DETERMINING FREQ. AXIS')
      END
      SUBROUTINE BT2BP (DISK, CNO, LUN, CATIN, IERR)
C-----------------------------------------------------------------------
C   Routine which generates a BP table from a BT table.
C   Inputs:
C      DISK    I        Volume on which data reside
C      CNO     I        Catalogue number of data
C      LUN     I        Main lun to use.
C      CATIN   I(256)   Catalogue header.
C   Outputs:
C      IERR    I        Error code, 0 => OK; anything else => problem
C   Note:  Routine uses lun 46 during its operation, that lun is freed
C   upon exit.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER    DISK, CNO, LUN, CATIN(256), IERR
C
      INTEGER   NUMBAT, I, IVER, NENTRY, IENT, LUN2, ANTFNC, SRCFNC,
     *   FQIFNC, OUTFQI, J
      REAL      RTIM
      LOGICAL MATCH, WANSRC, WANTIM
C                                       Declarations for BPINI
      INTEGER   BPKOLS(MAXBPC), BPNUMV(MAXBPC), IBPRNO, BPANT, BPPOL,
     *   BPIF, BPFRQ, BPBCHN, NUMSHF
      REAL      LOWSHF, DELSHF
      CHARACTER LBPTYP*8
C                                       Declarations for TABBP
      INCLUDE 'PFITLD.INC'
      INTEGER   BPREFT(2)
      REAL      BPWT(2*MAXIF), BNDPAS(2,LMXCIF)
      DOUBLE PRECISION IFFREQ(MAXIF), REFF
      INCLUDE 'DATSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBTV.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'SCRTCH.INC'
      EQUIVALENCE (TRECR, BNDPAS)
C-----------------------------------------------------------------------
      CALL COPY (256, CATIN, CATIEQ)
      REFF = CATDEQ(KDCRV+JLOCF)
      LUN2 = 46
C                                       Determine # tables
      NUMBAT = 1
      CALL FNDEXT ('BT', CATIEQ, NUMBAT)
      IF (NUMBAT.EQ.0) GO TO 999
C                                       Loop over tables
C                                       write BP table
      DO 100 I = 1,NUMBAT
         IVER = I
C                                       Open BT table
         CALL BTINI ('READ', TABUF2, DISK, CNO, IVER, CATIEQ, LUN, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, IVER
            GO TO 990
            END IF
         NENTRY = TABUF2(5)
C                                       Insert variables into
C                                       catalogue header if they
C                                       don't already exist, if
C                                       they do then just compare them.
         IF (I.EQ.1) THEN
            REFOFF = 0.D0
            CALL CATMAT (CATIEQ, MATCH)
            IF (.NOT.MATCH) THEN
               WRITE (MSGTXT,1040) 'BT'
               CALL MSGWRT (6)
               END IF
            END IF
C                                       Copy variables
         BPANT = NOANT
         BPPOL = NOPOLZ
         BPFRQ = NOBTCH
         BPBCHN = CHN1
         BPIF = NOBAND
         LBPTYP = ' '
C                                       Create and open BP table
         CALL BPINI ('WRIT', TABUF1, DISK, CNO, IVER, CATIEQ, LUN2,
     *      IBPRNO, BPKOLS, BPNUMV, BPANT, BPPOL, BPIF, BPFRQ, BPBCHN,
     *      NUMSHF, LOWSHF, DELSHF, LBPTYP, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, IVER
            GO TO 990
            END IF
C                                       Loop over entries
         DO 50 IENT = 1, NENTRY
C                                       Read BT entries
            CALL BTTAB ('READ', TABUF2, NOBAND, BPWT, BNDPAS, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1030) IERR, IVER
               GO TO 990
               END IF
C                                       Source number changes?
            IF (SRCCHA) ISRC = SRCFNC (ISRC)
C                                       Do we want this source?
            IF (.NOT.WANSRC(ISRC)) GO TO 50
C                                       Timerange selection?
            RTIM = TIME
            IF (.NOT.WANTIM(RTIM)) GO TO 50
            IF (RTIM.GT.TEND) GO TO 60
C                                       Antenna number changes?
            IF (ANTCHA) THEN
               NOSTA = ANTFNC (NOSTA, IARRAY)
               IF (REFAN1.GT.0) REFAN1 = ANTFNC (REFAN1, IARRAY)
               IF (REFAN2.GT.0) REFAN2 = ANTFNC (REFAN2, IARRAY)
               END IF
C                                       Copy necessary variables
            BPREFT(1) = REFAN1
            BPREFT(2) = REFAN2
C                                       Change FQID
            OUTFQI = FQIFNC(IFQID)
            DO 20 J = 1,BPIF
               IFFREQ(J) = BANDFR(J) - REFF
 20            CONTINUE
C                                       Write BP table
            IF (OUTFQI.GE.1) THEN
               CALL TABBP ('WRIT', TABUF1, IBPRNO, BPKOLS, BPNUMV, BPIF,
     *            BPFRQ, BPPOL, TIME, TINT, ISRC, IARRAY, NOSTA, CHBW,
     *            IFFREQ, OUTFQI, BPREFT, BPWT, BNDPAS, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1020) IERR, IVER
                  GO TO 990
                  END IF
               END IF
 50         CONTINUE
C                                       Close down current BP/BA
C                                       tables
 60      CALL TABIO ('CLOS', 1, IBPRNO, TABUF1, TABUF1, IERR)
         CALL TABIO ('CLOS', 1, IBTRNO, TABUF2, TABUF2, IERR)
 100     CONTINUE
C                                       Update CATBLK
      CALL CATIO ('UPDT', DISK, CNO, CATIEQ, 'REST', TABUF1, IERR)
      CALL COPY (256, CATIEQ, CATIN)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BA2BP: ERROR ',I3,' OPENING BP TABLE # ',I4)
 1010 FORMAT ('BT2BP: ERROR ',I3,' OPENING BT TABLE # ',I4)
 1020 FORMAT ('BT2BP: ERROR ',I3,' WRITING BP TABLE # ',I4)
 1030 FORMAT ('BT2BP: ERROR ',I3,' READING BT TABLE # ',I4)
 1040 FORMAT ('BT2BP: ',A,' KEYWORDS DO NOT MATCH CAT. HEADER')
      END
      SUBROUTINE CRETAB (SRTORD, VOL, CNO, VER, CATBLK, LUN, DATP, NHED,
     *   COLHED, LOGKOL, TABUFF, IERR)
C-----------------------------------------------------------------------
C  This routine will create a table file and initialize the header
C  values of the table from the data in common TABHDR
C  Inputs:
C     SRTORD    I          Logical column sorted on. 0=unknown,
C                          < 0 = descending
C     VOL       I          Disk volume number of parent file.
C     CNO       I          Catalog number of parent file.
C     VER       I          Version number table file. 0=use latest + 1.
C     CATBLK    I(256)     Catalog header of parent file.
C     LUN       I          Logical unit number to open table file.
C     NHED      I          # column headers to find logical column
C                          numbers for
C     COLHED    C(*)*24    Column head labels needed
C  In/Out:
C  Output:
C     DATP      I(128,2)   Data pointers used in table file control.
C     TABUFF    I(512)     IO buffer for table file.
C     LOGKOL    I(*)       Logical column #'s, -1 => not present
C     IERR      I          Error code.
C-----------------------------------------------------------------------
      INTEGER   SRTORD, VOL, CNO, VER, CATBLK(256), LUN, DATP(128,2),
     *   NHED, LOGKOL(*), TABUFF(512), IERR
      CHARACTER COLHED(*)*24
C
      INTEGER   IRNO, TITLES, UNITS, I, NREC, TTCODE(128), J, IMULT,
     *   IREMA, ILEFT
      CHARACTER CTYPE(128)*24
      INTEGER   ATNUM, IMNUM, MCNUM, FRNUM, SONUM
C    *   , TYNUM, GCNUM
      PARAMETER (ATNUM = 4)
      PARAMETER (IMNUM = 5)
      PARAMETER (MCNUM = 2)
      PARAMETER (FRNUM = 5)
      PARAMETER (SONUM = 8)
C      PARAMETER (TYNUM = 4)
C      PARAMETER (GCNUM = 16)
      CHARACTER ATCOL(ATNUM)*8, IMCOL(IMNUM)*8, MCCOL(MCNUM)*8,
     *   FRCOL(FRNUM)*8, SOCOL(SONUM)*8
C    *   ,TYCOL(TYNUM)*8, GCCOL(GCNUM)*8
      HOLLERITH HSTRNG(6)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'DATSEL.INC'
      DATA TITLES, UNITS /3,4/
      DATA ATCOL /'POLAA   ','POLCALA ','POLAB   ','POLCALB '/
      DATA IMCOL /'FREQ.VAR','PDELAY_1','PRATE_1 ','PDELAY_2',
     *   'PRATE_2 '/
      DATA MCCOL /'LO_OFFSE','DLO_OFFS'/
      DATA FRCOL /'BANDFREQ','CH_WIDTH','TOTAL_BA','SIDEBAND',
     *   'BB_CHAN '/
      DATA SOCOL /'IFLUX   ','QFLUX   ','UFLUX   ','VFLUX   ',
     *   'ALPHA   ','FREQOFF ','SYSVEL  ','RESTFREQ' /
C      DATA TYCOL /'TSYS 1  ','TANT 1  ','TSYS 2  ','TANT 2  '/
C      DATA GCCOL /'TYPE_1  ','NTERM_1 ','X_TYP_1 ','Y_TYP_1 ',
C     *            'X_VAL_1 ','Y_VAL_1 ','GAIN_1  ','SENS_1  ',
C     *            'TYPE_2  ','NTERM_2 ','X_TYP_2 ','Y_TYP_2 ',
C     *            'X_VAL_2 ','Y_VAL_2 ','GAIN_2  ','SENS_2  '/
C-----------------------------------------------------------------------
C                                       Zero out control block.
      CALL FILL (256, 0, DATP)
      CALL FILL (MAXNCL, 0, COLSEL)
C                                       Ensure small integer data
C                                       types don't get transferred
C                                       to output table type array
      CALL FILL (128, 0, TTCODE)
      DO 10 I = 1, ITNCOL
         TTCODE(I) = TFCODE(I)
         IF (MOD(TFCODE(I),10).EQ.6) THEN
            TTCODE(I) = TFCODE(I) - 2
            END IF
 10      CONTINUE
C                                       Set up IF selection
      IF (IFSLT) THEN
         DO 300 I = 1, ITNCOL
            IF (ITYPE.EQ.'AT') THEN
C                                       AT table
               DO 200 J = 1, ATNUM
                  IF (TTYPE(I)(1:8).EQ.ATCOL(J)(1:8)) THEN
                     IMULT = (TTCODE(I) - MOD(TTCODE(I),10)) / 10
                     IREMA = TTCODE(I) - IMULT*10
                     ILEFT = IMULT / ORIGIF
                     TTCODE(I) = IREMA + (ILEFT*SLIFS)*10
                     COLSEL(I) = ILEFT
                     END IF
 200              CONTINUE
            ELSE IF (ITYPE.EQ.'IM') THEN
C                                       IM table
               DO 210 J = 1, IMNUM
                  IF (TTYPE(I)(1:8).EQ.IMCOL(J)(1:8)) THEN
                     IMULT = (TTCODE(I) - MOD(TTCODE(I),10)) / 10
                     IREMA = TTCODE(I) - IMULT*10
                     ILEFT = IMULT / ORIGIF
                     TTCODE(I) = IREMA + (ILEFT*SLIFS)*10
                     COLSEL(I) = ILEFT
                     END IF
 210              CONTINUE
            ELSE IF (ITYPE.EQ.'MC') THEN
C                                       MC table
               DO 220 J = 1, MCNUM
                  IF (TTYPE(I)(1:8).EQ.MCCOL(J)(1:8)) THEN
                     IMULT = (TTCODE(I) - MOD(TTCODE(I),10)) / 10
                     IREMA = TTCODE(I) - IMULT*10
                     ILEFT = IMULT / ORIGIF
                     TTCODE(I) = IREMA + (ILEFT*SLIFS)*10
                     COLSEL(I) = ILEFT
                     END IF
 220              CONTINUE
            ELSE IF (ITYPE.EQ.'FR') THEN
C                                       FR table
               DO 230 J = 1, FRNUM
                  IF (TTYPE(I)(1:8).EQ.FRCOL(J)(1:8)) THEN
                     IMULT = (TTCODE(I) - MOD(TTCODE(I),10)) / 10
                     IREMA = TTCODE(I) - IMULT*10
                     ILEFT = IMULT / ORIGIF
                     TTCODE(I) = IREMA + (ILEFT*SLIFS)*10
                     COLSEL(I) = ILEFT
                     END IF
 230              CONTINUE
            ELSE IF (ITYPE.EQ.'SO') THEN
C                                       SO table
               DO 240 J = 1, SONUM
                  IF (TTYPE(I)(1:8).EQ.SOCOL(J)(1:8)) THEN
                     IMULT = (TTCODE(I) - MOD(TTCODE(I),10)) / 10
                     IREMA = TTCODE(I) - IMULT*10
                     ILEFT = IMULT / ORIGIF
                     TTCODE(I) = IREMA + (ILEFT*SLIFS)*10
                     COLSEL(I) = ILEFT
                     END IF
 240              CONTINUE
               END IF
 300        CONTINUE
         END IF
C
      CALL COPY (ITNCOL, TTCODE, DATP(1,2))
C                                       Initialize logical column array
      DO 20 I = 1, NHED
         LOGKOL(I) = -1
   20    CONTINUE
C                                       create/open
      NREC = MIN (NAXISI(2), 5000)
      CALL TABINI ('WRIT', ITYPE, VOL, CNO, VER, CATBLK, LUN, ITANKY,
     *   NREC, ITNCOL, DATP, TABUFF, IERR)
      IF (IERR.GT.0) GO TO 999
      IF (IERR.EQ.-1) THEN
C                                       Write column values
         DO 50 I = 1,ITNCOL
            IRNO = I
C                                       Write column titles.
            CALL CHR2H (24, TTYPE(I), 1, HSTRNG)
            CALL TABIO ('WRIT', TITLES, IRNO, HSTRNG, TABUFF, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       write units
            CALL CHR2H (8, TUNIT(I), 1, HSTRNG)
            CALL TABIO ('WRIT', UNITS, IRNO, HSTRNG, TABUFF, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Search for colum header match
            DO 40 J = 1, NHED
               IF (TTYPE(I).EQ.COLHED(J)) LOGKOL(J) = I
 40            CONTINUE
 50         CONTINUE
C                                       sort order
         TABUFF(43) = SRTORD
C                                       table title
         CALL CHR2H (24, EXTNAM, 1, TABUFF(101))
         END IF
C                                       If adding to old table
C                                       read titles and compare with
C                                       COLHED
      IF (IERR.EQ.0) THEN
         DO 100 I = 1,ITNCOL
            IRNO = I
C                                       Read column titles.
            CALL TABIO ('READ', TITLES, IRNO, HSTRNG, TABUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL H2CHR (24, 1, HSTRNG, CTYPE(I))
C                                       Search for colum header match
            DO 80 J = 1,NHED
               IF (CTYPE(I).EQ.COLHED(J)) LOGKOL(J) = I
 80            CONTINUE
 100        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE RDVTAB (FDVEC, TBIND, DPTR, NAXIS, TABUFF, TAPBUF,
     *   DAYOFF, LOGKOL, ROWF, BIF, IERR)
C-----------------------------------------------------------------------
C   This routine will read the data section of a FITS 3-D table and copy
C   the data to the AIPS version of the table. This is a special version
C   modIfied to update the time column by a day offset on the fly if
C   necessary. Will also edit IM tables based on user supplied selection
C   criteria. Will also update AN numbers in the MC table.
C   Inputs:
C      FDVEC    I(50)      File descriptor vector for TAPIO input
C      DPTR     I(128,2)   Data Pointers, used in table file control.
C      NAXIS    I(2)       Length of columns (in char), number of rows.
C      DAYOFF   D          Offset day number to be added to the time
C                         column.
C      LOGKOL   I(*)       Logical columns for TIME, TIME_INTERVAL,
C                          SOURCE_ID, ANTENNA_NO, ARRAY, FREQID,
C                          TIMERANG
C   In/Out:
C      TBIND    I          Buffer pointer in TAPBUF
C      TABUFF   I(512)     Disk Table file I/O buffer.
C      TAPBUF   I(*)       Tape I/O buffer.
C   Outputs:
C      ROWF     I          Row number at which appending began
C      IERR     I          Error code. 0=ok.
C-----------------------------------------------------------------------
      INTEGER   FDVEC(50), TBIND, DPTR(128,2), NAXIS(2),
     *   TABUFF(512), TAPBUF(*), LOGKOL(*), ROWF, BIF, IERR
      DOUBLE PRECISION DAYOFF
C
      CHARACTER CBUFF*2048
      REAL      RTIME
      INTEGER   TBYTCT, MXLREB, ROWL, ANTFNC, SRCFNC, NAXIS1, NAXIS2,
     *   LCTR, BYTCNT(7), KTYPE, I, IOFF, IT0, II, TCOUNT(128), NEXT,
     *   NBYTE, TPTYPE(128), LIMIT, TOFF(128), TKOL, SKOL,
     *   AKOL, ANKOL, TIKOL, FQKOL, TRKOL, LWRIT, JOFF, JT, JTRIM
      LOGICAL   WANSRC, WANTIM, SELECT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'SCRTCH.INC'
      DATA BYTCNT /8,4,1,4,1,2,1/
C-----------------------------------------------------------------------
      IERR = 0
      IF (NAXIS(2).EQ.0) THEN
         CALL TABIO ('CLOS', 0, LCTR, TRECR, TABUFF, IERR)
         WRITE (MSGTXT,1000) ITYPE
         GO TO 990
         END IF
C                                       Calculate end & type
C                                       of Column values.
C                                       Use TFCODE to determine types
C                                       because DPTR holds output info.
      TBYTCT = 0
      DO 10 I = 1,ITNCOL
         TPTYPE(I) = MOD (TFCODE(I),10)
         TCOUNT(I) = DPTR(I,2) / 10
         TOFF(I) = DPTR(I,1)
C                                       Count real bytes
         KTYPE = TPTYPE(I)
         TBYTCT = TBYTCT + TCOUNT(I) * BYTCNT(KTYPE)
 10      CONTINUE
C                                       Check buffer size
      MXLREB = XBPRSZ * NBITWD / 4
C                                       Record too big
      IF (TBYTCT.GT.MXLREB) THEN
         IERR = 5
         WRITE (MSGTXT,1010) TBYTCT, MXLREB
         GO TO 990
         END IF
C                                       Read first record,
      CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
      NEXT = 1
      IF (IERR.NE.0) GO TO 999
C                                       Loop for all lines in table.
      NAXIS1 = NAXIS(1)
      NAXIS2 = NAXIS(2)
      ROWF = TABUFF(5) + 1
      ROWL = TABUFF(5) + NAXIS2
      LWRIT = 0
      DO 800 LCTR = ROWF, ROWL
         SKOL = -1
         ANKOL = -1
         AKOL = -1
         TKOL = -1
         TIKOL = -1
         FQKOL = -1
         TRKOL = -1
         DO 200 I = 1,ITNCOL
            SELECT = COLSEL(I).GT.0
            JOFF = 1
            IF (SELECT) JOFF = (COLSEL(I) * (BIF-1)) + 1
C                                       Read a FITS table data entry.
            KTYPE = TPTYPE(I)
            NBYTE = BYTCNT(KTYPE) * TCOUNT(I)
            IF (SELECT) NBYTE = BYTCNT(KTYPE) * COLSEL(I) * ORIGIF
            IF (KTYPE.EQ.7) NBYTE = 1 + (TCOUNT(I)-1) / 8
            CALL GTF3D (FDVEC, TBIND, NEXT, TAPBUF, NBYTE, RECORD, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Go to correct type
            IOFF = TOFF(I)
C                                       Set up log. col. pointers
            IF (I.EQ.LOGKOL(1)) TKOL = IOFF
            IF (I.EQ.LOGKOL(2)) TIKOL = IOFF
            IF (I.EQ.LOGKOL(3)) SKOL = IOFF
            IF (I.EQ.LOGKOL(4)) ANKOL = IOFF
            IF (I.EQ.LOGKOL(5)) AKOL = IOFF
            IF (I.EQ.LOGKOL(6)) FQKOL = IOFF
            IF (I.EQ.LOGKOL(7)) TRKOL = IOFF
            IT0 = TPTYPE(I)
            GO TO (110, 120, 130, 140, 150, 160, 170), IT0
C                                       Double precision
 110        CALL ZR64RL (TCOUNT(I), JOFF, RECORD, TRECD(IOFF))
            GO TO 200
C                                       Single precision
 120        CALL ZR32RL (TCOUNT(I), JOFF, RECORD, TRECR(IOFF))
            GO TO 200
C                                       Character.
 130        CALL ZC8CL (TCOUNT(I), JOFF, RECORD, CBUFF)
            JT = JTRIM (CBUFF(:TCOUNT(I)))
            CALL CHR2H (TCOUNT(I), CBUFF, 1, TRECH(IOFF))
            GO TO 200
C                                       Long Integer.
C                                       Convert to local I
 140        CALL ZI32IL (TCOUNT(I), JOFF, RECORD, TRECI(IOFF))
            GO TO 200
C                                       Logical
 150        LIMIT = TCOUNT(I)
            CALL ZC8CL (LIMIT, JOFF, RECORD, CBUFF)
            DO 155 II = 1,LIMIT
               TRECL(IOFF+II-1) = CBUFF(II:II).EQ.'T'
 155           CONTINUE
            GO TO 200
C                                       Short Integer.
C                                       Convert to INTEGER
 160        CALL ZI16IL (TCOUNT(I), JOFF, RECORD, TRECI(IOFF))
            GO TO 200
C                                       bit array
 170        CALL ZX8XL (TCOUNT(I), RECORD(JOFF), TRECI(IOFF))
 200        CONTINUE
C                                       Deal with day offset
C                                       OB tables (and perhaps others)
C                                       use a modified Julian day
C                                       number and should not be
C                                       adjusted: here a time greater
C                                       than 1000 days is assumed to be
C                                       an MJD.
         IF (TKOL.GT.0) THEN
            IF (TRECD(TKOL).LE.1000.0D0) THEN
               TRECD(TKOL) = TRECD(TKOL) + DAYOFF
               END IF
            END IF
C                                       Also need to fix timerange in
C                                       flag tables
         IF (TRKOL.GT.0) THEN
            DO 210 I = 1, 2
               TRECR(TRKOL + I - 1) = TRECR(TRKOL + I - 1) + DAYOFF
  210       CONTINUE
         END IF
C                                       Edit the IM table on the fly
         IF (ITYPE.EQ.'IM') THEN
            IF (SRCCHA) TRECI(SKOL) = SRCFNC(TRECI(SKOL))
            IF (ANTCHA) TRECI(ANKOL) =
     *         ANTFNC(TRECI(ANKOL), TRECI(AKOL))
            IF (.NOT.WANSRC(TRECI(SKOL))) GO TO 800
            RTIME = TRECD(TKOL)
            IF (.NOT.WANTIM(RTIME)) GO TO 800
            IF (RTIME.GT.TEND) GO TO 850
            END IF
C                                       Edit the MC table on the fly
         IF (ITYPE.EQ.'MC') THEN
            IF (SRCCHA) TRECI(SKOL) = SRCFNC(TRECI(SKOL))
            IF (ANTCHA) TRECI(ANKOL) = ANTFNC(TRECI(ANKOL), 1)
            END IF
C                                       Write this table row.
         CALL TABIO ('WRIT', 0, LWRIT, TRECR, TABUFF, IERR)
 800     CONTINUE
 850  CALL TABIO ('CLOS', 0, LWRIT, TRECR, TABUFF, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Warning: table type ',A2,' is of zero length')
 1010 FORMAT ('ERROR: TABLE ROW =',I6,' BYTES, BUFFER SIZE =',I6)
      END
      SUBROUTINE CQADD (IDISK, JCNO, CATIN, JBUF1, JBUF2, JLUN1, JLUN2,
     *   IRET)
C-----------------------------------------------------------------------
C   Append correlation modes to a CQ table
C   Inputs:
C      IDISK    I        Disk volume number
C      JCNO     I        Catalog slot number of uv file
C      CATIN    I(256)   Catalog header block
C      JBUF1    I(512)   I/O buffer 1
C      JBUF2    I(512)   I/O buffer 2
C      JLUN1    I        LUN 1 to use for table I/O
C      JLUN2    I        LUN 2 to use for table I/O
C   Input from common:
C      ICQCOR   /CRFILT/ Correlation id. array
C      ICQFLT   /CRFILT/ Time filter id. array
C      RCQINT   /CRFILT/ Filter averaging time
C      NCQCOR   /CRFILT/ No. of entries in ICQCOR, ICQFLT and RCQINT
C   Output:
C      IRET     I        Return code; 0=> ok
C   Output to common:
C      Uses DCAT.INC and DUVH.INC
C-----------------------------------------------------------------------
      INTEGER   IDISK, JCNO, CATIN(256), JLUN1, JLUN2, JBUF1(512),
     *   JBUF2(512), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCQV.INC'
      INTEGER   MAXKEY, MAXKY2
      PARAMETER (MAXKEY = 4, MAXKY2 = MAXKEY+1)
      LOGICAL   WEXIT
      CHARACTER LTYPE*2, KEYSTR(MAXKEY)*8, LTAPER*8, LTAPCQ(MAXIF)*8,
     *   BNDCOD(MAXIF)*8
      DOUBLE PRECISION DFOFF(MAXIF), DFRQCQ(MAXIF), DCBWCQ(MAXIF)
      REAL      FINC(MAXIF), BANDW(MAXIF), TAVGCQ(MAXIF)
      INTEGER   NOIFCQ, IFQRNO, FQKOLS(MAXFQC), FQNUMV(MAXFQC), NUMIF,
     *   ICQRNO, CQKOLS(MAXCQC), CQNUMV(MAXCQC), IFQID, JIF, IROW,
     *   IFQDCQ, ISUBCQ, NFFTCQ(MAXIF), NCHCQ(MAXIF), NSAVCQ(MAXIF),
     *   NOVSCQ(MAXIF), NZPDCQ(MAXIF), IFLTCQ(MAXIF), NBITCQ(MAXIF),
     *   IOVLCQ(MAXIF), IFSIDE(MAXIF), IKLOC(MAXKEY), KEYVAL(MAXKY2),
     *   KEYTYP(MAXKEY), NSUBA, IVER, NKEY, NCOL, NREC, DATP(128,2),
     *   IERR, MSGSAV, I, IPOINT, NFFT, NOVS, NZPD, NCHAN, JCOR
      INCLUDE 'DFLT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
C                                       LK add the line Aug 1, 03
      INCLUDE 'DIGCOR.INC'
      DATA KEYSTR /'FFT_SIZE', 'OVERSAMP', 'ZERO_PAD', 'TAPER_FN'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Copy CATIN to CATBLK and
C                                       determine header pointers
      CALL COPY (256, CATIN, CATBLK)
      CALL UVPGET (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         IRET = 1
         GO TO 990
         END IF
C                                       Get no. of AN tables (subarrays)
      CALL FNDEXT ('AN', CATBLK, NSUBA)
C                                       Get keywords from the MC table
      LTYPE = 'MC'
      IVER = 1
      MSGSAV = MSGSUP
      MSGSUP = 32000
C                                       Initialization
      NFFT = -1
      NOVS = -1
      NZPD = -1
      LTAPER = ' '
C
      CALL TABINI ('READ', LTYPE, IDISK, JCNO, IVER, CATBLK, JLUN1,
     *   NKEY, NREC, NCOL, DATP, JBUF1, IERR)
C                                       Extract keywords
      IF (IERR.LE.0) THEN
         NKEY = MAXKEY
         CALL TABKEY ('READ', KEYSTR, NKEY, JBUF1, IKLOC, KEYVAL,
     *      KEYTYP, IERR)
         IF ((IERR.EQ.0) .OR. (IERR.GT.20)) THEN
            DO 50 I = 1, NKEY
               IPOINT = IKLOC(I)
               IF (IPOINT.GT.0) THEN
                  IF (I.EQ.1) THEN
                     NFFT = KEYVAL(IPOINT)
                  ELSE IF (I.EQ.2) THEN
                     NOVS = KEYVAL(IPOINT)
                  ELSE IF (I.EQ.3) THEN
                     NZPD = KEYVAL(IPOINT)
                  ELSE
                     CALL H2CHR (8, 1, KEYVAL(IPOINT), LTAPER)
                     END IF
                  END IF
  50           CONTINUE
            END IF
C                                       Close MC table
         CALL TABIO ('CLOS', 0, 0, JBUF1, JBUF1, IERR)
         END IF
      MSGSUP = MSGSAV
C                                       Get NCHAN from catalog hdr.
      IF (JLOCF.GT.0) THEN
         NCHAN = CATBLK(KINAX+JLOCF)
      ELSE
         NCHAN = 1
         END IF
C                                       Get NO_IF from catalog hdr.
      IF (JLOCIF.GT.0) THEN
         NOIFCQ = CATBLK(KINAX+JLOCIF)
      ELSE
         NOIFCQ = 1
         END IF
C                                       Can a CQ table be created ?
      WEXIT = ((NSUBA.LE.0).OR.(NFFT.LT.0).OR.(NOVS.LT.0).OR.
     *   (NZPD.LT.0).OR.(LTAPER.EQ.' '))
C                                       Exit if not possible
      IF (WEXIT) THEN
         WRITE (MSGTXT,1070)
         IRET = 2
         GO TO 990
         END IF
C                                       Open FQ table for READ
      IVER = 1
      CALL FQINI ('READ', JBUF1, IDISK, JCNO, IVER, CATBLK, JLUN1,
     *   IFQRNO, FQKOLS, FQNUMV, NUMIF, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('READ', 'FQINI', 'CQADD', IERR)
         IRET = 3
         GO TO 999
         END IF
C                                       Open CQ table for WRIT
      IVER = 1
      CALL CQINI ('WRIT', JBUF2, IDISK, JCNO, IVER, CATBLK, JLUN2,
     *   ICQRNO, CQKOLS, CQNUMV, NOIFCQ, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('WRIT', 'CQINI', 'CQADD', IERR)
         IRET = 5
         GO TO 999
         END IF
C                                       Get no. records in FQ table
      NREC = JBUF1(5)
C                                       Write CQ over again fully
      ICQRNO = 1
C                                       While NOT EOF(FQ_table) do
      DO 250 IROW = 1,NREC
         IFQRNO = IROW
         CALL TABFQ ('READ', JBUF1, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *      IFQID, DFOFF, FINC, BANDW, IFSIDE, BNDCOD, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'TABFQ', 'CQADD', IERR)
            IRET = 6
            GO TO 999
            END IF
C                                       Loop over all subarrays
         DO 200 I = 1,NSUBA
C                                       Loop over the new
C                                       correlation modes
            DO 180 JCOR = 1,NCQCOR
C                                       Fill CQ record
               IFQDCQ = IFQID
               ISUBCQ = I
               DO 150 JIF = 1, NOIFCQ
                  NFFTCQ(JIF) = NFFT
                  NCHCQ(JIF) = NCHAN
                  IF (NCHAN.GT.0) THEN
                     NSAVCQ(JIF) = NFFT / (2.0 * NCHAN)
                  ELSE
                     NSAVCQ(JIF) = -1
                     END IF
                  DFRQCQ(JIF) = FREQ + DFOFF(JIF)
                  DCBWCQ(JIF) = FINC(JIF)
                  LTAPCQ(JIF) = LTAPER
                  NOVSCQ(JIF) = NOVS
                  NZPDCQ(JIF) = NZPD
C                                       Encode correlation_id and
C                                       filter_id in this column
                  IFLTCQ(JIF) = 256 * (ICQCOR(JCOR) - 1) + ICQFLT(JCOR)
                  TAVGCQ(JIF) = RCQINT(JCOR)
C                                       LK add calculation of NBITCQ
C                                       Aug 1, 03
C                  NBITCQ(JIF) = 0
                  NBITCQ(JIF) = LOG(NLEVS(1)+0.1) / LOG(2-0.1)
                  IOVLCQ(JIF) = 0
  150             CONTINUE
C                                       Write CQ record
               CALL TABCQ ('WRIT', JBUF2, ICQRNO, CQKOLS, CQNUMV,
     *            NOIFCQ, IFQDCQ, ISUBCQ, NFFTCQ, NCHCQ, NSAVCQ,
     *            DFRQCQ, DCBWCQ, LTAPCQ, NOVSCQ, NZPDCQ, IFLTCQ,
     *            TAVGCQ, NBITCQ, IOVLCQ, IERR)
               IF (IERR.NE.0) THEN
                  CALL TABERR ('WRIT', 'TABCQ', 'CQADD', IERR)
                  IRET = 7
                  GO TO 999
                  END IF
  180          CONTINUE
  200       CONTINUE
  250    CONTINUE
C                                       Hard close FQ, CQ tables
      CALL TABIO ('CLOS', 0, IFQRNO, JBUF1, JBUF1, IRET)
      CALL TABIO ('CLOS', 0, ICQRNO, JBUF2, JBUF2, IRET)
C                                       Update CATBLK
      CALL CATIO ('UPDT', IDISK, JCNO, CATBLK, 'REST', JBUF1, IRET)
      CALL COPY (256, CATBLK, CATIN)
      GO TO 999
C                                       Error
  990 CALL MSGWRT (8)
C                                       Exit
  999 RETURN
C----------------------------------------------------------------------
 1000 FORMAT ('CQADD: ERROR',I4,' FROM UVPGET')
 1070 FORMAT ('CQADD: NOT ENOUGH INFORMATION TO CREATE CQ TABLE')
      END
      SUBROUTINE CORID (IFILT, TAVG, ICID, IRET)
C----------------------------------------------------------------------
C   Assign a correlation id. based on the filter id. and avg. time
C   Inputs:
C      IFILT     I        Filter id. (0 => no filter
C                                     1 => decimation factor 1  (8-1)
C                                     2 => decimation factor 2 (16-2)
C                                     3 => decimation factor 4 (32-4)
C                                     4 => decimation factor 8 (64-8)
C      TAVG      R        Filter averaging time (sec)
C   Input from common:
C      ICQCOR    /CFILTR/ Array of assigned correlation id.'s
C      ICQFLT    /CFILTR/ Array of associated filter id.'s
C      RCQINT    /CFILTR/ Array of associated integration times.
C      NCQCOR    /CFILTR/ Element count in ICQCOR, ICQFLT, RCQINT.
C   Output:
C      ICID      I        Assigned correlator id.
C      IRET      I        Return code (0=>ok, else error)
C----------------------------------------------------------------------
      REAL TAVG
      INTEGER IFILT, ICID, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'DFLT.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL WFOUND, WFREE
      INTEGER I, J, K, ICORR
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      ICID = 0
C                                       Search for match in array of
C                                       existing correlation id.
C                                       modes
      I = 1
      WFOUND = .FALSE.
 50   IF ((I.GT.NCQCOR) .OR. WFOUND) GO TO 100
         WFOUND = (ICQFLT(I).EQ.IFILT) .AND.
     *      (ABS(RCQINT(I)-TAVG).LT.0.001)
         I = I + 1
         GO TO 50
C
 100  IF (.NOT.WFOUND) THEN
C                                       Find lowest free corr. id.
         J = 1
         WFREE = .FALSE.
 150     IF ((J.GT.MAXCID).OR.WFREE) GO TO 200
            WFREE = .TRUE.
            DO 175 K = 1, NCQCOR
               IF (ICQCOR(K).EQ.J) WFREE = .FALSE.
 175           CONTINUE
            J = J + 1
            GO TO 150
C                                       Check for too many corr. id.'s
 200     IF (.NOT.WFREE) THEN
            IRET = 1
            WRITE (MSGTXT,1175) MAXCID
            GO TO 990
         ELSE
            ICORR = J - 1
C                                       Add to array of corr. id.'s
            NCQCOR = NCQCOR + 1
            ICQCOR(NCQCOR) = ICORR
            ICQFLT(NCQCOR) = IFILT
            RCQINT(NCQCOR) = TAVG
            END IF
      ELSE
         ICORR = ICQCOR(I-1)
         END IF
C
      ICID = ICORR
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C                                       Exit
 999  RETURN
C----------------------------------------------------------------------
 1175 FORMAT ('CORID: PARAMETER MAXCID=',I3,' TOO SMALL; CONTACT',
     *   ' AIPS ADMIN.')
      END
      SUBROUTINE MCSORT (DISKIN, CNOIN, GUSE, CATBLK, IERR)
C-----------------------------------------------------------------------
C   Sorts the MC table to time-ant order.
C   Inputs:
C      DISKIN   I   Volume number
C      CNOIN    I   File catalogue number
C      GUSE     I   Version number of MC table to use
C   Output:
C      IERR     I   Return code, 0=>OK, otherwise IM table exists but
C                   cannot be read.
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN, GUSE, CATBLK(256), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER COLHED(2)*24
      INTEGER   KOLS(2), KEY(2,2), MCLUN, MCBUFF(512), NMCINR, NKEY,
     *   NREC, NCOL, IMCRNO, DATP(128, 2), KEYSUB(2,2)
      LOGICAL   T
      REAL      FKEY(2,2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA COLHED /'TIME', 'ANTENNA_NO.' /
      DATA T /.TRUE./
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
C                                       Open Calibration table
      NKEY = 0
      NREC = 0
      NCOL = 0
      IMCRNO = 1
      MCLUN = 29
      CALL TABINI ('READ', 'MC', DISKIN, CNOIN, GUSE, CATBLK, MCLUN,
     *   NKEY, NREC, NCOL, DATP, MCBUFF, IERR)
      IF (IERR.LE.0) GO TO 150
      WRITE (MSGTXT,1000) IERR, 'MC', GUSE
      GO TO 990
C                                       Get number of scans
 150  NMCINR = MCBUFF(5)
C                                       Check if empty
      IF (NMCINR.LE.0) THEN
         WRITE (MSGTXT,1010)
         IERR = 1
         GO TO 990
         END IF
C                                       Get column pointers
      NKEY = 2
      CALL FNDCOL (NKEY, COLHED, 8, T, MCBUFF, KOLS, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.10)) GO TO 999
      IERR = 0
C                                       Close table
      CALL TABIO ('CLOS', 0, IMCRNO, MCBUFF, MCBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Always sort, don't trust the
C                                       correlator
      KEY(1,1) = KOLS(1)
      KEY(2,1) = 0
      KEY(1,2) = KOLS(2)
      KEY(2,2) = 0
C                                       Sort
      CALL TABSRT (DISKIN, CNOIN, 'MC', GUSE, GUSE, KEY, KEYSUB, FKEY,
     *   MCBUFF, CATBLK, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MCSORT: ERROR',I3,' OPENING ',A2,' TABLE, VERSION',I5)
 1010 FORMAT ('MCSORT: EMPTY IM TABLE')
 1020 FORMAT ('MCSORT: ERROR',I3,' CLOSING IM TABLE')
      END
      SUBROUTINE FL2FG (DISK, CNO, BIF, EIF, BCHAN, ECHAN, CATBLK, IRET)
C-----------------------------------------------------------------------
C   Apply IF and channel selection to flag data in FITS interferometry
C   data interchange flag (FL) tables and append it to FG table 1, which
C   should be created if it does not already exist. Delete each FL table
C   as it is converted.
C
C   Inputs:
C      DISK     I        Volume number of parent data file
C      CNO      I        Catalogue number of parent data file
C      BIF      I        First selected IF
C      EIF      I        Last selected IF
C      BCHAN    I        First selected channel
C      ECHAN    I        Last selected channel
C
C   Input/output:
C      CATBLK   I(256)   Catalogue header for parent data file (updated)
C
C   Output:
C      IRET     I        Return code:
C                           0 - tables converted
C                           1 - I/O error detected
C
C   Uses LUNs 40 and 41.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PFLV.INC'
C
      INTEGER   DISK, CNO, BIF, EIF, BCHAN, ECHAN, CATBLK(256), IRET
C
C     Local variables:
C
C     HIVER     Highest FL table version number
C     VER       Current FL table version number
C     EXIST     Does FL table exist?
C     TABLE     Is FL table a true table?
C     FITASC    Can FL table be written as an ASCII table?
C     TABREV    FL table revision number
C     NUMIF     Number of bands in FL table
C     ROW       Row number
C     SOURID    Source ID number
C     SUBARR    Subarray number
C     ANTS      Antenna numbers
C     FREQID    Frequency ID number
C     TIMRAN    Time range
C     BANDS     Band range
C     BANDS2    Band range corrected for IF selection
C     IFFLGS    Band flags
C     CHANS     Channel range
C     PFLAGS    Polarization flags
C     REASON    Reason string
C     SEVRTY    Severity level
C     FLROW     Current row in FL table
C     FLLUN     LUN for FL tables
C     FLBUFF    I/O control structure and buffer for FL tables
C     FLCOLS    Indices for FL table columns
C     FLNUMV    Column dimensions for FL tables
C     FGROW     Current row in FG table
C     FGLUN     LUN for FG tables
C     FGBUFF    I/O control structure and buffer for FG tables
C     FGCOLS    Indices for FG table columns
C     FGNUMV    Column dimensions for FG tables
C     FGVER     FG table version number
C     MSGSAV    Saved message suppression level
C     I         Loop index
C     FNAME     File name of FL table
C     JRET      Temporary error indicator
C
      INTEGER   HIVER, VER
      LOGICAL   EXIST, TABLE, FITASC
      INTEGER   TABREV, NUMIF
      INTEGER   ROW, SOURID, SUBARR, ANTS(2), FREQID, BANDS(2),
     *          BANDS2(2), CHNRAN(2), SEVRTY
      REAL      TIMRAN(2)
      LOGICAL   IFFLGS(MAXIF), PFLAGS(4)
      CHARACTER REASON*40
      INTEGER   FLROW, FLLUN, FLBUFF(512), FLCOLS(MAXFLC),
     *          FLNUMV(MAXFLC)
      INTEGER   FGROW, FGLUN, FGBUFF(512), FGCOLS(MAXFGC),
     *          FGNUMV(MAXFGC), FGVER
      INTEGER   MSGSAV, I, JRET
C
      PARAMETER (FLLUN = 40)
      PARAMETER (FGLUN = 41)
C
      INTEGER   SRCFNC, ANTFNC, FQIFNC
      EXTERNAL  SRCFNC, ANTFNC, FQIFNC
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'DATSEL.INC'
C-----------------------------------------------------------------------
      CALL FNDEXT ('FL', CATBLK, HIVER)
C
C     Process each table in turn:
C
      DO 80 VER = 1, HIVER
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL ISTAB ('FL', DISK, CNO, VER, FLLUN, FLBUFF, TABLE, EXIST,
     *               FITASC, IRET)
         IRET = 0
         MSGSUP = MSGSAV
         IF (EXIST) THEN
            CALL FLINI ('READ', FLBUFF, DISK, CNO, VER, CATBLK, FLLUN,
     *                  FLROW, FLCOLS, FLNUMV, NUMIF, TABREV, IRET)
            IF (IRET.EQ.0) THEN
               FGVER = 1
               CALL FLGINI ('WRIT', FGBUFF, DISK, CNO, FGVER, CATBLK,
     *            FGLUN, FGROW, FGCOLS, FGNUMV, IRET)
               IF (IRET.EQ.0) THEN
                  DO 70 ROW = 1,FLBUFF(5)
                     FLROW = ROW
                     CALL TABFL ('READ', FLBUFF, FLROW, FLCOLS, FLNUMV,
     *                  TABREV, NUMIF, SOURID, SUBARR, ANTS, FREQID,
     *                  TIMRAN, IFFLGS, CHNRAN, PFLAGS, REASON, SEVRTY,
     *                  IRET)
C                                       Remap source, antenna and
C                                       frequency IDs:
                     IF (IRET.EQ.0) THEN
                        SUBARR = MAX (1, SUBARR)
                        IF (DOSMAP) SOURID = SRCFNC(SOURID)
                        IF (DOAMAP) THEN
                           DO 10 I = 1, 2
                              IF (ANTS(I).NE.0) ANTS(I) =
     *                           ANTFNC(ANTS(I), SUBARR)
 10                           CONTINUE
                           END IF
                        IF (FREQID.GT.0) THEN
                           FREQID = FQIFNC (FREQID)
                           IF (FREQID.LT.1) GO TO 70
                           END IF
C                                       Adjust channel range for channel
C                                       selection
                        DO 20 I = 1,2
                           IF (CHNRAN(I).NE.0) CHNRAN(I) = MAX (1,
     *                        CHNRAN(I) - BIF + 1)
 20                        CONTINUE
C                                       Fill in channel range defaults
                        IF (CHNRAN(1).EQ.0) CHNRAN(1) = 1
                        IF (CHNRAN(2).EQ.0) CHNRAN(2) = ECHAN-BCHAN+1
C
C                       Write an FG table record for every contiguous
C                       range of flagged bands:
C
                        BANDS(1) = BIF
C
C                       Simulated "while" loop
C                       Invariant: FG table records have been written
C                                  out for bands 1 to BANDS(1) -  1 of
C                                  the current FL table record
C                       Bound: EIF + 1 - BANDS(1)
C
   30                   IF ((IRET.EQ.0)
     *                      .AND. (BANDS(1).NE.(EIF + 1))) THEN
                           I = BANDS(1)
C
C                          Simulated "while" loop
C                          Invariant: IFFLGS(BANDS(1):I-1) are all false
C                          Bound: EIF + 1 - I
C
   40                      IF (I.NE.EIF + 1) THEN
                              IF (.NOT. IFFLGS(I)) THEN
                                 I = I + 1
                                 GO TO 40
                                 END IF
                              END IF
C
C                          If I = EIF + 1 then there are no more bands
C                          for which FG table records are required
C                          otherwise I is the first band to be flagged
C                          in the next record.
C
                           BANDS(1) = I
C
C                          Simulated "while" loop
C                          Invariant: IFFLGS(BANDS(1):I-1) are all true
C                          Bound: EIF + 1 - I
C
   50                      IF (I.NE.EIF + 1) THEN
                              IF (IFFLGS(I)) THEN
                                 I = I + 1
                                 GO TO 50
                                 END IF
                              END IF
C
C                          Either I = EIF + 1 or IFFLGS(I) is false.
C                          In either case I - 1 is the last band to be
C                          flagged in the next FG table record.
C
                           BANDS(2) = I - 1
C
                           IF (BANDS(1).LE.EIF) THEN
                              DO 60 I = 1,2
                                 BANDS2(I) = BANDS(I) - BIF + 1
   60                            CONTINUE
                              CALL TABFLG ('WRIT', FGBUFF, FGROW,
     *                           FGCOLS, FGNUMV, SOURID, SUBARR, FREQID,
     *                           ANTS, TIMRAN, BANDS2, CHNRAN, PFLAGS,
     *                           REASON(1:24), IRET)
                              IF (IRET.NE.0) THEN
                                 CALL TABERR ('WRIT', 'TABFLG', 'FL2FG',
     *                              IRET)
                                 IRET = 1
                                 END IF
                              END IF
                           BANDS(1) = BANDS(2) + 1
                           GO TO 30
                           END IF
                     ELSE
                        CALL TABERR ('READ', 'TABFL ', 'FL2FG ', IRET)
                        IRET = 1
                        END IF
   70                CONTINUE
C
C                 Close FG table if no errors have been detected
C
                  IF (IRET.EQ.0) THEN
                     CALL TABFLG ('CLOS', FGBUFF, FGROW, FGCOLS,
     *                            FGNUMV, SOURID, SUBARR,FREQID, ANTS,
     *                            TIMRAN, BANDS, CHNRAN, PFLAGS,
     *                            REASON(1:24), IRET)
                     IF (IRET.NE.0) THEN
                        CALL TABERR ('CLOS', 'TABFLG', 'FL2FG ', IRET)
                        IRET = 1
                        END IF
                     END IF
               ELSE
                  CALL TABERR ('WRIT', 'FLGINI', 'FL2FG ', IRET)
                  MSGSAV = MSGSUP
                  MSGSUP = 32000
                  CALL TABFL ('CLOS', FLBUFF, FLROW, FLCOLS, FLNUMV,
     *                        TABREV, NUMIF, SOURID, SUBARR, ANTS,
     *                        FREQID, TIMRAN, IFFLGS, CHNRAN, PFLAGS,
     *                        REASON, SEVRTY, IRET)
                  MSGSUP = MSGSAV
                  IRET = 1
                  END IF
C
C              Close FL table if no errors have been detected:
C
               IF (IRET.EQ.0) THEN
                  CALL TABFL ('CLOS', FLBUFF, FLROW, FLCOLS, FLNUMV,
     *                        TABREV, NUMIF, SOURID, SUBARR, ANTS,
     *                        FREQID, TIMRAN, IFFLGS, CHNRAN, PFLAGS,
     *                        REASON, SEVRTY, IRET)
                  IF (IRET.NE.0) THEN
                     CALL TABERR ('CLOS', 'TABFL ', 'FL2FG ', IRET)
                     IRET = 1
                  END IF
               END IF
C
C              Delete converted FL table if no errors were detected:
C
               IF ((IRET.EQ.0) .AND. (DELEX)) THEN
                  CALL RMEXT (DISK, CNO, 'FL', VER, CATBLK, FLBUFF,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     CALL TABERR ('ZAP ', 'RMEXT ', 'FL2FG ', IRET)
                     IRET = 1
                     END IF
                  END IF
            ELSE
               CALL TABERR ('READ', 'FLINI ', 'FL2FG ', IRET)
               IRET = 1
               END IF
C
C           Force closure of the FL table if any errors were detected
C
            IF (IRET.NE.0) THEN
               MSGSAV = MSGSUP
               MSGSUP = 32000
               CALL TABFL ('CLOS', FLBUFF, FLROW, FLCOLS, FLNUMV,
     *            TABREV, NUMIF, SOURID, SUBARR, ANTS, FREQID, TIMRAN,
     *            IFFLGS, CHNRAN, PFLAGS, REASON, SEVRTY, JRET)
               MSGSUP = MSGSAV
               END IF
            END IF
 80      CONTINUE
      END
      SUBROUTINE GN2GC (DISK, CNO, BIF, EIF, CATBLK, IRET)
C-----------------------------------------------------------------------
C   Apply IF selection to gain curve data from FITS interferometry data
C   data interchange gain curve (GN) tables and append the selected
C   data records to GC table 1, creating it if it does not already
C   exist. Delete each GN table after it is converted.
C
C   Inputs:
C      DISK     I        Volume number of parent data file
C      CNO      I        Catalogue number of parent data file
C      BIF      I        First selected IF
C      EIF      I        Last selected IF
C
C   Input/output:
C      CATBLK   I(256)   Catalogue header for parent data file (updated)
C
C   Output:
C      IRET     I        Return code:
C                           0 - tables converted
C                           1 - I/O error detected
C
C   Uses LUNs 40 and 41.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PGNV.INC'
C
      INTEGER   DISK, CNO, BIF, EIF, CATBLK(256), IRET
C
C     Local variables:
C
C     HIVER     Highest GN table version number
C     VER       Current GN table version number
C     EXIST     Does GN table exist?
C     TABLE     Is GN table a true table?
C     FITASC    Can GN table be written as an ASCII table?
C     TABREV    GN table revision number
C     NUMIF     Number of bands in GN table
C     NUMPOL    Number of polarizations in GN table
C     NTABS     Number of tabulated values in GN table
C     ROW       Row number
C     ANTNUM    Antenna number
C     SUBARR    Subarray number
C     FREQID    Frequency ID number
C     TYPE      Gain curve types
C     NTERM     Numbers of terms
C     XTYPE     X value types
C     YTYPE     Y value types
C     XVALUE    X values
C     YVALUE    Y values
C     GAIN      Gains
C     SENS      Sensitivities
C     TYPES     Gain curve types after IF selection
C     NTERMS    Numbers of terms after IF selection
C     XTYPES    X value types after IF selection
C     YTYPES    Y value types after IF selection
C     XVALUS    X values after IF selection
C     YVALUT    Transposed Y values
C     GAINT     Transposed gain matrix
C     SENSS     Sensitivities after IF selection
C     GNROW     Current row in GN table
C     GNLUN     LUN for GN tables
C     GNBUFF    I/O control structure and buffer for GN tables
C     GNCOLS    Indices for GN table columns
C     GNNUMV    Column dimensions for GN tables
C     GCROW     Current row in GC table
C     GCLUN     LUN for GC tables
C     GCBUFF    I/O control structure and buffer for GC tables
C     GCCOLS    Indices for GC table columns
C     GCNUMV    Column dimensions for GC tables
C     GCVER     GC table version number
C     MSGSAV    Saved message suppression level
C     F         Destination IF number
C     I         Loop index
C     J         Loop index
C     K         Loop index
C     FNAME     File name of GN table
C     JRET      Temporary error indicator
C
      INTEGER   HIVER, VER
      LOGICAL   EXIST, TABLE, FITASC, ISONE
      INTEGER   TABREV, NUMIF, NUMPOL, NTABS
      INTEGER   ROW, ANTNUM, SUBARR, FREQID, TYPE(2, MAXIF),
     *          NTERM(2, MAXIF), XTYPE(2, MAXIF), YTYPE(2, MAXIF),
     *          TYPES(2, MAXIF), NTERMS(2, MAXIF), XTYPES(2, MAXIF),
     *          YTYPES(2, MAXIF)
      REAL      XVALUE(2, MAXIF), YVALUE(2, MXTBGN, MAXIF),
     *          GAIN(2, MXTBGN, MAXIF), SENS(2, MAXIF),
     *          YVALUT(2, MAXIF, MXTBGC), GAINT(2, MAXIF, MXTBGC),
     *          XVALUS(2, MAXIF), SENSS(2, MAXIF)
      INTEGER   GNROW, GNLUN, GNBUFF(512), GNCOLS(MAXGNC),
     *          GNNUMV(MAXGNC)
      INTEGER   GCROW, GCLUN, GCBUFF(512), GCCOLS(MAXGCC),
     *          GCNUMV(MAXGCC), GCVER
      INTEGER   MSGSAV, F, I, J, K, JRET, MSG
C
      PARAMETER (GNLUN = 40)
      PARAMETER (GCLUN = 41)
C
      INTEGER   ANTFNC, FQIFNC
      EXTERNAL  ANTFNC, FQIFNC
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'ORDER.INC'
C-----------------------------------------------------------------------
      CALL FNDEXT ('GN', CATBLK, HIVER)
      MSG = 5
C
C     Process each table in turn:
C
      DO 50 VER = 1, HIVER
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL ISTAB ('GN', DISK, CNO, VER, GNLUN, GNBUFF, TABLE, EXIST,
     *      FITASC, IRET)
         IRET = 0
         MSGSUP = MSGSAV
         IF (EXIST) THEN
C                                       Blank-fill arrays
            CALL FILL (2 * MAXIF, 0, TYPE)
            CALL FILL (2 * MAXIF, 0, NTERM)
            CALL FILL (2 * MAXIF, 0, XTYPE)
            CALL FILL (2 * MAXIF, 0, YTYPE)
            CALL RFILL (2 * MAXIF, FBLANK, XVALUE)
            CALL RFILL (2 * MAXIF * MXTBGC, FBLANK, YVALUE)
            CALL RFILL (2 * MAXIF * MXTBGC, FBLANK, GAIN)
            CALL RFILL (2 * MAXIF, FBLANK, SENS)
            CALL FILL (2 * MAXIF, 0, TYPES)
            CALL FILL (2 * MAXIF, 0, NTERMS)
            CALL FILL (2 * MAXIF, 0, XTYPES)
            CALL FILL (2 * MAXIF, 0, YTYPES)
            CALL RFILL (2 * MAXIF, FBLANK, XVALUS)
            CALL RFILL (2 * MAXIF * MXTBGC, FBLANK, YVALUT)
            CALL RFILL (2 * MAXIF * MXTBGC, FBLANK, GAINT)
            CALL RFILL (2 * MAXIF, FBLANK, SENSS)
C
            CALL GNINI ('READ', GNBUFF, DISK, CNO, VER, CATBLK, GNLUN,
     *         GNROW, GNCOLS, GNNUMV, NUMPOL, NUMIF, NTABS, TABREV,
     *         IRET)
            IF (IRET.EQ.0) THEN
               GCVER = 1
               NUMIF = EIF - BIF + 1
               CALL GCINI ('WRIT', GCBUFF, DISK, CNO, GCVER, CATBLK,
     *            GCLUN, GCROW, GCCOLS, GCNUMV, NUMPOL, NUMIF, NTABS,
     *            IRET)
               IF (IRET.EQ.0) THEN
                  DO 40 ROW = 1,GNBUFF(5)
                     GNROW = ROW
                     CALL TABGN ('READ', GNBUFF, GNROW, GNCOLS, GNNUMV,
     *                  NUMPOL, ANTNUM, SUBARR, FREQID, TYPE, NTERM,
     *                  XTYPE, YTYPE, XVALUE, YVALUE, GAIN, SENS, IRET)
                     IF (IRET.EQ.0) THEN
C                                       Remap antenna and frequency IDs
                        SUBARR = MAX (1, SUBARR)
                        IF (DOAMAP) THEN
                           IF (ANTNUM.NE.0) ANTNUM = ANTFNC (ANTNUM,
     *                        SUBARR)
                           END IF
                        IF (PHASED.LT.0) THEN
                           ISONE = ANTNUM.EQ.(-PHASED)
                        ELSE
                           ISONE = .FALSE.
                           END IF
C
C                       Select IFs and transpose YVALUE and GAIN:
C                       (This is not expected to be a performance
C                       bottleneck so we keep it simple)
C
                        DO 30 I = 1, NUMPOL
                           DO 20 J = BIF, EIF
                              F = FORDER(J - BIF + 1, FREQID)
                              IF (F.LE.0) THEN
                                 IF (MSG.GT.0) THEN
                                    WRITE (MSGTXT,1000) K, FREQID
                                    CALL MSGWRT (6)
                                    MSG = MSG - 1
                                    END IF
                                 F = K - BIF + 1
                                 END IF
                              TYPES(I, F)  = TYPE(I, J)
                              NTERMS(I, F) = NTERM(I, J)
                              XTYPES(I, F) = XTYPE(I, J)
                              YTYPES(I, F) = YTYPE(I, J)
                              XVALUS(I, F) = XVALUE(I, J)
                              DO 10 K = 1,MXTBGN
                                 YVALUT(I, F, K) = YVALUE(I, K, J)
                                 GAINT(I, F, K) = GAIN(I,K,J)
 10                              CONTINUE
                              SENSS(I, F) = SENS(I, J)
                              IF (ISONE) THEN
                                 IF (SENSS(I,F).NE.FBLANK) ISONE =
     *                              ABS (SENSS(I,F)-1.0).LT.0.001
                                 END IF
 20                           CONTINUE
 30                        CONTINUE
                        IF (FREQID.GT.0) THEN
                           IF (FQDUPS(FREQID).LT.FREQID) GO TO 40
                           FREQID = FQDUPS(FREQID)
                           FREQID = FQIFNC (FREQID)
                           IF (FREQID.LE.0) GO TO 40
                           END IF
                        CALL TABGC ('WRIT', GCBUFF, GCROW, GCCOLS,
     *                     GCNUMV, NUMPOL, NTABS, ANTNUM, SUBARR,
     *                     FREQID, TYPES, NTERMS, XTYPES, YTYPES,
     *                     XVALUS, YVALUT, GAINT, SENSS, IRET)
                        IF (IRET.NE.0) THEN
                           CALL TABERR ('WRIT', 'TABGC ', 'GN2GC ',
     *                        IRET)
                           IRET = 1
                           END IF
                        IF (ISONE) PHASED = -PHASED
                     ELSE
                        CALL TABERR ('READ', 'TABGN ', 'GN2GC ', IRET)
                        IRET = 1
                        END IF
 40                  CONTINUE
C
C                 Close GC table if no errors have been detected
C
                  IF (IRET.EQ.0) THEN
                     CALL TABGC ('CLOS', GCBUFF, GCROW, GCCOLS, GCNUMV,
     *                           NUMPOL, NTABS, ANTNUM, SUBARR, FREQID,
     *                           TYPES, NTERMS, XTYPES, YTYPES, XVALUS,
     *                           YVALUT, GAINT, SENSS, IRET)
                     IF (IRET.NE.0) THEN
                        CALL TABERR ('CLOS', 'TABGC ', 'GN2GC ', IRET)
                        IRET = 1
                     END IF
                  END IF
               ELSE
                  CALL TABERR ('WRIT', 'GCINI ', 'GN2GC ', IRET)
                  MSGSAV = MSGSUP
                  MSGSUP = 32000
                  CALL TABGC ('CLOS', GCBUFF, GCROW, GCCOLS, GCNUMV,
     *                        TABREV, NUMPOL, ANTNUM, SUBARR, FREQID,
     *                        TYPE, NTERM, XTYPE, YTYPE, XVALUE,
     *                        YVALUE, GAIN, SENS, IRET)
                  MSGSUP = MSGSAV
                  IRET = 1
                  END IF
C
C              Close GN table if no errors have been detected:
C
               IF (IRET.EQ.0) THEN
                  CALL TABGN ('CLOS', GNBUFF, GNROW, GNCOLS, GNNUMV,
     *               NUMPOL, ANTNUM, SUBARR, FREQID, TYPE, NTERM, XTYPE,
     *               YTYPE, XVALUE, YVALUE, GAIN, SENS, IRET)
                  IF (IRET.NE.0) THEN
                     CALL TABERR ('CLOS', 'TABGN ', 'GN2GC ', IRET)
                     IRET = 1
                     END IF
                  END IF
C
C              Delete converted GN table if no errors were detected:
C
               IF ((IRET.EQ.0) .AND. (DELEX)) THEN
                  CALL RMEXT (DISK, CNO, 'GN', VER, CATBLK, GNBUFF,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     CALL TABERR ('ZAP ', 'RMEXT ', 'GN2GC ', IRET)
                     IRET = 1
                     END IF
                  END IF
            ELSE
               CALL TABERR ('READ', 'GNINI ', 'GN2GC ', IRET)
               IRET = 1
               END IF
C
C           Force closure of the GN table if any errors were detected
C
            IF (IRET.NE.0) THEN
               MSGSAV = MSGSUP
               MSGSUP = 32000
               CALL TABGN ('CLOS', GNBUFF, GNROW, GNCOLS, GNNUMV,
     *            NUMPOL, ANTNUM, SUBARR, FREQID, TYPE, NTERM, XTYPE,
     *            YTYPE, XVALUE, YVALUE, GAIN, SENS, JRET)
               MSGSUP = MSGSAV
               END IF
            END IF
 50      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GN2GC: IF',I3,' FREQID',I3,' UNKNOWN')
      END
      SUBROUTINE PH2PC (DISK, CNO, BIF, EIF, CATBLK, IRET)
C-----------------------------------------------------------------------
C   Apply IF selection to phase-cal data in FITS interferometry data
C   interchange phase-cal (PH) tables and append the selected data to
C   PC table 1, creating it if it does not already exist. Delete each PH
C   table after it is converted.
C
C   Inputs:
C      DISK     I        Volume number of parent data file
C      CNO      I        Catalogue number of parent data file
C      BIF      I        First selected IF
C      EIF      I        Last selected IF
C
C   Input/output:
C      CATBLK   I(256)   Catalogue header for parent data file (updated)
C
C   Output:
C      IRET     I        Return code:
C                           0 - tables converted
C                           1 - I/O error detected
C
C   Uses LUNs 40 and 41.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PPHV.INC'
C
      INTEGER   DISK, CNO, BIF, EIF, CATBLK(256), IRET
C
C     Local variables:
C
C     HIVER     Highest PH table version number
C     VER       Current PH table version number
C     EXIST     Does PH table exist?
C     TABLE     Is PH table a true table?
C     FITASC    Can PH table be written as an ASCII table?
C     TABREV    PH table revision number
C     NUMIF     Number of bands in PH table
C     NUMPOL    Number of polarizations in PH table
C     NUMTON    Number of phase cal tones in PH table
C     ROW       Row number
C     TIME      Time of PH table record in days
C     TIMINT    Time interval covered by PH table record in days
C     SOURCE    Source ID
C     ANTNUM    Antenna number
C     SUBARR    Subarray number
C     FREQID    Frequency ID number
C     CABCAL    Cable cal delay measurement in sec
C     STATE     Percentage of time in each state
C     PCFREQ    Frequencies of phase-cal tones
C     PCREAL    Real components of phase-cal measurements
C     PCIMAG    Imaginary components of phase-cal measurements
C     PCRATE    Phase rates of phase-cal measurements
C     STATES    Percentage of time in each state after IF selection
C     PCFRQS    Frequencies of phase-cal tones after IF selection
C     PCRLS     Real components of phase-cal measurements after IF
C                  selection
C     PCIMS     Imaginary components of phase-cal measurements after IF
C                  selection
C     PCRTS     Phase rates of phase-cal measurements after IF selection
C     PHROW     Current row in PH table
C     PHLUN     LUN for PH tables
C     PHBUFF    I/O control structure and buffer for PH tables
C     PHCOLS    Indices for PH table columns
C     PHNUMV    Column dimensions for PH tables
C     PCROW     Current row in PC table
C     PCLUN     LUN for PC tables
C     PCBUFF    I/O control structure and buffer for PC tables
C     PCCOLS    Indices for PC table columns
C     PCNUMV    Column dimensions for PC tables
C     PCVER     PC table version number
C     MSGSAV    Saved message suppression level
C     F         Destination IF number
C     I         Loop index
C     J         Loop index
C     K         Loop index
C     JRET      Temporary error indicator
C
      INTEGER   HIVER, VER, MSG
      LOGICAL   EXIST, TABLE, FITASC
      INTEGER   TABREV, NUMIF, NUMPOL, NUMTON
      INTEGER   ROW, SOURCE, ANTNUM, SUBARR, FREQID
      DOUBLE PRECISION TIME, CABCAL, PCFREQ(2, MAXTON, MAXIF),
     *                 PCFRQS(2, MAXTON, MAXIF)
      REAL      TIMINT, STATE(2, 4, MAXIF), PCREAL(2, MAXTON, MAXIF),
     *          PCIMAG(2, MAXTON, MAXIF), PCRATE(2, MAXTON, MAXIF),
     *          STATES(2, 4, MAXIF), PCRLS(2, MAXTON, MAXIF),
     *          PCIMS(2, MAXTON, MAXIF), PCRTS(2, MAXTON, MAXIF)
      INTEGER   PHROW, PHLUN, PHBUFF(512), PHCOLS(MAXPHC),
     *          PHNUMV(MAXPHC)
      INTEGER   PCROW, PCLUN, PCBUFF(512), PCCOLS(MAXPCC),
     *          PCNUMV(MAXPCC), PCVER
      INTEGER   MSGSAV, F, I, J, K, JRET
C
      PARAMETER (PHLUN = 40)
      PARAMETER (PCLUN = 41)
C
      INTEGER   ANTFNC, FQIFNC, SRCFNC
      EXTERNAL  ANTFNC, FQIFNC, SRCFNC
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'ORDER.INC'
C-----------------------------------------------------------------------
      MSG = 5
      CALL FNDEXT ('PH', CATBLK, HIVER)
C
C     Process each table in turn:
C
      DO 60 VER = 1, HIVER
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL ISTAB ('PH', DISK, CNO, VER, PHLUN, PHBUFF, TABLE, EXIST,
     *               FITASC, IRET)
         IRET = 0
         MSGSUP = MSGSAV
         IF (EXIST) THEN
C
C           Blank-fill arrays
C
            CALL RFILL (2 * 4 * MAXIF, FBLANK, STATE)
            CALL DFILL (2 * MAXTON * MAXIF, DBLANK, PCFREQ)
            CALL RFILL (2 * MAXTON * MAXIF, FBLANK, PCREAL)
            CALL RFILL (2 * MAXTON * MAXIF, FBLANK, PCIMAG)
            CALL RFILL (2 * MAXTON * MAXIF, FBLANK, PCRATE)
            CALL RFILL (2 * 4 * MAXIF, FBLANK, STATES)
            CALL DFILL (2 * MAXTON * MAXIF, DBLANK, PCFRQS)
            CALL RFILL (2 * MAXTON * MAXIF, FBLANK, PCRLS)
            CALL RFILL (2 * MAXTON * MAXIF, FBLANK, PCIMS)
            CALL RFILL (2 * MAXTON * MAXIF, FBLANK, PCRTS)
C
            CALL PHINI ('READ', PHBUFF, DISK, CNO, VER, CATBLK, PHLUN,
     *                  PHROW, PHCOLS, PHNUMV, NUMPOL, NUMIF, NUMTON,
     *                  TABREV, IRET)
            IF (IRET.EQ.0) THEN
               PCVER = 1
               NUMIF = EIF - BIF + 1
               CALL PCINI ('WRIT', PCBUFF, DISK, CNO, PCVER, CATBLK,
     *                      PCLUN, PCROW, PCCOLS, PCNUMV, NUMPOL,
     *                      NUMIF, NUMTON, IRET)
               IF (IRET.EQ.0) THEN
                  DO 50 ROW = 1, PHBUFF(5)
                     PHROW = ROW
                     CALL TABPH ('READ', PHBUFF, PHROW, PHCOLS, PHNUMV,
     *                  NUMPOL, TIME, TIMINT, SOURCE, ANTNUM, SUBARR,
     *                  FREQID, CABCAL, STATE, PCFREQ, PCREAL, PCIMAG,
     *                  PCRATE, IRET)
                     IF (IRET.EQ.0) THEN
C
C                       Remap antenna and frequency IDs:
C
                        SUBARR = MAX (1, SUBARR)
                        IF (DOAMAP) THEN
                           IF (ANTNUM.NE.0) THEN
                              ANTNUM = ANTFNC(ANTNUM, SUBARR)
                           END IF
                        END IF
                        IF (DOSMAP) THEN
                           SOURCE = SRCFNC(SOURCE)
                        END IF
C
C                       Select IFs
C
                        DO 40 I = 1, NUMPOL
                           DO 30 K = BIF, EIF
                              F = FORDER(K - BIF + 1, FREQID)
                              IF (F.LE.0) THEN
                                 IF (MSG.GT.0) THEN
                                    WRITE (MSGTXT,1000) K, FREQID
                                    CALL MSGWRT (6)
                                    MSG = MSG - 1
                                    END IF
                                 F = K - BIF + 1
                                 END IF
                              DO 10 J = 1, 4
                                 STATES(I, J, F)
     *                              = STATE(I, J, K)
   10                         CONTINUE
                              DO 20 J = 1, NUMTON
                                 PCFRQS(I, J, F)
     *                              = PCFREQ(I, J, K)
                                 PCRLS(I, J, F)
     *                              = PCREAL(I, J, K)
                                 PCIMS(I, J, F)
     *                              = PCIMAG(I, J, K)
                                 PCRTS(I, J, F)
     *                              = PCRATE(I, J, K)
   20                         CONTINUE
   30                      CONTINUE
   40                   CONTINUE
                        IF (FREQID.GT.0) THEN
                           FREQID = FQDUPS(FREQID)
                           FREQID = FQIFNC (FREQID)
                           IF (FREQID.LE.0) GO TO 50
                           END IF
                        CALL TABPC ('WRIT', PCBUFF, PCROW, PCCOLS,
     *                              PCNUMV, NUMPOL, TIME, TIMINT,
     *                              SOURCE, ANTNUM, SUBARR, FREQID,
     *                              CABCAL, STATES, PCFRQS, PCRLS,
     *                              PCIMS, PCRTS, IRET)
                        IF (IRET.NE.0) THEN
                           CALL TABERR ('WRIT', 'TABPC ', 'PH2PC ',
     *                                  IRET)
                           IRET = 1
                        END IF
                     ELSE
                        CALL TABERR ('READ', 'TABPH ', 'PH2PC ', IRET)
                        IRET = 1
                     END IF
   50             CONTINUE
C
C                 Close PC table if no errors have been detected
C
                  IF (IRET.EQ.0) THEN
                     CALL TABPC ('CLOS', PCBUFF, PCROW, PCCOLS, PCNUMV,
     *                           NUMPOL, TIME, TIMINT, SOURCE, ANTNUM,
     *                           SUBARR, FREQID, CABCAL, STATES, PCFRQS,
     *                           PCRLS, PCIMS, PCRTS, IRET)
                     IF (IRET.NE.0) THEN
                        CALL TABERR ('CLOS', 'TABPC ', 'PH2PC ', IRET)
                        IRET = 1
                     END IF
                  END IF
               ELSE
                  CALL TABERR ('WRIT', 'PCINI ', 'PH2PC ', IRET)
                  MSGSAV = MSGSUP
                  MSGSUP = 32000
                  CALL TABPC ('CLOS', PCBUFF, PCROW, PCCOLS, PHNUMV,
     *                        NUMPOL, TIME, TIMINT, SOURCE, ANTNUM,
     *                        SUBARR, FREQID, CABCAL, STATE, PCFREQ,
     *                        PCREAL, PCIMAG, PCRATE, IRET)
                  MSGSUP = MSGSAV
                  IRET = 1
               END IF
C
C              Close PH table if no errors have been detected:
C
               IF (IRET.EQ.0) THEN
                  CALL TABPH ('CLOS', PHBUFF, PHROW, PHCOLS, PHNUMV,
     *               NUMPOL, TIME, TIMINT, SOURCE, ANTNUM, SUBARR,
     *               FREQID, CABCAL, STATE, PCFREQ, PCREAL, PCIMAG,
     *               PCRATE, IRET)
                  IF (IRET.NE.0) THEN
                     CALL TABERR ('CLOS', 'TABPH ', 'PH2PC ', IRET)
                     IRET = 1
                     END IF
                  END IF
C
C              Delete converted PH table if no errors were detected:
C
               IF ((IRET.EQ.0) .AND. (DELEX)) THEN
                  CALL RMEXT (DISK, CNO, 'PH', VER, CATBLK, PHBUFF,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     CALL TABERR ('ZAP ', 'RMEXT ', 'PH2PC ', IRET)
                     IRET = 1
                     END IF
                  END IF
            ELSE
               CALL TABERR ('READ', 'PHINI ', 'PH2PC ', IRET)
               IRET = 1
               END IF
C
C           Force closure of the PH table if any errors were detected
C
            IF (IRET.NE.0) THEN
               MSGSAV = MSGSUP
               MSGSUP = 32000
               CALL TABPH ('CLOS', PHBUFF, PHROW, PHCOLS, PHNUMV,
     *            NUMPOL, TIME, TIMINT, SOURCE, ANTNUM, SUBARR, FREQID,
     *            CABCAL, STATE, PCFREQ, PCREAL, PCIMAG, PCRATE, JRET)
               MSGSUP = MSGSAV
               END IF
            END IF
 60      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PH2PC: IF',I3,' FREQID',I3,' UNKNOWN')
      END
      SUBROUTINE TS2TY (DISK, CNO, BIF, EIF, CATBLK, IRET)
C-----------------------------------------------------------------------
C   Apply IF selection to data from FITS interferometry data interchange
C   system temperature (TS) tables and append the selecetd data to TY
C   table 1, creating it if it does not already exist. Delete each TS
C   table after it is converted.
C
C   Inputs:
C      DISK     I        Volume number of parent data file
C      CNO      I        Catalogue number of parent data file
C      BIF      I        First selected IF
C      EIF      I        Last selected IF
C
C   Input/output:
C      CATBLK   I(256)   Catalogue header for parent data file (updated)
C
C   Output:
C      IRET     I        Return code:
C                           0 - tables converted
C                           1 - I/O error detected
C
C   Uses LUNs 40 and 41.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTSV.INC'
C
      INTEGER   DISK, CNO, BIF, EIF, CATBLK(256), IRET
C
C     Local variables:
C
C     HIVER     Highest TS table version number
C     VER       Current TS table version number
C     EXIST     Does TS table exist?
C     TABLE     Is TS table a true table?
C     FITASC    Can TS table be written as an ASCII table?
C     TABREV    TS table revision number
C     NUMIF     Number of bands in TS table
C     NUMPOL    Number of polarizations in TS table
C     ROW       Row number
C     TIME      Time of TS table record in days
C     TIMINT    Time interval covered by TS table record in days
C     SOURCE    Source ID
C     ANTNUM    Antenna number
C     SUBARR    Subarray number
C     FREQID    Frequency ID number
C     TSYS      System temperatures before IF selection
C     TANT      Antenna temperatures before IF selection
C     TSYSS     System temperatures after IF selection
C     TANTS     Antenna temperatures after IF selection
C     TSROW     Current row in TS table
C     TSLUN     LUN for TS tables
C     TSBUFF    I/O control structure and buffer for TS tables
C     TSCOLS    Indices for TS table columns
C     TSNUMV    Column dimensions for TS tables
C     TYROW     Current row in TY table
C     TYLUN     LUN for TY tables
C     TYBUFF    I/O control structure and buffer for TY tables
C     TYCOLS    Indices for TY table columns
C     TYNUMV    Column dimensions for TY tables
C     TYVER     TY table version number
C     MSGSAV    Saved message suppression level
C     F         Destination IF number
C     I         Loop index
C     J         Loop index
C     JRET      Temporary error indicator
C
      INTEGER   HIVER, VER, MSG
      LOGICAL   EXIST, TABLE, FITASC, CHANGE
      INTEGER   TABREV, NUMIF, NUMPOL
      INTEGER   ROW, SOURCE, ANTNUM, SUBARR, FREQID
      DOUBLE PRECISION TIME
      REAL      TIMINT, TSYS(2, MAXIF), TANT(2, MAXIF), TSYSS(2, MAXIF),
     *          TANTS(2, MAXIF)
      INTEGER   TSROW, TSLUN, TSBUFF(512), TSCOLS(MAXTSC),
     *          TSNUMV(MAXTSC)
      INTEGER   TYROW, TYLUN, TYBUFF(512), TYCOLS(MAXTYC),
     *          TYNUMV(MAXTYC), TYVER
      INTEGER   MSGSAV, F, I, J, JRET
C
      PARAMETER (TSLUN = 40)
      PARAMETER (TYLUN = 41)
C
      INTEGER   ANTFNC, FQIFNC, SRCFNC
      EXTERNAL  ANTFNC, FQIFNC, SRCFNC
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'ORDER.INC'
C-----------------------------------------------------------------------
      MSG = 5
      CALL FNDEXT ('TS', CATBLK, HIVER)
C                                       Process each table in turn:
      DO 60 VER = 1, HIVER
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL ISTAB ('TS', DISK, CNO, VER, TSLUN, TSBUFF, TABLE, EXIST,
     *               FITASC, IRET)
         IRET = 0
         MSGSUP = MSGSAV
         IF (EXIST) THEN
C                                       Blank-fill arrays
            CALL RFILL (2 * MAXIF, FBLANK, TSYS)
            CALL RFILL (2 * MAXIF, FBLANK, TANT)
            CALL RFILL (2 * MAXIF, FBLANK, TSYSS)
            CALL RFILL (2 * MAXIF, FBLANK, TANTS)
C
            CALL TSINI ('READ', TSBUFF, DISK, CNO, VER, CATBLK, TSLUN,
     *         TSROW, TSCOLS, TSNUMV, NUMPOL, NUMIF, TABREV, IRET)
            IF (IRET.EQ.0) THEN
               TYVER = 1
               NUMIF = EIF - BIF + 1
               CALL TYINI ('WRIT', TYBUFF, DISK, CNO, TYVER, CATBLK,
     *            TYLUN, TYROW, TYCOLS, TYNUMV, NUMPOL, NUMIF, IRET)
               IF (IRET.EQ.0) THEN
                  DO 50 ROW = 1,TSBUFF(5)
                     TSROW = ROW
                     CALL TABTS ('READ', TSBUFF, TSROW, TSCOLS, TSNUMV,
     *                  NUMPOL, TIME, TIMINT, SOURCE, ANTNUM, SUBARR,
     *                  FREQID, TSYS, TANT, IRET)
                     IF (IRET.EQ.0) THEN
C                                       Remap antenna and frequency IDs
                        SUBARR = MAX (1, SUBARR)
                        IF (DOAMAP) THEN
                           IF (ANTNUM.NE.0) ANTNUM = ANTFNC (ANTNUM,
     *                        SUBARR)
                           END IF
                        IF (DOSMAP) SOURCE = SRCFNC(SOURCE)
                        CHANGE = PHASED.EQ.ANTNUM
C                                       Select IFs
                        DO 20 I = 1, NUMPOL
                           DO 10 J = BIF, EIF
                              F = FORDER(J - BIF + 1, FREQID)
                              IF (F.LE.0) THEN
                                 IF (MSG.GT.0) THEN
                                    WRITE (MSGTXT,1000) J, FREQID
                                    CALL MSGWRT (6)
                                    MSG = MSG - 1
                                    END IF
                                 F = J - BIF + 1
                                 END IF
                              TSYSS(I,F) = TSYS(I, J)
C                                       phased VLA is Tsys/Tant
                              IF ((CHANGE) .AND. (TANT(I,J).EQ.FBLANK))
     *                           THEN
                                 TANTS(I,F) = -1.0
                              ELSE
                                 TANTS(I,F) = TANT(I, J)
                                 END IF
 10                           CONTINUE
 20                        CONTINUE
                        IF (FREQID.GT.0) THEN
                           FREQID = FQDUPS(FREQID)
                           FREQID = FQIFNC (FREQID)
                           IF (FREQID.LE.0) GO TO 50
                           END IF
                        CALL TABTY ('WRIT', TYBUFF, TYROW, TYCOLS,
     *                     TYNUMV, NUMPOL, NUMIF, REAL(TIME), TIMINT,
     *                     SOURCE, ANTNUM, SUBARR, FREQID, TSYSS, TANTS,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           CALL TABERR ('WRIT', 'TABTY ', 'TS2TY ',
     *                        IRET)
                           IRET = 1
                           END IF
                     ELSE
                        CALL TABERR ('READ', 'TABTS ', 'TS2TY ', IRET)
                        IRET = 1
                        END IF
 50                  CONTINUE
C
C                 Close TY table if no errors have been detected
C
                  IF (IRET.EQ.0) THEN
                     CALL TABTY ('CLOS', TYBUFF, TYROW, TYCOLS, TYNUMV,
     *                  NUMPOL, NUMIF, TIME, TIMINT, SOURCE, ANTNUM,
     *                  SUBARR, FREQID, TSYSS, TANTS, IRET)
                     IF (IRET.NE.0) THEN
                        CALL TABERR ('CLOS', 'TABTY ', 'TS2TY ', IRET)
                        IRET = 1
                        END IF
                     END IF
               ELSE
                  CALL TABERR ('WRIT', 'TYINI ', 'TS2TY ', IRET)
                  MSGSAV = MSGSUP
                  MSGSUP = 32000
                  CALL TABTY ('CLOS', TYBUFF, TYROW, TYCOLS, TSNUMV,
     *               NUMPOL, NUMIF, TIME, TIMINT, SOURCE, ANTNUM,
     *               SUBARR, FREQID, TSYSS, TANTS, IRET)
                  MSGSUP = MSGSAV
                  IRET = 1
                  END IF
C
C              Close TS table if no errors have been detected:
C
               IF (IRET.EQ.0) THEN
                  CALL TABTS ('CLOS', TSBUFF, TSROW, TSCOLS, TSNUMV,
     *               NUMPOL, TIME, TIMINT, SOURCE, ANTNUM, SUBARR,
     *               FREQID, TSYS, TANT, IRET)
                  IF (IRET.NE.0) THEN
                     CALL TABERR ('CLOS', 'TABTS ', 'TS2TY ', IRET)
                     IRET = 1
                     END IF
                  END IF
C
C              Delete converted TS table if no errors were detected:
C
               IF ((IRET.EQ.0) .AND. (DELEX)) THEN
                  CALL RMEXT (DISK, CNO, 'TS', VER, CATBLK, TSBUFF,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     CALL TABERR ('ZAP ', 'RMEXT ', 'TS2TY ', IRET)
                     IRET = 1
                     END IF
                  END IF
            ELSE
               CALL TABERR ('READ', 'TSINI ', 'TS2TY ', IRET)
               IRET = 1
               END IF
C
C           Force closure of the TS table if any errors were detected
C
            IF (IRET.NE.0) THEN
               MSGSAV = MSGSUP
               MSGSUP = 32000
               CALL TABTS ('CLOS', TSBUFF, TSROW, TSCOLS, TSNUMV,
     *            NUMPOL, TIME, TIMINT, SOURCE, ANTNUM, SUBARR, FREQID,
     *            TSYS, TANT, JRET)
               MSGSUP = MSGSAV
               END IF
            END IF
 60      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TS2TY: IF',I3,' FREQID',I3,' UNKNOWN')
      END
      SUBROUTINE WR2WX (DISK, CNO, CATBLK, IRET)
C-----------------------------------------------------------------------
C   Apply source and antenna renumbering to data from FITS
C   interferometry data interchange weather (WR) tables and append the
C   selected data to WX table 1, creating it if it does not already
C   exist. Delete each WR table after it is converted.
C   Inputs:
C      DISK     I        Volume number of parent data file
C      CNO      I        Catalogue number of parent data file
C   Input/output:
C      CATBLK   I(256)   Catalogue header for parent data file (updated)
C   Output:
C      IRET     I        Return code:
C                           0 - tables converted
C                           1 - I/O error detected
C   Uses LUNs 40 and 41.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(256), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DWXV.INC'
      INCLUDE 'DWRV.INC'
C
      INTEGER   WXBUFF(512), WRBUFF(512), IWXRNO, IWRRNO,
     *   WXKOLS(MAXWXC), WRKOLS(MAXWRC), WXNUMV(MAXWXC), WRNUMV(MAXWRC),
     *   IANT, WXLUN, WRLUN, HIVER, VER, TABREV, ROW, WXVER, ANTFNC,
     *   SUBA
      DOUBLE PRECISION TIME
      REAL      DTIME, TEMP, PRESS, DEWPT, WVEL, WDIR, WGUST, PRECIP,
     *   H2OCOL, IONCOL
      CHARACTER OBSCOD*8, OBSDAT*8
      LOGICAL   EXIST, TABLE, FITASC
C
      PARAMETER (WXLUN = 40)
      PARAMETER (WRLUN = 41)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'DATSEL.INC'
      INCLUDE 'ORDER.INC'
C-----------------------------------------------------------------------
      CALL FNDEXT ('WR', CATBLK, HIVER)
C                                       Process each table in turn:
      SUBA = 1
      DO 60 VER = 1,HIVER
         CALL ISTAB ('WR', DISK, CNO, VER, WRLUN, WRBUFF, TABLE, EXIST,
     *      FITASC, IRET)
         IRET = 0
         IF (EXIST) THEN
C                                       open input WR
            CALL WRINI ('READ', WRBUFF, DISK, CNO, VER, CATBLK, WRLUN,
     *         IWRRNO, WRKOLS, WRNUMV, OBSCOD, OBSDAT, TABREV, IRET)
            IF (IRET.EQ.0) THEN
C                                       open output WX
               WXVER = 1
               CALL WXINI ('WRIT', WXBUFF, DISK, CNO, WXVER, CATBLK,
     *            WXLUN, IWXRNO, WXKOLS, WXNUMV, OBSCOD, OBSDAT, TABREV,
     *            IRET)
               IF (IRET.EQ.0) THEN
                  DO 50 ROW = 1,WRBUFF(5)
                     IWRRNO = ROW
                     CALL TABWR ('READ', WRBUFF, IWRRNO, WRKOLS, WRNUMV,
     *                  TIME, DTIME, IANT, TEMP, PRESS, DEWPT, WVEL,
     *                  WDIR, WGUST, PRECIP, H2OCOL, IONCOL, IRET)
                     IF (IRET.EQ.0) THEN
C                                       Remap antenna ID
                        IF (DOAMAP) THEN
                           IF (IANT.NE.0) IANT = ANTFNC(IANT,SUBA)
                           END IF
                        CALL TABWX ('WRIT', WXBUFF, IWXRNO, WXKOLS,
     *                     WXNUMV, TIME, DTIME, IANT, SUBA, TEMP, PRESS,
     *                     DEWPT, WVEL, WDIR, WGUST, PRECIP, H2OCOL,
     *                     IONCOL, IRET)
                        IF (IRET.NE.0) THEN
                           CALL TABERR ('WRIT', 'TABWX ', 'WRS2WX ',
     *                        IRET)
                           IRET = 1
                           END IF
                     ELSE
                        CALL TABERR ('READ', 'TABWX ', 'WT2WX ', IRET)
                        IRET = 1
                        END IF
 50                  CONTINUE
C                                       Close WX table if no errors
                  IF (IRET.EQ.0) THEN
                     CALL TABWX ('CLOS', WXBUFF, IWXRNO, WXKOLS, WXNUMV,
     *                  TIME, DTIME, IANT, SUBA, TEMP, PRESS, DEWPT,
     *                  WVEL, WDIR, WGUST, PRECIP, H2OCOL, IONCOL, IRET)
                     IF (IRET.NE.0) THEN
                        CALL TABERR ('CLOS', 'TABWX ', 'WR2WX ', IRET)
                        IRET = 1
                        END IF
                     END IF
               ELSE
                  CALL TABERR ('WRIT', 'WXINI ', 'WR2WX ', IRET)
                  CALL TABWX ('CLOS', WXBUFF, IWXRNO, WXKOLS, WXNUMV,
     *               TIME, DTIME, IANT, SUBA, TEMP, PRESS, DEWPT, WVEL,
     *               WDIR, WGUST, PRECIP,  H2OCOL, IONCOL, IRET)
                  IRET = 1
                  END IF
C                                       Close WR table if no errors
               IF (IRET.EQ.0) THEN
                  CALL TABWR ('CLOS', WRBUFF, IWRRNO, WRKOLS, WRNUMV,
     *               TIME, DTIME, IANT, TEMP, PRESS, DEWPT, WVEL,
     *               WDIR, WGUST, PRECIP, H2OCOL, IONCOL, IRET)
                  IF (IRET.NE.0) THEN
                     CALL TABERR ('CLOS', 'TABWR ', 'WR2WX ', IRET)
                     IRET = 1
                     END IF
                  END IF
C                                       Delete converted WR table if no
C                                       errors
               IF ((IRET.EQ.0) .AND. (DELEX)) THEN
                  CALL RMEXT (DISK, CNO, 'WR', VER, CATBLK, WRBUFF,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     CALL TABERR ('ZAP ', 'RMEXT ', 'WR2WX ', IRET)
                     IRET = 1
                     END IF
                  END IF
            ELSE
               CALL TABERR ('READ', 'TSINI ', 'TS2TY ', IRET)
               IRET = 1
               END IF
C                                       Force closure of the WR table if
C                                       any errors were detected
            IF (IRET.NE.0) CALL TABWR ('CLOS', WRBUFF, IWRRNO, WRKOLS,
     *         WRNUMV, TIME, DTIME, IANT, TEMP, PRESS, DEWPT, WVEL,
     *         WDIR, WGUST, PRECIP, H2OCOL, IONCOL, IANT)
            END IF
 60      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE KEYCHK (NXKEY, XKEY, XTYPE, NUMKEY, KEYWRD, KEYVAL,
     *   KEYTYP, IRET)
C-----------------------------------------------------------------------
C   Check the types of the keywords listed in KEYWRD against those
C   listed in XKEYS and correct the types if possible.
C   Input:
C      NXKEY      I       Number of keywords to check against
C      XKEY       C(*)*8  List of keywords to check against
C      XTYPE      I(*)    Type codes for keywords in XKEY
C      NUMKEY     I       Number of keywords to check
C      KEYWRD     C(*)*8  List of keywords to check
C
C   Input/Output:
C      KEYVAL     D(*)    Numeric keyword values
C      KEYTYP     I       Keyword type codes
C
C   Output:
C      IRET       I       Return status:
C                           0 - keywords OK or corrected
C                           1 - unrecoverable type (eg. string
C                               instead of number)
C-----------------------------------------------------------------------
      INTEGER   NXKEY, XTYPE(*), NUMKEY, KEYTYP(*), IRET
      CHARACTER XKEY(*)*8, KEYWRD(*)*8
      DOUBLE PRECISION KEYVAL(*)
C
C     Local variables
C
C     CURKEY    Current keyword index in KEYWRD
C     KEYIND    Keyword index in XKEY or -1
C     RULE      Table of conversion rules.
C     TNAME     Type names used in messages
C     I         Counter
C
      CHARACTER TNAME(5)*10
      INTEGER   CURKEY, KEYIND, RULE(5,5), I
      INCLUDE 'INCS:DMSG.INC'
      DATA RULE / 2, 3, 1, 5, 1,
     *            3, 2, 1, 5, 1,
     *            1, 1, 2, 1, 1,
     *            4, 4, 1, 2, 1,
     *            1, 1, 1, 1, 2 /
      DATA TNAME /'DOUBLE', 'FLOAT', 'INTEGER', 'CHARACTER', 'LOGICAL' /
C-----------------------------------------------------------------------
      IRET = 0
      DO 200 CURKEY = 1, NUMKEY
         KEYIND = -1
         DO 10 I = 1, NXKEY
            IF (KEYWRD(CURKEY).EQ.(XKEY(I))) KEYIND = I
 10         CONTINUE
C
C        Check the keyword type if it is one that we recognize:
C
         IF (KEYIND.GT.0) THEN
C
C           Select conversion rule based on required and actual type
C
            GO TO (110, 120, 130, 140, 150) RULE(XTYPE(KEYIND),
     *                                           KEYTYP(CURKEY))
C
  110       CONTINUE
C                                       No conversion possible
               WRITE (MSGTXT, 9110) KEYWRD(CURKEY)
               CALL MSGWRT (8)
               WRITE (MSGTXT, 9111) TNAME(KEYTYP(CURKEY)),
     *                              TNAME(XTYPE(KEYIND))
               CALL MSGWRT (8)
               IRET = 1
               GO TO 190
C
  120       CONTINUE
C                                       Correct type
               GO TO 190
C
  130       CONTINUE
C                                       Silent conversion
               KEYTYP(CURKEY) = XTYPE(KEYIND)
               GO TO 190
C
  140       CONTINUE
C                                       Can be corrected
               WRITE (MSGTXT, 1140) KEYWRD(CURKEY)
               CALL MSGWRT (6)
               WRITE (MSGTXT, 1141) TNAME(KEYTYP(CURKEY)),
     *                              TNAME(XTYPE(KEYIND))
               CALL MSGWRT (6)
               KEYTYP(CURKEY) = XTYPE(KEYIND)
               GO TO 190
C
  150       CONTINUE
C                                       Float instead of integer
C
C              Convert if the value is a floating-point integer
C              (noting that arithmetic on floating-point integers
C              is exact unless the mantissa overflows) otherwise
C              disallow the conversion:
C
               IF ((KEYVAL(CURKEY) - NINT (KEYVAL(CURKEY)))
     *           .EQ.0.0D0) THEN
                  WRITE (MSGTXT, 1140) KEYWRD(CURKEY)
                  CALL MSGWRT (6)
                  WRITE (MSGTXT, 1141) TNAME(KEYTYP(CURKEY)),
     *                                 TNAME(XTYPE(KEYIND))
                  CALL MSGWRT (6)
                  KEYTYP(CURKEY) = XTYPE(KEYIND)
               ELSE
                  WRITE (MSGTXT, 9110) KEYWRD(CURKEY)
                  CALL MSGWRT (8)
                  WRITE (MSGTXT, 9111) TNAME(KEYTYP(CURKEY)),
     *                                 TNAME(XTYPE(KEYIND))
                  CALL MSGWRT (8)
                  WRITE (MSGTXT, 9950)
                  IRET = 1
               END IF
               GO TO 190
C
  190       CONTINUE
         END IF
C
  200 CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1140 FORMAT ('KEYCHK: Keyword ',A8,' has the wrong value type')
 1141 FORMAT ('KEYCHK: Changing type from ',A,' to ',A)
 9110 FORMAT ('KEYCHK: KEYWORD ',A8,' HAS THE WRONG VALUE TYPE')
 9111 FORMAT ('KEYCHK: CAN NOT CONVERT FROM ',A,' TO ',A)
 9950 FORMAT ('KEYCHK: (FRACTIONAL PART IS NON-ZERO)')
      END
      SUBROUTINE IDIHDR (NUMKEY, KEYWRD, KEYVAL, KEYTYP, IRET)
C-----------------------------------------------------------------------
C   Check for standard FITS interferometry data interchange keywords
C   and fix value types if possible.
C
C   Inputs:
C      NUMKEY     I       Number of keywords to check
C      KEYWRD     C(*)*8  List of keywords to check
C
C   Input/Output:
C      KEYVAL     D(*)    Numeric keyword values
C      KEYTYP     I       Keyword type codes
C
C   Output:
C      IRET       I       Return status:
C                           0 - keywords OK or corrected
C                           1 - unrecoverable type (eg. string
C                               instead of number)
C-----------------------------------------------------------------------
      INTEGER   NUMKEY, KEYTYP(*), IRET
      CHARACTER KEYWRD(*)*8
      DOUBLE PRECISION KEYVAL(*)
C                                       Local variables
C     NIDIKY    Number of recognized keywords (constant)
C     IDIKEY    List of keywords to look for
C     IDITYP    List of types for IDIKEY
      INTEGER   NIDIKY
      PARAMETER (NIDIKY = 9)
      CHARACTER IDIKEY(NIDIKY)*8
      INTEGER   IDITYP(NIDIKY)
      INCLUDE 'INCS:PTAB.INC'
      DATA IDIKEY /'OBSCODE ', 'NO_STKD ', 'STK_1   ', 'NO_BAND ',
     *   'NO_CHAN ', 'REF_FREQ', 'CHAN_BW ', 'REF_PIXL', 'TABREV  '/
      DATA IDITYP /TABHOL,     TABINT,     TABINT,     TABINT,
     *    TABINT,     TABDBL,     TABDBL,     TABDBL,     TABINT/
C-----------------------------------------------------------------------
      CALL KEYCHK (NIDIKY, IDIKEY, IDITYP, NUMKEY, KEYWRD, KEYVAL,
     *   KEYTYP, IRET)
C
 999  RETURN
      END
      SUBROUTINE WRINI (OPCODE, WRBUFF, DISK, CNO, VER, CATBLK, LUNWR,
     *   IWRRNO, WRKOLS, WRNUMV, OBSCOD, OBSDAT, TABVER, IERR)
C-----------------------------------------------------------------------
C   Creates and initializes weather data tables: WR form
C   Inputs:
C      OPCODE         C*4  Operation code:
C                          'WRIT' = create/init for write or read
C                          'READ' = open for read only
C      WRBUFF(512)    I    I/O buffer and related storage, also defines
C                          file if open.
C      DISK           I    Disk to use.
C      CNO            I    Catalog slot number
C      VER            I    WR file version
C      CATBLK(256)    I    Catalog header block.
C      LUNWR          I    Logical unit number to use
C   Input (create) / output (pre-existing) (file keywords):
C      OBSCOD         C*8  Observing code.
C      OBSDAT         C*8  Observing date.
C      TABVER         I    Table revision number.
C      WRNUMV(MAXWRC) I    Element count in each column. On input only
C                          used if the file is created.
C   Output:
C      IWRRNO         I    Next row number, start of the file if READ,
C                          the last+1 if WRITE
C      WRKOLS(MAXWRC) I    The column pointer array in order:
C                          TIME, DTIM, ANT, TEMP, PRESS, DEWPT,
C                          WVEL, WDIR, H2OCOL, IONCOL, PRECIP
C   Output:
C      IERR           I    Return error code, 0=>OK, else TABINI or
C                          TABIO error.
C----------------------------------------------------------------------
      INCLUDE 'DWRV.INC'
      CHARACTER OPCODE*4, OBSCOD*8, OBSDAT*8
      INTEGER   WRBUFF(512), DISK, CNO, VER, CATBLK(256), LUNWR, IWRRNO,
     *   WRKOLS(MAXWRC), WRNUMV(MAXWRC), TABVER, IERR
C
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL T, DOREAD, NEWFIL
      CHARACTER TTITLE*56, TITLE(MAXWRC)*24, TITLE2(MAXWRC)*24,
     *   UNITS(MAXWRC)*8, KEYWRD(NKEYWR)*8
      INTEGER   I, JERR, ITEMP(6), NTTWR, NKEY, NREC, NCOL, IPOINT,
     *   NDATA, DATP(128,2), KLOCS(NKEYWR), KEYTYP(NKEYWR),
     *   KEYI(NKYWWR), DTYP(MAXWRC), WRKOL2(MAXWRC)
      REAL      KEYR(NKYWWR)
      DOUBLE PRECISION KEYD(NKY2WR)
      HOLLERITH KEYH(NKYWWR), HOLTMP(6)
      EQUIVALENCE (KEYI, KEYR, KEYD, KEYH)
      EQUIVALENCE (HOLTMP, ITEMP)
      DATA TTITLE / 'WEATHER DATA TABLE' /
      DATA TITLE /'TIME', 'TIME_INTERVAL', 'ANTENNA_NO',
     *   'TEMPERATURE', 'PRESSURE', 'DEWPOINT', 'WIND_VELOCITY',
     *   'WIND_DIRECTION', 'WIND_GUST', 'PRECIPITATION',
     *   'WVR_H2O', 'IONOS_ELECTRON'/
      DATA TITLE2 / 'TIME', 'TIME INTERVAL', 'ANTENNA NUMBER',
     *   'TEMPERATURE', 'PRESSURE', 'DEWPOINT', 'WIND VELOCITY',
     *   'WIND DIRECTION', 'WIND GUST', 'PRECIPITATION',
     *   'H2O COLUMN', 'ELECTRON COLUMN'/
      DATA UNITS / 2*'DAYS', ' ', 'CENTIGRA', 'MILLIBAR',
     *   'CENTIGRA', 'M/SEC', 'DEGREES', 'M/SEC', 'CM', 2*' ' /
      DATA KEYWRD / 'OBSCODE', 'RDATE', 'TABREV' /
      DATA NTTWR / 56 /
      DATA T / .TRUE. /
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Set up needed variables
      NREC = 1000
      NCOL = MAXWRC
      IF (DOREAD) NCOL = 0
      NKEY = NKEYWR
      NDATA = MAXWRC
      CALL FILL (NDATA, 0, WRKOLS)
      CALL FILL (NDATA, 0, WRNUMV)
C                                       Fill in types, lengths
C                                       See Going AIPS Vol 2 p13-3.
      IF (.NOT.DOREAD) THEN
         DTYP(WRDTIM) = TABDBL + 10
         DTYP(WRRINT) = TABFLT + 10
         DTYP(WRIANT) = TABINT + 10
         DTYP(WRRTMP) = TABFLT + 10
         DTYP(WRRPRS) = TABFLT + 10
         DTYP(WRRDWP) = TABFLT + 10
         DTYP(WRRVEL) = TABFLT + 10
         DTYP(WRRDIR) = TABFLT + 10
         DTYP(WRRGUS) = TABFLT + 10
         DTYP(WRRPRE) = TABFLT + 10
         DTYP(WRRH2O) = TABFLT + 10
         DTYP(WRRION) = TABFLT + 10
         CALL COPY (NCOL, DTYP, DATP(1,2))
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'WR', DISK, CNO, VER, CATBLK, LUNWR, NKEY,
     *   NREC, NCOL, DATP, WRBUFF, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'WRINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      IWRRNO = WRBUFF(5) + 1
      IF (DOREAD) IWRRNO = 1
      NKEY = NKEYWR
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, ITEMP, WRBUFF, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'WRINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, ITEMP, WRBUFF, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'WRINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTTWR, TTITLE, 1, WRBUFF(101))
C                                       Set keyword values
C                                       Observing code
         KLOCS(1) = 1
         KEYTYP(1) = TABHOL
         CALL CHR2H (8, OBSCOD, 1, KEYH(KLOCS(1)))
C                                       Observing date
         KLOCS(2) = 3
         KEYTYP(2) = TABHOL
         CALL CHR2H (8, OBSDAT, 1, KEYH(KLOCS(2)))
C                                       Table revision number
         KLOCS(3) = 5
         KEYTYP(3) = TABINT
         KEYI(KLOCS(3)) = IWRREV
C                                       Write to the WR table
         CALL TABKEY ('WRIT', KEYWRD, NKEYWR, WRBUFF, KLOCS, KEYI,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1).AND.(IERR.LE.20)) THEN
            WRITE (MSGTXT,1250) IERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         CALL TABKEY ('READ', KEYWRD, NKEYWR, WRBUFF, KLOCS, KEYI,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1).AND.(IERR.LE.20)) THEN
            WRITE (MSGTXT,1250) IERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
C                                       Observing code
         CALL H2CHR (8, 1, KEYH(KLOCS(1)), OBSCOD)
C                                       Observing date
         CALL H2CHR (8, 1, KEYH(KLOCS(2)), OBSDAT)
C                                       Table revision number
         TABVER = KEYI(KLOCS(3))
         END IF
C                                       Get array indices
C                                       Cover your ass from FNDCOL -
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYI, WRBUFF, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'WRINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'WR', DISK, CNO, VER, CATBLK, LUNWR, NKEY,
     *   NREC, NCOL, DATP, WRBUFF, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'WRINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXWRC, TITLE, 24, T, WRBUFF, WRKOLS, JERR)
      CALL FNDCOL (MAXWRC, TITLE2, 24, T, WRBUFF, WRKOL2, JERR)
C                                       Get array indices and no. values
      DO 150 I = 1,NCOL
         IPOINT = WRKOLS(I)
         IF (IPOINT.EQ.0) IPOINT = WRKOL2(I)
         IF (IPOINT.GT.0) THEN
            WRKOLS(I) = DATP(IPOINT,1)
            WRNUMV(I) = DATP(IPOINT,2) / 10
         ELSE
            WRKOLS(I) = -1
            WRNUMV(I) = 0
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
1250  FORMAT ('WRINI: ERROR', I3, ' RETURNED BY TABKEY')
1990  FORMAT ('WRINI: ERROR INITIALIZING WEATHER INFO TABLE FOR ', A4)
      END
      SUBROUTINE TABWR (OPCODE, WRBUFF, IWRRNO, WRKOLS, WRNUMV, TIME,
     *   DTIME, IANT, TEMP, PRESS, DEWPT, WVEL, WDIR, WGUST, PRECIP,
     *   H2OCOL, IONCOL, IERR)
C----------------------------------------------------------------------
C   Does I/O to a weather table.  Used after setup by WRINI.
C   Inputs:
C      OPCODE       C*4         Operation code:
C                               'READ' = read entry from table
C                               'WRIT' = write entry in table
C                               'CLOS' = close file, flush on write
C      WRBUFF       I(*)        I/O buffer and related storage, also
C                               defines file if open. Should have
C                               been returned by WRINI.
C   Input/Output:
C      IWRRNO       I           Next row to read or write
C      WRKOLS       I(MAXWRC)   The column pointer array in order:
C                               TIME, DTIME, IANT, TEMP, PRESS,
C                               DEWPT, WVEL, WDIR, H2OCOL, IONCOL
C      TIME         D           Time (days wrt reference day)
C      DTIME        R           integration time (days)
C      IANT         I           antenna number
C      TEMP         R           surface temperature (C)
C      PRESS        R           surface pressure (mbar)
C      DEWPT        R           dew point temperature (C)
C      WVEL         R           wind velocity (m/s)
C      WDIR         R           wind direction (east from north)
C      WGUST        R           wind gusts
C      PRECIP       R           precipitation since midnight (cm)
C      H2OCOL       R           water column (m^-2)
C      IONCOL       R           ion column (m^-2)
C   Output:
C      IERR         I           Return code (0=>ok; else error)
C                               Note: -1 => read by not selected
C----------------------------------------------------------------------
      INCLUDE 'DWRV.INC'
      CHARACTER OPCODE*4
      INTEGER   WRBUFF(512), IWRRNO, WRKOLS(MAXWRC), WRNUMV(MAXWRC),
     *   IANT, IERR
      DOUBLE PRECISION TIME
      REAL      DTIME, TEMP, PRESS, DEWPT, WVEL, WDIR, WGUST, PRECIP,
     *   H2OCOL, IONCOL
C
      DOUBLE PRECISION RECD(MXDPWR)
      REAL      RECR(MXSPWR)
      INTEGER   RECI(MXSPWR)
      EQUIVALENCE (RECD, RECR, RECI)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C----------------------------------------------------------------------
C                                       Initialization
      IERR = 0
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, IWRRNO, RECI, WRBUFF, IERR)
         IF (IERR.GT.0) CALL TABERR ('CLOS', 'TABIO', 'TABWR', IERR)
C                                       Do I/O
      ELSE
C                                       If write fill RECORD
         IF (OPCODE.NE.'READ') THEN
C                                       Time
            RECD(WRKOLS(WRDTIM)) = TIME
C                                       integration Time
            RECR(WRKOLS(WRRINT)) = DTIME
C                                       antenna
            RECI(WRKOLS(WRIANT)) = IANT
C                                       surface temperature
            RECR(WRKOLS(WRRTMP)) = TEMP
C                                       surface pressure
            RECR(WRKOLS(WRRPRS)) = PRESS
C                                       dew point temperature
            RECR(WRKOLS(WRRDWP)) = DEWPT
C                                       wind velocity
            RECR(WRKOLS(WRRVEL)) = WVEL
C                                       wind direction
            RECR(WRKOLS(WRRDIR)) = WDIR
C                                       precipitation column
            IF (WRNUMV(WRRGUS).GT.0) RECR(WRKOLS(WRRGUS)) = WGUST
C                                       precipitation column
            IF (WRNUMV(WRRPRE).GT.0) RECR(WRKOLS(WRRPRE)) = PRECIP
C                                       water column
            IF (WRNUMV(WRRH2O).GT.0) RECR(WRKOLS(WRRH2O)) = H2OCOL
C                                       ion column
            IF (WRNUMV(WRRION).GT.0) RECR(WRKOLS(WRRION)) = IONCOL
            END IF
C                                       Process record
 200     CALL TABIO (OPCODE, 0, IWRRNO, RECR, WRBUFF, IERR)
         IWRRNO = IWRRNO + 1
         IF (IERR.GT.0) THEN
            CALL TABERR (OPCODE, 'TABIO', 'TABWR', IERR)
            GO TO 999
         ELSE IF (IERR.LT.0) THEN
            GO TO 200
C                                       If READ pick data from record
         ELSE IF (OPCODE.EQ.'READ') THEN
C                                       Time
            TIME = RECD(WRKOLS(WRDTIM))
C                                       integration Time
            DTIME = RECR(WRKOLS(WRRINT))
C                                       antenna
            IANT = RECI(WRKOLS(WRIANT))
C                                       surface temperature
            TEMP = RECR(WRKOLS(WRRTMP))
C                                       surface pressure
            PRESS = RECR(WRKOLS(WRRPRS))
C                                       dew point temperature
            DEWPT = RECR(WRKOLS(WRRDWP))
C                                       wind velocity
            WVEL = RECR(WRKOLS(WRRVEL))
C                                       wind direction
            WDIR = RECR(WRKOLS(WRRDIR))
C                                       wind gusts
            WGUST = FBLANK
            IF (WRNUMV(WRRGUS).GT.0) WGUST = RECR(WRKOLS(WRRGUS))
C                                       precipitation column
            PRECIP = FBLANK
            IF (WRNUMV(WRRPRE).GT.0) PRECIP = RECR(WRKOLS(WRRPRE))
C                                       water column
            H2OCOL = FBLANK
            IF (WRNUMV(WRRH2O).GT.0) H2OCOL = RECR(WRKOLS(WRRH2O))
C                                       ion column
            IONCOL = FBLANK
            IF (WRNUMV(WRRION).GT.0) IONCOL = RECR(WRKOLS(WRRION))
            END IF
         END IF
C
 999  RETURN
      END
