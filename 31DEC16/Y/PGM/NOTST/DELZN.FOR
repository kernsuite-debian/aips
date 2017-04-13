LOCAL INCLUDE 'DELZN.INC'
C                                                          Include DELZN
C                                       Local include for DELZN
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MXSOU, MT, MAXFIT, MXAN, MXANSO, MXIFSO
      PARAMETER (MXSOU = 100)
      PARAMETER (MAXFIT = 200)
      PARAMETER (MXAN = 50)
      PARAMETER (MXIFSO = MAXIF*MXSOU)
C                                       MXSASO - product # antennas
C                                       and sources
      PARAMETER (MXANSO = MXAN*MXSOU)
C                                       MSNTIM - # times in SN table
      INTEGER   MSNTIM
      LONGINT   OFFVAL, OFFTIM, OFFELE, OFFREL, OFFREF, OFFBCO, OFFBCR,
     *   OFFVSA
      INTEGER   NITER

C                                       MT - # times of an antenna
C                                       in SN table
      PARAMETER (MT = 10000)
C                                       for all antennas, IFs, pol.
      INTEGER NIDC, IDC(MXSOU), NIDS, IDS(MXSOU), NNIF, NS, NPLOTS,
     *   NCOUNT, NNNIF, SEQIN, SUBA, DISKIN, CNOIN, SNVER,
     *   CLVER, CLUSE, NANTSL, BIF, EIF, ISTOK, FREQID, XINC,
     *   CURANT, ITIME(MXANSO), NTIME(MXANSO), INDTIM, CURIND,
     *   TVCHN, GRCHN, TVCORN(4), NPARM, NSS, NUMHIS, PRTLEV, NTOTT,
     *   NTERM, NFITA, NFITAT, NFITCL, ANTRTS(MT), ANTREF(2), REFNN
      DOUBLE PRECISION RAC(MXSOU), DECC(MXSOU), RAS(MXSOU), DECS(MXSOU)
      LOGICAL    DOTV, DOHIST, DOPLOT, DOCLTM, DOCLT1
      LOGICAL    DOUTFI, DOATMA, DOCLOC, DOREF, REFEQU, DOCLAT, DOCL,
     *   DORES, DOTWO, DOMOD, DODIR, DODISP
      REAL      XSIN, XDISIN, XFQID, XBAND, XFREQ, XBIF, XEIF, XTIME(8),
     *   XANT(50), XSUBA, XSNVER, XGVER, XNCOU, XXINC,
     *   XPRTL, XDOHI, XDOTV, XBAD(10), APARM(10), XGRCHN, SELBAN,
     *   VAL(2), TIMX(2), MAPX(2), MAPRX(2), BCOEX(2), BCOERX(2),
     *   VALMX(MXAN), VALMN(MXAN),  XAXIMX, XAXIMN, XYSCL(2), VALSAV(2),
     *   XYOFF(2), GMXX, GMNX, CHOUT(4), TIMITS(MT), MAPITS(MT),
     *   MAPRIT(MT), BCOITS(MT), BCORIT(MT), VALU(MT), VALU1(MT),
     *   TIMBEG, TIMEND, TIMSTA,
     *   TIMFIN, BUFF1(2048), FLUXX(MXIFSO), NRMS
      DOUBLE PRECISION   SOL(MAXFIT), VX(MAXFIT), SSQRES, VARY, FIT,
     *   VARRES, TIMCEN
      INTEGER   LESOL
      CHARACTER  HISCRD(1000)*64, NAMEIN*12, CLAIN*6, XSOUR(30)*16,
     *   XCALIB(30)*16, XSTOK*4, OPTYPE*4, OFILE*48,
     *   SOUS(MXSOU)*16, STOKK(2)*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALB(4,30),
     *   XXSTOK, XOPTY, XOFILE(12)
      DOUBLE PRECISION FRQOFF(MAXIF), SELFRQ, LAMBDA(MAXIF)
C                                       cpecific CL data
      INTEGER   CLRECI(13+32*MAXIF), CLKOLS(MAXCLC), CLNUMV(MAXCLC),
     *   FIXCNT,
     *   TIMCL,  INTCL, SOUCL, ANTCL, SUBCL, FRQCL, IFRCL,
     *   GDLCL, DOPCL, ATMCL, DATMCL,
     *   MBD1CL, CLK1CL, DCK1CL, DIS1CL, DDS1CL,
     *   RE1CL, IM1CL, DE1CL, RA1CL, WE1CL, RF1CL,
     *   MBD2CL, CLK2CL, DCK2CL, DIS2CL, DDS2CL,
     *   RE2CL, IM2CL, DE2CL, RA2CL, WE2CL, RF2CL
      REAL     CLRECR(13+32*MAXIF)
      DOUBLE PRECISION  CLRECD(13+32*MAXIF)
C                                       Buffers and file info
      INTEGER   BUFFER(1024)
C                                       Internal storage
      INTEGER   SNRECI(10+15*MAXIF), SNKOLS(MAXSNC), SNNUMV(MAXSNC),
     *   NUMANT, NUMPOL, NUMIF, ICODE, TIMSN, INTSN, SOUSN, ANTSN,
     *   SUBSN, FRQSN, IFRSN, NODSN, RE1SN, IM1SN, DL1SN, RA1SN,
     *   WT1SN, RF1SN, RE2SN, IM2SN, DL2SN, RA2SN, WT2SN, RF2SN
      REAL      GMMOD, SNRECR(10+15*MAXIF)
      DOUBLE PRECISION COSDEC, SINDEC, SNRECD(10+15*MAXIF)
C                                       Inputs and general info
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSNVER, XGVER,
     *   APARM, XXSOUR, XXCALB, XXSTOK, XBAND, XFREQ, XFQID,
     *   XBIF, XEIF, XTIME, XANT, XSUBA, XNCOU, XXINC,
     *   XOPTY, XOFILE, XPRTL, XDOHI, XDOTV, XGRCHN, XBAD
      COMMON /OTHPRM/ OFFVAL, OFFTIM, OFFELE, OFFREL, OFFREF,
     *   OFFBCO, OFFBCR, OFFVSA,
     *   SELBAN,
     *   SEQIN, DISKIN, CNOIN, SUBA, SNVER, CLVER, CLUSE, TVCHN, NPARM,
     *   DOTV, NUMHIS, PRTLEV, DOHIST, DOPLOT, DOUTFI, DOATMA, DOCLOC,
     *   DOREF, DOCLAT, DOCL, DOCLTM, DOCLT1, DORES, DOTWO, DODISP,
     *   DOMOD, DODIR, REFEQU,
     *   NFITA, NFITAT, NFITCL
      COMMON /EL/  RAC, RAS, DECC, DECS, IDC, NIDC, IDS, NIDS, NNIF,
     *   NS, VAL, VALSAV, TIMX, MAPX, MAPRX, BCOEX, BCOERX,
     *   ANTREF, VALMX, VALMN, XAXIMX,
     *   XAXIMN, NPLOTS, XYSCL, XYOFF, NCOUNT, XINC, GMXX, GMNX, CHOUT,
     *   CURANT, ITIME, NTIME, INDTIM, CURIND, GRCHN,
     *   TVCORN, TIMITS, MAPITS, MAPRIT, BCOITS, BCORIT, VALU, VALU1,
     *   ANTRTS,  NNNIF,
     *   ICODE, NSS, FLUXX, NTOTT, MSNTIM, NITER, NRMS,
     *   REFNN
      COMMON /LEASQ/   SOL, VX, SSQRES, VARY, FIT, VARRES, TIMCEN,
     *   LESOL
      COMMON /CINFO/ FRQOFF, SELFRQ, NANTSL,  BIF, EIF, ISTOK,
     *   FREQID, TIMBEG, TIMEND, TIMSTA, TIMFIN
      COMMON /CHRCOM/ HISCRD, NAMEIN, CLAIN, XSOUR, XCALIB,
     *   XSTOK, OPTYPE, SOUS, STOKK, OFILE
C                                       Buffers and file info
      COMMON /BUFRS/ BUFFER, BUFF1
C                                       Important constants
C                                       Internal storage
      COMMON /SNRECC/ LAMBDA, COSDEC, SINDEC, SNRECD, GMMOD,
     *   SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF,
     *   TIMSN, INTSN, SOUSN, ANTSN, SUBSN, FRQSN, IFRSN, NODSN,
     *   RE1SN, IM1SN, DL1SN, RA1SN, WT1SN, RF1SN,
     *   RE2SN, IM2SN, DL2SN, RA2SN, WT2SN, RF2SN
      COMMON /CLRECC/  CLRECD, NTERM,
     *   FIXCNT, CLKOLS, CLNUMV
      EQUIVALENCE (CLRECI, CLRECR, CLRECD)
      EQUIVALENCE (CLKOLS(CLDTIM), TIMCL), (CLKOLS(CLRTMI), INTCL),
     *   (CLKOLS(CLISID),SOUCL), (CLKOLS(CLIANT),ANTCL),
     *   (CLKOLS(CLISUB),SUBCL), (CLKOLS(CLIFQI),FRQCL),
     *   (CLKOLS(CLRIFR),IFRCL), (CLKOLS(CLDDEL),GDLCL),
     *   (CLKOLS(CLRDOP),DOPCL), (CLKOLS(CLRATM),ATMCL),
     *   (CLKOLS(CLRDAT),DATMCL)
      EQUIVALENCE (CLKOLS(CLRMD1),MBD1CL),
     *   (CLKOLS(CLRCK1),CLK1CL), (CLKOLS(CLRDC1),DCK1CL),
     *   (CLKOLS(CLRDS1),DIS1CL), (CLKOLS(CLRDD1),DDS1CL),
     *   (CLKOLS(CLRRE1),RE1CL), (CLKOLS(CLRIM1),IM1CL),
     *   (CLKOLS(CLRRA1),RA1CL), (CLKOLS(CLRDE1),DE1CL),
     *   (CLKOLS(CLRWE1),WE1CL), (CLKOLS(CLIRF1),RF1CL)
      EQUIVALENCE (CLKOLS(CLRMD2),MBD2CL),
     *   (CLKOLS(CLRCK2),CLK2CL), (CLKOLS(CLRDC2),DCK2CL),
     *   (CLKOLS(CLRDS2),DIS2CL), (CLKOLS(CLRDD2),DDS2CL),
     *   (CLKOLS(CLRRE2),RE2CL), (CLKOLS(CLRIM2),IM2CL),
     *   (CLKOLS(CLRRA2),RA2CL), (CLKOLS(CLRDE2),DE2CL),
     *   (CLKOLS(CLRWE2),WE2CL), (CLKOLS(CLIRF2),RF2CL)
Cdelete later
      EQUIVALENCE (SNRECI, SNRECR, SNRECD)
C                                                          End DELZN
LOCAL END
      PROGRAM DELZN
C-----------------------------------------------------------------------
C! Determines zenith delay observing calibrators and find corrections
C! based on the zenith delay and the source elevation.
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2003-2008, 2012, 2015
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
C;         Internet email: aipsmail@nrao.edu
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Task DELZN provides estimation of the residual atmosphere zenith
C   delay and clock error based on the SN table - output of FRING
C   and corrects the CL table based on the zenith delay and the source
C   elevation.
C   Inputs:
C      INNAME.....UV file name (name).       Standard defaults.
C      INCLASS....UV file name (class).      Standard defaults.
C      INSEQ......UV file name (seq. #).     0 => highest.
C      INDISK.....Disk unit #.               0 => any.
C      SNVER......The SN table version number - source of input data
C      GAINVER....Input CL table to copy to GAINUSE and then correct
C      GAINUSE....Output Cl table
C      APARM(10)..Control parameters
C                 1: what data to plot
C                    0 => zenith atm. delay
C                    1 => clock error
C                 2: number of terms at the polynomial for zenith atm.
C      SOURCES....Source list to calibrate .
C                 ' ' = all; a "-" before a source
C                 name means all except ANY source named.
C      CALSOUR....Calibrator list to select from the SN table
C                 ' ' = all; a "-" before a calibrator
C                 name means all except ANY calibrator named.
C      STOKES.....The desired Stokes type of the data:
C                 'R' = RCP, 'L' = LCP, 'other' = all available
C      SELBAN.....Bandwidth to select (kHz)
C      SELFREQ....Frequency to select (MHz)
C      FREQID.....Freq. ID to select, 0=>all
C      BIF........Lowest IF number, 0=>all
C      EIF........Highest IF number, 0=>all
C      TIMERANG...Time range of the data to be calibrateded. In order:
C                 Start day, hour, min. sec,
C                 end day, hour, min. sec. Days relative to ref.
C                 date.
C      ANTENNAS...A list of the antennas to be callibrated. All 0 => all.
C                 If any number is negative then all antennas listed
C                 are NOT to be callibrated and all others are.
C      SUBARRAY...The subarray to callibrate. Do only one at a time.
C      NPLOTS.....Number of plots to plot per page 0=>5.
C                 -1 => Don't make plots
C      XINC.......Plot every XINC'th point
C      OPTYPE.....The SN table Data to be fitted:
C                 'MDEL'' or '    ' =>  multiband delay,
C                 'PHAS' = phas delay
C      OUTFILE....Output file name. Delays at zenith
C      DOTV.......> 0 => TV, else plot file
C      BADDISK....A list of disks on which scratch files are not to
C                 be placed.
C
C programmer: Leonia Kogan, July 2003
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'DELZN.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'DELZN '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL DELZIN (PRGM, IRET)
      MSGTXT = 'Finished init'
      call msgwrt (6)
C                                       read SN table and store
C                                       times and values; fit model
      IF (IRET.EQ.0) CALL DELUV (IRET)
C                                       plot values
C                                       and model versus time
      IF (IRET.EQ.0) CALL PLTFIT (IRET)
      IRET = MAX (0, IRET)
C                                       Apply correction to the CL table
C                                       and record output file
      IF (IRET.EQ.0 .AND. (DOCL.OR.DOUTFI)) CALL OUTCL (IRET)
      IRET = MAX (0, IRET)
C                                       Copy and update HI file.
      IF (IRET.EQ.0) CALL DELHIS
C                                       Close down files, etc.
      CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE DELZIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   DELZIN gets input parameters for DELZN.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                1 => Invalid request
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   JERR, NSOUR
C
      CHARACTER STAT*4, BLANK*4, UTYPE*2, CODE(5)*4
      LOGICAL   T, MATCH
      INTEGER   IERR, I, IROUND, LUN, IIVER, NCODE, SNTOT,
     *   NIDCC, IS, IEND, NUMCL, LUN2, TABUFF(512)
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'DELZN.INC'
      INTEGER   DUMMY(MAXIF)
      REAL      FINC(MAXIF), TTIME
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DOTTV.INC'
C     CODE =                 1      2      3      4      5
      DATA NCODE, CODE /5, 'MDEL','PHAS','DISP','RATE','    '/
      DATA BLANK /'    '/
      DATA T /.TRUE./
      DATA LUN, LUN2  /29, 28/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T, BUFFER)
      CALL VHDRIN
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 353
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SUBA = XSUBA + 0.5
      IF (SUBA.LE.0) SUBA = 1
C
      PRTLEV = IROUND (XPRTL)
      DOHIST = XDOHI.GT.0.0
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCHN + 0.01
      IF (GRCHN.EQ.0) GRCHN = 1
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
      XINC = IROUND (XXINC)
      IF (XINC.LE.0) XINC = 1
      XXINC = XINC
      DO 15 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 15      CONTINUE
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (4, 1, XOPTY, OPTYPE)
      CALL H2CHR (48, 1, XOFILE, OFILE)
C                                       calculate the output file?
      DOUTFI = OFILE(1:1) .NE. ' '
C                                       OPTYPE ?
      ICODE = 1
      DO 20 I = 1,NCODE
         IF ((OPTYPE(1:3).EQ.CODE(I)(1:3)) .AND. (OPTYPE.NE.BLANK))
     *      ICODE = I
 20      CONTINUE
      OPTYPE = CODE(ICODE)
      CALL CHR2H (4, OPTYPE, 1, XOPTY)
C
      DO 30 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
         CALL H2CHR (16, 1, XXCALB(1,I), XCALIB(I))
 30      CONTINUE
C                                       Timerange
      TIMBEG = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0) +
     *   (XTIME(4) / (24.0*60.0*60.0))
      TIMEND = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0) +
     *   (XTIME(8) / (24.0*60.0*60.0))
      IF ((TIMEND.LT.TIMBEG) .OR. (TIMEND.LT.1.0E-5)) TIMEND = 1.0E20
      IF ((TIMEND.LT.TIMBEG) .OR. (TIMBEG.LT.1.0E-5)) TIMBEG = 0.0
C                                       Find file, read CATBLK
      CNOIN = 1
      STAT = 'SRCH'
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'WRIT', BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       what to plot?
      DOATMA = (APARM(1).LT.0.1)
      DOCLOC = (APARM(1).GT.0.5 .AND. APARM(1).LT.1.5)
      DOREF  = (APARM(1).GT.1.5 .AND. APARM(1).LT.2.5)
      DODISP = (APARM(1).GT.2.5 .AND. APARM(1).LT.3.5)
C                                       How to plot?
      DOMOD = (APARM(10).LT.0.1)
      DODIR = (APARM(10).GT.0.5 .AND. APARM(10).LT.1.5)
C                                       Number of terms at the
C                                       polynomial representation
C                                       of the zenith atmosphere
C                                       If NFITAT.EQ.0 then
C                                       nofit of atm
      NFITAT = APARM(2)
C                                       Number of terms at the
C                                       polynomial representation
C                                       of the clock errors
C                                       If NFITCL.EQ.0 then
C                                       nofit of clock error
C
C                                       Create CL table?
      NFITCL = APARM(3)
      IF (ICODE.NE.1) NFITCL = 0

      DOCL = (APARM(4).GT.0.5)
C                                       Will only atmosphere be
C                                       corrected at CL table or sum
C                                       of atmosphere and clock
      DOCLAT = (APARM(5).LT.0.1)
C                                       number of itteration in
C                                       editing the SN table
      NITER = APARM(6)
C                                       number of r.m.s in itteration
      NRMS = 3
C                                       calculate the output file for
C                                       the CL table times only if
C                                       CL table is there
      DOCLTM = APARM(8).GT.0.1
C
      IF (.NOT. DOCL .AND. DOCLTM) THEN
         DOCLTM = .FALSE.
         WRITE (MSGTXT,1048)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1050)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1048)
         CALL MSGWRT (8)
         END IF
C
      DOCLT1 = DOCLTM
C                                       plot data + model or
C                                       additionally plot residual
      DORES = APARM(9).GT.0.001
C
      IF (NRMS.EQ.0) NRMS=3
C                                       What the total number of fit
C                                       parameters for each antenna
      NFITA = NFITAT + NFITCL
C                                       Table version numbers
      SNVER = IROUND (XSNVER)
      CLVER = IROUND (XGVER)
C                                       Defaults for CL tables
      CALL FNDEXT ('CL', CATBLK, NUMCL)
      IF (CLVER.LE.0 .OR. CLVER.GT.NUMCL) CLVER = NUMCL
      CLUSE = NUMCL + 1
C                                       Prohibit CLUSE=1
      IF (CLUSE.EQ.1) THEN
         JERR = 5
         MSGTXT = 'ERROR: IT IS FORBIDDEN TO MODIFY CL VERSION 1'
         GO TO 990
         END IF
C                                       copy CLVER table to CLUSE table
      IF (DOCL) THEN
         CALL TABCOP ('CL', CLVER, CLUSE, LUN, LUN2, DISKIN,DISKIN,
     *      CNOIN, CNOIN, CATBLK, BUFF1, TABUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
         END IF
      XGVER = CLVER
C                                       determine number of SN tables
      CALL FNDEXT ('SN', CATBLK, SNTOT)
      IF (SNVER.EQ.0 .OR. SNVER.GT.SNTOT) SNVER = SNTOT
      XSNVER = SNVER
C                                       IF range
      BIF = IROUND (XBIF)
      EIF = IROUND (XEIF)
      IF (BIF.LE.0) BIF = 1
      IF ((EIF.LE.0) .AND. (JLOCIF.GT.0)) EIF = CATBLK(KINAX+JLOCIF)
      IF (EIF.LE.0) EIF = 1
      IF ((JLOCIF.GT.0) .AND. (BIF.GT.CATBLK(KINAX+JLOCIF)))
     *   BIF = CATBLK(KINAX+JLOCIF)
      IF ((JLOCIF.GT.0) .AND. (EIF.GT.CATBLK(KINAX+JLOCIF)))
     *   EIF = CATBLK(KINAX+JLOCIF)
      XBIF = BIF
      XEIF = EIF
C                                       NNIF number of selected IFs
      NNIF = EIF - BIF + 1
C                                       NNNIF number of IFs in data
      NNNIF = CATBLK(KINAX+JLOCIF)
C                                       Select sources
      NIDS = MXSOU
      NSOUR = 30
      CALL SOUSEL (XSOUR, NSOUR, DISKIN, CNOIN, CATBLK, BIF, NNIF,
     *   BUFFER, IDS, NIDS, RAS, DECS, SOUS, FLUXX, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       history: sources
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2350)
      NIDCC = (NIDS-0.1) / 3
      DO 60 IS = 1, NIDCC+1
         NUMHIS = NUMHIS + 1
         IF (NIDS.GE.3*IS) THEN
            IEND = 3
         ELSE
            IEND = NIDS - 3*(IS-1)
            END IF
         WRITE (HISCRD(NUMHIS),2400) (SOUS(3*(IS-1) + I), I = 1,IEND)
 60      CONTINUE
C                                       Select calibrators
      NIDC = MXSOU
      NSOUR = 30
      CALL SOUSEL (XCALIB, NSOUR, DISKIN, CNOIN, CATBLK, BIF, NNIF,
     *   BUFFER, IDC, NIDC, RAC, DECC, SOUS, FLUXX, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       history: calibrators
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2550)
      NIDCC = (NIDC-0.1) / 3
      DO 70 IS = 1, NIDCC+1
         NUMHIS = NUMHIS + 1
         IF (NIDC.GE.3*IS) THEN
            IEND = 3
         ELSE
            IEND = NIDC - 3*(IS-1)
            END IF
         WRITE (HISCRD(NUMHIS),2600) (SOUS(3*(IS-1) + I), I = 1,IEND)
 70      CONTINUE
C                                       select antennas
      NANTSL = 50
      CALL ANTSEL (XANT, NANTSL, SUBA, DISKIN, CNOIN, CATBLK,
     *   BUFFER, JERR)
      IF (NSTNS.EQ.0) THEN
         WRITE (MSGTXT,1400)
         JERR = 5
         GO TO 990
         END IF
C                                       history: antennas
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2650)
      NIDCC = (NSTNS-0.1) / 10
      DO 80 IS = 1, NIDCC+1
         NUMHIS = NUMHIS + 1
         IF (NSTNS.GE.10*IS) THEN
            IEND = 10
         ELSE
            IEND = NSTNS - 10*(IS-1)
            END IF
         WRITE (HISCRD(NUMHIS),2700) (TELNO(10*(IS-1) + I), I = 1,IEND)
 80      CONTINUE
C
      NRPARM = CATBLK(KIPCN)
C                                       Freq id
      FREQID = IROUND (XFQID)
      IF (FREQID.LE.0) FREQID = -1
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FREQID, JERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1060)
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
      XFQID = SELFRQ
C                                       Stokes' type.
      STOKK(1) = 'Rpol'
      STOKK(2) = 'Lpol'
      IF ((CATBLK(KINAX+JLOCS).EQ.1) .AND. (ICOR0.EQ.-2))
     *   STOKK(1) = 'Lpol'
      NSS = MIN (2, CATBLK(KINAX+JLOCS))
      NS = 1
      ISTOK = 0
      IF (XSTOK.EQ.'R   ') ISTOK = 1
      IF (XSTOK.EQ.'L   ') ISTOK = NSS
C                                       If none selected take what you
C                                       have.
      IF (ISTOK.EQ.0) THEN
         NS = NSS
         ISTOK = 1
C                                       Is selected Stokes' available?
      ELSE
         IF ((CATBLK(KINAX+JLOCS).EQ.1) .AND.
     *      ( (ICOR0.EQ.-1) .AND. (XSTOK.EQ.'L   ') .OR.
     *       (ICOR0.EQ.-2) .AND. (XSTOK.EQ.'R   '))) THEN
            JERR = 1
            MSGTXT = 'STOKES ' // XSTOK // ' UNAVAILABLE IN DATA'
            GO TO 990
            END IF
         END IF
C                                       number of plots
      NPLOTS = NS*NNIF*NSTNS
C                                       number of plots at a page
      NCOUNT = IROUND (XNCOU)
      NCOUNT = MIN (NCOUNT, NPLOTS)
      IF (NCOUNT.EQ.0) NCOUNT = 5
      XNCOU = NCOUNT
C
      IF (DORES) THEN
         NCOUNT = 2*NCOUNT
         END IF
C
      DOPLOT = NCOUNT.GE.0
      JERR = 0
C                                       history file
      NUMHIS = NUMHIS + 1
      TTIME = 0
      DO 90 I = 1, 8
         TTIME = TTIME + XTIME(I)
 90      CONTINUE
      IF (TTIME.EQ.0.0) THEN
         WRITE (HISCRD(NUMHIS),2100)
      ELSE
         WRITE (HISCRD(NUMHIS),2200) (XTIME(I),I=1,8)
         END IF
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2300) SNVER, CLVER, CLUSE
       NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2750) BIF, EIF, FREQID, SUBA
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2800) OPTYPE
      IF (NS.EQ.2) THEN
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2900)
      ELSE
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2950) STOKK(ISTOK)
         END IF
C
C                                       Get IF information
      IIVER = 1
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, IIVER, CATBLK, LUN,
     *   NUMIF, FRQOFF, DUMMY, FINC, BNDCOD, FREQID, JERR)
      IF (JERR.NE.0) GO TO 999
C
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DELZIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1048 FORMAT ( '-----------------------------------------------------')
 1050 FORMAT ( 'force even times for OUTFI, because CL is not created')
 1060 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1100 FORMAT ('ERROR: COPYING INPUT CL TO OUTPUT:',I4)
 1400 FORMAT ('ANTSEL: NOONE ANTENNA SELECTED')
 2100 FORMAT ('TIMERANG = beginning to end')
 2200 FORMAT (F3.0,1X,F3.0,F3.0,F4.1,' - ',F3.0,1X,F3.0,F3.0,F4.1,
     *   '/ Time range')
 2300 FORMAT ('Input SN table =',I3, 5X, 'Input CL table =',I3,
     *   'Output CL table =',I3)
 2350 FORMAT (5X,'Selected sources')
 2400 FORMAT (3A16)
 2550 FORMAT (5X,'Selected calibrators')
 2600 FORMAT (3A16)
 2650 FORMAT (5X,'Selected antennas')
 2700 FORMAT (10I4)
 2750 FORMAT ('BIF =', I3,5X, 'EIF =', I3,5X,'FREQID =',
     *   I3,5X,'SUBARRAY =', I3)
 2800 FORMAT ('OPTYPE = ',A4)
 2900 FORMAT ('STOKES = Rpol and Lpol')
 2950 FORMAT ('STOKES = ', A4)
       END
      SUBROUTINE DELUV (IERR)
C-----------------------------------------------------------------------
C   DELUV is called from DELZN. DELUV reads through the SN table,
C   fits the polynomials,
C   stores given values for the plotting routine PLTFIT
C   Output: IERR  I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LUN, ISNRNO, NUMREC, NUMNOD, ISN, JIF, JS, SOUN,
     *   FREQN, MODENO, IANT, KS, KIF, MAX2, LANT, SID,
     *   NTTOT, SOUANT, NFIT, IFIT, KFIT, IKFIT, IC,
     *   KC, LFIT, MFIT, RFIT, LIFIT, MIFIT, LKFIT, MKFIT, IIFSTO,
     *   REFOLD, REFNEW, RANT
      LOGICAL   ISAPL, FIRST
      REAL      RANOD(25), DECNOD(25), ELEV, ELEREF, BCO, BCOR,
     *   COSZ, MAPANT, MAPREF, TIMARG, TIMMI, TIMMA
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'DELZN.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      REAL      REAL(2,MAXIF), IMAG(2,MAXIF), DELAY(2,MAXIF),
     *   RATE(2,MAXIF), WEIGHT(2,MAXIF), MBDELY(2), TIMINT, IFRM,
     *   HL, RESID, RMSD, DISP(2), DDISP(2)
      INTEGER   REFA(2,MAXIF), REFN, ITER, IFSIDE(MAXIF), IIF, NWORDS
      DOUBLE PRECISION IFFREQ(MAXIF), R(MAXFIT), MATR(MAXFIT*MAXFIT),
     *   NOBS, SUM, SSQ
      CHARACTER BNDCOD(MAXIF)*8
      REAL      IFCHW(MAXIF), IFTBW(MAXIF), VALCUR, CCANT(10), CCREF(10)
      REAL   TCOEFR, ACOEFR, BCOEFR, TCOEF, ACOEF, BCOEF,
     *   A0CLO, A1CLO, VALRAT
      DOUBLE PRECISION CTIM, SIDT, TIMMAX, TIMMIN
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUN /29/
      DATA SIDT /1.00273790935D0/
C-----------------------------------------------------------------------
      CALL GETFQ (FREQID, DISKIN, CNOIN, CATBLK, LUN, IFFREQ, IFTBW,
     *   IFCHW, IFSIDE, BNDCOD, IERR)
C
C                                       Wavelength in mm
      DO 15 IIF = 1, NUMIF
         LAMBDA(IIF) = VELITE / (SAFREQ + IFFREQ(IIF)) * 1000
 15      CONTINUE
      IF (IERR.NE.0) GO TO 900
C   --------------------------------------------------------------
C   I  Read the SN table the first time to estimate number of    I
C   I  points for each selected antenna for selected calibrators I
C   --------------------------------------------------------------
      DO 30 IANT = 1,NSTNS
         DO 20 SID = 1,NIDC
            SOUANT = SID + NIDC*(IANT-1)
            ITIME(SOUANT) = 0
 20         CONTINUE
 30      CONTINUE
C                                       Open SN table, Read for info
      CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, SNVER, CATBLK, LUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
C
      TIMMIN = 1.0E10
      TIMMAX =-1.0E10
      DO 200 ISNRNO = 1,NUMREC
C                                       read SN table
         ISN = ISNRNO
         CALL TABSN ('READ', BUFFER, ISN, SNKOLS, SNNUMV, NSS, CTIM,
     *      TIMINT, SOUN, LANT, SUBA, FREQN, IFRM, MODENO, MBDELY,
     *      DISP, DDISP, REAL, IMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C                                       check if record is flagged
         IF (IERR.LT.0) GO TO 200
C                                       timerange selection
         IF ((CTIM.GT.TIMEND) .OR. (CTIM.LT.TIMBEG)) GO TO 200
C                                       FREQID selection
         IF ((FREQN.NE.0) .AND. (FREQN.NE.FREQID)) GO TO 200
C                                       antennas selection
         DO 40 IANT = 1,NSTNS
            IF (TELNO(IANT).EQ.LANT) GO TO 60
 40         CONTINUE
         GO TO 200
C                                       calibrators selection
 60      DO 80 SID = 1, NIDC
            IF (IDC(SID).EQ.SOUN) GO TO 120
 80         CONTINUE
         GO TO 200
C                                       calculate elevation of
C                                       the selected antenna for
C                                       the selected source
 120     HL = TWOPI*SIDT*CTIM + GSTIAT + STNLON(IANT) - RAC(SID)
         COSZ = DCOS(STNLAT(IANT))*DCOS(DECC(SID))*COS(HL) +
     *      DSIN(STNLAT(IANT))*DSIN(DECC(SID))
         IF (COSZ.GE.0.0) THEN
            SOUANT = SID + NIDC*(IANT-1)
            ITIME(SOUANT) = ITIME(SOUANT) + 1
            END IF
C
         TIMMIN = MIN (TIMMIN, CTIM)
         TIMMAX = MAX (TIMMAX, CTIM)
  200    CONTINUE
C                                       convert double precision to real
      TIMMI = TIMMIN
      TIMMA = TIMMAX
C                                       minimum selected time
      TIMSTA = MAX (TIMMI, TIMBEG)
C                                       maximum selected time
      TIMFIN = MIN (TIMMA, TIMEND)
C
      TIMCEN = (TIMMIN+TIMMAX) / 2
C
      WRITE (MSGTXT,1050) TIMCEN
      CALL MSGWRT (4)
C                                       Close table.
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Number of points for antennas
C                                       until antenna IANT and
C                                       sources until SID
C?????????????????? SOUANT(1) refs NTIME(0) ITIME(0)  ??????????????
      NTIME(1) = 0
      DO 220 IANT = 1, NSTNS
         DO 210 SID = 1, NIDC
            SOUANT = SID + NIDC*(IANT-1)
            IF (SOUANT.GT.1) NTIME(SOUANT) = NTIME(SOUANT-1) +
     *         ITIME(SOUANT-1)
 210        CONTINUE
 220     CONTINUE
      NTTOT = NTIME(SOUANT) + ITIME(SOUANT)
      MSNTIM = NTTOT+10
C                                       dynamic memory for array of
C                                       the antenna elevation,
C                                       reference antenna elevation,
C                                       reference antenna number,
C                                       time and value
C                                       BCOEFF for antenna and for
C                                       refference antenna
      NWORDS = (MSNTIM - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MAPX, OFFELE, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MAPRX, OFFREL, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, TIMX, OFFTIM, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, VAL, OFFVAL, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, VALSAV, OFFVSA, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, ANTREF, OFFREF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, BCOEX, OFFBCO, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, BCOERX, OFFBCR, IERR)
      IF (IERR.NE.0) GO TO 999
C   -----------------------------------------------------------
C   I  Read the SN table the second time to store elevations  I
C   I         and delays for each selected antenna and        I
C   I             for selected calibrators                    I
C   -----------------------------------------------------------
      MAX2 = MSNTIM
      XAXIMX = 0.0
      XAXIMN = 1000.0
C                                       check later about souant lower
      DO 250 IANT = 1,NSTNS
         VALMX(IANT) = -1.0E10
         VALMN(IANT) =  1.0E10
 250     CONTINUE
      CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, SNVER, CATBLK, LUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPL, IERR)
      IF (IERR.NE.0) GO TO 999
C
      NFIT = NFITA*NSTNS
C
      ITER = 0
C                                       New iteration in editing
 260  CONTINUE
C
      DO 280 IANT = 1,NSTNS
         DO 270 SID = 1,NIDC
            SOUANT = SID + NIDC*(IANT-1)
            ITIME(SOUANT) = 0
 270        CONTINUE
 280     CONTINUE
C                                       Force result vector R(NFIT),
C                                       matrix M(NFIT*NFIT) (in LEASQR)
C                                       to zero
      DO 310 IFIT = 1, NFIT
         R(IFIT) = 0.0
         DO 290 KFIT = 1, NFIT
            IKFIT = KFIT + (IFIT - 1)*NFIT
            MATR (IKFIT) = 0.0
 290        CONTINUE
 310     CONTINUE
      SUM = 0.0
      SSQ = 0.0
      NOBS = 0.0
C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
      REFEQU = .TRUE.
      FIRST = .TRUE.
C                                       reading the SN table rows
      DO 500 ISNRNO = 1,NUMREC
C                                       read SN table
         ISN = ISNRNO
         CALL TABSN ('READ', BUFFER, ISN, SNKOLS, SNNUMV, NSS, CTIM,
     *      TIMINT, SOUN, LANT, SUBA, FREQN, IFRM, MODENO, MBDELY, DISP,
     *      DDISP, REAL, IMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C                                       check if record is flagged
         IF (IERR.LT.0) GO TO 500
C                                       timerange selection
         IF ((CTIM.GT.TIMEND) .OR. (CTIM.LT.TIMBEG)) GO TO 500
C                                       FREQID selection
         IF ((FREQN.NE.0) .AND. (FREQN.NE.FREQID)) GO TO 500
C                                       antennas selection
         DO 320 IANT = 1,NSTNS
            IF (TELNO(IANT).EQ.LANT) GO TO 330
 320        CONTINUE
         GO TO 500
C                                       calibrators selection
 330     DO 340 SID = 1, NIDC
            IF (IDC(SID).EQ.SOUN) GO TO 350
 340        CONTINUE
         GO TO 500
C                                       Consider the reference antennas
C                                       are the same for all IFs, Stokes
C                                       but may be different for the
C                                       experiment
C
C                                       Exclude the rows IANT=REFN
C                                       Instead they considered at the
C                                       difference term
 350     REFN = REFA(1,1)
C                                       Is the REFN const for the whole
C                                       experiment?
         IF (FIRST) THEN
            REFOLD = REFN
            FIRST = .FALSE.
            END IF
         REFNEW = REFN
         REFEQU = REFEQU .AND. (REFOLD .EQ. REFNEW)
         REFOLD = REFNEW
C
         IF (TELNO(IANT).EQ.REFN) GO TO 500
C                                       calculate elevation of
C                                       the SN row antenna for
C                                       the SN row source
         HL = TWOPI*SIDT*CTIM + GSTIAT + STNLON(IANT) - RAC(SID)
         COSZ = DCOS(STNLAT(IANT))*DCOS(DECC(SID))*COS(HL) +
     *      DSIN(STNLAT(IANT))*DSIN(DECC(SID))
C                                       source is under horizon
         IF (COSZ.LT.0.0) GO TO 500
         ELEV = PI/2.0 - ACOS(COSZ)
C                                       B coefficient for IANT
         BCO = TWOPI*SIDT*
     *         DCOS(STNLAT(IANT))*DCOS(DECC(SID))*SIN(HL)
C                                       Is the reference antenna
C                                       in the selected antenna list?
         DO 360 RANT = 1,NSTNS
            IF (TELNO(RANT).EQ.REFN) GO TO 370
  360       CONTINUE
         GO TO 500
C                                       calculate elevation of the
C                                       SN row reference antenna
C                                       for the SN row source
  370    HL = TWOPI*SIDT*CTIM + GSTIAT + STNLON(RANT) - RAC(SID)
         COSZ = DCOS(STNLAT(RANT))*DCOS(DECC(SID))*COS(HL) +
     *      DSIN(STNLAT(RANT))*DSIN(DECC(SID))
C                                       source is under horizon
         IF (COSZ.LT.0.0) GO TO 500
         ELEREF = PI/2.0 - ACOS(COSZ)
C                                       B coefficient for RANT
         BCOR = TWOPI*SIDT*
     *         DCOS(STNLAT(RANT))*DCOS(DECC(SID))*SIN(HL)
C
         SOUANT = SID + NIDC*(IANT-1)
         ITIME(SOUANT) = ITIME(SOUANT) + 1
         INDTIM = ITIME(SOUANT) + NTIME(SOUANT)
         IF (INDTIM.LE.MAX2) THEN
C                                       mapping function is cosec(EL)
            IF (ELEV.GT.0.01) MAPANT = 1/SIN(ELEV)
C                                       mapping function is cosec(EL)
            IF (ELEREF.GT.0.01) MAPREF = 1/SIN(ELEREF)
C                                       store the elevations, time
            MAPX(INDTIM+OFFELE) = MAPANT
            MAPRX(INDTIM+OFFREL) = MAPREF
            TIMX(INDTIM+OFFTIM) = CTIM
C                                       store B coefficients for
C                                       OPTYPE = 'RATE'
C                                       IANT
            BCOEX(INDTIM+OFFBCO) = BCO
C                                       RANT
            BCOERX(INDTIM+OFFBCR) = BCOR
C
C                                       store the reference antennas
C                                       For each source/antenna
            ANTREF(INDTIM+OFFREF) = RANT
         ELSE
            WRITE (MSGTXT,1100) MAX2
            IERR = 1
            GO TO 990
            END IF

C                                       find MIN and MAX time
         XAXIMX = MAX(XAXIMX, TIMX(INDTIM+OFFTIM))
         XAXIMN = MIN(XAXIMN, TIMX(INDTIM+OFFTIM))
C                                       eliminate the flag points
C                                       including the flag points of
C                                       the previous edit iteration
C        IF (VAL(INDTIM+OFFVAL).EQ.FBLANK) GO TO 500
C                                       average VALs through all IFs
C                                       and stokes
         VALCUR = 0
         IIFSTO = 0
         DO 390 JIF = 1,NNIF
            KIF = BIF + JIF -1
            DO 380 JS = 1,NS
               KS = ISTOK + JS -1
C                                       chose a type of variable
C                                       mdel, phase, rate
               IF (ICODE.EQ.1) THEN
C                                       MBDELY should be identical
C                                       for all IFS. So use only
C                                       JIF=1
                  IF (JIF .GT. 1) GO TO 380
                  IF (MBDELY(KS).EQ.FBLANK) THEN
                     GO TO 380
                  ELSE
                     IIFSTO = IIFSTO + 1
C                                       delay in mm
                     VALCUR = VALCUR +
     *                  MBDELY(KS) *VELITE *1000
                     END IF
C                                       'phas'
               ELSE IF (ICODE.EQ.2) THEN
                  IF ((REAL(KS,KIF).EQ.FBLANK) .OR.
     *               (IMAG(KS,KIF).EQ.FBLANK)) THEN
                     GO TO 380
                  ELSE
                     IIFSTO = IIFSTO + 1
C                                       phase delay in mm
                     VALCUR = VALCUR +
     *                  ATAN2(IMAG(KS,KIF), REAL(KS,KIF))
     *                  / TWOPI * LAMBDA(KIF)
                     END IF
C                                       disp
               ELSE IF (ICODE.EQ.3) THEN
C                                       DISP should be identical
C                                       for all IFS. So use only
C                                       JIF=1
                  IF (JIF .GT. 1) GO TO 380
                  IF (DISP(KS).EQ.FBLANK) THEN
                     GO TO 380
                  ELSE
                     IIFSTO = IIFSTO + 1
C                                       dispersion in 1/mm
                     VALCUR = VALCUR +
     *                  DISP(KS) *VELITE *1000
                     END IF
C                                       'rate'
               ELSE IF (ICODE.EQ.4) THEN
                  IF (RATE(KS,KIF).EQ.FBLANK) THEN
                     GO TO 380
                  ELSE
C                                       the average RATE through
C                                       IFs and Stokes
                     IIFSTO = IIFSTO + 1
C                                       rate in sec/sec
C                                       VALCUR in mm/day
                     VALCUR = VALCUR + RATE(KS,KIF)*
     *                  VELITE *1D3*24.*60.*60./SIDT
                     END IF
                  END IF
C
 380           CONTINUE
 390        CONTINUE
C                                       find MAX&MIN of the value
C                                       for each ANT
C
C                                       exclude blanked points
         IF (IIFSTO.GT.0) THEN
C                                       VALUE.NE.FBLANK at least
C                                       for one IF,STOKES
            VALCUR = VALCUR / IIFSTO
C
C-----------start simulating rate having used delay at zenith----
            IF (APARM(10) .EQ. 314) THEN
               IF (ICODE .EQ. 4) THEN
                  VALRAT = VALCUR
                  TIMARG = CTIM - TIMCEN
                  TCOEFR = -25.001
                  ACOEFR = 253.323
                  BCOEFR = 2261.66
C
                  IF (IANT .EQ. 1) THEN
                     TCOEF = -119.322
                     ACOEF = -243.506
                     BCOEF = 2818.778
                     A0CLO = 0
                     A1CLO = 10
                     END IF
                  IF (IANT .EQ. 2) THEN
                     TCOEF = 50.059
                     ACOEF = 965.049
                     BCOEF = 5010.373
                     A0CLO = 0
                     A1CLO = 20
                     END IF
                  IF (IANT .EQ. 4) THEN
                     TCOEF = 12.624
                     ACOEF = 411.644
                     BCOEF = 1107.273
                     A0CLO = 0
                     A1CLO = 40
                     END IF
                  IF (IANT .EQ. 5) THEN
                     TCOEF = -554.449
                     ACOEF = 453.446
                     BCOEF = 28466.748
                     A0CLO = 0
                     A1CLO = 50
                     END IF
                  IF (IANT .EQ. 6) THEN
                     TCOEF = -19.819
                     ACOEF = -10.504
                     BCOEF = 1504.642
                     A0CLO = 0
                     A1CLO = 60
                     END IF
                  IF (IANT .EQ. 7) THEN
                     TCOEF = 11.242
                     ACOEF = 240.817
                     BCOEF = 940.816
                     A0CLO = 0
                     A1CLO = 70
                     END IF
                  IF (IANT .EQ. 8) THEN
                     TCOEF = 3.624
                     ACOEF = 352.213
                     BCOEF = 1402.638
                     A0CLO = 0
                     A1CLO = 80
                     END IF
C
                  VALCUR = TCOEF*(BCO*MAPANT**2) +
     *                  ACOEF *(MAPANT + BCO*TIMARG*MAPANT**2) +
     *                  BCOEF *(MAPANT*2*TIMARG + BCO*TIMARG**2*MAPANT
     *                  **2)- TCOEFR*(BCOR*MAPREF**2) -ACOEFR *(MAPREF +
     *                  BCOR*TIMARG*MAPREF**2) -BCOEFR *(MAPREF*2*TIMARG
     *                  + BCOR*TIMARG**2*MAPREF**2)+ A0CLO + A1CLO
                  IF (ISNRNO .LE. 300) THEN
                     WRITE (MSGTXT,3000) SID, IANT, VALRAT, VALCUR
                     CALL MSGWRT (8)
 3000                FORMAT ('SID=', I3,'  IANT=', I3, '  VALRAT=', F10
     *                  .1,'  VALCUR=', F10.1)
                     END IF

                  END IF
               END IF
C---------end simulating rate having used delay at zenith----
C
            VALMX(IANT) = MAX(VALMX(IANT), VALCUR)
            VALMN(IANT) = MIN(VALMN(IANT), VALCUR)
         ELSE
C                                       VALCUE.EQ.FBLANK for all
C                                       IF,STOKES
            VALCUR = FBLANK
            END IF
         VAL(INDTIM+OFFVAL) = VALCUR
         VALSAV(INDTIM+OFFVSA) = VALCUR
         IF (VALCUR.EQ.FBLANK) GO TO 500

         TIMARG = CTIM - TIMCEN
C
C                                       do not estimate RESID for the
C                                       first try when there is not
C                                       a solution yet
         IF (ITER.GT.0) THEN
            RESID = VALCUR
            DO 410 IC=1,NFITA
               IFIT = IC + NFITA*(IANT-1)
               RFIT = IC + NFITA*(RANT-1)
               IF (IC.LE.NFITAT) THEN
                  RESID = RESID - SOL(IFIT)*(TIMARG**(IC-1)) * MAPANT
     *               + SOL(RFIT)*(TIMARG**(IC-1)) * MAPREF
C                                       clock error of IANT
               ELSE
                  RESID = RESID - SOL(IFIT)*(TIMARG**(IC-(NFITAT+1)))
                  END IF
 410           CONTINUE
C                                       flag the point if...
C                                       and skip it
            IF (ABS(RESID).GT.NRMS*RMSD) THEN
               VAL(INDTIM+OFFVAL) = FBLANK
               GO TO 500
               END IF
            END IF
C
         NOBS = NOBS + 1
         SUM = SUM + VALCUR
         SSQ = SSQ + VALCUR*VALCUR
C                                       Prepare result vector R(NFIT)
C                                       and matrix MATR(NFIT*NFIT)
C                                       for routine LEASQR
C  --------------------------------------------------------------------
C I        Each measurement (the SN table row) is fitted as:           |
C |  SUM(I=1..NFITAT) [F(I+NFITA*(IANT-1))] * MAPANT                   |
C | +SUM(I=1..NFITCL) [F(I+NFITAT+NFITA*(IANT-1))                      |
C | -SUM(I=1..NFITAT) [F(I+NFITA*(REFA-1))] * MAPREF                   |
C  --------------------------------------------------------------------
C
         TIMARG = CTIM - TIMCEN
         DO 430 IC=1,NFITA
            IFIT = IC + NFITA*(IANT-1)
            RFIT = IC + NFITA*(RANT-1)


C
            IF (ICODE .LT. 4) THEN
C                                       OPTYPE = 'MDEL', or 'PHAS'
               IF (IC.LE.NFITAT) THEN
C                                       coefficients for zenith
C                                       atmosphere
                  CCANT(IC) = (TIMARG**(IC-1)) * MAPANT
                  CCREF(IC) = (TIMARG**(IC-1)) * (-MAPREF)
               ELSE
C                                       coefficients for clocks
                  CCANT(IC) = TIMARG**(IC-(NFITAT+1))
                  CCREF(IC) = 0
                  END IF
            ELSE
C                                       OPTYPE = 'RATE'
               IF (IC.LE.NFITAT) THEN
C                                       coefficients for zenith
C                                       atmosphere
                  CCANT(IC) =  (MAPANT*(IC-1)*TIMARG**(IC-2) +
     *               MAPANT**2 * BCO* TIMARG**(IC-1))
                  CCREF(IC) = -(MAPREF*(IC-1)*TIMARG**(IC-2) +
     *               MAPREF**2 * BCOR*TIMARG**(IC-1))
               ELSE
C                                       coefficients for clocks
                  CCANT(IC) = (IC-(NFITAT+1))*
     *               TIMARG **(IC-(NFITAT+1) -1)
                  CCREF(IC) = 0
                  END IF
               END IF
C
            R(IFIT) = R(IFIT) + VALCUR*CCANT(IC)
            R(RFIT) = R(RFIT) + VALCUR*CCREF(IC)
C
  430       CONTINUE
C                                       calculate matrix MATR
         DO 450 IC = 1, NFITA
C                                       IFIT,KFIT are column number
            IFIT = IC + NFITA*(IANT-1)
            KFIT = IC + NFITA*(RANT-1)
            DO 440 KC = 1, NFITA
C                                       LFIT,MFIT are row numbers
               LFIT = KC + NFITA*(IANT-1)
               MFIT = KC + NFITA*(RANT-1)
C                                       START OPTYPE = 'MDEL', or 'PHAS'
C
C                                       for IANT,IANT connections
               LIFIT = LFIT + (IFIT-1)*NFIT
               MATR(LIFIT) = MATR(LIFIT) + CCANT(IC)*CCANT(KC)
C                                       for REFN,IANT connections
               MIFIT = MFIT + (IFIT-1)*NFIT
               MATR(MIFIT) = MATR(MIFIT) + CCANT(IC)*CCREF(KC)
C                                       for IANT, REFN connections
               LKFIT = LFIT + (KFIT-1)*NFIT
               MATR(LKFIT) = MATR(LKFIT) + CCREF(IC)*CCANT(KC)
C                                       for REFN, REFN connections
               MKFIT = MFIT + (KFIT-1)*NFIT
               MATR(MKFIT) = MATR(MKFIT) + CCREF(IC)*CCREF(KC)
  440          CONTINUE
  450       CONTINUE
 500     CONTINUE
C                                       store the only reference antenna
      IF (REFEQU) REFNN = RANT
C                                       find the solition for each
C                                       selected antenna atm. senith
C                                       delay and clock error
C                                       All antenna differences were
C                                       used together to find solution
C                                       for all antennas
C
      CALL DLESQR (NFIT, NOBS, SUM, SSQ, R, MATR, SOL, VX, SSQRES,
     *   VARRES, VARY, FIT, LESOL)
C                                       estimate r.m.s of
C                                       the SN table data
      RMSD = SQRT(VARRES)
C                                       or
C      RMSD = SQRT(SSQRES/NOBS)

C                                       go back to next iteration
      ITER = ITER + 1
      IF (ITER.LE.NITER) GO TO 260
C                                       Close table.
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IERR
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('!!!!! center of the data is at ', F10.5, ' !!!!!')
 1100 FORMAT ('Product NANT*NTIME exceeds MAX = ', I6)
 1900 FORMAT ('TABIO: ERROR = ', I3)
      END
      SUBROUTINE SOUSEL (SOURCE, NSOUR, DISK, CNO, CAT, BIF, NNIF,
     *   BUFFER, ID, NID, RA, DEC, SOUS, FLUXS, IRET)
C-----------------------------------------------------------------------
C   Load all selected source identifiers from SU table.
C-----------------------------------------------------------------------
C   Inputs:
C      SOURCE  C*16(*)   List of source names.
C                        If the first character of any source names
C                        begins with a "-", all sources EXCEPT those
C                        named will be returned ( the "-" will be
C                        ignored in determining the source name).
C                        Blank source names are ignored.  Names should
C                        be left justified, blank filled
C      NSOUR     I       Number of entries in SOURCE
C      DISK      I       Disk number of the data set.
C      CNO       I       Catalog slot number of data set.
C      CAT       I(256)  Catalog header.
C      BIF       I       Beginning IF
C      NNIF      I       Number of selected IFs
C   Input/Output:
C      BUFFER    I(512)  Work buffer, used for I/O and manipulating
C                        source lists, should be at least min (512,NID)
C   Output:
C      ID        I(*)    Sources ID numbers of selected sources,
C      NID       I       Number of elements returned in ID.
C      RA        D(*)    Array of selected calibrators' RA
C      DEC       D(*)    Array of selected calibrators' declinations
C      SOUS      C*16(*) Names of selected sources
C      FLUXS     R(*)    Flux density of selected sources
C      IRET      I       Return code. 0 => OK; else failed.
C   Usage notes:
C       This routine uses AIPS LUN 27 which will be closed on normal
C       return. Version 1 of the source table is assumed.
C-----------------------------------------------------------------------
      CHARACTER SOURCE(*)*16, SOUS(*)*16, SOUST(50)*16
      INTEGER   DISK, CNO, NID, CAT(256), BUFFER(*), ID(*), KID, IID,
     *   IDT(50), BIF, NNIF, IRET
C
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INTEGER   VER, LUN, IDKOL, SUKOL,IDSOU, SQUAL, MAXID,
     *   NUMIF, ISURNO, NUMREC, I4, SUFQID, NSOUR, I, J, SKIF, SIF
      LOGICAL   EQUAL, DESEL, ALLBL, GOTIT
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RA(*), DEC(*), PI, DEGRAD, RAT(50), DECT(50),
     *   RAOBS, DECOBS
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC)
      REAL      FLUX(4,MAXIF), FREQO(MAXIF), RESTFQ(MAXIF), FLUXS(*),
     *   FLUXST(800)
      DOUBLE PRECISION LSRVEL(MAXIF)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IDKOL, SUKOLS(1)),   (SUKOL, SUKOLS(2))
      DATA VER, LUN /1, 27/
      DATA PI /3.14159265358979323846D0/
C-----------------------------------------------------------------------
      DEGRAD = PI/180.0D0
      MAXID = NID
      NID = 0
      IRET = 0
      DESEL = .FALSE.
      ALLBL = .TRUE.
      DO 10 I = 1,NSOUR
C                                       Check deselection
         DESEL = DESEL .OR. SOURCE(I)(1:1).EQ.'-'
C                                       Check if all blank, select
         ALLBL = ALLBL .AND. (SOURCE(I).EQ.'                ')
C
 10      CONTINUE
C                                       Initialize SOURCE table.
      CALL SOUINI ('READ', BUFFER, DISK, CNO, VER, CAT, LUN, NUMIF,
     *   VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
         END IF
C                                       number of subrows (IFS)
C                                       Get number of entries
      NUMREC = BUFFER(5)
C                                       Loop through source records.
      DO 500 ISURNO = 1,NUMREC
C                                       Read record
         I4 = ISURNO
         CALL TABSOU ('READ', BUFFER, I4, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, SQUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IRET)
C                                       See is source record turned off
         IF (IRET.LT.0) GO TO 500
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
            END IF
C                                       Check if in list SOURCE.
         GOTIT = .FALSE.
         DO 300 J = 1,NSOUR
C                                       See if deselected.
            IF (.NOT.DESEL) THEN
C                                       Sources selected.
               EQUAL = SOURCE(J).EQ.SOUNAM
C                                       cover blank source
               EQUAL = EQUAL .OR. ALLBL
               IF (EQUAL) THEN
                  IF ((NID+1).LE.MAXID) THEN
                     NID = NID + 1
                     ID(NID) = IDSOU
                     RA(NID) = RAAPP*DEGRAD
                     DEC(NID) = DECAPP*DEGRAD
                     SOUS(NID) = SOUNAM
C                                       take IFLUX
                     DO 20 I = 1, NNIF
                        SIF = I + (NID-1)*NNIF
                        FLUXS(SIF) = FLUX(1,I +BIF -1)
                        IF (FLUXS(SIF).EQ.0.0) FLUXS(SIF) = 1.0
   20                   CONTINUE
                     GO TO 500
                  ELSE
C                                       Too many sources selected
                     WRITE (MSGTXT,1300) MAXID
                     IRET = 5
                     GO TO 990
                     END IF
                  END IF
            ELSE
C                                       Deselected
C                                       Check for leading "-"
               IF (SOURCE(J)(1:1).EQ.'-') THEN
                  EQUAL = SOURCE(J)(2:16).EQ.SOUNAM(1:15)
               ELSE
                  EQUAL = SOURCE(J).EQ.SOUNAM
                  END IF
               GOTIT = GOTIT .OR. EQUAL
               END IF
 300        CONTINUE
            IF (DESEL .AND. (.NOT.GOTIT)) THEN
C                                       Source not deselected
               IF ((NID+1).LE.MAXID) THEN
                  NID = NID + 1
                  ID(NID) = IDSOU
                  RA(NID) = RAAPP*DEGRAD
                  DEC(NID) = DECAPP*DEGRAD
                  SOUS(NID) = SOUNAM
                  DO 320 I = 1, NNIF
                     SIF = I + (NID-1)*NNIF
                     FLUXS(SIF) = FLUX(1,I +BIF -1)
 320                 CONTINUE
               ELSE
C                                       Too many sources selected
                  WRITE (MSGTXT,1300) MAXID
                  IRET = 5
                  GO TO 990
                  END IF
               END IF
 500     CONTINUE
C                                       Close Source table
      CALL TABIO ('CLOS', 0, I4, SOUNAM, BUFFER, IRET)

C                                       sort sources if they named
      IF ((.NOT.DESEL) .AND. (.NOT.ALLBL)) THEN
         KID = 0
         DO 580 J = 1, NSOUR
            DO 560 IID = 1, NID
               IF (SOURCE(J).EQ.SOUS(IID)) THEN
                  KID = KID + 1
                  IDT(KID) = ID(IID)
                  RAT(KID) = RA(IID)
                  DECT(KID) = DEC(IID)
                  SOUST(KID) = SOUS(IID)
                  DO 520 I = 1, NNIF
                     SKIF = I + (KID-1)*NNIF
                     SIF = I + (IID-1)*NNIF
                     FLUXST(SKIF) = FLUXS(SIF)
 520                 CONTINUE
                  END IF
 560           CONTINUE
 580        CONTINUE
         NID = KID
         DO 600 IID = 1, NID
            ID(IID) = IDT(IID)
            RA(IID) = RAT(IID)
            DEC(IID) = DECT(IID)
            SOUS(IID) = SOUST(IID)
            DO 590 I = 1, NNIF
               SIF = I + (IID-1)*NNIF
               FLUXS(SIF) = FLUXST(SIF)
 590          CONTINUE
 600        CONTINUE
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('SOUSEL: ERROR ',I3,' INITIALIZING SOURCE TABLE')
 1120 FORMAT ('SOUSEL: ERROR ',I3,' READING SOURCE TABLE')
 1300 FORMAT ('SOUSEL: MORE SOURCES SELECTED THAN MAX (',I5,')')
      END
      SUBROUTINE ANTSEL (XANT, NANTSL, SUBA, DISK, CNO, CAT,
     *   BUFFER, IRET)
C-----------------------------------------------------------------------
C   Select anntennas from AN file
C-----------------------------------------------------------------------
C   Inputs:
C      XANT      R(*)    List of antennas numbers.
C                        If any of the numbers is negative all antennas
C                        EXCEPT those named will be returned
C      NANTSL    I       Number of entries in XANT
C      DISK      I       Disk number of the data set.
C      CNO       I       Catalog slot number of data set.
C      CAT       I(256)  Catalog header.
C   Outputs sent to COMMON (DANS.INC):
C      NSTNS    I        Number of elements returned in TELNO
C      STNNAM   C(*)*8   Antenna names
C      TELNO    I(*)     Antennas numbers of selected antennas
C      STNX     D(*)     X (meters)
C      STNY     D(*)     Y (meters)
C      STNZ     D(*)     Z (meters)
C      STNLAT   D(*)     Antenna latitude (rad).
C      STNLON   D(*)     Antenna east longitude (rad).
C      STNRAD   D(*)     Antenna radius from earth center (meter)
C      STNEPL   R(2,*)   Feed real/elipticity (poln, IF)
C      STNORI   R(2,*)   Feed imag/orientation (poln, IF)
C      STNPST   C*8      Feed solution type:
C                           'APPROX  ' => linear approximation
C                           'ORI-ELP ' => orientation-ellipticity
C      TIMLAB   C*8      Time system label (e.g. 'IAT', 'UTC')
C      ANTUTC   R        UT1-UTC (time sec)
C      ANTIAT   R        Data time - UTC (sec)
C      GSTIAT   D        GST (rad) at IAT=0 on reference date.
C      ROTIAT   D        Rotation rate of the earth in IAT (Radians/day)
C      ANTUTC   R        UT1-UTC (time sec)
C      ANTIAT   R        Data time - UTC (sec)
C      FQIDAN   I        FQID for which polzn properties determined.
C   Output:
C      BUFFER    I(512)  Work buffer
C      IRET      I       Return code. 0 => OK; else failed.
C   Usage notes:
C       This routine uses AIPS LUN 27 which will be closed on normal
C       return. Version 1 of the source table is assumed.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, IANT, CAT(256), BUFFER(*), I, J, MXAN,
     *   ANTENS(50), NANTSL, LIMIT, NEXT, SUBA, INTANT, IRET
      REAL      XANT(*)
      LOGICAL   EQUAL, DESEL, ALLANT, GOTIT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MXAN = NANTSL
C                                       Get antenna info
      CALL GETANT (DISK, CNO, SUBA, CAT, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
         END IF
C                                       Antenna list
      ALLANT = .TRUE.
      DESEL = .FALSE.
      DO 10 I = 1, NANTSL
         ANTENS(I) = 0
         ALLANT = ALLANT .AND. (ABS (XANT(I)).LE.1.0E-10)
         DESEL = DESEL .OR. (XANT(I).LT.-0.5)
 10      CONTINUE
      IF (ALLANT) THEN
         NANTSL = NSTNS
         GO TO 60
         END IF
      NEXT = 1
C                                       Not all selected - make list
C                                       ANTENNAS array worriing repeating
         DO 50 I = 1,50
            IF (XANT(I).EQ.0) GO TO 50
C                                       See if already have
               INTANT = XANT(I)
               LIMIT = NEXT - 1
               IF (LIMIT.LT.1) GO TO 40
               DO 30 J = 1,LIMIT
                  IF (INTANT.EQ.ANTENS(J)) GO TO 50
 30               CONTINUE
C                                       New antenna
 40               ANTENS(NEXT) = XANT(I)
                  NEXT = NEXT + 1
 50            CONTINUE
      NANTSL = NEXT - 1
 60   CONTINUE
C                                       Setup
      IANT = 0
      IRET = 0
C                                       Loop through AN file records.
      DO 500 I = 1,NSTNS
C                                       Check if in list XANT.
         GOTIT = .FALSE.
         DO 300 J = 1, NANTSL
C                                       See if deselected.
            IF (.NOT.DESEL) THEN
C                                       Sources selected.
               EQUAL = ANTENS(J).EQ.TELNO(I)
C                                       cover blank antens
               EQUAL = EQUAL .OR. ALLANT
               IF (EQUAL) THEN
                  IF ((IANT+1).LE.MXAN) THEN
                     IANT = IANT + 1
                     STNNAM(IANT) = STNNAM(I)
                     TELNO(IANT) = TELNO(I)
                     STNX(IANT) = STNX(I)
                     STNY(IANT) = STNY(I)
                     STNZ(IANT) = STNZ(I)
                     STNLAT(IANT) = STNLAT(I)
                     STNLON(IANT) = STNLON(I)
                     STNRAD(IANT) = STNRAD(I)
                     GO TO 500
                  ELSE
C                                       Too many antenns selected
                     WRITE (MSGTXT,1300) MXAN
                     IRET = 5
                     GO TO 990
                     END IF
                  END IF
            ELSE
C                                       Deselected
               EQUAL = ABS(ANTENS(J)).EQ.TELNO(I)
               GOTIT = GOTIT .OR. EQUAL
               END IF
 300        CONTINUE
            IF (DESEL .AND. (.NOT.GOTIT)) THEN
C                                       Antens not deselected
               IF ((IANT+1).LE.MXAN) THEN
                  IANT = IANT + 1
                  STNNAM(IANT) = STNNAM(I)
                  TELNO(IANT) = TELNO(I)
                  STNX(IANT) = STNX(I)
                  STNY(IANT) = STNY(I)
                  STNZ(IANT) = STNZ(I)
                  STNLAT(IANT) = STNLAT(I)
                  STNLON(IANT) = STNLON(I)
                  STNRAD(IANT) = STNRAD(I)
                  GO TO 500
               ELSE
C                                       Too many antenns selected
                  WRITE (MSGTXT,1300) MXAN
                  IRET = 5
                  GO TO 990
                  END IF
               END IF
 500     CONTINUE
      NSTNS = IANT
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ANTSEL:',I3,' READING ANTENNA TABLE')
 1300 FORMAT ('ANTSEL: MORE ANTENS SELECTED THAN MAX (',I5,')')
      END
      SUBROUTINE PLTFIT (IRET)
C-----------------------------------------------------------------------
C   PLTFIT plots the data thru calls to PLTEL
C   Output:
C      IRET     I  Return code, 0=OK else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
      INTEGER   MFITA
      PARAMETER (MFITA = 200)
      INTEGER   IPLOT, IPLT, IANT, ITIM, IT, IFIT, IIFIT,
     *   MXFIT, ITS, SOUANT, SID
      LOGICAL GOOD
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'DELZN.INC'
      REAL      FITPAR(MFITA), RMS(MFITA), FITREF(MFITA),
     *   RMSREF(MFITA), TOLER(5), TMAX, TMIN,
     *   TDIF, SIZEX, SIZEY, TEMP
      CHARACTER MEAS(3)*10, ATMCLO*6
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA TOLER /2.0, 2.0, 2.0, 0.0, 0.0/
      DATA MEAS /'mm', 'mm/day', 'mm/day/day'/
C-----------------------------------------------------------------------
      IRET = 0
      MXFIT = MAXFIT
C                                       initialize fit parameters
      CALL RFILL (MXFIT, 0.0, FITPAR)
      CALL RFILL (MXFIT, 0.0, FITREF)
C                                       Print out FITREF if there is
C                                       only one reference antenna:
C                                       REFNN
      IF (REFEQU) THEN
         DO 20 IFIT = 1, NFITA
            FITREF(IFIT) = SOL(IFIT + NFITA*(REFNN-1))
C                                       recalculate the variances of
C                                       the fit parameters to its RMS
            RMSREF(IFIT) = 0.0
            IF (VX(IFIT + NFITA*(REFNN-1)).GE.0) RMSREF(IFIT) =
     *         SQRT (VX(IFIT + NFITA*(REFNN-1)))
            IF (PRTLEV.GT.0) THEN
               IF (IFIT .LE. NFITAT) THEN
                  ATMCLO = '|ATMOS'
                  IIFIT = IFIT
               ELSE
                  ATMCLO = '|CLOCK'
                  IIFIT = IFIT - NFITAT
                  END IF
               WRITE (MSGTXT,1010) 'Ref ',TELNO(REFNN), IFIT,
     *               FITREF(IFIT), RMSREF(IFIT), MEAS(IIFIT), ATMCLO
                  CALL MSGWRT (5)
               END IF
20          CONTINUE
         END IF
C                                       Find last plot
      NPLOTS = 0
      DO 50 IANT = 1, NSTNS
         ITIM = 0
         DO 30 SID = 1, NIDC
            SOUANT = SID + NIDC*(IANT-1)
            ITIM = ITIM + ITIME(SOUANT)
 30         CONTINUE
C                                       Skip the antenna if ITIM=0
C                                       because DELUV skiped such lines
C                                       (IANT=REFN)
         IF (ITIM.GT.0) NPLOTS = NPLOTS + 1
 50      CONTINUE
C
      IF (DORES) NPLOTS = 2*NPLOTS

      IF (NPLOTS.LE.0) GO TO 980
C                                       Prepare parameters for PLTFIT
C                                       thru PLTEL
      SIZEX = 1000.0
      SIZEY = 1000.0 / NCOUNT
      IPLT = 0
C                                       Loop through plots
      DO 200 IANT = 1,NSTNS
         CURANT = IANT
         ITIM = 0
         DO 60 SID = 1, NIDC
            SOUANT = SID + NIDC*(IANT-1)
            ITIM = ITIM + ITIME(SOUANT)
 60         CONTINUE
         IF (ITIM.EQ.0) GO TO 200
C
         GMXX = -1.0E20
         GMNX = 1.0E20
C
         ITS = 0
         DO 80 SID = 1,NIDC
            SOUANT = SID + NIDC*(IANT-1)
            ITIM = ITIME(SOUANT)
            DO 70 IT = 1,ITIM
               INDTIM = IT + NTIME(SOUANT)
C                                       exclude blanked points
               TEMP = VAL(INDTIM+OFFVAL)
               IF (APARM(7).GT.0.0) TEMP = VALSAV(INDTIM+OFFVSA)
               IF (TEMP.NE.FBLANK) THEN
                  ITS = ITS + 1
C                                       store the given and reference
C                                       antenna map functions, times,
C                                       values and list of reference
C                                       antennas
                  MAPITS(ITS) = MAPX(INDTIM+OFFELE)
                  MAPRIT(ITS) = MAPRX(INDTIM+OFFREL)
                  TIMITS(ITS) = TIMX(INDTIM+OFFTIM)
                  VALU1(ITS) = TEMP
                  ANTRTS(ITS) = ANTREF(INDTIM+OFFREF)
C                                       store the B coefficients for
C                                       given and reference antennas
                  BCOITS(ITS) = BCOEX(INDTIM+OFFBCO)
                  BCORIT(ITS) = BCOERX(INDTIM+OFFBCR)
                  END IF
 70            CONTINUE
 80         CONTINUE
C                                       solution for gain's coefficients
C
         DO 82 IFIT = 1, NFITA
C                                       NFITA = 3, 4, or 5
            FITPAR(IFIT) = SOL(IFIT + NFITA*(IANT-1))
C                                       recalculate the variances of
C                                       the fit parameters to its RMS
            IF (VX(IFIT+NFITA*(IANT-1)).GE.0) RMS(IFIT) =
     *         SQRT (VX(IFIT + NFITA*(IANT-1)))
            IF (PRTLEV.GT.0) THEN
               IF (IFIT .LE. NFITAT) THEN
                  ATMCLO = '|ATMOS'
                  IIFIT = IFIT
               ELSE
                  ATMCLO = '|CLOCK'
                  IIFIT = IFIT - NFITAT
                  END IF
               WRITE (MSGTXT,1010) ' ', TELNO(IANT), IFIT, FITPAR(IFIT),
     *            RMS(IFIT), MEAS(IIFIT), ATMCLO
               CALL MSGWRT (5)
               END IF
   82       CONTINUE
         IF (DOHIST) THEN
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),1140) TELNO(IANT), FITPAR(1),
     *         FITPAR(2), FITPAR(3)
            END IF
C                                       plot the values and model
C                                       versus time
         GOOD = .FALSE.
         DO 115 IFIT = 1, NFITA
            IF (FITPAR(IFIT).NE.0) GOOD = .TRUE.
  115       CONTINUE
C                                       The antenna is bad if solution
C                                       is not found
         IF (.NOT.GOOD) THEN
            NPLOTS = NPLOTS - 1
            GO TO 200
            END IF
         IPLT = IPLT + 1
         IPLOT = IPLT - 1
         IPLOT = MOD (IPLOT, NCOUNT) + 1
         IF (IPLT.EQ.NPLOTS) IPLOT = -IPLOT
C                                       TMAX, TMIN new MAX&MIN of Y
         TMAX = VALMX(IANT) + 0.1*(VALMX(IANT)-VALMN(IANT))
         TMIN = VALMN(IANT) - 0.1*(VALMX(IANT)-VALMN(IANT))
         IF (ABS (TMAX-TMIN).LT.TOLER(ICODE)) THEN
            TMAX = TMAX + TOLER(ICODE)
            TMIN = TMIN - TOLER(ICODE)
            END IF
C                                       GMXX, GMNX MAX&MIN of Y for all
C                                       plots until the current one
         GMXX = MAX (GMXX, TMAX)
         GMNX = MIN (GMNX, TMIN)
         TDIF = TMAX - TMIN
         IF (ABS (TDIF).LE.1.0E-25) TDIF = 1.0E-25
         XYOFF(2) = TMIN
         XYSCL(2) = SIZEY / TDIF
C                                       now about X-axis
         TMAX = (XAXIMX + 0.1 * (XAXIMX - XAXIMN))
         TMIN = (XAXIMN - 0.1 * (XAXIMX - XAXIMN))
         TDIF = TMAX - TMIN
         IF (ABS (TDIF).LE.0.01) TDIF = 0.01
         XYOFF(1) = TMIN
         XYSCL(1) = SIZEX / TDIF
C                                       plot VALU/TIMITS and model
         IF (DOPLOT) THEN
            DOTWO = .FALSE.
C                                       If ref. antenna is not constant
C                                       do not plot it
            IF (DOREF .AND. .NOT.REFEQU) THEN
               WRITE (MSGTXT,1300)
               CALL MSGWRT (3)
               GO TO 200
               END IF
            CALL PLTEL (IPLOT, ITS, FITPAR, FITREF, IRET)
            IF (IRET.NE.0) DOPLOT = .FALSE.
C                                       plot residuals
            IF (DORES) THEN
               DOTWO  = .TRUE.
               IPLT = IPLT + 1
               IPLOT = IPLT - 1
               IPLOT = MOD (IPLOT, NCOUNT) + 1
               IF (IPLT.EQ.NPLOTS) IPLOT = -IPLOT
               CALL PLTEL (IPLOT, ITS, FITPAR, FITREF, IRET)
               IF (IRET.NE.0) DOPLOT = .FALSE.
               END IF
            END IF
 200     CONTINUE
C                                       SQRT  of residuals variance
      IF (PRTLEV.GT.1) THEN
         WRITE (MSGTXT,1182) SQRT(VARRES)
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       No baselines
 980  IRET = 8
      WRITE (MSGTXT,1200)
      CALL MSGWRT (2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (A4,'ANT ',I2,': FitPar',I2,F11.3,' (',F8.3,') ',A10,1X,
     *   A6)
 1140 FORMAT ('ANT=',I2,': ',1PE9.3,' + ',1PE9.3,'*T + ',1PE9.3,'*T**2')
 1182 FORMAT (2X, 'SQRT of variance of the residuals = ', F6.3, 'mm')
 1200 FORMAT ('PLTFIT: No data for sources selected')
 1300 FORMAT ('Ref. antenna is not constant. So the plots are canceled')
      END
      SUBROUTINE PLTEL (IPLOT, ITIM, FITPAR, FITREF, IRET)
C-----------------------------------------------------------------------
C   PLTEL actually plots data and model.
C   Input:
C      IPLOT   I    Plot number on current page. If neg. then this is
C                   last plot.
C      ITIM    I     Number of points at arrays TIMITS and VALU
C      FITPAR  R(*) Array of parameters of ANT fitting function
C      FITREF  R(*) Array of parameters of REFA fitting function
C   Inputs from Common:
C      TIMITS  R(*)  Array of data arguments
C      VALU    R(*)  Array of data function
C      GMNX    R    Max. value to plot
C      GMXX    R    Min. value to plot
C      XMX     R    Max. x value to plot
C      XMN     R    Min. x value to plot
C   Output:
C      IRET    I    Return code, 0 => OK, otherwise abort.
C                     -1 => user request termination
C                      1 => failed to add to catalog
C                      2 => failed to create
C                      3 => graph file write error
C                      4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   IPLOT, IRET, NPMOD
C
      CHARACTER TEXT*132, PFILE*48, ATIME*8, ADATE*12, AUNITS(28)*8,
     *   CHTYPE(28)*16, CHTMP*18, XUNITS*20, TYPE*2
      INTEGER   ELBUFF(256), VER, IERR, ITYPE, IPSIZE, LUNPL, LTYPE,
     *   FINDPL, DEPTH(5), INCHAR, INP, IT(3), ID(3), IAXLAB, IAPLOT,
     *   I, NGOOD, NNOFIT, LABEL, ITT, IFIRST, ITIM
      INTEGER   IFIT, RANT
      REAL      BLC(2), TRC(2), XYRATO, DX, DY, TR, VALUE, TI,
     *   XY(2), XTRC(2), XBLC(2), TLC(2), PLTINC, YYOFF(2), SIZE,
     *   XMULT(2), XVARIB, YPT, DBY, FITPAR(*), FITREF(*), PREV,
     *   ARGUMT
      REAL      MAPANT, MAPREF, BCO, BCOR
      REAL      VALUMX, VALUMN, TMAX, TMIN, TDIF,
     *   SIZEY, TOLER(5)
      LOGICAL   T, F, GOOD, CATUP, CURENT, PREVOS
      SAVE LABEL, LTYPE, ELBUFF
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'DELZN.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGPH.INC'
      DATA LUNPL /26/
      DATA DEPTH /5*1/
      DATA T, F /.TRUE.,.FALSE./
C                                       meters for icode=3 'rate'
      DATA AUNITS /'mm','mm','1/mm','m', 24*' '/
      DATA CHTYPE / 'Atm zenith depth', 'Clock error',
     *              'Refa atm depth', 'Dispersion', 24*' '/
      DATA XUNITS / 'Time (days)'/
      DATA TOLER /2.0, 2.0, 2.0, 0.0, 0.0/
Ctemporal!!!! add later toler(3) rate
C-----------------------------------------------------------------------
C                                       calculate array of zenith
C                                       delays: data
C                                       minus reference antenna zenith
C                                       delay x its mapping function
C                                       minus clock error, devide MAPF

C                                       and its max/min
      VALUMX = -1.0E10
      VALUMN =  1.0E10
      DO 50 ITT = 1,ITIM
         VALU(ITT) = VALU1(ITT)
         XVARIB = TIMITS(ITT) - TIMCEN
         MAPANT = MAPITS(ITT)
         MAPREF = MAPRIT(ITT)
         BCO  = BCOITS(ITT)
         BCOR = BCORIT(ITT)
C
         RANT = ANTRTS(ITT)
C                                       Fit parameters for the
C                                       reference antenna
         DO 10 IFIT = 1, NFITA
            FITREF(IFIT) = SOL(IFIT + NFITA*(RANT-1))
 10         CONTINUE
C
         IF (DOATMA .AND. DOMOD) THEN
C                                       value of zenith delay
C
C                                       add zenith delay under
C                                       the reference antenna

            DO 20 IFIT = 1,NFITAT
C                                       'mdel' or 'phas'
               IF (ICODE.LT.4) THEN
                  VALU(ITT) = VALU(ITT) +
     *               FITREF(IFIT)*(XVARIB**(IFIT-1))*MAPREF
C                                       'rate'
               ELSE
                  VALU(ITT) = VALU(ITT) +
     *               FITREF(IFIT)*(MAPREF*(IFIT-1)*XVARIB**(IFIT-2)
     *               +MAPREF**2 * BCOR*XVARIB**(IFIT-1))
                  END IF
 20            CONTINUE
C                                       subtract the clock error
            DO 30 IFIT = NFITAT+1,NFITA
C                                       'mdel' or 'phas'
               IF (ICODE.LT.4) THEN
                  VALU(ITT) = VALU(ITT) -
     *               FITPAR(IFIT)*(XVARIB**(IFIT-NFITAT-1))
C                                       'rate'
               ELSE
                  VALU(ITT) = VALU(ITT) - FITPAR(IFIT)* (IFIT-NFITAT-1)*
     *               XVARIB **(IFIT-NFITAT -2)
                  END IF
 30            CONTINUE
C                                       'mdel' or 'phas'
            IF (ICODE.LT.4) THEN
               VALU(ITT) = VALU(ITT)/MAPANT
C                                       'rate'
            ELSE
               VALU(ITT) = VALU(ITT)/(BCO*MAPANT**2)
               DO 32 IFIT = 1, NFITAT
C                                       subtract the expression to get
C                                       polynomial presentation of the
C                                       atmosphere at zenith
                  VALU(ITT) = VALU(ITT) -
     *               FITPAR(IFIT)*(IFIT-1)*XVARIB**(IFIT-2) /
     *               (BCO*MAPANT)
 32               CONTINUE


               END IF
            END IF
C                                       value of clock error
        IF (DOCLOC .AND. DOMOD) THEN
            DO 35 IFIT = 1,NFITAT
C                                       add zenith delay over
C                                       the reference antenna
C                                       'mdel' or 'phas'
               IF (ICODE.LT.4) THEN
                  VALU(ITT) = VALU(ITT) +
     *               FITREF(IFIT)*(XVARIB**(IFIT-1))*MAPREF
C                                       'rate'
               ELSE
                  VALU(ITT) = VALU(ITT) +
     *               FITREF(IFIT)*(MAPREF*(IFIT-1)*XVARIB**(IFIT-2)
     *               +MAPREF**2 * BCOR*XVARIB**(IFIT-1))
                  END IF
C                                       subtract zenith delay over
C                                       the current antenna
C                                       'mdel' or 'phas'
               IF (ICODE.LT.4) THEN
                  VALU(ITT) = VALU(ITT) -
     *               FITPAR(IFIT)*(XVARIB**(IFIT-1))*MAPANT
C                                       'rate'
               ELSE
                  VALU(ITT) = VALU(ITT) -
     *               FITPAR(IFIT)*(MAPANT*(IFIT-1)*XVARIB**(IFIT-2)
     *               +MAPANT**2 * BCO*XVARIB**(IFIT-1))
                  END IF
 35            CONTINUE
C                                       subtract/add the expression
C                                       to get polynomial presentation
C                                       of the clock
            DO 37 IFIT = NFITAT+1, NFITA
C                                       subtract the polynomial for
C                                       rate
               IF (ICODE.GE.4) THEN
                  VALU(ITT) = VALU(ITT) -
     *               FITPAR(IFIT)* (IFIT-NFITAT-1)*
     *               XVARIB **(IFIT-NFITAT -2)
C                                       add  the polynomial for
C                                       delay
                  VALU(ITT) = VALU(ITT) +
     *               FITPAR(IFIT)* XVARIB **(IFIT-NFITAT -1)
                  END IF
 37            CONTINUE
            END IF
C                                       value of reference antenna
C                                       delay using the current
C                                       antenna fitting
         IF ((DOREF .AND. REFEQU) .AND. DOMOD) THEN
            DO 40 IFIT = 1,NFITAT
C                                       subtract zenith delay over
C                                       the current antenna
C                                       'mdel' or 'phas'
               IF (ICODE.LT.4) THEN
                  VALU(ITT) = VALU(ITT) -
     *               FITPAR(IFIT)*(XVARIB**(IFIT-1))*MAPANT
C                                       'rate'
               ELSE
                  VALU(ITT) = VALU(ITT) -
     *               FITPAR(IFIT)*(MAPANT*(IFIT-1)*XVARIB**(IFIT-2)
     *               +MAPANT**2 * BCO*XVARIB**(IFIT-1))
                  END IF
 40            CONTINUE
C                                       subtract the clock error
            DO 45 IFIT = NFITAT+1,NFITA
C                                       'mdel' or 'phas'
               IF (ICODE.LT.4) THEN
                  VALU(ITT) = VALU(ITT) -
     *               FITPAR(IFIT)*(XVARIB**(IFIT-NFITAT-1))
C                                       'rate'
               ELSE
                  VALU(ITT) = VALU(ITT) -
     *               FITPAR(IFIT)* (IFIT-NFITAT-1)*
     *               XVARIB **(IFIT-NFITAT -2)
                  END IF
 45            CONTINUE
C                                       'mdel' or 'phas'
            IF (ICODE.LT.4) THEN
               VALU(ITT) = -VALU(ITT) / MAPREF
C                                       'rate'
            ELSE
               VALU(ITT) = -VALU(ITT)/(BCOR*MAPREF**2)
               DO 47 IFIT = 1, NFITAT
C                                       subtract the expression to get
C                                       polynomial presentation of the
C                                       atmosphere at zenith
                  VALU(ITT) = VALU(ITT) -
     *               FITREF(IFIT)*(IFIT-1)*XVARIB**(IFIT-2) /
     *               (BCOR*MAPREF)
 47               CONTINUE
               END IF
            END IF
C                                       the direct data are at VALU1
         IF (DODIR) THEN
            VALU(ITT) = VALU1(ITT)
            END IF
C                                       Convert to meters if 'rate'
         IF (ICODE.EQ.4) VALU(ITT) = VALU(ITT) / 1000.0
         VALUMX = MAX(VALUMX, VALU(ITT))
         VALUMN = MIN(VALUMN, VALU(ITT))
 50      CONTINUE

C                                       TMAX, TMIN new MAX&MIN of Y
      TMAX = VALUMX + 0.1*(VALUMX - VALUMN)
      TMIN = VALUMN - 0.1*(VALUMX - VALUMN)
      IF (ABS (TMAX-TMIN).LT.TOLER(ICODE)) THEN
         TMAX = TMAX + TOLER(ICODE)
         TMIN = TMIN - TOLER(ICODE)
         END IF
C                                       GMXX, GMNX MAX&MIN of Y for
C                                       this plot
      GMXX = TMAX
      GMNX = TMIN
      TDIF = TMAX - TMIN
      IF (ABS (TDIF).LE.1.0E-25) TDIF = 1.0E-25
      XYOFF(2) = TMIN
      SIZEY = 1000.0 / NCOUNT
      XYSCL(2) = SIZEY / TDIF
      NGOOD = 0
      NNOFIT = 0
      IRET = 3
      CATUP = T
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      XYRATO = 1.0
      PLTINC = 1000. / NCOUNT
C                                       Set window for current plot.
      XBLC(1) = BLC(1)
      XBLC(2) = 1000.0 - ABS (IPLOT) * PLTINC
      XTRC(1) = TRC(1)
      XTRC(2) = XBLC(2) + PLTINC - 1.0
C
      IF (DORES) XTRC(2) =  XTRC(2) - (MOD(IABS(IPLOT),2)+0)*4
      TLC(1) = XBLC(1)
      TLC(2) = XTRC(2)
C                                       Offsets for current plot.
      YYOFF(1) = XBLC(1)
      YYOFF(2) = XBLC(2)
C                                       fool with location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
C                                       LABTYP(LOCNUM)=0 for xaxis=time
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      TR = 1.2 * (GMXX - GMNX)
      TI = TR
      CALL METSCA (TR, CPREF(2,LOCNUM), GOOD)
      XMULT(2) = TR / TI
      CPREF(1,LOCNUM) = ' '
      XMULT(1) = 1.0
      DO 60 I = 1,2
         SIZE = 1000.0
         IF (I.EQ.2) SIZE = PLTINC
         TR = SIZE / XYSCL(I)
         RPLOC(I,LOCNUM) = XBLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I) * XMULT(I)
         AXINC(I,LOCNUM) = TR * XMULT(I) / (XTRC(I) - XBLC(I))
 60      CONTINUE
      CTYP(1,LOCNUM) = XUNITS
      CTYP(2,LOCNUM) = AUNITS(ICODE)
C                                       m/day for rate and DODIR
      IF (DODIR .AND. ICODE.EQ.4) CTYP(2,LOCNUM) = 'm/day'
C                                       Create plot file
      IF (ABS (IPLOT).EQ.1) THEN
C                                       Update catalog header.
         VER = 0
         IRET = 1
         IF (.NOT.DOTV) THEN
            CALL MADDEX ('PL', DISKIN, CNOIN, CATBLK, ELBUFF, CATUP,
     *         'WRIT', VER, IERR)
            IF (IERR.NE.0) THEN
               NCFILE = NCFILE - 1
               GO TO 999
               END IF
            END IF
         CALL ZPHFIL ('PL', DISKIN, CNOIN, VER, PFILE, IERR)
         IF (IERR.NE.0) GO TO 960
         IPSIZE = 0
         ITYPE = 40
         CALL GINIT (DISKIN, CNOIN, PFILE, IPSIZE, ITYPE, NPARM,
     *      XNAMEI, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, ELBUFF, LUNPL,
     *      FINDPL, IERR)
         IRET = 2
         IF (IERR.NE.0) GO TO 960
C                                       Number of characters on each
C                                       side of the plot
         CALL RFILL (4, 0.5, CHOUT)
C                                       Note that TICINC not fully
C                                       initialized as yet. -> INP being
C                                       larger than may be actually
C                                       plotted on this subplot.  This
C                                       is probably desirable.
         CALL CHNTIC (XBLC, XTRC, INP)
         INP = MAX (INP, 3)
C                                       standard labeling
         LABEL = 3
         LTYPE = 3
         CHOUT(1) = INP + 4.0
         CHOUT(2) = 3.333
         CHOUT(4) = 4.666
C                                       Init for line drawing.
         CALL GINITL (BLC, TRC, XYRATO, CHOUT, DEPTH, ELBUFF, IERR)
         IRET = 3
         IF (IERR.NE.0) GO TO 970
         IF (.NOT.DOTV) THEN
            IF (PRTLEV.GT.0) THEN
               WRITE (MSGTXT,1000) VER
               CALL MSGWRT (3)
               END IF
            END IF
         END IF
      IRET = 3
      CATUP = T
C                                       Draw border
      CALL GLTYPE (1, ELBUFF, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GPOS (XBLC(1), XTRC(2), ELBUFF, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XBLC(1), XBLC(2), ELBUFF, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XTRC(1), XBLC(2), ELBUFF, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XTRC(1), XTRC(2), ELBUFF, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XBLC(1), XTRC(2), ELBUFF, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Top labels: type & name
      IF (ABS(IPLOT).EQ.1) THEN
         DX = 0.0
         DY = 1.833
C                                       The second line of the header
         CALL GPOS (BLC(1), TRC(2), ELBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         IF (DOATMA) TEXT = CHTYPE(1)
         IF (DOCLOC) TEXT = CHTYPE(2)
         IF (DOREF)  TEXT = CHTYPE(3)
         IF (DODISP) TEXT = CHTYPE(4)
         IF (DODIR)  TEXT = 'SN table vs fits'
C                                       File name
         CALL H2CHR (18, KHIMNO, CATH(KHIMN), CHTMP)
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTMP(13:18))
         IF (DORES) THEN
            TEXT(17:) = '(+ residuals) vs time for'
            CALL NAMEST (CHTMP, CATBLK(KIIMS), TEXT(45:), INCHAR)
         ELSE
            TEXT(17:) = ' vs time for '
            CALL NAMEST (CHTMP, CATBLK(KIIMS), TEXT(35:), INCHAR)
            END IF
         CALL REFRMT (TEXT, ' ', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, ELBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       the third line of header
         DY = 0.5
         CALL GPOS (BLC(1), TRC(2), ELBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Table type
         TYPE = 'SN'
         IF (DODIR) THEN
            WRITE (TEXT,1005) TYPE, SNVER, OPTYPE
         ELSE
            WRITE (TEXT,1010) TYPE, SNVER, OPTYPE
            END IF
         INP = 7
C
         IF (NSTNS.EQ.1) THEN
            WRITE (TEXT(INP:),1040) STNNAM(CURANT), TELNO(CURANT)
            END IF
C
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, ELBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Date/time/version
         DY = 0.5 + 2 * 1.333
C                                       the first line of the header
         CALL GPOS (BLC(1), TRC(2), ELBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, ATIME, ADATE)
         WRITE (TEXT,1030) VER, ADATE, ATIME
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, ELBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Set up location common
      CALL GPOS (XBLC(1), XTRC(2), ELBUFF, IERR)
      IF (IERR.NE.0) GO TO 970
      DX =  1.5
      DY = -1.8
      INCHAR = 12
      INP = 1
      IF (NSTNS.NE.1) THEN
         WRITE (TEXT(INP:),1040) STNNAM(CURANT), TELNO(CURANT)
         INP = INP + 16
         END IF
      IF ((NSTNS.NE.1) .OR. (NS.NE.1) .OR. (NNIF.NE.1)) THEN
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, ELBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Blank bottom label.
      IF ((IPLOT.GE.0) .AND. (ABS (IPLOT).NE.NCOUNT)) THEN
         CPREF(1,LOCNUM) = ' '
         CTYP(1,LOCNUM) = ' '
         END IF

      IAXLAB = NCOUNT / 2 + 1
      IAPLOT = ABS (IPLOT)
C                                       Only label Y axis once.
C                                       comment to plot vertical labels
C                                       for each plot
C      IF ((IAPLOT.NE.IAXLAB) .AND. ((IPLOT.GE.0) .OR.
C     *   (IAPLOT.GT.IAXLAB))) CPREF(2,LOCNUM) = '-1'
C                                       vertical units only for the
C                                       first plots; later for the
C                                       second ones
      IF (DOTWO) CPREF(2,LOCNUM) = '-1'
C
C                                       Put on labels and ticks
C                                       only for the first plot
      IF (.NOT.DOTWO) THEN
         CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, ELBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       take data
C                                       Size of symbol.
      DX = 5.0
      DY = 5.0
      DBY = 0.5
C                                       prepare the data for plot
      CALL GLTYPE (4, ELBUFF, IERR)
      IF (IERR.NE.0) GO TO 970
C
      VALUMX = -1.0E10
      VALUMN =  1.0E10
      DO 70 ITT = 1, ITIM
         XVARIB = TIMITS(ITT)
C
         RANT = ANTRTS(ITT)
         MAPANT = MAPITS(ITT)
         MAPREF = MAPRIT(ITT)
         BCO  = BCOITS(ITT)
         BCOR = BCORIT(ITT)
C
         IF (DOTWO) THEN
C                                       calculate model for the
C                                       following subtruction

            ARGUMT = XVARIB - TIMCEN
C
            IF (DOATMA .AND. DOMOD) THEN
               VALUE = 0
               DO 61 IFIT = 1, NFITAT
                  VALUE = VALUE + FITPAR(IFIT)*ARGUMT**(IFIT-1)
 61               CONTINUE
               END IF
C
            IF (DOCLOC .AND. DOMOD) THEN
               VALUE = 0
               DO 62 IFIT = NFITAT+1, NFITA
                  VALUE = VALUE + FITPAR(IFIT)*ARGUMT**(IFIT-NFITAT-1)
 62               CONTINUE
               END IF
C
            IF ((DOREF .AND. REFEQU) .AND. DOMOD) THEN
               VALUE = 0
               DO 63 IFIT = 1, NFITAT
                  VALUE = VALUE + FITREF(IFIT)*ARGUMT**(IFIT-1)
 63               CONTINUE
               END IF
C
            IF (DODIR) THEN
C                                       direct data to plot
               IF (ICODE.LT.4) THEN
C                                       'mdel' or 'phas'
                  VALUE = 0
C                                       add the zenith atm. at ANT
                  DO 64 IFIT = 1, NFITAT
                     VALUE = VALUE + (FITPAR(IFIT)*ARGUMT**(IFIT-1)) *
     *                  MAPANT
 64                  CONTINUE
C                                       subtract the reference antenna
                  DO 65 IFIT = 1, NFITAT
                     VALUE = VALUE - (FITREF(IFIT)*ARGUMT**(IFIT-1)) *
     *                  MAPREF
 65                  CONTINUE
C                                       add the clock
                  DO 66 IFIT = NFITAT+1, NFITA
                     VALUE = VALUE + FITPAR(IFIT)*ARGUMT**(IFIT
     *                  -NFITAT-1)
 66                  CONTINUE

               ELSE
C                                       'rate'
                  VALUE = 0
C                                       add the two terms at IANT-RANT
                  DO 67 IFIT = 1, NFITAT
C                                       IANT
                     VALUE = VALUE +
     *                  FITPAR(IFIT)*(MAPANT*(IFIT-1)*ARGUMT**(IFIT-2)
     *                  +MAPANT**2 * BCO*ARGUMT**(IFIT-1))
C                                       -RANT
     *                  -FITREF(IFIT)*(MAPREF*(IFIT-1)*ARGUMT**(IFIT-2)
     *                  -MAPREF**2 * BCOR*ARGUMT**(IFIT-1))
 67                  CONTINUE
C                                       add the clock
                  DO 68 IFIT = NFITAT+1, NFITA
                     VALUE = VALUE + FITPAR(IFIT)*(IFIT-NFITAT-1)*
     *                  ARGUMT**(IFIT-NFITAT-2)
 68                  CONTINUE
                  END IF
               END IF
C                                       convert VALUE to meter if 'rate'
            IF (ICODE.EQ.4) VALUE = VALUE / 1000.0
C                                       subtract the model
            VALUE = VALU(ITT) - VALUE
C
            VALUMX = MAX(VALUMX, VALUE)
            VALUMN = MIN(VALUMN, VALUE)
         ELSE
            VALUE = VALU(ITT)
            END IF
C                                       store the VALUE at VALU()
         VALU(ITT) = VALUE
 70      CONTINUE
C                                       make the different scale for
C                                       the second plots (residuals)
      IF (DOTWO) THEN
C                                       TMAX, TMIN new MAX&MIN of Y
         TMAX = VALUMX + 0.1*(VALUMX - VALUMN)
         TMIN = VALUMN - 0.1*(VALUMX - VALUMN)
         IF (ABS (TMAX-TMIN).LT.TOLER(ICODE)) THEN
            TMAX = TMAX + TOLER(ICODE)
            TMIN = TMIN - TOLER(ICODE)
            END IF
C                                       GMXX, GMNX MAX&MIN of Y for
C                                       this plot
         GMXX = TMAX
         GMNX = TMIN
         TDIF = TMAX - TMIN
         IF (ABS (TDIF).LE.1.0E-25) TDIF = 1.0E-25
         XYOFF(2) = TMIN
         SIZEY = 1000.0 / NCOUNT
         XYSCL(2) = SIZEY / TDIF
C                                       LABTYP(LOCNUM)=0 for xaxis=time
         LABTYP(LOCNUM) = 0
         AXTYP(LOCNUM) = 0
         TR = 1.2 * (GMXX - GMNX)
         TI = TR
C
         CALL METSCA (TR, CPREF(2,LOCNUM), GOOD)
         XMULT(2) = TR / TI
         CPREF(1,LOCNUM) = ' '
         XMULT(1) = 1.0
         DO 600 I = 1,2
            SIZE = 1000.0
            IF (I.EQ.2) SIZE = PLTINC
            TR = SIZE / XYSCL(I)
            RPLOC(I,LOCNUM) = XBLC(I)
            RPVAL(I,LOCNUM) = XYOFF(I) * XMULT(I)
            AXINC(I,LOCNUM) = TR * XMULT(I) / (XTRC(I) - XBLC(I))
  600       CONTINUE
C                                       Put on labels and ticks
         CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, ELBUFF, IERR)
         IF (IERR.NE.0) GO TO 970

         END IF
C                                       Loop to plot the data
      DO 75 ITT = 1, ITIM
         XVARIB = TIMITS(ITT)
         VALUE = VALU(ITT)
         IF (VALUE.NE.FBLANK) THEN
C                                       Scale X, Y
            XY(1) = XVARIB
            XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
            IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1))) THEN
               NNOFIT = NNOFIT + 1
               GO TO 75
               END IF
            XY(2) = VALUE
            XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
            IF ((XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))) THEN
               NNOFIT = NNOFIT + 1
               GO TO 75
               END IF
            NGOOD = NGOOD + 1
C                                       Mark point
            DY = 5.0
            YPT = XY(2) + DY
            IF (YPT.GT.XTRC(2)) YPT = XTRC(2)
            CALL GPOS (XY(1), YPT, ELBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            YPT = XY(2) - DY
            IF (YPT.LT.XBLC(2)) YPT = XBLC(2)
            CALL GVEC (XY(1), YPT, ELBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
 75      CONTINUE
C-----------------------------------------------------------------------
C                                       Draw the model
C                                       NPMOD- number of points in model
C                                       draw
      NPMOD = ITIM
      IF (DOMOD .OR. DOTWO) NPMOD = 100
      IFIRST = 0
C
      DX = 5.0
      DY = 5.0
C
      DO 120 ITT = 1, NPMOD
         XVARIB = TIMITS(ITT)
         IF (DOMOD .OR. DOTWO)
     *      XVARIB = XAXIMN + (ITT-1)*((XAXIMX-XAXIMN)/(NPMOD-1.0))
         ARGUMT = XVARIB - TIMCEN
         MAPANT = MAPITS(ITT)
         MAPREF = MAPRIT(ITT)
         BCO  = BCOITS(ITT)
         BCOR = BCORIT(ITT)
C
         IF (DOATMA .AND. DOMOD) THEN
            VALUE = 0
            DO 80 IFIT = 1, NFITAT
               VALUE = VALUE + FITPAR(IFIT)*ARGUMT**(IFIT-1)
 80            CONTINUE
            END IF
C
         IF (DOCLOC .AND. DOMOD) THEN
            VALUE = 0
            DO 90 IFIT = NFITAT+1, NFITA
               VALUE = VALUE + FITPAR(IFIT)*ARGUMT**(IFIT-NFITAT-1)
 90            CONTINUE
            END IF
C
         IF ((DOREF .AND. REFEQU) .AND. DOMOD) THEN
            VALUE = 0
            DO 110 IFIT = 1, NFITAT
               VALUE = VALUE + FITREF(IFIT)*ARGUMT**(IFIT-1)
 110           CONTINUE
            END IF
C
C                                       direct data to plot
         IF (DODIR) THEN
C                                       'mdel' or 'phas'
            IF (ICODE.LT.4) THEN
               VALUE = 0
C                                       add the zenith atm. at ANT
               DO 112 IFIT = 1, NFITAT
                  VALUE = VALUE + (FITPAR(IFIT)*ARGUMT**(IFIT-1)) *
     *               MAPANT
 112              CONTINUE
C                                       subtract the reference antenna
               DO 113 IFIT = 1, NFITAT
                  VALUE = VALUE - (FITREF(IFIT)*ARGUMT**(IFIT-1)) *
     *               MAPREF
 113              CONTINUE
C                                       add the clock
               DO 114 IFIT = NFITAT+1, NFITA
                  VALUE = VALUE + FITPAR(IFIT)*ARGUMT**(IFIT-NFITAT-1)
 114              CONTINUE
C                                       'rate'
            ELSE
               VALUE = 0
C                                       add the two terms at IANT-RANT
               DO 115 IFIT = 1, NFITAT
C                                       IANT
                  VALUE = VALUE +
     *               FITPAR(IFIT)*(MAPANT*(IFIT-1)*ARGUMT**(IFIT-2)
     *               +MAPANT**2 * BCO*ARGUMT**(IFIT-1))
C                                       -RANT
     *               -FITREF(IFIT)*(MAPREF*(IFIT-1)*ARGUMT**(IFIT-2)
     *               -MAPREF**2 * BCOR*ARGUMT**(IFIT-1))
 115              CONTINUE
C                                       add the clock
               DO 116 IFIT = NFITAT+1, NFITA
                  VALUE = VALUE + FITPAR(IFIT)*(IFIT-NFITAT-1)*
     *               ARGUMT**(IFIT-NFITAT-2)
 116              CONTINUE
               END IF
            END IF
C                                       convert to meters if 'rate'
         IF (ICODE.EQ.4) VALUE = VALUE / 1000.0
C                                       Model is zero for for the
C                                       residual plot
         IF (DOTWO) VALUE = 0
C                                       Scale X, Y
         XY(1) = XVARIB
         XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
         IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1))) GO TO 120
         XY(2) = VALUE
         XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
C                                       move to the first point
         IFIRST = IFIRST + 1
C
         IF (IFIRST.EQ.1) THEN
            CALL GPOS (XY(1), XY(2), ELBUFF, IRET)
         ELSE
            CURENT = (XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))
            PREVOS = (PREV .LT.XBLC(2)) .OR. (PREV .GT.XTRC(2))
C
            IF (CURENT .OR. PREVOS) THEN
               CALL GPOS (XY(1), XY(2), ELBUFF, IRET)
            ELSE
               IF (DOMOD .OR. DOTWO) THEN
                  CALL GVEC (XY(1), XY(2), ELBUFF, IRET)
               ELSE
C                                       model is drawn by '-'
                  CALL GPOS (XY(1)+DX, XY(2), ELBUFF, IERR)
                  IF (IERR.NE.0) GO TO 970
                  CALL GVEC (XY(1)-DX, XY(2), ELBUFF, IERR)
                  IF (IERR.NE.0) GO TO 970
                  END IF
               END IF
            END IF
         PREV = XY(2)
 120     CONTINUE
C--------------------------------------------------------------
C                                       Done: finish plot
      IF (PRTLEV.GT.0) THEN
         WRITE (MSGTXT,1200) NGOOD
         CALL MSGWRT (3)
         IF (NNOFIT.GE.1) THEN
            WRITE (MSGTXT,1201) NNOFIT
            CALL MSGWRT (3)
            END IF
      END IF
      IF ((IPLOT.GT.0) .AND. (ABS(IPLOT).LT.NCOUNT)) GO TO 210
         GPHPAG = IPLOT.GT.0
         CALL GFINIS (ELBUFF, IERR)
         IF (IERR.GT.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, CNOIN, VER, ELBUFF, IERR)
            IERR = 0
            END IF
 210     IF (IERR.GT.0) GO TO 975
         IRET = MIN (IERR, 0)
         GO TO 999
C                                       ZPHFIL or GINIT failure.
 960  IF (PRTLEV.GT.0) THEN
         WRITE (MSGTXT,1960)
         CALL MSGWRT (8)
         END IF
      IF (.NOT.DOTV) THEN
         CALL DELEXT ('PL', DISKIN, CNOIN, 'WRIT', CATBLK, ELBUFF,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
      GO TO 999
C                                       Try to finish partial graph
 970  IF (PRTLEV.GT.0) THEN
         WRITE (MSGTXT,1970)
         CALL MSGWRT (6)
         WRITE (MSGTXT,1200) NGOOD
         CALL MSGWRT (2)
         IF (NNOFIT.GE.1) THEN
            WRITE (MSGTXT,1201) NNOFIT
            CALL MSGWRT (2)
            END IF
         END IF
      GPHPAG = IPLOT.GT.0
      CALL GFINIS (ELBUFF, IERR)
      IF (IERR.NE.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, CNOIN, VER, ELBUFF, IERR)
            IERR = 0
            END IF
         GO TO 999
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKIN, PFILE, IERR)
         CALL DELEXT ('PL', DISKIN, CNOIN, 'WRIT', CATBLK, ELBUFF,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Plot file version',I4,'  created.')
 1005 FORMAT (A2,I4, '  OPTYPE=', A4, ';', 5X, 'data(I),  model(-)')
 1010 FORMAT (A2,I4, '  OPTYPE=', A4)
 1030 FORMAT ('Plot file version',I4,'__created ',A, A)
 1040 FORMAT (A8,' (',I2,')_')
 1200 FORMAT ('PLTEL: ',I9,' points plotted')
 1201 FORMAT ('PLTEL: ',I9,' points did not fit')
 1960 FORMAT ('PLTEL: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('PLTEL: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
      END
      SUBROUTINE OUTCL (IERR)
C-----------------------------------------------------------------------
C   OUTCL makes the corrections of the CL table No high+1
C   Output: IERR  I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER MAXLIN
      PARAMETER (MAXLIN=100000)
      INTEGER   LUN, IRCODE, THSOU, ANT, SID, IANT,
     *   ICLRNO, NUMREC, LOOP, ILINES
      DOUBLE PRECISION   TCLT
      INTEGER   LUNPR, PFIND, NCH, ITRIM, NTIMES
      CHARACTER LINE*80, TEMOUT(MAXLIN)*80
      LOGICAL   F
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'DELZN.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN /29/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      FIXCNT = 0
C                                       Open CL table
      NUMPOL = 1
      IF (CATBLK(KINAX+JLOCS).GT.1) NUMPOL = 2
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
C                                       Reformat table?
      IF (DOCL) THEN
         CALL CLREFM (DISKIN, CNOIN, CLUSE, CATBLK, LUN, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL CALINI ('WRIT', BUFFER, DISKIN, CNOIN, CLUSE, CATBLK,
     *      LUN, ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF,
     *      NTERM, GMMOD, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL CALINI ('READ', BUFFER, DISKIN, CNOIN, CLVER, CATBLK,
     *      LUN, ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF,
     *      NTERM, GMMOD, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
      IRCODE = 0
C                                       Update table
      NTIMES = 100
      IF (DOCL) THEN
C                                       calculate output file for
C                                       the CL times
         FIXCNT = 0
C
         DOCLTM = .TRUE.
         DO 100 LOOP = 1,NUMREC
            ICLRNO = LOOP
            CALL TABIO ('READ', IRCODE, ICLRNO, CLRECR, BUFFER, IERR)
            IF (IERR.GT.0) GO TO 900
            IF (IERR.LT.0) GO TO 100
C                                       Check data
C                                       Time:
            IF ((CLRECD(TIMCL).LT.TIMBEG) .OR.
     *         (CLRECD(TIMCL).GT.TIMEND)) GO TO 100
C                                       Subarray
            IF ((CLRECI(SUBCL).NE.SUBA) .AND. (CLRECI(SUBCL).GT.0))
     *            GO TO 100
C                                       Freq id
            IF ((CLRECI(FRQCL).NE.FREQID) .AND. (CLRECI(FRQCL).GT.0)
     *            .AND.(FREQID.GT.0)) GO TO 100
C                                       Check source
            THSOU = CLRECI(SOUCL)
            DO 30 SID = 1, NIDS
               IF (THSOU.EQ.IDS(SID)) GO TO 40
 30            CONTINUE
            GO TO 100
 40         RA = RAS(SID)
            DEC = DECS(SID)
C                                       Check antenna
            ANT = CLRECI(ANTCL)
            DO 50 IANT = 1,NSTNS
               IF (TELNO(IANT).EQ.ANT) GO TO 60
 50          CONTINUE
            GO TO 100
C                                       count
 60         FIXCNT = FIXCNT + 1
            IF (FIXCNT.GT.MAXLIN) THEN
               WRITE (MSGTXT,1240) MAXLIN
               CALL MSGWRT (8)
               WRITE (MSGTXT,1250)
               IERR = 1
               GO TO 990
               END IF
C                                       Correct record.
C                                       Correction of the CL row
C                                       based on the vertical atmosphere
C                                       delay, clock error and elevation
C                                       of the source
C
C                                       time at the CL table line,
C                                       in days
            TCLT = CLRECD(TIMCL) - TIMCEN
            IF (ICODE.NE.3) THEN
               CALL ATMOV (TCLT, IANT, TEMOUT, IERR)
            ELSE
               CALL DISPV (TCLT, IANT, TEMOUT, IERR)
               END IF
            IF (IERR.NE.0) GO TO 100
C
            CALL TABIO ('WRIT', IRCODE, ICLRNO, CLRECR,
     *         BUFFER,IERR)
            IF (IERR.GT.0) GO TO 900
 100        CONTINUE
C                                       calculate output file for
C                                       even distributed NTIMES
         DOCLTM = DOCLT1
         IF (.NOT.DOCLTM) THEN
            FIXCNT = 0
            DO 120 IANT = 1,NSTNS
               DO 110 LOOP = 1,NTIMES+1
                  TCLT = TIMSTA + (LOOP-1) * (TIMFIN-TIMSTA)/NTIMES
     *               - TIMCEN
C                                       Correct record.
                  FIXCNT = FIXCNT + 1
C                                       calculation of the output file
                  IF (ICODE.NE.3) THEN
                     CALL ATMOV (TCLT, IANT, TEMOUT, IERR)
                  ELSE
                     CALL DISPV (TCLT, IANT, TEMOUT, IERR)
                     END IF
 110              CONTINUE
 120           CONTINUE
            END IF
C                                       .NOT.DOCL
      ELSE
C                                       calculate output file for
C                                       even distributed NTIMES
C
C                                       At the beginning:
C                                       IF(.NOT.DOCL) DOCLTM = .FALSE.
         FIXCNT = 0
         DO 140 IANT = 1,NSTNS
            DO 130  LOOP = 1,NTIMES+1
               TCLT = TIMSTA + (LOOP-1) * (TIMFIN-TIMSTA)/NTIMES
     *            - TIMCEN
C                                       Correct record.
               FIXCNT = FIXCNT + 1
C                                       calculation of the output file
               IF (ICODE.NE.3) THEN
                  CALL ATMOV (TCLT, IANT, TEMOUT, IERR)
               ELSE
                  CALL DISPV (TCLT, IANT, TEMOUT, IERR)
                  END IF
 130           CONTINUE
 140        CONTINUE
         END IF
C                                       record in the OUTFILE:
C                                       number of lines-FIXCNT
C                                       and following FIXCNT lines
C
C                                       open the OUTFILE
      IF (DOUTFI) THEN
         LUNPR = 3
         CALL ZTXOPN ('WRIT', LUNPR, PFIND, OFILE, .TRUE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPENING OUTPUT TEXT FILE'
            CALL MSGWRT (8)
            IERR = 0
            GO TO 160
            END IF
         WRITE (LINE,1270) FIXCNT
         NCH = ITRIM (LINE)
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITE OUTPUT TEXT FILE'
            CALL MSGWRT (8)
            IERR = 0
            GO TO 155
            END IF
         DO 150 ILINES = 1, FIXCNT
            LINE = TEMOUT(ILINES)
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH),
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'WRITE OUTPUT TEXT FILE'
               CALL MSGWRT (8)
               IERR = 0
               GO TO 155
               END IF
 150        CONTINUE
C                                       close the OUTFILE
 155     CALL ZTXCLS (LUNPR, PFIND, IERR)
         END IF
C
 160  IF (DOCL) THEN
         NUMHIS= NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2900) FIXCNT
         WRITE (MSGTXT,2901) FIXCNT, CLUSE
         CALL MSGWRT (6)
         END IF
C                                       Close table.
      CALL TABIO ('CLOS', IRCODE, LOOP, CLRECR, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IERR
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OUTCL: ERROR',I5,' ON ',A)
 1900 FORMAT ('TABIO ERROR',I3,' CORRECTING CL TABLE')
 2900 FORMAT (' / ',I6,' Records modified')
 2901 FORMAT (I6,' Records of the CL table ', I2, ' modified')
 1240 FORMAT ('Number of lines in OUTFILE exceeds MAX = ', I6)
 1250 FORMAT ('So use even times for OUTFILE: APARM(8)=1 .OR. ask LK')
 1270 FORMAT (I5)
      END
      SUBROUTINE ATMOV (TCLT, IANT, TEMOUT, IERR)
C-----------------------------------------------------------------------
C   Routine to apply atmospheric and clock error corrections to the
C   given CL table row on the basis of the fitted polynomials and
C   the source elevation
C   Input:
C      TCLT     D      Time
C      IANT     I      antenna number as it selected
C   Control info from common:
C      ISTOK    I      Polarization to correct, 1=first, 2=second,
C                      0 = both
C   Output:
C      TEMOUT   C(*)   File of zenith delays for each selected antennas
C                      and times
C      IERR     I      Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IANT, IERR
      DOUBLE PRECISION TCLT
      CHARACTER TEMOUT(*)*80
C
      INTEGER   I, ITEMP, IFIT, POLFAC, TIME(3)
      REAL      XT, YT, PDLYAT, PDLYCL, PDLY, DPDLAT, DPDLCL, DPDLY,
     *   DPDATZ, CFAC, SFAC, FQFAC, HA, DELDT, COSZ, ELERAD, MAPF,
     *   SECNDS, CTIMR
      CHARACTER TSIGN*1
      DOUBLE PRECISION FREQS, COSLAT, SINLAT, SIDT, DELZAV,
     *   CLOCAV
C                                       speed of light (meters/sec)
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'DELZN.INC'
      REAL      FITPAR(MAXFIT)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA SIDT /1.00273790935D0/
C-----------------------------------------------------------------------
      IERR = 0
C                                       time at the CL table line,
C                                       or even distributed time
C                                       in days
C      TCLT = CLRECD(TIMCL) - TIMCEN
C      CTIMR = CLRECD(TIMCL)
      CTIMR = TCLT + TIMCEN
C
      DO 10 IFIT = 1, NFITA
         FITPAR(IFIT) = SOL(IFIT + NFITA*(IANT-1))
 10      CONTINUE
C                                       estimate the atmzen delay, mm
      DELZAV = 0
      DO 20 IFIT = 1, NFITAT
         DELZAV = DELZAV + FITPAR(IFIT)*TCLT**(IFIT-1)
 20      CONTINUE
C                                       estimate the clock error, mm
      CLOCAV = 0
      DO 30 IFIT = NFITAT+1, NFITA
         CLOCAV = CLOCAV + FITPAR(IFIT)*TCLT**(IFIT-NFITAT-1)
 30      CONTINUE
C
C                                       derivative of atmosphere
C                                       at zenith in sec/sec
      DPDATZ = 0
      DO 40 IFIT = 2,NFITAT
         DPDATZ = DPDATZ + (IFIT-1)*FITPAR(IFIT)*TCLT**(IFIT-2)
 40      CONTINUE
      DPDATZ = DPDATZ/24./60./60./1000./VELITE
C                                       derivative (drift) of clocks
C                                       in sec/sec
      DPDLCL = 0
      POLFAC = 1
      DO 50 IFIT = NFITAT+2, NFITA
         POLFAC = POLFAC*(IFIT-NFITAT-1)
         DPDLCL = DPDLCL + POLFAC*FITPAR(IFIT)*TCLT**(IFIT-NFITAT-2)
 50      CONTINUE
      DPDLCL = DPDLCL/24./60./60./1000./VELITE
C                                       record in the  temp FILE:
C                                       TELNO(IANT), CTIM
C                                       and DELZAV (in cm)
C                                       and CLOCAV (in cm)
C                                       derivative of the zenith
C                                       atmosphere *E14
C                                       clock drift *E14
      IF (DOUTFI) THEN
         CALL TFDHMS (CTIMR, 1, TSIGN, TIME, SECNDS)
         WRITE (TEMOUT(FIXCNT), 1250) STNNAM(IANT)(1:3), TSIGN,
     *      TIME, SECNDS, DELZAV/10, CLOCAV/10, DPDATZ*1.0E14,
     *      DPDLCL*1.0E14
         END IF
      IF (.NOT. DOCL) GO TO 999
C
      IF (.NOT.DOCLTM) GO TO 999
C                                       antenna number at the CL table
C                                       line
      COSLAT = COS (STNLAT(IANT))
      SINLAT = SIN (STNLAT(IANT))
C                                       calculate elevation of the
C                                       antenna for the given source
      HA = TWOPI*SIDT*CTIMR + GSTIAT + STNLON(IANT) - RA
      COSZ = DCOS(STNLAT(IANT))*DCOS(DEC)*COS(HA) +
     *   DSIN(STNLAT(IANT))*DSIN(DEC)
C                                       source is under horizon
      IF (COSZ.LT.0.0) THEN
         IERR = 1
         GO TO 999
      ELSE
         ELERAD = PI / 2.0 - ACOS (COSZ)
         END IF
      FREQS = FREQ + FREQO(BIF)
      SINDEC = SIN (DEC)
      COSDEC = COS (DEC)

C                                       recalculate the  zenith delay
C                                       to the given elevation
C                                       using 1/SIN(EL) as a mapping
C                                       function and add the clock error
      IF (ELERAD.GT.0.01) THEN
         MAPF = 1 / SIN(ELERAD)
C                                       delays in second
         PDLYAT = DELZAV * MAPF/ 1000 / VELITE
         PDLYCL = CLOCAV / 1000 / VELITE
C                                       Correct only atmosphere?
         IF (DOCLAT) THEN
            PDLY = PDLYAT
         ELSE
            PDLY = PDLYAT + PDLYCL
            END IF
      ELSE
         PDLY = 0
         END IF
C                                       DELDT is derivative of ELV
C                                       (by time) multiplied by
C                                       COS(ELV)
      DELDT = -7.29211E-5 * SIN (HA) * COSLAT * COSDEC
C
C                                       derivative of atmosphere
C                                       in sec/sec
      DPDLAT = -PDLYAT * DELDT
      IF (ELERAD.GT.0.01) THEN
         DPDLAT = (DPDLAT + DPDATZ) / SIN(ELERAD)
      ELSE
         DPDLAT = 0
         END IF
C
C                                       Correct only atmosphere?
      IF (DOCLAT) THEN
         DPDLY = DPDLAT
      ELSE
         DPDLY = DPDLAT + DPDLCL
         END IF
C                                       Atmospheric group delay
      IF (CLRECR(ATMCL).NE.FBLANK) CLRECR(ATMCL) = CLRECR(ATMCL)
     *      - PDLYAT
C                                       Atmospheric group delay rate
      IF (CLRECR(DATMCL).NE.FBLANK) CLRECR(DATMCL) = CLRECR(DATMCL) -
     *   DPDLAT
C                                       clock shift
      IF (.NOT.DOCLAT .AND. CLRECR(CLK1CL).NE.FBLANK)
     *   CLRECR(CLK1CL) = CLRECR(CLK1CL) - PDLYCL
C                                       clock drift
      IF (.NOT.DOCLAT .AND. CLRECR(DCK1CL).NE.FBLANK)
     *   CLRECR(DCK1CL) = CLRECR(DCK1CL) - DPDLCL
C                                       multiband delay
      IF (CLRECR(MBD1CL).NE.FBLANK) CLRECR(MBD1CL) = CLRECR(MBD1CL)
     *      + PDLY
C                                       first Stokes
      IF (ISTOK.NE.2) THEN
         DO 100 I = BIF,EIF
            FQFAC = (FREQS+FRQOFF(I)) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE1CL+I-1)
            YT = CLRECR(IM1CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE1CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM1CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
            IF (CLRECR(DE1CL+I-1).NE.FBLANK)
     *         CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + PDLY
C                                       Phase rate
            IF (CLRECR(RA1CL+I-1).NE.FBLANK)
     *         CLRECR(RA1CL+I-1) = CLRECR(RA1CL+I-1) + DPDLY
 100        CONTINUE
         END IF
C                                       second Stokes
      IF ((ISTOK.NE.1) .OR. (NS.NE.1)) THEN
C                                       clock shift
         IF (CLRECR(CLK2CL).NE.FBLANK) CLRECR(CLK2CL) = CLRECR(CLK2CL)
     *         - PDLYCL
C                                       clock drift
         IF (CLRECR(DCK2CL).NE.FBLANK) CLRECR(DCK2CL) = CLRECR(DCK2CL) -
     *      DPDLCL
C                                       multi delay
         IF (CLRECR(MBD2CL).NE.FBLANK) CLRECR(MBD2CL) = CLRECR(MBD2CL)
     *      - PDLY
         DO 120 I = BIF,EIF
C                                       PDLY => correct both atm.
C                                       delay and clock shift
            FQFAC = (FREQS+FRQOFF(I)) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE2CL+I-1)
            YT = CLRECR(IM2CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE2CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM2CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
           IF (CLRECR(DE2CL+I-1).NE.FBLANK)
     *         CLRECR(DE2CL+I-1) = CLRECR(DE2CL+I-1) + PDLY
C                                       Phase rate
            IF (CLRECR(RA2CL+I-1).NE.FBLANK)
     *         CLRECR(RA2CL+I-1) = CLRECR(RA2CL+I-1) + DPDLY
 120        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1250 FORMAT (A3,A1,3I3,F5.1,F9.4,2X,F9.4,2X,F10.5,2X,F10.5)
      END
      SUBROUTINE DISPV (TCLT, IANT, TEMOUT, IERR)
C-----------------------------------------------------------------------
C   Routine to apply dispersion corrections to the given CL table row on
C   the basis of the fitted polynomials and the source elevation
C   Input:
C      TCLT     D      Time
C      IANT     I      antenna number as it selected
C   Control info from common:
C      ISTOK    I      Polarization to correct, 1=first, 2=second,
C                      0 = both
C   Output:
C      TEMOUT   C(*)   File of zenith delays for each selected antennas
C                      and times
C      IERR     I      Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IANT, IERR
      DOUBLE PRECISION TCLT
      CHARACTER TEMOUT(*)*80
C
      INTEGER   IFIT, TIME(3)
      REAL      PDLYAT, PDLY, DPDLAT, DPDLY, DPDATZ, HA, DELDT, COSZ,
     *   ELERAD, MAPF, SECNDS, CTIMR
      CHARACTER TSIGN*1
      DOUBLE PRECISION FREQS, COSLAT, SINLAT, SIDT, DISPAV
C                                       speed of light (meters/sec)
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'DELZN.INC'
      REAL      FITPAR(MAXFIT)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA SIDT /1.00273790935D0/
C-----------------------------------------------------------------------
      IERR = 0
C                                       time at the CL table line,
C                                       or even distributed time
C                                       in days
      CTIMR = TCLT + TIMCEN
C
      DO 10 IFIT = 1, NFITA
         FITPAR(IFIT) = SOL(IFIT + NFITA*(IANT-1))
 10      CONTINUE
C                                       estimate the atmzen delay, mm
      DISPAV = 0
      DO 20 IFIT = 1, NFITAT
         DISPAV = DISPAV + FITPAR(IFIT)*TCLT**(IFIT-1)
 20      CONTINUE
C                                       derivative of atmosphere
C                                       at zenith in sec/sec
      DPDATZ = 0
      DO 40 IFIT = 2, NFITAT
         DPDATZ = DPDATZ + (IFIT-1)*FITPAR(IFIT)*TCLT**(IFIT-2)
 40      CONTINUE
      DPDATZ = DPDATZ/24./60./60./1000./VELITE
C                                       record in the  temp FILE:
C                                       TELNO(IANT), CTIM
C                                       and DISPAV (in cm)
C                                       derivative of the zenith
C                                       atmosphere *E14
      IF (DOUTFI) THEN
         CALL TFDHMS (CTIMR, 1, TSIGN, TIME, SECNDS)
         WRITE (TEMOUT(FIXCNT), 1250) STNNAM(IANT)(1:3), TSIGN,
     *      TIME, SECNDS, DISPAV/10, DPDATZ*1.0E14
         END IF
      IF (.NOT.DOCL) GO TO 999
C
      IF (.NOT.DOCLTM) GO TO 999
C                                       antenna number at the CL table
C                                       line
      COSLAT = COS (STNLAT(IANT))
      SINLAT = SIN (STNLAT(IANT))
C                                       calculate elevation of the
C                                       antenna for the given source
      HA = TWOPI*SIDT*CTIMR + GSTIAT + STNLON(IANT) - RA
      COSZ = DCOS(STNLAT(IANT))*DCOS(DEC)*COS(HA) +
     *   DSIN(STNLAT(IANT))*DSIN(DEC)
C                                       source is under horizon
      IF (COSZ.LT.0.0) THEN
         IERR = 1
         GO TO 999
      ELSE
         ELERAD = PI / 2.0 - ACOS (COSZ)
         END IF
      FREQS = FREQ + FREQO(BIF)
      SINDEC = SIN (DEC)
      COSDEC = COS (DEC)

C                                       recalculate the  zenith delay
C                                       to the given elevation
C                                       using 1/SIN(EL) as a mapping
C                                       function and add the clock error
      IF (ELERAD.GT.0.01) THEN
         MAPF = 1 / SIN(ELERAD)
C                                       delays in second
         PDLYAT = DISPAV * MAPF/ 1000 / VELITE
C                                       Correct only atmosphere?
         PDLY = PDLYAT
      ELSE
         PDLY = 0
         END IF
C                                       DELDT is derivative of ELV
C                                       (by time) multiplied by
C                                       COS(ELV)
      DELDT = -7.29211E-5 * SIN (HA) * COSLAT * COSDEC
C
C                                       derivative of atmosphere
C                                       in sec/sec
      DPDLAT = -PDLYAT * DELDT
      IF (ELERAD.GT.0.01) THEN
         DPDLAT = (DPDLAT + DPDATZ) / SIN(ELERAD)
      ELSE
         DPDLAT = 0
         END IF
      DPDLY = DPDLAT
C                                       dispersion
      IF (CLRECR(DIS1CL).NE.FBLANK) CLRECR(DIS1CL) = CLRECR(DIS1CL)
     *      + PDLY
      IF (CLRECR(DDS1CL).NE.FBLANK) CLRECR(DDS1CL) = CLRECR(DDS1CL)
     *      + DPDLY
C                                       second Stokes
      IF ((ISTOK.NE.1) .OR. (NS.NE.1)) THEN
C                                       dispersion
         IF (CLRECR(DIS2CL).NE.FBLANK) CLRECR(DIS2CL) = CLRECR(DIS2CL)
     *      + PDLY
         IF (CLRECR(DDS2CL).NE.FBLANK) CLRECR(DDS2CL) = CLRECR(DDS2CL)
     *      + DPDLY
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1250 FORMAT (A3,A1,3I3,F5.1,F10.4,F12.4)
      END
      SUBROUTINE DELHIS
C-----------------------------------------------------------------------
C   DELHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12,  HILINE*72, LABEL*8
      INTEGER   LUN, IERR, TIM(3), DATE(3), I
      LOGICAL   T
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'DELZN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN, DISKIN, FCNO(NCFILE), BUFFER, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIM)
      CALL TIMDAT (TIM, DATE, CTIME(2)(1:8), CTIME)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history.
      WRITE (LABEL,1020) TSKNAM
      DO 50 I = 1,NUMHIS
         HILINE = LABEL // HISCRD(I)
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 200
 50      CONTINUE
C                                       Close HI file
 200  CALL HICLOS (LUN, T, BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DELHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6, 'RELEASE =''',A7,' ''  /********* Start ',
     *   A12, 2X, A8)
 1020 FORMAT (A6)
      END

