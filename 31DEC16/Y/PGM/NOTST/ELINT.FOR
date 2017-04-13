LOCAL INCLUDE 'MAXFIT.INC'
      INTEGER   MAXFIT
      PARAMETER (MAXFIT=200)
LOCAL END
LOCAL INCLUDE 'ELINT.INC'
C                                                          Include ELINT
C                                       Local include for ELINT
C                                       Needs parameter from PUVD.INC
C                                       Inputs and general info.
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'MAXFIT.INC'
      INTEGER   MXSOU, MXAIFS, MT,MXAN, MAIFSF, MXANSO, MAIFSS, MXIFSO,
     *   MOTT, MAXHIS
C                                       MOTT - number of lines in OT
C                                       table
      PARAMETER (MOTT = 100000)
      PARAMETER (MXSOU = 500)
      PARAMETER (MXAN = 50)
      PARAMETER (MXAIFS = MXAN*MAXIF*2)
      PARAMETER (MAIFSS = MXAIFS*MXSOU)
      PARAMETER (MXIFSO = MAXIF*MXSOU)
      PARAMETER (MAXHIS = 2000)
C                                       MSNTIM - # times in SN table
      INTEGER   MAIFST, MSNTIM
      LONGINT   OFFVAL, OFFEL
C     PARAMETER (MSNTIM = 10000)
C     PARAMETER (MAIFST = MSNTIM*2*MAXIF)
C                                       MXSASO - product # antennas
C                                       and sources
      PARAMETER (MXANSO = MXAN*MXSOU)
C                                       MT - # times of an antenna
C                                       in SN table
      PARAMETER (MT = 10000)
C                                       MAIFSF - # fit parameters
C                                       for all antennas, IFs, pol.
      PARAMETER (MAIFSF = MXAIFS*MAXFIT)
      INTEGER NIDC, IDC(MXSOU), NIDS, IDS(MXSOU), NNIF, NS, NPLOTS,
     *   NCOUNT, NNNIF, SID0, IND2, SEQIN, SUBA, DISKIN, CNOIN, SNVER,
     *   CLVER,  CLUSE, NANTSL, BIF, EIF, ISTOK,
     *   FREQID, XINC, CURIF, CURST,
     *   CURANT, ITIME(MXANSO), NTIME(MXANSO), INDTIM, CURIND, TVCHN,
     *   GRCHN, TVCORN(4), NPARM, NSS, NUMHIS, PRTLEV, NTOTT, PRTLE2,
     *   NTERM, IQUAL
      DOUBLE PRECISION RAC(MXSOU), DECC(MXSOU), RAS(MXSOU), DECS(MXSOU)
      LOGICAL   MULTI, DOTV, DOHIST, DOPLOT, OVETOP(MOTT), NOEXP
      REAL      XSIN, XDISIN, XSNVER, XGVER, XQUAL, XBAND, XFREQ, XFQID,
     *   XBIF, XEIF, XTIME(8), XANT(50), XSUBA, XNCOU, XXINC, XPRTL,
     *   XDOHI, XDOTV, XBAD(10), XGRCHN, SELBAN,
C    *   VAL(MAIFST), EL(MSNTIM),
     *   VAL(2), EL(2),
     *   VALMX(MXAIFS), VALMN(MXAIFS), ELEVMX, ELEVMN, XYSCL(2),
     *   XYOFF(2), GMXX, GMNX,
     *   CHOUT(4), ELE(MT), VALU(MT), FIT(MAIFSF), FITRMS(MXAIFS),
     *   TIMBEG, TIMEND,
     *   MULTS(MAIFSS), FLUXX(MXIFSO), TLEF(MOTT), TRIGH(MOTT)
      CHARACTER  HISCRD(MAXHIS)*64, NAMEIN*12, CLAIN*6, XSOUR(30)*16,
     *   XCALIB(30)*16, XSTOK*4, OPTYPE*4, OPCODE*4, TYPE*2,
     *   SOUS(MXSOU)*16, STOKK(2)*4, CALCO*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALB(4,30),
     *   XCALCO, XXSTOK, XOPTY, XOPCOD
      DOUBLE PRECISION FRQOFF(MAXIF), SELFRQ, FREQQ
C                                       cpecific CL data
      INTEGER   CLRECI(13+32*MAXIF), CLKOLS(MAXCLC), CLNUMV(MAXCLC),
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
     *   NUMANT, NUMPOL, NUMIF, ICODE, FIXCNT, IPCOD,
     *   TIMSN, INTSN, SOUSN, ANTSN, SUBSN, FRQSN, IFRSN, NODSN,
     *   RE1SN, IM1SN, DL1SN, RA1SN, WT1SN, RF1SN,
     *   RE2SN, IM2SN, DL2SN, RA2SN, WT2SN, RF2SN
      REAL      GMMOD, SNRECR(10+15*MAXIF), PARM(40), PANGLE(MAXANT)
      DOUBLE PRECISION COSDEC, SINDEC, SNRECD(10+15*MAXIF)
C                                       Inputs and general info
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSNVER,
     *   XGVER, XXSOUR, XXCALB, XQUAL, XCALCO, XXSTOK, XBAND,
     *   XFREQ, XFQID, XBIF,
     *   XEIF, XTIME, XANT, XSUBA, XNCOU, XXINC, XOPTY, XOPCOD,
     *   XPRTL, XDOHI, XDOTV, XGRCHN, XBAD
      COMMON /OTHPRM/ OFFVAL, OFFEL, SELBAN, SEQIN, DISKIN, CNOIN, SUBA,
     *   SNVER, MULTI, CLVER, CLUSE, TVCHN, NPARM, DOTV, NUMHIS,
     *   PRTLEV, DOHIST, DOPLOT, OVETOP, NOEXP, PRTLE2
      COMMON /EL/  RAC, RAS, DECC, DECS, IDC, NIDC, IDS, NIDS, NNIF,
     *   NS, VAL, EL, VALMX, VALMN, ELEVMX, ELEVMN, NPLOTS, XYSCL,
     *   XYOFF, NCOUNT, XINC, GMXX, GMNX,
     *   CHOUT, CURIF, CURST, CURANT, ITIME, NTIME, INDTIM, CURIND,
     *   GRCHN, TVCORN, ELE, VALU, FIT, FITRMS, NNNIF, ICODE, IPCOD,
     *   SID0,
     *   MULTS, IND2, NSS, FLUXX, TLEF, TRIGH, NTOTT, MAIFST, MSNTIM
      COMMON /CINFO/ FREQQ, FRQOFF, SELFRQ, NANTSL,  BIF, EIF, ISTOK,
     *   FREQID, TIMBEG, TIMEND, IQUAL
      COMMON /CHRCOM/ HISCRD, NAMEIN, CLAIN, XSOUR, XCALIB,
     *   XSTOK, OPTYPE, OPCODE, TYPE, SOUS, STOKK, CALCO
C                                       Buffers and file info
      COMMON /BUFRS/ BUFFER
C                                       Important constants
C                                       Internal storage
      COMMON /SNRECC/ COSDEC, SINDEC, SNRECD, GMMOD, PARM, PANGLE,
     *   FIXCNT, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF,
     *   TIMSN, INTSN, SOUSN, ANTSN, SUBSN, FRQSN, IFRSN, NODSN,
     *   RE1SN, IM1SN, DL1SN, RA1SN, WT1SN, RF1SN,
     *   RE2SN, IM2SN, DL2SN, RA2SN, WT2SN, RF2SN
      COMMON /CLRECC/  CLRECD, NTERM,
     *   CLKOLS, CLNUMV
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
      EQUIVALENCE (SNRECI, SNRECR, SNRECD)

C                                                          End ELINT
LOCAL END
      PROGRAM ELINT
C-----------------------------------------------------------------------
C! Determines applies calibration corrections to the SN table.
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000-2003, 2005-2007, 2010, 2011-2015
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
C   Task ELINT provides interpolation on elevation axis
C   Inputs:
C      INNAME.....UV file name (name).       Standard defaults.
C      INCLASS....UV file name (class).      Standard defaults.
C      INSEQ......UV file name (seq. #).     0 => highest.
C      INDISK.....Disk unit #.               0 => any.
C      TYPE.......Input table type. ' ' => SN
C      SOURCES....Source list.  ' ' = all; a "-" before a source
C                 name means all except ANY source named.
C      CALSOUR....Calibrator list.  ' ' = all; a "-" before a calibrator
C                 name means all except ANY calibrator named.
C      STOKES.....The desired Stokes type of the data:
C                 'R' = RCP, 'L' = LCP, 'other' = all available
C      SELBAN.....Bandwidth to select (kHz)
C      SELFREQ....Frequency to select (MHz)
C      FREQID.....Freq. ID to select, 0=>1
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
C      SNVER......The SN table version number which is to be updated.
C      NCOUNT.....Number of plots to plot per page 0=>5.
C      XINC.......Plot every XINC'th point
C      OPTYPE.....Data to be fitted: 'AMP '=  ampl.,
C                 'DELA' = delay, 'BP  ' = Bandpath
C      OPCODE.....Fit type: 'POLE', 'POLZ', 'PWR ', 'PWRN'
C      DOTV.......> 0 => TV, else plot file
C      BADDISK....A list of disks on which scratch files are not to
C                 be placed.
C
C programmer: Leonia Kogan, april 1996
C modified: Bryan Butler, may 1996
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
C      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'ELINT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'ELINT '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL ELCLIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C        IND2 = 1; Find solution for factors for different calibrators
C                  and coefficients of fitting function.
C        IND2 = 2; Apply the found factors to data and plot the data
C                  with fitting function.
C                                       read SN table and store
C                                       elevations and values
      DO 100 IND2 = 1,2
         IF (IRET.EQ.0) CALL ELCUV (IRET)
C                                       plot values versus elevation,
C                                       fit model and plot model
         IF (IRET.EQ.0) CALL PLTFIT (IRET)
         IRET = MAX (0, IRET)
  100    CONTINUE
C
      IF (IRET.EQ.0) CALL OUTCL (IRET)
C                                       Copy and update HI file.
      IF (IRET.EQ.0) CALL ELHIS
C                                       Close down files, etc.
  990 CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE ELCLIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   ELCLIN gets input parameters for ELINT.
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
      CHARACTER STAT*4, BLANK*4, UTYPE*2, CODE(5)*4, PCODE(4)*4, JCAL*4
      LOGICAL   T, MATCH, ALLBL
      INTEGER   IERR, I, IROUND, LUN, IIVER, NCODE, NPCODE, SNTOT,
     *   NUMCL, NIDCC, IS, IEND, LUN2, TABUFF(512), IREC
      INTEGER   ITIM, NUMREC, SID, MOT
      REAL      TIMEL, TIMER, BUFF1(2048)
C      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'ELINT.INC'
      INTEGER   DUMMY(MAXIF), OTTVER, JQUAL
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
      DATA NCODE, CODE /5, 'AMP ','DELA','BP  ','    ','    '/
C     PCODE =                   1      2      3      4
      DATA NPCODE, PCODE /4, 'POLE','POLZ','PWR ','PWRN'/
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
      NPARM = 334
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SUBA = XSUBA + 0.5
      IF (SUBA.LE.0) SUBA = 1
C      XPRTL = MAX (0.0, MIN (2.0, XPRTL))
      IF (XPRTL.LT.10.0) THEN
         PRTLEV = IROUND (XPRTL)
         PRTLE2 = 0
      ELSE
         PRTLEV = IROUND (XPRTL/10)
         PRTLE2 = XPRTL - PRTLEV * 10
         END IF
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
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      TYPE ='SN'
C                                       OPTYPE ?
      ICODE = 1
      DO 20 I = 1,NCODE
         IF ((OPTYPE(1:3).EQ.CODE(I)(1:3)) .AND. (OPTYPE.NE.BLANK))
     *      ICODE = I
 20      CONTINUE
C                                       correction of the oppacity is
C                                       provided if OPTYPE = AMP*
      NOEXP = OPTYPE(4:4).EQ.'L' .OR. OPTYPE(4:4).EQ.' '
      IF (ICODE.EQ.1) OPTYPE = 'AMP '
C                                       OPCODE ?
      IF (OPCODE.EQ.BLANK) THEN
         OPCODE = 'POLZ'
         IPCOD = 2
      ELSE
         IPCOD = 0
         DO 25 I = 1,NPCODE
            IF (OPCODE.EQ.PCODE(I)) IPCOD = I
 25         CONTINUE
         IF (IPCOD.EQ.0) THEN
            OPCODE = 'POLZ'
            IPCOD = 2
            END IF
         END IF
      CALL CHR2H (4, OPCODE, 1, XOPCOD)
      CALL CHR2H (4, OPTYPE, 1, XOPTY)
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
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1020) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
 40   CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'WRIT', BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 45
         WRITE (MSGTXT,1040) IERR
         GO TO 990
 45   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Check if multi source file
      CALL MULSDB (CATBLK, MULTI)
      IF (.NOT. MULTI) THEN
         JERR = 1
         WRITE (MSGTXT,1050)
         GO TO 990
         END IF
C                                       determine number of SN tables
      CALL FNDEXT ('SN', CATBLK, SNTOT)
C                                       input SN table
      SNVER = IROUND (XSNVER)
      IF (SNVER.EQ.0 .OR. SNVER.GT.SNTOT) SNVER = SNTOT
      XSNVER = SNVER
C                                       determine number of CL tables
      CALL FNDEXT ('CL', CATBLK, NUMCL)
      CLVER = IROUND (XGVER)
      IF (CLVER.LE.0 .OR. CLVER.GT.NUMCL) CLVER = NUMCL
      CLUSE = NUMCL + 1
      XGVER = CLVER
C                                       copy CLVER table to CLUSE table
      CALL TABCOP ('CL', CLVER, CLUSE, LUN, LUN2, DISKIN, DISKIN,
     *   CNOIN, CNOIN, CATBLK, BUFF1, TABUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 990
         END IF
C
      WRITE (MSGTXT,1200) CLUSE
      CALL MSGWRT (8)
C                                       frequency in GHz
      FREQQ = CATD(KDCRV + JLOCF) / 1.D9
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
      JQUAL = -1
      JCAL = ' '
      CALL SOUSEL (XSOUR, NSOUR, JQUAL, JCAL, DISKIN, CNOIN, CATBLK,
     *   BIF, NNIF, BUFFER, IDS, NIDS, RAS, DECS, SOUS, FLUXX, JERR)
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
C                                       is the list of CALSOR blank?
      ALLBL = .TRUE.
      DO 65 I = 1,NSOUR
         ALLBL = ALLBL .AND. (XCALIB(I).EQ.'                ')
   65    CONTINUE
      IF (ALLBL) THEN
         JERR = 1
         WRITE (MSGTXT,1055)
         GO TO 990
         END IF
C
      JQUAL = IROUND (XQUAL)
      CALL H2CHR (4, 1, XCALCO, JCAL)
      CALL SOUSEL (XCALIB, NSOUR, JQUAL, JCAL, DISKIN, CNOIN, CATBLK,
     *   BIF, NNIF, BUFFER, IDC, NIDC, RAC, DECC, SOUS, FLUXX, JERR)
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
C                                       fix ID of the best calibrator.
C                                       The best calibrator should be
C                                       the first one
      SID0 = 1
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
C------------------------------
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
C                                       Stokes' type.
      STOKK(1) = 'Rpol'
      STOKK(2) = 'Lpol'
      IF ((CATBLK(KINAX+JLOCS).EQ.1) .AND.
     *   (ICOR0.EQ.-2)) STOKK(1) = 'Lpol'
      NSS = MIN (2, CATBLK(KINAX+JLOCS))
      NS = 1
      ISTOK = 0
      IF (XSTOK.EQ.'R   ') ISTOK = 1
      IF (XSTOK.EQ.'L   ') ISTOK = NSS
C                                       If none selected take what you
C                                       have.
      IF (ISTOK.EQ.0) THEN
C                                       Both polarization are in the
C                                       data
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
      WRITE (HISCRD(NUMHIS),2300) SNVER
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2750) BIF, EIF, FREQID, SUBA
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2800) OPTYPE, OPCODE
      IF (NS.EQ.2) THEN
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2900)
      ELSE
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2950) STOKK(ISTOK)
         END IF
      CALL RFILL (8, 0.0, XTIME)
      XTIME(1) = TIMBEG
      XTIME(5) = TIMEND
C
C                                       Get IF information
      IIVER = 1
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, IIVER, CATBLK, LUN,
     *   NUMIF, FRQOFF, DUMMY, FINC, BNDCOD, FREQID, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       read OT table to store the times
C                                       when the source is observed by
C                                       elevation more than 90 degrees
C                                       (overtop)
C                                       Open OT table, Read for info
      OTTVER = 1
      NTOTT = 0
      MSGSUP = 32000
      CALL OTTINI ('READ', BUFFER, DISKIN, CNOIN, OTTVER, CATBLK, LUN,
     *   IERR)
      MSGSUP = 0
C                                       OT table does not exist
      IF (IERR.NE.0) THEN
         MSGTXT = 'No over-the-top (OT) file used'
         CALL MSGWRT (6)
         MOT = MOTT
         DO 110 ITIM = 1,MOT
            OVETOP(ITIM) = .FALSE.
 110        CONTINUE
         GO TO 999
         END IF
C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 230
      ITIM = 0
      DO 220 IREC = 1,NUMREC
C                                       read OT table
         IOTRNO = IREC
         CALL TABOTT ('READ', BUFFER, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'ERROR READING OVER-THE-TOP TABLE'
            CALL MSGWRT (8)
            GO TO 225
            END IF
C                                       timerange selection
         TIMEL = TIME8 - DT/2.
         TIMER = TIME8 + DT/2.
         IF ((TIMEL.GT.TIMEND) .OR. (TIMER.LT.TIMBEG)) GO TO 220
C                                       calibrators selection
         DO 190 SID = 1,NIDC
            IF (IDC(SID).EQ.JDSOUR) THEN
               IF (ITIM.GE.MOTT) THEN
                  MSGTXT = 'NUMBER OF LINES IN OT TABLE TOO BIG'
                  CALL MSGWRT (8)
                  GO TO 225
               ELSE
                  ITIM = ITIM + 1
                  TLEF(ITIM) = TIMEL
                  TRIGH(ITIM) = TIMER
                  OVETOP(ITIM) = OTT
                  GO TO 220
                  END IF
               END IF
 190        CONTINUE
 220     CONTINUE
 225  NTOTT = ITIM
C                                       Close table.
 230  CALL TABIO ('CLOS', 0, IOTRNO, BUFFER, BUFFER, I)
      IF (I.NE.0) THEN
         WRITE (MSGTXT,1900) I
         IF (IERR.LE.0) IERR = I
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ELCLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('Interpolation on elevation can be done only for ',
     *   'multiple source file')
 1055 FORMAT ('You have all blanks in CALSOUR. It is not a good idea')
 1060 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1100 FORMAT ('ERROR: COPYING INPUT CL TO OUTPUT:',I4)
 1200 FORMAT ('!!!   CL TABLE', I3, ' is UPDATED   !!!')
 1400 FORMAT ('ANTSEL: NOONE ANTENNA SELECTED')
 1900 FORMAT ('TABIO: ERROR = ', I3)
 2100 FORMAT ('TIMERANG = beginning to end')
 2200 FORMAT (F3.0,1X,F3.0,F3.0,F4.1,' - ',F3.0,1X,F3.0,F3.0,F4.1,
     *   '/ Time range')
 2300 FORMAT ('Input SN table =',I3, 5X, 'Output SN table =',I3)
 2350 FORMAT (5X,'Selected sources')
 2400 FORMAT (3A16)
 2550 FORMAT (5X,'Selected calibrators')
 2600 FORMAT (3A16)
 2650 FORMAT (5X,'Selected antennas')
 2700 FORMAT (10I4)
 2750 FORMAT ('BIF =', I3,5X, 'EIF =', I3,5X,'FREQID =',
     *   I3,5X,'SUBARRAY =', I3)
 2800 FORMAT ('OPTYPE = ',A4, 'OPCODE = ',A4)
 2900 FORMAT ('STOKES = Rpol and Lpol')
 2950 FORMAT ('STOKES = ', A4)
      END
      SUBROUTINE ELCUV (IERR)
C-----------------------------------------------------------------------
C   ELCUV is called from ELINT. ELCUV reads throught the SN table,
C   stores given values for the plotting routine PLTFIT and fitting
C   routine ELFIT.
C   Output: IERR  I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LUN, ISNRNO, NUMREC, NUMNOD, ISN, JIF, JS, SOUN,
     *   FREQN, MODENO, IANT, KS, KIF, MAX1, MAX2, LANT, SID,
     *   NTTOT, INDA, SOUANT, INDAS, LTIM
      LONGINT   IND
      LOGICAL   ISAPL
      REAL      RANOD(25), DECNOD(25), ELEV, COSZ, FAC
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'ELINT.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      REAL      REAL(2,MAXIF), IMAG(2,MAXIF), DELAY(2,MAXIF),
     *   RATE(2,MAXIF), WEIGHT(2,MAXIF), MBDELY(2), TIMINT, IFRM, HL
      INTEGER   REFA(2,MAXIF), NWORDS, DISP(2), DDISP(2)
      DOUBLE PRECISION CTIM, SIDT,  PI, TWOPI, RADDEG
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN /29/
      DATA SIDT, PI /1.00273790935D0, 3.14159265358979323846D0/
C-----------------------------------------------------------------------
      RADDEG = 180.0D0/PI
      TWOPI = 2.0D0*PI
      FIXCNT = 0
C   --------------------------------------------------------------
C   I  Read the SN table the first time to estimate number of    I
C   I  points for each selected antenna for selected calibrators I
C   --------------------------------------------------------------
      IF (IND2.NE.2) THEN
         DO 10 IANT = 1, NSTNS
            DO 5 SID = 1, NIDC
               SOUANT = SID + NIDC*(IANT-1)
               ITIME(SOUANT) = 0
 5             CONTINUE
 10         CONTINUE
C                                       Open SN table, Read for info
         CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, SNVER, CATBLK, LUN,
     *      ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *      GMMOD, RANOD, DECNOD, ISAPL, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Get number of records
         NUMREC = BUFFER(5)
         IF (NUMREC.LE.0) GO TO 999
         DO 200 ISNRNO = 1,NUMREC
C                                       read SN table
            ISN = ISNRNO
            CALL TABSN ('READ', BUFFER, ISN, SNKOLS, SNNUMV, NSS, CTIM,
     *         TIMINT, SOUN, LANT, SUBA, FREQN, IFRM, MODENO, MBDELY,
     *         DISP, DDISP, REAL, IMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C                                       timerange selection
            IF ((CTIM.GT.TIMEND) .OR. (CTIM.LT.TIMBEG)) GO TO 200
C                                       FREQID selection
            IF ((FREQN.NE.0) .AND. (FREQN.NE.FREQID)) GO TO 200
C                                       antennas selection
            DO 40 IANT = 1,NSTNS
               IF (TELNO(IANT).EQ.LANT) GO TO 60
 40            CONTINUE
            GO TO 200
C                                       calibrators selection
 60         DO 80 SID = 1, NIDC
               IF (IDC(SID).EQ.SOUN) GO TO 120
 80            CONTINUE
            GO TO 200
C
C                                       calculate elevation of
C                                       the selected antenna for
C                                       the selected source
 120        HL = TWOPI*SIDT*CTIM + GSTIAT + STNLON(IANT) - RAC(SID)
            COSZ = DCOS(STNLAT(IANT))*DCOS(DECC(SID))*COS(HL) +
     *         DSIN(STNLAT(IANT))*DSIN(DECC(SID))
            IF (COSZ.GE.0.0) THEN
               SOUANT = SID + NIDC*(IANT-1)
               ITIME(SOUANT) = ITIME(SOUANT) + 1
               END IF
 200        CONTINUE
C                                       Close table.
         CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Number of points for antennas
C                                       untill antenna IANT
         NTIME(1) = 0
         DO 210 IANT = 1, NSTNS
            DO 205 SID = 1, NIDC
               SOUANT = SID + NIDC*(IANT-1)
               IF (SOUANT.GT.1) NTIME(SOUANT) = NTIME(SOUANT-1) +
     *            ITIME(SOUANT-1)
 205           CONTINUE
 210        CONTINUE
         NTTOT = NTIME(SOUANT) + ITIME(SOUANT)
         MSNTIM = NTTOT+10
         MAIFST = MSNTIM * NS * NNIF
         NWORDS = (MSNTIM - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, EL, OFFEL, IERR)
         IF (IERR.NE.0) GO TO 999
         NWORDS = (MAIFST - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, VAL, OFFVAL, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C   -----------------------------------------------------------
C   I  Read the SN table the second time to store elevations  I
C   I    and amplitudes for each selected antenna and for     I
C   I             for selected calibrators                    I
C   -----------------------------------------------------------
      MAX1 = MAIFST
      MAX2 = MSNTIM
      DO 220 IANT = 1,NSTNS
         DO 215 SID = 1,NIDC
            SOUANT = SID + NIDC*(IANT-1)
            ITIME(SOUANT) = 0
 215        CONTINUE
 220     CONTINUE
      ELEVMX = 0.0
      ELEVMN = 200.0
C                                       check later about souant lower
      DO 235 IANT = 1,NSTNS
         DO 230 JIF = 1,NNIF
            DO 225 JS = 1,NS
               INDA = JS + (JIF-1)*NS + (IANT-1)*NS*NNIF
               VALMX(INDA) = -1.0E10
               VALMN(INDA) =  1.0E10
 225           CONTINUE
 230        CONTINUE
 235     CONTINUE
      CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, SNVER, CATBLK, LUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
      DO 500 ISNRNO = 1,NUMREC
C                                       read SN table
         ISN = ISNRNO
         CALL TABSN ('READ', BUFFER, ISN, SNKOLS, SNNUMV, NSS, CTIM,
     *      TIMINT, SOUN, LANT, SUBA, FREQN, IFRM, MODENO, MBDELY, DISP,
     *      DDISP, REAL, IMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C                                       timerange selection
         IF ((CTIM.GT.TIMEND) .OR. (CTIM.LT.TIMBEG)) GO TO 500
C                                       FREQID selection
         IF ((FREQN.NE.0) .AND. (FREQN.NE.FREQID)) GO TO 500
C                                       antennas selection
         DO 240 IANT = 1,NSTNS
            IF (TELNO(IANT).EQ.LANT) GO TO 260
 240        CONTINUE
         GO TO 500
C                                       calibrators selection
 260     DO 280 SID = 1, NIDC
            IF (IDC(SID).EQ.SOUN) GO TO 290
 280        CONTINUE
         GO TO 500
C                                       calculate elevation of
C                                       the selected antenna for
C                                       the selected source
 290     HL = TWOPI*SIDT*CTIM + GSTIAT + STNLON(IANT) - RAC(SID)
         COSZ = DCOS(STNLAT(IANT))*DCOS(DECC(SID))*COS(HL) +
     *      DSIN(STNLAT(IANT))*DSIN(DECC(SID))
C                                       source is under horizon
         IF (COSZ.LT.0.0) GO TO 500
         ELEV = PI/2.0 - ACOS(COSZ)
C                                       Is observation overtoped?
         DO 320 LTIM = 1,NTOTT
            IF ((CTIM.GE.TLEF(LTIM)) .AND. CTIM.LE.TRIGH(LTIM)) THEN
               IF (OVETOP(LTIM)) ELEV = PI - ELEV
               GO TO 340
               END IF
 320        CONTINUE
 340     SOUANT = SID + NIDC*(IANT-1)
         ITIME(SOUANT) = ITIME(SOUANT) + 1
         INDTIM = ITIME(SOUANT) + NTIME(SOUANT)
         IF (INDTIM.LE.MAX2) THEN
            EL(INDTIM+OFFEL) = ELEV * RADDEG
         ELSE
            WRITE (MSGTXT,1100) MAX2
            IERR = 1
            GO TO 990
            END IF
C                                       find MIN and MAX elevation
         ELEVMX = MAX(ELEVMX, EL(INDTIM+OFFEL))
         ELEVMN = MIN(ELEVMN, EL(INDTIM+OFFEL))
         DO 420 JIF = 1,NNIF
            KIF = BIF + JIF -1
            DO 400 JS = 1,NS
               KS = ISTOK + JS -1
               IND = JS + (JIF-1)*NS + (INDTIM-1)*NS*NNIF + OFFVAL
               IF (IND.LE.MAX1+OFFVAL) THEN
C                                       chose a type of variable
                  IF (ICODE.EQ.1) THEN
C                                       amplitude
                     IF (IND2.EQ.1) THEN
C                                       mark blank points
                        IF ((REAL(KS,KIF).EQ.FBLANK) .OR.
     *                     (IMAG(KS,KIF).EQ.FBLANK) .OR.
     *                     (WEIGHT(KS,KIF).LE.0.0)) THEN
                           VAL(IND) = FBLANK
                        ELSE
                           VAL(IND) = SQRT(REAL(KS,KIF)*REAL(KS,KIF) +
     *                        IMAG(KS,KIF)*IMAG(KS,KIF))
C                                       replace voltage to power for
C                                       OPCODE = 'PWR ' or 'PWRN'
                           IF ((IPCOD.EQ.3) .OR. (IPCOD.EQ.4))
     *                        VAL(IND) = 1 / VAL(IND) ** 2
                           END IF
                     ELSE IF (IND2.EQ.2) THEN
                        INDAS = JS + (JIF-1)*NS + (SID-1)*NS*NNIF +
     *                     (IANT-1)*NS*NNIF*NIDC
                        FAC = MULTS(INDAS)
                        IF (FAC.EQ.0) FAC = 1
                        IF (VAL(IND).NE.FBLANK) VAL(IND) = VAL(IND) /
     *                     FAC
                        END IF
C                                       delay
                  ELSE IF (ICODE.EQ.2) THEN
                     VAL(IND) = DELAY(KS, KIF)
                     END IF
C
               ELSE
                  WRITE (MSGTXT,1200) MAX1
                  IERR = 1
                  GO TO 990
                  END IF
C                                       find MAX&MIN of the value
C                                       for each ANT, JIF, JS
               INDA = JS + (JIF-1)*NS + (IANT-1)*NS*NNIF
C                                       exclude blanked points
               IF ((VAL(IND).NE.FBLANK)  .AND. (IND2.EQ.2)) THEN
                  VALMX(INDA) = MAX(VALMX(INDA), VAL(IND))
                  VALMN(INDA) = MIN(VALMN(INDA), VAL(IND))
                  END IF


 400           CONTINUE
 420        CONTINUE
 500     CONTINUE
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
 1100 FORMAT ('Product NANT*NTIME exceeds MAX = ', I6)
 1200 FORMAT ('Product NANT*NTIME*NNIF*NS exceeds MAX = ',I6)
 1900 FORMAT ('TABIO: ERROR = ', I3)
      END
      SUBROUTINE SOUSEL (SOURCE, NSOUR, IQU, CALC, DISK, CNO, CAT, BIF,
     *   NNIF, BUFFER, ID, NID, RA, DEC, SOUS, FLUXS, IRET)
C-----------------------------------------------------------------------
C   Load all source identifiers in the SU table.
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
C      IQU       I       qualifier restriction
C      CALC      C*4     Calcode restriction
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
      CHARACTER SOURCE(*)*16, CALC*4, SOUS(*)*16, SOUST(50)*16
      INTEGER   DISK, CNO, iqu, NID, CAT(256), BUFFER(*), ID(*), KID,
     *   IID, IDT(50), BIF, NNIF, IRET
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
      INCLUDE 'INCS:DFIL.INC'
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
               IF ((IQU.GE.0) .AND. (SQUAL.NE.IQU)) EQUAL = .FALSE.
               IF (CALC.NE.' ') THEN
                  IF (CALC.EQ.'*') THEN
                     IF (CALCOD.EQ.' ') EQUAL = .FALSE.
                  ELSE IF (CALC.EQ.'-CAL') THEN
                     IF (CALCOD.NE.' ') EQUAL = .FALSE.
                  ELSE
                     IF (CALCOD.NE.CALC) EQUAL = .FALSE.
                     END IF
                  END IF
               IF (EQUAL) THEN
                  IF ((NID+1).LE.MAXID) THEN
                     NID = NID + 1
C                                       take IFLUX
                     DO 20 I = 1, NNIF
                        SIF = I + (NID-1)*NNIF
                        FLUXS(SIF) = FLUX(1,I +BIF -1)
                        IF (FLUXS(SIF).EQ.0.0) FLUXS(SIF) = 1.0
   20                   CONTINUE
                     ID(NID) = IDSOU
                     RA(NID) = RAAPP*DEGRAD
                     DEC(NID) = DECAPP*DEGRAD
                     SOUS(NID) = SOUNAM
                     GO TO 500
C                                       Too many sources selected
                  ELSE
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
               IF ((IQU.GE.0) .AND. (SQUAL.NE.IQU)) EQUAL = .FALSE.
               IF (CALC.NE.' ') THEN
                  IF (CALC.EQ.'*') THEN
                     IF (CALCOD.EQ.' ') EQUAL = .FALSE.
                  ELSE IF (CALC.EQ.'-CAL') THEN
                     IF (CALCOD.NE.' ') EQUAL = .FALSE.
                  ELSE
                     IF (CALCOD.NE.CALC) EQUAL = .FALSE.
                     END IF
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
C   PLTFIT plots and fits the data through calls to PLTEL and ELFIT
C   Output:
C      IRET     I  Return code, 0=OK else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IPLOT, IPLT, IANT, JIF, JS, ITIM, IT, IFIT,
     *   MXFIT, INDAF, INDSF, ITS, SOUANT, SID, ITIMS(100),
     *   NTIMS(100), IOFFS,
     *   INDAS, NFIT0, INDIFS, COUNT(1000), ICOUNT, SIF, INDA, NSTN1
      LONGINT   IND
      LOGICAL GOOD, ANBAD
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'ELINT.INC'
      REAL      FITPAR(MAXFIT), VX(MAXFIT), MULT(MXSOU), TOLER(5),
     *   VARRES, MEAN(1000), MFACT, SIGMA, FLUXOL, FLUXNE, FLUXER,
     *   TMAX, TMIN, TDIF, SIZEX, SIZEY, GM, G2, ZM, RMSN, OFFSET
C      CHARACTER PLUS*3, MINUS*3, C1*3, C2*3, C3*3
      CHARACTER PLUS*1, MINUS*1, C1*1, C2*1, C3*1
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA TOLER /0.01, 1.0E-10, 3*0.0/
      DATA PLUS, MINUS /'+', '-'/
C-----------------------------------------------------------------------
      IRET = 0
      MXFIT = MAXFIT
C                                       initialize fit parameters
      DO 10 IFIT = 1,MXFIT
         FITPAR(IFIT) = 0.0
 10      CONTINUE
C                                       zero mean of factors through all
C                                       antennas and stokes
      DO 16 JIF = 1, NNIF
         DO 14 SID = 1, NIDC
            INDIFS = SID + NIDC*(JIF-1)
            MEAN(INDIFS) = 0
            COUNT(INDIFS) = 0
 14         CONTINUE
 16      CONTINUE
C                                       Find last plot
      NPLOTS = 0
      DO 50 IANT = 1, NSTNS
         ITIM = 0
         DO 20 SID = 1, NIDC
            SOUANT = SID + NIDC*(IANT-1)
            ITIM = ITIM + ITIME(SOUANT)
 20         CONTINUE
         IF (ITIM.EQ.0) GO TO 50
         DO 40 JIF = 1, NNIF
            DO 30 JS = 1, NS
               NPLOTS = NPLOTS + 1
 30            CONTINUE
 40         CONTINUE
 50      CONTINUE
      IF (NPLOTS.LE.0) GO TO 980
C                                       Prepare parameters for PLTFIT
C                                       thru PLTEL
      SIZEX = 1000.0
      SIZEY = 1000.0 / NCOUNT
      GMXX = -1.0E20
      GMNX = 1.0E20
      IPLT = 0
      NSTN1 = NSTNS
C                                       Loop thru plots
C
      DO 200 IANT = 1,NSTNS
         CURANT = IANT
         ITIM = 0
         ANBAD = .FALSE.
         DO 60 SID = 1, NIDC
            SOUANT = SID + NIDC*(IANT-1)
            ITIM = ITIM + ITIME(SOUANT)
 60         CONTINUE
         IF (ITIM.EQ.0) GO TO 200
C
         DO 160 JIF = 1,NNIF
            CURIF = BIF + JIF - 1
            DO 130 JS = 1,NS
               CURST = ISTOK + JS - 1
               ITS = 0
               NTIMS(1) = 0
               DO 80 SID = 1,NIDC
                  SOUANT = SID + NIDC*(IANT-1)
                  ITIM = ITIME(SOUANT)
                  ITIMS(SID) = ITIM
                  IF (SID.GT.1) NTIMS(SID) = NTIMS(SID-1) + ITIMS(SID-1)
                  DO 70 IT = 1,ITIM
                     INDTIM = IT + NTIME(SOUANT)
                     IND = JS + (JIF-1)*NS + (INDTIM-1)*NS*NNIF + OFFVAL
C                                       exclude blanked points
C                     IF (VAL(IND).EQ.FBLANK) GO TO 70
                     ITS = ITS + 1
                     ELE(ITS) = EL(INDTIM+OFFEL)
                     VALU(ITS) = VAL(IND)
 70                  CONTINUE
 80               CONTINUE
C                                       ITS - total number of points
C                                       for the given antenna
C                                       ITIMS(SID) - number of points
C                                       for the source SID
C                                       NTIMS(SID) - number of points
C                                       for the sources preceded SID
C
C                                       fit a function determined by
C                                       OPCODE into VALU(ELE)
               IF (IND2.NE.2) THEN
                  CALL ELFIT (ELE, VALU, NIDC, ITS, ITIMS, NTIMS, IPCOD,
     *               SID0, NOEXP, NFIT0, FITPAR, VX, MULT, VARRES, IRET)
                  IF (IPCOD.EQ.4) NFIT0 = NFIT0 + 1
C                                       zero fit parameters if no fit
                  IF (IRET.NE.0) THEN
                     DO 85 IFIT = 1, NFIT0
                        INDAF = JS + (JIF-1)*NS + (IANT-1)*NS*NNIF +
     *                     (IFIT-1)*NS*NNIF*NSTNS
                        FIT(INDAF) = 0.0
   85                   CONTINUE
                     IRET = 0
                     GO TO 130
                     END IF
                  VARRES = SQRT(VARRES)
C                                       normalize RMS by the first
C                                       coefficient at the fit
C                                       polinomial. Rick desire.
C                                       Feb 12, 2012
                  IF (IPCOD .EQ. 2) THEN
C                                       POLZ
                     RMSN = VARRES / FITPAR(1)
                  ELSE
                     IF (IPCOD .EQ. 1) THEN
C                                       POLE
                        RMSN = VARRES /
     *                     (FITPAR(1) + FITPAR(2)*90 + FITPAR(3)*8100)
                     ELSE
                        RMSN = 1
                        END IF
                     END IF
C                                       Ricks equation
                  OFFSET = 1440*SQRT(LOG(1 + RMSN)) / FREQQ
                  IOFFS = INT(OFFSET+0.5)
C                                       normalization if 'PWRN'
C                                       and if .NOT. 'AMPT'
                  IF (IPCOD.EQ.4) THEN
                     IF (NOEXP) THEN
                        CALL FNORM (FITPAR, FITPAR(NFIT0))
                     ELSE
                        FITPAR(NFIT0) = 1
                        END IF
                     VARRES = VARRES / FITPAR(NFIT0)
                     END IF
                  IF (PRTLEV.GT.0) THEN
                     C1 = PLUS
                     IF (FITPAR(1).LT.0.0) C1 = MINUS
                     C2 = PLUS
                     IF (FITPAR(2).LT.0.0) C2 = MINUS
                     C3 = PLUS
                     IF (FITPAR(3).LT.0.0) C3 = MINUS
                     IF (IPCOD.EQ.1) THEN
                        IF (NOEXP) THEN
                           IF (PRTLE2.EQ.0) THEN
                              WRITE (MSGTXT,1150) TELNO(IANT),
     *                           STOKK(CURST), CURIF, FITPAR(1), C2,
     *                           ABS(FITPAR(2)), C3, ABS(FITPAR(3))
C                                       recalculate polinomial
C                                       coefficients to parameters of
C                                       maximum
                           ELSE
                              GM = FITPAR(1) - (FITPAR(2) * FITPAR(2)) /
     *                           (4 * FITPAR(3))
                              G2 = FITPAR(3)
                              ZM = -FITPAR(2) / (2 * FITPAR(3))
                              WRITE (MSGTXT,1155) TELNO(IANT),
     *                           STOKK(CURST), CURIF, GM, C3, ABS(G2),
     *                           ZM
                              END IF
                           CALL MSGWRT (3)
                           IF ((DOHIST) .AND. (NUMHIS.LT.MAXHIS)) THEN
                              NUMHIS = NUMHIS + 1
                              HISCRD(NUMHIS) = MSGTXT
                              END IF
                        ELSE
                           WRITE (MSGTXT,1160) TELNO(IANT),
     *                        STOKK(CURST), CURIF, FITPAR(1), C2,
     *                        ABS(FITPAR(2)), C3, ABS(FITPAR(3))
                           CALL MSGWRT (3)
                           IF ((DOHIST) .AND. (NUMHIS.LT.MAXHIS)) THEN
                              NUMHIS = NUMHIS + 1
                              HISCRD(NUMHIS) = MSGTXT
                              END IF
                           WRITE (MSGTXT,1165) FITPAR(4)
                           CALL MSGWRT (3)
                           IF ((DOHIST) .AND. (NUMHIS.LT.MAXHIS)) THEN
                              NUMHIS = NUMHIS + 1
                              HISCRD(NUMHIS) = MSGTXT
                              END IF
                           END IF
                     ELSE
                        IF (NOEXP) THEN
                           IF (PRTLE2.EQ.0) THEN
                              WRITE (MSGTXT,1170) TELNO(IANT),
     *                           STOKK(CURST), CURIF, FITPAR(1), C2,
     *                           ABS(FITPAR(2)), C3, ABS(FITPAR(3))
C                                       recalculate polinomial
C                                       coefficients to parameters of
C                                       maximum
                           ELSE
                              GM = FITPAR(1) - (FITPAR(2) * FITPAR(2)) /
     *                           (4 * FITPAR(3))
                              G2 = FITPAR(3)
                              ZM = -FITPAR(2) / (2 * FITPAR(3))
                              WRITE (MSGTXT,1175) TELNO(IANT),
     *                           STOKK(CURST), CURIF, GM, C3, ABS(G2),
     *                           ZM
                              END IF
                           CALL MSGWRT (3)
                           IF ((DOHIST) .AND. (NUMHIS.LT.MAXHIS)) THEN
                              NUMHIS = NUMHIS + 1
                              HISCRD(NUMHIS) = MSGTXT
                              END IF
                        ELSE
                           WRITE (MSGTXT,1180) TELNO(IANT),
     *                        STOKK(CURST), CURIF, FITPAR(1), C2,
     *                        ABS(FITPAR(2)), ABS(FITPAR(3))
                           CALL MSGWRT (3)
                           IF ((DOHIST) .AND. (NUMHIS.LT.MAXHIS)) THEN
                              NUMHIS = NUMHIS + 1
                              HISCRD(NUMHIS) = MSGTXT
                              END IF
                           WRITE (MSGTXT,1185) FITPAR(4)
                           CALL MSGWRT (3)
                           IF ((DOHIST) .AND. (NUMHIS.LT.MAXHIS)) THEN
                              NUMHIS = NUMHIS + 1
                              HISCRD(NUMHIS) = MSGTXT
                              END IF
                           END IF
                        END IF
                     END IF
                  IF (PRTLEV.GT.1) THEN
C                                       errors of the found coefficients
                     WRITE (MSGTXT,1181) VX(1), VX(2), VX(3)
                     CALL MSGWRT (3)
C                                       SQRT (variance of residuals)
                     WRITE (MSGTXT, 1182) RMSN, IOFFS
                     CALL MSGWRT (3)
                     END IF
C                                       store found fit parameters
                  DO 90 IFIT = 1,NFIT0
                     INDAF = JS + (JIF-1)*NS + (IANT-1)*NS*NNIF +
     *                  (IFIT-1)*NS*NNIF*NSTNS
                     FIT(INDAF) = FITPAR(IFIT)
 90                  CONTINUE
C                                       Feb 12, 2012
C                                       store found RMSNs
                  INDSF = JS + (JIF-1)*NS + (IANT-1)*NS*NNIF
                  FITRMS(INDSF) = RMSN
C                                       store found factors for each
C                                       source
                  DO 100 SID = 1,NIDC
                     SOUANT = SID + NIDC*(IANT-1)
                     INDAS = JS + (JIF-1)*NS + (SID-1)*NS*NNIF +
     *                  (IANT-1)*NS*NNIF*NIDC
C                                       average found factors through
C                                       all antennas and stokes
                     INDIFS = SID + NIDC*(JIF-1)
                     MEAN(INDIFS) = MEAN(INDIFS) + MULT(SID)
                     COUNT(INDIFS) = COUNT(INDIFS) + 1
                     MULTS(INDAS) = MULT(SID)
 100                 CONTINUE
                     INDAF = JS + (JIF-1)*NS + (IANT-1)*NS*
     *                  NNIF + (IFIT-1)*NS*NNIF*NSTNS
                     FITPAR(IFIT) = FIT(INDAF)
C                                       second pass
               ELSE
                  GOOD = .FALSE.
C                                       restore RMS
C                                       Feb 12, 2012
                  INDSF = JS + (JIF-1)*NS + (IANT-1)*NS*NNIF
                  RMSN = FITRMS(INDSF)
C                                       restore fit paarameters
                  DO 115 IFIT = 1,NFIT0
                     INDAF = JS + (JIF-1)*NS + (IANT-1)*NS*
     *                  NNIF + (IFIT-1)*NS*NNIF*NSTNS
                     FITPAR(IFIT) = FIT(INDAF)
                     IF (FITPAR(IFIT).NE.0) GOOD = .TRUE.
 115                 CONTINUE
C                                       The antenna is bad if solution
C                                       is not found at least at one IF,
C                                       polarization
                  ANBAD = ANBAD .AND. (.NOT.GOOD)
                  IF (.NOT.GOOD) THEN
                     NPLOTS = NPLOTS - 1
C                                       the last plot: do not skip pltel
                     IF (NPLOTS.EQ.IPLT) THEN
                        WRITE (MSGTXT, 2000) TELNO(IANT), JIF, JS
                        CALL MSGWRT (3)
                        GO TO 999
                     ELSE
                        GO TO 130
                        END IF
                     END IF
                  IPLT = IPLT + 1
                  IPLOT = IPLT - 1
                  IPLOT = MOD (IPLOT, NCOUNT) + 1
                  IF (IPLT.EQ.NPLOTS) IPLOT = -IPLOT
                  INDA = JS + (JIF-1)*NS + (IANT-1)*NS*NNIF
                  IF (IPCOD.EQ.4) THEN
                     VALMX(INDA) = VALMX(INDA) / FITPAR(NFIT0)
                     VALMN(INDA) = VALMN(INDA) / FITPAR(NFIT0)
                     END IF
C                                       TMAX, TMIN new MAX&MIN of Y
                  TMAX = VALMX(INDA) + 0.1*(VALMX(INDA)-VALMN(INDA))
                  TMIN = VALMN(INDA) - 0.1*(VALMX(INDA)-VALMN(INDA))
                  IF (ABS (TMAX-TMIN).LT.TOLER(ICODE)) THEN
                     TMAX = TMAX + TOLER(ICODE)
                     TMIN = TMIN - TOLER(ICODE)
                     END IF
C                                       GMXX, GMNX MAX&MIN of Y for all
C                                       plots
                  GMXX = MAX (GMXX, TMAX)
                  GMNX = MIN (GMNX, TMIN)
                  TDIF = TMAX - TMIN
                  IF (ABS (TDIF).LE.1.0E-25) TDIF = 1.0E-25
                  XYOFF(2) = TMIN
                  XYSCL(2) = SIZEY / TDIF
C                                       now about X-axis
                  TMAX = (ELEVMX + 0.1 * (ELEVMX - ELEVMN))
                  TMIN = (ELEVMN - 0.1 * (ELEVMX - ELEVMN))
                  TDIF = TMAX - TMIN
                  IF (ABS (TDIF).LE.0.01) TDIF = 0.01
                  XYOFF(1) = TMIN
                  XYSCL(1) = SIZEX / TDIF
C                                       plot VALU/ELE and model
                  IF (DOPLOT) THEN
                     CALL PLTEL (IPLOT, ITS, FITPAR, RMSN, IRET)
                     IF (IRET.NE.0) DOPLOT = .FALSE.
                     END IF
                  END IF
 130           CONTINUE
 160        CONTINUE
C                                       count the number of good
C                                       antennas
         IF (ANBAD) NSTN1 = NSTN1 - 1
 200     CONTINUE
C                                       type average factors
      IF (IND2.EQ.1) THEN
         DO 240 JIF = 1,NNIF
            CURIF = BIF + JIF -1
            WRITE (MSGTXT,1250) CURIF
            CALL MSGWRT (3)
            WRITE (MSGTXT,1260)
            CALL MSGWRT (3)
            DO 220 SID = 1,NIDC
               INDIFS = SID + NIDC*(JIF-1)
C                                       average factor for the given
C                                       source and IF
               IF (COUNT(INDIFS).NE.0)
     *            MFACT = MEAN(INDIFS) / COUNT(INDIFS)
               SIGMA = 0.0
               ICOUNT = 0
               DO 215 IANT = 1, NSTNS
                  SOUANT = SID + NIDC*(IANT-1)
                  IF (ITIME(SOUANT).NE.0) THEN
                     DO 210 JS = 1, NS
                        INDAS = JS + (JIF-1)*NS + (SID-1)*NS*NNIF +
     *                     (IANT-1)*NS*NNIF*NIDC
                        SIGMA = SIGMA + (MULTS(INDAS)-MFACT)**2
                        ICOUNT = ICOUNT + 1
 210                    CONTINUE
                     END IF
 215              CONTINUE
               IF (ICOUNT.NE.0) THEN
                  SIGMA = SQRT (SIGMA / ICOUNT / NSTN1)
                  SIF = JIF + (SID-1)*NNIF
                  FLUXOL = FLUXX(SIF)
                  IF (IPCOD.EQ.1 .OR. IPCOD.EQ.2) THEN
                     IF (MFACT .NE. 0) THEN
                        FLUXNE = FLUXOL /(MFACT*MFACT)
                        FLUXER = 2 * FLUXNE * (SIGMA / MFACT)
                        END IF
                  ELSE
                     FLUXNE = FLUXOL * MFACT
                     IF (MFACT .NE. 0) THEN
                        FLUXER = FLUXNE * (SIGMA / MFACT)
                        END IF
                     END IF
                  WRITE (MSGTXT,1300) SOUS(SID), MFACT, SIGMA, FLUXOL,
     *               FLUXNE, FLUXER
                  CALL MSGWRT (3)
                  END IF
 220           CONTINUE
 240        CONTINUE
         END IF
      GO TO 999
C                                       No baselines
 980  IRET = 8
      WRITE (MSGTXT,1200)
      CALL MSGWRT (2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1150 FORMAT ('ANT=',I2,' STOK=',A1,' IF=',I2,':',E10.4,1X,A1,1X,
     *   E9.3,'*EL',1X,A1,1X, E9.3,'*EL**2')
 1155 FORMAT ('ANT=',I2,' STOK=',A1,' IF=',I2,':',F7.3,A,E9.3,
     *   '* (EL - ',F6.2,')^2' )
 1160 FORMAT ('ANT=',I2,' STOK=',A1,' IF=',I2,':','(',E9.3,A,E9.3,
     *   '*EL',A,E9.3,'*EL**2 )')
 1165 FORMAT (20X, '*EXP(-',E9.3,'/SIN(EL))' )
 1170 FORMAT ('ANT=',I2,' STOK=',A1,' IF=',I2,':',E10.4,1X,A1,1X,
     *   E9.3,'*ZA',1X,A1,1X, E9.3,'*ZA**2')
 1175 FORMAT ('ANT=',I2,' STOK=',A1,' IF=',I2,':',F7.3,A,E9.3,
     *   '* (ZA - ', F6.2,')^2' )
 1180 FORMAT ('ANT=',I2,' STOK=',A1,' IF=',I2,':','(',E9.3,A,E9.3,
     *   '*ZA',A,E9.3,'*ZA**2 )')
 1181 FORMAT ('errors', 14X, E9.3, 3X, E9.3, 6X, E9.3)
 1182 FORMAT (2X,'RMSN = ', F8.5, ' rms of poin. offset =',
     *   I3,' arcsec')
 1185 FORMAT (20X, '*EXP(-',E9.3,'/COS(ZA))' )
 1200 FORMAT ('PLTFIT: No data for sources selected')
 1250 FORMAT ('         IF= ', I2)
 1260 FORMAT ('CALIBRATOR',6X, 'MEANFACTOR', 4X,
     *   'FLUXOLD',8X, 'FLUXNEW')
 1300 FORMAT (1X,A8, 3X,F7.4,'+-',F7.4, 2X, F7.3, 3X,F7.3,'+-',
     *    F6.3)
 2000 FORMAT (' Data for last plot: IANT=', I2,
     *   ' IF=',I2, ' JS=',I2, ' are bad!')
      END
      SUBROUTINE FNORM (FITPAR, NORMV)
C-----------------------------------------------------------------------
C   FNORM normalizes the FITPAR parameters
C   Input:
C      FITPAR  R(*) Fit parameters
C   Output:
C      FITPAR  R(*) Normalized fit parameters
C      NORMV   R    Normalization factor
C-----------------------------------------------------------------------
      REAL FITPAR(*), NORMV, G0, G1, G2, VAL0, VAL90, ZMAX
      G0 = FITPAR(1)
      G1 = FITPAR(2)
      G2 = FITPAR(3)
      VAL0 = G0
      VAL90 = G0 + G1 * 90 + G2 * 8100.0
      IF (G1.LT.0.0 .AND. G2.LT.0.0) THEN
         NORMV = VAL0
      ELSE IF (G1.GT.0.0 .AND. G2.GT.0.0) THEN
         NORMV = VAL90
      ELSE IF (G1.LT.0.0 .AND. G2.GT.0.0) THEN
C degenerate, since the curve is minimum instead of maximum in the
C middle, so, the maximum must be at one of the endpoints...
         NORMV = MAX (VAL0, VAL90)
      ELSE
         ZMAX = - G1 / (2 * G2)
         NORMV = G0 + G1 * ZMAX + G2 * ZMAX * ZMAX
         END IF
      FITPAR(1) = FITPAR(1) / NORMV
      FITPAR(2) = FITPAR(2) / NORMV
      FITPAR(3) = FITPAR(3) / NORMV
      RETURN
      END
      SUBROUTINE PLTEL (IPLOT, ITIM, FITPAR, RMSN, IRET)
C-----------------------------------------------------------------------
C   PLTEL actually plots data and model.
C   Input:
C      IPLOT   I    Plot number on current page. If neg. then this is
C                   last plot.
C      ITIM    I     Number of points at arrays ELE and VALU
C      FITPAR  R(*) Array of parameters of fitting function
C      RMSN    R    RMS of polinomial fitting, devided by FITPAR(1)
C   Inputs from Common:
C      ELE     R(*)  Array of data arguments
C      VALU    R(*)  Array of data function
C      OPCODE  C(4) Determines a fitting function
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
      INTEGER   IPLOT, IRET, NPMOD, IOFFS
C
      CHARACTER TEXT*132, PFILE*48, ATIME*8, ADATE*12, AUNITS(28)*8,
     *   CHTYPE(28)*16, CHTMP*18, STKTMP*4, XUNITS*20
      CHARACTER C1*1, C2*2, C3*1, PLUS*1, MINUS*1
C      DATA PLUS, MINUS /'+', '-'/
      INTEGER   ELBUFF(256), VER, IERR, ITYPE, IPSIZE, LUNPL, LTYPE,
     *   FINDPL, DEPTH(5), INCHAR, INP, IT(3), ID(3), IAXLAB, IAPLOT,
     *   I, NGOOD, NNOFIT, LABEL, ITT, IFIRST, ITIM
      REAL      BLC(2), TRC(2), XYRATO, DX, DY, TR, VALUE, TI,
     *   XY(2), XTRC(2), XBLC(2), TLC(2), PLTINC, YYOFF(2), SIZE,
     *   XMULT(2), XVARIB, YPT, DBY, FITPAR(*), PREV, ELDEG, ZADEG,
     *   EXPON, SC, GM, G2, ZM, RMSN, OFFSET
      LOGICAL   T, F, GOOD, CATUP, CURENT, PREVOS
      SAVE LABEL, LTYPE, ELBUFF
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'ELINT.INC'
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
      DATA AUNITS /'Gain','Degrees','Seconds','Hz','Kelvin',' ','Hz',
     *             'SNR', 'Seconds', 'Kelvin', 'Seconds', 'Seconds',
     *             'Seconds','Degrees','       ',
     *             13*' '/
      DATA CHTYPE / 'Gain amp', 'Gain amp', 'Power gain',
     *              'Power gain', 24*' '/
      DATA XUNITS / 'Elevation (degrees)'/
      DATA PLUS, MINUS /'+', '-'/
C-----------------------------------------------------------------------
C                                       Ricks equation
      OFFSET = 1440*SQRT(LOG(1 + RMSN)) / FREQQ
      IOFFS = INT(OFFSET + 0.5)
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
      TLC(1) = XBLC(1)
      TLC(2) = XTRC(2)
C                                       Offsets for current plot.
      YYOFF(1) = XBLC(1)
      YYOFF(2) = XBLC(2)
C                                       fool with location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
C                                       LABTYP(LOCNUM)=0 for xaxis=elev
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      TR = 1.2 * (GMXX - GMNX)
      TI = TR
      CALL METSCA (TR, CPREF(2,LOCNUM), GOOD)
      XMULT(2) = TR / TI
      CPREF(1,LOCNUM) = ' '
      XMULT(1) = 1.0
      DO 5 I = 1,2
         SIZE = 1000.0
         IF (I.EQ.2) SIZE = PLTINC
         TR = SIZE / XYSCL(I)
         RPLOC(I,LOCNUM) = XBLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I) * XMULT(I)
         AXINC(I,LOCNUM) = TR * XMULT(I) / (XTRC(I) - XBLC(I))
 5      CONTINUE
      CTYP(1,LOCNUM) = XUNITS
      CTYP(2,LOCNUM) = AUNITS(ICODE)
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
         ITYPE = 41
C
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
C                                       stokes
      STKTMP = 'Rpol'
      IF (CURST.EQ.2) STKTMP = 'Lpol'
C                                       Top labels: type & name
      IF (ABS(IPLOT).EQ.1) THEN
         DX = 0.0
         DY = 1.833
C                                       The second line of the header
         CALL GPOS (BLC(1), TRC(2), ELBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         TEXT = CHTYPE(IPCOD)
         TEXT(17:) = ' vs elevation for '
C                                       File name
         CALL H2CHR (18, KHIMNO, CATH(KHIMN), CHTMP)
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTMP(13:18))
         CALL NAMEST (CHTMP, CATBLK(KIIMS), TEXT(35:), INCHAR)
         CALL REFRMT (TEXT, ' ', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, ELBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       the third line of header
         DY = 0.5
         CALL GPOS (BLC(1), TRC(2), ELBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Table type
         TEXT = TYPE
         WRITE (TEXT,1010) TYPE, SNVER
         INP = 7
C                                       Stokes and IF
         IF (NS.EQ.1) THEN
            TEXT(INP:) = ' ' // STKTMP
            INP = INP + 6
            END IF
         IF (NNIF.EQ.1) THEN
            WRITE (TEXT(INP:),1020) CURIF
            INP = INP + 6
            END IF
C                                       add new LK
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
         WRITE (TEXT(INP:),1041) TELNO(CURANT)
CCHANGELK
         IF (TELNO(CURANT) .LE. 9) THEN
            WRITE (TEXT(INP:),1041) TELNO(CURANT)
            INP = INP + 1
         ELSE
            WRITE (TEXT(INP:),1042) TELNO(CURANT)
            INP = INP + 2
            END IF
         END IF
C
      IF (NS.NE.1) THEN
         WRITE (TEXT(INP:),1015)  STKTMP
         INP = INP + 2
         END IF
      IF (NNIF.NE.1) THEN
         IF (CURIF .LE. 9) THEN
            WRITE (TEXT(INP:),1021) CURIF
            INP = INP + 5
         ELSE
            WRITE (TEXT(INP:),1020) CURIF
            INP = INP + 6
            END IF
         END IF
C
      IF (PRTLEV.GT.0) THEN
         C1 = PLUS
         IF (FITPAR(1) .LT. 0) C1 = MINUS
         C2 = PLUS
         IF (FITPAR(2) .LT. 0) C2 = MINUS
         C3 = PLUS
         IF (FITPAR(3) .LT. 0) C3 = MINUS
C
         IF (IPCOD.EQ.1) THEN
            IF (NOEXP) THEN
               IF (PRTLE2.EQ.0) THEN
                  WRITE (TEXT(INP:), 1150) C1, ABS(FITPAR(1)), C2,
     *               ABS(FITPAR(2)), C3, ABS(FITPAR(3)), RMSN, IOFFS
C                                       recalculate polinomial
C                                       coefficients to parameters of
C                                       maximum
               ELSE
                  GM = FITPAR(1) - (FITPAR(2) * FITPAR(2)) /
     *               (4 * FITPAR(3))
                  G2 = FITPAR(3)
                  ZM = -FITPAR(2) / (2 * FITPAR(3))
                  C1 = PLUS
                  IF (GM .LT. 0) C1 = MINUS
                  C2 = PLUS
                  IF (G2 .LT. 0) C2 = MINUS
                  C3 = MINUS
                  IF (ZM .LT. 0) C3 = PLUS
                  WRITE (TEXT(INP:),1155) C1, ABS(GM), C2, ABS(G2),
     *               C3, ABS(ZM)
                  END IF
            ELSE
               WRITE (TEXT(INP:), 1160) C1, ABS(FITPAR(1)), C2,
     *               ABS(FITPAR(2)), C3, ABS(FITPAR(3)), FITPAR(4)
               END IF
         ELSE
            IF (NOEXP) THEN
               IF (PRTLE2.EQ.0) THEN
                  WRITE (TEXT(INP:),1170) C1, ABS(FITPAR(1)), C2,
     *               ABS(FITPAR(2)), C3, ABS(FITPAR(3)), RMSN, IOFFS
C                                       recalculate polinomial
C                                       coefficients to parameters of
C                                       maximum
               ELSE
                  GM = FITPAR(1) - (FITPAR(2) * FITPAR(2)) /
     *               (4 * FITPAR(3))
                  G2 = FITPAR(3)
                  ZM = -FITPAR(2) / (2 * FITPAR(3))
                  C1 = PLUS
                  IF (GM .LT. 0) C1 = MINUS
                  C2 = PLUS
                  IF (G2 .LT. 0) C2 = MINUS
                  C3 = MINUS
                  IF (ZM .LT. 0) C3 = PLUS
                  WRITE (TEXT(INP:),1175) C1, ABS(GM), C2, ABS(G2),
     *               C3, ABS(ZM)
                  END IF
            ELSE
               WRITE (TEXT(INP:),1180) C1, ABS(FITPAR(1)), C2,
     *               ABS(FITPAR(2)), C3, ABS(FITPAR(3)), FITPAR(4)
               END IF
            END IF
         END IF
C
      IF (PRTLEV .GT. 0) THEN
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, ELBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Blank bottom label.
      IF ((IPLOT.GE.0) .AND. (ABS (IPLOT).NE.NCOUNT)) THEN
         CPREF(1,LOCNUM) = ' '
         CTYP(1,LOCNUM) = ' '
         END IF
C                                       Only label Y axis once.
      IAXLAB = NCOUNT / 2 + 1
      IAPLOT = ABS (IPLOT)
      IF ((IAPLOT.NE.IAXLAB) .AND. ((IPLOT.GE.0) .OR.
     *   (IAPLOT.GT.IAXLAB))) CPREF(2,LOCNUM) = '-1'
C                                       Put on labels and ticks
      CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, ELBUFF, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       take data
C                                       Size of symbol.
      DX = 5.0
      DY = 5.0
      DBY = 0.5
C                                       Loop to plot the data
      CALL GLTYPE (4, ELBUFF, IERR)
      IF (IERR.NE.0) GO TO 970
      DO 10 ITT = 1, ITIM
         XVARIB = ELE(ITT)
         VALUE = VALU(ITT)
         IF (VALUE.NE.FBLANK) THEN
            IF (IPCOD.EQ.4) VALUE = VALUE / FITPAR(4)
C                                       Scale X, Y
            XY(1) = XVARIB
            XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
            IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1))) THEN
               NNOFIT = NNOFIT + 1
               GO TO 10
               END IF
            XY(2) = VALUE
            XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
            IF ((XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))) THEN
               NNOFIT = NNOFIT + 1
               GO TO 10
               END IF
            NGOOD = NGOOD + 1
C                                       Mark point
            DY = 5.0
            CALL GPOS (XY(1)+DX, XY(2), ELBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL GVEC (XY(1)-DX, XY(2), ELBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            YPT = XY(2) + DY
            IF (YPT.GT.XTRC(2)) YPT = XTRC(2)
            CALL GPOS (XY(1), YPT, ELBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            YPT = XY(2) - DY
            IF (YPT.LT.XBLC(2)) YPT = XBLC(2)
            CALL GVEC (XY(1), YPT, ELBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
 10     CONTINUE
C-------------------------------------------------------------
C                                       Draw the model
C                                       NPMOD- number of points in model
C                                       draw
      NPMOD = 100
      IFIRST = 0
      DO 80 ITT = 1, NPMOD
         XVARIB = ELEVMN + (ITT-1)*((ELEVMX-ELEVMN)/(NPMOD-1.0))
         ELDEG = XVARIB
C                                       choose a model
         SC = SIN(ELDEG*DG2RAD)
         IF (NOEXP) THEN
            EXPON = 1
         ELSE
            IF (SC.NE.0.0) THEN
               EXPON = EXP(-FITPAR(4)/SC)
            ELSE
               EXPON = 0.0
               END IF
            END IF
         IF (IPCOD.EQ.1) THEN
            VALUE = (FITPAR(1) + FITPAR(2)*ELDEG +
     *         FITPAR(3)*ELDEG*ELDEG) * EXPON
         ELSE
            ZADEG = 90.0 - ELDEG
            VALUE = (FITPAR(1) + FITPAR(2)*ZADEG +
     *         FITPAR(3)*ZADEG*ZADEG) * EXPON
            END IF
C                                       Scale X, Y
         XY(1) = XVARIB
         XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
         IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1))) GO TO 80
         XY(2) = VALUE
         XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
C                                       move to the first point
         IFIRST = IFIRST + 1
         IF (IFIRST.EQ.1) THEN
            CALL GPOS (XY(1), XY(2), ELBUFF, IRET)
         ELSE
            CURENT = (XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))
            PREVOS = (PREV .LT.XBLC(2)) .OR. (PREV .GT.XTRC(2))
            IF (CURENT .OR. PREVOS) THEN
               CALL GPOS (XY(1), XY(2), ELBUFF, IRET)
            ELSE
               CALL GVEC (XY(1), XY(2), ELBUFF, IRET)
               END IF
            END IF
         PREV = XY(2)
 80      CONTINUE
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
  210    IF (IERR.GT.0) GO TO 975
         IRET = MIN (IERR, 0)
         GO TO 999
C                                       ZPHFIL or GINIT failure.
  960 CONTINUE
      IF (PRTLEV.GT.0) THEN
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
 970  CONTINUE
      IF (PRTLEV.GT.0) THEN
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
 1010 FORMAT (A2,I4, ';    RM is RMSN; OFF is POINTING  RMS')
 1015 FORMAT (A1)
 1020 FORMAT (' IF',I2)
 1021 FORMAT (' IF',I1)
 1030 FORMAT ('Plot file version',I4,'__created ',A, A)
 1040 FORMAT (A8,' (',I2,')_', '   RM is RMSN; OFF is POINTING  RMS')
 1041 FORMAT (I1)
 1042 FORMAT (I2)
 1200 FORMAT ('PLTEL: ',I9,' points plotted')
 1201 FORMAT ('PLTEL: ',I9,' points did not fit')
 1960 FORMAT ('PLTEL: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('PLTEL: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
 1150 FORMAT (A1,1PE8.2,A1,1PE8.2,'*EL',A1,1PE8.2,'*EL**2',
     * ' RM=', F5.3, ' OFF=', I2,'"'   )
 1155 FORMAT (A1, F6.3,1X,A1,1X,1PE9.3,'* (EL ',A1,F6.2, ')^2')
 1160 FORMAT ('(', A1,1PE8.2,A1,1PE8.2,'*EL',A1,1PE8.2,'*EL**2 )',
     *   '*EXP(-',1PE8.2,'/SIN(EL))' )
 1170 FORMAT (A1,1PE8.2,A1,1PE8.2,'*ZA',A1,1PE8.2,'*ZA**2;',
     * ' RM=', F5.3, ' OFF=', I2,'"'   )
 1175 FORMAT (A1, F6.3,1X, A1,1X,1PE9.3,'*(ZA ',A1,F6.2, ')^2')
 1180 FORMAT ('(', A1,1PE8.2, A1,1PE8.2,'*ZA',A1,1PE8.2,'*ZA**2 )',
     *   '*EXP(-',1PE8.2,'/COS(ZA))', 1X, F5.3 )
      END
C
      SUBROUTINE ELFIT (ELE, VALU, NIDC, ITIM, ITIMS, NTIMS, IPCOD,
     *   SID0, NOEXP, NFIT0, FITPAR, VX, MULT, VARRES, IRET)
C-----------------------------------------------------------------------
C   ELFIT fits  a function determined by OPCODE to the data VALU
C   given as a function of ELE
C   Input:
C      ELE     R(*)  Array of data arguments
C      VALU    R(*)  Array of data function
C      NIDC    I     Number of sources (calibrators)
C      ITIM    I     Total number of points at arrays ELE and VALU
C      ITIMS   I(*)  Number of points at arrays ELE and VALU for
C                    a source SID
C      NTIMS   I(*)  Number of points at arrays ELE and VALU for
C                    sources preceded SID
C      IPCOD   I     Determines a fitting function
C      SID0    I     The source number with best predicted flux
C   Output:
C      NFIT0   I     Number of parameters in fitting function
C      FITPAR  R(*)  Array of found parameters of fitting function
C      VX(*)   R(*)  Error(sigma) of the parameters
C      MULT    R(*)  Array of found factors to multiply data of sources
C      VARRES  R     Variance of the residuals.
C      IRET    I     Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INCLUDE 'MAXFIT.INC'
C
      INTEGER   NIDC, ITIM, ITIMS(*), NTIMS(*), IPCOD, SID0, NFIT0, IRET
      REAL      ELE(*), VALU(*), FITPAR(*), VX(*), MULT(*), VARRES
C
      INTEGER   I, NFIT, LESOL, IK, K, SID, IFIT, ITER, ISID, NT, IT,
     *   INDTIM, KFIT, IKFIT, NS0
      REAL      R(MAXFIT), MATR(MAXFIT*MAXFIT), NOBS, SUM, SSQ,
     *   SOL(MAXFIT), SSQRES, VARY, FIT, G0, G1, G2, FACTOR, DVALU, TAU,
     *   ROW(MAXFIT), ELORZA, FEXP, EXPON, POLIN, SC
      LOGICAL NOEXP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IRET = 0
C      ----------------------------------------------------------------
C      VALU(I; I=1...ITIM) = F * EXP(-TAU/COS(ZA) * (G0 + G1*X + G2*X*X)
C      X = ELEV, or X = ZA;  G0, G1, G2, F, TAU - unknown parameters
C      ------------ ---------------------------------------------------
C                                       find initial solution using
C                                       the data for all calibrators
C                                       supposing factors =1 for all
C                                       of them
      IF (NOEXP) THEN
         NFIT = 3
         CALL SOLIN3 (ELE, VALU, NFIT, ITIM, IPCOD, FITPAR,VARRES, IRET)
      ELSE
         NFIT = 4
         CALL SOLIN4 (ELE, VALU, NFIT, ITIM, IPCOD, FITPAR,VARRES, IRET)
         END IF
      NFIT0 = NFIT
      IF (IRET.NE.0) GO TO 999
C                                       There is only one source.
C                                       The source flux correction is
C                                       missed
      IF (NIDC.EQ.1) THEN
         MULT(1) = 1
         GO TO 999
         END IF
C                                       There is only one source
C                                       observed by the given antenna.
C                                       The source flux correction is
C                                       missed
      NS0 = 0
      DO 80 SID = 1, NIDC
         IF (ITIMS(SID).GT.0) NS0 = NS0 + 1
   80    CONTINUE
      IF (NS0.LE.1) THEN
         DO 90 SID = 1, NIDC
            MULT(SID) = 1
   90        CONTINUE
         GO TO 999
         END IF
C                                       Number of fit parameters is
C                                       more now by the number of
C                                       calibrators without one
      NFIT = NFIT0 + NIDC - 1
C                                       Initialize factors for all
C                                       calibrators to 1
      DO 110 IFIT = NFIT0+1, NFIT
         FITPAR(IFIT) = 1
  110    CONTINUE
C                                       start iterration to find
C                                       solutions for both G0,G1,G2,
C                                       TAU and factors for all
C                                       calibrators
      ITER = 1
  115 CONTINUE
C                                       Force result vector R(NFIT),
C                                       matrix M(NFIT*NFIT) to zero
      DO 130 I = 1, NFIT
         R(I) = 0.0
         DO 120 K = 1, NFIT
            IK = K + (I - 1)*NFIT
            MATR (IK) = 0.0
 120        CONTINUE
 130     CONTINUE

      SUM = 0.0
      SSQ = 0.0
      NOBS = 0.0
C                                       Prepare result vector R(NFIT)
C                                       and matrix MATR(NFIT*NFIT)
C                                       for routine LEASQR
      ISID = 0
      G0 = FITPAR(1)
      G1 = FITPAR(2)
      G2 = FITPAR(3)
      IF (NOEXP) THEN
         TAU = 0
      ELSE
         TAU = FITPAR(NFIT0)
         END IF
      DO 180 SID = 1, NIDC
         IF (SID.EQ.SID0) THEN
            FACTOR = 1
         ELSE
            ISID = ISID + 1
            FACTOR = FITPAR(NFIT0 + ISID)
            END IF
         NT = ITIMS(SID)
         DO 170 IT = 1,NT
            INDTIM = IT + NTIMS(SID)
C                                       exclude blank points
            IF (VALU(INDTIM).EQ.FBLANK) GOTO 170
            IF (IPCOD.EQ.1) THEN
               ELORZA = ELE(INDTIM)
               SC = SIN(ELORZA*DG2RAD)
            ELSE
               ELORZA = 90.0 - ELE(INDTIM)
               SC = COS(ELORZA*DG2RAD)
               END IF
            IF (SC.NE.0.0) THEN
               EXPON = EXP(-TAU/SC)
            ELSE
               EXPON = 0.0
               END IF
            FEXP = FACTOR * EXPON
            POLIN = G0 + G1*ELORZA + G2*ELORZA*ELORZA
            DVALU = VALU(INDTIM) - FEXP * POLIN
            NOBS = NOBS + 1
            SUM = SUM + DVALU
            SSQ = SSQ + DVALU*DVALU
C                                       prepare a row of matrix
            DO 140 IFIT = 1, NFIT
               IF (IFIT.LE.NFIT0) THEN
                  IF (NOEXP) THEN
                     ROW(IFIT) = (ELORZA**(IFIT-1)) * FEXP
                  ELSE
                     IF (IFIT.LT.NFIT0) THEN
                        ROW(IFIT) = (ELORZA**(IFIT-1)) * FEXP
                     ELSE
                        ROW(IFIT) = 0.0
                        IF (SC.NE.0) ROW(IFIT) = (-FEXP/SC) * POLIN
                        END IF
                     END IF
               ELSE
                  IF (ISID.EQ.IFIT-NFIT0) THEN
                     ROW(IFIT) = EXPON * POLIN
                  ELSE
                     ROW(IFIT) = 0.0
                     END IF
                  END IF
  140          CONTINUE
C
C                                       Prepare result vector R=A*RAT
C                                       and upper/right triangle of
C                                       matrix M = AT * A
            DO 160 IFIT = 1, NFIT
               R(IFIT) = R(IFIT) + DVALU*ROW(IFIT)
               DO 150 KFIT = IFIT, NFIT
                  IKFIT = IFIT + (KFIT-1)*NFIT
                  MATR(IKFIT) = MATR(IKFIT) + ROW(IFIT)*ROW(KFIT)
  150             CONTINUE
  160          CONTINUE
  170       CONTINUE
  180    CONTINUE
C
      CALL LEASQR (NFIT, NOBS, SUM, SSQ, R, MATR, SOL, VX, SSQRES,
     *   VARRES, VARY, FIT, LESOL)
      IRET = LESOL
      IF (IRET.NE.0) GO TO 999
C                                       find new solutions
      DO 190 IFIT = 1, NFIT
         FITPAR(IFIT) = FITPAR(IFIT) + SOL(IFIT)
C                                       Convert the variance of
C                                       of the fit parameters
C                                       to its rms
         IF (VX(IFIT) .GT. 0) VX(IFIT) = SQRT(VX(IFIT))
  190    CONTINUE
      ITER = ITER + 1
      IF (ITER.LE.4) GO TO 115
C                                       Solution for TAU
      TAU = FITPAR(NFIT0)
C                                       Fix factor for the best
C                                       calibrator to unit
      DO 195 SID = 1, NIDC-1
         IFIT = SID + NFIT0
         IF (SID.LT.SID0) THEN
            MULT(SID) = FITPAR(IFIT)
         ELSE
            MULT(SID+1) = FITPAR(IFIT)
            END IF
         IF (SID.EQ.SID0) THEN
            MULT(SID) = 1
            END IF
  195    CONTINUE
      IF (SID0.GT.NIDC-1) THEN
         MULT(NIDC) = 1
         END IF
      NFIT = NFIT + 1
C
 999  RETURN
      END
      SUBROUTINE SOLIN4(ELE, VALU, NFIT, ITIM,IPCOD,FITPAR,VARRES, IRET)
C-----------------------------------------------------------------------
C   Routine to find initial solution using the data for all calibrators
C   supposing factors =1 for all of them. Interpolation function is
C   a polinom of third degree multiplied by EXP(-tau*sec(ZA))
C   Input:
C      ELE     R(*)  Array of data arguments
C      VALU    R(*)  Array of data function
C      NFIT    I     Number of parameters to fit
C      ITIM    I     Total number of points at arrays ELE and VALU
C      IPCOD   I     Type of fit
C   Output:
C      FITPAR  R(*)  Array of found parameters of fitting function
C      VARRES  R     Variance of the residuals.
C      IRET    I     Error; 0 => OK
C-----------------------------------------------------------------------
      INTEGER  NFIT, ITIM, IPCOD, I, LESOL, IK, K, ITER, IFIT,
     *   KFIT, IKFIT, NFIT0, IRET
      REAL     ELE(*), VALU(*), R(10), MATR(100), NOBS, SUM, SSQ,
     *   SOL(10), VX(10), SSQRES, VARRES, VARY, FIT, FITPAR(*),
     *   ELORZA, G0, G1, G2, TAU, SC, EXPON, POLIN,
     *   DVALU, ROW(10)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C                                       find initial solution using
C                                       the data for all calibrators
C                                       supposing factors =1 for all
C                                       of them and TAU = 0
      NFIT0 = 3
      CALL SOLIN3 (ELE, VALU, NFIT, ITIM,IPCOD,FITPAR,VARRES,IRET)


      IF (IRET.NE.0) GO TO 999
C                                       Initialize TAU = 0
      FITPAR(NFIT) = 0
C                                       start iterration to find
C                                       solutions for both G0,G1,G2
C                                       and TAU
      ITER = 1
  100 CONTINUE
C                                       Force result vector R(NFIT),
C                                       matrix M(NFIT*NFIT) to zero
      DO 130 I = 1, NFIT
         R(I) = 0.0
         DO 120 K = 1, NFIT
            IK = K + (I - 1)*NFIT
            MATR (IK) = 0.0
  120       CONTINUE
  130    CONTINUE

         SUM = 0.0
         SSQ = 0.0
         NOBS = 0.0
C                                       Prepare result vector R(NFIT)
C                                       and matrix MATR(NFIT*NFIT)
C                                       for routine LEASQR
         G0 = FITPAR(1)
         G1 = FITPAR(2)
         G2 = FITPAR(3)
         TAU = FITPAR(NFIT)
      DO 180 I = 1, ITIM
C                                       exclude blank points
         IF (VALU(I).EQ.FBLANK) GOTO 180
         IF (IPCOD.EQ.1) THEN
            ELORZA = ELE(I)
            SC = SIN(ELORZA*DG2RAD)
         ELSE
            ELORZA = 90.0 - ELE(I)
            SC = COS(ELORZA*DG2RAD)
            END IF
         IF (SC.NE.0.0) THEN
            EXPON = EXP(-TAU/SC)
         ELSE
            EXPON = 0.0
            END IF
         POLIN = G0 + G1*ELORZA + G2*ELORZA*ELORZA
         DVALU = VALU(I) - EXPON * POLIN
         NOBS = NOBS + 1
         SUM = SUM + DVALU
         SSQ = SSQ + DVALU*DVALU
C                                       prepare a row of matrix
         DO 140 IFIT = 1, NFIT
            IF (IFIT.LT.NFIT) THEN
               ROW(IFIT) = (ELORZA**(IFIT-1)) * EXPON
            ELSE
               ROW(IFIT) = 0.0
               IF (SC.NE.0) ROW(IFIT) = (-EXPON/SC) * POLIN
               END IF
  140       CONTINUE
C
C                                       Prepare result vector R=A*RAT
C                                       and upper/right triangle of
C                                       matrix M = AT * A
         DO 160 IFIT = 1, NFIT
            R(IFIT) = R(IFIT) + DVALU*ROW(IFIT)
            DO 150 KFIT = IFIT, NFIT
               IKFIT = IFIT + (KFIT-1)*NFIT
               MATR(IKFIT) = MATR(IKFIT) + ROW(IFIT)*ROW(KFIT)
  150          CONTINUE
  160       CONTINUE
  180    CONTINUE
C
         CALL LEASQR (NFIT, NOBS, SUM, SSQ, R, MATR, SOL, VX, SSQRES,
     *                VARRES, VARY, FIT, LESOL)
         IRET = LESOL
         IF (IRET.NE.0) GO TO 999
C                                       find new solutions
         DO 190 IFIT = 1, NFIT
            FITPAR(IFIT) = FITPAR(IFIT) + SOL(IFIT)
  190       CONTINUE
         ITER = ITER + 1
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IF (ITER.LE.4) GO TO 100
C         IF (ITER.LE.2) GO TO 100
  999 RETURN
C-----------------------------------------------------------------
      END
C
      SUBROUTINE SOLIN3 (ELE, VALU, NFIT, ITIM,IPCOD,FITPAR,VARRES,IRET)
C-----------------------------------------------------------------------
C   Routine to find initial solution using the data for all calibrators
C   supposing factors =1 for all of them. Interpolation function is
C   a polinom of third degree
C   Input:
C      ELE     R(*)  Array of data arguments
C      VALU    R(*)  Array of data function
C      NFIT    I     Number of parameters to fit
C      ITIM    I     Total number of points at arrays ELE and VALU
C      IPCOD   I     Type of fit
C   Output:
C      FITPAR  R(*)  Array of found parameters of fitting function
C      VARRES  R     Variance of the residuals.
C      IRET    I     Error; 0 => OK
C-----------------------------------------------------------------------
      INTEGER  NFIT, ITIM, IPCOD, I, LESOL, IK, K, IRET
      REAL     ELE(*), VALU(*), R(10), MATR(100), NOBS, SUM, SSQ,
     *   SOL(10), VX(10), SSQRES, VARRES, VARY, FIT, FITPAR(*),
     *   ELORZA
      INCLUDE 'INCS:DDCH.INC'
C                                       Force result vector R(NFIT),
C                                       matrix M(NFIT*NFIT) to zero
      DO 20 I = 1, NFIT
         R(I) = 0.0
         DO 10 K = 1, NFIT
            IK = K + (I - 1)*NFIT
            MATR (IK) = 0.0
 10         CONTINUE
 20      CONTINUE
      SUM = 0.0
      SSQ = 0.0
      NOBS = 0.0
C                                       Prepare result vector R(NFIT)
C                                       and matrix MATR(NFIT*NFIT)
C                                       for routine LEASQR

      DO 40 I = 1, ITIM
C                                       exclude blank points
         IF (VALU(I).EQ.FBLANK) GOTO 40
         NOBS = NOBS + 1
         SUM = SUM + VALU(I)
         SSQ = SSQ + VALU(I)*VALU(I)
         IF (IPCOD.EQ.1 ) THEN
            ELORZA = ELE(I)
         ELSE
            ELORZA = 90.0 - ELE(I)
            END IF
C
         R(1) = R(1) + VALU(I)
         R(2) = R(2) + VALU(I)*ELORZA
         R(3) = R(3) + VALU(I)*ELORZA**2
C                                       calculate upper/right
C                                       triangle of MATR
         MATR(1) = MATR(1) + 1
         MATR(4) = MATR(4) + ELORZA
         MATR(5) = MATR(5) + ELORZA**2
         MATR(7) = MATR(7) + ELORZA**2
         MATR(8) = MATR(8) + ELORZA**3
         MATR(9) = MATR(9) + ELORZA**4
 40      CONTINUE
C
      CALL LEASQR (NFIT, NOBS, SUM, SSQ, R, MATR, SOL, VX, SSQRES,
     *   VARRES, VARY, FIT, LESOL)
C                                       solution for gain's coefficients
      FITPAR(1) = SOL(1)
      FITPAR(2) = SOL(2)
      FITPAR(3) = SOL(3)
      IRET = LESOL
  999 RETURN
      END
      SUBROUTINE ELHIS
C-----------------------------------------------------------------------
C   ELHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12,  HILINE*72, LABEL*8
      INTEGER   LUN, IERR, TIM(3), DATE(3), I
      LOGICAL   T
C      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'ELINT.INC'
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
      IF (IERR.NE.0) GO TO 200
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
      IF (NUMHIS.LE.0) GO TO 200
         WRITE (LABEL,1020) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
C                                       Close HI file
 200   CALL HICLOS (LUN, T, BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ELHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6, 'RELEASE =''',A7,' ''  /********* Start ',
     *   A12, 2X, A8)
 1020 FORMAT (A6)
      END
      SUBROUTINE OUTCL (IERR)
C-----------------------------------------------------------------------
C   OUTCL is called from CLCOR. OUTCL reads throught the CL table,
C   passing the records selected to the correction routine CLCCOR.
C   Output: IERR  I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LUN, IRCODE, THSOU, ANT, SID, IANT,
     *   ICLRNO, NUMREC, LOOP
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'ELINT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN /29/
C-----------------------------------------------------------------------
      FIXCNT = 0
C                                       Open CL table
      NUMPOL = 1
      IF (CATBLK(KINAX+JLOCS).GT.1) NUMPOL = 2
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
C                                       Reformat table?
      CALL CLREFM (DISKIN, CNOIN, CLUSE, CATBLK, LUN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CALINI ('WRIT', BUFFER, DISKIN, CNOIN, CLUSE, CATBLK, LUN,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
      IRCODE = 0
C
C                                       Update table
      DO 500 LOOP = 1,NUMREC
         ICLRNO = LOOP
         CALL TABIO ('READ', IRCODE, ICLRNO, CLRECR, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 900
         IF (IERR.LT.0) GO TO 500
C                                       Check data
C                                       Time:
         IF ((CLRECD(TIMCL).LT.TIMBEG) .OR.
     *       (CLRECD(TIMCL).GT.TIMEND)) GO TO 500
C                                       Subarray
         IF ((CLRECI(SUBCL).NE.SUBA) .AND. (CLRECI(SUBCL).GT.0))
     *      GO TO 500
C                                       Freq id
         IF ((CLRECI(FRQCL).NE.FREQID) .AND. (CLRECI(FRQCL).GT.0) .AND.
     *      (FREQID.GT.0)) GO TO 500
C                                       Check source
         THSOU = CLRECI(SOUCL)
         DO 30 SID = 1, NIDS
            IF (THSOU.EQ.IDS(SID)) GO TO 40
   30       CONTINUE
         GO TO 500
   40    CONTINUE
         RA = RAS(SID)
         DEC = DECS(SID)
C 70      LSTSOU = THSOU
C                                       Check antenna
         ANT = CLRECI(ANTCL)
         DO 50 IANT = 1, NSTNS
            IF (TELNO(IANT).EQ. ANT) GO TO 60
   50       CONTINUE
         GO TO 500
   60    CONTINUE
C                                       Correct record.
         FIXCNT = FIXCNT + 1
C                                       Correction of the CL row
         CALL ATMOV (IANT, IERR)
         IF (IERR.NE.0) GO TO 500
C                                       Rewrite record
         CALL TABIO ('WRIT', IRCODE, ICLRNO, CLRECR, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 900
 500     CONTINUE
C
      IF (NUMHIS.LT.MAXHIS) THEN
         NUMHIS= NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2900) FIXCNT
         END IF
      WRITE (MSGTXT,2901) FIXCNT, CLUSE
      CALL MSGWRT (6)
C                                       Close table.
      CALL TABIO ('CLOS', IRCODE, LOOP, CLRECR, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('TABIO ERROR',I3,' CORRECTING CL TABLE')
 2900 FORMAT (' / ',I6,' Records modified')
 2901 FORMAT (I6,' Records of the CL table ', I2, ' modified')
       END
      SUBROUTINE ATMOV (IANT, IERR)
C-----------------------------------------------------------------------
C   Routine to apply the gain  correction to the  given CL table row
C   on the basis of the fitted polynomials and
C   the source elevation
C   INPUT:
C      IANT      I    antenna number as it selected
C   Control info from common:
C      ISTOK     I    Polarization to correct, 1=first, 2=second,
C                     0 = both
C   Output:
C      IERR      I    Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   IANT, MXFIT, INDAF
      INTEGER   IFIT, JIF, KIF, JS, LTIM
      REAL      COSZ, ELERAD, ELORZA, HA, SC, EXPON, COEFF
      DOUBLE PRECISION  SIDT, TCLT
C                                       speed of light (meters/sec)
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'ELINT.INC'
      REAL FITPAR(MAXFIT)
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
      MXFIT = MAXFIT
C                                       calculate elevation of the
C                                       antenna for the given source
      TCLT = CLRECD(TIMCL)
      HA = TWOPI*SIDT*TCLT + GSTIAT + STNLON(IANT) - RA
      COSZ = DCOS(STNLAT(IANT))*DCOS(DEC)*COS(HA) +
     *   DSIN(STNLAT(IANT))*DSIN(DEC)
      IF (COSZ.LT.0.0) THEN
C                                       source is under horizon
         IERR = 1
         GO TO 999
      ELSE
         ELERAD = PI / 2.0 - ACOS (COSZ)
         SC = SIN(ELERAD)
C                                       Is observation overtoped?
         DO 60 LTIM = 1,NTOTT
            IF (TCLT.GE.TLEF(LTIM) .AND. TCLT .LE.
     *         TRIGH(LTIM)) THEN
               IF (OVETOP(LTIM)) ELERAD = PI - ELERAD
               GO TO 70
               END IF
   60       CONTINUE
   70    CONTINUE
         IF (IPCOD.EQ.1) THEN
            ELORZA = ELERAD * 180.0 / PI
         ELSE
            ELORZA = 90.0 - ELERAD * 180.0 / PI
            END IF
         END IF
      IF (ISTOK.EQ.2) GO TO 650
      DO 240 JIF = 1, NNIF
         KIF = BIF + JIF - 1
C
         JS = 1
         DO 80 IFIT = 1, MXFIT
            INDAF = JS + (JIF-1)*NS + (IANT-1)*NS*
     *         NNIF + (IFIT-1)*NS*NNIF*NSTNS
            FITPAR(IFIT) = FIT(INDAF)
   80       CONTINUE
C                                       POLX: G = G0 + G1*ZA + G2*ZA*ZA
         IF (NOEXP) THEN
            EXPON = 1
         ELSE
            IF (SC.NE.0.0) THEN
               EXPON = EXP(-FITPAR(4)/SC)
            ELSE
               EXPON = 0.0
               END IF
            END IF
C
         COEFF = (FITPAR(1) + FITPAR(2)*ELORZA
     *      + FITPAR(3)*ELORZA*ELORZA) * EXPON
         IF (IPCOD.EQ.1 .OR. IPCOD.EQ.2) THEN
            CLRECR(RE1CL+KIF-1) = CLRECR(RE1CL+KIF-1) * COEFF
            CLRECR(IM1CL+KIF-1) = CLRECR(IM1CL+KIF-1) * COEFF
C                                       return to voltage from fitted
C                                       power
         ELSE
            IF (COEFF.GT.0) THEN
               CLRECR(RE1CL+KIF-1) = CLRECR(RE1CL+KIF-1) / SQRT(COEFF)
               CLRECR(IM1CL+KIF-1) = CLRECR(IM1CL+KIF-1) / SQRT(COEFF)
            ELSE
               CLRECR(RE1CL+KIF-1) = FBLANK
               CLRECR(IM1CL+KIF-1) = FBLANK
               END IF
            END IF
  240    CONTINUE
 650  IF (ISTOK.EQ.1 .AND. NS.EQ.1) GO TO 999
      DO 260 JIF = 1, NNIF
         KIF = BIF + JIF - 1
         DO 180 IFIT = 1, MXFIT
            INDAF = NS + (JIF-1)*NS + (IANT-1)*NS*
     *         NNIF + (IFIT-1)*NS*NNIF*NSTNS
            FITPAR(IFIT) = FIT(INDAF)
  180       CONTINUE
C                                       POLX: G = G0 + G1*ZA + G2*ZA*ZA
         IF (NOEXP) THEN
            EXPON = 1
         ELSE
            IF (SC.NE.0.0) THEN
               EXPON = EXP(-FITPAR(4)/SC)
            ELSE
               EXPON = 0.0
               END IF
            END IF
C
         COEFF = (FITPAR(1) + FITPAR(2)*ELORZA
     *      + FITPAR(3)*ELORZA*ELORZA) * EXPON
         IF (IPCOD.EQ.1 .OR. IPCOD.EQ.2) THEN
            CLRECR(RE2CL+KIF-1) = CLRECR(RE2CL+KIF-1) * COEFF
            CLRECR(IM2CL+KIF-1) = CLRECR(IM2CL+KIF-1) * COEFF
C                                       return to voltage from fitted
C                                       power
         ELSE
            IF (COEFF.GT.0) THEN
               CLRECR(RE2CL+KIF-1) = CLRECR(RE2CL+KIF-1) / SQRT(COEFF)
               CLRECR(IM2CL+KIF-1) = CLRECR(IM2CL+KIF-1) / SQRT(COEFF)
            ELSE
               CLRECR(RE2CL+KIF-1) = FBLANK
               CLRECR(IM2CL+KIF-1) = FBLANK
               END IF
            END IF
  260    CONTINUE
C
 999  RETURN
      END
