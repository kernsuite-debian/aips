LOCAL INCLUDE 'ATMCA.INC'
C                                                          Include ATMCA
C                                       Local include for ATMCA
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MXSOU, MT, MAXFIT, MXAN, MXIFSO, MXANSO
      PARAMETER (MXSOU = 100)
      PARAMETER (MAXFIT = 100)
      PARAMETER (MXAN = 50)
      PARAMETER (MXIFSO = MAXIF*MXSOU)
C                                       MXSASO - product # antennas
C                                       and sources
      PARAMETER (MXANSO = MXAN*MXSOU)
      PARAMETER (MT = 10000)
      INTEGER NIDC, IDC(MXSOU), NIDS, IDS(MXSOU), NNIF, NS,
     *   NNNIF, SEQIN, SUBA, DISKIN, CNOIN, SNVER, SNOUT,
     *   CLVER, CLUSE, NANTSL, BIF, EIF, ISTOK, FREQID,
     *   NPARM, NSS, NUMHIS, NTERM, NFITA, KCLRNO, KSNRNO,
     *   NVALSO(MXANSO), RANT
      DOUBLE PRECISION RAC(MXSOU), DRAC(MXSOU), DECC(MXSOU),
     *   DDECC(MXSOU), RAS(MXSOU), DRAS(MXSOU), DECS(MXSOU),
     *   DDECS(MXSOU)
      LOGICAL    FIRSCL, FIRSSN, DOHIST, DOPLOT, DOUTFI, DOCL, DOSN,
     *   ENDSN, FSOUCL(MXSOU), FSOUSN(MXSOU), DOELRE, SNDELE
      REAL      XSIN, XDISIN, XFQID, XBAND, XFREQ, XBIF, XEIF, XTIME(8),
     *   XANT(50), XSUBA, XSNVER, XGVER, SOLINT, XDOHI, XBAD(10),
     *   APARM(10), SELBAN, TIMBEG, TIMEND, BUFF1(2048), FLUXX(MXIFSO),
     *   SOLANT(4*MXAN), TTARG, TSTART, TFINIS, VALSAN(MXANSO)
      DOUBLE PRECISION   SOL(MAXFIT), VX(MAXFIT), SSQRES, VARY, FIT,
     *   VARRES
      INTEGER   LESOL
      CHARACTER  HISCRD(1000)*64, NAMEIN*12, CLAIN*6, XSOUR(30)*16,
     *   XCALIB(30)*16, XSTOK*4, OPTYPE*4, OFILE*48,
     *   SOUS(30)*16, SOUC(30)*16, STOKK(2)*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALB(4,30),
     *   XXSTOK, XOPTY, XOFILE(12)
      DOUBLE PRECISION FRQOFF(MAXIF), SELFRQ, LAMBDA(MAXIF)
C                                       cpecific CL data
      INTEGER   CLRECI(13+32*MAXIF), CLKOLS(MAXCLC), CLNUMV(MAXCLC),
     *   FIXCL, FIXSN,
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
     *   XBIF, XEIF, XTIME, XANT, XSUBA, SOLINT,
     *   XOPTY, XOFILE, XDOHI, XBAD
      COMMON /OTHPRM/ SELBAN, SEQIN, DISKIN, CNOIN, SUBA, SNVER,
     *   SNOUT, CLVER, CLUSE, NPARM, NUMHIS, DOHIST,
     *   DOPLOT, DOUTFI, DOCL, DOSN, NFITA, FIRSCL, FIRSSN,
     *   ENDSN, FSOUCL, FSOUSN, DOELRE, SNDELE
      COMMON /EL/  RAC, DRAC, RAS, DRAS, DECC, DDECC, DECS, DDECS,
     *   IDC, NIDC, IDS, NIDS, NNIF, NS, NNNIF, ICODE, NSS, FLUXX,
     *   SOLANT, TTARG, TSTART, TFINIS, KCLRNO, KSNRNO, NVALSO,
     *   VALSAN
      COMMON /LEASQ/   SOL, VX, SSQRES, VARY, FIT, VARRES, LESOL
      COMMON /CINFO/ FRQOFF, SELFRQ, NANTSL, RANT, BIF, EIF, ISTOK,
     *   FREQID, TIMBEG, TIMEND
      COMMON /CHRCOM/ HISCRD, NAMEIN, CLAIN, XSOUR, XCALIB,
     *   XSTOK, OPTYPE, SOUS, SOUC, STOKK, OFILE
C                                       Buffers and file info
      COMMON /BUFRS/ BUFFER, BUFF1
C                                       Important constants
C                                       Internal storage
      COMMON /SNRECC/ LAMBDA, COSDEC, SINDEC, SNRECD, GMMOD,
     *   SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF,
     *   TIMSN, INTSN, SOUSN, ANTSN, SUBSN, FRQSN, IFRSN, NODSN,
     *   RE1SN, IM1SN, DL1SN, RA1SN, WT1SN, RF1SN,
     *   RE2SN, IM2SN, DL2SN, RA2SN, WT2SN, RF2SN, FIXSN
      COMMON /CLRECC/  CLRECD, NTERM,
     *   FIXCL, CLKOLS, CLNUMV
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
C
      EQUIVALENCE (SNRECI, SNRECR, SNRECD)
C                                                          End ATMCA
LOCAL END
      PROGRAM ATMCA
C-----------------------------------------------------------------------
C! Determines delay to the target source observing nearby calibrators
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2004-2005, 2011-2012, 2015
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
C   Task ATMCA provides estimation of the atmosphere delay towards
C   the target source using observation of several calibrators at its
C   vicinity
C   Inputs:
C      INNAME.....UV file name (name).       Standard defaults.
C      INCLASS....UV file name (class).      Standard defaults.
C      INSEQ......UV file name (seq. #).     0 => highest.
C      INDISK.....Disk unit #.               0 => any.
C      SNVER......The SN table version number - source of input data
C      GAINVER....Input CL table to copy to GAINUSE and then correct
C      GAINUSE....Output Cl table
C      APARM(10)..Control parameters
C      SOURCES....Source list to calibrate .
C                 ' ' = all; a "-" before a source
C                 name means all except ANY source named.
C                 The first in the list is the target source
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
C      SOLINT.....Duration of time intervals to find the solutions,
C                 in min.
C      OPTYPE.....The SN table Data to be fitted:
C                 'MDEL' =>  multiband delay,
C                 'PHAS' or '    ' =>  = phase delay
C      OUTFILE....Output file name. The free term and slopes for each
C                 solution interval and antennas.
C      BADDISK....A list of disks on which scratch files are not to
C                 be placed.
C
C programmer: Leonia Kogan, May 2004
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'ATMCA.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'ATMCA '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL ATMCIN (PRGM, IRET)
C                                       read SN table, select
C                                       calibrators. Fits the plane,
C                                       correct CL table
      IF (IRET.EQ.0) CALL ATMAIN (IRET)
C                                       Copy and update HI file.
      IF (IRET.EQ.0) CALL ATMHIS
C                                       Close down files, etc.
      CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE ATMCIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   ATMCIN gets input parameters for ATMCA.
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
     *   NIDCC, IS, SID, IEND, NUMCL, LUN2, TABUFF(512)
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'ATMCA.INC'
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
C
      DATA NCODE, CODE /5, 'PHAS','MDEL','    ','    ','    '/
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
C      NPARM = 353
C      NPARM = 354
C                                       removed NPLOTS, XINC, DOTV,
C                                       GRCHAN, PRTLEV
      NPARM = 349
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
C
      DOHIST = XDOHI.GT.0.0
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
C                                       Number of terms at the
C                                       fitting linear combination
      NFITA = APARM(1)
C                                       default NFITA=2
      IF (NFITA.LT.1) NFITA=2
C                                       fit elevations for ant and ref
      DOELRE = .FALSE.
      IF (NFITA .EQ. 5) THEN
         DOELRE = .TRUE.
         NFITA = 2
         END IF
C                                       Create CL table?
      DOCL = (APARM(2).GT.0.5)
C                                       Create output SN table?
C      DOSN = (APARM(3).GT.0.5 .AND. (NFITA.NE.1 .AND. .NOT.DOELRE))
      DOSN = (APARM(3).GT.0.5)
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
C                                       determine number of SN tables
      CALL FNDEXT ('SN', CATBLK, SNTOT)
      IF (SNVER.EQ.0 .OR. SNVER.GT.SNTOT) SNVER = SNTOT
C                                       output SN table
      SNOUT = SNTOT + 1
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
C                                       initiate the sources for future
C                                       using in ATMOCL
      DO 50 IS = 1, NIDS
         FSOUCL(IS) = .TRUE.
   50    CONTINUE
C                                       initiate the sources for future
C                                       using in ATMOSN
      DO 55 IS = 1, NIDS
         FSOUSN(IS) = .TRUE.
   55    CONTINUE
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
     *   BUFFER, IDC, NIDC, RAC, DECC, SOUC, FLUXX, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       store the calibrator positions
C                                       relatively the first calibrator
      DO 65 SID  = 1, NIDC
         DDECC(SID) = DECC(SID) - DECC(1)
         DRAC(SID) = (RAC(SID) - RAC(1)) *
     *      DCOS( (DECC(SID) + DECC(1))/2 )
   65    CONTINUE
C                                       store the source  positions
C                                       relatively the first calibrator
      DO 68 SID  = 1, NIDS
         DDECS(SID) = DECS(SID) - DECC(1)
         DRAS(SID) = (RAS(SID) - RAC(1)) *
     *      DCOS( (DECS(SID) + DECC(1))/2 )
   68    CONTINUE
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
         WRITE (HISCRD(NUMHIS),2600) (SOUC(3*(IS-1) + I), I = 1,IEND)
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
 1000 FORMAT ('ATMCIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
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
      SUBROUTINE ATMAIN (IERR)
C-----------------------------------------------------------------------
C   ATMAIN is called from ATMCA. ATMAIN reads through the SN table,
C   fits the plain to the calibrator data, makes the corrections for
C   the  sources listed in SOURCES into the CL table.
C   Output: IERR  I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      LOGICAL   T, F
      CHARACTER COLHED(2)*24, IGFILE*48
      INTEGER   NKEY, LKEY, KEY(2,2), TAKOLS(2), TABUFF(512),
     *   KEYSUB(2,2)
      REAL      FKEY(2,2)
C
      INTEGER   LUN, LUN2, ISNRNO, NUMREC, NUMNOD, ISN, JIF, JS,
     *   SOUN, FREQN, MODENO, IANT, KS, KIF, LANT, SID, SIDSTA,
     *   SIDK, NFIT, IFIT, KFIT, IKFIT, IC, SNOUTP,
     *   KC, LFIT, LIFIT, IIFSTO, KANT, IFITA, IFIANT, ICLRNO
      LOGICAL   ISAPL, FIRST
      REAL      RANOD(25), DECNOD(25), ELEV, COSZ, DELEV, DELEVR,
     *   ELEREF
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'ATMCA.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      REAL      REAL(2,MAXIF), IMAG(2,MAXIF), DELAY(2,MAXIF),
     *   RATE(2,MAXIF), WEIGHT(2,MAXIF), MBDELY(2), TIMINT, IFRM,
     *   HL, COEF(MAXFIT), MM2DEG, RMSD, DISP(2), DDISP(2)
      INTEGER   REFA(2,MAXIF), REFN, REFOLD, ISOLIN, IFSIDE(MAXIF), IIF,
     *   LUNPR, PFIND, NCH, ITRIM, TIME(3), NOBSSO(MXAN), NOBSS, INOBS,
     *   BUFF3(256), BUFF4(256)
      LOGICAL   YESSOU(MXANSO)
      REAL      SECNDS
      CHARACTER TSIGN, LINE*80, BNDCOD(MAXIF)*8

      DOUBLE PRECISION IFFREQ(MAXIF)
      DOUBLE PRECISION  R(MAXFIT), MATR(MAXFIT*MAXFIT),
     *   NOBSAN(MXAN), SUMANT(MXAN), SSQANT(MXAN),
     *   RRANT(MAXFIT), MTRANT(MAXFIT*MAXFIT), NOBS, SUM, SSQ
C
      DOUBLE PRECISION TCENTE
      REAL      IFCHW(MAXIF), IFTBW(MAXIF)
      REAL VALCUR
Ctemporal
      REAL PHAS0(10), SLRA(10), SLDEC(10), SLTIME(10)
      DOUBLE PRECISION CTIM, SIDT
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
C      INCLUDE 'INCS:ZPBUFSZ.INC'
      DATA LUN, LUN2  /29, 28/
      DATA SIDT /1.00273790935D0/
      DATA NKEY, LKEY /2,24/
      DATA T, F /.TRUE.,.FALSE./
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA COLHED /'TIME', 'ANTENNA NO.'/
C-----------------------------------------------------------------------
C                                       Edge time in days
C                                       Jumps accumulated until this
C                                       time
C      TCENTE = (4 + 40.0/60.0)/24.0
      TCENTE = APARM(5)
C                                       sort the CL table if required
      IF (DOCL) THEN
C                                       Open CL table
         CALL CALINI ('READ', BUFFER, DISKIN, CNOIN, CLUSE, CATBLK,
     *      LUN, ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF,
     *      NTERM, GMMOD, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Get number of records
         NUMREC = BUFFER(5)
         IF (NUMREC.LE.0) GO TO 999

C                                       Set column pointers for sort
         CALL FNDCOL (NKEY, COLHED, LKEY, T, BUFFER, TAKOLS, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
C                                       Close CL table.
         CALL TABIO ('CLOS', 0, ICLRNO, BUFFER, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Sort to time-ant order.
C                                       if necessary
         IF (BUFFER(43).NE.TAKOLS(1) .OR. BUFFER(44).NE.TAKOLS(2))
     *      THEN
            KEY(1,1) = TAKOLS(1)
            KEY(2,1) = TAKOLS(1)
            KEY(1,2) = TAKOLS(2)
            KEY(2,2) = TAKOLS(2)
            CALL TABSRT (DISKIN, CNOIN, 'CL', CLUSE, CLUSE, KEY, KEYSUB,
     *         FKEY, BUFFER, CATBLK, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1200) IERR
               GO TO 990
               END IF
            END IF
         END IF
C                                       end of CL table sort
      CALL GETFQ (FREQID, DISKIN, CNOIN, CATBLK, LUN, IFFREQ, IFTBW,
     *   IFCHW, IFSIDE, BNDCOD, IERR)
C
      DO 15 IIF = 1, NUMIF
C                                       Wavelength in mm
         LAMBDA(IIF) = VELITE / (SAFREQ + IFFREQ(IIF)) * 1000
   15    CONTINUE
      IF (IERR.NE.0) GO TO 900

C                                       solve the phase ambiguaty
C                                       if phase is used
      IF (ICODE.EQ.1) THEN
C                                       intermediate table next after
C                                       the last one
         SNOUTP = SNOUT
C                                       intermediate table next after
C                                       the output SN table if it
C                                       creates
         IF (DOSN) SNOUTP = SNOUT + 1
         CALL SNAMBG (DISKIN, CNOIN, SNVER, SNOUTP, TCENTE, IERR)
         WRITE (MSGTXT,1650) SNOUTP
         CALL MSGWRT (8)
C
         SNVER = SNOUTP
         END IF
C   --------------------------------------------------------------
C   |  Read the SN table the first time to sort it by ant-time   |
C   --------------------------------------------------------------
C                                       Open input SN table
      CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, SNVER, CATBLK, LUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
C                                       Set column pointers for sort
      CALL FNDCOL (NKEY, COLHED, LKEY, T, BUFFER, TAKOLS, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 990
         END IF
C                                       Close SN table.
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Sort to time-ant order.
C                                       if necessary
      IF (BUFFER(43).NE.TAKOLS(1) .OR. BUFFER(44).NE.TAKOLS(2))
     *   THEN
         KEY(1,1) = TAKOLS(1)
         KEY(2,1) = TAKOLS(1)
         KEY(1,2) = TAKOLS(2)
         KEY(2,2) = TAKOLS(2)
C                                       sort input SN table
         CALL TABSRT (DISKIN, CNOIN, 'SN', SNVER, SNVER, KEY, KEYSUB,
     *      FKEY, BUFFER, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1200) IERR
            GO TO 990
            END IF
         END IF
C                                       copy the sorted input SN table
C                                       to the output SN table
      IF (DOSN) THEN
         CALL TABCOP ('SN', SNVER, SNOUT, LUN, LUN2, DISKIN, DISKIN,
     *      CNOIN, CNOIN, CATBLK, BUFF1, TABUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1250) IERR, SNVER, SNOUT
            GO TO 990
            END IF
         END IF
C   -----------------------------------------------------------
C   |   Read the SN table the second time to store solutions  |
C   |           (delays) for each selected antenna            |
C   -----------------------------------------------------------
C
C                                       open the OUTFILE
      IF (DOUTFI) THEN
         LUNPR = 3
         CALL ZTXOPN ('WRIT', LUNPR, PFIND, OFILE, F, IERR)
         END IF
C                                       Open SN table
      CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, SNVER, CATBLK, LUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPL, IERR)
      IF (IERR.NE.0) GO TO 999
C
      NFIT = NFITA*NSTNS
C
C                                       Force result vector R(NFIT),
C                                       matrice MATR, NOBS, SUM, SSQ
C                                       to zero
      DO 30 IANT = 1, NSTNS
         NOBSAN(IANT) = 0
         SUMANT(IANT) = 0
         SSQANT(IANT) = 0
         NOBSSO(IANT) = 0
C                                       Initiate all selected sources
C                                       appearence for antennas
C                                       in the solution interval
         DO 20 SID = 1, NIDC
            YESSOU(SID + NIDC*(IANT-1)) = T
            VALSAN(SID + NIDC*(IANT-1)) = 0
            NVALSO(SID + NIDC*(IANT-1)) = 0
   20       CONTINUE
   30    CONTINUE
      DO 50 IFIT = 1, NFIT
         R(IFIT) = 0.0
         DO 40 KFIT = 1, NFIT
            IKFIT = KFIT + (IFIT - 1)*NFIT
            MATR (IKFIT) = 0.0
   40       CONTINUE
   50    CONTINUE
      ISOLIN = 0
C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
      FIRST = T
C                                       default of SOLIN is 30min
      IF (SOLINT .LT. 0.1) SOLINT = 30
C                                       SOLINT in days
      SOLINT = SOLINT/60.0/24.0
C                                       model to test
      DO 60 IANT = 1, NSTNS
         PHAS0(IANT) = 11 + (IANT-1)*5
         SLRA(IANT) = (5 + (IANT-1)*0.2) / DG2RAD
         SLDEC(IANT) = (3 + (IANT-1)*0.3) / DG2RAD
         SLTIME(IANT) = (2 + (IANT-1)*0.1) * 24.0 * 60.0
   60    CONTINUE
C                                       initial CL row number
      KCLRNO = 1
      KSNRNO = 1
      FIRSCL = .TRUE.
      FIRSSN = .TRUE.
C                                       conversion mm to degree
      MM2DEG = 360.0/LAMBDA(1)
C                                       Do not use the first(reference)
C                                       calibrator, if the only two
C                                       slopes are going to find.
C                                       (NFITA=APARM(1)=2)
C                                       Count of the calibrator list
C                                       will start with the second
C                                       calibrator (SIDSTA=2)
      IF (NFITA .EQ. 2) THEN
         SIDSTA = 2
      ELSE
         SIDSTA = 1
         END IF
C                                       Still use the first (reference)
C                                       calibrator points if APARM(4)=0
      IF (APARM(4) .EQ. 0 .AND. .NOT.DOELRE) SIDSTA = 1
C                                       delete? the intermediate SN
C                                       table
      SNDELE = APARM(6) .EQ. 0
C                                       RANOLD for checking if there is
C                                       only one reference antenna
      REFOLD = -10
C                                       cycle to NUMREC+1 to include
C                                       the last point to the solution
      DO 500 ISNRNO = 1, NUMREC+1
         ENDSN = ISNRNO.EQ.NUMREC+1
         IF (ENDSN) GO TO 115
C                                       read SN table
         ISN = ISNRNO
         CALL TABSN ('READ', BUFFER, ISN, SNKOLS, SNNUMV, NSS, CTIM,
     *      TIMINT, SOUN, LANT, SUBA, FREQN, IFRM, MODENO, MBDELY, DISP,
     *      DDISP, REAL, IMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C                                       check if record is flagged
         IF (IERR.LT.0) GO TO 500
C                                       FREQID selection
         IF ((FREQN.NE.0) .AND. (FREQN.NE.FREQID)) GO TO 500
C                                       antennas selection
         DO 70 IANT = 1,NSTNS
            IF (TELNO(IANT).EQ.LANT) GO TO 80
   70       CONTINUE
         GO TO 500
C                                       calibrators selection
C                                       To use the first(reference)
C                                       calibrator or not to use?
C
   80    DO 90 SID = SIDSTA, NIDC
            IF (IDC(SID).EQ.SOUN) GO TO 110
   90       CONTINUE
         GO TO 500
  110    CONTINUE
C?????????????check comparison CTIM=D with TIMBEG=R
C                                       timerange selection
         IF (CTIM.LT.TIMBEG) THEN
            GO TO 500
         ELSE
            IF (FIRST) THEN
               TSTART = CTIM
               TFINIS = TSTART + SOLINT
C                                       time of target source  TTARG
C                                       as the center of the current
C                                       solint interval
               TTARG = (TSTART + TFINIS) / 2
               FIRST = F
               END IF
            END IF

C                                       timerange selection
         IF (CTIM.GT.TIMEND) GO TO 520
C                                       calculates elevations and
C                                       use the refer. antennas
C                                       only IF DOELRE=T
         IF (.NOT. DOELRE) GO TO 115
C                                       Exclude the rows IANT=REFN
         REFN = REFA(1,1)
C
C                                       Exclude the rows IANT=REFN
         IF (TELNO(IANT) .EQ. REFN) GO TO 500
C                                       calculate elevation of
C                                       the SN row antenna for
C                                       the SN row source
         HL = TWOPI*SIDT*CTIM + GSTIAT + STNLON(IANT) - RAC(SID)
         COSZ = DCOS(STNLAT(IANT))*DCOS(DECC(SID))*COS(HL) +
     *      DSIN(STNLAT(IANT))*DSIN(DECC(SID))
C                                       source is under horizon
         IF (COSZ.LT.0.0) GO TO 500
         ELEV = PI/2.0 - ACOS(COSZ)

C                                       calculate elevation of
C                                       the SN row antenna for
C                                       the reference calibrator
C                                       (first in the list)
         HL = TWOPI*SIDT*CTIM + GSTIAT + STNLON(IANT) - RAC(1)
         COSZ = DCOS(STNLAT(IANT))*DCOS(DECC(1))*COS(HL) +
     *      DSIN(STNLAT(IANT))*DSIN(DECC(1))
C                                       source is under horizon
         IF (COSZ.LT.0.0) GO TO 500
C                                       difference in elevation
C                                       for the antenna IANT:
C                                       given source - ref. source
         DELEV = ELEV -(PI/2.0 - ACOS(COSZ))
C                                       Is the reference antenna
C                                       in the selected antenna list?
         IF (REFOLD .EQ. -10) THEN
C                                       select the first line reference
C                                       antenna
            REFOLD = REFN
            DO 111 RANT = 1,NSTNS
               IF (TELNO(RANT).EQ.REFN) GO TO 112
  111       CONTINUE
C
            IERR = 1
            WRITE (MSGTXT,1040) REFN
            GO TO 990
C
  112       CONTINUE
         ELSE
            IF (REFN .EQ. REFOLD) THEN
               GO TO 113
            ELSE
               IERR = 1
               WRITE (MSGTXT,1050) ISN
               GO TO 990
               END IF
            END IF
C                                       calculate elevation of the
C                                       SN row reference antenna
C                                       for the SN row source
  113    HL = TWOPI*SIDT*CTIM + GSTIAT + STNLON(RANT) - RAC(SID)
         COSZ = DCOS(STNLAT(RANT))*DCOS(DECC(SID))*COS(HL) +
     *      DSIN(STNLAT(RANT))*DSIN(DECC(SID))
C                                       source is under horizon
         IF (COSZ.LT.0.0) GO TO 500
         ELEREF = PI/2.0 - ACOS(COSZ)
C                                       calculate elevation of
C                                       the refer. antenna for
C                                       the reference calibrator
C                                       (first in the list)
         HL = TWOPI*SIDT*CTIM + GSTIAT + STNLON(RANT) - RAC(1)
         COSZ = DCOS(STNLAT(RANT))*DCOS(DECC(1))*COS(HL) +
     *      DSIN(STNLAT(RANT))*DSIN(DECC(1))
C                                       source is under horizon
         IF (COSZ.LT.0.0) GO TO 500
C                                       difference in elevation
C                                       for the refer. antenna :
C                                       given source - ref. source
         DELEVR = ELEREF -(PI/2.0 - ACOS(COSZ))
C----------------begin of the current solution----------------------
  115    CONTINUE
C
         IF ((CTIM.GT.TFINIS) .OR. ENDSN) THEN
            IF (NFITA .EQ. 1) THEN
C                                       average phases/delays
               DO 118 KANT = 1, NSTNS
                  DO 117 SIDK = 1, NIDC
                     IF (NVALSO(SIDK + NIDC*(KANT-1)) .NE. 0)
     *                  VALSAN(SIDK + NIDC*(KANT-1)) =
     *                  VALSAN(SIDK + NIDC*(KANT-1)) /
     *                  NVALSO(SIDK + NIDC*(KANT-1))
  117                CONTINUE
  118             CONTINUE
               GO TO 205
               END IF
            ISOLIN = ISOLIN + 1
C                                       find the solition for each
C                                       selected antenna
            CALL TFDHMS (TTARG, 1, TSIGN, TIME, SECNDS)
C                                       header of the print out
            IF (ICODE.EQ.1) THEN
               WRITE (MSGTXT,1500)
            ELSE IF (ICODE.EQ.2) THEN
               WRITE (MSGTXT,1600)
               END IF
            CALL MSGWRT (8)
            IF (DOUTFI) THEN
C                                       header of the outfile
               IF (ICODE.EQ.1) THEN
                  WRITE (LINE, 1500)
               ELSE IF (ICODE.EQ.2) THEN
                  WRITE (LINE,1600)
                  END IF
               NCH = ITRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IERR)
               END IF
C
            DO 200 KANT = 1, NSTNS
               NOBS = NOBSAN(KANT)
               INOBS = NOBS
C                                       number of succesfully observed
C                                       calibrators
               NOBSS = NOBSSO(KANT)
C                                       allow the same source for
C                                       APARM(1) = 5
               IF (DOELRE) NOBSS = NOBS
C                                       Include all source observations
C                                       if time slope fit is included
               IF (NFITA .EQ. 4) NOBSS = NOBS
C                                       find solution only if number of
C                                       observed succesfully sources
C                                       .GE.NFITA
               IF (NOBSS.GE.NFITA) THEN
                  SUM =  SUMANT(KANT)
                  SSQ =  SSQANT(KANT)
                  DO 130 IC = 1, NFITA
                     IFIT = IC + NFITA*(KANT-1)
                     RRANT(IC) = R(IFIT)
                     DO 120 KC = 1, NFITA
                        LFIT = KC + NFITA*(KANT-1)
                        LIFIT = LFIT + (IFIT-1)*NFIT
                        MTRANT(KC + (IC-1)*NFITA) = MATR(LIFIT)
  120                   CONTINUE
  130                CONTINUE
C
                  CALL DLESQR (NFITA, NOBS, SUM, SSQ, RRANT, MTRANT,
     *                  SOL,VX, SSQRES, VARRES, VARY, FIT, LESOL)
                  RMSD = SQRT(VARRES)
C
                  IF (DOUTFI) THEN
C                                       record in the OUTFILE:
                     IF (ICODE.EQ.1) THEN
                        WRITE (LINE,1300) TSIGN, TIME, SECNDS,
     *                     TELNO(KANT), INOBS, SOL(3)*MM2DEG,
     *                     SOL(1)*DG2RAD*MM2DEG,
     *                     SOL(2)*DG2RAD*MM2DEG,
     *                     SOL(4)/24.0/60.0*MM2DEG, RMSD*MM2DEG
                     ELSE IF (ICODE.EQ.2) THEN
                        WRITE (LINE, 1350) TSIGN, TIME, SECNDS,
     *                     TELNO(KANT), INOBS, SOL(3),
     *                     SOL(1)*DG2RAD,
     *                     SOL(2)*DG2RAD, SOL(4)/24.0/60.0, RMSD
                        END IF

C
                     NCH = ITRIM(LINE)
                     CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH),
     *                  IERR)
                     END IF
               ELSE
                  DO 140 IFITA = 1, NFITA
                     SOL(IFITA)= 0
  140                CONTINUE
                  END IF
C
               IF (ICODE.EQ.1) THEN
                  WRITE (MSGTXT,1300) TSIGN, TIME, SECNDS,
     *               TELNO(KANT), INOBS, SOL(3)*MM2DEG,
     *               SOL(1)*DG2RAD*MM2DEG,
     *               SOL(2)*DG2RAD*MM2DEG,
     *               SOL(4)/24.0/60.0*MM2DEG, RMSD*MM2DEG
               ELSE IF (ICODE.EQ.2) THEN
                  WRITE (MSGTXT,1350) TSIGN, TIME, SECNDS,
     *               TELNO(KANT), INOBS, SOL(3), SOL(1)*DG2RAD,
     *               SOL(2)*DG2RAD, SOL(4)/24.0/60.0, RMSD
                  END IF
               CALL MSGWRT (8)
               DO 150 IFITA = 1, NFITA
                  IFIANT = IFITA + NFITA*(KANT-1)
                  SOLANT(IFIANT) = SOL(IFITA)
  150             CONTINUE
  200          CONTINUE
C                                       End of solution for both
C                                       NFITA.EQ.1 and NFITA.NE.1
  205       CONTINUE
C                                       update CL table
            IF(DOCL) CALL OUTCL(IERR)
C                                       update output SN table
            IF(DOSN) CALL OUTSN(IERR)
C                                       Force result vector R(NFIT),
C                                       matrice MATR, NOBS, SUM, SSQ
C                                       to zero
C
            DO 220 KANT = 1, NSTNS
               NOBSAN(KANT) = 0
               SUMANT(KANT) = 0
               SSQANT(KANT) = 0
               NOBSSO(KANT) = 0
C                                       Initiate all selected sources
C                                       appearence for antennas
C                                       in the solution interval
               DO 210 SIDK = 1, NIDC
                  YESSOU(SIDK + NIDC*(KANT-1)) = T
                  VALSAN(SIDK + NIDC*(KANT-1)) = 0
                  NVALSO(SIDK + NIDC*(KANT-1)) = 0
  210             CONTINUE
  220          CONTINUE
C
            DO 240 IFIT = 1, NFIT
               R(IFIT) = 0.0
               DO 230 KFIT = 1, NFIT
                  IKFIT = KFIT + (IFIT - 1)*NFIT
                  MATR (IKFIT) = 0.0
  230             CONTINUE
  240          CONTINUE
            TSTART = CTIM
            TFINIS = TSTART + SOLINT
C                                       time of target source
C                                       TTARG for the current solint
C                                       interval
            TTARG = (TSTART + TFINIS) / 2
            END IF
C----------------end of the current solution----------------------
C                                       finish if ISNRNO.EQ.NUMREC+1
         IF (ENDSN) GO TO 500
C
C                                       average VALs through all IFs
C                                       and stokes
         VALCUR = 0
         IIFSTO = 0
         DO 280 JIF = 1,NNIF
            KIF = BIF + JIF -1
            DO 270 JS = 1,NS
               KS = ISTOK + JS -1
C                                       chose a type of variable
C                                       phase / delay
               IF (ICODE.EQ.1) THEN
C                                       ICODE=1 corresponds to the
C                                       phase delay
                  IF ((REAL(KS,KIF).EQ.FBLANK) .OR.
     *               (IMAG(KS,KIF).EQ.FBLANK)) THEN
                     GO TO 270
                  ELSE
                     IIFSTO = IIFSTO + 1
C                                       phase delay in mm
C                     VALCUR = VALCUR +
C     *                  ATAN2(IMAG(KS,KIF), REAL(KS,KIF))
C     *                  / TWOPI * LAMBDA(KIF)
C                                       REAL is phase in degrees
C                                       solved by two pi ambiguaty
                     VALCUR = VALCUR +
     *                  REAL(KS,KIF) / 360.0 * LAMBDA(KIF)
                     END IF
               ELSE IF (ICODE.EQ.2) THEN
C                                       MBDELY should be identical
C                                       for all IFS. So use only
C                                       JIF=1
                  IF (JIF .GT. 1) GO TO 270
                  IF (MBDELY(KS).EQ.FBLANK) THEN
                     GO TO 270
                  ELSE
                     IIFSTO = IIFSTO + 1
C                                       delay in mm
                     VALCUR = VALCUR +
     *                  MBDELY(KS) *VELITE *1000
                     END IF
                  END IF
C
 270           CONTINUE
 280        CONTINUE
C
         IF (IIFSTO.GT.0) THEN
C                                       VALUE.NE.FBLANK at least
C                                       for one IF,STOKES
            VALCUR = VALCUR / IIFSTO
         ELSE
C                                       VALCUE.EQ.FBLANK for all
C                                       IF,STOKES
            VALCUR = FBLANK
            END IF
C
         IF (VALCUR.EQ.FBLANK) GOTO 500
C                                       accumulate the values for all
C                                       antennas and calibrators
         VALSAN(SID + NIDC*(IANT-1)) =
     *      VALSAN(SID + NIDC*(IANT-1)) + VALCUR
         NVALSO(SID + NIDC*(IANT-1)) =
     *      NVALSO(SID + NIDC*(IANT-1)) + 1
C                                       Go to the next line if
C                                       NFITA.EQ.1
         IF (NFITA .EQ. 1) THEN
            GO TO 500
            END IF
C                                       coefficients of linear
C                                       interpretation of VALCUR
C                                       in vicinity of the reference
C                                       source. The reference source
C                                       is the first source at the
C                                       CAIBRATOR list!!!!
         IF (NFITA .EQ. 2) THEN
            IF (DOELRE) THEN
C                                       elevation gradients for
C                                       the Antenna and Reference ant.
C                                       in radians

               COEF(1) =  DELEV
               COEF(2) = -DELEVR
               COEF(3) = 0
               COEF(4) = 0
C                                       for test
C               VALCUR = (5*COEF(1) + 2*COEF(2))*RAD2DG
            ELSE
C                                       Only two slopes (RA,DEC)
C                                       are found
C                                       No solution for the offset
C                                       (COEF(3)=0)
               COEF(1) = DRAC(SID)
               COEF(2) = DDECC(SID)
               COEF(3) = 0
               COEF(4) = 0
               END IF
            END IF
         IF (NFITA .EQ. 3) THEN
C                                       The two slopes plus offset are
C                                       found. Yes solution for the
C                                       offset (COEF(3)=1)
            COEF(1) = DRAC(SID)
            COEF(2) = DDECC(SID)
            COEF(3) = 1
            COEF(4) = 0
            END IF
         IF (NFITA .EQ. 4) THEN
C                                       The two space slopes plus
C                                       offset plus time slopes are
C                                       found.
            COEF(1) = DRAC(SID)
            COEF(2) = DDECC(SID)
            COEF(3) = 1
            COEF(4) = CTIM - TTARG
            END IF
C
         NOBSAN(IANT) = NOBSAN(IANT) + 1
         IF (YESSOU(SID + NIDC*(IANT-1))) THEN
            YESSOU(SID + NIDC*(IANT-1)) = F
            NOBSSO(IANT) = NOBSSO(IANT) + 1
            END IF

         SUMANT(IANT) = SUMANT(IANT) + VALCUR
         SSQANT(IANT) = SSQANT(IANT) + VALCUR*VALCUR
C                                       Prepare result vector R(NFIT)
C                                       and matrix MATR(NFIT*NFIT)
C                                       for routine LEASQR
C  --------------------------------------------------------------------
C I    Each measurement (the SN table row for IANT) is fitted as:      |
C |                                                                    |
C |       VALUE(IANT) = FIT(1)*COEF(1) + FIT(2)*COEF(2) +              |
C |                     FIT(3)*COEF(3) + FIT(4)*COEF(4)                |
C |     COEF(1)=1,  COEF(2)=DRA,  COEF(3)=DDEC,  COEF(4)=DTIM          |
C  --------------------------------------------------------------------
C
C                                       TEST model!
C         VALCUR = PHAS0(IANT)*COEF(3) + SLRA(IANT)*COEF(1) +
C     *      SLDEC(IANT)*COEF(2)
C         VALCUR = PHAS0(IANT)*COEF(3) + SLRA(IANT)*COEF(1) +
C     *      SLDEC(IANT)*COEF(2) + SLTIME(IANT)*COEF(4)
C-------------------------------
C                                       calculate matrix MATR
         DO 320 IC = 1, NFITA
C                                       IFIT,KFIT are column number
            IFIT = IC + NFITA*(IANT-1)
            R(IFIT) = R(IFIT) + VALCUR * COEF(IC)
            DO 310 KC = 1, NFITA
C                                       LFIT,MFIT are row number
               LFIT = KC + NFITA*(IANT-1)
C                                       LIFIT number of MATR element
C                                       counting starts along column
               LIFIT = LFIT + (IFIT-1)*NFIT
C
               MATR(LIFIT) = MATR(LIFIT) + COEF(IC)*COEF(KC)
  310          CONTINUE
  320       CONTINUE

  500    CONTINUE
  520 CONTINUE
C
      IF (DOUTFI) THEN
C                                       close the OUTFILE
         CALL ZTXCLS (LUNPR, PFIND, IERR)
         END IF
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IERR)
C
      IF (SNDELE .AND. ICODE.EQ.1)  THEN
C                                       delete the SN table with solved
C                                       two pi ambiguity if required
C
C                                       Build file name.
         CALL ZPHFIL ('SN', DISKIN, CNOIN, SNOUTP, IGFILE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1670)
            CALL MSGWRT (8)
            GO TO 999
            END IF
         CALL ZDESTR (DISKIN, IGFILE, IERR)
C
         CALL DELEXT ('SN', DISKIN, CNOIN, 'WRWR',
     *      BUFF3, BUFF4, SNOUTP, IERR)
         WRITE (MSGTXT,1700) SNOUTP
         CALL MSGWRT (8)
         IF (DOSN) THEN
C                                       delete the output SN table also
C
C                                       Build file name.
            CALL ZPHFIL ('SN', DISKIN, CNOIN, SNOUT, IGFILE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1670)
               CALL MSGWRT (8)
               GO TO 999
               END IF
            CALL ZDESTR (DISKIN, IGFILE, IERR)
C
            CALL DELEXT ('SN', DISKIN, CNOIN, 'WRWR',
     *         BUFF3, BUFF4, SNOUT, IERR)
            WRITE (MSGTXT,1700) SNOUT
            CALL MSGWRT (8)
            END IF
         END IF
C
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1400) IERR
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('ANTENNAs do not include the reference ant.', I3)
 1050 FORMAT ('Reference antenna at row ', I5, ' is different')
 1100 FORMAT ('ERROR ',I5,' FINDING SN TABLE COLUMNS')
 1200 FORMAT ('TABSRT ERROR ',I5,' SORTING input SN TABLE')
 1250 FORMAT ('TABCOP ERROR ',I5,' COPIING',I3,' TO',I3,' SN TABLE')
 1300 FORMAT ( A1, 3I3, F5.1, I3, 3X, I3, 1X, F6.2,2X,F6.2,3X,F6.2,
     *            3X, F6.2, 1X, F6.2)
 1350 FORMAT ( A1, 3I3, F5.1, I3, 3X, I3, 1X, F6.2,2X,F6.3,3X,F6.3,
     *            3X, F6.3, 1X, F6.3)
 1400 FORMAT ('TABIO: ERROR = ', I3)
 1500 FORMAT ('    D:H:M:S', 5X, 'ANT',2X,'NOBS',3X,'deg',2X,
     *      'deg/deg',2X,'deg/deg',2X,'deg/min', 2X, 'rms')
 1600 FORMAT ('    D:H:M:S', 5X, 'ANT',2X,'NOBS',3X,'mm',3X,'mm/deg',
     *      3X,'mm/deg',3X,'mm/min', 2X, 'rms')
 1650 FORMAT ('!! Intermediate SN table ', I3, ' created !!')
 1670 FORMAT ('COULD NOT BUILD SN FILE NAME')
 1700 FORMAT ('!! Intermediate SN table ', I3, ' deleted !!')
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
C
      SUBROUTINE OUTCL (IERR)
C-----------------------------------------------------------------------
C   OUTCL makes the corrections of the CL table No high+1
C   Output: IERR  I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER MAXLIN
      PARAMETER (MAXLIN=10000)
      INTEGER   LUN, IRCODE, THSOU, ANT, IANT, SID,
     *   ICLRNO, NUMREC
      INTEGER   BUFFCL(1024)
      DOUBLE PRECISION   TCLT
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'ATMCA.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN /27/
C-----------------------------------------------------------------------
C                                       Open CL table
      NUMPOL = 1
      IF (CATBLK(KINAX+JLOCS).GT.1) NUMPOL = 2
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
C                                       Reformat table?
      IF (FIRSCL) THEN
         IF (NFITA .EQ. 1) THEN
            MSGTXT = '-----------CL table outputs warnings:-----------'
            CALL MSGWRT (8)
            END IF
         CALL CLREFM (DISKIN, CNOIN, CLUSE, CATBLK, LUN, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL CALINI ('WRIT', BUFFCL, DISKIN, CNOIN, CLUSE, CATBLK,
     *      LUN, ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF,
     *      NTERM, GMMOD, IERR)
         IF (IERR.NE.0) GO TO 999
         FIRSCL = .FALSE.
         FIXCL = 0
         END IF

C                                       Get number of records
      NUMREC = BUFFCL(5)
      IF (NUMREC.LE.0) GO TO 999
      IRCODE = 0
C
      ICLRNO = KCLRNO - 1
  500    CONTINUE
C
         ICLRNO = ICLRNO + 1
C
         IF (ICLRNO .GT. NUMREC) GO TO 520
         CALL TABIO ('READ', IRCODE, ICLRNO, CLRECR, BUFFCL, IERR)
C
         IF (IERR.GT.0) GO TO 900
         IF (IERR.LT.0) GO TO 500
C                                       Check data

C                                       Subarray
         IF ((CLRECI(SUBCL).NE.SUBA) .AND. (CLRECI(SUBCL).GT.0))
     *      GO TO 500
C                                       Freq id
         IF ((CLRECI(FRQCL).NE.FREQID) .AND. (CLRECI(FRQCL).GT.0) .AND.
     *      (FREQID.GT.0)) GO TO 500
C                                       Check source
         THSOU = CLRECI(SOUCL)
C                                       sources selection
         DO 10 SID = 1, NIDS
            IF (IDS(SID).EQ.THSOU) GO TO 20
   10       CONTINUE
         GO TO 500
   20    CONTINUE
C
C                                       Check antenna
         ANT = CLRECI(ANTCL)
         DO 30 IANT = 1, NSTNS
            IF (TELNO(IANT).EQ. ANT) GO TO 40
   30       CONTINUE
         GO TO 500
   40    CONTINUE

C                                       Time:
         IF (CLRECD(TIMCL).LE.TFINIS .OR. ENDSN) THEN
C                                       Correct record.
            FIXCL = FIXCL + 1
C                                       Correction of the CL row
C                                       based on PHAS0 and SLTIME
C
C                                       time at the CL table line,
C                                       in days
            TCLT = CLRECD(TIMCL)
C                                       calculate the corrections to
C                                       the CL table
            CALL ATMOCL (TCLT, IANT, SID, IERR)
            IF (IERR.NE.0) GO TO 500
C                                       Rewrite record
            IF (DOCL) CALL TABIO ('WRIT', IRCODE, ICLRNO, CLRECR, BUFFCL
     *         ,IERR)
            IF (IERR.GT.0) GO TO 900
            GO TO 500
         ELSE
            KCLRNO = ICLRNO
            GO TO 999
            END IF
C
  520 CONTINUE
C
      NUMHIS= NUMHIS + 1
      WRITE (HISCRD(NUMHIS),1100) FIXCL
      WRITE (MSGTXT,1200) FIXCL, CLUSE
      CALL MSGWRT (6)
C                                       Update GMMOD
C      IF (ABS (GMMOD-1.0).LE.1.0E-5) GO TO 600
C      CALL TABKEY ('WRIT', KEYWRD, 1, BUFFCL, 1, GMMOD, 2, IERR)
C                                       Close CL table.
C!!!!!!!!!!!!!!change LOOP for ICRNO
C      CALL TABIO ('CLOS', IRCODE, LOOP, CLRECR, BUFFCL, IERR)
      CALL TABIO ('CLOS', IRCODE, ICLRNO, CLRECR, BUFFCL, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1300) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (' / ',I6,' Records modified')
 1200 FORMAT (I6,' records of the CL table ', I2, ' modified')
 1300 FORMAT ('TABIO ERROR',I3,' CORRECTING CL TABLE')
      END
C
C
      SUBROUTINE OUTSN (IERR)
C-----------------------------------------------------------------------
C   OUTSN makes the corrections of the output SN  table No high+1
C   Output: IERR  I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LUN, THSOU, ANT, IANT, SID,
     *   ISNRNO, NUMREC, LANT
      INTEGER   BUFFSN(1024)
      INTEGER   NUMNOD, SOUN, ISN, SUBOUT, FREQN, MODENO
C
      INTEGER   NKEY, LOCS, KEYTYP
C      CHARACTER KEYW*8
      DOUBLE PRECISION   TCLT
C
C(NOT)TEMPORALLY IT WAS PCLTAB
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      INCLUDE 'ATMCA.INC'
C
      REAL      RANOD(25), DECNOD(25), REAL(2,MAXIF), IMAG(2,MAXIF),
     *   DELAY(2,MAXIF), DISP(2), DDISP(2),
     *   RATE(2,MAXIF), WEIGHT(2,MAXIF), MBDELY(2), TIMINT, IFRM
      DOUBLE PRECISION CTIM
      LOGICAL   ISAPL
      INTEGER   REFA(2,MAXIF)

      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN /28/
C      DATA KEYW /'APPLIED '/
C-----------------------------------------------------------------------
      NUMPOL = 1
      IF (CATBLK(KINAX+JLOCS).GT.1) NUMPOL = 2
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
      IF (FIRSSN) THEN
         IF (NFITA .EQ. 1) THEN
            MSGTXT = '-----------SN table outputs warnings:-----------'
            CALL MSGWRT (8)
            END IF
C                                       Open output SN table
         CALL SNINI ('WRIT', BUFFSN, DISKIN, CNOIN, SNOUT, CATBLK, LUN,
     *      ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *      GMMOD, RANOD, DECNOD, ISAPL, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Mark output SN table APPLIED.
C                                       Having spoken with Eric it
C                                       is decided not changing
C                                       'APPLIED' to 'TRUE'
C                                       because the output SN table
C                                       is atmosphere residual and can
C                                       be applied .LK. 06/10/04
               NKEY = 1
               LOCS = 1
               KEYTYP = 5
C                                       Rewrite
C               CALL TABKEY ('WRIT', KEYW, NKEY, BUFFSN, LOCS, T, KEYTYP,
C     *            IERR)
C
         FIRSSN = .FALSE.
         FIXSN = 0
         END IF
C                                       Get number of records
      NUMREC = BUFFSN(5)
      IF (NUMREC.LE.0) GO TO 999
C
      ISNRNO = KSNRNO - 1
  500    CONTINUE
C
         ISNRNO = ISNRNO + 1
C
         IF (ISNRNO .GT. NUMREC) GO TO 520
C                                       read SN table
         ISN = ISNRNO
         CALL TABSN ('READ', BUFFSN, ISN, SNKOLS, SNNUMV, NSS, CTIM,
     *      TIMINT, SOUN, LANT, SUBOUT, FREQN, IFRM, MODENO, MBDELY,
     *      DISP, DDISP, REAL, IMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C
         IF (IERR.GT.0) GO TO 900
         IF (IERR.LT.0) GO TO 500
C                                       Check data
C
C                                       Subarray
         IF ((SUBOUT.NE.SUBA) .AND. (SUBOUT.GT.0))
     *      GO TO 500
C                                       Freq id
         IF ((FREQN.NE.FREQID) .AND. (FREQN.GT.0) .AND.
     *      (FREQID.GT.0)) GO TO 500
C                                       Check source
         THSOU = SOUN
C                                       sources selection of the list
C                                       given in CALSOUR
         DO 10 SID = 1, NIDC
            IF (IDC(SID).EQ.THSOU) GO TO 20
   10       CONTINUE
         GO TO 500
   20    CONTINUE
C
C                                       Check antenna
         ANT = LANT
         DO 30 IANT = 1, NSTNS
            IF (TELNO(IANT).EQ. ANT) GO TO 40
   30       CONTINUE
         GO TO 500
   40    CONTINUE
C                                       Time:
         IF (CTIM.LE.TFINIS .OR. ENDSN) THEN
C                                       Correct record.
            FIXSN = FIXSN + 1
            TCLT = CTIM
            CALL ATMOSN (TCLT, IANT, SID, MBDELY, REAL, IMAG, DELAY,
     *         RATE, IERR)
            IF (IERR.NE.0) GO TO 500
C                                       Rewrite record
            ISN = ISNRNO
            CALL TABSN ('WRIT', BUFFSN, ISN, SNKOLS, SNNUMV, NSS, CTIM,
     *         TIMINT, SOUN, LANT, SUBOUT, FREQN, IFRM, MODENO, MBDELY,
     *         DISP, DDISP, REAL, IMAG, DELAY, RATE, WEIGHT, REFA, IERR)
            IF (IERR.GT.0) GO TO 900
            GO TO 500
         ELSE
            KSNRNO = ISNRNO
            GO TO 999
            END IF
C
  520 CONTINUE
C
      NUMHIS= NUMHIS + 1
      WRITE (HISCRD(NUMHIS),1100) FIXSN
      WRITE (MSGTXT,1200) FIXSN, SNOUT
      CALL MSGWRT (6)

C                                       Close SN table.
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFSN, BUFFSN, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1300) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (' / ',I6,' Records modified')
 1200 FORMAT (I6,' records of the output SN table ', I2, ' modified')
 1300 FORMAT ('TABIO ERROR',I3,' CORRECTING SN TABLE')
      END
C
      SUBROUTINE ATMOCL (TCLT, IANT, SID, IERR)
C-----------------------------------------------------------------------
C   Routine to apply atmospheric  corrections to the
C   given CL table row on the basis of the fitted linear representation
C   of the atmosphere at the vicinity of the calibrators
C   INPUT:
C      TCLT      D    Time at the CL table
C      IANT      I    antenna number as it selected
C       SID      I    source  number as it selected
C   Control info from common:
C      ISTOK     I    Polarization to correct, 1=first, 2=second,
C                     0 = both
C   Output:
C      IERR      I    Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   IANT, SID
      INTEGER   I, ITEMP, SIDK
      REAL      XT, YT, PDLY, DPDLY, CFAC, SFAC, FQFAC, DELMM0, SLRA,
     *   SLDEC, SLTIME, DELMM, RTARRE, RCALRE, COSCTA, TARCOR, DVALS,
     *   PROCTA, ANGL, WEIGH

      REAL      ELEV, COSZ, DELEV, DELEVR, ELEREF, HL, SLANT, SLREF
      REAL      CTIMR
      DOUBLE PRECISION FREQS, TCLT, SIDT
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'ATMCA.INC'
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
      FREQS = FREQ + FREQO(BIF)
C                                       REAL time of the CL table row
      CTIMR = TCLT
C
      IF (NFITA .EQ. 1) THEN
         DELMM = 0
         WEIGH = 0
C                                       distance target - reference
         RTARRE = SQRT (DRAS(SID)*DRAS(SID) +
     *                  DDECS(SID)*DDECS(SID))
C                                       Do not correct if the source
C                                       is the reference calibrator
         IF (RTARRE .EQ. 0) GO TO 250
         DO 200 SIDK = 2, NIDC
C                                       distance calibrator - reference
            RCALRE = SQRT (DRAC(SIDK)*DRAC(SIDK) +
     *                     DDECC(SIDK)*DDECC(SIDK))
C                                       difference of phase/delay
C                                       calibrator - reference
            DVALS = VALSAN(SIDK + NIDC*(IANT-1)) -
     *              VALSAN(1 + NIDC*(IANT-1))
C                                       COS [cal-ref, tar-ref]
            COSCTA = (DRAC(SIDK)*DRAS(SID) +
     *                DDECC(SIDK)*DDECS(SID)) / RTARRE / RCALRE

C                                       projection [cal-ref]
C                                       on [tar-ref]
C                                       PROCTA includes the sign!!!
            PROCTA = COSCTA * RCALRE
C
            IF (ABS(COSCTA) .GT. 0.71) THEN
C                                       use this calibrator, if
C                                       angle (tag-ref, cal-ref)
C                                       .LE. 45 degrees
               TARCOR = DVALS / PROCTA * RTARRE
C                                       weight the correction by the
C                                       projection of the distance C-R
               DELMM = DELMM + TARCOR * ABS(PROCTA)
               WEIGH = WEIGH + ABS(PROCTA)
            ELSE
C                                       type the warning if the given
C                                       calibrator is not used for the
C                                       given target source (SID)
               IF (FSOUCL(SID)) THEN
                  ANGL = ACOS(ABS(COSCTA)) * RAD2DG
                  WRITE (MSGTXT,1200) SOUS(SID)
                  CALL MSGWRT (8)
                  WRITE (MSGTXT,1300) SOUC(1), SOUC(SIDK)
                  CALL MSGWRT (8)
                  WRITE (MSGTXT,1400) ANGL
                  CALL MSGWRT (8)
                  WRITE (MSGTXT,1500)
                  CALL MSGWRT (8)
                  END IF
               TARCOR = 0
               END IF
  200       CONTINUE
         IF (FSOUCL(SID)) THEN
            MSGTXT = ' '
            CALL MSGWRT (8)
            END IF
C                                       prevent warning for a next
C                                       appearence of the source(SID)
         FSOUCL(SID) = .FALSE.
         IF ( WEIGH .GT. 0) DELMM = DELMM / WEIGH
C                                       add the phase/delay of
C                                       the reference calibrator
         DELMM = DELMM + VALSAN(1 + NIDC*(IANT-1))
  250    CONTINUE
C
      ELSE
         IF (DOELRE) THEN
C                                       fit gradients for the ANT and
C                                       REF along elevation
            SLANT =  SOLANT(1+NFITA*(IANT-1))
            SLREF =  SOLANT(2+NFITA*(IANT-1))
C                                       calculate elevation of
C                                       the CL row antenna for
C                                       the CL row source
            HL = TWOPI*SIDT*TCLT + GSTIAT + STNLON(IANT) - RAS(SID)
            COSZ = DCOS(STNLAT(IANT))*DCOS(DECS(SID))*COS(HL) +
     *         DSIN(STNLAT(IANT))*DSIN(DECS(SID))
C                                       source is under horizon
            IF (COSZ.LT.0.0) GO TO 999
            ELEV = PI/2.0 - ACOS(COSZ)

C                                       calculate elevation of
C                                       the CL row antenna for
C                                       the reference calibrator
C                                       (first in the list)
            HL = TWOPI*SIDT*TCLT + GSTIAT + STNLON(IANT) - RAC(1)
            COSZ = DCOS(STNLAT(IANT))*DCOS(DECC(1))*COS(HL) +
     *         DSIN(STNLAT(IANT))*DSIN(DECC(1))
C                                       source is under horizon
            IF (COSZ.LT.0.0) GO TO 999
C                                       difference in elevation
C                                       for the antenna IANT:
C                                       given source - ref. source
            DELEV = ELEV -(PI/2.0 - ACOS(COSZ))
C                                       calculate elevation of the
C                                       reference antenna
C                                       for the CL row source
            HL = TWOPI*SIDT*TCLT + GSTIAT + STNLON(RANT) - RAS(SID)
            COSZ = DCOS(STNLAT(RANT))*DCOS(DECS(SID))*COS(HL) +
     *         DSIN(STNLAT(RANT))*DSIN(DECS(SID))
C                                       source is under horizon
            IF (COSZ.LT.0.0) GO TO 999
            ELEREF = PI/2.0 - ACOS(COSZ)
C                                       calculate elevation of
C                                       the refer. antenna for
C                                       the reference calibrator
C                                       (first in the list)
            HL = TWOPI*SIDT*TCLT + GSTIAT + STNLON(RANT) - RAC(1)
            COSZ = DCOS(STNLAT(RANT))*DCOS(DECC(1))*COS(HL) +
     *      DSIN(STNLAT(RANT))*DSIN(DECC(1))
C                                       source is under horizon
            IF (COSZ.LT.0.0) GO TO 999
C                                       difference in elevation
C                                       for the refer. antenna :
C                                       given source - ref. source
            DELEVR = ELEREF -(PI/2.0 - ACOS(COSZ))
C
            DELMM = SLANT*DELEV - SLREF*DELEVR
C
         ELSE
            DELMM0 = 0
            IF (NFITA .GE. 3) DELMM0 = SOLANT(3+NFITA*(IANT-1))
            SLRA =   SOLANT(1+NFITA*(IANT-1))
            SLDEC =  SOLANT(2+NFITA*(IANT-1))
            SLTIME = 0
            IF (NFITA .GE. 4) SLTIME = SOLANT(4+NFITA*(IANT-1))
C                                       delays of the source
C                                       (calibrators)
C                                       in mm
            DELMM =  DELMM0 +
     *         SLRA   * DRAS(SID) +
     *         SLDEC  * DDECS(SID) +
     *            SLTIME * (TCLT - TTARG)
            END IF
         END IF
C                                       delays of the source
C                                       in second
      PDLY = DELMM / 1000 / VELITE
      DPDLY = 0
C
      IF (.NOT. DOCL) GO TO 999


C                                       Atmospheric group delay
      IF (CLRECR(ATMCL).NE.FBLANK) CLRECR(ATMCL) = CLRECR(ATMCL)
     *      - PDLY
C                                       multiband delay
      IF (CLRECR(MBD1CL).NE.FBLANK) CLRECR(MBD1CL) = CLRECR(MBD1CL)
     *      + PDLY
      IF (ISTOK.EQ.2) GO TO 650
C
      DO 600 I = BIF,EIF
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
     *      CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + PDLY
C                                       Phase rate
         IF (CLRECR(RA1CL+I-1).NE.FBLANK)
     *      CLRECR(RA1CL+I-1) = CLRECR(RA1CL+I-1) + DPDLY
 600     CONTINUE
C
 650     IF (ISTOK.EQ.1 .AND. NS.EQ.1) GO TO 999
C                                       multi delay
      IF (CLRECR(MBD2CL).NE.FBLANK) CLRECR(MBD2CL) = CLRECR(MBD2CL)
     *      + PDLY
      DO 700 I = BIF,EIF
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
     *      CLRECR(DE2CL+I-1) = CLRECR(DE2CL+I-1) + PDLY
C                                       Phase rate
         IF (CLRECR(RA2CL+I-1).NE.FBLANK)
     *      CLRECR(RA2CL+I-1) = CLRECR(RA2CL+I-1) + DPDLY

 700     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('| The angle Target ', A16, 27(' '), '|')
 1300 FORMAT ('| to Reference ', A16, ' to Calibrator ',
     *   A16, '|')
 1400 FORMAT ('| is ', F4.1,
     *   ' degrees and exceeds the threshold 30 degrees ',
     *   7(' '), '|')
 1500 FORMAT ('| So the CALIBRATOR is not used with the TARGET',
     *   15(' '),'|')
      END
C
      SUBROUTINE ATMOSN (TCLT, IANT, SID, MBDELY, REAL, IMAG, DELAY,
     *         RATE, IERR)
C-----------------------------------------------------------------------
C   Routine to apply atmospheric  corrections to the
C   given SN table row on the basis of the fitted linear representation
C   of the atmosphere at the vicinity of the calibrators
C   INPUT:
C      TCLT      D    Time at the SN table row
C      IANT      I    antenna number as it selected
C       SID      I    source  number as it selected
C   Control info from common:
C      ISTOK     I    Polarization to correct, 1=first, 2=second,
C                     0 = both
C   Output:
C      IERR      I    Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   IANT, SID
      INTEGER   I, ITEMP, SIDK
      REAL      XT, YT, PDLY, DPDLY, CFAC, SFAC, FQFAC, DELMM0, SLRA,
     *   SLDEC, SLTIME, DELMM, RTARRE, RCALRE, COSCTA, TARCOR, DVALS,
     *   PROCTA, ANGL, WEIGH

      REAL      ELEV, COSZ, DELEV, DELEVR, ELEREF, HL, SLANT, SLREF
      REAL      CTIMR
      DOUBLE PRECISION FREQS, TCLT, SIDT
CTEMPORALLY(NOT) IT was PCLTAB?
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      INCLUDE 'ATMCA.INC'
C
      REAL     REAL(2,MAXIF), IMAG(2,MAXIF), DELAY(2,MAXIF),
     *   RATE(2,MAXIF), MBDELY(2)
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
C
      IERR = 0
      FREQS = FREQ + FREQO(BIF)
C                                       REAL time of the CL table row
      CTIMR = TCLT
C                                       begin of the (APARM(1) =1,5)
      IF (NFITA .EQ. 1) THEN
         DELMM = 0
         WEIGH = 0
C                                       distance selected calibrator
C                                       - reference calibrator
         RTARRE = SQRT (DRAC(SID)*DRAC(SID) +
     *                  DDECC(SID)*DDECC(SID))
C                                       Do not correct the reference
C                                       calibrator (the first in the
C                                       calibrator list)
         IF (RTARRE .EQ. 0) GO TO 250
         DO 200 SIDK = 2, NIDC
C                                       distance calibrator - reference
            RCALRE = SQRT (DRAC(SIDK)*DRAC(SIDK) +
     *                     DDECC(SIDK)*DDECC(SIDK))
C                                       difference of phase/delay
C                                       calibrator - reference
            DVALS = VALSAN(SIDK + NIDC*(IANT-1)) -
     *              VALSAN(1 + NIDC*(IANT-1))
C                                       COS [cal-ref, source-ref]
            COSCTA = (DRAC(SIDK)*DRAC(SID) +
     *                DDECC(SIDK)*DDECC(SID)) / RTARRE / RCALRE

C                                       projection [cal-ref]
C                                       on [source-ref]
C                                       PROCTA includes the sign!!!
            PROCTA = COSCTA * RCALRE
C
            IF (ABS(COSCTA) .GT. 0.87) THEN
C                                       use this calibrator, if
C                                       angle (source-ref, cal-ref)
C                                       .LE. 30 degrees
               TARCOR = DVALS / PROCTA * RTARRE
C                                       weight the correction by the
C                                       projection of the distance C-R
               DELMM = DELMM + TARCOR * ABS(PROCTA)
               WEIGH = WEIGH + ABS(PROCTA)
            ELSE
C                                       type the warning if the given
C                                       calibrator is not used for the
C                                       given  source (SID)
               IF (FSOUSN(SID)) THEN
C               IF (FSOUSN(SID) .AND. .NOT.DOCL) THEN
                  ANGL = ACOS(ABS(COSCTA)) * RAD2DG
                  WRITE (MSGTXT,1200) SOUC(SID)
                  CALL MSGWRT (8)
                  WRITE (MSGTXT,1300) SOUC(1), SOUC(SIDK)
                  CALL MSGWRT (8)
                  WRITE (MSGTXT,1400) ANGL
                  CALL MSGWRT (8)
                  WRITE (MSGTXT,1500)
                  CALL MSGWRT (8)
                  END IF
               TARCOR = 0
               END IF
  200       CONTINUE
         IF (FSOUSN(SID)) THEN
            MSGTXT = ' '
            CALL MSGWRT (8)
            END IF
C                                       prevent warning for a next
C                                       appearence of the source(SID)
         FSOUSN(SID) = .FALSE.
         IF ( WEIGH .GT. 0) DELMM = DELMM / WEIGH
C                                       add the phase/delay of
C                                       the reference calibrator
         DELMM = DELMM + VALSAN(1 + NIDC*(IANT-1))
  250    CONTINUE
C
      ELSE
         IF (DOELRE) THEN
C                                       fit gradients for the ANT and
C                                       REF along elevation
            SLANT =  SOLANT(1+NFITA*(IANT-1))
            SLREF =  SOLANT(2+NFITA*(IANT-1))
C                                       calculate elevation of
C                                       the SN row antenna for
C                                       the SN row source
            HL = TWOPI*SIDT*TCLT + GSTIAT + STNLON(IANT) - RAC(SID)
            COSZ = DCOS(STNLAT(IANT))*DCOS(DECC(SID))*COS(HL) +
     *         DSIN(STNLAT(IANT))*DSIN(DECC(SID))
C                                       source is under horizon
            IF (COSZ.LT.0.0) GO TO 999
            ELEV = PI/2.0 - ACOS(COSZ)

C                                       calculate elevation of
C                                       the SN row antenna for
C                                       the reference calibrator
C                                       (first in the list)
            HL = TWOPI*SIDT*TCLT + GSTIAT + STNLON(IANT) - RAC(1)
            COSZ = DCOS(STNLAT(IANT))*DCOS(DECC(1))*COS(HL) +
     *         DSIN(STNLAT(IANT))*DSIN(DECC(1))
C                                       source is under horizon
            IF (COSZ.LT.0.0) GO TO 999
C                                       difference in elevation
C                                       for the antenna IANT:
C                                       given source - ref. source
            DELEV = ELEV -(PI/2.0 - ACOS(COSZ))
C                                       calculate elevation of the
C                                       reference antenna
C                                       for the SN row source
            HL = TWOPI*SIDT*TCLT + GSTIAT + STNLON(RANT) - RAC(SID)
            COSZ = DCOS(STNLAT(RANT))*DCOS(DECC(SID))*COS(HL) +
     *         DSIN(STNLAT(RANT))*DSIN(DECC(SID))
C                                       source is under horizon
            IF (COSZ.LT.0.0) GO TO 999
            ELEREF = PI/2.0 - ACOS(COSZ)
C                                       calculate elevation of
C                                       the refer. antenna for
C                                       the reference calibrator
C                                       (first in the list)
            HL = TWOPI*SIDT*TCLT + GSTIAT + STNLON(RANT) - RAC(1)
            COSZ = DCOS(STNLAT(RANT))*DCOS(DECC(1))*COS(HL) +
     *      DSIN(STNLAT(RANT))*DSIN(DECC(1))
C                                       source is under horizon
            IF (COSZ.LT.0.0) GO TO 999
C                                       difference in elevation
C                                       for the refer. antenna :
C                                       given source - ref. source
            DELEVR = ELEREF -(PI/2.0 - ACOS(COSZ))
C
            DELMM = SLANT*DELEV - SLREF*DELEVR
C
         ELSE
            DELMM0 = 0
            IF (NFITA .GE. 3) DELMM0 = SOLANT(3+NFITA*(IANT-1))
            SLRA =   SOLANT(1+NFITA*(IANT-1))
            SLDEC =  SOLANT(2+NFITA*(IANT-1))
            SLTIME = 0
C                                       end  of the (APARM(1) =1,5)
            IF (NFITA .GE. 4) SLTIME = SOLANT(4+NFITA*(IANT-1))
C                                       delays of the source
C                                       in mm
            DELMM =  DELMM0 +
     *         SLRA   * DRAC(SID) +
     *         SLDEC  * DDECC(SID) +
     *         SLTIME * (TCLT - TTARG)
            END IF
         END IF
C                                       correcting delay of the source
C                                       in second
      PDLY = DELMM / 1000 / VELITE
      DPDLY = 0
C                                       multiband delay of the SNOUT
C                                       minus the correcting delay
      IF (MBDELY(1).NE.FBLANK) MBDELY(1) = MBDELY(1) - PDLY
      IF (ISTOK.EQ.2) GO TO 650
C
      DO 600 I = BIF,EIF
C                                       look in ICODE
         IF (ICODE .EQ. 1) THEN
C                                       input SN table has the phase
C                                       (degrees) in the column REAL
            REAL(1,I) = REAL(1,I) - DELMM / LAMBDA(I) * 360.0
         ELSE
C                                       input SN table has the phase
C                                       in the columns REAL, IMAGE
            FQFAC = (FREQS+FRQOFF(I)) * (-PDLY)
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase correction considering
C                                       -PDLY
            XT = REAL(1,I)
            YT = IMAG(1,I)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               REAL(1,I) = XT * CFAC - YT * SFAC
               IMAG(1,I) = XT * SFAC + YT * CFAC
               END IF
            END IF
C                                       Delay
         IF (DELAY(1,I).NE.FBLANK) DELAY(1,I) = DELAY(1,I) - PDLY
C                                       Phase rate
         IF (RATE(1,I).NE.FBLANK) RATE(1,I) = RATE(1,I) - DPDLY
 600     CONTINUE
C
 650     IF (ISTOK.EQ.1 .AND. NS.EQ.1) GO TO 999
C                                       correction of the second
C                                       polarization
C
C                                       multi delay
      IF (MBDELY(2).NE.FBLANK) MBDELY(2) = MBDELY(2) - PDLY
      DO 700 I = BIF,EIF
C                                       look in ICODE
         IF (ICODE .EQ. 1) THEN
C                                       input SN table has the phase
C                                       (degrees) in the column REAL
            REAL(2,I) = REAL(2,I) - DELMM / LAMBDA(I) * 360.0
         ELSE
C                                       input SN table has the phase
C                                       in the columns REAL, IMAGE
            FQFAC = (FREQS+FRQOFF(I)) * (-PDLY)
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase correction considering
C                                       -PDLY
            XT = REAL(2,I)
            YT = IMAG(2,I)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               REAL(2,I) = XT * CFAC - YT * SFAC
               IMAG(2,I) = XT * SFAC + YT * CFAC
               END IF
            END IF
C                                       Delay
         IF (DELAY(2,I).NE.FBLANK) DELAY(2,I) = DELAY(2,I) - PDLY
C                                       Phase rate
         IF (RATE(2,I).NE.FBLANK) RATE(2,I) = RATE(2,I) - DPDLY
 700     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('| The angle Source ', A16, 27(' '), '|')
 1300 FORMAT ('| to Reference ', A16, ' to Calibrator ',
     *   A16, '|')
 1400 FORMAT ('| is ', F4.1,
     *   ' degrees and exceeds the threshold 30 degrees ',
     *   7(' '), '|')
 1500 FORMAT ('| So the CALIBRATOR is not used with the SOURCE',
     *   15(' '),'|')
      END
C
      SUBROUTINE ATMHIS
C-----------------------------------------------------------------------
C   ATMHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12,  HILINE*72, LABEL*8
      INTEGER   LUN, IERR, TIM(3), DATE(3), I
      INTEGER   BUFFHI(1024)
      LOGICAL   T
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'ATMCA.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN /28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN, DISKIN, FCNO(NCFILE), BUFFHI, IERR)
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
      CALL HIADD (LUN, HILINE, BUFFHI, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history.
      IF (NUMHIS.LE.0) GO TO 200
         WRITE (LABEL,1020) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFFHI, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
C                                       Close HI file
 200   CALL HICLOS (LUN, T, BUFFHI, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ATMHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6, 'RELEASE =''',A7,' ''  /********* Start ',
     *   A12, 2X, A8)
 1020 FORMAT (A6)
      END
      SUBROUTINE DLESQR (NP, N, SUM, SSQ, R, M, X, VX, SSQRES,
     *   VARRES, VARY, FIT, IERR)
C-----------------------------------------------------------------------
C     LEASQR does the matrix inversion and other necessary tasks
C     involved in a least squares analysis.
C
C     Given:
C          NP        I     Number of parameters.
C          N         D     The number of observations.
C          SUM       D     Error sum.
C          SSQ       D     Square error sum.
C          R(NP)     D     Results vector.
C
C     Given and returned:
C          M(NP,NP)  D     On input, the upper triangular part contains
C                          the design matrix.  This is not changed.
C                          On output, the lower triangular part contains
C                          the covariance matrix.  Diagonal elements of
C                          the covariance matrix are stored in VX.
C
C     Returned:
C          X(NP)     D     Vector holding the least squares solution.
C          VX(NP)    D     Variance of the best fit parameters.
C          SSQRES    D     Sum of squares of the residuals.
C          VARRES    D     Variance of the residuals.
C          VARY      D     Variance of the error values.
C          FIT       D     Goodness of fit parameter, lies between 0
C                          and 1.
C          IERR      I     Error status, 0 means successful.
C                             1 - nonspecific error return,
C                             2 - insufficient degrees of freedom.
C
C     Called:
C          none
C
C     Algorithm:
C          LU-triangular factorization with scaled partial pivoting.
C          The sub-diagonal triangular matrix contains the scaling
C          factors used at each step in the Gaussian elimination.  Row
C          interchanges are recorded in vectors MXS and SXM.
C             During forward substitution, the pivoting and Gaussian
C          elimination operations performed on matrix M are applied to
C          vector R.  Vector X holds the intermediate result.
C             On backward substitution, successive elements of the
C          solution vector, X, are calculated by substitution of the
C          preceding elements into the equations of the upper triangular
C          factorization of the design matrix.
C
C     Notes:
C       1) Strictly speaking, the design matrix will usually contain
C          rows of zeroes and therefore be singular.  This arises if no
C          observations sensitive to a particular parameter have been
C          done.
C             In practice, any such singularities are ignored and the
C          associated parameters remain undetermined.
C
C       2) The covariance matrix is the inverse of M(i,j) multiplied by
C          the variance of the residuals.  It is obtained by forward and
C          backward substitution on the columns of the unit matrix.
C
C       3) Two statement functions, C, and SC have been employed to
C          partially alleviate the problems posed by passing arrays in
C          FORTRAN.  The design/covariance matrix m(i,j) is copied into
C          the working vector s(i).  This is addressed by using C, and
C          SC in an attempt to make it look like the matrix that it
C          actually represents.
C
C       4) The maximum size problem that LEASQR can handle is set by
C          parameter MX.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1988/Sep/29. Code last modified; 1989/Nov/22.
C          Leonid Kogan modified the original version to
C          double precision; 2001/May/30.
C          This version allows number of observation to be equal
C          number of unknown parameters
C-----------------------------------------------------------------------
C     Parameter which determines the maximum size problem.
      INTEGER   MX
      PARAMETER (MX = 50)

      INTEGER   C, I, IERR, ITEMP, J, K, MXS(MX), NF, NP, PIVOT,
     *          SXM(MX)
      DOUBLE PRECISION   COLMAX, DTEMP, FIT, M(NP,NP), N, R(NP), RESIDU,
     *          RLEN, ROWMAX(MX), S(MX*MX), SC, RTEMP, SSQ, SSQRES, SUM,
     *          VARRES, VARY, VX(NP), W(MX), X(NP)

      INCLUDE 'INCS:DMSG.INC'

C     Statement functions for array manipulation, see note 3 above.
      C(I,J)  = NP*(I-1) + J
      SC(I,J) = S(C(I,J))
C-----------------------------------------------------------------------
C  Initialize.
C     Anticipate and return immediately on error.
      IERR = 1

C     Initialize arrays.
      DO 40 I = 1, NP
C        Vector which records row interchanges.
         MXS(I) = I

C        The solution and variance vectors.
         X(I)  = 0.0
         VX(I) = 0.0

C        Copy the design matrix and zero the covariance matrix.
         DO 10 J = 1, I-1
            M(I,J) = 0.0
            S(C(I,J)) = M(J,I)
 10      CONTINUE
         DO 20 J = I, NP
            S(C(I,J)) = M(I,J)
 20      CONTINUE

C        Find the maximum absolute element in each row.
         ROWMAX(I) = 0.0
         DO 30 J = 1, NP
            ROWMAX(I) = MAX(ROWMAX(I), ABS(SC(I,J)))
 30      CONTINUE
 40   CONTINUE

      VARY   = 0.0
      SSQRES = 0.0
      VARRES = 0.0
      FIT    = 0.0

C     Find the number of degrees of freedom.
      NF = N
      DO 60 I = 1, NP
         IF (ROWMAX(I).NE.0.0) THEN
            NF = NF - 1
         ELSE IF (R(I).NE.0.0) THEN
C           Any row of zeroes must extend to the results vector.
            WRITE (MSGTXT,50) I
 50         FORMAT ('LEASQR: Design matrix inconsistency in row',I4)
            CALL MSGWRT (6)
         END IF
 60   CONTINUE

C      IF (NF.LE.1) THEN
C      IF (NF.LT.1) THEN
C                                       LK, April 2004
      IF (NF.LT.0) THEN
         WRITE (MSGTXT,70)
 70      FORMAT ('LEASQR: Insufficient degrees of freedom.')
         CALL MSGWRT (6)
         IERR = 2
         RETURN
      END IF
C  Factorize the matrix.
      DO 120 K = 1, NP
C        Check for a row of zeroes.
         IF (ROWMAX(K).EQ.0.0) GO TO 120

C        A non-zero row maximum implies non-zero diagonal element.
         IF (SC(K,K).EQ.0.0) THEN
            WRITE (MSGTXT,50) MXS(K)
            CALL MSGWRT (6)
            GO TO 120
         END IF

C        Decide whether to pivot.
         COLMAX = ABS(SC(K,K))/ROWMAX(K)
         PIVOT = K
         DO 80 I = K+1, NP
            IF (ROWMAX(I).NE.0.0) THEN
               IF (ABS(SC(I,K))/ROWMAX(I).GT.COLMAX) THEN
                  COLMAX = ABS(SC(I,K))/ROWMAX(I)
                  PIVOT = I
               END IF
            END IF
 80      CONTINUE

         IF (PIVOT.GT.K) THEN
C           We must pivot, interchange the rows of the design matrix.
            DO 90 J = 1, NP
               DTEMP = SC(PIVOT,J)
               S(C(PIVOT,J)) = SC(K,J)
               S(C(K,J)) = DTEMP
 90         CONTINUE

C           Don't forget the vector of row maxima.
            DTEMP = ROWMAX(PIVOT)
            ROWMAX(PIVOT) = ROWMAX(K)
            ROWMAX(K) = DTEMP

C           Record the interchange for later use.
            ITEMP = MXS(PIVOT)
            MXS(PIVOT) = MXS(K)
            MXS(K) = ITEMP
         END IF

C        Gaussian elimination.
         DO 110 I = K+1, NP
C           Nothing to do if SC(i,k) is zero.
            IF (SC(I,K).NE.0.0) THEN
C              Save the scaling factor.
               S(C(I,K)) = SC(I,K)/SC(K,K)

C              Subtract rows.
               DO 100 J = K+1, NP
                  S(C(I,J)) = SC(I,J) - SC(I,K)*SC(K,J)
 100           CONTINUE
            END IF
 110     CONTINUE
 120  CONTINUE

C     MXS(i) records which row of M corresponds to row i of SC.
C     SXM(i) records which row of S corresponds to row i of M.
      DO 130 I = 1, NP
         SXM(MXS(I)) = I
 130  CONTINUE


C  Solve the normal equations.
      DO 150 I = 1, NP
C        Forward substitution.
         W(I) = R(MXS(I))
         DO 140 J = 1, I-1
            W(I) = W(I) - SC(I,J)*W(J)
 140     CONTINUE
 150  CONTINUE

      DO 170 I = NP, 1, -1
C        Backward substitution.
         IF (SC(I,I).NE.0.0) THEN
            DO 160 J = I+1, NP
               W(I) = W(I) - SC(I,J)*W(J)
 160        CONTINUE
            W(I) = W(I)/SC(I,I)
         END IF
         X(I) = W(I)
 170  CONTINUE

C     Check that the solution is acceptable.
      RLEN = 0.0
      RESIDU = 0.0
      DO 200 I = 1, NP
         RTEMP = 0.0
         DO 180 J = 1, I-1
            RTEMP = RTEMP + M(J,I)*X(J)
 180     CONTINUE
         DO 190 J = I, NP
            RTEMP = RTEMP + M(I,J)*X(J)
 190     CONTINUE

         RLEN = RLEN + R(I)**2
         RESIDU = RESIDU + (RTEMP - R(I))**2
 200  CONTINUE

      IF (RESIDU.GT.0.001*RLEN) THEN
         WRITE (MSGTXT,210) RESIDU/RLEN
 210     FORMAT ('LEASQR: The solution is discrepant at',E8.1)
         CALL MSGWRT (6)
         RETURN
      END IF
C  Determine goodness-of-fit estimates, and statistical errors.
      SSQRES = SSQ
      DO 220 I = 1, NP
         SSQRES = SSQRES - X(I)*R(I)
 220  CONTINUE
      IF (SSQRES.LT.0.0) SSQRES = 0.0

C      VARRES = SSQRES/NF
C                                       LK May 2004
      IF (NF .GT. 0) VARRES = SSQRES/NF
      VARY = (SSQ - SUM*SUM/N)/(N - 1.0)
      FIT = 1.0
      IF (VARY.NE.0.0) FIT = 1.0 - SSQRES/(SSQ - SUM*SUM/N)

C     Determine the covariance matrix.
      DO 280 K = 1, NP
C        Forward substitution affects only that part of W() below the
C        first non-zero entry.
         DO 230 I = 1, SXM(K)-1
            W(I) = 0.0
 230     CONTINUE
         W(SXM(K)) = 1.0
C
         DO 250 I = SXM(K)+1, NP
C           Forward substitution.
            W(I) = 0.0
            DO 240 J = SXM(K), I-1
               W(I) = W(I) - SC(I,J)*W(J)
 240        CONTINUE
 250     CONTINUE

         DO 270 I = NP, K, -1
            IF (SC(I,I).NE.0.0) THEN
C              Backward substitution.
               DO 260 J = I+1, NP
                  W(I) = W(I) - SC(I,J)*W(J)
 260           CONTINUE
               W(I) = W(I)/SC(I,I)
            END IF

            IF (I.NE.K) THEN
C              Off diagonal elements of the covariance matrix.
               M(I,K) = VARRES*W(I)
            ELSE IF (I.EQ.K) THEN
C              Diagonal elements of the covariance matrix.
               VX(K)  = VARRES*W(I)
            END IF
 270     CONTINUE
 280  CONTINUE
C  Successful completion.
      IERR = 0
      RETURN
      END
