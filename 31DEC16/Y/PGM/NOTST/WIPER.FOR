LOCAL INCLUDE 'WIPER.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   NUMPRM
      PARAMETER (NUMPRM = 20)
C
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XCALC, XSTOK,
     *   TVCATH(256)
      REAL   XSIN, XDISIN, XQUAL, XBAND, XFREQ, XFQID, XTIME(8),
     *   XANT(50), XBASE(50), XUVRA(2), XSUBA, XBCHAN, XECHAN, XNCHAV,
     *   XCHINC, XBIF, XEIF, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XFGOUT, XDOBND, XBPVER, XSMOTH(3), BPARM(10), XREFAN,
     *   XROTPA, XIMSIZ(2), XDOCEN, XDOALL, PRTLEV, XBADD(10)
      REAL      BUFF1(UVBFSS), TBEG, TFIN, XYSCL(3), XYOFF(3),
     *   RPARM(20), XYMIN(3), XYMAX(3), FINC(MAXIF), ROTATE,
     *   TVCATR(256), SCALXY(2)
      DOUBLE PRECISION FOFF(MAXIF), TVCATD(128)
      INTEGER   IAW1, IAW2, INC, SEQIN, DISKIN, LUNI, INDI, TYPEAX(2),
     *   NCH, VER, JBUFSZ, IANT(50), NANT, IBAS(50), NBAS, CNOIN,
     *   IFRQ, NFRQ, NSUBA, GRCHN, TVCHN, TVCORN(4), ISBAND(MAXIF),
     *   EXCLFQ(MAXIF,MAXFQ), CHINC, LABEL, FIXSCL, CSOU, SBUFF(512),
     *   REFANT, COUNT(3,3), FGVERI, FGVERO, TVSCR(MAXIMG), NBDCOR,
     *   MANT, MIF, TVCAT(256), NCHAN, FLGPIX, NSOU, NPOL, LCOR0,
     *   NFGWRI, BLFLAG(MAXANT,MAXANT), MBL, TTY(2), IPHASE, NCHAV,
     *   LTYPE, TVMAXX(2)
      LOGICAL   UVREV, ISUVR, MULTI, DESEL, DOTV, ONEIF, ONECHN,
     *   INVAX(2), ONEPOL, NOTCRS, ISCROS(4), SPHASE
      EQUIVALENCE (TVCAT, TVCATR, TVCATD, TVCATH)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XQUAL,
     *   XCALC, XSTOK, XBAND, XFREQ, XFQID, XTIME, XANT, XBASE, XUVRA,
     *   XSUBA, XBCHAN, XECHAN, XNCHAV, XCHINC, XBIF, XEIF, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XFGOUT, XDOBND, XBPVER,
     *   XSMOTH, BPARM, XREFAN, XROTPA, XIMSIZ, XDOCEN, XDOALL, PRTLEV,
     *   XBADD
      COMMON /CHPARM/ NAMEIN, CLAIN, XSOUR
      COMMON /BUFRS/ BUFF1, SBUFF, RPARM, JBUFSZ
      COMMON /UVPCOM/ TVCAT, FOFF, FINC, ISBAND, TBEG, TFIN, XYSCL,
     *   XYOFF, XYMIN, XYMAX, UVREV, ISUVR, MULTI, DOTV, IAW1, IAW2,
     *   INC, SEQIN, DISKIN, LUNI, INDI, TYPEAX, NCH, VER, CNOIN, IFRQ,
     *   NFRQ, NSUBA, TVCHN, GRCHN, TVCORN, EXCLFQ, CHINC, LABEL,
     *   FIXSCL, CSOU, REFANT, ROTATE, COUNT, FGVERI, FGVERO, ONEPOL,
     *   NOTCRS, ONEIF, ONECHN, TVSCR, NBDCOR, MANT, MIF, NCHAN, FLGPIX,
     *   NPOL, INVAX, LCOR0, ISCROS, NFGWRI, MBL, TTY, IPHASE, SPHASE,
     *   SCALXY, NCHAV, LTYPE, TVMAXX
      COMMON /BASSEL/ DESEL, IANT, NANT, IBAS, NBAS, NSOU, BLFLAG
LOCAL END
      PROGRAM WIPER
C-----------------------------------------------------------------------
C! WIPER plots uvdata like UVPLT, then edits them
C# EXT-appl Graphics Plot UV-util editing
C-----------------------------------------------------------------------
C;  Copyright (C) 2002-2016
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
C   WIPER plots uv data like UVPLT and then edits them.
C   Inputs:
C      INNAME                             Input UV file name (name)
C      INCLASS                            Input UV file name (class)
C      INSEQ             0.0     9999.0   Input UV file name (seq. #)
C      INDISK                             Input UV file disk unit #
C      SOURCES                            Sources to plot, ' '=>all.
C      QUAL            -10.0              Qualifier -1=>all
C      CALCODE                            Calibrator code '    '=>all
C      STOKES                             Stokes type to select.
C      SELBAND                            Bandwidth to select (kHz)
C      SELFREQ                            Frequency to select (MHz)
C      FREQID                             Freq. ID to select.
C      TIMERANG                           Time range to select
C      ANTENNAS                           Antennas to plot
C      BASELINE                           Baselines with ANTENNAS
C      UVRANGE                            UV range in kilolambda.
C      SUBARRAY          0.0     1000.0   Subarray, 0 => all
C      BCHAN             0.0     4096.0   1st spectral channel #
C      ECHAN             0.0     4096.0   Last spectral channel #
C      CHINC             0.0     4096.0   Increment in channel #
C      BIF                                Low IF number to plot
C      EIF                                Highest IF number to plot
C      DOCALIB          -1.0        2.0   If >0 calibrate data
C                                         = 2 calibrate weights
C      GAINUSE                            CAL (CL or SN) table to apply
C      DOPOL            -1.0       10.0   If >0 correct polarization.
C      BLVER                              BL table to apply.
C      FLAGVER                            Flag table version
C      DOBAND           -1.0       10.0   If >0 apply bandpass cal.
C                                         Method used depends on value
C                                         of DOBAND (see HELP file).
C      BPVER                              Bandpass table version
C      SMOOTH                             Smoothing function. See
C                                         HELP SMOOTH for details.
C      BPARM                              Control parameters
C                                         1 : X-axis type 0=>UV dist
C                                         2 : Y-axis type 0=>Ampl
C                                             1=> amplitude (Jy)
C                                             2=> phase (degrees)
C                                             3=> uv dist. (klambda)
C                                             4=> uv p.a. (deg N->E)
C                                             5=> time (IAT days)
C                                             6=> u (klambda)
C                                             7=> v (klambda)
C                                             8=> w (klambda)
C                                             9=> Re(Vis) (Jy)
C                                             10=> Im(Vis) (Jy)
C                                             11=> time (IAT hours)
C                                             12=> log(ampl)
C                                             13=> weight
C                                             14=> HA (hours)
C                                             15=> elevation (deg)
C                                             16=> parallactic angle
C                                             17=> uv dist. (klambda)
C                                                  along p.a.
C                                             18=> azimuth (deg)
C                                             19=> frequency
C                                         3 : > 0.0 => fixed scale
C                                             < 0.0 => fixed range
C                                         4 : Xmin (fixed scale)
C                                         5 : Xmax (fixed scale)
C                                         6 : Ymin (fixed scale)
C                                         7 : Ymax (fixed scale)
C                                         10: > 0 => plot auto-corr too
C      REFANT          0.0        90.0    > 0 => use REFANT for plot
C                                         types 14, 15, 16
C      ROTATE       -360.0       360.0    uv p.a. for projection
C                                         (deg N->E); type 17 only
C      BADDISK                            Disk to avoid for scratch.
C-----------------------------------------------------------------------
      INTEGER   NBL
      PARAMETER (NBL = 25)
C
      CHARACTER PRGM*6
      INTEGER   IERR, IRET, NX, NY, NWORDS, TVCORE(2), MALL(2), MBAD(2),
     *   MSOM(2), BLCORE(2)
      LONGINT   TVADDR, OFFALL, OFFBAD, OFFSOM, BLADDR
      LOGICAL   GETSCL
      INCLUDE 'WIPER.INC'
      REAL      XZY(3,MAXCIF)
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'WIPER '/
C-----------------------------------------------------------------------
C                                       Get input parameters and create
C                                       output file if nec.
      CALL WIPEIN (PRGM, NX, NY, IRET)
      IF (IRET.NE.0) GO TO 995
      FLGPIX = 0
C                                       Determine limits for all axes
C                                       if any autoscaling will be done.
      GETSCL = (FIXSCL.LE.0) .OR. (BPARM(4).GE.BPARM(5)) .OR.
     *   (BPARM(6).GE.BPARM(7))
      CALL SCAL (GETSCL, XZY, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Allocate memory
      NWORDS = (NX * NY - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, TVCORE, TVADDR, IRET)
      IF (IRET.NE.0) GO TO 995
      NWORDS = (NBL * NX * NY - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, BLCORE, BLADDR, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Plot data to image in memory
      CALL PLOTUV (NPOL, XZY, NX, NY, TVCORE(1+TVADDR), NBL,
     *   BLCORE(1+BLADDR), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Interactive editor
      CALL EDITUV (NX, NY, TVCORE(1+TVADDR), NBL, BLCORE(1+BLADDR),
     *   IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Reporting arrays
      NWORDS = (MANT * MANT * MIF * NPOL - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MALL, OFFALL,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MBAD, OFFBAD,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MSOM, OFFSOM,
     *   IRET)
      IF (IRET.EQ.0) THEN
         NWORDS = 1024 * NWORDS
         CALL FILL (NWORDS, 0, MALL(1+OFFALL))
         CALL FILL (NWORDS, 0, MBAD(1+OFFBAD))
         CALL FILL (NWORDS, 0, MSOM(1+OFFSOM))
      ELSE
         GO TO 990
         END IF
      NBDCOR = 0
C                                       Write flag table
      CALL WIPEUV (NX, NY, TVCORE(1+TVADDR), MANT, MIF, NPOL, XZY,
     *   MALL(1+OFFALL), MBAD(1+OFFBAD), MSOM(1+OFFSOM), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       report results
      IF (PRTLEV.GT.1.0) CALL REPORT (MANT, MIF, NPOL, MALL(1+OFFALL),
     *   MBAD(1+OFFBAD), MSOM(1+OFFSOM), NBDCOR, FLGPIX)
C                                       Report deeds to History file
      IF (IRET.EQ.0) CALL WIPEHI (NX, NY)
C                                       Clear memory
 990  CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, TVCORE, TVADDR, IERR)
C                                       Close it down
 995  IRET = MAX (0, IRET)
      CALL DIE (IRET, SBUFF)
C
 999  STOP
      END
      SUBROUTINE WIPEIN (PRGM, NX, NY, IRET)
C-----------------------------------------------------------------------
C   WIPEIN gets input parameters for WIPER .
C   Inputs:
C      PRGM   C*6   Program name
C   Output:
C      NX     I     X dimension of plot in memory image
C      NY     I     Y dimension of plot in memory image
C      IRET   I     Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   NX, NY, IRET
C
      CHARACTER UTYPE*2, STAT*4
      INTEGER  IUSER, I, IERR, ITEMP, IROUND, NPARM, LUNTB, LUN, FQVER,
     *   NIF, NUMAN(513), JERR, JJ, J
      LOGICAL   TABLE, FITASC, F, MATCH, SNEXST, EXIST
      INCLUDE 'WIPER.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA F /.FALSE./
      DATA LUNTB /19/
C-----------------------------------------------------------------------
C                                       Init IO et al.
      TSKNAM = PRGM
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSOU = 0
      NSCR = 0
      NCFILE = 0
      IRET = 0
      VER = 10000
      NFGWRI = 0
      J = MAXANT * MAXANT
      CALL FILL (J, 0, BLFLAG)
C                                       Get input parameters.
      NPARM = 288
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, SBUFF, IERR)
      RQUICK = .FALSE.
      IF (IERR.NE.0) THEN
         IRET = 8
         IF (IERR.NE.1) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
         GO TO 999
         END IF
      IF ((NPOPS.GT.NINTRN) .OR. (NTVDEV.LE.0)) THEN
         IRET = 4
         IF (NPOPS.GT.NINTRN) THEN
            MSGTXT = 'TV TASKS ARE RESERVED FOR INTERACTIVE USERS'
         ELSE
            MSGTXT = 'YOU HAVE NOT BEEN ASSIGNED A TV'
            END IF
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IRET = 5
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XSTOK, STOKES)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 25 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), SOURCS(I))
         XSOUR(I) = ' '
 25      CONTINUE
      SELQUA = IROUND (XQUAL)
      I = IROUND (XDOALL)
      I = MAX (0, I)
      ONECHN = MOD (I, 2) .EQ. 0
      I = I / 2
      ONEIF = MOD (I, 2) .EQ. 0
      I = I / 2
      ONEPOL = MOD (I, 2) .EQ. 0
      I = I / 2
      NOTCRS = MOD (I, 2) .NE. 0
      LTYPE = -7
C                                       Crunch input parameters.
      IUSER = NLUSER
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      TVCHN = 1
      REFANT = XREFAN + 0.01
      ROTATE = XROTPA
      CALL FILL (4, 0, TVCORN)
C                                       Get CATBLK from file.
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   IUSER, STAT, SBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      IUSER
         GO TO 990
         END IF
C                                       OK, get the header now
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', SBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         GO TO 990
         END IF
      CALL CHR2H (12, NAMEIN, 1, XNAMEI)
      CALL CHR2H (6, CLAIN, 1, XCLAIN)
      XDISIN = DISKIN
      XSIN = SEQIN
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
C                                       Multi-source file?
      CALL MULSDB (CATBLK, MULTI)
      IF (MULTI) THEN
         CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUNTB, SBUFF, TABLE, MULTI,
     *      FITASC, JERR)
         MULTI = MULTI .AND. (JERR.EQ.0)
         END IF
C                                       If calibrating, does SN exist
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      IF ((DOCAL) .AND. (.NOT.MULTI)) THEN
         CALL ISTAB ('SN', DISKIN, CNOIN, 1, LUNTB, SBUFF, TABLE,
     *      SNEXST, FITASC, IRET)
         IF ((.NOT.SNEXST) .OR. (IRET.NE.0)) THEN
            WRITE (MSGTXT,1050)
            CALL MSGWRT (8)
            DOCAL = .FALSE.
            END IF
         END IF
      XSIN = SEQIN
      XDISIN = DISKIN
      CALL COPY (256, CATBLK, CATUV)
      CALL COPY (256, CATBLK, TVCAT)
C                                       set flag versions
      CALL FNDEXT ('FG', CATBLK, I)
      FGVERI = IROUND (XFLAG)
      IF ((FGVERI.EQ.0) .OR. (FGVERI.GT.I)) FGVERI = I
      FGVERO = IROUND (XFGOUT)
      IF ((FGVERO.LE.0) .OR. (FGVERO.GT.I)) FGVERO = I + 1
      IF (FGVERO.LE.I) FGVERI = -ABS(FGVERI)
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      LCOR0 = ICOR0
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
C                                       Set time range.
      CALL RCOPY (8, XTIME, TIMRNG)
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)) .EQ.0.0)
     *   TIMRNG(1)=-1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)) .EQ.0.0)
     *   TIMRNG(5)=1.0E6
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      UVRNG(1) = XUVRA(1)
      UVRNG(2) = XUVRA(2)
      IF (TYPUVD.GT.0) CALL RFILL (2, 0.0, UVRNG)
      IF (UVRNG(2).LE.0.0) UVRNG(2) = 1.0E10
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, BCHAN)
      IF (BCHAN.GT.CATBLK(KINAX+JLOCF)) BCHAN = CATBLK(KINAX+JLOCF)
      ECHAN = IROUND (XECHAN)
      NCHAN = CATBLK(KINAX+JLOCF)
      IF (ECHAN.GT.CATBLK(KINAX+JLOCF)) ECHAN = CATBLK(KINAX+JLOCF)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      NCHAV = IROUND (XNCHAV)
      NCHAV = MAX (1, MIN (ECHAN-BCHAN+1, NCHAV))
      CHINC = IROUND (XCHINC)
      IF (CHINC.LE.0) CHINC = NCHAV
      IF (NCHAV.GE.ECHAN-BCHAN+1) CHINC = NCHAV
      I = (ECHAN + 1 - BCHAN - NCHAV) / CHINC
      ECHAN = BCHAN + I * CHINC + NCHAV - 1
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = IROUND (XBIF)
         EIF = IROUND (XEIF)
         BIF = MIN (MAX (1, BIF), CATBLK(KINAX+JLOCIF))
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         END IF
      MIF = EIF - BIF + 1
      DOPOL = IROUND(XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      DOAPPL = F
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LT.0) SUBARR = 0
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
C                                       Max antenna number
C                                       Allow multiple subarrays
      CALL FNDEXT ('AN', CATBLK, NSUBA)
      IF (NSUBA.GT.0) THEN
         CALL GETNAN (DISKIN, CNOIN, CATBLK, LUN, BUFF1, NUMAN, JERR)
         IF ((NSUBA.GT.0) .AND. (JERR.EQ.0)) THEN
            JJ = NUMAN(1)
            MANT = 0
            DO 80 J = 1,JJ
               MANT = MAX (MANT, NUMAN(J+1))
 80            CONTINUE
            END IF
         END IF
      IF ((SUBARR.GT.0) .AND. (SUBARR.LE.NSUBA)) NSUBA = 1
      NSUBA = MAX (1, NSUBA)
C                                       Allow multiple FQ ids
      NFRQ = 1
      IF ((FRQSEL.LE.0) .AND. (SELBAN.LE.0.0) .AND. (SELFRQ.LE.0D0))
     *   THEN
         FRQSEL = 1
C                                       Determine the number of FREQIDs.
         FQVER = 1
         CALL ISTAB ('FQ', DISKIN, CNOIN, FQVER, LUN, FQBUFF, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) THEN
            CALL FQINI ('READ', FQBUFF, DISKIN, CNOIN, FQVER, CATBLK,
     *         LUN, IFQRNO, FQKOLS, FQNUMV, NIF, IRET)
            IF (IRET.NE.0) GO TO 999
            NFRQ = FQBUFF(5)
            IF (NFRQ.GT.1) THEN
               WRITE (MSGTXT,1060) NFRQ
               CALL MSGWRT (3)
               END IF
            CALL TABIO ('CLOS', 0, IFQRNO, FQBUFF, FQBUFF, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         END IF
C                                       Find specified FQ id
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
C                                        Retain auto-correlations ?
      DOACOR = BPARM(10).GT.0.
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DO 85 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 85   CONTINUE
C                                       get image size
      NX = IROUND (XIMSIZ(1))
      NY = IROUND (XIMSIZ(2))
      IF (NX*NY.LE.0) THEN
         CALL TVOPEN (SBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         TVMAXX(1) = MAXXTV(1) - 7 * CSIZTV(1)
         TVMAXX(2) = MAXXTV(2) - 4 * CSIZTV(2)
         IF ((TVMAXX(1)-1)/2+1.GT.TVMAXX(2)) THEN
            NY = (TVMAXX(2)-1)/2
            NY = 2 * NY + 1
            NX = 2 * NY - 1
         ELSE
            NX = (TVMAXX(1)-1) / 2
            NX = 2 * NX + 1
            NY = (NX - 1) / 2 + 1
            END IF
         CALL TVCLOS (SBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Get axis types.
      TYPEAX(1) = IROUND (BPARM(1))
      TYPEAX(2) = IROUND (BPARM(2))
      INVAX(1) = TYPEAX(1).LT.0
      INVAX(2) = TYPEAX(2).LT.0
      TYPEAX(1) = ABS (TYPEAX(1))
      TYPEAX(2) = ABS (TYPEAX(2))
      IF (TYPEAX(1).EQ.6) INVAX(1) = .NOT.INVAX(1)
      IF (TYPEAX(2).EQ.6) INVAX(2) = .NOT.INVAX(2)
      FIXSCL = IROUND (BPARM(3))
C                                       Test type of plot
      IF (TYPUVD.LE.0) THEN
         IF ((TYPEAX(1).LT.1) .OR. (TYPEAX(1).GT.NUMPRM)) TYPEAX(1) = 3
         IF ((TYPEAX(2).LT.1) .OR. (TYPEAX(2).GT.NUMPRM)) TYPEAX(2) = 1
         IF ((TYPEAX(1).EQ.3) .OR. ((TYPEAX(1).GE.6) .AND.
     *      (TYPEAX(1).LE.8)) .OR. ((TYPEAX(1).EQ.17))) THEN
            BPARM(4) = BPARM(4) * 1.0E3
            BPARM(5) = BPARM(5) * 1.0E3
            END IF
         IF ((TYPEAX(2).EQ.3) .OR. ((TYPEAX(2).GE.6) .AND.
     *      (TYPEAX(2).LE.8)) .OR. ((TYPEAX(2).EQ.17))) THEN
            BPARM(6) = BPARM(6) * 1.0E3
            BPARM(7) = BPARM(7) * 1.0E3
            END IF
      ELSE
         IF ((TYPEAX(1).LT.1) .OR. (TYPEAX(1).GT.NUMPRM)) TYPEAX(1) = 11
         IF ((TYPEAX(2).LT.1) .OR. (TYPEAX(2).GT.NUMPRM)) TYPEAX(2) = 9
         IF (TYPEAX(1).EQ.1) TYPEAX(1) = 9
         IF (TYPEAX(1).EQ.2) TYPEAX(1) = 10
         IF (TYPEAX(1).EQ.3) TYPEAX(1) = 6
         IF (TYPEAX(1).EQ.4) TYPEAX(1) = 7
         IF (TYPEAX(1).EQ.8) TYPEAX(1) = 6
         IF (TYPEAX(2).EQ.1) TYPEAX(2) = 9
         IF (TYPEAX(2).EQ.2) TYPEAX(2) = 10
         IF (TYPEAX(2).EQ.3) TYPEAX(2) = 6
         IF (TYPEAX(2).EQ.4) TYPEAX(2) = 7
         IF (TYPEAX(2).EQ.8) TYPEAX(2) = 6
         END IF
      XYMAX(1) = -1.0E10
      XYMAX(2) = XYMAX(1)
      XYMAX(3) = XYMAX(1)
      XYMIN(1) = 1.E10
      XYMIN(2) = XYMIN(1)
      XYMIN(3) = XYMIN(1)
      IPHASE = 0
      IF (TYPEAX(1).EQ.2) IPHASE = 1
      IF (TYPEAX(2).EQ.2) IPHASE = 2
      SPHASE = .FALSE.
C                                       If plotting uv only
C                                       then plot conjugate points
      ITEMP = TYPEAX(1) * TYPEAX(2)
      UVREV = (ITEMP.EQ.42) .OR. (ITEMP.EQ.48) .OR. (ITEMP.EQ.56)
      UVREV = (UVREV) .AND. (TYPUVD.LE.0)
C                                       Initialize baseline selection.
      CALL SETANT (50, XANT, XBASE, NANT, NBAS, IANT, IBAS, DESEL)
C                                       open terminal
      TTY(1) = 5
      TTY(2) = 0
      CALL ZOPEN (TTY(1), TTY(2), 1, MSGTXT, .FALSE., .TRUE., .TRUE.,
     *   IRET)
      IF (IRET.NE.0) THEN
         TTY(2) = 0
         MSGTXT = 'FAILED TO OPEN INTERACTIVE TERMINAL'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WIPEIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('Error',I3,' finding ',A12,'.',A6,'.',I4,' disk =',
     *   I3,' user=',I5)
 1035 FORMAT ('Error',I3,' obtaining CATBLK ')
 1050 FORMAT ('NO SN FILE FOUND, BUT DOCALIB IS TRUE: NO CAL APPLIED')
 1060 FORMAT ('Plotting',I4,' frequency IDs.')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
      END
      SUBROUTINE SCAL (GETSCL, XZY, IRET)
C-----------------------------------------------------------------------
C   SCAL sends uv points one at a time to XYOFF .
C   Input:
C      GETSCL   L   If false, do not read all the data, just go through
C                   the rest of the motions
C   Output:
C      IRET     I   Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      LOGICAL   GETSCL
      REAL      XZY(3,*)
      INTEGER   IRET
C
      INTEGER   I, NUMVIS, XUMVIS, J, JJJ, ISUB, JSUB, NXVER, NIF,
     *   NXLUN, IROUND
      LOGICAL   REQBAS, REQAS
      INCLUDE 'WIPER.INC'
      REAL      SV, CATR(256)
      DOUBLE PRECISION CATD(128)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
C-----------------------------------------------------------------------
      NUMVIS = 0
      XUMVIS = 0
      JSUB = SUBARR
      NXVER = 1
      NXLUN = 100
      CSOU = -1
      REQAS = ((TYPEAX(1).GE.14) .AND. (TYPEAX(1).LE.16)) .OR.
     *   ((TYPEAX(2).GE.14) .AND. (TYPEAX(2).LE.16))
C                                       Loop for each FREQID.
      DO 150 IFRQ = 1,NFRQ
         CALL FILL (MAXIF, 0, EXCLFQ(1,IFRQ))
         IF (NFRQ.GT.1) THEN
            FRQSEL = IFRQ
            WRITE (MSGTXT,1000) IFRQ
            CALL MSGWRT (5)
            END IF
         CALL CHNDAT ('READ', NXBUFF, DISKIN, CNOIN, NXVER, CATUV,
     *      NXLUN, NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'PROBLEM FINDING FREQUENCIES'
            CALL MSGWRT (6)
            GO TO 150
            END IF
         DO 145 ISUB = 1,NSUBA
            IF (JSUB.EQ.0) SUBARR = ISUB
            IF (REQAS) THEN
               CALL GETANT (DISKIN, CNOIN, SUBARR, CATUV, SBUFF, IRET)
               IF (IRET.NE.0) THEN
                  EXCLFQ(ISUB,IFRQ) = 1
                  MSGTXT = 'PROBLEM FINDING ANTENNA INFO'
                  CALL MSGWRT (6)
                  GO TO 145
                  END IF
               END IF
C                                       Init vis file for read.
            CALL UVGET ('INIT', RPARM, BUFF1, IRET)
C
            IF (IRET.EQ.-1) GO TO 140
            IF (IRET.EQ.5) THEN
               IRET = 0
               EXCLFQ(ISUB,IFRQ) = 1
               GO TO 140
               END IF
            IF (IRET.GT.0) GO TO 999
            NPOL = NCOR
            DO 20 I = 1,NPOL
               ISCROS(I) = .TRUE.
               SV = CATD(KDCRV+JLOCS) + (I-CATR(KRCRP+JLOCS)) *
     *            CATR(KRCIC+JLOCS)
               J = IROUND (SV)
               IF ((J.GE.-2) .AND. (J.LE.1)) ISCROS(I) = .FALSE.
               IF ((J.GE.-6) .AND. (J.LE.-5)) ISCROS(I) = .FALSE.
 20            CONTINUE
C                                       Loop Read vis. record.
 100        CALL UVGET ('READ', RPARM, BUFF1, IRET)
               IF (IRET.EQ.-1) GO TO 140
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) IRET
                  GO TO 990
                  END IF
               IF (.NOT.GETSCL) GO TO 140
C                                       Do we need this baseline?
               IF (ILOCB.GE.0) THEN
                  I = INT (RPARM(ILOCB+1)) / 256
                  J = MOD (INT (RPARM(ILOCB+1)), 256)
               ELSE
                  I = RPARM(ILOCA1+1) + 0.1
                  J = RPARM(ILOCA2+1) + 0.1
                  END IF
               IF (.NOT.REQBAS (I, J, DESEL, IANT, NANT, IBAS, NBAS))
     *            GO TO 100
               NUMVIS = NUMVIS + 1
               IF (CURSOU.NE.CSOU) THEN
                  CSOU = CURSOU
                  CALL GETSOU (CSOU, DISKIN, CNOIN, CATUV, NXLUN, IRET)
                  IF (IRET.NE.0) THEN
                     MSGTXT = 'TROUBLE GETTING SOURCE INFO'
                     IF (REQAS) CALL MSGWRT (6)
C                                       add to source list
                  ELSE
                     DO 110 I = 1,NSOU
                        IF (SNAME.EQ.XSOUR(I)) GO TO 120
 110                    CONTINUE
                     IF (NSOU.LT.30) THEN
                        NSOU = NSOU + 1
                        XSOUR(NSOU) = SNAME
                        END IF
                     END IF
                  END IF
C                                       Find scales
 120           CALL FNDXY (RPARM, BUFF1, NPOL, XZY)
               CALL XYSCAL (NUMVIS, NPOL, XZY, JJJ, IRET)
               IF (IRET.EQ.0) XUMVIS = XUMVIS + JJJ
               GO TO 100
 140        CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
 145        CONTINUE
 150     CONTINUE
      SUBARR = JSUB
      IRET = 0
      IF (.NOT.GETSCL) GO TO 999
C                                       Any valid points
      IF (XUMVIS.LE.1) THEN
         IRET = 4
         WRITE (MSGTXT,1200) XUMVIS
         GO TO 990
         END IF
C                                       Final call to XYSCAL
      NUMVIS = -1
      CALL XYSCAL (NUMVIS, NPOL, XZY, JJJ, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1110) IRET
         GO TO 990
         END IF
      IRET = 0
      GO TO 999

C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Processing FREQID =',I3)
 1100 FORMAT ('SCAL: ERROR',I3,' READING VIS FILE')
 1110 FORMAT ('SCAL: XYSCAL ERROR',I3)
 1200 FORMAT ('FOUND',I5,' POINTS: NOT ENOUGH TO SELF-SCALE')
      END
      SUBROUTINE PLOTUV (NP, XZY, NX, NY, TVCORE, NBL, BLCORE, IRET)
C-----------------------------------------------------------------------
C   PLOTUV actually plots uv data into TVCORE
C   Inputs:
C      NX       I          X dimension of TVCORE
C      NY       I          Y dimension of TVCORE
C      NBL      I          Max # BL in BLCORE point
C   Output:
C      TVCORE   I(*,*)     Plot as count of samples in cell
C      BLCORE   I(*,*,*)   Baseline numbers in tv pixels
C      IRET     I          Return code, 0 => OK, otherwise abort.
C                              4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   NP, NX, NY, TVCORE(NX,NY), NBL, BLCORE(NBL,NX,NY), IRET
      REAL      XZY(3,NP,*)
C
      INCLUDE 'WIPER.INC'
      CHARACTER AUNITS(NUMPRM)*8, BUNITS(NUMPRM)*8, BNDCOD(MAXIF)*8
      INTEGER   I, IC, J, JJJ, ICO, NUMVIS, JSUB, ISUB, NXLUN, NIF,
     *   NXVER, LC, LF, LP, IX, IY, NREV, IBL
      REAL      BLC(2), TRC(2), TR, XY(2), TEMP, SXYMIN(2), SXYMAX(2)
      HOLLERITH CATH(256)
      LOGICAL   REQBAS, REQAS
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (CATBLK, CATH)
      DATA AUNITS /'Janskys ', 'Degrees ', 'Wavlngth', 'Degrees ',
     *   'IAT days', 'Wavlngth', 'Wavlngth', 'Wavlngth', 'Janskys ',
     *   'Janskys ', 'IAT hrs ', 'log (Jy)', '1/Jy**2', 'Hours',
     *   'Degrees ', 'Degrees ', 'Wavlngth', 'Degrees', 'Hertz',
     *   'Pixels'/
      DATA BUNITS /'Kelvins ', 'Degrees ', 'Wavlngth', 'Degrees ',
     *   'IAT days', 'Wavlngth', 'Wavlngth', 'Wavlngth', 'Kelvins ',
     *   'Kelvins ', 'IAT hrs ', 'log (K)', '1/(K**2)', 'Hours',
     *   'Degrees ', 'Degrees ', 'Wavlngth', 'Degrees', 'Hertz',
     *   'Pixels'/
C-----------------------------------------------------------------------
      NREV = 1
      IF (UVREV) NREV = 2
      IRET = 1
      CALL FILL (9, 0, COUNT)
      IX = NX * NY
      CALL FILL (IX, 0, TVCORE)
      IX = NBL * IX
      CALL FILL (IX, 0, BLCORE)
      REQAS = ((TYPEAX(1).GE.14) .AND. (TYPEAX(1).LE.16)) .OR.
     *   ((TYPEAX(2).GE.14) .AND. (TYPEAX(2).LE.16))
      SPHASE = .FALSE.
      IF ((IPHASE.GT.0) .AND.
     *   (XYMAX(3)-XYMIN(3).LT.XYMAX(IPHASE)-XYMIN(IPHASE))) THEN
         SPHASE = .TRUE.
         XYMIN(IPHASE) = XYMIN(3)
         XYMAX(IPHASE) = XYMAX(3)
         END IF
C                                       User sets the scales
C                                       Note that case of FIXSCL<0
C                                       is handled in setting XYMIN
C                                       and XYMAX.
      IF (FIXSCL.GT.0) THEN
         IF (BPARM(5).GT.BPARM(4)) THEN
            XYMIN(1) = BPARM(4)
            XYMAX(1) = BPARM(5)
            IF ((IPHASE.EQ.1) .AND. (XYMIN(1).GE.0.0)) SPHASE = .TRUE.
            END IF
         IF (BPARM(7).GT.BPARM(6)) THEN
            XYMIN(2) = BPARM(6)
            XYMAX(2) = BPARM(7)
            IF ((IPHASE.EQ.2) .AND. (XYMIN(2).GE.0.0)) SPHASE = .TRUE.
            END IF
         END IF
      IF (FIXSCL.LT.0) THEN
         IF (BPARM(5).GT.BPARM(4)) THEN
            XYMIN(1) = MAX (XYMIN(1), BPARM(4))
            XYMAX(1) = MIN (XYMAX(1), BPARM(5))
            IF ((IPHASE.EQ.1) .AND. (XYMIN(1).GE.0.0)) SPHASE = .TRUE.
            END IF
         IF (BPARM(7).GT.BPARM(6)) THEN
            XYMIN(2) = MAX (XYMIN(2), BPARM(6))
            XYMAX(2) = MIN (XYMAX(2), BPARM(7))
            IF ((IPHASE.EQ.2) .AND. (XYMIN(2).GE.0.0)) SPHASE = .TRUE.
            END IF
         END IF
C                                       Provide room at edges.
      DO 10 I = 1,2
         TEMP = 0.025 * (XYMAX(I) - XYMIN(I))
         IF (XYMIN(I).NE.BPARM(2*I+2)) XYMIN(I) = XYMIN(I) - TEMP
         IF (XYMAX(I).NE.BPARM(2*I+3)) XYMAX(I) = XYMAX(I) + TEMP
         IF (XYMAX(I).EQ.XYMIN(I)) GO TO 999
 10      CONTINUE
C                                       Now set the offset and scale.
      IF (INVAX(1)) THEN
         XYOFF(1) = XYMAX(1)
         XYSCL(1) = (NX - 0.001) / (XYMIN(1)-XYMAX(1))
      ELSE
         XYOFF(1) = XYMIN(1)
         XYSCL(1) = (NX - 0.001) / (XYMAX(1)-XYMIN(1))
         END IF
      IF (INVAX(2)) THEN
         XYOFF(2) = XYMAX(2)
         XYSCL(2) = (NY - 0.001) / (XYMIN(2)-XYMAX(2))
      ELSE
         XYOFF(2) = XYMIN(2)
         XYSCL(2) = (NY - 0.001) / (XYMAX(2)-XYMIN(2))
         END IF
C                                       Fill in last of actual parms
      BPARM(5) = (NX-0.001)/XYSCL(1) + XYOFF(1)
      BPARM(7) = (NY-0.001)/XYSCL(2) + XYOFF(2)
      BPARM(4) = XYOFF(1)
      BPARM(6) = XYOFF(2)
C                                      Initialize UV reading
      ISUVR = (UVRNG(1).GE.0.0) .AND. (UVRNG(2).GT.UVRNG(1))
      IF ((UVRNG(1).EQ.0.0) .AND. (UVRNG(2).GE.1.E10)) ISUVR = .FALSE.
      XUVRA(1) = UVRNG(1)
      XUVRA(2) = UVRNG(2)
      CALL RCOPY (8, TIMRNG, XTIME)
      XBCHAN = BCHAN
      XECHAN = ECHAN
      XCHINC = CHINC
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = NX - 0.001
      TRC(2) = NY - 0.001
      IRET = 3
C                                       Set up location common and TV
C                                       header
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      IF (TYPEAX(1).EQ.11) LABTYP(LOCNUM) = 7
      IF (TYPEAX(2).EQ.11) LABTYP(LOCNUM) = 70
      AXTYP(LOCNUM) = 0
      DO 30 I = 1,2
         TR = TRC(I) / XYSCL(I)
         RPLOC(I,LOCNUM) = BLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I)
         AXINC(I,LOCNUM) = TR / (TRC(I) - BLC(I))
C                                       do not do METSCA here
         IF (TYPEAX(I).NE.11) THEN
            CPREF(I,LOCNUM) = ' '
            SXYMIN(I) = XYMIN(I)
            SXYMAX(I) = XYMAX(I)
         ELSE
            CPREF(I,LOCNUM) = ' '
            RPVAL(I,LOCNUM) = RPVAL(I,LOCNUM) * 360.
            AXINC(I,LOCNUM) = AXINC(I,LOCNUM) * 360.
            CTYP(I,LOCNUM) = AUNITS(11)
            SXYMIN(I) = XYMIN(I) * 24.0
            SXYMAX(I) = XYMAX(I) * 24.0
            END IF
C                                       interferometer
         IF (TYPUVD.LE.0) THEN
            CTYP(I,LOCNUM) = AUNITS(TYPEAX(I))
C                                       single dish
         ELSE
            CTYP(I,LOCNUM) = BUNITS(TYPEAX(I))
            END IF
         TVCATD(KDCRV+I-1) = RPVAL(I,LOCNUM)
         TVCATR(KRCRP+I-1) = RPLOC(I,LOCNUM)
         TVCATR(KRCIC+I-1) = AXINC(I,LOCNUM)
         CALL CHR2H (8, CTYP(I,LOCNUM), 1, CATH(KHCTP+2*(I-1)))
 30      CONTINUE
      TVCAT(KINAX) = NX
      TVCAT(KINAX+1) = NY
      NUMVIS = 0
      IF (INVAX(1)) THEN
         WRITE (MSGTXT,1030) 'X', CPREF(1,LOCNUM), CTYP(1,LOCNUM)(:12),
     *      SXYMAX(1), SXYMIN(1)
      ELSE
         WRITE (MSGTXT,1030) 'X', CPREF(1,LOCNUM), CTYP(1,LOCNUM)(:12),
     *      SXYMIN(1), SXYMAX(1)
         END IF
      CALL MSGWRT (3)
      IF (INVAX(2)) THEN
         WRITE (MSGTXT,1030) 'Y', CPREF(2,LOCNUM), CTYP(2,LOCNUM)(:12),
     *      SXYMAX(2), SXYMIN(2)
      ELSE
         WRITE (MSGTXT,1030) 'Y', CPREF(2,LOCNUM), CTYP(2,LOCNUM)(:12),
     *      SXYMIN(2), SXYMAX(2)
         END IF
      CALL MSGWRT (3)
C                                       Loop for each FREQID.
      NXLUN = 100
      NXVER = 1
      JSUB = SUBARR
      DO 150 IFRQ = 1,NFRQ
         IF (NFRQ.GT.1) FRQSEL = IFRQ
         CALL CHNDAT ('READ', NXBUFF, DISKIN, CNOIN, NXVER, CATUV,
     *      NXLUN, NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'PROBLEM FINDING FREQUENCIES'
            CALL MSGWRT (6)
            GO TO 150
            END IF
         DO 145 ISUB = 1,NSUBA
            IF (EXCLFQ(ISUB,IFRQ).NE.0) GO TO 145
            IF (JSUB.EQ.0) SUBARR = ISUB
            IF (REQAS) THEN
               CALL GETANT (DISKIN, CNOIN, SUBARR, CATUV, SBUFF, IRET)
               IF (IRET.NE.0) THEN
                  EXCLFQ(ISUB,IFRQ) = 1
                  MSGTXT = 'PROBLEM FINDING ANTENNA INFO'
                  CALL MSGWRT (6)
                  GO TO 145
                  END IF
               END IF
C                                       Initialize UV reading.
            CALL UVGET ('INIT', RPARM, BUFF1, IRET)
C
            IF (IRET.EQ.-1) GO TO 140
            IF (IRET.EQ.5) GO TO 140
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT, 1050) IRET
               CALL MSGWRT (8)
               IRET = 4
               GO TO 999
               END IF
            NPOL = NCOR
C                                       Loop: Read vis. record.
 100        CALL UVGET ('READ', RPARM, BUFF1, IRET)
               IF (IRET.EQ.-1) GO TO 140
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) IRET
                  CALL MSGWRT (8)
                  IRET = 4
                  GO TO 999
                  END IF
C                                       Check whether we need this
C                                       baseline
               IF (ILOCB.GE.0) THEN
                  I = INT (RPARM(ILOCB+1)) / 256
                  J = MOD (INT (RPARM(ILOCB+1)), 256)
               ELSE
                  I = RPARM(ILOCA1+1) + 0.1
                  J = RPARM(ILOCA2+1) + 0.1
                  END IF
               IF (.NOT.REQBAS (I, J, DESEL, IANT, NANT, IBAS, NBAS))
     *            GO TO 100
               IBL = 1000*I + J
               NUMVIS = NUMVIS + 1
               IF (CURSOU.NE.CSOU) THEN
                  CSOU = CURSOU
                  CALL GETSOU (CSOU, DISKIN, CNOIN, CATUV, NXLUN, IRET)
                  IF (IRET.NE.0) THEN
                     MSGTXT = 'TROUBLE GETTING SOURCE INFO'
                     IF (REQAS) CALL MSGWRT (6)
C                                       add to source list
                  ELSE
                     DO 110 I = 1,NSOU
                        IF (SNAME.EQ.XSOUR(I)) GO TO 115
 110                    CONTINUE
                     IF (NSOU.LT.30) THEN
                        NSOU = NSOU + 1
                        XSOUR(NSOU) = SNAME
                        END IF
                     END IF
                  END IF
C                                       Get and scale X, Y
 115           CALL FNDXY (RPARM, BUFF1, NP, XZY)
               ICO = ECHAN - BCHAN + 1
               LC = 0
               DO 135 LF = BIF,EIF
                  DO 130 IC = 1,ICO,CHINC
                     LC = LC + 1
                     DO 129 LP = 1,NPOL
                        IF ((XZY(1,LP,LC).NE.FBLANK) .AND.
     *                     (XZY(2,LP,LC).NE.FBLANK) .AND.
     *                     (XZY(3,LP,LC).GT.0.0)) THEN
                           IF ((SPHASE) .AND. (IPHASE.GT.0)) THEN
                              TEMP = XZY(IPHASE,LP,LC)
                              IF (TEMP.LT.0) TEMP = TEMP + 360.0
                              XZY(IPHASE,LP,LC) = TEMP
                              END IF
                           DO 128 JJJ = 1,NREV
                              DO 120 J = 1,2
                                 XY(J) = XYSCL(J) *
     *                              (XZY(J,LP,LC)-XYOFF(J))
 120                             CONTINUE
C                                       Mark the point
                              IX = XY(1) + 1.0
                              IY = XY(2) + 1.0
                              IF (IX.LT.1) THEN
                                 IF (IY.GT.NY) THEN
                                    COUNT(1,1) = COUNT(1,1) + 1
                                 ELSE IF (IY.GT.0) THEN
                                    COUNT(1,2) = COUNT(1,2) + 1
                                 ELSE
                                    COUNT(1,3) = COUNT(1,3) + 1
                                    END IF
                              ELSE IF (IX.GT.NX) THEN
                                 IF (IY.GT.NY) THEN
                                    COUNT(3,1) = COUNT(3,1) + 1
                                 ELSE IF (IY.GT.0) THEN
                                    COUNT(3,2) = COUNT(3,2) + 1
                                 ELSE
                                    COUNT(3,3) = COUNT(3,3) + 1
                                    END IF
                              ELSE
                                 IF (IY.GT.NY) THEN
                                    COUNT(2,1) = COUNT(2,1) + 1
                                 ELSE IF (IY.GT.0) THEN
                                    COUNT(2,2) = COUNT(2,2) + 1
                                    TVCORE(IX,IY) = TVCORE(IX,IY) + 1
                                    CALL COUNTR (NX, NY, IX, IY, IBL,
     *                                 NBL, BLCORE)
                                 ELSE
                                    COUNT(2,3) = COUNT(2,3) + 1
                                    END IF
                                 END IF
                              XZY(1,LP,LC) = -XZY(1,LP,LC)
                              XZY(2,LP,LC) = -XZY(2,LP,LC)
 128                          CONTINUE
                           END IF
 129                    CONTINUE
 130                 CONTINUE
 135              CONTINUE
               GO TO 100
 140        CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
 145        CONTINUE
 150     CONTINUE
      SUBARR = JSUB
C                                       Done
      IRET = 0
      WRITE (MSGTXT,1150) COUNT(2,2)
      CALL MSGWRT (2)
      IF (COUNT(2,2).LE.0) IRET = 1
      JJJ = -COUNT(2,2)
      DO 160 I = 1,3
         DO 155 J = 1,3
            JJJ = JJJ + COUNT(J,I)
 155        CONTINUE
 160     CONTINUE
C                                       points off plot
      IF (JJJ.GT.0) THEN
         WRITE (MSGTXT,1155) JJJ
         CALL MSGWRT (2)
         MSGTXT = 'Image of the points'' location wrt image'
         CALL MSGWRT (2)
         WRITE (MSGTXT,1160) COUNT(1,1), COUNT(2,1), COUNT(3,1)
         CALL MSGWRT (2)
         WRITE (MSGTXT,1160) COUNT(1,2), COUNT(2,2), COUNT(3,2)
         CALL MSGWRT (2)
         WRITE (MSGTXT,1160) COUNT(1,3), COUNT(2,3), COUNT(3,3)
         CALL MSGWRT (2)
         END IF
C                                       Add 1 to sampled cells only
      MBL = 1
      DO 200 J = 1,NY
         DO 190 I = 1,NX
            IF (TVCORE(I,J).GE.1) THEN
               TVCORE(I,J) = TVCORE(I,J) + 1
               DO 180 IBL = MBL,NBL
                  IF (BLCORE(IBL,I,J).NE.0) MBL = MAX (MBL, IBL)
 180              CONTINUE
               END IF
 190        CONTINUE
 200     CONTINUE
      IF (MBL.GE.NBL) THEN
         WRITE (MSGTXT,1200) NBL
         CALL MSGWRT (6)
         MSGTXT = 'This will complicate flagging by baseline'
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('PLOTUV: ',A1,' axis in ',A,A,2X,2(1PE11.3))
 1050 FORMAT ('PLOTUV: ERROR',I3,' INIT VIS FILE')
 1100 FORMAT ('PLOTUV: ERROR',I3,' READING VIS FILE')
 1150 FORMAT ('PLOTUV: ',I10,' Points put in array')
 1155 FORMAT ('PLOTUV: ',I10,' Points did not fit')
 1160 FORMAT ('PLOTUV: ',I10,' * ',I10,' * ',I10)
 1200 FORMAT ('Warning: more than',I3,' baselines at a single point')
      END
      SUBROUTINE COUNTR (NX, NY, IX, IY, IBL, NBL, BLCORE)
C-----------------------------------------------------------------------
C   Records the baseline in BLCORE(i,IX,IY) if possible
C   Inputs:
C      NX       I            X dimension of BLCORE
C      NY       I            Y dimension of BLCORE
C      IX       I            X pixel to do counting
C      IY       I            Y pixel to do counting
C      IBL      I            Baseline code
C      NBL      I            Max number BLs in BLCORE
C   In/Out:
C      BLCORE   I(10,NX,NY)   array of baseline codes
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IX, IY, IBL, NBL, BLCORE(NBL,NX,NY)
C
      INTEGER   I, J
C-----------------------------------------------------------------------
      J = NBL
      DO 10 I = 1,NBL-1
         IF (IBL.EQ.ABS(BLCORE(I,IX,IY))) GO TO 999
         IF (BLCORE(I,IX,IY).EQ.0) J = MIN (J, I)
 10      CONTINUE
      IF (J.EQ.NBL) THEN
         BLCORE(NBL,IX,IY) = -1000000
      ELSE
         BLCORE(J,IX,IY) = IBL
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDITUV (NX, NY, TVCORE, NBL, BLCORE, IRET)
C-----------------------------------------------------------------------
C   EDITUV displays the uv data on the TV and offers editing options
C   Inputs:
C      NX       I           X dimension of TVCORE
C      NY       I           Y dimension of TVCORE
C      NBL      I          Max # baselines in BLCORE
C   In/out:
C      TVCORE   I(*,*)      Plot as count of samples in cell - negative
C                           means flag.
C      BLCORE   I(*,*,*)   Baseline numbers in tv pixels
C   Output:
C      IRET     I           Return code, 0 => OK, otherwise abort.
C                              4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   NX, NY, TVCORE(NX,NY), NBL, BLCORE(NBL,NX,NY), IRET
C
      INCLUDE 'WIPER.INC'
      INTEGER   MC
      PARAMETER (MC = 19)
C
      INTEGER   TVC(4), IMW(4), IX, IY, IGR, I, IGM, MTYP, MCOL, MROW,
     *   MGRS(2), TOPSEP, TIMLIM, CHS, TVBUT, IP, ITY, ISN, SVZOOM(3),
     *   IGB, BX(2), BY(2), PZERO(2), CATTMP(256), IGBB, NTITLE, SIDSEP,
     *   II, J
      CHARACTER ISHELP*6, TITLE*24, CHOICS(MC)*12, CHTYPE(NUMPRM)*9,
     *   CHTYP2(NUMPRM)*9, TEXT*9
      LOGICAL   LEAVE(MC), MENUOK, BORDER(4), ZERO(2)
      REAL      TEMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA MCOL, MROW, TOPSEP, SIDSEP, TIMLIM /1, MC, 20, 10, 0/
      DATA CHOICS /'ABORT', 'EXIT', ' ', 'OFF ZOOM', 'TVZOOM',
     *   'SET WINDOW','RESET WINDOW', 'FLAG POINT', 'FLAG AREA',
     *   'FLAG FAST', 'FLAG FAT', 'FLAG BASELIN', 'UNFLAG POINT',
     *   'UNFLAG AREA', 'UNFLAG FAST', 'UNFLAG FAT', 'UNFLAG BASEL',
     *   'WIPER SET', 'WIPER SIZE'/
      DATA LEAVE /3*.FALSE., .TRUE., 2*.FALSE., .TRUE., 4*.FALSE.,
     *   .TRUE.,4*.FALSE., .TRUE.,2*.FALSE./
      DATA CHTYPE /'Amplitude', 'Phase   ', 'UV dist ',
     *   'UV pa    ', 'Time    ', 'U       ', 'V       ',
     *   'W        ', 'Real    ', 'Imag    ', 'Time hrs',
     *   'log (amp)', 'Weight', 'Hour angl', 'Elevation', 'Para angl',
     *   'UV@pa=   ', 'Azimuth', 'Frequency', 'Channel'/
      DATA CHTYP2 /'Flux     ', 'Offset  ', 'UV dist ',
     *   'UV pa    ', 'Time    ', 'Longitude', 'Latitude',
     *   'W        ', 'Flux    ', 'Offset  ', 'Time hrs',
     *   'log(flux)', 'Weight', 'Hour angl', 'Elevation', 'Para angl',
     *   'UV@pa=   ', 'Azimuth', 'Frequency', 'Channel'/
C-----------------------------------------------------------------------
      CALL TVOPEN (SBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      TVMAXX(1) = MAXXTV(1) - 7 * CSIZTV(1)
      TVMAXX(2) = MAXXTV(2) - 4 * CSIZTV(2)
      MTYP = XDOCEN + 1.5
C                                       init the TV: off grays
C                                       clear graphics
      CALL YHOLD ('ONNN', IRET)
      TVZOOM(1) = 0
      TVZOOM(2) = MAXXTV(1)/2
      TVZOOM(3) = MAXXTV(2)/2
      CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Turn off scroll!!
      I = 0
      IX = 2 ** NGRAY
      CALL YSCROL (IX, I, I, .FALSE., IRET)
      IF (IRET.NE.0) GO TO 990
      DO 5 I = 1,NGRAY+NGRAPH
         IF ((I.GT.NGRAY) .AND. (I.LE.NGRAY+5)) THEN
            CALL YZERO (I, IRET)
            IF (IRET.NE.0) GO TO 990
            CALL YSLECT ('ONNN', I, 0, TVSCR, IRET)
         ELSE
            CALL YSLECT ('OFFF', I, 0, TVSCR, IRET)
            END IF
         IF (IRET.NE.0) GO TO 990
 5       CONTINUE
      IGB = NGRAY + 5
      IGM = NGRAY + 2
      CALL YSLECT ('OFFF', IGM, 0, TVSCR, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL YHOLD ('OFFF', IRET)
      MENUOK = .FALSE.
      TITLE =' '
      NTITLE = 0
C                                       where is the value 0?
      TEMP = - XYSCL(1) * XYOFF(1)
      PZERO(1) = TEMP + 1.0
      TEMP = - XYSCL(2) * XYOFF(2)
      PZERO(2) = TEMP + 1.0
C                                       full image again
 10   IMW(1) = 1
      IMW(2) = 1
      IMW(3) = NX
      IMW(4) = NY
C                                       does it fit
 20   I = 1
      II = 1
      IX = IMW(3) - IMW(1) + 1
      IY = IMW(4) - IMW(2) + 1
 25   IF (((IX+I-1)/I.GT.TVMAXX(1)) .OR. ((IY+I-1)/I.GT.TVMAXX(2))) THEN
         I = I + 1
         GO TO 25
         END IF
C                                       Must smooth
      IF (I.GT.1) THEN
         WRITE (MSGTXT,1025) I
         CALL MSGWRT (1)
C                                       can we pixel replicate
      ELSE
 30      IF ((IX*II.LT.TVMAXX(1)) .AND. (IY*II.LT.TVMAXX(2))) THEN
            II = II + 1
            GO TO 30
         ELSE
            II = II - 1
            END IF
         WRITE (MSGTXT,1026) II
         IF (II.GT.1) CALL MSGWRT (4)
         END IF
C                                       Init
      CALL YWINDO ('READ', WINDTV, IRET)
      IF (IRET.NE.0) THEN
         WINDTV(1) = 1
         WINDTV(2) = 1
         WINDTV(3) = MAXXTV(1)
         WINDTV(4) = MAXXTV(2)
         IRET = 0
         END IF
C                                       window location
      IX = (IX + I - 1) / I * II
      IY = (IY + I - 1) / I * II
      TVC(1) = (TVMAXX(1) - IX)/2 + 1 + (MAXXTV(1)-TVMAXX(1))
      TVC(3) = TVC(1) + IX - 1
      TVC(2) = WINDTV(2) + 4.5*CSIZTV(2)
      TVC(4) = TVC(2) + IY - 1
      IF (TVC(4).GE.MAXXTV(2)) THEN
         TVC(2) = 1 + 5 * CSIZTV(2)
         TVC(4) = TVC(2) + IY - 1
         IF (TVC(4).GE.MAXXTV(2)) THEN
            TVC(2) = (TVMAXX(2) - IY)/2 + 1 + (MAXXTV(2)-TVMAXX(2))
            TVC(4) = TVC(2) + IY - 1
            END IF
         END IF
      BORDER(1) = (TVC(1).GT.1) .AND. (IMW(1).EQ.1)
      BORDER(2) = (TVC(2).GT.1) .AND. (IMW(2).EQ.1)
      BORDER(3) = (TVC(3).LT.MAXXTV(1)) .AND. (IMW(3).EQ.NX)
      BORDER(4) = (TVC(4).LT.MAXXTV(2)) .AND. (IMW(4).EQ.NY)
      ZERO(1) = (PZERO(1).GE.IMW(1)) .AND. (PZERO(1).LE.IMW(3))
      ZERO(2) = (PZERO(2).GE.IMW(2)) .AND. (PZERO(2).LE.IMW(4))
      IF ((TVC(1)-1.LT.WINDTV(1)) .OR. (TVC(2)-1.LT.WINDTV(2)) .OR.
     *   (TVC(3)+1.GT.WINDTV(3)) .OR. (TVC(4)+1.GT.WINDTV(4))) THEN
         WINDTV(1) = MIN (WINDTV(1), TVC(1))
         WINDTV(2) = MIN (WINDTV(2), TVC(2))
         WINDTV(3) = MAX (WINDTV(3), TVC(3))
         WINDTV(4) = MAX (WINDTV(4), TVC(4))
         IF (BORDER(1)) WINDTV(1) = MIN (WINDTV(1), TVC(1)-1)
         IF (BORDER(2)) WINDTV(2) = MIN (WINDTV(2), TVC(2)-1)
         IF (BORDER(3)) WINDTV(3) = MAX (WINDTV(3), TVC(3)+1)
         IF (BORDER(4)) WINDTV(4) = MAX (WINDTV(4), TVC(4)+1)
         CALL YWINDO ('WRIT', WINDTV, IRET)
         END IF
      CALL COPY (4, IMW, TVCAT(IIWIN))
      CALL COPY (4, TVC, TVCAT(IICOR))
      J = I
      IF (II.GT.1) J = -II
      CALL TVLOAD (NX, NY, TVCORE, J, TVC, IMW, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       draw border and label
      IGBB = 5
      IGB = NGRAY + IGBB
      CALL YZERO (IGB, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL COPY (256, CATBLK, CATTMP)
      CALL COPY (256, TVCAT, CATBLK)
      IF (TYPUVD.LE.0) THEN
         TEXT = CHTYPE(TYPEAX(1))
      ELSE
         TEXT = CHTYP2(TYPEAX(1))
         END IF
      CALL CHR2H (8, TEXT, 1, CATH(KHCTP))
      IF (TYPUVD.LE.0) THEN
         TEXT = CHTYPE(TYPEAX(2))
      ELSE
         TEXT = CHTYP2(TYPEAX(2))
         END IF
      CALL CHR2H (8, TEXT, 1, CATH(KHCTP+2))
      CALL IAXIS1 (TVSCR, LTYPE, IGBB, 1, .FALSE., IRET)
      CALL COPY (256, CATTMP, CATBLK)
      CALL REMETS (CPREF(1,LOCNUM), SCALXY(1))
      CALL REMETS (CPREF(2,LOCNUM), SCALXY(2))
C                                       zero graphics
      IF (ZERO(1)) THEN
         BX(1) = TVC(1) + PZERO(1) - IMW(1)
         BX(2) = BX(1)
         BY(1) = MAX (1, TVC(2) - 1)
         BY(2) = MIN (MAXXTV(2), TVC(4) + 1)
         CALL IMVECT ('ONNN', IGB, 2, BX, BY, TVSCR, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      IF (ZERO(2)) THEN
         BY(1) = TVC(2) + PZERO(2) - IMW(2)
         BY(2) = BY(1)
         BX(1) = MAX (1, TVC(1) - 1)
         BX(2) = MIN (MAXXTV(1), TVC(3) + 1)
         CALL IMVECT ('ONNN', IGB, 2, BX, BY, TVSCR, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      IF (I.GT.1) THEN
         MSGTXT = 'Select a sub-image to edit'
         CALL MSGWRT (1)
         IGR = 3 + NGRAY
         CALL WIPBOX (IGR, TVC, IMW, TVSCR, IRET)
         IF (IRET.NE.0) GO TO 990
         GO TO 20
         END IF
C                                       build menu
      MGRS(2) = MIN (NGRAPH, 3)
      ISHELP = TSKNAM
 100  MGRS(1) = IGM - NGRAY
      IF (MENUOK) MGRS(1) = -MGRS(1)
      CALL COPY (3, TVZOOM, SVZOOM)
C                                       Init
      CALL YWINDO ('READ', WINDTV, IRET)
      IF (IRET.NE.0) THEN
         WINDTV(1) = 1
         WINDTV(2) = 1
         WINDTV(3) = MAXXTV(1)
         WINDTV(4) = MAXXTV(2)
         IRET = 0
         END IF
C                                       force window large enough
      IF ((TVC(1).LT.WINDTV(1)) .OR. (TVC(2).LT.WINDTV(2)) .OR.
     *   (TVC(3).GT.WINDTV(3)) .OR. (TVC(4).GT.WINDTV(4))) THEN
         WINDTV(1) = MIN (WINDTV(1), TVC(1))
         WINDTV(2) = MIN (WINDTV(2), TVC(2))
         WINDTV(3) = MAX (WINDTV(3), TVC(3))
         WINDTV(4) = MAX (WINDTV(4), TVC(4))
         CALL YWINDO ('WRIT', WINDTV, IRET)
         END IF
      CALL TVMENU (MTYP, MCOL, MROW, MGRS, TOPSEP, SIDSEP, ISHELP,
     *   CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHS, TVBUT, TVSCR, IRET)
      IF (IRET.NE.0) GO TO 990
      MENUOK = .TRUE.
C                                       Init
      CALL YWINDO ('READ', WINDTV, IRET)
      IF (IRET.NE.0) THEN
         WINDTV(1) = 1
         WINDTV(2) = 1
         WINDTV(3) = MAXXTV(1)
         WINDTV(4) = MAXXTV(2)
         IRET = 0
         END IF
C                                       force window large enough
      IF ((TVC(1).LT.WINDTV(1)) .OR. (TVC(2).LT.WINDTV(2)) .OR.
     *   (TVC(3).GT.WINDTV(3)) .OR. (TVC(4).GT.WINDTV(4))) THEN
         WINDTV(1) = MIN (WINDTV(1), TVC(1))
         WINDTV(2) = MIN (WINDTV(2), TVC(2))
         WINDTV(3) = MAX (WINDTV(3), TVC(3))
         WINDTV(4) = MAX (WINDTV(4), TVC(4))
         CALL YWINDO ('WRIT', WINDTV, IRET)
         END IF
C                                       Do something
      IF (CHOICS(CHS).EQ.'TVZOOM') THEN
         CALL TVZOME (IRET)
      ELSE IF (CHOICS(CHS).EQ.'OFF ZOOM') THEN
         TVZOOM(1) = 0
         TVZOOM(2) = MAXXTV(1)/2
         TVZOOM(3) = MAXXTV(2)/2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., IRET)
      ELSE IF (CHOICS(CHS).EQ.'SET WINDOW') THEN
         MSGTXT = 'Select a sub-image to edit'
         CALL MSGWRT (1)
         IGR = 3 + NGRAY
         CALL WIPBOX (IGR, TVC, IMW, TVSCR, IRET)
         IF (IRET.NE.0) GO TO 990
         TVZOOM(2) = MAXXTV(1)/2
         TVZOOM(3) = MAXXTV(2)/2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., IRET)
         IF (IRET.EQ.0) GO TO 20
      ELSE IF (CHOICS(CHS).EQ.'RESET WINDOW') THEN
         TVZOOM(1) = 0
         TVZOOM(2) = MAXXTV(1)/2
         TVZOOM(3) = MAXXTV(2)/2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., IRET)
         IF (IRET.EQ.0) GO TO 10
      ELSE IF (CHOICS(CHS)(:7).EQ.'WIPER S') THEN
         TEMP = REAL (TVCAT(IICOR+2) - TVCAT(IICOR)) /
     *      REAL (TVCAT(IIWIN+2) - TVCAT(IIWIN))
         I = TEMP + 0.1
         I = MAX (1, I)
         IF (CHOICS(CHS)(7:).EQ.'SIZE') THEN
            CALL WIPSIZ (BPARM(9), I, TVSCR, IRET)
         ELSE
            TVZOOM(1) = 0
            TVZOOM(2) = MAXXTV(1)/2
            TVZOOM(3) = MAXXTV(2)/2
            CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., IRET)
            IF (IRET.NE.0) GO TO 990
            CALL WIPSET (BPARM(9), I, TVSCR, IRET)
            CALL COPY (3, SVZOOM, TVZOOM)
            END IF
      ELSE IF (CHOICS(CHS).EQ.'EXIT') THEN
         GO TO 990
      ELSE IF (CHOICS(CHS).EQ.'ABORT') THEN
         MSGTXT = 'ABORT selected - any editing is discarded'
         CALL MSGWRT (6)
         IRET = -1
      ELSE IF ((CHOICS(CHS).EQ.'FLAG BASELIN') .OR.
     *   (CHOICS(CHS).EQ.'UNFLAG BASEL')) THEN
         MENUOK = .FALSE.
         ISN = 1
         IF (CHOICS(CHS)(:2).EQ.'UN') ISN = -1
         CALL FLAGBL (ISN, NX, NY, TVCORE, NBL, BLCORE, IRET)
         IF (IRET.EQ.0) GO TO 20
      ELSE
         MENUOK = .FALSE.
         ISN = 1
         IF (CHOICS(CHS)(:2).EQ.'UN') ISN = -1
         IP = 7 - ISN
         ITY = 0
         IF (CHOICS(CHS)(IP:).EQ.'POINT') ITY = 1
         IF (CHOICS(CHS)(IP:).EQ.'AREA') ITY = 2
         IF (CHOICS(CHS)(IP:).EQ.'FAST') ITY = 3
         IF (CHOICS(CHS)(IP:).EQ.'FAT') ITY = 4
         IF (ITY.GT.0) CALL WIPEIT (ITY, ISN, NX, NY, TVCORE, NBL,
     *      BLCORE, IRET)
         END IF
      IF (IRET.EQ.0) GO TO 100
C
 990  CALL YHOLD ('OFFF', ITY)
      CALL TVCLOS (SBUFF, ISN)
C
 999  RETURN
C-----------------------------------------------------------------------
 1025 FORMAT ('Loading image smoothed by',I2,' pixels')
 1026 FORMAT ('Loading image pixel replicated by',I2,' pixels')
      END
      SUBROUTINE FLAGBL (ISN, NX, NY, TVCORE, NBL, BLCORE, IRET)
C-----------------------------------------------------------------------
C   FLAGBL handles talking to the user about which baselines to flag
C   Inputs:
C      ISN      I          1 => flag, -1 => unflag
C      NX       I          X dimension of TVCORE
C      NY       I          Y dimension of TVCORE
C      NBL      I          Max # baselines in BLCORE
C   In/out:
C      TVCORE   I(*,*)     Plot as count of samples in cell - negative
C                          means flag.
C      BLCORE   I(*,*,*)   Baseline numbers in tv pixels
C   Outputs:
C      IRET     I          Error code
C-----------------------------------------------------------------------
      INTEGER   ISN, NX, NY, TVCORE(NX,NY), NBL, BLCORE(NBL,NX,NY), IRET
C
      INCLUDE 'WIPER.INC'
      CHARACTER MESSAG*72
      INTEGER   LTEMP(2), IA, JA, I1, I2, N, LT, IBL, I, IX, IY, NN, J
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (ISN.GT.0) THEN
         MESSAG = 'Enter baseline to flag (2 integers)'
      ELSE
         MESSAG = 'Enter baseline to unflag (2 integers)'
         END IF
      LTEMP(1) = 0
      LTEMP(2) = 0
      CALL INQINT (TTY, MESSAG, 2, LTEMP, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
      ELSE IF (IRET.EQ.0) THEN
         N = 0
         NN = 0
         IA = MIN (LTEMP(1), LTEMP(2))
         JA = MAX (LTEMP(1), LTEMP(2))
         IF (JA.LE.0) GO TO 999
         IF (IA.LE.0) THEN
            I1 = 1
            I2 = MANT
         ELSE
            I1 = IA
            I2 = IA
            END IF
         DO 100 IA = I1,I2
            IF ((ISN.GT.0) .AND. (BLFLAG(IA,JA).EQ.0)) THEN
               N = N + 1
               BLFLAG(IA,JA) = 1
               BLFLAG(JA,IA) = 1
               LT = 1000 * MIN (IA,JA) + MAX (IA,JA)
               DO 30 IY = 1,NY
                  DO 20 IX = 1,NX
                     IF (TVCORE(IX,IY).NE.0) THEN
                        I = 0
                        J = 0
                        DO 10 IBL = 1,MBL
                           IF (BLCORE(IBL,IX,IY).EQ.LT) THEN
                              BLCORE(IBL,IX,IY) = -BLCORE(IBL,IX,IY)
                              J = J + 1
                              END IF
                           IF (BLCORE(IBL,IX,IY).GT.0) I = I + 1
 10                        CONTINUE
                        IF ((I.EQ.0) .AND. (J.GT.0)) THEN
                           NN = NN + 1
                           TVCORE(IX,IY) = -ABS (TVCORE(IX,IY))
                           END IF
                        END IF
 20                  CONTINUE
 30               CONTINUE
            ELSE IF ((ISN.LE.0) .AND. (BLFLAG(IA,JA).EQ.1)) THEN
               N = N + 1
               BLFLAG(IA,JA) = 0
               BLFLAG(JA,IA) = 0
               LT = 1000 * MIN (IA,JA) + MAX (IA,JA)
               DO 60 IY = 1,NY
                  DO 50 IX = 1,NX
                     IF (TVCORE(IX,IY).NE.0) THEN
                        I = 0
                        J = 0
                        DO 40 IBL = 1,MBL
                           IF (BLCORE(IBL,IX,IY).EQ.-LT) THEN
                              J = J + 1
                              BLCORE(IBL,IX,IY) = -BLCORE(IBL,IX,IY)
                              END IF
                           IF (BLCORE(IBL,IX,IY).GT.0) I = I + 1
 40                        CONTINUE
                        IF ((I.GT.0) .AND. (J.GT.0) .AND.
     *                     (TVCORE(IX,IY).LT.0)) THEN
                           NN = NN + 1
                           TVCORE(IX,IY) = ABS (TVCORE(IX,IY))
                           END IF
                        END IF
 50                  CONTINUE
 60               CONTINUE
               END IF
 100        CONTINUE
         WRITE (MSGTXT,1100) N, NN
         CALL MSGWRT (3)
         END IF
      IRET = MAX (IRET, 0)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I4,' ASKING FOR NUMBERS')
 1100 FORMAT ('Changed',I5,' baselines and',I9,' displayed points')
      END
      SUBROUTINE WIPEUV (NX, NY, TVCORE, NA, NI, NP, XZY, MALL, MBAD,
     *   MSOM, IRET)
C-----------------------------------------------------------------------
C   WIPEUV replots uv data, compares with TVCORE, and writes flag info.
C   Inputs:
C      NX       I        X dimension of TVCORE
C      NY       I        Y dimension of TVCORE
C      NA      I      Number of antennas
C      NI      I      Number of IFs
C   In/out:
C      TVCORE   I(*,*)   Plot as count of samples in cell - negative
C                        means flag.  Removes counts as they occur so as
C                        to confirm correct usage.
C   Output:
C      MALL    I(*)   Counts samples by baseline etc
C      MBAD    I(*)   Counts full bad spectra by baseline
C      MSOM    I(*)   Counts partly bad spectra by baseline
C      IRET     I        Return code, 0 => OK, otherwise abort.
C                           4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NP, TVCORE(NX,NY), NA, NI, MALL(NA,NA,NP,NI),
     *   MBAD(NA,NA,NP,NI), MSOM(NA,NA,NP,NI), IRET
      REAL      XZY(3,NP,*)
C
      INCLUDE 'WIPER.INC'
      CHARACTER ATIME*8, ADATE*12, REASON*24, BNDCOD(MAXIF)*8
      INTEGER   I, IC, J, JJJ, ICO, NUMVIS, JSUB, ISUB, NXLUN, NIF,
     *   NXVER, LC, LF, IX, IY, NREV, DOUNT(3,3), LUN, LFGRNO, IA1, IA2,
     *   LBIF, LEIF, LBCH, LECH, TIME(3), DATE(3), FGBUFL(512), NIN,
     *   NBAD, LP, DIDALL(MAXANT), JBCH, JECH
      REAL      BLC(2), TRC(2), XY(2), BTIME, ETIME, TEMP
      HOLLERITH CATH(256)
      LOGICAL   REQBAS, REQAS, FLAGDO, PFLAGS(4,4), FLAGED, AFLAGS(4),
     *   GOTONE
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (CATBLK, CATH)
      DATA LUN /48/
      DATA AFLAGS /4*.TRUE./
C-----------------------------------------------------------------------
      GOTONE = .FALSE.
      LBCH = 1
      LECH = 0
      JBCH = 1
      JECH = 0
      LBIF = 1
      LEIF = 0
      NREV = 1
      IF (UVREV) NREV = 2
      FLAGED = .FALSE.
      IRET = 1
      CALL FILL (9, 0, DOUNT)
      REQAS = ((TYPEAX(1).GE.14) .AND. (TYPEAX(1).LE.16)) .OR.
     * ((TYPEAX(2).GE.14) .AND. (TYPEAX(2).LE.16))
C                                      Set Stokes flag
      CALL LFILL (16, .TRUE., PFLAGS)
      IF (ICOR0.NE.LCOR0) ONEPOL = .FALSE.
      IF ((ICOR0.LT.0) .AND. (LCOR0.LT.0)) THEN
         IF (ONEPOL) THEN
            IF (STOKES.EQ.'RR') PFLAGS(2,1) = .FALSE.
            IF (STOKES.EQ.'LL') PFLAGS(1,1) = .FALSE.
            END IF
         IF (NOTCRS) THEN
            PFLAGS(3,1) = .FALSE.
            PFLAGS(4,1) = .FALSE.
            PFLAGS(3,2) = .FALSE.
            PFLAGS(4,2) = .FALSE.
            END IF
         IF (STOKES.EQ.'RL') THEN
            PFLAGS(1,1) = .FALSE.
            PFLAGS(2,1) = .FALSE.
            PFLAGS(4,1) = .FALSE.
         ELSE IF (STOKES.EQ.'LR') THEN
            PFLAGS(1,1) = .FALSE.
            PFLAGS(2,1) = .FALSE.
            PFLAGS(3,1) = .FALSE.
         ELSE IF ((STOKES.EQ.'RLLR') .OR. (STOKES.EQ.'CROS')) THEN
            PFLAGS(1,1) = .FALSE.
            PFLAGS(2,1) = .FALSE.
            PFLAGS(4,1) = .FALSE.
            PFLAGS(1,2) = .FALSE.
            PFLAGS(2,2) = .FALSE.
            PFLAGS(3,2) = .FALSE.
         ELSE IF ((STOKES.EQ.'HALF') .OR. (STOKES.EQ.'RRLL')) THEN
            IF (ONEPOL) PFLAGS(2,1) = .FALSE.
            IF (ONEPOL) PFLAGS(1,2) = .FALSE.
         ELSE IF ((STOKES.EQ.'FULL') .OR. (STOKES.EQ.'RLRL')) THEN
            IF (ONEPOL) PFLAGS(2,1) = .FALSE.
            IF (ONEPOL) PFLAGS(1,2) = .FALSE.
            PFLAGS(1,3) = .FALSE.
            PFLAGS(2,3) = .FALSE.
            PFLAGS(4,3) = .FALSE.
            PFLAGS(1,4) = .FALSE.
            PFLAGS(2,4) = .FALSE.
            PFLAGS(3,4) = .FALSE.
            END IF
      ELSE IF ((ICOR0.GT.0) .AND. (LCOR0.GT.0)) THEN
         IF ((STOKES.EQ.'Q') .OR. (STOKES.EQ.'U')) THEN
            PFLAGS(1,1) = .FALSE.
            PFLAGS(4,1) = .FALSE.
         ELSE IF ((STOKES.EQ.'IQU') .OR. (STOKES.EQ.'IQUV')) THEN
            PFLAGS(1,2) = .FALSE.
            PFLAGS(4,2) = .FALSE.
            PFLAGS(1,3) = .FALSE.
            PFLAGS(4,3) = .FALSE.
            IF (NOTCRS) THEN
               PFLAGS(2,1) = .FALSE.
               PFLAGS(3,1) = .FALSE.
               PFLAGS(2,4) = .FALSE.
               PFLAGS(3,4) = .FALSE.
               END IF
            END IF
      ELSE IF ((ICOR0.GT.0) .AND. (LCOR0.LT.0)) THEN
         IF ((STOKES.EQ.'Q') .OR. (STOKES.EQ.'U')) THEN
            PFLAGS(1,1) = .FALSE.
            PFLAGS(2,1) = .FALSE.
         ELSE IF ((STOKES.EQ.'IQU') .OR. (STOKES.EQ.'IQUV')) THEN
            PFLAGS(1,2) = .FALSE.
            PFLAGS(2,2) = .FALSE.
            PFLAGS(1,3) = .FALSE.
            PFLAGS(2,3) = .FALSE.
            IF (NOTCRS) THEN
               PFLAGS(3,1) = .FALSE.
               PFLAGS(4,1) = .FALSE.
               PFLAGS(3,4) = .FALSE.
               PFLAGS(4,4) = .FALSE.
               END IF
            END IF
      ELSE IF ((ICOR0.LT.0) .AND. (LCOR0.GT.0)) THEN
         IF ((STOKES.EQ.'RL') .OR. (STOKES.EQ.'LR') .OR.
     *      (STOKES.EQ.'RLLR') .OR. (STOKES.EQ.'CROS')) THEN
            PFLAGS(1,1) = .FALSE.
            PFLAGS(4,1) = .FALSE.
            PFLAGS(1,2) = .FALSE.
            PFLAGS(4,2) = .FALSE.
         ELSE IF ((STOKES.EQ.'FULL') .OR. (STOKES.EQ.'RLRL')) THEN
            PFLAGS(1,3) = .FALSE.
            PFLAGS(4,3) = .FALSE.
            PFLAGS(1,4) = .FALSE.
            PFLAGS(4,4) = .FALSE.
            IF (NOTCRS) THEN
               PFLAGS(2,1) = .FALSE.
               PFLAGS(3,1) = .FALSE.
               PFLAGS(2,2) = .FALSE.
               PFLAGS(3,2) = .FALSE.
               END IF
            END IF
         END IF
C                                       Initialize UV reading
      ISUVR = (UVRNG(1).GE.0.0) .AND. (UVRNG(2).GT.UVRNG(1))
      IF ((UVRNG(1).EQ.0.0) .AND. (UVRNG(2).GE.1.E10)) ISUVR = .FALSE.
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = NX - 0.001
      TRC(2) = NY - 0.001
      IRET = 3
      NUMVIS = 0
C                                       Set flagging reason
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      REASON = TSKNAM // ADATE // ' ' // ATIME(:5)
C                                       Loop for each FREQID.
      NXLUN = 100
      NXVER = 1
      JSUB = SUBARR
      DO 160 IFRQ = 1,NFRQ
         IF (NFRQ.GT.1) FRQSEL = IFRQ
         CALL CHNDAT ('READ', NXBUFF, DISKIN, CNOIN, NXVER, CATUV,
     *      NXLUN, NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'PROBLEM FINDING FREQUENCIES'
            CALL MSGWRT (6)
            GO TO 160
            END IF
         DO 155 ISUB = 1,NSUBA
            IF (EXCLFQ(ISUB,IFRQ).NE.0) GO TO 155
            IF (JSUB.EQ.0) SUBARR = ISUB
            IF (REQAS) THEN
               CALL GETANT (DISKIN, CNOIN, SUBARR, CATUV, SBUFF, IRET)
               IF (IRET.NE.0) THEN
                  EXCLFQ(ISUB,IFRQ) = 1
                  MSGTXT = 'PROBLEM FINDING ANTENNA INFO'
                  CALL MSGWRT (6)
                  GO TO 155
                  END IF
               END IF
C                                       Initialize UV reading.
            CALL UVGET ('INIT', RPARM, BUFF1, IRET)
C
            IF (IRET.EQ.-1) GO TO 140
            IF (IRET.EQ.5) GO TO 140
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT, 1050) IRET
               CALL MSGWRT (8)
               IRET = 4
               GO TO 999
               END IF
            NPOL = NCOR
C                                       Loop: Read vis. record.
 100        CALL UVGET ('READ', RPARM, BUFF1, IRET)
               IF (IRET.EQ.-1) GO TO 140
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) IRET
                  CALL MSGWRT (8)
                  IRET = 4
                  GO TO 999
                  END IF
C                                       Check whether we need this
C                                       baseline
               IF (ILOCB.GE.0) THEN
                  IA1 = INT (RPARM(ILOCB+1)) / 256
                  IA2 = MOD (INT (RPARM(ILOCB+1)), 256)
               ELSE
                  IA1 = RPARM(ILOCA1+1) + 0.1
                  IA2 = RPARM(ILOCA2+1) + 0.1
                  END IF
               IF (.NOT.REQBAS (IA1, IA2, DESEL, IANT, NANT, IBAS,
     *            NBAS)) GO TO 100
               NUMVIS = NUMVIS + 1
               IF ((REQAS) .AND. (CURSOU.NE.CSOU)) THEN
                  CSOU = CURSOU
                  CALL GETSOU (CSOU, DISKIN, CNOIN, CATUV, NXLUN, IRET)
                  IF (IRET.NE.0) THEN
                     MSGTXT = 'TROUBLE GETTING SOURCE INFO'
                     CALL MSGWRT (6)
                     END IF
                  END IF
C                                       Get the time, set interval
               BTIME = RPARM(1+ILOCT) - 1.E-6
               ETIME = RPARM(1+ILOCT) + 1.E-6
C                                       Get and scale X, Y
               CALL FNDXY (RPARM, BUFF1, NP, XZY)
               ICO = ECHAN - BCHAN + 1
               LC = 0
               DO 135 LF = 1,NI
                  FLAGDO = .TRUE.
                  IF (ONEIF) THEN
                     LBIF = LF + BIF - 1
                     LEIF = LF + BIF - 1
                     END IF
                  DO 131 LP = 1,NPOL
                     NIN = 0
                     NBAD = 0
                     MALL(IA1,IA2,LP,LF) = MALL(IA1,IA2,LP,LF) + 1
                     LC = (LF - 1) * ((ICO-1)/CHINC + 1)
                     DO 130 IC = 1,ICO,CHINC
                        LC = LC + 1
                        IF ((XZY(1,LP,LC).NE.FBLANK) .AND.
     *                     (XZY(2,LP,LC).NE.FBLANK) .AND.
     *                     (XZY(3,LP,LC).GT.0.0)) THEN
                           IF ((SPHASE) .AND. (IPHASE.GT.0)) THEN
                              TEMP = XZY(IPHASE,LP,LC)
                              IF (TEMP.LT.0) TEMP = TEMP + 360.0
                              XZY(IPHASE,LP,LC) = TEMP
                              END IF
                           NIN = NIN + CHINC
                           IF (ONECHN) THEN
                              LBCH = IC + BCHAN -1
                              LECH = LBCH + CHINC - 1
                              END IF
                           DO 128 JJJ = 1,NREV
                              DO 120 J = 1,2
                                 XY(J) = XYSCL(J) *
     *                              (XZY(J,LP,LC)-XYOFF(J))
 120                             CONTINUE
C                                       Mark the point
                              IX = XY(1) + 1.0
                              IY = XY(2) + 1.0
                              IF (IX.LT.1) THEN
                                 IF (IY.GT.NY) THEN
                                    DOUNT(1,1) = DOUNT(1,1) + 1
                                 ELSE IF (IY.GT.0) THEN
                                    DOUNT(1,2) = DOUNT(1,2) + 1
                                 ELSE
                                    DOUNT(1,3) = DOUNT(1,3) + 1
                                    END IF
                              ELSE IF (IX.GT.NX) THEN
                                 IF (IY.GT.NY) THEN
                                    DOUNT(3,1) = DOUNT(3,1) + 1
                                 ELSE IF (IY.GT.0) THEN
                                    DOUNT(3,2) = DOUNT(3,2) + 1
                                 ELSE
                                    DOUNT(3,3) = DOUNT(3,3) + 1
                                    END IF
                              ELSE
                                 IF (IY.GT.NY) THEN
                                    DOUNT(2,1) = DOUNT(2,1) + 1
                                 ELSE IF (IY.GT.0) THEN
                                    DOUNT(2,2) = DOUNT(2,2) + 1
C                                       ALL the action: no flag
                                    IF (TVCORE(IX,IY).GE.0) THEN
                                       IF (TVCORE(IX,IY).GT.1) THEN
                                          TVCORE(IX,IY) = TVCORE(IX,IY)
     *                                       - 1
                                       ELSE IF (TVCORE(IX,IY).GE.0) THEN
                                          TVCORE(IX,IY) = 1000000000
                                          END IF
                                       IF (GOTONE) THEN
                                          NFGWRI = NFGWRI + 1
                                          CALL FLAGIT ('FLAG', LUN,
     *                                       DISKIN, CNOIN, FGVERI,
     *                                       FGVERO, LFGRNO, FGKOLS,
     *                                       FGNUMV, CURSOU, SUBARR,
     *                                       IFRQ, IA1, IA2, BTIME,
     *                                       ETIME, LBIF, LEIF, JBCH,
     *                                       JECH, PFLAGS(1,LP), REASON,
     *                                       CATUV, FGBUFL, IRET)
                                          IF (IRET.NE.0) GO TO 999
                                          GOTONE = .FALSE.
                                          END IF
C                                       flagging this point
                                    ELSE
                                       IF (TVCORE(IX,IY).LT.-1) THEN
                                          TVCORE(IX,IY) = TVCORE(IX,IY)
     *                                       + 1
                                       ELSE
                                          TVCORE(IX,IY) = -1000000000
                                          END IF
C                                       flag something
                                       IF (FLAGDO) THEN
                                          IF (ONECHN) THEN
                                             NBAD = NBAD + NCHAV
                                          ELSE
                                             NBAD = NBAD + 32768
                                             END IF
                                          FLAGED = .TRUE.
                                          IF (BLFLAG(IA1,IA2).EQ.0) THEN
                                             IF (.NOT.GOTONE) JBCH=LBCH
                                             JECH = LECH
                                             GOTONE = .TRUE.
                                             END IF
                                          FLAGDO = ONECHN
                                          END IF
                                       END IF
                                 ELSE
                                    DOUNT(2,3) = DOUNT(2,3) + 1
                                    END IF
                                 END IF
                              XZY(1,LP,LC) = -XZY(1,LP,LC)
                              XZY(2,LP,LC) = -XZY(2,LP,LC)
 128                          CONTINUE
                           END IF
 130                    CONTINUE
                     IF ((NBAD.GE.NIN) .AND. (NBAD.GT.0)) THEN
                        MBAD(IA1,IA2,LP,LF) = MBAD(IA1,IA2,LP,LF) + 1
                        NBDCOR = NBDCOR + NCHAN
                     ELSE IF (NBAD.GT.0) THEN
                        MSOM(IA1,IA2,LP,LF) = MSOM(IA1,IA2,LP,LF) + 1
                        NBDCOR = NBDCOR + NBAD
                        END IF
                     IF (GOTONE) THEN
                        NFGWRI = NFGWRI + 1
                        CALL FLAGIT ('FLAG', LUN, DISKIN, CNOIN, FGVERI,
     *                     FGVERO, LFGRNO, FGKOLS, FGNUMV, CURSOU,
     *                     SUBARR, IFRQ, IA1, IA2, BTIME, ETIME, LBIF,
     *                     LEIF, JBCH, JECH, PFLAGS(1,LP), REASON,
     *                     CATUV, FGBUFL, IRET)
                        IF (IRET.NE.0) GO TO 999
                        GOTONE = .FALSE.
                        END IF
 131                 CONTINUE
 135              CONTINUE
               GO TO 100
 140        CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
C                                       baseline flags
            LBCH = 1
            LECH = 0
            LBIF = BIF
            LEIF = EIF
            CALL FILL (MANT, 0, DIDALL)
            BTIME = TSTART
            ETIME = TEND
            DO 145 I = 1,MANT
               JJJ = 0
               DO 144 J = 1,MANT
                  IF (BLFLAG(I,J).GT.0) JJJ = JJJ + 1
 144              CONTINUE
               IF (JJJ.EQ.MANT) THEN
                  DIDALL(I) = 1
                  NFGWRI = NFGWRI + 1
                  FLAGED = .TRUE.
                  CALL FLAGIT ('FLAG', LUN, DISKIN, CNOIN, FGVERI,
     *               FGVERO, LFGRNO, FGKOLS, FGNUMV, CURSOU, SUBARR,
     *               IFRQ, I, 0, BTIME, ETIME, LBIF, LEIF, LBCH, LECH,
     *               AFLAGS, REASON, CATUV, FGBUFL, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
 145           CONTINUE
            DO 150 I = 1,MANT
               DO 146 J = I,MANT
                  IF ((BLFLAG(I,J).GT.0) .AND. (DIDALL(I).LE.0) .AND.
     *               (DIDALL(J).LE.0)) THEN
                     NFGWRI = NFGWRI + 1
                     FLAGED = .TRUE.
                     CALL FLAGIT ('FLAG', LUN, DISKIN, CNOIN, FGVERI,
     *                  FGVERO, LFGRNO, FGKOLS, FGNUMV, CURSOU, SUBARR,
     *                  IFRQ, I, J, BTIME, ETIME, LBIF, LEIF, LBCH,
     *                  LECH, AFLAGS, REASON, CATUV, FGBUFL, IRET)
                     IF (IRET.NE.0) GO TO 999
                     END IF
 146              CONTINUE
 150           CONTINUE
C                                       close/save flags
            IF (FLAGED) CALL FLAGIT ('CLOS', LUN, DISKIN, CNOIN, FGVERI,
     *         FGVERO, LFGRNO, FGKOLS, FGNUMV, CURSOU, SUBARR, IFRQ,
     *         IA1, IA2, BTIME, ETIME, LBIF, LEIF, LBCH, LECH, PFLAGS,
     *         REASON, CATUV, FGBUFL, IRET)
 155        CONTINUE
 160     CONTINUE
      SUBARR = JSUB
C                                       Done
      IRET = 0
      JJJ = 0
      DO 170 I = 1,3
         DO 165 J = 1,3
            DOUNT(J,I) = DOUNT(J,I) - COUNT(J,I)
            JJJ = JJJ + ABS (DOUNT(J,I))
 165        CONTINUE
 170     CONTINUE
C                                       points off plot
      IF (JJJ.GT.0) THEN
         WRITE (MSGTXT,1150) DOUNT(2,2)
         CALL MSGWRT (2)
         WRITE (MSGTXT,1155) JJJ
         IF (JJJ.NE.DOUNT(2,2)) CALL MSGWRT (2)
         MSGTXT = 'DIFFERENCE IMAGE OF THE POINTS'' LOCATION WRT IMAGE'
         CALL MSGWRT (2)
         WRITE (MSGTXT,1160) DOUNT(1,1), DOUNT(2,1), DOUNT(3,1)
         CALL MSGWRT (2)
         WRITE (MSGTXT,1160) DOUNT(1,2), DOUNT(2,2), DOUNT(3,2)
         CALL MSGWRT (2)
         WRITE (MSGTXT,1160) DOUNT(1,3), DOUNT(2,3), DOUNT(3,3)
         CALL MSGWRT (2)
         END IF
C                                       Check image itself
      JJJ = 0
      DO 180 J = 1,NY
         DO 175 I = 1,NX
            IF (ABS(TVCORE(I,J)).GT.1) THEN
               JJJ = JJJ + 1
               END IF
 175        CONTINUE
 180     CONTINUE
      IF (JJJ.GT.0) THEN
         WRITE (MSGTXT,1170) JJJ
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('WIPEUV: ERROR',I3,' INIT VIS FILE')
 1100 FORMAT ('WIPEUV: ERROR',I3,' READING VIS FILE')
 1150 FORMAT ('WIPEUV: ',I10,' DIFFERENT POINTS FELL IN ARRAY')
 1155 FORMAT ('WIPEUV: ',I10,' DIFFERENT POINTS DID NOT FIT')
 1160 FORMAT ('WIPEUV: ',I10,' * ',I10,' * ',I10)
 1170 FORMAT ('WIPEUV: ',I10,' PIXELS NOT PROPERLY ACCOUNTED FOR')
      END
      SUBROUTINE XYSCAL (NUMVIS, NP, XZY, NGOD, IRET)
C-----------------------------------------------------------------------
C   XYSCAL finds the scaling parameters needed to fit X and Y
C   into a 1000*1000 plotting area .
C   Inputs:
C      NUMVIS   I        Visibility number, -1 => final call, no data
C                        passed -> change to scaling factor from max/min
C      NP       I        Number pol in XZY
C      XZY      R(3,NP,*)   plotted parameters: 1 X, 2 Y, 3 weight
C   Outputs:
C      NGOD     I        Number good samples
C      IRET     I        Error return code , non-zero if error .
C   Outputs (common):
C      XYOFF    R(2)     when added to XZY changes minimum to zero .
C      XYSCL    R(2)     scale XZY so that maximum is 1000.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, NP, NGOD
      REAL      XZY(3,NP,*)
C
      INTEGER   IRET, I, IC, ICO, LF, LC, J, LP
      REAL      TEMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'WIPER.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Get the data limits.
C                                       Will reset to BPARMS where
C                                       requested later in plot routine.
      NGOD = 0
      IF (NUMVIS.GE.0) THEN
         IRET = -1
         ICO = ECHAN - BCHAN + 1
         LC = 0
         DO 50 LF = BIF,EIF
            DO 40 IC = 1,ICO,CHINC
               LC = LC + 1
               DO 30 LP = 1,NPOL
                  IF ((XZY(1,LP,LC).NE.FBLANK) .AND.
     *               (XZY(2,LP,LC).NE.FBLANK)) THEN
                     IRET = 0
                     NGOD = NGOD + 1
C                                       Find max, min from data
C                                       For autoscaling within range
C                                       (bparm(3)<0), don't look at
C                                       all points.
                     DO 20 I = 1,2
                        J = 2*I+2
                        TEMP = XZY(I,LP,LC)
                        IF ((FIXSCL.GE.0) .OR. (BPARM(J).GE.BPARM(J+1))
     *                     .OR. (BPARM(J).LE.TEMP)) THEN
                           IF (TEMP.LT.XYMIN(I)) XYMIN(I) = TEMP
                           END IF
                        IF ((FIXSCL.GE.0) .OR. (BPARM(J).GE.BPARM(J+1))
     *                     .OR. (BPARM(J+1).GE.TEMP)) THEN
                           IF (TEMP.GT.XYMAX(I)) XYMAX(I) = TEMP
                           END IF
 20                     CONTINUE
                     IF (IPHASE.GT.0) THEN
                        J = 2*IPHASE+2
                        TEMP = XZY(IPHASE,LP,LC)
                        IF (TEMP.LT.0.0) TEMP = TEMP + 360.0
                        IF ((FIXSCL.GE.0) .OR. (BPARM(J).GE.BPARM(J+1))
     *                     .OR. (BPARM(J).LE.TEMP)) THEN
                           IF (TEMP.LT.XYMIN(3)) XYMIN(3) = TEMP
                           END IF
                        IF ((FIXSCL.GE.0) .OR. (BPARM(J).GE.BPARM(J+1))
     *                     .OR. (BPARM(J+1).GE.TEMP)) THEN
                           IF (TEMP.GT.XYMAX(3)) XYMAX(3) = TEMP
                           END IF
                        END IF
                     END IF
 30               CONTINUE
 40            CONTINUE
 50         CONTINUE
C                                       Last call:
      ELSE
         DO 120 I = 1,2
            IF (XYMAX(I).LE.XYMIN(I)) GO TO 980
C                                       Deal with U,V,W axes
            IF (UVREV) THEN
               XYMAX(I) = MAX (ABS(XYMAX(I)), ABS(XYMIN(I)))
               XYMIN(I) = -XYMAX(I)
               END IF
            IF ((XYMIN(I).GT.0.0) .AND. (XYMIN(I).LT.0.15*XYMAX(I)))
     *         XYMIN(I) = 0.0
 120        CONTINUE
         IF ((XYMIN(3).GT.0.0) .AND. (XYMIN(3).LT.0.15*XYMAX(3)))
     *      XYMIN(3) = 0.0
         IF ((FIXSCL.EQ.2) .AND. (TYPEAX(1).GE.6) .AND. (TYPEAX(1).LE.8)
     *      .AND. (TYPEAX(2).GE.6) .AND. (TYPEAX(2).LE.8)) THEN
            XYMAX(1) = MAX (XYMAX(1), XYMAX(2))
            XYMAX(2) = XYMAX(1)
            XYMIN(1) = MIN (XYMIN(1), XYMIN(2))
            XYMIN(2) = XYMIN(1)
            END IF
         END IF
      GO TO 999
C
 980  IRET = 1
      WRITE (MSGTXT,1980) I
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('XYSCAL: AXIS',I2,' DEGENERATE')
      END
      SUBROUTINE FNDXY (RANDP, BUFR, NP, XZY)
C-----------------------------------------------------------------------
C   FNDXY extracts the desired X and Y values from the Vis record.
C   Inputs:
C      RANDP   R(*)     Random parameters
C      BUFR    R(*)     Visibility record
C      NP      I        number pol in XZY
C   Outputs:
C      XZY      R(3,NP,*)   X, Y values and weight
C-----------------------------------------------------------------------
      INTEGER   NP
      REAL      RANDP(*), BUFR(*), XZY(3,NP,*)
C
      REAL      TR, TI, CATUVR(256), WT, H1, E1, H2, E2, RADPA, A1, A2,
     *   S1, S2, C1, C2
      INTEGER   I, J, LAD, IC, ICO, IA1, IA2, LF, LC, LP, LL, L
      DOUBLE PRECISION FRQMUL, CATUVD(128), FZ, FI, TT
      INCLUDE 'WIPER.INC'
      REAL      PA(MAXANT)
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATUV, CATUVD, CATUVR)
C-----------------------------------------------------------------------
      IF (ILOCB.GE.0) THEN
         IA1 = RANDP(ILOCB+1) / 256. + 0.1
         IA2 = RPARM(ILOCB+1) - 256. * IA1 + 0.1
      ELSE
         IA1 = RANDP(ILOCA1+1) + 0.1
         IA2 = RANDP(ILOCA2+1) + 0.1
         END IF
C                                       Loop over channels
      ICO = ECHAN - BCHAN + 1
      LC = 0
      FRQMUL = 1.0D0
      DO 930 LF = BIF,EIF
         FZ = FOFF(LF) / UVFREQ + 1.0D0
         FI = FINC(LF) / UVFREQ
         DO 920 IC = 1,ICO,CHINC
            LC = LC + 1
            IF (TYPUVD.LE.0) FRQMUL = FZ + FI * (IC - 1 + BCHAN +
     *         (NCHAV-1.0)/2.0 - CATUVR(KRCRP+KLOCFY))
            DO 910 LP = 1,NPOL
               LAD = 1 + (IC-1)*INCF + (LF-BIF)*INCIF + (LP-1)*INCS
               WT = BUFR(LAD+2)
C                                       Loop over axes
               DO 900 I = 1,2
                  TR = 0.0
                  TI = 0.0
                  XZY(I,LP,LC) = FBLANK
                  J = TYPEAX(I)
C                                       amplitudes and phases
                  IF ((J.EQ.1) .OR. (J.EQ.2) .OR. (J.EQ.9) .OR.
     *               (J.EQ.10) .OR. (J.EQ.12) .OR. (J.EQ.13)) THEN
C                                       average channels
                     IF (NCHAV.GT.1) THEN
                        WT = 0.0
                        TR = 0.0
                        TI = 0.0
                        LL = LAD
                        DO 110 L = 1,NCHAV
                           IF (BUFR(LL+2).GT.0.0) THEN
                              WT = WT + BUFR(LL+2)
                              TR = TR + BUFR(LL+2)* BUFR(LL)
                              TI = TI + BUFR(LL+2)* BUFR(LL+1)
                              END IF
                           LL = LL + INCF
 110                       CONTINUE
                        IF (WT.GT.0.0) THEN
                           TR = TR / WT
                           TI = TI / WT
                           END IF
                     ELSE
                        TR = BUFR(LAD)
                        TI = BUFR(LAD+1)
                        END IF
                     IF (WT.LE.0.0) GO TO 900
C                                       amplitude
                     IF (J.EQ.1) THEN
                        IF ((IA1.EQ.IA2) .AND. (.NOT.ISCROS(LP))) THEN
                           XZY(I,LP,LC) = TR
                        ELSE
                           XZY(I,LP,LC) = SQRT (TR*TR + TI*TI)
                           END IF
C                                       log (ampl)
                     ELSE IF (J.EQ.12) THEN
                        XZY(I,LP,LC) = SQRT (TR*TR + TI*TI)
                        XZY(I,LP,LC) = LOG (MAX (1.E-12, XZY(I,LP,LC)))
C                                       phase
                     ELSE IF (J.EQ.2) THEN
                        IF ((TI.NE.0.0) .OR. (TR.NE.0.0)) XZY(I,LP,LC) =
     *                     RAD2DG * ATAN2 (TI, TR)
C                                       Real , Imag , Weight parts
                     ELSE IF (J.EQ.9) THEN
                        XZY(I,LP,LC) = TR
                     ELSE IF (J.EQ.10) THEN
                        XZY(I,LP,LC) = TI
                     ELSE IF (J.EQ.13) THEN
                        XZY(I,LP,LC) = WT
                        END IF
C                                       U, V distance
                  ELSE IF (J.EQ.3) THEN
                     XZY(I,LP,LC) = SQRT (RANDP(1+ILOCU)**2 +
     *                  RANDP(1+ILOCV)**2) * FRQMUL
C                                       U, V position angle
                  ELSE IF (J.EQ.4) THEN
                     TR = RANDP(1+ILOCU)
                     TI = RANDP(1+ILOCV)
                     IF ((TI.NE.0.0) .OR. (TR.NE.0.0)) XZY(I,LP,LC) =
     *                  RAD2DG * ATAN2 (TI, TR)
C                                       Time
C                                       Time (hours)
                  ELSE IF ((J.EQ.5) .OR. (J.EQ.11)) THEN
                     XZY(I,LP,LC) = RANDP(1+ILOCT)
C                                       U projected spacing
                  ELSE IF (J.EQ.6) THEN
                     XZY(I,LP,LC) = RANDP(1+ILOCU) * FRQMUL
C                                       V projected spacing
                  ELSE IF (J.EQ.7) THEN
                     XZY(I,LP,LC) = RANDP(1+ILOCV) * FRQMUL
C                                       W projected spacing
                  ELSE IF (J.EQ.8) THEN
                     XZY(I,LP,LC) = RANDP(1+ILOCW) * FRQMUL
C                                       HA, Elevation
                  ELSE IF ((J.EQ.14) .OR. (J.EQ.15) .OR. (J.EQ.18)) THEN
                     TT = RANDP(1+ILOCT)
                     IF (REFANT.GT.0) THEN
                        CALL SOUELV (REFANT, TT, H1, E1, A1)
                        IF (J.EQ.14) THEN
                           XZY(I,LP,LC) = H1 * RAD2DG / 15.0
                        ELSE IF (J.EQ.18) THEN
                           XZY(I,LP,LC) = A1 * RAD2DG
                        ELSE
                           XZY(I,LP,LC) = E1 * RAD2DG
                           END IF
                     ELSE
                        CALL SOUELV (IA1, TT, H1, E1, A1)
                        CALL SOUELV (IA2, TT, H2, E2, A2)
                        IF (J.EQ.14) THEN
                           S1 = SIN (H1)
                           S2 = SIN (H2)
                           C1 = COS (H1)
                           C2 = COS (H2)
                           XZY(I,LP,LC) = ATAN2 (S1+S2, C1+C2) *
     *                        RAD2DG / 15.
                        ELSE IF (J.EQ.18) THEN
                           S1 = SIN (A1)
                           S2 = SIN (A2)
                           C1 = COS (A1)
                           C2 = COS (A2)
                           XZY(I,LP,LC) = ATAN2 (S1+S2, C1+C2) * RAD2DG
                        ELSE
                           S1 = SIN (E1)
                           S2 = SIN (E2)
                           C1 = COS (E1)
                           C2 = COS (E2)
                           XZY(I,LP,LC) = ATAN2 (S1+S2, C1+C2) * RAD2DG
                           END IF
                        END IF
C                                       Parallactic angle
                  ELSE IF (J.EQ.16) THEN
                     H1 = RANDP(1+ILOCT)
                     CALL PARANG (H1, PA)
                     IF (REFANT.GT.0) THEN
                        XZY(I,LP,LC) = PA(REFANT) * RAD2DG
                     ELSE
                        S1 = SIN (PA(IA1))
                        S2 = SIN (PA(IA2))
                        C1 = COS (PA(IA1))
                        C2 = COS (PA(IA2))
                        XZY(I,LP,LC) = ATAN2 (S1+S2, C1+C2) * RAD2DG
                        END IF
C                                      Projected spacing along PA
                  ELSE IF (J.EQ.17) THEN
                     RADPA = ROTATE / RAD2DG
                     XZY(I,LP,LC) = ABS(RANDP(1+ILOCU)*SIN(RADPA) +
     *                  RANDP(1+ILOCV)*COS(RADPA)) * FRQMUL
C                                       frequency
                  ELSE IF (J.EQ.19) THEN
                     XZY(I,LP,LC) = FRQMUL * UVFREQ
C                                       Channel
                  ELSE IF (J.EQ.20) THEN
                     XZY(I,LP,LC) = (LF - 1) * CATUV(KINAX+KLOCFY) +
     *                  BCHAN - 1 + IC
                     END IF
 900              CONTINUE
               IF (J.EQ.13) THEN
                  XZY(3,LP,LC) = 1.0
               ELSE
                  XZY(3,LP,LC) = WT
                  END IF
 910           CONTINUE
 920        CONTINUE
 930     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FLAGIT (OPCODE, LUN, DISK, CNO, VERI, VER, LFGRNO,
     *   FGKOLS, FGNUMV, ID, SUBA, FQID, ANT1, ANT2, BTIME, ETIME, BIF,
     *   EIF, BCHAN, ECHAN, PFLAGS, REASON, CATUV, BUFF, IRET)
C-----------------------------------------------------------------------
C   Updates the Flag (FG) table. Adapted from FLAGUP
C   One entry is made indicating a visibility to be rejected.
C   The FLAG table will be opened on the first call but a final call
C   with OPCODE='CLOS' is required to close the file.
C   Inputs:
C      OPCODE   C*4      Operation desired, 'CLOS'=>close file
C                        Anything else = 'FLAG'
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VERI     I        Input version number
C      VER      I        FG file version
C      LUN      I        Logical unit number to use
C      ID       I(NID)   List of source ID as defined in SOURCE table
C      NID      I        Number of elements in ID
C      SUBA     I        Subarray number.
C      FQID     I        Freqid number
C      ANT1     I        First antenna number in baseline
C      ANT2     I        Second antenna number in baseline
C      BTIME    R        Start time of data to be flagged (Days)
C      ETIME    R        End time of data to be flagged (Days)
C      BIF      I        First IF number to flag. 0=>all
C      EIF      I        Last IF number to flag. 0=>all higher than IFS(1)
C      BCHAN    I        First channel number to flag. 0=>all
C      ECHAN    I        Last channel number to flag. 0=>all higher.
C      PFLAGS   L(4)     Correlator flags
C      REASON   C*24     Reason for flagging blank => ignore for unflag.
C   Input/Output:
C      CATUV    I(256)   Header for disk file to get FG table
C      BUFF     I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      LFGRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      FGKOLS   I(*)     The column pointer array in order, SOURCE,
C                        SUBARRAY, ANTS, TIMERANG, IFS, CHANS, PFLAGS,
C                        REASON
C      FGNUMV   I(*)     Element count in each column.
C   Output:
C      IRET     I        Error code, 0=>OK else TABIO error.
C                        Note: -1 => read, but record deselected.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, REASON*24
      INTEGER   LUN, DISK, CNO, VERI, VER, LFGRNO, FGKOLS(*), FGNUMV(*),
     *   ID, SUBA, FQID, ANT1, ANT2, BIF, EIF, BCHAN, ECHAN, CATUV(256),
     *   BUFF(*), IRET
      REAL      BTIME, ETIME
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER TREAS*24, CTEMP*12
      INTEGER   IDT, SUBT, ANTS(2), IFS(2), CHANS(2), IDUM, FIND, I,
     *   BUFF2(512), LUN2, IFGKOL(MAXFGC), IFGNUM(MAXFGC), NROW, IFQ,
     *   IFGRNO, OROW
      LOGICAL   PFLAGS(4), TFLAGS(4), FIRST
      REAL      TIMER(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE FIRST
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
C                                       See if table open - check FTAB
      IF (OPCODE.NE.'CLOS') THEN
         FIND = BUFF(82)
C                                       Open file
         IF ((FIND.LT.0) .OR. (FIND.GT.10000) .OR. (LUN.NE.FTAB(FIND)))
     *      THEN
            CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV',
     *         IDUM, 'CLRD', BUFF, IRET)
            CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV',
     *         IDUM, 'CLRD', BUFF, IRET)
            CALL CATFIX (DISK, CNO, 'WRIT')
C           CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV',
C    *         IDUM, 'WRIT', BUFF, IRET)
C
            CALL FLGINI ('WRIT', BUFF, DISK, CNO, VER, CATUV, LUN,
     *         LFGRNO, FGKOLS, FGNUMV, IRET)
C                                       Report on the need for flagging
            WRITE (MSGTXT,1000) VER
            IF (.NOT.FIRST) WRITE (MSGTXT,1001) VER
            CALL MSGWRT (2)
            IF (IRET.NE.0) GO TO 999
C                                       Copy the old file
            IF ((FIRST) .AND. (VERI.GT.0)) THEN
               LUN2 = LUN + 1
               CALL FLGINI ('READ', BUFF2, DISK, CNO, VERI, CATUV, LUN2,
     *            IFGRNO, IFGKOL, IFGNUM, IRET)
               IF (IRET.NE.0) GO TO 999
               NROW = BUFF2(5)
               DO 20 I = 1,NROW
                  CALL TABFLG ('READ', BUFF2, IFGRNO, IFGKOL, IFGNUM,
     *               IDT, SUBT, IFQ, ANTS, TIMER, IFS, CHANS, TFLAGS,
     *               TREAS, IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1010) 'READ', I, IRET
                     CALL MSGWRT (8)
                     GO TO 999
                     END IF
                  IF (IRET.EQ.0) THEN
                     CALL TABFLG ('WRIT', BUFF, LFGRNO, FGKOLS, FGNUMV,
     *                  IDT, SUBT, IFQ, ANTS, TIMER, IFS, CHANS, TFLAGS,
     *                  TREAS, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1010) 'WRIT', I, IRET
                        CALL MSGWRT (8)
                        GO TO 999
                        END IF
                     END IF
 20               CONTINUE
               OROW = BUFF(5)
               WRITE (MSGTXT,1002) OROW, NROW, VERI, VER
               CALL MSGWRT (2)
               CALL TABIO ('CLOS', 0, IFGRNO, BUFF2, BUFF2, I)
               END IF
            FIRST = .FALSE.
C                                       Mark as unsorted
            BUFF(43) = 0
            BUFF(44) = 0
            END IF
C                                       Set up for flagging
         ANTS(1) = ANT1
         ANTS(2) = ANT2
         TIMER(1) = BTIME
         TIMER(2) = ETIME
         IFS(1) = BIF
         IFS(2) = EIF
         CHANS(1) = BCHAN
         CHANS(2) = ECHAN
         ID = MAX (ID, 0)
C                                       Flag table entry.
         CALL TABFLG ('WRIT', BUFF, LFGRNO, FGKOLS, FGNUMV, ID, SUBA,
     *      FQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
C                                       Close
      ELSE
         CALL TABFLG ('CLOS', BUFF, LFGRNO, FGKOLS, FGNUMV, IDT, SUBT,
     *      FQID, ANTS, TIMER, IFS, CHANS, TFLAGS, TREAS, IRET)
C                                       Clear write status
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV', IDUM,
     *      'CLWR', BUFF, IRET)
C                                       Reset status to read
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV', IDUM,
     *      'READ', BUFF, IRET)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Found some bad data, will write flags to table FG', I4)
 1001 FORMAT ('Found some bad data, will add   flags to table FG', I4)
 1002 FORMAT ('Copy',I8,' of',I8,' rows from FG vers',I3,' to',I3)
 1010 FORMAT ('ERROR ',A,'ING FG TABLE IN ROW',I7,I5)
      END
      SUBROUTINE TVLOAD (NX, NY, TVCORE, PXINC, TVC, WIN, IRET)
C-----------------------------------------------------------------------
C   Subroutine to load a subset of the plot image to 2 graphics planes.
C   Inputs:
C      NX       I      X dimension of TVCORE
C      NY       I      Y dimension of TVCORE
C      TVCORE   I(*)   Plot data values (> 1 -> valid, < -1 flagged)
C      PXINC    I      Increment in x,y between included pixels
C      TVC      I(4)   TV corners: BLC x,y TRC x,y
C      WIN      I(4)   Map window: ""
C   Outputs
C      IRET     I      Error code: 0 => ok
C                                  1 => input errors
C   Commons: /MAPHDR/ CATBLK  image header
C-----------------------------------------------------------------------
      INTEGER   NX, NY, TVCORE(NX,NY), PXINC, TVC(4), WIN(4), IRET
C
      INTEGER   JERR, I, IYTV, NPIX, IG1, IG2, IX, PLINC, NXINT,
     *   IBUFF(10240), JBUFF(10240), IY, IXTV, J, MX, MY
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL YHOLD ('ONNN', IRET)
C                                       start map read
      IF (PXINC.GE.0) THEN
         PLINC = MAX (1, PXINC)
         NXINT = 1
         NPIX = (WIN(3) - WIN(1)) / PLINC + 1
      ELSE
         PLINC = 1
         NXINT = -PXINC
         NPIX = (1 + WIN(3) - WIN(1)) * NXINT
         END IF
      IG1 = 1 + NGRAY
      IG2 = 4 + NGRAY
      CALL YZERO (IG1, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL YZERO (IG2, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       set up TV writing
      IRET = 1
      IYTV = TVC(2)-1
      DO 70 IY = WIN(2),WIN(4),PLINC
         CALL FILL (NPIX, 0, IBUFF)
         CALL FILL (NPIX, 0, JBUFF)
         IXTV = 1 - NXINT
         DO 40 IX = WIN(1),WIN(3),PLINC
            IXTV = IXTV + NXINT
C                                       "sum"
            IF (PLINC.GT.1) THEN
               MY = MIN (IY+PLINC-1, WIN(4))
               MX = MIN (IX+PLINC-1, WIN(3))
               DO 30 J = IY,MY
                  DO 25 I = IX,MX
                     IF (TVCORE(I,J).GT.1) THEN
                        IBUFF(IXTV) = 1
                        JBUFF(IXTV) = 0
                     ELSE IF (TVCORE(I,J).LT.-1) THEN
                        IF (IBUFF(IXTV).EQ.0) JBUFF(IXTV) = 1
                        END IF
 25                  CONTINUE
 30               CONTINUE
C                                       "interpolate"
            ELSE IF (NXINT.GT.1) THEN
               IF (TVCORE(IX,IY).GT.1) THEN
                  CALL FILL (NXINT,1,IBUFF(IXTV))
               ELSE IF (TVCORE(IX,IY).LT.-1) THEN
                  CALL FILL (NXINT,1,JBUFF(IXTV))
                  END IF
            ELSE
               IF (TVCORE(IX,IY).GT.1) THEN
                  IBUFF(IXTV) = 1
               ELSE IF (TVCORE(IX,IY).LT.-1) THEN
                  JBUFF(IXTV) = 1
                  END IF
               END IF
 40         CONTINUE
C                                       write to TV
         DO 50 J = 1,NXINT
            IYTV = IYTV + 1
            CALL YIMGIO ('WRIT', IG1, TVC(1), IYTV, 0, NPIX, IBUFF,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
            CALL YIMGIO ('WRIT', IG2, TVC(1), IYTV, 0, NPIX, JBUFF,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
 50         CONTINUE
 70      CONTINUE
      IRET = 0
      GO TO 990
C
 980  WRITE (MSGTXT,1980) IRET
      CALL MSGWRT (8)
C
 990  CALL YHOLD ('OFFF', JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('ERROR FROM YIMGIO:',I5)
      END
      SUBROUTINE WIPBOX (IG, TVC, IMW, SCR, IRET)
C-----------------------------------------------------------------------
C   TVFBOX uses a graphics plane to show the user a rectangular box as
C   it is set with the cursor.
C   Inputs:
C      IG      I       graphics plane to use (NGRAY+1 - NGRAY+NGRAPH)
C      TVC     I(4)    TV corners of image
C   In/Out:
C      IMW     I(4)    In: Image corners painted in TVC
C                      Out: Image corners to repaint
C   Output:
C      SCR     I(*)    Scratch buffer: > X dimension (>1280)
C      IRET    I       Error code
C-----------------------------------------------------------------------
      INTEGER   IG, TVC(4), IMW(4), SCR(*), IRET
C
      INTEGER   ITW(3), IL, IX(5), IY(5), QUAD, IBUT, I, JERR, IPOS,
     *   JMW(4), MAG
      REAL      PPOS(2), RPOS(2)
      LOGICAL   T, F, DOIT, ONGR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Check inputs
      IRET = 2
      IF ((IG.LT.NGRAY+1) .OR. (IG.GT.NGRAY+NGRAPH)) GO TO 999
      MAG = 1 + TVZOOM(1)
      IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
C                                       Init
      CALL YWINDO ('READ', WINDTV, IRET)
      IF (IRET.NE.0) THEN
         WINDTV(1) = 1
         WINDTV(2) = 1
         WINDTV(3) = MAXXTV(1)
         WINDTV(4) = MAXXTV(2)
         IRET = 0
         END IF
      CALL ZTIME (ITW)
C                                       turn on graphics if needed
      I = 2 ** (IG-1)
      ONGR = MOD (TVLIMG(1)/I, 2) .EQ. 1
      IF (.NOT.ONGR) THEN
         CALL YSLECT ('ONNN', IG, 0, SCR, IRET)
         IPOS = 1
         IF (IRET.NE.0) GO TO 900
         END IF
      IPOS = 2
C                                       Init BLC of new box
      MSGTXT = 'Set B.L.C. : button A, B, C, or D to change to T.R.C.'
      CALL MSGWRT (1)
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2.0
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2.0
      CALL YCURSE ('FXIT', F, T, RPOS, QUAD, IBUT, IRET)
      IX(1) = RPOS(1) + 0.5
      IX(2) = IX(1)
      IX(3) = MAXXTV(1)
      IX(4) = IX(3)
      IX(5) = IX(4)
      IY(1) = MAXXTV(2)
      IY(2) = RPOS(2) + 0.5
      IY(3) = IY(2)
      IY(4) = IY(2)
      IY(5) = IY(4)
      RPOS(1) = IX(2)
      RPOS(2) = IY(2)
      IL = 1
C                                       No scroll correction
      QUAD = -1
C                                       ON cursor at desired position
 45   PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IRET)
      IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 900
      IF (IRET.EQ.2) CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IRET)
      IPOS = 3
      IF (IRET.NE.0) GO TO 900
C                                       Cursor read loop
 50   CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IRET)
      IPOS = 4
      IF (IRET.NE.0) GO TO 900
      IF (RPOS(1).LT.TVC(1)) RPOS(1) = TVC(1)
      IF (RPOS(2).LT.TVC(2)) RPOS(2) = TVC(2)
      IF (RPOS(1).GT.TVC(3)) RPOS(1) = TVC(3)
      IF (RPOS(2).GT.TVC(4)) RPOS(2) = TVC(4)
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (.NOT.DOIT) THEN
         GO TO 50
      ELSE
C                                       Erase current box
         CALL IMVECT ('OFFF', IG, 5, IX(1), IY(1), SCR, IRET)
         IPOS = 5
         IF (IRET.NE.0) GO TO 900
C                                       New corners: bottom
         IF (IL.NE.2) THEN
            IX(1) = RPOS(1) + 0.01
            IX(2) = IX(1)
            IY(2) = RPOS(2) + 0.01
            IY(3) = IY(2)
            IF (IL.EQ.1) THEN
               IY(4) = IY(3)
               IY(5) = IY(4)
            ELSE
               IX(5) = IX(1)
               END IF
C                                       top: regular boxes
         ELSE
            IX(3) = RPOS(1) + 0.81
            IX(4) = IX(3)
            IY(1) = RPOS(2) + 0.81
            IY(4) = IY(1)
            IY(5) = IY(1)
            IX(5) = IX(1)
            END IF
C                                       draw all boxes
         CALL IMVECT ('ONNN', IG, 5, IX(1), IY(1), SCR, IRET)
         IPOS = 6 + 100
         IF (IRET.NE.0) GO TO 900
C                                       Respond to buttons
         IF (IBUT.EQ.0) THEN
            GO TO 50
         ELSE
C                                       switch to TRC all buttons
            IF (IL.EQ.1) THEN
               IL = 2
               MSGTXT = 'Set T.R.C. : button A or B to repeat other'
     *            // ' corner'
               CALL MSGWRT (1)
               MSGTXT = 'Push C or D to exit'
               CALL MSGWRT (1)
               RPOS(1) = RPOS(1) + 10.0
               RPOS(2) = RPOS(2) + 10.0
               IF (RPOS(1).GT.MAXXTV(1)) RPOS(1) = MAXXTV(1)
               IF (RPOS(2).GT.MAXXTV(2)) RPOS(2) = MAXXTV(2)
C                                       switch to other corn, but A
            ELSE IF (IBUT.LE.2) THEN
               IF (IL.NE.3) THEN
                  IL = 3
                  RPOS(1) = IX(2)
                  RPOS(2) = IY(2)
                  MSGTXT = 'Reset B.L.C. : button A or B to repeat'
     *               // ' other corner'
               ELSE
                  IL = 2
                  RPOS(1) = IX(4)
                  RPOS(2) = IY(4)
                  MSGTXT = 'Reset T.R.C. : button A or B to repeat'
     *               // ' other corner'
                  END IF
               CALL MSGWRT (1)
               MSGTXT = 'Push C or D to exit'
               CALL MSGWRT (1)
               END IF
            IF (IBUT.LE.2) GO TO 45
            END IF
         END IF
C                                       force real BLC, TRC
      CALL IMVECT ('OFFF', IG, 5, IX(1), IY(1), SCR, IRET)
      IF (IX(2).GT.IX(4)) THEN
         JERR = IX(2)
         IX(2) = IX(4)
         IX(4) = JERR
         END IF
      IF (IY(2).GT.IY(4)) THEN
         JERR = IY(2)
         IY(2) = IY(4)
         IY(4) = JERR
         END IF
      CALL COPY (4, IMW, JMW)
      IMW(1) = JMW(1) + REAL(IX(2)-TVC(1)) / REAL(TVC(3)-TVC(1)) *
     *   REAL(JMW(3)-JMW(1)) + 0.5
      IMW(2) = JMW(2) + REAL(IY(2)-TVC(2)) / REAL(TVC(4)-TVC(2)) *
     *   REAL(JMW(4)-JMW(2)) + 0.5
      IMW(3) = JMW(1) + REAL(IX(4)-TVC(1)) / REAL(TVC(3)-TVC(1)) *
     *   REAL(JMW(3)-JMW(1)) + 0.5
      IMW(4) = JMW(2) + REAL(IY(4)-TVC(2)) / REAL(TVC(4)-TVC(2)) *
     *   REAL(JMW(4)-JMW(2)) + 0.5
      IRET = 0
C                                       Off cursor, graphics, scroll
 900  CALL YCURSE ('OFFF', F, T, RPOS, QUAD, IBUT, JERR)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1900) IRET, IPOS
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('WIPBOX: ERROR CODE',I7,' AT',I5)
      END
      SUBROUTINE WIPEIT (ITY, ISN, NX, NY, TVCORE, NBL, BLCORE, IRET)
C-----------------------------------------------------------------------
C   Does the interactive editing functions
C   Inputs:
C      ITY      I      Type: 1-4 => point, area, fast, fat fast
C      ISN      I      1 flag, -1 unflag
C      TVC      I(4)   TV pixel corners
C      IMW      I(4)   Image corners
C      NX       I      X dimension of TVCORE
C      NY       I      Y dimension of TVCORE
C      NBL      I      Max number baselines in BLCORE
C      BLCORE   I(*)   Basseline IDs
C   In/out:
C      TVCORE   I(*)   Image of plot
C   Outputs:
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   ITY, ISN, NX, NY, TVCORE(NX,NY), NBL, BLCORE(NBL,NX,NY),
     *   IRET
C
      INTEGER   WW
      PARAMETER (WW = 15)
C
      CHARACTER STRING*24
      INTEGER   GV, GC, IG1, IG2, ITW(3), NPIX, NROW, MAG, IX0, IY0, IX,
     *   IY, ECOUNT, QUAD, IBUT, IX1, IY1, IMX, IMY, PIXVAL, FMT, TVX,
     *   TVY, IXP(5), IYP(5), TVC(4), DOBLC, I, I1, I2, J, NS, J2,
     *   MASK(2*WW-1,2*WW-1), MTYP, SP, LMX, LMY, WIN(4), NXINT,
     *   IBUFF(10240), JBUFF(10240), IMX1, IMX2, IMY1, IMY2, II, LCOUNT,
     *   MCOUNT
      REAL      PPOS(2), RPOS(2), XFACT, YFACT, LPOS(2), WR, TEMP, TT
      LOGICAL   DOIT, T, F, STARTD
      DOUBLE PRECISION CRD
      INCLUDE 'WIPER.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      STARTD = .FALSE.
      MTYP = XDOCEN + 1.5
C                                       make mask
      IF (ITY.EQ.4) THEN
         J = (2*WW-1) ** 2
         CALL FILL (J, 0, MASK)
         WR = BPARM(9)
         IF (WR.LT.0.5) WR = 1.2
         IF (WR.GT.WW-2) WR = WW-2
         BPARM(9) = WR
         I1 = WW - WR - 0.5
         I2 = WW + WR + 0.5
         I1 = MAX (1, I1)
         I2 = MIN (2*WW-1, I2)
         TT = WW
         DO 20 J = I1,I2
            DO 10 I = I1,I2
               TEMP = SQRT ((I-TT)**2 + (J-TT)**2)
               IF (TEMP.LT.WR) MASK(I,J) = 1
 10            CONTINUE
 20         CONTINUE
         END IF
C
      CALL ZTIME (ITW)
      CALL FILL (5, 1, IXP)
      CALL FILL (5, 1, IYP)
      DOBLC = 0
      FMT = 11
      IF ((NX.LT.10000) .AND. (NY.LT.10000)) THEN
         FMT = 10
         IF ((NX.LT.1000) .AND. (NY.LT.1000)) FMT = 9
         END IF
C                                       Turn on graphics
      GV = NGRAY + 3
      GC = NGRAY + 2
      IG1 = NGRAY + 1
      IG2 = NGRAY + 4
      CALL YHOLD ('ONNN', IRET)
      IF (IRET.NE.0) GO TO 970
      CALL YZERO (GC, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL YSLECT ('ONNN', GC, 0, TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
      IF (ITY.EQ.2) THEN
         CALL YZERO (GV, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL YSLECT ('ONNN', GV, 0, TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         END IF
C                                       Display area: location, size
C                                       Approx corr for zoom
      CALL COPY (4, TVCAT(IICOR), TVC)
      CALL COPY (4, TVCAT(IIWIN), WIN)
      NPIX = 16 * CSIZTV(1)
      NROW = 7.5 * CSIZTV(2) + 0.5
      MAG = 1 + TVZOOM(1)
      IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
      IY0 = WINDTV(4) - MAG*NROW + 1 - (MAG-1)/2 - (CSIZTV(2)+1) / 2
      IF (MAG.GT.1) IY0 = IY0 + MAG
      IY0 = (IY0 - TVZOOM(3)) / MAG + TVZOOM(3)
      IY = MOD (IY0 - 1 - TVSCGY + 3*MAXXTV(2), MAXXTV(2)) + 1
      IY0 = MAX (1, IY)
      IF (IY0+NROW-1.GT.MAXXTV(2)) IY0 = MAXXTV(2) - NROW
C                                       Set X position
      IF (MTYP.LE.0) THEN
         IX0 = WINDTV(1) - (MAG-1)/2 + 1.5*CSIZTV(1)
      ELSE IF (MTYP.EQ.1) THEN
         IX0 = WINDTV(3) - (MAG-1)/2 - NPIX + 1
      ELSE
         IX0 = (WINDTV(1)+WINDTV(3)-NPIX) / 2 - (MAG-1)/2 + 1
         END IF
      IX0 = (IX0 - TVZOOM(2)) / MAG + TVZOOM(2)
      IX = MOD (IX0 - 1 - TVSCGX + 3*MAXXTV(1), MAXXTV(1)) + 1
      IX0 = MAX (1, IX)
      IF (IX0+NPIX-1.GT.MAXXTV(1)) IX0 = MAXXTV(1) - NPIX + 1
C                                       Fill with 1's, 0's
      IX1 = IX0 + NPIX - 1
      IY1 = IY0 + NROW - 1
      CALL YFILL (GC, IX0, IY0, IX1, IY1, 0, TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL YHOLD ('OFFF', IRET)
C                                       Flagging
      IF (ITY.EQ.1) THEN
         MSGTXT = 'Hit button A or B to mark flagged position, '
     *      // 'loop for more'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to mark flagged position, return to'
     *      // ' menu'
      ELSE IF (ITY.EQ.2) THEN
         MSGTXT = 'Hit button A to switch between BLC and TRC'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button B to mark final box, loop for more'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to mark final box, return to menu'
      ELSE
         MSGTXT = 'Position cursor, then hit button, A, B, or C'
     *      // ' to start operation'
         CALL MSGWRT (1)
         MSGTXT = 'Then simply position cursor on point to flag'
         CALL MSGWRT (1)
         MSGTXT = 'Hit buttons A, B, or C to flag and then return'
     *      // ' to menu'
         END IF
      CALL MSGWRT (1)
C                                       instructions: Button D
      MSGTXT = 'Hit button D to exit - no further flagging'
      CALL MSGWRT (1)
C                                       turn on cursor
      ECOUNT = 0
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
      CALL YCURSE ('FXIT', F, T, RPOS, QUAD, IBUT, IRET)
      XFACT = REAL (TVC(3) - TVC(1)) / REAL (WIN(3)-WIN(1))
      YFACT = REAL (TVC(4) - TVC(2)) / REAL (WIN(4)-WIN(2))
      NXINT = XFACT + 0.1
      LCOUNT = 0
      MCOUNT = 0
 105  PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IRET)
      IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 970
C                                       Cursor read loop point
 110  CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IRET)
      IF ((IBUT.GE.8) .OR. (IRET.NE.0)) GO TO 970
      LPOS(1) = RPOS(1)
      LPOS(2) = RPOS(2)
      IF (ITY.NE.4) THEN
         IF (RPOS(1).LT.TVC(1)) RPOS(1) = TVC(1)
         IF (RPOS(2).LT.TVC(2)) RPOS(2) = TVC(2)
         IF (RPOS(1).GT.TVC(3)) RPOS(1) = TVC(3)
         IF (RPOS(2).GT.TVC(4)) RPOS(2) = TVC(4)
      ELSE
         RPOS(1) = MAX (RPOS(1), TVC(1)-WR)
         RPOS(2) = MAX (RPOS(2), TVC(2)-WR)
         RPOS(1) = MIN (RPOS(1), TVC(3)+WR)
         RPOS(2) = MIN (RPOS(2), TVC(4)+WR)
         END IF
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (DOIT) THEN
         TVX = RPOS(1) + 0.49
         TVY = RPOS(2) + 0.49
         IMX = (RPOS(1) - TVC(1)) / XFACT + 0.49 + TVCAT(IIWIN)
         IMY = (RPOS(2) - TVC(2)) / YFACT + 0.49 + TVCAT(IIWIN+1)
C                                       Write text to TV: pixel
         IF (FMT.EQ.11) THEN
            WRITE (STRING,1120) IMX, IMY
         ELSE IF (FMT.EQ.10) THEN
            WRITE (STRING,1121) IMX, IMY
         ELSE
            WRITE (STRING,1122) IMX, IMY
            END IF
         IY = IY0 + 6.0*CSIZTV(2) + 0.5
         CALL YHOLD ('ONNN', IRET)
         CALL IMCHAR (GC, IX0, IY, 0, 0, STRING(:11), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
C                                       count
         LMX = MAX (1, MIN (NX, IMX))
         LMY = MAX (1, MIN (NY, IMY))
         PIXVAL = TVCORE (LMX,LMY)
         IF (PIXVAL.GT.1) THEN
            PIXVAL = PIXVAL - 1
         ELSE IF (PIXVAL.LT.-1) THEN
            PIXVAL = PIXVAL + 1
         ELSE
            PIXVAL = 0
            END IF
         WRITE (STRING,1125) PIXVAL
         CALL REFRMT (STRING, '_', IY)
         IY = IY0 + 1.5*CSIZTV(2) + 0.5
         CALL IMCHAR (GC, IX0, IY, 0, 0, STRING(:11), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         CRD = TVCATD(KDCRV) + (LMX - TVCATR(KRCRP)) * TVCATR(KRCIC)
         CRD = CRD * SCALXY(1)
         IF (FMT.EQ.11) THEN
            WRITE (STRING,1130) 'X', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1131) 'X', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1132) 'X', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1133) 'X', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1134) 'X', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1135) 'X', CRD
         ELSE IF (FMT.EQ.10) THEN
            WRITE (STRING,1140) 'X', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1141) 'X', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1142) 'X', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1143) 'X', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1144) 'X', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1145) 'X', CRD
         ELSE
            WRITE (STRING,1150) 'X', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1151) 'X', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1152) 'X', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1153) 'X', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1154) 'X', CRD
            END IF
         IY = IY0 + 4.5 * CSIZTV(2)
         CALL IMCHAR (GC, IX0, IY, 0, 0, STRING(:11), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         CRD = TVCATD(KDCRV+1) + (LMY-TVCATR(KRCRP+1)) * TVCATR(KRCIC+1)
         CRD = CRD * SCALXY(2)
         IF (FMT.EQ.11) THEN
            WRITE (STRING,1130) 'Y', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1131) 'Y', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1132) 'Y', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1133) 'Y', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1134) 'Y', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1135) 'Y', CRD
         ELSE IF (FMT.EQ.10) THEN
            WRITE (STRING,1140) 'Y', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1141) 'Y', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1142) 'Y', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1143) 'Y', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1144) 'Y', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1145) 'Y', CRD
         ELSE
            WRITE (STRING,1150) 'Y', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1151) 'Y', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1152) 'Y', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1153) 'Y', CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1154) 'Y', CRD
            END IF
         IY = IY0 + 3.0 * CSIZTV(2) + 0.5
         CALL IMCHAR (GC, IX0, IY, 0, 0, STRING(:11), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
C                                       baseline
         STRING = ' '
         NS = 0
         SP = 1
         J2 = MIN (MBL, NBL-1)
         DO 120 J = 1,J2
            IF (BLCORE(J,LMX,LMY).GT.0) THEN
               NS = NS + 1
               IF ((NS.EQ.1) .OR. (NS.EQ.2)) THEN
                  WRITE (STRING(SP:SP+4),1160) BLCORE(J,LMX,LMY)
                  STRING(SP+2:SP+2) = '-'
                  SP = SP + 6
               ELSE IF (NS.EQ.3) THEN
                  STRING(SP:SP) = '+'
                  END IF
               END IF
 120        CONTINUE
         DO 125 J = 1,MBL
            IF (BLCORE(J,LMX,LMY).LT.0) THEN
               NS = NS + 1
               IF (J.EQ.MBL) NS = MAX (3, NS)
               IF ((NS.EQ.1) .OR. (NS.EQ.2)) THEN
                  STRING(SP:SP) = 'F'
                  SP = SP + 1
                  WRITE (STRING(SP:SP+4),1160) -BLCORE(J,LMX,LMY)
                  STRING(SP+2:SP+2) = '-'
                  SP = SP + 6
               ELSE IF (NS.EQ.3) THEN
                  STRING(SP:SP+1) = '+F'
                  SP = SP + 1
                  END IF
               END IF
 125        CONTINUE
         IY = IY0
         CALL IMCHAR (GC, IX0, IY, 0, 0, STRING(:16), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL YHOLD ('OFFF', IRET)
C                                       flagging or boxes
         IF ((IBUT.GT.0) .AND. (.NOT.STARTD)) THEN
            STARTD = .TRUE.
            IF (ITY.GT.2) IBUT = 0
            END IF
         IF (ITY.NE.4) THEN
            LCOUNT = 0
            MCOUNT = 0
            END IF
         IF (((ITY.EQ.1) .AND. (IBUT.GT.0)) .OR. ((ITY.EQ.3) .AND.
     *      (STARTD))) THEN
            IF (ISN*TVCORE(IMX,IMY).GT.1) THEN
               TVCORE(IMX,IMY) = -TVCORE(IMX,IMY)
               IF (TVCORE(IMX,IMY).GT.0) THEN
                  IX = 1
                  IY = 0
               ELSE
                  IX = 0
                  IY = 1
                  END IF
               FLGPIX = FLGPIX + ISN
               LCOUNT = LCOUNT + 1
               MCOUNT = MCOUNT + ABS (TVCORE(IMX,IMY)) - 1
               IF (NXINT.EQ.1) THEN
                  CALL YIMGIO ('WRIT', IG1, TVX, TVY, 0, 1, IX, IRET)
                  IF (IRET.NE.0) GO TO 970
                  CALL YIMGIO ('WRIT', IG2, TVX, TVY, 0, 1, IY, IRET)
                  IF (IRET.NE.0) GO TO 970
               ELSE
                  TVX = (IMX - WIN(1)) * NXINT + TVC(1)
                  TVY = (IMY - WIN(2)) * NXINT + TVC(2)
                  CALL FILL (NXINT, IX, IBUFF)
                  CALL FILL (NXINT, IY, JBUFF)
                  DO 130 I = 1,NXINT
                     CALL YIMGIO ('WRIT', IG1, TVX, TVY, 0, NXINT,
     *                  IBUFF, IRET)
                     IF (IRET.NE.0) GO TO 970
                     CALL YIMGIO ('WRIT', IG2, TVX, TVY, 0, NXINT,
     *                  JBUFF, IRET)
                     IF (IRET.NE.0) GO TO 970
                     TVY = TVY + 1
 130                 CONTINUE
                  END IF
               END IF
            IF (ISN.GT.0) THEN
               WRITE (MSGTXT,1100) 'Flagged', LCOUNT, MCOUNT
            ELSE
               WRITE (MSGTXT,1100) 'Unflagged', LCOUNT, MCOUNT
               END IF
            IF (LCOUNT.GT.0) CALL MSGWRT (3)
            IF (IBUT+ITY.GE.4) GO TO 970
C                                       fat fast
         ELSE IF ((ITY.EQ.4) .AND. (STARTD)) THEN
            IF (ISN.LT.0) THEN
               IX = 1
               IY = 0
            ELSE
               IX = 0
               IY = 1
               END IF
            IMX1 = IMX - WR - 0.5
            IMX2 = IMX + WR + 0.5
            IMX1 = MAX (WIN(1), IMX1)
            IMX2 = MIN (WIN(3), IMX2)
            IMY1 = IMY - WR - 0.5
            IMY2 = IMY + WR + 0.5
            IMY1 = MAX (WIN(2), IMY1)
            IMY2 = MIN (WIN(4), IMY2)
            CALL YHOLD ('ONNN', IRET)
            DO 145 J = IMY1,IMY2
               DO 140 I = IMX1,IMX2
                  IF (MASK(I-IMX+WW,J-IMY+WW)*ISN*TVCORE(I,J).GT.1)
     *               THEN
                     FLGPIX = FLGPIX + ISN
                     TVCORE(I,J) = -TVCORE(I,J)
                     TVX = (I - WIN(1)) * NXINT + TVC(1)
                     TVY = (J - WIN(2)) * NXINT + TVC(2)
                     CALL FILL (NXINT, IX, IBUFF)
                     CALL FILL (NXINT, IY, JBUFF)
                     LCOUNT = LCOUNT + 1
                     MCOUNT = MCOUNT + ABS (TVCORE(I,J)) - 1
                     DO 135 II = 1,NXINT
                        CALL YIMGIO ('WRIT', IG1, TVX, TVY, 0, NXINT,
     *                     IBUFF, IRET)
                        IF (IRET.NE.0) GO TO 970
                        CALL YIMGIO ('WRIT', IG2, TVX, TVY, 0, NXINT,
     *                     JBUFF, IRET)
                        IF (IRET.NE.0) GO TO 970
                        TVY = TVY + 1
 135                    CONTINUE
                     END IF
 140              CONTINUE
 145           CONTINUE
            CALL YHOLD ('OFFF', IRET)
            IF (IBUT.GT.0) THEN
               IF (ISN.GT.0) THEN
                  WRITE (MSGTXT,1101) 'Flagged', LCOUNT, MCOUNT
               ELSE
                  WRITE (MSGTXT,1101) 'Unflagged', LCOUNT, MCOUNT
                  END IF
               CALL MSGWRT (3)
               GO TO 970
               END IF
         ELSE IF (ITY.EQ.2) THEN
            CALL IMVECT ('OFFF', GV, 5, IXP, IYP, TVSCR, IRET)
            IF (IRET.NE.0) GO TO 970
            IF (IBUT.GT.7) GO TO 970
C                                       init position
            IF (DOBLC.EQ.0) THEN
               IXP(1) = TVX
               IXP(2) = IXP(1)
               IXP(3) = IXP(1)
               IXP(4) = MAXXTV(1)
               IXP(5) = IXP(4)
               IYP(1) = MAXXTV(2)
               IYP(2) = IYP(1)
               IYP(3) = TVY
               IYP(4) = IYP(3)
               IYP(5) = IYP(3)
C                                       setting BLC
            ELSE IF (DOBLC.EQ.1) THEN
               IXP(3) = TVX
               IXP(2) = IXP(3)
               IYP(3) = TVY
               IYP(4) = IYP(3)
C                                       setting TRC
            ELSE
               IXP(1) = TVX
               IXP(4) = IXP(1)
               IXP(5) = IXP(1)
               IYP(1) = TVY
               IYP(2) = IYP(1)
               IYP(5) = IYP(1)
               END IF
            CALL IMVECT ('ONNN', GV, 5, IXP, IYP, TVSCR, IRET)
            IF (IRET.NE.0) GO TO 970
C                                       respond to buttons: switch
            IF ((IBUT.EQ.1) .OR. ((IBUT.GT.1) .AND. (DOBLC.EQ.0))) THEN
               IF (DOBLC.EQ.0) THEN
                  CALL IMVECT ('OFFF', GV, 5, IXP, IYP, TVSCR, IRET)
                  IF (IRET.NE.0) GO TO 970
                  IXP(4) = IXP(3) + 10
                  IXP(1) = IXP(4)
                  IXP(5) = IXP(4)
                  IYP(2) = IYP(3) + 10
                  IYP(1) = IYP(2)
                  IYP(5) = IYP(2)
                  DOBLC = -1
                  CALL IMVECT ('ONNN', GV, 5, IXP, IYP, TVSCR, IRET)
                  IF (IRET.NE.0) GO TO 970
               ELSE
                  DOBLC = -DOBLC
                  END IF
               IF (DOBLC.EQ.1) THEN
                  RPOS(1) = IXP(3)
                  RPOS(2) = IYP(3)
               ELSE
                  RPOS(1) = IXP(1)
                  RPOS(2) = IYP(2)
                  END IF
C                                       flag the area
            ELSE IF (IBUT.GT.1) THEN
               IF (IXP(3).GT.IXP(1)) THEN
                  TVX = IXP(3)
                  IXP(3) = IXP(1)
                  IXP(1) = TVX
                  END IF
               IF (IYP(3).GT.IYP(1)) THEN
                  TVY = IYP(3)
                  IYP(3) = IYP(1)
                  IYP(1) = TVY
                  END IF
               IMX1 = (IXP(3) - TVC(1)) / XFACT + 0.49 + WIN(1)
               IMY1 = (IYP(3) - TVC(2)) / YFACT + 0.49 + WIN(2)
               IMX2 = (IXP(1) - TVC(1)) / XFACT + 0.49 + WIN(1)
               IMY2 = (IYP(1) - TVC(2)) / YFACT + 0.49 + WIN(2)
               IF (ISN.LT.0) THEN
                  IX = 1
                  IY = 0
               ELSE
                  IX = 0
                  IY = 1
                  END IF
               CALL YHOLD ('ONNN', IRET)
               DO 170 IMY = IMY1,IMY2
                  DO 160 IMX = IMX1,IMX2
                     IF (ISN*TVCORE(IMX,IMY).GT.1) THEN
                        TVCORE(IMX,IMY) = -TVCORE(IMX,IMY)
                        FLGPIX = FLGPIX + ISN
                        IF (NXINT.EQ.1) THEN
                           IBUFF(1) = IX
                           JBUFF(1) = IY
                        ELSE
                           CALL FILL (NXINT, IX, IBUFF)
                           CALL FILL (NXINT, IY, JBUFF)
                           END IF
                        TVX = (IMX - WIN(1)) * NXINT + TVC(1)
                        TVY = (IMY - WIN(2)) * NXINT + TVC(2)
                        LCOUNT = LCOUNT + 1
                        MCOUNT = MCOUNT + ABS (TVCORE(IMX,IMY)) - 1
                        DO 150 I = 1,NXINT
                           CALL YIMGIO ('WRIT', IG1, TVX, TVY, 0, NXINT,
     *                        IBUFF, IRET)
                           IF (IRET.NE.0) GO TO 970
                           CALL YIMGIO ('WRIT', IG2, TVX, TVY, 0, NXINT,
     *                        JBUFF, IRET)
                           IF (IRET.NE.0) GO TO 970
                           TVY = TVY + 1
 150                       CONTINUE
                        END IF
 160                 CONTINUE
 170              CONTINUE
               DOBLC = 0
               CALL IMVECT ('OFFF', GV, 5, IXP, IYP, TVSCR, IRET)
               IF (IRET.NE.0) GO TO 970
               IF (ISN.GT.0) THEN
                  WRITE (MSGTXT,1100) 'Flagged', LCOUNT, MCOUNT
               ELSE
                  WRITE (MSGTXT,1100) 'Unflagged', LCOUNT, MCOUNT
                  END IF
               CALL MSGWRT (3)
               IF (IBUT.GT.3) GO TO 970
               CALL FILL (5, 1, IXP)
               CALL FILL (5, 1, IYP)
               PPOS(1) = 0.
               CALL YHOLD ('OFFF', IRET)
               END IF
            END IF
         END IF
      IF (ITY.EQ.4) GO TO 110
      IF ((LPOS(1).NE.RPOS(1)) .OR. (LPOS(2).NE.RPOS(2))) GO TO 105
      GO TO 110
C-----------------------------------------------------------------------
C                                       Close downs
C                                       TV error possibly
 970  IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1970) IRET
         CALL MSGWRT (6)
         END IF
      CALL YZERO (GC, IY)
      CALL YHOLD ('OFFF', IY)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A,I6,' pixels',I9,' vis points')
 1101 FORMAT (A,I7,' pixels',I12,' vis points')
 1120 FORMAT (I5,',',I5)
 1121 FORMAT (I4,',',I5)
 1122 FORMAT (I3,',',I4)
 1125 FORMAT ('COUNT',I6)
 1130 FORMAT (A1,'=',F9.5)
 1131 FORMAT (A1,'=',F9.4)
 1132 FORMAT (A1,'=',F9.3)
 1133 FORMAT (A1,'=',F9.2)
 1134 FORMAT (A1,'=',F9.1)
 1135 FORMAT (A1,'=',F9.0)
 1140 FORMAT (A1,1X,F8.5)
 1141 FORMAT (A1,1X,F8.4)
 1142 FORMAT (A1,1X,F8.3)
 1143 FORMAT (A1,1X,F8.2)
 1144 FORMAT (A1,1X,F8.1)
 1145 FORMAT (A1,1X,F8.0)
 1150 FORMAT (A1,1X,F7.4)
 1151 FORMAT (A1,1X,F7.3)
 1152 FORMAT (A1,1X,F7.2)
 1153 FORMAT (A1,1X,F7.1)
 1154 FORMAT (A1,1X,F7.0)
 1160 FORMAT (I5.5)
 1970 FORMAT ('WIPEIT: TV ACTION IO ERROR',I7)
      END
      SUBROUTINE REPORT (NA, NI, NP, MALL, MBAD, MSOM, NBDCOR, FLGPIX)
C-----------------------------------------------------------------------
C   Print the reports
C   Inputs:
C      NA      I      Number of antennas
C      NI      I      Number of IFs
C      MALL    I(*)   Counts samples by baseline etc
C      MBAD    I(*)   Counts full bad spectra by baseline
C      MSOM    I(*)   Counts partly bad spectra by baseline
C-----------------------------------------------------------------------
      INTEGER   NA, NI, NP, MALL(NA,NA,NP,NI), MBAD(NA,NA,NP,NI),
     *   MSOM(NA,NA,NP,NI), NBDCOR, FLGPIX
C
      INTEGER   I, J, K, L, TOTALA, TOTALB, MMAX, MXA, I1, I2, TOTALS,
     *   MA, MB
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   MAUX(MAXANT)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       First add the totals
      DO 100 K = 1,NP
         DO 90 L = 1,NI
            TOTALA = 0
            TOTALB = 0
            TOTALS = 0
            MMAX = 0
            MXA = 0
            DO 15 I = 1,NA
               DO 10 J = 1,NA
                  IF (MALL(I,J,K,L).GT.0) THEN
                     MXA = MAX (MXA, I)
                     MXA = MAX (MXA, J)
                     TOTALA = TOTALA + MALL(I,J,K,L)
                     IF (MALL(I,J,K,L).GT.MMAX) MMAX = MALL(I,J,K,L)
                     END IF
                  TOTALB = TOTALB + MBAD(I,J,K,L)
                  TOTALS = TOTALS + MSOM(I,J,K,L)
 10               CONTINUE
 15            CONTINUE
C                                       Now report on all visibilities
            IF (TOTALA.GT.0) THEN
               I = L + BIF - 1
               IF (ECHAN.LE.0) THEN
                  WRITE (MSGTXT,1020) TOTALB, TOTALA, I, K
                  CALL MSGWRT (4)
               ELSE
                  WRITE (MSGTXT,1021) TOTALB, TOTALA, I, K
                  CALL MSGWRT (4)
                  WRITE (MSGTXT,1022) TOTALS, TOTALA, I, K
                  CALL MSGWRT (4)
                  END IF
               WRITE (MSGTXT,1025) MMAX
               CALL MSGWRT (4)
               I1 = 1
 25            I2 = MIN (I1+27, MXA)
               IF (I2.GE.I1) THEN
                  DO 40 I= 1,MXA
                     DO 30 J= I1,I2
                        MAUX(J) = MALL(I,J,K,L)
                        MAUX(J) = NINT ((10. * MAUX(J)) / MMAX)
 30                     CONTINUE
                     WRITE (MSGTXT,1035) I, (MAUX(J), J = I1,I2)
                     CALL MSGWRT (4)
 40                  CONTINUE
                  I1 = I2 + 1
                  GO TO 25
                  END IF
               END IF
C                                       Now report percentage flagged
            IF ((TOTALA.GT.0) .AND. (TOTALB.GT.0)) THEN
               MSGTXT = 'Visibilities flagged (percent):'
               IF (ECHAN.GT.1) MSGTXT = 'Visibility spectra fully'
     *            // ' flagged (percent):'
               CALL MSGWRT (4)
               I1 = 1
 45            I2 = MIN (I1+27, MXA)
               IF (I2.GE.I1) THEN
                  DO 60 I = 1,MXA
                     DO 50 J = I1,I2
                        MA = MALL(I,J,K,L)
                        MB = MBAD(I,J,K,L)
                        IF (MA.GT.0) THEN
                           MAUX(J) = NINT ((100. * MB) / MA)
                        ELSE
                           MAUX(J) = 0
                           END IF
 50                     CONTINUE
                     WRITE (MSGTXT,1035) I, (MAUX(J), J = I1,I2)
                     CALL MSGWRT (4)
 60                  CONTINUE
                  I1 = I2 + 1
                  GO TO 45
                  END IF
               END IF
            IF ((TOTALA.GT.0) .AND. (TOTALS.GT.0)) THEN
               MSGTXT = 'Visibility spectra partly flagged (percent):'
               CALL MSGWRT (4)
               I1 = 1
 65            I2 = MIN (I1+27, MXA)
               IF (I2.GE.I1) THEN
                  DO 75 I = 1,MXA
                     DO 70 J = I1,I2
                        MA = MALL(I,J,K,L)
                        MB = MSOM(I,J,K,L)
                        IF (MA.GT.0) THEN
                           MAUX(J) = NINT ((100. * MB) / MA)
                        ELSE
                           MAUX(J) = 0
                           END IF
 70                     CONTINUE
                     WRITE (MSGTXT,1035) I, (MAUX(J), J = I1,I2)
                     CALL MSGWRT (4)
 75               CONTINUE
                  I1 = I2 + 1
                  GO TO 65
                  END IF
               END IF
 90         CONTINUE
 100     CONTINUE
C                                       Report flagged correlators
      WRITE (MSGTXT,1090) NBDCOR, FLGPIX
      CALL REFRMT (MSGTXT, '_', I1)
      CALL MSGWRT (4)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Flagged',I8,' of',I10,' correlators for IF',I3,' POL',I2)
 1021 FORMAT ('Fully flagged ',I8,' of',I10,' spectra for IF',I3,' POL',
     *   I2)
 1022 FORMAT ('Partly flagged',I8,' of',I10,' spectra for IF',I3,' POL',
     *   I2)
 1025 FORMAT ('Visibilities per baseline (tens of percent of', I9 ,'):')
 1035 FORMAT ('Ant',I3,1X,28(I2))
 1090 FORMAT ('Flagged',I10,' correlators in',I9,' pixels')
      END
      SUBROUTINE WIPSIZ (WR, NXINT, TVSCR, IRET)
C-----------------------------------------------------------------------
C   WIPSIZ is an interactive setting of the wiper size using a plot of a
C   circle
C   Inputs:
C      NXINT   I      Numbr TV pixels per image pixel
C   In/out:
C      WR      R      Wiper radius in pixels
C   Output:
C      TVSCR   I(*)   Scratch buffer
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   NXINT, TVSCR(*), IRET
      REAL      WR
C
      INTEGER   NA
      PARAMETER (NA=60)
C
      INTEGER   GV, NPIX, NROW, MAG, IX0, IY0, IX, IY, CX0, CY0, QUAD,
     *   IBUT, I, IXP(NA), IYP(NA), ITW(3), IX1, IY1, IXC(4), IYC(4)
      REAL      RPOS(2), PPOS(2), A, B
      LOGICAL   T, F, DOIT
      CHARACTER STRING*6
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA T,F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      NXINT = MAX (1, NXINT)
      CALL ZTIME (ITW)
      GV = 3 + NGRAY
      CALL YHOLD ('ONNN', IRET)
      IF (IRET.NE.0) GO TO 970
      CALL YZERO (GV, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL YSLECT ('ONNN', GV, 0, TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Display area: location, size
C                                       Approx corr for zoom
      NPIX = 6 * CSIZTV(1)
      NROW = 3 * CSIZTV(2) + 0.5
      MAG = 1 + TVZOOM(1)
      IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
      IX0 = WINDTV(1) - (MAG-1)/2 + 1.5 * CSIZTV(1)
      IY0 = WINDTV(4) - MAG*NROW + 1 - (MAG-1)/2
      IF (MAG.GT.1) IY0 = IY0 + MAG
      IX0 = (IX0 - TVZOOM(2)) / MAG + TVZOOM(2)
      IY0 = (IY0 - TVZOOM(3)) / MAG + TVZOOM(3)
      IX = MOD (IX0 - 1 - TVSCGX + 3*MAXXTV(1), MAXXTV(1)) + 1
      IY = MOD (IY0 - 1 - TVSCGY + 3*MAXXTV(2), MAXXTV(2)) + 1
C                                       Fill with 1's, 0's
      IX0 = MAX (1, IX)
      IY0 = MAX (1, IY)
      IF (IX0+NPIX-1.GT.MAXXTV(1)) IX0 = MAXXTV(1) - NPIX + 1
      IF (IY0+NROW-1.GT.MAXXTV(2)) IY0 = MAXXTV(2) - NROW + 1
      IX1 = IX0 + NPIX - 1
      IY1 = IY0 + NROW - 1
      CALL YFILL (GV, IX0, IY0, IX1, IY1, 0, TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2.0
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2.0
      CALL YCURSE ('FXIT', F, T, RPOS, QUAD, IBUT, IRET)
      CX0 = RPOS(1) + 0.5
      CY0 = RPOS(2) + 0.5
      RPOS(1) = RPOS(1) + WR
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IRET)
      IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 970
      MSGTXT = 'Cursor sets fat wiper radius'
      CALL MSGWRT (1)
      MSGTXT = 'Any button returns to the menu'
      CALL MSGWRT (1)
C                                       draw circle
 50   B = WR * NXINT
      IXC(1) = CX0 - B
      IXC(2) = CX0 + B
      IXC(3) = CX0
      IXC(4) = CX0
      IYC(1) = CY0
      IYC(2) = CY0
      IYC(3) = CY0 + B
      IYC(4) = CY0 - B
      CALL IMVECT ('ONNN', GV, 2, IXC(1), IYC(1), TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL IMVECT ('ONNN', GV, 2, IXC(3), IYC(3), TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
      DO 60 I = 1,NA
         A = (I - 1.0D0) * TWOPI / (NA - 1.0D0)
         IXP(I) = CX0 + B * SIN (A) + 0.5
         IYP(I) = CY0 + B * COS (A) + 0.5
 60      CONTINUE
      CALL IMVECT ('ONNN', GV, NA, IXP, IYP, TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
      WRITE (STRING,1060) WR
      CALL IMCHAR (GV, IX0, IY0, 0, 0, STRING(:6), TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL YHOLD ('OFFF', IRET)
C                                       Now read the cursor
 100  CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IRET)
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (IBUT.GT.0) THEN
         GO TO 970
      ELSE IF (DOIT) THEN
         WR = SQRT ((RPOS(1)-CX0)**2 + (RPOS(2)-CY0)**2) / NXINT
         WR = MAX (0.1, MIN (12.0, WR))
         CALL YHOLD ('ONNN', IRET)
         CALL IMVECT ('OFFF', GV, NA, IXP, IYP, TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL IMVECT ('OFFF', GV, 2, IXC(1), IYC(1), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL IMVECT ('OFFF', GV, 2, IXC(3), IYC(3), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         GO TO 50
      ELSE
         GO TO 100
         END IF
C                                       TV error possibly
 970  IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1970) IRET
         CALL MSGWRT (6)
         END IF
      CALL YZERO (GV, IY)
      CALL YHOLD ('OFFF', IY)
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT (F6.2)
 1970 FORMAT ('WIPSIZ: TV ACTION IO ERROR',I7)
      END
      SUBROUTINE WIPSET (WR, NXINT, TVSCR, IRET)
C-----------------------------------------------------------------------
C   WIPSIZ is an interactive setting of the wiper size using a plot of a
C   horizontal line labeled from 0 to 12.
C   NO ZOOM allowed.
C   Inputs:
C      NXINT   I      Replication factor in image
C   In/out:
C      WR      R      Wiper radius in pixels
C   Output:
C      TVSCR   I(*)   Scratch buffer
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   NXINT, TVSCR(*), IRET
      REAL      WR
C
      INTEGER   NA
      PARAMETER (NA=60)
C
      INTEGER   GV, NPIX, NROW, IX0, IY0, IX, IY, CX0, CY0, QUAD,
     *   IBUT, I, IXP(NA), IYP(NA), JX0, JX1, ITW(3)
      REAL      RPOS(2), PPOS(2), A, B
      LOGICAL   T, F, DOIT
      CHARACTER STRING*6
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA T,F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      CALL ZTIME (ITW)
      GV = 3 + NGRAY
      CALL YHOLD ('ONNN', IRET)
      IF (IRET.NE.0) GO TO 970
      CALL YZERO (GV, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL YSLECT ('ONNN', GV, 0, TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Display area: location, size
C                                       Approx corr for zoom
      NPIX = 6 * CSIZTV(1)
      NROW = 3 * CSIZTV(2) + 0.5
      IX0 = WINDTV(1)
      IY0 = WINDTV(4) - NROW + 1
C                                       Draw line
      CX0 = (WINDTV(1) + WINDTV(3)) / 2
      CY0 = (5.0*WINDTV(2) + 3.0*WINDTV(4)) / 8.0
      IX = CX0 - 6 * CSIZTV(1)
      MSGTXT = 'WIPER RADIUS'
      CALL IMCHAR (GV, IX, IY0, 0, 0, MSGTXT(:12), TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
      JX0 = WINDTV(1) + (WINDTV(3) - WINDTV(1)) / 10.0
      JX1 = WINDTV(3) - (WINDTV(3) - WINDTV(1)) / 10.0
      IXP(1) = JX0
      IXP(2) = JX1
      IY = IY0 - 10 * CSIZTV(2)
      IYP(1) = IY
      IYP(2) = IY
      CALL IMVECT ('ONNN', GV, 2, IXP, IYP, TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
      DO 10 I = 1,13
         IX = ((I-1.0) / 12.0) * (JX1 - JX0) + JX0 + 0.5
         IXP(1) = IX
         IXP(2) = IX
         IYP(1) = IY - CSIZTV(2)
         IYP(2) = IY + CSIZTV(2)
         CALL IMVECT ('ONNN', GV, 2, IXP, IYP, TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (STRING,1000) I-1
         IXP(1) = IX - CSIZTV(1)
         IYP(1) = IY - 2.5 * CSIZTV(2)
         CALL IMCHAR (GV, IXP, IYP, 0, 0, STRING(:2), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
 10      CONTINUE
      CY0 = IYP(1) - 8 - 12 * NXINT
      WR = MIN (WR, 12.0)
      RPOS(1) = JX0 + WR/12.0 * (JX1 - JX0)
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2.0
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IRET)
      IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 970
      MSGTXT = 'Cursor horizontal position sets fat wiper radius'
      CALL MSGWRT (1)
      MSGTXT = 'Any button returns to the menu'
      CALL MSGWRT (1)
C                                       draw circle
 50   B = NXINT * WR
      DO 60 I = 1,NA
         A = (I - 1.0D0) * TWOPI / (NA - 1.0D0)
         IXP(I) = CX0 + B * SIN (A) + 0.5
         IYP(I) = CY0 + B * COS (A) + 0.5
 60      CONTINUE
      CALL IMVECT ('ONNN', GV, NA, IXP, IYP, TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
      WRITE (STRING,1060) WR
      CALL IMCHAR (GV, IX0, IY0, 0, 0, STRING(:6), TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL YHOLD ('OFFF', IRET)
C                                       Now read the cursor
 100  CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IRET)
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (IBUT.GT.0) THEN
         GO TO 970
      ELSE IF (DOIT) THEN
         WR = (RPOS(1)-JX0) * 12.0 / (JX1 - JX0)
         WR = MAX (0.1, MIN (12.0, WR))
         CALL YHOLD ('ONNN', IRET)
         CALL IMVECT ('OFFF', GV, NA, IXP, IYP, TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         GO TO 50
      ELSE
         GO TO 100
         END IF
C                                       TV error possibly
 970  IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1970) IRET
         CALL MSGWRT (6)
         END IF
      CALL YZERO (GV, IY)
      CALL YHOLD ('OFFF', IY)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I2.2)
 1060 FORMAT (F6.2)
 1970 FORMAT ('WIPSET: TV ACTION IO ERROR',I7)
      END
      SUBROUTINE WIPEHI (NX, NY)
C-----------------------------------------------------------------------
C   WIPEHI appends to the history file.
C-----------------------------------------------------------------------
      INTEGER   NX, NY
C
      INCLUDE 'WIPER.INC'
      CHARACTER ATIME*8, ADATE*12, HILINE*72, TEXT*9, CHTYPE(NUMPRM)*9,
     *   CHTYP2(NUMPRM)*9, AXIS(2)*1
      INTEGER   LUN, IERR, I, TIME(3), DATE(3), HBUFF(256), J, NC, ITRIM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA LUN /28/
      DATA CHTYPE /'Amplitude', 'Phase   ', 'UV dist ',
     *   'UV pa    ', 'Time    ', 'U       ', 'V       ',
     *   'W        ', 'Real    ', 'Imag    ', 'Time hrs',
     *   'log (amp)', 'Weight', 'Hour angl', 'Elevation', 'Para angl',
     *   'UV@pa=   ', 'Azimuth', 'Frequency', 'Channel'/
      DATA CHTYP2 /'Flux     ', 'Offset  ', 'UV dist ',
     *   'UV pa    ', 'Time    ', 'Longitude', 'Latitude',
     *   'W        ', 'Flux    ', 'Offset  ', 'Time hrs',
     *   'log(flux)', 'Weight', 'Hour angl', 'Elevation', 'Para angl',
     *   'UV@pa=   ', 'Azimuth', 'Frequency', 'Channel'/
      DATA AXIS /'X','Y'/
C-----------------------------------------------------------------------
C                                       no HI if no flags
      IF (NBDCOR.LE.0) GO TO 999
C                                       Write History.
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN, DISKIN, CNOIN, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      WRITE (HILINE,1000) TSKNAM, ADATE, ATIME
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       No Sources
      IF (NSOU.LE.0) THEN
         WRITE (HILINE,1100) TSKNAM
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Sources by name
      ELSE
C                                       Included or excluded?
         WRITE (HILINE,1101) TSKNAM
         IF (DOSWNT) WRITE (HILINE,1102) TSKNAM
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       First two and label.
         IF (NSOU.EQ.1) THEN
            WRITE (HILINE,1103) TSKNAM, XSOUR(1)
         ELSE
            WRITE (HILINE,2103) TSKNAM, XSOUR(1), XSOUR(2)
            END IF
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Rest of sources
         DO 10 I = 3,NSOU,2
            WRITE (HILINE,1104) TSKNAM, XSOUR(I), XSOUR(I+1)
            CALL HIADD (LUN, HILINE, HBUFF, IERR)
            IF (IERR.NE.0) GO TO 900
 10         CONTINUE
         END IF
C                                       QUAL, CALCODE
      WRITE (HILINE,1110) TSKNAM, SELQUA, SELCOD
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Subarray
      WRITE (HILINE,1111) TSKNAM, SUBARR
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Flag table
      WRITE (HILINE,1112) TSKNAM, FGVER
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (HILINE,1113) TSKNAM, FGVERO
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       TIMERANG
      CALL HITIME (TSTART, TEND, LUN, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       IF range
      WRITE (HILINE,1114) TSKNAM, BIF, EIF
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Chan range
      WRITE (HILINE,1115) TSKNAM, BCHAN, ECHAN
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Calibration
C                                       Table
      IF (DOCAL) THEN
         WRITE (HILINE,1116) TSKNAM, CLUSE
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       BP table
      IF (XDOBND.GT.0.0) THEN
         WRITE (HILINE,1117) TSKNAM, DOBAND
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (HILINE,1118) TSKNAM, BPVER
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Plot type and range
      DO 20 I = 1,2
         IF (TYPUVD.LE.0) THEN
            TEXT = CHTYPE(TYPEAX(I))
         ELSE
            TEXT = CHTYP2(TYPEAX(I))
            END IF
         NC = ITRIM (TEXT)
         WRITE (HILINE,1119) TSKNAM, AXIS(I), TEXT(:NC)
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         J = 2 * (I + 1)
         WRITE (HILINE,1120) TSKNAM, AXIS(I), BPARM(J)
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         J = J + 1
         WRITE (HILINE,1121) TSKNAM, AXIS(I), BPARM(J)
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         J = NX
         IF (I.EQ.2) J = NY
         WRITE (HILINE,1122) TSKNAM, AXIS(I), J
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
 20      CONTINUE
C                                       Number of flagged correlators
      WRITE (HILINE,1123) TSKNAM, NBDCOR
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (HILINE,1124) TSKNAM, FLGPIX
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (HILINE,1125) TSKNAM, NFGWRI
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (MSGTXT,1130) NFGWRI, FGVERO
      CALL MSGWRT (4)
C                                       Close HI file
 900  CALL HICLOS (LUN, .TRUE., HBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'/********* Start ',A12,2X,A8)
 1100 FORMAT (A6,'SOURCES = ''''     /All sources selected')
 1101 FORMAT (A6,'/Sources excluded:')
 1102 FORMAT (A6,'/Sources included:')
 1103 FORMAT (A6,'SOURCES = ''',A16,'''')
 2103 FORMAT (A6,'SOURCES = ''',A16,''',''',A16,'''')
 1104 FORMAT (A6,'         ,''',A16,''',''',A16,'''')
 1110 FORMAT (A6,'QUAL = ',I4,' CALCODE = ',A4)
 1111 FORMAT (A6,'SUBARRAY =',I4)
 1112 FORMAT (A6,'FLAGVER  =',I4,' /Flagging table applied')
 1113 FORMAT (A6,'FLAGVERO =',I4,' /Flagging table written')
 1114 FORMAT (A6,'BIF =',I4,', EIF =',I4,'/ IF range')
 1115 FORMAT (A6,'BCHAN = ',I4,' ECHAN = ',I4,
     *   ' /Start and stop channels')
 1116 FORMAT (A6,'GAINUSE =',I3,' / CL table')
 1117 FORMAT (A6,'DOBAND =',I2,'  /BP correction done')
 1118 FORMAT (A6,'BPVER =',I3,' / BP correction used BP table')
 1119 FORMAT (A6,A1,'TYPE = ''',A,''' / plot axis type')
 1120 FORMAT (A6,A1,'MIN =',1PE12.5,'  / plot axis minimum')
 1121 FORMAT (A6,A1,'MAX =',1PE12.5,'  / plot axis maximum')
 1122 FORMAT (A6,A1,'PIXELS =',I6,'  / plot axis number TV pixels')
 1123 FORMAT (A6,'/ Correlators flagged:',I8)
 1124 FORMAT (A6,'/ TV pixels flagged:  ',I8)
 1125 FORMAT (A6,'/ FG records written: ',I8)
 1130 FORMAT (I10,' records written to FG table vers',I4)
      END
