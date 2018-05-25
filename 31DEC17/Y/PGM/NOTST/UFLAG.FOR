LOCAL INCLUDE 'UFLAG.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   MFLGAL
      PARAMETER (MFLGAL=20)
C
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XCALC, XSTOK,
     *   TVCATH(256)
      REAL   XSIN, XDISIN, XISCAL, XQUAL, XBAND, XFREQ, XFQID, XTIME(8),
     *   XANT(50), XBASE(50), XUVRA(2), XSUBA, XBCHAN, XECHAN, XNCHAV,
     *   XCHINC, XBIF, XEIF, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XFGOUT, XDOBND, XBPVER, XSMOTH(3), CELLSZ(2), XIMSIZ(2),
     *   DOWEGT, APARM(10), XDOCEN, XDOALL, PRTLEV, XBADD(10)
      REAL      BUFF1(UVBFSS), TBEG, TFIN, UVCELL(2), XYSCL(2),
     *   XYOFF(2), RPARM(20), XYMIN(2), XYMAX(2), FINC(MAXIF),
     *   TVCATR(256), SCALXY(2), FLGALV(2,MFLGAL,4)
      DOUBLE PRECISION FOFF(MAXIF), TVCATD(128)
      INTEGER   SEQIN, DISKIN, LUNI, INDI, TYPEAX(2),
     *   NCH, JBUFSZ, IANT(50), NANT, IBAS(50), NBAS, CNOIN, IFRQ, NFRQ,
     *   NSUBA, GRCHN, TVCHN, TVCORN(4), ISBAND(MAXIF), CHINC, LABEL,
     *   EXCLFQ(MAXIF,MAXFQ), CSOU, SBUFF(512), COUNT(3,3), FGVERI,
     *   FGVERO, TVSCR(MAXIMG), NBDCOR, MANT, MIF, TVCAT(256), NCHAN,
     *   FLGPIX, NSOU, NPOL, LCOR0, NFGWRI, BLFLAG(MAXANT,MAXANT), MBL,
     *   TTY(2), NCHAV, LTYPE, TVMAXX(2), NFLGAL(4), FGNEW, NFIXED,
     *   MVISP, VISCNT, FVIRST, NVI
      LOGICAL   UVREV, ISUVR, MULTI, DESEL, DOTV, ONEIF, ONECHN,
     *   ONEPOL, NOTCRS, ISCROS(4), FLAGON, FCINIT, PFLAGS(4), ISCAL
      EQUIVALENCE (TVCAT, TVCATR, TVCATD, TVCATH)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XISCAL,
     *   XQUAL, XCALC, XSTOK, XBAND, XFREQ, XFQID, XTIME, XANT, XBASE,
     *   XUVRA, XSUBA, XBCHAN, XECHAN, XNCHAV, XCHINC, XBIF, XEIF,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XFGOUT, XDOBND,
     *   XBPVER, XSMOTH, CELLSZ, XIMSIZ, DOWEGT, APARM, XDOCEN, XDOALL,
     *   PRTLEV, XBADD
      COMMON /CHPARM/ NAMEIN, CLAIN, XSOUR
      COMMON /BUFRS/ BUFF1, SBUFF, RPARM, JBUFSZ
      COMMON /UVPCOM/ TVCAT, FOFF, FINC, ISBAND, TBEG, TFIN, XYSCL,
     *   XYOFF, XYMIN, XYMAX, UVREV, ISUVR, MULTI, DOTV,
     *   SEQIN, DISKIN, LUNI, INDI, TYPEAX, NCH, CNOIN, IFRQ,
     *   NFRQ, NSUBA, TVCHN, GRCHN, TVCORN, EXCLFQ, CHINC, LABEL,
     *   CSOU, COUNT, FGVERI, FGVERO, ONEPOL, NOTCRS, ONEIF, ONECHN,
     *   TVSCR, NBDCOR, MANT, MIF, NCHAN, FLGPIX, NPOL, LCOR0, ISCROS,
     *   NFGWRI, MBL, TTY, SCALXY, NCHAV, LTYPE, TVMAXX, FLGALV, NFLGAL,
     *   FLAGON, FGNEW, FCINIT, PFLAGS, NFIXED, UVCELL, MVISP, VISCNT,
     *   FVIRST, NVI, ISCAL
      COMMON /BASSEL/ DESEL, IANT, NANT, IBAS, NBAS, NSOU, BLFLAG
LOCAL END
LOCAL INCLUDE 'UFLAGPLT.INC'
      REAL      PRANGE(2,8), GRANGE(2), XRANGE(2), VINFO(5)
      INTEGER   ITRTYP, IMGTYP, IBUFF(10240), JBUFF(10240)
      COMMON /UFPLOT/ PRANGE, GRANGE, XRANGE, VINFO, ITRTYP, IMGTYP,
     *   IBUFF, JBUFF
LOCAL END
LOCAL INCLUDE 'UFLAGFIL.INC'
      INTEGER   FCROW, FCGNUM, FCGANT(2), FCGSOR, FCGCHN(2), FCGIF(2),
     *   FCGSUB, FCGFQ, FCGIT(2), FCBUFF(512), FCKOLS(15), FCNUMV(15)
      CHARACTER FCGOP*8, FCGSTK*4, LDTYPE*8, FCGREA*24
      REAL      FCGTIM(2), DTIMES(2), DFLUXS(2)
      COMMON /UFFILE/ FCBUFF, FCKOLS, FCNUMV, FCGTIM, DTIMES, DFLUXS,
     *   FCROW, FCGNUM, FCGANT, FCGSOR, FCGCHN, FCGIF, FCGSUB, FCGFQ,
     *   FCGIT
      COMMON /CFILE/ FCGOP, FCGSTK, LDTYPE, FCGREA
LOCAL END
LOCAL INCLUDE 'UFLAGVIS.INC'
      INTEGER   MAXVIS
      PARAMETER (MAXVIS=10000)
C
      INTEGER   VLANT(2,MAXVIS), VLIF(MAXVIS), VLCHAN(2,MAXVIS),
     *   VLBASL(MAXVIS), VLFVC(MAXVIS)
      REAL      VLTIME(MAXVIS), VLREAL(MAXVIS), VLIMAG(MAXVIS)
      COMMON /UFLVIS/ VLTIME, VLREAL, VLIMAG, VLANT, VLIF, VLCHAN,
     *   VLBASL, VLFVC
LOCAL END
      PROGRAM UFLAG
C-----------------------------------------------------------------------
C! UFLAG plots uvdata in a U-V grid and then offers editing options
C# EXT-appl Graphics Plot UV-util editing
C-----------------------------------------------------------------------
C;  Copyright (C) 2016
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
C   UFLAG plots uv data like UVPLT and then edits them.
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
C      BADDISK                            Disk to avoid for scratch.
C-----------------------------------------------------------------------
      INTEGER   NBL
      PARAMETER (NBL = 25)
C
      CHARACTER PRGM*6, PHNAME*48, STCHAR(12)*2
      INTEGER   IERR, IRET, NX, NY, NWORDS, MALL(2), MBAD(2), MSOM(2),
     *   BLCORE(2), FVCORE(2), FVPNTR(2), VER, REPEAT, I, NLOOPS,
     *   NSTOKS, NBD, FLP
      LONGINT   TVADDR, OFFALL, OFFBAD, OFFSOM, BLADDR, FVADDR, MDADDR,
     *   FPADDR
      INCLUDE 'UFLAG.INC'
      REAL      XZY(5,MAXCIF), TVCORE(2), MDCORE(2)
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'UFLAG '/
      DATA STCHAR /'RR','LL', 'RL','LR', 'XX','YY', 'XY','YX',
     *   'I','V', 'Q','U'/
C-----------------------------------------------------------------------
C                                       Get input parameters and create
C                                       output file if nec.
      CALL FLAGIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 995
      REPEAT = -1
      FCINIT = .FALSE.
      NLOOPS = 0
      NSTOKS = 1
      NBD = 0
      FLP = 0
C                                       Determine limits for all axes
C                                       if any autoscaling will be done.
      CALL SCAL (XZY, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       set image size
      NX = XYMAX(1) / UVCELL(1) + 1.0
      NX = NX + MAX (2, NX/50)
      NX = 2 * NX + 1
      NY = XYMAX(2) / UVCELL(2) + 1.0
      NY = NY + MAX (2, NY/50)
      NY = NY + 11
      XYSCL(1) = 1.0 / UVCELL(1)
      XYOFF(1) = - (NX / 2) * UVCELL(1)
      XYSCL(2) = 1.0 / UVCELL(2)
      XYOFF(2) = -10.0 * UVCELL(2)
      WRITE (MSGTXT,1000) UVCELL
      CALL MSGWRT (3)
      WRITE (MSGTXT,1001) NX, NY
      CALL MSGWRT (3)
C                                       Allocate memory
      NWORDS = (5 * NX * NY - 1) / 1024 + 4
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, TVCORE, TVADDR, IRET)
      IF (IRET.NE.0) GO TO 995
      NWORDS = (NX * NY - 1) / 1024 + 4
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MDCORE, MDADDR, IRET)
      IF (IRET.NE.0) GO TO 995
      NWORDS = (NBL * NX * NY - 1) / 1024 + 4
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, BLCORE, BLADDR, IRET)
      IF (IRET.NE.0) GO TO 995
      NWORDS = (2 * NX * NY - 1) / 1024 + 4
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, FVPNTR, FPADDR, IRET)
      IF (IRET.NE.0) GO TO 995
      NVI = (ECHAN - BCHAN + 1 - NCHAV) / CHINC + 1
      NVI = NVI * (EIF-BIF+1) * VISCNT
      NWORDS = (2 * NVI - 1) / 1024 + 4
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, FVCORE, FVADDR, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Reporting arrays
      NWORDS = (MANT * MANT * MIF * NPOL - 1) / 1024 + 1
      IF ((PRTLEV.GT.1) .AND. (REPEAT.LE.0)) THEN
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MALL,
     *      OFFALL, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MBAD,
     *      OFFBAD, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MSOM,
     *      OFFSOM, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      IF (PRTLEV.GT.1) THEN
         NWORDS = 1024 * NWORDS
         CALL FILL (NWORDS, 0, MALL(1+OFFALL))
         CALL FILL (NWORDS, 0, MBAD(1+OFFBAD))
         CALL FILL (NWORDS, 0, MSOM(1+OFFSOM))
         END IF
C                                       Plot data to image in memory
 20   NWORDS = 2 * NVI
      CALL FILL (NWORDS, 0, FVCORE(1+FVADDR))
      FVIRST = 0
      CALL GRIDUV (NPOL, XZY, NX, NY, TVCORE(1+TVADDR), NBL,
     *   BLCORE(1+BLADDR), FVPNTR(1+FPADDR), FVCORE(1+FVADDR), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (MSGTXT,1020) VISCNT
      CALL MSGWRT (3)
      WRITE (MSGTXT,1021) FVIRST, NVI
      CALL MSGWRT (3)
      NFIXED = 0
      NLOOPS = NLOOPS + 1
      NBDCOR = 0
      FLGPIX = 0
C                                       Interactive editor
      CALL EDITUV (NX, NY, TVCORE(1+TVADDR), NBL, BLCORE(1+BLADDR),
     *   FVPNTR(1+FPADDR), FVCORE(1+FVADDR), MDCORE(1+MDADDR), REPEAT,
     *   IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Write flag table
      CALL FLAGUV (NX, NY, TVCORE(1+TVADDR), MANT, MIF, NPOL, XZY,
     *   MALL(1+OFFALL), MBAD(1+OFFBAD), MSOM(1+OFFSOM), IRET)
      IF (IRET.NE.0) GO TO 990
      NBD = NBD + NBDCOR
      FLP = FLP + FLGPIX
C                                       report results
      IF ((PRTLEV.GT.1) .AND. (IRET.EQ.0)) CALL REPORT (MANT, MIF, NPOL,
     *   MALL(1+OFFALL), MBAD(1+OFFBAD), MSOM(1+OFFSOM), NBDCOR, FLGPIX)
C                                       Loop to do it all over
      IF (REPEAT.GT.0) THEN
         CALL FNDEXT ('FG', CATUV, I)
         IF (I.GE.FGVERO) THEN
            FGVERI = FGVERO
            FGVERO = FGNEW
            FGNEW = FGNEW + 1
            FGVER = FGVERI
            END IF
C                                       delete FC table
         IF (FCINIT) THEN
            CALL ZPHFIL ('FC', DISKIN, CNOIN, 1, PHNAME, IERR)
            CALL ZDESTR (DISKIN, PHNAME, IERR)
            VER = 1
            IF (IERR.EQ.0) THEN
               CALL CATFIX (DISKIN, CNOIN, 'WRIT')
               CALL DELEXT ('FC', DISKIN, CNOIN, 'WRIT', CATUV,BUFF1,
     *            VER, IERR)
               END IF
            IF (IERR.EQ.0) THEN
               MSGTXT = 'Flag command table deleted'
               CALL MSGWRT (2)
            ELSE
               MSGTXT = 'FLAG COMMAND TABLE FAILED TO BE DELETED'
               CALL MSGWRT (8)
               END IF
            END IF
         FCINIT = .FALSE.
         IF (REPEAT.GT.1) THEN
            DO 25 I = 1,12
               IF (STOKES.EQ.STCHAR(I)) THEN
                  IF (MOD(I,2).EQ.1) THEN
                     STOKES = STCHAR(I+1)
                  ELSE
                     STOKES = STCHAR(I-1)
                     END IF
                  IF ((I.EQ.9) .OR. (I.EQ.10)) THEN
                     ISCAL = (XISCAL.GT.0.0) .AND. (STOKES.EQ.'I')
                     END IF
                  MSGTXT = 'Changed STOKES to ' // STOKES
                  CALL MSGWRT (2)
                  NSTOKS = NSTOKS + 1
                  GO TO 30
                  END IF
 25            CONTINUE
            END IF
 30      GO TO 20
         END IF
C                                       Report deeds to History file
      IF ((IRET.EQ.0) .AND. (NBD.GT.0)) CALL FLAGHI (NX, NY, NLOOPS,
     *   NSTOKS, NBD, FLP)
C                                       Clear memory
 990  CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, TVCORE, TVADDR, IERR)
C                                       delete FC table
      IF (FCINIT) THEN
         CALL ZPHFIL ('FC', DISKIN, CNOIN, 1, PHNAME, IERR)
         CALL ZDESTR (DISKIN, PHNAME, IERR)
         VER = 1
         IF (IERR.EQ.0) THEN
            MSGSUP = 32000
            CALL CATFIX (DISKIN, CNOIN, 'WRIT')
            MSGSUP = 0
            CALL DELEXT ('FC', DISKIN, CNOIN, 'WRIT', CATUV, BUFF1, VER,
     *         IERR)
            END IF
         IF (IERR.EQ.0) THEN
            MSGTXT = 'Flag command table deleted'
            CALL MSGWRT (2)
         ELSE
            MSGTXT = 'FLAG COMMAND TABLE FAILED TO BE DELETED'
            CALL MSGWRT (8)
            END IF
         END IF
C                                       Close it down
 995  IRET = MAX (0, IRET)
      CALL DIE (IRET, SBUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('UV cell size',2F11.1,' wavelengths')
 1001 FORMAT ('UV grid size',2I6,' pixels')
 1020 FORMAT ('Read',I10,' visibility records')
 1021 FORMAT ('Used',I10,' of',I10,' locations in the linked',
     *   ' list table')
      END
      SUBROUTINE FLAGIN (PRGM, IRET)
C-----------------------------------------------------------------------
C   FLAGIN gets input parameters for UFLAG .
C   Inputs:
C      PRGM   C*6   Program name
C   Output:
C      IRET   I     Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
C
      CHARACTER UTYPE*2, STAT*4, cstok(12)*4
      INTEGER  IUSER, I, IERR, IROUND, NPARM, LUNTB, LUN, FQVER, NIF,
     *   NUMAN(513), JERR, JJ, J
      LOGICAL   TABLE, FITASC, F, MATCH, SNEXST, EXIST
      INCLUDE 'UFLAG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA F /.FALSE./
      DATA LUNTB /19/
      DATA CSTOK /'I','Q','U','V','RR','LL','RL','LR','XX','YY','XY',
     *   'YX'/
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
      NFGWRI = 0
C                                       Get input parameters.
      NPARM = 290
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
C                                       cell size
      IF ((CELLSZ(1).LE.0.0) .OR. (CELLSZ(2).LE.0.0) .OR.
     *   (XIMSIZ(1).LE.0.0) .OR. (XIMSIZ(2).LE.0.0)) THEN
         WRITE (MSGTXT,1001) CELLSZ, XIMSIZ
         IRET = 10
         CALL MSGWRT (8)
         GO TO 999
         END IF
      UVCELL(1) = (180.0D0 * 3600.0D0) / (PI * CELLSZ(1) * XIMSIZ(1))
      UVCELL(2) = (180.0D0 * 3600.0D0) / (PI * CELLSZ(2) * XIMSIZ(2))
      IF (APARM(1).LT.1.0) APARM(1) = 3.0
      IF (APARM(2).LT.1.0) APARM(2) = 3.0
      IF (APARM(3).LT.1.0) APARM(3) = 4.0
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XSTOK, STOKES)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), SOURCS(I))
         XSOUR(I) = ' '
 10      CONTINUE
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
      XFLAG = FGVERI
      FGVERO = IROUND (XFGOUT)
      IF ((FGVERO.LE.0) .OR. (FGVERO.GT.I)) FGVERO = I + 1
      XFGOUT = FGVERO
      IF (FGVERO.LE.I) FGVERI = -ABS(FGVERI)
      FGNEW = MAX (I+1, FGVERO+1)
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       check Stokes
      IF (STOKES.EQ.' ') STOKES = 'I'
      DO 20 I = 1,12
         IF (STOKES.EQ.CSTOK(I)) THEN
            IF ((I.NE.1) .AND. (I.NE.5) .AND. (I.NE.6) .AND. (I.NE.9)
     *         .AND. (I.NE.10)) XISCAL = -1.0
            GO TO 30
            END IF
 20      CONTINUE
      MSGTXT = 'STOKES ''' // STOKES // ''' NOT ALLOWED'
      CALL MSGWRT (8)
      IRET = 8
      GO TO 999
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
 30   LCOR0 = ICOR0
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      ISCAL = (XISCAL.GT.0.0)
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
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DO 85 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 85      CONTINUE
C                                       Get axis types.
      TYPEAX(1) = 6
      TYPEAX(2) = 7
      XYMAX(1) = -1.0E10
      XYMAX(2) = XYMAX(1)
      XYMIN(1) = 1.E10
      XYMIN(2) = XYMIN(1)
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
 1000 FORMAT ('FLAGIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1001 FORMAT ('CELLSIZE, IMSIZE IMPROPER',2F9.5,2F6.0)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK =',
     *   I3,' USER=',I5)
 1035 FORMAT ('ERROR',I3,' OBTAINING CATBLK ')
 1050 FORMAT ('NO SN FILE FOUND, BUT DOCALIB IS TRUE: NO CAL APPLIED')
 1060 FORMAT ('Plotting',I4,' frequency IDs.')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
      END
      SUBROUTINE SCAL (XZY, IRET)
C-----------------------------------------------------------------------
C   SCAL sends uv points one at a time to XYOFF.
C   Output:
C      XZY    R(5,*)   Work array
C      IRET   I        Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      REAL      XZY(5,*)
      INTEGER   IRET
C
      INTEGER   I, NUMVIS, XUMVIS, J, JJJ, ISUB, JSUB, NXVER, NIF,
     *   NXLUN, IROUND
      LOGICAL   REQBAS
      INCLUDE 'UFLAG.INC'
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
      VISCNT = 0
      XUMVIS = 0
      JSUB = SUBARR
      NXVER = 1
      NXLUN = 100
      CSOU = -1
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
                  IF (IRET.EQ.0) THEN
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
      VISCNT = NUMVIS
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
      SUBROUTINE GRIDUV (NP, XZY, NX, NY, TVCORE, NBL, BLCORE, FVPNTR,
     *   FVCORE, IRET)
C-----------------------------------------------------------------------
C   GRIDUV actually plots uv data into TVCORE
C   Inputs:
C      NX       I          X dimension of TVCORE
C      NY       I          Y dimension of TVCORE
C      NBL      I          Max # BL in BLCORE point
C   Output:
C      TVCORE   R(*,*,*)   Plot array (As, Re, Im, Wt, Cnt)
C      BLCORE   I(*,*,*)   Baseline numbers in TVCORE pixels
C      FVPNTR   I(2,*,*)   image pointer info
C      FVCORE   I(2,*)     Vis numbers in linked list
C      IRET     I          Return code, 0 => OK, otherwise abort.
C                              4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   NP, NX, NY, NBL, BLCORE(NBL,NX,NY), FVPNTR(2,NX,NY),
     *   FVCORE(2,*), IRET
      REAL      XZY(5,NP,*), TVCORE(NX,NY,5)
C
      INCLUDE 'UFLAG.INC'
      INCLUDE 'UFLAGPLT.INC'
      CHARACTER AUNITS*8, BNDCOD(MAXIF)*8
      INTEGER   I, IC, J, JJJ, ICO, NUMVIS, JSUB, ISUB, NXLUN, NIF,
     *   NXVER, LC, LF, LP, IX, IY, IBL
      REAL      BLC(2), TRC(2), TR, XY(2), SXYMIN(2), SXYMAX(2), WT,
     *   PHS, AMP
      HOLLERITH CATH(256)
      LOGICAL   REQBAS
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATH)
      DATA AUNITS /'Wavlngth'/
C-----------------------------------------------------------------------
      CALL FILL (9, 0, COUNT)
      IX = 5 * NX * NY
      CALL RFILL (IX, 0.0, TVCORE)
      IX = NBL * NX * NY
      CALL FILL (IX, 0, BLCORE)
      IX = 2 * NX * NY
      CALL FILL (IX, 0, FVPNTR)
      J = MAXANT * MAXANT
      CALL FILL (J, 0, BLFLAG)
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
      AXTYP(LOCNUM) = 0
      DO 30 I = 1,2
         TR = TRC(I) / XYSCL(I)
         RPLOC(I,LOCNUM) = BLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I)
         AXINC(I,LOCNUM) = TR / (TRC(I) - BLC(I))
         CPREF(I,LOCNUM) = ' '
         SXYMIN(I) = XYMIN(I)
         SXYMAX(I) = XYMAX(I)
         CTYP(I,LOCNUM) = AUNITS
         TVCATD(KDCRV+I-1) = RPVAL(I,LOCNUM)
         TVCATR(KRCRP+I-1) = RPLOC(I,LOCNUM)
         TVCATR(KRCIC+I-1) = AXINC(I,LOCNUM)
         CALL CHR2H (8, CTYP(I,LOCNUM), 1, CATH(KHCTP+2*(I-1)))
 30      CONTINUE
      TVCAT(KINAX) = NX
      TVCAT(KINAX+1) = NY
      NUMVIS = 0
      WRITE (MSGTXT,1030) 'X', CPREF(1,LOCNUM), CTYP(1,LOCNUM)(:12),
     *   SXYMIN(1), SXYMAX(1)
      CALL MSGWRT (3)
      WRITE (MSGTXT,1030) 'Y', CPREF(2,LOCNUM), CTYP(2,LOCNUM)(:12),
     *   SXYMIN(2), SXYMAX(2)
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
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1100) IRET
               CALL MSGWRT (8)
               IRET = 4
               GO TO 999
            ELSE IF (IRET.EQ.0) THEN
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
               IBL = 10000*I + J
               NUMVIS = NUMVIS + 1
               IF (CURSOU.NE.CSOU) THEN
                  CSOU = CURSOU
                  CALL GETSOU (CSOU, DISKIN, CNOIN, CATUV, NXLUN, IRET)
                  IF (IRET.EQ.0) THEN
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
               WT = 1.0
               DO 135 LF = BIF,EIF
                  DO 130 IC = 1,ICO,CHINC
                     LC = LC + 1
                     DO 129 LP = 1,NPOL
                        IF ((XZY(1,LP,LC).NE.FBLANK) .AND.
     *                     (XZY(2,LP,LC).NE.FBLANK) .AND.
     *                     (XZY(5,LP,LC).GT.0.0)) THEN
                           DO 120 J = 1,2
                              XY(J) = XYSCL(J) *
     *                           (XZY(J,LP,LC)-XYOFF(J))
 120                          CONTINUE
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
                                 IF (DOWEGT.GT.0.0) WT = XZY(5,LP,LC)
                                 TVCORE(IX,IY,1) = TVCORE(IX,IY,1) +
     *                              WT * SQRT (XZY(3,LP,LC)**2 +
     *                              XZY(4,LP,LC)**2)
                                 TVCORE(IX,IY,2) = TVCORE(IX,IY,2) +
     *                              WT * XZY(3,LP,LC)
                                 TVCORE(IX,IY,3) = TVCORE(IX,IY,3) +
     *                              WT * XZY(4,LP,LC)
                                 TVCORE(IX,IY,4) = TVCORE(IX,IY,4) + WT
                                 TVCORE(IX,IY,5) = TVCORE(IX,IY,5) + 1.0
                                 CALL COUNTR (NX, NY, IX, IY, IBL,
     *                              NBL, BLCORE, FSTVS3, FVIRST, NVI,
     *                              FVPNTR, FVCORE)
                              ELSE
                                 COUNT(2,3) = COUNT(2,3) + 1
                                 END IF
                              END IF
                           END IF
 129                    CONTINUE
 130                 CONTINUE
 135              CONTINUE
               GO TO 100
               END IF
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
      PRANGE(1,1) = 1.E10
      PRANGE(2,1) = -PRANGE(1,1)
      PRANGE(1,2) = PRANGE(1,1)
      PRANGE(2,2) = -PRANGE(1,1)
      PRANGE(1,3) = PRANGE(1,1)
      PRANGE(2,3) = -PRANGE(1,1)
      PRANGE(1,4) = 180.0
      PRANGE(2,4) = -180.0
      CALL RFILL (8, 0.0, PRANGE(1,5))
      MBL = 1
      MVISP = 0
      DO 200 J = 1,NY
         DO 190 I = 1,NX
            IF ((TVCORE(I,J,5).GE.1.0) .AND. (TVCORE(I,J,4).GT.0.0))
     *         THEN
               DO 180 IBL = MBL,NBL
                  IF (BLCORE(IBL,I,J).NE.0) MBL = MAX (MBL, IBL)
 180              CONTINUE
               MVISP = MAX (MVISP, FVPNTR(1,I,J))
C                                       average data
               WT = TVCORE(I,J,4)
               TVCORE(I,J,1) = TVCORE(I,J,1) / WT
               AMP = SQRT (TVCORE(I,J,2)**2 + TVCORE(I,J,3)**2) / WT
               IF (AMP.LE.0.0) THEN
                  PHS = 0.0
               ELSE
                  PHS = RAD2DG * ATAN2 (TVCORE(I,J,3), TVCORE(I,J,2))
                  IF (PHS.GT.180.0) PHS = PHS - 360.0
                  IF (PHS.LT.-180.0) PHS = PHS + 360.0
                  END IF
               TVCORE(I,J,2) = AMP
               TVCORE(I,J,3) = PHS
               PRANGE(1,1) = MIN (PRANGE(1,1), TVCORE(I,J,1))
               PRANGE(2,1) = MAX (PRANGE(2,1), TVCORE(I,J,1))
               PRANGE(1,2) = MIN (PRANGE(1,2), TVCORE(I,J,2))
               PRANGE(2,2) = MAX (PRANGE(2,2), TVCORE(I,J,2))
               PRANGE(1,3) = MIN (PRANGE(1,3), TVCORE(I,J,3))
               PRANGE(2,3) = MAX (PRANGE(2,3), TVCORE(I,J,3))
               IF (TVCORE(I,J,5).EQ.1.0) THEN
                  TVCORE(I,J,4) = -1.0
               ELSE
                  TVCORE(I,J,4) = TVCORE(I,J,1) - TVCORE(I,J,2)
                  PRANGE(1,4) = MIN (PRANGE(1,4), TVCORE(I,J,4))
                  PRANGE(2,4) = MAX (PRANGE(2,4), TVCORE(I,J,4))
                  END IF
               TVCORE(I,J,5) = TVCORE(I,J,5) + 1.0
            ELSE
               TVCORE(I,J,1) = FBLANK
               TVCORE(I,J,2) = FBLANK
               TVCORE(I,J,3) = FBLANK
               TVCORE(I,J,4) = FBLANK
               END IF
 190        CONTINUE
 200     CONTINUE
      IF (MBL.GE.NBL) THEN
         WRITE (MSGTXT,1200) NBL
         CALL MSGWRT (6)
         MSGTXT = 'This will complicate flagging by baseline'
         CALL MSGWRT (6)
         MBL = NBL - 1
         END IF
      WRITE (MSGTXT,1201) MVISP
      CALL MSGWRT (2)
C                                      Set Stokes flag
      CALL LFILL (4, .TRUE., PFLAGS)
      IF ((ICOR0.LT.0) .AND. (LCOR0.LT.0)) THEN
         IF (ONEPOL) THEN
            IF (STOKES.EQ.'RR') PFLAGS(2) = .FALSE.
            IF (STOKES.EQ.'LL') PFLAGS(1) = .FALSE.
            END IF
         IF (NOTCRS) THEN
            PFLAGS(3) = .FALSE.
            PFLAGS(4) = .FALSE.
            END IF
         IF (STOKES.EQ.'RL') THEN
            PFLAGS(4) = .FALSE.
            IF (NOTCRS) THEN
               PFLAGS(1) = .FALSE.
               PFLAGS(2) = .FALSE.
               END IF
         ELSE IF (STOKES.EQ.'LR') THEN
            PFLAGS(3) = .FALSE.
            IF (NOTCRS) THEN
               PFLAGS(1) = .FALSE.
               PFLAGS(2) = .FALSE.
               END IF
            END IF
      ELSE IF ((ICOR0.GT.0) .AND. (LCOR0.GT.0)) THEN
         IF ((STOKES.EQ.'Q') .OR. (STOKES.EQ.'U')) THEN
            IF (NOTCRS) THEN
               PFLAGS(1) = .FALSE.
               PFLAGS(4) = .FALSE.
               END IF
            END IF
      ELSE IF ((ICOR0.GT.0) .AND. (LCOR0.LT.0)) THEN
         IF ((STOKES.EQ.'Q') .OR. (STOKES.EQ.'U')) THEN
            IF (NOTCRS) THEN
               PFLAGS(1) = .FALSE.
               PFLAGS(2) = .FALSE.
               END IF
            END IF
      ELSE IF ((ICOR0.LT.0) .AND. (LCOR0.GT.0)) THEN
         IF ((STOKES.EQ.'RL') .OR. (STOKES.EQ.'LR')) THEN
            IF (NOTCRS) THEN
               PFLAGS(1) = .FALSE.
               PFLAGS(4) = .FALSE.
               END IF
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('GRIDUV: ',A1,' axis in ',A,A,2X,2(1PE11.3))
 1050 FORMAT ('GRIDUV: ERROR',I3,' INIT VIS FILE')
 1100 FORMAT ('GRIDUV: ERROR',I3,' READING VIS FILE')
 1150 FORMAT ('GRIDUV: ',I10,' Points put in array')
 1155 FORMAT ('GRIDUV: ',I10,' Points did not fit')
 1160 FORMAT ('GRIDUV: ',I10,' * ',I10,' * ',I10)
 1200 FORMAT ('Warning: more than',I3,' baselines at a single point')
 1201 FORMAT ('Maximum visibility records in a single cell is',I6)
      END
      SUBROUTINE COUNTR (NX, NY, IX, IY, IBL, NBL, BLCORE, FSTVS3,
     *   FVIRST, NVI, FVPNTR, FVCORE)
C-----------------------------------------------------------------------
C   Records the baseline in BLCORE(i,IX,IY) if possible
C   Inputs:
C      NX       I            X dimension of BLCORE
C      NY       I            Y dimension of BLCORE
C      IX       I            X pixel to do counting
C      IY       I            Y pixel to do counting
C      IBL      I            Baseline code
C      NBL      I            Max number BLs in BLCORE
C      NVI      I            Max size of FVCORE
C   In/Out:
C      BLCORE   I(10,NX,NY)   array of baseline codes
C      FVCORE   I(2,*)       linked list of vis record numbers
C      FVIRST   i            Number positions used in FVCORE
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IX, IY, IBL, NBL, BLCORE(NBL,NX,NY), FSTVS3,
     *   FVIRST, NVI, FVPNTR(2,NX,NY), FVCORE(2,*)
C
      INTEGER   I, J, INEXT, NVALS, IHIGH
C-----------------------------------------------------------------------
C                                       baseline numbers
      J = NBL
      DO 10 I = 1,NBL-1
         IF (IBL.EQ.ABS(BLCORE(I,IX,IY))) GO TO 100
         IF (BLCORE(I,IX,IY).EQ.0) J = MIN (J, I)
 10      CONTINUE
      IF (J.EQ.NBL) THEN
         BLCORE(NBL,IX,IY) = -1000000
      ELSE
         BLCORE(J,IX,IY) = IBL
         END IF
C                                       vis numbers
 100  NVALS = FVPNTR(1,IX,IY)
      INEXT = FVPNTR(2,IX,IY)
      DO 110 J = 1,NVALS
         IF (J.EQ.NVALS) IHIGH = INEXT
         IF (FSTVS3.EQ.FVCORE(1,INEXT)) GO TO 999
         INEXT = FVCORE(2,INEXT)
 110     CONTINUE
C                                       add to linked list
      FVIRST = FVIRST + 1
      IF (FVIRST.LE.NVI) THEN
         INEXT = FVIRST
         FVPNTR(1,IX,IY) = FVPNTR(1,IX,IY) + 1
         IF (FVPNTR(1,IX,IY).EQ.1) THEN
            FVPNTR(2,IX,IY) = INEXT
         ELSE
            FVCORE(2,IHIGH) = INEXT
            END IF
         FVCORE(1,INEXT) = FSTVS3
         END IF
C
 999  RETURN
      END
      SUBROUTINE COUNTB (NX, NY, IX, IY, IBL, NBL, BLCORE)
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
C                                       baseline numbers
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
      SUBROUTINE EDITUV (NX, NY, TVCORE, NBL, BLCORE, FVPNTR, FVCORE,
     *   MDCORE, REPEAT, IRET)
C-----------------------------------------------------------------------
C   EDITUV displays the uv data on the TV and offers editing options
C   Inputs:
C      NX       I          X dimension of TVCORE
C      NY       I          Y dimension of TVCORE
C      NBL      I          Max # baselines in BLCORE
C   In/out:
C      TVCORE   R(*,*,5)   Plot : As, Av, 0, Wt, Ct+1
C      BLCORE   I(*,*,*)   Baseline numbers in tvcore pixels
C      FVPNTR   i(2,*,*)   Pointer/counter arra for vis numbers
C      FVCORE   I(2,*)     Visibility numbers linked list
C      MDCORE   R(*)       Buffer to compute median value
C   Output:
C      REPEAT   I          Apply flags and repeat
C      IRET     I          Return code, 0 => OK, otherwise abort.
C                             4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NBL, BLCORE(NBL,NX,NY), FVPNTR(2,NX,NY),
     *   FVCORE(2,*), REPEAT, IRET
      REAL      TVCORE(NX,NY,5), MDCORE(*)
C
      INCLUDE 'UFLAG.INC'
      INCLUDE 'UFLAGPLT.INC'
      INCLUDE 'INCS:PTVC.INC'
      INTEGER   MC
      PARAMETER (MC = 37)
C
      INTEGER   TVC(4), IMW(4), IX, IY, IGR, I, IGM, MTYP, MCOL, CHS,
     *   MROW(2), MGRS(2), TOPSEP, TIMLIM, TVBUT, IP, ITY, ISN, IGB,
     *   SVZOOM(3), BX(4), BY(4), PZERO(2), CATTMP(256), IGBB, NTITLE,
     *   SIDSEP, NLEVS, IYTV, LUTBUF(TVMLUT), II, JINC, ISWIN, JTRIM,
     *   IXLS(4), MCH, PWINTV(4)
      CHARACTER ISHELP*6, TITLE*24, CHOICS(MC)*13, CHTYPE(2)*9, TEXT*24,
     *   TRTYP(4)*4, IMTYP(4)*8, STCHAR*4, TTEXT*24
      LOGICAL   LEAVE(MC), MENUOK, BORDER(4), ZERO(2), WINSET
      REAL      TEMP, RPOS(2)
      DOUBLE PRECISION PTEMP(2)
      HOLLERITH CATH(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (BUFF1, LUTBUF), (CATBLK, CATH)
      DATA MCOL, TOPSEP, SIDSEP, TIMLIM /1, 8, 8, 0/
      DATA CHOICS /'ABORT', 'FLAG + EXIT', 'FLAG + REPEAT',
     *   'FLAG + SWITCH', ' ', 'OFF ZOOM', 'TVZOOM', 'OFF TRANS',
     *   'OFF COLOR', 'TVTRANSF', 'TVPSEUDO', 'TVPHLAME',
     *   'SET PIXRANGE', 'LOAD SQRT', 'VIEW SCALAR', 'VIEW ONLY',
     *   'VIEW ALL', 'FLAG PTS OFF', 'SET WINDOW', 'RESET WINDOW',
     *   ' ', 'FLAG POINT', 'FLAG AREA', 'FLAG FAST', 'FLAG BASELINE',
     *   'UNFLAG POINT', 'UNFLAG AREA', 'UNFLAG FAST','UNFLAG BASEL',
     *   ' ', 'EXAMINE VIS', 'USER FLAG VIS', 'AUTO FLAG VIS',
     *   4*' '/
      DATA LEAVE /6*.TRUE., .FALSE., 2*.TRUE., 3*.FALSE., 3*.TRUE.,
     *   .TRUE., 2*.TRUE., .FALSE., 2*.TRUE., 8*.FALSE., .TRUE.,
     *   .FALSE., 6*.TRUE./
      DATA CHTYPE /'U ', 'V '/
      DATA TRTYP /'LOG ','SQRT','LOG2','LIN '/
      DATA IMTYP /'VECTOR', 'PHASE', 'SCAL-VEC', 'SCALAR'/
C-----------------------------------------------------------------------
      REPEAT = -1
      FLAGON = .TRUE.
      WINSET = .FALSE.
      CALL FILL (4, 0, NFLGAL)
      ITRTYP = 1
      IMGTYP = 2
      GRANGE(1) = PRANGE(1,IMGTYP)
      GRANGE(2) = PRANGE(2,IMGTYP)
      XRANGE(1) = -20000.
      XRANGE(2) = 100000.
      CALL TVOPEN (SBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      TVMAXX(1) = MAXXTV(1) - 7 * CSIZTV(1)
      TVMAXX(2) = MAXXTV(2) - 5 * CSIZTV(2) - 26
      MTYP = XDOCEN + 1.5
      NLEVS = LUTOUT + 1
      IP = JTRIM (STOKES)
      STCHAR = '(' // STOKES(:IP) // ')'
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
         IF ((I.EQ.1) .OR. ((I.GT.NGRAY) .AND. (I.LE.NGRAY+4))) THEN
            CALL YZERO (I, IRET)
            IF (IRET.NE.0) GO TO 990
            CALL YSLECT ('ONNN', I, 0, TVSCR, IRET)
         ELSE
            CALL YSLECT ('OFFF', I, 0, TVSCR, IRET)
            END IF
         IF (IRET.NE.0) GO TO 990
 5       CONTINUE
      IGM = NGRAY + 2
      CALL YSLECT ('OFFF', IGM, 0, TVSCR, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL YHOLD ('OFFF', IRET)
      MENUOK = .FALSE.
      TITLE =' '
      NTITLE = 0
C                                       off transfer
      IYTV = MAXINT + 1
      TEMP = (LUTOUT+1.0) / (MAXINT+1.0)
      DO 10 I = 1,IYTV
         LUTBUF(I) = I * TEMP - 0.5
 10      CONTINUE
      I = 1
      CALL YLUT ('WRIT', I, 7, .FALSE., LUTBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OFF TRANSF'
         GO TO 990
         END IF
C                                       off color
      I = OFMINP + 1
      CALL RFILL (I, 0.0, BUFF1)
      NLEVS = LUTOUT + 1
      IF (I.LT.NLEVS) NLEVS = I
      PTEMP(1) = 1.0 / REAL (OFMOUT)
      PTEMP(2) = PTEMP(1) * (OFMOUT + 1.0) / REAL(NLEVS)
      DO 20 I = 1,NLEVS
         BUFF1(I) = I * PTEMP(2) - PTEMP(1)
 20      CONTINUE
      I = (OFMINP + 1) / NLEVS
      IYTV = NLEVS
      DO 25 II = 2,I
         CALL RCOPY (NLEVS, BUFF1, BUFF1(IYTV+1))
         IYTV = IYTV + NLEVS
25       CONTINUE
      CALL YOFM ('WRIT', 7, .FALSE., BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OFF COLOR'
         GO TO 990
         END IF
C                                       where is the value 0?
      TEMP = - XYSCL(1) * XYOFF(1)
      PZERO(1) = TEMP + 1.0
      TEMP = - XYSCL(2) * XYOFF(2)
      PZERO(2) = TEMP + 1.0
C                                       full image again
 30   IMW(1) = 1
      IMW(2) = 1
      IMW(3) = NX
      IMW(4) = NY
C                                       does it fit
 40   I = 1
      II = 1
      IX = IMW(3) - IMW(1) + 1
      IY = IMW(4) - IMW(2) + 1
 45   IF (((IX+I-1)/I.GT.TVMAXX(1)) .OR. ((IY+I-1)/I.GT.TVMAXX(2))) THEN
         I = I + 1
         GO TO 45
         END IF
C                                       Must smooth
      IF (I.GT.1) THEN
         WRITE (MSGTXT,1025) I
         CALL MSGWRT (4)
C                                       can we pixel replicate
      ELSE
 50      IF ((IX*II.LT.TVMAXX(1)) .AND. (IY*II.LT.TVMAXX(2))) THEN
            II = II + 1
            GO TO 50
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
      CALL COPY (4, WINDTV, PWINTV)
C                                       window location
      IX = (IX + I - 1) / I * II
      IY = (IY + I - 1) / I * II
      TVC(1) = (TVMAXX(1) - IX)/2 + 1 + (MAXXTV(1)-TVMAXX(1))
      TVC(3) = TVC(1) + IX - 1
      TVC(2) = WINDTV(2) + 3*CSIZTV(2) + 9
      TVC(4) = TVC(2) + IY - 1
      IF (TVC(4)+17+1.6*CSIZTV(2).GE.MAXXTV(2)) THEN
         TVC(2) = 1 + 5 * CSIZTV(2)
         TVC(4) = TVC(2) + IY - 1
         IF (TVC(4)+17+1.6*CSIZTV(2).GE.MAXXTV(2)) THEN
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
      JINC = I
      IF (II.GT.1) JINC = -II
      CALL TVLOAD (NX, NY, TVCORE, JINC, TVC, IMW, MDCORE, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       draw border and label
      IGBB = 1
      IGB = NGRAY + IGBB
      CALL YZERO (IGB, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL COPY (256, CATBLK, CATTMP)
      CALL COPY (256, TVCAT, CATBLK)
      TEXT = CHTYPE(1)
      CALL CHR2H (8, TEXT, 1, CATH(KHCTP))
      TEXT = CHTYPE(2)
      CALL CHR2H (8, TEXT, 1, CATH(KHCTP+2))
      CALL IAXIS1 (TVSCR, LTYPE, IGBB, 1, .FALSE., IRET)
      CALL COPY (256, CATTMP, CATBLK)
      CALL REMETS (CPREF(1,LOCNUM), SCALXY(1))
      CALL REMETS (CPREF(2,LOCNUM), SCALXY(2))
C                                       draw lines at zero
      IF (ZERO(1)) THEN
         BX(1) = TVC(1) + (PZERO(1) - IMW(1)) / I * II
         BX(2) = BX(1)
         BY(1) = MAX (1, TVC(2) - 1)
         BY(2) = MIN (MAXXTV(2), TVC(4) + 1)
         CALL IMVECT ('ONNN', IGB, 2, BX, BY, TVSCR, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      IF (ZERO(2)) THEN
         BY(1) = TVC(2) + (PZERO(2) - IMW(2)) / I * II
         BY(2) = BY(1)
         BX(1) = MAX (1, TVC(1) - 1)
         BX(2) = MIN (MAXXTV(1), TVC(3) + 1)
         CALL IMVECT ('ONNN', IGB, 2, BX, BY, TVSCR, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       also around wedge
      BX(1) = TVC(1)-1
      BX(2) = TVC(1)-1
      BX(3) = TVC(3)+1
      BX(4) = TVC(3)+1
      BY(1) = TVC(4)+1
      BY(2) = BY(1) + 16
      BY(3) = BY(2)
      BY(4) = BY(1)
      CALL IMVECT ('ONNN', IGB, 4, BX, BY, TVSCR, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       label wedge
      IF (IMGTYP.EQ.1) THEN
         TEXT = 'SCALAR ' // STCHAR
      ELSE IF (IMGTYP.EQ.2) THEN
         TEXT = 'VECTOR ' // STCHAR
      ELSE IF (IMGTYP.EQ.3) THEN
         TEXT = 'PHASE ' // STCHAR
      ELSE
         TEXT = 'SCALAR-VECTOR ' // STCHAR
         END IF
      IP = JTRIM (TEXT)
      BX(1) = (TVC(1) + TVC(3) - IP * CSIZTV(1)) / 2
      IXLS(2) = BX(1)
      IXLS(3) = (TVC(1) + TVC(3) + IP * CSIZTV(1)) / 2
      BY(1) = TVC(4) + 17 + 0.5*CSIZTV(2)
      CALL IMCHAR (IGB, BX(1), BY(1), 0, 0, TEXT(:IP), TVSCR, IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (TEXT,1030) VINFO(1)
      CALL CHTRIM (TEXT, 9, TEXT, IP)
      IP = MIN (5, IP)
      BX(1) = TVC(1) - (IP-4)*CSIZTV(1)
      BX(1) = MAX (BX(1), CSIZTV(1))
      IXLS(1) = BX(1) + IP * CSIZTV(1)
      CALL IMCHAR (IGB, BX(1), BY(1), 0, 0, TEXT(:IP), TVSCR, IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (TEXT,1030) VINFO(2)
      CALL CHTRIM (TEXT, 9, TEXT, IP)
      IP = MIN (5, IP)
      BX(1) = TVC(3) - (IP-4)*CSIZTV(1)
      BX(1) = MIN (BX(1), MAXXTV(1)-(IP+1)*CSIZTV(1))
      IXLS(4) = BX(1)
      CALL IMCHAR (IGB, BX(1), BY(1), 0, 0, TEXT(:IP), TVSCR, IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (TEXT,1030) VINFO(5)
      CALL CHTRIM (TEXT, 9, TEXT, IP)
      IP = MIN (5, IP)
      MCH = (IXLS(2) - IXLS(1)) / CSIZTV(1)
      IF (MCH.GT.9+IP) THEN
         BX(1) = (IXLS(1) + IXLS(2) - (7+IP)*CSIZTV(1)) / 2
         TTEXT = 'MEDIAN ' // TEXT(:IP)
         IP = IP + 7
         CALL IMCHAR (IGB, BX(1), BY(1), 0, 0, TTEXT(:IP), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 990
      ELSE IF (MCH.GT.2+IP) THEN
         BX(1) = (IXLS(1) + IXLS(2) - IP*CSIZTV(1)) / 2
         CALL IMCHAR (IGB, BX(1), BY(1), 0, 0, TEXT(:IP), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      WRITE (TEXT,1030) VINFO(3)
      CALL CHTRIM (TEXT, 9, TEXT, IP)
      IP = MIN (5, IP)
      WRITE (TTEXT,1030) VINFO(4)
      CALL CHTRIM (TTEXT, 9, TTEXT, MCH)
      MCH = MIN (5, MCH)
      TEXT = TEXT(:IP) // ' (' // TTEXT(:MCH) // ')'
      IP = IP + 3 + MCH
      MCH = (IXLS(4) - IXLS(3)) / CSIZTV(1)
      IF (MCH.GT.7+IP) THEN
         BX(1) = (IXLS(3) + IXLS(4) - (5+IP)*CSIZTV(1)) / 2
         TTEXT = 'MEAN ' // TEXT(:IP)
         IP = IP + 5
         CALL IMCHAR (IGB, BX(1), BY(1), 0, 0, TTEXT(:IP), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 990
      ELSE IF (MCH.GT.2+IP) THEN
         BX(1) = (IXLS(3) + IXLS(4) - IP*CSIZTV(1)) / 2
         CALL IMCHAR (IGB, BX(1), BY(1), 0, 0, TEXT(:IP), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       stats
      WRITE (MSGTXT,1030) VINFO(5)
C                                       force sub-image
      IF (I.GT.1) THEN
         MSGTXT = 'Select a sub-image to edit'
         CALL MSGWRT (1)
         IGR = 3 + NGRAY
         CALL WIPBOX (IGR, TVC, IMW, TVSCR, IRET)
         IF (IRET.NE.0) GO TO 990
         GO TO 40
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
      IF ((WINDTV(1).NE.PWINTV(1)) .OR. (WINDTV(2).NE.PWINTV(2)) .OR.
     *   (WINDTV(3).NE.PWINTV(3)) .OR. (WINDTV(4).NE.PWINTV(4)))
     *   GO TO 40
C                                       force window large enough
      IF ((TVC(1).LT.WINDTV(1)) .OR. (TVC(2).LT.WINDTV(2)) .OR.
     *   (TVC(3).GT.WINDTV(3)) .OR. (TVC(4).GT.WINDTV(4))) THEN
         WINDTV(1) = MIN (WINDTV(1), TVC(1))
         WINDTV(2) = MIN (WINDTV(2), TVC(2))
         WINDTV(3) = MAX (WINDTV(3), TVC(3))
         WINDTV(4) = MAX (WINDTV(4), TVC(4))
         CALL YWINDO ('WRIT', WINDTV, IRET)
         END IF
      CHOICS(14) = 'LOAD ' // TRTYP(ITRTYP)
      CHOICS(15) = 'VIEW ' // IMTYP(IMGTYP)
      IF (FLAGON) THEN
         CHOICS(18) = 'FLAG PTS OFF'
      ELSE
         CHOICS(18) = 'FLAG PTS ON'
         END IF
      MROW(1) = 18
      MROW(2) = MC - 4 - MROW(1)
      I = MC - 4
      IF (((XRANGE(1).GE.GRANGE(1)) .AND. (XRANGE(1).LT.GRANGE(2))) .OR.
     *   ((XRANGE(2).GT.GRANGE(1)) .AND. (XRANGE(2).LE.GRANGE(2)))) THEN
         MROW(2) = MROW(2) + 1
         I = I + 1
         CHOICS(I) = 'FLAG ALL'
         IF (WINSET) THEN
            MROW(2) = MROW(2) + 1
            I = I + 1
            CHOICS(I) = 'FLAG ALL WIN'
            END IF
         END IF
      IF (NFLGAL(IMGTYP).GT.0) THEN
         MROW(2) = MROW(2) + 1
         I = I + 1
         CHOICS(I) = 'UNDO FLAG ALL'
         IF (WINSET) THEN
            MROW(2) = MROW(2) + 1
            I = I + 1
            CHOICS(I) = 'UNDO FLAG WIN'
            END IF
         END IF
      MCOL = 2
      I = -1
      CALL TVMENU (I, MCOL, MROW, MGRS, TOPSEP, SIDSEP, ISHELP, CHOICS,
     *   TIMLIM, LEAVE, NTITLE, TITLE, CHS, TVBUT, TVSCR, IRET)
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
      ELSE IF (CHOICS(CHS).EQ.'OFF TRANS') THEN
         IYTV = MAXINT + 1
         TEMP = (LUTOUT+1.0) / (MAXINT+1.0)
         DO 110 I = 1,IYTV
            LUTBUF(I) = I * TEMP - 0.5
 110        CONTINUE
         I = 1
         CALL YLUT ('WRIT', I, 7, .FALSE., LUTBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OFF TRANSF'
            GO TO 990
            END IF
      ELSE IF (CHOICS(CHS).EQ.'OFF COLOR') THEN
         I = OFMINP + 1
         CALL RFILL (I, 0.0, BUFF1)
         NLEVS = LUTOUT + 1
         IF (I.LT.NLEVS) NLEVS = I
         PTEMP(1) = 1.0 / REAL (OFMOUT)
         PTEMP(2) = PTEMP(1) * (OFMOUT + 1.0) / REAL(NLEVS)
         DO 120 I = 1,NLEVS
            BUFF1(I) = I * PTEMP(2) - PTEMP(1)
 120        CONTINUE
         I = (OFMINP + 1) / NLEVS
         IYTV = NLEVS
         DO 130 II = 2,I
            CALL RCOPY (NLEVS, BUFF1, BUFF1(IYTV+1))
            IYTV = IYTV + NLEVS
 130        CONTINUE
         CALL YOFM ('WRIT', 7, .FALSE., BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OFF COLOR'
            GO TO 990
            END IF
      ELSE IF (CHOICS(CHS).EQ.'TVTRANSF') THEN
         I = 1
         IYTV = 1
         CALL IENHNS (I, 7, IYTV, RPOS, BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'TVTRANSF'
            GO TO 980
            END IF
      ELSE IF (CHOICS(CHS).EQ.'TVPSEUDO') THEN
         CALL TVPSUD (NLEVS, BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'TVPSEUDO'
            GO TO 980
            END IF
      ELSE IF (CHOICS(CHS).EQ.'TVPHLAME') THEN
         CALL TVFLAM (NLEVS, BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'TVPHLAME'
            GO TO 980
            END IF

      ELSE IF (CHOICS(CHS).EQ.'SET PIXRANGE') THEN
         MSGTXT = 'Enter min and max flux'
         IF (IMGTYP.EQ.3) MSGTXT = 'Enter min and max phase'
         CALL INQFLT (TTY, MSGTXT, 2, PTEMP, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR READING THE VALUES'
            CALL MSGWRT (6)
         ELSE
            PRANGE(1,4+IMGTYP) = PTEMP(1)
            PRANGE(2,4+IMGTYP) = PTEMP(2)
            IF (PTEMP(1).LT.PTEMP(2)) THEN
               GRANGE(1) = PTEMP(1)
               GRANGE(2) = PTEMP(2)
            ELSE
               GRANGE(1) = PRANGE(1,IMGTYP)
               GRANGE(2) = PRANGE(2,IMGTYP)
               END IF
            GO TO 40
            END IF
      ELSE IF (CHOICS(CHS)(:5).EQ.'LOAD ') THEN
         ITRTYP = ITRTYP + 1
         IF (ITRTYP.EQ.5) ITRTYP = 1
         MENUOK = .FALSE.
         GO TO 40
      ELSE IF (CHOICS(CHS)(:9).EQ.'FLAG PTS ') THEN
         FLAGON = .NOT.FLAGON
         MENUOK = .FALSE.
         I = 4 + NGRAY
         IF (FLAGON) THEN
            CALL YSLECT ('ONNN', I, 0, TVSCR, IRET)
         ELSE
            CALL YSLECT ('OFFF', I, 0, TVSCR, IRET)
            END IF
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, CHOICS(CHS)
            GO TO 980
            END IF
         GO TO 40
      ELSE IF (CHOICS(CHS).EQ.'VIEW ONLY') THEN
         CALL GRVIEW (TVC, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'VIEW ONLY'
            GO TO 980
            END IF
         MENUOK = .FALSE.
         GO TO 40
      ELSE IF (CHOICS(CHS).EQ.'VIEW ALL') THEN
         XRANGE(1) = -20000.
         XRANGE(2) = 100000.
         MENUOK = .FALSE.
         GO TO 40
      ELSE IF (CHOICS(CHS)(:5).EQ.'VIEW ') THEN
         IMGTYP = IMGTYP + 1
         IF (IMGTYP.EQ.5) IMGTYP = 1
         IF (PRANGE(1,4+IMGTYP).GE.PRANGE(2,4+IMGTYP)) THEN
            GRANGE(1) = PRANGE(1,IMGTYP)
            GRANGE(2) = PRANGE(2,IMGTYP)
            END IF
C                                       change types drop XRANGE
         XRANGE(1) = -20000.
         XRANGE(2) = 100000.
         MENUOK = .FALSE.
         GO TO 40
      ELSE IF (CHOICS(CHS).EQ.'SET WINDOW') THEN
         MSGTXT = 'Select a sub-image to edit'
         CALL MSGWRT (1)
         IGR = 3 + NGRAY
         CALL WIPBOX (IGR, TVC, IMW, TVSCR, IRET)
         IF (IRET.NE.0) GO TO 990
         WINSET = .TRUE.
         MENUOK = .FALSE.
         TVZOOM(2) = MAXXTV(1)/2
         TVZOOM(3) = MAXXTV(2)/2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., IRET)
         IF (IRET.EQ.0) GO TO 40
      ELSE IF (CHOICS(CHS).EQ.'RESET WINDOW') THEN
         WINSET = .FALSE.
         MENUOK = .FALSE.
         TVZOOM(1) = 0
         TVZOOM(2) = MAXXTV(1)/2
         TVZOOM(3) = MAXXTV(2)/2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., IRET)
         IF (IRET.EQ.0) GO TO 30
      ELSE IF (CHOICS(CHS).EQ.'FLAG + EXIT') THEN
         GO TO 990
      ELSE IF (CHOICS(CHS).EQ.'FLAG + REPEAT') THEN
         REPEAT = 1
         MSGTXT = '*** Warning - writing flag table and reloading data'
         CALL MSGWRT (5)
         GO TO 990
      ELSE IF (CHOICS(CHS).EQ.'FLAG + SWITCH') THEN
         REPEAT = 2
         MSGTXT = '*** Warning - writing flag table and reloading' //
     *      ' new STOKES'
         CALL MSGWRT (5)
         GO TO 990
      ELSE IF (CHOICS(CHS).EQ.'ABORT') THEN
         MSGTXT = 'ABORT selected - any editing is discarded'
         CALL MSGWRT (6)
         IRET = -1
      ELSE IF (CHOICS(CHS).EQ.'EXAMINE VIS') THEN
         ITY = 5
         ISN = 0
         MENUOK = .FALSE.
         CALL WIPEIT (ITY, ISN, NX, NY, TVCORE, NBL, BLCORE, FVPNTR,
     *      FVCORE, IRET)
         IF (IRET.EQ.0) GO TO 40
      ELSE IF (CHOICS(CHS).EQ.'USER FLAG VIS') THEN
         MENUOK = .FALSE.
         CALL AUTOIT (1, NX, NY, TVCORE, NBL, BLCORE, FVPNTR, FVCORE,
     *      IRET)
         IF (IRET.EQ.0) GO TO 40
      ELSE IF (CHOICS(CHS).EQ.'AUTO FLAG VIS') THEN
         MENUOK = .FALSE.
         CALL AUTOIT (2, NX, NY, TVCORE, NBL, BLCORE, FVPNTR, FVCORE,
     *      IRET)
         IF (IRET.EQ.0) GO TO 40
      ELSE IF ((CHOICS(CHS)(:8).EQ.'FLAG ALL') .OR.
     *   (CHOICS(CHS)(:10).EQ.'UNDO FLAG ')) THEN
         MENUOK = .FALSE.
         ISN = 1
         IF (CHOICS(CHS)(:2).EQ.'UN') ISN = -1
         ISWIN = -1
         IF (CHOICS(CHS).EQ.'FLAG ALL WIN') ISWIN = 1
         IF (CHOICS(CHS).EQ.'UNDO FLAG WIN') ISWIN = 1
         CALL FLGALL (ISN, ISWIN, NX, NY, TVCORE, IRET)
         IF (IRET.EQ.0) GO TO 40
      ELSE IF ((CHOICS(CHS).EQ.'FLAG BASELIN') .OR.
     *   (CHOICS(CHS).EQ.'UNFLAG BASEL')) THEN
         MENUOK = .FALSE.
         ISN = 1
         IF (CHOICS(CHS)(:2).EQ.'UN') ISN = -1
         CALL FLAGBL (ISN, NX, NY, TVCORE, NBL, BLCORE, IRET)
         IF (IRET.EQ.0) GO TO 40
      ELSE
         MENUOK = .FALSE.
         ISN = 1
         IF (CHOICS(CHS)(:2).EQ.'UN') ISN = -1
         IP = 7 - ISN
         ITY = 0
         IF (CHOICS(CHS)(IP:).EQ.'POINT') ITY = 1
         IF (CHOICS(CHS)(IP:).EQ.'AREA') ITY = 2
         IF (CHOICS(CHS)(IP:).EQ.'FAST') ITY = 3
         IF (ITY.GT.0) CALL WIPEIT (ITY, ISN, NX, NY, TVCORE, NBL,
     *      BLCORE, FVPNTR, FVCORE, IRET)
         END IF
      IF (IRET.EQ.0) GO TO 100
      GO TO 990
C
 980  CALL MSGWRT (8)
C
 990  CALL YHOLD ('OFFF', ITY)
      CALL TVCLOS (SBUFF, ISN)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I4,' ON ',A)
 1025 FORMAT ('Loading image smoothed by',I3,' pixels')
 1026 FORMAT ('Loading image pixel replicated by',I3,' pixels')
 1030 FORMAT (F9.3)
      END
      SUBROUTINE FLGALL (ISN, ISW, NX, NY, TVCORE, IRET)
C-----------------------------------------------------------------------
C   FLGALL flags or unflags all pixels of current type in XRANGE
C   Inputs:
C      ISN      I          1 => flag, -1 => unflag
C      ISW      I          1 -> only in window, -1 all image
C      NX       I          X dimension of TVCORE
C      NY       I          Y dimension of TVCORE
C   In/out:
C      TVCORE   R(*,*,5)   Plot as count of samples in cell - negative
C                          means flag.
C   Outputs:
C      IRET     I          Error code
C-----------------------------------------------------------------------
      INTEGER   ISN, ISW, NX, NY, IRET
      REAL      TVCORE(NX,NY,5)
C
      INCLUDE 'UFLAG.INC'
      INCLUDE 'UFLAGPLT.INC'
      INTEGER   IX, IY, PCOUNT, TVX, TVY, IP1, IG2, IM, IG, TVC(4),
     *   GRVALU, I, WIN(4), NXINT, IWIN(4)
      REAL      LRANGE(2)
      CHARACTER MSGBUF*80
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (ISN.GT.0) THEN
         LRANGE(1) = XRANGE(1)
         LRANGE(2) = XRANGE(2)
         XRANGE(1) = -20000.
         XRANGE(2) = 100000.
      ELSE IF (NFLGAL(IMGTYP).LE.0) THEN
         LRANGE(1) = -10000.
         LRANGE(2) = 100000.
      ELSE
         I = NFLGAL(IMGTYP)
         IF ((XRANGE(1).GE.GRANGE(1)) .AND. (XRANGE(2).LE.GRANGE(2)))
     *      I = I + 1
         IF (I.GT.1) THEN
            MSGBUF = 'Which FLAG ALL do you want to undo?'
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE TO TERMINAL'
               GO TO 980
               END IF
            END IF
         DO 10 I = 1,NFLGAL(IMGTYP)
            WRITE (MSGBUF,1010) I, FLGALV(1,I,IMGTYP),
     *         FLGALV(2,I,IMGTYP)
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE TO TERMINAL'
               GO TO 980
               END IF
 10         CONTINUE
         I = NFLGAL(IMGTYP)
         IF ((XRANGE(1).GE.GRANGE(1)) .AND. (XRANGE(2).LE.GRANGE(2)))
     *      THEN
            I = I + 1
            WRITE (MSGBUF,1011) I, XRANGE
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE TO TERMINAL'
               GO TO 980
               END IF
            END IF
         IF (I.GT.1) THEN
            WRITE (MSGBUF,1012) I
            CALL INQINT (TTY, MSGBUF, 1, IX, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'ASKING FOR INTEGER'
               GO TO 980
               END IF
         ELSE
            IX = 1
            END IF
         IF ((IX.LT.1) .OR. (IX.GT.I)) THEN
            LRANGE(1) = -10000.
            LRANGE(2) = 100000.
         ELSE IF (IX.LE.NFLGAL(IMGTYP)) THEN
            LRANGE(1) = FLGALV(1,IX,IMGTYP)
            LRANGE(2) = FLGALV(2,IX,IMGTYP)
            IF (ISW.LT.0) THEN
               DO 11 I = IX+1,NFLGAL(IMGTYP)
                  FLGALV(1,I-1,IMGTYP) = FLGALV(1,I,IMGTYP)
                  FLGALV(2,I-1,IMGTYP) = FLGALV(2,I,IMGTYP)
 11               CONTINUE
               NFLGAL(IMGTYP) = NFLGAL(IMGTYP) - 1
               END IF
         ELSE
            LRANGE(1) = XRANGE(1)
            LRANGE(2) = XRANGE(1)
            END IF
         END IF
      IF ((LRANGE(1).LT.PRANGE(1,IMGTYP)) .OR.
     *   (LRANGE(2).GT.PRAN GE(2,IMGTYP))) THEN
         MSGTXT = 'FLAG ALL OPERATION IMPROPERLY SELECTED'
         CALL MSGWRT (7)
         MSGTXT = 'USE VIEW ONLY COMMAND EXITING WITH BUTTON C FIRST'
         CALL MSGWRT (7)
      ELSE
         CALL COPY (4, TVCAT(IICOR), TVC)
         CALL COPY (4, TVCAT(IIWIN), WIN)
         NXINT = (TVC(3)-TVC(1)+1.0) / (WIN(3)-WIN(1)+1.0) + 0.1
         PCOUNT = 0
         IP1 = 1
         IG2 = 4 + NGRAY
         IF (ISW.GT.0) THEN
            CALL COPY (4, WIN, IWIN)
         ELSE
            IWIN(1) = 1
            IWIN(2) = 1
            IWIN(3) = NX
            IWIN(4) = NY
            END IF
         CALL YHOLD ('ONNN', IRET)
         DO 30 IY = IWIN(2),IWIN(4)
            TVY = (IY-WIN(2)) * NXINT + TVC(2)
            DO 20 IX = IWIN(1),IWIN(3)
               IF (ISN*TVCORE(IX,IY,5).GT.1.0) THEN
                  IF ((TVCORE(IX,IY,IMGTYP).NE.FBLANK) .AND.
     *               (TVCORE(IX,IY,IMGTYP).GE.LRANGE(1)) .AND.
     *               (TVCORE(IX,IY,IMGTYP).LE.LRANGE(2))) THEN
                     TVCORE(IX,IY,5) = -TVCORE(IX,IY,5)
                     FLGPIX = FLGPIX + ISN
                     PCOUNT = PCOUNT + 1
                     IF ((IX.GE.WIN(1)) .AND. (IX.LE.WIN(3)) .AND.
     *                  (IY.GE.WIN(2)) .AND. (IY.LE.WIN(4))) THEN
                        IF (TVCORE(IX,IY,5).GT.0.0) THEN
                           IM = GRVALU (TVCORE(IX,IY,IMGTYP))
                           IG = 0
                        ELSE
                           IM = 0
                           IG = 1
                           END IF
                        TVX = (IX-WIN(1)) * NXINT + TVC(1)
                        IF (NXINT.EQ.1) THEN
                           CALL YIMGIO ('WRIT', IP1, TVX, TVY, 0, 1, IM,
     *                        IRET)
                           IF (IRET.NE.0) GO TO 970
                           CALL YIMGIO ('WRIT', IG2, TVX, TVY, 0, 1, IG,
     *                        IRET)
                           IF (IRET.NE.0) GO TO 970
                        ELSE
                           CALL FILL (NXINT, IM, IBUFF)
                           CALL FILL (NXINT, IG, JBUFF)
                           DO 15 I = 1,NXINT
                              CALL YIMGIO ('WRIT', IP1, TVX, TVY, 0,
     *                           NXINT, IBUFF, IRET)
                              IF (IRET.NE.0) GO TO 970
                              CALL YIMGIO ('WRIT', IG2, TVX, TVY, 0,
     *                           NXINT, JBUFF, IRET)
                              IF (IRET.NE.0) GO TO 970
                              TVY = TVY + 1
 15                           CONTINUE
                           END IF
                        END IF
                     END IF
                  END IF
 20            CONTINUE
 30         CONTINUE
         CALL YHOLD ('ONNN', IRET)
         IF (ISN.GT.0) THEN
            WRITE (MSGTXT,1030) 'Flagged', PCOUNT
         ELSE
            WRITE (MSGTXT,1030) 'Unflagged', PCOUNT
            END IF
         CALL MSGWRT (4)
C                                       log it
         IF ((ISN.GT.0) .AND. (PCOUNT.GT.0)) THEN
            IF ((NFLGAL(IMGTYP).GE.MFLGAL) .AND. (ISW.LT.0)) THEN
               DO 50 I = 2,NFLGAL(IMGTYP)
                  FLGALV(1,I-1,IMGTYP) = FLGALV(1,I,IMGTYP)
                  FLGALV(2,I-1,IMGTYP) = FLGALV(2,I,IMGTYP)
 50               CONTINUE
               NFLGAL(IMGTYP) = NFLGAL(IMGTYP) - 1
               END IF
            NFLGAL(IMGTYP) = NFLGAL(IMGTYP) + 1
            FLGALV(1,NFLGAL(IMGTYP),IMGTYP) = LRANGE(1)
            FLGALV(2,NFLGAL(IMGTYP),IMGTYP) = LRANGE(2)
            END IF
         END IF
      GO TO 999
C
 970  WRITE (MSGTXT,1000) IRET, 'WRITE PIXEL VALUE TO TV'
 980  CALL MSGWRT (8)
      CALL YHOLD ('OFFF', IY)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLGALL ERROR',I4,' ON ',A)
 1010 FORMAT ('FLAG ALL number',I3,' values',2F9.3)
 1011 FORMAT ('Current limit  ',I3,' values',2F9.3)
 1012 FORMAT ('Enter a value between 1 and',I3)
 1030 FORMAT (A,I10,' samples')
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
C      TVCORE   R(*,*,5)   Plot as count of samples in cell - negative
C                          means flag.
C      BLCORE   I(*,*,*)   Baseline numbers in tv pixels
C   Outputs:
C      IRET     I          Error code
C-----------------------------------------------------------------------
      INTEGER   ISN, NX, NY, NBL, BLCORE(NBL,NX,NY), IRET
      REAL      TVCORE(NX,NY,5)
C
      INCLUDE 'UFLAG.INC'
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
                     IF (TVCORE(IX,IY,5).NE.0) THEN
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
                           TVCORE(IX,IY,5) = -ABS (TVCORE(IX,IY,5))
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
                     IF (TVCORE(IX,IY,5).NE.0) THEN
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
     *                     (TVCORE(IX,IY,5).LT.0)) THEN
                           NN = NN + 1
                           TVCORE(IX,IY,5) = ABS (TVCORE(IX,IY,5))
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
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, NP, NGOD
      REAL      XZY(5,NP,*)
C
      INTEGER   IRET, I, IC, ICO, LF, LC, LP
      REAL      TEMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'UFLAG.INC'
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
                     DO 20 I = 1,2
                        TEMP = XZY(I,LP,LC)
                        IF (TEMP.LT.XYMIN(I)) XYMIN(I) = TEMP
                        IF (TEMP.GT.XYMAX(I)) XYMAX(I) = TEMP
 20                     CONTINUE
                     END IF
 30               CONTINUE
 40            CONTINUE
 50         CONTINUE
C                                       Last call:
      ELSE
         IF ((XYMAX(1).LE.XYMIN(1)) .OR. (XYMAX(2).LE.XYMIN(2))) THEN
            IRET = 1
            MSGTXT = 'XYSCAL: ONE OF THE U-V AXES DEGENERATE'
            CALL MSGWRT (8)
C                                       Deal with U,Vaxes
         ELSE
            XYMAX(1) = MAX (ABS(XYMAX(1)), ABS(XYMIN(1)))
            XYMAX(2) = MAX (ABS(XYMAX(2)), ABS(XYMIN(2)))
            XYMIN(1) = -XYMAX(1)
            XYMIN(2) = 0.0
            END IF
         END IF
C
 999  RETURN
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
      REAL      RANDP(*), BUFR(*), XZY(5,NP,*)
C
      REAL      TR, TI, CATUVR(256), WT
      INTEGER   LAD, IC, ICO, IA1, IA2, LF, LC, LP, LL, L
      DOUBLE PRECISION FRQMUL, CATUVD(128), FZ, FI
      INCLUDE 'UFLAG.INC'
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
      DO 230 LF = BIF,EIF
         FZ = FOFF(LF) / UVFREQ + 1.0D0
         FI = FINC(LF) / UVFREQ
         DO 220 IC = 1,ICO,CHINC
            LC = LC + 1
            IF (TYPUVD.LE.0) FRQMUL = FZ + FI * (IC - 1 + BCHAN +
     *         (NCHAV-1.0)/2.0 - CATUVR(KRCRP+KLOCFY))
            DO 210 LP = 1,NPOL
               LAD = 1 + (IC-1)*INCF + (LF-BIF)*INCIF + (LP-1)*INCS
               WT = BUFR(LAD+2)
               TR = 0.0
               TI = 0.0
C                                       amplitudes and phases
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
 110                 CONTINUE
                  IF (WT.GT.0.0) THEN
                     TR = TR / WT
                     TI = TI / WT
                     END IF
               ELSE
                  TR = BUFR(LAD)
                  TI = BUFR(LAD+1)
                  END IF
C                                       Real , Imag , Weight parts
C                                       U, V with V > 0
               IF (WT.LE.0.0) THEN
                  CALL RFILL (5, FBLANK, XZY(1,LP,LC))
               ELSE IF (RANDP(1+ILOCV).LT.0.0) THEN
                  XZY(1,LP,LC) = -RANDP(1+ILOCU) * FRQMUL
                  XZY(2,LP,LC) = -RANDP(1+ILOCV) * FRQMUL
                  XZY(3,LP,LC) = TR
                  XZY(4,LP,LC) = -TI
                  XZY(5,LP,LC) = WT
               ELSE
                  XZY(1,LP,LC) = RANDP(1+ILOCU) * FRQMUL
                  XZY(2,LP,LC) = RANDP(1+ILOCV) * FRQMUL
                  XZY(3,LP,LC) = TR
                  XZY(4,LP,LC) = TI
                  XZY(5,LP,LC) = WT
                  END IF
 210           CONTINUE
 220        CONTINUE
 230     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE TVLOAD (NX, NY, TVCORE, PXINC, TVC, WIN, MDCORE, IRET)
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
      INTEGER   NX, NY, PXINC, TVC(4), WIN(4), IRET
      REAL      TVCORE(NX,NY,5), MDCORE(*)
C
      INTEGER   I, IYTV, NPIX, IP1, IG2, IX, PLINC, NXINT, IY, IXTV, J,
     *   MX, MY, NBL, GRVALU
      REAL      TEMP, NTEMP, SCL, VAL
      INCLUDE 'UFLAGPLT.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
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
      CALL YHOLD ('ONNN', IRET)
      IP1 = 1
      IG2 = 4 + NGRAY
      CALL YZERO (IP1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL YZERO (IG2, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IMSTAT (NX, NY, TVCORE, MDCORE, IRET)
      IF (IRET.NE.0) GO TO 999
      I = 4 + IMGTYP
      IF (PRANGE(1,I).GE.PRANGE(2,I)) THEN
         GRANGE(1) = VINFO(1)
         GRANGE(2) = VINFO(2)
      ELSE
         GRANGE(1) = PRANGE(1,I)
         GRANGE(2) = PRANGE(2,I)
         END IF
      WRITE (MSGTXT,1000) GRANGE
      CALL MSGWRT (2)
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
               TEMP = 0.0
               NTEMP = 0.0
               NBL = 0
               DO 30 J = IY,MY
                  DO 25 I = IX,MX
                     VAL = TVCORE(I,J,IMGTYP)
                     IF (VAL.NE.FBLANK) THEN
                        IF (TVCORE(I,J,5).GT.1.) THEN
                           TEMP = TEMP + TVCORE(I,J,IMGTYP)
                           NTEMP = NTEMP + 1.0
                        ELSE IF (TVCORE(I,J,5).LT.-1.) THEN
                           IF ((VAL.GT.XRANGE(1)) .AND.
     *                        (VAL.LT.XRANGE(2))) NBL = NBL + 1
                           END IF
                        END IF
 25                  CONTINUE
 30               CONTINUE
               IF (NTEMP.GT.0.0) THEN
                  TEMP = TEMP / NTEMP
                  IBUFF(IXTV) = GRVALU (TEMP)
                  JBUFF(IXTV) = 0
               ELSE IF (NBL.GT.0) THEN
                  IBUFF(IXTV) = 0
                  JBUFF(IXTV) = 1
                  END IF
C                                       "interpolate"
            ELSE IF (NXINT.GT.1) THEN
               VAL = TVCORE(IX,IY,IMGTYP)
               IF (TVCORE(IX,IY,5).GT.1.0) THEN
                  NBL = GRVALU (VAL)
                  CALL FILL (NXINT, NBL, IBUFF(IXTV))
               ELSE IF (TVCORE(IX,IY,5).LT.-1.0) THEN
                  IF ((VAL.NE.FBLANK) .AND. (VAL.GT.XRANGE(1)) .AND.
     *               (VAL.LT.XRANGE(2))) CALL FILL (NXINT, 1,
     *               JBUFF(IXTV))
                  END IF
            ELSE
               VAL = TVCORE(IX,IY,IMGTYP)
               IF (TVCORE(IX,IY,5).GT.1.0) THEN
                  NBL = GRVALU (VAL)
                  IBUFF(IXTV) = NBL
               ELSE IF (TVCORE(IX,IY,5).LT.-1.0) THEN
                  IF ((VAL.NE.FBLANK) .AND. (VAL.GT.XRANGE(1)) .AND.
     *               (VAL.LT.XRANGE(2))) JBUFF(IXTV) = 1
                  END IF
               END IF
 40         CONTINUE
C                                       write to TV
         DO 50 J = 1,NXINT
            IYTV = IYTV + 1
            CALL YIMGIO ('WRIT', IP1, TVC(1), IYTV, 0, NPIX, IBUFF,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
            CALL YIMGIO ('WRIT', IG2, TVC(1), IYTV, 0, NPIX, JBUFF,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
 50         CONTINUE
 70      CONTINUE
C                                       step function
      SCL = (VINFO(2) - VINFO(1)) / (NPIX - 1.0)
      DO 80 IX = 1,NPIX
         TEMP = VINFO(1) + SCL * (IX-1.0)
         TEMP = MAX (GRANGE(1), MIN (GRANGE(2), TEMP))
         IBUFF(IX) = GRVALU(TEMP)
 80      CONTINUE
      IYTV = IYTV + 1
      DO 90 I = 1,16
         IYTV = IYTV + 1
         IF (IYTV.LE.MAXXTV(2)) THEN
            CALL YIMGIO ('WRIT', IP1, TVC(1), IYTV, 0, NPIX, IBUFF,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
            END IF
 90      CONTINUE
      CALL YHOLD ('OFFF', IRET)
      IRET = 0
      GO TO 999
C
 980  WRITE (MSGTXT,1980) IRET
      CALL MSGWRT (8)
      CALL YHOLD ('OFFF', I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Loading TV image grey levels from',F9.3,'  to',F9.3)
 1980 FORMAT ('ERROR FROM YIMGIO:',I5)
      END
      SUBROUTINE IMSTAT (NX, NY, TVCORE, MDCORE, IRET)
C-----------------------------------------------------------------------
C   Finds statistics of current image
C   Inputs:
C      NX       I
C      NY       I
C      TVCORE   R(*,*,5)   Image arrays
C   Outputs:
C      MDCORE   R(*)       scratch buffer for median
C      IRET     I          > 0 -> no valid pixels
C   Common output:
C      VINFO    R(5)       min, max, mean, rms, median
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IRET
      REAL      TVCORE(NX,NY,5), MDCORE(*)
C
      INTEGER   IX, IY, NP
      REAL      VMIN, VMAX, TEMP, MEDIAN
      DOUBLE PRECISION VSUM, VSUMS
      INCLUDE 'UFLAGPLT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NP = 0
      VSUM = 0.0D0
      VSUMS = 0.0D0
      VMIN = 1.E10
      VMAX = -1.E10
      DO 20 IY = 1,NY
         DO 15 IX = 1,NX
            TEMP = TVCORE(IX,IY,IMGTYP)
            IF ((TEMP.NE.FBLANK) .AND. (TVCORE(IX,IY,5).GT.1.0)) THEN
               IF ((IMGTYP.EQ.3) .OR. (TEMP.GT.0.0)) THEN
                  NP = NP + 1
                  VMIN = MIN (VMIN, TEMP)
                  VMAX = MAX (VMAX, TEMP)
                  VSUM = VSUM + TEMP
                  VSUMS = VSUMS + TEMP * TEMP
                  MDCORE(NP) = TEMP
                  END IF
               END IF
 15         CONTINUE
 20      CONTINUE
      IF (NP.LE.0) THEN
         MSGTXT = 'IMSTAT FINDS NO VALID PIXELS'
         CALL MSGWRT (8)
         IRET = 0
      ELSE
         VINFO(1) = VMIN
         VINFO(2) = VMAX
         VSUM = VSUM / NP
         VINFO(3) = VSUM
         VSUMS = VSUMS / NP - VSUM * VSUM
         VINFO(4) = SQRT (MAX (0.0D0, VSUMS))
         VINFO(5) = MEDIAN (NP, MDCORE)
         END IF
C
 999  RETURN
      END
      INTEGER FUNCTION GRVALU (VALUE)
C-----------------------------------------------------------------------
C   Returns the integer value to be sent to the TV
C   Inputs:
C      VALUE    R   Image value
C   Outputs:
C      GRVALU   I   Value to send to TV
C-----------------------------------------------------------------------
      REAL      VALUE
C
      INCLUDE 'UFLAGPLT.INC'
      REAL      SCL, RMAX, RMIN, RSCALE(2), GDIF, Y, GMIN, GMAX
      LOGICAL   DOBLANK
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA RSCALE /9.0, 99.0/
C-----------------------------------------------------------------------
      IF (((XRANGE(1).GE.GRANGE(1)) .AND. (XRANGE(1).LT.GRANGE(2))) .OR.
     *   ((XRANGE(2).GT.GRANGE(1)) .AND. (XRANGE(2).LE.GRANGE(2)))) THEN
         DOBLANK = .TRUE.
         GMIN = MAX (XRANGE(1), GRANGE(1))
         GMAX = MIN (XRANGE(2), GRANGE(2))
      ELSE
         DOBLANK = .FALSE.
         GMIN = GRANGE(1)
         GMAX = GRANGE(2)
         END IF
      IF (VALUE.EQ.FBLANK) THEN
         GRVALU = 0
      ELSE IF (VALUE.GE.GMAX) THEN
         GRVALU = MAXINT
         IF (DOBLANK) GRVALU = 0
      ELSE IF (VALUE.LE.GMIN) THEN
         GRVALU = 1
         IF (DOBLANK) GRVALU = 0
      ELSE
         RMAX = MAXINT + 0.99
         RMIN = 1.01
         GDIF = GMAX - GMIN
         IF (GDIF.LE.0.0) GDIF = 1.0
C                                       linear
         IF (ITRTYP.EQ.1) THEN
            Y = (VALUE - GMIN) * (MAXINT - 0.02) / GDIF + RMIN
C                                       log
         ELSE IF (ITRTYP.EQ.2) THEN
            SCL = (MAXINT - 0.02) / LOG (1.0 + RSCALE(1))
            Y = SCL * LOG (1.0 + RSCALE(1)* (VALUE-GMIN) / GDIF) +
     *         RMIN
C                                       SQRT
         ELSE IF (ITRTYP.EQ.3) THEN
            Y = SQRT ((VALUE-GMIN) / GDIF) * (MAXINT - 0.02) + RMIN
C                                       log2
         ELSE IF (ITRTYP.EQ.4) THEN
            SCL = (MAXINT - 0.02) / LOG (1.0 + RSCALE(2))
            Y = SCL * LOG (1.0 + RSCALE(2)* (VALUE-GMIN) / GDIF) +
     *         RMIN
            END IF
         Y = MAX (RMIN, MIN (RMAX, Y))
         GRVALU = Y
         END IF
C
 999  RETURN
      END
      SUBROUTINE GRVIEW (TVC, IRET)
C-----------------------------------------------------------------------
C   Interactive selection of view only intensity range
C   Inputs:
C      TVC      I(4)   TV corners of image display
C   Outputs:
C      IRET     I      Error code
C   Common output:
C      XRANGE   R(2)   view only range
C-----------------------------------------------------------------------
      INTEGER   TVC(4), IRET
C
      INCLUDE 'UFLAG.INC'
      INCLUDE 'UFLAGPLT.INC'
      INTEGER   I, NPIX, NROW, GRC(4), NVAL, I1, I2, IXC(4), IYC(4),
     *   GR3, MAG, IY0, IX0, GR2, QUAD, IBUT, ITW(3), IVAL, IY, GR4
      REAL      SCAL, GDIF, RPOS(2), PPOS(2), X , LPOS(2)
      CHARACTER STRING*9
      LOGICAL   DOIT, T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      CALL ZTIME (ITW)
      GRC(1) = TVC(1) - 1
      GRC(3) = TVC(3) + 1
      GRC(2) = TVC(4) + 1
      GRC(4) = GRC(2) + 16
      IYC(1) = GRC(2)
      IYC(2) = GRC(4)
      IYC(3) = GRC(2)
      IYC(4) = GRC(4)
      IXC(1) = GRC(1)
      IXC(2) = GRC(1)
      IXC(3) = GRC(3)
      IXC(4) = GRC(4)
      XRANGE(1) = GRANGE(1)
      XRANGE(2) = GRANGE(2)
      GDIF = GRANGE(2) - GRANGE(1)
      NVAL = MAXINT + 1
      GR2 = 2 + NGRAY
      GR3 = 3 + NGRAY
      GR4 = 4 + NGRAY
      CALL YZERO (GR2, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL YZERO (GR3, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL YSLECT ('ONNN', GR2, 0, TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL YSLECT ('ONNN', GR3, 0, TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL YSLECT ('OFFF', GR4, 0, TVSCR, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       labeling
      CALL YWINDO ('READ', WINDTV, IRET)
      IF (IRET.NE.0) THEN
         WINDTV(1) = 1
         WINDTV(2) = 1
         WINDTV(3) = MAXXTV(1)
         WINDTV(4) = MAXXTV(2)
         IRET = 0
         END IF
      NPIX = 9 * CSIZTV(1)
      NROW = 2.5 * CSIZTV(2) + 0.5
      MAG = 1 + TVZOOM(1)
      IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
      IY0 = WINDTV(4) - MAG*NROW + 1 - (MAG-1)/2 - (CSIZTV(2)+1) / 2
      IF (MAG.GT.1) IY0 = IY0 + MAG
      IY0 = (IY0 - TVZOOM(3)) / MAG + TVZOOM(3)
      IY = MOD (IY0 - 1 - TVSCGY + 3*MAXXTV(2), MAXXTV(2)) + 1
      IY0 = MAX (1, IY)
      IF (IY0+NROW-1.GT.MAXXTV(2)) IY0 = MAXXTV(2) - NROW
      IX0 = WINDTV(1) - (MAG-1)/2 + 1.5*CSIZTV(1)
C                                       instructions
      MSGTXT = 'Hit Buttons A or B to switch between setting ' //
     *   'lower/upper limits'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button C to set limits'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button D to go back to unlimited display'
      CALL MSGWRT (1)
C                                       turn on cursor
      RPOS(1) = GRC(3)
      RPOS(2) = (GRC(2) + GRC(4)) / 2.0
      IVAL = 2
 10   PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IRET)
      IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 970
C                                       cursor read loop point
 20   CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IRET)
      LPOS(1) = RPOS(1)
      LPOS(2) = RPOS(2)
      IF (RPOS(1).LT.GRC(1)) RPOS(1) = GRC(1)
      IF (RPOS(2).LT.GRC(2)) RPOS(2) = GRC(2)
      IF (RPOS(1).GT.GRC(3)) RPOS(1) = GRC(3)
      IF (RPOS(2).GT.GRC(4)) RPOS(2) = GRC(4)
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (.NOT.DOIT) THEN
         GO TO 20
      ELSE
         CALL IMVECT ('OFFF', GR3, 2, IXC(1), IYC(1), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL IMVECT ('OFFF', GR3, 2, IXC(3), IYC(3), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         IXC(2*IVAL-1) = RPOS(1) + 0.5
         IXC(2*IVAL) = RPOS(1) + 0.5
         CALL IMVECT ('ONNN', GR3, 2, IXC(1), IYC(1), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL IMVECT ('ONNN', GR3, 2, IXC(3), IYC(3), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (STRING,1025) XRANGE(1)
         IY = IY0 + 1.5*CSIZTV(2) + 0.5
         CALL IMCHAR (GR2, IX0, IY, 0, 0, STRING(:9), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         WRITE (STRING,1025) XRANGE(2)
         CALL IMCHAR (GR2, IX0, IY0, 0, 0, STRING(:9), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         IF (IBUT.LT.4) THEN
            X = (RPOS(1)-GRC(1)) / (GRC(3)-GRC(1)) * GDIF + GRANGE(1)
            XRANGE(IVAL) = X
            I1 = 1 + (XRANGE(1) - GRANGE(1))/GDIF * MAXINT + 0.5
            I2 = 1 + (XRANGE(2) - GRANGE(1))/GDIF * MAXINT + 0.5
         ELSE
            IF (IBUT.GT.7) THEN
               XRANGE(1) = -10000.
               XRANGE(2) = 100000.
               END IF
            I1 = 1
            I2 = NVAL
            END IF
         CALL FILL (NVAL, 0, TVSCR)
         SCAL = (LUTOUT+1.0) / FLOAT (I2 - I1)
         DO 30 I = I1,I2
            TVSCR(I) = SCAL * (I-I1)
 30         CONTINUE
         I = 1
         CALL YLUT ('WRIT', I, 7, .FALSE., TVSCR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING LUT'
            GO TO 980
            END IF
         IF (IBUT.LT.4) THEN
            IF (IBUT.GT.0) THEN
               IVAL = 3 - IVAL
               RPOS(1) = IXC(2*IVAL)
               END IF
            IF ((RPOS(1).NE.LPOS(1)) .OR. (RPOS(2).NE.LPOS(2))) GO TO 10
            GO TO 20
            END IF
         END IF
C                                       done
      GO TO 990
C
 970  WRITE (MSGTXT,1000) IRET, 'BASIC TV FUNCTION'
C
 980  CALL MSGWRT (8)
C
 990  CALL YZERO (GR2, I)
      CALL YZERO (GR3, I)
      IF (FLAGON) THEN
         CALL YSLECT ('ONNN', GR4, 0, TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GRVIEW ERROR',I4,' ON ',A)
 1025 FORMAT (F9.3)
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
      SUBROUTINE WIPEIT (ITY, ISN, NX, NY, TVCORE, NBL, BLCORE, FVPNTR,
     *   FVCORE, IRET)
C-----------------------------------------------------------------------
C   Does the interactive editing functions
C   Inputs:
C      ITY      I      Type: 1-3 => point, area, fast  5 -> examine
C      ISN      I      1 flag, -1 unflag
C      NX       I      X dimension of TVCORE
C      NY       I      Y dimension of TVCORE
C      NBL      I      Max number baselines in BLCORE
C      BLCORE   I(*)   Basseline IDs
C      FVCORE   I(*)   Visibility number (only ITY = 5)
C   In/out:
C      TVCORE   R(*)   Image of plot
C   Outputs:
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   ITY, ISN, NX, NY, NBL, BLCORE(NBL,NX,NY),
     *   FVPNTR(2,NX,NY), FVCORE(2,*), IRET
      REAL      TVCORE(NX,NY,5)
C
      CHARACTER STRING*24, TYPES(4)*1, ATY*1
      INTEGER   GV, GC, IP1, IG2, ITW(3), NPIX, NROW, MAG, IX0, IY0, IX,
     *   IY, ECOUNT, QUAD, IBUT, IX1, IY1, IMX, IMY, FMT, TVX, TVY,
     *   IXP(5), IYP(5), TVC(4), DOBLC, J, NS, J2, MTYP, SP, LMX, LMY,
     *   GRVALU, IV, WIN(4), NXINT, IMX1, IMX2, IMY1, IMY2, I, LCOUNT,
     *   IA1, IA2
      REAL      PPOS(2), RPOS(2), XFACT, YFACT, LPOS(2), WR
      LOGICAL   DOIT, T, F, STARTD
      DOUBLE PRECISION CRD
      INCLUDE 'UFLAG.INC'
      INCLUDE 'UFLAGPLT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA TYPES /'S', 'A', 'P', 'D'/
C-----------------------------------------------------------------------
      STARTD = .FALSE.
      MTYP = XDOCEN + 1.5
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
      IP1 = 1
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
      CALL YWINDO ('READ', WINDTV, IRET)
      IF (IRET.NE.0) THEN
         WINDTV(1) = 1
         WINDTV(2) = 1
         WINDTV(3) = MAXXTV(1)
         WINDTV(4) = MAXXTV(2)
         IRET = 0
         END IF
      CALL COPY (4, TVCAT(IICOR), TVC)
      CALL COPY (4, TVCAT(IIWIN), WIN)
      IF (MANT.GT.99) THEN
         NPIX = 20 * CSIZTV(1)
      ELSE
         NPIX = 16 * CSIZTV(1)
         END IF
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
      ELSE IF (ITY.EQ.3) THEN
         MSGTXT = 'Position cursor, then hit button, A, B, or C'
     *      // ' to start operation'
         CALL MSGWRT (1)
         MSGTXT = 'Then simply position cursor on point to flag'
         CALL MSGWRT (1)
         MSGTXT = 'Hit buttons A, B, or C to flag and then return'
     *      // ' to menu'
      ELSE IF (ITY.EQ.5) THEN
         MSGTXT = 'Position cursor, then hit button A, B, or C'
     *      // ' to select sample'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to menu'
         END IF
      CALL MSGWRT (1)
C                                       instructions: Button D
      MSGTXT = 'Hit button D to exit - no further flagging'
      IF (ITY.NE.5) CALL MSGWRT (1)
C                                       turn on cursor
      ECOUNT = 0
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
      CALL YCURSE ('FXIT', F, T, RPOS, QUAD, IBUT, IRET)
      XFACT = REAL (TVC(3) - TVC(1)) / REAL (WIN(3)-WIN(1))
      YFACT = REAL (TVC(4) - TVC(2)) / REAL (WIN(4)-WIN(2))
      NXINT = XFACT + 0.1
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
         IMX = (RPOS(1) - TVC(1)) / XFACT + 0.49 + WIN(1)
         IMY = (RPOS(2) - TVC(2)) / YFACT + 0.49 + WIN(2)
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
C                                       flux
         LMX = MAX (1, MIN (NX, IMX))
         LMY = MAX (1, MIN (NY, IMY))
         CRD = TVCORE (LMX,LMY,IMGTYP)
         ATY = TYPES(IMGTYP)
         IF (CRD.EQ.FBLANK) THEN
            STRING = 'BLANKED'
         ELSE IF (FMT.EQ.11) THEN
            WRITE (STRING,1130) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1131) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1132) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1133) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1134) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1135) ATY, CRD
         ELSE IF (FMT.EQ.10) THEN
            WRITE (STRING,1140) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1141) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1142) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1143) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1144) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1145) ATY, CRD
         ELSE
            WRITE (STRING,1150) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1151) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1152) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1153) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1154) ATY, CRD
            END IF
         CALL REFRMT (STRING, '_', IY)
         IY = IY0 + 1.5*CSIZTV(2) + 0.5
         CALL IMCHAR (GC, IX0, IY, 0, 0, STRING(:11), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         CRD = TVCATD(KDCRV) + (LMX - TVCATR(KRCRP)) * TVCATR(KRCIC)
         CRD = CRD * SCALXY(1)
         ATY = 'U'
         IF (FMT.EQ.11) THEN
            WRITE (STRING,1130) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1131) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1132) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1133) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1134) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1135) ATY, CRD
         ELSE IF (FMT.EQ.10) THEN
            WRITE (STRING,1140) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1141) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1142) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1143) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1144) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1145) ATY, CRD
         ELSE
            WRITE (STRING,1150) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1151) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1152) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1153) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1154) ATY, CRD
            END IF
         IY = IY0 + 4.5 * CSIZTV(2)
         CALL IMCHAR (GC, IX0, IY, 0, 0, STRING(:11), TVSCR, IRET)
         IF (IRET.NE.0) GO TO 970
         CRD = TVCATD(KDCRV+1) + (LMY-TVCATR(KRCRP+1)) * TVCATR(KRCIC+1)
         CRD = CRD * SCALXY(2)
         ATY = 'V'
         IF (FMT.EQ.11) THEN
            WRITE (STRING,1130) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1131) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1132) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1133) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1134) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1135) ATY, CRD
         ELSE IF (FMT.EQ.10) THEN
            WRITE (STRING,1140) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1141) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1142) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1143) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1144) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1145) ATY, CRD
         ELSE
            WRITE (STRING,1150) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1151) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1152) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1153) ATY, CRD
            IF (STRING(FMT:FMT).EQ.'*') WRITE (STRING,1154) ATY, CRD
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
                  IA2 = BLCORE(J,LMX,LMY)
                  IA1 = IA2 / 10000
                  IA2 = IA2 - 10000*IA1
                  IF (MANT.LE.99) THEN
                     WRITE (STRING(SP:SP+4),1160) IA1, IA2
                     SP = SP + 6
                  ELSE
                     WRITE (STRING(SP:SP+6),1161) IA1, IA2
                     SP = SP + 8
                     END IF
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
                  IA2 = -BLCORE(J,LMX,LMY)
                  IA1 = IA2 / 10000
                  IA2 = IA2 - 10000*IA1
                  IF (MANT.LE.99) THEN
                     WRITE (STRING(SP:SP+4),1160) IA1, IA2
                     SP = SP + 6
                  ELSE
                     WRITE (STRING(SP:SP+6),1161) IA1, IA2
                     SP = SP + 8
                     END IF
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
            IF (ITY.EQ.3) IBUT = 0
            END IF
         LCOUNT = 0
         IF (((ITY.EQ.1) .AND. (IBUT.GT.0)) .OR. ((ITY.EQ.3) .AND.
     *      (STARTD))) THEN
            IF (ISN*TVCORE(IMX,IMY,5).GT.1.0) THEN
               TVCORE(IMX,IMY,5) = -TVCORE(IMX,IMY,5)
               IF (TVCORE(IMX,IMY,5).GT.0.0) THEN
                  IX = GRVALU (TVCORE(IMX,IMY,IMGTYP))
                  IY = 0
               ELSE
                  IX = 0
                  IY = 1
                  END IF
               FLGPIX = FLGPIX + ISN
               LCOUNT = LCOUNT + 1
               IF (NXINT.EQ.1) THEN
                  CALL YIMGIO ('WRIT', IP1, TVX, TVY, 0, 1, IX, IRET)
                  IF (IRET.NE.0) GO TO 970
                  CALL YIMGIO ('WRIT', IG2, TVX, TVY, 0, 1, IY, IRET)
                  IF (IRET.NE.0) GO TO 970
               ELSE
                  TVX = (IMX - WIN(1)) * NXINT + TVC(1)
                  TVY = (IMY - WIN(2)) * NXINT + TVC(2)
                  CALL FILL (NXINT, IX, IBUFF)
                  CALL FILL (NXINT, IY, JBUFF)
                  DO 130 I = 1,NXINT
                     CALL YIMGIO ('WRIT', IP1, TVX, TVY, 0, NXINT,
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
               WRITE (MSGTXT,1100) 'Flagged', LCOUNT
            ELSE
               WRITE (MSGTXT,1100) 'Unflagged', LCOUNT
               END IF
            CALL MSGWRT (3)
            IF (IBUT+ITY.GE.4) GO TO 970
C                                       examine
         ELSE IF ((ITY.EQ.5) .AND. (IBUT.GT.0)) THEN
            IF (IBUT.GT.7) GO TO 970
            CALL EXAVIS (NX, NY, TVCORE, NBL, BLCORE, FVPNTR, FVCORE,
     *         LMX, LMY, IRET)
            IF (IRET.NE.0) GO TO 970
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
                  IV = 0
                  IY = 1
                  END IF
               LCOUNT = 0
               CALL YHOLD ('ONNN', IRET)
               DO 150 IMY = IMY1,IMY2
                  DO 140 IMX = IMX1,IMX2
                     IF (ISN*TVCORE(IMX,IMY,5).GT.1.0) THEN
                        TVCORE(IMX,IMY,5) = -TVCORE(IMX,IMY,5)
                        FLGPIX = FLGPIX + ISN
                        IF (IX.EQ.1) IV =
     *                     GRVALU (TVCORE(IMX,IMY,IMGTYP))
                        IF (NXINT.EQ.1) THEN
                           IBUFF(1) = IV
                           JBUFF(1) = IY
                        ELSE
                           CALL FILL (NXINT, IV, IBUFF)
                           CALL FILL (NXINT, IY, JBUFF)
                           END IF
                        TVX = (IMX - WIN(1)) * NXINT + TVC(1)
                        TVY = (IMY - WIN(2)) * NXINT + TVC(2)
                        LCOUNT = LCOUNT + 1
                        DO 135 I = 1,NXINT
                           CALL YIMGIO ('WRIT', IP1, TVX, TVY, 0, NXINT,
     *                        IBUFF, IRET)
                           IF (IRET.NE.0) GO TO 970
                           CALL YIMGIO ('WRIT', IG2, TVX, TVY, 0, NXINT,
     *                        JBUFF, IRET)
                           IF (IRET.NE.0) GO TO 970
                           TVY = TVY + 1
 135                       CONTINUE
                        END IF
 140                 CONTINUE
 150              CONTINUE
               DOBLC = 0
               CALL IMVECT ('OFFF', GV, 5, IXP, IYP, TVSCR, IRET)
               IF (IRET.NE.0) GO TO 970
               IF (IBUT.GT.3) GO TO 970
               CALL FILL (5, 1, IXP)
               CALL FILL (5, 1, IYP)
               PPOS(1) = 0.
               CALL YHOLD ('OFFF', IRET)
               IF (ISN.GT.0) THEN
                  WRITE (MSGTXT,1100) 'Flagged', LCOUNT
               ELSE
                  WRITE (MSGTXT,1100) 'Unflagged', LCOUNT
                  END IF
               CALL MSGWRT (3)
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
 1100 FORMAT (A,I5,' samples')
 1120 FORMAT (I5,',',I5)
 1121 FORMAT (I4,',',I5)
 1122 FORMAT (I3,',',I4)
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
 1160 FORMAT (I2.2,'-',I2.2)
 1161 FORMAT (I3.2,'-',I3.2)
 1970 FORMAT ('WIPEIT: TV ACTION IO ERROR',I7)
      END
      SUBROUTINE AUTOIT (ITY, NX, NY, TVCORE, NBL, BLCORE, FVPNTR,
     *   FVCORE, IRET)
C-----------------------------------------------------------------------
C   Automatic flagging of vis in window
C   Inputs:
C      ITY      I      1 -> ask user, 2 -> use statistics
C      NX       I      X dimension of TVCORE
C      NY       I      Y dimension of TVCORE
C      NBL      I      Max number baselines in BLCORE
C      BLCORE   I(*)   Basseline IDs
C      FVCORE   I(*)   Visibility number (only ITY = 5)
C   In/out:
C      TVCORE   R(*)   Image of plot
C   Outputs:
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   ITY, NX, NY, NBL, BLCORE(NBL,NX,NY), FVPNTR(2,NX,NY),
     *   FVCORE(2,*), IRET
      REAL      TVCORE(NX,NY,5)
C
      INCLUDE 'UFLAG.INC'
      INCLUDE 'UFLAGPLT.INC'
      INTEGER   ITYPE, WIN(4), IX, IY, NFIX, NFLAGD, NRECS, NEXA, NS,
     *   NF, NMSG, IU, ITIME(3), TTIME, PTIME
      CHARACTER MSGBUF*72, UNITS(2)*3, TYPES(2)*10
      REAL      ERANGE(2), FRANGE(2), PHRANG(2), TEMP
      DOUBLE PRECISION XX(2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA UNITS /'Jy', 'deg'/
      DATA TYPES /'amplitudes','phases'/
C-----------------------------------------------------------------------
      IU = 1
      IF (IMGTYP.EQ.3) IU = 2
C                                       what are we to do?
      IF (ITY.NE.2) THEN
         MSGBUF = 'Examine pixels in range (2 values)'
         CALL INQFLT (TTY, MSGBUF, 2, XX, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING 2 FLOATING VALUES'
            GO TO 980
            END IF
         ERANGE(1) = XX(1)
         ERANGE(2) = XX(2)
         IF (ERANGE(1).GE.ERANGE(2)) THEN
            MSGTXT = 'INVALID CONSIDERATION RANGE: EXITING'
            GO TO 980
            END IF
         MSGBUF = 'Flag amplitudes below and above (2 values)'
         CALL INQFLT (TTY, MSGBUF, 2, XX, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING 2 FLOATING VALUES'
            GO TO 980
            END IF
         FRANGE(1) = XX(1)
         FRANGE(2) = XX(2)
         IF (FRANGE(1).GE.FRANGE(2)) THEN
            MSGTXT = 'INVALID FLAG RANGE: EXITING'
            GO TO 980
            END IF
         MSGBUF = 'Flag phases below and above (2 values)'
         CALL INQFLT (TTY, MSGBUF, 2, XX, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING 2 FLOATING VALUES'
            GO TO 980
            END IF
         PHRANG(1) = XX(1)
         PHRANG(2) = XX(2)
         IF (PHRANG(1).GE.PHRANG(2)) THEN
            PHRANG(1) = -200.
            PHRANG(2) = 200.
            END IF
         MSGBUF = 'Examine flagged (-1), unflagged (1), or both (0)'
         CALL INQINT (TTY, MSGBUF, 1, ITYPE, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING WHICH TO EXAMINE'
            GO TO 980
            END IF
         IF (ITYPE.LT.0) ITYPE = MAX (-1, ITYPE)
         IF (ITYPE.GT.0) ITYPE = MIN (1, ITYPE)
C                                       we set it but tell user
      ELSE IF (IMGTYP.LE.2) THEN
         ITYPE = 1
         FRANGE(2) = VINFO(5) + APARM(2) * VINFO(4)
         ERANGE(1) = VINFO(3) + APARM(1) * VINFO(4)
         WRITE (MSGTXT,1010) ERANGE(1), UNITS(IU)
         CALL MSGWRT (3)
         IF (.NOT.ISCAL) THEN
            ERANGE(2) = 1.E10
            FRANGE(1) = -1.0
            PHRANG(1) = -200.0
            PHRANG(2) = 200.0
            WRITE (MSGTXT,1012) TYPES(IU), FRANGE(2), UNITS(IU)
            CALL MSGWRT (3)
         ELSE
            ERANGE(2) = VINFO(5) - APARM(1) * VINFO(4)
            WRITE (MSGTXT,1011) ERANGE(2)
            IF (ERANGE(2).GT.0.0) CALL MSGWRT (3)
            FRANGE(1) = VINFO(5) - APARM(2) * VINFO(4)
            WRITE (MSGTXT,1013) TYPES(IU), FRANGE(1), UNITS(IU)
            IF (FRANGE(1).GE.0.0) CALL MSGWRT (3)
            WRITE (MSGTXT,1012) TYPES(IU), FRANGE(2), UNITS(IU)
            CALL MSGWRT (3)
            PHRANG(1) = -APARM(3) * RAD2DG * VINFO(4) / VINFO(5)
            PHRANG(2) = -PHRANG(1)
            WRITE (MSGTXT,1014) PHRANG(2)
            IF (PHRANG(2).LE.180.0) CALL MSGWRT (3)
            END IF
      ELSE
         MSGTXT = 'AUTO FLAG VIS IN THIS MODE NOT A GOOD IDEA'
         CALL MSGWRT (7)
         IRET = 0
         GO TO 999
         END IF
C                                       start loop
      NFIX = 0
      NFLAGD = 0
      NRECS = 0
      NEXA = 0
      CALL COPY (4, TVCAT(IIWIN), WIN)
      NMSG = 20000 / (WIN(3) - WIN(1) + 1)
      NMSG = MIN (NMSG, (WIN(4)-WIN(2))/4)
      PTIME = 0
      DO 100 IY = WIN(2),WIN(4)
         DO 90 IX = WIN(1),WIN(3)
            IF (((ITYPE.EQ.0) .AND. (ABS(TVCORE(IX,IY,5)).GT.1)) .OR.
     *         (ITYPE*TVCORE(IX,IY,5).GT.1)) THEN
               TEMP = TVCORE(IX,IY,IMGTYP)
               IF (TEMP.NE.FBLANK) THEN
                  CALL ZTIME (ITIME)
                  TTIME = 3600 * ITIME(1) + 60 * ITIME(2) + ITIME(3)
                  IF (ABS(TTIME-PTIME).GT.120) THEN
                     WRITE (MSGTXT,1050) IY, IX
                     CALL MSGWRT (1)
                     PTIME = TTIME
                     END IF
                  IF ((ERANGE(1).LE.ERANGE(2)) .AND. (TEMP.GE.ERANGE(1))
     *               .AND. (TEMP.LE.ERANGE(2))) THEN
                     NEXA = NEXA + 1
                     CALL VISFIX (NX, NY, TVCORE, NBL, BLCORE, FVPNTR,
     *                  FVCORE, IX, IY, FRANGE, PHRANG, NS, NF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'FROM VISFIX'
                        GO TO 980
                        END IF
                     IF (NS.GT.0) THEN
                        NFIX = NFIX + 1
                     ELSE IF (NF.GT.0) THEN
                        NFLAGD = NFLAGD + 1
                        END IF
                     NRECS = NRECS + NF
                  ELSE IF ((ERANGE(1).GT.ERANGE(2)) .AND.
     *               ((TEMP.LT.ERANGE(2)) .OR. (TEMP.GT.ERANGE(1))))
     *               THEN
                     NEXA = NEXA + 1
                     CALL VISFIX (NX, NY, TVCORE, NBL, BLCORE, FVPNTR,
     *                  FVCORE, IX, IY, FRANGE, PHRANG, NS, NF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'FROM VISFIX'
                        GO TO 980
                        END IF
                     IF (NS.GT.0) THEN
                        NFIX = NFIX + 1
                     ELSE IF (NF.GT.0) THEN
                        NFLAGD = NFLAGD + 1
                        END IF
                     NRECS = NRECS + NF
                     END IF
                  END IF
               END IF
 90         CONTINUE
 100     CONTINUE
      WRITE (MSGTXT,1100) NEXA, NFIX, NFLAGD
      CALL MSGWRT (4)
      WRITE (MSGTXT,1101) NRECS
      CALL MSGWRT (4)
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AUTOIT: ERROR',I5,' ON ',A)
 1010 FORMAT ('Examining unflagged pixels above    ',F9.3,1X,A)
 1011 FORMAT ('      and unflagged pixels below    ',F9.3,1X,A)
 1012 FORMAT ('Flagging visibility ',A,' above',F9.3,1X,A)
 1013 FORMAT ('Flagging visibility ',A,' below',F9.3,1X,A)
 1014 FORMAT ('Flagging absolute visibility phases >',F7.1,' degrees')
 1050 FORMAT ('Starting on row',I6,' column',I6)
 1100 FORMAT ('Examined',I7,', corrected',I7,', flagged',I7,' pixels')
 1101 FORMAT (4X,I10,' total flags written to FC table')
      END
      SUBROUTINE VISFIX (NX, NY, TVCORE, NBL, BLCORE, FVPNTR, FVCORE,
     *   IMX, IMY, FRANGE, PHRANG, NSUM, NF, IRET)
C-----------------------------------------------------------------------
C   Examines vis affecting a pixel and flags outside FRANGE
C   Inputs:
C      NX       I      X dimension of TVCORE
C      NY       I      Y dimension of TVCORE
C      NBL      I      Max number baselines in BLCORE
C      IMX      I      X pixel of interest
C      IMY      I      Y pixel of interest
C      FRANGE   R(2)   Flag amp if not between FRANGE(1) and FRANGE(2)
C      PHRANG   R(2)   Flag phase if not between PHRANG(1) and PHRANG(2)
C   In/out:
C      TVCORE   R(*)   Image of plot
C      BLCORE   I(*)   Baseline IDs
C      FVCORE   I(*)   Visibility number (only ITY = 5)
C   Output:
C      NSUM     I      > 0 => corrected pixel
C      NF       I      Number flags written to FC table
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NBL, BLCORE(NBL,NX,NY), FVPNTR(2,NX,NY),
     *   FVCORE(2,*), IMX, IMY, NSUM, NF, IRET
      REAL      TVCORE(NX,NY,5), FRANGE(2), PHRANG(2)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UFLAG.INC'
      INCLUDE 'UFLAGPLT.INC'
      INCLUDE 'UFLAGVIS.INC'
C
      INTEGER   IVIS, VCOUNT, IA1, IA2, IC, ICO, IC2, IX, IY, J, LC, LF,
     *   LP, NV, I, TVC(4), WIN(4), NXINT, TVX, TVY, IP1, IG2, GRVALU,
     *   NVALS, INEXT, MYVIS, MSG
      REAL      XZY(5,MAXCIF), TIME, AMP, XY(2), SUMR, SUMI, SUMA, PHASE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IP1 = 1
      IG2 = NGRAY + 4
      IF (ABS(TVCORE(IMX,IMY,5)).LE.1.0) THEN
         MSGTXT = 'No data at selected pixel'
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       get the vis
      VCOUNT = 0
      NV = 0
      NVALS = FVPNTR(1,IMX,IMY)
      INEXT = FVPNTR(2,IMX,IMY)
      MSG = 0
      DO 50 IVIS = 1,NVALS
         MYVIS = 0
         IF (INEXT.GT.0) MYVIS = FVCORE(1,INEXT)
         IF (MYVIS.GT.0) THEN
            INITVS = MYVIS
            CALL UVGET ('INIT', RPARM, BUFF1, IRET)
            IF (IRET.EQ.0) CALL UVGET ('READ', RPARM, BUFF1, IRET)
            IF (IRET.LT.0) THEN
               WRITE (MSGTXT,1005) MYVIS
               CALL MSGWRT (7)
            ELSE IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, MYVIS, 'READING VIS'
               CALL MSGWRT (8)
               GO TO 900
            ELSE
               CALL UVGET ('CLOS', RPARM, BUFF1, J)
               IF (ILOCB.GE.0) THEN
                  IA1 = INT (RPARM(ILOCB+1)) / 256
                  IA2 = MOD (INT (RPARM(ILOCB+1)), 256)
               ELSE
                  IA1 = RPARM(ILOCA1+1) + 0.1
                  IA2 = RPARM(ILOCA2+1) + 0.1
                  END IF
               TIME = RPARM(ILOCT+1)
               CALL FNDXY (RPARM, BUFF1, NPOL, XZY)
               ICO = ECHAN - BCHAN + 1
               LC = 0
               DO 40 LF = BIF,EIF
                  DO 35 IC = 1,ICO,CHINC
                     DO 30 LP = 1,NPOL
                        LC = LC + 1
                        IF ((XZY(1,LC).NE.FBLANK) .AND.
     *                     (XZY(2,LC).NE.FBLANK) .AND.
     *                     (XZY(5,LC).GT.0.0)) THEN
                           DO 20 J = 1,2
                              XY(J) = XYSCL(J) *
     *                           (XZY(J,LC)-XYOFF(J))
 20                           CONTINUE
C                                       this pixel ?
                           IX = XY(1) + 1.0
                           IY = XY(2) + 1.0
                           IF ((IX.EQ.IMX) .AND. (IY.EQ.IMY)) THEN
                              NV = NV + 1
                              IC2 = IC + CHINC - 1
                              VCOUNT = VCOUNT + 1
                              IF (NV.LE.MAXVIS) THEN
                                 VLANT(1,NV) = IA1
                                 VLANT(2,NV) = IA2
                                 VLIF(NV) = LF
                                 VLCHAN(1,NV) = IC
                                 VLCHAN(2,NV) = IC2
                                 VLTIME(NV) = TIME
                                 VLREAL(NV) = XZY(3,LC)
                                 VLIMAG(NV) = XZY(4,LC)
                                 VLBASL(NV) = 10000 * IA1 + IA2
                                 VLFVC(NV) = INEXT
                                 END IF
                              END IF
                           END IF
 30                     CONTINUE
 35                  CONTINUE
 40               CONTINUE
               END IF
            END IF
         IF (INEXT.GT.0) INEXT = FVCORE(2,INEXT)
 50      CONTINUE
      IF (NV.GT.MAXVIS) THEN
         WRITE (MSGTXT,1050) NV, MAXVIS
         CALL MSGWRT (7)
         WRITE (MSGTXT,1051) IMX, IMY
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       flag some ??
      NF = 0
      DO 100 VCOUNT = 1,NV
         IF ((VLREAL(VCOUNT).NE.FBLANK) .AND.(VLIMAG(VCOUNT).NE.FBLANK))
     *      THEN
            AMP = SQRT (VLREAL(VCOUNT)**2 + VLIMAG(VCOUNT)**2)
            PHASE = 0.0
            IF (AMP.NE.0) THEN
               PHASE = RAD2DG * ATAN2 (VLIMAG(VCOUNT), VLREAL(VCOUNT))
               IF (PHASE.GT.180.0) PHASE = PHASE - 360.0
               IF (PHASE.LT.-180.0) PHASE = PHASE + 360.0
               END IF
            IF ((AMP.LT.FRANGE(1)) .OR. (AMP.GT.FRANGE(2)) .OR.
     *         (PHASE.LT.PHRANG(1)) .OR. (PHASE.GT.PHRANG(2))) THEN
               NF = NF + 1
               CALL VLFLAG (VCOUNT, VLTIME, VLCHAN, VLIF, VLANT, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1970) IRET, 'WRITING TO FC TANLE'
                  GO TO 980
                  END IF
               VLREAL(VCOUNT) = FBLANK
               VLIMAG(VCOUNT) = FBLANK
               END IF
            END IF
 100     CONTINUE
      IF ((NV.GT.0) .AND. (NF.GT.0)) THEN
         NFIXED = NFIXED + 1
         NSUM = 0
         SUMR = 0.0
         SUMI = 0.0
         SUMA = 0.0
         CALL FILL (NBL, 0, BLCORE(1,IMX,IMY))
         DO 120 I = 1,NV
           IF (VLREAL(I).NE.FBLANK) THEN
              SUMR = SUMR + VLREAL(I)
              SUMI = SUMI + VLIMAG(I)
              SUMA = SUMA + SQRT (VLREAL(I)**2 + VLIMAG(I)**2)
              NSUM = NSUM + 1
              CALL COUNTB (NX, NY, IMX, IMY, VLBASL(I), NBL, BLCORE)
           ELSE
              FVCORE(1,VLFVC(I)) = 0
              END IF
 120       CONTINUE
C                                       fix up sample
         MSG = MSG + 1
         IF (NSUM.GT.0) THEN
            WRITE (MSGTXT,1120) 'Restoring corrected', IMX, IMY
            IF ((PRTLEV.GT.0.0) .OR. (MOD(MSG,200).EQ.0))
     *         CALL MSGWRT (3)
            TVCORE(IMX,IMY,1) = SUMA / NSUM
            TVCORE(IMX,IMY,2) = SQRT (SUMR*SUMR + SUMI*SUMI) / NSUM
            IF (TVCORE(IMX,IMY,2).LE.0.0) THEN
               PHASE = 0.0
            ELSE
               PHASE = RAD2DG * ATAN2 (SUMI, SUMR)
               IF (PHASE.GT.180.0) PHASE = PHASE - 360.0
               IF (PHASE.LT.-180.0) PHASE = PHASE + 360.0
               END IF
            TVCORE(IMX,IMY,3) = PHASE
            TVCORE(IMX,IMY,4) = TVCORE(IMX,IMY,1) - TVCORE(IMX,IMY,2)
            TVCORE(IMX,IMY,5) = ABS (TVCORE(IMX,IMY,5)) - NF
         ELSE
            WRITE (MSGTXT,1120) 'Flagging', IMX, IMY
            IF ((PRTLEV.GT.0.0) .OR. (MOD(MSG,200).EQ.0))
     *         CALL MSGWRT (3)
            TVCORE(IMX,IMY,1) = FBLANK
            TVCORE(IMX,IMY,2) = FBLANK
            TVCORE(IMX,IMY,3) = FBLANK
            TVCORE(IMX,IMY,4) = FBLANK
            TVCORE(IMX,IMY,5) = 0.0
            FVPNTR(1,IMX,IMY) = 0
            END IF
         WRITE (MSGTXT,1121) NF
         IF ((NF.GT.0) .AND. (PRTLEV.GT.0.0)) CALL MSGWRT(3)
C                                       update the TV
         IF (TVCORE(IMX,IMY,5).GT.0.0) THEN
            IX = GRVALU (TVCORE(IMX,IMY,IMGTYP))
            IY = 0
         ELSE
            IX = 0
            IY = 1
            END IF
         CALL COPY (4, TVCAT(IICOR), TVC)
         CALL COPY (4, TVCAT(IIWIN), WIN)
         NXINT = (TVC(3) - TVC(1) + 1.0) / (WIN(3) - WIN(1) + 1.0) + 0.1
         TVX = (IMX - WIN(1)) * NXINT + TVC(1)
         TVY = (IMY - WIN(2)) * NXINT + TVC(2)
         IF (NXINT.EQ.1) THEN
            CALL YIMGIO ('WRIT', IP1, TVX, TVY, 0, 1, IX, IRET)
            IF (IRET.NE.0) GO TO 970
            CALL YIMGIO ('WRIT', IG2, TVX, TVY, 0, 1, IY, IRET)
            IF (IRET.NE.0) GO TO 970
         ELSE
            CALL FILL (NXINT, IX, IBUFF)
            CALL FILL (NXINT, IY, JBUFF)
            DO 130 I = 1,NXINT
               CALL YIMGIO ('WRIT', IP1, TVX, TVY, 0, NXINT,
     *            IBUFF, IRET)
               IF (IRET.NE.0) GO TO 970
               CALL YIMGIO ('WRIT', IG2, TVX, TVY, 0, NXINT, JBUFF,
     *            IRET)
               IF (IRET.NE.0) GO TO 970
               TVY = TVY + 1
 130           CONTINUE
            END IF
         END IF
      IRET = 0
C
 900  IF (IRET.NE.0) CALL UVGET ('CLOS', RPARM, BUFF1, J)
      GO TO 999
C
 970  WRITE (MSGTXT,1970) IRET, 'WRITING TO THE TV'
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VISFIX ERROR',I4,' VIS',I10,' ON ',A)
 1005 FORMAT ('VISFIX FINDS NO DATA FOR VIS NUMBER',I10)
 1050 FORMAT ('VISFIX NUMBER SAMPLES',I6,' > LIMIT',I6)
 1051 FORMAT ('       CANNOT FIX PIXEL',2I5)
 1120 FORMAT (A,' sample at pixel',2I5,1X,A)
 1121 FORMAT ('Writing',I5,' flag commands to FC table')
 1970 FORMAT ('VISFIX ERROR',I4,' ON ',A)
      END
      SUBROUTINE EXAVIS (NX, NY, TVCORE, NBL, BLCORE, FVPNTR, FVCORE,
     *   IMX, IMY, IRET)
C-----------------------------------------------------------------------
C   Displays the vis associated with selected pixels
C   Inputs:
C      NX       I      X dimension of TVCORE
C      NY       I      Y dimension of TVCORE
C      NBL      I      Max number baselines in BLCORE
C      IMX      I      X pixel of interest
C      IMY      I      Y pixel of interest
C   In/out:
C      TVCORE   R(*)   Image of plot
C      BLCORE   I(*)   Basseline IDs
C      FVCORE   I(*)   Visibility number (only ITY = 5)
C   Output:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NBL, BLCORE(NBL,NX,NY), FVPNTR(2,NX,NY),
     *   FVCORE(2,*), IMX, IMY, IRET
      REAL      TVCORE(NX,NY,5)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UFLAG.INC'
      INCLUDE 'UFLAGPLT.INC'
      INCLUDE 'UFLAGVIS.INC'
C
      INTEGER   IVIS, ITT(3), VCOUNT, IA1, IA2, IC, ICO, IC2, IX, IY, J,
     *   LC, LF, LP, NV, NSUM, I, NF, TVC(4), WIN(4), NXINT, TVX, TVY,
     *   IP1, IG2, GRVALU, NVALS, INEXT, MYVIS
      REAL      XZY(5,MAXCIF), TIME, TSEC, AMP, XY(2), SUMR, SUMI, SUMA,
     *   PHASE
      CHARACTER TSIGN*1, MSGBUF*72
      LOGICAL   FIRST
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IP1 = 1
      IG2 = NGRAY + 4
      IF (ABS(TVCORE(IMX,IMY,5)).LE.1.0) THEN
         MSGTXT = 'No data at selected pixel'
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       get the vis
      VCOUNT = 0
      NV = 0
      FIRST = .TRUE.
      NVALS = FVPNTR(1,IMX,IMY)
      INEXT = FVPNTR(2,IMX,IMY)
      DO 50 IVIS = 1,NVALS
         MYVIS = 0
         IF (INEXT.GT.0) MYVIS = FVCORE(1,INEXT)
         IF (MYVIS.GT.0) THEN
            INITVS = MYVIS
            CALL UVGET ('INIT', RPARM, BUFF1, IRET)
            IF (IRET.EQ.0) CALL UVGET ('READ', RPARM, BUFF1, IRET)
            IF (IRET.LT.0) THEN
               WRITE (MSGTXT,1005) MYVIS
               CALL MSGWRT (7)
            ELSE IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, MYVIS, 'READING VIS'
               CALL MSGWRT (8)
               GO TO 900
            ELSE
               CALL UVGET ('CLOS', RPARM, BUFF1, J)
               IF (ILOCB.GE.0) THEN
                  IA1 = INT (RPARM(ILOCB+1)) / 256
                  IA2 = MOD (INT (RPARM(ILOCB+1)), 256)
               ELSE
                  IA1 = RPARM(ILOCA1+1) + 0.1
                  IA2 = RPARM(ILOCA2+1) + 0.1
                  END IF
               TIME = RPARM(ILOCT+1)
               CALL TFDHMS (TIME, 1, TSIGN, ITT, TSEC)
               CALL FNDXY (RPARM, BUFF1, NPOL, XZY)
               ICO = ECHAN - BCHAN + 1
               LC = 0
               DO 40 LF = BIF,EIF
                  DO 35 IC = 1,ICO,CHINC
                     DO 30 LP = 1,NPOL
                        LC = LC + 1
                        IF ((XZY(1,LC).NE.FBLANK) .AND.
     *                     (XZY(2,LC).NE.FBLANK) .AND.
     *                     (XZY(5,LC).GT.0.0)) THEN
                           DO 20 J = 1,2
                              XY(J) = XYSCL(J) *
     *                           (XZY(J,LC)-XYOFF(J))
 20                           CONTINUE
C                                       this pixel ?
                           IX = XY(1) + 1.0
                           IY = XY(2) + 1.0
                           IF ((IX.EQ.IMX) .AND. (IY.EQ.IMY)) THEN
                              IC2 = IC + CHINC - 1
                              VCOUNT = VCOUNT + 1
                              AMP = SQRT (XZY(3,LC)**2 + XZY(4,LC)**2)
                              PHASE = 0.0
                              IF (AMP.NE.0) THEN
                                 PHASE = RAD2DG * ATAN2 (XZY(4,LC),
     *                              XZY(3,LC))
                                 IF (PHASE.GT.180.0) PHASE = PHASE-360.0
                                 IF (PHASE.LT.-180.0) PHASE = PHASE+360.
                                 END IF
C                                       header
                              IF (FIRST) THEN
                                 WRITE (MSGTXT,1010)
                                 CALL MSGWRT (4)
                                 FIRST = .FALSE.
                                 END IF
                              WRITE (MSGTXT,1020) VCOUNT, ITT, TSEC,
     *                           IA1, IA2, LF, IC, IC2, AMP, PHASE
                              IF (MSGTXT(16:16).EQ.' ') MSGTXT(16:16) =
     *                           '0'
                              CALL MSGWRT (4)
                              IF (NV.LT.MAXVIS) THEN
                                 NV = NV + 1
                                 VLANT(1,NV) = IA1
                                 VLANT(2,NV) = IA2
                                 VLIF(NV) = LF
                                 VLCHAN(1,NV) = IC
                                 VLCHAN(2,NV) = IC2
                                 VLTIME(NV) = TIME
                                 VLREAL(NV) = XZY(3,LC)
                                 VLIMAG(NV) = XZY(4,LC)
                                 VLBASL(NV) = 10000 * IA1 + IA2
                                 VLFVC(NV) = INEXT
                              ELSE
                                 WRITE (MSGTXT,1021) MAXVIS
                                 IF (NV.EQ.MAXVIS) CALL MSGWRT (7)
                                 NV = NV + 1
                                 END IF
                              END IF
                           END IF
 30                     CONTINUE
 35                  CONTINUE
 40               CONTINUE
               END IF
            END IF
         IF (INEXT.GT.0) INEXT = FVCORE(2,INEXT)
 50      CONTINUE
C                                       excessive??
      IF (NV.GT.MAXVIS) THEN
         MSGTXT = 'TOO MANY SAMPLES OR VISIBILITY RECORDS TO FIX'
         CALL MSGWRT (7)
         GO TO 900
         END IF
C                                       flag some ??
      NF = 0
      MSGBUF = 'Enter sample count number to flag.  0 ends flagging'
 100  IF (NV.GT.NF) THEN
         CALL INQINT (TTY, MSGBUF, 1, VCOUNT, IRET)
         IF ((IRET.EQ.0) .AND. (VCOUNT.GE.1) .AND. (VCOUNT.LE.NV)) THEN
            IF ((VLREAL(VCOUNT).NE.FBLANK) .AND.
     *         (VLIMAG(VCOUNT).NE.FBLANK)) THEN
               NF = NF + 1
               CALL VLFLAG (VCOUNT, VLTIME, VLCHAN, VLIF, VLANT, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1970) IRET, 'WRITING TO FC TANLE'
                  GO TO 980
                  END IF
               VLREAL(VCOUNT) = FBLANK
               VLIMAG(VCOUNT) = FBLANK
            ELSE
               MSGTXT = 'Sample already flagged'
               CALL MSGWRT (2)
               END IF
            GO TO 100
            END IF
         END IF
      IF ((NV.GT.0) .AND. (NF.GT.0)) THEN
         NFIXED = NFIXED + 1
         NSUM = 0
         SUMR = 0.0
         SUMI = 0.0
         SUMA = 0.0
         CALL FILL (NBL, 0, BLCORE(1,IMX,IMY))
         DO 120 I = 1,NV
           IF (VLREAL(I).NE.FBLANK) THEN
              SUMR = SUMR + VLREAL(I)
              SUMI = SUMI + VLIMAG(I)
              SUMA = SUMA + SQRT (VLREAL(I)**2 + VLIMAG(I)**2)
              NSUM = NSUM + 1
              CALL COUNTB (NX, NY, IMX, IMY, VLBASL(I), NBL, BLCORE)
           ELSE
              FVCORE(1,VLFVC(I)) = 0
              END IF
 120       CONTINUE
C                                       fix up sample
         IF (NSUM.GT.0) THEN
            WRITE (MSGTXT,1120) 'Restoring corrected', IMX, IMY
            CALL MSGWRT (3)
            TVCORE(IMX,IMY,1) = SUMA / NSUM
            TVCORE(IMX,IMY,2) = SQRT (SUMR*SUMR + SUMI*SUMI) / NSUM
            IF (TVCORE(IMX,IMY,2).LE.0.0) THEN
               PHASE = 0.0
            ELSE
               PHASE = RAD2DG * ATAN2 (SUMI, SUMR)
               IF (PHASE.GT.180.0) PHASE = PHASE - 360.0
               IF (PHASE.LT.-180.0) PHASE = PHASE + 360.0
               END IF
            TVCORE(IMX,IMY,3) = PHASE
            TVCORE(IMX,IMY,4) = TVCORE(IMX,IMY,1) - TVCORE(IMX,IMY,2)
            TVCORE(IMX,IMY,5) = ABS (TVCORE(IMX,IMY,5)) - NF
         ELSE
            WRITE (MSGTXT,1120) 'Flagging', IMX, IMY
            CALL MSGWRT (3)
            TVCORE(IMX,IMY,1) = FBLANK
            TVCORE(IMX,IMY,2) = FBLANK
            TVCORE(IMX,IMY,3) = FBLANK
            TVCORE(IMX,IMY,4) = FBLANK
            TVCORE(IMX,IMY,5) = 0.0
            FVPNTR(1,IMX,IMY) = 0
            END IF
         WRITE (MSGTXT,1121) NF
         IF (NF.GT.0) CALL MSGWRT(3)
C                                       update the TV
         IF (TVCORE(IMX,IMY,5).GT.0.0) THEN
            IX = GRVALU (TVCORE(IMX,IMY,IMGTYP))
            IY = 0
         ELSE
            IX = 0
            IY = 1
            END IF
         CALL COPY (4, TVCAT(IICOR), TVC)
         CALL COPY (4, TVCAT(IIWIN), WIN)
         NXINT = (TVC(3) - TVC(1) +1.0) / (WIN(3) - WIN(1) + 1.0) + 0.1
         TVX = (IMX - WIN(1)) * NXINT + TVC(1)
         TVY = (IMY - WIN(2)) * NXINT + TVC(2)
         IF (NXINT.EQ.1) THEN
            CALL YIMGIO ('WRIT', IP1, TVX, TVY, 0, 1, IX, IRET)
            IF (IRET.NE.0) GO TO 970
            CALL YIMGIO ('WRIT', IG2, TVX, TVY, 0, 1, IY, IRET)
            IF (IRET.NE.0) GO TO 970
         ELSE
            CALL FILL (NXINT, IX, IBUFF)
            CALL FILL (NXINT, IY, JBUFF)
            DO 130 I = 1,NXINT
               CALL YIMGIO ('WRIT', IP1, TVX, TVY, 0, NXINT,
     *            IBUFF, IRET)
               IF (IRET.NE.0) GO TO 970
               CALL YIMGIO ('WRIT', IG2, TVX, TVY, 0, NXINT, JBUFF,
     *            IRET)
               IF (IRET.NE.0) GO TO 970
               TVY = TVY + 1
 130           CONTINUE
            END IF
         END IF
      IRET = 0
C
 900  IF (IRET.NE.0) CALL UVGET ('CLOS', RPARM, BUFF1, J)
      GO TO 999
C
 970  WRITE (MSGTXT,1970) IRET, 'WRITING TO THE TV'
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EXAVIS ERROR',I4,' VIS',I10,' ON ',A)
 1005 FORMAT ('EXAVIS FINDS NO DATA FOR VIS NUMBER',I10)
 1010 FORMAT (' Cnt',5X,'Time',6X,'  An1 - An2  IF  Bchan-Echan  ',
     *   5X,'Amp  Phase')
 1020 FORMAT (I4,I4,'/',2(I2.2,':'),F4.1,I5.2,' -',I4.2,I4,2I7,F9.2,
     *   F7.1)
 1021 FORMAT ('Too many samples to remember: more than',I5)
 1120 FORMAT (A,' sample at pixel',2I5,1X,A)
 1121 FORMAT ('Writing',I5,' flag commands to FC table')
 1970 FORMAT ('EXAVIS ERROR',I4,' ON ',A)
      END
      SUBROUTINE VLFLAG (IC, VLTIME, VLCHAN, VLIF, VLANT, IRET)
C-----------------------------------------------------------------------
C   VLFLAG writes a record to an FC table for UFLAG
C   Inputs:
C      IC       I        Subscript being flagged in the other inputs
C      VLTIME   R(*)     Time
C      VLCHAN   I(2,*)   Spectral channels
C      VLIF     I(*)     IF
C      VLANT    I(2,*)   Antennas
C   Outputs:
C      IRET
C-----------------------------------------------------------------------
      INTEGER   IC, VLCHAN(2,*), VLIF(*), VLANT(2,*), IRET
      REAL      VLTIME(*)
C
      INCLUDE 'UFLAG.INC'
      INCLUDE 'UFLAGFIL.INC'
      INTEGER   VER, FCNUM, LASTR, I, LUN, ITIME(3), IDATE(3)
      REAL      TEPS
      CHARACTER TTIME(2)*12
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA TEPS/1.E-7/
C-----------------------------------------------------------------------
C                                       open/create
      VER = 1
      LUN = 89
      IF (.NOT.FCINIT) THEN
         CALL FNDEXT ('FC', CATUV, I)
         IF (I.GT.0) THEN
            MSGTXT = 'WARNING: USING A PRE-EXISTING FC TABLE'
            CALL MSGWRT (7)
         ELSE
            MSGSUP = 32000
            CALL CATFIX (DISKIN, CNOIN, '    ')
            MSGSUP = 0
            END IF
         FCINIT = .TRUE.
         END IF
      CALL FCINI ('WRIT', LUN, DISKIN, CNOIN, VER, CATUV, FCNUM, LASTR,
     *   FCBUFF, FCKOLS, FCNUMV, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING FC TABLE'
         GO TO 980
         END IF
C                                       set parameters
      FCROW = FCBUFF(5) + 1
      FCGTIM(1) = VLTIME(IC) - TEPS
      FCGTIM(2) = VLTIME(IC) + TEPS
      FCGANT(1) = VLANT(1,IC)
      FCGANT(2) = VLANT(2,IC)
      FCGSOR = 0
      IF (ONECHN) THEN
         FCGCHN(1) = VLCHAN(1,IC)
         FCGCHN(2) = VLCHAN(2,IC)
      ELSE
         FCGCHN(1) = 1
         FCGCHN(2) = NCHAN
         END IF
      IF (ONEIF) THEN
         FCGIF(1) = VLIF(IC)
         FCGIF(2) = VLIF(IC)
      ELSE
         FCGIF(1) = BIF
         FCGIF(2) = EIF
         END IF
      FCGSTK = '0000'
      DO 10 I = 1,4
         IF (PFLAGS(I)) FCGSTK(I:I) = '1'
 10      CONTINUE
      FCGSUB = SUBARR
      FCGFQ = FRQSEL
      FCGNUM = FCROW
      FCGOP = ' '
      FCGIT(1) = 0
      FCGIT(2) = 0
      LDTYPE = STOKES
      CALL RCOPY (2, FCGTIM, DTIMES)
      DFLUXS(1) = 0.0
      DFLUXS(2) = 0.0
      CALL ZTIME (ITIME)
      CALL ZDATE (IDATE)
      IDATE(1) = -IDATE(1)
      CALL TIMDAT (ITIME, IDATE, TTIME(2), TTIME)
      FCGREA = TSKNAM // TTIME(1)(:9) // ' ' // TTIME(2)(:8)
      CALL TABFC ('WRIT', FCBUFF, FCROW, FCKOLS, FCNUMV, FCGTIM,
     *   FCGANT, FCGSOR, FCGCHN, FCGIF, FCGSTK, FCGSUB, FCGFQ, FCGNUM,
     *   FCGOP, FCGIT, LDTYPE, DTIMES, DFLUXS, FCGREA, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE FC TABLE RECORD'
         GO TO 980
         END IF
      CALL TABFC ('CLOS', FCBUFF, FCROW, FCKOLS, FCNUMV, FCGTIM,
     *   FCGANT, FCGSOR, FCGCHN, FCGIF, FCGSTK, FCGSUB, FCGFQ, FCGNUM,
     *   FCGOP, FCGIT, LDTYPE, DTIMES, DFLUXS, FCGREA, I)
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VLFLAG ERROR',I4,' ON ',A)
      END
      SUBROUTINE FLAGUV (NX, NY, TVCORE, NA, NI, NP, XZY, MALL, MBAD,
     *   MSOM, IRET)
C-----------------------------------------------------------------------
C   FLAGUV replots uv data, compares with TVCORE, and writes flag info.
C   Inputs:
C      NX       I         X dimension of TVCORE
C      NY       I         Y dimension of TVCORE
C      NA       I         Number of antennas
C      NI       I         Number of IFs
C   In/out:
C      TVCORE   I(*,*,5)   Plot As, Av, (3)->flag, Wt Cnt+1
C   Output: only if PRTLEV > 1
C      MALL     I(*)       Counts samples by baseline etc
C      MBAD     I(*)       Counts full bad spectra by baseline
C      MSOM     I(*)       Counts partly bad spectra by baseline
C      IRET     I          Return code, 0 => OK, otherwise abort.
C                             4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NP, NA, NI, MALL(NA,NA,NP,NI),
     *   MBAD(NA,NA,NP,NI), MSOM(NA,NA,NP,NI), IRET
      REAL      XZY(5,NP,*), TVCORE(NX,NY,5)
C
      INCLUDE 'UFLAG.INC'
      INCLUDE 'UFLAGFIL.INC'
      CHARACTER ATIME*8, ADATE*12, REASON*24, BNDCOD(MAXIF)*8
      INTEGER   I, IC, J, JJJ, ICO, NUMVIS, JSUB, ISUB, NXLUN, NIF,
     *   NXVER, LC, LF, IX, IY, DOUNT(3,3), LUN, LFGRNO, IA1, IA2,
     *   LBIF, LEIF, LBCH, LECH, TIME(3), DATE(3), FGBUFL(512), NIN,
     *   NBAD, LP, DIDALL(MAXANT), NFLAG, VER, FCNUM, LASTR, LUNFC,
     *   NROWS, JBCH, JECH
      REAL      BLC(2), TRC(2), XY(2), BTIME, ETIME
      HOLLERITH CATH(256)
      LOGICAL   REQBAS, FLAGDO, FLAGED, AFLAGS(4), GOTONE
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
      FLAGED = .FALSE.
      IRET = 1
      CALL FILL (9, 0, DOUNT)
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
      NFLAG = 0
      INITVS = 1
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
C                                       Initialize UV reading.
            CALL UVGET ('INIT', RPARM, BUFF1, IRET)
C
            IF (IRET.EQ.-1) GO TO 140
            IF (IRET.EQ.5) GO TO 140
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'INIT VIS FILE'
               IRET = 4
               GO TO 980
               END IF
            NPOL = NCOR
C                                       Loop: Read vis. record.
 100        CALL UVGET ('READ', RPARM, BUFF1, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING VIS FILE'
               IRET = 4
               GO TO 980
            ELSE IF (IRET.EQ.0) THEN
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
               IF (CURSOU.NE.CSOU) THEN
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
                     IF (PRTLEV.GT.1.0) MALL(IA1,IA2,LP,LF) =
     *                  MALL(IA1,IA2,LP,LF) + 1
                     LC = (LF - 1) * ((ICO-1)/CHINC + 1)
                     DO 130 IC = 1,ICO,CHINC
                        LC = LC + 1
                        IF ((XZY(1,LP,LC).NE.FBLANK) .AND.
     *                     (XZY(2,LP,LC).NE.FBLANK) .AND.
     *                     (XZY(5,LP,LC).GT.0.0)) THEN
                           NIN = NIN + CHINC
                           IF (ONECHN) THEN
                              LBCH = IC + BCHAN -1
                              LECH = LBCH + NCHAV - 1
                              END IF
                           DO 120 J = 1,2
                              XY(J) = XYSCL(J) * (XZY(J,LP,LC)-XYOFF(J))
 120                          CONTINUE
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
                                 IF (TVCORE(IX,IY,5).GE.0.0) THEN
                                    IF (TVCORE(IX,IY,5).GT.1.0) THEN
                                       TVCORE(IX,IY,5) = TVCORE(IX,IY,5)
     *                                    - 1.0
                                    ELSE IF (TVCORE(IX,IY,5).GE.0.0)
     *                                 THEN
                                       TVCORE(IX,IY,5) = 1000000000.
                                       END IF
                                    IF (GOTONE) THEN
                                       GOTONE = .FALSE.
                                       NFGWRI = NFGWRI + 1
                                       NFLAG = NFLAG + 1
                                       CALL FLAGIT ('FLAG', LUN, DISKIN,
     *                                    CNOIN, FGVERI, FGVERO, LFGRNO,
     *                                    FGKOLS, FGNUMV, CURSOU,
     *                                    SUBARR, IFRQ, IA1, IA2, BTIME,
     *                                    ETIME, LBIF, LEIF, JBCH, JECH,
     *                                    PFLAGS(1), REASON, CATUV,
     *                                    FGBUFL, IRET)
                                       IF (IRET.NE.0) GO TO 999
                                       END IF
C                                       flagging this point
                                 ELSE
                                    IF (TVCORE(IX,IY,5).LT.-1.0) THEN
                                       TVCORE(IX,IY,5) =
     *                                    TVCORE(IX,IY,5) + 1.0
                                    ELSE
                                       TVCORE(IX,IY,5) = -1000000000.
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
                                          IF (.NOT.GOTONE) JBCH = LBCH
                                          JECH = LECH
                                          GOTONE = .TRUE.
                                          END IF
                                       END IF
                                    FLAGDO = ONECHN
                                    END IF
                              ELSE
                                 DOUNT(2,3) = DOUNT(2,3) + 1
                                 END IF
                              END IF
                           END IF
 130                    CONTINUE
                     IF (GOTONE) THEN
                        GOTONE = .FALSE.
                        NFGWRI = NFGWRI + 1
                        NFLAG = NFLAG + 1
                        CALL FLAGIT ('FLAG', LUN, DISKIN, CNOIN, FGVERI,
     *                     FGVERO, LFGRNO, FGKOLS, FGNUMV, CURSOU,
     *                     SUBARR, IFRQ, IA1, IA2, BTIME,ETIME, LBIF,
     *                     LEIF, JBCH, JECH, PFLAGS(1), REASON, CATUV,
     *                     FGBUFL, IRET)
                           IF (IRET.NE.0) GO TO 999
                        END IF
                     IF ((NBAD.GE.NIN) .AND. (NBAD.GT.0)) THEN
                        IF (PRTLEV.GT.1.0) MBAD(IA1,IA2,LP,LF) =
     *                     MBAD(IA1,IA2,LP,LF) + 1
                        NBDCOR = NBDCOR + NCHAN
                     ELSE IF (NBAD.GT.0) THEN
                        IF (PRTLEV.GT.1.0) MSOM(IA1,IA2,LP,LF) =
     *                     MSOM(IA1,IA2,LP,LF) + 1
                        NBDCOR = NBDCOR + NBAD
                        END IF
 131                 CONTINUE
 135              CONTINUE
               GO TO 100
               END IF
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
                  IF (BLFLAG(I,J).GT.0) THEN
                     JJJ = JJJ + 1
                     IF (PRTLEV.GT.1) THEN
                        DO 143 LP = 1,NP
                           DO 142 LF = 1,NI
                              MBAD(I,J,LP,LF) = MBAD(I,J,LP,LF) + 1
 142                          CONTINUE
 143                       CONTINUE
                        END IF
                     END IF
 144              CONTINUE
               IF (JJJ.EQ.MANT) THEN
                  DIDALL(I) = 1
                  NFGWRI = NFGWRI + 1
                  NFLAG = NFLAG + 1
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
                     NFLAG = NFLAG + 1
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
         WRITE (MSGTXT,1170) DOUNT(1,1), DOUNT(2,1), DOUNT(3,1)
         CALL MSGWRT (2)
         WRITE (MSGTXT,1170) DOUNT(1,2), DOUNT(2,2), DOUNT(3,2)
         CALL MSGWRT (2)
         WRITE (MSGTXT,1170) DOUNT(1,3), DOUNT(2,3), DOUNT(3,3)
         CALL MSGWRT (2)
         END IF
C                                       Check image itself
      JJJ = 0
      DO 180 J = 1,NY
         DO 175 I = 1,NX
            IF (ABS(TVCORE(I,J,5)).GT.1.0) THEN
               JJJ = JJJ + 1
               END IF
 175        CONTINUE
 180     CONTINUE
      IF (JJJ.GT.NFIXED) THEN
         WRITE (MSGTXT,1180) JJJ - NFIXED
         CALL MSGWRT (7)
         END IF
C                                       FC table
      IF (FCINIT) THEN
         VER = 1
         LUNFC = 89
         CALL FCINI ('READ', LUNFC, DISKIN, CNOIN, VER, CATUV, FCNUM,
     *      LASTR, FCBUFF, FCKOLS, FCNUMV, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING FC TABLE'
            GO TO 980
            END IF
C                                       set parameters
         NROWS = FCBUFF(5)
         DO 300 I = 1,NROWS
            FCROW = I
            CALL TABFC ('READ', FCBUFF, FCROW, FCKOLS, FCNUMV, FCGTIM,
     *         FCGANT, FCGSOR, FCGCHN, FCGIF, FCGSTK, FCGSUB, FCGFQ,
     *         FCGNUM, FCGOP, FCGIT, LDTYPE, DTIMES, DFLUXS, FCGREA,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ FC TABLE RECORD'
               GO TO 980
               END IF
            NFGWRI = NFGWRI + 1
            NFLAG = NFLAG + 1
            CALL FLAGIT ('FLAG', LUN, DISKIN, CNOIN, FGVERI, FGVERO,
     *         LFGRNO, FGKOLS, FGNUMV, FCGSOR, FCGSUB, FCGFQ, FCGANT(1),
     *         FCGANT(2), FCGTIM(1), FCGTIM(2), FCGIF(1), FCGIF(2),
     *         FCGCHN(1), FCGCHN(2), PFLAGS(1), FCGREA, CATUV,
     *         FGBUFL, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING FLAG TABLE RECORD'
               GO TO 980
               END IF
            IF (PRTLEV.GT.1.0) THEN
               DO 210 LF = FCGIF(1),FCGIF(2)
                  JJJ = LF - BIF + 1
                  IA1 = FCGANT(1)
                  IA2 = FCGANT(2)
                  IF ((FCGCHN(1).LE.1) .AND. (FCGCHN(2).GE.NCHAN)) THEN
                     MBAD(IA1,IA2,1,JJJ) = MBAD(IA1,IA2,1,JJJ) + 1
                  ELSE
                     MSOM(IA1,IA2,1,JJJ) = MSOM(IA1,IA2,1,JJJ) + 1
                     END IF
 210              CONTINUE
               END IF
 300        CONTINUE
         CALL FLAGIT ('CLOS', LUN, DISKIN, CNOIN, FGVERI, FGVERO,
     *      LFGRNO, FGKOLS, FGNUMV, FCGSOR, FCGSUB, FCGFQ, FCGANT(1),
     *      FCGANT(2), FCGTIM(1), FCGTIM(2), FCGIF(1), FCGIF(2),
     *      FCGCHN(1), FCGCHN(2), PFLAGS(1), FCGREA, CATUV,
     *      FGBUFL, I)
         CALL TABFC ('CLOS', FCBUFF, FCROW, FCKOLS, FCNUMV, FCGTIM,
     *      FCGANT, FCGSOR, FCGCHN, FCGIF, FCGSTK, FCGSUB, FCGFQ,
     *      FCGNUM, FCGOP, FCGIT, LDTYPE, DTIMES, DFLUXS, FCGREA, I)
         END IF
      WRITE (MSGTXT,1160) NFLAG, FGVERO
      CALL MSGWRT (4)
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLAGUV ERROR',I4,' ON ',A)
 1150 FORMAT ('FLAGUV: ',I10,' DIFFERENT POINTS FELL IN ARRAY')
 1155 FORMAT ('FLAGUV: ',I10,' DIFFERENT POINTS DID NOT FIT')
 1160 FORMAT ('FLAGUV wrote',I8,' flag records to FG file version',I4)
 1170 FORMAT ('FLAGUV: ',I10,' * ',I10,' * ',I10)
 1180 FORMAT ('FLAGUV: ',I10,' PIXELS NOT PROPERLY ACCOUNTED FOR')
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
     *   IFGRNO, OROW, LVERI, LVERO
      LOGICAL   PFLAGS(4), TFLAGS(4), FIRST
      REAL      TIMER(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE FIRST, LVERI, LVERO
      DATA FIRST /.TRUE./
      DATA LVERI, LVERO /0, 0/
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
            FIRST = (VER.NE.LVERO) .OR. (VERI.NE.LVERI)
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
            LVERI = VERI
            LVERO = VER
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
 1002 FORMAT ('Copy',I10,' of',I10,' rows from FG vers',I3,' to',I3)
 1010 FORMAT ('ERROR ',A,'ING FG TABLE IN ROW',I7,I5)
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
      SUBROUTINE FLAGHI (NX, NY, NLOOPS, NSTOKS, NBD, FLP)
C-----------------------------------------------------------------------
C   FLAGHI appends to the history file.
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NLOOPS, NSTOKS, NBD, FLP
C
      INCLUDE 'UFLAG.INC'
      CHARACTER ATIME*8, ADATE*12, HILINE*72, TEXT*9, CHTYPE(2)*9,
     *   AXIS(2)*1, STCHAR(12)*2
      INTEGER   LUN, IERR, I, TIME(3), DATE(3), HBUFF(256), J, NC,
     *   JTRIM, IROUND
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA LUN /28/
      DATA CHTYPE /'U ', 'V '/
      DATA AXIS /'X','Y'/
      DATA STCHAR /'RR','LL', 'RL','LR', 'XX','YY', 'XY','YX',
     *   'I','V', 'Q','U'/
C-----------------------------------------------------------------------
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
      I = IROUND (XFLAG)
      WRITE (HILINE,1112) TSKNAM, I
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
      I = IROUND (XFGOUT)
      IF (NLOOPS.LE.1) THEN
         WRITE (HILINE,1113) TSKNAM, I
      ELSE
         WRITE (HILINE,2113) TSKNAM, I, FGVERO
         END IF
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
C                                       Stokes
      IF (NSTOKS.LE.1) THEN
         J = JTRIM (STOKES)
         HILINE = TSKNAM // 'STOKES = ''' // STOKES(:J) // ''''
      ELSE
         J = 0
         DO 15 I = 1,12
            IF (STOKES.EQ.STCHAR(I)) J = I
 15         CONTINUE
         J = ((J - 1) / 2) * 2 + 1
         I = JTRIM (STCHAR(J))
         HILINE = TSKNAM // 'STOKES = ''' // STCHAR(J)(:I) //
     *      ''' and ''' // STCHAR(J+1)(:I) // ''''
         END IF
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
         TEXT = CHTYPE(I)
         NC = JTRIM (TEXT)
         WRITE (HILINE,1119) TSKNAM, AXIS(I), TEXT(:NC)
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         J = 2 * (I + 1)
         WRITE (HILINE,1120) TSKNAM, AXIS(I), XYMIN(I)
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (HILINE,1121) TSKNAM, AXIS(I), XYMAX(I)
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         J = NX
         IF (I.EQ.2) J = NY
         WRITE (HILINE,1122) TSKNAM, AXIS(I), J
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
 20      CONTINUE
C                                       Number of flagged correlators
      WRITE (HILINE,1123) TSKNAM, NBD
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (HILINE,1124) TSKNAM, FLP
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
 1112 FORMAT (A6,'FLAGVER  =',I4,' /Input flagging table applied')
 1113 FORMAT (A6,'FLAGVERO =',I4,' /Flagging table written')
 2113 FORMAT (A6,'FLAGVERO =',I4,' -',I4,' /Flagging tables written')
 1114 FORMAT (A6,'BIF =',I4,', EIF =',I4,'/ IF range')
 1115 FORMAT (A6,'BCHAN = ',I4,' ECHAN = ',I4,
     *   ' /Start and stop channels')
 1116 FORMAT (A6,'GAINUSE =',I3,' / CL table')
 1117 FORMAT (A6,'DOBAND =',I2,'  /BP correction done')
 1118 FORMAT (A6,'BPVER =',I3,' / BP correction used BP table')
 1119 FORMAT (A6,A1,'TYPE = ''',A,''' / plot axis type')
 1120 FORMAT (A6,A1,'MIN =',1PE12.5,'  / plot axis minimum')
 1121 FORMAT (A6,A1,'MAX =',1PE12.5,'  / plot axis maximum')
 1122 FORMAT (A6,A1,'PIXELS =',I6,'  / plot axis number pixels')
 1123 FORMAT (A6,'/ Correlators flagged:',I8)
 1124 FORMAT (A6,'/ TV pixels flagged:  ',I8)
 1125 FORMAT (A6,'/ FG records written: ',I8)
 1130 FORMAT (I10,' records written to FG table vers',I4)
      END
