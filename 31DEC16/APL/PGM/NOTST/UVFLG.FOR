LOCAL INCLUDE 'UVFLG.INC'
C                                       Local include for UVFLG
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   XMXFLG, MXSOU
C                                       XMXFLG = max number of flags
      PARAMETER (XMXFLG = 40000, MXSOU = XSTBSZ)
      INTEGER   SEQ, DISK, LUN2, FIND2, FGVER, IERANT, NFLAG,
     *   FANT1(XMXFLG), FANT2(XMXFLG), FIFLIM(2,XMXFLG),
     *   FCHLIM(2,XMXFLG), FSUBA(XMXFLG), NID, ID(MXSOU), MXFLAG,
     *   NUMAN(513), CNOIN, BUFSZ, FRQSEL, FFQID(XMXFLG), NMESS, NOMIF,
     *   WFLAG, NECHO, KUAL, IBUFF(UVBFSS), IBUFF2(UVBFSS)
      LOGICAL   FFLAGS(4,XMXFLG), FSET, USEMSK, TABLE, MATCH, MULTI,
     *   ISCOMP, KFGRNO, ELEVAT, PULCAL, SHADOW
      INTEGER   FCOUNT(XMXFLG), GCOUNT(XMXFLG)
      HOLLERITH XNAME(3), XCLASS(2), XINFIL(12), XXSOUR(4,30), XCALC,
     *   XSTOKE, XOPCOD, XREAS(6)
      CHARACTER NAME*12, CLASS*6, INFILE*48, XSOUR(30)*16, STOKES*4,
     *   OPCODE*4, REASON*24, REESON(XMXFLG)*24, CALKOD*4
      REAL      XSEQIN, XVIN, XQUAL, XSUBAR, XBAND, XFREQ, XFQID,
     *   TIMER(8), XBCHAN, XECHAN, XBIF, XEIF, XANTS(50), XBASE(50),
     *   XFGVER, APARM(10), DOHIST, BUFF(UVBFSS), BUFF2(UVBFSS),
     *   FLAGT1(XMXFLG), FLAGT2(XMXFLG), SELBAN
      DOUBLE PRECISION SELFRQ
      EQUIVALENCE (BUFF, IBUFF), (BUFF2, IBUFF2)
      INCLUDE 'INCS:DCAT.INC'
C
      COMMON /FLAGS/ FLAGT1, FLAGT2, FCOUNT, GCOUNT, FFLAGS, FSET,
     *   USEMSK, TABLE, KFGRNO, NOMIF, FANT1, FANT2, FIFLIM, FCHLIM,
     *   FSUBA, FFQID, NFLAG, MXFLAG, NID, ID, NUMAN, NMESS, IERANT,
     *   WFLAG, ELEVAT, PULCAL, SHADOW, NECHO, KUAL
      COMMON /BUFRS/ BUFF, BUFF2, BUFSZ
      COMMON /INPARM/ XNAME, XCLASS, XSEQIN, XVIN, XINFIL, XXSOUR,
     *   XCALC, XQUAL, XSUBAR, XBAND, XFREQ, XFQID, TIMER, XBCHAN,
     *   XECHAN, XBIF, XEIF, XANTS, XBASE, XSTOKE, XFGVER, APARM,
     *   XOPCOD, XREAS, DOHIST
      COMMON /CHRCOM/ NAME, CLASS, INFILE, XSOUR, STOKES, OPCODE,
     *   REASON, REESON, CALKOD
      COMMON /FQCRAP/ SELFRQ, SELBAN, MATCH, SEQ, DISK, CNOIN, LUN2,
     *   FIND2, FGVER, FRQSEL, MULTI, ISCOMP
LOCAL END
      PROGRAM UVFLG
C-----------------------------------------------------------------------
C! Flag selected uv data
C# UV Calibration Editing
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2001, 2003, 2006-2012, 2014-2016
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
C   Task UVFLG flags selected portions of a uv data base.
C   Data either flagged directly or an entry is made in the flagging
C   table.
C   Inputs:
C      AIPS Adverb  Prg. Name      Description
C      INNAME       NAME        File name of data base to be flagged
C      INCLASS      CLASS       Input file class.
C      INSEQ        SEQ         Input file sequence number.
C      INDISK       DISK        Disk volumn on which file resides.
C      IN2NAME      NAME2       Name of run file with flag info.
C      SOURCES      XSOUR(4,30) Names of sources specified.
C      SUBARRAY     FSUBA       Subarray number
C      SELBAND      SELBAN      Bandwidth in FQ table
C      SELFREQ      SELFRQ      Freq in FQ table
C      FREQID       FFQID      Freq ID to flag
C      TIMERANG     TIMER(8)    Time range
C      BCHAN        BCHAN       First frequency channel
C      ECHAN        ECHAN       Last frequency channel
C      BIF          BIF         First IF
C      EIF          EIF         Last IF
C      ANTENNAS     ANTS(30)    Antenna specified
C      BASELINE     BASE(30)    Baselines
C      STOKES       STOK        Stokes' parameter.'R','L','RL'
C      FLAGVER      FGVER       Flag file version.
C      OPCODE       OPCO        'FLAG' or 'UNFL' or 'REAS'
C      REASON       REAS(6)     Reason to flag data.
C   Revised to read tables from the RUN area, April 1984.
C   Revised to use flagging tables June 1985.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'UVFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA PRGM /'UVFLG '/
C-----------------------------------------------------------------------
C                                       Get inputs.
      CALL UVFGIN (PRGM, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Write history
      CALL UVFGHS
C                                       Close down
 990  CALL DIE (IRET, FLAGT1)
C
 999  STOP
      END
      SUBROUTINE UVFGIN (PRGM, IRET)
C-----------------------------------------------------------------------
C   UVFGIN does the startup bookeeping and reads the flagging table
C   from the specified text file if necessary.
C   Input:
C   PRGM       C*6  Task name.
C   Output:
C     IRET         I    Return error code, 0=> OK, otherwise failed.
C   Output to common /FLAGS/:
C     TABLE        L   If TRUE then flagging info put in FG table .
C     NFLAG        I   Number of flagging criteria in common.
C     FANT1(*)     I   First antenna of pair, 0=>all
C     FANT2(*)     I   Second antenna of pair, 0=>all
C     FFLAGS(4,*)  L   Correlator/polarization flags.
C     FLAGT1(*)    R   Start time (days since reference day in CATBLK)
C     FLAGT2(*)    R   End time.
C     FVS1(*)      I   First vis. to consider flagging
C     FVS2(*)      I   Last vis. to consider flagging
C     FLGIF(*)     I   Correlator flag 0=>flag R&L,1=>R,2=>L
C                      -1=>unflag R, -2=> L, -3 => both.
C     FIFLIM(2,*)  I   First and last IF's selected 0=all
C     FCHLIM(2,*)  I   First and last channels selected 0=all
C     FSUBA(*)     I   Subarray to flag
C     FFQID(*)     I   FQ ID to flag
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UTYPE*2
      LOGICAL   T, F, CORR
C                                       Max. no. of time intervals on
C                                       any given baseline for elev.
C                                       flagging.
      INTEGER   MXTIM
      PARAMETER (MXTIM = 10)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NPARM, I, IERR, IRET, NSOUR, IROUND, LUN, II, LUNAN,
     *   XA1(MXBASE), XA2(MXBASE), NBASE, MAXBAS, KBCH, KECH, KBIF,
     *   KEIF, NOCHAN, NOIF, IFGRNO, LUNFQ, SUBA, FQD, NTIM, IFGVER, K,
     *   FGBUFF(512)
      REAL      T1(MXTIM), T2(MXTIM), STARTD, STOPD, ELEVTH,
     *   ELEVTL, FACTOR, SHMIN, CTMIN
      LOGICAL   WSBLNK, FGOPEN
      INCLUDE 'UVFLG.INC'
      INTEGER   FGKOLS(MAXFGC), FGNUMV(MAXFGC)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUN, LUNAN, LUNFQ /27, 30, 40/
      DATA MAXBAS /MXBASE/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
      TABLE = F
      NMESS = 0
C                                       Init I/O
      CALL ZDCHIN (T)
      CALL VHDRIN
      BUFSZ = UVBFSS * 2
      LUN2 = 17
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      MXFLAG = XMXFLG
      NFLAG = 0
      WFLAG = 0
C                                       Get input parameters.
      NPARM = 277
      IRET = 0
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAME, IBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         RQUICK = F
         IRET = 8
         GO TO 990
         END IF
C                                       Decode input.
      SEQ = XSEQIN + 0.1
      DISK = XVIN + 0.1
C                                       Characters
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL H2CHR (4, 1, XSTOKE, STOKES)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (24, 1, XREAS, REASON)
      CALL H2CHR (4, 1, XCALC, CALKOD)
      KUAL = IROUND (XQUAL)
      IF ((OPCODE.NE.'UFLG') .AND. (OPCODE.NE.'REAS') .AND.
     *   (OPCODE.NE.'WILD')) OPCODE = 'FLAG'
C                                       Test for flag mask.
      IF (STOKES.EQ.'CROS') STOKES = '0011'
      IF ((STOKES(1:1).EQ.'0') .OR. (STOKES(1:1).EQ.'1'))
     *   USEMSK = .TRUE.
      NECHO = IROUND (APARM(7))
      IF (NECHO.EQ.0) NECHO = 100
C
      WSBLNK = T
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
         IF (XSOUR(I).NE.' ') WSBLNK = F
 20      CONTINUE
C                                       Open file and get CATBLK.
C                                       open file non-excl to allow
C                                       a second open as well:
      UTYPE = 'UV'
      CALL MAPOPN ('HDWR', DISK, NAME, CLASS, SEQ, UTYPE, NLUSER, LUN2,
     *   FIND2, CNOIN, CATBLK, IBUFF, IERR)
      IF (IERR.GT.1) THEN
         WRITE (MSGTXT,1010) IERR
         RQUICK = F
         IRET = 8
         GO TO 990
         END IF
C                                       Mark in /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
C                                       Restart AIPS.
      IF (RQUICK) CALL RELPOP (IRET, IBUFF, IERR)
      IF (IRET.NE.0) GO TO 990
      IRET = 8
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Get info from CATBLK.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check if multi source file
      CALL MULSDB (CATBLK, MULTI)
C                                       Write table
      CALL FNDEXT ('FG', CATBLK, IFGVER)
      FGVER = XFGVER + 0.1
      IF (FGVER.LE.0) FGVER = MAX (1, IFGVER)
C                                       Get number(s) of antennas.
C                                       Since GETNAN will return
C                                       a reasonable default -
C                                       ignore error code.
      CALL GETNAN (DISK, CNOIN, CATBLK, LUNAN, IBUFF, NUMAN, IERANT)
C                                       Also get antenna names &
C                                       numbers
      SUBA = IROUND(XSUBAR)
      IF (SUBA.EQ.0) SUBA = 1
      IF (IERANT.EQ.0) THEN
         CALL GETANT (DISK, CNOIN, SUBA, CATBLK, IBUFF, IERANT)
         IF (IERANT.NE.0) THEN
            WRITE (MSGTXT,1030) IERANT
            CALL MSGWRT (6)
            IRET = 7
            GO TO 999
            END IF
         END IF
C                                       If multi-source get numbers.
C                                       Set max. no sources named.
      NSOUR = 30
      ELEVAT = (APARM(2).GT.0.0) .AND. (APARM(2).GT.APARM(1))
      IF ((ELEVAT) .AND. (APARM(1).EQ.0.0)) APARM(1) = -90.0
      IF ((.NOT.ELEVAT) .AND. (APARM(1).NE.0.0)) THEN
         MSGTXT = 'APARM(1)-APARM(2) IMPROPER ELEVATION RANGE'
         CALL MSGWRT (8)
         IRET = 5
         GO TO 999
         END IF
C                                       shadowing
      SHADOW = APARM(5).GT.0.0
      IF ((ELEVAT) .AND. (SHADOW)) THEN
         MSGTXT = 'ELEVATION FLAGGING REQUESTED AS WELL AS SHADOWING'
         CALL MSGWRT (8)
         IRET = 5
         GO TO 999
         END IF
C                                       factor of wanted suppression of
C                                       spurios fringes near fringe
C                                       rateequled zero
C
      FACTOR = APARM(3)
      PULCAL = FACTOR.GE.1
      IF ((PULCAL) .AND. (ELEVAT)) THEN
         MSGTXT = 'PULSE-CAL FLAGGING REQUESTED AS WELL AS ELEVATION'
         CALL MSGWRT (8)
         IRET = 5
         GO TO 999
         END IF
      IF ((PULCAL) .AND. (SHADOW)) THEN
         MSGTXT = 'PULSE-CAL FLAGGING REQUESTED AS WELL AS SHADOWING'
         CALL MSGWRT (8)
         IRET = 5
         GO TO 999
         END IF
C                                       Set max. no. sources sel.
      NID = 1
      ID(1) = 0
      IF (MULTI) THEN
C                                       ALLSOU gives the full list
C                                       of source IDs if XSOUR=BLANK.
C                                       This is needed for the elev.
C                                       calculations.
         IF ((ELEVAT .OR. PULCAL .OR. SHADOW) .AND. WSBLNK) THEN
            CALL ALLSOU (DISK, CNOIN, CATBLK, CALKOD, KUAL, IBUFF, ID,
     *         NID, IRET)
            IF (IRET.NE.0) GO TO 999
         ELSE IF ((OPCODE.EQ.'UFLG') .AND. (XSOUR(1).EQ.'-')) THEN
            NID = 1
            ID(1) = -1
         ELSE
            NID = MXSOU
            CALL SOURNU (XSOUR, KUAL, CALKOD, NSOUR, DISK, CNOIN, NID,
     *         IBUFF, ID, IRET)
            IF (IRET.LT.0) THEN
               MSGTXT = 'SOURCE(S) NOT FOUND IN SU TABLE'
               CALL MSGWRT (7)
               IRET = 5
               END IF
            IF (IRET.NE.0) GO TO 999
            IF (NID.EQ.0) NID = 1
            END IF
         END IF
C                                       Flag with INFILE
      IF (INFILE(1:20).NE.' ') THEN
         IF (IERANT.NE.0) THEN
            MSGTXT = 'TEXT FILE FLAGGING REQUIRES AN ANTENNA TABLE'
            CALL MSGWRT (7)
            IRET = 7
         ELSE
            CALL UVFTRD (INFILE, IRET)
            END IF
C                                       flag with adverbs
      ELSE
C                                       Freq id
         IF (XBAND.GT.0.0) SELBAN = XBAND
         IF (XFREQ.GT.0.0) SELFRQ = XFREQ
         FRQSEL = IROUND (XFQID)
         IF (FRQSEL.LT.0) FRQSEL = -1
         IF (FRQSEL.GE.0) THEN
            CALL FQMATC (DISK, CNOIN, CATBLK, LUNFQ, SELBAN, SELFRQ,
     *         MATCH, FRQSEL, IRET)
            IF (.NOT.MATCH) THEN
               WRITE (MSGTXT,1070)
               IRET = 1
               GO TO 990
               END IF
            IF (IRET.GT.0) GO TO 999
            END IF
C                                       Set flagging info from ADVERBS.
         FCOUNT(1) = 0
         GCOUNT(1) = 0
         FSUBA(1) = IROUND (XSUBAR)
         FFQID(1) = FRQSEL
C                                       Timerange
         FLAGT1(1) = TIMER(1) + TIMER(2)/24. +
     *      TIMER(3)/(24.*60.) + TIMER(4)/(24.*60.*60.)
         FLAGT2(1) = TIMER(5) + TIMER(6)/24. +
     *      TIMER(7)/(24.*60.) + TIMER(8)/(24.*60.*60.)
C                                       If both zero flag all times
         IF (FLAGT2(1).GE.0.0) THEN
            IF (FLAGT2(1).EQ.0.0) FLAGT2(1) = FLAGT1(1)
            IF ((FLAGT1(1).EQ.0.0) .AND. (FLAGT2(1).EQ.0.0))
     *         FLAGT2(1) = 9999.
            END IF
C                                       Tweak range a bit.
         IF (FLAGT1(1).EQ.FLAGT2(1)) THEN
            FLAGT1(1) = FLAGT1(1) - 1.0 / 172800.
            FLAGT2(1) = FLAGT2(1) + 1.0 / 172800.
            END IF
C                                       Check that start before finish.
         IF (FLAGT1(1).GT.FLAGT2(1)) THEN
            MSGTXT = 'ERROR: TIME1 > TIME2'
            IRET = 8
            GO TO 990
            END IF
C                                       IF, channel limits
         KBCH = IROUND (XBCHAN)
         NOCHAN = CATBLK(KINAX+JLOCF)
C                                       Check that KBCH < NOCHAN.
         IF ((KBCH.GT.NOCHAN) .AND. (.NOT.ELEVAT) .AND. (.NOT.SHADOW))
     *      THEN
            WRITE (MSGTXT,1240) KBCH, NOCHAN
            IRET = 1
            GO TO 990
            END IF
         IF ((OPCODE.NE.'UFLG') .OR. (KBCH.GE.0)) KBCH =
     *      MAX (1, MIN (KBCH, NOCHAN))
         KECH = IROUND (XECHAN)
         IF (OPCODE.NE.'UFLG') KECH = MAX (0, KECH)
         IF ((KECH.GT.0) .AND. (KECH.LT.KBCH)) GO TO 910
         IF (JLOCIF.GE.0) THEN
            NOIF = CATBLK(KINAX+JLOCIF)
            KBIF = IROUND (XBIF)
C                                       Check that (startIF) < NOIF.
            IF ((KBIF.GT.NOIF) .AND. (.NOT.ELEVAT) .AND. (.NOT.SHADOW))
     *         THEN
               WRITE (MSGTXT,1220) KBIF, NOIF
               IRET = 1
               GO TO 990
               END IF
            KEIF = IROUND (XEIF)
            IF (OPCODE.NE.'UFLG') THEN
               KBIF = MAX (1, MIN (KBIF, CATBLK(KINAX+JLOCIF)))
               IF ((KEIF.GT.0) .AND. (KEIF.LT.KBIF)) GO TO 900
               IF (KBIF.GT.KEIF) KEIF = CATBLK(KINAX+JLOCIF)
               KEIF = MAX (1, MIN (KEIF, CATBLK(KINAX+JLOCIF)))
            ELSE
               IF (KBIF.GE.0) KBIF = MAX (1, MIN (KBIF,
     *            CATBLK(KINAX+JLOCIF)))
               IF ((KEIF.GT.0) .AND. (KEIF.LT.KBIF)) GO TO 900
               IF ((KBIF.GT.KEIF) .AND. (KEIF.GE.0)) KEIF =
     *            CATBLK(KINAX+JLOCIF)
               IF (KEIF.GE.0) KEIF = MAX (1, MIN (KEIF,
     *            CATBLK(KINAX+JLOCIF)))
               END IF
         ELSE
            NOIF = 1
            KBIF = 1
            KEIF = 1
            END IF
         NOMIF = NOIF
C                                       Convert to baseline numbers.
         NBASE = MAXBAS
         CALL AN10RS (NUMAN, FSUBA(1), XANTS, XBASE, NBASE, XA1, XA2)
C                                       Timerange
C                                       Determine start/stop of data
         CALL UVTIME (DISK, CNOIN, CATBLK, STARTD, STOPD, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020)
            GO TO 990
            END IF
C                                       Flag data on elevation/PC with
C                                       the specified timerange
         IF ((FLAGT1(1).NE.0.0) .OR. (FLAGT2(1).NE.9999.0)) THEN
            STARTD = MAX (STARTD, FLAGT1(1))
            STOPD = MIN (STOPD, FLAGT2(1))
            END IF
C                                       Flagging on elevation
         IF (ELEVAT) THEN
            ELEVTL = APARM(1)
            ELEVTH = APARM(2)
            FQD = -1
            IF (OPCODE.NE.'UFLG') OPCODE = 'FLAG'
            CALL FLAGEL (LUN, FGBUFF, IFGRNO, FGKOLS, FGNUMV, FSUBA(1),
     *         STARTD, STOPD, FQD, FGOPEN, ELEVTL, ELEVTH, IRET)
            IF (IRET.GT.0) GO TO 999
C                                       Flagging on shadowing
         ELSE IF (SHADOW) THEN
            FQD = -1
            IF (OPCODE.NE.'UFLG') OPCODE = 'FLAG'
            SHMIN = APARM(5) * CATD(KDCRV+JLOCF) / VELITE
            CTMIN = APARM(6) * CATD(KDCRV+JLOCF) / VELITE
            CALL FLAGSH (LUN, FGBUFF, IFGRNO, FGKOLS, FGNUMV, FSUBA(1),
     *         STARTD, STOPD, FQD, FGOPEN, SHMIN, CTMIN, IRET)
            IF (IRET.GT.0) GO TO 999
C                                       PCAL tones
         ELSE IF (PULCAL) THEN
            IF (OPCODE.NE.'UFLG') OPCODE = 'FLAG'
            CALL FLGPCL (LUN, FGBUFF, IFGRNO, FGKOLS, FGNUMV, FACTOR,
     *         SUBA, WSBLNK, STARTD, STOPD, KBIF, KEIF, KBCH, KECH,
     *         FGOPEN, IRET)
            IF (IRET.GT.0) GO TO 999
C
         ELSE
            CALL FLAGUP (OPCODE, LUN, DISK, CNOIN, FGVER, FGBUFF,
     *         IFGRNO, FGKOLS, FGNUMV, ID, NID, FSUBA(1), FFQID(1),
     *         NBASE, XA1, XA2, FLAGT1(1), FLAGT2(1), KBIF, KEIF, KBCH,
     *         KECH, STOKES, REASON, WFLAG, IRET)
            IF (IRET.GT.0) GO TO 999
            FGOPEN = .TRUE.
            END IF
C                                       Close FG table
         IF (FGOPEN) THEN
            CALL FLAGUP ('CLOS', LUN, DISK, CNOIN, FGVER, FGBUFF,
     *         IFGRNO, FGKOLS, FGNUMV, ID, NID, FSUBA(1), FFQID(1),
     *         NBASE, XA1, XA2, FLAGT1(1), FLAGT2(1), KBIF, KEIF, KBCH,
     *         KECH, STOKES, REASON, I, IRET)
            IF (WFLAG.LE.0) THEN
               WRITE (MSGTXT,1211) FGVER
               CALL MSGWRT (7)
            ELSE
               WRITE (MSGTXT,1210) WFLAG, FGVER
               CALL MSGWRT (4)
               END IF
C                                       Nothing was added to the FG
C                                       table
         ELSE
            WRITE (MSGTXT,1215) FGVER
            CALL MSGWRT (7)
            END IF
C                                       Fill /FLAGS/ common,
C                                       Applying channel range?
         IF ((WFLAG.GT.0) .AND. (.NOT.ELEVAT) .AND. (.NOT.PULCAL)) THEN
            IF (KECH.LT.KBCH) KECH = CATBLK(KINAX+JLOCF)
            KECH = MAX (1, MIN (KECH, CATBLK(KINAX+JLOCF)))
C                                       loop over baseline.
            DO 300 I = 1,NBASE
C                                       use input timerange only
               T1(1) = FLAGT1(1)
               T2(1) = FLAGT2(1)
               NTIM = 1
C                                       Loop over all timeranges to
C                                       be flagged for this baseline.
               DO 280 K = 1,NTIM
                  IF (NFLAG.GE.MXFLAG) THEN
                     WRITE (MSGTXT,1200) MXFLAG
                     CALL MSGWRT (6)
                     IRET = -1
                     GO TO 999
                     END IF
                  NFLAG = NFLAG + 1
                  FCOUNT(NFLAG) = 0
                  GCOUNT(NFLAG) = 0
                  FANT1(NFLAG) = MIN (XA1(I), XA2(I))
                  FANT2(NFLAG) = MAX (XA1(I), XA2(I))
                  REESON(NFLAG) = REASON
                  CORR = T
C                                       Antenna flagging
                  IF (FANT1(NFLAG).EQ.0) THEN
                     CORR = F
                     FANT1(NFLAG) = FANT2(NFLAG)
                     FANT2(NFLAG) = 0
                     END IF
C                                       Make sure FANT1.LE.FANT2
                  IF (CORR) THEN
                     IF (FANT1(NFLAG).GE.FANT2(NFLAG)) THEN
                        II = FANT1(NFLAG)
                        FANT1(NFLAG) = FANT2(NFLAG)
                        FANT2(NFLAG) = II
                        END IF
                     END IF
C                                       Timerange
                  FLAGT1(NFLAG) = T1(K)
                  FLAGT2(NFLAG) = T2(K)
C                                       Channel and IF limits
                  FCHLIM(1,NFLAG) = KBCH
                  FCHLIM(2,NFLAG) = KECH
                  FIFLIM(1,NFLAG) = KBIF
                  FIFLIM(2,NFLAG) = KEIF
                  FSUBA(NFLAG) = IROUND (XSUBAR)
                  FFQID(NFLAG) = FRQSEL
C                                       Get flagging parms
                  IF (STOKES.EQ.'----') THEN
                     CALL FLGSTK ('    ', OPCODE, FFLAGS(1,NFLAG), FSET,
     *                  IRET)
                  ELSE
                     CALL FLGSTK (STOKES, OPCODE, FFLAGS(1,NFLAG), FSET,
     *                  IRET)
                     END IF
                  IF (IRET.NE.0) GO TO 999
 280              CONTINUE
 300           CONTINUE
            END IF
         END IF
      GO TO 999
C                                       adverb error
 900  WRITE (MSGTXT,1900) KBIF, KEIF
      IRET = 10
      GO TO 990
 910  WRITE (MSGTXT,1910) KBCH, KECH
      IRET = 10
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR:',I7,'GETTING INPUT PARAMETERS')
 1010 FORMAT ('ERROR:',I7,' FINDING UV FILE')
 1020 FORMAT ('UVFGIN: UNABLE TO DETERMINE DATA START/STOP TIME')
 1030 FORMAT ('ERROR:',I3,' READING ANTENNA TABLE')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1200 FORMAT ('WARNING: USING ONLY FIRST ',I5,' ENTRIES FROM TABLE')
 1210 FORMAT ('Wrote ',I8,' flags to flag table version',I3)
 1211 FORMAT ('WARNING: DID NOT WRITE/CHANGE ANY RECORDS IN FG VERSION',
     *   I4)
 1215 FORMAT ('NOTHING WAS ADDED TO FG TABLE ',I3)
 1220 FORMAT ('UVFGIN: Beginning IF=',I2,' bigger than number of IF=',
     *   I2)
 1240 FORMAT ('UVFGIN: Beginning CH=',I3,' bigger than number of CH=',
     *   I3)
 1900 FORMAT ('UVFGIN: SPECIFIED IFS',2I4,' IMPROPER')
 1910 FORMAT ('UVFGIN: SPECIFIED CHANNELS',2I7,' IMPROPER')
      END
      SUBROUTINE UVFGHS
C-----------------------------------------------------------------------
C   UVFGHS writes the flagging information in the history file
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, CTIME*8, CDATE*12
      INTEGER   IFLAG, IRET, LUNH, DATE(3), TIME(3), J, JTRIM
      LOGICAL   T
      INCLUDE 'UVFLG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA T /.TRUE./
      DATA LUNH /27/
C-----------------------------------------------------------------------
C                                       Open history file
      CALL HIINIT (2)
      CALL HIOPEN (LUNH, DISK, FCNO(1), IBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Write time and date on old file
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME, CDATE)
      WRITE (HILINE,1000) TSKNAM, RLSNAM, CDATE, CTIME
      CALL HIADD (LUNH, HILINE, IBUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       opcode
      WRITE (HILINE,1004) TSKNAM, OPCODE
      CALL HIADD (LUNH, HILINE, IBUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Text file name.
      IF (INFILE(1:20).NE.' ') THEN
         WRITE (HILINE,1005) TSKNAM, INFILE
         CALL HIADD (LUNH, HILINE, IBUFF, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
C                                       elevation
      IF (ELEVAT) THEN
         WRITE (HILINE,1010) TSKNAM, APARM(1), APARM(2)
         CALL HIADD (LUNH, HILINE, IBUFF, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
      IF (PULCAL) THEN
         WRITE (HILINE,1011) TSKNAM, APARM(3)
         CALL HIADD (LUNH, HILINE, IBUFF, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
      IF (SHADOW) THEN
         WRITE (HILINE,1012) TSKNAM, APARM(5), APARM(6)
         CALL HIADD (LUNH, HILINE, IBUFF, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
C                                       Loop thru flags.
      IF (DOHIST.GT.0.0) THEN
         DO 50 IFLAG = 1,NFLAG
C                                       Spectral channel & IF
            IF ((IFLAG.EQ.1) .OR. (FCHLIM(1,IFLAG).NE.FCHLIM(1,IFLAG-1))
     *         .OR. (FCHLIM(2,IFLAG).NE.FCHLIM(2,IFLAG-1)) .OR.
     *         (FIFLIM(1,IFLAG).NE.FIFLIM(1,IFLAG-1)) .OR.
     *         (FIFLIM(2,IFLAG).NE.FIFLIM(2,IFLAG-1))) THEN
               WRITE (HILINE,1015) TSKNAM, IFLAG, FCHLIM(1,IFLAG),
     *            FCHLIM(2,IFLAG), FIFLIM(1,IFLAG), FIFLIM(2,IFLAG)
               CALL HIADD (LUNH, HILINE, IBUFF, IRET)
               IF (IRET.NE.0) GO TO 100
               END IF
C                                       Get time in user units.
            IF ((IFLAG.EQ.1) .OR. (FLAGT1(IFLAG).NE.FLAGT1(IFLAG-1))
     *         .OR. (FLAGT2(IFLAG).NE.FLAGT2(IFLAG-1))) THEN
               CALL FGTIME (IFLAG, FLAGT1(IFLAG), FLAGT2(IFLAG), LUNH,
     *            IBUFF, IRET)
               IF (IRET.NE.0) GO TO 100
               END IF
C                                       Time and Flags
C                                       Trap antenna based flaging.
            IF ((IFLAG.EQ.1) .OR. (FANT1(IFLAG).NE.FANT1(IFLAG-1))
     *         .OR. (FANT2(IFLAG).NE.FANT2(IFLAG-1))) THEN
               IF (FANT2(IFLAG).EQ.0) THEN
                  WRITE (HILINE,1021) TSKNAM, IFLAG, FANT1(IFLAG),
     *               FFLAGS(1,IFLAG),  FFLAGS(2,IFLAG)
               ELSE
                  WRITE (HILINE,1020) TSKNAM, IFLAG, FANT1(IFLAG),
     *               FANT2(IFLAG), FFLAGS(1,IFLAG), FFLAGS(2,IFLAG),
     *               FFLAGS(3,IFLAG), FFLAGS(4,IFLAG)
                  END IF
               CALL HIADD (LUNH, HILINE, IBUFF, IRET)
               IF (IRET.NE.0) GO TO 100
               END IF
C                                       reason
            J = JTRIM (REESON(IFLAG))
            IF ((IFLAG.EQ.1) .OR. (REESON(IFLAG).NE.REESON(IFLAG-1)))
     *         THEN
               IF (REESON(IFLAG).NE.' ') THEN
                  WRITE (HILINE,1025) TSKNAM, IFLAG, REESON(IFLAG)
                  CALL HIADD (LUNH, HILINE, IBUFF, IRET)
                  IF (IRET.NE.0) GO TO 100
                  END IF
               END IF
 50         CONTINUE
         END IF
      WRITE (HILINE,1030) TSKNAM, FGVER
      CALL HIADD (LUNH, HILINE, IBUFF, IRET)
      IF (IRET.NE.0) GO TO 100
      WRITE (HILINE,1031) TSKNAM, WFLAG
      CALL HIADD (LUNH, HILINE, IBUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C
 100  CALL HICLOS (LUNH, T, IBUFF, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',A12,2X,A8)
 1004 FORMAT (A6,'OPCODE = ''',A,'''  / operation performed by UVFLG')
 1005 FORMAT (A6,'INFILE= ''',A,'''')
 1010 FORMAT (A6,'ELRANGE=',F6.1,' ,',F6.1,'  / flag elevation range')
 1011 FORMAT (A6,'PCSUPFAC=',F8.3,'  / Pulse cal suppression factor')
 1012 FORMAT (A6,'MINBASLN=',F6.1,' ,',F6.1,'  / min baseline',
     *   ' shadow, crosstalk')
 1015 FORMAT (A6,'/',I4,' BCHAN    =',I5,' ECHAN =',I5,'  BIF =',I3,
     *   ' EIF =',I3)
 1020 FORMAT (A6,'/',I4,' BASELINE =',I3,' -',I3,'  FLAGS = ',4L1)
 1021 FORMAT (A6,'/',I4,' BASELINE =',I3,' -  *  FLAGS = ',4L1)
 1025 FORMAT (A6,'/',I4,' REASON   =''',A,'''')
 1030 FORMAT (A6,'OUTFGVER=',I5,'  / output FG table version')
 1031 FORMAT (A6,'NFLAG =',I9,'   / number flag commands written')
      END
      SUBROUTINE FGTIME (IFLAG, TIME1, TIME2, HLUN, HBUFF, IERR)
C-----------------------------------------------------------------------
C   FGTIME interprets a timerange to the history file.
C   Inputs:
C      IFLAG   I        Flag number
C      TIME1   R        Start time in days: <= 0 => beginning
C      TIME2   R        Stop time in days: >= 1.0e4 => to end
C      HLUN    I        LUN of open history file
C   In/out:
C      HBUFF   I(256)   History writing buffer
C   Output:
C      IERR    I        Error code of HIADD : 0 => okay
C-----------------------------------------------------------------------
      REAL      TIME1, TIME2
      INTEGER   IFLAG, HLUN, HBUFF(256), IERR
C
      REAL      T1, T2
      INTEGER   IT1(3), IT2(3)
      CHARACTER HILINE*72
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       convert times
      IF (TIME1.GT.0.0) THEN
         T1 = TIME1
         IT1(1) = T1
         T1 = 24.0 * (T1 - IT1(1))
         IT1(2) = T1
         T1 = 60.0 * (T1 - IT1(2))
         IT1(3) = T1
         T1 = 60.0 * (T1 - IT1(3))
         END IF
      IF (TIME2.LT.1.0E4) THEN
         T2 = TIME2
         IT2(1) = T2
         T2 = 24.0 * (T2 - IT2(1))
         IT2(2) = T2
         T2 = 60.0 * (T2 - IT2(2))
         IT2(3) = T2
         T2 = 60.0 * (T2 - IT2(3))
         END IF
C                                       Man-readable form
      IF (TIME1.LE.0.0) THEN
         IF (TIME2.GE.9998.) THEN
            WRITE (HILINE,1000) TSKNAM, IFLAG
         ELSE
            WRITE (HILINE,1010) TSKNAM, IFLAG, IT2, T2
            IF (HILINE(48:48).EQ.' ') HILINE(48:48) = '0'
            IF (HILINE(49:49).EQ.' ') HILINE(49:49) = '0'
            END IF
      ELSE
         IF (TIME2.GE.9998.) THEN
            WRITE (HILINE,1020) TSKNAM, IFLAG, IT1, T1
            IF (HILINE(35:35).EQ.' ') HILINE(35:35) = '0'
            IF (HILINE(36:36).EQ.' ') HILINE(36:36) = '0'
         ELSE
            WRITE (HILINE,1030) TSKNAM, IFLAG, IT1, T1, IT2, T2
            IF (HILINE(35:35).EQ.' ') HILINE(35:35) = '0'
            IF (HILINE(36:36).EQ.' ') HILINE(36:36) = '0'
            IF (HILINE(55:55).EQ.' ') HILINE(55:55) = '0'
            IF (HILINE(56:56).EQ.' ') HILINE(56:56) = '0'
            END IF
         END IF
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'/',I4,' TIMERANG = beginning to end')
 1010 FORMAT (A6,'/',I4,' TIMERANG = beginning to',I5,'/',2(I2.2,':'),
     *   F5.2)
 1020 FORMAT (A6,'/',I4,' TIMERANG =',I5,'/',2(I2.2,':'),F5.2,' to end')
 1030 FORMAT (A6,'/',I4,' TIMERANG =',I5,'/',2(I2.2,':'),F5.2,' to',
     *   I5,'/',2(I2.2,':'),F5.2)
      END
      SUBROUTINE UVFTRD (FILE, IRET)
C-----------------------------------------------------------------------
C   UVFTRD reads a text file in containing flagging information using
C   KEYIN.
C   same meaning as the CPARM array passed from AIPS;
C   Values are placed in common /FLAGS/ and/or written to the flagging
C   file.
C    Inputs:
C      FILE     C*48 Name of text file
C    Output: IRET     I    Return code, 0=>OK otherwise failed.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMK
      PARAMETER (NUMK=185)
C
      CHARACTER  FILE*48, KEYS(NUMK)*8, ENDMRK*8, VALCH(NUMK)*8,
     *   CHSOUR*8, CHSUBA*8, CHTIME*8, CHBCH*8, CHECH*8, CHBIF*8,
     *   CHEIF*8, CHANT*8, CHBASE*8, CHSTOK*8, CHOPCO*8, CHREA*8,
     *   CHTMOF*8, CHFQID*8, OBSDAT*8, ANTNAM*8, STANAM(MAXANT)*8,
     *   CHTMR*8, CHBFRQ*8, CHEFRQ*8
      LOGICAL   T, F, WARN
      INTEGER   IRET, LUN, FIND, MODE, I, N, NUMKEY, IFGRNO, OFSOUR,
     *   OFSUBA, OFTIME, OFBCH, OFECH, OFBIF, OFEIF, OFANT, OFBASE,
     *   OFSTOK, OFOPCO, OFREA, OTMOF, OFQID, IDUM1,
     *   IDUM2, IDUM3, LIM2, IDATE(3), DAYN, STRLEN, STALEN(MAXANT),
     *   ODTMR, OFBFRQ, OFEFRQ
      DOUBLE PRECISION VALUE(NUMK)
      REAL      TIMOFF, RDAYN, DTMR
      INCLUDE 'UVFLG.INC'
      INTEGER   FGKOLS(MAXFGC), FGNUMV(MAXFGC)
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN  /10/
      DATA ENDMRK /'/       '/
C                                       Set KEYWORDS (ADVERBS)
      DATA    CHSOUR,     CHSUBA,     CHTIME
     *   /'SOURCES ', 'SUBARRAY', 'TIMERANG'/
      DATA   CHBCH,     CHECH,      CHBIF,    CHEIF
     *   /'BCHAN   ', 'ECHAN   ', 'BIF     ', 'EIF     '/
      DATA     CHANT,     CHBASE,     CHSTOK,    CHOPCO
     *   /'ANT?????', 'BAS?????', 'STOKES  ', 'OPCODE  '/
      DATA CHREA ,     CHTMOF,    CHBFRQ,   CHEFRQ
     *   /'REASON  ', 'TIMEOFF', 'BFREQ', 'EFREQ'/
      DATA CHFQID,     CHTMR
     *   /'FREQID  ', 'DTIMRANG'/
C                                       Set pointers in KEYIN arrays
      DATA NUMKEY /183/
      DATA  OFSOUR, OFSUBA, OFTIME, OFBCH, OFECH, OFBIF, OFEIF
     *   /    1,      61,     62,     70,   71,     72,    73/
      DATA OFANT, OFBASE, OFSTOK, OFOPCO, OFREA, OTMOF, OFQID, ODTMR
     *   /  74,    124,     174,   175,    176,    179,  180,  181/
      DATA OFBFRQ, OFEFRQ
     *   / 182,     183/
C-----------------------------------------------------------------------
      CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
      CALL DATEST (OBSDAT, IDATE)
      CALL DAYNUM (IDATE(1), IDATE(3), IDATE(2), DAYN)
      RDAYN = DAYN
C                                       Setup Antenna lists
      DO 10 I = 1,NSTNS
         CALL CHTRIM (STNNAM(I), 8, STANAM(I), STALEN(I))
 10      CONTINUE
C                                       Fill KEYWORD arrays.
      LIM2 = OFSUBA - 1
      DO 20 I = OFSOUR,LIM2,2
         KEYS(I) = CHSOUR
         KEYS(I+1) = '        '
 20      CONTINUE
      KEYS(OFSUBA) = CHSUBA
      KEYS(OFQID) = CHFQID
      LIM2 =  OFBCH - 1
      DO 30 I = OFTIME,LIM2
         KEYS(I) = CHTIME
 30      CONTINUE
      KEYS(OFBCH) = CHBCH
      KEYS(OFECH) = CHECH
      KEYS(OFBIF) = CHBIF
      KEYS(OFEIF) = CHEIF
      LIM2 = OFBASE - 1
      DO 40 I = OFANT,LIM2
         KEYS(I) = CHANT
         VALUE(I) = 0.0D0
         VALCH(I) = ' '
 40      CONTINUE
      LIM2 = OFSTOK - 1
      DO 50 I = OFBASE,LIM2
         KEYS(I) = CHBASE
         VALUE(I) = 0.0D0
         VALCH(I) = ' '
 50      CONTINUE
      KEYS(OFSTOK) = CHSTOK
      KEYS(OFOPCO) = CHOPCO
      KEYS(OFREA) = CHREA
      KEYS(OFREA+1) = '        '
      KEYS(OFREA+2) = '        '
      KEYS(OTMOF) = CHTMOF
      KEYS(ODTMR) = CHTMR
      KEYS(OFBFRQ) = CHBFRQ
      KEYS(OFEFRQ) = CHEFRQ
      VALUE(OFSUBA) = 0.0D0
      VALUE(OFQID) = -1.0D0
      VALUE(OFBCH) = 0.0D0
      VALUE(OFECH) = 0.0D0
      VALUE(OFBIF) = 0.0D0
      VALUE(OFEIF) = 0.0D0
      VALUE(OTMOF) = 0.0D0
      VALUE(ODTMR) = 0.0D0
      VALUE(OFBFRQ) = 0.0D0
      VALUE(OFEFRQ) = 0.0D0
      TIMOFF = VALUE(OTMOF)
      DTMR = VALUE(ODTMR)
      IRET = 0
      NFLAG = 0
C                                       Set KEYIN mode to read list.
      MODE = 0
      WARN = T
C                                       Open edit file
      CALL ZTXOPN ('READ', LUN, FIND, FILE, F, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 990
         END IF
C                                       Loop over flagging entries
 510     N = NUMKEY
C                                       Zero entries
         LIM2 = OFSUBA - 1
         DO 520 I = 1,N
            VALCH(I) = ' '
 520        CONTINUE
         DO 530 I = 1,N
            VALUE(I) = 0.0D0
 530        CONTINUE
C                                       Init FQ ID
         VALUE(OFQID) = -1.0D0
C                                       Read entry
         CALL KEYIN (KEYS, VALUE, VALCH, N, ENDMRK, MODE, LUN, FIND,
     *      IRET)
C                                       Check for read error.
         IF (IRET.LE.1) GO TO 560
            WRITE (MSGTXT,1510) IRET
            GO TO 990
C                                       Check for end
 560     IF (IRET.EQ.1) GO TO 700
C                                       Save IAT correction
         IF (VALUE(OTMOF).NE.0.0D0) TIMOFF = VALUE(OTMOF)
C                                       Save TIMERANG correction
         IF (VALUE(ODTMR).NE.0.0D0) DTMR = VALUE(ODTMR)
         RDAYN = DAYN
         IF (VALCH(OFANT).EQ.' ') RDAYN = 0.0
C                                       Correct times to IAT
         VALUE(OFTIME+3) = VALUE(OFTIME+3) + TIMOFF - DTMR
         VALUE(OFTIME+7) = VALUE(OFTIME+7) + TIMOFF + DTMR
         VALUE(OFTIME)   = VALUE(OFTIME) - RDAYN
         VALUE(OFTIME+4) = VALUE(OFTIME+4) - RDAYN
C
         IF (VALCH(OFANT)(1:1).NE.' ') THEN
C                                       Determine antenna numbers
C                                       from antenna names.
            CALL CHLTOU (8, VALCH(OFANT))
            CALL CHTRIM (VALCH(OFANT), 8, ANTNAM, STRLEN)
            DO 600 I = 1, NSTNS
C              SRCHLN = MIN (STRLEN, STALEN(I))
               IF (ANTNAM.EQ.STANAM(I)) THEN
                  VALUE(OFANT) = TELNO(I)
                  GO TO 610
                  END IF
  600          CONTINUE
            GO TO 510
C                                       Determine baseline numbers
C                                       from baseline names.
  610       IF (VALCH(OFBASE)(1:1).NE.' ') THEN
               CALL CHLTOU (8, VALCH(OFBASE))
               CALL CHTRIM (VALCH(OFBASE), 8, ANTNAM, STRLEN)
               DO 620 I = 1, NSTNS
C                 SRCHLN = MIN (STRLEN, STALEN(I))
                  IF (ANTNAM.EQ.STANAM(I)) THEN
                     VALUE(OFBASE) = TELNO(I)
                     GO TO 650
                     END IF
  620             CONTINUE
               GO TO 510
               END IF
            END IF
C                                       Write table or fill common
  650    CALL UVFTAB (VALUE, VALCH, IBUFF, IFGRNO, FGKOLS, FGNUMV,
     *      OFSOUR, OFSUBA, OFQID, OFTIME, OFBCH, OFECH, OFBIF, OFEIF,
     *      OFANT, OFBASE, OFSTOK, OFOPCO, OFREA, OFBFRQ, RDAYN, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       Get next record.
         IF (IRET.LE.0) GO TO 510
C                                       All done, close text file.
 700  CALL ZTXCLS (LUN, FIND, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Close FG file.
      CALL FLAGUP ('CLOS', LUN, DISK, CNOIN, FGVER, IBUFF, IFGRNO,
     *   FGKOLS, FGNUMV, ID, NID, FSUBA(1), FFQID(1), IDUM1, IDUM2,
     *   IDUM3, FLAGT1(1), FLAGT2(1), FIFLIM(1,1), FIFLIM(2,1),
     *   FCHLIM(1,1), FCHLIM(2,1), STOKES, REASON, I, IRET)
      IF (IRET.GT.0) GO TO 999
C                                       Make sure got some.
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('ERROR',I3,' OPENING EDIT TABLE')
 1510 FORMAT ('ERROR',I3,' READING EDIT TABLE')
      END
      SUBROUTINE UVFTAB (VALUE, VALCH, BUFFER, IFGRNO, FGKOLS, FGNUMV,
     *   OFSOUR, OFSUBA, OFQID, OFTIME, OFBCH, OFECH, OFBIF, OFEIF,
     *   OFANT, OFBASE, OFSTOK, OFOPCO, OFREA, OFBFRQ, RDAYN, IRET)
C-----------------------------------------------------------------------
C   Gets flagging info from KEYIN array and writes to FG file via FLAGUP
C   or squirrels it away in the common /FLAGS/.
C   NOTE: Uses LUN 29 for I/O to FG file.  File must be closed after
C   last call to UVFTAB with a CLOSE call to FLAGUP.
C   Input:
C     VALUE(*)     D   Numeric value array returned from KEYIN.
C     VALCH(*)     C*8 Character array returned from KEYIN.
C     OFSOUR       I   Offset in VALCH of source name (30*16 char, 2 el)
C     OFSUBA       I   Offset in VALUE of subarray no. 0=>all.
C     OFQID        I   Offset in VALUE of freq. id no. <=0 => all.
C     OFTIME       I   Offset in VALUE of TIMERANGE (8 reals)
C     OFBCH        I   Offset in VALUE of BCHAN (1 integer)
C     OFECH        I   Offset in VALUE of ECHAN (1 integer)
C     OFBIF        I   Offset in VALUE of BIF (1 integer)
C     OFEIF        I   Offset in VALUE of EIF (1 integer)
C     OFANT        I   Offset in VALUE of ANTENNA array (50 reals)
C     OFBAS        I   Offset in VALUE of BASELINE array (50 reals)
C     OFSTOK       I   Offset in VALCH of Stokes' parm. 4 char.
C     OFOPCO       I   Offset in VALCH of OPCODE 4 char.
C     OFREA        I   Offset in VALCH of REASON (24 char. 3 el.)
C     OFBFRQ       I   Offset in VALCH of BFREQ/EFREQ (2 reals)
C     RDAYN        R   Day #: for error reporting purposes only.
C   Input from common
C     DISK         I   Disk to use.
C     CNOIN        I   Catalog slot number
C     FGVER        I   FG file version
C     CATBLK(256)  I   Catalog header block.
C     NUMAN(513)   I   (1) = No. subarrays, following elements are
C                       the number of antennas in each subarray.
C   Input/Output:
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open.
C     IFGRNO       I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     FGKOLS(*)    I   The column pointer array in order, SOURCE,
C                      SUBARRAY, ANTS, TIMERANG, IFS, CHANS, PFLAGS,
C                      REASON
C     FGNUMV(*)    I   Element count in each column.
C   Input/output from COMMON /FLAGS/
C     NFLAG        I   Number of flagging criteria in common.
C   Output:
C     IRET         I   Error code, 0=>OK else TABIO error.
C   Output to common /FLAGS/:
C     FANT1(*)     I   First antenna of pair, 0=>all
C     FANT2(*)     I   Second antenna of pair, 0=>all
C     FFLAGS(4,*)  L   Correlator/polarization flags.
C     FLAGT1(*)    R   Start time (days since reference day in CATBLK)
C     FLAGT2(*)    R   End time.
C     FVS1(*)      I   First vis. to consider flagging
C     FVS2(*)      I   Last vis. to consider flagging
C     FLGIF(*)     I   Correlator flag 0=>flag R&L,1=>R,2=>L
C                      -1=>unflag R, -2=> L, -3 => both.
C     FIFLIM(2,*)  I   First and last IF's selected 0=all
C     FCHLIM(2,*)  I   First and last channels selected 0=all
C     FSUBA(*)     I   Subarray to flag
C     FFQID(*)     I   FQ ID to flag
C-----------------------------------------------------------------------
      DOUBLE PRECISION    VALUE(*)
      CHARACTER VALCH(*)*8
      INTEGER   BUFFER(*), IFGRNO, FGKOLS(*), FGNUMV(*), OFSOUR, OFSUBA,
     *   OFQID, OFTIME, OFBCH, OFECH, OFBIF, OFEIF, OFANT, OFBASE,
     *   OFSTOK, OFOPCO, OFREA, OFBFRQ, IRET
      REAL      RDAYN
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER XOPCO*4, XSTOK*4, XEASON*24, XOURCE(30)*16
      INTEGER   LUN, NFL, KSUBA, KBCH(MAXIF), KECH(MAXIF), KBIF(MAXIF),
     *   KEIF(MAXIF), I, JOFF, NSOUR, XA1(MXBASE), XA2(MXBASE), NBASE,
     *   MAXBAS, NOCHAN, NOIF, KFQID, LUNFQ, IROUND, TIM1(4), TIM2(4),
     *   NFRQS, J
      REAL      XNTENN(50), XASELN(50), XT1, XT2, RTEMP, YT1, YT2,
     *   SEC1, SEC2
      INCLUDE 'UVFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN /29/
      DATA MAXBAS /MXBASE/
C-----------------------------------------------------------------------
C                                       Crunch numbers from VALUE
C                                       SOURCES
      DO 20 I = 1,30
         JOFF = OFSOUR + (I-1) * 2
         XOURCE(I) = VALCH(JOFF) // VALCH(JOFF+1)
 20      CONTINUE
C                                       Look up ID's
      NSOUR = 30
      IF (MULTI) THEN
         NID = MXSOU
         CALL SOURNU (XOURCE, KUAL, CALKOD, NSOUR, DISK, CNOIN, NID,
     *      IBUFF2, ID, IRET)
         IF (IRET.LT.0) THEN
            MSGTXT = 'SOURCE(S) NOT FOUND IN SU TABLE'
            CALL MSGWRT (7)
            IRET = 5
            END IF
         IF (IRET.NE.0) GO TO 999
         IF (NID.EQ.0) NID = 1
      ELSE
         NID = 1
         ID(1) = 0
         END IF
C                                       Subarray
      KSUBA = VALUE(OFSUBA) + 0.1
      RTEMP = VALUE(OFQID)
      KFQID = IROUND (RTEMP)
      IF (KFQID.LT.0) KFQID = -1
      IF (KFQID.GE.0) THEN
         LUNFQ = 40
         CALL FQMATC (DISK, CNOIN, CATBLK, LUNFQ, SELBAN, SELFRQ,
     *      MATCH, KFQID, IRET)
         IF (.NOT.MATCH) THEN
            WRITE (MSGTXT,1080)
            IRET = 1
            GO TO 990
            END IF
         IF (IRET.GT.0) GO TO 999
         END IF
C                                       Timerange
      XT1 = VALUE(OFTIME) + VALUE(OFTIME+1) / 24. +
     *   VALUE(OFTIME+2) / (24. * 60.) + VALUE(OFTIME+3) / (24.*60.*60.)
      XT2 = VALUE(OFTIME+4) + VALUE(OFTIME+5) / 24. +
     *   VALUE(OFTIME+6) / (24. * 60.) + VALUE(OFTIME+7) / (24.*60.*60.)
C                                       If both 0 flag all.
      IF (ABS(XT2).LE.1.0E-5) XT2 = XT1
      IF ((ABS(XT1).LE.1.0E-5) .AND. (ABS(XT2).LE.1.0E-5)) XT2 = 9999.
      IF ((ABS(XT1+RDAYN).LE.1.0E-5) .AND. (ABS(XT2+RDAYN).LE.1.0E-5))
     *   THEN
         XT1 = 0.0
         XT2 = 9999
         RDAYN = 0
         END IF
C                                       Tweak range a bit (0.5 sec).
      IF (ABS(XT2-XT1).LT.1.E-6) THEN
         XT1 = XT1 - 1.0 / 172800.
         XT2 = XT2 + 1.0 / 172800.
         END IF
C                                       Check that start before finish.
      IF (XT1.GT.XT2) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (6)
         YT1 = RDAYN + XT1 + 1.0 / 172800.
         YT2 = RDAYN + XT2 - 1.0 / 172800.
         GO TO 990
         END IF
C                                       Channel and IF limits
      NFRQS = 1
      KBCH(1)  = VALUE(OFBCH) + 0.1
      KECH(1)  = VALUE(OFECH) + 0.1
      KBIF(1)  = VALUE(OFBIF) + 0.1
      KEIF(1)  = VALUE(OFEIF) + 0.1
      IF ((VALUE(OFBFRQ).GT.0.0D0) .AND. (VALUE(OFBFRQ+1).GT.0.0D0))
     *   THEN
         CALL FNDFRS (KFQID, VALUE(OFBFRQ), KBCH, KECH, KBIF, KEIF,
     *      NFRQS, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Make sure agree with CATBLK
      ELSE
         NOCHAN = CATBLK(KINAX+JLOCF)
         NOIF = 1
         IF (JLOCIF.GT.1) NOIF = CATBLK(KINAX+JLOCIF)
         KBCH(1) = MAX (KBCH(1), 1)
         KBCH(1) = MIN (KBCH(1), NOCHAN)
C                                       Applying channel range?
         KBIF(1) = MAX (KBIF(1), 1)
         KBIF(1) = MIN (KBIF(1), NOIF)
         IF (KEIF(1).LE.0) KEIF(1) = NOIF
         KEIF(1) = MIN (KEIF(1), NOIF)
         END IF
C                                       Antenna and baseline arrays.
      DO 50 I = 1,50
         XNTENN(I) = VALUE(OFANT+I-1)
         XASELN(I) = VALUE(OFBASE+I-1)
 50      CONTINUE
C                                       Convert to baseline numbers.
      NBASE = MAXBAS
      CALL AN10RS (NUMAN, KSUBA, XNTENN, XASELN, NBASE, XA1, XA2)
C                                       Stokes' parameter
      XSTOK = VALCH(OFSTOK)(1:4)
C                                       OPCODE
      XOPCO = VALCH(OFOPCO)(1:4)
C                                       Find reason why.
      XEASON = VALCH(OFREA) // VALCH(OFREA+1) // VALCH(OFREA+2)
C                                       Now the secret's out - hide it
C                                       on disk where no one will ever
C                                       see it again.
      DO 60 J = 1,NFRQS
         CALL FLAGUP (XOPCO, LUN, DISK, CNOIN, FGVER, BUFFER, IFGRNO,
     *      FGKOLS, FGNUMV, ID, NID, KSUBA, KFQID, NBASE, XA1, XA2, XT1,
     *      XT2, KBIF(J), KEIF(J), KBCH(J), KECH(J), XSTOK, XEASON, NFL,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
 60      CONTINUE
      WFLAG = WFLAG + NFL
C                                       Fill /FLAGS/ common,
C                                       loop over frequencies
      DO 100 J = 1,NFRQS
C                                       loop over baseline.
         DO 90 I = 1,NBASE
            IF (NFLAG.GE.MXFLAG) THEN
               WRITE (MSGTXT,1070) MXFLAG
               CALL MSGWRT (6)
               IRET = -1
               GO TO 999
               END IF
            NFLAG = NFLAG + 1
            REESON(NFLAG) = XEASON
            FANT1(NFLAG) = MIN (XA1(I), XA2(I))
            FANT2(NFLAG) = MAX (XA1(I), XA2(I))
            IF (FANT1(NFLAG).EQ.0) THEN
              FANT1(NFLAG) = FANT2(NFLAG)
              FANT2(NFLAG) = 0
              END IF
            FLAGT1(NFLAG) = XT1
            FLAGT2(NFLAG) = XT2
            FIFLIM(1,NFLAG) = KBIF(J)
            FIFLIM(2,NFLAG) = KEIF(J)
            FCHLIM(1,NFLAG) = KBCH(J)
            FCHLIM(2,NFLAG) = KECH(J)
            FSUBA(NFLAG) = KSUBA
            FFQID(NFLAG) = KFQID
C                                       Get flagging parms
            CALL FLGSTK (XSTOK, XOPCO, FFLAGS(1,NFLAG), FSET, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Let's not keep the
C                                       **** "REASON WHY" ****
C                                       a secret, tell the world!!!
            IF ((NMESS.EQ.NECHO+1) .AND. (NECHO.GT.0)) THEN
               MSGTXT = 'Too many reasons, will suppress printing'
               CALL MSGWRT (6)
               END IF
            IF ((NMESS.EQ.0) .AND. (NECHO.GT.0)) THEN
               WRITE (MSGTXT,1030)
               CALL MSGWRT (4)
               END IF
            IF (NMESS.LE.NECHO) THEN
               YT1 = RDAYN + XT1 + 1.0 / 172800.
               YT2 = RDAYN + XT2 - 1.0 / 172800.
               CALL TODHMS (YT1, TIM1)
               CALL TODHMS (YT2, TIM2)
               SEC1 = VALUE(OFTIME+3)
               SEC2 = VALUE(OFTIME+7)
               WRITE (MSGTXT,1031) FANT1(NFLAG), FANT2(NFLAG), KBIF(J),
     *            KEIF(J), TIM1(1), TIM1(2), TIM1(3), SEC1, TIM2(1),
     *            TIM2(2), TIM2(3), SEC2, XEASON
               IF (MSGTXT(22:22).EQ.' ') MSGTXT(22:22) = '0'
               IF (MSGTXT(36:36).EQ.' ') MSGTXT(36:36) = '0'
               CALL MSGWRT (4)
               END IF
            NMESS = NMESS + 1
 90         CONTINUE
 100     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('ERROR: TIME1 > TIME2')
 1030 FORMAT (' ANTs  IFs ',4X,'Time1',9X,'Time2',6X,'Reason')
 1031 FORMAT (I2,2I3,'-',I2,2(I3.2,'/',I2.2,':',I2.2,':',F4.1),1X,A)
 1070 FORMAT ('WARNING: ONLY FIRST ',I5,' ENTRIES HAVE BEEN USED')
 1080 FORMAT ('UVFTAB:NO MATCH TO FQID IN DATA - CHECK VALUE IN TABLE')
      END
      SUBROUTINE FNDFRS (KFQID, VALUEF, KBCH, KECH, KBIF, KEIF, NFRQS,
     *   IRET)
C-----------------------------------------------------------------------
C   FNDFRQ finds the combinations of BCHAN, ECHAN BIF, EIF that match
C   the requested frequencies
C   Inputs:
C      KFQID    I      Frequency ID number
C      VALUEF   D(2)   Begin and end frequencies
C   In/out
C      KBCH     I(*)   Begin channel : must be zero on input
C      KECH     I(*)   End channel
C      KBIF     I(*)   Begin IF
C      KEIF     I(*)   End IF
C   Output:
C      NFRQS    I      Number of frequencies found: 0 is not an error
C      IRET     I      error
C-----------------------------------------------------------------------
      INTEGER   KFQID, KBCH(*), KECH(*), KBIF(*), KEIF(*), NFRQS, IRET
      DOUBLE PRECISION VALUEF(2)
C
      INCLUDE 'UVFLG.INC'
      INTEGER   VER, LUN, NIF, CURFQ, IFQ, NCHAN, I, IROUND
      REAL      RTEMP
      DOUBLE PRECISION FLIMIT(2,MAXIF)
      SAVE CURFQ, FLIMIT, NIF, NCHAN
      INCLUDE 'INCS:DCHND.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA CURFQ /-1000/
C-----------------------------------------------------------------------
C                                       get frequencies
      IFQ = MAX (1, KFQID)
      IF (CURFQ.NE.IFQ) THEN
         NCHAN = CATBLK(KINAX+JLOCF)
         CALL FNDEXT ('FQ', CATBLK, VER)
         FQCHND = IFQ
         LUN = 55
         CALL CHNDAT ('READ', IBUFF, DISK, CNOIN, VER, CATBLK, LUN, NIF,
     *      FOFF, ISBAND, FINC, BNDCOD, FQCHND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING FREQUENCY TABLE'
            GO TO 990
            END IF
         CURFQ = FQCHND
         DO 10 I = 1,NIF
            FLIMIT(1,I) = CATD(KDCRV+JLOCF) + FOFF(I) +
     *         FINC(I) * (0.5 - CATR(KRCRP+JLOCF))
            FLIMIT(2,I) = CATD(KDCRV+JLOCF) + FOFF(I) +
     *         FINC(I) * (NCHAN + 0.5 - CATR(KRCRP+JLOCF))
 10         CONTINUE
         END IF
C                                       check for overuse
      IF ((KBIF(1).GT.0) .OR. (KEIF(1).GT.0) .OR. (KBCH(1).GT.0) .OR.
     *   (KECH(1).GT.0)) THEN
         MSGTXT = 'BFREQ & EFREQ WERE SPECIFIED BUT ALSO'
         CALL MSGWRT (7)
         WRITE (MSGTXT,1010) KBIF(1), KEIF(1), KBCH(1), KECH(1)
         IRET = -1
         GO TO 990
         END IF
      IRET = 0
C                                       take a look
      NFRQS = 0
      VALUEF(1) = VALUEF(1) * 1.D6
      VALUEF(2) = VALUEF(2) * 1.D6
      DO 50 I = 1,NIF
         IF (FINC(I).GT.0.0) THEN
            IF ((VALUEF(1).LE.FLIMIT(2,I)) .AND.
     *         (VALUEF(2).GE.FLIMIT(1,I))) THEN
               NFRQS = NFRQS + 1
               KBIF(NFRQS) = I
               KEIF(NFRQS) = I
               IF (VALUEF(1).LT.FLIMIT(1,I)) THEN
                  KBCH(NFRQS) = 1
               ELSE
                  RTEMP = NCHAN * (VALUEF(1) - FLIMIT(1,I)) /
     *               (FLIMIT(2,I) - FLIMIT(1,I)) + 0.5
                  KBCH(NFRQS) = IROUND (RTEMP)
                  END IF
               IF (VALUEF(2).GE.FLIMIT(2,I)) THEN
                  KECH(NFRQS) = NCHAN
               ELSE
                  RTEMP = NCHAN * (VALUEF(2) - FLIMIT(1,I)) /
     *               (FLIMIT(2,I) - FLIMIT(1,I)) + 0.5
                  KECH(NFRQS) = IROUND (RTEMP)
                  END IF
               KBCH(NFRQS) = MAX (1, MIN (NCHAN, KBCH(NFRQS)))
               KECH(NFRQS) = MAX (1, MIN (NCHAN, KECH(NFRQS)))
               END IF
         ELSE
            IF ((VALUEF(1).LE.FLIMIT(1,I)) .AND.
     *         (VALUEF(2).GE.FLIMIT(2,I))) THEN
               NFRQS = NFRQS + 1
               KBIF(NFRQS) = I
               KEIF(NFRQS) = I
               IF (VALUEF(1).LT.FLIMIT(2,I)) THEN
                  KBCH(NFRQS) = NCHAN
               ELSE
                  RTEMP = NCHAN + 0.5 - NCHAN * (VALUEF(1)-FLIMIT(2,I))
     *               / (FLIMIT(1,I) - FLIMIT(2,I))
                  KBCH(NFRQS) = IROUND (RTEMP)
                  END IF
               IF (VALUEF(2).GE.FLIMIT(1,I)) THEN
                  KECH(NFRQS) = 1
               ELSE
                  RTEMP = NCHAN + 0.5 - NCHAN * (VALUEF(2)-FLIMIT(2,I))
     *               / (FLIMIT(1,I) - FLIMIT(2,I))
                  KECH(NFRQS) = IROUND (RTEMP)
                  END IF
               KBCH(NFRQS) = MAX (1, MIN (NCHAN, KBCH(NFRQS)))
               KECH(NFRQS) = MAX (1, MIN (NCHAN, KECH(NFRQS)))
               END IF
            END IF
 50      CONTINUE
C
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FNDFRS ERROR',I4,' ON ',A)
 1010 FORMAT ('BIF=',I3,' EIF=',I3,' BCHAN=',I7,' ECHAN=',I7)
      END
      SUBROUTINE ALLSOU (DISK, CNO, CAT, CALKOD, KUAL, BUFFER, ID, NID,
     *   IRET)
C-----------------------------------------------------------------------
C   Load all source identifiers in the SU table matching calcode and
C   qual
C   Inputs:
C      DISK      I       Disk number of the data set.
C      CNO       I       Catalog slot number of data set.
C      CAT       I(256)  Catalog header.
C   Input/Output:
C      BUFFER    I(512)  Work buffer, used for I/O and manipulating
C                        source lists, should be at least min (512,NID)
C   Output:
C      ID        I(*)    Source ID numbers of selected sources,
C      NID       I       Number of elements returned in ID.
C      IRET      I       Return code. 0 => OK; else failed.
C   Usage notes:
C       This routine uses AIPS LUN 27 which will be closed on normal
C       return. Version 1 of the source table is assumed.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, NID, KUAL, CAT(256), BUFFER(*), ID(*), IRET
      CHARACTER CALKOD*4
C
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INTEGER   VER, LUN, IDKOL, SUKOL, IDSOU, SQUAL, NUMIF, ISURNO,
     *   NUMREC, I4, SUFQID
      LOGICAL   EQUAL
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RAOBS, DECOBS
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC)
      REAL      FLUX(4,MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      DOUBLE PRECISION LSRVEL(MAXIF)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IDKOL, SUKOLS(1)),   (SUKOL, SUKOLS(2))
      DATA VER, LUN /1, 27/
C-----------------------------------------------------------------------
C                                       Setup
      NID = 0
      IRET = 0
C                                       Initialize SOURCE table.
      CALL SOUINI ('READ', BUFFER, DISK, CNO, VER, CAT, LUN, NUMIF,
     *   VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
         END IF
C                                       Get number of entries
      NUMREC = BUFFER(5)
      NID = 0
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
C                                       is qualifier OK, too?
         EQUAL = ((KUAL.LT.0) .OR. (KUAL.EQ.SQUAL))
C                                       calcode
         IF (CALKOD.NE.' ') EQUAL = EQUAL .AND. ((CALKOD.EQ.CALCOD) .OR.
     *      ((CALKOD.EQ.'*') .AND. (CALCOD.NE.' ')) .OR.
     *      ((CALKOD.EQ.'-CAL') .AND. (CALCOD.EQ.' ')))
         IF (EQUAL) THEN
            NID = NID + 1
            ID(NID) = IDSOU
            END IF
500      CONTINUE
C                                       Close Source table
      CALL TABIO ('CLOS', 0, I4, SOUNAM, BUFFER, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ALLSOU: ERROR ',I3,' INITIALIZING SOURCE TABLE')
 1120 FORMAT ('ALLSOU: ERROR ',I3,' READING SOURCE TABLE')
      END
      SUBROUTINE FLAGEL (FGLUN, FGBUFF, IFGRNO, FGKOLS, FGNUMV,
     *   SUBA, STARTD, STOPD, FQID, FGOPEN, ELEVTL, ELEVTH, IRET)
C-----------------------------------------------------------------------
C   Updates the flag table (FG) to flag all data at the given
C   interval of elevation.
C   Inputs:
C      FGLUN    I        Logical unit number to use
C      SUBA     I        Subarray number.
C      STARTD   R        Start of the data (days)
C      STOPD    R        Stop  of the data (days)
C      FQID     I        Freqid number
C      ELEVTL   R        Lower elevation threshold (degrees)
C      ELEVTH   R        Upper elevation threshold (degrees)
C   Input from common:
C      OPCODE   C*4      Operation desired, 'FLAG'=> make entry,
C                        'UFLG' => deselect selected prev. entries.
C      STOKES   C*4      Stokes parameter desired
C      DISK     I        Disk to use.
C      CNOIN    I        Catalog slot number
C      FGVER    I        FG file version
C      XANTS    R(50)    List of antennas
C   Input/Output:
C      FGBUFF   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      IFGRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      FGKOLS   I(8)     The column pointer array in order, SOURCE,
C                        SUBARRAY, ANTS, TIMERANG, IFS, CHANS, PFLAGS,
C                        REASON
C      FGNUMV   I(*)     Element count in each column.
C   Output:
C      FGOPEN   L        FG table was open
C      IRET     I        Error code, 0=>OK else TABIO error.
C                        Note: -1 => read, but record deselected.
C-----------------------------------------------------------------------
      INTEGER   FGLUN, FGBUFF(*), IFGRNO, FGKOLS(*), FGNUMV(*), SUBA,
     *   FQID, IRET
      REAL      STARTD, STOPD, ELEVTL, ELEVTH
      LOGICAL   FGOPEN
C
      CHARACTER FILE*48
      CHARACTER LREAS*24
      INTEGER   SOUID, OLDSOU, IA1, IA2, IROUND, SID, XCOUNT, IVER,
     *   FIND, BIND, VO, BO, LENBU, NMCOR, NIO,IPOINT, J, ISUBA, LUNSS,
     *   LUNUV, LANT, JSUB, JSUB1, JSUB2, LFQID, NFL
      LOGICAL   T, F
      REAL      TIME, TIMOLD, BLTEMP, HA, EL1, AZ, TIMST, TIMEN, TINT,
     *   TDIF
      DOUBLE PRECISION TIMED
      INCLUDE 'UVFLG.INC'
      INCLUDE 'INCS:PUVD.INC'
      REAL      TBEG(MAXANT), TEND(MAXANT)
      LOGICAL   ANTOK(MAXANT), ANTREV
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUNSS /25/
      DATA LUNUV, BO, VO /16, 1, 0/
C-----------------------------------------------------------------------
      TINT = 0.09/24.0/3.6E3
      TDIF = TINT
      FGOPEN = .FALSE.
      LREAS = 'ELEVATION RANGE EXCLUDED'
      IF (REASON.NE.' ') LREAS = REASON
      CALL FNDEXT ('AN', CATBLK, ISUBA)
      IF ((SUBA.GT.0) .AND. (SUBA.LE.ISUBA)) THEN
         JSUB1 = SUBA
         JSUB2 = SUBA
      ELSE
         JSUB1 = 1
         JSUB2 = ISUBA
         END IF
C                                       Check ANTENNAS
      CALL LFILL (MAXANT, .FALSE., ANTOK)
      ANTREV = .FALSE.
      IA2 = 0
      DO 10 J = 1,50
         IA1 = IROUND (XANTS(J))
         IF (IA1.LT.0) THEN
            ANTREV = .TRUE.
            IA1 = -1
            END IF
         IF (IA1.GT.0) THEN
            ANTOK(IA1) = .TRUE.
            IA2 = J
         ELSE
            GO TO 15
            END IF
 10      CONTINUE
 15   IF (IA2.EQ.0) THEN
         CALL LFILL (MAXANT, .TRUE., ANTOK)
      ELSE IF (ANTREV) THEN
         DO 20 J = 1,MAXANT
            ANTOK(J) = .NOT.ANTOK(J)
 20         CONTINUE
         END IF
C                                       Open for Read.
      LENBU = 450
      LENBU = 0
      LFQID = 1
      IVER = 1
      CALL ZPHFIL ('UV', DISK, FCNO(1), IVER, FILE, IRET)
      CALL ZOPEN (LUNUV, FIND, DISK, FILE, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
      DO 200 JSUB = JSUB1,JSUB2
C                                       get antenna info this subarray
         CALL GETANT (DISK, CNOIN, SUBA, CATBLK, IBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1015) IRET, JSUB
            GO TO 990
            END IF
         CALL UVINIT ('READ', LUNUV, FIND, NVIS, VO, LREC, LENBU, BUFSZ,
     *      BUFF, BO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ'
            GO TO 990
            END IF
         XCOUNT = 0
         NMCOR = LREC - NRPARM
C                                       initialize the begin of flag
C                                       intervals
         CALL RFILL (MAXANT, -1000.0, TBEG)
         TIMOLD = -1.E6
         OLDSOU = -1
C                                       Start looping thru data.
C                                       Read buffer.
 50      XCOUNT = XCOUNT + 1
         CALL UVDISK ('READ', LUNUV, FIND, BUFF, NIO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1050) IRET, 'READ', XCOUNT
            GO TO 990
            END IF
         IPOINT = BIND
C                                       Loop through the data
         DO 100 J = 1,NIO
C                                       Decode time.
            TIME = BUFF(IPOINT+ILOCT)
C                                       Determine FQ ID
            IF (ILOCFQ.GT.-1) LFQID = IROUND (BUFF(IPOINT+ILOCFQ))
C                                       Determine subarray
            IF (ILOCB.GE.0) THEN
               BLTEMP = BUFF(IPOINT+ILOCB)
               IA1 = BLTEMP / 256. + 0.1
               BLTEMP = BLTEMP - IA1 * 256
               IA2 = BLTEMP + 0.1
               ISUBA = 100.0 * (BLTEMP - IA2) + 1.5
            ELSE
               IA1 = BUFF(IPOINT+ILOCA1) + 0.1
               IA1 = BUFF(IPOINT+ILOCA2) + 0.1
               ISUBA = BUFF(IPOINT+ILOCSA) + 0.1
               END IF
C                                       check time range, etc
            IF ((TIME.GE.STARTD) .AND. (TIME.LE.STOPD+1.16D-4) .AND.
     *         ((ISUBA.LE.0) .OR. (ISUBA.EQ.JSUB)) .AND.
     *         ((FQID.LE.0) .OR. (LFQID.EQ.FQID))) THEN
C                                       get source
               SOUID = 0
               IF (ILOCSU.GT.-1) SOUID = BUFF(IPOINT+ILOCSU)
C                                       Clear pending time intervals
               IF (SOUID.NE.OLDSOU) THEN
                  DO 60 LANT = 1,NSTNS
                     IF (TBEG(LANT).GT.-999.0) THEN
                        TIMST = TBEG(LANT)
                        TIMEN = TEND(LANT)
                        CALL FLAGUP (OPCODE, FGLUN, DISK, CNOIN, FGVER,
     *                     FGBUFF, IFGRNO, FGKOLS, FGNUMV,OLDSOU, 1,
     *                     JSUB, FQID, 1, LANT, 0, TIMST, TIMEN, 1, 0,
     *                     1, 0, STOKES, LREAS, NFL, IRET)
                        IF (IRET.NE.0) GO TO 999
                        TBEG(LANT) = -1000.0
                        FGOPEN = .TRUE.
                        WFLAG = WFLAG + NFL
                        END IF
 60                  CONTINUE
C                                       Check sources
                  IF (ID(1).GT.0) THEN
                     DO 70 SID = 1,NID
                        IF (SOUID.EQ.ID(SID)) GO TO 75
 70                     CONTINUE
                     GO TO 90
                     END IF
C                                       pick up the source coordinates
C                                       only when a new source appears
 75               IF (SOUID.NE.OLDSOU) CALL GETSOU (SOUID, DISK, CNOIN,
     *               CATBLK, LUNSS, IRET)
                  OLDSOU = SOUID
                  END IF
C                                       skip times closer 0.1sec
               IF (TIME-TIMOLD.GT.TDIF) THEN
                  TIMOLD = TIME
C                                       Start estimating of the flag
C                                       intervals for all antennas
C                                       for the given source
                  DO 80 LANT = 1,NSTNS
C                                       TIMED double precision
                     TIMED = TIME
                     CALL SOUELV (LANT, TIMED, HA, EL1, AZ)
                     EL1 = EL1 * RAD2DG
C                                       flag this point
                     IF ((EL1.GT.ELEVTL) .AND. (EL1.LT.ELEVTH) .AND.
     *                  (ANTOK(LANT))) THEN
                        IF (TBEG(LANT).LT.-999.0) TBEG(LANT) = TIME-TINT
                        TEND(LANT) = TIME + TINT
C                                       Don't flag this point
                     ELSE
C                                       But flag pending
                        IF (TBEG(LANT).GT.-999.0) THEN
                           TIMST = TBEG(LANT)
                           TIMEN = TEND(LANT)
                           CALL FLAGUP (OPCODE, FGLUN, DISK, CNOIN,
     *                        FGVER, FGBUFF, IFGRNO, FGKOLS, FGNUMV,
     *                        OLDSOU, 1, JSUB, FQID, 1,LANT, 0, TIMST,
     *                        TIMEN, 1, 0, 1, 0, STOKES, LREAS, NFL,
     *                        IRET)
                           IF (IRET.NE.0) GO TO 999
                           TBEG(LANT) = -1000.0
                           FGOPEN = .TRUE.
                           WFLAG = WFLAG + NFL
                           END IF
                        END IF
 80                  CONTINUE
                  END IF
               END IF
C                                       Update IPOINT
 90         IPOINT = IPOINT + LREC
 100        CONTINUE
         IF (NIO.GT.0) GO TO 50
C                                       Clear pending time intervals
         DO 110 LANT = 1,NSTNS
            IF (TBEG(LANT).GT.-999.0) THEN
               TIMST = TBEG(LANT)
               TIMEN = TEND(LANT)
               CALL FLAGUP (OPCODE, FGLUN, DISK, CNOIN, FGVER, FGBUFF,
     *            IFGRNO, FGKOLS, FGNUMV, OLDSOU, 1, JSUB, FQID, 1,
     *            LANT, 0, TIMST, TIMEN, 1, 0, 1, 0, STOKES, LREAS,
     *            NFL, IRET)
               IF (IRET.NE.0) GO TO 999
               TBEG(LANT) = -1000.0
               FGOPEN = .TRUE.
               WFLAG = WFLAG + NFL
               END IF
 110        CONTINUE
 200     CONTINUE
C                                       Close file.
      CALL ZCLOSE (LUNUV, FIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1300) IRET
         CALL MSGWRT (6)
         END IF
      IRET = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR:',I7,' INITIALIZING UV FILE FOR ',A4,'ING')
 1010 FORMAT ('ERROR:',I7,' OPENING UV FILE TO READ')
 1015 FORMAT ('ERROR:',I4,' READING ANTENNA FILE',I4)
 1050 FORMAT ('ERROR:',I7,1X,A4,'ING VIS BLOCK ',I10)
 1300 FORMAT ('ERROR:',I7,' CLOSING UV FILE ')
      END
      SUBROUTINE FLGPCL (LUN, FGBUFF, IFGRNO, FGKOLS, FGNUMV, FACTOR,
     *   SUBA, WSBLNK, STARTD, STOPD, BIF, EIF, BCHAN, ECHAN, FGOPEN,
     *   IRET)
C-----------------------------------------------------------------------
C   Updates the flag table (FG) to flag all data in the vicinity of
C   zero fringe rate.
C-----------------------------------------------------------------------
C   Inputs:
C      LUN      I        Logical unit number to use
C      FACTOR   R        Suppression of PCCAL
C      SUBA     I        Subarray number.
C      WSBLNK   L        True if all selected sources are blank
C      STARTD   R        Selected start time, days
C      STOPD    R        Selected stop time, days
C      BIF      I        First IF number to flag. 0=>all
C      EIF      I        Last IF number to flag. 0=>all higher than BIF
C      BCHAN    I        First channel number to flag. 0=>all
C      ECHAN    I        Last channel number to flag. 0=>all higher.
C   Input from common:
C      OPCODE   C*4      Operation desired, 'FLAG'=> make entry,
C                        'UFLG' => deselect selected prev. entries.
C      XANTS    R(*)     List of selected ANTENNA
C      XBASE    R(*)     List of selected BASELINE
C      STOKES   C*4      Stokes parameter desired
C      DISK     I        Disk to use.
C      CNOIN    I        Catalog slot number
C      FGVER      I      FG file version
C      NXANT    I        Number of selected antennas
C      TELN     I(*)     Array of selected antennas
C      FRQSEL   I        Freqid number
C   Input/Output:
C      FGBUFF   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      IFGRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      FGKOLS   I(8)     The column pointer array in order, SOURCE,
C                        SUBARRAY, ANTS, TIMERANG, IFS, CHANS, PFLAGS,
C                        REASON
C      FGNUMV   I(8)     Element count in each column.
C   Output:
C      FGOPEN   L        FG table was open
C      IRET     I        Error code, 0=>OK else TABIO error.
C                        Note: -1 => read, but record deselected.
C-----------------------------------------------------------------------
      INTEGER LUN, FGBUFF(*), IFGRNO, FGKOLS(8), FGNUMV(8),
     *   SUBA, FQD, BIF, EIF, BCHAN, ECHAN, IRET
      CHARACTER PCREAS*24
      INCLUDE 'INCS:PUVD.INC'
      INTEGER NXVER, BUFFNX(512), LUNI, INXRNO, NXKOLS(MAXNXC),
     *   NXNUMV(MAXNXC), NUMBNX, ISUB, VSTART, VEND, FRID, SID,
     *   INDSOU, AN1(MXBASE), AN2(MXBASE), NNBASE, IBASE, IK, ANTEM1,
     *   ANTEM2, ANTE(MXBASE), ANT1, ANT2, LUNSS, MAXBAS, I, K,
     *   NT0, IROUND, NSELIF, IFQ, KIF, BIFF, EIFF, IERR,
     *   IDAY, IHOUR, IMIN, NFL
C                                       FQ table start
      INTEGER LUNF, IFQRNI, NFQID, NUMIF, FQFID(MAXFQC),
     *   IFSIDE(MAXIF), FQID, FQVER, MXFQID, MXFQIF, NROW, IND
      REAL    FQKOLS(MAXFQC), FQNUMV(MAXFQC), IFCHW(MAXIF),
     *   IFTBW(MAXIF), HOUR, MINUT, SEC
      PARAMETER (MXFQIF = MAXIF*MAXFQC)
      DOUBLE PRECISION IFFREQ(MAXIF), DFIF(MXFQIF)
      CHARACTER BNDCOD(MAXIF)*8
C                                       FQ table end
      REAL RTIME, DTIME, TST, TEN, FACTOR, FR, BXY, DELTAT, DAYTIM,
     *   TLEFT, TRIGHT, STOPD, STARTD, TMED, XY, X, Y, ALFAB,
     *   S0, T0, TSTINI, TENINI
      DOUBLE PRECISION  XANTE(100), YANTE(100), U, V, HA, Z
      LOGICAL TABL, EXIST, FITASC, OURS, OURST, OUT, WSBLNK,
     *   FGOPEN
      INCLUDE 'UVFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUNI, LUNSS, LUNF /16, 25, 28/
      DATA MAXBAS /MXBASE/
C-----------------------------------------------------------------------
C                                       print out the Factor of
C                                       suppression
      WRITE (MSGTXT,1100) FACTOR
      CALL MSGWRT (8)
C                                       Block to read FQ table to find
C                                       correction to the frequency for
C                                       each IF and finally calculate
C                                       lambdas
      MXFQID = MAXFQC
      IRET = 0
      FQVER = 1
C                                       Open FQ table
      CALL FQINI ('READ', BUFFNX, DISK, CNOIN, FQVER, CATBLK, LUNF,
     *   IFQRNI, FQKOLS, FQNUMV, NUMIF, IRET)
      IF (IRET.EQ.0) THEN
         NROW = BUFFNX(5)
         DO 30 I = 1, NROW
            CALL TABFQ ('READ', BUFFNX, IFQRNI, FQKOLS, FQNUMV, NUMIF,
     *         FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1200) IRET
               GO TO 990
               END IF
            IF (FRQSEL.LE.0) THEN
C                                       take DFREQ for all available
C                                       IFs and FQIDs
               NFQID = NROW
               FQFID(I) = FQID
               DO 10 K = 1, NUMIF
                  IND = K + (I - 1) * NUMIF
                  DFIF(IND) = IFFREQ(K)
   10             CONTINUE
            ELSE
C                                       take DFREQ only for selected
C                                       FQID for all available IFs
               NFQID = 1
               IF (FRQSEL.EQ.FQID) THEN
                  FQFID(1) = FQID
                  DO 20 K = 1, NUMIF
                     IND = K
                     DFIF(IND) = IFFREQ(K)
   20                CONTINUE
                  GO TO 35
                  END IF
               END IF
C
   30       CONTINUE
         IF (FRQSEL.GT.0) THEN
C                                       given FRQSEL is not found at
C                                       the FQ table
            WRITE (MSGTXT,1300) FRQSEL
            GO TO 990
            END IF
   35    CONTINUE
C                                       close FQ table
         CALL TABIO ('CLOS', 0, IFQRNI, BUFFNX, BUFFNX, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1350) IRET
            GO TO 990
            END IF
      ELSE
         NFQID = MXFQID
         DO 45 I = 1, MXFQID
            DO 40 K = 1, NOMIF
               IND = K + (I - 1) * NOMIF
               DFIF(IND) = 0
   40          CONTINUE
   45       CONTINUE
         END IF
C                                       FQ table has been read.
      FGOPEN = .FALSE.
C                                       number of selected IFs
      NSELIF = EIF - BIF + 1
C                                       frequency from AN table header
      FR = SAFREQ
      S0 = GSTIAT
      DAYTIM = 1.0 / 1.002738
      PCREAS = 'small fringe rate'
      IF (REASON(1:1).NE.' ') PCREAS=REASON

C                                       Determine actual number of
C                                       baseline and list of the first
C                                       and second antennas
C                                       Convert to baseline numbers.
      NNBASE = MAXBAS
      CALL AN10RS (NUMAN, FSUBA(1), XANTS, XBASE, NNBASE, AN1, AN2)
C                                       Use STNRAD, STNLON, STNLAT
C                                       (output of GETANT in DANS.INC)
C                                       They and only they (not STNY)
C                                       are treated for different
C                                       coordinate system of VLA, VLBA,
C                                       ATCA. STNLON is to EAST.
      DO 50 K = 1, NSTNS
         ANTEM1 = TELNO(K)
         XY = STNRAD(ANTEM1) * COS(STNLAT(ANTEM1))
         XANTE(ANTEM1) = XY * COS(STNLON(ANTEM1))
C                                       transform to Left hand
         YANTE(ANTEM1) = -XY * SIN(STNLON(ANTEM1))
   50    CONTINUE
C                                       XLONG is west longtitute
      IF ((NNBASE.EQ.1).AND.(AN1(1).EQ.0).AND.(AN2(1).EQ.0)) THEN
C                                       all baseline are selected
C                                       ANTENNAS=0, BASELINE=0
C                                       Case #1 at AN10RS
         NNBASE = 0
         DO 70 I = 1, NSTNS
C                                       skip antennas with zeros
C                                       coordinates
C                                       such a behavior of STNY,
C                                       STNZ is a GETANT's feature
            IF ((STNX(I).LE.1.D2) .AND. (STNY(I).LE.1.D2) .AND.
     *         (STNZ(I).LT.1.D2)) GO TO 70
            DO 60 K = I+1,NSTNS
C                                       skip antennas with zeros
C                                       coordinates
               IF ((STNX(K).LT.1.D2) .AND. (STNY(K).LE.1.D2) .AND.
     *            (STNZ(K).LT.1.D2)) GO TO 60
               NNBASE = NNBASE + 1
               AN1(NNBASE) = TELNO(I)
               AN2(NNBASE) = TELNO(K)
  60           CONTINUE
  70       CONTINUE
      ELSE
C
         IF ((AN1(1) .NE. 0) .AND. (AN2(1).EQ.0)) THEN
C                                       Baselines ANTENNAS with any
C                                       antennas or BASELINE with
C                                       any antennas selected
C                                       ANTENNAS.EQ.0 and BASELINE.GT.0
C                                  .OR. ANTENNAS.GT.0 and BASELINE.EQ.0
C                                       Case #2 at AN10RS
            DO 90 I = 1, NNBASE
               ANTE(I) = AN1(I)
   90          CONTINUE
            IBASE = 0
            DO 130 I = 1, NNBASE
               ANTEM1 = ANTE(I)
               DO 120 K = 1, NSTNS
                  ANTEM2 = TELNO(K)
C                                       skip antennas with zeros
C                                       coordinates
                  IF (((STNX(ANTEM1).LT.1.D2) .AND.
     *               (STNY(ANTEM1).LT.1.D2) .AND.
     *               (STNZ(ANTEM1).LT.1.D2)) .OR.
     *               ((STNX(ANTEM2).LT.1.D2) .AND.
     *               (STNY(ANTEM2).LT.1.D2) .AND.
     *               (STNZ(ANTEM2).LT.1.D2))) GO TO 120
C                                       skip identical antennas
                  IF (ANTE(I).EQ.TELNO(K)) GO TO 120
C
                  DO 110 IK = 1, I-1
C                                       skip identical baseline
                     IF (ANTE(IK).EQ.TELNO(K)) GO TO 120
  110                CONTINUE
                  IBASE = IBASE + 1
                  AN1(IBASE) = ANTE(I)
                  AN2(IBASE) = TELNO(K)
  120             CONTINUE
  130          CONTINUE
            NNBASE = IBASE
         ELSE
C
            IF ((AN1(1) .NE. 0) .AND. (AN2(1) .NE. 0)) THEN
C                                       Baselines ANTENNAS with
C                                       BASELINE are selected.
C                                       Both ANTENNAS and BASELINE are
C                                       not zero
C                                       Case #3,#4, and #5 at AN10RS
               IBASE = 0
               DO 140 I = 1, NNBASE
                  ANTEM1 = AN1(I)
                  ANTEM2 = AN2(I)
C                                       skip identical antennas
                  IF (ANTEM1.EQ.ANTEM2) GO TO 140
C                                       skip antennas with zeros
C                                       coordinates
                  IF (((STNX(ANTEM1).LT.1.D2) .AND.
     *               (STNY(ANTEM1).LT.1.D2) .AND.
     *               (STNZ(ANTEM1).LT.1.D2)) .OR.
     *               ((STNX(ANTEM2).LT.1.D2) .AND.
     *               (STNY(ANTEM2).LT.1.D2) .AND.
     *               (STNZ(ANTEM2).LT.1.D2))) GO TO 140
                  IBASE = IBASE + 1
                  AN1(IBASE) = ANTEM1
                  AN2(IBASE) = ANTEM2
  140             CONTINUE
               NNBASE = IBASE
               END IF
            END IF
         END IF
C                                       arrange the second antenna's
C                                       number is bigger than the
C                                       first one
      DO 150 I = 1, NNBASE
         ANTEM1 = AN1(I)
         ANTEM2 = AN2(I)
         IF (ANTEM1.GT.ANTEM2) THEN
            AN1(I) = ANTEM2
            AN2(I) = ANTEM1
            END IF
  150    CONTINUE
C                                       check if NX table exists
      NXVER = 1
      CALL ISTAB ('NX', DISK, CNOIN, NXVER, LUNI, BUFFNX, TABL,
     *   EXIST, FITASC, IERR)
      IF (.NOT.EXIST) THEN
         IRET = 1
         WRITE (MSGTXT, 1400)
         GO TO 990
         END IF
C                                       Open NX table
      CALL NDXINI ('READ', BUFFNX, DISK, CNOIN, NXVER, CATBLK,
     *   LUNI, INXRNO, NXKOLS, NXNUMV, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1500) IRET
         GO TO 990
         END IF
C                                       How many entries in NX table
      NUMBNX = BUFFNX(5)

      DO 250 I = 1, NUMBNX
C                                       read the NX table's line
         CALL TABNDX ('READ', BUFFNX, INXRNO, NXKOLS, NXNUMV, RTIME,
     *      DTIME, INDSOU, ISUB, VSTART, VEND, FRID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1600) IRET
            GO TO 990
            END IF
C                                       find the sequent number of FRID
C                                       in FQ table
         DO 160 IFQ = 1, NFQID
            FQD = FQFID(IFQ)
            IF (FQD.EQ.FRID) GO TO 165
  160       CONTINUE
            GO TO 250
  165       CONTINUE
C                                       Check subarray
         OURS = (SUBA.LE.0) .OR. (SUBA.EQ.ISUB)
         IF (.NOT. OURS) GO TO 250
C                                       Check sources
         OURST = .FALSE.
         DO 190 SID = 1, NID
            IF (INDSOU.EQ.ID(SID)) THEN
               CALL GETSOU (INDSOU, DISK, CNOIN, CATBLK, LUNSS, IRET)
               DO 170 K = 1,30
C                                       compare with the list of
C                                       selected soutces
                  IF (WSBLNK .OR. (XSOUR(K).EQ.SNAME)) THEN
                     OURST = .TRUE.
                     GO TO 210
                     END IF
  170              CONTINUE
               END IF
  190       CONTINUE
  210    CONTINUE
C
         IF (.NOT.OURST) GO TO 250
C                                       start preparing times and
C                                       antennas for flagging
            TST = RTIME - DTIME*0.5
            TEN = RTIME + DTIME*0.5
C
            OUT = (TST.GT.STOPD) .OR. (TEN.LT.STARTD)
            IF (OUT) THEN
C                                       the source was not observed at
C                                       the selected timerange
               GO TO 250
            ELSE
C                                       store TST, TEN for the given
C                                       scan (source)
               TSTINI = MAX(TST, STARTD)
               TENINI = MIN(TEN, STOPD)
               END IF
            DO 230 IBASE = 1, NNBASE
C                                       restore TST, TEN for the given
C                                       scan (source)
               TST = TSTINI
               TEN = TENINI
               ANT1 = AN1(IBASE)
               ANT2 = AN2(IBASE)
               X = XANTE(ANT2) - XANTE(ANT1)
               Y = YANTE(ANT2) - YANTE(ANT1)
               IF (X .NE. 0) THEN
                  ALFAB = ATAN2 (Y, X)
               ELSE
                  ALFAB = PI / 2
                  END IF
C                                       T0 in days
               T0 = (ALFAB - S0 + RAAPP) * DAYTIM / TWOPI
C                                       Remove possible integer
C                                       difference in T0
               TMED = (TST + TEN) / 2.0
C               NT0 = IROUND((TMED - T0) / DAYTIM)
C                                       Fringe rate zeros (as U)
C                                       repeat with period of DAYTIM/2
C                                       not DAYTIM
               NT0 = IROUND((TMED - T0) / (DAYTIM/2))
C               T0 = T0 + NT0 * DAYTIM
               T0 = T0 + NT0 * (DAYTIM/2)
C
               DO 220 KIF = 1, NSELIF
C                                       restore TST, TEN for the given
C                                       scan (source)
                  TST = TSTINI
                  TEN = TENINI
                  BIFF = KIF + BIF -1
                  EIFF = BIFF
                  IND = BIFF + (IFQ - 1) * NUMIF
C                                       the frequency FR is the sum of
C                                       1. SAFREQ is AN table header
C                                       2. FREQO correction for the
C                                          given source from SU table
C                                       3. DFIF correction for the given
C                                          FREQID and IF from FQ table
                  FR = SAFREQ + FREQO(BIFF) + DFIF(IND)
C                                       Baseline projection on equator
C                                       in lambdas
                  BXY = SQRT (X*X + Y*Y) * FR / VELITE
C                                       DELTAT time interval of small
C                                       fringe rate
                  DELTAT = FACTOR * DAYTIM /
     *               (TWOPI * SQRT (BXY * COS(DECAPP)))
                  TLEFT = T0 - (DELTAT/2)
                  TRIGHT = T0 + (DELTAT/2)
                  OUT = (TST.GT.TRIGHT) .OR. (TEN.LT.TLEFT)
C                                       the interval of small fringe
C                                       rate is out of the given source
C                                       time, or we are not going
C                                       flagging the small fringe
C                                       problem
                  IF (OUT) THEN
                        GO TO 220
                  ELSE
                     TST = MAX(TST, TLEFT)
                     TEN = MIN(TEN, TRIGHT)
C                                       print flaged time intervals
C                                       for the given IF
                     IF (APARM(4).GT.0) THEN
                        IF (KIF.EQ.1) THEN
C                                       print the source, baseline,
C                                       time and U,V of expected zero
C                                       fringe rate only for the IF=1
                           Z = STNRAD(ANT2)*SIN(STNLAT(ANT2)) -
     *                        STNRAD(ANT1)*SIN(STNLAT(ANT1))
                           HA = (T0 *TWOPI/DAYTIM) + S0 - RAAPP
                           U = (-X * SIN(HA) + Y * COS(HA))* SAFREQ /
     *                        VELITE/1000
                           V = ((X * COS(HA) + Y * SIN(HA)) *SIN(DECAPP)
     *                        -Z*COS(DECAPP)) * SAFREQ / VELITE/1000
                           IDAY = T0
                           HOUR = (T0 - IDAY) * 24
                           IHOUR = HOUR
                           MINUT = (HOUR - IHOUR) * 60
                           IMIN = MINUT
                           SEC = (MINUT - IMIN) * 60
                           WRITE (MSGTXT,1800) SNAME, ANT1, ANT2, IDAY,
     *                        IHOUR,IMIN, SEC, U, V
                           CALL MSGWRT (8)
                           END IF
C
                        IDAY = TST
                        HOUR = (TST - IDAY) * 24
                        IHOUR = HOUR
                        MINUT = (HOUR - IHOUR) * 60
                        IMIN = MINUT
                        SEC = (MINUT - IMIN) * 60
C
                        WRITE (MSGTXT,1900) BIFF, IDAY, IHOUR, IMIN,
     *                     SEC
C
                        IDAY = TEN
                        HOUR = (TEN - IDAY) * 24
                        IHOUR = HOUR
                        MINUT = (HOUR - IHOUR) * 60
                        IMIN = MINUT
                        SEC = (MINUT - IMIN) * 60
C
                        WRITE (MSGTXT(35:80),1950) IDAY, IHOUR, IMIN,
     *                     SEC
                        CALL MSGWRT (8)

                        END IF
C                                       put flag line into FG table
                     CALL FLAGUP (OPCODE, LUN, DISK, CNOIN, FGVER,
     *                  FGBUFF, IFGRNO, FGKOLS, FGNUMV, INDSOU, 1,
     *                  SUBA, FQD, 1, ANT1, ANT2, TST, TEN, BIFF,
     *                  EIFF, BCHAN, ECHAN, STOKES, PCREAS, NFL, IRET)
C                                       Yes, the flag table was open
                     FGOPEN = .TRUE.
                     IF (IRET.GT.0) GO TO 999
                     WFLAG = WFLAG + NFL
                     END IF
  220             CONTINUE
  230          CONTINUE
  250    CONTINUE
C                                       close NX table
      CALL TABIO ('CLOS', 0, INXRNO, BUFFNX, BUFFNX, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1700) IRET
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Suppression factor = ', F4.1)
 1200 FORMAT ('FLGPCL:  ERROR ',I3,' READING FQ FILE')
 1300 FORMAT ('FLGPCL:  FQID', I3, ' is not found at the FQ table')
 1350 FORMAT ('FLGPCL:  ERROR ',I3,' CLOSING FQ FILE')
 1400 FORMAT ('There is no NX table. Run INDXR to get it')
 1500 FORMAT ('FLGPCL:  ERROR ',I3,' OPENING NX FILE')
 1600 FORMAT ('FLGPCL:  ERROR ',I3,' READING NX FILE')
 1700 FORMAT ('FLGPCL:  ERROR ',I3,' CLOSING NX FILE')
 1800 FORMAT ( A8, I2, ' -', I2, 2X, I1, '/',I2, ':' ,I2, ':', F4.1,
     *   ' U= ', F10.2, ' V= ', F10.2)
 1900 FORMAT ( 10X, 'IF = ', I2, ' | ', I1, '/', I2, ':' ,
     *   I2, ':', F4.1)
 1950 FORMAT ( '- ', I1, '/',I2, ':' ,I2, ':', F4.1)
      END
      SUBROUTINE FLAGSH (FGLUN, FGBUFF, IFGRNO, FGKOLS, FGNUMV, SUBA,
     *   STARTD, STOPD, FQID, FGOPEN, SHMIN, CTMIN, IRET)
C-----------------------------------------------------------------------
C   Updates the flag table (FG) to flag all data at the given
C   interval of elevation.
C   Inputs:
C      FGLUN    I        Logical unit number to use
C      SUBA     I        Subarray number.
C      STARTD   R        Start of the data (days)
C      STOPD    R        Stop  of the data (days)
C      FQID     I        Freqid number
C      SHMIN    R        Min baseline in lambda at ref freq shadowing
C      CTMIN    R        Min baseline in lambda for cross talk
C   Input from common:
C      OPCODE   C*4      Operation desired, 'FLAG'=> make entry,
C                        'UFLG' => deselect selected prev. entries.
C      STOKES   C*4      Stokes parameter desired
C      DISK     I        Disk to use.
C      CNOIN    I        Catalog slot number
C      FGVER    I        FG file version
C      XANTS    R(50)    List of antennas
C   Input/Output:
C      FGBUFF   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      IFGRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      FGKOLS   I(8)     The column pointer array in order, SOURCE,
C                        SUBARRAY, ANTS, TIMERANG, IFS, CHANS, PFLAGS,
C                        REASON
C      FGNUMV   I(*)     Element count in each column.
C   Output:
C      FGOPEN   L        FG table was open
C      IRET     I        Error code, 0=>OK else TABIO error.
C                        Note: -1 => read, but record deselected.
C-----------------------------------------------------------------------
      INTEGER   FGLUN, FGBUFF(*), IFGRNO, FGKOLS(*), FGNUMV(*), SUBA,
     *   FQID, IRET
      REAL      STARTD, STOPD, SHMIN, CTMIN
      LOGICAL   FGOPEN
C
      CHARACTER FILE*48
      CHARACTER LREAS1*24, LREAS2*24
      INTEGER   SOUID, OLDSOU, IA1, IA2, IROUND, SID, XCOUNT, IVER,
     *   FIND, BIND, VO, BO, LENBU, NMCOR, NIO,IPOINT, J, ISUBA, LUNSS,
     *   LUNUV, LANT, JSUB, JSUB1, JSUB2, LFQID, NFL, NB, LA1, LA2
      LOGICAL   T, F
      REAL      TIME, TIMOLD, BLTEMP, TIMST, TIMEN, TINT, TDIF
      INCLUDE 'UVFLG.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXB
      PARAMETER (MAXB = MAXANT*MAXANT)
      REAL      TBEG(MAXANT), TEND(MAXANT), TBB(MAXB), TBE(MAXB)
      LOGICAL   ANTBAD(MAXANT), BASBAD(MAXB)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUNSS /25/
      DATA LUNUV, BO, VO /16, 1, 0/
C-----------------------------------------------------------------------
      TINT = 0.09/24.0/3.6E3
      TDIF = TINT
      FGOPEN = .FALSE.
      LREAS1 = 'antenna shadowing'
      LREAS2 = 'possible cross-talk'
      IF (REASON.NE.' ') LREAS1 = REASON
      CALL FNDEXT ('AN', CATBLK, ISUBA)
      IF ((SUBA.GT.0) .AND. (SUBA.LE.ISUBA)) THEN
         JSUB1 = SUBA
         JSUB2 = SUBA
      ELSE
         JSUB1 = 1
         JSUB2 = ISUBA
         END IF
C                                       Open for Read.
      LENBU = 450
      LENBU = 0
      LFQID = 1
      IVER = 1
      CALL ZPHFIL ('UV', DISK, FCNO(1), IVER, FILE, IRET)
      CALL ZOPEN (LUNUV, FIND, DISK, FILE, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
      DO 200 JSUB = JSUB1,JSUB2
C                                       get antenna info this subarray
         CALL GETANT (DISK, CNOIN, SUBA, CATBLK, IBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1015) IRET, JSUB
            GO TO 990
            END IF
         CALL UVINIT ('READ', LUNUV, FIND, NVIS, VO, LREC, LENBU, BUFSZ,
     *      BUFF, BO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ'
            GO TO 990
            END IF
         XCOUNT = 0
         NMCOR = LREC - NRPARM
         NB = NSTNS * NSTNS
C                                       initialize the begin of flag
C                                       intervals
         CALL RFILL (MAXANT, -1000.0, TBEG)
         CALL RFILL (MAXB, -1000.0, TBB)
         CALL LFILL (NSTNS, .FALSE., ANTBAD)
         CALL LFILL (MAXB, .FALSE., BASBAD)
         TIMOLD = -1.E6
         OLDSOU = -1
C                                       Start looping thru data.
C                                       Read buffer.
 50      XCOUNT = XCOUNT + 1
         CALL UVDISK ('READ', LUNUV, FIND, BUFF, NIO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1050) IRET, 'READ', XCOUNT
            GO TO 990
            END IF
         IPOINT = BIND
C                                       Loop through the data
         DO 100 J = 1,NIO
C                                       Decode time.
            TIME = BUFF(IPOINT+ILOCT)
C                                       Determine FQ ID
            IF (ILOCFQ.GT.-1) LFQID = IROUND (BUFF(IPOINT+ILOCFQ))
C                                       Determine subarray
            IF (ILOCB.GE.0) THEN
               BLTEMP = BUFF(IPOINT+ILOCB)
               IA1 = BLTEMP / 256. + 0.1
               BLTEMP = BLTEMP - IA1 * 256
               IA2 = BLTEMP + 0.1
               ISUBA = 100.0 * (BLTEMP - IA2) + 1.5
            ELSE
               IA1 = BUFF(IPOINT+ILOCA1) + 0.1
               IA2 = BUFF(IPOINT+ILOCA2) + 0.1
               ISUBA = BUFF(IPOINT+ILOCSA) + 0.1
               END IF
C                                       check time range, etc
            IF ((TIME.GE.STARTD) .AND. (TIME.LE.STOPD+1.16D-4) .AND.
     *         ((ISUBA.LE.0) .OR. (ISUBA.EQ.JSUB)) .AND.
     *         ((FQID.LE.0) .OR. (LFQID.EQ.FQID))) THEN
C                                       get source
               SOUID = 0
               IF (ILOCSU.GT.-1) SOUID = BUFF(IPOINT+ILOCSU)
C                                       Clear pending time intervals
               IF (SOUID.NE.OLDSOU) THEN
                  DO 60 LANT = 1,NSTNS
                     IF (TBEG(LANT).GT.-999.0) THEN
                        TIMST = TBEG(LANT)
                        TIMEN = TEND(LANT)
                        CALL FLAGUP (OPCODE, FGLUN, DISK, CNOIN, FGVER,
     *                     FGBUFF, IFGRNO, FGKOLS, FGNUMV,OLDSOU, 1,
     *                     JSUB, FQID, 1, LANT, 0, TIMST, TIMEN, 1, 0,
     *                     1, 0, STOKES, LREAS1, NFL, IRET)
                        IF (IRET.NE.0) GO TO 999
                        TBEG(LANT) = -1000.0
                        FGOPEN = .TRUE.
                        WFLAG = WFLAG + NFL
                        END IF
 60                  CONTINUE
                  DO 65 LANT = 1,NB
                     IF (TBB(LANT).GT.-999.0) THEN
                        TIMST = TBB(LANT)
                        TIMEN = TBE(LANT)
                        LA1 = (LANT-1) / NSTNS + 1
                        LA2 = LANT - NSTNS * (LA1-1)
                        CALL FLAGUP (OPCODE, FGLUN, DISK, CNOIN, FGVER,
     *                     FGBUFF, IFGRNO, FGKOLS, FGNUMV,OLDSOU, 1,
     *                     JSUB, FQID, 1, LA1, LA2, TIMST, TIMEN, 1, 0,
     *                     1, 0, STOKES, LREAS2, NFL, IRET)
                        IF (IRET.NE.0) GO TO 999
                        TBB(LANT) = -1000.0
                        FGOPEN = .TRUE.
                        WFLAG = WFLAG + NFL
                        END IF
 65                  CONTINUE
C                                       Check sources
                  IF (ID(1).GT.0) THEN
                     DO 70 SID = 1,NID
                        IF (SOUID.EQ.ID(SID)) GO TO 75
 70                     CONTINUE
                     GO TO 90
                     END IF
C                                       pick up the source coordinates
C                                       only when a new source appears
 75               IF (SOUID.NE.OLDSOU) CALL GETSOU (SOUID, DISK, CNOIN,
     *               CATBLK, LUNSS, IRET)
                  OLDSOU = SOUID
                  END IF
C                                       new time interval
C                                       stuff old into time arrays
               IF (TIME-TIMOLD.GT.TDIF) THEN
                  TIMOLD = TIME
C                                       is this point good or no?
                  CALL ISSHAD (BUFF(IPOINT), JSUB, SHMIN, CTMIN, ANTBAD,
     *               BASBAD)
                  DO 80 LANT = 1,NSTNS
C                                       interval bad
                     IF (ANTBAD(LANT)) THEN
                        IF (TBEG(LANT).LT.-999.0) TBEG(LANT) =
     *                     TIMOLD - TINT
                        TEND(LANT) = TIMOLD + TINT
                        ANTBAD(LANT) = .FALSE.
C                                       interval now good, wasn't
                     ELSE IF (TBEG(LANT).GT.-999.0) THEN
                        TIMST = TBEG(LANT)
                        TIMEN = TEND(LANT)
                        CALL FLAGUP (OPCODE, FGLUN, DISK, CNOIN, FGVER,
     *                     FGBUFF, IFGRNO, FGKOLS, FGNUMV,OLDSOU, 1,
     *                     JSUB, FQID, 1, LANT, 0, TIMST, TIMEN, 1, 0,
     *                     1, 0, STOKES, LREAS1, NFL, IRET)
                        IF (IRET.NE.0) GO TO 999
                        TBEG(LANT) = -1000.0
                        FGOPEN = .TRUE.
                        WFLAG = WFLAG + NFL
                        END IF
 80                  CONTINUE
                  DO 85 LANT = 1,NB
C                                       interval bad
                     IF (BASBAD(LANT)) THEN
                        IF (TBB(LANT).LT.-999.0) TBB(LANT) =
     *                     TIMOLD - TINT
                        TBE(LANT) = TIMOLD + TINT
                        BASBAD(LANT) = .FALSE.
C                                       interval now good, wasn't
                     ELSE IF (TBB(LANT).GT.-999.0) THEN
                        TIMST = TBB(LANT)
                        TIMEN = TBE(LANT)
                        LA1 = (LANT-1) / NSTNS + 1
                        LA2 = LANT - NSTNS * (LA1-1)
                        CALL FLAGUP (OPCODE, FGLUN, DISK, CNOIN, FGVER,
     *                     FGBUFF, IFGRNO, FGKOLS, FGNUMV,OLDSOU, 1,
     *                     JSUB, FQID, 1, LA1, LA2, TIMST, TIMEN, 1, 0,
     *                     1, 0, STOKES, LREAS2, NFL, IRET)
                        IF (IRET.NE.0) GO TO 999
                        TBB(LANT) = -1000.0
                        FGOPEN = .TRUE.
                        WFLAG = WFLAG + NFL
                        END IF
 85                  CONTINUE
                  END IF
               END IF
C                                       Update IPOINT
 90         IPOINT = IPOINT + LREC
 100        CONTINUE
         IF (NIO.GT.0) GO TO 50
C                                       Clear pending time intervals
         DO 110 LANT = 1,NSTNS
            IF (TBEG(LANT).GT.-999.0) THEN
               TIMST = TBEG(LANT)
               TIMEN = TEND(LANT)
               CALL FLAGUP (OPCODE, FGLUN, DISK, CNOIN, FGVER, FGBUFF,
     *            IFGRNO, FGKOLS, FGNUMV, OLDSOU, 1, JSUB, FQID, 1,
     *            LANT, 0, TIMST, TIMEN, 1, 0, 1, 0, STOKES, LREAS1,
     *            NFL, IRET)
               IF (IRET.NE.0) GO TO 999
               TBEG(LANT) = -1000.0
               FGOPEN = .TRUE.
               WFLAG = WFLAG + NFL
               END IF
 110        CONTINUE
         DO 120 LANT = 1,NB
            IF (TBB(LANT).GT.-999.0) THEN
               TIMST = TBB(LANT)
               TIMEN = TBE(LANT)
               LA1 = (LANT-1) / NSTNS + 1
               LA2 = LANT - NSTNS * (LA1-1)
               CALL FLAGUP (OPCODE, FGLUN, DISK, CNOIN, FGVER, FGBUFF,
     *            IFGRNO, FGKOLS, FGNUMV,OLDSOU, 1, JSUB, FQID, 1, LA1,
     *            LA2, TIMST, TIMEN, 1, 0, 1, 0, STOKES, LREAS2, NFL,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               TBB(LANT) = -1000.0
               FGOPEN = .TRUE.
               WFLAG = WFLAG + NFL
               END IF
 120        CONTINUE
 200     CONTINUE
C                                       Close file.
      CALL ZCLOSE (LUNUV, FIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1300) IRET
         CALL MSGWRT (6)
         END IF
      IRET = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR:',I7,' INITIALIZING UV FILE FOR ',A4,'ING')
 1010 FORMAT ('ERROR:',I7,' OPENING UV FILE TO READ')
 1015 FORMAT ('ERROR:',I4,' READING ANTENNA FILE',I4)
 1050 FORMAT ('ERROR:',I7,1X,A4,'ING VIS BLOCK ',I10)
 1300 FORMAT ('ERROR:',I7,' CLOSING UV FILE ')
      END
      SUBROUTINE ISSHAD (RPARM, ISUB, SHMIN, CTMIN, ANTBAD, BASBAD)
C-----------------------------------------------------------------------
C   Finds shadowed data.  If an antenna is shadowed by any other antenna
C   it is flagged.
C   Inputs:
C      RPARM    R(*)   Random parameters - uses time
C      ISUB     I      Subarray number
C      SHMIN    R      Min baseline allowed shadowing
C      CTMIN    R      Min baseline allowed cross talk
C   Outputs:
C      ANTBAD   L(*)   T => antenna shadowed
C      BASBAD   L(*)   T => baseline cross talk
C-----------------------------------------------------------------------
      REAL      RPARM(*), SHMIN, CTMIN
      LOGICAL   ANTBAD(*), BASBAD(*)
      INTEGER   ISUB
C
      INTEGER   IA1, IA2, LSUB, IBL
      REAL      UV2
      DOUBLE PRECISION X(2), Y(2), Z(2), BX, BY, BZ, B1, B2, GH, U, V,
     *   W, JDREF, GMST, GAST, RATE, HAI, GSEC, HTR, ARRLON
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE LSUB, JDREF
      DATA LSUB /-1/
      DATA GSEC /1.0027375D0/
C-----------------------------------------------------------------------
      IF (LSUB.NE.ISUB) THEN
         LSUB = ISUB
         CALL JULDAY (RDATE, JDREF)
         END IF
      HTR = PI / 12.D0
      CALL GSTROT (JDREF, GMST, GAST, RATE)
      GAST = GAST * (24.D0/360.D0)
      GAST = GAST + (((RPARM(1+ILOCT)-(LSUB-1)*5)*24.D0) * GSEC)
      GAST = MOD (GAST, 24.D0)
      CALL LFILL (NSTNS, .FALSE., ANTBAD)
C                                              Calculate array longitude
C                                              Array BX= ARRAYC(1)
C                                              Array BY= ARRAYC(2)
C                                              Array BZ= ARRAYC(3)
      ARRLON = 0.0D0
      IF ((ABS(ARRAYC(1)).GT.1.D2) .AND. (ABS(ARRAYC(2)).GT.1.D2) .AND.
     *      (ABS(ARRAYC(3)).GT.1.D2))
     *      ARRLON = ATAN2 (ARRAYC(2), ARRAYC(1))
      DO 100 IA1 = 1,NSTNS-1
         X(1) = STNX(IA1)
         Y(1) = STNY(IA1)
         Z(1) = STNZ(IA1)
         DO 90 IA2 = IA1+1,NSTNS
            IF ((IA1.EQ.11) .AND. (IA2.EQ.28)) THEN
               MSGTXT = 'WE ARE HERE'
               END IF
            IBL = (IA1-1) * NSTNS + IA2
            BASBAD(IBL) = .FALSE.
            X(2) = STNX(IA2)
            Y(2) = STNY(IA2)
            Z(2) = STNZ(IA2)
            IF (((X(1).GT.1.D2) .OR. (Y(1).GT.1.0D2) .OR.
     *         (Z(1).GT.1.0D2)) .AND. ((X(2).GT.1.0D2) .OR.
     *         (Y(2).GT.1.0D2) .OR. (Z(2).GT.1.0D2))) THEN
               CALL BASLIN (2, X, Y, Z, CATD(KDCRV+JLOCF), ARRLON,
     *            BX, BY, BZ, B1, B2, GH)
               HAI = GAST*HTR - RAEPO - GH
               U = B2 * SIN(HAI)
               V = B1 * COS(DECEPO) - B2 * SIN(DECEPO) * COS(HAI)
               W = B1 * SIN(DECEPO) + B2 * COS(DECEPO) * COS(HAI)
               UV2 = SQRT (U*U + V*V)
               IF (UV2.LT.SHMIN) THEN
                  IF (W.GT.0.0) THEN
                     ANTBAD(IA2) = .TRUE.
                  ELSE
                     ANTBAD(IA1) = .TRUE.
                     END IF
               ELSE IF (UV2.LT.CTMIN) THEN
                  BASBAD(IBL) = .TRUE.
                  END IF
               END IF
 90         CONTINUE
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SOURNU (SOURCE, KUAL, CALKOD, NSOUR, DISK, CNO, NID,
     *   BUFFER, ID, IRET)
C-----------------------------------------------------------------------
C   Determines the source numbers of a list of source names from the
C   source table associated with a specified catalog entry.
C   Inputs:
C      SOURCE  C*16(*)   List of source names.
C                        If the first character of any source names
C                        begins with a "-", all sources EXCEPT those
C                        named will be returned ( the "-" will be
C                        ignored in determining the source name).
C                        Blank source names are ignored.  Names should
C                        be left justified, blank filled
C      KUAL      I       SOURCE qualifier, .lt. 0 => any.
C      CALKOD    C*4     restrict to matching this calcode.
C      NSOUR     I       Number of entries in SOURCE, may include
C                        blank names.
C      DISK      I       Disk number of the data set.
C      CNO       I       Catalog slot number of data set.
C   Input/Output:
C      NID       I       On input the maximum number of elements to be
C                        filled in ID; on output, the number of elements
C                        in ID. 0 => all selected.
C      BUFFER    I(512)  Work buffer, used for I/O and manipulating
C                        source lists, should be at least min (512,NID)
C   Output:
C      ID        I(*)    Source ID numbers of selected sources,
C                        If ID(1)=0 then all sources are selected.
C      IRET      I       Return code. 0 => OK; else failed.
C                           -1 => source/qual specified and not found
C                                 no message is generated
C   Usage notes:
C       This routine uses AIPS LUN 27 which will be closed on normal
C   return.   Version 1 of the source table is assumed.
C   LOCAL VERSION INCLUDES CALKOD
C-----------------------------------------------------------------------
      CHARACTER SOURCE(*)*16, CALKOD*4
      INTEGER   KUAL, NSOUR, DISK, CNO, NID, BUFFER(*), ID(*), IRET
C
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INTEGER   CAT(256), VER, LUN, IDKOL, SUKOL, I, IDSOU, SQUAL, J,
     *   MAXID, NUMIF, ISURNO, NUMREC, I4, SUFQID
      LOGICAL   EQUAL, DESEL, ALLSEL, GOTIT, ALLBLN
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RAOBS, DECOBS
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC)
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IDKOL, SUKOLS(1)),   (SUKOL, SUKOLS(2))
      DATA VER, LUN /1, 27/
C-----------------------------------------------------------------------
C                                       Setup
      MAXID = NID
      NID = 0
      IRET = 0
C                                       Check if sources deselected or
C                                       if all sources are selected.
      DESEL = .FALSE.
      ALLSEL = .TRUE.
      ALLBLN = .TRUE.
      DO 10 I = 1,NSOUR
C                                       Check deselection
         DESEL = DESEL .OR. ((SOURCE(I)(1:1).EQ.'-') .AND.
     *      (SOURCE(I)(2:).NE.' '))
C                                       Check if all blank (GvM, 1/93)
         ALLBLN = ALLBLN .AND. (SOURCE(I).EQ.' ')
C                                       Check if all blank, no qual
         ALLSEL = ALLSEL .AND. (SOURCE(I).EQ.' ') .AND. (KUAL.LT.0)
     *      .AND. (CALKOD.EQ.' ')
 10      CONTINUE
C                                       Check all selected case.
      ID(1) = 0
      IF (ALLSEL) GO TO 999
C                                       Get catalog header.
      CALL CATIO ('READ', DISK, CNO, CAT, 'REST', BUFFER, IRET)
      IF ((IRET.GT.0) .AND. (IRET.LT.5)) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Initialize SOURCE table.
      CALL SOUINI ('READ', BUFFER, DISK, CNO, VER, CAT, LUN, NUMIF,
     *   VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
         END IF
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
C                                       Sources selected.
            IF (.NOT.DESEL) THEN
               EQUAL = (SOURCE(J).EQ.SOUNAM) .OR. ALLBLN
C                                       is qualifier OK, too?
               EQUAL = EQUAL .AND. ((KUAL.LT.0) .OR.
     *            (KUAL.EQ.SQUAL))
C                                       calcode
               IF (CALKOD.NE.' ') EQUAL = EQUAL .AND.
     *            ((CALKOD.EQ.CALCOD) .OR.
     *            ((CALKOD.EQ.'*') .AND. (CALCOD.NE.' ')) .OR.
     *            ((CALKOD.EQ.'-CAL') .AND. (CALCOD.EQ.' ')))
               IF (EQUAL) THEN
                  IF ((NID+1).GT.MAXID) GO TO 310
                  NID = NID + 1
                  ID(NID) = IDSOU
                  GO TO 320
                  END IF
C                                       Deselected
            ELSE
C                                       Check for leading "-"
               IF (SOURCE(J)(1:1).EQ.'-') THEN
                  EQUAL = SOURCE(J)(2:16).EQ.SOUNAM(1:15)
               ELSE
                  EQUAL = SOURCE(J).EQ.SOUNAM
                  END IF
C                                       Check QUAL
               EQUAL = EQUAL .AND. ((KUAL.LT.0) .OR.
     *            (KUAL.EQ.SQUAL))
C                                       calcode
               IF (CALKOD.NE.' ') EQUAL = EQUAL .AND.
     *            ((CALKOD.EQ.CALCOD) .OR.
     *            ((CALKOD.EQ.'*') .AND. (CALCOD.NE.' ')) .OR.
     *            ((CALKOD.EQ.'-CAL') .AND. (CALCOD.EQ.' ')))
               GOTIT = GOTIT .OR. EQUAL
               END IF
 300        CONTINUE
C                                       Source not deselected
         IF (DESEL .AND. (.NOT.GOTIT)) THEN
            IF ((NID+1).GT.MAXID) GO TO 310
            NID = NID + 1
            ID(NID) = IDSOU
            END IF
         GO TO 320
C                                       Too many sources selected
 310     CONTINUE
            WRITE (MSGTXT,1300) MAXID
            IRET = 5
            GO TO 990
 320     CONTINUE
 500     CONTINUE
C                                       Close Source table
      CALL TABIO ('CLOS', 0, I4, SOUNAM, BUFFER, IRET)
      IF (NID.EQ.0) IRET = -1
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SOURNU: ERROR ',I3,' READING CATBLK FOR SOURCE TABLE')
 1100 FORMAT ('SOURNU: ERROR ',I3,' INITIALIZING SOURCE TABLE')
 1120 FORMAT ('SOURNU: ERROR ',I3,' READING SOURCE TABLE')
 1300 FORMAT ('SOURNU: MORE SOURCES SELECTED THAN MAX (',I5,')')
      END
