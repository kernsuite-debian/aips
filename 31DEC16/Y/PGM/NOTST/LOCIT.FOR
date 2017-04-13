LOCAL INCLUDE 'LOCIT.INC'
      INCLUDE 'INCS:PUVD.INC'
C
      INTEGER   MAXPTS, MAXDAT, MAXPLT
      PARAMETER (MAXPTS=50000)
      PARAMETER (MAXDAT=MAXPTS*2*MAXIF)
      PARAMETER (MAXPLT=4)
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOURC(4,30), XLPNAM(12), XSTOKE
      REAL      XUSER, XSIN, XDISIN, XVER, XTIME(8), XBAND, XFREQ,
     *   XFQID, XBIF, XEIF, XANT(50), XSUBA, XREFA, DPARM(10), XPRTLV,
     *   BPARM(10), XDOTV, XGRCH, XBADD(10)
C
      DOUBLE PRECISION  FREQIF(MAXIF), BFREQ
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16
      LOGICAL   ISINGL, DOPRT, DOTV, DOOTT
      INTEGER   DISKIN, SEQIN, CNOIN, INVER, BUFFER(512), REFANT,
     *   ANTLAB(MAXANT), TIMKOL, SUBKOL, ANTKOL, SOUKOL, IFRKOL, IFLKOL,
     *   LUNP(4), INDP(4), NUMIFS, GRCHAN, NPLOTS, NPARMS
      DOUBLE PRECISION HA(MAXPTS), DE(MAXPTS), AZ(MAXPTS), EL(MAXPTS),
     *   TI(MAXPTS)
      REAL      PHASES(MAXDAT), OLDBX(MAXANT), OLDBY(MAXANT),
     *   OLDBZ(MAXANT), OLDBK(MAXANT), SIGBX(MAXANT), SIGBY(MAXANT),
     *   SIGBZ(MAXANT)
      CHARACTER SOURNM(MAXPTS)*8
C
      COMMON /INPARM/ XUSER, XNAMEI, XCLAIN, XSIN, XDISIN, XVER,
     *   XSOURC, XTIME, XBAND, XFREQ, XFQID, XBIF, XEIF, XSTOKE, XANT,
     *   XSUBA, XREFA, DPARM, XPRTLV, XLPNAM, BPARM, XDOTV, XGRCH,
     *   XBADD
      COMMON /CHPARM/ NAMEIN, CLAIN, XSOUR, SOURNM
      COMMON /INFOLS/ FREQIF, BFREQ, ISINGL, DISKIN, SEQIN, CNOIN,
     *   INVER, BUFFER, ANTLAB, REFANT, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *   IFRKOL, IFLKOL, DOPRT, LUNP, INDP, NUMIFS, GRCHAN, NPLOTS,
     *   NPARMS, DOTV, DOOTT, OLDBX, OLDBY, OLDBZ, OLDBK, SIGBX, SIGBY,
     *   SIGBZ
      COMMON /DATLOC/ HA, DE, AZ, EL, TI, PHASES
C
      INCLUDE 'INCS:DSEL.INC'
LOCAL END
LOCAL INCLUDE 'DANTE.INC'
C                                       Include for antenna info common,
C                                       filled in by ANINFO.
C                                       is DANS w STNOffset added
      INTEGER   NSTNS, MNTYP(MAXANT), FQIDAN, TELNO(MAXANT)
      REAL      ANTIAT, ANTUTC, POLARX, POLARY, STNELP(2,MAXIF,MAXANT),
     *   STNORI(2,MAXIF,MAXANT)
      CHARACTER STNNAM(MAXANT)*8, STNPST*8, TIMLAB*8
      DOUBLE PRECISION STNX(MAXANT), STNY(MAXANT), STNZ(MAXANT),
     *   STNO(MAXANT), STNLAT(MAXANT), STNLON(MAXANT), STNRAD(MAXANT),
     *   CNTRX, CNTRY, CNTRZ, GSTIAT, ROTIAT, ARRLON
      COMMON /STATNS/ STNX, STNY, STNZ, STNO, STNLAT, STNLON, STNRAD,
     *   CNTRX, CNTRY, CNTRZ, ARRLON, GSTIAT, ROTIAT, ANTIAT, ANTUTC,
     *   POLARX, POLARY, STNELP, STNORI, NSTNS, MNTYP, FQIDAN, TELNO
      COMMON /STACHR/ STNNAM, STNPST, TIMLAB
C                                                          End DANTE.
LOCAL END
      PROGRAM LOCIT
C-----------------------------------------------------------------------
C! LOCIT finds antenna location corrections from phases in an SN table
C# Calibration UV VLA
C-----------------------------------------------------------------------
C;  Copyright (C) 2002-2012, 2016
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
C   Task LOCIT converts phases in an SN table into antenna location
C   corrections.
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      USERID         USERID        User number.
C      INNAME         NAME          File name to be listed.
C      INCLASS        CLASS         File class to be listed.
C      INSEQ          SEQ           File sequence number.
C      INDISK         DISK          Disk volumn on which file resides.
C      INVER          INVER         SN or CL table version
C      SOURCES        XSOUR(4,30)   Sources selected
C      TIMERANG       XTIME(8)      Timerange
C      SELBAND
C      SELFREQ
C      FREQID
C      BIF            BIF           IF number - low
C      EIF            EIF           IF number - high
C      ANTENNAS       XANT(50)      Antenna numbers
C      SUBARRAY       SUBARR        Subarray
C      REFANT         REFANT        reference antenna
C      DPARM          DPARM         control parameters
C      PRTLEV         PRTLEV        debug print level
C      OUTPRINT
C      BADDISK
C-----------------------------------------------------------------------
      INCLUDE 'LOCIT.INC'
      CHARACTER PRGM*6, PADNAM(MAXANT)*8
      INTEGER  IRET, IERR, CURANT, NPTS, NSAMP(MAXANT), I, IPLOT, JJ(4),
     *   JNP(4)
      DOUBLE PRECISION OLDPOS(4,MAXANT)
      REAL     PANS(4,MAXANT), PERR(4,MAXANT), PRMS(MAXANT),
     *   PDIF(MAXDAT,MAXPLT)
      LOGICAL   LAST
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'LOCIT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL LOCIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
      IPLOT = 0
      DOOTT = .TRUE.
C                                       Loop over antennas
      DO 10 I = 1,NANTSL
         CURANT = ANTLAB(I)
         IF (CURANT.NE.REFANT) THEN
            IF (REFANT.GT.0) THEN
               WRITE (MSGTXT,1000) CURANT, REFANT
            ELSE
               WRITE (MSGTXT,1001) CURANT
               END IF
            CALL MSGWRT (3)
            NSAMP(I) = 0
            PADNAM(I) = ' '
            CALL LOCDAT (CURANT, NPTS, NSAMP(I), IRET)
            IF (IRET.NE.0) NSAMP(I) = 0
            IF (IRET.EQ.0) CALL LOCFIT (CURANT, NPTS, NSAMP(I),
     *         OLDPOS(1,I), PANS(1,I), PERR(1,I), PRMS(I), PADNAM(I),
     *         PDIF(1,IPLOT+1), IRET)
            IF (IRET.GT.0) GO TO 990
            IF ((NPLOTS.GT.0) .AND. (IRET.EQ.0)) THEN
               IPLOT = IPLOT + 1
               JJ(IPLOT) = CURANT
               JNP(IPLOT) = NPTS
               IF (IPLOT.EQ.NPLOTS) THEN
                  LAST = I.EQ.NANTSL
                  CALL LOCPLT (IPLOT, JJ, JNP, PDIF, LAST, IRET)
                  IF (IRET.NE.0) GO TO 990
                  IPLOT = 0
                  END IF
            ELSE
               IRET = 0
               END IF
            END IF
 10      CONTINUE
      IF (IPLOT.GT.0) THEN
         LAST = .TRUE.
         CALL LOCPLT (IPLOT, JJ, JNP, PDIF, LAST, IRET)
         END IF
      CALL LOCSUM (NSAMP, OLDPOS, PANS, PERR, PRMS, PADNAM)
      IF (BPARM(9).GT.0.0) CALL LOCSPL (NSAMP, PADNAM)
C                                       Close down files, etc.
 990  DO 995 I = 1,4
         IF (INDP(I).GT.0) CALL ZTXCLS (LUNP(I), INDP(I), IERR)
 995     CONTINUE
C
      CALL DIE (IRET, BUFFER)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Solving for antenna',I3,' with reference antenna',I3)
 1001 FORMAT ('Solving for antenna',I3,', no reference antenna needed')
      END
      SUBROUTINE LOCIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   LOCIN gets input parameters for LOCIT, initializes AN, SU info
C   Inputs:
C      PRGN    C*6       Program name (2 chars/word)
C   Output:
C      IRET    I         Error code: 0 => ok
C                           5 => catalog troubles
C                           8 => can't start
C   Commons:
C      /INPARM/ all input adverbs in order given by INPUTS file
C      /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      INCLUDE 'LOCIT.INC'
      CHARACTER STAT*4, UTYPE*2, KEYS(6)*12, LPBASE*48, LPNAME*52,
     *   ATIME*8, ADATE*12
      INTEGER   IROUND, IERR, I, LUN, NIF, IIVER, MXIF, SMOTYP,
     *   NKEY, NREC, NCOL, COLS(6), ISBAND(MAXIF), DATP(128,2), RECNO,
     *   ITRIM, ILEN, PLUN(4), ITIME(3), IDATE(3), NUMST
      REAL      CATR(256), FINC(MAXIF)
      LOGICAL   T, F, TABLE, EXIST, FITASC, MATCH
      HOLLERITH CATH(256)
      DOUBLE PRECISION FOFF(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATR, CATBLK, CATH)
      DATA T, F /.TRUE.,.FALSE./
      DATA KEYS /'TIME', 'SUBARRAY', 'ANTENNA NO.', 'SOURCE ID',
     *   'REAL1', 'REAL2'/
      DATA PLUN /1,3,10,11/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
      CALL FILL (4, 0, INDP)
      CALL COPY (4, PLUN, LUNP)
C                                       Get input parameters.
      NPARMS = 240
      CALL GTPARM (PRGN, NPARMS, RQUICK, XUSER, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFFER, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
      XUSER = NLUSER
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XSTOKE, STOKES)
      DO 25 I = 1,30
         CALL H2CHR (16, 1, XSOURC(1,I), XSOUR(I))
 25      CONTINUE
      DO 30 I = 3,8
         DPARM(I) = DPARM(I) * DG2RAD
 30      CONTINUE
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      INVER = IROUND (XVER)
      REFANT = IROUND (XREFA)
      IF (DPARM(1).GT.0) REFANT = -1
      GRCHAN = XGRCH + 0.5
      NPLOTS = IROUND (BPARM(1))
      IF (NPLOTS.GT.MAXPLT) NPLOTS = MAXPLT
      DOTV = (XDOTV.GT.0.0) .OR. ((NPLOTS.LE.0) .AND. (BPARM(9).LE.0.0))
      IF (BPARM(5).LE.0.1) BPARM(5) = 1
C                                       Get CATBLK.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      STAT = 'WRIT'
      IF (DOTV) STAT = 'READ'
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
      IF (STAT.EQ.'WRIT') FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      BFREQ = FREQ
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DO 60 I = 1,30
         SOURCS(I) = XSOUR(I)
 60      CONTINUE
      CALL RCOPY (8, XTIME, TIMRNG)
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         BIF = 1
         EIF = 1
         END IF
      IF (DPARM(9).GT.0.0) EIF = BIF
      NUMIFS = EIF - BIF + 1
      NUMST = CATBLK(KINAX+JLOCS)
      NUMST = MAX (1, MIN (2, NUMST))
C                                       Antennas
      DO 70 I = 1,50
         ANTENS(I) = IROUND (XANT(I))
 70      CONTINUE
      DOAPPL = F
      SMOTYP = 0
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      DO 80 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 80      CONTINUE
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
C                                       Get IF freq offset.
      IIVER = 1
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, IIVER, CATBLK, LUN,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.GT.0) GO TO 999
      MXIF = MAXIF
      DO 90 I = 1,MXIF
         FREQIF(I) = FOFF(I)
 90     CONTINUE
C                                       See if a single source file.
      CALL MULSDB (CATBLK, ISINGL)
      IF (ISINGL) THEN
         CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUN, BUFFER, TABLE, EXIST,
     *      FITASC, IERR)
         ISINGL = EXIST .AND. (IERR.NE.0)
         END IF
      ISINGL = .NOT.ISINGL
      CALL H2CHR (8, 1, CATH(KHOBJ), SNAME)
C                                       Get antenna info for
C                                       parallactic angle or elevation
      CALL ANINFO (DISKIN, CNOIN, SUBARR, CATBLK, BUFFER, IRET)
C                                       Initialize source tables
C                                       Timerange
      IF (TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4).EQ.0.0)
     *   TIMRNG(1) = -1.0E6
      IF (TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8).EQ.0.0)
     *   TIMRNG(5) = 1.0E6
C                                       Set time range.
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      IUDISK = DISKIN
      IUCNO = CNOIN
      IULUN = 25
      IXLUN = 28
      ICLUN = 29
      IFLUN = 30
      CALL COPY (256, CATBLK, CATUV)
      KLOCSU = ILOCSU
      QUAL = 0
      CALCOD = ' '
      CURSOU = -1
C                                       source info
      CALL SOUFIL (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       interpret ANTENNAS
      CALL ANTSET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                 Use TABINI for column
C                                 pointer array
      NKEY = 0
      NREC = 0
      NCOL = 0
      CALL TABINI ('READ', 'SN', DISKIN, CNOIN, INVER, CATBLK, LUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
C                              Find column numbers,
      NCOL = 4 + NUMST
      CALL FNDCOL (NCOL, KEYS, 12, T, BUFFER, COLS, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'SN TABLE MISSING COLUMNS!!'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      TIMKOL = DATP(COLS(1),1)
      SUBKOL = DATP(COLS(2),1)
      ANTKOL = DATP(COLS(3),1)
      SOUKOL = DATP(COLS(4),1)
      IFRKOL = DATP(COLS(5),1)
      IF (NCOL.EQ.6) THEN
         IFLKOL = DATP(COLS(6),1)
      ELSE
         IFLKOL = -1
         END IF
C                              Close
      CALL TABIO ('CLOS', 0, RECNO, BUFFER, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open output device
      CALL H2CHR (48, 1, XLPNAM, LPBASE)
      ILEN = ITRIM (LPBASE)
      DOPRT = ILEN.GT.2
      IF (DOPRT) THEN
         DEVTAB(1) = 3
         CALL ZTIME (ITIME)
         CALL ZDATE (IDATE)
         CALL TIMDAT (ITIME, IDATE, ATIME, ADATE)
         WRITE (MSGTXT,1090) ADATE, ATIME
         LUNP(1) = PLUN(1)
         LUNP(2) = PLUN(2)
         INDP(2) = 0
         LPNAME = LPBASE(:ILEN) // '.FIT'
         CALL ZTXOPN ('WRIT', LUNP, INDP, LPNAME, .TRUE., IRET)
         MSGTXT = '------------------------------------------' //
     *      '----------------------'
         IF (IRET.EQ.0) CALL ZTXIO ('WRITE', LUNP, INDP, MSGTXT(:64),
     *      IRET)
         WRITE (MSGTXT,1090) ADATE, ATIME
         IF (IRET.EQ.0) CALL ZTXIO ('WRITE', LUNP, INDP, MSGTXT(:64),
     *      IRET)
         MSGTXT = '------------------------------------------' //
     *      '----------------------'
         IF (IRET.EQ.0) CALL ZTXIO ('WRITE', LUNP, INDP, MSGTXT(:64),
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            IRET = 0
            DOPRT = .FALSE.
            GO TO 990
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LOCIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1090 FORMAT (2('----------'),2X,A,A,2X,2('----------'))
 1100 FORMAT ('LOCIN: ERROR ',I3,' OPENING OUTPUT TEXT FILE')
      END
      SUBROUTINE LOCDAT (CURANT, NPTS, NSAMP, IRET)
C-----------------------------------------------------------------------
C   LOCDAT fills the data and associated arrays with the SN phases (w
C   refant phase subtracted and scaled for frequency) for antenna
C   CURANT.
C   Input:
C      CURANT   I      Antenna number desired
C   Output:
C      NPTS     I      Number of times found
C      NSAMP    I      Total number of samples
C      IRET     I      Error code: > 0 -> quit
C   Gustaaf's notes on the OT table:
C   The OT table looks very much like the CL table.  It has an entry at
C   the beginning and the end of every scan.  If the scan is long
C   enough, it has one or more in the middle as well.  BASUV reads an SN
C   entry, and then goes on to look for the OT entry that is closest to
C   it.  This guarantees that the OT entry applies to the same scan.
C   When NEWSN is TRUE, this means the proper OT entry was found and we
C   can go on to read a new SN entry.  If NEWSN is FALSE, we read the
C   next OT entry and stay with the current SN entry.
C-----------------------------------------------------------------------
      INTEGER   CURANT, NPTS, NSAMP, IRET
C
      INCLUDE 'LOCIT.INC'
      INTEGER   LUN, KOLS(MAXCLC), NUMV(MAXCLC), OTLUN, OTBUF(512),
     *   JERR, NUMNOD, RECNO, LSAMP, MAXOTT, I
      LOGICAL   OTOP, OTP, LAST, NEWSN, ISAPPL, OKAY
      REAL      TOTT, TOTP, TIM
      DOUBLE PRECISION RANOD(25), DECNOD(25), TS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DOTTV.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUN, OTLUN /30, 27/
C-----------------------------------------------------------------------
C                                       Open SN table
C                                       Re-open
      CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, INVER, CATBLK, LUN,
     *   RECNO, KOLS, NUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NUMPOL.EQ.2) THEN
         IF ((STOKES.EQ.'RR') .OR. (STOKES.EQ.'R')) THEN
            NUMPOL = 1
         ELSE IF ((STOKES.EQ.'LL') .OR. (STOKES.EQ.'L')) THEN
            NUMPOL = 1
            IFRKOL = IFLKOL
            END IF
         END IF
C                                       Open OT table
      IF (DOOTT) THEN
         CALL OTTINI ('READ', OTBUF, DISKIN, CNOIN, 1, CATBLK, OTLUN,
     *      IRET)
         IF (IRET.NE.0) THEN
            DOOTT = .FALSE.
            MSGTXT = 'Error accessing OT file, will NOT use OT' //
     *         ' information'
            CALL MSGWRT (6)
C                                       Read first OT entry -> TIME8,
C                                       OTT
         ELSE
            MAXOTT = OTBUF(5)
            CALL TABOTT ('READ', OTBUF, IRET)
C                                       TOTT/TOTP: current/previous
C                                       OTT/OTP  : current/previous
            IF (IRET.EQ.0) THEN
               TOTT = TIME8
               TOTP = TOTT - 10.0
               OTP  = OTT
            ELSE
               DOOTT = .FALSE.
               MSGTXT = 'Error accessing OT file, will NOT use OT' //
     *            ' information'
               CALL MSGWRT (6)
               END IF
            END IF
         END IF
C                                       init values to get new SN
      NEWSN = .TRUE.
      LAST  = .FALSE.
      NPTS = 0
      OTOP = .FALSE.
      CALL RFILL (MAXDAT, 0.0, PHASES)
C                              Read new SN entry.  Not wanted when
C                              searching for proper OT entry
 100  IF (NEWSN) THEN
         LSAMP = NSAMP
         CALL RDSN (CURANT, NPTS, NSAMP, TIM, RECNO, IRET)
         IF (IRET.EQ.-2) GO TO 200
         IF (IRET.GT.0)  GO TO 999
         LAST = IRET.EQ.-1
         IRET = 0
         END IF
C
      IF (DOOTT) THEN
C                              OT entry after SN entry?
 110     NEWSN = (TOTT - TIM).GT.0.0
C                              find out which OT entry to use. If
C                              previous one, no need to read table
         IF (NEWSN) THEN
            IF ((TOTT-TIM).LT.(TIM-TOTP)) THEN
C                              if current one, this becomes previous
C                              one; read new current one
               TOTP = TOTT
               OTP  = OTT
               IF (IOTRNO.LE.MAXOTT) THEN
                  CALL TABOTT ('READ', OTBUF, IRET)
                  IF (IRET.GT.0) GO TO 999
                  TOTT = TIME8
               ELSE
                  TOTT = 1.E10
                  END IF
               END IF
C                                       value to use is whatever is OTP
C                                       now
            OTOP = OTP
C                                       TOTT <= TIM; read next OT entry
         ELSE
            TOTP = TOTT
            OTP  = OTT
            IF (IOTRNO.LE.MAXOTT) THEN
               CALL TABOTT ('READ', OTBUF, IRET)
               IF (IRET.GT.0) GO TO 999
               TOTT = TIME8
            ELSE
               TOTT = 1.E10
               END IF
            END IF
C                                       go back to test this new OT
C                                       entry
         IF (.NOT.NEWSN) GO TO 110
C                                       if no OTT newsn always
      ELSE
         NEWSN = .TRUE.
         END IF
C                                       Position information
      IF (OTOP) THEN
         EL(NPTS) = PI - EL(NPTS)
         IF (AZ(NPTS).LE.PI) THEN
            AZ(NPTS) = AZ(NPTS) + PI
         ELSE
            AZ(NPTS) = AZ(NPTS) - PI
            END IF
         END IF
      OKAY = .TRUE.
      IF (DPARM(4).GT.DPARM(3)) OKAY = (EL(NPTS).GE.DPARM(3)) .AND.
     *   (EL(NPTS).LE.DPARM(4))
      IF (DPARM(6).GT.DPARM(5)) OKAY = OKAY .AND. (HA(NPTS).GE.DPARM(5))
     *   .AND. (HA(NPTS).LE.DPARM(6))
      IF (DPARM(8).GT.DPARM(7)) OKAY = OKAY .AND. (DE(NPTS).GE.DPARM(7))
     *   .AND. (DE(NPTS).LE.DPARM(8))
      IF (.NOT.OKAY) THEN
         NPTS = NPTS - 1
         NSAMP = LSAMP
         END IF
C                                       loop for more data
      IF (.NOT.LAST) GO TO 100
C                                       Done for now
 200  IF (DOOTT) CALL TABOTT ('CLOS', OTBUF, JERR)
      IF (NPTS.LE.0) THEN
         IRET = -1
      ELSE IF (IRET.EQ.-2) THEN
         IRET = 0
         END IF
C                                       center up the times
      IF (NPTS.GT.0) THEN
         TS = 0.0D0
         DO 210 I = 1,NPTS
            TS = TS + TI(I)
 210        CONTINUE
         TS = TS / NPTS
         DO 220 I = 1,NPTS
            TI(I) = TI(I) - TS
 220        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE RDSN (CURANT, NPTS, NSAMP, TIM, RECNO, IRET)
C-----------------------------------------------------------------------
C   RDSN reads the next time sample for the desired antenna and the
C   reference antenna from the SN table, differencing the phases.
C   Inputs:
C      CURANT   I   Desired antenna
C   In/Out:
C      NPTS     I   Number times now in array
C      NSAMP    I   Number samples now in array
C      RECNO    I   Next record number in SN table
C   Output:
C      TIM      R   Time of latest sample
C      IRET     I   Error code: 0 okay, > 0 die,
C                   -1 last sample, -2 end of file with no sample
C-----------------------------------------------------------------------
      INTEGER   CURANT, NPTS, NSAMP, RECNO, IRET
      REAL      TIM
C
      INCLUDE 'LOCIT.INC'
      INTEGER   I, LSOU, IANT, ISUB, ISOU, NUMREC, SUNUM, KOL, JOFF,
     *   IOFF, RECORD(XCLRSZ), ITIM(4), IPOL, IIF, JERR, ISLUN, NGOOD
      REAL      XRE, XIM, RECR(XCLRSZ), RPHASE(2,MAXIF), DT, XRE2, XIM2
      LOGICAL   GOTS, GOTR, NUSCAN, GOOD
      DOUBLE PRECISION LST, TIMTAB, RECD(XCLRSZ/2), FRMUL(MAXIF)
      EQUIVALENCE (RECORD, RECR, RECD)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'DANTE.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA ISLUN /26/
C-----------------------------------------------------------------------
      GOTS = .FALSE.
      GOTR = REFANT.LE.0
      LSOU = -10
      TIM = 1.0E10
C                                       Use 2 sec tolerance
      DT = 2.0 / 86400.0
C                                       Save scan number (0= no index)
      IF (RECNO.LT.1) RECNO = 1
C                                       Get number of records
      NUMREC = BUFFER(5)
 10   IF (NUMREC.GE.RECNO) THEN
         CALL TABIO ('READ', 0, RECNO, RECORD, BUFFER, IRET)
C                                       Check if flagged
         IF (IRET.LT.0) THEN
            RECNO = RECNO + 1
            GO TO 10
            END IF
C                                       Error
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       Extract parameters
         TIMTAB = RECD(TIMKOL)
         IANT = RECORD(ANTKOL)
         ISUB = RECORD(SUBKOL)
         ISOU = RECORD(SOUKOL)
C                                       Check if avg. or scan done
         IF (((CURSOU.NE.ISOU) .AND. (ILOCSU.GT.0) .AND. (GOTS) .AND.
     *      (GOTR)) .OR. (TIMTAB.GT.(TIM+DT))) GO TO 100
C                                       Time
         RECNO = RECNO + 1
         IF (TIMTAB.LT.TSTART) GO TO 10
         IF (TIMTAB.GT.TEND) THEN
            RECNO = NUMREC + 1
            GO TO 100
            END IF
C                                       Check subarray
         IF ((ISUB.NE.SUBARR) .AND. (ISUB.NE.0)) GO TO 10
C                                       Check antenna
         IF ((IANT.NE.CURANT) .AND. (IANT.NE.REFANT)) GO TO 10
C                                       Check source
         IF (NSOUWD.GT.0) THEN
            DO 20 I = 1,NSOUWD
               IF (ISOU.EQ.SOUWAN(I)) THEN
                  IF (DOSWNT) GO TO 25
                  GO TO 10
                  END IF
 20            CONTINUE
C                                       Not wanted
            IF (DOSWNT) GO TO 10
            END IF
C                                       Found selected datum:
 25      IF ((IANT.EQ.CURANT) .OR. (.NOT.GOTS)) THEN
C                                       Source no.
            IF (.NOT.GOTS) NUSCAN = CURSOU.NE.ISOU
            CURSOU = ISOU
            SUNUM = CURSOU
C                                       GETSOU needed for SOUGEO
            IF (CURSOU.NE.LSOU) THEN
               CALL GETSOU (CURSOU, IUDISK, IUCNO, CATUV, ISLUN, JERR)
               DO 30 IIF = BIF,EIF
                  FRMUL(IIF) = TWOPI * (1.0D0 + (FREQO(IIF) +
     *               FREQIF(IIF)) / FREQ)
 30               CONTINUE
               IF (DPARM(9).GT.0.0) THEN
                  BFREQ = FREQO(BIF+1) + FREQIF(BIF+1) - FREQO(BIF) -
     *               FREQIF(BIF)
                  FRMUL(BIF) = TWOPI
                  END IF
               END IF
            LSOU = CURSOU
C                                       Source elevation
            CALL SOUGEO (IANT, TIMTAB, DE(NPTS+1), HA(NPTS+1),
     *         EL(NPTS+1), AZ(NPTS+1))
            TI(NPTS+1) = TIMTAB
C                                       Time
            IF (TIM.GT.1.0E9) TIM = TIMTAB
            LST = GSTIAT + STNLON(IANT) * 1.002738D0 + TIMTAB * ROTIAT
            END IF
C                                       current antenna
         IF (IANT.EQ.CURANT) THEN
            GOTS = .FALSE.
C                                       fill in data array
C                                       scale phases to FREQ
            NGOOD = 0
            DO 40 IPOL = 1,NUMPOL
               KOL = IFRKOL
               JOFF = NPTS * NUMPOL * NUMIFS + 1
               IF (IPOL.EQ.2) THEN
                  KOL = IFLKOL
                  JOFF = JOFF + 1
                  END IF
               DO 35 IIF = BIF,EIF
                  IOFF = JOFF + NUMPOL * (IIF - BIF)
                  XRE = RECR(KOL+IIF-1)
                  XIM = RECR(KOL+IIF-1+NUMIF)
                  IF (DPARM(9).LE.0.0) THEN
                     GOOD = (XRE.NE.FBLANK) .AND. (XIM.NE.FBLANK)
                     IF (GOOD) THEN
                        PHASES(IOFF) = ATAN2 (XIM, XRE+1.0E-20) /
     *                     FRMUL(IIF)
                        NGOOD = NGOOD + 1
                     ELSE
                        PHASES(IOFF) = FBLANK
                        END IF
                  ELSE
                     XRE2 = RECR(KOL+IIF)
                     XIM2 = RECR(KOL+IIF+NUMIF)
                     GOOD = (XRE.NE.FBLANK) .AND. (XIM.NE.FBLANK) .AND.
     *                  (XRE2.NE.FBLANK) .AND. (XIM2.NE.FBLANK)
                     IF (GOOD) THEN
                        PHASES(IOFF) = (ATAN2 (XIM2, XRE2+1.0E-20) -
     *                     ATAN2 (XIM, XRE+1.0E-20)) / FRMUL(IIF)
                        IF (PHASES(IOFF).LT.-0.5) PHASES(IOFF) =
     *                     PHASES(IOFF) + 1.0
                        IF (PHASES(IOFF).GT.0.5) PHASES(IOFF) =
     *                     PHASES(IOFF) - 1.0
                        NGOOD = NGOOD + 1
                     ELSE
                        PHASES(IOFF) = FBLANK
                        END IF
                     END IF
                  GOTS = GOTS.OR.GOOD
 35               CONTINUE
 40            CONTINUE
C                                       reference antenna
         ELSE
            GOTR = .FALSE.
C                                       fill in reference array
            DO 60 IPOL = 1,NUMPOL
               KOL = IFRKOL
               IF (IPOL.EQ.2) KOL = IFLKOL
               DO 50 IIF = BIF,EIF
                  XRE = RECR(KOL+IIF-1)
                  XIM = RECR(KOL+IIF-1+NUMIF)
                  IF (DPARM(9).LE.0.0) THEN
                     GOOD = (XRE.NE.FBLANK) .AND. (XIM.NE.FBLANK)
                     IF (GOOD) THEN
                        RPHASE(IPOL,IIF) = ATAN2 (XIM, XRE+1.0E-20) /
     *                     FRMUL(IIF)
                     ELSE
                        RPHASE(IPOL,IIF) = FBLANK
                        END IF
                  ELSE
                     XRE2 = RECR(KOL+IIF)
                     XIM2 = RECR(KOL+IIF+NUMIF)
                     GOOD = (XRE.NE.FBLANK) .AND. (XIM.NE.FBLANK) .AND.
     *                  (XRE2.NE.FBLANK) .AND. (XIM2.NE.FBLANK)
                     IF (GOOD) THEN
                        RPHASE(IPOL,IIF) = (ATAN2 (XIM2, XRE2+1.0E-20) -
     *                     ATAN2 (XIM, XRE+1.0E-20)) / FRMUL(IIF)
                     ELSE
                        RPHASE(IPOL,IIF) = FBLANK
                        END IF
                     END IF
                  GOTR = GOTR.OR.GOOD
 50               CONTINUE
 60            CONTINUE
            END IF
         GO TO 10
         END IF
C                                       Scan done
C                                       But no data
 100  IF ((.NOT.GOTS) .OR. (.NOT.GOTR)) THEN
         IF (RECNO.GT.NUMREC) GO TO 900
         IF (XPRTLV.GT.1.5) THEN
            CALL TODHMS (TIM, ITIM)
            IF (GOTS) THEN
               WRITE (MSGTXT,1100) ITIM
            ELSE IF ((GOTR) .OR. (REFANT.LE.0)) THEN
               WRITE (MSGTXT,1101) ITIM, CURANT
            ELSE
               WRITE (MSGTXT,1102) ITIM, CURANT, REFANT
               END IF
            CALL MSGWRT (6)
            END IF
         GOTS = .FALSE.
         GOTR = REFANT.LE.0
         TIM = 1.0E10
         GO TO 10
         END IF
C                                       Got data - subtract reference
C                                       fill in data array
      IF (REFANT.GT.0) THEN
         GOTS = .FALSE.
         NGOOD = 0
         DO 120 IPOL = 1,NUMPOL
            JOFF = NPTS * NUMPOL * NUMIFS + 1
            IF (IPOL.EQ.2) JOFF = JOFF + 1
            DO 110 IIF = BIF,EIF
               IOFF = JOFF + NUMPOL * (IIF - BIF)
               GOOD = (PHASES(IOFF).NE.FBLANK) .AND.
     *            (RPHASE(IPOL,IIF).NE.FBLANK)
               IF (GOOD) THEN
                  PHASES(IOFF) = PHASES(IOFF) - RPHASE(IPOL,IIF)
                  IF (PHASES(IOFF).LT.-0.5) PHASES(IOFF) = PHASES(IOFF)
     *               + 1.0
                  IF (PHASES(IOFF).GT.0.5) PHASES(IOFF) = PHASES(IOFF)
     *               - 1.0
                  NGOOD = NGOOD + 1
               ELSE
                  PHASES(IOFF) = FBLANK
                  END IF
               GOTS = GOTS.OR.GOOD
 110           CONTINUE
 120        CONTINUE
         IF (.NOT.GOTS) THEN
            CALL TODHMS (TIM, ITIM)
            WRITE (MSGTXT,1120) ITIM, CURANT, REFANT
            CALL MSGWRT (6)
            GOTS = .FALSE.
            GOTR = REFANT.LE.0
            TIM = 1.0E10
            GO TO 10
            END IF
         END IF
C                                       finally raise the count
      NPTS = NPTS + 1
      IF (NPTS.GT.MAXPTS) THEN
         MSGTXT = 'TOO MANY SAMPLES: BUFFERS OVERFLOW'
         IRET = 10
         GO TO 990
         END IF
      NSAMP = NSAMP + NGOOD
      SOURNM(NPTS) = SNAME(:8)
C                                       Get source info
      IF ((KLOCSU.GT.0) .AND. (NUSCAN)) THEN
         CALL GETSOU (SUNUM, IUDISK, IUCNO, CATUV, ISLUN, IRET)
C                                       Didn't find source
         IF (IRET.EQ.11) THEN
            WRITE (MSGTXT,1126) SUNUM
            CALL MSGWRT (8)
            IRET = 0
         ELSE IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1125) IRET
            GO TO 990
         ELSE
            DO 130 IIF = BIF,EIF
               FRMUL(IIF) = (1.0D0 + (FREQO(IIF) + FREQIF(IIF)) / FREQ)
     *            * TWOPI
 130           CONTINUE
            IF (DPARM(9).GT.0.0) THEN
               BFREQ = FREQO(BIF+1) + FREQIF(BIF+1) - FREQO(BIF) -
     *            FREQIF(BIF)
               FRMUL(BIF) = TWOPI
               END IF
            END IF
         END IF
C                                       If end of data, close table
 900  IF (NUMREC.LT.RECNO) THEN
         CALL TABIO ('CLOS', 0, RECNO, RECORD, BUFFER, IRET)
         IRET = -1
C                                       Any thing found?
         IF (.NOT.GOTS) IRET = -2
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('RDSN: TABIO ERROR',I3,' READING GAIN TABLE')
 1100 FORMAT ('TIME',I3,'/',2(I2.2,':'),I2.2,'  MISSING REFANT DATA')
 1101 FORMAT ('TIME',I3,'/',2(I2.2,':'),I2.2,'  MISSING ANT',I3,' DATA')
 1102 FORMAT ('TIME',I3,'/',2(I2.2,':'),I2.2,'  MISSING ANT',2I3,
     *   ' DATA')
 1120 FORMAT ('TIME',I3,'/',2(I2.2,':'),I2.2,'  MISSING ANT',2I3,
     *   ' DIFFERENCE')
 1125 FORMAT ('RDSN: ERROR',I3,' READING SOURCE TABLE')
 1126 FORMAT ('RDSN: SOURCE ',I3,' NOT IN SU TABLE')
      END
      SUBROUTINE ANINFO (DISK, CNO, SUBA, CATBLK, BUFFER, IRET)
C-----------------------------------------------------------------------
C   ANINFO reads an antennas (AN) extension file and saves the informa-
C   tion in a common in DANTE.INC.
C
C   This is a modified version of GETANT. Whereas GETANT does not store
C   the values of STAXOF, ANINFO does, namely in the array STNO in a
C   common in DANTE.INC. DANTE.INC is similar to DANS.INC with the
C   STNO information added.
C
C   NOTE: Uses AIPS LUN 40 for read the antenna file.
C   Inputs:
C      DISK     I        Volume number
C      CNO      I        Catalog slot number
C      SUBA     I        Subarray number (AN version number)
C      CATBLK   I(256)   Catalog header block
C   Outputs sent to COMMON (DANTE.INC):
C      NSTNS    I        Number of antennas
C      STNNAM   C(*)*8   Antenna names
C      STNX     D(*)     X (meters)
C      STNY     D(*)     Y (meters)
C      STNZ     D(*)     Z (meters)
C      STNO     R(*)     offset (meters)
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
C     BUFFER    I(*)     I/O buffer
C     IRET      I        Error code: 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, SUBA, CATBLK(256), BUFFER(*), IRET
C
      CHARACTER CHSOLT*8, CHELP*8
      INTEGER   IVER, LUN, LOOP, NIF, CURANT, LOCS, KEYTYP, JERR, NR, I
      LOGICAL   DOELP
      DOUBLE PRECISION X, Y, Z, RHO, TWOPI, DTOR
      HOLLERITH HSTNPS(2)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'DANTE.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA CHSOLT, CHELP /'POLTYPE ','ORI-ELP'/
      DATA LUN /40/
C-----------------------------------------------------------------------
      TWOPI = 8.0D0 * ATAN (1.0D0)
      DTOR = TWOPI / 360.0D0
      DO 10 I = 1,MAXANT
         TELNO(I) = I
         STNNAM(I) = ' '
         STNX(I) = 0.0D0
         STNY(I) = 0.0D0
         STNZ(I) = 0.0D0
         STNO(I) = 0.0D0
 10      CONTINUE
C                                      Open AN extension file.
      IVER = SUBA
      IRET = 0
      CALL ANTINI ('READ', BUFFER, DISK, CNO, IVER, CATBLK, LUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      NR = BUFFER(5)
      NSTNS = 0
C                                       Get polarization solution type.
      STNPST = ' '
      MSGSUP = 32000
      CALL TABKEY ('READ', CHSOLT, 1, BUFFER, LOCS, HSTNPS, KEYTYP,
     *   JERR)
      MSGSUP = 0
      CALL H2CHR (8, 1, HSTNPS, STNPST)
      DOELP = STNPST.EQ.CHELP
C                                       Check if VLA
      CNTRX = ARRAYC(1)
      CNTRY = ARRAYC(2)
      CNTRZ = ARRAYC(3)
      GSTIAT = GSTIA0 * DTOR
      ROTIAT = DEGPDY * DTOR
      TIMLAB = TIMSYS
      ANTUTC = UT1UTC
      ANTIAT = DATUTC
      POLARX = POLRXY(1)
      POLARY = POLRXY(2)
      FQIDAN = ANFQID
      ARRLON = 0.0D0
      IF ((CNTRX.NE.0.0D0) .OR. (CNTRY.NE.0.0D0)) ARRLON = ATAN2 (CNTRY,
     *   CNTRX)
C                                      Read AN records
      DO 200 CURANT = 1,NR
         IANRNO = CURANT
         CALL TABAN ('READ', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
C                                       Skip records with
C                                       zero station numbers
         IF ((NOSTA.LE.0) .OR. (NOSTA.GT.MAXANT)) THEN
            WRITE (MSGTXT,1120) NOSTA, CURANT, IVER
            CALL MSGWRT (6)
            GO TO 200
            END IF
C
         NSTNS = MAX (NSTNS, NOSTA)
         STNNAM(NOSTA) = ANNAME
         STNX(NOSTA)   = STAXYZ(1)
         STNY(NOSTA)   = STAXYZ(2)
         STNZ(NOSTA)   = STAXYZ(3)
         STNO(NOSTA)   = STAXOF
         IF ((STAXYZ(1).EQ.0.0) .AND. (STAXYZ(2).EQ.0.0))
     *      STNY(NOSTA) = 1.0D0
         IF (STAXYZ(3).EQ.0.0) STNZ(NOSTA) = 1.0D0
C                                       X => (lat 0, long 0)
C                                       Y => (lat 0, long 90E)
C                                       Z => (lat 90)
C                                       Get antenna latitude and
C                                       east longitude.
         X = CNTRX
         Y = CNTRY
         Z = CNTRZ
         X = X + STAXYZ(1)*COS (ARRLON) - STAXYZ(2)*SIN (ARRLON)
         Y = Y + STAXYZ(2)*COS (ARRLON) + STAXYZ(1)*SIN (ARRLON)
         Z = Z + STAXYZ(3)
         RHO = SQRT (X*X + Y*Y + Z*Z)
         STNLAT(NOSTA) = 0.0
         STNLON(NOSTA) = 0.0
         IF (RHO.NE.0.D0) STNLAT(NOSTA) = ASIN (Z / RHO)
         IF (X.NE.0.D0) STNLON(NOSTA) = ATAN2 (Y, X)
         STNRAD(NOSTA) = RHO
         MNTYP(NOSTA) = MNTSTA
C                                       Feed polarizations
         NIF = 0
         DO 180 LOOP = 1,NOPCAL,2
            NIF = NIF + 1
            STNELP(1,NIF,NOSTA) = POLCA(LOOP)
            STNORI(1,NIF,NOSTA) = POLCA(LOOP+1)
            STNELP(2,NIF,NOSTA) = POLCB(LOOP)
            STNORI(2,NIF,NOSTA) = POLCB(LOOP+1)
C                                       Default circular poln
            IF ((ABS (POLCA(LOOP)).LT.1.0E-10) .AND. DOELP) THEN
               STNELP(1,NIF,NOSTA) =  0.785398164
               STNELP(2,NIF,NOSTA) = -0.785398164
               STNORI(1,NIF,NOSTA) = 0.0
               STNORI(2,NIF,NOSTA) = 0.0
               END IF
 180        CONTINUE
 200     CONTINUE
C                                      Close AN extension file
      CALL TABIO ('CLOS', 1, IANRNO, BUFFER, BUFFER, IRET)
      IF (IRET.EQ.0) GO TO 999
         WRITE (MSGTXT,1200) IRET
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANINFO: ERROR',I3,' OPEN-FOR-READ AN FILE')
 1100 FORMAT ('ANINFO: ERROR',I3,' READING AN FILE')
 1120 FORMAT ('ANINFO: STATION NUMBER',I6,' FOR ENTRY=',I4,' SUBA=',I3)
 1200 FORMAT ('ANINFO: ERROR',I3,' CLOSING AN FILE')
      END
      SUBROUTINE ANTSET (IRET)
C-----------------------------------------------------------------------
C   Routine to set up the list of antennas
C   Input from common:
C      XANT     R(*)        Antenna array
C   Output:
C      IRET     I           Return code, 0=OK, else failed
C   Output in Common:
C      ANTLAB   I(MAXANT)   The selected antennas
C      ANTENS   I(*)        Selected or de-selected antenna numbers
C      NANTSL   I           Number of antennas in list
C      DOAWNT   L           True if antennas selected
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I, J, NEXT, IARG, LIMIT, NUMAN(513), NOANT, LUN, NOSEL
      LOGICAL   T, F, ALLANT, DESEL
      INCLUDE 'LOCIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (NUMAN, UBUFF(1025))
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check for all ant. selected
      ALLANT = T
      DESEL = F
      DO 10 I = 1,50
         ANTENS(I) = 0
         ALLANT = ALLANT .AND. (ABS (XANT(I)).LE.1.0E-10)
         DESEL = DESEL .OR. (XANT(I).LT.-0.5)
 10      CONTINUE
C                                       Not all selected - make list
      IF (.NOT.ALLANT) THEN
         NEXT = 1
C                                       ANTENNAS array.
         DO 30 I = 1,50
            IARG = ABS (XANT(I)) + 0.5
            IF (IARG.GT.0) THEN
C                                       See if already have
               LIMIT = NEXT - 1
               DO 20 J = 1,LIMIT
                  IF (IARG.EQ.ANTENS(J)) GO TO 30
 20               CONTINUE
C                                       New antenna
               ANTENS(NEXT) = IARG
               NEXT = NEXT + 1
               END IF
 30         CONTINUE
         NOSEL = NEXT - 1
         NOANT = NOSEL
         END IF
C                                       Find number of antennas
      LUN = 28
      CALL GETNAN (DISKIN, CNOIN, CATBLK, LUN, UBUFF, NUMAN, IRET)
      IF ((IRET.NE.0) .OR. (NUMAN(1).LT.SUBARR)) THEN
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1031) IRET
         ELSE
            WRITE (MSGTXT,1030) NUMAN(1), SUBARR
            END IF
         IRET = 2
         GO TO 990
         END IF
      IF (ALLANT) NOANT = NUMAN(SUBARR+1)
      IF (ALLANT .AND. (SUBARR.LE.0)) NOANT = NUMAN(2)
C                                       Fill ANTLAB
      IF (.NOT.DESEL) THEN
C                                       Antennas selected
         DO 40 I = 1,NOANT
            IF (ALLANT) THEN
               ANTLAB(I) = I
            ELSE
               ANTLAB(I) = ANTENS(I)
               END IF
 40         CONTINUE
C                                       Antennas deselected
      ELSE
         NEXT = 1
         NOANT = NUMAN(SUBARR+1)
         DO 60 I = 1,NOANT
            DO 50 J = 1,NOSEL
               IF (I.EQ.ANTENS(J)) GO TO 60
 50            CONTINUE
            ANTLAB(NEXT) = I
            NEXT = NEXT + 1
 60         CONTINUE
         NOANT = NEXT - 1
         END IF
C                                       Done
      NANTSL = NOANT
      DOAWNT = .NOT.DESEL
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('Fewer subarrays available (',I3,') than specified ',I4)
 1031 FORMAT ('ANTSET: GETNAN ERROR ',I3,' SEARCHING ANTENNA TABLES')
      END
      SUBROUTINE SOUGEO (ANTNO, TIME, DEC, HA, EL, AZ)
C-----------------------------------------------------------------------
C   Subroutine to compute the apparent source elevations based on source
C   and antenna coordinates in common.  The routines ANINFO and GETSOU
C   should be called before this routine to but the correct values in
C   the relevant commons.
C   Inputs:
C      ANTNO      I    Antenna number
C      TIME       D    Current data time (days).
C   Input from common:
C      RAAPP      D    Apparent RA of source
C      DECAPP     D    Apparent Declination of source.
C      STNLAT     D(*) Antenna latitude (rad).
C      STNLON     D(*) Antenna east longitudes (rad).
C      GSTIAT     D    GST at IAT=0 of reference day (rad).
C      ROTIAT     D    Rotation of the earth rate in IAT.
C   Output:
C      DEC        D    apparent declination, or, if missing,
C                      mean declination
C      HA         D    Source hour angle (rad)
C      EL         D    Source elevation (rad)
C      AZ         D    Source azimuth   (rad)
C-----------------------------------------------------------------------
      INTEGER   ANTNO
      DOUBLE PRECISION TIME, DEC, HA, EL, AZ
C
      DOUBLE PRECISION HRANG, ANTLST, DRA, DDEC, CLAT, SLAT, SINA, COSA,
     *   SINZ, COSZ
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DANTE.INC'
C-----------------------------------------------------------------------
C                                       Antenna LST
      ANTLST = GSTIAT + STNLON(ANTNO) * 1.002738D0 + TIME * ROTIAT
C                                       Source position; if apparent
C                                       position missing use mean
C                                       position.
      DRA = RAAPP
      DDEC = DECAPP
      IF ((DRA.LE.1.0D-5) .AND. (DDEC.LE.1.0D-5)) THEN
         DRA = RAEPO
         DDEC = DECEPO
         END IF
C                                       Hour angle
      HRANG = ANTLST - DRA
C                                       Limit to between 0 and 2pi
      HRANG = MOD (HRANG, TWOPI)
C                                       translate to between -pi and pi
      IF (HRANG.GT.PI) HRANG = HRANG - TWOPI
      IF (HRANG.LT.-PI) HRANG = HRANG + TWOPI
      HA  = HRANG
      DEC = DDEC
C                                       SIN and COS latitude
      SLAT = SIN (STNLAT(ANTNO))
      CLAT = COS (STNLAT(ANTNO))
C                                       zenith angle / elevation
      COSZ = SLAT * SIN (DDEC) + CLAT * COS (DDEC) * COS (HRANG)
      EL   = ASIN (COSZ)
      SINZ = SQRT (1.0D0 - COSZ**2)
C                                       azimuth AZ
      COSA = (SIN (DDEC)*CLAT-COS (DDEC)*COS(HRANG)*SLAT)/ SINZ
      SINA = (-COS (DDEC) * SIN (HRANG)) / SINZ
C                                       first AZ between 0 and PI
      AZ = ACOS (COSA)
C                                       now AZ between 0 and 2*PI
      IF (SINA.LT.0.0) AZ = TWOPI - AZ
C      DARG = (SIN (DDEC) - SLAT * SIN (EL))
C     *   / COS(STNLAT(ANTNO)) / COS (EL)
C      AZ = ACOS (DARG)
C
 999  RETURN
      END
      SUBROUTINE LOCFIT (CURANT, NPTS, NSAMP, OLDPOS, PANS, PERR,
     *   RMSERR, PADNAM, PDIF, IRET)
C-----------------------------------------------------------------------
C   LOCFIT fits the antenna locations to the phases in common.
C   Inputs:
C      CURANT   I      Antenna number being fit
C      NPTS     I      Number of times
C      NSAMP    I      Number of samples
C   Output:
C      OLDPOS   D(4)   Initial antenna location
C      PANS     R(4)   Answers
C      PERR     R(4)   Uncertainties
C      RMSERR   R      Overall uncertainty
C      PADNAM   C*8    Antenna location name
C      PDIF     R(*)   Phase differences from model - turns
C      IRET     I      Error code: > 0 quit
C-----------------------------------------------------------------------
      INTEGER   CURANT, NPTS, NSAMP, IRET
      DOUBLE PRECISION OLDPOS(4)
      REAL      PANS(4), PERR(4), RMSERR, PDIF(*)
      CHARACTER PADNAM*8, PADTST*8
C
      INCLUDE 'LOCIT.INC'
      INTEGER   NCVM
      PARAMETER (NCVM=5+2*MAXIF)
C
      REAL      ANS(NCVM), COVAR(NCVM,NCVM), CHISQ, SUMRE, SUMIM, AVGPH,
     *   PFIT(MAXDAT), REDCNT, RCON, SIGERR(NCVM), NCOVAR(NCVM,NCVM),
     *   XT, YT
      INTEGER   MA, MFIT, LISTA(NCVM), I, J, II, NS, NF, NIF, NC, ITRIM
      CHARACTER LINE*132, PLINE*8, TYPE(5)*2
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'DANTE.INC'
      DATA TYPE /'Bx','By','Bz','k ','Dt'/
C-----------------------------------------------------------------------
      IRET = -1
      WRITE (MSGTXT,1000) CURANT, NPTS, NSAMP
      CALL MSGWRT (2)
      IF ((NSAMP.LE.0) .OR. (NPTS.LE.0)) GO TO 999
C                                       antenna info
      PADTST = STNNAM(CURANT)
      IF (PADTST(:5).EQ.'VLA: ') THEN
         PADNAM = ' ' // STNNAM(CURANT)(6:8)
      ELSE IF (PADTST(:5).EQ.'VLA:_') THEN
         PADNAM = ' ' // STNNAM(CURANT)(6:8)
      ELSE IF (PADTST(:4).EQ.'VLA:') THEN
         PADNAM = ' ' // STNNAM(CURANT)(5:7)
      ELSE IF (PADTST(:5).EQ.'EVLA:') THEN
         PADNAM = '*' // STNNAM(CURANT)(6:8)
      ELSE
         PADNAM = '*' // STNNAM(CURANT)(1:3)
         END IF
      I = ITRIM (PADNAM)
      IF (I.EQ.3) PADNAM(3:4) = '0' // PADNAM(3:3)
      OLDPOS(1) = 1.0E9 * STNX(CURANT) / VELITE
      OLDPOS(2) = 1.0E9 * STNY(CURANT) / VELITE
      OLDPOS(3) = 1.0E9 * STNZ(CURANT) / VELITE
      OLDPOS(4) = 1.0E9 * STNO(CURANT) / VELITE
C                                       what parms
      LISTA(1) = 1
      LISTA(2) = 2
      LISTA(3) = 3
      IF (DPARM(2).LE.0.0) THEN
         LISTA(4) = 4
         J = 4
      ELSE
         J = 3
         END IF
      MA = 5
      MFIT = J
      NIF = NUMPOL * NUMIFS
C                                       solve straight
      IF (DPARM(1).LE.0.0) THEN
         IF (DPARM(10).GT.0.0) THEN
            J = J + 1
            MFIT = J
            LISTA(J) = 5
            END IF
         DO 10 I = 1,NIF
            LISTA(J+I) = 5+I
 10         CONTINUE
         MFIT = MFIT + NIF
         MA = MA + NIF
         NF = 1
C                                       solve differences: fix data
      ELSE
         NF = 2
         NSAMP = 0
         DO 30 I = 1,NIF
            II = I
            DO 20 J = 1,NPTS-1
               IF ((PHASES(II).NE.FBLANK) .AND.
     *            (PHASES(II+NIF).NE.FBLANK)) THEN
                  PHASES(II) = PHASES(II+NIF) - PHASES(II)
                  NSAMP = NSAMP + 1
               ELSE
                  PHASES(II) = FBLANK
                  END IF
               II = II + NIF
 20            CONTINUE
 30         CONTINUE
         NPTS = NPTS - 1
         END IF
C                                       center phases
      IF ((XPRTLV.GE.0.0) .AND. (DOPRT)) THEN
         LINE = ' '
         CALL ZTXIO ('WRIT', LUNP, INDP, LINE(:2), IRET)
         WRITE (MSGTXT,1000) CURANT, NPTS, NSAMP
         NC = ITRIM (MSGTXT)
         IF (IRET.EQ.0) CALL ZTXIO ('WRIT', LUNP, INDP, MSGTXT(:NC),
     *      IRET)
         END IF
      DO 50 I = 1,NIF
         II = I
         SUMRE = 0.
         SUMIM = 0.
         NS = 0
         DO 40 J = 1,NPTS
            IF (PHASES(II).NE.FBLANK) THEN
               SUMRE = SUMRE + COS (TWOPI * PHASES(II))
               SUMIM = SUMIM + SIN (TWOPI * PHASES(II))
               NS = NS + 1
               END IF
            II = II + NIF
 40         CONTINUE
         IF (NS.GT.0) THEN
            SUMRE = SUMRE / NS
            SUMIM = SUMIM / NS
            AVGPH = ATAN2 (SUMIM, SUMRE) / TWOPI
            II = MOD (I-1, NUMPOL) + 1
            J = (I-1) / NUMPOL + 1
            WRITE (MSGTXT,1040) II, J+BIF-1, AVGPH*360.
            CALL MSGWRT (3)
            IF ((XPRTLV.GT.0.0) .AND. (DOPRT)) THEN
               NC = ITRIM (MSGTXT)
               CALL ZTXIO ('WRIT', LUNP, INDP, MSGTXT(:NC), IRET)
               IF (IRET.NE.0) DOPRT = .FALSE.
               END IF
            II = I
            DO 45 J = 1,NPTS
               IF (PHASES(II).NE.FBLANK) THEN
                  IF (PHASES(II).GT.AVGPH+0.5) THEN
                     PHASES(II) = PHASES(II) - 1.
                  ELSE IF (PHASES(II).LT.AVGPH-0.5) THEN
                     PHASES(II) = PHASES(II) + 1.
                     END IF
                  END IF
               II = II + NIF
 45            CONTINUE
            END IF
 50      CONTINUE
C                                       fit
      CALL LFIT (HA, DE, EL, TI, PHASES, NPTS, NIF, ANS, MA, LISTA,
     *   MFIT, NF, COVAR, NCVM, CHISQ, PFIT, PDIF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       output display
      IF ((XPRTLV.GT.0.0) .AND. (DOPRT)) THEN
         LINE = ' '
         CALL ZTXIO ('WRIT', LUNP, INDP, LINE(:1), IRET)
         LINE = 'Npnt Source     H.A.  Decl  Elev    Phase differences'
         IF (NIF.LE.1) LINE(34:) = '  ObsPhs   FitPhs   DifPhs'
         NC = ITRIM (LINE)
         IF (IRET.EQ.0) CALL ZTXIO ('WRIT', LUNP, INDP, LINE(:NC), IRET)
         IF (IRET.NE.0) DOPRT = .FALSE.
         IF (XPRTLV.GT.1.5) THEN
            MSGTXT = LINE(:72)
            CALL MSGWRT (2)
            END IF
         END IF
      IF ((XPRTLV.GT.0.0) .AND. (DOPRT)) THEN
         II = 0
         DO 70 J = 1,NPTS
            HA(J) = HA(J) * RAD2DG / 15.0D0
            DE(J) = DE(J) * RAD2DG
            EL(J) = EL(J) * RAD2DG
            AZ(J) = AZ(J) * RAD2DG
            WRITE (LINE,1050) J, SOURNM(J), HA(J), DE(J), EL(J)
            NC = 34
            IF (NIF.GT.1) THEN
               DO 60 I = 1,NIF
                  II = II + 1
                  IF (PDIF(II).EQ.FBLANK) THEN
                     PLINE = '  --.-'
                  ELSE
                     WRITE (PLINE,1055) PDIF(II)*360.0
                     END IF
                  LINE(NC:) = PLINE
                  NC = NC + 6
                  IF (NC.GT.125) GO TO 65
 60               CONTINUE
            ELSE
               II = II + 1
               IF (PHASES(II).EQ.FBLANK) THEN
                  PLINE = '  --.-'
               ELSE
                  WRITE (PLINE,1055) PHASES(II)*360.0
                  END IF
               LINE(NC:) = PLINE
               NC = NC + 6
               IF (PFIT(II).EQ.FBLANK) THEN
                  PLINE = '  --.-'
               ELSE
                  WRITE (PLINE,1055) PFIT(II)*360.0
                  END IF
               LINE(NC:) = PLINE
               NC = NC + 6
               IF (PDIF(II).EQ.FBLANK) THEN
                  PLINE = '  --.-'
               ELSE
                  WRITE (PLINE,1055) PDIF(II)*360.0
                  END IF
               LINE(NC:) = PLINE
               NC = NC + 6
               END IF
 65         NC = ITRIM (LINE)
            CALL ZTXIO ('WRIT', LUNP, INDP, LINE(:NC), IRET)
            IF (IRET.NE.0) THEN
               DOPRT = .FALSE.
               GO TO 75
               END IF
            IF (XPRTLV.GT.1.5) THEN
               MSGTXT = LINE(:64)
               CALL MSGWRT (2)
               END IF
 70         CONTINUE
         END IF
C                                       get sigmas et al
 75   REDCNT = SQRT (CHISQ / (NSAMP - MFIT - 1))
      RCON = 1.0E9 / BFREQ
      DO 80 I = 1,MA
         IF (I.LE.4) THEN
            ANS(I) = ANS(I) * RCON
            SIGERR(I) = SQRT (COVAR(I,I)) * REDCNT * ABS (RCON)
         ELSE IF (I.EQ.5) THEN
            ANS(I) = ANS(I) * 360.0 / 24.0
            SIGERR(I) = SQRT (COVAR(I,I)) * REDCNT * 15.0
         ELSE
            ANS(I) = ANS(I) * 360.0
            SIGERR(I) = SQRT (COVAR(I,I)) * REDCNT * 360.0
            END IF
 80      CONTINUE
C                                       Fix X/Y to array longitude
      XT = ANS(1)
      YT = ANS(2)
      ANS(1) = XT * COS(STNLON(CURANT)-ARRLON) -
     *   YT * SIN(STNLON(CURANT)-ARRLON)
      ANS(2) = YT * COS(STNLON(CURANT)-ARRLON) +
     *   XT * SIN(STNLON(CURANT)-ARRLON)
      XT = SIGERR(1)
      YT = SIGERR(2)
      SIGERR(1) = SQRT ((XT * COS(STNLON(CURANT)-ARRLON))**2 +
     *   (YT * SIN(STNLON(CURANT)-ARRLON))**2)
      SIGERR(2) = SQRT ((YT * COS(STNLON(CURANT)-ARRLON))**2 +
     *   (XT * SIN(STNLON(CURANT)-ARRLON))**2)
C                                       overall rms
      RMSERR = SQRT (CHISQ / NSAMP) * 360.0
      WRITE (MSGTXT,1080) RMSERR
      CALL MSGWRT (4)
      IF (DOPRT) THEN
         NC = ITRIM (MSGTXT)
         PLINE = ' '
         CALL ZTXIO ('WRIT', LUNP, INDP, PLINE(:2), IRET)
         IF (IRET.EQ.0) CALL ZTXIO ('WRIT', LUNP, INDP, MSGTXT(:NC),
     *      IRET)
         IF (IRET.NE.0) DOPRT = .FALSE.
         END IF
C                                       answers
      MSGTXT = 'Fitted Baselines and Errors in Nanoseconds are:'
      CALL MSGWRT (4)
      IF (DOPRT) THEN
         PLINE = ' '
         CALL ZTXIO ('WRIT', LUNP, INDP, PLINE(:2), IRET)
         NC = ITRIM (MSGTXT)
         IF (IRET.EQ.0) CALL ZTXIO ('WRIT', LUNP, INDP, MSGTXT(:NC),
     *      IRET)
         IF (IRET.NE.0) DOPRT = .FALSE.
         END IF
      DO 90 I = 1,MFIT
         J = LISTA(I)
         IF (J.LT.5) THEN
            WRITE (MSGTXT,1085) TYPE(J), ANS(J), SIGERR(J)
         ELSE IF (J.EQ.5) THEN
            MSGTXT = 'Fitted phase slope in degrees/hour'
            CALL MSGWRT (4)
            WRITE (MSGTXT,1085) TYPE(J), ANS(J), SIGERR(J)
         ELSE
            MSGTXT = 'Fitted average phase in degrees'
            IF (J.EQ.6) CALL MSGWRT (4)
            WRITE (MSGTXT,1086) J-5, ANS(J), SIGERR(J)
            END IF
         CALL MSGWRT (4)
         IF (DOPRT) THEN
            NC = ITRIM (MSGTXT)
            CALL ZTXIO ('WRIT', LUNP, INDP, MSGTXT(:NC), IRET)
            IF (IRET.NE.0) DOPRT = .FALSE.
            END IF
 90      CONTINUE
      MSGTXT = 'Fitted Baselines and Errors in centimeters are:'
      CALL MSGWRT (4)
      IF (DOPRT) THEN
         NC = ITRIM (MSGTXT)
         PLINE = ' '
         CALL ZTXIO ('WRIT', LUNP, INDP, PLINE(:2), IRET)
         IF (IRET.EQ.0) CALL ZTXIO ('WRIT', LUNP, INDP, MSGTXT(:NC),
     *      IRET)
         IF (IRET.NE.0) DOPRT = .FALSE.
         END IF
      DO 100 I = 1,MFIT
         J = LISTA(I)
         IF (J.LE.4) THEN
            WRITE (MSGTXT,1085) TYPE(J), ANS(J)*29.97, SIGERR(J)*29.97
            CALL MSGWRT (4)
            IF (DOPRT) THEN
               NC = ITRIM (MSGTXT)
               CALL ZTXIO ('WRIT', LUNP, INDP, MSGTXT(:NC), IRET)
               IF (IRET.NE.0) DOPRT = .FALSE.
               END IF
            END IF
 100     CONTINUE
C                                       normalized covariance matrix
      DO 110 J = 1,MA
         CALL RFILL (MA, 0.0, NCOVAR(1,J))
         IF (COVAR(J,J).NE.0.0) THEN
            DO 105 I = 1,MA
               IF (COVAR(I,I).NE.0.0) NCOVAR(I,J) = COVAR(I,J) /
     *            SQRT (COVAR(I,I) * COVAR(J,J))
 105           CONTINUE
            END IF
 110     CONTINUE
      LINE = '---- Normalized Covariance Matrix ---'
      IF ((DOPRT) .AND. (XPRTLV.GT.1.5)) THEN
         NC = ITRIM (LINE)
         PLINE = ' '
         CALL ZTXIO ('WRIT', LUNP, INDP, PLINE(:2), IRET)
         IF (IRET.EQ.0) CALL ZTXIO ('WRIT', LUNP, INDP, LINE(:NC), IRET)
         IF (IRET.NE.0) DOPRT = .FALSE.
         END IF
      MSGTXT = LINE(:80)
      CALL MSGWRT (2)
      DO 120 J = 1,MA
         WRITE (LINE,1110) (NCOVAR(I,J), I = 1,MIN(18,MA))
         IF ((DOPRT) .AND. (XPRTLV.GT.1.5)) THEN
            NC = ITRIM (LINE)
            CALL ZTXIO ('WRIT', LUNP, INDP, LINE(:NC), IRET)
            IF (IRET.NE.0) DOPRT = .FALSE.
            END IF
         MSGTXT = LINE(:80)
         CALL MSGWRT (2)
 120     CONTINUE
C                                       returns for summary displays
      CALL RCOPY (4, ANS, PANS)
      CALL RCOPY (4, SIGERR, PERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Solving antenna',I3,' with',I4,' times',I5,' samples')
 1040 FORMAT ('Pol',I2,' IF',I3,'  average phase is',F7.1,' degrees')
 1050 FORMAT (I4,1X,A8,F7.2,F6.1,F6.1)
 1055 FORMAT (F6.1)
 1080 FORMAT ('RMS error of fit =',F8.2,' degrees')
 1085 FORMAT (A,' =',F9.4,'   1-sigma error =',F8.4)
 1086 FORMAT ('Phase',I3,' =',F7.1,'   1-sigma error =',F6.2)
 1110 FORMAT (13F7.3)
      END
      SUBROUTINE LOCSUM (NSAMP, OLDPOS, PANS, PERR, PRMS, PADNAM)
C-----------------------------------------------------------------------
C   summary displays
C-----------------------------------------------------------------------
      INTEGER   NSAMP(*)
      DOUBLE PRECISION OLDPOS(4,*)
      REAL      PANS(4,*), PERR(4,*), PRMS(*)
      CHARACTER PADNAM(*)*(*)
C
      INCLUDE 'LOCIT.INC'
      INTEGER   I, J, IANT, NC, ITRIM, IRET, JANT, ILEN
      LOGICAL   FIRST, DOBK
      REAL      BX, BY, BZ, BK, BS
      CHARACTER LINE*132, BLANK*4, PAD*8, OLDPAD(MAXANT)*8, LPBASE*48,
     *   LPNAME*60, LPNAM2*60, LPNAM3*60
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANT.INC'
C-----------------------------------------------------------------------
      FIRST = .TRUE.
      BLANK = ' '
      DO 10 I = 1,NANTSL
         IF (NSAMP(I).GT.0) THEN
            IANT = ANTLAB(I)
            IF ((DOPRT) .AND. (FIRST)) THEN
               LINE = '             SUMMARY OF RESULTS -- ' //
     *            '(all quantities in nsec)'
               NC = ITRIM (LINE)
               CALL ZTXIO ('WRIT', LUNP, INDP, BLANK, IRET)
               IF (IRET.EQ.0) CALL ZTXIO ('WRIT', LUNP, INDP, LINE(:NC),
     *            IRET)
               IF (IRET.EQ.0) THEN
                   LINE = 'ANT L         OLD_Bx   DELBX   ERBX        '
     *                // 'OLD_By   DELBY   ERBY        OLD_Bz   DELBZ'
     +                // '   ERBZ   OLDK   DELK   ERRK  RMS NPTS'
                   NC = ITRIM (LINE)
                   CALL ZTXIO ('WRIT', LUNP, INDP, LINE(:NC), IRET)
                   END IF
               IF (IRET.NE.0) DOPRT = .FALSE.
               END IF
            IF (DOPRT) THEN
               WRITE (LINE,1005) IANT, PADNAM(I), (OLDPOS(J,I),
     *            PANS(J,I), PERR(J,I), J = 1,4), PRMS(I), NSAMP(I)
               NC = ITRIM (LINE)
               CALL ZTXIO ('WRIT', LUNP(1), INDP(1), LINE(:NC), IRET)
               IF (IRET.NE.0) DOPRT = .FALSE.
               END IF
            FIRST = .FALSE.
            END IF
 10      CONTINUE
C                                       Operator summary
      FIRST = .TRUE.
      DO 20 I = 1,NANTSL
         IF (NSAMP(I).GT.0) THEN
            IANT = ANTLAB(I)
            IF ((DOPRT) .AND. (FIRST)) THEN
               LINE = '             SUGGESTED BASELINE CHANGES' //
     *           ' -- in nsec'
               NC = ITRIM (LINE)
               CALL ZTXIO ('WRIT', LUNP(1), INDP(1), BLANK, IRET)
               IF (IRET.EQ.0) CALL ZTXIO ('WRIT', LUNP(1), INDP(1),
     *            LINE(:NC), IRET)
               IF (IRET.EQ.0) THEN
                   LINE =  'Ant L         Old_Bx         Old_By       '
     *                // '  Old_Bz   Old_K   Del_Bx  Del_By  Del_Bz '
     *                // ' Del_K   Rms Npt'
                   NC = ITRIM (LINE)
                   CALL ZTXIO ('WRIT', LUNP(1), INDP(1), LINE(:NC),
     *                IRET)
                   END IF
               IF (IRET.NE.0) DOPRT = .FALSE.
               END IF
            IF (DOPRT) THEN
               BX = 0.0
               BY = 0.0
               BZ = 0.0
               BK = 0.0
               IF (ABS(PANS(1,I)).GT.4.*PERR(1,I)) BX = PANS(1,I)
               IF (ABS(PANS(2,I)).GT.4.*PERR(2,I)) BY = PANS(2,I)
               IF (ABS(PANS(3,I)).GT.4.*PERR(3,I)) BZ = PANS(3,I)
               IF (ABS(PANS(4,I)).GT.4.*PERR(4,I)) BK = PANS(4,I)
               BS = ABS(BX) + ABS(BY) + ABS(BZ) + ABS(BK)
               IF (BS.GT.0.0) THEN
                  WRITE (LINE,1010) IANT, PADNAM(I), (OLDPOS(J,I), J =
     *               1,4), BX, BY, BZ, BK, PRMS(I), NSAMP(I)
                  NC = ITRIM (LINE)
                  CALL ZTXIO ('WRIT', LUNP(1), INDP(1), LINE(:NC), IRET)
                  IF (IRET.NE.0) DOPRT = .FALSE.
                  END IF
               END IF
            FIRST = .FALSE.
            END IF
 20      CONTINUE
C      IF (.NOT.DOPRT) GO TO 999
C                                       VLA format summary
C                                       Does old file exist?
      CALL RFILL (NANTSL, 0.0, OLDBX)
      CALL RFILL (NANTSL, 0.0, OLDBZ)
      CALL RFILL (NANTSL, 0.0, OLDBY)
      CALL RFILL (NANTSL, 0.0, OLDBK)
      CALL RFILL (NANTSL, 0.0, SIGBX)
      CALL RFILL (NANTSL, 0.0, SIGBZ)
      CALL RFILL (NANTSL, 0.0, SIGBY)
      DO 25 I = 1,NANTSL
         OLDPAD(I) = ' '
 25      CONTINUE
      CALL H2CHR (48, 1, XLPNAM, LPBASE)
      ILEN = ITRIM (LPBASE)
      LPNAME = LPBASE(:ILEN) // '.' // ANAME
      MSGSUP = 32000
      CALL ZTXOPN ('QRED', LUNP(2), INDP(2), LPNAME, .TRUE., IRET)
      MSGSUP = 0
      IF (IRET.EQ.0) THEN
         MSGTXT = '*****  FOUND OLD .array FILE  ******'
         CALL MSGWRT (5)
         MSGTXT = 'ADDING ITS CONTENTS TO CURRENT ANSWERS'
         CALL MSGWRT (5)
 30      CALL ZTXIO ('READ', LUNP(2), INDP(2), LINE, IRET)
         IF ((IRET.NE.0) .AND. (IRET.NE.2)) THEN
            WRITE (MSGTXT,1000) IRET, 'READING OLD .array FILE'
            CALL MSGWRT (8)
            GO TO 999
C                                       normal read, parse and repeat
         ELSE IF (IRET.EQ.0) THEN
            IF (LINE.EQ.' ') GO TO 30
            IF (LINE(:32).NE.' ') GO TO 30
            READ (LINE,1020,ERR=30) JANT, PAD, BX, BY, BZ, BK
C                                       find JANT in ANTLAB
            IANT = 0
            DO 35 I = 1,NANTSL
               IF (ANTLAB(I).EQ.JANT) IANT = I
 35            CONTINUE
            IF (IANT.LE.0) THEN
               WRITE (MSGTXT,1035) JANT, PAD
               CALL MSGWRT (8)
            ELSE IF (PAD.NE.PADNAM(IANT)(2:4)) THEN
               WRITE (MSGTXT,1036) JANT, PAD, PADNAM(JANT)(2:4)
               CALL MSGWRT (8)
            ELSE
               OLDBX(IANT) = BX
               OLDBY(IANT) = BY
               OLDBZ(IANT) = BZ
               OLDBK(IANT) = BK
               END IF
            GO TO 30
            END IF
C                                       close and rename
         CALL ZTXCLS (LUNP(2), INDP(2), IRET)
         NC = ITRIM (ANAME)
         LPNAM2 = LPBASE(:ILEN) // '.' // ANAME(:NC) // '.OLD'
         LPNAM3 = LPBASE(:ILEN) // '.' // ANAME(:NC) // '.OLDER'
c         MSGSUP = 32000
         CALL ZTXZAP (LUNP(2), LPNAM3, IRET)
         CALL ZTXREN (LUNP(2), LPNAM2, LUNP(3), LPNAM3, IRET)
         CALL ZTXREN (LUNP(2), LPNAME, LUNP(3), LPNAM2, IRET)
         MSGSUP = 0
      ELSE
         INDP(2) = 0
         END IF
C                                       now open for write
      IF (DOPRT) THEN
         CALL ZTXOPN ('WRIT', LUNP(2), INDP(2), LPNAME, .TRUE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OUTPUT .array FILE'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
      FIRST = .TRUE.
      DO 40 I = 1,NANTSL
         IF (NSAMP(I).GT.0) THEN
            IANT = ANTLAB(I)
            IF ((DOPRT) .AND. (FIRST)) THEN
               LINE = '; '
               LINE(21:) = 'BASELINE CORRECTIONS IN METERS'
               NC = ITRIM (LINE)
               CALL ZTXIO ('WRIT', LUNP(1), INDP(1), BLANK, IRET)
               IF (IRET.EQ.0) CALL ZTXIO ('WRIT', LUNP(1), INDP(1),
     *            LINE(:NC), IRET)
               IF (IRET.EQ.0) CALL ZTXIO ('WRIT', LUNP(2), INDP(2),
     *            LINE(:NC), IRET)
               IF (IRET.EQ.0) THEN
                   LINE =  ';MOVED OBSDATE   Put_In MC(IAT)   ANT  PAD'
     *                // '  Del_Bx  Del_By  Del_Bz   Del_K   Rms'
                   NC = ITRIM (LINE)
                   CALL ZTXIO ('WRIT', LUNP(1), INDP(1), LINE(:NC),
     *                IRET)
                   IF (IRET.EQ.0) CALL ZTXIO ('WRIT', LUNP(2), INDP(2),
     *                LINE(:NC), IRET)
                   END IF
               IF (IRET.NE.0) DOPRT = .FALSE.
               END IF
            IF (DOPRT) THEN
               BX = OLDBX(I)
               BY = OLDBY(I)
               BZ = OLDBZ(I)
               BK = OLDBK(I)
               IF (ABS(PANS(1,I)).GT.4.*PERR(1,I)) BX = BX + PANS(1,I) *
     *            0.2997
               IF (ABS(PANS(2,I)).GT.4.*PERR(2,I)) BY = BY + PANS(2,I) *
     *            0.2997
               IF (ABS(PANS(3,I)).GT.4.*PERR(3,I)) BZ = BZ + PANS(3,I) *
     *            0.2997
               IF (ABS(PANS(4,I)).GT.4.*PERR(4,I)) BK = BK + PANS(4,I) *
     *            0.2997
               BS = ABS(BX) + ABS(BY) + ABS(BZ) + ABS(BK)
               IF (BS.GT.0.0) THEN
                  PAD = PADNAM(I)(2:4)
                  WRITE (LINE,1020) IANT, PAD, BX, BY, BZ, BK, PRMS(I)
                  NC = ITRIM (LINE)
                  CALL ZTXIO ('WRIT', LUNP(1), INDP(1), LINE(:NC), IRET)
                  IF (IRET.EQ.0) CALL ZTXIO ('WRIT', LUNP(2), INDP(2),
     *               LINE(:NC), IRET)
                  IF (IRET.NE.0) DOPRT = .FALSE.
                  END IF
               END IF
            FIRST = .FALSE.
            END IF
 40      CONTINUE
C      IF (.NOT.DOPRT) GO TO 999
C                                       CLCOR input
      IF (DOPRT) THEN
         LPNAME = LPBASE(:ILEN) // '.001'
         CALL ZTXOPN ('WRIT', LUNP(3), INDP(3), LPNAME, .TRUE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OUTPUT .array FILE'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
      FIRST = .TRUE.
      DOBK = .FALSE.
      DO 50 I = 1,NANTSL
         IF (NSAMP(I).GT.0) THEN
            IANT = ANTLAB(I)
            IF ((DOPRT) .AND. (FIRST)) THEN
               LINE = '$  Baseline corrections for CLCOR'
               NC = ITRIM (LINE)
               CALL ZTXIO ('WRIT', LUNP(3), INDP(3), LINE(:NC), IRET)
               IF (IRET.NE.0) DOPRT = .FALSE.
               END IF
            IF ((DOPRT) .AND. (FIRST)) THEN
               LINE = 'dowait = 1 ; opcode ''antp'''
               NC = ITRIM (LINE)
               CALL ZTXIO ('WRIT', LUNP(3), INDP(3), LINE(:NC), IRET)
               IF (IRET.NE.0) DOPRT = .FALSE.
               END IF
            BX = 0.0
            BY = 0.0
            BZ = 0.0
            BK = 0.0
            IF (ABS(PANS(1,I)).GT.4.*PERR(1,I)) BX = BX + PANS(1,I) *
     *         0.2997
            IF (ABS(PANS(2,I)).GT.4.*PERR(2,I)) BY = BY + PANS(2,I) *
     *         0.2997
            IF (ABS(PANS(3,I)).GT.4.*PERR(3,I)) BZ = BZ + PANS(3,I) *
     *         0.2997
            IF (ABS(PANS(4,I)).GT.4.*PERR(4,I)) BK = BK + PANS(4,I) *
     *         0.2997
            BS = ABS(BX) + ABS(BY) + ABS(BZ)
            IF (ABS(BK).GT.0.0) DOBK = .TRUE.
            IF ((BS.GT.0.0) .AND. (DOPRT)) THEN
               PAD = PADNAM(I)(2:4)
               WRITE (LINE,1040) IANT, BX, BY, BZ, PAD,
     *            (PERR(J,I)*299.7, J = 1,3)
               NC = ITRIM (LINE)
               CALL ZTXIO ('WRIT', LUNP(3), INDP(3), LINE(:NC), IRET)
               IF (IRET.NE.0) DOPRT = .FALSE.
               END IF
            FIRST = .FALSE.
            END IF
 50      CONTINUE
      IF ((DOBK) .AND. (DOPRT))  THEN
         DO 60 I = 1,NANTSL
            IF (NSAMP(I).GT.0) THEN
               IANT = ANTLAB(I)
               IF ((DOPRT) .AND. (FIRST)) THEN
                  LINE = '$  Baseline K corrections for CLCOR'
                  NC = ITRIM (LINE)
                  CALL ZTXIO ('WRIT', LUNP(3), INDP(3), LINE(:NC), IRET)
                  IF (IRET.NE.0) DOPRT = .FALSE.
                  END IF
               IF ((DOPRT) .AND. (FIRST)) THEN
                  LINE = 'dowait = 1 ; opcode ''anax'''
                  NC = ITRIM (LINE)
                  CALL ZTXIO ('WRIT', LUNP(3), INDP(3), LINE(:NC), IRET)
                  IF (IRET.NE.0) DOPRT = .FALSE.
                  END IF
               IF (DOPRT) THEN
                  BK = 0.0
                  IF (ABS(PANS(4,I)).GT.4.*PERR(4,I)) BK = PANS(4,I) *
     *               0.2997
                  IF (ABS(BK).GT.0.0) THEN
                     PAD = PADNAM(I)(2:4)
                     WRITE (LINE,1050) IANT, BK, PAD, PERR(4,I)*299.7
                     NC = ITRIM (LINE)
                     CALL ZTXIO ('WRIT', LUNP(3), INDP(3), LINE(:NC),
     *                  IRET)
                     IF (IRET.NE.0) DOPRT = .FALSE.
                     END IF
                  END IF
               FIRST = .FALSE.
               END IF
 60         CONTINUE
         END IF
      IF (DOPRT) THEN
         LINE = 'dowait = -1 '
         NC = ITRIM (LINE)
         CALL ZTXIO ('WRIT', LUNP(3), INDP(3), LINE(:NC), IRET)
         IF (IRET.NE.0) DOPRT = .FALSE.
         END IF
C                                       PARMINATOR input
      IF (DOPRT) THEN
         LPNAME = LPBASE(:ILEN) // '.PAR'
         CALL ZTXOPN ('WRIT', LUNP(4), INDP(4), LPNAME, .TRUE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OUTPUT .array FILE'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
      FIRST = .TRUE.
      DO 70 I = 1,NANTSL
         IF (NSAMP(I).GT.0) THEN
            IANT = ANTLAB(I)
            PAD = PADNAM(I)(2:4)
            IF (PAD.EQ.'MPD') PAD = 'MAS'
            BX = OLDBX(I)
            BY = OLDBY(I)
            BZ = OLDBZ(I)
            BK = OLDBK(I)
            IF (ABS(PANS(1,I)).GT.4.*PERR(1,I)) BX = BX + PANS(1,I) *
     *         0.2997
            IF (ABS(PANS(2,I)).GT.4.*PERR(2,I)) BY = BY + PANS(2,I) *
     *         0.2997
            IF (ABS(PANS(3,I)).GT.4.*PERR(3,I)) BZ = BZ + PANS(3,I) *
     *         0.2997
            IF (ABS(PANS(4,I)).GT.4.*PERR(4,I)) BK = BK + PANS(4,I) *
     *         0.2997
C                                       send summed values out for plot
            IF (BPARM(9).GE.1.5) THEN
               OLDBX(I) = BX * 1000.0
               OLDBY(I) = BY * 1000.0
               OLDBZ(I) = BZ * 1000.0
               OLDBK(I) = BK * 1000.0
            ELSE
               OLDBX(I) = (OLDBX(I) + PANS(1,I)*0.2997) * 1.E3
               OLDBY(I) = (OLDBY(I) + PANS(2,I)*0.2997) * 1.E3
               OLDBZ(I) = (OLDBZ(I) + PANS(3,I)*0.2997) * 1.E3
               OLDBK(I) = (OLDBK(I) + PANS(4,I)*0.2997) * 1.E3
               END IF
            SIGBX(I) = PERR(1,I) * 299.7
            SIGBY(I) = PERR(2,I) * 299.7
            SIGBZ(I) = PERR(3,I) * 299.7
            IF ((DOPRT) .AND. (BX.NE.0.0)) THEN
               WRITE (LINE,1060) PAD, 'X', BX
               NC = ITRIM (LINE)
               CALL ZTXIO ('WRIT', LUNP(4), INDP(4), LINE(:NC), IRET)
               IF (IRET.NE.0) DOPRT = .FALSE.
               END IF
            IF ((DOPRT) .AND. (BY.NE.0.0)) THEN
               WRITE (LINE,1060) PAD, 'Y', BY
               NC = ITRIM (LINE)
               CALL ZTXIO ('WRIT', LUNP(4), INDP(4), LINE(:NC), IRET)
               IF (IRET.NE.0) DOPRT = .FALSE.
               END IF
            IF ((DOPRT) .AND. (BZ.NE.0.0)) THEN
               WRITE (LINE,1060) PAD, 'Z', BZ
               NC = ITRIM (LINE)
               CALL ZTXIO ('WRIT', LUNP(4), INDP(4), LINE(:NC), IRET)
               IF (IRET.NE.0) DOPRT = .FALSE.
               END IF
            IF ((DOPRT) .AND. (BK.NE.0.0)) THEN
               WRITE (LINE,1060) PAD, 'K', BK
               NC = ITRIM (LINE)
               CALL ZTXIO ('WRIT', LUNP(4), INDP(4), LINE(:NC), IRET)
               IF (IRET.NE.0) DOPRT = .FALSE.
               END IF
            FIRST = .FALSE.
            END IF
 70      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LOCSUM: ERROR',I4,1X,A)
 1005 FORMAT (I2,A4,3(F15.4,2F7.4),3F7.4,F5.1,I4)
 1010 FORMAT (I2,A4,3F15.4,5F8.4,F5.1,I4)
 1020 FORMAT (32X,I4,3X,A3,4F8.4,F6.1)
 1035 FORMAT ('OLD .array FILE ANTENNA',I3,' PAD NAME ',A3,' NOT FOUND')
 1036 FORMAT ('OLD .array FILE ANTENNA',I3,' PAD NAME ',A3,
     *   ' NOT EQUAL ',A3)
 1040 FORMAT ('antenn=',I2,',0; clcorp=',2(F7.4,','),F7.4,';go $ ',A3,
     *   3F7.1)
 1050 FORMAT ('antenn=',I2,',0; clcorp=',F7.4,',0; go $ ',A3, F7.1)
 1060 FORMAT (A3,',,',A1,',$',F7.4)
      END
      SUBROUTINE LFIT (HA, DEC, EL, TI, Y, NDATA, NIF, A, MA, LISTA,
     *   MFIT, NF, COVAR, NCVM, CHISQ, YFIT, YDIF, IRET)
C-----------------------------------------------------------------------
C   LFIT does the actual fitting
C   Inputs:
C      HA      D(*)      Hour angles radians
C      DEC     D(*)      Declinations radians
C      EL      D(*)      Elevations radians
C      TI      D(*)      Time days
C      Y       R(*,*)    The data  (NIF,NDATA)
C      NDATA   I         Number of data times
C      NIF     I         Number of IFs
C      MA      I         Number of fitted parameters, maximum
C      MFIT    I         Number of fitted parameters, actual
C      NCVM    I         Dimensions of covariance matrix (usually MA)
C      NF      I         Number of data points
C   In/Out:
C      LISTA   I(*)      Which parameters are fitted
C   Output:
C      A       R(*)      Answers (MA)
C      COVAR   R(*,*)    Covariance matrix (NCVM,NCVM)
C      CHISQ   R         Chi squared of fit
C      YFIT    R(*)      The fitted values
C      YDIF    R(*)      Difference: data - fitted
C-----------------------------------------------------------------------
      INTEGER   NDATA, NIF, MA, MFIT, NCVM, LISTA(*), NF, IRET
      DOUBLE PRECISION HA(*), DEC(*), EL(*), TI(*)
      REAL      Y(NIF,*), A(*), COVAR(NCVM,NCVM), CHISQ, YFIT(NIF,*),
     *   YDIF(NIF,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MMAX
      PARAMETER (MMAX=5+2*MAXIF)
      INTEGER   I, J, K, L, KK, IHIT
      REAL      BETA(MMAX), AFUNC(MMAX), YM, WT, SUM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      KK = MFIT + 1
      DO 15 J = 1,MA
         IHIT = 0
         DO 10 K = 1,MFIT
            IF (LISTA(K).EQ.J) IHIT = IHIT + 1
 10         CONTINUE
         IF (IHIT.EQ.0) THEN
            LISTA(KK) = J
            KK = KK + 1
         ELSE IF (IHIT.GT.1) THEN
            IRET = 2
            GO TO 900
            END IF
 15      CONTINUE
      IRET = 1
      IF (KK.NE.MA+1) GO TO 900
      IRET = 0
      DO 25 J = 1,MFIT
         DO 20 K = 1,MFIT
            COVAR(J,K) = 0.
 20         CONTINUE
         BETA(J) = 0.
 25      CONTINUE
C
      DO 45 I = 1,NDATA
        DO 40 L = 1,NIF
           CALL FUNCS (HA(I), DEC(I), EL(I), TI(I), L, NF, AFUNC, MA)
           YM = Y(L,I)
           IF (YM.NE.FBLANK) THEN
              DO 35 J = 1,MFIT
                 WT = AFUNC (LISTA(J))
                 DO 30 K = 1,J
                    COVAR(J,K) = COVAR(J,K) + WT * AFUNC(LISTA(K))
 30                 CONTINUE
                 BETA(J) = BETA(J) + YM * WT
 35              CONTINUE
              END IF
 40        CONTINUE
 45     CONTINUE
C
      IF (MFIT.GT.1) THEN
         DO 55 J = 2,MFIT
            DO 50 K = 1,J-1
               COVAR(K,J) = COVAR(J,K)
 50            CONTINUE
 55         CONTINUE
         END IF
C
      CALL GAUSSJ (COVAR, MFIT, NCVM, BETA, 1, 1, IRET)
      IF (IRET.NE.0) GO TO 999
      DO 60 J = 1,MFIT
         A(LISTA(J)) = BETA(J)
 60      CONTINUE
C                                       find CHIsquared, etc
      CHISQ = 0.
      DO 80 I = 1,NDATA
         DO 75 L = 1,NIF
            CALL FUNCS (HA(I), DEC(I), EL(I), TI(I), L, NF, AFUNC, MA)
            SUM = 0.
            DO 65 J = 1,MA
               SUM = SUM + A(J) * AFUNC(J)
 65            CONTINUE
            YFIT(L,I) = SUM
            IF (Y(L,I).NE.FBLANK) THEN
               YDIF(L,I) = Y(L,I) - SUM
               CHISQ = CHISQ + YDIF(L,I)**2
            ELSE
               YDIF(L,I) = FBLANK
               END IF
 75         CONTINUE
 80      CONTINUE
      CALL COVSRT (COVAR, NCVM, MA, LISTA, MFIT)
      GO TO 999
C
 900  WRITE (MSGTXT,1900) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('IMPROPER FORMULATION FOR LFIT',I3)
      END
      SUBROUTINE FUNCS (HA, DEC, EL, TI, L, NF, AFUNC, MA)
C-----------------------------------------------------------------------
C   Computes projection functions
C   Inputs:
C      HA      D(2)    Hour angle radians
C      DEC     D(2)    Declination radians
C      EL      D(2)    Elevation radians
C      L       I       IF number
C      NF      I       Number samples to use
C      MA      I       Actual number of parameters
C   Outputs:
C      AFUNC   R(*)    Projection functions
C-----------------------------------------------------------------------
      DOUBLE PRECISION HA(2), DEC(2), EL(2), TI
      REAL      AFUNC(*)
      INTEGER   L, NF, MA
C-----------------------------------------------------------------------
      IF (L.EQ.1) THEN
         IF (NF.EQ.2) THEN
            AFUNC(1) = COS(HA(2))*COS(DEC(2)) - COS(HA(1))*COS(DEC(1))
            AFUNC(2) = -SIN(HA(2))*COS(DEC(2)) + SIN(HA(1))*COS(DEC(1))
            AFUNC(3) = SIN(DEC(2)) - SIN(DEC(1))
            AFUNC(4) = COS(EL(2)) - COS(EL(1))
         ELSE
            AFUNC(1) = COS(HA(1)) * COS(DEC(1))
            AFUNC(2) = -SIN(HA(1)) * COS(DEC(1))
            AFUNC(3) = SIN(DEC(1))
            AFUNC(4) = COS(EL(1))
            AFUNC(5) = TI
            AFUNC(6) = 1.0D0
            IF (MA.GT.6) CALL RFILL (MA-6, 0.0, AFUNC(7))
            END IF
      ELSE
         AFUNC(4+L) = 0.0D0
         AFUNC(5+L) = 1.0D0
         END IF
C
 999  RETURN
      END
      SUBROUTINE COVSRT (COVAR, NCVM, MA, LISTA, MFIT)
C-----------------------------------------------------------------------
C   Reorders covariance matrix to parameter number from fit parameter
C   number
C   Inputs:
C      NCVM    I        Actual dimensions of COVAR
C      MA      I        Number of parameters
C      LISTA   I(*)     type of parameter
C      MFIT    I        Number of fit parameters
C   In/out
C      COVAR   R(*,*)   Covariance matrix
C-----------------------------------------------------------------------
      INTEGER   NCVM, MA, MFIT, LISTA(MFIT)
      REAL      COVAR(NCVM,NCVM)
C
      INTEGER   I, J
      REAL      SWAP
C-----------------------------------------------------------------------
C                                       zero upper half
      DO 20 J = 1,MA-1
         DO 10 I = J+1,MA
            COVAR(I,J) = 0.
 10         CONTINUE
 20      CONTINUE
C                                       sort into upper half
      DO 40 I = 1,MFIT-1
         DO 30 J = I+1,MFIT
            IF (LISTA(J).GT.LISTA(I)) THEN
               COVAR(LISTA(J),LISTA(I)) = COVAR(I,J)
            ELSE
               COVAR(LISTA(I),LISTA(J)) = COVAR(I,J)
               END IF
 30         CONTINUE
 40      CONTINUE
C                                       diagonal to left column
      SWAP = COVAR(1,1)
      DO 50 J = 1,MA
         COVAR(1,J) = COVAR(J,J)
         COVAR(J,J) = 0.
 50      CONTINUE
      COVAR(LISTA(1),LISTA(1)) = SWAP
C                                       left col -> sorted diagonal
      DO 60 J = 2,MFIT
         COVAR(LISTA(J),LISTA(J)) = COVAR(1,J)
 60      CONTINUE
C                                       right half to left half
      DO 80 J = 2,MA
         DO 70 I = 1,J-1
            COVAR(I,J) = COVAR(J,I)
 70         CONTINUE
 80      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GAUSSJ (A, N, NP, B, M, MP, IRET)
C-----------------------------------------------------------------------
C   Solves set of equations
C   Inputs:
C      N      I
C      NP     I
C      M      I
C      MP     I
C   In/out:
C      A      R(NP,NP)
C      B      R(NP,MP)
C      IRET   I          Error code: 8 matrix singular
C-----------------------------------------------------------------------
      INTEGER   N, NP, M, MP, IRET
      REAL      A(NP,NP), B(NP,MP)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MMAX
      PARAMETER (MMAX=4+2*MAXIF)
      INTEGER   IPIV(MMAX), INDXR(MMAX), INDXC(MMAX), I, J,  K, L, IROW,
     *   ICOL, LL
      REAL      BIG, DUM, PIVINV
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
      DO 10 J = 1,N
         IPIV(J) = 0
 10      CONTINUE
C
      DO 70 I = 1,N
         BIG = 0.
         DO 25 J = 1,N
            IF (IPIV(J).NE.1) THEN
               DO 20 K = 1,N
                  IF (IPIV(K).EQ.0) THEN
                     IF (ABS(A(J,K)).GE.BIG) THEN
                        BIG = ABS (A(J,K))
                        IROW = J
                        ICOL = K
                        END IF
                  ELSE IF (IPIV(K).GT.1) THEN
                     GO TO 900
                     END IF
 20               CONTINUE
               END IF
 25         CONTINUE
         IPIV(ICOL) = IPIV(ICOL)+1
         IF (IROW.NE.ICOL) THEN
            DO 30 L = 1,N
               DUM = A(IROW,L)
               A(IROW,L) = A(ICOL,L)
               A(ICOL,L) = DUM
 30            CONTINUE
            DO 35 L = 1,M
               DUM = B(IROW,L)
               B(IROW,L) = B(ICOL,L)
               B(ICOL,L) = DUM
 35            CONTINUE
            END IF
         INDXR(I) = IROW
         INDXC(I) = ICOL
         IF (A(ICOL,ICOL).EQ.0.) GO TO 900
         PIVINV = 1.0 / A(ICOL,ICOL)
         A(ICOL,ICOL) = 1.
         DO 40 L = 1,N
            A(ICOL,L) = A(ICOL,L) * PIVINV
 40         CONTINUE
         DO 45 L = 1,M
            B(ICOL,L) = B(ICOL,L) * PIVINV
 45         CONTINUE
         DO 60 LL = 1,N
            IF (LL.NE.ICOL) THEN
               DUM = A(LL,ICOL)
               A(LL,ICOL) = 0.
               DO 50 L = 1,N
                  A(LL,L) = A(LL,L)-A(ICOL,L)*DUM
 50               CONTINUE
               DO 55 L = 1,M
                  B(LL,L) = B(LL,L)-B(ICOL,L)*DUM
 55               CONTINUE
               END IF
 60         CONTINUE
 70      CONTINUE
C
      DO 80 L = N,1,-1
         IF (INDXR(L).NE.INDXC(L)) THEN
            DO 75 K = 1,N
               DUM = A(K,INDXR(L))
               A(K,INDXR(L)) = A(K,INDXC(L))
               A(K,INDXC(L)) = DUM
 75            CONTINUE
            END IF
 80      CONTINUE
      GO TO 999
C
 900  MSGTXT = 'GAUSJ: MATRIX SINGULAR'
      CALL MSGWRT (8)
      IRET = 8
C
 999  RETURN
      END
      SUBROUTINE LOCPLT (IPLOT, ANTS, PTS, PDIF, LAST, IRET)
C-----------------------------------------------------------------------
C   LOCPLT generates plots on the TV or in a plot file
C   Inputs
C      IPLOT   I        Number of antennas to plt this time
C      ANTS    I(*)     Antenna numbers
C      PTS     I(*)     Number points per pol/IF by antenna
C      PDIF    R(*,*)   Phase differences in turns
C      LAST    L        Last plot?
C   Output:
C      IRET    I        Quit?
C-----------------------------------------------------------------------
      INCLUDE 'LOCIT.INC'
      INTEGER   IPLOT, ANTS(*), PTS(*), IRET
      LOGICAL   LAST
      REAL      PDIF(MAXDAT,*)
C
      INTEGER   I, IAN,  LP, LF, NUMIFI, NUMIFO, INDI, INDO, NP, XPTS,
     *   LFRAME, TVCHN, TVCORN(4), DEPTH(5), LAN, JPLT, IOFF,
     *   PBUFF(256), VER, IERR, ITYPE, IPSIZE, LUNPL, LTYPE, FINDPL,
     *   INCHAR, INP, IT(3), ID(3), IAXLAB, NGOOD, NNOFIT, NN, STTYPE,
     *   NFRAME, CFRAME, PFRAME, LTTYPE, LINCOL
      REAL      RMAX(2,MAXIF,MAXPLT), RMIN(2,MAXIF,MAXPLT), XYOFF(2),
     *   XYSCL(2), PMAX, PMIN, CHOUT(4), BLC(2), TRC(2), XYRATO, DX, DY,
     *   TR, XY(2), XTRC(2), XBLC(2), PLTINC, YYOFF(2), AX(5), AY(5)
      CHARACTER PFILE*48, STCHAR(2)*1, TEXT*132, ATIME*8, ADATE*12,
     *   CHTMP*18
      HOLLERITH CATH(256)
      LOGICAL   GOOD, CATUP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'DANTE.INC'
      EQUIVALENCE (CATBLK, CATH)
      DATA TVCHN, TVCORN, DEPTH /1, 4*0, 5*1/
      DATA LUNPL /26/
      DATA STCHAR /'R','L'/
C-----------------------------------------------------------------------
C                                       conversions, max/min
      STTYPE = BPARM(6) + 0.5
      STTYPE = MAX (0, MIN (23, STTYPE))
      I = 2 * MAXIF * MAXPLT
      IF (BPARM(4).GE.BPARM(3)) THEN
         CALL RFILL (I, -1000.0, RMAX)
         CALL RFILL (I, 1000.0, RMIN)
      ELSE
         CALL RFILL (I, BPARM(3), RMAX)
         CALL RFILL (I, BPARM(4), RMIN)
         END IF
      NP = NUMPOL
      IF (NUMPOL.EQ.1) BPARM(2) = 0.0
      IF (BPARM(2).GT.0.) NP = 1
      NUMIFI = NUMPOL * NUMIFS
      NUMIFO = NP * NUMIFS
      INDI = 1
      INDO = 1
      XPTS = 0
      DO 50 IAN = 1,IPLOT
         XPTS = MAX (XPTS, PTS(IAN))
         INDI = 1
         INDO = 1
         DO 40 I = 1,PTS(IAN)
            DO 30 LF = 1,NUMIFS
C                                       RR - LL
               IF (BPARM(2).GT.0.0) THEN
                  IF ((PDIF(INDI,IAN).NE.FBLANK) .AND.
     *               (PDIF(INDI+1,IAN).NE.FBLANK)) THEN
                     PDIF(INDO,IAN) = 360.0 * (PDIF(INDI,IAN) -
     *                  PDIF(INDI+1,IAN))
                     RMAX(1,LF,IAN) = MAX (RMAX(1,LF,IAN),
     *                  PDIF(INDO,IAN))
                     RMIN(1,LF,IAN) = MIN (RMIN(1,LF,IAN),
     *                  PDIF(INDO,IAN))
                  ELSE
                     PDIF(INDO,IAN) = FBLANK
                     END IF
                  INDO = INDO + 1
                  INDI = INDI + 2
C                                       plot straight
               ELSE
                  DO 20 LP = 1,NP
                     IF (PDIF(INDI,IAN).NE.FBLANK) THEN
                        PDIF(INDO,IAN) = 360.0 * PDIF(INDI,IAN)
                        RMAX(LP,LF,IAN) = MAX (RMAX(LP,LF,IAN),
     *                     PDIF(INDO,IAN))
                        RMIN(LP,LF,IAN) = MIN (RMIN(LP,LF,IAN),
     *                     PDIF(INDO,IAN))
                     ELSE
                        PDIF(INDO,IAN) = FBLANK
                        END IF
                     INDO = INDO + 1
                     INDI = INDI + 1
 20                  CONTINUE
                  END IF
 30            CONTINUE
 40         CONTINUE
 50      CONTINUE
C                                       Fixed scale
      IF (BPARM(4).GT.BPARM(3)) THEN
         I = 2 * MAXIF * MAXPLT
         CALL RFILL (I, BPARM(4), RMAX)
         CALL RFILL (I, BPARM(3), RMIN)
C                                       Crowded
      ELSE IF (BPARM(8).GT.0.5) THEN
         DO 90 IAN = 1,IPLOT
            PMAX = -1000.
            PMIN = 1000.
            DO 60 LF = 1,NUMIFS
               IF (NP.GT.1) THEN
                  RMAX(1,LF,IAN) = MAX (RMAX(1,LF,IAN), RMAX(2,LF,IAN))
                  RMIN(1,LF,IAN) = MIN (RMIN(1,LF,IAN), RMIN(2,LF,IAN))
                  RMAX(2,LF,IAN) = RMAX(1,LF,IAN)
                  RMIN(2,LF,IAN) = RMIN(1,LF,IAN)
                  END IF
               PMAX = MAX (PMAX, RMAX(1,LF,IAN))
               PMIN = MIN (PMIN, RMIN(1,LF,IAN))
 60            CONTINUE
            IF (BPARM(8).GT.1.5) THEN
               DO 70 LF = 1,NUMIFS
                  RMAX(1,LF,IAN) = PMAX
                  RMIN(1,LF,IAN) = PMIN
                  IF (NP.GT.1) THEN
                     RMAX(2,LF,IAN) = PMAX
                     RMIN(2,LF,IAN) = PMIN
                     END IF
 70               CONTINUE
               END IF
 90         CONTINUE
         END IF
C                                       Graph drawing parameters.
      IF (BPARM(8).GT.1.5) THEN
         NFRAME = NPLOTS
      ELSE IF (BPARM(8).GT.0.5) THEN
         NFRAME = NPLOTS * NUMIFS
      ELSE
         NFRAME = NPLOTS * NUMIFO
         END IF
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1100.0
      TRC(2) = 800.0
      XYRATO = 1.0
      PLTINC = TRC(2) / NFRAME
      NFRAME = (NFRAME * IPLOT) / NPLOTS
      LFRAME = IPLOT * NUMIFO
      DOTV = XDOTV.GT.0.0
C                                       fool with location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      CPREF(1,LOCNUM) = ' '
      CTYP(1,LOCNUM) = ' '
      CPREF(2,LOCNUM) = ' '
C                                       X axis
      TR = XPTS * 1.03
      XYOFF(1) = BLC(1)
      XYSCL(1) = TRC(1) / TR
      RPLOC(1,LOCNUM) = BLC(1)
      RPVAL(I,LOCNUM) = XYOFF(1)
      AXINC(1,LOCNUM) = TR / (TRC(1) - BLC(1))
C                                       Loop over ants/IFs/pols
      DO 200 JPLT = 1,LFRAME
C                                       which Antenna, IF, pol
         LP = MOD (JPLT-1, NP) + 1
         I = (JPLT - LP) / NP
         LF = MOD (I, NUMIFS) + 1
         I = (I - LF + 1) / NUMIFS
         LAN = ANTS(I+1)
         IAN = I+1
         IF (BPARM(8).GT.1.5) THEN
            CFRAME = IAN
            PFRAME = (LF - 1) * NP + LP
         ELSE IF (BPARM(8).GT.0.5) THEN
            CFRAME = (IAN - 1) * NUMIFS + LF
            PFRAME = LP
         ELSE
            CFRAME = JPLT
            PFRAME = 1
            END IF
         LINCOL = 5 - PFRAME
         IF (LINCOL.LE.0) LINCOL = LINCOL + 4
         IF (LINCOL.LE.0) LINCOL = LINCOL + 4
         IF (LINCOL.LE.0) LINCOL = LINCOL + 4
         IF (LINCOL.LE.0) LINCOL = LINCOL + 4
         IF (LINCOL.LE.0) LINCOL = LINCOL + 4
C                                       Set window for current plot.
         XBLC(1) = BLC(1)
         XBLC(2) = TRC(2) - ABS (CFRAME) * PLTINC
         XTRC(1) = TRC(1)
         XTRC(2) = XBLC(2) + PLTINC - 1.0
         YYOFF(1) = XBLC(1)
         YYOFF(2) = XBLC(2)
C                                       Y axis
         PMAX = RMAX(LP,LF,IAN) + 0.1*(RMAX(LP,LF,IAN)-RMIN(LP,LF,IAN))
         PMIN = RMIN(LP,LF,IAN) - 0.1*(RMAX(LP,LF,IAN)-RMIN(LP,LF,IAN))
         TR = PMAX - PMIN
         IF (TR.LE.1.0) THEN
            PMAX = PMAX + 1.
            PMIN = PMIN - 1.
            TR = TR + 2.0
            END IF
         XYOFF(2) = PMIN
         XYSCL(2) = PLTINC / TR
         RPLOC(2,LOCNUM) = XBLC(2)
         RPVAL(2,LOCNUM) = XYOFF(2)
         AXINC(2,LOCNUM) = TR / (XTRC(2) - XBLC(2))
         CATUP = .TRUE.
C                                       Create plot file
         IF (JPLT.EQ.1) THEN
C                                       Update catalog header.
            VER = 0
            IRET = 1
            IF (.NOT.DOTV) THEN
               CALL MADDEX ('PL', DISKIN, CNOIN, CATBLK, PBUFF, CATUP,
     *            'WRIT', VER, IERR)
               IF (IERR.NE.0) THEN
                  NCFILE = NCFILE - 1
                  GO TO 999
                  END IF
               END IF
            CALL ZPHFIL ('PL', DISKIN, CNOIN, VER, PFILE, IERR)
            IF (IERR.NE.0) GO TO 960
            IPSIZE = 0
            ITYPE = 1
            CALL GINIT (DISKIN, CNOIN, PFILE, IPSIZE, ITYPE, NPARMS,
     *         XUSER, DOTV, TVCHN, GRCHAN, TVCORN, CATBLK, PBUFF, LUNPL,
     *         FINDPL, IRET)
            IF (IRET.NE.0) GO TO 960
C                                       Number of characters on each
C                                       side of the plot
            CALL RFILL (4, 0.5, CHOUT)
C                                       Not fully initialized, may make
C                                       INP too large which is okay.
            CALL CHNTIC (XBLC, XTRC, INP)
            INP = MAX (INP, 3)
            LTYPE = 3
            CHOUT(1) = INP + 4
            CHOUT(2) = 3.333
            CHOUT(4) = 3.333
C                                       Init for line drawing.
            CALL GINITL (BLC, TRC, XYRATO, CHOUT, DEPTH, PBUFF, IERR)
            IRET = 3
            IF (IERR.NE.0) GO TO 970
            IF (.NOT.DOTV) THEN
               WRITE (MSGTXT,1100) VER
               CALL MSGWRT (2)
               END IF
            END IF
         IRET = 3
C                                       Draw border
         CALL GLTYPE (1, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL GPOS (XBLC(1), XTRC(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL GVEC (XBLC(1), XBLC(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL GVEC (XTRC(1), XBLC(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL GVEC (XTRC(1), XTRC(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL GVEC (XBLC(1), XTRC(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Draw zero
         IF ((PMAX.GT.0.0) .AND. (PMIN.LT.0.0)) THEN
            XY(2) = - XYSCL(2) * XYOFF(2) + YYOFF(2)
            CALL GPOS (XBLC(1), XY(2), PBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL GVEC (XTRC(1), XY(2), PBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
C                                       Top labels: type & name
         IF (JPLT.EQ.1) THEN
            DX = 0.0
            DY = 0.5
C                                       The second line of the header
            CALL GPOS (BLC(1), TRC(2), PBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            IF (BPARM(2).GT.0.0) THEN
               TEXT = 'R - L phase difference'
            ELSE
               TEXT = 'Obs - model phases'
               END IF
            INCHAR = 24
            INP = 1
            CALL CHTRIM (TEXT, INCHAR, TEXT, INP)
            INP = INP + 1
            TEXT(INP:) = ' vs scan number'
            INP = INP + 16
C                                       File name
            CALL H2CHR (18, KHIMNO, CATH(KHIMN), CHTMP)
            CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTMP(13:18))
            CALL NAMEST (CHTMP, CATBLK(KIIMS), TEXT(INP:), INCHAR)
            CALL REFRMT (TEXT, ' ', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
C                                       Date/time/version
            DY = 0.5 + 1.333
C                                       the first line of the header
            CALL GPOS (BLC(1), TRC(2), PBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, ATIME, ADATE)
            WRITE (TEXT,1110) VER, ADATE, ATIME
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
C                                       station ID
         CALL GPOS (XBLC(1), XTRC(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         DX =  1.5
         DY = -1.8
         IF ((BPARM(8).GT.1.5) .AND. (NUMIFS.GT.1)) THEN
            IF (LAN.LE.9) THEN
               WRITE (TEXT,1120) STNNAM(LAN), LAN, BIF, EIF
            ELSE
               WRITE (TEXT,1121) STNNAM(LAN), LAN, BIF, EIF
               END IF
         ELSE
            IOFF = LF + BIF - 1
            IF (LAN.LE.9) THEN
               WRITE (TEXT,1125) STNNAM(LAN), LAN, IOFF
            ELSE
               WRITE (TEXT,1126) STNNAM(LAN), LAN, IOFF
               END IF
            END IF
         CALL REFRMT (TEXT, '_', INCHAR)
         IF (BPARM(2).GT.0.0) THEN
            TEXT(INCHAR+1:) = 'R-L'
            INCHAR = INCHAR + 3
         ELSE IF (BPARM(8).GT.0.5) THEN
            TEXT(INCHAR+1:) = 'R&L'
            INCHAR = INCHAR + 3
         ELSE IF (NUMPOL.GT.1) THEN
            TEXT(INCHAR+1:) = STCHAR(LP)
            INCHAR = INCHAR + 1
         ELSE
            TEXT(INCHAR+1:) = STOKES(:1)
            INCHAR = INCHAR + 1
            END IF
         CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Set up location common
C                                       Blank bottom label.
         IF (JPLT.EQ.LFRAME) CTYP(1,LOCNUM) = 'SCAN'
         IF (PFRAME.EQ.1) THEN
            CTYP(2,LOCNUM) = 'PHASEDIF'
         ELSE
            CTYP(2,LOCNUM) = ' '
            END IF
         IAXLAB = NFRAME / 2 + 1
         CPREF(2,LOCNUM) = '-1'
         IF ((CFRAME.EQ.IAXLAB) .AND. (PFRAME.EQ.1)) CPREF(2,LOCNUM) =
     *      ' '
C                                       Put on labels and ticks
         CALL CLAB1 (XBLC, XTRC, CHOUT, LTYPE, XYRATO, .FALSE., PBUFF,
     *      IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Loop
         IOFF = LP + (LF - 1) * NP
         DY = 2.0 * BPARM(5)
         DX = 2.0 * BPARM(5)
         IF (STTYPE.GT.0) THEN
            LTTYPE = STTYPE + PFRAME
         ELSE
            LTTYPE = 1
            END IF
         CALL GLTYPE (LINCOL, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         DO 130 NN = 1,PTS(IAN)
C                                       Y value
            IF (PDIF(IOFF,IAN).NE.FBLANK) THEN
C                                       Scale X
               XY(1) = NN
               XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
               XY(2) = PDIF(IOFF,IAN)
               XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
               IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1)) .OR.
     *            (XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))) THEN
                  NNOFIT = NNOFIT + 1
C                                       Mark point
               ELSE
                  NGOOD = NGOOD + 1
                  IF (BPARM(7).GE.-0.5) THEN
                     AX(1) = XY(1)
                     AX(2) = AX(1)
                     AX(3) = AX(1)
                     AX(4) = AX(1) - DX
                     AX(5) = AX(1) + DX
                     AY(1) = XY(2)
                     AY(2) = AY(1) + DY
                     AY(3) = AY(1) - DY
                     AY(4) = AY(1)
                     AY(5) = AY(1)
                     CALL PNTPLT (LTTYPE, AX, AY, XBLC, XTRC, .FALSE.,
     *                  .FALSE., PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     END IF
                  END IF
               END IF
            IOFF = IOFF + NUMIFO
 130        CONTINUE
C                                       connected lines
         IF (ABS(BPARM(7)).GT.1.5) THEN
            IOFF = LP + (LF - 1) * NP
            GOOD = .FALSE.
            DO 140 NN = 1,PTS(IAN)
C                                       Y value
               IF (PDIF(IOFF,IAN).NE.FBLANK) THEN
C                                       Scale X
                  XY(1) = NN
                  XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
                  XY(2) = PDIF(IOFF,IAN)
                  XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
                  IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1)) .OR.
     *               (XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))) THEN
                     GOOD = .FALSE.
C                                       Mark point
                  ELSE
                     XY(1) = XY(1) - 0.5 * XYSCL(1)
                     IF (GOOD) THEN
                        CALL GVEC (XY(1), XY(2), PBUFF, IERR)
                     ELSE
                        CALL GPOS (XY(1), XY(2), PBUFF, IERR)
                        END IF
                     IF (IERR.NE.0) GO TO 970
                     XY(1) = XY(1) + XYSCL(1)
                     CALL GVEC (XY(1), XY(2), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     GOOD = .TRUE.
                     END IF
               ELSE
                  GOOD = .FALSE.
                  END IF
               IOFF = IOFF + NUMIFO
 140           CONTINUE
         ELSE IF (ABS(BPARM(7)).GT.0.5) THEN
            IOFF = LP + (LF - 1) * NP
            GOOD = .FALSE.
            DO 150 NN = 1,PTS(IAN)
C                                       Y value
               IF (PDIF(IOFF,IAN).NE.FBLANK) THEN
C                                       Scale X
                  XY(1) = NN
                  XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
                  XY(2) = PDIF(IOFF,IAN)
                  XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
                  IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1)) .OR.
     *               (XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))) THEN
                     GOOD = .FALSE.
C                                       Mark point
                  ELSE
                     IF (GOOD) THEN
                        CALL GVEC (XY(1), XY(2), PBUFF, IERR)
                     ELSE
                        CALL GPOS (XY(1), XY(2), PBUFF, IERR)
                        END IF
                     IF (IERR.NE.0) GO TO 970
                     GOOD = .TRUE.
                     END IF
               ELSE
                  GOOD = .FALSE.
                  END IF
               IOFF = IOFF + NUMIFO
 150           CONTINUE
            END IF
 200     CONTINUE
C                                       Done: finish plot
      WRITE (MSGTXT,1200) NGOOD
      CALL MSGWRT (2)
      IF (NNOFIT.GE.1) THEN
         WRITE (MSGTXT,1202) NNOFIT
         CALL MSGWRT (2)
         END IF
      IF (JPLT.GE.LFRAME) THEN
         GPHPAG = .NOT.LAST
         CALL GFINIS (PBUFF, IRET)
         IF (IRET.GT.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, CNOIN, VER, PBUFF, IERR)
            IERR = 0
            END IF
         END IF
      GO TO 999
C                                       ZPHFIL or GINIT failure.
 960  WRITE (MSGTXT,1960)
      CALL MSGWRT (8)
      IF (.NOT.DOTV) THEN
         CALL DELEXT ('PL', DISKIN, CNOIN, 'WRIT', CATBLK, PBUFF,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
      GO TO 999
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      WRITE (MSGTXT,1200) NGOOD
      CALL MSGWRT (2)
      IF (NNOFIT.GE.1) THEN
         WRITE (MSGTXT,1202) NNOFIT
         CALL MSGWRT (2)
         END IF
      GPHPAG = JPLT.GT.0
      CALL GFINIS (PBUFF, IERR)
      IF (IERR.NE.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, CNOIN, VER, PBUFF, IERR)
            IERR = 0
            END IF
         GO TO 999
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKIN, PFILE, IERR)
         CALL DELEXT ('PL', DISKIN, CNOIN, 'WRIT', CATBLK, PBUFF,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Plot file version',I4,'  created.')
 1110 FORMAT ('Plot file version',I4,'__created ',A, A)
 1120 FORMAT (A,'(',I1,')__IFS',I3,' -',I3)
 1121 FORMAT (A,'(',I2,')__IFS',I3,' -',I3)
 1125 FORMAT (A,'(',I1,')__IF',I3)
 1126 FORMAT (A,'(',I2,')__IF',I3)
 1200 FORMAT ('LOCPLT:',I9,' points plotted')
 1202 FORMAT ('LOCPLT:',I9,' points did not fit')
 1960 FORMAT ('LOCPLT: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('LOCPLT: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
      END
      SUBROUTINE LOCSPL (NSAMP, PADNAM)
C-----------------------------------------------------------------------
C   Summary plot
C-----------------------------------------------------------------------
      INTEGER   NSAMP(*)
      CHARACTER PADNAM(*)*(*)
C
      INCLUDE 'LOCIT.INC'
      INTEGER   NSMIN, NSMAX, I, J, IS, JJ(MAXANT), JS(MAXANT), NGOOD,
     *   JPLT, PBUFF(256), NFRAME, VER, IRET, IERR, IPSIZE, ITYPE,
     *   TVCHN, DEPTH(5), TVCORN(4), LUNPL, FINDPL, INP, LTYPE, INCHAR,
     *   IT(3), ID(3), ITRIM, LTTYPE
      LOGICAL   CATUP
      REAL      BBMIN(3), BBMAX(3), DX, DY, XYSCL(2), XYOFF(2), PMAX,
     *   PMIN, CHOUT(4), BLC(2), TRC(2), XYRATO, TR, XY(2), XTRC(2),
     *   XBLC(2), PLTINC, AX(5), AY(5), YYOFF(2), RMAX, RMID, RMIN
      CHARACTER PFILE*48, TEXT*132, ATIME*8, ADATE*12, CHTMP*18,
     *   ARM(3)*1
      HOLLERITH CATH(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (CATBLK, CATH)
      DATA TVCHN, TVCORN, DEPTH, LUNPL /1, 4*0, 5*1, 26/
      DATA ARM /'W','E','N'/
C-----------------------------------------------------------------------
      NSMIN = 1000
      NSMAX = 0
      CALL RFILL (3, 1.E6, BBMIN)
      CALL RFILL (3, -1.E6, BBMAX)
      DO 10 I = 1,NANTSL
         JJ(I) = 0
         JS(I) = -1
         IF (NSAMP(I).GT.0) THEN
            IF (PADNAM(I)(2:2).EQ.ARM(1)) THEN
               JJ(I) = 1
            ELSE IF (PADNAM(I)(2:2).EQ.ARM(2)) THEN
               JJ(I) = 2
            ELSE IF (PADNAM(I)(2:2).EQ.ARM(3)) THEN
               JJ(I) = 3
               END IF
            END IF
         IF (JJ(I).GT.0) THEN
            READ (PADNAM(I)(3:4),1000) IS
            JS(I) = IS
            NSMIN = MIN (NSMIN, IS)
            NSMAX = MAX (NSMAX, IS)
            BBMIN(JJ(I)) = MIN (BBMIN(JJ(I)), OLDBX(I)-SIGBX(I))
            BBMIN(JJ(I)) = MIN (BBMIN(JJ(I)), OLDBY(I)-SIGBY(I))
            BBMIN(JJ(I)) = MIN (BBMIN(JJ(I)), OLDBZ(I)-SIGBZ(I))
            BBMAX(JJ(I)) = MAX (BBMAX(JJ(I)), OLDBX(I)+SIGBX(I))
            BBMAX(JJ(I)) = MAX (BBMAX(JJ(I)), OLDBY(I)+SIGBY(I))
            BBMAX(JJ(I)) = MAX (BBMAX(JJ(I)), OLDBZ(I)+SIGBZ(I))
            END IF
 10      CONTINUE
      IF (NSMIN.GT.NSMAX) THEN
         MSGTXT = 'LOCSPL: NO POINTS FOUND'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IF (NSMIN.LT.0.2*NSMAX) NSMIN = 0
      NSMAX = NSMAX + 2
      NFRAME = 3
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1100.0
      TRC(2) = 800.0
      XYRATO = 1.0
      PLTINC = TRC(2) / NFRAME
      DOTV = XDOTV.GT.0.0
C                                       fool with location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      CPREF(1,LOCNUM) = ' '
      CTYP(1,LOCNUM) = ' '
      CPREF(2,LOCNUM) = ' '
C                                       X axis
      TR = NSMAX - NSMIN
      XYOFF(1) = BLC(1)
      XYSCL(1) = TRC(1) / TR
      RPLOC(1,LOCNUM) = BLC(1)
      RPVAL(I,LOCNUM) = XYOFF(1)
      AXINC(1,LOCNUM) = TR / (TRC(1) - BLC(1))
      IF (BPARM(10).LE.0.2) BPARM(10) = 1.0
C                                       Loop over ants/IFs/pols
      NGOOD = 0
      DO 100 JPLT = 1,NFRAME
C                                       Set window for current plot.
         XBLC(1) = BLC(1)
         XBLC(2) = TRC(2) - (4-JPLT) * PLTINC
         XTRC(1) = TRC(1)
         XTRC(2) = XBLC(2) + PLTINC - 1.0
         YYOFF(1) = XBLC(1)
         YYOFF(2) = XBLC(2)
C                                       Y axis
         IF (BBMAX(JPLT)-BBMIN(JPLT).LT.1.0) THEN
            BBMAX(JPLT) = BBMAX(JPLT) + 0.75
            BBMIN(JPLT) = BBMIN(JPLT) - 0.75
            END IF
         PMAX = BBMAX(JPLT) + 0.15 * (BBMAX(JPLT) - BBMIN(JPLT))
         PMIN = BBMIN(JPLT) - 0.15 * (BBMAX(JPLT) - BBMIN(JPLT))
         TR = PMAX - PMIN
         XYOFF(2) = PMIN
         XYSCL(2) = PLTINC / TR
         RPLOC(2,LOCNUM) = XBLC(2)
         RPVAL(2,LOCNUM) = XYOFF(2)
         AXINC(2,LOCNUM) = TR / (XTRC(2) - XBLC(2))
         CATUP = .TRUE.
C                                       Create plot file
         IF (JPLT.EQ.1) THEN
C                                       Update catalog header.
            VER = 0
            IRET = 1
            IF (.NOT.DOTV) THEN
               CALL MADDEX ('PL', DISKIN, CNOIN, CATBLK, PBUFF, CATUP,
     *            'WRIT', VER, IERR)
               IF (IERR.NE.0) THEN
                  NCFILE = NCFILE - 1
                  GO TO 999
                  END IF
               END IF
            CALL ZPHFIL ('PL', DISKIN, CNOIN, VER, PFILE, IERR)
            IF (IERR.NE.0) GO TO 960
            IPSIZE = 0
            ITYPE = 1
            CALL GINIT (DISKIN, CNOIN, PFILE, IPSIZE, ITYPE, NPARMS,
     *         XUSER, DOTV, TVCHN, GRCHAN, TVCORN, CATBLK, PBUFF, LUNPL,
     *         FINDPL, IRET)
            IF (IRET.NE.0) GO TO 960
C                                       Number of characters on each
C                                       side of the plot
            CALL RFILL (4, 0.5, CHOUT)
C                                       Not fully initialized, may make
C                                       INP too large which is okay.
            CALL CHNTIC (XBLC, XTRC, INP)
            INP = MAX (INP, 3)
            LTYPE = 3
            CHOUT(1) = INP + 5
            CHOUT(2) = 3.333
            CHOUT(4) = 3.333
C                                       Init for line drawing.
            CALL GINITL (BLC, TRC, XYRATO, CHOUT, DEPTH, PBUFF, IERR)
            IRET = 3
            IF (IERR.NE.0) GO TO 970
            IF (.NOT.DOTV) THEN
               WRITE (MSGTXT,1100) VER
               CALL MSGWRT (2)
               END IF
            END IF
         IRET = 3
C                                       Draw border
         CALL GLTYPE (1, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL GPOS (XBLC(1), XTRC(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL GVEC (XBLC(1), XBLC(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL GVEC (XTRC(1), XBLC(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL GVEC (XTRC(1), XTRC(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL GVEC (XBLC(1), XTRC(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Draw zero
         IF ((PMAX.GT.0.0) .AND. (PMIN.LT.0.0)) THEN
            XY(2) = - XYSCL(2) * XYOFF(2) + YYOFF(2)
            CALL GPOS (XBLC(1), XY(2), PBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL GVEC (XTRC(1), XY(2), PBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
C                                       Top labels: type & name
         IF (JPLT.EQ.1) THEN
            DX = 0.0
            DY = 0.5
C                                       The second line of the header
            CALL GPOS (BLC(1), TRC(2), PBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            TEXT = 'Baseline offsets vs arm position'
            INP = ITRIM (TEXT) + 1
            TEXT(INP:) = '___'
            INP = INP + 3
C                                       File name
            CALL H2CHR (18, KHIMNO, CATH(KHIMN), CHTMP)
            CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTMP(13:18))
            CALL NAMEST (CHTMP, CATBLK(KIIMS), TEXT(INP:), INCHAR)
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
C                                       Date/time/version
            DY = 0.5 + 1.333
C                                       the first line of the header
            CALL GPOS (BLC(1), TRC(2), PBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, ATIME, ADATE)
            WRITE (TEXT,1110) VER, ADATE, ATIME
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
C                                       Set up location common
C                                       Blank bottom label.
         IF (JPLT.EQ.1) THEN
            CTYP(1,LOCNUM) = 'PAD number'
            CPREF(1,LOCNUM) = ' '
         ELSE
            CTYP(1,LOCNUM) = ' '
            CPREF(1,LOCNUM) = ' '
            END IF
         CTYP(2,LOCNUM) = 'Offset (mm)  ' // ARM(JPLT) // ' arm'
         CPREF(2,LOCNUM) =  ' '
C                                       Put on labels and ticks
         CALL CLAB1 (XBLC, XTRC, CHOUT, LTYPE, XYRATO, .FALSE., PBUFF,
     *      IERR)
         IF (IERR.NE.0) GO TO 970
C                                       antenna labels: left to right
         DO 20 IS = 1,NSMAX-1
            DO 15 I = 1,NANTSL
               IF ((JJ(I).EQ.JPLT) .AND. (JS(I).EQ.IS)) THEN
                  WRITE (TEXT,1000) ANTLAB(I)
                  CALL CHTRIM (TEXT, 2, TEXT, J)
C                                       where does it go
                  RMAX = MAX (OLDBX(I), OLDBY(I))
                  RMAX = MAX (RMAX, OLDBZ(I))
                  RMIN = MIN (OLDBX(I), OLDBY(I))
                  RMIN = MIN (RMIN, OLDBZ(I))
                  RMID = RMAX
                  IF ((OLDBX(I).NE.RMAX) .AND. (OLDBX(I).NE.RMIN)) THEN
                     RMID = OLDBX(I)
                  ELSE IF ((OLDBY(I).NE.RMAX) .AND. (OLDBY(I).NE.RMIN))
     *                  THEN
                     RMID = OLDBY(I)
                  ELSE IF ((OLDBZ(I).NE.RMAX) .AND. (OLDBZ(I).NE.RMIN))
     *                  THEN
                     RMID = OLDBZ(I)
                     END IF
                  DX = RMIN - BBMIN(JPLT)
                  DY = (RMIN + BBMIN(JPLT)) / 2.0
                  IF (RMID-RMIN.GT.DX) THEN
                     DX = RMID - RMIN
                     DY = (RMID + RMIN) / 2.0
                     END IF
                  IF (RMAX-RMID.GT.DX) THEN
                     DX = RMAX - RMID
                     DY = (RMAX + RMID) / 2.0
                     END IF
                  IF (BBMAX(JPLT)-RMAX.GT.DX) THEN
                     DX = BBMAX(JPLT) - RMAX
                     DY = (BBMAX(JPLT) + RMAX) / 2.0
                     END IF
                  XY(1) = XYSCL(1) * (JS(I) - XYOFF(1)) + YYOFF(1)
                  XY(2) = XYSCL(2) * (DY - XYOFF(2)) + YYOFF(2)
                  CALL GPOS (XY(1), XY(2), PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 970
                  DX = -J / 2.0
                  DY = -0.5
                  CALL GCHAR (J, 0, DX, DY, TEXT(:J), PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 970
                  END IF
 15            CONTINUE
 20         CONTINUE
         RMID = (PMAX + PMIN) / 2.0
         IF (ABS(RMID).LT.0.2) RMID = PMIN / 2.0
C                                       plot points: X circle
         DY = 4.0 * BPARM(10)
         DX = 4.0 * BPARM(10)
         CALL GLTYPE (2, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         LTTYPE = 3
         DO 30 I = 1,NANTSL
            IF (JJ(I).EQ.JPLT) THEN
               XY(1) = XYSCL(1) * (JS(I) - XYOFF(1)) + YYOFF(1)
               XY(2) = OLDBX(I)
               XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
               IF ((XY(1).GT.XBLC(1)) .AND. (XY(1).LT.XTRC(1)) .AND.
     *            (XY(2).GT.XBLC(2)) .AND. (XY(2).LT.XTRC(2))) THEN
                  AX(1) = XY(1)
                  AX(2) = AX(1)
                  AX(3) = AX(1)
                  AX(4) = AX(1) - DX
                  AX(5) = AX(1) + DX
                  AY(1) = XY(2)
                  AY(2) = AY(1) + DY
                  AY(3) = AY(1) - DY
                  AY(4) = AY(1)
                  AY(5) = AY(1)
                  CALL PNTPLT (LTTYPE, AX, AY, XBLC, XTRC, .FALSE.,
     *               .FALSE., PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 970
                  NGOOD = NGOOD + 1
                  IF (SIGBX(I)*XYSCL(2).GT.DY*1.5) THEN
                     AY(1) = OLDBX(I) + SIGBX(I)
                     AY(1) = XYSCL(2) * (AY(1) - XYOFF(2)) + YYOFF(2)
                     AY(2) = OLDBX(I) - SIGBX(I)
                     AY(2) = XYSCL(2) * (AY(2) - XYOFF(2)) + YYOFF(2)
                     CALL GPOS (AX(4), AY(1), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(5), AY(1), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(1), AY(1), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(1), AY(2), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(5), AY(2), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(4), AY(2), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     END IF
                  END IF
               END IF
 30         CONTINUE
         XY(1) = NSMAX - 1.2
         XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
         XY(2) = PMAX - 0.075 * (PMAX - PMIN)
         XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
         AX(1) = XY(1)
         AX(2) = AX(1)
         AX(3) = AX(1)
         AX(4) = AX(1) - DX
         AX(5) = AX(1) + DX
         AY(1) = XY(2)
         AY(2) = AY(1) + DY
         AY(3) = AY(1) - DY
         AY(4) = AY(1)
         AY(5) = AY(1)
         CALL PNTPLT (LTTYPE, AX, AY, XBLC, XTRC, .FALSE., .FALSE.,
     *      PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         XY(1) = NSMAX - 0.75 + 0.05 * BPARM(10)
         XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
         CALL GPOS (XY(1), XY(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL GCHAR (1, 0, -0.5, -0.5, 'X', PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       plot points: Y square
         CALL GLTYPE (3, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         LTTYPE = 4
         DO 40 I = 1,NANTSL
            IF (JJ(I).EQ.JPLT) THEN
               XY(1) = XYSCL(1) * (JS(I) - XYOFF(1)) + YYOFF(1)
               XY(2) = OLDBY(I)
               XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
               IF ((XY(1).GT.XBLC(1)) .AND. (XY(1).LT.XTRC(1)) .AND.
     *            (XY(2).GT.XBLC(2)) .AND. (XY(2).LT.XTRC(2))) THEN
                  AX(1) = XY(1)
                  AX(2) = AX(1)
                  AX(3) = AX(1)
                  AX(4) = AX(1) - DX
                  AX(5) = AX(1) + DX
                  AY(1) = XY(2)
                  AY(2) = AY(1) + DY
                  AY(3) = AY(1) - DY
                  AY(4) = AY(1)
                  AY(5) = AY(1)
                  CALL PNTPLT (LTTYPE, AX, AY, XBLC, XTRC, .FALSE.,
     *               .FALSE., PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 970
                  NGOOD = NGOOD + 1
                  IF (SIGBY(I)*XYSCL(2).GT.DY*1.5) THEN
                     AY(1) = OLDBY(I) + SIGBY(I)
                     AY(1) = XYSCL(2) * (AY(1) - XYOFF(2)) + YYOFF(2)
                     AY(2) = OLDBY(I) - SIGBY(I)
                     AY(2) = XYSCL(2) * (AY(2) - XYOFF(2)) + YYOFF(2)
                     CALL GPOS (AX(4), AY(1), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(5), AY(1), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(1), AY(1), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(1), AY(2), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(5), AY(2), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(4), AY(2), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     END IF
                  END IF
               END IF
 40         CONTINUE
         XY(1) = NSMAX -1.2
         XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
         XY(2) = PMAX - 0.15 * (PMAX - PMIN)
         XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
         AX(1) = XY(1)
         AX(2) = AX(1)
         AX(3) = AX(1)
         AX(4) = AX(1) - DX
         AX(5) = AX(1) + DX
         AY(1) = XY(2)
         AY(2) = AY(1) + DY
         AY(3) = AY(1) - DY
         AY(4) = AY(1)
         AY(5) = AY(1)
         CALL PNTPLT (LTTYPE, AX, AY, XBLC, XTRC, .FALSE., .FALSE.,
     *      PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         XY(1) = NSMAX - 0.75 + 0.05 * BPARM(10)
         XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
         CALL GPOS (XY(1), XY(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL GCHAR (1, 0, -0.5, -0.5, 'Y', PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       plot points: Z diamond
         CALL GLTYPE (4, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         LTTYPE = 6
         DO 50 I = 1,NANTSL
            IF (JJ(I).EQ.JPLT) THEN
               XY(1) = XYSCL(1) * (JS(I) - XYOFF(1)) + YYOFF(1)
               XY(2) = OLDBZ(I)
               XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
               IF ((XY(1).GT.XBLC(1)) .AND. (XY(1).LT.XTRC(1)) .AND.
     *            (XY(2).GT.XBLC(2)) .AND. (XY(2).LT.XTRC(2))) THEN
                  AX(1) = XY(1)
                  AX(2) = AX(1)
                  AX(3) = AX(1)
                  AX(4) = AX(1) - DX
                  AX(5) = AX(1) + DX
                  AY(1) = XY(2)
                  AY(2) = AY(1) + DY
                  AY(3) = AY(1) - DY
                  AY(4) = AY(1)
                  AY(5) = AY(1)
                  CALL PNTPLT (LTTYPE, AX, AY, XBLC, XTRC, .FALSE.,
     *               .FALSE., PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 970
                  NGOOD = NGOOD + 1
                  IF (SIGBZ(I)*XYSCL(2).GT.DY*1.5) THEN
                     AY(1) = OLDBZ(I) + SIGBZ(I)
                     AY(1) = XYSCL(2) * (AY(1) - XYOFF(2)) + YYOFF(2)
                     AY(2) = OLDBZ(I) - SIGBZ(I)
                     AY(2) = XYSCL(2) * (AY(2) - XYOFF(2)) + YYOFF(2)
                     CALL GPOS (AX(4), AY(1), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(5), AY(1), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(1), AY(1), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(1), AY(2), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(5), AY(2), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     CALL GVEC (AX(4), AY(2), PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 970
                     END IF
                  END IF
               END IF
 50         CONTINUE
         XY(1) = NSMAX - 1.2
         XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
         AX(1) = XY(1)
         AX(2) = AX(1)
         AX(3) = AX(1)
         AX(4) = AX(1) - DX
         AX(5) = AX(1) + DX
         XY(2) = PMAX - 0.225 * (PMAX - PMIN)
         XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
         AY(1) = XY(2)
         AY(2) = AY(1) + DY
         AY(3) = AY(1) - DY
         AY(4) = AY(1)
         AY(5) = AY(1)
         CALL PNTPLT (LTTYPE, AX, AY, XBLC, XTRC, .FALSE., .FALSE.,
     *      PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         XY(1) = NSMAX - 0.75 + 0.05 * BPARM(10)
         XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
         CALL GPOS (XY(1), XY(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL GCHAR (1, 0, -0.5, -0.5, 'Z', PBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
 100     CONTINUE
C                                       Done: finish plot
      WRITE (MSGTXT,1200) NGOOD
      CALL MSGWRT (2)
      CALL GFINIS (PBUFF, IRET)
      IF (IRET.GT.0) GO TO 975
      IF (.NOT.DOTV) THEN
         CALL HIPLOT (DISKIN, CNOIN, VER, PBUFF, IERR)
         IERR = 0
         END IF
      GO TO 999
C                                       ZPHFIL or GINIT failure.
 960  WRITE (MSGTXT,1960)
      CALL MSGWRT (8)
      IF (.NOT.DOTV) THEN
         CALL DELEXT ('PL', DISKIN, CNOIN, 'WRIT', CATBLK, PBUFF,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
      GO TO 999
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      WRITE (MSGTXT,1200) NGOOD
      CALL MSGWRT (2)
      CALL GFINIS (PBUFF, IERR)
      IF (IERR.EQ.0) THEN
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, CNOIN, VER, PBUFF, IERR)
            IERR = 0
            END IF
         GO TO 999
         END IF
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKIN, PFILE, IERR)
         CALL DELEXT ('PL', DISKIN, CNOIN, 'WRIT', CATBLK, PBUFF,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I2)
 1100 FORMAT ('Plot file version',I4,'  created.')
 1110 FORMAT ('Plot file version',I4,'__created ',A, A)
 1200 FORMAT ('LOCSPL:',I9,' points plotted')
 1960 FORMAT ('LOCSPL: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('LOCSPL: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
      END
