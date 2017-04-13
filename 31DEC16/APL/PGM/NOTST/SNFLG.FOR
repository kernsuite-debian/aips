LOCAL INCLUDE 'SNFLG.INC'
C                                       Local include for SNFLG
      INCLUDE 'INCS:PUVD.INC'
      REAL      XSIN, XDISIN, XNVER, XQUAL, XTIME(8), XBAND, XFREQ,
     *   XFQID, XBIF, XEIF, DOSTOK, DOIFS, XSUBA, XANT(50), CUTOFF,
     *   XFGVER, DPARM(10)
      HOLLERITH XNAMEI(3), XCLAIN(2), XTYPE, XXSOUR(4,30), XXSTOK,
     *   XOPTYP
      CHARACTER NAMEIN*12, CLAIN*6, TYPE*2, XSOUR(30)*16, XSTOK*4,
     *   OUTFIL*48, OPTYPE*4
C                                       Program info
      REAL      TSTART, TSTOP, SELBAN
      INTEGER   SEQIN, DISKIN, CNOIN, IVER, BIF, ANTS(50), NPARMS, NID,
     *   SID(500), NANTSL, SUMSTK, SUMIF, FRQSEL,SUMAMP, GRCHN, TVCHN,
     *   TVCORN(4), XVAR, ISOU, OSOU, IANT,SUBARR, EIF, EPOL, BPOL,
     *   FGIN, FGOUT, NFLAGS, SUBA1, SUBA2
      LOGICAL   DOAWNT, DOERRB, NNODAT
      DOUBLE PRECISION SELFRQ, GNRECD(XCLRSZ/2)
C                                       SN/CL table info
      INTEGER   CLBUFF(512), NCLINR, NUMANT, NUMPOL, NUMIF, ICLRNO,
     *   KOLS(40), KOLTYP(40), KOLDIM(40), ICLUN, GNRECI(XCLRSZ),
     *   TIMKOL, DTKOL, SOUKOL, ANTKOL, SUBKOL, FRQKOL, RE1KOL, IM1KOL,
     *   WT1KOL, RE2KOL, IM2KOL, WT2KOL, REKOL1, REKOL2, IMKOL1, IMKOL2,
     *   WTKOL1, WTKOL2, TIMECL
      REAL      GNREC(XCLRSZ)
C                                       Constants
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XTYPE, XNVER,
     *   XXSOUR, XQUAL, XTIME, XXSTOK, XBAND, XFREQ, XFQID, XBIF, XEIF,
     *   DOSTOK, DOIFS, XSUBA, XANT, CUTOFF, XFGVER, XOPTYP, DPARM
      COMMON /VGNCOM/ SELFRQ, TSTART, TSTOP, SELBAN, NID, SID, NANTSL,
     *   DOAWNT, SUMAMP, SUMSTK, SUMIF, FRQSEL, SEQIN, DISKIN, CNOIN,
     *   IVER, BIF, SUBARR, ANTS, NPARMS, GRCHN, TVCHN, TVCORN, XVAR,
     *   ISOU, OSOU, IANT, DOERRB, NNODAT, EIF, EPOL, BPOL, FGIN, FGOUT,
     *   NFLAGS, SUBA1, SUBA2
      COMMON /VGNCHR/ NAMEIN, CLAIN, TYPE, XSOUR, XSTOK, OUTFIL, OPTYPE
      COMMON /TABCOM/ GNREC, CLBUFF, NCLINR, NUMANT, NUMPOL, NUMIF,
     *   ICLRNO, KOLS, KOLTYP, KOLDIM, ICLUN,
     *   REKOL1, IMKOL1, WTKOL1, REKOL2, IMKOL2, WTKOL2, TIMECL
      EQUIVALENCE (GNREC, GNRECD, GNRECI)
      EQUIVALENCE (KOLS(1), TIMKOL), (KOLS(2), DTKOL),
     *   (KOLS(3), SOUKOL),  (KOLS(4), ANTKOL),  (KOLS(5), SUBKOL),
     *   (KOLS(6), FRQKOL),  (KOLS(13), RE1KOL), (KOLS(14), IM1KOL),
     *   (KOLS(17), WT1KOL), (KOLS(26), RE2KOL), (KOLS(27), IM2KOL),
     *   (KOLS(30), WT2KOL)
C                                                          End SNFLG
LOCAL END
      PROGRAM SNFLG
C-----------------------------------------------------------------------
C! Flags data based on jumps in an SN or CL table phases
C# UV Calibration Editing
C-----------------------------------------------------------------------
C;  Copyright (C) 2001, 2003-2005, 2008-2009, 2011-2012, 2015-2016
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
C   SNFLG reads an SN extension file and write entries into a flag
C   table.
C   which can be read into UVFLG.
C   Inputs:
C      INNAME.....UV file name (name).       Standard defaults.
C      INCLASS....UV file name (class).      Standard defaults.
C      INSEQ......UV file name (seq. #).     0 => highest.
C      INDISK.....Disk unit #.               0 => any.
C      INEXT......'SN' or 'CL' table to be examined
C      INVERS.....Version number of table to plot, 0=>highest no.
C      SOURCES....Source list.  '*' = all; a "-" before a source
C                 name means all except ANY source named.
C      TIMERANG...Time range of the data to be plotted. In order:
C                 Start day, hour, min. sec,
C                 end day, hour, min. sec. Days relative to ref.
C                 date.
C      STOKES.....The desired Stokes type of the output data:
C                 'R' = RCP, 'L' = LCP, 'DIFF' = difference
C      BIF........IF to plot
C      ANTENNAS...A list of the antennas to be plotted. All 0 => all.
C                 If any number is negative then all antennas listed
C                 are NOT to be plotted and all others are.
C      OPTYPE.....JUMP, AMP
C      DPARM......(1) Maximum allowed jump between adjacent SN entries
C                     (deg)
C                 (2) Minimum time interval over which to judge jumps
C                     (sec)
C   Modified from SNPLT by M. Reid; June 6, 1997
C   Generalized somewhat from SNFLG by L. Greenhill; April 5, 1998
C   AIPSified by EWG 5/2001
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET, NWORDS, IERR, NIF, NT, NP, NS, I, TWORDS
      LONGINT   POFF, TOFF
      REAL      PDATA(2), TDATA(2)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNFLG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGN /'SNFLG '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL SNPIN (PRGN, NT, NS, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       allocate memory
      NIF = EIF - BIF + 1
      NP = EPOL - BPOL + 1
      IF (SUMSTK.EQ.3) NP = 1
C                                       flag on amplitudes
      DO 100 SUBARR = SUBA1,SUBA2
         IF (OPTYPE.EQ.'AMP') THEN
            NWORDS = (3 * NCLINR - 1) / 1024 + 1
            TWORDS = (NCLINR * NP * NIF - 1) / 1024 + 1
            CALL ZMEMRY ('GET ', TSKNAM, NWORDS, PDATA, POFF, IRET)
            IF (IRET.NE.0) GO TO 995
            CALL ZMEMRY ('GET ', TSKNAM, TWORDS, TDATA, TOFF, IRET)
            IF (IRET.NE.0) GO TO 995
C                                       write flags...
            CALL SNAFLG (NP, NIF, PDATA(1+POFF), TDATA(1+TOFF), IRET)
C                                       free up memory
            CALL ZMEMRY ('FREE', TSKNAM, TWORDS, TDATA, TOFF, IERR)
            CALL ZMEMRY ('FREE', TSKNAM, NWORDS, PDATA, POFF, IERR)
C                                       flag on divergence from (1,0)
         ELSE IF (OPTYPE.EQ.'A&P') THEN
            CALL SNDFLG (NP, IRET)
C                                       flag on jumps
         ELSE
            I = (1.333 * NCLINR) / NUMANT
            NT = MAX (I, NT)
            NWORDS = (NT * NIF * NUMANT * NP - 1) / 1024 + 1
            CALL ZMEMRY ('GET ', TSKNAM, NWORDS, PDATA, POFF, IRET)
            IF (IRET.NE.0) GO TO 995
            TWORDS = (1.2 * NT + 0.9 - 1) / 1024 + 1
            CALL ZMEMRY ('GET ', TSKNAM, TWORDS, TDATA, TOFF, IRET)
            IF (IRET.NE.0) GO TO 995
C                                       write flags...
            CALL SNJFLG (NP, NIF, NUMANT, NT, PDATA(1+POFF),
     *          TDATA(1+TOFF), IRET)
C                                       free up memory
            CALL ZMEMRY ('FREE', TSKNAM, TWORDS, TDATA, TOFF, IERR)
            CALL ZMEMRY ('FREE', TSKNAM, NWORDS, PDATA, POFF, IERR)
            END IF
         IF (IRET.NE.0) GO TO 995
 100     CONTINUE
C                                       record the deeds
      IF (IRET.EQ.0) CALL SNFLGH
C                                       close down
 995  CALL DIE (IRET, CLBUFF)
C
 999  STOP
      END
      SUBROUTINE SNPIN (PRGN, NT, NS, IERR)
C-----------------------------------------------------------------------
C   Gets the input parameters for SNFLG.
C   Inputs:
C      PRGN    C*6  Program name
C   Output in common:
C      SUMSTK  I    Selected Stokes 0=both, 1=R, 2=L, 4=difference
C      SUMIF   I    Selected IF, 0=>all
C      ISTOK   I    1 = R, 2 = L
C   Output:
C      NT      I    Number of times needed
C      NS      I    Number of sources
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   NT, NS, IERR
C
      CHARACTER STAT*4, PRGN*6, TYPTMP*2
      INTEGER   IRET, BUFF(256), I, J, K, JERR, QUAL(30), NSOUR,
     *   BUFFER(512), IROUND, LUN, NSTOK, NSOU(500)
      LOGICAL T, F, MATCH
      REAL      VTIME, VTIMEO, DELTAT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      OSOU = -1
      NPARMS = 210
C                                        Get input parameters.
      CALL SETUP (PRGN, NPARMS, XNAMEI, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRET = 8
         RQUICK = .FALSE.
         GO TO 990
         END IF
C                                       Decode inputs.
C                                       characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (2, 1, XTYPE, TYPE)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
         QUAL(I) = IROUND (XQUAL)
 20      CONTINUE
      IF (TYPE.EQ.' ') TYPE ='SN'
      XTYPE = HBLANK
      CALL CHR2H (2, TYPE, 1, XTYPE)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
C                                       Integers
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      IVER = IROUND (XNVER)
      TSTART = XTIME(1) + (XTIME(2) / 24.0) + (XTIME(3) / (24.0*60.0)) +
     *   (XTIME(4) / (24.0*3600.0))
      TSTOP = XTIME(5) + (XTIME(6) / 24.0) + (XTIME(7) / (24.0*60.0)) +
     *   (XTIME(8) / (24.0*3600.0))
C                                        Defaults to plot all.
      IF (TSTART.GE.TSTOP) THEN
         TSTART = 0.0
         TSTOP = 999.0
         END IF
      FGOUT = 0
C                                       Find input catalog
      CNOIN = 1
      TYPTMP = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, TYPTMP,
     *   NLUSER, STAT, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      'UV', NLUSER
         GO TO 990
         END IF
C                                       Save name class etc.
      CALL CHR2H (12, NAMEIN, 1, XNAMEI)
      CALL CHR2H (6, CLAIN, 1, XCLAIN)
      XDISIN = DISKIN
      XSIN = SEQIN
C                                       Read catalog header
      STAT = 'WRIT'
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, STAT, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FCNO(NCFILE) = CNOIN
      FVOL(NCFILE) = DISKIN
      FRW(NCFILE) = 1
      XDISIN = DISKIN
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 990
      SEQIN = CATBLK(KIIMS)
      XSIN = SEQIN
      DELTAT = DPARM(2) / (3600.0*24.0)
C                                       IF'S
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = IROUND (XBIF)
         IF (BIF.LE.0) BIF = 1
         BIF = MIN (BIF, CATBLK(KINAX+JLOCIF))
         EIF = IROUND (XEIF)
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MIN (EIF, CATBLK(KINAX+JLOCIF))
         END IF
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) THEN
         SUBA1 = 1
         CALL FNDEXT ('AN', CATBLK, SUBA2)
      ELSE
         SUBA1 = SUBARR
         SUBA2 = SUBARR
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.GE.0) THEN
         LUN = 25
         CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *      FRQSEL, IERR)
         IF (.NOT.MATCH) THEN
            WRITE (MSGTXT,1070)
            IERR = 1
            GO TO 990
            END IF
         IF (IERR.GT.0) GO TO 999
         END IF
C                                       Look up sources
      IF (XSOUR(1).EQ.'ALL') THEN
         NID = 0
         SID(1) = 0
      ELSE
         NID = 500
         NSOUR = 30
         MSGSUP = 32000
         CALL SOURNU (XSOUR, QUAL, NSOUR, DISKIN, CNOIN, NID, BUFFER,
     *      SID, JERR)
         MSGSUP = 0
C                                       Trap problem with SU table.
         IF (JERR.NE.0) NID = 0
         IF ((NID.LE.0) .AND. (XSOUR(1).NE.' ')) NID = 1
C                                       Check if none specified
         IF ((SID(1).EQ.0) .AND. (XSOUR(1).EQ.' ')) NID = 0
         END IF
C                                       Check antennas desired.
      NANTSL = 0
      DOAWNT = T
      DO 70 J = 1,50
         ANTS(J) = IROUND (XANT(J))
         IF (ANTS(J).LT.0) DOAWNT = F
C                                       Make positive
         ANTS(J) = ABS (ANTS(J))
         DO 50 K = 1,NANTSL
            IF (ANTS(J).EQ.ANTS(K)) ANTS(J) = 0
 50         CONTINUE
C                                       Check for multiple entries
         IF (ANTS(J).GE.1) THEN
            NANTSL = NANTSL + 1
            ANTS(NANTSL) = ANTS(J)
            END IF
 70      CONTINUE
C                                       Make sure not too many
      IF (NANTSL.GT.MAXANT) NANTSL = MAXANT
C                                       Get antenna names
      CALL GETANT (DISKIN, CNOIN, 1, CATBLK, BUFFER, JERR)
C                                       Check and set Stokes
      NSTOK = CATBLK(KINAX+JLOCS)
      IF ((NSTOK.EQ.1) .AND. (ICOR0.EQ.-1)) XSTOK = 'R'
      IF (ICOR0.EQ.-2) XSTOK = 'L'
      IF (ICOR0.GT.0) XSTOK = 'I'
      BPOL = 1
      EPOL = MIN (2, NSTOK)
      IF ((XSTOK(1:1).EQ.'V') .OR. (XSTOK.EQ.'DIFF')) THEN
         IF (OPTYPE.EQ.'A&P') XSTOK = ' '
         END IF
      IF ((XSTOK(1:1).EQ.'V') .OR. (XSTOK.EQ.'DIFF')) THEN
         SUMSTK = 3
      ELSE IF (XSTOK(1:1).EQ.'L') THEN
         IF (ICOR0.EQ.-2) THEN
            EPOL = 1
         ELSE
            BPOL = 2
            END IF
         SUMSTK = 2
      ELSE
         SUMSTK = 0
         IF (XSTOK.EQ.'R') THEN
            EPOL = 1
            SUMSTK = 1
            END IF
         END IF
C                                       Open SN or CL table
      IF ((TYPE.EQ.'SN') .OR. (TYPE.EQ.'CL')) THEN
         CALL SNPOPN (IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Invalid table type
      ELSE
         IERR = 5
         MSGTXT = 'INVALID TABLE TYPE =' // TYPE
         GO TO 990
         END IF
C                                       count up times
      NT = 0
      VTIMEO = -1.E6
C                                       Loop thru data
      DO 100 J = 1,NCLINR
         ICLRNO = J
         CALL TABIO ('READ', 0, ICLRNO, GNREC, CLBUFF, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1090) IERR
            GO TO 990
C                                       Record within specified
C                                       time range ?
         ELSE IF (IERR.EQ.0) THEN
            IF (KOLTYP(TIMECL).EQ.1) THEN
               VTIME = GNRECD(TIMKOL)
            ELSE
               VTIME = GNREC(TIMKOL)
               END IF
            IF ((VTIME.GE.TSTART) .AND. (VTIME.LE.TSTOP)) THEN
               IF (VTIME-VTIMEO.GE.DELTAT) THEN
                  NT = NT + 1
                  VTIMEO = VTIME
                  END IF
C                                       Check source
               ISOU = GNRECI(SOUKOL)
               IF (NID.GT.0) THEN
                  DO 75 I = 1,NID
                     IF (ISOU.EQ.SID(I)) GO TO 80
 75                  CONTINUE
                  GO TO 100
                  END IF
 80            NSOU(ISOU) = 1
               END IF
            END IF
 100     CONTINUE
      IF (XSOUR(1).EQ.'ALL') THEN
         NS = 1
      ELSE
         NS = 0
         DO 110 J = 1,500
            IF (NSOU(J).GT.0) NS = NS + 1
 110        CONTINUE
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR;',I7,'GETTING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',I3,
     *   ' TYPE=',A2,' USER=',I4)
 1040 FORMAT ('ERROR',I3,' COPYING CATALOG HEADER')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1090 FORMAT ('SNPIN: ERROR =',I3,' FROM TABIO')
      END
      SUBROUTINE SNJFLG (NP, NI, NA, NT, PDATA, TDATA, IERR)
C-----------------------------------------------------------------------
C   SNJFLG does the JUMP opcode. SNJFLG reads the SN table to get the
C   phase information for flagging.
C   Input/Output in common:
C      TSTART   R   Start time
C      TSTOP    R   Stop time
C   Input:
C      NP       I   Number polarizations in PDATA
C      NI       I   Number IFs in PDATA
C      NA       I   Number ants in PDATA
C      NT       I   Number times allowed in PDATA
C   Output:
C      PDATA    R(*)
C      IERR     I   Error code, 0=OK else failed
C-----------------------------------------------------------------------
      INTEGER   NP, NI, NA, NT, IERR
      REAL      PDATA(NP,NI,NA,NT), TDATA(NT)
C
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL   NODATA, PFLAGS(4,2), BAD(2,MAXIF), LBAD(2), LJMP(2),
     *   JMP(2,MAXIF), NEED(MAXIF,2), QIF, QALL, QSTOK
      CHARACTER REASON*24
      INTEGER   I, ITIME, MXANT, NDPANT(MAXANT), IA, FGMAX, IROUND,
     *   BUFF1(512), BUFF2(512), LUN1, LUN2, FGKOLS(MAXFGC), J,
     *   FGNUMV(MAXFGC), OKOLS(MAXFGC), ONUMV(MAXFGC), NFGROW, IFGRNO,
     *   OFGRNO, SOURID, ANTNO(2), SUBA, FREQID, IFS(2), CHANS(2),
     *   ISUBA, IB, NPAIR, IT, MT, LIF, LP, LT, KT, NFLAG
      REAL      TIMER(2), TB, TE, VALUE(2,MAXIF), GTIME, XVARIB,
     *   VTIME, VTIMEO, DELTAT, PJUMPX, PJ
      INCLUDE 'SNFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUN1, LUN2 /46,47/
C-----------------------------------------------------------------------
      NFLAGS = 0
      CALL LFILL (NI, .TRUE., NEED(1,1))
      MT = NT
      CALL FXHDEX (CATBLK)
      CALL FNDEXT ('FG', CATBLK, FGMAX)
      FGIN = IROUND (XFGVER)
      IF ((FGIN.EQ.0) .OR. (FGIN.GT.FGMAX)) FGIN = FGMAX
      FGIN = MAX (0, FGIN)
      XFGVER = -1
      IF (FGOUT.LE.0) FGOUT = FGMAX + 1
C                                       Create new FG table
      CALL FLGINI ('WRIT', BUFF1, DISKIN, CNOIN, FGOUT, CATBLK, LUN1,
     *   OFGRNO, OKOLS, ONUMV, IERR)
      IF (IERR.NE.0) GO TO 999
      NFGROW = 0
C                                       Copy old FG table
      IF (FGIN.GT.0) THEN
         CALL FLGINI ('READ', BUFF2, DISKIN, CNOIN, FGIN, CATBLK, LUN2,
     *      IFGRNO, FGKOLS, FGNUMV, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'OLD FG TABLE NOT COPIED'
            CALL MSGWRT (7)
            GO TO 20
            END IF
         NFGROW = BUFF2(5)
C                                       Loop and copy
         DO 10 I = 1,NFGROW
            CALL TABFLG ('READ', BUFF2, IFGRNO, FGKOLS, FGNUMV, SOURID,
     *         SUBA, FREQID, ANTNO, TIMER, IFS, CHANS, PFLAGS, REASON,
     *         IERR)
            IF (IERR.GT.0) GO TO 999
            IF (IERR.EQ.0) THEN
               CALL TABFLG ('WRIT', BUFF1, OFGRNO, OKOLS, ONUMV, SOURID,
     *            SUBA, FREQID, ANTNO, TIMER, IFS, CHANS, PFLAGS,
     *            REASON, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
 10         CONTINUE
         CALL TABIO ('CLOS', 0, IFGRNO, BUFF2, BUFF2, IERR)
         WRITE (MSGTXT,1000) NFGROW, FGIN, FGOUT
         CALL MSGWRT (3)
         END IF
C                                       Find new flags
 20   MXANT= 0
      CALL FILL (MAXANT, 0, NDPANT)
      ITIME = 0
      VTIMEO = -9999
C
      NODATA = .TRUE.
      IF (DPARM(1).LT.0.1) DPARM(1) = 90.0
      IF (DPARM(2).LT.0.1) DPARM(2) = 5.0
      PJUMPX = DPARM(1)
      DELTAT = DPARM(2) / (3600.0*24.0)
C                                       Initialize actual timerange.
      TB = 1.0E5
      TE = -1.0E5
      XNVER = IVER
C                                       Loop thru data
      DO 100 J = 1,NCLINR
         ICLRNO = J
         CALL TABIO ('READ', 0, ICLRNO, GNREC, CLBUFF, IERR)
         IF (IERR.LT.0) GO TO 100
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
C                                       Record within specified
C                                       time range ?
         IF (KOLTYP(TIMECL).EQ.1) THEN
            GTIME = GNRECD(TIMKOL)
         ELSE
            GTIME = GNREC(TIMKOL)
            END IF
         IF ((GTIME.LT.TSTART) .OR. (GTIME.GT.TSTOP)) GO TO 100
C                                       Freq id
         IF (GNRECI(FRQKOL).NE.-1) THEN
            IF ((GNRECI(FRQKOL).NE.FRQSEL) .AND. (FRQSEL.GT.0))
     *         GO TO 100
            END IF
C                                       Antenna?
         IANT = GNRECI(ANTKOL)
         IF (NANTSL.GT.0) THEN
            DO 50 I = 1,NANTSL
               IF ((IANT.EQ.ANTS(I)).AND.DOAWNT) GO TO 60
               IF ((IANT.EQ.ANTS(I)).AND.(.NOT.DOAWNT)) GO TO 100
 50            CONTINUE
            IF (DOAWNT) GO TO 100
            END IF
C                                       Check source
 60      IF (NID.GT.0) THEN
            ISOU = GNRECI(SOUKOL)
            DO 70 I = 1,NID
               IF (ISOU.EQ.SID(I)) GO TO 80
 70            CONTINUE
            GO TO 100
            END IF
C                                       Subarray check
 80      ISUBA = GNRECI(SUBKOL)
         IF ((ISUBA.GT.0) .AND. (ISUBA.NE.SUBARR)) GO TO 100
C                                       Get start, stop times
         TB = MIN (TB, GTIME)
         TE = MAX (TE, GTIME)
C                                       Get value - checking weight
         CALL SNPDAT (VALUE, XVARIB)
         VTIME= XVARIB
         IF (ABS(VTIME-VTIMEO).GE.DELTAT) THEN
            ITIME = ITIME + 1
            VTIMEO = VTIME
            IF ((ITIME.GE.1) .AND. (ITIME.LE.MT)) TDATA(ITIME) =
     *         VTIME
            LP = NI * NP * NA
            CALL RFILL (LP, FBLANK, PDATA(1,1,1,ITIME))
            END IF
         IF ((ITIME.GE.1) .AND. (ITIME.LE.MT) .AND. (IANT.GE.1) .AND.
     *      (IANT.LE.NA)) THEN
            NDPANT(IANT) = NDPANT(IANT) + 1
            IF (MXANT.LT.IANT) MXANT = IANT
            DO 95 LIF = 1,NI
               DO 90 LP = 1,NP
                  PDATA(LP,LIF,IANT,ITIME) = VALUE(LP,LIF)
                  IF (VALUE(LP,LIF).NE.FBLANK) NODATA = .FALSE.
 90               CONTINUE
 95            CONTINUE
         ELSE
            WRITE (MSGTXT,1080) ITIME, IANT, MT, NA
            CALL MSGWRT (8)
            END IF
 100     CONTINUE
C                                       Check for no data
      IF (NODATA) THEN
         IERR = 6
         MSGTXT = 'NO DATA SELECTED'
         GO TO 990
         END IF
C                                       Should have all the station
C                                       based phases from SN table now
      WRITE (MSGTXT,1100) ITIME
      CALL MSGWRT (3)
      WRITE (MSGTXT,1101) MXANT
      CALL MSGWRT (3)
C                                       loop over baselines
      SOURID = 0
      SUBA = SUBARR
      FREQID = FRQSEL
      CHANS(1) = 1
      CHANS(2) = 0
      IFS(1) = 0
      IFS(2) = 0
      ANTNO(1) = 0
      ANTNO(2) = 0
      CALL LFILL (8, .TRUE., PFLAGS)
C                                       flag at start only if no timer
      IF (TSTART.EQ.0.0) THEN
         TIMER(1) = 0.0
         TIMER(2) = TDATA(1) - DELTAT * 0.99
         REASON = TSKNAM // 'start of data'
         CALL TABFLG ('WRIT', BUFF1, OFGRNO, OKOLS, ONUMV, SOURID, SUBA,
     *      FREQID, ANTNO, TIMER, IFS, CHANS, PFLAGS, REASON, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       flag at end if no time range
      IF (TSTOP.EQ.999.0) THEN
         TIMER(1) = TDATA(ITIME) + DELTAT * 0.99
         TIMER(2) = 999.0
         REASON = TSKNAM // 'end of data'
         CALL TABFLG ('WRIT', BUFF1, OFGRNO, OKOLS, ONUMV, SOURID, SUBA,
     *      FREQID, ANTNO, TIMER, IFS, CHANS, PFLAGS, REASON, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       polarization flags
      PFLAGS(1,1) = (SUMSTK.NE.2) .OR. (DOSTOK.GT.0.0)
      PFLAGS(2,1) = (SUMSTK.GT.1) .OR. (DOSTOK.GT.0.0)
      QSTOK = PFLAGS(1,1) .AND. PFLAGS(2,1)
      IF (SUMSTK.EQ.0) THEN
         PFLAGS(1,2) = DOSTOK.GT.0.0
         QSTOK = QSTOK .AND. PFLAGS(1,2)
         END IF
C                                       loop over all antennas for bad
C                                       ranges
      DO 150 IA = 1,MXANT
C                                       look at IA-IB baseline
         IF (NDPANT(IA).GT.0) THEN
            ANTNO(1) = IA
            ANTNO(2) = 0
            NFLAG = 0
            DO 125 LP = 1,NP
               DO 120 LIF = 1,NI
                  IFS(1) = LIF + BIF - 1
                  IFS(2) = IFS(1)
                  IT = 1
 105              IT = IT + 1
                  IF (IT.LT.ITIME) THEN
C                                       bad - find other end
                     IF (PDATA(LP,LIF,IA,IT-1).EQ.FBLANK) THEN
                        DO 110 LT = IT,ITIME
                           IF (PDATA(LP,LIF,IA,LT).NE.FBLANK) GO TO 115
 110                       CONTINUE
                        LT = ITIME + 1
 115                    KT = MAX (1, IT-2)
                        TIMER(1) = TDATA(KT) + 0.3 * (TDATA(IT-1) -
     *                     TDATA(KT))
                        IF (LT.GT.ITIME) THEN
                           TIMER(2) = TDATA(ITIME)
                        ELSE
                           TIMER(2) = TDATA(LT-1) + 0.7 * (TDATA(LT) -
     *                        TDATA(LT-1))
                           END IF
                        REASON = TSKNAM // 'missing phase(s)'
                        CALL TABFLG ('WRIT', BUFF1, OFGRNO, OKOLS,
     *                     ONUMV, SOURID, SUBA, FREQID, ANTNO, TIMER,
     *                     IFS, CHANS, PFLAGS(1,LP), REASON, IERR)
                        IF (IERR.NE.0) GO TO 980
                        NFLAG = NFLAG + 1
                        NFLAGS = NFLAGS + 1
                        IT = LT
                        END IF
                     GO TO 105
                     END IF
 120              CONTINUE
 125           CONTINUE
C                                       summary
            IF (NFLAG.GT.0) THEN
               WRITE (MSGTXT,1125) IA, NFLAG
               CALL MSGWRT (3)
               END IF
            END IF
 150     CONTINUE
C                                       loop over all antenna pairs
      DO 250 IA = 1,MXANT-1
         DO 240 IB = IA+1,MXANT
C                                       look at IA-IB baseline
            IF ((NDPANT(IA).GT.0) .AND. (NDPANT(IB).GT.0)) THEN
               ANTNO(1) = IA
               ANTNO(2) = IB
               NPAIR = 0
               NFLAG = 0
               DO 230 IT = 2,ITIME
                  NPAIR = NPAIR + 1
                  DO 210 LP = 1,NP
                     LBAD(LP) = .TRUE.
                     LJMP(LP) = .TRUE.
                     DO 205 LIF = 1,NI
                        BAD(LP,LIF) = (PDATA(LP,LIF,IA,IT-1).EQ.FBLANK)
     *                     .OR. (PDATA(LP,LIF,IB,IT-1).EQ.FBLANK) .OR.
     *                     (PDATA(LP,LIF,IA,IT).EQ.FBLANK) .OR.
     *                     (PDATA(LP,LIF,IB,IT).EQ.FBLANK)
                        JMP(LP,LIF) = .FALSE.
                        IF (.NOT.BAD(LP,LIF)) THEN
                           LBAD(LP) = .FALSE.
                           PJ = PDATA(LP,LIF,IB,IT) -
     *                        PDATA(LP,LIF,IA,IT) -
     *                        PDATA(LP,LIF,IB,IT-1) +
     *                        PDATA(LP,LIF,IA,IT-1)
                           IF (PJ.GT.180.0) PJ = PJ - 360.0
                           IF (PJ.LE.-180.0) PJ = PJ + 360.0
                           IF (PJ.GT.180.0) PJ = PJ - 360.0
                           IF (PJ.LE.-180.0) PJ = PJ + 360.0
                           IF (PJ.GT.180.0) PJ = PJ - 360.0
                           IF (PJ.LE.-180.0) PJ = PJ + 360.0
                           JMP(LP,LIF) = (ABS(PJ).GT.PJUMPX)
                           IF (.NOT.JMP(LP,LIF)) LJMP(LP) = .FALSE.
                           END IF
 205                    CONTINUE
 210                 CONTINUE
C                                       Loop over pol and IF
                  IF (NP.EQ.2) CALL LFILL (NI, .TRUE., NEED(1,2))
                  DO 220 LP = 1,NP
                     DO 215 LIF = 1,NI
                        IF ((LBAD(LP)) .OR. (LJMP(LP))) THEN
                           IFS(1) = 0
                           IFS(2) = 0
                        ELSE IF (DOIFS.LE.0.0) THEN
                           IFS(1) = LIF + BIF - 1
                           IFS(2) = IFS(1)
                           END IF
                        QIF = IFS(1).EQ.0
                        QALL = QIF .AND. QSTOK
C                                       check for jump
                        IF ((JMP(LP,LIF)) .AND. (NEED(LIF,LP))) THEN
                           PJ = PDATA(LP,LIF,IB,IT) -
     *                        PDATA(LP,LIF,IA,IT) -
     *                        PDATA(LP,LIF,IB,IT-1) +
     *                        PDATA(LP,LIF,IA,IT-1)
                           IF (PJ.GT.180.0) PJ = PJ - 360.0
                           IF (PJ.LE.-180.0) PJ = PJ + 360.0
                           IF (PJ.GT.180.0) PJ = PJ - 360.0
                           IF (PJ.LE.-180.0) PJ = PJ + 360.0
                           IF (PJ.GT.180.0) PJ = PJ - 360.0
                           IF (PJ.LE.-180.0) PJ = PJ + 360.0
                           TIMER(1) = TDATA(IT-1)
                           TIMER(2) = TDATA(IT)
                           WRITE (REASON,1210) TSKNAM, PJ
                           CALL TABFLG ('WRIT', BUFF1, OFGRNO, OKOLS,
     *                        ONUMV, SOURID, SUBA, FREQID, ANTNO, TIMER,
     *                        IFS, CHANS, PFLAGS(1,LP), REASON, IERR)
                           IF (IERR.NE.0) GO TO 980
                           NFLAG = NFLAG + 1
                           NFLAGS = NFLAGS + 1
                           IF (QALL) GO TO 230
                           NEED(LIF,2) = .NOT.QSTOK
                           IF (QIF) GO TO 220
                           END IF
 215                    CONTINUE
 220                 CONTINUE
 230              CONTINUE
C                                       summary
               IF (NFLAG.GT.0) THEN
                  WRITE (MSGTXT,1230) IA, IB, NFLAG, NPAIR
                  CALL MSGWRT (3)
                  END IF
               END IF
 240        CONTINUE
 250     CONTINUE
C
 980  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1980) IERR
         CALL MSGWRT (8)
         END IF
      CALL TABIO ('CLOS', 0, OFGRNO, BUFF1, BUFF1, I)
      IF ((SUBARR.EQ.SUBA2) .OR. (IERR.NE.0)) THEN
         CALL TABIO ('CLOS', 0, ICLRNO, GNREC, CLBUFF, IERR)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Copied',I9,' flags from vers',I4,' to',I4)
 1010 FORMAT ('SNJFLG: ERROR =',I3,' FROM TABIO')
 1080 FORMAT ('TIME OR ANTENNA # TOO BIG',I7,I3,' >',I7,I3)
 1100 FORMAT ('Total number of time indexes found:',I6)
 1101 FORMAT ('Highest antenna number found:',I3)
 1125 FORMAT ('Antenna',I3,' flagged',I6,' time ranges')
 1210 FORMAT (A,'PJUMP=',F8.1)
 1230 FORMAT ('Baseline',I3,' x',I3,' flagged',I6,' of',I6,' pairs')
 1980 FORMAT ('ERROR',I5,' WRITING OUTPUT FLAG TABLE')
      END
      SUBROUTINE SNAFLG (NP, NI, SDATA, ADATA, IERR)
C-----------------------------------------------------------------------
C   SNAFLG does the AMP opcode. SNJFLG reads the SN table to get the
C   amplitude information for flagging.
C   Input/Output in common:
C      TSTART   R   Start time
C      TSTOP    R   Stop time
C   Input:
C      NP       I   Number polarizations in PDATA
C      NI       I   Number IFs in PDATA
C   Output:
C      PDATA    R(*)
C      IERR     I   Error code, 0=OK else failed
C-----------------------------------------------------------------------
      INTEGER   NP, NI, IERR
      REAL      ADATA(NP,NI,*), SDATA(3,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, FGMAX, BUFF1(512), BUFF2(512), OFGRNO, IFGRNO,
     *   FGKOLS(MAXFGC), FGNUMV(MAXFGC), SOURID, SUBA, FREQID,
     *   ANTNO(2), IFS(2), CHANS(2), NFGROW, IROUND, LUN1, LUN2,
     *   OKOLS(MAXFGC), ONUMV(MAXFGC), J, ISUBA, LIF, LP, NR, NA,
     *   NS, LS, LA, LR, NSUM(2,MAXIF), NC, COUNT
      REAL      TIMER(2), TB, TE, GTIME, DELTAT
      DOUBLE PRECISION V, RMS(2,MAXIF), SUMA(2,MAXIF), SUMS(2,MAXIF),
     *   AVG(2,MAXIF), RM(10)
      LOGICAL   PFLAGS(4,2), QSTOK
      CHARACTER REASON*24
      INCLUDE 'SNFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUN1, LUN2 /46,47/
      DATA RM /6.D0,5.D0,4.D0,3.D0,3.D0,3.D0,3.D0,3.D0,3.D0,3.D0/
C-----------------------------------------------------------------------
      NFLAGS = 0
C                                       prepare output FG table
      CALL FNDEXT ('FG', CATBLK, FGMAX)
      FGIN = IROUND (XFGVER)
      IF ((FGIN.EQ.0) .OR. (FGIN.GT.FGMAX)) FGIN = FGMAX
      FGIN = MAX (0, FGIN)
      XFGVER = -1
      IF (FGOUT.LE.0) FGOUT = FGMAX + 1
C                                       Create new FG table
      CALL FLGINI ('WRIT', BUFF1, DISKIN, CNOIN, FGOUT, CATBLK, LUN1,
     *   OFGRNO, OKOLS, ONUMV, IERR)
      IF (IERR.NE.0) GO TO 999
      NFGROW = 0
C                                       Copy old FG table
      IF (FGIN.GT.0) THEN
         CALL FLGINI ('READ', BUFF2, DISKIN, CNOIN, FGIN, CATBLK, LUN2,
     *      IFGRNO, FGKOLS, FGNUMV, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'OLD FG TABLE NOT COPIED'
            CALL MSGWRT (7)
            GO TO 15
            END IF
         NFGROW = BUFF2(5)
C                                       Loop and copy
         DO 10 I = 1,NFGROW
            CALL TABFLG ('READ', BUFF2, IFGRNO, FGKOLS, FGNUMV, SOURID,
     *         SUBA, FREQID, ANTNO, TIMER, IFS, CHANS, PFLAGS, REASON,
     *         IERR)
            IF (IERR.GT.0) GO TO 999
            IF (IERR.EQ.0) THEN
               CALL TABFLG ('WRIT', BUFF1, OFGRNO, OKOLS, ONUMV, SOURID,
     *            SUBA, FREQID, ANTNO, TIMER, IFS, CHANS, PFLAGS,
     *            REASON, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
 10         CONTINUE
         CALL TABIO ('CLOS', 0, IFGRNO, BUFF2, BUFF2, IERR)
         WRITE (MSGTXT,1000) NFGROW, FGIN, FGOUT
         CALL MSGWRT (3)
         END IF
C                                       Loop thru data
 15   NR = 0
      NA = 0
      NS = 0
      DO 100 J = 1,NCLINR
         ICLRNO = J
         CALL TABIO ('READ', 0, ICLRNO, GNREC, CLBUFF, IERR)
         IF (IERR.LT.0) GO TO 100
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
C                                       Record within specified
C                                       time range ?
         IF (KOLTYP(TIMECL).EQ.1) THEN
            GTIME = GNRECD(TIMKOL)
         ELSE
            GTIME = GNREC(TIMKOL)
            END IF
         IF ((GTIME.LT.TSTART) .OR. (GTIME.GT.TSTOP)) GO TO 100
C                                       Freq id
         IF (GNRECI(FRQKOL).NE.-1) THEN
            IF ((GNRECI(FRQKOL).NE.FRQSEL) .AND. (FRQSEL.GT.0))
     *         GO TO 100
            END IF
C                                       Antenna?
         IANT = GNRECI(ANTKOL)
         IF (NANTSL.GT.0) THEN
            DO 50 I = 1,NANTSL
               IF ((IANT.EQ.ANTS(I)).AND.DOAWNT) GO TO 60
               IF ((IANT.EQ.ANTS(I)).AND.(.NOT.DOAWNT)) GO TO 100
 50            CONTINUE
            IF (DOAWNT) GO TO 100
            END IF
C                                       Check source
 60      ISOU = GNRECI(SOUKOL)
         IF (NID.GT.0) THEN
            DO 70 I = 1,NID
               IF (ISOU.EQ.SID(I)) GO TO 80
 70            CONTINUE
            GO TO 100
            END IF
         IF (NS.EQ.1) ISOU = 1
C                                       Subarray check
 80      ISUBA = GNRECI(SUBKOL)
         IF ((ISUBA.GT.0) .AND. (ISUBA.NE.SUBARR)) GO TO 100
         TB = MIN (TB, GTIME)
         TE = MAX (TE, GTIME)
C                                       Get value - checking weight
         NR = NR + 1
         CALL SNADAT (ADATA(1,1,NR), SDATA(1,NR))
         SDATA(2,NR) = ISOU
         SDATA(3,NR) = IANT
         NS = MAX (NS, ISOU)
         NA = MAX (NA, IANT)
 100     CONTINUE
C                                       prepare for flagging
      SOURID = 0
      SUBA = SUBARR
      FREQID = FRQSEL
      CHANS(1) = 1
      CHANS(2) = 0
      IFS(1) = 0
      IFS(2) = 0
      ANTNO(1) = 0
      ANTNO(2) = 0
      CALL LFILL (8, .TRUE., PFLAGS)
      IF (DPARM(1).LE.0.0) DPARM(1) = 6.0
      IF (DPARM(2).LE.0.0) DPARM(2) = 5.0
      DELTAT = DPARM(2) / (3600.0*24.0)
C                                       flag at start only if no timer
      IF (TSTART.EQ.0.0) THEN
         TIMER(1) = 0.0
         TIMER(2) = TB - DELTAT * 0.99
         REASON = TSKNAM // 'start of data'
         CALL TABFLG ('WRIT', BUFF1, OFGRNO, OKOLS, ONUMV, SOURID, SUBA,
     *      FREQID, ANTNO, TIMER, IFS, CHANS, PFLAGS, REASON, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       flag at end if no time range
      IF (TSTOP.EQ.999.0) THEN
         TIMER(1) = TE + DELTAT * 0.99
         TIMER(2) = 999.0
         REASON = TSKNAM // 'end of data'
         CALL TABFLG ('WRIT', BUFF1, OFGRNO, OKOLS, ONUMV, SOURID, SUBA,
     *      FREQID, ANTNO, TIMER, IFS, CHANS, PFLAGS, REASON, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       polarization flags
      PFLAGS(1,1) = (SUMSTK.NE.2) .OR. (DOSTOK.GT.0.0)
      PFLAGS(2,1) = (SUMSTK.GT.1) .OR. (DOSTOK.GT.0.0)
      QSTOK = PFLAGS(1,1) .AND. PFLAGS(2,1)
      IF (SUMSTK.EQ.0) THEN
         PFLAGS(1,2) = DOSTOK.GT.0.0
         QSTOK = QSTOK .AND. PFLAGS(1,2)
         END IF
C                                       Loop thru data
      DELTAT = DELTAT / 2.0
C                                       outer loop
      NC = NP * NI
      DO 200 LS = 1,NS
         DO 195 LA = 1,NA
            CALL DFILL (NC, 1.D3, RMS)
            CALL DFILL (NC, 0.D0, AVG)
            J = 0
C                                       robust RMS loop
 110        CALL DFILL (NC, 0.0D0, SUMA)
            CALL DFILL (NC, 0.0D0, SUMS)
            CALL FILL (NC, 0, NSUM)
            J = J + 1
            COUNT = 0
            DO 125 LR = 1,NR
               IANT = IROUND (SDATA(3,LR))
               ISOU = IROUND (SDATA(2,LR))
               IF ((IANT.EQ.LA) .AND. (ISOU.EQ.LS)) THEN
                  DO 120 LIF = 1,NI
                     DO 115 LP = 1,NP
                        V = ADATA(LP,LIF,LR)
                        IF ((ADATA(LP,LIF,LR).NE.FBLANK) .AND.
     *                     (ABS(V-AVG(LP,LIF)).LT.RM(J)*RMS(LP,LIF)))
     *                     THEN
                           SUMA(LP,LIF) = SUMA(LP,LIF) + V
                           SUMS(LP,LIF) = SUMS(LP,LIF) + V * V
                           NSUM(LP,LIF) = NSUM(LP,LIF) + 1
                           COUNT = COUNT + 1
                           END IF
 115                    CONTINUE
 120                 CONTINUE
                  END IF
 125           CONTINUE
C                                       found something
            IF (COUNT.GT.0) THEN
               DO 135 LIF = 1,NI
                  DO 130 LP = 1,NP
                     IF (NSUM(LP,LIF).GT.0) THEN
                        AVG(LP,LIF) = SUMA(LP,LIF) / NSUM(LP,LIF)
                        RMS(LP,LIF) = SUMS(LP,LIF) / NSUM(LP,LIF) -
     *                     AVG(LP,LIF)**2
                        RMS(LP,LIF) = SQRT (MAX (0.0D0, RMS(LP,LIF)))
                     ELSE IF (J.EQ.7) THEN
                        RMS(LP,LIF) = 0.0
                        END IF
 130                 CONTINUE
 135              CONTINUE
               IF (J.LT.7) GO TO 110
C                                       robust rms found
C                                       do flagging
               DO 150 LR = 1,NR
                  IANT = IROUND (SDATA(3,LR))
                  ISOU = IROUND (SDATA(2,LR))
                  IF ((IANT.EQ.LA) .AND. (ISOU.EQ.LS)) THEN
                     DO 145 LP = 1,NP
                        DO 140 LIF = 1,NI
                           IF ((ADATA(LP,LIF,LR).NE.FBLANK)
     *                        .AND. (RMS(LP,LIF).GT.0.0)) THEN
                              V = ABS (ADATA(LP,LIF,LR) - AVG(LP,LIF)) /
     *                           RMS(LP,LIF)
                           ELSE
                              V = FBLANK
                              END IF
                           IF ((V.EQ.FBLANK) .OR. (V.GT.DPARM(1))) THEN
                              ANTNO(1) = IANT
                              TIMER(1) = SDATA(1,LR) - DELTAT
                              TIMER(2) = SDATA(1,LR) + DELTAT
                              IF (DOIFS.LE.0.0) THEN
                                 IFS(1) = LIF
                                 IFS(2) = LIF
                                 END IF
                              IF (V.EQ.FBLANK) THEN
                                 WRITE (REASON,1162) TSKNAM
                              ELSE IF (V.LE.9999.) THEN
                                 WRITE (REASON,1160) TSKNAM, V
                              ELSE
                                 WRITE (REASON,1161) TSKNAM, V
                                 END IF
                              CALL TABFLG ('WRIT', BUFF1, OFGRNO, OKOLS,
     *                           ONUMV, SOURID, SUBA, FREQID, ANTNO,
     *                           TIMER, IFS, CHANS, PFLAGS(1,LP),
     *                           REASON, IERR)
                              IF (IERR.NE.0) GO TO 980
                              NFLAGS = NFLAGS + 1
                              IF ((DOSTOK.GT.0.0) .AND.
     *                           (DOIFS.GT.0.0))GO TO 150
                              IF (DOIFS.GT.0.0) GO TO 145
                              END IF
 140                       CONTINUE
 145                    CONTINUE
                     END IF
 150              CONTINUE
               END IF
 195        CONTINUE
 200     CONTINUE
C                                       done
 980  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1980) IERR
         CALL MSGWRT (8)
         END IF
      CALL TABIO ('CLOS', 0, OFGRNO, BUFF1, BUFF1, I)
      IF ((SUBARR.EQ.SUBA2) .OR. (IERR.NE.0)) THEN
         CALL TABIO ('CLOS', 0, ICLRNO, GNREC, CLBUFF, I)
         END IF
      IF (FGIN.GT.0) THEN
         WRITE (MSGTXT,1200) 'Added', NFLAGS, FGOUT, SUBARR
      ELSE
         WRITE (MSGTXT,1200) 'Wrote', NFLAGS, FGOUT, SUBARR
         END IF
      CALL MSGWRT (4)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Copied',I9,' flags from vers',I4,' to',I4)
 1010 FORMAT ('SNAFLG: ERROR =',I3,' FROM TABIO')
 1160 FORMAT (A,'DGAIN=',F6.1,' * RMS')
 1161 FORMAT (A,'DGAIN=',F8.1,'*RMS')
 1162 FORMAT (A,'DGAIN BLANKED')
 1200 FORMAT (A,I8,' flag records to FG version',I4,' subarray',I3)
 1980 FORMAT ('ERROR',I5,' WRITING OUTPUT FLAG TABLE')
      END
      SUBROUTINE SNPOPN (IERR)
C-----------------------------------------------------------------------
C   Routine to open SN, CL, PC or TY table and get necessary information
C   Input from Common:
C      TYPE     C*2  'SN', 'CL'
C      DISKIN   I     Disk number
C      CNOIN    I     Catalog slot number
C      CATBLK   I(*)  Catalog header
C      SUMSTK   I     Stokes type requested 0=both, 1=R, 2=L,
C                     3=difference
C   Output:
C      IERR     I     Error code, 0=OK else failed.
C   Output in common:
C      ICLRNO       I    Current cal record number
C      NCLINR       I    Number of gain records in file.
C      NUMANT       I    Number of antennas
C      NUMPOL       I    Number of polarizations
C      NUMIF        I    Number of IFs.
C      KOLS         I(*) Column pointers
C      KOLTYP       I(*) Column data types
C      KOLDIM       I(*) Column dimension
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER KEYW(4)*8, COLHD1(11)*24, COLHD2(13)*24, COLHD3(13)*24,
     *   COLTAB(40)*24, COLHED(37)*24, KEYSN(4)*8
      INTEGER   NKEY, NREC, NCOL, DATP(128,2), IPOINT, KEYTYP(4),
     *   KLOCS(4), KEYVAL(5), I, KP, MSGSAV
      LOGICAL   T
      REAL      KEYVR(5)
      INCLUDE 'SNFLG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANS.INC'
      EQUIVALENCE (KEYVAL, KEYVR)
      EQUIVALENCE (COLHED(1), COLHD1), (COLHED(12), COLHD2),
     *   (COLHED(25), COLHD3)
      DATA COLHD1 /'TIME                    ',
     *   'TIME INTERVAL           ',
     *   'SOURCE ID               ', 'ANTENNA NO.             ',
     *   'SUBARRAY                ', 'FREQ ID                 ',
     *   'I.FAR.ROT               ',
     *   'GEODELAY                ', 'DOPPOFF                 ',
     *   'ATMOS                   ', 'DATMOS                  '/
      DATA COLHD2 /'MBDELAY1      ',
     *   'REAL1                   ', 'IMAG1                   ',
     *   'RATE 1                  ', 'DELAY 1                 ',
     *   'WEIGHT 1                ', 'REFANT 1                ',
     *   'TSYS 1                  ', 'TANT 1                  ',
     *   'CLOCK 1                 ', 'DCLOCK 1                ',
     *   'DISP 1                  ', 'DDISP 1                 '/
      DATA COLHD3 /'MBDELAY2      ',
     *   'REAL2                   ', 'IMAG2                   ',
     *   'RATE 2                  ', 'DELAY 2                 ',
     *   'WEIGHT 2                ', 'REFANT 2                ',
     *   'TSYS 2                  ', 'TANT 2                  ',
     *   'CLOCK 2                 ', 'DCLOCK 2                ',
     *   'DISP 2                  ', 'DDISP 2                 '/
      DATA KEYSN /'NO_ANT  ', 'NO_POL  ', 'NO_IF   ','MGMOD   '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Open table
      ICLUN = 28
      NKEY = 0
      NREC = 0
      NCOL = 0
      ICLRNO = 1
      CALL TABINI ('READ', TYPE, DISKIN, CNOIN, IVER, CATBLK, ICLUN,
     *   NKEY, NREC, NCOL, DATP, CLBUFF, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1100) IERR, TYPE, IVER
         GO TO 980
         END IF
C                                       Get number of scans
      NCLINR = CLBUFF(5)
C                                       Check if empty
      IF (NCLINR.LE.0) THEN
         IERR = 6
         MSGTXT = 'ERROR: SELECTED TABLE IS EMPTY'
         GO TO 980
         END IF
C                                       Get column pointers
      NKEY = MAXCLC
      DO 10 I = 1,NKEY
         COLTAB(I) = COLHED(I)
   10    CONTINUE
      CALL FNDCOL (NKEY, COLTAB, 24, T, CLBUFF, KOLS, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.10)) GO TO 999
      IERR = 0
C                                       Time column logical number
      TIMECL = KOLS(1)
C                                       Convert to pointers, types
      DO 100 I = 1,NKEY
         KP = KOLS(I)
         IF (KP.GT.0) THEN
            KOLS(I) = DATP(KP,1)
            KOLTYP(I) = MOD (DATP(KP,2), 10)
            KOLDIM(I) = DATP(KP,2) / 10
         ELSE
            KOLS(I) = -1
            KOLTYP(I) = -1
            KOLDIM(I) = 0
            END IF
 100     CONTINUE
C                                       Table keywords
      NKEY = 4
      DO 20 I = 1,NKEY
         KEYW(I) = KEYSN(I)
   20    CONTINUE
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TABKEY ('READ', KEYW, NKEY, CLBUFF, KLOCS, KEYVAL, KEYTYP,
     *   IERR)
      MSGSUP = MSGSAV
      IF ((IERR.GE.1) .AND. (IERR.LE.20)) GO TO 999
      IERR = 0
C                                       No. antennas.
      NUMANT = NSTNS
      IPOINT = KLOCS(1)
      IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. poln.
      NUMPOL = 1
      IPOINT = KLOCS(2)
      IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF
      NUMIF = 1
      IPOINT = KLOCS(3)
      IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       Set pointers
C                                       1st poln
      IF (BPOL.EQ.1) THEN
         REKOL1 = RE1KOL - 1
         IMKOL1 = IM1KOL - 1
         WTKOL1 = WT1KOL - 1
C                                       2nd poln
      ELSE
         REKOL1 = RE2KOL - 1
         IMKOL1 = IM2KOL - 1
         WTKOL1 = WT2KOL - 1
         END IF
C                                       2nd Poln
      REKOL2 = RE2KOL - 1
      IMKOL2 = IM2KOL - 1
      WTKOL2 = WT2KOL - 1
C                                       Requested data not in table
      IF ((REKOL1.LT.0) .OR. (IMKOL1.LT.0) .OR. ((EPOL.NE.BPOL) .AND.
     *   ((REKOL2.LT.0) .OR. (IMKOL2.LT.0)))) THEN
         WRITE(MSGTXT,1500) TYPE
         IERR = 10
         END IF
C                                       Error
 980  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ERROR ',I3,' OPENING ',A,' TABLE NO. ',I3)
 1500 FORMAT (' REQUESTED DATA NOT IN ',A,' TABLE ')
      END
      SUBROUTINE SNPDAT (VALUE, XVARIB)
C-----------------------------------------------------------------------
C   Routine to return phase and time from gain record
C   Input from common:
C      GNREC    R(*)  Table record
C   Also uses pointers etc. set in SNPOPN
C   Output:
C      VALUE    R     Phase value, magic value blanked
C      XVARIB   R     Time
C-----------------------------------------------------------------------
      REAL     VALUE(2,*), XVARIB
C
      INTEGER   LIF
      REAL      V
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Time
      IF (KOLTYP(TIMECL).EQ.1) THEN
         XVARIB = GNRECD(TIMKOL)
      ELSE
         XVARIB = GNREC(TIMKOL)
         END IF
C                                       In case the data are bad
      DO 10 LIF = BIF,EIF
         VALUE(1,LIF-BIF+1) = FBLANK
         VALUE(2,LIF-BIF+1) = FBLANK
C                                       Phase (deg)
         IF ((GNREC(REKOL1+LIF).NE.FBLANK) .AND.
     *      (GNREC(IMKOL1+LIF).NE.FBLANK) .AND.
     *      (GNREC(WTKOL1+LIF).GE.CUTOFF)) THEN
            V = FBLANK
            IF (SUMSTK.EQ.3) THEN
               IF ((GNREC(REKOL2+LIF).NE.FBLANK) .AND.
     *            (GNREC(IMKOL2+LIF).NE.FBLANK) .AND.
     *            (GNREC(WTKOL2+LIF).GE.CUTOFF)) V = 57.296 *
     *            (ATAN2 (GNREC(IMKOL1+LIF), GNREC(REKOL1+LIF)+1.E-20) -
     *            ATAN2 (GNREC(IMKOL2+LIF), GNREC(REKOL2+LIF)+1.E-20))
            ELSE
               V = 57.296 *
     *            ATAN2 (GNREC(IMKOL1+LIF), GNREC(REKOL1+LIF) + 1.0E-20)
               END IF
            IF (V.NE.FBLANK) THEN
               IF (V.LT.-180.) V = V + 360.0
               IF (V.GT.180.) V = V - 360.0
               VALUE(1,LIF-BIF+1) = V
               END IF
            END IF
         IF ((SUMSTK.EQ.0) .AND.(GNREC(REKOL2+LIF).NE.FBLANK) .AND.
     *      (GNREC(IMKOL2+LIF).NE.FBLANK) .AND.
     *      (GNREC(WTKOL2+LIF).GE.CUTOFF)) THEN
            V = 57.296 *
     *         ATAN2 (GNREC(IMKOL2+LIF), GNREC(REKOL2+LIF) + 1.0E-20)
            IF (V.LT.-180.) V = V + 360.0
            IF (V.GT.180.) V = V - 360.0
            VALUE(2,LIF-BIF+1) = V
            END IF
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SNADAT (VALUE, XVARIB)
C-----------------------------------------------------------------------
C   Routine to return amplitude and time from gain record
C   Input from common:
C      GNREC    R(*)  Table record
C   Also uses pointers etc. set in SNPOPN
C   Output:
C      VALUE    R     Phase value, magic value blanked
C      XVARIB   R     Time
C-----------------------------------------------------------------------
      REAL     VALUE(2,*), XVARIB
C
      INTEGER   LIF
      REAL      V
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Time
      IF (KOLTYP(TIMECL).EQ.1) THEN
         XVARIB = GNRECD(TIMKOL)
      ELSE
         XVARIB = GNREC(TIMKOL)
         END IF
C                                       In case the data are bad
      DO 10 LIF = BIF,EIF
         VALUE(1,LIF-BIF+1) = FBLANK
         VALUE(2,LIF-BIF+1) = FBLANK
C                                       amplitude
         IF ((GNREC(REKOL1+LIF).NE.FBLANK) .AND.
     *      (GNREC(IMKOL1+LIF).NE.FBLANK) .AND.
     *      (GNREC(WTKOL1+LIF).GE.CUTOFF)) THEN
            V = FBLANK
            IF (SUMSTK.EQ.3) THEN
               IF ((GNREC(REKOL2+LIF).NE.FBLANK) .AND.
     *            (GNREC(IMKOL2+LIF).NE.FBLANK) .AND.
     *            (GNREC(WTKOL2+LIF).GE.CUTOFF)) THEN
                  V = SQRT (GNREC(IMKOL2+LIF)**2 + GNREC(REKOL2+LIF)**2)
                  IF (V.NE.0.0) THEN
                     V = SQRT (GNREC(IMKOL1+LIF)**2 +
     *                  GNREC(REKOL1+LIF)**2) / V
                  ELSE
                     V = FBLANK
                     END IF
                  END IF
            ELSE
               V = SQRT (GNREC(IMKOL1+LIF)**2 + GNREC(REKOL1+LIF)**2)
               END IF
            IF (V.NE.FBLANK) VALUE(1,LIF-BIF+1) = V
            END IF
         IF ((SUMSTK.EQ.0) .AND.(GNREC(REKOL2+LIF).NE.FBLANK) .AND.
     *      (GNREC(IMKOL2+LIF).NE.FBLANK) .AND.
     *      (GNREC(WTKOL2+LIF).GE.CUTOFF)) THEN
            V = SQRT (GNREC(IMKOL2+LIF)**2 + GNREC(REKOL2+LIF)**2)
            VALUE(2,LIF-BIF+1) = V
            END IF
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SNWDAT (VALUE, XVARIB)
C-----------------------------------------------------------------------
C   Routine to return weights from gain record
C   Input from common:
C      GNREC    R(*)  Table record
C   Also uses pointers etc. set in SNPOPN
C   Output:
C      VALUE    R     Weight value, magic value blanked
C      XVARIB   R     Time
C-----------------------------------------------------------------------
      REAL     VALUE(2,*), XVARIB
C
      INTEGER   LIF
      REAL      V
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Time
      IF (KOLTYP(TIMECL).EQ.1) THEN
         XVARIB = GNRECD(TIMKOL)
      ELSE
         XVARIB = GNREC(TIMKOL)
         END IF
C                                       In case the data are bad
      DO 10 LIF = BIF,EIF
         VALUE(1,LIF-BIF+1) = FBLANK
         VALUE(2,LIF-BIF+1) = FBLANK
C                                       amplitude
         IF ((GNREC(REKOL1+LIF).NE.FBLANK) .AND.
     *      (GNREC(IMKOL1+LIF).NE.FBLANK)) THEN
            V = FBLANK
            IF (SUMSTK.EQ.3) THEN
               IF ((GNREC(REKOL2+LIF).NE.FBLANK) .AND.
     *            (GNREC(IMKOL2+LIF).NE.FBLANK)) THEN
                  V = GNREC(WTKOL2+LIF)
                  V = MIN (V, GNREC(WTKOL1+LIF))
                  END IF
            ELSE
               V = GNREC(WTKOL1+LIF)
               END IF
            IF (V.NE.FBLANK) VALUE(1,LIF-BIF+1) = V
            END IF
         IF ((SUMSTK.EQ.0) .AND.(GNREC(REKOL2+LIF).NE.FBLANK) .AND.
     *      (GNREC(IMKOL2+LIF).NE.FBLANK)) THEN
            V = GNREC(WTKOL2+LIF)
            VALUE(2,LIF-BIF+1) = V
            END IF
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SNDFLG (NP, IERR)
C-----------------------------------------------------------------------
C   Writes flags based on discrepancy from stated limits
C   Input:
C      NP       I   Number polarizations in PDATA
C   Output:
C      PDATA    R(*)
C      IERR     I   Error code, 0=OK else failed
C-----------------------------------------------------------------------
      INTEGER   NP, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, FGMAX, BUFF1(512), BUFF2(512), OFGRNO, IFGRNO,
     *   FGKOLS(MAXFGC), FGNUMV(MAXFGC), SOURID, SUBA, FREQID, ANTNO(2),
     *   IFS(2), CHANS(2), NFGROW, IROUND, LUN1, LUN2, JIF, J, ISUBA,
     *   OKOLS(MAXFGC), ONUMV(MAXFGC), LIF, LP, TIME(3), DATE(3), IC
      REAL      TIMER(2), GTIME, DELTAT, TEPS, PHASE(2,MAXIF),
     *   AMPL(2,MAXIF), WGT(2,MAXIF), CUTS
      LOGICAL   PFLAGS(4,2), QSTOK
      CHARACTER REASON*24, CTIME*8, CDATE*12
      INCLUDE 'SNFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUN1, LUN2 /46,47/
C-----------------------------------------------------------------------
      TEPS = 0.01 / (24.0 * 60.0 * 60.0)
      CUTS = CUTOFF
      CUTOFF = -1.E6
C                                       defaults
      DPARM(1) = MAX (0.0, DPARM(1))
      IF (DPARM(2).EQ.0.0) DPARM(2) = 9999.
      IF (DPARM(3).EQ.0.0) DPARM(3) = -200.
      IF (DPARM(4).EQ.0.0) DPARM(4) = +200.
      IF (DPARM(5).EQ.0.0) DPARM(5) = CUTS
      IF (DPARM(6).LE.0.0) DPARM(6) = 999999.
      IF ((DPARM(2).LE.DPARM(1)) .OR. (DPARM(4).LE.DPARM(3)) .OR.
     *   (DPARM(6).LE.DPARM(5))) THEN
         IERR = 10
         MSGTXT = 'SNDFLG: DPARM VALUES INCOMPATIBLE'
         GO TO 990
         END IF
      NFLAGS = 0
C                                       prepare output FG table
      CALL FNDEXT ('FG', CATBLK, FGMAX)
      FGIN = IROUND (XFGVER)
      IF ((FGIN.EQ.0) .OR. (FGIN.GT.FGMAX)) FGIN = FGMAX
      FGIN = MAX (0, FGIN)
      XFGVER = -1
      IF (FGOUT.LE.0) FGOUT = FGMAX + 1
C                                       Create new FG table
      CALL FLGINI ('WRIT', BUFF1, DISKIN, CNOIN, FGOUT, CATBLK, LUN1,
     *   OFGRNO, OKOLS, ONUMV, IERR)
      IF (IERR.NE.0) GO TO 999
      NFGROW = 0
C                                       Copy old FG table
      IF (FGIN.GT.0) THEN
         CALL FLGINI ('READ', BUFF2, DISKIN, CNOIN, FGIN, CATBLK, LUN2,
     *      IFGRNO, FGKOLS, FGNUMV, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'OLD FG TABLE NOT COPIED'
            CALL MSGWRT (7)
            GO TO 15
            END IF
         NFGROW = BUFF2(5)
C                                       Loop and copy
         DO 10 I = 1,NFGROW
            CALL TABFLG ('READ', BUFF2, IFGRNO, FGKOLS, FGNUMV, SOURID,
     *         SUBA, FREQID, ANTNO, TIMER, IFS, CHANS, PFLAGS, REASON,
     *         IERR)
            IF (IERR.GT.0) GO TO 999
            IF (IERR.EQ.0) THEN
               CALL TABFLG ('WRIT', BUFF1, OFGRNO, OKOLS, ONUMV, SOURID,
     *            SUBA, FREQID, ANTNO, TIMER, IFS, CHANS, PFLAGS,
     *            REASON, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
 10         CONTINUE
         CALL TABIO ('CLOS', 0, IFGRNO, BUFF2, BUFF2, IERR)
         WRITE (MSGTXT,1000) NFGROW, FGIN, FGOUT
         CALL MSGWRT (3)
         END IF
C                                       prepare for flagging
 15   SOURID = 0
      SUBA = SUBARR
      FREQID = FRQSEL
      CHANS(1) = 1
      CHANS(2) = 0
      IFS(1) = 0
      IFS(2) = 0
      ANTNO(1) = 0
      ANTNO(2) = 0
      CALL ZTIME (TIME)
      CALL ZDATE (DATE)
      DATE(1) = -DATE(1)
      CALL TIMDAT (TIME, DATE, CTIME, CDATE)
      WRITE (REASON,1015) CDATE(:9), CTIME(:5)
C                                       polarization flags
      CALL LFILL (8, .TRUE., PFLAGS)
      PFLAGS(1,1) = (SUMSTK.NE.2) .OR. (DOSTOK.GT.0.0)
      PFLAGS(2,1) = (SUMSTK.GT.1) .OR. (DOSTOK.GT.0.0)
      QSTOK = PFLAGS(1,1) .AND. PFLAGS(2,1)
      IF (SUMSTK.EQ.0) THEN
         PFLAGS(1,2) = DOSTOK.GT.0.0
         QSTOK = QSTOK .AND. PFLAGS(1,2)
         END IF
C                                       Loop thru data
      DO 100 J = 1,NCLINR
         ICLRNO = J
         CALL TABIO ('READ', 0, ICLRNO, GNREC, CLBUFF, IERR)
         IF (IERR.LT.0) GO TO 100
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1020) IERR
            GO TO 990
            END IF
C                                       Record within specified
C                                       time range ?
         IF (KOLTYP(TIMECL).EQ.1) THEN
            GTIME = GNRECD(TIMKOL)
         ELSE
            GTIME = GNREC(TIMKOL)
            END IF
         IF ((GTIME.LT.TSTART) .OR. (GTIME.GT.TSTOP)) GO TO 100
C                                       Freq id
         IF (GNRECI(FRQKOL).NE.-1) THEN
            IF ((GNRECI(FRQKOL).NE.FRQSEL) .AND. (FRQSEL.GT.0))
     *         GO TO 100
            END IF
C                                       Antenna?
         IANT = GNRECI(ANTKOL)
         IF (NANTSL.GT.0) THEN
            DO 20 I = 1,NANTSL
               IF ((IANT.EQ.ANTS(I)).AND.DOAWNT) GO TO 25
               IF ((IANT.EQ.ANTS(I)).AND.(.NOT.DOAWNT)) GO TO 100
 20            CONTINUE
            IF (DOAWNT) GO TO 100
            END IF
C                                       Check source
 25      ISOU = GNRECI(SOUKOL)
         IF (NID.GT.0) THEN
            DO 30 I = 1,NID
               IF (ISOU.EQ.SID(I)) GO TO 35
 30            CONTINUE
            GO TO 100
            END IF
C                                       Subarray check
 35      ISUBA = GNRECI(SUBKOL)
         IF ((ISUBA.GT.0) .AND. (ISUBA.NE.SUBARR)) GO TO 100
C                                       get the data
         CALL SNPDAT (PHASE, DELTAT)
         CALL SNADAT (AMPL, DELTAT)
         CALL SNWDAT (WGT, DELTAT)
         DELTAT = GNREC(DTKOL)/2.0 + TEPS
         DO 90 LP = 1,NP
            DO 80 LIF = BIF,EIF
               JIF = LIF - BIF + 1
               IF ((AMPL(LP,JIF).NE.FBLANK) .AND.
     *            (PHASE(LP,JIF).NE.FBLANK)) THEN
C                                       bad point: make flag
                  IF ((AMPL(LP,JIF).LT.DPARM(1)) .OR.
     *               (AMPL(LP,JIF).GT.DPARM(2)) .OR.
     *               (PHASE(LP,JIF).LT.DPARM(3)) .OR.
     *               (PHASE(LP,JIF).GT.DPARM(4)) .OR.
     *               (WGT(LP,JIF).LT.DPARM(5)) .OR.
     *               (WGT(LP,JIF).GT.DPARM(6))) THEN
                     IF (DPARM(7).LE.0.0) ANTNO(1) = IANT
                     TIMER(1) = GTIME - DELTAT
                     TIMER(2) = GTIME + DELTAT
                     IF (DOIFS.LE.0.0) THEN
                        IFS(1) = LIF
                        IFS(2) = LIF
                        END IF
                     REASON = 'A&P:'
                     IC = 6
                     IF ((AMPL(LP,JIF).LT.DPARM(1)) .OR.
     *                  (AMPL(LP,JIF).GT.DPARM(2))) THEN
                        REASON(IC:) = 'GAIN'
                        IC = IC + 5
                        END IF
                     IF ((PHASE(LP,JIF).LT.DPARM(3)) .OR.
     *                  (PHASE(LP,JIF).GT.DPARM(4))) THEN
                        REASON(IC:) = 'PHASE'
                        IC = IC + 6
                        END IF
                     IF ((WGT(LP,JIF).LT.DPARM(5)) .OR.
     *                  (WGT(LP,JIF).GT.DPARM(6))) THEN
                        REASON(IC:) = 'WEIGHT'
                        IC = IC + 7
                        END IF
                     CALL TABFLG ('WRIT', BUFF1, OFGRNO, OKOLS, ONUMV,
     *                  SOURID, SUBA, FREQID, ANTNO, TIMER, IFS, CHANS,
     *                  PFLAGS(1,LP), REASON, IERR)
                     IF (IERR.NE.0) GO TO 980
                     NFLAGS = NFLAGS + 1
                     IF ((DOSTOK.GT.0.0) .AND. (DOIFS.GT.0.0)) GO TO 100
                     IF (DOIFS.GT.0.0) GO TO 90
                     END IF
                  END IF
 80            CONTINUE
 90         CONTINUE
 100     CONTINUE
C                                       done
 980  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1980) IERR
         CALL MSGWRT (8)
         END IF
      CALL TABIO ('CLOS', 0, OFGRNO, BUFF1, BUFF1, I)
      IF ((SUBARR.EQ.SUBA2) .OR. (IERR.NE.0)) THEN
         CALL TABIO ('CLOS', 0, ICLRNO, GNREC, CLBUFF, I)
         END IF
      IF (FGIN.GT.0) THEN
         WRITE (MSGTXT,1200) 'Added', NFLAGS, FGOUT, SUBARR
      ELSE
         WRITE (MSGTXT,1200) 'Wrote', NFLAGS, FGOUT, SUBARR
         END IF
      CALL MSGWRT (4)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Copied',I9,' flags from vers',I4,' to',I4)
 1015 FORMAT ('SNFLGA&P ',A,1X,A)
 1020 FORMAT ('SNDFLG ERROR',I5,' FROM TABIO')
 1200 FORMAT (A,I8,' flag records to FG version',I4,' subarray',I4)
 1980 FORMAT ('SNDFLG ERROR',I5,' WRITING OUTPUT FLAG TABLE')
      END
      SUBROUTINE SNFLGH
C-----------------------------------------------------------------------
C   SNFLGH writes the flagging information in the history file
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, CTIME*8, CDATE*12
      INTEGER   I, IROUND, IRET, LUN, DATE(3), TIME(3), J, J1, BUFF(256)
      LOGICAL   T
      INCLUDE 'SNFLG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA T /.TRUE./
      DATA LUN /27/
C-----------------------------------------------------------------------
C                                       Open history file
      CALL HIINIT (2)
      CALL HIOPEN (LUN, DISKIN, CNOIN, BUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Write time and date on old file
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME, CDATE)
      WRITE (HILINE,1000) TSKNAM, RLSNAM, CDATE, CTIME
      CALL HIADD (LUN, HILINE, BUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       operation
      WRITE (HILINE,1010) TSKNAM, OPTYPE
      CALL HIADD (LUN, HILINE, BUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       input file
      WRITE (HILINE,1011) TSKNAM, TYPE, IVER
      CALL HIADD (LUN, HILINE, BUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       cutoff
      IF (CUTOFF.GT.0.0) THEN
         WRITE (HILINE,1015) TSKNAM, CUTOFF
         CALL HIADD (LUN, HILINE, BUFF, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
C                                       inclusion restrictions
C                                       Sources
      IF (XSOUR(1).EQ.' ') THEN
         WRITE (HILINE,1020) TSKNAM
         CALL HIADD (LUN, HILINE, BUFF, IRET)
         IF (IRET.NE.0) GO TO 100
      ELSE
C                                       1st 2 and label.
         WRITE (HILINE,1021) TSKNAM, XSOUR(1), XSOUR(2)
         CALL HIADD (LUN, HILINE, BUFF, IRET)
         IF (IRET.NE.0) GO TO 100
C                                       Rest of sources
         DO 20 I = 3,30,2
            IF (XSOUR(I).NE.' ') THEN
               WRITE (HILINE,1022) TSKNAM, XSOUR(I), XSOUR(I+1)
               CALL HIADD (LUN, HILINE, BUFF, IRET)
               IF (IRET.NE.0) GO TO 100
               IF (XSOUR(I+1).EQ.' ') GO TO 30
            ELSE
               GO TO 30
               END IF
 20         CONTINUE
         END IF
C                                       qual freqid
 30   I = IROUND (XQUAL)
      WRITE (HILINE,1030) TSKNAM, I, FRQSEL
      CALL HIADD (LUN, HILINE, BUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       TIMERANG
      CALL HITIME (TSTART, TSTOP, LUN, BUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Stokes'
      WRITE (HILINE,1031) TSKNAM, XSTOK
      CALL HIADD (LUN, HILINE, BUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       IF range
      WRITE (HILINE,1032) TSKNAM, BIF, EIF
      CALL HIADD (LUN, HILINE, BUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Subarray
      WRITE (HILINE,1033) TSKNAM, SUBARR
      CALL HIADD (LUN, HILINE, BUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       antennas
      IF (NANTSL.LE.0) THEN
         WRITE (HILINE,1040) TSKNAM
         CALL HIADD (LUN, HILINE, BUFF, IRET)
         IF (IRET.NE.0) GO TO 100
      ELSE
         IF (DOAWNT) THEN
            HILINE = TSKNAM // ' / antennas included'
         ELSE
            HILINE = TSKNAM // ' / antennas excluded'
            END IF
         CALL HIADD (LUN, HILINE, BUFF, IRET)
         IF (IRET.NE.0) GO TO 100
         DO 40 I = 1,NANTSL,5
            J1 = MIN (NANTSL, I+4)
            WRITE (HILINE,1041) TSKNAM, (ANTS(J), J = I,J1)
            IF (I.GT.1) HILINE(7:16) = ' '
            CALL HIADD (LUN, HILINE, BUFF, IRET)
            IF (IRET.NE.0) GO TO 100
 40         CONTINUE
         END IF
C                                       flag tables
      IF (FGIN.GT.0) THEN
         WRITE (HILINE,1050) TSKNAM, FGIN, FGOUT
      ELSE
         WRITE (HILINE,1051) TSKNAM, FGOUT
         END IF
      CALL HIADD (LUN, HILINE, BUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       extend flags
      IF (DOSTOK.GT.0.0) THEN
         HILINE = TSKNAM // 'DOSTOKES = 1   / flag all Stokes' //
     *      ' when one is bad'
         CALL HIADD (LUN, HILINE, BUFF, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
      IF (DOIFS.GT.0.0) THEN
         HILINE = TSKNAM // 'DOIFS = 1   / flag all IFs' //
     *      ' when one is bad'
         CALL HIADD (LUN, HILINE, BUFF, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
C                                       OPTYPE parameters
      IF (OPTYPE.EQ.'A&P') THEN
         IF (DPARM(7).GT.0.0) THEN
            HILINE = TSKNAM // 'DPARM(7) = 1   / flag all ' //
     *         'antennas when one is bad'
            CALL HIADD (LUN, HILINE, BUFF, IRET)
            IF (IRET.NE.0) GO TO 100
            END IF
         WRITE (HILINE,1060) TSKNAM, DPARM(1), DPARM(2)
         CALL HIADD (LUN, HILINE, BUFF, IRET)
         IF (IRET.NE.0) GO TO 100
         WRITE (HILINE,1061) TSKNAM, DPARM(3), DPARM(4)
         CALL HIADD (LUN, HILINE, BUFF, IRET)
         IF (IRET.NE.0) GO TO 100
         WRITE (HILINE,1062) TSKNAM, DPARM(5), DPARM(6)
         CALL HIADD (LUN, HILINE, BUFF, IRET)
         IF (IRET.NE.0) GO TO 100
      ELSE IF (OPTYPE.EQ.'AMP') THEN
         WRITE (HILINE,1066) TSKNAM, DPARM(1)
         CALL HIADD (LUN, HILINE, BUFF, IRET)
         IF (IRET.NE.0) GO TO 100
         WRITE (HILINE,1067) TSKNAM, DPARM(2)
         CALL HIADD (LUN, HILINE, BUFF, IRET)
         IF (IRET.NE.0) GO TO 100
      ELSE
         WRITE (HILINE,1068) TSKNAM, DPARM(1)
         CALL HIADD (LUN, HILINE, BUFF, IRET)
         IF (IRET.NE.0) GO TO 100
         WRITE (HILINE,1069) TSKNAM, DPARM(2)
         CALL HIADD (LUN, HILINE, BUFF, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
      WRITE (HILINE,1070) TSKNAM, NFLAGS
      CALL HIADD (LUN, HILINE, BUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C
 100  CALL HICLOS (LUN, T, BUFF, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',A12,2X,A8)
 1010 FORMAT (A6,'OPTYPE = ''',A,''' / operation type')
 1011 FORMAT (A6,'INEXT = ''',A,''', INVERS=',I6,' / input cal table')
 1015 FORMAT (A6,'CUTOFF = ',F8.3,' / only weights > CUTOFF considered')
 1020 FORMAT (A6,'SOURCES = ''''    /All sources selected')
 1021 FORMAT (A6,'SOURCES  = ''',A,''',''',A,'''')
 1022 FORMAT (A6,'          ,''',A,''',''',A,'''')
 1030 FORMAT (A6,'QUAL =',I4,'  FREQID =',I3)
 1031 FORMAT (A6,'STOKES = ''',A4,''' / Stokes type')
 1032 FORMAT (A6,'BIF =',I4,', EIF =',I4,'/ IF range')
 1033 FORMAT (A6,'SUBARRAY =',I4)
 1040 FORMAT (A6,'ANTENNAS = 0    /all antennas included')
 1041 FORMAT (A6,'ANTENNAS = ',4(I3,','),I3)
 1050 FORMAT (A6,'FLAGVER =',I5,' OUTFGVER =',I5,
     *   ' / in FG copied to out')
 1051 FORMAT (A6,'OUTFGVER =',I5,' / brand new output FG file')
 1060 FORMAT (A6,'DPARM =',F6.4,F10.4,' / range allowed amp gains')
 1061 FORMAT (A6,'DPARM(3)~',2F7.1,' / range allowed phases')
 1062 FORMAT (A6,'DPARM(5)~',2F10.2,' / range allowed weights')
 1066 FORMAT (A6,'DPARM(1) =',F5.2,' / flag > DPARM(1)*rms amp gain')
 1067 FORMAT (A6,'DPARM(2) =',F7.2,' / flag interval in seconds')
 1068 FORMAT (A6,'DPARM(1) =',F7.2,' / flag phase jumps > DPARM(1)')
 1069 FORMAT (A6,'DPARM(2) =',F7.2,' / min interval in seconds')
 1070 FORMAT (A6,'/ wrote',I9,' new flag records to output flag table')
      END
