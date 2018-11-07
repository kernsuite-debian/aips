LOCAL INCLUDE 'UVPRM.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER NAMEIN*12, CLAIN*6
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XSTOK
      REAL      XSIN, XDISIN, XQUAL, XBAND, XFREQ, XFQID, XTIME(8),
     *   XANT(50), XBASE(50), XUVRA(2), XSUBA, XCHAN, XBIF, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH(3), BPARM(10), XBADD(10), FLUX
      REAL      BUFF1(UVBFSS), TBEG, TFIN, XYSCL(2), XYOFF(2),
     *   RPARM(20), FRQSCL, UVRANG(2)
      INTEGER   IAW1, IAW2, INC, SEQIN, DISKIN, LUNI, INDI, TYPCOR,
     *   TYPCCR, TYPEAX(2), NCH, FIF, VER, TESTEM(2), JBUFSZ, IANT(50),
     *   NANT, IBAS(50), NBAS, CNOIN, IFRQ, NFRQ, TVCHN,
     *   TVCORN(4), EXCLFQ(1000)
      LOGICAL   UVREV, SCALEM(2), NOUVR, MULTI, DESEL
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XSTOK, XBAND, XFREQ, XFQID, XTIME, XANT, XBASE, XUVRA, XSUBA,
     *   XCHAN, XBIF, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG,
     *   XDOBND, XBPVER, XSMOTH, BPARM, FLUX, XBADD
      COMMON /CHPARM/ NAMEIN, CLAIN
      COMMON /BUFRS/ BUFF1, RPARM, JBUFSZ
      COMMON /UVPCOM/ TBEG, TFIN, XYSCL, XYOFF, FRQSCL, UVREV, SCALEM,
     *   NOUVR, MULTI, IAW1, IAW2, INC, SEQIN, DISKIN, LUNI, INDI,
     *   TYPCOR, TYPCCR, TYPEAX, NCH, FIF, VER, TESTEM, CNOIN, IFRQ,
     *   NFRQ, TVCHN, TVCORN, EXCLFQ, UVRANG
      COMMON /BASSEL/ DESEL, IANT, NANT, IBAS, NBAS
LOCAL END
      PROGRAM UVPRM
C-----------------------------------------------------------------------
C! UVPRM plots uvdata, makes a standard 'PL' extension file
C# EXT-appl Graphics Plot appl UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2006, 2009-2012, 2015
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
C   UVPRM plots uv data . A 'PL' extension file is made which can
C   be displayed in the usual ways .
C   Inputs:
C     INNAME         NAMEIN        Name of input UV data.
C     INCLASS        CLAIN         Class of input UV data.
C     INSEQ          SEQIN         Seq. of input UV data.
C     INDISK         DISKIN        Disk number of input VU data.
C     CHANNEL        NCH           Channel #: 0 or 1 ok for cont.
C     BIF            FIF           IF number.
C     UVRANGE....Range of UV projected spacings to include (Klambda)
C     APARM......Selection parameters:
C        1 = Start IAT day (day 0 = first day in data base)
C        2 = Start IAT hour
C        3 = Start IAT minute
C        4 = Start IAT second
C        5 = Stop IAT day (day 0 = first day in data base)
C        6 = Stop IAT hour
C        7 = Stop IAT minute
C        8 = Stop IAT second
C        9 = antenna number
C            0 => all antennas.
C            100*M + N => correlator (M,N) with M<N
C        10: 1 => plot RR data only
C            2 => plot LL data only
C            3 => plot RL data only
C            4 => plot LR data only
C            5 => plot IPOL
C            6 => plot VPOL
C            7 => plot QPOL
C            8 => plot UPOL
C     BPARM......Control parameters:
C        1 = type of X-axis (where  1 = amplitude (Jy), 2 = phase
C           (degrees), 3 = u,v distance (klambda), 4 = u,v p.a.
C           (degrees, clockwise from v-axis), 5 = time (iat days),
C           6 = u, 7 = v, 8 = w (all in klambda), 9 = real part (Jy),
C           and 10 = imaginary part(Jy).)
C        2 = type of Y-axis
C        3 = Do not autoscale if non-zero, use following values:
C        4 = Minimum of X-axis,
C        5 = Maximum of X-axis,
C        6 = Minimum of Y-axis.
C        7 = Maximum of Y-axis,
C        8 = Number of bins on X-axis for averaging, 0=> no averaging.
C        9   If .ne. 0 then write binned values to message file
C-----------------------------------------------------------------------
      CHARACTER  PRGM*6
      INTEGER  IRET
      INCLUDE 'UVPRM.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'UVPRM '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVPIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       determine scaling
      IF ((SCALEM(1)) .OR. (SCALEM(2))) CALL SCAL (IRET)

      IF (IRET.NE.0) GO TO 995
C                                       Do Find UV data bins
      CALL FNDUV (IRET)
C                                       Close down
 995  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE UVPIN (PRGM, JERR)
C-----------------------------------------------------------------------
C   UVPIN gets input parameters for UVPRM .
C   Inputs:  PRGM   C*6       Program name
C   Output:  JERR   I         Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   JERR
C
      CHARACTER UTYPE*2, STAT*4
      INTEGER   IUSER, I, IERR, ITEMP, IROUND, IDUM, NPARM, LUNTB,
     *   LUN, FQVER, NIF
      LOGICAL   TABLE, FITASC, F, MATCH, SNEXST, EXIST
      INCLUDE 'UVPRM.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA F /.FALSE./
      DATA LUNTB /39/
C-----------------------------------------------------------------------
C                                       Init IO et al.
      TSKNAM = PRGM
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
      VER = 10000
C                                       Get input parameters.
      NPARM = 278
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 10
         JERR = 8
         RQUICK = .TRUE.
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XSTOK, STOKES)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 25 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 25      CONTINUE
C                                       Crunch input parameters.
      IUSER = NLUSER
      INC = 1
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
C                                       Get CATBLK from file.
      LUNI = 48
      UTYPE = 'UV'
      STAT = 'HDWR'
      STAT = 'READ'
      CALL MAPOPN (STAT, DISKIN, NAMEIN, CLAIN, SEQIN, UTYPE, IUSER,
     *   LUNI, INDI, CNOIN, CATBLK, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1035) IERR
         GO TO 990
 40   CALL CHR2H (12, NAMEIN, 1, XNAMEI)
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
         CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUNTB, BUFF1, TABLE, MULTI,
     *      FITASC, JERR)
         MULTI = MULTI .AND. (JERR.EQ.0)
         END IF
C                                       If calibrating, does SN exist
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      IF ((DOCAL) .AND. (.NOT.MULTI)) THEN
C                                       Look for SN file
         CALL ISTAB ('SN', DISKIN, CNOIN, 1, LUNTB, BUFF1,
     *      TABLE, SNEXST, FITASC, JERR)
         IF ((.NOT.SNEXST) .OR. (JERR.NE.0)) THEN
            WRITE (MSGTXT,1050)
            CALL MSGWRT (8)
            DOCAL = .FALSE.
            END IF
         END IF
      XSIN = SEQIN
      XDISIN = DISKIN
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      CALL RCOPY (8, XTIME, TIMRNG)
      UVRNG(1) = XUVRA(1)
      UVRNG(2) = XUVRA(2)
      BCHAN = IROUND (XCHAN)
      BCHAN = MAX (1, BCHAN)
      IF (BCHAN.GT.CATBLK(KINAX+JLOCF)) BCHAN = CATBLK(KINAX+JLOCF)
      ECHAN = BCHAN
      BIF = IROUND (XBIF)
      BIF = MAX (1, BIF)
      IF (JLOCIF.LT.0) BIF = 1
      IF ((JLOCIF.GT.0) .AND. (BIF.GT.CATBLK(KINAX+JLOCIF)))
     *   BIF = CATBLK(KINAX+JLOCIF)
      EIF = BIF
      FIF = BIF
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      DOAPPL = F
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LT.0) SUBARR = 0
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
C                                       Check STOKES
      CALL SETSTK (STOKES, DOCAL, IDUM)
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
C                                       Allow multiple FQ ids
      NFRQ = 1
      IF (FRQSEL.LE.0 .AND. SELBAN.LE.0.0 .AND. SELFRQ.LE.0D0) THEN
         FRQSEL = 1
C                                       Determine the number of FREQIDs.
         FQVER = 1
         CALL ISTAB ('FQ', DISKIN, CNOIN, FQVER, LUN, FQBUFF, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) THEN
            CALL FQINI ('READ', FQBUFF, DISKIN, CNOIN, FQVER, CATBLK,
     *         LUN, IFQRNO, FQKOLS, FQNUMV, NIF, JERR)
            IF (JERR.NE.0) GO TO 999
            NFRQ = FQBUFF(5)
            IF (NFRQ.GT.1) THEN
               WRITE (MSGTXT,1060) NFRQ
               CALL MSGWRT (3)
               END IF
            CALL TABIO ('CLOS', 0, IFQRNO, FQBUFF, FQBUFF, JERR)
            IF (JERR.NE.0) GO TO 999
            END IF
         END IF
C                                       Find specified FQ id
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DO 80 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 80      CONTINUE
C                                       Test type of plot
      IF ((BPARM(1).LT.1.0) .OR. (BPARM(1).GT.11.0)) BPARM(1) = 3.0
      IF ((BPARM(2).LT.1.0) .OR. (BPARM(2).GT.11.0)) BPARM(2) = 1.0
      IF ((BPARM(1).NE.3) .AND. ((BPARM(1).LT.6) .OR. (BPARM(1).GT.8)))
     *   GO TO 30
         BPARM(4) = BPARM(4) * 1.0E3
         BPARM(5) = BPARM(5) * 1.0E3
 30   IF ((BPARM(2).NE.3) .AND. ((BPARM(2).LT.6) .OR. (BPARM(2).GT.8)))
     *   GO TO 35
         BPARM(6) = BPARM(6) * 1.0E3
         BPARM(7) = BPARM(7) * 1.0E3
 35   XYSCL(1) = -1.0E10
      XYSCL(2) = XYSCL(1)
      XYOFF(1) = 1.E10
      XYOFF(2) = XYOFF(1)
C                                       If plotting uv only
C                                       then plot conjugate points
      ITEMP = BPARM(1) * BPARM(2) + 0.1
      UVREV = (ITEMP.EQ.42) .OR. (ITEMP.EQ.48) .OR. (ITEMP.EQ.56)
C                                       Autoscale ?
      SCALEM(1) = (BPARM(3).LE.0.0) .OR. (BPARM(4).EQ.BPARM(5))
      SCALEM(2) = (BPARM(3).LE.0.0) .OR. (BPARM(6).EQ.BPARM(7))
      TESTEM(1) = 1
      IF (BPARM(4).GT.BPARM(5)) TESTEM(1) = -1
      IF ((BPARM(3).EQ.0.0) .OR. (BPARM(4).EQ.BPARM(5))) TESTEM(1) = 0
      TESTEM(2) = 1
      IF (BPARM(6).GT.BPARM(7)) TESTEM(2) = -1
      IF ((BPARM(3).EQ.0.0) .OR. (BPARM(6).EQ.BPARM(7))) TESTEM(2) = 0
      TYPEAX(1) = IROUND (BPARM(1))
      TYPEAX(2) = IROUND (BPARM(2))
C                                       Clear antenna selection
C                                       criteria for UVGET
      DO 100 I = 1,50
         ANTENS(I) = 0
 100     CONTINUE
C                                       Initialize baseline selection
      CALL SETANT (50, XANT, XBASE, NANT, NBAS, IANT, IBAS, DESEL)
C                                       Update catalog header.
 200  VER = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVPIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1035 FORMAT ('ERROR',I3,' FINDING THE UV DATA SET')
 1050 FORMAT ('NO SN FILE FOUND, BUT DOCALIB IS TRUE: NO CAL APPLIED')
 1060 FORMAT ('Examinning ',I4,' frequency IDs.')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
      END
      SUBROUTINE SCAL (IRET)
C-----------------------------------------------------------------------
C   SCAL sends uv points one at a time to XYOFF .
C   IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I, NUMVIS, XUMVIS, J
      REAL      XY(2)
      LOGICAL   REQBAS
      INCLUDE 'UVPRM.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
 10   NUMVIS = 0
      XUMVIS = 0
C                                       Loop for each FREQID.
      DO 150 IFRQ = 1,NFRQ
         IF (NFRQ.GT.1) THEN
            FRQSEL = IFRQ
            EXCLFQ(IFRQ) = 0
            WRITE (MSGTXT,1000) IFRQ
            CALL MSGWRT (4)
            END IF
C                                       Init vis file for read.
         CALL UVGET ('INIT', RPARM, BUFF1, IRET)
         IF ((NFRQ.GT.1) .AND. (IRET.EQ.5)) THEN
            IRET = 0
            EXCLFQ(IFRQ) = 1
            GO TO 140
            END IF
         IF (IRET.GT.0) GO TO 999
         FRQSCL = FREQ / UVFREQ
C                                       Loop
 100     CONTINUE
C                                       Read vis. record.
            CALL UVGET ('READ', RPARM, BUFF1, IRET)
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
     *         GO TO 100
            NUMVIS = NUMVIS + 1
            IF (MOD(NUMVIS,INC).NE.0) GO TO 100
C                                       Find scales
            CALL FNDXY (RPARM, BUFF1, 1, 1, TYPEAX, FRQSCL, XY)
            CALL XYSCAL (NUMVIS, XY, IRET)
            IF (IRET.EQ.0) XUMVIS = XUMVIS + 1
            GO TO 100
 140     CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
 150     CONTINUE
C                                       Any valid points
      IF (XUMVIS.LE.1) THEN
         IRET = 4
         WRITE (MSGTXT,1200) XUMVIS
         GO TO 990
         END IF
C                                       Store UVRANGE FOUND
      UVRANG(1) = XYOFF(1)
      UVRANG(2) = XYSCL(1)
C                                       Final call to XYSCL.
      NUMVIS = -1
      CALL XYSCAL (NUMVIS, XY, IRET)
      IF (IRET.LE.0) GO TO 220
         WRITE (MSGTXT,1110) IRET
         GO TO 990
 220  IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Processing FREQID =',I3)
 1100 FORMAT ('SCAL: ERROR',I3,' READING VIS FILE')
 1110 FORMAT ('SCAL: XYSCL ERROR',I3)
 1200 FORMAT ('FOUND',I5,' POINTS: NOT ENOUGH TO SELF-SCALE')
      END
      SUBROUTINE FNDUV (IRET)
C-----------------------------------------------------------------------
C   FNDUV examines and bins the data.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C                          1 => failed to add to catalog
C                          2 => failed to create
C                          3 => graph file write error
C                          4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER MAXBIN, MXTBIN
      PARAMETER (MAXBIN=200, MXTBIN=3000)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IERR, ITYPE, IPSIZE, I, IRET, NPARMS, IAPARM(8), J, JJJ,
     *   NBIN, IBIN, IROUND, NGOOD, VALUES(4), NNOFIT, NUMVIS,
     *   PFQSID(MAXIF), MINANT, REFANT, LSTBIN, ANT1, ANT2,
     *   TIMBIN(MXTBIN), NTBINS, NTIME, NT
      REAL      BLC(2), TRC(2), XYRATO, XY(2), REFAVG,
     *   SUM(MAXBIN), SUM2(MAXBIN), AVERG, STDEV, XKCNT(MAXBIN),
     *   XZY(2), PFQTBW(MAXIF), PFQCHW(MAXIF), PBW, UVMIN, RMSMIN,
     *   ANTSUM(MAXANT), ANTSU2(MAXANT), ANTCNT(MAXANT), ANTRAD(MAXANT),
     *   FLXAT0, RMSAT0, TIMER, TIMAX, TIMIN, OTIME
      DOUBLE PRECISION PFQFRQ(MAXIF), PFREQ
      HOLLERITH CATH(256)
      LOGICAL   T, NOCHK, REQBAS, FINDFL, ZEROFL
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'UVPRM.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (CATBLK, CATH)
      DATA T /.TRUE./,   FINDFL, ZEROFL/.TRUE.,.TRUE./
      DATA UVMIN /0/, LSTBIN /0/, NTBINS/1/
C-----------------------------------------------------------------------
      NGOOD = 0
      NNOFIT = 0
      IRET = 1
C                                       Prepare for possible data bins
      NBIN = IROUND (BPARM(8))
      IF (NBIN.LT.1) NBIN = 50
      NBIN = MIN (MAXBIN, NBIN)
      BPARM(8) = NBIN
C                                       See if data range check
      NOCHK = (NBIN.NE.0) .AND. (BPARM(3).GT.0.)
C                                       Init Bins
      CALL RFILL (IBIN, 0.0, SUM)
      CALL RFILL (IBIN, 0.0, SUM2)
      CALL RFILL (IBIN, 0.0, XKCNT)
C                                       Init Antenna Bins
      CALL RFILL (MAXANT, 0.0, ANTSUM)
      CALL RFILL (MAXANT, 0.0, ANTSU2)
      CALL RFILL (MAXANT, 0.0, ANTCNT)
      CALL RFILL (MAXANT, 0.0, ANTRAD)
C                                       For X and Y axies
      DO 15 I = 1,2
C                                       IF User sets the scales
        IF (.NOT. SCALEM(I)) THEN
C                                       Read user Scale Values
           XYSCL(I) = BPARM(3+2*I)
           XYOFF(I) = BPARM(2+2*I)
           IF (XYSCL(I).EQ.XYOFF(I)) GO TO 999
           XYSCL(I) = 1000. / (XYSCL(I)-XYOFF(I))
C                                       End If reading user scales
           END IF
 15     CONTINUE
C                                       If X axis limits not set by user
        IF (SCALEM(1)) THEN
C                                       Transfer to array
           CALL RCOPY (1, UVRANG(1), VALUES(1))
C                                       Save UVRANGE Minimum
           CALL CATKEY ('WRIT', DISKIN, CNOIN, 'UVPRAMIN', 1, 1,
     *         VALUES, 2, CATBLK, IERR)
C                                       Transfer to array
           CALL RCOPY (1, UVRANG(2), VALUES(1))
C                                       Save UVRANGE Minimum
           CALL CATKEY ('WRIT', DISKIN, CNOIN, 'UVPRAMAX', 1, 1,
     *         VALUES, 2, CATBLK, IERR)
           END IF
C                                       Fill in last of actual parms
      BPARM(5) = 1000.0/XYSCL(1) + XYOFF(1)
      BPARM(7) = 1000.0/XYSCL(2) + XYOFF(2)
      BPARM(4) = XYOFF(1)
      BPARM(6) = XYOFF(2)
      IPSIZE = 0
      ITYPE = 8
      NPARMS = 278
C                                      Initialize UV reading
      IF ((UVRNG(1).EQ.0.0) .AND. (UVRNG(2).EQ.1.0E10)) NOUVR = T
      XUVRA(1) = UVRNG(1)
      XUVRA(2) = UVRNG(2)
C                                       UV range set
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      XYRATO = 1.0
      IRET = 3
      CALL FILL (5, 1, IAPARM)
C                                       FQ info
      IF (DOFQSL) THEN

         IF (NFRQ.EQ.1) THEN
            CALL GETFQ (FRQSEL, DISKIN, CNOIN, CATUV, 45, PFQFRQ,
     *         PFQTBW, PFQCHW, PFQSID, BNDCOD, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1080) IERR
               GO TO 990
               END IF
            PFREQ = PFQFRQ(BIF) / 1.0D9
            PBW = PFQTBW(BIF) / 1.0E6
            END IF
         END IF
 50   CONTINUE
      NUMVIS = 0
C                                       Loop for each FREQID.
      DO 150 IFRQ = 1,NFRQ
         IF (NFRQ.GT.1) THEN
            IF (EXCLFQ(IFRQ).NE.0) GO TO 150
            FRQSEL = IFRQ
            END IF
C                                       Initialize UV reading.
         CALL UVGET ('INIT', RPARM, BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT, 1050) IRET
            CALL MSGWRT (8)
            IRET = 4
            GO TO 975
            END IF
         FRQSCL = FREQ / UVFREQ
C                                       Loop
 100        CONTINUE
C                                       Read vis. record.
            CALL UVGET ('READ', RPARM, BUFF1, IRET)
            IF (IRET.EQ.-1) GO TO 140
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) IRET
               CALL MSGWRT (8)
               IRET = 4
               GO TO 975
               END IF
C                                       Find min and max time range
C                                       Get current time
            TIMER = RPARM(ILOCT+1)
C                                       Count time in 30 second units
            NTIME = TIMER*2880.0
C                                       If first vis, init
            IF (NUMVIS.LE.0) THEN
               TIMIN = TIMER
               TIMAX = TIMER
               LSTBIN= NTIME
               TIMBIN(NTBINS) = NTIME
            ELSE
C                                       Else not first time
C                                       IF not the same time
               IF (LSTBIN.NE.NTIME) THEN
C                                       set min and max
                  TIMIN = MIN (TIMER, TIMIN)
                  TIMAX = MAX (TIMER, TIMAX)
C                                       has this time been seen?
                  DO 110 NT = 1, NTBINS
C                                       jump out if time found
                     IF (TIMBIN(NT).EQ.NTIME) GO TO 115
 110                 CONTINUE
C                                       if hear, save a new time
                  NTBINS = MIN(NTBINS+1,MXTBIN)
                  TIMBIN(NTBINS) = NTIME
 115              CONTINUE
C                                       record new last
                  LSTBIN = NTIME
                  END IF
               END IF
C                                       Is this baseline needed?
            IF (ILOCB.GE.0) THEN
               ANT1 = INT (RPARM(ILOCB+1)) / 256
               ANT2 = MOD (INT (RPARM(ILOCB+1)), 256)
            ELSE
               ANT1 = RPARM(ILOCA1+1) + 0.1
               ANT2 = RPARM(ILOCA2+1) + 0.1
               END IF
            IF (.NOT.REQBAS (ANT1, ANT2, DESEL, IANT, NANT, IBAS, NBAS))
     *         GO TO 100
            NUMVIS = NUMVIS + 1
            IF (MOD(NUMVIS,INC).NE.0) GO TO 100
C                                       Get and scale X, Y
            CALL FNDXY (RPARM, BUFF1, 1, 1, TYPEAX, FRQSCL, XZY)
            DO 128 JJJ = 1,2
               DO 120 J = 1,2
                  XY(J) = XYSCL(J) * (XZY(J) - XYOFF(J))
                  IF (((XY(J).GE.BLC(J)) .AND. (XY(J).LE.TRC(J)))
     *               .OR. ((NOCHK) .AND. (J.EQ.2))) GO TO 120
                  NNOFIT = NNOFIT + 1
                  GO TO 127
 120              CONTINUE
               NGOOD = NGOOD + 1
C                                       If binning - accumulate
               IF (NBIN.GT.0) THEN
                  IBIN = 1 + NBIN * XY(1) / 1000.
                  IF (IBIN.LT.1) IBIN = 1
                  IF (IBIN.GT.NBIN) IBIN = NBIN
                  SUM(IBIN) = SUM(IBIN) + XY(2)
                  SUM2(IBIN) = SUM2(IBIN) + XY(2)*XY(2)
                  XKCNT(IBIN) = XKCNT(IBIN) + 1.0
                  END IF
C                                      Record values for each antenna
              ANTSUM(ANT1) = ANTSUM(ANT1) + XY(2)
              ANTSU2(ANT1) = ANTSU2(ANT1) + XY(2)*XY(2)
              ANTCNT(ANT1) = ANTCNT(ANT1) + 1.0
C                                      Sum Uv spacings
              ANTRAD(ANT1) = ANTRAD(ANT1) + XY(1)
C                                      Sums for other antenna
              ANTSUM(ANT2) = ANTSUM(ANT2) + XY(2)
              ANTSU2(ANT2) = ANTSU2(ANT2) + XY(2)*XY(2)
              ANTCNT(ANT2) = ANTCNT(ANT2) + 1.0
              ANTRAD(ANT2) = ANTRAD(ANT2) + XY(1)
 127        IF (.NOT.UVREV) GO TO 130
            XZY(1) = -XZY(1)
            XZY(2) = -XZY(2)
 128        CONTINUE
 130     CONTINUE
         GO TO 100
 140     CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
 150     CONTINUE
C                                       Done:
C                                       Print binned data.
      DO 210 IBIN = 1,NBIN
         IF (XKCNT(IBIN).GT.0.0) THEN
            AVERG = SUM(IBIN) / XKCNT(IBIN)
            STDEV = 0.0
            IF (XKCNT(IBIN).GE.2.0)
     *         STDEV = SQRT (ABS ((SUM2(IBIN)/XKCNT(IBIN)) -
     *         AVERG*AVERG)/(XKCNT(IBIN) - 1.0))
            XY(1) = (IBIN * 1000. - 500.) / NBIN
            XY(2) = AVERG
C                                       Write binned values
            XY(1) = XY(1) / XYSCL(1) + XYOFF(1)
            XY(2) = XY(2) / XYSCL(2) + XYOFF(2)
            STDEV = STDEV / XYSCL(2)
            WRITE (MSGTXT,1200) IBIN, IROUND (XKCNT(IBIN)),
     *            XY(1), XY(2), STDEV
C                                       Tell user if requested
            IF (BPARM(9).NE.0.0) CALL MSGWRT (5)
C                                       Find first UV spacing with Flux
            IF (FINDFL .AND. XY(2).LT.FLUX) THEN
               FINDFL = .FALSE.
               UVMIN  = XY(1)
               END IF
C                                       Get minimum spacing flux
            IF (ZEROFL) THEN
               ZEROFL = .FALSE.
               FLXAT0 = XY(2)
               RMSAT0 = STDEV
               END IF
C                                       End if points in bin
            END IF
 210        CONTINUE
C                                       Transfer to Flux to array
      CALL RCOPY (1, FLXAT0, VALUES(1))
C                                       Save Zero-spacing Flux
      CALL CATKEY ('WRIT', DISKIN, CNOIN, 'UVPFLUX0', 1, 1,
     *      VALUES, 2, CATBLK, IERR)
C                                       Transfer to Flux to array
      CALL RCOPY (1, RMSAT0, VALUES(1))
C                                       Save RMS at Zero
      CALL CATKEY ('WRIT', DISKIN, CNOIN, 'UVPRMSF0', 1, 1,
     *      VALUES, 2, CATBLK, IERR)
C                                       If Flux is non zero
C                                       Record Minimum UVRANGE
      IF (FLUX.GT.0.0) THEN
         WRITE (MSGTXT,1300) FLUX, UVMIN
         CALL MSGWRT (5)
         END IF
C                                       Record UVRANGE at Flux limit
      CALL RCOPY (1, UVMIN, VALUES(1))
C
      CALL CATKEY ('WRIT', DISKIN, CNOIN, 'UVPFLMIN', 1, 1,
     *         VALUES, 2, CATBLK, IERR)
C                                       Get Observation duration (days)
      OTIME = REAL(NTBINS)/2880.0
C                                       If obs time < 10 minutes
C                                       use max minus min
      IF (OTIME.LT.0.007) OTIME = TIMAX - TIMIN
C                                       Record Obs time
      CALL RCOPY (1, OTIME, VALUES(1))
C
      CALL CATKEY ('WRIT', DISKIN, CNOIN, 'UVPOTIME', 1, 1,
     *         VALUES, 2, CATBLK, IERR)
C                                       Find Antenna with Min RMS
      RMSMIN = 1E10
      MINANT = 0
C                                       For all Antennas
      DO 300 I = 1, MAXANT
C                                       If data for this antenna
         IF (ANTCNT(I) .GT. 2.0) THEN
            AVERG = ANTSUM(I) / ANTCNT(I)
            STDEV = SQRT (ABS ((ANTSU2(I)/ANTCNT(I)) -
     *         AVERG*AVERG)/(ANTCNT(I) - 1.0))
C                                       Write binned values
            AVERG = AVERG / XYSCL(2) + XYOFF(2)
            STDEV = STDEV / XYSCL(2)
            WRITE (MSGTXT,1400) I, IROUND (ANTCNT(I)),
     *            AVERG, STDEV
C                                       Tell user if requested
            IF (BPARM(9).NE.0.) CALL MSGWRT (5)
C                                       Scale Sum of UV spacings
            ANTRAD(I) = ANTRAD(I) / ANTCNT(I)
C                                       Weight Deviation by UV spacings
            STDEV = STDEV * ANTRAD(I)
C                                       Find Minimum Standard dev.
            IF (STDEV .LT. RMSMIN) THEN
C                                       Ref ant is antenna with RMSMIN
               MINANT = I
               RMSMIN = STDEV
               END IF
            END IF
 300     CONTINUE
C                                       Find Antenna with second Min RMS
      RMSMIN = 1E10
      REFANT = 0
C                                       For all Antennas
      DO 400 I = 1, MAXANT
C                                       If data for this antenna
         IF (ANTCNT(I) .GT. 2.0) THEN
            AVERG = ANTSUM(I) / ANTCNT(I)
            STDEV = SQRT (ABS ((ANTSU2(I)/ANTCNT(I)) -
     *         AVERG*AVERG))/(ANTCNT(I) - 1.0)
C                                       Write binned values
            AVERG = AVERG / XYSCL(2) + XYOFF(2)
            STDEV = STDEV / XYSCL(2)
C                                       Weight Deviation by UV spacings
            STDEV = STDEV * ANTRAD(I)
C                                       If small rms and not Minimum
            IF (STDEV .LT. RMSMIN .AND. I.NE.MINANT) THEN
C                                       Ref ant is antenna with RMSMIN
               REFANT = I
               RMSMIN = STDEV
               REFAVG = AVERG
               END IF
            END IF
 400     CONTINUE
C                                       Un-Weight Deviation
      IF (ANTRAD(REFANT) .GT. 0.0)
     *   RMSMIN = (RMSMIN / ANTRAD(REFANT)) * ANTCNT(REFANT)
C                                       Tell User
      WRITE (MSGTXT,1500,ERR=990) REFANT, RMSMIN
      CALL MSGWRT (3)
      WRITE (MSGTXT,1550,ERR=990) FLXAT0, RMSAT0
      CALL MSGWRT (3)
C                                       put refant in array for CATKEY
      VALUES(1) = REFANT
C                                       Record ref ant in header
      CALL CATKEY ('WRIT', DISKIN, CNOIN, 'UVPREFAN', 1, 1,
     *         VALUES, 4, CATBLK, IERR)
      IRET = 0
      GO TO 990
C                                       Destroy the plot file
 975  CONTINUE
      GO TO 999
C                                       No catalog update
C                                       Messages
 990  WRITE (MSGTXT,1990) NGOOD
      CALL MSGWRT (2)
      WRITE (MSGTXT,1991) NNOFIT
      IF (NNOFIT.GE.1.0D0) CALL MSGWRT (2)
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1031 FORMAT ('Antennas',I3,' -',I3,4X,'Stokes ',A4,' IF# ',I2)
 1040 FORMAT ('UVrange ',2(1PE12.4),' wavelengths')
 1050 FORMAT ('FNDUV: ERROR',I3,' INIT VIS FILE')
 1080 FORMAT ('FNDUV: ERROR ',I3,' GETTING FQ INFO')
 1100 FORMAT ('FNDUV: ERROR',I3,' READING VIS FILE')
 1200 FORMAT ('I=',I3,', N=',I9,', X=',1PE10.3,', Y=',E10.3,
     *        ', SIG=',E10.3)
 1300 FORMAT ('Average Flux <',1PE10.3,' Jy at ',1PE10.3,' lambdas')
 1400 FORMAT ('ANT=',I3,', N=',I9,', Y=', 1PE10.3,', SIG=',1PE10.3)
 1500 FORMAT ('Reference Antenna  =',I4,', RMS = ',1PE10.3,' Jy ')
 1550 FORMAT ('Source Flux  =',1PE10.3, '  +/-   ',1PE10.3,' Jy ')
 1990 FORMAT ('FNDUV: ',I10,' Points Examined')
 1991 FORMAT ('FNDUV: ',I10,' Points Excluded')
      END
      SUBROUTINE XYSCAL (NUMVIS, XY, IRET)
C-----------------------------------------------------------------------
C   XYSCAL finds the scaling parameters needed to fit X and Y
C   into a 1000*1000 plotting area .
C   Inputs:
C      NUMVIS     I    Visibility number, -1 => final call, no data
C                      passed -> change to scaling factor from max/min
C      XY         R    plotted parameters .
C   Outputs:
C      XYOFF      R    when added to XY changes minimum to zero .
C      XYSCL      R    scale XY so that maximum is 1000.
C      IRET       I    Error return code , non-zero if error .
C-----------------------------------------------------------------------
      INTEGER   NUMVIS
      REAL      XY(2), TEMP
C
      INTEGER   IRET, I, JJ
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'UVPRM.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       If not last pass
      IF (NUMVIS.GT.0) THEN
         IRET = -1
C                                       For X, Y axies
         DO 10 I = 1,2
C                                       If in Valid Range
            IF (TESTEM(I).NE.0) THEN
               JJ = 2*I + 2
               IF ((BPARM(JJ).LT.BPARM(JJ+1)) .AND. ((XY(I).LT.
     *            BPARM(JJ)) .OR. (XY(I).GT.BPARM(JJ+1)))) GO TO 999
               IF ((BPARM(JJ).GT.BPARM(JJ+1)) .AND. ((XY(I).GT.
     *            BPARM(JJ)) .OR. (XY(I).LT.BPARM(JJ+1)))) GO TO 999
               END IF
 10         CONTINUE
         IRET = 0
C                                       Find max, min from data
         DO 30 I = 1,2
            IF (SCALEM(I)) THEN
C                                       Store Min
               IF (XY(I).LT.XYOFF(I)) XYOFF(I) = XY(I)
C                                       Store Max
               IF (XY(I).GT.XYSCL(I)) XYSCL(I) = XY(I)
               END IF
 30         CONTINUE
      ELSE
C                                       Else last pass,
C                                       Convert to scaling factors
C                                       provide room at edges too.
         DO 120 I = 1,2
            IF (SCALEM(I)) THEN
C                                       If min greater than max
               IF (XYSCL(I).LE.XYOFF(I)) GO TO 980
C                                       Deal with U,V,W axes
               IF (UVREV) THEN
                  XYSCL(I) = MAX (ABS(XYSCL(I)), ABS(XYOFF(I)))
                  XYOFF(I) = -XYSCL(I)
                  END IF
               TEMP = 0.025 * (XYSCL(I) - XYOFF(I))
               XYSCL(I) = XYSCL(I) + TEMP
               XYOFF(I) = XYOFF(I) - TEMP
C                                       If Min is near both zero and max
               IF ((XYOFF(I).GT.0.0) .AND.
     *             (XYOFF(I).LT.0.15*XYSCL(I))) THEN
C                                       set min to zero
                  XYOFF(I) = 0.0
                  END IF
C                                       Convert to Max = 1000 scale
               XYSCL(I) = 1000.0 / (XYSCL(I) - XYOFF(I))
               END IF
C                                       End for both X and Y axies
 120        CONTINUE
C                                       End if not last pass
        END IF
C                                       Skip Error Messages
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
