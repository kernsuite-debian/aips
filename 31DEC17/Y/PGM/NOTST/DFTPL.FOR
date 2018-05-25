LOCAL INCLUDE 'DFTPL.INC'
C                                       Local include for DFTPL
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSTOK, XSOUR(4,30), XCALC
      REAL      USERID, XSIN, XDISIN, UVRANG(2), TIMER(8), SHIFT(2),
     *   BPARM(10), XQUAL, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF,
     *   XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG,
     *   XDOBND, XBPVER, XSMOTH(3), DOCIRC, PRTLEV, XLABEL, XDOTV, XGRCH
      DOUBLE PRECISION FOFF(MAXIF), DXC, DYC, DZC, RAS, DECS
      REAL      BUFF1(UVBFSS), TIMR(10000), DFT(10000), ERROR(10000),
     *   TBEG, TFIN, XYSCL(2), XYOFF(2), AREA(2), OFREQ, FRPIX, TAVG,
     *   XNUL, FINC(MAXIF)
      CHARACTER NAMEIN*12, CLAIN*6
      INTEGER   SEQIN, DISKIN, LUNI, INDI, LABEL, VER, NBASL, BCNT,
     *   TESTEM(2), JBUFSZ, GRCHN, TVCHN, TVCORN(4), NPARMS, IPLOT,
     *   FREQID, KNCS, KNCF, KNCIF, LTYPE
      LOGICAL   UVREV, SCALEM(2), NOUVR, DOGRID, DOTV, PACKED
      COMMON /INPARM/ USERID, XNAMEI, XCLAIN, XSIN, XDISIN, UVRANG,
     *   TIMER, SHIFT, XSTOK, BPARM, XSOUR, XQUAL, XCALC, XBAND, XFREQ,
     *   XFQID, XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, DOCIRC,
     *   PRTLEV, XLABEL, XDOTV, XGRCH
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /FTPCOM/ FOFF, DXC, DYC, DZC, RAS, DECS, FINC, TIMR, DFT,
     *   ERROR, TBEG, TFIN, TAVG, XYSCL, XYOFF, OFREQ, FRPIX, AREA,
     *   XNUL, UVREV, SCALEM, NOUVR, DOGRID, NBASL, BCNT, SEQIN, DISKIN,
     *   LUNI, INDI, LABEL, VER, TESTEM, GRCHN, TVCHN, TVCORN, DOTV,
     *   NPARMS, IPLOT, FREQID, PACKED, KNCS, KNCF, KNCIF, LTYPE
      COMMON /CHRCOM/ NAMEIN, CLAIN
C                                                          End DFTPL.
LOCAL END
      PROGRAM DFTPL
C-----------------------------------------------------------------------
C! Plots summed uv data for a position in the sky as a function of time.
C# UV Plot-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000-2002, 2008-2009, 2012, 2014-2017
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
C   DFTPL creates a 'PL' extension file for display of the DFT of the
C   visibilities for an arbitrary position in the sky. Plots resulting
C   flux as a function of time. The data may be binned with arbitrary
C   averages in time.
C
C   NOTE 1: DFTPL wants the first key of the sort order of the UV data
C           base to be TIME.
C
C   NOTE 2: At present, DFTPL will only plot up to 10000 bins. For 5 s
C           averages this means roughly 17 minutes of plotting. For 1
C           min averages, more than 3 hrs, etc.
C
C   Inputs:
C     USERID                       UV data file owner # ignored
C     INNAME         NAMEIN        Name of input UV data.
C     INCLASS        CLAIN         Class of input UV data.
C     INSEQ          SEQIN         Seq. of input UV data.
C     INDISK         DISKIN        Disk number of input VU data.
C     UVRANGE....Range of UV projected spacings to include (Klambda)
C     TIMER......Selection parameters:
C        1 = Start IAT day (day 0 = first day in data base)
C        2 = Start IAT hour
C        3 = Start IAT minute
C        4 = Start IAT second
C        5 = Stop IAT day (day 0 = first day in data base)
C        6 = Stop IAT hour
C        7 = Stop IAT minute
C        8 = Stop IAT second
C        9 = Offset in right ascension (asec)
C       10 = Offset in declination (asec)
C     BPARM......Control parameters:
C        1 = 1 then plot RR data only
C            2      plot LL data only
C            3      plot RL data only
C            4      plot LR data only
C            5      plot IPOL
C            6      plot VPOL
C            7      plot QPOL
C            8      plot UPOL
C        2 = averaging interval in sec
C        3 = Do not autoscale if > zero, use following values:
C        4 = Minimum of X-axis,
C        5 = Maximum of X-axis,
C        6 = Minimum of Y-axis.
C        7 = Maximum of Y-axis,
C        8 = X to Y ratio
C        9 = 1 => write time, flux density, and error to message file
C                 but don't plot error bars
C            0 => do not write to message file or plot error bars
C           -1 => write stuff to message file AND plot error bars
C   DOCIRCLE   DOGRID      Draw ticks all the way across ?
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C
C   Written by T. Bastian, Univ. of Colorado, Aug. 1986
C   Used UVPLT as task template.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IERR, IRET
      INCLUDE 'DFTPL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'DFTPL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL FTPIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Do DFT's
      CALL ACDFT (IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Plot 'em up
      CALL PLTFT (IRET)
C                                       Clear catlg on error
      IF ((IRET.NE.0) .AND. (NCFILE.GE.1)) THEN
         CALL DELEXT ('PL', FVOL(1), FCNO(1), 'READ', CATBLK, BUFF1,
     *      VER, IERR)
         CALL ZCLOSE (LUNI, INDI, IERR)
         NCFILE = NCFILE - 1
         END IF
C                                       Close down
 995  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE FTPIN (PRGM, JERR)
C-----------------------------------------------------------------------
C   FTPIN gets input parameters for DFTPL .
C   Inputs:
C      PRGM    C*6    Program name
C   Output:
C      JERR    I      Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   JERR
C
      INCLUDE 'DFTPL.INC'
      CHARACTER BNDCOD(MAXIF)*8, CSTOK(8)*4
      INTEGER   OLDCNO, IUSER, I, IERR, IROUND, FQVER, NIF, CHBUFF(512),
     *   ISBAND(MAXIF), LUNCH
      REAL      CATR(256), RPARM(20)
      LOGICAL   T
      DOUBLE PRECISION RA0, DEC0, CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA T /.TRUE./
      DATA CSTOK /'RR', 'LL', 'I', 'V', 'Q', 'U', 'XX', 'YY'/
C-----------------------------------------------------------------------
C                                       Init IO et al.
      CALL ZDCHIN (T, BUFF1)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
      VER = 10000
C                                       Get input parameters.
      NPARMS = 177
      CALL GTPARM (PRGM, NPARMS, RQUICK, USERID, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         JERR = 8
         RQUICK = .TRUE.
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR, 'GETTING USER ADVERB VALUES'
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      DOGRID = DOCIRC.GT.0.0
      USERID = NLUSER
      IUSER = NLUSER
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      TBEG = TIMER(1) + (TIMER(2)+(TIMER(3)+TIMER(4)/60.)/60.)/24.
      TFIN = TIMER(5) + (TIMER(6)+(TIMER(7)+TIMER(8)/60.)/60.)/24.
      IF (TFIN.LE.TBEG) TFIN = 1.E6
      IF (TBEG.LE.0.0) TBEG = -1.E6
      DOTV = XDOTV.GT.0.0
      GRCHN = IROUND (XGRCH)
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
      LABEL = IROUND (XLABEL)
      LTYPE = MOD (ABS(LABEL), 100)
      IF ((LTYPE.EQ.0) .OR. (LTYPE.GT.10)) LTYPE = 3
      IF (LTYPE.GT.7) LTYPE = 7
      IF ((LTYPE.GE.4) .AND. (LTYPE.LE.6)) LTYPE = 3
      IF (LABEL.LT.0) THEN
         LABEL = (LABEL/100)*100 + LTYPE
      ELSE
         LABEL = (LABEL/100)*100 - LTYPE
         END IF
      XLABEL = LABEL
      IPLOT = 1
      IF (ABS(BPARM(9)).GT.1.5) IPLOT = 2
      IF (ABS(BPARM(9)).GT.2.5) IPLOT = 3
      IF ((BPARM(10).LT.0.2) .OR. (BPARM(10).GT.10.0)) BPARM(10) = 1.
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XSTOK, STOKES)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOACOR = .FALSE.
      CALL RCOPY (8, TIMER, TIMRNG)
      DOPOL = IROUND(XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      DOAPPL = .FALSE.
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LT.0) SUBARR = 0
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      BCHAN = XBCHAN
      ECHAN = XECHAN
      BIF = XBIF
      EIF = XEIF
C                                       Test UV range
      NOUVR = .FALSE.
      IF ((UVRANG(1).GE.UVRANG(2)) .OR. (UVRANG(1).LT.0.0)) THEN
         NOUVR = .TRUE.
         UVRANG(1) = 0.0
         UVRANG(2) = 1.E10
         END IF
      UVRNG(1) = UVRANG(1)
      UVRNG(2) = UVRANG(2)
      UVRANG(1) = UVRANG(1) * 1.0E3
      UVRANG(2) = UVRANG(2) * 1.0E3
C                                       Set control parms
      XYSCL(1) = -1.0E10
      XYSCL(2) = XYSCL(1)
      XYOFF(1) = 1.E10
      XYOFF(2) = XYOFF(1)
C                                       Autoscale ?
      SCALEM(1) = (BPARM(3).LE.0.0) .OR. (BPARM(4).EQ.BPARM(5))
      SCALEM(2) = (BPARM(3).LE.0.0) .OR. (BPARM(6).EQ.BPARM(7))
      TESTEM(1) = 1
      IF (BPARM(4).GT.BPARM(5)) TESTEM(1) = -1
      IF ((BPARM(3).EQ.0.0) .OR. (BPARM(4).EQ.BPARM(5))) TESTEM(1) = 0
      TESTEM(2) = 1
      IF (BPARM(6).GT.BPARM(7)) TESTEM(2) = -1
      IF ((BPARM(3).EQ.0.0) .OR. (BPARM(6).EQ.BPARM(7))) TESTEM(2) = 0
C                                       X/Y ratio ?
      AREA(1) = 1000.
      AREA(2) = 1000.
      IF (BPARM(8).EQ.0.0) BPARM(8) = 1.0
      IF (BPARM(8).GT.1.0) AREA(2) = AREA(2) / BPARM(8)
      IF (BPARM(8).LT.1.0) AREA(1) = AREA(1) * BPARM(8)
C                                       Get CATBLK from UVGET
      CALL UVGET ('INIT', RPARM, BUFF1, IERR)
      IF (IERR.LT.0) THEN
         MSGTXT = 'INITIAL UVGET RETURNS NO DATA FOUND'
         GO TO 980
      ELSE IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INITIAL UVGET CALL'
         GO TO 980
         END IF
C                                       save adverbs
      XSIN = IUSEQ
      XDISIN = IUDISK
      CALL CHR2H (12, UNAME, 1, XNAMEI)
      CALL CHR2H (6, UCLAS, 1, XCLAIN)
      OLDCNO = IUCNO
      XBCHAN = BCHAN
      XECHAN = ECHAN
      XBIF = BIF
      XEIF = EIF
      XSUBA = SUBARR
      CALL UVGET ('CLOS', RPARM, BUFF1, IERR)
      IF (NCFILE.LE.0) THEN
         NCFILE = 1
         FVOL(NCFILE) = IUDISK
         FCNO(NCFILE) = IUCNO
         FRW(NCFILE) = 0
         END IF
      CALL COPY (256, CATUV, CATBLK)
C                                       UVPGET was called for output
      IF ((ILOCSU.GE.0) .OR. ((RA.EQ.0.0D0) .AND. (DEC.EQ.0.0D0))) THEN
         IERR = 8
         MSGTXT = 'YOU MUST SELECT ONLY ONE SOURCE'
         GO TO 980
         END IF
C                                       Source offsets
      RA0 = RA
      DEC0 = DEC
      IF (COS(DG2RAD*DEC0).NE.0.0D0) RA = RA0 + SHIFT(1) / 3600.D0
     *   / COS(DG2RAD * DEC0)
      DEC = DEC + SHIFT(2) / 3600.D0
      RAS = RA
      DECS = DEC
      DXC = SIN (DG2RAD * (RA-RA0)) * COS (DEC * DG2RAD)
      DYC = COS (DEC0 * DG2RAD) * SIN (DEC * DG2RAD) -
     *   SIN (DEC0 * DG2RAD) * COS (DEC * DG2RAD) *
     *   COS ((RA - RA0) * DG2RAD)
      DZC = SIN (DG2RAD * DEC0) * SIN (DG2RAD * DEC) +
     *   COS (DG2RAD * DEC0) * COS (DG2RAD * DEC) *
     *   COS (DG2RAD * (RA - RA0)) - 1.0D0
      DXC = TWOPI * DXC
      DYC = TWOPI * DYC
      DZC = TWOPI * DZC
C                                       Sort order OK ?
      IF (ISORT(:1).NE.'T') THEN
         MSGTXT = 'FIRST KEY OF SORT ORDER MUST BE TIME !!'
         CALL MSGWRT (6)
         JERR = 1
         GO TO 999
         END IF
C                                       Frequency and bandwidth
      IF (JLOCIF.LT.0) THEN
         FOFF(1) = 0.0D0
         FINC(1) = CATR(KRCIC+JLOCF)
      ELSE
         FQVER = 1
         LUNCH = 87
         CALL CHNDAT ('READ',  CHBUFF, DISKIN, OLDCNO, FQVER, CATBLK,
     *      LUNCH, NIF, FOFF, ISBAND, FINC, BNDCOD, FREQID, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            CALL MSGWRT (7)
            JERR = 1
            GO TO 999
            END IF
         END IF
      OFREQ = CATD(KDCRV+JLOCF)
      FRPIX = CATR(KRCRP+JLOCF)
C                                       Update catalog header.
      FRW(NCFILE) = 0
      JERR = 0
C                                       Check correlator display
      DO 20 I = 1,8
         IF (STOKES.EQ.CSTOK(I)) THEN
            BPARM(1) = I
            GO TO 999
            END IF
 20      CONTINUE
C                                       Stokes unavailable
      WRITE (MSGTXT,1900) STOKES
      JERR = 1
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FTPIN: ERROR',I3,' ON ',A)
 1050 FORMAT ('ERROR',I5,' READING FREQUENCIES WITH CHNDAT')
 1900 FORMAT ('REQUESTED STOKES PARAMETER ''',A,''' NOT ALLOWED')
      END
      SUBROUTINE ACDFT (IRET)
C-----------------------------------------------------------------------
C   ACDFT accumlates the flux density for each averaging interval as
C   well as an estimate of the error. Also sets scaling for later use.
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'DFTPL.INC'
      INTEGER   FLAG, NUMVIS, XUMVIS
      REAL      XY(2), Z, VIS(UVBFSS), RPARM(20)
      DOUBLE PRECISION T1, T2
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Set up binning
      XNUL = FBLANK
      CALL RFILL (10000, XNUL, DFT)
      CALL RFILL (10000, 0.0, ERROR)
C                                       Get start/stop times
      CALL TBTIME (TBEG, TFIN, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINDING START AND STOP TIMES'
         GO TO 990
         END IF
C                                       set bin count, T averaging
      BCNT = 1
      IF (BPARM(2).LE.0.001) BPARM(2) = 864. * (TFIN - TBEG)
      TAVG = BPARM(2) / 86400.
      T1 = TBEG
      T2 = T1 + TAVG
C                                       Init vis file for read.
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT UV IO'
         GO TO 990
         END IF
      NUMVIS = 0
      XUMVIS = 0
      KNCS = INCS
      KNCF = INCF
      KNCIF = INCIF
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING UV DATA'
         GO TO 990
      ELSE IF (IRET.EQ.0) THEN
C                                       Is this a valid point ?
 125     CALL WANTED (RPARM, VIS, T1, T2, FLAG)
C                                       Bad point, try again
         IF (FLAG.EQ.1) GO TO 100
C                                       End of time search
         IF (FLAG.EQ.3) GO TO 200
C                                       Next time interval
         IF (FLAG.EQ.2) THEN
            CALL DODFT (RPARM, VIS, T1, T2, XY, Z, FLAG, IRET)
            IF (IRET.EQ.0) THEN
               CALL XYSCAL (NUMVIS, XY, Z, IRET)
               IF (IRET.LT.0) THEN
                  BCNT = BCNT - 1
                  DFT(BCNT) = XNUL
                  END IF
               END IF
            T1 = T2
            T2 = T1 + TAVG
            GO TO 125
            END IF
C                                       Good point
         CALL DODFT (RPARM, VIS, T1, T2, XY, Z, FLAG, IRET)
         XUMVIS = XUMVIS + 1
         GO TO 100
         END IF
C                                       Any valid points
 200  IF (XUMVIS.LE.1) THEN
         IRET = 4
         WRITE (MSGTXT,1200) XUMVIS
         GO TO 990
         END IF
C                                       Final call to XYSCL.
      NUMVIS = -1
      CALL XYSCAL (NUMVIS, XY, Z, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1110) IRET
         GO TO 990
         END IF
C                                       close UV data
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ACDFT: ERROR',I3,' ON ',A)
 1110 FORMAT ('ACDFT: XYSCL ERROR',I3)
 1200 FORMAT ('FOUND',I5,' POINTS: NOT ENOUGH TO SELF-SCALE')
      END
      SUBROUTINE PLTFT (IRET)
C-----------------------------------------------------------------------
C   PLTFT actually plots the data.
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C                     1 => failed to add to catalog
C                     2 => failed to create
C                     3 => graph file write error
C                     4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER TEXT*80, PFILE*48, CHTYPE(2)*8, TSIGN*1, AUNITS(2)*8,
     *   NAMCL*18, TIME*8, DATE*12, TS*1, CHR*1, CHD*1
      INTEGER   BUFFER(256), IERR, ITYPE, IPSIZE, I, INCHAR, LUNPL,
     *   FINDPL, ITIM(8), INP, IBIN, NGOOD, NNOFIT, IT(3), ID(3),
     *   ITT(4), HMR(2), HMD(2)
      REAL      BLC(2), TRC(2), CHOUT(4), XYRATO, DX, DY, TR, TI, XY(2),
     *   STDEV, XXFREQ, SECR, SECD
      LOGICAL   GOOD, NOCHK, DOHST
      INCLUDE 'DFTPL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUNPL /26/
      DATA AUNITS /'IAT hrs ', 'Janskys '/
      DATA CHTYPE /'Time    ', 'DFT     '/
C-----------------------------------------------------------------------
      NGOOD = 0
      NNOFIT = 0
      IRET = 1
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', DISKIN, FCNO(1), CATUV, BUFF1, .TRUE.,
     *      'READ', VER, IRET)
         IF (IRET.NE.0) THEN
            NCFILE = NCFILE - 1
            GO TO 999
            END IF
         END IF
C                                       See if data range check
      NOCHK = (BPARM(3).GT.0.)
C                                       User sets the scales
      DO 15 I = 1,2
         IF (.NOT.SCALEM(I)) THEN
            XYSCL(I) = BPARM(3+2*I)
            XYOFF(I) = BPARM(2+2*I)
            IF (XYSCL(I).EQ.XYOFF(I)) GO TO 999
            XYSCL(I) = AREA(I) / (XYSCL(I)-XYOFF(I))
            END IF
 15      CONTINUE
C                                       Fill in last of actual parms
      BPARM(5) = AREA(1)/XYSCL(1) + XYOFF(1)
      BPARM(7) = AREA(2)/XYSCL(2) + XYOFF(2)
      BPARM(4) = XYOFF(1)
      BPARM(6) = XYOFF(2)
      DOHST = IPLOT.EQ.3
      CALL RFILL (8, 0.0, TIMER)
      TIMER(1) = TBEG
      TIMER(5) = TFIN
C                                       Create plot file
      CALL ZPHFIL ('PL', DISKIN, FCNO(1), VER, PFILE, IERR)
      IF (IERR.NE.0) GO TO 999
      IPSIZE = 0
      ITYPE = 23
      CALL GINIT (DISKIN, FCNO(1), PFILE, IPSIZE, ITYPE, NPARMS, USERID,
     *   DOTV, TVCHN, GRCHN, TVCORN, CATUV, BUFFER, LUNPL, FINDPL,
     *   IERR)
      IRET = 2
      IF (IERR.NE.0) GO TO 999
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = AREA(1)
      TRC(2) = AREA(2)
      XYRATO = BPARM(8)
      IRET = 3
      CALL FILL (5, 1, ITIM)
C                                       Set up location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 7
      AXTYP(LOCNUM) = 0
      DO 30 I = 1,2
         TR = AREA(I) / XYSCL(I)
         RPLOC(I,LOCNUM) = BLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I)
         AXINC(I,LOCNUM) = TR / (TRC(I) - BLC(I))
         IF (I.EQ.2) THEN
            TR = MAX (ABS(TR), ABS(XYOFF(2)), ABS(TR+XYOFF(2)))
            TI = TR
            CALL METSCL (LABEL, TR, CPREF(I,LOCNUM), GOOD)
            RPVAL(I,LOCNUM) = RPVAL(I,LOCNUM) * TR / TI
            AXINC(I,LOCNUM) = AXINC(I,LOCNUM) * TR / TI
         ELSE
            CPREF(I,LOCNUM) = ' '
            RPVAL(I,LOCNUM) = RPVAL(I,LOCNUM) * 360.
            AXINC(I,LOCNUM) = AXINC(I,LOCNUM) * 360.
            END IF
         CTYP(I,LOCNUM) = AUNITS(I)
 30      CONTINUE
C                                       number characters around
      CALL RFILL (4, 0.5, CHOUT)
      CALL CHNTIC (BLC, TRC, INP)
      IF (LTYPE.EQ.2) CHOUT(1) = 2.5
      IF (LTYPE.GT.2) CHOUT(1) = INP + 4.0
      IF (LTYPE.GT.1) CHOUT(2) = 2.0
      IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         CHOUT(4) = 3.333
         IF (LABEL.GT.1) CHOUT(4) = CHOUT(4) + 1.333
         IF (.NOT.NOUVR) CHOUT(4) = CHOUT(4) + 1.333
         IF ((TBEG.GE.-1.E5) .OR. (TFIN.LE.1.0E5)) CHOUT(4) =
     *      CHOUT(4) + 1.333
         END IF
C                                       Init for line drawing.
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, ITIM, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       Draw border
      CALL GLTYPE (1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Top labels: version number
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         DX = 0.0
         DY = CHOUT(4) - 1.5
         IF (LABEL.GT.1) THEN
            CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, TIME, DATE)
            WRITE (TEXT,1030) VER, DATE, TIME
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            DY = DY - 1.333
            END IF
C                                       Top labels: type & name
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         TEXT = CHTYPE(2) // ' vs ' // CHTYPE(1) // ' for '
         NAMCL = NAMEIN
         NAMCL(13:18) = CLAIN
         CALL NAMEST (NAMCL, SEQIN, TEXT(26:), INCHAR)
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         DY = DY - 1.333
C                                       Time range, ra, dec
         IF ((TBEG.GE.-1.E5) .OR. (TFIN.LE.1.0E5)) THEN
            CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL COORDD (1, RAS, CHR, HMR, SECR)
            CALL COORDD (2, DECS, CHD, HMD, SECD)
            CALL T2DHMS (TBEG, TS, ITIM(1))
            CALL T2DHMS (TFIN, TS, ITIM(5))
            ITIM(1) = MIN (ITIM(1), 999)
            ITIM(5) = MIN (ITIM(5), 999)
            WRITE (TEXT,1035) (ITIM(I), I = 1,8), HMR, SECR, CHD, HMD,
     *         SECD
            IF (TEXT(46:46).EQ.' ') TEXT(46:46) = '0'
            IF (TEXT(64:64).EQ.' ') TEXT(64:64) = '0'
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            DY = DY - 1.333
            END IF
C                                       UV range
         IF (.NOT.NOUVR) THEN
            CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            WRITE (TEXT,1040) UVRANG
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            DY = DY - 1.333
            END IF
C                                       Frequency, etc.
         XXFREQ = (OFREQ + FOFF(BIF) + ((ECHAN+BCHAN+1.)/2. - FRPIX) *
     *      FINC(BIF)) / 1.E9
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         WRITE (TEXT,1045) STOKES, BCHAN, ECHAN, BIF, EIF, XXFREQ
         IF (ECHAN.EQ.BCHAN) TEXT(25:32) = ' '
         IF (EIF.EQ.BIF) TEXT(40:44) = ' '
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Put on labels and ticks
      XYRATO = 1.0
      CALL CLAB1 (BLC, TRC, CHOUT, LABEL, XYRATO, DOGRID, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Plot binned data.
      IF (.NOT.DOHST) THEN
         CALL GLTYPE (4, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         DX = 3.0 * BPARM(10)
         DO 110 IBIN = 1,BCNT
C                                       Points ...
            IF (DFT(IBIN).NE.XNUL) THEN
               NGOOD = NGOOD + 1
               DY = ERROR(IBIN) * XYSCL(2)
               IF (IPLOT.EQ.1) DY = 3.0 * BPARM(10)
               XY(1) = (TIMR(IBIN) - XYOFF(1))*XYSCL(1)
               XY(2) = (DFT(IBIN) - XYOFF(2))*XYSCL(2)
               IF ((XY(2).GE.BLC(2)) .AND. (XY(2).LE.TRC(2))) THEN
                  CALL GPOS (XY(1)+DX, XY(2), BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 105
                  CALL GVEC (XY(1)-DX, XY(2), BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 105
                  CALL GPOS (XY(1), XY(2)+DY, BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 105
                  CALL GVEC (XY(1), XY(2)-DY, BUFFER, IRET)
                  END IF
C                                       Write binned values
 105           IF (PRTLEV.GT.0.0) THEN
                  CALL T2DHMS (TIMR(IBIN), TSIGN, ITT)
                  XY(2) = XY(2) / XYSCL(2) + XYOFF(2)
                  STDEV = ERROR(IBIN)
                  WRITE (MSGTXT,1200) IBIN, ITT, XY(2), STDEV
                  CALL MSGWRT (4)
                  IF (IRET.NE.0) GO TO 970
                  END IF
               END IF
 110        CONTINUE
C                                       Histogram mode ...
      ELSE
         DX = TAVG*XYSCL(1)/2.0
         CALL GLTYPE (2, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         DO 210 IBIN = 1,BCNT
            IF (DFT(IBIN).NE.XNUL) THEN
               NGOOD = NGOOD + 1
               XY(1) = (TIMR(IBIN) - XYOFF(1))*XYSCL(1)
               XY(2) = (DFT(IBIN) - XYOFF(2))*XYSCL(2)
               DY = 0.0
               IF (DFT(IBIN+1).NE.XNUL) DY =
     *            (DFT(IBIN+1)-DFT(IBIN))*XYSCL(2)
               IF (((XY(2)+DY).GT.AREA(2)) .OR.
     *            ((XY(2)+DY).LT.0.0)) DY = 0.0
               IF ((XY(2).GE.BLC(2)) .AND. (XY(2).LE.TRC(2))) THEN
                  CALL GPOS (XY(1)-DX, XY(2), BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 205
                  CALL GVEC (XY(1)+DX, XY(2), BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 205
                  CALL GVEC (XY(1)+DX, XY(2)+DY, BUFFER, IRET)
                  END IF
C                                       Write binned values
 205           IF (PRTLEV.GT.0.0) THEN
                  CALL T2DHMS (TIMR(IBIN), TSIGN, ITT)
                  XY(2) = XY(2) / XYSCL(2) + XYOFF(2)
                  STDEV = ERROR(IBIN)
                  WRITE (MSGTXT,1200) IBIN, ITT, XY(2), STDEV
                  CALL MSGWRT (6)
                  END IF
               END IF
 210        CONTINUE
         END IF
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.NE.0) GO TO 975
         IRET = 0
         GO TO 990
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 990
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKIN, PFILE, IERR)
         END IF
      GO TO 999
C                                       No catalog update
C                                       Messages
 990  WRITE (MSGTXT,1990) NGOOD
      CALL MSGWRT (2)
      WRITE (MSGTXT,1991) NNOFIT
      IF (NNOFIT.GE.1.0D0) CALL MSGWRT (2)
      IF (.NOT.DOTV) THEN
         CALL HIPLOT (DISKIN, FCNO(1), VER, BUFFER, IERR)
         WRITE (MSGTXT,1992) VER
         CALL MSGWRT (2)
         END IF
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('Plot file version',I4,'__created ',A12,A8)
 1035 FORMAT ('From',I3,'/',2(I2.2,':'),I2.2,' _to',I4,'/',2(I2.2,':'),
     *   I2.2,' __RA',I3.2,':',I2.2,':',F5.2,' _DEC ',A1,2(I2.2,':'),
     *   F4.1)
 1040 FORMAT ('UVrange ',2(1PE12.4),' wavelengths')
 1045 FORMAT ('Corr ',A,' _Channel',I6,' -',I6,' _IF',I3,' -',I3,
     *   ' _Freq =',F8.4,' GHz')
 1200 FORMAT (I5,' T=',I2,'/',2(I2.2,':'),I2.2,', S=',1PE12.5,
     *   ', RMS=',1PE12.5)
 1970 FORMAT ('PLTFT: ERROR DURING GRAPHING. WILL TRY TO FINISH',
     *   ' PARTIAL GRAPH')
 1990 FORMAT ('PLTFT: ',I10,' Points plotted')
 1991 FORMAT ('PLTFT: ',I10,' Points did not fit')
 1992 FORMAT ('PLTFT: Plot file version',I5,'  created.')
      END
      SUBROUTINE XYSCAL (NUMVIS, XY, Z, IRET)
C-----------------------------------------------------------------------
C   XYSCAL finds the scaling parameters needed to fit X and Y
C   into a 1000*1000 plotting area .
C   Inputs:
C      NUMVIS     I    Visibility number, -1 => final call, no data
C                      passed -> change to scaling factor from max/min
C      XY         R    plotted parameters .
C      Z          R    error on y parm.
C   Outputs:
C      XYOFF      R    when added to XY changes minimum to zero .
C      XYSCL      R    scale XY so that maximum is 1000.
C      IRET       I    Error return code , non-zero if error .
C-----------------------------------------------------------------------
      INTEGER   NUMVIS
      REAL      XY(2), Z, TEMP
      INTEGER   IRET, I, JJ
      INCLUDE 'DFTPL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Are they in requested range
      IF (NUMVIS.GE.0) THEN
         IRET = -1
         DO 10 I = 1,2
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
         IF (SCALEM(1)) THEN
            IF (XY(1).LT.XYOFF(1)) XYOFF(1) = XY(1)
            IF (XY(1).GT.XYSCL(1)) XYSCL(1) = XY(1)
            END IF
         IF (SCALEM(2)) THEN
            IF ((XY(2)-Z).LT.XYOFF(2)) XYOFF(2) = XY(2) - Z
            IF ((XY(2)+Z).GT.XYSCL(2)) XYSCL(2) = XY(2) + Z
            END IF
C                                       Convert to scaling factors
C                                       provide room at edges too.
      ELSE
         DO 120 I = 1,2
            IF (SCALEM(I)) THEN
               IF (XYSCL(I).LE.XYOFF(I)) GO TO 980
               TEMP = 0.01 * (XYSCL(I) - XYOFF(I))
               XYSCL(I) = XYSCL(I) + TEMP
               XYOFF(I) = XYOFF(I) - TEMP
               IF ((XYOFF(I).GT.0.0) .AND. (XYOFF(I).LT.0.15*XYSCL(I)))
     *            XYOFF(I) = 0.0
               XYSCL(I) = AREA(I)/(XYSCL(I) - XYOFF(I))
               END IF
 120        CONTINUE
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
      SUBROUTINE WANTED (RPBUF, VIS, T1, T2, FLAG)
C-----------------------------------------------------------------------
C   WANTED determines whether the current visibility sample is valid
C   and selected via the selection parameters.
C   Inputs:
C      RPBUF   R(*)   Random parameters
C      VIS     R(*)   Visibilities
C      T1      D      Start desired time range
C      T2      D      End desired time range
C   Outputs:
C      FLAG    I      0 => data selected as good
C                     1    data NO GOOD
C                     2    time exceeds T2
C                     3    time exceeds TFIN
C-----------------------------------------------------------------------
      REAL      RPBUF(*), VIS(*)
      DOUBLE PRECISION T1, T2
      INTEGER   FLAG
C
      REAL      TEMP
      INTEGER   LAD, IIF, ICH, IROUND
      LOGICAL   GOOD, ANY
      INCLUDE 'DFTPL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      FLAG = 1
C                                       Check FREQID
      IF (ILOCFQ.GE.0) THEN
         IIF = IROUND (RPBUF(1+ILOCFQ))
         IF ((FREQID.GT.0) .AND. (IIF.GT.0) .AND. (IIF.NE.FREQID))
     *      GO TO 999
         END IF
C                                       Check UV range
      IF (.NOT.NOUVR) THEN
         TEMP = SQRT (RPBUF(1+ILOCU)**2 + RPBUF(1+ILOCV)**2)
         IF ((TEMP.LT.UVRANG(1)) .OR. (TEMP.GT.UVRANG(2))) GO TO 999
         END IF
C                                       Are data flagged?
      ANY = .FALSE.
      DO 20 IIF = BIF,EIF
         DO 10 ICH = BCHAN,ECHAN
            LAD = 1 + (IIF-BIF)*KNCIF + (ICH-BCHAN)*KNCF
            GOOD = VIS(LAD+2).GT.0.0
            ANY = ANY .OR. GOOD
 10         CONTINUE
 20      CONTINUE
      IF (.NOT.ANY) GO TO 999
C                                       Test time range
      TEMP = RPBUF(1+ILOCT)
      IF (TEMP.LT.T1) GO TO 999
      FLAG = 2
      IF (TEMP.GE.T2) GO TO 999
      FLAG = 3
      IF (TEMP.GT.TFIN) GO TO 999
      FLAG = 0
C
 999  RETURN
      END
      SUBROUTINE DODFT (RPBUF, VIS, T1, T2, XY, Z, FLAG, IRET)
C-----------------------------------------------------------------------
C   DODFT computes the DFT for a given set of visibilities with the
C   proper sky offset applied.
C   Inputs:
C     RPBUF   R(*)   one visibility record - random parameters
C     VIS     R(*)   one visibility record - data
C     T1      D      Start of interval
C     T2      D      End of interval
C     FLAG    I      If 0, continue summing DFT
C                    If 2, wrap it up, clear
C   Outputs:
C     XY      R(2)   X, Y values
C     Z       R      Error on Y
C     IRET    I      0 => operation sucessful
C                    1 => trouble
C                    -1 => no data to average
C-----------------------------------------------------------------------
      REAL      RPBUF(*), VIS(*), XY(2), Z, WT, DELS
      INTEGER   FLAG, IRET
C
      REAL      TR, TI, UU, VV, WW, SMN, TNU, TNU1, TNU2, TUMWT, XX
      DOUBLE PRECISION T1, T2, AFREQ
      INTEGER   IIF, ICH, TAMP, LAD
      INCLUDE 'DFTPL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE TNU, TNU1, TNU2, TUMWT, TAMP
      DATA TNU, TNU1, TNU2, TUMWT, TAMP /4*0.0, 0/
C-----------------------------------------------------------------------
      TR = 0.0
      TI = 0.0
      IRET = 0
      IF (FLAG.NE.0) GO TO 500
C                                       loop over IF and channel
      DO 100 IIF = BIF,EIF
         DO 90 ICH = BCHAN,ECHAN
            LAD = 1 + (ICH-BCHAN)*KNCF + (IIF-BIF)*KNCIF
            AFREQ = OFREQ + FOFF(IIF) + (ICH-FRPIX) * FINC(IIF)
C                                       Set U and V
            UU = RPBUF(ILOCU+1) * AFREQ / OFREQ
            VV = RPBUF(ILOCV+1) * AFREQ / OFREQ
            WW = RPBUF(ILOCW+1) * AFREQ / OFREQ
C                                       Find visibilities and weights
            IF (VIS(LAD+2).GT.0.0) THEN
               TR = VIS(LAD)
               TI = VIS(LAD+1)
               WT = VIS(LAD+2)
C                                       sum it up
               XX = UU * DXC + VV * DYC + WW * DZC
               TNU = TR * COS (XX) + TI * SIN (XX)
               TNU1 = TNU1 + WT * TNU
               TNU2 = TNU2 + WT * TNU * TNU
               TUMWT = TUMWT + WT
               TAMP = TAMP + 1
               END IF
 90         CONTINUE
 100     CONTINUE
      GO TO 999
C                                      Finish up this interval
 500  IF (TUMWT.GT.0.0) THEN
         SMN = TNU1 / TUMWT
         DELS = TNU2/TUMWT - SMN*SMN
         DELS = SQRT (MAX (0.0, DELS))
         DFT(BCNT) = SMN
         TIMR(BCNT) = (T1+T2)/2.
         ERROR(BCNT) = DELS / SQRT (MAX (1.0, TAMP-1.))
         XY(1) = TIMR(BCNT)
         XY(2) = DFT(BCNT)
         Z = 0.0
         IF (IPLOT.EQ.2) Z = ERROR(BCNT)
         IRET = 0
         BCNT = BCNT + 1
      ELSE
         IRET = -1
         END IF
      DELS = 0.0
      TNU = 0.0
      TNU1 = 0.0
      TNU2 = 0.0
      TUMWT = 0.0
      TAMP = 0
C
 999  RETURN
      END
