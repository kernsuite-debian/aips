LOCAL INCLUDE 'DFTIM.INC'
C                                       Local include for DFTIM
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IMGLIM
      PARAMETER (IMGLIM=10000)
C
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMEO(3), XCLAOU(2), XSOUR(4,30),
     *   XCALC, XSTOK
      REAL      XSIN, XDISIN, XSOU, XDISOU, UVRANG(2), TIMER(8),
     *   SHIFT(2), BPARM(10), XQUAL, XBAND, XFREQ, XFQID, XSUBA, XBIF,
     *   XEIF, XBCHAN, XECHAN, XCHAVG, XDOCAL, XGUSE, XDOPOL, XPDVER,
     *   XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), BADD(10)
      DOUBLE PRECISION FOFF(MAXIF), DXC, DYC, DZC, RAS, DECS, OFREQ,
     *   FREQS(16384)
      REAL      BUFF1(UVBFSS), TIMR(IMGLIM), TBEG, TFIN, FRPIX, TAVG,
     *   FINC(MAXIF)
      CHARACTER NAMEIN*12, CLAIN*6, NAMEOU*12, CLAOU*6
      INTEGER   SEQIN, DISKIN, SEQOU, DISKOU, LUNI, INDI, JBUFSZ,
     *   NPARMS, NCHAVG, FREQID, KNCS, KNCF, KNCIF, NY, CATOLD(256)
      LOGICAL   NOUVR
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMEO, XCLAOU,
     *   XSOU, XDISOU, UVRANG, TIMER, SHIFT, XSTOK, BPARM, XSOUR, XQUAL,
     *   XCALC, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XBCHAN, XECHAN,
     *   XCHAVG, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH, BADD
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /FTPCOM/ FOFF, CATOLD, FREQS, OFREQ, DXC, DYC, DZC, RAS,
     *   DECS, FINC, TIMR, TBEG, TFIN, TAVG, FRPIX, NOUVR, SEQIN,
     *   DISKIN, LUNI, INDI, NPARMS, FREQID, KNCS, KNCF, KNCIF, SEQOU,
     *   DISKOU, NY, NCHAVG
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMEOU, CLAOU
C                                                          End DFTIM.
LOCAL END
      PROGRAM DFTIM
C-----------------------------------------------------------------------
C! images summed uv data for a position in the sky as a function of time
C# UV Analysis
C-----------------------------------------------------------------------
C;  Copyright (C) 2017-2018
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
C   DFTIM creates an image file to display the DFT of the visibilities
C   for an arbitrary position in the sky.  Plots resulting flux on
C   frequency and time axes.
C   NOTE 1: DFTIM wants the first key of the sort order of the UV data
C           base to be TIME.
C   NOTE 2: At present, DFTIM will only plot up to IMGLIM bins. For 5 s
C           averages this means roughly 14 hours of imaging. For 1
C           min averages, more than 7 days, etc.
C   Inputs:
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
C
C   Written by T. Bastian, Univ. of Colorado, Aug. 1986
C   Used UVPLT as task template.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   NX, NWORDS, IRET
      REAL      IMAGE(2), ERROR(2), WEIGHT(2)
      LONGINT   PIMAGE, PERROR, PWEIGH
      INCLUDE 'DFTIM.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'DFTIM '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL DFTIMI (PRGM, NX, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       allocate image
      NWORDS = (NX * IMGLIM - 1) / 1024 + 4
      CALL ZMEMRY ('GET ', PRGM, NWORDS, IMAGE, PIMAGE, IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', PRGM, NWORDS, ERROR, PERROR,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', PRGM, NWORDS, WEIGHT, PWEIGH,
     *   IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'FAILED TO GET DYNAMIC MEMORY'
         CALL MSGWRT (8)
         GO TO 995
         END IF
C                                       Do DFT's
      CALL DFTIMC (NX, IMAGE(1+PIMAGE), ERROR(1+PERROR),
     *   WEIGHT(1+PWEIGH), IRET)
      IF (IRET.NE.0) GO TO 995
C                                       output
      CALL DFTIMO (NX, IMAGE(1+PIMAGE), ERROR(1+PERROR), IRET)
C                                       Close down
 995  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE DFTIMI (PRGM, NX, JERR)
C-----------------------------------------------------------------------
C   DFTIMI gets input parameters for DFTIM .
C   Inputs:
C      PRGM    C*6    Program name
C   Output:
C      NX      I      Number spectral channels
C      JERR    I      Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   NX, JERR
C
      INCLUDE 'DFTIM.INC'
      CHARACTER BNDCOD(MAXIF)*8, CSTOK(8)*4
      INTEGER   OLDCNO, IUSER, I, IERR, IROUND, FQVER, NIF, CHBUFF(512),
     *   ISBAND(MAXIF), LUNCH, LF, LC, IC
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
C                                       Get input parameters.
      NPARMS = 189
      CALL GTPARM (PRGM, NPARMS, RQUICK, XNAMEI, BUFF1, IERR)
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
      IUSER = NLUSER
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SEQOU = IROUND (XSOU)
      DISKOU = IROUND (XDISOU)
      TBEG = TIMER(1) + (TIMER(2)+(TIMER(3)+TIMER(4)/60.)/60.)/24.
      TFIN = TIMER(5) + (TIMER(6)+(TIMER(7)+TIMER(8)/60.)/60.)/24.
      IF (TFIN.LE.TBEG) TFIN = 1.E6
      IF (TBEG.LE.0.0) TBEG = -1.E6
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMEO, NAMEOU)
      CALL H2CHR (6, 1, XCLAOU, CLAOU)
      CALL H2CHR (4, 1, XSTOK, STOKES)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 5 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 5       CONTINUE
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      NCHAVG = XCHAVG + 0.1
      NCHAVG = MAX (1, NCHAVG)
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
      NCHAVG = MIN (NCHAVG, ECHAN-BCHAN+1)
      NX = (ECHAN - BCHAN + 1) / NCHAVG
      ECHAN = BCHAN - 1 + NX * NCHAVG
      NX = NX * (EIF - BIF + 1)
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
      CALL COPY (256, CATUV, CATOLD)
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
C                                       compute frequency array
      IC = 0
      DO 40 LF = BIF,EIF
         DO 30 LC = BCHAN,ECHAN,NCHAVG
            IC = IC + 1
            FREQS(IC) = FOFF(LF) + FINC(LF) * (LC + (NCHAVG-1.0)/2.0 -
     *         FRPIX)
 30         CONTINUE
 40      CONTINUE
C                                       Update catalog header.
      FRW(NCFILE) = 0
      JERR = 0
C                                       Check correlator display
      DO 50 I = 1,8
         IF (STOKES.EQ.CSTOK(I)) THEN
            BPARM(1) = I
            GO TO 999
            END IF
 50      CONTINUE
C                                       Stokes unavailable
      WRITE (MSGTXT,1900) STOKES
      JERR = 1
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DFTIMI: ERROR',I3,' ON ',A)
 1050 FORMAT ('ERROR',I5,' READING FREQUENCIES WITH CHNDAT')
 1900 FORMAT ('REQUESTED STOKES PARAMETER ''',A,''' NOT ALLOWED')
      END
      SUBROUTINE DFTIMC (NX, IMAGE, ERROR, WEIGHT, IRET)
C-----------------------------------------------------------------------
C   DFTIMC accumlates the flux density for each averaging interval as
C   well as an estimate of the error. Also sets scaling for later use.
C   Inputs:
C      NX      I      X pixels in image
C   Output:
C      IMAGE   R(*)   DFT image
C      ERROR   R(*)   Error estimate
C      IRET    I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NX, IRET
      REAL      IMAGE(NX,*), ERROR(NX,*), WEIGHT(NX,*)
C
      INCLUDE 'DFTIM.INC'
      INTEGER   FLAG, NUMVIS, XUMVIS, I
      REAL      VIS(UVBFSS), RPARM(20)
      DOUBLE PRECISION T1, T2
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Set up binning
      I = NX * IMGLIM
      CALL RFILL (I, 0.0, IMAGE)
      CALL RFILL (I, 0.0, ERROR)
      CALL RFILL (I, 0.0, WEIGHT)
C                                       Get start/stop times
      CALL TBTIME (TBEG, TFIN, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINDING START AND STOP TIMES'
         GO TO 990
         END IF
C                                       set bin count, T averaging
      IF (BPARM(2).LE.0.001) BPARM(2) = 864. * (TFIN - TBEG)
      TAVG = BPARM(2) / 86400.
      T1 = TBEG
      T2 = T1 + TAVG
      NY = 1
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
            TIMR(NY) = (T1+T2)/2.
            T1 = T2
            T2 = T1 + TAVG
            NY = NY + 1
            GO TO 125
            END IF
C                                       Good point
         CALL DODFT (RPARM, VIS, T1, T2, FLAG, NY, NX, IMAGE,
     *      ERROR, WEIGHT, IRET)
         XUMVIS = XUMVIS + 1
         GO TO 100
         END IF
C                                       Any valid points
 200  IF (XUMVIS.LE.1) THEN
         IRET = 4
         WRITE (MSGTXT,1200) XUMVIS
         GO TO 990
         END IF
      FLAG = 2
      CALL DODFT (RPARM, VIS, T1, T2, FLAG, NY, NX, IMAGE, ERROR,
     *   WEIGHT, IRET)
C                                       close UV data
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DFTIMC: ERROR',I3,' ON ',A)
 1200 FORMAT ('FOUND',I5,' POINTS: NOT VERY INTERESTING')
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
      INCLUDE 'DFTIM.INC'
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
      SUBROUTINE DODFT (RPBUF, VIS, T1, T2, FLAG, IY, NX, IMAGE,
     *   ERROR, WEIGHT, IRET)
C-----------------------------------------------------------------------
C   DODFT computes the DFT for a given set of visibilities with the
C   proper sky offset applied.
C   Inputs:
C      RPBUF    R(*)   one visibility record - random parameters
C      VIS      R(*)   one visibility record - data
C      T1       D      Start of interval
C      T2       D      End of interval
C      FLAG     I      If 0, continue summing DFT
C                      If 2, wrap it up, clear
C      IY       I      row number in images
C      NX       I      size of images
C   In/out
C      IMAGE    R(*)   image
C      ERROR    R(*)   error
C      WEIGHT   R(*)   weight
C   Outputs:
C      XY       R(2)   X, Y values
C      Z        R      Error on Y
C      IRET     I      0 => operation sucessful
C                      1 => trouble
C                      -1 => no data to average
C-----------------------------------------------------------------------
      INTEGER   FLAG, IY, NX, IRET
      REAL      RPBUF(*), VIS(*), IMAGE(NX,*), ERROR(NX,*), WEIGHT(NX,*)
      DOUBLE PRECISION T1, T2
C
      REAL      TR, TI, UU, VV, WW, SMN, TNU, XX, WT, DELS
      DOUBLE PRECISION AFREQ
      INTEGER   IIF, ICH, TAMP, LAD, IX, LCH, LY
      INCLUDE 'DFTIM.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       loop over IF and channel
      IF (FLAG.EQ.0) THEN
         IX = 0
         DO 50 IIF = BIF,EIF
            DO 40 ICH = BCHAN,ECHAN,NCHAVG
               IX = IX + 1
               DO 30 LCH = ICH,ICH+NCHAVG-1
                  LAD = 1 + (LCH-BCHAN)*KNCF + (IIF-BIF)*KNCIF
                  AFREQ = OFREQ + FOFF(IIF) + (LCH-FRPIX) * FINC(IIF)
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
                     IMAGE(IX,IY) = IMAGE(IX,IY) + WT * TNU
                     ERROR(IX,IY) = ERROR(IX,IY) + WT * TNU * TNU
                     WEIGHT(IX,IY) = WEIGHT(IX,IY) + WT
                     END IF
 30               CONTINUE
 40            CONTINUE
 50         CONTINUE
C                                      Finish up full image
      ELSE
         IRET = -1
         DO 90 LY = 1,IY
            DO 80 IX = 1,NX
               IF (WEIGHT(IX,LY).GT.0.0) THEN
                  SMN = IMAGE(IX,LY) / WEIGHT(IX,LY)
                  DELS = ERROR(IX,LY) / WEIGHT(IX,LY) - SMN*SMN
                  DELS = SQRT (MAX (0.0, DELS))
                  IMAGE(IX,LY) = SMN
                  ERROR(IX,LY) = DELS / SQRT (MAX (1.0, TAMP-1.))
                  IRET = 0
               ELSE
                  IMAGE(IX,LY) = FBLANK
                  ERROR(IX,LY) = FBLANK
                  END IF
 80            CONTINUE
 90         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE DFTIMO (NX, IMAGE, ERROR, IRET)
C-----------------------------------------------------------------------
C   DFTIMO constructs an image header and writes out the waterfall
C   image and possibly the error as well
C   Inputs:
C      NX      I      Image X pixels
C      IMAGE   R(*)   Image(NX,NY)
C      ERROR   R(*)   Error(NX,NY)
C   Outputs:
C      IRET    I      Error condition
C-----------------------------------------------------------------------
      INTEGER   NX, IRET
      REAL      IMAGE(NX,*), ERROR(NX,*)
C
      INCLUDE 'DFTIM.INC'
      INTEGER   IX, IY, I, J, SLOT, LUNO, INDO, BIND, WIN(4), BOI
      REAL      BMIN, BMAX, CATOR(256), CATR(256)
      LOGICAL   WASBLK
      DOUBLE PRECISION CATOD(128), CATD(128)
      HOLLERITH CATOH(256), CATH(256)
      CHARACTER PHNAME*48
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      EQUIVALENCE (CATOLD, CATOD, CATOR, CATOH)
      DATA LUNO /39/
C-----------------------------------------------------------------------
C                                       make up a header
      CALL COPY (256, CATOLD, CATBLK)
      CALL UVPGET (IRET)
C
      CALL CHR2H (8, 'JY/BEAM ', 1, CATH(KHBUN))
C                                       random parameters
      J = KHPTP
      I = 2 * KIPTPN
      CALL RFILL (I, HBLANK, CATH(J))
C                                       axis parameters
      DO 20 I = 1,KICTPN
         J = I - 1
         CALL RFILL (2, HBLANK, CATH(KHCTP+2*J))
         CATR(KRCIC+J) = 1.0
         CATR(KRCRP+J) = 1.0
         CATBLK(KINAX+J) = 1
 20      CONTINUE
      CATBLK(KIGCN) = 0
      CATBLK(KIPCN) = 0
C                                       extensions
      CALL CATCLR (CATBLK)
C                                       now details
      CATBLK(KIDIM) = 3
      CALL CHR2H (8, 'FQID    ', 1, CATH(KHCTP))
      CATBLK(KINAX) = NX
      CATD(KDCRV) = 1.0D0
      CALL CHR2H (8, 'TIME    ', 1, CATH(KHCTP+2))
      CATBLK(KINAX+1) = NY
      CATD(KDCRV+1) = TIMR(1)
      CATR(KRCIC+1) = TAVG
      CALL CHR2H (8, 'FREQ    ', 1, CATH(KHCTP+4))
      CATBLK(KINAX+2) = 1
      CATD(KDCRV+2) = OFREQ
      CATR(KRCIC+2) = FOFF(1)
      IF ((JLOCR.GE.0) .AND. (JLOCD.GE.0)) THEN
         CATBLK(KIDIM) = 5
         CALL CHR2H (8, 'RA---SIN', 1, CATH(KHCTP+6))
         CALL CHR2H (8, 'DEC--SIN', 1, CATH(KHCTP+8))
         CATR(KRCIC+3) = 0.0
         CATR(KRCIC+4) = 0.0
         CATD(KDCRV+3) = RAS
         CATD(KDCRV+4) = DECS
         END IF
      IF (NAMEOU.EQ.' ') NAMEOU = NAMEIN
      IF (CLAOU.EQ.' ') CLAOU = TSKNAM
      CALL CHR2H (12, NAMEOU, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOU, KHIMCO, CATH(KHIMN))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHIMN))
      CATBLK(KIIMS) = SEQOU
      CALL MCREAT (DISKOU, SLOT, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATE OUTPUT IMAGE FILE'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKOU
      FCNO(NCFILE) = SLOT
      FRW(NCFILE) = 2
C                                       start IO
      CALL ZPHFIL ('MA', DISKOU, SLOT, 1, PHNAME, IRET)
      CALL ZOPEN (LUNO, INDO, DISKOU, PHNAME, .TRUE., .TRUE., .TRUE.,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT IMAGE'
         GO TO 990
         END IF
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
      BOI = 1
      CALL MINIT ('WRIT', LUNO, INDO, NX, NY, WIN, BUFF1, JBUFSZ, BOI,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT I/O TO OUTPUT IMAGE'
         GO TO 990
         END IF
      WASBLK = .FALSE.
      BMIN = 1.E10
      BMAX = -BMIN
      DO 50 IY = 1,NY
         CALL MDISK ('WRIT', LUNO, INDO, BUFF1, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT IMAGE'
            GO TO 990
            END IF
         DO 40 IX = 1,NX
            IF (IMAGE(IX,IY).EQ.FBLANK) THEN
               WASBLK = .TRUE.
            ELSE
               BMIN = MIN (BMIN, IMAGE(IX,IY))
               BMAX = MAX (BMAX, IMAGE(IX,IY))
               END IF
            BUFF1(BIND+IX-1) = IMAGE(IX,IY)
 40         CONTINUE
 50      CONTINUE
      CALL MDISK ('FINI', LUNO, INDO, BUFF1, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINISH WRITING OUTPUT IMAGE'
         GO TO 990
         END IF
      CALL ZCLOSE (LUNO, INDO, IRET)
      CATR(KRDMX) = BMAX
      CATR(KRDMN) = BMIN
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
C                                       history
      CALL DFTIMH (NX, DISKOU, SLOT, IUCNO, IRET)
C                                       error image
      IF (BPARM(3).GT.0.0) THEN
         CLAOU = 'DFTerr'
         CALL CHR2H (6, CLAOU, KHIMCO, CATH(KHIMN))
         CATBLK(KIIMS) = SEQOU
         CALL MCREAT (DISKOU, SLOT, BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CREATE ERROR IMAGE FILE'
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKOU
         FCNO(NCFILE) = SLOT
         FRW(NCFILE) = 2
C                                       start IO
         CALL ZPHFIL ('MA', DISKOU, SLOT, 1, PHNAME, IRET)
         CALL ZOPEN (LUNO, INDO, DISKOU, PHNAME, .TRUE., .TRUE., .TRUE.,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING ERROR IMAGE'
            GO TO 990
            END IF
         WIN(1) = 1
         WIN(2) = 1
         WIN(3) = NX
         WIN(4) = NY
         BOI = 1
         CALL MINIT ('WRIT', LUNO, INDO, NX, NY, WIN, BUFF1, JBUFSZ, BOI
     *      ,IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT I/O TO ERROR IMAGE'
            GO TO 990
            END IF
         WASBLK = .FALSE.
         BMIN = 1.E10
         BMAX = -BMIN
         DO 90 IY = 1,NY
            CALL MDISK ('WRIT', LUNO, INDO, BUFF1, BIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING ERROR IMAGE'
               GO TO 990
               END IF
            DO 80 IX = 1,NX
               IF (ERROR(IX,IY).EQ.FBLANK) THEN
                  WASBLK = .TRUE.
               ELSE
                  BMIN = MIN (BMIN, ERROR(IX,IY))
                  BMAX = MAX (BMAX, ERROR(IX,IY))
                  END IF
               BUFF1(BIND+IX-1) = ERROR(IX,IY)
 80            CONTINUE
 90         CONTINUE
         CALL MDISK ('FINI', LUNO, INDO, BUFF1, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'FINISH WRITING ERROR IMAGE'
            GO TO 990
            END IF
         CALL ZCLOSE (LUNO, INDO, IRET)
         CATR(KRDMX) = BMAX
         CATR(KRDMN) = BMIN
         CATR(KRBLK) = 0.0
         IF (WASBLK) CATR(KRBLK) = FBLANK
C                                       history
         CALL DFTIMH (NX, DISKOU, SLOT, IUCNO, IRET)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DFTIMO: ERROR',I4,' ON ',A)
      END
      SUBROUTINE DFTIMH (NX, DISK, CNO, INCNO, IRET)
C-----------------------------------------------------------------------
C   writes history to DFTIM
C   Inputs:
C      DISK   I   Disk number
C      CNO    I   Catalog number of output image
C   Output
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   NX, DISK, CNO, INCNO, IRET
C
      INCLUDE 'DFTIM.INC'
      INTEGER   LUN1, LUN2, IBUFF(256), OBUFF(256), I, VER, NI,
     *   FQID, IFSIDE, LF, LC
      CHARACTER HILINE*72, CLASS*6, BNDCOD*8
      DOUBLE PRECISION IFFREQ
      REAL      IFCHW, IFTBW
      HOLLERITH CATH(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATBLK, CATH)
      DATA LUN1, LUN2 / 45,46/
C-----------------------------------------------------------------------
C                                       Initialize HITAB
      CALL HIINIT (3)
C                                       Create and copy history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISK, INCNO, CNO, CATBLK, IBUFF,
     *   OBUFF, IRET)
      IF (IRET.GT.3) GO TO 999
      IF (IRET.EQ.3) GO TO 100
C                                       Add SUBIM history.
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, OBUFF,
     *   IRET)
      IF (IRET.NE.0) GO TO 100
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLASS)
      CALL HENCOO (TSKNAM, NAMEOU, CLASS, CATBLK(KIIMS), DISK, LUN2,
     *   OBUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       shift
      WRITE (HILINE,2000) TSKNAM, SHIFT
      CALL HIADD (LUN2, HILINE, OBUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       NCHAVG
      WRITE (HILINE,2001) TSKNAM, NCHAVG
      CALL HIADD (LUN2, HILINE, OBUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       cal adverbs
      CALL CALHIS (LUN2, OBUFF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       close
 100  CALL HICLOS (LUN2, .TRUE., OBUFF, I)
C                                       write FQ table
      VER = 1
      NI = 1
      CALL FQINI ('WRIT', FQBUFF, DISK, CNO, VER, CATBLK, LUN1,
     *   IFQRNO, FQKOLS, FQNUMV, NI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATING FQ TABLE'
         CALL MSGWRT (8)
         GO TO 200
         END IF
C                                       write
      IFSIDE = 1
      I = 0
      BNDCOD = ' '
      DO 120 LF = BIF,EIF
         DO 110 LC = BCHAN,ECHAN,NCHAVG
            I = I + 1
            IFQRNO = I
            IFFREQ = FREQS(I)
            FQID = I
            IFCHW = FINC(LF)*NCHAVG
            IFTBW = FINC(LF)*NCHAVG
            CALL TABFQ ('WRIT', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NI,
     *         FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING FQ TABLE'
               CALL MSGWRT (8)
               GO TO 200
               END IF
 110        CONTINUE
 120     CONTINUE
      CALL TABFQ ('CLOS', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *   FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, I)
C
 200  CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'CLWR', IBUFF, I)
      IF (I.NE.0) THEN
         WRITE (MSGTXT,1000) I, 'ON UPDATE HEADER WITH CATIO'
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DFTIMH: ERROR',I4,' ON ',A)
 2000 FORMAT (A6,'SHIFT=',F9.4,',',F9.4,4X,'/ position shift in asec')
 2001 FORMAT (A6,'NCHAVG=',I4,18X,'/ channels averaged')
      END
