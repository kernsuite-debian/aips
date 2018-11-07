LOCAL INCLUDE 'RFLAG.INC'
C                                       Local include for RFLAG
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XSTOK, XFUNC,
     *   XOUTXT(12)
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH(3), XOUFG, UVRANG(2), XBIF, XEIF, XYINC,
     *   DCHNS(4,20), XCHNS(4,20), XAVCHN, XSCALE, DOPLOT, FLAGDO,
     *   FPARM(30), VPARM(30), XNOISE(64), XSCUT(64), XDOTV, XGRCH,
     *   XLABEL, XYRATO, XNBOX, DOOUT, XPARM(7), BADD(10)
      REAL      SCRBUF(256), BUFF1(UVBFSS), NOISE(64), SCUTOF(64)
      INTEGER   SEQIN, DISKIN, JBUFSZ, CATOLD(256), INCSI, INCFI,
     *   INCIFI, NRPRMI, OLDCNO, CHNSEL(3,20,MAXIF), CHNDSL(3,20,MAXIF),
     *   NIF, NSTOK, NANT, NCHAN, COUNT(11), FGVERO, FGVERI, FGVERT,
     *   FGLUN, LIF, LFGRNO, FGKOLS(MAXFGC), FGNUMV(MAXFGC), FGERR,
     *   NPARM, LBIF, AVGCHN, XTRCHN, IBUFF1(UVBFSS), NTAVG
      LOGICAL   DOSPEC, SORTED, DOTIME
      CHARACTER NAMEIN*12, CLAIN*6, REASON*24, FUNCTY*4, OUTEXT*48
      EQUIVALENCE (BUFF1, IBUFF1)
C                                       XPARM included to save info
C                                       for EXTLIST
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XSUBA, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XOUFG, XSTOK,
     *   UVRANG, XBIF, XEIF, XYINC, DCHNS, XCHNS, XAVCHN, XSCALE,
     *   DOPLOT, FLAGDO, FPARM, VPARM, XNOISE, XSCUT, XDOTV, XGRCH,
     *   XLABEL, XFUNC, XYRATO, XNBOX, DOOUT, XOUTXT, BADD, XPARM
      COMMON /RFLAGS/ CATOLD, SEQIN, DISKIN, INCSI, INCFI, INCIFI,
     *   NRPRMI, OLDCNO, CHNDSL, CHNSEL, NIF, NSTOK, NANT, NCHAN, LIF,
     *   COUNT, FGVERO, FGVERI, FGVERT, FGLUN, LFGRNO, FGKOLS, FGNUMV,
     *   FGERR, NPARM, DOSPEC, LBIF, NOISE, SCUTOF, SORTED, AVGCHN,
     *   XTRCHN, DOTIME, NTAVG
      COMMON /CHARPM/ NAMEIN, CLAIN, REASON, FUNCTY, OUTEXT
      COMMON /BUFRS/ SCRBUF, BUFF1, JBUFSZ
C                                       End local include for RFLAG
LOCAL END
LOCAL INCLUDE 'RFHIST.INC'
      INTEGER   NHRMS, NHDEV
      PARAMETER (NHRMS = 20003)
      PARAMETER (NHDEV = 20003)
C
      DOUBLE PRECISION HISRMS(NHRMS), HISDEV(NHDEV)
      INTEGER   CENDEV, CENRMS, NBOXES
      REAL      INCRMS, INCDEV
      COMMON /HISCOM/ HISRMS, HISDEV, INCRMS, INCDEV, CENDEV, CENRMS,
     *   NBOXES
LOCAL END
LOCAL INCLUDE 'RFOLDFG.INC'
      INTEGER   OFGBUF(512), OFGLUN, OFGRNO, OFGSOU, OFGSUB, OFGFQ,
     *   OFGANT(2), OFGIFS(2), OFGCHN(2), OFGREC
      REAL      OFGTIM(2)
      LOGICAL   OFGFLG(4)
      CHARACTER OFGREA*24
      COMMON /OFGPRM/ OFGBUF, OFGLUN, OFGRNO, OFGSOU, OFGSUB, OFGFQ,
     *   OFGANT, OFGIFS, OFGCHN, OFGTIM, OFGFLG, OFGREC
      COMMON /OFGCHR/ OFGREA
LOCAL END
LOCAL INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:PUVD.INC'
C                                       set flag parameters
      INTEGER   MAXFLG
C                                       MAXFLG= max. no. flags active
      PARAMETER (MAXFLG=300001)
LOCAL END
LOCAL INCLUDE 'RFSCALE.INC'
      INTEGER   MXSORC
      PARAMETER (MXSORC=100)
C
      LOGICAL   DOSCAL
      INTEGER   NSRC, TRSORC(MXSORC)
      COMMON /SCALRF/ DOSCAL, NSRC, TRSORC
LOCAL END
      PROGRAM RFLAG
C-----------------------------------------------------------------------
C! Flags data based on rms in time/freq in cross-hand data
C# Utility UV Calibration editing
C-----------------------------------------------------------------------
C;  Copyright (C) 2011-2016, 2018
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
C   RFLAG does SPLIT/SPLAT while calculating weights based on the rms
C   in the spectram of each IF and polarization.  The usual calibration
C   adverbs STOKES, BCHAN, and ECHAN are suppressed.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C   full set of calibration adverbs
C      ICHANSEL
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, NWORDS, IERR, NBL, NTIME, I, LLGITS(2)
      REAL      B(2), FLGITS(2), RFSCAL(2), WTSUM(2)
      LONGINT   PB, PF, PRF, PWT
      EQUIVALENCE (FLGITS, LLGITS)
      INCLUDE 'RFLAG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'RFSCALE.INC'
      DATA PRGM /'RFLAG '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL RFLAIN (PRGM, IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .FALSE.
         GO TO 990
         END IF
      NBL = (NANT * (NANT+1)) / 2
      IF (FPARM(14).GE.NBL) THEN
         MSGTXT = 'FPARM(14) > Number baselines - reset to 0'
         CALL MSGWRT (6)
         FPARM(14) = 0.0
         END IF
      NTIME = FPARM(1) + 0.1
      NTIME = (NTIME/2)*2 + 1
      NTIME = MIN (MAX (3, NTIME), 99)
      FPARM(1) = NTIME
      NWORDS = (3 * NCHAN * NTIME * NBL * LIF * NSTOK - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', PRGM, NWORDS, B, PB, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 990
         END IF
      NWORDS = (NCHAN * NTIME * NBL * LIF - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', PRGM, NWORDS, FLGITS, PF, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 990
         END IF
      IF (DOPLOT.LT.0.0) RQUICK = .FALSE.
C                                       pre-scale
      IF (DOSCAL) THEN
         I = LIF * NANT * NANT * 10
         NWORDS = (I - 1) / 1024 + 4
         CALL ZMEMRY ('GET ', PRGM, NWORDS, RFSCAL, PRF, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', PRGM, NWORDS, WTSUM, PWT,
     *      IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
            CALL MSGWRT (8)
            RQUICK = .FALSE.
            GO TO 990
            END IF
         CALL RFILL (I, 0.0, RFSCAL(1+PRF))
         CALL RFILL (I, 0.0, WTSUM(1+PWT))
         CALL RFLSCL (LIF, NANT, RFSCAL(1+PRF), WTSUM(1+PWT), IRET)
         IF (IRET.GT.0) THEN
            RQUICK = .FALSE.
            GO TO 970
            END IF
         END IF
C                                       find, plot histograms
      IF (DOPLOT.GT.0.0) THEN
         CALL RFLAFP (NCHAN, NTIME, NANT, NBL, LIF, RFSCAL(1+PRF),
     *      B(1+PB), IRET)
         IF (IRET.NE.0) THEN
            RQUICK = .FALSE.
            GO TO 970
            END IF
         END IF
C                                       return parameters
      IF (RQUICK) THEN
         CALL PTPARM (128, XNOISE, SCRBUF, IERR)
         CALL RELPOP (IRET, SCRBUF, IERR)
         END IF
C                                       flag data
      IF ((FLAGDO.GT.0.0) .OR. (DOPLOT.LE.0.0)) THEN
         CALL RFLAFB (NCHAN, NTIME, NANT, NBL, LIF, RFSCAL(1+PRF),
     *      B(1+PB), LLGITS(1+PF), IRET)
         IF (IRET.EQ.0) THEN
            CALL RFLAHI
         ELSE
            GO TO 970
            END IF
         END IF
C                                       find, plot histograms
      IF (DOPLOT.LT.0.0) THEN
         CALL RFLAFP (NCHAN, NTIME, NANT, NBL, LIF, RFSCAL(1+PRF),
     *      B(1+PB), IRET)
         END IF
C                                       free memory
 970  CALL ZMEMRY ('FREE', PRGM, NWORDS, B, PB, IERR)
      CALL ZMEMRY ('FREE', PRGM, NWORDS, FLGITS, PF, IERR)
C                                       Close down files, etc.
 990  IF (.NOT.RQUICK) CALL PTPARM (128, XNOISE, SCRBUF, IERR)
      CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE RFLAIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   RFLAIN gets input parameters for RFLAG and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      INCLUDE 'RFLAG.INC'
      CHARACTER STAT*4, PTYPE*2
      INTEGER   IROUND, IERR, INCX, I, LUN, NW(MAXIF), K, K1, K2,
     *   J, DNW(MAXIF), IDATE(3), ITIME(3)
      LOGICAL   MATCH, MULTI
      REAL      CATR(256), RPARM(20)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'RFSCALE.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
      SORTED = .TRUE.
C                                       Get input parameters.
      NPARM = 540
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.NE.1) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS on failure
            CALL RELPOP (JERR, SCRBUF, IERR)
            END IF
         GO TO 999
         END IF
C                                       Crunch input parameters.
      JERR = 5
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (4, 1, XSTOK, STOKES)
      CALL H2CHR (4, 1, XFUNC, FUNCTY)
      CALL H2CHR (48, 1, XOUTXT, OUTEXT)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      DO 11 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 11      CONTINUE
      SELQUA = IROUND (XQUAL)
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      IF (STOKES.EQ.' ') STOKES = 'RLLR'
      NTAVG = XYINC + 0.1
      NTAVG = MAX (1, NTAVG)
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
      CALL RFILL (8, 0.0, XTIME)
      XTIME(1) = TSTART
      XTIME(5) = TEND
      DOPOL = IROUND(XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
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
C                                       scaling
      DOSCAL = XSCALE.GT.0.0
      NSRC = 0
      CALL FILL (MXSORC, 0, TRSORC)
      I = 10 * MAXIF * MAXANT * MAXANT
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       flag table versions
      CALL FNDEXT ('FG', CATBLK, I)
      IF ((FGVER.EQ.0) .OR. (FGVER.GT.I)) FGVER = I
      IF (I.EQ.0) FGVER = -1
      XFLAG = FGVER
      FGVERO = IROUND (XOUFG)
      IF ((FGVERO.LE.0) .OR. (FGVERO.GT.I+1)) FGVERO = I + 1
      XOUFG = FGVERO
      FGVERI = FGVER
      FGVERT = FGVERO
      IF (FGVERI.EQ.FGVERO) FGVERT = I + 1
      IF (FGVERT.LE.I) FGVERI = -ABS (FGVERI)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Channel selection?
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
         NIF = 1
      ELSE
         NIF = CATBLK(KINAX+JLOCIF)
         BIF = XBIF + 0.5
         BIF = MAX (1, MIN (NIF, BIF))
         EIF = XEIF + 0.5
         IF (EIF.LT.BIF) EIF = NIF
         EIF = MIN (EIF, NIF)
         END IF
      XBIF = BIF
      XEIF = EIF
      NCHAN = CATBLK(KINAX+JLOCF)
      BCHAN = 1
      ECHAN = NCHAN
      LBIF = BIF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
      XFQID = FRQSEL
C                                       Channel selection
      I = 60 * MAXIF
      CALL FILL (I, 0, CHNSEL)
      CALL FILL (I, 0, CHNDSL)
      CALL FILL (MAXIF, 0, NW)
      CALL FILL (MAXIF, 0, DNW)
      DO 40 J = 1,20
         K = IROUND (XCHNS(2,J))
         IF (K.GT.0) THEN
            K = IROUND (XCHNS(4,J))
            IF ((K.LE.0) .OR. (K.GT.MAXIF)) THEN
               K1 = 1
               K2 = MAXIF
            ELSE
               K1 = K
               K2 = K
               END IF
            DO 25 K = K1,K2
               NW(K) = NW(K) + 1
               DO 20 I = 1,3
                  CHNSEL(I,NW(K),K) = IROUND (XCHNS(I,J))
                  IF (CHNSEL(I,NW(K),K).LT.0) CHNSEL(I,NW(K),K) = 0
 20               CONTINUE
               IF (CHNSEL(3,NW(K),K).EQ.0) CHNSEL(3,NW(K),K) = 1
 25            CONTINUE
            END IF
         K = IROUND (DCHNS(2,J))
         IF (K.GT.0) THEN
            K = IROUND (DCHNS(4,J))
            IF ((K.LE.0) .OR. (K.GT.MAXIF)) THEN
               K1 = 1
               K2 = MAXIF
            ELSE
               K1 = K
               K2 = K
               END IF
            DO 35 K = K1,K2
               DNW(K) = DNW(K) + 1
               DO 30 I = 1,3
                  CHNDSL(I,DNW(K),K) = IROUND (DCHNS(I,J))
                  IF (CHNDSL(I,DNW(K),K).LT.0) CHNDSL(I,DNW(K),K) = 0
 30               CONTINUE
               IF (CHNDSL(3,DNW(K),K).EQ.0) CHNDSL(3,DNW(K),K) = 1
 35            CONTINUE
            END IF
 40      CONTINUE
C                                       If no channel selection
C                                       use VLA definition of
C                                       channel 0
      BCHAN = NCHAN
      ECHAN = 1
      DO 50 K = 1,MAXIF
         IF (NW(K).LE.0) THEN
            NW(K) = 1
            CHNSEL(1,1,K) = 1
            CHNSEL(2,1,K) = NCHAN
            CHNSEL(3,1,K) = 1
            END IF
         DO 45 I = 1,NW(K)
            CHNSEL(1,I,K) = MAX (1, MIN (CHNSEL(1,I,K), NCHAN))
            IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K))
     *         CHNSEL(2,I,K) = NCHAN
            CHNSEL(2,I,K) = MAX (1, MIN (CHNSEL(2,I,K), NCHAN))
            BCHAN = MIN (BCHAN, CHNSEL(1,I,K))
            ECHAN = MAX (ECHAN, CHNSEL(2,I,K))
 45         CONTINUE
         DO 46 I = 1,DNW(K)
            CHNDSL(1,I,K) = MAX (1, MIN (CHNDSL(1,I,K), NCHAN))
            IF (CHNDSL(2,I,K).LT.CHNDSL(1,I,K))
     *         CHNDSL(2,I,K) = NCHAN
            CHNDSL(2,I,K) = MAX (1, MIN (CHNDSL(2,I,K), NCHAN))
 46      CONTINUE
 50      CONTINUE
C                                       is this valid thing to do
      IF (ECHAN-BCHAN.LT.3) THEN
         JERR = 10
         MSGTXT = 'TOO FEW SPECTRAL CHANNELS TO COMPUTE RMS'
         GO TO 990
      ELSE IF (ECHAN-BCHAN.LT.10) THEN
         MSGTXT = 'NUMBER SPECTRAL CHANNELS PRETTY LOW FOR THIS OP'
         CALL MSGWRT (7)
         END IF
      CALL FNDEXT ('BP', CATBLK, K1)
      CALL MULSDB (CATBLK, MULTI)
      IF (MULTI) THEN
         CALL FNDEXT ('CL', CATBLK, K2)
      ELSE
         CALL FNDEXT ('SN', CATBLK, K2)
         END IF
      IF ((DOBAND.LE.0) .AND. (K1.GT.0)) THEN
         MSGTXT = 'WARNING: BANDPASS TABLE IS NOT BEING APPLIED'
         CALL MSGWRT (7)
         END IF
      IF ((.NOT.DOCAL) .AND. (K2.GT.0)) THEN
         MSGTXT = 'WARNING: CL/SN TABLES ARE NOT BEING APPLIED'
         CALL MSGWRT (7)
         END IF
C                                       reset to get all channels
      BCHAN = 1
      ECHAN = NCHAN
C                                       uvrange by SPW in UVGET
      UVRNG(1) = UVRANG(1)
      UVRNG(2) = UVRANG(2)
      IF (UVRANG(2).LE.UVRANG(1)) UVRANG(2) = 1.E9
      UVRANG(1) = UVRANG(1)*UVRANG(1)*1.E6
      UVRANG(2) = UVRANG(2)*UVRANG(2)*1.E6
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, SCRBUF, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      LIF = 1
      IF (JLOCIF.GE.0) LIF = CATBLK(KINAX+JLOCIF)
      NSTOK = CATBLK(KINAX+JLOCS)
      CALL CHR2H (4, STOKES, 1, XSTOK)
      CALL UVGET ('CLOS', RPARM, SCRBUF, IERR)
C                                       get max antenna number
      CALL GETANT (DISKIN, OLDCNO, SUBARR, CATUV, SCRBUF, IERR)
      NANT = NSTNS
C                                       Save file info (w cal adverbs)
      INCX = CATBLK(KINAX)
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       parameter check
      IF (FPARM(3).EQ.0.0) FPARM(3) = 1.0E6
      IF (FPARM(7).LE.0.0) FPARM(7) = 1.0
      IF (FPARM(9).LE.0.0) FPARM(9) = 5.0
      IF (FPARM(10).LE.0.0) FPARM(10) = 5.0
      IF (FPARM(11).LE.0.0) FPARM(11) = 1.1
      IF (FPARM(12).LE.0.0) FPARM(12) = 1.1
      IF (FPARM(13).LE.0.0) FPARM(13) = 1.E8
      FPARM(13) = FPARM(13) * FPARM(13)
      IF (FPARM(14).LT.0.0) FPARM(14) = 0.
      IF (FPARM(15).GT.0.0) FPARM(15) = FPARM(15) * FPARM(15)
      IF (FPARM(16).GT.0.0) FPARM(16) = FPARM(16) * FPARM(16)
      DOSPEC = FPARM(4).NE.0.0
C                                       channel median window
C                                       channels used from window
      IF (DOSPEC) THEN
         AVGCHN = IROUND (XAVCHN)
         IF ((AVGCHN.LE.0) .OR. (AVGCHN.GT.((NCHAN-2)/2)*2+1))
     *      AVGCHN = ((NCHAN-2)/2)*2 + 1
         IF (((AVGCHN+1)/2)*2.NE.AVGCHN+1) AVGCHN = AVGCHN - 1
         AVGCHN = MAX (3, AVGCHN)
         XTRCHN = IROUND (FPARM(5))
         IF (XTRCHN.EQ.0) XTRCHN = (AVGCHN / 40)
         IF (XTRCHN.GT.(AVGCHN-5)/10) XTRCHN = (AVGCHN-5)/10
         XTRCHN = MAX (0, 2*XTRCHN)
      ELSE
         AVGCHN = 0
         XTRCHN = 0
         END IF
C                                       FG table init
      CALL ZDATE (IDATE)
      CALL ZTIME (ITIME)
      WRITE (REASON,1010) IDATE, ITIME(1), ITIME(2)
      FGERR = 0
      IF (FGVERO.LT.0) FGERR = 1
      LFGRNO = 0
      FGLUN = 78
      JERR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RFLAIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('RFLAG:',I4,'/',I2.2,'/',I2.2,I3.2,':',I2.2)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE CHWANT (NC, NI, CHNSEL, CHFLGS)
C-----------------------------------------------------------------------
C   Makes a mask of the desired channels
C   Inputs:
C      NC       I            Number spectral chans
C      NI       I            Number IFs
C      CHNSEL   I(3,20,*)    Start, stop, incr 20 sets per IF
C   Outputs
C      CHFLGS   I(*,*)       1 => use, 0 => don't use
C-----------------------------------------------------------------------
      INTEGER   NC, NI, CHNSEL(3,20,*), CHFLGS(NC,NI)
C
      INTEGER   I, J, K
C-----------------------------------------------------------------------
      J = NC * NI
      CALL FILL (J, 0, CHFLGS)
      DO 30 K = 1,NI
         DO 20 J = 1,20
            IF ((CHNSEL(1,J,K).GT.0) .AND. (CHNSEL(3,J,K).GT.0) .AND.
     *         (CHNSEL(2,J,K).GE.CHNSEL(1,J,K))) THEN
               DO 10 I = CHNSEL(1,J,K),CHNSEL(2,J,K),CHNSEL(3,J,K)
                  CHFLGS(I,K) = 1
 10               CONTINUE
               END IF
 20         CONTINUE
 30      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RFLAHI
C-----------------------------------------------------------------------
C   RFLAHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, FGNAM1*48, FGNAM2*48, STAT*4
      INTEGER   HLUN, IERR, I, J, IROUND
      REAL      UVR(2), TEMP
      INCLUDE 'RFLAG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA HLUN /28/
C-----------------------------------------------------------------------
C                                       only if flags written
      IF ((COUNT(2).LE.0) .AND. (COUNT(3).LE.0)) GO TO 999
C                                       Move FG table to desired version
      CALL FNDEXT ('FG', CATUV, I)
      IF (I.GT.FGVERT) THEN
         MSGTXT = 'WE HAVE TOO MANY FG TABLES, NO COPY'
         CALL MSGWRT (6)
      ELSE IF ((FGVERO.GT.0) .AND. (FGVERO.LT.FGVERT)) THEN
         WRITE (MSGTXT,1000) FGVERT, FGVERO
         CALL MSGWRT (4)
         CALL ZPHFIL ('FG', DISKIN, OLDCNO, FGVERT, FGNAM1, IERR)
         CALL ZPHFIL ('FG', DISKIN, OLDCNO, FGVERO, FGNAM2, IERR)
         CALL ZDESTR (DISKIN, FGNAM2, IERR)
         CALL ZRENAM (DISKIN, FGNAM1, FGNAM2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1001) IERR
            CALL MSGWRT (8)
         ELSE
            STAT = 'READ'
            CALL DELEXT ('FG', DISKIN, OLDCNO, STAT, BUFF1, SCRBUF,
     *         FGVERT, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1002) IERR
               CALL MSGWRT (8)
               END IF
            END IF
         END IF
C                                       Write History.
      CALL HIINIT (3)
      CALL HIOPEN (HLUN, DISKIN, OLDCNO, SCRBUF, IERR)
C                                       calibration history
      CALL CALHIS (HLUN, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1010) TSKNAM, FPARM(2)
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
      IF (DOTIME) THEN
         WRITE (HILINE,1011) TSKNAM, NTAVG
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         I = IROUND (FPARM(1))
         WRITE (HILINE,1005) TSKNAM, I
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         IF ((FPARM(3).GT.0.0) .AND. (FPARM(3).LT.1.E6)) THEN
            WRITE (HILINE,1020) TSKNAM, FPARM(3)
            CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
            IF (IERR.NE.0) GO TO 100
         ELSE IF (FPARM(3).LE.0.0) THEN
            DO 5 I = BIF,EIF
               WRITE (HILINE,1021) TSKNAM, I, NOISE(I), I
               CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
               IF (IERR.NE.0) GO TO 100
 5             CONTINUE
            END IF
         END IF
C                                       channel de-selection
      DO 20 I = BIF,EIF
         DO 10 J = 1,20
            IF (CHNDSL(1,J,I).GT.0) THEN
               WRITE (HILINE,1023) TSKNAM, J, I, CHNDSL(1,J,I),
     *            CHNDSL(2,J,I), CHNDSL(3,J,I)
               CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
               IF (IERR.NE.0) GO TO 100
               END IF
 10         CONTINUE
 20      CONTINUE
C                                       UV range
      UVR(1) = SQRT (UVRANG(1)) / 1.E3
      UVR(2) = SQRT (UVRANG(2)) / 1.E3
      IF ((UVR(1).GT.0.0) .OR. (UVR(2).LT.9.E8)) THEN
         WRITE (HILINE,1025) TSKNAM, UVR
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       spectral rms parameters
      IF (DOSPEC) THEN
         IF (FPARM(4).GT.0.0) THEN
            WRITE (HILINE,1030) TSKNAM, FPARM(4)
            CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
            IF (IERR.NE.0) GO TO 100
         ELSE
            DO 25 I = BIF,EIF
               WRITE (HILINE,1022) TSKNAM, I, SCUTOF(I), I
               CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
               IF (IERR.NE.0) GO TO 100
 25            CONTINUE
            END IF
C                                       channel selection
         DO 40 I = BIF,EIF
            DO 30 J = 1,20
               IF (CHNSEL(1,J,I).GT.0) THEN
                  WRITE (HILINE,1031) TSKNAM, J, I, CHNSEL(1,J,I),
     *               CHNSEL(2,J,I), CHNSEL(3,J,I)
                  CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
                  IF (IERR.NE.0) GO TO 100
                  END IF
 30            CONTINUE
 40         CONTINUE
C                                       type
         WRITE (HILINE,1032) TSKNAM, AVGCHN
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,1033) TSKNAM, (XTRCHN+1)
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       flag excessives
      IF (FPARM(7).LT.1.0) THEN
         WRITE (HILINE,1040) TSKNAM, FPARM(7)
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       flag between
      I = FPARM(8) + 0.1
      IF (I.GT.0) THEN
         WRITE (HILINE,1041) TSKNAM, I
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      WRITE (HILINE,1042) TSKNAM, FGVERO
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1043) TSKNAM, FPARM(11)
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1044) TSKNAM, FPARM(12)
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       summary
      WRITE (HILINE,1051) TSKNAM, COUNT(2)
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1052) TSKNAM, COUNT(3)
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
      IF (COUNT(4).GT.0) THEN
         WRITE (HILINE,1053) TSKNAM, COUNT(4)
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (COUNT(5).GT.0) THEN
         WRITE (HILINE,1054) TSKNAM, COUNT(5)
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (COUNT(6).GT.0) THEN
         WRITE (HILINE,1055) TSKNAM, COUNT(6)
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (COUNT(7).GT.0) THEN
         FPARM(13) = SQRT (FPARM(13))
         WRITE (HILINE,1056) TSKNAM, FPARM(13), COUNT(7)
         FPARM(13) = FPARM(13) * FPARM(13)
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (COUNT(8).GT.0) THEN
         WRITE (HILINE,1057) TSKNAM, FPARM(14), COUNT(8)
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (COUNT(9).GT.0) THEN
         I = FPARM(6) + 0.1
         WRITE (HILINE,1058) TSKNAM, I, COUNT(9)
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (COUNT(10).GT.0) THEN
         TEMP = SQRT (FPARM(15))
         WRITE (HILINE,1059) TSKNAM, TEMP, COUNT(10)
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (COUNT(11).GT.0) THEN
         TEMP = SQRT (FPARM(16))
         WRITE (HILINE,1060) TSKNAM, TEMP, COUNT(11)
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       Close HI file
 100  CALL HICLOS (HLUN, .TRUE., SCRBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Temporary FG version',I4,' moves to output version',I4)
 1001 FORMAT ('ERROR',I4,' RENAMING TEMP FG TABLE TO OUTPUT VERSION')
 1002 FORMAT ('ERROR',I4,' REMOVING TEMP FG TABLE FROM HEADER')
 1005 FORMAT (A6,'FPARM(1)=',I7,' / data samples in time rms')
 1010 FORMAT (A6,'FPARM(2)=',F7.1,' / normal time interval in',
     *   ' time rms')
 1011 FORMAT (A6,'YINC =',I4,4X,'/ time samples pre-averaged')
 1020 FORMAT (A6,'FPARM(3)=',F7.4,' / flag all time rms > FPARM(3)')
 1021 FORMAT (A6,'NOISE(',I2,')=',F7.4,' / flag IF',I3,
     *   ' time rms > NOISE(i)')
 1022 FORMAT (A6,'NOISE(',I2,')=',F7.4,' / flag channel in IF',I3,
     *   ' deviation > NOISE(i)')
 1023 FORMAT (A6,'DCHANSEL(,',I2,',',I2,') =',I5,',',I5,',',I2,
     *   '  / channels not examined')
 1025 FORMAT (A6,'UVRANGE=',2F8.1,' / restrict baseline length klambda')
 1030 FORMAT (A6,'FPARM(4)=',F5.1,'  / spectral deviation flag > ',
     *   'FPARM(4)')
 1031 FORMAT (A6,'CHANSEL(,',I2,',',I2,') =',I5,',',I5,',',I2)
 1032 FORMAT (A6,'AVGCHAN =',I5,'  / width of spectral median window')
 1033 FORMAT (A6,'XTRCHAN =',I5,
     *   '  / # chans per spectral median window')
 1040 FORMAT (A6,'FPARM(7)=',F7.4,' / flag all if fraction flagged >',
     *   ' FPARM(7)')
 1041 FORMAT (A6,'FPARM(8)=',I3,' / flag up to F(8) channels',
     *   ' between flagged groups')
 1042 FORMAT (A6,'OUTFGVER=',I5,' / output flag file version number')
 1043 FORMAT (A6,'FPARM(11)=',F7.4,' / flag all baselines if fract',
     *   ' flagged>FPARM(11)')
 1044 FORMAT (A6,'FPARM(12)=',F7.4,' / flag antenna if fract of BL',
     *   ' flagged>FPARM(12)')
 1051 FORMAT (A6,'/ ',I12,' full spectrum flags written')
 1052 FORMAT (A6,'/ ',I12,' partial spectrum flags written')
 1053 FORMAT (A6,'/ ',I12,' channels flagged between flagged groups')
 1054 FORMAT (A6,'/ ',I12,' channels flagged over all baselines',
     *   ' (F(11))')
 1055 FORMAT (A6,'/ ',I12,' channels flagged over antenna',
     *   ' (F(12))')
 1056 FORMAT (A6,'FPARM(13)=',F7.1,' / ',I12,' channels pre-clipped')
 1057 FORMAT (A6,'FPARM(14)=',F7.0,' / ',I12,' channels pre-quacked')
 1058 FORMAT (A6,'FPARM(6)=',I4,' / ',I12,' channels over-run')
 1059 FORMAT (A6,'FPARM(15)=',F9.1,' / ',I12,' IFs * BLs flagged')
 1060 FORMAT (A6,'FPARM(16)=',F9.1,' / ',I12,' IFs * BLs flagged')
      END
      SUBROUTINE RFLSCL (LLIF, LLANT, RFSCAL, WTSUM, IRET)
C-----------------------------------------------------------------------
C   RFLSCL finds the scaling of the input data
C   Outputs:
C      IRET     I      error code
C   Common output:
C      NSRC     I      number sources encountered
C      TRSORC   I(*)   TRSORC(source #) = local source number
C      RFSCAL   R(*)   relative amplitude (if, ia1, ia2, lsource)
C-----------------------------------------------------------------------
      INTEGER   LLIF, LLANT, IRET
      REAL      RFSCAL(LLIF,LLANT,LLANT,*), WTSUM(LLIF,LLANT,LLANT,*)
C
      INCLUDE 'RFLAG.INC'
      INTEGER   CHFLGS(MAXCIF), CHFLGD(MAXCIF), IA1, IA2, ISRC, MXANT,
     *   JS, JF, JI, NS, IP, IND, LP, NUMVIS
      REAL      VIS(3,UVBFSS/3), RPARM(20), WS, TEMP, MEDIAN
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'RFSCALE.INC'
C-----------------------------------------------------------------------
      NCHAN = CATBLK(KINAX+JLOCF)
      NS = CATBLK(KINAX+JLOCS)
      CALL CHWANT (NCHAN, LIF, CHNSEL(1,1,BIF), CHFLGS)
      CALL CHWANT (NCHAN, LIF, CHNDSL(1,1,EIF), CHFLGD)
      MXANT = 0
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, SCRBUF)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN/INIT INPUT VIS FILE'
         GO TO 990
         END IF
      CALL COPY (256, SCRBUF, CATBLK)
      MSGTXT = 'Reading data to get scaling of amplitudes'
      CALL MSGWRT (2)
      NUMVIS = 0
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING DATA FIRST PASS'
         GO TO 990
C                                       got good data
      ELSE IF (IRET.EQ.0) THEN
         NUMVIS = NUMVIS + 1
         IF (MOD(NUMVIS,50000).EQ.0) THEN
            WRITE (MSGTXT,1100) NUMVIS
            CALL MSGWRT (2)
            END IF
C                                       antennas
         IF (ILOCB.GT.0) THEN
            TEMP = RPARM(1+ILOCB)
            IA1 = TEMP / 256 + 0.01
            IA2 = TEMP - 256*IA1 + 0.01
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         MXANT = MAX (IA1, MXANT)
         MXANT = MAX (IA2, MXANT)
C                                       source
         ISRC = 1
         IF (ILOCSU.GE.0) ISRC = RPARM(1+ILOCSU) + 0.01
         IF (TRSORC(ISRC).LE.0) THEN
            NSRC = NSRC + 1
            IF (NSRC.GT.10) THEN
               MSGTXT = 'TOO MANY SOURCES FOR SCALING'
               IRET = 10
               GO TO 990
               END IF
            TRSORC(ISRC) = NSRC
            END IF
         ISRC = TRSORC(ISRC)
C                                       loop over IF
         DO 150 JI = 1,LIF
            IP = 0
            WS = 0.0
            LP = 1
            DO 140 JF = 1,NCHAN
               IF ((CHFLGS(LP).GT.0) .AND. (CHFLGD(LP).LE.0)) THEN
                  IND = (JI - 1) * INCIFI + (JF - 1) * INCFI + 1
                  DO 130 JS = 1,NS
                     IF (VIS(3,IND).GT.0.0) THEN
                        IP = IP + 1
                        BUFF1(IP) = SQRT (VIS(1,IND)*VIS(1,IND) +
     *                     VIS(2,IND)*VIS(2,IND))
                        WS = WS + VIS(3,IND)
                        END IF
                     IND = IND + INCSI
 130                 CONTINUE
                  END IF
               LP = LP + 1
 140           CONTINUE
            IF (IP.GT.0) THEN
               TEMP = MEDIAN (IP, BUFF1)
               WS = SQRT (WS)
               RFSCAL(JI,IA1,IA2,ISRC) = RFSCAL(JI,IA1,IA2,ISRC) +
     *            WS * TEMP
               WTSUM(JI,IA1,IA2,ISRC) = WTSUM(JI,IA1,IA2,ISRC) + WS
               END IF
 150        CONTINUE
         GO TO 100
C                                       time to quit
      ELSE
         CALL UVGET ('CLOS', RPARM, VIS, IRET)
         END IF
C                                       now scale things
      DO 250 ISRC = 1,NSRC
         DO 240 JI = 1,LIF
            WS = 0.0
            IP = 0
            DO 215 IA1 = 1,MXANT
               DO 210 IA2 = 1,MXANT
                  IF (WTSUM(JI,IA1,IA2,ISRC).GT.0.0) THEN
                     RFSCAL(JI,IA1,IA2,ISRC) = RFSCAL(JI,IA1,IA2,ISRC) /
     *                  WTSUM(JI,IA1,IA2,ISRC)
                     IP = IP + 1
                     WS = WS + RFSCAL(JI,IA1,IA2,ISRC)
                     END IF
 210              CONTINUE
 215           CONTINUE
            IF (IP.GT.0) THEN
               WS = WS / IP
               DO 225 IA1 = 1,MXANT
                  DO 220 IA2 = 1,MXANT
                     RFSCAL(JI,IA1,IA2,ISRC) = RFSCAL(JI,IA1,IA2,ISRC)
     *                  / WS
 220                 CONTINUE
 225              CONTINUE
               END IF
 240        CONTINUE
 250     CONTINUE
      GO TO 999
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RFLSCL ERROR',I5,' ON ',A)
 1100 FORMAT ('RFLSCL: on visibility number',I11)
      END
      SUBROUTINE RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B, ISONE,
     *   GETNEW)
C-----------------------------------------------------------------------
C   moves the vis array to the right place in buffer
C   Inputs
C      NC       I      Number spectral channels
C      NT       I      Number times - max
C      NA       I      Number antennas
C      NB       I      Number baselines
C      NI       I      Number IFs
C      CT       I      Current time number
C      RPARM    R(*)   Random parameters: gets baseline number
C      VIS      R(*)   Vis values (3,*)
C   In/out
C      B        R(*)   Data buffer
C      ISONE    I(*)   0 => space available (baseline, time #)
C-----------------------------------------------------------------------
      INTEGER   NC, NT, NB, NI, CT, ISONE(NB,*)
      REAL      RPARM(*), VIS(3,*), B(3,NC,NT,NB,NI,*)
      LOGICAL   GETNEW
C
      INTEGER   JA1, JA2, JBL, JI, JP, JF, INDI
      REAL      BASEN, WT
      INCLUDE 'RFLAG.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      IF (ILOCB.GE.0) THEN
         BASEN = RPARM(1+ILOCB)
         JA1 = BASEN / 256. + 0.1
         JA2 = BASEN - JA1*256. + 0.1
      ELSE
         JA1 = RPARM(1+ILOCA1) + 0.1
         JA2 = RPARM(1+ILOCA2) + 0.1
         END IF
      JBL = NANT * (JA1-1) - ((JA1*(JA1-1))/2) + JA2
      GETNEW = ISONE(JBL,CT).GE.NTAVG
      IF (.NOT.GETNEW) THEN
         ISONE(JBL,CT) = 1 + ISONE(JBL,CT)
         DO 40 JI = 1,NI
            DO 30 JP = 1,NSTOK
               INDI = (JI-1) * INCIFI + (JP-1) * INCSI + 1 - INCFI
               IF (ISONE(JBL,CT).EQ.1) THEN
                  DO 20 JF = 1,NC
                     INDI = INDI + INCFI
                     WT = MAX (0.0, VIS(3,INDI))
                     B(3,JF,CT,JBL,JI,JP) = WT
                     B(1,JF,CT,JBL,JI,JP) = WT * VIS(1,INDI)
                     B(2,JF,CT,JBL,JI,JP) = WT * VIS(2,INDI)
 20                  CONTINUE
               ELSE
                  DO 25 JF = 1,NC
                     INDI = INDI + INCFI
                     WT = MAX (0.0, VIS(3,INDI))
                     B(3,JF,CT,JBL,JI,JP) = B(3,JF,CT,JBL,JI,JP) + WT
                     B(1,JF,CT,JBL,JI,JP) = B(1,JF,CT,JBL,JI,JP) + WT *
     *                  VIS(1,INDI)
                     B(2,JF,CT,JBL,JI,JP) = B(2,JF,CT,JBL,JI,JP) + WT *
     *                  VIS(2,INDI)
 25                  CONTINUE
                  END IF
 30            CONTINUE
 40         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE RWTAVG (NC, NT, NB, NI, CT, B)
C-----------------------------------------------------------------------
C   converts the summed and weighted vis into simple average
C   Inputs
C      NC       I      Number spectral channels
C      NT       I      Number times - max
C      NA       I      Number antennas
C      NB       I      Number baselines
C      NI       I      Number IFs
C      CT       I      Current time number
C   In/out
C      B        R(*)   Data buffer
C-----------------------------------------------------------------------
      INTEGER   NC, NT, NB, NI, CT
      REAL      B(3,NC,NT,NB,NI,*)
C
      INTEGER   JBL, JI, JP, JF
      REAL      WT
      INCLUDE 'RFLAG.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      DO 50 JBL = 1,NB
         DO 40 JI = 1,NI
            DO 30 JP = 1,NSTOK
               DO 20 JF = 1,NC
                  WT = B(3,JF,CT,JBL,JI,JP)
                  IF (WT.GT.0.0) THEN
                     B(1,JF,CT,JBL,JI,JP) = B(1,JF,CT,JBL,JI,JP) / WT
                     B(2,JF,CT,JBL,JI,JP) = B(2,JF,CT,JBL,JI,JP) / WT
                     END IF
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
 50      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RWRMSF (NC, NT, NA, NB, NI, NCT, J1, J2, J3, J4, TNUM,
     *   TIMES, CHFLGS, CHFLGD, SUBARR, FRQSEL, FGBUFF, CATUV, B,
     *   FLGITS, CNT)
C-----------------------------------------------------------------------
C   robust rms in arrays - generate flags
C   Inputs
C      NC       I      Number spectral channels
C      NT       I      Number times - max
C      NA       I      Number antennas
C      NB       I      Number baselines
C      NI       I      Number IFs
C      NCT      I      Number times in this call
C      J1       I      Time number to mark for time rms lower limit
C      J2       I      Time number to mark for time rms upper limit
C      J3       I      Time number to flag for spectral and extensions
C                      and then write out lower limit
C      J4       I      Time number upper limit with J3
C      TNUM     I(*)   Position in B of data
C      TIMES    R(*)   Time of data
C      CHFLGS   I(*)   > 0 => incl in freq rms (NC,NI)
C      CHFLGD   I(*)   > 0 => do not include (NC,NI)
C      B        R(*)   Data buffer (3,NC,NT,NB,NI,NP)
C   In/out:
C      CNT      I(7)   (1) Number data records processed
C                      (2) Number of full flag records written
C                      (3) Number partial flag table records written
C                      (4) Number channels between groups
C                      (5) Number channels done for all baselines
C                      (6) Number channels done for all BL to an antenna
C                      (7) Number channels pre-clipped
C                      (8) Number channels pre-quacked)
C                      (9) Number channels by GROW function
C                      (10) number spectral windows * BLs by (15)
C                      (11) number spectral windows * BLs by (15)
C   Output:
C      FLGITS   I(*)   Work buffer for flag info
C-----------------------------------------------------------------------
      INTEGER   NC, NT, NA, NB, NI, NCT, J1, J2, J3, J4, TNUM(*),
     *   CHFLGS(NC,NI), CHFLGD(NC,NI), SUBARR, FRQSEL, FGBUFF(512),
     *   CATUV(256), FLGITS(NC,NB,NI,NT), CNT(11)
      REAL      TIMES(2,*), B(3,NC,NT,NB,NI,*)
C
      INCLUDE 'RFLAG.INC'
      INTEGER   NITER
      PARAMETER (NITER=12)
C
      INTEGER   JF, JI, JP, JA1, JA2, JT, JB, L, LR, JJ, JX, K, JLIM,
     *   FLGIT(MAXCHA,99,4), CHNS(2), JCNT, NEXT, NX, JXX, JIB, JC1,
     *   JC2, JF1, JF2, NGROW, KA1, KA2, LIMIT
      LOGICAL   INONE, DOBL, DOANT, CNTIT
      REAL      RMSR, RMSI, MEDIAN, VM(MAXCHA,2), BTIME, TEMP,
     *   ETIME, EPS, V1, V2
      DOUBLE PRECISION WT, SR, SSR, SI, SSI, WR, AVR, AVI, V
      INTEGER FLGITA(MAXCHA), FLGITB(MAXCHA), FLGITC(MAXCHA)
      EQUIVALENCE (FLGITA, FLGIT(1,1,1))
      EQUIVALENCE (FLGITB, FLGIT(1,2,1))
      EQUIVALENCE (FLGITC, FLGIT(1,3,1))
      INCLUDE 'INCS:DMSG.INC'
      DATA EPS /1.1E-6/
C-----------------------------------------------------------------------
      IF ((J1.LE.0) .OR. (J2.LE.0)) GO TO 900
      CNT(1) = CNT(1) + J2 - J1 + 1
      NEXT = FPARM(8) + 0.1
      NGROW = FPARM(6) + 0.1
      NGROW = MAX (0, NGROW)
      LIMIT = 2147480000
C                                       pre-"quack"
      IF (FPARM(14).GE.1.0) THEN
         DO 670 JI = 1,NI
            DO 660 JF = 1,NC
               DO 650 JJ = 1,NCT
                  JX = TNUM(JJ)
                  L = 0
                  DO 620 JA1 = 1,NA
                     DO 610 JA2 = 1,NA
                        IF (JA2.LT.JA1) THEN
                           JB = NA * (JA2-1) - (JA2*(JA2-1))/2 + JA1
                        ELSE
                           JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
                           END IF
                        K = 0
                        DO 605 JP = 1,NSTOK
                           WT = B(3,JF,JX,JB,JI,JP)
                           IF (WT.GT.0) K = K + 1
 605                       CONTINUE
                        IF (K.GT.0) L = L + 1
 610                    CONTINUE
 620                 CONTINUE
                  IF ((L.GT.0) .AND. (L.LT.FPARM(14))) THEN
                     CNT(8) = CNT(8) + 1
                     DO 640 JA1 = 1,NA
                        DO 630 JA2 = 1,NA
                           IF (JA2.LT.JA1) THEN
                              JB = NA * (JA2-1) - (JA2*(JA2-1))/2 + JA1
                           ELSE
                              JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
                              END IF
                           IF (FLGITS(JF,JB,JI,JX).EQ.0)
     *                        FLGITS(JF,JB,JI,JX) = 2
 630                       CONTINUE
 640                    CONTINUE
                     END IF
 650              CONTINUE
 660           CONTINUE
 670        CONTINUE
         END IF
C                                       extreme clips
      IF ((FPARM(15).GT.0.0) .OR. (FPARM(16).GT.0.0)) THEN
         DO 770 JI = 1,NI
            DO 760 JJ = 1,NCT
               JX = TNUM(JJ)
               DO 750 JA1 = 1,NA
                  DO 720 JA2 = 1,NA
                     IF (JA2.LT.JA1) THEN
                        JB = NA * (JA2-1) - (JA2*(JA2-1))/2 + JA1
                     ELSE
                        JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
                        END IF
                     DOBL = .FALSE.
                     DOANT = .FALSE.
                     DO 710 JF = 1,NC
                        DO 705 JP = 1,NSTOK
                           IF (CHFLGD(JF,JI).LE.0) THEN
                              WT = B(3,JF,JX,JB,JI,JP)
                              IF (WT.GT.0) THEN
                                 V1 = B(1,JF,JX,JB,JI,JP)
                                 V2 = B(2,JF,JX,JB,JI,JP)
                                 SR = V1*V1 + V2*V2
                                 IF (SR.GT.FPARM(15)) THEN
                                    DOBL = .TRUE.
                                    END IF
                                 IF (SR.GT.FPARM(16)) THEN
                                    DOANT = .TRUE.
                                    END IF
                                 END IF
                              END IF
 705                       CONTINUE
 710                    CONTINUE
                     IF (FPARM(15).LE.0.0) DOBL = .FALSE.
                     IF (FPARM(16).LE.0.0) DOANT = .FALSE.
                     IF (DOANT) THEN
                        GO TO 725
C                                       flag JA2 - JA1
                     ELSE IF (DOBL) THEN
                        CNTIT = .FALSE.
                        DO 715 JF = 1,NC
                           FLGITS(JF,JB,JI,JX) = 2
                           DO 714 JP = 1,NSTOK
                              IF (B(3,JF,JX,JB,JI,JP).GT.0.0) THEN
                                 CNTIT = .TRUE.
                                 B(3,JF,JX,JB,JI,JP) =
     *                              -B(3,JF,JX,JB,JI,JP)
                                 END IF
 714                          CONTINUE
 715                       CONTINUE
                        IF (CNTIT) CNT(10) = CNT(10) + 1
                        END IF
 720                 CONTINUE
                  DOANT = .FALSE.
C                                       flag all BL with JA1
 725              IF (DOANT) THEN
                     KA1 = JA1
                     KA2 = JA2
                     DO 740 JA2 = 1,NA
C                                       baselines with KA1
                        IF (JA2.LT.JA1) THEN
                           JB = NA * (JA2-1) - (JA2*(JA2-1))/2 + KA1
                        ELSE
                           JB = NA * (KA1-1) - (KA1*(KA1-1))/2 + JA2
                           END IF
                        CNTIT = .FALSE.
                        DO 730 JF = 1,NC
                           FLGITS(JF,JB,JI,JX) = 2
                           DO 729 JP = 1,NSTOK
                              IF (B(3,JF,JX,JB,JI,JP).GT.0.0) THEN
                                 CNTIT = .TRUE.
                                 B(3,JF,JX,JB,JI,JP) =
     *                              -B(3,JF,JX,JB,JI,JP)
                                 END IF
 729                          CONTINUE
 730                       CONTINUE
                        IF (CNTIT) CNT(11) = CNT(11) + 1
C                                       baselines with KA2
                        IF (JA2.LT.KA2) THEN
                           JB = NA * (JA2-1) - (JA2*(JA2-1))/2 + KA2
                        ELSE
                           JB = NA * (KA2-1) - (KA2*(KA2-1))/2 + JA2
                           END IF
                        CNTIT = .FALSE.
                        DO 735 JF = 1,NC
                           FLGITS(JF,JB,JI,JX) = 2
                           DO 734 JP = 1,NSTOK
                              IF (B(3,JF,JX,JB,JI,JP).GT.0.0) THEN
                                 CNTIT = .TRUE.
                                 B(3,JF,JX,JB,JI,JP) =
     *                              -B(3,JF,JX,JB,JI,JP)
                                 END IF
 734                          CONTINUE
 735                       CONTINUE
                        IF (CNTIT) CNT(11) = CNT(11) + 1
 740                    CONTINUE
                     END IF
 750              CONTINUE
 760           CONTINUE
 770        CONTINUE
         END IF
C                                       time analysis
C                                       one channel at a time
      DO 200 JA1 = 1,NA
         DO 190 JA2 = JA1,NA
            JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
            DO 180 JI = 1,NI
               JIB = JI + LBIF - 1
               DO 10 JJ = J1,J2
                  JX = TNUM(JJ)
                  CALL FILL (NC, 0, FLGIT(1,JX,1))
                  DO 5 JF = 1,NC
                     IF (FLGITS(JF,JB,JI,JX).EQ.2) FLGIT(JF,JX,1) = 2
 5                   CONTINUE
                  CALL COPY (NC, FLGIT(1,JX,1), FLGIT(1,JX,2))
                  CALL COPY (NC, FLGIT(1,JX,1), FLGIT(1,JX,3))
                  CALL COPY (NC, FLGIT(1,JX,1), FLGIT(1,JX,4))
 10               CONTINUE
C                                       are there any data
C                                       mark already flagged
               INONE = .FALSE.
               DO 15 JF = 1,NC
                  IF (CHFLGD(JF,JI).LE.0) THEN
                     DO 14 JP = 1,NSTOK
                        DO 13 JT = 1,NCT
                           JX = TNUM(JT)
                           WT = B(3,JF,JX,JB,JI,JP)
                           IF (WT.LE.0.0) THEN
                              IF (FLGIT(JF,JX,JP).EQ.0)
     *                           FLGIT(JF,JX,JP) = 1
                           ELSE
                              INONE = .TRUE.
                              END IF
 13                        CONTINUE
 14                     CONTINUE
                     END IF
 15               CONTINUE
               IF (.NOT.INONE) GO TO 180
C                                       pre-clip
               IF (FPARM(13).LE.1.E14) THEN
                  DO 45 JT = 1,NCT
                     JX = TNUM(JT)
                     DO 40 JF = 1,NC
                        IF (CHFLGD(JF,JI).LE.0) THEN
                           DO 35 JP = 1,NSTOK
                              WT = B(3,JF,JX,JB,JI,JP)
                              IF (WT.GT.0.0) THEN
                                 V1 = B(1,JF,JX,JB,JI,JP)
                                 V2 = B(2,JF,JX,JB,JI,JP)
                                 SR = V1*V1 + V2*V2
                                 IF (SR.GT.FPARM(13)) THEN
                                    B(3,JF,JX,JB,JI,JP) =
     *                                 -ABS (B(3,JF,JX,JB,JI,JP))
                                    FLGIT(JF,JX,JP) = 2
                                    CNT(7) = CNT(7) + 1
                                    END IF
                                 END IF
 35                              CONTINUE
                           END IF
 40                     CONTINUE
 45                  CONTINUE
                  END IF
C                                       time rms
               IF (DOTIME) THEN
                  JLIM = 0
                  DO 75 JF = 1,NC
                     IF (CHFLGD(JF,JI).LE.0) THEN
                        JLIM = JLIM + 1
                        DO 70 JP = 1,NSTOK
                           SR = 0.0D0
                           SSR = 0.0D0
                           WR = 0.0D0
                           LR = 0
                           SI = 0.0D0
                           SSI = 0.0D0
                           DO 55 JT = 1,NCT
                              WT = B(3,JF,JT,JB,JI,JP)
                              IF (WT.GT.0.0) THEN
                                 V = B(1,JF,JT,JB,JI,JP)
                                 SR = SR + V * WT
                                 SSR = SSR + V * V * WT
                                 WR = WR + WT
                                 LR = LR + 1
                                 V = B(2,JF,JT,JB,JI,JP)
                                 SI = SI + V * WT
                                 SSI = SSI + V * V * WT
                                 END IF
 55                           CONTINUE
                           IF (WR.GT.0.0D0) THEN
                              AVR = SR / WR
                              SSR = SSR / WR
                              RMSR = SSR - AVR * AVR
                              RMSR = SQRT (MAX (0.0, RMSR))
                              AVI = SI / WR
                              SSI = SSI / WR
                              RMSI = SSI - AVI * AVI
                              RMSI = SQRT (MAX (0.0, RMSI))
                              RMSR = SQRT (RMSR*RMSR + RMSI*RMSI)
                              IF (RMSR.GT.NOISE(JIB)) THEN
                                 DO 60 JJ = J1,J2
                                    JX = TNUM(JJ)
                                    IF (FLGIT(JF,JX,JP).EQ.0)
     *                                 FLGIT(JF,JX,JP) = 2
 60                                 CONTINUE
                                 END IF
                           ELSE
                              DO 65 JJ = J1,J2
                                 JX = TNUM(JJ)
                                 IF (FLGIT(JF,JX,JP).EQ.0)
     *                              FLGIT(JF,JX,JP) = 2
 65                              CONTINUE
                              END IF
 70                        CONTINUE
                        END IF
 75                  CONTINUE
                  JLIM = JLIM * FPARM(7) + 0.75
               ELSE
                  JLIM = NC
                  END IF
C                                       single time spectral rms
               IF (DOSPEC) THEN
                  DO 160 JJ = J3,J4
                     JX = TNUM(JJ)
                     DO 150 JP = 1,NSTOK
C                                       median
                        JC1 = 1
 100                    JC2 = JC1 + AVGCHN - 1
                        IF ((JC1.LE.NC) .AND. (JC2.GT.NC)) THEN
                           JC2 = NC
                           JC1 = JC2 - AVGCHN + 1
                           END IF
                        IF (JC2.LE.NC) THEN
                           JF1 = (JC1 + JC2 - XTRCHN) / 2
                           JF2 = JF1 + XTRCHN
                           IF (JC1.EQ.1) JF1 = 1
                           IF (JC2.EQ.NC) JF2 = NC
                           L = 0
                           DO 110 JF = JC1,JC2
                              IF ((CHFLGS(JF,JI).GT.0) .AND.
     *                           (CHFLGD(JF,JI).LE.0) .AND.
     *                           (FLGIT(JF,JX,JP).EQ.0)) THEN
                                 WT = B(3,JF,JX,JB,JI,JP)
                                 IF (WT.GT.0.0) THEN
                                    L = L + 1
                                    VM(L,1) = B(1,JF,JX,JB,JI,JP)
                                    VM(L,2) = B(2,JF,JX,JB,JI,JP)
                                    END IF
                                 END IF
 110                          CONTINUE
                           IF (MOD(L,2).EQ.0) L = L - 1
                           IF (L.GT.0) THEN
                              AVR = MEDIAN (L, VM(1,1))
                              AVI = MEDIAN (L, VM(1,2))
C                                       go through the data
                              DO 120 JF = JF1,JF2
                                 WT = B(3,JF,JX,JB,JI,JP)
                                 IF ((WT.GT.0.0) .AND.
     *                              (FLGIT(JF,JX,JP).EQ.0)) THEN
                                    V = B(1,JF,JX,JB,JI,JP)
                                    TEMP = ABS (V-AVR)
                                    IF (TEMP.GT.SCUTOF(JI))
     *                                 FLGIT(JF,JX,JP) = 2
                                    V = B(2,JF,JX,JB,JI,JP)
                                    TEMP = ABS (V-AVI)
                                    IF (TEMP.GT.SCUTOF(JI))
     *                                 FLGIT(JF,JX,JP) = 2
                                    END IF
 120                             CONTINUE
                           ELSE
                              DO 130 JF = JF1,JF2
                                 IF ((CHFLGS(JF,JI).GT.0) .AND.
     *                              (FLGIT(JF,JX,JP).EQ.0)) THEN
                                    FLGIT(JF,JX,JP) = 2
                                    END IF
 130                             CONTINUE
                              END IF
                           JC1 = JC1 + 1 + XTRCHN
                           IF (JC2.LT.NC) GO TO 100
                           END IF
 150                    CONTINUE
 160                 CONTINUE
                  END IF
C                                       compress Stokes
               DO 175 JJ = J1,J2
                  JX = TNUM(JJ)
                  DO 170 JF = 1,NC
                     DO 165 JP = 2,NSTOK
                        FLGIT(JF,JX,1) = MAX (FLGIT(JF,JX,1),
     *                     FLGIT(JF,JX,JP))
 165                    CONTINUE
                     FLGITS(JF,JB,JI,JX) = FLGIT(JF,JX,1)
 170                 CONTINUE
 175              CONTINUE
 180           CONTINUE
 190        CONTINUE
 200     CONTINUE
C                                       now look at FLGITS
      DO 260 JA1 = 1,NA
         DO 255 JA2 = JA1,NA
            JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
            DO 250 JI = 1,NI
               DO 245 JJ = J3,J4
                  JX = TNUM(JJ)
                  JCNT = 0
                  DO 201 JF = 1,NC
                     IF (FLGITS(JF,JB,JI,JX).EQ.2) JCNT = JCNT + 1
 201                 CONTINUE
C                                       grow each region
                  IF ((NGROW.GT.0) .AND. (JCNT.GT.0)) THEN
                     CALL COPY (NC, FLGITS(1,JB,JI,JX), FLGIT)
                     DO 210 JF = 1,NC
C                                       look down
                        IF (FLGIT(JF,1,1).EQ.0) THEN
                           DO 204 K = 1,NGROW
                              IF ((JF-K.GE.1) .AND.
     *                           (FLGIT(JF-K,1,1).EQ.2)) THEN
                                 CNT(9) = CNT(9) + 1
                                 FLGITS(JF,JB,JI,JX) = 2
                                 GO TO 205
                                 END IF
 204                          CONTINUE
                           END IF
C                                       loop up
 205                    IF (FLGIT(JF,1,1).EQ.0) THEN
                           DO 209 K = 1,NGROW
                              IF ((JF+K.LE.NC) .AND.
     *                           (FLGIT(JF+K,1,1).EQ.2)) THEN
                                 CNT(9) = CNT(9) + 1
                                 FLGITS(JF,JB,JI,JX) = 2
                                 GO TO 210
                                 END IF
 209                          CONTINUE
                           END IF
 210                    CONTINUE
                     END IF
C                                       extend flags between
                  IF ((NEXT.GT.0) .AND. (JCNT.GT.0)) THEN
C                                       extend at beginning?
                     IF (FLGITS(1,JB,JI,JX).LE.0) THEN
                        DO 215 NX = 1,NEXT
                           IF (FLGITS(1+NX,JB,JI,JX).GT.1) THEN
                              CALL FILL (NX, 2, FLGITS(1,JB,JI,JX))
                              CNT(4) = CNT(4) + NX
                              GO TO 216
                              END IF
 215                       CONTINUE
                        END IF
C                                       look through channels
 216                 INONE = .FALSE.
                     DO 230 JF = 1,NC
C                                       extend?
                        IF ((INONE) .AND. (FLGITS(JF,JB,JI,JX).LE.0))
     *                     THEN
                           JXX = MIN (NC - JF, NEXT)
                           DO 220 NX = 1,JXX
                              IF (FLGITS(JF+NX,JB,JI,JX).GT.1) THEN
                                 CALL FILL (NX, 2, FLGITS(JF,JB,JI,JX))
                                 CNT(4) = CNT(4) + NX
                                 GO TO 225
                                 END IF
 220                          CONTINUE
C                                       expand to end
                           IF (JXX.LT.NEXT) THEN
                              CALL FILL (JXX+1, 2, FLGITS(JF,JB,JI,JX))
                              CNT(4) = CNT(4) + JXX + 1
                              END IF
                           END IF
 225                    IF (FLGITS(JF,JB,JI,JX).GT.0) THEN
                           IF ((.NOT.INONE) .AND.
     *                        (FLGITS(JF,JB,JI,JX).EQ.2)) INONE = .TRUE.
                        ELSE
                           INONE = .FALSE.
                           END IF
 230                    CONTINUE
                     END IF
C                                       count flags
                  JCNT = 0
                  K = 0
                  DO 240 JF = 1,NC
                     IF (FLGITS(JF,JB,JI,JX).GT.0) JCNT = JCNT + 1
                     IF (FLGITS(JF,JB,JI,JX).GT.1) K = K + 1
 240                 CONTINUE
                  IF (K.EQ.0) JCNT = 0
C                                       too many do all
                  IF (JCNT.GE.JLIM)
     *               CALL FILL (NC, 2, FLGITS(1,JB,JI,JX))
 245              CONTINUE
 250           CONTINUE
 255        CONTINUE
 260     CONTINUE
C                                       excess fraction of baselines
C                                       at a particular channel
      IF (FPARM(11).LT.1.0) THEN
         DO 340 JI = 1,NI
            DO 330 JF = 1,NC
               DO 320 JJ = J3,J4
                  JX = TNUM(JJ)
                  L = 0
                  K = 0
                  INONE = .FALSE.
                  DO 310 JA1 = 1,NA
                     DO 305 JA2 = JA1,NA
                        JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
                        IF (FLGITS(JF,JB,JI,JX).GT.0) L = L + 1
                        IF (FLGITS(JF,JB,JI,JX).GT.1) INONE = .TRUE.
                        K = K + 1
 305                    CONTINUE
 310                 CONTINUE
                  IF ((FPARM(11)*K.LT.L) .AND. (INONE) .AND.
     *               (L.LT.K)) THEN
                     CNT(5) = CNT(5) + 1
                     DO 315 JB = 1,NB
                        IF (FLGITS(JF,JB,JI,JX).EQ.0)
     *                     FLGITS(JF,JB,JI,JX) = 2
 315                    CONTINUE
                     END IF
 320              CONTINUE
 330           CONTINUE
 340        CONTINUE
         END IF
C                                       excess fraction of baselines
C                                       at a particular channel, antenna
      IF (FPARM(12).LT.1.0) THEN
         DO 450 JI = 1,NI
            DO 440 JF = 1,NC
               DO 430 JJ = J3,J4
                  JX = TNUM(JJ)
                  DO 420 JA1 = 1,NA
                     L = 0
                     K = 0
                     INONE = .FALSE.
                     DO 405 JA2 = 1,NA
                        IF (JA2.LT.JA1) THEN
                           JB = NA * (JA2-1) - (JA2*(JA2-1))/2 + JA1
                        ELSE
                           JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
                           END IF
                        IF (FLGITS(JF,JB,JI,JX).GT.0) L = L + 1
                        IF (FLGITS(JF,JB,JI,JX).GT.1) INONE = .TRUE.
                        K = K + 1
 405                    CONTINUE
                     IF ((FPARM(12)*K.LT.L) .AND. (INONE) .AND.
     *                  (L.LT.K)) THEN
                        CNT(6) = CNT(6) + 1
                        DO 415 JA2 = 1,NA
                           IF (JA2.LT.JA1) THEN
                              JB = NA * (JA2-1) - (JA2*(JA2-1))/2 + JA1
                           ELSE
                              JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
                              END IF
                           IF (FLGITS(JF,JB,JI,JX).EQ.0)
     *                        FLGITS(JF,JB,JI,JX) = 2
 415                       CONTINUE
                        END IF
 420                 CONTINUE
 430              CONTINUE
 440           CONTINUE
 450        CONTINUE
         END IF
C                                       now do flags - one time at a time
      DO 600 JJ = J3,J4
         JX = TNUM(JJ)
         DO 590 JA1 = 1,NA
            DO 580 JA2 = JA1,NA
               JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
               DO 570 JI = 1,NI
                  JIB = JI + LBIF - 1
C                                       take from big buffer
                  DO 504 JF = 1,NC
                     FLGIT(JF,JX,1) = FLGITS(JF,JB,JI,JX)
 504                 CONTINUE
C                                       flag
                  BTIME = TIMES(1,JX) - EPS
                  ETIME = TIMES(2,JX) + EPS
C                                       count flags
                  JCNT = 0
                  K = 0
                  DO 525 JF = 1,NC
                     IF (FLGIT(JF,JX,1).GT.0) JCNT = JCNT + 1
                     IF (FLGIT(JF,JX,1).GT.1) K = K + 1
 525                 CONTINUE
                  IF (K.EQ.0) JCNT = 0
C                                       too many do all
                  IF (JCNT.GE.JLIM) THEN
                     CHNS(1) = 1
                     CHNS(2) = NC
                     CALL FLAGIT ('FLAG', FGLUN, DISKIN, OLDCNO, FGVERI,
     *                  FGVERT, LFGRNO, FGKOLS, FGNUMV, SUBARR, FRQSEL,
     *                  JA1, JA2, BTIME, ETIME, JIB, CHNS, REASON,
     *                  CATUV, FGBUFF, SORTED, FGERR)
                     IF (FGERR.LE.0) CNT(2) = CNT(2) + 1
                     IF (LFGRNO.GT.LIMIT) GO TO 890
                  ELSE IF (JCNT.GT.0) THEN
                     INONE = .FALSE.
C                                       look through channels
                     CHNS(1) = NC + 1
                     DO 560 JF = 1,NC
                        IF (FLGIT(JF,JX,1).GT.0) THEN
                           IF (.NOT.INONE) THEN
                              CHNS(1) = MIN (JF, CHNS(1))
                              CHNS(2) = JF
                              IF (FLGIT(JF,JX,1).EQ.2) INONE = .TRUE.
                           ELSE
                              CHNS(2) = JF
                              END IF
C                                       write a flag
                        ELSE
                           IF (INONE) THEN
                              INONE = .FALSE.
                              CALL FLAGIT ('FLAG', FGLUN, DISKIN,
     *                           OLDCNO, FGVERI, FGVERT, LFGRNO, FGKOLS,
     *                           FGNUMV, SUBARR, FRQSEL, JA1, JA2,
     *                           BTIME, ETIME, JIB, CHNS, REASON, CATUV,
     *                           FGBUFF, SORTED, FGERR)
                              IF (FGERR.LE.0) CNT(3) = CNT(3) + 1
                              IF (LFGRNO.GT.LIMIT) GO TO 890
                              END IF
                           CHNS(1) = NC + 1
                           END IF
 560                    CONTINUE
                     IF (INONE) THEN
                        CALL FLAGIT ('FLAG', FGLUN, DISKIN, OLDCNO,
     *                     FGVERI, FGVERT, LFGRNO, FGKOLS, FGNUMV,
     *                     SUBARR, FRQSEL, JA1, JA2, BTIME, ETIME,
     *                     JIB, CHNS, REASON, CATUV, FGBUFF, SORTED,
     *                     FGERR)
                        IF (FGERR.LE.0) CNT(3) = CNT(3) + 1
                        IF (LFGRNO.GT.LIMIT) GO TO 890
                        END IF
                     END IF
 570              CONTINUE
 580           CONTINUE
 590        CONTINUE
 600     CONTINUE
      GO TO 999
C                                       too many records
 890  WRITE (MSGTXT,1890) LFGRNO
      CALL MSGWRT (8)
C                                       close down
 900  CALL FLAGIT ('CLOS', FGLUN, DISKIN, OLDCNO, FGVERI, FGVERT,
     *   LFGRNO, FGKOLS, FGNUMV, SUBARR, FRQSEL, JA1, JA2, BTIME, ETIME,
     *   JIB, CHNS, REASON, CATUV, FGBUFF, SORTED, FGERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1890 FORMAT ('TOO MANY FG RECORDS',I11,' FLAGGING WAY TOO MUCH')
      END
      SUBROUTINE RFLAFB (NC, NT, NA, NB, NI, RFSCAL, B, FLGITS, IRET)
C-----------------------------------------------------------------------
C   RFLAFB accumulates a buffer of NT times for all baselines, etc.
C   It then finds the rms in the buffer for each baseline, stokes, IF
C   and generates flags based on them.  It can also fit a robust mean
C   and rms to each spectrum and flag outliers.  All the real work is
C   done in subroutines, primarily RWRMSF.
C   Input in common:
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C   Inputs:
C      NC      I      Number spectral channels
C      NT      I      Number times to accumulate
C      NB      I      Max baseline number
C      NI      I      Max IF in data
C   Output:
C      B       R(*)   Big buffer to accumulate the vis
C      FLGITS  I(*)   Big buffer to accumulate flag info
C      IRET    I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NC, NT, NA, NB, NI, FLGITS(NC,NB,NI,NT), IRET
      REAL      B(3,NC,NT,NB,NI,*), RFSCAL(NI,NA,NA,*)
C
      INTEGER   NTMAX
      PARAMETER (NTMAX = 99)
      INCLUDE 'RFLAG.INC'
      INTEGER   NUMVIS, CATMP(256), CHFLGS(MAXCIF), NW, NVM, NTIMES, I,
     *   TNUM(NTMAX), VISN(2,NTMAX), LSOU, ISOU, NCT, CT, J1, J2, JT,
     *   NRBL(MAXANT,MAXANT), CHFLGD(MAXCIF), FGBUFF(512), J3, J4, NF,
     *   ISONE(MXBASE*NTMAX), LIMIT
      LOGICAL   END, FIRST, GOTONE, GETNEW, LDUM
      REAL      VIS(3,UVBFSS/3), RPARM(20), TB, TE,  TIME,
     *   TIMES(2,NTMAX), TLIMIT, BLENG, TINT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (VIS, BUFF1)
C-----------------------------------------------------------------------
      NVM = NC * NT
      IF (FPARM(2).LE.0.0) FPARM(2) = 10.
      TLIMIT = 2.01 * FPARM(1) * FPARM(2)
      TLIMIT = TLIMIT / (24. * 3600.) * NTAVG
      TINT = 1.001 * FPARM(2) / (24. * 3600.) * NTAVG
      LIMIT = 2147480000
C                                       noise cutoffs
C                                       may have just been set
      IF (FPARM(3).GE.0.0) THEN
         IF (FPARM(3).EQ.0.0) FPARM(3) = 1.E6
         CALL RFILL (64, FPARM(3), NOISE)
         DOTIME = NOISE(1).LT.1.0E6
      ELSE
         CALL RCOPY (64, XNOISE, NOISE)
         DOTIME = .FALSE.
         DO 10 I = 1,64
            IF (NOISE(I).LE.0.0) NOISE(I) = 1.E6
            IF ((NOISE(I).LT.1.E6) .AND. (I.GE.BIF) .AND. (I.LE.EIF))
     *         DOTIME = .TRUE.
 10         CONTINUE
         END IF
      IF (FPARM(4).GE.0.0) THEN
         IF (FPARM(4).EQ.0.0) FPARM(4) = 1.E6
         CALL RFILL (64, FPARM(4), SCUTOF)
      ELSE
         CALL RCOPY (64, XSCUT, SCUTOF)
         DO 20 I = 1,64
            IF (SCUTOF(I).LE.0.0) SCUTOF(I) = 1.E6
 20         CONTINUE
         END IF
C                                       counters, mask
      NCHAN = CATBLK(KINAX+JLOCF)
      CALL CHWANT (NCHAN, LIF, CHNSEL(1,1,BIF), CHFLGS)
      CALL CHWANT (NCHAN, LIF, CHNDSL(1,1,EIF), CHFLGD)
      COUNT(1) = 0
      COUNT(2) = 0
      COUNT(3) = 0
      COUNT(4) = 0
      COUNT(5) = 0
      COUNT(6) = 0
      COUNT(7) = 0
      COUNT(8) = 0
      COUNT(9) = 0
      COUNT(10) = 0
      COUNT(11) = 0
      NTIMES = 0
      I = MAXANT * MAXANT
      CALL FILL (I, 0, NRBL)
      I = MXBASE * NTMAX
      CALL FILL (I, 0, ISONE)
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN/INIT INPUT VIS FILE'
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
      NUMVIS = 0
      TB = -1000.0
      TE = TB
      NW = 3 * NC * NT * NB * NI * NSTOK
      CALL RFILL (NW, 0.0, B)
      LSOU = -1
      CT = 0
      NCT = 0
      FIRST = .TRUE.
      GOTONE = .FALSE.
      MSGTXT = 'Reading data to generate flags'
      CALL MSGWRT (2)
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      END = IRET.LT.0
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING DATA FIRST PASS'
         GO TO 990
C                                       got good data, now what
      ELSE IF (IRET.LE.0) THEN
C                                       in current time
         IF (END) THEN
            TIME = 1.E4
         ELSE
            CALL UVSCAL (NI, NA, RFSCAL, RPARM, VIS)
            NUMVIS = NUMVIS + 1
            IF (MOD(NUMVIS,50000).EQ.0) THEN
               NF = COUNT(2) + COUNT(3)
               WRITE (MSGTXT,1100) NUMVIS, NF
               CALL MSGWRT (2)
               END IF
            BLENG = RPARM(1+ILOCU)*RPARM(1+ILOCU) +
     *         RPARM(1+ILOCV)*RPARM(1+ILOCV)
            IF ((BLENG.LT.UVRANG(1)) .OR. (BLENG.GT.UVRANG(2)))
     *         GO TO 100
            TIME = RPARM(1+ILOCT)
            END IF
         ISOU = 1
         IF (ILOCSU.GE.0) ISOU = RPARM(1+ILOCSU) + 0.01
         IF (END) ISOU = -1
C                                       in current time/source
         IF ((ABS(TB-TIME).LT.TINT) .AND. (ISOU.EQ.LSOU)) THEN
            CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B, ISONE,
     *         GETNEW)
            IF (.NOT.GETNEW) THEN
               GOTONE = .TRUE.
               VISN(2,CT) = NUMVIS
               TIMES(1,CT) = MIN (TIMES(1,CT), TIME)
               TIMES(2,CT) = MAX (TIMES(2,CT), TIME)
               TB = TIMES(1,CT)
               END IF
         ELSE
            GETNEW = .TRUE.
            END IF
C                                       need new time bin
         IF (GETNEW) THEN
C                                       average this time before
C                                       changing it
            IF (CT.GT.0) CALL RWTAVG (NC, NT, NB, NI, CT, B)
C                                       just advance time counter
            IF ((NCT.LT.NT) .AND. (ISOU.EQ.LSOU) .AND.
     *         (TIME-TB.LT.TLIMIT)) THEN
               NCT = NCT + 1
               TNUM(NCT) = NCT
               CT = NCT
               JT = NB * (CT -1) + 1
               CALL FILL (NB, 0, ISONE(JT))
               JT = NC * NB * NI
               CALL FILL (JT, 0, FLGITS(1,1,1,CT))
               TIMES(1,NCT) = TIME
               TIMES(2,NCT) = TIME
               VISN(1,CT) = NUMVIS
               VISN(2,CT) = NUMVIS
               CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B, ISONE,
     *            LDUM)
               GOTONE = .TRUE.
               TB = TIME
C                                       still going, clear 1 or more
            ELSE IF ((ISOU.EQ.LSOU) .AND. (TIME-TB.LT.TLIMIT)) THEN
               J1 = (NT+1)/2
               J2 = J1
               IF (FIRST) J1 = 1
               J3 = 1
               J4 = 1
               IF (FPARM(20).LT.1.0) THEN
                  J1 = 1
                  J2 = NT
                  END IF
               FIRST = .FALSE.
               CALL RWRMSF (NC, NT, NA, NB, NI, NCT, J1, J2, J3, J4,
     *            TNUM, TIMES, CHFLGS, CHFLGD, SUBARR, FRQSEL, FGBUFF,
     *            CATUV, B, FLGITS, COUNT)
               IF (LFGRNO.GT.LIMIT) GO TO 890
C                                       next
               CT = TNUM(1)
               DO 150 JT = 1,NT-1
                  TNUM(JT) = TNUM(JT+1)
 150              CONTINUE
               JT = NC * NB * NI
               CALL FILL (JT, 0, FLGITS(1,1,1,CT))
               JT = NB * (CT -1) + 1
               CALL FILL (NB, 0, ISONE(JT))
               TNUM(NT) = CT
               TIMES(1,CT) = TIME
               TIMES(2,CT) = TIME
               VISN(1,CT) = NUMVIS
               VISN(2,CT) = NUMVIS
               CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B, ISONE,
     *            LDUM)
               GOTONE = .TRUE.
               TB = TIME
C                                       done with this scan
            ELSE
               IF (GOTONE) THEN
                  J1 = (NT+1)/2
                  J2 = NCT
                  IF (FIRST) J1 = 1
                  J3 = 1
                  J4 = NCT
                  IF (FPARM(20).LT.1.0) J1 = 1
                  CALL RWRMSF (NC, NT, NA, NB, NI, NCT, J1, J2, J3, J4,
     *               TNUM, TIMES, CHFLGS, CHFLGD, SUBARR, FRQSEL,
     *               FGBUFF, CATUV, B, FLGITS, COUNT)
                  IF (LFGRNO.GT.LIMIT) GO TO 890
                  END IF
               IF (.NOT.END) THEN
                  CALL RFILL (NW, 0.0, B)
                  FIRST = .TRUE.
                  CT = 1
                  CALL FILL (NB, 0, ISONE)
                  JT = NC * NB * NI
                  CALL FILL (JT, 0, FLGITS(1,1,1,CT))
                  NCT = 1
                  TNUM(1) = 1
                  TIMES(1,CT) = TIME
                  TIMES(2,CT) = TIME
                  VISN(1,CT) = NUMVIS
                  VISN(2,CT) = NUMVIS
                  CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B, ISONE,
     *               LDUM)
                  TB = TIME
                  LSOU = ISOU
                  GOTONE = .TRUE.
                  END IF
               END IF
            END IF
         IF (.NOT.END) GO TO 100
         END IF
C                                       close uv data set
 890  CALL UVGET ('CLOS', RPARM, VIS, IRET)
      J1 = 0
      J2 = 0
      J3 = J1
      J4 = J2
      CALL RWRMSF (NC, NT, NA, NB, NI, NCT, J1, J2, J3, J4, TNUM, TIMES,
     *   CHFLGS, CHFLGD, SUBARR, FRQSEL, FGBUFF, CATUV, B, FLGITS,
     *   COUNT)
      IRET = 0
C
      IF (COUNT(1).GT.0) THEN
         WRITE (MSGTXT,1240) COUNT(2)
         CALL MSGWRT (4)
         WRITE (MSGTXT,1241) COUNT(3)
         CALL MSGWRT (4)
         WRITE (MSGTXT,1242) COUNT(4)
         IF (COUNT(4).GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1243) COUNT(5)
         IF (COUNT(5).GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1244) COUNT(6)
         IF (COUNT(6).GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1245) COUNT(7)
         IF (COUNT(7).GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1246) COUNT(8)
         IF (COUNT(8).GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1247) COUNT(9)
         IF (COUNT(9).GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1248) COUNT(10)
         IF (COUNT(10).GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1249) COUNT(11)
         IF (COUNT(11).GT.0) CALL MSGWRT (4)
      ELSE
         MSGTXT = 'NO SAMPLES EXAMINED'
         CALL MSGWRT (7)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RFLAFB: ERROR',I3,' ON ',A)
 1100 FORMAT ('RFLAFB: on visibility number',I11,'   New flags',I11)
 1240 FORMAT (I12,' full-spectrum flag records written')
 1241 FORMAT (I12,' partial-spectrum flag records written')
 1242 FORMAT (I12,' channels flagged between flagged groups')
 1243 FORMAT (I12,' channels flagged in all baselines due to FPARM(11)')
 1244 FORMAT (I12,' channels flagged in all BL to ant due to FPARM(12)')
 1245 FORMAT (I12,' channels pre-clipped due to FPARM(13)')
 1246 FORMAT (I12,' channels pre-quacked due to FPARM(14)')
 1247 FORMAT (I12,' channels over-run due to FPARM(6)')
 1248 FORMAT (I12,' IFs * BLs flagged due to FPARM(15)')
 1249 FORMAT (I12,' IFs * BLs flagged due to FPARM(16)')
      END
      SUBROUTINE FLAGIT (OPCODE, LUN, DISK, CNO, VERI, VER, LFGRNO,
     *   FGKOLS, FGNUMV, SUBA, FQID, ANT1, ANT2, BTIME, ETIME, IFNUM,
     *   CHANS, REASON, CATUV, BUFF, SORTED, IRET)
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
C      NID      I        Number of elements in ID
C      SUBA     I        Subarray number.
C      FQID     I        Freqid number
C      ANT1     I        First antenna number in baseline
C      ANT2     I        Second antenna number in baseline
C      BTIME    R        Start time of data to be flagged (Days)
C      ETIME    R        End time of data to be flagged (Days)
C      IFNUM    I        IF number to flag
C      POLNUM   I        Polarization number to flag 1-4
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
C      SORTED   L        True => is in time order
C   In/Out:
C      IRET     I        Error code: > 0 on input -> return
C                        0=>OK else TABIO error.
C                        Note: -1 => read, but record deselected.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, REASON*24
      INTEGER   LUN, DISK, CNO, VERI, VER, LFGRNO, FGKOLS(*), FGNUMV(*),
     *   SUBA, FQID, ANT1, ANT2, IFNUM, CHANS(2), CATUV(256),
     *   BUFF(*), IRET
      REAL      BTIME, ETIME
      LOGICAL   SORTED
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER TREAS*24, CTEMP*12
      INTEGER   IDT, SUBT, ANTS(2), IFS(2), IDUM, FIND, IFGKOL(MAXFGC),
     *   IFGNUM(MAXFGC), ID
      LOGICAL   PFLAGS(4), TFLAGS(4), FIRST
      REAL      TIMER(2), LTIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'RFOLDFG.INC'
      SAVE FIRST, LTIME, IFGKOL, IFGNUM
      DATA FIRST, LTIME /.TRUE., -1000.0/
C-----------------------------------------------------------------------
C                                       See if table open - check FTAB
      IF (OPCODE.NE.'CLOS') THEN
         IF (IRET.GT.0) GO TO 999
         FIND = BUFF(82)
C                                       Open file
         IF ((FIND.LT.0) .OR. (FIND.GT.10000) .OR. (LUN.NE.FTAB(FIND)))
     *      THEN
            CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV',
     *         IDUM, 'CLRD', BUFF, IRET)
            CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV',
     *         IDUM, 'WRIT', BUFF, IRET)
C
            CALL FLGINI ('WRIT', BUFF, DISK, CNO, VER, CATUV, LUN,
     *         LFGRNO, FGKOLS, FGNUMV, IRET)
C                                       Report on the need for flagging
            WRITE (MSGTXT,1000) VER
            IF (.NOT.FIRST) WRITE (MSGTXT,1001) VER
            CALL MSGWRT (2)
            IF (IRET.NE.0) GO TO 999
            LTIME = -1000.
C                                       Copy the old file
            IF ((FIRST) .AND. (VERI.GT.0)) THEN
               OFGLUN = LUN + 1
               CALL FLGINI ('READ', OFGBUF, DISK, CNO, VERI, CATUV,
     *            OFGLUN, OFGRNO, IFGKOL, IFGNUM, IRET)
               IF (IRET.NE.0) GO TO 999
               OFGREC = OFGBUF(5)
               WRITE (MSGTXT,1002) OFGREC, VERI, VER
               CALL MSGWRT (2)
 10            IF (OFGRNO.LE.OFGREC) THEN
                  CALL TABFLG ('READ', OFGBUF, OFGRNO, IFGKOL, IFGNUM,
     *               OFGSOU, OFGSUB, OFGFQ, OFGANT, OFGTIM, OFGIFS,
     *               OFGCHN, OFGFLG, OFGREA, IRET)
                  IF (IRET.GT.0) GO TO 999
                  IF (IRET.LT.0) GO TO 10
                  END IF
            ELSE
               OFGREC = 0
               OFGLUN = 0
               IF (FIRST) THEN
                  MSGTXT = 'NOT COPYING OLD FG TO NEW ONE'
               ELSE
                  MSGTXT =
     *               'RE-OPENING OUTPUT FG: SORT ORDER LIKELY LOST'
                  END IF
               CALL MSGWRT (6)
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
         IFS(1) = IFNUM
         IFS(2) = IFNUM
         PFLAGS(1) = .TRUE.
         PFLAGS(2) = .TRUE.
         PFLAGS(3) = .TRUE.
         PFLAGS(4) = .TRUE.
         ID = 0
C                                       write input record first
 20      IF ((OFGREC.GT.0) .AND. (OFGTIM(1).LE.TIMER(1))) THEN
            CALL TABFLG ('WRIT', BUFF, LFGRNO, FGKOLS, FGNUMV, OFGSOU,
     *         OFGSUB, OFGFQ, OFGANT, OFGTIM, OFGIFS, OFGCHN, OFGFLG,
     *         OFGREA, IRET)
            IF (IRET.GT.0) GO TO 999
            IF (SORTED) THEN
               IF (OFGTIM(1).LT.LTIME) SORTED = .FALSE.
               LTIME = OFGTIM(1)
               END IF
 25         IF (OFGRNO.LE.OFGREC) THEN
               CALL TABFLG ('READ', OFGBUF, OFGRNO, IFGKOL, IFGNUM,
     *            OFGSOU, OFGSUB, OFGFQ, OFGANT, OFGTIM, OFGIFS, OFGCHN,
     *            OFGFLG, OFGREA, IRET)
               IF (IRET.GT.0) GO TO 999
               IF (IRET.LT.0) GO TO 25
               GO TO 20
            ELSE
               CALL TABFLG ('CLOS', OFGBUF, OFGRNO, IFGKOL, IFGNUM,
     *            OFGSOU, OFGSUB, OFGFQ, OFGANT, OFGTIM, OFGIFS, OFGCHN,
     *            OFGFLG, OFGREA, IRET)
               IF (IRET.GT.0) GO TO 999
               OFGREC = 0
               END IF
            END IF
C                                       Flag table entry.
         CALL TABFLG ('WRIT', BUFF, LFGRNO, FGKOLS, FGNUMV, ID, SUBA,
     *      FQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
         IF (SORTED) THEN
            IF (TIMER(1).LT.LTIME) SORTED = .FALSE.
            LTIME = TIMER(1)
            END IF
C                                       Close
      ELSE IF (.NOT.FIRST) THEN
C                                       input FG has some left
 120     IF (OFGREC.GT.0) THEN
            CALL TABFLG ('WRIT', BUFF, LFGRNO, FGKOLS, FGNUMV, OFGSOU,
     *         OFGSUB, OFGFQ, OFGANT, OFGTIM, OFGIFS, OFGCHN, OFGFLG,
     *         OFGREA, IRET)
            IF (IRET.GT.0) GO TO 999
            IF (SORTED) THEN
               IF (OFGTIM(1).LT.LTIME) SORTED = .FALSE.
               LTIME = OFGTIM(1)
               END IF
 125        IF (OFGRNO.LE.OFGREC) THEN
               CALL TABFLG ('READ', OFGBUF, OFGRNO, IFGKOL, IFGNUM,
     *            OFGSOU, OFGSUB, OFGFQ, OFGANT, OFGTIM, OFGIFS, OFGCHN,
     *            OFGFLG, OFGREA, IRET)
               IF (IRET.GT.0) GO TO 999
               IF (IRET.LT.0) GO TO 125
               GO TO 120
            ELSE
               CALL TABFLG ('CLOS', OFGBUF, OFGRNO, IFGKOL, IFGNUM,
     *            OFGSOU, OFGSUB, OFGFQ, OFGANT, OFGTIM, OFGIFS, OFGCHN,
     *            OFGFLG, OFGREA, IRET)
               IF (IRET.GT.0) GO TO 999
               OFGREC = 0
               END IF
            END IF
         IF (SORTED) THEN
            BUFF(43) = 5
            BUFF(44) = 0
            END IF
         CALL TABFLG ('CLOS', BUFF, LFGRNO, FGKOLS, FGNUMV, IDT, SUBT,
     *      FQID, ANTS, TIMER, IFS, CHANS, TFLAGS, TREAS, IRET)
C                                       Clear write status
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV', IDUM,
     *      'CLWR', BUFF, IRET)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Found some bad data, will write flags to table FG', I4)
 1001 FORMAT ('Found some bad data, will add   flags to table FG', I4)
 1002 FORMAT ('Merge',I8,' rows from FG vers',I3,' to',I3)
      END
      SUBROUTINE RFLAFP (NC, NT, NA, NB, NI, RFSCAL, B, IRET)
C-----------------------------------------------------------------------
C   RFLAFP accumulates a buffer of NT times for all baselines, etc.
C   It then finds the rms over time for each channel, IF, stokes and
C   accumulates a histogram.  It can also look at each time, fit a
C   robust mean and rms and then accumulate a histogram of the variation
C   of the data from that fit.  At the end, the fits are plotted.
C   Inputs:
C      NC      I      Number spectral channels
C      NT      I      Number times to accumulate
C      NA      I      Number antennas
C      NB      I      Max baseline number
C      NI      I      Max IF in data
C   Output:
C      B       R(*)   Big buffer to accumulate the vis
C      IRET    I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NC, NT, NA, NB, NI, IRET
      REAL      RFSCAL(NI,NA,NA,*), B(3,NC,NT,NB,NI,*)
C
      INTEGER   NTMAX
      PARAMETER (NTMAX = 99)
      INCLUDE 'RFLAG.INC'
      INCLUDE 'RFHIST.INC'
      INTEGER   NUMVIS, CATMP(256), CHFLGS(MAXCIF), NW, NVM, NTIMES, I,
     *   TNUM(NTMAX), VISN(2,NTMAX), LSOU, ISOU, NCT, CT, J1, J2, JT,
     *   CNT(3), NRBL(MAXANT,MAXANT), CHFLGD(MAXCIF),
     *   ISONE(MXBASE*NTMAX)
      LOGICAL   END, FIRST, GOTONE, GETNEW, LDUM
      REAL      VIS(3,UVBFSS/3), RPARM(20), TB, TE,  TINT, TIME,
     *   TIMES(2,NTMAX), TLIMIT, BLENG
      DOUBLE PRECISION RMSRMS(3,MAXCIF), DEVRMS(3,MAXCIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (VIS, BUFF1)
C-----------------------------------------------------------------------
C                                       histogram
      CALL DFILL (NHRMS, 0.0D0, HISRMS)
      CALL DFILL (NHDEV, 0.0D0, HISDEV)
      DOTIME = ABS(FPARM(3)).LT.1.E6
      IF ((.NOT.DOTIME) .AND. (.NOT.DOSPEC)) THEN
         MSGTXT = 'RFLAFP: NEITHER TIME FOR SPECTRAL PLOTS, QUITTING'
         CALL MSGWRT (8)
         IRET = 10
         GO TO 999
         END IF
      CNT(1) = 0
      CNT(2) = 0
      CNT(3) = 0
      NBOXES = XNBOX + 0.5
      IF (NBOXES.LT.20) NBOXES = 200
      IF (NBOXES.GT.NHRMS-3) NBOXES = NHRMS - 3
      NBOXES = (NBOXES/2) * 2 + 3
      XNBOX = NBOXES - 3
      IF (VPARM(1).LE.0.0) VPARM(1) = 2.0
      INCRMS = VPARM(1) / (NBOXES-3.0)
      IF (VPARM(2).LE.0.0) VPARM(2) = 2.0
      INCDEV = VPARM(2) / (NBOXES-3.0)
      IF (VPARM(3).LT.0.001) VPARM(3) = 1.0
      CENDEV = 2
      CENRMS = 2
C                                       parameters
      NVM = NC * NT
      IF (FPARM(2).LE.0.0) FPARM(2) = 10.
      TLIMIT = 2.01 * FPARM(1) * FPARM(2)
      TLIMIT = TLIMIT / (24. * 3600.) * NTAVG
      TINT = 1.001 * FPARM(2) / (24. * 3600.) * NTAVG
      I = NC * NI * 3
      CALL DFILL (I, 0.0D0, RMSRMS)
      CALL DFILL (I, 0.0D0, DEVRMS)
C                                       counters, mask
      NCHAN = CATBLK(KINAX+JLOCF)
      CALL CHWANT (NCHAN, LIF, CHNSEL(1,1,LBIF), CHFLGS)
      CALL CHWANT (NCHAN, LIF, CHNDSL(1,1,LBIF), CHFLGD)
      NTIMES = 0
      I = MAXANT * MAXANT
      CALL FILL (I, 0, NRBL)
      I = MXBASE * NTMAX
      CALL FILL (I, 0, ISONE)
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       post-flag plots need new FG
      IF (DOPLOT.LT.0) THEN
         FGVER = FGVERO
         END IF
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN/INIT INPUT VIS FILE'
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
      NUMVIS = 0
      TB = -1000.0
      TE = TB
      NW = 3 * NC * NT * NB * NI * NSTOK
      CALL RFILL (NW, 0.0, B)
      LSOU = -1
      CT = 0
      NCT = 0
      FIRST = .TRUE.
      GOTONE = .FALSE.
      MSGTXT = 'Reading data to generate plots'
      CALL MSGWRT (2)
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      END = IRET.LT.0
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING DATA FIRST PASS'
         GO TO 990
C                                       got good data, now what
      ELSE IF (IRET.LE.0) THEN
C                                       in current time
         IF (END) THEN
            TIME = 1.E4
         ELSE
            CALL UVSCAL (NI, NA, RFSCAL, RPARM, VIS)
            NUMVIS = NUMVIS + 1
            IF (MOD(NUMVIS,50000).EQ.0) THEN
               WRITE (MSGTXT,1100) NUMVIS
               CALL MSGWRT (2)
               END IF
            BLENG = RPARM(1+ILOCU)*RPARM(1+ILOCU) +
     *         RPARM(1+ILOCV)*RPARM(1+ILOCV)
            IF ((BLENG.LT.UVRANG(1)) .OR. (BLENG.GT.UVRANG(2)))
     *         GO TO 100
            TIME = RPARM(1+ILOCT)
            END IF
         ISOU = 1
         IF (ILOCSU.GE.0) ISOU = RPARM(1+ILOCSU) + 0.01
         IF (END) ISOU = -1
         IF ((ABS(TB-TIME).LT.TINT) .AND. (ISOU.EQ.LSOU)) THEN
            CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B, ISONE,
     *         GETNEW)
            IF (.NOT.GETNEW) THEN
               GOTONE = .TRUE.
               VISN(2,CT) = NUMVIS
               TIMES(1,CT) = MIN (TIMES(1,CT), TIME)
               TIMES(2,CT) = MAX (TIMES(2,CT), TIME)
               TB = TIMES(1,CT)
               END IF
         ELSE
            GETNEW = .TRUE.
            END IF
C                                       need new time bin
         IF (GETNEW) THEN
C                                       average this time before
C                                       changing it
            IF (CT.GT.0) CALL RWTAVG (NC, NT, NB, NI, CT, B)
C                                       just advance time counter
            IF ((NCT.LT.NT) .AND. (ISOU.EQ.LSOU) .AND.
     *         (TIME-TB.LT.TLIMIT)) THEN
               NCT = NCT + 1
               TNUM(NCT) = NCT
               CT = NCT
               JT = NB * (CT -1) + 1
               CALL FILL (NB, 0, ISONE(JT))
               TIMES(1,NCT) = TIME
               TIMES(2,NCT) = TIME
               VISN(1,CT) = NUMVIS
               VISN(2,CT) = NUMVIS
               CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B, ISONE,
     *            LDUM)
               GOTONE = .TRUE.
               TB = TIME
C                                       still going, clear 1 or more
            ELSE IF ((ISOU.EQ.LSOU) .AND. (TIME-TB.LT.TLIMIT)) THEN
               J1 = (NT+1)/2
               J2 = J1
               IF (FIRST) J1 = 1
               FIRST = .FALSE.
               CALL RWRMSH (NC, NT, NA, NB, NI, NCT, J1, J2, TNUM,
     *            CHFLGS, CHFLGD, B, RMSRMS, DEVRMS, CNT)
C                                       next
               CT = TNUM(1)
               DO 150 JT = 1,NT-1
                  TNUM(JT) = TNUM(JT+1)
 150              CONTINUE
               JT = NB * (CT -1) + 1
               CALL FILL (NB, 0, ISONE(JT))
               TNUM(NT) = CT
               TIMES(1,CT) = TIME
               TIMES(2,CT) = TIME
               VISN(1,CT) = NUMVIS
               VISN(2,CT) = NUMVIS
               CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B, ISONE,
     *            LDUM)
               GOTONE = .TRUE.
               TB = TIME
C                                       done with this scan
            ELSE
               IF (GOTONE) THEN
                  J1 = (NT+1)/2
                  J2 = NCT
                  IF (FIRST) J1 = 1
                  CALL RWRMSH (NC, NT, NA, NB, NI, NCT, J1, J2, TNUM,
     *               CHFLGS, CHFLGD, B, RMSRMS, DEVRMS, CNT)
                  END IF
               IF (.NOT.END) THEN
                  CALL RFILL (NW, 0.0, B)
                  FIRST = .TRUE.
                  CT = 1
                  CALL FILL (NB, 0, ISONE)
                  NCT = 1
                  TNUM(1) = 1
                  TIMES(1,CT) = TIME
                  TIMES(2,CT) = TIME
                  VISN(1,CT) = NUMVIS
                  VISN(2,CT) = NUMVIS
                  CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B, ISONE,
     *               LDUM)
                  TB = TIME
                  LSOU = ISOU
                  GOTONE = .TRUE.
                  END IF
               END IF
            END IF
         IF (.NOT.END) GO TO 100
         END IF
C                                       close uv data set
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
C                                       pre-clipped count
      IF (CNT(1).GT.0) THEN
         WRITE (MSGTXT,1150) CNT(1)
         CALL MSGWRT (4)
         END IF
      IF (CNT(2).GT.0) THEN
         WRITE (MSGTXT,1151) CNT(2)
         CALL MSGWRT (4)
         END IF
      IF (CNT(3).GT.0) THEN
         WRITE (MSGTXT,1152) CNT(3)
         CALL MSGWRT (4)
         END IF
C                                       mess with the RMSRMS array
      CALL RFLARM (NC, NI, RMSRMS, FPARM(9), XNOISE(LBIF))
      CALL RFLARM (NC, NI, DEVRMS, FPARM(10), XSCUT(LBIF))
C                                       write the output text file
      IF ((DOOUT.GT.0.0) .AND. (FPARM(4).EQ.0.0)) DOOUT = 1.0
      IF ((DOOUT.GT.0.0) .AND. (OUTEXT.NE.' ')) CALL RFLATE (NC, NI,
     *   RMSRMS, DEVRMS, DOOUT, OUTEXT)
C                                       Do plots
      IF (ABS(DOPLOT).LT.16.0) CALL RFLAGP (NC, NI, RMSRMS, DEVRMS,
     *   IRET)
      IRET = MAX (0, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RFLAFP: ERROR',I3,' ON ',A)
 1100 FORMAT ('RFLAFP: on visibility number',I10)
 1150 FORMAT (I12,' channels pre-clipped due to FPARM(13)')
 1151 FORMAT (I12,' IFs * BLs flagged due to FPARM(15)')
 1152 FORMAT (I12,' IFs * BLs flagged due to FPARM(16)')
      END
      SUBROUTINE RWRMSH (NC, NT, NA, NB, NI, NCT, J1, J2, TNUM, CHFLGS,
     *   CHFLGD, B, RMSRMS, DEVRMS, CNT)
C-----------------------------------------------------------------------
C   robust rms in arrays - sum to histograms
C   Inputs
C      NC       I      Number spectral channels
C      NT       I      Number times - max
C      NA       I      Number antennas
C      NB       I      Number baselines
C      NI       I      Number IFs
C      NCT      I      Number times in this call
C      J1       I      Time number to process lower limit
C      J2       I      Time number to process upper limit
C      TNUM     I(*)   Position in B of data
C      CHFLGS   I(*)   > 0 => incl in freq rms (NC,NI)
C      CHFLGD   I(*)   > 0 => do not include (NC,NI)
C      B        R(*)   Data buffer (3,NC,NT,NB,NI,NP)
C   In/out:
C      RMSRMS   D(*)   summing up time RMSes
C      DEVRMS   D(*)   summing up spectral deviations
C-----------------------------------------------------------------------
      INTEGER   NC, NT, NA, NB, NI, NCT, J1, J2, TNUM(*), CHFLGS(NC,NI),
     *   CHFLGD(NC,NI), CNT(3)
      REAL      B(3,NC,NT,NB,NI,*)
      DOUBLE PRECISION RMSRMS(3,NC,*), DEVRMS(3,NC,*)
C
      INCLUDE 'RFLAG.INC'
      INCLUDE 'RFHIST.INC'
      INTEGER   NITER
      PARAMETER (NITER=12)
      INTEGER   JF, JI, JP, JA1, JA2, JT, JB, L, LR, JJ, JX, K, JLIM,
     *   NEXT, IROUND, NR, JC1, JC2, JF1, JF2, KA1, KA2
      REAL      RMSR, RMSI, MEDIAN, VM(MAXCHA,2), TEMP, V1, V2
      DOUBLE PRECISION WT, SR, SSR, SI, SSI, WR, AVR, AVI, V
      LOGICAL   DOBL, DOANT, CNTIT
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NEXT = FPARM(8) + 0.1
C                                       pre-"quack"
      IF (FPARM(14).GE.1.0) THEN
         DO 670 JI = 1,NI
            DO 660 JF = 1,NC
               DO 650 JJ = 1,NCT
                  JX = TNUM(JJ)
                  L = 0
                  DO 620 JA1 = 1,NA
                     DO 610 JA2 = 1,NA
                        IF (JA2.LT.JA1) THEN
                           JB = NA * (JA2-1) - (JA2*(JA2-1))/2 + JA1
                        ELSE
                           JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
                           END IF
                        K = 0
                        DO 605 JP = 1,NSTOK
                           WT = B(3,JF,JX,JB,JI,JP)
                           IF (WT.GT.0) K = K + 1
 605                       CONTINUE
                        IF (K.GT.0) L = L + 1
 610                    CONTINUE
 620                 CONTINUE
                  IF ((L.GT.0) .AND. (L.LT.FPARM(14))) THEN
                     DO 640 JA1 = 1,NA
                        DO 630 JA2 = 1,NA
                           IF (JA2.LT.JA1) THEN
                              JB = NA * (JA2-1) - (JA2*(JA2-1))/2 + JA1
                           ELSE
                              JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
                              END IF
                           DO 625 JP = 1,NSTOK
                              B(3,JF,JX,JB,JI,JP) =
     *                           -ABS (B(3,JF,JX,JB,JI,JP))
 625                          CONTINUE
 630                       CONTINUE
 640                    CONTINUE
                     END IF
 650              CONTINUE
 660           CONTINUE
 670        CONTINUE
         END IF
C                                       extreme clips
      IF ((FPARM(15).GT.0.0) .OR. (FPARM(16).GT.0.0)) THEN
         DO 770 JI = 1,NI
            DO 760 JJ = 1,NCT
               JX = TNUM(JJ)
               DO 750 JA1 = 1,NA
                  DO 720 JA2 = 1,NA
                     IF (JA2.LT.JA1) THEN
                        JB = NA * (JA2-1) - (JA2*(JA2-1))/2 + JA1
                     ELSE
                        JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
                        END IF
                     DOBL = .FALSE.
                     DOANT = .FALSE.
                     DO 710 JF = 1,NC
                        DO 705 JP = 1,NSTOK
                           IF (CHFLGD(JF,JI).LE.0) THEN
                              WT = B(3,JF,JX,JB,JI,JP)
                              IF (WT.GT.0) THEN
                                 V1 = B(1,JF,JX,JB,JI,JP)
                                 V2 = B(2,JF,JX,JB,JI,JP)
                                 SR = V1*V1 + V2*V2
                                 IF (SR.GT.FPARM(15)) THEN
                                    DOBL = .TRUE.
                                    END IF
                                 IF (SR.GT.FPARM(16)) THEN
                                    DOANT = .TRUE.
                                    END IF
                                 END IF
                              END IF
 705                       CONTINUE
 710                    CONTINUE
                     IF (FPARM(15).LE.0.0) DOBL = .FALSE.
                     IF (FPARM(16).LE.0.0) DOANT = .FALSE.
                     IF (DOANT) THEN
                        GO TO 725
C                                       flag JA2 - JA1
                     ELSE IF (DOBL) THEN
                        CNTIT = .FALSE.
                        DO 715 JF = 1,NC
                           DO 714 JP = 1,NSTOK
                              IF (B(3,JF,JX,JB,JI,JP).GT.0.0) THEN
                                 CNTIT = .TRUE.
                                 B(3,JF,JX,JB,JI,JP) =
     *                              -B(3,JF,JX,JB,JI,JP)
                                 END IF
 714                          CONTINUE
 715                       CONTINUE
                        IF (CNTIT) CNT(2) = CNT(2) + 1
                        END IF
 720                 CONTINUE
                  DOANT = .FALSE.
C                                       flag all BL with JA1
 725              IF (DOANT) THEN
                     KA1 = JA1
                     KA2 = JA2
                     DO 740 JA2 = 1,NA
C                                       baselines with KA1
                        IF (JA2.LT.KA1) THEN
                           JB = NA * (JA2-1) - (JA2*(JA2-1))/2 + KA1
                        ELSE
                           JB = NA * (KA1-1) - (KA1*(KA1-1))/2 + JA2
                           END IF
                        CNTIT = .FALSE.
                        DO 730 JF = 1,NC
                           DO 729 JP = 1,NSTOK
                              IF (B(3,JF,JX,JB,JI,JP).GT.0.0) THEN
                                 CNTIT = .TRUE.
                                 B(3,JF,JX,JB,JI,JP) =
     *                              -B(3,JF,JX,JB,JI,JP)
                                 END IF
 729                          CONTINUE
 730                       CONTINUE
                        IF (CNTIT) CNT(3) = CNT(3) + 1
C                                       baselines with KA2
                        IF (JA2.LT.KA2) THEN
                           JB = NA * (JA2-1) - (JA2*(JA2-1))/2 + KA2
                        ELSE
                           JB = NA * (KA2-1) - (KA2*(KA2-1))/2 + JA2
                           END IF
                        CNTIT = .FALSE.
                        DO 735 JF = 1,NC
                           DO 734 JP = 1,NSTOK
                              IF (B(3,JF,JX,JB,JI,JP).GT.0.0) THEN
                                 CNTIT = .TRUE.
                                 B(3,JF,JX,JB,JI,JP) =
     *                              -B(3,JF,JX,JB,JI,JP)
                                 END IF
 734                          CONTINUE
 735                       CONTINUE
                        IF (CNTIT) CNT(3) = CNT(3) + 1
 740                    CONTINUE
                     END IF
 750              CONTINUE
 760           CONTINUE
 770        CONTINUE
         END IF
C                                       regular operation
      DO 400 JA1 = 1,NA
         DO 390 JA2 = JA1,NA
            JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
            DO 380 JI = 1,NI
C                                       are there any data
               DO 15 JF = 1,NC
                  IF (CHFLGD(JF,JI).LE.0) THEN
                     DO 14 JP = 1,NSTOK
                        DO 13 JT = 1,NCT
                           WT = B(3,JF,JT,JB,JI,JP)
                           IF (WT.GT.0.0) GO TO 19
 13                        CONTINUE
 14                     CONTINUE
                     END IF
 15               CONTINUE
               GO TO 380
C                                       pre-clip
 19            IF (FPARM(13).LE.1.E14) THEN
                  DO 30 JT = 1,NCT
                     JX = TNUM(JT)
                     DO 25 JF = 1,NC
                        IF (CHFLGD(JF,JI).LE.0) THEN
                           DO 20 JP = 1,NSTOK
                              WT = B(3,JF,JT,JB,JI,JP)
                              IF (WT.GT.0.0) THEN
                                 V1 = B(1,JF,JT,JB,JI,JP)
                                 V2 = B(2,JF,JT,JB,JI,JP)
                                 SR = V1*V1 + V2*V2
                                 IF (SR.GT.FPARM(13)) THEN
                                    B(3,JF,JT,JB,JI,JP) =
     *                                 -ABS (B(3,JF,JT,JB,JI,JP))
                                    CNT(1) = CNT(1) + 1
                                    END IF
                                 END IF
 20                           CONTINUE
                           END IF
 25                     CONTINUE
 30                  CONTINUE
                  END IF
C                                       time rms
               JLIM = NC
               IF (DOTIME) THEN
                  JLIM = 0
                  DO 80 JF = 1,NC
                     IF (CHFLGD(JF,JI).LE.0) THEN
                        JLIM = JLIM + 1
                        DO 70 JP = 1,NSTOK
                           SR = 0.0D0
                           SSR = 0.0D0
                           WR = 0.0D0
                           LR = 0
                           SI = 0.0D0
                           SSI = 0.0D0
                           NR = 0
                           DO 60 JT = 1,NCT
                              WT = B(3,JF,JT,JB,JI,JP)
                              IF (WT.GT.0.0) THEN
                                 V = B(1,JF,JT,JB,JI,JP)
                                 SR = SR + V * WT
                                 SSR = SSR + V * V * WT
                                 WR = WR + WT
                                 LR = LR + 1
                                 V = B(2,JF,JT,JB,JI,JP)
                                 SI = SI + V * WT
                                 SSI = SSI + V * V * WT
                                 NR = NR + 1
                                 END IF
 60                           CONTINUE
                           IF ((WR.GT.0.0D0) .AND. (NR.GT.1)) THEN
                              AVR = SR / WR
                              SSR = SSR / WR
                              RMSR = SSR - AVR * AVR
                              RMSR = SQRT (MAX (0.0, RMSR))
                              AVI = SI / WR
                              SSI = SSI / WR
                              RMSI = SSI - AVI * AVI
                              RMSI = SQRT (MAX (0.0, RMSI))
                              RMSR = SQRT (RMSR*RMSR + RMSI*RMSI)
                              IF (RMSR.GT.0.0) THEN
                                 L = RMSR / INCRMS + 0.5 + CENRMS
                                 L = MAX (1, MIN (NBOXES, L))
                                 HISRMS(L) = HISRMS(L) + 1.0D0
                                 RMSRMS(1,JF,JI) = RMSRMS(1,JF,JI) +
     *                              RMSR
                                 RMSRMS(2,JF,JI) = RMSRMS(2,JF,JI) +
     *                              RMSR*RMSR
                                 RMSRMS(3,JF,JI) = RMSRMS(3,JF,JI) +
     *                              1.0D0
                                 END IF
                              END IF
 70                        CONTINUE
                        END IF
 80                  CONTINUE
                  END IF
C                                       single time spectral rms
               IF (DOSPEC) THEN
                  DO 300 JJ = J1,J2
                     JX = TNUM(JJ)
                     DO 200 JP = 1,NSTOK
C                                       median
                        JC1 = 1
 100                    JC2 = JC1 + AVGCHN - 1
                        IF ((JC1.LE.NC) .AND. (JC2.GT.NC)) THEN
                           JC2 = NC
                           JC1 = JC2 - AVGCHN + 1
                           END IF
                        IF (JC2.LE.NC) THEN
                           JF1 = (JC1 + JC2 - XTRCHN) / 2
                           JF2 = JF1 + XTRCHN
                           IF (JC1.EQ.1) JF1 = 1
                           IF (JC2.EQ.NC) JF2 = NC
                           L = 0
                           DO 110 JF = JC1,JC2
                              IF ((CHFLGS(JF,JI).GT.0) .AND.
     *                           (CHFLGD(JF,JI).LE.0)) THEN
                                 WT = B(3,JF,JX,JB,JI,JP)
                                 IF (WT.GT.0.0) THEN
                                    L = L + 1
                                    VM(L,1) = B(1,JF,JX,JB,JI,JP)
                                    VM(L,2) = B(2,JF,JX,JB,JI,JP)
                                    END IF
                                 END IF
 110                          CONTINUE
                           IF (MOD(L,2).EQ.0) L = L - 1
                           IF (L.GT.0) THEN
                              AVR = MEDIAN (L, VM(1,1))
                              AVI = MEDIAN (L, VM(1,2))
                              DO 120 JF = JF1,JF2
                                 WT = B(3,JF,JX,JB,JI,JP)
                                 IF (WT.GT.0.0) THEN
C                                       real part
                                    V = B(1,JF,JX,JB,JI,JP)
                                    TEMP = ABS (V-AVR)
                                    DEVRMS(1,JF,JI) = DEVRMS(1,JF,JI) +
     *                                 TEMP
                                    DEVRMS(2,JF,JI) = DEVRMS(2,JF,JI) +
     *                                 TEMP * TEMP
                                    DEVRMS(3,JF,JI) = DEVRMS(3,JF,JI) +
     *                                 1.0D0
                                    TEMP = TEMP / INCDEV
                                    L = IROUND (TEMP) + CENDEV
                                    L = MAX (1, MIN (NBOXES, L))
                                    HISDEV(L) = HISDEV(L) + 1.0D0
C                                       imaginary
                                    V = B(2,JF,JX,JB,JI,JP)
                                    TEMP = ABS (V-AVI)
                                    DEVRMS(1,JF,JI) = DEVRMS(1,JF,JI) +
     *                                 TEMP
                                    DEVRMS(2,JF,JI) = DEVRMS(2,JF,JI) +
     *                                 TEMP * TEMP
                                    DEVRMS(3,JF,JI) = DEVRMS(3,JF,JI) +
     *                                 1.0D0
                                    TEMP = TEMP / INCDEV
                                    L = IROUND (TEMP) + CENDEV
                                    L = MAX (1, MIN (NBOXES, L))
                                    HISDEV(L) = HISDEV(L) + 1.0D0
                                    END IF
 120                             CONTINUE
                              END IF
                           JC1 = JC1 + 1 + XTRCHN
                           IF (JC2.LT.NC) GO TO 100
                           END IF
 200                    CONTINUE
 300                 CONTINUE
                  END IF
 380           CONTINUE
 390        CONTINUE
 400     CONTINUE
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE RFLARM (NC, NI, RMSRMS, SCALE, XNOISE)
C-----------------------------------------------------------------------
C   RFLARM computes things with the summed up time RMSes
C   Inputs:
C      NC       I      Number spectral channel
C      NI       I      Number of IFs
C   In/Out:
C      RMSRMS   D(*)   Sums of rmses, rms*rms, count
C      XNOISE   R(*)   Median attempt at clip level
C-----------------------------------------------------------------------
      INTEGER   NC, NI
      REAL      SCALE, XNOISE(*)
      DOUBLE PRECISION RMSRMS (3,NC,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   JC, JI, K
      REAL      VALUES(MAXCHA), MEDIAN, MEDR, MEDRR
C-----------------------------------------------------------------------
      DO 100 JI = 1,NI
         K = 0
         MEDR = 0.0
         MEDRR = 0.0
         DO 10 JC = 1,NC
            IF (RMSRMS(3,JC,JI).GE.1.0D0) THEN
               RMSRMS(1,JC,JI) = RMSRMS(1,JC,JI) / RMSRMS(3,JC,JI)
               RMSRMS(2,JC,JI) = RMSRMS(2,JC,JI) / RMSRMS(3,JC,JI) -
     *            RMSRMS(1,JC,JI) * RMSRMS(1,JC,JI)
               RMSRMS(2,JC,JI) = SQRT (MAX (0.0D0, RMSRMS(2,JC,JI)))
               K = K + 1
               VALUES(K) = RMSRMS(1,JC,JI)
               END IF
 10         CONTINUE
         IF (K.GT.0) THEN
            MEDR = MEDIAN (K, VALUES)
            DO 20 JC = 1,K
               VALUES(JC) = ABS (VALUES(JC)-MEDR)
 20            CONTINUE
            MEDRR = 1.4826 * MEDIAN (K, VALUES)
            END IF
         IF (SCALE.GT.0.0) XNOISE(JI) = SCALE * (MEDR + MEDRR)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RFLAGP (NC, NI, RMSRMS, DEVRMS, IRET)
C-----------------------------------------------------------------------
C   RFLAGP plots the histograms
C      NC       I      Number spectral channel
C      NI       I      Number of IFs
C      RMSRMS   D(*)   Sums of rmses, rms*rms, count
C      DEVRMS   D(*)   Mean of deviation, rms of deviation, count
C   Output:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   NC, NI, IRET
      DOUBLE PRECISION RMSRMS(3,NC,*), DEVRMS(3,NC,*)
C
      INCLUDE 'RFLAG.INC'
      INCLUDE 'RFHIST.INC'
      INTEGER   K, IVER, GRCHN, TVCHN, TVCORN(2), PLUN, PIND, LABEL,
     *   IROUND, I, I1, I2, IDOPL, NB1, NB2, IOFF, NX1, NX2, ZAND, JC,
     *   JI, JIB, LTYPE
      REAL      YMIN, YMAX, XMIN, XMAX, BLOG, HIST(NHRMS), NOMAX, NOMIN
      DOUBLE PRECISION XUNDER, XOVER, TOTAL, XTOTAL
      LOGICAL   DOTV, DOLOG, DOSELF, DOSSLF
      DOUBLE PRECISION CSUM, SUM
      CHARACTER PFILE*48, UUNITS*25
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       add 3 for answers
      NPARM = NPARM + 7
      UVRANG(1) = SQRT (UVRANG(1))/1.E3
      UVRANG(2) = SQRT (UVRANG(2))/1.E3
C                                       get real header back
      CALL COPY (256, CATUV, CATBLK)
      DOTV = XDOTV.GT.0.0
      CALL FNDEXT ('PL', CATBLK, IVER)
      IDOPL = IROUND (DOPLOT)
      IDOPL = ABS (IDOPL)
      TVCHN = 1
      TVCORN(1) = 0
      TVCORN(2) = 0
      GRCHN = XGRCH + 0.5
      LABEL = IROUND (XLABEL)
      LTYPE = MOD (ABS(LABEL), 100)
      IF (LTYPE.EQ.0) THEN
         IF (LABEL.LT.0) THEN
            LABEL = (LABEL/100)*100 - 3
         ELSE
            LABEL = (LABEL/100)*100 + 3
            END IF
         END IF
      DOLOG = FUNCTY.EQ.'LG'
      BLOG = ALOG10 (0.5)
      NB1 = IROUND (VPARM(4))
      NB2 = IROUND (VPARM(5))
      DOSELF = (NB1.LE.0) .OR. (NB1.GE.NB2)
      IF (.NOT.DOSELF) THEN
         NB1 = NB1 + 1
         NB1 = MAX (2, MIN (NB1, NBOXES-1))
         NB2 = NB2 + 1
         NB2 = MAX (2, MIN (NB2, NBOXES-1))
         DOSELF = NB1.GE.NB2
         END IF
      NX1 = IROUND (VPARM(8))
      NX2 = IROUND (VPARM(9))
      DOSSLF = (NX1.LE.0) .OR. (NX1.GE.NX2)
      IF (.NOT.DOSSLF) THEN
         NX1 = NX1 + 1
         NX1 = MAX (2, MIN (NX1, NBOXES-1))
         NX2 = NX2 + 1
         NX2 = MAX (2, MIN (NX2, NBOXES-1))
         DOSSLF = NX1.GE.NX2
         END IF
C                                       plot rms on time
      IF (MOD(IDOPL,2).NE.0) THEN
         IF (DOTIME) THEN
            UUNITS = 'RMS (JY)'
            YMIN = 1.E10
            YMAX = -1.E10
            I1 = NHRMS + 10
            I2 = 0
            K = 0
            XUNDER = HISRMS(1)
            XOVER = HISRMS(NBOXES)
            TOTAL = XUNDER + XOVER
            DO 10 I = 2,NBOXES-1
               IF ((K.GT.0) .OR. ((HISRMS(I).GT.0.0D0) .AND. (DOSELF))
     *            .OR. ((.NOT.DOSELF) .AND. (I.EQ.NB1))) THEN
                  I1 = MIN (I1, I)
                  IF (HISRMS(I).GT.0.0D0) I2 = I
                  K = K + 1
                  IF (DOLOG) THEN
                     IF (HISRMS(I).GT.0) THEN
                        HIST(K) = LOG10 (HISRMS(I))
                     ELSE
                        HIST(K) = BLOG
                        END IF
                  ELSE
                     HIST(K) = HISRMS(I)
                     END IF
                  YMIN = MIN (YMIN, HIST(K))
                  YMAX = MAX (YMAX, HIST(K))
                  END IF
               IF (.NOT.DOSELF) THEN
                  IF (I.LT.NB1) XUNDER = XUNDER + HISRMS(I)
                  IF (I.GT.NB2) XOVER = XOVER + HISRMS(I)
                  END IF
               TOTAL = TOTAL + HISRMS(I)
 10            CONTINUE
            IF ((YMIN.GT.0.0) .AND. (YMIN.LT.0.33*YMAX)) YMIN = 0.0
            IF ((.NOT.DOSELF) .AND. (VPARM(6).GT.0.0) .AND.
     *         (VPARM(6).GT.0.05*YMAX)) THEN
               YMAX = VPARM(6)
               YMIN = 0.0
               END IF
            YMAX = YMIN + VPARM(3) * (YMAX - YMIN)
            IF (I1.LT.I2/3) I1 = 2
            IF (.NOT.DOSELF) THEN
               I1 = MAX (I1, NB1)
               I2 = NB2
               END IF
            XMIN = (I1-CENRMS) * INCRMS
            XMAX = (I2-CENRMS) * INCRMS
            IVER = IVER + 1
            IF (.NOT.DOTV) THEN
               CALL MADDEX ('PL', DISKIN, OLDCNO, CATBLK, BUFF1, .TRUE.,
     *            'UPDT', IVER, IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'ERROR ADDING PLOT TO HEADER'
                  CALL MSGWRT (7)
                  END IF
               END IF
C                                       for extlist
            XPARM(1) = 1.0
            XPARM(2) = YMIN
            XPARM(3) = YMAX
            XPARM(4) = XMIN
            XPARM(5) = XMAX
            XPARM(6) = I1
            XPARM(7) = I2
            CALL ZPHFIL ('PL', DISKIN, OLDCNO, IVER, PFILE, IRET)
            CALL GINIT (DISKIN, OLDCNO, PFILE, 0, 36, NPARM, XNAMEI,
     *         DOTV, TVCHN, GRCHN, TVCORN, CATBLK, BUFF1, PLUN, PIND,
     *         IRET)
            IF (IRET.NE.0) GO TO 20
            CALL HISTOG (1, I1, I2, HIST, XUNDER, XOVER, TOTAL, XMIN,
     *         XMAX, YMIN, YMAX, DOLOG, LABEL, IVER, XYRATO, UUNITS,
     *         IBUFF1, GRCHN, DOTV, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'ERROR. WILL TRY TO FINISH PARTIAL GRAPH.'
               CALL MSGWRT (7)
               END IF
            GPHPAG = (DOSPEC) .OR. (IDOPL.GT.2)
            CALL GFINIS (BUFF1, IRET)
            IF (IRET.LT.0) GO TO 980
C                                       Successful plot file finished.
            IF (IRET.EQ.0) THEN
               IF (.NOT.DOTV) THEN
                  CALL HIPLOT (DISKIN, OLDCNO, IVER, BUFF1, IRET)
                  WRITE (MSGTXT,1015) IVER
                  CALL MSGWRT (5)
                  IRET = 0
                  END IF
               END IF
            END IF
C                                       plot spectral deviations
 20      IF (DOSPEC) THEN
            UUNITS = 'DEVIATION (JY)'
            YMIN = 1.E10
            YMAX = -1.E10
            I1 = NHDEV + 10
            I2 = 0
            K = 0
            CSUM = 0.0D0
            DO 21 I = 1,NBOXES
               CSUM = CSUM + HISDEV(I)
 21            CONTINUE
            SUM = HISDEV(1) + HISDEV(NBOXES)
            XUNDER = HISDEV(1)
            XOVER = HISDEV(NBOXES)
            XTOTAL = XUNDER + XOVER
            DO 30 I = 2,NBOXES-1
               IF ((K.GT.0) .OR. ((HISDEV(I).GT.0.0D0) .AND. (DOSSLF))
     *            .OR. ((.NOT.DOSSLF) .AND. (I.EQ.NX1))) THEN
                  I1 = MIN (I1, I)
                  IF (HISDEV(I).GT.0.0D0) I2 = I
                  K = K + 1
                  IF (DOLOG) THEN
                     IF (HISDEV(I).GT.0) THEN
                        HIST(K) = LOG10 (HISDEV(I))
                     ELSE
                        HIST(K) = BLOG
                        END IF
                  ELSE
                     HIST(K) = HISDEV(I)
                     END IF
                  YMIN = MIN (YMIN, HIST(K))
                  YMAX = MAX (YMAX, HIST(K))
                  END IF
               IF (.NOT.DOSSLF) THEN
                  IF (I.LT.NX1) XUNDER = XUNDER + HISDEV(I)
                  IF (I.GT.NX2) XOVER = XOVER + HISDEV(I)
                  END IF
               XTOTAL = XTOTAL + HISDEV(I)
 30            CONTINUE
            IF ((YMIN.GT.0.0) .AND. (YMIN.LT.0.33*YMAX)) YMIN = 0.0
            IF ((.NOT.DOSSLF) .AND. (VPARM(10).GT.0.0) .AND.
     *         (VPARM(10).GT.0.05*YMAX)) THEN
               YMAX = VPARM(10)
               YMIN = 0.0
               END IF
            YMAX = YMIN + VPARM(3) * (YMAX - YMIN)
            IF (I1.LT.I2/3) I1 = 2
            IF (.NOT.DOSSLF) THEN
               I1 = NX1
               I2 = NX2
               END IF
            XMIN = (I1-CENDEV) * INCDEV
            XMAX = (I2-CENDEV) * INCDEV
            IVER = IVER + 1
            IF (.NOT.DOTV) THEN
               CALL MADDEX ('PL', DISKIN, OLDCNO, CATBLK, BUFF1, .TRUE.,
     *            'UPDT', IVER, IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'ERROR ADDING PLOT TO HEADER'
                  CALL MSGWRT (7)
                  END IF
               END IF
C                                       for extlist
            XPARM(1) = 2.0
            XPARM(2) = YMIN
            XPARM(3) = YMAX
            XPARM(4) = XMIN
            XPARM(5) = XMAX
            XPARM(6) = I1
            XPARM(7) = I2
            CALL ZPHFIL ('PL', DISKIN, OLDCNO, IVER, PFILE, IRET)
            CALL GINIT (DISKIN, OLDCNO, PFILE, 0, 36, NPARM, XNAMEI,
     *         DOTV, TVCHN, GRCHN, TVCORN, CATBLK, BUFF1, PLUN, PIND,
     *         IRET)
            IF (IRET.NE.0) GO TO 100
            CALL HISTOG (2, I1, I2, HIST, XUNDER, XOVER, XTOTAL, XMIN,
     *         XMAX, YMIN, YMAX, DOLOG, LABEL, IVER, XYRATO, UUNITS,
     *         IBUFF1, GRCHN, DOTV, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'ERROR. WILL TRY TO FINISH PARTIAL GRAPH.'
               CALL MSGWRT (7)
               END IF
            GPHPAG = (ZAND(IDOPL,14).NE.0)
            CALL GFINIS (BUFF1, IRET)
            IF (IRET.LT.0) GO TO 980
C                                       Successful plot file finished.
            IF (IRET.EQ.0) THEN
               IF (.NOT.DOTV) THEN
                  CALL HIPLOT (DISKIN, OLDCNO, IVER, BUFF1, IRET)
                  WRITE (MSGTXT,1015) IVER
                  CALL MSGWRT (5)
                  IRET = 0
                  END IF
               END IF
            END IF
         END IF
C                                       cummulative
 100  IF (ZAND(IDOPL,2).EQ.2) THEN
         IF (DOTIME) THEN
            CSUM = 0.0D0
            DO 110 I = 1,NBOXES
               CSUM = CSUM + HISRMS(I)
 110           CONTINUE
            UUNITS = 'RMS (JY)'
            SUM = HISRMS(NBOXES)
            I2 = NBOXES - 2
            DO 115 I = NBOXES-1,2,-1
               SUM = SUM + HISRMS(I)
               HIST(I-1) = SUM / CSUM
               IF (I.EQ.2) HIST(1) = HIST(1) + HISRMS(1) / CSUM
               IF (HIST(I-1).LE.0.0) I2 = I-1
               IF (DOLOG) THEN
                  IF (HIST(I-1).GT.0.0) THEN
                     HIST(I-1) = LOG10 (HIST(I-1))
                  ELSE
                     HIST(I-1) = -LOG10 (CSUM) - 0.3
                     END IF
                  END IF
 115           CONTINUE
            I1 = 2
            IOFF = 1
            IF (.NOT.DOSELF) THEN
               IOFF = NB1 - I1 + 1
               I1 = NB1
               I2 = NB2
               END IF
            YMIN = MIN (HIST(I2), HIST(I2-1))
            YMAX = HIST(I1-1)
            IF ((YMIN.GT.0.0) .AND. (YMIN.LT.0.33*YMAX)) YMIN = 0.0
            IF ((.NOT.DOSELF) .AND. (VPARM(7).GT.0.0) .AND.
     *         (VPARM(7).GT.0.05*YMAX)) THEN
               YMAX = VPARM(7)
               YMIN = 0.0
               END IF
            YMAX = YMIN + VPARM(3) * (YMAX - YMIN)
            XMIN = (I1-CENRMS) * INCRMS
            XMAX = (I2-CENRMS) * INCRMS
            IVER = IVER + 1
            IF (.NOT.DOTV) THEN
               CALL MADDEX ('PL', DISKIN, OLDCNO, CATBLK, BUFF1, .TRUE.,
     *            'UPDT', IVER, IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'ERROR ADDING PLOT TO HEADER'
                  CALL MSGWRT (7)
                  END IF
               END IF
C                                       for extlist
            XPARM(1) = 3.0
            XPARM(2) = YMIN
            XPARM(3) = YMAX
            XPARM(4) = XMIN
            XPARM(5) = XMAX
            XPARM(6) = I1
            XPARM(7) = I2
            CALL ZPHFIL ('PL', DISKIN, OLDCNO, IVER, PFILE, IRET)
            CALL GINIT (DISKIN, OLDCNO, PFILE, 0, 36, NPARM, XNAMEI,
     *         DOTV, TVCHN, GRCHN, TVCORN, CATBLK, BUFF1, PLUN, PIND,
     *         IRET)
            IF (IRET.NE.0) GO TO 120
            CALL HISTOG (3, I1, I2, HIST(IOFF), 0.0D0, 0.0D0, TOTAL,
     *         XMIN, XMAX, YMIN, YMAX, DOLOG, LABEL, IVER, XYRATO,
     *         UUNITS, IBUFF1, GRCHN, DOTV, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'ERROR. WILL TRY TO FINISH PARTIAL GRAPH.'
               CALL MSGWRT (7)
               END IF
            GPHPAG = (DOSPEC) .OR. (ZAND(IDOPL,4).EQ.4)
            CALL GFINIS (BUFF1, IRET)
            IF (IRET.LT.0) GO TO 980
C                                       Successful plot file finished.
            IF (IRET.EQ.0) THEN
               IF (.NOT.DOTV) THEN
                  CALL HIPLOT (DISKIN, OLDCNO, IVER, BUFF1, IRET)
                  WRITE (MSGTXT,1015) IVER
                  CALL MSGWRT (5)
                  IRET = 0
                  END IF
               END IF
            END IF
C                                       plot spectral deviations
 120     IF (DOSPEC) THEN
            UUNITS = 'DEVIATION (JY)'
            CSUM = 0.0D0
            DO 130 I = 1,NBOXES
               CSUM = CSUM + HISDEV(I)
 130           CONTINUE
            SUM = HISDEV(1) + HISDEV(NBOXES)
            I2 = NBOXES - CENDEV + 1
            HIST(I2) = 0.0
            DO 140 I = NBOXES-1,CENDEV,-1
               SUM = SUM + HISDEV(I)
               K = I - CENDEV + 1
               HIST(K) = SUM / CSUM
               IF (HIST(K).LE.0.0) I2 = K
               IF (DOLOG) THEN
                  IF (HIST(K).GT.0.0) THEN
                     HIST(K) = LOG10 (HIST(K))
                  ELSE
                     HIST(K) = -LOG (CSUM) - 0.3
                     END IF
                  END IF
 140           CONTINUE
            I1 = 1
            IF (.NOT.DOSSLF) THEN
               I1 = (NX1 - CENDEV + 1) / 2 + 1
               I2 = (NX2 - CENDEV + 1) / 2 + 1
               I1 = MAX (1, I1)
               I2 = MAX (1, I2)
               IF (I2.LE.I1) I2 = NBOXES - CENDEV
               END IF
            K = NBOXES - CENDEV
            YMIN = MIN (HIST(I2), HIST(I2-1))
            YMAX = HIST(I1)
            IF ((YMIN.GT.0.0) .AND. (YMIN.LT.0.33*YMAX)) YMIN = 0.0
            IF ((.NOT.DOSELF) .AND. (VPARM(11).GT.0.0) .AND.
     *         (VPARM(11).GT.0.05*YMAX)) THEN
               YMAX = VPARM(11)
               YMIN = 0.0
               END IF
            YMAX = YMIN + VPARM(3) * (YMAX - YMIN)
            XMIN = 0.0
            XMAX = (I2 - 1) * INCDEV
            IVER = IVER + 1
            IF (.NOT.DOTV) THEN
               CALL MADDEX ('PL', DISKIN, OLDCNO, CATBLK, BUFF1, .TRUE.,
     *            'UPDT', IVER, IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'ERROR ADDING PLOT TO HEADER'
                  CALL MSGWRT (7)
                  END IF
               END IF
C                                       for extlist
            XPARM(1) = 4.0
            XPARM(2) = YMIN
            XPARM(3) = YMAX
            XPARM(4) = XMIN
            XPARM(5) = XMAX
            XPARM(6) = I1
            XPARM(7) = I2
            CALL ZPHFIL ('PL', DISKIN, OLDCNO, IVER, PFILE, IRET)
            CALL GINIT (DISKIN, OLDCNO, PFILE, 0, 36, NPARM, XNAMEI,
     *         DOTV, TVCHN, GRCHN, TVCORN, CATBLK, BUFF1, PLUN, PIND,
     *         IRET)
            IF (IRET.NE.0) GO TO 200
            CALL HISTOG (4, I1, I2, HIST(I1), 0.0D0, 0.0D0, XTOTAL,
     *         XMIN, XMAX, YMIN, YMAX, DOLOG, LABEL, IVER, XYRATO,
     *         UUNITS, IBUFF1, GRCHN, DOTV, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'ERROR. WILL TRY TO FINISH PARTIAL GRAPH.'
               CALL MSGWRT (7)
               END IF
            GPHPAG = ((DOSPEC) .AND. (ZAND(IDOPL,8).EQ.8)) .OR.
     *            (ZAND(IDOPL,4).EQ.4)
            CALL GFINIS (BUFF1, IRET)
            IF (IRET.LT.0) GO TO 980
C                                       Successful plot file finished.
            IF (IRET.EQ.0) THEN
               IF (.NOT.DOTV) THEN
                  CALL HIPLOT (DISKIN, OLDCNO, IVER, BUFF1, IRET)
                  WRITE (MSGTXT,1015) IVER
                  CALL MSGWRT (5)
                  IRET = 0
                  END IF
               END IF
            END IF
         END IF
C                                       plot time rmses vs channel
 200  IF ((DOTIME) .AND. (ZAND(IDOPL,4).EQ.4)) THEN
         YMIN = 1.E10
         YMAX = 0.0
         NOMAX = 0.0
         NOMIN = 1.E10
         DO 210 JI = 1,NI
            JIB = JI - 1 + LBIF
            NOMIN = MIN (NOMIN, XNOISE(JIB))
            NOMAX = MAX (NOMAX, XNOISE(JIB))
            DO 205 JC = 1,NC
               IF (RMSRMS(1,JC,JI)+RMSRMS(2,JC,JI).GT.YMAX) YMAX =
     *            RMSRMS(1,JC,JI) + RMSRMS(2,JC,JI)
               IF (RMSRMS(1,JC,JI)-RMSRMS(2,JC,JI).LT.YMIN) YMIN =
     *            RMSRMS(1,JC,JI) - RMSRMS(2,JC,JI)
 205           CONTINUE
 210        CONTINUE
         YMIN = MAX (0.0, YMIN)
         IF ((YMIN.GT.0.0) .AND. (YMIN.LT.0.33*YMAX)) YMIN = 0.0
         IF ((VPARM(12).LT.YMAX) .AND. (VPARM(13).GT.VPARM(12))) THEN
            YMIN = VPARM(12)
            YMAX = VPARM(13)
         ELSE
            YMIN = MIN (NOMIN, YMIN)
            IF (YMAX.GT.NOMAX+ 0.2*(NOMAX-YMIN)) THEN
               YMAX = NOMAX+ 0.11*(NOMAX-YMIN)
            ELSE
               YMAX = MAX (YMAX, NOMAX)
               END IF
            END IF
         XMIN = 0.0
         XMAX = (NC + 1) * NI
         IVER = IVER + 1
         IF (.NOT.DOTV) THEN
            CALL MADDEX ('PL', DISKIN, OLDCNO, CATBLK, BUFF1, .TRUE.,
     *         'UPDT', IVER, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'ERROR ADDING PLOT TO HEADER'
               CALL MSGWRT (7)
               END IF
            END IF
C                                       for extlist
         XPARM(1) = 5.0
         XPARM(2) = YMIN
         XPARM(3) = YMAX
         XPARM(4) = XMIN
         XPARM(5) = XMAX
         XPARM(6) = NC
         XPARM(7) = NI
         CALL ZPHFIL ('PL', DISKIN, OLDCNO, IVER, PFILE, IRET)
         CALL GINIT (DISKIN, OLDCNO, PFILE, 0, 36, NPARM, XNAMEI, DOTV,
     *      TVCHN, GRCHN, TVCORN, CATBLK, BUFF1, PLUN, PIND, IRET)
         IF (IRET.NE.0) GO TO 980
         CALL RMSPLT (1, NC, NI, LBIF, RMSRMS, XNOISE, XMIN, XMAX, YMIN,
     *      YMAX, LABEL, IVER, XYRATO, IBUFF1, DOTV, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR. WILL TRY TO FINISH PARTIAL GRAPH.'
            CALL MSGWRT (7)
            END IF
         GPHPAG = (DOSPEC) .AND. (ZAND(IDOPL,8).EQ.8)
         CALL GFINIS (BUFF1, IRET)
         IF (IRET.LT.0) GO TO 980
C                                       Successful plot file finished.
         IF (IRET.EQ.0) THEN
            IF (.NOT.DOTV) THEN
               CALL HIPLOT (DISKIN, OLDCNO, IVER, BUFF1, IRET)
               WRITE (MSGTXT,1015) IVER
               CALL MSGWRT (5)
               IRET = 0
               END IF
            END IF
         END IF
C                                       deviations
      IF ((DOSPEC) .AND. (ZAND(IDOPL,8).EQ.8)) THEN
         YMIN = 1.E10
         YMAX = 0.0
         NOMAX = 0.0
         NOMIN = 1.E10
         DO 310 JI = 1,NI
            JIB = JI - 1 + LBIF
            NOMIN = MIN (NOMIN, XSCUT(JIB))
            NOMAX = MAX (NOMAX, XSCUT(JIB))
            DO 305 JC = 1,NC
               IF (DEVRMS(1,JC,JI)+DEVRMS(2,JC,JI).GT.YMAX) YMAX =
     *            DEVRMS(1,JC,JI) + DEVRMS(2,JC,JI)
               IF (DEVRMS(1,JC,JI)-DEVRMS(2,JC,JI).LT.YMIN) YMIN =
     *            DEVRMS(1,JC,JI) - DEVRMS(2,JC,JI)
 305           CONTINUE
 310        CONTINUE
         YMIN = MAX (0.0, YMIN)
         IF ((YMIN.GT.0.0) .AND. (YMIN.LT.0.33*YMAX)) YMIN = 0.0
         IF ((VPARM(14).LT.YMAX) .AND. (VPARM(15).GT.VPARM(14))) THEN
            YMIN = VPARM(14)
            YMAX = VPARM(15)
         ELSE
            YMIN = MIN (NOMIN, YMIN)
            IF (YMAX.GT.NOMAX+ 0.2*(NOMAX-YMIN)) THEN
               YMAX = NOMAX+ 0.11*(NOMAX-YMIN)
            ELSE
               YMAX = MAX (YMAX, NOMAX)
               END IF
            END IF
         XMIN = 0.0
         XMAX = (NC + 1) * NI
         IVER = IVER + 1
         IF (.NOT.DOTV) THEN
            CALL MADDEX ('PL', DISKIN, OLDCNO, CATBLK, BUFF1, .TRUE.,
     *         'UPDT', IVER, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'ERROR ADDING PLOT TO HEADER'
               CALL MSGWRT (7)
               END IF
            END IF
C                                       for extlist
         XPARM(1) = 6.0
         XPARM(2) = YMIN
         XPARM(3) = YMAX
         XPARM(4) = XMIN
         XPARM(5) = XMAX
         XPARM(6) = NC
         XPARM(7) = NI
         CALL ZPHFIL ('PL', DISKIN, OLDCNO, IVER, PFILE, IRET)
         CALL GINIT (DISKIN, OLDCNO, PFILE, 0, 36, NPARM, XNAMEI, DOTV,
     *      TVCHN, GRCHN, TVCORN, CATBLK, BUFF1, PLUN, PIND, IRET)
         IF (IRET.NE.0) GO TO 980
         CALL RMSPLT (2, NC, NI, LBIF, DEVRMS, XSCUT, XMIN, XMAX, YMIN,
     *      YMAX, LABEL, IVER, XYRATO, IBUFF1, DOTV, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR. WILL TRY TO FINISH PARTIAL GRAPH.'
            CALL MSGWRT (7)
            END IF
         GPHPAG = .FALSE.
         CALL GFINIS (BUFF1, IRET)
         IF (IRET.LT.0) GO TO 980
C                                       Successful plot file finished.
         IF (IRET.EQ.0) THEN
            IF (.NOT.DOTV) THEN
               CALL HIPLOT (DISKIN, OLDCNO, IVER, BUFF1, IRET)
               WRITE (MSGTXT,1015) IVER
               CALL MSGWRT (5)
               IRET = 0
               END IF
            END IF
         END IF
C                                       reset units
 980  UVRANG(1) = UVRANG(1) * UVRANG(1) * 1.E6
      UVRANG(2) = UVRANG(2) * UVRANG(2) * 1.E6
C
 999  RETURN
C-----------------------------------------------------------------------
 1015 FORMAT ('Successful histogram plot file version',I7,' created')
      END
      SUBROUTINE HISTOG (ITY, NBX1, NBX2, HIST, UNDER, OVER, TOTAL,
     *   XMIN, XMAX, YMIN, YMAX, DOLOG, LABEL, IVER, XYRATO, UNITS,
     *   BUFF1, GRCHN, DOTV, IRET)
C-----------------------------------------------------------------------
C   This routine will write commands to an open plot file for drawing
C   a histogram.
C   Inputs:
C      ITY      I      Plot type: 1 Flux on X axis, count on Y axis
C                        2 x * sigma on X, count on Y
C      NBOXES   I      number of boxes for histogram.
C      HIST     R(*)   Histogram
C      XMIN     R      Min value in X
C      XMAX     R      Max value in X
C      YMIN     R      Min value in Y
C      XMAX     R      Max value in Y
C      DOLOG    L      T => use log(n) rather than linear scale
C      LABEL    I      Type of labeling
C      IVER     I      Plot file version number
C      UNITS    C*25   Units to use
C   In/out:
C      BUFF1    I(*)   I/O buffer for open, initialized pl file.
C   Output:
C      IRET     I      error code. 0=ok, 1=write error to plot file.
C-----------------------------------------------------------------------
      INTEGER   ITY, NBX1, NBX2, LABEL, IVER, BUFF1(*), GRCHN, IRET
      REAL      XMIN, XMAX, YMIN, YMAX, HIST(*), XYRATO
      DOUBLE PRECISION UNDER, OVER, TOTAL
      LOGICAL   DOLOG, DOTV
      CHARACTER UNITS*(*)
C
      REAL      BLC(7), CH(4), TRC(7), X, Y, FAC, LOCRAN(2), XYRAT,
     *   RANGE(2), XMN, YMN
      INTEGER   IDEPTH(5), I, LTYPE, NBOXES
      CHARACTER TXTMSG*80
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       Set character offsets.
      NBOXES = NBX2 - NBX1 + 1
      LOCRAN(2) = YMAX
      LOCRAN(1) = YMIN
      CALL GTICNT (LABEL, LOCRAN, I)
      RANGE(1) = XMIN
      RANGE(2) = XMAX
C                                       number characters around
      CALL RFILL (4, 0.5, CH)
      LTYPE = MOD (ABS (LABEL), 100)
      IF (LTYPE.EQ.2) CH(1) = 2.5
      IF (LTYPE.GT.2) CH(1) = I + 4.0
      IF (LTYPE.GT.1) CH(2) = 2.0
      IF (LTYPE.GT.2) CH(2) = CH(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         CH(2) = CH(2) + 2 * 1.333
C         IF ((UNDER.GT.0.0D0) .OR. (OVER.GT.0.0D0)) CH(2) = CH(2)+1.333
         CH(2) = CH(2) + 1.333
         END IF
      IF (LTYPE.EQ.2) CH(3) = 2.5
      IF (LTYPE.GT.1) CH(4) = CH(4) + 1.5
      IF (LTYPE.GT.2) CH(4) = CH(4) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CH(4) = CH(4) + 1.333
      IF ((LABEL.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7)) CH(4) =
     *   CH(4) + 1.333
C                                       Set BLC, TRC, XYRATO.
      CALL RFILL (5, 1.0, BLC(3))
      CALL RFILL (5, 1.0, TRC(3))
      CALL FILL (5, 1, IDEPTH)
      BLC(1) = 0.5 - 0.02 * NBOXES
      TRC(1) = NBOXES + 0.5 + 0.02 * NBOXES
      BLC(2) = YMIN - 0.05 * (YMAX - YMIN)
      TRC(2) = YMAX + 0.05 * (YMAX - YMIN)
C                                       default XYRATIO
      IF (XYRATO.LT.0.01) THEN
         IF (DOTV) THEN
            XMN = WINDTV(3) - WINDTV(1) + 1 - CSIZTV(1) * (CH(1)
     *         + CH(3))
            YMN = WINDTV(4) - WINDTV(2) + 1 - CSIZTV(2) * (CH(2)
     *         + CH(4))
            XYRATO = 1.414
            IF (YMN.GT.0.0) XYRATO = XMN / YMN
         ELSE
            XYRATO = 1.414
            END IF
         END IF
      XYRAT = (TRC(1) - BLC(1)) / (TRC(2) - BLC(2))
C                                       Kludge to keep XYRATO in bounds
C                                       to prevent overflow in GINITL.
      FAC = 1.0
      IF (XYRAT.GT.3.0) THEN
         DO 10 I = 1,10000
            IF (XYRAT.LT.2.0) GO TO 20
            FAC = FAC / 2.
            XYRAT = XYRAT / 2.
 10      CONTINUE
      ELSE IF (XYRAT.LT.0.333) THEN
         DO 15 I = 1,10000
            IF (XYRAT.GT.0.50) GO TO 20
            FAC = FAC * 2.
            XYRAT = XYRAT * 2.
 15         CONTINUE
         END IF
C
 20   TRC(2) = TRC(2) / FAC
      BLC(2) = BLC(2) / FAC
      XYRAT = XYRATO / XYRAT
C                                       Initialize plot file line drw.
      CALL GINITL (BLC, TRC, XYRAT, CH, IDEPTH, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       clear graphics channels
      IF ((DOTV) .AND. (GRCHN.EQ.0)) THEN
         CALL GLTYPE (4, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GPOS (BLC(1), BLC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (BLC(1), BLC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GLTYPE (3, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GPOS (BLC(1), BLC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (BLC(1), BLC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      CALL GLTYPE (1, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Draw borders.
      CALL GPOS (BLC(1), BLC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (TRC(1), BLC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (TRC(1), TRC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (BLC(1), TRC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (BLC(1), BLC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Labeling.
      CALL HLABX (ITY, BLC, TRC, FAC, NBX1, NBX2, RANGE, UNDER, OVER,
     *   TOTAL, DOLOG, IVER, LABEL, UNITS, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      TXTMSG = 'End labeling, draw histogram'
      CALL GCOMNT (-1, TXTMSG, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GLTYPE (2, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Position at first data point.
      X = 0.5
      Y = BLC(2)
      CALL GPOS (X, Y, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Loop for rest of data points.
      DO 30 I = 1,NBOXES
         Y = HIST(I) / FAC
         Y = MIN (Y, TRC(2))
         CALL GVEC (X, Y, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         X = I + 0.5
         CALL GVEC (X, Y, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         Y = BLC(2)
         CALL GVEC (X, Y, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
 30      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE HLABX (ITY, BLC, TRC, FAC, NBX1, NBX2, RANGE, UNDER,
     *   OVER, TOTAL, DOLOG, IVER, LABEL, UNITS, BUFF1, IRET)
C-----------------------------------------------------------------------
C   Write labeling for histogram.
C   Inputs:
C      ITY     I        Type: 1 time rms, 2 spectral rms, 3 cumulative
C      BLC     R(2)     bottom left corner of plot.
C      TRC     R(2)     top right hand corner of plot.
C      FAC     R        FAC*XYRATO = real XYRATIO.
C      IVER    I        plot file version number
C      LABEL   I        labeling type
C      UNITS   C*8      units
C   In/out:
C      BUFF1   I(256)   I/O buffer for plot file.
C   Output:
C      IRET    I        error code returned from GVEC.
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7), FAC, RANGE(2)
      DOUBLE PRECISION UNDER, OVER, TOTAL
      INTEGER   ITY, NBX1, NBX2, IVER, LABEL, BUFF1(256), IRET
      LOGICAL   DOLOG
      CHARACTER UNITS*(*)
C
      CHARACTER PREFIX(2)*5, TIME*8, DATE*12, CTEMP*8, CSTOK(12)*4,
     *   NAMSTR*18, MSGBUF*128
      LOGICAL   PFLAG
      REAL      XINTER(24), DCX, DCY, XNOINT, DIST, ODIST, XMAX, TICSCL,
     *   YTICEL, YTICER, XVAL, YPOS, TICLEN, XINT, X, FREQ, DCXM, XDIST
      INTEGER   INOINT, INCHAR, I, IXO, M, ITRY, NXFR, NXST, NAX, INC,
     *   IANGL, JSTOK, IT(3), ID(3), ICPNT, ITMP, NBOXES, N1, J, NU,
     *   JTRIM, LTYPE
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA TICSCL /70.0/
      DATA CSTOK /'????','Beam','Ipol','Qpol','Upol','Vpol','Ppol',
     *   'Fpol','Pang','Spix','Optd','    '/
      DATA XINTER /.01, .02, .05, .1, .2, .5, 1., 2., 5., 10., 20.,
     *   50., 100., 200., 500., 1000., 2000., 5000., 10000., 20000.,
     *   50000., 100000., 200000., 500000./
C-----------------------------------------------------------------------
      LTYPE = MOD (ABS(LABEL), 100)
      IF (LTYPE.LE.1) GO TO 999
      NU = JTRIM (UNITS)
      NBOXES = NBX2 - NBX1 + 1
C                                       Tic positions.
      TICLEN = (TRC(1) - BLC(1)) / TICSCL
      YTICEL = BLC(1) + TICLEN
      YTICER = TRC(1) - TICLEN
C                                       Find interval value.
      DIST = FAC * (TRC(2) - BLC(2))
      ODIST = DIST
      CALL METSCL (LABEL, DIST, PREFIX(2), PFLAG)
      IF (PFLAG) GO TO 110
      XDIST = DIST / ODIST
      XINT = 8.0
      DO 20 I = 1,21
         XNOINT = AINT (DIST/XINTER(I))
         IF (XNOINT.LE.XINT) GO TO 30
 20      CONTINUE
      GO TO 110
C                                       Interval and no of inter found.
 30   XINT = XINTER(I)
      INOINT = XNOINT + 2.5
      ODIST = XDIST * FAC * BLC(2)
      XVAL = AINT (ODIST/XINT) * XINT
      IF (XVAL.GE.ODIST) XVAL = XVAL - XINT
      IXO = I
      DCXM = -0.5
C                                       Loop for all tics.
      DO 100 I = 1,INOINT
         XVAL = XVAL + XINT
         YPOS = XVAL / FAC / XDIST
         IF (YPOS.GT.TRC(2)) GO TO 110
C                                       right hand tic.
         CALL GPOS (TRC(1), YPOS, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (YTICER, YPOS, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Left hand tic.
         CALL GPOS (YTICEL, YPOS, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (BLC(1), YPOS, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Write value.
         IF (LTYPE.GT.2) THEN
            WRITE (MSGBUF,1030) XVAL
            CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
            IF (IXO.GT.6) THEN
               INCHAR = INCHAR - 3
            ELSE IF (IXO.GT.3) THEN
               INCHAR = INCHAR - 1
               END IF
            DCX = - INCHAR - 1.0
            DCY = -0.5
            DCXM = MIN (DCXM, DCX)
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 100     CONTINUE
C                                       Number of pixels
 110  DCX = DCXM - 2.0
      YPOS = (TRC(2) + BLC(2)) / 2.0
      CALL GPOS (BLC(1), YPOS, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (ITY.EQ.1) THEN
         IF (DOLOG) THEN
            MSGBUF = PREFIX(2) // ' Log10 (number chan/time)'
         ELSE
            MSGBUF = PREFIX(2) // ' Number of chan/time'
            END IF
      ELSE IF (ITY.EQ.2) THEN
         IF (DOLOG) THEN
            MSGBUF = PREFIX(2) // ' Log10 (number chan)'
         ELSE
            MSGBUF = PREFIX(2) // ' Number of chan'
            END IF
      ELSE IF ((ITY.EQ.3) .OR. (ITY.EQ.4)) THEN
         IF (DOLOG) THEN
            MSGBUF = PREFIX(2) // ' Log10 (fraction beyond)'
         ELSE
            MSGBUF = PREFIX(2) // ' Fraction beyond'
            END IF
         END IF
      CALL CHTRIM (MSGBUF, 40, MSGBUF, INCHAR)
      DCY = INCHAR / 2.0 - 1.0
      CALL GCHAR (INCHAR, 1, DCX, DCY, MSGBUF, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write bucket numbers on top
      IF (NBOXES.LE.8) THEN
         M = 1
      ELSE IF (NBOXES.LE.16) THEN
         M = 2
      ELSE IF (NBOXES.LE.40) THEN
         M = 5
      ELSE IF (NBOXES.LE.80) THEN
         M = 10
      ELSE IF (NBOXES.LE.160) THEN
         M = 20
      ELSE IF (NBOXES.LE.400) THEN
         M = 50
      ELSE IF (NBOXES.LE.800) THEN
         M = 100
      ELSE IF (NBOXES.LE.1600) THEN
         M = 200
      ELSE IF (NBOXES.LE.4000) THEN
         M = 500
      ELSE IF (NBOXES.LE.8000) THEN
         M = 1000
      ELSE IF (NBOXES.LE.16000) THEN
         M = 2000
      ELSE IF (NBOXES.LE.40000) THEN
         M = 5000
         END IF
      TICLEN = (TRC(2) - BLC(2)) / TICSCL
      YTICEL = BLC(2) + TICLEN
      YTICER = TRC(2) - TICLEN
      DCY = 0.5
      N1 = ((NBX1 - 2) / M) * M
      IF (N1.LT.NBX1-2) N1 = N1 + M
      DO 150 I = N1,NBX2-1,M
         X = I - (NBX1-2)
         CALL GPOS (X, YTICER, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (X, TRC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (LTYPE.GT.2) THEN
            WRITE (MSGBUF,1115) I
            CALL CHTRIM (MSGBUF, 4, MSGBUF, INCHAR)
            DCX = 0.5 - REAL(INCHAR)
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 150     CONTINUE
C                                       Label RHS bucket #
      X = (TRC(1) + BLC(1)) / 2.0
      CALL GPOS (X, TRC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      DCX = -5.0
      DCY = 0.5
      IF (LTYPE.GT.2) DCY = DCY + 1.333
      MSGBUF = 'Box number'
      CALL GCHAR (10, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Range =
      IF (LTYPE.LT.7) THEN
         CALL GPOS (BLC(1), BLC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         DCX = 0.0
         DCY = -2.833
         IF (LTYPE.GT.2) DCY = DCY - 1.333
         IF (ITY.EQ.1) THEN
            MSGBUF = 'HISTOGRAM OF DEVIATIONS OVER TIME'
         ELSE IF (ITY.EQ.2) THEN
            MSGBUF = 'HISTOGRAM OF DEVIATIONS OVER SPECTRAL WINDOWS'
         ELSE IF (ITY.EQ.3) THEN
            MSGBUF = 'CUMULATIVE HISTOGRAM OF DEVIATIONS OVER TIME'
         ELSE IF (ITY.EQ.4) THEN
            MSGBUF = 'CUMULATIVE HISTOGRAM OF DEVIATIONS OVER ' //
     *         'SPECTRAL WINDOWS'
            END IF
         CALL REFRMT (MSGBUF, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Interval =
         CALL GPOS (BLC(1), BLC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         DCY = DCY - 1.333
         X = (RANGE(2) - RANGE(1)) / (NBOXES - 1.0)
         WRITE (MSGBUF,1151) RANGE(1), RANGE(2), UNITS(:NU), X,
     *      UNITS(:NU)
         CALL REFRMT (MSGBUF, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       underflow and overflow
         I = UNDER + 0.5D0
         M = OVER + 0.5D0
         J = TOTAL + 0.5D0
         IF ((I.GT.0) .OR. (M.GT.0)) THEN
            WRITE (MSGBUF,1154) I, M, J
         ELSE
            WRITE (MSGBUF,1155) J
            END IF
         CALL REFRMT (MSGBUF, '_', INCHAR)
         DCY = DCY - 1.333
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Determine label range
      DIST = RANGE(2) - RANGE(1)
      ODIST = DIST
      CALL METSCL (LABEL, DIST, PREFIX(1), PFLAG)
      IF (PFLAG) GO TO 190
      XDIST = DIST / ODIST
      ODIST = XDIST * RANGE(1)
C                                       Get interval
      DO 160 ITRY = 1,21
         XNOINT = AINT (DIST/XINTER(ITRY))
         IF (XNOINT.LE.8.0) GO TO 170
 160     CONTINUE
      GO TO 190
C                                       Bottom (value) tics
 170  XINT = XINTER(ITRY)
      DCY = -1.5
      XMAX = MAX (ABS(RANGE(2)), ABS(RANGE(1))) * XDIST
      INOINT = XNOINT + 2.5
      XVAL = AINT (ODIST/XINT) * XINT
      IF (XVAL.GE.ODIST) XVAL = XVAL - XINT
      IXO = ITRY
      DO 175 I = 1,INOINT
         XVAL = XVAL + XINT
         X = ((XVAL-ODIST)/DIST) * (NBOXES - 1.0) + 1.0
         IF (X.GT.TRC(1)) GO TO 180
         CALL GPOS (X, YTICEL, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (X, BLC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (LTYPE.GT.2) THEN
            WRITE (MSGBUF,1030) XVAL
            CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
            IF (IXO.GT.6) THEN
               INCHAR = INCHAR - 3
            ELSE IF (IXO.GT.3) THEN
               INCHAR = INCHAR - 1
               END IF
            DCX = 0.5 - INCHAR
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 175     CONTINUE
C                                       Label with prefix
 180  DCY = -1.5
      IF (LTYPE.GT.2) DCY = DCY - 1.333
      X = (TRC(1) + BLC(1)) / 2.0
      CALL GPOS (X, BLC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      WRITE (MSGBUF,1175) PREFIX(1), UNITS
      CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
      DCX = 0.5 - INCHAR / 2.0
      CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (LTYPE.GE.7) GO TO 999
C                                       which axis is which?
 190  NXFR = 0
      NXST = 0
      NAX = CATBLK(KIDIM)
      INC = 2
      DO 200 I = 1,NAX
         ICPNT = KHCTP+(I-1)*INC
         CALL H2CHR (8, 1, CATH(ICPNT), CTEMP)
         IF (CTEMP(1:4).EQ.'FREQ') NXFR  = I
         IF (CTEMP(1:4).EQ.'STOK') NXST  = I
 200     CONTINUE
C                                       Source name, stokes, freq.
      CALL GPOS (BLC(1), TRC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      DCX = 0.0
      DCY = 1.833
      IF (LTYPE.GT.2) DCY = DCY + 1.333
      IANGL = 0
      CALL H2CHR (8, 1, CATH(KHOBJ), CTEMP)
      FREQ = 0.0
      JSTOK = 12
      IF (NXFR.GT.2) FREQ = CATD(KDCRV+NXFR-1) + CATR(KRCIC+NXFR-1)
     *   * (BLC(NXFR) - CATR(KRCRP+NXFR-1))
      FREQ = FREQ / 1.E6
      IF (NXST.GT.2) JSTOK = CATD(KDCRV+NXST-1) + CATR(KRCIC+NXST-1)
     *   * (BLC(NXST) - CATR(KRCRP+NXST-1)) + 2.5
      IF (NXFR.GT.2) WRITE (MSGBUF,1200) CTEMP, CSTOK(JSTOK),
     *   FREQ
      IF (NXFR.LE.2) WRITE (MSGBUF,1200) CTEMP, CSTOK(JSTOK)
      CALL REFRMT (MSGBUF, '_', INCHAR)
C                                       image name
      INCHAR = INCHAR + 1
      IF (INCHAR.GT.1) THEN
         MSGBUF(INCHAR:INCHAR+2) = '  _'
         INCHAR = INCHAR + 3
         END IF
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMSTR(1:12))
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), NAMSTR(13:18))
      CALL NAMEST (NAMSTR, CATBLK(KIIMS), MSGBUF(INCHAR:), ITMP)
      CALL REFRMT (MSGBUF, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       time/date, version
      IF (LABEL.GT.0) THEN
         CALL GPOS (BLC(1), TRC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, TIME, DATE)
         WRITE (MSGBUF,1210) IVER, DATE, TIME
         CALL REFRMT (MSGBUF, '_', INCHAR)
         DCY = DCY + 1.333
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (F14.2)
 1115 FORMAT (I4)
 1151 FORMAT ('Range =',1PE10.2,' to',1PE10.2,1X,A,'___Interval =',
     *   1PE10.2,1X,A)
 1154 FORMAT ('Underflow =',I10,' _Overflow =',I10,' _Total=',I14)
 1155 FORMAT ('Total=',I14)
 1175 FORMAT (A5,1X,A)
 1200 FORMAT (A,'  _',A4,'_ ',F10.3,' MHz')
 1210 FORMAT ('Plot file version',I4,'__created ',A,A)
      END
      SUBROUTINE RMSPLT (ITY, NC, NI, LBIF, RMSRMS, XNOISE, XMIN, XMAX,
     *   YMIN, YMAX, LABEL, IVER, XYRATO, BUFF1, DOTV, IRET)
C-----------------------------------------------------------------------
C   This routine will write commands to an open plot file for drawing
C   a a plot of the RMSRMS array
C   Inputs:
C      ITY      I      Plot type: 1 time rmses, 2 real part deviations,
C                        3 imaginary part deviations
C      NC       I      Number spectral channels
C      NI       I      Number of IFs
C      LBIF     I      Current BIF
C      RMSRMS   D(*)   (avg/rms,count) for each channel, IF
C      XNOISE   R(*)   clip level for each IF
C      XMIN     R      Min value in X
C      XMAX     R      Max value in X
C      YMIN     R      Min value in Y
C      YMAX     R      Max value in Y
C      LABEL    I      Type of labeling
C      IVER     I      Plot file version number
C      DOTV     L      T = on TV directly
C   In/out:
C      XYRATO   R      X/Y ratio in plotting
C      BUFF1    I(*)   I/O buffer for open, initialized pl file.
C   Output:
C      IRET     I      error code. 0=ok, 1=write error to plot file.
C-----------------------------------------------------------------------
      INTEGER   ITY, NC, NI, LBIF, LABEL, IVER, BUFF1(*), IRET
      DOUBLE PRECISION RMSRMS(3,NC,*)
      REAL      XNOISE(*), XMIN, XMAX, YMIN, YMAX, XYRATO
      LOGICAL   DOTV
C
      REAL      BLC(7), CH(4), TRC(7), X, Y, FAC, LOCRAN(2), XYRAT,
     *   RANGE(2), XMN, YMN, AX(5), AY(5), DX, DY
      INTEGER   IDEPTH(5), I, LTYPE, ISYM, JC, JI
      CHARACTER TXTMSG*80
      LOGICAL   DO3C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       Set character offsets.
      ISYM = 3
      DO3C = .FALSE.
      LOCRAN(2) = YMAX
      LOCRAN(1) = YMIN
      CALL GTICNT (LABEL, LOCRAN, I)
      RANGE(1) = XMIN
      RANGE(2) = XMAX
C                                       number characters around
      CALL RFILL (4, 0.5, CH)
      LTYPE = MOD (ABS (LABEL), 100)
      IF (LTYPE.EQ.2) CH(1) = 2.5
      IF (LTYPE.GT.2) CH(1) = I + 4.0
      IF (LTYPE.GT.1) CH(2) = 2.0
      IF (LTYPE.GT.2) CH(2) = CH(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CH(2) = CH(2) + 2.666
      IF (LTYPE.EQ.2) CH(3) = 2.5
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CH(4) = CH(4) + 1.5
      IF ((LABEL.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7)) CH(4) =
     *   CH(4) + 1.333
C                                       Set BLC, TRC, XYRATO.
      CALL RFILL (5, 1.0, BLC(3))
      CALL RFILL (5, 1.0, TRC(3))
      CALL FILL (5, 1, IDEPTH)
      BLC(1) = XMIN
      TRC(1) = XMAX
      BLC(2) = YMIN - 0.05 * (YMAX - YMIN)
      TRC(2) = YMAX + 0.05 * (YMAX - YMIN)
C                                       default XYRATIO
      IF (XYRATO.LT.0.01) THEN
         IF (DOTV) THEN
            XMN = WINDTV(3) - WINDTV(1) + 1 - CSIZTV(1) * (CH(1)
     *         + CH(3))
            YMN = WINDTV(4) - WINDTV(2) + 1 - CSIZTV(2) * (CH(2)
     *         + CH(4))
            XYRATO = 1.414
            IF (YMN.GT.0.0) XYRATO = XMN / YMN
         ELSE
            XYRATO = 1.414
            END IF
         END IF
      XYRAT = (TRC(1) - BLC(1)) / (TRC(2) - BLC(2))
C                                       Kludge to keep XYRATO in bounds
C                                       to prevent overflow in GINITL.
      FAC = 1.0
      IF (XYRAT.GT.3.0) THEN
         DO 10 I = 1,10000
            IF (XYRAT.LT.2.0) GO TO 20
            FAC = FAC / 2.
            XYRAT = XYRAT / 2.
 10      CONTINUE
      ELSE IF (XYRAT.LT.0.333) THEN
         DO 15 I = 1,10000
            IF (XYRAT.GT.0.50) GO TO 20
            FAC = FAC * 2.
            XYRAT = XYRAT * 2.
 15         CONTINUE
         END IF
C
 20   TRC(2) = TRC(2) / FAC
      BLC(2) = BLC(2) / FAC
      XYRAT = XYRATO / XYRAT
C                                       Initialize plot file line drw.
      CALL GINITL (BLC, TRC, XYRAT, CH, IDEPTH, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GLTYPE (1, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Draw borders.
      CALL GPOS (BLC(1), BLC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (TRC(1), BLC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (TRC(1), TRC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (BLC(1), TRC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (BLC(1), BLC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       dividers
      DO 25 JI = 1,NI-1
         X = JI * (NC + 1.0)
         CALL GPOS (X, TRC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (X, BLC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
 25      CONTINUE
C                                       Labeling.
      CALL RMSLAB (ITY, BLC, TRC, FAC, NC, NI, LBIF, IVER, LABEL, BUFF1,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      TXTMSG = 'End labeling, draw rmses lines'
      CALL GCOMNT (-1, TXTMSG, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GLTYPE (2, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       rms +- lines
      I = 0
      DO 40 JI = 1,NI
         DO 30 JC = 1,NC
            I = I + 1
            X = I
            IF (RMSRMS(3,JC,JI).GE.1.0D0) THEN
               Y = (RMSRMS(1,JC,JI) + RMSRMS(2,JC,JI)) / FAC
               Y = MIN (Y, TRC(2))
               IF (Y.LT.TRC(2)) THEN
                  CALL GPOS (X-0.25, Y, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL GVEC (X+0.25, Y, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
               CALL GPOS (X, Y, BUFF1, IRET)
               IF (IRET.NE.0) GO TO 999
               Y = (RMSRMS(1,JC,JI) - RMSRMS(2,JC,JI)) / FAC
               IF (Y.LT.TRC(2)) THEN
                  Y = MAX (Y, BLC(2))
                  CALL GVEC (X, Y, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  IF (Y.GT.BLC(2)) THEN
                     CALL GPOS (X-0.25, Y, BUFF1, IRET)
                     IF (IRET.NE.0) GO TO 999
                     CALL GVEC (X+0.25, Y, BUFF1, IRET)
                     IF (IRET.NE.0) GO TO 999
                     END IF
                  END IF
               END IF
 30         CONTINUE
         I = I + 1
 40      CONTINUE
C                                       rms points
      TXTMSG = 'End labeling, draw rms points'
      CALL GCOMNT (-1, TXTMSG, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GLTYPE (4, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      DX = 0.33
      DY = 0.33
      IF (XYRAT.GT.1.0) THEN
         DY = DY * XYRAT
      ELSE
         DX = DX / XYRAT
         END IF
      I = 0
      DO 60 JI = 1,NI
         DO 50 JC = 1,NC
            I = I + 1
            X = I
            Y = RMSRMS(1,JC,JI) / FAC
            IF ((Y.LT.TRC(2)) .AND. (Y.GT.BLC(2)) .AND.
     *         (RMSRMS(3,JC,JI).GE.1.0D0)) THEN
               AX(1) = X
               AY(1) = Y
               AX(2) = AX(1)
               AX(3) = AX(1)
               AX(4) = AX(1) - 0.33
               AX(5) = AX(1) + 0.33
               AY(2) = AY(1) + 0.33
               AY(3) = AY(1) - 0.33
               AY(4) = AY(1)
               AY(5) = AY(1)
               CALL PNTPLT (ISYM, AX, AY, BLC, TRC, .FALSE., DO3C,
     *            BUFF1, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
 50         CONTINUE
         I = I + 1
 60      CONTINUE
C                                       XNOISE values
      TXTMSG = 'End labeling, draw XNOISE values'
      CALL GCOMNT (-1, TXTMSG, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GLTYPE (3, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      I = 0
      DO 70 JI = 1,NI
         X = I
         Y = XNOISE(JI-1+LBIF) / FAC
         I = I + NC + 1
         IF ((Y.LT.TRC(2)) .AND. (Y.GT.BLC(2))) THEN
            CALL GPOS (X, Y, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            X = I
            CALL GVEC (X, Y, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 70      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RMSLAB (ITY, BLC, TRC, FAC, NC, NI, LBIF, IVER, LABEL,
     *   BUFF1, IRET)
C-----------------------------------------------------------------------
C   Write labeling for rms plot
C   Inputs:
C      ITY      I      Plot type: 1 time rmses, 2 real part deviations,
C                        3 imaginary part deviations
C      BLC     R(2)     bottom left corner of plot.
C      TRC     R(2)     top right hand corner of plot.
C      FAC     R        FAC*XYRATO = real XYRATIO.
C      IVER    I        plot file version number
C      LABEL   I        labeling type
C   In/out:
C      BUFF1   I(256)   I/O buffer for plot file.
C   Output:
C      IRET    I        error code returned from GVEC.
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7), FAC
      INTEGER   ITY, NC, NI, LBIF, IVER, LABEL, BUFF1(256), IRET
C
      CHARACTER PREFIX(2)*5, TIME*8, DATE*12, CTEMP*8, CSTOK(12)*4,
     *   NAMSTR*18, MSGBUF*80
      LOGICAL   PFLAG
      REAL      XINTER(24), DCX, DCY, XNOINT, DIST, ODIST, TICSCL, XVAL,
     *   YTICEL, YTICER, YPOS, TICLEN, XINT, X, FREQ, DCXM, XDIST
      INTEGER   INOINT, INCHAR, I, IXO, NXFR, NXST, NAX, INC, IANGL,
     *   JSTOK, IT(3), ID(3), ICPNT, ITMP, LTYPE
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA TICSCL /70.0/
      DATA CSTOK /'????','Beam','Ipol','Qpol','Upol','Vpol','Ppol',
     *   'Fpol','Pang','Spix','Optd','    '/
      DATA XINTER /.001, .002, .005, .01, .02, .05, .1, .2, .5,
     *   1., 2., 5., 10., 20., 50., 100., 200., 500.,
     *   1000., 2000., 5000., 10000., 20000., 50000./
C-----------------------------------------------------------------------
      LTYPE = MOD (ABS(LABEL), 100)
      IF (LTYPE.LE.1) GO TO 999
C                                       Tic positions.
      TICLEN = (TRC(1) - BLC(1)) / TICSCL
      YTICEL = BLC(1) + TICLEN
      YTICER = TRC(1) - TICLEN
C                                       Find interval value.
      DIST = FAC * (TRC(2) - BLC(2))
      ODIST = DIST
      CALL METSCL (LABEL, DIST, PREFIX(2), PFLAG)
      IF (PFLAG) GO TO 110
      XDIST = DIST / ODIST
      XINT = 8.0
      DO 20 I = 1,24
         XNOINT = AINT (DIST/XINTER(I))
         IF (XNOINT.LE.XINT) GO TO 30
 20      CONTINUE
      GO TO 110
C                                       Interval and no of inter found.
 30   XINT = XINTER(I)
      INOINT = XNOINT + 2.5
      ODIST = XDIST * FAC * BLC(2)
      XVAL = AINT (ODIST/XINT) * XINT
      IF (XVAL.GE.ODIST) XVAL = XVAL - XINT
      IXO = I
      DCXM = -0.5
C                                       Loop for all tics.
      DO 100 I = 1,INOINT
         XVAL = XVAL + XINT
         YPOS = XVAL / FAC / XDIST
         IF (YPOS.GT.TRC(2)) GO TO 110
C                                       right hand tic.
         CALL GPOS (TRC(1), YPOS, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (YTICER, YPOS, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Left hand tic.
         CALL GPOS (YTICEL, YPOS, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (BLC(1), YPOS, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Write value.
         IF (LTYPE.GT.2) THEN
            WRITE (MSGBUF,1030) XVAL
            CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
            IF (IXO.GT.3) INCHAR = INCHAR - 1
            IF (IXO.GT.6) INCHAR = INCHAR - 1
            IF (IXO.GT.9) INCHAR = INCHAR - 2
            DCX = - INCHAR - 1.0
            DCY = -0.5
            DCXM = MIN (DCXM, DCX)
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 100     CONTINUE
C                                       RMS
 110  DCX = DCXM - 2.0
      YPOS = (TRC(2) + BLC(2)) / 2.0
      CALL GPOS (BLC(1), YPOS, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (ITY.EQ.1) THEN
         MSGBUF = PREFIX(2) // ' Mean RMS Jy'
      ELSE IF (ITY.EQ.2) THEN
         MSGBUF = PREFIX(2) // ' Mean abs(deviation) Jy'
         END IF
      CALL CHTRIM (MSGBUF, 40, MSGBUF, INCHAR)
      DCY = INCHAR / 2.0 - 1.0
      CALL GCHAR (INCHAR, 1, DCX, DCY, MSGBUF, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Nchan, BIF, EIF
      IF (LTYPE.LT.7) THEN
         MSGBUF = 'IF'
         DCY = -2.833 - 1.333
         XDIST = (TRC(1) - BLC(1)) / NI
         DO 120 I = 1,NI
            ITMP = LBIF + I - 1
            IF ((I.EQ.1) .OR. (NI.LE.4)) THEN
               IF (ITMP.LT.10) THEN
                  WRITE (MSGBUF(4:),1110) ITMP
               ELSE
                  WRITE (MSGBUF(4:),1111) ITMP
                  END IF
            ELSE
               IF (ITMP.LT.10) THEN
                  WRITE (MSGBUF(1:),1110) ITMP
               ELSE
                  WRITE (MSGBUF(1:),1111) ITMP
                  END IF
               END IF
            CALL REFRMT (MSGBUF, '_', INCHAR)
            X = (I - 0.5) * XDIST
            CALL GPOS (X, BLC(2), BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            DCX = -INCHAR/2.0
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
 120        CONTINUE
C                                       what plotted
         DCY = DCY - 1.333
         IF (ITY.EQ.1) THEN
            MSGBUF = 'SPECTRUM OF RMSES FROM TIME COMPUTATIONS'
         ELSE
            MSGBUF = 'SPECTRUM OF DEVIATIONS FROM SPECTRAL WINDOW'
     *         // ' COMPUTATIONS'
            END IF
         CALL REFRMT (MSGBUF, '_', INCHAR)
         CALL GPOS (BLC(1), BLC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         DCX = 0.0
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Determine label range
      CALL PINLAB (BLC, TRC, 1, NC, NI, LABEL, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Label with prefix
      DCY = -1.5
      IF (LTYPE.GT.2) DCY = -2.833
      X = (TRC(1) + BLC(1)) / 2.0
      CALL GPOS (X, BLC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      MSGBUF = 'Spectral channels'
      CALL CHTRIM (MSGBUF, 17, MSGBUF, INCHAR)
      DCX = 0.5 - INCHAR / 2.0
      CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (LTYPE.GE.7) GO TO 999
C                                       which axis is which?
      NXFR = 0
      NXST = 0
      NAX = CATBLK(KIDIM)
      INC = 2
      DO 200 I = 1,NAX
         ICPNT = KHCTP+(I-1)*INC
         CALL H2CHR (8, 1, CATH(ICPNT), CTEMP)
         IF (CTEMP(1:4).EQ.'FREQ') NXFR  = I
         IF (CTEMP(1:4).EQ.'STOK') NXST  = I
 200     CONTINUE
C                                       Source name, stokes, freq.
      CALL GPOS (BLC(1), TRC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      DCX = 0.0
      DCY = 0.5
      IANGL = 0
      CALL H2CHR (8, 1, CATH(KHOBJ), CTEMP)
      FREQ = 0.0
      JSTOK = 12
      IF (NXFR.GT.2) FREQ = CATD(KDCRV+NXFR-1) + CATR(KRCIC+NXFR-1)
     *   * (BLC(NXFR) - CATR(KRCRP+NXFR-1))
      FREQ = FREQ / 1.E6
      IF (NXST.GT.2) JSTOK = CATD(KDCRV+NXST-1) + CATR(KRCIC+NXST-1)
     *   * (BLC(NXST) - CATR(KRCRP+NXST-1)) + 2.5
      IF (NXFR.GT.2) WRITE (MSGBUF,1200) CTEMP, CSTOK(JSTOK),
     *   FREQ
      IF (NXFR.LE.2) WRITE (MSGBUF,1200) CTEMP, CSTOK(JSTOK)
      CALL REFRMT (MSGBUF, '_', INCHAR)
C                                       image name
      INCHAR = INCHAR + 1
      IF (INCHAR.GT.1) THEN
         MSGBUF(INCHAR:INCHAR+2) = '  _'
         INCHAR = INCHAR + 3
         END IF
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMSTR(1:12))
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), NAMSTR(13:18))
      CALL NAMEST (NAMSTR, CATBLK(KIIMS), MSGBUF(INCHAR:), ITMP)
      CALL REFRMT (MSGBUF, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       time/date, version
      IF (LABEL.GT.0) THEN
         CALL GPOS (BLC(1), TRC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, TIME, DATE)
         WRITE (MSGBUF,1210) IVER, DATE, TIME
         CALL REFRMT (MSGBUF, '_', INCHAR)
         DCY = DCY + 1.333
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (F14.3)
 1110 FORMAT (I1)
 1111 FORMAT (I2)
 1200 FORMAT (A,'  _',A4,'_ ',F10.3,' MHz')
 1210 FORMAT ('Plot file version',I4,'__created ',A,A)
      END
      SUBROUTINE PINLAB (BLC, TRC, BC, EC, NG, ILTYPE, BUFFER, IERR)
C-----------------------------------------------------------------------
C   To do X axis where we have multiple sub panels with integer counts
C   Inputs:
C      BLC      R(2)    X, Y pixels to form bottom left hand corner
C      TRC      R(2)    X, Y pixels to form the top right hand corner
C      BC       I       Begin count
C      EC       I       End count
C      NG       I       Number of such groups
C      ILTYPE   I       label type: 1 none, 2 no ticks, 3 RA/DEC
C                          4 center relative
C   In/out:
C      BUFFER   I(256)  the updated graphics output buffer.
C      IERR     I       error indicator: 0 = No error.
C-----------------------------------------------------------------------
      REAL      BLC(2), TRC(2)
      INTEGER   BC, EC, NG, ILTYPE, BUFFER(256), IERR
C
      INTEGER   NINTER
      PARAMETER (NINTER=15)
C
      INTEGER   INCHAR, LTYPE, I, XINTER(NINTER), XINT, DIST, NOINT,
     *   NINT, IG, XVAL
      REAL      DCX, DCY, XL, XI, XPOS
      INCLUDE 'INCS:DMSG.INC'
      DATA XINTER /1, 2, 5, 10, 20, 50, 100, 200, 500, 1000,
     *   2000, 5000, 10000, 20000, 50000/
C-----------------------------------------------------------------------
      LTYPE = MOD (ABS (ILTYPE), 100)
      IF (LTYPE.EQ.1) GO TO 999
C                                       tick marks
      XINT = 32 / NG
      XINT = MAX (3, MIN (16, XINT))
      DIST = EC - BC + 2
      DO 20 I = 1,NINTER
         NOINT = DIST / XINTER(I)
         IF (NOINT.LE.XINT) GO TO 30
 20      CONTINUE
      MSGTXT = 'PINLAB: TICK MARK ALGORITHM FAILED'
      CALL MSGWRT (6)
      IERR = 0
      GO TO 999
C                                       plot tick marks
 30   XINT = XINTER(I)
      NINT = (NOINT * NG) / 16
      NINT = MAX (1, NINT)
      NOINT = NOINT + 2
      DCX = -0.5
      XL = DIST * NG + 1
      XL = (TRC(1) - BLC(1)) / XL
      XI = (TRC(2) - BLC(2)) / 25.
      DCY = -1.5
      DO 50 IG = 1,NG
         XVAL = (BC / XINT) * XINT
         IF (XVAL.EQ.BC) XVAL = XVAL - XINT
         DO 40 I = 1,NOINT
            XVAL = XVAL + XINT
            IF ((XVAL.GE.BC) .AND. (XVAL.LE.EC)) THEN
               XPOS = (XVAL-BC+1 + (IG-1)*DIST) + BLC(1)
               CALL GPOS (XPOS, TRC(2), BUFFER, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL GVEC (XPOS, TRC(2)-XI, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL GPOS (XPOS, BLC(2)+XI, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL GVEC (XPOS, BLC(2), BUFFER, IERR)
               IF (IERR.NE.0) GO TO 999
               IF ((LTYPE.GT.2) .AND. (MOD(I,NINT).EQ.0)) THEN
                  WRITE (MSGTXT,1030) XVAL
                  CALL CHTRIM (MSGTXT, 6, MSGTXT, INCHAR)
                  DCX = 0.5 - INCHAR
                  CALL GCHAR (INCHAR, 0, DCX, DCY, MSGTXT, BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 999
                  END IF
               END IF
 40         CONTINUE
 50      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (I6)
      END
      SUBROUTINE RFLATE (NC, NI, RMSRMS, DEVRMS, DOOUT, OUTEXT)
C-----------------------------------------------------------------------
C   RFLATE writes out the data weights by channel to a text file
C   Inputs:
C      NC       I      Number spectral channels
C      NI       I      Number IFs
C      RMSRMS   D(*)   Time based rms array
C      DEVRMS   D(*)   Freq based rms array
C      DOOUT    R      User choice of which rms to use
C      OUTEXT   C*48   Output text file name
C-----------------------------------------------------------------------
      INTEGER   NC, NI
      DOUBLE PRECISION RMSRMS(3,NC,*), DEVRMS(3,NC,*)
      REAL      DOOUT
      CHARACTER OUTEXT*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LC, LI, L, I, J, LUN, LUNTMP, TIME(3), DATE(3), ITRIM,
     *   FIND, IRET
      REAL      WTS(MAXCIF), WTMIN
      CHARACTER LINE*72, ATIME*8, ADATE*12
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      L = DOOUT + 0.5
      L = MAX (1, L)
      I = 0
      J = 0
      DO 30 LI = 1,NI
         WTMIN = 1.E10
         DO 10 LC = 1,NC
            I = I + 1
            WTS(I) = -1.0
            IF (L.EQ.1) THEN
               IF (RMSRMS(1,LC,LI).GT.0.0D0) THEN
                  WTS(I) = RMSRMS(1,LC,LI)
                  WTMIN = MIN (WTMIN, WTS(I))
                  END IF
            ELSE IF (L.EQ.2) THEN
               IF (DEVRMS(1,LC,LI).GT.0.0D0) THEN
                  WTS(I) = DEVRMS(1,LC,LI)
                  WTMIN = MIN (WTMIN, WTS(I))
                  END IF
            ELSE
               IF ((RMSRMS(1,LC,LI).GT.0.0D0) .OR.
     *            (DEVRMS(1,LC,LI).GT.0.0D0)) THEN
                  WTS(I) = SQRT (RMSRMS(1,LC,LI)**2 +
     *               DEVRMS(1,LC,LI)**2)
                  WTMIN = MIN (WTMIN, WTS(I))
                  END IF
               END IF
 10         CONTINUE
C                                       scale into weight factors
         DO 20 LC = 1,NC
            J = J + 1
            IF (WTMIN.GT.0.0) THEN
               IF (WTS(J).GT.0.0) THEN
                  WTS(J) = (WTMIN / WTS(J)) ** 2
               ELSE
                  WTS(J) = 1.0
                  END IF
            ELSE
               WTS(J) = 1.0
               END IF
 20         CONTINUE
 30      CONTINUE
C                                       open text file
      LUN = LUNTMP (2)
      CALL ZTXOPN ('WRIT', LUN, FIND, OUTEXT, .TRUE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING TEXT FILE'
         GO TO 990
         END IF
C                                       comment
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      LINE = '# ' // TSKNAM // ADATE // ATIME
      J = ITRIM (LINE)
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(:J), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITING TEXT FILE'
         GO TO 990
         END IF
C                                       write values
      I = 0
      DO 50 LI = 1,NI
         DO 40 LC = 1,NC
            I = I + 1
            WRITE (LINE,1030) WTS(I), LC, LI
            CALL ZTXIO ('WRIT', LUN, FIND, LINE(:21), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING TEXT FILE'
               GO TO 990
               END IF
 40         CONTINUE
 50      CONTINUE
C
      CALL ZTXCLS (LUN, FIND, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RFLATE: ERROR',I4,' ON ',A)
 1030 FORMAT ('W ',F9.4,I7,I3)
      END
      SUBROUTINE DATFLG (RPARM, VIS, DROP, IERR)
C-----------------------------------------------------------------------
C   Flags data specified in flagging table
C   Inputs:
C      RPARM(*)   R    Random parameter array
C      VIS(3,*)   R    Visibility array
C   Inputs from include DSEL.INC:
C      CURSOU     I    Current source number
C      NUMFLG     I    Number of flagging entries.
C      TMFLST     R    Time of last visibility for which flagging
C                      was checked.
C      FLGSOU(*)  I    Source id numbers to flag, 0=all.
C      FLGANT(*)  I    Antenna numbers to flag, 0=all.
C      FLGBAS(*)  I    Baseline (A1*32768+A2) numbers to flag, 0=all.
C      FLGSUB(*)  I    Subarray numbers to flag, 0=all.
C      FLGFQD(*)  I    Freqid numbers to flag, <=0=all.
C                      Following should have defaults filled in.
C      FLGBIF(*)  I    First IF to flag.
C      FLGEIF(*)  I    Highest IF to flag.
C      FLGBCH(*)  I    First channel to flag.
C      FLGECH(*)  I    Highest channel to flag.
C      FLGPOL(4,*)L    Flags for the polarizations, should correspond
C                      to selected polarization types.
C   Output:
C      RPARM(*)   R    Random parameter array
C      VIS(3,*)   R    Visibility array
C      DROP       L    True if data all flagged.
C      IERR       I    Return code, 0=OK, else NXTFLG error number.
C-----------------------------------------------------------------------
      REAL      RPARM(*), VIS(3,*)
      LOGICAL   DROP
      INTEGER   IERR
C
      INTEGER   IFLAG, KBASE, A1, A2, FLGA, SUBA, JIF, JCHAN, JPOLN,
     *   LIMF1, LIMF2, LIMC1, LIMC2, IFADD, INDEX, STADD, IPOLPT, LFQ
      LOGICAL   GOOD
      REAL      TIME, SUM
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      DROP = .FALSE.
C                                       Check if new time
      TIME = RPARM(1+ILOCT)
      IF (TMFLST.LT.TIME) CALL NXTFLG (TIME, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Check if there are current flags
      IF (NUMFLG.LE.0) GO TO 999
C                                       Loop thru flagging criteria
      IF (ILOCB.GE.0) THEN
         KBASE = RPARM(1+ILOCB) + 0.1
         A1 = KBASE / 256 + 0.1
         A2 = KBASE - 256 * A1 + 0.1
         SUBA = (RPARM(1+ILOCB) - KBASE) * 100.0 + 1.5
      ELSE
         A1 = RPARM(1+ILOCA1) + 0.1
         A2 = RPARM(1+ILOCA2) + 0.1
         SUBA = RPARM(1+ILOCSA)
         END IF
      KBASE = 32768 * MIN (A1,A2) + MAX (A1,A2)
      DO 500 IFLAG = 1,NUMFLG
C                                       Check time if needed
         IF (.NOT.TIMORD) THEN
            IF ((TIME.LT.FLGTST(IFLAG)) .OR. (TIME.GT.FLGTND(IFLAG)))
     *         GO TO 500
            END IF
C                                       Check source
         IF ((FLGSOU(IFLAG).NE.CURSOU) .AND. (FLGSOU(IFLAG).NE.0) .AND.
     *      (CURSOU.NE.0)) GO TO 500
C                                       Check antenna
         FLGA = FLGANT(IFLAG)
         IF ((FLGA.NE.0) .AND. (FLGA.NE.A1) .AND. (FLGA.NE.A2))
     *      GO TO 500
C                                       Check baseline
         IF ((FLGBAS(IFLAG).NE.0) .AND. (FLGBAS(IFLAG).NE.KBASE))
     *      GO TO 500
C                                       Check subarray
         IF ((FLGSUB(IFLAG).GT.0) .AND. (FLGSUB(IFLAG).NE.SUBA))
     *      GO TO 500
C                                       Check freqid.: may be changed
C                                       from input to 1 already
         IF (ILOCFQ.GE.0) THEN
            IF (FRQSEL.GT.0) THEN
               LFQ = FRQSEL
            ELSE
               LFQ = RPARM(1+ILOCFQ) + 0.1
               END IF
            IF ((FLGFQD(IFLAG).GT.0) .AND. (FLGFQD(IFLAG).NE.LFQ) .AND.
     *         (LFQ.GT.0)) GO TO 500
            END IF
C                                       Some data to be flagged
C                                       Set limits
         LIMF1 = FLGBIF(IFLAG)
         LIMF2 = FLGEIF(IFLAG)
         LIMC1 = FLGBCH(IFLAG)
         LIMC2 = FLGECH(IFLAG)
C                                       Loop over polarizations
         IPOLPT = ABS(KCOR0) - 1
         IF (KCOR0.LT.-4) IPOLPT = IPOLPT - 4
         DO 400 JPOLN = 1,KNCOR
            IF (FLGPOL(JPOLN+IPOLPT,IFLAG)) THEN
               STADD = (JPOLN-1) * KNCS + 1
C                                       Loop over IF
               DO 300 JIF = LIMF1,LIMF2
                  INDEX = STADD + (JIF-1) * KNCIF + (LIMC1-1) * KNCF
                  IF (LIMC1.EQ.LIMC2) THEN
C                                       Single channel
                     VIS(3,INDEX) = - ABS (VIS(3,INDEX))
                  ELSE
C                                       Loop over channel
                     DO 200 JCHAN = LIMC1,LIMC2
C                                       Flag
                        VIS(3,INDEX) = - ABS (VIS(3,INDEX))
                        INDEX = INDEX + KNCF
 200                    CONTINUE
                     END IF
 300              CONTINUE
               END IF
 400        CONTINUE
 500     CONTINUE
C                                       Check if data all bad
      GOOD = .FALSE.
C                                       Loop over IF
      DO 530 JIF = BIF,EIF
         IFADD = (JIF-1) * KNCIF + 1
C                                       Loop over polarizations
         DO 520 JPOLN = 1,KNCOR
            INDEX = IFADD + (JPOLN-1) * KNCS + (BCHAN-1) * KNCF
C                                       Single channel
            IF (BCHAN.EQ.ECHAN) THEN
               GOOD = GOOD .OR. (VIS(3,INDEX).GT.0.0)
C                                       Multiple channels
            ELSE
               SUM = 0.0
               DO 510 JCHAN = BCHAN,ECHAN
                  SUM = SUM + MAX (0.0, VIS(3,INDEX))
                  INDEX = INDEX + KNCF
 510              CONTINUE
               GOOD = GOOD .OR. (SUM.GT.0.0)
               END IF
 520        CONTINUE
 530     CONTINUE
      DROP = .NOT.GOOD
C
 999  RETURN
      END
      SUBROUTINE NXTFLG (TIME, TABLE, IERR)
C-----------------------------------------------------------------------
C   Updates flagging tables in common fron an FG table.
C   Inputs:
C      TIME     R      Current time (days) for flag entries
C      TABLE    L      If table true then ignore baseline dependent
C                      and channel dependent flags
C   Inputs from common /CFMINF/(INCLUDEs C/DSEL.INC):
C      NUMFLG   I      number of current FLAG entries.
C      FGKOLS   I(MAXFGC)   The column pointer array in order, SOURCE,
C                      SUBARRAY, FREQID, ANTS, TIMERANG, IFS, CHANS,
C                      PFLAGS, REASON
C      FGNUMV   I(MAXFGC)   Element count for each column
C      IFGRNO   I      Current FLAG file record.
C   Output to common /CFMINF/:
C      NUMFLG   I      Number of flagging entries.
C      TMFLST   R      Time of last visibility for which flagging
C                      was checked.
C      FLGSOU   I(*)   Source id numbers to flag, 0=all.
C      FLGANT   I(*)   Antenna numbers to flag, 0=all.
C      FLGBAS   I(*)   Baseline (A1*32768+A2) numbers to flag, 0=all.
C      FLGSUB   I(*)   Subarray numbers to flag, 0=all.
C      FLGFQD   I(*)   Freqid numbers to flag, <=0=all.
C                      Following should have defaults filled in.
C      FLGBIF   I(*)   First IF to flag.
C      FLGEIF   I(*)   Highest IF to flag.
C      FLGBCH   I(*)   First channel to flag.
C      FLGECH   I(*)   Highest channel to flag.
C      FLGPOL   L(4,*)   Flags for the polarizations, should correspond
C                      to selected polarization types.
C      FLGTST   R(*)   Start time of flag.
C      FLGTND   R(*)   End time of flag.
C   Output:
C      IERR     I      Return code, 0=OK, else TABIO error number.
C                         10 => too many flags
C-----------------------------------------------------------------------
      REAL      TIME
      LOGICAL   TABLE
      INTEGER   IERR
C
      INTEGER   J, NDROP, LIMIT, RECI(30), MXFLG, SOUKOL, SUBKOL,
     *   FRQKOL, ANTKOL, TIMKOL, IFKOL, CHKOL, POLKOL, REAKOL, A1, A2,
     *   IT, I4, NFGREC, I, LIMIT4
      REAL      RECORD(31)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECORD, RECI)
      EQUIVALENCE (FGKOLS(1), SOUKOL), (FGKOLS(2), SUBKOL),
     *   (FGKOLS(3), FRQKOL), (FGKOLS(4), ANTKOL), (FGKOLS(5),TIMKOL),
     *   (FGKOLS(6), IFKOL),  (FGKOLS(7), CHKOL), (FGKOLS(8), POLKOL),
     *   (FGKOLS(9), REAKOL)
      DATA I4 /4/
C-----------------------------------------------------------------------
      IERR = 0
      MXFLG = MAXFLG
      TMFLST = TIME
C                                       Check if any flags expired.
C                                       Check if any flags expired.
 10   NDROP = 0
C                                       Find highest number expired flag
      IF ((NUMFLG.GT.0) .AND. (TIMORD)) THEN
         DO 20 I = 1,NUMFLG
            IF (FLGTND(I).LT.TIME) NDROP = I
 20         CONTINUE
         END IF
C                                       Compress, dropping flag.
      IF (NDROP.GT.0) THEN
         IF (NDROP.LT.NUMFLG) THEN
            LIMIT = NDROP + 1
            DO 150 I = LIMIT,NUMFLG
               IT = I - 1
               FLGTST(IT) = FLGTST(I)
               FLGTND(IT) = FLGTND(I)
               FLGSOU(IT) = FLGSOU(I)
               FLGANT(IT) = FLGANT(I)
               FLGFQD(IT) = FLGFQD(I)
               FLGBAS(IT) = FLGBAS(I)
               FLGSUB(IT) = FLGSUB(I)
               FLGBIF(IT) = FLGBIF(I)
               FLGEIF(IT) = FLGEIF(I)
               FLGBCH(IT) = FLGBCH(I)
               FLGECH(IT) = FLGECH(I)
               FLGPOL(1,IT) = FLGPOL(1,I)
               FLGPOL(2,IT) = FLGPOL(2,I)
               FLGPOL(3,IT) = FLGPOL(3,I)
               FLGPOL(4,IT) = FLGPOL(4,I)
 150           CONTINUE
            END IF
         NUMFLG = NUMFLG - 1
         GO TO 10
         END IF
C                                       Find next valid flag.
      NFGREC = FGBUFF(5)
C                                       Check if list exhausted
      IF (IFGRNO.GT.NFGREC) GO TO 999
C                                       Loop through records
 310  LIMIT4 = IFGRNO
      DO 360 I = LIMIT4,NFGREC
         IFGRNO = I
         IERR = 1
C                                       Read record.
         CALL TABIO ('READ', 0, IFGRNO, RECORD, FGBUFF, IERR)
C                                       Check if flagged
         IF (IERR.LT.0) GO TO 360
C                                       Check error
         IF (IERR.GT.0) GO TO 999
C                                       Check time.
         IF (TIMORD) THEN
            IF (TIME.LT.RECORD(TIMKOL)) GO TO 999
            IF (TIME.GT.RECORD(TIMKOL+1)) GO TO 360
            END IF
C                                       Check FQ ID.
         IF (RECI(FRQKOL).GT.0) THEN
            IF ((RECI(FRQKOL).NE.FRQSEL) .AND. (FRQSEL.GT.0) .AND.
     *         (RECI(FRQKOL).GT.0)) GO TO 360
            END IF
C                                       Check that starting IF
C                                       is in range
         IF ((RECI(IFKOL).GT.0).AND.
     *      (RECI(IFKOL).GT.CATUV(KINAX+KLOCIF))) GO TO 360
C                                       Check that starting
C                                       channel is in range
         IF ((RECI(CHKOL).GT.0).AND.
     *      (RECI(CHKOL).GT.CATUV(KINAX+KLOCFY))) GO TO 360
C                                       Does source number matter?
         IF ((RECI(SOUKOL).LE.0) .OR. (NSOUWD.LE.0)) GO TO 500
C                                       Search source lists
C                                       in UVCOP, SOUWAN is
C                                       list of wanted sources
         DO 340 J = 1,NSOUWD
            IF (RECI(SOUKOL).EQ.SOUWAN(J)) GO TO 500
 340        CONTINUE
 360     CONTINUE
      IERR = 0
      GO TO 999
C                                       Next entry
 500  NUMFLG = NUMFLG + 1
C                                       Check if too big
      IERR = 0
C                                       Fill in tables
      FLGTST(NUMFLG) = RECORD(TIMKOL)
      FLGTND(NUMFLG) = RECORD(TIMKOL+1)
      FLGSOU(NUMFLG) = RECI(SOUKOL)
      FLGFQD(NUMFLG) = RECI(FRQKOL)
      A1 = MIN (RECI(ANTKOL), RECI(ANTKOL+1))
      A2 = MAX (RECI(ANTKOL), RECI(ANTKOL+1))
      IF (A1.LE.0) THEN
         FLGANT(NUMFLG) = A2
         FLGBAS(NUMFLG) = 0
      ELSE
         FLGANT(NUMFLG) = RECI(ANTKOL)
         FLGBAS(NUMFLG) = A1*32768 + A2
         END IF
      FLGSUB(NUMFLG) = RECI(SUBKOL)
      FLGBIF(NUMFLG) = RECI(IFKOL)
      FLGEIF(NUMFLG) = RECI(IFKOL+1)
      IF (FLGBIF(NUMFLG).LE.0) FLGBIF(NUMFLG) = 1
      IF (FLGEIF(NUMFLG).LE.0) THEN
         IF (KLOCIF.GT.0) FLGEIF(NUMFLG) = CATUV (KINAX+KLOCIF)
         IF (KLOCIF.LE.0) FLGEIF(NUMFLG) = 1
         END IF
      FLGBCH(NUMFLG) = RECI(CHKOL)
      FLGECH(NUMFLG) = MIN (CATUV(KINAX+KLOCFY), RECI(CHKOL+1))
      IF (FLGBCH(NUMFLG).LE.0) FLGBCH(NUMFLG) = 1
      IF (FLGECH(NUMFLG).LE.0) FLGECH(NUMFLG) = CATUV (KINAX+KLOCFY)
C                                       Ensure that IF and channel
C                                       selection are in range
      FLGEIF(NUMFLG) = MIN (FLGEIF(NUMFLG), CATUV(KINAX+KLOCIF))
      FLGECH(NUMFLG) = MIN (FLGECH(NUMFLG), CATUV(KINAX+KLOCFY))
C
      CALL LG2BIT (I4, FLGPOL(1,NUMFLG), RECI(POLKOL), -1)
C                                       table ignores baseline based
C                                       and channel based
      IF (TABLE) THEN
         IF ((FLGBAS(NUMFLG).NE.0) .OR. (FLGBCH(NUMFLG).NE.1) .OR.
     *      (FLGECH(NUMFLG).NE.CATUV(KINAX+KLOCFY))) NUMFLG = NUMFLG - 1
         END IF
C                                       test for at limit
      IF (NUMFLG.GT.MXFLG-1) THEN
         IERR = 10
         WRITE (MSGTXT,1500) MXFLG-1
         GO TO 990
         END IF
C                                       Increment flag counter
      IFGRNO = IFGRNO + 1
C                                       Loop back for next
      IF (IFGRNO.LE.NFGREC) GO TO 310
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1500 FORMAT ('TOO MANY FLAGS AT SAME TIME (>',I7,')')
      END
      SUBROUTINE UVSCAL (LLIF, LLANT, RFSCAL, RPARM, VIS)
C-----------------------------------------------------------------------
C   Applies visibility scaling to the data if requested
C   Inputs:
C      RPARM   R(*)     Random parameters
C   In/Out:
C      VIS     R(3,*)   Visibilities
C-----------------------------------------------------------------------
      INTEGER   LLIF, LLANT
      REAL      RFSCAL(LLIF,LLANT,LLANT,*), RPARM(*), VIS(3,*)
C
      INTEGER   LI, LF, LS, NI, NF, NS, IA1, IA2, ISRC, IND
      REAL      SCAL, WTSCAL, TEMP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'RFSCALE.INC'
      INCLUDE 'RFLAG.INC'
C-----------------------------------------------------------------------
      IF (DOSCAL) THEN
         IF (ILOCB.GE.0) THEN
            TEMP = RPARM(1+ILOCB)
            IA1 = TEMP / 256 + 0.01
            IA2 = TEMP - 256*IA1 + 0.01
         ELSE
            IA1 = RPARM(ILOCA1) + 0.1
            IA2 = RPARM(ILOCA2) + 0.1
            END IF
         ISRC = 1
         IF (ILOCSU.GE.0) ISRC = RPARM(1+ILOCSU) + 0.01
         ISRC = TRSORC(ISRC)
         NS = CATBLK(KINAX+JLOCS)
         NF = CATBLK(KINAX+JLOCF)
         NI = 1
         IF (JLOCIF.GE.0) NI = CATBLK(KINAX+JLOCIF)
          DO 30 LI = 1,NI
            SCAL = RFSCAL(LI,IA1,IA2,ISRC)
            IF (SCAL.GT.0.0) THEN
               WTSCAL = SCAL * SCAL
               SCAL = 1.0 / SCAL
                DO 20 LS = 1,NS
                  IND = (LI - 1) * INCIFI + (LS - 1) * INCSI + 1
                  DO 10 LF = 1,NF
                     VIS(1,IND) = VIS(1,IND) * SCAL
                     VIS(2,IND) = VIS(2,IND) * SCAL
                     VIS(3,IND) = VIS(3,IND) * WTSCAL
                     IND = IND + INCFI
 10                  CONTINUE
 20               CONTINUE
               END IF
 30         CONTINUE
         END IF
C
 999  RETURN
      END
