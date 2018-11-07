LOCAL INCLUDE 'BPWAY.INC'
C                                       Local include for BPWAY
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMEO(3), XCLAOU(2), XSOUR(4,30),
     *   XCALC, XOPTYP
      REAL      XSIN, XDISIN, XSOU, XDISOU, XQUAL, XTIME(8), XFQID,
     *   XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH(3), XBIF, XEIF, XCHNS(4,20), FPARM(30), XCENT,
     *   BADD(10)
      REAL      SCRBUF(256), BUFF1(UVBFSS), BUFF2(UVBFSS), MYTIME(2),
     *   BUFF3(UVBFSS), DIFPIX
      INTEGER   SEQIN, DISKIN, SEQOU, DISKOU, JBUFSZ, CATOLD(256),
     *   INCSI, INCFI, INCIFI, NRPRMI, OLDCNO, NEWCNO, NIF, NSTOK,
     *   CHNSEL(3,20,MAXIF), NANT, NCHAN, LIF, NPARM, LBIF, AVGCHN,
     *   XTRCHN, MYSUBA, MYFQID, MYNSOU, MYSOUW(30), COUNT
      LOGICAL   MYDOSW
      CHARACTER NAMEIN*12, CLAIN*6, NAMEOU*12, CLAOU*9, OPTYPE*4
      DOUBLE PRECISION UVSCAL
C                                       XPARM included to save info
C                                       for EXTLIST
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMEO, XCLAOU,
     *   XSOU, XDISOU, XOPTYP, XSOUR, XQUAL, XCALC, XTIME, XFQID, XSUBA,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH, XBIF, XEIF, XCHNS, FPARM, XCENT, BADD
      COMMON /BPWAYP/ CATOLD, UVSCAL, SEQIN, DISKIN, SEQOU, DISKOU,
     *   INCSI, INCFI, INCIFI, NRPRMI, OLDCNO, CHNSEL, NIF, NSTOK, NANT,
     *   NCHAN, LIF, NPARM, LBIF, AVGCHN, XTRCHN, MYSUBA, MYFQID,
     *   MYTIME, MYNSOU, MYSOUW, MYDOSW, NEWCNO, COUNT, DIFPIX
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMEOU, CLAOU, OPTYPE
      COMMON /BUFRS/ SCRBUF, BUFF1, BUFF2, BUFF3, JBUFSZ
C                                       End local include for BPWAY
LOCAL END
LOCAL INCLUDE 'WTABLE.INC'
      INTEGER   WTLUN, WTRNO, WTBUFF(512), WTKOLS(4), WTNUMV(4)
      COMMON /WTCOMM/ WTBUFF, WTKOLS, WTNUMV, WTLUN, WTRNO
LOCAL END
      PROGRAM BPWAY
C-----------------------------------------------------------------------
C! Finds channel-dependent relative weights
C# Utility UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2014-2015, 2018
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
C   BPWAY does SPLIT/SPLAT while calculating weights based on the rms
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
      INTEGER   IRET, NWORDS, IERR, NBL, NTIME, I, IB(2)
      REAL      B(2)
      LONGINT   PB
      EQUIVALENCE (B, IB)
      INCLUDE 'BPWAY.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'BPWAY '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL BPWAIN (PRGM, IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .FALSE.
         GO TO 990
         END IF
      NBL = (NANT * (NANT+1)) / 2
      NTIME = FPARM(1) + 0.1
      NTIME = (NTIME/2)*2 + 1
      IF (NTIME.LE.0) NTIME = 9
      NTIME = MIN (MAX (3, NTIME), 99)
      FPARM(1) = NTIME
      I = 12
      NWORDS = ((3 * NTIME * NBL) - 1) / 1024 + 1
      NWORDS = MAX (I, NWORDS)
      NWORDS = NWORDS * NCHAN * LIF * NSTOK
      CALL ZMEMRY ('GET ', PRGM, NWORDS, B, PB, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       find rmses and apply
      NWORDS = 1024 * NWORDS
      IF (OPTYPE.EQ.'SORC') THEN
         CALL BPWASU (NCHAN, NTIME, NANT, NBL, LIF, B(1+PB), NWORDS,
     *      IB(1+PB), IRET)
      ELSE
         OPTYPE = 'SCAN'
         CALL BPWANX (NCHAN, NTIME, NANT, NBL, LIF, B(1+PB), NWORDS,
     *      IB(1+PB), IRET)
         END IF
      IF (IRET.NE.0) GO TO 970
C                                       HI copy. added info
      CALL BPWAHI
C                                       free memory
 970  NWORDS = NWORDS / 1024
      CALL ZMEMRY ('FREE', PRGM, NWORDS, B, PB, IERR)
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE BPWAIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   BPWAIN gets input parameters for BPWAY and creates an output file
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
      INCLUDE 'BPWAY.INC'
      CHARACTER STAT*4, PTYPE*2, BLANK*6
      INTEGER   IROUND, IERR, INCX, I, LUN, NW(MAXIF), K, K1, K2, J
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
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA BLANK /' '/
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
      COUNT = 0
C                                       Get input parameters.
      NPARM = 281
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.NE.1) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
         END IF
C                                       Restart AIPS on failure
      IF (RQUICK) CALL RELPOP (JERR, SCRBUF, IERR)
      IF (JERR.NE.0) GO TO 999
C                                       Crunch input parameters.
      JERR = 5
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMEO, NAMEOU)
      CALL H2CHR (6, 1, XCLAOU, CLAOU)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SEQOU = IROUND (XSOU)
      DISKOU = IROUND (XDISOU)
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
     *   TIMRNG(1) = -1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)).EQ.0.0)
     *   TIMRNG(5) = 1.0E6
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      CALL RFILL (8, 0.0, XTIME)
      XTIME(1) = TSTART
      XTIME(5) = TEND
      MYTIME(1) = TSTART
      MYTIME(2) = TEND
      DOPOL = IROUND(XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      DOAPPL = .FALSE.
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LT.0) SUBARR = 0
      MYSUBA = SUBARR
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
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
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      MYFQID = FRQSEL
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
      CALL FILL (MAXIF, 0, NW)
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
 40      CONTINUE
C                                       If no channel selection
C                                       use VLA definition of
C                                       channel 0
      BCHAN = NCHAN
      ECHAN = 1
      DO 50 K = 1,MAXIF
         IF (NW(K).LE.0) THEN
            NW(K) = 1
            CHNSEL(1,1,K) = NCHAN / 8 + 1
            CHNSEL(2,1,K) = NCHAN - (NCHAN/8)
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
 50      CONTINUE
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
      MYNSOU = NSOUWD
      MYDOSW = DOSWNT
      CALL COPY (30, SOUWAN, MYSOUW)
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
      IF (XCENT.LE.0.0) THEN
         UVSCAL = 1.0D0
         DIFPIX = 0.0
      ELSE
         INCX = CATBLK(KINAX+JLOCF) / 2 + 1
         DIFPIX = INCX - CATR(KRCRP+JLOCF)
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *      DIFPIX
         CATR(KRCRP+JLOCF) = INCX
         UVSCAL = CATD(KDCRV+JLOCF) / FREQ
         END IF
C                                       Put new values in CATBLK
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMEOU, CLAOU, SEQOU)
      CALL CHR2H (12, NAMEOU, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOU, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOU
C                                       Create output file.
      NEWCNO = 1
      JERR = 4
      CALL UVCREA (DISKOU, NEWCNO, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKOU
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 2
      CALL UVPGET (IERR)
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKOU, NEWCNO, IERR)
C                                       parameter check
      IF (FPARM(3).LE.0.0) FPARM(3) = 1.E8
      FPARM(3) = FPARM(3) * FPARM(3)
      I = IROUND (FPARM(4))
      IF ((I.LT.1) .OR. (I.GT.5)) I = 1
      FPARM(4) = I
      IF (FPARM(5).LE.0.0) FPARM(5) = 10.0
      IF (FPARM(6).LE.0.0) THEN
         IF (I.EQ.2) THEN
            FPARM(6) = 3.0 * FPARM(5)
         ELSE IF (I.EQ.3) THEN
            FPARM(6) = 4.0 * FPARM(5)
         ELSE IF (I.EQ.4) THEN
            FPARM(6) = 2.0 * FPARM(5)
         ELSE
            FPARM(6) = 10.0
            END IF
         END IF
      FPARM(5) = FPARM(5) / (24.0 * 60.0)
      FPARM(6) = FPARM(6) / (24.0 * 60.0)
      IF (FPARM(7).LE.0.0) THEN
         FPARM(7) = 1.E8
      ELSE
         FPARM(7) = 1.0 / SQRT (FPARM(7))
         END IF
      IF (FPARM(8).LE.0.0) THEN
         FPARM(8) = 0.01
      ELSE
         FPARM(8) = 1.0 / SQRT (FPARM(8))
         END IF
      JERR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPWAIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
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
      SUBROUTINE BPWANX (NC, NT, NA, NB, NI, B, NWD, INDEX, IRET)
C-----------------------------------------------------------------------
C   Scan by scan, BPWANX accumulates a buffer of NT times for all
C   baselines, etc.  It then finds the rms over time for each channel,
C   IF, stokes and writes this to a table.  It then sorts, smooths, and
C   resorts the table.  Then it rereads the input file for that scan
C   applying normalized, channel-dependent weights to the output data.
C   Inputs:
C      NC      I      Number spectral channels
C      NT      I      Number times to accumulate
C      NA      I      Number antennas
C      NB      I      Max baseline number
C      NI      I      Max IF in data
C      NWORDS  I      Number woords in INDEX total
C   Output:
C      B       R(*)   Big buffer to accumulate the vis
C      INDEX   I(*)   Equivalenced buffer to B (NB+2,*)
C      IRET    I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NC, NT, NA, NB, NI, NWD, INDEX(NB+2,*), IRET
      REAL      B(3,NC,NT,NB,NI,*)
C
      INTEGER   NTMAX
      PARAMETER (NTMAX = 99)
      INCLUDE 'BPWAY.INC'
      INCLUDE 'WTABLE.INC'
      INTEGER   NUMVIS, CATMP(256), CHFLGS(MAXCIF), NW, NVM, NTIMES, I,
     *   TNUM(NTMAX), VISN(2,NTMAX), LSOU, ISOU, NCT, CT, J1, J2, JT,
     *   NRBL(MAXANT,MAXANT), VER, NUMNX, INX, ISONE(MXBASE*NTMAX),
     *   LUNTMP, KOLSNX(MAXNXC), NUMVNX(MAXNXC), RNONX, BUFFNX(512),
     *   LUNX, NXSOUR, NXSUBA, VSTART, VEND, NXFQID, JB, KEY(2,2),
     *   KEYSUB(2,2), NTS, RNXRET, LUNOU, FINDOU, VO, ILENBU, BO, KBIND,
     *   IPTRO, NIOUT, NIOLIM, JA1, JA2, JBL, NCOPY, JI, JP, INDI, INDO,
     *   JF, NWORDS, MAXC, CTLIM, XCOUNT
      LOGICAL   END, FIRST, GOTONE, GETNEW, LDUM
      LONGINT   PSMTH
      REAL      VIS(3,UVBFSS/3), RPARM(20), TB, TE,  TINT, TIME, TEPS,
     *   TIMES(2,NTMAX), TLIMIT, NXDTIM, NXTIME, RMSRMS(3,MAXCIF),
     *   FKEY(2,2), RMSB(3*MAXCIF), BASEN, SMTH(2)
      DOUBLE PRECISION NZERO, NDONE
      CHARACTER PHNAME*48, STEP*22
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (VIS, BUFF1), (RMSRMS, RMSB)
      DATA KEY    /5, 0, 1, 0/
      DATA FKEY   /1.0, 0.0, 1.0, 0.0/
      DATA KEYSUB /4*1/
      DATA VO, BO /0, 1/
C-----------------------------------------------------------------------
      TEPS = 0.1 / (24.0 * 3600.0)
      CTLIM = NWD / (NB+2)
      XCOUNT = 0
C                                       parameters
      NVM = NC * NT
      IF (FPARM(2).LE.0.0) FPARM(2) = 10.
      TLIMIT = 2.01 * FPARM(1) * FPARM(2)
      TLIMIT = TLIMIT / (24. * 3600.)
      TINT = 1.001 * FPARM(2) / (24. * 3600.)
      I = NC * NI * 3
      CALL RFILL (I, 0.0, RMSRMS)
C                                       counters, mask
      NCHAN = CATBLK(KINAX+JLOCF)
      CALL CHWANT (NCHAN, LIF, CHNSEL(1,1,LBIF), CHFLGS)
      NTIMES = 0
      I = MAXANT * MAXANT
      CALL FILL (I, 0, NRBL)
      I = MXBASE * NTMAX
      CALL FILL (I, 0, ISONE)
C                                       NX table
      LUNX = LUNTMP (1)
      VER = 1
      CALL NDXINI ('READ', BUFFNX, DISKIN, OLDCNO, 1, CATOLD, LUNX,
     *   RNONX, KOLSNX, NUMVNX, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING THE INDEX TABLE'
         GO TO 990
         END IF
      NUMNX = BUFFNX(5)
C                                       init NX table on output
      CALL RNXGET (DISKIN, OLDCNO, CATUV)
      CALL RNXINI (DISKOU, NEWCNO, CATBLK, RNXRET)
C                                       output file
      CALL ZPHFIL ('UV', DISKOU, NEWCNO, 1, PHNAME, IRET)
      LUNOU = LUNTMP (0)
      CALL ZOPEN (LUNOU, FINDOU, DISKOU, PHNAME, .TRUE., .FALSE.,
     *   .TRUE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT UV DATA SET'
         GO TO 990
         END IF
C                                       init I/O
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNOU, FINDOU, NVIS, VO, LREC, ILENBU,
     *   JBUFSZ, BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT I/O TO OUTPUT DATA SET'
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       WT table
      DO 900 INX = 1,NUMNX
         CALL TABNDX ('READ', BUFFNX, RNONX, KOLSNX, NUMVNX, NXTIME,
     *      NXDTIM, NXSOUR, NXSUBA, VSTART, VEND, NXFQID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING THE INDEX TABLE'
            GO TO 990
            END IF
C                                       do we want this scan?
         IF ((MYSUBA.GT.0) .AND. (MYSUBA.NE.NXSUBA)) GO TO 900
         IF ((MYFQID.GT.0) .AND. (MYFQID.NE.NXFQID)) GO TO 900
         TSTART = NXTIME - NXDTIM/2.0 - TEPS
         TEND = NXTIME + NXDTIM/2.0 + TEPS
         IF ((TEND.LT.MYTIME(1)) .OR. (TSTART.GT.MYTIME(2))) GO TO 900
         IF (MYNSOU.GT.0) THEN
            DO 10 I = 1,MYNSOU
               IF (NXSOUR.EQ.MYSOUW(I)) THEN
                  IF (.NOT.MYDOSW) GO TO 900
                  GO TO 15
                  END IF
 10            CONTINUE
            IF (MYDOSW) GO TO 900
            END IF
C                                       yes, set UVGET parms
 15      FRQSEL = NXFQID
         SUBARR = NXSUBA
         CALL RFILL (8, 0.0, TIMRNG)
         TIMRNG(1) = TSTART
         TIMRNG(5) = TEND
         INITVS = VSTART
         TB = -1000.0
         TE = TB
         NW = 3 * NC * NT * NB * NI * NSTOK
         CALL RFILL (NW, 0.0, B)
         LSOU = -1
         CT = 0
         NCT = 0
         FIRST = .TRUE.
         GOTONE = .FALSE.
         STEP = 'Starting'
         WRITE (MSGTXT,1015) STEP, INX
         CALL MSGWRT (2)
         NZERO = 0.0D0
         NDONE = 0.0D0
C                                       WT work table
         WTLUN = LUNTMP (1)
         VER = 1
         CALL WTINI ('WRIT', WTBUFF, DISKIN, OLDCNO, VER, CATOLD, WTLUN,
     *      WTRNO, WTKOLS, WTNUMV, NC, NI, NSTOK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING THE WT (WORK) TABLE'
            GO TO 990
            END IF
C                                       reset WT table
         WTRNO = 1
         WTBUFF(5) = 0
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
C                                       Loop
C                                       Read vis. record.
 100     CALL UVGET ('READ', RPARM, VIS, IRET)
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
               NUMVIS = NUMVIS + 1
               TIME = RPARM(1+ILOCT)
               END IF
            ISOU = 1
            IF (ILOCSU.GE.0) ISOU = RPARM(1+ILOCSU) + 0.01
            IF (END) ISOU = -1
            IF ((ABS(TB-TIME).LT.TINT) .AND. (ISOU.EQ.LSOU)) THEN
               CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B, ISONE,
     *            GETNEW)
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
C                                       just advance time counter
               IF ((NCT.LT.NT) .AND. (ISOU.EQ.LSOU) .AND.
     *            (TIME-TB.LT.TLIMIT)) THEN
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
     *               LDUM)
                  GOTONE = .TRUE.
                  TB = TIME
C                                       still going, clear 1 or more
               ELSE IF ((ISOU.EQ.LSOU) .AND. (TIME-TB.LT.TLIMIT)) THEN
                  J1 = (NT+1)/2
                  J2 = J1
                  IF (FIRST) J1 = 1
                  FIRST = .FALSE.
                  CALL RWRMSH (NC, NT, NA, NB, NI, NCT, J1, J2, TNUM,
     *               CHFLGS, B, RMSRMS, TIMES, VISN, IRET)
                  IF (IRET.NE.0) GO TO 999
C                                       next
                  CT = TNUM(1)
                  DO 150 JT = 1,NT-1
                     TNUM(JT) = TNUM(JT+1)
 150                 CONTINUE
                  JT = NB * (CT -1) + 1
                  CALL FILL (NB, 0, ISONE(JT))
                  TNUM(NT) = CT
                  TIMES(1,CT) = TIME
                  TIMES(2,CT) = TIME
                  VISN(1,CT) = NUMVIS
                  VISN(2,CT) = NUMVIS
                  CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B, ISONE,
     *               LDUM)
                  GOTONE = .TRUE.
                  TB = TIME
C                                       done with this scan
               ELSE
                  IF (GOTONE) THEN
                     J1 = (NT+1)/2
                     J2 = NCT
                     IF (FIRST) J1 = 1
                     CALL RWRMSH (NC, NT, NA, NB, NI, NCT, J1, J2, TNUM,
     *                  CHFLGS, B, RMSRMS, TIMES, VISN, IRET)
                     IF (IRET.NE.0) GO TO 999
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
                     CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B,
     *                  ISONE, LDUM)
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
C                                       close WT file and sort
         CALL TABWT ('CLOS', WTBUFF, WTRNO, WTKOLS, WTNUMV, TIMES, VISN,
     *      JB, RMSRMS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSING WT TABLE BEFORE SMOOTH'
            GO TO 990
            END IF
         STEP = 'Sorting weights for'
         WRITE (MSGTXT,1015) STEP, INX
         CALL MSGWRT (2)
         KEY(1,1) = 3
         KEY(1,2) = 1
         CALL TABSRT (DISKIN, OLDCNO, 'WT', VER, VER+1, KEY, KEYSUB,
     *      FKEY, BUFF1, CATOLD, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'SORTING TO BASELINE-TIME'
            GO TO 990
            END IF
C                                       count, average polarizations
         CALL BPWCNT (VER+1, NC, NI, RMSRMS, MAXC, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'COUNTING WT TABLE'
            GO TO 990
            END IF
C                                       smooth
         STEP = 'Smoothing weights for'
         WRITE (MSGTXT,1015) STEP, INX
         CALL MSGWRT (2)
         J1 = NC * NI * NSTOK
         NWORDS = ((MAXC + 2) * J1 - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', 'BPWANX', NWORDS, SMTH, PSMTH, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY FOR SMOOTHING'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         NTS = (NWORDS * 1024) / J1
         CALL BPSMOO (VER+1, VER+2, J1, NTS, SMTH(1+PSMTH), RMSRMS,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'SMOOTHING WT TABLE'
            GO TO 990
            END IF
         CALL ZMEMRY ('FREE', 'BPWANX', NWORDS, SMTH, PSMTH, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'UNABLE TO FREE DYNAMIC MEMORY FOR SMOOTHING'
            CALL MSGWRT (8)
            END IF
C                                       sort back
         STEP = 'Re-sorting weights for'
         WRITE (MSGTXT,1015) STEP, INX
         CALL MSGWRT (2)
         KEY(1,1) = 2
         KEY(1,2) = 3
         CALL TABSRT (DISKIN, OLDCNO, 'WT', VER+2, VER, KEY, KEYSUB,
     *      FKEY, BUFF1, CATOLD, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'SORTING TO TIME-BASELINE'
            GO TO 990
            END IF
         CALL RMEXT (DISKIN, OLDCNO, 'WT', VER+2, CATOLD, SCRBUF, IRET)
         CALL RMEXT (DISKIN, OLDCNO, 'WT', VER+1, CATOLD, SCRBUF, IRET)
C                                       set UVGET parms for re-read
         FRQSEL = NXFQID
         SUBARR = NXSUBA
         CALL RFILL (8, 0.0, TIMRNG)
         TIMRNG(1) = TSTART
         TIMRNG(5) = TEND
         INITVS = VSTART
         STEP = 'Applying weights for'
         WRITE (MSGTXT,1015) STEP, INX
         CALL MSGWRT (2)
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
C                                       WT work table
         WTLUN = LUNTMP (1)
         VER = 1
         CALL WTINI ('READ', WTBUFF, DISKIN, OLDCNO, VER, CATOLD, WTLUN,
     *      WTRNO, WTKOLS, WTNUMV, NC, NI, NSTOK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING THE WT (WORK) TABLE'
            GO TO 990
            END IF
C                                       make index of WT file
         CT = 0
         NTS = WTBUFF(5)
         DO 160 I = 1,NTS
            CALL TABWT ('READ', WTBUFF, WTRNO, WTKOLS, WTNUMV, TIMES,
     *         VISN, JB, RMSRMS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING WT FILE TO INDEX'
               GO TO 990
               END IF
            IF ((CT.EQ.0) .OR. ((VISN(1,1).NE.INDEX(1,CT)) .AND.
     *         (VISN(2,1).NE.INDEX(2,CT)))) THEN
               IF (CT.LT.CTLIM) THEN
                  CT = CT + 1
                  CALL FILL (NB+2, 0, INDEX(1,CT))
                  INDEX(1,CT) = VISN(1,1)
                  INDEX(2,CT) = VISN(2,1)
               ELSE
                  WRITE (MSGTXT,1160) CTLIM
                  IRET = 99
                  GO TO 990
                  END IF
               END IF
            INDEX(JB+2,CT) = I
 160        CONTINUE
         CT = 1
C                                       Loop
C                                       Read vis. record.
 200     CALL UVGET ('READ', RPARM, VIS, IRET)
         END = IRET.LT.0
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING DATA FIRST PASS'
            GO TO 990
C                                       got good data
         ELSE IF (IRET.EQ.0) THEN
            NUMVIS = NUMVIS + 1
            IF (ILOCB.GE.0) THEN
               BASEN = RPARM(1+ILOCB)
               JA1 = BASEN / 256. + 0.1
               JA2 = BASEN - JA1*256. + 0.1
            ELSE
               JA1 = RPARM(1+ILOCA1) + 0.1
               JA2 = RPARM(1+ILOCA2) + 0.1
               END IF
            JBL = NANT * (JA1-1) - ((JA1*(JA1-1))/2) + JA2
 205        IF (NUMVIS.GT.INDEX(2,CT)) THEN
               CT = CT + 1
               GO TO 205
               END IF
            WTRNO = INDEX(JBL+2,CT)
            CALL TABWT ('READ', WTBUFF, WTRNO, WTKOLS, WTNUMV, TIMES,
     *         VISN, JB, RMSB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING WT FILE TO INDEX'
               GO TO 990
               END IF
            NCOPY = 3 * NI * NSTOK * NC
            DO 240 JI = 1,NI
               DO 230 JP = 1,NSTOK
                  INDI = (JI-1) * INCIFI + (JP-1) * INCSI + 1 - INCFI
                  INDO = (JI-1) * NC + (JP - 1) * NC * NI
                  DO 220 JF = 1,NC
                     INDI = INDI + INCFI
                     INDO = INDO + 1
                     IF (VIS(3,INDI).GT.0.0) THEN
                        IF (RMSB(INDO).GT.0.0) THEN
                           IF (FPARM(10).GT.0.0) VIS(3,INDI) = 1.0
                           VIS(3,INDI) = VIS(3,INDI) / (RMSB(INDO) ** 2)
                           NDONE = NDONE + 1.0D0
                        ELSE
                           NZERO = NZERO + 1.0D0
                           END IF
                        END IF
 220                 CONTINUE
 230              CONTINUE
 240           CONTINUE
            RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
            RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
            RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
            XCOUNT = XCOUNT + 1
            CALL RCOPY (NRPARM, RPARM, BUFF2(IPTRO))
            CALL RCOPY (NCOPY, VIS, BUFF2(IPTRO+NRPARM))
            NIOUT = NIOUT + 1
C                                       update NX table
            CALL RNXUPD (BUFF2(IPTRO), RNXRET)
            IPTRO = IPTRO + NCOPY + NRPARM
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNOU, FINDOU, BUFF2, NIOLIM,
     *            KBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT DATA SET'
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
            GO TO 200
            END IF
C                                       close uv data set
         CALL UVGET ('CLOS', RPARM, VIS, IRET)
         IF (NZERO.GT.0.0D0) THEN
            WRITE (MSGTXT,1900) NZERO
            CALL MSGWRT (3)
            END IF
         IF (NDONE.GT.0.0D0) THEN
            WRITE (MSGTXT,1901) NDONE
            CALL MSGWRT (3)
            END IF
 900     CONTINUE
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNOU, FINDOU, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINISH WRITING THE OUTPUT DATA SET'
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKOU, NEWCNO, LUNOU, CATBLK, IRET)
      CALL ZCLOSE (LUNOU, FINDOU, IRET)
      CALL UVPGET (IRET)
      IRET = 0
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPWANX: ERROR',I3,' ON ',A)
 1015 FORMAT (A22,' scan',I5)
 1160 FORMAT ('Reached index limit',I10)
 1900 FORMAT (F12.0,' visibility weights unchanged')
 1901 FORMAT (F12.0,' visibility weights changed')
      END
      SUBROUTINE BPWASU (NC, NT, NA, NB, NI, B, NWD, INDEX, IRET)
C-----------------------------------------------------------------------
C   Source by source, BPWASU accumulates a buffer of NT times for all
C   baselines, etc.  It then finds the rms over time for each channel,
C   IF, stokes and writes this to a table.  It then sorts, smooths, and
C   resorts the table.  Then it rereads the input file for that source
C   applying normalized, channel-dependent weights to the output data.
C   Inputs:
C      NC      I      Number spectral channels
C      NT      I      Number times to accumulate
C      NA      I      Number antennas
C      NB      I      Max baseline number
C      NI      I      Max IF in data
C      NWD     I      Number words in INDEX
C   Output:
C      B       R(*)   Big buffer to accumulate the vis
C      INDEX   I(*)   Equivalenced buffer to B (NB+2,*)
C      IRET    I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NC, NT, NA, NB, NI, NWD, INDEX(NB+2,*), IRET
      REAL      B(3,NC,NT,NB,NI,*)
C
      INTEGER   NTMAX
      PARAMETER (NTMAX = 99)
      INCLUDE 'BPWAY.INC'
      INCLUDE 'WTABLE.INC'
      INTEGER   NUMVIS, CATMP(256), CHFLGS(MAXCIF), NW, NVM, NTIMES, I,
     *   TNUM(NTMAX), VISN(2,NTMAX), LSOU, ISOU, NCT, CT, J1, J2, JT,
     *   NRBL(MAXANT,MAXANT), VER, NUMSU, ISU, ISONE(MXBASE*NTMAX),
     *   LUNTMP, KOLSSU(MAXSUC), NUMVSU(MAXSUC), RNOSU, BUFFSU(512),
     *   LUSU, SUSOUR, SUSUBA, SUFQID, JB, KEY(2,2), KEYSUB(2,2), NTS,
     *   RNXRET, LUNOU, FINDOU, VO, ILENBU, BO, KBIND, IPTRO, NIOUT,
     *   NIOLIM, JA1, JA2, JBL, NCOPY, JI, JP, INDI, INDO, JF, NWORDS,
     *   MAXC, QUAL, CTLIM, SUNIF, XCOUNT
      LOGICAL   END, FIRST, GOTONE, GETNEW, LDUM, MULTI, ORDER
      LONGINT   PSMTH
      REAL      VIS(3,UVBFSS/3), RPARM(20), TB, TE,  TINT, TIME, TEPS,
     *   TIMES(2,NTMAX), TLIMIT, RMSRMS(3,MAXCIF), PTIME, FKEY(2,2),
     *   RMSB(3*MAXCIF), BASEN, SMTH(2), FLUX(4,MAXIF)
      DOUBLE PRECISION NZERO, NDONE, FREQO(MAXIF), BANDW, RAEPO, DECEPO,
     *   EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL(MAXIF),
     *   LRESTF(MAXIF), PMRA, PMDEC
      CHARACTER PHNAME*48, STEP*22, VELTYP*8, VELDEF*8, SOUNAM*16,
     *   CALCOD*4
      HOLLERITH CATH(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (VIS, BUFF1), (RMSRMS, RMSB), (CATH, CATBLK)
      DATA KEY    /5, 0, 1, 0/
      DATA FKEY   /1.0, 0.0, 1.0, 0.0/
      DATA KEYSUB /4*1/
      DATA VO, BO /0, 1/
C-----------------------------------------------------------------------
      TEPS = 0.1 / (24.0 * 3600.0)
      CALL MULSDB (CATOLD, MULTI)
      CTLIM = NWD / (NB+2)
      ORDER = .TRUE.
      PTIME = -1000.
      XCOUNT = 0
C                                       parameters
      NVM = NC * NT
      IF (FPARM(2).LE.0.0) FPARM(2) = 10.
      TLIMIT = 2.01 * FPARM(1) * FPARM(2)
      TLIMIT = TLIMIT / (24. * 3600.)
      TINT = 1.001 * FPARM(2) / (24. * 3600.)
      I = NC * NI * 3
      CALL RFILL (I, 0.0, RMSRMS)
C                                       counters, mask
      NCHAN = CATBLK(KINAX+JLOCF)
      CALL CHWANT (NCHAN, LIF, CHNSEL(1,1,LBIF), CHFLGS)
      NTIMES = 0
      I = MAXANT * MAXANT
      CALL FILL (I, 0, NRBL)
      I = MXBASE * NTMAX
      CALL FILL (I, 0, ISONE)
C                                       SU table
      IF (MULTI) THEN
         LUSU = LUNTMP (1)
         VER = 1
         CALL SOUINI ('READ', BUFFSU, DISKIN, OLDCNO, 1, CATOLD, LUSU,
     *      SUNIF, VELTYP, VELDEF, SUFQID, RNOSU, KOLSSU, NUMVSU, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING THE SOURCE TABLE'
            GO TO 990
            END IF
         NUMSU = BUFFSU(5)
         DO 5 I = 1,30
            SOURCS(I) = ' '
 5          CONTINUE
      ELSE
         NUMSU = 1
         END IF
C                                       init SU table on output
      CALL RNXGET (DISKIN, OLDCNO, CATUV)
      CALL RNXINI (DISKOU, NEWCNO, CATBLK, RNXRET)
C                                       output file
      CALL ZPHFIL ('UV', DISKOU, NEWCNO, 1, PHNAME, IRET)
      LUNOU = LUNTMP (0)
      CALL ZOPEN (LUNOU, FINDOU, DISKOU, PHNAME, .TRUE., .FALSE.,
     *   .TRUE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT UV DATA SET'
         GO TO 990
         END IF
C                                       init I/O
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNOU, FINDOU, NVIS, VO, LREC, ILENBU,
     *   JBUFSZ, BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT I/O TO OUTPUT DATA SET'
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       WT table
      DO 900 ISU = 1,NUMSU
         IF (MULTI) THEN
            CALL TABSOU ('READ', BUFFSU, RNOSU, KOLSSU, NUMVSU, SUSOUR,
     *         SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING THE SOURCE TABLE'
               GO TO 990
               END IF
C                                       do we want this scan?
            IF (MYNSOU.GT.0) THEN
               DO 10 I = 1,MYNSOU
                  IF (SUSOUR.EQ.MYSOUW(I)) THEN
                     IF (.NOT.MYDOSW) GO TO 900
                     GO TO 15
                     END IF
 10               CONTINUE
               IF (MYDOSW) GO TO 900
               END IF
 15         FRQSEL = SUFQID
            SOURCS(1) = SOUNAM
C                                       single source
         ELSE
            CALL H2CHR (8, 1, CATH(KHOBJ), SOUNAM)
            FRQSEL = MYFQID
            END IF
         SUBARR = MYSUBA
         CALL RFILL (8, 0.0, TIMRNG)
         TIMRNG(1) = MYTIME(1)
         TIMRNG(5) = MYTIME(2)
         INITVS = 1
         TB = -10000
         TE = TB
         NW = 3 * NC * NT * NB * NI * NSTOK
         CALL RFILL (NW, 0.0, B)
         LSOU = -1
         CT = 0
         NCT = 0
         FIRST = .TRUE.
         GOTONE = .FALSE.
         STEP = 'Starting'
         WRITE (MSGTXT,1015) STEP, ISU, SOUNAM
         CALL MSGWRT (2)
         NZERO = 0.0D0
         NDONE = 0.0D0
C                                       WT work table
         WTLUN = LUNTMP (1)
         VER = 1
         CALL WTINI ('WRIT', WTBUFF, DISKIN, OLDCNO, VER, CATOLD, WTLUN,
     *      WTRNO, WTKOLS, WTNUMV, NC, NI, NSTOK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING THE WT (WORK) TABLE'
            GO TO 990
            END IF
C                                       reset WT table
         WTRNO = 1
         WTBUFF(5) = 0
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
C                                       Loop
C                                       Read vis. record.
 100     CALL UVGET ('READ', RPARM, VIS, IRET)
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
               NUMVIS = NUMVIS + 1
               TIME = RPARM(1+ILOCT)
               END IF
            ISOU = 1
            IF (ILOCSU.GE.0) ISOU = RPARM(1+ILOCSU) + 0.01
            IF (END) ISOU = -1
            IF ((ABS(TB-TIME).LT.TINT) .AND. (ISOU.EQ.LSOU)) THEN
               CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B, ISONE,
     *            GETNEW)
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
C                                       just advance time counter
               IF ((NCT.LT.NT) .AND. (ISOU.EQ.LSOU) .AND.
     *            (TIME-TB.LT.TLIMIT)) THEN
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
     *               LDUM)
                  GOTONE = .TRUE.
                  TB = TIME
C                                       still going, clear 1 or more
               ELSE IF ((ISOU.EQ.LSOU) .AND. (TIME-TB.LT.TLIMIT)) THEN
                  J1 = (NT+1)/2
                  J2 = J1
                  IF (FIRST) J1 = 1
                  FIRST = .FALSE.
                  CALL RWRMSH (NC, NT, NA, NB, NI, NCT, J1, J2, TNUM,
     *               CHFLGS, B, RMSRMS, TIMES, VISN, IRET)
                  IF (IRET.NE.0) GO TO 999
C                                       next
                  CT = TNUM(1)
                  DO 150 JT = 1,NT-1
                     TNUM(JT) = TNUM(JT+1)
 150                 CONTINUE
                  JT = NB * (CT -1) + 1
                  CALL FILL (NB, 0, ISONE(JT))
                  TNUM(NT) = CT
                  TIMES(1,CT) = TIME
                  TIMES(2,CT) = TIME
                  VISN(1,CT) = NUMVIS
                  VISN(2,CT) = NUMVIS
                  CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B, ISONE,
     *               LDUM)
                  GOTONE = .TRUE.
                  TB = TIME
C                                       done with this scan
               ELSE
                  IF (GOTONE) THEN
                     J1 = (NT+1)/2
                     J2 = NCT
                     IF (FIRST) J1 = 1
                     CALL RWRMSH (NC, NT, NA, NB, NI, NCT, J1, J2, TNUM,
     *                  CHFLGS, B, RMSRMS, TIMES, VISN, IRET)
                     IF (IRET.NE.0) GO TO 999
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
                     CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B,
     *                  ISONE, LDUM)
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
C                                       close WT file and sort
         CALL TABWT ('CLOS', WTBUFF, WTRNO, WTKOLS, WTNUMV, TIMES, VISN,
     *      JB, RMSRMS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSING WT TABLE BEFORE SMOOTH'
            GO TO 990
            END IF
         STEP = 'Sorting weights for'
         WRITE (MSGTXT,1015) STEP, ISU, SOUNAM
         CALL MSGWRT (2)
         KEY(1,1) = 3
         KEY(1,2) = 1
         CALL TABSRT (DISKIN, OLDCNO, 'WT', VER, VER+1, KEY, KEYSUB,
     *      FKEY, BUFF1, CATOLD, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'SORTING TO BASELINE-TIME'
            GO TO 990
            END IF
C                                       count, average polarizations
         CALL BPWCNT (VER+1, NC, NI, RMSRMS, MAXC, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'COUNTING WT TABLE'
            GO TO 990
            END IF
C                                       smooth
         STEP = 'Smoothing weights for'
         WRITE (MSGTXT,1015) STEP, ISU, SOUNAM
         CALL MSGWRT (2)
         J1 = NC * NI * NSTOK
         NWORDS = ((MAXC + 2) * J1 - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', 'BPWASU', NWORDS, SMTH, PSMTH, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY FOR SMOOTHING'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         NTS = (NWORDS * 1024) / J1
         CALL BPSMOO (VER+1, VER+2, J1, NTS, SMTH(1+PSMTH), RMSRMS,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'SMOOTHING WT TABLE'
            GO TO 990
            END IF
         CALL ZMEMRY ('FREE', 'BPWASU', NWORDS, SMTH, PSMTH, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'UNABLE TO FREE DYNAMIC MEMORY FOR SMOOTHING'
            CALL MSGWRT (8)
            END IF
C                                       sort back
         STEP = 'Re-sorting weights for'
         WRITE (MSGTXT,1015) STEP, ISU, SOUNAM
         CALL MSGWRT (2)
         KEY(1,1) = 2
         KEY(1,2) = 3
         CALL TABSRT (DISKIN, OLDCNO, 'WT', VER+2, VER, KEY, KEYSUB,
     *      FKEY, BUFF1, CATOLD, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'SORTING TO TIME-BASELINE'
            GO TO 990
            END IF
         CALL RMEXT (DISKIN, OLDCNO, 'WT', VER+2, CATOLD, SCRBUF, IRET)
         CALL RMEXT (DISKIN, OLDCNO, 'WT', VER+1, CATOLD, SCRBUF, IRET)
C                                       set UVGET parms for re-read
         FRQSEL = SUFQID
         SUBARR = SUSUBA
         CALL RFILL (8, 0.0, TIMRNG)
         TIMRNG(1) = TSTART
         TIMRNG(5) = TEND
         INITVS = 1
         STEP = 'Applying weights for'
         WRITE (MSGTXT,1015) STEP, ISU, SOUNAM
         CALL MSGWRT (2)
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
C                                       WT work table
         WTLUN = LUNTMP (1)
         VER = 1
         CALL WTINI ('READ', WTBUFF, DISKIN, OLDCNO, VER, CATOLD, WTLUN,
     *      WTRNO, WTKOLS, WTNUMV, NC, NI, NSTOK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING THE WT (WORK) TABLE'
            GO TO 990
            END IF
C                                       make index of WT file
         CT = 0
         NTS = WTBUFF(5)
         DO 160 I = 1,NTS
            CALL TABWT ('READ', WTBUFF, WTRNO, WTKOLS, WTNUMV, TIMES,
     *         VISN, JB, RMSRMS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING WT FILE TO INDEX'
               GO TO 990
               END IF
            IF ((CT.EQ.0) .OR. ((VISN(1,1).NE.INDEX(1,CT)) .AND.
     *         (VISN(2,1).NE.INDEX(2,CT)))) THEN
               IF (CT.LT.CTLIM) THEN
                  CT = CT + 1
                  CALL FILL (NB+2, 0, INDEX(1,CT))
                  INDEX(1,CT) = VISN(1,1)
                  INDEX(2,CT) = VISN(2,1)
               ELSE
                  WRITE (MSGTXT,1160) CTLIM
                  IRET = 99
                  GO TO 990
                  END IF
               END IF
            INDEX(JB+2,CT) = I
 160        CONTINUE
         CT = 1
C                                       Loop
C                                       Read vis. record.
 200     CALL UVGET ('READ', RPARM, VIS, IRET)
         END = IRET.LT.0
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING DATA FIRST PASS'
            GO TO 990
C                                       got good data
         ELSE IF (IRET.EQ.0) THEN
            NUMVIS = NUMVIS + 1
            IF (ILOCB.GE.0) THEN
               BASEN = RPARM(1+ILOCB)
               JA1 = BASEN / 256. + 0.1
               JA2 = BASEN - JA1*256. + 0.1
            ELSE
               JA1 = RPARM(1+ILOCA1) + 0.1
               JA2 = RPARM(1+ILOCA2) + 0.1
               END IF
            JBL = NANT * (JA1-1) - ((JA1*(JA1-1))/2) + JA2
 205        IF (NUMVIS.GT.INDEX(2,CT)) THEN
               CT = CT + 1
               GO TO 205
               END IF
            WTRNO = INDEX(JBL+2,CT)
            CALL TABWT ('READ', WTBUFF, WTRNO, WTKOLS, WTNUMV, TIMES,
     *         VISN, JB, RMSB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING WT FILE TO INDEX'
               GO TO 990
               END IF
            NCOPY = 3 * NI * NSTOK * NC
            DO 240 JI = 1,NI
               DO 230 JP = 1,NSTOK
                  INDI = (JI-1) * INCIFI + (JP-1) * INCSI + 1 - INCFI
                  INDO = (JI-1) * NC + (JP - 1) * NC * NI
                  DO 220 JF = 1,NC
                     INDI = INDI + INCFI
                     INDO = INDO + 1
                     IF (VIS(3,INDI).GT.0.0) THEN
                        IF (RMSB(INDO).GT.0.0) THEN
                           IF (FPARM(10).GT.0.0) VIS(3,INDI) = 1.0
                           VIS(3,INDI) = VIS(3,INDI) / (RMSB(INDO) ** 2)
                           NDONE = NDONE + 1.0D0
                        ELSE
                           NZERO = NZERO + 1.0D0
                           END IF
                        END IF
 220                 CONTINUE
 230              CONTINUE
 240           CONTINUE
            RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
            RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
            RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
            IF (RPARM(1+ILOCT).GE.PTIME) THEN
               PTIME = RPARM(1+ILOCT)
            ELSE
               ORDER = .FALSE.
               END IF
            XCOUNT = XCOUNT + 1
            CALL RCOPY (NRPARM, RPARM, BUFF2(IPTRO))
            CALL RCOPY (NCOPY, VIS, BUFF2(IPTRO+NRPARM))
            NIOUT = NIOUT + 1
C                                       update NX table
            CALL RNXUPD (BUFF2(IPTRO), RNXRET)
            IPTRO = IPTRO + NCOPY + NRPARM
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNOU, FINDOU, BUFF2, NIOLIM,
     *            KBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT DATA SET'
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
            GO TO 200
            END IF
C                                       close uv data set
         CALL UVGET ('CLOS', RPARM, VIS, IRET)
         IF (NZERO.GT.0.0D0) THEN
            WRITE (MSGTXT,1900) NZERO
            CALL MSGWRT (3)
            END IF
         IF (NDONE.GT.0.0D0) THEN
            WRITE (MSGTXT,1901) NDONE
            CALL MSGWRT (3)
            END IF
 900     CONTINUE
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNOU, FINDOU, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINISH WRITING THE OUTPUT DATA SET'
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKOU, NEWCNO, LUNOU, CATBLK, IRET)
      CALL ZCLOSE (LUNOU, FINDOU, IRET)
      CALL UVPGET (IRET)
      IRET = 0
C                                       NX close, order?
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         CALL MSGWRT (8)
         END IF
      IF (.NOT.ORDER) THEN
         MSGTXT = 'WARNING: OUTPUT DATA SET NOT IN TIME ORDER'
         CALL MSGWRT (8)
         CALL CHR2H (2, '  ', 1, CATBLK(KITYP))
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPWASU: ERROR',I3,' ON ',A)
 1015 FORMAT (A22,' source',I4,2X,A)
 1160 FORMAT ('Reached index limit',I10)
 1900 FORMAT (F12.0,' visibility weights unchanged')
 1901 FORMAT (F12.0,' visibility weights changed')
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
      INCLUDE 'BPWAY.INC'
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
      GETNEW = ISONE(JBL,CT).GT.0
      IF (.NOT.GETNEW) THEN
         ISONE(JBL,CT) = 1
         DO 40 JI = 1,NI
            DO 30 JP = 1,NSTOK
               INDI = (JI-1) * INCIFI + (JP-1) * INCSI + 1 - INCFI
               DO 20 JF = 1,NCHAN
                  INDI = INDI + INCFI
                  B(1,JF,CT,JBL,JI,JP) = VIS(1,INDI)
                  B(2,JF,CT,JBL,JI,JP) = VIS(2,INDI)
                  WT = VIS(3,INDI)
                  IF ((FPARM(10).GT.1.0) .AND. (WT.GT.0.0)) WT = 1.0
                  B(3,JF,CT,JBL,JI,JP) = WT
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE RWRMSH (NC, NT, NA, NB, NI, NCT, J1, J2, TNUM, CHFLGS,
     *   B, RMSRMS, TIMES, VISN, IRET)
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
C      B        R(*)   Data buffer (3,NC,NT,NB,NI,NP)
C   In/out:
C      RMSRMS   D(*)   summing up time RMSes
C      DEVRMS   D(*)   summing up spectral deviations
C-----------------------------------------------------------------------
      INTEGER   NC, NT, NA, NB, NI, NCT, J1, J2, TNUM(*), CHFLGS(NC,NI),
     *   VISN(2,*), IRET
      REAL      B(3,NC,NT,NB,NI,*), RMSRMS(NC,NI,*), TIMES(2,*)
C
      INCLUDE 'BPWAY.INC'
      INCLUDE 'WTABLE.INC'
      INTEGER   JF, JI, JP, JA1, JA2, JT, JB, LR, JX, JLIM, NR, J
      REAL      RMSR, RMSI, V1, V2
      DOUBLE PRECISION WT, SR, SSR, SI, SSI, WR, AVR, AVI, V
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       regular operation
      DO 400 JA1 = 1,NA
         DO 390 JA2 = JA1,NA
            JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
            JI = NI * NC * NSTOK
            CALL RFILL (JI, 0.0, RMSRMS)
            DO 100 JI = 1,NI
C                                       are there any data
               DO 15 JF = 1,NC
                  DO 14 JP = 1,NSTOK
                     DO 13 JT = 1,NCT
                        WT = B(3,JF,JT,JB,JI,JP)
                        IF (WT.GT.0.0) GO TO 19
 13                     CONTINUE
 14                  CONTINUE
 15               CONTINUE
               GO TO 100
C                                       pre-clip
 19            IF (FPARM(3).LE.1.E14) THEN
                  DO 30 JT = 1,NCT
                     JX = TNUM(JT)
                     DO 25 JF = 1,NC
                        DO 20 JP = 1,NSTOK
                           WT = B(3,JF,JT,JB,JI,JP)
                           IF (WT.GT.0.0) THEN
                              V1 = B(1,JF,JT,JB,JI,JP)
                              V2 = B(2,JF,JT,JB,JI,JP)
                              SR = V1*V1 + V2*V2
                              IF (SR.GT.FPARM(3)) THEN
                                 B(3,JF,JT,JB,JI,JP) =
     *                              -ABS (B(3,JF,JT,JB,JI,JP))
                                 COUNT = COUNT + 1
                                 END IF
                              END IF
 20                        CONTINUE
 25                     CONTINUE
 30                  CONTINUE
                  END IF
C                                       time rms
               JLIM = NC
               JLIM = 0
               DO 80 JF = 1,NC
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
 60                     CONTINUE
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
                        RMSRMS(JF,JI,JP) = RMSR
                        END IF
 70                  CONTINUE
 80               CONTINUE
C                                       Normalize
               DO 95 JP = 1,NSTOK
                  SR = 0.0D0
                  WR = 0.0D0
                  DO 85 JF = 1,NC
                     IF ((CHFLGS(JF,JI).GT.0) .AND.
     *                  (RMSRMS(JF,JI,JP).GT.0.0D0)) THEN
                        WR = WR + 1.0D0
                        SR = SR + RMSRMS(JF,JI,JP)
                        END IF
 85                  CONTINUE
                  IF (WR.GT.0.0D0) THEN
                     SR = SR / WR
                     DO 90 JF = 1,NC
                        V1 = RMSRMS(JF,JI,JP) / SR
                        IF (V1.GT.0.0) RMSRMS(JF,JI,JP) = MIN (FPARM(7),
     *                     MAX (FPARM(8), V1))
 90                     CONTINUE
                     END IF
 95               CONTINUE
 100           CONTINUE
C                                       write to table
            DO 120 J = J1,J2
               JX = TNUM(J)
               CALL TABWT ('WRIT', WTBUFF, WTRNO, WTKOLS, WTNUMV,
     *            TIMES(1,JX), VISN(1,JX), JB, RMSRMS, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITING WT TABLE'
                  CALL MSGWRT (8)
                  GO TO 999
                  END IF
 120           CONTINUE
 390        CONTINUE
 400     CONTINUE
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RWRMSH: ERROR',I5,' ON ',A)
      END
      SUBROUTINE BPWCNT (VER, KCH, KIF, RMSRMS, MAXC, IRET)
C-----------------------------------------------------------------------
C   BPWCNT counts the maximum number of times for any baseline
C   It also averages polarizations as instructed.
C   Inputs:
C      VER      I      WT version
C      KCH      I      Number channels
C      KIF      I      Number IFs
C   Outputs:
C      RMSRMS   R(*)   work buffer
C      MAXC     I      Maximum record count in a baseline
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   VER, KCH, KIF, MAXC, IRET
      REAL      RMSRMS(KCH,KIF,*)
C
      INCLUDE 'BPWAY.INC'
      INTEGER   INBUF(512), OUTBUF(512), LUNI, LUNO, LUNTMP, IWTRNO,
     *   WTKOLS(4), WTNUMV(4), VISN(2), JBL, CBL, CCNT, NC, NI, NS,
     *   NREC, IREC, IC, II, IS, NSUM
      REAL      TIMER(2), SUM
      LOGICAL   DOSTOK
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       open read
      LUNI = LUNTMP (1)
      CALL WTINI ('READ', INBUF, DISKIN, OLDCNO, VER, CATOLD, LUNI,
     *   IWTRNO, WTKOLS, WTNUMV, NC, NI, NS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING THE INPUT WT TABLE'
         GO TO 990
         END IF
      NREC = INBUF(5)
      CBL = 0
      CCNT = 0
      MAXC = 0
      DOSTOK = (FPARM(9).GT.0.0) .AND. (NS.GT.1)
C                                       output if do Stokes
      IF (DOSTOK) THEN
         LUNO = LUNTMP (1)
         CALL WTINI ('WRIT', OUTBUF, DISKIN, OLDCNO, VER, CATOLD, LUNO,
     *      IWTRNO, WTKOLS, WTNUMV, NC, NI, NS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING THE OUTPUT WT TABLE'
            GO TO 990
            END IF
         END IF
C                                       loop
      DO 100 IREC = 1,NREC
         IWTRNO = IREC
         CALL TABWT ('READ', INBUF, IWTRNO, WTKOLS, WTNUMV, TIMER, VISN,
     *      JBL, RMSRMS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING INPUT WT TABLE'
            GO TO 990
            END IF
C                                       count
         IF (JBL.EQ.CBL) THEN
            CCNT = CCNT + 1
         ELSE
            MAXC = MAX (MAXC, CCNT)
            CCNT = 1
            CBL = JBL
            END IF
C                                       average Stokes?
         IF (DOSTOK) THEN
            DO 50 IC = 1,NC
               DO 40 II = 1,NI
                  SUM = 0.0
                  NSUM = 0
                  DO 20 IS = 1,NS
                     IF (RMSRMS(IC,II,IS).GT.0.0) THEN
                        NSUM = NSUM + 1
                        SUM = SUM + RMSRMS(IC,II,IS)
                        END IF
 20                  CONTINUE
                  IF (NSUM.GT.0) THEN
                     SUM = SUM / NSUM
                     DO 30 IS = 1,NS
                        IF (RMSRMS(IC,II,IS).GT.0.0) RMSRMS(IC,II,IS) =
     *                     SUM
 30                     CONTINUE
                     END IF
 40               CONTINUE
 50            CONTINUE
            IWTRNO = IREC
            CALL TABWT ('WRIT', OUTBUF, IWTRNO, WTKOLS, WTNUMV, TIMER,
     *         VISN, JBL, RMSRMS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RE-WRITING INPUT WT TABLE'
               GO TO 990
               END IF
            END IF
 100     CONTINUE
C                                       last count
      MAXC = MAX (MAXC, CCNT)
C                                       close files
      CALL TABWT ('CLOS', INBUF, IWTRNO, WTKOLS, WTNUMV, TIMER, VISN,
     *   JBL, RMSRMS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING INPUT WT TABLE'
         GO TO 990
         END IF
      IF (DOSTOK) THEN
         CALL TABWT ('CLOS', OUTBUF, IWTRNO, WTKOLS, WTNUMV, TIMER,
     *      VISN, JBL, RMSRMS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSING OUTPUT WT TABLE'
            GO TO 990
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPWCNT: ERROR',I5,' ON ',A)
      END
      SUBROUTINE BPSMOO (INVER, OUTVER, ND, NTS, B, RMSRMS, IRET)
C-----------------------------------------------------------------------
C   BPSMOO takes a sorted WT table and smooths it in time, replacing
C   null and good solutions with the smoothed value of the good ones.
C   Inputs:
C      INVER    I      Input table version
C      OUTVER   I      Output table version (not same)
C      ND       I      Number values in data row
C      NTS      I      Maximum number times in buffer B
C   Output:
C      B        R(*)   Work buffer
C      IRET     I      Disk error
C-----------------------------------------------------------------------
      INTEGER   INVER, OUTVER, ND, NTS, IRET
      REAL      B(ND,*), RMSRMS(*)
C
      INCLUDE 'BPWAY.INC'
      INTEGER   INBUF(512), OUTBUF(512), LUNI, LUNO, LUNTMP, IWTRNO,
     *   OWTRNO, WTKOLS(4), WTNUMV(4), NC, NI, NS, VISNUM(2,UVBFSS/2),
     *   JBL, LBL, CBL, NREC, IREC, NVAL, VISN(2), IVAL
      REAL      TIMES(2,UVBFSS/2), TIMER(2)
      LOGICAL   OVER, FULL
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (TIMES, BUFF1), (VISNUM, BUFF3)
C-----------------------------------------------------------------------
      LUNI = LUNTMP (1)
      CALL WTINI ('READ', INBUF, DISKIN, OLDCNO, INVER, CATOLD, LUNI,
     *   IWTRNO, WTKOLS, WTNUMV, NC, NI, NS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING THE INPUT WT TABLE'
         GO TO 990
         END IF
      NREC = INBUF(5)
      LUNO = LUNTMP (1)
      CALL WTINI ('WRIT', OUTBUF, DISKIN, OLDCNO, OUTVER, CATOLD, LUNO,
     *   OWTRNO, WTKOLS, WTNUMV, NC, NI, NS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING THE OUTPUT WT TABLE'
         GO TO 990
         END IF
      LBL = 0
      CBL = 0
      NVAL = 0
      DO 100 IREC = 1,NREC
 10      CALL TABWT ('READ', INBUF, IWTRNO, WTKOLS, WTNUMV, TIMER, VISN,
     *      JBL, RMSRMS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING INPUT WT TABLE'
            GO TO 990
            END IF
C                                       add to list
         OVER = .TRUE.
         IF (IREC.EQ.1) CBL = JBL
         IF ((JBL.EQ.CBL) .AND. (NVAL.LT.NTS)) THEN
            NVAL = NVAL + 1
            VISNUM(1,NVAL) = VISN(1)
            VISNUM(2,NVAL) = VISN(2)
            TIMES(1,NVAL) = TIMER(1)
            TIMES(2,NVAL) = TIMER(2)
            CALL RCOPY (ND, RMSRMS, B(1,NVAL))
            OVER = .FALSE.
            END IF
C                                       dump the list
         IF ((OVER) .OR. (IREC.GE.NREC)) THEN
            IVAL = FPARM(4) + 0.1
            FULL = .FALSE.
            IF ((IVAL.EQ.1) .OR. (IVAL.EQ.5)) THEN
               FULL = 2.*(TIMES(NVAL,2)-TIMES(1,1)).LT.FPARM(6)
               END IF
C                                       same answer full scan
            IF (FULL) THEN
               IVAL = 1
               CALL SMOTHR (FPARM(4), ND, IVAL, NVAL, TIMES, B, RMSRMS)
               DO 40 IVAL = 1,NVAL
                  CALL TABWT ('WRIT', OUTBUF, OWTRNO, WTKOLS, WTNUMV,
     *               TIMES(1,IVAL), VISNUM(1,IVAL), CBL, RMSRMS, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT WT TABLE'
                     GO TO 990
                     END IF
 40               CONTINUE
C                                       time variable answer
            ELSE
               DO 50 IVAL = 1,NVAL
                  CALL SMOTHR (FPARM(4), ND, IVAL, NVAL, TIMES, B,
     *               RMSRMS)
                  CALL TABWT ('WRIT', OUTBUF, OWTRNO, WTKOLS, WTNUMV,
     *               TIMES(1,IVAL), VISNUM(1,IVAL), CBL, RMSRMS, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT WT TABLE'
                     GO TO 990
                     END IF
 50               CONTINUE
               END IF
            IF ((JBL.EQ.CBL) .AND. (NVAL.GE.NTS)) THEN
               MSGTXT = 'WARNING: SMOOTHING NOT RIGHT DUE TO MEMORY'
               CALL MSGWRT (7)
               END IF
C                                       re-read last record
C                                       for new baseline
            IF (IREC.LT.NREC) THEN
               CBL = JBL
               NVAL = 0
               IWTRNO = IWTRNO - 1
               GO TO 10
               END IF
            END IF
 100     CONTINUE
C                                       close files
      CALL TABWT ('CLOS', INBUF, IWTRNO, WTKOLS, WTNUMV, TIMER, VISN,
     *   JBL, RMSRMS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING INPUT WT TABLE'
         GO TO 990
         END IF
      CALL TABWT ('CLOS', OUTBUF, OWTRNO, WTKOLS, WTNUMV, TIMES,
     *   VISNUM, CBL, RMSRMS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING OUTPUT WT TABLE'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPSMOO: ERROR',I5,' ON ',A)
      END
      SUBROUTINE SMOTHR (FUNC, ND, EV, NV, TS, B, ANS)
C-----------------------------------------------------------------------
C   Applies smoothing
C   Inputs:
C      FUNC   R(3)   Function: type, FWHM, support
C      ND     I      Number channels in data
C      EV     I      The time to evaluate
C      NV     I      The total number of times
C      TS     R(*)   Array of time ranges (2,NV)
C      B      R(*)   The data before smoothing (ND,NV)
C   Outputs:
C      ANS    R(*)   Smoothed result for time EV (ND,2)
C-----------------------------------------------------------------------
      INTEGER   ND, EV, NV
      REAL      FUNC(3), TS(2,*), B(ND,*), ANS(ND,*)
C
      INTEGER   ITYPE, I, J, K, MWF
      REAL      T0, T, S, F, P, MWFBUF(15000), MEDIAN
C-----------------------------------------------------------------------
      ITYPE = FUNC(1) + 0.01
      S = FUNC(3) / 2.0
      I = 2 * ND
      CALL RFILL (I, 0.0, ANS)
      T0 = (TS(1,EV) + TS(2,EV)) / 2.0
C                                       boxcar
      IF (ITYPE.EQ.1) THEN
         DO 120 I = EV,NV
            T = (TS(1,I) + TS(2,I)) / 2.0
            IF (T-T0.LE.S) THEN
               DO 110 J = 1,ND
                  IF (B(J,I).GT.0.0) THEN
                     ANS(J,1) = ANS(J,1) + B(J,I)
                     ANS(J,2) = ANS(J,2) + 1.0
                     END IF
 110              CONTINUE
            ELSE
               GO TO 125
               END IF
 120        CONTINUE
 125     DO 140 K = 1,EV-1
            I = EV - K
            T = (TS(1,I) + TS(2,I)) / 2.0
            IF (T0-T.LE.S) THEN
               DO 130 J = 1,ND
                  IF (B(J,I).GT.0.0) THEN
                     ANS(J,1) = ANS(J,1) + B(J,I)
                     ANS(J,2) = ANS(J,2) + 1.0
                     END IF
 130              CONTINUE
            ELSE
               GO TO 600
               END IF
 140        CONTINUE
C                                       Gaussian
      ELSE IF (ITYPE.EQ.2) THEN
         P =(2.0 * SQRT (LOG(2.0))) /  FUNC(2)
         P = P * P
         DO 220 I = EV,NV
            T = (TS(1,I) + TS(2,I)) / 2.0 - T0
            IF (T.LE.S) THEN
               F = EXP (-T * T * P)
               DO 210 J = 1,ND
                  IF (B(J,I).GT.0.0) THEN
                     ANS(J,1) = ANS(J,1) + F * B(J,I)
                     ANS(J,2) = ANS(J,2) + F
                     END IF
 210              CONTINUE
            ELSE
               GO TO 225
               END IF
 220        CONTINUE
 225     DO 240 K = 1,EV-1
            I = EV - K
            T = T0 - ((TS(1,I) + TS(2,I)) / 2.0)
            IF (T.LE.S) THEN
               F = EXP (-T * T * P)
               DO 230 J = 1,ND
                  IF (B(J,I).GT.0.0) THEN
                     ANS(J,1) = ANS(J,1) + F * B(J,I)
                     ANS(J,2) = ANS(J,2) + F
                     END IF
 230              CONTINUE
            ELSE
               GO TO 600
               END IF
 240        CONTINUE
C                                       Exponential
      ELSE IF (ITYPE.EQ.3) THEN
         P =(2.0 * SQRT (LOG(2.0))) /  FUNC(2)
         DO 320 I = EV,NV
            T = (TS(1,I) + TS(2,I)) / 2.0 - T0
            IF (T.LE.S) THEN
               F = EXP (-T * P)
               DO 310 J = 1,ND
                  IF (B(J,I).GT.0.0) THEN
                     ANS(J,1) = ANS(J,1) + F * B(J,I)
                     ANS(J,2) = ANS(J,2) + F
                     END IF
 310              CONTINUE
            ELSE
               GO TO 325
               END IF
 320        CONTINUE
 325     DO 340 K = 1,EV-1
            I = EV - K
            T = T0 - ((TS(1,I) + TS(2,I)) / 2.0)
            IF (T.LE.S) THEN
               F = EXP (-T * P)
               DO 330 J = 1,ND
                  IF (B(J,I).GT.0.0) THEN
                     ANS(J,1) = ANS(J,1) + F * B(J,I)
                     ANS(J,2) = ANS(J,2) + F
                     END IF
 330              CONTINUE
            ELSE
               GO TO 600
               END IF
 340        CONTINUE
C                                       Linear
      ELSE IF (ITYPE.EQ.4) THEN
         P = 1.0 /  FUNC(2)
         DO 420 I = EV,NV
            T = (TS(1,I) + TS(2,I)) / 2.0 - T0
            IF (T.LE.S) THEN
               F = 1.0 - T * P
               F = MAX (0.0, F)
               DO 410 J = 1,ND
                  IF (B(J,I).GT.0.0) THEN
                     ANS(J,1) = ANS(J,1) + F * B(J,I)
                     ANS(J,2) = ANS(J,2) + F
                     END IF
 410              CONTINUE
            ELSE
               GO TO 425
               END IF
 420        CONTINUE
 425     DO 440 K = 1,EV-1
            I = EV - K
            T = T0 - ((TS(1,I) + TS(2,I)) / 2.0)
            IF (T.LE.S) THEN
               F = 1.0 - T * P
               F = MAX (0.0, F)
               DO 430 J = 1,ND
                  IF (B(J,I).GT.0.0) THEN
                     ANS(J,1) = ANS(J,1) + F * B(J,I)
                     ANS(J,2) = ANS(J,2) + F
                     END IF
 430              CONTINUE
            ELSE
               GO TO 600
               END IF
 440        CONTINUE
C                                       median
      ELSE IF (ITYPE.EQ.5) THEN
         DO 530 J = 1,ND
            MWF = 0
            DO 510 I = EV,NV
               T = (TS(1,I) + TS(2,I)) / 2.0
               IF (T-T0.LE.S) THEN
                  IF (B(J,I).GT.0.0) THEN
                     MWF = MWF + 1
                     MWFBUF(MWF) = B(J,I)
                     END IF
               ELSE
                  GO TO 515
                  END IF
 510           CONTINUE
 515        DO 520 K = 1,EV-1
               I = EV - K
               T = (TS(1,I) + TS(2,I)) / 2.0
               IF (T0-T.LE.S) THEN
                  IF (B(J,I).GT.0.0) THEN
                     MWF = MWF + 1
                     MWFBUF(MWF) = B(J,I)
                     END IF
               ELSE
                  GO TO 525
                  END IF
 520           CONTINUE
 525        IF (MWF.GT.0) THEN
               ANS(J,1) = MEDIAN (MWF, MWFBUF)
            ELSE
               ANS(J,1) = 0.0
               END IF
 530        CONTINUE
         GO TO 999
         END IF
C                                       Normalize
 600  DO 610 J = 1,ND
         IF (ANS(J,2).GT.0.0) ANS(J,1) = ANS(J,1) / ANS(J,2)
 610     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE WTINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IWTRNO, WTKOLS, WTNUMV, NCHAN, NIF, NSTOKE, IRET)
C-----------------------------------------------------------------------
C   WTINI creates and/or opens a WT temporary weight file used only by
C   BPWAY
C   Inputs:
C      OPCODE   C*4      'READ', 'WRIT'
C      BUFFER   I(512)   buffer used for control and I/O
C      DISK     I        disk of data set
C      CNO      I        catalog number
C      VER      I        version
C      CATBLK   I(256)   data set header
C      LUN      I        LUN to use
C   In/out:
C      NCHAN    I        Number spectral channels
C      NIF      I        Number spectral windows (IFs)
C      NSTOKE   I        Number Stokes parameters
C   Outputs:
C      IWTRNO   I        Record number: 1 on read, max + 1 on write
C      WTKOLS   I(4)     Column pointer array
C      WTNUMV   I(4)     Column dimensions
C      IRET     I        Error code
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, IWTRNO,
     *   WTKOLS(4), WTNUMV(4), NCHAN, NIF, NSTOKE, IRET
C
      CHARACTER TITLE(4)*24, UNITS(4)*8, TTITLE*56, KEYW(3)*8
      HOLLERITH HOLTMP(6)
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(4), NDATA, I,
     *   IPOINT, NC, ITRIM, JERR, KLOCS(3), KEYTYP(3), KEYVAL(3), MSGSAV
      LOGICAL   DOREAD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NTT /56/
      DATA TTITLE /'AIPS TEMPORARY WEIGHTS WORK TABLE '/
      DATA DTYP /22,24,14,2/
      DATA TITLE /'TIMES', 'VIS NUMS', 'BASELINE', 'RMSES'/
      DATA UNITS /'DAYS', ' ', ' ', 'JY'/
      DATA KEYW /'NO_CHAN', 'NO_IF', 'NO_POL'/
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NDATA = 4
      NREC = 2000
      NCOL = NDATA
      NKEY = 3
      CALL FILL (NDATA, 0, WTKOLS)
      CALL FILL (NDATA, 0, WTNUMV)
C                                       Fill in types, lengths
      IF (.NOT.DOREAD) THEN
         DTYP(4) = 10 * (NCHAN * NIF * NSTOKE) + 2
         DO 10 I = 1,NDATA
            DATP(I,2) = DTYP(I)
            WTNUMV(I) = DTYP(I) / 10
 10         CONTINUE
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'WT', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'WTINI', IRET)
         GO TO 990
         END IF
C                                       Get number of scans
      IWTRNO = BUFFER(5) + 1
      IF (DOREAD) IWTRNO = 1
C                                       File created, initialize
      IF (IRET.LT.0) THEN
C                                       Col. labels.
         DO 40 I = 1,NDATA
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IRET)
            IF (IRET.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'WTINI', IRET)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IRET)
            IF (IRET.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'WTINI', IRET)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
C                                       No. antennas.
         KLOCS(1) = 1
         KEYTYP(1) = 4
         KEYVAL(1) = NCHAN
C                                       No. polarizations
         KLOCS(2) = 2
         KEYTYP(2) = 4
         KEYVAL(2) = NIF
C                                       No. IFs.
         KLOCS(3) = 3
         KEYTYP(3) = 4
         KEYVAL(3) = NSTOKE
C                                       Only write if just created.
         NKEY = 3
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'WTINI', IRET)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         MSGSUP = 32000
         NKEY = 3
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IRET)
         MSGSUP = MSGSAV
         IF ((IRET.GE.1) .AND. (IRET.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'WTINI', IRET)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       No. antennas.
         IPOINT = KLOCS(1)
         IF (IPOINT.GT.0) NCHAN = KEYVAL(IPOINT)
C                                       No. IFs per pair.
         IPOINT = KLOCS(2)
         IF (IPOINT.GT.0) NIF = KEYVAL(IPOINT)
C                                       No. IF pairs.
         IPOINT = KLOCS(3)
         IF (IPOINT.GT.0) NSTOKE = KEYVAL(IPOINT)
         END IF
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, BUFFER, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'WTINI', IRET)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'WT', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'WTINI', IRET)
         GO TO 990
         END IF
      CALL FNDCOL (4, TITLE, 24, .TRUE., BUFFER, WTKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,4
         IPOINT = WTKOLS(I)
         IF (IPOINT.GT.0) THEN
            WTKOLS(I) = DATP(IPOINT,1)
            WTNUMV(I) = DATP(IPOINT,2) / 10
            IF (WTNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            WTKOLS(I) = -1
            WTNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            CALL MSGWRT (6)
            END IF
 150     CONTINUE
      IRET = 0
      GO TO 999
C                                      Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('WTINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('WTINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('WTINI: ERROR INITIALIZING WEIGHTS WORK TABLE FOR ',A4)
      END
      SUBROUTINE TABWT (OPCODE, BUFFER, IWTRNO, WTKOLS, WTNUMV, TIMES,
     *   VISN, BASNUM, RMSES, IRET)
C-----------------------------------------------------------------------
C   TABWT reads and writes WT table records
C   Inputs:
C      OPCODE   C*4      READ, WRIT, CLOS
C      WTKOLS   I(4)     Column pointers
C      WTNUMV   I(4)     Column dimensions
C   In/out:
C      BUFFER   I(512)   Table IO buffer
C      IWTRNO   I        Row to read, write, returned 1 higher
C      TIMES    R(2)     Range of times in row (days)
C      VISN     I(2)     Range of visibility numbers in row
C      BASNUM   I        Baseline number
C      RMSES    R(*)     RMSes (channel, IF, polarization)
C   Output:
C      IRET     I        Error code
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), IWTRNO, WTKOLS(*), WTNUMV(*), VISN(2),
     *   BASNUM, IRET
      REAL      TIMES(2), RMSES(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IPOINT, RECI(MAXCIF+5), NVAL
      REAL      RECORD(MAXCIF+5)
      EQUIVALENCE (RECORD, RECI)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, IWTRNO, RECORD, BUFFER, IRET)
         IF (IRET.GT.0) GO TO 990
C                                       write - fill buffer
      ELSE IF (OPCODE.EQ.'WRIT') THEN
         IPOINT = WTKOLS(1)
         RECORD(IPOINT) = TIMES(1)
         RECORD(IPOINT+1) = TIMES(2)
         IPOINT = WTKOLS(2)
         RECI(IPOINT) = VISN(1)
         RECI(IPOINT+1) = VISN(2)
         IPOINT = WTKOLS(3)
         RECI(IPOINT) = BASNUM
         IPOINT = WTKOLS(4)
         NVAL = WTNUMV(4)
         CALL RCOPY (NVAL, RMSES, RECORD(IPOINT))
C                                       and send
         CALL TABIO (OPCODE, 0, IWTRNO, RECORD, BUFFER, IRET)
         IWTRNO = IWTRNO + 1
         IF (IRET.GT.0) GO TO 990
C                                       read buffer
      ELSE IF (OPCODE.EQ.'READ') THEN
         CALL TABIO (OPCODE, 0, IWTRNO, RECORD, BUFFER, IRET)
         IWTRNO = IWTRNO + 1
         IF (IRET.GT.0) GO TO 990
C                                       and return to caller
         IPOINT = WTKOLS(1)
         TIMES(1) = RECORD(IPOINT)
         TIMES(2) = RECORD(IPOINT+1)
         IPOINT = WTKOLS(2)
         VISN(1) = RECI(IPOINT)
         VISN(2) = RECI(IPOINT+1)
         IPOINT = WTKOLS(3)
         BASNUM = RECI(IPOINT)
         IPOINT = WTKOLS(4)
         NVAL = WTNUMV(4)
         CALL RCOPY (NVAL, RECORD(IPOINT), RMSES)
C                                       bad opcode
      ELSE
         IRET = 8
         MSGTXT = 'TABWT BAD OPCODE = ''' // OPCODE // ''''
         CALL MSGWRT (8)
         END IF
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('TABWT: TABIO ERROR',I3)
      END
      SUBROUTINE BPWAHI
C-----------------------------------------------------------------------
C   BPWAHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, SMTYPE(4)*8
      INTEGER   LUNI, HLUN, IERR, I, J, IROUND
      INCLUDE 'BPWAY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA LUNI, HLUN /28,29/
      DATA SMTYPE /'Boxcar', 'Gaussian', 'Exponent', 'Linear'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUNI, HLUN, DISKIN, DISKOU, OLDCNO, NEWCNO,
     *   CATBLK, BUFF1, SCRBUF, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, HLUN, SCRBUF,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, NAMEOU, CLAOU, SEQOU, DISKOU, HLUN, SCRBUF,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
C                                       calibration history
      CALL CALHIS (HLUN, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1010) TSKNAM, FPARM(2)
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
      I = IROUND (FPARM(1))
      WRITE (HILINE,1005) TSKNAM, I
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       channel selection
      DO 40 I = BIF,EIF
         DO 30 J = 1,20
            IF (CHNSEL(1,J,I).GT.0) THEN
               WRITE (HILINE,1015) TSKNAM, J, I, CHNSEL(1,J,I),
     *            CHNSEL(2,J,I), CHNSEL(3,J,I)
               CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
               IF (IERR.NE.0) GO TO 100
               END IF
 30         CONTINUE
 40      CONTINUE
C                                       pre-clip
      IF (COUNT.GT.0) THEN
         FPARM(3) = SQRT (FPARM(3))
         WRITE (HILINE,1040) TSKNAM, FPARM(3), COUNT
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       smoothing
      FPARM(5) = FPARM(5) * (24.0 * 60.0)
      FPARM(6) = FPARM(6) * (24.0 * 60.0)
      I = FPARM(4) + 0.01
      WRITE (HILINE,1045) TSKNAM, FPARM(4), SMTYPE(I)
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
      IF ((I.GE.2) .AND. (I.LE.4)) THEN
         WRITE (HILINE,1046) TSKNAM, FPARM(5)
         CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      WRITE (HILINE,1047) TSKNAM, FPARM(6)
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       rms allowed range
      FPARM(7) = 1.0 / (FPARM(7) ** 2)
      FPARM(8) = 1.0 / (FPARM(8) ** 2)
      WRITE (HILINE,1048) TSKNAM, FPARM(7)
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1049) TSKNAM, FPARM(8)
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       OPTYPE
      WRITE (HILINE,1050) TSKNAM, OPTYPE
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       weight usage
      IF (FPARM(10).GT.1.0) THEN
         WRITE (HILINE,1051) TSKNAM, FPARM(10),
     *      'ignore all input weights'
      ELSE IF (FPARM(10).GT.0.0) THEN
         WRITE (HILINE,1051) TSKNAM, FPARM(10),
     *      'ignore input weights on output'
      ELSE
         WRITE (HILINE,1051) TSKNAM, FPARM(10),
     *      'use input weights'
         END IF
      CALL HIADD (HLUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Close HI file
 100  CALL HICLOS (HLUN, .TRUE., SCRBUF, IERR)
C                                       delete the WT table
      MSGTXT = 'Deleting temporary WT table'
      CALL MSGWRT (2)
      CALL RMEXT (DISKIN, OLDCNO, 'WT', 0, CATOLD, SCRBUF, IERR)
C                                       Copy tables
      CALL COPTAB (DISKIN, OLDCNO, DISKOU, NEWCNO, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'BPWAHI: ERROR COPYING TABLES TO OUTPUT UV'
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKOU, NEWCNO, DIFPIX, BUFF1, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'CENTHI: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Temporary FG version',I4,' moves to output version',I4)
 1005 FORMAT (A6,'FPARM(1)=',I7,' / data samples in time rms')
 1010 FORMAT (A6,'FPARM(2)=',F7.1,' / normal time interval in',
     *   ' time rms')
 1015 FORMAT (A6,'CHANSEL(,',I2,',',I2,') =',I5,',',I5,',',I2)
 1040 FORMAT (A6,'FPARM(3)=',F7.1,' / ',I12,' channels pre-clipped')
 1045 FORMAT (A6,'FPARM(4)=',F3.1,4X,' / ',A,' smoothing function')
 1046 FORMAT (A6,'FPARM(5)=',F7.2,' / smoothing function FWHM',
     *   ' in minutes')
 1047 FORMAT (A6,'FPARM(6)=',F7.2,' / smoothing function support',
     *   ' in minutes')
 1048 FORMAT (A6,'FPARM(7)=',F7.3,' / minimum normalized rms')
 1049 FORMAT (A6,'FPARM(8)=',F7.1,' / maximum normalized rms')
 1050 FORMAT (A6,'OPTYPE=''',A,'''  / data grouping type')
 1051 FORMAT (A6,'FPARM(10)=',F4.1,' / ',A)
      END
