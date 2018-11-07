LOCAL INCLUDE 'TRUEP.INC'
C                                       Local include for TRUEP
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SMOMAX
      PARAMETER (SMOMAX = 256)
C
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMI2(3), XCLAI2(2), XSOUR(4,30),
     *   XCALC
      REAL      XSIN, XDISIN, XSIN2, XDISI2, XQUAL, XTIME(8), XBAND,
     *   XFREQ, XFQID, XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3),
     *   DOSPEC, APARM(10), BADD(10),
     *   SCRBUF(512), BUFF2(UVBFSS), SFLUX(MAXIF,2), FINC(MAXIF),
     *   FZERO(2), SINDEX(2), SMTABL(SMOMAX)
      DOUBLE PRECISION FOFF(MAXIF), RFREQ, REFCHN
      INTEGER   SEQIN, SEQIN2, DISKIN, DISKI2, JBUFSZ, INCSI, INCFI,
     *   INCIFI, OLDCNO, MAGIC, SUPRAD
      CHARACTER NAMEIN*12, CLAIN*6, NAMIN2*12, CLAIN2*6
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMI2, XCLAI2,
     *   XSIN2, XDISI2, XSOUR, XQUAL, XCALC, XTIME, XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, DOSPEC, APARM,
     *   BADD
      COMMON /TRUEPP/ SEQIN, SEQIN2, DISKIN, DISKI2, INCSI, INCFI,
     *   INCIFI, OLDCNO, MAGIC, SFLUX, SMTABL, SUPRAD
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMIN2, CLAIN2
      COMMON /BUFRS/ SCRBUF, BUFF2, JBUFSZ
      COMMON /FREQS/ FOFF, RFREQ, REFCHN, FINC, FZERO, SINDEX
C                                       End local include for TRUEP
LOCAL END
      PROGRAM TRUEP
C-----------------------------------------------------------------------
C! REads 2 data sets to find true polarization: 2nd set w rotated horn
C# Utility UV UV-util VLA VLB Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2010-2013, 2015-2016, 2018
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
C   TRUEP averages the RL and LR data for all baselines to a chosen
C   antenna in the first data set.  It then does the same in a second
C   data set in which the horn of that antenna has been rotated by
C   some angle, usually 90 degrees.  This allows full determination of
C   the true D terms, rather than just the realtive ones.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      IN2NAME        NAMIN2        Name of input UV data.
C      IN2CLASS       CLAIN2        Class of input UV data.
C      IN2SEQ         SEQIN2        Seq. of input UV data.
C      IN2DISK        DISKI2        Disk number of input VU data.
C   full set of calibration adverbs
C      SPECTRAL       DOSPEC        > 0, channel dependent
C      APARM(10)      APARM         User specified array.
C-----------------------------------------------------------------------
      INCLUDE 'TRUEP.INC'
      CHARACTER PRGM*6
      INTEGER   IRET, NI, NC, NA, NW
      REAL      MATRL1(2), MATLR1(2), MATRL2(2), MATLR2(2), DR1(2),
     *   DR2(2), DL1(2), DL2(2)
      COMPLEX   CDR1(2), CDR2(2), CDL1(2), CDL2(2)
      EQUIVALENCE (DR1, CDR1), (DR2, CDR2), (DL1, CDL1), (DL2, CDL2)
      LONGINT   PRL1, PLR1, PRL2, PLR2, PDR1, PDR2, PDL1, PDL2
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'TRUEP '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL TRUEPI (PRGM, NC, NI, NA, IRET)
      IF (IRET.NE.0) GO TO 990
      NW = (3 * NC * NI * NA - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', PRGM, NW, MATRL1, PRL1, IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', PRGM, NW, MATRL2, PRL2, IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', PRGM, NW, MATLR1, PLR1, IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', PRGM, NW, MATLR2, PLR2, IRET)
      NW = (2 * NC * NI * NA - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', PRGM, NW, DR1, PDR1, IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', PRGM, NW, DR2, PDR2, IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', PRGM, NW, DL1, PDL1, IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', PRGM, NW, DL2, PDL2, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'FAILED TO GET DYNAMIC MEMORY NEEDED'
         CALL MSGWRT (8)
         GO TO 990
         END IF
      MSGTXT = 'Reading data set 2'
      CALL MSGWRT (2)
      CALL TRUEPG (2, NC, NI, NA, MATRL2(1+PRL2), MATLR2(1+PLR2), IRET)
      IF (IRET.NE.0) GO TO 980
      MSGTXT = 'Reading data set 1'
      CALL MSGWRT (2)
      CALL TRUEPG (1, NC, NI, NA, MATRL1(1+PRL1), MATLR1(1+PLR1), IRET)
      IF (IRET.NE.0) GO TO 980
C                                       now solve
      MSGTXT = 'Solving for D terms'
      CALL MSGWRT (2)
      PDR1 = PDR1 / 2
      PDR2 = PDR2 / 2
      PDL1 = PDL1 / 2
      PDL2 = PDL2 / 2
      CALL TRUEPS (NC, NI, NA, MATRL1(1+PRL1), MATLR1(1+PLR1),
     *   MATRL2(1+PRL2), MATLR2(1+PLR2), CDR1(1+PDR1), CDL1(1+PDL1),
     *   CDR2(1+PDR2), CDL2(1+PDL2), IRET)
C                                       free memory
 980  CALL ZMEMRY ('FRAL', PRGM, NW, MATRL1, PRL1, NC)
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE TRUEPI (PRGN, NC, NI, NA, IRET)
C-----------------------------------------------------------------------
C   TRUEPI gets input parameters for TRUEP
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      NC     I     Number of spectral channels to solve
C      NI     I     Number OFs to solve
C      NA     I     Number antennas maximum
C      IRET   I     Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C-----------------------------------------------------------------------
      INTEGER   NC, NI, NA, IRET
      CHARACTER PRGN*6
C
      INCLUDE 'TRUEP.INC'
      INTEGER   IROUND, NPARM, IERR, NFREQ, I, LUN, NUMAN(513), VER,
     *   ISBAND(MAXIF), FREQID
      CHARACTER PTYPE*2, STAT*4, BNDCOD(MAXIF)*8
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA LUN /57/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 184
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRBUF, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMI2, NAMIN2)
      CALL H2CHR (6, 1, XCLAI2, CLAIN2)
      SEQIN = IROUND (XSIN)
      SEQIN2 = IROUND (XSIN2)
      DISKIN = IROUND (XDISIN)
      DISKI2 = IROUND (XDISI2)
      MAGIC = IROUND (APARM(1))
      IF ((MAGIC.GT.0) .AND. (MAGIC.LE.MAXANT)) THEN
         IRET = 0
      ELSE
         IRET = 10
         MSGTXT = 'YOU MUST SPECIFY THE REF ANTENNA IN APARM(1)'
         END IF
      SUPRAD = 0
      SMTAB(1) = 0.0
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
C                                       find the data set
      UNAME = NAMEIN
      UCLAS = CLAIN
      IUDISK = DISKIN
      IUSEQ = SEQIN
      UDISK = DISKIN
      USEQ = SEQIN
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', IUDISK, OLDCNO, UNAME, UCLAS, IUSEQ, PTYPE,
     *   NLUSER, STAT, SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, UNAME, UCLAS, IUSEQ, IUDISK, NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', IUDISK, OLDCNO, CATBLK, 'REST', SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1005) IRET
         GO TO 990
         END IF
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       freq info
      REFCHN = CATR(KRCRP+JLOCF)
      RFREQ = CATD(KDCRV+JLOCF)
      VER = 1
      CALL CHNDAT ('READ', SCRBUF, IUDISK, OLDCNO, VER, CATBLK, LUN, NI,
     *   FOFF, ISBAND, FINC, BNDCOD, FREQID, IRET)
C                                       Channel selection?
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = IROUND (XBIF)
         EIF = IROUND (XEIF)
         BIF = MIN (MAX (1, BIF), CATBLK(KINAX+JLOCIF))
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         END IF
      NI = EIF - BIF + 1
      BCHAN = IROUND (XBCHAN)
      ECHAN = IROUND (XECHAN)
      NFREQ = CATBLK(KINAX+JLOCF)
      IF ((BCHAN.LE.0) .OR. (BCHAN.GT.NFREQ)) BCHAN = 1
      IF ((ECHAN.LE.0) .OR. (ECHAN.GT.NFREQ)) ECHAN = NFREQ
      IF (BCHAN.GT.ECHAN) THEN
         MSGTXT = 'INVALID BCHAN AND ECHAN'
         CALL MSGWRT (6)
         IRET = 1
         GO TO 990
         END IF
      NC = ECHAN - BCHAN + 1
      IF (DOSPEC.LE.0.0) NC = 1
C                                       get max ant number
      CALL GETNAN (IUDISK, OLDCNO, CATBLK, LUN, SCRBUF, NUMAN, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'ERROR GETTING NUMBERS OF ANTENNAS'
         GO TO 990
         END IF
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LT.0) SUBARR = 0
      IF ((SUBARR.GT.0) .AND. (SUBARR.LT.NUMAN(1))) THEN
         NA = NUMAN(SUBARR+1)
      ELSE
         NA = 0
         DO 10 I = 1,NUMAN(1)
            NA = MAX (NA, NUMAN(I+1))
 10         CONTINUE
         END IF
      IF (NA.LE.0) THEN
         MSGTXT = 'NO ANTENNAS FOUND'
         IRET = 8
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TRUEPI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1005 FORMAT ('ERROR',I3,' COPYING CATBLK FILE 1')
      END
      SUBROUTINE TRUEPG (II, NC, NI, NA, MATRL, MATLR, IRET)
C-----------------------------------------------------------------------
C   TRUEPG reads data set II and fills the RL and LR matrices with
C   averaged values.
C   Inputs:
C      II      I        Which data set
C   Outputs:
C      MATRL   R(2,*)   RL data
C      MATLR   R(2,*)   LR data
C      IRET    I        Error code
C-----------------------------------------------------------------------
      INCLUDE 'TRUEP.INC'
      INTEGER   II, NC, NI, NA, IRET
      REAL      MATRL(3,NC,NI,*), MATLR(3,NC,NI,*)
C
      CHARACTER PTYPE*2, STAT*4
      INTEGER   LUN, I1, I2, NCHAN, IROUND, NFREQ, NUMVIS, INCX,
     *   IA1, IA2, I, LF, L1, NIF
      LOGICAL   MATCH
      REAL      RPARM(20), VIS(3,MAXCIF), CATR(256), SUMR1, SUMR2,
     *   SUMI1, SUMI2, SUMW1, SUMW2
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
C-----------------------------------------------------------------------
      I = 3 * NC * NI * NA
      CALL RFILL (I, 0.0, MATRL)
      CALL RFILL (I, 0.0, MATLR)
C                                       init
      CALL SELINI
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (16, 1, XSOUR(1,II), SOURCS(1))
      IF ((II.EQ.2) .AND. (SOURCS(1).EQ.' ')) CALL H2CHR (16, 1, XSOUR,
     *   SOURCS(1))
      SELQUA = IROUND (XQUAL)
      STOKES = 'RLLR'
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      IF (II.EQ.1) THEN
         UNAME = NAMEIN
         UCLAS = CLAIN
         IUDISK = DISKIN
         IUSEQ = SEQIN
         UDISK = DISKIN
         USEQ = SEQIN
      ELSE
         UNAME = NAMIN2
         UCLAS = CLAIN2
         IUDISK = DISKI2
         IUSEQ = SEQIN2
         UDISK = DISKI2
         USEQ = SEQIN2
         END IF
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
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
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', IUDISK, OLDCNO, UNAME, UCLAS, IUSEQ, PTYPE,
     *   NLUSER, STAT, SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, UNAME, UCLAS, IUSEQ, IUDISK, NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', IUDISK, OLDCNO, CATBLK, 'REST', SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1005) IRET, II
         GO TO 990
         END IF
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Channel selection?
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
         NIF = 1
      ELSE
         BIF = IROUND (XBIF)
         EIF = IROUND (XEIF)
         NIF = CATBLK(KINAX+JLOCIF)
         BIF = MIN (MAX (1, BIF), NIF)
         IF (EIF.LT.BIF) EIF = NIF
         END IF
      NFREQ = CATBLK(KINAX+JLOCF)
      BCHAN = IROUND (XBCHAN)
      ECHAN = IROUND (XECHAN)
      IF ((BCHAN.LE.0) .OR. (BCHAN.GT.NFREQ)) BCHAN = 1
      IF ((ECHAN.LE.0) .OR. (ECHAN.GT.NFREQ)) ECHAN = NFREQ
      IF (BCHAN.GT.ECHAN) THEN
         MSGTXT = 'INVALID BCHAN AND ECHAN'
         CALL MSGWRT (6)
         IRET = 1
         GO TO 990
         END IF
C                                       get source flux
      CALL GETFL (II, SOURCS(1), IUDISK, OLDCNO, SFLUX(1,II))
      CALL GINDEX (II, NIF, SFLUX(1,II))
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, II
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, SCRBUF, IRET)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', IUDISK, OLDCNO, UNAME, UCLAS, IUSEQ, PTYPE,
     *   NLUSER, 'READ', SCRBUF, IRET)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET, II
         GO TO 990
         END IF
      NUMVIS = 0
      NCHAN = ECHAN - BCHAN + 1
      NIF = EIF - BIF + 1
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET, II
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         IF (ILOCB.GE.0) THEN
            IA2 = RPARM(1+ILOCB) + 0.1
            IA1 = IA2 / 256
            IA2 = IA2 - IA1*256
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         IF (IA1.EQ.MAGIC) THEN
            NUMVIS = NUMVIS + 1
            DO 130 LF = 1,NIF
               L1 = (LF - 1) * INCIFI
               IF (NC.EQ.1) THEN
                  SUMR1 = 0.0
                  SUMI1 = 0.0
                  SUMW1 = 0.0
                  SUMR2 = 0.0
                  SUMI2 = 0.0
                  SUMW2 = 0.0
                  DO 110 I = 1,NCHAN
                     I1 = (I - 1) * INCFI + 1 + L1
                     I2 = I1 + INCSI
                     IF (VIS(3,I1).GT.0.0) THEN
                        SUMR2 = SUMR2 + VIS(1,I1) * VIS(3,I1)
                        SUMI2 = SUMI2 - VIS(2,I1) * VIS(3,I1)
                        SUMW2 = SUMW2 + VIS(3,I1)
                        END IF
                     IF (VIS(3,I2).GT.0.0) THEN
                        SUMR1 = SUMR1 + VIS(1,I2) * VIS(3,I2)
                        SUMI1 = SUMI1 - VIS(2,I2) * VIS(3,I2)
                        SUMW1 = SUMW1 + VIS(3,I2)
                        END IF
 110                 CONTINUE
                  MATRL(1,1,LF,IA2) = MATRL(1,1,LF,IA2) + SUMR1
                  MATRL(2,1,LF,IA2) = MATRL(2,1,LF,IA2) + SUMI1
                  MATRL(3,1,LF,IA2) = MATRL(3,1,LF,IA2) + SUMW1
                  MATLR(1,1,LF,IA2) = MATLR(1,1,LF,IA2) + SUMR2
                  MATLR(2,1,LF,IA2) = MATLR(2,1,LF,IA2) + SUMI2
                  MATLR(3,1,LF,IA2) = MATLR(3,1,LF,IA2) + SUMW2
               ELSE
                  DO 120 I = 1,NCHAN
                     I1 = (I - 1) * INCFI + 1 + L1
                     I2 = I1 + INCSI
                     IF (VIS(3,I1).GT.0.0) THEN
                        MATLR(1,I,LF,IA2) = MATLR(1,I,LF,IA2) +
     *                     VIS(1,I1) * VIS(3,I1)
                        MATLR(2,I,LF,IA2) = MATLR(2,I,LF,IA2) -
     *                     VIS(2,I1) * VIS(3,I1)
                        MATLR(3,I,LF,IA2) = MATLR(3,I,LF,IA2) +
     *                     VIS(3,I1)
                        END IF
                     IF (VIS(3,I2).GT.0.0) THEN
                        MATRL(1,I,LF,IA2) = MATRL(1,I,LF,IA2) +
     *                     VIS(1,I2) * VIS(3,I2)
                        MATRL(2,I,LF,IA2) = MATRL(2,I,LF,IA2) -
     *                     VIS(2,I2) * VIS(3,I2)
                        MATRL(3,I,LF,IA2) = MATRL(3,I,LF,IA2) +
     *                     VIS(3,I2)
                        END IF
 120                 CONTINUE
                  END IF
 130           CONTINUE
         ELSE IF (IA2.EQ.MAGIC) THEN
            NUMVIS = NUMVIS + 1
            DO 180 LF = 1,NIF
               L1 = (LF - 1) * INCIFI
               IF (NC.EQ.1) THEN
                  SUMR1 = 0.0
                  SUMI1 = 0.0
                  SUMW1 = 0.0
                  SUMR2 = 0.0
                  SUMI2 = 0.0
                  SUMW2 = 0.0
                  DO 160 I = 1,NCHAN
                     I1 = (I - 1) * INCFI + 1 + L1
                     I2 = I1 + INCSI
                     IF (VIS(3,I1).GT.0.0) THEN
                        SUMR1 = SUMR1 + VIS(1,I1) * VIS(3,I1)
                        SUMI1 = SUMI1 + VIS(2,I1) * VIS(3,I1)
                        SUMW1 = SUMW1 + VIS(3,I1)
                        END IF
                     IF (VIS(3,I2).GT.0.0) THEN
                        SUMR2 = SUMR2 + VIS(1,I2) * VIS(3,I2)
                        SUMI2 = SUMI2 + VIS(2,I2) * VIS(3,I2)
                        SUMW2 = SUMW2 + VIS(3,I2)
                        END IF
 160                 CONTINUE
                  MATRL(1,1,LF,IA1) = MATRL(1,1,LF,IA1) + SUMR1
                  MATRL(2,1,LF,IA1) = MATRL(2,1,LF,IA1) + SUMI1
                  MATRL(3,1,LF,IA1) = MATRL(3,1,LF,IA1) + SUMW1
                  MATLR(1,1,LF,IA1) = MATLR(1,1,LF,IA1) + SUMR2
                  MATLR(2,1,LF,IA1) = MATLR(2,1,LF,IA1) + SUMI2
                  MATLR(3,1,LF,IA1) = MATLR(3,1,LF,IA1) + SUMW2
               ELSE
                  DO 170 I = 1,NCHAN
                     I1 = (I - 1) * INCFI + 1 + L1
                     I2 = I1 + INCSI
                     IF (VIS(3,I1).GT.0.0) THEN
                        MATRL(1,I,LF,IA1) = MATRL(1,I,LF,IA1) +
     *                     VIS(1,I1) * VIS(3,I1)
                        MATRL(2,I,LF,IA1) = MATRL(2,I,LF,IA1) +
     *                     VIS(2,I1) * VIS(3,I1)
                        MATRL(3,I,LF,IA1) = MATRL(3,I,LF,IA1) +
     *                     VIS(3,I1)
                        END IF
                     IF (VIS(3,I2).GT.0.0) THEN
                        MATLR(1,I,LF,IA1) = MATLR(1,I,LF,IA1) +
     *                     VIS(1,I2) * VIS(3,I2)
                        MATLR(2,I,LF,IA1) = MATLR(2,I,LF,IA1) +
     *                     VIS(2,I2) * VIS(3,I2)
                        MATLR(3,I,LF,IA1) = MATLR(3,I,LF,IA1) +
     *                     VIS(3,I2)
                        END IF
 170                 CONTINUE
                  END IF
 180           CONTINUE
            END IF
         GO TO 100
         END IF
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      IF (NUMVIS.LE.0) THEN
         WRITE (MSGTXT,1120) II
         IRET = 8
         GO TO 990
         END IF
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', IUDISK, OLDCNO, UNAME, UCLAS, IUSEQ, PTYPE,
     *   NLUSER, 'CLRD', SCRBUF, IRET)
      NCFILE = NCFILE - 1
      IRET = 0
C                                       Error
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1005 FORMAT ('ERROR',I3,' COPYING CATBLK FILE',I2)
 1010 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS FILE',I2)
 1020 FORMAT ('TRUEPG: ERROR',I3,' OPEN/INIT INPUT VIS FILE',I2)
 1100 FORMAT ('TRUEPG: ERROR',I3,' READING VIS FILE',I2)
 1120 FORMAT ('TRUEPG: FOUND NOTHING FOR FILE',I2)
      END
      SUBROUTINE TRUEPS (NC, NI, NA, MATRL1, MATLR1, MATRL2, MATLR2,
     *   DR1, DL1, DR2, DL2, IRET)
C-----------------------------------------------------------------------
C   Solves the matrices for the D terms
C   Inputs
C      NC       I               Number channels to solve
C      NI       I               Number IFs to solve
C      NA       I               Number of antennas to solve
C      MATRL1   R(3,NC,NI,NA)   RL matrix 1
C      MATLR1   R(3,NC,NI,NA)   LR matrix 1
C      MATRL2   R(3,NC,NI,NA)   RL matrix 2
C      MATLR2   R(3,NC,NI,NA)   LR matrix 2
C   Outputs:
C      IRET    I          Error
C-----------------------------------------------------------------------
      INCLUDE 'TRUEP.INC'
      INTEGER   NC, NI, NA, IRET
      REAL      MATRL1(3,NC,NI,*), MATLR1(3,NC,NI,*), MATRL2(3,NC,NI,*),
     *   MATLR2(3,NC,NI,*)
      COMPLEX   DR1(NC,NI,*), DL1(NC,NI,*), DR2(NC,NI,*), DL2(NC,NI,*)
C
      INTEGER   IA1, NS, I, NCHAN, NIF, LC, LF, PDBUFF(512), PDV, PDLUN,
     *   IPDRNO, PDKOLS(9), PDNUMV(9), NUMA, NUMP, NUMI, NUMF, J, KF,
     *   ND(2*MAXCIF), NPART
      REAL      S, AMP(4), PH(4), PHDIFF, AFLUX1, AFLUX2
      DOUBLE PRECISION SR, SI, SSR, SSI, TR, TI, TTR, TTI, F
      COMPLEX   VR1L2, RR1L2, VL1R2, RL1R2, SS, DD, NUM, DEN,
     *   DTERM1(2*MAXCIF), DTERM2(2*MAXCIF)
      CHARACTER POLTYP*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PDLUN /57/
C-----------------------------------------------------------------------
      NPART = 0
      NIF = EIF - BIF + 1
      NCHAN = ECHAN - BCHAN + 1
      IF (APARM(2).EQ.0.0) APARM(2) = 90.0
      S = APARM(2) * DG2RAD * 2.0
      NUM = CMPLX (1.0 + COS(S), SIN(S))
      DEN = CMPLX (1.0 - COS(S), -SIN(S))
      DO 40 LF = 1,NIF
         KF = LF + BIF - 1
         IF ((APARM(3).GT.0.5) .AND. (NC.EQ.1)) THEN
            WRITE (MSGTXT,1000) KF
            CALL MSGWRT (3)
            MSGTXT = 'DATA            UNROTATED                  ' //
     *         ' ROTATED'
            CALL MSGWRT (3)
            MSGTXT = ' ANT   AMP RL PHASE  AMP LR PHASE  AMP RL' //
     *         ' PHASE  AMP LR PHASE'
            CALL MSGWRT (3)
            END IF
         AFLUX1 = SFLUX(KF,1)
         AFLUX2 = SFLUX(KF,2)
         DO 30 LC = 1,NC
            IF (NC.GT.1) THEN
               F = RFREQ + FOFF(LF) + FINC(LF) * (LC - REFCHN)
               AFLUX1 = FZERO(1) * (F/RFREQ)**SINDEX(1)
               AFLUX2 = FZERO(2) * (F/RFREQ)**SINDEX(2)
               END IF
            DO 20 IA1 = 1,NA
               DR1(LC,LF,IA1) = CMPLX (FBLANK, FBLANK)
               DL1(LC,LF,IA1) = CMPLX (FBLANK, FBLANK)
               DR2(LC,LF,IA1) = CMPLX (FBLANK, FBLANK)
               DL2(LC,LF,IA1) = CMPLX (FBLANK, FBLANK)
               S = MATRL1(3,LC,LF,IA1) + MATRL2(3,LC,LF,IA1) +
     *            MATLR1(3,LC,LF,IA1) + MATLR2(3,LC,LF,IA1)
C                                       some good, not all
               IF ((S.GT.0.0) .AND. ((MATRL1(3,LC,LF,IA1).LE.0.0) .OR.
     *            (MATRL2(3,LC,LF,IA1).LE.0.0) .OR.
     *            (MATLR1(3,LC,LF,IA1).LE.0.0) .OR.
     *            (MATLR2(3,LC,LF,IA1).LE.0.0))) THEN
                  IF (NC.EQ.1) THEN
                     WRITE (MSGTXT,1005) IA1, MATRL1(3,LC,LF,IA1),
     *                  MATRL2(3,LC,LF,IA1), MATLR1(3,LC,LF,IA1),
     *                  MATLR2(3,LC,LF,IA1)
                     CALL MSGWRT (7)
                  ELSE
                     NPART = NPART + 1
                     END IF
C                                       4 good can solve
               ELSE IF (S.GT.0.0) THEN
C                                       do averages
                  MATRL1(1,LC,LF,IA1) = MATRL1(1,LC,LF,IA1) /
     *               MATRL1(3,LC,LF,IA1) / AFLUX1
                  MATRL1(2,LC,LF,IA1) = MATRL1(2,LC,LF,IA1) /
     *               MATRL1(3,LC,LF,IA1) / AFLUX1
                  MATRL2(1,LC,LF,IA1) = MATRL2(1,LC,LF,IA1) /
     *               MATRL2(3,LC,LF,IA1) / AFLUX2
                  MATRL2(2,LC,LF,IA1) = MATRL2(2,LC,LF,IA1) /
     *               MATRL2(3,LC,LF,IA1) / AFLUX2
                  MATLR1(1,LC,LF,IA1) = MATLR1(1,LC,LF,IA1) /
     *               MATLR1(3,LC,LF,IA1) / AFLUX1
                  MATLR1(2,LC,LF,IA1) = MATLR1(2,LC,LF,IA1) /
     *               MATLR1(3,LC,LF,IA1) / AFLUX1
                  MATLR2(1,LC,LF,IA1) = MATLR2(1,LC,LF,IA1) /
     *               MATLR2(3,LC,LF,IA1) / AFLUX2
                  MATLR2(2,LC,LF,IA1) = MATLR2(2,LC,LF,IA1) /
     *               MATLR2(3,LC,LF,IA1) / AFLUX2
                  IF ((APARM(3).GT.0.5) .AND. (NC.EQ.1)) THEN
                     AMP(1) = SQRT (MATRL1(1,LC,LF,IA1)**2 +
     *                  MATRL1(2,LC,LF,IA1)**2)
                     AMP(2) = SQRT (MATLR1(1,LC,LF,IA1)**2 +
     *                  MATLR1(2,LC,LF,IA1)**2)
                     AMP(3) = SQRT (MATRL2(1,LC,LF,IA1)**2 +
     *                  MATRL2(2,LC,LF,IA1)**2)
                     AMP(4) = SQRT (MATLR2(1,LC,LF,IA1)**2 +
     *                  MATLR2(2,LC,LF,IA1)**2)
                     CALL RFILL (4, 0.0, PH)
                     IF (AMP(1).GT.0.0) PH(1) = RAD2DG * ATAN2
     *                  (MATRL1(2,LC,LF,IA1), MATRL1(1,LC,LF,IA1))
                     IF (AMP(2).GT.0.0) PH(2) = RAD2DG * ATAN2
     *                  (MATLR1(2,LC,LF,IA1), MATLR1(1,LC,LF,IA1))
                     IF (AMP(3).GT.0.0) PH(3) = RAD2DG * ATAN2
     *                  (MATRL2(2,LC,LF,IA1), MATRL2(1,LC,LF,IA1))
                     IF (AMP(4).GT.0.0) PH(4) = RAD2DG * ATAN2
     *                  (MATLR2(2,LC,LF,IA1), MATLR2(1,LC,LF,IA1))
                     WRITE (MSGTXT,1010) IA1, MAGIC, (AMP(I), PH(I),
     *                  I = 1,4)
                     CALL MSGWRT (3)
                     END IF
                  VR1L2 = CMPLX (MATRL1(1,LC,LF,IA1),
     *               MATRL1(2,LC,LF,IA1))
                  RR1L2 = CMPLX (MATRL2(1,LC,LF,IA1),
     *               MATRL2(2,LC,LF,IA1))
                  SS = VR1L2 + RR1L2
                  DD = VR1L2 - RR1L2
                  DR1(LC,LF,IA1) = 0.5 * (SS - DD * NUM / DEN)
                  DL2(LC,LF,IA1) = CONJG (DD / DEN)
                  VL1R2 = CMPLX (MATLR1(1,LC,LF,IA1),
     *               MATLR1(2,LC,LF,IA1))
                  RL1R2 = CMPLX (MATLR2(1,LC,LF,IA1),
     *               MATLR2(2,LC,LF,IA1))
                  SS = VL1R2 + RL1R2
                  DD = VL1R2 - RL1R2
                  DL1(LC,LF,IA1) = 0.5 * (SS - DD * NUM / DEN)
                  DR2(LC,LF,IA1) = CONJG (DD / DEN)
                  END IF
 20            CONTINUE
 30         CONTINUE
 40      CONTINUE
      IF (NPART.GT.0) THEN
         WRITE (MSGTXT,1040) NPART
         CALL MSGWRT (7)
         END IF
C                                       report results
      IF (NC.EQ.1) THEN
         LC = 1
         DO 60 LF = 1,NIF
            NS = 0
            SR = 0.0D0
            SI = 0.0D0
            SSR = 0.0D0
            SSI = 0.0D0
            TR = 0.0D0
            TI = 0.0D0
            TTR = 0.0D0
            TTI = 0.0D0
            WRITE (MSGTXT,1000) LF+BIF-1
            CALL MSGWRT (4)
            MSGTXT = 'D TERMS         ANTENNA 1                  ' //
     *         ' ANTENNA 2'
            CALL MSGWRT (4)
            MSGTXT = ' ANT   AMP  R PHASE  AMP  L PHASE  AMP  R' //
     *         ' PHASE  AMP  L PHASE'
            CALL MSGWRT (4)
            DO 50 IA1 = 1,NA
               S = MATRL1(3,LC,LF,IA1) * MATRL2(3,LC,LF,IA1) *
     *            MATLR1(3,LC,LF,IA1) * MATLR2(3,LC,LF,IA1)
               IF (S.GT.0.0) THEN
                  AMP(1) = CABS (DR1(LC,LF,IA1))
                  AMP(2) = CABS (DL1(LC,LF,IA1))
                  PH(1) = 0.0
                  PH(2) = 0.0
                  IF (AMP(1).GT.0.0) PH(1) = RAD2DG * ATAN2
     *               (AIMAG(DR1(LC,LF,IA1)), REAL(DR1(LC,LF,IA1)))
                  IF (AMP(2).GT.0.0) PH(2) = RAD2DG * ATAN2
     *               (AIMAG(DL1(LC,LF,IA1)), REAL(DL1(LC,LF,IA1)))
                  AMP(3) = CABS (DR2(LC,LF,IA1))
                  AMP(4) = CABS (DL2(LC,LF,IA1))
                  PH(3) = 0.0
                  PH(4) = 0.0
                  IF (AMP(3).GT.0.0) PH(3) = RAD2DG * ATAN2
     *               (AIMAG(DR2(LC,LF,IA1)), REAL(DR2(LC,LF,IA1)))
                  IF (AMP(4).GT.0.0) PH(4) = RAD2DG * ATAN2
     *               (AIMAG(DL2(LC,LF,IA1)), REAL(DL2(LC,LF,IA1)))
                  WRITE (MSGTXT,1020) IA1, MAGIC, (AMP(I), PH(I),
     *               I = 1,4)
                  CALL MSGWRT (4)
                  NS = NS + 1
                  SR = SR + REAL (DR2(LC,LF,IA1))
                  SI = SI + AIMAG (DR2(LC,LF,IA1))
                  SSR = SSR + REAL (DR2(LC,LF,IA1))**2
                  SSI = SSI + AIMAG (DR2(LC,LF,IA1))**2
                  TR = TR + REAL (DL2(LC,LF,IA1))
                  TI = TI + AIMAG (DL2(LC,LF,IA1))
                  TTR = TTR + REAL (DL2(LC,LF,IA1))**2
                  TTI = TTI + AIMAG (DL2(LC,LF,IA1))**2
                  END IF
 50            CONTINUE
            IF (NS.GT.0.0) THEN
               SR = SR / NS
               SI = SI / NS
               SSR = MAX (0.0D0, SSR / NS - SR * SR)
               SSI = MAX (0.0D0, SSI / NS - SI * SI)
               AMP(1) = SQRT (SR*SR + SI*SI)
               IF (AMP(1).GT.0.0) THEN
                  PH(1) = ATAN2 (SI, SR) * RAD2DG
                  AMP(2) = SQRT (SR*SR*SSR + SI*SI*SSI) / AMP(1)
                  PH(2) = SQRT (SR*SR*SSI + SI*SI*SSR) / AMP(1) * RAD2DG
               ELSE
                  PH(1) = 0.0
                  AMP(2) = 0.0
                  PH(2) = 0.0
                  END IF
               TR = TR / NS
               TI = TI / NS
               TTR = MAX (0.0D0, TTR / NS - TR * TR)
               TTI = MAX (0.0D0, TTI / NS - TI * TI)
               AMP(3) = SQRT (TR*TR + TI*TI)
               IF (AMP(3).GT.0.0) THEN
                  PH(3) = ATAN2 (TI, TR) * RAD2DG
                  AMP(4) = SQRT (TR*TR*TTR + TI*TI*TTI) / AMP(3)
                  PH(4) = SQRT (TR*TR*TTI + TI*TI*TTR) / AMP(3) * RAD2DG
               ELSE
                  PH(3) = 0.0
                  AMP(4) = 0.0
                  PH(4) = 0.0
                  END IF
               WRITE (MSGTXT,1030) NS, MAGIC
               CALL MSGWRT (4)
               MSGTXT = ' R   AMP  +- SIGa   PH  +- SIGp   ' //
     *            'L   AMP  +- SIGa   PH  +- SIGp'
               CALL MSGWRT (4)
               WRITE (MSGTXT,1035) AMP(1), AMP(2), PH(1), PH(2), AMP(3),
     *            AMP(4), PH(3), PH(4)
               CALL MSGWRT (4)
               END IF
 60         CONTINUE
C                                       write out a PD table
      ELSE
         PDV = 0
         NUMA = NA
         NUMP = 2
         NUMI = NI
         NUMF = NC
         POLTYP = 'APPROX'
         PHDIFF = 0.0
         CALL PDINI ('WRIT', PDBUFF, IUDISK, OLDCNO, PDV, CATUV,
     *      PDLUN, IPDRNO, PDKOLS, PDNUMV, NUMA, NUMP, NUMI, NUMF,
     *      POLTYP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1060) IRET, 'CREATE NEW PD TABLE'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         WRITE (MSGTXT,1061) PDV
         CALL MSGWRT (4)
         LC = 2 * NC * NI
         DO 62 I = 1,LC
            ND(I) = 0
            DTERM2(I) = CMPLX (0.0, 0.0)
 62         CONTINUE
         CALL FILL (2*MAXCIF, 0.0, ND)
         DO 90 I = 1,NA
            IF (I.NE.MAGIC) THEN
               J = 0
               DO 70 LF = 1,NI
                  DO 65 LC = 1,NC
                     J = J + 1
                     DTERM1(J) = DR1(LC,LF,I)
                     IF (REAL(DR2(LC,LF,I)).NE.FBLANK) THEN
                        DTERM2(J) = DTERM2(J) + DR2(LC,LF,I)
                        ND(J) = ND(J) + 1
                        END IF
 65                  CONTINUE
 70               CONTINUE
               DO 80 LF = 1,NI
                  DO 75 LC = 1,NC
                     J = J + 1
                     DTERM1(J) = DL1(LC,LF,I)
                     IF (REAL(DL2(LC,LF,I)).NE.FBLANK) THEN
                        DTERM2(J) = DTERM2(J) + DL2(LC,LF,I)
                        ND(J) = ND(J) + 1
                        END IF
 75                  CONTINUE
 80               CONTINUE
               CALL SMTHIT (NC, NI, DTERM1)
               CALL TABPD ('WRIT', PDBUFF, IPDRNO, PDKOLS, PDNUMV, NUMI,
     *            NUMF, NUMP, I, SUBARR, FRQSEL, MAGIC, PHDIFF, DTERM1,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1060) IRET, 'WRITE NEW PD TABLE'
                  CALL MSGWRT (8)
                  GO TO 990
                  END IF
               END IF
 90         CONTINUE
         DO 95 I = 1,J
            IF (ND(I).GT.0) THEN
               DTERM2(I) = DTERM2(I) / ND(I)
            ELSE
               DTERM2(I) = FBLANK
               END IF
 95         CONTINUE
         CALL SMTHIT (NC, NI, DTERM2)
         CALL TABPD ('WRIT', PDBUFF, IPDRNO, PDKOLS, PDNUMV, NUMI, NUMF,
     *      NUMP, MAGIC, SUBARR, FRQSEL, MAGIC, PHDIFF, DTERM2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1060) IRET, 'WRITE NEW PD TABLE REFANT'
            CALL MSGWRT (8)
            END IF
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL TABPD ('CLOS', PDBUFF, IPDRNO, PDKOLS, PDNUMV, NUMI, NUMF,
     *   NUMP, MAGIC, SUBARR, FRQSEL, MAGIC, PHDIFF, DTERM2, I)

 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BEGIN IF',I3)
 1005 FORMAT ('ANT',I3,' WTS ',4(1PE10.2),' NOT ALL >0')
 1010 FORMAT (I2.2,'-',I2.2,4(F8.3,F6.1))
 1020 FORMAT (I2.2,'&',I2.2,4(F8.4,F6.1))
 1030 FORMAT ('Average over',I3,' samples for antenna',I3.2)
 1035 FORMAT ('**',2F8.5,F7.2,F6.2,4X,2F8.5,F7.2,F6.2)
 1040 FORMAT (I6,' chan * ant were missing data from 1 but not the',
     *   ' other data set')
 1060 FORMAT ('TRUEPS: ERROR',I4,' ON ',A)
 1061 FORMAT ('TRUEPS writing PD table version',I5)
      END
      SUBROUTINE GINDEX (II, NI, SFL)
C-----------------------------------------------------------------------
C   Find spectral index
C   Inputs:
C      II       I      Which data set
C      NI       I      Number values
C      SFL      R(*)   Flux values
C   Output in Common:
C      FZERO    R(2)   (II) flux at ref freq
C      SINDEX   R(2)   (II) spectral index
C-----------------------------------------------------------------------
      INTEGER   II, NI
      REAL      SFL(*)
C
      INCLUDE 'TRUEP.INC'
      INTEGER   I, N
      DOUBLE PRECISION SX, SXX, SY, SXY, X, Y
C-----------------------------------------------------------------------
      SX = 0.0D0
      SY = 0.0D0
      SXX = 0.0D0
      SXY = 0.0D0
      FZERO(II) = SFL(1)
      SINDEX(II) = 0.0
      IF (NI.GT.1) THEN
         N = 0
         DO 20 I = 1,NI
            IF (SFL(I).GT.0.0) THEN
               Y = LOG10 (SFL(I))
               X = LOG10 (1.0D0 + FOFF(I)/RFREQ)
               SX = SX + X
               SY = SY + Y
               SXX = SXX + X * X
               SXY = SXY + X * Y
               N = N + 1
               END IF
 20         CONTINUE
         X = N * SXX - SX * SX
         IF ((N.GT.0) .AND. (X.GT.0.0D0)) THEN
            SINDEX(II) = (N * SXY - SX * SY) / X
            FZERO(II) = (SY * SXX - SX * SXY) / X
            FZERO(II) = 10.0 ** FZERO(II)
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE GETFL (II, SOURCE, DISK, CNO, SFLUX)
C-----------------------------------------------------------------------
C   reads source table and gets a flux
C   Inputs:
C      II       I      Which file - for message
C      SOURCE   C*16   Source name
C      DISK     I      Disk number
C      CNO      I      Catalog number
C   Outputs:
C      SFLUX     R      Flux
C-----------------------------------------------------------------------
      INTEGER   II, DISK, CNO
      CHARACTER SOURCE*(*)
      REAL      SFLUX(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IRET, BUFFER(512), NUMREC, IRNO, LOOP, XDSOUR, XQUAL,
     *   XUFQID, VER, LUN, KOLS(MAXSUC), NUMV(MAXSUC), NUMIF, I
      REAL      XFLUX(4,MAXIF)
      DOUBLE PRECISION XFREQO(MAXIF), XBANDW, XRAEPO, XECEPO, XEPOCH,
     *   XRAAPP, XECAPP, XSRVEL(MAXIF), XESTFQ(MAXIF), XPMRA, XPMDEC,
     *   XRAOBS, XDECOB
      CHARACTER XSNAME*16, XALCOD*4, XELTYP*8, XELDEF*8
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C      INCLUDE 'INCS:DSOU.INC'
C-----------------------------------------------------------------------
      CALL RFILL (MAXIF, 0.0, SFLUX)
      IF (SOURCE.NE.' ') THEN
         MSGSUP = 32000
         VER = 1
         LUN = 69
         CALL SOUINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *      NUMIF, XELTYP, XELDEF, XUFQID, IRNO, KOLS, NUMV, IRET)
         IF (IRET.EQ.0) THEN
            NUMREC = BUFFER(5)
            DO 20 LOOP = 1,NUMREC
               IRNO = LOOP
               CALL TABSOU ('READ', BUFFER, IRNO, KOLS, NUMV, XDSOUR,
     *            XSNAME, XQUAL, XALCOD, XFLUX, XFREQO, XBANDW, XRAEPO,
     *            XECEPO, XEPOCH, XRAAPP, XECAPP, XRAOBS, XDECOB,
     *            XSRVEL, XESTFQ, XPMRA, XPMDEC, IRET)
               IF (IRET.EQ.0) THEN
                  IF (XSNAME.EQ.SOURCE) THEN
                     DO 15 I = 1,NUMIF
                        SFLUX(I) = XFLUX(1,I)
 15                     CONTINUE
                     GO TO 25
                     END IF
               ELSE IF (IRET.GT.0) THEN
                  GO TO 25
                  END IF
 20            CONTINUE
 25         CALL TABSOU ('CLOS', BUFFER, IRNO, KOLS, NUMV, XDSOUR,
     *         XSNAME, XQUAL, XALCOD, XFLUX, XFREQO, XBANDW, XRAEPO,
     *         XECEPO, XEPOCH, XRAAPP, XECAPP, XRAOBS, XDECOB, XSRVEL,
     *         XESTFQ, XPMRA, XPMDEC, LOOP)
            END IF
         END IF
      MSGSUP = 0
      DO 30 I = 1,NUMIF
         IF (SFLUX(I).LE.0) THEN
            SFLUX(I) = 1.0
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1025) II, I, SFLUX(I), IRET
            ELSE
               WRITE (MSGTXT,1026) II, I, SFLUX(I)
               END IF
         ELSE
            WRITE (MSGTXT,1027) II, I, SFLUX(I)
            END IF
         CALL MSGWRT (2)
 30      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1025 FORMAT ('File',I2,' IF',I3,' flux=',F7.3,'  Error in SU table',I3)
 1026 FORMAT ('File',I2,' IF',I3,' flux=',F7.3,'  not found in table')
 1027 FORMAT ('File',I2,' IF',I3,' flux=',F7.3,'  found in SU table')
      END
      SUBROUTINE SMTHIT (NC, NI, DTERM)
C-----------------------------------------------------------------------
C   Applies frquency smoothing if desired
C   Inputs:
C      NC      I           number spectral channels
C      NI      I           number spectral bands (IFs)
C   In/out:
C      DTERM   C(NC,NI,2)   Answers to be smoothed
C-----------------------------------------------------------------------
      INTEGER   NC, NI
      COMPLEX   DTERM(*)
C
      INCLUDE 'TRUEP.INC'
      INTEGER   IT, IROUND, N, IP, II, IC, JB, JE, J1, J2, J, K, L, I
      REAL      FX, W, X, WIDTHS(4), SUPS(4)
      COMPLEX   WTERM(2*MAXCIF)
      INCLUDE 'INCS:DDCH.INC'
      DATA WIDTHS /4.0, 2.0, 2.0, 3.0/
      DATA SUPS /1.0, 3.0, 1.0, 4.0/
C-----------------------------------------------------------------------
C                                       exit on no smoothing
      IT = IROUND (APARM(4))
      IF (IT.LE.0) GO TO 999
C                                       init functions
      IF ((SUPRAD.LE.0) .OR. (SMTABL(1).LE.0.0)) THEN
         IT = MOD (IT-1, 4) + 1
         APARM(4) = IT
         IF ((APARM(5).LT.0.5) .OR. (APARM(5).GT.NC/3)) APARM(5) =
     *      WIDTHS(IT)
         IF ((APARM(6).GT.4.*SUPS(IT)*APARM(5)) .OR.
     *      (APARM(6).LT.APARM(5))) APARM(6) = SUPS(IT) * APARM(5)
         SUPRAD = APARM(6) / 2.0 + 0.1
         IF (SUPRAD+1.GT.SMOMAX) THEN
            SUPRAD = SMOMAX - 1
            APARM(5) = (2. * SUPRAD) / SUPS(IT)
            END IF
         APARM(6) = 2.0 * SUPRAD + 1.0
         N = 1 + SUPRAD
         FX = 2.0 / APARM(5)
         SMTABL(1) = 1.0
C                                       Compute look-up tables
         W = SMTABL(1)
C                                       Hanning smooth
         IF (IT.EQ.1) THEN
            DO 10 I = 2,N
               X = I - 1.0
               SMTABL(I) = MAX (0.0, 1.0-FX*X)
               W = W + 2 * SMTABL(I)
 10            CONTINUE
C                                       Gaussian smooth
         ELSE IF (IT.EQ.2) THEN
            FX = -LOG(2.0) * FX * FX
            DO 20 I = 2,N
               X = I - 1.0
               SMTABL(I) = EXP (FX * X * X)
               W = W + 2 * SMTABL(I)
 20            CONTINUE
C                                       Boxcar smooth
         ELSE IF (IT.EQ.3) THEN
            FX = 1.0 / FX
            DO 30 I = 2,N
               X = I - 1.0
               IF (X.LT.FX) THEN
                  SMTABL(I) = 1.0
               ELSE IF (X.EQ.FX) THEN
                  SMTABL(I) = 0.5
                  END IF
               W = W + 2 * SMTABL(I)
 30            CONTINUE
C                                      Sinc smooth
         ELSE IF (IT.EQ.4) THEN
            FX = 3.14159 * FX
            DO 40 I = 2,N
               X = (I - 1.0) * FX
               SMTABL(I) = SIN(X) / X
               W = W + 2 * SMTABL(I)
 40            CONTINUE
            END IF
C                                       Normalize integral
         IF (W.LE.0.0) W = 1.0
         DO 50 I = 1,N
            SMTABL(I) = SMTABL(I) / W
 50         CONTINUE
         END IF
C                                       now apply
      DO 200 IP = 1,2
         DO 190 II = 1,NI
            JB = 1 + (II - 1) * NC + (IP - 1) * NC * NI
            JE = JB + NC - 1
            DO 120 IC = 1,NC
               K = JB + IC - 1
               J1 = MAX (K - SUPRAD, JB)
               J2 = MIN (K + SUPRAD, JE)
               WTERM(K) = CMPLX (0.0, 0.0)
               W = 0.0
               DO 110 J = J1,J2
                  IF (REAL(DTERM(J)).NE.FBLANK) THEN
                     L = ABS (K-J) + 1
                     WTERM(K) = WTERM(K) + SMTABL(L) * DTERM(J)
                     W = W + SMTABL(L)
                     END IF
 110              CONTINUE
               IF (W.GT.0.0) THEN
                  WTERM(K) = WTERM(K) / W
               ELSE
                  WTERM(K) = CMPLX (FBLANK, FBLANK)
                  END IF
 120           CONTINUE
            DO 130 J = JB,JE
               DTERM(J) = WTERM(J)
 130           CONTINUE
 190        CONTINUE
 200     CONTINUE
C
 999  RETURN
      END
