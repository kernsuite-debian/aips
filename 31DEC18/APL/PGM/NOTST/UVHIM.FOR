LOCAL INCLUDE 'UVHIM.INC'
      REAL      XSEQ, XDISK, XQUAL, XBAND, XFREQ, XFQID, XTIME(8),
     *   XANT(50), XBASE(50), XUVRA(2), XSUBA, XBCHAN, XECHAN, XCHINC,
     *   XBIF, XEIF, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG,
     *   XDOBND, XBPVER, XSMOTH(3), XOSEQ, XODISK, XIMSIZ(2), BPARM(10),
     *   DOBLNK, BADD(10)
      HOLLERITH XNAME(3), XCLASS(2), XSOUR(4,30), XCALC, XSTOK,
     *   XONAME(3), XOCLAS(2), MAPH(256)
      REAL      DOMAIN(2,2), MAPR(256)
      INTEGER   INDISK, CNO, NSUBA, NFRQ, CHINC, IMSIZE(2), CSIZE,
     *   CTYPE, DISKO, CNOO, MAPHDR(256), SEQO, INSEQ, ATYPE(2), NOUT,
     *   NIN, SCRTCH(256)
      LOGICAL   HERMIT
      DOUBLE PRECISION MAPD(128)
      CHARACTER INNAME*12, INCLAS*6, OUNAME*12, OUCLAS*6
      EQUIVALENCE (MAPHDR, MAPH, MAPR, MAPD)
      COMMON /UVHIMP/ XNAME, XCLASS, XSEQ, XDISK, XSOUR, XQUAL, XCALC,
     *   XSTOK, XBAND, XFREQ, XFQID, XTIME, XANT, XBASE, XUVRA, XSUBA,
     *   XBCHAN, XECHAN, XCHINC, XBIF, XEIF, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XIMSIZ, XONAME,
     *   XOCLAS, XOSEQ, XODISK, BPARM, DOBLNK, BADD
      COMMON /UVHIMI/ MAPHDR, SCRTCH, INDISK, CNO, NSUBA, NFRQ, CHINC,
     *   IMSIZE, DOMAIN, CSIZE, CTYPE, DISKO, CNOO, INSEQ, SEQO,
     *   ATYPE, NOUT, NIN, HERMIT
      COMMON /UVHIMC/ INNAME, INCLAS, OUNAME, OUCLAS
LOCAL END
      PROGRAM UVHIM
C-----------------------------------------------------------------------
C! Makes image of 2-D histogram of a UV data set.
C# Util UV Analysis
C-----------------------------------------------------------------------
C;  Copyright (C) 2006, 2008-2010, 2012, 2015-2016, 2018
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
C   UVHIM makes an image of a 2-D histogram of a UV data set.
C   Adverbs:
C      INNAME(3)    uv name  (12 chars).
C      INCLASS(2)   uv class ( 6 chars).
C      INSEQ        uv sequence number.
C      INDISK       uv disk.
C      STOKES       Stokes parameter (I, Q, U, V, RR, LL, RL, LR).
C      BCHAN        1st Spectral channel number.
C      ECHAN        last Spectral channel
C      BIF          1st IF band to use
C      EIF          last Spectral channel
C-----------------------------------------------------------------------
      INTEGER   IERR, NWORDS
      LONGINT   OFFSET, COFSET
      REAL      HIMAG(2), HCONV(2)
      INCLUDE 'UVHIM.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Initialize and open file.
      CALL UVHGIN (IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'INITIALIZATION PROBLEM.'
         GO TO 990
         END IF
C                                       make memory for image
      NWORDS = (IMSIZE(1) * IMSIZE(2) - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, HIMAG, OFFSET, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'CANNOT MAKE MEMORY FOR IMAGE'
         GO TO 990
         END IF
      NWORDS = CSIZE * CSIZE
      HCONV(1) = 1.0
      COFSET = 0
      IF (NWORDS.GT.1) THEN
         NWORDS = (NWORDS - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, HCONV, COFSET, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'CANNOT MAKE MEMORY FOR CONVOLUTION FUNCTION'
            GO TO 990
            END IF
         CALL BLDCNV (CSIZE, CTYPE, HCONV(1+COFSET))
         END IF
C                                       Accumulate statistics from the
C                                       uv data.
      CALL BLDHGM (IMSIZE(1), IMSIZE(2), HIMAG(1+OFFSET), CSIZE,
     *   HCONV(1+COFSET), IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'PROBLEM PROCESSING THE UV DATA.'
         GO TO 990
         END IF
C                                       write out image
      CALL WRIHGM (IMSIZE(1), IMSIZE(2), HIMAG(1+OFFSET), IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'PROBLEM WRITING OUT THE IMAGE'
         GO TO 990
         END IF
      GO TO 995
C
 990  CALL MSGWRT (8)
C
 995  CALL DIE (IERR, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE UVHGIN (IERR)
C-----------------------------------------------------------------------
C    UVHGIN gets adverbs for UVHIM, opens the uv file, and determines
C    what is to be done.
C    Inputs:
C       IERR         I       Error code, 0 means success.
C   Output in Common:
C       .....        R(*)    AIPS adverbs values.
C       NBINS        I       Number of histogram bins.
C       INDISK       I       uv input disk number.
C       CNO          I       uv catalog slot number.
C       BCHAN        I       Frequency channel.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      CHARACTER PRGM*6, PTYPE*2, CBLANK*6
      LOGICAL   TABLE, EXIST, FITASC, MATCH
      INTEGER   I, IRET, IROUND, IUSER, LUN, FQVER, NIF, JERR, UVLUN,
     *   UVIND, NPARMS
      HOLLERITH CATH(256), CATUH(256)
      REAL      CATR(256), CATUR(256)
      DOUBLE PRECISION CATD(128), CATUD(128)
      INCLUDE 'UVHIM.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      EQUIVALENCE (CATUV, CATUH, CATUR, CATUD)
      DATA PRGM /'UVHIM '/
      DATA CBLANK /' '/
C-----------------------------------------------------------------------
C                                       Initialize parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARMS = 290
      CALL GTPARM (PRGM, NPARMS, RQUICK, XNAME, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010)
         CALL MSGWRT (8)
         RQUICK = .TRUE.
         CALL RELPOP (IERR, SCRTCH, IRET)
         GO TO 999
         END IF
C                                       Restart AIPS.
      IF (RQUICK) CALL RELPOP (IERR, SCRTCH, IRET)
C                                       Decode adverb values.
C                                       AIPS user number.
      IUSER = NLUSER
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
C                                       Input uv name etc.
      CALL H2CHR (12, 1, XNAME, INNAME)
      CALL H2CHR (6, 1, XCLASS, INCLAS)
      INSEQ  = IROUND (XSEQ)
      INDISK = IROUND (XDISK)
      CALL H2CHR (12, 1, XONAME, OUNAME)
      CALL H2CHR (6, 1, XOCLAS, OUCLAS)
      SEQO  = IROUND (XOSEQ)
      DISKO = IROUND (XODISK)
      CALL H2CHR (4, 1, XSTOK, STOKES)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
C                                       Number of histogram bins.
      IMSIZE(1) = IROUND (XIMSIZ(1))
      IMSIZE(2) = IROUND (XIMSIZ(2))
      IF (IMSIZE(1).LE.64) IMSIZE(1) = 256
      IF (IMSIZE(2).LE.64) IMSIZE(2) = 256
C                                       Open the uv file and get its
C                                       catalog header.
      UVLUN = 45
      PTYPE = 'UV'
      CALL MAPOPN ('READ', INDISK, INNAME, INCLAS, INSEQ, PTYPE, IUSER,
     *   UVLUN, UVIND, CNO, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'UVHGIN: ERROR OPENING UV FILE.'
         IERR = 1
         GO TO 990
         END IF
      CALL CHR2H (12, INNAME, 1, XNAME)
      CALL CHR2H (6, INCLAS, 1, XCLASS)
      XDISK = INDISK
      XSEQ = INSEQ
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = INDISK
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 0
      CALL COPY (256, CATBLK, CATUV)
C                                       Get uv pointer information from
C                                       the catalog header.
      CALL UVPGET (IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'UVHGIN: ERROR GETTING OFFSETS FROM UV FILE HEADER.'
         IERR = 1
         GO TO 990
         END IF
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = INNAME
      UCLAS = INCLAS
      UDISK = INDISK
      USEQ = INSEQ
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
C                                       Check spectral channel.
      BCHAN = 1
      ECHAN = 1
      IF (JLOCF.GE.0) THEN
         I = CATBLK(KINAX+JLOCF)
         IF (I.GT.1) THEN
            BCHAN = IROUND (XBCHAN)
            ECHAN = IROUND (XECHAN)
            IF (BCHAN.LE.0) BCHAN = 1
            IF ((ECHAN.LT.BCHAN) .OR. (ECHAN.GT.I)) ECHAN = I
            IF (BCHAN.GT.I) THEN
               WRITE (MSGTXT,1020) BCHAN, I
               IERR = 1
               GO TO 990
               END IF
            END IF
         END IF
      CHINC = XCHINC
      CHINC = MAX (1, CHINC)
      IF (CHINC.GT.ECHAN-BCHAN+1) CHINC = 1
C                                       Check IF band
      BIF = 1
      EIF = 1
      IF (JLOCIF.GT.1) THEN
         I = CATBLK(KINAX+JLOCIF)
         IF (I.GT.1) THEN
            BIF = IROUND (XBIF)
            EIF = IROUND (XEIF)
            IF (BIF.LE.0) BIF = 1
            IF ((EIF.LT.BIF) .OR. (EIF.GT.I)) EIF = I
            IF (BIF.GT.I) THEN
               WRITE (MSGTXT,1025) BIF, I
               IERR = 1
               GO TO 990
               END IF
            END IF
         END IF
      XBCHAN = BCHAN
      XECHAN = ECHAN
      XCHINC = CHINC
      XBIF = BIF
      XEIF = EIF
      DOPOL = IROUND(XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      DOAPPL = .FALSE.
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
C                                       Allow multiple subarrays
      CALL FNDEXT ('AN', CATBLK, NSUBA)
      IF ((SUBARR.GT.0) .AND. (SUBARR.LE.NSUBA)) NSUBA = 1
      NSUBA = MAX (1, NSUBA)
C                                       Allow multiple FQ ids
      NFRQ = 1
      IF ((FRQSEL.LE.0) .AND. (SELBAN.LE.0.0) .AND. (SELFRQ.LE.0D0))
     *   THEN
         FRQSEL = 1
C                                       Determine the number of FREQIDs.
         FQVER = 1
         CALL ISTAB ('FQ', INDISK, CNO, FQVER, LUN, FQBUFF, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) THEN
            CALL FQINI ('READ', FQBUFF, INDISK, CNO, FQVER, CATBLK,
     *         LUN, IFQRNO, FQKOLS, FQNUMV, NIF, JERR)
            IF (JERR.NE.0) GO TO 999
            NFRQ = FQBUFF(5)
            IF (NFRQ.GT.1) THEN
               WRITE (MSGTXT,1030) NFRQ
               CALL MSGWRT (3)
               END IF
            CALL TABIO ('CLOS', 0, IFQRNO, FQBUFF, FQBUFF, JERR)
            IF (JERR.NE.0) GO TO 999
            END IF
         END IF
C                                       Find specified FQ id
      CALL FQMATC (INDISK, CNO, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
      DOACOR = .FALSE.
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
C                                       histogram type
      ATYPE(1) = IROUND (BPARM(1))
      ATYPE(2) = IROUND (BPARM(2))
      IF ((ATYPE(1).LT.1) .OR. (ATYPE(1).GT.12) .OR. (ATYPE(2).LT.1)
     *   .OR. (ATYPE(2).GT.12) .OR. (ATYPE(1).EQ.ATYPE(2))) THEN
         ATYPE(1) = 1
         ATYPE(2) = 2
         END IF
C                                       Hermitian??
      IF (ATYPE(1).EQ.12) THEN
         HERMIT = ATYPE(2).EQ.9
         ATYPE(1) = 10
      ELSE IF (ATYPE(2).EQ.12) THEN
         HERMIT = ATYPE(1).EQ.9
         ATYPE(2) = 10
         END IF
      CSIZE = 100.0 * BPARM(8) + 0.5
      CSIZE = (CSIZE/2) * 2 + 1
      CTYPE = IROUND (BPARM(9))
      IF (CSIZE.EQ.1) CTYPE = -1
C                                       build image header
      CALL CATINI (CATBLK)
      CALL RCOPY (2, CATUH(KHOBJ), CATH(KHOBJ))
      CALL RCOPY (2, CATUH(KHTEL), CATH(KHTEL))
      CALL RCOPY (2, CATUH(KHINS), CATH(KHINS))
      CALL RCOPY (2, CATUH(KHOBS), CATH(KHOBS))
      CALL RCOPY (2, CATUH(KHDOB), CATH(KHDOB))
      CATBLK(KIDIM) = 5
      CATBLK(KINAX) = IMSIZE(1)
      CATBLK(KINAX+1) = IMSIZE(2)
      CATR(KREPO) = CATUR(KREPO)
C                                       Build new file cat name.
      CALL MAKOUT (INNAME, INCLAS, INSEQ, CBLANK, OUNAME, OUCLAS, SEQO)
      CALL CHR2H (12, OUNAME, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, OUCLAS, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQO
C                                       Create new cataloged file.
      CALL MCREAT (DISKO, CNOO, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 990
         END IF
      SEQO = CATBLK(KIIMS)
      CALL COPY (256, CATBLK, MAPHDR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CNOO
      FRW(NCFILE) = 2
C                                       copy some (all) keywords
      CALL KEYPCP (INDISK, CNO, DISKO, CNOO, 0, ' ', IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('UVHGIN: ERROR GETTING ADVERB VALUES.')
 1020 FORMAT ('UVHGIN: FREQUENCY CHANNEL',I4,' EXCEEDS LIMIT',I4)
 1025 FORMAT ('UVHGIN: IF BAND',I4,' EXCEEDS LIMIT',I4)
 1030 FORMAT ('Plotting',I4,' frequency IDs.')
 1100 FORMAT ('UVHGIN: MCREAT ERROR',I5)
      END
      SUBROUTINE BLDHGM (NX, NY, IMG, CS, CF, IERR)
C-----------------------------------------------------------------------
C   BLDHGM does two passes through the uv data file.  Firstly to get
C   the maxima and minima of the various parameters, and second to
C   construct the histogram image
C   Inputs:
C      NX     I          X pixels in image
C      NY     I          Y pixels in image
C      CS     I          size convolving function
C      CF     R(CS,CS)   convolving function
C   Outputs:
C      IMG    R(NX,NY)   histogram image
C      IERR   I          error code from UV disk IO mostly
C-----------------------------------------------------------------------
      INTEGER   NX, NY, CS, IERR
      REAL      IMG(NX,NY), CF(CS,CS)
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      LOGICAL   DOTHIS, REQBAS
      INTEGER   ADDRES, IBIN(2), IROUND, K, IIF, ICHAN, ISTK1, ISTK2,
     *   ISTK, JSUB, IFRQ, ISUB, NIF, NXVER, NXLUN, NANT, IANT(50),
     *   NBAS, IBAS(50), DESEL, I, J, IHERM, NHERM
      REAL      AMPMAX, AMPSQ, ASQMAX, IM, R2D, RE, RSQ, TMAX, TMIN, U,
     *   V, VAR, W, WGT, WGTMAX, UVMAX, RSQMAX, VIS(UVBFSS), RPARM(20),
     *   CATUVR(256), RBIN(2)
      DOUBLE PRECISION FI, FZ, FRQMUL
      INCLUDE 'UVHIM.INC'
      INCLUDE 'INCS:DSEL.INC'
      DOUBLE PRECISION FOFF(MAXIF)
      REAL      FINC(MAXIF)
      INTEGER   ISBAND(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATUV, CATUVR)
      PARAMETER (R2D = 180.0/3.14159265358)
C-----------------------------------------------------------------------
      NHERM = 1
      IF (HERMIT) NHERM = 2
      JSUB = SUBARR
      NXVER = 1
      NXLUN = 90
C                                       Initialize baseline selection.
      CALL SETANT (50, XANT, XBASE, NANT, NBAS, IANT, IBAS, DESEL)
C                                       init maxima
      RSQMAX =  0.0
      TMIN   =  1E30
      TMAX   = -1E30
      ASQMAX =  0.0
      WGTMAX =  0.0
      ISTK1 = 1
      ISTK2 = 0
C                                       range set by user
      IF ((BPARM(3).GT.0.0) .AND. (BPARM(4).LT.BPARM(5)) .AND.
     *   (BPARM(6).LT.BPARM(7)))  THEN
         DOMAIN(1,1) = BPARM(4)
         DOMAIN(2,1) = BPARM(5)
         DOMAIN(1,2) = BPARM(6)
         DOMAIN(2,2) = BPARM(7)
C                                       range set by data
      ELSE
         MSGTXT = 'Begin determination of scaling'
         CALL MSGWRT (1)
         DO 70 IFRQ = 1,NFRQ
            IF (NFRQ.GT.1) FRQSEL = IFRQ
            CALL CHNDAT ('READ', NXBUFF, INDISK, CNO, NXVER, CATUV,
     *         NXLUN, NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'PROBLEM FINDING FREQUENCIES'
               CALL MSGWRT (6)
               GO TO 70
               END IF
            DO 60 ISUB = 1,NSUBA
               IF (JSUB.EQ.0) SUBARR = ISUB
C                                       Init vis file for read.
               CALL UVGET ('INIT', RPARM, VIS, IERR)
C
               IF (IERR.EQ.-1) GO TO 50
               IF (IERR.EQ.5) GO TO 50
               IF (IERR.GT.0) GO TO 999
               IF (ISTK2.LE.0) ISTK2 = CATBLK(KINAX+JLOCS)

C                                       Loop Read vis. record.
 10            CALL UVGET ('READ', RPARM, VIS, IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1010) IERR
                  GO TO 990
C                                       a data record
               ELSE IF (IERR.EQ.0) THEN
C                                       Do we need this baseline?
                  IF (ILOCB.GE.0) THEN
                     I = INT (RPARM(ILOCB+1)) / 256
                     J = MOD (INT (RPARM(ILOCB+1)), 256)
                  ELSE
                     I = RPARM(ILOCA1+1) + 0.1
                     J = RPARM(ILOCA2+1) + 0.1
                     END IF
                  IF (.NOT.REQBAS (I, J, DESEL, IANT, NANT, IBAS, NBAS))
     *               GO TO 10
                  DOTHIS = .FALSE.
                  DO 40 ICHAN = BCHAN,ECHAN,CHINC
                     DO 30 IIF = BIF,EIF
                        FZ = FOFF(IIF) / UVFREQ + 1.0D0
                        FI = FINC(IIF) / UVFREQ
                        FRQMUL = 1.0D0
                        IF (TYPUVD.LE.0) FRQMUL = FZ + FI *
     *                    (ICHAN - 1 + BCHAN - CATUVR(KRCRP+KLOCFY))
                        FRQMUL = FRQMUL ** 2
                        DO 20 ISTK = ISTK1,ISTK2
                           ADDRES = 1 + (ICHAN-BCHAN) * INCF +
     *                        (IIF-BIF) * INCIF + (ISTK-ISTK1) * INCS
                           WGT    = VIS(ADDRES+2)
C                                       Get maxima and minima.
                           IF (WGT.GT.0.0) THEN
                              DOTHIS = .TRUE.
                              RE = VIS(ADDRES)
                              IM = VIS(ADDRES+1)
                              AMPSQ  = RE**2 + IM**2
                              ASQMAX = MAX (ASQMAX, AMPSQ)
                              WGTMAX = MAX (WGTMAX, WGT)
                              END IF
 20                        CONTINUE
 30                     CONTINUE
 40                  CONTINUE
C                                       Get maxima and minima.
                  IF (DOTHIS) THEN
                     RSQ    = (RPARM(1+ILOCU)**2 + RPARM(1+ILOCV)**2)
     *                  * FRQMUL
                     RSQMAX = MAX (RSQ, RSQMAX)
                     TMIN   = MIN (TMIN, RPARM(1+ILOCT))
                     TMAX   = MAX (TMAX, RPARM(1+ILOCT))
                     END IF
                  GO TO 10
                  END IF
 50            CALL UVGET ('CLOS', RPARM, VIS, IERR)
 60            CONTINUE
 70         CONTINUE
C
         AMPMAX = SQRT (ASQMAX)
         UVMAX = SQRT (RSQMAX)
         DO 75 I = 1,2
            IF (ATYPE(I).LE.2) THEN
               DOMAIN(1,I) = -AMPMAX
               DOMAIN(2,I) = AMPMAX
            ELSE IF (ATYPE(I).EQ.3) THEN
               DOMAIN(1,I) = 0.0
               DOMAIN(2,I) = AMPMAX
            ELSE IF (ATYPE(I).EQ.4) THEN
               DOMAIN(1,I) = -180.0
               DOMAIN(2,I) = 180.0
            ELSE IF (ATYPE(I).EQ.5) THEN
               DOMAIN(1,I) = 0.0
               DOMAIN(2,I) = WGTMAX
            ELSE IF (ATYPE(I).EQ.6) THEN
               DOMAIN(1,I) = TMIN
               DOMAIN(2,I) = TMAX
            ELSE IF (ATYPE(I).EQ.7) THEN
               DOMAIN(1,I) = 0.0
               DOMAIN(2,I) = UVMAX
            ELSE IF (ATYPE(I).EQ.8) THEN
               DOMAIN(1,I) = -180.0
               DOMAIN(2,I) = 180.0
            ELSE IF (ATYPE(I).GE.9) THEN
               DOMAIN(1,I) = -UVMAX
               DOMAIN(2,I) = UVMAX
               END IF
            IF (BPARM(3).GT.0.0) THEN
               IF (BPARM(2+2*I).LT.BPARM(3+2*I)) THEN
                  DOMAIN(1,I) = BPARM(2+2*I)
                  DOMAIN(2,I) = BPARM(3+2*I)
               ELSE IF (BPARM(2+2*I).GT.BPARM(3+2*I)) THEN
                  DOMAIN(1,I) = MAX (BPARM(3+2*I), DOMAIN(1,I))
                  DOMAIN(2,I) = MIN (BPARM(2+2*I), DOMAIN(2,I))
                  END IF
               END IF
 75         CONTINUE
         END IF
C                                       Second pass:  Accumulate data
C                                       for the histograms.
C                                       Clear the histogram storage
C                                       array.
      MSGTXT = 'Begin binning of the data'
      CALL MSGWRT (1)
      I = NX * NY
      CALL RFILL (I, 0.0, IMG)
      NOUT = 0
      NIN = 0
C                                       over FQID
      DO 170 IFRQ = 1,NFRQ
         IF (NFRQ.GT.1) FRQSEL = IFRQ
         CALL CHNDAT ('READ', NXBUFF, INDISK, CNO, NXVER, CATUV,
     *      NXLUN, NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
         IF (IERR.NE.0) GO TO 170
         DO 160 ISUB = 1,NSUBA
            IF (JSUB.EQ.0) SUBARR = ISUB
C                                       Init vis file for read.
            CALL UVGET ('INIT', RPARM, VIS, IERR)
C
            IF (IERR.EQ.-1) GO TO 155
            IF (IERR.EQ.5) GO TO 155
            IF (IERR.GT.0) GO TO 999
            IF (ISTK2.LE.0) ISTK2 = CATBLK(KINAX+JLOCS)
C                                       Loop Read vis. record.
 110        CALL UVGET ('READ', RPARM, VIS, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1010) IERR, IFRQ, ISUB
               GO TO 990
C                                       a data record
            ELSE IF (IERR.EQ.0) THEN
C                                       Do we need this baseline?
               IF (ILOCB.GE.0) THEN
                  I = INT (RPARM(ILOCB+1)) / 256
                  J = MOD (INT (RPARM(ILOCB+1)), 256)
               ELSE
                  I = RPARM(ILOCA1+1) + 0.1
                  J = RPARM(ILOCA2+1) + 0.1
                  END IF
               IF (.NOT.REQBAS (I, J, DESEL, IANT, NANT, IBAS, NBAS))
     *            GO TO 110
C                                       Get visibilities out of uvbuff.
               DO 150 ICHAN = BCHAN,ECHAN,CHINC
                  DO 140 IIF = BIF,EIF
                     FZ = FOFF(IIF) / UVFREQ + 1.0D0
                     FI = FINC(IIF) / UVFREQ
                     FRQMUL = 1.0D0
                     IF (TYPUVD.LE.0) FRQMUL = FZ + FI *
     *                  (ICHAN - 1 + BCHAN - CATUVR(KRCRP+KLOCFY))
C                                       Get (u,v,w) out of uvbuff.
                     U = RPARM(1+ILOCU) * FRQMUL
                     V = RPARM(1+ILOCV) * FRQMUL
                     W = RPARM(1+ILOCW) * FRQMUL
                     DO 130 ISTK = ISTK1,ISTK2
                        ADDRES = 1 + (ICHAN-BCHAN) * INCF +
     *                     (IIF-BIF) * INCIF + (ISTK-ISTK1) * INCS
                        RE  = VIS(ADDRES)
                        IM  = VIS(ADDRES+1)
                        WGT = VIS(ADDRES+2)
                        IF (WGT.GT.0.0) THEN
                           DO 125 IHERM = 1,NHERM
                           DO 120 I = 1,2
                              VAR = 0.0
                              K = ATYPE(I)
C                                       Identify the variable.
                              IF (K.EQ.9) THEN
                                 VAR = U
                              ELSE IF (K.EQ.10) THEN
                                 VAR = V
                              ELSE IF (K.EQ.11) THEN
                                 VAR = W
                              ELSE IF (K.EQ.7) THEN
                                 VAR = SQRT (U*U + V*V)
                              ELSE IF (K.EQ.8) THEN
                                 IF ((U.NE.0.0) .OR. (V.NE.0.0))
     *                              VAR = ATAN2 (V, U) * R2D
                              ELSE IF (K.EQ.6) THEN
                                 VAR = RPARM(1+ILOCT)
                              ELSE IF (K.EQ.1) THEN
                                 VAR = RE
                              ELSE IF (K.EQ.2) THEN
                                 VAR = IM
                              ELSE IF (K.EQ.3) THEN
                                 VAR = SQRT (RE*RE + IM*IM)
                              ELSE IF (K.EQ.4) THEN
                                 IF ((RE.NE.0.0) .OR. (IM.NE.0.0))
     *                              VAR = ATAN2 (IM, RE) * R2D
                              ELSE IF (K.EQ.5) THEN
                                 VAR = WGT
                                 END IF
C                                       Calculate the bin.
                              RBIN(I) = (IMSIZE(I)-1.0) *
     *                           (VAR-DOMAIN(1,I)) /
     *                           (DOMAIN(2,I)-DOMAIN(1,I)) + 1.0
                              IBIN(I) = IROUND (RBIN(I))
 120                          CONTINUE
C                                       Check for under- or overflow.
                           IF ((IBIN(1).LT.1) .OR.(IBIN(2).LT.1) .OR.
     *                        (IBIN(1).GT.IMSIZE(1)) .OR.
     *                        (IBIN(2).GT.IMSIZE(2))) THEN
                              NOUT = NOUT + 1
C                                       grid the sample
                           ELSE IF (CS.EQ.1) THEN
                              NIN = NIN + 1
                              IMG(IBIN(1),IBIN(2)) =
     *                           IMG(IBIN(1),IBIN(2)) + 1
                           ELSE
                              NIN = NIN + 1
                              CALL GRIDIT (NX, NY, IMG, CS, CF, RBIN,
     *                           IBIN)
                              END IF
                           U = -U
                           V = -V
 125                       CONTINUE
                           END IF
 130                    CONTINUE
 140                 CONTINUE
 150              CONTINUE
               GO TO 110
               END IF
 155        CALL UVGET ('CLOS', RPARM, VIS, IERR)
 160        CONTINUE
 170     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR INITING FQ',I3,' SUBARRAY',I3)
      END
      SUBROUTINE BLDCNV (CS, CT, CF)
C-----------------------------------------------------------------------
C   Build and normalizes the convolving function
C   Inputs:
C      CS   I          Size in pixels of convolving function
C      CT   I          Type of convolving function
C   Outputs:
C      CF   R(CS,CS)   Convolving function
C-----------------------------------------------------------------------
      INTEGER   CS, CT
      REAL      CF(CS,CS)
C
      INTEGER   I, J, IC, IA, IB, I1, I2, J1, J2, II, JJ, IR
      REAL      X, Y, R, SUP, YF, SCALE
C-----------------------------------------------------------------------
      IC = (CS + 1) / 2
      IF ((CT.LT.-4) .OR. (CT.GT.4) .OR. (CT.EQ.0)) CT = -1
C                                       initial function computation
      SCALE = 1.0 / MAX (1.0, IC-1.0)
      SUP = (IC-1) * (IC-1)
C                                       rectangles : pill box
      IF (CT.EQ.-1) THEN
         I = CS * CS
         CALL RFILL (I, 1.0, CF)
C                                       linear
      ELSE IF (CT.EQ.-2) THEN
         DO 25 J = 1,CS
            Y = ABS (J-IC)
            YF = MAX (0.0, (1.0 - Y*SCALE))
            DO 20 I = 1,CS
               X = ABS (I-IC)
               CF(I,J) = YF * MAX (0.0, (1.0 - X*SCALE))
 20            CONTINUE
 25         CONTINUE
C                                       exponential
      ELSE IF (CT.EQ.-3) THEN
         DO 35 J = 1,CS
            Y = ABS (J-IC) * SCALE
            YF = EXP ( - 2.0 * Y)
            DO 30 I = 1,CS
               X = ABS (I-IC) * SCALE
               CF(I,J) = YF * EXP (-2.0 * X)
 30            CONTINUE
 35         CONTINUE
C                                       gaussian
      ELSE IF (CT.EQ.-4) THEN
         DO 45 J = 1,CS
            Y = ABS (J-IC) * SCALE
            YF = EXP ( - 4.0 * Y * Y)
            DO 40 I = 1,CS
               X = ABS (I-IC) * SCALE
               CF(I,J) = YF * EXP (-4.0 * X * X)
 40            CONTINUE
 45         CONTINUE
C                                       circular: pill box
      ELSE IF (CT.EQ.1) THEN
         DO 115 J = 1,CS
            Y = (J-IC)*(J-IC)
            DO 110 I = 1,CS
               X = I-IC
               R = X*X + Y
               IF (R.LE.SUP) THEN
                  CF(I,J) = 1.0
               ELSE
                  CF(I,J) = 0.0
                  END IF
 110           CONTINUE
 115        CONTINUE
C                                       circular: linear
      ELSE IF (CT.EQ.2) THEN
         DO 125 J = 1,CS
            Y = (J-IC)*(J-IC)
            DO 120 I = 1,CS
               X = I-IC
               R = X*X + Y
               IF (R.LE.SUP) THEN
                  R = SQRT (R) * SCALE
                  CF(I,J) = MAX (0.0, (1.0 - R))
               ELSE
                  CF(I,J) = 0.0
                  END IF
 120           CONTINUE
 125        CONTINUE
C                                       circular: exponential
      ELSE IF (CT.EQ.3) THEN
         DO 135 J = 1,CS
            Y = (J-IC)*(J-IC)
            DO 130 I = 1,CS
               X = I-IC
               R = X*X + Y
               IF (R.LE.SUP) THEN
                  R = SQRT (R) * SCALE
                  CF(I,J) = 1.0 - EXP (-2.0 * R)
               ELSE
                  CF(I,J) = 0.0
                  END IF
 130           CONTINUE
 135        CONTINUE
C                                       circular: gaussian
      ELSE IF (CT.EQ.4) THEN
         DO 145 J = 1,CS
            Y = (J-IC)*(J-IC)
            DO 140 I = 1,CS
               X = I-IC
               R = X*X + Y
               IF (R.LE.SUP) THEN
                  R = R * SCALE * SCALE
                  CF(I,J) = 1.0 - EXP (-4.0 * R)
               ELSE
                  CF(I,J) = 0.0
                  END IF
 140           CONTINUE
 145        CONTINUE
         END IF
C                                       Now normalize so that each
C                                       convolution sums to 1.0
      IA = IC - 25
      IB = IC + 24
      IR = (CS -1) / 100 + 1
      DO 290 J = IA,IB
         J1 = J - 50 * IR
         IF (J1.LT.1) J1 = J1 + 50
         IF (J1.LT.1) J1 = J1 + 50
         IF (J1.LT.1) J1 = J1 + 50
         IF (J1.LT.1) J1 = J1 + 50
         J2 = J + 50 * IR
         IF (J2.GT.CS) J2 = J2 - 50
         IF (J2.GT.CS) J2 = J2 - 50
         IF (J2.GT.CS) J2 = J2 - 50
         IF (J2.GT.CS) J2 = J2 - 50
         DO 280 I = IA,IB
            I1 = I - 50 * IR
            IF (I1.LT.1) I1 = I1 + 50
            IF (I1.LT.1) I1 = I1 + 50
            IF (I1.LT.1) I1 = I1 + 50
            IF (I1.LT.1) I1 = I1 + 50
            I2 = I + 50 * IR
            IF (I2.GT.CS) I2 = I2 - 50
            IF (I2.GT.CS) I2 = I2 - 50
            IF (I2.GT.CS) I2 = I2 - 50
            IF (I2.GT.CS) I2 = I2 - 50
            SUP = 0.0
            DO 220 JJ = J1,J2,50
               DO 210 II = I1,I2,50
                  SUP = SUP + CF(II,JJ)
 210              CONTINUE
 220           CONTINUE
            IF (SUP.GT.0.0) THEN
               DO 240 JJ = J1,J2,50
                  DO 230 II = I1,I2,50
                     CF(II,JJ) = CF(II,JJ) / SUP
 230                 CONTINUE
 240              CONTINUE
               END IF
 280        CONTINUE
 290     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GRIDIT (NX, NY, IMG, CS, CF, RBIN, IBIN)
C-----------------------------------------------------------------------
C   grids a sample to the image
C   Inputs:
C      NX     I          X size of image
C      NY     I          Y size of image
C      CS     I          Size of convolving function
C      CF     R(CS,CS)   Convolving function
C      RBIN   R(2)       Bin location in image pixels
C      IBIN   I(2)       Integerized bin location
C   In/Out
C      IMG    R(NX,NY)   Image
C-----------------------------------------------------------------------
      INTEGER   NX, NY, CS, IBIN(2)
      REAL      IMG(NX,NY), CF(CS,CS), RBIN(2)
C
      INTEGER   IC, ICX, ICY, IROUND, IR, I, I1, I2, J, J1, J2, II, JJ
C-----------------------------------------------------------------------
      IC = (CS + 1) / 2
      ICX = IROUND ((RBIN(1) - IBIN(1)) * 50.0)
      ICY = IROUND ((RBIN(2) - IBIN(2)) * 50.0)
      ICX = IC - ICX
      ICY = IC - ICY
      IR = (CS - 1) / 100 + 1
      J1 = IBIN(2) - IR
      J2 = IBIN(2) + IR
      I1 = IBIN(1) - IR
      I2 = IBIN(1) + IR
      DO 50 J = J1,J2
         JJ = ICY + (J-IBIN(2)) * 50
         IF ((JJ.GE.1) .AND. (JJ.LE.CS)) THEN
            DO 40 I = I1,I2
               II = ICX + (I-IBIN(1)) * 50
               IF ((II.GE.1) .AND. (II.LE.CS))
     *            IMG(I,J) = IMG(I,J) + CF(II,JJ)
 40            CONTINUE
            END IF
 50      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE WRIHGM (NX, NY, IMG, IERR)
C-----------------------------------------------------------------------
C   WRIHGM completes the image header, writes the image data, does the
C   HI file
C   Inputs:
C      NX     I          X size of image
C      NY     I          Y size of image
C      IMG    R(NX,NY)   Image
C   Output:
C      IERR   I          Error code
C   CATBLK comes in pointing at UV data, MAPHDR at image header
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IERR
      REAL      IMG(NX,NY)
C
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'UVHIM.INC'
      INTEGER   I, J, OLUN, OIND, OWIN(4), IBLKOF, NBY, OBIND, IH1LUN,
     *   IH2LUN, I1, I2, NFILES
      CHARACTER TYPE(11)*8, UNITS(11)*11, MTYPE*2, HILINE*72
      REAL      CATR(256), BUFF(MABFSS), RMAX, RMIN, RSUM
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA TYPE /'VIS (RE)', 'VIS (IM)', 'VIS (AM)', 'VIS (PH)',
     *   'WEIGHT', 'TIME', 'BASELENG', 'BASEL PA', 'U', 'V', 'W'/
      DATA UNITS /'Jy', 'Jy', 'Jy', 'Degrees', '1/(Jy**2)', 'Days',
     *   'Wavelengths', 'Degrees', 3*'Wavelengths'/
C-----------------------------------------------------------------------
C                                       work on header
      CALL CHR2H (8, 'COUNT   ', 1, MAPH(KHBUN))
      MAPD(KDORA) = CATD(KDORA)
      MAPD(KDODE) = CATD(KDODE)
      MAPD(KDRST) = CATD(KDRST)
      CALL CHR2H (8, TYPE(ATYPE(1)), 1, MAPH(KHCTP))
      CALL CHR2H (8, TYPE(ATYPE(2)), 1, MAPH(KHCTP+2))
      MAPD(KDCRV) = DOMAIN(1,1)
      MAPD(KDCRV+1) = DOMAIN(1,2)
      MAPR(KRCRP) = 1.0
      MAPR(KRCRP+1) = 1.0
      MAPR(KRCIC) = (DOMAIN(2,1)-DOMAIN(1,1)) / (IMSIZE(1)-1.0)
      MAPR(KRCIC+1) = (DOMAIN(2,2)-DOMAIN(1,2)) / (IMSIZE(2)-1.0)
      I = 1
      IF (JLOCF.GE.0) THEN
         I = I + 1
         MAPH(KHCTP+2*I) = CATH(KHCTP+2*JLOCF)
         MAPH(KHCTP+2*I+1) = CATH(KHCTP+2*JLOCF+1)
         MAPD(KDCRV+I) = CATD(KDCRV+JLOCF) + (1.-CATR(KRCRP+JLOCF)) *
     *      CATR(KRCIC+JLOCF)
         MAPR(KRCIC+I) = CATR(KRCIC+JLOCF)
         MAPR(KRCRP+I) = 1.0
         END IF
      IF (JLOCR.GE.0) THEN
         I = I + 1
         MAPH(KHCTP+2*I) = CATH(KHCTP+2*JLOCR)
         MAPH(KHCTP+2*I+1) = CATH(KHCTP+2*JLOCR+1)
         MAPD(KDCRV+I) = CATD(KDCRV+JLOCR)
         MAPR(KRCIC+I) = CATR(KRCIC+JLOCR)
         MAPR(KRCRP+I) = 1.0
         END IF
      IF (JLOCD.GE.0) THEN
         I = I + 1
         MAPH(KHCTP+2*I) = CATH(KHCTP+2*JLOCD)
         MAPH(KHCTP+2*I+1) = CATH(KHCTP+2*JLOCD+1)
         MAPD(KDCRV+I) = CATD(KDCRV+JLOCD)
         MAPR(KRCIC+I) = CATR(KRCIC+JLOCD)
         MAPR(KRCRP+I) = 1.0
         END IF
      MAPHDR(KIDIM) = I + 1
      RMAX = -1.E10
      RMIN = -RMAX
      RSUM = 0.0
      DO 20 J = 1,NY
         DO 10 I = 1,NX
            IF ((DOBLNK.GT.0.0) .AND. (IMG(I,J).LE.0.0)) THEN
               IMG(I,J) = FBLANK
            ELSE
               RMAX = MAX (RMAX, IMG(I,J))
               RMIN = MIN (RMIN, IMG(I,J))
               RSUM = RSUM + IMG(I,J)
               END IF
 10         CONTINUE
 20      CONTINUE
      MAPR(KRDMX) = RMAX
      MAPR(KRDMN) = RMIN
      IF (DOBLNK.GT.0.0) MAPR(KRBLK) = FBLANK
C                                       force header update
      CALL CATIO ('UPDT', DISKO, CNOO, MAPHDR, 'REST', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CATIO UPDATE'
         CALL MSGWRT (7)
         END IF
C                                       open image output
      OLUN = 49
      MTYPE = 'MA'
      CALL MAPOPN ('INIT', DISKO, OUNAME, OUCLAS, SEQO, MTYPE, NLUSER,
     *   OLUN, OIND, CNOO, MAPHDR, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN'
         GO TO 990
         END IF
      OWIN(1) = 1
      OWIN(2) = 1
      OWIN(3) = NX
      OWIN(4) = NY
      IBLKOF = 1
      NBY = 2 * MABFSS
      CALL MINIT ('WRIT', OLUN, OIND, NX, NY, OWIN, BUFF, NBY, IBLKOF,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'MINIT'
         GO TO 990
         END IF
      DO 40 J = 1,NY
         CALL MDISK ('WRIT', OLUN, OIND, BUFF, OBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITE'
            GO TO 990
            END IF
         CALL RCOPY (NX, IMG(1,J), BUFF(OBIND))
 40      CONTINUE
      CALL MDISK ('FINI', OLUN, OIND, BUFF, OBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'FINISH'
         GO TO 990
         END IF
C                                       Initialize HITAB
      NFILES = 3
      CALL HIINIT (NFILES)
C                                       Create and copy history file.
      IH1LUN = 61
      IH2LUN = 62
      CALL HISCOP (IH1LUN, IH2LUN, INDISK, DISKO, CNO, CNOO,
     *   MAPHDR, BUFF, SCRTCH, IERR)
      IF (IERR.GT.3) GO TO 300
      IF (IERR.EQ.3) GO TO 200
C                                       add UVHIM history
      CALL HENCO1 (TSKNAM, INNAME, INCLAS, INSEQ, INDISK, IH2LUN,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, OUNAME, OUCLAS, SEQO, DISKO, IH2LUN,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       calibration adverbs
C                                       TIMERANG
      CALL HITIME (TSTART, TEND, IH2LUN, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Stokes'
      WRITE (HILINE,1100) TSKNAM, XSTOK
      CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       IF range
      WRITE (HILINE,1110) TSKNAM, BIF, EIF
      CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Chan range
      WRITE (HILINE,1120) TSKNAM, BCHAN, ECHAN, CHINC
      CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Subarray
      IF (NSUBA.LE.1) THEN
         WRITE (HILINE,1130) TSKNAM, SUBARR
      ELSE
         WRITE (HILINE,1131) TSKNAM, NSUBA
         END IF
      CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Flagging
      IF (FGVER.GT.0) THEN
         WRITE (HILINE,1140) TSKNAM, FGVER
         CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                        Spectral smoothing
      IF (SMOOTH(1).GT.0.5) THEN
         I1 = SMOOTH(1) + 0.5
         I2 = SMOOTH(3) + 0.5
         WRITE (HILINE,1150) TSKNAM, I1, SMOOTH(2), I2
         CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Calibration
      IF (DOCAL) THEN
         WRITE (HILINE,1160) TSKNAM, CLUSE
         CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Polzn correction
      IF (DOPOL.GT.0) THEN
         WRITE (HILINE,1170) TSKNAM, DOPOL
         CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       BL table
      IF (XBLVER.GE.0.0) THEN
         WRITE (HILINE,1180) TSKNAM, BLVER
         CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       BP table
      IF (XDOBND.GT.0.0) THEN
         WRITE (HILINE,1190) TSKNAM, DOBAND
         CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         WRITE (HILINE,1200) TSKNAM, BPVER
         CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Calibrate weights?
      IF (DOCAL) THEN
         IF (DOWTCL) THEN
            WRITE (HILINE,1210) TSKNAM
         ELSE
            WRITE (HILINE,1211) TSKNAM
            END IF
         CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                      Add any other history
      WRITE (HILINE,1220) TSKNAM, 'X', TYPE(ATYPE(1)), UNITS(ATYPE(1))
      CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
      MSGTXT = HILINE(8:)
      CALL MSGWRT (2)
      WRITE (HILINE,1221) TSKNAM, 'X', DOMAIN(1,1), DOMAIN(2,1)
      CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
      MSGTXT = HILINE(8:)
      CALL MSGWRT (2)
      WRITE (HILINE,1220) TSKNAM, 'Y', TYPE(ATYPE(2)), UNITS(ATYPE(2))
      CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
      MSGTXT = HILINE(8:)
      CALL MSGWRT (2)
      WRITE (HILINE,1221) TSKNAM, 'Y', DOMAIN(1,2), DOMAIN(2,2)
      CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
      MSGTXT = HILINE(8:)
      CALL MSGWRT (2)
      WRITE (HILINE,1225) TSKNAM, NIN, RSUM
      CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
      MSGTXT = HILINE(8:)
      CALL MSGWRT (2)
      IF (NOUT.GT.0) THEN
         WRITE (HILINE,1226) TSKNAM, NOUT
         CALL HIADD (IH2LUN, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         MSGTXT = HILINE(8:)
         CALL MSGWRT (2)
         END IF
C
 200  CALL HICLOS (IH2LUN, .TRUE., SCRTCH, IERR)
C                                       close image
 300  CALL MAPCLS ('INIT', DISKO, CNOO, OLUN, OIND, MAPHDR, .TRUE.,
     *   SCRTCH, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WRIHGM: ERROR',I5,' DOING ',A)
 1100 FORMAT (A6,'STOKES = ''',A4,''' / Stokes type')
 1110 FORMAT (A6,'BIF =',I4,', EIF =',I4,'/ IF range')
 1120 FORMAT (A6,'BCHAN=',I5,', ECHAN=',I5,', CHINC=',I3,
     *   '   / Chan range')
 1130 FORMAT (A6,'SUBARRAY =',I4)
 1131 FORMAT (A6,'NSUBA =',I4,'   / multiple subarrays included')
 1140 FORMAT (A6,'/ Edited using FG table version',I3)
 1150 FORMAT (A6,'SMOOTH = ',I1,',',F6.1,',',I4,
     *   ' / Spectral smoothing parms')
 1160 FORMAT (A6,'GAINUSE =',I3,' / CL table')
 1170 FORMAT (A6,'DOPOL = ',I2,'  / polarization correction made')
 1180 FORMAT (A6,'BL table ',I3,' / applied to data')
 1190 FORMAT (A6,'/ BP correction done, DOBAND = ',I2)
 1200 FORMAT (A6,'/ BP correction used BP table ',I2)
 1210 FORMAT (A6,'/ Weights calibrated')
 1211 FORMAT (A6,'/ Weights not calibrated')
 1220 FORMAT (A6,'/ ',A,'-axis type  ',A,'   units  ',A)
 1221 FORMAT (A6,'/ ',A,'-axis range',2(1PE13.5))
 1225 FORMAT (A6,'/ Pixels in image',I10,' pix sum',F13.2)
 1226 FORMAT (A6,'/ Values outside image',I10)
      END
