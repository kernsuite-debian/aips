LOCAL INCLUDE 'UVLIN.INC'
C                                               Local include for UVLIN
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XINFIL(12),
     *   XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH(3), XDOAC, XSOUT, XDISO, DOCONT,
     *   RMS, SHIFT(2), XCHNS(4,20), XORD, PRTLEV, XCENT, BADD(10),
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), FINC(MAXIF), DIFPIX
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT, LBIF,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI,
     *   LRECO, NRPRMI, NRPRMO, FITWTS(MAXCIF), NBDCOR, NORDER, NCHAN,
     *   NIF, NPOLN, NANT, ISBAND(MAXIF), NEWCNO, OLDCNO
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, INFILE*48, NAMOUT*12, CLAOUT*6
      DOUBLE PRECISION FOFF(MAXIF)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XDOAC,
     *   XINFIL, XNAMOU, XCLAOU, XSOUT, XDISO, SHIFT, RMS, DOCONT,
     *   XCHNS, XORD, PRTLEV, XCENT, BADD
      COMMON /CHARPM/ NAMEIN, CLAIN, INFILE, NAMOUT, CLAOUT
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /INFO/ CATOLD, FOFF, ISBAND, FINC, LBIF, SEQIN, SEQOUT,
     *   DISKIN, DISKO, ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO,
     *   INCIFO, LRECI, LRECO, NRPRMI, NRPRMO, ISCOMP, NBDCOR, NORDER,
     *   NCHAN, NIF, NPOLN, NANT, NEWCNO, OLDCNO, DIFPIX
      COMMON /WEIGHT/ FITWTS
LOCAL END
      PROGRAM UVLIN
C-----------------------------------------------------------------------
C! Fits and optionally removes continuum contribution to the visibility
C# Utility UV UV-util VLA VLB SPECTRAL
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2000, 2005, 2008-2012, 2014-2016,
C;  Copyright (C) 2018
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
C   UVLIN averages a set of channels and subtracts them from another
C   range of channels.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      INFILE         INFILE        Disk file containing channel weights
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      SHIFT          SHIFT         shift in arcsec
C      DOCONT         DOCONT        Retain continuum if > 0
C      FLUX           RMS           Max deviation for unity weight
C      NBOX           XNB           Number of boxes
C      BOX            XBOXES        Boxes (start-stop pairs)
C      PRTLEV         PRTLEV        > 0 => write flagging info to the
C                                   message file
C   Programmers: J.M. Uson and T.J. Cornwell (with help from R. Braun).
C      J. M. Uson (7/1/93) (cleanup, report info for VLA)
C      EWG overhauled all the vla-specific junk
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INCLUDE 'UVLIN.INC'
      INTEGER   MALL(2), MBAD(2), MCHAN(2), NWORDS, IRET, I
      LONGINT   OFFALL, OFFBAD, OFFCHN
C     INTEGER   MALL(MAXANT,MAXANT,4*MAXIF), MCHAN(4*MAXCIF),
C    *   MBAD(MAXANT,MAXANT,4*MAXIF)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'UVLIN '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVLIIN (PRGM, IRET)
C                                       Allocate memory
      NWORDS = (NANT * NANT * NPOLN * NIF - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MALL, OFFALL,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MBAD, OFFBAD,
     *   IRET)
      IF (IRET.EQ.0) THEN
         NWORDS = 1024 * NWORDS
         CALL FILL (NWORDS, 0, MALL(1+OFFALL))
         CALL FILL (NWORDS, 0, MBAD(1+OFFBAD))
         END IF
      NWORDS = (NPOLN * NCHAN * NIF - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MCHAN, OFFCHN,
     *   IRET)
      IF (IRET.EQ.0) CALL FILL (1024*NWORDS, 0, MCHAN(1+OFFCHN))
C                                       Filter data.
      IF (IRET.EQ.0) CALL UVLIUV (NANT, NPOLN, NCHAN, NIF,
     *   MALL(1+OFFALL), MBAD(1+OFFBAD), MCHAN(1+OFFCHN), IRET)
C                                       Write history
      IF (IRET.EQ.0) CALL UVLIHI
C                                       report results
      IF ((IRET.EQ.0) .AND. (PRTLEV.GT.0)) CALL REPORT (NANT, NPOLN,
     *   NCHAN, NIF, MALL(1+OFFALL), MBAD(1+OFFBAD), MCHAN(1+OFFCHN))
C                                       Close down files, etc.
      CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, MCHAN, OFFCHN, I)
C
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE UVLIIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   UVLIIN gets input parameters for UVLIN and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      ISCOMP  L  If true data is compressed
C      LRECI   I  Input file record length
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
      CHARACTER PRGN*6, STAT*4, BLANK*6, PTYPE*2, LINE*80
      INTEGER   IROUND, NPARM, IERR, INCX, I, TLUN, TFIND, JERR, LUN,
     *   FITCH, FITWT, FITWTT, NDX, J, MPR, IOFF, NVER, JJ, NUMAN(513),
     *   JT, JTRIM
      DOUBLE PRECISION DP, CATD(128)
      LOGICAL   T, MATCH
      REAL      RPARM(20), CATR(256)
      HOLLERITH CATH(256)
      INCLUDE 'UVLIN.INC'
      INTEGER   CHNSEL(3,20,MAXIF), NW(MAXIF), K, K1, K2
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      DATA BLANK  /'      '/
      DATA T /.TRUE./
      DATA TLUN /10/
      DATA MPR /4/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 271
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
      NORDER = XORD + 0.1
      NORDER = MAX (0, MIN (1, NORDER))
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOACOR = XDOAC.GT.0.0
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
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
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Save input file info
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
      NIF = EIF - BIF + 1
      LBIF = BIF
      NCHAN = CATBLK(KINAX+JLOCF)
      NPOLN = CATBLK(KINAX+JLOCS)
C                                       Max antenna number
      NANT = MAXANT
      CALL FNDEXT ('AN', CATBLK, NVER)
      IF (NVER.GT.0) THEN
         LUN = 27
         CALL GETNAN (DISKIN, OLDCNO, CATBLK, LUN, BUFF1, NUMAN, JERR)
         IF ((NVER.GT.0) .AND. (JERR.EQ.0)) THEN
            JJ = NUMAN(1)
            NANT = 0
            DO 37 J = 1,JJ
               NANT = MAX (NANT, NUMAN(J+1))
 37            CONTINUE
            END IF
         END IF
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
C                                       get freq info
      I = 1
      CALL CHNDAT ('READ', BUFF1, DISKIN, OLDCNO, I, CATBLK, LUN, K,
     *   FOFF, ISBAND, FINC, BNDCOD, FRQSEL, JERR)
      IF (JERR.GT.0) THEN
         MSGTXT = 'ERROR READING FQ FILE'
         GO TO 990
         END IF
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, BUFF1, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, BUFF1, IERR)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
      NRPRMI = NRPARM
C                                       center frequencies
      IF (JLOCF.LT.0) XCENT = -1.
      IF (XCENT.GT.0.0) THEN
         INCX = CATBLK(KINAX+JLOCF) / 2 + 1
         DIFPIX = INCX - CATR(KRCRP+JLOCF)
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *      DIFPIX
         CATR(KRCRP+JLOCF) = INCX
      ELSE
         DIFPIX = 0.0
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       read compressed => write compr.
      IF (ISCOMP) THEN
         CATBLK(KINAX) = 1
         I = CATBLK(KIPCN)
         CALL CHR2H (8, 'WEIGHT  ', 1, CATBLK(KHPTP+2*I))
         CALL CHR2H (8, 'SCALE   ', 1, CATBLK(KHPTP+2*I+2))
         CATBLK(KIPCN) = I + 2
         ILOCWT = I
         END IF
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1040) IERR
            GO TO 990
            END IF
C                                       Only overwrite Input file, do
C                                       not destroy existing otherwise
         IF ((CCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN)) THEN
            WRITE (MSGTXT,1050)
            GO TO 990
            END IF
C                                       Recover existing CATBLK
         FRW(NCFILE+1) = 2
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1060) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
      NEWCNO = CCNO
C                                       Save output file info
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      INCX = CATBLK(KINAX)
      LRECO = LREC
      NRPRMO = NRPARM
      INCSO = INCS / INCX
      INCFO = INCF / INCX
      INCIFO = INCIF / INCX
C                                       copy header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, PTYPE,
     *   NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                        Get and report parameters
      WRITE (MSGTXT,1070) NAMEIN, CLAIN
      CALL MSGWRT (MPR)
      I = NCHAN * NIF
C                                       Get channel weights
      CALL FILL (I, 0, FITWTS)
      CALL FILL (MAXIF, 0, NW)
      IF (INFILE.EQ.' ') THEN
         MSGTXT = 'No name given for weight file, using ICHANSEL'
         CALL MSGWRT (MPR)
         I = 60 * MAXIF
         CALL FILL (I, 0, CHNSEL)
         DO 90 J = 1,20
            K = IROUND (XCHNS(2,J))
            IF (K.LE.0) GO TO 100
            K = IROUND (XCHNS(4,J))
            IF ((K.LE.0) .OR. (K.GT.NIF)) THEN
               K1 = 1
               K2 = NIF
            ELSE
               K1 = K
               K2 = K
               END IF
            DO 80 K = K1,K2
               NW(K) = NW(K) + 1
               CHNSEL(1,NW(K),K) = MAX (0, IROUND (XCHNS(1,J)))
               CHNSEL(2,NW(K),K) = MAX (0, IROUND (XCHNS(2,J)))
               CHNSEL(3,NW(K),K) = MAX (1, IROUND (XCHNS(3,J)))
 80            CONTINUE
 90         CONTINUE
C                                       If no channel selection
C                                       use 1 - NCHAN
 100     DO 130 K = 1,NIF
            IOFF = (K - 1) * NCHAN
            IF (NW(K).LE.0) THEN
               NW(K) = 1
               CHNSEL(1,1,K) = 1
               CHNSEL(2,1,K) = NCHAN
               CHNSEL(3,1,K) = 1
               END IF
            DO 120 I = 1,NW(K)
               CHNSEL(1,I,K) = MAX (1, MIN (CHNSEL(1,I,K), NCHAN))
               IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K)) CHNSEL(2,I,K) = NCHAN
               CHNSEL(2,I,K) = MAX (1, MIN (CHNSEL(2,I,K), NCHAN))
               DO 110 J = CHNSEL(1,I,K),CHNSEL(2,I,K),CHNSEL(3,I,K)
                  FITWTS(J+IOFF) = 1
 110              CONTINUE
 120           CONTINUE
 130        CONTINUE
      ELSE
         WRITE (MSGTXT,1110) INFILE
         CALL MSGWRT (MPR)
         CALL ZTXOPN ('READ', TLUN, TFIND, INFILE, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 700
         FITWTT = 0
 200     CONTINUE
            CALL ZTXIO ('READ', TLUN, TFIND, LINE, IERR)
            IF (IERR.EQ.2) GO TO 220
            IF (IERR.NE.0) GO TO 800
            JT = JTRIM (LINE)
            NDX = 1
            CALL GETNUM (LINE, 80, NDX, DP)
            IF (DP.EQ.DBLANK) GO TO 975
            FITCH = NINT (DP)
            CALL GETNUM (LINE, 80, NDX, DP)
            IF (DP.EQ.DBLANK) GO TO 975
            K = NINT (DP)
            CALL GETNUM (LINE, 80, NDX, DP)
            IF (DP.EQ.DBLANK) GO TO 975
            FITWT = NINT (DP)
            FITWT = MAX (FITWT, 0)
            IF (K.LE.0) THEN
               K1 = 1
               K2 = NIF
            ELSE
               K1 = K
               K2 = K
               END IF
            DO 210 K = K1,K2
               NW(K) = NW(K) + 1
               FITWTS(FITCH+(K-1)*NCHAN) = FITWT
 210           CONTINUE
            GO TO 200
 220     CALL ZTXCLS (TLUN, TFIND, IERR)
         DO 225 K = 1,NIF
            IF (NW(K).LE.0) CALL FILL (NCHAN, 1, FITWTS(1+(K-1)*NCHAN))
 225        CONTINUE
         END IF
C
      IF (RMS.LE.0.0) RMS = 1.0E20
      WRITE (MSGTXT, 1220) RMS
      CALL MSGWRT (4)
C
      IF (DOCONT.GT.0.0) THEN
         MSGTXT = 'Writing data with continuum retained'
      ELSE
         MSGTXT = 'Writing data with best continuum fit subtracted'
         END IF
      CALL MSGWRT (4)
C
      WRITE (MSGTXT,1240) SHIFT(1), SHIFT(2)
      CALL MSGWRT (4)
C                                       Report on weights used
      IF (PRTLEV.GT.1.5) THEN
         DO 270 K = 1,NIF
            IOFF = (K-1)*NCHAN
            DO 260 I = 1,NCHAN,20
               K1 = I
               K2 = MIN (I+19, NCHAN)
               WRITE (MSGTXT,1250) K, I, (FITWTS(J+IOFF),J = K1,K2)
               CALL MSGWRT (MPR)
 260           CONTINUE
 270        CONTINUE
         END IF
      GO TO 999
C                                       Did not find file with weights
 700  WRITE (MSGTXT,1700) INFILE
      GO TO 990
C                                       Error reading file with weights
 800  WRITE (MSGTXT,1800) INFILE
      GO TO 990
 975  MSGTXT = 'ERROR PARSING LINE ' // LINE(:61)
      JERR = 1
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('UVLIIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1030 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1035 FORMAT ('This program can only handle', I5, ' channels, sorry')
 1040 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1050 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1060 FORMAT ('UVLIIN: ERROR',I3,' UPDATING NEW CATBLK')
 1070 FORMAT ('UV input file: ',A16,'.',A6)
 1110 FORMAT ('Reading weights file: ',A48)
 1220 FORMAT ('Flagging threshold for unit weight = ',1PE12.3,' Jy')
 1240 FORMAT ('Continuum SHIFT =',F11.2,2X,F11.2,' arcsec')
 1250 FORMAT ('Weights (',I3,'/',I5,') ',20I2)
 1700 FORMAT ('Could not open weights file:', A48)
 1800 FORMAT ('Error reading weights file:', A48)
      END
      SUBROUTINE UVLIUV (NA, NP, NC, NI, MALL, MBAD, MCHAN, IRET)
C-----------------------------------------------------------------------
C   UVLIUV sends uv data one point at a time to the filtering
C   routine and then writes the modified data if requested.
C   Inputs:
C      NA      I      Number of antennas
C      NP      I      Number of polarizations
C      NC      I      Number of channels
C      NI      I      Number of IFs
C   Input in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true, data is compressed
C   Output:
C      MALL    I(*)   Counts samples by baseline etc
C      MBAD    I(*)   Counts full bad spectra by baseline
C      MCHAN   I(*)   Counts flags by channel
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NA, NP, NC, NI, MALL(NA,NA,NP,NI), MBAD(NA,NA,NP,NI),
     *   MCHAN(NP,NC,NI), IRET
C
      CHARACTER OFILE*48
      INTEGER   IPTRI, IPTRO, LUNO, INDO, ILENBU, KBIND, NIOUT, NIOLIM,
     *   IA1, IA2, BO, VO, NUMVIS, XCOUNT, NCORO, NCOPY, CATMP(256),
     *   RNXRET
      LOGICAL   T, F
      INCLUDE 'UVLIN.INC'
      REAL      VIS(UVBFSS), RESULT(UVBFSS), RPARM(20)
      DOUBLE PRECISION UVSCAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (BUFF1, VIS)
      DATA LUNO /17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Number of visibilities in input
C                                       and output files.
      NCORO = (LRECO - NRPRMO)
      IF (.NOT.ISCOMP) NCORO = NCORO / 3
      NCOPY = LRECO - NRPRMO
C                                       Open and init for read
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
      CALL UVPGET (IRET)
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       make an index table
      CALL RNXGET (DISKIN, OLDCNO, CATOLD)
      CALL RNXINI (DISKO, NEWCNO, CATBLK, RNXRET)
      NUMVIS = 0
      XCOUNT = 0
      IF ((FREQ.GT.0.0D0) .AND. (UVFREQ.GT.0.0D0)) THEN
         UVSCAL = FREQ / UVFREQ
      ELSE
         UVSCAL = 1.0D0
         END IF
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
C                                       Loop over
      ELSE IF (IRET.EQ.0) THEN
         IF (ILOCB.GE.0) THEN
            IA2 = BUFF1(IPTRI+ILOCB) + 0.1
            IA1 = IA2 / 256
            IA2 = IA2 - IA1*256
         ELSE
            IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
            IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
            END IF
         NUMVIS = NUMVIS + 1
C                                      Call filtering routine.
         CALL UVLINF (NUMVIS, RPARM(1+ILOCU), RPARM(1+ILOCV),
     *      RPARM(1+ILOCW), IA1, IA2, NA, NP, NC, NI, MALL, MBAD, MCHAN,
     *      VIS, RESULT, IRET)
C                                       Branch upon return of UVLINF
C                                       Error (fatal)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
C                                       Copy to output.
         ELSE IF (IRET.EQ.0) THEN
            XCOUNT = XCOUNT + 1.0D0
            CALL RNXUPD (RPARM, RNXRET)
            RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
            RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
            RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
            CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
C                                       Compressed
            IF (ISCOMP) THEN
               CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *            BUFF2(IPTRO+NRPRMO))
            ELSE
               CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
               END IF
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
            END IF
C                                       Write vis record.
         IF (NIOUT.GE.NIOLIM) THEN
            CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1150) IRET
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
            END IF
C                                       Read next buffer.
         GO TO 100
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       close NX table
      CALL RNXCLS (RNXRET)
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVLIUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('UVLIUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('UVLIUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1100 FORMAT ('UVLIUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('UVLIUV: UVBASE ERROR',I3)
 1150 FORMAT ('UVLIUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE REPORT (NA, NP, NC, NI, MALL, MBAD, MCHAN)
C-----------------------------------------------------------------------
C   Print the reports
C   Inputs:
C      NA      I      Number of antennas
C      NP      I      Number of polarizations
C      NC      I      Number of channels
C      NI      I      Number of IFs
C      MALL    I(*)   Counts samples by baseline etc
C      MBAD    I(*)   Counts full bad spectra by baseline
C      MCHAN   I(*)   Counts flags by channel
C-----------------------------------------------------------------------
      INTEGER   NA, NP, NC, NI, MALL(NA,NA,NP,NI), MBAD(NA,NA,NP,NI),
     *   MCHAN(NP,NC,NI)
C
      INTEGER   I, J, K, L, TOTALA, TOTALB, MMAX, MXA, I1, I2, MPR
      INCLUDE 'UVLIN.INC'
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   MAUX(MAXANT)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA MPR /4/
C-----------------------------------------------------------------------
      DO 90 L = 1,NI
         DO 80 K = 1,NP
C                                       First add the totals
            TOTALA = 0
            TOTALB = 0
            MMAX = 0
            DO 20 I = 1,NA
               DO 10 J = 1,NA
                  IF (MALL(I,J,K,L).GT.0) THEN
                     MXA = MAX (MXA, I)
                     MXA = MAX (MXA, J)
                     TOTALA = TOTALA + MALL(I,J,K,L)
                     IF (MALL(I,J,K,L).GT.MMAX) MMAX = MALL(I,J,K,L)
                     END IF
                  TOTALB = TOTALB + MBAD(I,J,K,L)
 10               CONTINUE
 20            CONTINUE
C                                       Now report on all visibilities
            IF (TOTALA.GT.0) THEN
               WRITE (MSGTXT,1020) TOTALB, TOTALA, K, L
               CALL MSGWRT (MPR)
               WRITE (MSGTXT,1021) MMAX
               CALL MSGWRT (MPR)
               I1 = 1
 25            I2 = MIN (I1+27, MXA)
               IF (I2.GE.I1) THEN
                  WRITE (MSGTXT,1025) (J, J = I1,I2)
                  CALL MSGWRT (MPR)
                  DO 35 I= 1,MXA
                     DO 30 J= I1,I2
                        MAUX(J) = NINT ((10. * MALL(I,J,K,L)) / MMAX)
 30                     CONTINUE
                     WRITE (MSGTXT,1030) I, (MAUX(J), J = I1,I2)
                     CALL MSGWRT (MPR)
 35                  CONTINUE
                  I1 = I2 + 1
                  GO TO 25
                  END IF
C                                       Now report percentage flagged
               MSGTXT = 'Visibilities flagged (percent):'
               CALL MSGWRT (MPR)
               I1 = 1
 40            I2 = MIN (I1+27, MXA)
               IF (I2.GE.I1) THEN
                  WRITE (MSGTXT,1025) (J, J = I1,I2)
                  CALL MSGWRT (MPR)
                  DO 50 I = 1,MXA
                     DO 45 J = I1,I2
                        MAUX(J) = 0
                        IF (MALL(I,J,K,L).GT.0) THEN
                           MAUX(J) = NINT ((100. * MBAD(I,J,K,L)) /
     *                        MALL(I,J,K,L))
                        ELSE
                           MAUX(J) = 0
                           END IF
 45                     CONTINUE
                     WRITE (MSGTXT,1030) I, (MAUX(J), J = I1,I2)
                     CALL MSGWRT (MPR)
 50                  CONTINUE
                  I1 = I2 + 1
                  GO TO 40
                  END IF
C                                       Now report on channel triggers
               IF (PRTLEV.GT.1.5) THEN
                  DO 60 I = 1,NCHAN
                     IF (MCHAN(K,I,L).GT.0) THEN
                        WRITE (MSGTXT,1050) I, MCHAN(K,I,L)
                        CALL MSGWRT (MPR)
                        END IF
 60                  CONTINUE
                  END IF
               END IF
 80         CONTINUE
 90      CONTINUE
C                                       Report total of flagged corrs.
      WRITE (MSGTXT,1090) NBDCOR
      CALL MSGWRT (MPR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Flagged ',I8,' of',I8,' correlators for corr IF',I2,I3)
 1021 FORMAT ('Visibilities per baseline (tens of percent of ',I8,'):')
 1025 FORMAT (7X,28(I2))
 1030 FORMAT ('Ant',I3,1X,28(I2))
 1050 FORMAT ('Channel:',I4,'; ',I8,' flags')
 1090 FORMAT ('Flagged ',I8,' correlators')
      END
      SUBROUTINE UVLIHI
C-----------------------------------------------------------------------
C   UVLIHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, J, K, K1, K2
      LOGICAL   T
      INCLUDE 'UVLIN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                       calibration adverbs
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       INFILE
      IF (INFILE.NE.' ') THEN
         WRITE (HILINE,1010) TSKNAM, INFILE
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Weights used
      DO 30 K = 1,NIF
         DO 25 I = 1,NCHAN,20
            K1 = I
            K2 = MIN (I+19, NCHAN)
            WRITE (HILINE,1020) TSKNAM, K, I, (FITWTS(J),J = K1,K2)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 25         CONTINUE
 30      CONTINUE
C                                       Threshold
      WRITE (HILINE,1030) TSKNAM, RMS
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C
      IF (DOCONT.LE.0) THEN
         WRITE (HILINE,1040) TSKNAM
      ELSE
         WRITE (HILINE,1041) TSKNAM
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Number flagged correlators
      WRITE (HILINE,1050) TSKNAM, NBDCOR
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       SHIFT
      WRITE (HILINE,1060) TSKNAM, SHIFT(1), SHIFT(2)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       ORDER
      WRITE (HILINE,1065) TSKNAM, NORDER
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                       Copy tables
      CALL COPTAB (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'UVLIHI: ERROR COPYING TABLES TO OUTPUT'
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, NEWCNO, DIFPIX, BUFF2(1025), BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'UVLIHI: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVLIHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Weights from: ',A48)
 1020 FORMAT (A6,'/ Weights (',I3,'/',I5,') ',20I2)
 1030 FORMAT (A6,'/ Flagging threshold for unit weight = ',F8.2,' Jy')
 1040 FORMAT (A6,'/ Best continuum subtracted')
 1041 FORMAT (A6,'/ Kept continuum')
 1050 FORMAT (A6,'/ Correlators flagged:', I8)
 1060 FORMAT (A6,'/ Continuum SHIFT =',F11.2,2X,F11.2,' arcsec')
 1065 FORMAT (A6,'ORDER =',I3,10X,'/ order of polynomial fit')
      END
      SUBROUTINE UVLINF (NUMVIS, U, V, W, IA1, IA2, NA, NP, NC, NI,
     *   MALL, MBAD, MCHAN, VIS, RESULT, IRET)
C-----------------------------------------------------------------------
C   Routine to fit straight line to chosen channels and subtract.
C   *** IT FITS TO REAL AND IMAGINARY COMPONENTS ***
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      U       R    U in wavelengths
C      V       R    V in wavelengths
C      W       R    W in wavelengths
C      IA1     I    First antenna number
C      IA2     I    Second antenna number
C      NA      I      Number of antennas
C      NP      I      Number of polarizations
C      NC      I      Number of channels
C      NI      I      Number of IFs
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      FITWT   I(MAXCHA)  Weights for fitting
C   Output:
C      MALL    I(*)   Counts samples by baseline etc
C      MBAD    I(*)   Counts full bad spectra by baseline
C      MCHAN   I(*)   Counts flags by channel
C      RESULT  R(3,*) Output visibilities selected in frequency.
C      IRET    I  Return code  -1 => don't write
C                               0 => OK
C                              >0 => error, terminate.
C   Output in COMMON:
C      CATBLK    I         Catalog header block
C   Auxiliary variables for info (VLA only, change dimensions for other)
C      MALL     I         28 by 28 by 8 matrix for all visibilities
C      MBAD     I         28 by 28 by 8 matrix for bad visibilities
C      MCHAN    I         512 by 8 matrix to keep track of flag triggers
C
C-----------------------------------------------------------------------
      INCLUDE 'UVLIN.INC'
      INTEGER   NUMVIS, IA1, IA2, NA, NP, NC, NI, MALL(NA,NA,NP,NI),
     *   MBAD(NA,NA,NP,NI), MCHAN(NP,NC,NI), IRET
      REAL      U, V, W, VIS(3,*), RESULT(3,*)
C
      INTEGER   LOOP, INDEX, OFF, IS, IIF, FITNUM, IOFF
      REAL      AVGR, AVGI, AMPLIT, XX, CATR(256), FITRE(MAXCHA),
     *   FITIM(MAXCHA), FITCHA(MAXCHA), AR, BR, AI, BI
      COMPLEX   ZZ, VS
      DOUBLE PRECISION RA0, DEC0, RFREQ, DFREQ, CFREQ, TRUEF, DXC, DYC,
     *   DZC
      LOGICAL   T, F, ALLBAD, RESCON, GOODDT, DOSHIF, REPORT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATR, CATBLK)
      SAVE RFREQ, DXC, DYC, DZC, REPORT, RESCON, DOSHIF
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Set up on first call
      IF (NUMVIS.EQ.1) THEN
         REPORT = F
         NBDCOR = 0
         RESCON = DOCONT.GT.0.0
         RFREQ = CATR(KRCRP+JLOCF)
         DOSHIF = (ABS(SHIFT(1)).GT.1E-6) .OR.
     *            (ABS(SHIFT(2)).GT.1E-6)
         RA0 = RA
         DEC0 = DEC
         IF (COS(DG2RAD*DEC0).NE.0.0D0) RA = RA0 + SHIFT(1) / 3600.D0
     *      / COS(DG2RAD * DEC0)
         DEC = DEC + SHIFT(2) / 3600.D0
         DXC = SIN (DG2RAD * (RA-RA0)) * COS (DEC * DG2RAD)
         DYC = COS (DEC0 * DG2RAD) * SIN (DEC * DG2RAD) -
     *      SIN (DEC0 * DG2RAD) * COS (DEC * DG2RAD) *
     *      COS ((RA - RA0) * DG2RAD)
         DZC = SIN (DG2RAD * DEC0) * SIN (DG2RAD * DEC) +
     *      COS (DG2RAD * DEC0) * COS (DG2RAD * DEC) *
     *      COS (DG2RAD * (RA - RA0)) - 1.0D0
         DXC = TWOPI * DXC
         DYC = TWOPI * DYC
         DZC = TWOPI * DZC
C                                        if PRTLEV>0
         IF (PRTLEV.GT.0.) THEN
            REPORT = T
            END IF
         END IF
C                                       End if first vis
      IRET = 0
      IF (NUMVIS.GT.0) THEN
         IF (MOD(NUMVIS,10000).EQ.0) THEN
            WRITE (MSGTXT,1000) NUMVIS
            CALL MSGWRT (4)
            END IF
C                                       Loop over IF
         DO 400 IIF = 1,NIF
            IOFF = (IIF - 1) * NCHAN
            TRUEF = 1.0D0 + FOFF(IIF+LBIF-1) / UVFREQ
            DFREQ = FINC(IIF+LBIF-1) / UVFREQ
C                                       Loop over Stokes
            DO 350 IS = 1,NPOLN
               ALLBAD = F
               GOODDT = T
C                                       Offset in Stokes and IF
               OFF = (IS - 1) * INCSI + (IIF - 1) * INCIFI
C
               IF (DOSHIF) THEN
                  INDEX = OFF + 1
                  DO 60 LOOP = 1, NCHAN
                     CFREQ = TRUEF + DFREQ * (LOOP - RFREQ)
                     XX = (U*DXC + V*DYC + W*DZC) * CFREQ
                     ZZ = CMPLX (COS(XX), -SIN(XX))
                     VS = CMPLX (VIS(1,INDEX), VIS(2,INDEX)) * ZZ
                     VIS(1,INDEX) = REAL(VS)
                     VIS(2,INDEX) = AIMAG(VS)
                     INDEX = INDEX + INCFI
 60                  CONTINUE
                  END IF
C                                       Index for this channel, IF
               INDEX = OFF + 1
C                                       Get arrays for line fitting
               FITNUM = 0
               DO 100 LOOP = 1,NCHAN
                  IF (VIS(3,INDEX).GT.0.0) THEN
                      IF (FITWTS(LOOP+IOFF).GT.0) THEN
                          FITNUM = FITNUM + 1
                          FITRE(FITNUM) = VIS(1,INDEX)
                          FITIM(FITNUM) = VIS(2,INDEX)
                          FITCHA(FITNUM) = LOOP
                          END IF
                      END IF
                  INDEX = INDEX + INCFI
 100              CONTINUE
C                                       Check that some data are ok
            IF (FITNUM.GT.0) THEN
               CALL LINFIT (NORDER, FITCHA, FITRE, FITNUM, AR, BR)
               CALL LINFIT (NORDER, FITCHA, FITIM, FITNUM, AI, BI)
            ELSE
               AR = 0.0
               BR = 0.0
               AI = 0.0
               BI = 0.0
               GOODDT = F
               END IF
C                                       Now subtract from data
C                                       Actual offset in Stokes and IF
               INDEX = OFF + 1
      INCLUDE 'INCS:ZVND.INC'
               DO 200 LOOP = 1,NCHAN
C                                       Get interpolated value (re, im)
                  AVGR = AR + BR * LOOP
                  AVGI = AI + BI * LOOP
C                                       Subtract from vis.
                  RESULT(1,INDEX) = VIS(1,INDEX) - AVGR
                  RESULT(2,INDEX) = VIS(2,INDEX) - AVGI
                  AMPLIT = RESULT(1,INDEX)**2 + RESULT(2,INDEX)**2
C                                       Scale residual-squared by
C                                       integration time, take sq. root
                  AMPLIT = SQRT ( AMPLIT * ABS (VIS(3,INDEX) ) )
C                                       Do check if this is a test chan.
                  IF ((FITWTS(LOOP+IOFF).EQ.1).AND. (AMPLIT.GT.RMS))
     *               THEN
                     ALLBAD = T
C                                       Accumulate for statistics
C                                       if report wanted
                     IF (REPORT) MCHAN(IS,LOOP,IIF) = MCHAN(IS,LOOP,IIF)
     *                  + 1
                     END IF
C                                       Make output with or without
C                                       continuum
                  IF (RESCON) THEN
                     RESULT(1,INDEX) = VIS(1,INDEX)
                     RESULT(2,INDEX) = VIS(2,INDEX)
                     END IF
C                                       Copy original weight
                  RESULT(3,INDEX) = VIS(3,INDEX)
                  INDEX = INDEX + INCFI
 200              CONTINUE
C                                       If reporting, increase
C                                       corresponding counter
               IF (GOODDT.AND.REPORT) MALL(IA1,IA2,IS,IIF) =
     *            MALL(IA1,IA2,IS,IIF) + 1
C                                       Now flag everything if needed
               IF (ALLBAD)  THEN
                  INDEX = OFF + 1
                  DO 250 LOOP = 1,NCHAN
                     IF (RESULT(3,INDEX).GT.0) RESULT(3,INDEX) =
     *                  -RESULT(3,INDEX)
                     INDEX = INDEX + INCFI
250                  CONTINUE
C                                       Increase corresponding counters
                  IF (GOODDT) THEN
                     NBDCOR = NBDCOR + 1
                     IF (REPORT) MBAD(IA1,IA2,IS,IIF) =
     *                  MBAD(IA1,IA2,IS,IIF) + 1
                     END IF
                  END IF
C                                       Shift back if needed
               IF (DOSHIF) THEN
                  INDEX = OFF + 1
                  DO 300 LOOP = 1,NCHAN
                     CFREQ = TRUEF + DFREQ * (LOOP - RFREQ)
                     XX = (U*DXC + V*DYC + W*DZC) * CFREQ
                     ZZ = CMPLX (COS(XX), SIN(XX))
                     VS = CMPLX (RESULT(1,INDEX), RESULT(2,INDEX)) * ZZ
                     RESULT(1,INDEX) = REAL(VS)
                     RESULT(2,INDEX) = AIMAG(VS)
                     INDEX = INDEX + INCFI
 300                 CONTINUE
                  END IF
 350           CONTINUE
 400        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Working on visibility record ', I8)
      END
      SUBROUTINE LINFIT (N, X, Y, NDATA, A, B)
C-----------------------------------------------------------------------
C     Routine to fit straight line
C-----------------------------------------------------------------------
      DOUBLE PRECISION SX, SY, SXY, SYY, SXX
      INTEGER   N, NDATA, I
      REAL      X(*), Y(*), A, B, DELTA
C-----------------------------------------------------------------------
      SX = 0.D0
      SY = 0.D0
      SXX = 0.D0
      SXY = 0.D0
      SYY = 0.D0
      DO 100 I = 1,NDATA
         SY = SY + Y(I)
         IF (N.GT.0) THEN
            SX = SX + X(I)
            SXX = SXX + X(I) * X(I)
            SXY = SXY + X(I) * Y(I)
            SYY = SYY + Y(I) * Y(I)
            END IF
 100     CONTINUE
      A = 0.0
      B = 0.0
      IF ((NDATA.GE.2) .AND. (N.GT.0)) THEN
         DELTA = NDATA * SXX - SX * SX
         IF (DELTA.GT.0) THEN
            A = ( SXX * SY - SX * SXY ) / DELTA
            B = ( SXY * NDATA - SX * SY ) / DELTA
            END IF
      ELSE IF ((NDATA.GE.1) .AND. (N.EQ.0)) THEN
         A = SY / NDATA
         END IF
C
 999  RETURN
      END
