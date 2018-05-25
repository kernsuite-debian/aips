LOCAL INCLUDE 'UVLSD.INC'
C                                       Local include for UVLSD
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XNAMOU(3),
     *   XCLAOU(2)
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH(3), XDOAC, XSOUT, XDISO, XBCHAN,
     *   XECHAN, XCHSEL(4,20), XORD, DOOUT, XCHANF, MFLUX, RFLUX,
     *   SHIFT(2), XCENT,
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), BUFF3(UVBFSS), FINC(MAXIF),
     *   DIFPIX
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, LBCHAN, LECHAN, JBUFSZ,
     *   ILOCWT, CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO,
     *   INCIFO, LRECO, NRPRMI, NRPRMO, CHNSEL(4,20,MAXIF), LBIF,
     *   NCHSEL, CHFIT0, DISK2, SEQO2, CAT2(256), INCS2, INCF2, INCIF2,
     *   LREC2, NRPRM2, CHMASK(MAXCIF), NORDER, NIF, NFREQ, OLDCNO,
     *   NEWCNO(2), ISBAND(MAXIF)
      LOGICAL   ISCOMP, DOFLUX
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      DOUBLE PRECISION FOFF(MAXIF)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XDOAC,
     *   XNAMOU, XCLAOU, XSOUT, XDISO, XBCHAN, XECHAN, XCHSEL, XORD,
     *   DOOUT, XCHANF, MFLUX, RFLUX, SHIFT, XCENT
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /BUFRS/ BUFF1, BUFF2, BUFF3, JBUFSZ
      COMMON /INFO/ CATOLD, CAT2, FOFF, FINC, ISBAND, SEQIN, SEQOUT,
     *   DISKIN, DISKO, LBCHAN, LECHAN, ILOCWT, INCSI, INCFI, INCIFI,
     *   INCSO, INCFO, INCIFO, LRECO, NRPRMI, NRPRMO, ISCOMP, CHNSEL,
     *   NCHSEL, CHFIT0, DISK2, SEQO2, INCS2, INCF2, INCIF2, LREC2,
     *   NRPRM2, CHMASK, DOFLUX, NORDER, NIF, NFREQ, OLDCNO, NEWCNO,
     *   LBIF, DIFPIX
LOCAL END
      PROGRAM UVLSD
C-----------------------------------------------------------------------
C! Averages several channels and divides the uv data.
C# Utility UV UV-util VLA VLB SPECTRAL
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2007-2012, 2014-2015
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
C   UVLSD averages a set of channels and divides them into another
C   range of channels.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C          calibration adverbs added
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      BCHAN          BCHAN         Start channel to write out
C      ECHAN          ECHAN         End channel to write out
C      CHANSEL(3,10)  CHNSEL        Channels to select for baseline
C      DOOUTPUT       DOOUT         > 0 => write fit uv data also
C      CHANNEL        CHFIT0        first fit channel out
C      SHIFT          SHIFT(2)      RA and Dec phase shift done before
C                                   fit and then undone.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'UVLSD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'UVLSD '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVLSDI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Filter data.
      CALL UVLSDU (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UVLSDH
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE UVLSDI (PRGN, JERR)
C-----------------------------------------------------------------------
C   UVLSDI gets input parameters for UVLSD and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      BCHAN   I  Lowest channel number to write.
C      ECHAN   I  Highest channel number to write.
C      ISCOMP  L  If true data is compressed
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
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, CATMP(256), J, K, K1, K2
      LOGICAL   T, MATCH
      INCLUDE 'UVLSD.INC'
      INTEGER   NW(MAXIF), IOFF, LUN
      REAL      CATR(256), RPARM(20)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA BLANK /' '/
      DATA T /.TRUE./
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
      NPARM = 252
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
      DISK2 = DISKO
      SEQO2 = SEQOUT
      DOFLUX = (MFLUX.GT.0.0) .OR. (RFLUX.GT.0.0)
      IF (MFLUX.LE.0.0) MFLUX = 1.E20
      IF (RFLUX.LE.0.0) RFLUX = 1.E20
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
        WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
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
      NFREQ = CATBLK(KINAX+JLOCF)
      LBCHAN = IROUND (XBCHAN)
      LECHAN = IROUND (XECHAN)
      CHFIT0 = IROUND (XCHANF)
      IF ((CHFIT0.LE.0) .OR. (CHFIT0.GT.NFREQ)) CHFIT0 = IROUND
     *   (CATR(KRCRP+JLOCF))
      IF ((CHFIT0.LE.0) .OR. (CHFIT0.GT.NFREQ)) CHFIT0 =
     *   IROUND (CATR(KRCRP+JLOCF))
      BCHAN = 1
      ECHAN = NFREQ
C
      IF ((LBCHAN.LE.0) .OR. (LBCHAN.GT.NFREQ)) LBCHAN = 1
      IF ((LECHAN.LE.0) .OR. (LECHAN.GT.NFREQ)) LECHAN = NFREQ
      IF (LBCHAN.GT.LECHAN) THEN
         MSGTXT = 'INVALID BCHAN AND ECHAN'
         CALL MSGWRT (6)
         JERR = 1
         GO TO 990
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
C                                       Set fit windows
      I = 60 * MAXIF
      CALL FILL (I, 0, CHNSEL)
      CALL FILL (MAXIF, 0, NW)
      DO 20 J = 1,20
         K = IROUND (XCHSEL(2,J))
         IF (K.LE.0) GO TO 25
         K = IROUND (XCHSEL(4,J))
         IF ((K.LE.0) .OR. (K.GT.NIF)) THEN
            K1 = 1
            K2 = NIF
         ELSE
            K1 = K
            K2 = K
            END IF
         DO 15 K = K1,K2
            NW(K) = NW(K) + 1
            CHNSEL(1,NW(K),K) = MAX (0, IROUND (XCHSEL(1,J)))
            CHNSEL(2,NW(K),K) = MAX (0, IROUND (XCHSEL(2,J)))
            CHNSEL(3,NW(K),K) = MAX (1, IROUND (XCHSEL(3,J)))
 15         CONTINUE
 20      CONTINUE
C                                       If no channel selection
C                                       use 1 - NFREQ
 25   CALL FILL (MAXCIF, 0, CHMASK)
      DO 35 K = 1,NIF
         IOFF = (K - 1) * NFREQ
         IF (NW(K).LE.0) THEN
            NW(K) = 1
            CHNSEL(1,1,K) = 1
            CHNSEL(2,1,K) = NFREQ
            CHNSEL(3,1,K) = 1
            END IF
         DO 30 I = 1,NW(K)
            CHNSEL(1,I,K) = MAX (1, MIN (CHNSEL(1,I,K), NFREQ))
            IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K)) CHNSEL(2,I,K) = NFREQ
            CHNSEL(2,I,K) = MAX (1, MIN (CHNSEL(2,I,K), NFREQ))
            DO 28 J = CHNSEL(1,I,K),CHNSEL(2,I,K),CHNSEL(3,I,K)
               CHMASK(J+IOFF) = 1
 28            CONTINUE
 30         CONTINUE
 35      CONTINUE
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, BUFF1, IERR)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       check that single source by now
      CALL AXEFND (6, 'SOURCE', CATBLK(KIPCN), CATH(KHPTP), I, IERR)
      IF (IERR.EQ.0) THEN
         MSGTXT = 'MORE THAN ONE SOURCE SELECTED: NOT ALLOWED'
         JERR = 10
         GO TO 990
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
C                                       Frequency axis
      CATBLK(KINAX+JLOCF) = LECHAN - LBCHAN + 1
      CATR(KRCRP+JLOCF) = CATR(KRCRP+JLOCF) - LBCHAN + 1
      CATR(KRARP) = CATR(KRARP) - LBCHAN + 1
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
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
         IF ((CCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN)) THEN
            WRITE (MSGTXT,1060)
            GO TO 990
            END IF
C                                       Recover existing CATBLK
         FRW(NCFILE+1) = 2
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1065) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
      NEWCNO(1) = CCNO
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
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       second output file
      IF (DOOUT.GT.0.0) THEN
         CALL COPY (256, CATBLK, CATMP)
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, 'BASFIT', KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = SEQO2
C                                       continuum not compressed
         IF (ISCOMP) THEN
            CATBLK(KINAX) = 3
            CATBLK(KIPCN) = CATBLK(KIPCN) - 2
            END IF
C                                       Frequency axis
         CATBLK(KINAX+JLOCF) = 1
         CATR(KRCRP+JLOCF) = CATR(KRCRP+JLOCF) - CHFIT0 + 1
         CATR(KRARP) = CATR(KRARP) - CHFIT0 + 1
C                                       Create output file.
         NEWCNO(2) = 1
         FRW(NCFILE+1) = 3
         JERR = 4
         CALL UVCREA (DISK2, NEWCNO(2), BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISK2
         FCNO(NCFILE) = NEWCNO(2)
         FRW(NCFILE) = FRW(NCFILE) - 1
C                                       Save output file info
         CALL UVPGET (JERR)
         IF (JERR.NE.0) GO TO 999
         INCX = CATBLK(KINAX)
         LREC2 = LREC
         NRPRM2 = NRPARM
         INCS2 = INCS / INCX
         INCF2 = INCF / INCX
         INCIF2 = INCIF / INCX
         JERR = 0
C                                       save catblk
         SEQO2 = CATBLK(KIIMS)
         CALL COPY (256, CATBLK, CAT2)
         CALL COPY (256, CATMP, CATBLK)
C                                       copy header keywords
         CALL KEYCOP (DISKIN, OLDCNO, DISK2, NEWCNO(2), IERR)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVLSDI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('UVLSDI: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE UVLSDU (IRET)
C-----------------------------------------------------------------------
C   UVLSDU sends uv data one point at a time to the filtering
C   routine and then writes the modified data if requested.
C   Input in common:
C      BCHAN   I  Lowest channel number to write.
C      ECHAN   I  Highest channel number to write.
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OFILE*48
      INTEGER   IPTRO, LUNO, INDO, LUN2, IND2, ILENBU, KBIND, NIOUT,
     *   NIOLIM, IA1, IA2, BO, VO, NUMVIS, XCOUNT, NCORO, RNXRET, NCOPY,
     *   NCOR2, NCOP2, IPTR2, NIOU2, NIOLI2, CATMP(256)
      LOGICAL   T, F
      INCLUDE 'UVLSD.INC'
      REAL      DUM, VIS(UVBFSS), RESULT(UVBFSS), RESUL2(UVBFSS),
     *   RPARM(20)
      DOUBLE PRECISION UVSCAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (BUFF1, VIS)
      DATA LUNO, LUN2 /17, 18/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Number of visibilities in input
C                                       and output files.
      NCORO = (LRECO - NRPRMO)
      IF (.NOT.ISCOMP) NCORO = NCORO / 3
      NCOPY = LRECO - NRPRMO
      NCOR2 = (LREC2 - NRPRM2) / 3
      NCOP2 = LREC2 - NRPRM2
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
      CALL RNXINI (DISKO, NEWCNO(1), CATBLK, RNXRET)
C                                       Open vis file for write
      IF (DOOUT.GT.0.0) THEN
         CALL ZPHFIL ('UV', DISK2, NEWCNO(2), 1, OFILE, IRET)
         CALL ZOPEN (LUN2, IND2, DISK2, OFILE, T, F, F, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       Init vis file for write
         ILENBU = 0
         CALL UVINIT ('WRIT', LUN2, IND2, NVIS, VO, LREC2, ILENBU,
     *      JBUFSZ, BUFF3, BO, KBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
         IPTR2 = KBIND
         NIOU2 = 0
         NIOLI2 = ILENBU
         END IF
C                                       Init vis file for read.
      NUMVIS = 0
      XCOUNT = 0
      IF ((FREQ.GT.0.0D0) .AND. (UVFREQ.GT.0.0D0)) THEN
         UVSCAL = FREQ / UVFREQ
      ELSE
         UVSCAL = 1.0D0
         END IF
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVGET ('READ', RPARM, VIS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
C                                       Loop over buffer
         IF (IRET.EQ.0) THEN
            IF (ILOCB.GE.0) THEN
               IA2 = RPARM(1+ILOCB) + 0.1
               IA1 = IA2 / 256
               IA2 = IA2 - IA1*256
            ELSE
               IA1 = RPARM(1+ILOCA1) + 0.1
               IA2 = RPARM(1+ILOCA2) + 0.1
               END IF
            NUMVIS = NUMVIS + 1
C                                       Call filtering routine.
            CALL UVLSDF (NUMVIS, RPARM(1+ILOCU), RPARM(1+ILOCV),
     *         RPARM(1+ILOCW), VIS, RESULT, RESUL2, IRET)
C                                       Branch on his return
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
            ELSE IF (IRET.EQ.0) THEN
               XCOUNT = XCOUNT + 1
C                                       update NX table
               CALL RNXUPD (RPARM, RNXRET)
               RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
               RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
               RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
               CALL RCOPY (NRPRMO, RPARM, BUFF2(IPTRO))
               IF (ISCOMP) THEN
                  CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *               BUFF2(IPTRO+NRPRMO))
               ELSE
                  CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
                  END IF
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
               IF (DOOUT.GT.0.0) THEN
                  CALL RCOPY (NRPRM2, RPARM, BUFF3(IPTR2))
                  CALL RCOPY (NCOP2, RESUL2, BUFF3(IPTR2+NRPRM2))
                  IPTR2 = IPTR2 + LREC2
                  NIOU2 = NIOU2 + 1
                  END IF
               END IF
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1150) IRET, XCOUNT
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
C                                       Write vis record.
            IF ((DOOUT.GT.0.0) .AND. (NIOU2.GE.NIOLI2)) THEN
               CALL UVDISK ('WRIT', LUN2, IND2, BUFF3, NIOLI2, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1150) IRET, XCOUNT
                  GO TO 990
                  END IF
               IPTR2 = KBIND
               NIOU2 = 0
               END IF
C                                       Read next buffer.
            GO TO 100
            END IF
C                                       Final call to UVLSDF.
      NUMVIS = -1
      CALL UVLSDF (NUMVIS, DUM, DUM, DUM, BUFF1, RESULT, RESUL2, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET, XCOUNT
         GO TO 990
         END IF
C                                       Finish write
      IF (DOOUT.GT.0.0) THEN
         NIOU2 = - NIOU2
         CALL UVDISK ('FLSH', LUN2, IND2, BUFF3, NIOU2, KBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1150) IRET, XCOUNT
            GO TO 990
            END IF
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       close NX table
      CALL RNXCLS (RNXRET)
C                                       compress other output file
      IF (DOOUT.GT.0.0) CALL UCMPRS (NVIS, DISK2, NEWCNO(2), LUN2, CAT2,
     *   IRET)
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IF (DOOUT.GT.0.0) CALL ZCLOSE (LUN2, IND2, IRET)
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
 1000 FORMAT ('UVLSDU: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1010 FORMAT ('UVLSDU: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('UVLSDU: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1100 FORMAT ('UVLSDU: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('UVLSDU: UVLSDF ERROR',I3)
 1150 FORMAT ('UVLSDU: ERROR',I3,' WRITING VIS FILE AT VIS',I9)
      END
      SUBROUTINE UVLSDH
C-----------------------------------------------------------------------
C   UVLSDH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, K, INVER, OUVER
      LOGICAL   T
      INCLUDE 'UVLSD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO(1), CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
C                                       calibration adverbs
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       LBCHAN, LECHAN
      WRITE (HILINE,2000) TSKNAM, LBCHAN, LECHAN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       shifts
      IF ((SHIFT(1).NE.0.0) .OR. (SHIFT(2).NE.0.0)) THEN
         WRITE (HILINE,2015) TSKNAM, SHIFT
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                      CHANSEL
      DO 25 K = 1,NIF
         DO 20 I = 1,20
            IF ((CHNSEL(1,I,K).GT.0) .AND.
     *         (CHNSEL(2,I,K).GE.CHNSEL(1,I,K))) THEN
               WRITE (HILINE,2020) TSKNAM, I, CHNSEL(1,I,K),
     *            CHNSEL(2,I,K), CHNSEL(3,I,K), K
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 100
               END IF
 20         CONTINUE
 25      CONTINUE
C                                       ORDER
      WRITE (HILINE,2025) TSKNAM, NORDER
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Close HI file
 100  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL COPTAB (DISKIN, OLDCNO, DISKO, NEWCNO(1), IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'UVLSDH: ERROR COPYING TABLES TO LINE UV'
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, NEWCNO(1), DIFPIX, BUFF1, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'UVLSDH: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO(1), CATBLK, 'REST', BUFF1, IERR)
C
      IF (DOOUT.GT.0.0) THEN
         CALL COPY (256, CAT2, CATBLK)
C                                       Copy/open history file.
         CALL HISCOP (LUN1, LUN2, DISKIN, DISK2, OLDCNO, NEWCNO(2),
     *      CATBLK, BUFF1, BUFF2, IERR)
         IF (IERR.GT.2) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (6)
            GO TO 200
            END IF
C                                       New history
         CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *      IERR)
         IF (IERR.NE.0) GO TO 200
         CALL HENCOO (TSKNAM, NAMOUT, 'BASFIT', SEQO2, DISK2, LUN2,
     *      BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       calibration adverbs
         CALL CALHIS (LUN2, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       output channel
         WRITE (HILINE,2010) TSKNAM, CHFIT0
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       shifts
         IF ((SHIFT(1).NE.0.0) .OR. (SHIFT(2).NE.0.0)) THEN
            WRITE (HILINE,2015) TSKNAM, SHIFT
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
C                                      CHANSEL
         DO 125 K = 1,NIF
            DO 120 I = 1,20
               IF ((CHNSEL(1,I,K).GT.0) .AND.
     *            (CHNSEL(2,I,K).GE.CHNSEL(1,I,K))) THEN
                  WRITE (HILINE,2020) TSKNAM, I, CHNSEL(1,I,K),
     *               CHNSEL(2,I,K), CHNSEL(3,I,K), K
                  CALL HIADD (LUN2, HILINE, BUFF2, IERR)
                  IF (IERR.NE.0) GO TO 200
                  END IF
 120           CONTINUE
 125        CONTINUE
C                                       ORDER
         WRITE (HILINE,2025) TSKNAM, NORDER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       Close HI file
 200     CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                       copy new NX table
         INVER = 1
         OUVER = 1
         CALL TABCOP ('NX', INVER, OUVER, LUN1, LUN2, DISKO, DISK2,
     *      NEWCNO(1), NEWCNO(2), CATBLK, BUFF1, BUFF2, IERR)
C                                        Copy tables
         CALL COPTAB (DISKIN, OLDCNO, DISK2, NEWCNO(2), IERR)
         IF (IERR.GT.2) THEN
            MSGTXT = 'UVLSDH: ERROR COPYING TABLES TO CONTINUUM UV'
            CALL MSGWRT (6)
            END IF
C                                       correct for FQCENTER
         CALL CENTFQ (DISK2, NEWCNO(2), DIFPIX, BUFF1, BUFF2, IERR)
         IF (IERR.GT.0) THEN
            MSGTXT = 'UVLSDH: ERROR CORRECTING FQ TABLE'
            CALL MSGWRT (6)
            END IF
C                                        Update CATBLK.
         CALL CATIO ('UPDT', DISK2, NEWCNO(2), CATBLK, 'REST', BUFF1,
     *      IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVLSDH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BCHAN =',I4,', ECHAN =',I4,
     *   ' / Output channel selection')
 2010 FORMAT (A6,'CHANNEL =',I5,5X,'/ Channel at which fit evaluated')
 2015 FORMAT (A6,'SHIFT =',F10.5,' , ',F10.5,' / shifts in asec before',
     *   ' fit')
 2020 FORMAT (A6,'CHANSEL(',I2,')=',I5,',',I5,',',I2,5X,
     *   '/ channels fit IF =',I3)
 2025 FORMAT (A6,'ORDER =',I3,10X,'/ order of polynomial fit')
      END
      SUBROUTINE UVLSDF (NUMVIS, U, V, W, VIS, RESULT, RESUL2, IRET)
C-----------------------------------------------------------------------
C   Routine to fit baseline to channels, divide and perhaps flag
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      U       R    U in wavelengths
C      V       R    V in wavelengths
C      W       R    W in wavelengths
C      T       R    Time in days.
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
C      CHNSEL(3,10) Gives channel selection
C      BCHAN   I    Lowest channel number to write.
C      ECHAN   I    Highest channel number to write.
C      NRPRMI  I    Input number of random parameters.
C      INCSI   I    Input Stokes' increment in vis.
C      INCFI   I    Input frequency increment in vis.
C      INCIFI  I    Input IF increment in vis.
C      LRECO   I    Output file record length
C      NRPRMO  I    Output number of random parameters.
C      INCSO   I    Output Stokes' increment in vis.
C      INCFO   I    Output frequency increment in vis.
C      INCIFO  I    Output IF increment in vis.
C   Output:
C      RESULT  R(3,*) Output visibilities selected in frequency.
C      RESUL2  R(3,*) Baseline fit value at CHFIT0
C      IRET    I  Return code  -1 => don't write
C                               0 => OK
C                              >0 => error, terminate.
C   Output in COMMON:
C      CATBLK    I         Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
      REAL      U, V, W, VIS(3,*), RESULT(3,*), RESUL2(3,*)
C
      INTEGER   NPOLN, INDEX, OFF, IS, IIF, INDEX2, NFIT, IOFF, ICHAN,
     *   NSX, NFLUXF
      REAL      SX, SXX, RSY, ISY, RSXY, ISXY, RM, RB, IM, IB, SSX, MSX,
     *   XX, SW, CATR(256)
      COMPLEX   VS, ZZ
      DOUBLE PRECISION DXC, DYC, DZC, RFREQ, DFREQ, CFREQ, RA0, DEC0,
     *   TRUEF
      LOGICAL   FLGWT, DOSHIF
      INCLUDE 'UVLSD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATR, CATBLK)
      SAVE NPOLN, NFLUXF, DXC, DYC, DZC, RFREQ, DOSHIF
C-----------------------------------------------------------------------
C                                       Set up on first call
      IF (NUMVIS.EQ.1) THEN
         NFLUXF = 0
         NPOLN = CATBLK(KINAX+JLOCS)
C                                       shift parameters
         DOSHIF = (ABS(SHIFT(1)).GT.1E-6) .OR. (ABS(SHIFT(2)).GT.1E-6)
         IF (DOSHIF) THEN
            RFREQ = CATR(KRCRP+JLOCF)
            RA0 = RA
            DEC0 = DEC
            IF (COS(DG2RAD*DEC0).NE.0.0D0) RA = RA0 + SHIFT(1) / 3600.D0
     *         / COS(DG2RAD * DEC0)
            DEC = DEC + SHIFT(2) / 3600.D0
            DXC = SIN (DG2RAD * (RA-RA0)) * COS (DEC * DG2RAD)
            DYC = COS (DEC0 * DG2RAD) * SIN (DEC * DG2RAD) -
     *         SIN (DEC0 * DG2RAD) * COS (DEC * DG2RAD) *
     *         COS ((RA - RA0) * DG2RAD)
            DZC = SIN (DG2RAD * DEC0) * SIN (DG2RAD * DEC) +
     *         COS (DG2RAD * DEC0) * COS (DG2RAD * DEC) *
     *         COS (DG2RAD * (RA - RA0)) - 1.0D0
            DXC = TWOPI * DXC
            DYC = TWOPI * DYC
            DZC = TWOPI * DZC
            END IF
         END IF
      IRET = 0
      IF (NUMVIS.GT.0) THEN
C                                       Loop over IF
         DO 400 IIF = 1,NIF
            IOFF = (IIF - 1) * NFREQ
            TRUEF = 1.0D0 + FOFF(IIF+LBIF-1) / UVFREQ
            DFREQ = FINC(IIF+LBIF-1) / UVFREQ
C                                       Loop over Stokes
            DO 350 IS = 1,NPOLN
C                                       Offset in Stokes and IF
               OFF = (IS - 1) * INCSI + (IIF - 1) * INCIFI + 1
C                                       shift phases before fit
               IF (DOSHIF) THEN
                  DO 20 ICHAN = LBCHAN,LECHAN
                     INDEX = OFF + (ICHAN-1) * INCFI
                     CFREQ = TRUEF + DFREQ * (ICHAN - RFREQ)
                     XX = (U * DXC + V * DYC + W * DZC) * CFREQ
                     ZZ = CMPLX (COS(XX), -SIN(XX))
                     VS = CMPLX (VIS(1,INDEX), VIS(2,INDEX)) * ZZ
                     VIS(1,INDEX) = REAL (VS)
                     VIS(2,INDEX) = AIMAG (VS)
 20                  CONTINUE
                  END IF
C                                       Least squares fit to real and
C                                       imaginary parts of selected
C                                       channels
               SX = 0.0
               RSY = 0.0
               ISY = 0.0
               RSXY = 0.0
               ISXY = 0.0
               SXX = 0.0
               NFIT = 0
               SW = 0.0
C
               DO 50 ICHAN = LBCHAN,LECHAN
                  IF (CHMASK(ICHAN+IOFF).GT.0) THEN
                     INDEX = OFF + (ICHAN-1) * INCFI
C                                       Compute sums
                     IF (VIS(3,INDEX).GT.0.0) THEN
                        NFIT = NFIT + 1
                        RSY = RSY + VIS(1,INDEX)
                        ISY = ISY + VIS(2,INDEX)
                        SW = SW + VIS(3,INDEX)
                        IF (NORDER.EQ.1) THEN
                           SX = SX + ICHAN
                           RSXY = RSXY + ICHAN*VIS(1,INDEX)
                           ISXY = ISXY + ICHAN*VIS(2,INDEX)
                           SXX = SXX + ICHAN*ICHAN
                           END IF
                        END IF
                     END IF
 50               CONTINUE
C                                       Compute m & b; y = mx + b
C                                       Real and imaginary separately
               RM = 0.0
               IM = 0.0
               FLGWT = .FALSE.
               IF ((NFIT.GT.1) .AND. (NORDER.EQ.1)) THEN
                  RM = (NFIT * RSXY) - (SX * RSY)
                  RM = RM / (NFIT * SXX - SX**2)
                  RB = (RSY / NFIT) - (RM * SX / NFIT)
C
                  IM = (NFIT * ISXY) - (SX * ISY)
                  IM = IM / (NFIT * SXX - SX**2)
                  IB = (ISY / NFIT) - (IM * SX / NFIT)
C                                       DC terms only
               ELSE IF ((NFIT.GT.0) .AND. (NORDER.EQ.0)) THEN
                  RB = RSY / NFIT
                  IB = ISY / NFIT
C                                       No data
               ELSE
                  RB = 0.0
                  IB = 0.0
                  SW = 0.0
                  FLGWT = .TRUE.
                  END IF
C                                       Subtract baseline from input
C                                       if could get a fit
               INDEX = OFF + (LBCHAN - 1) * INCFI
               INDEX2 =  (IS - 1) * INCSO + (IIF - 1) * INCIFO + 1
               SSX = 0.0
               MSX = 0.0
C
      INCLUDE 'INCS:ZVND.INC'
               DO 300 ICHAN = LBCHAN,LECHAN
C                                       Subtract from vis.
                  VS = CMPLX (VIS(1,INDEX), VIS(2,INDEX))
                  ZZ = CMPLX (RB + RM*ICHAN, IB + IM*ICHAN)
                  IF (CABS(ZZ).EQ.0.0) THEN
                     RESULT(3,INDEX2) = -ABS(VIS(3,INDEX))
                     RESULT(1,INDEX2) = VIS(1,INDEX)
                     RESULT(2,INDEX2) = VIS(2,INDEX)
                  ELSE
                     VS = VS / ZZ
                     RESULT(1,INDEX2) = REAL (VS)
                     RESULT(2,INDEX2) = AIMAG (VS)
                     RESULT(3,INDEX2) = VIS(3,INDEX)
                     END IF
                  IF (FLGWT) RESULT(3,INDEX2) = - ABS (VIS(3,INDEX))
                  IF ((DOFLUX) .AND. (RESULT(3,INDEX2).GT.0.0) .AND.
     *               (CHMASK(ICHAN+IOFF).GT.0)) THEN
                     SX = RESULT(1,INDEX2) * RESULT(1,INDEX2) +
     *                  RESULT(2,INDEX2) * RESULT(2,INDEX2)
                     SSX = SSX + SX
                     MSX = MAX (MSX, SX)
                     NSX = NSX + 1
                     END IF
C                                       NO shift phases back after fit
                  INDEX = INDEX + INCFI
                  INDEX2 = INDEX2 + INCFO
 300              CONTINUE
               IF (NSX.GT.0) THEN
                  SSX = SQRT (SSX / NSX)
                  MSX = SQRT (MSX)
                  IF ((SSX.GT.RFLUX) .OR. (MSX.GT.MFLUX)) THEN
                     NFLUXF = NFLUXF + 1
                     INDEX2 =  (IS - 1) * INCSO + (IIF - 1) * INCIFO + 1
                     DO 310 ICHAN = LBCHAN,LECHAN
                        RESULT(3,INDEX2) = - ABS (RESULT(3,INDEX2))
                        INDEX2 = INDEX2 + INCFO
 310                    CONTINUE
                     FLGWT = .TRUE.
                     END IF
                  END IF
               IF (DOOUT.GT.0.0) THEN
                  INDEX2 =  (IS - 1) * INCS2 + (IIF - 1) * INCIF2 + 1
                  RESUL2(1,INDEX2) = (RB + RM*CHFIT0)
                  RESUL2(2,INDEX2) = (IB + IM*CHFIT0)
                  RESUL2(3,INDEX2) = SW
                  IF (FLGWT) RESUL2(3,INDEX2) = - ABS (SW)
C                                       shift phases back after fit
                  IF (DOSHIF) THEN
                     CFREQ = TRUEF+ DFREQ * (CHFIT0 - RFREQ)
                     XX = (U * DXC + V * DYC + W * DZC) * CFREQ
                     ZZ = CMPLX (COS(XX), SIN(XX))
                     VS = CMPLX (RESUL2(1,INDEX2), RESUL2(2,INDEX2))*ZZ
                     RESUL2(1,INDEX2) = REAL (VS)
                     RESUL2(2,INDEX2) = AIMAG (VS)
                     END IF
                  END IF
 350           CONTINUE
 400        CONTINUE
C                                       last call
      ELSE
         IF (DOFLUX) THEN
            WRITE (MSGTXT,1500) NFLUXF
            IF (NFLUXF.LE.0) MSGTXT = 'No points flagged for excess'
     *         // ' residuals'
            CALL MSGWRT (6)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1500 FORMAT ('Flagged',I9,' time/IF/Stokes samples for excess',
     *   ' residuals')
      END
