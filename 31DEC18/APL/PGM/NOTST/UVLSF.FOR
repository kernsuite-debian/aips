LOCAL INCLUDE 'UVLSF.INC'
C                                       Local include for UVLSF
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XNAMOU(3),
     *   XCLAOU(2)
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH(3), XDOAC, XSOUT, XDISO, XBCHAN,
     *   XECHAN, XCHSEL(4,20), XORD, DOOUT, XCHANF, MFLUX, RFLUX,
     *   SHIFT(2), XCENT, BADD(10),
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), BUFF3(UVBFSS), FINC(MAXIF),
     *   DIFPIX
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECO,
     *   NRPRMI, NRPRMO, CHNSEL(3,20,MAXIF), LBIF, NCHSEL, CHFIT0,
     *   DISK2, SEQO2, CAT2(256), INCS2, INCF2, INCIF2, LREC2, NRPRM2,
     *   NORDER, NIF, NFREQ, LBCHAN, LECHAN, OLDCNO, NEWCNO(2), NFLUXF,
     *   ISBAND(MAXIF)
      LOGICAL   ISCOMP, DOFLUX, ADBACK
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      DOUBLE PRECISION FOFF(MAXIF)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XDOAC,
     *   XNAMOU, XCLAOU, XSOUT, XDISO, XBCHAN, XECHAN, XCHSEL, XORD,
     *   DOOUT, XCHANF, MFLUX, RFLUX, SHIFT, XCENT, BADD
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /BUFRS/ BUFF1, BUFF2, BUFF3, JBUFSZ
      COMMON /INFO/ CATOLD, CAT2, FOFF, FINC, ISBAND, SEQIN, SEQOUT,
     *   DISKIN, DISKO, ILOCWT,INCSI, INCFI, INCIFI, INCSO, INCFO,
     *   INCIFO, LRECO, NRPRMI, NRPRMO, ISCOMP, CHNSEL, NCHSEL, CHFIT0,
     *   DISK2, SEQO2, INCS2, INCF2, INCIF2, LREC2, NRPRM2, DOFLUX,
     *   NORDER, NIF, NFREQ, LBCHAN, LECHAN, OLDCNO, NEWCNO, ADBACK,
     *   NFLUXF, LBIF, DIFPIX
LOCAL END
LOCAL INCLUDE 'UVLSF2.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   JJC, CHMASK(MAXCIF), NPTS, IOFF
      REAL      DATA(2,MAXCIF), AARRAY(5,5), CARRAY(5,5), GAMMA(5),
     *   MOMENT(15), POLYFN(5), POLAVG(5), POLXFN(MAXCIF,5), XBAR
      COMMON /GDATA/ POLXFN, DATA, AARRAY, CARRAY, GAMMA, MOMENT, XBAR,
     *   POLYFN, POLAVG, JJC, CHMASK, NPTS, IOFF
LOCAL END
      PROGRAM UVLSF
C-----------------------------------------------------------------------
C! Averages several channels and subtracts from uv data.
C# Utility UV UV-util VLA VLB SPECTRAL
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2000, 2007-2012, 2014-2015, 2017-2018
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
C   UVLSF averages a set of channels and subtracts them from another
C   range of channels.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C         calibration adverbs added
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
      INTEGER   IRET
      INCLUDE 'UVLSF.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'UVLSF '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVLSFI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Filter data.
      CALL UVLSFU (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UVLSFH
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE UVLSFI (PRGN, JERR)
C-----------------------------------------------------------------------
C   UVLSFI gets input parameters for UVLSF and creates an output file
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
      INCLUDE 'UVLSF.INC'
      INCLUDE 'UVLSF2.INC'
      INTEGER   NW(MAXIF), LUN
      CHARACTER BNDCOD(MAXIF)*8
      REAL      CATR(256), RPARM(20)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
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
      NPARM = 262
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
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
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
      NORDER = IROUND (XORD)
      ADBACK = NORDER.LT.0
      NORDER = ABS (NORDER)
      NORDER = MAX (0, MIN (4, NORDER))
      JJC = NORDER + 1
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
      BCHAN = 1
      ECHAN = NFREQ
      IF ((CHFIT0.LE.0) .OR. (CHFIT0.GT.NFREQ)) CHFIT0 = IROUND
     *   (CATR(KRCRP+JLOCF))
      IF ((CHFIT0.LE.0) .OR. (CHFIT0.GT.NFREQ)) CHFIT0 = MAX (1,
     *   NFREQ/2)
      IF (DOOUT.GT.0.0) THEN
         WRITE (MSGTXT,1005) CHFIT0
         CALL MSGWRT (3)
         END IF
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
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
      NRPRMI = NRPARM
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
C                                       copy header keywords
         CALL KEYCOP (DISKIN, OLDCNO, DISK2, NEWCNO(2), IERR)
C                                       save catblk
         SEQO2 = CATBLK(KIIMS)
         CALL COPY (256, CATBLK, CAT2)
         CALL COPY (256, CATMP, CATBLK)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVLSFI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1005 FORMAT ('Continuum written at channel =',I6)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('UVLSFI: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE UVLSFU (IRET)
C-----------------------------------------------------------------------
C   UVLSFU sends uv data one point at a time to the filtering
C   routine and then writes the modified data if requested.
C   Input in common:
C      BCHAN   I  Lowest channel number to write.
C      ECHAN   I  Highest channel number to write.
C      LRECO   I  Output file record length
C      ISCOMP  L  If true data is compressed
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OFILE*48
      INTEGER   IPTRO, LUNO, INDO, LUN2, IND2, ILENBU, KBIND, NIOUT,
     *   NIOLIM, IA1, IA2, BO, VO, NUMVIS,XCOUNT, NCORO, RNXRET, NCOPY,
     *   NCOR2, NCOP2, IPTR2, NIOU2, NIOLI2, CATMP(256), NBAD
      LOGICAL   T, F
      INCLUDE 'UVLSF.INC'
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
      NBAD = 0
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
            IA2 = RPARM(1+ILOCB) + 0.1
            IA1 = IA2 / 256
            IA2 = IA2 - IA1*256
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         NUMVIS = NUMVIS + 1
C                                       Call filtering routine.
         CALL UVLSFF (NUMVIS, RPARM(1+ILOCU), RPARM(1+ILOCV),
     *      RPARM(1+ILOCW), VIS, RESULT, RESUL2, IRET)
C                                       Branch on his return
C                                       Error (fatal)
         IF (IRET.GT.0) THEN
            NBAD = NBAD + 1
C                                       Copy to output.
         ELSE IF (IRET.EQ.0) THEN
            XCOUNT = XCOUNT + 1
C                                       update NX table
            CALL RNXUPD (RPARM, RNXRET)
            RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
            RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
            RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
            CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
            IF (ISCOMP) THEN
               CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *            BUFF2(IPTRO+NRPRMO))
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
            END IF
C                                       Read next buffer.
         GO TO 100
         END IF
C                                       Final call to UVLSFF.
      NUMVIS = -1
      CALL UVLSFF (NUMVIS, DUM, DUM, DUM, BUFF1, RESULT, RESUL2, IRET)
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
      CALL UCMPRS (NVIS, DISKO, NEWCNO(1), LUNO, CATBLK, IRET)
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
      IF (NBAD.GT.0) THEN
         WRITE (MSGTXT,1200) NBAD
         CALL MSGWRT (6)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVLSFU: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1010 FORMAT ('UVLSFU: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('UVLSFU: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1100 FORMAT ('UVLSFU: ERROR',I3,' READING VIS FILE')
 1150 FORMAT ('UVLSFU: ERROR',I3,' WRITING VIS FILE AT VIS',I9)
 1200 FORMAT ('DELETED',I8,' VISIBILITIES - NOT ENOUGH VALID CHANNELS',
     *   ' TO FIT')
      END
      SUBROUTINE UVLSFH
C-----------------------------------------------------------------------
C   UVLSFH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, K, INVER, OUVER
      LOGICAL   T
      INCLUDE 'UVLSF.INC'
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
C                                       BCHAN,ECHAN
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
         MSGTXT = 'UVLSFH: ERROR COPYING TABLES TO LINE UV'
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, NEWCNO(1), DIFPIX, BUFF1, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'UVLSFH: ERROR CORRECTING FQ TABLE'
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
C                                       selected channel
         WRITE (HILINE,2010) TSKNAM, CHFIT0
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       shifts
         IF ((SHIFT(1).NE.0.0) .OR. (SHIFT(2).NE.0.0)) THEN
            WRITE (HILINE,2015) TSKNAM, SHIFT
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
C                                       CHANSEL
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
C                                       Copy tables
         CALL COPTAB (DISKIN, OLDCNO, DISK2, NEWCNO(2), IERR)
         IF (IERR.GT.2) THEN
            MSGTXT = 'UVLSFH: ERROR COPYING TABLES TO CONTINUUM UV'
            CALL MSGWRT (6)
            END IF
C                                       Update CATBLK.
         CALL CATIO ('UPDT', DISK2, NEWCNO(2), CATBLK, 'REST', BUFF1,
     *      IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVLSFH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BCHAN =',I4,', ECHAN =',I4,
     *   ' / Output channel selection')
 2010 FORMAT (A6,'CHANNEL =',I5,5X,'/ Channel at which fit evaluated')
 2015 FORMAT (A6,'SHIFT =',F10.5,' , ',F10.5,' / shifts in asec before',
     *   ' fit')
 2020 FORMAT (A6,'CHANSEL(',I2,')=',I5,',',I5,',',I2,5X,
     *   '/ channels fit IF =',I3)
 2025 FORMAT (A6,'ORDER =',I3,10X,'/ order of polynomial fit')
      END
      SUBROUTINE UVLSFF (NUMVIS, U, V, W, VIS, RESULT, RESUL2, IRET)
C-----------------------------------------------------------------------
C   Routine to fit baseline to channels, subtract and perhaps flag
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      U       R    U in wavelengths
C      V       R    V in wavelengths
C      W       R    W in wavelengths
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
C      CHNSEL(3,10) Gives channel selection
C      BCHAN   I    Lowest channel number to write.
C      ECHAN   I    Highest channel number to write.
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
C      IRET    I  Return code   1 => don't write - not could fit
C                               0 => OK
C   Output in COMMON:
C      CATBLK    I         Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
      REAL      U, V, W, VIS(3,*), RESULT(3,*), RESUL2(3,*)
C
      INTEGER   NPOLN, INDEX, OFF, IS, IIF, INDEX2, ICHAN, NSX
      REAL      SX, SSX, MSX, XX, SW, PARMS(9,2), CATR(256)
      COMPLEX   VS, ZZ
      DOUBLE PRECISION DXC, DYC, DZC, RFREQ, DFREQ, CFREQ, RA0, DEC0,
     *   TRUEF
      LOGICAL   FLGWT, DOSHIF, WASOK, SOMEOK
      INCLUDE 'UVLSF.INC'
      INCLUDE 'UVLSF2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATR, CATBLK)
      SAVE NPOLN, DXC, DYC, DZC, RFREQ, DOSHIF
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
      SOMEOK = .FALSE.
      IF (NUMVIS.GT.0) THEN
C                                       Loop over IF
         DO 400 IIF = 1,NIF
            IOFF = (IIF-1) * NFREQ
            TRUEF = 1.0D0 + FOFF(IIF+LBIF-1) / UVFREQ
            DFREQ = FINC(IIF+LBIF-1) /UVFREQ
C                                       Loop over Stokes
            DO 350 IS = 1,NPOLN
C                                       Offset in Stokes and IF
               OFF = (IS - 1) * INCSI + (IIF - 1) * INCIFI + 1
C                                       shift phases before fit
               IF (DOSHIF) THEN
                  DO 20 ICHAN = 1,NFREQ
                     INDEX = OFF + (ICHAN-1) * INCFI
                     CFREQ = TRUEF + DFREQ * (ICHAN - RFREQ)
                     XX = (U * DXC + V * DYC + W * DZC) * CFREQ
                     ZZ = CMPLX (COS(XX), -SIN(XX))
                     VS = CMPLX (VIS(1,INDEX), VIS(2,INDEX)) * ZZ
                     VIS(1,INDEX) = REAL (VS)
                     VIS(2,INDEX) = AIMAG (VS)
 20                  CONTINUE
                  END IF
C                                       move to array
               INDEX = OFF
               SW = 0.0
               DO 30 ICHAN = 1,NFREQ
                  IF (VIS(3,INDEX).GT.0) THEN
                     DATA(1,ICHAN) = VIS(1,INDEX)
                     DATA(2,ICHAN) = VIS(2,INDEX)
                     IF (CHMASK(ICHAN+IOFF).GT.0) SW = SW + VIS(3,INDEX)
                  ELSE
                     DATA(1,ICHAN) = FBLANK
                     DATA(2,ICHAN) = FBLANK
                     END IF
                  INDEX = INDEX + INCFI
 30               CONTINUE
C                                       init arrays
               CALL POLYIN (NFREQ, IRET)
               WASOK = IRET.EQ.0
C                                       solve
               IF (WASOK) THEN
                  CALL XBALMS (NFREQ, PARMS)
C                                       convert to model
                  CALL XBFUNC (NFREQ, PARMS)
                  SOMEOK = .TRUE.
                  END IF
C                                       Subtract baseline from input
C                                       if could get a fit
               INDEX = OFF + (LBCHAN - 1) * INCFI
               INDEX2 =  (IS - 1) * INCSO + (IIF - 1) * INCIFO + 1
               SSX = 0.0
               MSX = 0.0
               NSX = 0
               FLGWT = .FALSE.
C
      INCLUDE 'INCS:ZVND.INC'
               DO 300 ICHAN = LBCHAN,LECHAN
C                                       Subtract from vis.
                  IF (.NOT.WASOK) VIS(3,INDEX) = -ABS (VIS(3,INDEX))
                  IF (VIS(3,INDEX).GT.0.0) THEN
                     RESULT(1,INDEX2) = VIS(1,INDEX) - DATA(1,ICHAN)
                     RESULT(2,INDEX2) = VIS(2,INDEX) - DATA(2,ICHAN)
                     RESULT(3,INDEX2) = VIS(3,INDEX)
                  ELSE
                     RESULT(1,INDEX2) = VIS(1,INDEX)
                     RESULT(2,INDEX2) = VIS(2,INDEX)
                     RESULT(3,INDEX2) = VIS(3,INDEX)
                     END IF
                  IF ((DOFLUX) .AND. (RESULT(3,INDEX2).GT.0.0) .AND.
     *               (CHMASK(ICHAN+IOFF).GT.0)) THEN
                     SX = RESULT(1,INDEX2) * RESULT(1,INDEX2) +
     *                  RESULT(2,INDEX2) * RESULT(2,INDEX2)
                     SSX = SSX + SX
                     MSX = MAX (MSX, SX)
                     NSX = NSX + 1
                     END IF
                  IF ((ADBACK) .AND. (VIS(3,INDEX).GT.0.0)) THEN
                     RESULT(1,INDEX2) = RESULT(1,INDEX2) +
     *                  DATA(1,CHFIT0)
                     RESULT(2,INDEX2) = RESULT(2,INDEX2) +
     *                  DATA(2,CHFIT0)
                     END IF
C                                       shift phases back after fit
                  IF (DOSHIF) THEN
                     CFREQ = TRUEF + DFREQ * (ICHAN - RFREQ)
                     XX = (U * DXC + V * DYC + W * DZC) * CFREQ
                     ZZ = CMPLX (COS(XX), SIN(XX))
                     VS = CMPLX (RESULT(1,INDEX2), RESULT(2,INDEX2))*ZZ
                     RESULT(1,INDEX2) = REAL (VS)
                     RESULT(2,INDEX2) = AIMAG (VS)
                     END IF
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
                  RESUL2(1,INDEX2) = DATA(1,CHFIT0)
                  RESUL2(2,INDEX2) = DATA(2,CHFIT0)
                  RESUL2(3,INDEX2) = SW
                  IF (FLGWT) RESUL2(3,INDEX2) = - ABS (SW)
C                                       shift phases back after fit
                  IF (DOSHIF) THEN
                     CFREQ = TRUEF + DFREQ * (CHFIT0 - RFREQ)
                     XX = (U * DXC + V * DYC + W * DZC) * CFREQ
                     ZZ = CMPLX (COS(XX), SIN(XX))
                     VS = CMPLX (RESUL2(1,INDEX2), RESUL2(2,INDEX2))*ZZ
                     RESUL2(1,INDEX2) = REAL (VS)
                     RESUL2(2,INDEX2) = AIMAG (VS)
                     END IF
                  END IF
 350           CONTINUE
 400        CONTINUE
         IF (SOMEOK) THEN
            IRET = 0
         ELSE
            IRET = 1
            END IF
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
      SUBROUTINE POLYIN (NDATA, IERR)
C-----------------------------------------------------------------------
C   POLYIN prepares the parameters of a set of orthogonal polynomials.
C   All are carried in COMMON /GDATA/.
C   Input:
C      NDATA   I   Number of points in data array
C   Output:
C      IERR    I   0 ok, 1 no good data, 2 other singularity
C-----------------------------------------------------------------------
      INTEGER   NDATA, IERR
C
      REAL      PP, AL, SUM, TEMP
      INTEGER   MMAX, I, J, K, JJ, KK, N, MM
      INCLUDE 'UVLSF2.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       calculate moments
      MMAX = 2 * JJC - 1
      CALL RFILL (MMAX, 0.0, MOMENT)
      NPTS = 0
      DO 20 I = 1,NDATA
         IF ((CHMASK(I+IOFF).EQ.1) .AND. (DATA(1,I).NE.FBLANK)) THEN
            PP = 1
            AL = I + XBAR
            NPTS = NPTS + 1
            DO 15 J = 2,MMAX
               PP = PP * AL
               MOMENT(J) = MOMENT(J) + PP
 15            CONTINUE
            END IF
 20      CONTINUE
      IERR = 1
      IF (NPTS.LE.0) GO TO 999
      IERR = 2
      DO 25 J = 2,MMAX
         MOMENT(J) = MOMENT(J) / NPTS
 25      CONTINUE
      MOMENT(1) = 1.0
C                                       Matrix: P(K) = G(K) * (X**K
C                                        - SUM (A(K,J)*P(J)))
      MMAX = 25
      CALL RFILL (MMAX, 0.0, AARRAY)
      CALL RFILL (MMAX, 0.0, CARRAY)
      GAMMA(1) = 1.0
      DO 50 K = 2,JJC
         SUM = 0.0
         KK = K - 1
         DO 40 J = 1,KK
            AARRAY(K,J) = MOMENT(J+K-1)
            IF (J.GT.1) THEN
               JJ = J - 1
               DO 30 MM = 1,JJ
                  AARRAY(K,J) = AARRAY(K,J) - AARRAY(J,MM)*AARRAY(K,MM)
 30               CONTINUE
               END IF
            AARRAY(K,J) = AARRAY(K,J) * GAMMA(J)
            SUM = SUM + AARRAY(K,J) ** 2
 40         CONTINUE
         TEMP = MOMENT(2*K-1) - SUM
         IF (TEMP.LE.0.0) GO TO 999
         GAMMA(K) = 1.0 / SQRT (TEMP)
 50      CONTINUE
C                                       Matrix: P(K) = SUM (C(K,J) *
C                                                      X**J)
      CARRAY(1,1) = GAMMA(1)
      DO 65 K = 2,JJC
         CARRAY(K,K) = GAMMA(K)
         KK = K - 1
         DO 60 MM = 1,KK
            DO 55 N = MM,KK
               CARRAY(K,MM) = CARRAY(K,MM) - GAMMA(K) * AARRAY(K,N)
     *            * CARRAY(N,MM)
 55            CONTINUE
 60         CONTINUE
 65      CONTINUE
C                                       average of polynomials
      MMAX = 5
      CALL RFILL (MMAX, 0.0, POLAVG)
      DO 75 I = 1,NDATA
         CALL POLYEV (I)
         DO 70 J = 1,MMAX
            POLAVG(J) = POLAVG(J) + POLYFN(J)
            POLXFN(I,J) = POLYFN(J)
 70         CONTINUE
 75      CONTINUE
      DO 80 J = 1,MMAX
         POLAVG(J) = POLAVG(J) / NDATA
 80      CONTINUE
      IERR = 0
C
 999  RETURN
      END
      SUBROUTINE POLYEV (IX)
C-----------------------------------------------------------------------
C   POLYEV evaluates the orthogonal polynomials at the X value given.
C   Inputs:
C      IX   I   X position
C-----------------------------------------------------------------------
      INTEGER   IX
C
      REAL      AX
      INTEGER   J, K, KK
      INCLUDE 'UVLSF2.INC'
C-----------------------------------------------------------------------
      POLYFN(1) = 1.0
      AX = IX + XBAR
      DO 20 K = 2,JJC
         POLYFN(K) = AX**(K-1)
         KK = K-1
         DO 10 J = 1,KK
            POLYFN(K) = POLYFN(K) - AARRAY(K,J) * POLYFN(J)
 10         CONTINUE
         POLYFN(K) = POLYFN(K) * GAMMA(K)
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE XBALMS (INPTS, PARMS)
C-----------------------------------------------------------------------
C   XBALMS computes the answers
C   Inputs:
C      INPTS   I      Number data points
C   Output:
C      PARMS   R(9)   Answers (1 - 8), sigma ** 2 (9)
C-----------------------------------------------------------------------
      INTEGER   INPTS
      REAL      PARMS(9,2)
C
      INTEGER   I, J, K
      REAL      YBAR, YBAR2
      INCLUDE 'UVLSF2.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DO 50 K = 1,2
C                                       clear sum variables
         DO 10 I = 1,9
            PARMS(I,K) = 0.0
 10         CONTINUE
         YBAR = 0.0
         YBAR2 = 0.0
C                                       sum: data, data**2
C                                       data*polyfunc(j)
         DO 30 I = 1,INPTS
            IF ((CHMASK(I+IOFF).EQ.1) .AND. (DATA(K,I).NE.FBLANK)) THEN
               YBAR = YBAR + DATA(K,I)
               YBAR2 = YBAR2 + DATA(K,I)**2
               DO 20 J = 1,JJC
                  PARMS(J,K) = PARMS(J,K) + DATA(K,I) * POLXFN(I,J)
 20               CONTINUE
               END IF
 30         CONTINUE
C                                       average
C                                       sigma**2=ybar2-sum(parms**2)
         YBAR = YBAR / NPTS
         YBAR2 = YBAR2 / NPTS
         PARMS(9,K) = YBAR2
         DO 40 J = 1,JJC
            PARMS(J,K) = PARMS(J,K) / NPTS
            PARMS(9,K) = PARMS(9,K) - PARMS(J,K)**2
 40         CONTINUE
 50      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE XBFUNC (NDATA, PARMS)
C-----------------------------------------------------------------------
C   XBFUNC computes the model
C   Inputs:
C      NDATA   I      Number of data points in row
C      PARMS   R(9,2)   factors of orthogonal polynomials
C   Common: /GDATA/
C      DATA    R(?)   Original slice data pointsreplaced by model
C-----------------------------------------------------------------------
      INTEGER   NDATA
      REAL      PARMS(9,2)
C
      INTEGER   I, J, K
      REAL      SUM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'UVLSF2.INC'
C-----------------------------------------------------------------------
      DO 50 K = 1,2
         DO 20 I = 1,NDATA
            IF (DATA(K,I).NE.FBLANK) THEN
               SUM = 0.0
               DO 10 J = 1,JJC
                  SUM = SUM + PARMS(J,K) * POLXFN(I,J)
 10               CONTINUE
               DATA(K,I) = SUM
               END IF
 20         CONTINUE
 50      CONTINUE
C
 999  RETURN
      END
