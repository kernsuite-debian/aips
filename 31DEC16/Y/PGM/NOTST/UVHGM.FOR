LOCAL INCLUDE 'UVHGM.INC'
      REAL      XUSER, XSEQ, XDISK, XQUAL, XBAND, XFREQ, XFQID,
     *   XTIME(8), XANT(50), XBASE(50), XUVRA(2), XSUBA, XBCHAN, XECHAN,
     *   XCHINC, XBIF, XEIF, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH(3), XDOALL, XBOXS, XRANGE(2),
     *   XLABEL, XDOTV, XGRCHN, XPARMS(50)
      HOLLERITH XNAME(3), XCLASS(2), XSOUR(4,30), XCALC, XSTOK,
     *   XAXTYP(2), XOPCOD
      LOGICAL   DOLOG, DOWGT, DOTV
      INTEGER   NBINS, INDISK, CNO, UVLUN, UVIND, IVER, GRCHN, LABEL,
     *   NSUBA, NFRQ, NPARMS, CHINC
      COMMON /INPARM/ XUSER, XNAME, XCLASS, XSEQ, XDISK, XSOUR, XQUAL,
     *   XCALC, XSTOK, XBAND, XFREQ, XFQID, XTIME, XANT, XBASE, XUVRA,
     *   XSUBA, XBCHAN, XECHAN, XCHINC, XBIF, XEIF, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG,XDOBND, XBPVER, XSMOTH, XDOALL,
     *   XBOXS, XRANGE, XAXTYP, XOPCOD, XLABEL, XDOTV, XGRCHN, XPARMS
      COMMON /UVHGMP/ DOLOG, DOWGT, NBINS, INDISK, CNO, UVLUN, UVIND,
     *   IVER, DOTV, GRCHN, LABEL, NSUBA, NFRQ, NPARMS, CHINC
LOCAL END
LOCAL INCLUDE 'GDATA.INC'
      DOUBLE PRECISION DATA(32768)
      INTEGER   NITTER, ITTER
      COMMON /GDATA/ DATA, NITTER, ITTER
LOCAL END
LOCAL INCLUDE 'ANTDATA.INC'
      INTEGER   NUMAN(513), BLOFF(513)
      COMMON /ANDATA/ NUMAN, BLOFF
LOCAL END
      PROGRAM UVHGM
C-----------------------------------------------------------------------
C! Makes histograms summarizing the statistics of a UV data set.
C# Util UV Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1999, 2002-2003, 2006, 2009-2010, 2012
C;  Copyright (C) 2014-2016
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
C   UVHGM is an AIPS task which produces histograms summarizing the
C   contents of a uv data set.  The histograms are saved as PL type
C   extension files associated with the uv data.
C   Adverbs:
C      USERID       AIPS user number.
C      INNAME(3)    uv name  (12 chars).
C      INCLASS(2)   uv class ( 6 chars).
C      INSEQ        uv sequence number.
C      INDISK       uv disk.
C      STOKES       Stokes parameter (I, Q, U, V, RR, LL, RL, LR).
C      BCHAN        1st Spectral channel number.
C      ECHAN        last Spectral channel
C      BIF          1st IF band to use
C      EIF          last Spectral channel
C      DOALL        > 0 => plot unflagged AND all data on 2 curves
C                   else only unflagged data
C      NBOXES       Number of histogram bins.
C      AXTYPE(2)    Operation code in the form  'A', 'UVWR', etc.
C                   Up to 8 histograms can be produced in one go.
C                      U = u,  V = v,  W = w
C                      R = SQRT(u*u + v*v)
C                      D = SQRT(u*u + v*v + w*w)
C                      O = baseline position angle
C                      T = time
C                      B = baseline antenna pair
C                      S = source identification number
C                      F = frequency identification number
C                      H = real part of the visibility
C                      I = imaginary part
C                      A = amplitude,  P = phase, C = weight
C                      X = amplitude weighted: A/sqrt(WT)
C      OPCODE       CLIN: linear by count
C                   CLOG: log10  by count
C                   WLIN: linear by weight
C                   WLOG: log10  by weight
C-----------------------------------------------------------------------
      CHARACTER UTITLE*80, LTITLE*80, TYPE(16)*12, UNITS(16)*12,
     *   XLAB*40, YLAB*40, CHTMP8*8, NS*18
      INTEGER   HDX(16), I, IERR, IOBLK(256), K, NERR, NPLTS, NOUT(2,16)
      LOGICAL   LAST
      REAL      DOMAIN(2,16), HGMS(32768), MHZ, CATR(256)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128), PARMS(3), FVEC(32768)
      INCLUDE 'UVHGM.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATD, CATR, CATH, CATBLK)
      DATA TYPE /
     *   'U           ', 'V           ', 'W           ',
     *   'R           ', 'Baselength  ', 'Baseangle   ',
     *   'Time        ', 'Baselines   ', 'Source Id   ',
     *   'Frequency Id', 'Real        ', 'Imaginary   ',
     *   'Amplitude   ', 'Phase       ', 'Weight      ',
     *   'Amp*sqrt(wt)'/
      DATA UNITS /
     *   'Wavelengths ', 'Wavelengths ', 'Wavelengths ',
     *   'Wavelengths ', 'Wavelengths ', 'Degrees     ',
     *   'Days        ', 'Number      ', 'Number      ',
     *   'Number      ', 'Jy          ', 'Jy          ',
     *   'Jy          ', 'Degrees     ', '1/(Jy**2)   ',
     *   'Jy * (1/Jy) '/
C-----------------------------------------------------------------------
C                                       Initialize and open file.
      CALL UVHGIN (HDX, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'INITIALIZATION PROBLEM.'
         GO TO 990
         END IF
C                                       Accumulate statistics from the
C                                       uv data.
      CALL BLDHGM (HDX, DOMAIN, NOUT, HGMS, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'PROBLEM PROCESSING THE UV DATA.'
         GO TO 990
         END IF
C                                       Source name and frequency.
      CALL H2CHR (8, 1, CATH(KHOBJ), CHTMP8)
      MHZ = (CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF)
     *   * (1.0 - CATR(KRCRP+JLOCF))) / 1.0E6
      WRITE (UTITLE,1010) CHTMP8, MHZ, STOKES
C                                       Image name.
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NS)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), NS(13:18))
      CALL NAMEST (NS, CATBLK(KIIMS), UTITLE(31:), I)
      CALL REFRMT (UTITLE, '_', I)
C                                       Lower title
      LTITLE = ' '
      IF ((JLOCIF.GE.0) .AND. (CATBLK(KINAX+JLOCIF).GT.1)) THEN
         IF (CATBLK(KINAX+JLOCF).GT.1) THEN
            WRITE (LTITLE,1020) BIF, EIF, BCHAN, ECHAN
         ELSE
            WRITE (LTITLE,1021) BIF, EIF
            END IF
      ELSE
         IF (CATBLK(KINAX+JLOCF).GT.1) WRITE (LTITLE,1022) BCHAN, ECHAN
         END IF
      CALL REFRMT (LTITLE, '_', I)
      MSGTXT = 'Begin plotting of histograms'
      CALL MSGWRT (1)
C                                       Now produce the PLot files.
      NPLTS = 0
      DO 110 K = 1,16
         IF (HDX(K).GT.0) NPLTS = NPLTS + 1
 110     CONTINUE
      DO 120 K = 1,16
         IF (HDX(K).GT.0) THEN
            NPLTS = NPLTS - 1
            LAST = NPLTS.LE.0
            XLAB = TYPE(K)
C                                       Y-axis label.
            YLAB = 'LOG '
            I = 1
            IF (DOLOG) I = 5
            IF (DOWGT) THEN
               YLAB(I:) = 'Sum weight'
            ELSE
               YLAB(I:) = 'Count'
               END IF
C                                       Gaussian fitting
            IERR = -1
            IF (XDOALL.GT.0.0) CALL FITHGM (HGMS(HDX(K)), DOMAIN(1,K),
     *         PARMS, FVEC, IERR)
            IF (IERR.NE.0) PARMS(1) = 0.0D0
C                                       Do the histogram.
            CALL PLHGM (HGMS(HDX(K)), NOUT(1,K), XLAB, YLAB, UTITLE,
     *         LTITLE, UNITS(K), DOMAIN(1,K), LAST, PARMS, FVEC, IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
 120     CONTINUE
      GO TO 995
C                                       Close down files etc.
 990  CALL MSGWRT (8)
 995  CALL MAPCLS ('WRIT', INDISK, CNO, UVLUN, UVIND, CATBLK, .FALSE.,
     *   IOBLK, NERR)
      IERR = MAX (0, IERR)
      CALL DIE (IERR, IOBLK)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT (A8,1X,F10.3,' MHz__',A4)
 1020 FORMAT ('IF number',2I3,'__Spectral channel',2I5)
 1021 FORMAT ('IF number',2I3)
 1022 FORMAT ('Spectral channel',2I5)
      END
      SUBROUTINE UVHGIN (HDX, IERR)
C-----------------------------------------------------------------------
C    UVHGIN gets adverbs for UVHGM, opens the uv file, and determines
C    what is to be done.
C    Inputs:
C       HDX          I(16)   pointers into HGMS,
C       IERR         I       Error code, 0 means success.
C   Output in Common:
C       .....        R(*)    AIPS adverbs values.
C       NBINS        I       Number of histogram bins.
C       INDISK       I       uv input disk number.
C       CNO          I       uv catalog slot number.
C       UVLUN        I       uv logical unit number.
C       UVIND        I       uv buffer pointer returned by MAPOPN.
C       BCHAN        I       Frequency channel.
C       DOLOG        L       If true, use log10 bins.
C       DOWGT        L       If true, use visibility weights.
C-----------------------------------------------------------------------
      INTEGER   HDX(16), IERR
C
      CHARACTER PRGM*6, INNAME*12, INCLAS*6, OPCODE(4)*4, AXTYPE*18,
     *   TEXT*8, CHTMP4*4, PTYPE*2
      LOGICAL   NODATA, TABLE, EXIST, FITASC, MATCH
      INTEGER   I, INSEQ, IOBLK(256), IRET, IROUND, IUSER, J, K, LUN,
     *   FQVER, NIF, JERR, NHDX, LTYPE, MANT, SCRBUF(512)
      HOLLERITH CATH(256)
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'UVHGM.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'ANTDATA.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      DATA PRGM /'UVHGM '/
      DATA AXTYPE /' UVWRDOTBSFHIAPCX '/
      DATA OPCODE /'CLIN', 'CLOG', 'WLIN', 'WLOG'/
C-----------------------------------------------------------------------
C                                       Initialize parameters.
      CALL ZDCHIN (.TRUE., IOBLK)
      CALL VHDRIN
      CALL SELINI
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARMS = 271
      CALL GTPARM (PRGM, NPARMS, RQUICK, XUSER, IOBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010)
         CALL MSGWRT (8)
         RQUICK = .TRUE.
         CALL RELPOP (IERR, IOBLK, IRET)
         GO TO 999
         END IF
C                                       Restart AIPS.
      IF (RQUICK) CALL RELPOP (IERR, IOBLK, IRET)
C                                       Decode adverb values.
C                                       AIPS user number.
      XUSER = NLUSER
      IUSER = NLUSER
C                                       Input uv name etc.
      CALL H2CHR (12, 1, XNAME, INNAME)
      CALL H2CHR (6, 1, XCLASS, INCLAS)
      INSEQ  = IROUND (XSEQ)
      INDISK = IROUND (XDISK)
      CALL H2CHR (4, 1, XSTOK, STOKES)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
C                                       Number of histogram bins.
      NBINS = IROUND (XBOXS)
      IF (NBINS.LT.2)    NBINS = 128
      IF (NBINS.GT.32768) NBINS = 32768
      XBOXS = REAL(NBINS)
C                                       Histograms required.
      CALL H2CHR (8, 1, XAXTYP, TEXT)
C                                       Bin opcode.
      DOLOG = .FALSE.
      DOWGT = .FALSE.
      CALL H2CHR (4, 1, XOPCOD, CHTMP4)
      IF (CHTMP4.EQ.OPCODE(2)) THEN
         DOLOG = .TRUE.
      ELSE IF (CHTMP4.EQ.OPCODE(3)) THEN
         DOWGT = .TRUE.
      ELSE IF (CHTMP4.EQ.OPCODE(4)) THEN
         DOLOG = .TRUE.
         DOWGT = .TRUE.
      ELSE
         CALL CHR2H (4, OPCODE(1), 1, XOPCOD)
         END IF
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCHN + 0.1
      LABEL = IROUND (XLABEL)
      LTYPE = MOD (ABS(LABEL), 100)
      IF ((LTYPE.EQ.0) .OR. (LTYPE.GT.10)) LTYPE = 3
      IF (LTYPE.GT.7) LTYPE = 7
      IF ((LTYPE.GE.4) .AND. (LTYPE.LE.6)) LTYPE = 3
      IF (LABEL.LT.0) THEN
         LABEL = (LABEL/100)*100 - LTYPE
      ELSE
         LABEL = (LABEL/100)*100 + LTYPE
         END IF
C                                       Open the uv file and get its
C                                       catalog header.
      UVLUN = 56
      PTYPE = 'UV'
      CALL MAPOPN ('READ', INDISK, INNAME, INCLAS, INSEQ, PTYPE, IUSER,
     *   UVLUN, UVIND, CNO, CATBLK, IOBLK, IERR)
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
      NSUBA = MAX (1, NSUBA)
      IF (NSUBA.EQ.1) SUBARR = 1
      IF ((SUBARR.GT.0) .AND. (SUBARR.LE.NSUBA)) NSUBA = 1
      XSUBA = SUBARR
      IF (NSUBA.GT.1) XSUBA = -NSUBA
      CALL GETNAN (INDISK, CNO, CATBLK, LUN, SCRBUF, NUMAN, IERR)
      IF (IERR.EQ.0) THEN
         IF (SUBARR.GT.0) THEN
            NUMAN(2) = NUMAN(1+SUBARR)
            NUMAN(1) = 1
            BLOFF(1) = 0
            BLOFF(2) = (NUMAN(1+SUBARR) * (NUMAN(1+SUBARR) + 1)) / 2
         ELSE
            MANT = 0
            DO 52 I = 1,NUMAN(1)
               J = NUMAN(1+I)
               J = (J * (J+1)) / 2
               BLOFF(I) = MANT
               MANT = MANT + J
 52            CONTINUE
            BLOFF(1+NUMAN(1)) = MANT
            END IF
         END IF

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
               XFQID = -NFRQ
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
      IF (NFRQ.LE.1) XFQID = MAX (1, FRQSEL)
      DOACOR = .FALSE.
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
C                                       Clear the array of pointers.
      DO 100 I = 1,16
         HDX(I) = 0
 100     CONTINUE
      NHDX = 0
C                                       Construct pointers into the
C                                       histogram array.
      J = 1
      DO 140 I = 1,8
         K = INDEX (AXTYPE, TEXT(I:I)) - 1
         IF (K.GT.0) THEN
C                                       Check that the desired random
C                                       parameters are present.
            NODATA = .FALSE.
C                                       Axtype U.
            IF (K.EQ.1) THEN
               IF (ILOCU.EQ.-1) NODATA = .TRUE.
C                                       Axtype V.
            ELSE IF (K.EQ.2) THEN
               IF (ILOCV.EQ.-1) NODATA = .TRUE.
C                                       Axtype W.
            ELSE IF (K.EQ.3) THEN
               IF (ILOCW.EQ.-1) NODATA = .TRUE.
C                                       Axtype R.
            ELSE IF (K.EQ.4) THEN
               IF (ILOCU.EQ.-1) NODATA = .TRUE.
               IF (ILOCV.EQ.-1) NODATA = .TRUE.
C                                       Axtype D.
            ELSE IF (K.EQ.5) THEN
               IF (ILOCU.EQ.-1) NODATA = .TRUE.
               IF (ILOCV.EQ.-1) NODATA = .TRUE.
               IF (ILOCW.EQ.-1) NODATA = .TRUE.
C                                       Axtype P.
            ELSE IF (K.EQ.6) THEN
               IF (ILOCU.EQ.-1) NODATA = .TRUE.
               IF (ILOCV.EQ.-1) NODATA = .TRUE.
C                                       Axtype T.
            ELSE IF (K.EQ.7) THEN
               IF (ILOCT.EQ.-1) NODATA = .TRUE.
C                                       Axtype B.
            ELSE IF (K.EQ.8) THEN
               IF (ILOCB.EQ.-1) THEN
                  IF ((ILOCA1.EQ.-1) .OR. (ILOCA2.EQ.-1) .OR.
     *               (ILOCSA.EQ.-1)) NODATA = .TRUE.
                  END IF
C                                       Axtype S.
            ELSE IF (K.EQ.9) THEN
               IF (ILOCSU.EQ.-1) NODATA = .TRUE.
C                                       Axtype S.
            ELSE IF (K.EQ.10) THEN
               IF (ILOCFQ.EQ.-1) NODATA = .TRUE.
               END IF
C                                       No data for this histogram.
            IF (NODATA) THEN
               WRITE (MSGTXT,1110) TEXT(I:I)
               CALL MSGWRT (6)
C                                       No room left in HGMS.
            ELSE IF (J+NBINS.GT.32768) THEN
               WRITE (MSGTXT,1120) TEXT(I:I)
               CALL MSGWRT (6)
            ELSE
               HDX(K)  = J
               J = J + NBINS
               NHDX = NHDX + 1
               END IF
         ELSE IF (K.EQ.-1) THEN
            WRITE (MSGTXT,1130) TEXT(I:I)
            CALL MSGWRT (6)
            END IF
 140     CONTINUE
C                                       No servicable request.
      IF (NHDX.EQ.0) THEN
         WRITE (MSGTXT,1150)
         IERR = 1
         GO TO 990
         END IF
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
 1110 FORMAT ('AXTYPE ',A1,' SKIPPED - NO DATA.')
 1120 FORMAT ('AXTYPE ',A1,' SKIPPED - INSUFFICIENT STORAGE.')
 1130 FORMAT ('UNRECOGNIZED AXTYPE ',A1,' IGNORED.')
 1150 FORMAT ('UVHGIN: NO SERVICABLE REQUEST.')
      END
      SUBROUTINE GETBL (RPARM, ISUB, BL)
C-----------------------------------------------------------------------
C   computes B (baseline) value
C   Inputs:
C      RPARM   R(*)   Random parameter set
C      ISUB    I      Current subarray
C   Output:
C      BL      R      Baseline number
C-----------------------------------------------------------------------
      INTEGER   ISUB
      REAL      RPARM(*), BL
C
      INTEGER   A1, A2, N
      INCLUDE 'ANTDATA.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       antenna numbers
      IF (ILOCB.GE.0) THEN
         A2 = RPARM(1+ILOCB) + 0.1
         A1 = A2 / 256
         A2 = A2 - A1 * 256
      ELSE
         A1 = RPARM(1+ILOCA1) + 0.1
         A2 = RPARM(1+ILOCA2) + 0.1
         END IF
C                                       number antennas this subarray
      N = NUMAN(ISUB+1)
      BL = N * (A1-1) - ((A1-2) * (A1-1)) / 2 + A2 + BLOFF(ISUB)
C
 999  RETURN
      END
      SUBROUTINE BLDHGM (HDX, DOMAIN, NOUT, HGMS, IERR)
C-----------------------------------------------------------------------
C   BLDHGM does two passes through the uv data file.  Firstly to get
C   the maxima and minima of the various parameters, and second to
C   construct the histograms.
C   Inputs:
C      HDX      I(16)     pointers into HGMS,
C                         number of histograms to do,
C   Inputs from common:
C      INDISK   I         uv input disk number.
C      UVLUN    I         uv logical unit number.
C      UVIND    I         uv buffer pointer returned by MAPOPN.
C   Outputs:
C      NOUT     I(2,16)   Number samples low/high of each histogram
C      DOMAIN   R(2,16)   Range of values on the horizontal axis
C                         of each histogram.
C      HGMS     R(*)      Array containing the histograms.
C      IERR     I         Error code, 0 means success.
C-----------------------------------------------------------------------
      INTEGER   HDX(16), NOUT(2,16), IERR
      REAL      DOMAIN(2,16), HGMS(*)
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      LOGICAL   DOTHIS, REQBAS
      INTEGER   ADDRES, IBIN, IROUND, K, L, IIF, ICHAN, ISTK1, ISTK2,
     *   ISTK, JSUB, IFRQ, ISUB, NIF, NXVER, NXLUN, NANT, IANT(50),
     *   NBAS, IBAS(50), DESEL, I, J
      REAL      AMPSQ, ASQMAX, BMAX, BMIN, CNTRIB, DSQ, DSQMAX, FQMAX,
     *   FQMIN, IM, R2D, RE, RSQ, SUMAX, SUMIN, TMAX, TMIN, U, UVWMAX,
     *   V, VAR, W, WGT, WGTMAX, UVMAX, RSQMAX, WAMPMX, WAMPSQ, WASQMX,
     *   VIS(UVBFSS), RPARM(20), CATUVR(256), RBIN, REMAX, REMIN, IMMAX,
     *   IMMIN, ASQMIN
      DOUBLE PRECISION FI, FZ, FRQMUL
      INCLUDE 'UVHGM.INC'
      INCLUDE 'INCS:DSEL.INC'
      DOUBLE PRECISION FOFF(MAXIF)
      REAL      FINC(MAXIF)
      INTEGER   ISBAND(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'ANTDATA.INC'
      EQUIVALENCE (CATUV, CATUVR)
      PARAMETER (R2D = 180.0/3.14159265358)
C-----------------------------------------------------------------------
      JSUB = SUBARR
      NXVER = 1
      NXLUN = 90
C                                       Initialize baseline selection.
      CALL SETANT (50, XANT, XBASE, NANT, NBAS, IANT, IBAS, DESEL)
C                                       init maxima
      DSQMAX =  0.0
      RSQMAX =  0.0
      TMIN   =  1E30
      TMAX   = -1E30
      ASQMAX =  -1.E10
      ASQMIN = 1.E10
      WASQMX =  0.0
      WGTMAX =  0.0
      REMIN = 1.E10
      REMAX = -REMIN
      IMMIN = 1.E10
      IMMAX = -REMIN
      FQMIN = 0.0
      FQMAX = 0.1
      SUMIN = 0.0
      SUMAX = 0.1
      BMIN = 0
      BMAX = BLOFF(NSUBA+1)
      ISTK1 = 1
      ISTK2 = 0
      MSGTXT = 'Begin determination of scaling'
      IF (XRANGE(2).LE.XRANGE(1)) CALL MSGWRT (1)
C                                       range set by user
      IF (XRANGE(2).GT.XRANGE(1)) THEN
         DO 5 L = 1,16
            DOMAIN(1,L) = XRANGE(1)
            DOMAIN(2,L) = XRANGE(2)
 5          CONTINUE
C                                       range set by data
      ELSE
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
                              WAMPSQ  = AMPSQ * WGT
                              ASQMAX = MAX (ASQMAX, AMPSQ)
                              ASQMIN = MIN (ASQMIN, AMPSQ)
                              WASQMX = MAX (WASQMX, WAMPSQ)
                              WGTMAX = MAX (WGTMAX, ABS(WGT))
                              REMAX = MAX (REMAX, RE)
                              REMIN = MIN (REMIN, RE)
                              IMMAX = MAX (IMMAX, IM)
                              IMMIN = MIN (IMMIN, IM)
                              END IF
 20                        CONTINUE
 30                     CONTINUE
 40                  CONTINUE
C                                       Get maxima and minima.
                  IF (DOTHIS) THEN
                     RSQ    = (RPARM(1+ILOCU)**2 + RPARM(1+ILOCV)**2)
     *                  * FRQMUL
                     RSQMAX = MAX (RSQ, RSQMAX)
                     DSQ    = RSQ + (RPARM(1+ILOCW)**2) * FRQMUL
                     DSQMAX = MAX (DSQMAX, DSQ)
                     TMIN   = MIN (TMIN, RPARM(1+ILOCT))
                     TMAX   = MAX (TMAX, RPARM(1+ILOCT))
                     IF (ILOCSU.GE.0) THEN
                        SUMIN  = MIN (SUMIN, RPARM(1+ILOCSU))
                        SUMAX  = MAX (SUMAX, RPARM(1+ILOCSU))
                        END IF
                     IF (ILOCFQ.GE.0) THEN
                        FQMIN = MIN (FQMIN, RPARM(1+ILOCFQ))
                        FQMAX = MAX (FQMAX, RPARM(1+ILOCFQ))
                        END IF
                     END IF
                  GO TO 10
                  END IF
 50            CALL UVGET ('CLOS', RPARM, VIS, IERR)
 60            CONTINUE
 70         CONTINUE
C
         UVWMAX = SQRT (DSQMAX)
         WAMPMX = SQRT (WASQMX)
         UVMAX = SQRT (RSQMAX)
         DOMAIN(1,1)  = -UVMAX
         DOMAIN(2,1)  =  UVMAX
         DOMAIN(1,2)  = -UVMAX
         DOMAIN(2,2)  =  UVMAX
         DOMAIN(1,3)  = -UVWMAX
         DOMAIN(2,3)  =  UVWMAX
         DOMAIN(1,4)  =  0.0
         DOMAIN(2,4)  =  UVMAX
         DOMAIN(1,5)  =  0.0
         DOMAIN(2,5)  =  UVWMAX
         DOMAIN(1,6)  = -180.0
         DOMAIN(2,6)  = +180.0
         DOMAIN(1,7)  =  TMIN
         DOMAIN(2,7)  =  TMAX
         DOMAIN(1,8)  =  BMIN
         DOMAIN(2,8)  =  BMAX
         DOMAIN(1,9)  =  SUMIN
         DOMAIN(2,9)  =  SUMAX
         DOMAIN(1,10) =  FQMIN
         DOMAIN(2,10) =  FQMAX
         DOMAIN(1,11) =  REMIN
         DOMAIN(2,11) =  REMAX
         DOMAIN(1,12) =  IMMIN
         DOMAIN(2,12) =  IMMAX
         DOMAIN(1,13) =  SQRT (ASQMIN)
         DOMAIN(2,13) =  SQRT (ASQMAX)
         DOMAIN(1,14) = -180.0
         DOMAIN(2,14) = +180.0
         DOMAIN(1,15) =  0.0
         DOMAIN(2,15) =  WGTMAX
         DOMAIN(1,16) =  0.0
         DOMAIN(2,16) =  WAMPMX
         END IF
C                                       Second pass:  Accumulate data
C                                       for the histograms.
C                                       Clear the histogram storage
C                                       array.
      CALL FILL (32, 0, NOUT)
      MSGTXT = 'Begin binning of the data'
      CALL MSGWRT (1)
      CALL RFILL (32768, 0.0, HGMS)
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
                           DO 120 K = 1,16
                              IF (HDX(K).GT.0) THEN
                                 VAR = 0.0
C                                       Identify the variable.
                                 IF (K.EQ.1) THEN
                                    VAR = U
                                 ELSE IF (K.EQ.2) THEN
                                    VAR = V
                                 ELSE IF (K.EQ.3) THEN
                                    VAR = W
                                 ELSE IF (K.EQ.4) THEN
                                    VAR = SQRT (U*U + V*V)
                                 ELSE IF (K.EQ.5) THEN
                                    VAR = SQRT (U*U + V*V + W*W)
                                 ELSE IF (K.EQ.6) THEN
                                    IF ((U.NE.0.0) .OR. (V.NE.0.0))
     *                                 VAR = ATAN2 (V, U) * R2D
                                 ELSE IF (K.EQ.7) THEN
                                    VAR = RPARM(1+ILOCT)
                                 ELSE IF (K.EQ.8) THEN
                                    CALL GETBL (RPARM, ISUB, VAR)
                                 ELSE IF (K.EQ.9) THEN
                                    IF (ILOCSU.GE.0) VAR =
     *                                 RPARM(1+ILOCSU)
                                 ELSE IF (K.EQ.10) THEN
                                    IF (ILOCFQ.GE.0) VAR =
     *                                 RPARM(1+ILOCFQ)
                                 ELSE IF (K.EQ.11) THEN
                                    VAR = RE
                                 ELSE IF (K.EQ.12) THEN
                                    VAR = IM
                                 ELSE IF (K.EQ.13) THEN
                                    VAR = SQRT (RE*RE + IM*IM)
                                 ELSE IF (K.EQ.14) THEN
                                    IF ((RE.NE.0.0) .OR. (IM.NE.0.0))
     *                                 VAR = ATAN2 (IM, RE) * R2D
                                 ELSE IF (K.EQ.15) THEN
                                    VAR = WGT
                                 ELSE IF (K.EQ.16) THEN
                                    VAR = SQRT (RE*RE + IM*IM) *
     *                                 SQRT (WGT)
                                    END IF
C                                       Calculate the bin.
                                 RBIN = (NBINS-1.0) * (VAR-DOMAIN(1,K))
     *                              / (DOMAIN(2,K)-DOMAIN(1,K)) + 1.0
                                 IBIN = IROUND (RBIN)
C                                       Check for under- or overflow.
                                 IF (IBIN.LT.1) THEN
                                    NOUT(1,K) = NOUT(1,K) + 1
                                 ELSE IF (IBIN.GT.NBINS) THEN
                                    NOUT(2,K) = NOUT(2,K) + 1
                                 ELSE
C                                       Count or weight?
                                    CNTRIB = 1.0
                                    IF (DOWGT) CNTRIB = WGT
C                                       Add this contribution.
                                    IBIN = HDX(K) + IBIN - 1
                                    HGMS(IBIN) = HGMS(IBIN) + CNTRIB
                                    END IF
                                END IF
 120                         CONTINUE
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
      SUBROUTINE PLHGM (HGM1, NOUT, XLAB, YLAB, UTITLE, LTITLE, UNITS,
     *   DOMAIN, LAST, PARMS, FVEC, IERR)
C-----------------------------------------------------------------------
C   PLHGM produces a histogram as an AIPS plot file.
C   Inputs:
C      HGM1     R(*)   Histogram constructed using only good data, i.e.
C                      having non-negative weight.
C      NOUT     I(2)   Number samples low/high of plot
C      XLAB     C*40   X-axis type
C      YLAB     C*40   Y-axis label
C      UTITLE   C*80   Top title for histogram
C      LTITLE   C*80   Lower title for histogram - plot if not ' '
C      UNITS    C*12   Units on the x-axis
C      DOMAIN   R(2)   The domain of values over which the histograms
C                      are defined.
C      LAST     L      T => there are no more plots
C      PARMS    D(3)   Gaussian fit parameters
C      FVEC     D(*)   Gaussian values
C   Inputs from common:
C      INDISK   I      Input disk number.
C      CNO      I      Catalog slot number.
C      .....    R(80)  AIPS adverbs values.
C   Output to common:
C      IVER     I      The PLot file version number containing histogram
C   Outputs:
C      IERR     I      Error code, 0 means success.
C-----------------------------------------------------------------------
      INTEGER   NOUT(2), IERR
      CHARACTER XLAB*40, YLAB*40, UTITLE*80, LTITLE*80, UNITS*12
      REAL      HGM1(*), DOMAIN(2)
      LOGICAL   LAST
      DOUBLE PRECISION PARMS(3), FVEC(*)
C
      CHARACTER PFILE*48, CHT12*12, CHT6*6, CHTY*2
      LOGICAL   YES
      INTEGER   IBIN, IOBLK(256), IRET, PIND, PLUN, TVCHN, TVCORN(4)
      REAL      BMAX
      INCLUDE 'UVHGM.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      DATA YES /.TRUE./
      DATA PLUN /26/
C-----------------------------------------------------------------------
C                                       Add plot file to the image
C                                       catalog header.
      IF (.NOT.DOTV) THEN
         CHT12 = ' '
         CHT6 = ' '
         CHTY = ' '
         CALL CATDIR ('CSTA', INDISK, CNO, CHT12, CHT6, 0, CHTY, 0,
     *      'CLRD', IOBLK, IERR)
         IF (IERR.EQ.0) CALL CATDIR ('CSTA', INDISK, CNO, CHT12, CHT6,
     *      0, CHTY, 0, 'WRIT', IOBLK, IERR)
         CALL MADDEX ('PL', INDISK, CNO, CATUV, IOBLK, YES, 'WRIT',
     *      IVER, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'PLHGM: ERROR UPDATING CATALOGUE HEADER.'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
C                                       Create and initialize the plot.
C                                       Compute the maximum bin count.
      BMAX = 0
      DO 20 IBIN = 1,NBINS
         BMAX = MAX (BMAX, HGM1(IBIN))
 20      CONTINUE
C                                       Add extra info for EXTLIST.
      CALL CHR2H (40, XLAB, 1, XPARMS(1))
      CALL CHR2H (40, YLAB, 1, XPARMS(11))
      CALL CHR2H (80, UTITLE, 1, XPARMS(21))
      CALL CHR2H (12, UNITS, 1, XPARMS(41))
      XPARMS(44) = DOMAIN(1)
      XPARMS(45) = DOMAIN(2)
      XPARMS(46) = BMAX
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
C                                       Open the PLot file.
      CALL ZPHFIL ('PL', INDISK, CNO, IVER, PFILE, IERR)
      IBIN = NPARMS + 46
      CALL GINIT (INDISK, CNO, PFILE, 0, 19, IBIN, XUSER, DOTV, TVCHN,
     *   GRCHN, TVCORN, CATUV, IOBLK, PLUN, PIND, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'PLHGM: ERROR OPENING PLOT FILE FOR THE HISTOGRAM.'
         CALL MSGWRT (8)
         IF (.NOT.DOTV) CALL DELEXT ('PL', INDISK, CNO, 'WRIT', CATUV,
     *      IOBLK, IVER, IRET)
         GO TO 999
         END IF
C                                       Draw the histogram.
      CALL DRWHGM (HGM1, NOUT, XLAB, YLAB, UTITLE, LTITLE, UNITS,
     *   DOMAIN, BMAX, PARMS, FVEC, IOBLK, IERR)
C                                       Error?  Try to use it.
      IF (IERR.NE.0) THEN
         MSGTXT = 'PLHGM: ATTEMPTING TO FINISH HISTOGRAM AFTER ERROR.'
         CALL MSGWRT (8)
         END IF
      GPHPAG = .NOT.LAST
      CALL GFINIS (IOBLK, IERR)
C                                       Fatal error doing histogram.
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1070) XLAB(:10), IVER
         CALL MSGWRT (8)
C                                       Destroy the PLot file on error.
         IF (.NOT.DOTV) THEN
            CALL ZCLOSE (PLUN, PIND, IRET)
            CALL ZDESTR (INDISK, PFILE, IRET)
            CALL DELEXT ('PL', INDISK, CNO, 'WRIT', CATUV, IOBLK,
     *         IVER, IRET)
            END IF
      ELSE
         IF (.NOT.DOTV) THEN
            WRITE (MSGTXT,1075) XLAB(:10), IVER
            CALL MSGWRT (3)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('DESTROY ',A,' PLOT VERSION',I4,' DUE TO ERRORS')
 1075 FORMAT ('Successful ',A,' plot version',I4,' created')
      END
      SUBROUTINE DRWHGM (HGM1, NOUT, XLAB, YLAB, UTITLE, LTITLE, UNITS,
     *   DOMAIN, BMAX, PARMS, FVEC, IOBLK, IERR)
C-----------------------------------------------------------------------
C   DRWHGM writes commands to an open plot file to draw a histogram.
C   Inputs:
C      HGM1     R(*)     Histogram constructed using only good data,
C                        i.e. having non-negative weight.
C      NOUT     I(2)     Number samples low/high of plot
C      XLAB     C*40     X-axis type
C      YLAB     C*40     Y-axis label
C      UTITLE   C*80     Top title for histogram
C      LTITLE   C*80     Lower title for histogram - plot if not ' '
C      UNITS    C*12     Units on the x-axis
C      DOMAIN   R(2)     The domain of values over which the histograms
C                        are defined.
C      BMAX     R        Maximum bin count.
C      PARMS    D(3)     Gaussian parameter fit if > 0
C      FVEC     D(*)     Gaussian values
C   In/out:
C      IOBLK    I(256)   I/O buffer for open, initialized PL type
C                        extension file.
C   Outputs:
C      IERR     I        Error code, 0 means success.
C-----------------------------------------------------------------------
      INTEGER   NOUT(2), IOBLK(256), IERR
      REAL      HGM1(*), DOMAIN(2), BMAX
      DOUBLE PRECISION PARMS(3), FVEC(*)
      CHARACTER XLAB*40, YLAB*40, UTITLE*80, LTITLE*80, UNITS*12
C
      INTEGER   IBIN, IDEPTH(5), J, I, NCHAR, IT(3), ID(3), ITRIM, LTYPE
      REAL      BLC(2), CH(4), RANGE(2), TRC(2), X, XSCALE, XYRATO, Y1,
     *   Y2, YSCALE, X2, DX, DY, SF(2)
      LOGICAL   FLAG
      CHARACTER STRING*80, ADATE*12, ATIME*8
      INCLUDE 'UVHGM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
      IERR = 1
      IF (BMAX.LE.0.0) THEN
         WRITE (MSGTXT,1000) BMAX
         GO TO 995
         END IF
      RANGE(1) = 0.0
      RANGE(2) = BMAX
      IF (DOLOG) THEN
         RANGE(1) = LOG10(0.5)
         RANGE(2) = LOG10(RANGE(2))
         END IF
C                                       Set BLC, TRC, XYRATO.
      CALL FILL (5, 1, IDEPTH)
      BLC(1) = -1.0
      BLC(2) = -1.0
      TRC(1) = 102.0
      TRC(2) = 102.0
      XYRATO = 1.0
C                                       Set coordinate common
      LOCNUM = 1
      RPVAL(1,LOCNUM) = DOMAIN(1)
      RPVAL(2,LOCNUM) = RANGE(1)
      RPLOC(1,LOCNUM) = 1.0
      RPLOC(2,LOCNUM) = 1.0
      AXINC(1,LOCNUM) = (DOMAIN(2) - DOMAIN(1)) / 99.0
      AXINC(2,LOCNUM) = (RANGE(2) - RANGE(1)) / 99.0
      ROT(LOCNUM) = 0.0
      NCHLAB(1,LOCNUM) = 0
      NCHLAB(2,LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      CORTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 4
      AXFUNC(1,LOCNUM) = 0
      AXFUNC(2,LOCNUM) = 0
      NCHLAB(1,LOCNUM) = 0
      NCHLAB(2,LOCNUM) = 0
      CTYP(1,LOCNUM) = UNITS
      CTYP(2,LOCNUM) = YLAB
C                                       metric scaling
      DO 10 I = 1,2
         SF(I) = 1.0
         IF (CTYP(I,LOCNUM).NE.'Days') THEN
            Y1 = ABS (AXINC(I,LOCNUM) * (TRC(I) - BLC(I)))
            Y2 = Y1
            CALL METSCL (LABEL, Y2, CPREF(I,LOCNUM), FLAG)
            IF ((.NOT.FLAG) .AND. (Y1.NE.0.0)) THEN
               SF(I) = Y2 / Y1
               RPVAL(I,LOCNUM) = RPVAL(I,LOCNUM) * Y2 / Y1
               AXINC(I,LOCNUM) = AXINC(I,LOCNUM) * Y2 / Y1
               END IF
         ELSE
            CPREF(I,LOCNUM) = ' '
            END IF
 10      CONTINUE
C                                       Set character offsets.
      LTYPE = MOD (ABS (LABEL), 100)
      CALL RFILL (4, 0.5, CH)
      CALL CHNTIC (BLC, TRC, J)
      IF (LTYPE.EQ.2) CH(1) = 2.5
      IF (LTYPE.GT.2) CH(1) = J + 4.0
      IF (LTYPE.GT.1) CH(2) = 2.0
      IF (LTYPE.GT.2) CH(2) = CH(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         CH(2) = CH(2) + 1.333
         IF (LTITLE.NE.' ') CH(2) = CH(2) + 1.333
         IF ((NOUT(1).GT.0) .OR. (NOUT(2).GT.0)) CH(2) = CH(2) + 1.333
         CH(4) = 2.0
         IF (LABEL.GT.0) CH(4) = CH(4) + 1.333
         END IF
C                                       Initialize for line drawing
      CALL GINITL (BLC, TRC, XYRATO, CH, IDEPTH, IOBLK, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'DRWHGM: ERROR INITIALIZING FOR LINE DRAWING.'
         GO TO 995
         END IF
      CALL GLTYPE (1, IOBLK, IERR)
      IF (IERR.NE.0) GO TO 920
C                                       Draw borders.
      CALL GPOS (BLC(1), BLC(2), IOBLK, IERR)
      IF (IERR.NE.0) GO TO 910
      CALL GVEC (TRC(1), BLC(2), IOBLK, IERR)
      IF (IERR.NE.0) GO TO 920
      CALL GVEC (TRC(1), TRC(2), IOBLK, IERR)
      IF (IERR.NE.0) GO TO 920
      CALL GVEC (BLC(1), TRC(2), IOBLK, IERR)
      IF (IERR.NE.0) GO TO 920
      CALL GVEC (BLC(1), BLC(2), IOBLK, IERR)
      IF (IERR.NE.0) GO TO 920
C                                       Calculate range and scales.
      XSCALE = 99.0 / (NBINS - 1.0)
      YSCALE = 99.0 / (RANGE(2) - RANGE(1))
C                                       Labeling: source, freq, etc
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         DX = 0.0
         DY = 0.5
         NCHAR = ITRIM (UTITLE)
         CALL GPOS (BLC(1), TRC(2), IOBLK, IERR)
         IF (IERR.NE.0) GO TO 910
         CALL GCHAR (NCHAR, 0, DX, DY, UTITLE, IOBLK, IERR)
         IF (IERR.NE.0) GO TO 930
         END IF
C                                       Labeling: date
      IF ((LABEL.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, ATIME, ADATE)
         WRITE (STRING,1010) IVER, ADATE, ATIME
         DY = DY + 1.333
         CALL REFRMT (STRING, '_', NCHAR)
         CALL GPOS (BLC(1), TRC(2), IOBLK, IERR)
         IF (IERR.NE.0) GO TO 910
         CALL GCHAR (NCHAR, 0, DX, DY, STRING, IOBLK, IERR)
         IF (IERR.NE.0) GO TO 930
         END IF
C                                       Labeling: bin stuff
      WRITE (STRING,1011) NBINS
      CALL CHTRIM (STRING, 10, STRING, NCHAR)
      NCHAR = NCHAR + 2
      STRING(NCHAR-1:) = ' '
      I = ITRIM (XLAB)
      STRING(NCHAR:) = XLAB(:I) // ' bins of width '
      NCHAR = NCHAR + I + 15
      X = (DOMAIN(2) - DOMAIN(1)) / (NBINS - 1)
      WRITE (ADATE,1012) X
      STRING(NCHAR:) = ADATE(2:10) // ' ' // UNITS
      NCHAR = ITRIM (STRING)
      MSGTXT = 'Plot ' // STRING
      CALL MSGWRT (4)
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         DY = -1.5 - 1.333
         IF (LTYPE.GT.2) DY = DY - 1.333
         CALL GPOS (BLC(1), BLC(2), IOBLK, IERR)
         IF (IERR.NE.0) GO TO 910
         CALL GCHAR (NCHAR, 0, DX, DY, STRING, IOBLK, IERR)
         IF (IERR.NE.0) GO TO 930
C                                       samples outside
         IF ((NOUT(1).GT.0) .OR. (NOUT(2).GT.0)) THEN
            WRITE (STRING,1013) NOUT
            CALL REFRMT (STRING, '_', NCHAR)
            DY = DY - 1.333
            CALL GPOS (BLC(1), BLC(2), IOBLK, IERR)
            IF (IERR.NE.0) GO TO 910
            CALL GCHAR (NCHAR, 0, DX, DY, STRING, IOBLK, IERR)
            IF (IERR.NE.0) GO TO 930
            END IF
C                                       lower title: chan, IF
         IF (LTITLE.NE.' ') THEN
            NCHAR = ITRIM (LTITLE)
            DY = DY - 1.333
            CALL GPOS (BLC(1), BLC(2), IOBLK, IERR)
            IF (IERR.NE.0) GO TO 910
            CALL GCHAR (NCHAR, 0, DX, DY, LTITLE, IOBLK, IERR)
            IF (IERR.NE.0) GO TO 930
            END IF
         END IF
C                                       tick marks, labels, ...
      CALL CLAB1 (BLC, TRC, CH, LABEL, XYRATO, .FALSE., IOBLK, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'DRWHGM: PLOT ERROR OCCURRED WHILE DRAWING TICKS.'
         GO TO 995
         END IF
C                                       Draw the histogram
      CALL GLTYPE (2, IOBLK, IERR)
      IF (IERR.NE.0) GO TO 920
      X = 1.0 - XSCALE / 2.0
      CALL GPOS (X, 1.0, IOBLK, IERR)
      IF (IERR.NE.0) GO TO 910
      DO 30 IBIN = 1,NBINS
         Y1 = HGM1(IBIN)
         IF (DOLOG) Y1 = LOG10 (MAX (Y1, 0.5))
         Y1 = (Y1 - RANGE(1)) * YSCALE + 1.0
         CALL GVEC (X, Y1, IOBLK, IERR)
         IF (IERR.NE.0) GO TO 920
         X2 = X + XSCALE
         CALL GVEC (X2, Y1, IOBLK, IERR)
         IF (IERR.NE.0) GO TO 920
         X = X2
         IF ((XDOALL.LE.1.0) .OR. (IBIN.EQ.NBINS)) THEN
            CALL GVEC (X, 1.0, IOBLK, IERR)
            IF (IERR.NE.0) GO TO 920
            END IF
 30      CONTINUE
C                                       Draw the Gaussian
      IF (PARMS(1).GT.0.0) THEN
         CALL GLTYPE (4, IOBLK, IERR)
         IF (IERR.NE.0) GO TO 920
         X = 1.0
         DO 40 IBIN = 1,NBINS
            Y1 = -FVEC(IBIN)
            IF (DOLOG) Y1 = LOG10 (MAX (Y1, 0.5))
            Y1 = (Y1 - RANGE(1)) * YSCALE + 1.0
            IF (IBIN.EQ.1) THEN
               CALL GPOS (X, Y1, IOBLK, IERR)
            ELSE
               CALL GVEC (X, Y1, IOBLK, IERR)
               END IF
            IF (IERR.NE.0) GO TO 910
            X = X + XSCALE
 40         CONTINUE
         CALL GLTYPE (1, IOBLK, IERR)
         IF (IERR.NE.0) GO TO 920
         Y1 = SF(1) * PARMS(2)
         Y2 = ABS (Y1)
         IF (Y2.GT.1000.) THEN
            WRITE (STRING,1040) 'C =', Y1
         ELSE IF (Y2.GT.1.0) THEN
            WRITE (STRING,1041) 'C =', Y1
         ELSE IF (Y2.GT.0.01) THEN
            WRITE (STRING,1042) 'C =', Y1
         ELSE
            WRITE (STRING,1043) 'C =', Y1
            END IF
         DX = 8.0
         DY = -4.0
         CALL REFRMT (STRING, '_', NCHAR)
         CALL GPOS (BLC(1), TRC(2), IOBLK, IERR)
         IF (IERR.NE.0) GO TO 910
         CALL GCHAR (NCHAR, 0, DX, DY, STRING, IOBLK, IERR)
         IF (IERR.NE.0) GO TO 930
         Y1 = SF(1) * PARMS(3)
         Y2 = ABS (Y1)
         IF (Y2.GT.1000.) THEN
            WRITE (STRING,1040) 'RMS=', Y1
         ELSE IF (Y2.GT.1.0) THEN
            WRITE (STRING,1041) 'RMS=', Y1
         ELSE IF (Y2.GT.0.01) THEN
            WRITE (STRING,1042) 'RMS=', Y1
         ELSE
            WRITE (STRING,1043) 'RMS=', Y1
            END IF
         DX = 8.0
         DY = -6.0
         CALL REFRMT (STRING, '_', NCHAR)
         CALL GPOS (BLC(1), TRC(2), IOBLK, IERR)
         IF (IERR.NE.0) GO TO 910
         CALL GCHAR (NCHAR, 0, DX, DY, STRING, IOBLK, IERR)
         IF (IERR.NE.0) GO TO 930
         WRITE (MSGTXT,1050) PARMS(2), UNITS
         CALL MSGWRT (4)
         WRITE (MSGTXT,1051) PARMS(3), UNITS
         CALL MSGWRT (4)
         END IF
      GO TO 999
C                                       Error return from GPOS.
 910  MSGTXT = 'DRWHGM: PLOT ERROR OCCURRED WHILE MOVING TO A POINT.'
      GO TO 995
C                                       Error return from GVEC.
 920  MSGTXT = 'DRWHGM: PLOT ERROR OCCURRED WHILE DRAWING A LINE.'
      GO TO 995
C                                       error return from GCHAR
 930  MSGTXT = 'DRWHGM: PLOT ERROR OCCURRED WHILE DRAWING CHARACTERS.'
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DRWHGM: MAXIMUM BIN CONTAINED ONLY',F10.4,' COUNTS.')
 1010 FORMAT ('PLot file version',I4,'__created ',A12,A8)
 1011 FORMAT (I8)
 1012 FORMAT (1PE10.3)
 1013 FORMAT ('Samples outside plot:',I8,' below,_',I8,' above')
 1040 FORMAT (A,F7.0)
 1041 FORMAT (A,F8.3)
 1042 FORMAT (A,F8.4)
 1043 FORMAT (A,F9.6)
 1050 FORMAT ('Gaussian fit center',1PE11.4,2X,A)
 1051 FORMAT ('Gaussian fit RMS   ',1PE11.4,2X,A)
      END
      SUBROUTINE FITHGM (RHIS, DOMAIN, PARMS, FVEC, IERR)
C-----------------------------------------------------------------------
C   FITHGM fits a Gaussian to the histogram in HGM
C   Inputs
C      RHIS     R(*)   Histogram with NBINS samples
C      DOMAIN   R(2)   Range of values
C   Outputs:
c      PARMS    D(3)   Fit parameters
C      FVEC     D(*)   Gaussian values
C      IERR     I      > 0 => fit fails
C-----------------------------------------------------------------------
      REAL      RHIS(*), DOMAIN(2)
      DOUBLE PRECISION PARMS(3), FVEC(*)
      INTEGER   IERR
C
      EXTERNAL  XGFUNC
      INTEGER   I, RIMAX, RILOW, RIHIGH, IPVT(3), INFO, JNPTS, I3, I4
      REAL      RMAX, RHS, RS, RINT, RAVG, RRMS
      DOUBLE PRECISION FJAC(14,3), TOL
      INCLUDE 'UVHGM.INC'
      INCLUDE 'GDATA.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      ITTER = 0
      NITTER = 100
      RIMAX = -1
      RMAX = -1
      RHS = 0.0
      DO 10 I = 1,NBINS
         RHS = RHS + RHIS(I)
         IF (RHIS(I).GT.RMAX) THEN
            RIMAX = I
            RMAX = RHIS(I)
            END IF
 10      CONTINUE
      IF ((RIMAX.LE.2) .OR. (RIMAX.GE.NBINS-1)) THEN
         MSGTXT = 'GAUSSIAN FITS FAILS: MAX AT EDGE'
         IERR = 1
         GO TO 990
         END IF
      JNPTS = 0
      RILOW = 0
      RIHIGH = RIMAX + 1
      RS = 0.0
      RAVG = 0.0
      RRMS = 0.0
      DO 15 I = 1,NBINS
         IF (RHIS(I).GT.0.04*RMAX) THEN
            RS = RS + RHIS(I)
            RAVG = RAVG + I * RHIS(I)
            RRMS = RRMS + I * I * RHIS(I)
            END IF
 15      CONTINUE
      RAVG = RAVG / RS
      RRMS = RRMS / RS - RAVG * RAVG
      RRMS = SQRT (MAX (0.0, RRMS))
      RINT = (DOMAIN(2) - DOMAIN(1)) / (NBINS-1.0)
      RAVG = (RAVG - 1.0) * RINT + DOMAIN(1)
      RRMS = RRMS * RINT
      WRITE (MSGTXT,1015) RAVG, RRMS
C     CALL MSGWRT (4)
      RMAX = 0.3 * RMAX
      DO 20 I = 1,NBINS
         I3 = MIN (RIMAX + I, NBINS)
         I4 = MAX (RIMAX - I, 2)
         IF ((RHIS(I3).LT.RMAX) .AND. (RHIS(I4).LT.RMAX)) GO TO 30
         IF (RHIS(I4).GE.RMAX) RILOW = I4
         IF (RHIS(I3).GE.RMAX) RIHIGH = I3
 20      CONTINUE
      IF (RIHIGH-RILOW.GT.700) THEN
         MSGTXT = 'HISTOGRAM TOO WIDE FOR GAUSSIAN FITTER'
         IERR = 2
         GO TO 990
         END IF
 30   RS = 0.0
      DO 40 I = RILOW,RIHIGH
         JNPTS = JNPTS + 1
         DATA(JNPTS) = RHIS(I)
         RS = RS + RHIS(I)
 40      CONTINUE
      IF (JNPTS.LT.4) THEN
         MSGTXT = 'HISTOGRAM TOO NARROW TO GAUSSIAN FIT'
         IERR = 3
         GO TO 990
         END IF
      IF (RS.LT.0.1*RHS) THEN
         MSGTXT = 'HISTOGRAM CONTAINS TOO SMALL A FRACTION OF THE DATA'
         IERR = 4
         GO TO 990
         END IF
C                                       restore max and other initial
      PARMS(1) = RMAX / 0.3
      PARMS(2) = RIMAX - RILOW + 1
      PARMS(3) = 1.3 * (RIHIGH - RILOW + 1.0) / 2.0
C                                       call fitting routine
      I = 1
      CALL XGFUNC (JNPTS, 3, PARMS, FVEC, FJAC, I)
      TOL = 1.D-5
      I = 3
      CALL XGALMS (XGFUNC, JNPTS, I, PARMS, FVEC, FJAC, 14, TOL,
     *   INFO, IPVT)
      IF ((INFO.LE.0) .OR. (INFO.GT.3)) THEN
         WRITE (MSGTXT,1040) INFO
         IF (INFO.EQ.4) MSGTXT = 'GAUSSIAN FITTING ROUTINES' //
     *      ' THINK THE HISTOGRAM IS ORTHOGONAL'
         CALL MSGWRT (6)
         PARMS(2) = 1.0 + (RAVG - DOMAIN(1)) / RINT
         PARMS(3) = RRMS * SQRT (8.0 * LOG (2.0)) / RINT
      ELSE
         PARMS(2) = PARMS(2) - 1.0 + RILOW
         END IF
      IERR = 0
C                                       get Gaussian
      DO 45 I = 1,NBINS
         DATA(I) = 0.0D0
 45      CONTINUE
      I = 1
      CALL XGFUNC (NBINS, 3, PARMS, FVEC, FJAC, I)
      RINT = (DOMAIN(2) - DOMAIN(1)) / (NBINS-1.0)
      PARMS(2) = (PARMS(2) - 1.0) * RINT + DOMAIN(1)
      PARMS(3) = PARMS(3) * RINT / SQRT (8.0 * LOG (2.0))
      GO TO 999
C
 990  CALL MSGWRT (8)
c
 999  RETURN
C-----------------------------------------------------------------------
 1015 FORMAT ('Simple average, rms',2(1PE12.4))
 1040 FORMAT ('GAUSSIAN FITTING RETURNS INFO CODE',I3,
     *   ' using ordinary mean/rms')
      END
      SUBROUTINE XGALMS (FCN, M, N, X, FVEC, FJAC, LDFJAC, TOL, INFO,
     *   IPVT)
C-----------------------------------------------------------------------
C   XGALMS provides an extra interface to the math routine LMSTR1
C   and holds the WORK array (for overlay purposes)
C   Inputs:
c      FCN      EXT      Function to evaluate the model
C      M        I        Number data points (adj. array dim.)
C      LDFJAC   I        Number points on first axis of FJAC (adj.
C                        array dim.)
C      TOL      D        Tolerance desired
C   In/out:
c      X        D(N)     Initial guess/ answer
C      FVEC     D(M)     Function (Data - model) evaluation
C      FJAC     D(N,N)
C      INFO     I        Error code: 1 - 3 good, 0 bad input,
C                           4 orthogonal, 5 - 7 poor fit
C      IPVT     D(N)
C   See precursor remarks to LMSTR1 or LMSTR for details.
C   This version hard codes N = 3 parameters.
C-----------------------------------------------------------------------
      EXTERNAL  FCN
      INTEGER   M, N, LDFJAC, LWA
      INTEGER   INFO, IPVT(N)
      DOUBLE PRECISION    TOL, X(N), FVEC(N), FJAC(LDFJAC,N), WA(920)
      DATA LWA /920/
C-----------------------------------------------------------------------
C                                       It's just a dummy routine
      N = 3
      CALL LMSTR1 (FCN, M, N, X, FVEC, FJAC, LDFJAC, TOL, INFO, IPVT,
     *   WA, LWA)
C
 999  RETURN
      END
      SUBROUTINE XGFUNC (M, N, PARMS, FVEC, FJROW, IFLAG)
C-----------------------------------------------------------------------
C   This routine is called by the Argonne package to calculate the
C   difference between the current fit and the actual data OR the
C   Jacobian for this difference.
C   Inputs:
C      M       I      Number of data points in slice
C      N       I      No. of parameters (assumed 3 here)
C      PARMS   D(N)   parameters of gaussian components,
C                     GMAX, GPOS, GWIDTH
C      IFLAG   I      1 = calculate difference for current guess.
C                     > 1  IFLAG-1 is the position in the array to
C                          evaluate the Jacobian
C   COMMON /GDATA/:
C      DATA    R(*)   Original slice data points.
C      ITTER   I      Number of calls to evaluate FVEC.
C   Outputs:
C      FVEC    D(M)   Slice data points minus data points evaluated for
C                     current guess.
C      FJROW   D(N)   Row (IFLAG - 1) of Jacobian.
C-----------------------------------------------------------------------
      INTEGER   N, M, IFLAG
      DOUBLE PRECISION PARMS(N), FVEC(M), FJROW(N)
C
      DOUBLE PRECISION AMP, POS, SIG, EFACT, RES2, TSIG2, X, HALFAC
      INTEGER   IDATA
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'GDATA.INC'
      DATA HALFAC /2.77258872D0/
C-----------------------------------------------------------------------
C                                       Determine difference between
C                                       data and current fit.
      IF (IFLAG.LE.1) THEN
         ITTER = ITTER + 1
         IF (ITTER.GT.NITTER) THEN
            IFLAG = -1
            GO TO 999
            END IF
         DO 20 IDATA = 1,M
            FVEC(IDATA) = DATA(IDATA)
            IF (FVEC(IDATA).EQ.FBLANK) THEN
               FVEC(IDATA) = 0.0D0
            ELSE
               X = IDATA
               AMP = PARMS(1)
               IF (AMP.GT.0.0D0) THEN
                  POS = PARMS(2)
                  SIG = PARMS(3)
                  RES2 = (X - POS) / SIG
                  RES2 = HALFAC * RES2 * RES2
                  IF (RES2.LE.69.0D0) FVEC(IDATA) = FVEC(IDATA) -
     *               AMP * EXP (-RES2)
                  END IF
               END IF
 20         CONTINUE
C                                       Calculate Jacobian.
      ELSE
         IDATA = IFLAG - 1
         X = IDATA
         FJROW(1) = 0.0D0
         FJROW(2) = 0.0D0
         FJROW(3) = 0.0D0
         AMP = PARMS(1)
         POS = PARMS(2)
         SIG = PARMS(3)
         RES2 = HALFAC * (X - POS) * (X - POS)
         TSIG2 = RES2 / (SIG * SIG)
         IF (TSIG2.LE.69.0D0) THEN
            EFACT = -EXP (-TSIG2)
            FJROW(1) = EFACT
            EFACT = 2.0D0 * EFACT * AMP / (SIG * SIG)
            FJROW(2) = HALFAC * EFACT * (X-POS)
            FJROW(3) = EFACT * RES2 / SIG
            END IF
         END IF
C
 999  RETURN
      END
