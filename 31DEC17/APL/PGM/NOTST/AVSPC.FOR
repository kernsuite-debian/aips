LOCAL INCLUDE 'AVSPC.INC'
C                                       Local include for AVSPC
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XNAMOU(3),
     *   XCLAOU(2), XAVOPT, XXSTOK
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH(3), XDOAC, XSOUT, XDISO,
     *   XCHNS(4,20), XCHANL, BUFF2(UVBFSS), AVGBW, AVGCH(MAXIF),
     *   REFOLD, RPARM(20)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, NW(MAXIF),
     *   CHNSEL(3,20,MAXIF), OLDCNO, NEWCNO, CNTCHN(MAXIF), ILOCWT,
     *   NOUTCH, NCHAVG, NINCH, INCSO, INCFO, INCIFO, LRECO,
     *   NRPRMI, NRPRMO, SCRBUF(256)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, AVOPT*4
      LOGICAL   ISCOMP
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XXSTOK, XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH,
     *   XDOAC, XNAMOU, XCLAOU, XSOUT, XDISO, XCHNS, XAVOPT, XCHANL
      COMMON /OTHERS/ AVGBW, AVGCH,SEQIN, SEQOUT, DISKIN, DISKO, CHNSEL,
     *   NW, OLDCNO, NEWCNO, CNTCHN, NINCH, NOUTCH, NCHAVG, REFOLD,
     *   ISCOMP, INCSO, INCFO, INCIFO, LRECO, NRPRMI, NRPRMO,
     *   ILOCWT
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, AVOPT
      COMMON /BUFRS/ SCRBUF, RPARM, BUFF2, JBUFSZ
C                                       Input CATBLK
      INTEGER   CATIN(256)
      REAL      CATRI(256)
      HOLLERITH CATHI(256)
      DOUBLE PRECISION CATDI(128)
      COMMON /MAPIN/ CATIN
      EQUIVALENCE (CATIN, CATRI, CATHI, CATDI)
C                                       Sideband information
      DOUBLE PRECISION FOFF(MAXIF)
      REAL    FINC(MAXIF)
      INTEGER ISBAND(MAXIF)
      COMMON /SIDEB/ FOFF, FINC, ISBAND
LOCAL END
      PROGRAM AVSPC
C-----------------------------------------------------------------------
C! Task to average uv data in frequency.
C# Utility UV UV-util VLA VLB Spectral Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998-2001, 2004, 2006-2007, 2009-2015, 2017
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
C   AVSPC allows a user to average in frequency.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      ICHANSEL(4,20) CHNSEL        Array of start,stop,incr channel
C                                   numbers for selection.
C      AVOPTION       AVOPT         Averaging option
C      BIF            BIF           Lowest IF in average
C      EIF            EIF           Hoghest IF number in average.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'AVSPC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'AVSPC '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL AVGSIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL DOAVG (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL AVGHIS
C                                       Ensure SU/FQ tables have
C                                       correct bandwidth values
      CALL SUMOD (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL FQMOD (BIF, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE AVGSIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   AVGSIN gets input parameters for AVSPC and creates an output file.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   NPARM, IERR, IROUND, I, J, IVER, FQLUN, NIF, LUN, INCX,
     *   K, NC, SUMCHN, K1, K2
      LOGICAL   MATCH
      REAL      CHWT, CATR(256)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      LOGICAL   T
      INCLUDE 'AVSPC.INC'
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
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
      NPARM = 245
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, SCRBUF, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XAVOPT, AVOPT)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      DO 5 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 5       CONTINUE
      SELQUA = IROUND (XQUAL)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOACOR = XDOAC.GT.0.0
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
     *   PTYPE, NLUSER, STAT, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Copy catblk to CATIN
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      CALL COPY (256, CATBLK, CATIN)
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
      BCHAN = 1
      ECHAN = CATBLK(KINAX+JLOCF)
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
C                                       Channel selection
      I = 60 * MAXIF
      CALL FILL (I, 0, CHNSEL)
      CALL FILL (MAXIF, 0, NW)
      DO 20 J = 1,20
         K = IROUND (XCHNS(2,J))
         IF (K.LE.0) GO TO 25
         K = IROUND (XCHNS(4,J))
         IF ((K.LE.0) .OR. (K.GT.MAXIF)) THEN
            K1 = 1
            K2 = MAXIF
         ELSE
            K1 = K
            K2 = K
            END IF
         DO 15 K = K1,K2
            NW(K) = NW(K) + 1
            DO 10 I = 1,3
               CHNSEL(I,NW(K),K) = IROUND (XCHNS(I,J))
               IF (CHNSEL(I,NW(K),K).LT.0) CHNSEL(I,NW(K),K) = 0
 10            CONTINUE
            IF (CHNSEL(3,NW(K),K).EQ.0) CHNSEL(3,NW(K),K) = 1
 15         CONTINUE
 20      CONTINUE
C                                       Create new file.
C                                       Check ratio of input/output chns
 25   NINCH = CATBLK(KINAX+JLOCF)
      NOUTCH = 1
C
      IF (AVOPT.EQ.'SUBS') THEN
         NCHAVG = IROUND (XCHANL)
         NOUTCH = NINCH / NCHAVG
         IF (NOUTCH.LT.1) THEN
            NOUTCH = 1
            AVOPT = ' '
            MSGTXT = 'AVOPTION CHANGED TO BLANK, CHANNEL > NINCH'
            CALL MSGWRT (8)
         ELSE
            I = NOUTCH * NCHAVG
            IF (I.LT.NINCH) THEN
               J = NINCH - I
               WRITE (MSGTXT,1010) J
               CALL MSGWRT (4)
               MSGTXT = 'the input spectrum furthest from the LO'
               CALL MSGWRT (4)
               END IF
            WRITE (MSGTXT,1025) NOUTCH
            CALL MSGWRT (4)
            END IF
         END IF
C                                       Get sideband info
      IVER = 1
      FQLUN = 30
      MSGSUP = 32000
      CALL CHNDAT ('READ', SCRBUF, DISKIN, OLDCNO, IVER, CATIN,
     *   FQLUN, NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
      MSGSUP = 0
C                                       Note if packed data
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       If no channel selection
C                                       use VLA definition of
C                                       channel 0
      DO 50 K = 1,MAXIF
         IF (AVOPT.EQ.'SUBS') THEN
            NW(K) = 1
            CHNSEL(1,1,K) = 1
            CHNSEL(2,1,K) = NOUTCH * NCHAVG
            CHNSEL(3,1,K) = 1
         ELSE IF (NW(K).LE.0) THEN
            NW(K) = 1
            CHNSEL(1,1,K) = (NINCH+1)/8 + 1
            CHNSEL(2,1,K) = NINCH - ((NINCH+1)/8)
            CHNSEL(3,1,K) = 1
            END IF
         DO 30 I = 1,NW(K)
            CHNSEL(1,I,K) = MAX (1, MIN (CHNSEL(1,I,K), NINCH))
            IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K))
     *         CHNSEL(2,I,K) = NINCH
            CHNSEL(2,I,K) = MAX (1, MIN (CHNSEL(2,I,K), NINCH))
 30         CONTINUE
C                                       Find average channel number
         SUMCHN = 0
         NC = 0
         DO 40 I = 1,NINCH
            CHWT = 1.0
            CALL WANTCH (CHNSEL(1,1,K), I, CHWT)
            IF (CHWT.GT.0.0) THEN
               SUMCHN = SUMCHN + I
               NC = NC + 1
               END IF
 40         CONTINUE
         IF (NC.GT.0) THEN
            AVGCH(K) = SUMCHN
            AVGCH(K) = AVGCH(K) / NC
         ELSE
            AVGCH(K) = 1.0
            END IF
         CNTCHN(K) = NC
         IF (AVOPT.EQ.'SUBS') CNTCHN(K) = NCHAVG
 50      CONTINUE
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, BUFF2, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, BUFF2, IERR)
C                                       Save input file info
      NRPRMI = NRPARM
C                                       read compressed => write compr.
      IF (ISCOMP) THEN
         CATBLK(KINAX) = 1
         I = CATBLK(KIPCN)
         CALL CHR2H (8, 'WEIGHT  ', 1, CATBLK(KHPTP+2*I))
         CALL CHR2H (8, 'SCALE   ', 1, CATBLK(KHPTP+2*I+2))
         CATBLK(KIPCN) = I + 2
         ILOCWT = I
         END IF
C                                       CATBLK represents output file
      CATBLK(KINAX+JLOCF) = NOUTCH
      IF (NAMOUT.EQ.' ') NAMOUT = NAMEIN
      IF (CLAOUT.EQ.' ') THEN
         IF (AVOPT.EQ.'SUBS') THEN
            CLAOUT = 'SUB SP'
         ELSE
            CLAOUT = 'CH.0'
            END IF
         END IF
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      NEWCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
C                                       Ensure correct size output
C                                       file is created
      CALL UVCREA (DISKO, NEWCNO, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
         IF ((NEWCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN)) THEN
            MSGTXT = 'MAY OVERWRITE INPUT FILE --- QUITTING'
            GO TO 990
            END IF
C                                       Recover existing CATBLK
         FRW(NCFILE+1) = 2
         CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'WRIT', SCRBUF,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1065) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                       Save output file info
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      INCX = CATBLK(KINAX)
      LRECO = LREC
      NRPRMO = NRPARM
      INCSO = INCS / INCX * 3
      INCFO = INCF / INCX * 3
      INCIFO = INCIF / INCX * 3
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', SCRBUF, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       Copy any header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AVGSIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('Will reject ',I3,' channels from the end of')
 1025 FORMAT ('Output file will have ',I4,' channels/IF')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('AVGSIN: UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1065 FORMAT ('AVGSIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE DOAVG (IRET)
C-----------------------------------------------------------------------
C   DOAVG will average the data in frequency according to the
C   selection criteria specified by the user.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OFILE*48, TELTYP*4
      INTEGER   IPTRO, LUNO, INDO, RNXRET, ILENBU, KBIND, NIOUT, NIOLIM,
     *   BO, VO, NUMVIS, XCOUNT, NFRQ, NIF, NPOL, SCHAN, LCHAN,
     *   MXIF, CATMP(256), NCOPY, NCORO
      REAL      RESULT(3,MAXCIF), VIS(3,MAXCIF), REF, INC, FAV, TMP,
     *   CATR(256)
      LOGICAL   T, F, DOAVSP
      DOUBLE PRECISION OLDFRQ, CATD(128), UVSCAL
      HOLLERITH CATH(256)
      INCLUDE 'AVSPC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA LUNO /17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
      DATA MXIF /MAXIF/
C-----------------------------------------------------------------------
C                                       Number of visibilities in input
C                                       and output files.
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, NEWCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Set up useful values
      NFRQ = CATBLK(KINAX+JLOCF)
      NIF = 1
      IF (JLOCIF.GT.0) NIF = CATBLK(KINAX+JLOCIF)
      NPOL = CATBLK(KINAX+JLOCS)
C                                       Check array sizes
      IF (NPOL*NIF*NFRQ.GT.MAXCIF) THEN
         WRITE (MSGTXT,1017) NIF, NFRQ, NPOL, MAXCIF
         IRET = 6
         GO TO 990
         END IF
      DOAVSP = AVOPT.NE.'SUBS'
      CALL H2CHR (4, 1, CATH(KHTEL), TELTYP)
C                                       Modify increment on freq.
C                                       axis to reflect averaging.
C                                       Also change ref pixel to
C                                       reflect extrema of channels
      REF = CATR(KRCRP+JLOCF)
      REFOLD = REF
      OLDFRQ = CATD(KDCRV+JLOCF)
      INC = CATR(KRCIC+JLOCF)
      FAV = (AVGCH(BIF) - REF)*INC + OLDFRQ
      IF (AVOPT.EQ.'SUBS') THEN
         CATR(KRCIC+JLOCF) = CATR(KRCIC+JLOCF) * NCHAVG
      ELSE
         CATR(KRCIC+JLOCF) = CATR(KRCIC+JLOCF) * CNTCHN(BIF)
         END IF
      AVGBW = CATR(KRCIC+JLOCF)
      IF (AVOPT.EQ.'SUBS') THEN
         TMP = NCHAVG
         CATR(KRCRP+JLOCF) = ((CATR(KRCRP+JLOCF) - 1.0 -
     *      ((TMP-1.0)/2.0)) / TMP) + 1.0
         CATR(KRARP) = ((CATR(KRARP) - 1.0 -
     *      ((TMP-1.0)/2.0)) / TMP) + 1.0
      ELSE
         REF = 1.0 - (FAV - OLDFRQ)/AVGBW
         CATR(KRCRP+JLOCF) = REF
         REF = 1.0 - (AVGCH(BIF)-CATR(KRARP)) / CNTCHN(BIF)
         CATR(KRARP) = REF
         END IF
      SCHAN = 1
      LCHAN = CATIN(KINAX+JLOCF)
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
      NUMVIS = 0
      XCOUNT = 0
C                                       make an index table
      CALL RNXGET (DISKIN, OLDCNO, CATIN)
      CALL RNXINI (DISKO, NEWCNO, CATBLK, RNXRET)
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
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         NUMVIS = NUMVIS + 1
         RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
         RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
         RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
C                                       average
         IF (DOAVSP) THEN
            CALL AVGCHN (VIS, NPOL, BCHAN, ECHAN, BIF, EIF, CHNSEL,
     *         INCSO, INCIFO, RESULT)
C                                       Average spectral chunks
         ELSE
            CALL AVGSUB (VIS, NPOL, NIF, NCHAVG, NOUTCH, INCSO,
     *         INCIFO, INCFO, RESULT)
            END IF
         XCOUNT = XCOUNT + 1
         CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
C                                       update NX table
         CALL RNXUPD (RPARM, RNXRET)
         IF (ISCOMP) THEN
            CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *         BUFF2(IPTRO+NRPRMO))
         ELSE
            CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
            END IF
         IPTRO = IPTRO + LRECO
         NIOUT = NIOUT + 1
C                                       Write vis record.
         IF (NIOUT.GE.NIOLIM) THEN
            CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1150) IRET
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
            END IF
C                                       read next record
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
      CALL UCMPRS (NVIS, DISKO, NEWCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      CALL RNXCLS (RNXRET)
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
 1000 FORMAT ('DOAVG: ERROR',I5,' FROM UVGET ON INIT')
 1010 FORMAT ('DOAVG: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1017 FORMAT ('DOAVG: NUMBER IFS, CHANS, POLS',I4,I6,I2,
     *   ' EXCEEDS BUFFER',I8)
 1020 FORMAT ('DOAVG: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1100 FORMAT ('DOAVG: ERROR',I3,' READING VIS FILE')
 1150 FORMAT ('DOAVG: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE AVGHIS
C-----------------------------------------------------------------------
C   AVGHIS copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, J, K
      LOGICAL   T
      INCLUDE 'AVSPC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   SCRBUF, BUFF2, IERR)
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
C                                       calibration history
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Sub spectrum
      IF (AVOPT.EQ.'SUBS') THEN
         WRITE (HILINE,1040) TSKNAM, AVOPT, NCHAVG, NOUTCH
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       CHNSEL
      ELSE
         DO 20 K = BIF,EIF
            WRITE (HILINE,1010) TSKNAM, K
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 100
            DO 10 I = 1,NW(K)
               WRITE (HILINE,1020) TSKNAM, (CHNSEL(J,I,K),J = 1,3), K
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 100
 10            CONTINUE
 20         CONTINUE
         END IF
C                                       Close HI file
 100  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                       Copy tables - local version
      CALL COPTAB (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'AVGHIS: ERROR COPYING TABLES TO OUTPUT UV'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AVGHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'IF=',I3,'  CHANSEL = ')
 1020 FORMAT (A6,3X,3(I6,1X),'/ IF',I3,' Avgd: Start, Stop, Inc')
 1040 FORMAT (A6,'AVOPT=''',A,''' Chns avgd = ',I4,' # o/p chns = ',I4)
      END
      SUBROUTINE SUMOD (IRET)
C-----------------------------------------------------------------------
C   Routine to modify the SU table as it's passed from the input file to
C   the output to ensure that the bandwidth column is correct.
C   Output:
C      IRET   I   Error code, 0 => OK, >0 => error
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VELTYP*8, VELDEF*8, CALCOD*4, SOUNAM*16
      REAL      SUKOLS(MAXSUC), SUNUMV(MAXSUC), FLUX(4,MAXIF),
     *   OKOLS(MAXSUC), ONUMV(MAXSUC)
      INTEGER   LUN1, LUN2, ISURNI, ISURNO, I, NROW, IDSOU, QUAL, NUMIF,
     *   JERR, IVER, OVER, BUFFI(512), BUFFO(512), SUFQID
      LOGICAL   TABLE, EXIST, FITASC
      DOUBLE PRECISION BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, PMRA,
     *   PMDEC, RAOBS, DECOBS, FREQO(MAXIF), LSRVEL(MAXIF),
     *   LRESTF(MAXIF)
      INCLUDE 'AVSPC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27, 28/
C-----------------------------------------------------------------------
      IRET = 0
      IVER = 1
      CALL ISTAB ('SU', DISKO, NEWCNO, IVER, LUN1, BUFFI, TABLE, EXIST,
     *   FITASC, JERR)
      IF (.NOT.EXIST) GO TO 999
C                                       Open input SU table
      CALL SOUINI ('READ', BUFFI, DISKO, NEWCNO, IVER, CATBLK, LUN1,
     *   NUMIF, VELTYP, VELDEF, SUFQID, ISURNI, SUKOLS, SUNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Open  output SU table
      OVER = 1
      IF (SUFQID.EQ.-999) SUFQID = -1
      CALL SOUINI ('WRIT', BUFFO, DISKO, NEWCNO, OVER, CATBLK, LUN2,
     *   NUMIF, VELTYP, VELDEF, SUFQID, ISURNO, OKOLS, ONUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Number of rows
      NROW = BUFFI(5)
      DO 100 I = 1,NROW
         CALL TABSOU ('READ', BUFFI, ISURNI, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
C
         BANDW = AVGBW
         ISURNO = I
         CALL TABSOU ('WRIT', BUFFO, ISURNO, OKOLS, ONUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
 100     CONTINUE
C                                       Close the tables
      CALL TABIO ('CLOS', 0, ISURNI, BUFFI, BUFFI, IRET)
      CALL TABIO ('CLOS', 0, ISURNO, BUFFO, BUFFO, IRET)
      WRITE (MSGTXT,1040) DISKO, NEWCNO, OVER
      CALL MSGWRT (4)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUMOD: ERROR ',I3,' OPENING INPUT SU TABLE')
 1010 FORMAT ('SUMOD: ERROR ',I3,' OPENING OUTPUT SU TABLE')
 1020 FORMAT ('SUMOD: ERROR ',I3,' READING INPUT SU TABLE')
 1030 FORMAT ('SUMOD: ERROR ',I3,' WRITING OUTPUT SU TABLE')
 1040 FORMAT ('Updated SU table bandwidth at vol/cno/vers',I3,I5,I4)
      END
      SUBROUTINE FQMOD (BIF, IRET)
C-----------------------------------------------------------------------
C   Routine to modify the FQ table as it's passed from the input file to
C   the output to ensure that the bandwidth column and frequencies are
C   correct.
C   Output:
C      IRET   I   Error code, 0 => OK, >0 => error
C-----------------------------------------------------------------------
      INTEGER   BIF, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      REAL      FQKOLS(MAXFQC), FQNUMV(MAXFQC), IFCHW(MAXIF),
     *   IFTBW(MAXIF), OKOLS(MAXFQC), ONUMV(MAXFQC)
      INTEGER   LUN1, LUN2, IFQRNI, IFQRNO, I, NROW, NUMIF, OVER, NOIF,
     *   JERR, IFSIDE(MAXIF), FQID, J, IVER, BUFFI(512), BUFFO(512), K
      LOGICAL   TABLE, EXIST, FITASC
      DOUBLE PRECISION IFFREQ(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'AVSPC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27, 28/
C-----------------------------------------------------------------------
      IRET = 0
      IVER = 1
      CALL ISTAB ('FQ', DISKO, NEWCNO, IVER, LUN1, BUFFI, TABLE, EXIST,
     *   FITASC, JERR)
      IF (.NOT.EXIST) GO TO 999
C                                       Open input FQ table
      CALL FQINI ('READ', BUFFI, DISKO, NEWCNO, IVER, CATBLK, LUN1,
     *   IFQRNI, FQKOLS, FQNUMV, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      NOIF = NUMIF
C                                       Open  output FQ table
      OVER = 1
      CALL FQINI ('WRIT', BUFFO, DISKO, NEWCNO, OVER, CATBLK, LUN2,
     *   IFQRNO, OKOLS, ONUMV, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Number of rows
      NROW = BUFFI(5)
      DO 100 I = 1,NROW
         CALL TABFQ ('READ', BUFFI, IFQRNI, FQKOLS, FQNUMV, NUMIF,
     *      FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
C                                       Correct offsets
         DO 20 J = 1,NUMIF
            K = BIF + J - 1
            IFFREQ(J) = IFFREQ(J) + IFCHW(J) * (CNTCHN(BIF)*AVGCH(K) -
     *         CNTCHN(K)*AVGCH(BIF) + REFOLD *
     *         (CNTCHN(K) - CNTCHN(BIF))) / MAX (1, CNTCHN(BIF))
 20         CONTINUE
C                                       Set bandwidths
         DO 30 J = 1,NUMIF
            K = BIF + J - 1
            IFTBW(J) = CNTCHN(K) * IFTBW(J)
            IFCHW(J) = CNTCHN(K) * IFCHW(J)
 30         CONTINUE
         IFQRNO = I
         CALL TABFQ ('WRIT', BUFFO, IFQRNO, OKOLS, ONUMV, NOIF, FQID,
     *      IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
 100     CONTINUE
C                                       Close the tables
      CALL TABIO ('CLOS', 0, IFQRNI, BUFFI, BUFFI, IRET)
      CALL TABIO ('CLOS', 0, IFQRNO, BUFFO, BUFFO, IRET)
      WRITE (MSGTXT,1040) DISKO, NEWCNO, OVER
      CALL MSGWRT (4)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FQMOD: ERROR ',I3,' OPENING INPUT FQ TABLE')
 1010 FORMAT ('FQMOD: ERROR ',I3,' OPENING OUTPUT FQ TABLE')
 1020 FORMAT ('FQMOD: ERROR ',I3,' READING INPUT FQ TABLE')
 1030 FORMAT ('FQMOD: ERROR ',I3,' WRITING OUTPUT FQ TABLE')
 1040 FORMAT ('Updated FQ table bandwidths ar vol/cno/vers',I3,I5,I4)
      END
      SUBROUTINE AVGSUB (VIS, NUMPOL, NUMIF, NCHAVG, NOUTCH, JNCS,
     *   JNCIF, JNCF, VISOUT)
C-----------------------------------------------------------------------
C   Routine to average a spectrum in frequency to produce a so-called
C   pseudo-continuum channel. The CHNSEL array is used to specify which
C   channels are required in the average.  It must be okay - it is not
C   checked here.
C   Inputs:
C      VIS      R(*)      Array containing the input visibility data
C                         (Re, Im, Wt)
C      NUMPOL   I         Number of polarizations
C      NUMIF    I         Number of IF's
C      NUMFRQ   I         Number of spectral channels
C      NCHAVG   I         # channels to average to form 1 o/p channel
C      NOUTCH   I         # output channels
C      JNCS     I         Index for polzn in output array
C      JNCIF    I         Index for IF in output array
C      JNCF     I         Index for FREQ in output array
C   Output:
C      VISOUT   R(*)      Pseudo-continuum visibility
C-----------------------------------------------------------------------
      INTEGER   NUMPOL, NUMIF, NCHAVG, NOUTCH, JNCS, JNCIF, JNCF
      REAL      VIS(*), VISOUT(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LOOPS, LOOPIF, LOOPF, INDEX, INP, OUTDEX, I
      REAL      SUMWT, SUMRE, SUMIM, WT, XNORM
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Check increment in channel
C                                       selector array and default
C                                       values of start and stop
C                                       Select/average
      DO 70 LOOPS = 1,NUMPOL
         DO 60 LOOPIF = 1,NUMIF
            INDEX = 1 + (LOOPS-1)*INCS + (LOOPIF-1)*INCIF
            DO 30 LOOPF = 1, NOUTCH
               OUTDEX = 1 + (LOOPS-1)*JNCS + (LOOPIF-1)*JNCIF +
     *            (LOOPF-1)*JNCF
               SUMWT = 0.0
               SUMRE = 0.0
               SUMIM = 0.0
               DO 20 I = 1, NCHAVG
                  INP = INDEX + (((LOOPF-1)*NCHAVG) + I-1)*INCF
                  WT = VIS(INP+2)
                  IF (WT.GT.0.0) THEN
                     SUMRE = SUMRE + VIS(INP)*WT
                     SUMIM = SUMIM + VIS(INP+1)*WT
                     SUMWT = SUMWT + WT
                     END IF
 20               CONTINUE
               XNORM = 1.0
               IF (SUMWT.GT.1.0E-10) XNORM = 1.0 / SUMWT
               VISOUT(OUTDEX) = SUMRE * XNORM
               VISOUT(OUTDEX+1) = SUMIM * XNORM
               VISOUT(OUTDEX+2) = SUMWT
 30            CONTINUE
 60         CONTINUE
 70      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE COPTAB (DISKIN, CNOIN, DISKOU, CNOOUT, IRET)
C-----------------------------------------------------------------------
C   Updates tables for selection by IF etc
C   Inputs:
C      DISKIN   I        Input disk number
C      CONIN    I        Input catalog number
C      DISKOU   I        Output disk number
C      CNOOUT   I        Output catalog number
C   Inputs in common:
C      BIF      I        First IF
C      EIF      I        Highest IF selected
C      FQOFF    D        Frequency offset
C      SELIF    L        Select IFs or not
C   Output:
C      IRET     I        Return code, 0=>OK
C   Modified version - omits BP, FG
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN, DISKOU, CNOOUT, IRET
C
      INCLUDE 'INCS:DSEL.INC'
      DOUBLE PRECISION CATD(128)
      REAL      CATR(256), FINC(MAXIF)
      LOGICAL   TABLE, EXIST, FITASC, SELIF, ISBAND(MAXIF), MULTI
      CHARACTER NOTTYP(21)*2
      INTEGER   IERR, LUN1, LUN2, VER, NVER, OFQID, ISUB, JSUB, AN(50),
     *   NA, NNIF, I, FREQID, BUFF1(512), BUFF2(512), ISURNO, SIDSOU,
     *   SQUAL, SUFQID,NSOURC, INOGRP, KOLS(MAXSUC), NUMV(MAXSUC),
     *   NONOT, BPOL, EPOL, IROUND
      CHARACTER VELTYP*8, VELDEF*8, SSNAME*16, SCALCO*4, BNDCOD(MAXIF)*8
      DOUBLE PRECISION SBANDW, SRAEPO, SDECEP, SEPOCH, SRAAPP, SDECAP,
     *   SPMRA, SPMDEC, SLSRVE(MAXIF), SFREQO(MAXIF), SLREST(MAXIF),
     *   TIME1, TIME2, FOFF(MAXIF), FQOFF, SRAOBS, SDECOB
      REAL     SFLUX(4,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATR, CATD, CATBLK)
      DATA LUN1, LUN2 /28, 29/
      DATA NONOT, NOTTYP /21, 'NX', 'FQ', 'CH', 'AN', 'CL', 'CD', 'IM',
     *   'MC', 'PC', 'SN', 'SY', 'SU', 'TY', 'WX', 'FG', 'BP', 'CQ',
     *   'GC', 'BL', 'CP', 'PD'/
C-----------------------------------------------------------------------
C                                       Single source now?
      MULTI = ILOCSU.GT.0
C                                       polarization
      IF (CATUV(KINAX+JLOCS).EQ.CATBLK(KINAX+JLOCS)) THEN
         BPOL = 1
         EPOL = MIN (2, CATBLK(KINAX+JLOCS))
      ELSE
         FINC(1) = CATD(KDCRV+JLOCS) + (1 - CATR(KRCRP+JLOCS)) *
     *      CATR(KRCIC+JLOCS)
         BPOL = ABS (IROUND (FINC(1)))
         EPOL = MIN (2, CATBLK(KINAX+JLOCS))
         EPOL = MAX (EPOL, BPOL)
         END IF
C                                       Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKOU, CNOIN,
     *   CNOOUT, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'FLGIHI: ERROR COPYING TABLES'
         CALL MSGWRT (6)
         END IF
C                                       more complex tables
      MSGTXT = 'Updating tables for IF/FREQID/channel selection'
      CALL MSGWRT (4)
      ISUB = 0
      JSUB = -1
      NA = 0
C                                       spectral tables
      CALL FNDEXT ('FG', CATUV, NVER)
      IF (NVER.GT.0) THEN
         IF (FGVER.LE.0) THEN
            MSGTXT = 'FLAG TABLES ARE DISCARDED'
         ELSE IF (FGVER.LT.NVER) THEN
            MSGTXT = 'FLAG TABLES ABOVE FLAGVER ARE DISCARDED'
         ELSE
            MSGTXT = 'ALL FLAG TABLES ARE ASSUMED TO BE APPLIED'
            END IF
         CALL MSGWRT (7)
         END IF
      CALL FNDEXT ('BP', CATUV, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'BANDPASS TABLES ARE DISCARDED'
         CALL MSGWRT (7)
         END IF
      CALL FNDEXT ('CP', CATUV, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'CAL SOURCE POLARIZATION TABLES ARE DISCARDED'
         CALL MSGWRT (7)
         END IF
      CALL FNDEXT ('PD', CATUV, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'ANTENNA SPECTRAL POLARIZATION TABLES ARE DISCARDED'
         CALL MSGWRT (7)
         END IF
C                                       allow 15 min extra to be sure
C                                       that one gets all needed rows
      TIME1 = TSTART - 0.0104D0
      TIME2 = TEND + 0.0104D0
      SELIF = (BIF.GT.1) .OR. (EIF.LT.CATUV(KINAX+KLOCIF)) .OR.
     *   (FRQSEL.GT.0)
      CALL FILL (50, 0, AN)
C                                       FQ table
      IF (JLOCIF.GT.0) THEN
         CALL DFILL (MAXIF, 0.0D0, SFREQO)
C                                       Multi to single source
         IF ((KLOCSU.GE.0) .AND. (ILOCSU.LT.0)) THEN
C                                       Open file
            CALL SOUINI ('READ', BUFF2, IUDISK, IUCNO, 1, CATUV, LUN1,
     *         INOGRP, VELTYP, VELDEF, SUFQID, I, KOLS, NUMV, IRET)
            IF (IRET.NE.0) GO TO 20
C                                       Get number of sources.
            NSOURC = BUFF2(5)
C                                       Loop looking for source.
            DO 10 I = 1,NSOURC
               ISURNO = I
               CALL TABSOU ('READ', BUFF2, ISURNO, KOLS, NUMV, SIDSOU,
     *            SSNAME, SQUAL, SCALCO, SFLUX, SFREQO, SBANDW, SRAEPO,
     *            SDECEP, SEPOCH, SRAAPP, SDECAP, SRAOBS, SDECOB,
     *            SLSRVE, SLREST, SPMRA, SPMDEC, IRET)
               IF (IRET.GT.0) CALL DFILL (MAXIF, 0.0D0, SFREQO)
               IF ((SIDSOU.EQ.SOUWAN(1)) .OR. (IRET.GT.0)) GO TO 15
 10            CONTINUE
C                                       Didn't find
            CALL DFILL (MAXIF, 0.0D0, SFREQO)
C                                       Close file
 15         CALL TABIO ('CLOS', 0, I, BUFF2, BUFF2, IRET)
            END IF
C                                       Read old
 20      VER = 1
         CALL CHNDAT ('READ', BUFF1, DISKIN, CNOIN, VER, CATUV, LUN1,
     *      NNIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Fixup
         NNIF = EIF - BIF + 1
         FQOFF = FOFF(BIF)
C                                       force the first IF to zero
         DO 30 I = BIF,EIF
            FOFF(I) = FOFF(I) - FQOFF + (SFREQO(I) - SFREQO(BIF))
 30         CONTINUE
C                                       Output ref IF = 1
         CATD(KDCRV+JLOCIF) = 1.0D0
         CATR(KRCRP+JLOCIF) = 1.0
C                                       Rewrite new
         VER = 1
         FREQID = 1
         CALL CHNDAT ('WRIT', BUFF1, DISKOU, CNOOUT, VER, CATBLK, LUN1,
     *      NNIF, FOFF(BIF), ISBAND(BIF), FINC(BIF), BNDCOD(BIF),
     *      FREQID, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Reference frequency in AN table
C                                       IF selection
      CALL FNDEXT ('AN', CATUV, NVER)
      DO 100 VER = 1,NVER
         CALL ISTAB ('AN', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF ((EXIST) .AND. (IERR.EQ.0)) CALL ANSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, VER, CATUV, CATBLK, LUN1, LUN2, BIF,
     *      EIF, FQOFF, DOPOL, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 100     CONTINUE
C                                       Revise tables: note depends on
C                                       doing loops 0 times if none
      OFQID = FRQSEL
C                                       CL tables
      CALL FNDEXT ('CL', CATUV, NVER)
      IF (.NOT.MULTI) THEN
         VER = 1
         CALL ISTAB ('CL', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL CL2FO (DISKIN, CNOIN,
     *      VER, LUN1, CATUV, DISKOU, CNOOUT, LUN2, CATBLK, SOUWAN,
     *      BUFF1, BUFF2, IRET)
         IF (IRET.NE.0) GO TO 999
      ELSE IF (DOCAL) THEN
         VER = 1
         CALL ISTAB ('CL', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
C                                       do NOT select on sources
         IF (EXIST .AND. (IERR.EQ.0)) CALL CLNULL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, 0, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.NE.0) GO TO 999
      ELSE
         DO 140 VER = 1,NVER
            CALL ISTAB ('CL', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *         EXIST, FITASC, IERR)
C                                       do NOT select on sources
            IF (EXIST .AND. (IERR.EQ.0)) CALL CLSEL (DISKIN, CNOIN,
     *         DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL,
     *         EPOL, BIF, EIF, OFQID, TIME1, TIME2, 0, SOUWAN, AN, NA,
     *         ISUB, JSUB, BUFF1, BUFF2, IRET)
            IF (IRET.NE.0) GO TO 999
 140        CONTINUE
         END IF
C                                       CD tables
      CALL FNDEXT ('CD', CATUV, NVER)
      DO 145 VER = 1,NVER
         CALL ISTAB ('CD', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
C                                       do not select on antenna
         IF (EXIST.AND.(IERR.EQ.0)) CALL CDSEL (DISKIN, CNOIN, DISKOU,
     *      CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL, BIF,
     *      EIF, OFQID, AN, 0, ISUB, JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 145     CONTINUE
C                                       CQ tables
      CALL FNDEXT ('CQ', CATUV, NVER)
      DO 160 VER = 1, NVER
         CALL ISTAB ('CQ', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST.AND.(IERR.EQ.0)) CALL CQSEL (DISKIN, CNOIN, DISKOU,
     *      CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BIF, EIF, OFQID,
     *      ISUB, JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 160     CONTINUE
C                                       GC tables
      CALL FNDEXT ('GC', CATUV, NVER)
      DO 200 VER = 1,NVER
         CALL ISTAB ('GC', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL GCSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BCHAN, ECHAN, BIF, EIF, OFQID, AN, NA, ISUB, JSUB, BUFF1,
     *      BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 200     CONTINUE
C                                       IM tables
      CALL FNDEXT ('IM', CATUV, NVER)
      DO 220 VER = 1,NVER
         CALL ISTAB ('IM', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL IMSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 220     CONTINUE
C                                       MC tables
      CALL FNDEXT ('MC', CATUV, NVER)
      DO 240 VER = 1,NVER
         CALL ISTAB ('MC', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL MCSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 240     CONTINUE
C                                       PC tables
      CALL FNDEXT ('PC', CATUV, NVER)
      DO 260 VER = 1,NVER
         CALL ISTAB ('PC', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL PCSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 260     CONTINUE
C                                       SN tables
      CALL FNDEXT ('SN', CATUV, NVER)
      IF (DOCAL) NVER = 0
      DO 280 VER = 1,NVER
         CALL ISTAB ('SN', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL SNSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, ISUB, JSUB, BUFF1, BUFF2,
     *      IRET)
         IF (IRET.GT.0) GO TO 999
 280     CONTINUE
C                                       SY tables
      CALL FNDEXT ('SY', CATUV, NVER)
      DO 290 VER = 1,NVER
         CALL ISTAB ('SY', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL SYSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 290     CONTINUE
C                                       SU tables
      IF (ILOCSU.GE.0) THEN
         CALL FNDEXT ('SU', CATUV, NVER)
         DO 300 VER = 1,NVER
            CALL ISTAB ('SU', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *         EXIST, FITASC, IERR)
            IF (EXIST .AND. (IERR.EQ.0)) CALL SUSEL (DISKIN, CNOIN,
     *         DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BIF, EIF,
     *         OFQID, BUFF1, BUFF2, IRET)
            IF (IRET.GT.0) GO TO 999
 300        CONTINUE
         END IF
C                                       TY tables
      CALL FNDEXT ('TY', CATUV, NVER)
      DO 320 VER = 1,NVER
         CALL ISTAB ('TY', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL TYSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, AN, NA, ISUB, JSUB, BUFF1,
     *      BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 320     CONTINUE
C                                       WX tables
      CALL FNDEXT ('WX', CATUV, NVER)
      DO 340 VER = 1,NVER
         CALL ISTAB ('WX', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL WXSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, TIME1,
     *      TIME2, AN, NA, ISUB, JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 340     CONTINUE
C                                       BL tables
      CALL FNDEXT ('BL', CATUV, NVER)
      IF (DOBL) NVER = 0
      DO 350 VER = 1,NVER
         CALL ISTAB ('BL', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL BLSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      AN, NA, ISUB, JSUB, BIF, EIF, OFQID, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 350     CONTINUE
C
 999  RETURN
      END
