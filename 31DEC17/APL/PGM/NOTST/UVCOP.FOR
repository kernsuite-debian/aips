LOCAL INCLUDE 'UVCOP.INC'
C                                       Local include for UVCOP
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   MMXSOU
C                                       MMXSOU = max number of sources
      PARAMETER (MMXSOU = XSTBSZ)
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, NAMOUT*12, CLAOUT*6,
     *   HISCRD(5)*64
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XNAMOU(3),
     *   XCLAOU(2), CATH(256)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS, JBUFSZ, NTCOR,
     *   QUAL(30), NBASE, ANT1(1000), ANT2(1000), CATOLD(256), MXIF,
     *   MXCH, NA, AN(50), NCORI, NCORO, UCATI(256), PRMTRN(14), OLNVIS,
     *   NVORNG, VORNG(2,1000), IBUFF1(UVBFSL), IBUFF2(UVBFSL)
      LOGICAL   SELCH, DROPCH(MAXCIF), DOBOTH, DOAC, DOXC, MULTI,
     *   DOSCL, REPTON, SELIF
      DOUBLE PRECISION XCOUNT(4), FQOFF, CATD(128), UVWSCL
      REAL      XSIN, XDISIN, XSOUT, XDISO, XBAND, XFREQ, XFQID, XBCHAN,
     *   XECHAN, XBIF, XEIF, XQUAL, XUVRA(2), TIME(8), XANT(50),
     *   XBASE(50), XSUB, XFLAG, BPARM(10), XNAC, XCENT, BUFF1(UVBFSL),
     *   BUFF2(UVBFSL), UVRA2(2), CATR(256), DIFPIX
      EQUIVALENCE (CATBLK, CATD, CATH, CATR)
      EQUIVALENCE (IBUFF1, BUFF1), (IBUFF2, BUFF2)
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /CHSEL/ SELCH, DROPCH, NBASE, ANT1, ANT2, MXIF,
     *   MXCH, QUAL, NA, AN, SELIF
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, XBAND, XFREQ, XFQID, XBCHAN, XECHAN, XBIF, XEIF,
     *   XXSOUR, XQUAL, XUVRA, TIME, XANT, XBASE, XSUB, XFLAG, BPARM,
     *   XNAC, XCENT
      COMMON /COPPRM/ XCOUNT, FQOFF, UCATI, CATOLD, UVWSCL, UVRA2,
     *   SEQIN, SEQOUT, DISKIN, DISKO, NTCOR, NUMHIS, DOBOTH, DOAC,
     *   DOXC, MULTI, DOSCL, REPTON, NCORI, NCORO, PRMTRN, OLNVIS,
     *   NVORNG, VORNG, DIFPIX
      COMMON /CHPARM/ NAMEIN, NAMOUT, XSOUR, CLAIN, CLAOUT, HISCRD
LOCAL END
LOCAL INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:PUVD.INC'
C                                       set flag parameters
      INTEGER   MAXFLG
C                                       MAXFLG= max. no. flags active
      PARAMETER (MAXFLG=600001)
LOCAL END
      PROGRAM UVCOP
C-----------------------------------------------------------------------
C! UVCOP copies selected data from UV data base
C# Spectral UV-util CALIBRATION
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2001, 2003-2017
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
C   UVCOP copies a selected timerange and set of frequency channels
C   from a UV data base.
C   Inputs:
C     AIPS adverb    Prg. name.          Description.
C       INNAME         NAMEIN        Name of input uv data.
C       INCLASS        CLAIN         Class of input uv data.
C       INSEQ          SEQIN         Seq. of input uv data.
C       INDISK         DISKIN        Disk number of input uv data.
C       OUTNAME        NAMOUT        Name of the output uv file.
C                                    Default output is input file.
C       OUTCLASS       CLAOUT        Class of the output uv file.
C       OUTSEQ         SEQOUT        Seq. number of output uv data.
C       OUTDISK        DISKO         Disk number of the output file.
C       BCHAN          BCHAN         First channel to select.
C       ECHAN          ECHAN         Last channel to select.
C       BIF            BIF           First IF number
C       EIF            EIF           Highest IF number
C       SOURCES        XSOUR         Source list
C       QUAL           QUAL          Qualifier
C       UVRANGE(2)     UVRA          Range of projected baselines
C                                    in 1000's of wavelengths
C       TIMERANG(8)    TIME          Timerange to be copied
C                                    1-4 = start Day, Hour, Min, Sec.
C                                    5-8 =  end  Day, Hour, Min, Sec.
C       UVCOPARM(10)   BPARM         1 > 0 => keep fully flagged data
C                                    2 > 0 -> drop subarrays
C                                    3 -->    data type to copy
C                                    4 > 0 => report progress
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER IRET

      INCLUDE 'UVCOP.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA PRGM /'UVCOP '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL COPYIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL COPYUV (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL COPYHI
      CALL COPTAB (IRET)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE COPYIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   COPYIN gets input parameters for COPY and creates an output file
C   if necessary.  Also set the parameters which are used to select
C   data.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                5 => catalog troubles
C                                6 => too much data to select ch.
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C      NSOUWD     I    Number of sources IDs to copy (0=>all)
C      SOUWAN     I(*) List of source IDs to copy.
C      FRQSEL     I    Freq ID sel, .le. 0 => all
C      SELCH      L    If .TRUE. some of the channels are to be dropped.
C      DROPCH(*)  L    If .TRUE. the corresponding correlator value is
C                      to be dropped.
C      FQOFF      D    Additive change in reference frequency
C      DOSCL      L    If true rescale u,v,w
C      UVWSCL     R    U,v,w scaling factor
C      ISCMP      L    If .TRUE. then data is compressed
C----------------------------------------------------------------------
      CHARACTER STAT*4, PRGN*6, BLANK*6, UTYPE*2
      INTEGER   JERR, NCHOLD, NIFOLD, NIFNEW, ICOUNT, CHNUM, ITEMP, I,
     *   OLDCNO, J, IROUND, NPARM, IERR, NUMAN(513), ISUB, LUNAN, LUNI,
     *   LUNO, INVER, OUTVER, TNIF, NUMFQ, NSOUR, NEVIS, IFQ, OFQ, K,
     *   IBUFF(512), KEY(2,2), KEYSUB(2,2), INCX
      REAL      TIME1, TIME2, FKEY(2,2)
      LOGICAL   T, F, MATCH, TABLE, EXIST, FITASC, FOUND
      INCLUDE  'UVCOP.INC'
      DOUBLE PRECISION FOFF(MAXIF), FRQREF, FRQOFF
      REAL      FINC(MAXIF), FRQINC
      INTEGER   ISBAND(MAXIF), FQBEG, FQEND
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (BUFF1, IBUFF)
      DATA BLANK /'      '/
      DATA T, F /.TRUE.,.FALSE./
      DATA KEY  /5,0, 1,0/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 266
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
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
         QUAL(I) = IROUND (XQUAL)
 20      CONTINUE
C                                       Crunch input parameters
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
      CALL RCOPY (2, XUVRA, UVRA)
      IF (UVRA(2).LE.1.0E-20) UVRA(2) = 1.0E10
      UVRA2(1) = (UVRA(1) * 1.0E3)**2
      UVRA2(2) = (UVRA(2) * 1.0E3)**2
      SUBARR = IROUND (XSUB)
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
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
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      NVORNG = 1
      VORNG(1,1) = 1
      VORNG(2,1) = NVIS
C                                       Save frequency of u,v,w
      UVFREQ = FREQ
C                                       Save no. random parms.
      NPRMIN = NRPARM
C                                       Save source no. pointer
      KLOCSU = ILOCSU
C                                       Is FREQSEL rand parm present
      DOFQSL = ILOCFQ .GT. 0
      KLOCFQ = ILOCFQ
      OLNVIS = NVIS
C                                       Save relevant pointers for
C                                       flagging
      KLOCIF = JLOCIF
      KLOCFY = JLOCF
      LRECIN = LREC
      NCORI = (LRECIN - NRPARM) / CATBLK(KINAX)
C                                       Save old CATBLK
      CALL COPY (256, CATBLK, CATOLD)
      CALL COPY (256, CATBLK, CATUV)
      CALL FNDEXT ('AN', CATOLD, I)
      IF ((SUBARR.GT.I) .OR. (SUBARR.LT.0)) SUBARR = 0
      IUDISK = DISKIN
      IUCNO = OLDCNO
      IFLUN = 30
      KNCOR = NCOR
      KCOR0 = ICOR0
      KNCF = INCF / CATUV(KINAX)
      KNCIF = INCIF / CATUV(KINAX)
      KNCS = INCS / CATUV(KINAX)
      UBUFSZ = UVBFSL * 2
C                                       Compressed?
      ISCMP = CATBLK(KINAX).EQ.1
C                                       Data type to copy
      DOBOTH = ABS(BPARM(3)).LT.0.5
      DOXC = ABS(BPARM(3)-1.0).LT.0.5
      DOAC = ABS(BPARM(3)-2.0).LT.0.5
      BPARM(5) = MAX (0.0, BPARM(5))
      IF (BPARM(7).EQ.0.0) BPARM(7) = 15.0
      BPARM(7) = MAX (0.0, BPARM(7))
C                                       Report progress?
      REPTON = BPARM(4).GT.0.0
C                                       Find WEIGHT random parameter
      IF (ISCMP) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), KLOCWT,
     *      IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'COULD NOT FIND WEIGHT FOR COMPRESSED DATA'
            JERR = 6
            GO TO 990
            END IF
      ELSE
         KLOCWT = -1
         END IF
C                                       Freq id
      IF (ILOCFQ.GE.0) THEN
         SELBAN = XBAND
         SELFRQ = XFREQ
         FRQSEL = IROUND (XFQID)
       ELSE
          SELBAN = 0.
          SELFRQ = 0.
          FRQSEL = 0
          END IF
      IF (FRQSEL.LE.0) FRQSEL = -1
      IF ((SELBAN.GT.0.0) .OR. (SELFRQ.GT.0.0)) THEN
         LUNI = 28
         CALL FQMATC (DISKIN, OLDCNO, CATOLD, LUNI, SELBAN, SELFRQ,
     *      MATCH, FRQSEL, JERR)
         IF (.NOT.MATCH) THEN
            WRITE (MSGTXT,1070)
            JERR = 1
            GO TO 990
            END IF
         IF (JERR.GT.0) GO TO 999
         END IF
C                                       IF range
      IF (JLOCIF.GT.0) THEN
         BIF = IROUND (XBIF)
         EIF = IROUND (XEIF)
         NIFOLD = CATBLK(KINAX+JLOCIF)
         MXIF = NIFOLD
         BIF = MAX (1, MIN (BIF, NIFOLD))
         IF (EIF.LT.BIF) EIF = NIFOLD
         EIF = MAX (1, MIN (EIF, NIFOLD))
         NIFNEW = EIF - BIF + 1
      ELSE
         BIF = 1
         EIF = 1
         NIFOLD = 1
         NIFNEW = 1
         END IF
      SELIF = (BIF.GT.1) .OR. (EIF.LT.MXIF) .OR. (FRQSEL.GT.0)
      FGVER = IROUND (XFLAG)
      CALL FNDEXT ('FG', CATBLK, I)
      IF (FGVER.GT.I) FGVER = -1
      DOFLAG =  FGVER.GT.0
      TMFLST = -1.E20
      NUMFLG = 0
C                                       Check high channel number.
      BCHAN = IROUND (XBCHAN)
      ECHAN = IROUND (XECHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      MXCH = CATBLK(KINAX+JLOCF)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
C                                       Set time range.
      TIME1 = TIME(1)+TIME(2)/24.+TIME(3)/(24.*60.)+TIME(4)/(24.*3600.)
      TIME2 = TIME(5)+TIME(6)/24.+TIME(7)/(24.*60.)+TIME(8)/(24.*3600.)
      XCOUNT(2) = -1.0D6
      IF (TIME1.NE.0.0) XCOUNT(2) = TIME1
      XCOUNT(3) = TIME2
      IF ((TIME1.GE.TIME2) .OR. (TIME2.EQ.0.0)) THEN
         XCOUNT(3) = 1.0D6
         TIME2 = 1.E6
         END IF
      XCOUNT(4) = 0.0D0
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT,CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Determine number of correlators
      NTCOR = 1
      J = CATBLK(KIDIM)
      DO 50 I = 2,J
         NTCOR = NTCOR * CATBLK(KINAX+I-1)
 50      CONTINUE
C                                       Modify header for channel
C                                       select, keep same ref. frequency
C                                       but it will have a new number.
      NCHOLD = CATBLK(KINAX+JLOCF)
      CATBLK(KINAX+JLOCF) = ECHAN - BCHAN + 1
      CATR(KRCRP+JLOCF) = CATR(KRCRP+JLOCF) - BCHAN + 1
C                                       IF selection in CATBLK
      IF (NIFOLD.GT.NIFNEW) THEN
         CATBLK(KINAX+JLOCIF) = NIFNEW
         CATD(KRCRP+JLOCIF) = 1.0
         CATD(KDCRV+JLOCIF) = 1.0
         END IF
C                                       Change alt. ref. pixel; keep
C                                       same ref. pixel which now has
C                                       a new number.
      CATR(KRARP) = CATR(KRARP) - (BCHAN - 1)
C                                       Multi-source file?
      CALL MULSDB (CATOLD, MULTI)
C                                       Source list
      NSOUWD = 0
      IF (MULTI) THEN
         NSOUR = 30
         NSOUWD = MMXSOU
         CALL SOURNU (XSOUR, QUAL, NSOUR, DISKIN, OLDCNO, NSOUWD, BUFF1,
     *      SOUWAN, JERR)
         IF (JERR.LT.0) THEN
            MSGTXT = 'WARNING: Sources selected not found'
            CALL MSGWRT (6)
            JERR = 5
            END IF
         IF (JERR.NE.0) GO TO 999
         IF (SOUWAN(1).LE.0) NSOUWD = 0
         END IF
C                                       translate random parameters
      CALL PRMSET (CATBLK, PRMTRN)
C                                       Create output file.
      CALL ESTSIZ (OLDCNO, NEVIS, IERR)
      IF ((IERR.EQ.0) .AND. (NEVIS.GT.0)) CATBLK(KIGCN) = MIN (NEVIS,
     *   CATBLK(KIGCN))
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.2) GO TO 60
            WRITE (MSGTXT,1050) IERR
            GO TO 990
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
 60      IF ((CCNO.EQ.OLDCNO) .AND. (DISKO.EQ.DISKIN) .AND.
     *   (BPARM(2).LE.0.0)) GO TO 65
            WRITE (MSGTXT,1060)
            IF ((CCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN))
     *         WRITE (MSGTXT,1061)
            GO TO 990
C                                       Recover existing CATBLK
 65      FRW(NCFILE+1) = 2
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
C                                       Reset the common
      CALL UVPGET (JERR)
C                                       Put input file in READ
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
C                                       Copy any header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      IF (IERR.NE.0) GO TO 999
      SEQOUT = CATBLK(KIIMS)
      XCOUNT(1) = 0.0D0
C                                       Set data selection flags.
      SELCH = (NCHOLD.GT.CATBLK(KINAX+JLOCF)) .OR. (NIFOLD.GT.NIFNEW)
C                                       Set correlator flags.
      ICOUNT = -1
      DO 100 I = 1,NTCOR
C                                       Determine channel number.
         ICOUNT = ICOUNT + 1
         ITEMP = ICOUNT / KNCF
         CHNUM = MOD (ITEMP, NCHOLD) + 1
C                                       Check if data wanted.
         DROPCH(I) = (CHNUM.LT.BCHAN) .OR. (CHNUM.GT.ECHAN)
C                                       IF Selection
         IF (NIFOLD.GT.NIFNEW) THEN
            ITEMP = ICOUNT / KNCIF
            CHNUM = MOD (ITEMP, NIFOLD) + 1
C                                       Check if data wanted.
            DROPCH(I) = DROPCH(I) .OR. ((CHNUM.LT.BIF) .OR.
     *         (CHNUM.GT.EIF))
            END IF
 100     CONTINUE
C                                       Copy selected portion of FQ
C                                       table.
      INVER = 1
      OUTVER = 1
      LUNI = 27
      LUNO = 28
      INVER = 1
C                                       Find  range of FREQids to copy
C                                       Select single FREQid
      IF (FRQSEL.GT.0) THEN
         FQBEG = FRQSEL
         FQEND = FRQSEL
C                                       Select all FREQids
C                                       Check if FQ table exists
      ELSE
         CALL ISTAB ('FQ', DISKIN, OLDCNO, INVER, LUNI, BUFF1, TABLE,
     *      EXIST, FITASC, JERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1080) JERR
            GO TO 990
            END IF
C                                       Find number of FQ entries
         IF (EXIST) THEN
            CALL FQINI ('READ', IBUFF, DISKIN, OLDCNO, INVER, CATOLD,
     *         LUNI, IFQRNO, FQKOLS, FQNUMV, TNIF, IERR)
            IF (IERR.EQ.0) THEN
               CALL TABIO ('CLOS', 0, IFQRNO, IBUFF, IBUFF, JERR)
               IF (JERR.GT.0) GO TO 999
               NUMFQ = IBUFF(5)
            ELSE
               JERR = IERR
               GO TO 999
               END IF
C                                       This will do for CH table
C                                       or no table cases
         ELSE
            NUMFQ = 1
            END IF
C
         FQBEG = 1
         FQEND = NUMFQ
         END IF
      FQOFF = 0.0D0
C                                       Loop over all desired FREQids
      OFQ = 0
      DO 150 IFQ = FQBEG, FQEND
         CALL CHNDAT ('READ', IBUFF, DISKIN, OLDCNO, INVER, CATOLD,
     *      LUNI, TNIF, FOFF, ISBAND, FINC, BNDCOD, IFQ, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT, 1090) IFQ
            GO TO 990
            END IF
C                                       Get new reference values
         IF (IFQ.EQ.FQBEG) THEN
            FRQREF = CATD(KDCRV+JLOCF) + FOFF(BIF)
            FRQINC = FINC(BIF)
            FRQOFF = FOFF(BIF)
            FQOFF = FOFF(BIF)
            END IF
C                                       Fix up IF selection in FQ
         DO 155 J = BIF,EIF
            FOFF(J) = FOFF(J) - FRQOFF
 155        CONTINUE
C                                       Write new FQ table
         OFQ = OFQ + 1
         CALL CHNDAT ('WRIT', IBUFF, DISKO, CCNO, INVER, CATBLK, LUNO,
     *      NIFNEW, FOFF(BIF), ISBAND(BIF), FINC(BIF), BNDCOD(BIF), OFQ,
     *      JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT, 1095) OFQ
            GO TO 990
            END IF
 150     CONTINUE
C                                       use index table
      CALL FNDEXT ('NX', CATOLD, I)
      IF (I.GT.0) CALL FINDVO (1000, DISKIN, OLDCNO, CATOLD, SUBARR,
     *   FRQSEL, NSOUWD, SOUWAN, TIME1, TIME2, NVORNG, VORNG)
C                                       Rescale u,v,w to reflect new
C                                       reference frequency
C                                       use start time
      DIFPIX = 0.0
      IF ((FRQSEL.GT.1) .OR. (BIF.NE.1) .OR. (XCENT.GT.0.0)) THEN
         DOSCL = T
         IF (XCENT.GT.0.0) THEN
            INCX = CATBLK(KINAX+JLOCF) / 2 + 1
            DIFPIX = INCX - CATR(KRCRP+JLOCF)
            FRQREF = FRQREF + FRQINC * DIFPIX
            CATR(KRCRP+JLOCF) = INCX
            END IF
         UVWSCL = FRQREF / CATD(KDCRV+JLOCF)
C                                       Update header as well
         CATD(KDCRV+JLOCF) = FRQREF
         CATR(KRCIC+JLOCF) = FRQINC
      ELSE
         DOSCL = F
         UVWSCL = 1.0D0
         END IF
C                                       Baselines desired -
C                                       get subarray info
      ISUB = 0
      LUNAN = 20
      NBASE = 1000
      CALL GETNAN (DISKIN, OLDCNO, CATOLD, LUNAN, BUFF1, NUMAN, IERR)
      CALL AN10RS (NUMAN, ISUB, XANT, XBASE, NBASE, ANT1, ANT2)
C                                       Trap for all baselines.
      IF ((NBASE.EQ.1) .AND. (ANT1(1).EQ.0) .AND. (ANT2(1).EQ.0))
     *   NBASE = 0
C                                       Find number of selected antennas
C                                       and their list
      NA = 0
      IF (DOAC) THEN
         IF (NBASE.NE.0) THEN
            DO 170 I = 1,50
               FOUND = .FALSE.
               DO 160 K = 1,NBASE
                  IF (FOUND) GO TO 170
                  IF (I.EQ.ANT1(K)) THEN
                     FOUND = .TRUE.
                     NA = NA + 1
                     AN(NA) = ANT1(K)
                     END IF
 160              CONTINUE
 170           CONTINUE
            END IF
      ELSE
         IF (ANT2(1).NE.0) THEN
            DO 190 I = 1,50
               FOUND = .FALSE.
               DO 180 K = 1,NBASE
                  IF (FOUND) GO TO 190
                  IF (I.EQ.ANT1(K)) THEN
                     FOUND = .TRUE.
                     NA = NA + 1
                     AN(NA) = ANT1(K)
                     END IF
                  IF (FOUND) GO TO 190
                  IF (I.EQ.ANT2(K)) THEN
                     FOUND = .TRUE.
                     NA = NA + 1
                     AN(NA) = ANT2(K)
                     END IF
 180              CONTINUE
 190           CONTINUE
            END IF
         END IF
C                                       save header!
      CALL COPY (256, CATBLK, UCATI)
C                                       Flag table
      TIMORD = ISORT(1:1).EQ.'T'
      UBUFSZ = UVBFSL * 2
      IF (DOFLAG) THEN
C                                       Reformat table?
         CALL FGREFM (IUDISK, IUCNO, FGVER, CATUV, IFLUN, JERR)
         CALL FLGINI ('READ', FGBUFF, IUDISK, IUCNO, FGVER, CATUV,
     *      IFLUN, IFGRNO, FGKOLS, FGNUMV, JERR)
         IF (JERR.NE.0) DOFLAG = F
C                                       Resort if necessary.
         IF (DOFLAG .AND. (FGBUFF(43).NE.KEY(1,1))) THEN
C                                       Sort to time order.
            CALL TABIO ('CLOS', 0, K, UBUFF, FGBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TABSRT (IUDISK, IUCNO, 'FG', FGVER, FGVER, KEY, KEYSUB,
     *         FKEY, FGBUFF, CATUV, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Re initialize.
            CALL FLGINI ('READ', FGBUFF, IUDISK, IUCNO, FGVER, CATUV,
     *         IFLUN, IFGRNO, FGKOLS, FGNUMV, IERR)
            END IF
         END IF
      IF (DOFLAG) THEN
         WRITE (MSGTXT,1190) FGVER
      ELSE
         MSGTXT = 'No flag table is applied this time'
         END IF
      CALL MSGWRT (3)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COPYIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,
     *   ' DISK=',I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY NOT OVERWRITE INPUT FILE IF DELETING SUBARRAYS')
 1061 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('COPYIN: ERROR',I3,' UPDATING NEW CATBLK')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1080 FORMAT ('COPYIN: ERROR CHECKING EXISTENCE OF FQ TABLE = ', I3)
 1090 FORMAT ('COPYIN: ERROR READING INPUT FQ/CH TABLE FOR FQID=', I3)
 1095 FORMAT ('COPYIN: ERROR WRITING OUTPUT FQ TABLE FOR FQID = ', I3)
 1190 FORMAT ('Applying flag table version',I4,' to the data')
      END
      SUBROUTINE COPYUV (IRET)
C-----------------------------------------------------------------------
C   COPYUV sends uv data one point at a time to the time check routine
C   and then writes the modified data if requested.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C                       9 => output record size exceeds input.
C-----------------------------------------------------------------------
      CHARACTER OFILE*48, IFILE*48
      INTEGER   IRET, INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, LRECO,
     *   I, ILENBU, KBIND, NIOUT, NIOLIM, IBIND, INCX, VO, BO, NUMVIS,
     *   PRTCNT, INTRVL, TIMINT(4), IROUND, ILOCWT, NN, NREC,NP, NCOPY,
     *   RNXRET, IVORNG, LVIS
      REAL      XDAY, DUM
      LOGICAL   T, F, DOARR
      INCLUDE 'UVCOP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI, LUNO /16, 17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
      DATA INTRVL /10000/
C-----------------------------------------------------------------------
C                                       Set length of complex axis
      CALL COPY (256, UCATI, CATBLK)
      INCX = CATBLK(KINAX)
      ISCMP = INCX.LE.1
      INCX = 3
C                                       Find weight and scale.
      IF (ISCMP) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            IRET = 9
            GO TO 990
            END IF
         END IF
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'INPUT'
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'OUTPUT'
         GO TO 990
         END IF
C                                       Init vis file for write
C                                       LRECO = length of output rec.
      LRECO = CATBLK(KINAX)
      DO 30 I = 2,KICTPN
         LRECO = LRECO * MAX (1, CATBLK(KINAX+I-1))
 30      CONTINUE
      LRECO = LRECO + NRPARM
      NCOPY = LRECO - NRPARM
      NCORO = (LRECO - NRPARM) / CATBLK(KINAX)
C                                       Make sure LREC >= LRECO
      IF ((LREC.NE.LRECO) .OR. (LRECIN.LT.LRECO)) THEN
         WRITE (MSGTXT,1030) LRECO, LREC, LRECIN
         IRET = 9
         GO TO 990
         END IF
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, OLNVIS, VO, LRECO, ILENBU,
     *   JBUFSZ, BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'OUTPUT'
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
      NUMVIS = 0
      PRTCNT = 0
      DOARR = .FALSE.
      NN = 0
      CURSOU = 0
C                                       make an index table
      CALL RNXGET (DISKIN, IUCNO, CATOLD)
      CALL RNXINI (DISKO, CCNO, CATBLK, RNXRET)
C                                       first VO range
      IVORNG = 0
C                                       Initialize reading VIS. file.
 60   IVORNG = IVORNG + 1
      VO = VORNG(1,IVORNG) - 1
      ILENBU = 0
      LVIS = VORNG(2,IVORNG) - VO
      CALL UVINIT ('READ', LUNI, INDI, LVIS, VO, LRECIN, ILENBU,
     *   JBUFSZ, BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'INPUT'
         GO TO 990
         END IF
C                                       Loop
C                                       Read vis. record.
 100     CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ', 'INPUT'
            GO TO 990
            END IF
         IPTRI = IBIND
C                                       done this range - do more?
         IF (INIO.LE.0) THEN
            IF (IVORNG.GE.NVORNG) GO TO 200
            GO TO 60
            END IF
C                                       keep going in this range
         DO 190 I = 1,INIO
            NUMVIS = NUMVIS + 1
C                                       move random parameters
            DO 110 NP = 1,NRPARM
               BUFF2(IPTRO+NP-1) = BUFF1(IPTRI+PRMTRN(NP)-1)
 110           CONTINUE
C                                       If TB sort order and record time
C                                       > final selected time then can
C                                       stop.
            IF (ISORT(1:1).EQ.'T') THEN
               IF (BUFF2(IPTRO+ILOCT).GT.XCOUNT(3)) GO TO 200
               END IF
            IF (ILOCSU.GT.-1) CURSOU = IROUND(BUFF2(IPTRO+ILOCSU))
C                                       compressed
            IF (ISCMP) THEN
               CALL ZUVXPN (NCORI, BUFF1(IPTRI+NPRMIN),
     *            BUFF1(IPTRI+KLOCWT), UBUFF)
               CALL COPYIT (NUMVIS, BUFF2(IPTRO+ILOCT),
     *            UBUFF, BUFF2(IPTRO), INCX, DOARR, IRET)
               IF (IRET.EQ.0) CALL ZUVPAK (NCORO, UBUFF,
     *            BUFF2(IPTRO+ILOCWT), BUFF2(IPTRO+NRPARM))
C                                       Decide if it's kept, select
            ELSE
               CALL COPYIT (NUMVIS, BUFF2(IPTRO+ILOCT),
     *            BUFF1(IPTRI+NPRMIN), BUFF2(IPTRO), INCX, DOARR, IRET)
               IF (IRET.EQ.0) CALL RCOPY (NCOPY, BUFF1(IPTRI+NPRMIN),
     *            BUFF2(IPTRO+NRPARM))
               END IF
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
            ELSE IF (IRET.EQ.0) THEN
               NIOUT = NIOUT + 1
               IF (ILOCSU.GT.-1) CURSOU = IROUND(BUFF2(IPTRO+ILOCSU))
C                                       update NX table
               CALL RNXUPD (BUFF2(IPTRO), RNXRET)
C                                       Keep user informed.
               PRTCNT = PRTCNT + 1
               IF ((PRTCNT.EQ.INTRVL) .AND. (REPTON)) THEN
C                                       Time of this record.
                  XDAY = BUFF2(IPTRO+ILOCT)
                  CALL TODHMS (XDAY, TIMINT)
                  WRITE (MSGTXT,1130) NUMVIS, TIMINT
                  CALL MSGWRT (3)
                  PRTCNT = 0
                  END IF
               IPTRO = IPTRO + LRECO
C                                       Check file size
               NN = NN + 1
               IF (NN.GT.CATBLK(KIGCN)) THEN
                  NREC = (10100 * LRECO * 2) / 512 + 1
                  CALL ZEXPND (LUNO, DISKO, OFILE, NREC, IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'EXPAND', 'OUTPUT'
                     GO TO 990
                     END IF
                  CALL ZEXIST (DISKO, OFILE, NREC, IRET)
                  CATBLK(KIGCN) = (NREC * 256.0D0) / LRECO
                  END IF
               END IF
C                                       OK, but no output please
            IPTRI = IPTRI + LRECIN
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRIT', 'OUTPUT'
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
 190        CONTINUE
         GO TO 100
C                                       Final call to COPYIT
 200  NUMVIS = -1
      CALL COPYIT (NUMVIS, DUM, BUFF1, BUFF1, INCX, DOARR, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1120) IRET
         IF (IRET.EQ.99) MSGTXT = 'NO DATA COPIED; DELETING OUTPUT'
         GO TO 990
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINISH', 'OUTPUT'
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT(1) + 0.01
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
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
 1000 FORMAT ('COPYUV: ERROR',I4,' ',A,'ING ',A,' VIS FILE')
 1030 FORMAT ('COPYUV: LRECO=',I6,'.GT. LREC, LRECIN=',2I6)
 1120 FORMAT ('COPYUV: COPYIT ERROR',I3)
 1130 FORMAT ('Processing Input visibility # ',I8,' at ',I3,'/',
     *   2(I2.2,':'),I2.2)
      END
      SUBROUTINE COPYHI
C-----------------------------------------------------------------------
C   COPYHI copies and updates history file for COPY
C-----------------------------------------------------------------------
      INTEGER   NONOT
      PARAMETER (NONOT=21)
C
      CHARACTER NOTTYP(NONOT)*2, HILINE*72,  LABEL*8, NTEMP*16
      INTEGER   LUN1, LUN2, I, IERR, NNANT, NNBAS, IC1, IC2, IC
      LOGICAL   T, F, EXCL, DESEL
      INCLUDE 'UVCOP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T, F /.TRUE.,.FALSE./
      DATA NOTTYP /'AN','BP','CL','CQ','FG','GC','IM','MC','PC','SN',
     *    'SU','TY','WX','BL','CP','PD','NX','FQ','CH','SY','CD'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Add beginning, end channel.
      IF (SELCH) THEN
         WRITE (HILINE,1011) TSKNAM, BCHAN, ECHAN
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       ANTENNAS
      CALL SETANT (50, XANT, XBASE, NNANT, NNBAS, ANT1, ANT2, DESEL)
      IC2 = 0
 110  IC1 = IC2 + 1
      IC2 = IC1 + 14
      IC2 = MIN (IC2, NNANT)
      IF (IC2.GE.IC1) THEN
         IF (IC1.EQ.1) THEN
            WRITE (HILINE,2110) TSKNAM, (ANT1(IC), IC=IC1,IC2)
         ELSE
            WRITE (HILINE,2111) TSKNAM, (ANT1(IC), IC=IC1,IC2)
            END IF
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         GO TO 110
         END IF
      IC2 = 0
 115  IC1 = IC2 + 1
      IC2 = IC1 + 14
      IC2 = MIN (IC2, NNBAS)
      IF (IC2.GE.IC1) THEN
         IF (IC1.EQ.1) THEN
            WRITE (HILINE,2130) TSKNAM, (ANT2(IC), IC=IC1,IC2)
         ELSE
            WRITE (HILINE,2131) TSKNAM, (ANT2(IC), IC=IC1,IC2)
            END IF
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         GO TO 115
         END IF
      IF (DESEL) THEN
         HILINE = TSKNAM // '/  baselines omitted'
      ELSE IF ((NNANT.GT.0) .OR. (NNBAS.GT.0)) THEN
         HILINE = TSKNAM // '/  baselines selected'
      ELSE
         HILINE = TSKNAM // '/  all baselines selected'
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Add UVRANGE
      WRITE (HILINE,1012) TSKNAM, UVRA
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       IF Selection
      WRITE (HILINE,1013) TSKNAM, BIF, EIF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       FQ Selection
      IF (FRQSEL.GT.0) THEN
         WRITE (HILINE,1014) TSKNAM, FRQSEL
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       QUAL
      WRITE (HILINE,1015) TSKNAM, QUAL(1)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       subarr
      IF (SUBARR.GT.0) THEN
         WRITE (HILINE,1016) TSKNAM, SUBARR
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Source selection
      IF (NSOUWD.LE.0) THEN
C                                       All
         WRITE (HILINE,3000) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
      ELSE
C                                       Give list
C                                       List included or excluded?
         EXCL = F
         DO 140 I = 1,30
            IF (XSOUR(I)(1:1).EQ.'-') THEN
               EXCL = T
C                                       Strip leading '-'
               NTEMP = XSOUR(I)(2:16)
               XSOUR(I) = NTEMP
               END IF
 140        CONTINUE
         IF (EXCL) THEN
            WRITE (HILINE,3001) TSKNAM
         ELSE
            WRITE (HILINE,3002) TSKNAM
            END IF
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       1st 2 and label.
         WRITE (HILINE,3003) TSKNAM, XSOUR(1), XSOUR(2)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Rest of sources
         IF (NSOUWD.GT.2) THEN
            DO 150 I = 1,NSOUWD,2
               WRITE (HILINE,3004) TSKNAM, XSOUR(I), XSOUR(I+1)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 200
 150           CONTINUE
            END IF
         END IF
C                                       flag table
      IF (FGVER.GT.0) THEN
         WRITE (HILINE,3005) TSKNAM, FGVER
      ELSE
         WRITE (HILINE,3006) TSKNAM
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       weight flag
      IF (BPARM(5).GT.0.0) THEN
         WRITE (HILINE,3010) TSKNAM, BPARM(5)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       table time extension
      WRITE (HILINE,3011) TSKNAM, BPARM(7)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Add any other history.
      IF (NUMHIS.GE.1) THEN
         WRITE (LABEL,1010) TSKNAM
         HILINE(1:8) = LABEL(1:8)
         DO 190 I = 1,NUMHIS
            HILINE(9:64) = HISCRD(I)(1:64)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 190        CONTINUE
         END IF
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                       Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST', BUFF1,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COPYHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/')
 1011 FORMAT (A6,'BCHAN =',I5,', ECHAN=',I5,
     *   ' / First and last ch. no.')
 1012 FORMAT (A6,'UVRANGE = ',1PE12.5,',',1PE12.5,
     *   ' / Kilowavelengths')
 1013 FORMAT (A6,'BIF =',I5,' EIF =',I5,' / Range of IFs copied')
 1014 FORMAT (A6,'FREQID =',I5,' / Selected FQ id.')
 1015 FORMAT (A6,'QUAL =',I5,' / Selected Qualifier')
 1016 FORMAT (A6,'SUBARRAY =',I3,' / Selected subarray -> #1')
 1020 FORMAT ('COPYHI: ERROR COPYING TABLES')
 2110 FORMAT (A6,'ANTENNAS = ',15(I2,','))
 2111 FORMAT (A6,'           ',15(I2,','))
 2130 FORMAT (A6,'BASELINE = ',15(I2,','))
 2131 FORMAT (A6,'           ',15(I2,','))
 3000 FORMAT (A6,'SOURCES = ''''     /All sources selected')
 3001 FORMAT (A6,'/Sources excluded:')
 3002 FORMAT (A6,'/Sources included:')
 3003 FORMAT (A6,'SOURCES = ''',A16,''',''',A16,'''')
 3004 FORMAT (A6,'         ,''',A16,''',''',A16,'''')
 3005 FORMAT (A6,'FLAGVER =',I4,'   / Flag table applied')
 3006 FORMAT (A6,'FLAGVER = -1    / No flag table applied')
 3010 FORMAT (A6,'UVCOPPRM(5)=',F11.4,'  / flag weights < BP(5)')
 3011 FORMAT (A6,'UVCOPPRM(7)=',F7.2,'  / table copy time extension')
      END
      SUBROUTINE COPYIT (NUMVIS, SAMTIM, VIS, RPARM, INCX, DONARR, IRET)
C-----------------------------------------------------------------------
C   NCOPYIT applies a clipping operation to the Visibilities using
C   XCOUNT(1) as a counter and XCOUNT(2) - XCOUNT(3) as the allowed
C   time range.  It also refuses to pass fully flagged samples unless
C   BPARM(1) is TRUE (> 0).  XCOUNT(4) is used to count the refusals.
C   Also checks that data in the requested uv range and that correct
C   data type is being copied.  Data with low weight can be flagged
C   with BPARM(5).
C       Correlators can also be selected using SELCH and DROPCH to
C   select the correlators to be dropped.
C   Inputs:
C      NUMVIS     I    Visibility number, -1 => final call, no data
C                      passed but allows any operations to be completed
C      SAMTIM     R    Time of current sample
C      VIS(3,*)   R    VIS record; RE, IM, WT for NTCOR samples
C   In/Out:
C      RPARM(*)   R    Random parameter array. When a single FQ ID
C                      is copied the FQ r.p in this array is changed.
C      INCX       I    Length of complex (1st) axis
C      DONARR     L    T -> an array > 1 has been dropped
C   Inputs from COMMON
C      TIME(8)    R    User time range
C      BPARM(10)  R    User array.
C      RA         D    Right ascension (1950) of phase center. (deg)
C      DEC        D    Declination (1950) of phase center. (deg)
C      FREQ       D    Frequency of observation (Hz)
C      NRPARM     I    # random parameters.
C      NCOR       I    # correlators
C      NTCOR      I    Total number of input correlators.
C      NSOUWD     I    Number of sources IDs to copy (0=>all)
C      SOUWAN     I(*) List of source IDs to copy.
C      DOSCL      L    If true rescale u,v,w
C      UVWSCL     R    U,v,w scaling factor
C      FRQSEL     I    Freq ID sel, .le.0 => all
C      SELCH      L    If .TRUE. some of the channels are to be dropped.
C      DROPCH(*)  L    If .TRUE. the corresponding correlator value is
C                      to be dropped.
C      ISCMP      L    If .TRUE. then data is compressed
C      KLOCWT     I    0-rel pointer for Weight random parameter
C      CATBLK(256)I    Catalog header record. See [DOC]HEADER.
C      NBASE      I    Number of baselines to check; 0=all OK.
C      ANT1(*)    I    First antenna numbers
C      ANT2(*)    I    Second antenna numbers
C      DOBOTH     L    Both xc & ac data copyied
C      DOXC       L    Only xc data
C      DOAC       L    Only ac data
C   Output:
C      VIS        R    Visibilities possibly with some flagged
C      IRET       I    Return code  -1 => don't write
C                                0 => OK
C                               >0 => error, terminate.
C   Output in COMMON
C      NUMHIS     I    # history entries (max. 10)
C      HISCRD(16,NUMHIS) R   History records
C      CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, INCX, IRET
      REAL      SAMTIM, VIS(INCX,*), RPARM(*)
      LOGICAL   DONARR
C
      REAL      UVDIS, TIME1, TIME2
      INTEGER   OUTP, I, IA1, IA2, ITIME(3), SFQID, SID, LOOP, NEWFQ,
     *   ISUB, NGOOD
      LOGICAL   GOTIT, DROP
      INCLUDE 'UVCOP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Select data
      IF (NUMVIS.LE.0) GO TO 200
C                                       Data type
         IF (ILOCB.GE.0) THEN
            IA1 = RPARM(1+ILOCB) / 256.0 + 0.1
            IA2 = RPARM(1+ILOCB) - IA1 * 256.0 + 0.1
            ISUB = (RPARM(1+ILOCB) - 256*IA1 - IA2) * 100.0 + 1.5
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            ISUB = RPARM(1+ILOCSA) + 0.1
            END IF
         IRET = -1
         IF (DOBOTH) IRET = 0
         IF (DOXC .AND. (IA1.NE.IA2)) IRET = 0
         IF (DOAC .AND. (IA1.EQ.IA2)) IRET = 0
C                                       subarray test
         IF ((SUBARR.GT.0) .AND. (ISUB.NE.SUBARR)) IRET = -1
         IF (IRET.EQ.-1) GO TO 999
C                                       Time
         IF (SAMTIM.LT.XCOUNT(2)) IRET = -1
         IF (IRET.EQ.-1) GO TO 999
         IF (SAMTIM.GT.XCOUNT(3)) IRET = -1
         IF (IRET.EQ.-1) GO TO 999
C                                       Subarray correction Plus Time
C                                       correction for single-source
         IF ((SUBARR.GT.0) .OR. (BPARM(2).GT.0.0)) THEN
            IF (ILOCB.GE.0) THEN
               RPARM(1+ILOCB) = 256 * IA1 + IA2
            ELSE
               RPARM(1+ILOCSA) = 1.0
               END IF
            IF ((ILOCSU.LT.0) .AND. (SAMTIM.GT.5.0)) SAMTIM =
     *         SAMTIM - 5.0 * (ISUB - 1)
            DONARR = BPARM(2).GT.0.0
            END IF
C                                       FQ selection
         IF (FRQSEL.GT.0) THEN
            NEWFQ = 1
            SFQID = RPARM(1+ILOCFQ) + 0.5
            IF (SFQID.NE.FRQSEL) THEN
               IRET = -1
               GO TO 999
               END IF
            RPARM(1+ILOCFQ) = NEWFQ
            END IF
C                                       Source selection
         IF (NSOUWD.GT.0) THEN
            SID = RPARM(1+ILOCSU) + 0.5
            GOTIT = .FALSE.
            DO 10 LOOP = 1,NSOUWD
               GOTIT = GOTIT .OR. (SID.EQ.SOUWAN(LOOP))
 10            CONTINUE
            IF (.NOT.GOTIT) THEN
               IRET = -1
               GO TO 999
               END IF
            END IF
C                                       Check UVRANGE
         UVDIS = RPARM(1+ILOCU) * RPARM(1+ILOCU) + RPARM(1+ILOCV) *
     *      RPARM(1+ILOCV)
         IF ((UVDIS.LT.UVRA2(1)) .OR. (UVDIS.GT.UVRA2(2))) IRET = -1
         IF (IRET.EQ.-1) GO TO 999
C                                       Baselines
         IF (NBASE.GT.0) THEN
            DO 20 I = 1,NBASE
               IF ((IA1.EQ.ANT1(I)) .AND. (IA2.EQ.ANT2(I))) GO TO 25
               IF (((IA1.EQ.ANT1(I)) .OR. (IA2.EQ.ANT1(I))) .AND.
     *            (ANT2(I).EQ.0)) GO TO 25
 20            CONTINUE
C                                       Not wanted
            IRET = -1
            GO TO 999
            END IF
C                                       flagging
 25      IF (DOFLAG) THEN
            CALL DATFLG (RPARM, VIS, DROP, IRET)
         ELSE
            DROP = .FALSE.
            END IF
C                                       Check flagging
         IF ((IRET.GE.0) .AND. (BPARM(1).LE.0.)) THEN
C                                       Check for weights
            IF ((INCX.GT.2) .AND. (.NOT.DROP)) THEN
               NGOOD = 0
               DO 30 I = 1,NTCOR
                  IF (.NOT.DROPCH(I)) THEN
                     IF (VIS(3,I).GT.BPARM(5)) THEN
                        NGOOD = NGOOD + 1
                     ELSE
                        VIS(3,I) = -ABS (VIS(3,I))
                        END IF
                     END IF
 30              CONTINUE
               IF (NGOOD.LE.0) DROP = .TRUE.
               END IF
            IF (DROP) THEN
               IRET = -1
               XCOUNT(4) = XCOUNT(4) + 1.0D0
               END IF
            END IF
C                                       Count net output records
         XCOUNT(1) = XCOUNT(1) + IRET + 1.0D0
         IF (IRET.LT.0) GO TO 999
C                                       Rescale u,v,w?
         IF (DOSCL) THEN
            RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVWSCL
            RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVWSCL
            RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVWSCL
            END IF
C                                       If selecting channels shuffle
C                                       data.
         IF (SELCH) THEN
            OUTP = 0
            DO 150 I = 1,NTCOR
C                                       Want data.
               IF (.NOT.DROPCH(I)) THEN
                  OUTP = OUTP + 1
                  VIS(1,OUTP) = VIS(1,I)
                  IF (INCX.GE.2) VIS(2,OUTP) = VIS(2,I)
                  IF (INCX.GE.3) VIS(3,OUTP) = VIS(3,I)
                  END IF
 150           CONTINUE
            END IF
         GO TO 999
C                                        History cards
 200  IF (XCOUNT(2).GT.-1.0D6) THEN
         TIME1 = TIME(1) + TIME(2)/24. + TIME(3)/(24.*60.) +
     *      TIME(4)/(24.*3600.)
         ITIME(1) = TIME1
         TIME1 = 24. * (TIME1 - ITIME(1))
         ITIME(2) = TIME1
         TIME1 = 60. * (TIME1 - ITIME(2))
         ITIME(3) = TIME1
         TIME1 = 60. * (TIME1 - ITIME(3))
         WRITE (MSGTXT,1200) ITIME, TIME1
      ELSE
         WRITE (MSGTXT,1201)
         END IF
      CALL MSGWRT (4)
      HISCRD(1)(1:64) = MSGTXT(1:64)
      IF ((XCOUNT(3).GT.-1.0D6)
     *      .AND. (XCOUNT(3) .LT. 0.999D6)) THEN
         TIME2 = TIME(5) + TIME(6)/24. + TIME(7)/(24.*60.) +
     *      TIME(8)/(24.*3600.)
         ITIME(1) = TIME2
         TIME2 = 24. * (TIME2 - ITIME(1))
         ITIME(2) = TIME2
         TIME2 = 60. * (TIME2 - ITIME(2))
         ITIME(3) = TIME2
         TIME2 = 60. * (TIME2 - ITIME(3))
         WRITE (MSGTXT,1202) ITIME, TIME2
      ELSE
         WRITE (MSGTXT,1203)
         END IF
      CALL MSGWRT (4)
      HISCRD(2)(1:64) = MSGTXT(1:64)
      WRITE (MSGTXT,1204) XCOUNT(1)
      CALL MSGWRT (4)
      HISCRD(3)(1:64) = MSGTXT(1:64)
      IF (BPARM(1).GT.0.) WRITE (MSGTXT,1205)
      IF (BPARM(1).LE.0.) WRITE (MSGTXT,1206) XCOUNT(4)
      CALL MSGWRT (4)
      HISCRD(4)(1:64) = MSGTXT(1:64)
      NUMHIS = 4
      IF (DONARR) THEN
         WRITE (MSGTXT,1207)
         CALL MSGWRT (4)
         HISCRD(5)(1:64) = MSGTXT(1:64)
         NUMHIS = 5
         END IF
C                                       error check
      IF (XCOUNT(1).LE.0.0) IRET = 99
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('Copied from time=',I5,2I3.2,F6.2)
 1201 FORMAT ('Copied from the beginning')
 1202 FORMAT ('        to  time=',I5,2I3.2,F6.2)
 1203 FORMAT ('        to  the end')
 1204 FORMAT ('Copied',F10.0,' vis records')
 1205 FORMAT ('Can include fully flagged samples')
 1206 FORMAT ('Dropped',F9.0,' flagged vis records')
 1207 FORMAT ('Dropped subarray designations > 1')
      END
      SUBROUTINE COPTAB (IRET)
C-----------------------------------------------------------------------
C   Updates tables for selection by IF
C   Inputs in common:
C      BIF   I  First IF
C      EIF   I  Highest IF selected
C      FQOFF D  Frequency offset
C      SELIF L  Select IFs or not
C   Output:
C      IRET  I  Return code, 0=>OK
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IERR, LUN1, LUN2, VER, NVER, OFQID, ISUB, JSUB, IV,
     *   TFLAG, IROUND, BPOL, EPOL, BVER
      LOGICAL   TABLE, EXIST, FITASC
      DOUBLE PRECISION T1, T2
      INCLUDE 'UVCOP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      DATA LUN1, LUN2 /28,29/
C-----------------------------------------------------------------------
      MSGTXT = 'Updating tables for IF/FREQID/channel selection'
      CALL MSGWRT (4)
      IF (SUBARR.GT.0) THEN
         ISUB = SUBARR
         JSUB = 1
      ELSE
         ISUB = 0
         JSUB = -1
         IF (BPARM(2).GT.0.0) JSUB = 1
         END IF
      TFLAG = IROUND (BPARM(6))
C                                       STOKES selection not allowed
C                                       these ok even for 1 pol data
      BPOL = 1
      EPOL = 2
C                                       allow 15 min extra to be sure
C                                       that one gets all needed rows
      T2 = BPARM(7) / (24.0D0 * 60.0D0)
      T1 = XCOUNT(2) - T2
      T2 = XCOUNT(3) + T2
C                                       Revise tables: note depends on
C                                       doing loops 0 times if none
C                                       AN tables
C                                       Reference frequency in AN table
      CALL FNDEXT ('AN', CATOLD, NVER)
      IF (JSUB.EQ.1) NVER = 1
      DO 20 VER = 1,NVER
         IV = VER
         IF (SUBARR.GT.0) IV = SUBARR
         CALL ISTAB ('AN', DISKIN, FCNO(2), IV, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL ANSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), IV, VER, CATOLD, CATBLK, LUN1, LUN2, BIF,
     *      EIF, FQOFF, DOPOL, IBUFF1, IBUFF2, IRET)
 20      CONTINUE
      OFQID = FRQSEL
C                                       BP tables
      CALL FNDEXT ('BP', CATOLD, NVER)
      DO 120 VER = 1,NVER
         CALL ISTAB ('BP', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL BPSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, BCHAN, ECHAN, T1, T2, OFQID, ISUB, JSUB, IBUFF1,
     *      IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 120     CONTINUE
C                                       CL tables
      CALL FNDEXT ('CL', CATOLD, NVER)
      DO 140 VER = 1,NVER
         CALL ISTAB ('CL', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
C                                       do NOT select on sources
         IF (EXIST .AND. (IERR.EQ.0)) CALL CLSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, T1, T2, 0, SOUWAN, AN, NA, ISUB, JSUB,
     *      IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 140     CONTINUE
C                                       CD tables
      CALL FNDEXT ('CD', CATUV, NVER)
      DO 145 VER = 1,NVER
         CALL ISTAB ('CD', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
C                                       do not select on antenna
         IF (EXIST.AND.(IERR.EQ.0)) CALL CDSEL (DISKIN, FCNO(2), DISKO,
     *      FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL, BIF,
     *      EIF, OFQID, AN, 0, ISUB, JSUB, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 145     CONTINUE
C                                       CP tables
      CALL FNDEXT ('CP', CATOLD, NVER)
      DO 150 VER = 1,NVER
         CALL ISTAB ('CP', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST.AND.(IERR.EQ.0)) CALL CPSEL (DISKIN, FCNO(2), DISKO,
     *      FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BIF, EIF, BCHAN,
     *      ECHAN, OFQID, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 150     CONTINUE
C                                       CQ tables
      CALL FNDEXT ('CQ', CATOLD, NVER)
      DO 160 VER = 1, NVER
         CALL ISTAB ('CQ', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST.AND.(IERR.EQ.0)) CALL CQSEL (DISKIN, FCNO(2), DISKO,
     *      FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BIF, EIF, OFQID,
     *      ISUB, JSUB, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 160     CONTINUE
C                                       FG tables
      CALL FNDEXT ('FG', CATOLD, NVER)
C                                       copy FG tables if flaging was
C                                       not applied
      CALL FNDEXT ('FG', CATUV, NVER)
      IF ((FGVER.GT.0) .AND. (NVER.GT.0)) THEN
         BVER = FGVER + 1
         IF (NVER.LE.FGVER) THEN
            MSGTXT = 'WARNING: NO FG TABLES ARE COPIED SINCE HIGHEST' //
     *         ' WAS APPLIED'
            NVER = 0
         ELSE
            WRITE (MSGTXT,1160) BVER, NVER
            END IF
         CALL MSGWRT (6)
         END IF
      DO 180 VER = BVER,NVER
         CALL ISTAB ('FG', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0) .AND. (VER.GT.FGVER))
     *      CALL FGSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER, CATOLD,
     *      CATBLK, LUN1, LUN2, BIF, EIF, BCHAN, ECHAN,  T1, T2, OFQID,
     *      ISUB, JSUB, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 180     CONTINUE
C                                       GC tables
      CALL FNDEXT ('GC', CATOLD, NVER)
      DO 200 VER = 1,NVER
         CALL ISTAB ('GC', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL GCSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BCHAN, ECHAN, BIF, EIF, OFQID, AN, NA, ISUB, JSUB, IBUFF1,
     *      IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 200     CONTINUE
C                                       IM tables
      CALL FNDEXT ('IM', CATOLD, NVER)
      DO 220 VER = 1,NVER
         CALL ISTAB ('IM', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL IMSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, T1, T2, NSOUWD, SOUWAN, AN, NA, ISUB, JSUB,
     *      IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 220     CONTINUE
C                                       MC tables
      CALL FNDEXT ('MC', CATOLD, NVER)
      DO 240 VER = 1,NVER
         CALL ISTAB ('MC', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL MCSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, T1, T2, NSOUWD, SOUWAN, AN, NA, ISUB, JSUB,
     *      IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 240     CONTINUE
C                                       PC tables
      CALL FNDEXT ('PC', CATOLD, NVER)
      DO 260 VER = 1,NVER
         CALL ISTAB ('PC', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL PCSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, T1, T2, NSOUWD, SOUWAN, AN, NA, ISUB, JSUB,
     *      IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 260     CONTINUE
C                                       PD tables
      CALL FNDEXT ('PD', CATOLD, NVER)
      DO 270 VER = 1,NVER
         CALL ISTAB ('PD', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL PDSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, BCHAN, ECHAN, OFQID, ISUB, JSUB, IBUFF1, IBUFF2,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
 270     CONTINUE
C                                       SN tables
      CALL FNDEXT ('SN', CATOLD, NVER)
      DO 280 VER = 1,NVER
         CALL ISTAB ('SN', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) THEN
            IF ((DOFLAG) .AND. (MOD(TFLAG/2,2).NE.1)) THEN
               CALL SNFSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER,
     *            CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, EIF,
     *            OFQID, T1, T2, ISUB, JSUB, IBUFF1, IBUFF2, IRET)
            ELSE
               CALL SNSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER,
     *            CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, EIF,
     *            OFQID, T1, T2, ISUB, JSUB, IBUFF1, IBUFF2, IRET)
               END IF
            IF (IRET.GT.0) GO TO 999
            END IF
 280     CONTINUE
C                                       SY tables
      CALL FNDEXT ('SY', CATUV, NVER)
      DO 290 VER = 1,NVER
         CALL ISTAB ('SY', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) THEN
            IF ((DOFLAG) .AND. (MOD(TFLAG,2).NE.1)) THEN
               CALL SYFSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER, CATUV,
     *            CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, EIF, OFQID, T1,
     *            T2, NSOUWD, SOUWAN, AN, NA, ISUB, JSUB, IBUFF1,
     *            IBUFF2, IRET)
            ELSE
               CALL SYSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER, CATUV,
     *            CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, EIF, OFQID, T1,
     *            T2, NSOUWD, SOUWAN, AN, NA, ISUB, JSUB, IBUFF1,
     *            IBUFF2, IRET)
               END IF
            IF (IRET.GT.0) GO TO 999
            END IF
 290     CONTINUE
C                                       SU tables: all sources
      CALL FNDEXT ('SU', CATOLD, NVER)
      DO 300 VER = 1,NVER
         CALL ISTAB ('SU', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL SUSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BIF, EIF,
     *      OFQID, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 300     CONTINUE
C                                       TY tables
      CALL FNDEXT ('TY', CATOLD, NVER)
      DO 320 VER = 1,NVER
         CALL ISTAB ('TY', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) THEN
            IF ((DOFLAG) .AND. (MOD(TFLAG,2).NE.1)) THEN
               CALL TYFSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER,
     *            CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, EIF,
     *            OFQID, T1, T2, AN, NA, ISUB, JSUB, IBUFF1, IBUFF2,
     *            IRET)
            ELSE
               CALL TYSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER,
     *            CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, EIF,
     *            OFQID, T1, T2, AN, NA, ISUB, JSUB, IBUFF1, IBUFF2,
     *            IRET)
               END IF
            IF (IRET.GT.0) GO TO 999
            END IF
 320     CONTINUE
C                                       WX tables
      CALL FNDEXT ('WX', CATOLD, NVER)
      DO 340 VER = 1,NVER
         CALL ISTAB ('WX', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL WXSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, T1, T2, AN,
     *      NA, ISUB, JSUB, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 340     CONTINUE
C                                       BL tables
      CALL FNDEXT ('BL', CATOLD, NVER)
      DO 350 VER = 1,NVER
         CALL ISTAB ('BL', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL BLSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      AN, NA, ISUB, JSUB, BIF, EIF, OFQID, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 350     CONTINUE
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, FCNO(1), DIFPIX, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'COPYHI: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1160 FORMAT ('WARNING: ONLY FG TABLE VERSIONS',I4,' TO',I4,' COPIED')
      END
      SUBROUTINE ESTSIZ (CNO, NEVIS, IRET)
C-----------------------------------------------------------------------
C   Routine to examine the NX table and based on time ranges, FREQID
C   and selected sources' list determine the estimated number of
C   visibilities in the output file.
C   Input:
C       CNO          I         Cat. # of file
C   Input from common:
C       MULTY        L         Multy- or single- source data
C       NSOUWD       I         Number of selected sources
C       XCOUNT(2)    D         Start time
C       XCOUNT(3)    D         Stop time
C       FRQSEL       I         FQ ID to use, < 1 => all
C       DISKIN       I         Vol containing input file
C       OLDCNO       I         Cat. number of input file
C   Output:
C       NEVIS        I         Estimated # visibilities
C       IRET         I         Error code, 0 => OK
C-----------------------------------------------------------------------
      INCLUDE 'UVCOP.INC'
      INTEGER NEVIS, CNO, IRET
C
      INTEGER NXVER, LUNI, IERR, K, NUMBNX, I, IDSOUR, ISUB, VSTART,
     *   VEND, FREQID, SUMVIS, BUFFNX(512), RTMP
      REAL    RTIME, DTIME
      LOGICAL TABLE, EXIST, FITASC, OURS, OURST
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI /16/
C-----------------------------------------------------------------------
      IRET = 0
      NEVIS = CATBLK(KIGCN)
C                                       Is this multi-source?
      CALL MULSDB (CATBLK, MULTI)
      IF (.NOT.MULTI) THEN
         IF (DOAC .AND. (XNAC.GT.0.0)) THEN
            RTMP = NEVIS
            NEVIS = XNAC * RTMP
            RTMP = NEVIS
            NEVIS = RTMP + 0.1 * RTMP
            END IF
         GO TO 999
         END IF
C                                       Is there an NX table
      NXVER = 1
      CALL ISTAB ('NX', DISKIN, CNO, NXVER, LUNI, BUFFNX, TABLE,
     *   EXIST, FITASC, IERR)
      IF (.NOT.EXIST) THEN
         NEVIS = CATBLK(KIGCN)
         GO TO 999
         END IF
C                                       Open the table
      CALL NDXINI ('READ', BUFFNX, DISKIN, CNO, NXVER, CATBLK,
     *   LUNI, INXRNO, NXKOLS, NXNUMV, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       How many entries
      NUMBNX = BUFFNX(5)
      SUMVIS = 0
      DO 100 I = 1, NUMBNX
         CALL TABNDX ('READ', BUFFNX, INXRNO, NXKOLS, NXNUMV, RTIME,
     *      DTIME, IDSOUR, ISUB, VSTART, VEND, FREQID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       Check on FREQID
         OURS = ((FRQSEL.LE.0) .OR. (FREQID .EQ. FRQSEL))
C                                       Check time
         OURS = OURS .AND. (((RTIME+DTIME*0.5) .GE. XCOUNT(2)) .AND.
     *      ((RTIME-DTIME*0.5) .LE. XCOUNT(3)))
C                                       Check sources if MULTI
         OURST = .FALSE.
         IF (NSOUWD.EQ.0) OURST = .TRUE.
         DO 80 K = 1, NSOUWD
            IF (IDSOUR .EQ. SOUWAN(K)) OURST = .TRUE.
   80       CONTINUE
         IF (OURS .AND. OURST) SUMVIS = SUMVIS + (VEND - VSTART + 1)
 100     CONTINUE
      NEVIS = SUMVIS
      IF (DOAC .AND. (XNAC.GT.0.0)) THEN
         RTMP = NEVIS
         NEVIS = XNAC * RTMP
         RTMP = NEVIS
         NEVIS = RTMP + 0.1 * RTMP
         END IF
      CALL TABNDX ('CLOS', BUFFNX, INXRNO, NXKOLS, NXNUMV, TIME,
     *   DTIME, IDSOUR, ISUB, VSTART, VEND, FREQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ESTSIZ: ERROR ',I3,' OPENING NX FILE')
 1010 FORMAT ('ESTSIZ: ERROR ',I3,' READING NX FILE')
 1020 FORMAT ('ESTSIZ: ERROR ',I3,' CLOSING NX FILE')
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
         A1 = KBASE / 256
         A2 = KBASE - 256 * A1
         SUBA = (RPARM(1+ILOCB) - KBASE) * 100.0 + 1.5
      ELSE
         A1 = RPARM(1+ILOCA1) + 0.1
         A2 = RPARM(1+ILOCA2) + 0.1
         SUBA = RPARM(1+ILOCSA) + 0.1
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
C      FLGBAS   I(*)   Baseline (A1*256+A2) numbers to flag, 0=all.
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
      SUBROUTINE PRMSET (CATBLK, TRN)
C-----------------------------------------------------------------------
C   Drops REMOVED from random parameter list and makes list of output
C   indices
C   In/Out:
C      CATBLK   I(256)   Catalog header - input random parms can have
C                        REMOVED  - output does not (and # changed)
C   Output:
C      TRN      I(14)    output parm(j) = input parm(trn(j))
C-----------------------------------------------------------------------
      INTEGER   CATBLK(256), TRN(*)
C
      INTEGER   NPI, NPO, I
      CHARACTER TYPE*8
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      NPO = 0
      NPI = CATBLK(KIPCN)
      DO 20 I = 1,NPI
         CALL H2CHR (8, 1, CATBLK(KHPTP+2*I-2), TYPE)
         IF (TYPE.NE.'REMOVED') THEN
            NPO = NPO + 1
            TRN(NPO) = I
            CALL CHR2H (8, TYPE, 1, CATBLK(KHPTP+2*NPO-2))
            END IF
 20      CONTINUE
      CATBLK(KIPCN) = NPO
C
 999  RETURN
      END
      SUBROUTINE TYFSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, TB, TE, AN, NA, ISUB,
     *   JSUB, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies a subset of IFs in a TY table, can also modify the FQ ID
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      VER      I        Version to check/modify
C      CATIN    I(256)   Input catalog header
C      LUNI     I    p   LUN to use
C      LUNO     I        LUN to use
C      BIF      I        Start IF number
C      EIF      I        End IF number
C      IFQID    I        FQ ID to select (output value is 1)
C                        if <= 0 then output value unchanged.
C      TB       D        Beginning time in days
C      TE       D        Ending time in days
C      AN       I(*)     Array of selected antennas
C      NA       I        Number of selected antennas
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      CATOUT   I(256)   Output catalog header
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, AN(8), NA, ISUB, JSUB,
     *   BUFFER(*), OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ITYRNO, TYKOLS(MAXTYC), TYNUMV(MAXTYC), NUMPOL, NUMIF,
     *   OKOLS(MAXTYC), ONUMV(MAXTYC), NTYROW, I, OVER, SOURID, ANTNO,
     *   SUBA, FREQID, OTYRNO, NEWNIF, IIF, JIF, IPOL, K, NDEL, NTOT,
     *   JRET, LBPOL, NEWPOL
      LOGICAL   REFMT, GOTIT, GOTONE
      REAL      TIME, TIMEI, TSYS(2,MAXIF), TANT(2,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
C                                       open flag table
      TMFLST = -1.E20
      NUMFLG = 0
      IFGRNO = 1
      NDEL = 0
      NTOT = 0
C                                       Open TY file
      CALL TYINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, ITYRNO,
     *   TYKOLS, TYNUMV, NUMPOL, NUMIF, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       New no. of IFs
      NEWNIF = MAX (MIN (NUMIF, EIF) - BIF + 1, 0)
      LBPOL = MAX (1, BPOL)
      NEWPOL = MIN (2, MIN (NUMPOL, EPOL)) - LBPOL + 1
      IF (NEWPOL.LE.0) THEN
         NEWPOL = NUMPOL
         LBPOL = 1
         END IF
      REFMT = (NEWNIF.NE.NUMIF) .OR. (NEWPOL.NE.NUMPOL)
C                                       # rows in old table
      NTYROW = BUFFER(5)
C                                       Open up new TY table
      OVER = VER
      CALL TYINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OTYRNO, OKOLS, ONUMV, NEWPOL, NEWNIF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NTYROW
         CALL TABTY ('READ', BUFFER, ITYRNO, TYKOLS, TYNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, TSYS, TANT,
     *      IRET)
C                                       Error reading table
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
C                                       check subarray, time
         ELSE IF (IRET.EQ.0) THEN
            CALL TYFLG (NUMPOL, NUMIF, TIME, SOURID, ANTNO, SUBA,
     *         FREQID, TSYS, TANT, JRET)
            IF (JRET.GT.0) THEN
               IRET = JRET
               GO TO 999
               END IF
            IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *         IRET = -1
            IF ((TIME.LT.TB) .OR. (TIME.GT.TE)) IRET = -1
            IF ((IFQID.GT.0) .AND. (FREQID.GT.0) .AND.
     *         (IFQID.NE.FREQID)) IRET = -1
C                                       antenna
            IF ((NA.GT.0) .AND. (ANTNO.GT.0)) THEN
               GOTIT = .FALSE.
               DO 30 K = 1,NA
                  GOTIT = GOTIT .OR. (ANTNO.EQ.AN(K))
 30               CONTINUE
               IF (.NOT.GOTIT) IRET = -1
               END IF
            IF ((JRET.EQ.-1) .AND. (IRET.EQ.0)) THEN
               IRET = -1
               NDEL = NDEL + 1
               END IF
            END IF
C                                       Is this record selected ?
         IF ((IRET.LT.0) .OR. (NEWNIF.EQ.0)) THEN
            REFMT = .TRUE.
C                                       Select IFs
         ELSE
            GOTONE = .FALSE.
            DO 90 JIF = 1,NEWNIF
               IIF = JIF + BIF - 1
               DO 80 IPOL = 1,NEWPOL
                  K = IPOL + LBPOL - 1
                  TSYS(IPOL,JIF) = TSYS(K,IIF)
                  TANT(IPOL,JIF) = TANT(K,IIF)
                  IF (TSYS(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
                  IF (TANT(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
 80               CONTINUE
 90            CONTINUE
C                                       Write new one
            IF (GOTONE) THEN
               IF (IFQID.GT.0) FREQID = 1
               IF (JSUB.GE.0) SUBA = JSUB
               NTOT = NTOT + 1
               CALL TABTY ('WRIT', OBUFF, OTYRNO, OKOLS, ONUMV, NUMPOL,
     *            NEWNIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *            TSYS, TANT, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1020) IRET
                  GO TO 990
                  END IF
            ELSE
               REFMT = .TRUE.
               END IF
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ITYRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OTYRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted TY', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied TY', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         NTOT = NTOT + NDEL
         WRITE (MSGTXT,1101) NDEL, NTOT
         CALL REFRMT (MSGTXT, '_', I)
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('TYFSEL: ERROR ',I3,' RETURNED FROM TYINI')
 1020 FORMAT ('TYFSEL: ERROR ',I3,' RETURNED FROM TABTY')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
 1101 FORMAT ('__Fully deleted',I10,' of',I12,' TY records applying',
     *   ' flag table')
      END
      SUBROUTINE TYFLG (NPOL, NIF, TIME, SOURID, ANTNO, SUBA,
     *   FREQID, TSYS, TANT, IRET)
C-----------------------------------------------------------------------
C   Flags a TY table row based on the flags in FG table loaded to Common
C   Inputs:
C      NPOL     I      Number polarizations in TY data
C      NIF      I      Number of IFs in those data
C      TIME     R      Time of table row
C      SOURID   I      Source number of row
C      ANTNO    I      Antenna number of row
C      SUBA     I      Subarray of row
C      FREQID   I      Frequency ID if row
C   In/Out:
C      TSYS     R(*)   System temperature array - flagged -> FBLANK
C      TANT     R(*)   Antenna temperature array
C   Inputs from include DSEL.INC:
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
C      IRET     I      0 -> okay, -1 -> all flagged
C-----------------------------------------------------------------------
      INTEGER   NPOL, NIF, SOURID, ANTNO, SUBA, FREQID, IRET
      REAL      TIME, TSYS(2,*), TANT(2,*)
C
      INTEGER   IFLAG, FLGA, JPOLN, JIF, LIMF1, LIMF2, IPOLPT
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (TMFLST.LT.TIME) CALL NXTFLG (TIME, .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NUMFLG.LE.0) GO TO 999
C                                       loop over current flags
      DO 50 IFLAG = 1,NUMFLG
C                                       Check time if needed
         IF ((TIME.LT.FLGTST(IFLAG)) .OR. (TIME.GT.FLGTND(IFLAG)))
     *      GO TO 50
C                                       Check source
         IF ((FLGSOU(IFLAG).NE.SOURID) .AND. (FLGSOU(IFLAG).NE.0) .AND.
     *      (SOURID.NE.0)) GO TO 50
C                                       Check antenna
         FLGA = FLGANT(IFLAG)
         IF ((FLGA.NE.0) .AND. (FLGA.NE.ANTNO)) GO TO 50
C                                       Check subarray
         IF ((FLGSUB(IFLAG).GT.0) .AND. (FLGSUB(IFLAG).NE.SUBA))
     *      GO TO 50
C                                       Check freqid.: may be changed
C                                       from input to 1 already
         IF ((FLGFQD(IFLAG).GT.0) .AND. (FLGFQD(IFLAG).NE.FREQID) .AND.
     *      (FREQID.GT.0)) GO TO 50
C                                       Some data to be flagged
C                                       Set limits
         LIMF1 = FLGBIF(IFLAG)
         LIMF2 = FLGEIF(IFLAG)
C                                       Loop over polarizations
         IPOLPT = ABS(KCOR0) - 1
         IF (KCOR0.LT.-4) IPOLPT = IPOLPT - 4
         DO 40 JPOLN = 1,NPOL
            IF (FLGPOL(JPOLN+IPOLPT,IFLAG)) THEN
C                                       Loop over IF
               DO 30 JIF = LIMF1,LIMF2
                  TSYS(JPOLN,JIF) = FBLANK
                  TANT(JPOLN,JIF) = FBLANK
 30               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Check if data all
      DO 70 JPOLN = 1,NPOL
         DO 60 JIF = 1,NIF
            IF ((TSYS(JPOLN,JIF).NE.FBLANK) .OR.
     *         (TANT(JPOLN,JIF).NE.FBLANK)) GO TO 999
 60         CONTINUE
 70      CONTINUE
      IRET = -1
C
 999  RETURN
      END
      SUBROUTINE SNFSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, TB, TE, ISUB, JSUB,
     *   BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies a subset of IFs in a SN table, can also modify the FQ ID
C   Applies flagging as well
C   Inputs:
C      DISKI           I       Input volume number
C      CNOI            I       Input catalog number
C      DISKO           I       Output volume number
C      CNOO            I       Output catalog number
C      VER             I       Version to check/modify
C      CATIN(256)      I       Input catalog header
C      CATOUT(256)     I       Output catalog header
C      LUNI            I       LUN to use
C      LUNO            I       LUN to use
C      BIF             I       Start IF number
C      EIF             I       End IF number
C      IFQID           I       FQ ID to select (output value is 1)
C                              if <= 0 then output value unchanged.
C      TB       D        Beginning time in days
C      TE       D        Ending time in days
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      BUFFER          I(*)    Work buffer
C      OBUFF           I(*)    Work buffer
C   Output:
C      IRET            I       Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, ISUB, JSUB, BUFFER(*),
     *   OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ISNRNO, SNKOLS(MAXSNC), SNNUMV(MAXSNC), NUMANT, NUMPOL,
     *   NUMIF, NUMNOD, OKOLS(MAXSNC), ONUMV(MAXSNC), NSNROW, I, OVER,
     *   SOURID, ANTNO, SUBA, FREQID, NODENO, REFA(2,MAXIF), OSNRNO,
     *   NEWNIF, IIF, JIF, IPOL, NDEL, NTOT, JRET, LBPOL, NEWPOL, K,
     *   LBIF
      LOGICAL   ISAPPL, REFMT, GOTONE
      REAL      GMMOD, RANOD(25), DECNOD(25), TIMEI, IFR, MBDELY(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF), DISP(2), DDISP(2)
      DOUBLE PRECISION TIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
C                                       open flag table
      TMFLST = -1.E20
      NUMFLG = 0
      IFGRNO = 1
      NDEL = 0
      NTOT = 0
C                                       Open SN file
      CALL SNINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       New no. of IFs
      LBIF = MAX (1, BIF)
      NEWNIF = MIN (NUMIF, EIF) - BIF + 1
      IF (NEWNIF.LE.0) THEN
         NEWNIF = NUMIF
         LBIF = 1
         END IF
      LBPOL = MAX (1, BPOL)
      NEWPOL = MIN (2, MIN (NUMPOL, EPOL)) - LBPOL + 1
      IF (NEWPOL.LE.0) THEN
         NEWPOL = NUMPOL
         LBPOL = 1
         END IF
      REFMT = (NEWNIF.NE.NUMIF) .OR. (NEWPOL.NE.NUMPOL)
C                                       # rows in old table
      NSNROW = BUFFER(5)
C                                       Open up new SN table
      OVER = VER
      CALL SNINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OSNRNO, OKOLS, ONUMV, NUMANT, NEWPOL, NEWNIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NSNROW
         CALL TABSN ('READ', BUFFER, ISNRNO, SNKOLS, SNNUMV, NUMPOL,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
C                                       check subarray and FQ
         IF (IRET.EQ.0) THEN
            CALL SNFLG (NUMPOL, NUMIF, TIME, SOURID, ANTNO, SUBA,
     *         FREQID, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, JRET)
            IF (JRET.GT.0) THEN
               IRET = JRET
               GO TO 999
               END IF
            IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *         IRET = -1
            IF ((IFQID.GT.0) .AND. (FREQID.GT.0) .AND.
     *         (IFQID.NE.FREQID)) IRET = -1
            IF ((TIME.LT.TB) .OR. (TIME.GT.TE)) IRET = -1
            IF ((JRET.EQ.-1) .AND. (IRET.EQ.0)) THEN
               IRET = -1
               NDEL = NDEL + 1
               END IF
            END IF
C                                       Is this record selected ?
         IF ((IRET.LT.0) .OR. (NEWNIF.EQ.0)) THEN
            REFMT = .TRUE.
C                                       Error reading table
         ELSE IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
C                                       Select IFs
         ELSE
            GOTONE = .FALSE.
            DO 90 JIF = 1,NEWNIF
               IIF = JIF + LBIF - 1
               DO 80 IPOL = 1,NEWPOL
                  K = IPOL + LBPOL - 1
                  CREAL(IPOL,JIF) = CREAL(K,IIF)
                  CIMAG(IPOL,JIF) = CIMAG(K,IIF)
                  DELAY(IPOL,JIF) = DELAY(K,IIF)
                  RATE(IPOL,JIF) = RATE(K,IIF)
                  WEIGHT(IPOL,JIF) = WEIGHT(K,IIF)
                  REFA(IPOL,JIF) = REFA(K,IIF)
                  IF (CREAL(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
 80               CONTINUE
 90            CONTINUE
C                                       Write new one
            IF (GOTONE) THEN
               IF (IFQID.GT.0) FREQID = 1
               IF (JSUB.GE.0) SUBA = JSUB
               NTOT = NTOT + 1
               CALL TABSN ('WRIT', OBUFF, OSNRNO, OKOLS, ONUMV, NEWPOL,
     *            TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *            MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *            WEIGHT, REFA, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1020) IRET
                  GO TO 990
                  END IF
            ELSE
               REFMT = .TRUE.
               END IF
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OSNRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted SN', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied SN', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         NTOT = NTOT + NDEL
         WRITE (MSGTXT,1101) NDEL, NTOT
         CALL REFRMT (MSGTXT, '_', I)
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SNSEL: ERROR ',I3,' RETURNED FROM SNINI')
 1020 FORMAT ('SNSEL: ERROR ',I3,' RETURNED FROM TABSN')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
 1101 FORMAT ('__Fully deleted',I10,' of',I12,' SN records applying',
     *   ' flag table')
      END
      SUBROUTINE SNFLG (NPOL, NIF, TIME, SOURID, ANTNO, SUBA,
     *   FREQID, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
C-----------------------------------------------------------------------
C   Flags a SN table row based on the flags in FG table loaded to Common
C   Inputs:
C      NPOL     I        Number polarizations in TY data
C      NIF      I        Number of IFs in those data
C      TIME     D        Time of table row
C      SOURID   I        Source number of row
C      ANTNO    I        Antenna number of row
C      SUBA     I        Subarray of row
C      FREQID   I        Frequency ID if row
C   In/Out:
C      CREAL    R(2,*)   Real part of solution
C      CIMAG    R(2,*)   Imaginary part of solution
C      DELAY    R(2,*)   Delay
C      RATE     R(2,*)   Rate
C      WEIGHT   R(2,*)   Solution weight
C      REFA     I(2,*)   Reference antenna
C   Inputs from include DSEL.INC:
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
C      IRET     I      0 -> okay, -1 -> all flagged
C-----------------------------------------------------------------------
      DOUBLE PRECISION TIME
      INTEGER   NPOL, NIF, SOURID, ANTNO, SUBA, FREQID, REFA(2,*), IRET
      REAL      CREAL(2,*), CIMAG(2,*), DELAY(2,*), RATE(2,*),
     *   WEIGHT(2,*)
C
      INTEGER   IFLAG, FLGA, JPOLN, JIF, LIMF1, LIMF2, IPOLPT
      REAL      RTIME
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      RTIME = TIME
      IF (TMFLST.LT.TIME) CALL NXTFLG (RTIME, .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NUMFLG.LE.0) GO TO 999
C                                       loop over current flags
      DO 50 IFLAG = 1,NUMFLG
C                                       Check time if needed
         IF ((TIME.LT.FLGTST(IFLAG)) .OR. (TIME.GT.FLGTND(IFLAG)))
     *      GO TO 50
C                                       Check source
         IF ((FLGSOU(IFLAG).NE.SOURID) .AND. (FLGSOU(IFLAG).NE.0) .AND.
     *      (SOURID.NE.0)) GO TO 50
C                                       Check antenna
         FLGA = FLGANT(IFLAG)
         IF ((FLGA.NE.0) .AND. (FLGA.NE.ANTNO)) GO TO 50
C                                       Check subarray
         IF ((FLGSUB(IFLAG).GT.0) .AND. (FLGSUB(IFLAG).NE.SUBA))
     *      GO TO 50
C                                       Check freqid.: may be changed
C                                       from input to 1 already
         IF ((FLGFQD(IFLAG).GT.0) .AND. (FLGFQD(IFLAG).NE.FREQID) .AND.
     *      (FREQID.GT.0)) GO TO 50
C                                       Some data to be flagged
C                                       Set limits
         LIMF1 = FLGBIF(IFLAG)
         LIMF2 = FLGEIF(IFLAG)
C                                       Loop over polarizations
         IPOLPT = ABS(KCOR0) - 1
         IF (KCOR0.LT.-4) IPOLPT = IPOLPT - 4
         DO 40 JPOLN = 1,NPOL
            IF (FLGPOL(JPOLN+IPOLPT,IFLAG)) THEN
C                                       Loop over IF
               DO 30 JIF = LIMF1,LIMF2
                  CREAL(JPOLN,JIF) = FBLANK
                  CIMAG(JPOLN,JIF) = FBLANK
                  DELAY(JPOLN,JIF) = FBLANK
                  RATE(JPOLN,JIF) = FBLANK
                  WEIGHT(JPOLN,JIF) = 0.0
                  REFA(JPOLN,JIF) = 0
 30               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Check if data all gone
      DO 70 JPOLN = 1,NPOL
         DO 60 JIF = 1,NIF
            IF ((CREAL(JPOLN,JIF).NE.FBLANK) .AND.
     *         (CIMAG(JPOLN,JIF).NE.FBLANK)) GO TO 999
 60         CONTINUE
 70      CONTINUE
      IRET = -1
C
 999  RETURN
      END
      SUBROUTINE SYFSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, TB, TE, NSOU, SOUIND,
     *   AN, NA, ISUB, JSUB, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies a subset of IFs in a SY table, can also modify the FQ ID
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      VER      I        Version to check/modify
C      CATIN    I(256)   Input catalog header
C      CATOUT   I(256)   Output catalog header
C      LUNI     I        LUN to use
C      LUNO     I        LUN to use
C      BPOL     I        First polarization to copy
C      EPOL     I        Last polarization to copy
C      BIF      I        Start IF number
C      EIF      I        End IF number
C      IFQID    I        FQ ID to select (set to 1 on output)
C                           if <= 0 then output value unchanged.
C      TB       D        Beginning time in days
C      TE       D        Ending time in days
C      NSOU     I        Number of selected sources
C      SOUIND   I(*)     Array of sources indexes selected
C      AN       I(*)     Array of selected antennas
C      NA       I        Number of selected antennas
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, AN(*), NA, SOUIND(*),
     *   NSOU, ISUB, JSUB, BUFFER(*), OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXSYC
      PARAMETER (MAXSYC=13)
C
      INTEGER   ISYRNO, SYKOLS(MAXSYC), SYNUMV(MAXSYC), NUMANT, NUMPOL,
     *   NUMIF, OKOLS(MAXSYC), ONUMV(MAXSYC), NSYROW, I, SOURID, ANTNO,
     *   SUBA, FREQID, OSYRNO, NEWNIF, IIF, JIF, IPOL, K, OVER, LBIF,
     *   NEWPOL, LBPOL, NDEL, NTOT, JRET, NPART, CALTYP
      REAL      PDIFF(2,MAXIF), PSUM(2,MAXIF), PGAIN(2,MAXIF), TIMEI
      DOUBLE PRECISION TIME
      LOGICAL   GOTIT, REFMT, GOTONE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
C                                       open flag table
      TMFLST = -1.E20
      NUMFLG = 0
      IFGRNO = 1
      NDEL = 0
      NTOT = 0
      NPART = 0
C                                       Open SY file
      CALL SYINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, ISYRNO,
     *   SYKOLS, SYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       New no. of IF's
      LBIF = MAX (1, BIF)
      NEWNIF = MIN (NUMIF, EIF) - LBIF + 1
      IF (NEWNIF.LE.0) THEN
         LBIF = 1
         NEWNIF = NUMIF
         END IF
      LBPOL = MAX (1, BPOL)
      NEWPOL = MIN (2, MIN (NUMPOL, EPOL)) - LBPOL + 1
      IF (NEWPOL.LE.0) THEN
         NEWPOL = NUMPOL
         LBPOL = 1
         END IF
      REFMT = (NEWNIF.NE.NUMIF) .OR. (NEWPOL.NE.NUMPOL)
C                                       # rows in old table
      NSYROW = BUFFER(5)
C                                       Open up new SY table
      OVER = VER
      CALL SYINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OSYRNO, OKOLS, ONUMV, NUMANT, NEWPOL, NEWNIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NSYROW
         CALL TABSY ('READ', BUFFER, ISYRNO, SYKOLS, SYNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, CALTYP, SOURID, ANTNO, SUBA, FREQID,
     *      PDIFF, PSUM, PGAIN, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
C                                       flag info
         CALL SYFLG (NUMPOL, NUMIF, TIME, SOURID, ANTNO, SUBA, FREQID,
     *      PDIFF, PSUM, PGAIN, NPART, JRET)
         IF (JRET.GT.0) THEN
            IRET = JRET
            GO TO 999
            END IF
C                                       Time selection
         IF ((TIME.LT.TB) .OR. (TIME.GT.TE)) IRET = -1
C                                       Sources selection
         IF ((NSOU.GT.0) .AND. (SOURID.GT.0)) THEN
            GOTIT = .FALSE.
            DO 20 K = 1,NSOU
               GOTIT = GOTIT .OR. (SOURID.EQ.SOUIND(K))
 20            CONTINUE
            IF (.NOT.GOTIT) IRET = -1
            END IF
C                                       Antennas selection
         IF ((NA.GT.0) .AND. (ANTNO.GT.0)) THEN
            GOTIT = .FALSE.
            DO 30 K = 1,NA
               GOTIT = GOTIT .OR. (ANTNO.EQ.AN(K))
 30            CONTINUE
            IF (.NOT.GOTIT) IRET = -1
            END IF
C                                       FQ selection
         IF ((IFQID.GT.0) .AND. (FREQID.GT.0) .AND. (IFQID.NE.FREQID))
     *      IRET = -1
C                                       Suba selection
         IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *      IRET = -1
         IF ((JRET.EQ.-1) .AND. (IRET.EQ.0)) THEN
            IRET = -1
            NDEL = NDEL + 1
            END IF
C                                       Is this record selected ?
         IF (IRET.LT.0) THEN
            REFMT = .TRUE.
C                                       Re-number IF's
         ELSE
            GOTONE = .FALSE.
            DO 90 JIF = 1,NEWNIF
               IIF = JIF + BIF - 1
               DO 80 IPOL = 1,NEWPOL
                  K = IPOL + LBPOL - 1
                  PDIFF(IPOL,JIF) = PDIFF(K,IIF)
                  PSUM(IPOL,JIF) = PSUM(K,IIF)
                  PGAIN(IPOL,JIF) = PGAIN(K,IIF)
                  IF (PDIFF(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
                  IF (PSUM(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
 80               CONTINUE
 90            CONTINUE
            IF (GOTONE) THEN
               IF (IFQID.GT.0) FREQID = 1
               IF (JSUB.GE.0) SUBA = JSUB
               NTOT = NTOT + 1
               CALL TABSY ('WRIT', OBUFF, OSYRNO, OKOLS, ONUMV, NEWPOL,
     *            NEWNIF, TIME, TIMEI, CALTYP, SOURID, ANTNO, SUBA,
     *            FREQID, PDIFF, PSUM, PGAIN, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1040) IRET
                  GO TO 990
                  END IF
            ELSE
               REFMT = .TRUE.
               END IF
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ISYRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OSYRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted SY', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied SY', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         NTOT = NTOT + NDEL
         WRITE (MSGTXT,1101) NDEL, NTOT
         CALL REFRMT (MSGTXT, '_', I)
         CALL MSGWRT (3)
         WRITE (MSGTXT,1102) NPART
         CALL REFRMT (MSGTXT, '_', I)
         IF (NPART.GT.0) CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SYFSEL: ERROR ',I3,' INITING OLD TABLE')
 1020 FORMAT ('SYFSEL: ERROR ',I3,' INITING NEW TABLE')
 1030 FORMAT ('SYFSEL: ERROR ',I3,' READING OLD TABLE')
 1040 FORMAT ('SYFSEL: ERROR ',I3,' WRITING NEW TABLE')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
 1101 FORMAT ('__Fully deleted',I10,' of',I12,' SY records applying',
     *   ' flag table')
 1102 FORMAT ('__Partly flagged',I10,' SY records applying flag table')
      END
      SUBROUTINE SYFLG (NPOL, NIF, TIME, SOURID, ANTNO, SUBA,
     *   FREQID, PDIFF, PSUM, PGAIN, NPART, IRET)
C-----------------------------------------------------------------------
C   Flags a TY table row based on the flags in FG table loaded to Common
C   Inputs:
C      NPOL     I      Number polarizations in TY data
C      NIF      I      Number of IFs in those data
C      TIME     R      Time of table row
C      SOURID   I      Source number of row
C      ANTNO    I      Antenna number of row
C      SUBA     I      Subarray of row
C      FREQID   I      Frequency ID if row
C   In/Out:
C      PDIFF    R(*)   Pon-Poff
C      PSUM     R(*)   Pon+Poff
C      PGAIN    R(*)   Post detection gains
C      NPART    I      count of partly flagged records
C   Inputs from include DSEL.INC:
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
C      IRET     I      0 -> okay, -1 -> all flagged
C-----------------------------------------------------------------------
      INTEGER   NPOL, NIF, SOURID, ANTNO, SUBA, FREQID, NPART, IRET
      REAL      PDIFF(2,*), PSUM(2,*), PGAIN(2,*)
      DOUBLE PRECISION TIME
C
      INTEGER   IFLAG, FLGA, JPOLN, JIF, LIMF1, LIMF2, IPOLPT
      REAL      RTIME
      LOGICAL   PART
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      RTIME = TIME
      IF (TMFLST.LT.TIME) CALL NXTFLG (RTIME, .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NUMFLG.LE.0) GO TO 999
      PART = .FALSE.
C                                       loop over current flags
      DO 50 IFLAG = 1,NUMFLG
C                                       Check time if needed
         IF ((TIME.LT.FLGTST(IFLAG)) .OR. (TIME.GT.FLGTND(IFLAG)))
     *      GO TO 50
C                                       Check source
         IF ((FLGSOU(IFLAG).NE.SOURID) .AND. (FLGSOU(IFLAG).NE.0) .AND.
     *      (SOURID.NE.0)) GO TO 50
C                                       Check antenna
         FLGA = FLGANT(IFLAG)
         IF ((FLGA.NE.0) .AND. (FLGA.NE.ANTNO)) GO TO 50
C                                       Check subarray
         IF ((FLGSUB(IFLAG).GT.0) .AND. (FLGSUB(IFLAG).NE.SUBA))
     *      GO TO 50
C                                       Check freqid.: may be changed
C                                       from input to 1 already
         IF ((FLGFQD(IFLAG).GT.0) .AND. (FLGFQD(IFLAG).NE.FREQID) .AND.
     *      (FREQID.GT.0)) GO TO 50
C                                       Some data to be flagged
C                                       Set limits
         LIMF1 = FLGBIF(IFLAG)
         LIMF2 = FLGEIF(IFLAG)
C                                       Loop over polarizations
         IPOLPT = ABS(KCOR0) - 1
         IF (KCOR0.LT.-4) IPOLPT = IPOLPT - 4
         DO 40 JPOLN = 1,NPOL
            IF (FLGPOL(JPOLN+IPOLPT,IFLAG)) THEN
               PART = .TRUE.
C                                       Loop over IF
               DO 30 JIF = LIMF1,LIMF2
                  PDIFF(JPOLN,JIF) = FBLANK
                  PSUM(JPOLN,JIF) = FBLANK
                  PGAIN(JPOLN,JIF) = FBLANK
 30               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Check if data all flagged
      IF (PART) NPART = NPART + 1
      DO 70 JPOLN = 1,NPOL
         DO 60 JIF = 1,NIF
            IF ((PDIFF(JPOLN,JIF).NE.FBLANK) .OR.
     *         (PSUM(JPOLN,JIF).NE.FBLANK)) GO TO 999
 60         CONTINUE
 70      CONTINUE
      IF (PART) NPART = NPART - 1
      IRET = -1
C
 999  RETURN
      END
