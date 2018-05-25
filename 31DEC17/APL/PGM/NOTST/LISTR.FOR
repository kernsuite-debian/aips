LOCAL INCLUDE 'LISTR.INC'
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER NAMEIN*12, CLAIN*6, OPTYPE*4, XEXT*2, XSOUR(30)*16,
     *   XCALCO*4, XSTOK*4, INEXT*2, LPNAME*48, LINE*132, TITL1*132,
     *   TITL2*132, SCRTCH*132
      HOLLERITH XNAMEI(3), XCLAIN(2), XOPTYP, XXEXT, XSOURC(4,30),
     *   XXCALC, XSTOKE, XLPNAM(12)
      DOUBLE PRECISION  FREQIF(MAXIF), BFREQ
      REAL      XSIN, XDISIN, XVER, XTIME(8), XBAND, XFREQ, XFQID, XBIF,
     *   XEIF, XBCHAN, XECHAN, XANT(50), XBASE(50), XUVRA(2), XSUBA,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH(3), XINC, DPARM(10), XDOAC, XFACTR, DOCRT, XBADD(10),
     *   RPARM(20), VIS(3,MAXCIF), TCAL(4,MAXIF,MAXANT)
      LOGICAL   ISINGL, VERBOS
      INTEGER   DISKIN, SEQIN, CNOIN, INVER, BUFFER(512), LUNP, FINDP,
     *   NACROS, IPCNT, PAGE, IXINC, NCOUNT
C
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XOPTYP, XXEXT, XVER,
     *   XSOURC, XXCALC, XTIME, XSTOKE, XBAND, XFREQ, XFQID, XBIF, XEIF,
     *   XBCHAN, XECHAN, XANT, XBASE, XUVRA, XSUBA, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XINC,
     *   DPARM, XDOAC, XFACTR, DOCRT, XLPNAM, XBADD
      COMMON /CHPARM/ NAMEIN, CLAIN, OPTYPE, XEXT, XSOUR, XCALCO,
     *   XSTOK, INEXT, LPNAME, LINE, TITL1, TITL2, SCRTCH
      COMMON /INFOLS/ FREQIF, BFREQ, RPARM, VIS, ISINGL, VERBOS, DISKIN,
     *   SEQIN, CNOIN, INVER, BUFFER, NACROS, IPCNT, PAGE, LUNP, FINDP,
     *   TCAL, IXINC, NCOUNT
LOCAL END
LOCAL INCLUDE 'LISTR.BUF'
      REAL   XXBUFF(10000)
      COMMON /BUFFRS/ XXBUFF
LOCAL END
      PROGRAM LISTR
C-----------------------------------------------------------------------
C! LISTR prints uv data in a variety of forms.
C# Calibration UV VLA VLB
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
C   Task LISTR prints data from uv data files in a variety of forms.
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      INNAME         NAME          File name to be listed.
C      INCLASS        CLASS         File class to be listed.
C      INSEQ          SEQ           File sequence number.
C      INDISK         DISK          Disk volumn on which file resides.
C      INEXT          INEXT         SN or CL table to list
C      INVER          INVER         SN or CL table version
C      SOURCES        XSOUR(4,30)   Sources selected
C      CALCODE        XCALCO        Calibrator source code
C      TIMERANG       XTIME(8)      Timerange
C      STOKES         XSTOK         Stokes' parameter
C      BIF            BIF           IF number
C      BCHAN          BCHAN         Channel number
C      ANTENNAS       XANT(50)      Antenna numbers
C      UVRANGE        UVRANG        Range of UV in 1000's wavelengths
C      SUBARRAY       SUBARR        Subarray
C      DOCALIB        DOCAL         Calibrate?
C      GAINUSE        GAUSE         CL version to apply.
C      FLAGVER        FGVER         Flag table version
C      DOBAND                       Bandpass calibrate?
C      BPVER                        BP table to apply
C      SMOOTH                       Smoothing function
C      OPTYPE         OPTYPE        Type of listing
C      DPARM          DPARM         Control info.
C      DOCRT          DOCRT         > 0 => use CRT, else line printer
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, STR*4
      INTEGER   IRET, IERR, IB, IE, DTYPE, TTY(2), I, CATSAV(256)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'LISTR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA PRGM /'LISTR '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL LSTRIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL COPY (256, CATBLK, CATSAV)
C                                       multi-band delay no IF depend
      IF (OPTYPE.EQ.'GAIN') THEN
         DTYPE = DPARM(1) + 0.5
         IF (DTYPE.EQ.12) THEN
            BIF = 1
            EIF = 1
            END IF
      ELSE IF ((OPTYPE.EQ.'EFST') .OR. (OPTYPE.EQ.'SEFD')) THEN
         DTYPE = 0
         END IF
C                                       Count lines for line printer
C                                       Summaries
      IF (OPTYPE.EQ.'SCAN') THEN
         CALL SCANCH (IRET)
C                                       Count: 1 IF at a time
      ELSE
         IB = BIF
         IE = EIF
         DO 10 BIF = IB,IE
            STOKES = XSTOK
            EIF = BIF
            INXRNO = 0
            IDSOUR = 0
            IF (OPTYPE.EQ.'MATX') THEN
               CALL MATXCH (IRET)
            ELSE IF (OPTYPE.EQ.'LIST') THEN
               CALL LISTCH (IRET)
            ELSE IF ((OPTYPE.EQ.'EFST') .OR. (OPTYPE.EQ.'SEFD') .OR.
     *         (OPTYPE.EQ.'GAIN')) THEN
               IF (VERBOS) THEN
                  CALL GAINCH (IRET)
               ELSE
                  CALL GAICH1 (IRET)
                  END IF
               END IF
            CALL COPY (256, CATUV, CATBLK)
            IF (IRET.NE.0) GO TO 900
 10         CONTINUE
         END IF
C                                       check count
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
         IF (NCOUNT.GT.1000) THEN
            IRET = -1
            IPCNT = -1
            CALL LPCLOS (LUNP, FINDP, IPCNT, IERR)
            END IF
      ELSE IF (NCOUNT.GT.500) THEN
         IPCNT = 0
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRTCH, .FALSE., .FALSE.,
     *      .TRUE., IRET)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IRET.GT.0) GO TO 980
         WRITE (SCRTCH,1900) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, IRET)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IRET.GT.0) GO TO 990
         SCRTCH = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRTCH, 1, STR, IRET)
         IF (IRET.GT.0) GO TO 990
         IF ((STR(:1).NE.'y') .AND. (STR(:1).NE.'Y')) THEN
            IPCNT = -1
            CALL LPCLOS (LUNP, FINDP, IPCNT, IERR)
            IRET = -1
            SCRTCH = 'Good choice - save trees'
         ELSE
            SCRTCH = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      IF (IRET.NE.0) GO TO 990
C                                       now actually print
      CALL COPY (256, CATSAV, CATBLK)
      IF (OPTYPE.EQ.'SCAN') THEN
         CALL SCANUV (IRET)
C                                       List data: 1 IF at a time
      ELSE
         IPCNT = 998
         DO 20 BIF = IB,IE
            STOKES = XSTOK
            EIF = BIF
            IF (IPCNT.GT.(PRTMAX+1)/2) IPCNT = 998
            TITL1 = ' '
            TITL2 = ' '
            LINE = ' '
            INXRNO = 0
            IDSOUR = 0
            IF (OPTYPE.EQ.'MATX') THEN
               CALL MATXUV (IRET)
            ELSE IF (OPTYPE.EQ.'LIST') THEN
               CALL LISTUV (IRET)
            ELSE IF ((OPTYPE.EQ.'EFST') .OR. (OPTYPE.EQ.'SEFD') .OR.
     *         (OPTYPE.EQ.'GAIN')) THEN
               IF (VERBOS) THEN
                  CALL GAINUV (IRET)
               ELSE
                  CALL GAIUV1 (IRET)
                  END IF
               END IF
            CALL COPY (256, CATUV, CATBLK)
            IF (IRET.NE.0) GO TO 900
 20         CONTINUE
         END IF
C                                       Close printer
 900  IF (IPCNT.GE.0) CALL LPCLOS (LUNP, FINDP, IPCNT, IERR)
      GO TO 990
C
 980  CALL MSGWRT (8)
C                                       Close down files, etc.
 990  IRET = MAX (0, IRET)
      CALL DIE (IRET, BUFFER)
C
 999  STOP
C-----------------------------------------------------------------------
 1900 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE LSTRIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   LSTRIN gets input parameters for LISTR
C   Inputs:
C      PRGN    C*6       Program name (2 chars/word)
C   Output:
C      JERR    I         Error code: 0 => ok
C                           5 => catalog troubles
C                           8 => can't start
C   Commons:
C      /INPARM/ all input adverbs in order given by INPUTS file
C      /MAPHDR/ output file catalog header
C   See prologue comments in LISTR for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   JERR
C
      CHARACTER STAT*4, STATUS*4, OPCO(6)*4, UTYPE*2
      INTEGER   IROUND, NPARM, IERR, I, LUN, NIF, NOPT, IIVER, MXIF
      REAL      CATR(256)
      LOGICAL   F, EQUAL, MARKRD, TABLE, EXIST, FITASC, MATCH
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ISBAND(MAXIF)
      DOUBLE PRECISION    FOFF(MAXIF)
      REAL      FINC(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'LISTR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATR,CATBLK)
      DATA F /.FALSE./
      DATA NOPT, OPCO /6,'MATX','LIST','GAIN','SCAN','EFST','SEFD'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
      NCOUNT = 0
C                                       Get input parameters.
      NPARM = 297
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF ((IERR.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      IF (DOCRT.GT.0.0) RQUICK = .FALSE.
      IF (RQUICK) RQUICK = LPNAME.NE.' '
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      CALL H2CHR (2, 1, XXEXT, XEXT)
      CALL H2CHR (4, 1, XSTOKE, XSTOK)
C                                       Calcode abreviations
      IF (XCALCO(1:2).EQ.'CA') XCALCO = '*   '
      IF (XCALCO(1:1).EQ.'-')  XCALCO = '-CAL'
      SELCOD = XCALCO
      DO 25 I = 1,30
         CALL H2CHR (16, 1, XSOURC(1,I), XSOUR(I))
 25      CONTINUE
      DOACOR = XDOAC.GT.0.0
      IXINC = IROUND (XINC)
      IXINC = MAX (1, IXINC)
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      INVER = IROUND (XVER)
      INEXT = XEXT
      IF ((INEXT.NE.'CL') .AND. (INEXT.NE.'SN') .AND.
     *   (INEXT.NE.'TY') .AND. (INEXT.NE.'SY')) INEXT = 'SN'
C                                       Get CATBLK.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      STATUS = 'REST'
C                                       Mark "READ" if GAIN or SCAN
      MARKRD = (OPTYPE.EQ.OPCO(3)) .OR. (OPTYPE.EQ.OPCO(4)) .OR.
     *   (OPTYPE.EQ.OPCO(5)) .OR. (OPTYPE.EQ.OPCO(6))
      IF (MARKRD) STATUS = 'READ'
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, STATUS, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Mark in CFILES if catalog
C                                       marked "READ"
      IF (MARKRD) THEN
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKIN
         FCNO(NCFILE) = CNOIN
         FRW(NCFILE) = 0
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      BFREQ = FREQ
      I = IROUND(DPARM(1))
      IF (OPTYPE.EQ.'GAIN') THEN
         IF (I.EQ.10) INEXT = 'TY'
         IF (I.EQ.13) INEXT = 'TY'
         IF ((INEXT.EQ.'TY') .AND. (I.NE.13)) I = 10
         IF ((I.GE.15) .AND. (I.LE.19)) INEXT = 'SY'
         IF (INEXT.EQ.'SY') THEN
            IF ((I.LT.15) .OR. (I.GT.19)) I = 15
            END IF
         END IF
      IF ((OPTYPE.EQ.'SEFD') .OR. (OPTYPE.EQ.'EFST')) THEN
         I = 0
         IF (INEXT.NE.'CL') INEXT = 'SN'
         END IF
      DPARM(1) = I
C                                       Check sort order
      IF (ISORT(1:1).NE.'T') THEN
         MSGTXT = 'YOUR DATA ARE NOT IN T* ORDER, USE UVSRT'
         JERR = 1
         GO TO 990
         END IF
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DO 70 I = 1,30
         SOURCS(I) = XSOUR(I)
 70      CONTINUE
      CALL RCOPY (8, XTIME, TIMRNG)
      UVRNG(1) = XUVRA(1)
      UVRNG(2) = XUVRA(2)
      STOKES = XSTOK
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         BIF = 1
         EIF = 1
         END IF
C                                       Antennas
      DO 60 I = 1,50
         ANTENS(I) = IROUND (XANT(I))
 60      CONTINUE
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      DOAPPL = F
      SUBARR = IROUND (XSUBA)
      IF ((SUBARR.LE.0) .AND. (OPTYPE.NE.'GAIN') .AND.
     *   (OPTYPE.NE.'EFST') .AND. (OPTYPE.NE.'SEFD')) SUBARR = 1
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DO 80 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 80      CONTINUE
C                                       OPTYPE
      EQUAL = F
      DO 90 I = 1,NOPT
         EQUAL = EQUAL .OR. (OPTYPE.EQ.OPCO(I))
 90      CONTINUE
C                                       Default ='MATX'
      IF (.NOT.EQUAL) OPTYPE = OPCO(1)
      JERR = 0
C                                       Type of gain listing
      VERBOS = DPARM(5).GT.0.0
C                                       get full antenna info
      CALL GETANT (DISKIN, CNOIN, MAX(1,SUBARR), CATBLK, BUFFER, JERR)
      IF ((JERR.NE.0) .AND. (DPARM(9).GT.0.0)) THEN
          MSGTXT = 'GETANT ERROR, DPARM(9) SET TO 0'
          CALL MSGWRT (7)
          DPARM(9) = 0.0
          END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      LUN = 28
      IF ((OPTYPE.EQ.'SCAN') .AND. (FRQSEL.LE.0)) FRQSEL = 1
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       Get IF freq offset.
      LUN = 28
      IIVER = 1
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, IIVER, CATBLK, LUN,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
      IF (IERR.GT.0) GO TO 999
      MXIF = MAXIF
      DO 100 I = 1, MXIF
         FREQIF(I) = FOFF(I)
 100     CONTINUE
      IF (((OPTYPE.EQ.'GAIN') .OR. (OPTYPE.EQ.'SEFD') .OR.
     *   (OPTYPE.EQ.'EFST')) .AND. (XBAND.LE.0.0) .AND. (XFREQ.LE.0.0)
     *   .AND. (XFQID.LE.0.0)) FRQSEL = -1
C                                       See if a single source file.
      CALL MULSDB (CATBLK, ISINGL)
      IF (ISINGL) THEN
         CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUN, BUFFER, TABLE, EXIST,
     *      FITASC, IERR)
         ISINGL = EXIST .AND. (IERR.EQ.0) .AND. TABLE
         END IF
      ISINGL = .NOT.ISINGL
C                                       Init printer
      PAGE = 0
      IPCNT = 980
      TITL1 = ' '
      TITL2 = ' '
      LINE = ' '
C                                       Open output device
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, BUFFER, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1080) JERR
         JERR = 1
         GO TO 990
         END IF
C                                       CDs
      IF ((OPTYPE.EQ.'GAIN') .AND. (INEXT.EQ.'SY')) THEN
         I = 1
         CALL GETCDS (DISKIN, CNOIN, I, SUBARR, FRQSEL, CATBLK, TCAL,
     *      JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1100) JERR
            GO TO 990
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LSTRIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1080 FORMAT ('LSTRIN: ERROR ',I3,' OPENING OUTPUT ''PRINT'' DEVICE')
 1100 FORMAT ('LSTRIN: ERROR ',I3,' READING TCALS FOR SY GAIN LIST')
      END
      SUBROUTINE SCANCH (IRET)
C-----------------------------------------------------------------------
C   Checks line count of the printer list of index and source info.
C   Output:
C      IRET   I    Return code, 0=OK, else failed, -1 => too much
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   MAXSOU
      PARAMETER (MAXSOU=10000)
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  CCODE*1, VELTYP*8, VELDEF*8, SUNAME*16, CALCOD*4
      INTEGER   QUAL, NXSOUR, NXSUB, INXLUN, ISLUN, I, J,
     *   SUKOLS(MAXSUC), SUNUMV(MAXSUC), IDSOU, IERR, NNIF, NSOURC,
     *   NDXRNO, SURNO, LOOP, NXVS, NXVE, MXSOU, IIVER, FQID,
     *   FQSIDE(MAXIF), NMFQIF, NUMFQE, NXFQI, NXVER, NLOOP, SUFQID,
     *   MAXSUB
      LOGICAL   FQEXIS, TABLE, FITASC, SAMEFQ, ISNX, ISSU
      REAL      NXTIME, NXDTIM, FQTBW(MAXIF), FQCHBW(MAXIF)
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, SFREQ,
     *   SLSRV, SRF, DECAPP, PMRA, PMDEC, FQFRQ(MAXIF), RAOBS, DECOBS
      INTEGER   XQUAL(MAXSOU+1), XSCNT(MAXSOU+1), KSID(MAXIF), KQUAL
      CHARACTER SXNAME(MAXSOU+1)*16, XCODE(MAXSOU+1)*4, BNDCOD(MAXIF)*8
      REAL      FLUX(4,MAXIF), FINC(MAXIF), CATR(256)
      DOUBLE PRECISION    LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF),
     *   FOFF(MAXIF), CATD(128), FREQAS(MAXIF), VELOCS(MAXIF),
     *   FQREST(MAXIF)
      HOLLERITH CATH(256)
      INCLUDE 'LISTR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATD, CATH, CATR, CATBLK)
      DATA MXSOU /MAXSOU/
      DATA ISLUN, INXLUN /27,28/
C-----------------------------------------------------------------------
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
C                                       Do NX, SU tables exist?
      CALL ISTAB ('NX', DISKIN, CNOIN, 1, INXLUN, BLBUFF, TABLE, ISNX,
     *   FITASC, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1001) IRET, 'NX'
         GO TO 990
         END IF
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, INXLUN, BLBUFF, TABLE, ISSU,
     *   FITASC, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1001) IRET, 'SU'
         GO TO 990
         END IF
C
      FREQ = BFREQ * 1.0D-9
      MAXSUB = 1
C                                       Initialize source info
      I = MXSOU + 1
      DO 10 LOOP = 1,I
         XSCNT(LOOP) = 0
         XQUAL(LOOP) = 0
         XCODE(LOOP) = 'HELP'
         SXNAME(LOOP) = 'Not in SU table'
 10      CONTINUE
      SXNAME(I) = 'Too many sources'
      XSCNT(I) = -1
      EPOCH = CATR(KREPO)
C                                       Get source info
      IF (ISSU) THEN
         CALL SOUINI ('READ', BLBUFF, DISKIN, CNOIN, 1, CATBLK, ISLUN,
     *      NUMIF, VELTYP, VELDEF, SUFQID, SURNO, SUKOLS, SUNUMV, IRET)
         IF (IRET.NE.0) GO TO 999
         SUFQID = MAX (1, SUFQID)
         NSOURC = BLBUFF(5)
         DO 30 LOOP = 1,NSOURC
            SURNO = LOOP
            CALL TABSOU ('READ', BLBUFF, SURNO, SUKOLS, SUNUMV, IDSOU,
     *         SUNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) GO TO 999
            IF ((IDSOU.GT.0) .AND. (IDSOU.LE.MXSOU)) THEN
               SXNAME(IDSOU) = SUNAME
               XCODE(IDSOU) = CALCOD
               XQUAL(IDSOU) = QUAL
               END IF
 30         CONTINUE
      ELSE
         SAMEFQ = .TRUE.
         NSOURC = 1
         XCODE(1) = ' '
         XQUAL(1) = 0
         CALL H2CHR (8, 1, CATH(KHOBJ), SXNAME(1))
         SUFQID = 1
         IF (CATBLK(KIALT).NE.0) THEN
            I = CATBLK(KIALT)/256 + 1
            VELTYP = 'RADIO'
            IF (I.EQ.1) VELTYP = 'OPTICAL'
            I = CATBLK(KIALT) - (I-1) * 256
            VELDEF = 'LSR'
            IF (I.EQ.2) VELDEF = 'BARYCENTR'
            IF (I.EQ.3) VELDEF = 'TOPOCENTR'
         ELSE
            VELTYP = ' '
            VELDEF = ' '
            END IF
         END IF
C                                       Initialize index table
      IF (ISNX) THEN
         NXVER = 1
         CALL NDXINI ('READ', NXBUFF, DISKIN, CNOIN, NXVER, CATBLK,
     *      INXLUN, NDXRNO, NXKOLS, NXNUMV, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Initialize FQ table
      IQLUN = 44
      NXVER = 1
      CALL ISTAB ('FQ', DISKIN, CNOIN, NXVER, IQLUN, FQBUFF, TABLE,
     *   FQEXIS, FITASC, IRET)
      IF (FQEXIS) THEN
         CALL FQINI ('READ', FQBUFF, DISKIN, CNOIN, NXVER, CATBLK,
     *      IQLUN, IFQRNO, FQKOLS, FQNUMV, NMFQIF, IRET)
         IF (IRET.NE.0) GO TO 999
         NUMFQE = FQBUFF(5)
         END IF
C                                       Names for labels
C                                       first page & page titles
      NCOUNT = NCOUNT + 2
      IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 2
      NCOUNT = NCOUNT + 1
C                                       Get number of index records
      IF (ISNX) THEN
         NINDEX = NXBUFF(5)
C                                       Loop thru index table
         DO 50 LOOP = 1,NINDEX
C                                       Read index table
            NDXRNO = LOOP
            CALL TABNDX ('READ', NXBUFF, NDXRNO, NXKOLS, NXNUMV, NXTIME,
     *         NXDTIM, NXSOUR, NXSUB, NXVS, NXVE, NXFQI, IRET)
            IF (IRET.GT.0) GO TO 999
            MAXSUB = MAX (MAXSUB, NXSUB)
C                                       Print line
            IF ((NXSOUR.LT.1) .OR. (NXSOUR.GT.MXSOU)) NXSOUR = MXSOU + 1
C                                       If calc code OK
            CCODE = XCODE(NXSOUR)(1:1)
            J = NXVE - NXVS + 1
            IF ((XCALCO(:1).EQ.' ') .OR. (CCODE.EQ.XCALCO(:1)) .OR.
     *         ((XCALCO(:1).EQ.'*') .AND. (CCODE.NE.' ')) .OR.
     *         ((XCALCO(:1).EQ.'-') .AND. (CCODE.EQ.' '))) NCOUNT =
     *         NCOUNT + 1
C                                       Sum visibilities
            XSCNT(NXSOUR) = XSCNT(NXSOUR) + J
 50         CONTINUE
C                                       Close  INDEX table
         CALL TABIO ('CLOS', 0, NDXRNO, NXBUFF, NXBUFF, IERR)
         END IF
C                                       Source summary
      LINE = ' '
      TITL2 = ' '
C                                       if page half full, new page
      IF ((ISSU) .AND. (IPCNT.GT.PRTMAX/2)) IPCNT = 980
      IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 2
C                                       Velocity type, defination
      NCOUNT = NCOUNT + 1
      IF (DOCRT.GT.-2.5) THEN
         IF (MAXSUB.GT.1) NCOUNT = NCOUNT + 1
         NCOUNT = NCOUNT + 1
         END IF
      NCOUNT = NCOUNT + 1
C                                       Loop over source table
      IF (ISSU) THEN
C                                       Position, flux
         DO 130 LOOP = 1,NSOURC
            SURNO = LOOP
            CALL TABSOU ('READ', BLBUFF, SURNO, SUKOLS, SUNUMV, IDSOU,
     *         SUNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) GO TO 999
            KQUAL = QUAL
C                                       for all IFs
            DO 110 I = 1,NUMIF
C                                       Calc freq, velocity and rest fq
               SFREQ = FREQ + FREQO(I) * 1.0D-9
               SLSRV = LSRVEL(I) * 1.0D-3
               SRF = RESTFQ(I) * 1.0D-9
C                                       store on first source
               IF (LOOP.EQ.1) THEN
                  FREQAS(I) = SFREQ
                  VELOCS(I) = SLSRV
                  FQREST(I) = SRF
                  SAMEFQ    = .TRUE.
C                                       check on rest of sources
               ELSE
                  IF ((FREQAS(I).NE.SFREQ) .OR. (VELOCS(I).NE.SLSRV)
     *               .OR.(FQREST(I).NE.SRF)) SAMEFQ = .FALSE.
                  END IF
C                                       end for all IFs loop
 110           CONTINUE
C                                       Convert RA, dec
            NXSOUR = IDSOU
            IF ((NXSOUR.LT.1) .OR. (NXSOUR.GT.MXSOU)) NXSOUR = MXSOU + 1
C                                       If calc code OK
            CCODE = XCODE(NXSOUR)(1:1)
            IF ((XCALCO(:1).EQ.' ') .OR. (CCODE.EQ.XCALCO(:1)) .OR.
     *         ((XCALCO(:1).EQ.'*') .AND. (CCODE.NE.' ')) .OR.
     *         ((XCALCO(1:1).EQ.'-') .AND. (CCODE.EQ.' '))) THEN
C                                       Write row
               NCOUNT = NCOUNT + 1
C                                       Other IF fluxes
               IF ((NACROS.GE.105) .AND. (NUMIF.GT.1)) NCOUNT = NCOUNT +
     *            NUMIF - 1
               END IF
 130        CONTINUE
      ELSE
         NCOUNT = NCOUNT + 1
         END IF
C                                       Get IF frequency info
      IF (.NOT.FQEXIS) THEN
         IIVER = 1
         CALL CHNDAT ('READ', NXBUFF, DISKIN, CNOIN, IIVER, CATBLK,
     *      INXLUN, NNIF, FOFF, KSID, FINC, BNDCOD, FRQSEL, IRET)
      ELSE
         CALL TABFQ ('READ', FQBUFF, SUFQID, FQKOLS, FQNUMV, NMFQIF,
     *      FQID, FOFF, FQCHBW, FQTBW, FQSIDE, BNDCOD, IRET)
         END IF
      IF (IRET.NE.0) GO TO 999
      IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
      NCOUNT = NCOUNT + 1
      NLOOP = NSOURC
C                                       Only once if FQ same for all
      IF (SAMEFQ) NLOOP = 1
C                                       Frequency, velocity, rest freq.
      IF (ISSU) THEN
         DO 150 LOOP = 1,NLOOP
            SURNO = LOOP
            CALL TABSOU ('READ', BLBUFF, SURNO, SUKOLS, SUNUMV, IDSOU,
     *         SUNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) GO TO 999
            KQUAL = QUAL
            SFREQ = FREQ + (FOFF(1) + FREQO(1)) * 1.0D-9
            SLSRV = LSRVEL(1) * 1.0D-3
            SRF = RESTFQ(1) * 1.0D-9
C                                       If FQ info same for all sources
            IF (SAMEFQ) SUNAME = 'All Sources     '
C                                       If calc code OK
            CCODE = XCODE(IDSOU)(1:1)
            IF ((XCALCO(:1).EQ.' ') .OR. (CCODE.EQ.XCALCO(:1)) .OR.
     *         ((XCALCO(:1).EQ.'*') .AND. (CCODE.NE.' ')) .OR. (SAMEFQ)
     *         .OR. ((XCALCO(1:1).EQ.'-') .AND. (CCODE.EQ.' '))) THEN
               NCOUNT = NCOUNT + 1
C                                       Rest of IF loop
               IF (NUMIF.GT.1) NCOUNT = NCOUNT + NUMIF - 1
               END IF
 150        CONTINUE
C                                       Close source table
         CALL TABIO ('CLOS', 0, SURNO, BLBUFF, BLBUFF, IERR)
C                                       single source
      ELSE
         I = CATBLK(KIALT) / 256 + 1
         J = CATBLK(KIALT) - (I-1) * 256
         IF ((I.GE.1) .AND. (I.LE.2) .AND. (J.GE.1) .AND. (J.LE.3))
     *      NCOUNT = NCOUNT + 1
         END IF
C                                       FQ table stuff
C                                       New page
      IF (FQEXIS) THEN
C                                       if not enough room, new page
         IF ((IPCNT+(NUMFQE*NMFQIF)+3).GT.PRTMAX) IPCNT = 980
         IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 2
C                                       List the FQ entries
         NCOUNT = NCOUNT + 1
         DO 170 LOOP = 1, NUMFQE
            CALL TABFQ ('READ', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NMFQIF,
     *         FQID, FQFRQ, FQCHBW, FQTBW, FQSIDE, BNDCOD, IRET)
            IF (IRET.NE.0) GO TO 999
            NCOUNT = NCOUNT + NMFQIF
 170        CONTINUE
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('SCANCH: ERROR ',I3,' CHECKING ',A2,' TABLE')
      END
      SUBROUTINE SCANUV (IRET)
C-----------------------------------------------------------------------
C   Gives column listing of the index table and source info.
C   Output:
C      IRET   I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  CCODE*1, VELTYP*8, VELDEF*8, SUNAME*16, CALCOD*4,
     *   CH1*1, CH2*1
      INTEGER   IRET, TIME(8), QUAL, NXSOUR, NXSUB, INXLUN, ISLUN, I, J,
     *   HMRA(2), DMDEC(2), SUKOLS(MAXSUC), SUNUMV(MAXSUC), IDSOU, IERR,
     *   NNIF, NSOURC, NDXRNO, SURNO, LOOP, NXVS, NXVE, MXSOU, IIVER,
     *   FQID, FQSIDE(MAXIF), NMFQIF, NUMFQE, NXFQI, NXVER, NLOOP,
     *   SUFQID, MAXSUB
      LOGICAL   FQEXIS, TABLE, FITASC, SAMEFQ, ISNX, ISSU
      REAL      T1, T2, NXTIME, NXDTIM, SECRA, SECDEC, FQTBW(MAXIF),
     *   FQCHBW(MAXIF)
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, SFREQ,
     *   SLSRV, SRF, DECAPP, PMRA, PMDEC, FQFRQ(MAXIF), RAOBS, DECOBS
      INTEGER   MAXSOU
      PARAMETER (MAXSOU=10000)
      INTEGER   XQUAL(MAXSOU+1), XSCNT(MAXSOU+1), KSID(MAXIF), KQUAL
      CHARACTER SXNAME(MAXSOU+1)*16, XCODE(MAXSOU+1)*4, BNDCOD(MAXIF)*8
      REAL      FLUX(4,MAXIF), FINC(MAXIF), CATR(256)
      DOUBLE PRECISION    LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF),
     *   FOFF(MAXIF), CATD(128), REFF, FREQAS(MAXIF), VELOCS(MAXIF),
     *   FQREST(MAXIF)
      HOLLERITH CATH(256)
      INCLUDE 'LISTR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATD, CATH, CATR, CATBLK)
      DATA MXSOU /MAXSOU/
      DATA ISLUN, INXLUN /27,28/
C-----------------------------------------------------------------------
C                                       Do NX, SU tables exist?
      CALL ISTAB ('NX', DISKIN, CNOIN, 1, INXLUN, BLBUFF, TABLE, ISNX,
     *   FITASC, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1001) IRET, 'NX'
         GO TO 990
         END IF
      IF (.NOT.ISNX) THEN
         MSGTXT = 'NX TABLE MISSING, SCAN OUTPUT WILL OMIT'
         CALL MSGWRT (6)
         END IF
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, INXLUN, BLBUFF, TABLE, ISSU,
     *   FITASC, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1001) IRET, 'SU'
         GO TO 990
         END IF
      IF (.NOT.ISSU) THEN
         MSGTXT = 'NO SOURCE TABLE, SCAN OUTPUT WILL OMIT'
         CALL MSGWRT (6)
         END IF
C
      FREQ = BFREQ * 1.0D-9
      MAXSUB = 1
C                                       Initialize source info
      I = MXSOU + 1
      DO 10 LOOP = 1,I
         XSCNT(LOOP) = 0
         XQUAL(LOOP) = 0
         XCODE(LOOP) = 'HELP'
         SXNAME(LOOP) = 'Not in SU table'
 10      CONTINUE
      SXNAME(I) = 'Too many sources'
      XSCNT(I) = -1
      EPOCH = CATR(KREPO)
C                                       Get source info
      IF (ISSU) THEN
         CALL SOUINI ('READ', BLBUFF, DISKIN, CNOIN, 1, CATBLK, ISLUN,
     *      NUMIF, VELTYP, VELDEF, SUFQID, SURNO, SUKOLS, SUNUMV, IRET)
         IF (IRET.NE.0) GO TO 999
         SUFQID = MAX (1, SUFQID)
         NSOURC = BLBUFF(5)
         DO 30 LOOP = 1,NSOURC
            SURNO = LOOP
            CALL TABSOU ('READ', BLBUFF, SURNO, SUKOLS, SUNUMV, IDSOU,
     *         SUNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) GO TO 999
            IF ((IDSOU.GT.0) .AND. (IDSOU.LE.MXSOU)) THEN
               SXNAME(IDSOU) = SUNAME
               XCODE(IDSOU) = CALCOD
               XQUAL(IDSOU) = QUAL
               END IF
 30         CONTINUE
      ELSE
         SAMEFQ = .TRUE.
         NSOURC = 1
         XCODE(1) = ' '
         XQUAL(1) = 0
         CALL H2CHR (8, 1, CATH(KHOBJ), SXNAME(1))
         SUFQID = 1
         IF (CATBLK(KIALT).NE.0) THEN
            I = CATBLK(KIALT)/256 + 1
            VELTYP = 'RADIO'
            IF (I.EQ.1) VELTYP = 'OPTICAL'
            I = CATBLK(KIALT) - (I-1) * 256
            VELDEF = 'LSR'
            IF (I.EQ.2) VELDEF = 'BARYCENTR'
            IF (I.EQ.3) VELDEF = 'TOPOCENTR'
         ELSE
            VELTYP = ' '
            VELDEF = ' '
            END IF
         END IF
C                                       Initialize index table
      IF (ISNX) THEN
         NXVER = 1
         CALL NDXINI ('READ', NXBUFF, DISKIN, CNOIN, NXVER, CATBLK,
     *      INXLUN, NDXRNO, NXKOLS, NXNUMV, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Initialize FQ table
      IQLUN = 44
      NXVER = 1
      CALL ISTAB ('FQ', DISKIN, CNOIN, NXVER, IQLUN, FQBUFF, TABLE,
     *   FQEXIS, FITASC, IRET)
      IF (FQEXIS) THEN
         CALL FQINI ('READ', FQBUFF, DISKIN, CNOIN, NXVER, CATBLK,
     *      IQLUN, IFQRNO, FQKOLS, FQNUMV, NMFQIF, IRET)
         IF (IRET.NE.0) GO TO 999
         NUMFQE = FQBUFF(5)
         END IF
C                                       Names for labels
C                                       first page & page titles
      WRITE (LINE,1030) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER
      CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 950
      TITL1 = LINE
      WRITE (LINE,1031) FREQ, NCOR, NVIS
      CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 950
      IF (DOCRT.GT.-2.5) THEN
         LINE = 'Scan summary listing'
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
      IF (DPARM(9).LE.0.0) THEN
         WRITE (TITL2,1035)
      ELSE
         WRITE (TITL2,1036)
         END IF
      IF (NACROS.LT.80) TITL2(73:) = ' '
      CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 950
C                                       Get number of index records
      IF (ISNX) THEN
         NINDEX = NXBUFF(5)
C                                       Loop thru index table
         DO 50 LOOP = 1,NINDEX
C                                       Read index table
            NDXRNO = LOOP
            CALL TABNDX ('READ', NXBUFF, NDXRNO, NXKOLS, NXNUMV, NXTIME,
     *         NXDTIM, NXSOUR, NXSUB, NXVS, NXVE, NXFQI, IRET)
            IF (IRET.GT.0) GO TO 999
            MAXSUB = MAX (MAXSUB, NXSUB)
C                                       Convert time
            T1 = NXTIME - 0.5 * NXDTIM
            T2 = NXTIME + 0.5 * NXDTIM
            IF (DPARM(9).GT.0.0) THEN
               CALL T2LST (T1)
               CALL T2LST (T2)
               END IF
C                                       To Days Hours Minutes Secs
            CALL TODHMS (T1, TIME(1))
            CALL TODHMS (T2, TIME(5))
C                                       Print line
            IF ((NXSOUR.LT.1) .OR. (NXSOUR.GT.MXSOU)) NXSOUR = MXSOU + 1
C                                       If calc code OK
            CCODE = XCODE(NXSOUR)(1:1)
            J = NXVE - NXVS + 1
            IF ((XCALCO(:1).EQ.' ') .OR. (CCODE.EQ.XCALCO(:1)) .OR.
     *         ((XCALCO(:1).EQ.'*') .AND. (CCODE.NE.' ')) .OR.
     *         ((XCALCO(:1).EQ.'-') .AND. (CCODE.EQ.' '))) THEN
               IF (NACROS.LE.79) THEN
                  WRITE (LINE,1040) LOOP, SXNAME(NXSOUR), XQUAL(NXSOUR),
     *               XCODE(NXSOUR), NXSUB, TIME, NXFQI
               ELSE
                  WRITE (LINE,1040) LOOP, SXNAME(NXSOUR), XQUAL(NXSOUR),
     *               XCODE(NXSOUR), NXSUB, TIME, NXFQI, NXVS, NXVE
                  END IF
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
               END IF
C                                       Sum visibilities
            XSCNT(NXSOUR) = XSCNT(NXSOUR) + J
 50         CONTINUE
C                                       Close  INDEX table
         CALL TABIO ('CLOS', 0, NDXRNO, NXBUFF, NXBUFF, IERR)
         END IF
C                                       Source summary
      LINE = ' '
      TITL2 = ' '
C                                       if page half full, new page
      IF ((ISSU) .AND. (IPCNT.GT.PRTMAX/2)) IPCNT = 980
      IF (DOCRT.GT.-2.5) THEN
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
      IF ((NACROS.GE.105) .AND. (ISSU)) THEN
         WRITE (TITL2,1050) EPOCH, EPOCH
      ELSE
         WRITE (TITL2,1051) EPOCH, EPOCH
         END IF
      IF (DOCRT.GT.-2.5) THEN
         LINE = 'Source summary'
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
C                                       Velocity type, defination
      WRITE (LINE,1060) VELTYP, VELDEF
      CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 950
      IF (DOCRT.GT.-2.5) THEN
         IF (MAXSUB.GT.1) THEN
            WRITE (LINE,1061) MAXSUB
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
      CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 950
C                                       Loop over source table
      IF (ISSU) THEN
C                                       Position, flux
         DO 130 LOOP = 1,NSOURC
            SURNO = LOOP
            CALL TABSOU ('READ', BLBUFF, SURNO, SUKOLS, SUNUMV, IDSOU,
     *         SUNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) GO TO 999
            KQUAL = QUAL
C                                       for all IFs
            DO 110 I = 1,NUMIF
C                                       Calc freq, velocity and rest fq
               SFREQ = FREQ + FREQO(I) * 1.0D-9
               SLSRV = LSRVEL(I) * 1.0D-3
               SRF = RESTFQ(I) * 1.0D-9
C                                       store on first source
               IF (LOOP.EQ.1) THEN
                  FREQAS(I) = SFREQ
                  VELOCS(I) = SLSRV
                  FQREST(I) = SRF
                  SAMEFQ    = .TRUE.
C                                       check on rest of sources
               ELSE
                  IF ((FREQAS(I).NE.SFREQ) .OR. (VELOCS(I).NE.SLSRV)
     *               .OR.(FQREST(I).NE.SRF)) SAMEFQ = .FALSE.
                  END IF
C                                       end for all IFs loop
 110           CONTINUE
C                                       Convert RA, dec
            CALL COORDD (1, RAEPO, CH1, HMRA, SECRA)
            CALL COORDD (2, DECEPO, CH2, DMDEC, SECDEC)
            NXSOUR = IDSOU
            IF ((NXSOUR.LT.1) .OR. (NXSOUR.GT.MXSOU)) NXSOUR = MXSOU + 1
C                                       If calc code OK
            CCODE = XCODE(NXSOUR)(1:1)
            IF ((XCALCO(:1).EQ.' ') .OR. (CCODE.EQ.XCALCO(:1)) .OR.
     *         ((XCALCO(:1).EQ.'*') .AND. (CCODE.NE.' ')) .OR.
     *         ((XCALCO(1:1).EQ.'-') .AND. (CCODE.EQ.' '))) THEN
C                                       Write row
               IF (NACROS.LT.105) THEN
                  WRITE (LINE,1110) IDSOU, SUNAME, KQUAL, CALCOD, CH1,
     *               HMRA, SECRA, CH2, DMDEC, SECDEC, XSCNT(NXSOUR)
               ELSE
                  WRITE (LINE,1111) IDSOU, SUNAME, KQUAL, CALCOD, CH1,
     *               HMRA, SECRA, CH2, DMDEC, SECDEC, FLUX(1,1),
     *               FLUX(2,1), FLUX(3,1), FLUX(4,1), XSCNT(NXSOUR)
                  END IF
               IF (LINE(43:43).EQ.' ') LINE(43:43) = '0'
               IF (LINE(58:58).EQ.' ') LINE(58:58) = '0'
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
C                                       Other IF fluxes
               IF ((NACROS.GE.105) .AND. (NUMIF.GT.1)) THEN
                  DO 120 I = 2,NUMIF
                     IF ((FLUX(1,I).GT.0.0) .OR. (DOCRT.GT.-2.5)) THEN
                        WRITE (LINE,1112) I, FLUX(1,I), FLUX(2,I),
     *                     FLUX(3,I), FLUX(4,I)
                        CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                     TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                        IF (IERR.NE.0) GO TO 950
                        END IF
 120                 CONTINUE
                  END IF
               END IF
 130        CONTINUE
      ELSE
         CALL AXEFND (2, 'RA', CATBLK(KIDIM), CATH(KHCTP), I, IERR)
         RAEPO = CATD(KDCRV+I)
         CALL AXEFND (3, 'DEC', CATBLK(KIDIM), CATH(KHCTP), I, IERR)
         DECEPO = CATD(KDCRV+I)
         CALL COORDD (1, RAEPO, CH1, HMRA, SECRA)
         CALL COORDD (2, DECEPO, CH2, DMDEC, SECDEC)
         WRITE (LINE,1110) 1, SXNAME(1), XQUAL(1), XCODE(1), CH1,
     *      HMRA, SECRA, CH2, DMDEC, SECDEC, XSCNT(1)
         IF (LINE(43:43).EQ.' ') LINE(43:43) = '0'
         IF (LINE(58:58).EQ.' ') LINE(58:58) = '0'
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
C                                       Get IF frequency info
      IF (.NOT.FQEXIS) THEN
         IIVER = 1
         CALL CHNDAT ('READ', NXBUFF, DISKIN, CNOIN, IIVER, CATBLK,
     *      INXLUN, NNIF, FOFF, KSID, FINC, BNDCOD, FRQSEL, IRET)
      ELSE
         CALL TABFQ ('READ', FQBUFF, SUFQID, FQKOLS, FQNUMV, NMFQIF,
     *      FQID, FOFF, FQCHBW, FQTBW, FQSIDE, BNDCOD, IRET)
         END IF
      IF (IRET.NE.0) GO TO 999
      IF (DOCRT.GT.-2.5) THEN
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
      WRITE (TITL2,1130)
      CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *    IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 950
      NLOOP = NSOURC
C                                       Only once if FQ same for all
      IF (SAMEFQ) NLOOP = 1
C                                       Frequency, velocity, rest freq.
      IF (ISSU) THEN
         DO 150 LOOP = 1,NLOOP
            SURNO = LOOP
            CALL TABSOU ('READ', BLBUFF, SURNO, SUKOLS, SUNUMV, IDSOU,
     *         SUNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) GO TO 999
            KQUAL = QUAL
            SFREQ = FREQ + (FOFF(1) + FREQO(1)) * 1.0D-9
            SLSRV = LSRVEL(1) * 1.0D-3
            SRF = RESTFQ(1) * 1.0D-9
C                                       If FQ info same for all sources
            IF (SAMEFQ) SUNAME = 'All Sources     '
C                                       If calc code OK
            CCODE = XCODE(IDSOU)(1:1)
            IF ((XCALCO(:1).EQ.' ') .OR. (CCODE.EQ.XCALCO(:1)) .OR.
     *         ((XCALCO(:1).EQ.'*') .AND. (CCODE.NE.' ')) .OR. (SAMEFQ)
     *         .OR. ((XCALCO(1:1).EQ.'-') .AND. (CCODE.EQ.' '))) THEN
               WRITE (LINE,1131) IDSOU, SUNAME, SFREQ, SLSRV, SRF
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
C                                       Rest of IF loop
               IF (NUMIF.GT.1) THEN
                  DO 140 I = 2,NUMIF
                     SFREQ = FREQ + (FOFF(I) + FREQO(I)) * 1.0D-9
                     SLSRV = LSRVEL(I) * 1.0D-3
                     SRF = RESTFQ(I) * 1.0D-9
                     WRITE (LINE,1132) I, SFREQ, SLSRV, SRF
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
 140                 CONTINUE
                  END IF
               END IF
 150        CONTINUE
C                                       Close source table
         CALL TABIO ('CLOS', 0, SURNO, BLBUFF, BLBUFF, IERR)
C                                       single source
      ELSE
         I = CATBLK(KIALT) / 256 + 1
         J = CATBLK(KIALT) - (I-1) * 256
         IF ((I.GE.1) .AND. (I.LE.2) .AND. (J.GE.1) .AND. (J.LE.3)) THEN
            SRF = CATD(KDRST) * 1.0D-9
            SLSRV = CATD(KDARV)
            SFREQ = CATD(KDCRV+JLOCF) * 1.0D-9
            WRITE (LINE,1131) 1, SXNAME(1), SFREQ, SLSRV, SRF
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
         END IF
C                                       FQ table stuff
C                                       New page
      IF (FQEXIS) THEN
C                                       if not enough room, new page
         IF ((IPCNT+(NUMFQE*NMFQIF)+3).GT.PRTMAX) IPCNT = 980
         LINE = ' '
         TITL2 = ' '
         IF (DOCRT.GT.-2.5) THEN
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
C                                       FQ summary
            REFF = CATR(KRCRP+JLOCF)
            WRITE (LINE,1150) REFF
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
C                                       List the FQ entries
         WRITE (TITL2,1151)
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         REFF = CATD(KDCRV+JLOCF)
         DO 170 LOOP = 1, NUMFQE
            CALL TABFQ ('READ', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NMFQIF,
     *         FQID, FQFRQ, FQCHBW, FQTBW, FQSIDE, BNDCOD, IRET)
            IF (IRET.NE.0) GO TO 999
            DO 160 I = 1,NMFQIF
               FQFRQ(I) = (FQFRQ(I)+REFF) * 1.0D-9
               FQTBW(I) = FQTBW(I) * 1.0E-3
               FQCHBW(I) = FQCHBW(I) * 1.0E-3
               IF (I.EQ.1) THEN
                  WRITE (LINE,1152) FQID, I, FQFRQ(I), FQTBW(I),
     *               FQCHBW(I), FQSIDE(I), BNDCOD(I)
               ELSE
                  WRITE (LINE,1153) I, FQFRQ(I), FQTBW(I), FQCHBW(I),
     *               FQSIDE(I), BNDCOD(I)
                  END IF
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
 160           CONTINUE
 170        CONTINUE
         END IF
      IRET = 0
      GO TO 999
C                                       Close files.
 950  IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1950) IERR
         CALL MSGWRT (8)
         IRET = 1
      ELSE
         IRET = -1
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('SCANUV: ERROR ',I3,' CHECKING ',A2,' TABLE')
 1030 FORMAT ('File = ',A12,'.',A6,'.',I4,' Vol =',I2,'  Userid =',I5)
 1031 FORMAT ('Freq =',F13.9,' GHz   Ncor =',I3,'   No. vis =',I10)
 1035 FORMAT ('Scan      Source      Qual  Calcode Sub',9X,'Timerange',
     *   10X,'FrqID   START VIS  END VIS')
 1036 FORMAT ('Scan      Source      Qual  Calcode Sub',6X,'Timerange',
     *   ' (LST)',7X,'FrqID   START VIS  END VIS')
 1040 FORMAT (I4,1X,A16,':',I5.4,2X,A4,2X,I4,I3,'/',I2.2,2(':',I2.2),
     *   ' - ',I3,'/',I2.2,2(':',I2.2),2X,I4,I8, 3X, I8)
 1050 FORMAT ('  ID Source',11X,'Qual  Calcode RA(',F6.1,')     Dec(',
     *   F6.1,')   IFlux   QFlux   UFlux   VFlux  No. vis')
 1051 FORMAT ('  ID Source',11X,'Qual  Calcode RA(',F6.1,')     Dec(',
     *   F6.1,')  No. vis')
 1060 FORMAT ('Velocity type = ''',A8,'''    Definition = ''',A8,'''')
 1061 FORMAT ('WARNING: File contains',I4,' subarrays - vis.',
     *   ' counts may not be accurate')
 1110 FORMAT (I4,1X,A16,':',I5.4,3X,A4,1X,A1,2(I2.2,':'),F7.4,1X,A1,
     *   2(I2.2,':'),F6.3,I8)
 1111 FORMAT (I4,1X,A16,':',I5.4,3X,A4,1X,A1,2(I2.2,':'),F7.4,1X,A1,
     *   2(I2.2,':'),F6.3,4F8.3,I8)
 1112 FORMAT (13X,'IF(',I2,')',44X,4F8.3)
 1130 FORMAT ('  ID Source',12X,'Freq(GHz) Velocity(Km/s)',
     *   ' Rest freq (GHz)')
 1131 FORMAT (I4,1X,A16,2X,F9.4,F15.4,F16.4)
 1132 FORMAT (5X,'IF(',I3,')',11X,F9.4,F15.4,F16.4)
 1150 FORMAT ('Frequency Table summary uses reference channel',F9.2)
 1151 FORMAT ('FQID IF#      Freq(GHz)      BW(kHz)   Ch.Sep(kHz)',
     *   '  Sideband  Bandcode')
 1152 FORMAT (I4,2X,I2,2X,F15.8,2X,F11.4,2X,F10.4,5X,I2,5X,A)
 1153 FORMAT (6X,I2,2X,F15.8,2X,F11.4,2X,F10.4,5X,I2,5X,A)
 1950 FORMAT ('SCANUV: ERROR',I5,' DOING I/O TO TERMINAL')
      END
      SUBROUTINE SCNMAX (SCANV, NANT, DTYPE, MTYPE, NCOLPV, SMAX, SFACT,
     *   AVG, SIG)
C-----------------------------------------------------------------------
C   Routine to find the maximum, non blank value in an array and
C   determine the proper scaling factor for printing.
C   Inputs:
C     SCANV(maxant,maxant,2)R    Scan values.
C     NANT                  I    Max. antenns number in scan.
C     DTYPE                 I    Data type, 0=amp, 1=phase, 2=RMS
C                                3=amp+rms, 4=phase+rms, 5=amp+phase
C     MTYPE                 I    Pointer for 3rd dim in SCANV.
C                                2 = rms.
C     NCOLPV                I    Number of columns per value printed
C   Output:
C     SMAX                  R    Maximum abs value.
C     SFACT                 R    Scaling factor to print values.
C     AVG                   R    Matrix average (phase in deg.)
C     SIG(2)                R    Sigma of average, of matrix
C-----------------------------------------------------------------------
      INTEGER   NANT, DTYPE, MTYPE, NCOLPV, I, J, ICNT
      REAL      SMAX, SFACT, AVG, SIG(2), VALUE, AVALUE, TEMP, XV(2),
     *   XAVG
      LOGICAL   ISFAZ
      DOUBLE PRECISION    SUM1, SUM2, XS1(2), XS2(2)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      REAL      SCANV(MAXANT,MAXANT,2)
C-----------------------------------------------------------------------
C                                       Find maximum non blank and do
C                                       sums for average, sigma
      SMAX = 0.0
      SFACT = 1.0
      SUM1 = 0.0D0
      SUM2 = 0.0D0
      ICNT = 0
      AVG = 0.0
      XAVG = 0.0
      SIG(1) = 0.0
      SIG(2) = 0.0
      XV(1) = 0.0
      XV(2) = 0.0
      XS1(1) = 0.0D0
      XS1(2) = 0.0D0
      XS2(1) = 0.0D0
      XS2(2) = 0.0D0
      IF (NANT.LE.0) GO TO 999
      ISFAZ = ((DTYPE.EQ.1) .OR. ((DTYPE.EQ.4).AND.(MTYPE.EQ.1)) .OR.
     *   ((DTYPE.EQ.5).AND.(MTYPE.EQ.2)))
      DO 100 I = 1,NANT
         DO 100 J = 1,NANT
            IF (SCANV(I,J,MTYPE).NE.FBLANK) THEN
               VALUE = SCANV(I,J,MTYPE)
               AVALUE = ABS (VALUE)
               SMAX = MAX (SMAX, AVALUE)
               SUM1 = SUM1 + VALUE
               SUM2 = SUM2 + VALUE * VALUE
               ICNT = ICNT + 1
               IF (ISFAZ) THEN
                  XV(1) = COS (SCANV(I,J,MTYPE))
                  XV(2) = SIN (SCANV(I,J,MTYPE))
                  XS1(1) = XS1(1) + XV(1)
                  XS1(2) = XS1(2) + XV(2)
                  XS2(1) = XS2(1) + XV(1) * XV(1)
                  XS2(2) = XS2(2) + XV(2) * XV(2)
                  END IF
               END IF
 100        CONTINUE
      IF (SMAX.LE.1.0E-20) GO TO 999
C                                       Scale for phase to degrees
      IF (ISFAZ) SMAX = SMAX * RAD2DG
      IF ((DTYPE.EQ.4).AND.(MTYPE.EQ.2)) SMAX = SMAX * RAD2DG
C                                       Determine scaling
      VALUE = ALOG (SMAX) / ALOG (10.0)
      I = - VALUE
      IF (VALUE.GT.0.0) I = I - 1
      SFACT = 10.0 ** (I + (NCOLPV-1))
      IF ((ISFAZ) .OR. ((DTYPE.EQ.4).AND.(MTYPE.EQ.2)))
     *   SFACT = SFACT * RAD2DG
C                                       Average and sigma
      IF (.NOT.ISFAZ) THEN
         IF (ICNT.GT.0) AVG = SUM1 / ICNT
         IF (ICNT.GT.3) THEN
            TEMP = (SUM2/ICNT) - AVG*AVG
            IF (TEMP.LT.0.0) TEMP = 0.0
            SIG(1) = SQRT (TEMP / (ICNT - 1))
            SIG(2) = SQRT (TEMP)
            END IF
         IF ((DTYPE.EQ.4).AND.(MTYPE.EQ.2)) THEN
            AVG = AVG * RAD2DG
            SIG(1) = SIG(1) * RAD2DG
            SIG(2) = SIG(2) * RAD2DG
            END IF
      ELSE
         IF (ICNT.GT.0) THEN
            XV(1) = XS1(1) / ICNT
            XV(2) = XS1(2) / ICNT
            AVG = ATAN2 (XV(2), (XV(1)+1.0E-20))
            XAVG = XS1(2) / ICNT
            END IF
         IF (ICNT.GT.3) THEN
            TEMP = (XS2(2)/ICNT) - XAVG*XAVG
            IF (TEMP.LT.0.0) TEMP = 0.0
            SIG(1) = SQRT (TEMP / (ICNT - 1))
            SIG(1) = ASIN(SIG(1))
            SIG(2) = SQRT (TEMP)
            SIG(2) = ASIN(SIG(2))
            END IF
         AVG = AVG * RAD2DG
         SIG(1) = SIG(1) * RAD2DG
         SIG(2) = SIG(2) * RAD2DG
         END IF
C
 999  RETURN
      END
      SUBROUTINE MATXCH (IRET)
C-----------------------------------------------------------------------
C   counts line for printer matrix listings of scan averaged uv data.
C   Info for UVGET is set in LSTRIN.
C   Output:
C      IRET   I    Return code, 0=OK, else failed; -1 too much
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER CCODE*1
      INTEGER   TIME(8), NANT, DTYPE, NPASS, IPASS, IANTHI, IANTLO,
     *   NANTPP, IROW, NCOLPV, IATY, IANT, MANT, I, J, MTYPE, NOTYPE,
     *   IPOLF, KANT, CATSAV(256)
      LOGICAL   AMPVEC, DONE, SAMESC
      REAL      CATR(256), SMAX, SFACT, SFACTI, AVG, SIG(2), DT,
     *    AVGS(2,2,2), SFACTS
      DOUBLE PRECISION    CATD(128), SFREQ
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANTLAB(MAXANT), ACTANS(MAXANT), NOANT, LUN,
     *   IUBUFF(512)
      REAL      SCANV(MAXANT,MAXANT,2), SCANVA(MAXANT,2)
      INCLUDE 'LISTR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATR, CATD), (IUBUFF, UBUFF)
C-----------------------------------------------------------------------
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
C                                       Setup
      AMPVEC = DPARM(2).GT.0.5
      IATY = 2
      IF (AMPVEC) IATY = 1
      DTYPE = DPARM(1) + 0.5
      IF (DTYPE.EQ.14) DTYPE = -1
      IF ((DTYPE.LT.-1) .OR. (DTYPE.GT.6)) DTYPE = 0
      NOTYPE = 1
      IF (DTYPE.GT.2) NOTYPE = 2
      SAMESC = (DPARM(6).EQ.0) .AND. (DTYPE.EQ.3)
      DT = DPARM(4) / 1440.0
C                                       Single source default = 10 min.
      IF (ISINGL) THEN
         IF (DPARM(4).LT.1.0E-7) DT = 10.0 / 1440.0
C                                       Multisource DT default = scan
      ELSE
         IF (DPARM(4).LT.1.0E-7) DT = 1.0E10
         END IF
C                                       Trap polarization case(s)
      IPOLF = 0
C                                       List RL,LR
      IF (STOKES.EQ.'POL ') THEN
         STOKES = 'FULL'
         IPOLF = 1
         END IF
C                                       List RL, Conjugate (LR)
      IF (STOKES.EQ.'POLC') THEN
         STOKES = 'FULL'
         IPOLF = -1
         END IF
C                                       Number of char per col.
      NCOLPV = 4
      IF ((DPARM(3).GT.3.6) .AND. (DPARM(3).LT.10.4))
     *   NCOLPV = DPARM(3) + 0.5
C                                       Init. vis record
      DO 10 I = 1,20
         RPARM(I) = 0.0
 10      CONTINUE
      RPARM(1) = FBLANK
C                                       Open uv data etc.
      CALL COPY (256, CATBLK, CATSAV)
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       what antennas in array
      LUN = 77
      CALL GETANS (DISKIN, CNOIN, CATUV, LUN, IUBUFF, SUBARR, NOANT,
     *   ACTANS, IRET)
      IF ((IRET.NE.0) .AND. (IRET.NE.9) .AND. (IRET.NE.10)) THEN
         WRITE (MSGTXT,1511) IRET
         IRET = 2
         GO TO 990
         END IF

C                                       Antenna list
      IANT = 1
      MANT = 1
      IF ((NANTSL.GT.0) .AND. (.NOT.DOAWNT)) MANT = NANTSL
      IF ((NANTSL.GT.0) .AND. DOAWNT) THEN
         DO 20 I = 1,NANTSL
            J = ANTENS(I)
            IF (ACTANS(J).GT.0) THEN
               ANTLAB(IANT) = J
               IANT = IANT + 1
               END IF
 20         CONTINUE
      ELSE
         DO 30 I = 1,MAXANT
C                                       Search list of deselected
C                                       antennas.
            DO 25 J = 1,MANT
               IF ((.NOT.DOAWNT) .AND. (ABS(ANTENS(J)).EQ.I)) GO TO 30
 25            CONTINUE
            IF (ACTANS(I).GT.0) THEN
               ANTLAB(IANT) = I
               IANT = IANT + 1
               END IF
 30         CONTINUE
         END IF
      NANTSL = IANT - 1
      CALL COPY (NANTSL, ANTLAB, ANTENS)
      DOAWNT = .TRUE.
      FREQ = BFREQ * 1.0D-9
C                                       first page titles
      NCOUNT = NCOUNT + 1
      IF (DOCRT.GT.-2.5) THEN
         NCOUNT = NCOUNT + 2
         IF (DOCAL) NCOUNT = NCOUNT + 1
         IF (DOPOL.GT.0) NCOUNT = NCOUNT + 1
         IF (DOBL) NCOUNT = NCOUNT + 1
         IF (DOFLAG) NCOUNT = NCOUNT + 1
         IF (DOBAND.GT.0) NCOUNT = NCOUNT + 1
         NCOUNT = NCOUNT + 1
         END IF
C                                       While data, Loop thru scans
 100  CONTINUE
         CALL SCANAV (AMPVEC, DTYPE, DT, SCANV, SCANVA, DPARM(9), TIME,
     *      NANT, IPOLF, RPARM, VIS, AVGS, IRET)
         DONE = IRET.LT.0
         IF (IRET.GT.0) GO TO 999
C                                       If calc code OK
         CCODE = CALCOD(1:1)
         IF ((XCALCO(:1).EQ.' ') .OR. (CCODE.EQ.XCALCO(:1)) .OR.
     *      ((XCALCO(:1).EQ.'*') .AND. (CCODE.NE.' ')) .OR.
     *      ((XCALCO(:1).EQ.'-') .AND. (CCODE.EQ.' ') .AND.
     *      (NANT.GT.0))) THEN
C                                       Determine number of passes
            NPASS = ((1.0 * NCOLPV * NANT) / (NACROS - NCOLPV)) + 0.999
            NANTPP = (NACROS - 5) / NCOLPV
C                                       Loop for matrix type.
            DO 650 MTYPE = 1,NOTYPE
C                                       Find max value
               SFACTS = SFACT
               CALL SCNMAX (SCANV, NANT, DTYPE, MTYPE, NCOLPV, SMAX,
     *            SFACT, AVG, SIG)
               IF ((DTYPE.EQ.1) .OR. (DTYPE.EQ.4)) THEN
                  IF (DPARM(10).GT.0.0) THEN
                     SFACT = RAD2DG / DPARM(10)
                  ELSE IF (DPARM(7).LE.0.0) THEN
                     SFACT =  RAD2DG
                     END IF
               ELSE IF ((DTYPE.EQ.5) .AND. (MTYPE.EQ.2)) THEN
                  IF (DPARM(7).LE.0.0) SFACT =  RAD2DG
               ELSE IF (DPARM(10).GT.0.0) THEN
                  SFACT = 1.0 / DPARM(10)
                  END IF
               IF ((SAMESC) .AND. (MTYPE.EQ.2)) SFACT = SFACTS
               SFACTI = 1000.0 / SFACT
C                                       Header for scan (2 blank lines
C                                       first - first pass only)
               IF (MTYPE.EQ.1) THEN
                  IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
                  NCOUNT = NCOUNT + 1
C                                       Source info
                  SFREQ = (BFREQ + FREQO(BIF) + FREQIF(BIF)) * 1.0D-9
                  IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
                  END IF
               NCOUNT = NCOUNT + 1
               IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
C                                       Auto correlation
               IF ((DTYPE.EQ.0) .AND. (DOACOR)) THEN
                  NCOUNT = NCOUNT + 1
                  IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
                  END IF
               DO 600 IPASS = 1,NPASS
                  IANTLO = (IPASS - 1) * NANTPP + 1
                  IANTHI = IANTLO + NANTPP - 1
                  IANTHI = MIN (IANTHI, NANT)
C                                       Section label
                  IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
                  IF (((IPCNT.GT.3) .AND. (IPCNT.LT.PRTMAX-1)) .OR.
     *               (DOCRT.LE.-2.5)) NCOUNT = NCOUNT + 1
                  IF (DTYPE.EQ.0) THEN
                     IF ((ICOR0.EQ.1) .OR. (NCOR.EQ.1)) THEN
                        KANT = NANT + 1
                     ELSE
                        KANT = NANT + 2
                        END IF
                  ELSE
                     KANT = NANT
                     END IF
                  DO 400 IROW = 1, KANT
C                                       Write row
                     NCOUNT = NCOUNT + 1
 400                 CONTINUE
 600              CONTINUE
C                                       Give matrix avg, sigma
               NCOUNT = NCOUNT + 1
               IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 2
 650           CONTINUE
C                                       End If ok to print
            END IF
C                                       end while more data - loop
         IF (.NOT.DONE) GO TO 100
      IRET = 0
      CALL COPY (256, CATSAV, CATBLK)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('MATXCH: ERROR',I4,' DETERMINING UV-FILE PARAMETERS')
 1511 FORMAT ('MATXCH: GETANS ERROR ',I3,' SEARCHING ANTENNA TABLES')
      END
      SUBROUTINE MATXUV (IRET)
C-----------------------------------------------------------------------
C   Gives matrix listings of scan averaged uv data.
C   Info for UVGET is set in LSTRIN.
C   Output:
C      IRET   I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      CHARACTER ENTRY*20, ATYPE(2)*12, FRMAT(10)*5, FORM*5, CHLINE*132,
     *   CCODE*1, POLA1*3, POLA2*3, LTYPE*12, PART(2)*5
      INTEGER   TIME(8), NANT, DTYPE, NPASS, IPASS, IANTHI, IANTLO,
     *   NANTPP, IROW, ICOL, NCOLPV, COLPNT, IATY, IRET, IANT, MANT,
     *   I, J, IERR, MTYPE, NOTYPE, IPOLF, I4TEMP, KQUAL, KANT
      LOGICAL   AMPVEC, DONE, SAMESC
      REAL      CATR(256), XROUND, SMAX, SFACT, SFACTI, AVG, SIG(2), DT,
     *   AVGS(2,2,2), SFACTS
      DOUBLE PRECISION    CATD(128), SFREQ
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANTLAB(MAXANT), ACTANS(MAXANT), NOANT, LUN,
     *   IUBUFF(512)
      REAL      SCANV(MAXANT,MAXANT,2), SCANVA(MAXANT,2)
      INCLUDE 'LISTR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATR, CATD), (IUBUFF, UBUFF)
      DATA FRMAT /'(I1)','(I2)','(I3)','(I4)','(I5)','(I6)','(I7)',
     *   '(I8)','(I9)','(I10)'/
      DATA ATYPE /'Ampscalar   ', 'Vector      '/
      DATA PART /'upper','lower'/
C-----------------------------------------------------------------------
C                                       Setup
      AMPVEC = DPARM(2).GT.0.5
      IATY = 2
      IF (AMPVEC) IATY = 1
      DTYPE = DPARM(1) + 0.5
      IF (DTYPE.EQ.14) DTYPE = -1
      IF ((DTYPE.LT.-1) .OR. (DTYPE.GT.6)) THEN
         WRITE (MSGTXT,1000) DTYPE
         CALL MSGWRT (6)
         DTYPE = 0
         END IF
      NOTYPE = 1
      IF (DTYPE.GT.2) NOTYPE = 2
      SAMESC = (DPARM(6).EQ.0) .AND. (DTYPE.EQ.3)
      DT = DPARM(4) / 1440.0
C                                       Single source default = 10 min.
      IF (ISINGL) THEN
         IF (DPARM(4).LT.1.0E-7) DT = 10.0 / 1440.0
C                                       Multisource DT default = scan
      ELSE
         IF (DPARM(4).LT.1.0E-7) DT = 1.0E10
         END IF
C                                       Trap polarization case(s)
      IPOLF = 0
C                                       Stokes = '' => 'HALF'
C      IF ((STOKES.EQ.' ') .AND. (DOPOL.LE.0)) STOKES = 'HALF'
C                                       List RL,LR
      IF (STOKES.EQ.'POL ') THEN
         STOKES = 'FULL'
         IPOLF = 1
         END IF
C                                       List RL, Conjugate (LR)
      IF (STOKES.EQ.'POLC') THEN
         STOKES = 'FULL'
         IPOLF = -1
         END IF
C                                       Number of char per col.
      NCOLPV = 4
      IF ((DPARM(3).GT.3.6) .AND. (DPARM(3).LT.10.4))
     *   NCOLPV = DPARM(3) + 0.5
      FORM = FRMAT(NCOLPV)
C                                       Init. vis record
      DO 10 I = 1,20
         RPARM(I) = 0.0
 10      CONTINUE
      RPARM(1) = FBLANK
C                                       Open uv data etc.
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
          END IF
C                                       what antennas in array
      LUN = 77
      CALL GETANS (DISKIN, CNOIN, CATUV, LUN, IUBUFF, SUBARR, NOANT,
     *   ACTANS, IRET)
      IF ((IRET.NE.0) .AND. (IRET.NE.9) .AND. (IRET.NE.10)) THEN
         WRITE (MSGTXT,1511) IRET
         IRET = 2
         GO TO 990
         END IF

C                                       Antenna list
      IANT = 1
      MANT = 1
      IF ((NANTSL.GT.0) .AND. (.NOT.DOAWNT)) MANT = NANTSL
      IF ((NANTSL.GT.0) .AND. DOAWNT) THEN
         DO 20 I = 1,NANTSL
            J = ANTENS(I)
            IF (ACTANS(J).GT.0) THEN
               ANTLAB(IANT) = J
               IANT = IANT + 1
               END IF
 20         CONTINUE
      ELSE
         DO 30 I = 1,MAXANT
C                                       Search list of deselected
C                                       antennas.
            DO 25 J = 1,MANT
               IF ((.NOT.DOAWNT) .AND. (ABS(ANTENS(J)).EQ.I)) GO TO 30
 25            CONTINUE
            IF (ACTANS(I).GT.0) THEN
               ANTLAB(IANT) = I
               IANT = IANT + 1
               END IF
 30         CONTINUE
         END IF
      NANTSL = IANT - 1
      CALL COPY (NANTSL, ANTLAB, ANTENS)
      DOAWNT = .TRUE.
      FREQ = BFREQ * 1.0D-9
C                                       first page titles
      IF (NACROS.GE.90) THEN
         WRITE (LINE,1040) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER, BCHAN,
     *      ECHAN, BIF
      ELSE
         WRITE (LINE,1041) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER, BCHAN,
     *      ECHAN, BIF
         END IF
      CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 950
      IF (DOCRT.GT.-2.5) THEN
         WRITE (LINE,1042) FREQ, NCOR, NVIS
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         WRITE (LINE,1043) STOKES, SUBARR
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         IF (DOCAL) THEN
            WRITE (LINE,1050) CLUSE
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
         IF (DOPOL.GT.0) THEN
            LINE = 'Applying polarization corrections'
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
         IF (DOBL) THEN
            WRITE (LINE,1051) BLVER
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
         IF (DOFLAG) THEN
            WRITE (LINE,1052) FGVER
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
         IF (DOBAND.GT.0) THEN
            WRITE (LINE,1053) BPVER
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
C                                       While data, Loop thru scans
 100  CONTINUE
         CALL SCANAV (AMPVEC, DTYPE, DT, SCANV, SCANVA, DPARM(9), TIME,
     *      NANT, IPOLF, RPARM, VIS, AVGS, IRET)
         DONE = IRET.LT.0
         IF (IRET.GT.0) GO TO 999
C                                       If calc code OK
         CCODE = CALCOD(1:1)
         IF ((XCALCO(:1).EQ.' ') .OR. (CCODE.EQ.XCALCO(:1)) .OR.
     *      ((XCALCO(:1).EQ.'*') .AND. (CCODE.NE.' ')) .OR.
     *      ((XCALCO(:1).EQ.'-') .AND. (CCODE.EQ.' ') .AND.
     *      (NANT.GT.0))) THEN
C                                       Determine number of passes
            NPASS = ((1.0 * NCOLPV * NANT) / (NACROS - NCOLPV)) + 0.999
            NANTPP = (NACROS - 5) / NCOLPV
C                                       Loop for matrix type.
            DO 650 MTYPE = 1,NOTYPE
C                                       Find max value
               SFACTS = SFACT
               CALL SCNMAX (SCANV, NANT, DTYPE, MTYPE, NCOLPV, SMAX,
     *            SFACT, AVG, SIG)
               IF ((DTYPE.EQ.1) .OR. (DTYPE.EQ.4)) THEN
                  IF (DPARM(10).GT.0.0) THEN
                     SFACT = RAD2DG / DPARM(10)
                  ELSE IF (DPARM(7).LE.0.0) THEN
                     SFACT =  RAD2DG
                     END IF
               ELSE IF ((DTYPE.EQ.5) .AND. (MTYPE.EQ.2)) THEN
                  IF (DPARM(7).LE.0.0) SFACT =  RAD2DG
               ELSE IF (DPARM(10).GT.0.0) THEN
                  SFACT = 1.0 / DPARM(10)
                  END IF
               IF ((SAMESC) .AND. (MTYPE.EQ.2)) SFACT = SFACTS
               SFACTI = 1000.0 / SFACT
C                                       Header for scan (2 blank lines
C                                       first - first pass only)
               IF (MTYPE.EQ.1) THEN
                  IF (DOCRT.GT.-2.5) THEN
                     LINE = ' '
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  KQUAL = QUAL
                  WRITE (LINE,1100) TIME, SNAME, KQUAL
                  IF (DPARM(9).GT.0.0) LINE(1:4) = 'LST '
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
C                                       Source info
                  SFREQ = (BFREQ + FREQO(BIF) + FREQIF(BIF)) * 1.0D-9
                  IF (DOCRT.GT.-2.5) THEN
                     WRITE (LINE,1101) FLUX(1,BIF), CALCOD, SFREQ
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  END IF
C                                       Type of data listed
C                                       Weight sums
               IF (DTYPE.EQ.-1) WRITE (LINE,1113) SFACTI
C                                       Amplitude
               LTYPE = ATYPE (IATY)
               IF ((DTYPE.EQ.0) .OR. ((DTYPE.GE.3) .AND. (MTYPE.EQ.1)))
     *            WRITE (LINE,1110) SFACTI, ATYPE(IATY)
C                                       Phase
               IF ((DTYPE.EQ.1) .OR. ((DTYPE.EQ.4) .AND. (MTYPE.EQ.1))
     *            .OR. ((DTYPE.EQ.5) .AND. (MTYPE.EQ.2))) THEN
                  WRITE (LINE,1111) SFACTI*RAD2DG, ATYPE(2)
                  LTYPE = ATYPE(2)
                  END IF
C                                       rms
               IF ((DTYPE.EQ.2) .OR. ((DTYPE.EQ.3) .AND. (MTYPE.EQ.2)))
     *            WRITE (LINE,1112) SFACTI, ATYPE(IATY)
               IF ((DTYPE.EQ.4) .AND. (MTYPE.EQ.2))
     *            WRITE (LINE,1114) SFACTI*RAD2DG, ATYPE(IATY)
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
C                                       Polarizations
               LINE = STOKES(:1) // 'POL in upper right'
               IF ((ICOR0.EQ.-1) .AND. (NCOR.EQ.1)) THEN
                  LINE = 'RCP in upper right'
                  POLA1 = 'RCP'
                  END IF
               IF ((ICOR0.EQ.-2) .AND. (NCOR.EQ.1)) THEN
                  LINE = 'LCP in upper right'
                  POLA1 = 'LCP'
                  END IF
               IF ((ICOR0.EQ.-1) .AND. (NCOR.GE.2)) THEN
                  LINE = 'RCP in upper right, LCP in lower left'
                  POLA1 = 'RCP'
                  POLA2 = 'LCP'
                  END IF
               IF ((ICOR0.EQ.-5) .AND. (NCOR.EQ.1)) THEN
                  LINE = 'XX in upper right'
                  POLA1 = 'XX'
                  END IF
               IF ((ICOR0.EQ.-6) .AND. (NCOR.EQ.1)) THEN
                  LINE = 'YY in upper right'
                  POLA1 = 'YY'
                  END IF
               IF ((ICOR0.EQ.-5) .AND. (NCOR.GE.2)) THEN
                  LINE = 'XX in upper right, YY in lower left'
                  POLA1 = 'XX'
                  POLA2 = 'YY'
                  END IF
               IF (ICOR0.EQ.1) THEN
                  LINE = 'IPOL in upper right'
                  POLA1 = 'IPO'
                  END IF
               IF ((ICOR0.EQ.-1) .AND.
     *            (NCOR.GE.2) .AND. (IPOLF.EQ.1)) THEN
                  LINE = 'RL in upper right, LR in lower left'
                  POLA1 = 'RL'
                  POLA2 = 'LR'
                  END IF
               IF ((ICOR0.EQ.-1) .AND.
     *            (NCOR.GE.2) .AND. (IPOLF.EQ.-1)) THEN
                  LINE = 'RL in upper right, conjg(LR) in lower left'
                  POLA1 = 'RL'
                  POLA2 = 'CLR'
                  END IF
               IF ((ICOR0.EQ.-5) .AND.
     *            (NCOR.GE.2) .AND. (IPOLF.EQ.1)) THEN
                  LINE = 'XY in upper right, YX in lower left'
                  POLA1 = 'XY'
                  POLA2 = 'YX'
                  END IF
               IF ((ICOR0.EQ.-5) .AND.
     *            (NCOR.GE.2) .AND. (IPOLF.EQ.-1)) THEN
                  LINE = 'XY in upper right, conjg(YX) in lower left'
                  POLA1 = 'XY'
                  POLA2 = 'CYX'
                  END IF
               IF (DOCRT.GT.-2.5) THEN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
                  END IF
C                                       Auto correlation
               IF ((DTYPE.EQ.0) .AND. (DOACOR)) THEN
                  WRITE (LINE,1105) SFACTI*100
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
                  IF (DOCRT.GT.-2.5) THEN
                     LINE = 'Two last lines correspond to auto' //
     *                  ' correlation'
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  END IF
               DO 600 IPASS = 1,NPASS
                  IANTLO = (IPASS - 1) * NANTPP + 1
                  IANTHI = IANTLO + NANTPP - 1
                  IANTHI = MIN (IANTHI, NANT)
C                                       Section label
                  IF (DOCRT.GT.-2.5) THEN
                     LINE = ' '
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  KQUAL = QUAL
                  WRITE (TITL1,1100) TIME, SNAME, KQUAL
                  TITL2 = 'Ant'
                  COLPNT = 5
                  DO 150 ICOL = IANTLO,IANTHI
                     WRITE (ENTRY,1120) ANTLAB(ICOL)
                     TITL2(COLPNT:COLPNT+NCOLPV-1) = ENTRY(:NCOLPV)
                     COLPNT = COLPNT + NCOLPV
 150                 CONTINUE
                  IF (((IPCNT.GT.3) .AND. (IPCNT.LT.PRTMAX-1)) .OR.
     *               (DOCRT.LE.-2.5)) THEN
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, TITL2, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  IF (DTYPE.EQ.0) THEN
                     IF ((ICOR0.EQ.1) .OR. (NCOR.EQ.1)) THEN
                        KANT = NANT + 1
                     ELSE
                        KANT = NANT + 2
                        END IF
                  ELSE
                     KANT = NANT
                     END IF
                  DO 400 IROW = 1, KANT
                     IF (IROW .LE. NANT) THEN
                        WRITE (LINE,1150) ANTLAB(IROW)
                     ELSE
                        IF (IROW.EQ.NANT+1) THEN
                           WRITE (LINE,1160) POLA1
                        ELSE
                           WRITE (LINE,1170) POLA2
                           END IF
                        END IF
                     COLPNT = 5
                     DO 200 ICOL = IANTLO,IANTHI
                        IF (IROW .LE. NANT) THEN
                        IF (SCANV(IROW,ICOL,MTYPE).NE.FBLANK) THEN
                           IF (SCANV(IROW,ICOL,MTYPE).LT.0.0) THEN
                              XROUND = -0.5
                           ELSE
                              XROUND = 0.5
                              END IF
                           I4TEMP = SFACT *
     *                        SCANV(IROW,ICOL,MTYPE) + XROUND
                           WRITE (CHLINE,FORM) I4TEMP
                           LINE(COLPNT:COLPNT+NCOLPV-1) =
     *                        CHLINE(:NCOLPV)
                           END IF
                           END IF
                        IF (IROW.EQ.NANT + 1) THEN
                           IF (SCANVA(ICOL,1) .NE. FBLANK) THEN
                              I4TEMP = (SFACT/100.0)* SCANVA(ICOL,1)
     *                           + 0.5
                              WRITE (CHLINE,FORM) I4TEMP
                              LINE(COLPNT:COLPNT+NCOLPV-1) =
     *                           CHLINE(:NCOLPV)
                              END IF
                           END IF
                        IF (IROW.EQ.NANT + 2) THEN
                           IF (SCANVA(ICOL,2) .NE. FBLANK) THEN
                              I4TEMP = (SFACT/100.0)* SCANVA(ICOL,2)
     *                           + 0.5
                              WRITE (CHLINE,FORM) I4TEMP
                              LINE(COLPNT:COLPNT+NCOLPV-1) =
     *                           CHLINE(:NCOLPV)
                              END IF
                           END IF
                        COLPNT = COLPNT + NCOLPV
 200                    CONTINUE
C                                       Write row
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
 400                 CONTINUE
C                                       Blank titles
                  TITL1 = ' '
                  TITL2 = ' '
 600              CONTINUE
C                                       Give matrix avg, sigma
               WRITE (LINE,1600) AVG, SIG
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
               IF (DOCRT.GT.-2.5) THEN
                  DO 610 I = 1,2
                     IF ((AVGS(1,I,MTYPE).NE.0.0) .OR.
     *                  (AVGS(2,I,MTYPE).NE.0.0)) THEN
                        WRITE (LINE,1601) LTYPE(:10), PART(I),
     *                     AVGS(1,I,MTYPE), AVGS(2,I,MTYPE)
                        CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                     TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                        IF (IERR.NE.0) GO TO 950
                        END IF
 610                 CONTINUE
                  END IF
 650           CONTINUE
C                                       End If ok to print
            END IF
C                                       end while more data - loop
         IF (.NOT.DONE) GO TO 100
      IRET = 0
      GO TO 999
C                                       CRT error
 950  IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1950) IERR
         CALL MSGWRT (8)
         IRET = 1
      ELSE
         IRET = -1
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MATXUV: REQESTED DISPLAY TYPE',I3,' CHANGED TO 0')
 1010 FORMAT ('MATXUV: ERROR',I4,' DETERMINING UV-FILE PARAMETERS')
 1040 FORMAT ('File = ',A12,'.',A6,'.',I4,'   Vol =',I2,'  Userid =',
     *   I5,'   Chans=',I5,' -',I5,'   IF =',I2)
 1041 FORMAT (A12,'.',A6,'.',I4,'  Vol=',I2,'  User=',I5,'  Chans=',
     *   I5,' -',I5,'  IF=',I2)
 1042 FORMAT ('Freq=',F13.9,' GHz   Ncor=',I3,'   No. vis=',I10)
 1043 FORMAT ('Stokes = ',A4,' Subarray = ',I3)
 1050 FORMAT ('Applying calibration table ',I3)
 1051 FORMAT ('Applying baseline table ',I3)
 1052 FORMAT ('Applying flag table ',I3)
 1053 FORMAT ('Applying bandpass table ',I3)
 1100 FORMAT ('Time =',I4,'/',2(I2.2,':'),I2.2,' to',I4,'/',2(I2.2,':'),
     *   I2.2,'   Source = ',A16,':',I5.4)
 1101 FORMAT ('Flux =',F8.4,' Jy, Calcode = ',A4,', Freq =',F13.9,
     *   ' GHz')
 1105 FORMAT ('Amplitudes of AUTO, 1000 =',F13.6,
     *           ' Jy, averaging type = Scalar')
 1110 FORMAT ('Amplitudes, 1000 =',F8.3,' Jy, averaging type = ',A12)
 1111 FORMAT ('Phase, 1000 =',F8.2,' degrees, averaging type = ',A12)
 1112 FORMAT ('Rms, 1000 =',F8.3,' Jy, averaging type = ',A12)
 1113 FORMAT ('Weight sums, 1000 =',1PE10.2)
 1114 FORMAT ('Phase rms, 1000 =',F8.2,' deg, averging type = ',A12)
 1120 FORMAT ('--',I2,'------')
 1150 FORMAT (I3,'|',128X)
 1160 FORMAT (A3,'|',128X)
 1170 FORMAT (A3,'|',128X)
 1511 FORMAT ('MATXUV: GETANS ERROR ',I3,' SEARCHING ANTENNA TABLES')
 1600 FORMAT ('Ampscalar average of matrix    =',1PE10.3,'(',1PE10.3,
     *   ') sigma =',1PE10.3)
 1601 FORMAT (A10,'average of ',A,' data=',1PE10.3,11X,' sigma =',
     *   1PE10.3)
 1950 FORMAT ('MATXUV: ERROR',I5,' DOING I/O TO TERMINAL')
      END
      SUBROUTINE SCANAV (AMPVEC, DTYPE, DT, SCANV, SCANVA, DP9, TIME,
     *   NANT, IPOLF, RPARM, VIS, AVGS, IERR)
C-----------------------------------------------------------------------
C   Reads an indexed uv data base and returns scan averages of amp,
C   phase or the RMS scatter.  Needs to be initialized by a call to
C   UVGET.  The  order of the antennas returned in SCANV is defined by
C   the order in the common array ANTENS unless all antennas were
C   specified, (NANTSL = 0).
C      IF 2 polarizations were specified to UVGET then they will be
C   returned in the two halves of SCANV.
C   Inputs:
C     AMPVEC     L    If true do ampscalar averaging else vector.
C     DTYPE      I    Output type, -1 => weight, 0 => amplitude, 1=>
C                     phase, 2 => amplitude RMS, 3=amp+rms, 4=phase+rms.
C                     5 = amp+phase.
C     DT         R    Maximum length integration in days.
C     IPOLF      I    Polarization flag, 0=normal, 1=RL,LR,
C                     -1 = RL,CONJG(LR)
C   Input/Output:
C     RPARM(20)  R    Random parameter array, first record of call.
C                     (1) = 'INDE' => don't use.
C     VIS(*)     R    Visibility array, first record of call.
C   Outputs:
C     SCANV(maxant,maxant,2) R   The result for antennas I<J,
C                     (I,J,*) = first polarization
C                     (J,I,*) = second polarization
C                     Undefined values will contain 'INDE'.
C                     Note: maxant is defined in the parameter include
C                     INCS:PUVD.INC.  Third dimension used for first and
C                     second data types (e.g. amp. + rms).
C     SCANVA(MAXANT,2) R  The amplitude of auto correlation.
C     TIME(8)    I    Time range, start, stop; days, hours, min, sec.
C     NANT       I    Highest antenna number encountered.
C                     Actually highest index in SCANV
C     AVGS(2,2,2) R   Average & rms for each of the 2 types done in the
C                     AMPVEC style
C     IERR       I    Return code, 0 => OK, -1 => out of data,
C                     > 0 => failed.
C   Output to common in D/CSOU.INC
C     SNAME      C    Source name (16 char)
C     QUAL       I    Source qualifier.
C     CALCOD     R    Calibrator code 4 char.
C     FLUX(4,IF) R    Total flux density I, Q, U, V pol, (Jy) each IF
C     FREQO(IF)  D    Frequency offset (Hz)
C   Related values in common:
C     NANTSL     I    Number of antennas selected, 0 > all.
C     ANTENS(*)  I    Antenna numbers in order used in SCANV.
C      If the end of data is encountered (IERR=-1) then UVGET is called
C   with OPCODE='CLOS'.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL   AMPVEC
      REAL      DT, DP9, SCANV(MAXANT,MAXANT,2), SCANVA(MAXANT,2),
     *   RPARM(*), VIS(*), AVGS(2,2,2)
      INTEGER   TIME(8), NANT, IPOLF, IERR
C
      REAL      T1, T2, CATUV4(256), PFACT
      INTEGER   MXANT, I, J, IA1, IA2, JA1, JA2, KA1, KA2, SUNUM, LIMIT,
     *   LIMIT1, JERR, NNANT, ISLUN, IANT, DDTYPE, IPOFF, DTYPE, SCANUM,
     *   KVIS, IVIS, IPTR
      LOGICAL   ORDER, DONE1, GOTDAT, DORMS2, DOFAZ2
      INTEGER   ENSANT(MAXANT)
      DOUBLE PRECISION WORK(MAXANT,MAXANT,6), WORKA(MAXANT,2), W, SR(4),
     *   SI(4), COUNTA(MAXANT,2), COUNT(MAXANT,MAXANT,4), SW(4), SSR(4),
     *   SSI(4), AMP, TEMP, TEMP1, TEMP2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATUV, CATUV4)
      DATA ISLUN /26/
      DATA MXANT /MAXANT/
C-----------------------------------------------------------------------
      DOXCOR = .TRUE.
C                                       See if first record read
      DONE1 = RPARM(1).NE.FBLANK
      GOTDAT = .FALSE.
      DDTYPE = MOD (DTYPE, 3)
      IF (DTYPE.EQ.-1) DDTYPE = -1
      IF (DTYPE.EQ.5) DDTYPE = 0
      DORMS2 = (DTYPE.EQ.3) .OR. (DTYPE.EQ.4)
      DOFAZ2 = DTYPE.EQ.5
      KVIS = (LREC-NRPARM) / 3
      KVIS = KVIS / NCOR
C                                       Polarization
      IPOFF = 0
      IF (IPOLF.NE.0) IPOFF = 6
      PFACT = 1.0
      IF (IPOLF.EQ.-1) PFACT = -1.0
C                                       Clear arrays
 10   ORDER = NANTSL.GT.0
      NANT = NANTSL
      IF ((NANT.LE.0) .OR. (.NOT.DOAWNT)) NANT = MXANT
      NNANT = NANTSL
      DO 50 I = 1,NANT
         COUNTA(I,1) = 0.0D0
         COUNTA(I,2) = 0.0D0
         WORKA(I,1) = 0.D0
         WORKA(I,2) = 0.D0
         SCANVA(I,1) = FBLANK
         SCANVA(I,2) = FBLANK
         DO 50 J = 1,NANT
            COUNT(I,J,1) = 0.0D0
            COUNT(I,J,2) = 0.0D0
            COUNT(I,J,3) = 0.0D0
            COUNT(I,J,4) = 0.0D0
            WORK(I,J,1) = 0.0D0
            WORK(I,J,2) = 0.0D0
            WORK(I,J,3) = 0.0D0
            WORK(I,J,4) = 0.0D0
            WORK(I,J,5) = 0.0D0
            WORK(I,J,6) = 0.0D0
            SCANV(I,J,1) = FBLANK
            SCANV(I,J,2) = FBLANK
 50         CONTINUE
C                                       Get address array for antennas
      CALL FILL (MXANT, 1, ENSANT)
C                                       Antennas selected
      DO 60 I = 1,NANT
         J = ABS(ANTENS(I))
         IF (J.GT.0) ENSANT(J) = I
 60      CONTINUE
      IF (DOAWNT .OR. (NANTSL.LE.0)) GO TO 90
C                                       Antennas deselected
      IANT = 1
      DO 80 I = 1,NANT
         DO 70 J = 1,NANTSL
            IF (ABS(ANTENS(J)).EQ.I) GO TO 80
 70         CONTINUE
C                                       Add antenna I to list.
         ENSANT(I) = IANT
         IANT = IANT + 1
 80      CONTINUE
C                                       Initialize time
 90   T1 = 1.0E10
      T2 = 1.0E10
C                                       Save scan number (0= no index)
      SCANUM = INXRNO
C                                       Loop reading data
 100     IF (.NOT.DONE1) THEN
            CALL UVGET ('READ', RPARM, VIS, IERR)
            IF (IERR.GT.0) GO TO 999
            END IF
         DONE1 = .FALSE.
C                                       Check if scan done
         IF ((INXRNO.GT.SCANUM) .OR. (IERR.LT.0) .OR.
     *      (RPARM(1+ILOCT).GT.(T1+DT))) GO TO 500
C                                       Antenna numbers
         IF (ILOCB.GE.0) THEN
            JA1 = RPARM(ILOCB+1) / 256. + 0.1
            JA2 = RPARM(ILOCB+1) - JA1 * 256 + 0.1
         ELSE
            JA1 = RPARM(ILOCA1+1) + 0.1
            JA2 = RPARM(ILOCA2+1) + 0.1
            END IF
         KA1 = JA1
         KA2 = JA2
         IF (ORDER) KA1 = ENSANT(JA1)
         IF (ORDER) KA2 = ENSANT(JA2)
         IA1 = MIN (KA1, KA2)
         IA2 = MAX (KA1, KA2)
         NNANT = MAX (NNANT, IA2)
         GOTDAT = .TRUE.
C                                       Time
         T2 = RPARM(ILOCT+1)
         IF (T1.GT.1.0E5) T1 = T2
C                                       Source no.
         SUNUM = CURSOU
C                                       Vector average:
C                                       Accumulate 1 st. poln.
         IPTR = IPOFF
         DO 120 IVIS = 1,KVIS
            IF (VIS(IPTR+3).GT.0.0) THEN
               W = VIS(IPTR+3)
               IF (IA1.EQ.IA2) THEN
                  COUNTA(IA1,1) = COUNTA(IA1,1) + W
                  WORKA(IA1,1) = WORKA(IA1,1) + VIS(IPTR+1)*W
               ELSE
                  COUNT(IA1,IA2,1) = COUNT(IA1,IA2,1) + W
                  COUNT(IA1,IA2,3) = COUNT(IA1,IA2,3) + W**2
                  WORK(IA1,IA2,1) = WORK(IA1,IA2,1) + VIS(IPTR+1)*W
                  WORK(IA2,IA1,1) = WORK(IA2,IA1,1) + VIS(IPTR+2)*W
                  WORK(IA1,IA2,2) = WORK(IA1,IA2,2) + (VIS(IPTR+1)**2)*W
                  WORK(IA2,IA1,2) = WORK(IA2,IA1,2) + (VIS(IPTR+2)**2)*W
                  IF (AMPVEC) THEN
                     AMP = SQRT (VIS(IPTR+1) * VIS(IPTR+1) + VIS(IPTR+2)
     *                  * VIS(IPTR+2))
                     WORK(IA1,IA2,5) = WORK(IA1,IA2,5) + AMP*W
                     WORK(IA2,IA1,5) = WORK(IA2,IA1,5) + AMP*AMP*W
                     END IF
                  END IF
               END IF
C                                       Accumulate 2 nd. poln.
            IF ((NCOR.GT.1) .AND. (VIS(IPTR+6).GT.0.0)) THEN
               W = VIS(IPTR+6)
               IF (IA1.EQ.IA2) THEN
                  COUNTA(IA1,2) = COUNTA(IA1,2) + W
                  WORKA(IA1,2) = WORKA(IA1,2) + VIS(IPTR+4)*W
               ELSE
                  COUNT(IA1,IA2,2) = COUNT(IA1,IA2,2) + W
                  COUNT(IA1,IA2,4) = COUNT(IA1,IA2,4) + W**2
                  WORK(IA1,IA2,3) = WORK(IA1,IA2,3) + VIS(IPTR+4)*W
                  WORK(IA2,IA1,3) = WORK(IA2,IA1,3) + VIS(IPTR+5)*W
                  WORK(IA1,IA2,4) = WORK(IA1,IA2,4) + (VIS(IPTR+4)**2)*W
                  WORK(IA2,IA1,4) = WORK(IA2,IA1,4) + (VIS(IPTR+5)**2)*W
                  IF (AMPVEC) THEN
                     AMP = SQRT (VIS(IPTR+4) * VIS(IPTR+4) + VIS(IPTR+5)
     *                  * VIS(IPTR+5))
                     WORK(IA1,IA2,6) = WORK(IA1,IA2,6) + AMP*W
                     WORK(IA2,IA1,6) = WORK(IA2,IA1,6) + AMP*AMP*W
                     END IF
                  END IF
               END IF
            IPTR = IPTR + 3 * NCOR
 120        CONTINUE
         GO TO 100
C                                       Scan done
C                                       See if have any data.
 500  IF ((.NOT.GOTDAT) .AND. (IERR.EQ.0)) GO TO 10
      IF (.NOT.GOTDAT) GO TO 800
      NANT = NNANT
C                                       Data for amplitude in AUTO
      IF (DTYPE.EQ.0) THEN
         DO 525 I = 1,NANT
            IF (COUNTA(I,1).GT.0.0) SCANVA(I,1) = WORKA(I,1)/COUNTA(I,1)
            IF (COUNTA(I,2).GT.0.0) SCANVA(I,2) = WORKA(I,2)/COUNTA(I,2)
  525       CONTINUE
         END IF
      CALL RFILL (4, 0.0, SR)
      CALL RFILL (4, 0.0, SI)
      CALL RFILL (4, 0.0, SW)
      CALL RFILL (4, 0.0, SSR)
      CALL RFILL (4, 0.0, SSI)
C                                       Vector averaging
      LIMIT = NANT - 1
      DO 560 I = 1,LIMIT
         LIMIT1 = I + 1
         DO 550 J = LIMIT1,NANT
C                                       Weight
            IF ((DDTYPE.EQ.-1) .AND. (COUNT(I,J,1).GT.0.0D0)) THEN
               SCANV(I,J,1) = COUNT(I,J,1)
               SR(1) = SR(1) + SCANV(I,J,1)
               SSR(1) = SSR(1) + COUNT(I,J,3)
               SW(1) = SW(1) + 1
               END IF
            IF ((DDTYPE.EQ.-1) .AND. (COUNT(I,J,2).GT.0.0D0)) THEN
               SCANV(J,I,1) = COUNT(I,J,2)
               SR(2) = SR(2) + COUNT(I,J,2)
               SSR(2) = SSR(2) + COUNT(I,J,4)
               SW(2) = SW(2) + 1
               END IF
C                                       Amplitude
            IF ((DDTYPE.EQ.0) .AND. (COUNT(I,J,1).GT.0.0D0)) THEN
               IF (AMPVEC) THEN
                  SCANV(I,J,1) = WORK(I,J,5) / COUNT(I,J,1)
                  SR(1) = SR(1) + WORK(I,J,5)
                  SSR(1) = SSR(1) + WORK(J,I,5)
                  SW(1) = SW(1) + COUNT(I,J,1)
               ELSE
                  SCANV(I,J,1) = SQRT (WORK(I,J,1)*WORK(I,J,1) +
     *               WORK(J,I,1)*WORK(J,I,1)) / COUNT(I,J,1)
                  SR(1) = SR(1) + WORK(I,J,1)
                  SI(1) = SI(1) + WORK(J,I,1)
                  SSR(1) = SSR(1) + WORK(I,J,2)
                  SSI(1) = SSI(1) + WORK(J,I,2)
                  SW(1) = SW(1) + COUNT(I,J,1)
                  END IF
               END IF
            IF ((DDTYPE.EQ.0) .AND. (COUNT(I,J,2).GT.0.0D0)) THEN
               IF (AMPVEC) THEN
                  SCANV(J,I,1) = WORK(I,J,6) / COUNT(I,J,2)
                  SR(2) = SR(2) + WORK(I,J,6)
                  SSR(2) = SSR(2) + WORK(J,I,6)
                  SW(2) = SW(2) + COUNT(I,J,2)
               ELSE
                  SCANV(J,I,1) = SQRT (WORK(I,J,3)*WORK(I,J,3) +
     *               WORK(J,I,3)*WORK(J,I,3)) / COUNT(I,J,2)
                  SR(2) = SR(2) + WORK(I,J,3)
                  SI(2) = SI(2) + WORK(J,I,3)
                  SSR(2) = SSR(2) + WORK(I,J,4)
                  SSI(2) = SSI(2) + WORK(J,I,4)
                  SW(2) = SW(2) + COUNT(I,J,2)
                  END IF
               END IF
C                                       Phase (first)
            IF ((DDTYPE.EQ.1) .AND. (COUNT(I,J,1).GT.0.0D0)) THEN
               SCANV(I,J,1) = ATAN2 (WORK(J,I,1), WORK(I,J,1)+1.0D-20)
               SR(1) = SR(1) + WORK(I,J,1)
               SI(1) = SI(1) + WORK(J,I,1)
               SSR(1) = SSR(1) + WORK(I,J,2)
               SSI(1) = SSI(1) + WORK(J,I,2)
               SW(1) = SW(1) + COUNT(I,J,1)
               END IF
            IF ((DDTYPE.EQ.1) .AND. (COUNT(I,J,2).GT.0.0D0)) THEN
               SCANV(J,I,1) = PFACT *
     *            ATAN2 (WORK(J,I,3), WORK(I,J,3)+1.0D-20)
               SR(2) = SR(2) + WORK(I,J,3)
               SI(2) = SI(2) + WORK(J,I,3)
               SSR(2) = SSR(2) + WORK(I,J,4)
               SSI(2) = SSI(2) + WORK(J,I,4)
               SW(2) = SW(2) + COUNT(I,J,2)
               END IF
C                                       Phase (second)
            IF (DOFAZ2 .AND. (COUNT(I,J,1).GT.0.0D0)) THEN
               SCANV(I,J,2) = ATAN2 (WORK(J,I,1), WORK(I,J,1)+1.0D-20)
               SR(3) = SR(3) + WORK(I,J,1)
               SI(3) = SI(3) + WORK(J,I,1)
               SSR(3) = SSR(3) + WORK(I,J,2)
               SSI(3) = SSI(3) + WORK(J,I,2)
               SW(3) = SW(3) + COUNT(I,J,1)
               END IF
            IF (DOFAZ2 .AND. (COUNT(I,J,2).GT.0.0D0)) THEN
               SCANV(J,I,2) = PFACT *
     *            ATAN2 (WORK(J,I,3), WORK(I,J,3)+1.0D-20)
               SR(4) = SR(4) + WORK(I,J,3)
               SI(4) = SI(4) + WORK(J,I,3)
               SSR(4) = SSR(4) + WORK(I,J,4)
               SSI(4) = SSI(4) + WORK(J,I,4)
               SW(4) = SW(4) + COUNT(I,J,2)
               END IF
C                                       RMS (first)
            IF ((DDTYPE.EQ.2) .AND. (COUNT(I,J,1).GT.0.0D0)) THEN
               IF (AMPVEC) THEN
                  TEMP = (COUNT(I,J,1)*WORK(J,I,5) - (WORK(I,J,5)**2))
               ELSE
                  TEMP1 = (COUNT(I,J,1)*WORK(I,J,2) - (WORK(I,J,1)**2))
                  TEMP2 = (COUNT(I,J,1)*WORK(J,I,2) - (WORK(J,I,1)**2))
                  TEMP = (WORK(I,J,1)**2)*TEMP1 + (WORK(J,I,1)**2)*TEMP2
                  TEMP = TEMP / ((WORK(I,J,1)**2) + (WORK(J,I,1)**2))
                  END IF
               IF (TEMP.LT.0.0D0) TEMP = 0.0D0
               SCANV(I,J,1) = SQRT (TEMP) / COUNT(I,J,1)
               SR(1) = SR(1) + SCANV(I,J,1)
               SSR(1) = SSR(1) + SCANV(I,J,1)*SCANV(I,J,1)
               SW(1) = SW(1) + 1
               END IF
            IF ((DDTYPE.EQ.2) .AND. (COUNT(I,J,2).GT.0.0D0)) THEN
               IF (AMPVEC) THEN
                  TEMP = (COUNT(I,J,2)*WORK(J,I,6) - (WORK(I,J,6)**2))
               ELSE
                  TEMP1 = (COUNT(I,J,2)*WORK(I,J,4) - (WORK(I,J,3)**2))
                  TEMP2 = (COUNT(I,J,2)*WORK(J,I,4) - (WORK(J,I,3)**2))
                  TEMP = (WORK(I,J,3)**2)*TEMP1 + (WORK(J,I,3)**2)*TEMP2
                  TEMP = TEMP / ((WORK(I,J,3)**2) + (WORK(J,I,3)**2))
                  END IF
               IF (TEMP.LT.0.0D0) TEMP = 0.0D0
               SCANV(J,I,1) = SQRT (TEMP) / COUNT(I,J,2)
               SR(2) = SR(2) + SCANV(J,I,1)
               SSR(2) = SSR(2) + SCANV(J,I,1)*SCANV(J,I,1)
               SW(2) = SW(2) + 1
               END IF
C                                       RMS (second matrix)
            IF ((DORMS2) .AND. (COUNT(I,J,1).GT.0.0D0)) THEN
               IF (DTYPE.EQ.4) THEN
                  TEMP1 = (COUNT(I,J,1)*WORK(I,J,2) - (WORK(I,J,1)**2))
                  TEMP2 = (COUNT(I,J,1)*WORK(J,I,2) - (WORK(J,I,1)**2))
                  TEMP = (WORK(J,I,1)**2)*TEMP1 + (WORK(I,J,1)**2)*TEMP2
                  TEMP = TEMP / ((WORK(I,J,1)**2)+(WORK(J,I,1)**2))**2
                  TEMP = TEMP * COUNT(I,J,1)* COUNT(I,J,1)
               ELSE IF (AMPVEC) THEN
                  TEMP = (COUNT(I,J,1)*WORK(J,I,5) - (WORK(I,J,5)**2))
               ELSE
                  TEMP1 = (COUNT(I,J,1)*WORK(I,J,2) - (WORK(I,J,1)**2))
                  TEMP2 = (COUNT(I,J,1)*WORK(J,I,2) - (WORK(J,I,1)**2))
                  TEMP = (WORK(I,J,1)**2)*TEMP1 + (WORK(J,I,1)**2)*TEMP2
                  TEMP = TEMP / ((WORK(I,J,1)**2)+(WORK(J,I,1)**2))
                  END IF
               IF (TEMP.LT.0.0D0) TEMP = 0.0D0
               SCANV(I,J,2) = SQRT (TEMP) / COUNT(I,J,1)
               SR(3) = SR(3) + SCANV(I,J,2)
               SSR(3) = SSR(3) + SCANV(I,J,2)*SCANV(I,J,2)
               SW(3) = SW(3) + 1
               END IF
            IF ((DORMS2) .AND. (COUNT(I,J,2).GT.0.0D0)) THEN
               IF (DTYPE.EQ.4) THEN
                  TEMP1 = (COUNT(I,J,2)*WORK(I,J,4) - (WORK(I,J,3)**2))
                  TEMP2 = (COUNT(I,J,2)*WORK(J,I,4) - (WORK(J,I,3)**2))
                  TEMP = (WORK(J,I,3)**2)*TEMP1 + (WORK(I,J,3)**2)*TEMP2
                  TEMP = TEMP / ((WORK(I,J,3)**2) + (WORK(J,I,3)**2))**2
                  TEMP = TEMP * COUNT(I,J,2)* COUNT(I,J,2)
               ELSE IF (AMPVEC) THEN
                  TEMP = (COUNT(I,J,2)*WORK(J,I,6) - (WORK(I,J,6)**2))
               ELSE
                  TEMP1 = (COUNT(I,J,2)*WORK(I,J,4) - (WORK(I,J,3)**2))
                  TEMP2 = (COUNT(I,J,2)*WORK(J,I,4) - (WORK(J,I,3)**2))
                  TEMP = (WORK(I,J,3)**2)*TEMP1 + (WORK(J,I,3)**2)*TEMP2
                  TEMP = TEMP / ((WORK(I,J,3)**2) + (WORK(J,I,3)**2))
                  END IF
               IF (TEMP.LT.0.0D0) TEMP = 0.0D0
               SCANV(J,I,2) = SQRT (TEMP) / COUNT(I,J,2)
               SR(4) = SR(4) + SCANV(J,I,2)
               SSR(4) = SSR(4) + SCANV(J,I,2)*SCANV(J,I,2)
               SW(4) = SW(4) + 1
               END IF
 550        CONTINUE
 560     CONTINUE
C                                       average matrix averages
      CALL RFILL (8, 0.0, AVGS)
      DO 570 I = 1,4
         IF (SW(I).GT.0.0D0) THEN
            SR(I) = SR(I) / SW(I)
            SSR(I) = SSR(I) / SW(I)
            SI(I) = SI(I) / SW(I)
            SSI(I) = SSI(I) / SW(I)
            END IF
 570     CONTINUE
C                                       convert to output form
      DO 580 I = 1,2
         IF (SW(I).GT.0.0D0) THEN
C                                       weight
            IF (DDTYPE.EQ.-1) THEN
               AVGS(1,I,1) = SR(I)
               SSR(I) = SSR(I) - SR(I)*SR(I)
               IF (SSR(I).GT.0.0D0) AVGS(2,I,1) = SQRT (SSR(I))
C                                       amplitude
            ELSE IF (DDTYPE.EQ.0) THEN
               AVGS(1,I,1) = SQRT (SR(I)*SR(I) + SI(I)*SI(I))
               IF (AVGS(1,I,1).GT.0.0D0) THEN
                  SSR(I) = SSR(I) - SR(I)*SR(I)
                  SSI(I) = SSI(I) - SI(I)*SI(I)
                  TEMP = (SR(I)*SR(I)*SSR(I) + SI(I)*SI(I)*SSI(I)) /
     *               (AVGS(1,I,1) * AVGS(1,I,1))
                  IF (TEMP.GT.0.0D0) AVGS(2,I,1) = SQRT (TEMP)
                  END IF
C                                       phase (first)
            ELSE IF (DDTYPE.EQ.1) THEN
               AVGS(1,I,1) = ATAN2 (SI(I), SR(I)+1.E-20) * RAD2DG
               IF (I.EQ.2) AVGS(1,I,1) = PFACT * AVGS(1,I,1)
               SSR(I) = SSR(I) - SR(I)*SR(I)
               SSI(I) = SSI(I) - SI(I)*SI(I)
               TEMP = (SR(I)*SR(I)+SI(I)*SI(I)) ** 2
               IF (TEMP.GT.0.0D0) THEN
                  TEMP = (SR(I)*SR(I)*SSI(I) + SI(I)*SI(I)*SSR(I)) /
     *               TEMP
                  IF (TEMP.GT.0.0D0) AVGS(2,I,1) = SQRT (TEMP)
     *               * RAD2DG
               ELSE
                  AVGS(2,I,1) = 999.0
                  END IF
C                                       RMS (first)
            ELSE IF (DDTYPE.EQ.2) THEN
               AVGS(1,I,1) = SR(I)
               TEMP = SSR(I) - SR(I)*SR(I)
               IF (TEMP.GT.0.0D0) AVGS(2,I,1) = SQRT (TEMP)
               END IF
C                                       phase (second)
            IF ((DOFAZ2) .AND. (SW(2+I).GT.0.0D0)) THEN
               AVGS(1,I,2) = ATAN2 (SI(2+I), SR(2+I)+1.E-20)
     *            * RAD2DG
               IF (I.EQ.2) AVGS(1,I,2) = PFACT * AVGS(1,I,2)
               SSR(2+I) = SSR(2+I) - SR(2+I)*SR(2+I)
               SSI(2+I) = SSI(2+I) - SI(2+I)*SI(2+I)
               TEMP = (SR(2+I)*SR(2+I)+SI(2+I)*SI(2+I)) ** 2
               IF (TEMP.GT.0.0D0) THEN
                  TEMP = (SR(2+I)*SR(2+I)*SSI(2+I) +
     *               SI(2+I)*SI(2+I)*SSR(2+I)) / TEMP
                  IF (TEMP.GT.0.0D0) AVGS(2,I,2) = SQRT (TEMP) *
     *               RAD2DG
               ELSE
                  AVGS(2,I,2) = 999.0
                  END IF
               END IF
C                                       RMS (second)
            IF ((DORMS2) .AND. (SW(2+I).GT.0.0D0)) THEN
               AVGS(1,I,2) = SR(2+I)
               TEMP = SSR(2+I) - SR(2+I)*SR(2+I)
               IF (TEMP.GT.0.0D0) AVGS(2,I,2) = SQRT (TEMP)
               IF (DTYPE.EQ.4) THEN
                  AVGS(1,I,2) = AVGS(1,I,2) * RAD2DG
                  AVGS(2,I,2) = AVGS(2,I,2) * RAD2DG
                  END IF
               END IF
            END IF
 580     CONTINUE
C                                       Get source info
      CALL GETSOU (SUNUM, IUDISK, IUCNO, CATUV, ISLUN, JERR)
C                                       Didn't find source
      IF (JERR.EQ.11) THEN
         WRITE (MSGTXT,1750) SUNUM
         CALL MSGWRT (8)
         JERR = 0
      ELSE IF (JERR.GT.0) THEN
         IERR = JERR
         WRITE (MSGTXT,1700) JERR
         GO TO 990
         END IF
C                                       Time to Days Hours Mins Secs
 800  IF (T1.LT.1000.) THEN
         IF (DP9.GT.0.0) THEN
            CALL T2LST (T1)
            CALL T2LST (T2)
            END IF
         CALL TODHMS (T1, TIME(1))
         CALL TODHMS (T2, TIME(5))
         END IF
C                                       If end of data, close UVGET
      IF (IERR.LT.0) CALL UVGET ('CLOS', RPARM, VIS, JERR)
      IF (JERR.NE.0) IERR = JERR
      IF ((IERR.LT.0) .AND. (.NOT.GOTDAT)) THEN
         IERR = 1
         MSGTXT = 'NO DATA FOUND'
         CALL MSGWRT (8)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1700 FORMAT ('SCANAV: ERROR',I3,' READING SOURCE TABLE')
 1750 FORMAT ('SCANAV: SOURCE ',I3,' NOT IN SU TABLE')
      END
      SUBROUTINE LISTCH (IRET)
C-----------------------------------------------------------------------
C   Counts lines for column listing of uv data (multi or single source)
C   Info for UVGET is set in LSTRIN. (except ANTENS)
C   Output:
C      IRET   I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER CCODE*1
      INTEGER   TIME(8), DTYPE, NPASS, IPASS, NCOLPV, I, IATY, NUMBAS,
     *   NUMPRT, IOFF, IIPOL, SCANUM, LCOUNT, CATSAV(256)
      LOGICAL   F, AMPVEC, DONE, NUSCAN
      REAL      CATR(256), SMAX, SFACT, SFACTI, DT, SFACOL, PSFACT,
     *   SFACTA
      HOLLERITH CATH(256)
      DOUBLE PRECISION    CATD(128)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANTLAB(2,MXBASE)
      REAL      SCANV(MXBASE)
      INCLUDE 'LISTR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
C                                       Setup
      CALL H2CHR (8, 1, CATH(KHOBJ), SNAME)
      PSFACT = -12345.0
      QUAL = 0
      CALCOD = ' '
      AMPVEC = DPARM(2).GT.0.5
      IATY = 2
      IF (AMPVEC) IATY = 1
      DTYPE = DPARM(1) + 0.5
      IF ((DTYPE.LT.0) .OR. (DTYPE.GT.2)) THEN
         IF (DTYPE.EQ.4) THEN
            DTYPE = 1
         ELSE
            DTYPE = 0
            END IF
         END IF
C                                       Number of char per col.
      NCOLPV = 4
      IF ((DPARM(3).GT.3.6) .AND. (DPARM(3).LT.10.4))
     *   NCOLPV = DPARM(3) + 0.5
C                                       Integration time
      DT = DPARM(4) / 1440.
C                                       Init. vis record
      DO 10 I = 1,20
         RPARM(I) = 0.0
         VIS(1,I) = 0.0
         VIS(2,I) = 0.0
         VIS(3,I) = 0.0
 10      CONTINUE
C                                       Setup for col. listing.
      CALL COLSET (NUMBAS, ANTLAB, IIPOL, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL COPY (256, CATBLK, CATSAV)
C                                       Determine number to print
      NUMPRT = (NACROS-15) / NCOLPV
C                                       if more will fit than needed
      IF (NUMPRT.GT.NUMBAS) THEN
         NUMPRT = NUMBAS
C                                       spread print a little
         NCOLPV = MAX( NCOLPV, MIN( 6, ((NACROS-15)/NUMPRT)))
         END IF
      NPASS = ((NUMBAS*1.0) / NUMPRT) + 0.999
C                                       Start outer loop
      FLUX(1,BIF) = 0.0
      FLUX(2,BIF) = 0.0
      FLUX(3,BIF) = 0.0
      FLUX(4,BIF) = 0.0
      DO 600 IPASS = 1,NPASS
         NUMPRT = (NACROS-15) / NCOLPV
         IOFF = (IPASS-1) * NUMPRT
         IF ((IOFF+NUMPRT).GT.NUMBAS) NUMPRT = NUMBAS - IOFF
C                                       Open uv data etc.
         RPARM(1) = FBLANK
         SCANUM = -10
         CALL UVGET ('INIT', RPARM, VIS, IRET)
C                                       Don't apply SN tables again.
         DOAPPL = F
         CLVER = CLUSE
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       first page titles
         IF ((IPASS.EQ.1) .OR. (DOCRT.GT.-2.5)) NCOUNT = NCOUNT + 1
         IF (DOCRT.GT.-2.5) THEN
            NCOUNT = NCOUNT + 2
            IF (DOCAL) NCOUNT = NCOUNT + 1
            IF (DOBL) NCOUNT = NCOUNT + 1
            IF (DOFLAG) NCOUNT = NCOUNT + 1
            IF (DOBAND.GT.0) NCOUNT = NCOUNT + 1
            NCOUNT = NCOUNT + 1
            END IF
C                                       While more data, Loop thru
 100     CONTINUE
            CALL LISTAV (NUMPRT, ANTLAB(1,IOFF+1), AMPVEC, DTYPE, DT,
     *         SCANV, DPARM(9), TIME, NUSCAN, SCANUM, RPARM, VIS, IRET)
            DONE = IRET.LT.0
C                                       See if any data
            IF (IRET.EQ.-2) GO TO 600
            IF (IRET.GT.0) GO TO 999
C                                       If calc code OK
            CCODE = CALCOD(:1)
            IF ((XCALCO(:1).EQ.' ') .OR. (CCODE.EQ.XCALCO(:1)) .OR.
     *         ((XCALCO(:1).EQ.'*') .AND. (CCODE.NE.' ')) .OR.
     *         ((XCALCO(:1).EQ.'-') .AND. (CCODE.EQ.' '))) THEN
               IF (NUSCAN) SFACOL = 1.0E10
               IF (NUSCAN) LCOUNT = -1
C                                       Find max value
               CALL LSTMAX (SCANV, ANTLAB(1,IOFF+1), NUMPRT, DTYPE,
     *            NCOLPV, SMAX, SFACT)
               IF (DTYPE.EQ.1) THEN
                  IF (DPARM(10).GT.0.0) THEN
                     SFACT =  RAD2DG / DPARM(10)
                  ELSE IF (DPARM(7).LE.0.0) THEN
                     SFACT =  RAD2DG
                     END IF
               ELSE IF (DPARM(10).GT.0.0) THEN
                  SFACT = 1.0 / DPARM(10)
                  END IF
C                                       If scale goes up by 10,
C                                       ignore, use previous scale
               IF ((SFACT/SFACOL.LE.10.01) .AND.
     *            (SFACT/SFACOL.GT.0.99)) SFACT = SFACOL
               SFACTI = 1000.0 / SFACT
               SFACTA = SFACTI
               IF (DTYPE.EQ.0) SFACTA = 100.0 * SFACTA
C                                       Header for scan
               IF (NUSCAN) THEN
                  IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
                  NCOUNT = NCOUNT + 1
C                                       Source info
                  IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
                  END IF
C                                       Type of data listed
               IF ((NUSCAN) .OR. (SFACT.NE.SFACOL)) THEN
                  IF ((PSFACT.NE.SFACTI) .OR. (DOCRT.GT.-2.5)) THEN
                     NCOUNT = NCOUNT + 1
                     IF ((DOACOR) .AND. (DTYPE.EQ.0)) NCOUNT = NCOUNT+1
                     END IF
                  PSFACT = SFACTI
                  END IF
C                                       Section label
               IF (NUSCAN) THEN
                  IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
                  IF (((IPCNT.GT.3) .AND. (IPCNT.LT.(PRTMAX-1))) .OR.
     *               (DOCRT.LE.-2.5)) NCOUNT = NCOUNT + 1
                  END IF
C                                       Scale Data
               LCOUNT = LCOUNT + 1
               IF (MOD(LCOUNT,IXINC).EQ.0) NCOUNT = NCOUNT + 1
C                                       End if Cal Code OK
               END IF
C                                       End while more data - loop
            IF (.NOT.DONE) GO TO 100
C                                       Force new page
 600     CONTINUE
      IRET = 0
      CALL COPY (256, CATSAV, CATBLK)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('LISTCH: ERROR ',I3,' OPENING UV DATA FILE')
      END
      SUBROUTINE LISTUV (IRET)
C-----------------------------------------------------------------------
C   Gives column listing of uv data (multi or single source)
C   Info for UVGET is set in LSTRIN. (except ANTENS)
C   Output:
C      IRET   I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      CHARACTER ENTRY*20, ATYPE(2)*12, CCODE*1
      INTEGER   TIME(8), DTYPE, NPASS, IPASS, ITEMP, ICOL, NCOLPV, I,
     *   COLPNT, IATY, IRET, IERR, NUMBAS, NUMPRT, IOFF, IIPOL, I4TEMP,
     *   SCANUM, KQUAL, LCOUNT
      LOGICAL   F, AMPVEC, DONE, NUSCAN
      REAL      CATR(256), SMAX, SFACT, SFACTI, DT, SFACOL, XROUND,
     *   PSFACT, SFACTA
      HOLLERITH CATH(256)
      DOUBLE PRECISION    CATD(128), SFREQ
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANTLAB(2,MXBASE)
      REAL      SCANV(MXBASE)
      INCLUDE 'LISTR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA ATYPE /'Ampscalar   ', 'Vector      '/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Setup
      CALL H2CHR (8, 1, CATH(KHOBJ), SNAME)
      PSFACT = -12345.0
      QUAL = 0
      CALCOD = ' '
      AMPVEC = DPARM(2).GT.0.5
      IATY = 2
      IF (AMPVEC) IATY = 1
      DTYPE = DPARM(1) + 0.5
      IF ((DTYPE.LT.0) .OR. (DTYPE.GT.2)) THEN
         I = DTYPE
         IF (DTYPE.EQ.4) THEN
            DTYPE = 1
         ELSE
            DTYPE = 0
            END IF
         WRITE (MSGTXT,1000) I, DTYPE
         CALL MSGWRT (6)
         END IF
C                                       Number of char per col.
      NCOLPV = 4
      IF ((DPARM(3).GT.3.6) .AND. (DPARM(3).LT.10.4))
     *   NCOLPV = DPARM(3) + 0.5
C                                       Integration time
      DT = DPARM(4) / 1440.
C                                       Init. vis record
      DO 10 I = 1,20
         RPARM(I) = 0.0
         VIS(1,I) = 0.0
         VIS(2,I) = 0.0
         VIS(3,I) = 0.0
 10      CONTINUE
C                                       Setup for col. listing.
      CALL COLSET (NUMBAS, ANTLAB, IIPOL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Determine number to print
      NUMPRT = (NACROS-15) / NCOLPV
C                                       if more will fit than needed
      IF (NUMPRT.GT.NUMBAS) THEN
         NUMPRT = NUMBAS
C                                       spread print a little
         NCOLPV = MAX( NCOLPV, MIN( 6, ((NACROS-15)/NUMPRT)))
         END IF
      NPASS = ((NUMBAS*1.0) / NUMPRT) + 0.999
C                                       Start outer loop
      FLUX(1,BIF) = 0.0
      FLUX(2,BIF) = 0.0
      FLUX(3,BIF) = 0.0
      FLUX(4,BIF) = 0.0
      DO 600 IPASS = 1,NPASS
         NUMPRT = (NACROS-15) / NCOLPV
         IOFF = (IPASS-1) * NUMPRT
         IF ((IOFF+NUMPRT).GT.NUMBAS) NUMPRT = NUMBAS - IOFF
C                                       Open uv data etc.
         RPARM(1) = FBLANK
         SCANUM = -10
         CALL UVGET ('INIT', RPARM, VIS, IRET)
C                                       Don't apply SN tables again.
         DOAPPL = F
         CLVER = CLUSE
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       first page titles
         IF (NACROS.GE.90) THEN
            WRITE (LINE,1020) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER,
     *         BCHAN, ECHAN, BIF
         ELSE
            WRITE (LINE,1021) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER,
     *        BCHAN, ECHAN, BIF
            END IF
         IF ((IPASS.EQ.1) .OR. (DOCRT.GT.-2.5)) THEN
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
         IF (DOCRT.GT.-2.5) THEN
            FREQ = BFREQ * 1.0D-9
            WRITE (LINE,1022) FREQ, NCOR, NVIS
            FREQ = BFREQ
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            WRITE (LINE,1023) STOKES, SUBARR
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            IF (DOCAL) THEN
               WRITE (LINE,1030) CLUSE
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
               END IF
            IF (DOBL) THEN
               WRITE (LINE,1031) BLVER
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
               END IF
            IF (DOFLAG) THEN
               WRITE (LINE,1032) FGVER
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
               END IF
            IF (DOBAND.GT.0) THEN
               WRITE (LINE,1033) BPVER
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
               END IF
            LINE = ' '
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
C                                       While more data, Loop thru
 100     CONTINUE
            CALL LISTAV (NUMPRT, ANTLAB(1,IOFF+1), AMPVEC, DTYPE, DT,
     *         SCANV, DPARM(9), TIME, NUSCAN, SCANUM, RPARM, VIS, IRET)
            DONE = IRET.LT.0
C                                       See if any data
            IF (IRET.EQ.-2) GO TO 600
            IF (IRET.GT.0) GO TO 999
C                                       If calc code OK
            CCODE = CALCOD(:1)
            IF ((XCALCO(:1).EQ.' ') .OR. (CCODE.EQ.XCALCO(:1)) .OR.
     *         ((XCALCO(:1).EQ.'*') .AND. (CCODE.NE.' ')) .OR.
     *         ((XCALCO(:1).EQ.'-') .AND. (CCODE.EQ.' '))) THEN
               IF (NUSCAN) SFACOL = 1.0E10
               IF (NUSCAN) LCOUNT = -1
C                                       Find max value
               CALL LSTMAX (SCANV, ANTLAB(1,IOFF+1), NUMPRT, DTYPE,
     *            NCOLPV, SMAX, SFACT)
               IF (DTYPE.EQ.1) THEN
                  IF (DPARM(10).GT.0.0) THEN
                     SFACT =  RAD2DG / DPARM(10)
                  ELSE IF (DPARM(7).LE.0.0) THEN
                     SFACT =  RAD2DG
                     END IF
               ELSE IF (DPARM(10).GT.0.0) THEN
                  SFACT = 1.0 / DPARM(10)
                  END IF
C                                       If scale goes up by 10,
C                                       ignore, use previous scale
               IF ((SFACT/SFACOL.LE.10.01) .AND.
     *            (SFACT/SFACOL.GT.0.99)) SFACT = SFACOL
               SFACTI = 1000.0 / SFACT
               SFACTA = SFACTI
               IF (DTYPE.EQ.0) SFACTA = 100.0 * SFACTA
C                                       Header for scan
               IF (NUSCAN) THEN
                  IF (DOCRT.GT.-2.5) THEN
                     LINE = ' '
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  KQUAL = QUAL
                  WRITE (TITL1,1100) SNAME, KQUAL, STOKES, BIF,
     *               BCHAN, ECHAN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, TITL1, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
C                                       Source info
                  IF (DOCRT.GT.-2.5) THEN
                     SFREQ = (BFREQ + FREQO(BIF) + FREQIF(BIF)) * 1.0D-9
                     WRITE (LINE,1101) FLUX(IIPOL,BIF), CALCOD, SFREQ
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  END IF
C                                       Type of data listed
               IF ((NUSCAN) .OR. (SFACT.NE.SFACOL)) THEN
                  IF (DTYPE.EQ.0) WRITE (LINE,1110) SFACTI, ATYPE(IATY)
                  IF (DTYPE.EQ.1) WRITE (LINE,1111) SFACTI*RAD2DG,
     *               ATYPE(2)
                  IF (DTYPE.EQ.2) WRITE (LINE,1112) SFACTI, ATYPE(IATY)
                  IF ((PSFACT.NE.SFACTI) .OR. (DOCRT.GT.-2.5)) THEN
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     IF ((DOACOR) .AND. (DTYPE.EQ.0)) THEN
                        WRITE (LINE,1117) SFACTA
                        CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                     TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                        IF (IERR.NE.0) GO TO 950
                        END IF
                     END IF
                  PSFACT = SFACTI
                  END IF
C                                       Section label
               IF (NUSCAN) THEN
                  IF (DOCRT.GT.-2.5) THEN
                     LINE = ' '
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  TITL2 = 'Time/Baseline'
                  IF (DPARM(9).GT.0.0) TITL2 = 'LST/Baselines'
                  COLPNT = 15
                  DO 150 ICOL = 1,NUMPRT
                     IF (NCOLPV.LE.5) THEN
                        WRITE (ENTRY,1115) ANTLAB(1,IOFF+ICOL),
     *                     ANTLAB(2,IOFF+ICOL)
                     ELSE
                        WRITE (ENTRY,1116) ANTLAB(1,IOFF+ICOL),
     *                     ANTLAB(2,IOFF+ICOL)
                        END IF
                     TITL2(COLPNT:COLPNT+NCOLPV-1) = ENTRY(11-NCOLPV:10)
                     COLPNT = COLPNT + NCOLPV
 150                 CONTINUE
                  IF (((IPCNT.GT.3) .AND. (IPCNT.LT.(PRTMAX-1))) .OR.
     *               (DOCRT.LE.-2.5)) THEN
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, TITL2, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  END IF
C                                       Scale Data
               LCOUNT = LCOUNT + 1
               IF (MOD(LCOUNT,IXINC).EQ.0) THEN
                  SFACOL = SFACT
                  WRITE (LINE,1160) (TIME(ICOL), ICOL = 1,4)
                  COLPNT = 15
                  ITEMP = 10 - NCOLPV + 1
                  DO 200 ICOL = 1,NUMPRT
                     IF (SCANV(ICOL).NE.FBLANK) THEN
                        XROUND = 0.5
                        IF (SCANV(ICOL).LT.0.0) XROUND = -0.5
                        IF ((ANTLAB(1,ICOL+IOFF).EQ.ANTLAB(2,ICOL+IOFF))
     *                     .AND. (DTYPE.EQ.0)) THEN
                           I4TEMP = SFACT * SCANV(ICOL) / 100.0 + XROUND
                        ELSE
                           I4TEMP = SFACT * SCANV(ICOL) + XROUND
                           END IF
                        WRITE (ENTRY,1161) I4TEMP
                     ELSE
                        ENTRY = ' '
                        END IF
                     LINE(COLPNT:COLPNT+NCOLPV-1) = ENTRY(ITEMP:10)
                     COLPNT = COLPNT + NCOLPV
 200                 CONTINUE
C                                       Write row
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
                  END IF
C                                       End if Cal Code OK
               END IF
C                                       End while more data - loop
            IF (.NOT.DONE) GO TO 100
C                                       Force new page
         LINE = ' '
         TITL1 = ' '
         TITL2 = ' '
         IPCNT = 998
 600     CONTINUE
      IRET = 0
      GO TO 999
C                                       CRT error
 950  IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1950) IERR
         CALL MSGWRT (8)
         IRET = 1
      ELSE
         IRET = -1
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WARNING: REQUESTED DISPLAY TYPE',I3,' CHANGED TO',I2)
 1010 FORMAT ('LISTUV: ERROR ',I3,' OPENING UV DATA FILE')
 1020 FORMAT ('File = ',A12,'.',A6,'.',I4,' Vol =',I2,'  Userid =',I5,
     *   3X,'Channels =',I3,'-',I4,' IF =',I2)
 1021 FORMAT (A12,'.',A6,'.',I4,' Vol=',I2,' User=',I5,' Chans=',I3,'-',
     *   I4,' IF =',I2)
 1022 FORMAT ('Freq=',F13.9,' GHz   Ncor=',I3,'   No. vis=',I10)
 1023 FORMAT ('Stokes = ',A4,' Subarray = ',I3)
 1030 FORMAT ('Applying calibration table ',I3)
 1031 FORMAT ('Applying baseline table ',I3)
 1032 FORMAT ('Applying flag table ',I3)
 1033 FORMAT ('Applying bandpass table ',I3)
 1100 FORMAT ('Source=',A16,':',I5.4,', Stokes=',A4,', IF=',I3,
     *   ', Chans=',I4,'-',I4)
 1101 FORMAT ('Flux =',F8.4,' Jy, Calcode = ',A4,', Freq =',F13.9,
     *   ' GHz')
 1110 FORMAT ('Amplitudes, 1000 =',F11.6,' Jy, averging type = ',A12)
 1111 FORMAT ('Phase, 1000 =',F11.5,' degrees, averging type = ',A12)
 1112 FORMAT ('Rms, 1000 =',F11.6,' Jy, averging type = ',A12)
 1117 FORMAT ('Amplitudes of AUTO, 1000 =',F13.6,' Jy')
 1160 FORMAT (I4,'/',2(I2.2,':'),I2.2)
 1161 FORMAT (I10)
 1115 FORMAT (6X,2I2.2)
 1116 FORMAT (5X,I2.2,'-',I2.2)
 1950 FORMAT ('LISTUV: ERROR',I5,' DOING I/O TO TERMINAL')
      END
      SUBROUTINE LSTMAX (SCANV, ANTLAB, NUMPRT, DTYPE, NCOLPV, SMAX,
     *   SFACT)
C-----------------------------------------------------------------------
C   Routine to find the maximum, non blank value in an array and
C   determine the proper scaling factor for printing.
C   Inputs:
C      SCANV   R(mxbase)   Scan values.
C      NANT    I           Max. antenns number in scan.
C      DTYPE   I           Data type, 0=amp, 1=phase, 2=RMS
C      NCOLPV  I           Number of columns per value printed
C   Output:
C      SMAX    R           Maximum abs value.
C      SFACT   R           Scaling factor to print values.
C-----------------------------------------------------------------------
      INTEGER   NUMPRT, DTYPE, NCOLPV, I, ANTLAB(2,*)
      REAL      SMAX, SFACT, VALUE
      INCLUDE 'INCS:PUVD.INC'
      REAL      SCANV(MXBASE)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Find maximum non blank
      SMAX = 0.0
      SFACT = 1.0
      IF (NUMPRT.GT.0) THEN
         DO 100 I = 1,NUMPRT
            IF (SCANV(I).NE.FBLANK) THEN
               VALUE = ABS (SCANV(I))
               IF (ANTLAB(1,I).EQ.ANTLAB(2,I)) VALUE = VALUE / 100.0
               SMAX = MAX (SMAX, VALUE)
               END IF
 100        CONTINUE
         END IF
C                                       Determine scaling
      IF (SMAX.GT.1.0E-20) THEN
C                                       Scale for phase to degrees
         IF (DTYPE.EQ.1) SMAX = SMAX * RAD2DG
         VALUE = ALOG (SMAX) / ALOG (10.0)
         I = - VALUE
         IF (VALUE.GT.0.0) I = I - 1
         SFACT = 10.0 ** (I + (NCOLPV-1))
         IF (DTYPE.EQ.1) SFACT = RAD2DG * SFACT
         END IF
C
 999  RETURN
      END
      SUBROUTINE LISTAV (NUMBAS, ANTLAB, AMPVEC, DTYPE, DT, SCANV, DP9,
     *   TIME, NUSCAN, SCANUM, RPARM, VIS, IERR)
C-----------------------------------------------------------------------
C   Reads a uv data base and returns averages of amp, phase or the RMS
C   scatter for selected baselines.
C   Needs to be initialized by a call to UVGET.
C   The  order of the baselines returned in SCANV is defined by
C   the order in the array NATLAB.  All data specified  (channels, IFs)
C   are averaged but only one polarization is allowed.
C   Inputs:
C     NUMBAS     I    Number of baselines to average.
C     ANTLAB(2,MXBASE)  I    The First (*,1) and second (*,2) antenna
C                     numbers of each baseline selected.
C     AMPVEC     L    If true do ampscalar averaging else vector.
C     DTYPE      I    Output type, 0 => amplitude, 1=> phase,
C                     3 => amplitude RMS.
C     DT         R    Averaging time in days
C   Input/Output:
C     RPARM(20)  R    Random parameter array, first record of call.
C                     (1) = 'INDE' => don't use.
C     VIS(3,*  ) R    Visibility array, first record of call.
C   Outputs:
C     SCANV(maxant,maxant) R   The result for antennas I<J,
C                     (I,J) = first polarization
C                     (J,I) = second polarization
C                     Undefined values will contain 'INDE'.
C                     Note: maxant is defined in the parameter include
C                     INCS:PUVD.INC.
C     TIME(8)    I    Time range, start, stop; days, hours, min, sec.
C                     Unless NUSCAN only first 4 values are set.
C     NUSCAN     L    True IF the first record in a new scan.
C     IERR       I    Return code, 0 => OK, -1 => out of data,
C                     > 0 => failed.
C   Output to common in D/CSOU.INC
C     SNAME(4)   R    Source name (16 char. 4 / word.)
C     QUAL       I    Source qualifier.
C     CALCOD     R    Calibrator code 4 char.
C     FLUX(4,IF) R    Total flux density I, Q, U, V pol, (Jy) each IF
C     FREQO(IF)  D    Frequency offset (Hz)
C   Note:   If the end of data is encountered (IERR=-1) then UVGET is
C   called with OPCODE='CLOS'.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMBAS, ANTLAB(2,MXBASE), DTYPE, TIME(8), SCANUM,
     *   IERR
      LOGICAL   AMPVEC, NUSCAN
      REAL      DT, DP9, SCANV(MXBASE), RPARM(*), VIS(3,*)
C
      LOGICAL   DONE1, GOTDAT
      INTEGER   I, JA1, JA2, SUNUM, JERR, ISLUN, IBASE, IVIS, KVIS,
     *   CNTTIM, JBASE
      REAL      T1
      DOUBLE PRECISION WORK(MXBASE,4), SUMTIM, COUNT(MXBASE), W, AMP,
     *   TEMP, TEMP1, TEMP2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      DATA ISLUN /26/
C-----------------------------------------------------------------------
C                                       See if first record read
      DONE1 = RPARM(1).NE.FBLANK
      GOTDAT = .FALSE.
C                                       Clear arrays
 10   CONTINUE
      DO 50 I = 1,NUMBAS
         COUNT(I) = 0.0D0
         WORK(I,1) = 0.0D0
         WORK(I,2) = 0.0D0
         WORK(I,3) = 0.0D0
         WORK(I,4) = 0.0D0
         SCANV(I) = FBLANK
 50      CONTINUE
      CNTTIM = 0
      SUMTIM = 0.0D0
C                                       Initialize time
      T1 = 1.0E10
C                                       Save scan number (0= no index)
      NUSCAN = SCANUM.NE.INXRNO
      SCANUM = INXRNO
      KVIS = (LREC-NRPARM) / 3
C                                       Loop reading data
 100     IF (.NOT.DONE1) THEN
            CALL UVGET ('READ', RPARM, VIS, IERR)
            IF (IERR.GT.0) GO TO 999
            END IF
         DONE1 = .FALSE.
C                                       Check if avg. or scan done
         IF ((INXRNO.GT.SCANUM) .OR. (IERR.LT.0) .OR.
     *      (RPARM(ILOCT+1).GT.(T1+DT))) GO TO 500
C                                       Antenna numbers
         IF (ILOCB.GE.0) THEN
            JA1 = RPARM(ILOCB+1) / 256. + 0.1
            JA2 = RPARM(ILOCB+1) - JA1 * 256 + 0.1
         ELSE
            JA1 = RPARM(ILOCA1+1) + 0.1
            JA2 = RPARM(ILOCA2+1) + 0.1
            END IF
C                                       Find baseline
         IBASE = 0
         JBASE = 0
         DO 110 I = 1,NUMBAS
            IF ((JA1.EQ.ANTLAB(1,I) .AND. (JA2.EQ.ANTLAB(2,I)))) THEN
               IF (IBASE.GT.0) THEN
                  JBASE = I
               ELSE
                  IBASE = I
                  END IF
               END IF
 110        CONTINUE
C                                       Not wanted
         IF (IBASE.LE.0) GO TO 100
C                                       got data
         GOTDAT = .TRUE.
C                                       Time
         SUMTIM = SUMTIM + RPARM(ILOCT+1)
         IF (T1.GT.1.0E9) T1 = RPARM(ILOCT+1)
         CNTTIM = CNTTIM + 1
C                                       Source no.
         SUNUM = CURSOU
C                                       Vector average:
      INCLUDE 'INCS:ZVD.INC'
            DO 150 IVIS = 1,KVIS
               W = VIS(3,IVIS)
               IF (W.GT.0.0D0) THEN
                  COUNT(IBASE) = COUNT(IBASE) + W
                  WORK(IBASE,1) = WORK(IBASE,1) + VIS(1,IVIS)*W
                  WORK(IBASE,2) = WORK(IBASE,2) + VIS(2,IVIS)*W
                  IF (AMPVEC) THEN
                     AMP = SQRT (VIS(1,IVIS) * VIS(1,IVIS) +
     *                  VIS(2,IVIS) * VIS(2,IVIS))
                     WORK(IBASE,3) = WORK(IBASE,3) + AMP*W
                     WORK(IBASE,4) = WORK(IBASE,4) + AMP*AMP*W
                  ELSE
                     WORK(IBASE,3) = WORK(IBASE,3) + W*(VIS(1,IVIS)**2)
                     WORK(IBASE,4) = WORK(IBASE,4) + W*(VIS(2,IVIS)**2)
                     END IF
                  END IF
C                                       in case 2 displays of same
               IF ((W.GT.0.0D0) .AND. (JBASE.GT.0)) THEN
                  COUNT(JBASE) = COUNT(JBASE) + W
                  WORK(JBASE,1) = WORK(JBASE,1) + W * VIS(1,IVIS)
                  WORK(JBASE,2) = WORK(JBASE,2) + W * VIS(2,IVIS)
                  IF (AMPVEC) THEN
                     AMP = SQRT (VIS(1,IVIS) * VIS(1,IVIS) +
     *                  VIS(2,IVIS) * VIS(2,IVIS))
                     WORK(JBASE,3) = WORK(JBASE,3) + W * AMP
                     WORK(JBASE,4) = WORK(JBASE,4) + W * AMP*AMP
                  ELSE
                     WORK(JBASE,3) = WORK(JBASE,3) + W*(VIS(1,IVIS)**2)
                     WORK(JBASE,4) = WORK(JBASE,4) + W*(VIS(2,IVIS)**2)
                     END IF
                  END IF
 150           CONTINUE
         GO TO 100
C                                       Scan done
C                                       See if have any data.
 500  IF ((.NOT.GOTDAT) .AND. (IERR.EQ.0)) GO TO 10
      IF (GOTDAT) THEN
         DO 550 I = 1,NUMBAS
C                                       Amplitude
            IF ((DTYPE.EQ.0) .AND. (COUNT(I).GT.0.0D0)) THEN
               IF (AMPVEC) THEN
                  SCANV(I) =  WORK(I,3) / COUNT(I)
               ELSE
                  SCANV(I) = SQRT (WORK(I,1)*WORK(I,1) +
     *               WORK(I,2)*WORK(I,2)) / COUNT(I)
                  END IF
               END IF
C                                       Phase
            IF ((DTYPE.EQ.1) .AND. (COUNT(I).GT.00D0))
     *         SCANV(I) = ATAN2 (WORK(I,2), WORK(I,1)+1.0D-20)
C                                       RMS
            IF ((DTYPE.EQ.2) .AND. (COUNT(I).GT.2)) THEN
               IF (AMPVEC) THEN
                  TEMP = COUNT(I) * WORK(I,4) - (WORK(I,2)**2)
               ELSE
                  TEMP1 = (COUNT(I) * WORK(I,3) - (WORK(I,1)**2))
                  TEMP2 = (COUNT(I) * WORK(I,4) - (WORK(I,2)**2))
                  TEMP = (WORK(I,1)**2)*TEMP1 + (WORK(I,2)**2)*TEMP2
                  TEMP = TEMP / ((WORK(I,1)**2) + (WORK(I,2)**2))
                  END IF
               SCANV(I) = SQRT (MAX (0.0D0, TEMP)) / COUNT(I)
               END IF
 550        CONTINUE
C                                       Get source info
         IF (NUSCAN) THEN
            CALL GETSOU (SUNUM, IUDISK, IUCNO, CATUV, ISLUN, JERR)
C                                       Didn't find source
            IF (JERR.EQ.11) THEN
               WRITE (MSGTXT,1750) SUNUM
               CALL MSGWRT (8)
               JERR = 0
               END IF
            IF (JERR.GT.0) THEN
               IERR = JERR
               WRITE (MSGTXT,1700) JERR
               GO TO 990
               END IF
            END IF
         END IF
C                                       Time
      T1 = 0.
      IF (CNTTIM.GT.0) T1 = SUMTIM / CNTTIM
      IF (DP9.GT.0.0) CALL T2LST (T1)
      CALL TODHMS (T1, TIME)
C                                       If end of data, close UVGET
      IF (IERR.LT.0) CALL UVGET ('CLOS', RPARM, VIS, JERR)
      IF (JERR.NE.0) IERR = JERR
      IF ((IERR.LT.0) .AND. (.NOT.GOTDAT)) IERR = -2
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1700 FORMAT ('LISTAV: ERROR',I3,' READING SOURCE TABLE')
 1750 FORMAT ('LISTAV: SOURCE ',I3,' NOT IN SU TABLE')
      END
      SUBROUTINE COLSET (NUMBAS, ANTLAB, IPOL, IRET)
C-----------------------------------------------------------------------
C   Routine to set up for column listing; checks that data in "TB"
C   order and prepares the list of antennas (ANTENS) for UVGET that
C   includes all antennas mentioned in either XANT or XBASE.
C   Also checks allowed Stokes types.
C   Input from common:
C      ISORT              C    Two char. sort order, must be 'TB'
C      XANT(*)            R    Antenna array
C      XBASE(*)           R    Baseline array
C   Output:
C      NUMBAS             I    The number of baselines selected.
C      ANTLAB(2,MXBASE)   I    The First (*,1) and second (*,2) antenna
C                              numbers of each baseline selected.
C      IPOL               I    Polarization type 1=I,2=Q,3=U,4=V
C                              R or L => 1
C      IRET               I    Return code, 0=OK, else failed
C   Output in Common:
C      ANTENS(*)          I    Selected antenna numbers
C      STOKES             C*4? Stokes' parameter selected (changed if
C                              an unallowed type specified)
C-----------------------------------------------------------------------
      CHARACTER STO(12)*4
      INTEGER   NUMBAS, IPOL, IRET, I, J, K, NEXT, IARG, MXBAS, IROUND,
     *   NOANT, IANT1, IANT2, IANT, LUN, NSTO, JANT, LIM
      LOGICAL   ALLANT, ALLBAS
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANTLAB(2,MXBASE), ACTANS(MAXANT), IUBUFF(512)
      INCLUDE 'LISTR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (IUBUFF, UBUFF)
      DATA NSTO, STO /12, 'I   ','Q   ','U   ','V   ','RR  ','LL  ',
     *   'RL  ','LR  ', 'XX', 'YY', 'XY', 'YX'/
      DATA MXBAS /MXBASE/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check Stokes
      DO 10 I = 1,NSTO
         IPOL = I
         IF (STOKES.EQ.STO(I)) GO TO 30
 10      CONTINUE
C                                       Not allowed type, use 'I' or
C                                       'RR' or 'LL'
         IF (ICOR0.GT.0) THEN
            WRITE (MSGTXT,1010) STOKES, STO(1)
            STOKES = STO(1)
            IPOL = 1
         ELSE IF (ICOR0.LT.0) THEN
            IF (ICOR0.EQ.-1) THEN
               WRITE (MSGTXT,1010) STOKES, STO(5)
               STOKES = STO(5)
               IPOL = 1
            ELSE IF (ICOR0.EQ.-2) THEN
               WRITE (MSGTXT,1010) STOKES, STO(6)
               STOKES = STO(6)
               IPOL = 1
            ELSE IF (ICOR0.EQ.-5) THEN
               WRITE (MSGTXT,1010) STOKES, STO(9)
               STOKES = STO(9)
               IPOL = 1
            ELSE IF (ICOR0.EQ.-6) THEN
               WRITE (MSGTXT,1010) STOKES, STO(10)
               STOKES = STO(10)
               IPOL = 1
               END IF
            END IF
         CALL MSGWRT (6)
 30   IF (IPOL.GT.4) IPOL = 1
C                                       Check sort order
      IF (ISORT.NE.'TB') THEN
         IRET = 1
         WRITE (MSGTXT,1030) ISORT
         GO TO 990
         END IF
C                                       BASELINE array selection
      NEXT = 0
      DO 55 I = 1,50
         IARG = IROUND (XBASE(I))
         IF (IARG.NE.0) THEN
            NEXT = NEXT + 1
            ANTENS(NEXT) = IARG
            END IF
 55      CONTINUE
C                                       Remove duplicates
      ALLBAS = NEXT.EQ.0
      IF (NEXT.GT.1) CALL IMERGE (NEXT, ANTENS, NEXT)
C                                       Fix input array for latter
      DO 60 I = 1,50
         IF (I.LE.NEXT) THEN
            XBASE(I) = ANTENS(I)
         ELSE
            XBASE(I) = 0.0
            END IF
 60      CONTINUE
C                                       ANTENNAS array selection
      NEXT = 0
      CALL FILL (MAXANT, 0, ANTENS)
C                                       ANTENNAS array.
      DO 65 I = 1,50
         IARG = IROUND (XANT(I))
         IF (IARG.NE.0) THEN
            NEXT = NEXT + 1
            ANTENS(NEXT) = IARG
            END IF
 65      CONTINUE
C                                       Remove duplicates
      ALLANT = NEXT.EQ.0
      IF (NEXT.GT.1) CALL IMERGE (NEXT, ANTENS, NEXT)
C                                       Fix input array for later
      DO 70 I = 1,50
         IF (I.LE.NEXT) THEN
            XANT(I) = ANTENS(I)
         ELSE
            XANT(I) = 0.0
            END IF
 70      CONTINUE
C                                       BASELINE array -> ANTENS
      IF ((ALLANT) .OR. (ALLBAS)) THEN
         CALL FILL (MAXANT, 0, ANTENS)
      ELSE
         DO 80 I = 1,50
            IARG = IROUND (XBASE(I))
C                                       See if already have
            IF (IARG.NE.0) THEN
               DO 75 J = 1,NEXT
                  IF (ABS(IARG).EQ.ABS(ANTENS(J))) GO TO 80
 75               CONTINUE
C                                       New antenna
               NEXT = NEXT + 1
               ANTENS(NEXT) = IARG
               END IF
 80         CONTINUE
         END IF
C                                       Fill ANTLAB with baseline
C                                       numbers.
      DO 510 I = 1,MXBAS
         ANTLAB(1,I) = 0
         ANTLAB(2,I) = 0
 510     CONTINUE
C                                       Find number of antennas
      LUN = 28
      CALL GETANS (DISKIN, CNOIN, CATBLK, LUN, IUBUFF, SUBARR, NOANT,
     *   ACTANS, IRET)
      IF ((IRET.NE.0) .AND. (IRET.NE.9) .AND. (IRET.NE.10)) THEN
         WRITE (MSGTXT,1511) IRET
         IRET = 2
         GO TO 990
         END IF
C                                       Fill arrays
      NEXT = 1
      DO 600 I = 1,MAXANT
         IF ((ALLANT) .OR. (I.GT.50)) THEN
            IANT1 = I
         ELSE
            IANT1 = ABS (XANT(I)) + 0.5
            END IF
         IF ((IANT1.LE.0) .OR. (IANT1.GT.NOANT) .OR.
     *      (ACTANS(IANT1).LE.0)) GO TO 600
         DO 590 J = 1,MAXANT
            IF ((ALLBAS) .OR. (J.GT.50)) THEN
               IANT2 = J
            ELSE
               IANT2 = ABS (XBASE(J)) + 0.5
               END IF
            IF ((IANT2.LE.0) .OR. (IANT2.GT.NOANT) .OR.
     *         (ACTANS(IANT2).LE.0)) GO TO 590
C                                       Check for redundant baselines
            IF ((IANT1.NE.IANT2) .OR. (DOACOR)) THEN
               IF (DPARM(8).LE.0.0) THEN
                  JANT = MIN (IANT1, IANT2)
                  IANT = MAX (IANT1, IANT2)
                  LIM = NEXT - 1
                  DO 570 K = 1,LIM
                     IF ((JANT.EQ.ANTLAB(1,K)) .AND.
     *                  (IANT.EQ.ANTLAB(2,K))) GO TO 590
 570                 CONTINUE
                  END IF
               ANTLAB(1,NEXT) = MIN (IANT1, IANT2)
               ANTLAB(2,NEXT) = MAX (IANT1, IANT2)
               NEXT = NEXT + 1
               END IF
 590        CONTINUE
 600     CONTINUE
      NUMBAS = NEXT - 1
      IF (NUMBAS.GT.0) GO TO 999
C                                       No baselines
      IRET = 2
      WRITE (MSGTXT,1600)
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Warning: Stokes=',A4,' not allowed, using Stokes=',A4)
 1030 FORMAT ('INCORRECT SORT ORDER = ',A2,' NOT TB')
 1511 FORMAT ('COLSET: GETANS ERROR ',I3,' SEARCHING ANTENNA TABLES')
 1600 FORMAT ('ERROR: NO BASELINES SPECIFIED!')
      END
      SUBROUTINE GAINCH (IRET)
C-----------------------------------------------------------------------
C   Counts lines for column listing of a table
C   Output:
C      IRET   I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER   KEYS(7)*48, DATKEY(20)*48, UTYPE*2, TYPKEY*48
      INTEGER   TIME(8), DTYPE, NPASS, IPASS, NCOLPV, SKEY(2,2), L,
     *   MCOR, NUMPRT, IOFF, IIPOL, JPOL, LUN, NUMNOD, COLS(7), TYPKOL,
     *   TIMKOL, SUBKOL, ANTKOL, SOUKOL, KOL, KOL2, L2, KSTOK, RECNO,
     *   LPASS, KPASS, COLS2(7), FQKOL, LCOUNT, NDIGIT, MDIGIT
      LOGICAL   T, DONE, NUSCAN, ISAPPL, AMPPHS
      REAL      CATR(256), SFACT,  SFACTI, FKEY(2,2), SMAX, DT, SFACOL,
     *   FREQX, TSEC
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128), RANOD(25), DECNOD(25)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANTLAB(MAXANT), NTERM, KOLS(MAXCLC), NUMV(MAXCLC),
     *   NKEY, NREC, NCOL, DATP(128,2), KEYSUB(2,2), NUMKEY
      REAL      SCANV(MAXANT)
      INCLUDE 'LISTR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB /4*1/
      DATA KEYS /'TIME', 'SUBARRAY', 'ANTENNA NO.', 'SOURCE ID',
     *   'FREQ ID', ' ', ' '/
      DATA TYPKEY /'CAL TYPE'/
      DATA DATKEY /'REAL1', 'WEIGHT 1', 'RATE 1','DELAY 1','TSYS 1',
     *             'MBDELAY1','TANT 1', 'POWER DIF1', 'POWER SUM1',
     *             'POST GAIN1',
     *             'REAL2', 'WEIGHT 2', 'RATE 2','DELAY 2','TSYS 2',
     *             'MBDELAY2','TANT 2', 'POWER DIF2', 'POWER SUM2',
     *             'POST GAIN2'/
      DATA T /.TRUE./
      DATA LUN /30/
C-----------------------------------------------------------------------
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
C                                       Setup
      CALL H2CHR (8, 1, CATH(KHOBJ), SNAME)
      QUAL = 0
      CALCOD = ' '
      SFACT = 1.0
      CURSOU = -1
      DTYPE = DPARM(1) + 0.5
C                                       Get antenna info for
C                                       parallactic angle or elevation
      IF ((DTYPE.EQ.9) .OR. (DTYPE.EQ.11)) THEN
         CALL GETANT (DISKIN, CNOIN, SUBARR, CATBLK, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010)
            GO TO 990
            END IF
         END IF
C                                       Initialize source tables
C                                       Timerange
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)).EQ.0.0)
     *   TIMRNG(1) = -1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)).EQ.0.0)
     *   TIMRNG(5) = 1.0E6
C                                       Set time range.
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
       TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      IUDISK = DISKIN
      IUCNO = CNOIN
      IULUN = 25
      IXLUN = 28
      ICLUN = 29
      IFLUN = 30
      CALL COPY (256, CATBLK, CATUV)
      KLOCSU = ILOCSU
      CALL SOUFIL (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Only allow amp, phase,delay,rate
C                                       SNR, par. ang.,Tsys, elev. or
C                                       amp/phase
C                                       anything else = phase
      IF ((DTYPE.NE.0) .AND. (DTYPE.NE.1) .AND. (DTYPE.NE.5) .AND.
     *   (DTYPE.NE.6) .AND. (DTYPE.NE.7) .AND. (DTYPE.NE.8) .AND.
     *   (DTYPE.NE.9) .AND. (DTYPE.NE.10) .AND. (DTYPE.NE.11) .AND.
     *   (DTYPE.NE.12) .AND. (DTYPE.NE.13) .AND. (DTYPE.NE.15) .AND.
     *   (DTYPE.NE.16) .AND. (DTYPE.NE.17) .AND. (DTYPE.NE.18) .AND.
     *   (DTYPE.NE.19)) DTYPE=1
      LPASS = 1
      IF (DTYPE.EQ.5) LPASS = 2
      AMPPHS = DTYPE.EQ.5
C                                       Only allow R/L for amp,phase
      IF ((DTYPE.GT.1) .AND. (STOKES.EQ.'POLD')) STOKES = ' '
C                                       Number of char per col.
      NCOLPV = 4
      IF ((DPARM(3).GT.3.6) .AND. (DPARM(3).LT.10.4))
     *   NCOLPV = DPARM(3) + 0.5
      NDIGIT = (10**NCOLPV) - 1
      MDIGIT = (10**(NCOLPV-1)) - 1
      MDIGIT = -MDIGIT
C                                       Setup for col. listing.
      CALL GANSET (ANTLAB, IIPOL, IRET)
      IF (IRET.NE.0) GO TO 999
      FREQ = BFREQ * 1.0D-9
C                                       Determine number to print
      NUMPRT = (NACROS-15) / NCOLPV
      IF (NUMPRT.GT.NANTSL) NUMPRT = NANTSL
      NPASS = ((NANTSL*1.0) / NUMPRT) + 0.999
      MCOR = 1
      IF (IIPOL.EQ.3) MCOR = 2
      IF (STOKES.EQ.'POLD') MCOR = 1
      IF ((DTYPE.EQ.15) .OR. (DTYPE.EQ.16)) THEN
         NUMKEY = 7
      ELSE
         NUMKEY = 6
         END IF
C                                       Loop over amp/phase listings
      DO 700 KPASS = 1,LPASS
         NUSCAN = .FALSE.
         IF (AMPPHS .AND. (KPASS.EQ.1)) DTYPE = 0
         IF (AMPPHS .AND. (KPASS.EQ.2)) DTYPE = 1
C                                       IF loop
C                                       Only One pass for multiband
C                                       delay.
         IF (DTYPE.EQ.12) BIF = 1
C                                          Polarization loop
         DO 640 JPOL = 1,MCOR
            KSTOK = 1
            IF ((JPOL.EQ.2) .OR. (ICOR0.EQ.-2)) KSTOK = 2
            IF ((JPOL.EQ.2) .OR. (ICOR0.EQ.-6)) KSTOK = 2
            IF ((IIPOL.EQ.2) .OR. (JPOL.EQ.2)) KSTOK = 2
            IF (STOKES.EQ.'POLD') KSTOK = 3
            IF (ICOR0.LE.-5) KSTOK = KSTOK + 3
C                                          Start outer loop
            DO 600 IPASS = 1,NPASS
               NUMPRT = (NACROS-15) / NCOLPV
               IOFF = (IPASS-1) * NUMPRT
               IF ((IOFF+NUMPRT).GT.NANTSL) NUMPRT = NANTSL - IOFF
               CURSOU = -1
               LCOUNT = -1
C                                          Open table
C                                          Use TABINI for column
C                                          pointer array
               NKEY = 0
               NREC = 0
               NCOL = 0
               CALL TABINI ('READ', INEXT, DISKIN, CNOIN, INVER,
     *            CATBLK, LUN, NKEY, NREC, NCOL, DATP, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Close
               CALL TABIO ('CLOS', 0, RECNO, BUFFER, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Re-open
               GMMOD = 1.0
               IF (INEXT.EQ.'CL') THEN
                  CALL CALINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *               NUMIF, NTERM, GMMOD, IRET)
               ELSE IF (INEXT.EQ.'SN') THEN
                  CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *               NUMIF, NUMNOD, GMMOD, RANOD, DECNOD, ISAPPL, IRET)
               ELSE IF (INEXT.EQ.'TY') THEN
                  CALL TYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMPOL, NUMIF,
     *               IRET)
               ELSE IF (INEXT.EQ.'SY') THEN
                  CALL SYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *               NUMIF, IRET)
               ELSE
                  IRET = 10
                  END IF
               IF (IRET.NE.0) GO TO 999
               IF (GMMOD.LE.0.0) GMMOD = 1.0
C                                          Find column numbers
               IF ((DTYPE.EQ.0) .OR. (DTYPE.EQ.1)) L = 1
               IF (DTYPE.EQ.9) L = 1
               IF (DTYPE.EQ.8) L = 2
               IF (DTYPE.EQ.7) L = 3
               IF (DTYPE.EQ.6) L = 4
               IF (DTYPE.EQ.10) L = 5
               IF (DTYPE.EQ.13) L = 7
               IF (DTYPE.EQ.11) L = 1
               IF (DTYPE.EQ.12) L = 6
               IF (DTYPE.EQ.15) L = 8
               IF (DTYPE.EQ.16) L = 8
               IF (DTYPE.EQ.17) L = 8
               IF (DTYPE.EQ.18) L = 9
               IF (DTYPE.EQ.19) L = 10
C                                          Correct for polarization
               IF (((IIPOL.EQ.3) .AND. (JPOL.EQ.2)) .OR.
     *            ((IIPOL.EQ.2) .AND.
     *            ((ICOR0.EQ.-1) .OR. (ICOR0.EQ.-5)))) L = L + 10
               L2 = L
               KEYS(6) = DATKEY(L)
               IF (NUMKEY.EQ.7) KEYS(7) = DATKEY(L+1)
               IF ((DTYPE.EQ.15) .OR. (DTYPE.EQ.16)) THEN
                  CALL FNDCOL (1, TYPKEY, 24, T, BUFFER, TYPKOL, IRET)
                  IF ((IRET.NE.0) .AND. (IRET.LE.10)) GO TO 999
                  IF (IRET.GT.10) TYPKOL = -1
                  END IF
               CALL FNDCOL (NUMKEY, KEYS, 24, T, BUFFER, COLS, IRET)
               IF (IRET.NE.0) GO TO 999
               L = COLS(1)
               TIMKOL = DATP(L,1)
               L = COLS(2)
               SUBKOL = DATP(L,1)
               L = COLS(3)
               ANTKOL = DATP(L,1)
               L = COLS(4)
               SOUKOL = DATP(L,1)
               L = COLS(5)
               FQKOL = DATP(L,1)
               L = COLS(6)
               KOL = DATP(L,1)
               KOL2 = 0
               IF (NUMKEY.EQ.7) THEN
                  L = COLS(7)
                  KOL2 = DATP(L,1)
                  END IF
               IF (STOKES.EQ.'POLD') THEN
                  KEYS(6) = DATKEY(L2 + 7)
                  CALL FNDCOL (6, KEYS, 24, T, BUFFER, COLS2, IRET)
                  IF (IRET.NE.0) GO TO 999
                  L = COLS2(6)
                  KOL2 = DATP(L,1)
                  END IF
C                                       Correct for IF
C                                       Except for mb delay
               IF (DTYPE.NE.12) THEN
                  KOL = KOL + (BIF - 1)
                  IF (KOL2.GT.0) KOL2 = KOL2 + (BIF - 1)
                  END IF
C                                          Resort if necessary.
               IF (BUFFER(43).NE.TIMKOL) THEN
C                                          Sort to time order.
                  CALL TABIO ('CLOS', 0, RECNO, BUFFER, BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 999
C                                          Change catalog status to WRIT
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'CLRD', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
                  IRET = 0
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'WRIT', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
                  IRET = 0
C                                          Mark as "WRIT" in CFILES
                  FRW(1) = 1
                  UBUFSZ = UVBFSL * 2
                  SKEY(1,1) = COLS(1)
                  SKEY(2,1) = COLS(1)
                  SKEY(1,2) = COLS(3)
                  SKEY(2,2) = COLS(3)
                  CALL TABSRT (DISKIN, CNOIN, INEXT, INVER, INVER,
     *               SKEY, KEYSUB, FKEY, BUFFER, CATBLK, IRET)
                  IF (IRET.NE.0) GO TO 999
C                                          Change status back to READ
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'CLWR', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
                  IRET = 0
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'READ', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
                  IRET = 0
C                                          Mark as "READ" in CFILES
                  FRW(1) = 0
C                                          Re initialize.
                  GMMOD = 1.0
                  IF (INEXT.EQ.'CL') THEN
                     CALL CALINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *                  NUMIF, NTERM, GMMOD, IRET)
                  ELSE IF (INEXT.EQ.'SN') THEN
                     CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *                  NUMIF, NUMNOD, GMMOD, RANOD, DECNOD, ISAPPL,
     *                  IRET)
                  ELSE IF (INEXT.EQ.'TY') THEN
                     CALL TYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMPOL, NUMIF,
     *                  IRET)
                  ELSE
                     CALL SYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *                  NUMIF, IRET)
                     END IF
                  IF (IRET.NE.0) GO TO 999
                  IF (GMMOD.LE.0.0) GMMOD = 1.0
                  END IF
C                                          first page titles
               IF ((IPASS.EQ.1) .OR. (DOCRT.GT.-2.5)) NCOUNT = NCOUNT+1
               IF (DOCRT.GT.-2.5) THEN
                  NCOUNT = NCOUNT + 3
                  IF (INEXT.EQ.'SN') NCOUNT = NCOUNT + 1
                  END IF
C                                          Use 0.01 sec tolerance
               DT = 0.01 / 86400.0
C                                          Loop thru data
 100           CONTINUE
                  CALL GAINAV (OPTYPE, NUMPRT, ANTLAB(IOFF+1), BUFFER,
     *               TIMKOL, SOUKOL, SUBKOL, ANTKOL, FQKOL, KOL, KOL2,
     *               TYPKOL, DTYPE, JPOL, BIF, TCAL, SCANV, DT,
     *               DPARM(9), TIME, TSEC, NUSCAN, RECNO, IRET)
                  DONE = IRET.LT.0
C                                          Check if found data
                  IF (IRET.EQ.-2) GO TO 600
                  IF (IRET.GT.0) GO TO 999
                  IF (NUSCAN) THEN
                     SFACOL = 1.0E10
                     SFACT = 1.0E6
                     LCOUNT = -1
                     END IF
C                                          Find max value
                  FREQX = FREQ * 1.0D9 + FREQO(BIF) + FREQIF(BIF)
                  CALL GAINMX (SCANV, NUMPRT, DTYPE, NCOLPV, FREQX,
     *               SMAX, SFACT)
                  IF ((DTYPE.EQ.1) .OR. (DTYPE.EQ.9) .OR.
     *               (DTYPE.EQ.11)) THEN
                     IF ((DPARM(10).GT.0.0) .AND. (KPASS.EQ.1)) THEN
                        SFACT = RAD2DG / DPARM(10)
                     ELSE IF (DPARM(7).LE.0.0) THEN
                        SFACT = RAD2DG
                        END IF
                  ELSE IF (DPARM(10).GT.0.0) THEN
                     SFACT = 1.0 / DPARM(10)
                     END IF
                  IF ((XFACTR.GT.0.0) .AND. (DPARM(10).LE.0.0)) SFACT =
     *               SFACT / XFACTR
                  SFACTI = 1000.0 / SFACT
C                                          Header for scan
C                                          (2 blank lines FIRST)
                  IF (NUSCAN) THEN
                     NUSCAN = .FALSE.
                     IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
                     NCOUNT = NCOUNT + 1
C                                          Source info
                     IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
                     END IF
C                                       Type of data listed: 1=amp,
C                                       2=phase, 6=delay, 7=rate,
C                                       8 = SNR, 9=Par. ang., 10 = Tsys
C                                       11 = elev., 12= mb delay 13 Tant
                  IF (SFACT.NE.SFACOL) THEN
                     NCOUNT = NCOUNT + 1
C                                          Section label
                     IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
                     IF (((IPCNT.GT.3) .AND. (IPCNT.LT.PRTMAX-1))
     *                  .OR. (DOCRT.LE.-2.5)) NCOUNT = NCOUNT + 1
                     END IF
                  LCOUNT = LCOUNT + 1
                  IF (MOD(LCOUNT,IXINC).EQ.0) NCOUNT = NCOUNT + 1
C                                          If more - loop
                  IF (.NOT.DONE) GO TO 100
 600           CONTINUE
C                                          End of polarization loop
 640        CONTINUE
C                                       End of amp/phase loop
 700     CONTINUE
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('GAINCH: ERROR ',I3,' READING ANTENNA FILE')
      END
      SUBROUTINE GAINUV (IRET)
C-----------------------------------------------------------------------
C   Gives column listing of an SN or CL table (multi or single source).
C   Output:
C      IRET   I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      CHARACTER   KEYS(7)*48, STOKX(6)*4, DATKEY(20)*48, ENTRY*20,
     *   UTYPE*2, TYPKEY*48
      INTEGER   TIME(8), DTYPE, NPASS, IPASS, ITEMP, ICOL, NCOLPV, IRET,
     *   COLPNT, SKEY(2,2), L, IERR, MCOR, NUMPRT, IOFF, IIPOL, KQUAL,
     *   JPOL, LUN, NUMNOD, COLS(7), TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *   KOL, KOL2, L2, KSTOK, I4TEMP, RECNO,  LPASS, KPASS, COLS2(7),
     *   FQKOL, LCOUNT, NDIGIT, MDIGIT, TYPKOL
      LOGICAL   T, DONE, NUSCAN, ISAPPL, AMPPHS
      REAL      CATR(256), SFACT,  SFACTI, FKEY(2,2), SMAX, DT, SFACOL,
     *   FREQX, XROUND, XTEMP, TSEC
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128), SFREQ, RANOD(25), DECNOD(25)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANTLAB(MAXANT), NTERM, KOLS(MAXCLC), NUMV(MAXCLC),
     *   NKEY, NREC, NCOL, DATP(128,2), KEYSUB(2,2), NUMKEY
      REAL      SCANV(MAXANT)
      INCLUDE 'LISTR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB /4*1/
      DATA STOKX /'R', 'L', 'R-L', 'X', 'Y', 'X-Y'/
      DATA KEYS /'TIME', 'SUBARRAY', 'ANTENNA NO.', 'SOURCE ID',
     *   'FREQ ID', ' ', ' '/
      DATA TYPKEY /'CAL TYPE'/
      DATA DATKEY /'REAL1', 'WEIGHT 1', 'RATE 1','DELAY 1','TSYS 1',
     *             'MBDELAY1','TANT 1', 'POWER DIF1', 'POWER SUM1',
     *             'POST GAIN1',
     *             'REAL2', 'WEIGHT 2', 'RATE 2','DELAY 2','TSYS 2',
     *             'MBDELAY2','TANT 2', 'POWER DIF2', 'POWER SUM2',
     *             'POST GAIN2'/
      DATA T /.TRUE./
      DATA LUN /30/
C-----------------------------------------------------------------------
C                                       Setup
      CALL H2CHR (8, 1, CATH(KHOBJ), SNAME)
      QUAL = 0
      CALCOD = ' '
      SFACT = 1.0
      FLUX(1,BIF) = 0.0
      FLUX(2,BIF) = 0.0
      FLUX(3,BIF) = 0.0
      FLUX(4,BIF) = 0.0
      CURSOU = -1
      DTYPE = DPARM(1) + 0.5
C                                       Get antenna info for
C                                       parallactic angle or elevation
      IF ((DTYPE.EQ.9) .OR. (DTYPE.EQ.11)) THEN
         CALL GETANT (DISKIN, CNOIN, SUBARR, CATBLK, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010)
            GO TO 990
            END IF
         END IF
C                                       Initialize source tables
C                                       Timerange
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)).EQ.0.0)
     *   TIMRNG(1) = -1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)).EQ.0.0)
     *   TIMRNG(5) = 1.0E6
C                                       Set time range.
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
       TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      IUDISK = DISKIN
      IUCNO = CNOIN
      IULUN = 25
      IXLUN = 28
      ICLUN = 29
      IFLUN = 30
      CALL COPY (256, CATBLK, CATUV)
      KLOCSU = ILOCSU
      CALL SOUFIL (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Only allow amp, phase,delay,rate
C                                       SNR, par. ang.,Tsys, elev. or
C                                       amp/phase
C                                       anything else = phase
      IF ((DTYPE.NE.0) .AND. (DTYPE.NE.1) .AND. (DTYPE.NE.5) .AND.
     *   (DTYPE.NE.6) .AND. (DTYPE.NE.7) .AND. (DTYPE.NE.8) .AND.
     *   (DTYPE.NE.9) .AND. (DTYPE.NE.10) .AND. (DTYPE.NE.11) .AND.
     *   (DTYPE.NE.12) .AND. (DTYPE.NE.13) .AND. (DTYPE.NE.15) .AND.
     *   (DTYPE.NE.16) .AND. (DTYPE.NE.17) .AND. (DTYPE.NE.18) .AND.
     *   (DTYPE.NE.19)) DTYPE=1
      LPASS = 1
      IF (DTYPE.EQ.5) LPASS = 2
      AMPPHS = DTYPE.EQ.5
C                                       Only allow R/L for amp,phase
      IF ((DTYPE.GT.1) .AND. (STOKES.EQ.'POLD')) STOKES = ' '
C                                       Number of char per col.
      NCOLPV = 4
      IF ((DPARM(3).GT.3.6) .AND. (DPARM(3).LT.10.4))
     *   NCOLPV = DPARM(3) + 0.5
      NDIGIT = (10**NCOLPV) - 1
      MDIGIT = (10**(NCOLPV-1)) - 1
      MDIGIT = -MDIGIT
C                                       Setup for col. listing.
      CALL GANSET (ANTLAB, IIPOL, IRET)
      IF (IRET.NE.0) GO TO 999
      FREQ = BFREQ * 1.0D-9
C                                       Determine number to print
      NUMPRT = (NACROS-15) / NCOLPV
      IF (NUMPRT.GT.NANTSL) NUMPRT = NANTSL
      NPASS = ((NANTSL*1.0) / NUMPRT) + 0.999
      MCOR = 1
      IF (IIPOL.EQ.3) MCOR = 2
      IF (STOKES.EQ.'POLD') MCOR = 1
      IF ((DTYPE.EQ.15) .OR. (DTYPE.EQ.16)) THEN
         NUMKEY = 7
      ELSE
         NUMKEY = 6
         END IF
C                                       Loop over amp/phase listings
      DO 700 KPASS = 1,LPASS
         NUSCAN = .FALSE.
         IF (AMPPHS .AND. (KPASS.EQ.1)) DTYPE = 0
         IF (AMPPHS .AND. (KPASS.EQ.2)) DTYPE = 1
C                                       IF loop
C                                       Only One pass for multiband
C                                       delay.
         IF (DTYPE.EQ.12) BIF = 1
C                                          Polarization loop
         DO 640 JPOL = 1,MCOR
            KSTOK = 1
            IF ((JPOL.EQ.2) .OR. (ICOR0.EQ.-2)) KSTOK = 2
            IF ((JPOL.EQ.2) .OR. (ICOR0.EQ.-6)) KSTOK = 2
            IF ((IIPOL.EQ.2) .OR. (JPOL.EQ.2)) KSTOK = 2
            IF (STOKES.EQ.'POLD') KSTOK = 3
            IF (ICOR0.LE.-5) KSTOK = KSTOK + 3
C                                          Start outer loop
            DO 600 IPASS = 1,NPASS
               NUMPRT = (NACROS-15) / NCOLPV
               IOFF = (IPASS-1) * NUMPRT
               IF ((IOFF+NUMPRT).GT.NANTSL) NUMPRT = NANTSL - IOFF
               CURSOU = -1
               LCOUNT = -1
C                                          Open table
C                                          Use TABINI for column
C                                          pointer array
               NKEY = 0
               NREC = 0
               NCOL = 0
               CALL TABINI ('READ', INEXT, DISKIN, CNOIN, INVER,
     *            CATBLK, LUN, NKEY, NREC, NCOL, DATP, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Close
               CALL TABIO ('CLOS', 0, RECNO, BUFFER, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Re-open
               GMMOD = 1.0
               IF (INEXT.EQ.'CL') THEN
                  CALL CALINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *               NUMIF, NTERM, GMMOD, IRET)
               ELSE IF (INEXT.EQ.'SN') THEN
                  CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *               NUMIF, NUMNOD, GMMOD, RANOD, DECNOD, ISAPPL, IRET)
               ELSE IF (INEXT.EQ.'TY') THEN
                  CALL TYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMPOL, NUMIF,
     *               IRET)
               ELSE IF (INEXT.EQ.'SY') THEN
                  CALL SYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *               NUMIF, IRET)
               ELSE
                  IRET = 10
                  END IF
               IF (IRET.NE.0) GO TO 999
               IF (GMMOD.LE.0.0) GMMOD = 1.0
C                                          Find column numbers
               IF ((DTYPE.EQ.0) .OR. (DTYPE.EQ.1)) L = 1
               IF (DTYPE.EQ.9) L = 1
               IF (DTYPE.EQ.8) L = 2
               IF (DTYPE.EQ.7) L = 3
               IF (DTYPE.EQ.6) L = 4
               IF (DTYPE.EQ.10) L = 5
               IF (DTYPE.EQ.13) L = 7
               IF (DTYPE.EQ.11) L = 1
               IF (DTYPE.EQ.12) L = 6
               IF (DTYPE.EQ.15) L = 8
               IF (DTYPE.EQ.16) L = 8
               IF (DTYPE.EQ.17) L = 8
               IF (DTYPE.EQ.18) L = 9
               IF (DTYPE.EQ.19) L = 10
C                                          Correct for polarization
               IF (((IIPOL.EQ.3) .AND. (JPOL.EQ.2)) .OR.
     *            ((IIPOL.EQ.2) .AND.
     *            ((ICOR0.EQ.-1) .OR. (ICOR0.EQ.-5)))) L = L + 10
               L2 = L
               KEYS(6) = DATKEY(L)
               IF (NUMKEY.EQ.7) KEYS(7) = DATKEY(L+1)
               IF ((DTYPE.EQ.15) .OR. (DTYPE.EQ.16)) THEN
                  CALL FNDCOL (1, TYPKEY, 24, T, BUFFER, TYPKOL, IRET)
                  IF ((IRET.NE.0) .AND. (IRET.LE.10)) GO TO 999
                  IF (IRET.GT.10) TYPKOL = -1
                  END IF
               CALL FNDCOL (NUMKEY, KEYS, 24, T, BUFFER, COLS, IRET)
               IF (IRET.NE.0) GO TO 999
               L = COLS(1)
               TIMKOL = DATP(L,1)
               L = COLS(2)
               SUBKOL = DATP(L,1)
               L = COLS(3)
               ANTKOL = DATP(L,1)
               L = COLS(4)
               SOUKOL = DATP(L,1)
               L = COLS(5)
               FQKOL = DATP(L,1)
               L = COLS(6)
               KOL = DATP(L,1)
               KOL2 = 0
               IF (NUMKEY.EQ.7) THEN
                  L = COLS(7)
                  KOL2 = DATP(L,1)
                  END IF
               IF (STOKES.EQ.'POLD') THEN
                  KEYS(6) = DATKEY(L2 + 7)
                  CALL FNDCOL (6, KEYS, 24, T, BUFFER, COLS2, IRET)
                  IF (IRET.NE.0) GO TO 999
                  L = COLS2(6)
                  KOL2 = DATP(L,1)
                  END IF
C                                       Correct for IF
C                                       Except for mb delay
               IF (DTYPE.NE.12) THEN
                  KOL = KOL + (BIF - 1)
                  IF (KOL2.GT.0) KOL2 = KOL2 + (BIF - 1)
                  END IF
C                                          Resort if necessary.
               IF (BUFFER(43).NE.TIMKOL) THEN
C                                          Sort to time order.
                  CALL TABIO ('CLOS', 0, RECNO, BUFFER, BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 999
C                                          Change catalog status to WRIT
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'CLRD', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
                  IRET = 0
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'WRIT', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
                  IRET = 0
C                                          Mark as "WRIT" in CFILES
                  FRW(1) = 1
                  UBUFSZ = UVBFSL * 2
                  SKEY(1,1) = COLS(1)
                  SKEY(2,1) = COLS(1)
                  SKEY(1,2) = COLS(3)
                  SKEY(2,2) = COLS(3)
                  CALL TABSRT (DISKIN, CNOIN, INEXT, INVER, INVER,
     *               SKEY, KEYSUB, FKEY, BUFFER, CATBLK, IRET)
                  IF (IRET.NE.0) GO TO 999
C                                          Change status back to READ
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'CLWR', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
                  IRET = 0
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'READ', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
                  IRET = 0
C                                          Mark as "READ" in CFILES
                  FRW(1) = 0
C                                          Re initialize.
                  GMMOD = 1.0
                  IF (INEXT.EQ.'CL') THEN
                     CALL CALINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *                  NUMIF, NTERM, GMMOD, IRET)
                  ELSE IF (INEXT.EQ.'SN') THEN
                     CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *                  NUMIF, NUMNOD, GMMOD, RANOD, DECNOD, ISAPPL,
     *                  IRET)
                  ELSE IF (INEXT.EQ.'TY') THEN
                     CALL TYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMPOL, NUMIF,
     *                  IRET)
                  ELSE
                     CALL SYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *                  NUMIF, IRET)
                     END IF
                  IF (IRET.NE.0) GO TO 999
                  IF (GMMOD.LE.0.0) GMMOD = 1.0
                  END IF
C                                          first page titles
               IF ((IPASS.EQ.1) .OR. (DOCRT.GT.-2.5)) THEN
                  WRITE (LINE,1020) NAMEIN, CLAIN, SEQIN, DISKIN,
     *               NLUSER, BIF
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
                  END IF
               IF (DOCRT.GT.-2.5) THEN
                  WRITE (LINE,1021) FREQ, NCOR, NVIS
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
                  WRITE (LINE,1022) STOKX(KSTOK), SUBARR
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
                  WRITE (LINE,1023) INEXT, INVER
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
                  IF (INEXT.EQ.'SN') THEN
                     IF (ISAPPL) THEN
                        LINE = 'SN table has already been applied '
     *                     // 'to a CL table'
                     ELSE
                        LINE = 'SN table has not been applied ' //
     *                     'to a CL table'
                        END IF
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  END IF
C                                          Use 0.01 sec tolerance
               DT = 0.01 / 86400.0
C                                          Loop thru data
 100           CONTINUE
                  CALL GAINAV (OPTYPE, NUMPRT, ANTLAB(IOFF+1), BUFFER,
     *               TIMKOL, SOUKOL, SUBKOL, ANTKOL, FQKOL, KOL, KOL2,
     *               TYPKOL, DTYPE, JPOL, BIF, TCAL, SCANV, DT,
     *               DPARM(9), TIME, TSEC, NUSCAN, RECNO, IRET)
                  DONE = IRET.LT.0
C                                          Check if found data
                  IF (IRET.EQ.-2) GO TO 600
                  IF (IRET.GT.0) GO TO 999
                  IF (NUSCAN) THEN
                     SFACOL = 1.0E10
                     SFACT = 1.0E6
                     LCOUNT = -1
                     END IF
C                                          Find max value
                  FREQX = FREQ * 1.0D9 + FREQO(BIF) + FREQIF(BIF)
                  CALL GAINMX (SCANV, NUMPRT, DTYPE, NCOLPV, FREQX,
     *               SMAX, SFACT)
                  IF ((DTYPE.EQ.1) .OR. (DTYPE.EQ.9) .OR.
     *               (DTYPE.EQ.11)) THEN
                     IF ((DPARM(10).GT.0.0) .AND. (KPASS.EQ.1)) THEN
                        SFACT = RAD2DG / DPARM(10)
                     ELSE IF (DPARM(7).LE.0.0) THEN
                        SFACT = RAD2DG
                        END IF
                  ELSE IF (DPARM(10).GT.0.0) THEN
                     SFACT = 1.0 / DPARM(10)
                     END IF
                  IF ((XFACTR.GT.0.0) .AND. (DPARM(10).LE.0.0)) SFACT =
     *               SFACT / XFACTR
                  SFACTI = 1000.0 / SFACT
C                                          Header for scan
C                                          (2 blank lines FIRST)
                  IF (NUSCAN) THEN
                     NUSCAN = .FALSE.
                     IF (DOCRT.GT.-2.5) THEN
                        LINE = ' '
                        CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                     TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                        IF (IERR.NE.0) GO TO 950
                        END IF
                     KQUAL = QUAL
                     WRITE (TITL1,1100) SNAME, KQUAL, STOKX(KSTOK),
     *                  BIF
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, TITL1, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
C                                          Source info
                     IF (DOCRT.GT.-2.5) THEN
                        SFREQ = FREQ + ((FREQO(BIF) + FREQIF(BIF))
     *                     * 1.0D-9)
                        WRITE (LINE,1101) FLUX(1,BIF), CALCOD, SFREQ
                        CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                     TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                        IF (IERR.NE.0) GO TO 950
                        END IF
                     END IF
C                                       Type of data listed: 1=amp,
C                                       2=phase, 6=delay, 7=rate,
C                                       8 = SNR, 9=Par. ang., 10 = Tsys
C                                       11 = elev., 12= mb delay 13 Tant
                  IF (SFACT.NE.SFACOL) THEN
                     IF (DTYPE.EQ.1) THEN
                        LINE = 'Gain phases in degrees'
                        IF ((DPARM(7).GT.0.0) .OR. (DPARM(10).GT.0.0)
     *                     .OR. ((XFACTR.GT.0.0) .AND. (XFACTR.NE.1.0)))
     *                     WRITE (LINE,1109) 'Gain phases',
     *                     SFACTI * RAD2DG
                     ELSE IF (DTYPE.EQ.0) THEN
                        WRITE (LINE,1110) SFACTI
                        IF (OPTYPE.EQ.'EFST') WRITE (LINE,1116) SFACTI
                        IF (OPTYPE.EQ.'SEFD') WRITE (LINE,1117) SFACTI
                     ELSE IF (DTYPE.EQ.6) THEN
                        XTEMP = SFACTI * 1.0E9
                        IF (ABS (XTEMP).GT.1.0E6) XTEMP = 0.0
                        WRITE (LINE,1111) XTEMP
                     ELSE IF (DTYPE.EQ.7) THEN
                        XTEMP = SFACTI * 1000.0 * FREQX
                        IF (ABS (XTEMP).GT.1.0E6) XTEMP = 0.0
                        WRITE (LINE,1112) XTEMP
                        END IF
                     IF (ABS (SFACTI).GT.1.0E6) SFACTI = 0.0
                     IF (DTYPE.EQ.8) THEN
                        WRITE (LINE,1113) SFACTI
                     ELSE IF (DTYPE.EQ.9) THEN
                        LINE = 'Parallactic angle in degrees'
                        IF ((DPARM(7).GT.0.0) .OR. (DPARM(10).GT.0.0)
     *                     .OR. ((XFACTR.GT.0.0) .AND. (XFACTR.NE.1.0)))
     *                     WRITE (LINE,1109) 'Parallactic angle',
     *                     SFACTI * RAD2DG
                     ELSE IF (DTYPE.EQ.10) THEN
                        WRITE (LINE,1114) 'System', SFACTI
                     ELSE IF (DTYPE.EQ.13) THEN
                        WRITE (LINE,1114) 'Antenna', SFACTI
                     ELSE IF (DTYPE.EQ.11) THEN
                        LINE = 'Source elevation in degrees'
                        IF ((DPARM(7).GT.0.0) .OR. (DPARM(10).GT.0.0)
     *                     .OR. ((XFACTR.GT.0.0) .AND. (XFACTR.NE.1.0)))
     *                     WRITE (LINE,1109) 'Source elevation',
     *                     SFACTI * RAD2DG
                     ELSE IF (DTYPE.EQ.12) THEN
                        XTEMP = SFACTI * 1.0E9
                        IF (ABS (XTEMP).GT.1.0E6) XTEMP = 0.0
                        WRITE (LINE,1115) XTEMP
                     ELSE IF (DTYPE.EQ.15) THEN
                        WRITE (LINE,1114) 'P system', SFACTI
                     ELSE IF (DTYPE.EQ.16) THEN
                        WRITE (LINE,1118) SFACTI
                     ELSE IF (DTYPE.EQ.17) THEN
                        WRITE (LINE,1119) 'Pdif', SFACTI
                     ELSE IF (DTYPE.EQ.18) THEN
                        WRITE (LINE,1119) 'Psum', SFACTI
                     ELSE IF (DTYPE.EQ.19) THEN
                        WRITE (LINE,1119) 'Post Detector Gain', SFACTI
                        END IF
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
C                                          Section label
                     IF (DOCRT.GT.-2.5) THEN
                        LINE = ' '
                        CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                     TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                        IF (IERR.NE.0) GO TO 950
                        END IF
                     TITL2 = 'Time/Antennas'
                     IF (DPARM(9).GT.0.0) TITL2(1:4) = 'LST '
                     COLPNT = 15
                     DO 150 ICOL = 1,NUMPRT
                        WRITE (ENTRY,1120) ANTLAB(IOFF+ICOL)
                        TITL2(COLPNT:COLPNT+NCOLPV-1) =
     *                     ENTRY(11-NCOLPV:10)
                        COLPNT = COLPNT + NCOLPV
 150                    CONTINUE
                     IF (((IPCNT.GT.3) .AND. (IPCNT.LT.PRTMAX-1))
     *                  .OR. (DOCRT.LE.-2.5)) THEN
                        CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                     TITL2, TITL2, IPCNT, PAGE, SCRTCH, IERR)
                        IF (IERR.NE.0) GO TO 950
                        END IF
                     END IF
                  LCOUNT = LCOUNT + 1
                  IF (MOD(LCOUNT,IXINC).EQ.0) THEN
                     SFACOL = SFACT
                     WRITE (LINE,1150) (TIME(ICOL), ICOL = 1,3), TSEC
                     IF (LINE(10:10).EQ.' ') LINE(10:10) = '0'
                     COLPNT = 15
                     ITEMP = 10 - NCOLPV + 1
                     DO 200 ICOL = 1,NUMPRT
                        IF (SCANV(ICOL).NE.FBLANK) THEN
                           XROUND = 0.5
                           IF (SCANV(ICOL).LT.0.0) XROUND = -0.5
                           I4TEMP = SFACT * SCANV(ICOL) + XROUND
                           IF (I4TEMP.GT.NDIGIT) THEN
                              ENTRY = '**********'
                           ELSE IF (I4TEMP.LT.MDIGIT) THEN
                              ENTRY = '----------'
                           ELSE
                              WRITE (ENTRY,1151) I4TEMP
                              END IF
                        ELSE
                           ENTRY = ' '
                           END IF
                        LINE(COLPNT:COLPNT+NCOLPV-1) = ENTRY(ITEMP:10)
                        COLPNT = COLPNT + NCOLPV
 200                    CONTINUE
C                                          Write row
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
C                                          If more - loop
                  IF (.NOT.DONE) GO TO 100
 600           CONTINUE
C                                          New page if necessary
            IF ((BIF.NE.EIF) .OR. (JPOL.NE.MCOR) .OR. (KPASS.NE.LPASS))
     *         THEN
               IF (IPCNT.GT.(PRTMAX+1)/2) THEN
                  LINE = ' '
                  TITL1 = ' '
                  TITL2 = ' '
                  IPCNT = 998
                  END IF
               END IF
C                                          End of polarization loop
 640        CONTINUE
C                                       End of amp/phase loop
 700     CONTINUE
      IRET = 0
      GO TO 999
C                                       CRT error
 950  IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1950) IERR
         CALL MSGWRT (8)
         IRET = 1
      ELSE
         IRET = -1
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('GAINUV: ERROR ',I3,' READING ANTENNA FILE')
 1020 FORMAT ('File = ',A12,'.',A6,'.',I4,' Vol =',I2,'  Userid =',I5,
     *   3X,' IF =',I3)
 1021 FORMAT ('Freq=',F13.9,' GHz   Ncor=',I3,'   No. vis=',I10)
 1022 FORMAT ('Polarization = ',A4,' Subarray = ',I3)
 1023 FORMAT ('Listing ',A2,' table, version ',I3)
 1100 FORMAT ('Source=',A16,':',I5.4,',  Stokes=',A4,', IF=',I2)
 1101 FORMAT ('Flux =',F8.4,' Jy, Calcode = ',A4,', Freq =',F13.9,
     *   ' GHz')
 1109 FORMAT (A,', 1000 =',F13.6,' degrees')
 1110 FORMAT ('Gain amplitudes, 1000 =',F13.6)
 1111 FORMAT ('Residual delay, 1000 =',F12.3,' nanoseconds')
 1112 FORMAT ('Residual rate, 1000 =',F12.3,' mHz')
 1113 FORMAT ('Solution SNR, 1000 =',F9.1)
 1114 FORMAT (A,' temperature, 1000 =',F10.1,' Kelvins')
 1115 FORMAT ('Multiband delay, 1000 =',F12.3,' nanoseconds')
 1116 FORMAT ('Gain amp -> Eff Tsys, 1000 =',F14.4,' Kelvins')
 1117 FORMAT ('Gain amp -> SEFD, 1000 =',F14.4,' Janskys')
 1118 FORMAT ('Pdif Gain amplitudes, 1000 =',F13.6)
 1119 FORMAT (A,', 1000 =',F13.6)
 1120 FORMAT ('--------',I2.2)
 1150 FORMAT (I2,'/',2(I2.2,':'),F4.1)
 1151 FORMAT (I10)
 1950 FORMAT ('GAINUV: ERROR',I5,' DOING I/O TO TERMINAL')
      END
      SUBROUTINE GAICH1 (IRET)
C-----------------------------------------------------------------------
C   Counts lines of column listing of a table
C   looks like the old DEC-10/ISIS format, this is the default routine.
C   Output:
C      IRET   I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER   KEYS(7)*48, CCODE*1, DATKEY(20)*48, UTYPE*2, TYPKEY*48
      INTEGER   TIME(8), DTYPE, NPASS, IPASS, NCOLPV, SKEY(2,2), L,
     *   MCOR, NUMPRT, IOFF, IIPOL, JPOL, LUN, NUMNOD, COLS(7),
     *   TIMKOL, SUBKOL, ANTKOL, SOUKOL, KOL, KOL2, KSTOK, RECNO, IDAY,
     *   LPASS, KPASS, NDIGIT, L2, COLS2(7), MDIGIT, FQKOL, KEYSUB(2,2),
     *   LCOUNT, CATSAV(256), TYPKOL
      LOGICAL   T, F, DONE, NUSCAN, ISAPPL, DONHDR, AMPPHS
      REAL      CATR(256), SFACT, FSFACT, SFACTI, FKEY(2,2), SMAX, DT,
     *   SFACOL, FREQX, TSEC
      HOLLERITH CATH(256)
      DOUBLE PRECISION    CATD(128), RANOD(25), DECNOD(25)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANTLAB(MAXANT), NTERM, KOLS(MAXCLC), NUMV(MAXCLC),
     *   NKEY, NREC, NCOL,  DATP(128,2), NUMKEY
      REAL      SCANV(MAXANT)
      INCLUDE 'LISTR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB /4*1/
      DATA KEYS /'TIME', 'SUBARRAY', 'ANTENNA NO.', 'SOURCE ID',
     *   'FREQ ID', ' ', ' '/
      DATA TYPKEY /'CAL TYPE'/
      DATA DATKEY /'REAL1', 'WEIGHT 1', 'RATE 1','DELAY 1','TSYS 1',
     *             'MBDELAY1','TANT 1', 'POWER DIF1', 'POWER SUM1',
     *             'POST GAIN1',
     *             'REAL2', 'WEIGHT 2', 'RATE 2','DELAY 2','TSYS 2',
     *             'MBDELAY2','TANT 2', 'POWER DIF2', 'POWER SUM2',
     *             'POST GAIN2'/
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN /30/
C-----------------------------------------------------------------------
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
C                                       Setup
      CALL H2CHR (8, 1, CATH(KHOBJ), SNAME)
      QUAL = 0
      CALCOD = ' '
      SFACT = 1.0
      CURSOU = -1
      DTYPE = DPARM(1) + 0.5
C                                       Get antenna info for
C                                       parallactic angle or elevation
      IF ((DTYPE.EQ.9) .OR. (DTYPE.EQ.11)) THEN
         CALL GETANT (DISKIN, CNOIN, SUBARR, CATBLK, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010)
            GO TO 990
            END IF
         END IF
C                                       Initialize source tables
C                                       Timerange
      IF (TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4).EQ.0.0)
     *   TIMRNG(1) = -1.0E6
      IF (TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8).EQ.0.0)
     *   TIMRNG(5) = 1.0E6
C                                       Set time range.
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      IUDISK = DISKIN
      IUCNO = CNOIN
      IULUN = 25
      IXLUN = 28
      ICLUN = 29
      IFLUN = 30
      CALL COPY (256, CATBLK, CATUV)
      KLOCSU = ILOCSU
      CALL SOUFIL (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Only allow amp, phase,delay,rate
C                                       SNR, par. ang.,Tsys, elev. or
C                                       amp/phase, mb delay
C                                       anything else = phase
      IF ((DTYPE.NE.0) .AND. (DTYPE.NE.1) .AND. (DTYPE.NE.5) .AND.
     *   (DTYPE.NE.6) .AND. (DTYPE.NE.7) .AND. (DTYPE.NE.8) .AND.
     *   (DTYPE.NE.9) .AND. (DTYPE.NE.10) .AND. (DTYPE.NE.11) .AND.
     *   (DTYPE.NE.12) .AND. (DTYPE.NE.13) .AND. (DTYPE.NE.15) .AND.
     *   (DTYPE.NE.16) .AND. (DTYPE.NE.17) .AND. (DTYPE.NE.18) .AND.
     *   (DTYPE.NE.19)) DTYPE=1
      LPASS = 1
      IF (DTYPE.EQ.5) LPASS = 2
      AMPPHS = DTYPE.EQ.5
C                                       Only allow R/L for amp, phase
      IF ((DTYPE.GT.1) .AND. (STOKES.EQ.'POLD')) STOKES = ' '
C                                       Number of char per col.
      NCOLPV = 4
      IF ((DPARM(3).GT.3.6) .AND. (DPARM(3).LT.10.4))
     *   NCOLPV = DPARM(3) + 0.5
      NDIGIT = (10**NCOLPV) - 1
      MDIGIT = (10**(NCOLPV-1)) - 1
      MDIGIT = -MDIGIT
      CALL COPY (256, CATBLK, CATSAV)
C                                       Setup for col. listing.
      CALL GANSET (ANTLAB, IIPOL, IRET)
      IF (IRET.NE.0) GO TO 999
      FREQ = BFREQ * 1.0D-9
C                                       Determine number to print
      NUMPRT = (NACROS-18) / NCOLPV
      IF (NUMPRT.GT.NANTSL) NUMPRT = NANTSL
      NPASS = ((NANTSL*1.0) / NUMPRT) + 0.999
      MCOR = 1
      IF (IIPOL.EQ.3) MCOR = 2
      IF (STOKES.EQ.'POLD') MCOR = 1
      IF ((DTYPE.EQ.15) .OR. (DTYPE.EQ.16)) THEN
         NUMKEY = 7
      ELSE
         NUMKEY = 6
         END IF
C                                       Loop over amp/phase listings
      DO 700 KPASS = 1,LPASS
         NUSCAN = .FALSE.
         IF (AMPPHS .AND. (KPASS.EQ.1)) DTYPE = 0
         IF (AMPPHS .AND. (KPASS.EQ.2)) DTYPE = 1
C                                       Only One pass for multiband
C                                       delay.
         IF (DTYPE.EQ.12) BIF = 1
         LCOUNT = -1
C                                       Polarization loop
         DO 640 JPOL = 1,MCOR
            DONHDR = F
            IDAY = -1
            KSTOK = 1
            IF ((JPOL.EQ.2) .OR. (ICOR0.EQ.-2)) KSTOK = 2
            IF ((JPOL.EQ.2) .OR. (ICOR0.EQ.-6)) KSTOK = 2
            IF ((IIPOL.EQ.2) .OR. (JPOL.EQ.2)) KSTOK = 2
            IF (STOKES.EQ.'POLD') KSTOK = 3
            IF (ICOR0.LE.-5) KSTOK = KSTOK + 3
C                                       Start outer loop
            DO 600 IPASS = 1,NPASS
               NUMPRT = (NACROS-20) / NCOLPV
               IOFF = (IPASS-1) * NUMPRT
               IF ((IOFF+NUMPRT).GT.NANTSL) NUMPRT = NANTSL - IOFF
               CURSOU = -1
               DONHDR = F
C                                       Open table
               IRET = 0
C                                       Use TABINI for column
C                                       pointer array
               NKEY = 0
               NREC = 0
               NCOL = 0
               CALL TABINI ('READ', INEXT, DISKIN, CNOIN, INVER,
     *            CATBLK, LUN, NKEY, NREC, NCOL, DATP, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Close
               CALL TABIO ('CLOS', 0, RECNO, BUFFER, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Re-open
               GMMOD = 1.0
               IF (INEXT.EQ.'CL') THEN
                  CALL CALINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *               NUMIF, NTERM, GMMOD, IRET)
               ELSE IF (INEXT.EQ.'SN') THEN
                  CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *               NUMIF, NUMNOD, GMMOD, RANOD, DECNOD, ISAPPL, IRET)
               ELSE IF (INEXT.EQ.'TY') THEN
                  CALL TYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMPOL, NUMIF,
     *               IRET)
               ELSE IF (INEXT.EQ.'SY') THEN
                  CALL SYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *               NUMIF, IRET)
               ELSE
                  IRET = 10
                  END IF
               IF (IRET.NE.0) GO TO 999
               IF (GMMOD.LE.0.0) GMMOD = 1.0
C                                       Find column numbers
               IF ((DTYPE.EQ.0) .OR. (DTYPE.EQ.1)) L = 1
               IF (DTYPE.EQ.9) L = 1
               IF (DTYPE.EQ.8) L = 2
               IF (DTYPE.EQ.7) L = 3
               IF (DTYPE.EQ.6) L = 4
               IF (DTYPE.EQ.10) L = 5
               IF (DTYPE.EQ.13) L = 7
               IF (DTYPE.EQ.11) L = 1
               IF (DTYPE.EQ.12) L = 6
               IF (DTYPE.EQ.15) L = 8
               IF (DTYPE.EQ.16) L = 8
               IF (DTYPE.EQ.17) L = 8
               IF (DTYPE.EQ.18) L = 9
               IF (DTYPE.EQ.19) L = 10
C                                       Correct for polarization
               IF (((IIPOL.EQ.3) .AND. (JPOL.EQ.2)) .OR.
     *            ((IIPOL.EQ.2) .AND.
     *            ((ICOR0.EQ.-1) .OR. (ICOR0.EQ.-5)))) L = L + 10
               L2 = L
               KEYS(6) = DATKEY(L)
               IF (NUMKEY.EQ.7) KEYS(7) = DATKEY(L+1)
               IF ((DTYPE.EQ.15) .OR. (DTYPE.EQ.16)) THEN
                  CALL FNDCOL (1, TYPKEY, 24, T, BUFFER, TYPKOL, IRET)
                  IF ((IRET.NE.0) .AND. (IRET.LE.10)) GO TO 999
                  IF (IRET.GT.10) TYPKOL = -1
                  END IF
               CALL FNDCOL (NUMKEY, KEYS, 24, T, BUFFER, COLS, IRET)
               IF (IRET.NE.0) GO TO 999
               L = COLS(1)
               TIMKOL = DATP(L,1)
               L = COLS(2)
               SUBKOL = DATP(L,1)
               L = COLS(3)
               ANTKOL = DATP(L,1)
               L = COLS(4)
               SOUKOL = DATP(L,1)
               L = COLS(5)
               FQKOL = DATP(L,1)
               L = COLS(6)
               KOL = DATP(L,1)
               KOL2 = 0
               IF (NUMKEY.EQ.7) THEN
                  L = COLS(7)
                  KOL2 = DATP(L,1)
                  END IF
               IF (STOKES.EQ.'POLD') THEN
                  KEYS(6) = DATKEY(L2 + 7)
                  CALL FNDCOL (6, KEYS, 24, T, BUFFER, COLS2, IRET)
                  IF (IRET.NE.0) GO TO 999
                  L = COLS2(6)
                  KOL2 = DATP(L,1)
                  END IF
C                                       Correct for IF
C                                       Except for mb delay
               IF (DTYPE.NE.12) THEN
                  KOL = KOL + (BIF - 1)
                  IF (KOL2.GT.0) KOL2 = KOL2 + (BIF - 1)
                  END IF
C                                       Resort if necessary.
               IF (BUFFER(43).NE.TIMKOL) THEN
C                                       Sort to time order.
                  CALL TABIO ('CLOS', 0, RECNO, BUFFER, BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 999
C                                       Change catalog status to WRIT
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'CLRD', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'WRIT', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
C                                       Mark as "WRIT" in CFILES
                  FRW(1) = 1
                  SKEY(1,1) = COLS(1)
                  SKEY(2,1) = COLS(1)
                  SKEY(1,2) = COLS(3)
                  SKEY(2,2) = COLS(3)
                  CALL TABSRT (DISKIN, CNOIN, INEXT, INVER, INVER, SKEY,
     *               KEYSUB, FKEY, BUFFER, CATBLK, IRET)
                  IF (IRET.NE.0) GO TO 999
C                                       Change status back to READ
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'CLWR', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'READ', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
                  IRET = 0
C                                       Mark as "READ" in CFILES
                  FRW(1) = 0
C                                       Re initialize.
                  GMMOD = 1.0
                  IF (INEXT.EQ.'CL') THEN
                     CALL CALINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *                  NUMIF, NTERM, GMMOD, IRET)
                  ELSE IF (INEXT.EQ.'SN') THEN
                     CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *                  NUMIF, NUMNOD, GMMOD, RANOD, DECNOD, ISAPPL,
     *                  IRET)
                  ELSE IF (INEXT.EQ.'TY') THEN
                     CALL TYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMPOL, NUMIF,
     *                  IRET)
                  ELSE
                     CALL SYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *                  NUMIF, IRET)
                     END IF
                  IF (IRET.NE.0) GO TO 999
                  IF (GMMOD.LE.0.0) GMMOD = 1.0
                  END IF
C                                       Determine scaling
               FREQX = FREQ * 1.0D9 + FREQO(BIF) + FREQIF(BIF)
               SFACOL = 0.0
               SFACT = 1.0E6
               CALL GANSCL (OPTYPE, NUMPRT, ANTLAB(IOFF+1), NCOLPV,
     *            BUFFER, TIMKOL, SOUKOL, SUBKOL, ANTKOL, KOL, DTYPE,
     *            FREQX, SFACT, SMAX, IRET)
               IF (IRET.GT.0) GO TO 999
               SFACT = SFACT / 10.
               IF ((DTYPE.EQ.1) .OR. (DTYPE.EQ.9) .OR.
     *            (DTYPE.EQ.11)) THEN
                  IF ((DPARM(10).GT.0.0) .AND. (KPASS.EQ.1)) THEN
                     SFACT = RAD2DG / DPARM(10)
                  ELSE IF (DPARM(7).LE.0.0) THEN
                     SFACT = RAD2DG
                     END IF
               ELSE IF (DPARM(10).GT.0.0) THEN
                  SFACT = 1.0 / DPARM(10)
                  END IF
               IF ((XFACTR.GT.0.0) .AND. (DPARM(10).LE.0.0)) SFACT =
     *            SFACT / XFACTR
               SFACTI = 1000.0 / SFACT
C                                       first page titles
               IF ((KPASS.EQ.1) .OR. (DOCRT.GT.-2.5)) NCOUNT = NCOUNT+1
               IF (DOCRT.GT.-2.5) THEN
                  NCOUNT = NCOUNT + 3
                  IF (INEXT.EQ.'SN') NCOUNT = NCOUNT + 1
                  END IF
C                                       Use 0.01 sec tolerance
               DT = 0.01 / 86400.0
C                                       While more data - Loop
 100           CONTINUE
                  CALL GAINAV (OPTYPE, NUMPRT, ANTLAB(IOFF+1), BUFFER,
     *               TIMKOL, SOUKOL, SUBKOL, ANTKOL, FQKOL, KOL, KOL2,
     *               TYPKOL, DTYPE, JPOL, BIF, TCAL, SCANV, DT,
     *               DPARM(9), TIME, TSEC, NUSCAN, RECNO, IRET)
                  DONE = IRET.LT.0
C                                       Check if found data
                  IF (IRET.EQ.-2) GO TO 600
                  IF (IRET.GT.0) GO TO 999
C                                       If calc code OK
                  CCODE = CALCOD(:1)
                  IF ((XCALCO(:1).EQ.' ') .OR. (CCODE.EQ.XCALCO(:1))
     *               .OR. ((XCALCO(:1).EQ.'*') .AND. (CCODE.NE.' '))
     *               .OR. ((XCALCO(:1).EQ.'-') .AND. (CCODE.EQ.' ')))
     *               THEN
                     IF (NUSCAN .OR. SMAX.EQ.0.0) THEN
                        DONHDR = F
                        FREQX = FREQ * 1.0D9 + FREQO(BIF) +
     *                     FREQIF(BIF)
                        IF (NUSCAN) LCOUNT = -1
                        END IF
C                                       Header for scan (2 blank lines
C                                       FIRST)
                     IF (.NOT.DONHDR) THEN
                        DONHDR = T
                        IF (SFACT.EQ.SFACOL) NUSCAN = .FALSE.
                        IF ((NUSCAN) .AND. (DOCRT.GT.-2.5)) NCOUNT =
     *                     NCOUNT + 1
C                                       Type of data listed
C                                       1=amp, 2=phase, 6=delay, 7=rate
C                                       8 = SNR, 9=Par. ang., 10 = Tsys
C                                       11 = elev. 12=mb delay
                        IF (SFACT.NE.SFACOL) THEN
C                                       Save multiplication factor
                           FSFACT = SFACT
                           IF ((NUSCAN) .OR. (SMAX.NE.0.0)) THEN
                              NUSCAN = .FALSE.
C                                       Source info
                              NCOUNT = NCOUNT + 1
                              IF (DOCRT.GT.-2.5) NCOUNT= NCOUNT + 2
                              IF ((DOCRT.LE.-2.5) .OR. ((IPCNT.GT.3)
     *                           .AND. (IPCNT.LT.PRTMAX-1))) NCOUNT =
     *                           NCOUNT + 1
                              END IF
                           END IF
                        END IF
                     LCOUNT = LCOUNT + 1
                     IF (MOD(LCOUNT,IXINC).EQ.0) THEN
                        SFACOL = SFACT
C                                       Print day number if have to
                        IF (TIME(1).NE.IDAY) THEN
                           IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
                           NCOUNT = NCOUNT + 1
                           END IF
C                                       Write row
                        NCOUNT = NCOUNT + 1
                        END IF
C                                       End if CalCode OK
                     END IF
C                                       End while more data - loop
                  IF (.NOT.DONE) GO TO 100
C                                       end pass loop
 600           CONTINUE
C                                       End of polarization loop
 640        CONTINUE
C                                       End of amp/phase loop
 700     CONTINUE
      IRET = 0
      CALL COPY (256, CATSAV, CATBLK)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('GAICH1: ERROR ',I3,' READING ANTENNA FILE')
      END
      SUBROUTINE GAIUV1 (IRET)
C-----------------------------------------------------------------------
C   Gives column listing of an SN or CL table (multi or single source).
C   looks like the old DEC-10/ISIS format, this is the default routine.
C   Output:
C      IRET   I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      CHARACTER   KEYS(7)*48, CCODE*1, STOKX(6)*4, DATKEY(20)*48,
     *   ENTRY*20, UTYPE*2, TYPKEY*48
      INTEGER   TIME(8), DTYPE, NPASS, IPASS, ITEMP, ICOL, NCOLPV, IRET,
     *   COLPNT, SKEY(2,2), L, IERR, MCOR, NUMPRT, IOFF, IIPOL, JPOL,
     *   LUN, NUMNOD, COLS(7), TIMKOL, SUBKOL, ANTKOL, SOUKOL, KOL,
     *   KOL2, KSTOK, I4TEMP, RECNO, IDAY, LPASS, KPASS, NDIGIT, L2,
     *   COLS2(7), MDIGIT, FQKOL, KEYSUB(2,2), LCOUNT, TYPKOL
      LOGICAL   T, F, DONE, NUSCAN, ISAPPL, DONHDR, AMPPHS
      REAL      CATR(256), SFACT, FSFACT, SFACTI, FKEY(2,2), SMAX, DT,
     *   SFACOL, FREQX, XROUND, XTEMP, TSEC
      HOLLERITH CATH(256)
      DOUBLE PRECISION    CATD(128), SFREQ, RANOD(25), DECNOD(25)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANTLAB(MAXANT), NTERM, KOLS(MAXCLC), NUMV(MAXCLC),
     *   NKEY, NREC, NCOL,  DATP(128,2), NUMKEY
      REAL      SCANV(MAXANT)
      INCLUDE 'LISTR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB /4*1/
      DATA STOKX /'R', 'L', 'R-L', 'X', 'Y', 'X-Y'/
      DATA KEYS /'TIME', 'SUBARRAY', 'ANTENNA NO.', 'SOURCE ID',
     *   'FREQ ID', ' ', ' '/
      DATA TYPKEY /'CAL TYPE'/
      DATA DATKEY /'REAL1', 'WEIGHT 1', 'RATE 1','DELAY 1','TSYS 1',
     *             'MBDELAY1','TANT 1', 'POWER DIF1', 'POWER SUM1',
     *             'POST GAIN1',
     *             'REAL2', 'WEIGHT 2', 'RATE 2','DELAY 2','TSYS 2',
     *             'MBDELAY2','TANT 2', 'POWER DIF2', 'POWER SUM2',
     *             'POST GAIN2'/
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN /30/
C-----------------------------------------------------------------------
C                                       Setup
      CALL H2CHR (8, 1, CATH(KHOBJ), SNAME)
      QUAL = 0
      CALCOD = ' '
      SFACT = 1.0
      FLUX(1,BIF) = 0.0
      FLUX(2,BIF) = 0.0
      FLUX(3,BIF) = 0.0
      FLUX(4,BIF) = 0.0
      CURSOU = -1
      DTYPE = DPARM(1) + 0.5
C                                       Get antenna info for
C                                       parallactic angle or elevation
      IF ((DTYPE.EQ.9) .OR. (DTYPE.EQ.11)) THEN
         CALL GETANT (DISKIN, CNOIN, SUBARR, CATBLK, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010)
            GO TO 990
            END IF
         END IF
C                                       Initialize source tables
C                                       Timerange
      IF (TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4).EQ.0.0)
     *   TIMRNG(1) = -1.0E6
      IF (TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8).EQ.0.0)
     *   TIMRNG(5) = 1.0E6
C                                       Set time range.
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      IUDISK = DISKIN
      IUCNO = CNOIN
      IULUN = 25
      IXLUN = 28
      ICLUN = 29
      IFLUN = 30
      CALL COPY (256, CATBLK, CATUV)
      KLOCSU = ILOCSU
      CALL SOUFIL (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Only allow amp, phase,delay,rate
C                                       SNR, par. ang.,Tsys, elev. or
C                                       amp/phase, mb delay
C                                       anything else = phase
      IF ((DTYPE.NE.0) .AND. (DTYPE.NE.1) .AND. (DTYPE.NE.5) .AND.
     *   (DTYPE.NE.6) .AND. (DTYPE.NE.7) .AND. (DTYPE.NE.8) .AND.
     *   (DTYPE.NE.9) .AND. (DTYPE.NE.10) .AND. (DTYPE.NE.11) .AND.
     *   (DTYPE.NE.12) .AND. (DTYPE.NE.13) .AND. (DTYPE.NE.15) .AND.
     *   (DTYPE.NE.16) .AND. (DTYPE.NE.17) .AND. (DTYPE.NE.18) .AND.
     *   (DTYPE.NE.19)) DTYPE=1
      LPASS = 1
      IF (DTYPE.EQ.5) LPASS = 2
      AMPPHS = DTYPE.EQ.5
C                                       Only allow R/L for amp, phase
      IF ((DTYPE.GT.1) .AND. (STOKES.EQ.'POLD')) STOKES = ' '
C                                       Number of char per col.
      NCOLPV = 4
      IF ((DPARM(3).GT.3.6) .AND. (DPARM(3).LT.10.4))
     *   NCOLPV = DPARM(3) + 0.5
      NDIGIT = (10**NCOLPV) - 1
      MDIGIT = (10**(NCOLPV-1)) - 1
      MDIGIT = -MDIGIT
C                                       Setup for col. listing.
      CALL GANSET (ANTLAB, IIPOL, IRET)
      IF (IRET.NE.0) GO TO 999
      FREQ = BFREQ * 1.0D-9
C                                       Determine number to print
      NUMPRT = (NACROS-18) / NCOLPV
      IF (NUMPRT.GT.NANTSL) NUMPRT = NANTSL
      NPASS = ((NANTSL*1.0) / NUMPRT) + 0.999
      MCOR = 1
      IF (IIPOL.EQ.3) MCOR = 2
      IF (STOKES.EQ.'POLD') MCOR = 1
      IF ((DTYPE.EQ.15) .OR. (DTYPE.EQ.16)) THEN
         NUMKEY = 7
      ELSE
         NUMKEY = 6
         END IF
C                                       Loop over amp/phase listings
      DO 700 KPASS = 1,LPASS
         NUSCAN = .FALSE.
         IF (AMPPHS .AND. (KPASS.EQ.1)) DTYPE = 0
         IF (AMPPHS .AND. (KPASS.EQ.2)) DTYPE = 1
C                                       Only One pass for multiband
C                                       delay.
         IF (DTYPE.EQ.12) BIF = 1
         LCOUNT = -1
C                                       Polarization loop
         DO 640 JPOL = 1,MCOR
            DONHDR = F
            IDAY = -1
            KSTOK = 1
            IF ((JPOL.EQ.2) .OR. (ICOR0.EQ.-2)) KSTOK = 2
            IF ((JPOL.EQ.2) .OR. (ICOR0.EQ.-6)) KSTOK = 2
            IF ((IIPOL.EQ.2) .OR. (JPOL.EQ.2)) KSTOK = 2
            IF (STOKES.EQ.'POLD') KSTOK = 3
            IF (ICOR0.LE.-5) KSTOK = KSTOK + 3
C                                       Start outer loop
            DO 600 IPASS = 1,NPASS
               NUMPRT = (NACROS-20) / NCOLPV
               IOFF = (IPASS-1) * NUMPRT
               IF ((IOFF+NUMPRT).GT.NANTSL) NUMPRT = NANTSL - IOFF
               CURSOU = -1
               DONHDR = F
C                                       Open table
               IRET = 0
C                                       Use TABINI for column
C                                       pointer array
               NKEY = 0
               NREC = 0
               NCOL = 0
               CALL TABINI ('READ', INEXT, DISKIN, CNOIN, INVER,
     *            CATBLK, LUN, NKEY, NREC, NCOL, DATP, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Close
               CALL TABIO ('CLOS', 0, RECNO, BUFFER, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Re-open
               GMMOD = 1.0
               IF (INEXT.EQ.'CL') THEN
                  CALL CALINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *               NUMIF, NTERM, GMMOD, IRET)
               ELSE IF (INEXT.EQ.'SN') THEN
                  CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *               NUMIF, NUMNOD, GMMOD, RANOD, DECNOD, ISAPPL, IRET)
               ELSE IF (INEXT.EQ.'TY') THEN
                  CALL TYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMPOL, NUMIF,
     *               IRET)
               ELSE IF (INEXT.EQ.'SY') THEN
                  CALL SYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *               CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *               NUMIF, IRET)
               ELSE
                  IRET = 10
                  END IF
               IF (IRET.NE.0) GO TO 999
               IF (GMMOD.LE.0.0) GMMOD = 1.0
C                                       Find column numbers
               IF ((DTYPE.EQ.0) .OR. (DTYPE.EQ.1)) L = 1
               IF (DTYPE.EQ.9) L = 1
               IF (DTYPE.EQ.8) L = 2
               IF (DTYPE.EQ.7) L = 3
               IF (DTYPE.EQ.6) L = 4
               IF (DTYPE.EQ.10) L = 5
               IF (DTYPE.EQ.13) L = 7
               IF (DTYPE.EQ.11) L = 1
               IF (DTYPE.EQ.12) L = 6
               IF (DTYPE.EQ.15) L = 8
               IF (DTYPE.EQ.16) L = 8
               IF (DTYPE.EQ.17) L = 8
               IF (DTYPE.EQ.18) L = 9
               IF (DTYPE.EQ.19) L = 10
C                                       Correct for polarization
               IF (((IIPOL.EQ.3) .AND. (JPOL.EQ.2)) .OR.
     *            ((IIPOL.EQ.2) .AND.
     *            ((ICOR0.EQ.-1) .OR. (ICOR0.EQ.-5)))) L = L + 10
               L2 = L
               KEYS(6) = DATKEY(L)
               IF (NUMKEY.EQ.7) KEYS(7) = DATKEY(L+1)
               IF ((DTYPE.EQ.15) .OR. (DTYPE.EQ.16)) THEN
                  CALL FNDCOL (1, TYPKEY, 24, T, BUFFER, TYPKOL, IRET)
                  IF ((IRET.NE.0) .AND. (IRET.LE.10)) GO TO 999
                  IF (IRET.GT.10) TYPKOL = -1
                  END IF
               CALL FNDCOL (NUMKEY, KEYS, 24, T, BUFFER, COLS, IRET)
               IF (IRET.NE.0) GO TO 999
               L = COLS(1)
               TIMKOL = DATP(L,1)
               L = COLS(2)
               SUBKOL = DATP(L,1)
               L = COLS(3)
               ANTKOL = DATP(L,1)
               L = COLS(4)
               SOUKOL = DATP(L,1)
               L = COLS(5)
               FQKOL = DATP(L,1)
               L = COLS(6)
               KOL = DATP(L,1)
               KOL2 = 0
               IF (NUMKEY.EQ.7) THEN
                  L = COLS(7)
                  KOL2 = DATP(L,1)
                  END IF
               IF (STOKES.EQ.'POLD') THEN
                  KEYS(6) = DATKEY(L2 + 7)
                  CALL FNDCOL (6, KEYS, 24, T, BUFFER, COLS2, IRET)
                  IF (IRET.NE.0) GO TO 999
                  L = COLS2(6)
                  KOL2 = DATP(L,1)
                  END IF
C                                       Correct for IF
C                                       Except for mb delay
               IF (DTYPE.NE.12) THEN
                  KOL = KOL + (BIF - 1)
                  IF (KOL2.GT.0) KOL2 = KOL2 + (BIF - 1)
                  END IF
C                                       Resort if necessary.
               IF (BUFFER(43).NE.TIMKOL) THEN
C                                       Sort to time order.
                  CALL TABIO ('CLOS', 0, RECNO, BUFFER, BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 999
C                                       Change catalog status to WRIT
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'CLRD', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'WRIT', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
C                                       Mark as "WRIT" in CFILES
                  FRW(1) = 1
                  SKEY(1,1) = COLS(1)
                  SKEY(2,1) = COLS(1)
                  SKEY(1,2) = COLS(3)
                  SKEY(2,2) = COLS(3)
                  CALL TABSRT (DISKIN, CNOIN, INEXT, INVER, INVER, SKEY,
     *               KEYSUB, FKEY, BUFFER, CATBLK, IRET)
                  IF (IRET.NE.0) GO TO 999
C                                       Change status back to READ
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'CLWR', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
                  UTYPE = 'UV'
                  CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN,
     *               SEQIN, UTYPE, NLUSER, 'READ', UBUFF, IRET)
                  IF ((IRET.GE.1) .AND. (IRET.LE.8)) GO TO 999
                  IRET = 0
C                                       Mark as "READ" in CFILES
                  FRW(1) = 0
C                                       Re initialize.
                  GMMOD = 1.0
                  IF (INEXT.EQ.'CL') THEN
                     CALL CALINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *                  NUMIF, NTERM, GMMOD, IRET)
                  ELSE IF (INEXT.EQ.'SN') THEN
                     CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *                  NUMIF, NUMNOD, GMMOD, RANOD, DECNOD, ISAPPL,
     *                  IRET)
                  ELSE IF (INEXT.EQ.'TY') THEN
                     CALL TYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMPOL, NUMIF,
     *                  IRET)
                  ELSE
                     CALL SYINI ('READ', BUFFER, DISKIN, CNOIN, INVER,
     *                  CATBLK, LUN, RECNO, KOLS, NUMV, NUMANT, NUMPOL,
     *                  NUMIF, IRET)
                     END IF
                  IF (IRET.NE.0) GO TO 999
                  IF (GMMOD.LE.0.0) GMMOD = 1.0
                  END IF
C                                       Determine scaling
               FREQX = FREQ * 1.0D9 + FREQO(BIF) + FREQIF(BIF)
               SFACOL = 0.0
               SFACT = 1.0E6
               CALL GANSCL (OPTYPE, NUMPRT, ANTLAB(IOFF+1), NCOLPV,
     *            BUFFER, TIMKOL, SOUKOL, SUBKOL, ANTKOL, KOL, DTYPE,
     *            FREQX, SFACT, SMAX, IRET)
               IF (IRET.GT.0) GO TO 999
               SFACT = SFACT / 10.
               IF ((DTYPE.EQ.1) .OR. (DTYPE.EQ.9) .OR.
     *            (DTYPE.EQ.11)) THEN
                  IF ((DPARM(10).GT.0.0) .AND. (KPASS.EQ.1)) THEN
                     SFACT = RAD2DG / DPARM(10)
                  ELSE IF (DPARM(7).LE.0.0) THEN
                     SFACT = RAD2DG
                     END IF
               ELSE IF (DPARM(10).GT.0.0) THEN
                  SFACT = 1.0 / DPARM(10)
                  END IF
               IF ((XFACTR.GT.0.0) .AND. (DPARM(10).LE.0.0)) SFACT =
     *            SFACT / XFACTR
               SFACTI = 1000.0 / SFACT
C                                       first page titles
               IF ((KPASS.EQ.1) .OR. (DOCRT.GT.-2.5)) THEN
                  WRITE (LINE,1020) NAMEIN, CLAIN, SEQIN, DISKIN,
     *               NLUSER, BIF
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
                  END IF
               IF (DOCRT.GT.-2.5) THEN
                  WRITE (LINE,1021) FREQ, NCOR, NVIS
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
                  WRITE (LINE,1022) STOKX(KSTOK), SUBARR
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
                  WRITE (LINE,1023) INEXT, INVER
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
                  IF (INEXT.EQ.'SN') THEN
                     IF (ISAPPL) THEN
                        LINE = 'SN table has already been applied '
     *                     // 'to a CL table'
                     ELSE
                        LINE = 'SN table has not been applied ' //
     *                     'to a CL table'
                        END IF
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  END IF
C                                       Use 0.01 sec tolerance
               DT = 0.01 / 86400.0
C                                       While more data - Loop
 100           CONTINUE
                  CALL GAINAV (OPTYPE, NUMPRT, ANTLAB(IOFF+1), BUFFER,
     *               TIMKOL, SOUKOL, SUBKOL, ANTKOL, FQKOL, KOL, KOL2,
     *               TYPKOL, DTYPE, JPOL, BIF, TCAL, SCANV, DT,
     *               DPARM(9), TIME, TSEC, NUSCAN, RECNO, IRET)
                  DONE = IRET.LT.0
C                                       Check if found data
                  IF (IRET.EQ.-2) GO TO 600
                  IF (IRET.GT.0) GO TO 999
C                                       If calc code OK
                  CCODE = CALCOD(:1)
                  IF ((XCALCO(:1).EQ.' ') .OR. (CCODE.EQ.XCALCO(:1))
     *               .OR. ((XCALCO(:1).EQ.'*') .AND. (CCODE.NE.' '))
     *               .OR. ((XCALCO(:1).EQ.'-') .AND. (CCODE.EQ.' ')))
     *               THEN
                     IF (NUSCAN .OR. SMAX.EQ.0.0) THEN
                        DONHDR = F
                        FREQX = FREQ * 1.0D9 + FREQO(BIF) +
     *                     FREQIF(BIF)
                        IF (NUSCAN) LCOUNT = -1
                        END IF
C                                       Header for scan (2 blank lines
C                                       FIRST)
                     IF (.NOT.DONHDR) THEN
                        DONHDR = T
                        IF (SFACT.EQ.SFACOL) NUSCAN = .FALSE.
                        IF ((NUSCAN) .AND. (DOCRT.GT.-2.5)) THEN
                           LINE = ' '
                           CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS,
     *                        TITL1, TITL2, LINE, IPCNT, PAGE, SCRTCH,
     *                        IERR)
                           IF (IERR.NE.0) GO TO 950
                           END IF
C                                       Type of data listed
C                                       1=amp, 2=phase, 6=delay, 7=rate
C                                       8 = SNR, 9=Par. ang., 10 = Tsys
C                                       11 = elev. 12=mb delay
                        IF (SFACT.NE.SFACOL) THEN
                           IF (DTYPE.EQ.0) THEN
                              WRITE (LINE,1100) SFACTI
                              IF (OPTYPE.EQ.'EFST') WRITE (LINE,1106)
     *                           SFACTI
                              IF (OPTYPE.EQ.'SEFD') WRITE (LINE,1107)
     *                           SFACTI
                           ELSE IF (DTYPE.EQ.1) THEN
                              LINE = 'Gain phases in degrees'
                              IF ((DPARM(7).GT.0.0) .OR.
     *                           (DPARM(10).GT.0.0) .OR.
     *                           ((XFACTR.GT.0.0) .AND.
     *                           (XFACTR.NE.1.0))) WRITE (LINE,1108)
     *                           'Gain phases', SFACTI * RAD2DG
                           ELSE IF (DTYPE.EQ.6) THEN
                              XTEMP = SFACTI * 1.0E9
                              IF (ABS (XTEMP).GT.1.0E6) XTEMP = 0.0
                              WRITE (LINE,1101) XTEMP
                           ELSE IF (DTYPE.EQ.7) THEN
                              XTEMP = SFACTI * 1000.0 * FREQX
                              IF (ABS (XTEMP).GT.1.0E6) XTEMP = 0.0
                              WRITE (LINE,1102) XTEMP
                              END IF
                           IF (ABS (SFACTI).GT.1.0E6) SFACTI = 0.0
                           IF (DTYPE.EQ.8) THEN
                              WRITE (LINE,1103) SFACTI
                           ELSE IF (DTYPE.EQ.9) THEN
                              LINE = 'Parallactic angle in degrees'
                              IF ((DPARM(7).GT.0.0) .OR.
     *                           (DPARM(10).GT.0.0) .OR.
     *                           ((XFACTR.GT.0.0) .AND.
     *                           (XFACTR.NE.1.0))) WRITE (LINE,1108)
     *                           'Parallactic angle', SFACTI * RAD2DG
                           ELSE IF (DTYPE.EQ.10) THEN
                              WRITE (LINE,1104) 'System', SFACTI
                           ELSE IF (DTYPE.EQ.13) THEN
                              WRITE (LINE,1104) 'Antenna', SFACTI
                           ELSE IF (DTYPE.EQ.11) THEN
                              LINE = 'Source elevation in degrees'
                              IF ((DPARM(7).GT.0.0) .OR.
     *                           (DPARM(10).GT.0.0) .OR.
     *                           ((XFACTR.GT.0.0) .AND.
     *                           (XFACTR.NE.1.0))) WRITE (LINE,1108)
     *                           'Source elevation', SFACTI * RAD2DG
                           ELSE IF (DTYPE.EQ.12) THEN
                              XTEMP = SFACTI * 1.0E9
                              IF (ABS (XTEMP).GT.1.0E6) XTEMP = 0.0
                              WRITE (LINE,1105) XTEMP
                           ELSE IF (DTYPE.EQ.15) THEN
                              WRITE (LINE,1104) 'P system', SFACTI
                           ELSE IF (DTYPE.EQ.16) THEN
                              WRITE (LINE,1109) SFACTI
                           ELSE IF (DTYPE.EQ.17) THEN
                              WRITE (LINE,1119) 'Pdif', SFACTI
                           ELSE IF (DTYPE.EQ.18) THEN
                              WRITE (LINE,1119) 'Psum', SFACTI
                           ELSE IF (DTYPE.EQ.19) THEN
                              WRITE (LINE,1119) 'Post Detector gain',
     *                           SFACTI
                              END IF
C                                       Save multiplication factor
                           FSFACT = SFACT
                           IF ((NUSCAN) .OR. (SMAX.NE.0.0)) THEN
                              NUSCAN = .FALSE.
C                                       Source info
                              CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS,
     *                           TITL1, TITL2, LINE, IPCNT, PAGE,
     *                           SCRTCH, IERR)
                              IF (IERR.NE.0) GO TO 950
                              IF (DOCRT.GT.-2.5) THEN
                                 SFREQ = ((FREQO(BIF) + FREQIF(BIF))
     *                              * 1.0D-9) + FREQ
                                 WRITE (LINE,1110) STOKX(KSTOK), BIF,
     *                              SFREQ
                                 CALL PRTLIN (LUNP, FINDP, DOCRT,
     *                              NACROS, TITL1, TITL2, LINE, IPCNT,
     *                              PAGE, SCRTCH, IERR)
                                 IF (IERR.NE.0) GO TO 950
C                                       Section label
                                 LINE = ' '
                                 CALL PRTLIN (LUNP, FINDP, DOCRT,
     *                              NACROS, TITL1, TITL2, LINE, IPCNT,
     *                              PAGE, SCRTCH, IERR)
                                 IF (IERR.NE.0) GO TO 950
                                 END IF
                              TITL2 = '  Time   Source '
                              IF (DPARM(9).GT.0.0) TITL2(3:6) = 'LST '
                              COLPNT = 20
                              DO 150 ICOL = 1,NUMPRT
                                 WRITE (ENTRY,1111) ANTLAB(IOFF+ICOL)
                                 TITL2(COLPNT:COLPNT+NCOLPV-1) =
     *                              ENTRY(11-NCOLPV:10)
                                 COLPNT = COLPNT + NCOLPV
 150                             CONTINUE
                              IF ((DOCRT.LE.-2.5) .OR. ((IPCNT.GT.3)
     *                           .AND. (IPCNT.LT.PRTMAX-1))) THEN
                                 CALL PRTLIN (LUNP, FINDP, DOCRT,
     *                              NACROS, TITL1, TITL2, TITL2, IPCNT,
     *                              PAGE, SCRTCH, IERR)
                                 IF (IERR.NE.0) GO TO 950
                                 END IF
                              END IF
                           END IF
                        END IF
                     LCOUNT = LCOUNT + 1
                     IF (MOD(LCOUNT,IXINC).EQ.0) THEN
                        SFACOL = SFACT
C                                       Print day number if have to
                        IF (TIME(1).NE.IDAY) THEN
                           IF (DOCRT.GT.-2.5) THEN
                              LINE = ' '
                              CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS,
     *                           TITL1, TITL2, LINE, IPCNT, PAGE,
     *                           SCRTCH, IERR)
                              IF (IERR.NE.0) GO TO 950
                              END IF
                           IDAY = TIME(1)
                           WRITE (LINE,1150) IDAY
                           CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS,
     *                        TITL1, TITL2, LINE, IPCNT, PAGE, SCRTCH,
     *                        IERR)
                           IF (IERR.NE.0) GO TO 950
                           END IF
C                                       Time and source name
                        WRITE (LINE,1151) TIME(2), TIME(3), TSEC,
     *                     SNAME(:8)
                        IF (LINE(7:7).EQ.' ') LINE(7:7) = '0'
                        COLPNT = 20
C                                       Values
                        ITEMP = 10 - NCOLPV + 1
                        DO 200 ICOL = 1,NUMPRT
                           IF (SCANV(ICOL).NE.FBLANK) THEN
                              XROUND = 0.5
                              IF (SCANV(ICOL).LT.0.0) XROUND = -0.5
                              I4TEMP = FSFACT * SCANV(ICOL) + XROUND
                              IF (I4TEMP.GT.NDIGIT) THEN
                                 ENTRY = '**********'
                              ELSE IF (I4TEMP.LT.MDIGIT) THEN
                                 ENTRY = '----------'
                              ELSE
                                 WRITE (ENTRY,1152) I4TEMP
                                 END IF
                           ELSE
                              ENTRY = ' '
                              END IF
                           LINE(COLPNT:COLPNT+NCOLPV-1) =
     *                        ENTRY(ITEMP:10)
                           COLPNT = COLPNT + NCOLPV
 200                       CONTINUE
C                                       Write row
                        CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                     TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                        IF (IERR.NE.0) GO TO 950
                        END IF
C                                       End if CalCode OK
                     END IF
C                                       End while more data - loop
                  IF (.NOT.DONE) GO TO 100
C                                       end pass loop
 600           CONTINUE
C                                       New page if necessary
            IF ((BIF.NE.EIF) .OR. (JPOL.NE.MCOR) .OR. (KPASS.NE.LPASS))
     *         THEN
               IF (IPCNT.GT.(PRTMAX+1)/2) THEN
                  LINE = ' '
                  TITL1 = ' '
                  TITL2 = ' '
                  IPCNT = 998
                  END IF
               END IF
C                                       End of polarization loop
 640        CONTINUE
C                                       End of amp/phase loop
 700     CONTINUE
      IRET = 0
      GO TO 999
C                                    CRT error
 950  IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1950) IERR
         CALL MSGWRT (8)
         IRET = 1
      ELSE
         IRET = -1
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('GAIUV1: ERROR ',I3,' READING ANTENNA FILE')
 1020 FORMAT ('File = ',A12,'.',A6,'.',I4,' Vol =',I2,'  Userid =',I5,
     *   3X,' IF =',I3)
 1021 FORMAT ('Freq=',F13.9,' GHz   Ncor=',I3,'   No. vis=',I10)
 1022 FORMAT ('Polarization = ',A4,' Subarray = ',I3)
 1023 FORMAT ('Listing ',A2,' table, version ',I3)
 1100 FORMAT ('Gain amplitudes, 1000 =',F13.6)
 1101 FORMAT ('Residual delay, 1000 =',F12.3,' nanoseconds')
 1102 FORMAT ('Residual rate, 1000 =',F12.3,' mHz')
 1103 FORMAT ('Solution SNR, 1000 = ',F9.1)
 1104 FORMAT (A,' temperature, 1000 = ',F9.1,' Kelvins')
 1105 FORMAT ('Multiband delay, 1000 =',F12.3,' nanoseconds')
 1106 FORMAT ('Gain amp -> Eff Tsys, 1000 =',F14.4,' Kelvins')
 1107 FORMAT ('Gain amp -> SEFD, 1000 =',F14.4,' Janskys')
 1108 FORMAT (A,', 1000 =',F13.6,' degrees')
 1109 FORMAT ('Pdif Gain amplitudes, 1000 =',F13.6)
 1110 FORMAT ('Stokes = ',A4,' IF = ',I2,' Freq =',F13.9,' GHz')
 1111 FORMAT ('--------',I2.2)
 1119 FORMAT (A,', 1000 =',F13.6)
 1150 FORMAT ('Day # ',I3)
 1151 FORMAT (2(I2.2,':'),F4.1,1X,A8)
 1152 FORMAT (I10)
 1950 FORMAT ('GAIUV1: ERROR',I5,' DOING I/O TO TERMINAL')
      END
      SUBROUTINE GAINMX (SCANV, NUMPRT, DTYPE, NCOLPV, FREQ, SMAX,
     *   SFACT)
C-----------------------------------------------------------------------
C   Routine to find the maximum, non blank value in an array and
C   determine the proper scaling factor for printing.
C   Tries to keep the scaling factor from changing unnecessarily.
C   Delays will be scaled to nanosec, and rates to mHz.
C   Inputs:
C      SCANV    R(maxant)   Scan values.
C      NANT     I           Max. antenns number in scan.
C      DTYPE    I           Data type, 0=amp, 1=phase, 6=delay, 7=rate,
C                           8 = SNR, 10 = Tsys, 11 = elevation
C      NCOLPV   I           Number of columns per value printed
C      FREQ     R           Frequency (Hz)
C   Input/output:
C      SMAX     R           Maximum abs value.
C      SFACT    R           Scaling factor to print values.
C-----------------------------------------------------------------------
      INTEGER   NUMPRT, DTYPE, NCOLPV, I
      REAL      SMAX, SFACT, FREQ, VALUE, RATFAC, TSMAX, TSFACT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      REAL      SCANV(MAXANT)
C-----------------------------------------------------------------------
C                                       Rate conversion factor
      RATFAC = 0.0
      IF (DTYPE.EQ.7) RATFAC = 1000.0 * FREQ
C                                       Find maximum non blank
      TSMAX = 0.0
      IF (NUMPRT.GT.0) THEN
         DO 100 I = 1,NUMPRT
            IF (SCANV(I).NE.FBLANK) TSMAX = MAX (TSMAX, ABS (SCANV(I)))
 100        CONTINUE
         END IF
C                                       Determine scaling
      IF (TSMAX.GT.1.0E-20) THEN
C                                       Scale for phase to degrees
         IF ((DTYPE.EQ.1) .OR. (DTYPE.EQ.9) .OR. (DTYPE.EQ.11)) TSMAX =
     *      TSMAX * RAD2DG
C                                       Scale delay to nanosec.
         IF ((DTYPE.EQ.6) .OR. (DTYPE.EQ.12)) TSMAX = TSMAX * 1.0E9
C                                       Scale delays to mHz.
         IF (DTYPE.EQ.7) TSMAX = TSMAX * RATFAC
C                                       Determine scaling
         VALUE = ALOG (TSMAX) / ALOG (10.0)
         I = - VALUE
         IF (VALUE.GT.0.0) I = I - 1
         TSFACT = 10.0 ** (I + (NCOLPV-1))
         I = TSFACT / SFACT + 0.5
         IF ((DTYPE.EQ.6) .OR. (DTYPE.EQ.12)) TSFACT = TSFACT * 1.0E9
         IF (DTYPE.EQ.7) TSFACT = TSFACT * RATFAC
C                                       Don't increase SFACT by 10.
C         IF (I.NE.10) THEN
            SMAX = TSMAX
            SFACT = TSFACT
C            END IF
         IF ((DTYPE.EQ.1) .OR. (DTYPE.EQ.9) .OR. (DTYPE.EQ.11))
     *      SFACT = SFACT * RAD2DG
         END IF
C
 999  RETURN
      END
      SUBROUTINE GAINAV (OPTYPE, NOANT, ANTLAB, BUFFER, TIMKOL, SOUKOL,
     *   SUBKOL, ANTKOL, FQKOL, KOL, KOL2, TYPKOL, DTYPE, JPOL, TIF,
     *   TCAL, SCANV, DT, DP9, TIME, TSEC, NUSCAN, RECNO, IERR)
C-----------------------------------------------------------------------
C   Reads selected values from an open table, (SN or CL) which are
C   accumulated into SCANV and reads source info when the source
C   changes.  Only does 1 subarray at a time.
C   Inputs:
C     NOANT      I    Number of antennas to select
C     ANTLAB(*)  I    antenna numbers, in order.
C     BUFFER(*)  I    Table I/O buffer
C     TIMKOL     I    Pointer (in D)   array for time in record.
C     SOUKOL     I    Pointer (in I)   array for source number
C     SUBKOL     I    Pointer (in I)   array for subarray number
C     ANTKOL     I    Pointer (in I)   array for antenna number
C     FQKOL      I    Pointer (in I)   array for frequency ID #
C     KOL        I    Pointer (in I)   array for desired data
C     KOL2       I    Pointer (in I)   array for orthogonal polzn.;
C                     form R/L if KOL2 > 0.
C     DTYPE      I    Type of data: 0=amp,1=phase,6=delay,7=rate,
C                     8=SNR,9=Parallactic angle,10=Tsys, 11=elev.
C                     12 = mb delay
C     DT         R    Accumulation time in days
C     JPOL       I    Polarization for SY
C     BIF        I    IF number for SY
C   Input from common:
C     SOUWAN(*)  I    List of sources specified
C     NSOUWD     I    Number of sources in SOUWAN
C     DOSWNT     L    If true then sources selected, else deselected
C   Input/Output:
C     RECNO      I    Next record number to read.
C                     RECNO<0 => initialize.
C   Input/output to common:
C     CURSOU     I    Current source number.
C   Outputs:
C     SCANV(*)   R   The selected data array.
C                     Undefined values will contain 'INDE'.
C                     Note: maxant is defined in the parameter include
C                     INCS:PUVD.INC.
C     TIME(8)    I    Time days, hours, min, sec.
C     NUSCAN     L    True IF the first record in a new scan.
C     IERR       I    Return code, 0 => OK, -1 => out of data,
C                     -2 => no data found.
C                     > 0 => failed.
C   Output to common in DSOU.INC
C     SNAME      C*16 Source name
C     QUAL       I    Source qualifier.
C     CALCOD     R    Calibrator code 4 char.
C     FLUX(4,IF) R    Total flux density I, Q, U, V pol, (Jy) each IF
C     FREQO(IF)  D    Frequency offset (Hz)
C   Note:   If the end of data is encountered (IERR=-1) then TABIO
C   called with OPCODE='CLOS'.
C   Uses buffers UBUFF from DSEL.INC common.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPTYPE*4
      INTEGER   NOANT, ANTLAB(*), BUFFER(512), TIMKOL, SOUKOL, SUBKOL,
     *   ANTKOL, FQKOL, KOL, KOL2, TYPKOL, DTYPE, TIME(8), RECNO, IERR,
     *   JPOL, TIF
      REAL      SCANV(*), DT, DP9, TSEC, TCAL(4,MAXIF,MAXANT)
      LOGICAL   NUSCAN
C
      LOGICAL   GOTDAT, GOOD
      INTEGER   LFQ, I, ISUB, SUNUM, RECORD(1), JERR, ISLUN, IANT, ISOU,
     *   IPOINT, LSOU, CNTTIM, NUMREC
      REAL      REC4(1), SUMTIM, XRE, XIM, TIME4, HAT, XRE2, XIM2, AMP1,
     *   PHS1, AMP2, PHS2, AMP, PHS, AZ, TEMP, TC
      DOUBLE PRECISION    REC8(1), TLAST, TIMTAB, T1
      REAL      PANGLE(MAXANT)
      CHARACTER TSIGN*1
      INCLUDE 'LISTR.BUF'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (XXBUFF, RECORD, REC4, REC8)
      DATA ISLUN /26/
C-----------------------------------------------------------------------
      GOTDAT = .FALSE.
C                                       Clear SCANV
 10   CONTINUE
      DO 50 I = 1,NOANT
         SCANV(I) = FBLANK
 50      CONTINUE
      CNTTIM = 0
      SUMTIM = 0.0
C                                       Initialize time
      T1 = 1.0E10
      TLAST = -1.0D20
      LSOU = -10
C                                       Save scan number (0= no index)
      IF (RECNO.LT.1) RECNO = 1
C                                       Get number of records
      NUMREC = BUFFER(5)
C                                       Loop reading data
 100  IF (NUMREC.LT.RECNO) THEN
         RECNO = RECNO + 1
         GO TO 500
      ELSE
         CALL TABIO ('READ', 0, RECNO, RECORD, BUFFER, IERR)
         RECNO = RECNO + 1
         TIMTAB = REC8(TIMKOL)
         IF (DTYPE.EQ.10) TIMTAB = REC4(TIMKOL)
         IF (DTYPE.EQ.13) TIMTAB = REC4(TIMKOL)
C                                       Check if flagged
         IF (IERR.LT.0) GO TO 100
C                                       Error
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1105) IERR
            GO TO 990
            END IF
C                                       Check if avg. or scan done
         IF (((CURSOU.NE.RECORD(SOUKOL)) .AND. (ILOCSU.GE.0) .AND.
     *      GOTDAT) .OR.(TIMTAB.GT.(T1+DT))) GO TO 500
C                                       Time
         IF (TIMTAB.LT.TSTART) GO TO 100
         IF (TIMTAB.GT.TEND) THEN
            RECNO = NUMREC + 2
            GO TO 500
            END IF
C                                       Antenna, subarray number
         IANT = RECORD(ANTKOL)
         ISUB = RECORD(SUBKOL)
         ISOU = RECORD(SOUKOL)
         LFQ = RECORD(FQKOL)
C                                       check freqid
         IF ((FRQSEL.GT.0) .AND. (LFQ.GT.0) .AND. (LFQ.NE.FRQSEL))
     *      GO TO 100
C                                       Check subarray
         IF ((ISUB.NE.SUBARR) .AND. (ISUB.GT.0) .AND. (SUBARR.GT.0))
     *      GO TO 100
C                                       Check antenna
         DO 110 I = 1,NOANT
            IPOINT = I
            IF (IANT.EQ.ANTLAB(I)) GO TO 120
 110        CONTINUE
C                                       Not wanted
         GO TO 100
C                                       Check source
 120     IF (NSOUWD.LE.0) GO TO 140
         DO 130 I = 1,NSOUWD
            IF ((ISOU.EQ.SOUWAN(I)) .AND. DOSWNT) GO TO 140
            IF ((ISOU.EQ.SOUWAN(I)) .AND. (.NOT.DOSWNT)) GO TO 100
 130        CONTINUE
C                                       Not wanted
         IF (DOSWNT) GO TO 100
C                                       Found selected datum:
C                                       Time
 140     SUMTIM = SUMTIM + TIMTAB
         IF (T1.GT.1.0E9) T1 = TIMTAB
         CNTTIM = CNTTIM + 1
C                                       Source no.
         IF ((.NOT.GOTDAT) .AND. (CURSOU.NE.ISOU)) NUSCAN = .TRUE.
C         NUSCAN = CURSOU.NE.ISOU
         CURSOU = ISOU
         SUNUM = CURSOU
C                                       Get parallactic angle if DTYPE=9
         IF (((DTYPE.EQ.9).OR.(DTYPE.EQ.11)) .AND.
     *      (TIMTAB .GT.TLAST)) THEN
C                                       Source info if necessary:
            IF (CURSOU.NE.LSOU) CALL GETSOU (CURSOU, IUDISK, IUCNO,
     *         CATUV, ISLUN, JERR)
C                                       Will trap missing source later.
C                                       Get parallactic angles:
            TIME4 = TIMTAB
            IF (DTYPE.EQ.9) CALL PARANG (TIME4, PANGLE)
            END IF
C                                       Save value
         SCANV(IPOINT) = REC4(KOL)
         IF ((DTYPE.EQ.10) .OR. (DTYPE.EQ.13)) THEN
            IF (ABS(SCANV(IPOINT)-999.0).LT.0.1) SCANV(IPOINT) = FBLANK
            END IF
C                                       SY table are combinations
         IF ((DTYPE.EQ.15) .OR. (DTYPE.EQ.16)) THEN
            IF ((TYPKOL.GT.0) .AND. (RECORD(TYPKOL).EQ.1)) THEN
               TC = TCAL(JPOL+2,TIF,IANT)
            ELSE
               TC = TCAL(JPOL,TIF,IANT)
               END IF
            IF ((TC.NE.FBLANK) .AND. (TC.GT.0.0) .AND.
     *         (SCANV(IPOINT).NE.FBLANK)) THEN
               SCANV(IPOINT) = SCANV(IPOINT) / TC
               IF (DTYPE.EQ.15) THEN
                  IF ((REC4(KOL2).EQ.FBLANK) .OR. (SCANV(IPOINT).EQ.0.))
     *               THEN
                     SCANV(IPOINT) = FBLANK
                  ELSE
                     SCANV(IPOINT) = REC4(KOL2)/2./SCANV(IPOINT)
                     END IF
                  END IF
               END IF
            END IF
C                                       Only use imaginary part if there
C                                       is one (none for SNR etc.)
         IF (DTYPE.LT.2) THEN
            XRE = REC4(KOL)
            XIM = REC4(KOL+NUMIF)
            GOOD = (XRE.NE.FBLANK) .AND. (XIM.NE.FBLANK)
            XRE = XRE / GMMOD
            XIM = XIM / GMMOD
C                                       Polarization ratio R/L ?
            IF (KOL2.GT.0) THEN
               XRE2 = REC4(KOL2)
               XIM2 = REC4(KOL2+NUMIF)
               GOOD = (GOOD) .AND. (XRE2.NE.FBLANK) .AND.
     *            (XIM2.NE.FBLANK)
               XRE2 = XRE2 / GMMOD
               XIM2 = XIM2 / GMMOD
               IF (GOOD) THEN
                  AMP2 = SQRT (XRE2*XRE2 + XIM2*XIM2)
                  PHS2 = ATAN2 (XIM2, XRE2+1.0E-20)
                  AMP1 = SQRT (XRE*XRE + XIM*XIM)
                  PHS1 = ATAN2 (XIM, XRE+1.0E-20)
                  AMP = 0.0
                  IF (AMP2.NE.0) AMP = AMP1 / AMP2
                  PHS = PHS1 - PHS2
                  XRE = AMP * COS (PHS)
                  XIM = AMP * SIN (PHS)
                  END IF
               END IF
C                                       Amplitude
            IF ((DTYPE.EQ.0) .AND. GOOD) THEN
               SCANV(IPOINT) = (XRE*XRE + XIM*XIM)
               IF (OPTYPE.EQ.'EFST') THEN
                  SCANV(IPOINT) = SCANV(IPOINT) * 36.8
               ELSE IF (OPTYPE.EQ.'SEFD') THEN
                  SCANV(IPOINT) = SCANV(IPOINT) * 206.0
               ELSE
                  SCANV(IPOINT) = SQRT (SCANV(IPOINT))
                  END IF
C                                       Phase
            ELSE IF ((DTYPE.EQ.1) .AND. GOOD) THEN
               SCANV(IPOINT) = ATAN2 (XIM, XRE+1.0E-20)
               END IF
            END IF
C                                       Parallactic angle
         IF (DTYPE.EQ.9) SCANV(IPOINT) = PANGLE(IANT)
C                                       Source elevation
         IF (DTYPE.EQ.11) CALL SOUELV (IANT, TIMTAB, HAT, SCANV(IPOINT),
     *      AZ)
         TLAST = TIMTAB
         LSOU = CURSOU
         IF (SCANV(IPOINT).NE.FBLANK) GOTDAT = .TRUE.
         GO TO 100
         END IF
C                                       Scan done
C                                       See if have any data.
 500  IF ((.NOT.GOTDAT) .AND. (NUMREC.GE.RECNO)) GO TO 10
      IF (GOTDAT) THEN
C                                       Get source info
         IF ((KLOCSU.GE.0) .AND. (NUSCAN)) THEN
            CALL GETSOU (SUNUM, IUDISK, IUCNO, CATUV, ISLUN, JERR)
C                                       Didn't find source
            IF (JERR.EQ.11) THEN
               WRITE (MSGTXT,1750) SUNUM
               CALL MSGWRT (8)
               JERR = 0
               END IF
            IF (JERR.GT.0) THEN
               IERR = JERR
               WRITE (MSGTXT,1700) JERR
               GO TO 990
               END IF
            END IF
         END IF
C                                       Time
      TEMP = 0.0
      IF (CNTTIM.GT.0) TEMP = SUMTIM / CNTTIM
      IF (DP9.GT.0.0) CALL T2LST (TEMP)
C                                       convert to days hours mins secs
      CALL TFDHMS (TEMP, 1, TSIGN, TIME, TSEC)
C                                       Correct record number for next
C                                       call
      IF (NUMREC.GE.RECNO-1) THEN
         RECNO = RECNO - 1
C                                       If end of data, close UVGET
      ELSE
         CALL TABIO ('CLOS', 0, RECNO, RECORD, BUFFER, IERR)
         IERR = -1
C                                       Any thing found?
         IF (.NOT.GOTDAT) IERR = -2
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1105 FORMAT ('GAINAV: TABIO ERROR',I3,' READING GAIN TABLE')
 1700 FORMAT ('GAINAV: ERROR',I3,' READING SOURCE TABLE')
 1750 FORMAT ('GAINAV: SOURCE ',I3,' NOT IN SU TABLE')
      END
      SUBROUTINE GANSET (ANTLAB, IPOL, IRET)
C-----------------------------------------------------------------------
C   Routine to set up for column listing of SN or CL table and prepare
C   the list of antennas (ANTENS).
C   Also checks allowed Stokes types default = 'HALF'.
C   Input from common:
C     XANT(*)            R    Antenna array
C   Output:
C     ANTLAB(MAXANT)   I    The First (*,1) and second (*,2) antenna
C                           numbers of each baseline selected.
C     IPOL             I    Polarization type 1=R, 2=L, 3=R,L
C     IRET             I    Return code, 0=OK, else failed
C   Output in Common:
C     ANTENS(*)        I    Selected antenna numbers
C     NANTSL           I    Number of antennas in list
C     DOAWNT           L    True if antennas selected
C     STOKES           C*4? Stokes' parameter selected (changed if
C                           an unallowed type specified)
C-----------------------------------------------------------------------
      CHARACTER STO(5)*4
      INTEGER   IPOL, IRET, I, J, NEXT, IARG, LIMIT, NOANT, LUN, NSTO,
     *   NOSEL
      LOGICAL   T, F, ALLANT, DESEL
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANTLAB(MAXANT), ACTANS(MAXANT), IUBUFF(512)
      INCLUDE 'LISTR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (IUBUFF, UBUFF)
      DATA NSTO, STO /5, 'R   ','L   ','HALF','POLD', 'I   '/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check Stokes
      IF ((ICOR0.EQ.-1) .AND. (CATBLK(KINAX+JLOCS).LE.1)) STOKES = 'R'
      IF ((ICOR0.EQ.-2) .AND. (CATBLK(KINAX+JLOCS).LE.1)) STOKES = 'L'
      IF ((ICOR0.EQ.-5) .AND. (CATBLK(KINAX+JLOCS).LE.1)) STOKES = 'X'
      IF ((ICOR0.EQ.-6) .AND. (CATBLK(KINAX+JLOCS).LE.1)) STOKES = 'Y'
      IF ((ICOR0.EQ. 1) .AND. (CATBLK(KINAX+JLOCS).LE.1)) STOKES = 'I'
      DO 10 I = 1,NSTO
         IPOL = I
         IF (STOKES.EQ.STO(I)) GO TO 50
 10      CONTINUE
C                                       Not allowed type, use 'HALF'
         WRITE (MSGTXT,1010) STOKES, STO(3)
         CALL MSGWRT (6)
         STOKES = STO(3)
         IPOL = 3
C                                       Check for all ant. selected
 50   ALLANT = T
      DESEL = F
      DO 100 I = 1,50
         ANTENS(I) = 0
         ALLANT = ALLANT .AND. (ABS (XANT(I)).LE.1.0E-10)
         DESEL = DESEL .OR. (XANT(I).LT.-0.5)
 100     CONTINUE
      IF (ALLANT) GO TO 500
C                                       Not all selected - make list
         NEXT = 1
C                                       ANTENNAS array.
         DO 150 I = 1,50
            IARG = ABS (XANT(I)) + 0.5
            IF (IARG.EQ.0) GO TO 150
C                                       See if already have
               LIMIT = NEXT - 1
               IF (LIMIT.LT.1) GO TO 140
               DO 130 J = 1,LIMIT
                  IF (IARG.EQ.ANTENS(J)) GO TO 150
 130              CONTINUE
C                                       New antenna
 140              ANTENS(NEXT) = IARG
                  NEXT = NEXT + 1
 150           CONTINUE
      NOSEL = NEXT - 1
      NOANT = NOSEL
C                                       Find number of antennas
 500  LUN = 28
      CALL GETANS (DISKIN, CNOIN, CATBLK, LUN, IUBUFF, SUBARR, NOANT,
     *   ACTANS, IRET)
      IF ((IRET.NE.0) .AND. (IRET.NE.9) .AND. (IRET.NE.10)) THEN
         WRITE (MSGTXT,1511) IRET
         IRET = 2
         GO TO 990
         END IF
C                                       Fill ANTLAB
C                                       Antennas selected
      IF (.NOT.DESEL) THEN
         NEXT = 1
         DO 550 I = 1,NOANT
            J = I
            IF ((.NOT.ALLANT) .AND. (I.LE.50)) J = ABS(ANTENS(I))
            IF (ACTANS(J).GT.0) THEN
               ANTLAB(NEXT) = J
               NEXT = NEXT + 1
               END IF
 550        CONTINUE
C                                       Antennas deselected
      ELSE
         NEXT = 1
         DO 590 I = 1,NOANT
            IF (ACTANS(I).GT.0) THEN
               DO 570 J = 1,MIN(NOSEL,50)
                  IF (I.EQ.ABS(ANTENS(J))) GO TO 590
 570              CONTINUE
               ANTLAB(NEXT) = I
               NEXT = NEXT + 1
               END IF
 590        CONTINUE
         END IF
C                                       Done
         NOANT = NEXT - 1
      NANTSL = NOANT
      DOAWNT = .NOT.DESEL
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Warning: Stokes=',A4,' not allowed, using Stokes=',A4)
 1511 FORMAT ('GANSET: GETANS ERROR ',I3,' SEARCHING ANTENNA TABLES')
      END
      SUBROUTINE GANSCL (OPTYPE, NOANT, ANTLAB, NCOLPV, BUFFER, TIMKOL,
     *   SOUKOL, SUBKOL, ANTKOL, KOL, DTYPE, FREQ, SFACT, SMAX, IERR)
C-----------------------------------------------------------------------
C   Runs through the open table and determines the scaling factor
C   for printing.
C   Inputs:
C     OPTYPE     C*4  GAIN, SEFD, EFTS
C     NOANT      I    Number of antennas to select
C     ANTLAB(*)  I    antenna numbers, in order.
C     BUFFER(*)  I    Table I/O buffer
C     NCOLPV     I    Number of columns per value printed
C     TIMKOL     I    Pointer (in D)   array for time in record.
C     SOUKOL     I    Pointer (in I)   array for source number
C     SUBKOL     I    Pointer (in I)   array for subarray number
C     ANTKOL     I    Pointer (in I)   array for antenna number
C     KOL        I    Pointer (in I)   array for desired data
C     DTYPE      I    Type of data: 0=amp,1=phase,6=delay,7=rate,
C                     8=SNR,9=Parallactic angle,10=Tsys, 11=elev.
C                     12=multiband delay
C     FREQ       R    Frequency of IF
C   Input from common:
C     SOUWAN(*)  I    List of sources specified
C     NSOUWD     I    Number of sources in SOUWAN
C     DOSWNT     L    If true then sources selected, else deselected
C   Outputs:
C     SFACT      R    Scaling factor
C     SMAX       R    Maximum absolute value
C     IERR       I    Return code, 0 => OK, -1 => out of data,
C                     -2 => no data found.
C                     > 0 => failed.
C-----------------------------------------------------------------------
      CHARACTER OPTYPE*4
      INTEGER   NOANT, TIMKOL, SOUKOL, SUBKOL, ANTKOL, KOL, DTYPE, IERR,
     *   BUFFER(*), NCOLPV
      REAL      SFACT, SMAX, FREQ
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANTLAB(MAXANT)
C
      INTEGER   RECNO, RECORD(1), NUMREC, ISOU, LSOU, I, IANT, ISUB,
     *   JERR, IPOINT, ISLUN, J, LUNTMP
      REAL      REC4(1), TIME, VALUE, XRE, XIM, TSFACT, PANGLE(MAXANT),
     *   HAT, FACRAT, AZ
      LOGICAL   GOOD
      DOUBLE PRECISION REC8(1), TIMTAB
      INCLUDE 'LISTR.BUF'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (XXBUFF, RECORD, REC4, REC8)
C-----------------------------------------------------------------------
      MSGTXT = 'Determining scaling parameters'
      CALL MSGWRT (6)
      RECNO = 1
      NUMREC = BUFFER(5)
      SMAX = -1.0E10
      ISLUN = LUNTMP (1)
      FACRAT = 0.0
      IF (DTYPE.EQ.7) FACRAT = 1000.0 * FREQ
C                                        Loop through data
      DO 100 I = 1, NUMREC
         CALL TABIO ('READ', 0, RECNO, RECORD, BUFFER, IERR)
         RECNO = RECNO + 1
         IF (IERR.LT.0) GO TO 100
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
C                                        Check time
         TIMTAB = REC8(TIMKOL)
         IF (DTYPE.EQ.10) TIMTAB = REC4(TIMKOL)
         IF (DTYPE.EQ.13) TIMTAB = REC4(TIMKOL)
         IF (TIMTAB.LT.TSTART) GO TO 100
         IF (TIMTAB.GT.TEND) GO TO 200
C                                       Antenna, subarray number
         IANT = RECORD(ANTKOL)
         ISUB = RECORD(SUBKOL)
         ISOU = RECORD(SOUKOL)
C                                       Check subarray
         IF ((ISUB.NE.SUBARR) .AND. (ISUB.GT.0) .AND. (SUBARR.GT.0))
     *      GO TO 100
C                                       Check antenna
         DO 50 J = 1,NOANT
            IPOINT = J
            IF (IANT.EQ.ANTLAB(J)) GO TO 60
 50         CONTINUE
C                                       Not wanted
         GO TO 100
C                                       Check source
 60      IF (NSOUWD.LE.0) GO TO 80
         DO 70 J = 1,NSOUWD
            IF ((ISOU.EQ.SOUWAN(J)) .AND. DOSWNT) GO TO 80
            IF ((ISOU.EQ.SOUWAN(J)) .AND. (.NOT.DOSWNT)) GO TO 100
 70         CONTINUE
C                                       Not wanted
         IF (DOSWNT) GO TO 100
C                                       Found selected datum:
C                                       Time
 80      TIME = TIMTAB
C                                       Get parallactic angle if DTYPE=9
C                                       Get source info for DTYPE = 11
         IF ((DTYPE.EQ.9).OR.(DTYPE.EQ.11)) THEN
C                                       Source info if necessary:
            IF (ISOU.NE.LSOU) CALL GETSOU (ISOU, IUDISK, IUCNO,
     *         CATUV, ISLUN, JERR)
            LSOU = ISOU
C                                       Will trap missing source later.
C                                       Get parallactic angles:
            IF (DTYPE.EQ.9) CALL PARANG (TIME, PANGLE)
            END IF
C                                       Form necessary values
         VALUE = FBLANK
         XRE = REC4(KOL)
         GOOD = (XRE.NE.FBLANK)
         XRE = XRE / GMMOD
         IF (DTYPE.LT.2) THEN
            XIM = REC4(KOL+NUMIF)
            GOOD = (GOOD) .AND. (XIM.NE.FBLANK)
            XIM = XIM / GMMOD
         ELSE
            XIM = 0.0
            END IF
C                                       Amplitude
         IF ((DTYPE.EQ.0) .AND. GOOD) THEN
            VALUE = XRE*XRE + XIM*XIM
            IF (OPTYPE.EQ.'EFST') THEN
               VALUE = VALUE * 36.8
            ELSE IF (OPTYPE.EQ.'SEFD') THEN
               VALUE = VALUE * 206
            ELSE
               VALUE = SQRT (VALUE)
               END IF
C                                       Phase
         ELSE IF ((DTYPE.EQ.1) .AND. GOOD) THEN
            VALUE = ATAN2 (XIM, XRE+1.0E-20)
C                                       Parallactic angle
         ELSE IF (DTYPE.EQ.9) THEN
            VALUE = PANGLE(IANT)
C                                       Source elevation
         ELSE IF (DTYPE.EQ.11) THEN
            CALL SOUELV (IANT, TIMTAB, HAT, VALUE, AZ)
C                                       others
         ELSE
            VALUE = REC4(KOL)
            END IF
C                                       Find max non-blank value
         IF (VALUE.NE.FBLANK) SMAX = MAX (SMAX, ABS(VALUE))
 100     CONTINUE
C                                       Determine scaling
 200  IF (SMAX.GT.1.0E-20) THEN
C                                       Scale for phase to degrees
         IF ((DTYPE.EQ.1) .OR. (DTYPE.EQ.9) .OR. (DTYPE.EQ.11))
     *      SMAX = SMAX * RAD2DG
C                                       Scale delay to nanosec.
         IF ((DTYPE.EQ.6) .OR. (DTYPE.EQ.12)) SMAX = SMAX * 1.0E9
C                                       Scale rates to mHz.
         IF (DTYPE.EQ.7) SMAX = SMAX * FACRAT
C                                       Determine scaling
         VALUE = ALOG (SMAX) / ALOG (10.0)
         I = - VALUE
         IF (VALUE.GT.0.0) I = I - 1
         TSFACT = 10.0 ** (I + (NCOLPV-1))
         I = TSFACT / SFACT + 0.5
         IF ((DTYPE.EQ.6) .OR. (DTYPE.EQ.12)) TSFACT = TSFACT * 1.0E9
         IF (DTYPE.EQ.7) TSFACT = TSFACT * FACRAT
C                                       Don't increase SFACT by 10.
C         IF (I.NE.10) THEN
            SFACT = TSFACT
C            END IF
         IF ((DTYPE.EQ.1) .OR. (DTYPE.EQ.9) .OR. (DTYPE.EQ.11))
     *      SFACT = SFACT * RAD2DG
         END IF
C                                       Inform user of maximum
C                                       absolute value
      IF (SMAX.EQ.-1.0E10) THEN
         WRITE (MSGTXT,1005)
         GO TO 990
         END IF
      IF (DTYPE.EQ.0) THEN
         IF (OPTYPE.EQ.'EFST') THEN
            WRITE (MSGTXT,1110) SMAX
         ELSE IF (OPTYPE.EQ.'SEFD') THEN
            WRITE (MSGTXT,1120) SMAX
         ELSE
            WRITE (MSGTXT,1010) SMAX
            END IF
         END IF
      IF (DTYPE.EQ.1) WRITE (MSGTXT,1020) SMAX
      IF (DTYPE.EQ.2) WRITE (MSGTXT,1030) SMAX
      IF (DTYPE.EQ.6) WRITE (MSGTXT,1040) SMAX
      IF (DTYPE.EQ.7) WRITE (MSGTXT,1050) SMAX
      IF (DTYPE.EQ.8) WRITE (MSGTXT,1060) SMAX
      IF (DTYPE.EQ.9) WRITE (MSGTXT,1080) SMAX
      IF (DTYPE.EQ.10) WRITE (MSGTXT,1070) 'Tsys', SMAX
      IF (DTYPE.EQ.11) WRITE (MSGTXT,1090) SMAX
      IF (DTYPE.EQ.12) WRITE (MSGTXT,1100) SMAX
      IF (DTYPE.EQ.13) WRITE (MSGTXT,1070) 'Tant', SMAX
      IF (DTYPE.EQ.15) WRITE (MSGTXT,1070) 'Psys', SMAX
      IF (DTYPE.EQ.16) WRITE (MSGTXT,1130) SMAX
      IF (DTYPE.EQ.17) WRITE (MSGTXT,1140) 'Pdif', SMAX
      IF (DTYPE.EQ.18) WRITE (MSGTXT,1140) 'Psum', SMAX
      IF (DTYPE.EQ.19) WRITE (MSGTXT,1140) 'PostGain', SMAX
      CALL MSGWRT (6)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GANSCL: ERROR ',I4,' READING GAIN TABLE')
 1005 FORMAT ('All values are blanked')
 1010 FORMAT ('Maximum absolute amplitude = ',F12.5)
 1020 FORMAT ('Maximum absolute phase = ',F12.5,' degrees')
 1030 FORMAT ('Maximum absolute amplitude = ',F12.5)
 1040 FORMAT ('Maximum absolute residual delay = ',F12.5,' nanosec')
 1050 FORMAT ('Maximum absolute residual rate = ',F12.5,' mHz')
 1060 FORMAT ('Maximum absolute SNR = ',F12.5)
 1070 FORMAT ('Maximum absolute ',A,' = ',F12.5,' Kelvin')
 1080 FORMAT ('Maximum absolute parallactic angle = ',F12.5,' degrees')
 1090 FORMAT ('Maximum absolute source elevation = ',F12.5,' degrees')
 1100 FORMAT ('Maximum absolute multiband delay = ',F12.5,' nanosec')
 1110 FORMAT ('Maximum absolute Eff Tsys = ',F12.5,' degrees')
 1120 FORMAT ('Maximum absolute SEFD = ',F12.5,' degrees')
 1130 FORMAT ('Maximum absolute gain = Pdif/TCAL = ',F12.5)
 1140 FORMAT ('Maximum absolute ',A,' = ',F12.5,' counts')
      END
      SUBROUTINE GETANS (DISK, CNO, CATBLK, LUN, BUFFER, SUBARR, MAXANN,
     *   ACTANS, IRET)
C-----------------------------------------------------------------------
C   Checks the antenna files and returns the maximum antenna number in
C   the requested subarray and an array that shows if any specific
C   antenna occurred.  If an antenna station includes the string OUT it
C   is taken to be not present.
C   Inputs:
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      CATBLK   I(256)   Catalog header block.
C      LUN      I        Logical unit number to use
C      BUFFER   I(512)   I/O buffer and related storage.
C      SUBARR   I        Subarray, if <= 0 do all
C   Output:
C      MAXANN   I        Maximum antenna number in ACTANS
C      ACTANS   I(MAXANT)  > 0 => antenna # occurred in SUBARR
C      IRET     I        Return error code, 0 => ok,
C                           else TABINI or TABIO error.
C                            9 = requested SUBARR too large
C                           10 = no AN files.
C-----------------------------------------------------------------------
      INTEGER   BUFFER(512), DISK, CNO, CATBLK(256), LUN, SUBARR,
     *   MAXANN, ACTANS(*), IRET
C
      INTEGER   VER, NSUBA, I, NBUFF, II, NUMREC, NS1, NS2, J
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANT.INC'
C-----------------------------------------------------------------------
C                                       Check CATBLK for AN files.
      NBUFF = 1024
      CALL FNDEXT ('AN', CATBLK, NSUBA)
      CALL FILL (MAXANT, 0, ACTANS)
      MAXANN = 0
C                                       No AN files found.
      IF (NSUBA.LE.0) THEN
         IRET = 10
         MSGTXT = 'GETANS: NO AN FILES FOUND'
         IF (TYPUVD.LE.0) GO TO 990
         MAXANN = 28
         CALL FILL (28, 1, ACTANS)
         GO TO 999
         END IF
      IF (SUBARR.LE.0) THEN
         NS1 = 1
         NS2 = NSUBA
      ELSE IF (SUBARR.GT.NSUBA) THEN
         WRITE (MSGTXT,1000) SUBARR, NSUBA
         IRET = 9
         GO TO 990
      ELSE
         NS1 = SUBARR
         NS2 = SUBARR
         END IF

C                                       Loop over AN files
      DO 200 I = NS1,NS2
         VER = I
C                                       Open file
         CALL ANTINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1030) IRET, 'READ', VER
            GO TO 990
            END IF
C                                       Get number of antennas in
C                                       subarray.
         NUMREC = BUFFER(5)
         DO 100 II = 1,NUMREC
            CALL TABAN ('READ', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.GT.0) GO TO 150
            J = INDEX (ANNAME, 'OUT')
            IF ((ANNAME.NE.' ') .AND. (J.LE.0) .AND. (IRET.EQ.0)) THEN
               ACTANS(NOSTA) = ACTANS(NOSTA) + 1
               MAXANN = MAX (MAXANN, NOSTA)
               END IF
 100        CONTINUE
C                                       Close
 150     CALL TABIO ('CLOS', 0, II, BUFFER, BUFFER, IRET)
         IF (IRET.EQ.0) GO TO 200
            WRITE (MSGTXT,1030) IRET, 'CLOS', VER
            GO TO 990
 200     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETANS: REQUESTED SUBARRAY',I4,
     *   ' GREATER THAN MAX SUBARRAY',I4)
 1030 FORMAT ('GETANS: ERROR',I3,1X,A4,'ING AN FILE ',I5)
      END
      SUBROUTINE T2LST (T)
C-----------------------------------------------------------------------
C   Converts T to LST
C   In/out:
C      T   R   Time in, LST out in days
C-----------------------------------------------------------------------
      REAL      T
C
      DOUBLE PRECISION ANTLST
      INTEGER   ANT1, I
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANS.INC'
      SAVE ANT1
      DATA ANT1 /0/
C-----------------------------------------------------------------------
C                                       find usable antenna
      IF ((ANT1.LE.0) .OR. (STNLON(ANT1).EQ.0.0D0)) THEN
         DO 10 I = 1,MAXANT
            IF (STNLON(I).NE.0.0D0) THEN
               ANT1 = I
               GO TO 20
               END IF
 10         CONTINUE
         END IF
C                                       now use it
 20   ANTLST = GSTIAT + STNLON(ANT1) + T * ROTIAT
      T = ANTLST / TWOPI
C
 999  RETURN
      END
