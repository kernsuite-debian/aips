LOCAL INCLUDE 'PSHOUV.INC'
C                                       Parameter include for SHOUV
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXTRP, MAXSPC
C                                       MAXTRP = max. no. triples
      PARAMETER (MAXTRP=20)
C                                       MAXSPC = max. no. spectral
C                                       points.
      PARAMETER (MAXSPC=MAXCIF)
LOCAL END
LOCAL INCLUDE 'SHOUV.INC'
      INCLUDE 'PSHOUV.INC'
C                                       Local include for SHOUV
      CHARACTER NAMEIN*12, CLAIN*6, OPTYPE*4, XSOUR(30)*16,
     *   XCALCO*4, XSTOK*4, INEXT*2, LPNAME*48, LINE*132, TITL1*132,
     *   TITL2*132, SCRTCH*132
      HOLLERITH XNAMEI(3), XCLAIN(2), XOPTYP, XSOURC(4,30),
     *   XXCALC, XSTOKE, XLPNAM(12)
      DOUBLE PRECISION  BFREQ, FOFF(MAXIF)
      REAL      XSIN, XDISIN, XTIME(8), XBAND, XFREQ, XFQID, XBIF, XEIF,
     *   XBCHAN, XECHAN, XANT(50), XUVRA(2), XSUBA, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3),
     *   DPARM(10), DOCRT, XBADD(10), RPARM(20), VIS(3,MAXSPC)
      LOGICAL   ISINGL, VERBOS
      INTEGER   DISKIN, SEQIN, CNOIN, BUFFER(512), LUNP, FINDP,
     *   NACROS, IPCNT, PAGE
C
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XOPTYP, XSOURC,
     *   XXCALC, XTIME, XSTOKE, XBAND, XFREQ, XFQID, XBIF, XEIF, XBCHAN,
     *   XECHAN, XANT, XUVRA, XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER,
     *   XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, DPARM, DOCRT, XLPNAM,
     *   XBADD
      COMMON /CHPARM/ NAMEIN, CLAIN, OPTYPE, XSOUR, XCALCO,
     *   XSTOK, INEXT, LPNAME, LINE, TITL1, TITL2, SCRTCH
      COMMON /INFOLS/ FOFF, BFREQ, RPARM, VIS, ISINGL, VERBOS, DISKIN,
     *   SEQIN, CNOIN, BUFFER, NACROS, IPCNT, PAGE, LUNP, FINDP
LOCAL END
      PROGRAM SHOUV
C-----------------------------------------------------------------------
C! SHOUV prints uv data in a variety of forms.
C# Calibration UV VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2000, 2004, 2006-2007, 2009-2012
C;  Copyright (C) 2015-2016
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
C   Task SHOUV prints data from uv data files in a variety of forms.
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      INNAME         NAME          File name to be listed.
C      INCLASS        CLASS         File class to be listed.
C      INSEQ          SEQ           File sequence number.
C      INDISK         DISK          Disk volumn on which file resides.
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
      CHARACTER PRGM*6
      INTEGER  IRET, IERR
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SHOUV.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'SHOUV '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL SHOWIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       List data
      IF (OPTYPE.EQ.'CLOS') THEN
         CALL CLPHCH (IRET)
         IF (IRET.EQ.0) CALL CLPHAS (IRET)
      ELSE IF ((OPTYPE.EQ.'SPEC') .OR. (OPTYPE.EQ.'AVIF')) THEN
         CALL SPECCH (IRET)
         IF (IRET.EQ.0) CALL SPECUV (IRET)
         END IF
C                                       Close printer
      IF (IPCNT.GE.0) CALL LPCLOS (LUNP, FINDP, IPCNT, IERR)
C                                       Shut down
 990  IRET = MAX (0, IRET)
      CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE SHOWIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   SHOWIN gets input parameters for SHOUV
C   Inputs:
C      PRGN    C*6       Program name
C   Output:
C      JERR    I         Error code: 0 => ok
C                           5 => catalog troubles
C                           8 => can't start
C   Commons:
C      /INPARM/ all input adverbs in order given by INPUTS file
C      /MAPHDR/ output file catalog header
C   See prologue comments in SHOUV for more details.
C-----------------------------------------------------------------------
      CHARACTER STAT*4, STATUS*4, PRGN*6, OPCO(3)*4, UTYPE*2, STK(8)*4
      INTEGER   JERR, IROUND, NPARM, IERR, I, LUN, NIF, NOPT, IIVER,
     *   NSTOK, ISTOK
      REAL      CATR(256)
      LOGICAL   F, EQUAL, TABLE, EXIST, FITASC, MATCH
      INCLUDE 'SHOUV.INC'
      INTEGER   ISBAND(MAXIF)
      REAL      FINC(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATR, CATBLK)
      DATA F /.FALSE./
      DATA NOPT, OPCO /3,'CLOS','SPEC','AVIF'/
      DATA NSTOK, STK /8,'I','Q','U','V','RR','LL','RL','LR'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
C                                       Fixed PPM 1996.09.30: was 241
      NPARM = 242
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF ((IERR.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      RQUICK = RQUICK .AND. (DOCRT.LE.0.0)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      IF (DOCRT.GT.0.0) RQUICK = .FALSE.
      IF (RQUICK) RQUICK = LPNAME.NE.' '
      IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      CALL H2CHR (4, 1, XSTOKE, XSTOK)
C                                       Calcode abreviations
      IF (XCALCO(1:2).EQ.'CA') XCALCO = '*   '
      IF (XCALCO(1:1).EQ.'-')  XCALCO = '-CAL'
      SELCOD = XCALCO
      DO 25 I = 1,30
         CALL H2CHR (16, 1, XSOURC(1,I), XSOUR(I))
 25      CONTINUE
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
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
      STATUS = 'READ'
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, STATUS, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Check Stokes', default='I'
      ISTOK = -1
      DO 40 I = 1,NSTOK
         IF (XSTOK.EQ.STK(I)) ISTOK = I
 40      CONTINUE
      IF (ISTOK.LE.0) XSTOK = 'I'
C                                       Mark in CFILES if catalog
C                                       marked "READ"
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      BFREQ = FREQ
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
      CALL FILL (MAXANT, 0, ANTENS)
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
      IF (SUBARR.LE.0) SUBARR = 1
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
C                                       Unknown OPCODE?
      IF (.NOT.EQUAL) THEN
         JERR = 1
         MSGTXT = 'UNKNOWN OPTYPE =' // OPTYPE
         GO TO 990
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, JERR)
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
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
C                                       Open output device
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, BUFFER, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1080) JERR
         JERR = 1
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SHOWIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1080 FORMAT ('SHOWIN: ERROR ',I3,' OPENING OUTPUT ''PRINT'' DEVICE')
      END
      SUBROUTINE CLPHCH (IRET)
C-----------------------------------------------------------------------
C   Counts lines for column listing of selected closure phases.
C   Info for UVGET is set in SHOWIN. (except ANTENS)
C   Output:
C      IRET   I    Return code, 0=OK, else failed, -1 => line count
C                  too much for user
C-----------------------------------------------------------------------
      INTEGER   IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SHOUV.INC'
      CHARACTER CCODE*1, STR*4
      INTEGER   TIME(4), NPASS, IPASS, NCOLPV, I, LIM, IATY, IERR,
     *   NUMPRT, IOFF, SCANUM, CPTRIP(3,MAXTRP), NUMTRP, NCOUNT, TTY(2)
      LOGICAL   AMPVEC, DONE, NUSCAN
      REAL      CATR(256), SMAX, SFACT, SFACTI, DT
      HOLLERITH CATH(256)
      DOUBLE PRECISION    CATD(128), SFREQ
      REAL      SCANV(MAXTRP)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
C-----------------------------------------------------------------------
C                                       test only direct to line printer
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
      NCOUNT = 0
C                                       Setup
      CALL H2CHR (8, 1, CATH(KHOBJ), SNAME)
      QUAL = 0
      CALCOD = ' '
      FLUX(1,BIF) = 0.0
      FLUX(2,BIF) = 0.0
      FLUX(3,BIF) = 0.0
      FLUX(4,BIF) = 0.0
      AMPVEC = DPARM(2).GT.0.5
      IATY = 2
      IF (AMPVEC) IATY = 1
C                                       Number of char per col.
      NCOLPV = DPARM(3) + 0.5
      NCOLPV = MIN (NCOLPV, 9)
      NCOLPV = MAX (NCOLPV, 5)
C                                       Integration time
      DT = DPARM(4) / 1440.
C                                       Init. vis record
      DO 10 I = 1,20
         RPARM(I) = 0.0
         VIS(1,I) = 0.0
         VIS(2,I) = 0.0
         VIS(3,I) = 0.0
 10      CONTINUE
C                                       Setup for closure phase listing
      CALL CLOSET (NUMTRP, CPTRIP, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Determine number to print
      NUMPRT = (NACROS-15) / NCOLPV
C                                       if more will fit than needed
      IF (NUMPRT.GT.NUMTRP) THEN
         NUMPRT = NUMTRP
C                                       spread print a little
         NCOLPV = MAX (NCOLPV, MIN (6, ((NACROS-15)/NUMPRT)))
         NCOLPV = MIN (NCOLPV, 9)
         END IF
      NPASS = ((NUMTRP*1.0) / NUMPRT) + 0.999
C                                       Start outer loop
      DO 600 IPASS = 1,NPASS
         NUMPRT = (NACROS-15) / NCOLPV
         IOFF = (IPASS-1) * NUMPRT
         IF ((IOFF+NUMPRT).GT.NUMTRP) NUMPRT = NUMTRP - IOFF
C                                       Open uv data etc.
         RPARM(1) = FBLANK
         SCANUM = -10
         CALL UVGET ('INIT', RPARM, VIS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
         FREQ = FREQ * 1.0D-9
         BFREQ = BFREQ * 1.0D-9
C                                       first page titles
         IF (DOCRT.GT.-2.5) THEN
            NCOUNT = NCOUNT + 3
            IF (DOCAL) NCOUNT = NCOUNT + 1
            IF (DOBL) NCOUNT = NCOUNT + 1
            IF (DOFLAG) NCOUNT = NCOUNT + 1
            IF (DOBAND.GT.0) NCOUNT = NCOUNT + 1
C                                       Tell baselines used for the
C                                       different labels
            NCOUNT = NCOUNT + 1
            END IF
         DO 50 I = 1,NUMTRP,5
            LIM = I + 4
            LIM = MIN (LIM, NUMTRP)
            NCOUNT = NCOUNT + 1
 50         CONTINUE
         IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
C                                       Set scaling factor
         SMAX = 180.0
         SFACT = 57.29577951
         SFACTI = 1000.0 / SFACT
         SCANUM = -1
C                                       While more data, Loop thru
 100     CALL CLOSAV (NUMPRT, CPTRIP(1,IOFF+1), AMPVEC, DT, SCANV, TIME,
     *      NUSCAN, SCANUM, RPARM, VIS, IRET)
            DONE = IRET.LT.0
C                                       See if any data
            IF (IRET.EQ.-2) GO TO 600
            IF (IRET.GT.0) GO TO 999
C                                       If calc code OK
            CCODE = CALCOD(:1)
            IF ((XCALCO(:1).EQ.' ') .OR. (CCODE.EQ.XCALCO(:1)) .OR.
     *         ((XCALCO(:1).EQ.'*') .AND. (CCODE.NE.' ')) .OR.
     *         ((XCALCO(:1).EQ.'-') .AND. (CCODE.EQ.' '))) THEN
C                                       Header for scan
               IF (NUSCAN) THEN
                  IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
                  NCOUNT = NCOUNT + 1
C                                       Source info
                  SFREQ = BFREQ + ((FREQO(BIF) + FOFF(BIF)) * 1.0D-9)
                  IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
                  END IF
C                                       Type of data listed
               IF (NUSCAN) THEN
                  IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 2
                  IF (((IPCNT.GT.3) .AND. (IPCNT.LT.PRTMAX-1)) .OR.
     *               (DOCRT.LE.-2.5)) NCOUNT = NCOUNT + 1
                  END IF
C                                       Write row
               NCOUNT = NCOUNT + 1
C                                       End if Cal Code OK
               END IF
C                                       End while more data - loop
            IF (.NOT.DONE) GO TO 100
 600     CONTINUE
C                                       ask if needed
      IRET = 0
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
         IF (NCOUNT.GT.1000) THEN
            IRET = -1
            IPCNT = -1
            CALL LPCLOS (LUNP, FINDP, IPCNT, IERR)
            END IF
      ELSE IF (NCOUNT.GT.500) THEN
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRTCH, .FALSE., .FALSE.,
     *      .TRUE., IRET)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IRET.GT.0) GO TO 990
         WRITE (SCRTCH,1950) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, IRET)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IRET.GT.0) GO TO 990
         SCRTCH = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRTCH, 1, STR, IRET)
         IF (IRET.GT.0) GO TO 990
         IF ((STR(:1).NE.'y') .AND. (STR(:1).NE.'Y')) THEN
            IRET = -1
            SCRTCH = 'Good choice - save trees'
            IPCNT = -1
            CALL LPCLOS (LUNP, FINDP, IPCNT, IERR)
         ELSE
            SCRTCH = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IPCNT = -1
      CALL LPCLOS (LUNP, FINDP, IPCNT, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CLPHCH: ERROR ',I3,' OPENING UV DATA FILE')
 1950 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE CLPHAS (IRET)
C-----------------------------------------------------------------------
C   Gives column listing of selected closure phases.
C   Info for UVGET is set in SHOWIN. (except ANTENS)
C   Output:
C      IRET   I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SHOUV.INC'
      CHARACTER ENTRY*20, ATYPE(2)*12, CCODE*1, TRPLAB*36
      INTEGER   TIME(4), NPASS, IPASS, ICOL, NCOLPV, I, J, LIM,
     *   COLPNT, IATY, IRET, IERR, NUMPRT, IOFF,
     *   SCANUM, CPTRIP(3,MAXTRP), NUMTRP, IL1, IL2
      LOGICAL   AMPVEC, DONE, NUSCAN
      REAL      CATR(256), SMAX, SFACT, SFACTI, DT
      HOLLERITH CATH(256)
      DOUBLE PRECISION    CATD(128), SFREQ, DRVAL, DSFACT
      REAL      SCANV(MAXTRP)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA ATYPE /'Ampscalar   ', 'Vector      '/
      DATA TRPLAB /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C-----------------------------------------------------------------------
C                                       Setup
      CALL H2CHR (8, 1, CATH(KHOBJ), SNAME)
      QUAL = 0
      CALCOD = ' '
      FLUX(1,BIF) = 0.0
      FLUX(2,BIF) = 0.0
      FLUX(3,BIF) = 0.0
      FLUX(4,BIF) = 0.0
      AMPVEC = DPARM(2).GT.0.5
      IATY = 2
      IF (AMPVEC) IATY = 1
C                                       Number of char per col.
      NCOLPV = DPARM(3) + 0.5
      NCOLPV = MIN (NCOLPV, 9)
      NCOLPV = MAX (NCOLPV, 5)
C                                       Integration time
      DT = DPARM(4) / 1440.
C                                       Init. vis record
      DO 10 I = 1,20
         RPARM(I) = 0.0
         VIS(1,I) = 0.0
         VIS(2,I) = 0.0
         VIS(3,I) = 0.0
 10      CONTINUE
C                                       Setup for closure phase listing
      CALL CLOSET (NUMTRP, CPTRIP, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Determine number to print
      NUMPRT = (NACROS-15) / NCOLPV
C                                       if more will fit than needed
      IF (NUMPRT.GT.NUMTRP) THEN
         NUMPRT = NUMTRP
C                                       spread print a little
         NCOLPV = MAX (NCOLPV, MIN (6, ((NACROS-15)/NUMPRT)))
         NCOLPV = MIN (NCOLPV, 9)
         END IF
      NPASS = ((NUMTRP*1.0) / NUMPRT) + 0.999
C                                       Start outer loop
      DO 600 IPASS = 1,NPASS
         NUMPRT = (NACROS-15) / NCOLPV
         IOFF = (IPASS-1) * NUMPRT
         IF ((IOFF+NUMPRT).GT.NUMTRP) NUMPRT = NUMTRP - IOFF
C                                       Open uv data etc.
         RPARM(1) = FBLANK
         SCANUM = -10
         CALL UVGET ('INIT', RPARM, VIS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
         FREQ = FREQ * 1.0D-9
         BFREQ = BFREQ * 1.0D-9
C                                       first page titles
         IF (DOCRT.GT.-2.5) THEN
            IF (NACROS.GE.90) THEN
               WRITE (LINE,1020) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER,
     *            BCHAN, ECHAN, BIF, EIF
            ELSE
               WRITE (LINE,1021) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER,
     *            BCHAN, ECHAN, BIF, EIF
               END IF
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            WRITE (LINE,1022) FREQ, NCOR, NVIS
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
C                                       Tell baselines used for the
C                                       different labels
            LINE = 'Triplet Labels:'
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
         DO 50 I = 1,NUMTRP,5
            LIM = I + 4
            LIM = MIN (LIM, NUMTRP)
            LINE = '  '
            DO 40 J = I,LIM
               WRITE(ENTRY,1040) TRPLAB(J:J), CPTRIP(1,J), CPTRIP(2,J),
     *            CPTRIP(3,J)
               IL1 = 1 + (J-I) * 14
               IL2 = IL1 + 13
               LINE(IL1:IL2) = ENTRY(1:14)
 40            CONTINUE
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
 50         CONTINUE
         IF (DOCRT.GT.-2.5) THEN
            LINE = ' '
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
C                                       Set scaling factor
         SMAX = 180.0
         SFACT = 57.29577951
         SFACTI = 1000.0 / SFACT
         SCANUM = -1
C                                       While more data, Loop thru
 100     CALL CLOSAV (NUMPRT, CPTRIP(1,IOFF+1), AMPVEC, DT, SCANV, TIME,
     *      NUSCAN, SCANUM, RPARM, VIS, IRET)
            DONE = IRET.LT.0
C                                       See if any data
            IF (IRET.EQ.-2) GO TO 600
            IF (IRET.GT.0) GO TO 999
C                                       If calc code OK
            CCODE = CALCOD(:1)
            IF ((XCALCO(:1).EQ.' ') .OR. (CCODE.EQ.XCALCO(:1)) .OR.
     *         ((XCALCO(:1).EQ.'*') .AND. (CCODE.NE.' ')) .OR.
     *         ((XCALCO(:1).EQ.'-') .AND. (CCODE.EQ.' '))) THEN
C                                       Header for scan
               IF (NUSCAN) THEN
                  IF (DOCRT.GT.-2.5) THEN
                     LINE = ' '
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  WRITE (TITL1,1100) SNAME, QUAL, STOKES, BIF,
     *               EIF, BCHAN, ECHAN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, TITL1, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
C                                       Source info
                  SFREQ = BFREQ + ((FREQO(BIF) + FOFF(BIF)) * 1.0D-9)
                  IF (DOCRT.GT.-2.5) THEN
                     WRITE (LINE,1101) FLUX(1,BIF), CALCOD, SFREQ
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  END IF
C                                       Type of data listed
               IF (NUSCAN) THEN
                  IF (DOCRT.GT.-2.5) THEN
                     WRITE (LINE,1111) ATYPE(IATY)
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
C                                       Section label
                     LINE = ' '
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
C                                       Column labels
                  TITL2 = '   '
                  COLPNT = 15
                  DO 150 ICOL = 1,NUMPRT
                     ENTRY = '    ' // TRPLAB(IOFF+ICOL:IOFF+ICOL) //
     *                  '  '
                     TITL2(COLPNT:COLPNT+NCOLPV-1) = ENTRY
                     COLPNT = COLPNT + NCOLPV
 150                 CONTINUE
                  IF (((IPCNT.GT.3) .AND. (IPCNT.LT.PRTMAX-1)) .OR.
     *               (DOCRT.LE.-2.5)) THEN
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, TITL2, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  END IF
C                                       Scale Data
               WRITE (LINE,1160) TIME
               COLPNT = 15
               DSFACT = DBLE (SFACT)
               DO 200 ICOL = 1,NUMPRT
                  IF (SCANV(ICOL).NE.FBLANK) THEN
                     DRVAL = DSFACT * DBLE (SCANV(ICOL))
                     CALL FMTVAL (DRVAL, NCOLPV, 1, ENTRY, IERR)
                     IF (IERR.NE.0) THEN
                        IRET = 2
                        GO TO 999
                        END IF
                  ELSE
                     ENTRY = ' '
                     END IF
                  LINE(COLPNT:COLPNT+NCOLPV-1) = ENTRY(1:NCOLPV)
                  COLPNT = COLPNT + NCOLPV
 200              CONTINUE
C                                       Write row
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
C                                       End if Cal Code OK
               END IF
C                                       End while more data - loop
            IF (.NOT.DONE) GO TO 100
 600     CONTINUE
C                                       CRT error
 950  IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1950) IERR
         CALL MSGWRT (8)
         IRET = 1
      ELSE
         IRET = 0
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CLPHAS: ERROR ',I3,' OPENING UV DATA FILE')
 1020 FORMAT ('File = ',A12,'.',A6,'.',I4,' Vol =',I2,'  Userid =',I5,
     *   3X,'Channels =',I3,'-',I4,' IFS =',I2,'-',I3)
 1021 FORMAT (A12,'.',A6,'.',I4,' Vol=',I2,' User=',I5,' Chans=',I3,'-',
     *   I4,' IFS =',I2,'-',I3)
 1022 FORMAT ('Freq=',F13.9,' GHz   Ncor=',I3,'   No. vis=',I10)
 1023 FORMAT ('Stokes = ',A4,' Subarray = ',I3)
 1030 FORMAT ('Applying calibration table ',I3)
 1031 FORMAT ('Applying baseline table ',I3)
 1032 FORMAT ('Applying flag table ',I3)
 1033 FORMAT ('Applying bandpass table ',I3)
 1040 FORMAT (' ',A,' =',3I3,' ')
 1100 FORMAT ('Source=',A16,':',I4.4,', Stokes=',A4,', IFS=',I3,'-',
     *   I3,', Chans=',I4,'-',I4)
 1101 FORMAT ('Flux =',F8.4,' Jy, Calcode = ',A4,', Freq =',F13.9,
     *   ' GHz')
 1111 FORMAT ('Closure phases in degrees, averaging type = ',A12)
 1160 FORMAT (I4,'/',2(I2.2,':'),I2.2)
 1950 FORMAT ('CLPHAS: ERROR',I5,' DOING I/O TO TERMINAL')
      END
      SUBROUTINE CLOSAV (NUMTRP, CPTRIP, AMPVEC, DT, SCANV, TIME,
     *   NUSCAN, SCANUM, RPARM, VIS, IERR)
C-----------------------------------------------------------------------
C   Reads a uv data base and returns averages of closure phases for
C   selected triplets.  The triplets are specified in array CPTRIP.
C   Needs to be initialized by a call to UVGET.
C   Inputs:
C     NUMTRP     I    The number of triplets selected
C     CPTRIP     I(3,*) The antenna numbers involved in the triplets.
C     AMPVEC     L    If true do ampscalar averaging else vector.
C     DT         R    Averaging time in days
C   Input/Output:
C     RPARM      R(*) Random parameter array, first record of call.
C                     (1) = 'INDE' => don't use.
C     VIS        R(3,*) Visibility array, first record of call.
C   Outputs:
C     SCANV      R(*) The closure phase values for the selected
C                     triplets, corresponds to CPTRIP.
C                     Undefined values will contain 'INDE'.
C     TIME       I(4) Time days, hours, min, sec.
C     NUSCAN     L    True IF the first record in a new scan.
C     IERR       I    Return code, 0 => OK, -1 => out of data,
C                     > 0 => failed.
C   Output to common in DSOU.INC
C     SNAME      C*16 Source name
C     QUAL       I    Source qualifier.
C     CALCOD     C*4  Calibrator code
C     FLUX(4,IF) R    Total flux density I, Q, U, V pol, (Jy) each IF
C     FREQO(IF)  D    Frequency offset (Hz)
C   Note:   If the end of data is encountered (IERR=-1) then UVGET is
C   called with OPCODE='CLOS'.
C-----------------------------------------------------------------------
      INTEGER   NUMTRP, CPTRIP(3,*), TIME(4), SCANUM, IERR
      LOGICAL   AMPVEC, NUSCAN
      REAL      RPARM(*), VIS(3,*), DT, SCANV(*)
C
      INCLUDE 'PSHOUV.INC'
      LOGICAL   DONE1, GOTDAT, GOOD1
      INTEGER   I, JA1, JA2, SUNUM, JERR, ISLUN, ITRIP, IBASE,
     *   IVIS,  COUNT(3,MAXTRP), KVIS, CNTTIM, ICP, STTRIP
      REAL      T1, SUMTIM, WORK(2,3,MAXTRP), PI, TWOPI, CP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      DATA ISLUN /26/
      DATA PI, TWOPI /3.141592654,6.283185308/
C-----------------------------------------------------------------------
C                                       Save scan number (0= no index)
      NUSCAN = SCANUM.NE.INXRNO
      SCANUM = INXRNO
C                                       See if first record read
      DONE1 = RPARM(1).NE.FBLANK
      GOTDAT = .FALSE.
C                                       Clear arrays
 10   CONTINUE
      DO 50 I = 1,NUMTRP
         COUNT(1,I) = 0
         COUNT(2,I) = 0
         COUNT(3,I) = 0
         WORK(1,1,I) = 0.0
         WORK(2,1,I) = 0.0
         WORK(1,2,I) = 0.0
         WORK(2,2,I) = 0.0
         WORK(1,3,I) = 0.0
         WORK(2,3,I) = 0.0
         SCANV(I) = FBLANK
 50      CONTINUE
      CNTTIM = 0
      SUMTIM = 0.0
      NUSCAN = NUSCAN .OR. (SCANUM.NE.INXRNO)
      SCANUM = INXRNO
C                                       Initialize time
      T1 = 1.0E10
      KVIS = (LREC-NRPARM) / 3
C                                       Loop reading data
 100     IF (.NOT.DONE1) CALL UVGET ('READ', RPARM, VIS, IERR)
         IF (IERR.GT.0) GO TO 999
         DONE1 = .FALSE.
C                                       Check if avg. or scan done
         IF ((INXRNO.GT.SCANUM) .OR. (IERR.LT.0) .OR.
     *      (RPARM(ILOCT+1).GT.(T1+DT))) GO TO 500
C                                       Antenna numbers
         IF (ILOCB.GE.0) THEN
            JA1 = RPARM(ILOCB+1) / 256. + 0.1
            JA2 = RPARM(ILOCB+1) - JA1 * 256 + 0.1
         ELSE
            JA1 = RPARM(1+ILOCA1) + 0.1
            JA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         STTRIP = 1
C                                       Return to here to look for
C                                       further triplets involving this
C                                       baseline.
C                                       Find triplet and member
 105     DO 110 I = STTRIP,NUMTRP
            ITRIP = I
            IBASE = 1
            IF ((JA1.EQ.CPTRIP(1,I).AND.(JA2.EQ.CPTRIP(2,I)))) GO TO 120
            IBASE = 2
            IF ((JA1.EQ.CPTRIP(1,I).AND.(JA2.EQ.CPTRIP(3,I)))) GO TO 120
            IBASE = 3
            IF ((JA1.EQ.CPTRIP(2,I).AND.(JA2.EQ.CPTRIP(3,I)))) GO TO 120
 110        CONTINUE
C                                       Not wanted
         GO TO 100
 120     GOTDAT = .TRUE.
         STTRIP = ITRIP + 1
C                                       Time
         SUMTIM = SUMTIM + RPARM(ILOCT+1)
         IF (T1.GT.1.0E9) T1 = RPARM(ILOCT+1)
         CNTTIM = CNTTIM + 1
C                                       Source no.
         SUNUM = CURSOU
         IF (.NOT.AMPVEC) THEN
C                                       Vector average:
      INCLUDE 'INCS:ZVD.INC'
            DO 150 IVIS = 1,KVIS
               IF (VIS(3,IVIS).GT.0.0) THEN
                  COUNT(IBASE,ITRIP) = COUNT(IBASE,ITRIP) + 1
                  WORK(1,IBASE,ITRIP) = WORK(1,IBASE,ITRIP) +
     *               VIS(1,IVIS)
                  WORK(2,IBASE,ITRIP) = WORK(2,IBASE,ITRIP) +
     *               VIS(2,IVIS)
                  END IF
 150           CONTINUE
         ELSE
C                                       Ampscalar averaging
      INCLUDE 'INCS:ZVD.INC'
            DO 250 IVIS = 1,KVIS
               IF (VIS(3,IVIS).GT.0.0) THEN
                  COUNT(IBASE,ITRIP) = COUNT(IBASE,ITRIP) + 1
                  WORK(2,IBASE,ITRIP) = WORK(2,IBASE,ITRIP) +
     *               ATAN2 (VIS(2,IVIS), VIS(1,IVIS)+1.0E-25)
                  END IF
 250           CONTINUE
            END IF
C                                       This baseline may be involved in
C                                       more triplets.
         GO TO 105
C                                       Integration done?
C                                       See if have any data.
 500  IF ((.NOT.GOTDAT) .AND. (IERR.EQ.0)) GO TO 10
      IF (.NOT.GOTDAT) GO TO 800
      GOOD1 = .FALSE.
      IF (.NOT.AMPVEC) THEN
C                                       Vector averaging
         DO 550 I = 1,NUMTRP
            IF ((COUNT(1,I).GT.0) .AND. (COUNT(2,I).GT.0) .AND.
     *         (COUNT(3,I).GT.0)) THEN
               SCANV(I) = ATAN2 (WORK(2,1,I), WORK(1,1,I)+1.0E-20) -
     *            ATAN2 (WORK(2,2,I), WORK(1,2,I)+1.0E-20) +
     *            ATAN2 (WORK(2,3,I), WORK(1,3,I)+1.0E-20)
               GOOD1 = .TRUE.
               END IF
 550           CONTINUE
      ELSE
C                                       Else Amp scalar ave
         DO 650 I = 1,NUMTRP
            IF ((COUNT(1,I).GT.0) .AND. (COUNT(2,I).GT.0) .AND.
     *         (COUNT(3,I).GT.0)) THEN
               SCANV(I) = WORK(2,1,I) - WORK(2,2,I) + WORK(2,3,I)
               GOOD1 = .TRUE.
               END IF
 650        CONTINUE
C                                       end if vector averaging
         END IF
C                                       Any good values?
      IF ((.NOT.GOOD1) .AND. (IERR.EQ.0)) GO TO 10
C                                       Force range of values= +/- pi rad.
      DO 660 I = 1,NUMTRP
         IF (SCANV(I).NE.FBLANK) THEN
            CP = SCANV(I) / TWOPI
            ICP = CP
            CP = (CP - ICP) * TWOPI
            IF (CP.GT.PI) CP = CP - TWOPI
            IF (CP.LT.-PI) CP = CP + TWOPI
            SCANV(I) = CP
            END IF
 660     CONTINUE
C                                       Get source info
      IF (NUSCAN) THEN
         CALL GETSOU (SUNUM, IUDISK, IUCNO, CATUV, ISLUN, JERR)
         IF (JERR.EQ.11) THEN
C                                       Didn't find source
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
C                                       Time
 800  T1 = 0
      IF (CNTTIM.GT.0) T1 = SUMTIM / CNTTIM
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
 1700 FORMAT ('CLOSAV: ERROR',I3,' READING SOURCE TABLE')
 1750 FORMAT ('CLOSAV: SOURCE ',I3,' NOT IN SU TABLE')
      END
      SUBROUTINE CLOSET (NUMTRP, CPTRIP, IRET)
C-----------------------------------------------------------------------
C   Routine to set up for column listing and prepares the list of
C   antennas (ANTENS) for UVGET that includes all antennas mentioned in
C   XANT.
C   Input from common:
C      XANT(*)            R    Antenna array
C   Output:
C      NUMTRP             I    The number of baselines selected.
C      CPTRIP(3,*)        I    The antenna numbers in each triplet.
C                              numbers of each baseline selected.
C      IRET               I    Return code, 0=OK, else failed
C   Output in Common:
C      ANTENS(*)          I    Selected antenna numbers
C-----------------------------------------------------------------------
      INTEGER   NUMTRP, CPTRIP(3,*), IRET
C
      INTEGER   I, A1, A2, A3, NEXT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SHOUV.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Set antenna list.
      CALL FILL (MAXANT, 0, ANTENS)
      NEXT = 0
C                                       Find triplets requested
      NUMTRP = 0
C                                       Fifty elements in XANT
      DO 20 I = 1, 46, 3
         IF ((XANT(I).GT.0.0) .AND. (NUMTRP.LT.MAXTRP)) THEN
            A1 = XANT(I) + 0.5
            A2 = XANT(I+1) + 0.5
            A3 = XANT(I+2) + 0.5
C                                       Check validity
            IF ((A1.GT.0).AND.(A2.GT.0).AND.(A3.GT.0) .AND.
     *         (A1.NE.A2).AND.(A1.NE.A3).AND.(A2.NE.A3)) THEN
C                                       List antennas in ascending order
               NUMTRP = NUMTRP + 1
               CPTRIP(1,NUMTRP) = MIN (A1, A2, A3)
               CPTRIP(3,NUMTRP) = MAX (A1, A2, A3)
               IF ((A1.GT.CPTRIP(1,NUMTRP)) .AND.
     *            (A1.LT.CPTRIP(3,NUMTRP))) CPTRIP(2,NUMTRP) = A1
               IF ((A2.GT.CPTRIP(1,NUMTRP)) .AND.
     *            (A2.LT.CPTRIP(3,NUMTRP))) CPTRIP(2,NUMTRP) = A2
               IF ((A3.GT.CPTRIP(1,NUMTRP)) .AND.
     *            (A3.LT.CPTRIP(3,NUMTRP))) CPTRIP(2,NUMTRP) = A3
C                                       Selected antennas:
               NEXT = NEXT + 1
               ANTENS(NEXT) = A1
               NEXT = NEXT + 1
               ANTENS(NEXT) = A2
               NEXT = NEXT + 1
               ANTENS(NEXT) = A3
               END IF
            END IF
 20      CONTINUE
C                                       Remove duplicate antennas
      CALL IMERGE (NEXT, ANTENS, NEXT)
C                                       Make sure some triplets were
C                                       specified.
      IF (NUMTRP.LE.0) THEN
         MSGTXT = 'NO TRIPLETS SPECIFIED!'
         IRET = 5
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE SPECCH (IRET)
C-----------------------------------------------------------------------
C   Gives column listing of multiple spectral channels/IFs
C   Info for UVGET is set in SHOWIN. (except ANTENS)
C   Output:
C      IRET   I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER CCODE*1, STR*4
      INTEGER   TIME(4), DTYPE, NPASS, IPASS, NCOLPV, I, IATY, IERR,
     *   NUMSPC, NUMPRT, IOFF, SCANUM, NCOUNT, TTY(2)
      LOGICAL   F, AMPVEC, DONE, NUSCAN
      REAL      CATR(256), SMAX, SFACT, SFACTI, DT, SFACOL
      HOLLERITH CATH(256)
      DOUBLE PRECISION    CATD(128), SFREQ
      INCLUDE 'SHOUV.INC'
      INTEGER   IFCLAB(2,MAXSPC)
      REAL      SCANV(MAXSPC)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       test only direct to line printer
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
      NCOUNT = 0
C                                       Setup
      SFACOL = 1.0E10
      CALL H2CHR (8, 1, CATH(KHOBJ), SNAME)
      QUAL = 0
      CALCOD = ' '
      FLUX(1,BIF) = 0.0
      FLUX(2,BIF) = 0.0
      FLUX(3,BIF) = 0.0
      FLUX(4,BIF) = 0.0
      AMPVEC = DPARM(2).GT.0.5
      IATY = 2
      IF (AMPVEC) IATY = 1
      DTYPE = DPARM(1) + 0.5
      IF ((DTYPE.LT.0) .OR. (DTYPE.GT.2)) DTYPE = 0
C                                       Number of char per col. (min=5)
      NCOLPV = DPARM(3) + 0.5
      NCOLPV = MAX (NCOLPV, 5)
C                                       Allow ~6 significant figures
C                                       for both phase or amplitude
      IF (DTYPE.EQ.1) THEN
         NCOLPV = MIN (NCOLPV, 9)
      ELSE
         NCOLPV = MIN (NCOLPV, 7)
         END IF
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
      CALL SPCSET (NUMSPC, IFCLAB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Determine number to print
      NUMPRT = (NACROS-15) / NCOLPV
C                                       if more will fit than needed
      IF (NUMPRT.GT.NUMSPC) THEN
         NUMPRT = NUMSPC
C                                       spread print a little
         NCOLPV = MAX (NCOLPV, MIN (6, ((NACROS-15)/NUMPRT)))
         END IF
      NPASS = ((NUMSPC*1.0) / NUMPRT) + 0.999
C                                       Start outer loop
      DO 600 IPASS = 1,NPASS
         NUMPRT = (NACROS-15) / NCOLPV
         IOFF = (IPASS-1) * NUMPRT
         IF ((IOFF+NUMPRT).GT.NUMSPC) NUMPRT = NUMSPC - IOFF
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
         FREQ = FREQ * 1.0D-9
         BFREQ = BFREQ * 1.0D-9
C                                       first page titles
         IF (DOCRT.GT.-2.5) THEN
            NCOUNT = NCOUNT + 4
            IF (DOCAL) NCOUNT = NCOUNT + 1
            IF (DOBL) NCOUNT = NCOUNT + 1
            IF (DOFLAG) NCOUNT = NCOUNT + 1
            IF (DOBAND.GT.0) NCOUNT = NCOUNT + 1
            NCOUNT = NCOUNT + 1
            END IF
         SCANUM = -1
C                                       While more data, Loop thru
 100     CALL SPECAV (NUMPRT, IFCLAB(1,IOFF+1), AMPVEC, DTYPE, DT,
     *      SCANV, TIME, NUSCAN, SCANUM, RPARM, VIS, IRET)
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
C                                       Find max value
               CALL SPCMAX (SCANV, NUMPRT, DTYPE, NCOLPV, SMAX, SFACT)
C                                       If scale goes up by 10,
C                                       ignore, use previous scale
               IF ((SFACT/SFACOL.LE.10.01) .AND.
     *            (SFACT/SFACOL.GT.0.99)) SFACT = SFACOL
               SFACTI = 1000.0 / SFACT
C                                       Header for scan
               IF (NUSCAN) THEN
                  IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
                  NCOUNT = NCOUNT + 1
C                                       Source info
                  SFREQ = BFREQ + ((FREQO(BIF) + FOFF(BIF)) * 1.0D-9)
                  IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
                  END IF
C                                       Type of data listed
               IF ((NUSCAN) .OR. (SFACT.NE.SFACOL)) THEN
                  IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 2
                  IF (((IPCNT.GT.3) .AND. (IPCNT.LT.PRTMAX-1)) .OR.
     *               (DOCRT.LE.-2.5)) NCOUNT = NCOUNT + 1
                  END IF
C                                       Write row
               NCOUNT = NCOUNT + 1
C                                       End if Cal Code OK
               END IF
C                                       End while more data - loop
            IF (.NOT.DONE) GO TO 100
 600     CONTINUE
C                                       ask if needed
      IRET = 0
      IF (NCOUNT.GT.500) THEN
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRTCH, .FALSE., .FALSE.,
     *      .TRUE., IRET)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IRET.GT.0) GO TO 990
         WRITE (SCRTCH,1950) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, IRET)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IRET.GT.0) GO TO 990
         SCRTCH = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRTCH, 1, STR, IRET)
         IF (IRET.GT.0) GO TO 990
         IF ((STR(:1).NE.'y') .AND. (STR(:1).NE.'Y')) THEN
            IRET = -1
            SCRTCH = 'Good choice - save trees'
            IPCNT = -1
            CALL LPCLOS (LUNP, FINDP, IPCNT, IERR)
         ELSE
            SCRTCH = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IPCNT = -1
      CALL LPCLOS (LUNP, FINDP, IPCNT, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SPECCH: ERROR ',I3,' OPENING UV DATA FILE')
 1950 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE SPECUV (IRET)
C-----------------------------------------------------------------------
C   Gives column listing of multiple spectral channels/IFs
C   Info for UVGET is set in SHOWIN. (except ANTENS)
C   Output:
C      IRET   I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      CHARACTER ENTRY*20, ATYPE(2)*12, CCODE*1
      INTEGER   TIME(4), DTYPE, NPASS, IPASS, ICOL, NCOLPV, I,
     *   COLPNT, IATY, IRET, IERR, NUMSPC, NUMPRT, IOFF, SCANUM
      LOGICAL   F, AMPVEC, DONE, NUSCAN
      REAL      CATR(256), SMAX, SFACT, SFACTI, DT, SFACOL
      HOLLERITH CATH(256)
      DOUBLE PRECISION    CATD(128), SFREQ, DRVAL, DSFACT
      INCLUDE 'SHOUV.INC'
      INTEGER   IFCLAB(2,MAXSPC)
      REAL      SCANV(MAXSPC)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA ATYPE /'Ampscalar   ', 'Vector      '/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Setup
      SFACOL = 1.0E10
      CALL H2CHR (8, 1, CATH(KHOBJ), SNAME)
      QUAL = 0
      CALCOD = ' '
      FLUX(1,BIF) = 0.0
      FLUX(2,BIF) = 0.0
      FLUX(3,BIF) = 0.0
      FLUX(4,BIF) = 0.0
      AMPVEC = DPARM(2).GT.0.5
      IATY = 2
      IF (AMPVEC) IATY = 1
      DTYPE = DPARM(1) + 0.5
      IF ((DTYPE.LT.0) .OR. (DTYPE.GT.2)) DTYPE = 0
C                                       Number of char per col. (min=5)
      NCOLPV = DPARM(3) + 0.5
      NCOLPV = MAX (NCOLPV, 5)
C                                       Allow ~6 significant figures
C                                       for both phase or amplitude
      IF (DTYPE.EQ.1) THEN
         NCOLPV = MIN (NCOLPV, 9)
      ELSE
         NCOLPV = MIN (NCOLPV, 7)
         END IF
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
      CALL SPCSET (NUMSPC, IFCLAB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Determine number to print
      NUMPRT = (NACROS-15) / NCOLPV
C                                       if more will fit than needed
      IF (NUMPRT.GT.NUMSPC) THEN
         NUMPRT = NUMSPC
C                                       spread print a little
         NCOLPV = MAX (NCOLPV, MIN (6, ((NACROS-15)/NUMPRT)))
         END IF
      NPASS = ((NUMSPC*1.0) / NUMPRT) + 0.999
C                                       Start outer loop
      DO 600 IPASS = 1,NPASS
         NUMPRT = (NACROS-15) / NCOLPV
         IOFF = (IPASS-1) * NUMPRT
         IF ((IOFF+NUMPRT).GT.NUMSPC) NUMPRT = NUMSPC - IOFF
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
         FREQ = FREQ * 1.0D-9
         BFREQ = BFREQ * 1.0D-9
C                                       first page titles
         IF (DOCRT.GT.-2.5) THEN
            IF (NACROS.GE.90) THEN
               WRITE (LINE,1020) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER,
     *            BCHAN, ECHAN, BIF, EIF
            ELSE
               WRITE (LINE,1021) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER,
     *            BCHAN, ECHAN, BIF, EIF
               END IF
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            WRITE (LINE,1022) FREQ, NCOR, NVIS
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            WRITE (LINE,1023) STOKES, SUBARR
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            WRITE (LINE,1024) ANTENS(1), ANTENS(2)
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
         SCANUM = -1
C                                       While more data, Loop thru
 100     CALL SPECAV (NUMPRT, IFCLAB(1,IOFF+1), AMPVEC, DTYPE, DT,
     *      SCANV, TIME, NUSCAN, SCANUM, RPARM, VIS, IRET)
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
C                                       Find max value
               CALL SPCMAX (SCANV, NUMPRT, DTYPE, NCOLPV, SMAX, SFACT)
C                                       If scale goes up by 10,
C                                       ignore, use previous scale
               IF ((SFACT/SFACOL.LE.10.01) .AND.
     *            (SFACT/SFACOL.GT.0.99)) SFACT = SFACOL
               SFACTI = 1000.0 / SFACT
C                                       Header for scan
               IF (NUSCAN) THEN
                  IF (DOCRT.GT.-2.5) THEN
                     LINE = ' '
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  WRITE (TITL1,1100) SNAME, QUAL, STOKES, BIF,
     *               EIF, BCHAN, ECHAN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, TITL1, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
C                                       Source info
                  SFREQ = BFREQ + ((FREQO(BIF) + FOFF(BIF)) * 1.0D-9)
                  IF (DOCRT.GT.-2.5) THEN
                     WRITE (LINE,1101) FLUX(1,BIF), CALCOD, SFREQ
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  END IF
C                                       Type of data listed
               IF ((NUSCAN) .OR. (SFACT.NE.SFACOL)) THEN
                  IF (DOCRT.GT.-2.5) THEN
                     IF (DTYPE.EQ.0) WRITE (LINE,1110) SFACTI,
     *                  ATYPE(IATY)
                     IF (DTYPE.EQ.1) WRITE (LINE,1111) ATYPE(IATY)
                     IF (DTYPE.EQ.2) WRITE (LINE,1112) SFACTI,
     *                  ATYPE(IATY)
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
C                                       Section label
                     LINE = ' '
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  WRITE (TITL2,1150) ANTENS(1), ANTENS(2)
                  COLPNT = 15
                  DO 150 ICOL = 1,NUMPRT
                     IF (IFCLAB(2,IOFF+ICOL).GT.0) THEN
C                                       channels selected
                        IF (NCOLPV.LE.6) THEN
                           WRITE (ENTRY,1115) IFCLAB(1,IOFF+ICOL),
     *                        IFCLAB(2,IOFF+ICOL)
                        ELSE
                           WRITE (ENTRY,1116) IFCLAB(1,IOFF+ICOL),
     *                        IFCLAB(2,IOFF+ICOL)
                           END IF
                     ELSE
C                                       channels averaged
                        WRITE (ENTRY,1117) IFCLAB(1,IOFF+ICOL)
                        END IF
                     TITL2(COLPNT:COLPNT+NCOLPV-1) = ENTRY
                     COLPNT = COLPNT + NCOLPV
 150                 CONTINUE
                  IF (((IPCNT.GT.3) .AND. (IPCNT.LT.PRTMAX-1)) .OR.
     *               (DOCRT.LE.-2.5)) THEN
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, TITL2, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  END IF
C                                       Scale Data
               SFACOL = SFACT
               WRITE (LINE,1160) TIME
               COLPNT = 15
               DSFACT = DBLE (SFACT)
               DO 200 ICOL = 1,NUMPRT
                  IF (SCANV(ICOL).NE.FBLANK) THEN
                     DRVAL = DSFACT * DBLE (SCANV(ICOL))
                     CALL FMTVAL (DRVAL, NCOLPV, DTYPE, ENTRY, IERR)
                     IF (IERR.NE.0) THEN
                        IRET = 2
                        GO TO 999
                        END IF
                  ELSE
                     ENTRY = ' '
                     END IF
                  LINE(COLPNT:COLPNT+NCOLPV-1) = ENTRY(1:NCOLPV)
                  COLPNT = COLPNT + NCOLPV
 200              CONTINUE
C                                       Write row
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
C                                       End if Cal Code OK
               END IF
C                                       End while more data - loop
            IF (.NOT.DONE) GO TO 100
 600     CONTINUE
C                                       CRT error
 950  IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1950) IERR
         CALL MSGWRT (8)
         IRET = 1
      ELSE
         IRET = 0
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SPECUV: ERROR ',I3,' OPENING UV DATA FILE')
 1020 FORMAT ('File = ',A12,'.',A6,'.',I4,' Vol =',I2,'  Userid =',I5,
     *   3X,'Channels =',I3,'-',I4,' IFS =',I2,'-',I3)
 1021 FORMAT (A12,'.',A6,'.',I4,' Vol=',I2,' User=',I5,' Chans=',I3,'-',
     *   I4,' IFS =',I2,'-',I3)
 1022 FORMAT ('Freq=',F13.9,' GHz   Ncor=',I3,'   No. vis=',I10)
 1023 FORMAT ('Stokes = ',A4,' Subarray = ',I3)
 1024 FORMAT ('Spectral points on baseline = ',I3, ' - ', I3)
 1030 FORMAT ('Applying calibration table ',I3)
 1031 FORMAT ('Applying baseline table ',I3)
 1032 FORMAT ('Applying flag table ',I3)
 1033 FORMAT ('Applying bandpass table ',I3)
 1100 FORMAT ('Source=',A16,':',I4.4,', Stokes=',A4,', IFS=',I3,'-',
     *   I3,', Chans=',I4,'-',I4)
 1101 FORMAT ('Flux =',F8.4,' Jy, Calcode = ',A4,', Freq =',F13.9,
     *   ' GHz')
 1110 FORMAT ('Amplitudes, 1000 = ',1PE9.2,' Jy, averaging type = ',A12)
 1111 FORMAT ('Phases in degrees, averaging type = ',A12)
 1112 FORMAT ('Rms, 1000 = ',1PE9.2,' Jy, averaging type = ',A12)
 1150 FORMAT ('IF/Chan(',I2,'-',I2,')')
 1160 FORMAT (I4,'/',2(I2.2,':'),I2.2)
 1115 FORMAT (I2, I3,'      ')
 1116 FORMAT (I2,'/',I3,'     ')
 1117 FORMAT (I3,'/ *      ')
 1950 FORMAT ('SPECUV: ERROR',I5,' DOING I/O TO TERMINAL')
      END
      SUBROUTINE SPCSET (NUMSPC, IFCLAB, IRET)
C-----------------------------------------------------------------------
C   Routine to set up for column listing and prepares the list of
C   antennas (ANTENS) for UVGET that includes only the first two
C   antennas in  XANT.
C   Input from common:
C      XANT     R(*) Antenna array(first two elements give
C                    baseline).
C   Output:
C      NUMSPC   I    The number of spectral samples
C                    (IF/channel)
C      IFCLAB(  I(*,2)The IF (*,1) and channel (*,2) array.
C      IRET     I    Return code, 0=OK, else failed
C   Output in Common:
C      ANTENS   I(*) Selected antenna numbers
C-----------------------------------------------------------------------
      INTEGER NUMSPC, IFCLAB(2,*), IRET
C
      INTEGER   I, J
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SHOUV.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Set baseline
      CALL FILL (MAXANT, 0, ANTENS)
      ANTENS(1) = XANT(1) + 0.5
      ANTENS(2) = XANT(2) + 0.5
      IF ((ANTENS(1).LE.0) .OR. (ANTENS(2).LE.0)) THEN
         MSGTXT = 'NO BASELINE SELECTED'
         IRET = 1
         GO TO 990
         END IF
C                                       Fill Column labels.
      NUMSPC = 0
C                                       OPTYPE='SPEC'
      IF (OPTYPE.EQ.'SPEC') THEN
         DO 40 I = BIF,EIF
            DO 30 J = BCHAN,ECHAN
               IF (NUMSPC.LE.MAXSPC) THEN
                  NUMSPC = NUMSPC + 1
                  IFCLAB(1,NUMSPC) = I
                  IFCLAB(2,NUMSPC) = J
                  END IF
 30            CONTINUE
 40         CONTINUE
C                                       OPTYPE='AVIF'
      ELSE IF (OPTYPE.EQ.'AVIF') THEN
         DO 50 I = BIF,EIF
            IF (NUMSPC.LE.MAXSPC) THEN
               NUMSPC = NUMSPC + 1
               IFCLAB(1,NUMSPC) = I
               IFCLAB(2,NUMSPC) = -1
               END IF
 50         CONTINUE
         END IF
      IF (NUMSPC.LE.0) THEN
C                                       No baselines
         IRET = 2
         MSGTXT = 'ERROR: NO IF/CHANNELS SPECIFIED!'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE SPCMAX (SCANV, NUMPRT, DTYPE, NCOLPV, SMAX, SFACT)
C-----------------------------------------------------------------------
C   Routine to find the maximum, non blank value in an array and
C   determine the proper scaling factor for printing.
C   Inputs:
C      SCANV   R(*)   Scan values.
C      NUMPRT  I      Number of values in SCANV
C      DTYPE   I      Data type, 0=amp, 1=phase, 2=RMS
C      NCOLPV  I      Number of columns per value printed
C   Output:
C      SMAX    R      Maximum abs value.
C      SFACT   R      Scaling factor to print values.
C-----------------------------------------------------------------------
      INTEGER   NUMPRT, DTYPE, NCOLPV
      REAL      SCANV(NUMPRT), SMAX, SFACT
C
      INTEGER   I
      REAL      VALUE
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Find maximum non blank
      SMAX = 0.0
      SFACT = 1.0
      IF (NUMPRT.GT.0) THEN
         DO 100 I = 1,NUMPRT
            IF (SCANV(I).NE.FBLANK) THEN
               VALUE = ABS (SCANV(I))
               SMAX = MAX (SMAX, VALUE)
               END IF
 100        CONTINUE
         END IF
C                                       Determine scaling
      IF (SMAX.GT.1.0E-20) THEN
C                                       Scale for phase to degrees
         IF (DTYPE.EQ.1) SMAX = SMAX * 57.29577951
         VALUE = ALOG (SMAX) / ALOG (10.0)
         I = - VALUE
         IF (VALUE.GT.0.0) I = I - 1
         IF (DTYPE.EQ.1) THEN
            SFACT = 57.29577951
         ELSE
            SFACT = 10.0 ** (I + (NCOLPV-1))
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE SPECAV (NUMSPC, IFCLAB, AMPVEC, DTYPE, DT, SCANV, TIME,
     *   NUSCAN, SCANUM, RPARM, VIS, IERR)
C-----------------------------------------------------------------------
C   Reads a uv data base and returns averages of amp, phase or the RMS
C   scatter for selected spectral points (IF/channel).
C   Needs to be initialized by a call to UVGET.
C   The  order of the values returned in SCANV is defined by the order
C   in the array IFCLAB.  Channel values are averaged if IFCLAB(2,?) is
C   less that 1.
C   Inputs:
C     NUMSPC  I        Number of baselines to average.
C     IFCLAB  I(2,*)   The IF (*,1) and channel (*,2) numbers.
C     AMPVEC  L        If true do ampscalar averaging else vector.
C     DTYPE   I        Output type, 0 => amplitude, 1=> phase,
C                         3 => amplitude RMS.
C     DT      R        Averaging time in days
C   Input/Output:
C     RPARM    R(*)    Random parameter array, first record of call.
C                      (1) = 'INDE' => don't use.
C     VIS      R(3,*)  Visibility array, first record of call.
C   Outputs:
C     SCANV    R(*)   The result for entries in IFCLAB.
C                     Undefined values will contain 'INDE'.
C     TIME     I(*)   Time days, hours, min, sec.
C     NUSCAN   L      True IF the first record in a new scan.
C     IERR     I      Return code, 0 => OK, -1 => out of data,
C                     > 0 => failed.
C   Output to common in DSOU.INC
C     SNAME      C*16 Source name
C     QUAL       I    Source qualifier.
C     CALCOD     C*4  Calibrator code
C     FLUX(4,IF) R    Total flux density I, Q, U, V pol, (Jy) each IF
C     FREQO(IF)  D    Frequency offset (Hz)
C   Note:   If the end of data is encountered (IERR=-1) then UVGET is
C   called with OPCODE='CLOS'.
C-----------------------------------------------------------------------
      INTEGER   NUMSPC, IFCLAB(2,*), DTYPE, TIME(4), SCANUM, IERR
      LOGICAL   AMPVEC, NUSCAN
      REAL      RPARM(*), VIS(3,*), DT, SCANV(*)
C
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL   DONE1, GOTDAT, GOOD1
      INTEGER   I, SUNUM, JERR, ISLUN, ISPEC, IVIS, COUNT(MAXCIF),
     *   KVIS, CNTTIM, IFNO, CHNO, INDEX, LNCF, LNCIF
      REAL      T1, AMP, SUMTIM, WORK(MAXCIF,4), TEMP, TEMP1, TEMP2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      DATA ISLUN /26/
C-----------------------------------------------------------------------
C                                       Save scan number (0= no index)
      NUSCAN = SCANUM.NE.INXRNO
      SCANUM = INXRNO
C                                       See if first record read
      DONE1 = RPARM(1).NE.FBLANK
      GOTDAT = .FALSE.
      LNCF = INCF / 3
      LNCIF = INCIF / 3
C                                       Clear arrays
 10   CONTINUE
      DO 50 I = 1,NUMSPC
         COUNT(I) = 0
         WORK(I,1) = 0.0
         WORK(I,2) = 0.0
         WORK(I,3) = 0.0
         WORK(I,4) = 0.0
         SCANV(I) = FBLANK
 50      CONTINUE
      CNTTIM = 0
      SUMTIM = 0.0
      NUSCAN = NUSCAN .OR. (SCANUM.NE.INXRNO)
      SCANUM = INXRNO
C                                       Initialize time
      T1 = 1.0E10
C                                       Loop reading data
 100     IF (.NOT.DONE1) CALL UVGET ('READ', RPARM, VIS, IERR)
         IF (IERR.GT.0) GO TO 999
         DONE1 = .FALSE.
C                                       Check if avg. or scan done
         IF ((INXRNO.GT.SCANUM) .OR. (IERR.LT.0) .OR.
     *      (RPARM(ILOCT+1).GT.(T1+DT))) GO TO 500
C                                       Time
         SUMTIM = SUMTIM + RPARM(ILOCT+1)
         IF (T1.GT.1.0E9) T1 = RPARM(ILOCT+1)
         CNTTIM = CNTTIM + 1
C                                       Source no.
         SUNUM = CURSOU
C                                       Loop over spectral points
         DO 300 ISPEC = 1,NUMSPC
            IFNO = IFCLAB(1,ISPEC)
            IF (IFCLAB(2,ISPEC).GT.0) THEN
C                                       Individual Channels
               CHNO = IFCLAB(2,ISPEC)
               KVIS = 1
            ELSE
C                                       Average channels
               CHNO = BCHAN
               KVIS = ECHAN - BCHAN + 1
               END IF
            INDEX = 1 + (CHNO-BCHAN) * LNCF + (IFNO-BIF) * LNCIF
            GOTDAT = .TRUE.
            IF (.NOT.AMPVEC) THEN
C                                       Vector average:
      INCLUDE 'INCS:ZVD.INC'
               DO 150 IVIS = 1,KVIS
                  IF (VIS(3,INDEX).GT.0.0) THEN
                  COUNT(ISPEC) = COUNT(ISPEC) + 1
                  WORK(ISPEC,1) = WORK(ISPEC,1) + VIS(1,INDEX)
                  WORK(ISPEC,2) = WORK(ISPEC,2) + VIS(2,INDEX)
                  WORK(ISPEC,3) = WORK(ISPEC,3) + VIS(1,INDEX) ** 2
                  WORK(ISPEC,4) = WORK(ISPEC,4) + VIS(2,INDEX) ** 2
                  END IF
               INDEX = INDEX + LNCF
 150           CONTINUE
         ELSE
C                                       Ampscalar averaging
      INCLUDE 'INCS:ZVD.INC'
            DO 250 IVIS = 1,KVIS
               IF (VIS(3,INDEX).GT.0.0) THEN
                  AMP = SQRT (VIS(1,INDEX) * VIS(1,INDEX) +
     *               VIS(2,INDEX) * VIS(2,INDEX))
                  VIS(1,INDEX) = VIS(1,INDEX) + 1.0E-20
                  COUNT(ISPEC) = COUNT(ISPEC) + 1
                  WORK(ISPEC,1) = WORK(ISPEC,1) + AMP
                  IF (DTYPE.EQ.1) WORK(ISPEC,2) = WORK(ISPEC,2) +
     *               ATAN2 (VIS(2,INDEX), VIS(1,INDEX))
                  WORK(ISPEC,3) = WORK(ISPEC,3) + AMP*AMP
                  END IF
               INDEX = INDEX + LNCF
 250        CONTINUE
         END IF
 300     CONTINUE
         GO TO 100
C                                       Average done
C                                       See if have any data.
 500  IF ((.NOT.GOTDAT) .AND. (IERR.EQ.0)) GO TO 10
      IF (.NOT.GOTDAT) GO TO 800
      GOOD1 = .FALSE.
      IF (.NOT.AMPVEC) THEN
C                                       Vector averaging
         DO 550 I = 1,NUMSPC
            GOOD1 = GOOD1 .OR. (COUNT(I).GT.0)
C                                       Amplitude
            IF ((DTYPE.EQ.0) .AND. (COUNT(I).GT.0))
     *         SCANV(I) = SQRT (WORK(I,1)*WORK(I,1) +
     *            WORK(I,2)*WORK(I,2)) / COUNT(I)
C                                       Phase
            IF ((DTYPE.EQ.1) .AND. (COUNT(I).GT.0))
     *         SCANV(I) = ATAN2 (WORK(I,2), WORK(I,1)+1.0E-20)
C                                       RMS
            IF ((DTYPE.EQ.2) .AND. (COUNT(I).GT.2)) THEN
               TEMP1 = (COUNT(I) * WORK(I,3) - (WORK(I,1)**2))
               TEMP2 = (COUNT(I) * WORK(I,4) - (WORK(I,2)**2))
               TEMP = TEMP1 + TEMP2
               IF (TEMP.LT.0.0) TEMP = 0.0
               SCANV(I) = SQRT (TEMP) / COUNT(I)
               END IF
 550           CONTINUE
      ELSE
C                                       Else Amp scalar ave
         DO 650 I = 1,NUMSPC
            GOOD1 = GOOD1 .OR. (COUNT(I).GT.0)
C                                       Amplitude
            IF ((DTYPE.EQ.0) .AND. (COUNT(I).GT.0))
     *         SCANV(I) = WORK(I,1) / COUNT(I)
C                                       Phase
            IF ((DTYPE.EQ.1) .AND. (COUNT(I).GT.0))
     *         SCANV(I) = WORK(I,2) / COUNT(I)
C                                       RMS
            IF ((DTYPE.EQ.2) .AND. (COUNT(I).GT.2)) THEN
               TEMP = (COUNT(I) * WORK(I,3) - (WORK(I,1)**2))
               IF (TEMP.LT.0.0) TEMP = 0.0
               SCANV(I) = SQRT (TEMP) / COUNT(I)
               END IF
 650        CONTINUE
C                                       end if vector averaging
         END IF
C                                       Any good values?
      IF ((.NOT.GOOD1) .AND. (IERR.EQ.0)) GO TO 10
C                                       Get source info
      IF (NUSCAN) THEN
         CALL GETSOU (SUNUM, IUDISK, IUCNO, CATUV, ISLUN, JERR)
         IF (JERR.EQ.11) THEN
C                                       Didn't find source
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
C                                       Time
 800  T1 = 0
      IF (CNTTIM.GT.0) T1 = SUMTIM / CNTTIM
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
 1700 FORMAT ('SPECAV: ERROR',I3,' READING SOURCE TABLE')
 1750 FORMAT ('SPECAV: SOURCE ',I3,' NOT IN SU TABLE')
      END
      SUBROUTINE FMTVAL (DRVAL, NCOLWD, ITYPE, LENTRY, IRET)
C-----------------------------------------------------------------------
C   Format amplitude or phase data for the specified column width.
C   Inputs:
C      DRVAL       D        Value to be formatted.
C      NCOLWD      I        Desired column width
C      ITYPE       I        Data type (1=phase; else amplitude)
C   Outputs:
C      LENTRY      C*10     String containing formatted value in
C                           character positions (1:NCOLWD)
C      IRET        I        Return code (0 => ok; else error)
C----------------------------------------------------------------------
      CHARACTER LENTRY*10
      DOUBLE PRECISION DRVAL
      INTEGER NCOLWD, ITYPE, IRET
C
      DOUBLE PRECISION DFACT(5), DROUND, DMULT, DTMP
      CHARACTER LWORK*10, LBLANK*10
      INTEGER NDIGIT, I4TMP, J
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA LBLANK /'          '/
      DATA DFACT / 1.0D0, 1.0D1, 1.0D2, 1.0D3, 1.0D4/
C----------------------------------------------------------------------
C                                       Initialisation
      IRET = 0
      LENTRY = LBLANK
C                                       Check input parameters
      IF ((NCOLWD.GT.10).OR.(NCOLWD.LT.5)) THEN
         IRET = 1
         WRITE (MSGTXT,1000) NCOLWD
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Rounding factor
      DROUND = 0.5D0
      IF (DRVAL.LT.0.0) DROUND = -0.5D0
C                                       Treat amplitude and phase
C                                       differently.
      IF (ITYPE.EQ.1) THEN
C                                       1: Phase
         NDIGIT = MAX (NCOLWD - 6, 0)
         DMULT = DFACT(NDIGIT+1)
         I4TMP = DRVAL * DMULT + DROUND
         DTMP = I4TMP / DMULT
         WRITE (LWORK,9000) DTMP
         J = NCOLWD
         IF (LWORK(NCOLWD:NCOLWD).EQ.'.') J = NCOLWD - 1
         LENTRY = LWORK(1:J)
C                                       Else: Amplitude
      ELSE
         I4TMP = DRVAL + DROUND
         WRITE (LWORK,9010) I4TMP
         J = 10 - NCOLWD + 1
         LENTRY = LWORK(J:10)
         END IF
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1000  FORMAT ('FMTVAL: CALLED WITH COLUMN WIDTH OUT OF RANGE')
9000  FORMAT (F10.4)
9010  FORMAT (I10)
      END
