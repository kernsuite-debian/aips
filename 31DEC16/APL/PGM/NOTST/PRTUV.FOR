LOCAL INCLUDE 'PRTUV.INC'
C                                       Local include for PRTUV
      INTEGER MMXSOU
C                                       MMXSOU=Max. no. source
      PARAMETER (MMXSOU=500)
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAME(3), XCLAS(2), XSOUR(4,30), XLPNAM(12)
      REAL      XSEQ, XDISK, XNCH, XBIF, XSTR, XNIT, XINC, UVRANG(2),
     *   CPARM(10), DPARM(10), DOCRT, BTIME, ETIME, BUFF(UVBFSS),
     *   BUFF2(UVBFSS), DOEBAR
      LOGICAL   MULTI, DOSWNT, SCAL, DORAND, DOAMPH
      INTEGER   DISK, CNO, SEQ, USERID, LUNP, FINDP, LUN, FIND, PAGE,
     *   IPCNT, SOUWAN(MMXSOU), NSOUWD, NACROS, BIF, VO, INC, NCH, ANT1,
     *   ANT2, NUMCH, NUMCHM, NUMIF, OTYPE, HM(2), DD(2), BUFSZ, ISUB,
     *   NVORNG, VORNG(2,1000), NCORP
      REAL      RSEC, DSEC, XAMP, UVM, XWT, NWT, MAXW, MINW, MAXA,
     *   MAXUVW
      CHARACTER NAME*12, CLASS*6, SAUCE(30)*16, TITL1*132, TITL2*132,
     *   SCRTCH*132, LINE*132, LPNAME*48, CHSIG1*1, CHSIG2*1
      COMMON /INPARM/ XNAME, XCLAS, XSEQ, XDISK, XSOUR, XNCH, XBIF,
     *   XSTR, XNIT, XINC, UVRANG, CPARM, DPARM, DOCRT, XLPNAM, DOEBAR
      COMMON /PRTUIP/ RSEC, DSEC, BTIME, ETIME, MULTI, DOSWNT, DISK,
     *   CNO, SEQ, USERID, LUNP, FINDP, LUN, FIND, PAGE, IPCNT, SOUWAN,
     *   NSOUWD, NACROS, BIF, VO, INC, NCH, ANT1, ANT2, NUMCH, NUMCHM,
     *   NUMIF, OTYPE, HM, DD, XAMP, UVM, XWT, NWT, ISUB, SCAL, MAXW,
     *   MINW, MAXA, MAXUVW, DORAND, DOAMPH, NVORNG, VORNG, NCORP
      COMMON /PRUUBF/ BUFSZ, BUFF, BUFF2
      COMMON /PRTUCS/ NAME, CLASS, SAUCE, TITL1, TITL2, SCRTCH, LINE,
     *   LPNAME, CHSIG1, CHSIG2
LOCAL END
      PROGRAM PRTUV
C-----------------------------------------------------------------------
C! Lists selected portion of a catalogued uv data base.
C# UV UV-util VLA VLB Printer
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998-2001, 2004, 2007, 2009-2013, 2015-2016
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
C   Task PRTUV lists a selected portion of a catalogd uv data base.
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      USERID         USERID        User number.
C      INNAME         NAME          File name to be listed.
C      INCLASS        CLASS         File class to be listed.
C      INSEQ          SEQ           File sequence number.
C      INDISK         DISK          Disk volumn on which file resides.
C      SOURCES        SAUCE         List of up to 30 sources to be
C                                   listed
C      CHANNEL        NCH           Spectral channel.
C      BIF            BIF           If number
C      BPRINT         VO            Visibility number to start list
C      NPRINT         XNIT          Number of components to list
C                                   default: 1 Page.
C      XINC           INC           Increment for vis.
C      UVRANGE        UVRANG        Range of UV in 1000's wavelengths
C      CPARM          CPARM         1-4: start time D, H, M, S
C                                   5-8: stop time D, H, M, S
C                                   9  : 100*ant1 + ant2
C                                   10 : Multiply amps by this.
C      DOCRT          DOCRT         > 0 => use CRT, else line printer
C      OUTPRINT       LPNAME        File name to keep printer output
C   Programmer: W. D. Cotton, Jan. 1981.  EWG (Aug 1984), PJD (Aug 1989)
C   Modified for R   uv data base July 1981.
C   CPARM(10) added by RCW Dec 1992.
C   Modified to print 1/sqrt(wt) for weight  RCW  16apr92
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PRTUV.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'PRTUV '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL PRTUIN (PRGM, IRET)
C                                       count lines
      IF (IRET.EQ.0) CALL PRTUCH (IRET)
C                                       Print it
      IF ((IRET.EQ.0) .AND. (SCAL)) CALL PRTUSC (IRET)
C                                       Print it
      IF (IRET.EQ.0) CALL PRTUDO (IRET)
C
      IRET = MAX (0, IRET)
      CALL DIE (IRET, BUFF)
C
 999  STOP
      END
      SUBROUTINE PRTUIN (PRGM, IRET)
C-----------------------------------------------------------------------
C   Startup routine for PRTUV.  Gets input parameters and converts them
C   to the internal forms.  Looks up the sources to be used.
C   Input:
C      PRGM     C*6    Task name
C   Output:
C      IRET     I      Return code, 1=OK, else failed
C   Output in comon:
C      NAME     C*12   Input file name
C      CLASS    C*6    Input file class
C      SEQ      I      Input file sequence no.
C      DISK     I      Input file disk no.
C      CNO      I      Input file catalog slot no.
C      USERID   I      User ID number.
C      LPNAME   C*48   Output file name
C      SAUCE    C(*)*16 Source names
C      NACROS   I      No. columns in output.
C      NCH      I      Start channel number
C      BIF      I      IF number
C      VO       I      First vis to consider.
C      INC      I      Increment between vis.
C      BTIME    R      Start time (days)
C      ETIME    R      End time (days)
C      ANT1     I      First antenna selected.
C      ANT2     I      Second antenna selected.
C      MULTI    L      True if a multi-source file.
C      NUMCHM   I      Possible number of channels
C      NUMCH    I      Number of channels to print
C      OTYPE    I      Output listing type (length)
C      SOURCE   C*8    Source name
C      HM       I(2)   Source RA h, m
C      RSEC     R      Source RA, s
C      CHSIG1   C*1    RA sign (' ')
C      DD       I(2)   Source Dec h, m
C      DSEC     R      Source Dec, s
C      CHSIG2   C*1    Dec sign (' ')
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
C
      INTEGER   NPARM, I, SULUN, MAXSOU, IERR, IROUND
      CHARACTER UTYPE*2, LLCH*4, MMCH*4
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PRTUV.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA SULUN /17/
C-----------------------------------------------------------------------
      IRET = 5
C                                       Init I/O
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Initialize CFIL.INC

      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 168
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAME, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         IF (IRET.EQ.1) GO TO 990
         END IF
C                                       Restart AIPS.
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      IF ((IRET.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      IF (DOCRT.GT.0.0) RQUICK = .FALSE.
      IF (RQUICK) RQUICK = LPNAME.NE.' '
      IF (RQUICK) CALL RELPOP (IRET, BUFF, IERR)
      IRET = IERR
      IF (IRET.NE.0) GO TO 999
      IRET = 8
C                                       Process ADVERB parameters
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLAS, CLASS)
      DO 15 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SAUCE(I))
 15      CONTINUE
C                                       Decode input.
      USERID = NLUSER
      SEQ = IROUND (XSEQ)
      DISK = IROUND (XDISK)
      NCH = 1
      IF (XNCH.GT.0) NCH = IROUND (XNCH)
      BIF = 1
      IF (XBIF.GT.0.1) BIF = IROUND (XBIF)
      IF (XSTR.LT.1.0) XSTR = 1.0
      VO = XSTR - 0.99
      INC = IROUND (XINC)
      INC = MAX (1, INC)
      IF (XNIT.LT.1.0) THEN
         XNIT = PRTMAX - 10.
         IF ((DOCRT.GT.0.0).OR.(LPNAME(1:1).NE.' ')) XNIT = 30000.
      ELSE IF ((DOCRT.LE.0.0).AND.(LPNAME(1:1).EQ.' ')) THEN
         XNIT = MIN (2000.0, XNIT)
         END IF
      UVRANG(1) = MAX (0.0, UVRANG(1))
      UVRANG(2) = MAX (0.0, UVRANG(2))
      IF (UVRANG(2).LE.UVRANG(1)+0.05) UVRANG(2) = 1.E10
      UVRANG(1) = UVRANG(1) ** 2
      UVRANG(2) = UVRANG(2) ** 2
      BTIME = ((CPARM(4)/60. + CPARM(3)) / 60. + CPARM(2)) / 24.
     *   + CPARM(1)
      ETIME = ((CPARM(8)/60. + CPARM(7)) / 60. + CPARM(6)) / 24.
     *   + CPARM(5)
      IF (ETIME.LE.BTIME) ETIME = 1.E9
C                                       Print Amp, Phas or Re, Im
      DOAMPH = DPARM(7).LT.0.01
C                                       antenna and subarray limits
      ANT1 = CPARM(9) / 100.0 + 0.001
      ANT2 = (CPARM(9) - 100.0 * ANT1) + 0.001
      ISUB = 100.0 * (CPARM(9) - 100*ANT1 - ANT2) + 0.01
      IF(CPARM(10).EQ.0.0) CPARM(10) = 1.0
      IF (ANT1.GE.ANT2) THEN
         I = ANT1
         ANT1 = ANT2
         ANT2 = I
         END IF
      IF ((ANT1.GT.0) .AND. (ANT2.GT.0)) THEN
         WRITE (MSGTXT,1015) ANT1, ANT2
         CALL MSGWRT (4)
      ELSE IF (ANT2.GT.0) THEN
         WRITE (MSGTXT,1016) ANT2
         CALL MSGWRT (4)
         END IF
      IF (ISUB.GT.0) THEN
         WRITE (MSGTXT,1017) ISUB
         CALL MSGWRT (4)
         END IF
C                                       scal or not scal
      SCAL = DPARM(1).LT.0
      IF (.NOT.SCAL) THEN
         MAXW = DPARM(2)
         IF (MAXW.LE.0) MAXW = 9.9
         MINW = DPARM(3)
         IF (MINW.LE.0) MINW = 0.11
         MAXA = DPARM(4)
         IF (MAXA.LE.0) MAXA = 99
         MAXUVW = DPARM(5)
         IF (MAXUVW.LE.0) MAXUVW = 10000
         END IF
C                                       Print regular or random
C                                       parameters?
      DORAND = DPARM(6).GT.0.01
C                                       Open file and get CATBLK.
      UTYPE = 'UV'
      LUN = 16
      CALL MAPOPN ('READ', DISK, NAME, CLASS, SEQ, UTYPE, USERID, LUN,
     *   FIND, CNO, CATBLK, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Mark in CFIL.INC
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 0
C                                       Get info from CATBLK.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 990
      IF (XSTR.GT.NVIS) THEN
         VO = XSTR + 0.1
         WRITE (MSGTXT,1021) VO, NVIS
         IRET = 1
         GO TO 990
         END IF
      NVORNG = 1
      VORNG(1,1) = VO + 1
      VORNG(2,1) = NVIS
C                                       Fill array of source numbers
      NSOUWD = 1
      CALL MULSDB (CATBLK, MULTI)
      IF (MULTI) THEN
         MAXSOU = MMXSOU
         CALL SOULIS (SAUCE, DISK, CNO, SULUN, MAXSOU, NSOUWD, DOSWNT,
     *      SOUWAN, IRET)
C                                       Error check
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1011) IRET
            GO TO 990
            END IF
C                                       Make sure not too many sources.
         IF (NSOUWD.GT.MMXSOU) THEN
            WRITE (MSGTXT,1010) NSOUWD, MMXSOU
            IRET = 5
            GO TO 990
            END IF
         END IF
C
      IF (CATBLK(KINAX+JLOCF).LE.1) NCH = 1
      FREQ = FREQ + (NCH-CATR(KRCRP+JLOCF)) * CATR(KRCIC+JLOCF)
C                                       Check spectral channel, IF.
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
      IF ((NCH.GT.CATBLK(KINAX+JLOCF)) .OR. (BIF.GT.NUMIF)) THEN
         IF (NCH.GT.CATBLK(KINAX+JLOCF)) WRITE (MSGTXT,1025) NCH,
     *      CATBLK(KINAX+JLOCF)
         IF (BIF.GT.NUMIF) WRITE (MSGTXT,1026) BIF, NUMIF
         IRET = 6
         GO TO 990
         END IF
C                                       Open output device
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Determine no. channels to do
C                                       and output type
      NUMCHM = CATBLK(KINAX+JLOCF) - NCH + 1
      NCORP = NCOR
      IF (DOAMPH) THEN
         NUMCH = (NACROS - 55) / (19 * NCOR)
      ELSE
         NUMCH = (NACROS - 55) / (23 * NCOR)
         END IF
      IF (NUMCH.GE.1) THEN
         OTYPE = 1
         NUMCH = MIN (NUMCH, NUMCHM)
         IF (DOAMPH) THEN
            IF (19*NCOR*NUMCH+55.LT.NACROS-3) OTYPE = 2
         ELSE
            IF (23*NCOR*NUMCH+55.LT.NACROS-3) OTYPE = 2
            END IF
      ELSE
         IF (DOAMPH) THEN
            NUMCH = (NACROS - 41) / (15 * NCOR)
         ELSE
            NUMCH = (NACROS - 41) / (19 * NCOR)
            END IF
         IF (NUMCH.GE.1) THEN
            OTYPE = 3
         ELSE
            IF (DOAMPH) THEN
               NUMCH = (NACROS - 24) / (14 * NCOR)
            ELSE
               NUMCH = (NACROS - 24) / (17 * NCOR)
               END IF
            IF (NUMCH.GE.1) THEN
               OTYPE = 4
            ELSE
 20            IF (DOAMPH) THEN
                  NUMCH = (NACROS - 14) / (14 * NCORP)
               ELSE
                  NUMCH = (NACROS - 14) / (17 * NCORP)
                  END IF
               IF (NUMCH.LE.0) THEN
                  NCORP = NCORP - 1
                  GO TO 20
                  END IF
               OTYPE = 5
               END IF
            END IF
         END IF
      NUMCH = MIN (NUMCH, NUMCHM)
C                                       Convert coordinates.
      IF (NSOUWD.GT.1) THEN
         SOURCE = 'Several '
         RA = 0.0D0
         DEC = 0.0D0
         END IF
      IF ((NSOUWD.EQ.1) .AND. (MULTI) .AND. (DOSWNT)) THEN
         CALL GETSOU (SOUWAN(1), DISK, CNO, CATBLK, SULUN, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1990) IRET, SOUWAN(1)
            GO TO 990
            END IF
         SOURCE = SNAME
         RA = RAEPO * RAD2DG
         DEC = DECEPO * RAD2DG
         END IF
C                                       use start time
      CALL FNDEXT ('NX', CATBLK, I)
      IF ((I.GT.0) .AND. ((BTIME.GT.0.0) .OR. (SAUCE(1).NE.' ')))
     *   CALL FINDVO (1000, DISK, CNO, CATBLK, ISUB, 0, NSOUWD, SOUWAN,
     *   BTIME, ETIME, NVORNG, VORNG)
C                                       RA, Dec for labels
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCR*2), LLCH)
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCD*2), MMCH)
      IF (LLCH(:2).EQ.'RA') THEN
         CALL COORDD (1, RA, CHSIG1, HM, RSEC)
      ELSE
         CALL COORDD (2, RA, CHSIG1, HM, RSEC)
         END IF
      CALL COORDD (2, DEC, CHSIG2, DD, DSEC)
      FREQ = FREQ * 1.0E-9
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR:',I7,'GETTING INPUT PARAMETERS')
 1010 FORMAT ('ERROR:',I7,' OBTAINING DATA FOR SOURCE #',I5)
 1011 FORMAT ('ERROR:',I7,'READING SOURCE TABLE')
 1015 FORMAT ('Displays limited to baseline',I3.2,' -',I3.2)
 1016 FORMAT ('Displays limited to all baselines to antenna',I3.2)
 1017 FORMAT ('Displays limited to subarray',I3.2)
 1020 FORMAT ('ERROR:',I7,' OPENING UV FILE')
 1021 FORMAT ('BPRINT',I9,' GREATER THAN NVIS',I9)
 1025 FORMAT ('CHANNEL',I5,' > MAX =',I5)
 1026 FORMAT ('IF',I5,' > MAX =',I5)
 1040 FORMAT ('ERROR',I5,' OPENING OUTPUT DEVICE')
 1990 FORMAT ('ERROR:',I7,' OBTAINING DATA FOR SOURCE #',I5)
      END
      SUBROUTINE SOULIS (SOURCS, IUDISK, IUCNO, IXLUN, MAXSOU, NSOUWD,
     *   DOSWNT, SOUWAN, IERR)
C-----------------------------------------------------------------------
C   Fills in array of source numbers to be included or excluded.
C   Inputs:
C      SOURCS(30)   C*16 Names of up to 30 sources, *=>all
C                        First character of name '-' => all except those
C                        specified.
C      IUDISK       I    Disk no on which data resides
C      IUCNO        I    Catalogue number
C      IXLUN        I    LUN to use for SU table access
C      MAXSOU       I    Maximum number of sources.
C   Output:
C      NSOUWD       I    Number of sources included or excluded; if
C                        0 all sources are included.
C      DOSWNT       L    If .TRUE. then sources in SOUWAN are included
C                        If .FALSE. then excluded.
C      SOUWAN(*)    I    The source numbers of sources included or
C                        excluded.
C      IERR         I    Return code, 0=>OK, otherwise source file
C                        exists but cannot be read.
C                        1=TABIO problem, 2=no sources or calibrators
C-----------------------------------------------------------------------
      CHARACTER  SOURCS(30)*16, SELCOD*4
      INTEGER    IUDISK, IUCNO, IXLUN, MAXSOU, NSOUWD, SOUWAN(*), IERR
      LOGICAL    DOSWNT
C
      CHARACTER  VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4, TMPNAM*16
      INTEGER    JERR, IBUFF(1024), IDSOU, QUAL, INOGRP, J,
     *   K, NSOU, SELQUA, UBUFF(1024)
      LOGICAL   T, F, EQUAL, TABLE, EXIST, FITASC, ALLSOU,
     *   DESLT, DOCALC, DOQUAL, ANYCC, NOCAL, DOAPPL
      INTEGER   NSOURC, I, ISURNO, BADCNT, SUFQID
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RAOBS, DECOBS
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC)
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION    LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Defaults for now
      SELQUA = -1
      SELCOD = '*   '
C                                       Setup for CALCODE and qualifier
C                                       selection.
      DOQUAL = SELQUA.NE.-1
      ANYCC = SELCOD.EQ.'*   '
      DOCALC = (SELCOD.NE.' ') .AND. (SELCOD.NE.'* ') .AND.
     *   (SELCOD.NE.'-CAL')
      NOCAL = SELCOD.EQ.'-CAL'
C                                       See if SU file exists.
      CALL ISTAB ('SU', IUDISK, IUCNO, 1, IXLUN, IBUFF, TABLE, EXIST,
     *   FITASC, JERR)
      IF ((JERR.NE.0) .OR. (.NOT.TABLE) .OR. (.NOT.EXIST)) GO TO 800
C                                       Open SU table
      CALL SOUINI ('READ', IBUFF, IUDISK, IUCNO, 1, CATBLK, IXLUN,
     *   INOGRP, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV,
     *   JERR)
      IF (JERR.LE.0) GO TO 20
         WRITE (MSGTXT,1000) JERR
         GO TO 990
C                                       Get number of sources.
 20   NSOURC = IBUFF(5)
C                                       Check if empty
      IF (NSOURC.LE.0) GO TO 800
      DOSWNT = T
      NSOUWD = 0
      ALLSOU = F
      NSOU = 0
C                                       Check if source excluded
C                                       or if all are included
      DO 30 J = 1,30
C                                       Sources
         ALLSOU = ALLSOU .OR. (SOURCS(J)(1:1).EQ.'*')
         DESLT = SOURCS(J)(1:1).EQ.'-'
         IF (DESLT) DOSWNT = F
C                                       Find number of sources
         EQUAL = SOURCS(J).EQ.'                '
         IF (.NOT.EQUAL) NSOU = J
C                                       Remove any minus sign
         IF (DESLT) THEN
            TMPNAM = SOURCS(J)(2:16)
            SOURCS(J) = TMPNAM
            END IF
 30      CONTINUE
C                                       Make sure need to look at table
      ALLSOU = ALLSOU .OR. (NSOU.LE.0)
      IF ((ALLSOU) .AND.
     *   (.NOT.(DOCALC.OR.ANYCC.OR.NOCAL.OR.DOQUAL))) GO TO 900
      IF (NSOU.LE.1) NSOU = 1
      BADCNT = 0
C                                       Sources
      DOAPPL = F
       IF (ALLSOU .AND.
     *   ((.NOT.((.NOT.DOAPPL).AND.(DOCALC.OR.ANYCC.OR.NOCAL)))
     *   .AND. (.NOT.DOQUAL))) GO TO 900
       IF (NSOU.LE.1) NSOU = 1
C                                       Loop through records
      DO 100 I = 1,NSOURC
         IERR = 1
C                                       Read record
         CALL TABSOU ('READ', IBUFF, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, JERR)
C                                       Check error
         IF (JERR.GT.0) THEN
            WRITE (MSGTXT,1020) JERR
            GO TO 990
            END IF
         IERR = 0
C                                       Search lists
         DO 80 J = 1,NSOU
            IF (.NOT.ALLSOU) THEN
C                                       Source list
               IF (SOURCS(J).NE.SOUNAM) GO TO 80
               END IF
C                                       Check qualifier
            IF (DOQUAL .AND. (QUAL.NE.SELQUA)) GO TO 80
C                                       Redundancy check
            IF (NSOUWD.GE.1) THEN
               DO 40 K = 1,NSOUWD
                  IF (SOUWAN(K).EQ.IDSOU) GO TO 80
 40               CONTINUE
               END IF
C                                       Add source
            IF (NSOUWD.LT.MAXSOU) THEN
               NSOUWD = NSOUWD + 1
               SOUWAN(NSOUWD) = IDSOU
C                                       Overflowed array
            ELSE
               BADCNT = BADCNT + 1
               END IF
 80         CONTINUE
 100     CONTINUE
      IF (BADCNT.GT.0) THEN
         WRITE (MSGTXT,1100) BADCNT, MAXSOU
         CALL MSGWRT (6)
         END IF
C                                       No sources found
      IF (NSOUWD.LE.0) THEN
         IERR = 2
         WRITE (MSGTXT,1101)
         GO TO 990
         END IF
      GO TO 900
C                                       No SOURCE file
 800  NSOUWD = 0
C                                       Close file
 900  IF (JERR.LE.0) CALL TABIO ('CLOS', 0, I, UBUFF, IBUFF, JERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOULIS: ERROR',I3,' OPENING SOURCE TABLE')
 1020 FORMAT ('SOULIS: ERROR',I3,' READING SOURCE TABLE')
 1100 FORMAT ('SOULIS: ',I5,' MORE SOURCES SELECTED THAN ',I6,
     *   ' ALLOWED')
 1101 FORMAT ('SOULIS: ALL SOURCES REJECTED BY SELECTION CRITERIA')
      END
      SUBROUTINE PRTUSC (IRET)
C-----------------------------------------------------------------------
C   Determines ranges of u,v,w, amplitudes, weights
C   Output:
C      IRET     I      Return code, 1=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LIMIT2, ICH1, ICH2, KK, IROUND, INDEX, J, K, KKK, I,
     *   IPOINT, NMCOR, ILOCWT, ISKIP, JLAST, IA1, IA2, BO, XCOUNT,
     *   PCOUNT, LVIS, NIO, NNCH, SOUID, LENBU, OLDSOU, BIND, KNCS,
     *   KNCF, KNCIF, ISU, IVORNG
      LOGICAL   ISCMP, WANTED, ISCROS(4)
      REAL      AMP(6), R, UVN, WEIGHT, YAMP, U, V, W, XX, YY, XDAY,
     *   TEMP
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PRTUV.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IRET = 5
C                                       init the range parameters
      XAMP = 0.0
      UVM = 0.0
      XWT = 0.0
      NWT = 1.E10
      MSGTXT = 'Finding the scaling parameters to set formats'
      CALL MSGWRT (1)
C                                       Set pointers, counters
      LIMIT2 = NCORP * NUMCH
      ICH1 = NCH
      ICH2 = NCH + NUMCH - 1
      NNCH = NCH - 1
      DO 10 J = 1,NCOR
         TEMP = CATD(KDCRV+JLOCS) + (J - CATR(KRCRP+JLOCS)) *
     *      CATR(KRCIC+JLOCS)
         I = IROUND(TEMP)
         IF (I.LE.-5) I = I + 4
         ISCROS(J) = (I.EQ.-3) .OR. (I.EQ.-4)
 10      CONTINUE
C                                       "WEIGHT" pointer for compressed
C                                       data.
      ISCMP = CATBLK(KINAX).EQ.1
      IF (ISCMP) THEN
         KNCS = INCS * 3
         KNCF = INCF * 3
         KNCIF = INCIF * 3
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            GO TO 990
            END IF
      ELSE
         KNCS = INCS
         KNCF = INCF
         KNCIF = INCIF
         ILOCWT = 0
         END IF
      OLDSOU = -1
      BUFSZ = UVBFSS * 2
      IVORNG = 0
C                                       Initialize reading VIS. file.
 60   IVORNG = IVORNG + 1
      VO = VORNG(1,IVORNG) - 1
      LENBU = 0
      BO = 1
      LVIS = VORNG(2,IVORNG) - VO
      CALL UVINIT ('READ', LUN, FIND, LVIS, VO, LREC, LENBU, BUFSZ,
     *   BUFF, BO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1090) IRET
         CALL MSGWRT (8)
         GO TO 960
         END IF
      XCOUNT = VO
      XCOUNT = XCOUNT - INC + 1
      PCOUNT = 0
      ISKIP = 1
      SOUID = 1
      NMCOR = (LREC - NRPARM) / CATBLK(KINAX)
C                                       Start looping thru data.
 100  CONTINUE
C                                       Read buffer.
         CALL UVDISK ('READ', LUN, FIND, BUFF, NIO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            CALL MSGWRT (8)
            GO TO 960
            END IF
         IPOINT = BIND + (ISKIP-1) * LREC
         IF (NIO.LE.0) THEN
            IF (IVORNG.GE.NVORNG) GO TO 960
            GO TO 60
            END IF
C                                       Loop thru buffer.
         IF (ISKIP.GT.NIO) GO TO 170
            DO 160 J = ISKIP,NIO,INC
C                                       Update counters and check
C                                       If finished.
               XCOUNT = XCOUNT + INC
               IF (XCOUNT.GT.VORNG(2,IVORNG)) THEN
                  IF (IVORNG.GE.NVORNG) GO TO 960
                  GO TO 60
                  END IF
C                                       If time, decode it.
               IF (ILOCT.GE.0) THEN
                  XDAY = BUFF(IPOINT+ILOCT)
C                                       If time check requested,
C                                       valid time and time in range
                  IF ((ISORT(:1).EQ.'T') .AND. (XDAY.GT.ETIME))
     *               GO TO 960
                  IF ((XDAY.LT.BTIME) .OR. (XDAY.GT.ETIME)) GO TO 150
               ELSE
                  XDAY = 0.
                  END IF
C                                       Determine antennas.
               IF (ILOCB.GE.0) THEN
                  IA1 = BUFF(IPOINT+ILOCB) / 256. + 0.1
                  IA2 = BUFF(IPOINT+ILOCB) - IA1*256. + 0.1
                  ISU = 100.0 * (BUFF(IPOINT+ILOCB) - IA1*256 - IA2)+1.5
               ELSE
                  IA1 = BUFF(IPOINT+ILOCA1) + 0.1
                  IA2 = BUFF(IPOINT+ILOCA2) + 0.1
                  ISU = BUFF(IPOINT+ILOCSA) + 0.1
                  END IF
               IF ((ISUB.GT.0) .AND. (ISUB.NE.ISU)) GO TO 150
               IF ((ANT1.GT.0) .AND. ((ANT1.NE.IA1) .OR.
     *            (ANT1.NE.IA2))) GO TO 150
               IF ((ANT1.LE.0) .AND. (ANT2.GT.0) .AND. (ANT2.NE.IA1)
     *            .AND. (ANT2.NE.IA2)) GO TO 150
C                                       Check source
               IF (MULTI) SOUID = IROUND (BUFF(IPOINT+ILOCSU))
               CALL SOURS (.FALSE., SOUID, OLDSOU, WANTED, IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'ERROR PROCESSING NEW SOURCE'
                  CALL MSGWRT (8)
                  GO TO 990
                  END IF
               IF (.NOT.WANTED) GO TO 150
C                                       Convert uvw to kilo lamda.
               U = BUFF(IPOINT+ILOCU) * 0.001
               V = BUFF(IPOINT+ILOCV) * 0.001
               W = BUFF(IPOINT+ILOCW) * 0.001
               R = U * U + V * V
               IF ((R.LT.UVRANG(1)) .OR. (R.GT.UVRANG(2))) GO TO 150
               UVM = MAX (UVM, U)
               UVM = MAX (UVM, V)
               UVM = MAX (UVM, W)
               UVN = U
               UVN = MIN (UVN, V)
               UVN = MIN (UVN, W)
               UVM = MAX (UVM, -10.0*UVN)
C                                       Get vis.
C                                       Unpack compressed data
               IF (ISCMP) CALL ZUVXPN (NMCOR, BUFF(IPOINT+NRPARM),
     *            BUFF(IPOINT+ILOCWT), BUFF2)
               K = 0
               YAMP = 0.0
               DO 120 KK = ICH1,ICH2
                  DO 110 KKK = 1,NCOR
                     K = K + 1
C                                       Compressed
                     IF (ISCMP) THEN
                        INDEX = 1 + (KK-1)*KNCF + (KKK-1)*KNCS +
     *                     (BIF-1)*KNCIF
                        XX = BUFF2(INDEX)
                        YY = BUFF2(INDEX+1)
                        WEIGHT = BUFF2(INDEX+2)
C                                       Expanded
                     ELSE
                        INDEX = IPOINT + NRPARM + (KK-1)*KNCF +
     *                     (KKK-1)*KNCS + (BIF-1)*KNCIF
                        XX = BUFF(INDEX)
                        YY = BUFF(INDEX+1)
                        WEIGHT = BUFF(INDEX+2)
                        END IF
                     IF (WEIGHT.NE.0.0) THEN
                        XWT = MAX (XWT, ABS(WEIGHT))
                        NWT = MIN (NWT, ABS(WEIGHT))
                        END IF
                     IF ((IA1.NE.IA2) .OR. (ISCROS(KKK))) THEN
                        AMP(K) = SQRT (XX*XX+YY*YY) * CPARM(10)
                     ELSE
                        AMP(K) = XX * CPARM(10)
                        END IF
                     XAMP = MAX (XAMP, AMP(K))
                     YAMP = MIN (YAMP, AMP(K))
 110                 CONTINUE
 120              CONTINUE
C                                       Write VIS data: CRT
               XAMP = MAX (XAMP, -10.0*YAMP)
               PCOUNT = PCOUNT + 1
               IF (PCOUNT.GE.XNIT) GO TO 960
C                                       Update IPOINT
 150           IPOINT = IPOINT + LREC * MIN (LENBU, INC)
               JLAST = J
 160           CONTINUE
 170     ISKIP = ISKIP - NIO
         IF (ISKIP+NIO.LE.NIO) ISKIP = INC - (NIO-JLAST)
         GO TO 100
C                                       Close files.
 960  IF (IRET.LE.0) IRET = 0
C
      GO TO 999
C                                        Write end message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1090 FORMAT ('ERROR:',I7,' INITIALIZING UV FILE')
 1100 FORMAT ('ERROR:',I7,' READING VIS ')
      END
      SUBROUTINE PRTUCH (IRET)
C-----------------------------------------------------------------------
C   Counts lines of print
C   Output:
C      IRET     I      Return code, 1=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LIMIT2, ICH1, ICH2, IROUND, I, J, IPOINT, NMCOR, ISKIP,
     *   JLAST, IA1, IA2, BO, XCOUNT, PCOUNT, LVIS, NIO, NNCH, SOUID,
     *   LENBU, OLDSOU, BIND, JTRIM, ISU, IFQ, NCOUNT, TTY(2), IERR,
     *   IVORNG
      CHARACTER STR*4
      LOGICAL   F, WANTED
      REAL      R, U, V, W
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PRTUV.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
      IF (.NOT.SCAL) THEN
         XAMP = MAXA
         XWT = MAXW
         NWT = MINW
         UVM = MAXUVW
         END IF
      IRET = 5
C                                       Set pointers, counters
      LIMIT2 = NCORP * NUMCH
      ICH1 = NCH
      ICH2 = NCH + NUMCH - 1
      NNCH = NCH - 1
C                                       first page
      NCOUNT = 0
      J = JTRIM (ISORT)
      PAGE = 0
      IPCNT = 998
      TITL1 = ' '
      TITL2 = ' '
      IF (DOCRT.GT.-2.5) THEN
         IF (((OTYPE.GE.1) .AND. (OTYPE.LE.3)) .AND. ((BIF.GT.1) .OR.
     *      (NCH.GT.1))) THEN
            NCOUNT = NCOUNT + 1
            END IF
         NCOUNT = NCOUNT + 1
         NCOUNT = NCOUNT + 4
         END IF
      IVORNG = 0
      BO = 1
      BUFSZ = UVBFSS  * 2
      PCOUNT = 0
      PAGE = 1
      SOUID = 1
      NMCOR = (LREC - NRPARM) / CATBLK(KINAX)
      IFQ = 0
C                                       Initialize reading VIS. file.
 60   IVORNG = IVORNG + 1
      VO = VORNG(1,IVORNG) - 1
      LENBU = 0
      LVIS = VORNG(2,IVORNG) - VO
      OLDSOU = -1
      CALL UVINIT ('READ', LUN, FIND, LVIS, VO, LREC, LENBU, BUFSZ,
     *   BUFF, BO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1090) IRET
         CALL MSGWRT (8)
         GO TO 960
         END IF
      XCOUNT = VO
      XCOUNT = XCOUNT - INC + 1
      ISKIP = 1
C                                       Start looping thru data.
 100  CONTINUE
C                                       Read buffer.
         CALL UVDISK ('READ', LUN, FIND, BUFF, NIO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            CALL MSGWRT (8)
            GO TO 960
            END IF
         IPOINT = BIND + (ISKIP-1) * LREC
         IF (NIO.LE.0) THEN
            IF (IVORNG.GE.NVORNG) GO TO 900
            GO TO 60
            END IF
C                                       Loop thru buffer.
         IF (ISKIP.GT.NIO) GO TO 170
            DO 160 J = ISKIP,NIO,INC
C                                       Update counters and check
C                                       If finished.
               XCOUNT = XCOUNT + INC
               IF (XCOUNT.GT.VORNG(2,IVORNG)) THEN
                  IF (IVORNG.GE.NVORNG) GO TO 900
                  GO TO 60
                  END IF
C                                       Determine antennas.
               IF (ILOCB.GE.0) THEN
                  IA1 = BUFF(IPOINT+ILOCB) / 256. + 0.1
                  IA2 = BUFF(IPOINT+ILOCB) - IA1*256. + 0.1
                  ISU = 100.0 * (BUFF(IPOINT+ILOCB) - IA1*256 - IA2)+1.5
               ELSE
                  IA1 = BUFF(IPOINT+ILOCA1) + 0.1
                  IA2 = BUFF(IPOINT+ILOCA2) + 0.1
                  ISU = BUFF(IPOINT+ILOCSA) + 0.1
                  END IF
               IF ((ISUB.GT.0) .AND. (ISUB.NE.ISU)) GO TO 150
               IF ((ANT1.GT.0) .AND. ((ANT1.NE.IA1) .OR.
     *            (ANT2.NE.IA2))) GO TO 150
               IF ((ANT1.LE.0) .AND. (ANT2.GT.0) .AND. (ANT2.NE.IA1)
     *            .AND. (ANT2.NE.IA2)) GO TO 150
C                                       Check source
               IF (MULTI) SOUID = IROUND (BUFF(IPOINT+ILOCSU))
               CALL SOURS (.TRUE., SOUID, OLDSOU, WANTED, IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'ERROR PROCESSING NEW SOURCE'
                  CALL MSGWRT (8)
                  GO TO 960
                  END IF
               IF (.NOT.WANTED) GO TO 150
               IF (ILOCFQ.GE.0) IFQ = BUFF(IPOINT+ILOCFQ) + 0.1
C                                       Convert uvw to kilo lamda.
               U = BUFF(IPOINT+ILOCU) * 0.001
               V = BUFF(IPOINT+ILOCV) * 0.001
               W = BUFF(IPOINT+ILOCW) * 0.001
               R = U * U + V * V
               IF ((R.LT.UVRANG(1)) .OR. (R.GT.UVRANG(2))) GO TO 150
C                                       Write VIS data
               NCOUNT = NCOUNT + 1
               PCOUNT = PCOUNT + 1
               IF (PCOUNT.GE.XNIT) GO TO 900
C                                       Update IPOINT
 150           IPOINT = IPOINT + LREC * MIN (LENBU, INC)
               JLAST = J
 160           CONTINUE
 170     ISKIP = ISKIP - NIO
         IF (ISKIP+NIO.LE.NIO) ISKIP = INC - (NIO-JLAST)
         GO TO 100
C                                       check count
 900  IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
         IF (NCOUNT.GT.1000) THEN
            IRET = -1
            PCOUNT = -1
            CALL LPCLOS (LUNP, FINDP, PCOUNT, IERR)
            END IF
      ELSE IF (NCOUNT.GT.500) THEN
         IPCNT = 0
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRTCH, .FALSE., .FALSE.,
     *      .TRUE., IRET)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IRET.GT.0) GO TO 950
         WRITE (SCRTCH,1900) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, IRET)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IRET.GT.0) GO TO 950
         SCRTCH = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRTCH, 1, STR, IRET)
         IF (IRET.GT.0) GO TO 950
         IF ((STR(:1).NE.'y') .AND. (STR(:1).NE.'Y')) THEN
            PCOUNT = -1
            CALL LPCLOS (LUNP, FINDP, PCOUNT, IERR)
            IRET = -1
            SCRTCH = 'Good choice - save trees'
         ELSE
            SCRTCH = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      GO TO 999
C                                       CRT error
 950  CALL MSGWRT (8)
C                                       Close files.
 960  IF (IRET.GT.0) CALL LPCLOS (LUNP, FINDP, IPCNT, I)
      IF (IRET.LE.0) IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1090 FORMAT ('ERROR:',I7,' INITIALIZING UV FILE')
 1100 FORMAT ('ERROR:',I7,' READING VIS ')
 1900 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE PRTUDO (IRET)
C-----------------------------------------------------------------------
C   Prints selected portions of uv data file.
C   Output:
C      IRET     I      Return code, 1=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER  MAXRP
      PARAMETER (MAXRP = 15)
      CHARACTER RLIST(18)*8, REXIST(15)*8, R4CHAR(15)*8, OUTRAN(15)*8
      INTEGER   PRMTRN(MAXRP), NPRM, ANOTA(15), LOCLIS(15),
     *   LOCSEL(15), NPASEL, KINT, KREAL, RANINT(15), NLOC, ILOCSC,
     *   ILFIND, IERR, IVORNG
      REAL   RANREA(15)
      INTEGER   LIMIT2, ICH1, ICH2, II, JJ, KK, IROUND, INDEX, I, J, L,
     *   K, KKK, IPOINT, NMCOR, ILOCWT, ISKIP, JLAST, ICH(6), IA1, IA2,
     *   BO, XCOUNT, PCOUNT, LVIS, PHASE(6), IWT(6), NIO, NNCH, SOUID,
     *   LENBU, OLDSOU, ITT(4), JCOR, BIND, KNCS, KNCF, KNCIF, JTRIM,
     *   WTFM, ISU, IFQ, IU, IV, IW, JTT(3), NC, KRNC, KINC
      CHARACTER ISTOKE(6)*2, JSTOKE(4,3)*2, LLCH*4, MMCH*4, TCHAR*24,
     *   WCHAR*2
      LOGICAL   F, ISCMP, WANTED, ISCROS(4)
      REAL      AMP(6), R, WEIGHT, TEMP, TPHS, U, V, W, XX, YY, RWT(6),
     *   XDAY, WTSC, RTT
C
      REAL      RE(6), IM(6)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PRTUV.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA F /.FALSE./
      DATA JSTOKE /'XX','YY','XY','YX','RR','LL','RL','LR',
     *   'I ','Q ','U ','V '/
      DATA RLIST /'UU-L    ','VV-L    ','WW-L    ','TIME1   ',
     *   'BASELINE','SOURCE  ','FREQSEL ','INTTIM  ',
     *   'GATEID  ','CORR-ID ','FILTER  ','DATE    ',
     *   'WEIGHT  ','SCALE   ','REMOVED', 'SUBARRAY', 'AMTENNA1',
     *   'ANTENNA2'/
C-----------------------------------------------------------------------
C                                       LOCLIS for the undetermined
C                                       ILOCs are put zero
      LOCLIS(1) = ILOCU
      LOCLIS(2) = ILOCV
      LOCLIS(3) = ILOCW
      LOCLIS(4) = ILOCT
      LOCLIS(5) = ILOCB
      LOCLIS(6) = ILOCSU
      LOCLIS(7) = ILOCFQ
      LOCLIS(8) = ILOCIT
      LOCLIS(9) = 100
      LOCLIS(10) = ILOCID
      LOCLIS(11) = 100
      LOCLIS(12) = 100
      LOCLIS(13) = 100
      LOCLIS(14) = 100
      LOCLIS(15) = 100
      IF (.NOT.SCAL) THEN
         XAMP = MAXA
         XWT = MAXW
         NWT = MINW
         UVM = MAXUVW
         END IF
      IRET = 5
C                                       Set pointers, counters
      LIMIT2 = NCORP * NUMCH
      ICH1 = NCH
      ICH2 = NCH + NUMCH - 1
      NNCH = NCH - 1
C                                       "WEIGHT" pointer for compressed
C                                       data.
      ISCMP = CATBLK(KINAX).EQ.1
      IF (ISCMP) THEN
         KNCS = INCS * 3
         KNCF = INCF * 3
         KNCIF = INCIF * 3
         CALL AXEFND (8, 'SCALE   ', CATBLK(KIPCN), CATH(KHPTP), ILOCSC,
     *      IERR)
         IF (IERR .EQ. 0) LOCLIS(14) = ILOCSC
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      IRET)
         IF (IRET .EQ. 0) LOCLIS(13) = ILOCWT
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            GO TO 990
            END IF
      ELSE
         KNCS = INCS
         KNCF = INCF
         KNCIF = INCIF
         ILOCWT = 0
         END IF
C                                       Stokes labels
      DO 15 II = 1,4,NCOR
         NNCH = NNCH + 1
         DO 10 JJ = 1,NCOR
            INDEX = II + JJ - 1
            ICH(INDEX) = NNCH
            TEMP = CATD(KDCRV+JLOCS) + (JJ - CATR(KRCRP+JLOCS)) *
     *         CATR(KRCIC+JLOCS)
            I = IROUND(TEMP)
            IF (I.LE.-5) THEN
               L = 1
               I = I + 4
            ELSE IF (I.LT.0) THEN
               L = 2
            ELSE
               L = 3
               END IF
            ISCROS(JJ) = (I.EQ.-3) .OR. (I.EQ.-4)
            I = ABS(I)
            ISTOKE(INDEX) = JSTOKE(I,L)
 10         CONTINUE
 15      CONTINUE
C                                       RA-Dec labels
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCR*2), LLCH)
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCD*2), MMCH)
      WCHAR = 'Wt'
      IF (DOEBAR.GT.0.0) WCHAR = 'Si'
      IF (DOAMPH) THEN
         IF ((OTYPE.EQ.1) .OR. (OTYPE.EQ.2)) THEN
            TCHAR = '    Amp Phas'
            IF (XAMP.LE.99.9) TCHAR = '   Amp  Phas'
            TCHAR(18-WTFM:) = WCHAR
         ELSE IF (OTYPE.EQ.3) THEN
            TCHAR = '    Amp Phas ' // WCHAR
            IF (XAMP.LE.99.9) TCHAR = '   Amp  Phas ' // WCHAR
         ELSE
            TCHAR = '   Amp Phas ' // WCHAR
            IF (XAMP.LE.99.9) TCHAR = '  Amp  Phas ' // WCHAR
            END IF
      ELSE
         IF ((OTYPE.EQ.1) .OR. (OTYPE.EQ.2)) THEN
            TCHAR = '     Re      Im'
            IF (XAMP.LE.99.9) TCHAR = '   Re      Im'
            TCHAR(22-WTFM:) = WCHAR
         ELSE IF (OTYPE.EQ.3) THEN
            TCHAR = '     Re      Im  ' // WCHAR
            IF (XAMP.LE.99.9) TCHAR = '   Re      Im    ' // WCHAR
         ELSE
            TCHAR = '    Re     Im  ' // WCHAR
            IF (XAMP.LE.99.9) TCHAR = '  Re     Im    ' // WCHAR
            END IF
         END IF
      IF (XWT.LE.0.0) THEN
         XWT = 1.0
         NWT = 1.0
         END IF
      IF (DOEBAR.GT.0) THEN
         TEMP = 1.0 / SQRT (XWT)
         XWT = 1.0 / SQRT (NWT)
         NWT = TEMP
         END IF
C                                       weight scaling
      TEMP = LOG10 (XWT/0.995)
      I = TEMP + 99.0
      I =  100 - I
      WTSC = 10.0 ** I
      IF (OTYPE.LT.3) THEN
         TEMP = XWT / NWT
         IF (TEMP.GT.100000.0) THEN
            MSGTXT = 'Full dynamic range of weights cannot be printed'
            CALL MSGWRT (6)
            END IF
         WTFM = 3
         IF ((XWT.LT.9.99995) .AND. (NWT.GE.0.0001)) THEN
            WTSC = 1.0
            WTFM = 4
            IF (XWT.LT.0.09995) WTSC = 10.0
         ELSE IF ((XWT.LT.99.9995) .AND. (NWT.GE.0.001)) THEN
            WTSC = 1.0
            WTFM = 3
         ELSE IF ((XWT.LT.999.995) .AND. (NWT.GE.0.01)) THEN
            WTSC = 1.0
            WTFM = 2
         ELSE IF ((XWT.LT.9999.95) .AND. (NWT.GE.0.1)) THEN
            WTSC = 1.0
            WTFM = 1
            END IF
      ELSE
         TEMP = XWT / NWT
         IF (TEMP.GT.99.50) THEN
            MSGTXT = 'Full dynamic range of weights cannot be printed'
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Write header
C                                       first page
      J = JTRIM (ISORT)
      PAGE = 0
      IPCNT = 998
      TITL1 = ' '
      TITL2 = ' '
      IF (DOCRT.GT.-2.5) THEN
         IF (NACROS.GE.90) THEN
            WRITE (LINE,1050) NAME, CLASS, SEQ, DISK, USERID, ICH1, ICH2
         ELSE
            WRITE (LINE,1051) NAME, CLASS, SEQ, DISK, USERID, ICH1, ICH2
            END IF
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 950
         WRITE (LINE,1055) SOURCE, LLCH, CHSIG1, HM, RSEC, MMCH, CHSIG2,
     *      DD, DSEC, BIF
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 950
         WRITE (LINE,1056) FREQ, NCOR, NVIS, ISORT
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 950
         IF (((OTYPE.GE.1) .AND. (OTYPE.LE.3)) .AND. ((BIF.GT.1) .OR.
     *      (NCH.GT.1))) THEN
            LINE = 'NOTE: U,V and W are in wavelengths at the reference'
     *         // ' frequency'
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 950
            END IF
         IF (WTSC.NE.1.0) THEN
            WRITE (LINE,1057) WTSC
            IF (DOEBAR.GT.0.0) LINE(:7) = 'Sigmas '
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 950
            END IF
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 950
         END IF
C                                       Page titles
      IF (.NOT.DORAND) THEN
         IF (DOAMPH) THEN
            IF (OTYPE.EQ.1) THEN
               WRITE (TITL1,1060) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *            ISTOKE(JCOR), JCOR = 1,LIMIT2)
               WRITE (TITL2,1070) (TCHAR(:19), JCOR = 1,LIMIT2)
            ELSE IF (OTYPE.EQ.2) THEN
               WRITE (TITL1,1061) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *            ISTOKE(JCOR), JCOR = 1,LIMIT2)
               WRITE (TITL2,1071) (TCHAR(:19), JCOR = 1,LIMIT2)
            ELSE IF (OTYPE.EQ.3) THEN
               WRITE (TITL1,1062) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *            ISTOKE(JCOR), JCOR = 1,LIMIT2)
               WRITE (TITL2,1072) (TCHAR(:15), JCOR = 1,LIMIT2)
            ELSE IF (OTYPE.EQ.4) THEN
               WRITE (TITL1,1063) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *            ISTOKE(JCOR), JCOR = 1,LIMIT2)
               WRITE (TITL2,1073) (TCHAR(:14), JCOR = 1,LIMIT2)
            ELSE IF (OTYPE.EQ.5) THEN
               WRITE (TITL1,1064) SOURCE, ISORT, (ICH(JCOR),
     *            ISTOKE(JCOR), JCOR = 1,LIMIT2)
               WRITE (TITL2,1074) (TCHAR(:14), JCOR = 1,LIMIT2)
               END IF
         ELSE
            IF (OTYPE.EQ.1) THEN
               WRITE (TITL1,3060) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *            ISTOKE(JCOR), JCOR = 1,LIMIT2)
               WRITE (TITL2,1070) (TCHAR(:23), JCOR = 1,LIMIT2)
            ELSE IF (OTYPE.EQ.2) THEN
               WRITE (TITL1,3061) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *            ISTOKE(JCOR), JCOR = 1,LIMIT2)
               WRITE (TITL2,1071) (TCHAR(:23), JCOR = 1,LIMIT2)
            ELSE IF (OTYPE.EQ.3) THEN
               WRITE (TITL1,3062) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *            ISTOKE(JCOR), JCOR = 1,LIMIT2)
               WRITE (TITL2,1072) (TCHAR(:19), JCOR = 1,LIMIT2)
            ELSE IF (OTYPE.EQ.4) THEN
               WRITE (TITL1,3063) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *            ISTOKE(JCOR), JCOR = 1,LIMIT2)
               WRITE (TITL2,1073) (TCHAR(:17), JCOR = 1,LIMIT2)
            ELSE IF (OTYPE.EQ.5) THEN
               WRITE (TITL1,3064) SOURCE, ISORT, (ICH(JCOR),
     *            ISTOKE(JCOR), JCOR = 1,LIMIT2)
               WRITE (TITL2,1074) (TCHAR(:17), JCOR = 1,LIMIT2)
               END IF
            END IF
C                                       title of the random parameters
      ELSE
C                                       list of the random parameters
         CALL LISRAN (OUTRAN, ANOTA)
C                                       remove REMOVEDs
         CALL PRMSET (CATBLK, PRMTRN)
         NPRM = CATBLK(KIPCN)
C                                       restructure RPARM
C                                       form the real  group
         KREAL = 0
         DO 25 K = 1,NPRM
            REXIST(K) = OUTRAN(PRMTRN(K))
            IF (ANOTA(PRMTRN(K)).EQ.1) THEN
               KREAL = KREAL + 1
               NLOC = 0
               DO 20 J = 1,18
                  NC = 8
                  IF (K.LE.3) NC = 4
                  IF (REXIST(K)(:NC).EQ.RLIST(J)(:NC)) NLOC = J
 20               CONTINUE
               IF (NLOC.EQ.0) THEN
                  LOCSEL(KREAL) = 100
               ELSE IF (LOCLIS(NLOC).EQ.100) THEN
                  CALL AXEFND (8, REXIST(K), CATBLK(KIPCN), CATH(KHPTP),
     *               ILFIND, IERR)
                  IF (IERR.EQ.0) LOCSEL(KREAL) = ILFIND
                  IF (IERR.NE.0) LOCSEL(KREAL) = 100
               ELSE
                  LOCSEL(KREAL) = LOCLIS(NLOC)
                  END IF
               R4CHAR(KREAL) = REXIST(K)
               IF (LOCSEL(KREAL).EQ.100) KREAL = KREAL -1
               END IF
 25         CONTINUE
C                                       form the integer group
         KK = KREAL
         DO 35 K = 1,NPRM
            REXIST(K) = OUTRAN(PRMTRN(K))
            IF (ANOTA(PRMTRN(K)).EQ.0) THEN
               KK = KK + 1
               NLOC = 0
               DO 30 J = 1,15
                  IF (REXIST(K).EQ.RLIST(J)) NLOC = J
 30               CONTINUE
               IF (NLOC.EQ.0) THEN
                  LOCSEL(KK) = 100
               ELSE IF (LOCLIS(NLOC).EQ.100) THEN
                  CALL AXEFND (8, REXIST(K), CATBLK(KIPCN), CATH(KHPTP),
     *               ILFIND, IERR)
                  IF (IERR.EQ.0) LOCSEL(KK) = ILFIND
                  IF (IERR.NE.0) LOCSEL(KK) = 100
               ELSE
                  LOCSEL(KK) = LOCLIS(NLOC)
                  END IF
               R4CHAR(KK) = REXIST(K)
               IF (LOCSEL(KK).EQ.100) KK = KK - 1
               END IF
 35         CONTINUE
C                                       titles
         NPASEL = KK
         KINT = KK - KREAL
         KRNC = 10
         KINC = 5
 40      KK = (NACROS - 36 - KRNC*KREAL - KINC*KINT)
         IF (KK.GT.2*KREAL+KINT) THEN
            KRNC = KRNC + 2
            KINC = KINC + 1
            GO TO 40
         ELSE IF (KK.GT.NPASEL) THEN
            KRNC = KRNC + 1
            KINC = KINC + 1
            GO TO 40
         ELSE IF (KK.GT.KREAL) THEN
            KRNC = KRNC + 1
            END IF
         KRNC = MIN (15, KRNC)
         KINC = MIN (10, KINC)
         TITL1 = ' '
         WRITE (TITL2,2070)
         KK = 36
         DO 45 K = 1,KREAL
            KK = KK + KRNC - 8
            TITL2(KK:) = R4CHAR(K)
            KK = KK + 8
 45         CONTINUE
         J = MIN (8, KINC-1)
         DO 50 K = KREAL+1,NPASEL
            KK = KK + KINC
            TITL2(KK-J:) = R4CHAR(K)(:J)
 50         CONTINUE
         END IF
      IVORNG = 0
      BO = 1
      BUFSZ = UVBFSS  * 2
      PCOUNT = 0
      PAGE = 1
      SOUID = 1
      NMCOR = (LREC - NRPARM) / CATBLK(KINAX)
      IFQ = 0
C                                       Initialize reading VIS. file.
 60   IVORNG = IVORNG + 1
      VO = VORNG(1,IVORNG) - 1
      LENBU = 0
      LVIS = VORNG(2,IVORNG) - VO
      OLDSOU = -1
      CALL UVINIT ('READ', LUN, FIND, LVIS, VO, LREC, LENBU, BUFSZ,
     *   BUFF, BO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1090) IRET
         CALL MSGWRT (8)
         GO TO 960
         END IF
      XCOUNT = VO
      XCOUNT = XCOUNT - INC + 1
      ISKIP = 1
C                                       Start looping thru data.
 100  CONTINUE
C                                       Read buffer.
         CALL UVDISK ('READ', LUN, FIND, BUFF, NIO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            CALL MSGWRT (8)
            GO TO 960
            END IF
         IPOINT = BIND + (ISKIP-1) * LREC
         IF (NIO.LE.0) THEN
            IF (IVORNG.GE.NVORNG) GO TO 960
            GO TO 60
            END IF
C                                       Loop thru buffer.
         IF (ISKIP.GT.NIO) GO TO 170
            DO 160 J = ISKIP,NIO,INC
C                                       Update counters and check
C                                       If finished.
               XCOUNT = XCOUNT + INC
               IF (XCOUNT.GT.VORNG(2,IVORNG)) THEN
                  IF (IVORNG.GE.NVORNG) GO TO 960
                  GO TO 60
                  END IF
C                                       If time, decode it.
               IF (ILOCT.GE.0) THEN
                  XDAY = BUFF(IPOINT+ILOCT)
C                                       If time check requested,
C                                       valid time and time in range
                  IF ((ISORT(:1).EQ.'T') .AND. (XDAY.GT.ETIME))
     *               GO TO 960
                  IF ((XDAY.LT.BTIME) .OR. (XDAY.GT.ETIME)) GO TO 150
               ELSE
                  XDAY = 0.
                  END IF
               CALL TODHMS (XDAY, ITT)
               IF (DORAND) THEN
                  CALL T2DHMS (XDAY, 3, JTT, RTT)
               ELSE
                  CALL T2DHMS (XDAY, 2, JTT, RTT)
                  END IF
C                                       Determine antennas.
               IF (ILOCB.GE.0) THEN
                  IA1 = BUFF(IPOINT+ILOCB) / 256. + 0.1
                  IA2 = BUFF(IPOINT+ILOCB) - IA1*256. + 0.1
                  ISU = 100.0 * (BUFF(IPOINT+ILOCB) - IA1*256 - IA2)+1.5
               ELSE
                  IA1 = BUFF(IPOINT+ILOCA1) + 0.1
                  IA2 = BUFF(IPOINT+ILOCA2) + 0.1
                  ISU = BUFF(IPOINT+ILOCSA) + 0.1
                  END IF
               IF ((ISUB.GT.0) .AND. (ISUB.NE.ISU)) GO TO 150
               IF ((ANT1.GT.0) .AND. ((ANT1.NE.IA1) .OR.
     *            (ANT2.NE.IA2))) GO TO 150
               IF ((ANT1.LE.0) .AND. (ANT2.GT.0) .AND. (ANT2.NE.IA1)
     *            .AND. (ANT2.NE.IA2)) GO TO 150
C                                       Check source
               IF (MULTI) SOUID = IROUND (BUFF(IPOINT+ILOCSU))
               CALL SOURS (.TRUE., SOUID, OLDSOU, WANTED, IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'ERROR PROCESSING NEW SOURCE'
                  CALL MSGWRT (8)
                  GO TO 990
                  END IF
               IF (.NOT.WANTED) GO TO 150
               IF (ILOCFQ.GE.0) IFQ = BUFF(IPOINT+ILOCFQ) + 0.1
C                                       Convert uvw to kilo lamda.
               U = BUFF(IPOINT+ILOCU) * 0.001
               V = BUFF(IPOINT+ILOCV) * 0.001
               W = BUFF(IPOINT+ILOCW) * 0.001
               R = U * U + V * V
               IF ((R.LT.UVRANG(1)) .OR. (R.GT.UVRANG(2))) GO TO 150
               IU = IROUND (U)
               IV = IROUND (V)
               IW = IROUND (W)
C                                       Get vis.
C                                       Unpack compressed data
               IF (ISCMP) CALL ZUVXPN (NMCOR, BUFF(IPOINT+NRPARM),
     *            BUFF(IPOINT+ILOCWT), BUFF2)
               K = 0
               DO 120 KK = ICH1,ICH2
                  DO 110 KKK = 1,NCOR
                     K = K + 1
C                                       Compressed
                     IF (ISCMP) THEN
                        INDEX = 1 + (KK-1)*KNCF + (KKK-1)*KNCS +
     *                     (BIF-1)*KNCIF
                        XX = BUFF2(INDEX)
                        YY = BUFF2(INDEX+1)
                        WEIGHT = BUFF2(INDEX+2)
C                                       Expanded
                     ELSE
                        INDEX = IPOINT + NRPARM + (KK-1)*KNCF +
     *                     (KKK-1)*KNCS + (BIF-1)*KNCIF
                        XX = BUFF(INDEX)
                        YY = BUFF(INDEX+1)
                        WEIGHT = BUFF(INDEX+2)
                        END IF
                     TPHS = WEIGHT * WTSC
C                                       For the long printout, set
C                                       weight to the 'sigma'.
C                                       For integer printouts, this
C                                       may cause problems so wait.
                     RWT(K) = TPHS
                     IF ((DOEBAR.GT.0) .AND. (WEIGHT.NE.0.0)) THEN
                        RWT(K) = WTSC / SQRT (ABS(WEIGHT))
                        IF (WEIGHT.LT.0.0) RWT(K) = -RWT(K)
                        END IF
                     IWT(K) = IROUND (RWT(K))
                     IF (IWT(K).EQ.0) THEN
                        IF (TPHS.LT.0.0) IWT(K) = -1
                        IF (TPHS.GT.0.0) IWT(K) = 1
                        END IF
                     IF ((IA1.NE.IA2) .OR. (ISCROS(KKK))) THEN
                        AMP(K) = SQRT (XX*XX+YY*YY) * CPARM(10)
                        TPHS = 57.296 * ATAN2 (YY, XX+1.0E-20)
                        PHASE(K) = IROUND (TPHS)
                     ELSE
                        AMP(K) = XX * CPARM(10)
                        PHASE(K) = 0.0
                        END IF
C
                     RE(K) = XX * CPARM(10)
                     IM(K) = YY * CPARM(10)
 110                 CONTINUE
 120              CONTINUE
C                                       print random parameters
               IF (DORAND) THEN
                  DO 125 K = 1,NPASEL
                     IF (K.LE.KREAL) THEN
                        RANREA(K) = BUFF(IPOINT+LOCSEL(K))
                     ELSE
                        RANINT(K-KREAL) = BUFF(IPOINT+LOCSEL(K))
                        END IF
 125                 CONTINUE
                  WRITE (LINE,2120) XCOUNT, JTT(1), JTT(2), JTT(3), RTT,
     *               IA1, IA2, ISU
                  IF (LINE(18:18).EQ.' ') LINE(18:18) = '0'
                  IF (KRNC.EQ.10) THEN
                     WRITE (LINE(36:),2121) (RANREA(K), K = 1,KREAL)
                  ELSE IF (KRNC.EQ.11) THEN
                     WRITE (LINE(36:),2122) (RANREA(K), K = 1,KREAL)
                  ELSE IF (KRNC.EQ.12) THEN
                     WRITE (LINE(36:),2123) (RANREA(K), K = 1,KREAL)
                  ELSE IF (KRNC.EQ.13) THEN
                     WRITE (LINE(36:),2124) (RANREA(K), K = 1,KREAL)
                  ELSE IF (KRNC.EQ.14) THEN
                     WRITE (LINE(36:),2125) (RANREA(K), K = 1,KREAL)
                  ELSE IF (KRNC.EQ.15) THEN
                     WRITE (LINE(36:),2126) (RANREA(K), K = 1,KREAL)
                     END IF
                  IF (KINC.EQ.5) THEN
                     WRITE (SCRTCH,2131) (RANINT(K), K = 1,KINT)
                  ELSE IF (KINC.EQ.6) THEN
                     WRITE (SCRTCH,2132) (RANINT(K), K = 1,KINT)
                  ELSE IF (KINC.EQ.7) THEN
                     WRITE (SCRTCH,2133) (RANINT(K), K = 1,KINT)
                  ELSE IF (KINC.EQ.8) THEN
                     WRITE (SCRTCH,2134) (RANINT(K), K = 1,KINT)
                  ELSE IF (KINC.EQ.9) THEN
                     WRITE (SCRTCH,2135) (RANINT(K), K = 1,KINT)
                  ELSE IF (KINC.EQ.10) THEN
                     WRITE (SCRTCH,2136) (RANINT(K), K = 1,KINT)
                     END IF
                  LINE(36+KRNC*KREAL:) = SCRTCH(1:KINC*KINT)
C                                       Write VIS data: CRT
C                                       low UVW, low flux
               ELSE IF ((UVM.LE.999.98) .OR. (OTYPE.GE.4)) THEN
C
                  IF (OTYPE.EQ.1) THEN
                     IF (WTFM.EQ.1) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1131,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, U, V, W, (AMP(K),
     *                           PHASE(K),RWT(K), K = 1,LIMIT2)
                           ELSE
                              WRITE (LINE,3131,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, U, V, W, (RE(K),
     *                           IM(K),RWT(K), K = 1,LIMIT2)
                              END IF

                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1136,ERR=140) XCOUNT, ITT,
     *                           IA1 ,IA2, ISU, IFQ, U, V, W, (AMP(K),
     *                           PHASE(K),RWT(K), K = 1,LIMIT2)
                           ELSE
                              WRITE (LINE,3136,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, U, V, W, (RE(K),
     *                           IM(K),RWT(K), K = 1,LIMIT2)
                              END IF
                           END IF
                     ELSE IF (WTFM.EQ.2) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1132,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, U, V, W, (AMP(K),
     *                           PHASE(K),RWT(K), K = 1,LIMIT2)
                           ELSE
                              WRITE (LINE,3132,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, U, V, W, (RE(K),
     *                           IM(K),RWT(K), K = 1,LIMIT2)
                              END IF
                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1137,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, U, V, W, (AMP(K),
     *                           PHASE(K),RWT(K), K = 1,LIMIT2)
                           ELSE
                              WRITE (LINE,3137,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, U, V, W, (RE(K),
     *                           IM(K),RWT(K), K = 1,LIMIT2)
                              END IF
                           END IF
                     ELSE IF (WTFM.EQ.3) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1133,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, U, V, W, (AMP(K),
     *                           PHASE(K),RWT(K), K = 1,LIMIT2)
                           ELSE
                              WRITE (LINE,3133,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, U, V, W, (RE(K),
     *                           IM(K),RWT(K), K = 1,LIMIT2)
                              END IF
                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1138,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, U, V, W, (AMP(K),
     *                           PHASE(K),RWT(K), K = 1,LIMIT2)
                           ELSE
                              WRITE (LINE,3138,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, U, V, W, (RE(K),
     *                           IM(K),RWT(K), K = 1,LIMIT2)
                              END IF
                           END IF
                     ELSE IF (WTFM.EQ.4) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1134,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, U, V, W, (AMP(K),
     *                           PHASE(K),RWT(K), K = 1,LIMIT2)
                           ELSE
                              WRITE (LINE,3134,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, U, V, W, (RE(K),
     *                           IM(K),RWT(K), K = 1,LIMIT2)
                              END IF
                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1139,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, U, V, W, (AMP(K),
     *                           PHASE(K),RWT(K), K = 1,LIMIT2)
                           ELSE
                              WRITE (LINE,3139,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, U, V, W, (RE(K),
     *                           IM(K),RWT(K), K = 1,LIMIT2)
                              END IF
                           END IF
                        END IF
                  ELSE IF (OTYPE.EQ.2) THEN
                     IF (WTFM.EQ.1) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1141,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3141,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1146,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3146,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                           END IF
                     ELSE IF (WTFM.EQ.2) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1142,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3142,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1147,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3147,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                           END IF
                     ELSE IF (WTFM.EQ.3) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1143,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3143,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1148,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3148,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                           END IF
                     ELSE IF (WTFM.EQ.4) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1144,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3144,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1149,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3149,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, U, V, W,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                           END IF
                        END IF
                     IF (LINE(18:18).EQ.' ') LINE(18:18) = '0'
                     IF (LINE(19:19).EQ.' ') LINE(19:19) = '0'
                  ELSE IF (OTYPE.EQ.3) THEN
                     IF (XAMP.LE.99.9) THEN
                        IF (DOAMPH) THEN
                           WRITE (LINE,1150,ERR=140) XCOUNT, ITT, IA1,
     *                        IA2,U, V, (AMP(K), PHASE(K), IWT(K),K = 1
     *                        ,LIMIT2)
                        ELSE
                           WRITE (LINE,3150,ERR=140) XCOUNT, ITT, IA1,
     *                        IA2,U, V, (RE(K), IM(K), IWT(K),K = 1
     *                        ,LIMIT2)
                           END IF
                     ELSE
                        IF (DOAMPH) THEN
                           WRITE (LINE,1155,ERR=140) XCOUNT, ITT, IA1,
     *                        IA2,U, V, (AMP(K), PHASE(K), IWT(K),K = 1
     *                        ,LIMIT2)
                        ELSE
                           WRITE (LINE,3155,ERR=140) XCOUNT, ITT, IA1,
     *                        IA2,U, V, (RE(K), IM(K), IWT(K),K = 1
     *                        ,LIMIT2)
                           END IF
                        END IF
                  ELSE IF (OTYPE.EQ.4) THEN
                     IF (XAMP.LE.99.9) THEN
                        IF (DOAMPH) THEN
                           WRITE (LINE,1151,ERR=140) XCOUNT, ITT, IA1,
     *                        IA2, (AMP(K), PHASE(K), IWT(K),
     *                        K = 1,LIMIT2)
                        ELSE
                           WRITE (LINE,3151,ERR=140) XCOUNT, ITT, IA1,
     *                        IA2, (RE(K), IM(K), IWT(K), K = 1,LIMIT2)
                           END IF
                     ELSE
                        IF (DOAMPH) THEN
                           WRITE (LINE,1156,ERR=140) XCOUNT, ITT, IA1,
     *                        IA2, (AMP(K), PHASE(K), IWT(K),
     *                        K = 1,LIMIT2)
                        ELSE
                           WRITE (LINE,3156,ERR=140) XCOUNT, ITT, IA1,
     *                        IA2, (RE(K), IM(K), IWT(K), K = 1,LIMIT2)
                           END IF
                        END IF
                  ELSE IF (OTYPE.EQ.5) THEN
                     IF (XAMP.LE.99.9) THEN
                        IF (DOAMPH) THEN
                           WRITE (LINE,1152,ERR=140) ITT(2), ITT(3),
     *                        ITT(4), IA1, IA2, (AMP(K), PHASE(K),
     *                        IWT(K), K = 1,LIMIT2)
                        ELSE
                           WRITE (LINE,3152,ERR=140) ITT(2), ITT(3),
     *                        ITT(4), IA1, IA2, (RE(K), IM(K),
     *                        IWT(K), K = 1,LIMIT2)
                           END IF
                     ELSE
                        IF (DOAMPH) THEN
                           WRITE (LINE,1157,ERR=140) ITT(2), ITT(3),
     *                        ITT(4), IA1, IA2, (AMP(K), PHASE(K),
     *                        IWT(K), K = 1,LIMIT2)
                        ELSE
                           WRITE (LINE,3157,ERR=140) ITT(2), ITT(3),
     *                        ITT(4), IA1, IA2, (RE(K), IM(K),
     *                        IWT(K), K = 1,LIMIT2)
                           END IF
                        END IF
                     END IF
C                                       High UVW, low flux
               ELSE
                  IF (OTYPE.EQ.1) THEN
                     IF (WTFM.EQ.1) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1161,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3161,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1166,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3166,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                                 END IF
                           END IF
                     ELSE IF (WTFM.EQ.2) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1162,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3162,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1167,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3167,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                           END IF
                     ELSE IF (WTFM.EQ.3) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1163,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3163,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
C
                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1168,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3168,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                           END IF
                     ELSE IF (WTFM.EQ.4) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1164,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3164,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1169,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3169,ERR=140) XCOUNT, ITT,
     *                           IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                           END IF
                        END IF
                  ELSE IF (OTYPE.EQ.2) THEN
                     IF (WTFM.EQ.1) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1171,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3171,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1176,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3176,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                           END IF
                     ELSE IF (WTFM.EQ.2) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1172,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3172,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1177,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3177,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                           END IF
C
                     ELSE IF (WTFM.EQ.3) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1173,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3173,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1178,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3178,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                           END IF
                     ELSE IF (WTFM.EQ.4) THEN
                        IF (XAMP.LE.99.9) THEN
                           IF (DOAMPH) THEN
                              WRITE (LINE,1174,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3174,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                        ELSE
                           IF (DOAMPH) THEN
                              WRITE (LINE,1179,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (AMP(K), PHASE(K), RWT(K), K=1,LIMIT2)
                           ELSE
                              WRITE (LINE,3179,ERR=140) XCOUNT, JTT,
     *                           RTT, IA1, IA2, ISU, IFQ, IU, IV, IW,
     *                           (RE(K), IM(K), RWT(K), K = 1,LIMIT2)
                              END IF
                           END IF
                        END IF
                     IF (LINE(18:18).EQ.' ') LINE(18:18) = '0'
                     IF (LINE(19:19).EQ.' ') LINE(19:19) = '0'
                  ELSE IF (OTYPE.EQ.3) THEN
                     IF (XAMP.LE.99.9) THEN
                        IF (DOAMPH) THEN
                           WRITE (LINE,1180,ERR=140) XCOUNT, ITT, IA1,
     *                        IA2, IU, IV, (AMP(K), PHASE(K), IWT(K),
     *                        K = 1,LIMIT2)
                        ELSE
                           WRITE (LINE,3180,ERR=140) XCOUNT, ITT, IA1,
     *                        IA2, IU, IV, (RE(K), IM(K), IWT(K),
     *                        K = 1,LIMIT2)
                           END IF
                     ELSE
                        IF (DOAMPH) THEN
                           WRITE (LINE,1185,ERR=140) XCOUNT, ITT, IA1,
     *                        IA2, IU, IV, (AMP(K), PHASE(K), IWT(K),
     *                        K = 1,LIMIT2)
                        ELSE
                           WRITE (LINE,3185,ERR=140) XCOUNT, ITT, IA1,
     *                        IA2, IU, IV, (RE(K), IM(K), IWT(K),
     *                        K = 1,LIMIT2)
                           END IF
                        END IF
                     END IF
                  END IF
C                                       Write VIS data
 140           CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 960
               PCOUNT = PCOUNT + 1
               IF (PCOUNT.GE.XNIT) GO TO 960
C                                       Update IPOINT
 150           IPOINT = IPOINT + LREC * MIN (LENBU, INC)
               JLAST = J
 160           CONTINUE
 170     ISKIP = ISKIP - NIO
         IF (ISKIP+NIO.LE.NIO) ISKIP = INC - (NIO-JLAST)
         GO TO 100
C                                       CRT error
 950  CONTINUE
         WRITE (MSGTXT,1950) IRET
         CALL MSGWRT (8)
C                                       Close files.
 960  IF (IRET.LE.0) IRET = 0
      CALL LPCLOS (LUNP, FINDP, IPCNT, IRET)
C
      CALL MAPCLS ('READ', DISK, CNO, LUN, FIND, CATBLK, F, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1980) IRET
         GO TO 990
         END IF
      NCFILE = NCFILE - 1
      GO TO 999
C                                        Write end message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('File = ',A12,'.',A6,'.',I4,'   Vol =',I2,4X,'Userid =',
     *   I5,5X,'Channels =',I5,' to ',I5)
 1051 FORMAT (A12,'.',A6,'.',I4,'  Vol=',I2,'  User=',I5,'  Channels=',
     *   I4,' to ',I4)
 1055 FORMAT ('Source= ',A8,3X,A4,' = ',A1,I2.2,I3.2,F6.2,3X,A4,' = ',
     *   A1,I2.2,I3.2,F5.1,' IF =',I3)
 1056 FORMAT ('Freq=',F13.9,' GHz   Ncor=',I3,'   No. vis=',I10,
     *   '   Sort order= ',A2)
 1057 FORMAT ('Weights have been multiplied by',1PE9.1)
 1060 FORMAT ('Source= ',A8,4X,'Freq= ',F13.9,4X,'Sort= ',A2,4X,
     *   5(4X,I4,3X,A2,6X))
 1061 FORMAT ('Source= ',A8,6X,'Freq= ',F13.9,5X,'Sort= ',A2,4X,
     *   5(4X,I4,3X,A2,6X))
 1062 FORMAT (A8,4X,'Freq=',F13.9,4X,'Sort= ',A2,5(3X,I4,1X,A2,5X))
 1063 FORMAT (A8,F13.9,1X,A2,4(3X,I4,1X,A2,4X))
 1064 FORMAT (A8,2X,A2,2X,4(3X,I4,1X,A2,4X))
 1070 FORMAT ('  Vis #',5X,'IAT',6X,'Ant  Su Fq U(klam) V(klam) ',
     *   'W(klam)',5A)
 1071 FORMAT ('  Vis #',6X,'IAT',8X,'Ant  Su Fq U(klam) V(klam) ',
     *   'W(klam)',5A)
 1072 FORMAT ('  Vis #',5X,'IAT',6X,'Ant  U(klam) V(klam)',5A)
 1073 FORMAT (' Vis #',5X,'IAT',6X,'Ant ',4A)
 1074 FORMAT (2X,'IAT',5X,'Ant ',4A)
 1090 FORMAT ('ERROR:',I7,' INITIALIZING UV FILE')
 1100 FORMAT ('ERROR:',I7,' READING VIS ')
 1131 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,I4,F7.1))
 1132 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,I4,F7.2))
 1133 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,I4,F7.3))
 1134 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,I4,F7.4))
 1136 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,I4,F7.1))
 1137 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,I4,F7.2))
 1138 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,I4,F7.3))
 1139 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,I4,F7.4))
 1141 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,I4,F7.1))
 1142 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,I4,F7.2))
 1143 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,I4,F7.3))
 1144 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,I4,F7.4))
 1146 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,I4,F7.1))
 1147 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,I4,F7.2))
 1148 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,I4,F7.3))
 1149 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,I4,F7.4))
 1150 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F8.2,5(F8.3,I4,I3))
 1151 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,4(F7.3,I4,I3))
 1152 FORMAT (I2.2,2(':',I2.2),I3,'-',I2,4(F7.3,I4,I3))
 1155 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F8.2,5(F8.1,I4,I3))
 1156 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,4(F7.1,I4,I3))
 1157 FORMAT (I2.2,2(':',I2.2),I3,'-',I2,4(F7.1,I4,I3))
 1161 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.3,I4,F7.1))
 1162 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.3,I4,F7.2))
 1163 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.3,I4,F7.3))
 1164 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.3,I4,F7.4))
 1166 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.1,I4,F7.1))
 1167 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.1,I4,F7.2))
 1168 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.1,I4,F7.3))
 1169 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.1,I4,F7.4))
 1171 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.3,I4,F7.1))
 1172 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.3,I4,F7.2))
 1173 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.3,I4,F7.3))
 1174 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.3,I4,F7.4))
 1176 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.1,I4,F7.1))
 1177 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.1,I4,F7.2))
 1178 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.1,I4,F7.3))
 1179 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.1,I4,F7.4))
 1180 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I8,5(F8.3,I4,I3))
 1185 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I8,5(F8.1,I4,I3))
 2070 FORMAT ('  Vis #',7X,'IAT',8X,'Base Suba')
 2120 FORMAT (I7,I3,'/',2(I2.2,':'),F6.3,I3,'-',I2,2X,I3)
 2121 FORMAT (10(1PE10.2))
 2122 FORMAT (10(1PE11.3))
 2123 FORMAT (10(1PE12.4))
 2124 FORMAT (10(1PE13.5))
 2125 FORMAT (10(1PE14.6))
 2126 FORMAT (10(1PE15.7))
 2131 FORMAT (10I5)
 2132 FORMAT (10I6)
 2133 FORMAT (10I7)
 2134 FORMAT (10I8)
 2135 FORMAT (10I9)
 2136 FORMAT (10I10)
 3060 FORMAT ('Source= ',A8,4X,'Freq= ',F13.9,4X,'Sort= ',A2,4X,
     *   5(5X,I4,5X,A2,7X))
 3061 FORMAT ('Source= ',A8,6X,'Freq= ',F13.9,5X,'Sort= ',A2,4X,
     *   5(5X,I4,5X,A2,7X))
 3062 FORMAT (A8,4X,'Freq=',F13.9,4X,'Sort= ',A2,5(4X,I4,3X,A2,6X))
 3063 FORMAT (A8,F13.9,1X,A2,4(4X,I4,2X,A2,5X))
 3064 FORMAT (A8,2X,A2,2X,4(4X,I4,2X,A2,5X))
 3131 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,F8.3,F7.1))
 3132 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,F8.3,F7.2))
 3133 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,F8.3,F7.3))
 3134 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,F8.3,F7.4))
 3136 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,F8.1,F7.1))
 3137 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,F8.1,F7.2))
 3138 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,F8.1,F7.3))
 3139 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,F8.1,F7.4))
 3141 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,F8.3,F7.1))
 3142 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,F8.3,F7.2))
 3143 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,F8.3,F7.3))
 3144 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.3,F8.3,F7.4))
 3146 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,F8.1,F7.1))
 3147 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,F8.1,F7.2))
 3148 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,F8.1,F7.3))
 3149 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3F8.2,
     *   4(F8.1,F8.1,F7.4))
 3150 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F8.2,
     *   5(F8.3,F8.3,I3))
 3151 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,4(F7.3,F7.3,I3))
 3152 FORMAT (I2.2,2(':',I2.2),I3,'-',I2,4(F7.3,F7.3,I3))
 3155 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F8.2,
     *   5(F8.1,F8.1,I3))
 3156 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,4(F7.1,F7.1,I3))
 3157 FORMAT (I2.2,2(':',I2.2),I3,'-',I2,4(F7.1,F7.1,I3))
 3161 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.3,F8.3,F7.1))
 3162 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.3,F8.3,F7.2))
 3163 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.3,F8.3,F7.3))
 3164 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.3,F8.3,F7.4))
 3166 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.1,F8.1,F7.1))
 3167 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.1,F8.1,F7.2))
 3168 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.1,F8.1,F7.3))
 3169 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I3,3I8,
     *   5(F8.1,F8.1,F7.4))
 3171 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.3,F8.3,F7.1))
 3172 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.3,F8.3,F7.2))
 3173 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.3,F8.3,F7.3))
 3174 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.3,F8.3,F7.4))
 3176 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.1,F8.1,F7.1))
 3177 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.1,F8.1,F7.2))
 3178 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.1,F8.1,F7.3))
 3179 FORMAT (I7,I3,'/',2(I2.2,':'),F5.2,I3,'-',I2,2I3,3I8,
     *   5(F8.1,F8.1,F7.4))
 3180 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I8,5(F8.3,F8.3,I3))
 3185 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2I8,5(F8.1,F8.1,I3))
 1950 FORMAT ('ERROR',I5,' DOING I/O TO TERMINAL OR PRINTER')
 1980 FORMAT ('ERROR:',I7,' CLOSING UV FILE ')
      END
      SUBROUTINE SOURS (DOPRT, SOUID, OLDSOU, WANTED, IERR)
C-----------------------------------------------------------------------
C   Process the next source number adding header info if the source
C   changes. Calls GETSOU to fill in commons with source info using
C   GETSOU.
C   Input:
C      DOPRT   L      Do display?
C      SOUID   I      Source ID number
C   Input from common:
C      MULTI   L      If true then this is a multi source file.
C      DISK    I      Input file disk number.
C      CNO     I      Input file catalog slot number
C      NSOUWD  I      Number of source numbers to check in SOUWAN
C      SOUWAN  I(*)   List of source numbers desired.
C      LUNP    I      LUN for output.
C      FINDP   I      FTAB pointer for output.
C      DOCRT   R      Requested output type and width.
C      NACROS  I      Actual output width.
C      OTYPE   I      Output width type.
C   Input/output:
C      OLDSOU  I      Last source number, -1 on input => first call.
C   Input/output from common:
C      TITL1   C*132  First title line
C      TITL2   C*132  Second title line
C      IPCNT   I      Line count on page
C      PAGE    I      Page number
C   Output:
C      WANTED  L      If true this source is wanted.
C      IERR    I      Return error code, 0=>OK else failed
C   Output in common:C      SNAME   C*8    Source name (DUVH.INC)
C      All values in DSOU.INC
C-----------------------------------------------------------------------
      LOGICAL   DOPRT, WANTED
      INTEGER   SOUID, OLDSOU, IERR
C
      INTEGER   ISU, SULUN
      INCLUDE 'PRTUV.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA SULUN /17/
C-----------------------------------------------------------------------
      IERR = 0
      WANTED = .TRUE.
C                                       If same source as last skip.
      IF (SOUID.EQ.OLDSOU) GO TO 999
      IF (MULTI) THEN
C                                       Wanted?
         WANTED = .FALSE.
         DO 20 ISU = 1,NSOUWD
            IF (SOUID.EQ.SOUWAN(ISU)) WANTED = .TRUE.
 20         CONTINUE
C                                       Source selected?
         IF (.NOT.DOSWNT) WANTED = .NOT.WANTED
         IF (.NOT.WANTED) GO TO 999
C                                       Get new source info
         CALL GETSOU (SOUID, DISK, CNO, CATBLK, SULUN, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR, SOUID
            GO TO 990
            END IF
         SOURCE = SNAME
         END IF
C                                       Change source name in TITL1
      IF (DOPRT) THEN
         IF (OTYPE.LT.3) THEN
            TITL1(9:16) = SOURCE(1:8)
         ELSE
            TITL1(1:8) = SOURCE(1:8)
            END IF
C                                       First call - print titles
         IF (OLDSOU.EQ.-1) THEN
C
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         TITL1, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         TITL2, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
C                                       Blank line for first or new
C                                       source
         IF (((OLDSOU.EQ.-1) .OR. (SOUID.NE.OLDSOU)) .AND.
     *     (DOCRT.GT.-2.5)) THEN
            LINE = ' '
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
C                                       Title for subsequent new source.
         IF ((OLDSOU.NE.-1) .AND. (SOUID.NE.OLDSOU))  THEN
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         TITL1, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            IF (DOCRT.GT.-2.5) THEN
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            TITL2, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
               END IF
            END IF
         END IF
C                                       Save old source number
      OLDSOU = SOUID
      GO TO 999
C                                       Error writing output
 950  WRITE (MSGTXT,1950) IERR
C                                       Error
 990  IF (IERR.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('ERROR:',I7,' OBTAINING DATA FOR SOURCE #',I5)
 1950 FORMAT ('ERROR',I5,' DOING I/O TO TERMINAL OR PRINTER')
      END
      SUBROUTINE T2DHMS (TIMEIN, NDIG, TIME, RTIME)
C-----------------------------------------------------------------------
C   Convert from Time to Days Hours Minutes Seconds format
C   Input:
C      TIMEIN   R       Input:  Time
C      NDIG     I       Number digits in seconds display
C   Output:
C      TIME     I*(3)   Output Time in Days Hours Minutes
C      RTIME    R       SECONDS
C-----------------------------------------------------------------------
      REAL     TIMEIN, RTIME
      INTEGER  NDIG, TIME(3)
C
      REAL     T
      INTEGER  I, J
C-----------------------------------------------------------------------
      T = TIMEIN
      IF (TIMEIN.LT.0.0) T = -T
C
      TIME(1) = T
      T = (T - TIME(1)) * 24.0
      TIME(2) = T
      T = (T - TIME(2)) * 60.0
      TIME(3) = T
      T = (T - TIME(3)) * 60.0
      RTIME   = T
      J = 10 ** NDIG
      J = MAX (1, J)
      I = J*T + 0.5
C                                       Now Remove 60 seconds
      IF (I.GE.J*60) THEN
         RTIME = RTIME - 60.0
         TIME(3) = TIME(3) + 1
         END IF
C                                       Now Remove 60 minutes
      IF (TIME(3).GE.60) THEN
         TIME(3) = TIME(3) - 60
         TIME(2) = TIME(2) + 1
         END IF
C                                       Now Remove 24 hours
      IF (TIME(2).GE.24) THEN
         TIME(2) = TIME(2) - 24
         TIME(1) = TIME(1) + 1
         END IF
C                                       Sign
      IF (TIMEIN.LT.0.0) TIME(1) = -TIME(1)
C
 999  RETURN
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
      SUBROUTINE LISRAN (OUTRAN, ANOTA)
C-----------------------------------------------------------------------
C   Returns the list of the random parameters and relevant array
C   of logicals - whether the parameter is (is not) averaged
C   Input from common:
C      NRPARM   I        Number of random parameters
C      CATBLK   I(256)   Header
C   Outputs:
C      OUTRAN   C(*)*8   Array of random parameters
C      ANOTA    I(*)     type of the parameter
C                        0 => integer
C                        1 => real
C                        2 => omit (time baseline)
C                        -1=> not recognized
C-----------------------------------------------------------------------
      INTEGER   ANOTA(*)
      CHARACTER OUTRAN(*)*8
C
      INTEGER   NUMPAR
      PARAMETER (NUMPAR = 18)
C
      INTEGER   IPARM, INDPAR, NRPRIN, I, J
      CHARACTER CTEMP*8, NAMPAR(NUMPAR)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA NAMPAR / 'UU-L    ', 'VV-L    ', 'WW-L    ', 'TIME1   ',
     *   'BASELINE', 'SOURCE  ', 'FREQSEL ', 'INTTIM  ', 'GATEID  ',
     *   'CORR-ID ', 'FILTER  ', 'DATE    ', 'WEIGHT  ', 'SCALE   ',
     *   'REMOVED ', 'SUBARRAY', 'ANTENNA1', 'ANTENNA2'/
C-----------------------------------------------------------------------
      NRPRIN = NRPARM
      NRPRIN = MIN (NRPRIN, KIPTPN)
      DO 40 IPARM = 1,NRPRIN
         ANOTA(IPARM) = -1
         INDPAR = KHPTP + 2 * (IPARM - 1)
         CALL H2CHR (8, 1, CATH(INDPAR), CTEMP)
         OUTRAN(IPARM) = CTEMP
         DO 20 I = 1,NUMPAR
            J = 8
            IF (I.LT.4) J = 4
            IF (CTEMP(:J).EQ.NAMPAR(I)(:J)) THEN
               ANOTA(IPARM) = 0
               IF ((I.LE.3) .OR. (I.EQ.8) .OR. (I.EQ.13) .OR.
     *            (I.EQ.14)) ANOTA(IPARM) = 1
               IF ((I.GE.4) .AND. (I.LE.5)) ANOTA(IPARM) = 2
               END IF
 20         CONTINUE
         IF (ANOTA(IPARM).LT.0) THEN
            WRITE (MSGTXT,1000) OUTRAN(IPARM)
            CALL MSGWRT (6)
            END IF
 40      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WARNING: ',A,' NOT RECOGNIZED')
      END
