      PROGRAM PRTSD
C-----------------------------------------------------------------------
C! Prints selected single dish format data.
C# Singledish Printer Terminal Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2000, 2004, 2007, 2009, 2015-2016,
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
C   Task PRTSD lists a selected portion of a catalogd uv data base.
C   Single dish version.
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      INNAME         NAME          File name to be listed.
C      INCLASS        CLASS         File class to be listed.
C      INSEQ          SEQ           File sequence number.
C      INDISK         DISK          Disk volumn on which file resides.
C      CHANNEL        NCH           Spectral channel.
C      BIF            BIF           IF number
C      TIMERANG       XTIME         1-4: start time D, H, M, S
C                                   5-8: stop time D, H, M, S
C      ANTENNAS       XANTS         beams.
C      BCOUNT         VO            Visibility number to start list
C      NCOUNT         XNIT          Number of components to list
C                                   default: 1 Page.
C      XINC           INC           Increment for vis.
C      UVRANGE        UVRANG        Range of UV in 1000's wavelengths
C      DOCRT          DOCRT         If true (>0) write to terminal.
C      OUTPRINT       LPNAME        File name to keep printer output in
C      DOCELL         DOOFF         If true give offset not value.
C-----------------------------------------------------------------------
      CHARACTER CHSCAN*8, CHSAMP*8, PRGM*6, TITL1*132, TITL2*132,
     *   LINE*132, SCRTCH*132, LPNAME*48, TYPLAB(4)*12, JSTOKE(4,3)*2,
     *   TCHAR(2)*4, CHOFFS*4, CHSIGR*1, CHSIGD*1, NAME*12, CLASS*6,
     *   ISTOKE(7)*2, LLCH*4, MMCH*4, TSIGN*1
      HOLLERITH CATH(256), XNAME(3), XCLASS(2), XLPNAM(12)
      LOGICAL   T, NORMAL, DOOFF, ISPROJ, AZEL, RADEC
      INTEGER   DISK, SEQ, USERID, FINDP, PAGE, HM(2), DD(2), IPCNT,
     *   LUNP, ITT(3), IWT(7), NCH, ICH(7), IRAS, IDECS, IOPC, IROUND,
     *   NPARM, IRET, IERR, INC, I, NUMCH, LIMIT2, ICH1, ICH2,NNCH, II,
     *   JJ, INDEX, L, JCOR, K, KK, KKK, NUMCHM, NACROS,OTYPE, IBEM,
     *   IVOFF, WTOFF, SAMPLE, ILOCSC, ILOCSM, PCOUNT,RECCNT, FIRST,
     *   LAST, SCAN, I4TEMP, JTRIM, IA1, IA2, NCOUNT, TTY(2)
      REAL      RSEC, DSEC, XSEQ, XSTR, XNIT, XINC, XTIME(8),
     *   BUFF(12300), XNCH, CATR(256), XAMP, YAMP, UVSCAL, XREAL(7),
     *   DOCRT, XDISK, TEMP, XBIF, XANT(50), XSUBA, XDOCAL, XGUSE,
     *   XFLAG, XDOCEL, BADD(10), XDAY, SECNDS, XWT, NWT, WTSC, WEIGHT
      DOUBLE PRECISION    CATD(128), XRA, XDEC
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      COMMON /INPARM/ XNAME, XCLASS, XSEQ, XDISK, XNCH, XBIF, XTIME,
     *   XANT, XSUBA, XDOCAL, XGUSE, XFLAG, XSTR, XNIT, XINC, DOCRT,
     *   XLPNAM, XDOCEL, BADD
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA TYPLAB /'Unprojected ', 'Unprojected ', 'Projected   ',
     *   'Rel. Az-El'/
      DATA T /.TRUE./
      DATA JSTOKE /'X ','Y ','??','??', 'R ','L ','??','??',
     *   'I ','Q ','U ','V '/
      DATA PRGM /'PRTSD '/
      DATA CHSCAN, CHSAMP /'SCAN','SAMP'/
      DATA TCHAR, CHOFFS  /'FLUX','WT  ', 'OFFS'/
C-----------------------------------------------------------------------
C                                       Init I/O
      TSKNAM = PRGM
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
C                                       Get input parameters.
      NPARM = 98
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAME, BUFF, IRET)
      IF (IRET.EQ.0) GO TO 10
         IF (IRET.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IRET
            CALL MSGWRT (8)
C                                       Restart AIPS.
 10   IF ((IRET.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      IF (DOCRT.GT.0.0) RQUICK = .FALSE.
      IF (RQUICK) RQUICK = LPNAME.NE.' '
      IF (RQUICK) CALL RELPOP (IRET, BUFF, IERR)
      IF (IRET.NE.0) GO TO 990
      IRET = 8
C                                       Decode input.
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
      USERID = NLUSER
      SEQ = XSEQ + 0.1
      DISK = XDISK + 0.1
      NCH = XNCH + 0.1
      IF (NCH.LE.0) NCH = 1
      BIF = XBIF + 0.1
      IF (BIF.LE.0) BIF = 1
      IF (XSTR.LT.1.0) XSTR = 1.0
      FIRST = XSTR + 0.1
      INC = XINC + 0.1
      INC = MAX (1, INC)
      IF (XNIT.LT.1.0) THEN
         XNIT = PRTMAX - 10
         IF (DOCRT.GT.0.0) XNIT = 30000
      ELSE
         IF (DOCRT.LE.0.0) XNIT = MIN (2000., XNIT)
         END IF
      I4TEMP = IROUND (XNIT)
      LAST = FIRST + (I4TEMP - 1) * INC
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
C                                       See if offset wanted
      DOOFF = XDOCEL.GT.0.0
C                                       Setup for SDGET
      UNAME = NAME
      UCLAS = CLASS
      UDISK = DISK
      USEQ = SEQ
      CALL RCOPY (8, XTIME, TIMRNG)
      BCHAN = 1
      ECHAN = 0
      EIF = BIF
      DOCAL = XDOCAL.GT.0.0
      DO 15 I = 1,50
         ANTENS(I) = IROUND (XANT(I))
 15      CONTINUE
      SUBARR = IROUND (XSUBA)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
C                                       Open file and get CATBLK and
C                                       initialize reading data file.
      INITVS = FIRST
      CALL SDGET ('INIT', BUFF, BUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get info from CATBLK.
      MSGSUP = 32000
      CALL UVPGET (IERR)
      MSGSUP = 0
      NORMAL = IERR.EQ.0
      ISPROJ = TYPUVD.EQ.2
      IERR = 0
      UVSCAL = 1.0 / FREQ
      IF (NORMAL) UVSCAL = 1.0
      IOPC = 1
      IF (ISPROJ) IOPC = 2
      IF (.NOT.NORMAL) ILOCB = 2
      IBEM = 0
      SAMPLE = 0
      SCAN = 0
C                                       BS or regular SD data?
      AZEL = TYPUVD.EQ.3
      IF (AZEL) IOPC = 2
C                                       SCAN pointer
      CALL AXEFND (4, CHSCAN, CATBLK(KIPCN), CATH(KHPTP), ILOCSC, IERR)
      IF (IERR.NE.0) ILOCSC = -1
C                                       SAMPLE pointer
      CALL AXEFND (4, CHSAMP, CATBLK(KIPCN), CATH(KHPTP), ILOCSM, IERR)
      IF (IERR.NE.0) ILOCSM = -1
      IERR = 0
C                                       Visibility offset
      IVOFF = NRPARM
      IF (JLOCIF.LE.0) BIF = 1
      IF (JLOCIF.GT.0) IVOFF = NRPARM + (BIF-1) * INCIF
      WTOFF = 2
C                                       See if printing offset
      IF (DOOFF) THEN
         IVOFF = IVOFF + 1
         WTOFF = 1
         TCHAR(1) = CHOFFS
         END IF
C                                       Write header on unit 1
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         CALL MSGWRT (8)
         GO TO 920
         END IF
C                                       Check spectral channel.
      IF (CATBLK(KINAX+JLOCF).LE.1) NCH = 1
      FREQ = FREQ + (NCH-CATR(KRCRP+JLOCF)) * CATR(KRCIC+JLOCF)
      IF (NCH.LE.CATBLK(KINAX+JLOCF)) GO TO 30
         WRITE (MSGTXT,1025) NCH,CATBLK(KINAX+JLOCF)
         CALL MSGWRT (8)
         GO TO 990
C                                        Determine no. channels to do
 30   NUMCHM = CATBLK(KINAX+JLOCF) - NCH + 1
      NUMCH = (NACROS - 54) / (13 * NCOR)
      NUMCH = MIN (NUMCH, NUMCHM)
      NUMCH = MIN (NUMCH, 6)
      OTYPE = 1
      IF (NUMCH.GE.1) GO TO 35
         NUMCH = 1
         IF (NACROS.LE.80) OTYPE = 2
         IF (NACROS.LE.72) OTYPE = 3
 35   ICH1 = NCH
      ICH2 = NCH + NUMCH - 1
      LIMIT2 = NCOR * NUMCH
      NNCH = NCH - 1
      DO 40 II = 1,LIMIT2,NCOR
         NNCH = NNCH + 1
         DO 40 JJ = 1,NCOR
            INDEX = II + JJ - 1
            ICH(INDEX) = NNCH
            TEMP = CATD(KDCRV+JLOCS) + (JJ - CATR(KRCRP+JLOCS)) *
     *         CATR(KRCIC+JLOCS)
            I = IROUND (TEMP)
            IF (I.LT.-4) THEN
               L = 1
               I = -(I + 4)
            ELSE IF (I.LT.0) THEN
               L = 2
               I = ABS(I)
            ELSE
               L = 3
               END IF
            ISTOKE(INDEX) = JSTOKE(I,L)
 40         CONTINUE
C                                       Convert coordinates.
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCR*2), LLCH)
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCD*2), MMCH)
      RADEC = (LLCH(:2).EQ.'RA') .AND. (MMCH(:3).EQ.'DEC')
      IF (RADEC) THEN
         CALL COORDD (1, RA, CHSIGR, HM, RSEC)
      ELSE
         IOPC = 2
         CALL COORDD (2, RA, CHSIGR, HM, RSEC)
         END IF
      CALL COORDD (2, DEC, CHSIGD, DD, DSEC)
C                                       read through data to determine
C                                       scaling
      PCOUNT = 0
      RECCNT = FIRST - 1
      XAMP = 0.0
      XWT = 0.0
      NWT = 1.0E10
      MSGTXT = 'Finding extrema to set data formats'
      CALL MSGWRT (1)
      LUNP = 0
      FINDP = 0
      NCOUNT = 8
      IF (DOCAL) NCOUNT = NCOUNT + 1
      IF (DOFLAG) NCOUNT = NCOUNT + 1
      IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
C                                       Start looping thru data.
 50   CALL SDGET ('READ', BUFF, BUFF(1+NRPARM), IERR)
      IF (IERR.GT.0) THEN
         GO TO 900
      ELSE IF (IERR.LT.0) THEN
         IERR = 0
      ELSE
         RECCNT = RECCNT + 1
         IF (RECCNT.LT.FIRST) GO TO 50
         IF (RECCNT.GT.LAST) GO TO 900
         IF (MOD(RECCNT-FIRST, INC).NE.0) GO TO 50
C                                       Get vis.
         NCOUNT = NCOUNT + 1
         K = 0
         YAMP = 0.0
         DO 65 KK = ICH1,ICH2
            DO 60 KKK = 1,NCOR
               K = K + 1
               INDEX = 1 + (KK-1)*INCF + (KKK-1)*INCS + IVOFF
               XREAL(K) = BUFF(INDEX)
               XAMP = MAX (XAMP, XREAL(K))
               YAMP = MIN (YAMP, XREAL(K))
               WEIGHT = ABS (BUFF(INDEX+WTOFF))
               XWT = MAX (XWT, WEIGHT)
               IF (WEIGHT.GT.0.0) NWT = MIN (NWT, WEIGHT)
 60            CONTINUE
 65         CONTINUE
         XAMP = MAX (XAMP, -10.0*YAMP)
         PCOUNT = PCOUNT + 1
         IF (PCOUNT.LT.XNIT) GO TO 50
         END IF
C                                       check count
      IF ((DOCRT.LE.0.0) .AND. (LPNAME.EQ.' ')) THEN
         IF ((NPOPS.GT.NINTRN) .OR.(ISBTCH.EQ.32000)) THEN
            IF (NCOUNT.GT.1000) IERR = -1
         ELSE IF (NCOUNT.GT.500) THEN
            TTY(1) = 5
            CALL ZOPEN (TTY(1), TTY(2), 1, SCRTCH, .FALSE., .FALSE.,
     *         .TRUE., IERR)
            MSGTXT = 'PROBLEM OPENING TERMINAL'
            IF (IERR.GT.0) GO TO 890
            WRITE (SCRTCH,1045) NCOUNT
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, IERR)
            MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
            IF (IERR.GT.0) GO TO 890
            SCRTCH = 'Do you really want to print this much??' //
     *         ' Enter Y or y if so'
            CALL INQSTR (TTY, SCRTCH, 1, LLCH, IERR)
            IF (IERR.GT.0) GO TO 890
            IF ((LLCH(:1).NE.'y') .AND. (LLCH(:1).NE.'Y')) THEN
               SCRTCH = 'Good choice - save trees'
               IERR = -1
            ELSE
               SCRTCH = 'OKAY, printing anyway'
               END IF
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, I)
            CALL ZCLOSE (TTY(1), TTY(2), I)
            END IF
         END IF
      IF (IERR.NE.0) GO TO 900
C                                       Restart I/O
      CALL SDGET ('CLOS', BUFF, BUFF, IERR)
      IF (IERR.EQ.0) CALL SDGET ('INIT', BUFF, BUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       UVPGET was called again
      FREQ = FREQ + (NCH-CATR(KRCRP+JLOCF)) * CATR(KRCIC+JLOCF)
      FREQ = FREQ * 1.0E-9
C                                       weight scaling, range
      IF (XWT.LE.0.0) THEN
         XWT = 1.0
         NWT = 1.0
         END IF
      TEMP = LOG10 (XWT/0.9995)
      I = TEMP + 98.0
      I =  100 - I
      WTSC = 10.0 ** I
      IF ((NWT.GE.10.0) .AND. (XWT.LT.999.5)) WTSC = 1.0
      IF ((NWT.GE.1.0) .AND. (XWT.LT.99.5)) WTSC = 10.0
      TEMP = NWT * WTSC
      IF (TEMP.LT.1.0) THEN
         MSGTXT = 'Full dynamic range of weights cannot be printed'
         CALL MSGWRT (6)
         END IF
      IPCNT = 998
      PAGE = 0
C                                      Leading information
      I = JTRIM (ISORT)
      I = JTRIM (SOURCE)
      TITL1 = ' '
      TITL2 = ' '
      IF (DOCRT.GT.-2.5) THEN
         IF (NACROS.LE.90) THEN
            WRITE (LINE,1050) NAME, CLASS, SEQ, DISK, USERID, ICH1,
     *         ICH2, BIF
         ELSE
            WRITE (LINE,1051) NAME, CLASS, SEQ, DISK, USERID, ICH1,
     *         ICH2, BIF
            END IF
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1055) SOURCE, LLCH, CHSIGR, HM, RSEC, MMCH, CHSIGD,
     *      DD, DSEC
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1056) FREQ, NCOR, NVIS, ISORT
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         LINE = TYPLAB(TYPUVD+1) // 'Single dish format data'
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Calibration and editing
         IF (DOCAL) THEN
            WRITE (LINE,1060) CLUSE
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IF (DOFLAG) THEN
            WRITE (LINE,1061) FGVER
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IF (WTSC.NE.1.0) THEN
            WRITE (LINE,1062) WTSC
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Page titles
      IF (OTYPE.EQ.1) THEN
         WRITE (TITL1,1065) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *      ISTOKE(JCOR), JCOR = 1,LIMIT2)
         IF (AZEL) THEN
            WRITE (TITL2,1066) (TCHAR(1), TCHAR(2), JCOR = 1,LIMIT2)
         ELSE IF (RADEC) THEN
            WRITE (TITL2,1067) (TCHAR(1), TCHAR(2), JCOR = 1,LIMIT2)
         ELSE
            WRITE (TITL2,1068) LLCH, MMCH, (TCHAR(1), TCHAR(2),
     *         JCOR = 1,LIMIT2)
            END IF
      ELSE IF (OTYPE.EQ.2) THEN
         WRITE (TITL1,1070) SOURCE, FREQ, ISORT, (ICH(JCOR),
     *      ISTOKE(JCOR), JCOR = 1,LIMIT2)
         IF (AZEL) THEN
            WRITE (TITL2,1071) (TCHAR(1), TCHAR(2), JCOR = 1,LIMIT2)
         ELSE IF (RADEC) THEN
            WRITE (TITL2,1072) (TCHAR(1), TCHAR(2), JCOR = 1,LIMIT2)
         ELSE
            WRITE (TITL2,1073) LLCH, MMCH, (TCHAR(1), TCHAR(2),
     *         JCOR = 1,LIMIT2)
            END IF
      ELSE IF (OTYPE.EQ.3) THEN
         WRITE (TITL1,1075) SOURCE, FREQ, (ICH(JCOR), ISTOKE(JCOR),
     *      JCOR = 1,LIMIT2)
         IF (AZEL) THEN
            WRITE (TITL2,1076) (TCHAR(1), TCHAR(2), JCOR = 1,LIMIT2)
         ELSE IF (RADEC) THEN
            WRITE (TITL2,1077) (TCHAR(1), TCHAR(2), JCOR = 1,LIMIT2)
         ELSE
            WRITE (TITL2,1078) LLCH, MMCH, (TCHAR(1), TCHAR(2),
     *         JCOR = 1,LIMIT2)
            END IF
         END IF
      CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (DOCRT.GT.-2.5) THEN
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      PCOUNT = 0
      RECCNT = FIRST - 1
C                                       Start looping thru data.
 110  CALL SDGET ('READ', BUFF, BUFF(1+NRPARM), IERR)
      IF (IERR.GT.0) THEN
         GO TO 900
      ELSE IF (IERR.LT.0) THEN
         IERR = 0
      ELSE
         RECCNT = RECCNT + 1
         IF (RECCNT.LT.FIRST) GO TO 110
         IF (RECCNT.GT.LAST) GO TO 900
         IF (MOD(RECCNT-FIRST, INC).NE.0) GO TO 110
C                                       Decode time.
         XDAY = BUFF(1+ILOCT)
         CALL TFDHMS (XDAY, 1, TSIGN, ITT, SECNDS)
         IF (ITT(1).GT.9999) ITT(1) = 9999
C                                       Convert RA, Dec.
         XRA = BUFF(1+ILOCU) * UVSCAL
         XDEC = BUFF(1+ILOCV) * UVSCAL
         CALL COORDD (IOPC, XRA, CHSIGR, HM, RSEC)
         CALL COORDD (2, XDEC, CHSIGD, DD, DSEC)
         IRAS = IROUND (RSEC)
         IDECS = IROUND (DSEC)
         IF (IRAS.EQ.60) THEN
            IRAS = 0
            HM(2) = HM(2) + 1
            END IF
         IF (HM(2).EQ.60) THEN
            HM(2) = 0
            HM(1) = HM(1) + 1
            END IF
         IF (IDECS.EQ.60) THEN
            IDECS = 0
            DD(2) = DD(2) + 1
            END IF
         IF (DD(2).EQ.60) THEN
            DD(1) = DD(1) + 1
            DD(2) = 0
            END IF
C                                       Beam
         IF (ILOCB.GE.0) THEN
            IBEM = BUFF(1+ILOCB) - 255.5
            IA1 = BUFF(1+ILOCB) + 0.2
            IA2 = MOD (IA1, 256)
            IA1 = IA1 / 256
         ELSE IF (ILOCA1.GE.0) THEN
            IA1 = BUFF(1+ILOCA1) + 0.1
            IA2 = BUFF(1+ILOCA2) + 0.1
            IBEM = 256 * IA1 + IA2 - 256
            END IF
         IF (IA1.EQ.IA2) IBEM = IA1
C                                       Scan, sample
         IF (ILOCSC.GE.0) SCAN = BUFF(1+ILOCSC) + 0.5
         IF (ILOCSM.GE.0) SAMPLE = BUFF(1+ILOCSM) + 0.5
         IF (SCAN.GT.99999) SCAN = 99999
         IF (SAMPLE.GT.9999) SAMPLE = 9999
C                                       Get vis.
         K = 0
         DO 125 KK = ICH1,ICH2
            DO 120 KKK = 1,NCOR
               K = K + 1
               INDEX = 1 + (KK-1)*INCF + (KKK-1)*INCS + IVOFF
               XREAL(K) = BUFF(INDEX)
               IWT(K) = IROUND (BUFF(INDEX+WTOFF)*WTSC)
               IWT(K) = MAX (-999, MIN (999, IWT(K)))
 120           CONTINUE
 125        CONTINUE
C                                       Write VIS data.
         IF (XAMP.LE.999.9) THEN
            IF (OTYPE.EQ.1) WRITE (LINE,1120,ERR=140) SCAN, SAMPLE,
     *         ITT, SECNDS, IBEM, CHSIGR, HM, IRAS, CHSIGD, DD, IDECS,
     *         (XREAL(K), IWT(K), K = 1,LIMIT2)
            IF (OTYPE.EQ.2) WRITE (LINE,1121,ERR=140) SCAN, SAMPLE,
     *         IBEM, CHSIGR, HM, IRAS, CHSIGD, DD, IDECS, (XREAL(K),
     *         IWT(K), K = 1,LIMIT2)
            IF (OTYPE.EQ.3) WRITE (LINE,1122,ERR=140) CHSIGR, HM,
     *         IRAS, CHSIGD, DD, IDECS, (XREAL(K), IWT(K),
     *         K = 1,LIMIT2)
         ELSE
            IF (OTYPE.EQ.1) WRITE (LINE,1130,ERR=140) SCAN, SAMPLE,
     *         ITT, SECNDS, IBEM, CHSIGR, HM, IRAS, CHSIGD, DD, IDECS,
     *         (XREAL(K), IWT(K), K = 1,LIMIT2)
            IF (OTYPE.EQ.2) WRITE (LINE,1131,ERR=140) SCAN, SAMPLE,
     *         IBEM, CHSIGR, HM, IRAS, CHSIGD, DD, IDECS, (XREAL(K),
     *         IWT(K), K = 1,LIMIT2)
            IF (OTYPE.EQ.3) WRITE (LINE,1132,ERR=140) CHSIGR, HM,
     *         IRAS, CHSIGD, DD, IDECS, (XREAL(K), IWT(K),
     *         K = 1,LIMIT2)
            END IF
 140     IF (OTYPE.EQ.1) CALL FILZCH (2, 22, LINE)
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         PCOUNT = PCOUNT + 1
         IF (PCOUNT.LT.XNIT) GO TO 110
         END IF
      GO TO 900
C
 890  CALL MSGWRT (8)
C                                       Close files.
 900  IF (IERR.LE.0) IRET = 0
      IF (LUNP.GT.0) CALL LPCLOS (LUNP, FINDP, IPCNT, IERR)
C
 920  CALL SDGET ('CLOS', BUFF, BUFF, IERR)
C                                        Write end message
 990  CALL DIETSK (IRET, RQUICK, BUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR:',I7,'GETTING INPUT PARAMETERS')
 1025 FORMAT ('CHANNEL',I5,' .GT. MAX =',I5)
 1040 FORMAT ('UNABLE TO OPEN OUTPUT DEVICE: ERROR=',I7)
 1045 FORMAT ('Requested print job is',I10,' lines long!')
 1050 FORMAT (A12,'.',A6,'.',I4,' vol=',I2,' user=',I5,' Channels=',I4,
     *   '-',I4,' IF=',I3)
 1051 FORMAT ('File = ',A12,'.',A6,'.',I4,' vol =',I2,'  userid =',I5,
     *   2X,'Channels =',I5,' to ',I5,' IF=',I3)
 1055 FORMAT ('Source= ',A8,3X,A4,' = ',A1,I2.2,I3.2,F6.2,3X,A4,' = ',
     *   A1,I2.2,I3.2,F5.1)
 1056 FORMAT ('Freq=',F13.9,' GHz   ncor=',I3,'   No. vis=',I10,
     *   '   Sort order= ',A2)
 1060 FORMAT ('Applying CS table ',I3)
 1061 FORMAT ('Applying FG table ',I3)
 1062 FORMAT ('Weights have been multiplied by',F12.4)
 1065 FORMAT (A,8X,'Freq=',F13.9,4X,'Sort=',A2,7X,
     *   6(3X,I4,2X,A2,2X))
 1066 FORMAT ('  Scan/#  ',8X,'IAT',6X,'Beam ',6X,'Az',8X,'El',4X,
     *   6(4X,A4,3X,A2))
 1067 FORMAT ('  Scan/#  ',8X,'IAT',6X,'Beam ',6X,'RA',8X,'Dec',3X,
     *   6(4X,A4,3X,A2))
 1068 FORMAT ('  Scan/#  ',8X,'IAT',6X,'Beam ',6X,A,6X,A,2X,
     *   6(4X,A4,3X,A2))
 1070 FORMAT (A,4X,F13.9,2X,A2,6X,5(3X,I4,2X,A2,2X))
 1071 FORMAT ('  Scan/#  ','Beam ',5X,'Az',7X,'El',4X,4(4X,A4,3X,A2))
 1072 FORMAT ('  Scan/#  ','Beam ',5X,'RA',7X,'Dec',3X,4(4X,A4,3X,A2))
 1073 FORMAT ('  Scan/#  ','Beam ',5X,A,5X,A,2X,4(4X,A4,3X,A2))
 1075 FORMAT (A,F12.8,4(3X,I4,2X,A2,2X))
 1076 FORMAT (5X,'Az',7X,'El',4X,4(4X,A4,3X,A2))
 1077 FORMAT (5X,'RA',7X,'Dec',3X,4(4X,A4,3X,A2))
 1078 FORMAT (5X,A,5X,A,2X,4(4X,A4,3X,A2))
 1120 FORMAT (I5,'/',I4.4,I4,'/',2(I2.2,':'),F4.1,I6,1X,
     *   2(2X,A1,I2.2,2I3.2),6(F9.4,I4))
 1121 FORMAT (I5,'/',I4.4,I4,1X,2(1X,A1,I2.2,2I3.2),6(F9.4,I4))
 1122 FORMAT (2(1X,A1,I2.2,2I3.2),6(F9.4,I4))
 1130 FORMAT (I5,'/',I4.4,I4,'/',2(I2.2,':'),F4.1,I6,1X,
     *   2(2X,A1,I2.2,2I3.2),6(F9.2,I4))
 1131 FORMAT (I5,'/',I4.4,I4,1X,2(1X,A1,I2.2,2I3.2),6(F9.2,I4))
 1132 FORMAT (2(1X,A1,I2.2,2I3.2),6(F9.2,I4))
      END
