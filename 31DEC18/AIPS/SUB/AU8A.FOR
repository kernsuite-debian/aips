      SUBROUTINE AU8A (BRANCH)
C-----------------------------------------------------------------------
C! verb EXTLIST to list contents of plot files and other extension files
C# POPS-appl Plot-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999-2012, 2016-2018
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
C   AU8A lists attributes of extension files (verb EXTLIST).
C   Currently supports 'PL', 'SL', 'CC' types.
C   Inputs:
C      BRANCH    I      not used (only one verb in here).
C   Common /CAPL/:
C      INNAME    R(3)   the name of the parent file.
C      INCLASS   R(2)   the class of the parent file.
C      INSEQ     R      sequence number of the parent file.
C      INTYPE    R      type of the parent file.
C      INEXT     R      extension file type. 'PL', 'SL', 'CC'
C                             only types available.
C   Plot types supported: 1 Misc.   2 CNTR    3 greys   4 PROFL
C                         5 SL2PL   6 pcntr   7 IMEAN   8 UVPLT
C                       [ 9 gnplt][10 vbplt] 11 PFPLn [12 gaplt]
C                        13 PLCUB  14 IMVIM  15 TAPLT  16 POSSM
C                        17 SNPLT  18 kntr   19 UVHGM  20 ISPEC
C                        21 VPLOT  22 CLPLT  23 DFTPL  24 FRPLT
C                        25 FRMAP  26 PLOTR  27 GREYS  28 PCNTR
C                        29 KNTR   30 BPLOT  31 APCAL  32 ANBPL
C                        33 IRING  34 RSPEC  35 CAPLT  36 RFLAG
C                        37 CCNTR  38 SERCH  39 EVAUV  40 DELZN
C                        41 ELINT  42 FGPLT  43 GAL    44 SNIFS
C                        45 SOUSP  46 WETHR  47 RM2PL  48 XG2PL
C                        49 ALVAR  50 PCPLT  51 PLROW  52 PLOTC
C                        53 UVHOL  54 PBEAM  55 PEEK
C   pcntr, greys, kntr (6, 3, 18) are old form replacd 2002-Nov
C   vbplt, gaplt, gnplt not supported 7/7/03
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INTEGER   NPLOTS, NVERS
      PARAMETER (NPLOTS=56)
      PARAMETER (NVERS=4)
C
      CHARACTER NAME*12, CLASS*6, TYPE*2, CINEXT*2, IECHAR*2, CTEMP*25,
     *   PRGNAM*6, LTASK*6, TIME*8, DATE*12, CHCORR(13)*4, TUVPLT(18)*8,
     *   COLTYP(2)*8, BTEMP*2, RESULT*132, CHAX(2)*1, ATEMP*12,
     *   EXCHAR(8)*2, SORTOR(4)*6, TS1*1, TS2*1, TYPEBP(10)*12,
     *   XATYPE*10, YATYPE*14, CODET*4, CDUM*1, FUNC*2, FILNAM*48,
     *   PNAMES(NPLOTS)*5, DTEMP*4, TANBPL(17)*8, TVPLOT(22)*8,
     *   TCLPLT(4)*8, RFTY(6)*30, STRNG*80, DIDIT(2)*8, WXCODE(12)*4,
     *   SNTYPE(23)*4, NAME2*12, CLASS2*6, RMTYPE(5)*10, PRTASK*6,
     *   UVHOLC(5)*12, PBEAMC(8)*28, PEEKS(11)*6
      REAL      DATA(768), TEMP(6), FAC, RANGE1, RANGE2, XICUT, XMAX,
     *   XMIN, XPCUT, XPLEV, ZXR, XLEVS, X, Y, XCLEV, TDEL, ABLC(2),
     *   ATRC(2), STFACT, TDATA(10), T1, T2, RDUM, FP(2), ROTATE, SOLINT
      HOLLERITH HDATA(768), HREC(256)
      INTEGER   IRNO, IBCNT, IECNT, IICNT, IDATA(768), IBLC(2), ITRC(2),
     *   IX, IY, I, IELUN, ISX, IERR, IETYPE, IFIND, IMAX, IMAXV, IMLUN,
     *   INOPTS, IOBSVR, IPTYPE, IROT, IS, ISEQ, ISKW, ISLOT, IUSER, IV,
     *   IVER, IVOL, ICTR, IFIRST, IGM, INGM, J, NBXS, NS, IROUND, II,
     *   LSGN(2), NKEY, NCOL, NREC, IBDROP, IEDROP, INOSL, IBLCX, ITRCX,
     *   JJ, IP, IBVER, NCNT, IANT1(50), IANT2(50), NANT1, NANT2,
     *   NPARMS, STVERS, J1, J2, IT1(4), IT2(4), POTERR, IDUM, FT, NP,
     *   JTRIM, PNPARM(NPLOTS,NVERS), CVER, COFF1, COFF2, COFF3
      LOGICAL   NOSAVE, NOEXCL, EQUAL, NOWAIT, ABSEM(2), TABLE, FITASC,
     *   EXIST, WASRGB, ISOLD
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (IDATA, DATA, HDATA)
      EQUIVALENCE (NOWAIT, NOSAVE, NOEXCL)
      DATA IELUN, IMLUN /27,17/
      DATA NOSAVE /.FALSE./
      DATA CHAX /'X','Y'/
      DATA EXCHAR /'  ','PL','SL','CC','TA','HI','GA','DR'/
      DATA PRGNAM, TDEL /'AU8A ', 0.15/
      DATA SORTOR /'Ascend', 'AsceAb', 'Descnd', 'DescAb'/
      DATA CHCORR /'RR  ','LL  ','RL  ','LR  ','IPOL','VPOL','QPOL',
     *   'UPOL', 'XX','YY','XY','YX','????'/
      DATA RFTY /'Time rms', 'Spectral deviation',
     *   'Cummulative time rms', 'Cummulative spec deviation',
     *   'Time rms spectrum',    'Spectral deviation spectrum'/
      DATA TCLPLT /'Time', 'GSTime', 'CL Phase', 'CL ampl'/
      DATA TUVPLT /'Ampltude', 'Phase', 'UV dist', 'UV PA', 'Time',
     *   'U lambda', 'V lambda','W lambda', 'Real', 'Imag', 'IAT hrs',
     *   'Log ampl', 'Weight', 'Hour ang', 'Elevatn', 'Par ang',
     *   'UV d @pa', 'Azimuth'/
      DATA TVPLOT /'Ampltude', 'Phase', 'UV dist', 'UV PA', 'Time',
     *   'U lambda', 'V lambda','W lambda', 'Real', 'Imag', 'GST hrs',
     *   'IAT hrs', 'Source #', 'Freqid', 'Int time', 'Weight', 'RMS',
     *   'Hour ang', 'Elevatn', 'Par ang', 'Azimuth', 'Log amp'/
      DATA TYPEBP /'Cross-power ', 'Total-power ', 'Bandpass-cal',
     *   'Auto-cor fun', 'Cros-cor fun', 'BD table', 'Polar Dterms',
     *   'Cal polar', 'Cal polar/I','Plot type ??'/
      DATA TANBPL /'Time', 'Time hms', 'Source #', 'HourAngl', 'Elev',
     *   'Azimuth', 'Para Ang', 3*' ', 'Ampltude', 'CmplxAmp', 'Phase',
     *   'Phase !A', 'Real', 'Imag', 'Weight'/
      DATA DIDIT /'omitted', 'plotted'/
      DATA WXCODE /'TEMP', 'PRES', 'DEWP', 'WVEL', 'WDIR', 'RHUM',
     *   'CH2O', 'KZOP', 'QZOP', 'WCOS', 'WSIN', 'DDEP'/
      DATA SNTYPE /'PHAS','AMP ','DELA','RATE', 'TSYS','SUM ','DOPL',
     *   'SNR ','MDEL','TANT','ATM ','GEO ','CCAL','DDLY','REAL','IMAG',
     *   'IFR ','PDIF','PSUM','PGN ','PON ','POFF','PSYS'/
      DATA RMTYPE /'Channels', 'Frequency','Wavelength','Lambda^2',
     *   'Velocity'/
      DATA UVHOLC /'Amplitude db','Amplitude','Phase','Real part',
     *   'Imaginary'/
      DATA PBEAMC /'Contour of data', 'Contour of model',
     *   'Contour of residual', 'Azimuthal average', 'Az average sum',
     *   'Az avg data-model', 'Az avg data-model sum',
     *   'Az avg (data-model)/model'/
      DATA PEEKS /'Tilt', 'Azzero', 'Elzero', 'Colima', 'Sag', 'Az3',
     *   'Azcntr', 'Elcntr', 'Axsprp', 'Az2', 'Refrac'/
C                                       must match in AU2A (PLGET)
      DATA PNAMES /'Misc.', 'CNTR ', 'greys', 'PROFL', 'SL2PL',
     *   'pcntr', 'IMEAN', 'UVPLT', 'gnplt', 'vbplt',
     *   'PFPLn', 'gaplt', 'PLCUB', 'IMVIM', 'TAPLT',
     *   'POSSM', 'SNPLT', 'kntr ', 'UVHGM', 'ISPEC',
     *   'VPLOT', 'CLPLT', 'DFTPL', 'FRPLT', 'FRMAP',
     *   'PLOTR', 'GREYS', 'PCNTR', 'KNTR ', 'BPLOT',
     *   'APCAL', 'ANBPL', 'IRING', 'RSPEC', 'CAPLT',
     *   'RFLAG', 'CCNTR', 'SERCH', 'EVAUV', 'DELZN',
     *   'ELINT', 'FGPLT', 'GAL  ', 'SNIFS', 'SOUSP',
     *   'WETHR', 'RM2PL', 'XG2PL', 'ALVAR', 'PCPLT',
     *   'PLROW', 'PLOTC', 'UVHOL', 'PBEAM', 'PEEK',
     *   'LOCIT'/
      DATA PNPARM /
     *     0,  63, 279,  39,  20,  87,  46, 304,   0,   0,
     *     0,   0,  39,  47,  82, 314, 242,  80, 327,  49,
     *   413, 330, 187, 314, 305, 126, 331, 208, 222,  85,
     *   342, 306,  62,  57, 317, 547,  70, 136, 453, 353,
     *   334, 127,  97, 210, 340, 113,  61,  62, 280,  85,
     *   29,  154, 391,  96, 152, 240,
C                                       cver 2
     *     0,   0,   0,   0,   0,   0,  45, 303,   0,   0,
     *     0,   0,   0,   0,  47, 313, 212,   0, 317,  48,
     *   403, 320, 176, 311, 301,  66, 330, 207, 210,  83,
     *     0, 296,   0,  48, 307, 537,   0,   0,   0,   0,
     *     0,   0,   0,   0, 338, 103,   0,   0, 270,   0,
     *     0,   0,   0,   0,   0,   0,
C                                       cver 3
     *     0,   0,   0,   0,   0,   0,   0, 302,   0,   0,
     *     0,   0,   0,   0,   0, 311, 210,   0, 316,  47,
     *   402, 307,  39, 310, 300,  56, 318, 116, 119,  81,
     *     0, 295,   0,   0, 307, 535,   0,   0,   0,   0,
     *     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     *     0,   0,   0,   0,   0,   0,
C                                       cver 4
     *     0,   0,   0,   0,   0, 310,   0, 301,   0,   0,
     *     0,   0,   0,   0,   0,   0, 209,   0, 126,  46,
     *   397, 306,   0,   0,   0,  46,   0, 114, 117,   0,
     *     0,   0,   0,   0, 306,   0,   0,   0,   0,   0,
     *     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     *     0,   0,   0,   0,   0,   0/
C-----------------------------------------------------------------------
      IF (BRANCH.NE.1) GO TO 999
      POTERR = 101
      IPTYPE = 0
C                                       Find start of real*4
C                                       parms in graph file.
      IS = 11
C                                       Create name string.
      CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, NAME)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, CLASS)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INSEQ', 'I', 1, 0, ISEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      TYPE = ' '
      IUSER = NLUSER
C                                       Open map file, get header.
      CALL MAPOPN ('READ', IVOL, NAME, CLASS, ISEQ, TYPE, IUSER,
     *   IMLUN, IFIND, ISLOT, CATBLK, IDATA, IERR)
      IF (IERR.GT.1) GO TO 980
C                                       Close map file.
      CALL MAPCLS ('READ', IVOL, ISLOT, IMLUN, IFIND, CATBLK, NOSAVE,
     *   IDATA, IERR)
C                                       Determine extension file type.
      CALL ADVERB ('INEXT', 'C', 1, 2, IDUM, RDUM, CINEXT)
      IF (ERRNUM.NE.0) GO TO 980
      DO 10 IETYPE = 1,8
         IF (CINEXT.EQ.EXCHAR(IETYPE)) GO TO 20
 10      CONTINUE
      IETYPE = 10
C                                       Find highest version number.
 20   IF (IETYPE.NE.1) IECHAR = CINEXT
      IF (IETYPE.EQ.1) IECHAR = EXCHAR(2)
      CALL FNDEXT (IECHAR, CATBLK, IMAXV)
      IF (IETYPE.EQ.1) IETYPE = 2
      IF (IETYPE.EQ.9) IETYPE = 5
      IF (IMAXV.GT.0) GO TO 30
         WRITE (MSGTXT,1025) IECHAR
         CALL MSGWRT (7)
         GO TO 980
 30   IMAXV = MIN (46655, IMAXV)
      CALL ADVERB ('INVERS', 'I', 1, 0, IBVER, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF ((IBVER.LT.1) .OR. (IBVER.GT.IMAXV)) IBVER = 1
C                                       Is it a table file?
      IF (IETYPE.EQ.10) THEN
         DO 40 IVER = IBVER,IMAXV
            CALL ISTAB (IECHAR, IVOL, ISLOT, IVER, IELUN, IDATA, TABLE,
     *         EXIST, FITASC, IERR)
            IF ((IERR.EQ.0) .AND. (TABLE) .AND. (EXIST)) GO TO 50
 40         CONTINUE
         WRITE (MSGTXT,1040) CINEXT
         GO TO 60
C                                       It is a table type
 50      IETYPE = 5
         END IF
C                                       Write listing headers
 60   CALL H2CHR (18, 1,CATH(KHIMN), CTEMP)
      IF (IETYPE.LE.5) GO TO (100, 100, 105, 110, 110), IETYPE
         IF (IETYPE.EQ.6) WRITE (MSGTXT,1060)
         IF (IETYPE.EQ.7) WRITE (MSGTXT,1061)
         IF (IETYPE.EQ.8) WRITE (MSGTXT,1062)
         CALL MSGWRT (7)
         GO TO 980
C                                       Plot files
 100  CONTINUE
         WRITE (MSGTXT,1100) IUSER, IVOL
         CALL NAMEST (CTEMP, ISEQ, MSGTXT(34:), I)
         CALL MSGWRT (5)
         WRITE (MSGTXT,1102)
         CALL MSGWRT (5)
         GO TO 120
C                                       Write slice file header.
 105  CONTINUE
         WRITE (MSGTXT,1105) IUSER, IVOL
         CALL NAMEST (CTEMP, ISEQ, MSGTXT(34:), I)
         CALL MSGWRT (5)
         WRITE (MSGTXT,1107)
         CALL MSGWRT (5)
         IPTYPE = 0
         GO TO 120
C                                       Write CC file header.
 110  CONTINUE
         WRITE (MSGTXT,1110) IECHAR, IUSER, IVOL
         CALL NAMEST (CTEMP, ISEQ, MSGTXT(34:), I)
         CALL MSGWRT (5)
         WRITE (MSGTXT,1112)
         CALL MSGWRT (5)
C                                       Loop for all versions.
 120  DO 890 IVER = IBVER,IMAXV
C                                       Tables stand alone
         IF ((IETYPE.EQ.4) .OR. (IETYPE.EQ.5)) GO TO 830
         IS = 11
         NCNT = 0
C                                       Open extension file.
C                                       Turn off error msgs.
 124     MSGSUP = 32000
         CALL OPEXT (IECHAR, IVOL, ISLOT, IVER, IELUN, NOEXCL, NOWAIT,
     *      IFIND, IERR)
         MSGSUP = 0
         IF (IERR.EQ.0) GO TO 125
            IF (IERR.NE.5) GO TO 890
C                                       wait for busy files a little
            NCNT = NCNT + 1
            IF (NCNT.LE.5) THEN
               CALL ZDELAY (TDEL, IERR)
               GO TO 124
               END IF
            WRITE (MSGTXT,1124) IVER
            CALL MSGWRT (6)
            GO TO 890
C                                       Read first plot record
 125     IF (IETYPE.EQ.3) GO TO 800
         CALL ZFIO ('READ', IELUN, IFIND, 1, IDATA, IERR)
         IF (IERR.NE.0) GO TO 885
         NPARMS = IDATA(10)
         IRNO = (NPARMS + 9) / 256 + 2
C                                       first record of plot commands
         CALL ZFIO ('READ', IELUN, IFIND, IRNO, IDATA, IERR)
         IF (IERR.NE.0) GO TO 885
         IPTYPE = IDATA(6)
         IF ((IPTYPE.LT.-10) .OR. (IPTYPE.GT.100)) GO TO 880
C                                       Slice plot BLC, TRC.
         IF (IPTYPE.EQ.5) THEN
            CALL COPY (4, IDATA(32), TEMP)
            IBDROP = IDATA(30)
            IEDROP = IDATA(31)
C                                       Determine number of slice points
            X = ABS(TEMP(3) - TEMP(1)) + 1.0
            Y = ABS(TEMP(4) - TEMP(2)) + 1.0
            IMAX = SQRT (X*X + Y*Y) + .5
            INOSL = 512
            IF (IMAX.GT.256) INOSL = 1024
            IF (IMAX.GT.512) INOSL = 2048
            IF (IMAX.GT.1024) INOSL = 4096
C                                       Calculate slice blc, trc, with
C                                       bdrop & edrop
            IBLC(1) = (TEMP(3) - TEMP(1)) * IBDROP / (INOSL - 1)
     *         + TEMP(1) + 0.5
            IBLC(2) = (TEMP(4) - TEMP(2)) * IBDROP / (INOSL - 1)
     *         + TEMP(2) + 0.5
            ITRC(1) = TEMP(3) - (TEMP(3) - TEMP(1)) * IEDROP /
     *         (INOSL - 1) + 0.5
            ITRC(2) = TEMP(4) - (TEMP(4) - TEMP(2)) * IEDROP /
     *         (INOSL - 1) + 0.5
C                                       GINITL window
         ELSE
            CALL COPY (2, IDATA(10), IBLC)
            CALL COPY (2, IDATA(12), ITRC)
            IF ((IBLC(1).LT.-100000) .OR. (IBLC(2).LT.-100000) .OR.
     *         (ITRC(1).GT.100000) .OR. (ITRC(2).GT.100000)) GO TO 880
            END IF
C                                       Read record 1.
         CALL ZFIO ('READ', IELUN, IFIND, 1, IDATA, IERR)
         IF (IERR.NE.0) GO TO 885
         CALL H2CHR (6, 1, IDATA, LTASK)
         NPARMS = IDATA(10)
         IF (NPARMS.GT.246) THEN
            CALL ZFIO ('READ', IELUN, IFIND, 2, IDATA(257), IERR)
            IF (IERR.NE.0) GO TO 885
            END IF
         IF (NPARMS.GT.502) THEN
            CALL ZFIO ('READ', IELUN, IFIND, 3, IDATA(513), IERR)
            IF (IERR.NE.0) GO TO 885
            END IF
         CALL ZCLOSE (IELUN, IFIND, IERR)
         IFIND = 0
C                                       Do time and date.
         IDATA(4) = -IDATA(4)
         CALL TIMDAT (IDATA(7), IDATA(4), TIME, DATE)
C                                       Branch to detailed file type
         IF ((IPTYPE.LT.1) .OR. (IPTYPE.GT.NPLOTS)) GO TO 790
C                                       determine version
         CVER = 0
         DO 155 ISX = 1,NVERS
            IF ((PNPARM(IPTYPE,ISX).GT.0) .AND. (CVER.EQ.0)) CVER = 99
            IF (NPARMS.EQ.PNPARM(IPTYPE,ISX)) CVER = ISX
 155        CONTINUE
C                                       unrecognized version
         IF (CVER.EQ.99) THEN
            WRITE (MSGTXT,1155) NPARMS
            CALL MSGWRT (6)
            CVER = 0
            END IF
         GO TO (210, 220, 230, 240, 250, 260, 270, 280, 200, 200,
     *          310, 200, 330, 340, 350, 360, 370, 380, 390, 400,
     *          410, 420, 430, 440, 450, 460, 470, 480, 490, 500,
     *          510, 520, 530, 540, 420, 550, 560, 570, 580, 590,
     *          600, 610, 620, 630, 640, 650, 660, 670, 680, 690,
     *          700, 710, 720, 730, 740, 750), IPTYPE
C                                       Cancelled types
 200     CONTINUE
            MSGTXT = PNAMES(IPTYPE) // ' no longer supported'
            CALL MSGWRT (6)
            GO TO 790
C                                       Miscellaneous plot file.
 210     CONTINUE
            WRITE (MSGTXT,1210) IVER, DATE
            CALL MSGWRT (5)
            WRITE (MSGTXT,1211) LTASK, TIME
            CALL MSGWRT (5)
            GO TO 890
C                                       Contour plot - CNTR
 220     CONTINUE
            ISX = IS + 8
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            XPLEV = DATA(IS+24)
            XCLEV = DATA(IS+25)
            IF (XPLEV.GT.0.0001) XCLEV = XPLEV * CATR(KRDMX) / 100.0
            BTEMP = ' C'
            WRITE (MSGTXT,1220) IVER, BTEMP, DATE, IBLC, XCLEV
            CALL MSGWRT (5)
            WRITE (MSGTXT,1221) TIME, ITRC
C                                       Do levels.
            ICTR = 35
            DO 225 I = 1,30
               XLEVS = DATA(25+IS+I)
               WRITE (CTEMP,1222,ERR=224) XLEVS
               IF ((XLEVS.GE.-99.90) .AND. (XLEVS.LE.999.90))
     *            WRITE (CTEMP,1223,ERR=224) XLEVS
               IF ((XLEVS.GE.-9.990) .AND. (XLEVS.LE.99.990))
     *            WRITE (CTEMP,1224,ERR=224) XLEVS
               IF ((XLEVS.GE.-0.9990) .AND. (XLEVS.LE.9.9990))
     *            WRITE (CTEMP,1225,ERR=224) XLEVS
 224           IF (XLEVS.GE.DATA(IS+26+I)) THEN
                  GO TO 226
               ELSE
                  MSGTXT(ICTR:ICTR+6) = CTEMP(1:7)
                  ICTR = ICTR + 7
C                                       Last lev on line.
                  IF (ICTR.GT.58) THEN
                     CALL MSGWRT (5)
                     MSGTXT = ' '
                     ICTR = 35
                     END IF
                  END IF
 225           CONTINUE
 226        MSGTXT(ICTR:) = CTEMP(1:6) // ')'
            CALL MSGWRT (5)
C                                       Stars
            STVERS = DATA(IS+57) + 0.01
            STFACT = DATA(IS+58)
            IF ((STVERS.GT.0) .AND. (STFACT.GT.0.0)) THEN
               WRITE (MSGTXT,1228) STVERS, STFACT
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       Grey scale - GREYS
 230     CONTINUE
            ISX = IS + 16
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            RANGE1 = DATA(IS+31)
            RANGE2 = DATA(IS+32)
            WRITE (MSGTXT,1230) IVER, DATE, IBLC, RANGE1
            CALL MSGWRT (5)
            WRITE (MSGTXT,1231) TIME, ITRC, RANGE2
            CALL MSGWRT (5)
C                                       Grey scale file contains
C                                       contour commands
            IF (DATA(IS).GT.0.0) THEN
               CALL H2CHR (12, 1, DATA(IS+9), NAME)
               CALL H2CHR (6, 1, DATA(IS+12), CLASS)
               I = DATA(IS+14)
               J = DATA(IS+15)
               WRITE (MSGTXT,1232) 'Contour image:     ', NAME, CLASS,
     *            I, J
               CALL MSGWRT (5)
               XCLEV = DATA(IS+36)
               WRITE (MSGTXT,1233) XCLEV
C                                       Do levels.
               ICTR = 33
               DO 235 I = 1,30
                  XLEVS = DATA(36+IS+I)
                  WRITE (CTEMP,1222,ERR=234) XLEVS
                  IF ((XLEVS.GE.-99.90) .AND. (XLEVS.LE.999.90))
     *               WRITE (CTEMP,1223,ERR=234) XLEVS
                  IF ((XLEVS.GE.-9.990) .AND. (XLEVS.LE.99.990))
     *               WRITE (CTEMP,1224,ERR=234) XLEVS
                  IF ((XLEVS.GE.-0.9990) .AND. (XLEVS.LE.9.9990))
     *               WRITE (CTEMP,1225,ERR=234) XLEVS
 234              IF (XLEVS.GE.DATA(IS+37+I)) THEN
                     GO TO 236
                  ELSE
                     MSGTXT(ICTR:ICTR+6) = CTEMP(1:7)
                     ICTR = ICTR + 7
C                                       Last lev on line.
                     IF (ICTR.GT.58) THEN
                        CALL MSGWRT (5)
                        MSGTXT = ' '
                        ICTR = 12
                        END IF
                     END IF
 235              CONTINUE
 236           MSGTXT(ICTR:) = CTEMP(1:6) // ')'
               CALL MSGWRT (5)
               END IF
            STVERS = DATA(IS+69) + 0.01
            STFACT = DATA(IS+70)
            IF ((STVERS.GT.0) .AND. (STFACT.GT.0.0)) THEN
               WRITE (MSGTXT,1228) STVERS, STFACT
               CALL MSGWRT (5)
               END IF
            I = DATA(IS+71) + 0.01
            J = DATA(IS+72) + 0.01
            IF ((I.GT.0) .AND. (I.LE.J)) THEN
               WRITE (MSGTXT,1236) I, J
               CALL MSGWRT (5)
               END IF
            II = DATA(IS+73)
            IF (II.GT.0) THEN
               ISX = IS + 73
               DO 238 I = 1,II
                  IBLC(1) = DATA(ISX+1) + .5
                  IBLC(2) = DATA(ISX+2) + .5
                  ITRC(1) = DATA(ISX+3) + .5
                  ITRC(2) = DATA(ISX+4) + .5
                  ISX = ISX + 4
                  WRITE (MSGTXT,1237) I, IBLC, ITRC
                  CALL MSGWRT (5)
 238              CONTINUE
               END IF
            GO TO 890
C                                       Profile - PROFL
 240     CONTINUE
            ISX = IS + 8
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            ZXR = DATA(IS+23)
            IROT = IROUND (DATA(IS+24))
            ISKW = IROUND (DATA(IS+25))
            IOBSVR = IROUND (DATA(IS+26))
            IF ((DATA(IS+28).LT.-10.) .OR. (DATA(IS+27).LT.-10.) .OR.
     *         (DATA(IS+28).GT.999.) .OR. (DATA(IS+27).GT.999.))
     *         GO TO 880
            IX = DATA(IS+27) + .5
            IY = DATA(IS+28) + .5
            WRITE (MSGTXT,1240) IVER, DATE, IBLC, IROT, ISKW, IOBSVR
            CALL MSGWRT (5)
            WRITE (MSGTXT,1242) TIME, ITRC, IX, IY, ZXR
            CALL MSGWRT (5)
            STVERS = DATA(IS+33) + 0.01
            STFACT = DATA(IS+34)
            IF ((STVERS.GT.0) .AND. (STFACT.GT.0.0)) THEN
               WRITE (MSGTXT,1228) STVERS, STFACT
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       Slice - SL2PL
 250     CONTINUE
            RANGE1 = DATA(IS+14)
            RANGE2 = DATA(IS+15)
            IF ((DATA(IS+11).LT.-10.) .OR. (DATA(IS+11).GT.256.))
     *         GO TO 880
            IX = DATA(IS+11) + 0.5
            WRITE (MSGTXT,1250) IVER, DATE, IBLC, RANGE1, IX
            CALL MSGWRT (5)
            WRITE (MSGTXT,1231) TIME, ITRC, RANGE2
            CALL MSGWRT (5)
            GO TO 890
C                                       Polarization vector plot - PCNTR
 260     CONTINUE
            ISX = IS + 24
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            XPLEV = DATA(IS+40)
            XCLEV = DATA(IS+41)
            FAC = DATA(IS+72)
            IROT = IROUND (DATA(IS+73))
            IF ((DATA(IS+75).LT.-10.) .OR. (DATA(IS+74).LT.-10.) .OR.
     *         (DATA(IS+75).GT.999.) .OR. (DATA(IS+74).GT.999.))
     *         GO TO 880
            IX = DATA(IS+74) + .5
            IY = DATA(IS+75) + .5
            XPCUT = DATA(IS+76)
            XICUT = DATA(IS+77)
            IF (XPLEV.GT.0.0001) XCLEV = XPLEV * CATR(KRDMX) / 100.0
            IF (DATA(IS+1).GT.0.0) THEN
               WRITE (MSGTXT,1260,ERR=261) IVER, DATE, IBLC, FAC, IX, IY
 261           CALL MSGWRT (5)
               WRITE (MSGTXT,1261,ERR=262) TIME, ITRC, XICUT, XPCUT
 262           CALL MSGWRT (5)
               CALL H2CHR (12, 1, DATA(IS+10), NAME)
               CALL H2CHR (6, 1, DATA(IS+13), CLASS)
               I = DATA(IS+15)
               J = DATA(IS+16)
               IF (DATA(IS+1).LT.1.5) THEN
                  WRITE (MSGTXT,1232) 'Vector length img: ', NAME,
     *               CLASS, I, J
               ELSE IF (DATA(IS+1).LT.2.5) THEN
                  WRITE (MSGTXT,1232) 'Q Stokes image:    ', NAME,
     *               CLASS, I, J
               ELSE
                  WRITE (MSGTXT,1263)
                  END IF
               CALL MSGWRT (5)
               CALL H2CHR (12, 1, DATA(IS+17), NAME)
               CALL H2CHR (6, 1, DATA(IS+20), CLASS)
               I = DATA(IS+22)
               J = DATA(IS+23)
               IF (DATA(IS+1).LT.1.5) THEN
                  WRITE (MSGTXT,1232) 'Vector angle img:  ', NAME,
     *               CLASS, I, J
                  CALL MSGWRT (5)
               ELSE IF (DATA(IS+1).LT.2.5) THEN
                  WRITE (MSGTXT,1232) 'U Stokes image:    ', NAME,
     *               CLASS, I, J
                  CALL MSGWRT (5)
                  END IF
            ELSE
               WRITE (MSGTXT,1264) IVER, DATE, IBLC
               CALL MSGWRT (5)
               WRITE (MSGTXT,1221) TIME, ITRC
               CALL MSGWRT (5)
               END IF
            IF (DATA(IS).GT.0.0) THEN
               WRITE (MSGTXT,1266) IROT, XCLEV
               CALL MSGWRT (5)
               MSGTXT = ' '
C                                       Do levels.
               ICTR = 35
               DO 266 I = 1,30
                  XLEVS = DATA(41+IS+I)
                  WRITE (CTEMP,1222,ERR=265) XLEVS
                  IF ((XLEVS.GE.-99.90) .AND. (XLEVS.LE.999.90))
     *               WRITE (CTEMP,1223,ERR=265) XLEVS
                  IF ((XLEVS.GE.-9.990) .AND. (XLEVS.LE.99.990))
     *               WRITE (CTEMP,1224,ERR=265) XLEVS
                  IF ((XLEVS.GE.-0.9990) .AND. (XLEVS.LE.9.9990))
     *               WRITE (CTEMP,1225,ERR=265) XLEVS
 265              IF (XLEVS.GE.DATA(IS+42+I)) THEN
                     GO TO 268
                  ELSE
                     MSGTXT(ICTR:ICTR+6) = CTEMP(1:7)
                     ICTR = ICTR + 7
C                                       Last lev on line.
                     IF (ICTR.GT.58) THEN
                        CALL MSGWRT (5)
                        MSGTXT = ' '
                        ICTR = 35
                        END IF
                     END IF
 266              CONTINUE
 268           MSGTXT(ICTR:ICTR+5) = CTEMP(1:6)
               CALL MSGWRT (5)
               END IF
            STVERS = DATA(IS+79) + 0.01
            STFACT = DATA(IS+81)
            IF ((STVERS.GT.0) .AND. (STFACT.GT.0.0)) THEN
               WRITE (MSGTXT,1228) STVERS, STFACT
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       Histogram - IMEAN
 270     CONTINUE
            COFF1 = 0
            IF (CVER.EQ.2) COFF1 = 1
            ISX = IS + 9
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            IF ((DATA(IS+24-COFF1).LT.-100.) .OR.
     *         (DATA(IS+24-COFF1).GT.9999.)) GO TO 880
            NBXS = DATA(IS+24-COFF1) + .5
            RANGE1 = DATA(IS+25-COFF1)
            RANGE2 = DATA(IS+26-COFF1)
            WRITE (MSGTXT,1270) IVER, DATE, IBLC, RANGE1, NBXS
            CALL MSGWRT (5)
            CALL H2CHR (2, 1, HDATA(IS+27-COFF1), BTEMP)
            WRITE (MSGTXT,1271) TIME, ITRC, RANGE2, BTEMP
            CALL MSGWRT (5)
            GO TO 890
C                                       UV plot file - UVPLT
C                                       CVER=2 DOACOR added
C                                          (BP(10) changed)
C                                       CVER=3 add pdver at 254
C                                       CVER=4 diff after 246
C                                       (NCHAV, APARM, XYRATIO added)
 280     CONTINUE
            COFF1 = 0
            IF (CVER.GT.3) COFF1 = 1
C                                       BCHAN check
            IF ((DATA(IS+245).LT.-100.) .OR. (DATA(IS+245).GT.39999.))
     *         GO TO 880
C                                       XINC check
            ICTR = IS + 262 - COFF1
            IF ((DATA(ICTR).LT.-100.) .OR. (DATA(ICTR).GT.99999.))
     *         GO TO 880
            ITRC(1) = DATA(ICTR) + 0.1
            CALL H2CHR (2, 1, HDATA(IS+130), BTEMP)
            ITRC(2) = DATA(IS+244) + 0.1
            WRITE (MSGTXT,1280) IVER, 'UV ', DATE, BTEMP, ITRC(1),
     *         'subarr', ITRC(2)
            MSGTXT(23:33) = 'No cal done'
            IF (CVER.LE.3) THEN
               IF (DATA(IS+251).GT.0.0) MSGTXT(23:33) = 'Cal applied'
            ELSE
               IF (DATA(IS+252-COFF1).GT.0.0) MSGTXT(23:33) =
     *            'Cal applied'
               END IF
            CALL MSGWRT (5)
C                                       Axes: type, range
            ICTR = IS + 261
            IF (CVER.LE.3) ICTR = IS + 273
            IF (CVER.EQ.4) ICTR = IS + 272
            CALL RCOPY (10, DATA(ICTR), TDATA)
            IF ((TDATA(1).LT.-10.) .OR. (TDATA(1).GT.99.)) GO TO 880
            IF ((TDATA(2).LT.-10.) .OR. (TDATA(2).GT.99.)) GO TO 880
            IX = TDATA(1) + 0.1
            IF (IX.EQ.11) THEN
               TDATA(4) = TDATA(4) * 24.0
               TDATA(5) = TDATA(5) * 24.0
               END IF
            WRITE (MSGTXT,1281) TIME, TUVPLT(IX), TDATA(4), TDATA(5)
            CALL MSGWRT (5)
            IX = TDATA(2) + 0.01
            IF (IX.EQ.11) THEN
               TDATA(6) = TDATA(6) * 24.0
               TDATA(7) = TDATA(7) * 24.0
               END IF
            IF (TDATA(6).GE.TDATA(7)) THEN
               WRITE (MSGTXT,1283) TUVPLT(IX)
            ELSE
               WRITE (MSGTXT,1282) TUVPLT(IX), TDATA(6), TDATA(7)
               END IF
            CALL MSGWRT (5)
C                                       time range
            ICTR = IS + 133
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=284) TS1, IT1, TS2, IT2
               END IF
 284        CALL MSGWRT (5)
C                                       UV range
            ICTR = IS + 242
            IF ((DATA(ICTR).GT.0.0) .OR. ((DATA(ICTR).LT.DATA(ICTR+1))
     *         .AND. (DATA(ICTR+1).LT.1.0E9))) THEN
               WRITE (MSGTXT,1285) DATA(ICTR), DATA(ICTR+1)
               CALL MSGWRT (5)
               END IF
C                                       channels range
            ITRC(1) = DATA(IS+245) + 0.1
            ITRC(2) = DATA(IS+246) + 0.1
            COFF2 = IS+248
            IX = DATA(COFF2) + 0.1
            IBLC(1) = DATA(COFF2+1) + 0.1
            IBLC(2) = DATA(COFF2+2) + 0.1
            WRITE (MSGTXT,2285) ITRC, IX, IBLC
            CALL MSGWRT (5)
C                                       antennas, baseline
            ICTR = IS + 141
            NANT1 = 0
            NANT2 = 0
            IANT1(1) = 0
            IANT2(1) = 0
            DO 286 I = 1,50
               IX = IROUND (DATA(ICTR+I))
               IF ((NANT1.GT.0) .AND. (IX.EQ.IANT1(NANT1))) IX = 0
               IF (IX.NE.0) THEN
                  NANT1 = NANT1 + 1
                  IANT1(NANT1) = IX
                  END IF
               IX = IROUND (DATA(ICTR+I+50))
               IF ((NANT2.GT.0) .AND. (IX.EQ.IANT2(NANT2))) IX = 0
               IF (IX.NE.0) THEN
                  NANT2 = NANT2 + 1
                  IANT2(NANT2) = IX
                  END IF
 286           CONTINUE
C                                       print them - all
            MSGTXT = ' '
            IF ((NANT1.LE.0) .AND. (NANT2.LE.0)) THEN
               MSGTXT(23:) = 'All antenna pairs included'
               CALL MSGWRT (5)
C                                       by ones
            ELSE IF ((NANT1.LE.1) .AND. (NANT2.LE.1)) THEN
               WRITE (MSGTXT,1286) IANT1(1), IANT2(1)
               IF (IANT1(1).EQ.0) MSGTXT(31:32) = '**'
               IF (IANT2(1).EQ.0) MSGTXT(39:40) = '**'
               CALL MSGWRT (5)
C                                       individually
            ELSE
               IF (NANT1.LE.0) THEN
                  MSGTXT(23:) = 'All antennas'
                  CALL MSGWRT (5)
               ELSE
                  IY = 0
 287              IX = IY + 1
                  IF (IX.LE.NANT1) THEN
                     IY = MIN (IX+15, NANT1)
                     WRITE (MSGTXT,1287) (IANT1(I), I = IX,IY)
                     CALL MSGWRT (5)
                     GO TO 287
                     END IF
                  END IF
               IF (NANT2.LE.0) THEN
                  MSGTXT(:22) = ' '
                  MSGTXT(23:) = 'With all antennas'
                  CALL MSGWRT (5)
               ELSE
                  IY = 0
 288              IX = IY + 1
                  IF (IX.LE.NANT2) THEN
                     IY = MIN (IX+15, NANT2)
                     WRITE (MSGTXT,1288) (IANT2(I), I = IX,IY)
                     CALL MSGWRT (5)
                     GO TO 288
                     END IF
                  END IF
               END IF
C                                       UVPLT binning
            IF (TDATA(8).GT.0.5) THEN
               I = IROUND (TDATA(8))
               WRITE (MSGTXT,1289) I
               CALL MSGWRT (5)
               END IF
C                                       XYRATIO
            IF (CVER.EQ.1) THEN
               T1 = DATA(IS+303)
            ELSE IF (CVER.EQ.2) THEN
               T1 = DATA(IS+302)
            ELSE
               T1 = DATA(IS+301-COFF1)
               END IF
            IF ((CVER.LE.3) .AND. (T1.NE.1.0) .AND. (T1.NE.0.0)) THEN
               WRITE (MSGTXT,1367) T1
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       Self-cal Gain file plots - GNPLT
 290     CONTINUE
            GO TO 890
C                                       VBPLT
 300     CONTINUE
            GO TO 890
C                                       PFPL1,2,3: paraforms
 310     CONTINUE
            ISX = IS + 8
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            RANGE1 = DATA(IS+22)
            RANGE2 = DATA(IS+23)
            WRITE (MSGTXT,1310) IVER, LTASK, DATE, IBLC, RANGE1
            CALL MSGWRT (5)
            WRITE (MSGTXT,1231) TIME, ITRC, RANGE2
            CALL MSGWRT (5)
            GO TO 890
C                                       GAPLT
 320     CONTINUE
            GO TO 890
C                                       PLCUB
 330     CONTINUE
            ISX = IS + 8
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            RANGE1 = DATA(IS+24)
            RANGE2 = DATA(IS+25)
            WRITE (MSGTXT,1330) IVER, DATE, IBLC, RANGE1
            CALL MSGWRT (5)
            WRITE (MSGTXT,1231) TIME, ITRC, RANGE2
            CALL MSGWRT (5)
            IBLCX = DATA(IS+10) + 0.01
            ITRCX = DATA(IS+17) + 0.01
            IX = DATA(IS+22) + 0.01
            IY = DATA(IS+23) + 0.01
            WRITE (MSGTXT,1332) IBLCX, ITRCX, IX, IY
            CALL MSGWRT (5)
            GO TO 890
C                                       IMVIM
 340     CONTINUE
            ISX = IS + 14
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            RANGE1 = DATA(IS+34)
            RANGE2 = DATA(IS+35)
            WRITE (MSGTXT,1340) IVER, DATE, IBLC, RANGE1
            CALL MSGWRT (5)
            WRITE (MSGTXT,1231) TIME, ITRC, RANGE2
            CALL MSGWRT (5)
            CALL H2CHR (12, 1, DATA(IS+7), NAME)
            CALL H2CHR (6, 1, DATA(IS+10), CLASS)
            I = DATA(IS+12)
            J = DATA(IS+13)
            WRITE (MSGTXT,1341) NAME, CLASS, I, J
            CALL MSGWRT (5)
            RANGE1 = DATA(IS+36)
            RANGE2 = DATA(IS+37)
            WRITE (MSGTXT,1342) RANGE1, RANGE2
            CALL MSGWRT (5)
            IX = DATA(IS+29) + 0.1
            IY = DATA(IS+30) + 0.1
            IV = DATA(IS+31) + 0.1
            CALL H2CHR (2, 1, DATA(IS+33), BTEMP)
            CALL H2CHR (4, 1, DATA(IS+32), CTEMP)
            WRITE (MSGTXT,1343) IX, IY, IV, CTEMP(:4), BTEMP
            CALL MSGWRT (5)
            IF (CTEMP(1:3).EQ.'BIN') THEN
               I = DATA(IS+38) + 0.1
               J = DATA(IS+39) + 0.1
               WRITE (MSGTXT,1344) I, J
               CALL MSGWRT (5)
               END IF
            IX = IROUND (DATA(IS+41))
            IY = IROUND (DATA(IS+42))
            WRITE (MSGTXT,1345) DATA(IS+40), IX, IY
            CALL MSGWRT (5)
            GO TO 890
C                                       TAPLT
 350     CONTINUE
            IBCNT = DATA(IS+10) + 0.1
            IECNT = DATA(IS+11) + 0.1
            IICNT = DATA(IS+12) + 0.1
            WRITE (MSGTXT,1350) IVER, DATE, IBCNT, IECNT, IICNT
            CALL MSGWRT (5)
            I = DATA(IS+9) + 0.1
            J = DATA(IS+24) + 0.1
            IF (J.LE.0) THEN
               WRITE (MSGTXT,1351) TIME, DATA(IS+8), I
            ELSE
               WRITE (MSGTXT,1352) TIME, DATA(IS+8), I, J
               END IF
            CALL MSGWRT (5)
            DO 356 II = 1,2
               IBLC(1) = IROUND(DATA(IS+9+5*II))
               ITRC(1) = IROUND(DATA(IS+10+5*II))
               IBLC(2) = IROUND(DATA(IS+11+5*II))
               ITRC(2) = IROUND(DATA(IS+12+5*II))
               I = IROUND(DATA(IS+13+5*II))
               ABSEM(1) = IBLC(1).LT.0
               ABSEM(2) = IBLC(2).LT.0
               IBLC(1) = ABS(IBLC(1))
               IBLC(2) = ABS(IBLC(2))
               EQUAL = I.GT.9
               IF ((DATA(IS+13).GT.0.0) .AND. (II.EQ.2)) THEN
                  ABSEM(1) = .FALSE.
                  IBLC(1) = -1
                  EQUAL = .FALSE.
                  END IF
               WRITE (MSGTXT,1353) CHAX(II)
               IP = 1
               IX = 4 * II + 30 + IS
               IY = 31 + II + IS
               RESULT = ' '
               COLTYP(1) = ' '
               COLTYP(2) = ' '
               IF ((DATA(IS+13).GT.0.0) .AND. (II.EQ.2)) COLTYP(1) =
     *            'HISTGRAM'
               IF (IBLC(1).EQ.0) COLTYP(1) = 'ROW'
               IF ((EQUAL) .AND. (IBLC(2).EQ.0)) COLTYP(2) = 'ROW'
               ABLC(1) = 1.0
               ABLC(2) = 0.0
               CALL SELSTR (IP, I, EQUAL, ABSEM, IBLC, ITRC, DATA(IX),
     *            ABLC, DATA(IY), COLTYP, IP, RESULT)
               IP = IP - 1
               IY = -39
 354           IY = IY + 40
               IF (IY.LE.IP) THEN
                  IX = 40
                  IF (IY+IX-1.GT.IP) IX = IP + 1 - IY
                  MSGTXT(25:24+IX) = RESULT(IY:IY+IX-1)
                  CALL MSGWRT (5)
                  MSGTXT = ' '
                  GO TO 354
                  END IF
 356           CONTINUE
            DO 358 II = 1,2
               RANGE1 = DATA(IS+25+2*II)
               RANGE2 = DATA(IS+26+2*II)
               WRITE (MSGTXT,1356) CHAX(II), RANGE1, RANGE2
               CALL MSGWRT (5)
 358           CONTINUE
C                                       row selection adverbs
            IF (CVER.LE.1) THEN
               IP = IS + 40
               DO 359 I = 1,7
                  IP = IP + 4
                  IX = DATA(IP) + 0.1
                  IY = DATA(IP+1) + 0.1
                  IF (IX.GT.0) THEN
                     IF (I.EQ.1) THEN
                        CALL H2CHR (16, 1, DATA(IS+74), STRNG)
                        IF (JTRIM(STRNG(:16)).GT.0) THEN
                           WRITE (MSGTXT,1357) STRNG(:16)
                           CALL MSGWRT (5)
                           END IF
                        END IF
                     WRITE (MSGTXT,1358) IX, IY, DATA(IP+2), DATA(IP+3)
                     CALL MSGWRT (5)
                  ELSE
                     GO TO 890
                     END IF
 359              CONTINUE
               END IF
            GO TO 890
C                                       POSSM
C                                       CVER=1 forces change after 251
C                                       CVER=2 differ at 300 from 3/4
C                                       CVER=3,4 differ after 251
C                                       POSSM change of 2003-08-04
 360     CONTINUE
            COFF1 = 0
            IF (CVER.GT.1) COFF1 = 1
            IF (CVER.EQ.4) COFF1 = 2
            CALL H2CHR (8, 1, HDATA(IS+273-COFF1), CTEMP)
            IF ((CTEMP.NE.'RL/RR') .AND. (CTEMP.NE.'RL/LL') .AND.
     *         (CTEMP.NE.'LR/RR') .AND. (CTEMP.NE.'LR/LL') .AND.
     *         (CTEMP.NE.'RR/LL') .AND. (CTEMP.NE.'LL/RR'))
     *         CALL H2CHR (4, 1, HDATA(IS+143), CTEMP)
            WRITE (MSGTXT,1360) IVER, DATE, CTEMP
            IF (DATA(IS+262-COFF1).GE.0.0) THEN
               MSGTXT(36:) = 'vector averaging'
            ELSE
               MSGTXT(36:) = 'scalar averaging'
               END IF
C                                       plot type
            J = IROUND (DATA(IS+269-COFF1)) + 1
            IF ((J.LT.1) .OR. (J.GT.9)) J = 10
            MSGTXT(53:) = TYPEBP(J)
            CALL MSGWRT (5)
            CALL H2CHR (4, 1, HDATA(IS+272-COFF1), CODET)
            J1 = 1
            J2 = 2
            IF (CODET.EQ.'AMP ') THEN
               J2 = 0
            ELSE IF (CODET.EQ.'PHAS') THEN
               J1 = 0
            ELSE IF (CODET.EQ.'R&I ') THEN
               J1 = 9
               J2 = 10
            ELSE IF (CODET.EQ.'REAL') THEN
               J1 = 9
               J2 = 0
            ELSE IF (CODET.EQ.'IMAG') THEN
               J1 = 0
               J2 = 10
               END IF
C                                       amplitude range
            IF (J1.GT.0) THEN
               WRITE (MSGTXT,1361) TIME, TUVPLT(J1), DATA(IS+264-COFF1),
     *            DATA(IS+265-COFF1)
               CALL MSGWRT (5)
               END IF
C                                       phase range
            IF (J2.GT.0) THEN
               IF (J1.GT.0) THEN
                  WRITE (MSGTXT,1362) TUVPLT(J2), DATA(IS+266-COFF1),
     *               DATA(IS+267-COFF1)
               ELSE
                  WRITE (MSGTXT,1361) TIME, TUVPLT(J2),
     *               DATA(IS+266-COFF1), DATA(IS+267-COFF1)
                  END IF
               CALL MSGWRT (5)
               END IF
C                                       Divide by channel 0
            IF ((DATA(IS+277-COFF1).GT.0.0) .AND. (J.NE.3)) THEN
               J1 = DATA(IS+278-COFF1) + 0.1
               J2 = DATA(IS+279-COFF1) + 0.1
               WRITE (MSGTXT,1363) J1, J2, DATA(IS+280-COFF1)
               CALL MSGWRT (5)
               END IF
C                                       Channel, IF range
            IBLC(1) = DATA(IS+146)
            IBLC(2) = DATA(IS+147)
            ITRC(1) = DATA(IS+144)
            ITRC(2) = DATA(IS+145)
            IF (DATA(IS+218).GT.0.0) THEN
               WRITE (MSGTXT,1364) IBLC, ITRC, 'plotted'
            ELSE
               WRITE (MSGTXT,1364) IBLC, ITRC, 'averaged'
               END IF
            CALL MSGWRT (5)
C                                       Smooth
            IF (DATA(IS+257-COFF1).GT.0.5) THEN
               WRITE (MSGTXT,1365) DATA(IS+257-COFF1),
     *            DATA(IS+258-COFF1), DATA(IS+259-COFF1)
               CALL MSGWRT (5)
               END IF
C                                       Shift
            IF ((DATA(IS+260-COFF1).NE.0.0) .OR.
     *         (DATA(IS+261-COFF1).NE.0.0)) THEN
               WRITE (MSGTXT,1366) DATA(IS+260-COFF1),
     *            DATA(IS+261-COFF1)
               CALL MSGWRT (5)
               END IF
C                                       UV range
            ICTR = IS + 133
            IF ((DATA(ICTR).GT.0.0) .OR. ((DATA(ICTR).LT.DATA(ICTR+1))
     *         .AND. (DATA(ICTR+1).LE.1.0E9))) THEN
               WRITE (MSGTXT,1285) DATA(ICTR), DATA(ICTR+1)
               CALL MSGWRT (5)
               END IF
C                                       antennas, time range
C                                       time range
            ICTR = IS + 134
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=364) TS1, IT1, TS2, IT2
               END IF
 364        CALL MSGWRT (5)
C                                       Antennas
            NANT1 = 0
            ICTR = IS + 148
            DO 365 I = 1,50
               J = IROUND (DATA(ICTR+I))
               IF (J.NE.0) THEN
                  NANT1 = NANT1 + 1
                  IANT1(NANT1) = J
                  END IF
 365           CONTINUE
            IY = 0
 366        IX = IY + 1
            IF (IX.LE.NANT1) THEN
               IY = MIN (IX+15, NANT1)
               WRITE (MSGTXT,1287) (IANT1(I), I = IX,IY)
               CALL MSGWRT (5)
               GO TO 366
               END IF
            GO TO 890
C                                       SNPLT
C                                       CVER=2 doblank, doscan
C                                       CVER=3,4 no diff here
C                                       change of 2003-07-06
 370     CONTINUE
            CALL H2CHR (2, 1, HDATA(IS+7), BTEMP)
            CALL H2CHR (4, 1, HDATA(IS+199), ATEMP)
            CALL H2CHR (4, 1, HDATA(IS+200), DTEMP)
            I = DATA(IS+8) + 0.01
            WRITE (MSGTXT,1370) IVER, BTEMP, DATE, I, ATEMP(:4),
     *         DTEMP
            CALL MSGWRT (5)
            IF (CVER.GT.1) THEN
               RANGE1 = DATA(IS+195)
               RANGE2 = DATA(IS+196)
            ELSE
               RANGE1 = DATA(IS+211)
               RANGE2 = DATA(IS+221)
               END IF
            IF (RANGE2.GT.RANGE1) THEN
               WRITE (MSGTXT,1371) TIME, RANGE1, RANGE2
            ELSE
               WRITE (MSGTXT,1372) TIME
               END IF
            CALL MSGWRT (5)
C                                       time range
            ICTR = IS + 129
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE IF ((T1.NE.0) .OR. (T2.NE.0)) THEN
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=374) TS1, IT1, TS2, IT2
               END IF
 374        CALL MSGWRT (5)
C                                       Antennas
            NANT1 = 0
            ICTR = IS + 144
            DO 375 I = 1,50
               J = IROUND (DATA(ICTR+I))
               IF (J.NE.0) THEN
                  NANT1 = NANT1 + 1
                  IANT1(NANT1) = J
                  END IF
 375           CONTINUE
            IY = 0
 376        IX = IY + 1
            IF (IX.LE.NANT1) THEN
               IY = MIN (IX+15, NANT1)
               WRITE (MSGTXT,1287) (IANT1(I), I = IX,IY)
               CALL MSGWRT (5)
               GO TO 376
               END IF
            IF (CVER.LE.2) THEN
               IX = 2
               IY = 1
               IF (CVER.EQ.2) THEN
                  IF (DATA(IS+206).LE.0.0) IX = 1
                  IF (DATA(IS+208).LE.0.0) IY = 1
               ELSE
                  IF (DATA(IS+236).LE.0.0) IX = 1
                  IF (DATA(IS+238).LE.0.0) IY = 1
                  END IF
               WRITE (MSGTXT,1373) DIDIT(IX), DIDIT(IY)
               CALL MSGWRT (5)
               END IF
            IF ((CVER.EQ.1) .AND. (ATEMP.EQ.'MULT')) THEN
               DO 377 I = 1,10
                  J = DATA(IS+200+I)
                  IF (J.LE.0) GO TO 890
                  RANGE1 = DATA(IS+210+I)
                  RANGE2 = DATA(IS+220+I)
                  IF (RANGE2.GT.RANGE1) THEN
                     WRITE (MSGTXT,1374) SNTYPE(J), RANGE1, RANGE2
                  ELSE
                     WRITE (MSGTXT,1375) SNTYPE(J)
                     END IF
                  CALL MSGWRT (5)
 377           CONTINUE
               END IF
            GO TO 890
C                                       KNTR
 380     CONTINUE
            ISX = IS + 16
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            XCLEV = DATA(IS+38)
            BTEMP = ' K'
            WRITE (MSGTXT,1220) IVER, BTEMP, DATE, IBLC, XCLEV
            IF (DATA(IS+0).LE.0.0) MSGTXT(35:) = 'No contours drawn'
            CALL MSGWRT (5)
C                                       Do levels.
            WRITE (MSGTXT,1221) TIME, ITRC
            IF (DATA(IS+0).GT.0.0) THEN
               ICTR = 35
               DO 382 I = 1,30
                  XLEVS = DATA(38+IS+I)
                  WRITE (CTEMP,1222,ERR=381) XLEVS
                  IF ((XLEVS.GE.-99.90) .AND. (XLEVS.LE.999.90))
     *               WRITE (CTEMP,1223,ERR=381) XLEVS
                  IF ((XLEVS.GE.-9.990) .AND. (XLEVS.LE.99.990))
     *               WRITE (CTEMP,1224,ERR=381) XLEVS
                  IF ((XLEVS.GE.-0.9990) .AND. (XLEVS.LE.9.9990))
     *               WRITE (CTEMP,1225,ERR=381) XLEVS
 381              IF (XLEVS.GE.DATA(IS+39+I)) THEN
                     GO TO 383
                  ELSE
                     MSGTXT(ICTR:ICTR+6) = CTEMP(1:7)
                     ICTR = ICTR + 7
C                                       Last lev on line.
                     IF (ICTR.GT.58) THEN
                        CALL MSGWRT (5)
                        MSGTXT = ' '
                        ICTR = 35
                        END IF
                     END IF
 382              CONTINUE
 383           MSGTXT(ICTR:) = CTEMP(1:6) // ')'
C                                       no contour
            ELSE
               MSGTXT(35:) = 'Grey-scale image(s) drawn'
               IF (DATA(IS+1).LE.0.0) MSGTXT(35:) =
     *            'No grey-scale images drawn'
               END IF
            CALL MSGWRT (5)
C                                       KNTR panels
            IBLC(1) = IROUND (DATA(IS+18))
            ITRC(1) = IROUND (DATA(IS+25))
            ITRC(2) = IROUND (DATA(IS+30))
            WRITE (MSGTXT,1381) IBLC(1), ITRC(1), ITRC(2)
            CALL MSGWRT (5)
C                                       2nd image
            IF ((DATA(IS).GT.1.5) .OR. (DATA(IS+1).GT.1.5)) THEN
               CALL H2CHR (12, 1, DATA(IS+9), NAME)
               CALL H2CHR (6, 1, DATA(IS+12), CLASS)
               I = DATA(IS+14)
               J = DATA(IS+15)
               WRITE (MSGTXT,1232) 'Contour image:     ', NAME, CLASS,
     *            I, J
               IF (DATA(IS+1).GT.1.5) WRITE (MSGTXT,1382) NAME, CLASS,
     *            I, J
               CALL MSGWRT (5)
               END IF
C                                       grey-scale
            IF (DATA(IS+1).GT.0.0) THEN
               WRITE (MSGTXT,1383) DATA(IS+33), DATA(IS+34), 'LN'
               CALL MSGWRT (5)
               END IF
C                                       Stars
            STVERS = DATA(IS+58) + 0.01
            STFACT = DATA(IS+59)
            IF ((STVERS.GT.0) .AND. (STFACT.GT.0.0)) THEN
               WRITE (MSGTXT,1228) STVERS, STFACT
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       UVHGM
C                                       CVER=1 add baddisk
C                                       CVER=2 add pdver at 253
C                                       CVER=3 start 2006-04-14 270
C                                       Cver=4 older 23 adverbs
C                                       both +46 info for EXTLIST
C                                       vers 1,2,3 have XPARMS beyond
C                                       the adverbs
 390     CONTINUE
            COFF1 = 0
            IF (CVER.GT.2) COFF1 = 1
            IF (CVER.EQ.4) THEN
               NANT1 = IROUND (DATA(IS+9))
               NANT2 = IROUND (DATA(IS+10))
               J1 = IROUND (DATA(IS+11))
               J2 = IROUND (DATA(IS+11))
               CALL H2CHR (4, 1, DATA(IS+8), ATEMP)
               CALL H2CHR (10, 1, DATA(IS+30), XATYPE)
               CALL H2CHR (14, 1, DATA(IS+40), YATYPE)
            ELSE
               NANT1 = IROUND (DATA(IS+245))
               NANT2 = IROUND (DATA(IS+246))
               J1 = IROUND (DATA(IS+248))
               J2 = IROUND (DATA(IS+249))
               CALL H2CHR (4, 1, DATA(IS+130), ATEMP)
               IF (CVER.EQ.1) THEN
                  CALL H2CHR (10, 1, DATA(IS+281-COFF1), XATYPE)
                  CALL H2CHR (14, 1, DATA(IS+291-COFF1), YATYPE)
               ELSE
                  CALL H2CHR (10, 1, DATA(IS+271-COFF1), XATYPE)
                  CALL H2CHR (14, 1, DATA(IS+281-COFF1), YATYPE)
                  END IF
               END IF
            WRITE (MSGTXT,1390) IVER, DATE, NANT1, NANT2, J1, J2,
     *         ATEMP(:4)
            CALL MSGWRT (5)
            WRITE (MSGTXT,1391) TIME, XATYPE, YATYPE
            CALL MSGWRT (5)
            COFF2 = IS+70
            IF (CVER.EQ.2) COFF2 = IS+311-COFF1
            IF (CVER.EQ.1) COFF2 = IS+321-COFF1
            CALL H2CHR (12, 1, DATA(COFF2), YATYPE)
            WRITE (MSGTXT,1392) DATA(COFF2+3), DATA(COFF2+4),
     *         YATYPE(:12)
            CALL MSGWRT (5)
            NBXS = IROUND (DATA(IS+14))
            IF (CVER.LE.3) NBXS = IROUND (DATA(IS+262-COFF1))
            WRITE (MSGTXT,1393) NBXS, DATA(COFF2+5)
            CALL MSGWRT (5)
C                                       cal adverbs only in CVER=1,2
            IF (CVER.LE.2) THEN
C                                       time range
               ICTR = IS + 133
               T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *            DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
               T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *            DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
               IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *            THEN
                  MSGTXT = ' '
                  MSGTXT(13:) = 'times from beginning to end'
               ELSE
                  CALL T2DHMS (T1, TS1, IT1)
                  CALL T2DHMS (T2, TS2, IT2)
                  WRITE (MSGTXT,1284,ERR=391) TS1, IT1, TS2, IT2
                  END IF
 391           CALL MSGWRT (5)
C                                       UV range
               ICTR = IS + 242
               IF ((DATA(ICTR).GT.0.0) .OR.
     *            ((DATA(ICTR).LT.DATA(ICTR+1)) .AND.
     *            (DATA(ICTR+1).LE.1.0E9))) THEN
                  WRITE (MSGTXT,1285) DATA(ICTR), DATA(ICTR+1)
                  CALL MSGWRT (5)
                  END IF
C                                       antennas, baseline
               ICTR = IS + 141
               NANT1 = 0
               NANT2 = 0
               IANT1(1) = 0
               IANT2(1) = 0
               DO 392 I = 1,50
                  IX = IROUND (DATA(ICTR+I))
                  IF ((NANT1.GT.0) .AND. (IX.EQ.IANT1(NANT1))) IX = 0
                  IF (IX.NE.0) THEN
                     NANT1 = NANT1 + 1
                     IANT1(NANT1) = IX
                     END IF
                  IX = IROUND (DATA(ICTR+I+50))
                  IF ((NANT2.GT.0) .AND. (IX.EQ.IANT2(NANT2))) IX = 0
                  IF (IX.NE.0) THEN
                     NANT2 = NANT2 + 1
                     IANT2(NANT2) = IX
                     END IF
 392              CONTINUE
C                                       print them - all
               MSGTXT = ' '
               IF ((NANT1.LE.0) .AND. (NANT2.LE.0)) THEN
                  MSGTXT(23:) = 'All antenna pairs included'
                  CALL MSGWRT (5)
C                                       by ones
               ELSE IF ((NANT1.LE.1) .AND. (NANT2.LE.1)) THEN
                  WRITE (MSGTXT,1286) IANT1(1), IANT2(1)
                  IF (IANT1(1).EQ.0) MSGTXT(31:32) = '**'
                  IF (IANT2(1).EQ.0) MSGTXT(39:40) = '**'
                  CALL MSGWRT (5)
C                                       individually
               ELSE
                  IF (NANT1.LE.0) THEN
                     MSGTXT(23:) = 'All antennas'
                     CALL MSGWRT (5)
                  ELSE
                     IY = 0
 393                 IX = IY + 1
                     IF (IX.LE.NANT1) THEN
                        IY = MIN (IX+15, NANT1)
                        WRITE (MSGTXT,1287) (IANT1(I), I = IX,IY)
                        CALL MSGWRT (5)
                        GO TO 393
                        END IF
                     END IF
                  IF (NANT2.LE.0) THEN
                     MSGTXT(:22) = ' '
                     MSGTXT(23:) = 'With all antennas'
                     CALL MSGWRT (5)
                  ELSE
                     IY = 0
 394                 IX = IY + 1
                     IF (IX.LE.NANT2) THEN
                        IY = MIN (IX+15, NANT2)
                        WRITE (MSGTXT,1288) (IANT2(I), I = IX,IY)
                        CALL MSGWRT (5)
                        GO TO 394
                        END IF
                     END IF
                  END IF
C                                       subarray and fqid
               IX = IROUND (DATA(IS+244))
               IY = IROUND (DATA(IS+133))
               IF ((IX.GE.0) .AND. (IY.GE.0)) THEN
                  WRITE (MSGTXT,1394) IX, IY
               ELSE IF ((IX.GE.0) .AND. (IY.LT.0)) THEN
                  WRITE (MSGTXT,1395) IX, -IY
               ELSE IF ((IY.GE.0) .AND. (IX.LT.0)) THEN
                  WRITE (MSGTXT,1396) -IX, IY
               ELSE
                  WRITE (MSGTXT,1397) -IX, -IY
                  END IF
               IF (DATA(IS+250).GT.0.0) THEN
                  MSGTXT(41:) = 'Cal applied'
               ELSE
                  MSGTXT(41:) = 'No cal done'
                  END IF
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       ISPEC (20)
C                                       versions similar
C                                       xyratio in ver 1,2
 400     CONTINUE
            ISX = IS + 8
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            RANGE1 = DATA(IS+23)
            RANGE2 = DATA(IS+24)
            NS = DATA(IS+25) + 0.01
            WRITE (MSGTXT,1400) IVER, DATE, IBLC, RANGE1, NS
            IF (IPTYPE.EQ.34) MSGTXT(5:11) = 'RMSpect'
            CALL MSGWRT (5)
            BTEMP = 'pt'
            IF (DATA(IS+30).GT.0.0) BTEMP = 'bx'
            WRITE (MSGTXT,1271) TIME, ITRC, RANGE2, BTEMP
            CALL MSGWRT (5)
            LSGN(1) = DATA(IS+10) + 0.5
            LSGN(2) = DATA(IS+17) + 0.5
            X = 1.0
            IF (CVER.LE.2) X = DATA(IS+47)
            WRITE (MSGTXT,1402) LSGN, X
            CALL MSGWRT (5)
            CALL H2CHR (4, 1, HDATA(IS+22), CODET)
            IF (CODET.NE.' ') THEN
               WRITE (MSGTXT,1403) CODET
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       VPLOT
C                                       CVER=1 adds baddisk
C                                       CVER=3,4 differ after 210
C                                       VPLOT change of 2003-07-06
 410     CONTINUE
            COFF1 = -2
            IF (CVER.EQ.3) COFF1 = -2
            IF (CVER.EQ.4) COFF1 = 0
C                                       BCHAN
            IF ((DATA(IS+207).LT.-100.) .OR. (DATA(IS+207).GT.39999.))
     *         GO TO 880
C                                       XINC
            ICTR = IS + 213 - COFF1
            IF ((DATA(ICTR).LT.-100.) .OR. (DATA(ICTR).GT.9999.))
     *         GO TO 880
            ITRC(1) = DATA(ICTR) + 0.1
C                                       POLPLOT
            IF (CVER.LE.2) THEN
               CALL H2CHR (8, 1, HDATA(IS+395), ATEMP)
            ELSE IF (CVER.EQ.3) THEN
               CALL H2CHR (8, 1, HDATA(IS+394), ATEMP)
            ELSE
               CALL H2CHR (8, 1, HDATA(IS+391-COFF1), ATEMP)
               END IF
C                                       SOLINT
            SOLINT = DATA(IS+337-COFF1)
            IF (CVER.LE.2) SOLINT = DATA(IS+340)
            WRITE (MSGTXT,1410) IVER, 'V ', DATE, ATEMP, ITRC(1),
     *         SOLINT
C                                       DOCALIB
            IF (DATA(IS+325-COFF1).GT.0.0) THEN
               MSGTXT(23:33) = 'Cal applied'
            ELSE
               MSGTXT(23:33) = 'No cal done'
               END IF
            CALL MSGWRT (5)
C                                       Axes: type, range
            IF (CVER.LE.2) THEN
               CALL RCOPY (10, DATA(IS+353), TDATA)
            ELSE IF (CVER.EQ.3) THEN
               CALL RCOPY (10, DATA(IS+352), TDATA)
            ELSE
               CALL RCOPY (10, DATA(IS+349-COFF1), TDATA)
               END IF
            IF ((TDATA(1).LT.-10.) .OR. (TDATA(1).GT.99.)) GO TO 880
            IF ((TDATA(2).LT.-10.) .OR. (TDATA(2).GT.99.)) GO TO 880
            IX = TDATA(1) + 0.1
            IF (IX.EQ.12) THEN
               TDATA(4) = TDATA(4) * 24.0
               TDATA(5) = TDATA(5) * 24.0
               END IF
            WRITE (MSGTXT,1281) TIME, TVPLOT(IX), TDATA(4), TDATA(5)
            CALL MSGWRT (5)
            IX = TDATA(2) + 0.01
            IF (IX.EQ.12) THEN
               TDATA(6) = TDATA(6) * 24.0
               TDATA(7) = TDATA(7) * 24.0
               END IF
            IF (TDATA(6).GE.TDATA(7)) THEN
               WRITE (MSGTXT,1283) TVPLOT(IX)
            ELSE
               WRITE (MSGTXT,1282) TVPLOT(IX), TDATA(6), TDATA(7)
               END IF
            CALL MSGWRT (5)
C                                       time range
            ICTR = IS + 216 - COFF1
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=411) TS1, IT1, TS2, IT2
               END IF
 411        CALL MSGWRT (5)
C                                       UV range
            ICTR = IS + 214 - COFF1
            IF ((DATA(ICTR).GT.0.0) .OR. ((DATA(ICTR).LT.DATA(ICTR+1))
     *         .AND. (DATA(ICTR+1).LE.1.0E9))) THEN
               WRITE (MSGTXT,1285) DATA(ICTR), DATA(ICTR+1)
               CALL MSGWRT (5)
               END IF
C                                       channels range
            ITRC(1) = DATA(IS+207) + 0.1
            ITRC(2) = DATA(IS+208) + 0.1
            IF (CVER.LE.3) THEN
               IBLC(1) = DATA(IS+210) + 0.1
               IBLC(2) = DATA(IS+211) + 0.1
            ELSE
               IBLC(1) = DATA(IS+209) + 0.1
               IBLC(2) = DATA(IS+210) + 0.1
               END IF
            WRITE (MSGTXT,1411) ITRC, IBLC
            CALL MSGWRT (5)
C                                       VPLOT lists pairs for each plot
            ICTR = IS + 224 - COFF1
            NANT1 = 0
            DO 412 I = 1,50
               IX = IROUND (DATA(ICTR+I))
               IY = IROUND (DATA(ICTR+I+50))
               IF ((IX.GT.0) .AND. (IY.GT.0)) THEN
                  NANT1 = NANT1 + 1
                  IANT1(NANT1) = IX
                  IANT2(NANT1) = IY
                  END IF
 412           CONTINUE
            DO 413 I = 1,NANT1,7
               IX = MIN (I+6, NANT1)
               WRITE (MSGTXT,1412) (IANT1(J), IANT2(J), J = I,IX)
               IF (I.EQ.1) MSGTXT(13:17) = 'pairs'
               CALL MSGWRT (5)
 413           CONTINUE
C                                       coloring
            IF (CVER.EQ.4) THEN
               IF (DATA(IS+211).GT.0.0) THEN
                  MSGTXT = ' '
                  MSGTXT(13:) = 'IFs averaged together'
                  CALL MSGWRT (5)
               ELSE IF (DATA(IS+212).GT.0.0) THEN
                  MSGTXT = ' '
                  MSGTXT(13:) = 'IFs separated by color'
                  CALL MSGWRT (5)
                  END IF
            ELSE IF (CVER.LE.3) THEN
               IF (DATA(IS+212).GT.0.0) THEN
                  MSGTXT = ' '
                  MSGTXT(13:) = 'IFs averaged together'
                  CALL MSGWRT (5)
               ELSE IF (DATA(IS+214).GT.0.0) THEN
                  MSGTXT = ' '
                  MSGTXT(13:) = 'IFs separated by color'
                  CALL MSGWRT (5)
                  END IF
               END IF
C                                       VPLOT model stuff
            IF (DATA(IS+16).GT.0.0) THEN
               CALL H2CHR (12, 1, DATA(IS+8), NAME)
               CALL H2CHR (6, 1, DATA(IS+11), CLASS)
               I = DATA(IS+13) + 0.5
               J = DATA(IS+14) + 0.5
               WRITE (MSGTXT,1415) NAME, CLASS, I, J
               CALL MSGWRT (5)
               I = DATA(IS+15) + 0.5
               J = DATA(IS+16) + 0.5
               WRITE (MSGTXT,1416) I, J
               CALL MSGWRT (5)
               END IF
C                                       XYRATIO
            IF (CVER.LE.2) THEN
               T1 = DATA(IS+402)
            ELSE
               T1 = DATA(IS+401)
               END IF
            IF ((CVER.LE.3) .AND. (T1.NE.1.0) .AND. (T1.NE.0.0)) THEN
               WRITE (MSGTXT,1367) T1
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       CLPLT
C                                       CAPLT
C                                       CVER=1 adds baddisk ar end
C                                       CVER=2,3 differ only by PDVER
 420     CONTINUE
            COFF1 = 0
            IF (CVER.GT.3) COFF1 = 1
C                                       BCHAN
            IF ((DATA(IS+207).LT.-100.) .OR. (DATA(IS+207).GT.39999.))
     *         GO TO 880
C                                       XINC
            IF ((DATA(IS+211).LT.-100.) .OR. (DATA(IS+211).GT.9999.))
     *         GO TO 880
            ITRC(1) = DATA(IS+211) + 0.1
C                                       POLPLOT
            CALL H2CHR (8, 1, HDATA(IS+299-COFF1), ATEMP)
C                                       SOLINT
            T1 = DATA(IS+286-COFF1)
            WRITE (MSGTXT,1410) IVER, 'CL', DATE, ATEMP, ITRC(1), T1
            IF (IPTYPE.EQ.35) MSGTXT(5:6) = 'CA'
C                                       DOCALIB
            ICTR = IS + 273
            IF (DATA(ICTR).GT.0.0) THEN
               MSGTXT(23:33) = 'Cal applied'
            ELSE
               MSGTXT(23:33) = 'No cal done'
               END IF
            CALL MSGWRT (5)
C                                       Axes: type, range
            CALL RCOPY (10, DATA(IS+289-COFF1), TDATA)
            IF ((TDATA(1).LT.-10.) .OR. (TDATA(1).GT.99.)) GO TO 880
            IF ((TDATA(2).LT.-10.) .OR. (TDATA(2).GT.99.)) GO TO 880
            IX = TDATA(1) + 0.1
            IF (IX.NE.2) IX = 1
            WRITE (MSGTXT,1281) TIME, TCLPLT(IX), TDATA(4), TDATA(5)
            CALL MSGWRT (5)
            IX = 3
            IF (IPTYPE.EQ.35) IX = 4
            IF (TDATA(6).GE.TDATA(7)) THEN
               WRITE (MSGTXT,1283) TCLPLT(IX)
            ELSE
               WRITE (MSGTXT,1282) TCLPLT(IX), TDATA(6), TDATA(7)
               END IF
            CALL MSGWRT (5)
C                                       time range
            ICTR = IS + 213
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=421) TS1, IT1, TS2, IT2
               END IF
 421        CALL MSGWRT (5)
C                                       UV range
            ICTR = IS + 212
            IF ((DATA(ICTR).GT.0.0) .OR. ((DATA(ICTR).LT.DATA(ICTR+1))
     *         .AND. (DATA(ICTR+1).LT.1.0E9))) THEN
               WRITE (MSGTXT,1285) DATA(ICTR), DATA(ICTR+1)
               CALL MSGWRT (5)
               END IF
C                                       channels range
            ITRC(1) = DATA(IS+207) + 0.1
            ITRC(2) = DATA(IS+208) + 0.1
            IBLC(1) = DATA(IS+209) + 0.1
            IBLC(2) = DATA(IS+210) + 0.1
            WRITE (MSGTXT,1411) ITRC, IBLC
            CALL MSGWRT (5)
C                                       TRIANGLE
            ICTR = IS + 222
            NANT1 = 0
            DO 424 I = 1,50
               IX = IROUND (DATA(ICTR+I))
               IF (IX.GT.0) THEN
                  NANT1 = NANT1 + 1
                  IANT1(NANT1) = IX
                  END IF
 424           CONTINUE
            DO 425 I = 1,NANT1,10
               IX = MIN (I+9, NANT1)
               WRITE (MSGTXT,1424) (IANT1(J), J = I,IX)
               IF (I.EQ.1) THEN
                  MSGTXT(13:20) = 'triangle'
                  IF (IPTYPE.EQ.35) MSGTXT(13:20) = 'quadrngl'
                  END IF
               CALL MSGWRT (5)
 425           CONTINUE
C                                       model stuff
            IF (DATA(IS+16).GT.0.0) THEN
               CALL H2CHR (12, 1, DATA(IS+8), NAME)
               CALL H2CHR (6, 1, DATA(IS+11), CLASS)
               I = DATA(IS+13) + 0.5
               J = DATA(IS+14) + 0.5
               WRITE (MSGTXT,1415) NAME, CLASS, I, J
               CALL MSGWRT (5)
               I = DATA(IS+15) + 0.5
               J = DATA(IS+16) + 0.5
               WRITE (MSGTXT,1416) I, J
               CALL MSGWRT (5)
               END IF
C                                       XYRATIO
            T1 = DATA(IS+306-COFF1)
            IF ((CVER.LE.3) .AND. (T1.NE.1.0) .AND. (T1.NE.0.0)) THEN
               WRITE (MSGTXT,1367) T1
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       DFTPL
C                                       CVER=1 with Stokes, baddisk
C                                       CVER=2 with cal adverbs
 430     CONTINUE
            COFF1 = 0
            IF (CVER.EQ.1) COFF1 = 1
            IF (CVER.EQ.3) THEN
               NS = DATA(IS+25) + 0.01
               IF ((NS.LT.1) .OR. (NS.GT.8)) NS = 13
               WRITE (MSGTXT,1430) IVER, DATE, CHCORR(NS), DATA(IS+30),
     *            DATA(IS+31)
               CALL MSGWRT (5)
               CALL T2DHMS (DATA(IS+10), TS1, IT1)
               CALL T2DHMS (DATA(IS+14), TS2, IT2)
               WRITE (MSGTXT,1431,ERR=431) TIME, TS1, IT1, TS2, IT2
 431           CALL MSGWRT (5)
C                                       UV range
               IF (DATA(IS+9).LT.1.E10) THEN
                  DATA(IS+8) = DATA(IS+8) / 1.E3
                  DATA(IS+9) = DATA(IS+9) / 1.E3
                  WRITE (MSGTXT,1285) DATA(IS+8), DATA(IS+9)
                  END IF
C                                       shift
               IF ((DATA(IS+18).NE.0.) .OR. (DATA(IS+19).NE.0.0)) THEN
                  WRITE (MSGTXT,1432) DATA(IS+18), DATA(IS+19)
                  CALL MSGWRT (5)
                  END IF
            ELSE IF (CVER.LE.2) THEN
C                                       Stokes changed
               IF (CVER.EQ.2) THEN
                  NS = DATA(IS+COFF1+20) + 0.01
                  IF ((NS.LT.1) .OR. (NS.GT.12)) NS = 13
                  WRITE (MSGTXT,1430) IVER, DATE, CHCORR(NS),
     *               DATA(IS+COFF1+25), DATA(IS+COFF1+26)
               ELSE
                  CALL H2CHR (4, 1, DATA(IS+20), DTEMP)
                  WRITE (MSGTXT,1430) IVER, DATE, DTEMP,
     *               DATA(IS+COFF1+25), DATA(IS+COFF1+26)
                  END IF
               CALL MSGWRT (5)
               CALL T2DHMS (DATA(IS+10), TS1, IT1)
               CALL T2DHMS (DATA(IS+14), TS2, IT2)
               WRITE (MSGTXT,1431,ERR=432) TIME, TS1, IT1, TS2, IT2
 432           CALL MSGWRT (5)
C                                       UV range
               IF (DATA(IS+9).LT.1.E10) THEN
                  DATA(IS+8) = DATA(IS+8) / 1.E3
                  DATA(IS+9) = DATA(IS+9) / 1.E3
                  WRITE (MSGTXT,1285) DATA(IS+8), DATA(IS+9)
                  CALL MSGWRT (5)
                  END IF
C                                       shift
               IF ((DATA(IS+18).NE.0.) .OR. (DATA(IS+19).NE.0.0)) THEN
                  WRITE (MSGTXT,1432) DATA(IS+18), DATA(IS+19)
                  CALL MSGWRT (5)
                  END IF
C                                       subarray, freqid, flagver
               IX = IROUND (DATA(IS+COFF1+155))
               IY = IROUND (DATA(IS+COFF1+154))
               NS = IROUND (DATA(IS+COFF1+165))
               WRITE (MSGTXT,1433) IX, IY, NS
               CALL MSGWRT (5)
C                                       chans, IFs, cal
               IX = IROUND (DATA(IS+COFF1+158))
               IY = IROUND (DATA(IS+COFF1+159))
               J1 = IROUND (DATA(IS+COFF1+156))
               J2 = IROUND (DATA(IS+COFF1+157))
               WRITE (MSGTXT,1434) J1, J2, IX, IY
               IF (DATA(IS+COFF1+160).GT.0.0) THEN
                  MSGTXT(50:) = 'Cal applied'
               ELSE
                  MSGTXT(50:) = 'No cal done'
                  END IF
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       FRPLT
C                                       CVER 1 doacor, pdver, factor
C                                       CVER=2,3 differ after 251
C                                       FRPLT change of 2003-08-04
 440     CONTINUE
            COFF1 = 0
            IF (CVER.GT.1) COFF1 = 2
            IF (CVER.EQ.3) COFF1 = COFF1 + 1
            IF ((DATA(IS+144).LT.-100.) .OR. (DATA(IS+144).GT.9999.))
     *         GO TO 880
            IF ((DATA(IS+146).LT.-100.) .OR. (DATA(IS+146).GT.9999.))
     *         GO TO 880
            CALL H2CHR (4, 1, DATA(IS+143), ATEMP)
            WRITE (MSGTXT,1440) IVER, DATE, ATEMP
            IF (DATA(IS+269-COFF1).LE.0.5) THEN
               MSGTXT(53:) = 'fringe rate'
            ELSE
               MSGTXT(53:) = 'vis vs time'
               END IF
            T1 = DATA(IS+249)
            IF (CVER.EQ.1) T1 = DATA(IS+250)
            IF (T1.GT.0.0) THEN
               MSGTXT(23:33) = 'Cal applied'
            ELSE
               MSGTXT(23:33) = 'No cal done'
               END IF
            CALL MSGWRT (5)
C                                       plot ranges
            WRITE (MSGTXT,1441) TIME, DATA(IS+265-COFF1),
     *         DATA(IS+266-COFF1)
            CALL MSGWRT (5)
            WRITE (MSGTXT,1442) DATA(IS+267-COFF1), DATA(IS+268-COFF1),
     *         DATA(IS+263-COFF1)
            CALL MSGWRT (5)
C                                       channels range
            ITRC(1) = DATA(IS+146) + 0.1
            ITRC(2) = DATA(IS+147) + 0.1
            IBLC(1) = DATA(IS+144) + 0.1
            IBLC(2) = DATA(IS+145) + 0.1
            WRITE (MSGTXT,1411) ITRC, IBLC
            CALL MSGWRT (5)
C                                       time range
            ICTR = IS + 134
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE IF ((T1.NE.0) .OR. (T2.NE.0)) THEN
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=441) TS1, IT1, TS2, IT2
               END IF
 441        CALL MSGWRT (5)
C                                       UV range
            ICTR = IS + 133
            IF ((DATA(ICTR).GT.0.0) .OR. ((DATA(ICTR).LT.DATA(ICTR+1))
     *         .AND. (DATA(ICTR+1).LE.1.0E9))) THEN
               WRITE (MSGTXT,1285) DATA(ICTR), DATA(ICTR+1)
               CALL MSGWRT (5)
               END IF
C                                       Divide channel 0
            IF (DATA(IS+278-COFF1).GT.0.0) THEN
               IX = DATA(IS+279-COFF1) + 0.01
               IY = DATA(IS+280-COFF1) + 0.01
               WRITE (MSGTXT,1443) IX, IY
               CALL MSGWRT (5)
               END IF
C                                       shift
            IF ((DATA(IS+261-COFF1).NE.0.0) .OR.
     *         (DATA(IS+262-COFF1).NE.0.0)) THEN
               WRITE (MSGTXT,1444) DATA(IS+261-COFF1),
     *            DATA(IS+262-COFF1)
               CALL MSGWRT (5)
               END IF
C                                       antennas
            ICTR = IS + 148
            NANT1 = 0
            DO 442 I = 1,50
               IX = IROUND (DATA(ICTR+I))
               IF (IX.GT.0) THEN
                  NANT1 = NANT1 + 1
                  IANT1(NANT1) = IX
                  END IF
 442           CONTINUE
            DO 443 I = 1,NANT1,10
               IX = MIN (I+9, NANT1)
               WRITE (MSGTXT,1424) (IANT1(J), J = I,IX)
               IF (I.EQ.1) MSGTXT(13:20) = 'antennas'
               CALL MSGWRT (5)
 443           CONTINUE
            GO TO 890
C                                       FRMAP
C                                       CVER=1 changes things at 252, 257
C                                       CVER=2,3 differ after 251
C                                       FRMAP change of 2003-08-04
 450     CONTINUE
            COFF1 = 0
            IF (CVER.GT.1) COFF1 = 4
            IF (CVER.EQ.3) COFF1 = COFF1 + 1
            IF ((DATA(IS+144).LT.-100.) .OR. (DATA(IS+144).GT.9999.))
     *         GO TO 880
            IF ((DATA(IS+146).LT.-100.) .OR. (DATA(IS+146).GT.9999.))
     *         GO TO 880
            CALL H2CHR (4, 1, DATA(IS+143), ATEMP)
            WRITE (MSGTXT,1450) IVER, DATE, ATEMP
            IF (DATA(IS+269-COFF1).LE.0.5) THEN
               MSGTXT(51:) = 'solutions fnd'
            ELSE
               MSGTXT(51:) = 'sets of lines'
               END IF
            IF (DATA(IS+249).GT.0.0) THEN
               MSGTXT(23:33) = 'Cal applied'
            ELSE
               MSGTXT(23:33) = 'No cal done'
               END IF
            CALL MSGWRT (5)
C                                       channels range
            ITRC(1) = DATA(IS+146) + 0.1
            ITRC(2) = DATA(IS+147) + 0.1
            IBLC(1) = DATA(IS+144) + 0.1
            IBLC(2) = DATA(IS+145) + 0.1
            WRITE (MSGTXT,1411) ITRC, IBLC
            CALL MSGWRT (5)
C                                       time range
            ICTR = IS + 134
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE IF ((T1.NE.0) .OR. (T2.NE.0)) THEN
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=451) TS1, IT1, TS2, IT2
               END IF
 451        CALL MSGWRT (5)
C                                       UV range
            ICTR = IS + 133
            IF ((DATA(ICTR).GT.0.0) .OR. ((DATA(ICTR).LT.DATA(ICTR+1))
     *         .AND. (DATA(ICTR+1).LE.1.0E9))) THEN
               WRITE (MSGTXT,1285) DATA(ICTR), DATA(ICTR+1)
               CALL MSGWRT (5)
               END IF
C                                       image parms
            T1 = DATA(IS+265-COFF1) * 2.0
            WRITE (MSGTXT,1451) 'X', T1, DATA(IS+263-COFF1),
     *         DATA(IS+267-COFF1)
            CALL MSGWRT (5)
            T1 = DATA(IS+266-COFF1) * 2.0
            WRITE (MSGTXT,1451) 'Y', T1, DATA(IS+264-COFF1),
     *         DATA(IS+268-COFF1)
            CALL MSGWRT (5)
C                                       line limits
            WRITE (MSGTXT,1452) DATA(IS+271-COFF1), DATA(IS+272-COFF1)
            CALL MSGWRT (5)
            IX = DATA(IS+276-COFF1) + 0.1
            WRITE (MSGTXT,1453) DATA(IS+270-COFF1), DATA(IS+273-COFF1),
     *         IX
            CALL MSGWRT (5)
            IX = DATA(IS+274-COFF1) + 0.1
            WRITE (MSGTXT,1454) IX, DATA(IS+275-COFF1),
     *         DATA(IS+277-COFF1)
            CALL MSGWRT (5)
            GO TO 890
C                                       PLOTR
C                                       CVER 1 BPARM->RPARM etc
C                                       CVER 2 adds DPARM
C                                       CVER 3
C                                       CVER 4
 460     CONTINUE
            COFF1 = -3
            IF (CVER.LE.3) COFF1 = 0
            RANGE1 = DATA(IS+20)
            RANGE2 = DATA(IS+21)
            WRITE (MSGTXT,1460) IVER, LTASK, DATE, RANGE1, RANGE2
            CALL MSGWRT (5)
            RANGE1 = DATA(IS+22)
            RANGE2 = DATA(IS+23)
            WRITE (MSGTXT,1461) TIME, RANGE1, RANGE2
            CALL MSGWRT (5)
C                                       numbers plotted
            NP = 10
            IF (CVER.LE.1) NP = 30
            JJ = 0
            DO 461 I = 1,NP
               J1 = IROUND (DATA(IS+29+I))
               IF (J1.GT.COFF1) THEN
                  JJ = JJ + 1
                  IANT1(JJ) = I
                  IANT2(JJ) = J1
                  END IF
 461           CONTINUE
            J1 = 1
 462        J2 = MIN (JJ, J1+4)
            IF (J2.GE.J1) THEN
               WRITE (MSGTXT,1462) (IANT1(J), IANT2(J), J = J1,J2)
               CALL MSGWRT (5)
               GO TO 462
               END IF
C                                       lines plotted
            IF (CVER.LE.3) THEN
               JJ = 0
               IP = IS + 39
               IF (CVER.LE.1) IP = IS + 59
               DO 463 I = 1,NP
                  J1 = IROUND (DATA(IP+I))
                  IF (J1.GT.0) THEN
                     JJ = JJ + 1
                     IANT1(JJ) = I
                     IANT2(JJ) = J1
                     END IF
 463              CONTINUE
               J1 = 1
 464           J2 = MIN (JJ, J1+4)
               IF (J2.GE.J1) THEN
                  WRITE (MSGTXT,1463) (IANT1(J), IANT2(J), J = J1,J2)
                  CALL MSGWRT (5)
                  GO TO 464
                  END IF
               END IF
C                                       order of fit
            IF (CVER.LE.2) THEN
               JJ = 0
               IP = IS + 49
               IF (CVER.LE.1) IP = IS + 89
               DO 465 I = 1,NP
                  J1 = IROUND (DATA(IP+I))
                  IF (J1.GT.0) THEN
                     JJ = JJ + 1
                     IANT1(JJ) = I
                     IANT2(JJ) = J1
                     END IF
 465              CONTINUE
               J1 = 1
 466           J2 = MIN (JJ, J1+4)
               IF (J2.GE.J1) THEN
                  WRITE (MSGTXT,1464) (IANT1(J), IANT2(J), J = J1,J2)
                  CALL MSGWRT (5)
                  GO TO 466
                  END IF
               END IF
C                                       math functions
            J1 = IROUND (DATA(IS+26)) + 1
            J2 = IROUND (DATA(IS+29)) + 1
            IF ((J1.GT.1) .OR. (J2.GT.1)) THEN
               FT = J1
               FP(1) = DATA(IS+24)
               FP(2) = DATA(IS+25)
               NP = 13
               MSGTXT = ' '
               DO 467 J = 1,2
                  IF ((FT.NE.1) .OR. (FP(1).NE.1.0) .OR. (FP(2).NE.0.0))
     *               THEN
                     MSGTXT(NP:) = 'Xplot = '
                     IF (J.EQ.2) MSGTXT(NP:NP) = 'Y'
                     NP = NP + 8
                     IF (FT.EQ.2) THEN
                        MSGTXT(NP:) = 'LOG'
                        NP = NP + 4
                     ELSE IF (FT.EQ.3) THEN
                        MSGTXT(NP:) = 'LOG10'
                        NP = NP + 6
                     ELSE IF (FT.EQ.4) THEN
                        MSGTXT(NP:) = 'EXP'
                        NP = NP + 4
                     ELSE IF (FT.EQ.7) THEN
                        MSGTXT(NP:) = 'SQRT'
                        NP = NP + 5
                     ELSE IF (FT.EQ.9) THEN
                        MSGTXT(NP:) = '1 /'
                        NP = NP + 4
                     ELSE IF (FT.EQ.10) THEN
                        MSGTXT(NP:) = '1 / SQRT'
                        NP = NP + 9
                        END IF
                     MSGTXT(NP:) = '['
                     NP = NP + 1
                     IF (FP(1).NE.1.0) THEN
                        IF (ABS(FP(1)).GT.1000.) THEN
                           WRITE (MSGTXT(NP:),1465) FP(1)
                        ELSE IF (ABS(FP(1)).GT.10.) THEN
                           WRITE (MSGTXT(NP:),1466) FP(1)
                        ELSE
                           WRITE (MSGTXT(NP:),1467) FP(1)
                           END IF
                        NP = NP + 8
                        MSGTXT(NP:) = '*'
                        NP = NP + 2
                        END IF
                     MSGTXT(NP:) = 'X'
                     IF (J.EQ.2) MSGTXT(NP:) = 'Y'
                     NP = NP + 1
                     IF (FP(2).NE.0.0) THEN
                        IF (FP(2).LT.0) THEN
                           MSGTXT(NP:) = ' -'
                        ELSE
                           MSGTXT(NP:) = ' +'
                           END IF
                        NP = NP + 2
                        X = ABS (FP(2))
                        IF (X.GT.1000.) THEN
                           WRITE (MSGTXT(NP:),1465) X
                        ELSE IF (X.GT.10.) THEN
                           WRITE (MSGTXT(NP:),1466) X
                        ELSE
                           WRITE (MSGTXT(NP:),1467) X
                           END IF
                        NP = NP + 7
                        END IF
                     MSGTXT(NP:) = ']'
                     NP = NP + 2
                     IF (FT.EQ.5) THEN
                        MSGTXT(NP:) = '** 2'
                     ELSE IF (FT.EQ.6) THEN
                        MSGTXT(NP:) = '** 3'
                     ELSE IF (FT.EQ.8) THEN
                        MSGTXT(NP:) = '** 1/3'
                        END IF
                     CALL MSGWRT (5)
                     MSGTXT = ' '
                     NP = 13
                     END IF
                  FT = J2
                  FP(1) = DATA(IS+27)
                  FP(2) = DATA(IS+28)
 467              CONTINUE
               END IF
            GO TO 890
C                                       Grey scale - GREYS
C                                       version after 11/20/02
C                                       CVER=1 after 1-Nov-2004
C                                          differs after 95
C                                       CVER=2 after 5-Oct-2003
C                                       CVER=3 no display diff
 470     CONTINUE
            COFF1 = 0
            IF (CVER.EQ.2) COFF1 = 1
            IF (CVER.EQ.3) COFF1 = 1
            ISX = IS + 30
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            RANGE1 = DATA(IS+45)
            RANGE2 = DATA(IS+46)
            CALL H2CHR (2, 1, DATA(IS+47), FUNC)
            WRITE (MSGTXT,1230) IVER, DATE, IBLC, RANGE1
            CALL MSGWRT (5)
            WRITE (MSGTXT,1271) TIME, ITRC, RANGE2, FUNC
            CALL MSGWRT (5)
C                                       True color build
            IF (DATA(IS+1).GT.0.0) THEN
               CALL H2CHR (12, 1, DATA(IS+16), NAME)
               CALL H2CHR (6, 1, DATA(IS+19), CLASS)
               I = DATA(IS+21)
               J = DATA(IS+22)
               WRITE (MSGTXT,1232) 'Green image:       ', NAME, CLASS,
     *            I, J
               CALL MSGWRT (5)
               CALL H2CHR (12, 1, DATA(IS+23), NAME)
               CALL H2CHR (6, 1, DATA(IS+26), CLASS)
               I = DATA(IS+28)
               J = DATA(IS+29)
               WRITE (MSGTXT,1232) 'Blue image:        ', NAME, CLASS,
     *            I, J
               CALL MSGWRT (5)
               END IF
C                                       Grey scale file contains
C                                       contour commands
            IF (DATA(IS).GT.0.0) THEN
               CALL H2CHR (12, 1, DATA(IS+9), NAME)
               CALL H2CHR (6, 1, DATA(IS+12), CLASS)
               I = DATA(IS+14)
               J = DATA(IS+15)
               WRITE (MSGTXT,1232) 'Contour image:     ', NAME, CLASS,
     *            I, J
               CALL MSGWRT (5)
               XCLEV = DATA(IS+61)
               WRITE (MSGTXT,1233) XCLEV
C                                       Do levels.
               ICTR = 33
               DO 475 I = 1,30
                  XLEVS = DATA(61+IS+I)
                  IF (XLEVS.GT.999.90) THEN
                     WRITE (CTEMP,1222,ERR=474) XLEVS
                  ELSE IF (XLEVS.GE.-99.90) THEN
                     WRITE (CTEMP,1223,ERR=474) XLEVS
                  ELSE IF (XLEVS.GE.-9.990) THEN
                     WRITE (CTEMP,1224,ERR=474) XLEVS
                  ELSE IF (XLEVS.GE.-0.9990) THEN
                     WRITE (CTEMP,1225,ERR=474) XLEVS
                  ELSE
                     WRITE (CTEMP,1222,ERR=474) XLEVS
                     END IF
 474              IF (XLEVS.GE.DATA(IS+62+I)) GO TO 476
                     MSGTXT(ICTR:ICTR+6) = CTEMP(1:7)
                     ICTR = ICTR + 7
C                                       Last lev on line.
                     IF (ICTR.GT.58) THEN
                        CALL MSGWRT (5)
                        MSGTXT = ' '
                        ICTR = 12
                        END IF
 475              CONTINUE
 476           MSGTXT(ICTR:) = CTEMP(1:6) // ')'
               CALL MSGWRT (5)
               END IF
            STVERS = DATA(IS+94) + 0.01
            STFACT = DATA(IS+95)
            IF ((STVERS.GT.0) .AND. (STFACT.GT.0.0)) THEN
               WRITE (MSGTXT,1228) STVERS, STFACT
               CALL MSGWRT (5)
               END IF
            I = DATA(IS+97-COFF1) + 0.01
            J = DATA(IS+98-COFF1) + 0.01
            IF ((I.GT.0) .AND. (I.LE.J)) THEN
               WRITE (MSGTXT,1236) I, J
               CALL MSGWRT (5)
               END IF
            II = DATA(IS+99-COFF1)
            IF (II.GT.0) THEN
               ISX = IS + 99 - COFF1
               DO 478 I = 1,II
                  IBLC(1) = DATA(ISX+1) + .5
                  IBLC(2) = DATA(ISX+2) + .5
                  ITRC(1) = DATA(ISX+3) + .5
                  ITRC(2) = DATA(ISX+4) + .5
                  ISX = ISX + 4
                  WRITE (MSGTXT,1237) I, IBLC, ITRC
                  CALL MSGWRT (5)
 478              CONTINUE
               END IF
            CALL H2CHR (48, 1, DATA(IS+307-COFF1), FILNAM)
            IF (FILNAM.NE.' ') THEN
               WRITE (MSGTXT,1478) FILNAM
               I = JTRIM (MSGTXT)
               MSGTXT(I+1:) = ''''
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       Polarization vector plot -
C                                       PCNTR after 11/2002
C                                       CVER=1 add DOFRACT after ICUT
C                                       CVER=2 add RGBLEVS 2006-04-23
C                                       CVER=3 changes at 46&85 from
C                                       CVER=4 on 2003-07-08
 480     CONTINUE
            IF (CVER.EQ.4) THEN
               COFF2 = 2
               COFF1 = 1
            ELSE
               COFF2 = 0
               COFF1 = 0
               END IF
            COFF3 = 0
            IF (CVER.EQ.1) COFF3 = 1
            ISX = IS + 32
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            XPLEV = DATA(IS+49-COFF1)
            XCLEV = DATA(IS+50-COFF1)
            FAC = DATA(IS+81-COFF1)
            ROTATE = DATA(IS+82-COFF1)
            IF ((DATA(IS+83-COFF1).LT.-10.) .OR.
     *         (DATA(IS+84-COFF1).LT.-10.) .OR.
     *         (DATA(IS+83-COFF1).GT.999.) .OR.
     *         (DATA(IS+84-COFF1).GT.999.)) GO TO 880
            IX = DATA(IS+83-COFF1) + .5
            IY = DATA(IS+84-COFF1) + .5
            XPCUT = DATA(IS+85-COFF1)
            XICUT = DATA(IS+86-COFF1)
            IF (XPLEV.GT.0.0001) XCLEV = XPLEV * CATR(KRDMX) / 100.0
            IF (DATA(IS+1).GT.0.0) THEN
               WRITE (MSGTXT,1260,ERR=481) IVER, DATE, IBLC, FAC, IX, IY
 481           CALL MSGWRT (5)
               WRITE (MSGTXT,1261,ERR=482) TIME, ITRC, XICUT, XPCUT
 482           CALL MSGWRT (5)
               CALL H2CHR (12, 1, DATA(IS+11), NAME)
               CALL H2CHR (6, 1, DATA(IS+14), CLASS)
               I = DATA(IS+16)
               J = DATA(IS+17)
               IF (DATA(IS+1).LT.1.5) THEN
                  WRITE (MSGTXT,1232) 'Vector length img: ', NAME,
     *               CLASS, I, J
               ELSE IF (DATA(IS+1).LT.2.5) THEN
                  WRITE (MSGTXT,1232) 'Q Stokes image:    ', NAME,
     *               CLASS, I, J
               ELSE
                  WRITE (MSGTXT,1263)
                  END IF
               CALL MSGWRT (5)
               CALL H2CHR (12, 1, DATA(IS+18), NAME)
               CALL H2CHR (6, 1, DATA(IS+21), CLASS)
               I = DATA(IS+23)
               J = DATA(IS+24)
               IF (DATA(IS+1).LT.1.5) THEN
                  WRITE (MSGTXT,1232) 'Vector angle img:  ', NAME,
     *               CLASS, I, J
                  CALL MSGWRT (5)
               ELSE IF (DATA(IS+1).LT.2.5) THEN
                  WRITE (MSGTXT,1232) 'U Stokes image:    ', NAME,
     *               CLASS, I, J
                  CALL MSGWRT (5)
                  END IF
               IF (ROTATE.NE.0.0) THEN
                  WRITE (MSGTXT,1482) ROTATE
                  CALL MSGWRT (5)
                  END IF
               IF ((CVER.EQ.1) .AND. (DATA(IS+87).GT.0.0)) THEN
                  MSGTXT = ' '
                  MSGTXT(13:) = 'Vectors are fractional polarization'
                  CALL MSGWRT (5)
                  END IF
            ELSE
               WRITE (MSGTXT,1264) IVER, DATE, IBLC
               CALL MSGWRT (5)
               WRITE (MSGTXT,1221) TIME, ITRC
               CALL MSGWRT (5)
               END IF
            WASRGB = .FALSE.
            IF (DATA(IS).GT.0.0) THEN
               WRITE (MSGTXT,1233) XCLEV
C                                       Do levels.
               ICTR = 33
               DO 486 I = 1,30
                  XLEVS = DATA(50+IS+I-COFF1)
                  IF (XLEVS.GT.999.90) THEN
                     WRITE (CTEMP,1222,ERR=485) XLEVS
                  ELSE IF (XLEVS.GE.-99.90) THEN
                     WRITE (CTEMP,1223,ERR=485) XLEVS
                  ELSE IF (XLEVS.GE.-9.990) THEN
                     WRITE (CTEMP,1224,ERR=485) XLEVS
                  ELSE IF (XLEVS.GE.-0.9990) THEN
                     WRITE (CTEMP,1225,ERR=485) XLEVS
                  ELSE
                     WRITE (CTEMP,1222,ERR=485) XLEVS
                     END IF
C                                       check RGBLEVS
                  IF (CVER.EQ.1) THEN
                     JJ = IS + 112 + 3*I + COFF3
                     IF ((DATA(JJ).GT.0.0) .AND. (DATA(JJ).LE.1.0))
     *                  WASRGB = .TRUE.
                     IF ((DATA(JJ+1).GT.0.0) .AND. (DATA(JJ+1).LE.1.0))
     *                  WASRGB = .TRUE.
                     IF ((DATA(JJ+2).GT.0.0) .AND. (DATA(JJ+2).LE.1.0))
     *                  WASRGB = .TRUE.
                     END IF
 485              IF (XLEVS.GE.DATA(IS+51+I-COFF1)) GO TO 488
                     MSGTXT(ICTR:ICTR+6) = CTEMP(1:7)
                     ICTR = ICTR + 7
C                                       Last lev on line.
                     IF (ICTR.GT.58) THEN
                        CALL MSGWRT (5)
                        MSGTXT = ' '
                        ICTR = 12
                        END IF
 486              CONTINUE
 488           MSGTXT(ICTR:) = CTEMP(1:6) // ')'
               CALL MSGWRT (5)
               IF (WASRGB) THEN
                  MSGTXT = ' '
                  MSGTXT (13:) =
     *               'Contours drawn in user-selected colors'
                  CALL MSGWRT (5)
                  END IF
               END IF
C                                       grey scale part
            IF (DATA(IS+2).GT.0.0) THEN
               CALL H2CHR (12, 1, DATA(IS+25), NAME)
               CALL H2CHR (6, 1, DATA(IS+28), CLASS)
               I = DATA(IS+30)
               J = DATA(IS+31)
               IF (DATA(IS+106-COFF2+COFF3).GT.0.0) THEN
                  WRITE (MSGTXT,1232) 'RGB cube image :   ', NAME,
     *               CLASS, I, J
               ELSE
                  WRITE (MSGTXT,1232) 'Grey scale image : ', NAME,
     *               CLASS, I, J
                  END IF
               CALL MSGWRT (5)
               RANGE1 = DATA(IS+90-COFF2+COFF3)
               RANGE2 = DATA(IS+91-COFF2+COFF3)
               CALL H2CHR (2, 1, DATA(IS+92-COFF2+COFF3), FUNC)
               WRITE (MSGTXT,1383) RANGE1, RANGE2, FUNC
               CALL MSGWRT (5)
               CALL H2CHR (48, 1, DATA(IS+94-COFF2+COFF3), FILNAM)
               IF (FILNAM.NE.' ') THEN
                  WRITE (MSGTXT,1478) FILNAM
                  I = JTRIM (MSGTXT)
                  MSGTXT(I+1:) = ''''
                  CALL MSGWRT (5)
                  END IF
               END IF
            STVERS = DATA(IS+107-COFF2+COFF3) + 0.01
            STFACT = DATA(IS+108-COFF2+COFF3)
            IF ((STVERS.GT.0) .AND. (STFACT.GT.0.0)) THEN
               WRITE (MSGTXT,1228) STVERS, STFACT
               CALL MSGWRT (5)
               END IF
C                                       coloring
            IF (CVER.EQ.1) THEN
               IF ((DATA(IS+87+COFF3).GT.0.0) .AND. (DATA(IS+1).GT.0.0))
     *            THEN
                  MSGTXT = ' '
                  MSGTXT(13:) = 'Polarization vectors colored by their'
     *               // ' angles'
                  CALL MSGWRT (5)
                  END IF
               IF (DATA(IS+46).GT.0.0) THEN
                  MSGTXT = ' '
                  IF (DATA(IS).GT.0.0) THEN
                     MSGTXT(13:) = 'Contours colored by their planes'
                  ELSE
                     MSGTXT(13:) = 'Polarization vectors colored by'
     *               // ' their planes'
                     END IF
                  CALL MSGWRT (5)
                  END IF
               END IF
            GO TO 890
C                                       KNTR - 11/27/02 version
C                                       CVER=1 add INFILE
C                                       CVER=2 add RGBLEVS 2006-04-23
C                                       CVER=3 changes at 98&104 from
C                                       CVER=4 on 2003-07-08
 490     CONTINUE
            IF (CVER.EQ.4) THEN
               COFF2 = 2
               COFF1 = 1
            ELSE
               COFF2 = 0
               COFF1 = 0
               END IF
            ISX = IS + 31
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            XCLEV = DATA(IS+67)
            BTEMP = ' K'
            WRITE (MSGTXT,1490) IVER, BTEMP, DATE, IBLC
            IF (DATA(IS+0).LE.0.0) THEN
               MSGTXT(36:) = 'No contours drawn'
            ELSE IF (DATA(IS+1).LE.0.0) THEN
               MSGTXT(36:) = 'No grey-scale images drawn'
            ELSE IF (DATA(IS+2).LE.0.0) THEN
               MSGTXT(36:) = 'No polarization vectors drawn'
               END IF
            CALL MSGWRT (5)
            WRITE (MSGTXT,1221) TIME, ITRC
            IF (DATA(IS+0).LE.0.0) THEN
               IF (DATA(IS+1).LE.0.0) THEN
                  MSGTXT(36:) = 'No grey-scale images drawn'
               ELSE IF (DATA(IS+2).LE.0.0) THEN
                  MSGTXT(36:) = 'No polarization vectors drawn'
                  END IF
            ELSE IF (DATA(IS+1).LE.0.0) THEN
               IF (DATA(IS+2).LE.0.0) MSGTXT(36:) =
     *           'No polarization vectors drawn'
               END IF
            CALL MSGWRT (5)
C                                       Do levels.
            WASRGB = .FALSE.
            IF (DATA(IS).GT.0.0) THEN
               WRITE (MSGTXT,1233) XCLEV
               ICTR = 33
               DO 492 I = 1,30
                  XLEVS = DATA(67+IS+I)
                  IF (XLEVS.GT.999.90) THEN
                     WRITE (CTEMP,1222,ERR=491) XLEVS
                  ELSE IF (XLEVS.GE.-99.90) THEN
                     WRITE (CTEMP,1223,ERR=491) XLEVS
                  ELSE IF (XLEVS.GE.-9.990) THEN
                     WRITE (CTEMP,1224,ERR=491) XLEVS
                  ELSE IF (XLEVS.GE.-0.9990) THEN
                     WRITE (CTEMP,1225,ERR=491) XLEVS
                  ELSE
                     WRITE (CTEMP,1222,ERR=491) XLEVS
                     END IF
C                                       check RGBLEVS
                  IF (CVER.LE.2) THEN
                     JJ = IS + 115 + 3*I
                     IF (CVER.EQ.1) JJ = JJ + 12
                     IF ((DATA(JJ).GT.0.0) .AND. (DATA(JJ).LE.1.0))
     *                  WASRGB = .TRUE.
                     IF ((DATA(JJ+1).GT.0.0) .AND. (DATA(JJ+1).LE.1.0))
     *                  WASRGB = .TRUE.
                     IF ((DATA(JJ+2).GT.0.0) .AND. (DATA(JJ+2).LE.1.0))
     *                  WASRGB = .TRUE.
                     END IF
 491              IF (XLEVS.GE.DATA(IS+68+I)) GO TO 493
                     MSGTXT(ICTR:ICTR+6) = CTEMP(1:7)
                     ICTR = ICTR + 7
C                                       Last lev on line.
                     IF (ICTR.GT.58) THEN
                        CALL MSGWRT (5)
                        MSGTXT = ' '
                        ICTR = 12
                        END IF
 492              CONTINUE
 493           MSGTXT(ICTR:) = CTEMP(1:6) // ')'
               CALL MSGWRT (5)
               IF (WASRGB) THEN
                  MSGTXT = ' '
                  MSGTXT (13:) =
     *               'Contours drawn in user-selected colors'
                  CALL MSGWRT (5)
                  END IF
               END IF
C                                       KNTR panels
            IBLC(1) = IROUND (DATA(IS+33))
            ITRC(1) = IROUND (DATA(IS+40))
            ITRC(2) = IROUND (DATA(IS+45))
            IF ((IBLC(1).NE.1) .OR. (ITRC(1).NE.1)) THEN
               WRITE (MSGTXT,1381) IBLC(1), ITRC(1), ITRC(2)
               CALL MSGWRT (5)
               END IF
C                                       2nd image
            IF ((DATA(IS).GT.1.5) .OR. (DATA(IS+1).GT.1.5)) THEN
               CALL H2CHR (12, 1, DATA(IS+10), NAME)
               CALL H2CHR (6, 1, DATA(IS+13), CLASS)
               I = DATA(IS+15)
               J = DATA(IS+16)
               IF ((DATA(IS).GT.1.5) .AND. (DATA(IS+1).GT.1.5)) THEN
                  IF (DATA(IS+63).GT.0.0) THEN
                     WRITE (MSGTXT,1232) 'Cont & RGB image:  ', NAME,
     *                  CLASS, I, J
                  ELSE
                     WRITE (MSGTXT,1232) 'Cont & Grey image: ', NAME,
     *                  CLASS, I, J
                     END IF
               ELSE IF (DATA(IS).GT.1.5) THEN
                  WRITE (MSGTXT,1232) 'Contour image:     ', NAME,
     *               CLASS, I, J
               ELSE
                  IF (DATA(IS+63).GT.0.0) THEN
                     WRITE (MSGTXT,1232) 'RGB cube image:    ', NAME,
     *                  CLASS, I, J
                  ELSE
                     WRITE (MSGTXT,1232) 'Grey-scale image:  ', NAME,
     *                  CLASS, I, J
                     END IF
                  END IF
               CALL MSGWRT (5)
               END IF
C                                       grey-scale pixrange
            IF (DATA(IS+1).GT.0.0) THEN
               CALL H2CHR (2, 1, DATA(IS+50), FUNC)
               WRITE (MSGTXT,1383) DATA(IS+48), DATA(IS+49), FUNC
               IF (DATA(IS+63).GT.0.0) MSGTXT(13:22) = 'RGB-scaled'
               CALL MSGWRT (5)
               CALL H2CHR (48, 1, DATA(IS+51), FILNAM)
               IF (FILNAM.NE.' ') THEN
                  WRITE (MSGTXT,1478) FILNAM
                  I = JTRIM (MSGTXT)
                  MSGTXT(I+1:) = ''''
                  CALL MSGWRT (5)
                  END IF
               END IF
C                                       polarization
            IF (DATA(IS+2).GT.0.0) THEN
               RDUM = DATA(IS+99-COFF1)
               WRITE (MSGTXT,1492) RDUM
               CALL MSGWRT (5)
               IX = DATA(IS+101-COFF1) + .5
               IY = DATA(IS+102-COFF1) + .5
               XPCUT = DATA(IS+103-COFF1)
               XICUT = DATA(IS+104-COFF1)
               WRITE (MSGTXT,1493) IX, IY, XICUT, XPCUT
               CALL MSGWRT (5)
               CALL H2CHR (12, 1, DATA(IS+17), NAME)
               CALL H2CHR (6, 1, DATA(IS+20), CLASS)
               I = DATA(IS+22)
               J = DATA(IS+23)
               IF (DATA(IS+1).LT.10.0) THEN
                  WRITE (MSGTXT,1232) 'Vector length img: ', NAME,
     *               CLASS, I, J
               ELSE
                  WRITE (MSGTXT,1232) 'Q Stokes image:    ', NAME,
     *               CLASS, I, J
                  END IF
               CALL MSGWRT (5)
               CALL H2CHR (12, 1, DATA(IS+24), NAME)
               CALL H2CHR (6, 1, DATA(IS+27), CLASS)
               I = DATA(IS+29)
               J = DATA(IS+30)
               IF (DATA(IS+1).LT.10.0) THEN
                  WRITE (MSGTXT,1232) 'Vector angle img:  ', NAME,
     *               CLASS, I, J
               ELSE
                  WRITE (MSGTXT,1232) 'U Stokes image:    ', NAME,
     *               CLASS, I, J
                  END IF
               CALL MSGWRT (5)
               ROTATE = DATA(IS+100-coff1)
               IF (ROTATE.NE.0.0) THEN
                  WRITE (MSGTXT,1482) ROTATE
                  CALL MSGWRT (5)
                  END IF
               END IF
C                                       Stars
            STVERS = DATA(IS+109-COFF2) + 0.01
            STFACT = DATA(IS+110-COFF2)
            IF ((STVERS.GT.0) .AND. (STFACT.GT.0.0)) THEN
               WRITE (MSGTXT,1228) STVERS, STFACT
               CALL MSGWRT (5)
               END IF
C                                       coloring
            IF (CVER.EQ.1) THEN
               IF ((DATA(IS+105).GT.0.0) .AND. (DATA(IS+2).GT.0.0)) THEN
                  MSGTXT = ' '
                  MSGTXT(13:) = 'Polarization vectors colored by their'
     *               // ' angles'
                  CALL MSGWRT (5)
                  END IF
               IF ((DATA(IS+98).GT.0.0) .AND. (DATA(IS).GT.0.0)) THEN
                  MSGTXT = ' '
                  MSGTXT(13:) = 'Contours colored by their planes'
                  CALL MSGWRT (5)
                  END IF
               END IF
            GO TO 890
C                                       BPLOT
C                                       CVER=1 change at 8
C                                       CVER=2,3 no diff here
C                                       change of 2003-07-06
 500     CONTINUE
            COFF1 = 0
            IF (CVER.GT.1) COFF1 = 1
            CALL H2CHR (4, 1, DATA(IS+24-COFF1), DTEMP)
            CALL H2CHR (2, 1, DATA(IS+76-COFF1), BTEMP)
            IF (BTEMP.EQ.'TA') THEN
               CTEMP = 'All antennas/time'
            ELSE
               CTEMP = 'All times/antenna'
               END IF
            STVERS = DATA(IS+9-COFF1) + 0.01
            WRITE (MSGTXT,1500) IVER, DATE, CTEMP(:17), STVERS, DTEMP
            CALL MSGWRT (5)
C                                       pixrange
            CALL H2CHR (4, 1, DATA(IS+75-COFF1), DTEMP)
            RANGE1 = DATA(IS+77-COFF1)
            RANGE2 = DATA(IS+78-COFF1)
            WRITE (MSGTXT,1501) TIME, DTEMP, RANGE1, RANGE2
            CALL MSGWRT (5)
C                                       channels range
            ITRC(1) = DATA(IS+20-COFF1) + 0.1
            ITRC(2) = DATA(IS+21-COFF1) + 0.1
            IBLC(1) = DATA(IS+22-COFF1) + 0.1
            IBLC(2) = DATA(IS+23-COFF1) + 0.1
            WRITE (MSGTXT,1411) ITRC, IBLC
            CALL MSGWRT (5)
C                                       time range
            ICTR = IS + 11 - COFF1
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE IF ((T1.NE.0) .OR. (T2.NE.0)) THEN
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=501) TS1, IT1, TS2, IT2
               END IF
 501        CALL MSGWRT (5)
C                                       antennas
            ICTR = IS + 24 - COFF1
            NANT1 = 0
            DO 502 I = 1,50
               IX = IROUND (DATA(ICTR+I))
               IF (IX.GT.0) THEN
                  NANT1 = NANT1 + 1
                  IANT1(NANT1) = IX
                  END IF
 502           CONTINUE
            DO 503 I = 1,NANT1,10
               IX = MIN (I+9, NANT1)
               WRITE (MSGTXT,1424) (IANT1(J), J = I,IX)
               IF (I.EQ.1) MSGTXT(13:20) = 'antennas'
               CALL MSGWRT (5)
 503           CONTINUE
            IF (CVER.LE.2) THEN
               IF (DATA(IS+80-COFF1).GT.0.0) THEN
                  MSGTXT = ' '
                  MSGTXT(13:) = 'Bandpasses separated by color'
                  IF (IROUND(DATA(IS+80-COFF1)).EQ.2)
     *               MSGTXT(13:) = 'Bandpass values separated by color'
                  CALL MSGWRT (5)
                  END IF
               END IF
            GO TO 890
C                                       APCAL
 510     CONTINUE
            CALL H2CHR (4, 1, HDATA(IS+193), DTEMP)
            WRITE (MSGTXT,1510) IVER, DATE, DTEMP, DATA(IS+194),
     *         DATA(IS+195)
            CALL MSGWRT (5)
            ICTR = IS + 181
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = TIME
               MSGTXT(23:) = 'times from beginning to end'
            ELSE
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1431,ERR=511) TIME, TS1, IT1, TS2, IT2
               END IF
 511        CALL MSGWRT (5)
            CALL H2CHR (48, 1, HDATA(IS+206), FILNAM)
            IF (FILNAM.NE.' ') THEN
               WRITE (MSGTXT,1511) FILNAM
               I = JTRIM (MSGTXT)
               MSGTXT(I+1:) = ''''
               CALL MSGWRT (5)
               END IF
            CALL H2CHR (4, 1, HDATA(IS+58), DTEMP)
            DO 512 I = 1,4
               II = DATA(IS+6+I) + 0.1
               IF (II.GT.0) THEN
                  JJ = DATA(IS+307+I) + 0.01
                  WRITE (MSGTXT,1512) I, II, DTEMP(I:I), DATA(IS+217+I),
     *               DATA(IS+277+I), JJ
                  CALL MSGWRT (5)
                  END IF
 512           CONTINUE
            GO TO 890
C                                       ANBPL
C                                       CVER=1 has baddisk at end
C                                       CVER=2 pdver 253
C                                       CVER=3,4 differ only XYRATIO
 520     CONTINUE
            COFF1 = 0
            IF (CVER.GT.2) COFF1 = 1
            ITRC(1) = DATA(IS+138) + 0.1
            CALL H2CHR (2, 1, HDATA(IS+149), BTEMP)
            ITRC(2) = DATA(IS+261-COFF1) + 0.1
            WRITE (MSGTXT,1280) IVER, 'ANB', DATE, BTEMP, ITRC(1),
     *         'subarr', ITRC(2)
            IF (DATA(IS+250).GT.0.0) THEN
               MSGTXT(23:33) = 'Cal applied'
            ELSE
               MSGTXT(23:33) = 'No cal done'
               END IF
            CALL MSGWRT (5)
C                                       Axes: type, range
            CALL RCOPY (10, DATA(IS+265-COFF1), TDATA)
            IF ((TDATA(1).LT.-10.) .OR. (TDATA(1).GT.99.)) GO TO 880
            IF ((TDATA(2).LT.-10.) .OR. (TDATA(2).GT.99.)) GO TO 880
            IX = TDATA(1) + 0.1
            IF (IX.LE.2) THEN
               TDATA(4) = TDATA(4) * 24.0
               TDATA(5) = TDATA(5) * 24.0
               END IF
            WRITE (MSGTXT,1281) TIME, TANBPL(IX), TDATA(4), TDATA(5)
            CALL MSGWRT (5)
            IX = TDATA(2) + 0.01
            IF (TDATA(6).GE.TDATA(7)) THEN
               WRITE (MSGTXT,1283) TANBPL(IX)
            ELSE
               WRITE (MSGTXT,1282) TANBPL(IX), TDATA(6), TDATA(7)
               END IF
            CALL MSGWRT (5)
C                                       time range
            ICTR = IS + 140
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=521) TS1, IT1, TS2, IT2
               END IF
 521        CALL MSGWRT (5)
C                                       UV range
            ICTR = IS + 139
            IF ((DATA(ICTR).GT.0.0) .OR. ((DATA(ICTR).LT.DATA(ICTR+1))
     *         .AND. (DATA(ICTR+1).LE.1.0E9))) THEN
               WRITE (MSGTXT,1285) DATA(ICTR), DATA(ICTR+1)
               CALL MSGWRT (5)
               END IF
C                                       channels range
            ITRC(1) = DATA(IS+133) + 0.1
            ITRC(2) = DATA(IS+134) + 0.1
            IX = 1
            IBLC(1) = DATA(IS+135) + 0.1
            IBLC(2) = DATA(IS+136) + 0.1
            WRITE (MSGTXT,2285) ITRC, IX, IBLC
            CALL MSGWRT (5)
C                                       antennas, baseline
            ICTR = IS + 149
            NANT1 = 0
            NANT2 = 0
            IANT1(1) = 0
            IANT2(1) = 0
            DO 522 I = 1,50
               IX = IROUND (DATA(ICTR+I))
               IF ((NANT1.GT.0) .AND. (IX.EQ.IANT1(NANT1))) IX = 0
               IF (IX.NE.0) THEN
                  NANT1 = NANT1 + 1
                  IANT1(NANT1) = IX
                  END IF
               IX = IROUND (DATA(ICTR+I+50))
               IF ((NANT2.GT.0) .AND. (IX.EQ.IANT2(NANT2))) IX = 0
               IF (IX.NE.0) THEN
                  NANT2 = NANT2 + 1
                  IANT2(NANT2) = IX
                  END IF
 522           CONTINUE
C                                       print them - all
            MSGTXT = ' '
            IF ((NANT1.LE.0) .AND. (NANT2.LE.0)) THEN
               MSGTXT(23:) = 'All antenna pairs included'
               CALL MSGWRT (5)
C                                       by ones
            ELSE IF ((NANT1.LE.1) .AND. (NANT2.LE.1)) THEN
               WRITE (MSGTXT,1286) IANT1(1), IANT2(1)
               IF (IANT1(1).EQ.0) MSGTXT(21:22) = '**'
               IF (IANT2(1).EQ.0) MSGTXT(29:30) = '**'
               CALL MSGWRT (5)
C                                       individually
            ELSE
               IF (NANT1.LE.0) THEN
                  MSGTXT(23:) = 'All antennas'
                  CALL MSGWRT (5)
               ELSE
                  IY = 0
 523              IX = IY + 1
                  IF (IX.LE.NANT1) THEN
                     IY = MIN (IX+15, NANT1)
                     WRITE (MSGTXT,1287) (IANT1(I), I = IX,IY)
                     CALL MSGWRT (5)
                     GO TO 523
                     END IF
                  END IF
               IF (NANT2.LE.0) THEN
                  MSGTXT(:22) = ' '
                  MSGTXT(23:) = 'With all antennas'
                  CALL MSGWRT (5)
               ELSE
                  IY = 0
 524              IX = IY + 1
                  IF (IX.LE.NANT2) THEN
                     IY = MIN (IX+15, NANT2)
                     WRITE (MSGTXT,1288) (IANT2(I), I = IX,IY)
                     CALL MSGWRT (5)
                     GO TO 524
                     END IF
                  END IF
               END IF
C                                       XYRATIO
            T1 = DATA(IS+281-COFF1)
            IF ((CVER.LE.2) .AND. (T1.NE.1.0) .AND. (T1.NE.0.0)) THEN
               WRITE (MSGTXT,1367) T1
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       IRING
 530     CONTINUE
            ISX = IS + 8
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            IF (DATA(IS+35).GT.1.5) THEN
               CTEMP = 'Cumulative sums by rings'
            ELSE
               CTEMP = 'Averages in rings'
               END IF
            WRITE (MSGTXT,1530) IVER, DATE, IBLC, CTEMP
            CALL MSGWRT (5)
            WRITE (MSGTXT,1531) TIME, ITRC, DATA(IS+22), DATA(IS+23)
            CALL MSGWRT (5)
            ISX = IS + 23
            WRITE (MSGTXT,1532) (DATA(ISX+I), I = 1,4)
            CALL MSGWRT (5)
            IF (DATA(IS+37).GT.0.0) THEN
               CALL H2CHR (8, 1, DATA(IS+42), ATEMP)
               WRITE (MSGTXT,1533) ATEMP(:8), DATA(IS+37)
               CALL MSGWRT (5)
               END IF
            ISX = IS + 31
            IF (DATA(ISX+6).GT.0.5) THEN
               CTEMP = 'pixels'
            ELSE
               CTEMP = 'arcsec'
               END IF
            WRITE (MSGTXT,1534) (DATA(ISX+I), I = 1,3), CTEMP(:6)
            CALL MSGWRT (5)
            IF (DATA(ISX+4).GT.0.0) THEN
               CTEMP = 'Blanks -> 0.0 '
            ELSE
               CTEMP = 'Blanks ignored'
               END IF
            ISX = IROUND (DATA(IS+46))
            WRITE (MSGTXT,1535) ISX, DATA(IS+47), CTEMP(:14)
            CALL MSGWRT (5)
            GO TO 890
C                                       RSPEC
C                                       CVER=1 adds OPCODE at 23
C                                              and output image
 540     CONTINUE
            COFF1 = 0
            IF (CVER.GT.1) COFF1 = 9
            ISX = IS + 8
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            RANGE1 = DATA(IS+32-COFF1)
            RANGE2 = DATA(IS+33-COFF1)
            NS = DATA(IS+34-COFF1) + 0.01
            WRITE (MSGTXT,1400) IVER, DATE, IBLC, RANGE1, NS
            MSGTXT(5:11) = 'RMSpect'
            CALL MSGWRT (5)
            BTEMP = 'pt'
            IF (DATA(IS+39-COFF1).GT.0.0) BTEMP = 'bx'
            WRITE (MSGTXT,1271) TIME, ITRC, RANGE2, BTEMP
            CALL MSGWRT (5)
            LSGN(1) = DATA(IS+10) + 0.5
            LSGN(2) = DATA(IS+17) + 0.5
            X = 1.0
            IF (CVER.LE.2) X = DATA(IS+47)
            WRITE (MSGTXT,1402) LSGN, X
            CALL MSGWRT (5)
            CALL H2CHR (4, 1, HDATA(IS+22), CODET)
            IF (CODET.NE.' ') THEN
               WRITE (MSGTXT,1403) CODET
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       RFLAG
C                                       CVER=1 adds baddisk
 550     CONTINUE
            COFF3 = 0
            IF (CVER.EQ.1) COFF3 = 10
            IF (CVER.LE.2) THEN
               COFF1 = 1
               COFF2 = 2
            ELSE
               COFF1 = 0
               COFF2 = 0
               END IF
            ISX = IROUND (DATA(IS+528+COFF2+COFF3))
            CALL H2CHR (4, 1, DATA(IS+512+COFF2), ATEMP)
            IF ((ATEMP.EQ.'LG') .AND. (ISX.LE.4)) THEN
               I = JTRIM (RFTY(ISX))
               STRNG = 'log10 (' // RFTY(ISX)(:I) // ')'
            ELSE
               STRNG = RFTY(ISX)
               END IF
            I = JTRIM (STRNG)
            WRITE (MSGTXT,1550) IVER, DATE, STRNG(:I)
            CALL MSGWRT (5)
            IF (ISX.LE.4) THEN
               I = DATA(IS+534+COFF2+COFF3) - DATA(IS+533+COFF2+COFF3) +
     *            1.5
               WRITE (MSGTXT,1551) TIME, DATA(IS+531+COFF2+COFF3),
     *            DATA(IS+532+COFF2+COFF3), 'Jy', I
            ELSE
               IX = DATA(IS+156) + 0.5
               IY = DATA(IS+157) + 0.5
               I = DATA(IS+533+COFF2+COFF3) + 0.5
               WRITE (MSGTXT,1552) TIME, I, IX, IY
               END IF
            CALL MSGWRT (5)
            CALL H2CHR (4, 1, DATA(IS+153), ATEMP)
            WRITE (MSGTXT,1553) DATA(IS+529+COFF2+COFF3),
     *         DATA(IS+530+COFF2+COFF3), ATEMP(:4)
            CALL MSGWRT (5)
C                                       sources
            ICTR = IS + 7
            CALL H2CHR (16, 1, DATA(ICTR), CTEMP)
            IF (CTEMP.NE.' ') THEN
               II = 1
               MSGTXT = ' '
               MSGTXT(13:) = 'SOURCES='
               IX = 20
               I = JTRIM (CTEMP)
               IF (IX+I+2.LT.64) THEN
                  MSGTXT(IX+1:) = '''' // CTEMP(:I) // ''''
                  IX = IX + I + 2
                  END IF
 551           II = II + 1
               IF (II.LE.30) THEN
                  ICTR = ICTR + 4
                  CALL H2CHR (16, 1, DATA(ICTR), CTEMP)
                  I = JTRIM (CTEMP)
                  IF (I.GT.0) THEN
                     MSGTXT(IX+1:) = ','
                     IX = IX + 1
                     IF (IX+I+2.GE.64) THEN
                        CALL MSGWRT (5)
                        MSGTXT = ' '
                        IX = 20
                        END IF
                     MSGTXT(IX+1:) = '''' // CTEMP(:I) // ''''
                     IX = IX + I + 2
                     GO TO 551
                     END IF
                  END IF
               IF (IX.GT.20) CALL MSGWRT (5)
               END IF
C                                       UV range
            ICTR = IS + 154
            IF ((DATA(ICTR).GT.0.0) .OR. ((DATA(ICTR).LT.DATA(ICTR+1))
     *         .AND. (DATA(ICTR+1).LT.1.0E9))) THEN
               WRITE (MSGTXT,1285) DATA(ICTR), DATA(ICTR+1)
               CALL MSGWRT (5)
               END IF
C                                       time range
            ICTR = IS + 128
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=552) TS1, IT1, TS2, IT2
               END IF
 552        CALL MSGWRT (5)
C                                       time rms parms
            I = IROUND (DATA(IS+321+COFF2))
            WRITE (MSGTXT,1554) I, DATA(IS+322+COFF2)
            CALL MSGWRT (5)
            I = IROUND (DATA(IS+318+COFF1))
            WRITE (MSGTXT,1555) I
            IF (DATA(IS+324+COFF2).NE.0.0) CALL MSGWRT (5)
            GO TO 890
C                                       Contour plot - CCNTR
 560     CONTINUE
            ISX = IS + 8
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            XPLEV = DATA(IS+24)
            XCLEV = DATA(IS+25)
            IF (XPLEV.GT.0.0001) XCLEV = XPLEV * CATR(KRDMX) / 100.0
            BTEMP = ' C'
            WRITE (MSGTXT,1220) IVER, BTEMP, DATE, IBLC, XCLEV
            CALL MSGWRT (5)
            WRITE (MSGTXT,1221) TIME, ITRC
C                                       Do levels.
            ICTR = 35
            DO 562 I = 1,30
               XLEVS = DATA(25+IS+I)
               WRITE (CTEMP,1222,ERR=561) XLEVS
               IF ((XLEVS.GE.-99.90) .AND. (XLEVS.LE.999.90))
     *            WRITE (CTEMP,1223,ERR=561) XLEVS
               IF ((XLEVS.GE.-9.990) .AND. (XLEVS.LE.99.990))
     *            WRITE (CTEMP,1224,ERR=561) XLEVS
               IF ((XLEVS.GE.-0.9990) .AND. (XLEVS.LE.9.9990))
     *            WRITE (CTEMP,1225,ERR=561) XLEVS
 561           IF (XLEVS.GE.DATA(IS+26+I)) THEN
                  GO TO 563
               ELSE
                  MSGTXT(ICTR:ICTR+6) = CTEMP(1:7)
                  ICTR = ICTR + 7
C                                       Last lev on line.
                  IF (ICTR.GT.58) THEN
                     CALL MSGWRT (5)
                     MSGTXT = ' '
                     ICTR = 35
                     END IF
                  END IF
 562           CONTINUE
 563        MSGTXT(ICTR:) = CTEMP(1:6) // ')'
            CALL MSGWRT (5)
C                                       ST, CC, MF
            CALL H2CHR (2, 1, HDATA(IS+57), BTEMP)
            STVERS = DATA(IS+58) + 0.01
            STFACT = DATA(IS+65)
            IF ((BTEMP.EQ.'CC') .OR. (BTEMP.EQ.'MF')) THEN
               IF (DATA(IS+64).GT.0.0) THEN
                  STFACT = STFACT / DATA(IS+64)
                  WRITE (MSGTXT,1563) BTEMP, STVERS, STFACT
               ELSE
                  WRITE (MSGTXT,1564) BTEMP, STVERS, STFACT
                  END IF
            ELSE
               WRITE (MSGTXT,1565) BTEMP, STVERS, STFACT
               END IF
            CALL MSGWRT (5)
            IF ((BTEMP.EQ.'CC') .OR. (BTEMP.EQ.'MF')) THEN
               J1 = DATA(IS+59) + 0.01
               J2 = DATA(IS+60) + 0.01
               IF (DATA(IS+62).GT.DATA(IS+61)) THEN
                  IF (J2.GT.0) THEN
                     WRITE (MSGTXT,1566) J2, J1, DATA(IS+61),
     *                  DATA(IS+62)
                  ELSE
                     WRITE (MSGTXT,1567) J1, DATA(IS+61), DATA(IS+62)
                     END IF
                  CALL MSGWRT (5)
               ELSE IF (J2.GT.0) THEN
                  WRITE (MSGTXT,1568) J2, J1
                  CALL MSGWRT (5)
                  END IF
               END IF
            GO TO 890
C                                       SERCH
 570     CONTINUE
            ISX = IS + 7
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
C                                       get ra-dec window
            IX = DATA(ISX) + .1
            IBLC(1) = DATA(ISX+1) + .1
            IBLC(2) = DATA(ISX+2) + .1
            IY = DATA(ISX+7) + .5
            ITRC(1) = DATA(ISX+8) + .5
            ITRC(2) = DATA(ISX+9) + .5
            II = DATA(IS+21) + 0.1
            JJ = DATA(IS+22) + 0.1
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            WRITE (MSGTXT,1570) IVER, DATE, IX, IBLC, II
            CALL MSGWRT (5)
            WRITE (MSGTXT,1571) TIME, IY, ITRC, JJ
            CALL MSGWRT (5)
            II = DATA(IS+23) + 0.1
            JJ = DATA(IS+24) + 0.1
            IX = DATA(IS+21) + 0.1
            WRITE (MSGTXT,1572) II, JJ, IX
            CALL MSGWRT (5)
            CALL H2CHR (48, 1, DATA(IS+112), FILNAM)
            IF (FILNAM.NE.' ') THEN
               WRITE (MSGTXT,1573) FILNAM
               I = JTRIM (MSGTXT)
               MSGTXT(I+1:) = ''''
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       EVAUV
 580     CONTINUE
            FT = DATA(IS+443) + 0.1
            WRITE (MSGTXT,1580) IVER, DATE
            IF (FT.EQ.1) THEN
               MSGTXT(23:) = 'Data-model mean flux vs uv radius'
            ELSE IF (FT.EQ.2) THEN
               MSGTXT(23:) = 'Data/model-1 mean gain vs uv radius'
            ELSE IF (FT.EQ.3) THEN
               MSGTXT(23:) = 'Histogram model image full flux range'
            ELSE IF (FT.EQ.4) THEN
               MSGTXT(23:) = 'Histogram model image part flux range'
            ELSE IF (FT.EQ.5) THEN
               MSGTXT(23:) = 'Histogram image real vs imag data-model'
            ELSE IF (FT.EQ.6) THEN
               MSGTXT(23:) = 'Histogram image re vs im data/model-1'
            ELSE
               MSGTXT(23:) = 'unknown plot type'
               END IF
            CALL MSGWRT (5)
            X = DATA(IS+444+COFF1)
            Y = DATA(IS+445+COFF1)
            T1 = Y
            IF (X.LT.0.0) T1 = MAX (ABS(Y), ABS(10.0*X))
            IF (T1.LT.999.) THEN
               WRITE (MSGTXT,1581) TIME, 'X', X, Y
            ELSE IF (T1.LT.9999.) THEN
               WRITE (MSGTXT,1582) TIME, 'X', X, Y
            ELSE IF (T1.LT.99999.) THEN
               WRITE (MSGTXT,1583) TIME, 'X', X, Y
            ELSE IF (T1.LT.999999.) THEN
               WRITE (MSGTXT,1584) TIME, 'X', X, Y
            ELSE
               WRITE (MSGTXT,1585) TIME, 'X', X, Y
               END IF
            IF (FT.LE.2) MSGTXT(50:) = 'wavelengths'
            CALL MSGWRT (5)
            X = DATA(IS+446+COFF1)
            Y = DATA(IS+447+COFF1)
            T1 = Y
            IF (X.LT.0.0) T1 = MAX (ABS(Y), ABS(10.0*X))
            IF (T1.LT.999.) THEN
               WRITE (MSGTXT,1581) TIME, 'Y', X, Y
            ELSE IF (T1.LT.9999.) THEN
               WRITE (MSGTXT,1582) TIME, 'Y', X, Y
            ELSE IF (T1.LT.99999.) THEN
               WRITE (MSGTXT,1583) TIME, 'Y', X, Y
            ELSE IF (T1.LT.999999.) THEN
               WRITE (MSGTXT,1584) TIME, 'Y', X, Y
            ELSE
               WRITE (MSGTXT,1585) TIME, 'Y', X, Y
               END IF
            IF (FT.EQ.1) THEN
               MSGTXT(50:) = 'Jy'
            ELSE IF (FT.EQ.2) THEN
               MSGTXT(50:) = 'gain'
            ELSE IF (FT.LE.4) THEN
               MSGTXT(50:) = 'counts'
               CALL H2CHR (2, 1, HDATA(IS+166+COFF1), FUNC)
               IF (FUNC.EQ.'LG') MSGTXT(50:) = 'log (counts)'
               END IF
            CALL MSGWRT (5)
C                                       smodel or image
            X = DATA(IS+397)
            IF (X.EQ.0.0) THEN
               CALL H2CHR (12, 1, DATA(IS+9), NAME)
               CALL H2CHR (6, 1, DATA(IS+12), CLASS)
               I = IROUND (DATA(IS+14))
               J = IROUND (DATA(IS+15))
               WRITE (MSGTXT,1586) NAME, CLASS, I, J
            ELSE
               WRITE (MSGTXT,1588) X, DATA(IS+398), DATA(IS+399)
               END IF
            CALL MSGWRT (5)
            IF (FT.GT.4) THEN
               I = IROUND (DATA(IS+419))
               J = IROUND (DATA(IS+420))
               WRITE (MSGTXT,1587) I, J
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       DELZN
 590     CONTINUE
            I = DATA(IS+7) + 0.1
            J = DATA(IS+8) + 0.1
            WRITE (MSGTXT,1590) IVER, DATE, I, J
            CALL MSGWRT (5)
            WRITE (MSGTXT,1591) TIME
            FT = DATA(IS+9) + 1.1
            IF (FT.EQ.1) THEN
               MSGTXT(31:) = 'zenith atmosphere delay'
            ELSE IF (FT.EQ.2) THEN
               MSGTXT(31:) = 'clock error'
            ELSE IF (FT.EQ.3) THEN
               MSGTXT(31:) = 'reference antenna value'
            ELSE IF (FT.EQ.4) THEN
               MSGTXT(31:) = 'dispersion'
            ELSE
               MSGTXT(31:) = '?? APARM(1) bad value'
               END IF
            CALL MSGWRT (5)
C                                       time range
            ICTR = IS + 264
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=591) TS1, IT1, TS2, IT2
               END IF
 591        CALL MSGWRT (5)
C                                       IFs, subarray
            I = DATA(IS+263)
            J = DATA(IS+264)
            ICTR = DATA(IS+323)
            WRITE (MSGTXT,1592) I, J, ICTR
            CALL MSGWRT (5)
C                                       use, plot
            CALL H2CHR (4, 1, HDATA(IS+326), CODET)
            IF (CODET.EQ.' ') CODET = 'MDEL'
            WRITE (MSGTXT,1593) CODET
            I = IROUND (DATA(IS+18))
            IF (I.EQ.1) THEN
               MSGTXT(33:) = 'model vs recalculated data'
            ELSE
               MSGTXT(33:) = 'data vs fit models directly'
               END IF
            CALL MSGWRT (5)
            I = DATA(IS+324)
            J = DATA(IS+325)
            WRITE (MSGTXT,1594) I, J
            CALL MSGWRT (5)
            GO TO 890
C                                       ELINT
 600     CONTINUE
            I = DATA(IS+7) + 0.1
            J = DATA(IS+8) + 0.1
            ICTR = DATA(IS+316) + 0.1
            WRITE (MSGTXT,1600) IVER, DATE, I, J, ICTR
            CALL MSGWRT (5)
            I = DATA(IS+255) + 0.1
            J = DATA(IS+256) + 0.1
            ICTR = DATA(IS+315) + 0.1
            WRITE (MSGTXT,1601) TIME, I, J, ICTR
            CALL MSGWRT (5)
C                                       time range
            ICTR = IS + 256
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=601) TS1, IT1, TS2, IT2
               END IF
 601        CALL MSGWRT (5)
            CALL H2CHR (4, 1, HDATA(IS+319), CODET)
            IF (CODET.EQ.' ') CODET = 'POLZ'
            MSGTXT = ' '
            MSGTXT(13:) = 'Fit type'
            IF (CODET.EQ.'POLE') THEN
               MSGTXT (22:) = 'amplitude vs elevation'
            ELSE IF (CODET.EQ.'POLZ') THEN
               MSGTXT (22:) = 'amplitude vs zenith distance'
            ELSE IF (CODET(:3).EQ.'PWR') THEN
               MSGTXT (22:) = 'power vs zenith distance'
               END IF
            CALL MSGWRT (5)
C                                       optype
            GO TO 890
C                                       FGPLT
 610     CONTINUE
            CALL H2CHR (4, 1, HDATA(IS+16), CODET)
            I = DATA(IS+7) + 0.1
            WRITE (MSGTXT,1610) IVER, DATE, I, CODET
            CALL MSGWRT (5)
            I = DATA(IS+19) + 0.01
            J = DATA(IS+20) + 0.01
            J1 = DATA(IS+21) + 0.01
            J2 = DATA(IS+22) + 0.01
            WRITE (MSGTXT,1611) TIME, I, J, J1, J2
            CALL MSGWRT (5)
C                                       time range
            ICTR = IS + 7
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=611) TS1, IT1, TS2, IT2
               END IF
 611        CALL MSGWRT (5)
            GO TO 890
C                                       GAL
 620     CONTINUE
            ISX = IS + 22
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            IBLC(1) = DATA(ISX) + .5
            IBLC(2) = DATA(ISX+1) + .5
            ITRC(1) = DATA(ISX+7) + .5
            ITRC(2) = DATA(ISX+8) + .5
            IF ((ITRC(1).LT.IBLC(1)) .OR. (ITRC(2).LT.IBLC(2)))
     *         GO TO 880
            CALL H2CHR (2, 1, HDATA(IS+37), CODET)
            WRITE (MSGTXT,1620) IVER, DATE, IBLC
            IF (CODET.EQ.'BR') THEN
               MSGTXT(51:) = 'Brandt'
            ELSE IF (CODET.EQ.'EX') THEN
               MSGTXT(51:) = 'Exponential'
            ELSE IF (CODET.EQ.'CC') THEN
               MSGTXT(51:) = 'Constant'
            ELSE IF (CODET.EQ.'SB') THEN
               MSGTXT(51:) = 'Solid body'
               END IF
            CALL MSGWRT (5)
            WRITE (MSGTXT,1621) TIME, ITRC, DATA(IS+67)
            CALL MSGWRT (5)
C                                       weight image
            IF (DATA(IS+51).GT.0.0) THEN
               CALL H2CHR (12, 1, DATA(IS+8), NAME)
               CALL H2CHR (6, 1, DATA(IS+11), CLASS)
               I = DATA(IS+13)
               J = DATA(IS+14)
               WRITE (MSGTXT,1622) NAME, CLASS, I, J
               CALL MSGWRT (5)
               END IF
C                                       plot ranges
            WRITE (MSGTXT,1623) DATA(IS+58), DATA(IS+59),
     *         DATA(IS+66), DATA(IS+65)
            CALL MSGWRT (5)
            WRITE (MSGTXT,1624) DATA(IS+60), DATA(IS+61),
     *         DATA(IS+62), DATA(IS+63)
            CALL MSGWRT (5)
C                                       fit values
            WRITE (MSGTXT,1625) DATA(IS+38), DATA(IS+39), DATA(IS+42)
            CALL MSGWRT (5)
            WRITE (MSGTXT,1626) DATA(IS+40), DATA(IS+41)
            CALL MSGWRT (5)
            GO TO 890
C                                       SNIFS
 630     CONTINUE
            CALL H2CHR (2, 1, HDATA(IS+7), BTEMP)
            CALL H2CHR (4, 1, HDATA(IS+198), ATEMP)
            CALL H2CHR (4, 1, HDATA(IS+199), DTEMP)
            I = DATA(IS+8) + 0.01
            WRITE (MSGTXT,1630) IVER, BTEMP, DATE, I, ATEMP(:4),
     *         DTEMP
            CALL MSGWRT (5)
            RANGE1 = DATA(IS+195)
            RANGE2 = DATA(IS+196)
            IF (RANGE2.GT.RANGE1) THEN
               WRITE (MSGTXT,1371) TIME, RANGE1, RANGE2
            ELSE
               WRITE (MSGTXT,1372) TIME
               END IF
            CALL MSGWRT (5)
C                                       time range
            ICTR = IS + 129
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE IF ((T1.NE.0) .OR. (T2.NE.0)) THEN
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=631) TS1, IT1, TS2, IT2
               END IF
 631        CALL MSGWRT (5)
C                                       Antennas
            NANT1 = 0
            ICTR = IS + 144
            DO 632 I = 1,50
               J = IROUND (DATA(ICTR+I))
               IF (J.NE.0) THEN
                  NANT1 = NANT1 + 1
                  IANT1(NANT1) = J
                  END IF
 632           CONTINUE
            IY = 0
 633        IX = IY + 1
            IF (IX.LE.NANT1) THEN
               IY = MIN (IX+15, NANT1)
               WRITE (MSGTXT,1287) (IANT1(I), I = IX,IY)
               CALL MSGWRT (5)
               GO TO 633
               END IF
C                                       solint, nplots
            I = DATA(IS+197) + 0.01
            WRITE (MSGTXT,1631) DATA(IS+202), I
            CALL MSGWRT (5)
            GO TO 890
C                                       SOUSP
 640     CONTINUE
            IF (CVER.EQ.1) THEN
               COFF1 = 2
            ELSE
               COFF1 = 0
               END IF
            CALL H2CHR (16, 1, HDATA(IS+7), STRNG)
            J = JTRIM(STRNG(:16))
            WRITE (MSGTXT,1640) IVER, DATE, STRNG(:J)
            IF (DATA(IS+333+COFF1).EQ.1.0) THEN
               MSGTXT(24:30) = 'log-log'
            ELSE
               MSGTXT(24:30) = 'lin-lin'
               END IF
            CALL MSGWRT (5)
            I = DATA(IS+129)
            CALL H2CHR (4, 1, HDATA(IS+127), STRNG)
            WRITE (MSGTXT,1641) TIME, STRNG(:4), I
            CALL MSGWRT (5)
            WRITE (MSGTXT,1642) DATA(IS+329+COFF1), DATA(IS+330+COFF1),
     *         DATA(IS+331+COFF1), DATA(IS+332+COFF1)
            CALL MSGWRT (5)
            X = 10.0 ** DATA(IS+334+COFF1)
            WRITE (MSGTXT,1643) X, DATA(IS+335+COFF1)
            CALL MSGWRT (5)
            IF ((CVER.EQ.1) .AND. (DATA(IS+322).GT.0.0)) THEN
               I = IROUND (DATA(IS+323))
               J = IROUND (DATA(IS+324))
               J = MAX (I, J)
               WRITE (MSGTXT,1644) I, J
               IF (J.GT.0) CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       WETHR
C                                       CVER=1 adds baddisk
 650     CONTINUE
            I = DATA(IS+7) + 0.1
            J = DATA(IS+69) + 0.1
            ICTR = DATA(IS+70) + 0.1
            WRITE (MSGTXT,1650) IVER, DATE, I
            CALL MSGWRT (5)
            WRITE (MSGTXT,1651) TIME, J, ICTR
            CALL MSGWRT (5)
C                                       time range
            ICTR = IS + 7
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=651) TS1, IT1, TS2, IT2
               END IF
 651        CALL MSGWRT (5)
            IF (DATA(IS+67).GE.DATA(IS+68)) THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'Plots self-scaling'
            ELSE
               WRITE (MSGTXT,1652) DATA(IS+67), DATA(IS+68)
               END IF
            CALL MSGWRT (5)
C                                       what plotted
            CALL H2CHR (4, 1, HDATA(IS+74), CODET)
            IF ((CODET.NE.'MULT') .AND. (CODET.NE.'FLAG')) THEN
               WRITE (MSGTXT,1653) CODET
               CALL MSGWRT (5)
            ELSE
               ICTR = IS + 76
               MSGTXT = ' '
               MSGTXT(13:) = 'Plot'
               J = 18
 652           I = DATA(ICTR) + 0.1
               IF ((I.GT.0) .AND. (I.LE.12)) THEN
                  MSGTXT(J:) = WXCODE(I)
                  J = J + 5
                  IF (J.GE.60) THEN
                     CALL MSGWRT (5)
                     J = 18
                     MSGTXT(J:) = ' '
                     END IF
                  ICTR = ICTR + 1
                  IF (ICTR.LT.IS+86) GO TO 652
                  END IF
               IF (J.GT.18) CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       RM2PL
 660     CONTINUE
            I = DATA(IS+15) + 0.1
            J = DATA(IS+16) + 0.1
            ICTR = DATA(IS+14) + 0.1
            WRITE (MSGTXT,1660) IVER, 'RM', DATE, I, J, ICTR
            CALL MSGWRT (5)
            I = IROUND (DATA(IS+12))
            CALL H2CHR (12, 1, DATA(IS+7), NAME2)
            CALL H2CHR (6, 1, DATA(IS+10), CLASS2)
            WRITE (MSGTXT,1661) TIME, 'U', NAME2, CLASS2, I
            CALL MSGWRT (5)
C                                       average
            IF (DATA(IS+21).LT.0.0) THEN
               J = DATA(IS+22) + 0.1
               IF (J.GT.0) THEN
                  WRITE (MSGTXT,1662) J
                  CALL MSGWRT (5)
                  END IF
            ELSE
               I = DATA(IS+21) + 0.1
               J = DATA(IS+22) + 0.1
               IF ((I.GT.0) .OR. (J.GT.0)) THEN
                  WRITE (MSGTXT,1663) I, J
                  CALL MSGWRT (5)
                  END IF
               END IF
C                                       what plotted
            MSGTXT = ' '
            MSGTXT(13:) = 'Plot Q'
            I = 20
            IF (DATA(IS+25).GT.0.0) THEN
               MSGTXT(I:) = 'Data'
               I = I + 5
               END IF
            IF (DATA(IS+26).GT.0.0) THEN
               MSGTXT(I:) = 'Model'
               I = I + 6
               END IF
            IF (DATA(IS+27).GT.0.0) THEN
               MSGTXT(I:) = 'Resid'
               I = I + 6
               END IF
            IF (DATA(IS+28).GT.0.0) THEN
               MSGTXT(I:) = 'Comps'
               I = I + 6
               END IF
            IF (I.GT.20) THEN
               WRITE (MSGTXT(I:),1664) DATA(IS+17), DATA(IS+18)
               CALL MSGWRT (5)
               END IF
            MSGTXT = ' '
            MSGTXT(13:) = 'Plot U'
            I = 20
            IF (DATA(IS+30).GT.0.0) THEN
               MSGTXT(I:) = 'Data'
               I = I + 5
               END IF
            IF (DATA(IS+31).GT.0.0) THEN
               MSGTXT(I:) = 'Model'
               I = I + 6
               END IF
            IF (DATA(IS+32).GT.0.0) THEN
               MSGTXT(I:) = 'Resid'
               I = I + 6
               END IF
            IF (DATA(IS+33).GT.0.0) THEN
               MSGTXT(I:) = 'Comps'
               I = I + 6
               END IF
            IF (I.GT.20) THEN
               WRITE (MSGTXT(I:),1664) DATA(IS+19), DATA(IS+20)
               CALL MSGWRT (5)
               END IF
C                                       X axis
            I = IROUND (DATA(IS+35))
            J = IROUND (DATA(IS+36))
            ICTR = IROUND (DATA(IS+38)) + 1
            ICTR = MAX (1, MIN (4, ICTR))
            WRITE (MSGTXT,1666) I, J, RMTYPE(ICTR), 'U', DATA(IS+37)
            CALL MSGWRT (5)
            GO TO 890
C                                       XG2PL
 670     CONTINUE
            I = DATA(IS+16) + 0.1
            J = DATA(IS+17) + 0.1
            ICTR = DATA(IS+14) + 0.1
            WRITE (MSGTXT,1660) IVER, 'XG', DATE, I, J, ICTR
            CALL MSGWRT (5)
            I = IROUND (DATA(IS+12))
            CALL H2CHR (12, 1, DATA(IS+7), NAME2)
            CALL H2CHR (6, 1, DATA(IS+10), CLASS2)
            WRITE (MSGTXT,1661) TIME, 'V', NAME2, CLASS2, I
            IF (NAME2.EQ.' ') MSGTXT(23:) = 'NO Vpol image'
            CALL MSGWRT (5)
C                                       average
            IF (DATA(IS+22).LT.0.0) THEN
               J = DATA(IS+23) + 0.1
               IF (J.GT.0) THEN
                  WRITE (MSGTXT,1662) J
                  CALL MSGWRT (5)
                  END IF
            ELSE
               I = DATA(IS+22) + 0.1
               J = DATA(IS+23) + 0.1
               IF ((I.GT.0) .OR. (J.GT.0)) THEN
                  WRITE (MSGTXT,1663) I, J
                  CALL MSGWRT (5)
                  END IF
               END IF
C                                       what plotted
            MSGTXT = ' '
            MSGTXT(13:) = 'Plot I'
            I = 20
            IF (DATA(IS+26).GT.0.0) THEN
               MSGTXT(I:) = 'Data'
               I = I + 5
               END IF
            IF (DATA(IS+27).GT.0.0) THEN
               MSGTXT(I:) = 'Model'
               I = I + 6
               END IF
            IF (DATA(IS+28).GT.0.0) THEN
               MSGTXT(I:) = 'Resid'
               I = I + 6
               END IF
            IF (DATA(IS+29).GT.0.0) THEN
               MSGTXT(I:) = 'Comps'
               I = I + 6
               END IF
            IF (I.GT.20) THEN
               WRITE (MSGTXT(I:),1664) DATA(IS+18), DATA(IS+19)
               CALL MSGWRT (5)
               END IF
            MSGTXT = ' '
            MSGTXT(13:) = 'Plot V'
            I = 20
            IF (DATA(IS+31).GT.0.0) THEN
               MSGTXT(I:) = 'Data'
               I = I + 5
               END IF
            IF (DATA(IS+32).GT.0.0) THEN
               MSGTXT(I:) = 'Model'
               I = I + 6
               END IF
            IF (DATA(IS+33).GT.0.0) THEN
               MSGTXT(I:) = 'Resid'
               I = I + 6
               END IF
            IF (DATA(IS+34).GT.0.0) THEN
               MSGTXT(I:) = 'Comps'
               I = I + 6
               END IF
            IF (I.GT.20) THEN
               WRITE (MSGTXT(I:),1664) DATA(IS+20), DATA(IS+21)
               CALL MSGWRT (5)
               J = IROUND (DATA(IS+15))
               IF (J.GT.0) THEN
                  WRITE (MSGTXT,1665) J
                  CALL MSGWRT (5)
                  END IF
               END IF
C                                       X axis
            I = IROUND (DATA(IS+36))
            J = IROUND (DATA(IS+37))
            ICTR = IROUND (DATA(IS+39)) + 1
            ICTR = MAX (1, MIN (3, ICTR))
            IF (ICTR.EQ.3) ICTR = 5
            WRITE (MSGTXT,1666) I, J, RMTYPE(ICTR), 'V', DATA(IS+38)
            CALL MSGWRT (5)
            GO TO 890
C                                       ALVAR
C                                       CVER 1 has baddisk at end
 680     CONTINUE
            I = IROUND (DATA(IS+141))
            J = IROUND (DATA(IS+142))
            ICTR = IROUND (DATA(IS+143))
            CALL H2CHR (4, 1, HDATA(IS+129), DTEMP)
            WRITE (MSGTXT,1680) IVER, DATE, J, ICTR, I, DTEMP
            CALL MSGWRT (5)
            WRITE (MSGTXT,1681) TIME
            I = IROUND (DATA(IS+258))
            IF (I.EQ.0) I = 3
            IF (I.EQ.1) THEN
               MSGTXT(28:) = 'mean of vector amp of normalized vis'
            ELSE IF (I.EQ.2) THEN
               MSGTXT(28:) = 'mean of phase adjusted vis'
            ELSE IF (I.EQ.3) THEN
               MSGTXT(28:) = 'median of vector amp normalized vis'
            ELSE IF (I.EQ.4) THEN
               MSGTXT(28:) = 'median of phase adjusted vis'
               END IF
            CALL MSGWRT (5)
C                                       time range
            ICTR = IS + 129
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=681) TS1, IT1, TS2, IT2
               END IF
 681        CALL MSGWRT (5)
C                                       cal, solint, functype
            IF (DATA(IS+246).GT.0.0) THEN
               MSGTXT(13:) = 'Cal applied'
            ELSE
               MSGTXT(13:) = 'No cal done'
               END IF
            CALL H2CHR (2, 1, HDATA(IS+260), BTEMP)
            WRITE (MSGTXT(26:),1682) DATA(IS+257), BTEMP
            CALL MSGWRT (5)
C                                       bchan, echan, nplots
            I = IROUND (DATA(IS+144))
            J = IROUND (DATA(IS+145))
            ICTR = IROUND (DATA(IS+259))
            WRITE (MSGTXT,1683) I, J, ICTR
            IF (ICTR.LT.0) MSGTXT(37:) = 'All baselines together'
            CALL MSGWRT (5)
C                                       antennas, baseline
            ICTR = IS + 145
            NANT1 = 0
            NANT2 = 0
            IANT1(1) = 0
            IANT2(1) = 0
            DO 682 I = 1,50
               IX = IROUND (DATA(ICTR+I))
               IF ((NANT1.GT.0) .AND. (IX.EQ.IANT1(NANT1))) IX = 0
               IF (IX.NE.0) THEN
                  NANT1 = NANT1 + 1
                  IANT1(NANT1) = IX
                  END IF
               IX = IROUND (DATA(ICTR+I+50))
               IF ((NANT2.GT.0) .AND. (IX.EQ.IANT2(NANT2))) IX = 0
               IF (IX.NE.0) THEN
                  NANT2 = NANT2 + 1
                  IANT2(NANT2) = IX
                  END IF
 682           CONTINUE
C                                       print them - all
            MSGTXT = ' '
            IF ((NANT1.LE.0) .AND. (NANT2.LE.0)) THEN
               MSGTXT(23:) = 'All antenna pairs included'
               CALL MSGWRT (5)
C                                       by ones
            ELSE IF ((NANT1.LE.1) .AND. (NANT2.LE.1)) THEN
               WRITE (MSGTXT,1286) IANT1(1), IANT2(1)
               IF (IANT1(1).EQ.0) MSGTXT(21:22) = '**'
               IF (IANT2(1).EQ.0) MSGTXT(29:30) = '**'
               CALL MSGWRT (5)
C                                       individually
            ELSE
               IF (NANT1.LE.0) THEN
                  MSGTXT(23:) = 'All antennas'
                  CALL MSGWRT (5)
               ELSE
                  IY = 0
 683              IX = IY + 1
                  IF (IX.LE.NANT1) THEN
                     IY = MIN (IX+15, NANT1)
                     WRITE (MSGTXT,1287) (IANT1(I), I = IX,IY)
                     CALL MSGWRT (5)
                     GO TO 683
                     END IF
                  END IF
               IF (NANT2.LE.0) THEN
                  MSGTXT(:22) = ' '
                  MSGTXT(23:) = 'With all antennas'
                  CALL MSGWRT (5)
               ELSE
                  IY = 0
 684              IX = IY + 1
                  IF (IX.LE.NANT2) THEN
                     IY = MIN (IX+15, NANT2)
                     WRITE (MSGTXT,1288) (IANT2(I), I = IX,IY)
                     CALL MSGWRT (5)
                     GO TO 684
                     END IF
                  END IF
               END IF
            GO TO 890
C                                       PCPLT
 690     CONTINUE
            I = IROUND (DATA(IS+11))
            J = IROUND (DATA(IS+22))
            ICTR = IROUND (DATA(IS+23))
            CALL H2CHR (4, 1, HDATA(IS+24), DTEMP)
            WRITE (MSGTXT,1690) IVER, DATE, J, ICTR, I, DTEMP
            CALL MSGWRT (5)
            CALL H2CHR (2, 1, HDATA(IS+76), BTEMP)
            CALL H2CHR (4, 1, HDATA(IS+75), DTEMP)
            I = IROUND (DATA(IS+9))
            WRITE (MSGTXT,1691) TIME, DTEMP, BTEMP, I
            CALL MSGWRT (5)
C                                       time range
            ICTR = IS + 11
            T1 = (((DATA(ICTR+4) / 60.0 + DATA(ICTR+3)) / 60.0) +
     *         DATA(ICTR+2)) / 24.0 + DATA(ICTR+1)
            T2 = (((DATA(ICTR+8) / 60.0 + DATA(ICTR+7)) / 60.0) +
     *         DATA(ICTR+6)) / 24.0 + DATA(ICTR+5)
            IF (((T1.LE.-10.0) .AND. (T2.GE.100.0)) .OR. (T1.GE.T2))
     *         THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'times from beginning to end'
            ELSE
               CALL T2DHMS (T1, TS1, IT1)
               CALL T2DHMS (T2, TS2, IT2)
               WRITE (MSGTXT,1284,ERR=691) TS1, IT1, TS2, IT2
               END IF
 691        CALL MSGWRT (5)
            GO TO 890
C                                       PLROW
 700     CONTINUE
            WRITE (MSGTXT,1700) IVER, DATE, DATA(IS+22), DATA(IS+23)
            CALL MSGWRT (5)
            I = IROUND (DATA(IS+24))
            WRITE (MSGTXT,1701) TIME, I, DATA(IS+25)
            CALL MSGWRT (5)
            IBLC(1) = IROUND (DATA(IS+8))
            IBLC(2) = IROUND (DATA(IS+9))
            ITRC(1) = IROUND (DATA(IS+15))
            ITRC(2) = IROUND (DATA(IS+16))
            I = IROUND (DATA(IS+10))
            WRITE (MSGTXT,1702) IBLC, ITRC, I
            CALL MSGWRT (5)
            GO TO 890
C                                       PLOTC
 710     CONTINUE
            CALL H2CHR (6, 1, DATA(IS+0), PRTASK)
            WRITE (MSGTXT,1710) IVER, DATE, PRTASK
            CALL MSGWRT (5)
            IV = DATA(IS+136) + 0.1
            IF (PRTASK.EQ.'UVPLT') THEN
               I = DATA(IS+130) + 0.1
               J = DATA(IS+131) + 0.1
               II = DATA(IS+133) + 0.1
               IX = DATA(IS+134) + 0.1
               IY = DATA(IS+135) + 0.1
               WRITE (MSGTXT,1711) TIME, 'X: spectral channel',
     *            ' Y: polarization'
               CALL MSGWRT (5)
               WRITE (MSGTXT,1712) IX, IY, I, J, II
               CALL MSGWRT (5)
            ELSE IF ((PRTASK.EQ.'EDITA') .OR. (PRTASK.EQ.'EDITR') .OR.
     *         (PRTASK.EQ.'SNEDT') .OR. ((IV.NE.2) .AND.
     *         (PRTASK.EQ.'SNPLT'))) THEN
               IX = DATA(IS+134) + 0.1
               IY = DATA(IS+135) + 0.1
               WRITE (MSGTXT,1711) TIME, 'X: spectral window',
     *            ' Y: polarization'
               CALL MSGWRT (5)
               WRITE (MSGTXT,1713) IX, IY
               CALL MSGWRT (5)
            ELSE IF ((PRTASK.EQ.'SNPLT') .OR. (PRTASK.EQ.'ELFIT')) THEN
               WRITE (MSGTXT,1711) TIME, ' X: nothing',
     *            ' Y: source name'
               CALL MSGWRT (5)
               CALL H2CHR (2, 1, DATA(IS+137), CINEXT)
               IX = DATA(IS+138)
               WRITE (MSGTXT,1714) CINEXT, IX
               CALL MSGWRT (5)
            ELSE IF (PRTASK.EQ.'VPLOT') THEN
               I = DATA(IS+130) + 0.1
               J = DATA(IS+131) + 0.1
               II = DATA(IS+133) + 0.1
               IX = DATA(IS+134) + 0.1
               IY = DATA(IS+135) + 0.1
               IV = IROUND (DATA(IS+139))
               IF (IV.EQ.1) THEN
                  WRITE (MSGTXT,1711) TIME, 'X: spectral channel',
     *            ' Y: polarization'
                  CALL MSGWRT (5)
                  WRITE (MSGTXT,1715) I, J, II
                  CALL MSGWRT (5)
               ELSE IF (IV.EQ.2) THEN
                  WRITE (MSGTXT,1711) TIME, 'X: spectral window',
     *            ' Y: polarization'
                  CALL MSGWRT (5)
                  WRITE (MSGTXT,1713) IX, IY
                  CALL MSGWRT (5)
               ELSE IF (IV.EQ.3) THEN
                  WRITE (MSGTXT,1711) TIME, 'X: spectral window',
     *            ' Y: polarization'
                  CALL MSGWRT (5)
                  WRITE (MSGTXT,1712) IX, IY, I, J, II
                  CALL MSGWRT (5)
               ELSE
                  WRITE (MSGTXT,1711) TIME, ' X: nothing',
     *               ' Y: polarization'
                  CALL MSGWRT (5)
                  END IF
            ELSE IF (PRTASK.EQ.'ANBPL') THEN
               CALL H2CHR (4, 1, DATA(IS+140), DTEMP)
               IX = DATA(IS+134) + 0.1
               IY = DATA(IS+135) + 0.1
               IF (DTEMP.EQ.'ALST') THEN
                  WRITE (MSGTXT,1711) TIME, ' X: nothing',
     *               ' Y: polarization'
                  CALL MSGWRT (5)
               ELSE IF (DTEMP.EQ.'ALIF') THEN
                  WRITE (MSGTXT,1711) TIME, ' X: spectral window',
     *               ' Y: nothing'
                  CALL MSGWRT (5)
                  WRITE (MSGTXT,1713) IX, IY
                  CALL MSGWRT (5)
               ELSE
                  WRITE (MSGTXT,1711) TIME, ' X: spectral window',
     *               ' Y: polarization'
                  CALL MSGWRT (5)
                  WRITE (MSGTXT,1713) IX, IY
                  CALL MSGWRT (5)
                  END IF
            ELSE IF (PRTASK.EQ.'SNIFS') THEN
               CALL H2CHR (4, 1, DATA(IS+140), DTEMP)
               IF (DTEMP.EQ.'ALAN') THEN
                  WRITE (MSGTXT,1711) TIME, ' X: antenna number',
     *               ' Y: polarization'
                  CALL MSGWRT (5)
               ELSE
                  WRITE (MSGTXT,1711) TIME,
     *               ' X: time in 1st SOLINT',
     *               ' Y: polarization'
                  CALL MSGWRT (5)
                  END IF
            ELSE IF ((PRTASK.EQ.'BPEDT') .OR. (PRTASK.EQ.'PCEDT')) THEN
               DO 712 J = 1,4
                  IT1(J) = DATA(IS+142+J) + 0.01
                  IT2(J) = DATA(IS+146+J) + 0.01
 712              CONTINUE
               IF ((DATA(IS+143).LT.0.0) .AND. (DATA(IS+147).GT.100))
     *            THEN
                  WRITE (MSGTXT,1711) TIME, '  Full time range plotted'
               ELSE
                  WRITE (MSGTXT,1716) TIME, IT1, IT2
                  END IF
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       UVHOL
 720     CONTINUE
C                                       pick up timerange
C                                       antennas, baseline
            J1 = 0
            J2 = 0
            DO 721 J = 1,50
               IF (J.LE.4) THEN
                  IT1(J) = DATA(IS+132+J) + 0.01
                  IT2(J) = DATA(IS+136+J) + 0.01
                  END IF
               IANT1(J) = DATA(IS+140+J) + 0.01
               IF (IANT1(J).GT.0) J1 = J
               IANT2(J) = DATA(IS+190+J) + 0.01
               IF (IANT2(J).GT.0) J2 = J
 721           CONTINUE
            WRITE (MSGTXT,1720) IVER, DATE, IT1, IT2
            CALL MSGWRT (5)
            J = 2
            IF (DATA(IS+337+7).GT.0.0) J = 4
            IF (DATA(IS+348+7).GT.0.0) J = J + 1
            IF ((DATA(IS+348+6).GT.0.0) .AND. (J.EQ.2)) J = 1
            WRITE (MSGTXT,1721) TIME, UVHOLC(J), DATA(IS+372),
     *         DATA(IS+373)
            CALL MSGWRT (5)
            J1 = MIN (J1, 8)
            J2 = MIN (J2, 8)
            WRITE (MSGTXT,1722) 'Reference', (IANT1(J), J = 1,J1)
            CALL MSGWRT (5)
            WRITE (MSGTXT,1722) 'Moving   ', (IANT2(J), J = 1,J2)
            CALL MSGWRT (5)
            CALL H2CHR (4, 1, DATA(IS+129), DTEMP)
C                                       npoints
            IF (DATA(IS+338).LE.0.0) THEN
               J1 = DATA(IS+337) + 0.01
               WRITE (MSGTXT,1723) DTEMP, J1
               IF (DATA(IS+337+6).GT.0.0) THEN
                  MSGTXT(41:) = 'averaged'
               ELSE
                  MSGTXT(41:) = 'plotted'
                  END IF
               CALL MSGWRT (5)
            ELSE
               J1 = DATA(IS+337+2) + 0.1
               J2 = DATA(IS+337+3) + 0.1
               WRITE (MSGTXT,1724) DTEMP, J1, J2
               IF (DATA(IS+337+6).GT.0.0) THEN
                  MSGTXT(52:) = 'then average'
               ELSE
                  MSGTXT(52:) = 'then plot'
                  END IF
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       PBEAM
 730     CONTINUE
C                                       type of plot
            JJ = DATA(IS+40+18) + 0.1
            WRITE (MSGTXT,1730) IVER, DATE, PBEAMC(JJ)
            CALL MSGWRT (5)
C                                       time, power cutoff
            WRITE (MSGTXT,1731) TIME, DATA(IS+40+1)
            CALL MSGWRT (5)
C                                       infile
            CALL H2CHR (48, 1, DATA(IS+5), FILNAM)
            J = JTRIM (FILNAM)
            J = MIN (44, J)
            WRITE (MSGTXT,1732) 'INfile ', FILNAM(:J)
            CALL MSGWRT (5)
            CALL H2CHR (48, 1, DATA(IS+17), FILNAM)
            J = JTRIM (FILNAM)
            IF (J.GT.0) THEN
               J = MIN (44, J)
               WRITE (MSGTXT,1732) 'IN2file', FILNAM(:J)
               CALL MSGWRT (5)
               IF (DATA(IS+40+6).LT.0.01) THEN
                  WRITE (MSGTXT,1733) 'combined'
               ELSE
                  WRITE (MSGTXT,1733) 'separately'
                  END IF
               CALL MSGWRT (5)
               END IF
            IF (DATA(IS+40+5).LT.-0.1) THEN
               MSGTXT = ' '
               MSGTXT(13:) = 'Do not exclude data for phase'
            ELSE
               WRITE (MSGTXT,1734) DATA(IS+40+5)
               END IF
            CALL MSGWRT (5)
            J = DATA(IS+40+4) + 0.1
            J = MIN (J, 6)
C                                       fit answers
            JJ = MIN (4, J)
            WRITE (MSGTXT,1735) (DATA(IS+40+20+I), I = 1,JJ)
            CALL MSGWRT (5)
            IF (JJ.LT.J) THEN
               WRITE (MSGTXT,1735) (DATA(IS+40+20+I), I = JJ+1,J)
               MSGTXT(13:16) = ' '
               CALL MSGWRT (5)
               END IF
            WRITE (MSGTXT,1736) DATA(IS+40+27), DATA(IS+40+28)
            CALL MSGWRT (5)
            IF ((DATA(IS+40+29).NE.0.0) .OR. (DATA(IS+40+30) .NE.0.0))
     *         THEN
               WRITE (MSGTXT,1737) DATA(IS+40+29), DATA(IS+40+30)
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       PEEK
 740     CONTINUE
C                                       antenna, ampl, wind
            JJ = DATA(IS+151) + 0.1
            WRITE (MSGTXT,1740) IVER, DATE, JJ, DATA(IS+64+5),
     *         DATA(IS+64+6)
            CALL MSGWRT (5)
C                                       time, elevation range
            WRITE (MSGTXT,1741) TIME, DATA(IS+64+3), DATA(IS+64+4)
            CALL MSGWRT (5)
C                                       beamwidth range
            WRITE (MSGTXT,1742) DATA(IS+64+7), DATA(IS+64+8)
            CALL MSGWRT (5)
C                                       date/time range
            WRITE (MSGTXT,1743) DATA(IS+64+1)+DATA(IS+56+1),
     *         DATA(IS+64+2)+DATA(IS+56+5)
            CALL MSGWRT (5)
C                                       filter out
            WRITE (MSGTXT,1744) DATA(IS+64+9), DATA(IS+64+10)
            CALL MSGWRT (5)
C                                       solve for
            MSGTXT = ' '
            MSGTXT(23:) = 'Solve for '
            JJ = 33
            DO 741 I = 1,11
               IF (DATA(IS+75+I).GT.0.0) THEN
                  MSGTXT(JJ:) = PEEKS(I)
                  JJ = JTRIM (MSGTXT) + 2
                  IF (JJ.GT.59) THEN
                     CALL MSGWRT (5)
                     JJ = 33
                     MSGTXT(JJ:) = ' '
                     END IF
                  END IF
 741           CONTINUE
            IF (JJ.GT.33) CALL MSGWRT (5)
            GO TO 890
C                                       LOCIT
 750     CONTINUE
C                                       BIF, EIF, STOKES
            J = DATA(IS+139) + 0.1
            JJ = DATA(IS+140) + 0.1
            IX = IROUND (DATA(IS+217+8))
            CALL H2CHR (4, 1, DATA(IS+141), DTEMP)
            WRITE (MSGTXT,1750) IVER, DATE, J, JJ, DTEMP, IX
            CALL MSGWRT (5)
            JJ = 0
            DO 751 J = 1,50
               IANT1(J) = DATA(IS+141+J) + 0.1
               IF (IANT1(J).GT.0) JJ = J
 751           CONTINUE
            WRITE (MSGTXT,1751) TIME, (IANT1(J), J = 1,MIN(8,JJ))
            CALL MSGWRT (5)
            MSGTXT = ' '
            JJ = 21
            IF (DATA(IS+193+1).GT.0.0) THEN
               MSGTXT(JJ:) = 'Scan difference method used'
               JJ = JJ + 29
               END IF
            IF (DATA(IS+193+2).GT.0.0) THEN
               MSGTXT(JJ:) = 'K term omitted'
               JJ = JJ + 16
            ELSE
               MSGTXT(JJ:) = 'K term FIT'
               JJ = JJ + 12
               END IF
            IF (DATA(IS+193+9).GT.0.0) THEN
               IF (JJ.GT.44) THEN
                  CALL MSGWRT (5)
                  JJ = 21
                  END IF
               MSGTXT(JJ:) = 'IF differences used'
               JJ = JJ + 21
               END IF
            IF (DATA(IS+193+10).GT.0.0) THEN
               MSGTXT(JJ:) = 'plus phase slope'
               JJ = JJ + 16
               END IF
            IF (JJ.GT.21) CALL MSGWRT (5)
            IF (DATA(IS+193+4).GT.DATA(IS+193+3)) THEN
               WRITE (MSGTXT,1752) 'Elevation range  ', DATA(IS+193+3),
     *            DATA(IS+193+4)
               CALL MSGWRT (5)
               END IF
            IF (DATA(IS+193+6).GT.DATA(IS+193+5)) THEN
               WRITE (MSGTXT,1752) 'Hour angle range ', DATA(IS+193+5),
     *            DATA(IS+193+6)
               CALL MSGWRT (5)
               END IF
            IF (DATA(IS+193+8).GT.DATA(IS+193+7)) THEN
               WRITE (MSGTXT,1752) 'Declination range', DATA(IS+193+7),
     *            DATA(IS+193+8)
               CALL MSGWRT (5)
               END IF
            GO TO 890
C                                       unknown type
 790     CONTINUE
            WRITE (MSGTXT,1890) IVER, DATE, IBLC
            CALL MSGWRT (5)
            WRITE (MSGTXT,1891) LTASK, TIME, ITRC
            CALL MSGWRT (5)
            GO TO 890
C                                       Slice files.
 800     CONTINUE
C                                       record 2 = task parms
            CALL ZFIO ('READ', IELUN, IFIND, 2, IDATA, IERR)
            IF (IERR.NE.0) GO TO 885
            IDATA(4) = -IDATA(4)
            CALL TIMDAT (IDATA(7), IDATA(4), TIME, DATE)
C                                       new or old format 1/2009
            CALL H2CHR (4, 1, DATA(33), CODET)
            IF ((CODET.EQ.'AVER') .OR. (CODET.EQ.'FLUX') .OR.
     *         (CODET.EQ.'LGFL') .OR. (CODET.EQ.'LNFL') .OR.
     *         (CODET.EQ.'ADER') .OR. (CODET.EQ.'FDER') .OR.
     *         (CODET.EQ.'LGAV') .OR. (CODET.EQ.'LNAV')) THEN
               ISOLD = .FALSE.
            ELSE
               ISOLD = .TRUE.
               END IF
            ISX = IS + 8
            IF ((DATA(ISX).LT.-100.) .OR. (DATA(ISX+1).LT.-100.) .OR.
     *         (DATA(ISX+7).GT.90000.) .OR. (DATA(ISX+8).GT.90000.))
     *         GO TO 880
            ABLC(1) = DATA(ISX)
            ABLC(2) = DATA(ISX+1)
            ATRC(1) = DATA(ISX+7)
            ATRC(2) = DATA(ISX+8)
            XMIN = DATA(IS+23)
            XMAX = DATA(IS+24)
            CALL H2CHR (6, 1, IDATA, LTASK)
            IF (ISOLD) THEN
               CODET = 'AVER'
            ELSE
               CALL H2CHR (4, 1, DATA(IS+22), CODET)
               END IF
            CALL H2CHR (8, 1, CATH(KHBUN), ATEMP)
            IF (CODET.EQ.'FLUX') ATEMP = 'JY'
C                                       Get no. of points in rec 1.
            CALL ZFIO ('READ', IELUN, IFIND, 1, IDATA, IERR)
            IF (IERR.NE.0) GO TO 885
            INOPTS = IDATA(57)
C                                       Write line.
            WRITE (MSGTXT,1800) IVER, DATE, ABLC, XMIN, INOPTS, LTASK
            CALL MSGWRT (5)
            WRITE (MSGTXT,1802) TIME, ATRC, XMAX, ATEMP
            CALL MSGWRT (5)
C                                       List slice models if present.
            INGM = IDATA(58)
            IF (INGM.EQ.0) GO TO 815
               IFIRST = IDATA(59)
               DO 810 IGM = 1,INGM
                  IRNO = IFIRST + IGM - 1
                  CALL ZFIO ('READ', IELUN, IFIND, IRNO, IDATA, IERR)
                  IF (IERR.NE.0) GO TO 815
                  IS = 25
                  IDATA(IS+3) = -IDATA(IS+3)
                  CALL TIMDAT (IDATA(IS+6), IDATA(IS+3), TIME, DATE)
                  IP = IDATA(IS+2)
                  IF (.NOT.ISOLD) THEN
                     WRITE (MSGTXT,1804) DATE, IGM, DATA(46)
                     CALL MSGWRT (5)
                     CALL H2CHR (16, 1, DATA(75), RESULT)
                     CALL H2CHR (16, 1, DATA(79), CTEMP)
                     JJ = 50
                     IF ((JTRIM(RESULT).GT.0) .AND. (JTRIM(CTEMP).GT.0))
     *                  THEN
                        DO 805 I = 1,IP
                           WRITE (MSGTXT,1806) I, 'Peak', DATA(JJ+1),
     *                        DATA(JJ+2), RESULT(:16)
                           IF (I.EQ.1) MSGTXT(6:13) = TIME
                           CALL MSGWRT (5)
                           WRITE (MSGTXT,1807) 'Pos ', DATA(JJ+3),
     *                        DATA(JJ+4), CTEMP(:16)
                           CALL MSGWRT (5)
                           WRITE (MSGTXT,1807) 'FWhm', DATA(JJ+5),
     *                        DATA(JJ+6), CTEMP(:16)
                           CALL MSGWRT (5)
                           JJ = JJ + 6
 805                       CONTINUE
                        JJ = 83
                        IF ((DATA(JJ).NE.0.0) .OR. (DATA(JJ+3).GT.0.0))
     *                     THEN
                           WRITE (MSGTXT,1808) 'Constant ', DATA(JJ),
     *                        DATA(JJ+3)
                           CALL MSGWRT (5)
                           JJ = JJ + 1
                           IF ((DATA(JJ).NE.0.0) .OR.
     *                        (DATA(JJ+3).GT.0.0)) THEN
                              WRITE (MSGTXT,1808) 'Slope    ', DATA(JJ),
     *                           DATA(JJ+3)
                              CALL MSGWRT (5)
                              JJ = JJ + 1
                              IF ((DATA(JJ).NE.0.0) .OR.
     *                           (DATA(JJ+3).GT.0.0)) THEN
                                 WRITE (MSGTXT,1808) 'Curvature',
     *                              DATA(JJ), DATA(JJ+3)
                                 CALL MSGWRT (5)
                                 END IF
                              END IF
                           END IF
                        END IF
C                                       old format no physical units
                  ELSE
                     WRITE (MSGTXT,1804) DATE, IGM
                     CALL MSGWRT (5)
                     JJ = 0
                     RESULT = '????'
                     CALL H2CHR (8, 1, CATH(KHBUN), RESULT(6:))
                     CTEMP = 'Slice points'
                     DO 808 I = 1,IP
                        WRITE (MSGTXT,1806) I, 'Peak', DATA(JJ+1),
     *                     DATA(JJ+12), RESULT(:16)
                        IF (I.EQ.1) MSGTXT(6:13) = TIME
                        CALL MSGWRT (5)
                        WRITE (MSGTXT,1807) 'Pos ', DATA(JJ+2),
     *                     DATA(JJ+13), CTEMP(:16)
                        CALL MSGWRT (5)
                        WRITE (MSGTXT,1807) 'FWhm', DATA(JJ+3),
     *                     DATA(JJ+14), CTEMP(:16)
                        CALL MSGWRT (5)
                        JJ = JJ + 3
 808                    CONTINUE
                     END IF
 810              CONTINUE
C                                       Close slice file.
 815           CALL ZCLOSE (IELUN, IFIND, IERR)
               IFIND = 0
               GO TO 890
C                                       Tables (and CC) extensions
 830     CONTINUE
            MSGSUP = 32000
            NKEY = 0
            NCOL = 0
            NREC = 0
            CALL TABINI ('READ', IECHAR, IVOL, ISLOT, IVER, CATBLK,
     *          IELUN, NKEY, NREC, NCOL, HREC, IDATA, IERR)
            MSGSUP = 0
            IF (IERR.NE.0) GO TO 890
C                                       creation date/task
            CALL TIMDAT (IDATA(14), IDATA(11), TIME, DATE)
            CALL H2CHR (6, 1, IDATA(29), LTASK)
            WRITE (MSGTXT,1830) IVER, LTASK, DATE, TIME
            CALL MSGWRT (5)
C                                       last write date/task
            CALL TIMDAT (IDATA(36), IDATA(33), TIME, DATE)
            CALL H2CHR (6, 1, IDATA(39), LTASK)
            WRITE (MSGTXT,1831) LTASK, DATE, TIME
            CALL MSGWRT (5)
C                                       size, sort order
            IRNO = IDATA(5)
            CTEMP = ' '
            IF (IDATA(43).GT.0) II = 1
            IF (IDATA(44).GT.0) JJ = 1
            IF (IDATA(43).LT.0) II = 3
            IF (IDATA(44).LT.0) JJ = 3
            IF (IDATA(43).GT.256) II = 2
            IF (IDATA(44).GT.256) JJ = 2
            IF (IDATA(43).LT.-256) II = 4
            IF (IDATA(44).LT.-256) JJ = 4
            I = ABS(IDATA(43))
            IF (I.GT.256) I = I - 256
            J = ABS(IDATA(44))
            IF (J.GT.256) J = J - 256
            IF (I.GT.0) THEN
               WRITE (ATEMP,1832) I, SORTOR(II)
               CTEMP(1:12) = ATEMP
               END IF
            IF (J.GT.0) THEN
               WRITE (ATEMP,1832) J, SORTOR(JJ)
               CTEMP(14:25) = ATEMP(1:12)
               END IF
            WRITE (MSGTXT,1833) NCOL, IRNO, CTEMP(1:25)
            CALL MSGWRT (5)
C                                       selection strings
            IY = 0
            IF (IDATA(61).GT.0) THEN
               NS = IDATA(61)
               DO 835 I = 1,NS
                  IRNO = I
                  CALL TABIO ('READ', 2, IRNO, HREC, IDATA, IERR)
                  IF (IERR.NE.0) GO TO 845
                  IS = IDATA(63+I) - IDATA(62+I)
                  IF (I.EQ.NS) IS = IDATA(62) - IDATA(62+I)
                  IF (IS.GT.16) IS = 16
                  IS = 4 * IS
                  CALL H2CHR (IS, 1, HREC, RESULT)
                  WRITE (MSGTXT,1834) RESULT(1:IS)
                  CALL MSGWRT (5)
 835              CONTINUE
               END IF
C                                       column headings
 840        IX = IY + 1
            IF (IX.LE.NCOL) THEN
               IY = IX + 4
               IF (IY.GT.NCOL) IY = NCOL
               IS = 15
               WRITE (MSGTXT,1840)
               DO 842 I = IX,IY
                  IRNO = I
                  CALL TABIO ('READ', 3, IRNO, HREC, IDATA, IERR)
                  IF (IERR.NE.0) GO TO 845
                  CALL H2CHR (8, 1, HREC, RESULT)
                  MSGTXT(IS:) = RESULT(:8)
                  IS = IS + 10
 842              CONTINUE
               CALL MSGWRT (5)
               IS = 15
               WRITE (MSGTXT,1842)
               DO 844 I = IX,IY
                  IRNO = I
                  CALL TABIO ('READ', 4, IRNO, HREC, IDATA, IERR)
                  IF (IERR.NE.0) GO TO 845
                  CALL H2CHR (8, 1, HREC, RESULT)
                  MSGTXT(IS:) = RESULT(:8)
                  IS = IS + 10
 844              CONTINUE
               CALL MSGWRT (5)
               GO TO 840
               END IF
C                                       close table file
 845        CALL TABIO ('CLOS', 4, IRNO, HREC, IDATA, IERR)
            GO TO 890
C                                       Error reading extension file.
 880     WRITE (MSGTXT,1880) IVER
         CALL MSGWRT (8)
         GO TO 889
C                                       Error reading extension file.
 885     WRITE (MSGTXT,1885) IVER, IERR
         CALL MSGWRT (8)
C                                       Close file
 889     IF (IFIND.GT.0) CALL ZCLOSE (IELUN, IFIND, IERR)
 890     CONTINUE
      GO TO 999
C-----------------------------------------------------------------------
C                                       All error msg. handled locally.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1025 FORMAT ('NO EXTENSION FILES OF TYPE ',A2)
 1040 FORMAT ('EXTENSION TYPE ''',A2,''' NOT SUPPORTED BY EXTLIST')
 1060 FORMAT ('USE VERB PRTHI FOR HISTORY FILES')
 1061 FORMAT ('USE TASKS PRTGA, GNPLT FOR GAIN FILES')
 1062 FORMAT ('USE TASK PRTDR FOR DELAY/RATE FILES')
 1100 FORMAT ('Plot files:   user',I5,2X,'disk',I2)
 1102 FORMAT ('Ver type',4X,'Date/time BLC/TRC',5X,'Other information')
 1105 FORMAT ('Slice files:  user',I5,2X,'disk',I2)
 1107 FORMAT ('Ver  Date/time     BLC/TRC',9X,'Min/max',4X,'No pts')
 1110 FORMAT (A2,' files:',5X,'user',I5,2X,'disk',I2)
 1112 FORMAT ('Ver',17X,'task',8X,'date',8X,'time')
 1124 FORMAT ('VERSION',I4,' IS APPARENTLY BEING WRITTEN NOW')
 1155 FORMAT ('WARNING: NPARMS=',I4,
     *   ' not recognized size, plot too new?')
 1210 FORMAT (I3,' Misc    ',A9)
 1211 FORMAT (4X,A6,2X,A8)
 1220 FORMAT (I3,A2,'ontour ',A9,' (',I4,',',I4,') LEVS=',1PE11.4,' *')
 1221 FORMAT (12X,A8,2X,'(',I4,',',I4,')')
 1222 FORMAT (F6.0,',')
 1223 FORMAT (F6.1,',')
 1224 FORMAT (F6.2,',')
 1225 FORMAT (F6.3,',')
 1228 FORMAT (12X,'Star file version',I4,' plotted at factor',1PE11.4)
 1230 FORMAT (I3,' Grey sc ',A9,' (',I4,',',I4,') pxrange= ',1PE11.4)
 1231 FORMAT (12X,A8,2X,'(',I4,',',I4,')',10X,1PE11.4)
 1232 FORMAT (12X,A19,A12,'.',A6,'.',I4,3X,'disk',I2)
 1233 FORMAT (12X,'Levs= ',1PE11.4,' *(')
 1236 FORMAT (12X,'Slice versions',I4,' to',I4,' plotted')
 1237 FORMAT (12X,'BOX(',I2.2,') =',4I5,' plotted')
 1240 FORMAT (I3,' Profile ',A9,' (',I4,',',I4,') rot=',I3,' skew=',I3,
     *   ' dist=',I5)
 1242 FORMAT (12X,A8,2X,'(',I4,',',I4,') xinc=',I3,'  yinc=',I3,
     *   '  zxr=',F3.1)
 1250 FORMAT (I3,' Slice   ',A9,' (',I4,',',I4,') pxrange= ',1PE11.4,
     *   ' invrs=',I3)
 1260 FORMAT (I3,' Pol plt ',A9,' (',I4,',',I4,') factor',F8.2,' xinc',
     *   I3,' yinc',I3)
 1261 FORMAT (12X,A8,2X,'(',I4,',',I4,') icut',F10.5,'  pcut',F10.5)
 1263 FORMAT (12X,'Q and U Stokes taken from present data cube')
 1264 FORMAT (I3,' Pol plt ',A9,' (',I4,',',I4,')  No polarization ',
     *   'lines plotted')
 1266 FORMAT (12X,'rotate',I4,12X,'levs=',1PE11.4,' *')
 1270 FORMAT (I3,' Histogm ',A9,' (',I4,',',I4,') pxrange= ',1PE11.4,
     *   '  nbxs=',I3)
 1271 FORMAT (12X,A8,2X,'(',I4,',',I4,')',10X,1PE11.4,'  type=',A2)
 1280 FORMAT (I3,1X,A3,'plot ',A9,1X,11X,2X,'Stok ',A4,2X,'inc',I5,
     *   2X,A,I2)
 1281 FORMAT (12X,A8,2X,'xaxis: ',A8,1PE12.4,' to',1PE12.4)
 1282 FORMAT (22X,'yaxis: ',A8,1PE12.4,' to',1PE12.4)
 1283 FORMAT (22X,'yaxis: ',A8,' self-scaling')
 1284 FORMAT (12X,'times from ',A1,I2.2,'/',2(I2.2,':'),I2.2,' to ',A1,
     *   I2.2,'/',2(I2.2,':'),I2.2)
 1285 FORMAT (12X,'uvrange=',2(1PE12.4),' Kilo wavelengths')
 2285 FORMAT (12X,'spectral channels',I5,' to',I5,' by',I4,'   IFs',
     *   I3,' to',I3)
 1286 FORMAT (12X,'antenna',I3,' with',I3)
 1287 FORMAT (12X,'ants',16I3.2)
 1288 FORMAT (12X,'with',16I3.2)
 1289 FORMAT (12X,'plot binned into',I4,' bins')
 1310 FORMAT (I3,1X,A6,2X,A9,' (',I4,',',I4,') pxrange= ',1PE11.4)
 1330 FORMAT (I3,' PL cube ',A9,' (',I4,',',I4,') pxrange= ',1PE11.4)
 1332 FORMAT (12X,'X pixels:',I4,'-',I4,'    yinc',I3,'    zinc',I3)
 1340 FORMAT (I3,' Im v Im ',A9,' (',I4,',',I4,')  Xrange= ',1PE11.4)
 1341 FORMAT (12X,'Y-axis image: ',A12,'.',A6,'.',I4,3X,'disk',I2)
 1342 FORMAT (12X,'Yrange =      ',1PE11.4,' to ',1PE11.4)
 1343 FORMAT (12X,'X,Y,Z inc =   ',3I4,5X,'optype=''',A4,''' ',A2)
 1344 FORMAT (12X,'number X bins ',I4,5X,'Y bins ',I4)
 1345 FORMAT (12X,'symbol factor ',F6.2,5X,'X,Y offset ',2I4,' pixels')
 1350 FORMAT (I3,' Table   ',A9,'   row',I8,' to',I9,' by',I5)
 1351 FORMAT (12X,A8,4X,'table type: ',A2,', version',I4)
 1352 FORMAT (12X,A8,4X,'table type: ',A2,', version',I4,' Xbins',I4)
 1353 FORMAT (12X,A1,'axis:')
 1356 FORMAT (12X,A1,'range =',4X,1PE11.4,' to ',1PE11.4)
 1357 FORMAT (12X,'Row selection string ''',A16,'''')
 1358 FORMAT (12X,'Select row',I4,'(',I3,') =',1PE12.4,' +-',1PE12.4)
 1360 FORMAT (I3,' Spectra ',A9,' Stokes ',A5)
 1361 FORMAT (12X,A8,2X,A8,2X,1PE12.4,' to',1PE12.4)
 1362 FORMAT (22X,A8,2X,1PE12.4,' to',1PE12.4)
 1363 FORMAT (12X,'Divide by avg of chans',I5,' to',I5,' if >',
     *   1PE12.4)
 1364 FORMAT (12X,'Channel range',I4,I5,'  IF range',I2,I3,2X,A)
 1365 FORMAT (12X,'Frequency smoothing parameters',3F4.0)
 1366 FORMAT (12X,'Shift phase by',F9.4,',',F9.4,' arcsec in RA, dec')
 1367 FORMAT (12X,'XYratio=',F7.3)
 1370 FORMAT (I3,1X,A2,' plot ',A9,' of vers',I4,'  optype ',A4,
     *   '  opcode ',A4)
 1371 FORMAT (12X,A8,2X,'Pixrange = ',1PE11.4,' to ',1PE11.4)
 1372 FORMAT (12X,A8,2X,'each plot is self-scaled')
 1373 FORMAT (12X,'Blanked values ',A,'   scan boundaries ',A)
 1374 FORMAT (12X,'Plot type ',A4,2X,'Pixrange  ',1PE11.4,' to ',
     *   1PE11.4)
 1375 FORMAT (12X,'Plot type ',A4,2X,'each plot is self-scaled')
 1381 FORMAT (12X,'Planes',I4,' through',I4,' by',I4)
 1382 FORMAT (12X,'Grey-scale image: ',A12,'.',A6,'.',I4,3X,'disk',I2)
 1383 FORMAT (12X,'Grey-scale pixrange=',2(1PE11.4),'  type=',A2)
 1390 FORMAT (I3,1X,'UVhisto ',A9,' chans',2I5,'  IFs',I3,I4,
     *   '   Stokes ',A)
 1391 FORMAT (12X,A8,2X,'Plot ',A10,' versus ',A14)
 1392 FORMAT (12X,'Histogram from',1PE11.3,' to',1PE11.3,1X,A)
 1393 FORMAT (12X,'Number of bins',I5,'  Max bin value',1PE11.3)
 1394 FORMAT (12X,'Subarray =',I3,2X,'FreqID =',I3)
 1395 FORMAT (12X,'Subarray =',I3,I4,' FreqIDs')
 1396 FORMAT (12X,I2,' Subarrays',2X,'FreqID =',I3)
 1397 FORMAT (12X,I2,' Subarrays',I5,' FreqIDs')
 1400 FORMAT (I3,' Spectrm ',A9,' (',I4,',',I4,') pxrange= ',1PE11.4,
     *   ' zinc=',I3)
 1402 FORMAT (12X,'Planes',I6,' through',I6,12X,'xyratio=',F5.2)
 1403 FORMAT (12X,'OPTYPE = ''',A4,'''')
 1410 FORMAT (I3,1X,A2,' plot ',A9,1X,11X,2X,'Stokes ',A5,2X,'inc',I4,
     *   ' avg',F4.1)
 1411 FORMAT (12X,'spectral channels ',I5,' to',I5,'    IFs',I4,' to',
     *   I4)
 1412 FORMAT (12X,5X,' :',7(I3.2,'-',I2.2))
 1415 FORMAT (12X,'model image:   ',A12,'.',A6,'.',I4,3X,'disk',I2)
 1416 FORMAT (12X,'version',I4,6X,'# comps',I7)
 1424 FORMAT (12X,8X,' :',10(I4.2))
 1430 FORMAT (I3,' DFT plt ',A9,2X,A4,' plot range',2(1PE12.4))
 1431 FORMAT (12X,A8,'  times from ',A1,I2.2,'/',2(I2.2,':'),I2.2,
     *   ' to ',A1,I2.2,'/',2(I2.2,':'),I2.2)
 1432 FORMAT (12X,'shift =',2(1PE12.4),' arc sec')
 1433 FORMAT (12X,'Subarray =',I3,2X,'FreqID =',I3,2X,'Flagver =',I3)
 1434 FORMAT (12X,'IFs',I3,' -',I3,2X,'Channels',I6,' -',I6)
 1440 FORMAT (I3,1X,'FR plot ',A9,1X,11X,2X,'Stokes ',A4,' show')
 1441 FORMAT (12X,A8,'  amplitude range',2(1PE12.4))
 1442 FORMAT (22X,'phase range',2F7.1,4X,'time avg',F5.1)
 1443 FORMAT (12X,'Divide by avg of chans',I5,' to',I5)
 1444 FORMAT (12X,'shift =',2(1PE12.4),' arc sec')
 1450 FORMAT (I3,1X,'FR map  ',A9,1X,11X,2X,'Stokes ',A4,'  w')
 1451 FORMAT (12X,A1,'-axis field',F7.1,'  incr',F7.3,'  offset',F6.1,
     *   ' masec')
 1452 FORMAT (12X,'Averaging period',F7.1,' interval',F7.1,' minutes')
 1453 FORMAT (12X,'Threshold',F4.1,' sigma, accuracy',F6.2,
     *   ' masec, iter',I5)
 1454 FORMAT (12X,'Lines: plotted',I6,' required',F6.2,' terminate',
     *   F6.2)
 1460 FORMAT (I3,1X,A6,2X,A9,' Xaxis range',2(1PE11.4))
 1461 FORMAT (12X,A8,'  Yaxis range',2(1PE11.4))
 1462 FORMAT (12X,'Point(Symbol) types',5(I2,'(',I2,')'))
 1463 FORMAT (12X,'Point(Line  ) types',5(I2,'(',I2,')'))
 1464 FORMAT (12X,'Point(fit   ) order',5(I2,'(',I2,')'))
 1465 FORMAT (F7.0)
 1466 FORMAT (F7.2)
 1467 FORMAT (F7.4)
 1478 FORMAT (12X,'Ofmfile=''',A)
 1482 FORMAT (12X,'Polarization vectors rotated by',F6.1,' degrees')
 1490 FORMAT (I3,A2,'ontour ',A9,' (',I4,',',I4,')')
 1492 FORMAT (12X,'Polarization vectors drawn: length factor=',F8.2)
 1493 FORMAT (12X,'Xinc',I3,'  yinc',I3,'    icut',F10.5,'   pcut',
     *   F10.5)
 1500 FORMAT (I3,' BP plot ',A9,2X,A,' BP vers',I3,'  stokes ',A4)
 1501 FORMAT (12X,A8,2X,A4,' Pixrange = ',1PE11.4,' to ',1PE11.4)
 1510 FORMAT (I3,' APCAL   ',A9,2X,'OPCODE=''',A4,'''  BFACT=',F6.2,
     *   '  ZAlim=',F5.1)
 1511 FORMAT (12X,'INFILE = ''',A)
 1512 FORMAT (12X,'Plot',I2,' ant',I3,A1,'  Trecvr=',F7.2,'  Tau=',
     *   F6.3,'  Dofit=',I1)
 1530 FORMAT (I3,' IRING   ',A9,' (',I4,',',I4,') ',A)
 1531 FORMAT (12X,A8,2X,'(',I4,',',I4,') Dx,Dy',2F9.3,' pixels')
 1532 FORMAT (12X,'PA=',F7.2,' Incl=',F6.2,' Az rng=',F6.1,' Az cent=',
     *   F7.2)
 1533 FORMAT (12X,'X axis is ''',A,''' = asec *',1PE11.4)
 1534 FORMAT (12X,'Rings ',F7.1,' : ',F7.1,' by ',F6.1,2X,A)
 1535 FORMAT (12X,'Symbol',I4,' Factor=',F7.3,5X,A)
 1550 FORMAT (I3,' RFLAG   ',A9,4X,A)
 1551 FORMAT (12X,A8,2X,'Xrange',2F9.3,1X,A,' in',I6,' boxes')
 1552 FORMAT (12X,A8,2X,'Nchan ',I7,4X,'IFs',I3,' through',I3)
 1553 FORMAT (12X,'Yrange',2(1PE11.3),4X,'Stokes ',A4)
 1554 FORMAT (12X,'Time rms over',I3,' samples <',F6.1,' seconds')
 1555 FORMAT (12X,'Spectral median window',I4,' channels wide')
 1563 FORMAT (12X,A2,' version',I4,' comps scaled by',F9.3)
 1564 FORMAT (12X,A2,' version',I4,' comps plotted at size',F8.2)
 1565 FORMAT (12X,A2,' version',I4,' stars scaled by',F9.3)
 1566 FORMAT (12X,I5,' comps, symbols 0-',I2,' fluxes',2(1PE10.3))
 1567 FORMAT (12X,'symbols 0-',I2,' over flux range',2(1PE10.3))
 1568 FORMAT (12X,'Limited to',I6,' comps with symbol ',I2)
 1570 FORMAT (I3,' Search  ',A9,' (',2(I4,','),I4,')  Yinc',I3)
 1571 FORMAT (12X,A8,2X,'(',2(I4,','),I4,')  Zinc',I3)
 1572 FORMAT (12X,'Widths from',I3,' to',I3,' by',I3)
 1573 FORMAT (12X,'Outtext ''',A,'''')
 1580 FORMAT (I3,' Eval uv ',A9)
 1581 FORMAT (12X,A8,2X,A1,'range',2F10.4,' Jy')
 1582 FORMAT (12X,A8,2X,A1,'range',2F10.3,' Jy')
 1583 FORMAT (12X,A8,2X,A1,'range',2F10.2,' Jy')
 1584 FORMAT (12X,A8,2X,A1,'range',2F10.1,' Jy')
 1585 FORMAT (12X,A8,2X,A1,'range',2F10.0,' Jy')
 1586 FORMAT (12X,'Model image: ',A12,'.',A6,'.',I5,3X,'disk',I2)
 1587 FORMAT (12X,'Image size',I5,' pixels, smoothed by',I3,' pixels')
 1588 FORMAT (12X,'SMODEL =',3F10.5)
 1590 FORMAT (I3,' DELZN   ',A9,' snver',I3,' gainver',I3)
 1591 FORMAT (12X,A8,2X,'Plot of')
 1592 FORMAT (12X,'IFs',I3,' to',I3,'   subarray',I3)
 1593 FORMAT (12X,'Use ',A4,' data, plot')
 1594 FORMAT (12X,I2,' plots/page   every',I3,' point plotted')
 1600 FORMAT (I3,' ELINT   ',A9,' snver',I3,' gainver',I3,'  nplots',I3)
 1601 FORMAT (12X,A8,2X,'IFs',I3,' -',I3,'  subarray',I3)
 1610 FORMAT (I3,' Flags   ',A9,'  flagver',I3,'  stokes ',A)
 1611 FORMAT (12X,A8,2X,'IFs',I3,' -',I3,'  channels',I6,' -',I6)
 1620 FORMAT (I3,' Galaxy  ',A9,' (',I4,',',I4,')  function type:')
 1621 FORMAT (12X,A8,2X,'(',I4,',',I4,')  annuli',F5.1,' pixels')
 1622 FORMAT (12X,'Weight image: ',A12,'.',A6,'.',I5,3X,'disk',I2)
 1623 FORMAT (12X,'Radii',2F8.2,'  velocities',F7.1,F9.1)
 1624 FORMAT (12X,'Cos(pa)',2F7.3,'  Sin(pa)',2F7.3)
 1625 FORMAT (12X,'Fit center',F7.2,',',F7.2,'  systemic velocity',
     *   F8.1)
 1626 FORMAT (12X,'Fit Position angle',F8.2,'  inclination',F7.2)
 1630 FORMAT (I3,1X,A2,' IFpl ',A9,' of vers',I4,'  optype ',A4,
     *   '  opcode ',A4)
 1631 FORMAT (12X,'Solint',F8.1,' sec   plots/page',I2)
 1640 FORMAT (I3,' Sour SP ',A9,11X,'for source ',A)
 1641 FORMAT (12X,A8,2X,' calcode ',A4,'  order',I3)
 1642 FORMAT (12X,'Fit sp index',F7.3,' curvature',3F7.3)
 1643 FORMAT (12X,'Fit flux',F9.3,' at 1 GHz, rms',F7.3)
 1644 FORMAT (12X,'Perhaps corrected SN table versions',I3,' -',I3)
 1650 FORMAT (I3,' Weather ',A9,' table invers',I3)
 1651 FORMAT (12X,A8,2X,I2,' plots/page   every',I3,' point plotted')
 1652 FORMAT (12X,'Plot range',2F9.3)
 1653 FORMAT (12X,'Plot ',A,' only')
 1660 FORMAT (I3,1X,A2,' plot ',A9,'  RA,Dec pixel ',2I6,'  table vers',
     *   I4)
 1661 FORMAT (12X,A8,3X,A1,' image  ',A12,'.',A6,'.',I4)
 1662 FORMAT (12X,'Average over circle of radius',I3,' pixels')
 1663 FORMAT (12X,'Average over rectangle widths',2I3,' pixels')
 1664 FORMAT ('  range',F8.3,F7.3)
 1665 FORMAT (12X,'ZE table vers',I4)
 1666 FORMAT (12X,'Chans',2I7,'  X axis ',A,2X,A1,' frac',F5.2)
 1680 FORMAT (I3,' Allan Var ',A8,'  IFs',I3,' -',I3,'  subarray',I3,
     *   '  Stokes ',A)
 1681 FORMAT (12X,A8,2X,'Plot ')
 1682 FORMAT ('Pre-average time',F7.1,'  FuncType ',A2)
 1683 FORMAT (12X,'Channels',I6,' -',I6,'  Plots/page',I2)
 1690 FORMAT (I3,' PC plot ',A8,'  IFs',I3,' -',I3,'  subarray',I3,
     *   '  Stokes ',A)
 1691 FORMAT (12X,A8,2X,'Plot type ',A,'  Sort ',A,'  PC version',I3)
 1700 FORMAT (I3,' RowPlot ',A9,' PixRange',2F10.4)
 1701 FORMAT (12X,A8,'  Every',I3,' row  Offset',F10.4)
 1702 FORMAT (12X,'BLC',2I6,'  TRC',2I6,'  Plane',I3)
 1710 FORMAT (I3,' PlotCol ',A9,'  PRTASK = ''',A,'''')
 1711 FORMAT (12X,A8,3X,A,A)
 1712 FORMAT (12X,'BIF',I3,' EIF',I3,' BCHAN',I5,' ECHAN',I5,
     *   ' CHINC',I3)
 1713 FORMAT (12X,'BIF',I3,' EIF',I3)
 1714 FORMAT (12X,'From INEXT ''',A2,'''  VERSION',I5)
 1715 FORMAT (12X,'BCHAN',I5,' ECHAN',I5,' CHINC',I3)
 1716 FORMAT (12X,A8,3X,'TimeRange=',I2,'/',2(I2.2,':'),I2.2,' -',I2,
     *   '/',2(I2.2,':'),I2.2)
 1720 FORMAT (I3,' UVHOL   ',A9,' Time range',I3,'/',2(I2.2,':'),I2.2,
     *   ' -',I3,'/',2(I2.2,':'),I2.2)
 1721 FORMAT (12X,A8,2X,A,' Plot range',2F9.3)
 1722 FORMAT (12X,A,' antennas',8I4)
 1723 FORMAT (12X,'Stokes ''',A,'''   Npoints=',I3)
 1724 FORMAT (12X,'Stokes ''',A,'''   drop',I3,' and',I3,' samples')
 1730 FORMAT (I3,' PBEAM   ',A9,'  PLOT ',A)
 1731 FORMAT (12X,A8,'   Fit out to cutoff',F6.3)
 1732 FORMAT (12X,A,1X,A)
 1733 FORMAT (12X,'Fit two files ',A)
 1734 FORMAT (12X,'Exclude phases greater than',F6.1,' degrees')
 1735 FORMAT (12X,'Fit:',4(1PE12.3))
 1736 FORMAT (12X,'Xshift',F10.4,'  Yshift',F10.4)
 1737 FORMAT (12X,'Eccentricity',F7.4,'  Pos angle',F8.2)
 1740 FORMAT (I3,' PEEK    ',A9,' Antenna',I4,'   Amp >',F7.3,' wind <',
     *   F5.1,' m/s')
 1741 FORMAT (12X,A8,'  Elevation range',2F8.1)
 1742 FORMAT (20X,'  Beamwidth range',2F8.3)
 1743 FORMAT (20X,'  Time range',2F14.5)
 1744 FORMAT (20X,'  Filtered output iff >',F5.2,' and >',F4.1,' sigma')
 1750 FORMAT (I3,' LOCIT   ',A9,3X,'BIF',I3,'  EIF',I3,'   STOKES ',A4,
     *   '  Crowded',I2)
 1751 FORMAT (12X,A8,' Antennas',8I4)
 1752 FORMAT (20X,A,2F7.1,' degrees')
 1890 FORMAT (I3,' ??????? ',A9,' (',I4,',',I4,')')
 1891 FORMAT (4X,A6,2X,A8,2X,'(',I4,',',I4,')')
 1800 FORMAT (I3,2X,A9,2X,'(',F7.2,',',F7.2,')',2X,1PE11.4,I6,'  ',A6)
 1802 FORMAT (5X,A8,3X,'(',F7.2,',',F7.2,')',2X,1PE11.4,1X,A)
 1804 FORMAT (5X,A9,3X,'gaussian model',I4,'  rms =',1PE12.4)
 1806 FORMAT (15X,I1,1X,A4,1PE11.3,' +-',1PE10.3,2X,A16)
 1807 FORMAT (17X,A4,1PE11.3,' +-',1PE10.3,2X,A16)
 1808 FORMAT (12X,A9,1PE11.3,' +-',1PE10.3)
 1830 FORMAT (I3,' Created by',6X,A5,' at ',A12,1X,A8)
 1831 FORMAT (4X,'Last written by ',A5,' at ',A12,1X,A8)
 1832 FORMAT (I3,1X,'(',A6,')')
 1833 FORMAT (4X,'Ncol',I4,'  Nrow',I7,3X,'Sort cols:', A25)
 1834 FORMAT (4X,'Select on: ',A)
 1840 FORMAT (4X,'Columns:')
 1842 FORMAT (4X,'Units:  ')
 1880 FORMAT ('FILE VERSION',I7,' APPEARS TO CONTAIN GARBAGE')
 1885 FORMAT ('ERROR READING PLOT FILE VERSION',I7,' ERROR',I7)
      END
