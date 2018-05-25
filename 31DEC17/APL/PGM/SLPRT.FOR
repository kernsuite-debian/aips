LOCAL INCLUDE 'SLPRT.INC'
      HOLLERITH XNAMIN(3), XCLSIN(2), XOUTPR(12)
      REAL      XDOMOD, XSEQ, XDISK, XINV, XINV2, XDOCRT
      COMMON /INPARM/ XDOMOD, XNAMIN, XCLSIN, XSEQ, XDISK, XINV, XINV2,
     *   XDOCRT, XOUTPR
      INTEGER   INSEQ, INDISK, INVER, INVER2, INCNO, SCRTCH(256), SLUN,
     *   SIND, PLUN, PIND, NACROS, PAGE, IPCNT
      LOGICAL   CHANGE(3)
      CHARACTER NAMEIN*12, CLSIN*6, OUTPRT*48, OPTYPE*4, TITL1*132,
     *   TITL2*132, SCRSTR*132, LINE*132
      COMMON /PRTPRM/ SCRTCH, INSEQ, INDISK, INVER, INVER2, INCNO, SLUN,
     *   SIND, PLUN, PIND, NACROS, CHANGE, PAGE, IPCNT
      COMMON /PRTCHR/ NAMEIN, CLSIN, OUTPRT, OPTYPE, TITL1, TITL2,
     *   SCRSTR, LINE
      INTEGER   ISLHDR(256), INOSL, NGAUSS
      REAL      XBLC(7), XTRC(7), YMIN, YMAX, FQFINC, SLICE(16384),
     *   GPARM(12), MODEL(16384)
      DOUBLE PRECISION FQFREQ
      COMMON /PRTSLI/ FQFREQ, ISLHDR, SLICE, MODEL, XBLC, XTRC, YMIN,
     *   YMAX, FQFINC, INOSL, NGAUSS, GPARM
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
LOCAL END
      PROGRAM SLPRT
C-----------------------------------------------------------------------
C! Task to print contents of a SLice file
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2016
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
C   SLPRT will create a printer file representation of a SLice file.
C   Inputs:   (from AIPS)
C      DOMODEL   R      > 0 means print model(s).
C      INNAME    H(3)   name of primary file.
C      INCLASS   H(2)   class of primary file.
C      INSEQ     R      sequence number of primary file.
C      INDISK    R      disk volume number. 0 means try all.
C      INVERS    R      version number of slice file, 0 means try
C                       latest version.
C      IN2VERS   R      version number of gaussian model if
C                       DOSLICE or DOMODEL is true.
C      DOCRT     R      > 0 -> user terminal, else print file/printer
C      OUTPRINT  H(12)  Printer file - ' ' -> line printer
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IRET, SIMPLE
      INCLUDE 'SLPRT.INC'
      DATA PRGNAM /'SLPRT '/
C-----------------------------------------------------------------------
      CALL SLPRTI (PRGNAM, SIMPLE, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       print spectra
      IF (SIMPLE.GT.0) THEN
         CALL SLPRTS (SIMPLE, IRET)
C                                       position coord
      ELSE
         CALL SLPRTP (IRET)
         END IF
C                                       close down
 990  IRET = MAX (0, IRET)
      CALL DIETSK (IRET, RQUICK, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE SLPRTI (PRGNAM, SIMPLE, IRET)
C-----------------------------------------------------------------------
C   SLPRTI does start up tasks for SLPRT
C   Inputs:
C      PRGNAM   C*6   Task name
C   Outputs:
C      SIMPLE   I     slice along SIMPLE axis which is not a position
C                     coordinate
C      IRET     I     > 0 -> quit now
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*(*)
      INTEGER   SIMPLE, IRET
C
      CHARACTER TYPIN*2, PFILE*48, STR*8, STATUS*4
      INTEGER   ISBLK(256), IROUND, DEPTH(5), I, J, IC, IERR, INPRMS,
     *   IPOS, IRRN, NCOUNT, TTY(2)
      REAL      RSBLK(256), X, XCEN, BASLIN(3)
      DOUBLE PRECISION DSBLK(128)
      INCLUDE 'SLPRT.INC'
      EQUIVALENCE (ISBLK, RSBLK, DSBLK)
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       Get input values from AIPS.
      INPRMS = 23
      CALL GTPARM (PRGNAM, INPRMS, RQUICK, XDOMOD, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GETTING PARAMETERS FROM AIPS'
         CALL MSGWRT (8)
         END IF
      CALL H2CHR (48, 1, XOUTPR, OUTPRT)
      IF ((IRET.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   XDOCRT = MIN (-1.0, XDOCRT)
      RQUICK = (RQUICK) .AND. (XDOCRT.LE.0.0)
      IF (RQUICK) RQUICK = OUTPRT.NE.' '
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMIN, NAMEIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      INSEQ = IROUND (XSEQ)
      INDISK = IROUND (XDISK)
      INVER = IROUND (XINV)
      INVER2 = IROUND (XINV2)
C                                       Open map file & get header.
      INCNO = 1
      TYPIN = 'MA'
      CALL CATDIR ('SRCH', INDISK, INCNO, NAMEIN, CLSIN, INSEQ, TYPIN,
     *   NLUSER, STATUS, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINDING IMAGE IN CATALOG'
         GO TO 990
         END IF
C                                       Read catalog header.
      CALL CATIO ('READ', INDISK, INCNO, CATBLK, 'REST', SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING IMAGE HEADER'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = INDISK
      FCNO(NCFILE) = INCNO
      FRW(NCFILE) = 0
C                                       open SL file
      CALL FNDEXT ('SL', CATBLK, I)
      IF ((INVER.LE.0) .OR. (INVER.GT.I)) INVER = I
      IF (INVER.LE.0) THEN
         MSGTXT = 'NO SL FILES FOUND'
         GO TO 990
         END IF
      SLUN = 25
      CALL ZPHFIL ('SL', INDISK, INCNO, INVER, PFILE, IRET)
      CALL ZOPEN (SLUN, SIND, INDISK, PFILE, .FALSE., .FALSE., .TRUE.,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING SL FILE'
         GO TO 990
         END IF
C                                       read SL header
      CALL ZFIO ('READ', SLUN, SIND, 1, ISLHDR, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ SLICE HEADER RECORD'
         GO TO 990
         END IF
C                                       get input adverbs
      CALL ZFIO ('READ', SLUN, SIND, 2, ISBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ SLICE ADVERBS RECORD'
         GO TO 990
         END IF
      CALL RCOPY (7, RSBLK(19), XBLC)
      CALL RCOPY (7, RSBLK(26), XTRC)
      YMIN = RSBLK(34)
      YMAX = RSBLK(35)
      CALL H2CHR (4, 1, RSBLK(33), OPTYPE)
C                                       new format
      IF ((OPTYPE.EQ.'AVER') .OR. (OPTYPE.EQ.'FLUX') .OR.
     *   (OPTYPE.EQ.'LGFL') .OR. (OPTYPE.EQ.'LNFL') .OR.
     *   (OPTYPE.EQ.'ADER') .OR. (OPTYPE.EQ.'FDER') .OR.
     *   (OPTYPE.EQ.'LGAV') .OR. (OPTYPE.EQ.'LNAV')) THEN
         FQFINC = RSBLK(36)
         FQFREQ = DSBLK(19)
C                                       old format
      ELSE
         FQFINC = 0.0
         FQFREQ = 0.0D0
         END IF
C                                       special units
      IF (OPTYPE.EQ.'FLUX') CALL CHR2H (8, 'Jy      ', 1, CATH(KHBUN))
      IF (OPTYPE.EQ.'LGFL') CALL CHR2H (8, 'LOG10 JY', 1, CATH(KHBUN))
      IF (OPTYPE.EQ.'LNFL') CALL CHR2H (8, 'LN (JY) ', 1, CATH(KHBUN))
      IF (OPTYPE.EQ.'FDER') CALL CHR2H (8, 'dJY / dx', 1, CATH(KHBUN))
C                                       model
      IF (XDOMOD.GT.0.0) THEN
         IF ((INVER2.LE.0) .OR. (INVER2.GT.ISLHDR(58))) INVER2 =
     *      ISLHDR(58)
         IF (INVER2.LE.0) THEN
            MSGTXT = 'NO MODELS AVAILABLE'
            CALL MSGWRT (6)
            XDOMOD = -1.0
            END IF
         END IF
C                                       number slice values
      INOSL = ISLHDR(57)
C                                       number of lines of print
      IF ((XDOCRT.LE.0.0) .AND. (OUTPRT.EQ.' ')) THEN
         NCOUNT = INOSL + 8
         IRET = 0
         IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
            IF (NCOUNT.GT.1000) IRET = -1
         ELSE IF (NCOUNT.GT.500) THEN
            TTY(1) = 5
            CALL ZOPEN (TTY(1), TTY(2), 1, SCRSTR, .FALSE., .FALSE.,
     *         .TRUE., IRET)
            MSGTXT = 'PROBLEM OPENING TERMINAL'
            IF (IRET.GT.0) GO TO 990
            WRITE (SCRSTR,1010) NCOUNT
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRSTR, IRET)
            MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
            IF (IRET.GT.0) GO TO 990
            SCRSTR = 'Do you really want to print this much??' //
     *         ' Enter Y or y if so'
            CALL INQSTR (TTY, SCRSTR, 1, STR, IRET)
            IF (IRET.GT.0) GO TO 990
            IF ((STR(:1).NE.'y') .AND. (STR(:1).NE.'Y')) THEN
               IRET = -1
               SCRSTR = 'Good choice - save trees'
            ELSE
               SCRSTR = 'OKAY, printing anyway'
               END IF
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRSTR, I)
            CALL ZCLOSE (TTY(1), TTY(2), I)
            IF (IRET.NE.0) GO TO 999
            END IF
         END IF
C                                       read slice values
      IPOS = 1
      IRRN = 2
 30   IF (IPOS.LE.INOSL) THEN
         IRRN = IRRN + 1
         CALL ZFIO ('READ', SLUN, SIND, IRRN, ISBLK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ SLICE DATA'
            GO TO 990
            END IF
         CALL RCOPY (256, RSBLK, SLICE(IPOS))
         IPOS = IPOS + 256
         GO TO 30
         END IF
C                                       read model
      NGAUSS = 0
      IF (XDOMOD.GT.0.0) THEN
         IRRN = ISLHDR(59) + INVER2 - 1
         CALL ZFIO ('READ', SLUN, SIND, IRRN, ISBLK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ MODEL DATA'
            GO TO 990
            END IF
         NGAUSS = ISBLK(27)
         CALL RCOPY (12, RSBLK, GPARM)
         CALL RCOPY (3, RSBLK(83), BASLIN)
         XCEN = (ISBLK(25) + 1 + INOSL - ISBLK(26)) / 2.0
C                                       compute model
         CALL RFILL (INOSL, 0.0, MODEL)
         DO 50 I = 1,INOSL
            X = I
            DO 40 IC = 1,NGAUSS
               J = 3 * IC - 2
               MODEL(I) = MODEL(I) + GPARM(J) *
     *            EXP (- 2.772 * (GPARM(J+1) - X) ** 2 /
     *            (GPARM(J+2) * GPARM(J+2)))
 40            CONTINUE
            X = X - XCEN
            MODEL(I) = MODEL(I) + BASLIN(1) + X*BASLIN(2) +
     *         X*X*BASLIN(3)
 50         CONTINUE
         END IF
C                                       close slice
      CALL ZCLOSE (SLUN, SIND, IRET)
C                                       open printer
      IF (OUTPRT.EQ.' ') XDOCRT = MAX (-1.0, XDOCRT)
      CALL LPOPEN (OUTPRT, XDOCRT, PLUN, PIND, NACROS, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING THE PRINTER'
         GO TO 990
         END IF
C                                       coordinates
      DO 60 I = 1,5
         DEPTH(I) = XBLC(I+2) + 0.01
 60      CONTINUE
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
      J = 0
      IC = 0
      DO 65 I = 1,3
         CHANGE(I) = ABS(XTRC(I)-XBLC(I)).GT.0.01
         IF (CHANGE(I)) THEN
            J = J + 1
            IC = I
            END IF
 65      CONTINUE
      SIMPLE = 0
      IF (J.EQ.1) THEN
         CALL H2CHR (8, 1, CATH(KHCTP+2*(IC-1)), STR)
         IF (STR(:4).EQ.'FREQ') SIMPLE = IC
         IF (STR(:4).EQ.'VELO') SIMPLE = IC
         IF (STR(:4).EQ.'FELO') SIMPLE = IC
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I4,' ON ',A)
 1010 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE SLPRTS (SIMPLE, IRET)
C-----------------------------------------------------------------------
C   print "simple" slices - all on 1 axis which is frequency,
C   velocity, or felocity
C   Inputs:
C      SIMPLE   I   axis number of "simple" axis
C   Outputs:
C      IRET     I   not 0 => failure of some sort
C-----------------------------------------------------------------------
      INTEGER   SIMPLE, IRET
C
      INCLUDE 'SLPRT.INC'
      INTEGER   HM(2), DM(2), I, JTRIM
      REAL      XPIX, YPIX, ZPIX, RSEC, DSEC, REFP, REFI, VDIV, RMS,
     *   NRMS
      CHARACTER CHSIG1*1, CHSIG2*1, LLCH*4, MMCH*4, AXIS*8, UNITS*8,
     *   AUNIT*8, CTEMP*8
      DOUBLE PRECISION XX, YY, ZZ, RR, DD, VV, REFV
C-----------------------------------------------------------------------
      CALL H2CHR (8, 1, CATH(KHBUN), CTEMP)
      I = JTRIM (CTEMP)
      UNITS = ' '
      UNITS(9-I:) = CTEMP(:I)
      CALL H2CHR (8, 1, CATH(KHCTP+2*SIMPLE-2), CTEMP)
      VDIV = 1.0
      AUNIT = ' '
      IF (CTEMP(:4).EQ.'FREQ') THEN
         VDIV = 1.E6
         AUNIT = '     MHz'
      ELSE IF (CTEMP(:4).EQ.'VELO') THEN
         VDIV = 1.E3
         AUNIT = '    km/s'
      ELSE IF (CTEMP(:4).EQ.'FELO') THEN
         VDIV = 1.E3
         AUNIT = '    km/s'
         END IF
      I = JTRIM (CTEMP)
      AXIS = ' '
      AXIS(9-I:) = CTEMP(:I)
C                                       base coordinate
      XPIX = XBLC(1)
      YPIX = XBLC(2)
      ZPIX = XBLC(3)
      CALL XYVAL (XPIX, YPIX, XX, YY, ZZ, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INITIAL COORDINATE COMPUTATION'
         CALL MSGWRT (6)
         END IF
      IF (CORTYP(LOCNUM).EQ.1) THEN
         RR = XX
         DD = YY
      ELSE IF (CORTYP(LOCNUM).EQ.2) THEN
         RR = YY
         DD = XX
      ELSE IF (CORTYP(LOCNUM).EQ.3) THEN
         RR = XX
         DD = ZZ
      ELSE IF (CORTYP(LOCNUM).EQ.4) THEN
         RR = ZZ
         DD = XX
      ELSE IF (CORTYP(LOCNUM).EQ.5) THEN
         RR = YY
         DD = ZZ
      ELSE IF (CORTYP(LOCNUM).EQ.6) THEN
         RR = ZZ
         DD = YY
         END IF
      CALL H2CHR (4, 1, CATH(KHCTP+KLOCL(LOCNUM)*2), LLCH)
      CALL H2CHR (4, 1, CATH(KHCTP+KLOCM(LOCNUM)*2), MMCH)
      IF (RR.LT.0.0D0) RR = RR + 360.0D0
      IF (RR.LT.0.0D0) RR = RR + 360.0D0
      IF (RR.LT.0.0D0) RR = RR + 360.0D0
      IF (LLCH(:2).EQ.'RA') THEN
         CALL COORDD (1, RR, CHSIG1, HM, RSEC)
      ELSE
         CALL COORDD (2, RR, CHSIG1, HM, RSEC)
         END IF
      CALL COORDD (2, DD, CHSIG2, DM, DSEC)
C                                       Page header
      PAGE = 0
      IPCNT = 998
      TITL1 = ' '
      TITL2 = ' '
      IF (XDOCRT.GT.-2.5) THEN
C                                       file
         WRITE (LINE,1010) NAMEIN, CLSIN, INSEQ, INDISK, NLUSER
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         IF (IRET.NE.0) GO TO 950
C                                       SLice file
         WRITE (LINE,1015) INVER, INVER2
         IF (XDOMOD.LE.0.0) LINE(26:) = ' '
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         IF (IRET.NE.0) GO TO 950
C                                       slice ends
         WRITE (LINE,1020) XBLC(1), XBLC(2), XBLC(3), XTRC(1), XTRC(2),
     *      XTRC(3)
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         IF (IRET.NE.0) GO TO 950
C                                       main coordinate
         WRITE (LINE,1025) LLCH, HM, RSEC, MMCH, CHSIG2, DM, DSEC
         IF (LINE(15:15).EQ.' ') LINE(15:15) = '0'
         IF (LINE(38:38).EQ.' ') LINE(38:38) = '0'
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         IF (IRET.NE.0) GO TO 950
         LINE = ' '
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         IF (IRET.NE.0) GO TO 950
         END IF
C                                       page titles
      WRITE (TITL1,1030) AXIS
      IF (XDOMOD.LE.0.0) TITL1(32:) = ' '
      WRITE (TITL2,1031) AUNIT, UNITS, UNITS, UNITS
      IF (XDOMOD.LE.0.0) TITL2(32:) = ' '
      IF (XDOCRT.GT.-2.5) THEN
         LINE = TITL1
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         IF (IRET.NE.0) GO TO 950
         LINE = TITL2
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         IF (IRET.NE.0) GO TO 950
         END IF
      REFV = CATD(KDCRV+SIMPLE-1) / VDIV
      REFP = CATR(KRCRP+SIMPLE-1)
      REFI = CATR(KRCIC+SIMPLE-1) / VDIV
      RMS = 0.0
      NRMS = 0.0
      DO 100 I = 1,INOSL
         XX = XBLC(SIMPLE) + (I-1.0) / (INOSL-1.0) *
     *      (XTRC(SIMPLE) - XBLC(SIMPLE))
         IF (AXFUNC(SIMPLE,LOCNUM).EQ.0) THEN
            VV = REFV + (XX - REFP) * REFI
C                                       felocity
         ELSE
             VV = REFV + (XX - REFP) * REFI /
     *         (1.0D0 + AXDENU(LOCNUM) * (XX-REFP))
            END IF
         IF (SLICE(I).NE.FBLANK) THEN
            WRITE (LINE,1050) I, VV, SLICE(I)
            IF (XDOMOD.GT.0.0) THEN
               WRITE(LINE(32:),1055) MODEL(I), SLICE(I)-MODEL(I)
               RMS = RMS + (SLICE(I)-MODEL(I))**2
               NRMS = NRMS + 1.0
               END IF
         ELSE
            WRITE (LINE,1051) I, VV
            END IF
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         IF (IRET.NE.0) GO TO 950
 100     CONTINUE
      IF ((XDOMOD.GT.0.0) .AND. (NRMS.GT.0.0)) THEN
         RMS = SQRT (RMS/NRMS)
         WRITE (LINE,1100) RMS
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         END IF
C
 950  IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'PRINT LINE ERROR'
         CALL MSGWRT (8)
         END IF
      IRET = MAX (0, IRET)
      CALL LPCLOS (PLUN, PIND, IPCNT, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SLPRTS ERROR',I4,' ON ',A)
 1010 FORMAT ('File = ',A12,'.',A6,'.',I4,'   Vol =',I2,4X,'Userid =',
     *   I5)
 1015 FORMAT ('SLice file version =',I5,'   model version =',I2)
 1020 FORMAT ('SLice  Blc=',3F8.1,'    Trc=',3F8.1)
 1025 FORMAT (A4,' = ',I3.2,':',I2.2,':',F5.2,4X,A4,' = ',A1,I2.2,':',
     *   I2.2,':',F4.1)
 1030 FORMAT (' Pixel',4X,A8,'  Slice value  Model value     Residual')
 1031 FORMAT (10X,A8,5X,A8,5X,A8,5X,A8)
 1050 FORMAT (I6,F12.3,F13.5)
 1051 FORMAT (I6,F12.3,6X,'Blanked')
 1055 FORMAT (F13.5,F13.6)
 1100 FORMAT ('Data-model RMS =',F13.6)
      END
      SUBROUTINE SLPRTP (IRET)
C-----------------------------------------------------------------------
C   print not so "simple" slices
C   Outputs:
C      IRET     I   not 0 => failure of some sort
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'SLPRT.INC'
      INTEGER   HM(2), I, NAXES, ICH, J, K, JTRIM
      REAL      PIXEL(3), RSEC, VDIV(3), RMS, NRMS
      CHARACTER CHSIG1*1, AXIS(3)*8, UNITS*8, AUNIT(3)*8, ASTRNG(3)*13,
     *   CTEMP*8
      DOUBLE PRECISION VV(3)
C-----------------------------------------------------------------------
      NAXES = 3
      IF (CATBLK(KINAX+2).LE.1) NAXES = 2
C                                       right shift units
      CALL H2CHR (8, 1, CATH(KHBUN), CTEMP)
      J = JTRIM (CTEMP)
      UNITS = ' '
      UNITS(9-J:) = CTEMP(1:J)
      DO 10 I = 1,NAXES
         CALL H2CHR (8, 1, CATH(KHCTP+2*I-2), CTEMP)
         IF (CTEMP(:4).EQ.'FREQ') THEN
            VDIV(I) = 1.0E6
            AUNIT(I) = '     MHz'
         ELSE IF (CTEMP(2:4).EQ.'ELO') THEN
            VDIV(I) = 1.0E3
            AUNIT(I) = '    km/s'
         ELSE
            VDIV(I) = 1.0
            AUNIT(I) = ' '
            END IF
         J = JTRIM (CTEMP)
         AXIS(I) = ' '
         AXIS(I)(9-J:) = CTEMP(1:J)
 10      CONTINUE
C                                       Page header
      PAGE = 0
      IPCNT = 998
      TITL1 = ' '
      TITL2 = ' '
      IF (XDOCRT.GT.-2.5) THEN
C                                       file
         WRITE (LINE,1010) NAMEIN, CLSIN, INSEQ, INDISK, NLUSER
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         IF (IRET.NE.0) GO TO 950
C                                       SLice file
         WRITE (LINE,1015) INVER, INVER2
         IF (XDOMOD.LE.0.0) LINE(26:) = ' '
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         IF (IRET.NE.0) GO TO 950
C                                       slice ends
         WRITE (LINE,1020) XBLC(1), XBLC(2), XBLC(3), XTRC(1), XTRC(2),
     *      XTRC(3)
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         IF (IRET.NE.0) GO TO 950
         LINE = ' '
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         IF (IRET.NE.0) GO TO 950
         END IF
C                                       page titles
      WRITE (TITL1,1030) (AXIS(I), I = 1,NAXES)
      ICH = 7 + 14*NAXES
      IF (XDOMOD.GT.0.0) THEN
         TITL1(ICH:) = '  Slice value  Model value    Residual'
      ELSE
         TITL1(ICH:) = '  Slice value'
         END IF
      WRITE (TITL2,1031) (AUNIT(I), I = 1,NAXES)
      IF (XDOMOD.GT.0.0) THEN
         TITL2(ICH:) = '     ' //  UNITS // '     ' // UNITS // '    '
     *      // UNITS
      ELSE
         TITL2(ICH:) = '     ' //  UNITS
         END IF
      IF (XDOCRT.GT.-2.5) THEN
         LINE = TITL1
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         IF (IRET.NE.0) GO TO 950
         LINE = TITL2
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         IF (IRET.NE.0) GO TO 950
         END IF
      PIXEL(3) = 1.0
      RMS = 0.0
      NRMS = 0.0
      DO 100 I = 1,INOSL
         DO 20 J = 1,NAXES
            PIXEL(J) = (I-1.0)/(INOSL-1.0) * (XTRC(J) - XBLC(J)) +
     *         XBLC(J)
 20         CONTINUE
         IF (NAXES.GT.2) ZDEPTH(ZAXIS(LOCNUM)-2,LOCNUM) = PIXEL(3)
         CALL XYVAL (PIXEL(1), PIXEL(2), VV(1), VV(2), VV(3), IRET)
         DO 30 J = 1,NAXES
            IF (AXFUNC(J,LOCNUM).EQ.0) THEN
               VV(J) = CATD(KDCRV+J-1) + (PIXEL(J) - CATR(KRCRP+J-1)) *
     *            CATR(KRCIC+J-1)
               WRITE (ASTRNG(J),1040) VV(J)/VDIV(J)
C                                       felocity
            ELSE IF (AXFUNC(J,LOCNUM).EQ.1) THEN
                VV(J) = CATD(KDCRV+J-1) + (PIXEL(J) - CATR(KRCRP+J-1)) *
     *            CATR(KRCIC+J-1) /
     *            (1.0D0 + AXDENU(LOCNUM) * (PIXEL(J)-CATR(KRCRP+J-1)))
               WRITE (ASTRNG(J),1040) VV(J)/VDIV(J)
            ELSE
              IF (AXIS(J)(:2).EQ.'RA') THEN
                 IF (VV(J).LT.0.0D0) VV(J) = VV(J) + 360.0D0
                 CALL COORDD (1, VV(J), CHSIG1, HM, RSEC)
                 WRITE (ASTRNG(J),1041) HM, RSEC
                 IF (ASTRNG(J)(8:8).EQ.' ') ASTRNG(J)(8:8) = '0'
              ELSE IF (AXIS(J)(2:4).EQ.'LON') THEN
                 IF (VV(J).LT.0.0D0) VV(J) = VV(J) + 360.0D0
                 CALL COORDD (2, VV(J), CHSIG1, HM, RSEC)
                 WRITE (ASTRNG(J),1042) HM, RSEC
                 IF (ASTRNG(J)(9:9).EQ.' ') ASTRNG(J)(9:9) = '0'
              ELSE
                 CALL COORDD (2, VV(J), CHSIG1, HM, RSEC)
                 WRITE (ASTRNG(J),1043) CHSIG1, HM, RSEC
                 IF (ASTRNG(J)(9:9).EQ.' ') ASTRNG(J)(9:9) = '0'
                 END IF
               END IF
 30         CONTINUE
         WRITE (LINE,1050) I, (ASTRNG(K), K = 1,NAXES)
         IF (SLICE(I).EQ.FBLANK) THEN
            LINE(ICH:) = '      Blanked'
         ELSE IF (XDOMOD.GT.0) THEN
            WRITE (LINE(ICH:),1051) SLICE(I), MODEL(I),
     *         SLICE(I)-MODEL(I)
            RMS = RMS + (SLICE(I)-MODEL(I))**2
            NRMS = NRMS + 1.0
         ELSE
            WRITE (LINE(ICH:),1051) SLICE(I)
            END IF
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         IF (IRET.NE.0) GO TO 950
 100     CONTINUE
      IF ((XDOMOD.GT.0.0) .AND. (NRMS.GT.0.0)) THEN
         RMS = SQRT (RMS/NRMS)
         WRITE (LINE,1100) RMS
         CALL PRTLIN (PLUN, PIND, XDOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRSTR, IRET)
         END IF
C
 950  IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'PRINT LINE ERROR'
         CALL MSGWRT (8)
         END IF
      IRET = MAX (0, IRET)
      CALL LPCLOS (PLUN, PIND, IPCNT, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SLPRTS ERROR',I4,' ON ',A)
 1010 FORMAT ('File = ',A12,'.',A6,'.',I4,'   Vol =',I2,4X,'Userid =',
     *   I5)
 1015 FORMAT ('SLice file version =',I5,'   model version =',I2)
 1020 FORMAT ('SLice  Blc=',3F8.1,'    Trc=',3F8.1)
 1030 FORMAT (' Pixel',3(6X,A8))
 1031 FORMAT (6X,3(6X,A8))
 1040 FORMAT (F13.3)
 1041 FORMAT (1X,I2.2,':',I2.2,':',F6.3)
 1042 FORMAT (1X,I3.2,':',I2.2,':',F5.2)
 1043 FORMAT (1X,A1,I2.2,':',I2.2,':',F5.2)
 1050 FORMAT (I6,3(1X,A13))
 1051 FORMAT (2F13.5,F12.6)
 1100 FORMAT ('Data-model RMS =',F13.6)
      END
