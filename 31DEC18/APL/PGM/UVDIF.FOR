LOCAL INCLUDE 'UVDIF.INC'
C                                       Local include for UVDIF
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER NAME*12, CLASS*6, NAME2*12, CLASS2*12, OPCODE*4,
     *   LPNAME*48, TITL1*132, TITL2*132, SCRTCH*132, LINE*132
      HOLLERITH XNAME(3), XCLASS(2), XNAME2(3), XCLAS2(2),
     *   XOPCOD, CAT2H(256), XLPNAM(12)
      REAL      XSEQ, XDISK, XSEQ2, XDISK2, XNCH, XBIF, XNIT, UVRANG(2),
     *   APARM(10), DOCRT
      REAL      BUFF(UVBFSS), BUFF2(UVBFSS), RADIUS(2), POSANG(2),
     *   FMAX(4), CAT2R(256), UVM, XAMP, XWT, NWT
      DOUBLE PRECISION CAT2D(128)
      LOGICAL   DOMAX, DOUVBX, ISUVR, ISCMP1, ISCMP2, DOFLAG
      INTEGER   LUN, FIND, PLUN, PIND, CNO, ISTKS, NCH, BIF, SEQ, DISK,
     *   USERID, LUN2, FIND2, CNO2, SEQ2, DISK2, NITER, CAT2I(256),
     *   IPCNT, PAGE, NACROS, KLOCWT, LLOCWT, NRP1, NRP2, LREC2,
     *   I1LOCU, I1LOCV, I1LOCW, I1LOCB, I1LOCT, I1LOCS, I1LOCF,
     *   I1LOSA, I1LOA1, I1LOA2,
     *   I2LOCU, I2LOCV, I2LOCW, I2LOCB, I2LOCT, I2LOCS, I2LOCF,
     *   I2LOSA, I2LOA1, I2LOA2
C                                       Local include for UVDIF
      COMMON /INPARM/ XNAME, XCLASS, XSEQ, XDISK, XNAME2, XCLAS2, XSEQ2,
     *   XDISK2, XNCH, XBIF, XNIT, UVRANG, XOPCOD, APARM, DOCRT, XLPNAM
      COMMON /CHPARM/ NAME, CLASS, NAME2, CLASS2, OPCODE, LPNAME,
     *   TITL1, TITL2, LINE, SCRTCH
      COMMON /FNDUVC/ BUFF, BUFF2, RADIUS, POSANG, FMAX, NITER,
     *   DOMAX, DOUVBX, ISUVR, ISCMP1, ISCMP2, LUN, FIND, LUN2, FIND2,
     *   NCH, BIF, PLUN, PIND, CNO, CNO2, ISTKS, SEQ, DISK, SEQ2, DISK2,
     *   USERID, PAGE, IPCNT, NACROS, KLOCWT, LLOCWT, UVM, XAMP, XWT,
     *   NWT, NRP1, NRP2, LREC2, DOFLAG,
     *   I1LOCU, I1LOCV, I1LOCW, I1LOCB, I1LOCT, I1LOCS, I1LOCF, I1LOSA,
     *   I1LOA1, I1LOA2, I2LOCU, I2LOCV, I2LOCW, I2LOCB, I2LOCT, I2LOCS,
     *   I2LOCF, I2LOSA, I2LOA1, I2LOA2
      COMMON /MAP2HD/ CAT2I
      EQUIVALENCE (CAT2I, CAT2R, CAT2H, CAT2D)
LOCAL END
LOCAL INCLUDE 'BUFFS.INC'
      INCLUDE 'ZPBUFSZ.INC'
      REAL      LBUFF(UVBFSS), KBUFF(UVBFSS), KBUFF2(UVBFSS),
     *   LBUFF2(UVBFSS)
      COMMON /UVDUFS/ LBUFF, KBUFF, LBUFF2, KBUFF2
LOCAL END
      PROGRAM UVDIF
C-----------------------------------------------------------------------
C! UVDIF prints the differences between two uv data sets
C# UV UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999, 2002-2004, 2007-2009, 2015-2016
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
C   UVDIF is an AIPS system task to locate questionable data in a
C   catalogd UV data base and to print them.  Compares 2 data sets.
C   Inputs:
C     USERID    R         User number
C     INNAME    R(3)      File name: 12 chars
C     INCLASS   R(2)      File class: 6 chars
C     INSEQ     R         File seq #
C     INDISK    R         Disk volume on which file resides
C     IN2NAME   R(3)      File name: 12 chars
C     IN2CLASS  R(2)      File class: 6 chars
C     IN2SEQ    R         File seq #
C     IN2DISK   R         Disk volume on which file resides
C     CHANNEL   R         Spectral channel number
C     BIF       R         IF to test.
C     NITER     R         Limit on number of lines to print
C     UVRANGE   R(2)      Range of wavelengths to check in 1000's
C     OPCODE    R         Opcode: 4 chars
C                            'CLIP' : print high flux diffs
C                            'UVBX' : print high u,v,w diffs
C                            else do both
C     APARM     R(10)     (1) max cutoff IPOL (Jy) flux diff
C                         (2) max cutoff cross-pol (Jy)
C                         (3) max cutoff u,v,w diff (wavelengths)
C     DOCRT     R         > 0 -> use CRT, else line printer
C     OUTPRINT  H         File name to keep printer output
C-----------------------------------------------------------------------
      LOGICAL   RQUICK
      INTEGER   IRET
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'UVDIF.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Init, open file
      CALL FNDUIN (RQUICK, IRET)
C                                       Get scales
      IF (IRET.EQ.0) CALL FNDUSC (IRET)
C                                       Do it
      IF (IRET.EQ.0) THEN
         IF (OPCODE.EQ.'UVBX') THEN
            CALL FNDUBX (IRET)
         ELSE IF (OPCODE.EQ.'CLIP') THEN
            CALL FNDUCL (IRET)
         ELSE
            CALL FNDUPR (IRET)
            END IF
         END IF
C                                       Close up shop
      IRET = MAX (0, IRET)
      CALL DIETSK (IRET, RQUICK, BUFF)
C
 999  STOP
      END
      SUBROUTINE FNDUIN (RQUICK, IRET)
C-----------------------------------------------------------------------
C   FNDUIN inits the task FNDUV and opens the UV file and printer.
C   Outputs:
C      RQUICK   L    T => AIPS has been restarted
C      IRET     I    error code: 0 => keep going, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, ITYPE*2, CTEMP1*8, CTEMP2*8, UTYPE*2, SCRSTR*132
      LOGICAL   RQUICK
      INTEGER   IRET
      LOGICAL   F, EQUAL
      INTEGER   NPARM,I, J, IERR, INC, NT, IROUND, NCOUNT, TTY(2)
      DOUBLE PRECISION   DEPS
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'UVDIF.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA F /.FALSE./
      DATA PRGM /'UVDIF '/
C-----------------------------------------------------------------------
C                                       Init I/O
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Get input parameters.
      NPARM = 43
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAME, BUFF, IRET)
      IF (IRET.EQ.0) GO TO 10
         RQUICK = .TRUE.
         IF (IRET.EQ.1) GO TO 999
C                                       Restart AIPS.
 10   IF ((IRET.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      IF (DOCRT.GT.0.0) RQUICK = .FALSE.
      IF ((IRET.EQ.0) .AND. (RQUICK)) RQUICK = LPNAME.NE.' '
      IF (RQUICK) CALL RELPOP (IRET, BUFF, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 8
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLASS2)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      DOFLAG = APARM(6).GT.0.99
C                                       Decode input.
      USERID = NLUSER
      SEQ = IROUND (XSEQ)
      DISK = IROUND (XDISK)
      SEQ2 = IROUND (XSEQ2)
      DISK2 = IROUND (XDISK2)
      BIF = IROUND (XBIF)
      BIF = MAX (1, BIF)
      NCH = IROUND (XNCH)
      NCH = MAX (1, NCH)
      NITER = IROUND (XNIT)
      IF (NITER.LE.0) NITER = 5 * (PRTMAX - 7) - 4
      IF (UVRANG(1).LE.0.0) UVRANG(1) = 0.0
      IF (UVRANG(2).LE.UVRANG(1)) UVRANG(2) = 1.E9
      ISUVR = (UVRANG(1).GT.0.001) .OR. (UVRANG(2).LT.9.8E8)
      DOMAX = OPCODE.EQ.'CLIP'
      DOUVBX = OPCODE.EQ.'UVBX'
      IF ((DOMAX) .OR. (DOUVBX)) GO TO 15
         DOMAX = .TRUE.
         DOUVBX = .TRUE.
C                                       Stokes
 15   ISTKS = 8
C                                       Open file 2 and get CAT2.
      LUN2 = 17
      ITYPE = ' '
      CALL MAPOPN ('READ', DISK2, NAME2, CLASS2, SEQ2, ITYPE, USERID,
     *   LUN2, FIND2, CNO2, CAT2I, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         CALL MSGWRT (8)
         GO TO 940
         END IF
      ISCMP2 = CAT2I(KINAX).EQ.1
      NRP2 = CAT2I(KIPCN)
      CALL COPY (256, CAT2I, CATBLK)
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 950
      LREC2 = LREC
      I2LOCU = ILOCU
      I2LOCV = ILOCV
      I2LOCW = ILOCW
      I2LOCB = ILOCB
      I2LOCT = ILOCT
      I2LOCS = ILOCSU
      I2LOCF = ILOCFQ
      I2LOSA = ILOCSA
      I2LOA1 = ILOCA1
      I2LOA2 = ILOCA2
C                                       Open file and get CATBLK.
      LUN = 16
      UTYPE = 'UV'
      CALL MAPOPN ('READ', DISK, NAME, CLASS, SEQ, UTYPE, USERID, LUN,
     *   FIND, CNO, CATBLK, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1025) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Get info from CATBLK.
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 950
      I1LOCU = ILOCU
      I1LOCV = ILOCV
      I1LOCW = ILOCW
      I1LOCB = ILOCB
      I1LOCT = ILOCT
      I1LOCS = ILOCSU
      I1LOCF = ILOCFQ
      I1LOSA = ILOCSA
      I1LOA1 = ILOCA1
      I1LOA2 = ILOCA2
C                                       Compressed?
      ISCMP1 = CATBLK(KINAX).EQ.1
      NRP1 = CATBLK(KIPCN)
C                                       Check IF
      IF ((JLOCIF.LE.0) .OR. (CATBLK(KINAX+JLOCIF).LE.1)) BIF = 1
      IF ((JLOCIF.GT.0) .AND. (BIF.GT.CATBLK(KINAX+JLOCIF))) THEN
         WRITE (MSGTXT,1030) BIF, CATBLK(KINAX+JLOCIF)
         CALL MSGWRT (8)
         GO TO 950
         END IF
C                                       Check spectral channel.
      IF (CATBLK(KINAX+JLOCF).LE.1) NCH = 1
      IF (NCH.GT.CATBLK(KINAX+JLOCF)) THEN
         WRITE (MSGTXT,1032) NCH, CATBLK(KINAX+JLOCF)
         CALL MSGWRT (8)
         GO TO 950
         END IF
C                                       Compare headers
      EQUAL = ITYPE.EQ.'SC'
      IF (ITYPE.NE.'SC') THEN
         IF (APARM(5).LT.1.5) THEN
            IF (ABS(CATBLK(KIGCN)-CAT2I(KIGCN)).GT.5) GO TO 49
            END IF
         IF (CATBLK(KIDIM).NE.CAT2I(KIDIM)) GO TO 49
         INC = 2
         NT = CATBLK(KIDIM)
         IF (NT.GT.4) NT = 4
         DO 45 I = 2,NT
            J = I - 1
            DEPS = 0.01 * ABS(CATR(KRCIC+J))
            IF (DEPS.LE.0.0D0) DEPS = 1.0D-4
            IF (CATBLK(KINAX+J).NE.CAT2I(KINAX+J)) GO TO 49
            IF (APARM(5).LT.0.5) THEN
               IF (ABS(CATR(KRCRP+J)-CAT2R(KRCRP+J)).GT.DEPS) GO TO 49
               IF (ABS(CATD(KDCRV+J)-CAT2D(KDCRV+J)).GT.DEPS) GO TO 49
               IF (ABS(CATR(KRCIC+J)-CAT2R(KRCIC+J)).GT.DEPS) GO TO 49
               END IF
            IF (APARM(5).LT.1.5) THEN
               J = KHCTP + J * INC
               CALL H2CHR (8, 1, CATH(J), CTEMP1)
               CALL H2CHR (8, 1, CAT2H(J), CTEMP2)
               IF (CTEMP1.NE.CTEMP2) GO TO 49
               END IF
 45         CONTINUE
         IF (CATBLK(KIGCN).NE.CAT2I(KIGCN)) THEN
            MSGTXT = 'WARNING: 2 DATASETS NOT QUITE THE SAME SIZE'
            CALL MSGWRT (6)
            END IF
         GO TO 50
C                                       no match
 49      CONTINUE
            WRITE (MSGTXT,1049)
            CALL MSGWRT (8)
            GO TO 940
         END IF
C                                       check line count
 50   IF ((DOCRT.LE.0.0) .AND. (LPNAME.EQ.' ')) THEN
         NCOUNT = 0
         IRET = 0
         MSGTXT = 'Checking count of lines for direct output to printer'
         CALL MSGWRT (2)
         IF (OPCODE.EQ.'UVBX') THEN
            CALL CHKUBX (NCOUNT, IRET)
         ELSE IF (OPCODE.EQ.'CLIP') THEN
            CALL CHKUCL (NCOUNT, IRET)
         ELSE
            CALL CHKUPR (NCOUNT, IRET)
            END IF
         IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
            IF ((NCOUNT.GT.1000) .AND.(IRET.EQ.0)) IRET = -1
         ELSE IF ((IRET.EQ.0) .AND. (NCOUNT.GT.500)) THEN
            TTY(1) = 5
            CALL ZOPEN (TTY(1), TTY(2), 1, SCRSTR, .FALSE., .FALSE.,
     *         .TRUE., IRET)
            MSGTXT = 'PROBLEM OPENING TERMINAL'
            IF (IRET.GT.0) GO TO 990
            WRITE (SCRSTR,1050) NCOUNT
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRSTR, IRET)
            MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
            IF (IRET.GT.0) GO TO 990
            SCRSTR = 'Do you really want to print this much??' //
     *         ' Enter Y or y if so'
            CALL INQSTR (TTY, SCRSTR, 1, CTEMP1, IRET)
            IF (IRET.GT.0) GO TO 990
            IF ((CTEMP1(:1).NE.'y') .AND. (CTEMP1(:1).NE.'Y')) THEN
               SCRSTR = 'Good choice - save trees'
               IRET = -1
            ELSE
               SCRSTR = 'OKAY, printing anyway'
               END IF
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRSTR, I)
            CALL ZCLOSE (TTY(1), TTY(2), I)
            END IF
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Open printer
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, PLUN, PIND, NACROS, BUFF, IERR)
      IF (IERR.NE.0) GO TO 940
      IRET = 1
      IPCNT = 998
      PAGE = 0
      TITL1 = ' '
      TITL2 = ' '
C                                       function parameters
      IF (DOMAX) THEN
         IF (APARM(4).EQ.0.0) APARM(4) = 1.E8
         IF ((APARM(2).LE.0.0) .AND. (APARM(1).LE.0.0)) APARM(1) = 1.0
         IF ((APARM(2).LE.0.0) .AND. (APARM(1).GT.0.0)) APARM(2) = 1.E8
         IF ((APARM(1).LE.0.0) .AND. (APARM(2).GT.0.0)) APARM(1) = 1.E8
         IF ((APARM(1).LT.1.E5) .AND. (DOCRT.GT.-2.5)) THEN
            WRITE (LINE,1060) APARM(1)
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 940
            END IF
         IF ((APARM(2).LT.1.E5) .AND. (DOCRT.GT.-2.5)) THEN
            WRITE (LINE,1061) APARM(2)
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 940
            END IF
         IF ((ABS(APARM(4)).LT.1.E5) .AND. (DOCRT.GT.-2.5)) THEN
            IF (APARM(4).GT.0.0) THEN
               WRITE (LINE,1062) APARM(4)
            ELSE
               WRITE (LINE,1063) -APARM(4)
               END IF
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 940
            END IF
         END IF
C                                       Test values of U, V, W
      IF (DOUVBX) THEN
         IF (APARM(3).LE.0.0) APARM(3) = 1000.
         IF (DOCRT.GT.-2.5) THEN
            WRITE (LINE,1065) APARM(3)
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 940
            END IF
         APARM(3) = APARM(3) / 1000.0
         END IF
C                                       Scale UVRANG
      IF ((ISUVR) .AND. (DOCRT.GT.-2.5)) THEN
         WRITE (LINE,1070) UVRANG
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 940
         END IF
      UVRANG(1) = UVRANG(1) * UVRANG(1)
      UVRANG(2) = UVRANG(2) * UVRANG(2)
      IRET = 0
      GO TO 999
C                                       Close map on error
 940  CALL MAPCLS ('READ', DISK2, CNO2, LUN2, FIND2, CAT2I, F, BUFF2,
     *    IERR)
      IF (IERR.EQ.0) GO TO 950
         WRITE (MSGTXT,1940) IERR
         CALL MSGWRT (6)
 950  CALL MAPCLS ('READ', DISK, CNO, LUN, FIND, CATBLK, F, BUFF, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1950) IERR
C
 990     CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1025 FORMAT ('ERROR:',I7,' OPENING 1ST UV FILE')
 1030 FORMAT ('IF',I5,' .GT. MAX =',I5)
 1032 FORMAT ('CHANNEL',I5,' .GT. MAX =',I5)
 1035 FORMAT ('ERROR:',I7,' OPENING 2ND UV FILE')
 1049 FORMAT ('HEADERS DO NOT MATCH')
 1050 FORMAT ('Requested print job is',I10,' lines long!')
 1060 FORMAT ('Will print all total int. points   >',F12.6,' JY')
 1061 FORMAT ('Will print all polarization points >',F12.6,' JY')
 1062 FORMAT ('Will print all weight points       >',F12.6,
     *   ' and flagging')
 1063 FORMAT ('Will print all weight points       >',F12.6,
     *   ' flagging ignored')
 1065 FORMAT ('Will print all uv differences      >',F12.6,' lambda')
 1070 FORMAT ('Limited to baselines between',2(1PE13.4),' klambda')
 1940 FORMAT ('ERROR:',I7,' CLOSING 2ND UV FILE ')
 1950 FORMAT ('ERROR:',I7,' CLOSING 1ST UV FILE ')
      END
      SUBROUTINE FNDUSC (IRET)
C-----------------------------------------------------------------------
C   FNDUSC reads the UV files and determines extrema for u-v-w, flux,
C   and weight.
C   Output:
C      IRET   I   Return code: 0 success or user requests end, else bad
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'UVDIF.INC'
      INTEGER   JADR(8), BIND, CINC, BUFSZ, J, LENBU, MVIS, IA1, IA2,
     *   ICH1, IERR, JERR, KK, IPINT, NIO, NNCH, NUMCH, LRECF, NUMCHM,
     *   BO, VO, COUNT, NCORR, DPTR, IERR1
      REAL   VIS(3,8), TEMP, UVN
      INCLUDE 'BUFFS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PUVD.INC'
      DATA LENBU, BO, VO /16, 1, 0/
C-----------------------------------------------------------------------
      IRET = 8
      CINC = 2
C                                       Set up for compressed data.
      NCORR = (LREC - NRPARM) / CATBLK(KINAX)
      LRECF = NRPARM + 3 * NCORR
      DPTR = 1 + NRPARM
      CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), KLOCWT,
     *   JERR)
      IF (ISCMP1 .AND. ((JERR.NE.0) .OR. (KLOCWT.LT.0))) THEN
         IRET = 5
         MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Determine no. channels to do
      FREQ = FREQ - (CATR(KRCRP+JLOCF)-NCH) * CATR(KRCIC+JLOCF)
      NUMCHM = CATBLK(KINAX+JLOCF) - NCH + 1
      IF (NCORR.GT.1) NUMCHM = 1
      NUMCH = (NACROS - 52) / (15 * NCOR)
      NUMCH = MAX (1, MIN (NUMCH, NUMCHM))
      ICH1 = NCH
      NNCH = NCH - 1
C                                       Make sure NITER is OK
      XAMP = 0.
      UVM = 0.
      XWT = 0.
      NWT = 1.E10
C                                       Initialize reading VIS. file.
      LENBU = 0
      BUFSZ = UVBFSS * 2
      CALL UVINIT ('READ', LUN, FIND, NVIS, VO, LREC, LENBU, BUFSZ,
     *   BUFF, BO, BIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      COUNT = 0
C                                       Counters to uncompressed
      IF (ISCMP1) THEN
         INCF = 3 * INCF
         INCS = 3 * INCS
         INCIF = 3 * INCIF
         END IF
C                                       Try to handle line
      CALL SETVIS (ICH1, NUMCH, BIF, MVIS, JADR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1075)
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Start looping thru data.
 100  CONTINUE
C                                       Read buffer.
         CALL UVDISK ('READ', LUN, FIND, BUFF, NIO, BIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, COUNT
            CALL MSGWRT (8)
            GO TO 999
            END IF
         IPINT = BIND - LREC
C                                       Loop thru buffer.
         DO 175 J = 1,NIO
C                                       Update counters
            COUNT = COUNT + 1
            IPINT = IPINT + LREC
C                                       Compressed?
            IF (ISCMP1) THEN
               CALL RCOPY (NRP1, BUFF(IPINT), KBUFF)
               CALL ZUVXPN (NCORR, BUFF(IPINT+NRP1),
     *            BUFF(IPINT+KLOCWT), KBUFF(DPTR))
            ELSE
               CALL RCOPY (LRECF, BUFF(IPINT), KBUFF)
               END IF
C                                       Convert uvw to kilo lamda.
            IF (I1LOCB.GE.0) THEN
               IA1 = KBUFF(1+I1LOCB)/256. + 0.1
               IA2 = KBUFF(1+I1LOCB) - IA1*256. + 0.1
            ELSE
               IA1 = KBUFF(1+I1LOA1) + 0.1
               IA2 = KBUFF(1+I1LOA2) + 0.1
               END IF
            UVM = MAX (UVM, KBUFF(1+I1LOCU))
            UVM = MAX (UVM, KBUFF(1+I1LOCV))
            UVM = MAX (UVM, KBUFF(1+I1LOCW))
            UVN = 0.0
            UVN = MIN (UVN, KBUFF(1+I1LOCU))
            UVN = MIN (UVN, KBUFF(1+I1LOCV))
            UVN = MIN (UVN, KBUFF(1+I1LOCW))
            UVM = MAX (UVM, -10.0*UVN)
C                                       Convert to VIS
            CALL GETVIS (MVIS, JADR, KBUFF(DPTR), VIS, IERR1)
C                                       Accumulate statistics
            DO 115 KK = 1,MVIS
               XWT = MAX (XWT, ABS(VIS(3,KK)))
               IF (VIS(3,KK).NE.0.0) NWT = MIN (NWT, ABS(VIS(3,KK)))
               IF (IA1.EQ.IA2) THEN
                  TEMP = VIS(1,KK)
               ELSE
                  TEMP = SQRT (VIS(1,KK)*VIS(1,KK) +
     *               VIS(2,KK)*VIS(2,KK))
                  END IF
               XAMP = MAX (XAMP, TEMP)
               XAMP = MAX (XAMP, -10.0*TEMP)
 115           CONTINUE
 175        CONTINUE
         IF ((COUNT.LT.15000) .AND. (NIO.GT.0)) GO TO 100
C
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('ERROR:',I7,' INITIALIZING 1ST UV FILE')
 1075 FORMAT ('REQUESTED STOKES TYPE NOT AVAILABLE IN DATA SET')
 1100 FORMAT ('ERROR:',I7,' READING 1ST UV FILE VIS BLOCK ',I9)
      END
      SUBROUTINE FNDUPR (IRET)
C-----------------------------------------------------------------------
C   FNDUPR reads the UV files and prints those samples meeting the
C   criteria given by OPCODE and APARM.
C   Output:
C      IRET   I   Return code: 0 success or user requests end, else bad
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'UVDIF.INC'
      CHARACTER  CHS1*1, CHS2*1, ISTOKE(6)*2, JSTOKE(4,2)*2,TCHAR*12,
     *   LLCH*4, MMCH*4, KEYWRD*8
      LOGICAL   F, DOIT, EQUAL, FDOIT, UDOIT, SDOIT, NOPRNT, DOLAST,
     *   SRDOIT
      INTEGER   ICH(4), JADR(8), BIND, CINC, ITYPE(8), BUFSZ, HM(2),
     *   DD(2), J, ITT(4), LENBU, IWT(6), PHASE(8), INDEX, MVIS, IROUND,
     *   I, L, IA1, IA2, ICH1, ICH2, IERR, JERR, II, JJ, KK, KKK, IPINT,
     *   JCOR, LIMIT2, NIO, NNCH, NUMCH, K, LRECF, NUMCHM, OTYPE,
     *   BIND2, NIO2, IPINT2, NB, IB1, IB2, IERR2, BO, VO, XCOUNT,
     *   COUNT, IAPCNT, FCOUNT, UCOUNT, SCOUNT, NCORR, DIFCNT, LRECA,
     *   NUMKEY, LOCS, KEYTYP, DPTR, LA1, LA2, LB1, LB2, IERR1, SRCONT
      REAL   RSEC, DSEC, AMP(8), VIS(3,8), TEMP, TPHS, R, RADEG, U, V,
     *   W, XX, YY, YAMP, UVN, RMS, WTSC, U2, V2, W2, VIS2(3,8),
     *   XDAY, XDAY2, DIFMAX, RATMAX, RATRMS, DIFMIN, LDAY, LDAY2
      DOUBLE PRECISION DIF, SUMDIF, SUMDI2, MAXDIF
      INCLUDE 'BUFFS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PUVD.INC'
      DATA F /.FALSE./
      DATA JSTOKE /'RR','LL','RL','LR', 'I ','Q ','U ','V '/
      DATA LENBU, BO, VO /16, 1, 0/
      DATA TCHAR /' Amp Phas Wt'/
C-----------------------------------------------------------------------
      IRET = 8
      RADEG = 180.0 / 3.14159
      CINC = 2
      FCOUNT = 0
      UCOUNT = 0
      SCOUNT = 0
      SRCONT = 0
      NOPRNT = .FALSE.
C                                       range parameters
      UVM = 0.001 * UVM
      IF (XWT.LE.0.0) THEN
         XWT = 1.0
         NWT = 1.0
         END IF
      TEMP = LOG10 (XWT/0.995)
      I = TEMP + 99.0
      I =  100 - I
      WTSC = 10.0 ** I
      TEMP = NWT * WTSC
      IF (TEMP.LT.1.0) THEN
         MSGTXT = 'Full dynamic range of weights cannot be printed'
         CALL MSGWRT (6)
         END IF
C                                       Set up for compressed data.
      NCORR = (LREC - NRPARM) / CATBLK(KINAX)
      LRECF = 3 * NCORR
      DPTR = 1 + MAX (NRP1, NRP2)
      LRECA = LRECF + DPTR - 1
      CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), KLOCWT,
     *   JERR)
      IF (ISCMP1 .AND. ((JERR.NE.0) .OR. (KLOCWT.LT.0))) THEN
         IRET = 5
         MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL AXEFND (8, 'WEIGHT  ', CAT2I(KIPCN), CAT2H(KHPTP), LLOCWT,
     *   JERR)
      IF (ISCMP2 .AND. ((JERR.NE.0) .OR. (LLOCWT.LT.0))) THEN
         IRET = 5
         MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Initialize difference statistics
      DIFCNT = 0
      SUMDIF = 0.0D0
      SUMDI2 = 0.0D0
      DIFMAX = -1.0E10
      DIFMIN = 1.0E10
      MAXDIF = -1.0E10
C                                        Determine no. channels to do
      FREQ = FREQ - (CATR(KRCRP+JLOCF)-NCH) * CATR(KRCIC+JLOCF)
      NUMCHM = CATBLK(KINAX+JLOCF) - NCH + 1
      NUMCH = (NACROS - 52) / (15 * NCOR)
      NUMCH = MIN (NUMCH, NUMCHM)
      OTYPE = 1
      IF ((NUMCHM.GT.1) .AND. (NACROS.LE.80) .AND. (NCOR.EQ.1)) OTYPE =
     *   2
      IF ((OTYPE.EQ.2) .AND. (NACROS.LE.72)) OTYPE = 3
      IF (OTYPE.GT.1) NUMCH = 2
      IF (NUMCH.LT.1) THEN
         NUMCH = 1
         IF (NACROS.LE.80) OTYPE = 2
         IF (NACROS.LE.72) OTYPE = 3
         IF (NCOR.NE.2) THEN
            OTYPE = 4
            IF (NACROS.LE.80) OTYPE = 5
            IF (NACROS.LE.72) OTYPE = 6
            END IF
         END IF
      LIMIT2 = NCOR * NUMCH
      ICH1 = NCH
      ICH2 = NCH + NUMCH - 1
      NNCH = NCH - 1
      CALL FILL (8, 3, ITYPE)
      DO 10 II = 1,LIMIT2,NCOR
         NNCH = NNCH + 1
         DO 10 JJ = 1,NCOR
            INDEX = II + JJ - 1
            ICH(INDEX) = NNCH
            TEMP = CATD(KDCRV+JLOCS) + (JJ-CATR(KRCRP+JLOCS)) *
     *         CATR(KRCIC+JLOCS)
            I = IROUND(TEMP)
            L = 2
            IF (I.LT.0) L = 1
            I = ABS(I)
            ISTOKE(INDEX) = JSTOKE(I,L)
            ITYPE(INDEX) = 2
            IF (I+L.LE.3) ITYPE(INDEX) = 1
 10         CONTINUE
C                                       Convert coordinates.
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCR*CINC), LLCH)
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCD*CINC), MMCH)
      EQUAL = LLCH(1:2).EQ.'RA'
      IF (EQUAL) THEN
         CALL COORDD (1, RA, CHS1, HM, RSEC)
      ELSE
         CALL COORDD (2, RA, CHS1, HM, RSEC)
         END IF
      CALL COORDD (2, DEC, CHS2, DD, DSEC)
      FREQ = FREQ * 1.0E-9
C                                       Make sure NITER is OK
      IF (NITER.GT.NVIS) NITER = NVIS
C                                       Write header
      IF (DOCRT.GT.-2.5) THEN
         LINE = ' '
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       source names
         IF (NACROS.GE.90) THEN
            WRITE (LINE,1020) 1, NAME, CLASS, SEQ, DISK, USERID, ICH1,
     *         ICH2, BIF
         ELSE
            WRITE (LINE,1021) 1, NAME, CLASS, SEQ, DISK, USERID, ICH1,
     *         ICH2, BIF
            END IF
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (NACROS.GE.90) THEN
            WRITE (LINE,1020) 2, NAME2, CLASS2, SEQ2, DISK2, USERID,
     *         ICH1, ICH2, BIF
         ELSE
            WRITE (LINE,1021) 2, NAME2, CLASS2, SEQ2, DISK2, USERID,
     *         ICH1, ICH2, BIF
            END IF
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1025) SOURCE, LLCH, CHS1, HM, RSEC, MMCH, CHS2, DD,
     *      DSEC
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1030) FREQ, NCOR, NVIS, ISORT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (WTSC.NE.1.0) THEN
            WRITE (LINE,1031) WTSC
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         END IF
C                                       Page titles
      IF (OTYPE.EQ.1) WRITE (TITL1,1040) SOURCE, FREQ, ISORT,
     *   (ICH(JCOR), ISTOKE(JCOR), JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.2) WRITE (TITL1,1041) SOURCE, FREQ, ISORT,
     *   (ICH(JCOR), ISTOKE(JCOR), JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.3) WRITE (TITL1,1042) SOURCE, FREQ, ISORT,
     *   (ICH(JCOR), ISTOKE(JCOR), JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.4) WRITE (TITL1,1043) SOURCE, FREQ, ISORT,
     *   (ICH(JCOR), ISTOKE(JCOR), JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.5) WRITE (TITL1,1044) SOURCE, FREQ, ISORT,
     *   (ICH(JCOR), ISTOKE(JCOR), JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.6) WRITE (TITL1,1045) SOURCE, ISORT,
     *   (ICH(JCOR), ISTOKE(JCOR), JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.1) WRITE (TITL2,1050) (TCHAR, JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.2) WRITE (TITL2,1051) (TCHAR, JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.3) WRITE (TITL2,1052) (TCHAR, JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.4) WRITE (TITL2,1053) (TCHAR, JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.5) WRITE (TITL2,1054) (TCHAR, JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.6) WRITE (TITL2,1055) (TCHAR, JCOR = 1,LIMIT2)
      IF (DOCRT.GT.-2.5) THEN
         LINE = ' '
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (DOCRT.GT.-2.5) THEN
         LINE = ' '
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Initialize reading VIS. file.
      LENBU = 0
      BUFSZ = UVBFSS * 2
      CALL UVINIT ('READ', LUN, FIND, NVIS, VO, LREC, LENBU, BUFSZ,
     *   BUFF, BO, BIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
      CALL UVINIT ('READ', LUN2, FIND2, NVIS, VO, LREC2, LENBU, BUFSZ,
     *   BUFF2, BO, BIND2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1065) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
      COUNT = 0
      XCOUNT = -1
      PAGE = 1
      IAPCNT = 0
C                                       Try to handle line
      CALL SETVIS (ICH1, NUMCH, BIF, MVIS, JADR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1075)
         CALL MSGWRT (8)
         GO TO 900
         END IF
C                                       Start looping thru data.
 100  CONTINUE
C                                       Read buffer.
         COUNT = COUNT + 1
         CALL UVDISK ('READ', LUN, FIND, BUFF, NIO, BIND, IERR)
         IF (IERR.EQ.0) GO TO 105
            WRITE (MSGTXT,1100) IERR, COUNT
            CALL MSGWRT (8)
            GO TO 900
 105     CALL UVDISK ('READ', LUN2, FIND2, BUFF2, NIO2, BIND2, IERR)
         IF (IERR.EQ.0) GO TO 110
            WRITE (MSGTXT,1105) IERR, COUNT
            CALL MSGWRT (8)
            GO TO 900
 110     DOLAST = (NIO.LE.0) .OR. (NIO2.LE.0)
         IPINT = BIND - LREC
         IPINT2 = BIND2 - LREC2
         NIO = MAX (1, NIO)
C                                       Loop thru buffer.
         DO 175 J = 1,NIO
C                                       Update counters
            XCOUNT = XCOUNT + 1
            IPINT = IPINT + LREC
            IPINT2 = IPINT2 + LREC2
C                                       CONVERT
            IF (.NOT.DOLAST) THEN
C                                       Compressed?
               CALL RCOPY (NRP1, BUFF(IPINT), KBUFF)
               IF (ISCMP1) THEN
                  CALL ZUVXPN (NCORR, BUFF(IPINT+NRP1),
     *               BUFF(IPINT+KLOCWT), KBUFF(DPTR))
               ELSE
                  CALL RCOPY (LRECF, BUFF(IPINT+NRP1), KBUFF(DPTR))
                  END IF
               CALL RCOPY (NRP2, BUFF2(IPINT2), KBUFF2)
               IF (ISCMP2) THEN
                  CALL ZUVXPN (NCORR, BUFF2(IPINT2+NRP2),
     *               BUFF2(IPINT2+LLOCWT), KBUFF2(DPTR))
               ELSE
                  CALL RCOPY (LRECF, BUFF2(IPINT2+NRP2), KBUFF2(DPTR))
                  END IF
               END IF
C                                       TEST
            IF (XCOUNT.GT.0) THEN
               SRDOIT = .FALSE.
               SDOIT = .FALSE.
               UDOIT = .FALSE.
               FDOIT = .FALSE.
C                                       Decode time.
               LDAY = LBUFF(1+I1LOCT)
               LDAY2 = LBUFF2(1+I2LOCT)
               IF ((LDAY.LT.0.0) .OR. (LDAY.GT.1000.)) SDOIT = .TRUE.
               IF ((LDAY2.LT.0.0) .OR. (LDAY2.GT.1000.)) SDOIT = .TRUE.
               CALL TODHMS (LDAY, ITT)
C                                       Determine antennas.
               IF (I1LOCB.GE.0) THEN
                  LA1 = LBUFF(1+I1LOCB)/256. + 0.1
                  LA2 = LBUFF(1+I1LOCB) - LA1*256. + 0.1
               ELSE
                  LA1 = LBUFF(1+I1LOA1) + 0.1
                  LA2 = LBUFF(1+I1LOA2) + 0.1
                  END IF
               IF (I2LOCB.GE.0) THEN
                  LB1 = LBUFF2(1+I2LOCB)/256. + 0.1
                  LB2 = LBUFF2(1+I2LOCB) - LB1*256. + 0.1
               ELSE
                  LB1 = LBUFF2(1+I2LOA1) + 0.1
                  LB2 = LBUFF2(1+I2LOA2) + 0.1
                  END IF
               IF (TYPUVD.LE.0) THEN
                  IF ((LA1.LE.0) .OR. (LA1.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LA2.LE.0) .OR. (LA2.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LB1.LE.0) .OR. (LB1.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LB2.LE.0) .OR. (LB2.GT.MAXANT)) SDOIT = .TRUE.
                  END IF
C                                       Sort order swap ?
               IF ((ABS(LDAY-LDAY2).GT.1.2E-5) .OR. (LA1.NE.LB1) .OR.
     *            (LA2.NE.LB2)) THEN
C                                       Decode time.
                  XDAY = KBUFF(1+I1LOCT)
                  XDAY2 = KBUFF2(1+I2LOCT)
C                                       Determine antennas.
                  IF (I1LOCB.GE.0) THEN
                     IA1 = KBUFF(1+I1LOCB)/256. + 0.1
                     IA2 = KBUFF(1+I1LOCB) - IA1*256. + 0.1
                  ELSE
                     IA1 = KBUFF(1+I1LOA1) + 0.1
                     IA2 = KBUFF(1+I1LOA2) + 0.1
                     END IF
                  IF (I2LOCB.GE.0) THEN
                     IB1 = KBUFF2(1+I2LOCB)/256. + 0.1
                     IB2 = KBUFF2(1+I2LOCB) - IB1*256. + 0.1
                  ELSE
                     IB1 = KBUFF2(1+I2LOA1) + 0.1
                     IB2 = KBUFF2(1+I2LOA2) + 0.1
                     END IF
                  SRDOIT = (LA1.EQ.IB1) .AND. (LA2.EQ.IB2) .AND.
     *               (LB1.EQ.IA1) .AND. (LB2.EQ.IA2) .AND.
     *               (ABS(LDAY-XDAY2).LE.1.2E-5) .AND.
     *               (ABS(XDAY-LDAY2).LE.1.2E-5) .AND. (.NOT.DOLAST)
C                                       Yes: swap buffers
                  IF (SRDOIT) THEN
                     SRCONT = SRCONT + 1
                     LDAY2 = XDAY2
                     LB1 = IB1
                     LB2 = IB2
                     CALL RCOPY (LRECA, LBUFF2, KBUFF2)
                     IF (ISCMP2) THEN
                        CALL RCOPY (NRP2, BUFF2(IPINT2), LBUFF2)
                        CALL ZUVXPN (NCORR, BUFF2(IPINT2+NRP2),
     *                     BUFF2(IPINT2+LLOCWT), LBUFF2(DPTR))
                     ELSE
                        CALL RCOPY (LRECA, BUFF2(IPINT2), LBUFF2)
                        END IF
C                                       No: error
                  ELSE
                     SDOIT = .TRUE.
                     END IF
                  END IF
C                                       Convert uvw to kilo lamda.
               U = LBUFF(1+I1LOCU) * 0.001
               V = LBUFF(1+I1LOCV) * 0.001
               W = LBUFF(1+I1LOCW) * 0.001
               U2 = LBUFF2(1+I2LOCU) * 0.001
               V2 = LBUFF2(1+I2LOCV) * 0.001
               W2 = LBUFF2(1+I2LOCW) * 0.001
               UVM = MAX (UVM, U)
               UVM = MAX (UVM, V)
               UVM = MAX (UVM, W)
               UVN = U
               UVN = MIN (UVN, V)
               UVN = MIN (UVN, W)
               UVM = MAX (UVM, -10.0*UVN)
               R = U * U + V * V
C                                       UV box and range
               IF (.NOT.SDOIT) THEN
                  IF ((R.LT.UVRANG(1)) .OR. (R.GT.UVRANG(2))) GO TO 170
                  IF (DOUVBX) THEN
                     IF ((ABS(U-U2).GT.APARM(3)) .OR. (ABS(V-V2).GT.
     *                  APARM(3)) .OR. (ABS(W-W2).GT.APARM(3)))
     *                  UDOIT = .TRUE.
                     END IF
                  END IF
C                                       Convert to VIS
               CALL GETVIS (MVIS, JADR, LBUFF(DPTR), VIS, IERR1)
               CALL GETVIS (MVIS, JADR, LBUFF2(DPTR), VIS2, IERR2)
               IF ((IERR1.NE.0) .AND. (IERR2.NE.0)) GO TO 170
               FDOIT = (IERR1.NE.IERR2) .AND. (APARM(4).GT.0.0)
C                                       Accumulate statistics
               DOIT = FDOIT .OR. UDOIT .OR. SDOIT
               IF (.NOT.DOIT) THEN
                  DO 115 KK = 1,MVIS
                     IF ((VIS(3,KK).GT.0.0) .OR. (VIS2(3,KK).GT.0.0)
     *                  .OR. (DOFLAG)) THEN
                        DIFMAX = MAX (DIFMAX, VIS(1,KK), VIS(2,KK))
                        DIFMAX = MAX (DIFMAX, VIS2(1,KK), VIS2(2,KK))
                        DIFMIN = MIN (DIFMIN, VIS(1,KK), VIS(2,KK))
                        DIFMIN = MIN (DIFMIN, VIS2(1,KK), VIS2(2,KK))
                        DIF = VIS2(1,KK) - VIS(1,KK)
                        DIFCNT = DIFCNT + 1
                        SUMDIF = SUMDIF + DIF
                        SUMDI2 = SUMDI2 + DIF*DIF
                        MAXDIF = MAX (MAXDIF, ABS (DIF))
                        DIF = VIS2(2,KK) - VIS(2,KK)
                        DIFCNT = DIFCNT + 1
                        SUMDIF = SUMDIF + DIF
                        SUMDI2 = SUMDI2 + DIF*DIF
                        MAXDIF = MAX (MAXDIF, ABS (DIF))
                        END IF
 115                 CONTINUE
                  END IF
C                                       Do we want this one
               IF ((.NOT.DOIT) .AND. (DOMAX)) THEN
                  DO 130 K = 1,MVIS
                     IF ((ITYPE(K).LE.2) .AND. ((VIS(3,K).GT.0.0) .OR.
     *                  (VIS2(3,K).GT.0.0) .OR. (DOFLAG))) THEN
                        IF (ABS(VIS(1,K)-VIS2(1,K)).GT.APARM(ITYPE(K)))
     *                     FDOIT = .TRUE.
                        IF (ABS(VIS(2,K)-VIS2(2,K)).GT.APARM(ITYPE(K)))
     *                     FDOIT = .TRUE.
                        IF (APARM(4).GT.0.0) THEN
                           IF (ABS(VIS(3,K)-VIS2(3,K)).GT.APARM(4))
     *                        FDOIT = .TRUE.
                           IF ((VIS(3,K).LE.0.0).AND.(VIS2(3,K).GT.0.0))
     *                        FDOIT = .TRUE.
                           IF ((VIS(3,K).GT.0.0).AND.(VIS2(3,K).LE.0.0))
     *                        FDOIT = .TRUE.
                        ELSE
                           IF (ABS(ABS(VIS(3,K))-ABS(VIS2(3,K))).GT.
     *                        -APARM(4)) FDOIT = .TRUE.
                           END IF
                        END IF
 130                 CONTINUE
                  DOIT = FDOIT
                  END IF
               IF (.NOT.DOIT) GO TO 170
               IF (IAPCNT.GE.NITER) NOPRNT = .TRUE.
               IF ((NOPRNT) .AND. (DOCRT.GT.0.0)) GO TO 200
               IF (.NOT.NOPRNT) IAPCNT = IAPCNT + 1
               IF (FDOIT) FCOUNT = FCOUNT + 1
               IF (UDOIT) UCOUNT = UCOUNT + 1
               IF (SDOIT) SCOUNT = SCOUNT + 1
               IF (NOPRNT) GO TO 170
C                                       Get vis.
               DO 165 NB = 1,2
               K = 0
               YAMP = 0.0
               DO 140 KK = ICH1,ICH2
                  DO 140 KKK = 1,NCOR
                     K = K + 1
                     INDEX = 1 + (KK-1) * INCF + (KKK-1) * INCS +
     *                  (BIF-1) * INCIF + MAX (NRP1, NRP2)
                     IF (NB.EQ.1) THEN
                        XX = LBUFF(INDEX)
                        YY = LBUFF(INDEX+1)
                        TPHS = LBUFF(INDEX+2)
                     ELSE
                        XX = LBUFF2(INDEX)
                        YY = LBUFF2(INDEX+1)
                        TPHS = LBUFF2(INDEX+2)
                        END IF
                     TPHS = MAX (-99.0, MIN (99.0, TPHS*WTSC))
                     IWT(K) = IROUND (TPHS)
                     IF (LA1.EQ.LA2) THEN
                        AMP(K) = XX
                        TPHS = 0.0
                     ELSE
                        AMP(K) = SQRT(XX*XX+YY*YY)
                        TPHS = 57.296 * ATAN2 (YY, XX+1.0E-20)
                        END IF
                     PHASE(K) = IROUND (TPHS)
                     XAMP = MAX (XAMP, AMP(K))
                     YAMP = MIN (YAMP, AMP(K))
 140                 CONTINUE
               IF (NB.NE.1) THEN
                  LA1 = LB1
                  LA2 = LB2
                  U = U2
                  V = V2
                  W = W2
                  CALL TODHMS (LDAY2, ITT)
                  END IF
C                                       Write VIS data: CRT
               XAMP = MAX (XAMP, -10.0*YAMP)
C                                       low UVW, low flux
               IF ((XAMP.GT.99.9) .AND. (OTYPE.GT.2)) GO TO 150
                  IF ((UVM.GT.9999.98) .AND. (OTYPE.LT.5)) GO TO 145
                     IF (OTYPE.EQ.1) WRITE (LINE,1140,ERR=160)
     *                  XCOUNT, ITT, LA1, LA2, U, V, W,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     IF (OTYPE.EQ.2) WRITE (LINE,1141,ERR=160)
     *                  XCOUNT, ITT, LA1, LA2, U, V,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     IF (OTYPE.EQ.3) WRITE (LINE,1142,ERR=160)
     *                  XCOUNT, ITT, LA1, LA2, U, V,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     IF (OTYPE.EQ.4) WRITE (LINE,1143,ERR=160)
     *                  XCOUNT, ITT, LA1, LA2, U, V,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     IF (OTYPE.EQ.5) WRITE (LINE,1144,ERR=160)
     *                  XCOUNT, ITT, LA1, LA2,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     IF (OTYPE.EQ.6) WRITE (LINE,1145,ERR=160)
     *                  ITT(2), ITT(3), ITT(4), LA1, LA2,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     GO TO 160
C                                       High UVW, low flux
 145              CONTINUE
                     IF (OTYPE.EQ.1) WRITE (LINE,1146,ERR=160)
     *                  XCOUNT, ITT, LA1, LA2, U, V, W,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     IF (OTYPE.EQ.2) WRITE (LINE,1147,ERR=160)
     *                  XCOUNT, ITT, LA1, LA2, U, V,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     IF (OTYPE.EQ.3) WRITE (LINE,1148,ERR=160)
     *                  XCOUNT, ITT, LA1, LA2, U, V,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     IF (OTYPE.EQ.4) WRITE (LINE,1149,ERR=160)
     *                  XCOUNT, ITT, LA1, LA2, U, V,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     GO TO 160
C                                       low UVW, higher flux
 150           CONTINUE
                  IF ((UVM.GT.9999.98) .AND. (OTYPE.LT.5)) GO TO 155
                     IF (OTYPE.EQ.3) WRITE (LINE,1150,ERR=160)
     *                  XCOUNT, ITT, LA1, LA2, U, V,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     IF (OTYPE.EQ.4) WRITE (LINE,1151,ERR=160)
     *                  XCOUNT, ITT, LA1, LA2, U, V,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     IF (OTYPE.EQ.5) WRITE (LINE,1152,ERR=160)
     *                  XCOUNT, ITT, LA1, LA2,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     IF (OTYPE.EQ.6) WRITE (LINE,1153,ERR=160)
     *                  ITT(2), ITT(3), ITT(4), LA1, LA2,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     GO TO 160
C                                       High UVW, higher flux
 155              CONTINUE
                     IF (OTYPE.EQ.3) WRITE (LINE,1155,ERR=160)
     *                  XCOUNT, ITT, LA1, LA2, U, V,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     IF (OTYPE.EQ.4) WRITE (LINE,1156,ERR=160)
     *                  XCOUNT, ITT, LA1, LA2, U, V,
     *                  (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
 160           CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
 165           CONTINUE
               END IF
 170        IF (DOLAST) THEN
               GO TO 200
            ELSE
               CALL RCOPY (LRECA, KBUFF, LBUFF)
               CALL RCOPY (LRECA, KBUFF2, LBUFF2)
               END IF
 175        CONTINUE
         GO TO 100
C                                       Number lines printed
 200  IF (DOCRT.LE.0.0) THEN
         LINE = ' '
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1200) IAPCNT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      WRITE (MSGTXT,1200) IAPCNT
      IF (IAPCNT.GT.0) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
      IF ((IAPCNT.LT.NITER) .OR. (NITER.EQ.NVIS)) GO TO 210
         IF (DOCRT.LE.0.0) THEN
            WRITE (LINE,1202)
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IPCNT = IPCNT + 2
         WRITE (MSGTXT,1203)
         CALL MSGWRT (6)
 210  IPCNT = IPCNT + 2
      IF (DOCRT.LE.0.0) THEN
         WRITE (LINE,1210) FCOUNT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1211) UCOUNT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1212) SCOUNT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1213) SRCONT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      WRITE (MSGTXT,1210) FCOUNT
      IF (FCOUNT.GT.0) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
      WRITE (MSGTXT,1211) UCOUNT
      IF (UCOUNT.GT.0) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
      WRITE (MSGTXT,1212) SCOUNT
      IF (SCOUNT.GT.0) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
      WRITE (MSGTXT,1213) SRCONT
      IF (SRCONT.GT.0) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
C                                       Write statistics in header of
C                                       first input file
 900  IF (DIFCNT.LT.2) THEN
         MSGTXT = 'NO POINTS EVEN CHECKED FOR DIFFERENCES IN FLUX'
         CALL MSGWRT (7)
         DIFMAX = 1.0E12
         DIFMIN = 1.0E12
         DIFCNT = 2
         SUMDIF = 1.
         SUMDI2 = 2.
         END IF
      DIFMAX = MAX (ABS(DIFMAX), ABS(DIFMIN))
      SUMDIF = SUMDIF / DIFCNT
      SUMDI2 = SUMDI2 / DIFCNT
      RMS = SQRT (ABS (SUMDI2 - SUMDIF*SUMDIF) / (DIFCNT-1))
      RATMAX = MAXDIF / DIFMAX
      RATRMS = RMS / DIFMAX
      IF ((IERR.EQ.0) .AND. (DOCRT.LE.0.0)) THEN
         IF (DOCRT.GT.-2.5) THEN
            LINE = ' '
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         WRITE (LINE,1900) RATMAX, RATRMS
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      WRITE (MSGTXT,1900) RATMAX, RATRMS
      CALL MSGWRT (5)
      RATMAX = MAX (RATMAX, 1.57683E-30)
      RATRMS = MAX (RATRMS, 1.57683E-30)
      NUMKEY = 1
      LOCS = 1
      KEYTYP = 2
      KEYWRD = 'RATMAX  '
      CALL CATKEY ('WRIT', DISK, CNO, KEYWRD, NUMKEY, LOCS, RATMAX,
     *   KEYTYP,  LBUFF, JERR)
      KEYWRD = 'RATRMS  '
      CALL CATKEY ('WRIT', DISK, CNO, KEYWRD, NUMKEY, LOCS, RATRMS,
     *   KEYTYP,  LBUFF, JERR)
C                                       Close files.
      IF (IERR.LE.0) IRET = 0
      CALL LPCLOS (PLUN, PIND, IPCNT, IERR)
C
      CALL MAPCLS ('READ', DISK, CNO, LUN, FIND, CATBLK, F, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1910) IERR
         CALL MSGWRT (6)
         END IF
      CALL MAPCLS ('READ', DISK2, CNO2, LUN2, FIND2, CAT2I, F, BUFF,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1920) IERR
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('File',I1,' = ',A12,'.',A6,'.',I4,'   Vol =',I2,
     *   '    UserID =',I5,5X,'Channels =',I5,' to ',I5,' BIF=',I3)
 1021 FORMAT (I1,1X,A12,'.',A6,'.',I4,'  Vol=',I2,'  User=',I5,
     *   '  Chan=',I4,' to',I4,'  IF=',I3)
 1025 FORMAT ('Source= ',A8,3X,A4,' = ',A1,I2.2,I3.2,F6.2,3X,A4,' = ',
     *   A1,I2.2,I3.2,F5.1)
 1030 FORMAT ('Freq=',F13.9,' GHz   ncor=',I3,'   No. vis=',I10,
     *   '   Sort order= ',A2)
 1031 FORMAT ('Weights have been multiplied by',1PE10.2)
 1040 FORMAT ('Source= ',A8,4X,'Freq= ',F13.9,4X,'Sort= ',A2,1X,
     *   5(4X,I4,1X,A2,4X))
 1041 FORMAT (A,4X,'Freq=',F13.9,4X,'Sort= ',A2,1X,5(4X,I4,1X,A2,4X))
 1042 FORMAT (A,4X,'Freq=',F13.9,4X,'Sort= ',A2,1X,4(3X,I4,1X,A2,4X))
 1043 FORMAT (A,4X,'Freq=',F13.9,4X,'Sort= ',A2,4(3X,I4,1X,A2,4X))
 1044 FORMAT (A,F13.9,1X,A2,4(3X,I4,1X,A2,4X))
 1045 FORMAT (A,2X,A2,2X,4(3X,I4,1X,A2,4X))
 1050 FORMAT ('  Vis #',5X,'IAT',6X,'Ant   U(Klam)  V(Klam)  W(Klam)',
     *   5(3X,A))
 1051 FORMAT ('  Vis #',5X,'IAT',6X,'Ant   U(Klam)  V(Klam)',5(3X,A))
 1052 FORMAT ('  Vis #',5X,'IAT',6X,'Ant   U(Klam)  V(Klam)',4(2X,A))
 1053 FORMAT (' Vis #',5X,'IAT',6X,'Ant   U(Klam)  V(Klam)',4(2X,A))
 1054 FORMAT (' Vis #',5X,'IAT',6X,'Ant ',4(2X,A))
 1055 FORMAT (2X,'IAT',5X,'Ant ',4(2X,A))
 1060 FORMAT ('ERROR:',I7,' INITIALIZING 1ST UV FILE')
 1065 FORMAT ('ERROR:',I7,' INITIALIZING 2ND UV FILE')
 1075 FORMAT ('REQUESTED STOKES TYPE NOT AVAILABLE IN DATA SET')
 1100 FORMAT ('ERROR:',I7,' READING 1ST UV FILE VIS BLOCK ',I9)
 1105 FORMAT ('ERROR:',I7,' READING 2ND UV FILE VIS BLOCK ',I9)
 1140 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,3F9.2,5(F8.3,I4,I3))
 1141 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.2,5(F8.3,I4,I3))
 1142 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.2,4(F7.3,I4,I3))
 1143 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.2,4(F7.3,I4,I3))
 1144 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,4(F7.3,I4,I3))
 1145 FORMAT (I2.2,2(':',I2.2),I3,'-',I2,4(F7.3,I4,I3))
 1146 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,3F9.0,5(F8.3,I4,I3))
 1147 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.0,5(F8.3,I4,I3))
 1148 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.0,4(F7.3,I4,I3))
 1149 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.0,4(F7.3,I4,I3))
 1150 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.2,4(F7.1,I4,I3))
 1151 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.2,4(F7.1,I4,I3))
 1152 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,4(F7.1,I4,I3))
 1153 FORMAT (I2.2,2(':',I2.2),I3,'-',I2,4(F7.1,I4,I3))
 1155 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.0,4(F7.1,I4,I3))
 1156 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F9.0,4(F7.1,I4,I3))
 1200 FORMAT (I5,' Points printed')
 1202 FORMAT ('0********** PRINT LIMIT REACHED: MORE POINTS MAY EXIST',
     *   ' **********')
 1203 FORMAT ('WARNING: PRINT LIMIT REACHED, NOT ALL POINTS LISTED')
 1210 FORMAT (I8,' Points found for flux differences')
 1211 FORMAT (I8,' Points found for u,v,w differences')
 1212 FORMAT (I8,' Points found for other differences')
 1213 FORMAT (I8,' Points swapped for apparent sort order reversal')
 1900 FORMAT ('Visibilty Max diff/peak =',1PE11.3,'  RMS/peak =',E11.3)
 1910 FORMAT ('ERROR:',I7,' CLOSING 1ST UV FILE ')
 1920 FORMAT ('ERROR:',I7,' CLOSING 2ND UV FILE ')
      END
      SUBROUTINE SETVIS (ICH, NCH, IFNUM, MVIS, JADR, IERR)
C-----------------------------------------------------------------------
C   SETVIS setup the array JADR for reformatting uv data.  There is also
C   a check to make sure the desired data is available.  Calls to GETVIS
C   will reformat the data.  Needs values set by UVPGET and VHDRIN.
C   Only 1 IF will be processed.
C   Inputs:
C      ICH     I      First line channel desired.
C      NCH     I      Number of line channels desired
C      IFNUM   I      IF number wanted.
C   Output:
C      MVIS    I      Number of visibilities in requested output format
C      JADR    I(*)   Pointers to the first and second visibility
C                     input records to be used in the output record.
C      IERR    I      Error flag. 0 =>OK, otherwise data unavailable.
C   Common (input):
C      /MAPHDR/  must have uv header
C      /UVHDR/   must be initialized by UVPGET
C-----------------------------------------------------------------------
      INTEGER   ICH, NCH, IFNUM, MVIS, JADR(*), IERR
C
      INTEGER   JNCS, JNCF, JNCIF, I, IOFF
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IERR = 1
C                                       Set pointers.
      JNCS = INCS / 3
      JNCF = INCF / 3
      JNCIF = INCIF / 3
      IF (JNCIF.LT.1) JNCIF = 1
      IF ((ICH.LT.1) .OR. (ICH+NCH-1.GT.CATBLK(KINAX+JLOCF))) GO TO 999
      IOFF = (IFNUM-1) * JNCIF + (ICH-1) * JNCF
C                                       Check IF
      IF ((IFNUM.GT.1) .AND. (IFNUM.GT.CATBLK(KINAX+JLOCIF))) GO TO 999
      IF (IFNUM.LE.0) GO TO 999
C                                       pure correlators
      IERR = 0
      MVIS = NCOR * NCH
      DO 10 I = 1,MVIS
         JADR(I) = IOFF + (I-1) * JNCS + 1
         IF (MOD(I,NCOR).EQ.0) IOFF = IOFF + JNCF
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GETVIS (MVIS, JADR, DATA, VIS, IERR)
C-----------------------------------------------------------------------
C   GETVIS gets and reformats uv data. Requires setup by SETVIS.
C   Inputs:
C      MVIS          I    Number of visibilities wanted.
C      JADR(MVIS)    I    Pointers set by SETVIS.
C      DATA(3,*)     R    Visibility portion of input data.
C   Outputs:
C      VIS(3,MVIS)   R    Visibilities.
C      IERR          I    Error code, 0=>OK,
C                            2 = bad input.
C-----------------------------------------------------------------------
      INTEGER   MVIS, JADR(*), IERR
      REAL      DATA(3,*), VIS(3,*)
C
      INTEGER   I, IP
C-----------------------------------------------------------------------
      VIS(1,1) = 0.0
      VIS(2,1) = 0.0
      VIS(3,1) = 0.0
C                                       Check input.
      IERR = 2
      IF (MVIS.LE.0) GO TO 999
      IERR = 1
      DO 50 I = 1,MVIS
         IP = JADR(I)
         VIS(1,I) = DATA(1,IP)
         VIS(2,I) = DATA(2,IP)
         VIS(3,I) = DATA(3,IP)
         IF (VIS(3,I).GT.0.0) IERR = 0
 50      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FNDUBX (IRET)
C-----------------------------------------------------------------------
C   FNDUBX reads the UV files and prints those samples showing
C   differences in u,v,w.
C   Output:
C      IRET   I   Return code: 0 success or user requests end, else bad
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'UVDIF.INC'
      CHARACTER  CHS1*1, CHS2*1, ISTOKE(6)*2, JSTOKE(4,2)*2,TCHAR*12,
     *   LLCH*4, MMCH*4, KEYWRD*8
      LOGICAL   F, DOIT, EQUAL, FDOIT, UDOIT, SDOIT, NOPRNT, DOLAST,
     *   SRDOIT
      INTEGER   ICH(4), JADR(8), BIND, CINC, ITYPE(8), BUFSZ, HM(2),
     *   DD(2), J, ITT(4), LENBU, IWT(6), PHASE(8), INDEX, MVIS, IROUND,
     *   I, L, IA1, IA2, ICH1, ICH2, IERR, JERR, II, JJ, KK, KKK, IPINT,
     *   JCOR, LIMIT2, NIO, NNCH, NUMCH, K, LRECF, NUMCHM, OTYPE, LCOR,
     *   BIND2, NIO2, IPINT2, NB, IB1, IB2, IERR2, BO, VO, XCOUNT,
     *   COUNT, IAPCNT, FCOUNT, UCOUNT, SCOUNT, NCORR, DIFCNT, LRECA,
     *   NUMKEY, LOCS, KEYTYP, DPTR, LA1, LA2, LB1, LB2, IERR1, SRCONT
      REAL   RSEC, DSEC, AMP(8), VIS(3,8), TEMP, TPHS, R, RADEG, U, V,
     *   W, XX, YY, YAMP, UVN, RMS, WTSC, U2, V2, W2, VIS2(3,8),
     *   XDAY, XDAY2, DIFMAX, RATMAX, RATRMS, DIFMIN, LDAY, LDAY2
      DOUBLE PRECISION DIF, SUMDIF, SUMDI2, MAXDIF
      INCLUDE 'BUFFS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PUVD.INC'
      DATA F /.FALSE./
      DATA JSTOKE /'RR','LL','RL','LR', 'I ','Q ','U ','V '/
      DATA LENBU, BO, VO /16, 1, 0/
      DATA TCHAR /' Amp Phas Wt'/
C-----------------------------------------------------------------------
      IRET = 8
      RADEG = 180.0 / 3.14159
      CINC = 2
      FCOUNT = 0
      UCOUNT = 0
      SCOUNT = 0
      SRCONT = 0
      NOPRNT = .FALSE.
C                                       range parameters
      UVM = 0.001 * UVM
      IF (XWT.LE.0.0) THEN
         XWT = 1.0
         NWT = 1.0
         END IF
      TEMP = LOG10 (XWT/0.995)
      I = TEMP + 99.0
      I =  100 - I
      WTSC = 10.0 ** I
      TEMP = NWT * WTSC
      IF (TEMP.LT.1.0) THEN
         MSGTXT = 'Full dynamic range of weights cannot be printed'
         CALL MSGWRT (6)
         END IF
C                                       Set up for compressed data.
      NCORR = (LREC - NRPARM) / CATBLK(KINAX)
      LRECF = 3 * NCORR
      DPTR = 1 + MAX (NRP1, NRP2)
      LRECA = LRECF + DPTR - 1
      CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), KLOCWT,
     *   JERR)
      IF (ISCMP1 .AND. ((JERR.NE.0) .OR. (KLOCWT.LT.0))) THEN
         IRET = 5
         MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL AXEFND (8, 'WEIGHT  ', CAT2I(KIPCN), CAT2H(KHPTP), LLOCWT,
     *   JERR)
      IF (ISCMP2 .AND. ((JERR.NE.0) .OR. (LLOCWT.LT.0))) THEN
         IRET = 5
         MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Initialize difference statistics
      DIFCNT = 0
      SUMDIF = 0.0D0
      SUMDI2 = 0.0D0
      DIFMAX = -1.0E10
      DIFMIN = 1.0E10
      MAXDIF = -1.0E10
C                                        Determine no. channels to do
      FREQ = FREQ - (CATR(KRCRP+JLOCF)-NCH) * CATR(KRCIC+JLOCF)
      NUMCHM = CATBLK(KINAX+JLOCF) - NCH + 1
      LCOR = MIN (2, NCOR)
      NUMCH = (NACROS - 61) / (16 * LCOR)
      NUMCH = MIN (NUMCH, NUMCHM)
      OTYPE = 1
      IF (NUMCH.LT.1) THEN
         NUMCH = 1
         IF (NACROS.LE.80) LCOR = 1
         IF (NACROS.LE.72) OTYPE = 2
         END IF
      LIMIT2 = LCOR * NUMCH
      ICH1 = NCH
      ICH2 = NCH + NUMCH - 1
      NNCH = NCH - 1
      CALL FILL (8, 3, ITYPE)
      DO 10 II = 1,LIMIT2,LCOR
         NNCH = NNCH + 1
         DO 10 JJ = 1,LCOR
            INDEX = II + JJ - 1
            ICH(INDEX) = NNCH
            TEMP = CATD(KDCRV+JLOCS) + (JJ-CATR(KRCRP+JLOCS)) *
     *         CATR(KRCIC+JLOCS)
            I = IROUND(TEMP)
            L = 2
            IF (I.LT.0) L = 1
            I = ABS(I)
            ISTOKE(INDEX) = JSTOKE(I,L)
            ITYPE(INDEX) = 2
            IF (I+L.LE.3) ITYPE(INDEX) = 1
 10         CONTINUE
C                                       Convert coordinates.
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCR*CINC), LLCH)
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCD*CINC), MMCH)
      EQUAL = LLCH(1:2).EQ.'RA'
      IF (EQUAL) THEN
         CALL COORDD (1, RA, CHS1, HM, RSEC)
      ELSE
         CALL COORDD (2, RA, CHS1, HM, RSEC)
         END IF
      CALL COORDD (2, DEC, CHS2, DD, DSEC)
      FREQ = FREQ * 1.0E-9
C                                       Make sure NITER is OK
      IF (NITER.GT.NVIS) NITER = NVIS
C                                       Write header
      IF (DOCRT.GT.-2.5) THEN
         LINE = ' '
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       source names
         IF (NACROS.GE.90) THEN
            WRITE (LINE,1020) 1, NAME, CLASS, SEQ, DISK, USERID, ICH1,
     *         ICH2, BIF
         ELSE
            WRITE (LINE,1021) 1, NAME, CLASS, SEQ, DISK, USERID, ICH1,
     *         ICH2, BIF
            END IF
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (NACROS.GE.90) THEN
            WRITE (LINE,1020) 2, NAME2, CLASS2, SEQ2, DISK2, USERID,
     *         ICH1, ICH2, BIF
         ELSE
            WRITE (LINE,1021) 2, NAME2, CLASS2, SEQ2, DISK2, USERID,
     *         ICH1, ICH2, BIF
            END IF
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1025) SOURCE, LLCH, CHS1, HM, RSEC, MMCH, CHS2, DD,
     *      DSEC
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1030) FREQ, NCOR, NVIS, ISORT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (WTSC.NE.1.0) THEN
            WRITE (LINE,1031) WTSC
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         END IF
C                                       Page titles
      IF (OTYPE.EQ.1) WRITE (TITL1,1040) SOURCE, FREQ, ISORT,
     *   (ICH(JCOR), ISTOKE(JCOR), JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.2) WRITE (TITL1,1041) SOURCE, FREQ, ISORT,
     *   (ICH(JCOR), ISTOKE(JCOR), JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.1) WRITE (TITL2,1050) (TCHAR, JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.2) WRITE (TITL2,1051) (TCHAR, JCOR = 1,LIMIT2)
      IF (DOCRT.GT.-2.5) THEN
         LINE = ' '
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (DOCRT.GT.-2.5) THEN
         LINE = ' '
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Initialize reading VIS. file.
      LENBU = 0
      BUFSZ = UVBFSS * 2
      CALL UVINIT ('READ', LUN, FIND, NVIS, VO, LREC, LENBU, BUFSZ,
     *   BUFF, BO, BIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
      CALL UVINIT ('READ', LUN2, FIND2, NVIS, VO, LREC2, LENBU, BUFSZ,
     *   BUFF2, BO, BIND2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1065) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
      COUNT = 0
      XCOUNT = -1
      PAGE = 1
      IAPCNT = 0
C                                       Try to handle line
      CALL SETVIS (ICH1, NUMCH, BIF, MVIS, JADR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1075)
         CALL MSGWRT (8)
         GO TO 900
         END IF
C                                       Start looping thru data.
 100  CONTINUE
C                                       Read buffer.
         COUNT = COUNT + 1
         CALL UVDISK ('READ', LUN, FIND, BUFF, NIO, BIND, IERR)
         IF (IERR.EQ.0) GO TO 105
            WRITE (MSGTXT,1100) IERR, COUNT
            CALL MSGWRT (8)
            GO TO 900
 105     CALL UVDISK ('READ', LUN2, FIND2, BUFF2, NIO2, BIND2, IERR)
         IF (IERR.EQ.0) GO TO 110
            WRITE (MSGTXT,1105) IERR, COUNT
            CALL MSGWRT (8)
            GO TO 900
 110     DOLAST = (NIO.LE.0) .OR. (NIO2.LE.0)
         IPINT = BIND - LREC
         IPINT2 = BIND2 - LREC2
         NIO = MAX (1, NIO)
C                                       Loop thru buffer.
         DO 175 J = 1,NIO
C                                       Update counters
            XCOUNT = XCOUNT + 1
            IPINT = IPINT + LREC
            IPINT2 = IPINT2 + LREC2
C                                       CONVERT
            IF (.NOT.DOLAST) THEN
C                                       Compressed?
               CALL RCOPY (NRP1, BUFF(IPINT), KBUFF)
               IF (ISCMP1) THEN
                  CALL ZUVXPN (NCORR, BUFF(IPINT+NRP1),
     *               BUFF(IPINT+KLOCWT), KBUFF(DPTR))
               ELSE
                  CALL RCOPY (LRECF, BUFF(IPINT+NRP1), KBUFF(DPTR))
                  END IF
               CALL RCOPY (NRP2, BUFF2(IPINT2), KBUFF2)
               IF (ISCMP2) THEN
                  CALL ZUVXPN (NCORR, BUFF2(IPINT2+NRP2),
     *               BUFF2(IPINT2+LLOCWT), KBUFF2(DPTR))
               ELSE
                  CALL RCOPY (LRECF, BUFF2(IPINT2+NRP2), KBUFF2(DPTR))
                  END IF
               END IF
C                                       TEST
            IF (XCOUNT.GT.0) THEN
               SRDOIT = .FALSE.
               SDOIT = .FALSE.
               UDOIT = .FALSE.
               FDOIT = .FALSE.
C                                       Decode time.
               LDAY = LBUFF(1+I1LOCT)
               LDAY2 = LBUFF2(1+I2LOCT)
               IF ((LDAY.LT.0.0) .OR. (LDAY.GT.1000.)) SDOIT = .TRUE.
               IF ((LDAY2.LT.0.0) .OR. (LDAY2.GT.1000.)) SDOIT = .TRUE.
               CALL TODHMS (LDAY, ITT)
C                                       Determine antennas.
               IF (I1LOCB.GE.0) THEN
                  LA1 = LBUFF(1+I1LOCB)/256. + 0.1
                  LA2 = LBUFF(1+I1LOCB) - LA1*256. + 0.1
               ELSE
                  LA1 = LBUFF(1+I1LOA1) + 0.1
                  LA2 = LBUFF(1+I1LOA2) + 0.1
                  END IF
               IF (I2LOCB.GE.0) THEN
                  LB1 = LBUFF2(1+I2LOCB)/256. + 0.1
                  LB2 = LBUFF2(1+I2LOCB) - LB1*256. + 0.1
               ELSE
                  LB1 = LBUFF2(1+I2LOA1) + 0.1
                  LB2 = LBUFF2(1+I2LOA2) + 0.1
                  END IF
               IF (TYPUVD.LE.0) THEN
                  IF ((LA1.LE.0) .OR. (LA1.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LA2.LE.0) .OR. (LA2.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LB1.LE.0) .OR. (LB1.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LB2.LE.0) .OR. (LB2.GT.MAXANT)) SDOIT = .TRUE.
                  END IF
C                                       Sort order swap ?
               IF ((ABS(LDAY-LDAY2).GT.1.2E-5) .OR. (LA1.NE.LB1) .OR.
     *            (LA2.NE.LB2)) THEN
C                                       Decode time.
                  XDAY = KBUFF(1+I1LOCT)
                  XDAY2 = KBUFF2(1+I2LOCT)
C                                       Determine antennas.
                  IF (I1LOCB.GE.0) THEN
                     IA1 = KBUFF(1+I1LOCB)/256. + 0.1
                     IA2 = KBUFF(1+I1LOCB) - IA1*256. + 0.1
                  ELSE
                     IA1 = KBUFF(1+I1LOA1) + 0.1
                     IA2 = KBUFF(1+I1LOA2) + 0.1
                     END IF
                  IF (I2LOCB.GE.0) THEN
                     IB1 = KBUFF2(1+I2LOCB)/256. + 0.1
                     IB2 = KBUFF2(1+I2LOCB) - IB1*256. + 0.1
                  ELSE
                     IB1 = KBUFF2(1+I2LOA1) + 0.1
                     IB2 = KBUFF2(1+I2LOA2) + 0.1
                     END IF
                  SRDOIT = (LA1.EQ.IB1) .AND. (LA2.EQ.IB2) .AND.
     *               (LB1.EQ.IA1) .AND. (LB2.EQ.IA2) .AND.
     *               (ABS(LDAY-XDAY2).LE.1.2E-5) .AND.
     *               (ABS(XDAY-LDAY2).LE.1.2E-5) .AND. (.NOT.DOLAST)
C                                       Yes: swap buffers
                  IF (SRDOIT) THEN
                     SRCONT = SRCONT + 1
                     LDAY2 = XDAY2
                     LB1 = IB1
                     LB2 = IB2
                     CALL RCOPY (LRECA, LBUFF2, KBUFF2)
                     CALL RCOPY (NRP2, BUFF2(IPINT2), LBUFF2)
                     IF (ISCMP2) THEN
                        CALL ZUVXPN (NCORR, BUFF2(IPINT2+NRP2),
     *                     BUFF2(IPINT2+LLOCWT), LBUFF2(DPTR))
                     ELSE
                        CALL RCOPY (LRECF, BUFF2(IPINT2+NRP2),
     *                     LBUFF2(DPTR))
                        END IF
C                                       No: error
                  ELSE
                     SDOIT = .TRUE.
                     END IF
                  END IF
C                                       Convert uvw to kilo lamda.
               U = LBUFF(1+I1LOCU) * 0.001
               V = LBUFF(1+I1LOCV) * 0.001
               W = LBUFF(1+I1LOCW) * 0.001
               U2 = LBUFF2(1+I2LOCU) * 0.001
               V2 = LBUFF2(1+I2LOCV) * 0.001
               W2 = LBUFF2(1+I2LOCW) * 0.001
               UVM = MAX (UVM, U)
               UVM = MAX (UVM, V)
               UVM = MAX (UVM, W)
               UVN = U
               UVN = MIN (UVN, V)
               UVN = MIN (UVN, W)
               UVM = MAX (UVM, -10.0*UVN)
               R = U * U + V * V
C                                       UV box and range
               IF (.NOT.SDOIT) THEN
                  IF ((R.LT.UVRANG(1)) .OR. (R.GT.UVRANG(2))) GO TO 170
                  IF (DOUVBX) THEN
                     IF ((ABS(U-U2).GT.APARM(3)) .OR. (ABS(V-V2).GT.
     *                  APARM(3)) .OR. (ABS(W-W2).GT.APARM(3)))
     *                  UDOIT = .TRUE.
                     END IF
                  END IF
C                                       Convert to VIS
               CALL GETVIS (MVIS, JADR, LBUFF(DPTR), VIS, IERR1)
               CALL GETVIS (MVIS, JADR, LBUFF2(DPTR), VIS2, IERR2)
               IF ((IERR1.NE.0) .AND. (IERR2.NE.0)) GO TO 170
               FDOIT = (IERR1.NE.IERR2) .AND. (APARM(4).GT.0.0)
C                                       Accumulate statistics
               DOIT = FDOIT .OR. UDOIT .OR. SDOIT
               IF (.NOT.DOIT) THEN
                  DO 115 KK = 1,MVIS
                     IF ((VIS(3,KK).GT.0.0) .OR. (VIS2(3,KK).GT.0.0)
     *                  .OR. (DOFLAG)) THEN
                        DIFMAX = MAX (DIFMAX, VIS(1,KK), VIS(2,KK))
                        DIFMAX = MAX (DIFMAX, VIS2(1,KK), VIS2(2,KK))
                        DIFMIN = MIN (DIFMIN, VIS(1,KK), VIS(2,KK))
                        DIFMIN = MIN (DIFMIN, VIS2(1,KK), VIS2(2,KK))
                        DIF = VIS2(1,KK) - VIS(1,KK)
                        DIFCNT = DIFCNT + 1
                        SUMDIF = SUMDIF + DIF
                        SUMDI2 = SUMDI2 + DIF*DIF
                        MAXDIF = MAX (MAXDIF, ABS (DIF))
                        DIF = VIS2(2,KK) - VIS(2,KK)
                        DIFCNT = DIFCNT + 1
                        SUMDIF = SUMDIF + DIF
                        SUMDI2 = SUMDI2 + DIF*DIF
                        MAXDIF = MAX (MAXDIF, ABS (DIF))
                        END IF
 115                 CONTINUE
                  END IF
               IF (.NOT.DOIT) GO TO 170
               IF (IAPCNT.GE.NITER) NOPRNT = .TRUE.
               IF ((NOPRNT) .AND. (DOCRT.GT.0.0)) GO TO 200
               IF (.NOT.NOPRNT) IAPCNT = IAPCNT + 1
               IF (FDOIT) FCOUNT = FCOUNT + 1
               IF (UDOIT) UCOUNT = UCOUNT + 1
               IF (SDOIT) SCOUNT = SCOUNT + 1
               IF (NOPRNT) GO TO 170
C                                       Get vis.
               DO 165 NB = 1,2
               K = 0
               YAMP = 0.0
               DO 140 KK = ICH1,ICH2
                  DO 140 KKK = 1,LCOR
                     K = K + 1
                     INDEX = 1 + (KK-1) * INCF + (KKK-1) * INCS +
     *                  (BIF-1) * INCIF + MAX (NRP1, NRP2)
                     IF (NB.EQ.1) THEN
                        XX = LBUFF(INDEX)
                        YY = LBUFF(INDEX+1)
                        TPHS = LBUFF(INDEX+2)
                     ELSE
                        XX = LBUFF2(INDEX)
                        YY = LBUFF2(INDEX+1)
                        TPHS = LBUFF2(INDEX+2)
                        END IF
                     TPHS = MAX (-99.0, MIN (99.0, TPHS*WTSC))
                     IWT(K) = IROUND (TPHS)
                     IF (LA1.EQ.LA2) THEN
                        AMP(K) = XX
                        TPHS = 0.0
                     ELSE
                        AMP(K) = SQRT(XX*XX+YY*YY)
                        TPHS = 57.296 * ATAN2 (YY, XX+1.0E-20)
                        END IF
                     PHASE(K) = IROUND (TPHS)
                     XAMP = MAX (XAMP, AMP(K))
                     YAMP = MIN (YAMP, AMP(K))
 140                 CONTINUE
               IF (NB.NE.1) THEN
                  LA1 = LB1
                  LA2 = LB2
                  U = U2
                  V = V2
                  W = W2
                  CALL TODHMS (LDAY2, ITT)
                  END IF
C                                       Write VIS data: CRT
               XAMP = MAX (XAMP, -10.0*YAMP)
C                                       low UVW
               IF (UVM.LE.9999.98) THEN
                  IF (OTYPE.EQ.1) THEN
                     WRITE (LINE,1140,ERR=160) XCOUNT, ITT, LA1, LA2, U,
     *                  V, W, (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  ELSE
                     WRITE (LINE,1141,ERR=160) XCOUNT, ITT, LA1, LA2, U,
     *                  V, (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     END IF
C                                       High UVW
               ELSE
                  IF (OTYPE.EQ.1) THEN
                     WRITE (LINE,1146,ERR=160) XCOUNT, ITT, LA1, LA2, U,
     *                  V, W, (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  ELSE
                     WRITE (LINE,1147,ERR=160) XCOUNT, ITT, LA1, LA2, U,
     *                  V, (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                     END IF
                  END IF
 160           CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
 165           CONTINUE
               END IF
 170        IF (DOLAST) THEN
               GO TO 200
            ELSE
               CALL RCOPY (LRECA, KBUFF, LBUFF)
               CALL RCOPY (LRECA, KBUFF2, LBUFF2)
               END IF
 175        CONTINUE
         GO TO 100
C                                       Number lines printed
 200  IF (DOCRT.LE.0.0) THEN
         IF (DOCRT.GT.-2.5) THEN
            LINE = ' '
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         WRITE (LINE,1200) IAPCNT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      WRITE (MSGTXT,1200) IAPCNT
      IF (IAPCNT.GT.0) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
      IF ((IAPCNT.LT.NITER) .OR. (NITER.EQ.NVIS)) GO TO 210
         IF (DOCRT.LE.0.0) THEN
            WRITE (LINE,1202)
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IPCNT = IPCNT + 2
         WRITE (MSGTXT,1203)
         CALL MSGWRT (6)
 210  IPCNT = IPCNT + 2
      IF (DOCRT.LE.0.0) THEN
         WRITE (LINE,1210) FCOUNT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1211) UCOUNT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1212) SCOUNT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1213) SRCONT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      WRITE (MSGTXT,1210) FCOUNT
      IF (FCOUNT.GT.0) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
      WRITE (MSGTXT,1211) UCOUNT
      IF (UCOUNT.GT.0) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
      WRITE (MSGTXT,1212) SCOUNT
      IF (SCOUNT.GT.0) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
      WRITE (MSGTXT,1213) SRCONT
      IF (SRCONT.GT.0) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
C                                       Write statistics in header of
C                                       first input file
 900  IF (DIFCNT.LT.2) THEN
         DIFMAX = 1.0E12
         DIFMIN = 1.0E12
         DIFCNT = 2
         SUMDIF = 1.
         SUMDI2 = 2.
         END IF
      DIFMAX = MAX (ABS(DIFMAX), ABS(DIFMIN))
      SUMDIF = SUMDIF / DIFCNT
      SUMDI2 = SUMDI2 / DIFCNT
      RMS = SQRT (ABS (SUMDI2 - SUMDIF*SUMDIF) / (DIFCNT-1))
      RATMAX = MAXDIF / DIFMAX
      RATRMS = RMS / DIFMAX
      IF ((IERR.EQ.0) .AND. (DOCRT.LE.0.0)) THEN
         IF (DOCRT.GT.-2.5) THEN
            LINE = ' '
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         WRITE (LINE,1900) RATMAX, RATRMS
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      WRITE (MSGTXT,1900) RATMAX, RATRMS
      CALL MSGWRT (5)
      RATMAX = MAX (RATMAX, 1.57683E-30)
      RATRMS = MAX (RATRMS, 1.57683E-30)
      NUMKEY = 1
      LOCS = 1
      KEYTYP = 2
      KEYWRD = 'RATMAX  '
      CALL CATKEY ('WRIT', DISK, CNO, KEYWRD, NUMKEY, LOCS, RATMAX,
     *   KEYTYP,  LBUFF, JERR)
      KEYWRD = 'RATRMS  '
      CALL CATKEY ('WRIT', DISK, CNO, KEYWRD, NUMKEY, LOCS, RATRMS,
     *   KEYTYP,  LBUFF, JERR)
C                                       Close files.
      IF (IERR.LE.0) IRET = 0
      CALL LPCLOS (PLUN, PIND, IPCNT, BUFF, IERR)
C
      CALL MAPCLS ('READ', DISK, CNO, LUN, FIND, CATBLK, F, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1910) IERR
         CALL MSGWRT (6)
         END IF
      CALL MAPCLS ('READ', DISK2, CNO2, LUN2, FIND2, CAT2I, F, BUFF,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1920) IERR
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('File',I1,' = ',A12,'.',A6,'.',I4,'   Vol =',I2,
     *   '    UserID =',I5,5X,'Channels =',I5,' to ',I5,' BIF=',I3)
 1021 FORMAT (I1,1X,A12,'.',A6,'.',I4,'  Vol=',I2,'  User=',I5,
     *   '  Chan=',I4,' to',I4,'  IF=',I3)
 1025 FORMAT ('Source= ',A8,3X,A4,' = ',A1,I2.2,I3.2,F6.2,3X,A4,' = ',
     *   A1,I2.2,I3.2,F5.1)
 1030 FORMAT ('Freq=',F13.9,' GHz   ncor=',I3,'   No. vis=',I10,
     *   '   Sort order= ',A2)
 1031 FORMAT ('Weights have been multiplied by',1PE10.2)
 1040 FORMAT ('Source= ',A8,4X,'Freq= ',F13.9,4X,'Sort= ',A2,10X,
     *   4(5X,I4,1X,A2,4X))
 1041 FORMAT (A,4X,'Freq=',F13.9,4X,'Sort= ',A2,7X,2(4X,I4,1X,A2,4X))
 1050 FORMAT ('  Vis #',5X,'IAT',6X,'Ant      U(Klam)     V(Klam)',
     *   '     W(Klam)',4(4X,A))
 1051 FORMAT ('  Vis #',5X,'IAT',6X,'Ant      U(Klam)     V(Klam)',
     *   2(3X,A))
 1060 FORMAT ('ERROR:',I7,' INITIALIZING 1ST UV FILE')
 1065 FORMAT ('ERROR:',I7,' INITIALIZING 2ND UV FILE')
 1075 FORMAT ('REQUESTED STOKES TYPE NOT AVAILABLE IN DATA SET')
 1100 FORMAT ('ERROR:',I7,' READING 1ST UV FILE VIS BLOCK ',I9)
 1105 FORMAT ('ERROR:',I7,' READING 2ND UV FILE VIS BLOCK ',I9)
 1140 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,3F12.5,4(F9.3,I4,I3))
 1141 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F12.5,2(F8.3,I4,I3))
 1146 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,3F12.3,4(F9.3,I4,I3))
 1147 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,2F12.3,2(F8.3,I4,I3))
 1200 FORMAT (I5,' Points printed')
 1202 FORMAT ('0********** PRINT LIMIT REACHED: MORE POINTS MAY EXIST',
     *   ' **********')
 1203 FORMAT ('WARNING: PRINT LIMIT REACHED, NOT ALL POINTS LISTED')
 1210 FORMAT (I8,' Points found for flux differences')
 1211 FORMAT (I8,' Points found for u,v,w differences')
 1212 FORMAT (I8,' Points found for other differences')
 1213 FORMAT (I8,' Points swapped for apparent sort order reversal')
 1900 FORMAT ('Visibilty Max diff/peak =',1PE11.3,'  RMS/peak =',E11.3)
 1910 FORMAT ('ERROR:',I7,' CLOSING 1ST UV FILE ')
 1920 FORMAT ('ERROR:',I7,' CLOSING 2ND UV FILE ')
      END
      SUBROUTINE FNDUCL (IRET)
C-----------------------------------------------------------------------
C   FNDUPR reads the UV files and prints those samples meeting the
C   criteria given by OPCODE and APARM.
C   Output:
C      IRET   I   Return code: 0 success or user requests end, else bad
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'UVDIF.INC'
      CHARACTER  CHS1*1, CHS2*1, ISTOKE(6)*2, JSTOKE(4,2)*2, TCHAR*20,
     *   LLCH*4, MMCH*4, KEYWRD*8
      LOGICAL   F, DOIT, EQUAL, FDOIT, SDOIT, NOPRNT, DOLAST, SRDOIT
      INTEGER   ICH(4), JADR(8), BIND, CINC, ITYPE(8), BUFSZ, HM(2),
     *   DD(2), J, ITT(4), LENBU, IWT(6), IPHASE(8), INDEX, MVIS,
     *   IROUND, I, L, IA1, IA2, ICH1, ICH2, IERR, JERR, II, JJ, KK,
     *   KKK, IPINT, JCOR, LIMIT2, NIO, NNCH, NUMCH, K, LRECF, NUMCHM,
     *   OTYPE, BIND2, NIO2, IPINT2, NB, IB1, IB2, IERR2, BO, VO,
     *   XCOUNT, COUNT, IAPCNT, FCOUNT, SCOUNT, NCORR, DIFCNT, LRECA,
     *   NUMKEY, LOCS, KEYTYP, DPTR, LA1, LA2, LB1, LB2, IERR1, SRCONT
      REAL   RSEC, DSEC, AMP(8), VIS(3,8), TEMP, TPHS, R, RADEG, U, V,
     *   W, XX, YY, YAMP, UVN, RMS, WTSC, U2, V2, W2, VIS2(3,8),
     *   XDAY, XDAY2, DIFMAX, RATMAX, RATRMS, DIFMIN, LDAY, LDAY2,
     *   PHASE(8)
      DOUBLE PRECISION DIF, SUMDIF, SUMDI2, MAXDIF
      INCLUDE 'BUFFS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PUVD.INC'
      DATA F /.FALSE./
      DATA JSTOKE /'RR','LL','RL','LR', 'I ','Q ','U ','V '/
      DATA LENBU, BO, VO /16, 1, 0/
C-----------------------------------------------------------------------
      IRET = 8
      RADEG = 180.0 / 3.14159
      CINC = 2
      FCOUNT = 0
      SCOUNT = 0
      SRCONT = 0
      NOPRNT = .FALSE.
C                                       Set up for compressed data.
      NCORR = (LREC - NRPARM) / CATBLK(KINAX)
      LRECF = 3 * NCORR
      DPTR = 1 + MAX (NRP1, NRP2)
      LRECA = LRECF + DPTR - 1
      CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), KLOCWT,
     *   JERR)
      IF (ISCMP1 .AND. ((JERR.NE.0) .OR. (KLOCWT.LT.0))) THEN
         IRET = 5
         MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL AXEFND (8, 'WEIGHT  ', CAT2I(KIPCN), CAT2H(KHPTP), LLOCWT,
     *   JERR)
      IF (ISCMP2 .AND. ((JERR.NE.0) .OR. (LLOCWT.LT.0))) THEN
         IRET = 5
         MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Initialize difference statistics
      DIFCNT = 0
      SUMDIF = 0.0D0
      SUMDI2 = 0.0D0
      DIFMAX = -1.0E10
      DIFMIN = 1.0E10
      MAXDIF = -1.0E10
C                                        Determine no. channels to do
      FREQ = FREQ - (CATR(KRCRP+JLOCF)-NCH) * CATR(KRCIC+JLOCF)
      NUMCHM = CATBLK(KINAX+JLOCF) - NCH + 1
C      IF (NCORR.GT.1) NUMCHM = 1
      NUMCH = (NACROS - 25) / (19 * NCOR)
      NUMCH = MIN (NUMCH, NUMCHM)
      OTYPE = 1
      IF (NUMCH.LT.1) THEN
         NUMCH = 1
         OTYPE = 2
         IF (NACROS.LT.84) OTYPE = 3
         END IF
      LIMIT2 = NCOR * NUMCH
      ICH1 = NCH
      ICH2 = NCH + NUMCH - 1
      NNCH = NCH - 1
      CALL FILL (8, 3, ITYPE)
      DO 10 II = 1,LIMIT2,NCOR
         NNCH = NNCH + 1
         DO 10 JJ = 1,NCOR
            INDEX = II + JJ - 1
            ICH(INDEX) = NNCH
            TEMP = CATD(KDCRV+JLOCS) + (JJ-CATR(KRCRP+JLOCS)) *
     *         CATR(KRCIC+JLOCS)
            I = IROUND(TEMP)
            L = 2
            IF (I.LT.0) L = 1
            I = ABS(I)
            ISTOKE(INDEX) = JSTOKE(I,L)
            ITYPE(INDEX) = 2
            IF (I+L.LE.3) ITYPE(INDEX) = 1
 10         CONTINUE
C                                       range parameters
      IF (XWT.LE.0.0) THEN
         XWT = 1.0
         NWT = 1.0
         END IF
      TEMP = LOG10 (XWT/0.995)
      I = TEMP + 99.0
      I =  100 - I
      IF (OTYPE.EQ.1) I = I + 1
      WTSC = 10.0 ** I
      TEMP = NWT * WTSC
      IF (TEMP.LT.1.0) THEN
         MSGTXT = 'Full dynamic range of weights cannot be printed'
         CALL MSGWRT (6)
         END IF
C                                       Convert coordinates.
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCR*CINC), LLCH)
      CALL H2CHR (4, 1, CATH(KHCTP+JLOCD*CINC), MMCH)
      EQUAL = LLCH(1:2).EQ.'RA'
      IF (EQUAL) THEN
         CALL COORDD (1, RA, CHS1, HM, RSEC)
      ELSE
         CALL COORDD (2, RA, CHS1, HM, RSEC)
         END IF
      CALL COORDD (2, DEC, CHS2, DD, DSEC)
      FREQ = FREQ * 1.0E-9
C                                       Make sure NITER is OK
      IF (NITER.GT.NVIS) NITER = NVIS
C                                       Write header
      IF (DOCRT.GT.-2.5) THEN
         LINE = ' '
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       source names
         IF (NACROS.GE.90) THEN
            WRITE (LINE,1020) 1, NAME, CLASS, SEQ, DISK, USERID, ICH1,
     *         ICH2, BIF
         ELSE
            WRITE (LINE,1021) 1, NAME, CLASS, SEQ, DISK, USERID, ICH1,
     *         ICH2, BIF
            END IF
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (NACROS.GE.90) THEN
            WRITE (LINE,1020) 2, NAME2, CLASS2, SEQ2, DISK2, USERID,
     *         ICH1, ICH2, BIF
         ELSE
            WRITE (LINE,1021) 2, NAME2, CLASS2, SEQ2, DISK2, USERID,
     *         ICH1, ICH2, BIF
            END IF
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1025) SOURCE, LLCH, CHS1, HM, RSEC, MMCH, CHS2, DD,
     *      DSEC
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1030) FREQ, NCOR, NVIS, ISORT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (WTSC.NE.1.0) THEN
            WRITE (LINE,1031) WTSC
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         END IF
C                                       Page titles
      IF (OTYPE.EQ.1) WRITE (TITL1,1040) SOURCE, FREQ, ISORT,
     *   (ICH(JCOR), ISTOKE(JCOR), JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.2) WRITE (TITL1,1041) SOURCE, FREQ, ISORT,
     *   (ICH(JCOR), ISTOKE(JCOR), JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.3) WRITE (TITL1,1042) SOURCE, FREQ,
     *   (ICH(JCOR), ISTOKE(JCOR), JCOR = 1,LIMIT2)
      IF (OTYPE.EQ.1) THEN
         TCHAR = '   Amp    Phase  Wt'
         WRITE (TITL2,1050) (TCHAR, JCOR = 1,LIMIT2)
      ELSE IF (OTYPE.EQ.2) THEN
         TCHAR = '  Amp   Phas Wt'
         WRITE (TITL2,1051) (TCHAR, JCOR = 1,LIMIT2)
      ELSE
         TCHAR = '  Amp  Phas Wt'
         WRITE (TITL2,1052) (TCHAR, JCOR = 1,LIMIT2)
         END IF
      IF (DOCRT.GT.-2.5) THEN
         LINE = ' '
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (DOCRT.GT.-2.5) THEN
         LINE = ' '
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Initialize reading VIS. file.
      LENBU = 0
      BUFSZ = UVBFSS * 2
      CALL UVINIT ('READ', LUN, FIND, NVIS, VO, LREC, LENBU, BUFSZ,
     *   BUFF, BO, BIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
      CALL UVINIT ('READ', LUN2, FIND2, NVIS, VO, LREC2, LENBU, BUFSZ,
     *   BUFF2, BO, BIND2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1065) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
      COUNT = 0
      XCOUNT = -1
      PAGE = 1
      IAPCNT = 0
C                                       Try to handle line
      CALL SETVIS (ICH1, NUMCH, BIF, MVIS, JADR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1075)
         CALL MSGWRT (8)
         GO TO 900
         END IF
C                                       Start looping thru data.
 100  CONTINUE
C                                       Read buffer.
         COUNT = COUNT + 1
         CALL UVDISK ('READ', LUN, FIND, BUFF, NIO, BIND, IERR)
         IF (IERR.EQ.0) GO TO 105
            WRITE (MSGTXT,1100) IERR, COUNT
            CALL MSGWRT (8)
            GO TO 900
 105     CALL UVDISK ('READ', LUN2, FIND2, BUFF2, NIO2, BIND2, IERR)
         IF (IERR.EQ.0) GO TO 110
            WRITE (MSGTXT,1105) IERR, COUNT
            CALL MSGWRT (8)
            GO TO 900
 110     DOLAST = (NIO.LE.0) .OR. (NIO2.LE.0)
         IPINT = BIND - LREC
         IPINT2 = BIND2 - LREC2
         NIO = MAX (1, NIO)
C                                       Loop thru buffer.
         DO 175 J = 1,NIO
C                                       Update counters
            XCOUNT = XCOUNT + 1
            IPINT = IPINT + LREC
            IPINT2 = IPINT2 + LREC2
C                                       CONVERT
            IF (.NOT.DOLAST) THEN
C                                       Compressed?
               CALL RCOPY (NRP1, BUFF(IPINT), KBUFF)
               IF (ISCMP1) THEN
                  CALL ZUVXPN (NCORR, BUFF(IPINT+NRP1),
     *               BUFF(IPINT+KLOCWT), KBUFF(DPTR))
               ELSE
                  CALL RCOPY (LRECF, BUFF(IPINT+NRP1), KBUFF(DPTR))
                  END IF
               CALL RCOPY (NRP2, BUFF2(IPINT2), KBUFF2)
               IF (ISCMP2) THEN
                  CALL ZUVXPN (NCORR, BUFF2(IPINT2+NRP2),
     *               BUFF2(IPINT2+LLOCWT), KBUFF2(DPTR))
               ELSE
                  CALL RCOPY (LRECF, BUFF2(IPINT2+NRP2), KBUFF2(DPTR))
                  END IF
               END IF
C                                       TEST
            IF (XCOUNT.GT.0) THEN
               SRDOIT = .FALSE.
               SDOIT = .FALSE.
               FDOIT = .FALSE.
C                                       Decode time.
               LDAY = LBUFF(1+I1LOCT)
               LDAY2 = LBUFF2(1+I2LOCT)
               IF ((LDAY.LT.0.0) .OR. (LDAY.GT.1000.)) SDOIT = .TRUE.
               IF ((LDAY2.LT.0.0) .OR. (LDAY2.GT.1000.)) SDOIT = .TRUE.
               CALL TODHMS (LDAY, ITT)
C                                       Determine antennas.
               IF (I1LOCB.GE.0) THEN
                  LA1 = LBUFF(1+I1LOCB)/256. + 0.1
                  LA2 = LBUFF(1+I1LOCB) - LA1*256. + 0.1
               ELSE
                  LA1 = LBUFF(1+I1LOA1) + 0.1
                  LA2 = LBUFF(1+I1LOA2) + 0.1
                  END IF
               IF (I2LOCB.GE.0) THEN
                  LB1 = LBUFF2(1+I2LOCB)/256. + 0.1
                  LB2 = LBUFF2(1+I2LOCB) - LB1*256. + 0.1
               ELSE
                  LB1 = LBUFF2(1+I2LOA1) + 0.1
                  LB2 = LBUFF2(1+I2LOA2) + 0.1
                  END IF
               IF (TYPUVD.LE.0) THEN
                  IF ((LA1.LE.0) .OR. (LA1.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LA2.LE.0) .OR. (LA2.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LB1.LE.0) .OR. (LB1.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LB2.LE.0) .OR. (LB2.GT.MAXANT)) SDOIT = .TRUE.
                  END IF
C                                       Sort order swap ?
               IF ((ABS(LDAY-LDAY2).GT.1.2E-5) .OR. (LA1.NE.LB1) .OR.
     *            (LA2.NE.LB2)) THEN
C                                       Decode time.
                  XDAY = KBUFF(1+I1LOCT)
                  XDAY2 = KBUFF2(1+I2LOCT)
C                                       Determine antennas.
                  IF (I1LOCB.GE.0) THEN
                     IA1 = KBUFF(1+I1LOCB)/256. + 0.1
                     IA2 = KBUFF(1+I1LOCB) - IA1*256. + 0.1
                  ELSE
                     IA1 = KBUFF(1+I1LOA1) + 0.1
                     IA2 = KBUFF(1+I1LOA2) + 0.1
                     END IF
                  IF (I2LOCB.GE.0) THEN
                     IB1 = KBUFF2(1+I2LOCB)/256. + 0.1
                     IB2 = KBUFF2(1+I2LOCB) - IB1*256. + 0.1
                  ELSE
                     IB1 = KBUFF2(1+I2LOA1) + 0.1
                     IB2 = KBUFF2(1+I2LOA2) + 0.1
                     END IF
                  SRDOIT = (LA1.EQ.IB1) .AND. (LA2.EQ.IB2) .AND.
     *               (LB1.EQ.IA1) .AND. (LB2.EQ.IA2) .AND.
     *               (ABS(LDAY-XDAY2).LE.1.2E-5) .AND.
     *               (ABS(XDAY-LDAY2).LE.1.2E-5) .AND. (.NOT.DOLAST)
C                                       Yes: swap buffers
                  IF (SRDOIT) THEN
                     SRCONT = SRCONT + 1
                     LDAY2 = XDAY2
                     LB1 = IB1
                     LB2 = IB2
                     CALL RCOPY (LRECA, LBUFF2, KBUFF2)
                     IF (ISCMP2) THEN
                        CALL RCOPY (NRP2, BUFF2(IPINT2), LBUFF2)
                        CALL ZUVXPN (NCORR, BUFF2(IPINT2+NRP2),
     *                     BUFF2(IPINT2+LLOCWT), LBUFF2(DPTR))
                     ELSE
                        CALL RCOPY (LRECF, BUFF2(IPINT2+NRP2),
     *                     LBUFF2(DPTR))
                        END IF
C                                       No: error
                  ELSE
                     SDOIT = .TRUE.
                     END IF
                  END IF
C                                       Convert uvw to kilo lamda.
               U = LBUFF(1+I1LOCU) * 0.001
               V = LBUFF(1+I1LOCV) * 0.001
               W = LBUFF(1+I1LOCW) * 0.001
               U2 = LBUFF2(1+I2LOCU) * 0.001
               V2 = LBUFF2(1+I2LOCV) * 0.001
               W2 = LBUFF2(1+I2LOCW) * 0.001
               UVM = MAX (UVM, U)
               UVM = MAX (UVM, V)
               UVM = MAX (UVM, W)
               UVN = U
               UVN = MIN (UVN, V)
               UVN = MIN (UVN, W)
               UVM = MAX (UVM, -10.0*UVN)
               R = U * U + V * V
C                                       UV box and range
               IF (.NOT.SDOIT) THEN
                  IF ((R.LT.UVRANG(1)) .OR. (R.GT.UVRANG(2))) GO TO 170
                  END IF
C                                       Convert to VIS
               CALL GETVIS (MVIS, JADR, LBUFF(DPTR), VIS, IERR1)
               CALL GETVIS (MVIS, JADR, LBUFF2(DPTR), VIS2, IERR2)
               IF ((IERR1.NE.0) .AND. (IERR2.NE.0)) GO TO 170
               FDOIT = (IERR1.NE.IERR2) .AND. (APARM(4).GT.0.0)
C                                       Accumulate statistics
               DOIT = FDOIT .OR. SDOIT
               IF (.NOT.DOIT) THEN
                  DO 115 KK = 1,MVIS
                     IF ((VIS(3,KK).GT.0.0) .OR. (VIS2(3,KK).GT.0.0)
     *                  .OR. (DOFLAG)) THEN
                        DIFMAX = MAX (DIFMAX, VIS(1,KK), VIS(2,KK))
                        DIFMAX = MAX (DIFMAX, VIS2(1,KK), VIS2(2,KK))
                        DIFMIN = MIN (DIFMIN, VIS(1,KK), VIS(2,KK))
                        DIFMIN = MIN (DIFMIN, VIS2(1,KK), VIS2(2,KK))
                        DIF = VIS2(1,KK) - VIS(1,KK)
                        DIFCNT = DIFCNT + 1
                        SUMDIF = SUMDIF + DIF
                        SUMDI2 = SUMDI2 + DIF*DIF
                        MAXDIF = MAX (MAXDIF, ABS (DIF))
                        DIF = VIS2(2,KK) - VIS(2,KK)
                        DIFCNT = DIFCNT + 1
                        SUMDIF = SUMDIF + DIF
                        SUMDI2 = SUMDI2 + DIF*DIF
                        MAXDIF = MAX (MAXDIF, ABS (DIF))
                        END IF
 115                 CONTINUE
                  END IF
C                                       Do we want this one
               IF ((.NOT.DOIT) .AND. (DOMAX)) THEN
                  DO 130 K = 1,MVIS
                     IF ((ITYPE(K).LE.2) .AND. ((VIS(3,K).GT.0.0) .OR.
     *                  (VIS2(3,K).GT.0.0) .OR. (DOFLAG))) THEN
                        IF (ABS(VIS(1,K)-VIS2(1,K)).GT.APARM(ITYPE(K)))
     *                     FDOIT = .TRUE.
                        IF (ABS(VIS(2,K)-VIS2(2,K)).GT.APARM(ITYPE(K)))
     *                     FDOIT = .TRUE.
                        IF (APARM(4).GT.0.0) THEN
                           IF (ABS(VIS(3,K)-VIS2(3,K)).GT.APARM(4))
     *                        FDOIT = .TRUE.
                           IF ((VIS(3,K).LE.0.0).AND.(VIS2(3,K).GT.0.0))
     *                        FDOIT = .TRUE.
                           IF ((VIS(3,K).GT.0.0).AND.(VIS2(3,K).LE.0.0))
     *                        FDOIT = .TRUE.
                        ELSE
                           IF (ABS(ABS(VIS(3,K))-ABS(VIS2(3,K))).GT.
     *                        -APARM(4)) FDOIT = .TRUE.
                           END IF
                        END IF
 130                 CONTINUE
                  DOIT = FDOIT
                  END IF
               IF (.NOT.DOIT) GO TO 170
               IF (IAPCNT.GE.NITER) NOPRNT = .TRUE.
               IF ((NOPRNT) .AND. (DOCRT.GT.0.0)) GO TO 200
               IF (.NOT.NOPRNT) IAPCNT = IAPCNT + 1
               IF (FDOIT) FCOUNT = FCOUNT + 1
               IF (SDOIT) SCOUNT = SCOUNT + 1
               IF (NOPRNT) GO TO 170
C                                       Get vis.
               DO 165 NB = 1,2
               K = 0
               YAMP = 0.0
               DO 140 KK = ICH1,ICH2
                  DO 140 KKK = 1,NCOR
                     K = K + 1
                     INDEX = 1 + (KK-1) * INCF + (KKK-1) * INCS +
     *                  (BIF-1) * INCIF + MAX (NRP1, NRP2)
                     IF (NB.EQ.1) THEN
                        XX = LBUFF(INDEX)
                        YY = LBUFF(INDEX+1)
                        TPHS = LBUFF(INDEX+2)
                     ELSE
                        XX = LBUFF2(INDEX)
                        YY = LBUFF2(INDEX+1)
                        TPHS = LBUFF2(INDEX+2)
                        END IF
                     IF (OTYPE.EQ.1) THEN
                        TPHS = MAX (-999.0, MIN (999.0, TPHS*WTSC))
                     ELSE
                        TPHS = MAX (-99.0, MIN (99.0, TPHS*WTSC))
                        END IF
                     IWT(K) = IROUND (TPHS)
                     IF (LA1.EQ.LA2) THEN
                        AMP(K) = XX
                        TPHS = 0.0
                     ELSE
                        AMP(K) = SQRT(XX*XX+YY*YY)
                        TPHS = 57.296 * ATAN2 (YY, XX+1.0E-20)
                        END IF
                     PHASE(K) = TPHS
                     IPHASE(K) = IROUND (TPHS)
                     XAMP = MAX (XAMP, AMP(K))
                     YAMP = MIN (YAMP, AMP(K))
 140                 CONTINUE
               IF (NB.NE.1) THEN
                  LA1 = LB1
                  LA2 = LB2
                  U = U2
                  V = V2
                  W = W2
                  CALL TODHMS (LDAY2, ITT)
                  END IF
C                                       Write VIS data: CRT
               XAMP = MAX (XAMP, -10.0*YAMP)
C                                       low flux
               IF ((XAMP.LE.99.9) .OR. ((OTYPE.EQ.1) .AND.
     *            (XAMP.LE.999.9))) THEN
                  IF (OTYPE.EQ.1) WRITE (LINE,1140,ERR=160) XCOUNT, ITT,
     *               LA1, LA2, (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.2) WRITE (LINE,1141,ERR=160) XCOUNT, ITT,
     *               LA1, LA2, (AMP(K), IPHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.3) WRITE (LINE,1152,ERR=160) XCOUNT,
     *               ITT(2), ITT(3), ITT(4), LA1, LA2,
     *              (AMP(K), IPHASE(K), IWT(K), K = 1,LIMIT2)
               ELSE
                  IF (OTYPE.EQ.1) WRITE (LINE,1150,ERR=160) XCOUNT, ITT,
     *               LA1, LA2, (AMP(K), PHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.2) WRITE (LINE,1151,ERR=160) XCOUNT, ITT,
     *               LA1, LA2, (AMP(K), IPHASE(K), IWT(K), K = 1,LIMIT2)
                  IF (OTYPE.EQ.3) WRITE (LINE,1152,ERR=160) XCOUNT,
     *               ITT(2), ITT(3), ITT(4), LA1, LA2,
     *              (AMP(K), IPHASE(K), IWT(K), K = 1,LIMIT2)
                  END IF
 160           CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
 165           CONTINUE
               END IF
 170        IF (DOLAST) THEN
               GO TO 200
            ELSE
               CALL RCOPY (LRECA, KBUFF, LBUFF)
               CALL RCOPY (LRECA, KBUFF2, LBUFF2)
               END IF
 175        CONTINUE
         GO TO 100
C                                       Number lines printed
 200  IF (DOCRT.LE.0.0) THEN
         IF (DOCRT.GT.-2.5) THEN
            LINE = ' '
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         WRITE (LINE,1200) IAPCNT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      WRITE (MSGTXT,1200) IAPCNT
      IF (IAPCNT.GT.0) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
      IF ((IAPCNT.LT.NITER) .OR. (NITER.EQ.NVIS)) GO TO 210
         IF (DOCRT.LE.0.0) THEN
            WRITE (LINE,1202)
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IPCNT = IPCNT + 2
         WRITE (MSGTXT,1203)
         CALL MSGWRT (6)
 210  IPCNT = IPCNT + 2
      IF (DOCRT.LE.0.0) THEN
         WRITE (LINE,1210) FCOUNT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1212) SCOUNT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1213) SRCONT
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      WRITE (MSGTXT,1210) FCOUNT
      IF (FCOUNT.GT.0) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
      WRITE (MSGTXT,1212) SCOUNT
      IF (SCOUNT.GT.0) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
      WRITE (MSGTXT,1213) SRCONT
      IF (SRCONT.GT.0) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
C                                       Write statistics in header of
C                                       first input file
 900  IF (DIFCNT.LT.2) THEN
         MSGTXT = 'NO POINTS EVEN CHECKED FOR DIFFERENCES IN FLUX'
         CALL MSGWRT (7)
         DIFMAX = 1.0E12
         DIFMIN = 1.0E12
         DIFCNT = 2
         SUMDIF = 1.
         SUMDI2 = 2.
         END IF
      DIFMAX = MAX (ABS(DIFMAX), ABS(DIFMIN))
      SUMDIF = SUMDIF / DIFCNT
      SUMDI2 = SUMDI2 / DIFCNT
      RMS = SQRT (ABS (SUMDI2 - SUMDIF*SUMDIF) / (DIFCNT-1))
      RATMAX = MAXDIF / DIFMAX
      RATRMS = RMS / DIFMAX
      IF ((IERR.EQ.0) .AND. (DOCRT.LE.0.0)) THEN
         IF (DOCRT.GT.-2.5) THEN
            LINE = ' '
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         WRITE (LINE,1900) RATMAX, RATRMS
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      WRITE (MSGTXT,1900) RATMAX, RATRMS
      CALL MSGWRT (5)
      RATMAX = MAX (RATMAX, 1.57683E-30)
      RATRMS = MAX (RATRMS, 1.57683E-30)
      NUMKEY = 1
      LOCS = 1
      KEYTYP = 2
      KEYWRD = 'RATMAX  '
      CALL CATKEY ('WRIT', DISK, CNO, KEYWRD, NUMKEY, LOCS, RATMAX,
     *   KEYTYP,  LBUFF, JERR)
      KEYWRD = 'RATRMS  '
      CALL CATKEY ('WRIT', DISK, CNO, KEYWRD, NUMKEY, LOCS, RATRMS,
     *   KEYTYP,  LBUFF, JERR)
C                                       Close files.
      IF (IERR.LE.0) IRET = 0
      CALL LPCLOS (PLUN, PIND, IPCNT, BUFF, IERR)
C
      CALL MAPCLS ('READ', DISK, CNO, LUN, FIND, CATBLK, F, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1910) IERR
         CALL MSGWRT (6)
         END IF
      CALL MAPCLS ('READ', DISK2, CNO2, LUN2, FIND2, CAT2I, F, BUFF,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1920) IERR
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('File',I1,' = ',A12,'.',A6,'.',I4,'   Vol =',I2,
     *   '    UserID =',I5,5X,'Channels =',I5,' to ',I5,' BIF=',I3)
 1021 FORMAT (I1,1X,A12,'.',A6,'.',I4,'  Vol=',I2,'  User=',I5,
     *   '  Chan=',I4,' to',I4,'  IF=',I3)
 1025 FORMAT ('Source= ',A8,3X,A4,' = ',A1,I2.2,I3.2,F6.2,3X,A4,' = ',
     *   A1,I2.2,I3.2,F5.1)
 1030 FORMAT ('Freq=',F13.9,' GHz   ncor=',I3,'   No. vis=',I10,
     *   '   Sort order= ',A2)
 1031 FORMAT ('Weights have been multiplied by',1PE10.2)
 1040 FORMAT (A8,F13.9,1X,A2,1X,5(6X,I4,1X,A2,6X))
 1041 FORMAT (A8,F13.9,1X,A2,4(4X,I4,1X,A2,4X))
 1042 FORMAT (A8,F13.9,4(4X,I4,1X,A2,3X))
 1050 FORMAT ('  Vis #',5X,'IAT',6X,'Ant ',5A19)
 1051 FORMAT ('  Vis #',5X,'IAT',6X,'Ant ',4A15)
 1052 FORMAT ('  Vis #',4X,'IAT',4X,'Ant ',4A14)
 1060 FORMAT ('ERROR:',I7,' INITIALIZING 1ST UV FILE')
 1065 FORMAT ('ERROR:',I7,' INITIALIZING 2ND UV FILE')
 1075 FORMAT ('REQUESTED STOKES TYPE NOT AVAILABLE IN DATA SET')
 1100 FORMAT ('ERROR:',I7,' READING 1ST UV FILE VIS BLOCK ',I9)
 1105 FORMAT ('ERROR:',I7,' READING 2ND UV FILE VIS BLOCK ',I9)
 1140 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,5(F9.4,F6.1,I4))
 1141 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,4(F8.4,I4,I3))
 1150 FORMAT (I7,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,5(F9.3,F6.1,I4))
 1151 FORMAT (I6,I3,'/',I2.2,2(':',I2.2),I3,'-',I2,4(F8.2,I4,I3))
 1152 FORMAT (I6,I3,'/',I2.2,':',I2.2,I3,'-',I2,4(F7.1,I4,I3))
 1200 FORMAT (I5,' Points printed')
 1202 FORMAT ('0********** PRINT LIMIT REACHED: MORE POINTS MAY EXIST',
     *   ' **********')
 1203 FORMAT ('WARNING: PRINT LIMIT REACHED, NOT ALL POINTS LISTED')
 1210 FORMAT (I8,' Points found for flux differences')
 1212 FORMAT (I8,' Points found for other differences')
 1213 FORMAT (I8,' Points swapped for apparent sort order reversal')
 1900 FORMAT ('Visibilty Max diff/peak =',1PE11.3,'  RMS/peak =',E11.3)
 1910 FORMAT ('ERROR:',I7,' CLOSING 1ST UV FILE ')
 1920 FORMAT ('ERROR:',I7,' CLOSING 2ND UV FILE ')
      END
      SUBROUTINE CHKUBX (NCOUNT, IRET)
C-----------------------------------------------------------------------
C   FNDUBX reads the UV files and counts print lines showing
C   differences in u,v,w.
C   Output:
C      NCOUNT   I   Count of lines
C      IRET     I   Return 0 success or user requests end, else bad
C-----------------------------------------------------------------------
      INTEGER   NCOUNT, IRET
C
      INCLUDE 'UVDIF.INC'
      LOGICAL   F, DOIT, FDOIT, UDOIT, SDOIT, NOPRNT, DOLAST, SRDOIT
      INTEGER   JADR(8), BIND, CINC, BUFSZ, J, ITT(4), LENBU, MVIS, I,
     *   IA1, IA2, ICH1, IERR, JERR, IPINT, NIO, NUMCH, LRECF, NUMCHM,
     *   LCOR, BIND2, NIO2, IPINT2, IB1, IB2, IERR2, BO, VO, XCOUNT,
     *   COUNT, IAPCNT, NCORR, LRECA, DPTR, LA1, LA2, LB1, LB2, IERR1,
     *   SRCONT
      REAL   VIS(3,8), TEMP, R, U, V, W, WTSC, U2, V2, W2, VIS2(3,8),
     *   XDAY, XDAY2, LDAY, LDAY2
      INCLUDE 'BUFFS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PUVD.INC'
      DATA F /.FALSE./
      DATA LENBU, BO, VO /16, 1, 0/
C-----------------------------------------------------------------------
      IRET = 8
      CINC = 2
      SRCONT = 0
      NOPRNT = .FALSE.
C                                       range parameters
      IF (XWT.LE.0.0) THEN
         XWT = 1.0
         NWT = 1.0
         END IF
      TEMP = LOG10 (XWT/0.995)
      I = TEMP + 99.0
      I =  100 - I
      WTSC = 10.0 ** I
      TEMP = NWT * WTSC
C                                       Set up for compressed data.
      NCORR = (LREC - NRPARM) / CATBLK(KINAX)
      LRECF = 3 * NCORR
      DPTR = 1 + MAX (NRP1, NRP2)
      LRECA = LRECF + DPTR - 1
      CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), KLOCWT,
     *   JERR)
      IF (ISCMP1 .AND. ((JERR.NE.0) .OR. (KLOCWT.LT.0))) THEN
         IRET = 5
         MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL AXEFND (8, 'WEIGHT  ', CAT2I(KIPCN), CAT2H(KHPTP), LLOCWT,
     *   JERR)
      IF (ISCMP2 .AND. ((JERR.NE.0) .OR. (LLOCWT.LT.0))) THEN
         IRET = 5
         MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                        Determine no. channels to do
      NUMCHM = CATBLK(KINAX+JLOCF) - NCH + 1
      LCOR = MIN (2, NCOR)
      NUMCH = (NACROS - 61) / (16 * LCOR)
      NUMCH = MAX (1, MIN (NUMCH, NUMCHM))
      ICH1 = NCH
C                                       Make sure NITER is OK
      IF (NITER.GT.NVIS) NITER = NVIS
C                                       Write header
      IF (DOCRT.GT.-2.5) THEN
         NCOUNT = NCOUNT + 5
         IF (WTSC.NE.1.0) NCOUNT = NCOUNT + 1
         END IF
C                                       Page titles
      IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
      NCOUNT = NCOUNT + 2
      IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
C                                       Initialize reading VIS. file.
      LENBU = 0
      BUFSZ = UVBFSS * 2
      CALL UVINIT ('READ', LUN, FIND, NVIS, VO, LREC, LENBU, BUFSZ,
     *   BUFF, BO, BIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
      CALL UVINIT ('READ', LUN2, FIND2, NVIS, VO, LREC2, LENBU, BUFSZ,
     *   BUFF2, BO, BIND2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1065) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
      COUNT = 0
      XCOUNT = -1
      IAPCNT = 0
C                                       Try to handle line
      CALL SETVIS (ICH1, NUMCH, BIF, MVIS, JADR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1075)
         CALL MSGWRT (8)
         GO TO 900
         END IF
C                                       Start looping thru data.
 100  CONTINUE
C                                       Read buffer.
         COUNT = COUNT + 1
         CALL UVDISK ('READ', LUN, FIND, BUFF, NIO, BIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, COUNT
            CALL MSGWRT (8)
            GO TO 900
            END IF
         CALL UVDISK ('READ', LUN2, FIND2, BUFF2, NIO2, BIND2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1105) IERR, COUNT
            CALL MSGWRT (8)
            GO TO 900
            END IF
         DOLAST = (NIO.LE.0) .OR. (NIO2.LE.0)
         IPINT = BIND - LREC
         IPINT2 = BIND2 - LREC2
         NIO = MAX (1, NIO)
C                                       Loop thru buffer.
         DO 175 J = 1,NIO
C                                       Update counters
            XCOUNT = XCOUNT + 1
            IPINT = IPINT + LREC
            IPINT2 = IPINT2 + LREC2
C                                       CONVERT
            IF (.NOT.DOLAST) THEN
C                                       Compressed?
               CALL RCOPY (NRP1, BUFF(IPINT), KBUFF)
               IF (ISCMP1) THEN
                  CALL ZUVXPN (NCORR, BUFF(IPINT+NRP1),
     *               BUFF(IPINT+KLOCWT), KBUFF(DPTR))
               ELSE
                  CALL RCOPY (LRECF, BUFF(IPINT+NRP1), KBUFF(DPTR))
                  END IF
               CALL RCOPY (NRP2, BUFF2(IPINT2), KBUFF2)
               IF (ISCMP2) THEN
                  CALL ZUVXPN (NCORR, BUFF2(IPINT2+NRP2),
     *               BUFF2(IPINT2+LLOCWT), KBUFF2(DPTR))
               ELSE
                  CALL RCOPY (LRECF, BUFF2(IPINT2+NRP2), KBUFF2(DPTR))
                  END IF
               END IF
C                                       TEST
            IF (XCOUNT.GT.0) THEN
               SRDOIT = .FALSE.
               SDOIT = .FALSE.
               UDOIT = .FALSE.
               FDOIT = .FALSE.
C                                       Decode time.
               LDAY = LBUFF(1+I1LOCT)
               LDAY2 = LBUFF2(1+I2LOCT)
               IF ((LDAY.LT.0.0) .OR. (LDAY.GT.1000.)) SDOIT = .TRUE.
               IF ((LDAY2.LT.0.0) .OR. (LDAY2.GT.1000.)) SDOIT = .TRUE.
               CALL TODHMS (LDAY, ITT)
C                                       Determine antennas.
               IF (I1LOCB.GE.0) THEN
                  LA1 = LBUFF(1+I1LOCB)/256. + 0.1
                  LA2 = LBUFF(1+I1LOCB) - LA1*256. + 0.1
               ELSE
                  LA1 = LBUFF(1+I1LOA1) + 0.1
                  LA2 = LBUFF(1+I1LOA2) + 0.1
                  END IF
               IF (I2LOCB.GE.0) THEN
                  LB1 = LBUFF2(1+I2LOCB)/256. + 0.1
                  LB2 = LBUFF2(1+I2LOCB) - LB1*256. + 0.1
               ELSE
                  LB1 = LBUFF2(1+I2LOA1) + 0.1
                  LB2 = LBUFF2(1+I2LOA2) + 0.1
                  END IF
               IF (TYPUVD.LE.0) THEN
                  IF ((LA1.LE.0) .OR. (LA1.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LA2.LE.0) .OR. (LA2.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LB1.LE.0) .OR. (LB1.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LB2.LE.0) .OR. (LB2.GT.MAXANT)) SDOIT = .TRUE.
                  END IF
C                                       Sort order swap ?
               IF ((ABS(LDAY-LDAY2).GT.1.2E-5) .OR. (LA1.NE.LB1) .OR.
     *            (LA2.NE.LB2)) THEN
C                                       Decode time.
                  XDAY = KBUFF(1+I1LOCT)
                  XDAY2 = KBUFF2(1+I2LOCT)
C                                       Determine antennas.
                  IF (I1LOCB.GE.0) THEN
                     IA1 = KBUFF(1+I1LOCB)/256. + 0.1
                     IA2 = KBUFF(1+I1LOCB) - IA1*256. + 0.1
                  ELSE
                     IA1 = KBUFF(1+I1LOA1) + 0.1
                     IA2 = KBUFF(1+I1LOA2) + 0.1
                     END IF
                  IF (I2LOCB.GE.0) THEN
                     IB1 = KBUFF2(1+I2LOCB)/256. + 0.1
                     IB2 = KBUFF2(1+I2LOCB) - IB1*256. + 0.1
                  ELSE
                     IB1 = KBUFF2(1+I2LOA1) + 0.1
                     IB2 = KBUFF2(1+I2LOA2) + 0.1
                     END IF
                  SRDOIT = (LA1.EQ.IB1) .AND. (LA2.EQ.IB2) .AND.
     *               (LB1.EQ.IA1) .AND. (LB2.EQ.IA2) .AND.
     *               (ABS(LDAY-XDAY2).LE.1.2E-5) .AND.
     *               (ABS(XDAY-LDAY2).LE.1.2E-5) .AND. (.NOT.DOLAST)
C                                       Yes: swap buffers
                  IF (SRDOIT) THEN
                     SRCONT = SRCONT + 1
                     LDAY2 = XDAY2
                     LB1 = IB1
                     LB2 = IB2
                     CALL RCOPY (LRECA, LBUFF2, KBUFF2)
                     CALL RCOPY (NRP2, BUFF2(IPINT2), LBUFF2)
                     IF (ISCMP2) THEN
                        CALL ZUVXPN (NCORR, BUFF2(IPINT2+NRP2),
     *                     BUFF2(IPINT2+LLOCWT), LBUFF2(DPTR))
                     ELSE
                        CALL RCOPY (LRECF, BUFF2(IPINT2+NRP2),
     *                     LBUFF2(DPTR))
                        END IF
C                                       No: error
                  ELSE
                     SDOIT = .TRUE.
                     END IF
                  END IF
C                                       Convert uvw to kilo lamda.
               U = LBUFF(1+I1LOCU) * 0.001
               V = LBUFF(1+I1LOCV) * 0.001
               W = LBUFF(1+I1LOCW) * 0.001
               U2 = LBUFF2(1+I2LOCU) * 0.001
               V2 = LBUFF2(1+I2LOCV) * 0.001
               W2 = LBUFF2(1+I2LOCW) * 0.001
               R = U * U + V * V
C                                       UV box and range
               IF (.NOT.SDOIT) THEN
                  IF ((R.LT.UVRANG(1)) .OR. (R.GT.UVRANG(2))) GO TO 170
                  IF (DOUVBX) THEN
                     IF ((ABS(U-U2).GT.APARM(3)) .OR. (ABS(V-V2).GT.
     *                  APARM(3)) .OR. (ABS(W-W2).GT.APARM(3)))
     *                  UDOIT = .TRUE.
                     END IF
                  END IF
C                                       Convert to VIS
               CALL GETVIS (MVIS, JADR, LBUFF(DPTR), VIS, IERR1)
               CALL GETVIS (MVIS, JADR, LBUFF2(DPTR), VIS2, IERR2)
               IF ((IERR1.NE.0) .AND. (IERR2.NE.0)) GO TO 170
               FDOIT = (IERR1.NE.IERR2) .AND. (APARM(4).GT.0.0)
C                                       Accumulate statistics
               DOIT = FDOIT .OR. UDOIT .OR. SDOIT
               IF (.NOT.DOIT) GO TO 170
               IF (IAPCNT.GE.NITER) NOPRNT = .TRUE.
               IF (.NOT.NOPRNT) IAPCNT = IAPCNT + 1
               IF (.NOT.NOPRNT) NCOUNT = NCOUNT + 1
               END IF
 170        IF (DOLAST) THEN
               GO TO 200
            ELSE
               CALL RCOPY (LRECA, KBUFF, LBUFF)
               CALL RCOPY (LRECA, KBUFF2, LBUFF2)
               END IF
 175        CONTINUE
         GO TO 100
C                                       Number lines printed
 200  IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
      NCOUNT = NCOUNT + 1
      IF ((IAPCNT.GE.NITER) .AND. (NITER.NE.NVIS)) THEN
         IF (DOCRT.LE.0.0) NCOUNT = NCOUNT + 1
         IPCNT = IPCNT + 2
         END IF
      IPCNT = IPCNT + 2
      NCOUNT = NCOUNT + 4
C                                       Write statistics in header of
C                                       first input file
 900  IF (IERR.EQ.0) THEN
         IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
         NCOUNT = NCOUNT + 1
         END IF
C                                       Close files.
      IF (IERR.LE.0) IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('CHKUBK ERROR:',I7,' INITIALIZING 1ST UV FILE')
 1065 FORMAT ('CHKUBX ERROR:',I7,' INITIALIZING 2ND UV FILE')
 1075 FORMAT ('CHKUBX: REQUESTED STOKES TYPE NOT AVAILABLE IN DATA SET')
 1100 FORMAT ('CHKUBX ERROR:',I7,' READING 1ST UV FILE VIS BLOCK ',I9)
 1105 FORMAT ('CHKUBX ERROR:',I7,' READING 2ND UV FILE VIS BLOCK ',I9)
      END
      SUBROUTINE CHKUCL (NCOUNT, IRET)
C-----------------------------------------------------------------------
C   CHKUPR reads the UV files and counts those samples meeting the
C   criteria given by OPCODE and APARM.
C   Output:
C      NCOUNT   I   Count of lines
C      IRET     I   Return 0 success or user requests end, else bad
C-----------------------------------------------------------------------
      INTEGER   NCOUNT, IRET
C
      INCLUDE 'UVDIF.INC'
      LOGICAL   F, DOIT, FDOIT, SDOIT, NOPRNT, DOLAST, SRDOIT
      INTEGER   JADR(8), BIND, CINC, ITYPE(8), BUFSZ,  J, LENBU, INDEX,
     *   MVIS, IROUND, I, L, IA1, IA2, ICH1, IERR, JERR, II, JJ, IPINT,
     *   LIMIT2, NIO, NUMCH, K, LRECF, NUMCHM, BIND2, NIO2, IPINT2, IB1,
     *   IB2, IERR2, BO, VO, XCOUNT, COUNT, IAPCNT, NCORR, LRECA, DPTR,
     *   LA1, LA2, LB1, LB2, IERR1, SRCONT
      REAL   VIS(3,8), TEMP, R, U, V, W, WTSC, U2, V2, W2, VIS2(3,8),
     *   XDAY, XDAY2, LDAY, LDAY2
      INCLUDE 'BUFFS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PUVD.INC'
      DATA F /.FALSE./
      DATA LENBU, BO, VO /16, 1, 0/
C-----------------------------------------------------------------------
      IRET = 8
      CINC = 2
      SRCONT = 0
      NOPRNT = .FALSE.
C                                       Set up for compressed data.
      NCORR = (LREC - NRPARM) / CATBLK(KINAX)
      LRECF = 3 * NCORR
      DPTR = 1 + MAX (NRP1, NRP2)
      LRECA = LRECF + DPTR - 1
      CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), KLOCWT,
     *   JERR)
      IF (ISCMP1 .AND. ((JERR.NE.0) .OR. (KLOCWT.LT.0))) THEN
         IRET = 5
         MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL AXEFND (8, 'WEIGHT  ', CAT2I(KIPCN), CAT2H(KHPTP), LLOCWT,
     *   JERR)
      IF (ISCMP2 .AND. ((JERR.NE.0) .OR. (LLOCWT.LT.0))) THEN
         IRET = 5
         MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                        Determine no. channels to do
      NUMCHM = CATBLK(KINAX+JLOCF) - NCH + 1
      NUMCH = (NACROS - 25) / (19 * NCOR)
      NUMCH = MAX (1, MIN (NUMCH, NUMCHM))
      LIMIT2 = NCOR * NUMCH
      ICH1 = NCH
      CALL FILL (8, 3, ITYPE)
      DO 10 II = 1,LIMIT2,NCOR
         DO 10 JJ = 1,NCOR
            INDEX = II + JJ - 1
            TEMP = CATD(KDCRV+JLOCS) + (JJ-CATR(KRCRP+JLOCS)) *
     *         CATR(KRCIC+JLOCS)
            I = IROUND(TEMP)
            L = 2
            IF (I.LT.0) L = 1
            I = ABS(I)
            ITYPE(INDEX) = 2
            IF (I+L.LE.3) ITYPE(INDEX) = 1
 10         CONTINUE
C                                       range parameters
      IF (XWT.LE.0.0) THEN
         XWT = 1.0
         NWT = 1.0
         END IF
      TEMP = LOG10 (XWT/0.995)
      I = TEMP + 99.0
      I =  100 - I
      WTSC = 10.0 ** I
      TEMP = NWT * WTSC
C                                       Make sure NITER is OK
      IF (NITER.GT.NVIS) NITER = NVIS
C                                       Write header
      IF (DOCRT.GT.-2.5) THEN
         NCOUNT = NCOUNT + 6
         IF (WTSC.NE.1.0) NCOUNT = NCOUNT + 1
         END IF
      NCOUNT = NCOUNT + 2
      IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
C                                       Initialize reading VIS. file.
      LENBU = 0
      BUFSZ = UVBFSS * 2
      CALL UVINIT ('READ', LUN, FIND, NVIS, VO, LREC, LENBU, BUFSZ,
     *   BUFF, BO, BIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
      CALL UVINIT ('READ', LUN2, FIND2, NVIS, VO, LREC2, LENBU, BUFSZ,
     *   BUFF2, BO, BIND2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1065) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
      COUNT = 0
      XCOUNT = -1
      IAPCNT = 0
C                                       Try to handle line
      CALL SETVIS (ICH1, NUMCH, BIF, MVIS, JADR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1075)
         CALL MSGWRT (8)
         GO TO 900
         END IF
C                                       Start looping thru data.
 100  CONTINUE
C                                       Read buffer.
         COUNT = COUNT + 1
         CALL UVDISK ('READ', LUN, FIND, BUFF, NIO, BIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, COUNT
            CALL MSGWRT (8)
            GO TO 900
            END   IF
         CALL UVDISK ('READ', LUN2, FIND2, BUFF2, NIO2, BIND2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1105) IERR, COUNT
            CALL MSGWRT (8)
            GO TO 900
            END IF
         DOLAST = (NIO.LE.0) .OR. (NIO2.LE.0)
         IPINT = BIND - LREC
         IPINT2 = BIND2 - LREC2
         NIO = MAX (1, NIO)
C                                       Loop thru buffer.
         DO 175 J = 1,NIO
C                                       Update counters
            XCOUNT = XCOUNT + 1
            IPINT = IPINT + LREC
            IPINT2 = IPINT2 + LREC2
C                                       CONVERT
            IF (.NOT.DOLAST) THEN
C                                       Compressed?
               CALL RCOPY (NRP1, BUFF(IPINT), KBUFF)
               IF (ISCMP1) THEN
                  CALL ZUVXPN (NCORR, BUFF(IPINT+NRP1),
     *               BUFF(IPINT+KLOCWT), KBUFF(DPTR))
               ELSE
                  CALL RCOPY (LRECF, BUFF(IPINT+NRP1), KBUFF(DPTR))
                  END IF
               CALL RCOPY (NRP2, BUFF2(IPINT2), KBUFF2)
               IF (ISCMP2) THEN
                  CALL ZUVXPN (NCORR, BUFF2(IPINT2+NRP2),
     *               BUFF2(IPINT2+LLOCWT), KBUFF2(DPTR))
               ELSE
                  CALL RCOPY (LRECF, BUFF2(IPINT2+NRP2), KBUFF2(DPTR))
                  END IF
               END IF
C                                       TEST
            IF (XCOUNT.GT.0) THEN
               SRDOIT = .FALSE.
               SDOIT = .FALSE.
               FDOIT = .FALSE.
C                                       Decode time.
               LDAY = LBUFF(1+I1LOCT)
               LDAY2 = LBUFF2(1+I2LOCT)
               IF ((LDAY.LT.0.0) .OR. (LDAY.GT.1000.)) SDOIT = .TRUE.
               IF ((LDAY2.LT.0.0) .OR. (LDAY2.GT.1000.)) SDOIT = .TRUE.
C                                       Determine antennas.
               IF (I1LOCB.GE.0) THEN
                  LA1 = LBUFF(1+I1LOCB)/256. + 0.1
                  LA2 = LBUFF(1+I1LOCB) - LA1*256. + 0.1
               ELSE
                  LA1 = LBUFF(1+I1LOA1) + 0.1
                  LA2 = LBUFF(1+I1LOA2) + 0.1
                  END IF
               IF (I2LOCB.GE.0) THEN
                  LB1 = LBUFF2(1+I2LOCB)/256. + 0.1
                  LB2 = LBUFF2(1+I2LOCB) - LB1*256. + 0.1
               ELSE
                  LB1 = LBUFF2(1+I2LOA1) + 0.1
                  LB2 = LBUFF2(1+I2LOA2) + 0.1
                  END IF
               IF (TYPUVD.LE.0) THEN
                  IF ((LA1.LE.0) .OR. (LA1.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LA2.LE.0) .OR. (LA2.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LB1.LE.0) .OR. (LB1.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LB2.LE.0) .OR. (LB2.GT.MAXANT)) SDOIT = .TRUE.
                  END IF
C                                       Sort order swap ?
               IF ((ABS(LDAY-LDAY2).GT.1.2E-5) .OR. (LA1.NE.LB1) .OR.
     *            (LA2.NE.LB2)) THEN
C                                       Decode time.
                  XDAY = KBUFF(1+I1LOCT)
                  XDAY2 = KBUFF2(1+I2LOCT)
C                                       Determine antennas.
                  IF (I1LOCB.GE.0) THEN
                     IA1 = KBUFF(1+I1LOCB)/256. + 0.1
                     IA2 = KBUFF(1+I1LOCB) - IA1*256. + 0.1
                  ELSE
                     IA1 = KBUFF(1+I1LOA1) + 0.1
                     IA2 = KBUFF(1+I1LOA2) + 0.1
                     END IF
                  IF (I2LOCB.GE.0) THEN
                     IB1 = KBUFF2(1+I2LOCB)/256. + 0.1
                     IB2 = KBUFF2(1+I2LOCB) - IB1*256. + 0.1
                  ELSE
                     IB1 = KBUFF2(1+I2LOA1) + 0.1
                     IB2 = KBUFF2(1+I2LOA2) + 0.1
                     END IF
                  SRDOIT = (LA1.EQ.IB1) .AND. (LA2.EQ.IB2) .AND.
     *               (LB1.EQ.IA1) .AND. (LB2.EQ.IA2) .AND.
     *               (ABS(LDAY-XDAY2).LE.1.2E-5) .AND.
     *               (ABS(XDAY-LDAY2).LE.1.2E-5) .AND. (.NOT.DOLAST)
C                                       Yes: swap buffers
                  IF (SRDOIT) THEN
                     SRCONT = SRCONT + 1
                     LDAY2 = XDAY2
                     LB1 = IB1
                     LB2 = IB2
                     CALL RCOPY (LRECA, LBUFF2, KBUFF2)
                     IF (ISCMP2) THEN
                        CALL RCOPY (NRP2, BUFF2(IPINT2), LBUFF2)
                        CALL ZUVXPN (NCORR, BUFF2(IPINT2+NRP2),
     *                     BUFF2(IPINT2+LLOCWT), LBUFF2(DPTR))
                     ELSE
                        CALL RCOPY (LRECF, BUFF2(IPINT2+NRP2),
     *                     LBUFF2(DPTR))
                        END IF
C                                       No: error
                  ELSE
                     SDOIT = .TRUE.
                     END IF
                  END IF
C                                       Convert uvw to kilo lamda.
               U = LBUFF(1+I1LOCU) * 0.001
               V = LBUFF(1+I1LOCV) * 0.001
               W = LBUFF(1+I1LOCW) * 0.001
               U2 = LBUFF2(1+I2LOCU) * 0.001
               V2 = LBUFF2(1+I2LOCV) * 0.001
               W2 = LBUFF2(1+I2LOCW) * 0.001
               R = U * U + V * V
C                                       UV box and range
               IF (.NOT.SDOIT) THEN
                  IF ((R.LT.UVRANG(1)) .OR. (R.GT.UVRANG(2))) GO TO 170
                  END IF
C                                       Convert to VIS
               CALL GETVIS (MVIS, JADR, LBUFF(DPTR), VIS, IERR1)
               CALL GETVIS (MVIS, JADR, LBUFF2(DPTR), VIS2, IERR2)
               IF ((IERR1.NE.0) .AND. (IERR2.NE.0)) GO TO 170
               FDOIT = (IERR1.NE.IERR2) .AND. (APARM(4).GT.0.0)
C                                       Accumulate statistics
               DOIT = FDOIT .OR. SDOIT
C                                       Do we want this one
               IF ((.NOT.DOIT) .AND. (DOMAX)) THEN
                  DO 130 K = 1,MVIS
                     IF ((ITYPE(K).LE.2) .AND. ((VIS(3,K).GT.0.0) .OR.
     *                  (VIS2(3,K).GT.0.0) .OR. (DOFLAG))) THEN
                        IF (ABS(VIS(1,K)-VIS2(1,K)).GT.APARM(ITYPE(K)))
     *                     FDOIT = .TRUE.
                        IF (ABS(VIS(2,K)-VIS2(2,K)).GT.APARM(ITYPE(K)))
     *                     FDOIT = .TRUE.
                        IF (APARM(4).GT.0.0) THEN
                           IF (ABS(VIS(3,K)-VIS2(3,K)).GT.APARM(4))
     *                        FDOIT = .TRUE.
                           IF ((VIS(3,K).LE.0.0).AND.(VIS2(3,K).GT.0.0))
     *                        FDOIT = .TRUE.
                           IF ((VIS(3,K).GT.0.0).AND.(VIS2(3,K).LE.0.0))
     *                        FDOIT = .TRUE.
                        ELSE
                           IF (ABS(ABS(VIS(3,K))-ABS(VIS2(3,K))).GT.
     *                        -APARM(4)) FDOIT = .TRUE.
                           END IF
                        END IF
 130                 CONTINUE
                  DOIT = FDOIT
                  END IF
               IF (.NOT.DOIT) GO TO 170
               IF (IAPCNT.GE.NITER) NOPRNT = .TRUE.
               IF (.NOT.NOPRNT) IAPCNT = IAPCNT + 1
               IF (.NOT.NOPRNT) NCOUNT = NCOUNT + 1
               END IF
 170        IF (DOLAST) THEN
               GO TO 200
            ELSE
               CALL RCOPY (LRECA, KBUFF, LBUFF)
               CALL RCOPY (LRECA, KBUFF2, LBUFF2)
               END IF
 175        CONTINUE
         GO TO 100
C                                       Number lines printed
 200  IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
      NCOUNT = NCOUNT + 1
      IF ((IAPCNT.LT.NITER) .OR. (NITER.EQ.NVIS)) GO TO 210
         NCOUNT = NCOUNT + 1
         IPCNT = IPCNT + 2
 210  IPCNT = IPCNT + 2
      NCOUNT = NCOUNT + 3
C                                       Write statistics in header of
C                                       first input file
 900  IF (IERR.EQ.0) THEN
         IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
         NCOUNT = NCOUNT + 1
         END IF
C                                       Close files.
      IF (IERR.LE.0) IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('CHKUCL ERROR:',I7,' INITIALIZING 1ST UV FILE')
 1065 FORMAT ('CHKUCL ERROR:',I7,' INITIALIZING 2ND UV FILE')
 1075 FORMAT ('CHKUCL: REQUESTED STOKES TYPE NOT AVAILABLE IN DATA SET')
 1100 FORMAT ('CHKUCL ERROR:',I7,' READING 1ST UV FILE VIS BLOCK ',I9)
 1105 FORMAT ('CHKUCL ERROR:',I7,' READING 2ND UV FILE VIS BLOCK ',I9)
      END
      SUBROUTINE CHKUPR (NCOUNT, IRET)
C-----------------------------------------------------------------------
C   CHKUPR reads the UV files and counts those samples meeting the
C   criteria given by OPCODE and APARM.
C   Output:
C      NCOUNT   I   Count of lines
C      IRET     I   Return 0 success or user requests end, else bad
C-----------------------------------------------------------------------
      INTEGER   NCOUNT, IRET
C
      INCLUDE 'UVDIF.INC'
      LOGICAL   F, DOIT, FDOIT, UDOIT, SDOIT, NOPRNT, DOLAST,
     *   SRDOIT
      INTEGER   JADR(8), BIND, CINC, ITYPE(8), BUFSZ, J, ITT(4),
     *   LENBU, INDEX, MVIS, IROUND, I, L, IA1, IA2, ICH1, IERR, JERR,
     *   II, JJ, IPINT, LIMIT2, NIO, NNCH, NUMCH, K, LRECF, NUMCHM,
     *   BIND2, NIO2, IPINT2, IB1, IB2, IERR2, BO, VO, XCOUNT,
     *   COUNT, IAPCNT, NCORR, LRECA, DPTR, LA1, LA2, LB1, LB2, IERR1,
     *   SRCONT
      REAL   VIS(3,8), TEMP, R, U, V, W, WTSC, U2, V2, W2, VIS2(3,8),
     *   XDAY, XDAY2, LDAY, LDAY2
      INCLUDE 'BUFFS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PUVD.INC'
      DATA F /.FALSE./
      DATA LENBU, BO, VO /16, 1, 0/
C-----------------------------------------------------------------------
      IRET = 8
      CINC = 2
      SRCONT = 0
      NOPRNT = .FALSE.
C                                       range parameters
      IF (XWT.LE.0.0) THEN
         XWT = 1.0
         NWT = 1.0
         END IF
      TEMP = LOG10 (XWT/0.995)
      I = TEMP + 99.0
      I =  100 - I
      WTSC = 10.0 ** I
      TEMP = NWT * WTSC
      IF (TEMP.LT.1.0) THEN
         MSGTXT = 'Full dynamic range of weights cannot be printed'
         CALL MSGWRT (6)
         END IF
C                                       Set up for compressed data.
      NCORR = (LREC - NRPARM) / CATBLK(KINAX)
      LRECF = 3 * NCORR
      DPTR = 1 + MAX (NRP1, NRP2)
      LRECA = LRECF + DPTR - 1
      CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), KLOCWT,
     *   JERR)
      IF (ISCMP1 .AND. ((JERR.NE.0) .OR. (KLOCWT.LT.0))) THEN
         IRET = 5
         MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL AXEFND (8, 'WEIGHT  ', CAT2I(KIPCN), CAT2H(KHPTP), LLOCWT,
     *   JERR)
      IF (ISCMP2 .AND. ((JERR.NE.0) .OR. (LLOCWT.LT.0))) THEN
         IRET = 5
         MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                        Determine no. channels to do
      NUMCHM = CATBLK(KINAX+JLOCF) - NCH + 1
      NACROS = 132
      NUMCH = (NACROS - 52) / (15 * NCOR)
      NUMCH = MAX (1, MIN (NUMCH, NUMCHM))
      LIMIT2 = NCOR * NUMCH
      ICH1 = NCH
      NNCH = NCH - 1
      CALL FILL (8, 3, ITYPE)
      DO 10 II = 1,LIMIT2,NCOR
         NNCH = NNCH + 1
         DO 10 JJ = 1,NCOR
            INDEX = II + JJ - 1
            TEMP = CATD(KDCRV+JLOCS) + (JJ-CATR(KRCRP+JLOCS)) *
     *         CATR(KRCIC+JLOCS)
            I = IROUND(TEMP)
            L = 2
            IF (I.LT.0) L = 1
            I = ABS(I)
            ITYPE(INDEX) = 2
            IF (I+L.LE.3) ITYPE(INDEX) = 1
 10         CONTINUE
C                                       Make sure NITER is OK
      IF (NITER.GT.NVIS) NITER = NVIS
C                                       Write header
      IF (DOCRT.GT.-2.5) THEN
         NCOUNT = NCOUNT + 5
         IF (WTSC.NE.1.0) NCOUNT = NCOUNT + 1
         END IF
      IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 2
      NCOUNT = NCOUNT + 2
C                                       Initialize reading VIS. file.
      LENBU = 0
      BUFSZ = UVBFSS * 2
      CALL UVINIT ('READ', LUN, FIND, NVIS, VO, LREC, LENBU, BUFSZ,
     *   BUFF, BO, BIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
      CALL UVINIT ('READ', LUN2, FIND2, NVIS, VO, LREC2, LENBU, BUFSZ,
     *   BUFF2, BO, BIND2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1065) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
      COUNT = 0
      XCOUNT = -1
      IAPCNT = 0
C                                       Try to handle line
      CALL SETVIS (ICH1, NUMCH, BIF, MVIS, JADR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1075)
         CALL MSGWRT (8)
         GO TO 900
         END IF
C                                       Start looping thru data.
 100  CONTINUE
C                                       Read buffer.
         COUNT = COUNT + 1
         CALL UVDISK ('READ', LUN, FIND, BUFF, NIO, BIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, COUNT
            CALL MSGWRT (8)
            GO TO 900
            END IF
         CALL UVDISK ('READ', LUN2, FIND2, BUFF2, NIO2, BIND2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1105) IERR, COUNT
            CALL MSGWRT (8)
            GO TO 900
            END IF
         DOLAST = (NIO.LE.0) .OR. (NIO2.LE.0)
         IPINT = BIND - LREC
         IPINT2 = BIND2 - LREC2
         NIO = MAX (1, NIO)
C                                       Loop thru buffer.
         DO 175 J = 1,NIO
C                                       Update counters
            XCOUNT = XCOUNT + 1
            IPINT = IPINT + LREC
            IPINT2 = IPINT2 + LREC2
C                                       CONVERT
            IF (.NOT.DOLAST) THEN
C                                       Compressed?
               CALL RCOPY (NRP1, BUFF(IPINT), KBUFF)
               IF (ISCMP1) THEN
                  CALL ZUVXPN (NCORR, BUFF(IPINT+NRP1),
     *               BUFF(IPINT+KLOCWT), KBUFF(DPTR))
               ELSE
                  CALL RCOPY (LRECF, BUFF(IPINT+NRP1), KBUFF(DPTR))
                  END IF
               CALL RCOPY (NRP2, BUFF2(IPINT2), KBUFF2)
               IF (ISCMP2) THEN
                  CALL ZUVXPN (NCORR, BUFF2(IPINT2+NRP2),
     *               BUFF2(IPINT2+LLOCWT), KBUFF2(DPTR))
               ELSE
                  CALL RCOPY (LRECF, BUFF2(IPINT2+NRP2), KBUFF2(DPTR))
                  END IF
               END IF
C                                       TEST
            IF (XCOUNT.GT.0) THEN
               SRDOIT = .FALSE.
               SDOIT = .FALSE.
               UDOIT = .FALSE.
               FDOIT = .FALSE.
C                                       Decode time.
               LDAY = LBUFF(1+I1LOCT)
               LDAY2 = LBUFF2(1+I2LOCT)
               IF ((LDAY.LT.0.0) .OR. (LDAY.GT.1000.)) SDOIT = .TRUE.
               IF ((LDAY2.LT.0.0) .OR. (LDAY2.GT.1000.)) SDOIT = .TRUE.
               CALL TODHMS (LDAY, ITT)
C                                       Determine antennas.
               IF (I1LOCB.GE.0) THEN
                  LA1 = LBUFF(1+I1LOCB)/256. + 0.1
                  LA2 = LBUFF(1+I1LOCB) - LA1*256. + 0.1
               ELSE
                  LA1 = LBUFF(1+I1LOA1) + 0.1
                  LA2 = LBUFF(1+I1LOA2) + 0.1
                  END IF
               IF (I2LOCB.GE.0) THEN
                  LB1 = LBUFF2(1+I2LOCB)/256. + 0.1
                  LB2 = LBUFF2(1+I2LOCB) - LB1*256. + 0.1
               ELSE
                  LB1 = LBUFF2(1+I2LOA1) + 0.1
                  LB2 = LBUFF2(1+I2LOA2) + 0.1
                  END IF
               IF (TYPUVD.LE.0) THEN
                  IF ((LA1.LE.0) .OR. (LA1.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LA2.LE.0) .OR. (LA2.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LB1.LE.0) .OR. (LB1.GT.MAXANT)) SDOIT = .TRUE.
                  IF ((LB2.LE.0) .OR. (LB2.GT.MAXANT)) SDOIT = .TRUE.
                  END IF
C                                       Sort order swap ?
               IF ((ABS(LDAY-LDAY2).GT.1.2E-5) .OR. (LA1.NE.LB1) .OR.
     *            (LA2.NE.LB2)) THEN
C                                       Decode time.
                  XDAY = KBUFF(1+I1LOCT)
                  XDAY2 = KBUFF2(1+I2LOCT)
C                                       Determine antennas.
                  IF (I1LOCB.GE.0) THEN
                     IA1 = KBUFF(1+I1LOCB)/256. + 0.1
                     IA2 = KBUFF(1+I1LOCB) - IA1*256. + 0.1
                  ELSE
                     IA1 = KBUFF(1+I1LOA1) + 0.1
                     IA2 = KBUFF(1+I1LOA2) + 0.1
                     END IF
                  IF (I2LOCB.GE.0) THEN
                     IB1 = KBUFF2(1+I2LOCB)/256. + 0.1
                     IB2 = KBUFF2(1+I2LOCB) - IB1*256. + 0.1
                  ELSE
                     IB1 = KBUFF2(1+I2LOA1) + 0.1
                     IB2 = KBUFF2(1+I2LOA2) + 0.1
                     END IF
                  SRDOIT = (LA1.EQ.IB1) .AND. (LA2.EQ.IB2) .AND.
     *               (LB1.EQ.IA1) .AND. (LB2.EQ.IA2) .AND.
     *               (ABS(LDAY-XDAY2).LE.1.2E-5) .AND.
     *               (ABS(XDAY-LDAY2).LE.1.2E-5) .AND. (.NOT.DOLAST)
C                                       Yes: swap buffers
                  IF (SRDOIT) THEN
                     SRCONT = SRCONT + 1
                     LDAY2 = XDAY2
                     LB1 = IB1
                     LB2 = IB2
                     CALL RCOPY (LRECA, LBUFF2, KBUFF2)
                     IF (ISCMP2) THEN
                        CALL RCOPY (NRP2, BUFF2(IPINT2), LBUFF2)
                        CALL ZUVXPN (NCORR, BUFF2(IPINT2+NRP2),
     *                     BUFF2(IPINT2+LLOCWT), LBUFF2(DPTR))
                     ELSE
                        CALL RCOPY (LRECA, BUFF2(IPINT2), LBUFF2)
                        END IF
C                                       No: error
                  ELSE
                     SDOIT = .TRUE.
                     END IF
                  END IF
C                                       Convert uvw to kilo lamda.
               U = LBUFF(1+I1LOCU) * 0.001
               V = LBUFF(1+I1LOCV) * 0.001
               W = LBUFF(1+I1LOCW) * 0.001
               U2 = LBUFF2(1+I2LOCU) * 0.001
               V2 = LBUFF2(1+I2LOCV) * 0.001
               W2 = LBUFF2(1+I2LOCW) * 0.001
               R = U * U + V * V
C                                       UV box and range
               IF (.NOT.SDOIT) THEN
                  IF ((R.LT.UVRANG(1)) .OR. (R.GT.UVRANG(2))) GO TO 170
                  IF (DOUVBX) THEN
                     IF ((ABS(U-U2).GT.APARM(3)) .OR. (ABS(V-V2).GT.
     *                  APARM(3)) .OR. (ABS(W-W2).GT.APARM(3)))
     *                  UDOIT = .TRUE.
                     END IF
                  END IF
C                                       Convert to VIS
               CALL GETVIS (MVIS, JADR, LBUFF(DPTR), VIS, IERR1)
               CALL GETVIS (MVIS, JADR, LBUFF2(DPTR), VIS2, IERR2)
               IF ((IERR1.NE.0) .AND. (IERR2.NE.0)) GO TO 170
               FDOIT = (IERR1.NE.IERR2) .AND. (APARM(4).GT.0.0)
C                                       Accumulate statistics
               DOIT = FDOIT .OR. UDOIT .OR. SDOIT
C                                       Do we want this one
               IF ((.NOT.DOIT) .AND. (DOMAX)) THEN
                  DO 130 K = 1,MVIS
                     IF ((ITYPE(K).LE.2) .AND. ((VIS(3,K).GT.0.0) .OR.
     *                  (VIS2(3,K).GT.0.0) .OR. (DOFLAG))) THEN
                        IF (ABS(VIS(1,K)-VIS2(1,K)).GT.APARM(ITYPE(K)))
     *                     FDOIT = .TRUE.
                        IF (ABS(VIS(2,K)-VIS2(2,K)).GT.APARM(ITYPE(K)))
     *                     FDOIT = .TRUE.
                        IF (APARM(4).GT.0.0) THEN
                           IF (ABS(VIS(3,K)-VIS2(3,K)).GT.APARM(4))
     *                        FDOIT = .TRUE.
                           IF ((VIS(3,K).LE.0.0).AND.(VIS2(3,K).GT.0.0))
     *                        FDOIT = .TRUE.
                           IF ((VIS(3,K).GT.0.0).AND.(VIS2(3,K).LE.0.0))
     *                        FDOIT = .TRUE.
                        ELSE
                           IF (ABS(ABS(VIS(3,K))-ABS(VIS2(3,K))).GT.
     *                        -APARM(4)) FDOIT = .TRUE.
                           END IF
                        END IF
 130                 CONTINUE
                  DOIT = FDOIT
                  END IF
               IF (.NOT.DOIT) GO TO 170
               IF (IAPCNT.GE.NITER) NOPRNT = .TRUE.
               IF (.NOT.NOPRNT) IAPCNT = IAPCNT + 1
               IF (NOPRNT) GO TO 170
               NCOUNT = NCOUNT + 1
               END IF
 170        IF (DOLAST) THEN
               GO TO 200
            ELSE
               CALL RCOPY (LRECA, KBUFF, LBUFF)
               CALL RCOPY (LRECA, KBUFF2, LBUFF2)
               END IF
 175        CONTINUE
         GO TO 100
C                                       Number lines printed
 200  NCOUNT = NCOUNT + 2
      IF ((IAPCNT.LT.NITER) .OR. (NITER.EQ.NVIS)) GO TO 210
         NCOUNT = NCOUNT + 1
         IPCNT = IPCNT + 2
 210  IPCNT = IPCNT + 2
      NCOUNT = NCOUNT + 4
C                                       Write statistics in header of
C                                       first input file
 900  IF (IERR.EQ.0) THEN
         IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
         NCOUNT = NCOUNT + 1
         END IF
C                                       Close files.
      IF (IERR.LE.0) IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('CHKUPR ERROR:',I7,' INITIALIZING 1ST UV FILE')
 1065 FORMAT ('CHKUPR ERROR:',I7,' INITIALIZING 2ND UV FILE')
 1075 FORMAT ('CHKUPR REQUESTED STOKES TYPE NOT AVAILABLE IN DATA SET')
 1100 FORMAT ('CHKUPR ERROR:',I7,' READING 1ST UV FILE VIS BLOCK ',I9)
 1105 FORMAT ('CHKUPR ERROR:',I7,' READING 2ND UV FILE VIS BLOCK ',I9)
      END
