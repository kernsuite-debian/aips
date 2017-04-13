LOCAL INCLUDE 'PRTIM.INC'
C                                       Local include for PRTIM
      HOLLERITH XNAMIN(3), XCLSIN(2), XLPNAM(12)
      CHARACTER NAMEIN*12, CLASIN*6, LPNAME*48, TITL1*132,
     *   TITL2*132, ALINE*132, SCRTCH*132
      REAL      RSEQIN, DISKIN, BLC(7), TRC(7), RNDIG, MULT, RXINC,
     *   RYINC, XDOCRT, DOCRT
      LOGICAL   SHORT
      INTEGER   LINMAX, LUNPRT, INDPRT, LINPRT, IPAGE
      COMMON /CHRTIM/ ALINE, TITL1, TITL2, SCRTCH, NAMEIN, CLASIN,
     *   LPNAME
      COMMON /CPRTIM/ DOCRT, SHORT, LINMAX, LUNPRT, INDPRT, LINPRT,
     *   IPAGE
      COMMON /INPARM/ XNAMIN, XCLSIN, RSEQIN, DISKIN, BLC, TRC, RNDIG,
     *   MULT, RXINC, RYINC, XDOCRT, XLPNAM
LOCAL END
      PROGRAM PRTIM
C-----------------------------------------------------------------------
C! Task displays a map on line-printer or terminal
C# Map-util Plot-util Printer
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998-1999, 2004, 2007, 2009, 2014-2016
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
C   PRTIM displays a map on a line-printer or terminal device.
C   Inputs:
C      USERID     R     Print user number
C      INNAME(3)  H     Image name (name)
C      INCLASS(2) H     Image name (class)
C      INSEQ      R     Image name (seq. #)
C      INDISK     R     Input disk unit
C      BLC(7)     R     Bottom left corner
C      TRC(7)     R     Top right corner
C      NDIG       R     Number of digits in display
C      FACTOR     R     Multiplication factor
C      XINC       R     Increment columns
C      YINC       R     Increment rows
C      DOCRT      R     >0 -> use CRT, else line printer
C      OUTPRINT   H     File name to keep printer output
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER MTYPE*2, PRGNAM*6, STR*4
      INTEGER   CNO, DLUN, MIND, IRET, NPARM, IERR, NDIG, ISCR(256),
     *   WIND(4), NGET, USER, IVOL, KVOL, DEPTH(7), IBLKOF, SEQIN, NX,
     *   NY, NLINES, NBKOF, ININD, ILO, I, J, LINE, JBUFSZ, NCOL, NPL,
     *   JJ, XINC, YINC, L3, L3L, L3U, L4, L4L, L4U, L5, L5L, L5U, L6,
     *   L6L, L6U, L7, L7L, L7U, IPRN(140), BLANK3, IROUND, NCOUNT,
     *   TTY(2)
      LOGICAL   F, RQUICK, DONE, FIRST
      REAL      MSCAL, DMAX,  DMIN, SCALE, VMAX, RBUFF(MABFSS), CON1,
     *   TEMP, RMAX, RMIN
      INCLUDE 'PRTIM.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (ISCR(1), RBUFF(1))
      DATA DLUN, NBKOF /16, 1/
      DATA F /.FALSE./
      DATA PRGNAM /'PRTIM '/
C-----------------------------------------------------------------------
C                                       Initialize for 1 map and
C                                       three non-map files
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Initialize from AIPS
      NPARM = 38
      CALL GTPARM (PRGNAM, NPARM, RQUICK, XNAMIN, ISCR, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
         END IF
      DOCRT = XDOCRT
      IF ((IRET.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      IF (DOCRT.GT.0.0) RQUICK = .FALSE.
      IF (RQUICK) RQUICK = LPNAME.NE.' '
      IF (RQUICK) CALL RELPOP (IRET, ISCR, IERR)
      IF (IRET.NE.0) GO TO 995
      IRET = 4

C                                       Assign parameters
      SEQIN = RSEQIN + 0.01
      IVOL = DISKIN + 0.01
      NDIG = RNDIG + 0.01
      XINC = RXINC + 0.01
      YINC = RYINC + 0.01
      IF (NDIG.LE.0) NDIG = 1
      IF (NDIG.GT.7) NDIG = 7
      IF (MULT.LE.0.) MULT = 1.0
      IF (XINC.LE.0) XINC = 1
      IF (YINC.LE.0) YINC = 1
      USER = NLUSER
      CALL H2CHR (12, 1, XNAMIN, NAMEIN)
      CALL H2CHR (6, 1, XCLSIN, CLASIN)
C                                       open map: get cat header
      KVOL = IVOL
      MTYPE = 'MA'
      CALL MAPOPN ('READ', KVOL, NAMEIN, CLASIN, SEQIN, MTYPE, USER,
     *    DLUN, MIND, CNO, CATBLK, ISCR, IERR)
      IF (IERR.GT.1) GO TO 995
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
C                                       Determine scale factor
      DMAX = CATR(KRDMX)
      DMIN = CATR(KRDMN)
      VMAX = MAX (ABS(DMAX), ABS(DMIN))
      IF ((DMIN.LT.0.0) .AND. (NDIG.GT.1)) VMAX = MAX (VMAX,
     *   10.0 * ABS(DMIN))
      IF (VMAX.LE.0.0) THEN
         WRITE (MSGTXT,1015)
         CALL MSGWRT (8)
         GO TO 960
         END IF
      I = LOG10 (VMAX) + 25.0
      I = I - 24
      VMAX = 10.0**I
      MSCAL = VMAX / MULT
      SCALE = (10.0**NDIG) / MSCAL
      JBUFSZ = MABFSS * 2
C                                       Set up window in y
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 960
      WIND(2) = TRC(2) + 0.01
      WIND(4) = BLC(2) + 0.01
C                                       NLINES  total number of lines
      NLINES = WIND(2) - WIND(4) + 1
C                                       open printer excl
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNPRT, INDPRT, LINMAX, ISCR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) LUNPRT, IERR
         CALL MSGWRT (8)
         GO TO 960
         END IF
      SHORT = LINMAX.LT.110
C                                       Get various measures per line
C                                       NPL = # of col/line
C                                       NCOL = range of col num/line
      NPL = (LINMAX - 6) / NDIG
      NCOL = NPL * XINC
C                                       Set up some counter
      IPAGE = 0
      CON1 = SCALE
      RMAX = 10.**NDIG + 1.5
      RMIN = 10.**MAX(NDIG-1, 1) + 1.5
      RMIN = -RMIN
      BLANK3 = RMIN - 2
      DEPTH(1) = (TRC(1) + BLC(1)) / 2.0 + 0.5
      DEPTH(2) = (TRC(2) + BLC(2)) / 2.0 + 0.5
      L3L = BLC(3) + 0.01
      L4L = BLC(4) + 0.01
      L5L = BLC(5) + 0.01
      L6L = BLC(6) + 0.01
      L7L = BLC(7) + 0.01
      L3U = TRC(3) + 0.01
      L4U = TRC(4) + 0.01
      L5U = TRC(5) + 0.01
      L6U = TRC(6) + 0.01
      L7U = TRC(7) + 0.01
C                                       count lines
      IF ((DOCRT.LE.0.0) .AND. (LPNAME.EQ. ' ')) THEN
         NCOUNT = 0
         MSGTXT = 'Checking count of lines for direct output to printer'
         CALL MSGWRT (2)
C                                       loop over planes
         DO 100 L7 = L7L,L7U
         DO 100 L6 = L6L,L6U
         DO 100 L5 = L5L,L5U
         DO 100 L4 = L4L,L4U
         DO 100 L3 = L3L,L3U
            WIND(1) = BLC(1) + 0.01
            WIND(1) = WIND(1) - NCOL
            WIND(3) = WIND(1) + NCOL - 1
            DEPTH(3) = L3
            DEPTH(4) = L4
            DEPTH(5) = L5
            DEPTH(6) = L6
            DEPTH(7) = L7
            CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), DEPTH(3), IBLKOF,
     *         IERR)
            IF (IERR.NE.0) GO TO 950
            IBLKOF = IBLKOF + NBKOF
            DONE = .FALSE.
            FIRST = .TRUE.
C                                       Begin paging loop
 50         WIND(1) = WIND(1) + NCOL
               WIND(3) = WIND(3) + NCOL
               LINE = WIND(2) + 1
C                                       Is this the last page?
               IF (WIND(3).GE.TRC(1)) THEN
                  DONE = .TRUE.
                  WIND(3) = TRC(1) + 0.01
                  END IF
               NGET = WIND(3) - WIND(1) + 1
C                                       Write the page headers
               IF (FIRST) THEN
                  NCOUNT = NCOUNT + 9 + CATBLK(KIDIM)
               ELSE
                  NCOUNT = 2 + NCOUNT
                  END IF
               FIRST = .FALSE.
C                                       Read a line from the map
               DO 90 I = 1,NLINES
C                                       Do we want this line?
                  IF (MOD(I-1,YINC).EQ.0) NCOUNT = NCOUNT + 1
 90               CONTINUE
C                                       Was this the last line?
               IF (.NOT.DONE) GO TO 50
 100        CONTINUE
         IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
            IF (NCOUNT.GT.1000) THEN
               IRET = -1
               LINE = -1
               CALL LPCLOS (LUNPRT, INDPRT, LINE, IERR)
               END IF
         ELSE IF (NCOUNT.GT.500) THEN
            TTY(1) = 5
            CALL ZOPEN (TTY(1), TTY(2), 1, SCRTCH, .FALSE., .FALSE.,
     *         .TRUE., IRET)
            MSGTXT = 'PROBLEM OPENING TERMINAL'
            IF (IRET.GT.0) GO TO 940
            WRITE (SCRTCH,1100) NCOUNT
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, IRET)
            MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
            IF (IRET.GT.0) GO TO 940
            SCRTCH = 'Do you really want to print this much??' //
     *         ' Enter Y or y if so'
            CALL INQSTR (TTY, SCRTCH, 1, STR, IRET)
            IF (IRET.GT.0) GO TO 940
            IF ((STR(:1).NE.'y') .AND. (STR(:1).NE.'Y')) THEN
               LINE = -1
               CALL LPCLOS (LUNPRT, INDPRT, LINE, IERR)
               IRET = -1
               SCRTCH = 'Good choice - save trees'
            ELSE
               SCRTCH = 'OKAY, printing anyway'
               END IF
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, I)
            CALL ZCLOSE (TTY(1), TTY(2), I)
            IF (IRET.NE.0) GO TO 960
            END IF
         END IF
C                                       loop over planes
      DO 200 L7 = L7L,L7U
      DO 200 L6 = L6L,L6U
      DO 200 L5 = L5L,L5U
      DO 200 L4 = L4L,L4U
      DO 200 L3 = L3L,L3U
         WIND(1) = BLC(1) + 0.01
         WIND(1) = WIND(1) - NCOL
         WIND(3) = WIND(1) + NCOL - 1
         DEPTH(3) = L3
         DEPTH(4) = L4
         DEPTH(5) = L5
         DEPTH(6) = L6
         DEPTH(7) = L7
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), DEPTH(3), IBLKOF,
     *      IERR)
         IF (IERR.NE.0) GO TO 950
         IBLKOF = IBLKOF + NBKOF
         DONE = .FALSE.
         FIRST = .TRUE.
C                                       Begin paging loop
 150     WIND(1) = WIND(1) + NCOL
            WIND(3) = WIND(3) + NCOL
            LINE = WIND(2) + 1
C                                       Is this the last page?
            IF (WIND(3).GE.TRC(1)) THEN
               DONE = .TRUE.
               WIND(3) = TRC(1) + 0.01
               END IF
            NGET = WIND(3) - WIND(1) + 1
C                                       Write the page headers
            CALL PHEAD (WIND, DEPTH, XINC, NDIG, MSCAL, FIRST, IERR)
            IF (IERR.NE.0) GO TO 950
            FIRST = .FALSE.
C                                       Initialize the mapfile
            CALL MINIT ('READ', DLUN, MIND, NX, NY, WIND, RBUFF, JBUFSZ,
     *         IBLKOF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1055) IERR
               CALL MSGWRT (8)
               GO TO 950
               END IF
C                                       Read a line from the map
            DO 190 I = 1,NLINES
               CALL MDISK ('READ', DLUN, MIND, RBUFF, ININD, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1060) IERR, I
                  CALL MSGWRT (8)
                  GO TO 950
                  END IF
               LINE = LINE - 1
C                                       Do we want this line?
               IF (MOD(I-1,YINC).EQ.0) THEN
                  ILO = ININD - 1
                  JJ = 0
C                                       Scale line and store:
                  DO 170 J = 1,NGET,XINC
                     JJ = JJ + 1
                     TEMP = RBUFF(ILO+J)
                     IF (TEMP.EQ.FBLANK) THEN
                        IPRN(JJ) = BLANK3
                     ELSE
                        TEMP = TEMP * CON1
                        TEMP = MAX (RMIN, MIN (RMAX, TEMP))
                        IPRN(JJ) = IROUND (TEMP)
                        END IF
 170                 CONTINUE
C                                          Write out this line
                  CALL PLINE (LINE, IPRN, JJ, NDIG, BLANK3, IERR)
                  IF (IERR.NE.0) GO TO 950
                  END IF
 190           CONTINUE
C                                       Was this the last line?
            IF (.NOT.DONE) GO TO 150
 200     CONTINUE
      GO TO 950
C                                       Close mapfile and directory
 940  CALL MSGWRT (8)
C
 950  IF (IERR.LE.0) IRET = 0
      CALL LPCLOS (LUNPRT, INDPRT, LINPRT, IERR)
 960  CALL MAPCLS ('READ', KVOL, CNO, DLUN, MIND, CATBLK, F, ISCR, IERR)
C
 995  IRET = MAX (0, IRET)
      CALL DIETSK (IRET, RQUICK, ISCR)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR IN GTPARM=',I4)
 1015 FORMAT ('MAP IS ALL ZERO')
 1040 FORMAT ('PRINTER LUN',I3,' OPEN ERR =',I6)
 1055 FORMAT ('CAN''T INITIALIZE MAP.  IER=',I6)
 1060 FORMAT ('ERROR IN READING MAP.  IER=',I6,'  LINE=',I4)
 1100 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE PHEAD (WIND, DEPTH, XINC, NDIG, DSEC, FIRST, IERR)
C-----------------------------------------------------------------------
C   PHEAD prints page headers for PRTIM
C   Inputs:
C      WIND    I(4)   x-y window of the moment
C      DEPTH   I(7)   depth on axes 1 - 7
C      XINC    I      increment on first axis
C      NDIG    I      # digits in print out
C      DSEC    R      Data max at 10 ** NDIG
C      FIRST   L      T => full header
C   Output:
C      IERR    I      0 -> OK, -1 user asks quit, > 0 error.
C   Uses the catalog header block in common /MAPHDR/
C-----------------------------------------------------------------------
      INTEGER   WIND(4), DEPTH(7), XINC, NDIG, IERR
      REAL      DSEC
      LOGICAL   FIRST
C
      CHARACTER BLANK*8, WPROC(5)*8, CHSTOK(20)*4, FORM1*20, NNTEMP*18,
     *   AXIS(2)*1, CHSGN1*1, CHSGN2*1, CHSGN4*1, NTEMP*12, CTEMP*6,
     *   BTEMP*8, STOKEV*4, STRING*50, XTEMP*8
      INTEGER   I, J, NAX, INC, HM(2), XST, IST, I1, I2, I3, J1, J2,
     *   IPIX(50), IROUND, IN, IDM(2), IRM(2), JERR(7), J4, ITMP, NC
      REAL      TEMP, DEC, SEC, AREA, RTEMP, SECD, SECR, RDX, RDY
      DOUBLE PRECISION DX, DY, X(7), DT, COSR, SINR, FREQV, XT
      LOGICAL   FLUXFL, DONEIT, NOSWAP, LONG
      INCLUDE 'PRTIM.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA BLANK, NOSWAP /'    ', .FALSE./
      DATA WPROC /'Dirty   ','Compnts ','Residual','Points  ',
     *   'Clean   '/
      DATA CHSTOK /'Beam','Ipol','Qpol','Upol','Vpol','Ppol','Fpol',
     *   'Pang','Spix','Optd','Rotm','????','RR','LL','RL','LR',
     *   'XX','YY','XY','YX'/
      DATA AXIS /'X','Y'/
C-----------------------------------------------------------------------
      LONG = .NOT.SHORT
C                                       force page: do we continue?
      IERR = 0
      LINPRT = 998
      TITL1 = ' '
      TITL2 = ' '
C                                       source, img name, page
      CALL H2CHR (18, KHIMNO, CATH(KHIMN), NNTEMP)
      IF ((DOCRT.GT.-2.5) .AND. (FIRST)) THEN
         CALL H2CHR (8, 1, CATH(KHOBJ), BTEMP)
         CALL H2CHR (12, KHIMNO, CATH(KHIMN), NTEMP)
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CTEMP)
         IF (LONG) THEN
            WRITE (ALINE,1000) BTEMP, NTEMP, CTEMP, CATBLK(KIIMS)
         ELSE
            WRITE (ALINE,1001) BTEMP, NTEMP, CTEMP, CATBLK(KIIMS)
            END IF
         CALL PRTLIN (LUNPRT, INDPRT, DOCRT, LINMAX, TITL1, TITL2,
     *      ALINE, LINPRT, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       set up axes loop
      NAX = CATBLK(KIDIM)
      INC = 2
      I1 = 0
      I2 = 0
      I3 = 0
      STOKEV = BLANK
      FREQV = 0.D0
      SECR = 0.0
      SECD = 0.0
      CHSGN2 = ' '
      CALL FILL (2, 0, IRM)
      CHSGN4 = ' '
      CALL FILL (2, 0, IDM)
      LOCNUM = 1
      CALL SETLOC (DEPTH(3), NOSWAP)
      RDX = DEPTH(1)
      RDY = DEPTH(2)
      CALL FILL (7, 0, JERR)
      CALL XYVAL (RDX, RDY, X(1), X(2), XT, JERR(1))
      JERR(2) = JERR(1)
      IF (KLOCA(LOCNUM).GE.0) THEN
         X(KLOCA(LOCNUM)+1) = XT
         JERR(KLOCA(LOCNUM)+1) = JERR(1)
         END IF
      IF (NAX.GE.3) THEN
         DONEIT = AXTYP(LOCNUM).NE.4
         DO 10 I = 3,NAX
            J = I - 1
            IF (((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) .AND.
     *         (KLOCA(LOCNUM).EQ.J)) GO TO 10
            IF (AXFUNC(I,LOCNUM).LE.0) THEN
               X(I) = CATD(KDCRV+J) + (DEPTH(I) - CATR(KRCRP+J)) *
     *            CATR(KRCIC+J)
            ELSE IF (AXFUNC(I,LOCNUM).EQ.1) THEN
               X(I) = DEPTH(I) - CATR(KRCRP+J)
               X(I) = CATD(KDCRV+J) + X(I) * CATR(KRCIC+J) /
     *            (1.D0 + AXDENU(LOCNUM)*X(I))
            ELSE IF (AXTYP(LOCNUM).NE.4) THEN
               X(I) = CATD(KDCRV+J) + (DEPTH(I) - CATR(KRCRP+J)) *
     *            CATR(KRCIC+J)
            ELSE IF (.NOT.DONEIT) THEN
               DONEIT = .TRUE.
               DX = (ZDEPTH(KLOCL(LOCNUM)-1,LOCNUM) - RPLOC(3,LOCNUM)) *
     *            AXINC(3,LOCNUM)
               DY = (ZDEPTH(KLOCM(LOCNUM)-1,LOCNUM) - RPLOC(4,LOCNUM)) *
     *            AXINC(4,LOCNUM)
               COSR = COS (ROT(LOCNUM) * COND2R)
               SINR = SGNROT(LOCNUM) * SIN (ROT(LOCNUM) * COND2R)
               DT = (DX * COSR - DY * SINR) * COND2R
               DY = (DY * COSR + DX * SINR) * COND2R
               DX = DT
               COSR = RPVAL(3,LOCNUM) * COND2R
               SINR = RPVAL(4,LOCNUM) * COND2R
               CALL NEWPOS (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM), COSR, SINR,
     *            DX, DY, X(KLOCL(LOCNUM)+1), X(KLOCM(LOCNUM)+1), IN,
     *            JERR(KLOCL(LOCNUM)+1))
               JERR(KLOCM(LOCNUM)+1) = JERR(KLOCL(LOCNUM)+1)
               X(KLOCL(LOCNUM)+1) = X(KLOCL(LOCNUM)+1) / COND2R
               X(KLOCM(LOCNUM)+1) = X(KLOCM(LOCNUM)+1) / COND2R
               END IF
 10         CONTINUE
         END IF
C                                       Loop to display axes
      DO 50 I = 1,NAX
         J = I - 1
         ITMP = KHCTP+J*INC
         CALL H2CHR (8, 1, CATH(ITMP), BTEMP)
         CTEMP = BTEMP
         CALL CHLTOU (8, XTEMP)
         IF (XTEMP(1:4).EQ.'FREQ') I3 = I
C                                       Check axis type
         IF ((JERR(I).EQ.0) .AND. ((XTEMP(:2).EQ.'LL') .OR.
     *      (XTEMP(:2).EQ.'RA') .OR. (XTEMP(:2).EQ.'MM') .OR.
     *      (XTEMP(:3).EQ.'DEC'))) THEN
C                                       RA axis
            IF ((XTEMP(:2).EQ.'LL') .OR. (XTEMP(:2).EQ.'RA')) THEN
               CALL COORDD (1, X(I), CHSGN1, HM, SEC)
               CALL COORDD (1, X(I), CHSGN2, IRM, SECR)
               I1 = I
C                                       DEC axis
            ELSE
               CALL COORDD (2, X(I), CHSGN1, HM, SEC)
               CALL COORDD (2, X(I), CHSGN4, IDM, SECD)
               I2 = I
               END IF
            DEC = CATR(KRCIC+J) * 3600.
            IF ((DOCRT.GT.-2.5) .AND. (FIRST)) THEN
               IF (LONG) THEN
                  IF (I.LE.2) THEN
                     WRITE (ALINE,1010) AXIS(I), BTEMP, CHSGN1, HM, SEC,
     *                  DEPTH(I), DEC, CATR(KRCRT+J)
                  ELSE
                     WRITE (ALINE,1011) I, BTEMP, CHSGN1, HM, SEC,
     *                  DEPTH(I), CATR(KRCRT+J)
                     END IF
               ELSE
                  IF (I.LE.2) THEN
                     WRITE (ALINE,1015) AXIS(I), BTEMP, CHSGN1, HM, SEC,
     *                  DEPTH(I), DEC, CATR(KRCRT+J)
                  ELSE
                     WRITE (ALINE,1016) I, BTEMP, CHSGN1, HM, SEC,
     *                  DEPTH(I), CATR(KRCRT+J)
                     END IF
                  END IF
               END IF
C                                       Stokes axis
         ELSE IF (J.EQ.KLOCS(LOCNUM)) THEN
            RTEMP = X(I)
            J1 = IROUND (RTEMP) + 1
            J2 = J1
            IF ((J1.LE.0) .OR. (J1.GT.11)) J2 = 12
            IF ((J1.LT.0) .AND. (J1.GE.-7)) J2 = 13 - J1
            STOKEV = CHSTOK(J2)
            IF ((DOCRT.GT.-2.5) .AND. (FIRST)) THEN
               IF (LONG) THEN
                  IF (I.LE.2) THEN
                     WRITE (ALINE,1020) AXIS(I), BTEMP, STOKEV,
     *                  DEPTH(I), CATR(KRCIC+J), CATR(KRCRT+J)
                  ELSE
                     WRITE (ALINE,1021) I, BTEMP, STOKEV, DEPTH(I),
     *                  CATR(KRCRT+J)
                     END IF
               ELSE
                  IF (I.LE.2) THEN
                     WRITE (ALINE,1025) AXIS(I), BTEMP, STOKEV,
     *                  DEPTH(I), CATR(KRCIC+J), CATR(KRCRT+J)
                  ELSE
                     WRITE (ALINE,1026) I, BTEMP, STOKEV, DEPTH(I),
     *                  CATR(KRCRT+J)
                     END IF
                  END IF
               END IF
C                                       No RA or DEC axis
         ELSE
            IF (J.EQ.KLOCF(LOCNUM)) FREQV = X(I)
            IF ((DOCRT.GT.-2.5) .AND. (FIRST)) THEN
               IF (LONG) THEN
                  IF (I.LE.2) THEN
                     WRITE (ALINE,1030) AXIS(I), BTEMP, X(I), DEPTH(I),
     *                  CATR(KRCIC+J), CATR(KRCRT+J)
                  ELSE
                     WRITE (ALINE,1031) I, BTEMP, X(I), DEPTH(I),
     *                  CATR(KRCRT+J)
                     END IF
               ELSE
                  IF (I.LE.2) THEN
                     WRITE (ALINE,1035) AXIS(I), BTEMP, X(I), DEPTH(I),
     *                  CATR(KRCIC+J), CATR(KRCRT+J)
                  ELSE
                     WRITE (ALINE,1036) I, BTEMP, X(I), DEPTH(I),
     *                  CATR(KRCRT+J)
                     END IF
                  END IF
               END IF
            END IF
         IF ((DOCRT.GT.-2.5) .AND. (FIRST)) THEN
            CALL PRTLIN (LUNPRT, INDPRT, DOCRT, LINMAX, TITL1, TITL2,
     *         ALINE, LINPRT, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 50      CONTINUE
      FLUXFL = (I1.GT.0) .AND. (I2.GT.0)
C                                       clean parameters
      IF ((CATBLK(KINIT).GT.0) .AND. (DOCRT.GT.-2.5) .AND. (FIRST)) THEN
         TEMP = CATR(KRBMJ) * 3600.0
         SEC = CATR(KRBMN) * 3600.0
         DEC = CATR(KRBPA)
         J = CATBLK(KITYP)
         IF ((J.LT.1) .OR. (J.GT.4)) J = 1
         IF ((J.EQ.1) .AND. (CATBLK(KINIT).GT.0)) J = 5
         IF (LONG) THEN
            IF ((TEMP.GT.0.5) .OR. (SEC.GT.0.5)) THEN
               WRITE (ALINE,1050) CATBLK(KINIT), TEMP, SEC, DEC,
     *            WPROC(J)
            ELSE
               WRITE (ALINE,1051) CATBLK(KINIT), TEMP, SEC, DEC,
     *           WPROC(J)
               END IF
         ELSE
            IF ((TEMP.GT.0.5) .OR. (SEC.GT.0.5)) THEN
               WRITE (ALINE,1055) CATBLK(KINIT), TEMP, SEC, DEC,
     *            WPROC(J)
            ELSE
               WRITE (ALINE,1056) CATBLK(KINIT), TEMP, SEC, DEC,
     *            WPROC(J)
               END IF
            END IF
         CALL PRTLIN (LUNPRT, INDPRT, DOCRT, LINMAX, TITL1, TITL2,
     *      ALINE, LINPRT, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       print scaling info
      CALL H2CHR (8, 1, CATH(KHBUN), BTEMP)
      XTEMP = BTEMP
      CALL CHLTOU (8, XTEMP)
      J4 = 10.0**NDIG + 0.1
      FLUXFL = (FLUXFL) .AND. (XTEMP(1:8).EQ.'JY/BEAM ')
      IF (LONG) THEN
         WRITE (ALINE,1060) J4, DSEC, BTEMP
      ELSE
         WRITE (ALINE,1065) J4, DSEC, BTEMP
         END IF
      IF ((DOCRT.GT.-2.5) .OR. (FIRST)) THEN
         CALL PRTLIN (LUNPRT, INDPRT, DOCRT, LINMAX, TITL1, TITL2,
     *      ALINE, LINPRT, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Clean beam area
      AREA = 1.1331 * CATR(KRBMJ) * CATR(KRBMN) * 3600.0 * 3600.0
      IF ((FLUXFL) .AND. (AREA.GT.0.0) .AND. (DOCRT.GT.-2.5) .AND.
     *   (FIRST)) THEN
         SEC = DSEC / AREA
         IF (LONG) THEN
            WRITE (ALINE,1070) J4, SEC
         ELSE
            WRITE (ALINE,1075) J4, SEC
            END IF
         CALL PRTLIN (LUNPRT, INDPRT, DOCRT, LINMAX, TITL1, TITL2,
     *      ALINE, LINPRT, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 999
         IF ((I3.GT.0) .AND. (X(I3).GT.1.E3)) THEN
            X(I3) = X(I3) / 1.D9
            SEC = 1.3849E6 * SEC / (X(I3) * X(I3))
            IF (LONG) THEN
               WRITE (ALINE,1080) J4, SEC
            ELSE
               WRITE (ALINE,1085) J4, SEC
               END IF
            CALL PRTLIN (LUNPRT, INDPRT, DOCRT, LINMAX, TITL1, TITL2,
     *         ALINE, LINPRT, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
C                                       Second title line:
C                                       Print x pixel positions: every
C                                       fifth one divisible by 5 if ?
      XST = WIND(1) - XINC
      DO 110 I = 1,5
         XST = XST + XINC
         IF (MOD(XST,5).EQ.0) GO TO 115
 110     CONTINUE
      XST = WIND(1)
      I = 1
C                                       XST: first pix value
C                                       IST: first pixel loc
 115  IST = I
      I2 = 5 * XINC
      I1 = XST - I2
      DO 120 I = 1,50
         IPIX(I) = I1 + I2*I
         IF (IPIX(I).GT.WIND(3)) GO TO 125
 120     CONTINUE
      I = 51
 125  I1 = I - 1
C                                       Format (J1X, 255(I4,J2X))
      J1 = 2 + IST*NDIG
      J2 = 5*NDIG - 4
      I = I1 - 1
      WRITE (FORM1,1125) J1, I, J2
      IF (I.LE.0) WRITE (FORM1,1126) J1
      WRITE (TITL2,FORM1) (IPIX(I), I = 1,I1)
C                                       First title line
      CALL NAMEST (NNTEMP, CATBLK(KIIMS), STRING, NC)
      I = 1
      LONG = (LINMAX - NC - 6).GE.70
      IF (LONG) THEN
         J1 = (LINMAX - NC - 68) / 4
         TITL1 = 'Image=' // STRING(:NC)
         I = NC + 7
      ELSE
         J1 = (LINMAX - NC - 43) / 4
         TITL1 = STRING(:NC)
         I = NC + 1
         END IF
      J1 = MIN (5, J1)
      I = I + J1
      WRITE (STRING,1130) CHSGN2, IRM, SECR, CHSGN4, IDM, SECD, FREQV,
     *   STOKEV
      IF (STRING(8:8).EQ.' ') STRING(8:8) = '0'
      IF (STRING(22:22).EQ.' ') STRING(22:22) = '0'
      IF (LONG) THEN
         TITL1(I:) = 'RA=' // STRING(1:13) // BLANK(:J1) // 'DEC=' //
     *      STRING(15:26) // BLANK(:J1) // 'Freq=' // STRING(28:41) //
     *      BLANK(:J1) // 'Stokes=' // STRING(43:46)
      ELSE
         TITL1(I:) = STRING(1:13) // BLANK(:J1) // STRING(15:26) //
     *      BLANK(:J1) // STRING(28:41) // BLANK(:J1) // STRING(43:46)
         END IF
      IF ((DOCRT.GT.-2.5) .OR. (FIRST)) THEN
         CALL PRTLIN (LUNPRT, INDPRT, DOCRT, LINMAX, TITL1, TITL2,
     *      TITL1, LINPRT, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      CALL PRTLIN (LUNPRT, INDPRT, DOCRT, LINMAX, TITL1, TITL2,
     *   TITL2, LINPRT, IPAGE, SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (16X,'Object=',A8,8X,'Imagename=',A,'.',A,'.',I4)
 1001 FORMAT ('Object=',A8,6X,'Imagename=',A,'.',A,'.',I4)
 1010 FORMAT (9X,A1,' Axis',4X,'Type=',A8,5X,'Ref Value=',A1,I2,I3,
     *   F6.2,3X,'at pixel',I5,5X,'Inc=',F9.3,5X,'rot=',F6.1)
 1011 FORMAT (9X,'  Axis',I2,2X,'Type=',A8,5X,'Ref Value=',A1,I2,I3,
     *   F6.2,3X,'at pixel',I5,5X,'rot=',F6.1)
 1015 FORMAT (A1,' Axis',3X,A8,2X,'Val=',A1,I2,I3,F6.2,' at',I5,2X,
     *   'Inc=',F9.3,3X,'rot=',0PF6.1)
 1016 FORMAT ('  Axis',I2,1X,A8,2X,'Val=',A1,I2,I3,F6.2,' at',I5,2X,
     *   'rot=',0PF6.1)
 1020 FORMAT (9X,A1,' Axis',4X,'Type=',A8,9X,'Value= ',A4,10X,
     *   'at pixel',I5,5X,'Inc=',1PE11.4,3X,'rot=',0PF6.1)
 1021 FORMAT (9X,'  Axis',I2,2X,'Type=',A8,9X,'Value= ',A4,10X,
     *   'at pixel',I5,5X,'rot=',0PF6.1)
 1025 FORMAT (A1,' Axis',3X,A8,2X,'Val=',A4,8X,' at',I5,2X,
     *   'Inc=',1PE10.3,2X,'rot=',0PF6.1)
 1026 FORMAT ('  Axis',I2,1X,A8,2X,'Val=',A4,8X,' at',I5,2X,
     *   'rot=',0PF6.1)
 1030 FORMAT (9X,A1,' Axis',4X,'Type=',A8,5X,'Ref value=',1PE13.5,
     *   2X,'at pixel',I5,5X,'inc=',1PE11.4,3X,'rot=',0PF6.1)
 1031 FORMAT (9X,'  Axis',I2,2X,'Type=',A8,9X,'Value=',1PE13.5,2X,
     *   'at pixel',I5,5X,'rot=',0PF6.1)
 1035 FORMAT (A1,' Axis',3X,A8,2X,'Val=',1PE12.5,' at',I5,2X,
     *   'INC=',1PE10.3,2X,'rot=',0PF6.1)
 1036 FORMAT ('  Axis',I2,1X,A8,2X,'Val=',1PE12.5,' at',I5,2X,
     *   'rot=',0PF6.1)
 1050 FORMAT (19X,'CLEAN nit=',I8,6X,'BMAJ=',F5.2,6X,'BMIN=',F5.2,
     *   6X,'BPA=',F6.1,10X,'Type=',A8)
 1051 FORMAT (19X,'CLEAN nit=',I8,6X,'BMAJ=',F6.4,5X,'BMIN=',F6.4,
     *   5X,'BPA=',F6.1,10X,'Type=',A8)
 1055 FORMAT ('CLEAN nit=',I8,3X,'BMAJ=',F5.2,2X,'BMIN=',F5.2,
     *   2X,'BPA=',F6.1,3X,'Type=',A8)
 1056 FORMAT ('CLEAN nit=',I8,3X,'BMAJ=',F6.4,1X,'BMIN=',F6.4,
     *   1X,'BPA=',F6.1,3X,'Type=',A8)
 1060 FORMAT (37X,'Map scale:',I10,'=',1PE12.4,1X,A8)
 1065 FORMAT ('   Map scale:',I10,'=',1PE12.4,1X,A8)
 1070 FORMAT (34X,'Approx scale:',I10,'=',1PE12.4,' Jy / (arcsec)**2')
 1075 FORMAT ('Approx scale:',I10,'=',1PE12.4,' Jy / (arcsec)**2')
 1080 FORMAT (34X,'Approx scale:',I10,'=',1PE12.4,' Kelvins')
 1085 FORMAT ('Approx scale:',I10,'=',1PE12.4,' Kelvins')
 1125 FORMAT ('(',I2,'X,I4,',I2,'(',I2,'X,I4))')
 1126 FORMAT ('(',I2,'X,I4)')
 1130 FORMAT (A1,I2.2,':',I2.2,':',F6.3,1X,A1,I2.2,':',I2.2,':',F5.2,
     *   1X,1PE14.6,1X,A4)
      END
      SUBROUTINE PLINE (LINE, IPRN, NPL, NDIG, PBLANK, IERR)
C-----------------------------------------------------------------------
C   PLINE writes a map line, suitably scaled, on the line printer or
C   CRT for PRTIM.
C   Inputs:
C      LINE     I      Pixel value of the line
C      NPL      I      Number of pixel per written line
C      NDIG     I      Number of digits in the display
C      PBLANK   I      Value of blanked pixel
C   In/out:
C      IPRN     I(*)   Array of scaled map values (used for scrtch)
C   Output:
C      IERR     I      Error code: 0 -> ok,
C                         -1 -> user wants to quit,
C                         else error
C-----------------------------------------------------------------------
      INTEGER   LINE, IPRN(140), NPL, NDIG, PBLANK, IERR
C
      CHARACTER SYM(23)*1, IDIS*1, BDIS(3)*8, LPRN*140, FORM3*8,
     *   PRTMP*12
      INTEGER   IMAX, IMIN, ITEMP, J, I, IX
      INCLUDE 'PRTIM.INC'
      DATA SYM /'-','J','I','H','G','F','E','D','C','B','A',
     *   '.','1','2','3','4','5','6','7','8','9','T','+'/
      DATA IDIS, BDIS /':', '::::::::','++++++++','--------'/
C-----------------------------------------------------------------------
C                                       Special print out for 1 digit
      IF (NDIG.EQ.1) THEN
         DO 30 I = 1,NPL
            ITEMP = IPRN(I)
C                                       Is this a blank pixel
            IF (ITEMP.EQ.PBLANK) THEN
               LPRN(I:I) = IDIS
            ELSE
               ITEMP = MAX (-11, MIN (11,  ITEMP))
               LPRN(I:I) = SYM(ITEMP+12)
               END IF
 30         CONTINUE
         WRITE (ALINE,1030) LINE, LPRN(:NPL)
C                                       multi-digit displays
      ELSE
         IMAX = 10.0**NDIG - 0.5
         IMIN = -IMAX / 10
         WRITE (FORM3,1050) NDIG
         WRITE (ALINE,1051) LINE
         J = 7 - NDIG
         DO 70 I = 1,NPL
            J = J + NDIG
            ITEMP = IPRN(I)
            IX = 0
            IF (ITEMP.GT.IMAX) IX = 2
            IF (ITEMP.LT.IMIN) IX = 3
            IF (ITEMP.EQ.PBLANK) IX = 1
C                                       good pixel
            IF (IX.EQ.0) THEN
               WRITE (PRTMP,FORM3) ITEMP
               ALINE(J:J+NDIG-1) = PRTMP(:NDIG)
C                                       Blanked or overflow pixel
            ELSE
               ALINE(J:J+NDIG-1) = BDIS(IX)(:NDIG)
               END IF
 70         CONTINUE
         END IF
C                                       Write it
      CALL PRTLIN (LUNPRT, INDPRT, DOCRT, LINMAX, TITL1, TITL2,
     *   ALINE, LINPRT, IPAGE, SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (I4,2X,A)
 1050 FORMAT ('(I',I1,')')
 1051 FORMAT (I4)
      END
