LOCAL INCLUDE 'TAPLT.INC'
C                                       Include for TAPLT.
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMIN(3), XCLAIN(2), XINEXT, XKEYST(4)
      REAL      USERID, XSIN, XDISIN, XVER, BCOUNT, ECOUNT, XINC,
     *   DOHIST, APARM(10), BPARM(10), CPARM(10), XARR(30), DOINV,
     *   XLABEL, XDOTV, XGRCH
      CHARACTER NAMEIN*12, CLAIN*6, INEXT*2, CTVAL*16
      DOUBLE PRECISION  RSUM(2), DEG2R
      REAL      XYSCL(2), XYOFF(2), SUM(400), SUM2(400), RTVAL(7),
     *   ETVAL(7)
      INTEGER   SBUF(256), TABUF(512), NKEY, NCOL, NREC,
     *   RECI(XBPRSZ), IBCNT, IECNT, INC, BINCNT(400), SEQIN, DISKIN,
     *   TALUN, TYPEAX(2,2), EVER, PVER, TESTEM(2), CNO, NPARM,
     *   DTYPAX(2,2), ISUBAX(2,2), TAIND, OPTYPE(2), GRCHN, TVCHN,
     *   TVCORN(4), LABEL, LROW, NBIN, NTVAL, ITVAL(4,7), DATP(128,2)
      LOGICAL   SCALEM(2), SUMEM(2), TWOOF(2), ABSEM(2,2), DOTV
      COMMON /INPARM/ USERID, XNAMIN, XCLAIN, XSIN, XDISIN, XINEXT,
     *   XVER, BCOUNT, ECOUNT, XINC, DOHIST, APARM, BPARM, CPARM,
     *   XARR, XKEYST, DOINV, XLABEL, XDOTV, XGRCH
      COMMON /CHPARM/ NAMEIN, CLAIN, INEXT, CTVAL
      COMMON /BUFRS/ RSUM, DEG2R, RECI, SBUF, DATP, TABUF, NKEY,
     *   NCOL, NREC, XYSCL, XYOFF, SUM, SUM2, IBCNT, IECNT, INC, BINCNT,
     *   SCALEM, SUMEM, TWOOF, ABSEM, SEQIN, DISKIN, TALUN, TYPEAX,
     *   EVER, PVER, TESTEM, CNO, NPARM, DTYPAX, ISUBAX, TAIND, OPTYPE,
     *   GRCHN, TVCHN, TVCORN, DOTV, LABEL, LROW, NBIN, RTVAL, ETVAL,
     *   NTVAL, ITVAL
LOCAL END
       PROGRAM TAPLT
C-----------------------------------------------------------------------
C!  Task plots table extension data.
C#  EXT-util EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2002, 2005, 2007, 2009, 2012, 2014-2015
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
C   TAPLT plots table extension data . A 'PL' extension file is made
C   which can be displayed in the usual ways.
C   Inputs:
C     USERID                       file owner # ignored
C     INNAME         NAMEIN        Name of main input file
C     INCLASS        CLAIN         Class of main input file
C     INSEQ          SEQIN         Seq. of main input file
C     INDISK         DISKIN        Disk number of main input file.
C     CHANNEL        NCH           Channel #: 0 or 1 ok for cont.
C     INEXT......Type of input table extension.  '  ' = 'CC'
C     INVERS.....Version number of table extension.  0 => highest.
C     BCOUNT.....Beginning row number to be included in plot. 0 = 1.
C     ECOUNT.....Ending row number included in plot.  0 = highest.
C     XINC.......Increment in row number between rows included in
C                plot.    0 => 1.
C     DOHIST.....> 0 => plot as histogram in the X-axis parameter.
C                <= 0 plot two columns against each other.
C     APARM......Column selection parameters:
C        1 = X-axis logical column number A:  0 => row number.
C            < 0 => use absolute value of col. abs(APARM(1))
C        2 = The subscript of the X-axis column A data to use if the
C            column is an array.  0 => 1.
C        3 = X-axis logical column number B:  0 => row number.
C            < 0 => use absolute value of col. abs(APARM(1))
C        4 = The subscript of the X-axis column B data to use if the
C            column is an array.  0 => 1.
C        5 = X-axis function number: Various functions may be
C            performed on column A only or on a pair of columns A
C            and B before plotting.  Functions which use column A
C            only are 0 = no function, 1 = sum, 2 = asin, 3 = Log10,
C            4 = ln, 5 = exp, 6 = sin, 7 = cos, 8 = tan, 9 = atan.
C            In these cases, APARM(3) and (4) are ignored.
C            Functions which use both columns are 10 = +, 11 = -,
C            12 = *, 13 = /, 14 = **, 15 = mod, 16 = Modulus,
C            17 = atan2, 18 = max, 19 = min.
C        6 = Y-axis logical column number A:  as for X-axis.
C        7 = The subscript of the Y-axis column A data to use if the
C            column is an array.  0 => 1.
C        8 = Y-axis logical column number B:  as for X-axis.
C        9 = The subscript of the Y-axis column B data to use if the
C            column is an array.  0 => 1.
C        10 = Y-axis function number: as for X-axis.
C     BPARM......Plot control parameters:
C        1 = If DOHIST > 0, the number of bins in the histogram
C            (0 => 50).  If DOHIST <= 0 and BPARM(1) > 0, the plot
C            will be of bin averages in X of the specified quantity.
C            There will be BPARM(1) number of bins in the plot.  For
C            bins with more than 2 samples the vertical height of
C            the symbol represents the standard deviation of the
C            mean of the distribution in the bin.  In this case, 0
C            means to plot all points rather than the binned
C            average.
C        2 = If DOHIST <= 0 and BPARM(1) > 0 and BPARM(2) > 0, the
C            bin averages will be printed in the message file at
C            message level 4.
C        3 = If greater than zero, use BPARM(4) - BPARM(7) as
C            the ranges of the axes.  If less than zero, use the
C            BPARMs to limit the range of the axes, but self-
C            scale the axes within that range.  If 0.0, fully
C            self-scaling.
C        4 = Minimum of X-axis.
C        5 = Maximum of X-axis (if = BPARM(4) do self-scale in X).
C        6 = Minimum of Y-axis.
C        7 = Maximum of Y-axis (if = BPARM(6) do self-scale in Y).
C        9 = The exponent to which to take the X-axis result value
C            before plotting.   0 => 1.
C        10 = The exponent to which to take the Y-axis result value
C            before plotting.   0 => 1.
C     CPARM......After the value or absolute value of a column is
C             found, it may be scaled and offset before other
C             functions are applied to it.
C        1 = X-axis logical column number A scale factor: 0 -> 1.
C        2 = X-axis logical column number A offset.
C        3 = X-axis logical column number B scale factor: 0 -> 1.
C        4 = X-axis logical column number B offset.
C        5 = Y-axis logical column number A scale factor: 0 -> 1.
C        6 = Y-axis logical column number A offset.
C        7 = Y-axis logical column number B scale factor: 0 -> 1.
C        8 = Y-axis logical column number B offset.
C     LTYPE.......Labeling type
C     DOTV........> 0 => plot directly on the TV device, otherwise
C                 make a plot file for later display on one or
C                 more devices (including the TV if desired).
C     GRCHAN......Graphics channel (1 - 3) to use for line drawing.
C                 0 => 1.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IERR, IRET
      INCLUDE 'TAPLT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'TAPLT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL TAPIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       determine scaling
      IF ((SCALEM(1)) .OR. ((SCALEM(2)) .AND. (NBIN.EQ.0))) THEN
         CALL TASCAL (IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       do histogram
      IF (DOHIST.GT.0.0) THEN
         CALL TAHIST (IRET)
         IF (IRET.NE.0) GO TO 990
      ELSE IF ((NBIN.NE.0) .AND. (SCALEM(2))) THEN
         CALL TABSCL (IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       Do plot
      CALL PLOTAB (IRET)
C                                       Clear catlg on error
 990  IF ((IRET.EQ.0) .OR. (NCFILE.LT.1) .OR. (FRW(1).GT.0) .OR.
     *   (DOTV)) GO TO 995
         CALL DELEXT ('PL', FVOL(1), FCNO(1), 'READ', CATBLK, SBUF,
     *      PVER, IERR)
         NCFILE = NCFILE - 1
C                                       Close down
 995  CALL DIE (IRET, SBUF)
C
 999  STOP
      END
      SUBROUTINE TAPIN (PRGM, JERR)
C-----------------------------------------------------------------------
C   TAPIN gets input parameters for TAPLT .
C   Inputs:  PRGM   C*6)      Program name
C   Output:  JERR   I         Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   JERR
C
      CHARACTER INTYPE*2, STAT*4, CHAX(2)*2
      INTEGER   IUSER, I, IERR, IROUND, IO, ID1, ID2, J, K, JTRIM, I4T,
     *   LTYPE
      LOGICAL   T
      INCLUDE 'TAPLT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, CHAX /.TRUE., 'X-','Y-'/
C-----------------------------------------------------------------------
C                                       Init IO et al.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      DEG2R = ATAN(1.0D0) / 45.0D0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
      PVER = 10000
C                                       Get input parameters.
      NPARM = 82
      CALL GTPARM (PRGM, NPARM, RQUICK, USERID, SBUF, IERR)
      IF (IERR.EQ.0) GO TO 10
         JERR = 8
         RQUICK = .TRUE.
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, SBUF, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 0
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMIN, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (2, 1, XINEXT, INEXT)
C                                       Crunch input parameters.
      USERID = NLUSER
      IUSER = NLUSER
      IF (XINC.LT.1.0) XINC = 1.0
      INC = IROUND (XINC)
      IF (INC.LE.0) INC = 1
      XINC = INC
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
      LABEL = IROUND (XLABEL)
      LTYPE = MOD (ABS(LABEL), 100)
      IF ((LTYPE.EQ.0) .OR. (LTYPE.GT.10)) LTYPE = 3
      IF (LTYPE.GT.7) LTYPE = 7
      IF ((LTYPE.GE.4) .AND. (LTYPE.LE.6)) LTYPE = 3
      IF (LABEL.GE.0) THEN
         LABEL = (LABEL/100)*100 + LTYPE
      ELSE
         LABEL = (LABEL/100)*100 - LTYPE
         END IF
C                                       Get CATBLK from file.
      CNO = 1
      INTYPE = ' '
      CALL CATDIR ('SRCH', DISKIN, CNO, NAMEIN, CLAIN, SEQIN, INTYPE,
     *   IUSER, STAT, SBUF, IERR)
      IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IERR, NAMEIN, CLAIN, SEQIN, DISKIN, IUSER
         GO TO 990
 20   STAT = 'WRIT'
      IF (DOTV) STAT = 'READ'
      CALL CATIO ('READ', DISKIN, CNO, CATBLK, STAT, SBUF, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IERR
         GO TO 990
 30   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 1
      XSIN = SEQIN
      XDISIN = DISKIN
C                                       Find extension file
      IF (INEXT.EQ.'  ') INEXT = 'CC'
      EVER = IROUND (XVER)
      IF (INEXT.EQ.INTYPE) EVER = 1
      TALUN = 28
      NKEY = 0
      NCOL = 0
      NREC = 0
      LROW = 0
      CALL TABINI ('READ', INEXT, DISKIN, CNO, EVER, CATBLK, TALUN,
     *   NKEY, NREC, NCOL, DATP, TABUF, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IERR, INEXT, EVER
         GO TO 990
C                                       Set/check parms
 40   IECNT = TABUF(5)
      TAIND = TABUF(82)
      IF ((ECOUNT.GE.1.0) .AND. (ECOUNT.LT.IECNT)) IECNT = ECOUNT + 0.1
      IBCNT = BCOUNT + 0.1
      IF (IBCNT.LE.0) IBCNT = 1
      ECOUNT = IECNT
      BCOUNT = IBCNT
      XVER = EVER
      IF (IBCNT+INC.LE.IECNT) GO TO 45
         WRITE (MSGTXT,1040) IBCNT, IECNT, INC
         GO TO 990
 45   SUMEM(2) = .FALSE.
      DO 60 I = 1,2
         IO = (I-1)*5
         OPTYPE(I) = IROUND (APARM(IO+5))
         IF (OPTYPE(I).LT.0) OPTYPE(I) = 0
         IF ((I.EQ.2) .AND. (DOHIST.GT.0.0)) GO TO 60
         TWOOF(I) = OPTYPE(I).GT.9
         TYPEAX(1,I) = IROUND (APARM(IO+1))
         TYPEAX(2,I) = 0
         IF (TWOOF(I)) TYPEAX(2,I) = IROUND (APARM(IO+3))
         IF ((TYPEAX(1,I).GE.-NCOL) .AND. (TYPEAX(1,I).LE.NCOL) .AND.
     *      (TYPEAX(2,I).GE.-NCOL) .AND. (TYPEAX(2,I).LE.NCOL)) GO TO 50
            WRITE (MSGTXT,1045) CHAX(I), TYPEAX(1,I), TYPEAX(2,I)
            GO TO 990
 50      SUMEM(I) = OPTYPE(I).EQ.1
         ABSEM(1,I) = TYPEAX(1,I).LT.0
         ABSEM(2,I) = TYPEAX(2,I).LT.0
         TYPEAX(1,I) = ABS(TYPEAX(1,I))
         TYPEAX(2,I) = ABS(TYPEAX(2,I))
         DTYPAX(1,I) = 0
         DTYPAX(2,I) = 0
         ISUBAX(1,I) = APARM(IO+2) + 0.1
         ISUBAX(2,I) = 1
         IF (TWOOF(I)) ISUBAX(2,I) = APARM(IO+4) + 0.1
         IF (TYPEAX(1,I).GT.0) DTYPAX(1,I) = DATP(TYPEAX(1,I),2)
         IF (TYPEAX(2,I).GT.0) DTYPAX(2,I) = DATP(TYPEAX(2,I),2)
         ID1 = DTYPAX(1,I) / 10
         ID2 = DTYPAX(2,I) / 10
         IF (ID1.LT.1) ID1 = 1
         IF (ID2.LT.1) ID2 = 1
         DTYPAX(1,I) = MOD (DTYPAX(1,I), 10)
         DTYPAX(2,I) = MOD (DTYPAX(2,I), 10)
         IF ((ISUBAX(1,I).LE.0) .OR. (ID1.EQ.1)) ISUBAX(1,I) = 1
         IF ((ISUBAX(2,I).LE.0) .OR. (ID2.EQ.1)) ISUBAX(2,I) = 1
         IF ((DTYPAX(1,I).NE.3) .AND. (DTYPAX(1,I).NE.5) .AND.
     *      (DTYPAX(1,I).NE.7) .AND. (DTYPAX(2,I).NE.3) .AND.
     *      (DTYPAX(2,I).NE.5) .AND. (DTYPAX(2,I).NE.7)) GO TO 55
            WRITE (MSGTXT,1050) CHAX(I)
            GO TO 990
 55      IF ((ISUBAX(1,I).LE.ID1) .AND. (ISUBAX(2,I).LE.ID2)) GO TO 60
            WRITE (MSGTXT,1055) CHAX(I), ISUBAX(1,I), ISUBAX(2,I),
     *         ID1, ID2
            GO TO 990
 60      CONTINUE
      IF (DOHIST.GT.0.) ABSEM(1,2) = .FALSE.
      IF (DOHIST.GT.0.) ABSEM(2,2) = .FALSE.
      IF ((DOHIST.GT.0.) .AND. ((OPTYPE(2).GT.4) .OR. (OPTYPE(2).LE.2)))
     *   OPTYPE(2) = 0
      IF (OPTYPE(1).GT.19) OPTYPE(1) = 0
      IF (OPTYPE(2).GT.19) OPTYPE(2) = 0
      APARM(5) = OPTYPE(1)
      APARM(10) = OPTYPE(2)
      IF (BPARM(9).EQ.0.0) BPARM(9) = 1.0
      IF (BPARM(10).EQ.0.0) BPARM(10) = 1.0
      IF (CPARM(1).EQ.0.0) CPARM(1) = 1.0
      IF (CPARM(3).EQ.0.0) CPARM(3) = 1.0
      IF (CPARM(5).EQ.0.0) CPARM(5) = 1.0
      IF (CPARM(7).EQ.0.0) CPARM(7) = 1.0
      IF (CPARM(9).EQ.0.0) CPARM(9) = 1.0
C                                       Update catalog header for PL
      FRW(NCFILE) = 0
      PVER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', DISKIN, FCNO(1), CATBLK, SBUF, T, 'READ',
     *      PVER, IERR)
         IF (IERR.NE.0) THEN
            NCFILE = NCFILE - 1
            WRITE (MSGTXT,1060) IERR
            GO TO 990
            END IF
         END IF
C                                       parms
      XYSCL(1) = -1.0E10
      XYSCL(2) = XYSCL(1)
      XYOFF(1) = 1.E10
      XYOFF(2) = XYOFF(1)
      APARM(2) = ISUBAX(1,1)
      APARM(4) = ISUBAX(2,1)
      APARM(7) = ISUBAX(1,2)
      APARM(9) = ISUBAX(2,2)
C                                       Prepare for possible binning of
C                                       data
      NBIN = IROUND (BPARM(1))
      IF (DOHIST.GT.0.0) NBIN = ABS (NBIN)
      IF ((DOHIST.GT.0.0) .AND. (NBIN.LE.1)) NBIN = 50
      NBIN = MAX (-400, MIN (400, NBIN))
      BPARM(1) = NBIN
C                                       Autoscale ?
      SCALEM(1) = (BPARM(3).LE.0.0) .OR. (BPARM(4).EQ.BPARM(5))
      SCALEM(2) = (BPARM(3).LE.0.0) .OR. (BPARM(6).EQ.BPARM(7))
      TESTEM(1) = 1
      IF (BPARM(4).GT.BPARM(5)) TESTEM(1) = -1
      IF ((BPARM(3).EQ.0.0) .OR. (BPARM(4).EQ.BPARM(5))) TESTEM(1) = 0
      TESTEM(2) = 1
      IF (BPARM(6).GT.BPARM(7)) TESTEM(2) = -1
      IF ((BPARM(3).EQ.0.0) .OR. (BPARM(6).EQ.BPARM(7))) TESTEM(2) = 0
      IF (DOHIST.GT.0.0) THEN
         TESTEM(2) = 0
         SCALEM(2) = .TRUE.
         END IF
      DO 75 I = 1,400
         SUM(I) = 0.0
         SUM2(I) = 0.0
         BINCNT(I) = 0
 75      CONTINUE
C                                       testing on a column value
      J = 1
      NTVAL = 0
      DO 90 K = 1,7
         ITVAL(1,K) = IROUND (XARR(J))
         IF ((ITVAL(1,K).GT.0) .AND. (ITVAL(1,K).LE.NCOL)) THEN
            NTVAL = K
            ITVAL(2,K) = IROUND (XARR(J+1))
            I = MOD (DATP(ITVAL(1,K),2), 10)
            I4T = DATP(ITVAL(1,K),2) / 10
            ITVAL(2,K) = MAX (1, MIN (I4T, ITVAL(2,K)))
            IF ((I.EQ.1) .OR. (I.EQ.2)) THEN
               RTVAL(K) = XARR(J+2)
               IF (XARR(J+3).LE.0.0) XARR(J+3) = 1.0E-2
               ETVAL(K) = XARR(J+3)
            ELSE IF (I.EQ.4) THEN
               ITVAL(4,K) = IROUND (XARR(J+2))
               IF (XARR(J+3).LE.0.0) XARR(J+3) = 1.0E-2
               ETVAL(K) = XARR(J+3)
            ELSE
               CALL H2CHR (16, 1, XKEYST, CTVAL)
               ETVAL(K) = MIN (I4T, JTRIM (CTVAL))
               END IF
            ITVAL(3,K) = I
            J = J + 4
         ELSE
            GO TO 999
            END IF
 90      CONTINUE
      GO TO 999
C                                       Error message
 990  CALL MSGWRT (8)
      JERR = 5
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TAPIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ERROR',I4,' FINDING ',A12,'.',A6,'.',I4,' DISK',I2,
     *   ' USER',I5)
 1020 FORMAT ('ERROR',I4,' READING CATALOG HEADER')
 1030 FORMAT ('ERROR',I4,' OPENING EXT. ',A2,' VERS',I6)
 1040 FORMAT ('ERROR IN BCOUNT, ECOUNT INC:',3I8)
 1045 FORMAT (A2,'AXIS COLUMN NUMBERS',2I5,' OUT OF RANGE')
 1050 FORMAT ('I PLOT ONLY FLOATING OR INTEGER COLUMNS: PROBLEM IN ',A2,
     *   'AXIS')
 1055 FORMAT (A2,'AXIS SUBSCRIPTS',2I6,' EXCEED LIMITS',2I6)
 1060 FORMAT ('ERROR',I5,' ADDING PL FILE TO HEADER')
      END
      SUBROUTINE TASCAL (IRET)
C-----------------------------------------------------------------------
C   TASCAL read the table file to determine the scaling.
C   IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET, IROW, ICNT, JCNT, LINC, LBCNT
      DOUBLE PRECISION    XY(2)
      LOGICAL   PSC
      INCLUDE 'TAPLT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      ICNT = 0
      JCNT = 0
      RSUM(1) = 0.0D0
      RSUM(2) = 0.0D0
      PSC = SCALEM(2)
C                                       Loop
      IF ((SUMEM(1)) .OR. (SUMEM(2))) THEN
         LINC = 1
         LBCNT = 1
      ELSE
         LINC = INC
         LBCNT = IBCNT
         END IF
      IF (NBIN.GT.0) SCALEM(2) = .FALSE.
      DO 100 IROW = LBCNT,IECNT,LINC
         CALL FNDTXY (IROW, XY, IRET)
         IF ((IRET.LT.0).OR.(XY(1).EQ.DBLANK).OR.(XY(2).EQ.DBLANK))
     *      GO TO 100
         IF (IRET.GT.0) GO TO 999
C                                       Find scales
         IF ((IROW.GE.IBCNT) .AND. (MOD(IROW-IBCNT,INC).EQ.0)) THEN
            ICNT = ICNT + 1
            CALL XYSCAT (ICNT, XY, IRET)
            IF (IRET.EQ.0) JCNT = JCNT + 1
            END IF
 100     CONTINUE
C                                       Any valid points
      IF (JCNT.GT.1) GO TO 110
         IRET = 4
         WRITE (MSGTXT,1100) JCNT
         GO TO 990
C                                       Final call to XYSCL.
 110  ICNT = -1
      CALL XYSCAT (ICNT, XY, IRET)
      IF (IRET.LE.0) GO TO 120
         WRITE (MSGTXT,1110) IRET
         GO TO 990
 120  IRET = 0
      SCALEM(2) = PSC
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('FOUND',I5,' POINTS: NOT ENOUGH TO SELF-SCALE')
 1110 FORMAT ('TASCAL: XYSCAT ERROR',I3)
      END
      SUBROUTINE TABSCL (IRET)
C-----------------------------------------------------------------------
C   TABSCL reads the table file to determine the binned Y scale
C   Output
C      IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I, IP, NP, IROW, ICNT, JCNT, LINC, LBCNT
      REAL      TEMP, STD
      DOUBLE PRECISION    XY(2), XMIN, XMAX, DX
      INCLUDE 'TAPLT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      ICNT = 0
      JCNT = 0
      IF (SCALEM(1)) THEN
         XMIN = XYOFF(1)
         XMAX = 1000.0 / XYSCL(1) + XYOFF(1)
      ELSE
         XMIN = BPARM(4)
         XMAX = BPARM(5)
         END IF
      NP = ABS (NBIN)
      DX = (XMAX - XMIN) / NP
      IF (SUMEM(1)) THEN
         LINC = 1
         LBCNT = 1
      ELSE
         LINC = INC
         LBCNT = IBCNT
         END IF
      RSUM(1) = 0.0D0
      RSUM(2) = 0.0D0
C                                       Loop
      DO 100 IROW = LBCNT,IECNT,LINC
         CALL FNDTXY (IROW, XY, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       Find scales
         IF ((IRET.EQ.0) .AND. (IROW.GE.IBCNT) .AND.
     *      (MOD(IROW-IBCNT,INC).EQ.0) .AND. (XY(1).NE.DBLANK) .AND.
     *      (XY(2).NE.DBLANK)) THEN
            IF (XY(1).EQ.XMAX) XY(1) = XMAX - DX/100.
            IP = (XY(1) - XMIN) / DX + 1.0D0
            IF ((IP.LT.1) .OR. (IP.GT.NP)) THEN
               JCNT = JCNT + 1
            ELSE
               ICNT = ICNT + 1
               BINCNT(IP) = BINCNT(IP) + 1
               SUM(IP) = SUM(IP) + XY(2)
               SUM2(IP) = SUM2(IP) + XY(2)*XY(2)
               END IF
            END IF
 100     CONTINUE
C                                       Any valid points
      IF (ICNT.LE.1) THEN
         IRET = 4
         WRITE (MSGTXT,1100) ICNT
         GO TO 990
         END IF
C                                       Any missing points
      IF (JCNT.GT.0) THEN
         WRITE (MSGTXT,1110) JCNT
         CALL MSGWRT (4)
         END IF
      IRET = 0
      WRITE (MSGTXT,1120) ICNT
      CALL MSGWRT (4)
      XMAX = -1.0D9
      XMIN = 1.D9
      DO 130 I = 1,NP
         STD = 0.0
C                                       include only filled bins
         IF ((NBIN.GT.0) .AND. (BINCNT(I).GT.0)) THEN
            SUM(I) = SUM(I) / BINCNT(I)
            IF (BINCNT(I).GE.2) STD = SQRT (ABS ((SUM2(I)/BINCNT(I)) -
     *         SUM(I)*SUM(I)) / (BINCNT(I) - 1.0))
            SUM2(I) = STD
            IF (XMAX.LT.SUM(I)+STD) XMAX = SUM(I)+STD
            IF (XMIN.GT.SUM(I)-STD) XMIN = SUM(I)-STD
            END IF
 130     CONTINUE
      XYOFF(2) = XMIN
      XYSCL(2) = XMAX
      TEMP = 0.025 * (XYSCL(2) - XYOFF(2))
      XYSCL(2) = XYSCL(2) + TEMP
      XYOFF(2) = XYOFF(2) - TEMP
      XYSCL(2) = 1000.0 / (XYSCL(2) - XYOFF(2))
      DO 140 I = 1,NP
         BINCNT(I) = 0
         SUM(I) = 0.0
         SUM2(I) = 0.0
 140     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('FOUND',I5,' POINTS: NOT ENOUGH TO SET A SCALE')
 1110 FORMAT ('Found',I8,' points outside x range while binning')
 1120 FORMAT ('Found',I8,' points to include in bins')
      END
      SUBROUTINE TAHIST (IRET)
C-----------------------------------------------------------------------
C   TAHIST reads the table file to determine the histogram.
C   IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET, I, IP, NP, IROW, ICNT, JCNT, LINC, LBCNT
      REAL      TEMP
      DOUBLE PRECISION    XY(2), XMIN, XMAX, DX
      INCLUDE 'TAPLT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      ICNT = 0
      JCNT = 0
      IF (SCALEM(1)) THEN
         XMIN = XYOFF(1)
         XMAX = 1000.0 / XYSCL(1) + XYOFF(1)
      ELSE
         XMIN = BPARM(4)
         XMAX = BPARM(5)
         END IF
      NP = BPARM(1) + 0.01
      DX = (XMAX - XMIN) / NP
      IF (SUMEM(1)) THEN
         LINC = 1
         LBCNT = 1
      ELSE
         LINC = INC
         LBCNT = IBCNT
         END IF
      RSUM(1) = 0.0D0
      RSUM(2) = 0.0D0
C                                       Loop
      DO 100 IROW = LBCNT,IECNT,LINC
         CALL FNDTXY (IROW, XY, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       Find scales
         IF ((IRET.EQ.0) .AND. (IROW.GE.IBCNT) .AND.
     *      (MOD(IROW-IBCNT,INC).EQ.0) .AND. (XY(1).NE.DBLANK) .AND.
     *      (XY(2).NE.DBLANK)) THEN
            IF (XY(1).EQ.XMAX) XY(1) = XMAX - DX/100.
            IP = (XY(1) - XMIN) / DX + 1.0D0
            IF ((IP.LT.1) .OR. (IP.GT.NP)) THEN
               JCNT = JCNT + 1
            ELSE
               ICNT = ICNT + 1
               BINCNT(IP) = BINCNT(IP) + 1
               END IF
            END IF
 100     CONTINUE
C                                       Any valid points
      IF (ICNT.GT.1) GO TO 110
         IRET = 4
         WRITE (MSGTXT,1100) ICNT
         GO TO 990
C                                       Any missing points
 110  IF (JCNT.LE.0) GO TO 120
         WRITE (MSGTXT,1110) JCNT
         CALL MSGWRT (4)
 120  IRET = 0
      WRITE (MSGTXT,1120) ICNT
      CALL MSGWRT (4)
      XMAX = 0.0D0
      XMIN = 0.0D0
      DO 130 I = 1,NP
         SUM(I) = BINCNT(I)
         IF ((OPTYPE(2).GE.3) .AND. (SUM(I).LT.1.0)) SUM(I) = -0.3
         IF ((OPTYPE(2).EQ.3) .AND. (SUM(I).GE.1.0))
     *      SUM(I) = LOG10 (SUM(I))
         IF ((OPTYPE(2).GE.4) .AND. (SUM(I).GE.1.0))
     *      SUM(I) = LOG (SUM(I))
         IF (SUM(I).GT.0.0) SUM(I) = SUM(I) ** BPARM(10)
         IF (SUM(I).GT.XMAX) XMAX = SUM(I)
         IF (SUM(I).LT.XMIN) XMIN = SUM(I)
 130     CONTINUE
      XYOFF(2) = XMIN
      XYSCL(2) = XMAX
      TEMP = 0.025 * (XYSCL(2) - XYOFF(2))
      XYSCL(2) = XYSCL(2) + TEMP
      XYOFF(2) = XYOFF(2) - TEMP
      XYSCL(2) = 1000.0 / (XYSCL(2) - XYOFF(2))
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('FOUND',I5,' POINTS: NOT ENOUGH TO HISTOGRAM')
 1110 FORMAT ('Found',I8,' points outside x range while histogramming')
 1120 FORMAT ('Found',I8,' points to include in histogram')
      END
      SUBROUTINE PLOTAB (IRET)
C-----------------------------------------------------------------------
C   PLOTAB actually plots table data.
C   Output:
C      IRET   I   Return code, 0 => OK, otherwise abort.
C                       1 => failed to add to catalog
C                       2 => failed to create
C                       3 => graph file write error
C                       4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER TEXT*1024, PFILE*48, COLTYP(2,2)*8, TIME*8, DATE*12,
     *   CHAX(2)*2, CTEMP*18
      INTEGER   BUFFER(256), IERR, ITYPE, IPSIZE, I, LUNPL, FINDPL,
     *   IAPARM(8), INCHAR, INP, J, IBIN, IP, IT(3), ID(3), II, JJ,
     *   LTYPE
      REAL      BLC(2), TRC(2), CHOUT(4), XYRATO, DX, DY, TR, TI, XY(2),
     *   AVERG, STDEV, RESULT(6), TEMP, SCLR(2)
      DOUBLE PRECISION    XZY(2)
      INTEGER   NGOOD, NNOFIT, LINC, IROW, IRNO, LBCNT, NCOUNT, NB
      LOGICAL   GOOD, NOCHK, DOGRID
      INCLUDE 'TAPLT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNPL /26/
      DATA CHAX /'X-','Y-'/
C-----------------------------------------------------------------------
      NGOOD = 0
      NCOUNT = 0
      NNOFIT = 0
      IRET = 1
      RSUM(1) = 0.0D0
      RSUM(2) = 0.0D0
      NB = ABS (NBIN)
C                                       See if data range check
      NOCHK = (NB.GT.0)
C                                       User sets the scales
      DO 10 I = 1,2
         IF (.NOT.SCALEM(I)) THEN
            XYSCL(I) = BPARM(3+2*I)
            XYOFF(I) = BPARM(2+2*I)
            TEMP = 0.015 * (XYSCL(I) - XYOFF(I))
            XYSCL(I) = XYSCL(I) + TEMP
            XYOFF(I) = XYOFF(I) - TEMP
            IF (XYSCL(I).EQ.XYOFF(I)) GO TO 999
            XYSCL(I) = 1000. / (XYSCL(I)-XYOFF(I))
            END IF
 10      CONTINUE
C                                       Fill in last of actual parms
      BPARM(5) = 1000.0/XYSCL(1) + XYOFF(1)
      BPARM(7) = 1000.0/XYSCL(2) + XYOFF(2)
      BPARM(4) = XYOFF(1)
      BPARM(6) = XYOFF(2)
C                                       Create plot file
      CALL ZPHFIL ('PL', DISKIN, FCNO(1), PVER, PFILE, IERR)
      IF (IERR.NE.0) GO TO 999
      IPSIZE = 0
      ITYPE = 15
      CALL GINIT (DISKIN, FCNO(1), PFILE, IPSIZE, ITYPE, NPARM, USERID,
     *   DOTV, TVCHN, GRCHN, TVCORN, CATBLK, BUFFER, LUNPL, FINDPL,
     *   IERR)
      IRET = 2
      IF (IERR.NE.0) GO TO 999
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      XYRATO = 1.0
      IRET = 3
      CALL FILL (5, 1, IAPARM)
C                                       Set up location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      DO 30 I = 1,2
         TR = 1000.0 / XYSCL(I)
         TI = TR
         RPLOC(I,LOCNUM) = BLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I)
         AXINC(I,LOCNUM) = TR / (TRC(I) - BLC(I))
         CALL METSCL (LABEL, TR, CPREF(I,LOCNUM), GOOD)
         RPVAL(I,LOCNUM) = RPVAL(I,LOCNUM) * TR / TI
         AXINC(I,LOCNUM) = AXINC(I,LOCNUM) * TR / TI
C                                       get col. units
         DO 20 J = 1,2
            IF ((J.NE.2) .OR. (TWOOF(I))) THEN
               IF ((I.EQ.2) .AND. (DOHIST.GT.0.0)) THEN
                  IF (J.EQ.1) CTYP(I,LOCNUM) = 'COUNT'
                  COLTYP(J,I) = 'HISTGRAM'
               ELSE IF (TYPEAX(J,I).EQ.0) THEN
                  IF (J.EQ.1) CTYP(I,LOCNUM) = 'ROW NUMB'
                  COLTYP(J,I) = 'ROW NUMB'
               ELSE
                  IRNO = TYPEAX(J,I)
                  IF (J.EQ.1) CALL TABIO ('READ', 4, IRNO, RESULT,
     *               TABUF, IERR)
                  IF (IERR.NE.0) GO TO 970
                  IF (J.EQ.1) CALL H2CHR (8, 1, RESULT, CTYP(I,LOCNUM))
                  CALL TABIO ('READ', 3, IRNO, RESULT, TABUF, IERR)
                  IF (IERR.NE.0) GO TO 970
                  CALL H2CHR (8, 1, RESULT, COLTYP(J,I))
                  IF (CTYP(I,LOCNUM).EQ.' ') CTYP(I,LOCNUM) =
     *               COLTYP(J,I)
                  END IF
               END IF
 20         CONTINUE
         IF ((OPTYPE(I).EQ.9) .OR. (OPTYPE(I).EQ.17)) CTYP(I,LOCNUM) =
     *      'DEGREES'
 30      CONTINUE
C                                       Count characters around
      CALL RFILL (4, 0.5, CHOUT)
      CALL CHNTIC (BLC, TRC, INP)
      LTYPE = MOD (ABS (LABEL), 100)
      IF (LTYPE.EQ.2) CHOUT(1) = 2.5
      IF ((LTYPE.GT.2) .AND. (INP.GT.0)) CHOUT(1) = 4.0 + INP
      IF (LTYPE.GT.1) CHOUT(2) = 2.0
      IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = 4.666
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = CHOUT(4) + 1.333
C                                       Init for line drawing.
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, IAPARM, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL GLTYPE (1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Draw border
      CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Top labels: type & name
      DX = 0.0
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) THEN
         DY = 0.5 + 3 * 1.333
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, TIME, DATE)
         WRITE (TEXT,1030) PVER, DATE, TIME
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Top labels: type & name
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         DY = 0.5 + 2 * 1.333
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         INP = 21
         WRITE (TEXT,1031) INEXT, EVER
         TEXT(INP-1:INP-1) = '_'
         CALL H2CHR (18, 1, CATH(KHIMN), CTEMP)
         CALL NAMEST (CTEMP, SEQIN, TEXT(INP:), INCHAR)
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Top label: col. labels
         DO 45 I = 1,2
            DY = DY - 1.333
            CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            TEXT = ' '
            WRITE (TEXT,1032) CHAX(I)
            IP = 9
            IF (I.EQ.1) THEN
               IF (NBIN.NE.0) THEN
                  TEXT(9:) = 'BINNED'
                  IP = 16
                  END IF
            ELSE IF (DOHIST.LE.0.0) THEN
               IF (NBIN.LT.0) THEN
                  TEXT(9:) = 'BINNED SUM'
                  IP = 20
               ELSE IF (NBIN.GT.0) THEN
                  TEXT(9:) = 'BINNED AVG'
                  IP = 20
                  END IF
               END IF
C                                       first column(s)
            II = 4*I - 3
            JJ = 8 + I
            IF (I.EQ.1) THEN
               SCLR(1) = CPARM(9)
               SCLR(2) = CPARM(10)
            ELSE
               SCLR(1) = 1.0
               SCLR(2) = 0.0
               END IF
            CALL SELSTR (IP, OPTYPE(I), TWOOF(I), ABSEM(1,I),
     *         TYPEAX(1,I), ISUBAX(1,I), CPARM(II), SCLR, BPARM(JJ),
     *         COLTYP(1,I), IP, TEXT)
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
 45         CONTINUE
         END IF
C                                       Put on labels and ticks
      DOGRID = .FALSE.
      CALL CLAB1 (BLC, TRC, CHOUT, LABEL, XYRATO, DOGRID, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Read table file:
      DX = 1.0
      DY = 1.0
      CALL GLTYPE (2, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Plot histogram
      IF (DOHIST.GT.0.0) THEN
         DX = 5.
         DY = 5.
         DO 50 IBIN = 1,NBIN
            XY(1) = (IBIN * 1000. - 500.) / NBIN
            XY(2) = XYSCL(2) * (SUM(IBIN) - XYOFF(2))
            IF ((XY(2).LT.BLC(2)) .OR. (XY(2).GT.TRC(2))) NNOFIT =
     *         NNOFIT + 1
            IF ((XY(2).GE.BLC(2)) .AND. (XY(2).LE.TRC(2))) THEN
               CALL GPOS (XY(1)+DX, XY(2), BUFFER, IRET)
               IF (IRET.NE.0) GO TO 970
               CALL GVEC (XY(1)-DX, XY(2), BUFFER, IRET)
               IF (IRET.NE.0) GO TO 970
               CALL GPOS (XY(1), XY(2)+DY, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 970
               CALL GVEC (XY(1), XY(2)-DY, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 970
               NGOOD = NGOOD + 1
               END IF
 50         CONTINUE
C                                       Loop
      ELSE
         IF ((SUMEM(1)) .OR. (SUMEM(2))) THEN
            LINC = 1
            LBCNT = 1
         ELSE
            LINC = INC
            LBCNT = IBCNT
            END IF
         DO 80 IROW = LBCNT,IECNT,LINC
            CALL FNDTXY (IROW, XZY, IRET)
            IF (IRET.GT.0) GO TO 999
            IF ((IRET.EQ.0) .AND. (IROW.GE.IBCNT) .AND.
     *         (MOD(IROW-IBCNT,INC).EQ.0) .AND. (XZY(1).NE.DBLANK) .AND.
     *         (XZY(2).NE.DBLANK)) THEN
               DO 70 J = 1,2
                  XY(J) = XYSCL(J) * (XZY(J) - XYOFF(J))
                  IF (((XY(J).GE.BLC(J)) .AND. (XY(J).LE.TRC(J))) .OR.
     *               ((NOCHK) .AND. (J.EQ.2))) GO TO 70
                     NNOFIT = NNOFIT + 1
                     GO TO 80
 70               CONTINUE
               NGOOD = NGOOD + 1
C                                       If binning - accumulate
               IF (NB.GT.0) THEN
                  IBIN = 1 + NB * XY(1) / 1000.
                  IF (IBIN.LT.1) IBIN = 1
                  IF (IBIN.GT.NB) IBIN = NB
                  SUM(IBIN) = SUM(IBIN) + XY(2)
                  SUM2(IBIN) = SUM2(IBIN) + XY(2)*XY(2)
                  BINCNT(IBIN) = BINCNT(IBIN) + 1
C                                       Mark the point
               ELSE
                  CALL GPOS (XY(1)+DX, XY(2), BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 970
                  CALL GVEC (XY(1)-DX, XY(2), BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 970
                  CALL GPOS (XY(1), XY(2)+DY, BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 970
                  CALL GVEC (XY(1), XY(2)-DY, BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 970
                  END IF
               END IF
 80         CONTINUE
C                                       Done: finish plot
         IF (NBIN.GT.0) THEN
C                                       Plot binned data.
            NCOUNT = NGOOD
            NGOOD = 0
            NNOFIT = 0
            DX = 5.0
            DO 120 IBIN = 1,NB
               STDEV = 0.0
C                                       sum plot
               IF ((BINCNT(IBIN).LE.0) .OR. (NBIN.LT.0)) THEN
                  XY(1) = (IBIN * 1000. - 500.) / NB
                  XY(2) = SUM(IBIN)
                  IF (BINCNT(IBIN).GT.0) XY(2) = XY(2) +
     *               XYSCL(2) * XYOFF(2) * (BINCNT(IBIN) - 1)
                  IF ((XY(2).LT.BLC(2)) .OR. (XY(2).GT.TRC(2))) THEN
                     NNOFIT = NNOFIT + 1
                  ELSE
                     DY = 5.0
                     CALL GPOS (XY(1)+DX, XY(2), BUFFER, IRET)
                     IF (IRET.NE.0) GO TO 970
                     CALL GVEC (XY(1)-DX, XY(2), BUFFER, IRET)
                     IF (IRET.NE.0) GO TO 970
                     CALL GPOS (XY(1), XY(2)+DY, BUFFER, IRET)
                     IF (IRET.NE.0) GO TO 970
                     CALL GVEC (XY(1), XY(2)-DY, BUFFER, IRET)
                     IF (IRET.NE.0) GO TO 970
                     NGOOD = NGOOD + 1
                     END IF
C                                       average w plus/minus
               ELSE
                  AVERG = SUM(IBIN) / BINCNT(IBIN)
                  IF (BINCNT(IBIN).GE.2) STDEV =
     *               SQRT (ABS ((SUM2(IBIN)/BINCNT(IBIN)) -
     *               AVERG*AVERG)/(BINCNT(IBIN) - 1.0))
                  DY = STDEV
                  IF (BINCNT(IBIN).GE.2.0) DY = MAX (DX, DY)
                  XY(1) = (IBIN * 1000. - 500.) / NB
                  XY(2) = AVERG
                  IF ((XY(2).LT.BLC(2)) .OR. (XY(2).GT.TRC(2))) THEN
                     NNOFIT = NNOFIT + 1
                  ELSE
                     CALL GPOS (XY(1)+DX, XY(2), BUFFER, IRET)
                     IF (IRET.NE.0) GO TO 970
                     CALL GVEC (XY(1)-DX, XY(2), BUFFER, IRET)
                     IF (IRET.NE.0) GO TO 970
                     CALL GPOS (XY(1), XY(2)+DY, BUFFER, IRET)
                     IF (IRET.NE.0) GO TO 970
                     CALL GVEC (XY(1), XY(2)-DY, BUFFER, IRET)
                     IF (IRET.NE.0) GO TO 970
                     NGOOD = NGOOD + 1
                     END IF
                  END IF
               IF (BPARM(2).GT.0.0) THEN
                  XY(1) = XY(1) / XYSCL(1) + XYOFF(1)
                  XY(2) = XY(2) / XYSCL(2) + XYOFF(2)
                  STDEV = STDEV / XYSCL(2)
                  WRITE (MSGTXT,1110) XY(1), XY(2), STDEV
                  CALL MSGWRT (4)
                  END IF
 120           CONTINUE
            END IF
         END IF
C                                       finish plot
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.EQ.0) THEN
         IRET = 0
         GO TO 990
         END IF
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 990
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKIN, PFILE, IERR)
         END IF
      GO TO 999
C                                       No catalog update
C                                       Messages
 990  WRITE (MSGTXT,1990) NGOOD
      CALL MSGWRT (2)
      WRITE (MSGTXT,1991) NNOFIT
      IF (NNOFIT.GE.1.0D0) CALL MSGWRT (2)
      WRITE (MSGTXT,1992) NCOUNT
      IF (NCOUNT.GE.1.0D0) CALL MSGWRT (2)
      WRITE (MSGTXT,1993) PVER
      CALL MSGWRT (2)
      CALL ZCLOSE (TALUN, TAIND, IERR)
      IF (.NOT.DOTV) CALL HIPLOT (DISKIN, FCNO(1), PVER, BUFFER, IERR)
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('Plot file version',I4,'__created ',A12,A8)
 1031 FORMAT ('Ext ',A2,'__Vers',I4)
 1032 FORMAT (A2,'Axis: ')
 1110 FORMAT ('X=',1PE12.5,', Y=',E12.5,', SIG=',E12.5)
 1970 FORMAT ('PLOTAB: Error during graphing. will try to finish',
     *   ' partial graph')
 1990 FORMAT ('PLOTAB: ',I10,' points plotted')
 1991 FORMAT ('PLOTAB: ',I10,' points did not fit')
 1992 FORMAT ('PLOTAB: ',I10,' points entered into bins')
 1993 FORMAT ('PLOTAB: Plot file version',I5,'  created.')
      END
      SUBROUTINE XYSCAT (ICNT, XY, IRET)
C-----------------------------------------------------------------------
C   XYSCAT finds the scaling parameters needed to fit X and Y
C   into a 1000*1000 plotting area .
C   Inputs:
C      NUMVIS     I    Data number, -1 => final call, no data
C                      passed -> change to scaling factor from max/min
C      XY         D    plotted parameters .
C   Outputs:
C      XYOFF      R    when added to XY changes minimum to zero .
C      XYSCL      R    scale XY so that maximum is 1000.
C      IRET       I    Error return code , non-zero if error .
C-----------------------------------------------------------------------
      DOUBLE PRECISION    XY(2), TEMP
      INTEGER   IRET, I, JJ, ICNT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TAPLT.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Are they in requested range
      IF (ICNT.GE.0) THEN
         IRET = -1
         DO 10 I = 1,2
            IF (TESTEM(I).NE.0) THEN
               JJ = 2*I + 2
               IF ((BPARM(JJ).LT.BPARM(JJ+1)) .AND. ((XY(I).LT.
     *            BPARM(JJ)) .OR. (XY(I).GT.BPARM(JJ+1)))) GO TO 999
               IF ((BPARM(JJ).GT.BPARM(JJ+1)) .AND. ((XY(I).GT.
     *            BPARM(JJ)) .OR. (XY(I).LT.BPARM(JJ+1)))) GO TO 999
               END IF
 10         CONTINUE
         IRET = 0
C                                       Find max, min from data
         DO 30 I = 1,2
            IF (SCALEM(I)) THEN
               IF (XY(I).LT.XYOFF(I)) XYOFF(I) = XY(I)
               IF (XY(I).GT.XYSCL(I)) XYSCL(I) = XY(I)
               END IF
 30         CONTINUE
C                                       Convert to scaling factors
C                                       provide room at edges too.
      ELSE
         DO 120 I = 1,2
            IF (SCALEM(I)) THEN
               IF (XYSCL(I).GT.XYOFF(I)) THEN
                  IRET = 1
                  WRITE (MSGTXT,1980) I
                  CALL MSGWRT (8)
               ELSE
                  TEMP = 0.025 * (XYSCL(I) - XYOFF(I))
                  XYSCL(I) = XYSCL(I) + TEMP
                  XYOFF(I) = XYOFF(I) - TEMP
                  IF ((XYOFF(I).GT.0.0) .AND.
     *               (XYOFF(I).LT.0.15*XYSCL(I))) XYOFF(I) = 0.0
                  XYSCL(I) = 1000.0 / (XYSCL(I) - XYOFF(I))
                  END IF
               END IF
 120        CONTINUE
         END IF
      GO TO 999
C
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('XYSCAT: AXIS',I2,' DEGENERATE')
      END
      SUBROUTINE FNDTXY (IROW, XY, IRET)
C-----------------------------------------------------------------------
C   FNDTXY extracts the desired X and Y values from the table file.
C   Inputs:  IROW   I        Table row number
C   Outputs: XY     D(2)     X, Y values
C            IRET   I        Error code: <0 -> skip row, > 0 bad error
C-----------------------------------------------------------------------
      INTEGER   IROW, IRET
      DOUBLE PRECISION XY(2)
C
      INCLUDE 'TAPLT.INC'
      DOUBLE PRECISION  DADATA(XBPRSZ/2), XZY(2)
      REAL      D4DATA(XBPRSZ)
      INTEGER   D3DATA(XBPRSZ), I, D2DATA(XBPRSZ), J, JT, IO, JTRIM, M
      LOGICAL   DOIT
      HOLLERITH DHDATA(XBPRSZ)
      CHARACTER SCRTCH*132
      EQUIVALENCE (DADATA, D4DATA, D3DATA, D2DATA, DHDATA)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       do we want this row
      DOIT = .TRUE.
      IRET = -1
      DO 110 I = 1,NTVAL
         J = ITVAL(1,I)
         CALL GETCOL (IROW, J, DATP, LROW, TABUF, JT, DADATA,
     *      RECI, IRET)
         IF (IRET.NE.0) GO TO 999
         JT = ITVAL(3,I)
         IF (JT.EQ.1) THEN
            IF (ABS(DADATA(ITVAL(2,I))-RTVAL(I)).GT.ETVAL(I))
     *         DOIT = .FALSE.
         ELSE IF (JT.EQ.2) THEN
            IF (ABS(D4DATA(ITVAL(2,I))-RTVAL(I)).GT.ETVAL(I))
     *         DOIT = .FALSE.
         ELSE IF (JT.EQ.4) THEN
            IF (ABS(D3DATA(ITVAL(2,I))-ITVAL(4,I)).GT.ETVAL(I))
     *         DOIT = .FALSE.
         ELSE
            M = ETVAL(I) + 0.1
            CALL H2CHR (M, 1, DHDATA, SCRTCH)
            JT = JTRIM (SCRTCH(:M))
            IF (SCRTCH(:M).NE.CTVAL(:M)) DOIT = .FALSE.
            END IF
 110     CONTINUE
      IF ((DOINV.GT.0.0) .AND. (NTVAL.GT.0)) DOIT = .NOT.DOIT
      IF (.NOT.DOIT) THEN
         IRET = -1
         GO TO 999
         END IF
C                                       Loop over axes
      DO 100 I = 1,2
         IF ((I.EQ.2) .AND. (DOHIST.GT.0.0)) GO TO 100
C                                       Get values
         DO 10 J = 1,2
            IF ((J.EQ.1) .OR. (TWOOF(I))) THEN
               XZY(J) = IROW
               IF (TYPEAX(J,I).NE.0) THEN
                  CALL GETCOL (IROW, TYPEAX(J,I), DATP, LROW, TABUF, JT,
     *               DADATA, RECI, IRET)
                  IF (IRET.NE.0) GO TO 999
                  JT = MOD (JT, 10)
                  IF (JT.EQ.1) THEN
                     IF (DADATA(ISUBAX(J,I)).EQ.DBLANK) IRET = -1
                     XZY(J) = DADATA(ISUBAX(J,I))
                  ELSE IF (JT.EQ.2) THEN
                     IF (D4DATA(ISUBAX(J,I)).EQ.FBLANK) IRET = -1
                     XZY(J) = D4DATA(ISUBAX(J,I))
                  ELSE IF (JT.EQ.4) THEN
                     XZY(J) = D3DATA(ISUBAX(J,I))
                  ELSE IF (JT.EQ.6) THEN
                     XZY(J) = D2DATA(ISUBAX(J,I))
                     END IF
                  IF (IRET.NE.0) GO TO 999
                  IF (ABSEM(J,I)) XZY(J) = ABS(XZY(J))
                  END IF
               IO = 4 * I + 2 * J - 5
               XZY(J) = XZY(J) * CPARM(IO) + CPARM(IO+1)
               END IF
 10         CONTINUE
C                                       perform function
         IRET = -2
         J = OPTYPE(I) + 1
         GO TO (20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
     *      34, 35, 36, 37, 38, 39), J
C                                       straight through
 20      XY(I) = XZY(1)
         GO TO 95
C                                       Sum
 21      RSUM(I) = RSUM(I) + XZY(1)
         XY(I) = RSUM(I)
         GO TO 95
C                                       ASIN
 22      IF (ABS(XZY(1)).GT.1.0D0) GO TO 999
         XY(I) = ASIN (XZY(1)) / DEG2R
         GO TO 95
C                                       LOG10
 23      IF (XZY(1).LE.0.0D0) GO TO 999
         XY(I) = LOG10 (XZY(1))
         GO TO 95
C                                       LN
 24      IF (XZY(1).LE.0.0D0) GO TO 999
         XY(I) = LOG (XZY(1))
         GO TO 95
C                                       EXP
 25      XY(I) = EXP (XZY(1))
         GO TO 95
C                                       SIN
 26      XY(I) = SIN (DEG2R * XZY(1))
         GO TO 95
C                                       COS
 27      XY(I) = COS (DEG2R * XZY(1))
         GO TO 95
C                                       TAN
 28      XY(I) = TAN (DEG2R * XZY(1))
         GO TO 95
C                                       ARC TAN
 29      XY(I) = RAD2DG * ATAN (XZY(1)) / DEG2R
         GO TO 95
C                                       +
 30      XY(I) = XZY(1) + XZY(2)
         GO TO 95
C                                       -
 31      XY(I) = XZY(1) - XZY(2)
         GO TO 95
C                                       *
 32      XY(I) = XZY(1) * XZY(2)
         GO TO 95
C                                       /
 33      IF (XZY(2).EQ.0.D0) GO TO 999
         XY(I) = XZY(1) / XZY(2)
         GO TO 95
C                                       **
 34      IF (XZY(1).LT.0.D0) GO TO 999
         XY(I) = XZY(1) ** XZY(2)
         GO TO 95
C                                       MOD
 35      IF (XZY(2).EQ.0.D0) GO TO 999
         XY(I) = MOD (XZY(1), XZY(2))
         GO TO 95
C                                       Modulus
 36      XY(I) = SQRT (XZY(1)*XZY(1) + XZY(2)*XZY(2))
         GO TO 95
C                                       ATAN2
 37      IF ((XZY(1).EQ.0.D0) .AND. (XZY(2).EQ.0.D0)) GO TO 999
         XY(I) = RAD2DG * ATAN2 (XZY(1), XZY(2))
         GO TO 95
C                                       Max
 38      XY(I) = XZY(1)
         IF (XZY(2).GT.XZY(1)) XY(I) = XZY(2)
         GO TO 95
C                                       Min
 39      XY(I) = XZY(1)
         IF (XZY(2).LT.XZY(1)) XY(I) = XZY(2)
C                                       Okay!: scale
 95      IO = 8 + I
         IF ((BPARM(IO).NE.1.0) .AND. (BPARM(IO).NE.0.0) .AND.
     *      (XY(I).NE.0.0)) THEN
            JT = BPARM(IO)
            IF (JT.NE.BPARM(IO)) THEN
               IF (XY(I).GT.0.0D0) XY(I) = XY(I) ** BPARM(IO)
            ELSE
               XY(I) = XY(I) ** JT
               END IF
            END IF
C                                       Added X scale
         IF (I.EQ.1) XY(I) = XY(I) * CPARM(9) + CPARM(10)
 100     CONTINUE
      IRET = 0
C
 999  RETURN
      END
